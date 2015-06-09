{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example:
    Small database with CRUD operations and filtering.
    To keep things simple, the list box is rebuild every time
    that the database is updated. This is perfectly fine for rapid prototyping.
    A more sophisticated approach would use incremental updates.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}

import Prelude hiding (lookup)
import Data.List (isPrefixOf)
import Data.Maybe
import qualified Data.Map as Map

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    -- GUI layout
    f           <- frame    [ text := "CRUD Example (Simple)" ]
    listBox     <- singleListBox f []
    createBtn   <- button f [ text := "Create" ]
    deleteBtn   <- button f [ text := "Delete" ]
    filterEntry <- entry  f [ ]
    
    firstname <- entry f [ ]
    lastname  <- entry f [ ]
    
    let dataItem = grid 10 10 [[label "First Name:", widget firstname]
                              ,[label "Last Name:" , widget lastname]]
    set f [layout := margin 10 $
            grid 10 5
                [[row 5 [label "Filter prefix:", widget filterEntry], glue]
                ,[minsize (sz 200 300) $ widget listBox, dataItem]
                ,[row 10 [widget createBtn, widget deleteBtn], glue]
                ]]

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = mdo
            -- events from buttons
            eCreate <- event0 createBtn command       
            eDelete <- event0 deleteBtn command
            -- filter string
            tFilterString <- reactiveTextEntry filterEntry bFilterString
            let bFilterString = stepper "" $ rumors tFilterString
                tFilter = isPrefixOf <$> tFilterString
                bFilter = facts  tFilter
                eFilter = rumors tFilter

            -- list box with selection
            eSelection <- rumors <$> reactiveListDisplay listBox
                bListBoxItems bSelection bShowDataItem
            -- data item display
            eDataItemIn <- rumors <$> reactiveDataItem (firstname,lastname)
                bSelectionDataItem

            let -- database
                bDatabase :: Behavior t (Database DataItem)
                bDatabase = accumB emptydb $ unions
                    [ create ("Emil","Example") <$ eCreate
                    , filterJust $ update' <$> bSelection <@> eDataItemIn
                    , delete <$> filterJust (bSelection <@ eDelete)
                    ]
                    where
                    update' mkey x = flip update x <$> mkey
                
                -- selection
                bSelection :: Behavior t (Maybe DatabaseKey)
                bSelection = stepper Nothing $ unions
                    [ eSelection
                    , Nothing <$ eDelete
                    , Just . nextKey <$> bDatabase <@ eCreate
                    , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
                        <$> bSelection <*> bShowDataItem <@> eFilter
                    ]
                
                bLookup :: Behavior t (DatabaseKey -> Maybe DataItem)
                bLookup = flip lookup <$> bDatabase
                
                bShowDataItem :: Behavior t (DatabaseKey -> String)
                bShowDataItem = (maybe "" showDataItem .) <$> bLookup
                
                bListBoxItems :: Behavior t [DatabaseKey]
                bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

                bSelectionDataItem :: Behavior t (Maybe DataItem)
                bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

            -- automatically enable / disable editing
            let
                bDisplayItem :: Behavior t Bool
                bDisplayItem = isJust <$> bSelection
            
            sink deleteBtn [ enabled :== bDisplayItem ]
            sink firstname [ enabled :== bDisplayItem ]
            sink lastname  [ enabled :== bDisplayItem ]
    
    network <- compile networkDescription    
    actuate network

{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb :: Database a
emptydb = Database 0 Map.empty

keys :: Database a -> [DatabaseKey]
keys    = Map.keys . db

create :: a -> Database a -> Database a
create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db

update :: DatabaseKey -> a -> Database a -> Database a
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db

delete :: DatabaseKey -> Database a -> Database a
delete key   (Database newkey db) = Database newkey     $ Map.delete key db

lookup :: DatabaseKey -> Database a -> Maybe a
lookup key   (Database _      db) = Map.lookup key db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
type DataItem = (String, String)

showDataItem :: ([Char], [Char]) -> [Char]
showDataItem (firstname, lastname) = lastname ++ ", " ++ firstname

-- single text entry
reactiveTextEntry :: Frameworks t
    => TextCtrl a
    -> Behavior t String              -- text value
    -> Moment t (Tidings t String)    -- user changes
reactiveTextEntry w btext = do
    eUser <- eventText w        -- user changes

    -- filter text setting that are simultaneous with user events
    etext <- changes btext
    let
        etext2 = fst $ split $ unionWith (curry snd) (Left () <$ etext) (Right () <$ eUser)
        btext2 = imposeChanges btext etext2

    sink w [ text :== btext2 ]  -- display value
    return $ tidings btext eUser

-- whole data item (consisting of two text entries)
reactiveDataItem :: Frameworks t
    => (TextCtrl a, TextCtrl b)
    -> Behavior t (Maybe DataItem)
    -> Moment t (Tidings t DataItem)
reactiveDataItem (firstname,lastname) binput = do
    t1 <- reactiveTextEntry firstname (fst . fromMaybe ("","") <$> binput)
    t2 <- reactiveTextEntry lastname  (snd . fromMaybe ("","") <$> binput)
    return $ (,) <$> t1 <*> t2


{-----------------------------------------------------------------------------
    reactive list display
    
    Display a list of (distinct) items in a list box.
    The current selection contains one or no items.
    Changing the set may unselect the current item,
        but will not change it to another item.
------------------------------------------------------------------------------}
reactiveListDisplay :: forall t a b. (Ord a, Frameworks t)
    => SingleListBox b          -- ListBox widget to use
    -> Behavior t [a]           -- list of items
    -> Behavior t (Maybe a)     -- selected element
    -> Behavior t (a -> String) -- display an item
    -> Moment t
        (Tidings t (Maybe a))   -- current selection as item (possibly empty)
reactiveListDisplay w bitems bsel bdisplay = do
    -- animate output items
    sink w [ items :== map <$> bdisplay <*> bitems ]
   
    -- animate output selection
    let bindices :: Behavior t (Map.Map a Int)
        bindices = (Map.fromList . flip zip [0..]) <$> bitems
        bindex   = (\m a -> fromMaybe (-1) $ flip Map.lookup m =<< a) <$>
                    bindices <*> bsel
    sink w [ selection :== bindex ]

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 :: Behavior t (Map.Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems
    esel <- eventSelection w
    return $ tidings bsel $ flip Map.lookup <$> bindices2 <@> esel


--------------------------------------------------------------------------------


-- | Data type representing a behavior 'facts'
-- and suggestions to change it 'rumors'.
data Tidings t a = T { facts :: Behavior t a, rumors :: Event t a }

-- | Smart constructor. Combine facts and rumors into 'Tidings'.
tidings :: Behavior t a -> Event t a -> Tidings t a
tidings b e = T b (calm e)

instance Functor (Tidings t) where
    fmap f (T b e) = T (fmap f b) (fmap f e)

-- | The applicative instance combines 'rumors'
-- and uses 'facts' when some of the 'rumors' are not available.
instance Applicative (Tidings t) where
    pure x  = T (pure x) never
    f <*> x = uncurry ($) <$> pair f x

pair :: Tidings t a -> Tidings t b -> Tidings t (a,b)
pair (T bx ex) (T by ey) = T b e
    where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\(x,_) (_,y) -> (x,y)) x y
