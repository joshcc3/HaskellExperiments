{-# LANGUAGE OverloadedStrings #-}

module Templates.Widgets where


import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Bootstrap
import Text.Blaze.BootstrapM
import Data.Monoid
import Yesod



btnDefault  :: MonadWidget m => m ()
btnDefault = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-default" $ "Default"

btnPrimary :: MonadWidget m => m ()
btnPrimary = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-primary" $ "Primary"

btnSuccess :: MonadWidget m => m ()
btnSuccess = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-success" $ "Success"

btnInfo :: MonadWidget m => m ()
btnInfo = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-info" $ "Info"

btnWarning :: MonadWidget m => m ()
btnWarning = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-warning" $ "Warning"

btnDanger :: MonadWidget m => m ()
btnDanger = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-danger" $ "Danger"


btnLine :: MonadWidget m => m ()
btnLine = toWidget $ \_ -> button ! type_ "button" ! class_ "btn btn-lg btn-link" $ "Link"



navbar1 :: MonadWidget m => m ()
navbar1 = toWidget $ \_ -> do 
           H.nav ! A.class_ "navbar navbar-inverse navbar-fixed-top" $
               H.div ! A.class_ "container" $ do
                  H.div ! A.class_ "navbar-header" $ do
                      H.button ! A.type_ "button" ! 
                         A.class_ "navbar-toggle collapsed" !
                           dataToggle "collapse" !
                            dataTarget "#navbar" ! 
                              ariaExpanded True ! 
                                ariaControls "navbar" $ do
                                    H.span ! A.class_ "sr-only" $ "Toggle Navigation"
                                    H.span ! A.class_ "icon-bar" $ ""
                                    H.span ! A.class_ "icon-bar" $ ""
                                    H.span ! A.class_ "icon-bar" $ ""
                      H.a ! A.class_ "navbar-brand" ! A.href "#" $ "JCrib"
                  H.div ! A.id "navbar" ! A.class_ "navbar-collapse collapse" $ do
                     H.ul ! A.class_ "nav navbar-nav" $ do
                       H.li ! A.class_ "active" $
                            H.a ! A.href "#" $ "Crib"
                       H.li $ H.a ! A.href "#about" $ "'Bout"
                       H.li $ H.a ! A.href "#contact" $ "Hola?"
                       H.li ! A.class_ "dropdown" $ do
                          H.a ! A.href "#" ! A.class_ "dropdown-toggle" ! 
                           dataToggle "dropdown" ! role "button" ! 
                            ariaExpanded False $ 
                                 "Dropdown" <> (H.span ! A.class_ "caret" $ "")
                          H.ul ! A.class_ "dropdown-menu" ! role "menu" $ do
                            H.li $ H.a ! A.href "#" $ "Accion Uno"
                            H.li $ H.a ! A.href "#" $ "Accion Dos"
                            H.li $ H.a ! A.href "#" $ "Accion TTres"
                            H.li ! A.class_ "divider" $ ""
                            H.li ! A.class_ "dropdown-header" $ "Ole Ole Ole"
                            H.li $ H.a ! A.href "#" $ "Here we go"



dropDown :: MonadWidget m => m ()
dropDown = toWidget $ \_ -> 
  H.div ! class_ "dropdown theme-dropdown clearfix" $ do
    a ! A.id "dropdownMenu1" ! href "#" ! class_ "sr-only dropdown-toggle" ! dataAttribute "toggle" "dropdown" $ do
        "Dropdown"
        H.span ! class_ "caret" $ mempty
    ul ! class_ "dropdown-menu" $ do
        li ! class_ "active" $ a ! tabindex "-1" ! href "#" $ "Action"
        li $ a ! tabindex "-1" ! href "#" $ "Another action"
        li $ a ! tabindex "-1" ! href "#" $ "Something else here"
        li ! class_ "divider" $ mempty
        li $ a ! tabindex "-1" ! href "#" $ "Separated link"

alertSuccess :: MonadWidget m => Html -> m () 
alertSuccess m  = toWidget $ \render -> H.div ! class_ "alert alert-success"  $ m

alertInfo :: MonadWidget m => Html -> m () 
alertInfo m  = toWidget $ \render -> H.div ! class_ "alert alert-info" $ m


alertWarning :: MonadWidget m => Html -> m () 
alertWarning m = toWidget $ \render -> H.div ! class_ "alert alert-warning" $ m

alertDanger :: MonadWidget m => Html -> m () 
alertDanger m = toWidget $ \render -> H.div ! class_ "alert alert-danger" $ m

