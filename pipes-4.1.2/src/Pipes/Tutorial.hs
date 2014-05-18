{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| Conventional Haskell stream programming forces you to choose only two of the
    following three features:

    * Effects

    * Streaming

    * Composability

    If you sacrifice /Effects/ you get Haskell's pure and lazy lists, which you
    can transform using composable functions in constant space, but without
    interleaving effects.

    If you sacrifice /Streaming/ you get 'mapM', 'forM' and
    \"ListT done wrong\", which are composable and effectful, but do not return
    a single result until the whole list has first been processed and loaded
    into memory.

    If you sacrifice /Composability/ you write a tightly coupled read,
    transform, and write loop in 'IO', which is streaming and effectful, but is
    not modular or separable.

    @pipes@ gives you all three features: effectful, streaming, and composable
    programming.  @pipes@ also provides a wide variety of stream programming
    abstractions which are all subsets of a single unified machinery:

    * effectful 'Producer's (like generators),

    * effectful 'Consumer's (like iteratees),

    * effectful 'Pipe's (like Unix pipes), and:

    * 'ListT' done right.

    All of these are connectable and you can combine them together in clever and
    unexpected ways because they all share the same underlying type.

    @pipes@ requires a basic understanding of monad transformers, which you can
    learn about by reading either:

    * the paper \"Monad Transformers - Step by Step\",

    * chapter 18 of \"Real World Haskell\" on monad transformers, or:

    * the documentation of the @transformers@ library.

    If you want a Quick Start guide to @pipes@, read the documentation in
    "Pipes.Prelude" from top to bottom.

    This tutorial is more extensive and explains the @pipes@ API in greater
    detail and illustrates several idioms.
-}

module Pipes.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Producers
    -- $producers

    -- * Composability
    -- $composability

    -- * Consumers
    -- $consumers

    -- * Pipes
    -- $pipes

    -- * ListT
    -- $listT

    -- * Tricks
    -- $tricks

    -- * Conclusion
    -- $conclusion

    -- * Appendix: Types
    -- $types

    -- * Appendix: Time Complexity
    -- $timecomplexity
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer.Strict
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Prelude hiding ((.), id)

{- $introduction
    The @pipes@ library decouples stream processing stages from each other so
    that you can mix and match diverse stages to produce useful streaming
    programs.  If you are a library writer, @pipes@ lets you package up
    streaming components into a reusable interface.  If you are an application
    writer, @pipes@ lets you connect pre-made streaming components with minimal
    effort to produce a highly-efficient program that streams data in constant
    memory.

    To enforce loose coupling, components can only communicate using two
    commands:

    * 'yield': Send output data

    * 'await': Receive input data

    @pipes@ has four types of components built around these two commands:

    * 'Producer's can only 'yield' values and they model streaming sources

    * 'Consumer's can only 'await' values and they model streaming sinks

    * 'Pipe's can both 'yield' and 'await' values and they model stream
      transformations

    * 'Effect's can neither 'yield' nor 'await' and they model non-streaming
      components

    You can connect these components together in four separate ways which
    parallel the four above types:

    * 'for' handles 'yield's

    * ('>~') handles 'await's

    * ('>->') handles both 'yield's and 'await's

    * ('>>=') handles return values

    As you connect components their types will change to reflect inputs and
    outputs that you've fused away.  You know that you're done connecting things
    when you get an 'Effect', meaning that you have handled all inputs and
    outputs.  You run this final 'Effect' to begin streaming.
-}

{- $producers
    'Producer's are effectful streams of input.  Specifically, a 'Producer' is a
    monad transformer that extends any base monad with a new 'yield' command.
    This 'yield' command lets you send output downstream to an anonymous
    handler, decoupling how you generate values from how you consume them.

    The following @stdinLn@ 'Producer' shows how to incrementally read in
    'String's from standard input and 'yield' them downstream, terminating
    gracefully when reaching the end of the input:

> -- echo.hs
>
> import Control.Monad (unless)
> import Pipes
> import System.IO (isEOF)
>
> --         +--------+-- A 'Producer' that yields 'String's
> --         |        |
> --         |        |      +-- Every monad transformer has a base monad.
> --         |        |      |   This time the base monad is 'IO'.
> --         |        |      |  
> --         |        |      |  +-- Every monadic action has a return value.
> --         |        |      |  |   This action returns '()' when finished
> --         v        v      v  v
> stdinLn :: Producer String IO ()
> stdinLn = do
>     eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
>     unless eof $ do
>         str <- lift getLine
>         yield str            -- 'yield' the 'String'
>         stdinLn              -- Loop

    'yield' emits a value, suspending the current 'Producer' until the value is
    consumed.  If nobody consumes the value (which is possible) then 'yield'
    never returns.  You can think of 'yield' as having the following type:

@
 'yield' :: 'Monad' m => a -> 'Producer' a m ()
@

    The true type of 'yield' is actually more general and powerful.  Throughout
    the tutorial I will present type signatures like this that are simplified at
    first and then later reveal more general versions.  So read the above type
    signature as simply saying: \"You can use 'yield' within a 'Producer', but
    you may be able to use 'yield' in other contexts, too.\"

    Click the link to 'yield' to navigate to its documentation.  There you will
    see that 'yield' actually uses the 'Producer'' (with an apostrophe) type
    synonym which hides a lot of polymorphism behind a simple veneer.  The
    documentation for 'yield' says that you can also use 'yield' within a
    'Pipe', too, because of this polymorphism:

@
 'yield' :: 'Monad' m => a -> 'Pipe' x a m ()
@

    Use simpler types like these to guide you until you understand the fully
    general type.

    'for' loops are the simplest way to consume a 'Producer' like @stdinLn@.
    'for' has the following type:

@
 \-\-                +-- Producer      +-- The body of the   +-- Result
 \-\-                |   to loop       |   loop              |
 \-\-                v   over          v                     v
 \-\-                --------------    ------------------    ----------
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Effect' m ()) -> 'Effect' m r
@

    @(for producer body)@ loops over @(producer)@, substituting each 'yield' in
    @(producer)@ with @(body)@.

    You can also deduce that behavior purely from the type signature:

    * The body of the loop takes exactly one argument of type @(a)@, which is
      the same as the output type of the 'Producer'.  Therefore, the body of the
      loop must get its input from that 'Producer' and nowhere else.

    * The return value of the input 'Producer' matches the return value of the
      result, therefore 'for' must loop over the entire 'Producer' and not skip
      anything.

    The above type signature is not the true type of 'for', which is actually
    more general.  Think of the above type signature as saying: \"If the first
    argument of 'for' is a 'Producer' and the second argument returns an
    'Effect', then the final result must be an 'Effect'.\"

    Click the link to 'for' to navigate to its documentation.  There you will
    see the fully general type and underneath you will see equivalent simpler
    types.  One of these says that if the body of the loop is a 'Producer', then
    the result is a 'Producer', too:

@
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' b m ()) -> 'Producer' b m r
@

    The first type signature I showed for 'for' was a special case of this
    slightly more general signature because a 'Producer' that never 'yield's is
    also an 'Effect':

@
 data 'X'  -- The uninhabited type

\ type 'Effect' m r = 'Producer' 'X' m r
@

    This is why 'for' permits two different type signatures.  The first type
    signature is just a special case of the second one:

@
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' b m ()) -> 'Producer' b m r

\ -- Specialize \'b\' to \'X\'
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Producer' 'X' m ()) -> 'Producer' 'X' m r

\ -- Producer X = Effect
 'for' :: 'Monad' m => 'Producer' a m r -> (a -> 'Effect'     m ()) -> 'Effect'     m r
@

    This is the same trick that all @pipes@ functions use to work with various
    combinations of 'Producer's, 'Consumer's, 'Pipe's, and 'Effect's.  Each
    function really has just one general type, which you can then simplify down
    to multiple useful alternative types.

    Here's an example use of a 'for' @loop@, where the second argument (the
    loop body) is an 'Effect':

> -- echo.hs
>
> loop :: Effect IO ()
> loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"
>     lift $ putStrLn str          -- The body of the 'for' loop
>
> -- more concise: loop = for stdinLn (lift . putStrLn)

    In this example, 'for' loops over @stdinLn@ and replaces every 'yield' in
    @stdinLn@ with the body of the loop, printing each line.  This is exactly
    equivalent to the following code, which I've placed side-by-side with the
    original definition of @stdinLn@ for comparison:

> loop = do                      |  stdinLn = do
>     eof <- lift isEOF          |      eof <- lift isEOF
>     unless eof $ do            |      unless eof $ do
>         str <- lift getLine    |          str <- lift getLine
>         (lift . putStrLn) str  |          yield str
>         loop                   |          stdinLn

    You can think of 'yield' as creating a hole and a 'for' loop is one way to
    fill that hole.

    Notice how the final @loop@ only 'lift's actions from the base monad and
    does nothing else.  This property is true for all 'Effect's, which are just
    glorified wrappers around actions in the base monad.  This means we can run
    these 'Effect's to remove their 'lift's and lower them back to the
    equivalent computation in the base monad:

@
 'runEffect' :: 'Monad' m => 'Effect' m r -> m r
@

    This is the real type signature of 'runEffect', which refuses to accept
    anything other than an 'Effect'.  This ensures that we handle all inputs and
    outputs before streaming data:

> -- echo.hs
>
> main :: IO ()
> main = runEffect loop

    ... or you could inline the entire @loop@ into the following one-liner:

> main = runEffect $ for stdinLn (lift . putStrLn)

    Our final program loops over standard input and echoes every line to
    standard output until we hit @Ctrl-D@ to end the input stream:

> $ ghc -O2 echo.hs
> $ ./echo
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> <Ctrl-D>
> $

    The final behavior is indistinguishable from just removing all the 'lift's
    from @loop@:

> main = do               |  loop = do
>     eof <- isEof        |      eof <- lift isEof
>     unless eof $ do     |      unless eof $ do
>         str <- getLine  |          str <- lift getLine
>         putStrLn str    |          (lift . putStrLn) str
>         main            |          loop

    This @main@ is what we might have written by hand if we were not using
    @pipes@, but with @pipes@ we can decouple the input and output logic from
    each other.  When we connect them back together, we still produce streaming
    code equivalent to what a sufficiently careful Haskell programmer would
    have written.

    You can also use 'for' to loop over lists, too.  To do so, convert the list
    to a 'Producer' using 'each', which is exported by default from "Pipes":

> each :: Monad m => [a] -> Producer a m ()
> each as = mapM_ yield as

    Combine 'for' and 'each' to iterate over lists using a \"foreach\" loop:

>>> runEffect $ for (each [1..4]) (lift . print)
1
2
3
4

    'each' is actually more general and works for any 'Foldable':

@
 'each' :: ('Monad' m, 'Foldable' f) => f a -> 'Producer' a m ()
@

     So you can loop over any 'Foldable' container or even a 'Maybe':

>>> runEffect $ for (each (Just 1)) (lift . print)
1

-}

{- $composability
    You might wonder why the body of a 'for' loop can be a 'Producer'.  Let's
    test out this feature by defining a new loop body that @duplicate@s every
    value:

> -- nested.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude already has 'stdinLn'
> 
> duplicate :: Monad m => a -> Producer a m ()
> duplicate x = do
>     yield x
>     yield x
>
> loop :: Producer String IO ()
> loop = for P.stdinLn duplicate
>
> -- This is the exact same as:
> --
> -- loop = for P.stdinLn $ \x -> do
> --     yield x
> --     yield x

    This time our @loop@ is a 'Producer' that outputs 'String's, specifically
    two copies of each line that we read from standard input.  Since @loop@ is a
    'Producer' we cannot run it because there is still unhandled output.
    However, we can use yet another 'for' to handle this new duplicated stream:

> -- nested.hs
>
> main = runEffect $ for loop (lift . putStrLn)

    This creates a program which echoes every line from standard input to
    standard output twice:

> $ ./nested
> Test<Enter>
> Test
> Test
> ABC<Enter>
> ABC
> ABC
> <Ctrl-D>
> $

    But is this really necessary?  Couldn't we have instead written this using a
    nested for loop?

> main = runEffect $
>     for P.stdinLn $ \str1 ->
>         for (duplicate str1) $ \str2 ->
>             lift $ putStrLn str2

    Yes, we could have!  In fact, this is a special case of the following
    equality, which always holds no matter what:

@
 \-\- s :: Monad m =>      'Producer' a m ()  -- i.e. \'P.stdinLn\'
 \-\- f :: Monad m => a -> 'Producer' b m ()  -- i.e. \'duplicate\'
 \-\- g :: Monad m => b -> 'Producer' c m ()  -- i.e. \'(lift . putStrLn)\'

\ for (for s f) g = for s (\\x -> for (f x) g)
@

    We can understand the rationale behind this equality if we first define the
    following operator that is the point-free counterpart to 'for':

@
 (~>) :: Monad m
      => (a -> 'Producer' b m ())
      -> (b -> 'Producer' c m ())
      -> (a -> 'Producer' c m ())
 (f ~> g) x = for (f x) g
@

    Using ('~>') (pronounced \"into\"), we can transform our original equality
    into the following more symmetric equation:

@
 f :: Monad m => a -> 'Producer' b m ()
 g :: Monad m => b -> 'Producer' c m ()
 h :: Monad m => c -> 'Producer' d m ()

\ \-\- Associativity
 (f ~> g) ~> h = f ~> (g ~> h)
@

    This looks just like an associativity law.  In fact, ('~>') has another nice
    property, which is that 'yield' is its left and right identity:

> -- Left Identity
> yield ~> f = f

> -- Right Identity
> f ~> yield = f

    In other words, 'yield' and ('~>') form a 'Category', specifically the
    generator category, where ('~>') plays the role of the composition operator
    and 'yield' is the identity.  If you don't know what a 'Category' is, that's
    okay, and category theory is not a prerequisite for using @pipes@.  All you
    really need to know is that @pipes@ uses some simple category theory to keep
    the API intuitive and easy to use.

    Notice that if we translate the left identity law to use 'for' instead of
    ('~>') we get:

> for (yield x) f = f x

    This just says that if you iterate over a pure single-element 'Producer',
    then you could instead cut out the middle man and directly apply the body of
    the loop to that single element.

    If we translate the right identity law to use 'for' instead of ('~>') we
    get:

> for s yield = s

    This just says that if the only thing you do is re-'yield' every element of
    a stream, you get back your original stream.

    These three \"for loop\" laws summarize our intuition for how 'for' loops
    should behave and because these are 'Category' laws in disguise that means
    that 'Producer's are composable in a rigorous sense of the word.

    In fact, we get more out of this than just a bunch of equations.  We also
    get a useful operator: ('~>').  We can use this operator to condense
    our original code into the following more succinct form that composes two
    transformations:

> main = runEffect $ for P.stdinLn (duplicate ~> lift . putStrLn)

    This means that we can also choose to program in a more functional style and
    think of stream processing in terms of composing transformations using
    ('~>') instead of nesting a bunch of 'for' loops.

    The above example is a microcosm of the design philosophy behind the @pipes@
    library:

    * Define the API in terms of categories

    * Specify expected behavior in terms of category laws

    * Think compositionally instead of sequentially
-}

{- $consumers
    Sometimes you don't want to use a 'for' loop because you don't want to consume
    every element of a 'Producer' or because you don't want to process every
    value of a 'Producer' the exact same way.

    The most general solution is to externally iterate over the 'Producer' using
    the 'next' command:

@
 'next' :: 'Monad' m => 'Producer' a m r -> m ('Either' r (a, 'Producer' a m r))
@

    Think of 'next' as pattern matching on the head of the 'Producer'.  This
    'Either' returns a 'Left' if the 'Producer' is done or it returns a 'Right'
    containing the next value, @a@, along with the remainder of the 'Producer'.

    However, sometimes we can get away with something a little more simple and
    elegant, like a 'Consumer', which represents an effectful sink of values.  A
    'Consumer' is a monad transformer that extends the base monad with a new
    'await' command. This 'await' command lets you receive input from an
    anonymous upstream source.

    The following @stdoutLn@ 'Consumer' shows how to incrementally 'await'
    'String's and print them to standard output, terminating gracefully when
    receiving a broken pipe error:

> import Control.Monad (unless)
> import Control.Exception (try, throwIO)
> import qualified GHC.IO.Exception as G
> import Pipes
>
> --          +--------+-- A 'Consumer' that awaits 'String's
> --          |        |
> --          v        v
> stdoutLn :: Consumer String IO ()
> stdoutLn = do
>     str <- await  -- 'await' a 'String'
>     x   <- lift $ try $ putStrLn str
>     case x of
>         -- Gracefully terminate if we got a broken pipe error
>         Left e@(G.IOError { G.ioe_type = t}) ->
>             lift $ unless (t == G.ResourceVanished) $ throwIO e
>         -- Otherwise loop
>         Right () -> stdoutLn

    'await' is the dual of 'yield': we suspend our 'Consumer' until we receive a
    new value.  If nobody provides a value (which is possible) then 'await'
    never returns.  You can think of 'await' as having the following type:

@
 'await' :: 'Monad' m => 'Consumer' a m a
@

    One way to feed a 'Consumer' is to repeatedly feed the same input using
    ('>~') (pronounced \"feed\"):

@
 \-\-                 +- Feed       +- Consumer to    +- Returns new
 \-\-                 |  action     |  feed           |  Effect
 \-\-                 v             v                 v  
 \-\-                 ----------    --------------    ----------
 ('>~') :: 'Monad' m => 'Effect' m b -> 'Consumer' b m c -> 'Effect' m c
@

    @(draw >~ consumer)@ loops over @(consumer)@, substituting each 'await' in
    @(consumer)@ with @(draw)@.

    So the following code replaces every 'await' in 'P.stdoutLn' with
    @(lift getLine)@ and then removes all the 'lift's:

>>> runEffect $ lift getLine >~ stdoutLn
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
...

    You might wonder why ('>~') uses an 'Effect' instead of a raw action in the
    base monad.  The reason why is that ('>~') actually permits the following
    more general type:

@
 ('>~') :: 'Monad' m => 'Consumer' a m b -> 'Consumer' b m c -> 'Consumer' a m c
@

    ('>~') is the dual of ('~>'), composing 'Consumer's instead of 'Producer's.

    This means that you can feed a 'Consumer' with yet another 'Consumer' so
    that you can 'await' while you 'await'.  For example, we could define the
    following intermediate 'Consumer' that requests two 'String's and returns
    them concatenated:

> doubleUp :: Monad m => Consumer String m String
> doubleUp = do
>     str1 <- await
>     str2 <- await
>     return (str1 ++ str2)
>
> -- more concise: doubleUp = (++) <$> await <*> await

    We can now insert this in between @(lift getLine)@ and @stdoutLn@ and see
    what happens:

>>> runEffect $ lift getLine >~ doubleUp >~ stdoutLn
Test<Enter>
ing<Enter>
Testing
ABC<Enter>
DEF<Enter>
ABCDEF
42<Enter>
000<Enter>
42000
...

    'doubleUp' splits every request from 'stdoutLn' into two separate requests
    and
    returns back the concatenated result.

    We didn't need to parenthesize the above chain of ('>~') operators, because
    ('>~') is associative:

> -- Associativity
> (f >~ g) >~ h = f >~ (g >~ h)

    ... so we can always omit the parentheses since the meaning is unambiguous:

> f >~ g >~ h

    Also, ('>~') has an identity, which is 'await'!

> -- Left identity
> await >~ f = f
>
> -- Right Identity
> f >~ await = f

    In other words, ('>~') and 'await' form a 'Category', too, specifically the
    iteratee category, and 'Consumer's are also composable.
-}

{- $pipes
    Our previous programs were unsatisfactory because they were biased either
    towards the 'Producer' end or the 'Consumer' end.  As a result, we had to
    choose between gracefully handling end of input (using 'P.stdinLn') or
    gracefully handling end of output (using 'P.stdoutLn'), but not both at the
    same time.

    However, we don't need to restrict ourselves to using 'Producer's
    exclusively or 'Consumer's exclusively.  We can connect 'Producer's and
    'Consumer's directly together using ('>->') (pronounced \"pipe\"):

@
 ('>->') :: 'Monad' m => 'Producer' a m r -> 'Consumer' a m r -> 'Effect' m r
@

    This returns an 'Effect' which we can run:

> -- echo2.hs
>
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude also provides 'stdoutLn'
>
> main = runEffect $ P.stdinLn >-> P.stdoutLn

    This program is more declarative of our intent: we want to stream values
    from 'P.stdinLn' to 'P.stdoutLn'.  The above \"pipeline\" not only echoes
    standard input to standard output, but also handles both end of input and
    broken pipe errors:

> $ ./echo2
> Test<Enter>
> Test
> ABC<Enter>
> ABC
> 42<Enter>
> 42
> <Ctrl-D>
> $

    ('>->') is \"pull-based\" meaning that control flow begins at the most
    downstream component (i.e. 'P.stdoutLn' in the above example).  Any time a
    component 'await's a value it blocks and transfers control upstream and
    every time a component 'yield's a value it blocks and restores control back
    downstream, satisfying the 'await'.  So in the above example, ('>->')
    matches every 'await' from 'P.stdoutLn' with a 'yield' from 'P.stdinLn'.

    Streaming stops when either 'P.stdinLn' terminates (i.e. end of input) or
    'P.stdoutLn' terminates (i.e. broken pipe).  This is why ('>->') requires
    that both the 'Producer' and 'Consumer' share the same type of return value:
    whichever one terminates first provides the return value for the entire
    'Effect'.

    Let's test this by modifying our 'Producer' and 'Consumer' to each return a
    diagnostic 'String':

> -- echo3.hs
>
> import Control.Applicative ((<$))  -- (<$) modifies return values
> import Pipes
> import qualified Pipes.Prelude as P
> import System.IO
>
> main = do
>     hSetBuffering stdout NoBuffering
>     str <- runEffect $
>         ("End of input!" <$ P.stdinLn) >-> ("Broken pipe!" <$ P.stdoutLn)
>     hPutStrLn stderr str

    This lets us diagnose whether the 'Producer' or 'Consumer' terminated first:

> $ ./echo3
> Test<Enter>
> Test
> <Ctrl-D>
> End of input!
> $ ./echo3 | perl -e 'close STDIN'
> Test<Enter>
> Broken pipe!
> $

    You might wonder why ('>->') returns an 'Effect' that we have to run instead
    of directly returning an action in the base monad.  This is because you can
    connect things other than 'Producer's and 'Consumer's, like 'Pipe's, which
    are effectful stream transformations.

    A 'Pipe' is a monad transformer that is a mix between a 'Producer' and
    'Consumer', because a 'Pipe' can both 'await' and 'yield'.  The following
    example 'Pipe' is analagous to the Prelude's 'take', only allowing a fixed
    number of values to flow through:

> -- take.hs
>
> import Control.Monad (replicateM_)
> import Pipes
> import Prelude hiding (take)
>
> --              +--------- A 'Pipe' that
> --              |    +---- 'await's 'a's and
> --              |    | +-- 'yield's 'a's
> --              |    | |
> --              v    v v
> take ::  Int -> Pipe a a IO ()
> take n = do
>     replicateM_ n $ do                     -- Repeat this block 'n' times
>         x <- await                         -- 'await' a value of type 'a'
>         yield x                            -- 'yield' a value of type 'a'
>     lift $ putStrLn "You shall not pass!"  -- Fly, you fools!

    You can use 'Pipe's to transform 'Producer's, 'Consumer's, or even other
    'Pipe's using the same ('>->') operator:

@
 ('>->') :: 'Monad' m => 'Producer' a m r -> 'Pipe'   a b m r -> 'Producer' b m r
 ('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Consumer' b m r -> 'Consumer' a m r
 ('>->') :: 'Monad' m => 'Pipe'   a b m r -> 'Pipe'   b c m r -> 'Pipe'   a c m r
@

    For example, you can compose 'P.take' after 'P.stdinLn' to limit the number
    of lines drawn from standard input:

> maxInput :: Int -> Producer String IO ()
> maxInput n = P.stdinLn >-> take n

>>> runEffect $ maxInput 3 >-> P.stdoutLn
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
You shall not pass!
>>>

    ... or you can pre-compose 'P.take' before 'P.stdoutLn' to limit the number
    of lines written to standard output:

> maxOutput :: Int -> Consumer String IO ()
> maxOutput n = take n >-> P.stdoutLn

>>> runEffect $ P.stdinLn >-> maxOutput 3
<Exact same behavior>

    Those both gave the same behavior because ('>->') is associative:

> (p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

    Therefore we can just leave out the parentheses:

>>> runEffect $ P.stdinLn >-> take 3 >-> P.stdoutLn
<Exact same behavior>

    ('>->') is designed to behave like the Unix pipe operator, except with less
    quirks.  In fact, we can continue the analogy to Unix by defining 'cat'
    (named after the Unix @cat@ utility), which reforwards elements endlessly:

> cat :: Monad m => Pipe a a m r
> cat = forever $ do
>     x <- await
>     yield x

     'cat' is the identity of ('>->'), meaning that 'cat' satisfies the
     following two laws:

> -- Useless use of 'cat'
> cat >-> p = p
>
> -- Forwarding output to 'cat' does nothing
> p >-> cat = p

    Therefore, ('>->') and 'cat' form a 'Category', specifically the category of
    Unix pipes, and 'Pipe's are also composable.

    A lot of Unix tools have very simple definitions when written using @pipes@:

> -- unix.hs
>
> import Control.Monad (forever)
> import Pipes
> import qualified Pipes.Prelude as P  -- Pipes.Prelude provides 'take', too
> import Prelude hiding (head)
>
> head :: Monad m => Int -> Pipe a a m ()
> head = P.take
>
> yes :: Monad m => Producer String m r
> yes = forever $ yield "y"
>
> main = runEffect $ yes >-> head 3 >-> P.stdoutLn

    This prints out 3 \'@y@\'s, just like the equivalent Unix pipeline:

> $ ./unix
> y
> y
> y
> $ yes | head -3
> y
> y
> y
> $

    This lets us write \"Haskell pipes\" instead of Unix pipes.  These are much
    easier to build than Unix pipes and we can connect them directly within
    Haskell for interoperability with the Haskell language and ecosystem.
-}

{- $listT
    @pipes@ also provides a \"ListT done right\" implementation.  This differs
    from the implementation in @transformers@ because this 'ListT':

    * obeys the monad laws, and

    * streams data immediately instead of collecting all results into memory.

    The latter property is actually an elegant consequence of obeying the monad
    laws.

    To bind a list within a 'ListT' computation, combine 'Select' and 'each':

> import Pipes
> 
> pair :: ListT IO (Int, Int)
> pair = do
>     x <- Select $ each [1, 2]
>     lift $ putStrLn $ "x = " ++ show x
>     y <- Select $ each [3, 4]
>     lift $ putStrLn $ "y = " ++ show y
>     return (x, y)

    You can then loop over a 'ListT' by using 'every':

@
 'every' :: 'Monad' m => 'ListT' m a -> 'Producer' a m ()
@

    So you can use your 'ListT' within a 'for' loop:

>>> runEffect $ for (every pair) (lift . print)
x = 1
y = 3
(1,3)
y = 4
(1,4)
x = 2
y = 3
(2,3)
y = 4
(2,4)

    ... or a pipeline:

>>> import qualified Pipes.Prelude as P
>>> runEffect $ every pair >-> P.print
<Exact same behavior>

    Note that 'ListT' is lazy and only produces as many elements as we request:

>>> runEffect $ for (every pair >-> P.take 2) (lift . print)
x = 1
y = 3
(1,3)
y = 4
(1,4)

    You can also go the other way, binding 'Producer's directly within a
    'ListT'.  In fact, this is actually what 'Select' was already doing:

@
 'Select' :: 'Producer' a m () -> 'ListT' m a
@

    This lets you write crazy code like:

> import Pipes
> import qualified Pipes.Prelude as P
> 
> input :: Producer String IO ()
> input = P.stdinLn >-> P.takeWhile (/= "quit")
> 
> name :: ListT IO String
> name = do
>     firstName <- Select input
>     lastName  <- Select input
>     return (firstName ++ " " ++ lastName)

    Here we're binding standard input non-deterministically (twice) as if it
    were an effectful list:

>>> runEffect $ every name >-> P.stdoutLn
Daniel<Enter>
Fischer<Enter>
Daniel Fischer
Wagner<Enter>
Daniel Wagner
quit<Enter>
Donald<Enter>
Stewart<Enter>
Donald Stewart
Duck<Enter>
Donald Duck
quit<Enter>
quit<Enter>
>>>

    Notice how this streams out values immediately as they are generated, rather
    than building up a large intermediate result and then printing all the
    values in one batch at the end.
-}

{- $tricks
    @pipes@ is more powerful than meets the eye so this section presents some
    non-obvious tricks you may find useful.

    Many pipe combinators will work on unusual pipe types and the next few
    examples will use the 'cat' pipe to demonstrate this.

    For example, you can loop over the output of a 'Pipe' using 'for', which is
    how 'P.map' is defined:

> map :: Monad m => (a -> b) -> Pipe a b m r
> map f = for cat $ \x -> yield (f x)
>
> -- Read this as: For all values flowing downstream, apply 'f'

    This is equivalent to:

> map f = forever $ do
>     x <- await
>     yield (f x)

    You can also feed a 'Pipe' input using ('>~').  This means we could have
    instead defined the @yes@ pipe like this:

> yes :: Monad m => Producer String m r
> yes = return "y" >~ cat
>
> -- Read this as: Keep feeding "y" downstream

    This is equivalent to:

> yes = forever $ yield "y"

    You can also sequence two 'Pipe's together.  This is how 'P.drop' is
    defined:

> drop :: Monad m => Int -> Pipe a a m r
> drop n = do
>     replicateM_ n await
>     cat

    This is equivalent to:

> drop n = do
>     replicateM_ n await
>     forever $ do
>         x <- await
>         yield x

    You can even compose pipes inside of another pipe:

> customerService :: Producer String IO ()
> customerService = do
>     each [ "Hello, how can I help you?"        -- Begin with a script
>          , "Hold for one second."
>          ]
>     P.stdinLn >-> P.takeWhile (/= "Goodbye!")  -- Now continue with a human

    Also, you can often use 'each' in conjunction with ('~>') to traverse nested
    data structures.  For example, you can print all non-'Nothing' elements
    from a doubly-nested list:

>>> runEffect $ (each ~> each ~> each ~> lift . print) [[Just 1, Nothing], [Just 2, Just 3]]
1
2
3

    Another neat thing to know is that 'every' has a more general type:

@
 'every' :: ('Monad' m, 'Enumerable' t) => t m a -> 'Producer' a m ()
@

    'Enumerable' generalizes 'Foldable' and if you have an effectful container
    of your own that you want others to traverse using @pipes@, just have your
    container implement the 'toListT' method of the 'Enumerable' class:

> class Enumerable t where
>     toListT :: Monad m => t m a -> ListT m a

    You can even use 'Enumerable' to traverse effectful types that are not even
    proper containers, like 'Control.Monad.Trans.Maybe.MaybeT':

> input :: MaybeT IO String
> input = do
>     str <- lift getLine
>     guard (str /= "Fail")
>     return str

>>> runEffect $ every input >-> P.stdoutLn
Test<Enter>
Test
>>> runEffect $ every input >-> P.stdoutLn
Fail<Enter>
>>>

-}

{- $conclusion
    This tutorial covers the concepts of connecting, building, and reading
    @pipes@ code.  However, this library is only the core component in an
    ecosystem of streaming components.  Derived libraries that build immediately
    upon @pipes@ include:

    * @pipes-concurrency@: Concurrent reactive programming and message passing

    * @pipes-parse@: Minimal utilities for stream parsing

    * @pipes-safe@: Resource management and exception safety for @pipes@

    These libraries provide functionality specialized to common streaming
    domains.  Additionally, there are several libraries on Hackage that provide
    even higher-level functionality, which you can find by searching under the
    \"Pipes\" category or by looking for packages with a @pipes-@ prefix in
    their name.  Current examples include:

    * @pipes-network@/@pipes-network-tls@: Networking

    * @pipes-zlib@: Compression and decompression

    * @pipes-binary@: Binary serialization

    * @pipes-attoparsec@: High-performance parsing

    * @pipes-aeson@: JSON serialization and deserialization

    Even these derived packages still do not explore the full potential of
    @pipes@ functionality, which actually permits bidirectional communication.
    Advanced @pipes@ users can explore this library in greater detail by
    studying the documentation in the "Pipes.Core" module to learn about the
    symmetry of the underlying 'Proxy' type and operators.

    To learn more about @pipes@, ask questions, or follow @pipes@ development,
    you can subscribe to the @haskell-pipes@ mailing list at:

    <https://groups.google.com/forum/#!forum/haskell-pipes>

    ... or you can mail the list directly at:

    <mailto:haskell-pipes@googlegroups.com>

    Additionally, for questions regarding types or type errors, you might find
    the following appendix on types very useful.
-}

{- $types
    @pipes@ uses parametric polymorphism (i.e. generics) to overload all
    operations.  You've probably noticed this overloading already::

    * 'yield' works within both 'Producer's and 'Pipe's

    * 'await' works within both 'Consumer's and 'Pipe's

    * ('>->') connects 'Producer's, 'Consumer's, and 'Pipe's in varying ways

    This overloading is great when it works, but when connections fail they
    produce type errors that appear intimidating at first.  This section
    explains the underlying types so that you can work through type errors
    intelligently.

    'Producer's, 'Consumer's, 'Pipe's, and 'Effect's are all special cases of a
    single underlying type: a 'Proxy'.  This overarching type permits fully
    bidirectional communication on both an upstream and downstream interface.
    You can think of it as having the following shape:

> Proxy a' a b' b m r
>
> Upstream | Downstream
>     +---------+
>     |         |
> a' <==       <== b'  -- Information flowing upstream
>     |         |
> a  ==>       ==> b   -- Information flowing downstream
>     |    |    |
>     +----|----+
>          v
>          r

    The four core types do not use the upstream flow of information.  This means
    that the @a'@ and @b'@ in the above diagram go unused unless you use the
    more advanced features provided in "Pipes.Core".

    @pipes@ uses type synonyms to hide unused inputs or outputs and clean up
    type signatures.  These type synonyms come in two flavors:

    * Concrete type synonyms that explicitly close unused inputs and outputs of
      the 'Proxy' type

    * Polymorphic type synonyms that don't explicitly close unused inputs or
      outputs

    The concrete type synonyms use @()@ to close unused inputs and 'X' (the
    uninhabited type) to close unused outputs:

    * 'Effect': explicitly closes both ends, forbidding 'await's and 'yield's

> type Effect = Proxy X () () X
>
>  Upstream | Downstream
>     +---------+
>     |         |
> X  <==       <== ()
>     |         |
> () ==>       ==> X
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Producer': explicitly closes the upstream end, forbidding 'await's

> type Producer b = Proxy X () () b
>
> Upstream | Downstream
>     +---------+
>     |         |
> X  <==       <== ()
>     |         |
> () ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Consumer': explicitly closes the downstream end, forbidding 'yield's

> type Consumer a = Proxy () a () X
>
> Upstream | Downstream
>     +---------+
>     |         |
> () <==       <== ()
>     |         |
> a  ==>       ==> X
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Pipe': marks both ends open, allowing both 'await's and 'yield's

> type Pipe a b = Proxy () a () b
>
> Upstream | Downstream
>     +---------+
>     |         |
> () <==       <== ()
>     |         |
> a  ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    When you compose 'Proxy's using ('>->') all you are doing is placing them
    side by side and fusing them laterally.  For example, when you compose a
    'Producer', 'Pipe', and a 'Consumer', you can think of information flowing
    like this:

>        Producer                Pipe                 Consumer
>     +-----------+          +----------+          +------------+
>     |           |          |          |          |            |
> X  <==         <==   ()   <==        <==   ()   <==          <== ()
>     |  stdinLn  |          |  take 3  |          |  stdoutLn  |
> () ==>         ==> String ==>        ==> String ==>          ==> X
>     |     |     |          |    |     |          |      |     |
>     +-----|-----+          +----|-----+          +------|-----+
>           v                     v                       v
>           ()                    ()                      ()

     Composition fuses away the intermediate interfaces, leaving behind an
     'Effect':

>                    Effect
>     +-----------------------------------+
>     |                                   |
> X  <==                                 <== ()
>     |  stdinLn >-> take 3 >-> stdoutLn  |
> () ==>                                 ==> X
>     |                                   |
>     +----------------|------------------+
>                      v
>                      ()

    @pipes@ also provides polymorphic type synonyms with apostrophes at the end
    of their names.  These use universal quantification to leave open any unused
    input or output ends (which I mark using @*@):

    * 'Producer'': marks the upstream end unused but still open

> type Producer' b m r = forall x' x . Proxy x' x () b m r
>
> Upstream | Downstream
>     +---------+
>     |         |
>  * <==       <== ()
>     |         |
>  * ==>       ==> b
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Consumer'': marks the downstream end unused but still open

> type Consumer' a m r = forall y' y . Proxy () a y' y m r
>
> Upstream | Downstream
>     +---------+
>     |         |
> () <==       <== * 
>     |         |
> a  ==>       ==> *
>     |    |    |
>     +----|----+
>          v
>          r

    * 'Effect'': marks both ends unused but still open

> type Effect' m r = forall x' x y' y . Proxy x' x y' y m r
>
> Upstream | Downstream
>     +---------+
>     |         |
>  * <==       <== * 
>     |         |
>  * ==>       ==> *
>     |    |    |
>     +----|----+
>          v
>          r

    Note that there is no polymorphic generalization of a 'Pipe'.

    Like before, if you compose a 'Producer'', a 'Pipe', and a 'Consumer'':

>        Producer'               Pipe                 Consumer'
>     +-----------+          +----------+          +------------+
>     |           |          |          |          |            |
>  * <==         <==   ()   <==        <==   ()   <==          <== *
>     |  stdinLn  |          |  take 3  |          |  stdoutLn  |
>  * ==>         ==> String ==>        ==> String ==>          ==> *
>     |     |     |          |     |    |          |      |     |
>     +-----|-----+          +-----|----+          +------|-----+
>           v                      v                      v
>           ()                     ()                     ()

    ... they fuse into an 'Effect'':

>                    Effect'
>     +-----------------------------------+
>     |                                   |
>  * <==                                 <== *
>     |  stdinLn >-> take 3 >-> stdoutLn  |
>  * ==>                                 ==> *
>     |                                   |
>     +----------------|------------------+
>                      v
>                      ()

    Polymorphic type synonyms come in handy when you want to keep the type as
    general as possible.  For example, the type signature for 'yield' uses
    'Producer'' to keep the type signature simple while still leaving the
    upstream input end open:

@
 'yield' :: 'Monad' m => a -> 'Producer'' a m ()
@

    This type signature lets us use 'yield' within a 'Pipe', too, because the
    'Pipe' type synonym is a special case of the polymorphic 'Producer'' type 
    synonym:

@
 type 'Producer'' b m r = forall x' x . 'Proxy' x' x () b m r
 type 'Pipe'    a b m r =               'Proxy' () a () b m r
@

    The same is true for 'await', which uses the polymorphic 'Consumer'' type
    synonym:

@
 'await' :: 'Monad' m => 'Consumer'' a m a
@

    We can use 'await' within a 'Pipe' because a 'Pipe' is a special case of the
    polymorphic 'Consumer'' type synonym:

@
 type 'Consumer'' a   m r = forall y' y . 'Proxy' () a y' y m r
 type 'Pipe'      a b m r =               'Proxy' () a () b m r
@

    However, polymorphic type synonyms cause problems in many other cases:

    * They usually give the wrong behavior when used as the argument of a
      function (known as the \"negative\" or \"contravariant\" position) like
      this:

> f :: Producer' a m r -> ...  -- Wrong
>
> f :: Producer  a m r -> ...  -- Right

      The former function only accepts polymorphic 'Producer's as arguments.
      The latter function accepts both polymorphic and concrete 'Producer's,
      which is probably what you want.

    * Even when you desire a polymorphic argument, this induces a higher-ranked
      type, because it translates to a @forall@ which you cannot factor out to
      the top-level to simplify the type signature:

> f :: (forall x' x y' . Proxy x' x y' m r) -> ...

      These kinds of type signatures require the @RankNTypes@ extension.

    * Even when you have polymorphic type synonyms as the result of a function
      (i.e.  the \"positive\" or \"covariant\" position), recent versions of
      @ghc@ such still require the @RankNTypes@ extension.  For example, the
      'Pipes.Prelude.fromHandle' function from "Pipes.Prelude" requires
      @RankNTypes@ to compile correctly on @ghc-7.6.3@:

> fromHandle :: MonadIO m => Handle -> Producer' String m ()

    * You can't use polymorphic type synonyms inside other type constructors
      without the @ImpredicativeTypes@ extension:

> io :: IO (Producer' a m r)  -- Type error without ImpredicativeTypes

    * You can't partially apply polymorphic type synonyms:

> stack :: MaybeT (Producer' a m) r  -- Type error

    In these scenarios you should fall back on the concrete type synonyms, which
    are better behaved.  If concrete type synonyms are unsatisfactory, then ask
    @ghc@ to infer the most general type signature and use that.

    For the purposes of debugging type errors you can just remember that:

>  Input --+    +-- Output
>          |    |
>          v    v
> Proxy a' a b' b m r
>       ^    ^
>       |    |
>       +----+-- Ignore these

    For example, let's say that you try to run the 'P.stdinLn' 'Producer'.  This
    produces the following type error:

>>> runEffect P.stdinLn
<interactive>:4:5:
    Couldn't match expected type `X' with actual type `String'
    Expected type: Effect m0 r0
      Actual type: Proxy X () () String IO ()
    In the first argument of `runEffect', namely `P.stdinLn'
    In the expression: runEffect P.stdinLn

    'runEffect' expects an 'Effect', which is equivalent to the following type:

> Effect          IO () = Proxy X () () X      IO ()

    ... but 'P.stdinLn' type-checks as a 'Producer', which has the following
    type:

> Producer String IO () = Proxy X () () String IO ()

    The fourth type variable (the output) does not match.  For an 'Effect' this
    type variable should be closed (i.e. 'X'), but 'P.stdinLn' has a 'String'
    output, thus the type error:

>    Couldn't match expected type `X' with actual type `String'

    Any time you get type errors like these you can work through them by
    expanding out the type synonyms and seeing which type variables do not
    match.

    You may also consult this table of type synonyms to more easily compare
    them:

> type Effect             = Proxy X  () () X
> type Producer         b = Proxy X  () () b
> type Consumer    a      = Proxy () a  () X
> type Pipe        a    b = Proxy () a  () b
>
> type Server        b' b = Proxy X  () b' b 
> type Client   a' a      = Proxy a' a  () X
>
> type Effect'            m r = forall x' x y' y . Proxy x' x y' y m r
> type Producer'        b m r = forall x' x      . Proxy x' x () b m r
> type Consumer'   a      m r = forall      y' y . Proxy () a y' y m r
>
> type Server'       b' b m r = forall x' x      . Proxy x' x b' b m r
> type Client'  a' a      m r = forall      y' y . Proxy a' a y' y m r

-}

{- $timecomplexity
    There are three functions that give quadratic time complexity when used in
    within @pipes@:

    * 'sequence'

    * 'replicateM'

    * 'mapM'

    For example, the time complexity of this code segment scales quadratically
    with `n`:

> import Control.Monad (replicateM)
> import Pipes
>
> quadratic :: Int -> Consumer a m [a]
> quadratic n = replicateM n await

    These three functions are generally bad practice to use, because all three
    of them correspond to \"ListT done wrong\", building a list in memory
    instead of streaming results.

    However, sometimes situations arise where one deliberately intends to build
    a list in memory.  The solution is to use the \"codensity transformation\"
    to transform the code to run with linear time complexity.  This involves:

    * wrapping the code in the @Codensity@ monad transformer (from
      @Control.Monad.Codensity@ module of the @kan-extensions@ package) using
      'lift'

    * applying 'sequence' \/ 'replicateM' \/ 'mapM'

    * unwrapping the code using @lowerCodensity@

    To illustrate this, we'd transform the above example to:

> import Control.Monad.Codensity (lowerCodensity)
> 
> linear :: Monad m => Int -> Consumer a m [a]
> linear n = lowerCodensity $ replicateM n $ lift await

    This will produce the exact same result, but in linear time.
-}
