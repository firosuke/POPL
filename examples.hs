module Module where

-- You can load this in Haskell.
-- There are some commands you can type in below, namely:

-- For question 7, sheet 4 -- done with the non-continuation version of the monad:
-- test0, test0_strict
-- test1
-- test2
-- test3

-- An encoding of a "guessing game", a user (Cindy), and her interactions
-- both with the guessing game, and a broken variant of the game
-- guessingGame
-- out1
-- out2

--- ... and then the same examples, done with continuations





-- We'll define our own typeclass for monads: "MyMonad".

-- this is because if we try to do it the (modern) Haskell way:
--   instance Monad m where ...
-- we'll discover we ALSO need to make m an instance
-- of the Functor and Applicative typeclasses.

-- I am lazy so I'll just do this instead:

class MyMonad m where
  result :: a -> m a 
  ($>)   :: m a -> (a -> m b) -> m b

-- I'll also use sequencing, which we can think of as semicolon ; .
-- (ma ; mb) means "run ma, then throw away the value it produces and run mb"
-- The corresponding monadic operator in Haskell is called >>.
-- To avoid clashing with Haskell syntax, I'm using unicode ; which looks
-- exactly the same as normal semicolon (but it's a "Greek semicolon", apparently.) 

(;) :: (MyMonad m) => m a -> m b -> m b
infixr 1 ;
ma ; mb = ma $> (\_ -> mb)

-- we use integers for input/output, and strings for "final results"
-- returned by continuations.

type Value = Int
type Answer = String

-- Let's define "commands", or more accurately we can think of them as 
-- *sequences* of commands, or protocols.

data Command a = Done a | Out Value (Command a) | In (Value -> Command a)

-- Note that (Command a) consists of input/output of type Value, and
-- returns (Done a) things of type a.

-- A slightly silly function for showing commands. When the command is a request
-- for input, we'll only show the results of inputting 0 and 1 (and "..." for the rest).
-- We'll also only go to depth 7 (so that test1 is displayed fully).
-- Below, show' has an extra parameter n, recording how deep we should display
-- the syntax tree of in/out/done interactions

instance (Show a) => Show (Command a) where
  show cmd = show' 7 cmd  
             where show' 0 _ = "..."
                   show' _ (Done a) = "Done " ++ show a
                   show' n (Out v cmd) = "Out " ++ show v ++ "; " ++ show' (n-1) cmd
                   show' n (In f) = "In <(0 -> " ++ show' (n-1) (f 0) ++ "), (1 -> " ++ show' (n-1) (f 1) ++ "), ... >"

-- We can think of (Out v xm) as representing an output action, showing value v,
-- where xm is the Command (or part of the protocol) that comes next. 
-- Similarly, (In f) is a request for input;
-- the idea is that if the user inputs a value v, then we proceed with the Command
-- given by (f v). When such a protocol finishes, we should see a command "Done a".

-- Command is a monad.

instance MyMonad Command where
  result a  = Done a
  xm $> f  = case xm of
                Done a     -> f a                      -- 1
                Out v xm'  -> Out v (xm' $> f)         -- 2
                In g       -> In (\v -> (g v) $> f)    -- 3

-- Some rough explanation:
-- To "sequence" the command(s) xm with f, we preserve xm's output actions (line 2), ensuring
-- that whatever happens after the output (here called xm') is also "followed by" f, i.e. (xm' $> f) ;
-- similarly, we preserve xm 's input-requests (line 3) but ensure that whatever happens after
-- any input v (i.e. (g v)), is also followed by f, i.e. ((g v) $> f); 
-- lastly if xm is done (line 1) with result a, we just "proceed" with the computation (f a).

-- We'll imitate answers to question 7 with respect to this monad.
-- However we won't be doing this in an interpreter -- we'll just produce a command
-- (Done "Nil"), rather than "returning Nil in the interpreter".
-- So we won't make a distinction between a program that returns 42, and a computation that produces "Done 42".

-- 7a 

disable :: a -> Command a -> Command a
disable y cmd = case cmd of
                  Done a    -> Done a
                  Out v _   -> Done y
                  In g      -> In (\v -> disable y (g v)) 

-- 7b

limit :: Int -> a -> Command a -> Command a
limit 0 y cmd = disable y cmd
limit n y cmd = case cmd of
                  Done a    -> Done a
                  Out v cmd' -> Out v (limit (n-1) y cmd')
                  In g      -> In (\v -> limit n y (g v))

-- for convenience, we'll have a limit function that returns "Nil".

limit' :: Int -> Command String -> Command String
limit' n = limit n "Nil"

-- 7c
-- What would happen if the interpreter was strict?
-- Let's have a strict version of upfrom. 

-- I won't implement strict versions of limit and disable, as it doesn't affect 
-- the result here. I'll leave that to you. 
-- (You could write (f $! x) instead of (f x) when you want to force evaluation of x.
-- Or look up "seq".)

upfrom :: Int -> Command a
upfrom n = Out n (upfrom (n+1))

upfrom_strict :: Int -> Command a
upfrom_strict n = (Out $! n) $! (upfrom_strict $! (n+1))

-- You can see the effect already, by running test0 and test0_strict. 

-- TRY ME
test0 :: Command String
test0 = limit' 3 (upfrom 1)

-- TRY ME  Be ready to press Ctrl-C.
test0_strict :: Command String
test0_strict = limit' 3 (upfrom_strict 1)

-- 7d
-- use a helper function to accumulate the outputted values.

collect :: Command a -> Command [Value]
collect = collect' [] 

collect' :: [Value] -> Command a -> Command [Value]
collect' vs cmd = case cmd of
                  Done a    -> Done vs 
                  Out v cmd' -> collect' (vs ++ [v]) cmd'
                  In g      -> In (\v -> collect' vs (g v))

-- A more efficient option would be to write (v:vs) rather than (vs ++ [v]), and
-- then replace (Done vs) with (Done (reverse vs)).


-- 7e

-- TRY ME
test1 :: Command String
test1 = limit' 6 (limit' 3 (upfrom 1) ; upfrom 5)

-- outputs 1,2,3,5,6,7 then returns "Nil".

output :: Int -> Command String
output n = Out n (Done "Nil")

-- TRY ME
test2 :: Command String
test2 = limit' 1 (Done "Nil") ; output 1 ; output 2

-- outputs 1 and 2, then returns "Nil".

-- TRY ME
test3 :: Command [Value]
test3 = collect (limit' 3 (upfrom 10))

-- Note that test3 returns [10,11,12].


-- A bonus example for you: a simple guessing game.

-- We can only output Ints, sadly, so read:
-- Out 0 == Please guess a number
-- Out 1 == Too high
-- Out -1 == Too low
-- But Commands can return a String -- so we'll make it a Command String

-- TRY ME
guessingGame :: Command String
guessingGame = Out 0 (In handleGuess)

handleGuess :: Value -> Command String
handleGuess 1 = Done "You win!"
handleGuess n | n < 1   = Out (-1) guessingGame
              | otherwise = Out 1 guessingGame


-- To anticipate the introduction of continuations below, as users that interact with the
-- system, let's have a player who interacts with commands. Her name is Clever Cindy.

-- she starts with the value 3 in mind. We'll record and finally show the string of
-- everything she says. These are the parameters to cindy'.

cindy :: Command String -> String
cindy cmd = cindy' 3 "Hello. " cmd

cindy' :: Value -> String -> Command String -> String
-- Given her state (value and spoken string) and the next command, she interacts with it
-- as follows, to produce the final string.

-- Whenever she sees Output 0 (meaning "Please guess a number"), she says bring it on!
cindy' n spoken (Out 0 cmd) = cindy' n (spoken ++ "Bring it on! ") cmd
-- When she's asked for input, she inputs the number she has in mind (and keeps it).
cindy' n spoken (In g) = cindy' n (spoken ++ "I'll put in " ++ show n ++ ". ") (g n)
-- If she sees "Too high" (Output 1), she reduces her number.
cindy' n spoken (Out 1 cmd) = cindy' (n-1) (spoken ++ "Too high. ") cmd
-- Similarly, if too low.
cindy' n spoken (Out (-1) cmd) = cindy' (n+1) (spoken ++ "Too low. ") cmd
-- Let's handle the Done case.
cindy' _ spoken (Done text) = spoken ++ (if text == "You win!" then "Yay, I won!" 
                                         else "It says " ++ text ++ ", apparently. I'm done for the day.")
-- Finally, if any other number is output, she says it out loud and carries on playing, unless it's 666 in which case she immediately runs away and ignores
-- the next command (cmd). (But we still need to return her spoken output.)
cindy' n spoken (Out m cmd) | m == 666  = spoken ++ "Aargh, now it's the evil number 666! I'm out of here!"
                            | otherwise = cindy' n (spoken ++ "It output " ++ show m ++ "? I don't know what that means. ") cmd


-- Let's ask her to play the game. Let's also ask her to interact with another series of commands.

-- TRY ME
out1 = cindy guessingGame

-- Now we'll ask her to guess a number, twice, and then if the input is > 2, it says "too high", otherwise
-- it outputs 42 and then 666 (and then 5, although Cindy has run away by this point, so she doesn't see it.)

-- TRY ME
out2 = cindy (output 0 ; output 0 ; output 0 ; In f)
         where f = \n -> if n > 2 then (output 1 ; In f) else (output 42 ; output 666 ; output 5 )

-- (Pointless exercise: extend Cindy so that she remembers how many times she's seem (Out 0 ...) in a row.
--  if she sees it more than twice in a row, make her say "This is boring, I'm off.")




-- CONTINUATIONS


-- Now let's do a continuation version of "commands", or protocols. M is defined shortly.

data Command2 a = Done2 a | Out2 Value (M a) | In2 (Value -> (M a))

-- (We need to use different names Done2, etc, so the constructors don't
-- clash with the above.) 

-- (Exercise for later: try replacing "M a" with "Command2 a", so it's just the same as
-- the previous example. Can you still define $> for the monad M below? Good luck.)

-- Informally, Command2 works a similar way to Command; but we replaced "Command a" with "M a"
-- in the Out2 and In2 constructors.

-- Those parts still represent "whatever happens next" -- e.g. (Out2 v xm) still means
-- we output value v, followed by the computation xm (of type M a).

-- In more detail, we can still think of Command2 as a kind of "protocol"; but now we distinguish
-- those from "computations" of type (M a). These are functions, which accept a continuation
-- as input, before we get an answer.

type Continuation a = (Command2 a) -> Answer

-- I would have liked to write this:

-- type M a = Continuation a -> Answer

-- ... but we run into Haskell trouble later, trying to make this a monad. Instead, we have to do something like this:

newtype M a = Wrap {unwrap :: Continuation a -> Answer}

-- This is record syntax: we can "Wrap" a function (Continuation a -> Answer) into a value of type M a. 
-- Conversely, given such a value, we can "unwrap" it into the corresponding function. It seems very ugly to me, 
-- but I can't find a cleaner way to write it (and what's below). Suggestions welcome!

-- as mentioned in the classes, we can think of a continuation k as a "consumer" or "user"
-- of Command2 values (i.e. protocols). To "run" a computation xm, we have to give it a continuation
-- k, saying how we will respond to in/out/done commands (implicitly produced by the computation xm).
-- Then when we evaluate (xm k), this "user" k interacts with the computation xm, and finally produces an answer.

-- (Whereas the previous monad didn't feature any "interaction" with the commands, unless you wrote
-- a separate function of some type (Command a) -> b, like cindy above.)

-- This M is also a monad!
-- Following the simpler version of M above, I wanted to write something like this:
{-
instance MyMonad M where
  result a  k  = k (Done2 a)                    
  (xm $> f) k  = xm k'
                 where k' cmd = case cmd of       
                                  Done2 a    -> f a k                       
                                  Out2 v xm' -> k (Out2 v (xm' $> f))        
                                  In2 g      -> k (In2 (\v -> (g v) $> f))                            
-}

-- ... but Haskell complains about the use of "type" M, as well as the parameter k on the left.
-- Then it complains about the "where" clause.
-- Finally, I give up and write this:
-- (Essentially, "Wrap $ \k -> stuff" is the same as "Wrap (\k -> stuff)" )

instance MyMonad M where
  result a  = Wrap $ \k -> k (Done2 a)                                               -- 1
  xm $> f   = Wrap $ \k -> let k' cmd = case cmd of                                  -- 2
                                            Done2 a    -> (unwrap (f a)) k           -- 3
                                            Out2 v xm' -> k (Out2 v (xm' $> f))      -- 4
                                            In2 g      -> k (In2 (\v -> (g v) $> f)) -- 5
                                    in (unwrap xm) k'                                -- 6

-- (This is uglier in Haskell than I'd hoped. Firstly we have to "Wrap" the functions into monadic values; secondly
-- we have to put the k in lambdas; thirdly, when you want to apply a computation xm to a continuation k, you have to 
-- first "unwrap" xm into the function. E.g. (unwrap xm) k, rather than (xm k).
-- For exam purposes, I don't think you have to worry about these details.
-- You can do something like the simpler version in {- -} before it.)

-- Line 1 defines the simple computation "result a". When it is supplied a user "k",
-- it simply tells the user, "we're Done, with value a", by passing the Command2
-- "Done a" directly to the user k. They can do whatever they want with the value "a", but 
-- they should produce an Answer (according to the types.)

-- Lines 2-6 show how to "join together" a computation xm with a function f of its ouptut, giving 
-- a new computation (xm $> f).
-- How should this computation (xm $> f) interact with a user k? 
-- We do that in line 6, by "running" the first stage of the protocol (xm), with a "new user", k'.
-- If you imagine k as a person who responds to in/out/done commands, you can think of k' as a
-- "person in the middle", that accepts xm 's protocol commands (cmd), but then tweaks them before
-- communicating them on to the original user. Their job is to ensure that whatever happens, we "run"
-- the function f after the first stage of computation xm is finished.

-- This "new user"/person-in-the-middle responds to commands (cmd) produced by the computation xm, as defined in lines 2-5.
-- We have to say what Answer is produced in response to each possible Command2.
-- Line 3: If the computation xm signals the command "Done a", then the new user essentially steps aside,
-- and we run the next stage of computation (f a) with the original user, k.
-- Line 4: If the computation xm signals (Out v xm') -- or "output v (and then proceed with computation xm')" --
-- Then the new user passes that output to the old user k. However, they tweak it: instead of saying "(...proceed with
-- computation xm')", they say "proceed with computation (xm' $> f)", so that we remember to pass the result of xm' to f.
-- Line 5 is similar: the new user passes the input-request (In g) to the old user k, but they make sure that whatever
-- input v is supplied by the old user, the next step of computation is not just (g v), but ((g v) $> f).


-- Let's redo the previous examples, but in this monad.

-- We'll use a continuation, "showC", which acts like the implementation of "show" above.
-- We'll use this to show values of type M a.

instance (Show a) => Show (M a) where
         show xm = (unwrap xm) showC

showC :: (Show a) => Continuation a   --i.e. Command2 a -> Answer
showC = showC' 7 

-- as for the previous monad, showC' remembers the depth we're going to display
-- the difference is small: essentially we replaced (show' (n-1) cmd) with
-- (unwrap xm') (showC' (n-1)), passing (showC' (n-1)) as a continuation to xm'

showC' :: (Show a) => Int -> Continuation a
showC' 0 cmd2 = "..."
showC' _ (Done2 a) = "Done " ++ show a
showC' n (Out2 v xm') = "Out " ++ show v ++ "; " ++ (unwrap xm') (showC' (n-1))
showC' n (In2 f) = "In <(0 -> " ++ (unwrap (f 0)) (showC' (n-1)) ++ "), (1 -> " ++ (unwrap (f 1)) (showC' (n-1)) ++ "), ... >"


-- We'll imitate answers to question 7 with respect to this monad... 
-- 7a 

disableC :: a -> M a -> M a
disableC y xm = Wrap $ \k -> let k' cmd = case cmd of
                                            Done2 a    -> k (Done2 a)
                                            Out2 v _   -> k (Done2 y)
                                            In2 g      -> k (In2 (\v -> disableC y (g v)))
                             in (unwrap xm) k'
                             
-- I'm not convinced this is fully strict, but I'll leave checking it
-- for future work...
disable_strictC :: a -> M a -> M a
disable_strictC y xm = Wrap $ \k -> let k' cmd = case cmd of
                                            Done2 a    -> k $! (Done2 a)
                                            Out2 v _   -> k $! (Done2 y)
                                            In2 g      -> k $! (In2 $! (\v -> disable_strictC y (g v)))
                             in (unwrap xm) $! k'



-- 7b

limitC :: Int -> a -> M a -> M a
limitC 0 y xm = Wrap $ \k -> unwrap (disableC y xm) k
limitC n y xm = Wrap $ \k -> let k' cmd = case cmd of
                                            Done2 a    -> k (Done2 a)
                                            Out2 v xm' -> k (Out2 v (limitC (n-1) y xm'))
                                            In2 g      -> k (In2 (\v -> limitC n y (g v)))
                             in (unwrap xm) k'

limitC' :: Int -> M String -> M String
limitC' n = limitC n "Nil"


-- I'm not convinced this is fully strict, but I'll leave checking it
-- for future work...
limit_strictC :: Int -> a -> M a -> M a
limit_strictC 0 y xm = Wrap $! \k -> (unwrap (disable_strictC y xm)) $! k
limit_strictC n y xm = Wrap $! \k -> let k' cmd = case cmd of
                                            Done2 a    -> k $! (Done2 a)
                                            Out2 v xm' -> k $! ((Out2 v) $! (limit_strictC (n-1) y xm'))
                                            In2 g      -> k $! (In2 (\v -> limit_strictC n y (g v)))
                             in (unwrap xm) $! k'

limit_strictC' :: Int -> M String -> M String
limit_strictC' n = limit_strictC n "Nil"

-- 7c
-- What would happen if the interpreter was strict?
-- Note, this gives different results from the previous monad.
-- Informally, it's because we only unwind an infinite computation
-- like upfromC by passing its commands to the continuation, one at a time;
-- the continuation can stop before examining all the commands.

-- TRY upfromC 0
upfromC :: Int -> M a
upfromC n = Wrap $ \k -> k (Out2 n (upfromC (n+1))) 

-- TRY upfrom_strictC 0  -- note that this doesn't hang,
-- as the "showC" continuation doesn't try to inspect the
-- entire sequence of commands
upfrom_strictC :: Int -> M a
upfrom_strictC n = Wrap $! \k -> k ((Out2 n) $! (upfrom_strictC $! (n+1)))

-- You can see the effect by running upfromC, upfrom_strictC,
-- and also test0 and test0_strict. 

-- TRY ME
test0C :: M String
test0C = limitC' 3 (upfromC 1)

-- TRY ME
test0C_strict :: M String
test0C_strict = limit_strictC' 3 (upfrom_strictC 1)

-- 7d
-- use a helper function to accumulate the outputted values.

collectC :: M a -> M [Value]
collectC = collectC' [] 

collectC' :: [Value] -> M a -> M [Value]
collectC' vs xm = Wrap $ \k -> let k' cmd = case cmd of
                                              Done2 a    -> k (Done2 vs)
                                              Out2 v xm' -> (unwrap (collectC' (vs ++ [v]) xm')) k
                                              In2 g      -> k (In2 (\v -> collectC' vs (g v)))
                               in (unwrap xm) k'

{- Simplified version:

collectC' vs xm k = let k' cmd = case cmd of
                                              Done2 a    -> k (Done2 vs)
                                              Out2 v xm' -> (collectC' (vs ++ [v]) xm')) k
                                              In2 g      -> k (In2 (\v -> collectC' vs (g v)))
                               in xm k'

-}

-- A more efficient option would be to write (v:vs) rather than (vs ++ [v]), and
-- then replace (Done vs) with (Done (reverse vs)).

-- 7e

-- TRY ME
test1C :: M String
test1C = limitC' 6 (limitC' 3 (upfromC 1) ; upfromC 5)

-- outputs 1,2,3,5,6,7 then "returns Nil" (here, Done -1).

outputC :: Int -> M String
outputC n = Wrap $ \k -> k (Out2 n (result "Nil"))

-- we might as well also define:

inputC :: (Value -> M a) -> M a
inputC f = Wrap $ \k -> k (In2 f)

-- TRY ME
test2C :: M String
test2C = limitC' 1 (result "Nil") ; outputC 1 ; outputC 2

-- outputs 1 and 2, then "returns Nil" (Done -1).

-- TRY ME
test3C :: M [Value]
test3C = collectC (limitC' 3 (upfromC 10))

-- Note that test3 returns [10,11,12].


-- A bonus example for you: a simple guessing game.

-- We can only output Ints, sadly, so read:
-- Out 0 == Please guess a number
-- Out 1 == Too high
-- Out -1 == Too low
-- But Commands can return a String -- so we'll make it a Command String

-- TRY ME
guessingGameC :: M String
guessingGameC = Wrap $ \k -> k (Out2 0 (inputC handleGuessC))

handleGuessC :: Value -> M String
handleGuessC 1 = Wrap $ \k -> k(Done2 "You win!")
-- equivalently, handleGuessC 1 = result "You win!"
handleGuessC n | n < 1     = Wrap $ \k -> k (Out2 (-1) guessingGameC)
               | otherwise = Wrap $ \k -> k (Out2 1 guessingGameC)


cindyC :: Command2 String -> String
cindyC cmd2 = cindyC' 3 "Hello. " cmd2

cindyC' :: Value -> String -> Command2 String -> String
cindyC' n spoken (Out2 0 xm') = (unwrap xm') (cindyC' n (spoken ++ "Bring it on! "))
cindyC' n spoken (In2 g) = (unwrap (g n)) (cindyC' n (spoken ++ "I'll put in " ++ show n ++ ". "))
cindyC' n spoken (Out2 1 xm') = (unwrap xm') (cindyC' (n-1) (spoken ++ "Too high. "))
cindyC' n spoken (Out2 (-1) xm') = (unwrap xm') (cindyC' (n+1) (spoken ++ "Too low. "))
cindyC' _ spoken (Done2 text) = spoken ++ (if text == "You win!" then "Yay, I won!" 
                                         else "It says " ++ text ++ ", apparently. I'm done for the day.")
cindyC' n spoken (Out2 m xm') | m == 666  = spoken ++ "Aargh, now it's the evil number 666! I'm out of here!"
                              | otherwise = (unwrap xm') (cindyC' n (spoken ++ "It output " ++ show m ++ "? I don't know what that means. "))

-- Let's ask her to play the game. Let's also ask her to interact with another series of commands.

-- TRY ME
out1C = (unwrap guessingGameC) cindyC

-- TRY ME
brokenGame = (outputC 0 ; outputC 0 ; outputC 0 ; inputC f)
             where f = \n -> if n > 2 then (outputC 1 ; inputC f) else (outputC 42 ; outputC 666 ; outputC 5 )

out2C = (unwrap brokenGame) cindyC







