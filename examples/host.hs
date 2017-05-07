{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

{-|

This program gives an example of how to construct a simple framework that
allows programmers to write FRP applications that process keystrokes (of
type Char) to produce a view (of type String).  This model, where programmers
process Event(s) to produce Behavior(s) is akin to the OpenGL/GLUT model, where
input is pushed into the application via callbacks and and output is pulled
via periodic sampling in a render function.  Other application models work
differently; for example, DOM applications built with reflex-dom ultimately
produce Events rather than Behaviors as their output, since changes need to be
pushed (rather than pulled) into the web browser's DOM hierarchy.  The precise
mix of input and output structures required will depend on the needs of the
external systems with which the FRP-enabled program is interacting.

-}

module Main where

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Dependent.Sum (DSum ((:=>)))
import System.IO (hSetEcho, hSetBuffering, stdin, BufferMode (NoBuffering))

-- | Define the type for apps using our host framework.  Programmers
--   will write programs of type @TypingApp t m@ and use our
--   framework to run them.
--
--   In this framework, the user will write programs that take an input
--   event representing keystrokes and produce an output behavior representing
--   the current view to be shown.  This is similar to how polling-driven
--   output frameworks such as OpenGL will work.
type TypingApp t m = (Reflex t, MonadHold t m, MonadFix m)
                  => Event t Char
                  -> m (Behavior t String)

-- | Run a program written in the framework.  This will do all the necessary
--   work to integrate the Reflex-based guest program with the outside world
--   via IO.
host :: (forall t m. TypingApp t m)
        -- ^ By keeping t and m abstract, we ensure that the user (the
        --   programmer using our framework) can't make any assumptions
        --   about which Reflex implementation is being used
     -> IO ()
host myGuest =

  -- Use the Spider implementation of Reflex.
  runSpiderHost $ do

    -- Create an event to be used as input.
    -- It will fire wehenver we use eTriggerRef.
    (e, eTriggerRef) <- newEventWithTriggerRef

    -- Evaluate our user's program to set up the data flow graph.
    -- This usually only needs to be done once; the user can change the data
    -- flow graph arbitrarily in response to events.
    --
    -- runHostFrame is an efficient way of running a computation that
    -- can build arbitrary data flow graphs using 'hold' and 'sample'.
    --
    -- (The pure combinators in the Reflex class can be used in any context,
    -- so they don't need any special treatment - but inside runHostFrame is
    -- as good a place as any to run them.)
    b <- runHostFrame $ myGuest e

    -- Begin our event processing loop.
    forever $ do

      -- Get an input event and display it.
      input <- liftIO getChar
      liftIO $ putStrLn $ "Input Event: " ++ show input

      -- Retrieve the current event trigger.
      mETrigger <- liftIO $ readIORef eTriggerRef

      -- Use the trigger to deliver the event.
      case mETrigger of
        Nothing ->
          -- This means that nobody is subscribed to the input event.
          --
          -- Since this is the only input event in this system, that would
          -- mean the guest program must be really boring!  However, in larger
          -- programs, there are often many input events, and most programs
          -- will not care about every single one of them.
          --
          -- Note: The missing trigger does NOT mean we should buffer the
          -- input and deliver it later - it means that nobody is interested
          -- in this occurrence, so we should discard it.
          return ()
        Just eTrigger ->
          -- We have a trigger, so someone is interested in this input event
          -- occurrence.
          --
          -- fireEvents will process an event frame to deliver the event to
          -- anyone in the data flow graph who is interested in it.  It can
          -- also deliver multiple simultaneous events if necessary.  However,
          -- the same event cannot be firing multiple times simultaneously;
          -- system behavior is undefined if the same trigger is provided more
          -- than once.
          fireEvents [eTrigger :=> Identity input]

      -- Retrieve the current output of the user's program and display it.
      output <- runHostFrame $ sample b
      liftIO $ putStrLn $ "Output Behavior: " ++ show output

-- | This is a simple guest program written with our framework.  It just
--   accumulates all the characters that the user has typed.
--   Backspace functionality is left as an exercise for the reader.
guest :: TypingApp t m
guest e = do

  -- Accumulate the input events in a list.
  -- Each one represents a keypress from the end user.
  d <- foldDyn (:) [] e

  -- Since we're using cons to accumulate keystrokes, they will end up in
  -- reverse order.  Use `reverse` to fix that.
  return $ fmap reverse $ current d

-- | Main is just doing some setup so that the program's output will look nice,
--   and then invoking `host`.
main :: IO ()
main = do
  putStrLn "Welcome to the example Reflex host app; press Ctrl+C to exit"
  putStrLn "Press any key to process it with the Reflex FRP engine"

  -- Prevent the user's input from showing up until we want it to.
  hSetEcho stdin False

  -- Ensure that we process each character right away, instead of waiting
  -- until the user presses enter.
  hSetBuffering stdin NoBuffering

  -- Run the guest program using our host framework.
  host guest
