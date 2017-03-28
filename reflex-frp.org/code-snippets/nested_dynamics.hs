{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Reflex.Dom
import Data.Text as T
import Data.Monoid

-- A slightly contrived example just to demonstrate use of nested dynamics
-- This example also has a nested state machine,
-- By using foldDynM, we could use foldDyn inside of it.
--
-- A ScoreCard can either display some info/updates or the current score
data ScoreCard t =
    Info (Dynamic t Text)
  | Score (Dynamic t Int)

data GameEvent =
    NewGame
  | DoTurn Text Int -- Some info and points earned

main = mainWidget $ do

  newGameEv <- button "New Game"

  toggleCardEv <- button "Toggle Display"

  m1 <- button "Do move 1"
  m2 <- button "Do move 2"
  m3 <- button "Do move 3"
  m4 <- button "Do move 4"

  let
      -- gameEv :: (Reflex t) => Event t GameEvent
      gameEv = leftmost [newGame, move1, move2, move3, move4]

      newGame = const NewGame <$> newGameEv
      move1 = const (DoTurn "Move 1" 1) <$> m1
      move2 = const (DoTurn "Move 2" 20) <$> m2
      move3 = const (DoTurn "Move 3" 10) <$> m3
      move4 = const (DoTurn "Move 4" 5) <$> m4

  -- Capture the score, in a Dynamic independent of ScoreCard
  -- This will maintain its value irrespective of the state of ScoreCard
  scoreDyn <- do
    let handleGameEvent (NewGame) _ = 0
        handleGameEvent (DoTurn _ s) p = p + s

    foldDyn handleGameEvent 0 gameEv

  let 
      initCard = Score scoreDyn
      
      eventHandler _ (Info _) = return (Score scoreDyn)
      eventHandler _ (Score _) = do
        let handleGameEvent (NewGame)    _  = "New Game!"
            handleGameEvent (DoTurn t _) "" = "You did " <> t
            handleGameEvent (DoTurn t _) p = p <> ", " <> t

        -- Internal state machine using foldDyn
        -- Capture the info text in a Dynamic which is in the scope
        -- of ScoreCard.
        -- So this will be reset whenever you toggle the display of score card
        textDyn <- foldDyn handleGameEvent "" gameEv
        return (Info textDyn)
            
  -- external state machine using foldDynM
  -- Here the (ScoreCard t) itself contains a Dynamic value
  -- scoreCardDyn :: Dynamic t (ScoreCard t)
  scoreCardDyn <- foldDynM eventHandler initCard toggleCardEv

  dyn (renderScoreCard <$> scoreCardDyn)

  return ()

renderScoreCard sc =
  el "div" $
    case sc of
      (Info t) -> text "Info: " >> dynText t
      (Score s) -> text "Score: " >> dynText (T.pack <$> show <$> s)
