{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} -- TODO Remove this

module Emoji (insertEmojiAt) where

type Parser a = String -> [(String, a)]

runParser :: Parser a -> String -> [a]
runParser x s = map snd (x s)

separate :: (String -> Maybe (a, String)) -> Parser a
separate f s = case f s of
                  Nothing -> []
                  Just (v,s') -> [(s',v)]

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (_:_) [] = Nothing
stripPrefix (x:xs) (y:ys) 
  | x == y = stripPrefix xs ys
  | otherwise = Nothing

string :: String -> Parser String
string s = separate (\t -> fmap ((,) s) (stripPrefix s t))

many :: Parser a -> Parser [a]
many p s = some p s ++ [(s,[])]

some :: Parser a -> Parser [a]
some p s = do (s',v) <- p s; (s'',vs) <- many p s'; return (s'',v:vs)

parseMessage :: Parser [[String]]
parseMessage s = emptyMessage ++ nonemptyMessage
  where
    emptyMessage = if null s then [(s,[])] else []
    nonemptyMessage = do
      (s',l) <- many (string ".") s
      return (s',[l])

insertEmojiAt :: String        -- ^ current string
              -> (Int, String) -- ^ new location of caret, new string
insertEmojiAt str =
  -- We first parse the message.
  case runParser parseMessage str of
    (((_:ss) : _) : _) ->
      let newMessage = ".." ++ concat ss
          m = length newMessage
       in (m, newMessage)
    _ -> (1, ".")
