{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} -- TODO Remove this

module Emoji where

import Control.Applicative
import Control.Monad.State
import Data.Char
import qualified Data.Map as BaseMap
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Traversable (for)
import Network.URI hiding (scheme)
import Numeric

--- Parser ---

-- | A very simple type of parser combinators specialised to the case of segmenting Text values, and maintaining information about character positions
-- in them. We'd use Attoparsec or something here, but it doesn't keep track of character position so easily, and we need that.

-- TODO: Should this be in its own module?

newtype Parser a = P (StateT (Int,Text) [] a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

-- | Run a parser on a given piece of text, giving a list of the possible parsed results.
runParser :: Parser a -> Text -> [a]
runParser (P x) s = evalStateT x (0,s)

-- | Run a parser on a given piece of text, giving Nothing if the parse failed, and Just v where v is the first successfully parsed value otherwise.
runParserMaybe :: Parser a -> Text -> Maybe a
runParserMaybe p s = listToMaybe (runParser p s)

-- | Get the current character position in the text.
currentPos :: Parser Int
currentPos = P $ gets fst

-- | This parser fails if the given value is Nothing, and returns v if the given value is Just v.
guardMaybe :: Maybe a -> Parser a
guardMaybe m = case m of Nothing -> empty; Just v -> return v

-- | Given a means of separating a Text value into (Maybe) a value of type a, and a depleted Text value, give a Parser which does that, succeeding
-- uniquely with result v in the case that the function gives Just (v,s'), and failing otherwise.
separate :: (Text -> Maybe (a, Text)) -> Parser a
separate f = P $ do (n,s) <- get
                    case f s of
                      Nothing -> empty
                      Just (v,s') -> put (n + (T.length s - T.length s'), s') >> return v

-- | Attempt to match a fixed piece of text.
string :: Text -> Parser Text
string s = separate (\t -> fmap ((,) s) (T.stripPrefix s t))

-- | Given a predicate, match as many characters satisfying that predicate as possible. Efficient, as it uses Data.Text.span, but has no backtracking.
spanP :: (Char -> Bool) -> Parser Text
spanP p = separate (Just . T.span p)

-- | Match a single character which satisfies the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = separate (\t -> do (c,t') <- T.uncons t; guard (p c); return (c,t'))

-- | Match everything up to but not including the next occurrence of the given text. If that text doesn't occur, will match the
-- entire remainder of the input. Never fails.
breakOnP :: Text -> Parser Text
breakOnP s = separate (Just . T.breakOn s)

-- | This parser succeeds if and only if there is still input remaining.
notAtEnd :: Parser ()
notAtEnd = P $ do (_,s) <- get; guard (not (T.null s))

-- | This parser succeeds if and only if there is no input remaining.
atEnd :: Parser ()
atEnd = P $ do (_,s) <- get; guard (T.null s)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = sepBy1 p q <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = do v <- p
                vs <- many (q *> p)
                return (v:vs)

enclosedBy :: Parser a -> Parser b -> Parser a
enclosedBy p q = q *> p <* q

-- Like many, but prefer to obtain a smaller number of occurrences.
few :: Parser a -> Parser [a]
few p = return [] <|> few1 p

-- Like many1, but prefer to obtain a smaller number of occurrences.
few1 :: Parser a -> Parser [a]
few1 p = liftA2 (:) p (few p)

-- | A value of type Span consists of a pair of caret locations in a given piece of text. For example, in the string
-- "abcdef", the location of "ab" would be Span 0 2, while "cde" would be Span 2 5. Note how adjacent spans will be of
-- the form Span a b, Span b c.
data Span = Span !Int !Int
  deriving (Eq, Ord, Show, Read)

-- | Extract the start and end point of a given span.
spanStart, spanEnd :: Span -> Int
spanStart (Span a _) = a
spanEnd (Span _ b) = b

-- Converts a parser p which produces a function of a Span into one which gets the current position before and after parsing with p, and applies the function to the Span.
withSpan :: Parser (Span -> a) -> Parser a
withSpan p = do
  start <- currentPos
  f <- p
  end <- currentPos
  return (f (Span start end))

---

-- | This type represents the various formatting styles (bold, italic, strikethrough) a user can apply to text.
data TextFormatType = FTBold | FTItalic | FTStrike
  deriving (Eq, Ord, Show)

-- | This type represents a part of a message line. In the case of SPre, the content may actually contain newline characters,
-- but since it will be rendered as preformatted text, those don't need to be rendered in the HTML as <br> tags.
-- Every constructor here takes a Span as its last argument, indicating the caret locations at its beginning and end.
data Segment = SText Text Span -- ^ A simple piece of text.
             | SLink Text URI Span -- ^ A parsed link, with the text to display for it, and the URI.
             | SEmoji Text Span -- ^ A plain emoji, with its shortcode.
             | SSelectEmoji [(Int, Text)] Span -- ^ An emoji which may be selected, with a list of the shortcodes for the options,
                                                 -- along with their character positions in the text.
             | SSelect Span -- ^ A message mutual-exclusive radio option.
             | SCheckbox Span -- ^ A checkbox
             | SFormat TextFormatType [Segment] Span -- ^ A formatted portion of text, with the given formatting, and a list of the segments contained in it.
                                                     -- Note that formatting can't span across multiple lines.
             | SPre Text Span -- ^ A preformatted text block, to be rendered in a <pre> tag in the HTML.
             | SCode Text Span -- ^ A code segment, to be rendered in a <code> tag in the HTML
             | SMention Text Span -- ^ An @mention
  deriving (Eq, Ord, Show)

-- | Corresponding to the Segment type, we have a type Segment' which represents possible one-hole contexts in which we might want to insert something new into
-- a Segment. Most of the constructors of Segment are not recursive, and so don't need their own constructors here.
data Segment' = SHole'
                  -- ^ This case is represents just a hole in which to put the new thing.
                  -- This case comes up when we want to substitute one segment for another.
                  -- The segment to be edited is returned as part of focusSegmentPM, and there will be an SHole' in its place.
              | SSplit' Segment Segment
                  -- ^ This case represents a hole in between the two given segments. This arises for instance when the caret is in the middle
                  -- of some plain text, and we wish to split the text segment and insert into the middle of it.
              | SFormat' TextFormatType [Segment] Segment' [Segment]
                  -- ^ This case represents a hole in one of the segments inside a formatting segment.
                  -- It gives the formatting type, a list of segments before the one with the hole, the segment with the hole in it,
                  -- and then the segments after.
  deriving (Eq, Ord, Show)

-- | Segments are formed together into lines of text. Each line may be a blockquote, or an ordinary line.
data Line = LQuote [Segment] Span
              -- ^ A blockquote line.
          | LLine [Segment] Span
              -- ^ An ordinary line of text.
  deriving (Eq, Ord, Show)

-- | Correspondingly, we have a type for representing a line in which we have a hole to insert something new.
data Line' = LQuote' [Segment] Segment' [Segment]
              -- ^ A blockquote line with a hole in it consists of the segments before the hole,
              -- the segment with the hole, and the segments after the hole.
           | LLine' [Segment] Segment' [Segment]
              -- ^ Similarly, for an ordinary line.
  deriving (Eq, Ord, Show)

-- | A complete parsed message consists of a list of lines.
data ParsedMessage = ParsedMessage [Line] Span
  deriving (Eq, Ord, Show)

-- | A parsed message with a hole in it consists of the lines before the hole, a line with a hole in it, and the lines after.
data ParsedMessage' = ParsedMessage' [Line] Line' [Line]
  deriving (Eq, Ord, Show)

-- | This type represents modes that the user can be in while inserting an emoji.
data EmojiMode = EMPlain -- ^ Insert a plain emoji
               | EMPlusOne -- ^ Insert a +1 actionable emoji.
               | EMSelectOne -- ^ Insert or extend a mutually exclusive option between emojis to +1.
  deriving (Eq, Ord, Show)

--- Message parsing ---

emojiData = BaseMap.fromList [("blush", ())]

-- | Matches an emoji shortname.
emojiName :: Parser Text
emojiName =
  do t <- spanP (\x -> isAlpha x || isDigit x || x `elem` ['-','_'])
     maybe empty return $ BaseMap.lookup t emojiData
     return t

-- | Matches a link wrapped in parentheses
parseWrappedLink :: Parser (Span -> Segment)
parseWrappedLink = do
  string "("
  SLink t u s <- withSpan parseLink
  string ")"
  -- wrap the link summary with the parens that were consumed during parsing
  return . const $ SLink ("(" <> t <> ")") u s

-- | Matches a link.
parseLink :: Parser (Span -> Segment)
parseLink = parseLinkURI <|> parseLinkHeuristic

-- | Matches anything starting with "www.", turning it into a link.
parseLinkHeuristic :: Parser (Span -> Segment)
parseLinkHeuristic = do
  ws <- string "www."
  rs <- T.pack <$> many (satisfy $ not . isSpace)
  let x = ws <> rs
  u <- guardMaybe (parseURI (T.unpack ("http://" <> x)))
  return (SLink x u)

-- | Matches anything that parseURI from the Network.URI module recognises as a link.
parseLinkURI :: Parser (Span -> Segment)
parseLinkURI = do
  x <- spanP (not . isSpace)
  u <- guardMaybe (checkURI x)
  return $ SLink x u

checkURI :: Text -> Maybe URI
checkURI x = do
  u <- parseURI (T.unpack x)
  let scheme = uriScheme u
  guard (scheme `elem` ["http:","https:","ftp:","mailto:"])
  return u

parseSegment :: Parser Segment
parseSegment = parseSegmentExcluding Set.empty

-- | Match a Segment.
parseSegmentExcluding :: Set TextFormatType -> Parser Segment
parseSegmentExcluding exclusions = withSpan $
     do string "@"
        name <- spanP isAlphaNum
        return $ SMention name
 <|> do string ":O:"
        return SSelect
 <|> do string ":X:"
        return SCheckbox
 <|> do string "+:"
        ns <- (do n <- currentPos; s <- emojiName; return (n,s)) `sepBy` string ","
        string ":"
        return (SSelectEmoji ns)
 <|> do string ":"
        s <- emojiName
        string ":"
        return (SEmoji s)
 <|> withExclusion FTBold   (\e -> fmap (SFormat FTBold) (few1 (parseSegmentExcluding e) `enclosedBy` string "*"))
 <|> withExclusion FTItalic (\e -> fmap (SFormat FTItalic) (few1 (parseSegmentExcluding e) `enclosedBy` string "_"))
 <|> withExclusion FTStrike (\e -> fmap (SFormat FTStrike) (few1 (parseSegmentExcluding e) `enclosedBy` string "~"))
 <|> fmap SPre (breakOnP "```" `enclosedBy` string "```")
 <|> fmap SCode (breakOnP "`" `enclosedBy` string "`")
 <|> parseWrappedLink
 <|> parseLink
 -- only get exactly one here, because the rest may be the start of something valid
 <|> SText <$> fmap T.singleton (satisfy (\c -> c `elem` delims && not (isSpace c)))
 <|> do nonpunct <- spanP (\c -> c `notElem` delims && not (isSpace c))
        space <- spanP (\c -> isSpace c && c /= '\n')
        let t = T.append nonpunct space
        guard (not . T.null $ t)
        return (SText t)
  where delims :: [Char]
        delims = "+:*_~`"
        withExclusion :: TextFormatType -> (Set TextFormatType -> Parser a) -> Parser a
        withExclusion ft f = if Set.member ft exclusions then empty else f (Set.insert ft exclusions)

-- | Match zero or more segments
parseSegments :: Parser [Segment]
parseSegments = many parseSegment

-- | Match a blockquote line (without the trailing newline)
parseQuote :: Parser Line
parseQuote =
  withSpan $ do
    string ">"
    quote <- parseSegments
    return (LQuote quote)

-- | Match an ordinary line (without the trailing newline)
parseLine :: Parser Line
parseLine =
  withSpan $ do
    line <- parseSegments
    return (LLine line)

-- | Parse any message.
parseMessage :: Parser ParsedMessage
parseMessage = withSpan $ emptyMessage <|> nonemptyMessage
  where
    emptyMessage = ParsedMessage [] <$ atEnd
    nonemptyMessage = do
      ls <- (parseQuote <|> parseLine) `sepBy` string "\n"
      return (ParsedMessage ls)

--- Line editing ---

--TODO: come up with some more uniform way to encode all the decisions that this function deals with

-- | Insert an emoji into the given text, given the current mode, the emoji shortcode to be inserted, the location of the caret, and the current text.
-- Produces the new location of the caret along with the new text.
insertEmojiAt :: EmojiMode
              -> Text        -- ^ emoji to be inserted
              -> Int           -- ^ location of caret
              -> Text        -- ^ current string
              -> (Int, Text) -- ^ new location of caret, new string
insertEmojiAt mode e n str =
  -- We first parse the message.
  case runParser parseMessage str of
    [] -> (n, str) -- This should never happen because the message parser should never fail, but just in case it ever does, we don't change anything.
                   -- Should be a noticeable bug without being too disastrous.
    (pm : _) ->
      let -- We begin the editing by using focusSegmentPM to chop the ParsedMessage, obtaining the starting location of the cut, a Maybe Segment which
          -- represents a piece under the caret which may need modification, and a ParsedMessage' which is the parsed message with a hole in it.
          (k, sF, pm') = focusSegmentPM n pm
          -- A function used to modify the newly inserted segment, reinserting along with it whatever segment it was that we popped out with focusSegmentPM.
          catFocused = case sF of
            Nothing -> id -- If we didn't extract anything, no need to transform the thing we're inserting.
            Just (SLink t u s@(Span a _)) -- If we extracted a link...
              | n == a -> (++ [SText " " (Span 0 0), SLink t u s])
                  -- and the caret is at the start of the link, we reinsert an extra space and the link after the newly inserted segment.
              | otherwise -> ([SLink t u s, SText " " (Span 0 0)] ++) -- otherwise, put the link and a space before the newly inserted thing.
            Just x -> (x:) -- If we extracted anything else, we insert it before the new segment.

          -- The list of segments to be inserted into the hole left by focusSegmentPM, depending on the insertion mode.
          insertion = case mode of
            EMPlain -> catFocused [SEmoji e (Span 0 0)]
            EMPlusOne -> catFocused [SSelectEmoji [(0, e)] (Span 0 0)]
            EMSelectOne -> case sF of
              Just (SSelectEmoji es s) -> [SSelectEmoji (es ++ [(0,e)]) s] -- In this particular case, we replace the focused segment entirely.
              _ -> catFocused [SSelectEmoji [(0,e)] (Span 0 0)]

          -- We obtain a new ParsedMessage by applying unfocusSegmentPM to the insertion and the ParsedMessage', filling the hole,
          -- and then applying unparsePM to translate that back into plain text.
          newMessage = unparsePM (unfocusSegmentPM insertion pm')

          -- Here we compute the difference in length between the segment we removed and the segments we reinserted.
          l = sum (map (T.length . unparseS) insertion) -- length of newly inserted segment
            - sum (fmap (T.length . unparseS) sF) -- length of focused segment

          -- An additional adjustment to the caret might be required...
          adjustment = case sF of
            Just (SLink _ _ (Span a _)) | a == n -> -1 -- If we're at the start of a link, we insert an additional space after the emoji, and don't want to move the caret past it.
            _ -> 0

          -- The new position of the caret is the cut location, plus the difference in insertion length, plus the adjustment.
          newCaret = k + l + adjustment

       in (newCaret, newMessage)

-- | Given a caret location, and a ParsedMessage, cut the ParsedMessage, resulting in the location at which we've cut,
-- a Maybe Segment which is the segment underneath the caret that we may wish to replace,
-- and a ParsedMessage' representing the ParsedMessage with a hole in it where we cut it.
focusSegmentPM :: Int -- ^ Location at which to attempt to cut
               -> ParsedMessage
               -> ( Int -- Location of actual cut
                  , Maybe Segment -- Extracted segment, in cases where we will want to make a replacement
                  , ParsedMessage' -- Message with hole in it
                  )
focusSegmentPM _ (ParsedMessage [] (Span a _)) = (a, Nothing, ParsedMessage' [] (LLine' [] SHole' []) []) -- If there's nothing left of the message, give a hole on a new line.
focusSegmentPM n (ParsedMessage (l : ls') (Span _ v))
  | Span a b <- lineSpan l, (a <= n && n <= b) -- The caret is in the first line
  = let (k, sF, l') = focusSegmentL n l -- So focus in that line
     in (k, sF, ParsedMessage' [] l' ls')
  | Span _ b <- lineSpan l, (b < n) -- The caret is after the first line
  = let (k, sF, ParsedMessage' lsL l' lsR) = focusSegmentPM n (ParsedMessage ls' (Span b v)) -- So recurse on the message not including that line.
     in (k, sF, ParsedMessage' (l:lsL) l' lsR) -- and extend the result by putting the first line back into it.
-- Note that in the case where we have a blockquote marker at the start of a line "> ", the caret may be positioned strictly to the left of the first segment on the line.
-- In this case, we insert before it a new line on which to insert the item.
focusSegmentPM _ (ParsedMessage ls@(l@(LQuote {}) : _) _) -- NB: n < spanStart (lineSpan l)
  = (spanStart (lineSpan l), Nothing, ParsedMessage' [] (LLine' [] SHole' []) ls)
-- The following case should not ordinarily be possible, but it behaves sensibly and completes the pattern matching.
focusSegmentPM _ (ParsedMessage (l@(LLine ss _) : ls') _) -- NB: n < spanStart (lineSpan l)
  = (spanStart (lineSpan l), Nothing, ParsedMessage' [] (LLine' [] SHole' ss) ls')

-- | This does what focusSegmentPM does, but with respect to a single line.
focusSegmentL :: Int -> Line -> (Int, Maybe Segment, Line')
focusSegmentL n (LQuote (s:ss) _) = focusSegmentSegs LQuote LQuote' insertLeftLine' focusSegmentL n s ss
focusSegmentL n (LLine (s:ss) _) = focusSegmentSegs LLine LLine' insertLeftLine' focusSegmentL n s ss
focusSegmentL _ (LQuote [] (Span a _)) = (a, Nothing, LQuote' [] SHole' [])
focusSegmentL _ (LLine [] (Span a _)) = (a, Nothing, LLine' [] SHole' [])

-- The implementation of focusSegmentL and part of focusSegmentS, would otherwise be repetitive for LQuote vs. LLine vs. SFormat ft, so we abstract over the cases, using
-- functions which operate on lists of segments.
-- Note that the type parameter t will either be Line or Segment, and the type t' will be Line' or Segment' respectively.
focusSegmentSegs :: ([Segment] -> Span -> t) -- ^ the LQuote or LLine data constructor, or SFormat ft
                 -> ([Segment] -> Segment' -> [Segment] -> t') -- ^ the LQuote' or LLine' data constructor, or SFormat' ft
                 -> (Segment -> t' -> t') -- ^ How to insert a Segment at the beginning of the left portion of either a line with hole, or a Segment'
                 -> (Int -> t -> (Int, Maybe Segment, t')) -- ^ Either focusSegmentL or focusSegmentS
                 -> Int -- ^ The location of the caret
                 -> Segment -- ^ The first segment to consider
                 -> [Segment] -- ^ The rest of the segments on that line or within the SFormat
                 -> (Int, Maybe Segment, t') -- ^ The location of the cut, maybe a removed segment, and the Line' or Segment' as appropriate.
focusSegmentSegs con con' insL' focus n s ss
  | Span a b <- segmentSpan s -- If the caret is strictly within or just at the end of an SSelectEmoji at the start of the line or format chunk,
  , SSelectEmoji {} <- s
  , a < n && n <= b
  = subsegmentFocus -- We want to focusSegmentS within this segment.
  | Span a b <- segmentSpan s -- If the caret is touching a link (within, or at either end)
  , SLink {} <- s
  , a <= n && n <= b
  = subsegmentFocus -- We want to focusSegmentS within this segment.
  | Span a b <- segmentSpan s -- If the caret is touching a link (within, or at either end)
  , SMention {} <- s
  , a <= n && n <= b
  = subsegmentFocus -- We want to focusSegmentS within this segment.
  | Span a b <- segmentSpan s -- If the caret is strictly within some segment s,
  , a < n && n < b
  = case s of
      SFormat {} -> subsegmentFocus -- and s is an SFormat or SText, we want to focusSegmentS inside it.
      SText {} -> subsegmentFocus
      _ -> (spanEnd (segmentSpan s), Nothing, con' [s] SHole' ss) -- Otherwise, we cut the line/format block at the end of this segment, and don't pop out any segment.

  | Span _ b <- segmentSpan s -- If the caret is after the end of this segment...
  , b <= n
  = let (k, sF, l') = focus n (con ss (Span b n)) -- ...focus recursively on the rest of the segments in the line/format block
     in (k, sF, insL' s l') -- and then reinsert this initial segment on the left of the result.
  | Span a _ <- segmentSpan s -- If the caret is before the start of this segment...
  , n <= a
  = (a, Nothing, con' [] SHole' (s:ss)) -- Make an LLine'/LQuote'/SFormat ft with an SHole' focused, and nothing to the left.
  where
    subsegmentFocus = -- In case we want to focus inside this segment...
      let (k, sF, s') = focusSegmentS n s -- apply focusSegmentS to it, getting s' :: Segment'
       in (k, sF, con' [] s' ss) -- Make an LLine'/LQuote'/SFormat ft with s' focused in it.

focusSegmentSegs _ _ _ _ _ _ _ = error "focusSegmentSegs: this pattern match failure ought to be impossible"

-- | Insert a Segment at the very start of the left portion of an LQuote' or LLine', used to reinsert a segment skipped by recursion.
insertLeftLine' :: Segment -> Line' -> Line'
insertLeftLine' s (LQuote' sL s' sR) = LQuote' (s:sL) s' sR
insertLeftLine' s (LLine' sL s' sR) = LLine' (s:sL) s' sR

-- | Insert a Segment at the very start of the left portion of an SFormat'. If given any other Segment', this will fail.
insertLeftSFormat' :: Segment -> Segment' -> Segment'
insertLeftSFormat' s (SFormat' ft sL s' sR) = SFormat' ft (s:sL) s' sR
insertLeftSFormat' _ _ = error "insertLeftSFormat': applied to non-SFormat'"

-- | This does what focusSegmentPM does, but for a single Segment.
focusSegmentS :: Int -> Segment -> (Int, Maybe Segment, Segment')
focusSegmentS n (SFormat ft (s:ss) sp) -- If the segment is a nonempty SFormat...
  | Span a b <- segmentSpan s -- and the caret is strictly within the first segment,
  , a < n && n < b
  = focusSegmentSegs (SFormat ft) (SFormat' ft) insertLeftSFormat' focusSegmentS n s ss -- we apply focusSegmentSegs to determine how to deal with that first segment appropriately
  | Span _ b <- segmentSpan s -- If the caret is past the end of the first segment,
  , b <= n
  = let (k, sF, SFormat' _ sL s' sR) = focusSegmentS n (SFormat ft ss sp) -- recurse on the remainder of the segments in the format block
     in (k, sF, SFormat' ft (s:sL) s' sR) -- and put the first segment back into the result.
  | Span a _ <- segmentSpan s -- If the caret is on the left of the first segment...
  , n <= a
  = (a, Nothing, SFormat' ft [] SHole' (s:ss)) -- ... put the hole inside the format block, before the first segment.
focusSegmentS _ (SFormat ft [] (Span a _)) =  -- If the format block is empty,
  (a, Nothing, SFormat' ft [] SHole' []) -- just put a hole in it.
focusSegmentS n (SLink t url s@(Span a b)) = -- If the segment is a link,
  ( if n == a -- If the caret is at the left edge of the link,
      then a -- we're cutting at the start of the link
      else b -- otherwise, at the end of the link
  , Just (SLink t url s) -- we pop out the link, so that later a space can be inserted to the left or right of it as appropriate
  , SHole' -- and leave a hole.
  )
focusSegmentS n (SText s (Span a b)) = -- If the segment is a piece of text,
  let k = n-a -- Determine the number of characters on the left
      (before, after) = T.splitAt k s -- and split the text there
  in ( n -- The cut point is exactly on the caret
     , Nothing -- Nothing gets popped out
     , SSplit' (SText before (Span a n)) (SText after (Span n b)) -- The hole lies between two new text segments.
     )
focusSegmentS _ seg = (spanEnd (segmentSpan seg), Just seg, SHole') -- In all other cases, pop the segment out, leaving a hole.

-- | Reinsert a list of segments into a message with a hole in it, obtaining a ParsedMessage. Note that spans are not tracked through this part of the operation, so
-- any new Spans are just Span 0 0.
unfocusSegmentPM :: [Segment] -> ParsedMessage' -> ParsedMessage
unfocusSegmentPM ins (ParsedMessage' lL l' lR) = ParsedMessage (lL ++ unfocusSegmentL ins l' : lR) (Span 0 0)

-- | Reinsert a list of segments into a line with a hole in it, obtaining a new Line.
unfocusSegmentL :: [Segment] -> Line' -> Line
unfocusSegmentL ins (LQuote' sL s' sR) = LQuote (sL ++ unfocusSegmentS ins s' ++ sR) (Span 0 0)
unfocusSegmentL ins (LLine' sL s' sR) = LLine (sL ++ unfocusSegmentS ins s' ++ sR) (Span 0 0)

-- | Reinsert a list of segments into a segment with a hole in it, obtaining a new list of segments.
unfocusSegmentS :: [Segment] -> Segment' -> [Segment]
unfocusSegmentS ins SHole' = ins
unfocusSegmentS ins (SSplit' l r) = l : ins ++ [r]
unfocusSegmentS ins (SFormat' ft sL s' sR) = [SFormat ft (sL ++ unfocusSegmentS ins s' ++ sR) (Span 0 0)]

-- | Unparse a segment, resulting in the original text.
unparseS :: Segment -> Text
unparseS seg = case seg of
  SText s _               -> s
  SLink s _ _             -> s
  SEmoji s _              -> ":" <> s <> ":"
  SSelectEmoji es _       -> "+:" <> T.intercalate "," [e | (_,e) <- es] <> ":"
  SSelect _               -> ":O:"
  SCheckbox _             -> ":X:"
  SFormat FTBold segs _   -> "*" <> unparseSs segs <> "*"
  SFormat FTItalic segs _ -> "_" <> unparseSs segs <> "_"
  SFormat FTStrike segs _ -> "~" <> unparseSs segs <> "~"
  SCode s _               -> "`" <> s <> "`"
  SPre s _                -> "```" <> s <> "```"
  SMention s _            -> "@" <> s

-- | Unparse a list of segments by concatenating the results
unparseSs :: [Segment] -> Text
unparseSs = T.concat . map unparseS

-- | Unparse a Line
unparseL :: Line -> Text
unparseL ln = case ln of
  LQuote segs _ -> ">" <> unparseSs segs
  LLine segs _ -> unparseSs segs

-- | Unparse an entire ParsedMessage
unparsePM :: ParsedMessage -> Text
unparsePM pm = case pm of
  ParsedMessage lns _ -> T.intercalate "\n" (map unparseL lns)

-- | Get the Span associated with any given Segment.
segmentSpan :: Segment -> Span
segmentSpan seg = case seg of
  SText _ s        -> s
  SLink _ _ s      -> s
  SEmoji _ s       -> s
  SSelectEmoji _ s -> s
  SSelect s        -> s
  SCheckbox s      -> s
  SFormat _ _ s    -> s
  SPre _ s         -> s
  SCode _ s        -> s
  SMention _ s     -> s

-- | Get the Span associated with a given Line.
lineSpan :: Line -> Span
lineSpan ln = case ln of
  LQuote _ s -> s
  LLine _ s  -> s

-- | Get the list of segments in a line.
lineSegments :: Line -> [Segment]
lineSegments (LQuote ss _) = ss
lineSegments (LLine ss _) = ss

--- Utilities, not used internally to the parser or line editor ---

-- | Test if a link occurs in the given text.
hasLink :: Text -> Bool
hasLink msg = isJust $ do
  m <- runParserMaybe parseMessage msg
  listToMaybe (messageLinks m)

-- | Parse message text and extract the links from it.
textLinks :: Text -> [(Text, URI)]
textLinks t = case runParserMaybe parseMessage t of
  Nothing -> []
  Just pm -> messageLinks pm

-- | Extract the links from a parsed message.
messageLinks :: ParsedMessage -> [(Text, URI)]
messageLinks pm = [(t,url) | SLink t url _ <- flattenMessage pm]

-- | Extract all the @mentions
messageMentions :: ParsedMessage -> [Text]
messageMentions pm = [ t | SMention t _ <- flattenMessage pm ]

-- | Obtain a flat list of segments from a parsed message. This is particularly used in the backend when we need to discover if a message contains
-- actionable emojis.
flattenMessage :: ParsedMessage -> [Segment]
flattenMessage (ParsedMessage ls _) = ls >>= flattenLine

-- | c.f. flattenMessage, but for a line.
flattenLine :: Line -> [Segment]
flattenLine l = lineSegments l >>= flattenSegment

-- | c.f. flattenMessage, but for a single segment. In the case that the input segment is an SFormat, this flattens all its constituents recursively.
flattenSegment :: Segment -> [Segment]
flattenSegment (SFormat _ ss _) = ss >>= flattenSegment
flattenSegment s = [s]

-- | Show a summary of the users who have sent an actionable vote
showSenderList :: Int -- ^ The total number of users as reported by the DB
               -> Int
               -> [Text] -- ^ A subset of user names reported by the DB
               -> Text
showSenderList c lim names = T.intercalate ", " (take lim names) <> if c > lim then " and " <> T.pack (show (c - lim)) <> " others..." else ""
