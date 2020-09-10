module CommonMark.Structure
    ( Block(..)
    , Blocks(..)
    , Inline(..)
    , Inlines(..)
    , ListType(..)
    , ListEnumerator(..)
    , ListSpacing(..)
    , ListDelimiter(..)
    , ListItem(..)
    , ListItems(..)
    , Indent(..)
    , Level(..)
    , Info(..)
    , emptyOList
    , createOList
    
    , toHtml
    , toHtml'
    , toCmBlock
    ) where

import Data.Tree
import Data.List (dropWhileEnd)

data ListSpacing =
      LooseList
    | TightList
    deriving (Show, Eq)

data ListEnumerator = 
    Decimal
    deriving (Show, Eq)

data ListDelimiter =
      PeriodDelim
    | ParenDelim
    deriving (Show, Eq)

data ListType =
      OrderedList Int ListEnumerator ListDelimiter
    | UnorderedList Char
    deriving (Show, Eq)

data Block =
      Document String Blocks
    | BlockQuote Indent Level Blocks
    | FencedCode Indent Level Info String
    | IndentedCode Indent String
    | List ListType ListSpacing ListItems
    | AtxHeading Indent Level Inlines
    | SetextHeading Level Inlines
    | ThematicBreak Char
    | Paragraph Inlines
    | TmpBlock String
    deriving (Show)

data Inline =
      RawText String
    | Emphasis Char Inlines
    | Strong Char Inlines
    | InlineLink String Source Title
    | InlineImageLink String Source Title
    | CodeSpan String
    | Break
    deriving (Show)

type Source = String

type Title = String

type Indent = Int

type Level = Int

type Info = String

type Blocks = [Block]

type Inlines = [Inline]

type ListItem = Blocks

type ListItems = [ListItem]

emptyOList :: Int -> ListDelimiter -> Block
emptyOList n d = List (OrderedList n Decimal d) TightList []

createOList :: Int -> ListDelimiter -> ListItems -> Block
createOList n d = List (OrderedList n Decimal d) TightList

{-- Structure to HTML/CommonMark --}

-- HTML
toHtml :: Block -> String

toHtml (Document _ bs) = concatMap toHtml bs

toHtml (BlockQuote _ _ bs) = bqOpen ++ concatMap toHtml bs ++ bqClose
  where
    bqOpen = "<blockquote>"
    bqClose = "</blockquote>\n"

toHtml (FencedCode _ _ info text) = fcOpen ++ text ++ fcClose
  where
    fcOpen = "<pre><code>"
    fcClose = "</code></pre>\n"

toHtml (IndentedCode _ text) = icOpen ++ text ++ icClose
  where
    icOpen = "<pre><code>"
    icClose = "</code></pre>\n"

toHtml (AtxHeading _ l ils) = hOpen ++ concatMap toHtml' ils ++ hClose
  where
    hOpen = "<h" ++ show l ++ ">"
    hClose = "</h" ++ show l ++ ">\n"

toHtml (SetextHeading l ils) = hOpen ++ concatMap toHtml' ils ++ hClose
  where
    hOpen = "<h" ++ show l ++ ">"
    hClose = "</h" ++ show l ++ ">\n"

toHtml (ThematicBreak _) = "<hr />\n"

toHtml (Paragraph ils) = pOpen ++ concatMap toHtml' ils ++ pClose
  where
    pOpen = "<p>"
    pClose = "</p>\n"

toHtml (List (OrderedList start _ _) _ its) = lOpen ++ itsToHtml its ++ lClose
  where
    lOpen = "<ol start=\"" ++ show start ++ "\">\n"
    lClose = "</ol>\n"
    itsToHtml [] = ""
    itsToHtml (x:xs) = "<li>" ++ concatMap toHtml x ++ "</li>\n" ++ itsToHtml xs

toHtml _ = ""




-- RawText String
--     | Emphasis Char Inlines
--     | Strong Char Inlines
--     | InlineLink String Source Title
--     | InlineImageLink
--     | CodeSpan String
--     | Break

toHtml' :: Inline -> String

toHtml' (RawText text) = text

toHtml' (CodeSpan text) = cOpen ++ text ++ cClose
  where
    cOpen = "<span><code>"
    cClose = "</code></span>"

toHtml' (Emphasis _ ils) = emOpen ++ concatMap toHtml' ils ++ emClose
  where
    emOpen = "<em>"
    emClose = "</em>"

toHtml' (Strong _ ils) = sOpen ++ concatMap toHtml' ils ++ sClose
  where
    sOpen = "<strong>"
    sClose = "</strong>"

toHtml' (InlineLink text src title) = lOpen ++ text ++ lClose
  where
    lOpen = "<a href=\"" ++ src ++ "\"" ++ title' ++ ">"
    lClose = "</a>\n"
    title' = if title == ""
                then ""
                else " title=\"" ++ title ++ "\""

toHtml' (InlineImageLink desc src title) = "<img" ++ s ++ a ++ t ++ " />\n"
  where
    s = " src=\"" ++ src ++ "\""
    a = " alt=\"" ++ desc ++ "\""
    t = if title == ""
           then ""
           else " title=\"" ++ title ++ "\""

toHtml' Break = "<br />\n"


-- CommonMark
indent :: Indent -> String
indent n = replicate n ' '

toCmBlock :: Indent -> Block -> String
toCmBlock i (Document _ bs) = concatMap (toCmBlock i) bs

toCmBlock i (BlockQuote _ _ bs) = indText ++ "\n"
  where
    bsText = concatMap (toCmBlock i) bs
    ind = indent i ++ "> "
    indText = foldl (\a x->a ++ ind ++ x ++ "\n") "" $ dropWhileEnd (== "") $ lines bsText

toCmBlock i (FencedCode _ lvl info text) = ind ++ fcOpen ++ ind ++ text ++ ind ++ fcClose
  where
    fcOpen = replicate lvl '`' ++ " " ++ info ++ "\n"
    fcClose = "\n" ++ replicate lvl '`' ++ "\n\n"
    ind = indent i

toCmBlock i (IndentedCode _ text) = indent i ++ format text ++ "\n\n"
  where
    format s = unlines $ map (indent (i + 4) ++) . lines $ s

toCmBlock i (AtxHeading _ l il) = indent i ++ atxPref ++ concatMap toCmInline il ++ "\n\n"
  where
    atxPref = replicate l '#' ++ " "

toCmBlock i (SetextHeading l il) = ind ++ concatMap toCmInline il ++ "\n" ++ ind ++ setext ++ "\n"
  where
    setext = if l == 1 then "===\n" else "---\n"
    ind = indent i

toCmBlock i (ThematicBreak c) = "\n" ++ indent i ++ replicate 3 c ++ "\n\n"

toCmBlock i (Paragraph il) = indent i ++ concatMap toCmInline il ++ "\n\n"

toCmBlock i ls@(List (OrderedList st en del) _ its) = concatMap (toCmListItem d) its
  where
    d  = indent i ++ show st ++ en ++ "\n"
    en = if del == ParenDelim then ") " else ". "

toCmBlock _ _ = ""

toCmListItem :: String -> ListItem -> String
toCmListItem del li = ""
  where
    dl = length del
  
nestedList :: Block -> Int -> String -> String
nestedList ls@(List _ _ its) n d = ""

toCmInline :: Inline -> String

toCmInline (RawText t) = t

toCmInline (Emphasis c ils) = [c] ++ concatMap toCmInline ils ++ [c]

toCmInline (Strong c ils) = [c,c] ++ concatMap toCmInline ils ++ [c,c]

toCmInline (CodeSpan t) = "`" ++ t ++ "`"

toCmInline (InlineLink text src title) = "[" ++ text ++ "](" ++ src ++ lClose
  where
    lClose = if title == ""
             then ")"
             else " \"" ++ title ++ "\")"

toCmInline (InlineImageLink t src title) = "![" ++ t ++ "](" ++ src ++ lClose
  where
    lClose = if title == ""
             then ")"
             else " \"" ++ title ++ "\")"
