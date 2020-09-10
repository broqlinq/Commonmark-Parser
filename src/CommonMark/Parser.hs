module CommonMark.Parser
    ( pDocument
    , pBlocks
    , pBlockQuote
    , pBlockQuoteLines
    , pBlockQuoteLines'
    , pOList
    , pOListItem
    , pListFirstLine
    , pListNextLine
    , countIndent
    , pAtxHeading
    , pSetextHeading
    , pThematicBreak
    , pFencedCode
    , pIndentedCode
    , pLinkReference
    , pBlankLine
    , pParagraph
    , pParagraph'

    , pInlines
    , pInlineLink
    , pInlineLinkText
    , pInlineLinkText'
    , pInlineLinkSource
    , pInlineLinkSource'

    , BState(..)
    , initState
    , listState
    , eol
    , checkLine
    ) where

import CommonMark.Structure

import Text.Parsec
import Data.List
import Control.Monad (guard, void)
import qualified Data.Map as Map

type Key = String

type LinkReference = (String, String)

data BState = BState { indent :: Int
                     , bqLevel :: Int
                     , listIndent :: Int
                     , listSym :: Char
                     , listSpacing :: ListSpacing
                     , refMap :: Map.Map Key LinkReference
                     }
                     deriving (Show)


initState :: BState
initState = BState { indent = 0
                   , bqLevel = 0
                   , listIndent = 0
                   , listSym = ' '
                   , listSpacing = TightList
                   , refMap = mempty
                   }

listState :: BState
listState = initState { listSym = ')' }

insertReference :: (Key, LinkReference) -> Parsec String BState ()
insertReference (key, val) = do
    rm <- refMap <$> getState
    updateState (\st -> st{ refMap = insert' rm })
    return ()
    where
        insert' = Map.insertWith ov' key val
        ov' _ y = y

pDocument :: String -> IO (Either ParseError Block)
pDocument fileName = do
    fileContent <- readFile fileName
    let res = runParser pBlocks initState fileName fileContent
    case res of
        Right bs -> return $ Right (Document fileName bs)
        Left err  -> return $ Left err

pBlocks :: Parsec String BState Blocks
pBlocks = do
    bs <- many pBlock'
    eof
    return bs
    where
        pBlock' = try (pBlankLine >> return (TmpBlock "BLANK"))
                  <|> try pBlockQuote
                  <|> try pOList
                  <|> try pFencedCode
                  <|> try pIndentedCode
                  <|> try (pLinkReference >> return (TmpBlock "LINK REF"))
                  <|> try pAtxHeading
                  <|> try pSetextHeading
                  <|> try pThematicBreak
                  <|> pParagraph
                  <?> "fatal parsing error"

{-- Single Block parsers --}

-- ATX Heading
pAtxHeading :: Parsec String BState Block
pAtxHeading = do
    nonindentSpaces
    lvl <- fromToChars '#' 1 6
    oneOf " \t"
    whitespace
    text <- trimHeading <$> anyTillEnd
    st <- getState
    let res = runParser pInlines st "" text
    case res of
        Left err  -> error "inline parsing failed"
        Right ils -> return $ AtxHeading 0 lvl ils

-- Setext Heading
pSetextHeading :: Parsec String BState Block
pSetextHeading = do
    (Paragraph ils) <- pParagraph
    fromToChars ' ' 0 3
    c <- oneOf "-="
    many $ char c
    ln <- many $ noneOf "\r\n"
    eol <|> eof
    guard $ onlyOf " \t\r\n" ln
    return $ SetextHeading (lvl c) ils
    where
        lvl x = if x == '=' then 1 else 2

-- Block Quote
pBlockQuote :: Parsec String BState Block
pBlockQuote = do
    upToChars ' ' 3
    char '>'
    upToChars ' ' 1
    ls <- pBlockQuoteLines
    st <- getState
    let res = runParser pBlocks st "" ls
    case res of
        Left err -> return $ BlockQuote 0 1 []
        Right bs -> return $ BlockQuote 0 1 bs

pBlockQuoteLines :: Parsec String BState String
pBlockQuoteLines = do
    ln <- fstLine
    rl <- many pBlockQuoteLines'
    return $ ln ++ concat rl
    where
        fstLine = do
            x <- many $ noneOf "\r\n"
            e <- option "" (eol >> return "\n")
            return $ x ++ e

pBlockQuoteLines' :: Parsec String BState String
pBlockQuoteLines' = do
    notFollowedBy $     try atxLn 
                    <|> try fCodeLn
                    <|> try uListLn 
                    <|> try oListLn
    try bqPref <|> noBqPref
    where
        atxLn = do
            nonindentSpaces
            fromToChars '#' 1 6
            void (oneOf " \t") <|> eof

        uListLn = do
            nonindentSpaces
            oneOf "-*"
            void (oneOf " \t") <|> eof

        oListLn = do
            nonindentSpaces
            many digit
            oneOf ".)"
            void (oneOf " \t") <|> eof

        fCodeLn = do
            nonindentSpaces
            f <- many1 $ char '`'
            let l = length f
            guard $ l >= 3
            many $ noneOf "`\r\n"
            eol <|> eof

        noBqPref = do
            ln <- many1 $ noneOf "\r\n"
            el <- option "" (eol >> return "\n")
            return $ ln ++ el
        
        bqPref = do
            nonindentSpaces
            char '>'
            upToChars ' ' 1
            noBqPref

-- Lists Ordered/Bullet
pOList :: Parsec String BState Block
pOList = do
    spPref <- nonindentSpaces
    start <- many1 digit
    del <- listDelim
    s <- (char ' ' >> return (1 :: Int)) <|> ((eol <|> eof) >> return (0 :: Int))
    let ind = spPref + length start + 1 + s
    let sym = if del == ParenDelim then ')' else '.'
    (fsln, newind) <- pListFirstLine ind
    -- oind <- listIndent <$> getState
    updateState (\st -> st{ listIndent = newind
                          , listSpacing = TightList
                          , listSym = sym})
    rln <- many pListNextLine
    -- updateState (\st -> st{listIndent = oind})

    rli <- many pOListItem

    st <- getState
    let res = runParser pBlocks st "" (concat $ fsln:rln)
    case res of
        Left _    -> return $ createOList (read start) del []
        Right lis -> return $ createOList (read start) del (lis:rli)

pOListItem :: Parsec String BState ListItem
pOListItem = try (do
    spPre <- nonindentSpaces
    sym <- listSym <$> getState
    start <- many1 digit
    char sym >> char ' '
    let ind = spPre + length start + 2
    (fl, nind) <- pListFirstLine ind
    -- oind <- listIndent <$> getState
    updateState (\st -> st{ listIndent = nind })
    rl <- many pListNextLine
    -- updateState (\st -> st{listIndent = oind})

    st <- getState
    let res = runParser pBlocks st "" (concat $ fl:rl)
    case res of
        Left _   -> return []
        Right li -> return li)

pListFirstLine :: Int -> Parsec String BState (String, Int)
pListFirstLine oind = do
    sp <- countIndent
    guard $ sp < 4
    ln <- option "" anyTillEndLn
    return (ln, oind + sp)

pListNextLine :: Parsec String BState String
pListNextLine = try (do
    ind <- listIndent <$> getState
    count ind $ char ' '
    option "" anyTillEndLn)
    <|> try (do
            notFollowedBy $     try oListPref
                            <|> try uListPref
                            <|> try atxPref
                            <|> try fencedCodePref
                            <|> try thBreak
            anyTillEndLn)
    where
        thBreak = do
            nonindentSpaces
            c <- oneOf "-*_"
            br <- many $ char c
            guard $ length (c:br) >= 3
            whitespace
            many (char 'c' >> whitespace)
            eol <|> eof
            return ()

-- Fenced Code Block
pFencedCode :: Parsec String BState Block
pFencedCode = do
    fence <- many1 $ char '`'
    let l = length fence
    guard $ l >= 3
    whitespace
    info <- many $ noneOf "` \r\n"
    whitespace
    eol
    text <- pFencedCode' l
    return $ FencedCode 0 l info text

pFencedCode' :: Int -> Parsec String BState String
pFencedCode' l = (eof >> return "") 
    <|> try (do
                count l (char '`')
                many $ char '`'
                whitespace
                eol <|> eof
                return "")
    <|> (do
            st <- many $ noneOf "\r\n"
            eol <|> eof
            rest <- pFencedCode' l
            case rest of
                [] -> return $ st ++ rest
                _  -> return $ unlines $ st:lines rest)

-- Indented Code Block
pIndentedCode :: Parsec String BState Block
pIndentedCode = do
    count 4 $ char ' '
    ln <- many1 $ noneOf "\r\n"
    guard $ not . onlyOf " \t" $ ln
    eol <|> eof
    rest <- pIndentedCode'
    return $ IndentedCode 0 (unlines $ ln:lines rest)

pIndentedCode' :: Parsec String BState String
pIndentedCode' = try (do
        count 4 $ char ' '
        ln <- many $ noneOf "\r\n"
        eof <|> eol
        rest <- pIndentedCode'
        case rest of
            [] -> return $ ln ++ rest
            _  -> return $ unlines $ ln:lines rest)
        <|> return ""

-- Link Reference
pLinkReference :: Parsec String BState BState
pLinkReference = do
    nonindentSpaces
    key <- betweenBrackets $ many (noneOf "]\r\n")
    guard $ not . onlyOf " \t" $ key
    char ':'
    whitespace
    link <- many1 $ noneOf " \t\r\n"
    whitespace
    title <- option "" $ many1 $ noneOf " \t\r\n"
    whitespace
    eol <|> eof
    insertReference (key, (link, title))
    getState

-- Paragraph
pParagraph :: Parsec String BState Block
pParagraph = do
    nonindentSpaces
    ln <- anyTillEndLn
    guard $ not . onlyOf " \t" $ ln
    rest <- option "" (try pParagraph')
    st <- getState
    let res = runParser pInlines st "" (ln ++ rest)
    case res of
        Left err  -> error "inline parsing failed"
        Right ils -> return $ Paragraph ils

pParagraph' :: Parsec String BState String
pParagraph' = do
    whitespace
    l <- pParagraphLine'
    rest <- option "" (try pParagraph')
    case rest of
        [] -> return l
        _  -> return $ l ++ rest

pParagraphLine' :: Parsec String BState String
pParagraphLine' = do
    notFollowedBy $     try atxPref
                    <|> try setextLn
                    <|> try blockQuotePref
                    <|> try fencedCodePref
                    <|> try oListPref
                    <|> try uListPref
    anyTillEndLn

-- Thematic Break
pThematicBreak :: Parsec String BState Block
pThematicBreak = do
    nonindentSpaces
    c <- oneOf "-_*"
    whitespace
    count 2 $ sym' c
    many $ sym' c
    eol <|> eof
    return $ ThematicBreak c
    where
        sym' x = void (char x) >> whitespace

-- Blank Line
pBlankLine :: Parsec String BState ()
pBlankLine = do
    whitespace
    eol
    return ()


{-- Inline parsers --}

-- Order:
-- 1) inline code blocks
-- 2) breaks
-- 3) links
-- 4) emphasis

pInlines :: Parsec String BState Inlines
pInlines = many $ do
        try pCodeSpan
    <|> try pInlineImageLink
    <|> try pInlineLink
    <|> (do
        c <- anyChar
        txt <- many $ noneOf "`*_[!"
        return $ RawText $ replaceNewlines $ c:txt)

pCodeSpan :: Parsec String BState Inline
pCodeSpan = do
    char '`'
    text <- many1 $ noneOf "`"
    char '`'
    void (noneOf "`") <|> eof
    return $ CodeSpan $ trimWhitespace . replaceNewlines $ text

pInlineLink :: Parsec String BState Inline
pInlineLink = do
    text <- pInlineLinkText
    (src, title) <-     try pInlineLinkSource
                    <|> pInlineReferenceLink
    return $ InlineLink text src title

pInlineLinkText :: Parsec String BState String
pInlineLinkText = try (do
    char '['
    rs <- many pInlineLinkText'
    char ']'
    return $ concat rs)

pInlineLinkText' :: Parsec String BState String
pInlineLinkText' = try (many1 $ noneOf "[]")
    <|> try (notFollowedBy (try pInlineLink) >> brackets)
    where
        brackets = do
            char '['
            ts <- many pInlineLinkText'
            char ']'
            return $ "[" ++ concat ts ++ "]"
    
pInlineLinkSource :: Parsec String BState (String, String)
pInlineLinkSource = do 
    char '('
    whitespace
    sr <- many pInlineLinkSource'
    tt <- option "" pInlineLinkTitle
    whitespace
    char ')'
    return (concat sr, tt)

pInlineLinkSource' :: Parsec String BState String
pInlineLinkSource' = try (many1 $ noneOf " ()")
    <|> try parens
    where
        parens = do
            char '('
            s <- many ilsrc
            char ')'
            return $ "(" ++ concat s ++ ")"
        ilsrc =     try (many1 $ noneOf "()")
                <|> try parens

pInlineLinkTitle :: Parsec String BState String
pInlineLinkTitle = do
    char '"'
    t <- many $ noneOf "\""
    char '"'
    return t

pInlineReferenceLink :: Parsec String BState (String, String)
pInlineReferenceLink = do
    char '['
    key <- many $ noneOf "[]"
    char ']'
    rMap <- refMap <$> getState
    let val = Map.lookup key rMap
    case val of
        Just src -> return src
        _        -> guard False >> return ("","")

pInlineImageLink :: Parsec String BState Inline
pInlineImageLink = do
    char '!'
    (InlineLink c s t) <- pInlineLink
    return $ InlineImageLink c s t

pStrongEmphasis :: Parsec String BState Inline
pStrongEmphasis = do
    
    return $ RawText ""

{-- Utility parsers --}

countIndent :: Parsec String BState Int
countIndent = sum <$> many (char ' ' >> return 1)

betweenBrackets :: Parsec String BState String -> Parsec String BState String
betweenBrackets = between (char '[') (char ']')

betweenQuotes :: Parsec String BState String -> Parsec String BState String
betweenQuotes = between (char '"') (char '"')

nonindentSpaces :: Parsec String BState Int
nonindentSpaces = upToChars ' ' 3

whitespace :: Parsec String BState ()
whitespace = void $ many $ oneOf " \t"

upToChars :: Char -> Int -> Parsec String BState Int
upToChars c n = try (do
    guard $ n > 0
    char c
    r <- upToChars c (n-1)
    return $ r + 1)
    <|> return 0

fromToChars :: Char -> Int -> Int -> Parsec String BState Int
fromToChars c lb ub = do
    count lb $ char c
    r <- upToChars c (ub - lb)
    return $ lb + r

anyTillEnd :: Parsec String BState String
anyTillEnd = do
    s <- many1 $ noneOf "\r\n"
    eol <|> eof
    return s

anyTillEndLn :: Parsec String BState String
anyTillEndLn = do
    s <- many1 $ noneOf "\r\n"
    (eol >> return (unlines [s])) <|> (eof >> return s)

eol :: Parsec String BState ()
eol = void (try (string "\r\n") <|> try (string "\n\r") <|> string "\r" <|> string "\n" <?> "end of line")

listDelim :: Parsec String BState ListDelimiter
listDelim = (char '.' >> return PeriodDelim) 
            <|> (char ')' >> return ParenDelim)

atxPrefixes :: [String]
atxPrefixes = ["# ", "## ", "### ", "#### ", "##### ", "###### "]

bqPrefixes :: [String]
bqPrefixes = [ "> ", " > ", "  > ", "   > "
             , ">", " >", "  >", "   >" ]

setextLine :: String -> Bool
setextLine [] = False
setextLine s  = (splen <= 3) && (onlyOf "-" ts || onlyOf "=" ts)
    where
        splen = length . takeWhile (== ' ') $ s
        ts    = tws . reverse . tws $ s
        tws   = dropWhile (`elem` " \t\r\n")

oListPref :: Parsec String BState ()
oListPref = void $ do
            nonindentSpaces
            many1 digit
            oneOf ".)"
            void (char ' ') <|> eol <|> eof

uListPref :: Parsec String BState ()        
uListPref = void $ do
    nonindentSpaces
    oneOf "-*"
    void (char ' ') <|> eol <|> eof

atxPref :: Parsec String BState ()
atxPref = void $ nonindentSpaces >> fromToChars '#' 1 6 >> char ' '

setextLn :: Parsec String BState ()
setextLn = void $ do 
    nonindentSpaces
    c <- oneOf "-="
    many $ char c
    whitespace
    eol <|> eof

blockQuotePref :: Parsec String BState ()
blockQuotePref = void $ nonindentSpaces >> char '>'

fencedCodePref :: Parsec String BState ()
fencedCodePref = void $ do
    nonindentSpaces
    f <- many1 $ char '`'
    guard $ length f >= 3
    many $ noneOf "`\r\n"
    eol <|> eof

{-- Utility functions --}

isFencedLn :: String -> Bool
isFencedLn [] = False
isFencedLn s = nsplen <= 3 && flen >= 3 && noacc
    where
        nsplen = length . takeWhile (== ' ') $ s
        flen = length . takeWhile (== '`') . dropWhile (== ' ') $ s
        noacc = notElem '`' $ dropWhile (== '`') . dropWhile (== ' ') $ s

replaceNewlines :: String -> String
replaceNewlines [] = []
replaceNewlines (x:xs) = nx : replaceNewlines xs
    where
        nx = if x == '\n' then ' ' else x

trimWhitespace :: String -> String
trimWhitespace = let ds = dropWhile (`elem` " \t") in
    reverse . ds . reverse . ds

isBqPrefix :: String -> Bool
isBqPrefix "" = False
isBqPrefix s = any (`isPrefixOf` s) bqPrefixes

onlyOf :: (Eq a) => [a] -> [a] -> Bool
onlyOf [] _ = True
onlyOf _ [] = True
onlyOf ls (x:xs) = x `elem` ls && onlyOf ls xs

trimHeading :: String -> String
trimHeading [] = []
trimHeading st
    | canTrim st = reverse . fspace . fhash . fspace . reverse $ st
    | otherwise  = st
    where
        canTrim s' = (take 1 . fhash . fspace . reverse $ s') `elem` [" ","\t"]
        fhash      = dropWhile (== '#')
        fspace     = dropWhile (== ' ')


group' :: [Char] -> [[Char]]
group' = groupBy f
    where
        f ' ' ' ' = True
        f ' ' _   = False
        f _ ' '   = False
        f _ _     = True


checkLine :: Parsec String BState Block
checkLine = do
    spPre <- nonindentSpaces
    start <- many1 digit
    delim <- listDelim
    char ' '
    let ind = spPre + length start + 2
    let sym = if delim == ParenDelim then ')' else '.'
    (fln, nind) <- pListFirstLine ind
    -- oind <- listIndent <$> getState
    updateState (\st -> st{ listIndent = nind
                          , listSpacing = TightList
                          , listSym = sym})
    rln <- many pListNextLine
    -- updateState (\st -> st{listIndent = oind})

    rli <- many pOListItem

    -- st <- getState
    -- let res = runParser pBlocks st "" (concat $ fln:rln)
    -- case res of
    --     Left _    -> return $ createOList (read start) delim []
    --     Right lis -> return $ createOList (read start) delim (lis:rli)
    return $ TmpBlock (concat $ fln:rln)