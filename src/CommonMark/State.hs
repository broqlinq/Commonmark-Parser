module CommonMark.State
    ( BlockType(..)
    , BlockData(..)
    , BlockState(..)
    , BlockStack(..)

    , emptyDocument
    , emptyBlockQuote
    , emptyFencedCode
    , emptyIndentedCode
    , emptyListItem
    , emptyParagraph
    , emptyThematicBreak
    ) where

data BlockType =
      BTDocument
    | BTBlockQuote
    | BTListItem
    | BTList Char Int
    | BTFencedCode String
    | BTIndentedCode
    | BTParagraph
    | BTThematicBreak Char
    | BTLinkReference
    | BTAtxHeading Int
    | BTSetexHeading Int
    deriving (Show, Eq)

data BlockData = BlockData { blockType          :: !BlockType
                           , blockContent       :: ![String]
                           , blockChildren      :: ![BlockData]
                           , blockIsOpen        :: !Bool
                           , blockIsContainer   :: !Bool
                           , blockContainsLines :: !Bool
                           , blockIsLazy        :: !Bool
                           }
                           deriving (Show)

data BlockState = BlockState { blockStack :: !BlockStack
                             , refMap :: [Int]
                             }
                             deriving (Show)

type BlockStack = [BlockData]

getCurrentBlockType :: BlockState -> BlockType
getCurrentBlockType = blockType . getCurrentBlockData

getCurrentBlockData :: BlockState -> BlockData
getCurrentBlockData bs = b
    where
        (b:_) = blockStack bs




{-- Base Block Data for all possible block types --}

emptyDocument :: BlockData
emptyDocument = BlockData { blockType          = BTDocument
                          , blockContent       = []
                          , blockChildren      = []
                          , blockIsOpen        = True
                          , blockIsContainer   = True
                          , blockContainsLines = False
                          , blockIsLazy        = False
                          }

emptyBlockQuote :: BlockData
emptyBlockQuote = BlockData { blockType          = BTBlockQuote
                            , blockContent       = []
                            , blockChildren      = []
                            , blockIsOpen        = True
                            , blockIsContainer   = True
                            , blockContainsLines = False
                            , blockIsLazy        = True
                            }

emptyListItem :: BlockData
emptyListItem = BlockData { blockType          = BTListItem
                          , blockContent       = []
                          , blockChildren      = []
                          , blockIsOpen        = True
                          , blockIsContainer   = True
                          , blockContainsLines = True
                          , blockIsLazy        = True
                          }

emptyFencedCode :: String -> BlockData
emptyFencedCode info = BlockData { blockType        = BTFencedCode info
                                 , blockContent       = []
                                 , blockChildren      = []
                                 , blockIsOpen        = True
                                 , blockIsContainer   = False
                                 , blockContainsLines = True
                                 , blockIsLazy        = True
                                 }

emptyIndentedCode :: BlockData
emptyIndentedCode = BlockData { blockType      = BTIndentedCode
                              , blockContent       = []
                              , blockChildren      = []
                              , blockIsOpen        = True
                              , blockIsContainer   = False
                              , blockContainsLines = True
                              , blockIsLazy        = True
                              }

emptyParagraph :: BlockData
emptyParagraph = BlockData { blockType          = BTParagraph
                           , blockContent       = []
                           , blockChildren      = []
                           , blockIsOpen        = True
                           , blockIsContainer   = False
                           , blockContainsLines = True
                           , blockIsLazy        = True
                           }

emptyThematicBreak :: Char -> BlockData
emptyThematicBreak c = BlockData { blockType          = BTThematicBreak c
                                 , blockContent       = []
                                 , blockChildren      = []
                                 , blockIsOpen        = False
                                 , blockIsContainer   = False
                                 , blockContainsLines = False
                                 , blockIsLazy        = False
                                 }