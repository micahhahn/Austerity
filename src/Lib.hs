module Lib
    ( someFunc
    ) where

import System.IO
import Pdf.Document
import Pdf.Document.Internal.Types
import Pdf.Document.PageNode
import Pdf.Content.Processor

import Data.List (sortBy, groupBy)
import Data.HashMap.Strict
import qualified Data.Text as T
import Text.Groom
import Data.Maybe (catMaybes)
import Pdf.Content.Transform as V

data TextBox = TextBox { _left :: Int
                       , _top :: Int
                       , _bottom :: Int
                       , _right :: Int
                       , _text :: T.Text
                       } deriving (Show)

data TextSpan = TextSpan { _sLeft :: Int
                         , _sRight :: Int
                         , _sText :: T.Text 
                         } deriving (Show)

flattenGlyph :: Span -> TextBox
flattenGlyph (Span gs _) = 
    let (Vector left top) = (glyphTopLeft . head) gs
        (Vector right bottom) = (glyphBottomRight . last) gs
        text = T.concat . catMaybes $ glyphText <$> gs
    in TextBox (floor left) (floor top) (floor bottom) (floor right) text

sortNodes :: TextBox -> TextBox -> Ordering
sortNodes (TextBox l1 t1 r1 b1 text1) (TextBox l2 t2 r2 b2 text2) = case compare t1 t2 of
    EQ -> case compare l1 l2 of
        {- EQ -> error $ "Overlapping textboxes! " ++ (T.unpack text1) ++ " | " ++ (T.unpack text2) -}
        x -> x
    LT -> GT
    GT -> LT

lines' ts = 
    let sorted = sortBy sortNodes ts
        grouped = groupBy (\l r -> _top l == _top r) sorted
        fmaped = ((\(TextBox l _ _ r t) -> TextSpan l r t) <$>) <$> grouped
    in fmaped

someFunc = withBinaryFile "C:\\Users\\micah\\Dropbox\\Financial\\First National Bank\\Regular Checking X3203\\Statement Closing 2017-11-17.pdf" ReadMode $ \handle -> do
    pdf <- pdfWithHandle handle
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    kids <- pageNodeKids rootNode
    pageNodes <- mapM (\ref -> do
                        pn <- loadPageNode pdf ref 
                        case pn of 
                            PageTreeNode (PageNode _ _ d) -> return undefined
                            PageTreeLeaf p -> do
                                gs <- pageExtractGlyphs p
                                return $ flattenGlyph <$> gs) kids
    let ls = lines' <$> pageNodes
    return ls