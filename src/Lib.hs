module Lib
    ( someFunc
    ) where

import System.IO
import Pdf.Document
import Pdf.Document.Internal.Types
import Pdf.Document.PageNode
import Pdf.Content.Processor

import Data.HashMap.Strict
import qualified Data.Text as T
import Text.Groom
import Data.Maybe (catMaybes)
import Pdf.Content.Transform as V

data TextBox = TextBox { _topLeft :: V.Vector Double
                       , _bottomRight :: V.Vector Double
                       , _text :: T.Text
                       } deriving (Show)

flattenGlyph :: Span -> TextBox
flattenGlyph (Span gs _) = 
    let topLeft = (glyphTopLeft . head) gs
        bottomRight = (glyphBottomRight . last) gs
        text = T.concat . catMaybes $ glyphText <$> gs
    in TextBox topLeft bottomRight text

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
    return pageNodes