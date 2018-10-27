module Pdf (
    TextBox(..),
    TextPage(..),
    Rect(..),
    extractText,
    scaleTextPage
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (catMaybes)
import System.IO

import Pdf.Document
import Pdf.Document.Internal.Types
import Pdf.Document.PageNode
import Pdf.Content.Processor
import Pdf.Content.Transform

{- Uses pdf-toolbox to extract all text from a pdf file -}

data Rect = Rect { left :: Double
                 , top :: Double
                 , width :: Double
                 , height :: Double 
                 } deriving (Show)

data TextBox = TextBox { box :: Rect
                       , text :: Text
                       } deriving (Show)

data TextPage = TextPage { contentBox :: Rect
                         , textBoxes :: [TextBox]
                         } deriving (Show)

scaleRect :: Double -> Rect -> Rect
scaleRect s (Rect l t w h) = Rect (s * l) (s * t) (s * w) (s * h)

scaleTextBox :: Double -> TextBox -> TextBox
scaleTextBox s (TextBox b t) = TextBox (scaleRect s b) t

scaleTextPage :: Double -> TextPage -> TextPage
scaleTextPage s (TextPage b bs) = TextPage (scaleRect s b) (scaleTextBox s <$> bs)

flattenGlyph :: Span -> TextBox
flattenGlyph (Span gs x) = 
    let (Vector l t) = (glyphTopLeft . head) gs
        (Vector r b) = (glyphBottomRight . last) gs
        text = Text.concat . catMaybes $ glyphText <$> gs
    in TextBox (Rect l t 0 (b - t)) text

extractText :: FilePath -> IO [TextPage]
extractText fp = withBinaryFile fp ReadMode $ \handle -> do
    pdf <- pdfWithHandle handle
    doc <- document pdf
    catalog <- documentCatalog doc
    rootNode <- catalogPageNode catalog
    kids <- pageNodeKids rootNode
    mapM (\ref -> do
                pn <- loadPageNode pdf ref 
                case pn of 
                    PageTreeNode (PageNode _ _ d) -> return undefined
                    PageTreeLeaf p -> do
                        gs <- pageExtractGlyphs p
                        (Rectangle l t w h) <- pageMediaBox p
                        return $ TextPage (Rect l t w h) $ flattenGlyph <$> gs) kids