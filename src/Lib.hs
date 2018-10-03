{-# LANGUAGE DataKinds #-}

module Lib
    ( someFunc
    ) where

import System.IO
import Pdf.Document
import Pdf.Document.Internal.Types
import Pdf.Document.PageNode
import Pdf.Content.Processor

import Money

import Data.Int
import Data.List (sortBy, groupBy)
import Data.Text (Text)
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

data Date = Date !Int16 !Int8 !Int8
            deriving (Show, Eq, Ord)

newtype Money = Money { unMoney :: Double }
                deriving (Show)

data Statement = Statement 
    { fromDate :: Date
    , toDate :: Date
    , accountNumber :: Text
    , accountName :: Text
    , summary :: Summary
    , transactions :: [Transaction]
    } deriving (Show)

data Summary = Summary
    { beginningBalance :: Dense "USD"
    , endingBalance :: Dense "USD"
    , depositsCount :: Int
    , depositsSum :: Dense "USD"
    , withdrawalsCount :: Int
    , withdrawalsSum :: Dense "USD"
    } deriving (Show)

data Amount = Deposit (Dense "USD")
            | Withdrawal (Dense "USD")
            deriving (Show)

data Transaction = Transaction
    { id :: Text
    , date :: Date
    , postDate :: Date
    , description :: Text
    , amount :: Amount
    , balance :: Dense "USD"
    } deriving (Show)

{- Relatively simple validation to make sure the statement is being processed correctly -}
validateStatement :: Statement -> Maybe String
validateStatement s
    | toDate s >= fromDate s = Just $ "fromDate " ++ show (fromDate s) ++ " should be before toDate " ++ show (toDate s)
    | depositsCount (summary s) /= length deposits = Just $ "Expected " ++ show (depositsCount (summary s)) ++ " deposits, found " ++ show (length deposits)
    | depositsSum (summary s) /= foldr (+) (dense' 0.0) deposits = Just $ "Expected " ++ show (depositsSum (summary s)) ++ " of deposits, found " ++ show (foldr (+) (dense' 0.0) deposits) 
    | withdrawalsCount (summary s) /= length withdrawals = Just $ "Expected " ++ show (withdrawalsCount (summary s)) ++ " withdrawals, found " ++ show (length withdrawals)
    | withdrawalsSum (summary s) /= foldr (+) (dense' 0.0) withdrawals = Just $ "Expected " ++ show (withdrawalsSum (summary s)) ++ " of withdrawls, found " ++ show (foldr (+) (dense' 0.0) deposits)
    | endingBalance (summary s) /= (balance . last . transactions) s = Just $ "Expected ending balance of " ++ show (endingBalance (summary s)) ++ " , found " ++ show ((balance . last . transactions) s)
    | otherwise = case foldr mapper (Right $ beginningBalance . summary $ s) (transactions s) of
                       Left s -> Just s
                       _ -> Nothing
    where deposits = [ x | (Deposit x) <- amount <$> transactions s]
          withdrawals = [ x | (Withdrawal x) <- amount <$> transactions s]

          flattenAmount (Deposit x) = x
          flattenAmount (Withdrawal x) = -x

          mapper :: Transaction -> Either String (Dense "USD") -> Either String (Dense "USD")
          mapper _ (Left s) = Left s
          mapper t (Right a) = if a + flattenAmount (amount t) == balance t
                                   then Right $ balance t
                                   else Left $ "Expected ending balance of " ++ show (a + flattenAmount (amount t)) ++ ", but found " ++ show (balance t)

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