{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Ratio ((%))
import qualified Data.Text as T
import qualified Text.Read as R
import Text.Groom
import Data.Maybe (catMaybes)
import Pdf.Content.Transform as V

data TextBox = TextBox { _left :: Int
                       , _top :: Int
                       , _bottom :: Int
                       , _right :: Int
                       , _text :: T.Text
                       } deriving (Show)

data TextSpan = TextSpan { sLeft :: Int 
                         , sText :: T.Text
                         } deriving (Show)

data Date = Date { year :: !Int16
                 , month :: !Int8
                 , day :: !Int8
                 } deriving (Show, Eq, Ord)

newtype Money = Money { unMoney :: Double }
                deriving (Show)

data Statement = Statement 
    { header :: Header
    , summary :: Summary
    , transactions :: [Transaction]
    } deriving (Show)

data Header = Header
    { fromDate :: Date
    , toDate :: Date
    , accountNumber :: Text
    , accountName :: Text
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
    | toDate (header s) >= fromDate (header s) = Just $ "fromDate " ++ show (fromDate (header s)) ++ " should be before toDate " ++ show (toDate (header s))
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

getMonth :: Integral a => Text -> Maybe a
getMonth "January" = Just 1
getMonth "February" = Just 2
getMonth "March" = Just 3
getMonth "April" = Just 4
getMonth "May" = Just 5
getMonth "June" = Just 6
getMonth "July" = Just 7
getMonth "August" = Just 8
getMonth "September" = Just 9
getMonth "October" = Just 10
getMonth "November" = Just 11
getMonth "December" = Just 12
getMonth _ = Nothing

toEither :: Maybe a -> String -> Either String a
toEither (Just a) _ = Right a
toEither Nothing s = Left s

readMaybe :: Read a => Text -> Maybe a
readMaybe = R.readMaybe . T.unpack

parseStatementDate :: Text -> Text -> Text -> Either String Date
parseStatementDate yt mt dt = do
    y <- toEither (readMaybe yt) $ "Could not parse year '" ++ (T.unpack yt) ++ "'"
    m <- toEither (getMonth mt) $ "Could not parse month '" ++ (T.unpack mt) ++ "'"
    d <- toEither (readMaybe . T.init $ dt) $ "Could not parse day '" ++ (T.unpack dt) ++ "'"
    return $ Date y m d

readDollars :: Text -> Either String (Dense "USD")
readDollars t = do
    let ts = T.splitOn "." t
    if length ts == 2 then pure () else Left $ "Expected dot in amount '" ++ (T.unpack t) ++ "'"
    let (ds : cs : []) = ts
    dollars <- toEither (readMaybe (T.filter (/= ',') ds) :: Maybe Integer) $ "Could not parse '" ++ (T.unpack ds) ++ "'"
    cents <- toEither (readMaybe (T.filter (/= ',') cs) :: Maybe Integer) $ "Cound not parse '" ++ (T.unpack cs) ++ "'"
    return (dense' (((dollars * 100) + cents) % 100) :: Dense "USD") 

readShortDate :: Text -> (Date, Date) -> Either String Date
readShortDate t (startDate, endDate) = do
    let ds = T.splitOn "/" t
    if length ds == 2 then pure () else Left $ "Expected date to be in form mm/dd"
    let (mt : dt : []) = ds
    m <- toEither (readMaybe mt) $ "Could not parse month '" ++ (T.unpack mt) ++ "'"
    d <- toEither (readMaybe dt) $ "Could not parse day '" ++ (T.unpack dt) ++ "'"
    return $ if m == 1 then Date (year endDate) m d else Date (year startDate) m d

parseStatementSummary :: TextSpan -> Either String (Date, Date)
parseStatementSummary ss = do
    let ts = T.splitOn " " (sText ss)
    if length ts == 7 then pure () else Left $ "Unexpected number of arguments in statement summary header '" ++ (T.unpack . sText $ ss) ++ "'"
    let (sm : sd : sy : th : em : ed : ey : []) = ts
    if th == "through" then pure () else Left $ "Unexpected '" ++ (T.unpack th) ++ "' in statement summary header '" ++ (T.unpack . sText $ ss) ++ "'"
    sd <- parseStatementDate sy sm sd
    ed <- parseStatementDate ey em ed
    return (sd, ed)

isTableHeader :: [TextSpan] -> Bool
isTableHeader ts = if length ts /= 6 
                   then False 
                   else let (TextSpan _ dt : TextSpan _ pd : TextSpan _ ds : TextSpan _ dp : TextSpan _ wd : TextSpan _ db : []) = ts
                        in dt == "Date" && pd == "Post Date" && ds == "Description" && dp == "Deposits" && wd == "Withdrawals" && db == "Daily Balance"

parseTransaction :: [TextSpan] {- line -}
                 -> (Date, Date) {- statement date range -}
                 -> Either String Transaction
parseTransaction cs dr = do
    if length cs == 6 then pure () else Left $ "Expected 6 arguments in row, but found " ++ (show . length $ cs)
    let (TextSpan _ dt : TextSpan _ pd : TextSpan _ dp : TextSpan _ de : TextSpan _ wd : TextSpan _ db : []) = cs
    date <- readShortDate dt dr
    postDate <- readShortDate pd dr
    amount <- if wd == " " then Deposit <$> readDollars de else Withdrawal <$> readDollars wd
    dba <- readDollars db
    return $ Transaction "asdf" date postDate dp amount dba
                        
parseHeader :: [[TextSpan]] -> Either String Header
parseHeader as = Left "asdf" {- let ((_ : s : _) : ls) = dropWhile ((/= "STATEMENT SUMMARY") . _sText . head) as
                     (sm : sd : sy : _ : em : ed : ey : []) = T.splitOn " " (_sText s)
                     startDate = Date (T.decimal sy) 0 0
                 in r-}

flattenGlyph :: Span -> TextBox
flattenGlyph (Span gs _) = 
    let (Vector left top) = (glyphTopLeft . head) gs
        (Vector right bottom) = (glyphBottomRight . last) gs
        text = T.concat . catMaybes $ glyphText <$> gs
    in TextBox (floor left) (floor top) (floor bottom) (floor right) text
 
sortNodes :: TextBox -> TextBox -> Ordering
sortNodes (TextBox l1 t1 r1 b1 text1) (TextBox l2 t2 r2 b2 text2) = case compare t1 t2 of
    EQ -> compare l1 l2
    LT -> GT
    GT -> LT

lines' ts = 
    let sorted = sortBy sortNodes ts
        grouped = groupBy (\l r -> _top l == _top r) sorted
        fmaped = ((\(TextBox l _ _ _ t) -> TextSpan l t) <$>) <$> grouped
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
    writeFile "C:\\Users\\micah\\Desktop\\Pdf.txt" (groom ls)
    return ls