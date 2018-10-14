{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad
import System.IO

import Data.Decimal
import Pdf.Document
import Pdf.Document.Internal.Types
import Pdf.Document.PageNode
import Pdf.Content.Processor

import Data.Bifunctor
import Data.Int
import Data.List (sortBy, groupBy)
import Data.Text (Text)
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
                 } deriving (Eq, Ord)

instance Show Date where
    show (Date y m d) = (show y) ++ "." ++ (show m) ++ "." ++ (show d) 

type Money = Decimal

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
    { beginningBalance :: Money
    , endingBalance :: Money
    , depositsCount :: Int
    , depositsSum :: Money
    , withdrawalsCount :: Int
    , withdrawalsSum :: Money
    } deriving (Show)

data Amount = Deposit Money
            | Withdrawal Money
            deriving (Show)

data Transaction = Transaction
    { id :: Text
    , date :: Date
    , postDate :: Date
    , description :: Text
    , amount :: Amount
    , balance :: Money
    } deriving (Show)

{- Relatively simple validation to make sure the statement is being processed correctly -}
validateStatement :: Statement -> Maybe String
validateStatement s
    | fromDate (header s) >= toDate (header s) = Just $ "fromDate " ++ show (fromDate (header s)) ++ " should be before toDate " ++ show (toDate (header s))
    | depositsCount (summary s) /= length deposits = Just $ "Expected " ++ show (depositsCount (summary s)) ++ " deposits, found " ++ show (length deposits)
    | depositsSum (summary s) /= foldr (+) 0.0 deposits = Just $ "Expected " ++ show (depositsSum (summary s)) ++ " of deposits, found " ++ show (foldr (+) 0.0 deposits) 
    | withdrawalsCount (summary s) /= length withdrawals = Just $ "Expected " ++ show (withdrawalsCount (summary s)) ++ " withdrawals, found " ++ show (length withdrawals)
    | withdrawalsSum (summary s) /= foldr (+) 0.0 withdrawals = Just $ "Expected " ++ show (withdrawalsSum (summary s)) ++ " of withdrawls, found " ++ show (foldr (+) 0.0 deposits)
    | endingBalance (summary s) /= (balance . last . transactions) s = Just $ "Expected ending balance of " ++ show (endingBalance (summary s)) ++ " , found " ++ show ((balance . last . transactions) s)
    | otherwise = case foldl (flip mapper) (Right $ beginningBalance . summary $ s) (transactions s) of
                       Left s -> Just s
                       _ -> Nothing
    where deposits = [ x | (Deposit x) <- amount <$> transactions s]
          withdrawals = [ x | (Withdrawal x) <- amount <$> transactions s]

          flattenAmount (Deposit x) = x
          flattenAmount (Withdrawal x) = -x

          mapper :: Transaction -> Either String Money -> Either String Money
          mapper _ (Left s) = Left s
          mapper t (Right a) = if a + flattenAmount (amount t) == balance t
                                   then Right $ balance t
                                   else Left $ "In transaction '" ++ (show t) ++ "': Expected ending balance of " ++ show (a + flattenAmount (amount t)) ++ ", but found " ++ show (balance t)

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

readDollars :: Text -> Either String Money
readDollars t = do
    let ts = T.splitOn "." (T.dropWhile (== '$') t)
    if length ts == 2 then pure () else Left $ "Expected dot in amount '" ++ (T.unpack t) ++ "'"
    let (ds : cs : []) = ts
    dollars <- toEither (readMaybe (T.filter (/= ',') ds) :: Maybe Integer) $ "Could not parse '" ++ (T.unpack ds) ++ "'"
    cents <- toEither (readMaybe (T.filter (/= ',') cs) :: Maybe Integer) $ "Cound not parse '" ++ (T.unpack cs) ++ "'"
    return $ Decimal 2 (dollars * 100 + cents)

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

isTableHeader :: [Text] -> Bool
isTableHeader ts = case safeUnpack6 ts of
                        Right (dt, pd, ds, dp, wd, db) -> dt == "Date" && pd == "Post Date" && ds == "Description" && dp == "Deposits" && wd == "Withdrawals" && db == "Daily Balance"
                        _ -> False

safeUnpack2 :: (Show a) => [a] -> Either String (a, a)
safeUnpack2 (a1 : a2 : []) = Right (a1, a2)
safeUnpack2 as = Left $ "Expected 2 elements, but found " ++ (show as)

safeUnpack3 :: (Show a) => [a] -> Either String (a, a, a)
safeUnpack3 (a1 : a2 : a3 : []) = Right (a1, a2, a3)
safeUnpack3 as = Left $ "Expected 3 elements, but found '" ++ (show as)

safeUnpack4 :: (Show a) => [a] -> Either String (a, a, a, a)
safeUnpack4 (a1 : a2 : a3 : a4 : []) = Right (a1, a2, a3, a4)
safeUnpack4 as = Left $ "Expected 4 elements, but found '" ++ (show as)

safeUnpack5 :: (Show a) => [a] -> Either String (a, a, a, a, a)
safeUnpack5 (a1 : a2 : a3 : a4 : a5 : []) = Right (a1, a2, a3, a4, a5)
safeUnpack5 as = Left $ "Expected 5 elements, but found '" ++ (show as)

safeUnpack6 :: (Show a) => [a] -> Either String (a, a, a, a, a, a)
safeUnpack6 (a1 : a2 : a3 : a4 : a5 : a6 : []) = Right (a1, a2, a3, a4, a5, a6)
safeUnpack6 as = Left $ "Expected 6 elements, but found '" ++ (show as)

safeUnpack7 :: (Show a) => [a] -> Either String (a, a, a, a, a, a, a)
safeUnpack7 (a1 : a2 : a3 : a4 : a5 : a6 : a7 : []) = Right (a1, a2, a3, a4, a5, a6, a7)
safeUnpack7 as = Left $ "Expected 7 elements, but found '" ++ (show as)

isSummaryTableHeader :: [Text] -> Bool
isSummaryTableHeader ts = case safeUnpack4 ts of
                               Right (accDesc, accNum, beg, end) -> accDesc == "Account Description" 
                                                                 && accNum == "Account #" 
                                                                 && beg == "Beginning" 
                                                                 && end == "Ending"
                               Left _ -> False

parseHeader :: [[Text]] -> Either String (Header, [[Text]])
parseHeader as = do
    let (l1 : l1s) = as
    (ss, ds) <- safeUnpack2 l1
    if ss /= "STATEMENT SUMMARY" then Left "Expected 'STATEMENT SUMMARY'" else pure ()
    (sm, sd, sy, th, em, ed, ey) <- safeUnpack7 $ T.splitOn " " ds
    startDate <- parseStatementDate sy sm sd
    if th /= "through" then Left $ "Unexpected date format '" ++ (T.unpack ds) ++ "'" else pure ()
    endDate <- parseStatementDate ey em ed
    let (l2 : l2s) = l1s
    if isSummaryTableHeader l2 then pure () else Left $ "Unrecognized summary table header '" ++ (show l2) ++ "'"
    let (l3 : l3s) = l2s
    let (l4 : l4s) = l3s
    let (l5 : l5s) = l4s
    (accDesc, accNum, _, _) <- safeUnpack4 l5
    return (Header startDate endDate accDesc accNum, l4s)

parseTotal :: Text {- Label to match -}
           -> [Text]
           -> Either String (Int, Money)
parseTotal label ts = do
    (label, value) <- safeUnpack2 ts
    if label == label then pure () else Left $ "Expected first item to be '" ++ (T.unpack label) ++ "'"
    (count, for, sum) <- safeUnpack3 (T.splitOn " " value)
    depositCount' <- toEither (readMaybe count :: Maybe Int) $ "Expected '" ++ (T.unpack label) ++ " count' to be an integer"
    if for == "for" then pure () else Left $ "Expected 'for'"
    sumCount' <- readDollars sum
    return (depositCount', sumCount')

parseSummary :: [[Text]] -> Either String (Summary, [[Text]])
parseSummary ts = do
    let (l1 : l1s) = ts
    if last l1 == "Account Detail" then pure () else Left $ "Expected line to contain 'Account Detail'"
    let (l2 : l2s) = l1s
    beginningBalance <- first ("While parsing beginning balance: " ++) $ do
        (balanceLabel, balance, _, _) <- safeUnpack4 l2
        if balanceLabel == "Beginning Balance" then pure () else Left $ "Expected first item to be 'Beginning Balance'"
        readDollars balance
    let (l3 : l3s) = l2s
    (depositCount', depositSum') <- first ("While parsing total deposits: " ++) (parseTotal "Total Deposits" l3)
    let (l4 : l4s) = l3s
    (withdrawalCount', withdrawalSum') <- first ("While parsing total withdrawals: " ++) (parseTotal "Total Withdrawals" l4)
    let (l5 : l5s) = l4s
    endingBalance <- first ("While parsing ending balance: " ++) $ do
        (label, balance) <- safeUnpack2 l5
        if label == "Ending Balance" then pure () else Left $ "Expected first item to be 'Ending Balance'"
        readDollars balance
    return (Summary beginningBalance endingBalance depositCount' depositSum' withdrawalCount' withdrawalSum', l5s)

parseTransaction :: [Text] {- Transaction IDs -}
                 -> [Text] {- Transaction Details -}
                 -> (Date, Date) {- Statement Date Range -}
                 -> Either String Transaction
parseTransaction ids dts dr = do
    (id1, id2) <- safeUnpack2 ids
    tid <- if id1 == id2 then pure id1 
                         else Left $ "Expected 2 identical transaction ids, but found '" ++ (T.unpack id1) ++ "' and '" ++ (T.unpack id2) ++ "'"
    (date, postDate, desc, deposit, withdrawal, balance) <- safeUnpack6 dts
    date' <- readShortDate date dr
    postDate' <- readShortDate postDate dr
    amount' <- if withdrawal == " " then Deposit <$> readDollars deposit else Withdrawal <$> readDollars withdrawal
    balance' <- readDollars balance
    return $ Transaction tid date' postDate' desc amount' balance'

stripTransaction :: (Date, Date) -> [[Text]] -> Either String (Transaction, [[Text]])
stripTransaction dr ts = do
    let (dts : ids : tss) = if length (head ts) == 1 then tail ts else ts
    t <- parseTransaction ids dts dr 
    return (t, tss)

parseAll :: (Date, Date) -> [[Text]] -> Either String [Transaction]
parseAll dr ts = if length ts > 1 && (head . head $ ts) /= "MONTHLY USAGE SUMMARY"
                 then do 
                    (t, tss) <- stripTransaction dr ts
                    (t:) <$> parseAll dr tss
                 else return []

parseTransactions :: (Date, Date) -> [[Text]] -> Either String [Transaction]
parseTransactions dr ts = do
    let table = dropWhile (not . isTableHeader) ts
    if length table == 0 
        then return []
        else do
            let rows = tail table
            let row1 = if (head . head $ rows) == "Beginning Balance" then tail rows else rows
            parseAll dr row1

parseDocument :: [[[Text]]] -> Either String Statement
parseDocument pgs = do
    (header, p1) <- case pgs of 
                         (p1 : _) -> let ls = dropWhile ((/= "STATEMENT SUMMARY") . head) p1
                                     in if length ls == 0 then Left "Could not find 'STATEMENT SUMMARY'" else parseHeader ls
                         _ -> Left "Expected at least one page"
    let p1' = dropWhile ((/= "Account Detail") . last) p1
    (summary, p1'') <- parseSummary p1' 
    ts <- sequence $ parseTransactions (fromDate header, toDate header) <$> pgs
    return $ Statement header summary (concat ts)

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
    
lines' :: [TextBox] -> [[Text]]
lines' ts = 
    let sorted = sortBy sortNodes ts
        grouped = groupBy (\l r -> _top l == _top r) sorted
        fmaped = ((\(TextBox l _ _ _ t) -> t) <$>) <$> grouped
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
    let ls = parseDocument $ lines' <$> pageNodes
    writeFile "C:\\Users\\micah\\Desktop\\Pdf.txt" (groom ls)
    return ls