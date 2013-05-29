module Main where


import System.Environment
import Data.List
import Data.List.Utils
import Data.List.Split
import Data.String.Utils
import System.IO.UTF8
import System.IO
import Data.Time.Format
import System.Locale
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.Printf

main::IO()
main = do 
        args <- getArgs
        if (isArgsOk args) 
                then calcDiffInFile $ head args
                else System.IO.putStrLn "Argument is wrong"
        return ()
        where
                isArgsOk args = length args == 1

calcDiffInFile :: FilePath -> IO ()
calcDiffInFile f = do   contents <- System.IO.readFile f
                        System.IO.UTF8.putStrLn (toStr $ calcDateTot $ mergeEqDesc $ map lineToRow $ fileToLines contents)

fileToLines::String -> [String]
fileToLines contents = tail $ filter noComentsOrBlanks $ lines contents
        where 
                noComentsOrBlanks str = not (startswith "#" str) && length (splitOn "," str) == 6 && not (null (strip str))
                
lineToRow :: String -> (String, String, NominalDiffTime, Float)
lineToRow str = (date', desc', diff, numberDiff) 
               where 
               [date', desc', start', stop', remove_time,_] = map strip $ splitOn "," str
               start = parseCalenderTime date' start'
               stop = parseCalenderTime date' stop'
               timeNotWork = secondsToDiffTime $ parseTimeNotWorked remove_time 
               diff = diffUTCTime (subtract' stop timeNotWork) start 
               numberDiff = getTimeDiff diff
               subtract' (UTCTime day time) seconds = UTCTime day (time - seconds)
               parseCalenderTime date time = readTime defaultTimeLocale "%Y-%m-%d %H:%M" (date ++ " " ++ time) :: UTCTime

parseTimeNotWorked :: String -> Integer
parseTimeNotWorked str = h * 3600 + m * 60
        where 
               [_, time'] = if not (null (strip str)) then map strip $ splitOn ":" str else ["",""]
               h = if "h" `isInfixOf` time' then parseHour time' else 0
               m = if "min" `isInfixOf` time' then parseMin time' else 0
               parseMin mStr = parsePart mStr "min"
               parseHour hStr = parsePart hStr "h"
               parsePart txt splitter = read i :: Integer
                        where 
                                [i,_] = map strip $ splitOn splitter txt

getTimeDiff :: POSIXTime -> Float
getTimeDiff diff = h' + (m' / 60 :: Float) 
        where
        m' = read (formatTime defaultTimeLocale "%M" (posixSecondsToUTCTime diff)) :: Float
        h' = read (formatTime defaultTimeLocale "%H" (posixSecondsToUTCTime diff))::Float

--mergeEqDesc ::(Num t, Num t3, Ord t1, Ord t2) => [(t1, t2, t3, t)] -> [(t1, t2, t3, t)]
mergeEqDesc ::[(String, String, NominalDiffTime, Float)] -> [(String, String, NominalDiffTime, Float)]
mergeEqDesc theList = [ newTuple typesPerDate | dates <- map (groupBy desc) groupByDates , typesPerDate <- dates ]
        where
                groupByDates = groupBy date $ sortBy dateAndDesc theList
                newTuple ls = (first (head ls), second (head ls), sumDiff ls ,sumH ls)
                sumDiff xs = if length xs == 1 then third (head xs) else  third (head xs) + sumDiff (tail xs)
                sumH xs = if null xs then 0 else forth (head xs) + sumH (tail xs)
                date (d1,_,_,_) (d2,_,_,_) = d1 == d2
                desc (_,t1,_,_) (_,t2,_,_) = t1 == t2


dateAndDesc :: Ord a => (a, String, t, t1) -> (a, String, t2, t3) -> Ordering
dateAndDesc (d1,t1,_,_) (d2,t2,_,_)
        | d1 < d2 = LT
        | d1 > d2 = GT
        | otherwise = totLastCompare t1 t2
        where 
                totLastCompare a1 a2
                        | a1 == totLable = LT
                        | a2 == totLable = LT
                        | otherwise = compare a1 a2
        
totLable :: String
totLable = "tot"

calcDateTot ::[(String, String, NominalDiffTime, Float)] -> [(String, String, NominalDiffTime, Float)]
calcDateTot ls = sortBy dateAndDesc (ls ++ [ newTotRow d' | d' <- destinctDates ])
        where
                destinctDates = [ d | (d,_,_,_) <- nubBy (\a1 a2 -> first a1 == first a2) ls]
                sumOverDate d = foldr (\(h1,v1) (h2,v2) -> (h1 + h2,v1 + v2)) (0,0) [ (dH,dV) | (d',_,dH,dV) <- ls , d == d']
                newTotRow date = (date, totLable, fst (sumOverDate date), snd (sumOverDate date) ) 
                
first   :: (t, t1, t2, t3) -> t
first   (d,_,_,_) = d
second  ::(t, t1, t2, t3) -> t1
second  (_,t,_,_) = t
third   ::(t, t1, t2, t3) -> t2
third   (_,_,i,_) = i
forth   :: (t, t1, t2, t3) -> t3
forth   (_,_,_,f) = f

toStr :: [(String, String, POSIXTime, Float)] -> String
toStr = toStr' ""
toStr' :: String -> [(String, String, NominalDiffTime, Float)] -> String
toStr' curDate ls 
        | null ls = ""
        | curDate == d = diff ++ toStr' curDate (tail ls)
        | curDate /= d = "\n" ++ d ++ ":" ++ diff ++ toStr' d (tail ls)
        | otherwise = ""
        where
                diff = "\n\t" ++ second(head ls) ++ ": " ++ formatDiff (third (head ls)) (forth (head ls)) 
                d = first (head ls) 

formatDiff :: POSIXTime -> Float -> String
formatDiff diff number= timeDiff ++ "(" ++ timeCalc  ++ ")"
        where 
        timeDiff = formatTime defaultTimeLocale "%H:%M" (posixSecondsToUTCTime diff)
        timeCalc = printf "%.2f" number               

-- :main C:\Users\mwikland\Documents\time_report
