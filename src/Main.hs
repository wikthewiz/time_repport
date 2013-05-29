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
                isArgsOk args = do length args == 1

toStr curDate ls 
        | ls == [] = ""
        | curDate == d = entry ++ toStr curDate (tail ls)
        | curDate /= d = "\n" ++ d ++ ":" ++ entry ++ toStr d (tail ls)
        where
                entry = "\n\t" ++ second(head ls) ++ ": " ++(formatDiff (third (head ls)) (forth (head ls))) 
                d = first (head ls) 
               

sortTheList theList = [(first (head typesPerDate), second (head typesPerDate), sumDiff typesPerDate ,sumH typesPerDate ) | dates <- map (groupBy desc) $ groupBy date $ sortBy dateAndDesc theList , typesPerDate <- dates ]
        where 
                dateAndDesc (d1,t1,_,_) (d2,t2,_,_)
                        | d1 < d2 = LT
                        | d1 > d1 = GT
                        | otherwise = compare t1 t2
                sumDiff xs = if length xs == 1 then third (head xs) else  third (head xs) + sumDiff (tail xs)
                sumH xs = if null xs then 0 else forth (head xs) + sumH (tail xs)
                date (d1,_,_,_) (d2,_,_,_) = d1 == d2
                desc (_,t1,_,_) (_,t2,_,_) = t1 == t2


first (d,_,_,_) = d
second (_,t,_,_) = t
third (_,_,i,_) = i
forth (_,_,_,f) = f
                
calcDiffInFile :: FilePath -> IO ()
calcDiffInFile f = do   contents <- System.IO.readFile f
                        let raw = tail $ filter noComentsOrBlanks $ lines contents
                        System.IO.UTF8.putStrLn (toStr "" $ sortTheList $ map calcTimeDiff raw)
                        return ()
                    where
                        noComentsOrBlanks :: String -> Bool
                        noComentsOrBlanks str = not (startswith "#" str) && 
                                length (splitOn "," str) == 6 && 
                                not (null (strip str))
                                
calcTimeDiff :: String -> (String, String, NominalDiffTime, Float)
calcTimeDiff str = (date', desc', diff, numberDiff) 
               where 
               [date', desc', start', stop', remove_time,_] = map strip $ splitOn "," str
               start = parseCalenderTime date' start'
               stop = parseCalenderTime date' stop'
               timeNotWork = secondsToDiffTime $ parseTimeNotWorked remove_time 
               diff = diffUTCTime (subtract' stop timeNotWork) start 
               numberDiff = getTimeDiff diff
               subtract' (UTCTime day time) seconds = UTCTime day (time - seconds)
               parseCalenderTime date time = readTime defaultTimeLocale "%Y-%m-%d %H:%M" (date ++ " " ++ time) :: UTCTime

parseTimeNotWorked str = h * 3600 + m * 60
        where 
               [_, time'] = if not (null (strip str)) then map strip $ splitOn ":" str else ["",""]
               h = if isInfixOf "h" time' then parseHour time' else 0
               m = if isInfixOf "min" time' then parseMin time' else 0
               parseMin mStr = parsePart mStr "min"
               parseHour hStr = parsePart hStr "h"
               parsePart txt splitter = read i :: Integer
                        where 
                                [i,_] = map strip $ splitOn splitter txt

calcListToStr::(String, String, NominalDiffTime, Float) -> String
calcListToStr (a,b,c,d) = a ++ ", " ++ b ++ ", " ++ formatDiff c d

formatDiff diff number= timeDiff ++ "(" ++ timeCalc  ++ ")"
        where 
        timeDiff = formatTime defaultTimeLocale "%H:%M" (posixSecondsToUTCTime diff)
        timeCalc = printf "%.2f" number

getTimeDiff diff = h' + (m' / 60 :: Float) 
        where
        m' = read (formatTime defaultTimeLocale "%M" (posixSecondsToUTCTime diff)) :: Float
        h' = read (formatTime defaultTimeLocale "%H" (posixSecondsToUTCTime diff))::Float
         
-- :main C:\Users\mwikland\Documents\time_report
