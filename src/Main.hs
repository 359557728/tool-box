{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM, forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Data.Time (Day, addDays, defaultTimeLocale, diffDays, formatTime, getCurrentTime, utctDay)
import Data.Time.Calendar (addGregorianMonthsClip)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, utctDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (hoursToTimeZone, localDay, utcToLocalTime)
import Database.Redis (Connection, PortID (..), connect, connectAuth, connectHost, connectPort, defaultConnectInfo, del, keys, runRedis)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Files (getFileStatus, modificationTime)
import System.Process
import Data.List (nub)

-- Function to generate a date string in "YYYY-MM-DD" format with UTC+8 timezone
generateDateString :: Integer -> IO String
generateDateString n = do
    currentTime <- getCurrentTime
    let utcPlus8 = hoursToTimeZone 8
    let localTime = utcToLocalTime utcPlus8 currentTime
    let targetDay = addDays n (localDay localTime)
    return $ formatTime defaultTimeLocale "%Y%m%d" targetDay

-- Function to create a directory if it doesn't exist
createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists path = do
    exists <- doesDirectoryExist path
    if not exists
        then createDirectoryIfMissing True path
        else putStrLn $ "Directory already exists: " ++ path

-- Get the date 60 days ago
getPastDate :: IO String
getPastDate = do
    currentTime <- getCurrentTime
    let pastTime = addDays (-60) (utctDay currentTime)
    return $ formatTime defaultTimeLocale "%Y-%m-%d" pastTime

-- Clear Redis keys with the specified prefix and date condition
cacheClear :: String -> IO ()
cacheClear prefix = do
    let redisHost = "101.254.96.163" -- Replace with your Redis server IP
    let redisPort = 6379 -- Replace with your Redis server port
    let redisPassword = Just "campSync" -- Replace with your Redis password
    let connInfo =
            defaultConnectInfo
                { connectHost = redisHost
                , connectPort = PortNumber (fromIntegral redisPort)
                , connectAuth = redisPassword
                }
    conn <- connect connInfo
    pastDate <- getPastDate
    let pattern = BS.pack $ prefix ++ "*"
    runRedis conn $ do
        Right keysList <- keys pattern
        let keysToDelete =
                filter
                    ( \k ->
                        let datePart = drop (length prefix) (BS.unpack k)
                         in datePart < pastDate
                    )
                    keysList
        forM_ keysToDelete $ \key -> do
            del [key]
            return ()

folderCreate :: String -> IO ()
folderCreate prefix = do
    forM_ [0 .. 2] $ \n -> do
        dateStr <- generateDateString n
        let fullPath = prefix ++ "/" ++ dateStr
        createDirectoryIfNotExists fullPath
        putStrLn $ "Checked or created directory: " ++ fullPath

isModifiedEarlierThan :: NominalDiffTime -> FilePath -> IO Bool
isModifiedEarlierThan days path = do
    status <- getFileStatus path
    let modTime = posixSecondsToUTCTime (realToFrac (modificationTime status))
    currentTime <- getCurrentTime
    return $ (currentTime `diffUTCTime` modTime) > days

removeOldDirectories :: FilePath -> NominalDiffTime -> IO ()
removeOldDirectories dir days = do
    contents <- listDirectory dir
    let fullPaths = map (dir </>) contents
    oldDirs <- filterM (isModifiedEarlierThan days) fullPaths
    forM_ oldDirs $ \oldDir -> do
        let rmCommand = "rm"
        let rmArgs = ["-rf", oldDir]
        (rmExitCode, _, rmStderr) <- readProcessWithExitCode rmCommand rmArgs ""
        case rmExitCode of
            ExitSuccess -> putStrLn $ "Removed: " ++ oldDir
            ExitFailure _ -> putStrLn $ "Failed to remove: " ++ oldDir ++ ". Error: " ++ rmStderr

folderRemove :: String -> IO ()
folderRemove prefix = do
    let days = 30 * 24 * 60 * 60
    srcExists <- doesDirectoryExist prefix
    if srcExists
        then removeOldDirectories prefix (fromIntegral days)
        else putStrLn $ "Source directory does not exist: " ++ prefix

monthsBetween :: String -> String -> [String]
monthsBetween startDate endDate =
    let parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"
        formatMonth = formatTime defaultTimeLocale "%Y-%m"
        start = parseDate startDate
        end = parseDate endDate
        go current
            | current > end = []
            | otherwise = formatMonth current : go (addGregorianMonthsClip 1 current)
     in go start

firstDayOfPreviousQuarter :: IO String
firstDayOfPreviousQuarter = do
    today <- getCurrentTime >>= pure . utctDay
    let (currentYear, currentMonth, _) = toGregorian today
        (previousQuarterYear, previousQuarterMonth)
            | currentMonth <= 3 = (currentYear - 1, 10)
            | currentMonth <= 6 = (currentYear, 1)
            | currentMonth <= 9 = (currentYear, 4)
            | otherwise = (currentYear, 7)
        yearString = show previousQuarterYear
        monthString = if previousQuarterMonth < 10 then "0" ++ show previousQuarterMonth else show previousQuarterMonth
    return $ yearString ++ "-" ++ monthString ++ "-01"

monthsSinceLastQuarter :: IO [String]
monthsSinceLastQuarter = do
    startDate <- firstDayOfPreviousQuarter
    today <- getCurrentTime >>= pure . formatTime defaultTimeLocale "%Y-%m-%d"
    return $ monthsBetween startDate today

monthToQuarter :: String -> Integer
monthToQuarter month =
    let (year, monthStr) = splitAt 4 month
        monthInt = read (dropWhile (== '-') monthStr) :: Int
        quarter = (monthInt - 1) `div` 3 + 1
    in read year * 10 + toInteger quarter

monthsToQuarters :: [String] -> [Integer]
monthsToQuarters monthStrs = nub $ map monthToQuarter monthStrs

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["cache-clear", prefix] -> cacheClear prefix
        ["folder-create", prefix] -> folderCreate prefix
        ["folder-remove", prefix] -> folderRemove prefix
        ["month-between", start, end] -> mapM_ putStrLn (monthsBetween start end)
        ["month-since-last-quarter"] -> monthsSinceLastQuarter >>= mapM_ putStrLn
        ["quarter-since-last-quarter"] -> monthsSinceLastQuarter >>= return . monthsToQuarters >>= mapM_ print
        _ -> putStrLn "Not Supported Command"
