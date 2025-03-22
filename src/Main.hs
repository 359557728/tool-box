{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM, forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Time
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Calendar (addGregorianMonthsClip, fromGregorian, toGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
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

-- Function to create a directory if it doesn't exist
createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists path = do
    exists <- doesDirectoryExist path
    if not exists
        then createDirectoryIfMissing True path
        else putStrLn $ "Directory already exists: " ++ path

-- Get the date 60 days ago
getPastDate :: IO String
getPastDate = formatTime defaultTimeLocale "%Y-%m-%d" . addDays (-60) <$> (localDay . utcToLocalTime (hoursToTimeZone 8) <$> getCurrentTime)

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
                        let datePart = drop (length prefix) (BS.unpack k) in datePart < pastDate
                    )
                    keysList
        forM_ keysToDelete $ \key -> do
            del [key]
            return ()

folderCreate :: String -> IO ()
folderCreate prefix = do
    forM_ [0 .. 2] $ \n -> do
        today <- localDay . utcToLocalTime (hoursToTimeZone 8) <$> getCurrentTime
        let date = formatTime defaultTimeLocale "%Y%m%d" $ addDays n today
        let fullPath = prefix ++ "/" ++ date
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

sixMonthsBefore :: IO [String]
sixMonthsBefore = do
    startDate <- formatTime defaultTimeLocale "%Y-%m-%d" . addGregorianMonthsClip (-6) <$> (localDay . utcToLocalTime (hoursToTimeZone 8) <$> getCurrentTime)
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

eachDayCurMonth :: IO [String]
eachDayCurMonth = do
    today <- localDay . utcToLocalTime (hoursToTimeZone 8) <$> getCurrentTime
    let (y, m, _) = toGregorian today
        firstDay = fromGregorian y m 1
        days = [firstDay .. today]
    return $ map (formatTime defaultTimeLocale "%Y-%m-%d") days

lastNDays :: String -> IO [String]
lastNDays n = do
    today <- localDay . utcToLocalTime (hoursToTimeZone 8) <$> getCurrentTime
    let days = [addDays (-i) today | i <- [ 0 .. fromIntegral ((read n) - 1) ]]
    return $ map (formatTime defaultTimeLocale "%Y-%m-%d") days

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["cache-clear", prefix] -> cacheClear prefix
        ["folder-create", prefix] -> folderCreate prefix
        ["folder-remove", prefix] -> folderRemove prefix
        ["month-between", start, end] -> mapM_ putStrLn (monthsBetween start end)
        ["month-since-last-quarter"] -> sixMonthsBefore >>= mapM_ putStrLn
        ["quarter-since-last-quarter"] -> sixMonthsBefore >>= return . monthsToQuarters >>= mapM_ print
        ["each-day-cur-month"] -> eachDayCurMonth >>= mapM_ putStrLn
        ["last-n-days", n] -> lastNDays n >>= mapM_ putStrLn
        _ -> putStrLn "Not Supported Command"
