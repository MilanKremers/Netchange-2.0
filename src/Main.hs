{-# language NamedFieldPuns #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

data Network = Network { u :: Int, n :: Int, dests :: [Int], neighU :: [Int], dU :: [(Int, Int)], nbU :: [(Int, Int)], ndisU :: [(Int, [(Int, Int)])]} deriving (Show)
data Message = MyDist ((Int, Int)) | Fail (Int) | Repair (Int)
data Mode = Table | SendMessage Int [String] | MakeConnection Int | Disconnect Int  deriving Show
data Config = Config {cfgMode :: !Mode} deriving Show

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours
  network <- newIORef (Network { u = me, n = 1, dests = [], neighU = [], dU = [], nbU = [], ndisU = [] })
  modifyIORef network (updateN (length neighbours))
  modifyIORef network (updateV (me : neighbours))
  modifyIORef network (updateNeighU neighbours)
  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ portToAddress me
  listen serverSocket 1024
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket
  
  -- Initialization of the netchange algorithm
  modifyIORef network (initialize)
  network'' <- readIORef network
  putStrLn $ "neighbours " ++ show (network'')
  
  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  {-case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbour : _ -> do
      putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
      client <- connectSocket neighbour
      chandle <- socketToHandle client ReadWriteMode
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.
      hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
      putStrLn "I sent a message to the neighbour"
      message <- hGetLine chandle
      putStrLn $ "Neighbour send a message back: " ++ show message
      threadDelay 100000-}

  args <- getLine
  let config = parseConfig args

  case cfgMode config of
    Table -> do
      undefined
    SendMessage portnumber bericht -> do
      putStrLn $ "Connecting to neighbour " ++ show portnumber  ++ "..."
      client <- connectSocket portnumber
      chandle <- socketToHandle client ReadWriteMode
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.
      hPutStrLn chandle $ show bericht
      putStrLn "I sent a message to the neighbour"
      message <- hGetLine chandle
      putStrLn $ "Neighbour send a message back: " ++ show message
      threadDelay 100000
    MakeConnection portnumber-> do
      undefined
    Disconnect portnumber -> do
      undefined
    

  {-case cfgMode config of
    Count -> do
      case cfgSync config of
        SyncIORef -> do
          counter <- newIORef 0
          bool <- newIORef True
          l <- return $ IORefLock (bool)
          countIORef config counter l
          threadDelay 10000
          result <- readIORef counter
          print result
        SyncMVar -> do
          counter <- newIORef 0
          bool <- newMVar True
          l <- return $ MVarLock (bool)
          countIORef config counter l
          threadDelay 10000
          result <- readIORef counter
          print result-}

  

  threadDelay 10000000000


-- Parses the command line arguments
parseConfig :: [String] -> Config
parseConfig (mode' : rest)
  = Config mode
  where
    -- Program mode
    mode = case (mode', rest) of
      ("R", []) -> Table
      ("C", (x:y)) -> SendMessage (read x) y
      ("B", [x]) -> MakeConnection (read x)
      ("D", [x]) -> Disconnect (read x)
      _ -> error "Illegal mode or wrong number of arguments"
parseConfig _ = error "Wrong number of arguments"

-- Initialization function for the IORef network
initialize :: Network -> Network
initialize network = network{ ndisU = initNDis (n network) (neighU network) (dests network), 
                              dU = initDU (u network) (n network) (dests network),
                              nbU = initNBU (u network) (dests network)}

initNDis :: Int -> [Int] -> [Int] -> [(Int, [(Int, Int)])]
initNDis _ [] _ = []
initNDis n (w:ws) vs = singleNeighbourNDis n w vs : initNDis n ws vs

singleNeighbourNDis :: Int -> Int -> [Int] -> (Int, [(Int, Int)])
singleNeighbourNDis n w vs = (w, singleNeighbourNDis' n vs)
  where singleNeighbourNDis' :: Int -> [Int] -> [(Int, Int)]
        singleNeighbourNDis' _ [] = []
        singleNeighbourNDis' n' (x:xs) = (x, n') : singleNeighbourNDis' n' xs 

initDU :: Int -> Int -> [Int] -> [(Int, Int)] 
initDU _ _ [] = []
initDU u n (v:vs) | u == v    = (v, 0) : initDU u n vs
                  | otherwise = (v, n) : initDU u n vs

initNBU :: Int -> [Int] -> [(Int, Int)] 
initNBU _  [] = []
initNBU u (v:vs) | u == v    = (v, 0) : initNBU u vs
                 | otherwise = (v, -1) : initNBU u vs

-- Update functions for the IORef network
updateN :: Int -> Network -> Network
updateN x network = network{ n = n network + x }

updateV :: [Int] -> Network -> Network
updateV x network = network{ dests = (dests network) ++ x }

updateNeighU :: [Int] -> Network -> Network
updateNeighU x network = network{ neighU = (neighU network) ++ x }

-- Functions provided in template
readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000
          connect'
        Right _ -> return client

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket
  _ <- forkIO $ handleConnection connection
  listenForConnections serverSocket

handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  -- vanaf hier zelf

  --vanaf hier weer template
  putStrLn $ "Incomming connection send a message: " ++ message
-- hierin gaan we netchange opnieuw aanroepen
  --hClose chandle -- dit alleen na een opdracht in de console
