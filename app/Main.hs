{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lens.Micro ((^.))
import Control.Monad ( void, forever )
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Brick ( App(..)
             , withBorderStyle
             , padRight, padLeft, padTop, padAll, Padding(..))
import qualified Brick.BChan as BC       
import qualified Brick.Main  as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.List             as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap        as A
import qualified Data.Vector          as Vec
import Brick.Types ( Widget )
import Brick.Widgets.Core ( (<+>), str, vLimit, hLimit, vBox, withAttr )
import Brick.Util (fg, on)
import Text.RawString.QQ

import Data.Aeson
import Data.Aeson.Types (parse)
import Data.Text        (pack)
import Network.HTTP.Conduit
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L8
import Net.Stocks
import qualified Net.IEX.TimeSeries         as IEXTimeSeries
-- import Control.Monad.Writer
-- import Control.Monad.State
import System.IO
import System.IO.Strict                     as S
import System.Directory
import Data.List                            as DL
import qualified System.IO.Streams                 as Streams
import           Net.CoinbasePro.Environment       (Environment (..))
import           Net.CoinbasePro.Types ( ProductId(..), Price(..), CandleGranularity(..) )
import           Net.CoinbasePro.WebSocketFeed         (subscribeToFeed)
import           Net.CoinbasePro.WebSocketFeed.Request (ChannelName (..))
import           Net.CoinbasePro.WebSocketFeed.Channel (ChannelMessage (..))
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Match    as M
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Open     as O
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Done     as D
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Received as R

import           Net.CoinbasePro.Unauthenticated
import           Net.CoinbasePro.Request
import           Net.CoinbasePro.Environment
import           Net.CoinbasePro.Types

import Data.Time.Clock
import Control.Concurrent
import Data.Text.Chart as C

import Debug.Trace    as DT

type Name = ()

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- COINBASE PRO ----------------------------------------------------------------------------------------------------------------------------------------------

logFile = ".log" :: FilePath

logPrice :: Maybe String -> IO String
logPrice Nothing  = do
  S.readFile logFile
logPrice (Just s) = do
  h <- openFile logFile AppendMode
  hPutStrLn h s
  hClose h

  S.readFile logFile

readPrice :: Maybe ChannelMessage -> IO (Maybe String)
readPrice msg =
  case msg of
    Just (MatchMessage x) -> return $ Just (show $ unPrice $ M.price x)
    Just (OpenMessage  x) -> return $ Just (show $ unPrice $ O.price x)
    Just (ReceivedMessage r) -> return $ rPrice r
    Just (DoneMessage     d) -> return $ dPrice d
    _ -> return Nothing
    where
      dPrice d =
        case D.price d of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing

      rPrice r =
        case R.price r of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing

logPriceCB :: IO ()
logPriceCB = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logPrice

options' :: C.Options
options' =
  C.Options { height = 14 }

graphString :: IO String
graphString = do
  s <- S.readFile logFile
  let d = DL.reverse . take 80 . DL.reverse $ fmap round (read <$> lines s :: [Double]) :: [Integer]
      result = unlines $ plotWithString options' d
  return result

graphString' :: IO String
graphString' = do
  --let d = DL.reverse . take 80 . DL.reverse $ fmap round (read <$> lines s :: [Double]) :: [Integer]
  let d = candles (ProductId $ pack "BTC-USD") (Just fromDate) (Just toDate) Day
  cs <- run Sandbox d
  let ps = (round  . unPrice . low <$> cs) :: [Integer]
      result = unlines $ plotWithString options' ps
  return result
  

testCBP :: IO ()
testCBP = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logPrice >> graphString >>= (\s ->  M.defaultMain theApp initialState)

---------------------------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- IEX CLOUD -------------------------------------------------------------------------------------------------------------------------------------------------

type AuthAndSymbol = (String, Symbol)
type Symbol = String

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query = try $ simpleHttp query

parms = QSParms 0 0
symb  = "AAPL"
token = "Tpk_8d1fbeccf06745019a98635b05346b90"
query = "https://sandbox.iexapis.com/stable/stock/aapl/quote?token=Tpk_8d1fbeccf06745019a98635b05346b90"
        -- "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2010-01-01&interval=2&format=csv?token=Tpk_8d1fbeccf06745019a98635b05346b90"
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/previous/quote?token=Tpk_8d1fbeccf06745019a98635b05346b90" -- previous day range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/3m?token=Tpk_8d1fbeccf06745019a98635b05346b90"          -- 3 months range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/1m?token=Tpk_8d1fbeccf06745019a98635b05346b90"          -- 1 month  range
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/7d?token=Tpk_8d1fbeccf06745019a98635b05346b90"          -- 7 days
        -- "https://sandbox.iexapis.com/stable/stock/AAPL/chart/1d?token=Tpk_8d1fbeccf06745019a98635b05346b90"          -- 1 day per minute
        -- https://iexcloud.io/docs/api/#historical-prices

        -- "https://sandbox.iexapis.com/stable/stock/AAPL/intraday-prices?token=Tpk_8d1fbeccf06745019a98635b05346b90"   -- intraday prices
        -- https://iexcloud.io/docs/api/#intraday-prices -- API docs.
-- TODO: historic prices rest call   type
-- TODO: hustoric prices rest return type
-- TODO: same for intraday prices: intraday type call
--                                 intraday type return

testIEX :: IO ()
testIEX = do
  obj <- getNonJSONData "https://sandbox.iexapis.com/stable/stock/AAPL/quote?token=Tpk_8d1fbeccf06745019a98635b05346b90"
  let obj'        = fromJust ( fromMaybe mempty $ decode ((\(Right x)->x) obj) :: Maybe Object)
      latestPrice = parse (obj' .:) (pack "latestPrice") :: Result Double
      symbol      = parse (obj' .:) (pack "symbol")      :: Result String

  let symbol' =
        case symbol of
          Success x -> x
          Error   e -> e
  let latestPrice' =
        case latestPrice of
          Success x -> x
          Error   _ -> (-1)

  putStrLn $ symbol' ++ " : " ++ show latestPrice'
  return ()

-- TODO : do similar to CoinbasePro logPrice
logPriceIEX :: IO ()
logPriceIEX = undefined

---------------------------------------------------------------------------------------------------------------------------------------------------------------


fromJust :: Monoid a => Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = mempty

test0Output :: String
test0Output = tail [r|
6.00 ┤    ╭─╮
5.00 ┤   ╭╯ ╰╮
4.00 ┤  ╭╯   ╰╮
3.00 ┤ ╭╯     ╰╮
2.00 ┤╭╯       ╰╮
1.00 ┼╯         ╰
|]

test1Output :: String
test1Output = tail [r|
 5.00 ┤          ╭────────╮
 3.75 ┤       ╭──╯        ╰──╮
 2.50 ┤  ╭────╯              ╰────╮
 1.25 ┤╭─╯                        ╰─╮
 0.00 ┼╯                            ╰╮                            ╭
-1.25 ┤                              ╰─╮                        ╭─╯
-2.50 ┤                                ╰────╮              ╭────╯
-3.75 ┤                                     ╰──╮        ╭──╯
-5.00 ┤                                        ╰────────╯
|]

test2Output :: String
test2Output = tail [r|
 5.00 ┤                        ╭────────╮
 3.75 ┤                     ╭──╯        ╰──╮
 2.50 ┤                ╭────╯              ╰────╮
 1.25 ┤              ╭─╯                        ╰─╮
 0.00 ┼              |                            ╰╮              
-1.25 ┤            ╭─╯                             ╰─╮            
-2.50 ┤       ╭────╯                                 ╰────╮       
-3.75 ┤    ╭──╯                                           ╰──╮    
-5.00 ┤────╯                                                 ╰─────
|]

--drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI :: AppState -> [Widget ()]
drawUI (AppState s l) = [ui]
    where
        --lbl = str "Item " <+> cur <+> str " of " <+> total
        lbl = cur
        cur
          = case l^.(L.listSelectedL) of
              Nothing -> str "-"
              Just i -> str $ (listElements l)Vec.!i --str (show (i + 1))
        -- total = str $ show $ Vec.length $ l^.(L.listElementsL)
        -- sel
        --   = case l^.(L.listSelectedL) of
        --       Nothing -> 0
        --       Just i  -> i
        -- fs =
        --   [ test1Output
        --   , test2Output
        --   , test3Output]
        graph
          = B.borderWithLabel lbl $
            hLimit 80 $
            vLimit 15 $
            --str test1Output
            --str $ fs!!(sel `mod` length fs)
            str s

        box
          = B.borderWithLabel lbl $
            hLimit 69 $
            vLimit 15 $
            L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [
                                C.hCenter graph
                              , C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

newtype LogEvent = Log String
-- newtype TickerEvent = Ticker String

-- TODO: attempting writing a logger event
-- logEvent :: s -> T.BrickEvent n LogEvent -> T.EventM n (T.Next s)
-- logEvent s (T.AppEvent (Log ls)) = undefined

logThread :: BC.BChan LogEvent -> IO ()
logThread chan = do
  --ls <- return ["suka1", "nah"]
  s <- graphString'
  threadDelay 1000000
  BC.writeBChan chan $ Log s

-- testEager :: IO String
-- testEager = do
--   handle   <- openFile logFile ReadMode
--   h <- openFile logFile ReadMode
--   c <- System.IO.hGetContents h
--   seq c ()
--   hClose h
--   return c

--handleEvent :: AppState -> T.BrickEvent n LogEvent -> T.EventM n (T.Next AppState)
handleEvent :: AppState -> T.BrickEvent () LogEvent -> T.EventM () (T.Next AppState)
handleEvent app@(AppState s _) (T.AppEvent (Log l)) =
  M.continue $ app
  { header = l }
handleEvent app@(AppState _ l) (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar '+') [] ->
      let el  = nextElement (L.listElements l)
          pos = Vec.length $ l^.L.listElementsL
      in M.continue $ app { appList = L.listInsert pos el l }

    V.EvKey (V.KChar '-') [] ->
      case l^.L.listSelectedL of
        Nothing -> M.continue $ app { appList = l }
        Just i  -> M.continue $ app { appList = L.listRemove i l }

    V.EvKey V.KEsc [] -> M.halt $ app { appList = l }

    ev -> M.continue =<< (\l' -> return app { appList = l'}) =<< L.handleListEvent ev l
    where

      nextElement :: Vec.Vector String -> String
      nextElement v = fromMaybe "?" $ Vec.find (`Vec.notElem` v) (Vec.fromList ["sin", "cos", "tan", "ctan", "atan"])

handleEvent s _ =
  M.continue s

appEvent :: L.List () String -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () String))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = nextElement (L.listElements l)
                pos = Vec.length $ l^.L.listElementsL
            in M.continue $ L.listInsert pos el l

        V.EvKey (V.KChar '-') [] ->
            case l^.L.listSelectedL of
                Nothing -> M.continue l
                Just i -> M.continue $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< L.handleListEvent ev l
    where

      nextElement :: Vec.Vector String -> String
      nextElement v = fromMaybe "?" $ Vec.find (`Vec.notElem` v) (Vec.fromList ["sin", "cos", "tan", "ctan", "atan"])
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = str s -- if sel
                   -- then withAttr customAttr (str $ "<" <> s <> ">")
                   -- else str s
    in C.hCenter $ str "Item " <+> selStr (show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

sysfg :: V.Color
sysfg = V.rgbColor 00 99 00

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            sysfg `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.green)
    , (customAttr,            fg V.red)
    ]
    -- [ (L.listAttr,            V.white `on` V.black)
    -- , (L.listSelectedAttr,    V.black `on` V.white)
    -- , (customAttr,            fg V.red)
    -- ]

-- type AppState = (L.List () String)
data AppState =
     AppState
     {
       header  :: String
     , appList :: (L.List () String)
     }

initialState :: AppState
initialState =
  AppState "initialState" initialList

initialList :: L.List () String
initialList = L.list () (Vec.fromList
                          [ "Bitcoin"
                          , "Ethereum"
                          , "Litecoin"
                          , "DogeCoin"
                          ]) 1

--theApp :: String -> M.App (L.List () String) e ()
theApp :: M.App AppState LogEvent ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          --, M.appHandleEvent = appEvent
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

fromDate = (read "2021-01-01 00:00:00 UTC")::UTCTime
toDate =   (read "2021-03-01 00:00:00 UTC")::UTCTime

main' :: IO ()
main' = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ do
    threadDelay 1000000
    Streams.read msgs >>= readPrice >>= logPrice
    graphString >>= (\s ->  M.defaultMain theApp initialState)
    putStrLn "Hi!"
  -- msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  -- forever $ Streams.read msgs >>= readPrice >>= logPrice >> graphString >>= (\s ->  M.defaultMain (theApp s) initialState) >> threadDelay 100

main1 :: IO ()
main1 = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logPrice >> graphString >>= (\s ->  M.defaultMain theApp initialState)

main2 :: IO ()
main2 = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  void $ Streams.read msgs >>= readPrice >>= logPrice >> graphString' >>= (\s ->  M.defaultMain theApp initialState)

main :: IO ()
main = do
  eventChan  <- BC.newBChan 10
  forkIO $ forever $ do
    logThread eventChan
  
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let finalState = M.customMain initialVty buildVty
                (Just eventChan) theApp initialState
  void finalState

