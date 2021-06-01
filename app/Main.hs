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

graphLogString :: IO String
graphLogString = do
  s <- S.readFile logFile
  let d = DL.reverse . take 80 . DL.reverse $ fmap round (read <$> lines s :: [Double]) :: [Integer]
      result = unlines $ plotWithString options' d
  return result

graphQueryString :: IO String
graphQueryString = do
  --let d = DL.reverse . take 80 . DL.reverse $ fmap round (read <$> lines s :: [Double]) :: [Integer]
  let d = candles (ProductId $ pack "BTC-USD") (Just fromDate) (Just toDate) Day
  cs <- run Sandbox d
  let ps = (round  . unPrice . low <$> cs) :: [Integer]
      result = unlines $ plotWithString options' ps
  return result

fromDate = (read "2021-01-01 00:00:00 UTC")::UTCTime
toDate =   (read "2021-03-01 00:00:00 UTC")::UTCTime

testCBP :: IO ()
testCBP = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logPrice >> graphLogString >>= (\s ->  M.defaultMain theApp initialState)

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

newtype TickerEvent = Ticker String

tickerThread :: BC.BChan TickerEvent -> IO ()
tickerThread chan = do
  --ls <- return ["suka1", "nah"]
  s <- graphQueryString
  threadDelay 1000000
  BC.writeBChan chan $ Main.Ticker s

handleEvent :: AppState -> T.BrickEvent () TickerEvent -> T.EventM () (T.Next AppState)
handleEvent app@(AppState s _) (T.AppEvent (Main.Ticker l)) =
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
    [ (L.listAttr,            V.green `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.green)
    , (customAttr,            fg V.red)
    ]

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

theApp :: M.App AppState TickerEvent ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
  eventChan  <- BC.newBChan 10
  forkIO $ forever $ do
    tickerThread eventChan
  
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  let runState = M.customMain initialVty buildVty
                (Just eventChan) theApp initialState
  void runState
