{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lens.Micro ((^.))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V
import Brick ( App(..)
             , withBorderStyle
             , padRight, padLeft, padTop, padAll, Padding(..))
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
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
import Data.List

import           Control.Monad                     (forever)
import qualified System.IO.Streams                 as Streams
import           Net.CoinbasePro.Environment       (Environment (..))
import           Net.CoinbasePro.Types             (ProductId (..))
import           Net.CoinbasePro.WebSocketFeed         (subscribeToFeed)
import           Net.CoinbasePro.WebSocketFeed.Request (ChannelName (..))
import           Net.CoinbasePro.WebSocketFeed.Channel (ChannelMessage (..))
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Match    as M
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Open     as O
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Done     as D
import           Net.CoinbasePro.WebSocketFeed.Channel.Full.Received as R
import           Net.CoinbasePro.Types (Price (..))

import Debug.Trace    as DT

type Name = ()

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- COINBASE PRO ----------------------------------------------------------------------------------------------------------------------------------------------

logFile = ".log" :: FilePath

logger :: Maybe String -> IO String
logger Nothing  = do
  s <- S.readFile logFile
  return s
logger (Just s) = do
  h <- openFile logFile AppendMode
  hPutStrLn h s
  hClose h

  s <- S.readFile logFile
  return s

readPrice :: Maybe ChannelMessage -> IO (Maybe String)
readPrice msg = do
  case msg of
    Just (MatchMessage x) -> return $ Just (show $ unPrice $ M.price x)
    Just (OpenMessage  x) -> return $ Just (show $ unPrice $ O.price x)
    Just (ReceivedMessage r) -> return $ rPrice r
    Just (DoneMessage     d) -> return $ dPrice d
    _ -> return Nothing
    where
      dPrice d =
        case (D.price d) of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing
      
      rPrice r =
        case (R.price r) of
          Just p -> Just (show $ unPrice p)
          Nothing -> Nothing
      
logPrice :: IO ()
logPrice = do
  msgs <- subscribeToFeed [ProductId "BTC-USD"] [Full] Sandbox Nothing
  forever $ Streams.read msgs >>= readPrice >>= logger

-- TODO: graphPrice:
graphPrice :: FilePath -> IO ()
graphPrice f = undefined

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
      latestPrice = parse ((.:) obj') (pack "latestPrice") :: Result Double
      -- latestPrice1 = (\ (Success x) -> x ) (parse ((.:) obj') (pack "latestPrice") :: Result Double) :: Double
      symbol      = parse ((.:) obj') (pack "symbol")      :: Result String

  let symbol' =
        case symbol of
          Success x -> x
          Error   e -> e
  let latestPrice' =
        case latestPrice of
          Success x -> x
          Error   _ -> (-1)

  putStrLn $ symbol' ++ " : " ++ (show latestPrice')
  return ()

-- TODO : do similar to CoinbasePro logPrice
logPrice' :: IO ()
logPrice' = undefined

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
drawUI :: L.List () String -> [Widget ()]
drawUI l = [ui]
    where
        --lbl = str "Item " <+> cur <+> str " of " <+> total
        lbl = cur
        cur
          = case l^.(L.listSelectedL) of
              Nothing -> str "-"
              Just i -> str $ (listElements l)Vec.!i --str (show (i + 1))
        -- total = str $ show $ Vec.length $ l^.(L.listElementsL)
        sel
          = case l^.(L.listSelectedL) of
              Nothing -> 0
              Just i  -> i
        fs =
          [ test1Output
          , test2Output ]
        graph
          = B.borderWithLabel lbl $
            hLimit 80 $
            vLimit 15 $
            --str test1Output
            str $ fs!!(sel `mod` length fs)

        box
          = B.borderWithLabel lbl $
            hLimit 67 $
            vLimit 15 $
            L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [
                                C.hCenter graph
                              , C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: L.List () String -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () String))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = nextElement (L.listElements l)
                pos = Vec.length $ l^.(L.listElementsL)
            in M.continue $ L.listInsert pos el l

        V.EvKey (V.KChar '-') [] ->
            case l^.(L.listSelectedL) of
                Nothing -> M.continue l
                Just i -> M.continue $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< L.handleListEvent ev l
    where

      nextElement :: Vec.Vector String -> String
      nextElement v = fromMaybe "?" $ Vec.find (flip Vec.notElem v) (Vec.fromList ["sin", "cos", "tan", "ctan", "atan"])
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState :: L.List () String
initialState = L.list () (Vec.fromList
                          [ "Bitcoin"
                          , "Ethereum"
                          , "Litecoin"
                          , "DogeCoin"
                          ]) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () String) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState

