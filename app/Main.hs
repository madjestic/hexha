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
import Network.HTTP.Conduit
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L8
import Net.Stocks
import qualified Net.IEX.TimeSeries         as IEXTimeSeries

import Debug.Trace    as DT

type Name = ()

---------------------------------------------------------------------------------------------------------------------------------------------------------------
--- iexcloud --------------------------------------------------------------------------------------------------------------------------------------------------

type AuthAndSymbol = (String, Symbol)
type Symbol = String

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query = try $ simpleHttp query

test' :: IO (Maybe [IEXTimeSeries.TimeSeries])
test' =
  do
    -- obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_8d1fbeccf06745019a98635b05346b90"
    obj <- getNonJSONData "https://sandbox.iexapis.com/stable/tops?symbols=aapl&token=Tpk_8d1fbeccf06745019a98635b05346b90"
    putStrLn $ show obj
    -- case (DT.trace ("obj :" ++ show obj) $ obj) of
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getTS'' :: AuthAndSymbol -> QSParms -> IO (Maybe [IEXTimeSeries.TimeSeries])
-- getTS'' (auth, symb) parms = do
getTS'' _ _ = do
  obj <- getNonJSONData "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_8d1fbeccf06745019a98635b05346b90"
         -- baseURL
         -- ++ "/time-series/REPORTED_FINANCIALS/"
         -- ++ symb
         -- ++ "/10-Q"
         -- ++ "?"
         -- ++ "from=2018-01-01&to=2019-06-01"
         -- ++ tokenize' auth
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr


parms = QSParms 0 0
symb  = "AAPL"
token = "Tpk_8d1fbeccf06745019a98635b05346b90"
query = "https://sandbox.iexapis.com/stable/time-series/REPORTED_FINANCIALS/AAPL/10-Q?from=2018-01-01&to=2019-06-01&token=Tpk_8d1fbeccf06745019a98635b05346b90"

test =
  do
    x <- getTS'' (token, symb) parms
    print $ length x

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

-- obj <- getNonJSONData "https://sandbox.iexapis.com/stable/tops?symbols=aapl&token=Tpk_8d1fbeccf06745019a98635b05346b90"
-- Right "[{\"symbol\":\"AAPL\",\"sector\":\"nhycottelongerloccei\",\"securityType\":\"cs\",\"bidPrice\":0,\"bidSize\":0,\"askPrice\":0,\"askSize\":0,\"lastUpdated\":1638239671098,\"lastSalePrice\":135.04,\"lastSaleSize\":104,\"lastSaleTime\":1656554394913,\"volume\":2101406}]"
-- (\(Right x)->x) obj :: L8.ByteString
-- decode ((\(Right x)->x) obj) :: Maybe [Object]
-- fromMaybe mempty $ decode ((\(Right x)->x) obj) :: Maybe [Object]

-- obj <- getNonJSONData "https://sandbox.iexapis.com/stable/tops?symbols=aapl&token=Tpk_8d1fbeccf06745019a98635b05346b90"
-- obj' = head $ fromJust ( fromMaybe mempty $ decode ((\(Right x)->x) obj) :: Maybe [Object])
-- parse ((.:) obj') (Data.Text.pack "lastSalePrice") :: Result Double
-- parse ((.:) obj') (Data.Text.pack "symbol") :: Result String

