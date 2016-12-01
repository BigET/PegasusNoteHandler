module Main where

import Prelude hiding (splitAt, concat, drop, null)
import qualified Prelude as P (concat)
import System.IO
import System.Environment
import System.Exit
import System.USB
import Data.Vector ((!))
import qualified Data.Vector as V (toList)
import Data.List (find)
import Control.Applicative ((<$>))
import Data.ByteString (pack, unpack, ByteString(..), index, splitAt, concat, drop, null)
import qualified Data.ByteString as B (length)
import Data.Word
import Data.Bits

data PenMode = PenModePlaceHolder | PenMode | MouseMode | MobileMode deriving (Enum, Show)
data PenBattery = PenBatNoReport | PenBatLow | PenBatGood deriving Enum
data PenOrientation = PenOrientationTop | PenOrientationLeft | PenOrientationRight deriving Enum
data PenScale = ScaleMinim | ScaleVerySmall | ScaleSmall | ScaleSmallish |
    ScaleMedium | ScaleBigish | ScaleBig | ScaleVeryBig | ScaleMaxim deriving Enum

sendCommand :: DeviceHandle -> ByteString -> IO ()
sendCommand dev msg = writeControlExact dev ctrlSetup msg 1000
  where ctrlSetup = ControlSetup Class ToEndpoint 9 2 0

receiveCommand :: DeviceHandle -> IO ByteString
receiveCommand dev = readInterrupt dev eaddr 0x40 1000 >>= r
  where eaddr = EndpointAddress 0x81 In
        r (resp, Completed) = return resp
        r _ = error "unfinished response."

sendAndReceiveCommand :: DeviceHandle -> ByteString -> IO ByteString
sendAndReceiveCommand dev msg = sendCommand dev msg >> receiveCommand dev

setOrientationAndScale :: DeviceHandle -> PenOrientation -> PenScale -> IO()
setOrientationAndScale dev orient scale = sendCommand dev msg
  where msg = pack [0x02, 0x04, 0x80, 0xb6, fromIntegral $ fromEnum orient,
                fromIntegral $ fromEnum scale]

getVersionAndMode :: DeviceHandle -> IO (PenMode, Word16)
getVersionAndMode dev = sendAndReceiveCommand dev msg >>= r
  where msg = pack [0x02, 0x01, 0x95]
        r resp = return (toEnum $ fromIntegral $ 0x03 .&. index resp 10,
                fromIntegral (index resp 3) * 256 + fromIntegral (index resp 4))

setMode :: DeviceHandle -> PenMode -> IO ()
setMode dev mode = sendCommand dev $ pack [0x02, 0x04, 0x80, 0xb5, wmode, wmode]
  where wmode = fromIntegral $ fromEnum mode

downloadPages :: DeviceHandle -> IO (ByteString)
downloadPages dev = do
    let testSig = replicate 5 (0xaa ==) ++ [const True, const True, (0x55 ==), (0x55 ==)]
        getPNr :: ByteString -> Int
        getPNr msg | and $ zipWith ($) testSig $ unpack msg =
                        fromIntegral (index msg 5) * 256 + fromIntegral (index msg 6)
                   | otherwise = error $ "invalid response:" ++ show msg
        getPag expPagNr = do
            (pgNr, msg) <- splitAt 2 <$> receiveCommand dev
            let pNr = fromIntegral (index pgNr 0) * 256 + fromIntegral (index pgNr 1)
            if pNr == expPagNr then return msg
                else error $ "expecting pageNr:" ++ show expPagNr ++ ", but got:" ++ show pNr
        sendACK = sendCommand dev (pack [0x02, 0x01, 0xb6])
    pagNr <- getPNr <$> sendAndReceiveCommand dev (pack [0x02, 0x01, 0xb5])
    sendACK
    pags <- sequence $ map getPag $ [1 .. pagNr]
    sendACK
    return $ concat pags

deletePages :: DeviceHandle -> IO ()
deletePages = flip sendCommand $ pack [0x02, 0x01, 0xb0]


penMinX = -4935
penMaxX = 4935
penMinY = -1270
penMaxY = 12690
penW = penMaxX - penMinX
penH = penMaxY - penMinY
a4W = 210
a4H = 297
xRatio :: Float
xRatio = a4W / penW
yRatio :: Float
yRatio = a4H / penH
xUp = 0
yUp = -32768

data Stroke = MoveTo {strokeX :: !Int, strokeY :: !Int} | LineTo {strokeX :: !Int, strokeY :: !Int} deriving Show
data Page = Page !Int [Stroke] deriving Show

data Document = Document String [Page] deriving Show

psHeader docTitle pages = unlines [
    "%!PS-Adobe-1.0",
    "%%Title: " ++ docTitle,
    "%%Creator: Pegasus2PS",
    "%%BoundingBox: 0 0 595 842",
    "%%Pages: " ++ show pages,
    "%%EndComments", "",
    "2.830 2.830 scale", "",
    "0.3 setlinewidth", "", ""]

psPage (Page pnr strks) = unlines $
    ("%%Page: " ++ show pnr ++ ' ' : show pnr) :
    map psStroke strks ++
    ["stroke", "", ""]
  where psStroke (MoveTo x y) = pPoint x y ++ ' ' : "moveto"
        psStroke (LineTo x y) = pPoint x y ++ ' ' : "lineto"
        pPoint x y = show ((fromIntegral x - penMinX) * xRatio) ++
            ' ' : show (a4H - (fromIntegral y - penMinY) * yRatio)
        moveMode LineTo{} = "lineto"

encode (Document title pags) = psHeader title (length pags) ++ P.concat (map psPage pags)

decodePage :: Int -> Int -> ByteString -> [Page]
decodePage pnr addr strm
    | nextAddr == 0 = []
    | otherwise = Page pnr strokes : decodePage (pnr + 1) nextAddr nextStrm
  where nextAddr = if B.length strm >= 4 then fromIntegral (index strm 0) + fromIntegral (index strm 1) * 256 + fromIntegral (index strm 3) * 65536
                else 0
        (pg, nextStrm) = splitAt (nextAddr - addr) strm
        (hdr, strks) = splitAt 14 pg
        strokes = decodeStroke True strks
        decodeStroke :: Bool -> ByteString -> [Stroke]
        decodeStroke pUp sr
            | B.length sr < 4 = []
            | x == xUp && y == yUp = decodeStroke True srs
            | pUp = MoveTo x y : decodeStroke False srs
            | otherwise = LineTo x y : decodeStroke False srs
          where x = cmpl $ fromIntegral (index sr 0) + fromIntegral (index sr 1) * 256
                y = cmpl $ fromIntegral (index sr 2) + fromIntegral (index sr 3) * 256
                cmpl val = if val >= 0x8000 then val - 0x10000 else val
                srs = drop 4 sr

decode strm = Document "saved pages" $ decodePage 1 0 strm


main = do
    ctx <- newCtx
    hSetBuffering stdout $ BlockBuffering Nothing
    hSetBinaryMode stdout True
    args <- getArgs
    usbdevs <- getDevices ctx >>= mapM (\x -> ((,) x) <$> getDeviceDesc x)
    let matchPen DeviceDesc{deviceVendorId = 0x0e20, deviceProductId = 0x0101} = True
        matchPen _ = False
    case fst <$> find (matchPen . snd) usbdevs of
        Nothing -> hPutStrLn stderr "There is no pen." >> exitFailure
        Just penDev -> withDeviceHandle penDev $ \penHndl -> do
            setAutoDetachKernelDriver penHndl True
            withClaimedInterface penHndl 0 $ withClaimedInterface penHndl 1 $ do
                setOrientationAndScale penHndl PenOrientationTop ScaleMaxim
                getVersionAndMode penHndl
                pags <- downloadPages penHndl
                if "-d" `elem` args then deletePages penHndl else return ()
                putStrLn $ encode $ decode pags
