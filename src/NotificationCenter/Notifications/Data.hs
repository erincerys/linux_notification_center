{-# LANGUAGE OverloadedStrings #-}

module NotificationCenter.Notifications.Data
  ( Urgency(..)
  , CloseType (..)
  , Notification(..)
  , notiAge
  , Image(..)
  , parseImageString
  , rawImgToPixBuf
  , fitWithinWithoutEnlarging
  , thumbnailOnReceive
  ) where

import qualified Data.Text as Text
import Data.Word ( Word32, Word8 )
import Data.Int ( Int32, Int )
import qualified Data.ByteString as BS
import qualified Data.Map as Map ( Map )
import Data.List ( sortOn )
import Data.Time.Clock ( UTCTime, NominalDiffTime, diffUTCTime )
import DBus ( Variant (..), Signal )

import qualified Data.Yaml as Y
import Data.Yaml as Y ((.=))

import GI.GLib (bytesNew, bytesGetData)
import GI.GdkPixbuf (Pixbuf(..), pixbufNewFromBytes, Colorspace(..)
                    , pixbufGetWidth, pixbufGetHeight, pixbufGetRowstride
                    , pixbufGetHasAlpha, pixbufGetBitsPerSample
                    , pixbufGetNChannels, pixbufReadPixelBytes
                    , pixbufScaleSimple, InterpType(..))

data Urgency = Normal | Low | High deriving Eq
data CloseType = Timeout | User | CloseByCall | Other deriving Eq

instance Eq Notification where
  a == b = notiId a == notiId b

data Notification = Notification
  { notiAppName :: Text.Text -- ^ Application name
  , notiRepId :: Word32 -- ^ Replaces id
  , notiId :: Int -- ^ Id
  , notiIcon :: !Image -- ^ App icon
  , notiImg :: !Image -- ^ Image
  , notiImgSize :: Int -- ^ Image size
  , notiSummary :: Text.Text -- ^ Summary
  , notiBody :: Text.Text -- ^ Body
  , notiActions :: [Text.Text] -- ^ Actions
  , notiActionCommands :: [(String, String)] -- ^ Actions
  , notiActionIcons :: !Bool -- ^ Use icons for action-buttons
  , notiHints :: !(Map.Map Text.Text Variant) -- ^ Hints
  , notiUrgency :: !Urgency
  , notiTimeout :: Int32 -- ^ Expires timeout (milliseconds)
  , notiTime :: Text.Text
  , notiTransient :: !Bool
  , notiSendClosedMsg :: Bool -- ^ If notiOnClosed should be ignored
  , notiOnClosed :: CloseType -> IO ()
  , notiTop :: Maybe Int
  , notiRight :: Maybe Int
    -- ^ Should be called when the notification is closed, either by
    --   timeout or by user
  , notiOnAction :: [(String, String)] -> String -> Maybe String -> IO ()
    -- ^ Should be called when an action is used
  , notiPercentage :: !(Maybe Double)
    -- ^ The percentage that should be shown in a percentage bar
  , notiClassName :: Text.Text
    -- ^ Class name to be attached to the window of the notification
  , notiReceived :: UTCTime
  }

notiAge :: UTCTime -> Notification -> NominalDiffTime
notiAge now n = diffUTCTime now (notiReceived n)

instance Y.ToJSON Notification where
  toJSON n = Y.object [
    "appname" .= notiAppName n
    , "repId" .= notiRepId n
    , "id" .= notiId n
    , "icon" .= ((show $ notiIcon n) :: String)
    , "image" .= ((show $ notiImg n) :: String)
    , "imageSize" .= notiImgSize n
    , "title" .= notiSummary n
    , "body" .= notiBody n
    , "actions" .= notiActions n
    , "actionIcons" .= notiActionIcons n
--    , "notiHints" .= notiHints n
--    , "notiUrgency" .= ((show $ notiUrgency n) :: String)
    , "timeout" .= notiTimeout n
    , "time" .= notiTime n
    , "transient" .= notiTransient n
    , "sendClosedMsg" .= notiSendClosedMsg n
    , "top" .= notiTop n
    , "right" .= notiRight n
    , "percentage" .= notiPercentage n ]

instance Show Notification where
  show n = foldl (++) ""
    [ "Notification { \n"
    , "  notiAppName = " ++ (Text.unpack $ notiAppName n) ++ ", \n"
    , "  notiRepId = " ++ (show $ notiRepId n) ++ ", \n"
    , "  notiId = " ++ (show $ notiId n) ++ ", \n"
    , "  notiIcon = " ++ (show $ notiIcon n) ++ ", \n"
    , "  notiImg = " ++ (show $ notiImg n) ++ ", \n"
    , "  notiImgSize = " ++ (show $ notiImgSize n) ++ ", \n"
    , "  notiSummary = " ++ (Text.unpack $ notiSummary n) ++ ", \n"
    , "  notiBody = " ++ (Text.unpack $ notiBody n) ++ ", \n"
    , "  notiActions = " ++ (show $ Text.unpack <$> notiActions n) ++ ", \n"
    , "  notiActionIcons = " ++ (show $ notiActionIcons n) ++ ", \n"
    , "  notiHints = " ++ (show $ notiHints n) ++ ", \n"
    , "  notiTimeout = " ++ (show $ notiTimeout n) ++ ", \n"
    , "  notiTime = " ++ (Text.unpack $ notiTime n) ++ ", \n"
    , "  notiTransient = " ++ (show $ notiTransient n) ++ ", \n"
    , "  notiSendClosedMsg = " ++ (show $ notiSendClosedMsg n) ++ "\n"
    , "  notiTop = " ++ (show $ notiTop n) ++ "\n"
    , "  notiRight = " ++ (show $ notiRight n) ++ "\n"
    , "  notiPercentage = " ++ (show $ notiPercentage n) ++ "\n"
    , " }\n" ]

data Image = RawImg
  ( Int32 -- width
  , Int32 -- height
  , Int32 -- rowstride
  , Bool -- alpha
  , Int32 -- bits per sample
  , Int32 -- channels
  , BS.ByteString -- image data
  )
  | ImagePath String
  | NamedIcon String
  | NoImage deriving (Show, Eq)

parseImageString :: Text.Text -> Image
parseImageString a = if (Text.isPrefixOf "file://" a) then
                       ImagePath $ Text.unpack$ Text.drop 6 a
                     else
                       if (Text.length a > 0) then
                         NamedIcon $ Text.unpack a
                       else
                         NoImage


-- | Returns Nothing for image-data whose header does not describe its
--   buffer, which gdk-pixbuf would otherwise read past.
rawImgToPixBuf :: Image -> IO (Maybe Pixbuf)
rawImgToPixBuf (RawImg (width, height, rowstride, alpha, bits, channels, datas))
  | valid = do
      bytes <- bytesNew (Just datas)
      Just <$> pixbufNewFromBytes bytes
        ColorspaceRgb alpha bits width height rowstride
  | otherwise = return Nothing
  where
    rowBytes = toInteger width * toInteger channels
    valid = bits == 8
      && channels == (if alpha then 4 else 3)
      && width > 0 && height > 0
      && toInteger rowstride >= rowBytes
      && toInteger (BS.length datas)
         >= toInteger (height - 1) * toInteger rowstride + rowBytes
rawImgToPixBuf _ = return Nothing

needsShrinking :: Int32 -> Int32 -> Int32 -> Int32 -> Bool
needsShrinking targetW targetH w h =
  w >= targetW && h >= targetH && (w, h) /= (targetW, targetH)

fitWithinWithoutEnlarging :: Int32 -> Int32 -> Pixbuf -> IO (Maybe Pixbuf)
fitWithinWithoutEnlarging w h pb = do
  oldW' <- pixbufGetWidth pb
  oldH' <- pixbufGetHeight pb
  let oldW = fromIntegral oldW' :: Double
      oldH = fromIntegral oldH' :: Double
      targetW = fromIntegral w :: Double
      targetH = fromIntegral h :: Double
  if not (needsShrinking w h oldW' oldH') then
    return (Just pb)
    else do
    let newW = fromIntegral $ floor $ if oldW > oldH then
          targetW else (oldW * (targetH / oldH))
        newH = fromIntegral $ floor $ if oldW > oldH then
          (oldH * (targetW / oldW)) else targetH
    pixbufScaleSimple pb newW newH InterpTypeBilinear

thumbnailOnReceive :: Int -> Image -> IO Image
thumbnailOnReceive size img@(RawImg (width, height, _, _, _, _, _))
  | not (needsShrinking size' size' width height) = return img
  | otherwise = do
      mPb <- rawImgToPixBuf img
      mScaled <- maybe (return Nothing) (fitWithinWithoutEnlarging size' size') mPb
      maybe (return NoImage) toRawImg mScaled
  where
    size' = fromIntegral size
    toRawImg pb = do
      w <- pixbufGetWidth pb
      h <- pixbufGetHeight pb
      stride <- pixbufGetRowstride pb
      alpha <- pixbufGetHasAlpha pb
      bits <- pixbufGetBitsPerSample pb
      channels <- pixbufGetNChannels pb
      pixels <- bytesGetData =<< pixbufReadPixelBytes pb
      return $ maybe NoImage
        (\px -> RawImg (w, h, stride, alpha, bits, channels, px)) pixels
thumbnailOnReceive _ img = return img
