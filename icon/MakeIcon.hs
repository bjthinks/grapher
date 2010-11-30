import qualified Data.ByteString as B
import Data.Word
import Data.Bits

-- ICO is a little-endian file format

int8 :: Int -> B.ByteString
int8 n = B.pack [ fromIntegral n ]

int16 :: Int -> B.ByteString
int16 n = B.pack [ fromIntegral n, fromIntegral (n `shiftR` 8) ]

int32 :: Int -> B.ByteString
int32 n = B.pack [ fromIntegral n,
                   fromIntegral (n `shiftR` 8),
                   fromIntegral (n `shiftR` 16),
                   fromIntegral (n `shiftR` 24) ]

pixels :: String -> B.ByteString
pixels s = B.concat $ map pixelByte s

pixelByte :: Char -> B.ByteString
pixelByte '#' = B.concat [ int8 0, int8 0, int8 0, int8 255 ]
pixelByte '.' = B.concat [ int8 255, int8 255, int8 255, int8 255 ]

icon :: B.ByteString
icon = B.concat [
  -- Reserved, always 0
  int16 0,
  -- Image type, 1 for icon (.ICO) file
  int16 1,
  -- Number of images in the file
  int16 1,

  -- For each image,
  -- Width in pixels
  int8 16,
  -- Height in pixels
  int8 16,
  -- Number of colors in the palette, 0 for Truecolor
  int8 0,
  -- Reserved 0
  int8 0,
  -- Color planes: should be 1
  int16 1,
  -- Bits per pixel
  int16 32,
  -- Size of image data in bytes
  int32 1128,
  -- Offset of image data in the file
  int32 22,

  -- Image data header
  -- Size of header = 40
  int32 40,
  -- Icon width
  int32 16,
  -- Icon height (both AND and XOR bitmaps put together)
  int32 32,
  -- Number of planes
  int16 1,
  -- Bits per pixel
  int16 32,
  -- Type of compression = 0
  int32 0,
  -- Size of image in bytes, 0 for uncompressed
  int32 0,
  -- 4 unused int32s
  int32 0, int32 0, int32 0, int32 0,
  -- Color map -- does not exist

  -- Pixels are stored bottom-up, left-to-right
  -- XOR bitmap
  B.concat $ map pixels $ reverse
  [".......#.......#",
   ".......#......#.",
   ".......#.....#..",
   ".......#....#...",
   ".......#...#....",
   ".......#..#.....",
   ".......#.#......",
   ".......##.......",
   "################",
   "......##........",
   ".....#.#........",
   "....#..#........",
   "...#...#........",
   "..#....#........",
   ".#.....#........",
   "#......#........"],
  -- AND bitmap is basically an alpha channel, one bit per pixel,
  -- padded with zeroes so each row is 32 bits
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 0, int8 0, int8 0, int8 0,
  int8 0, int8 0, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,
  int8 255, int8 255, int8 0, int8 0,

  B.pack []]

main :: IO ()
main = B.putStr icon
