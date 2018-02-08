module Compiler.BytecodeGeneration.ToByteUtil where
import Data.Int (Int8,Int16, Int32, Int64)
import Data.Word
import Data.Bits
import Data.ByteString as Bs hiding (foldr)

unpackByteStringToListWord8 :: ByteString -> [Word8]
unpackByteStringToListWord8 x = (Bs.unpack x)

concatByteStrings :: [ByteString] -> ByteString
concatByteStrings = foldr (\x y-> (Bs.append x y)) Bs.empty

packListWord8ToByteString :: [Word8] -> ByteString
packListWord8ToByteString x = (Bs.pack x)

convertListWord16ToListWord8 :: [Word16] -> [Word8]
convertListWord16ToListWord8 = foldr (\x app -> (convertWord16ToListWord8 x) ++ app)[]

convertWord16ToListWord8 :: Word16 -> [Word8]
convertWord16ToListWord8 x = [(fromIntegral (shiftR x 8))] ++ [fromIntegral x]

convertWord32ToListWord8 :: Word32 -> [Word8]
convertWord32ToListWord8 x = [(fromIntegral (shiftR x 24))] ++ [(fromIntegral (shiftR x 16))] ++ [(fromIntegral (shiftR x 8))]++ [fromIntegral x]

convertInt32ToWord32 :: Int32 -> Word32
convertInt32ToWord32 = fromIntegral

convertInt32ToListWord8 :: Int32 -> [Word8]
convertInt32ToListWord8 w = (convertWord16ToListWord8(fromIntegral $ shiftR w 16)) ++ (convertWord16ToListWord8 . fromIntegral) w

convertInt16ToListWord8 :: Int16 -> [Word8]
convertInt16ToListWord8 = convertWord16ToListWord8 . fromIntegral

convertInt8ToWort8 :: Int8 -> Word8
convertInt8ToWort8 x = fromIntegral x
