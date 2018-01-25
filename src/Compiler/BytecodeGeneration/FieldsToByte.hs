module Compiler.BytecodeGeneration.FieldsToByte (fieldsToByte) where
import Data.Word (Word8,Word16)
import Data.Bits
import Compiler.BytecodeGeneration.AttributesToByte
import Compiler.AbstractBytecode
import Compiler.BytecodeGeneration.ToByteUtil

fieldsToByte :: Fields -> [Word8]
fieldsToByte = (foldr (\x app -> (fieldToByte x) ++ app) [])

fieldToByte :: Field -> [Word8]
fieldToByte (Field fieldAccessFlags fieldNameIndex fieldDescriptorIndex fieldAttributes) =
              (convertWord16ToListWord8 (fieldAccessFlagsToByte fieldAccessFlags))
              ++ (convertWord16ToListWord8 fieldNameIndex)
              ++ (convertWord16ToListWord8 fieldDescriptorIndex)
              ++ (convertWord16ToListWord8 (((fromIntegral . length) fieldAttributes) :: Word16))
              ++ (attributesToByte fieldAttributes)

fieldAccessFlagsToByte :: FieldAccessFlags -> Word16
fieldAccessFlagsToByte = foldr (\x acc -> (fieldAccessFlagToByte x) .|. acc) 0x0000

fieldAccessFlagToByte :: FieldAccessFlag -> Word16
fieldAccessFlagToByte F_PUBLIC = 0x0001
fieldAccessFlagToByte F_PRIVATE = 0x0002
fieldAccessFlagToByte F_PROTECTED = 0x0004
fieldAccessFlagToByte F_STATIC = 0x0008
fieldAccessFlagToByte F_FINAL = 0x0010
fieldAccessFlagToByte F_VOLATILE = 0x0040
fieldAccessFlagToByte F_TRANSIENT = 0x0080
fieldAccessFlagToByte F_SYNTHETIC = 0x1000
fieldAccessFlagToByte F_ENUM = 0x4000
