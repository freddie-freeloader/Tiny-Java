module Compiler.BytecodeGeneration.MethodsToByte (methodsToByte) where
import Data.Word (Word8,Word16)
import Data.Bits
import Compiler.BytecodeGeneration.AttributesToByte
import Compiler.AbstractBytecode
import Compiler.BytecodeGeneration.ToByteUtil

methodsToByte :: Methods -> [Word8]
methodsToByte = (foldr (\x app -> (methodToByte x) ++ app) [])

methodToByte :: Method -> [Word8]
methodToByte (Method methodAccessFlags methodNameIndex methodDescriptorIndex methodAttributes) =
              (convertWord16ToListWord8 (methodAccessFlagsToByte methodAccessFlags))
              ++ (convertWord16ToListWord8 methodNameIndex)
              ++ (convertWord16ToListWord8 methodDescriptorIndex)
              ++ (convertWord16ToListWord8 (((fromIntegral . length) methodAttributes) :: Word16))
              ++ (attributesToByte methodAttributes)

methodAccessFlagsToByte :: MethodAccessFlags -> Word16
methodAccessFlagsToByte = foldr (\x acc -> (methodAccessFlagToByte x) .|. acc) 0x0000

methodAccessFlagToByte :: MethodAccessFlag -> Word16
methodAccessFlagToByte M_PUBLIC = 0x0001
methodAccessFlagToByte M_PRIVATE = 0x0002
methodAccessFlagToByte M_PROTECTED = 0x0004
methodAccessFlagToByte M_STATIC = 0x0008
methodAccessFlagToByte M_FINAL = 0x0010
methodAccessFlagToByte M_SYNCHRONIZED = 0x0020
methodAccessFlagToByte M_BRIDGE = 0x0040
methodAccessFlagToByte M_VARARGS = 0x0080
methodAccessFlagToByte M_NATIVE = 0x0100
methodAccessFlagToByte M_ABSTRACT = 0x400
methodAccessFlagToByte M_STRICT = 0x0800
methodAccessFlagToByte M_SYNTHETIC = 0x1000
