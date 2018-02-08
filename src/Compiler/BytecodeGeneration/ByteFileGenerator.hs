module Compiler.BytecodeGeneration.ByteFileGenerator (generateByteFile) where
import System.IO hiding (writeFile)
import Data.ByteString as Bs hiding (foldr,length)
import Data.Word (Word8,Word16)
import Data.Bits
import Compiler.AbstractBytecode
import Compiler.BytecodeGeneration.ConstantPoolToByte
import Compiler.BytecodeGeneration.FieldsToByte
import Compiler.BytecodeGeneration.MethodsToByte
import Compiler.BytecodeGeneration.AttributesToByte
import Compiler.BytecodeGeneration.ToByteUtil

generateByteFile ::  FilePath -> ClassFile -> IO()
generateByteFile filePath classFile = Bs.writeFile filePath (packListWord8ToByteString (classFileToByte classFile))

classFileToByte :: ClassFile -> [Word8]
classFileToByte (ClassFile magic minver maxver arrayCP
  accessFlags this super arrayInterfaces
  arrayFields arrayMethods arrayAttributes) =
             (magicToByte magic)
             ++ (minverToByte minver)
             ++ (maxverToByte maxver)
             ++ convertWord16ToListWord8 ((((fromIntegral . length) arrayCP) :: Word16)+ 1)
             ++ (arrayCPToByte arrayCP)
             ++ (convertWord16ToListWord8  (classFileAccessFlagsToByte accessFlags))
             ++ (thisToByte this)
             ++ (superToByte super)
             ++ convertWord16ToListWord8 (((fromIntegral . length) arrayInterfaces) :: Word16)
             ++ (arrayInterfacesToByte arrayInterfaces)
             ++ convertWord16ToListWord8 (((fromIntegral . length) arrayFields) :: Word16)
             ++ (arrayFieldsToByte arrayFields)
             ++ convertWord16ToListWord8 (((fromIntegral . length) arrayMethods) :: Word16)
             ++ (arrayMethodsToByte arrayMethods)
             ++ convertWord16ToListWord8 (((fromIntegral . length) arrayAttributes) :: Word16)
             ++ (arrayAttributesToByte arrayAttributes)

magicToByte :: Magic -> [Word8]
magicToByte _ = [0xca,0xfe,0xba,0xbe]

minverToByte :: MinorVersion -> [Word8]
minverToByte _ = [0x00,0x00]

maxverToByte :: MajorVersion -> [Word8]
maxverToByte _ = [0x00,0x30]

arrayCPToByte :: ConstantPool -> [Word8]
arrayCPToByte arrayCP = constantPoolToByte arrayCP

classFileAccessFlagsToByte :: ClassFileAccessFlags -> Word16
classFileAccessFlagsToByte = foldr (\x acc -> (classFileAccessFlagToByte x) .|. acc) 0x0000

classFileAccessFlagToByte :: ClassFileAccessFlag -> Word16
classFileAccessFlagToByte PUBLIC = 0x0001
classFileAccessFlagToByte FINAL = 0x0010
classFileAccessFlagToByte SUPER = 0x0020
classFileAccessFlagToByte INTERFACE = 0x0200
classFileAccessFlagToByte ABSTRACT = 0x0400
classFileAccessFlagToByte SYNTHETIC = 0x1000
classFileAccessFlagToByte ANNOTATION = 0x2000
classFileAccessFlagToByte ENUM = 0x4000

thisToByte :: ThisClass -> [Word8]
thisToByte (ThisClass thisClassIndex) = convertWord16ToListWord8 thisClassIndex

superToByte :: SuperClass -> [Word8]
superToByte (SuperClass superClassIndex) = convertWord16ToListWord8 superClassIndex

arrayInterfacesToByte :: Interfaces -> [Word8]
arrayInterfacesToByte _ = []

arrayFieldsToByte :: Fields -> [Word8]
arrayFieldsToByte arrayFields = fieldsToByte arrayFields

arrayMethodsToByte :: Methods -> [Word8]
arrayMethodsToByte arrayMethods = methodsToByte arrayMethods

arrayAttributesToByte :: Attributes -> [Word8]
arrayAttributesToByte arrayAttributes = attributesToByte arrayAttributes
