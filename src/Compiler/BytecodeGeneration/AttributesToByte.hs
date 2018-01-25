module Compiler.BytecodeGeneration.AttributesToByte (attributesToByte) where
import Data.Word (Word8,Word16,Word32)
import Data.Bits
import Compiler.AbstractBytecode
import Compiler.BytecodeGeneration.InstructionsToBytes
import Compiler.BytecodeGeneration.ToByteUtil

attributesToByte :: Attributes -> [Word8]
attributesToByte = (foldr (\x app -> (attributeToByte x) ++ app) [])

attributeToByte :: Attribute -> [Word8]
attributeToByte (ConstantValue x y) =
  (convertWord16ToListWord8 x) ++ [0x02] ++ (convertWord16ToListWord8 y)

attributeToByte (Code codeNameIndex maxStack maxLocals instructions exeptionTable attributes) =
  (convertWord16ToListWord8 codeNameIndex)
  ++ (convertWord32ToListWord8 (((fromIntegral . length) v) :: Word32))
  ++ v
  where v = (convertWord16ToListWord8 maxStack)
          ++ (convertWord16ToListWord8 maxLocals)
          ++ (convertWord32ToListWord8 (((fromIntegral . length) w) :: Word32))
          ++ w
          ++ [0x00,0x00] --exeptionTable length
          -- ++ (exeptionTableToByte exeptionTable)
          ++ (convertWord16ToListWord8 (((fromIntegral . length) attributes) :: Word16))
          ++ (attributesToByte attributes)
          where w = (instructionsToByte instructions)

attributeToByte (StackMapTable stackMapNameIndex entries) = -- dummy implementation
  [0x00,0x00]
  ++ [0x00,0x00,0x00,0x02] -- Attribute_length
  ++ [0x00,0x00] --number of Entries

attributeToByte (InnerClass innerClassNameIndex classes) =
  (convertWord16ToListWord8 innerClassNameIndex)
  ++ (convertWord32ToListWord8 (((fromIntegral . length) classesByteLength) :: Word32))
  ++ classesByteLength
  where classesByteLength = (convertWord16ToListWord8(((fromIntegral . length) classes) :: Word16))
                            ++ (convertListWord16ToListWord8(classesToByte classes))

attributeToByte (Synthetic syntheticNameIndex) =
  (convertWord16ToListWord8 syntheticNameIndex)
  ++ [0x00,0x00,0x00,0x00]

attributeToByte (SourceFile sourceFileNameIndex sourcefileIndex) =
  (convertWord16ToListWord8 sourceFileNameIndex)
  ++ [0x00,0x00,0x00,0x02]
  ++ (convertWord16ToListWord8 sourcefileIndex)

--TODO from here on

attributeToByte (LineNumberTable lineNumberTableNameIndex lineNumberTable) =
  (convertWord16ToListWord8 lineNumberTableNameIndex)
  -- ++ attributelength in bytes
  -- ++ linenumbertable length numbers
  ++ (convertListWord16ToListWord8 (lineNumberTableToByte lineNumberTable))

{-
attributeToByte (LocalVariableTable x localVariableTable) =
  (convertListWord16ToListWord8 x) ++ (convertListWord16ToListWord8 (localvariableTableToByte localVariableTable))
attributeToByte (Deprecated x) = (convertListWord16ToListWord8 x)
-}
-- Maybe TODO from ExceptionTables(line 221) to VerificationType(line 268)

--verificationTypeToByte :: VerificationType ->

classesToByte :: Classes -> [Word16]
classesToByte = foldr (\x acc -> (classToByte x) ++ acc) []

classToByte :: Class -> [Word16]
classToByte x = (inner_class_info_index x)
  : (f $ outer_class_info_index x)
  : (f $ inner_name_index x)
  : (innerClassAccessFlagsToByte $ inner_class_access_flags x)
  : [] where f (Just t) = t
             f Nothing = 0

innerClassAccessFlagsToByte :: InnerClassAccessFlags -> Word16
innerClassAccessFlagsToByte = foldr (\x acc -> (innerClassAccessFlagToByte x) .|. acc) 0x0000

innerClassAccessFlagToByte :: InnerClassAccessFlag -> Word16
innerClassAccessFlagToByte IC_PUBLIC = 0x0001
innerClassAccessFlagToByte IC_PRIVATE = 0x0002
innerClassAccessFlagToByte IC_PROTECTED = 0x0004
innerClassAccessFlagToByte IC_STATIC = 0x0008
innerClassAccessFlagToByte IC_FINAL = 0x0010
innerClassAccessFlagToByte IC_INTERFACE = 0x0200
innerClassAccessFlagToByte IC_ABSTRACT = 0x0400
innerClassAccessFlagToByte IC_SYNTHETIC = 0x1000
innerClassAccessFlagToByte IC_ANNOTATION = 0x2000
innerClassAccessFlagToByte IC_ENUM = 0x4000

lineNumberTableToByte ::  LineNumberTable -> [Word16]
lineNumberTableToByte = foldr (\x acc -> (lineNumberToByte x) ++ acc) []

lineNumberToByte :: LineNumber -> [Word16]
lineNumberToByte x = (line_number_start_pc x)
  : (line_number x)
  : []

localvariableTableToByte :: LocalVariableTable -> [Word16]
localvariableTableToByte = foldr (\x acc -> (localVariableToByte x) ++ acc) []

localVariableToByte :: LocalVariable -> [Word16]
localVariableToByte x = (local_variable_start_pc x)
  -- add length FIXME
  : (local_variable_name_index x)
  : (local_variable_descriptor_index x)
  : (local_variable_index x)
  : []
