module Compiler.BytecodeGeneration.ConstantPoolToByte (constantPoolToByte) where
import Data.Int (Int16, Int32, Int64)
import Data.Word
import Data.Bits
import Data.Text as T hiding (foldr,length)
import Data.Text.Encoding
import Compiler.AbstractBytecode
import Compiler.BytecodeGeneration.ToByteUtil

referenceToByte :: Reference -> Word16
referenceToByte GetField = 0x0001
referenceToByte GetStatic = 0x0002
referenceToByte PutField = 0x0003
referenceToByte PutStatic = 0x0004
referenceToByte InvokeVirtual = 0x0005
referenceToByte InvokeStatic = 0x0006
referenceToByte InvokeSpecial = 0x0007
referenceToByte NewInvokeSpecial = 0x0008
referenceToByte InvokeInterface = 0x0009

constantTagToByte :: Constant -> [Word8]
constantTagToByte (CONSTANT_Utf8 _ ) = [0x01]
constantTagToByte (CONSTANT_Integer _ ) = [0x03]
constantTagToByte (CONSTANT_Float _ ) = [0x04]
constantTagToByte (CONSTANT_Long _ ) = [0x05]
constantTagToByte (CONSTANT_Double _ ) = [0x06]
constantTagToByte (CONSTANT_Class _) = [0x07]
constantTagToByte (CONSTANT_String _ ) = [0x08]
constantTagToByte (CONSTANT_FieldRef _ _) = [0x09]
constantTagToByte (CONSTANT_MethodRef _ _) = [0x0a]
constantTagToByte (CONSTANT_InterfaceMethodRef _ _) = [0x0b]
constantTagToByte (CONSTANT_NameAndType _ _) = [0x0c]
constantTagToByte (CONSTANT_MethodHandle _ _) = [0x0f]
constantTagToByte (CONSTANT_MethodType _ ) = [0x10]
constantTagToByte (CONSTANT_InvokeDynamic _ _) = [0x12]

constantPoolToByte :: ConstantPool -> [Word8]
constantPoolToByte = (foldr (\x app -> (constantToByte x) ++ app) [])

constantToByte :: Constant -> [Word8]
constantToByte (CONSTANT_Class nameIndex) =
    (constantTagToByte (CONSTANT_Class nameIndex))
    ++ (convertWord16ToListWord8 nameIndex)

constantToByte (CONSTANT_FieldRef classIndex nameAndTypeIndex) =
    (constantTagToByte (CONSTANT_FieldRef classIndex nameAndTypeIndex))
    ++ (convertWord16ToListWord8 classIndex)
    ++ (convertWord16ToListWord8 nameAndTypeIndex)

constantToByte (CONSTANT_String stringIndex) =
    (constantTagToByte (CONSTANT_String stringIndex))
    ++ (convertWord16ToListWord8 stringIndex)

constantToByte (CONSTANT_MethodRef classIndex nameAndTypeIndex) =
    (constantTagToByte (CONSTANT_MethodRef classIndex nameAndTypeIndex))
    ++ (convertWord16ToListWord8 classIndex)
    ++ (convertWord16ToListWord8 nameAndTypeIndex)

constantToByte (CONSTANT_Integer intValue) =
    (constantTagToByte (CONSTANT_Integer intValue))
    ++ (convertWord32ToListWord8 $ convertInt32ToWord32 intValue)

constantToByte (CONSTANT_NameAndType constantNameAndTypeIndex constantNameAndTypeDescriptorIndex) =
    (constantTagToByte (CONSTANT_NameAndType constantNameAndTypeIndex constantNameAndTypeDescriptorIndex))
    ++ (convertWord16ToListWord8 constantNameAndTypeIndex)
    ++ (convertWord16ToListWord8 constantNameAndTypeDescriptorIndex)

constantToByte (CONSTANT_Utf8 utf8Value) =
    (constantTagToByte (CONSTANT_Utf8 utf8Value))
    ++ (convertWord16ToListWord8 (((fromIntegral . length) utf8) :: Word16))
    ++ utf8
    where utf8 = (unpackByteStringToListWord8((encodeUtf8 . T.pack) utf8Value)) -- string -> bytestring -> [Word8]

constantToByte (CONSTANT_MethodHandle referenceKind referenceIndex) =
    (constantTagToByte (CONSTANT_MethodHandle referenceKind referenceIndex))
    ++ (convertWord16ToListWord8 (referenceToByte referenceKind))
    ++ (convertWord16ToListWord8 referenceIndex)

constantToByte (CONSTANT_MethodType descriptorIndex) =
    (constantTagToByte (CONSTANT_MethodType descriptorIndex))
    ++ (convertWord16ToListWord8 descriptorIndex)


constantToByte (CONSTANT_InvokeDynamic bootstrapMethodAttributIndex nameAndTypeIndex) =
    (constantTagToByte (CONSTANT_InvokeDynamic bootstrapMethodAttributIndex nameAndTypeIndex))
    ++ (convertWord16ToListWord8 bootstrapMethodAttributIndex)
    ++ (convertWord16ToListWord8 nameAndTypeIndex)
