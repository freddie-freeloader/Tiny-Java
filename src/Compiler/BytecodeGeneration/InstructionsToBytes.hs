module Compiler.BytecodeGeneration.InstructionsToBytes(instructionsToByte) where
import Data.Word (Word8, Word16)
import Data.Int (Int16, Int32)
import Compiler.Instructions
import Compiler.BytecodeGeneration.ToByteUtil

-- | Translate a list of instructions to bytecode that can be interpreted by the JVM
instructionsToByte :: [Instruction] -> [Word8]
instructionsToByte xs = foldr (\x acc -> instructionToByte x ++ acc) [] xs


-- | Translate a instruction with parameter to bytecode representation
instructionToByte :: Instruction -> [Word8]
instructionToByte Aconst_Null = 0x00 : []
instructionToByte (Aload index) = 0x19 : index : []
instructionToByte Aload_0 = 0x2a : []
instructionToByte Aload_1 = 0x2b : []
instructionToByte Aload_2 = 0x2c : []
instructionToByte Aload_3 = 0x2d : []
instructionToByte Areturn = 0xb0 : []
instructionToByte (Astore index) = 0x3a : index : []
instructionToByte Astore_0 = 0x4b : []
instructionToByte Astore_1 = 0x4c : []
instructionToByte Astore_2 = 0x4d : []
instructionToByte Astore_3 = 0x4e : []
instructionToByte (Bipush byte) = 0x10 : byte : []
instructionToByte (Checkcast index) = 0xc0 : convertWord16ToListWord8 index
instructionToByte Dup = 0x59 : []
instructionToByte Dup_X1 = 0x5a : []
instructionToByte Dup_X2 = 0x5b : []
instructionToByte Dup2 = 0x5c : []
instructionToByte Dup2_X1 = 0x5d : []
instructionToByte Dup2_X2 = 0x5e : []
instructionToByte (Getfield index) = 0xb4 : convertWord16ToListWord8 index
instructionToByte (Getstatic index) = 0xb2 : convertWord16ToListWord8 index
instructionToByte (Goto branch) = 0xa7 : convertInt16ToListWord8 branch
instructionToByte (Goto_W branch) = 0xc8 : convertInt32ToListWord8 branch
instructionToByte I2C = 0x92 : []
instructionToByte Iadd = 0x60 : []
instructionToByte Iand = 0x7e : []
instructionToByte Iconst_M1 = 0x02 : []
instructionToByte Iconst_0 = 0x03 : []
instructionToByte Iconst_1 = 0x04 : []
instructionToByte Iconst_2 = 0x05 : []
instructionToByte Iconst_3 = 0x06 : []
instructionToByte Iconst_4 = 0x07 : []
instructionToByte Iconst_5 = 0x08 : []
instructionToByte Idiv = 0x6c : []
instructionToByte (If_Acmpeq branch) = 0xa5 : convertInt16ToListWord8 branch
instructionToByte (If_Acmpne branch) = 0xa6 : convertInt16ToListWord8 branch
instructionToByte (If_Icmpeq branch) = 0x9f : convertInt16ToListWord8 branch
instructionToByte (If_Icmpge branch) = 0xa2 : convertInt16ToListWord8 branch
instructionToByte (If_Icmpgt branch) = 0xa3 : convertInt16ToListWord8 branch
instructionToByte (If_Icmple branch) = 0xa4 : convertInt16ToListWord8 branch
instructionToByte (If_Icmplt branch) = 0xa1 : convertInt16ToListWord8 branch
instructionToByte (If_Icmpne branch) = 0xa0 : convertInt16ToListWord8 branch
instructionToByte (Ifeq branch) = 0x99 : convertInt16ToListWord8 branch
instructionToByte (Ifge branch) = 0x9c : convertInt16ToListWord8 branch
instructionToByte (Ifgt branch) = 0x9d : convertInt16ToListWord8 branch
instructionToByte (Ifle branch) = 0x9e : convertInt16ToListWord8 branch
instructionToByte (Iflt branch) = 0x9b : convertInt16ToListWord8 branch
instructionToByte (Ifne branch) = 0x9a : convertInt16ToListWord8 branch
instructionToByte (Ifnonnull branch) = 0x9a : convertInt16ToListWord8 branch
instructionToByte (Ifnull branch) = 0xc6 : convertInt16ToListWord8 branch
instructionToByte (Iinc index count) = 0xc6 : index : fromIntegral count : []
instructionToByte (Iload index) = 0x15 : index : []
instructionToByte Iload_0 = 0x1a : []
instructionToByte Iload_1 = 0x1b : []
instructionToByte Iload_2 = 0x1c : []
instructionToByte Iload_3 = 0x1d : []
instructionToByte Imul = 0x68 : []
instructionToByte Ineg = 0x74 : []
instructionToByte (Instanceof index) = 0xc1 : convertWord16ToListWord8 index
instructionToByte (Invokespecial index) = 0xb7 : convertWord16ToListWord8 index
instructionToByte (Invokestatic index) = 0xb8 : convertWord16ToListWord8 index
instructionToByte (Invokevirtual index) = 0xb6 : convertWord16ToListWord8 index
instructionToByte Ior = 0x80 : []
instructionToByte Irem = 0x70 : []
instructionToByte Ireturn = 0xac : []
instructionToByte Ishl = 0x78 : []
instructionToByte Ishr = 0x7a : []
instructionToByte (Istore index) = 0x36 : index : []
instructionToByte Istore_0 = 0x3b : []
instructionToByte Istore_1 = 0x3c : []
instructionToByte Istore_2 = 0x3d : []
instructionToByte Istore_3 = 0x3e : []
instructionToByte Isub = 0x64 : []
instructionToByte Iushr = 0x7c : []
instructionToByte Ixor = 0x82 : []
instructionToByte (Jsr branch) = 0xa8 : convertInt16ToListWord8 branch
instructionToByte (Jsr_W branch) = 0xc9 : convertInt32ToListWord8 branch
instructionToByte (Ldc index) = 0x12 : index : []
instructionToByte (Ldc_W index) = 0x13 : convertWord16ToListWord8 index
instructionToByte (Lookupswitch _ _ _ _ _ _ _ _ _) = 0x00 : [] -- TODO: Empty, should this stay here
instructionToByte (New index) = 0xbb : convertWord16ToListWord8 index
instructionToByte Nop = 0x00 : []
instructionToByte Pop = 0x57 : []
instructionToByte Pop2 = 0x58 : []
instructionToByte (Putfield index) = 0xb5 : convertWord16ToListWord8 index
instructionToByte (Putstatic index) = 0xb3 : convertWord16ToListWord8 index
instructionToByte (Ret index) = 0xa9 : index : []
instructionToByte Return = 0xb1 : []
instructionToByte Swap = 0x5f : []
instructionToByte (Tableswitch _ _ _ _ _ _ _ _ _ _ _ _ _) = 0x00 : [] -- TODO: Empty, should this stay here
instructionToByte (Wide ins index) = 0xc4 : (head . instructionToByte) ins : convertWord16ToListWord8 index
instructionToByte (WideIinc index count) = 0xc4 : (head . instructionToByte) (Iinc 0 0) : (convertWord16ToListWord8 index) ++ (convertInt16ToListWord8 count)
