module Compiler.AstToClassFileTranslator.InstructionsToLength where
import Data.Word (Word8, Word16)
import Data.Int (Int8, Int16, Int32)
import Compiler.Instructions

getInstructionSize :: Instruction -> Word16
getInstructionSize (Aconst_Null) = 1
getInstructionSize (Aload _) = 2
getInstructionSize (Aload_0) = 1
getInstructionSize (Aload_1) = 1
getInstructionSize (Aload_2) = 1
getInstructionSize (Aload_3) = 1
getInstructionSize (Areturn) = 1
getInstructionSize (Astore _) = 2
getInstructionSize (Astore_0) = 1
getInstructionSize (Astore_1) = 1
getInstructionSize (Astore_2) = 1
getInstructionSize (Astore_3) = 1
getInstructionSize (Bipush _) = 2
getInstructionSize (Checkcast _) = 3
getInstructionSize (Dup) = 1
getInstructionSize (Dup_X1) = 1
getInstructionSize (Dup_X2) = 1
getInstructionSize (Dup2) = 1
getInstructionSize (Dup2_X1) = 1
getInstructionSize (Dup2_X2) = 1
getInstructionSize (Getfield _) = 3
getInstructionSize (Getstatic _) = 3
getInstructionSize (Goto _) = 3
getInstructionSize (Goto_W _) = 5
getInstructionSize (I2C) = 1
getInstructionSize (Iadd) = 1
getInstructionSize (Iand) = 1
getInstructionSize (Iconst_M1) = 1
getInstructionSize (Iconst_0) = 1
getInstructionSize (Iconst_1) = 1
getInstructionSize (Iconst_2) = 1
getInstructionSize (Iconst_3) = 1
getInstructionSize (Iconst_4) = 1
getInstructionSize (Iconst_5) = 1
getInstructionSize (Idiv) = 1
getInstructionSize (If_Acmpeq _) = 3
getInstructionSize (If_Acmpne _) = 3
getInstructionSize (If_Icmpeq _) = 3
getInstructionSize (If_Icmpge _) = 3
getInstructionSize (If_Icmpgt _) = 3
getInstructionSize (If_Icmple _) = 3
getInstructionSize (If_Icmplt _) = 3
getInstructionSize (If_Icmpne _) = 3
getInstructionSize (Ifeq _) = 3
getInstructionSize (Ifge _) = 3
getInstructionSize (Ifgt _) = 3
getInstructionSize (Ifle _) = 3
getInstructionSize (Iflt _) = 3
getInstructionSize (Ifne _) = 3
getInstructionSize (Ifnonnull _) = 3
getInstructionSize (Ifnull _) = 3
getInstructionSize (Iinc _ _) = 3
getInstructionSize (Iload _) = 2
getInstructionSize (Iload_0) = 2
getInstructionSize (Iload_1) = 2
getInstructionSize (Iload_2) = 2
getInstructionSize (Iload_3) = 2
getInstructionSize (Imul) = 1
getInstructionSize (Ineg) = 1
getInstructionSize (Instanceof _) = 3
getInstructionSize (Invokespecial _) = 3
getInstructionSize (Invokestatic _) = 3
getInstructionSize (Invokevirtual _) = 3
getInstructionSize (Ior) = 1
getInstructionSize (Irem) = 1
getInstructionSize (Ireturn) = 1
getInstructionSize (Ishl) = 1
getInstructionSize (Ishr) = 1
getInstructionSize (Istore _) = 2
getInstructionSize (Istore_0) = 1
getInstructionSize (Istore_1) = 1
getInstructionSize (Istore_2) = 1
getInstructionSize (Istore_3) = 1
getInstructionSize (Isub) = 1
getInstructionSize (Iushr) = 1
getInstructionSize (Ixor) = 1
getInstructionSize (Jsr _) = 3
getInstructionSize (Jsr_W _) = 5
getInstructionSize (Ldc _) = 2
getInstructionSize (Ldc_W _) = 3
-- getInstructionSize (Lookupswitch _ _ _ _ _ _ _ _ _) =  TODOOOO
getInstructionSize (New _) = 3
getInstructionSize (Nop) = 1
getInstructionSize (Pop) = 1
getInstructionSize (Pop2) = 1
getInstructionSize (Putfield _) = 3
getInstructionSize (Putstatic _) = 3
getInstructionSize (Ret _) = 2
getInstructionSize (Return) = 1
getInstructionSize (Swap) = 1
-- getInstructionSize (Tableswitch _ _ _ _ _ _ _ _ _ _ _ _ _) TODOOOOOO
getInstructionSize (Wide instr _) = 2 + (getInstructionSize instr)
getInstructionSize (WideIinc _ _) = 5



