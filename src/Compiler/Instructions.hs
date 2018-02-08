module Compiler.Instructions where
import Data.Int (Int8, Int16, Int32)
import Data.Word (Word8, Word16)


type Instructions = [Instruction]

data Instruction =
    Aconst_Null
    {-
    OP: 0000 0001, 01
    Byte: None
    Stack: → null
    Description: <td>push a  reference onto the stack</td>
    -}
    | Aload
    {
    aload_index :: Word8
    }
    {-
    OP: 0001 1001, 19
    Byte: 1: index
    Stack: → objectref
    Description: <td>load a reference onto the stack from a local variable </td>
    -}
    | Aload_0
    {-
    OP: 0010 1010, 2a
    Byte: None
    Stack: → objectref
    Description: <td>load a reference onto the stack from local variable 0</td>
    -}
    | Aload_1
    {-
    OP: 0010 1011, 2b
    Byte: None
    Stack: → objectref
    Description: <td>load a reference onto the stack from local variable 1</td>
    -}
    | Aload_2
    {-
    OP: 0010 1100, 2c
    Byte: None
    Stack: → objectref
    Description: <td>load a reference onto the stack from local variable 2</td>
    -}
    | Aload_3
    {-
    OP: 0010 1101, 2d
    Byte: None
    Stack: → objectref
    Description: <td>load a reference onto the stack from local variable 3</td>
    -}
    | Areturn
    {-
    OP: 1011 0000, b0
    Byte: None
    Stack: objectref → [empty]
    Description: <td>return a reference from a method</td>
    -}
    | Astore
    {
    astore_index :: Word8
    }
    {-
    OP: 0011 1010, 3a
    Byte: 1: index
    Stack: objectref →
    Description: <td>store a reference into a local variable </td>
    -}
    | Astore_0
    {-
    OP: 0100 1011, 4b
    Byte: None
    Stack: objectref →
    Description: <td>store a reference into local variable 0</td>
    -}
    | Astore_1
    {-
    OP: 0100 1100, 4c
    Byte: None
    Stack: objectref →
    Description: <td>store a reference into local variable 1</td>
    -}
    | Astore_2
    {-
    OP: 0100 1101, 4d
    Byte: None
    Stack: objectref →
    Description: <td>store a reference into local variable 2</td>
    -}
    | Astore_3
    {-
    OP: 0100 1110, 4e
    Byte: None
    Stack: objectref →
    Description: <td>store a reference into local variable 3</td>
    -}
    | Bipush
    {
    bipush_byte :: Int8 -- TODO AENDERN
    }
    | Sipush
    {
      sipush_twoByte :: Int16 -- TODOOOOOO HINZUFÜGEN OFFIZIELL
    }
    {-
    OP: 0001 0000, 10
    Byte: 1: byte
    Stack: → value
    Description: <td>push a  onto the stack as an integer </td>
    -}
    | Checkcast
    {
    checkcast_indexbytes :: Word16
    }
    {- NOTE: Do we need this?
    OP: 1100 0000, c0
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref → objectref
    Description: <td>checks whether an  is of a certain type, the class reference of which is in the constant pool at  (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Dup
    {-
    OP: 0101 1001, 59
    Byte: None
    Stack: value → value, value
    Description: <td>duplicate the value on top of the stack</td>
    -}
    | Dup_X1
    {-
    OP: 0101 1010, 5a
    Byte: None
    Stack: value2, value1 → value1, value2, value1
    Description: <td>insert a copy of the top value into the stack two values from the top. value1 and value2 must not be of the type double or long.</td>
    -}
    | Dup_X2
    {-
    OP: 0101 1011, 5b
    Byte: None
    Stack: value3, value2, value1 → value1, value3, value2, value1
    Description: <td>insert a copy of the top value into the stack two (if value2 is double or long it takes up the entry of value3, too) or three values (if value2 is neither double nor long) from the top</td>
    -}
    | Dup2
    {-
    OP: 0101 1100, 5c
    Byte: None
    Stack: {value2, value1} → {value2, value1}, {value2, value1}
    Description: <td>duplicate top two stack words (two values, if value1 is not double nor long; a single value, if value1 is double or long)</td>
    -}
    | Dup2_X1
    {-
    OP: 0101 1101, 5d
    Byte: None
    Stack: value3, {value2, value1} → {value2, value1}, value3, {value2, value1}
    Description: <td>duplicate two words and insert beneath third word (see explanation above)</td>
    -}
    | Dup2_X2
    {-
    OP: 0101 1110, 5e
    Byte: None
    Stack: {value4, value3}, {value2, value1} → {value2, value1}, {value4, value3}, {value2, value1}
    Description: <td>duplicate two words and insert beneath fourth word</td>
    -}
    | Getfield
    {
    getfield_indexbytes :: Word16
    }
    {-
    OP: 1011 0100, b4
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref → value
    Description: <td>get a field  of an object , where the field is identified by field reference in the constant pool  (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Getstatic
    {
    getstatic_indexbytes :: Word16
    }
    {-
    OP: 1011 0010, b2
    Byte: 2: indexbyte1, indexbyte2
    Stack: → value
    Description: <td>get a static field  of a class, where the field is identified by field reference in the constant pool  (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Goto
    {
    goto_branchbytes :: Word16
    }
    {-
    OP: 1010 0111, a7
    Byte: 2: branchbyte1, branchbyte2
    Stack: [no change]
    Description: <td>goes to another instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Goto_W
    {
    goto_w_branchbytes :: Int32
    }
    {-
    OP: 1100 1000, c8
    Byte: 4: branchbyte1, branchbyte2, branchbyte3, branchbyte4
    Stack: [no change]
    Description: <td>goes to another instruction at  (signed int constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 24 + branchbyte2 &lt;&lt; 16 + branchbyte3 &lt;&lt; 8 + branchbyte4</span>)</td>
    -}
    | I2C
    {-
    OP: 1001 0010, 92
    Byte: None
    Stack: value → result
    Description: <td>convert an int into a character</td>
    -}
    | Iadd
    {-
    OP: 0110 0000, 60
    Byte: None
    Stack: value1, value2 → result
    Description: <td>add two ints</td>
    -}
    | Iand
    {-
    OP: 0111 1110, 7e
    Byte: None
    Stack: value1, value2 → result
    Description: <td>perform a bitwise AND on two integers</td>
    -}
    | Iconst_M1
    {-
    OP: 0000 0010, 02
    Byte: None
    Stack: → -1
    Description: <td>load the int value −1 onto the stack</td>
    -}
    | Iconst_0
    {-
    OP: 0000 0011, 03
    Byte: None
    Stack: → 0
    Description: <td>load the int value 0 onto the stack</td>
    -}
    | Iconst_1
    {-
    OP: 0000 0100, 04
    Byte: None
    Stack: → 1
    Description: <td>load the int value 1 onto the stack</td>
    -}
    | Iconst_2
    {-
    OP: 0000 0101, 05
    Byte: None
    Stack: → 2
    Description: <td>
     the int value 2 onto the stack</td>
    -}
    | Iconst_3
    {-
    OP: 0000 0110, 06
    Byte: None
    Stack: → 3
    Description: <td>load the int value 3 onto the stack</td>
    -}
    | Iconst_4
    {-
    OP: 0000 0111, 07
    Byte: None
    Stack: → 4
    Description: <td>load the int value 4 onto the stack</td>
    -}
    | Iconst_5
    {-
    OP: 0000 1000, 08
    Byte: None
    Stack: → 5
    Description: <td>load the int value 5 onto the stack</td>
    -}
    | Idiv
    {-
    OP: 0110 1100, 6c
    Byte: None
    Stack: value1, value2 → result
    Description: <td>divide two integers</td>
    -}
    | If_Acmpeq
    {
    if_acmpeq_branchbytes :: Int16
    }
    {-
    OP: 1010 0101, a5
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if references are equal, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Acmpne
    {
    if_acmpne_branchbytes :: Int16
    }
    {-
    OP: 1010 0110, a6
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if references are not equal, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmpeq
    {
    if_icmpeq_branchbytes :: Int16
    }
    {-
    OP: 1001 1111, 9f
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if ints are equal, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmpge
    {
    if_icmpge_branchbytes :: Word16
    }
    {-
    OP: 1010 0010, a2
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if  is greater than or equal to , branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmpgt
    {
    if_icmpgt_branchbytes :: Word16
    }
    {-
    OP: 1010 0011, a3
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if  is greater than , branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmple
    {
    if_icmple_branchbytes :: Word16
    }
    {-
    OP: 1010 0100, a4
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if  is less than or equal to , branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmplt
    {
    if_icmplt_branchbytes :: Word16
    }
    {-
    OP: 1010 0001, a1
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if  is less than , branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | If_Icmpne
    {
    if_icmpne_branchbytes :: Word16
    }
    {-
    OP: 1010 0000, a0
    Byte: 2: branchbyte1, branchbyte2
    Stack: value1, value2 →
    Description: <td>if ints are not equal, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifeq
    {
    ifeq_branchbytes :: Int16
    }
    {-
    OP: 1001 1001, 99
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifge
    {
    ifge_branchbytes :: Int16
    }
    {-
    OP: 1001 1100, 9c
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is greater than or equal to 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifgt
    {
    ifgt_branchbytes :: Int16
    }
    {-
    OP: 1001 1101, 9d
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is greater than 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifle
    {
    ifle_branchbytes :: Int16
    }
    {-
    OP: 1001 1110, 9e
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is less than or equal to 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Iflt
    {
    iflt_branchbytes :: Int16
    }
    {-
    OP: 1001 1011, 9b
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is less than 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifne
    {
    ifne_branchbytes :: Word16
    }
    {-
    OP: 1001 1010, 9a
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is not 0, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifnonnull
    {
    ifnonnull_branchbytes :: Int16
    }
    {-
    OP: 1100 0111, c7
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is not null, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Ifnull
    {
    ifnull_branchbytes :: Int16
    }
    {-
    OP: 1100 0110, c6
    Byte: 2: branchbyte1, branchbyte2
    Stack: value →
    Description: <td>if  is null, branch to instruction at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>)</td>
    -}
    | Iinc
    {
    iinc_index :: Word8,
    iinc_const :: Int8
    }
    {-
    OP: 1000 0100, 84
    Byte: 2: index, const
    Stack: [No change]
    Description: <td>increment local variable  by signed byte </td>
    -}
    | Iload
    {
    iload_index :: Word8
    }
    {-
    OP: 0001 0101, 15
    Byte: 1: index
    Stack: → value
    Description: <td>load an int  from a local variable </td>
    -}
    | Iload_0
    {-
    OP: 0001 1010, 1a
    Byte: None
    Stack: → value
    Description: <td>load an int  from local variable 0</td>
    -}
    | Iload_1
    {-
    OP: 0001 1011, 1b
    Byte: None
    Stack: → value
    Description: <td>load an int  from local variable 1</td>
    -}
    | Iload_2
    {-
    OP: 0001 1100, 1c
    Byte: None
    Stack: → value
    Description: <td>load an int  from local variable 2</td>
    -}
    | Iload_3
    {-
    OP: 0001 1101, 1d
    Byte: None
    Stack: → value
    Description: <td>load an int  from local variable 3</td>
    -}
    | Imul
    {-
    OP: 0110 1000, 68
    Byte: None
    Stack: value1, value2 → result
    Description: <td>multiply two integers</td>
    -}
    | Ineg
    {-
    OP: 0111 0100, 74
    Byte: None
    Stack: value → result
    Description: <td>negate int</td>
    -}
    | Instanceof
    {
    instanceof_indexbytes :: Word16
    }
    {-
    OP: 1100 0001, c1
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref → result
    Description: <td>determines if an object  is of a given type, identified by class reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Invokespecial
    {
    invokespecial_indexbytes :: Word16
    }
    {-
    OP: 1011 0111, b7
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref, [arg1, arg2, ...] → result
    Description: <td>invoke instance method on object  and puts the result on the stack (might be void); the method is identified by method reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Invokestatic
    {
    invokestatic_indexbytes :: Word16
    }
    {-
    OP: 1011 1000, b8
    Byte: 2: indexbyte1, indexbyte2
    Stack: [arg1, arg2, ...] → result
    Description: <td>invoke a static method and puts the result on the stack (might be void); the method is identified by method reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Invokevirtual -- TODO: Do we support this?
    {
    invokevirtual_indexbytes :: Word16
    }
    {-
    OP: 1011 0110, b6
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref, [arg1, arg2, ...] → result
    Description: <td>invoke virtual method on object  and puts the result on the stack (might be void); the method is identified by method reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Ior
    {-
    OP: 1000 0000, 80
    Byte: None
    Stack: value1, value2 → result
    Description: <td>bitwise int OR</td>
    -}
    | Irem
    {-
    OP: 0111 0000, 70
    Byte: None
    Stack: value1, value2 → result
    Description: <td>logical int remainder</td>
    -}
    | Ireturn
    {-
    OP: 1010 1100, ac
    Byte: None
    Stack: value → [empty]
    Description: <td>return an integer from a method</td>
    -}
    | Ishl
    {-
    OP: 0111 1000, 78
    Byte: None
    Stack: value1, value2 → result
    Description: <td>int shift left</td>
    -}
    | Ishr
    {-
    OP: 0111 1010, 7a
    Byte: None
    Stack: value1, value2 → result
    Description: <td>int arithmetic shift right</td>
    -}
    | Istore
    {
    istore_index :: Word8
    }
    {-
    OP: 0011 0110, 36
    Byte: 1: index
    Stack: value →
    Description: <td>store int  into variable </td>
    -}
    | Istore_0
    {-
    OP: 0011 1011, 3b
    Byte: None
    Stack: value →
    Description: <td>store int  into variable 0</td>
    -}
    | Istore_1
    {-
    OP: 0011 1100, 3c
    Byte: None
    Stack: value →
    Description: <td>store int  into variable 1</td>
    -}
    | Istore_2
    {-
    OP: 0011 1101, 3d
    Byte: None
    Stack: value →
    Description: <td>store int  into variable 2</td>
    -}
    | Istore_3
    {-
    OP: 0011 1110, 3e
    Byte: None
    Stack: value →
    Description: <td>store int  into variable 3</td>
    -}
    | Isub
    {-
    OP: 0110 0100, 64
    Byte: None
    Stack: value1, value2 → result
    Description: <td>int subtract</td>
    -}
    | Iushr
    {-
    OP: 0111 1100, 7c
    Byte: None
    Stack: value1, value2 → result
    Description: <td>int logical shift right</td>
    -}
    | Ixor
    {-
    OP: 1000 0010, 82
    Byte: None
    Stack: value1, value2 → result
    Description: <td>int xor</td>
    -}
    | Jsr
    {
    jsr_branchbytes :: Int16
    }
    {-
    OP: 1010 1000, a8
    Byte: 2: branchbyte1, branchbyte2
    Stack: → address
    Description: <td>jump to subroutine at  (signed short constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 8 + branchbyte2</span>) and place the return address on the stack</td>
    -}
    | Jsr_W
    {
    jsr_w_branchbytes :: Int32
    }
    {-
    OP: 1100 1001, c9
    Byte: 4: branchbyte1, branchbyte2, branchbyte3, branchbyte4
    Stack: → address
    Description: <td>jump to subroutine at  (signed int constructed from unsigned bytes <span class="monospaced" style="font-family: monospace, monospace;">branchbyte1 &lt;&lt; 24 + branchbyte2 &lt;&lt; 16 + branchbyte3 &lt;&lt; 8 + branchbyte4</span>) and place the return address on the stack</td>
    -}
    | Ldc
    {
    ldc_index :: Word8
    }
    {-
    OP: 0001 0010, 12
    Byte: 1: index
    Stack: → value
    Description: <td>push a constant  from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, or java.lang.invoke.MethodHandle) onto the stack</td>
    -}
    | Ldc_W
    {
    ldc_w_indexbytes :: Word16
    }
    {-
    OP: 0001 0011, 13
    Byte: 2: indexbyte1, indexbyte2
    Stack: → value
    Description: <td>push a constant  from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, or java.lang.invoke.MethodHandle) onto the stack (wide  is constructed as <span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Lookupswitch -- NOTE: Do we need this?
    {
    lookupswitch_defaultbyte1 :: Int,
    lookupswitch_defaultbyte2 :: Int,
    lookupswitch_defaultbyte3 :: Int,
    lookupswitch_defaultbyte4 :: Int,
    lookupswitch_npairs1 :: Int,
    lookupswitch_npairs2 :: Int,
    lookupswitch_npairs3 :: Int,
    lookupswitch_npairs4 :: Int,
    lookupswitch_match_offset_pairs :: Int
    }
    {-
    OP: 1010 1011, ab
    Byte: 8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...
    Stack: key →
    Description: <td>a target address is looked up from a table using a key and execution continues from the instruction at that address</td>
    -}
    | New
    {
    new_indexbytes :: Word16
    }
    {-
    OP: 1011 1011, bb
    Byte: 2: indexbyte1, indexbyte2
    Stack: → objectref
    Description: <td>create new object of type identified by class reference in constant pool  (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Nop
    {-
    OP: 0000 0000, 00
    Byte: None
    Stack: [No change]
    Description: <td>perform no operation</td>
    -}
    | Pop
    {-
    OP: 0101 0111, 57
    Byte: None
    Stack: value →
    Description: <td>discard the top value on the stack</td>
    -}
    | Pop2
    {-
    OP: 0101 1000, 58
    Byte: None
    Stack: {value2, value1} →
    Description: <td>discard the top two values on the stack (or one value, if it is a double or long)</td>
    -}
    | Putfield
    {
    putfield_indexbytes :: Word16
    }
    {-
    OP: 1011 0101, b5
    Byte: 2: indexbyte1, indexbyte2
    Stack: objectref, value →
    Description: <td>set field to  in an object , where the field is identified by a field reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Putstatic
    {
    putstatic_indexbytes :: Word16
    }
    {-
    OP: 1011 0011, b3
    Byte: 2: indexbyte1, indexbyte2
    Stack: value →
    Description: <td>set static field to  in a class, where the field is identified by a field reference  in constant pool (<span class="monospaced" style="font-family: monospace, monospace;">indexbyte1 &lt;&lt; 8 + indexbyte2</span>)</td>
    -}
    | Ret -- NOTE: Figure out what it is used for
    {
    ret_index :: Word8
    }
    {-
    OP: 1010 1001, a9
    Byte: 1: index
    Stack: [No change]
    Description: <td>continue execution from address taken from a local variable  (the asymmetry with jsr is intentional)</td>
    -}
    | Return
    {-
    OP: 1011 0001, b1
    Byte: None
    Stack: → [empty]
    Description: <td>return void from method</td>
    -}
    | Swap
    {-
    OP: 0101 1111, 5f
    Byte: None
    Stack: value2, value1 → value1, value2
    Description: <td>swaps two top words on the stack (note that value1 and value2 must not be double or long)</td>
    -}
    | Tableswitch
    {
    tableswitch_defaultbyte1 :: Int,
    tableswitch_defaultbyte2 :: Int,
    tableswitch_defaultbyte3 :: Int,
    tableswitch_defaultbyte4 :: Int,
    tableswitch_lowbyte1 :: Int,
    tableswitch_lowbyte2 :: Int,
    tableswitch_lowbyte3 :: Int,
    tableswitch_lowbyte4 :: Int,
    tableswitch_highbyte1 :: Int,
    tableswitch_highbyte2 :: Int,
    tableswitch_highbyte3 :: Int,
    tableswitch_highbyte4 :: Int,
    tableswitch_jump_offsets :: Int
    }
    {-
    OP: 1010 1010, aa
    Byte: 16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...
    Stack: index →
    Description: <td>continue execution from an address in the table at offset </td>
    -}
    | Wide -- TODO: Fix this and other one byte values
    {
      wide_instruction :: Instruction,
      wide_index_bytes :: Word16
    }
    {-
    OP: 1100 0100, c4
    Byte: None --TODO
    Stack: [same as for corresponding instructions]
    Description: <td>execute , where  is either iload, fload, aload, lload, dload, istore, fstore, astore, lstore, dstore, or ret, but assume the  is 16 bit; or execute iinc, where the  is 16 bits and the constant to increment by is a signed 16 bit short</td>
    -}
    | WideIinc
    {
      wide_iinc_indexbytes :: Word16,
      wide_iinc_countbytes :: Int16
    }
    {-
      Combination of wide and iinc
    -}
    deriving (Eq, Show)
