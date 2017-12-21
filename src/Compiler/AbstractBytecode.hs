module Compiler.AbstractBytecode where

import Data.Int (Int16, Int32, Int64)

data ClassFile = ClassFile
  { -- | Should be initialized as 0xCAFEBABE
    magic :: Magic
    -- | Sets the version of the classfile (will be constant)
  , minor_version :: MinorVersion
  , major_version :: MajorVersion
  -- | Constantenpool
  , constant_pool :: ConstantPool
  , classFileAccessFlags :: ClassFileAccessFlags
  , this :: ThisClass
  , super :: SuperClass
  -- | Interfaces
  , interfaces :: Interfaces
  , fields :: Fields -- Fields
  , methods :: Methods -- Methoden
  , attributes :: Attributes -- Attribute
  }

data Magic = Magic

data MinorVersion = MinorVersion

data MajorVersion = MajorVersion

type ConstantPool = [Constant]

data Constant =
    CONSTANT_Class{
      constant_class_name_index :: Index_Constant_Pool
    }
  | CONSTANT_FieldRef{
      class_index :: Index_Constant_Pool
    , name_and_type_index :: Index_Constant_Pool
    }
  | CONSTANT_MethodRef{
      class_index :: Index_Constant_Pool
    , name_and_type_index :: Index_Constant_Pool
    }
  | CONSTANT_InterfaceMethodRef{
      class_index :: Index_Constant_Pool
    , name_and_type_index :: Index_Constant_Pool
    }
  | CONSTANT_String{
      string_index :: Index_Constant_Pool
    }
  | CONSTANT_Integer{
      int_value :: Int32
    }
  | CONSTANT_Float{
      float_value :: Float
    }
  | CONSTANT_Long{
      long_value :: Int64
    }
  | CONSTANT_Double{
      double_value :: Double
    }
  | CONSTANT_NameAndType{
      constnat_name_and_type_name_index :: Index_Constant_Pool
    , constnat_name_and_type_descriptor_index :: Index_Constant_Pool
    }
  | CONSTANT_Utf8{
      utf8_value :: String
      --NOTE: Can calc length
      -- difference in UTF8 format to java
    }
  | CONSTANT_MethodHandle{
      reference_kind :: Reference
    , reference_index :: Index_Constant_Pool
    }
  | CONSTANT_MethodType{
     descriptor_index :: Index_Constant_Pool
    }
  | CONSTANT_InvokeDynamic{
      bootstrap_method_attribute_index :: Index_Bootstrap_Methods
    , name_and_type_index :: Index_Constant_Pool
    }

type Index_Constant_Pool = Int64 -- Maybe Data

data Reference = GetField
                |GetStatic
                |PutField
                |PutStatic
                |InvokeVirtual
                |InvokeStatic
                |InvokeSpecial
                |NewInvokeSpecial
                |InvokeInterface

type Index_Bootstrap_Methods = Int64 -- Maybe Data

type ClassFileAccessFlags = [ClassFileAccessFlag]

data ClassFileAccessFlag = PUBLIC
                  |FINAL
                  |SUPER
                  |INTERFACE
                  |ABSTRACT
                  |SYNTHETIC
                  |ANNOTATION
                  |ENUM

data ThisClass = ThisClass{
                this_class_index :: Index_Constant_Pool
                }

data SuperClass = SuperClass{
                super_class_index :: Maybe Index_Constant_Pool
                }

type Interfaces = [Interface]

data Interface = Interface -- Maybe TODO

type Fields = [Field]

data Field = Field{
    field_access_flags :: FieldAccessFlags
  , field_name_index :: Index_Constant_Pool -- name_index
  , field_descriptor_index :: Index_Constant_Pool -- descriptor_index
  , field_attributes :: Attributes
  }

type FieldAccessFlags = [FieldAccessFlag]

data FieldAccessFlag = F_PUBLIC
                    |F_PRIVATE
                    |F_PROTECTED
                    |F_STATIC
                    |F_FINAL
                    |F_VOLATILE
                    |F_TRANSIENT
                    |F_SYNTHETIC
                    |F_ENUM

type Methods = [Method]

data Method = Method{
    method_access_flags :: MethodAccessFlags
  , method_name_index :: Index_Constant_Pool -- name_index
  , method_descriptor_index :: Index_Constant_Pool -- descriptor_index
  , method_attributes :: Attributes
  }

type MethodAccessFlags = [MethodAccessFlag]

data MethodAccessFlag = M_PUBLIC
                      |M_PRIVATE
                      |M_PROTECTED
                      |M_STATIC
                      |M_FINAL
                      |M_SYNCHRONIZED
                      |M_BRIDGE
                      |M_VARARGS
                      |M_NATIVE
                      |M_ABSTRACT
                      |M_STRICT
                      |M_SYNTHETIC

type Attributes = [Attribute]

data Attribute =
    ConstantValue {
     attribute_name_index :: Index_Constant_Pool
   , attribute_constantvalue_index :: Index_Constant_Pool
   }
   |Code {
      attribute_name_index :: Index_Constant_Pool -- attribute_name_index
    , max_stack :: Int16 -- max_stack
    , max_locals :: Int16 -- max_local
    , code :: Instructions
    , exception_tables :: ExceptionTables
    , code_attributes :: Attributes
  }
  |StackMapTable {
    attribute_name_index :: Index_Constant_Pool
  , stack_map_frame :: Entries
  }
  | InnerClass {
    inner_class_name_index :: Index_Constant_Pool
  , classes :: Classes
  }
  | Synthetic {
    synthetic_name_index :: Index_Constant_Pool
  }
  | SourceFile {
    source_file_name_index :: Index_Constant_Pool
  , sourcefile_index :: Index_Constant_Pool
  }
  | LineNumberTable {
    line_number_table_name_index :: Index_Constant_Pool
  , line_number_table :: LineNumberTable
  }
  | LocalVariableTable{
    local_variable_table_name_index :: Index_Constant_Pool
  , local_variable_table :: LocalVariableTable
  }
  | Deprecated {
    deprecated_name_index :: Index_Constant_Pool
  }

data Instructions = Aaload
                  | Bipush --TODO

type ExceptionTables = [ExceptionTable]

data ExceptionTable = Exception --Maybe TODO

type Entries = [Entry]

data Entry = SameFrame
            | SameLocals1StackItemFrame {
              stack :: UnionStack
            }
            | SameLocals1StackItemFrameExtended {
              offset_delta :: Int16
            , sameLocals1_stack :: UnionStack
            }
            |ChopFrame {
              offset_delta :: Int16
            }
            |SameFrameExtended {
              offset_delta :: Int16
            }
            |AppendFrame {
              offset_delta :: Int16
            , appendFrame_locals :: UnionLocals
            }
            |FullFrame {
              offset_delta :: Int16
            , locals :: UnionLocals
            , stack :: UnionLocals
            }


type UnionLocals = [Local]
type UnionStack = [StackItem]
type Local = VerificationType
type StackItem = VerificationType
data VerificationType = ItemTop
                      | ItemInteger
                      | ItemFoat
                      | ItemLong
                      | ItemDouble
                      | ItemNull
                      | ItemUninitializedThis
                      | ItemObject {
                        constantvalue_index :: Index_Constant_Pool
                      }
                      | ItemUninitialized {
                        offset :: Int16
                      }

type Classes = [Class]

data Class = Class {
                    inner_class_info_index :: Index_Constant_Pool
                  , outer_class_info_index :: Maybe Index_Constant_Pool
                  , inner_name_index :: Maybe Index_Constant_Pool
                  , inner_class_access_flags :: InnerClassAccessFlags
                  }

type InnerClassAccessFlags = [InnerClassAccessFlag]

data InnerClassAccessFlag = IC_PUBLIC
                          | IC_PRIVATE
                          | IC_PROTECTED
                          | IC_STATIC
                          | IC_FINAL
                          | IC_INTERFACE
                          | IC_ABSTRACT
                          | IC_SYNTHETIC
                          | IC_ANNOTATION
                          | IC_ENUM

type LineNumberTable = [LineNumber]

data LineNumber = LineNumber {
                    line_number_start_pc :: Int16
                  , line_number :: Int16
                  }

type LocalVariableTable = [LocalVariable]

data LocalVariable = LocalVariable {
                      local_variable_start_pc :: Int16
                    , local_variable_name_index :: Index_Constant_Pool
                    , local_variable_descriptor_index :: Index_Constant_Pool
                    , index :: Int -- FIXME figure out something
                    }
