module Compiler.AbstractBytecode where
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8,Word16,Word32,Word64)
import Compiler.Instructions

data ClassFile = ClassFile
  { magic :: Magic, -- CAFEBABE
    minver :: MinorVersion, -- Versionen
    maxver :: MajorVersion,
    array_cp :: ConstantPool, -- Konstantenpool
    classFileAccessFlags :: ClassFileAccessFlags, -- Berechtigungen
    this :: ThisClass, -- This-Klasse
    super :: SuperClass, -- Super-Klasse
    array_interfaces :: Interfaces, -- Interfaces
    array_fields :: Fields, -- Fields
    array_methods :: Methods, -- Methoden
    array_attributes :: Attributes -- Attribute
  } deriving (Eq, Show)

data Magic = Magic
 deriving (Eq, Show)

data MinorVersion = MinorVersion
 deriving (Eq, Show)

data MajorVersion = MajorVersion
 deriving (Eq, Show)

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
      constant_name_and_type_name_index :: Index_Constant_Pool
    , constant_name_and_type_descriptor_index :: Index_Constant_Pool
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
    }  deriving (Eq, Show)


type Index_Constant_Pool = Word16

data Reference = GetField
                |GetStatic
                |PutField
                |PutStatic
                |InvokeVirtual
                |InvokeStatic
                |InvokeSpecial
                |NewInvokeSpecial
                |InvokeInterface
                 deriving (Eq, Show)

type Index_Bootstrap_Methods = Word16

type ClassFileAccessFlags = [ClassFileAccessFlag]

data ClassFileAccessFlag = PUBLIC
                  |FINAL
                  |SUPER
                  |INTERFACE
                  |ABSTRACT
                  |SYNTHETIC
                  |ANNOTATION
                  |ENUM
                         deriving (Eq, Show)

data ThisClass = ThisClass{
                this_class_index :: Index_Constant_Pool
                } deriving (Eq, Show)

data SuperClass = SuperClass{
                super_class_index :: Index_Constant_Pool
                }  deriving (Eq, Show)

type Interfaces = [Interface]

data Interface = Interface -- Maybe TODO
  deriving (Eq, Show)

type Fields = [Field]

data Field = Field{
    field_access_flags :: FieldAccessFlags
  , field_name_index :: Index_Constant_Pool -- name_index
  , field_descriptor_index :: Index_Constant_Pool -- descriptor_index
  , field_attributes :: Attributes
  } deriving (Eq, Show)

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
                     deriving (Eq, Show)

type Methods = [Method]

data Method = Method{
    method_access_flags :: MethodAccessFlags
  , method_name_index :: Index_Constant_Pool -- name_index
  , method_descriptor_index :: Index_Constant_Pool -- descriptor_index
  , method_attributes :: Attributes
  } deriving (Eq, Show)

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
                      deriving (Eq, Show)

type Attributes = [Attribute]

data Attribute = -- Maybe TODO extend
    ConstantValue {
     constant_value_name_index :: Index_Constant_Pool
   , constant_value_constantvalue_index :: Index_Constant_Pool
   }
   |Code {
      code_name_index :: Index_Constant_Pool -- attribute_name_index
    , max_stack :: Word16 -- max_stack
    , max_locals :: Word16 -- max_local
    , code :: Instructions
    , exception_tables :: ExceptionTables
    , attributes :: Attributes
    }
    |StackMapTable {
      stack_map_name_index :: Index_Constant_Pool
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
    } deriving (Eq, Show)

type ExceptionTables = [ExceptionTable]

data ExceptionTable = Exception --Maybe TODO
 deriving (Eq, Show)

type Entries = [Entrie]

data Entrie = SameFrame
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
            } deriving (Eq, Show)


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
                       deriving (Eq, Show)

type Classes = [Class]

data Class = Class {
                    inner_class_info_index :: Index_Constant_Pool
                  , outer_class_info_index :: Index_Constant_Pool
                  , inner_name_index :: Index_Constant_Pool
                  , inner_class_access_flags :: InnerClassAccessFlags
                  } deriving (Eq, Show)

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
                          deriving (Eq, Show)

type LineNumberTable = [LineNumber]

data LineNumber = LineNumber {
                    line_number_start_pc :: Word16
                  , line_number :: Word16
                  } deriving (Eq, Show)

type LocalVariableTable = [LocalVariable]

data LocalVariable = LocalVariable {
                      local_variable_start_pc :: Word16
                    , local_variable_name_index :: Index_Constant_Pool
                    , local_variable_descriptor_index :: Index_Constant_Pool
                    , local_variable_index :: Word16 -- is index of the LocalVariableTable
                    } deriving (Eq, Show)
