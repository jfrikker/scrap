module Joe.LLVM (
  writeGlobal
) where

import qualified Data.List as List
import qualified Joe.LLIR as LLIR
import qualified Joe.Prim as Prim
import LLVM.AST (Definition(GlobalDefinition))
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import LLVM.AST.Operand (Operand)
import qualified LLVM.AST.Type as Type
import LLVM.IRBuilder
import qualified LLVM.IRBuilder.Constant as Constant

writeGlobal :: String -> LLIR.Global -> Definition
writeGlobal name func@(LLIR.Global params returnType expr) = GlobalDefinition $ Global.functionDefaults {
  Global.name = name',
  Global.parameters = (params', False),
  Global.returnType = llvmType returnType,
  Global.basicBlocks = body
  }
  where name' = Name.mkName name
        params' = List.map (\(name, t) -> Global.Parameter (llvmType t) (Name.mkName $ name) []) params
        body = execIRBuilder emptyIRBuilder $ do
          result <- writeExpression expr
          ret result

writeExpression :: LLIR.Expression -> IRBuilder Operand
writeExpression (LLIR.LocalReference name t) = return $ Operand.LocalReference (llvmType t) (Name.mkName name)
writeExpression (LLIR.I64Literal val) = return $ Constant.int64 $ fromIntegral val
writeExpression (LLIR.Binary Prim.Add a1 a2) = do
  a1' <- writeExpression a1
  a2' <- writeExpression a2
  add a1' a2'
writeExpression (LLIR.Call (LLIR.GlobalReference name _) args) = do
  args' <- mapM (\a -> writeExpression a >>= \a' -> return (a', [])) args
  call func args'
  where argTypes = map (\a -> (llvmType $ LLIR.dataType a, [])) args
        functionType = Type.FunctionType {
          Type.resultType = Type.i64,
          Type.argumentTypes = [],
          Type.isVarArg = False
          }
        func = Operand.ConstantOperand $ Constant.GlobalReference functionType $ Name.mkName name
writeExpression e = error $ "Unable to write expression " ++ show e

llvmType :: LLIR.Type -> Type.Type
llvmType LLIR.I64Type = Type.i64
llvmType t = error $ "Unable to compute llvm data type for " ++ show t
