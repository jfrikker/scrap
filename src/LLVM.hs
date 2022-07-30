module LLVM (
  writeDefinitions,
  writeFunction
) where

import qualified Data.List.Index as List
import LLVM.AST (Definition(GlobalDefinition))
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Global as Global
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.Operand as Operand
import LLVM.AST.Operand (Operand)
import qualified LLVM.AST.Type as Type
import LLVM.IRBuilder
import qualified LLVM.IRBuilder.Constant as Constant
import qualified LLIR

writeDefinitions :: [(String, LLIR.Function)] -> [Definition]
writeDefinitions = map (uncurry writeFunction)

writeFunction :: String -> LLIR.Function -> Definition
writeFunction symbol (LLIR.Function params expr) = GlobalDefinition $ Global.functionDefaults {
  Global.name = Name.mkName symbol,
  Global.parameters = (params', False),
  Global.returnType = llvmType $ LLIR.dataType expr,
  Global.basicBlocks = body
  }
  where params' = List.imap (\i t -> Global.Parameter (llvmType t) (Name.mkName $ "param" ++ (show i)) []) params
        body = execIRBuilder emptyIRBuilder $ do
          result <- writeExpression expr
          ret result

writeExpression :: LLIR.Expression -> IRBuilder Operand
writeExpression (LLIR.Argument idx t) = return $ Operand.LocalReference (llvmType t) (Name.mkName $ "param" ++ (show idx))
writeExpression (LLIR.I64Literal val) = return $ Constant.int64 $ fromIntegral val
writeExpression (LLIR.Binary LLIR.Add a1 a2) = do
  a1' <- writeExpression a1
  a2' <- writeExpression a2
  add a1' a2'
writeExpression (LLIR.StaticFunctionCall name args _) = do
  args' <- mapM (\a -> writeExpression a >>= \a' -> return (a', [])) args
  call func args'
  where argTypes = map (\a -> (llvmType $ LLIR.dataType a, [])) args
        functionType = Type.FunctionType {
          Type.resultType = Type.i64,
          Type.argumentTypes = [],
          Type.isVarArg = False
          }
        func = Operand.ConstantOperand $ Constant.GlobalReference functionType $ Name.mkName name

llvmType :: LLIR.Type -> Type.Type
llvmType LLIR.I64Type = Type.i64
