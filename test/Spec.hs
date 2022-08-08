import qualified StaticFunctionTemplateEliminationSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  StaticFunctionTemplateEliminationSpec.spec
