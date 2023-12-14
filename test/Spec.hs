import Test.Tasty
import EvalSpec
import ContextSpec

main = defaultMain $ testGroup "all" [evalTests, csvTests, contextTests]
