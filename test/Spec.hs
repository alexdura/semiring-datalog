import Test.Tasty
import EvalSpec

main = defaultMain $ testGroup "all" [evalTests, csvTests]
