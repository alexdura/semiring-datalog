import Test.Tasty
import EvalSpec
import ContextSpec
import PicoJavaSpec

main = defaultMain $ testGroup "all" [evalTests, csvTests, contextTests, picoJavaTests]
