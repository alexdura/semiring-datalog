import Test.Tasty
import EvalSpec
import ContextSpec
import PicoJavaSpec
import SaigaPicoJavaSpec

main = defaultMain $ testGroup "all" [evalTests, csvTests, contextTests, picoJavaTests, saigaPicoJavaTests]
