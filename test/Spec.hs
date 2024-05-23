import Test.Tasty
import EvalSpec
import ContextSpec
import PicoJavaSpec
import SaigaPicoJavaSpec
import SaigaDatalogSpec
import DemandTransformationSpec

main = defaultMain $ testGroup "all" [
  evalTests,
  csvTests,
  contextTests,
  picoJavaTests,
  saigaPicoJavaTests,
  saigaDatalogTests,
  demandTransformationTests]
