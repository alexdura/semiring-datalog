import Test.Tasty
import EvalSpec
import ContextSpec
import PicoJavaSpec
import SaigaPicoJavaSpec
import SaigaToDatalogTranslationSpec
import DemandTransformationSpec
import DatalogPicoJavaSpec
import SaigaCFGLangSpec

main = defaultMain $ testGroup "all" [
  evalTests,
  csvTests,
  contextTests,
  picoJavaTests,
  saigaPicoJavaTests,
  saigaDatalogTests,
  demandTransformationTests,
  datalogPicoJavaTests,
  saigaCFGLangTests
  ]
