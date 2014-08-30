import Test.Framework (defaultMain)
import qualified Flow as F
import qualified Routing as R

main = defaultMain $ F.tests ++ R.tests
