import qualified NFC
import qualified NFD
import qualified NFKC
import qualified NFKD
import Test.Hspec

main :: IO ()
main = hspec $ do
    NFC.specs
    NFD.specs
    NFKC.specs
    NFKD.specs
