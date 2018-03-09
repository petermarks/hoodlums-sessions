import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector as V

import Lib

instance Arbitrary Simple where
    arbitrary = arbitraryBoundedEnum

main :: IO ()
main = hspec $ do
    describe "Lib.jumpSpecial" $ do
        it "works for an example case" $ do
            let t = ['a', 'b', 'c', capsLock, 'd', 'e', backspace, capsLock, capsLock, 'f']
            jumpSpecial t 0 `shouldBe` (0, False) -- 'a'
            jumpSpecial t 1 `shouldBe` (1, False) -- 'b'
            jumpSpecial t 2 `shouldBe` (2, False) -- 'c'
            jumpSpecial t 3 `shouldBe` (2, True)  -- 'c'
            jumpSpecial t 4 `shouldBe` (4, False) -- 'd'
            jumpSpecial t 5 `shouldBe` (5, False) -- 'e'
            jumpSpecial t 6 `shouldBe` (4, False) -- 'd'
            jumpSpecial t 7 `shouldBe` (4, True)  -- 'd'
            jumpSpecial t 8 `shouldBe` (4, False) -- 'd'
            jumpSpecial t 9 `shouldBe` (9, False) -- 'f'

        it "works for corner cases" $ do
            jumpSpecial [BS, BS, BS, BS] 3 `shouldBe` (-1, False)
            jumpSpecial [BS, BS, CapsLock, BS] 3 `shouldBe` (-1, True)
            jumpSpecial [CapsLock, BS, CapsLock, BS] 3 `shouldBe` (-1, False)

    describe "Lib.areEqual" $ do
        it "works for trivial cases without special commands" $ do
            areEqual ([] :: V.Vector Simple) [] `shouldBe` True
            areEqual [Upper] [Upper] `shouldBe` True
            areEqual [Lower] [Upper] `shouldBe` False

        it "(==) implies areEqual" $ do
            property $ \xs ->
                let v = V.fromList (xs :: [Simple])
                in areEqual v v `shouldBe` True

        it "works for a simple case with backspace" $
            areEqual [Upper, Upper, BS, Lower] [Upper, Lower] `shouldBe` True

        it "works for a simple case with capslock" $
            areEqual [Upper, Upper, CapsLock, Lower] [Upper, Upper, Upper] `shouldBe` True