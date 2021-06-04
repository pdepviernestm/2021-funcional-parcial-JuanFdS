module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
   describe "Parcial" $ do
     it "Test de ejemplo" $ do
       2 + 2 `shouldBe` 4

escribime :: Expectation
escribime = implementame

