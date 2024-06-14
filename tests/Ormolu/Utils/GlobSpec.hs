module Ormolu.Utils.GlobSpec (spec) where

import Ormolu.Utils.Glob (matchesGlob, mkGlob)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), listOf, property, suchThat)

newtype Wildcardless = Wildcardless String
    deriving (Show)

instance Arbitrary Wildcardless where
    arbitrary = fmap Wildcardless $ listOf (arbitrary `suchThat` (/= '*'))

newtype ModuleName = ModuleName String
    deriving (Show)

instance Arbitrary ModuleName where
    arbitrary = fmap ModuleName $ listOf (arbitrary `suchThat` (/= '.')) `suchThat` (not . null)

spec :: Spec
spec =
    describe "glob" $ do
        it "should match modules exactly" $
            "Hello.world" `matchesGlob` mkGlob "Hello.world" `shouldBe` True
        it "should fail if case doesn't match" $
            "Some.thing" `matchesGlob` mkGlob "Some.Thing" `shouldBe` False
        it "should allow * to match anything on the same level" $
            "Level1.Level2.Level3" `matchesGlob` mkGlob "Level1.Le*2.Level3" `shouldBe` True
        it "should fail if input doesn't match glob even with wildcards" $
            "Control.Monad.Writer.Lazy" `matchesGlob` mkGlob "Data.Functor.*.Lazy" `shouldBe` False
        it "should disallow * to match anything on different level" $
            "Level1.Level2.Level3.Level4" `matchesGlob` mkGlob "Level1.*.Level4" `shouldBe` False
        it "should allow multiple *" $
            "Some.Fake.Path.With.Internals" `matchesGlob` mkGlob "Some.*.Path.*.Internals" `shouldBe` True
        it "should allow * at the beginning" $
            "MyApp42.Control.Monad" `matchesGlob` mkGlob "*42.Control.Monad" `shouldBe` True
        it "should allow * at the end" $
            "System.IO.Stuff" `matchesGlob` mkGlob "System.IO.*" `shouldBe` True
        it "should allow ** to match anything on the same level" $
            "Level1.Level2.Level3" `matchesGlob` mkGlob "Level1.L**2.Level3" `shouldBe` True
        it "should allow ** to match anything on multiple levels" $
            "Level1.Level2.Level3.Level4.Level5" `matchesGlob` mkGlob "Level1.L**4.Level5" `shouldBe` True

        describe "Properties" $ do
            it "should obey: s `matches` glob s with s being any string without '*'" $
                property $
                    \(Wildcardless s) -> s `matchesGlob` mkGlob s
            it "should obey: (a <> b <> c) `matches` glob (a <> * <> c) with b being a module name" $
                property $
                    \a (ModuleName b) c -> (a <> b <> c) `matchesGlob` mkGlob (a <> "*" <> c)
            it "should obey:  (a <> b <> c) `matches` glob (a <> ** <> c)" $
                property $
                    \a b c -> (a <> b <> c) `matchesGlob` mkGlob (a <> "**" <> c)
