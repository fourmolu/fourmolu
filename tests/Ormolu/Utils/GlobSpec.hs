module Ormolu.Utils.GlobSpec (spec) where

import Ormolu.Utils.Glob (Glob, matchesGlob, mkGlob)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), listOf, oneof, property, suchThat)

data GlobCasePart
    = SingleWildcardFor !String
    | DoubleWildcardFor !String
    | ExactPart !String
    deriving (Eq, Show)

newtype GlobCase = GlobCase [GlobCasePart]
    deriving (Eq, Show)

gcGlob :: GlobCase -> Glob
gcGlob = mkGlob . toGlobString
    where
        toGlobString (GlobCase ps) = concatMap toGlobPart ps
        toGlobPart p = case p of
            SingleWildcardFor _ -> "*"
            DoubleWildcardFor _ -> "**"
            ExactPart s -> s

gcMatchingString :: GlobCase -> String
gcMatchingString = toMatchingString
    where
        toMatchingString (GlobCase ps) = concatMap toMatchingPart ps
        toMatchingPart p = case p of
            SingleWildcardFor s -> s
            DoubleWildcardFor s -> s
            ExactPart s -> s

instance Arbitrary GlobCase where
    arbitrary = GlobCase <$> listOf genPart
        where
            genPart = oneof [genSingleWildCard, genDoubleWildCard, genExact]
            genSingleWildCard = SingleWildcardFor <$> listOf (arbitrary `suchThat` (/= '.'))
            genDoubleWildCard = DoubleWildcardFor <$> arbitrary
            genExact = ExactPart <$> arbitrary

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
        it "should match strings where wildcards can be replaced by 0 or more characters" $
            property $
                \gc -> gcMatchingString gc `matchesGlob` gcGlob gc
