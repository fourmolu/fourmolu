module Ormolu.Utils.GlobSpec (spec) where

import Ormolu.Utils.Glob (Glob, matchesGlob, mkGlob)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), listOf, oneof, property)

data GlobCasePart
    = WildcardFor !String
    | ExactPart !String
    deriving (Eq, Show)

newtype GlobCase = GlobCase [GlobCasePart]
    deriving (Eq, Show)

gcGlob :: GlobCase -> Glob
gcGlob = mkGlob . toGlobString
    where
        toGlobString (GlobCase ps) = concatMap toGlobPart ps
        toGlobPart p = case p of
            WildcardFor _ -> "*"
            ExactPart s -> s

gcMatchingString :: GlobCase -> String
gcMatchingString = toMatchingString
    where
        toMatchingString (GlobCase ps) = concatMap toMatchingPart ps
        toMatchingPart p = case p of
            WildcardFor s -> s
            ExactPart s -> s

instance Arbitrary GlobCase where
    arbitrary = GlobCase <$> listOf genPart
        where
            genPart = oneof [genWildCard, genExact]
            genWildCard = WildcardFor <$> arbitrary
            genExact = ExactPart <$> arbitrary

spec :: Spec
spec =
    describe "glob" $ do
        it "should match strings exactly" $
            "Hello.world" `matchesGlob` mkGlob "Hello.world" `shouldBe` True
        it "should fail if case doesn't match" $
            "Some.thing" `matchesGlob` mkGlob "Some.Thing" `shouldBe` False
        it "should allow wildcards to match anything" $
            "Some content here" `matchesGlob` mkGlob "Som*here" `shouldBe` True
        it "should fail if input doesn't match glob even with wildcards" $
            "Completely different content there" `matchesGlob` mkGlob "content*here" `shouldBe` False
        it "should allow multiple wildcards" $
            "Some.Fake.Module.Path.With.Internals" `matchesGlob` mkGlob "Some.*.Path.*.Internals" `shouldBe` True
        it "should allow wildcard at the beginning" $
            "Anything goes!" `matchesGlob` mkGlob "*oes!" `shouldBe` True
        it "should allow wildcard at the end" $
            "Lorem ipsum..." `matchesGlob` mkGlob "Lorem ip*" `shouldBe` True
        it "should match strings where wildcards can be replaced by 0 or more characters" $
            property $ \gc ->
                gcMatchingString gc `matchesGlob` gcGlob gc `shouldBe` True