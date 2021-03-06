module Language.Vitrux.LexSpec where

import Control.Applicative ((<*))
import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Either (rights)
import Language.Vitrux.Lex
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, parse)

-- TODO move this
ucFirst :: String -> String
ucFirst [] = []
ucFirst (x : xs) = toUpper x : xs

spec :: Spec
spec = do
    let keywords = [("alias", aliasKeyword), ("foreign", foreignKeyword), ("import", importKeyword), ("struct", structKeyword), ("sub", subKeyword)]

    describe "Language.Vitrux.Lex.identifier" $ do
        it "lexes one-character identifiers" $ do
            forM_ ["_", "a", "A"] $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` [id]

        it "lexes multicharacter identifiers" $ do
            forM_ ["__", "a_b", "Ab", "aB", "abC_", "abc9023"] $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` [id]

        it "does not lex keywords" $ do
            forM_ (map fst keywords) $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` []

    forM_ keywords $ \(keyword, keywordParser) -> do
        describe ("Language.Vitrux.Lex.keyword" ++ ucFirst keyword) $ do
            it ("lexes '" ++ keyword ++ "'") $ do
                rights [parse (keywordParser <* eof) "" keyword] `shouldBe` [()]

            it ("does not lex '" ++ keyword ++ "x'") $ do
                rights [parse (keywordParser <* eof) "" (keyword ++ "x")] `shouldBe` []

            it "does not lex something weird" $ do
                rights [parse (keywordParser <* eof) "" "bullshit"] `shouldBe` []

    describe "Language.Vitrux.Lex.stringLiteral" $ do
        it "lexes string literals" $ do
            forM_ ["", "a", "A B C"] $ \str -> do
                rights [parse (stringLiteral <* eof) "" ("\"" ++ str ++ "\"")] `shouldBe` [str]
