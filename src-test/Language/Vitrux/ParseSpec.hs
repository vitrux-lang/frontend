module Language.Vitrux.ParseSpec where

import Control.Applicative ((<*))
import Data.Either (rights)
import Language.Vitrux.AST
import Language.Vitrux.Parse
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, runParser)

-- TODO move these
voidType = NamedType (UnqualifiedName "Void")
exampleSubType = SubType [(NamedType (UnqualifiedName "A")), (NamedType (UnqualifiedName "B"))] (NamedType (UnqualifiedName "C"))
aType = NamedType (UnqualifiedName "A")
bType = NamedType (UnqualifiedName "B")
cType = NamedType (UnqualifiedName "C")
dType = NamedType (UnqualifiedName "D")
emptyBlock = BlockExpr []
makeType name = NamedType (UnqualifiedName name)
stringFoo = StringLiteralExpr "foo"
stringBar = StringLiteralExpr "bar"
abcQualifiedName = QualifiedName (ModuleName ["a", "b"]) "c"
abcType = NamedType abcQualifiedName
helloWorldSource = "import mill.log\n" ++
                   "\n" ++
                   "sub main(console: log.Logger): () {\n" ++
                   "    log.info(console, \"Hello, world!\")" ++
                   "}\n"
helloWorldAST = Module [ ImportDecl (ModuleName ["mill", "log"])
                       , SubDecl "main"
                                 [Parameter "console" (NamedType (QualifiedName (ModuleName ["log"]) "Logger"))]
                                 (TupleType [])
                                 (BlockExpr [ExprStmt $ CallExpr
                                                                        (NameExpr (QualifiedName (ModuleName ["log"]) "info"))
                                                                        [NameExpr (UnqualifiedName "console"), StringLiteralExpr "Hello, world!"]])
                       ]

spec :: Spec
spec = do
    describe "Language.Vitrux.Parse.module_" $ do
      it "parses the hello world program" $ do
        rights [runParser module_ 0 "" helloWorldSource] `shouldBe` [helloWorldAST]

    describe "Language.Vitrux.Parse.name" $ do
      it "parses unqualified names" $ do
        rights [runParser (name <* eof) 0 "" "a"] `shouldBe` [UnqualifiedName "a"]

      it "parses qualified names" $ do
        rights [runParser (name <* eof) 0 "" "a.b.c"] `shouldBe` [abcQualifiedName]

    describe "Language.Vitrux.Parse.parameter" $ do
      it "parses a parameter with a type" $ do
        rights [runParser (parameter <* eof) 0 "" "a: b"] `shouldBe` [Parameter "a" (NamedType (UnqualifiedName "b"))]

    describe "Language.Vitrux.Parse.parameterList" $ do
      it "parses an empy parameter list" $ do
        rights [runParser (parameterList <* eof) 0 "" "()"] `shouldBe` [[]]

      it "parses a parameter list with a single element" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b)"] `shouldBe` [[Parameter "a" (makeType "b")]]

      it "parses a parameter list with multiple elements" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b, c: d, e: f)"] `shouldBe` [[Parameter "a" (makeType "b"), Parameter "c" (makeType "d"), Parameter "e" (makeType "f")]]

      it "allows a trailing comma" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b,)"] `shouldBe` [[Parameter "a" (makeType "b")]]

    describe "Language.Vitrux.Parse.type_" $ do
      it "parses named types" $ do
        rights [runParser (type_ <* eof) 0 "" "A"] `shouldBe` [aType]
        rights [runParser (type_ <* eof) 0 "" "m.A"] `shouldBe` [NamedType (QualifiedName (ModuleName ["m"]) "A")]
        rights [runParser (type_ <* eof) 0 "" "m.n.A"] `shouldBe` [NamedType (QualifiedName (ModuleName ["m", "n"]) "A")]

      it "parses sub types" $ do
        rights [runParser (type_ <* eof) 0 "" "(A) => B"] `shouldBe` [SubType [aType] bType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => C"] `shouldBe` [SubType [aType, bType] cType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => (C) => D"] `shouldBe` [SubType [aType, bType] (SubType [cType] dType)]

      it "parses tuple types" $ do
        rights [runParser (type_ <* eof) 0 "" "()"] `shouldBe` [TupleType []]
        rights [runParser (type_ <* eof) 0 "" "(A)"] `shouldBe` [TupleType [aType]]
        rights [runParser (type_ <* eof) 0 "" "(A, B)"] `shouldBe` [TupleType [aType, bType]]

    describe "Language.Vitrux.Parse.blockExpr" $ do
      it "parses an empty block" $ do
        rights [runParser (blockExpr <* eof) 0 "" "{}"] `shouldBe` [BlockExpr []]
        rights [runParser (blockExpr <* eof) 0 "" "{ }"] `shouldBe` [BlockExpr []]

    describe "Language.Vitrux.Parse.aliasDecl" $ do
      it "parses alias decls" $ do
        rights [runParser (aliasDecl <* eof) 0 "" "alias T = (A, B) => C"] `shouldBe` [AliasDecl "T" exampleSubType]

    describe "Language.Vitrux.Parse.structDecl" $ do
      it "parses struct decls" $ do
        rights [runParser (structDecl <* eof) 0 "" "struct T { }"] `shouldBe` [StructDecl "T" []]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A }"] `shouldBe` [StructDecl "T" [Field "x" aType]]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A y: B }"] `shouldBe` [StructDecl "T" [Field "x" aType, Field "y" bType]]

    describe "Language.Vitrux.Parse.structLitExpr" $ do
      it "parses an empty struct literal" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{}"] `shouldBe` [StructLiteralExpr aType []]

      it "parses a struct literal with one value" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{foo: \"foo\"}"] `shouldBe` [StructLiteralExpr aType [FieldValue "foo" stringFoo]]

      it "parses a struct literal with values" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{foo: \"foo\", bar :\"bar\"}"] `shouldBe` [StructLiteralExpr aType [FieldValue "foo" stringFoo, FieldValue "bar" stringBar]]

    describe "Language.Vitrux.Parse.subDecl" $ do
      it "parses an empty sub declaration" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(): Void { }"] `shouldBe` [SubDecl "foo" [] voidType emptyBlock]

      it "parses an sub declaration with a parameter" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(a: b): (A, B) => C { }"] `shouldBe` [SubDecl "foo" [Parameter "a" (makeType "b")] exampleSubType emptyBlock]

    describe "Language.Vitrux.Parse.foreignSubDecl" $ do
      it "parses foreign sub declarations" $ do
        rights [runParser (foreignSubDecl <* eof) 0 "" "foreign \"./console.js\" sub ecmascript.returnCall info(message: String): ()"] `shouldBe` [ForeignSubDecl (ForeignLibrary "./console.js") (CallingConvention (QualifiedName (ModuleName ["ecmascript"]) "returnCall")) "info" [Parameter "message" (NamedType (UnqualifiedName "String"))] (TupleType [])]
