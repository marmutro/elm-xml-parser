module Xml.Parser exposing (parseXml, XmlAst(..))


{-| A parser which converts XML into an XmlAst,
which can be further transformed.

@docs XmlAst, parseXml
-}

import Combine exposing (..)
import Combine.Char exposing (..)
import String


type alias Name =
  String


type alias Key =
  String


type alias Value =
  String


type alias Attribute =
  ( Key, Value )


{-| The XML AST representation -}
type XmlAst
  = Element Name (List Attribute) (List XmlAst)
  | Body String
  | Comment String
  | CDATA String


spaces : Parser s (List Char)
spaces =
  many (space <|> newline <|> tab)


letter : Parser s Char
letter =
  upper <|> lower


betweenBoth : Char -> Parser s String
betweenBoth ch =
  String.fromList
    <$> between
          (char ch)
          (char ch)
          (many1 ((noneOf [ ch ])) <|> succeed [])


betweenSingleQuotes : Parser s String
betweenSingleQuotes =
  betweenBoth '\''


betweenDoubleQuotes : Parser s String
betweenDoubleQuotes =
  betweenBoth '"'


quotedString : Parser s String
quotedString =
  betweenSingleQuotes <|> betweenDoubleQuotes


attributeName : Parser s String
attributeName =
  String.fromList
    <$> many1 (letter <|> digit <|> char '-' <|> char ':')
    <?> "Invalid Attribute name"


tagName : Parser s String
tagName =
  String.fromList
    <$> many (choice [ letter, digit, char '_', char ':' ])
    <?> "Invalid Tag name"


keyValue : Parser s ( String, String )
keyValue =
  (\key value -> ( key, value ))
    <$> (attributeName <* spaces <* char '=' <* spaces)
    <*> (quotedString <* spaces)


openTag : Parser s ( String, List ( String, String ) )
openTag =
  (\name attribs -> ( name, attribs ))
    <$> (char '<' *> tagName)
    <*> (spaces *> many keyValue <* char '>')


closeTag : String -> Parser s ()
closeTag str =
  ()
    <$ (string "</" *> spaces *> string str *> spaces *> char '>')
    <?> ("Expected closing Tag for " ++ toString str)


withExplicitCloseTag : Parser s XmlAst
withExplicitCloseTag =
  (\( name, attribs, xml ) -> Element name attribs xml)
    <$> ((openTag <* spaces) >>= \( name, attribs ) -> (\xml -> ( name, attribs, xml )) <$> (many (innerXml <* spaces) <* closeTag name))


comment : Parser s XmlAst
comment =
  (String.fromList >> String.trim >> Comment)
    <$> (string "<!--" *> manyTill anyChar (string "-->"))


cdata : Parser s XmlAst
cdata =
  (String.fromList >> String.trim >> CDATA)
    <$> (string "<![CDATA[" *> manyTill anyChar (string "]]>"))


withoutExplicitCloseTag : Parser s XmlAst
withoutExplicitCloseTag =
  (\name attribs -> Element name attribs [])
    <$> ((char '<' *> tagName <* spaces))
    <*> (many keyValue <* string "/>")


parseBody : Parser s XmlAst
parseBody =
  (Body << String.trim << String.fromList) <$> (many1 (noneOf [ '<', '>' ]))


xmlDeclaration : Parser s ()
xmlDeclaration =
  () <$ (string "<?xml" *> Combine.while ((/=) '?') <* string "?>")


xmlParser : Parser s XmlAst
xmlParser =
  lazy (\() -> withExplicitCloseTag) <|> lazy (\() -> withoutExplicitCloseTag)


rootElements : Parser s (List XmlAst)
rootElements =
  many1 (choice [ xmlParser, comment <* spaces ])


innerXml : Parser s XmlAst
innerXml =
  comment <|> cdata <|> xmlParser <|> parseBody


parser : Parser s (List XmlAst)
parser =
  spaces *> maybe xmlDeclaration *> spaces *> rootElements <* spaces <* end


{-| Trys to parse the input string as an AST -}
parseXml : String -> Result (List String) (List XmlAst)
parseXml str =
  case parse parser str of
    Ok (_, _, xml ) ->
      Ok xml

    Err (_, _, failure ) ->
      Err failure
