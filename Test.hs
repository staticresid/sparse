{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Text.XML
import Data.Maybe(mapMaybe)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Foldable(foldMap)
import Data.Text.Read(decimal)
import Prelude hiding(Show,show)
import Text.Show.Text(show)

data SomeDate = SomeDate String

data MyStructure = MyAAttr { myaattr :: Int }
                 | MyBContent { mybcontentfirst :: T.Text, mybcontentsecond :: T.Text}

isElement :: Name -> Node -> Maybe Element
isElement n' (NodeElement e@(Element n _ _)) | n == n' = Just e
                                             | otherwise = Nothing
isElement _ _ = Nothing

catContent :: Node -> T.Text
catContent (NodeContent c) = c
catContent _ = T.empty

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

findNode :: Name -> Element -> Maybe Element
findNode n (Element _ _ nodes) = safeHead . mapMaybe ( isElement n ) $ nodes

findAttribute :: Name -> Element -> Maybe T.Text
findAttribute n (Element _ attrs _ ) = n `Map.lookup` attrs

findContent :: Element -> Maybe T.Text
findContent (Element _ _ nodes ) = Just $ foldMap catContent nodes

parseDecimal :: T.Text -> Maybe Int
parseDecimal t = case decimal t of
  Left _ -> Nothing
  Right ( e,_ ) -> Just e

writeDecimal :: Int -> T.Text
writeDecimal = show

constructMyStructure :: Element -> Maybe MyStructure
constructMyStructure e = ( MyAAttr <$> (findNode "a" e >>= findAttribute "attr" >>= parseDecimal) )
                     <|> ( MyBContent <$> (findNode "b" e >>= findContent) <*> (findNode "c" e >>= findContent) )


data XmlIso a = XmlIso {
    xmlIsoFrom :: T.Text -> Maybe a
  , xmlIsoTo :: a -> T.Text
  }

xmlIsoId :: XmlIso a
xmlIsoId = undefined

myDecimalIso = undefined

data NodePath = AttributePath [Name] | ContentPath [Name]

data XmlTypedNode a b = XmlTypedNode {
    xmlTypedNodePath :: NodePath
  , xmlPartialIso :: XmlIso a
  , xmlGet :: b -> a
  }

xmlParse :: XmlTypedNode a b -> T.Text -> Maybe a
xmlParse tn = xmlIsoFrom ( xmlPartialIso tn )

-- a :: Int, b :: MyStructure
class XmlFunctor f where
  -- first try
  -- liftXml :: (XmlTypedNode a -> b) -> f a -> f b
  liftXml :: (a -> b) -> XmlTypedNode a b -> f b


newtype XmlParser a = XmlParser {
  parseXml :: Element -> Maybe a
  }

newtype XmlConstructor a = XmlConstructor {
  constructXml :: a -> Element
  }

findContentNode :: Element -> [Name] -> Maybe T.Text
findContentNode = undefined

instance XmlFunctor XmlParser where
  liftXml f typedNode = XmlParser $ \element ->
    case xmlTypedNodePath typedNode of
      ContentPath names -> case findContentNode element names of
        Nothing -> Nothing
        Just t -> case xmlParse typedNode t of
          Nothing -> Nothing
          Just a -> Just ( f a )
      AttributePath _ -> undefined

class XmlApplicative f where
  liftXml2 :: f (a -> b) -> XmlTypedNode a b -> f b

instance XmlApplicative XmlParser where
  liftXml2 partialParser typedNode = XmlParser $ \element ->
    case parseXml partialParser element of
      Nothing -> Nothing
      Just f -> case xmlTypedNodePath typedNode of
                  ContentPath names -> case findContentNode element names of
                    Nothing -> Nothing
                    Just t -> case xmlParse typedNode t of
                      Nothing -> Nothing
                      Just a -> Just ( f a )
                  AttributePath _ -> undefined

xmlParseText = Just

{-
constructTwo :: ( XmlFunctor f,XmlApplicative f ) => f MyStructure
constructTwo = liftXml2 ( liftXml MyBContent (XmlTypedNode (ContentPath [ "b" ]) xmlParseText mybcontentfirst ) ) (XmlTypedNode (ContentPath [ "c" ]) xmlParseText mybcontentsecond )
-}

constructTwoPartial :: XmlFunctor f => f (T.Text -> MyStructure)
constructTwoPartial = liftXml MyBContent (XmlTypedNode (ContentPath [ "b" ]) xmlIsoId ( mybcontentfirst ))

--constructMyStructureSimpler :: Syntax a => a MyStructure
--constructMyStructureSimpler = (MyAAttr <-> (XmlTypedNode "a/@attr" myDecimal )) <@> ( MyBContent <-> (XmlTypedNode "b" xmlIsoId ) <@> (XmlTypedNode "c" xmlIsoId ) )
constructJustOne :: XmlFunctor f => f MyStructure
-- first try
--constructJustOne = MyAAttr <-> (XmlTypedNode "a/@attr" myDecimal)
constructJustOne = liftXml MyAAttr (XmlTypedNode (AttributePath [ "a","attr" ]) myDecimalIso myaattr)

--MyAAttr <$> findNode "a/@attr" parseDecimal <*> findNode "foo/baz" id

{-
destructMyStructure :: Element -> MyStructure -> Element
destructMyStructure e s = case s of
  MyAAttr a -> ( createNode "a" e )
-}
--  MyAAttr a -> createAttribute "attr" ( createNode "a" e ) (writeDecimal a)
--  MyBContent a b -> createNode "b" e
{-
thenDo = undefined

findElement :: Element -> Maybe MyStructure
findElement = ( findNode "foo" `thenDo` findNode "bar" ) `thenDo` findAttribute "baz"
-}

main :: IO ()
main = undefined
