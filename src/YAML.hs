{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module YAML where

import Control.Monad (mplus, forM)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.ByteString.Char8 (unpack)
import Data.Text.Arbitrary
import Data.Yaml.Parser
import DeriveArbitrary
import Test.QuickCheck
import Text.Libyaml
import Text.PrettyPrint

import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as T

$(devArbitrary ''YamlValue)

-- TODO: Fix this (in your domain. IN YOUR DOMAIN!)
--mencode :: YamlValue -> LC8.ByteString
--mencode = LC8.fromStrict . encodePretty defConfig

encodePretty :: YamlValue -> ByteString
encodePretty yv = case encodeDoc yv of
  Just d  -> pack . render $ d
  Nothing -> error ("Malformed YAML value: " ++ show yv)

encodeDoc :: YamlValue -> Maybe Doc
encodeDoc yv = encodeMapping yv
  `mplus` encodeSequence yv
  `mplus` encodeScalar yv
  `mplus` encodeAlias yv

-- | Encode a yaml scalar
encodeScalar :: YamlValue -> Maybe Doc
encodeScalar yv = case yv of
  Scalar bs t st an -> return . encodeScalar' an t st . text . unpack $ bs
  _                 -> Nothing
  where encodeScalar' an t st = encodeAnchor an .  encodeTag t . encodeStyle st

-- | Encode an yaml alias value
encodeAlias :: YamlValue -> Maybe Doc
encodeAlias (Alias an) = return $ "*" <> text an
encodeAlias _          = Nothing

encodeSequence :: YamlValue -> Maybe Doc
encodeSequence yvs = case yvs of
  Sequence lst an -> encodeAnchor an <$> encodeList lst
  _               -> Nothing
  where encodeList lst = vcat <$> mapM (\v -> ("- " <>) <$> encodeDoc v) lst

encodeMapping :: YamlValue -> Maybe Doc
encodeMapping yv = case yv of
  Mapping alist an -> encodeAnchor an <$> encodeAlist alist
  _                -> Nothing
  where encodeAlist :: [(Text, YamlValue)] -> Maybe Doc
        encodeAlist alist = vcat <$> mapM encodePair alist

        encodePair :: (Text, YamlValue) -> Maybe Doc
        encodePair (k, v) = indOrNl v (text (T.unpack k)) <$> encodeDoc v

        indOrNl :: YamlValue -> Doc -> Doc -> Doc
        indOrNl (Mapping _ _)  d1 d2 = d1 <> ":" $$ nest 2 d2
        indOrNl (Sequence _ _) d1 d2 = d1 <> ":" $$ nest 2 d2
        indOrNl _              d1 d2 = d1 <> ":" <+> d2


indent :: YamlValue -> Doc -> Doc
indent (Mapping _ _)     doc = nest 2 doc
indent _                 doc = doc

encodeAnchor :: Anchor -> Doc -> Doc
encodeAnchor (Just an) doc = "&" <> text an <+> doc
encodeAnchor Nothing doc = doc

encodeStyle :: Text.Libyaml.Style -> Doc -> Doc
encodeStyle s doc = case s of
  Any          -> encodeStyle DoubleQuoted doc
  PlainNoTag   -> encodeStyle Plain doc
  Plain        -> doc
  SingleQuoted -> quotes doc
  DoubleQuoted -> doubleQuotes doc
  Literal      -> text " | " $$ nest 2 doc
  Folded       -> text " > " $$ nest 2 doc

encodeTag :: Tag -> Doc -> Doc
encodeTag t doc = case t of
  StrTag   -> text "!!str " <> doc
  FloatTag -> text "!!float " <> doc
  NullTag  -> text "!!null " <> doc
  BoolTag  -> text "!!bool " <> doc
  SetTag   -> text "!!set " <> doc
  IntTag   -> text "!!int " <> doc
  SeqTag   -> text "!!seq " <> doc
  MapTag   -> text "!!map " <> doc
  UriTag n -> text "!!" <> text (drop nsLength n) <+> doc
  NoTag    -> doc
  where nsLength = length ("tag:yaml.org,2002:" :: String)
