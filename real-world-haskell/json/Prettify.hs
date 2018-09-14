module Prettify (
        renderJValue
    ) where

import Numeric
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))
import Data.List (intercalate)

import SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
    deriving Show

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

instance Semigroup Doc where
    Empty <> y = y
    x <> Empty = x
    x <> y = x `Concat` y

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

hcat :: [Doc] -> Doc
hcat xs = undefined

smallHex :: Int -> Doc
smallHex x = text "\\u"
        <> text (replicate (4 - length h) '0')
        <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d > 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

--series :: Char -> Char -> (a -> Doc) -> [a] -> Doc

--oneChar :: Char -> Doc
--oneChar c = case lookup c simpleEscapes of
                --Just r -> text r
                --Nothing | mustEscape c -> hexEscape c
                        -- | otherwise -> char c
    --where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

renderJValue :: JValue -> Doc
--renderJValue (JString s)   = string s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
--renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
    --where pairs [] = ""
          --pairs ps = intercalate ", " (map renderPair ps)
          --renderPair (k, v) = show k ++ ": " ++ renderJValue v
--renderJValue (JArray a) = "{" ++ values a ++ "}"
    --where values [] = ""
          --values vs = intercalate ", " (map renderJValue vs)

