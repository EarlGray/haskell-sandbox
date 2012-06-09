module PrettyJSON (
    showJValue,
) where

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, compact, pretty)

import Data.Bits (shiftR, shiftL, (.&.))
import Data.Char (ord)
import Numeric (showHex)

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

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
hexEscape c | d < 0x10000   = smallHex d
            | otherwise     = astral (d - 0x10000)
        where d = ord c

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right 

showJValue (JBool True) = text "true"
showJValue (JBool False) = text "false"
showJValue (JNumber n)  = text (show n)
showJValue (JString s)  = text s
showJValue (JNull)      = text "null"
showJValue (JArray arr) = series '[' ']' showJValue arr
showJValue (JObject o) = series '{' '}' field o
    where field (name, val) = string name <> text ": " <> showJValue val
