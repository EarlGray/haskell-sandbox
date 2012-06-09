module PutJSON where

import Data.List (intercalate)
import SimpleJSON

showJValue :: JValue -> String
showJValue (JString a)  = show a
showJValue (JNumber n)  = show n
showJValue (JBool True) = "true"
showJValue (JBool False) = "false"
showJValue (JNull)      = "null"

showJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map showPair ps)
          showPair (k, v)   = show k ++ ": " ++ showJValue v

showJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map showJValue vs)

putJValue :: JValue -> IO ()
putJValue = putStrLn . showJValue
