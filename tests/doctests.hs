module Main where

import Data.Monoid ((<>))
import Test.DocTest

path :: String
path = "src/Database/PostgreSQL/Simple/Classy/Exceptions"

main :: IO ()
main = doctest [ "-isrc"
               , path <> "/SqlError.hs"
               , path <> "/ExecStatus.hs"
               , path <> "/FormatError.hs"
               , path <> "/QueryError.hs"
               , path <> "/ResultError.hs"
               ]
