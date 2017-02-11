{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Types

import Data.Foldable

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import System.Environment
import System.IO.Temp

import qualified Data.Text as T
import Data.Text.ICU.Replace
import Data.Text.ICU

-- import csv
-- import optparse-applicative
-- import tmpfile

main :: IO ()
main = do
  args <- getArgs
  let [inputFileName, outputFileName] = args -- TODO: make it right
  withSystemTempFile "MTBankToHomemoney.csv" $ \tmpFileName hFile -> do
    fixBrokenMtbankCsv inputFileName tmpFileName
    mtRecords <- parseMtbankCsv tmpFileName
    let hmRecords = mt2hm <$> mtRecords
    dumpHomemoneyCsv outputFileName hmRecords
    return ()

fixBrokenMtbankCsv :: FilePath -> FilePath -> IO ()
fixBrokenMtbankCsv input output = do
  rawContent <- readFile input
  let fixedContent = fixMtbankCsv rawContent
  writeFile output fixedContent

fixMtbankCsv :: String -> String
fixMtbankCsv text =
  T.unpack $ foldl' (\s (reg, rep) -> replaceAll reg rep s) (T.pack text)
    [ ("(\\A.*[\r\n]+){10}",     "") -- TODO: make it better and check if it works
    , (regex [Multiline] "^\"", "BEGBEGBEG")
    , ("\"\r\n",                "ENDENDEND\r\n")
    , ("\",\"",                 "MIDMIDMID")
    , ("\"",                    "\"\"")
    , ("BEGBEGBEG",             "\"")
    , ("ENDENDEND",             "\"")
    , ("MIDMIDMID",             "\",\"")
    ]

parseMtbankCsv :: FilePath -> IO [MTRecord]
parseMtbankCsv input = do
  rawCsv <- BL.readFile input
  case decodeByNameWith defaultDecodeOptions rawCsv of
    Left _err -> print _err >> return [] -- TODO: make it right
    Right (_header, v) -> return (V.toList v)

mt2hm :: MTRecord -> HMRecord
mt2hm _ = HMRecord{}

dumpHomemoneyCsv :: FilePath -> [HMRecord] -> IO ()
dumpHomemoneyCsv output records =
  BL.writeFile output (encodeDefaultOrderedByName records)
