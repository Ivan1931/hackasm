{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
  AST(..),
  A(..),
  C(..),
  LoopAn(..),
  lineParser,
  parseLines,
  cInstructions,
  jumpInstructions,
  dInstructions
) where

import Data.Binary.Put
import Data.Attoparsec.Combinator (count, many', many1)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad (foldM)
import Data.Default (def, Default)
import Errors

cInstructions:: [(Text, Int)]
cInstructions = [
        ("0"  , 0b0101010000000),
        ("1"  , 0b0111111000000),
        ("-1" , 0b0111010000000),
        ("D"  , 0b0001100000000),
        ("A"  , 0b0110000000000),
        ("M"  , 0b1110000000000),
        ("!D" , 0b0001101000000),
        ("!A" , 0b0110001000000),
        ("-D" , 0b0001111000000),
        ("-A" , 0b0110011000000),
        ("D+1", 0b0011111000000),
        ("A+1", 0b0110111000000),
        ("D-1", 0b0001110000000),
        ("A-1", 0b0110010000000),
        ("D+A", 0b0000010000000),
        ("D-A", 0b0010011000000),
        ("A-D", 0b0000111000000),
        ("D&A", 0b0000000000000),
        ("D|A", 0b0010101000000),
        ("!M" , 0b1110001000000),
        ("-M" , 0b1110011000000),
        ("M+1", 0b1110111000000),
        ("M-1", 0b1110010000000),
        ("D+M", 0b1000010000000),
        ("D-M", 0b1010011000000),
        ("M-D", 0b1000111000000),
        ("D&M", 0b1000000000000),
        ("D|M", 0b1010101000000)
  ] 

jumpInstructions :: [(Text, Int)]
jumpInstructions = [
      ("JGT", 0b0001),
      ("JEQ", 0b0010),
      ("JGE", 0b0011),
      ("JLT", 0b0100),
      ("JNE", 0b0101),
      ("JLE", 0b0110),
      ("JMP", 0b0111)
  ]

dInstructions :: [(Text, Int)]
dInstructions = [
    ("A"  , 0b100000),
    ("D"  , 0b010000),
    ("M"  , 0b001000),
    ("AD" , 0b110000),
    ("AM" , 0b101000),
    ("DM" , 0b011000),
    ("ADM", 0b111000)
  ]
{-
 - dest=compute;jump
 -}


data C = C {
          dest :: Text,
          comp :: Text,
          jump :: Text
        } deriving(Eq, Show)

data A = Numeric Int | Symbolic Text
       deriving(Eq, Show)

newtype LoopAn = LoopAn {
    anno :: Text
  } deriving(Eq, Show)

data AST = Ai A | Ci C | Loop LoopAn | Comment Text
         deriving(Eq, Show)

instance Default C where
  def = C "" "" ""

instance Default A where
  def = Symbolic ""

applyToInstructions :: [(Text, a)] -> Parser Text
applyToInstructions = choice . map (string . fst)

destParser :: Parser Text
destParser =
  let
    validChar = inClass "AMD"
  in do
    dest <- many1 (takeWhile1 validChar)
    return $ T.concat dest

computeParser :: Parser Text
computeParser = do
  option '0' (char '=')
  comp <- applyToInstructions . reverse $ cInstructions
  return comp

jumpParser :: Parser Text
jumpParser = do
  char ';'
  jump <- applyToInstructions jumpInstructions
  return jump

destComp :: Parser C
destComp = do
  d <- destParser
  c <- computeParser
  return $ def { dest = d, comp = c }

compJump :: Parser C
compJump = do
  c <- computeParser
  j <- jumpParser
  return $ def { comp = c, jump = j }

fullC :: Parser C
fullC = do
  d <- destParser
  c <- computeParser
  j <- jumpParser
  return $ C { dest = d, comp = c, jump = j }

cParser :: Parser C
cParser = choice [fullC, compJump, destComp]

numericAddress :: Parser A
numericAddress = do
  char '@'
  address <- many1 digit
  return . Numeric . read $ address
  
specialSymCharacters :: Char -> Bool
specialSymCharacters = inClass "._!#$%^&*"

symbolLetter :: Parser Char
symbolLetter = choice [letter, satisfy specialSymCharacters, digit]

symbolicAddress :: Parser A
symbolicAddress = do
  char '@'
  first  <- choice [letter, satisfy specialSymCharacters]
  rest   <- many symbolLetter
  return . Symbolic $ first `T.cons` T.pack rest

aParser :: Parser A
aParser = choice [numericAddress, symbolicAddress]

loopParser :: Parser LoopAn
loopParser = do
  char '('
  address <- many1 symbolLetter
  char ')'
  return . LoopAn . T.pack $ address

isWhiteSpace :: Char -> Bool
isWhiteSpace c = ws c || isEndOfLine c
  where ws = inClass " \t"

commentParser :: Parser Text
commentParser = do
  char '/'
  char '/'
  AT.takeWhile (not . isEndOfLine)

lineParser :: Parser AST
lineParser =
  let
    a' = fmap Ai aParser
    c' = fmap Ci cParser
    loop' = fmap Loop loopParser
    lineWithComment = do
      x <- choice [a', c', loop']
      comment <- option "" commentParser
      return x
    comment' = fmap Comment commentParser
  in
    choice [lineWithComment, comment']

parseLines :: [Text] -> Either AssembleError (Int, [AST])
parseLines lines = foldM (\ acc str -> parse acc $ T.strip str) (0, []) (reverse lines)
  where parse (idx, xs) ""   = Right (idx+1, xs)
        parse (idx, xs) str  =
          case parseOnly lineParser str of
            Left error -> Left $ AssembleError idx (ParseError error str)
            Right ast  -> Right (idx+1, ast:xs)

