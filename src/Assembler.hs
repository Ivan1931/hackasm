{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module Assembler where

import Debug.Trace
import Parser
import Errors
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (putInt16be, runPut, Put)
import Data.Binary (Binary, Word8)
import Data.List.Split (chunksOf)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Bits
import Control.Monad (foldM)
import Data.Default
import Data.List (sort)

data AssembleState = AssembleState {
                       allocatedAddrs       :: Int,
                       lineInFile           :: Int,
                       instructionNumber    :: Int,
                       symbolTable          :: M.Map T.Text Int
                    }
                    deriving (Eq, Show)

initSymbolTable :: M.Map T.Text Int
initSymbolTable = M.fromList [
                  ("KBD"   , 24576),
                  ("SCREEN", 16384),
                  ("THAT"  , 4),
                  ("THIS"  , 3),
                  ("ARG"   , 2),
                  ("LCL"   , 1),
                  ("SP"    , 0),
                  ("R0"    , 0),
                  ("R1"    , 1),
                  ("R2"    , 2),
                  ("R3"    , 3),
                  ("R4"    , 4),
                  ("R5"    , 5),
                  ("R6"    , 6),
                  ("R7"    , 7),
                  ("R8"    , 8),
                  ("R9"    , 9),
                  ("R10"   , 10),
                  ("R11"   , 11),
                  ("R12"   , 12),
                  ("R13"   , 13),
                  ("R14"   , 14),
                  ("R15"   , 15)
                ]

instance Default AssembleState where
  def = AssembleState {
          allocatedAddrs    = 16,
          lineInFile        = 0,
          instructionNumber = 0,
          symbolTable       = initSymbolTable
        }

type AssembleResult = Either AssembleError AssembleState

incLineInFile :: AssembleState -> AssembleState
incLineInFile state = state { lineInFile = lif + 1 }
  where lif = lineInFile state

incInstructionNumber :: AssembleState -> AssembleState
incInstructionNumber state = state { instructionNumber = inum + 1 }
  where inum = instructionNumber state

marchForward :: AssembleState -> AssembleState
marchForward = incLineInFile . incInstructionNumber

resetLines :: AssembleState -> AssembleState
resetLines state = state {
              lineInFile        = 0,
              instructionNumber = 0
            }

buildLoops :: AST -> AssembleState -> AssembleResult
buildLoops (Comment _) state =
  let
    lif = lineInFile state
  in Right $ state { lineInFile = lif + 1 }
buildLoops (Loop loopAn) state =
  let
    LoopAn sym = loopAn
    table      = symbolTable state
    inum       = instructionNumber state
  in case table M.!? sym of
    Just addr -> Left $ AssembleError {
                          reason = AmbiguousLoop sym,
                          line   = lineInFile state
                        }
    Nothing   ->
      let
        newState = state { symbolTable = M.insert sym inum table }
      in 
        Right $ incLineInFile newState
buildLoops _ state = Right $ marchForward state
        
buildAddrs :: AST -> AssembleState -> AssembleResult
buildAddrs (Ai (Symbolic sym)) state =
  let
    table = symbolTable state
  in
    case table M.!? sym of
      Just addr -> Right $ marchForward state
      Nothing   -> Right $ marchForward newState
        where aa       = allocatedAddrs state
              newSymbolTable = M.insert sym aa table
              newState = state {
                          allocatedAddrs = aa + 1,
                          symbolTable    = newSymbolTable
                       }
buildAddrs _ state = Right $ marchForward state 

buildSymbols :: [AST] -> AssembleResult
buildSymbols ast = do
  withLoops <- fmap resetLines $ foldM (flip buildLoops) def ast
  withAddrs <- fmap resetLines $ foldM (flip buildAddrs) withLoops ast
  return withAddrs

instructionsOnly :: [AST] -> [AST]
instructionsOnly = filter isInstruction
  where 
    isInstruction (Ai _) = True
    isInstruction (Ci _) = True
    isInstruction _      = False


cLookup :: M.Map T.Text Int
cLookup = M.fromList cInstructions

jLookup :: M.Map T.Text Int
jLookup = M.fromList jumpInstructions

dLookup :: M.Map T.Text Int
dLookup = M.fromList dInstructions

assembler :: AST -> AssembleState -> Put
assembler (Ai (Symbolic sym)) state =
  let
    table = symbolTable state
  in
    putInt16be $ fromIntegral (table M.! sym)
assembler (Ai (Numeric num)) _ = putInt16be $ fromIntegral num
assembler (Ci instr) _ =
  let
    mask :: Int
    mask = 0b111 `shiftL` 13
    uggoHack = T.pack . sort . T.unpack . dest
    dst  = M.findWithDefault 0 (uggoHack instr) dLookup
    cmp  = M.findWithDefault 0 (comp instr) cLookup
    jmp  = M.findWithDefault 0 (jump instr) jLookup
    bits = mask .|. dst .|. cmp .|. jmp
  in putInt16be $ fromIntegral bits
assembler _ _  = undefined

assemble :: [AST] -> Either AssembleError BL.ByteString
assemble ast = 
  let
    cleanedAST = instructionsOnly ast
    toPut      = (flip assembler) . resetLines
  in do
    symTable <- buildSymbols ast
    return . runPut $ foldMap (toPut symTable) cleanedAST

word16ToString :: Word8 -> String
word16ToString word = foldl step "" [0..7]
  where 
    step str bitIdx =
      let
        mask = 0x1 `shiftL` bitIdx
      in if (word .&. mask) == 0x0 then
        '0' : str
      else
        '1' : str

assembleBinString :: [AST] -> Either AssembleError [String]
assembleBinString ast = do
  result <- assemble ast
  return $ reverse $ map (concat . reverse) $ chunksOf 2 $ BL.foldl f [] result
  where f xs w = word16ToString w : xs
