module Main where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)

import           Data.List.Split (splitOn)
import           Text.ParserCombinators.ReadP
import           Data.Char (isLetter)
import           Text.Read (readMaybe)
import           Data.Maybe (fromMaybe)
import           Text.Pretty.Simple (pPrint)

import           System.IO
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    let paths = if null args then error "must provide a ballot file as argument"
                             else args
    inputs <- mapM readFile paths
    let ballots = (map (parseBallot . rmSpace) . lines . concat) inputs
    (pPrint . M.toAscList . result) ballots

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

data Result = Result String String
    deriving (Eq, Show)

vote :: ReadP Result
vote = Result <$> anything <* divider <*> anything <* eof

anything :: ReadP String
anything = many1 $ satisfy isLetter

divider :: ReadP String
divider = many1 $ satisfy (== '|')

rmSpace :: String -> String
rmSpace "" = ""
rmSpace (c:cs) = if c == ' '
                then rmSpace cs
                else c : rmSpace cs

parseVote :: String -> Map String Vote
parseVote = M.fromList . map fst . readP_to_S vote

parseBallot :: String -> Map String Vote
parseBallot b = M.unions $ map parseVote (splitOn "," b)

data Vote = Support
          | Oppose
          | Abstain
          deriving (Eq, Show, Read)

toVote :: String -> Vote
toVote = fromMaybe Abstain . readMaybe

vote2int :: Vote -> Int
vote2int Support = 1
vote2int Oppose = -1
vote2int Abstain = 0

result :: [Map String Vote] -> Map String Int
result = M.unionsWith (+) . map (M.map vote2int)
