module Day16.Mod where

import Numeric
import Utils.Mod

type BinNum = [Int]

toDecimal :: BinNum -> Int
toDecimal bits = foldl (\sum (idx, val) -> sum + val * (2 ^ idx)) 0 (zip [0 .. (length bits)] (reverse bits))

toBin :: Int -> BinNum
toBin 0 = [0]
toBin 1 = [1]
toBin n
  | even n = toBin (n `div` 2) ++ [0]
  | otherwise = toBin (n `div` 2) ++ [1]

getBin :: String -> BinNum
getBin str = toBin num
  where
    [(num, _)] = readHex str -- TODO: maybe trim ending zeros?

readBin :: IO BinNum
readBin = getBin <$> getRawInput

-- parsePacket :: BinNum -> BinNum

-- First 3 bits are packet version
-- Next 3 bits are type id
-- if Id is not 4 (100) it s an operator type
-- if id is 4, it is a literal type. next bits encode the literal number
-- if operator next bit is length type id
-- length id 0 = next 15 bits is size in bits of the sub packets for this packet
-- length id 1 = next 11 bits are a number that represents the number of sub-packets immediately contained
-- rest is sub packets

data PacketData = Literal Int | Op Int [Packet] deriving (Show, Eq)

data Packet = Packet {version :: Int, packetData :: PacketData} | Empty deriving (Show, Eq)

-- >>> parseLiteral [1,0,1,1,1,1,1,1,1,0,0,0,1,0,1,0,0,0]
-- (2021,[0,0,0])
parseLiteral :: BinNum -> (Int, BinNum)
parseLiteral binNum = let (literal, rest) = parseHelper binNum in (toDecimal literal, rest)
  where
    parseHelper :: BinNum -> (BinNum, BinNum)
    parseHelper [] = ([], [])
    parseHelper (0 : xs) = splitAt 4 xs
    parseHelper (1 : xs) = (thisPart ++ restNum, remainingPackets)
      where
        thisPart = take 4 xs
        (restNum, remainingPackets) = parseHelper (drop 4 xs)

parseUntilRestIsLen :: Int -> BinNum -> ([Packet], BinNum)
parseUntilRestIsLen n binNum | length binNum <= n = ([], binNum)
parseUntilRestIsLen n binNum = (parsedPacket : recParsed, recRemaining)
  where
    startLen = length binNum
    (parsedPacket, remaining) = parsePacket binNum
    newLen = length binNum
    newN = n - (startLen - newLen)
    (recParsed, recRemaining) = parseUntilRestIsLen newN remaining

parseNPackets :: Int -> BinNum -> ([Packet], BinNum)
parseNPackets 0 binNum = ([], binNum)
parseNPackets n binNum = (parsedPacket : recParsed, recRemaining)
  where
    (parsedPacket, remaining) = parsePacket binNum
    (recParsed, recRemaining) = parseUntilRestIsLen (n -1) remaining

parsePacketData :: BinNum -> BinNum -> (PacketData, BinNum)
parsePacketData [1, 0, 0] binData = let (literal, rest) = parseLiteral binData in (Literal literal, rest)
parsePacketData typeId (0 : binData) = (Op (toDecimal typeId) subPackets, remaining)
  where
    (subPacketLenBin, rest) = splitAt 15 binData
    subPacketLen = toDecimal subPacketLenBin
    goalLen = length rest - subPacketLen
    (subPackets, remaining) = parseUntilRestIsLen goalLen rest
parsePacketData typeId (1 : binData) = (Op (toDecimal typeId) subPackets, remaining)
  where
    (numPackets, rest) = splitAt 11 binData
    (subPackets, remaining) = parseNPackets (toDecimal numPackets) rest
parsePacketData typeId notHandled = error ("sad: " ++ show (typeId, notHandled))

parsePacket :: BinNum -> (Packet, BinNum)
parsePacket binNum | all (== 0) binNum = (Empty, [])
parsePacket binNum = (Packet {version = toDecimal version, packetData = parsedData}, restBits)
  where
    (version, rest) = splitAt 3 binNum
    (typeId, rest') = splitAt 3 rest
    (parsedData, restBits) = parsePacketData typeId rest'

part1 :: IO ()
part1 = do
  num <- readBin
  -- let num = getBin "EE00D40C823060"
  print num
  print $ parsePacket num
  return ()

part2 :: IO ()
part2 = do
  [input] <- readInputLines
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
