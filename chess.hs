module Chess where

import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Bits
import Data.Char
import Data.Word (Word64)
import Numeric (showIntAtBase)

type Bitboard = Word64

data Player = White | Black deriving(Eq,Enum,Ord)
data Board = Board {
                whitePawns :: Bitboard,
                whiteRooks :: Bitboard,
                whiteKnights :: Bitboard,
                whiteBishops :: Bitboard,
                whiteQueens :: Bitboard,
                whiteKing :: Bitboard,
                blackPawns :: Bitboard,
                blackRooks :: Bitboard,
                blackKnights :: Bitboard,
                blackBishops :: Bitboard,
                blackQueens :: Bitboard,
                blackKing :: Bitboard,
                allWhitePieces :: Bitboard,
                allBlackPieces :: Bitboard,
                allPieces :: Bitboard
} deriving (Show)

fenString = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
{-fenString = "rnbqkbnr/pppppppp/PPPPPPPP/pppppppp/PPPPPPPP/RNBQKBNR w KQkq - 0 1"-}

fenToBoard :: String -> Board
fenToBoard str =
    Board _whitePawns _whiteRooks _whiteKnights _whiteBishops _whiteQueens _whiteKing _blackPawns _blackRooks _blackKnights _blackBishops _blackQueens _blackKing _allWhitePieces _allBlackPieces _allPieces
    where
        indexedBoard = zip [0..] $ intercalate "" z
            where 
                x = concatMap (splitOn " ") $ splitOn "/" fenString
                y = reverse $ take 8 x -- only get piece positions
                z = map digitExpand y

                digitExpand = concatMap (\x -> if isNumber x then
                    replicate (read [x] :: Int) '1' else [x])

        piecesToBit c = foldl (\x y->(.|.) x (bit y :: Bitboard)) 0 indexList
            where indexList = map fst $ filter ((==c) . snd) indexedBoard

        _whitePawns     = piecesToBit 'P'
        _whiteRooks     = piecesToBit 'R'
        _whiteKnights   = piecesToBit 'N'
        _whiteBishops   = piecesToBit 'B'
        _whiteQueens    = piecesToBit 'Q'
        _whiteKing      = piecesToBit 'K'
        _blackPawns     = piecesToBit 'p'
        _blackRooks     = piecesToBit 'r'
        _blackKnights   = piecesToBit 'n'
        _blackBishops   = piecesToBit 'b'
        _blackQueens    = piecesToBit 'q'
        _blackKing      = piecesToBit 'k'
        _allWhitePieces = _whitePawns .|. _whiteRooks .|. _whiteKnights .|. _whiteBishops .|. _whiteQueens .|. _whiteKing
        _allBlackPieces = _blackPawns .|. _blackRooks .|. _blackKnights .|. _blackBishops .|. _blackQueens .|. _blackKing
        _allPieces      = _allWhitePieces .|. _allBlackPieces

-- mask: all bits set to 0 except specified row/column
-- clear: complement of mask
maskRank r =
    foldl (\x y->(.|.) x (bit y :: Bitboard)) 0 $ take 8 [8*r..]
clearRank r =
    complement $ maskRank r

maskFile f =
    foldl (\x y->(.|.) x (bit y :: Bitboard)) 0 $ [f,f+8..63]
clearFile f =
    complement $ maskFile f
        
-- UTILITY
indexOccupied i board 
    | (bit i :: Bitboard) .&. (allPieces board) /= 0 = True
    | otherwise = False

listBitboard bb =
    map reverse $ chunksOf 8 $ zeroPad $ showIntAtBase 2 intToDigit bb ""
    where
        zeroPad str = (replicate (64 - (length str)) '0') ++ str

printBitboard bb = 
    putStr . unlines $ listBitboard bb

-- PSEUDO-LEGAL MOVE GENERATION
whitePawnMoves squareIndex board = allMoves
    where
        _allPieces = allPieces board
        not_allPieces = complement _allPieces
        indexWord = bit squareIndex :: Bitboard

        oneStep = (indexWord `shiftL` 8) .&. not_allPieces
        twoStep = ((oneStep .&. maskRank 2) `shiftL` 8) .&. not_allPieces
        forwardMoves = oneStep .|. twoStep

        leftAttack = (indexWord .&. clearFile 0) `shiftL` 7
        rightAttack = (indexWord .&. clearFile 7) `shiftL` 9
        attackMoves = (leftAttack .|. rightAttack) .&. (allBlackPieces board)

        allMoves = forwardMoves .|. attackMoves

blackPawnMoves squareIndex board = allMoves
    where
        _allPieces = allPieces board
        not_allPieces = complement _allPieces
        indexWord = bit squareIndex :: Bitboard

        oneStep = (indexWord `shiftR` 8) .&. not_allPieces
        twoStep = ((oneStep .&. maskRank 5) `shiftR` 8) .&. not_allPieces
        forwardMoves = oneStep .|. twoStep

        leftAttack = (indexWord .&. clearFile 0) `shiftR` 9
        rightAttack = (indexWord .&. clearFile 7) `shiftR` 7
        attackMoves = (leftAttack .|. rightAttack) .&. (allWhitePieces board)

        allMoves = forwardMoves .|. attackMoves

knightMoves squareIndex ownSide board = allMoves
    where
        indexWord = bit squareIndex :: Bitboard

        spot1Clip = clearFile 0 .&. clearFile 1
        spot2Clip = clearFile 0
        spot3Clip = clearFile 7
        spot4Clip = clearFile 6 .&. clearFile 7
        spot5Clip = clearFile 6 .&. clearFile 7
        spot6Clip = clearFile 7
        spot7Clip = clearFile 0
        spot8Clip = clearFile 0 .&. clearFile 1

        spot1 = (indexWord .&. spot1Clip) `shiftL` 6
        spot2 = (indexWord .&. spot2Clip) `shiftL` 15
        spot3 = (indexWord .&. spot3Clip) `shiftL` 17
        spot4 = (indexWord .&. spot4Clip) `shiftL` 10

        spot5 = (indexWord .&. spot5Clip) `shiftR` 6
        spot6 = (indexWord .&. spot6Clip) `shiftR` 15
        spot7 = (indexWord .&. spot7Clip) `shiftR` 17
        spot8 = (indexWord .&. spot8Clip) `shiftR` 10

        allMoves = (spot1 .|. spot2 .|. spot3 .|. spot4 .|. spot5 .|. spot6 .|. spot7 .|. spot8) .&. (complement ownSide)

kingMoves squareIndex ownSide board = allMoves
    where
        indexWord = bit squareIndex :: Bitboard

        clipFile0 = indexWord .&. clearFile 0
        clipFile7 = indexWord .&. clearFile 7

        spot1 = clipFile0 `shiftL` 7
        spot2 = indexWord `shiftL` 8
        spot3 = clipFile7 `shiftL` 9
        spot4 = clipFile7 `shiftL` 1

        spot5 = clipFile7 `shiftR` 7
        spot6 = indexWord `shiftR` 8
        spot7 = clipFile0 `shiftR` 9
        spot8 = clipFile0 `shiftR` 1

        allMoves = (spot1 .|. spot2 .|. spot3 .|. spot4 .|. spot5 .|. spot6 .|. spot7 .|. spot8) .&. (complement ownSide)
