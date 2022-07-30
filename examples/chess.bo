module chess

import ../std/strings
import ../std/assert


type Piece  { Pawn() | Knight() | Bishop() | Castle() | Queen() | King() }
type Colour { White() | Black() }
type Square { Empty() | Taken(Colour, Piece) }
type Board  [8 [8 Square]]


fn index(p Piece)
    switch p
        Pawn();   return 0
        Knight(); return 1
        Bishop(); return 2
        Castle(); return 3
        Queen();  return 4
        King();   return 5


fn string(p Piece) string
    return ["phbcqk"[index(p)]]


fn string(s Square)
    switch s
        Empty();               return " "
        Taken(White(), piece); return string(piece)
        Taken(Black(), piece); return toUpper(string(piece))


let knightMoves = [(1, 2), (2, 1), (-1, 2), (-2, 1), (-1, -2), (-2, -1), (1, -2), (2, -1)]
let bishopMoves = [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (-1, 1), (-2, 2), (-3, 3), (-4, 4), (-5, 5), (-6, 6), (-7, 7), (1, -1), (2, -2), (3, -3), (4, -4), (5, -5), (6, -6), (7, -7), (-1, -1), (-2, -2), (-3, -3), (-4, -4), (-5, -5), (-6, -6), (-7, -7)]
let castleMoves = [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (-1, 0), (-2, 0), (-3, 0), (-4, 0), (-5, 0), (-6, 0), (-7, 0), (0, -1), (0, -2), (0, -3), (0, -4), (0, -5), (0, -6), (0, -7)]
let queenMoves  = [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (-1, 0), (-2, 0), (-3, 0), (-4, 0), (-5, 0), (-6, 0), (-7, 0), (0, -1), (0, -2), (0, -3), (0, -4), (0, -5), (0, -6), (0, -7), (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7), (-1, 1), (-2, 2), (-3, 3), (-4, 4), (-5, 5), (-6, 6), (-7, 7), (1, -1), (2, -2), (3, -3), (4, -4), (5, -5), (6, -6), (7, -7), (-1, -1), (-2, -2), (-3, -3), (-4, -4), (-5, -5), (-6, -6), (-7, -7)]
let kingMoves   = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]


let board = initBoard()



fn initBoard() Board
    let board = zero():Board

    for [i] board[1]; board[1][i] = Taken(Black(), Pawn())
    for [i] board[6]; board[6][i] = Taken(White(), Pawn())

    board[0][0] = Taken(Black(), Castle())
    board[0][1] = Taken(Black(), Knight())
    board[0][2] = Taken(Black(), Bishop())
    board[0][3] = Taken(Black(), Queen())
    board[0][4] = Taken(Black(), King())
    board[0][5] = Taken(Black(), Bishop())
    board[0][6] = Taken(Black(), Knight())
    board[0][7] = Taken(Black(), Castle())

    board[7][0] = Taken(White(), Castle())
    board[7][1] = Taken(White(), Knight())
    board[7][2] = Taken(White(), Bishop())
    board[7][3] = Taken(White(), King())
    board[7][4] = Taken(White(), Queen())
    board[7][5] = Taken(White(), Bishop())
    board[7][6] = Taken(White(), Knight())
    board[7][7] = Taken(White(), Castle())

    return board


fn validMoves(colour Colour, board Board) [(i64, i64, i64, i64)]
    let locations = []
    for [r] board
        for [c] board[r]
            if board[r][c] -> Taken(col, p) | col == colour
                locations <- [(r, c)]

    print(locations)

    let valid = []
    return valid



fn printBoard()
    for [row] board
        print(board[row])

fn main()
    print("play chess")
    printBoard()

    validMoves(White(), board)
