namespace FSAI

module Minimax =
    open System
    let boardSize = int 8
    let empty = byte 0
    let white = byte 1
    let black = byte 2
    let valid = byte 3
    let tie = byte 4
    let minValue = -2147483648
    let maxValue = 2147483647

    let directions = [(-1,1); (0,1) ; (1,1)
                      (-1,0);         (1,0)
                      (-1,-1);(0,-1);(1,-1)]
    
    let countCorners (board: byte[,]) (tile: byte) =
        let corners = [board.[0,0];board.[0,7];board.[7,0];board.[7,7]]
        List.filter (fun corner -> corner = tile) corners
        |> List.length

    let isOnBoard x y = 0 <= x && x <= 7 && 0 <= y && y <= 7

    let otherTile tile = 
        if tile = black then 
            white
        else if tile = white then 
            black
        else 
            empty

    let rec validDirection (board: byte[,])(x: int)(y: int)(direction: (int*int)) (tile: byte)=
        if isOnBoard x y then
            if board.[x,y] = otherTile tile then
                let dirX,dirY = direction
                validDirection board (x+dirX) (y+dirY) direction tile
            elif board.[x,y] = tile then
                true
            else 
                false
        else
            false
    let rec loopValidDirection (board: byte[,])(x: int)(y: int)(directionList: (int*int)list)(tile: byte) =
        match directionList with
        |[] -> false
        |head::tail ->
            let dirX,dirY = head
            if validDirection board (x+dirX)(y+dirY) head tile then
                true
            else 
                loopValidDirection board x y tail tile

    let validMove (board: byte[,])(x: int)(y: int)(tile: byte) =
        if board.[x,y] = empty then
            loopValidDirection board x y directions tile
        else 
            false 

    let getValidMoves (board: byte[,])(tile: byte) =
        let moveList = [
            for X in 0..7 do
                for Y in 0..7 do
                    if (validMove board X Y tile) then 
                        yield (X,Y)]
        moveList
               
    let rec getScore (board: byte[,]) tile x y score =
        if x < 8 then
            if y < 8 then
                if board.[x, y] = tile then
                    getScore board tile x (y + 1) (score + 1)
                else
                    getScore board tile x (y + 1) score
            else
                getScore board tile (x + 1) 0 score
        else
            score
                                  

    let getWinner (board: byte[,])=
        let blackScore = getScore board black 0 0 0
        let whiteScore = getScore board white 0 0 0
        if blackScore = 0 || whiteScore = 0 || blackScore + whiteScore = 64 || 
            List.length(getValidMoves board white) + List.length(getValidMoves board black) = 0 then
                if whiteScore > blackScore then 
                    white
                elif blackScore > whiteScore then
                    black
                else 
                    tie
        else 
            empty

        
    let rec whileIsOnBoard(board: byte[,])(tile: byte)(x:int)(y: int)(i: int)(j: int) dirFlippedPieces =
        if isOnBoard x y then
            if board.[x, y] = tile then
                dirFlippedPieces
            else
                if board.[x, y] = otherTile tile then
                    whileIsOnBoard board tile (x-1-i) (y-1-i) i j (dirFlippedPieces@[(x, y)])
                else if board.[x, y] = empty then
                    []
                else
                    whileIsOnBoard board tile (x-1-i) (y-1-j) i j dirFlippedPieces
        else
            dirFlippedPieces

    let rec loopDirections (board: byte[,]) tile moveX moveY i j flippedPieces =
        if i < 3 then
            if j < 3 then
                let x = moveX - 1 + i
                let y = moveY - 1 + j
                if isOnBoard x y && board.[x, y] = otherTile tile then
                    let dirFlippedPieces = [(x, y)]
                    let x1 = x - 1 + i
                    let y1 = y - 1 + j
                    let newDirFlippedPieces = whileIsOnBoard board tile x1 y1 i j dirFlippedPieces
                    loopDirections board tile moveX moveY i (j+1) (flippedPieces@newDirFlippedPieces)
                else loopDirections board tile moveX moveY i (j+1) flippedPieces
            else loopDirections board tile moveX moveY (i+1) 0 flippedPieces
        else flippedPieces

    let getFlippedPieces (board: byte[,]) (move: int*int) (tile: byte) =
        let (moveX, moveY) = move
        if board.[moveX, moveY] = empty then
            loopDirections board tile moveX moveY 0 0 []  
        else
            []

    let evaluation (board: byte[,])=
        let blackScore = getScore board black 0 0 0
        let whiteScore = getScore board white 0 0 0
        let blackMobility = (getValidMoves board black).Length
        let whiteMobility = (getValidMoves board white).Length
        if blackScore = 0 then 
            -200000
        elif whiteScore = 0 then
            200000
        else 
            if blackScore + whiteScore = (boardSize * boardSize) || blackMobility + whiteMobility = 0 then
                if (blackScore < whiteScore) then
                    -100000 - whiteScore + blackScore
                elif (blackScore > whiteScore) then
                    100000 + blackScore - whiteScore
                else 
                    0
            else 
                let firstEvaluation = blackScore - whiteScore
                if blackScore + whiteScore > 55 then
                    (blackScore - whiteScore)
                else  
                    let secondEvaluation = firstEvaluation + (blackMobility - whiteMobility)*10
                    let thirdEvaluation = secondEvaluation + ((countCorners board black) - (countCorners board white))*100
                    thirdEvaluation

    let rec loopFlippedPieces (board: byte[,]) tile i flippedPieces =
        if i < List.length flippedPieces then
            let flippedPiece = List.tryItem i flippedPieces
            let (x, y) = flippedPiece.Value
            board.[x, y] <- tile
            loopFlippedPieces board tile (i+1) flippedPieces
        else
            board

    let makeMove (board: byte[,]) (move: int*int) (tile: byte) =
        let flippedPieces = getFlippedPieces board move tile
        let boardFlippedPieces = loopFlippedPieces board tile 0 flippedPieces
        if not(List.isEmpty flippedPieces) then
            let (moveX, moveY) = move
            boardFlippedPieces.[moveX, moveY] <- tile
            boardFlippedPieces
        else
            boardFlippedPieces

       

    let rec minmax (board: byte[,]) depth alpha beta tile isMaxPlayer =
        if depth = 0 || (getWinner board  <> empty) then
            (evaluation board)
        else
            let bestScore =
                if isMaxPlayer then
                    minValue
                else
                    maxValue

            let rec loopValidMoves (board:byte[,]) (validMoves:(int*int)list) (tile:byte) (isMaxPlayer:bool) (bestScore:int)  (alpha:int) (beta:int) =
                match validMoves with
                | [] -> bestScore
                | head::tail -> 
            
                    let childBoard = makeMove board head tile
                    let nodeScore = minmax childBoard (depth-1) alpha beta (otherTile tile) (not isMaxPlayer)
                    if isMaxPlayer then
                        let newBestScore = max bestScore nodeScore
                        let newAlpha = max newBestScore alpha
                        if beta <= newAlpha then
                            newBestScore
                        else
                            (loopValidMoves board tail tile isMaxPlayer newBestScore newAlpha beta)

                    else
                        let newBestScore = min bestScore nodeScore
                        let newBeta = min newBestScore beta
 
                        if newBeta <= alpha then
                            newBestScore
                        else
                            (loopValidMoves board tail tile isMaxPlayer newBestScore alpha newBeta)

            let validMoves = getValidMoves board tile

            if validMoves.IsEmpty then
                (minmax board depth alpha beta (otherTile tile) (not isMaxPlayer))
            else
                (loopValidMoves board validMoves tile isMaxPlayer bestScore alpha beta)
    