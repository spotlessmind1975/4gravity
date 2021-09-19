' *****************************************************************************
' * 4 (FOUR) GRAVITY - a connect 4 clone for retrocomputers                   *
' *****************************************************************************
' * Copyright 2021 Marco Spedaletti (asimov@mclink.it)
' * Powered by ugBASIC (https://ugbasic.iwashere.eu/)
' *
' * Licensed under the Apache License, Version 2.0 (the "License");
' * you may not use this file except in compliance with the License.
' * You may obtain a copy of the License at
' *
' * http://www.apache.org/licenses/LICENSE-2.0
' *
' * Unless required by applicable law or agreed to in writing, software
' * distributed under the License is distributed on an "AS IS" BASIS,
' * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
' * See the License for the specific language governing permissions and
' * limitations under the License.
' *----------------------------------------------------------------------------
' * Concesso in licenza secondo i termini della Licenza Apache, versione 2.0
' * (la "Licenza"); è proibito usare questo file se non in conformità alla
' * Licenza. Una copia della Licenza è disponibile all'indirizzo:
' *
' * http://www.apache.org/licenses/LICENSE-2.0
' *
' * Se non richiesto dalla legislazione vigente o concordato per iscritto,
' * il software distribuito nei termini della Licenza è distribuito
' * "COSì COM'è", SENZA GARANZIE O CONDIZIONI DI ALCUN TIPO, esplicite o
' * implicite. Consultare la Licenza per il testo specifico che regola le
' * autorizzazioni e le limitazioni previste dalla medesima.
' ****************************************************************************/

' ============================================================================
' GAME CONSTANTS
' ============================================================================

' Number of rows for the entire playfield. 
' Note that it is 0 based, so the last row is (rows-1).
CONST rows = 6

' Number of columns for the entire playfield. 
' Note that it is 0 based, so the last column is (columns-1).
CONST columns = 7

' Number of tokens that can be employed during the entire game.
' Currently, will be rows x columns.
CONST tokens = rows * columns

' Number of tokens "in a row" to win.
CONST tokensInARowToWin = 4

' Value that represent a free cell inside the playfield.
CONST freeCell = $ff

' Value that represent an unused token inside the tokens' set.
CONST unusedToken = $ff

' This is the constants used to distinguish between tokens of the
' first and the second player.
CONST tokenA = 0
CONST tokenB = 1

' This is the constants used to distinguish between first 
' and second player.
CONST player1 = 1
CONST player2 = 2

' ============================================================================
' DATA SECTION
' ============================================================================

' This is the matrix that represent the entire playfield. So each cell is
' represented by a single unsigned byte (BYTE, 0...255). Each cell is
' filled with "freeCell" contant (if the cell is free) or with the "color"
' of the token ("tokenA", "tokenB"). This is the internal representation
' used by the various algorithms, to understand if the game is over or not.
DIM playfield AS BYTE WITH freeCell (columns,rows)

' These vectors will contain the actual X, Y and Color for each token.
' Note that they can be filled by "unusedToken" value, that is used as a
' placemark to tell to the various algorithms that the token is not used.
DIM tokenX AS BYTE WITH unusedToken (tokens)
DIM tokenY AS BYTE WITH unusedToken (tokens)
DIM tokenC AS BYTE WITH unusedToken (tokens)

' These vectors will help to calculate the effective winner, without
' exploring the playfield, that is (relatively) slow.
DIM tokensPerRow AS BYTE (rows,2)
DIM tokensPerColumn AS BYTE (columns,2)
DIM tokensPerDiagonalA AS BYTE (columns+rows,2)
DIM tokensPerDiagonalB AS BYTE (columns+rows,2)

' This variable store the last used token index.
' (we force to be a byte!)
VAR lastUsedToken AS BYTE = unusedToken

' This variable store the player that must current play
currentPlayer = player1

' ============================================================================
' CODE SECTION
' ============================================================================

' ----------------------------------------------------------------------------
' --- STARTUP
' ----------------------------------------------------------------------------

' Let's choose an hires graphical mode with a minimal number of colors,
' and clear the screen, in order to support.
BITMAP ENABLE (320,200,2)
CLS

' Load the graphical resources.
tokenAImage = IMAGE LOAD("resources/tokenA.png")
tokenBImage = IMAGE LOAD("resources/tokenB.png")
emptyImage = IMAGE LOAD("resources/empty.png")

' Precalculate the width and the height of the various images.
' They are always of the same size, so it is sufficient to
' take the first image's dimensions.
imageWidth = IMAGE WIDTH(tokenAImage)
imageHeight = IMAGE HEIGHT(tokenAImage)

' Precalculate offsets in order to put the playfield at the center
' of the screen.
offsetWidth = ( SCREEN WIDTH - ( columns * imageWidth ) ) / 2
offsetHeight = ( SCREEN HEIGHT - ( rows * imageHeight ) ) / 2

' ----------------------------------------------------------------------------
' --- GRAPHICAL PROCEDURES
' ----------------------------------------------------------------------------

' This method is able to draw the movement of a single token.
PROCEDURE drawMovingToken[t]

    SHARED offsetWidth, offsetHeight
    SHARED imageWidth, imageHeight
    SHARED tokenAImage, tokenBImage, emptyImage
    SHARED tokenY, tokenX, tokenC

    x = tokenX(t)
    y = tokenY(t)
    c = tokenC(t)

    previousX = offsetWidth + x*imageWidth

    IF y > 0 THEN
        previousY = offsetHeight + (y-1)*imageHeight
        actualY = previousY + imageHeight
    ELSE
        actualY = offsetHeight + (y)*imageHeight
        previousY = actualY
    ENDIF

    PUT IMAGE emptyImage AT previousX, previousY 

    IF c == tokenA THEN
        PUT IMAGE tokenAImage AT previousX, actualY 
    ELSE
        PUT IMAGE tokenBImage AT previousX, actualY 
    ENDIF

END PROC

' This method is used to draw the entire playfield.
PROCEDURE drawPlayfield

    SHARED emptyImage
    SHARED imageWidth, imageHeight
    SHARED offsetWidth, offsetHeight

    dy = offsetHeight

    FOR y = 0 TO rows-1
        dx = offsetWidth
        FOR x = 0 TO columns-1
            PUT IMAGE emptyImage AT dx, dy
            dx = dx + imageWidth
        NEXT
        dy = dy + imageHeight
    NEXT    

END PROC

' ----------------------------------------------------------------------------
' --- ALGORITHMS PROCEDURES
' ----------------------------------------------------------------------------

' This function will check if a given cell of the playfield is occupied by
' a token (of any color).
PROCEDURE isCellOccupied[x,y]

    SHARED playfield

    RETURN playfield(x,y) <> freeCell

END PROC

' This function will check if a given token has the cell under it 
' occupied or not. 
PROCEDURE isCellOccupiedUnderToken[t]

    SHARED tokenX, tokenY

    RETURN isCellOccupied[tokenX(t),tokenY(t)+1]

END PROC

' This function will check if a given token is on the lower border
' of the playfield.
PROCEDURE isTokenAtBottom[t]

    SHARED tokenY

    RETURN tokenY(t) == (rows-1)

END PROC

' This procedure will move the token by one step down.
PROCEDURE moveTokenDown[t]

    SHARED playfield, tokenX, tokenY, tokenC
    SHARED tokensPerRow, tokensPerColumn, tokensPerDiagonalA, tokensPerDiagonalB

    x = tokenX(t)
    y = tokenY(t)
    c = tokenC(t)

    IF y <> unusedToken THEN
        playfield(x,y) = freeCell
        tokensPerRow(y,c) = tokensPerRow(y,c) - 1 
        tokensPerColumn(x,c) = tokensPerColumn(x,c) - 1 
        tokensPerDiagonalA(x+y,c) = tokensPerDiagonalA(x+y,c) - 1 
        tokensPerDiagonalB((columns-x-1)+y,c) = tokensPerDiagonalB((columns-x-1)+y,c) - 1 
    ENDIF

    y = y + 1
    tokenY(t) = y

    playfield(x,y) = c

    tokensPerRow(y,c) = tokensPerRow(y,c) + 1 
    tokensPerColumn(x,c) = tokensPerColumn(x,c) + 1 
    tokensPerDiagonalA(x+y,c) = tokensPerDiagonalA(x+y,c) + 1 
    tokensPerDiagonalB((columns-x-1)+y,c) = tokensPerDiagonalB((columns-x-1)+y,c) + 1 

    drawMovingToken[t]

END PROC

' This procedure will check if there are the conditions
' to move down a token by one. If so, it will move
' the token down by one step.
PROCEDURE moveToken[t]
    
    SHARED playfield, tokenX, tokenY, lastUsedToken

    EXIT PROC IF t > lastUsedToken
    EXIT PROC IF isTokenAtBottom[t]

    IF NOT isCellOccupiedUnderToken[t] THEN
        CALL moveTokenDown[t]
    ENDIF 

END PROC

' This procedure wiill move every (used) tokens
' if the conditions are met.
PROCEDURE moveTokens

    SHARED lastUsedToken

    EXIT PROC IF lastUsedToken == unusedToken

    FOR i = 0 TO lastUsedToken
        moveToken[i]
    NEXT

END PROC

' This procedure will put (if possible) a token on the playfield.
PROCEDURE putTokenAt[x,c]
    
    SHARED tokenX, tokenY, tokenC, lastUsedToken

    EXIT PROC IF lastUsedToken == tokens

    IF NOT isCellOccupied[x,0] THEN

        INC lastUsedToken
        
        t = lastUsedToken

        tokenX(t) = x
        tokenC(t) = c

        RETURN TRUE

    ENDIF

    RETURN FALSE

END PROC

' This procedure will poll the keyboard for action from player.
PROCEDURE pollKeyboardForColumn

    SHARED currentPlayer

    k = INKEY$

    x = VAL(k)

    IF ( x > 0 ) AND ( x <= columns ) THEN

        IF currentPlayer == player1 THEN
            actualTokenType = tokenA
            nextPlayer = player2
        ELSE
            actualTokenType = tokenB
            nextPlayer = player1
        ENDIF

        IF putTokenAt[(x-1),actualTokenType] THEN
            currentPlayer = nextPlayer
        ENDIF

    ENDIF

END PROC

' This function will check if the player "c" won on a specific
' column "x".
PROCEDURE checkIfPlayerWonOnColumn[c,x]

    SHARED playfield

    VAR count AS BYTE = 0

    FOR y = 0 TO rows-1
        IF ( playfield(x,y) == c ) THEN
            INC count
        ELSE
            count = 0
        ENDIF
    NEXT    

    RETURN count >= 4

END PROC

' This function will check if the player "c" won on a specific
' row "y".
PROCEDURE checkIfPlayerWonOnRow[c,y]

    SHARED playfield

    VAR count AS BYTE = 0

    FOR x = 0 TO columns-1
        IF ( playfield(x,y) == c ) THEN
            INC count
        ELSE
            count = 0
        ENDIF
    NEXT    

    RETURN count >= 4

END PROC

' This function will check if the player "c" won on a specific
' diagonal "A".
PROCEDURE checkIfPlayerWonOnDiagonalA[c,d]

    SHARED playfield

    VAR count AS BYTE = 0

    FOR x = 0 TO columns-1
        IF x+d < rows THEN
            IF ( playfield(x,x+d) == c ) THEN
                INC count
            ELSE
                count = 0
            ENDIF
        ENDIF
    NEXT    

    RETURN count >= 4

END PROC

' This function will check if the player "c" won on a specific
' diagonal "B".
PROCEDURE checkIfPlayerWonOnDiagonalB[c,d]

    SHARED playfield

    VAR count AS BYTE = 0

    FOR x = 0 TO columns-1
        IF (columns-x-1)+d < rows THEN
            IF ( playfield(x,(columns-x-1)+d) == c ) THEN
                INC count
            ELSE
                count = 0
            ENDIF
        ENDIF
    NEXT    

    RETURN count >= 4

END PROC

' This procedure will check if any player has won
PROCEDURE checkIfPlayerWon

    SHARED lastUsedToken, tokensPerRow, tokensPerColumn, tokensPerDiagonalA, tokensPerDiagonalB

    result = FALSE

    IF lastUsedToken == unusedToken THEN
        RETURN FALSE
    ENDIF

    ' FOR y = 0 TO rows-1
    '     IF ( tokensPerRow(y,tokenA) >= 4 ) THEN
    '         IF checkIfPlayerWonOnRow[tokenA,y] THEN
    '             RETURN player1
    '         ENDIF
    '     ENDIF
    '     IF ( tokensPerRow(y,tokenB) >= 4 ) THEN
    '         IF checkIfPlayerWonOnRow[tokenB,y] THEN
    '             RETURN player2
    '         ENDIF
    '     ENDIF
    '     LOCATE 1,20: PRINT "y = ";y
    ' NEXT    

    ' FOR x = 0 TO columns-1
    '     IF ( tokensPerColumn(x,tokenA) >= 4 ) THEN
    '         IF checkIfPlayerWonOnColumn[tokenA,x] THEN
    '             RETURN player1
    '         ENDIF
    '     ENDIF
    '     IF ( tokensPerColumn(x,tokenB) >= 4 ) THEN
    '         IF checkIfPlayerWonOnColumn[tokenB,x] THEN
    '             RETURN player2
    '         ENDIF
    '     ENDIF
    ' NEXT    

    ' FOR x = 0 TO (rows+columns)-1
    '     IF ( tokensPerDiagonalA(x,tokenA) >= 4 ) THEN
    '         IF checkIfPlayerWonOnDiagonalA[tokenA,x] THEN
    '             RETURN player1
    '         ENDIF
    '     ENDIF
    '     IF ( tokensPerDiagonalA(x,tokenB) >= 4 ) THEN
    '         IF checkIfPlayerWonOnDiagonalA[tokenB,x] THEN
    '             RETURN player2
    '         ENDIF
    '     ENDIF
    '     LOCATE 1,22: PRINT "x = ";x
    ' NEXT    

    ' FOR x = 0 TO (rows+columns)-1
    '     IF ( tokensPerDiagonalB(x,tokenA) >= 4 ) THEN
    '         IF checkIfPlayerWonOnDiagonalB[tokenA,x] THEN
    '             RETURN player1
    '         ENDIF
    '     ENDIF
    '     IF ( tokensPerDiagonalB(x,tokenB) >= 4 ) THEN
    '         IF checkIfPlayerWonOnDiagonalB[tokenB,x] THEN
    '             RETURN player2
    '         ENDIF
    '     ENDIF
    '     LOCATE 1,23: PRINT "x = ";x
    ' NEXT    

    RETURN FALSE

END PROC

' ----------------------------------------------------------------------------
' --- MAIN LOOP
' ----------------------------------------------------------------------------

' Initial playfield
CALL drawPlayfield

' ----------------------------------------------------------------------------
' --- MAIN LOOP
' ----------------------------------------------------------------------------

BEGIN GAMELOOP

    CALL pollKeyboardForColumn

    CALL moveTokens

    playerWon = checkIfPlayerWon[]

    IF playerWon == player1 THEN
        LOCATE 1,1: CENTER "player 1 win!"
        HALT
    ELSE IF playerWon == player2 THEN
        LOCATE 1,1: PRINT "player 2 win! (";playerWon;",";player2;")"
        HALT
    ENDIF

END GAMELOOP
