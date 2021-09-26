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

' This variable store the last used token index.
' (we force to be a byte!)
VAR lastUsedToken AS BYTE = unusedToken

' This variable store the last used column.
' (we force to be a byte!)
VAR lastUsedColumn AS BYTE = unusedToken

' This variable store the player that must current play
currentPlayer = player1

' This variable store the player that must current wait
previousPlayer = player2

' ============================================================================
' CODE SECTION
' ============================================================================

' ----------------------------------------------------------------------------
' --- STARTUP
' ----------------------------------------------------------------------------

' Let's choose an hires graphical mode with a minimal number of colors,
' and clear the screen, in order to support.
BITMAP ENABLE (16)
CLS

' Load the graphical resources.
tokenAImage = IMAGE LOAD("resources/tokenAC.png")
tokenBImage = IMAGE LOAD("resources/tokenBC.png")
emptyImage = IMAGE LOAD("resources/emptyC.png")

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

' For commodity, all those variables are global:

GLOBAL playfield, tokenX, tokenY, tokenC
GLOBAL lastUsedToken, lastUsedColumn, currentPlayer, previousPlayer
GLOBAL offsetWidth, offsetHeight
GLOBAL imageWidth, imageHeight
GLOBAL tokenAImage, tokenBImage, emptyImage

' This method is able to draw the movement of a single token.
PROCEDURE drawMovingToken[t]

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


' This procedure will move the token by one step down.
PROCEDURE moveTokenDown[t]

    x = tokenX(t)
    y = tokenY(t)
    c = tokenC(t)

    IF y <> unusedToken THEN
        playfield(x,y) = freeCell
    ENDIF

    y = y + 1
    tokenY(t) = y

    playfield(x,y) = c

    drawMovingToken[t]

END PROC

' This procedure will check if there are the conditions
' to move down a token by one. If so, it will move
' the token down by one step.
PROCEDURE moveToken[t]
    
    EXIT PROC IF t > lastUsedToken
    EXIT PROC IF tokenY(t) == (rows-1)

    IF playfield(tokenX(t),tokenY(t)+1) == freeCell THEN
        CALL moveTokenDown[t]
        RETURN TRUE
    ELSE
        RETURN FALSE
    ENDIF 

END PROC

' This procedure wiill move every (used) tokens
' if the conditions are met.
PROCEDURE moveTokens

    anyMovedToken = FALSE

    EXIT PROC IF lastUsedToken == unusedToken

    FOR i = 0 TO lastUsedToken
        anyMovedToken = anyMovedToken OR moveToken[i]
    NEXT

    RETURN anyMovedToken

END PROC

' This procedure will put (if possible) a token on the playfield.
PROCEDURE putTokenAt[x,c]
    
    EXIT PROC IF lastUsedToken == tokens
    
    IF playfield(x,0) == freeCell THEN

        INC lastUsedToken
        
        t = lastUsedToken

        tokenX(t) = x
        tokenC(t) = c

        lastUsedColumn = x

        RETURN TRUE

    ENDIF

    RETURN FALSE

END PROC

' This procedure will poll the keyboard for action from player.
PROCEDURE pollKeyboardForColumn

    k = INKEY$

    x = VAL(k)

    IF ( x > 0 ) AND ( x <= columns ) THEN

        IF currentPlayer == player1 THEN
            actualTokenType = tokenA
            nextPlayer = player2
            previousPlayer = player1
        ELSE
            actualTokenType = tokenB
            nextPlayer = player1
            previousPlayer = player2
        ENDIF

        IF putTokenAt[(x-1),actualTokenType] THEN
            currentPlayer = nextPlayer
        ENDIF

    ENDIF

END PROC

PROCEDURE countTokensOfAColorFromXYOnDirection[c,x,y,dx,dy]

    cx = x
    cy = y
    t = 0

    FOR i=0 TO 3
        IF playfield(cx,cy) <> c THEN
            RETURN FALSE
        ENDIF
        t = t + 1
        cx = cx + dx
        IF ( cx < 0 ) OR ( cx == columns ) THEN 
            EXIT
        ENDIF
        cy = cy + dy
        IF ( cy < 0 ) OR ( cy == rows ) THEN 
            EXIT
        ENDIF
    NEXT

    RETURN t > 3

END PROC

' This procedure will check if any player has won
PROCEDURE checkIfPlayerWon

    result = FALSE

    IF lastUsedToken == unusedToken THEN
        RETURN FALSE
    ENDIF

    IF lastUsedColumn == unusedToken THEN
        RETURN FALSE
    ENDIF

    c = tokenC(lastUsedToken)
    cx = tokenX(lastUsedToken)
    cy = tokenY(lastUsedToken)

    IF cy == unusedToken THEN
        RETURN FALSE
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,-1] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,0] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,1] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,0,1] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,1] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,0] THEN
        RETURN previousPlayer
    ENDIF

    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,-1] THEN
        RETURN previousPlayer
    ENDIF

    lastUsedColumn = unusedToken

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

    IF NOT moveTokens[] THEN

        playerWon = checkIfPlayerWon[]

        IF playerWon == player1 THEN
            LOCATE 1,1: CENTER "player 1 win!"
            HALT
        ELSE IF playerWon == player2 THEN
            LOCATE 1,1: PRINT "player 2 win! (";playerWon;",";player2;")"
            HALT
        ENDIF
    ENDIF

END GAMELOOP
