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
' COMPILER OPTIONS (in order to have more spare space)
' ============================================================================

' We ask to define at most 10 independent strings.
' This will free about 1Kb
DEFINE STRING COUNT 10

' We ask to use at most 64 bytes for strings.
' This will free about 2Kb
DEFINE STRING SPACE 64

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
' Currently, it will be rows x columns.
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

' These are the constants used to distinguish between first 
' and second player.
CONST noPlayer = 0
CONST player1 = 1
CONST player2 = 2

' These are the constants used to distinguish between human and computer
CONST human = 1
CONST computer = 2

' Constant labels
CONST player1Label = "PLAYER 1"
CONST player2Label = "PLAYER 2"

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

' This variable store if the player1 is an human or a computer
player1Type = human

' This variable store if the player2 is an human or a computer
player2Type = human

' This variable store the current frame for arrow and direction
arrow = 0
arrowDirection = 1

' ============================================================================
' CODE SECTION
' ============================================================================

' ----------------------------------------------------------------------------
' --- STARTUP
' ----------------------------------------------------------------------------

' Let's choose an hires graphical mode with enough number of colors,
' and let's clear the screen with a black border.
BITMAP ENABLE (16)
CLS
COLOR BORDER BLACK

' We must add constants on this point because only here we have
' informations about graphical mode selected.
CONST player1MenuLabel = IF(SCREEN HEIGHT >= 100, "[1] HUMAN / [2] COMPUTER", "1=HUMAN 2=PC")
CONST player2MenuLabel = IF(SCREEN HEIGHT >= 100, "[3] HUMAN / [4] COMPUTER", "3=HUMAN 4=PC")

' Assign all the graphical resources. Note the use of ":=" direct assing
' operator. This is needed to avoid useless copies.
titleImage := IMAGE LOAD("resources/title.png")
tokenAImage := IMAGE LOAD("resources/tokenAC.png")
tokenBImage := IMAGE LOAD("resources/tokenBC.png")
emptyImage := IMAGE LOAD("resources/emptyC.png")
player1Image := IMAGE LOAD("resources/player1.png")
player2Image := IMAGE LOAD("resources/player2.png")
computer1Image := IMAGE LOAD("resources/computer1.png")
computer2Image := IMAGE LOAD("resources/computer2.png")
arrow1Image := IMAGE LOAD("resources/arrow1.png")
arrow2Image := IMAGE LOAD("resources/arrow2.png")
arrow3Image := IMAGE LOAD("resources/arrow3.png")
clearImage := IMAGE LOAD("resources/clear.png")

' Precalculate the width and the height of the various images.
' They are always of the same size, so it is sufficient to
' take the first image's dimensions.
POSITIVE CONST imageWidth = IMAGE WIDTH(tokenAImage)
POSITIVE CONST imageHeight = IMAGE HEIGHT(tokenAImage)

' Precalculate offsets in order to put the playfield at the center
' of the screen.
POSITIVE CONST offsetWidth = ( SCREEN WIDTH - ( columns * imageWidth ) ) / 2
POSITIVE CONST offsetHeight = ( SCREEN HEIGHT - ( rows * imageHeight ) ) / 2

' Offset of the main title
POSITIVE CONST offsetTitleX = ( SCREEN WIDTH - IMAGE WIDTH(titleImage) ) / 2
POSITIVE CONST offsetTitleY = ( SCREEN HEIGHT - IMAGE HEIGHT(titleImage) - 2 * IMAGE HEIGHT(player1Image) - 4 * 8 ) / 2

' Offset of the main title (final)
POSITIVE CONST offsetYTitle = offsetTitleY 

' Precalculate offsets of arrows
POSITIVE CONST arrowX2 = SCREEN WIDTH - IMAGE WIDTH(arrow1Image)
POSITIVE CONST arrowY = SCREEN HEIGHT - IMAGE HEIGHT(player1Image) - IMAGE HEIGHT(arrow1Image)

' Precalculate offsets of players
POSITIVE CONST offsetYPlayers = SCREEN HEIGHT - IMAGE HEIGHT(player1Image)
POSITIVE CONST offsetXPlayer2 = SCREEN WIDTH - IMAGE WIDTH(player1Image)

' Precalculate offsets of menu entries
CONST offsetXMainMenuPlayerv IN (0,SCREEN WIDTH) = offsetTitleX - IF(offsetTitleX>( IMAGE WIDTH(player1Image) / 2 ), ( IMAGE WIDTH(player1Image) / 2 ), 0 )
CONST offsetXMainMenu IN (0,SCREEN WIDTH) = ( offsetTitleX + IMAGE WIDTH(player1Image) ) / 8 + 6
CONST offsetYMainMenu IN (0,SCREEN HEIGHT) = offsetTitleY + IMAGE HEIGHT(titleImage) + 8
CONST offsetYMainMenu2 IN (0,SCREEN HEIGHT)  = offsetYMainMenu + IMAGE HEIGHT(player1Image) + 8

POSITIVE CONST screenHeight = SCREEN HEIGHT

' For commodity, all those variables are global:
GLOBAL playfield, tokenX, tokenY, tokenC
GLOBAL lastUsedToken, lastUsedColumn, currentPlayer, previousPlayer
GLOBAL tokenAImage, tokenBImage, emptyImage
GLOBAL titleImage, player1Image, player2Image
GLOBAL arrow1Image, arrow2Image, arrow3Image
GLOBAL computer1Image, computer2Image
GLOBAL arrow, arrowDirection
GLOBAL clearImage
GLOBAL player1Type, player2Type

' ----------------------------------------------------------------------------
' --- GRAPHICAL PROCEDURES
' ----------------------------------------------------------------------------

' This procedure is responsible for initializing all game variables 
' before each game. Furthermore, it will also initialize the random number 
' generation system. .
PROCEDURE gameInit

    ' Initialize the random number generator
    RANDOMIZE TIMER
    
    ' Fill matrix with all free cells
    FILL playfield WITH freeCell

    ' Fill vectors with unused tokens
    FILL tokenX WITH unusedToken
    FILL tokenY WITH unusedToken
    FILL tokenC WITH unusedToken

    ' No token has been used.
    lastUsedToken = # unusedToken

    ' No column has been filled.
    lastUsedColumn = # unusedToken

    ' Player 1 starts always as first player.
    ' Next player (or, previous player) is the second player
    currentPlayer = # player1
    previousPlayer = # player2

    ' Nobody wins
    playerWon = # noPlayer

    ' Both players start as humans
    player1Type = # human
    player2Type = # human

    ' Reset the arrow animation.
    arrow = # 0
    arrowDirection = # 1

END PROC

' This method is able to draw the movement of a single token.
PROCEDURE drawMovingToken[t]

    ' Let's take coordinates of the token and the token type.
    x = tokenX(t)
    y = tokenY(t)
    c = tokenC(t)

    ' The abscissa is fixed, and it is calculated as the pixel
    ' that starts the playfield plus the relative column given.
    ' Each column of the playfield is large as a single token.
    previousX = offsetWidth + x*imageWidth

    ' If the ordinate is greater than zero, it means that
    ' the token is slowly falling on the column...
    IF y > 0 THEN
        ' ... so we calculate the previous position of the
        ' token, and the actual as the previous plus the
        ' the height of a token.
        previousY = offsetHeight + (y-1)*imageHeight
        actualY = previousY + imageHeight
    ELSE
        ' Otherwise, the actual and previous position are the
        ' very same. This is needed to draw the token as soon
        ' as it is inserted in the playfield.
        actualY = offsetHeight + (y)*imageHeight
        previousY = actualY
    ENDIF

    ' Let's clear the previous position of the token.
    PUT IMAGE emptyImage AT previousX, previousY 

    ' Now we can draw the token at the actual position.
    ' We must use the correct image.
    IF c == tokenA THEN
        PUT IMAGE tokenAImage AT previousX, actualY 
    ELSE
        PUT IMAGE tokenBImage AT previousX, actualY 
    ENDIF

END PROC

' This procedure is used to draw the game plan. 
' As it is drawn only once, it is a very 
' simple routine.
PROCEDURE drawPlayfield

    ' Clear the screen    
    CLS

    IF ( screenHeight >= 100 ) THEN
        ' Put the title "4 GRAVITY!" at the head of the screen.
        PUT IMAGE titleImage AT offsetTitleX, 0 
    ENDIF

    ' To draw the various empty squares of the game, we iterate for the rows 
    ' and for the columns. To avoid doing multiplications (which are usually 
    ' slow operations) we use simple increments and reassignments.
    dy = # offsetHeight
    FOR y = 0 TO rows-1
        dx = # offsetWidth
        FOR x = 0 TO columns-1
            PUT IMAGE emptyImage AT dx, dy
            dx = dx + imageWidth
        NEXT
        dy = dy + imageHeight
    NEXT    

    ' Now let's draw the two player icons, on the left (first player, red) 
    ' and on the right of the screen (second player, yellow).
    ' Clearly, we find ourselves in the situation of having to distinguish
    ' whether the player is a human or a computer. This distinction is 
    ' necessary for drawing using the correct icon, for both first and
    ' second player.
    IF player1Type == human THEN
        PUT IMAGE player1Image AT 0, offsetYPlayers
    ELSE
        PUT IMAGE computer1Image AT 0, y
    ENDIF

    IF player2Type == human THEN
        PUT IMAGE player2Image AT offsetXPlayer2, offsetYPlayers
    ELSE
        PUT IMAGE computer2Image AT offsetXPlayer2, offsetYPlayers
    ENDIF

    IF ( screenHeight >= 100 ) THEN
        ' We characterize the player with his/her name.
        PEN RED
        LOCATE 6, 24: PRINT player1Label;

        PEN YELLOW
        LOCATE 25, 24: PRINT player2Label;
    ENDIF

END PROC

' This procedure is used to draw the arrow animation.
PROCEDURE drawArrowAnimation

    ' To ensure a constant speed animation, we memorize the moment 
    ' in time when we drew the last frame. By doing so, we ensure 
    ' that the animation will always be at the same speed.
    SHARED lastTiming

    ' So, the first time we must register this time
    IF lastTiming == 0 THEN
        lastTiming = TI

    ' On the other times...
    ELSE

        ' When at least 1/60 of a second has passed, then we are 
        ' allowed to draw the new arrow frame, if available. 
        IF ( TI - lastTiming ) > 1 THEN

            ' The animation is "bounce", so as soon as we get to the
            ' last frame we have to go back in the animation.
            IF arrowDirection == 1 THEN

                ' Let's increment the number of the frame.
                INC arrow

                ' On the last frame, we revert direction.
                IF arrow == 30 THEN
                    arrowDirection = # 0
                ENDIF

            ELSE

                ' Let's decrement the number of the frame.
                DEC arrow

                ' On the first frame, we revert direction.
                IF arrow == 0 THEN
                    arrowDirection = # 1
                ENDIF

            ENDIF

            ' We delete the arrow of the player who is not playing now.
            IF currentPlayer == player1 THEN 
                x = # 0
                PUT IMAGE clearImage AT arrowX2, arrowY 
            ELSE
                x = # arrowX2
                PUT IMAGE clearImage AT 0, arrowY
            ENDIF

            ' We draw, if there is the possibility, the frame of the arrow.
            IF arrow == 21 THEN
                PUT IMAGE arrow3Image AT x, arrowY
            ELSE IF arrow == 11 THEN
                PUT IMAGE arrow2Image AT x, arrowY
            ELSE IF arrow == 1 THEN
                PUT IMAGE arrow3Image AT x, arrowY
            ENDIF

            ' Update timings
            lastTiming = TI

        ENDIF

    ENDIF

END PROC

' This procedure updates the color of the numbers above the columns 
' to indicate which player is currently playing.
PROCEDURE drawPlayerStatus

    IF ( screenHeight >= 100 ) THEN
        ' The color RED for the first player 
        ' and YELLOW for the second player.
        IF currentPlayer == player1 THEN 
            PEN RED
        ELSE
            PEN YELLOW
        ENDIF

        LOCATE 1, 5: CENTER "  1   2   3   4   5   6   7"
    ENDIF

END PROC

PROCEDURE informationalMessages ON C64

    ' To ensure a constant speed animation of informational
    ' title, we memorize the moment in time when we drew the 
    ' last informational title. By doing so, we ensure 
    ' that the animation will always be at the same speed.
    SHARED lastTiming

    IF ( screenHeight >= 100 ) THEN
        IF (TI-lastTiming) > 600 THEN
            IF m == 0 THEN
                PEN CYAN
                LOCATE 1,yt: CENTER " SEE MORE GAMES AT "
                LOCATE 1,yt+1: CENTER "https://retroprogramming.iwashere.eu/"
                m = 1
            ELSE
                PEN BLUE
                LOCATE 1,yt: CENTER "POWERED BY ugBASIC"
                LOCATE 1,yt+1: CENTER "     https://ugbasic.iwashere.eu/    "
                m = 0
            ENDIF
            lastTiming = TI
        ENDIF
    ENDIF

END PROC


' This procedure deals with designing the initial screen, 
' including the menu with which the player can choose the 
' game mode (two human players, player against computer, 
' computer against computer).
PROCEDURE drawTitleScreen

    ' Take note of which informational message we are
    ' going to show (0 = see more games; 1 = ugbasic)
    m = 0

    ' Take note if the SPACE key has been pressed,
    ' and the game can be started as well.
    done = FALSE

    ' Let's clear the screen
    CLS

    ' We calculate the position in which to write the text. 
    ' In a nutshell, we place ourselves on the right of the 
    ' player icon.

    ' The title, on the other hand, we position it centrally 
    ' vertically on the screen, but moved slightly upwards.
    y = offsetYTitle

    ' Draw the title ("4 GRAVITY!")
    PUT IMAGE titleImage AT offsetTitleX, y

    ' Clear the keyboard buffer, in order to avoid to
    ' detect any WAIT KEY key press as a key pressed.
    CLEAR KEY

    ' Let's define the variable that will wait for a key press.
    k = ""

    ' Here we start a loop where we will stay until the player 
    ' has pressed the SPACE key.
    REPEAT

        ' The main color of the writing will be white.
        PEN WHITE

        ' This is the position from which to start writing.
        ' It corresponds to the lower edge of the title, 
        ' from which we move down to make room for the icons.
        ' We calculate manually the equivalend text position.
        yt = offsetYMainMenu / 8

        ' We design a different icon depending on whether 
        ' it is a human player or a computer (player 1).
        IF player1Type == human THEN
            PUT IMAGE player1Image AT offsetXMainMenuPlayer, offsetYMainMenu
        ELSE
            PUT IMAGE computer1Image AT offsetXMainMenuPlayer, offsetYMainMenu
        ENDIF

        LOCATE offsetXMainMenu,yt: PRINT player1MenuLabel;

        ' This is the next position from which to start writing.
        y = offsetYMainMenu2
        ' We calculate manually the equivalend text position.
        yt = offsetYMainMenu2 / 8

        ' We design a different icon depending on whether 
        ' it is a human player or a computer (player 2).
        IF player2Type == human THEN
            PUT IMAGE player2Image AT offsetXMainMenuPlayer, offsetYMainMenu2
        ELSE
            PUT IMAGE computer2Image AT offsetXMainMenuPlayer, offsetYMainMenu2
        ENDIF

        LOCATE offsetXMainMenu,yt: PRINT player2MenuLabel;

        INC yt
        INC yt

        IF ( screenHeight >= 100 ) THEN

            INC yt

        ELSE

        ENDIF

        ' Let's suggest to press the SPACE key to PLAY!
        LOCATE 10,yt: CENTER "[SPACE] TO PLAY"

        INC yt
        INC yt

        ' A loop to wait for a valid key.
        DO

            k = INKEY$

            ' While waiting for a button to be pressed, 
            ' we offer a couple of informational messages.
            CALL informationalMessages

            IF k == " " THEN
                PUT IMAGE computer2Image AT offsetXMainMenuPlayer, offsetYMainMenu2
            ENDIF

        UNTIL k<>""

        ' SPACE equals START GAME!
        IF k == " " THEN
            done = TRUE
        ELSE

            ' Let's check the key pressed (it is a number?)
            v = VAL(k)

            IF v == 1 THEN
                player1Type = human
            ELSE IF v == 2 THEN
                player1Type = computer
            ELSE IF v == 3 THEN
                player2Type = human
            ELSE IF v == 4 THEN
                player2Type = computer
            ENDIF
        ENDIF

        NOP

    UNTIL done

END PROC

' This procedure deals with designing the final screen.
PROCEDURE drawFinalScreen[p]

    ' Clear the screen
    CLS
    
    ' The title, on the other hand, we position it centrally 
    ' vertically on the screen, but moved slightly upwards.
    ' Draw the title ("4 GRAVITY!")
    PUT IMAGE titleImage AT offsetTitleX, offsetYTitle

    ' Calculate the position where to write
    y = offsetYTitle + 2 * IMAGE HEIGHT(titleImage)
    yt = y / 8

    ' Position the writing and...
    LOCATE 1,yt

    ' ... if player 1 wins...
    IF p == player1 THEN

        PEN RED
        CENTER "PLAYER 1 WINS" 

    ' ... if player 2 wins...
    ELSE IF p == player2 THEN

        PEN YELLOW
        CENTER "PLAYER 2 WINS" 

    ' ... if nobody wins...
    ELSE

        PEN WHITE
        CENTER "GAME TIE" 

    ENDIF

    ' ' Suggest to press any key to start.
    LOCATE 10,yt + 4: CENTER "*ANY KEY* TO CONTINUE"

    WAIT KEY

END PROC

' ' ----------------------------------------------------------------------------
' ' --- ALGORITHMS PROCEDURES
' ' ----------------------------------------------------------------------------

' This procedure will move the token by one step down.
PROCEDURE moveTokenDown[t]

    ' Let's take coordinates of the token and the token type.
    x = tokenX(t)
    y = tokenY(t)
    c = tokenC(t)

    ' If the ordinate is valid, then it means that we have
    ' to free the actual position on the playfield.
    IF y <> unusedToken THEN
        playfield(x,y) = freeCell
    ENDIF

    ' Move to the next ordinate.
    y = y + 1

    ' Save the new position.
    tokenY(t) = y

    ' Occupy the playfield cell.
    playfield(x,y) = c

    ' Now we can draw the movement on the graphical playfield.
    drawMovingToken[t]

END PROC

' This procedure will check if there are the conditions
' to move down a token by one cell. If so, it will move
' the token down by one step.
PROCEDURE moveToken[t]

    ' The token cannot be moved if it is not currently used.    
    EXIT PROC WITH FALSE IF t > lastUsedToken

    ' The token cannot be moved if it is on the last position.
    EXIT PROC WITH FALSE IF tokenY(t) == (rows-1)

    ' The token can be moved only if the next (vertical) cell
    ' is free. In that case...
    IF playfield(tokenX(t),tokenY(t)+1) == freeCell THEN

        ' ... move the token down by one position!
        CALL moveTokenDown[t]

        ' We communicate to the caller that the token has been
        ' moved. This information will be used to avoid to
        ' make any check while tokens are moving.
        RETURN TRUE
    ELSE
        ' We communicate to the caller that the token has NOT been moved.
        RETURN FALSE
    ENDIF 

END PROC

' This procedure wiill move every (used) tokens
' if the conditions are met.
PROCEDURE moveTokens

    ' There are not used tokens. So we communicate to the caller
    ' that no token has been moved. This information will be used
    ' to avoid to make any check while tokens are moving.
    EXIT PROC WITH FALSE IF lastUsedToken == unusedToken

    ' Has any token been moved?
    anyMovedToken = FALSE

    ' Take a look for every used token: is there any token
    ' that must be moved? 
    FOR i = 0 TO lastUsedToken
        ' If so, the infomation about the fact that has
        ' been moved will be retrieved and returned back.
        anyMovedToken = anyMovedToken OR moveToken[i]
    NEXT

    RETURN anyMovedToken

END PROC

' This procedure will put (if possible) a token on the playfield.
PROCEDURE putTokenAt[x,c]
    
    ' No more token available, so... exit!
    EXIT PROC WITH FALSE IF lastUsedToken == tokens
    
    ' Cannot put a token if another token is moving down...
    EXIT PROC WITH FALSE IF lastUsedColumn <> unusedToken

    ' If the given column is free...
    IF playfield(x,0) == freeCell THEN

        ' Take another token, and initialize
        ' its position and type.
        INC lastUsedToken

        t = lastUsedToken

        tokenX(t) = x
        tokenC(t) = c

        lastUsedColumn = x

        ' Return the information that the token has
        ' been put on the playfield.
        RETURN TRUE

    ENDIF

    ' Token cannot be put.
    RETURN FALSE

END PROC

' This is the common procedure between the computer and the human player. 
' The aim is to check if there is a possibility to put a token.
' Of course, he also takes care of changing players if that happens.
PROCEDURE pollToken[x]

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
        ' Little hack to update arrow animation.
        lastTiming = TI: arrowDirection = 1: arrow = 0
    ENDIF

END PROC

' This procedure will poll the computer for action.
' Here is a little mathematical study to do. According to game theory, 
' "connect 4" is not a game that has random components. In fact, it is 
' a game where it is possible to define the winning and losing strategies 
' in a deterministic way. This is where this somewhat "lateral" algorithm 
' comes into play. It is about taking advantage of the principle 
' that randomly choosing a position from among those possible, avoiding 
' repetitions, can guarantee a good winning performance.
PROCEDURE pollComputerForColumn

    ' Avoid to use the very same column already used.
    SHARED lastComputerColumn

    x = ( ( RANDOM BYTE ) MOD columns ) + 1

    IF ( x > 0 ) AND ( x <= columns ) AND ( lastComputerColumn <> x ) THEN
        CALL pollToken[x]
        lastComputerColumn = x
    ENDIF

END PROC

' This procedure will poll the keyboard for action from player.
PROCEDURE pollKeyboardForColumn

    k = INKEY$

    x = VAL(k)

    IF ( x > 0 ) AND ( x <= columns ) THEN
        CALL pollToken[x]
    ENDIF

END PROC

' This routine allows to calculate how many tokens of a certain 
' color there are along a certain line, starting from a specific 
' position. This is partial information, which however tells us 
' if the last move was successful.
PROCEDURE countTokensOfAColorFromXYOnDirection[c,x,y,dx,dy]

    ' Center of counting
    cx = x
    cy = y

    ' Number of tokens of the same value.
    ' Counting myself as 1.
    t = 1

    ' Loop along at most 3 cells
    FOR i=0 TO 3

        ' Is cell occupied by a different token type
        ' or it is empty? Let's stop counting!
        IF playfield(cx,cy) <> c THEN
            RETURN t
        ENDIF

        ' Let's increment the number of tokens.

        INC t

        ' Move along the direction, stopping if
        ' the border of the playfield has been reached.
        cx = cx + dx
        IF ( cx < 0 ) OR ( cx == columns ) THEN 
            EXIT
        ENDIF

        ' Move along the direction, stopping if
        ' the border of the playfield has been reached.
        cy = cy + dy
        IF ( cy < 0 ) OR ( cy == rows ) THEN 
            EXIT
        ENDIF

    NEXT

    ' Return the number of tokens counted.
    RETURN t

END PROC

' This is the overall check procedure, which checks 
' whether the last player won or lost. It is a 
' "divide and conquer" algorithm; together with a 
' check on the last move made.
PROCEDURE checkIfPlayerWon

    ' Nobody can win if no token has been used.
    EXIT PROC WITH FALSE IF lastUsedToken == unusedToken 

    ' Nobody can win if no token has been chosen.
    EXIT PROC WITH FALSE IF lastUsedColumn == unusedToken

    ' Let's take coordinates of the token and the token type.
    c = tokenC(lastUsedToken)
    cx = tokenX(lastUsedToken)
    cy = tokenY(lastUsedToken)

    ' Nobody can win if last token is moving.
    EXIT PROC WITH FALSE IF cy == unusedToken

    ' Has the player won on NORD EAST direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,-1] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on EAST direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,0] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on SOUTH EAST direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,1,1] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on SOUTH direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,0,1] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on SOUTH WEST direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,1] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on NORD direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,0] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Has the player won on NORTH WEST direction?
    IF countTokensOfAColorFromXYOnDirection[c,cx,cy,-1,-1] >= tokensInARowToWin THEN
        RETURN previousPlayer
    ENDIF

    ' Let's reset the used column.
    lastUsedColumn = unusedToken

    RETURN FALSE

END PROC

' ----------------------------------------------------------------------------
' --- MAIN LOOP
' ----------------------------------------------------------------------------

' This is where the main game loop begins.
BEGIN GAMELOOP

    ' Initialize the game
    CALL gameInit

    ' Initial screen (and options)
    CALL drawTitleScreen

    ' Initial playfield
    CALL drawPlayfield

    ' When the game start, nobody wins.    
    playerWon = noPlayer

    ' We repeat this loop until someone has won 
    ' (or all the tokens are gone!).
    REPEAT

        ' Draw the arrow to make clear who is playing
        CALL drawArrowAnimation

        ' If tokens are not moving...
        IF NOT moveTokens[] THEN

            ' Check if somebody wins.
            playerWon = checkIfPlayerWon[]

            ' Update the player status.
            CALL drawPlayerStatus

            ' If nobody has win, asks for the next move.
            IF playerWon == noPlayer THEN
                IF currentPlayer == player1 THEN
                    IF player1Type == human THEN
                        CALL pollKeyboardForColumn
                    ELSE
                        CALL pollComputerForColumn
                    ENDIF
                ELSE
                    IF player2Type == human THEN
                        CALL pollKeyboardForColumn
                    ELSE
                        CALL pollComputerForColumn
                    ENDIF
                ENDIF
            ENDIF

        ENDIF

    UNTIL playerWon <> noPlayer OR lastUsedToken == tokens

    ' Final screen
    CALL drawFinalScreen[playerWon]

END GAMELOOP