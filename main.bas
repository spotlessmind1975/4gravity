CONST rows = 6
CONST columns = 7
CONST tokens = rows*columns
CONST freeCell = $ff
CONST unusedToken = $ff
CONST tokenA = 1
CONST tokenB = 2

DIM playfield AS BYTE WITH freeCell (columns,rows)
DIM tokenX AS BYTE WITH unusedToken (tokens)
DIM tokenY AS BYTE (tokens)
DIM tokenC AS BYTE (tokens)

lastTokenType = tokenB

BITMAP ENABLE (320,200,2)
CLS

tokenAImage = IMAGE LOAD("resources/tokenA.png")
tokenBImage = IMAGE LOAD("resources/tokenB.png")
emptyImage = IMAGE LOAD("resources/empty.png")

imageWidth = IMAGE WIDTH(tokenAImage)
imageHeight = IMAGE HEIGHT(tokenAImage)

offsetWidth = ( SCREEN WIDTH - ( columns * imageWidth ) ) / 2
offsetHeight = ( SCREEN HEIGHT - ( rows * imageHeight ) ) / 2

PROCEDURE isCellOccupied[x,y]
    SHARED playfield
    RETURN playfield(x,y) <> freeCell
END PROC

PROCEDURE isCellOccupiedUnderToken[t]
    SHARED tokenX, tokenY
    RETURN isCellOccupied[tokenX(t),tokenY(t)+1]
END PROC

PROCEDURE isTokenUsed[t]
    SHARED tokenX
    RETURN tokenX(t) <> unusedToken
END PROC

PROCEDURE isTokenAtBottom[t]
    SHARED tokenY
    RETURN tokenY(t) == (rows-1)
END PROC

PROCEDURE moveTokenDown[t]
    SHARED playfield, tokenX, tokenY
    playfield(tokenX(t),tokenY(t)) = freeCell
    tokenY(t) = tokenY(t) + 1
    playfield(tokenX(t),tokenY(t)) = t
END PROC

PROCEDURE moveToken[t]
    
    SHARED playfield, tokenX, tokenY

    EXIT PROC IF NOT isTokenUsed[t]
    EXIT PROC IF isTokenAtBottom[t]

    IF NOT isCellOccupiedUnderToken[t] THEN
        CALL moveTokenDown[t]
    ENDIF 

END PROC

PROCEDURE moveTokens

    FOR i = 0 TO tokens-1
        IF isTokenUsed[i] THEN
            moveToken[i]
        ENDIF
    NEXT

END PROC

PROCEDURE takeNextUnusedToken

    j = unusedToken

    FOR i = 0 TO tokens-1
        IF NOT isTokenUsed[i] THEN
            j = i
            EXIT
        ENDIF
    NEXT

    RETURN j

END PROC

PROCEDURE putTokenAt[x,c]
    
    SHARED tokenX, tokenY, tokenC

    IF NOT isCellOccupied[x,0] THEN

        t = takeNextUnusedToken[]

        IF t <> unusedToken THEN

            tokenX(t) = x
            tokenC(t) = c
            playfield(x,y) = t

            RETURN TRUE

        ENDIF

    ENDIF

    RETURN FALSE

END PROC

PROCEDURE drawPlayfield

    SHARED playfield
    SHARED tokenAImage, tokenBImage, emptyImage
    SHARED imageWidth, imageHeight
    SHARED offsetWidth, offsetHeight

    FOR y = 0 TO rows-1
        FOR x = 0 TO columns-1
            t = playfield(x,y) 
            IF t == freeCell THEN
                PUT IMAGE emptyImage AT offsetWidth + x*imageWidth, offsetHeight + y*imageHeight
            ELSE 
                IF tokenC(t) == tokenA THEN
                    PUT IMAGE tokenAImage AT offsetWidth + x*imageWidth, offsetHeight + y*imageHeight
                ELSE
                    PUT IMAGE tokenBImage AT offsetWidth + x*imageWidth, offsetHeight + y*imageHeight
                ENDIF
            ENDIF
        NEXT
    NEXT    

END PROC

PROCEDURE pollKeyboardForColumn

    SHARED lastTokenType

    k = INKEY$

    x = VAL(k)

    IF ( x > 0 ) AND ( x <= columns ) THEN

        IF lastTokenType == tokenA THEN
            actualTokenType = tokenB
        ELSE
            actualTokenType = tokenA
        ENDIF

        IF putTokenAt[(x-1),actualTokenType] THEN
            lastTokenType = actualTokenType
        ENDIF

    ENDIF

END PROC

DO

    CALL drawPlayfield
    CALL pollKeyboardForColumn
    CALL moveTokens

LOOP
