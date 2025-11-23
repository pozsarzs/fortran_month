C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C pi.for
C Calculating value of Pi with some methods
C ******************************************************************************

      PROGRAM PI
C VARIABLES
      INTEGER DIN, DOUT
      INTEGER ITER, LOS 
      COMMON /GLBLUN/DIN, DOUT
      COMMON /GLBMNU/IMITEM(4)
      DATA ITER/10/, LOS/41/
C I/O DEVICES
      DATA DIN/5/, DOUT/3/
C     DATA DIN/5/, DOUT/6/
C HEADER
      CALL SPLIT(LOS)
      WRITE(DOUT, 98)
C MENU
   10 CALL SPLIT(LOS)
      WRITE(DOUT, 97) IMITEM(1)
      WRITE(DOUT, 96) IMITEM(2)
      WRITE(DOUT, 93) IMITEM(3)
      WRITE(DOUT, 92) IMITEM(4)
      CALL SPLIT(LOS)
      WRITE(DOUT, 95)
      READ(DIN, 94) I
      WRITE(DOUT, 99)
      N = MENU(I)
      GOTO(60, 20, 21, 22, 10), N
   20 CALL ITERAT(ITER)
      GOTO 10
   21 CALL PIWALL(ITER)
      WRITE(DOUT, 99)
      GOTO 10
   22 CALL PILEIB(ITER)
      WRITE(DOUT, 99)
      GOTO 10
C END OF PROGRAM
   60 WRITE(DOUT, 99)
      STOP
C FORMAT DECLARATIONS
   92 FORMAT(1X, A1, 1X, 20HThe Leibniz's method) 
   93 FORMAT(1X, A1, 1X, 26HThe Wallis product formula)
   94 FORMAT(A1)
   95 FORMAT(1X, 14HPlease select:, 1X)
   96 FORMAT(1X, A1, 1X, 25HInput number of iteration)
   97 FORMAT(1X, A1, 1X, 4HQuit)
   98 FORMAT(42H CALCULATING VALUE OF PI WITH SOME METHODS)
   99 FORMAT(1H )
      END

C *** INITIALIZE GLOBAL ARRAYS ***
      BLOCK DATA
      COMMON /GLBMNU/IMITEM(4)
      DATA IMITEM/1Hq, 1Hn, 1Ha, 1Hb/
      END

C *** WRITE SPLITTER TO CONSOLE ***
      SUBROUTINE SPLIT(LENGTH)
      INTEGER DIN, DOUT, LENGTH
      INTEGER LINE(80)
      INTEGER I
      COMMON /GLBLUN/DIN, DOUT
      DATA LINE /80*1H-/
      WRITE(DOUT, 199) (LINE(I), I = 1, LENGTH)
      RETURN      
  199 FORMAT(1X, 80A1)
      END

C *** RETURN THE NUMBER OF THE SELECTED MENUITEM ***
      INTEGER FUNCTION MENU(SLCT)
      INTEGER SLCT, VALID
      COMMON /GLBMNU/IMITEM(4)
      DATA VALID/5/
      DO 200 I = 1, 4
      IF (SLCT .EQ. IMITEM(I)) VALID = I
  200 CONTINUE
      MENU = VALID
      RETURN
      END

C *** GET NUMBER OF ITERATION
      SUBROUTINE ITERAT(ITER)
      INTEGER DIN, DOUT, LENGTH
      INTEGER ITER
      COMMON /GLBLUN/DIN, DOUT
      WRITE(DOUT, 398)
      WRITE(DOUT, 399)
      READ(DIN, 397, ERR = 320, END = 320) ITER
      IF (ITER .LT. 10) ITER = 10
      RETURN
  320 WRITE(DOUT, 396)
      ITER = 10
      RETURN
  396 FORMAT(1X, 22HWrong value, using 10.)
  397 FORMAT(I5)
  398 FORMAT(1X, 31HNumber of iteration (10-32767):)
  399 FORMAT(1H )
      END

C *** THE WALLIS PRODUCT FORMULA ***
      SUBROUTINE PIWALL(IT)
      INTEGER DIN, DOUT, LENGTH
      INTEGER I, IT 
      REAL R
      DOUBLE PRECISION D
      COMMON /GLBLUN/DIN, DOUT
      D = 1.0
      DO 1100 I = 1, IT
      R = FLOAT(I)
      D = D * (((2 * R) * (2 * R)) / ((2 * R - 1) * (2 * R + 1))) 
      WRITE(DOUT, 1199) I
 1100 CONTINUE
      D = 2 * D
      WRITE(DOUT, 1198) D
      RETURN
 1198 FORMAT(1X, 7HResult:, 1X, F68.66)
 1199 FORMAT(1H+, 5HStep:, 1X, I6)
      END

C *** THE LEIBNIZ'S METHOD ***
      SUBROUTINE PILEIB(IT)
      INTEGER DIN, DOUT, LENGTH
      INTEGER I, IT
      DOUBLE PRECISION D
      COMMON /GLBLUN/DIN, DOUT
      D = 0.0
      DO 2100 I = 2, IT
      IF (MOD(I, 2) .EQ. 0) D = D - (1.0/(2.0 * I - 1))
      IF (MOD(I, 2) .NE. 0) D = D + (1.0/(2.0 * I - 1))
      WRITE(DOUT, 2199) I
 2100 CONTINUE
      D = 4 * (1 + D)
      WRITE(DOUT, 2198) D
      RETURN
 2198 FORMAT(1X, 7HResult:, 1X, F68.66)
 2199 FORMAT(1H+, 5HStep:, 1X, I6)
      END
