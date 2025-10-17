C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C means.for
C Calculating the mean of numbers
C ******************************************************************************

      PROGRAM MEANS
      INTEGER DIN, DOUT, TF, RF, INUMS
      COMMON /GLBMNU/IMITEM(15)
      COMMON /GLBDAT/RNUMS(10)
      DATA INUMS/0/
C I/O DEVICES ON CPM/M (MS FORTRAN-80):
C   LUN 1, 3..5: TTY
C   LUN 2:       LPT
C   LUN 6..10:   file
      DATA DIN/5/, DOUT/3/, TF/6/, RF/7/
C     DATA DIN/5/, DOUT/6/, TF/8/, RF/8/
C HEADER
      CALL SPLIT(DOUT)
      WRITE(DOUT, 98)
C MENU
   10 CALL SPLIT(DOUT)
      WRITE(DOUT, 97)
      WRITE(DOUT, 96)
      WRITE(DOUT, 95)
      WRITE(DOUT, 94)
      WRITE(DOUT, 93)
      WRITE(DOUT, 92) 
      WRITE(DOUT, 91)
      WRITE(DOUT, 90)
      WRITE(DOUT, 89)
      WRITE(DOUT, 88)
      WRITE(DOUT, 87)
      WRITE(DOUT, 86)
      WRITE(DOUT, 85)
      WRITE(DOUT, 84)
      WRITE(DOUT, 83)
      CALL SPLIT(DOUT)
      WRITE(DOUT, 82)
      READ(DIN, 81) I
      WRITE(DOUT, 99)
      N = MENU(I)
      GOTO(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
     1 33, 60, 10), N
   20 INUMS = IGETDT(DIN, DOUT)
      GOTO 10
   21 IF (INUMS .EQ. 0) GOTO 35
      CALL SHOWDT(DOUT, INUMS)
      WRITE(DOUT, 99)
      GOTO 10
   22 INUMS = ILDDT(RF)
      IF (INUMS .NE. 0) GOTO 10
      WRITE(DOUT, 77)
      GOTO 10
   23 IF (INUMS .EQ. 0) GOTO 35
      IF (ISVDT(RF, INUMS) .EQ. 1) GOTO 10
      WRITE(DOUT, 78)
      GOTO 10
   24 IF (INUMS .LT. 2) GOTO 34
      IF (IALLMN(DOUT, .FALSE., INUMS) .EQ. 1) GOTO 10
      GOTO 10
   25 IF (INUMS .LT. 2) GOTO 34
      R = RARIMN(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   26 IF (INUMS .LT. 2) GOTO 34
      R = RGEOMN(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   27 IF (INUMS .LT. 2) GOTO 34
      R = RHARMN(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   28 IF (INUMS .LT. 2) GOTO 34
      R = RQUAMN(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   29 IF (INUMS .LT. 2) GOTO 34
      R = RLOGMN(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   30 IF (INUMS .LT. 2) GOTO 34
      R = RMED(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   31 IF (INUMS .LT. 2) GOTO 34
      R = RMODUS(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   32 IF (INUMS .LT. 2) GOTO 34
      R = RSTDDV(INUMS)
      WRITE(DOUT, 80) R
      WRITE(DOUT, 99)
      GOTO 10
   33 IF (INUMS .LT. 2) GOTO 34
      IF (IALLMN(TF, .TRUE., INUMS) .EQ. 1) GOTO 10
      WRITE(DOUT, 79)
      GOTO 10
   34 WRITE(DOUT, 76)
      GOTO 10
   35 WRITE(DOUT, 75)
      GOTO 10
C END OF PROGRAM
   60 WRITE(DOUT, 99)
      STOP

C ** FORMAT DECLARATIONS **
   75 FORMAT(15H No input data!)
   76 FORMAT(34H At least two inputs are required!)
   77 FORMAT(25H Data file reading error!)
   78 FORMAT(25H Data file writing error!)
   79 FORMAT(27H Report file writing error!)
   80 FORMAT(8H Result:, 14X, F10.4)
   81 FORMAT(A1)
   82 FORMAT(15H Please select:, 1X)
   83 FORMAT(7H q Quit)
   84 FORMAT(15H s Save summary)
   85 FORMAT(21H 8 Standard deviation, 1X, F10.4)
   86 FORMAT(8H 7 Modus, 14X, F10.4)
   87 FORMAT(9H 6 Median, 13X, F10.4)
   88 FORMAT(19H 5 Logarithmic mean, 3X, F10.4)
   89 FORMAT(17H 4 Quadratic mean, 5X, F10.4)
   90 FORMAT(16H 3 Harmonic mean, 6X, F10.4)
   91 FORMAT(17H 2 Geometric mean, 5X, F10.4)
   92 FORMAT(18H 1 Arithmetic mean, 4X, f10.4)
   93 FORMAT(19H 0 All calculations)
   94 FORMAT(27H w Write input data to file)
   95 FORMAT(28H r Read input data from file)
   96 FORMAT(18H h Show input data)
   97 FORMAT(19H e Input data entry)
   98 FORMAT(32H CALCULATING THE MEAN OF NUMBERS)
   99 FORMAT(1H )
      END

C *** INITIALIZE GLOBAL ARRAYS ***
      BLOCK DATA
      COMMON /GLBMNU/IMITEM(15)
      COMMON /GLBDAT/RNUMS(10)
      DATA IMITEM/1He, 1Hh, 1Hr, 1Hw, 1H0, 1H1, 1H2, 1H3, 1H4, 1H5,
     1 1H6, 1H7, 1H8, 1Hs, 1Hq/
      END

C *** WRITE SPLITTER TO CONSOLE ***
      SUBROUTINE SPLIT(LUN)
      WRITE(LUN, 199)
      RETURN
  199 FORMAT(1H , 31(1H-))
      END

C *** RETURN THE NUMBER OF THE SELECTED MENUITEM, RESULT: 1..16 ***
      INTEGER FUNCTION MENU(SLCT)
      INTEGER SLCT, VALID
      COMMON /GLBMNU/IMITEM(15)
      DATA VALID/16/
      DO 200 I = 1, 15
      IF (SLCT .EQ. IMITEM(I)) VALID = I
  200 CONTINUE
      MENU = VALID
      RETURN
      END

C *** ENTER INPUT DATA ***
      FUNCTION IGETDT(LUNI, LUNO)
      COMMON /GLBDAT/RNUMS(10)
      IREAD = 0
      WRITE(LUNO, 398)
      WRITE(LUNO, 399)
      DO 310 I = 1, 10
      READ(LUNI, 397, ERR = 320, END = 320) X
      IF (X .EQ. 0.0) GOTO 320
      RNUMS(I) = X
      IREAD = IREAD + 1
  310 CONTINUE
      GOTO 340
  320 CONTINUE
      K = IREAD + 1
      DO 330 I = K, 10
      RNUMS(I) = 0.0
  330 CONTINUE
      IGETDT = IREAD
      WRITE(LUNO, 396) IREAD
  340 RETURN
  396 FORMAT(17H Entered numbers:, I2)
  397 FORMAT(F10.4)
  398 FORMAT(37H Please enter the numbers (0 to end):)
  399 FORMAT(1H )
      END

C *** SHOW INPUT DATA ***
      SUBROUTINE SHOWDT(LUN, INS)
      COMMON /GLBDAT/RNUMS(10)
      WRITE(LUN, 498)
      WRITE(LUN, 499)
      IF (INS .EQ. 0) GOTO 410
      DO 410 I = 1, INS, 5
      WRITE(LUN, 497) RNUMS(I), RNUMS(I + 1), RNUMS(I + 2),
     1 RNUMS(I + 3), RNUMS(I + 4)
  410 CONTINUE
      WRITE(LUN, 499)
      WRITE(LUN, 496)
      RETURN
  496 FORMAT(41H Note: The logarithmic mean is calculated,
     1 27H from the first two values.)
  497 FORMAT(1X, F10.4, 4(4X, F10.4))
  498 FORMAT(29H Input data for calculations:)
  499 FORMAT(1H )
      END

C *** READ INPUT NUMBERS FROM FILE ***
      FUNCTION ILDDT(LUN)
      COMMON /GLBDAT/RNUMS(10)
      READ(LUN, ERR = 510), J, (RNUMS(I), I = 1, 10)
      ILDDT = J
      RETURN      
  510 ILDDT = 0
      RETURN
      END

C *** WRITE INPUT NUMBERS TO FILE ***
      FUNCTION ISVDT(LUN, INS)
      COMMON /GLBDAT/RNUMS(10)
      WRITE(LUN, ERR = 610) INS, (RNUMS(I), I = 1, 10)
      ISVDT = 1
      RETURN      
  610 ISVDT = 0
      RETURN      
      END

C *** CALCULATING ALL MEANS AND WRITE TO CONSOLE OR FILE ***
      FUNCTION IALLMN(LUN, F, INS)
      LOGICAL F
      IF (F) CALL OPEN(LUN, 11HREPORT  TXT, 0)
      IF (F .EQ. FALSE) GOTO 710
      CALL SHOWDT(LUN, INS)
      WRITE(LUN, 799, ERR = 720)
  710 WRITE(LUN, 798, ERR = 720)
      WRITE(LUN, 799, ERR = 720)
      R = RARIMN(INS)
      WRITE(LUN, 797, ERR = 720) R
      R = RGEOMN(INS)
      WRITE(LUN, 796, ERR = 720) R
      R = RHARMN(INS)
      WRITE(LUN, 795, ERR = 720) R
      R = RQUAMN(INS)
      WRITE(LUN, 794, ERR = 720) R
      R = RLOGMN(INS)
      WRITE(LUN, 793, ERR = 720) R
      R = RMED(INS)
      WRITE(LUN, 792, ERR = 720) R
      R = RMODUS(INS)
      WRITE(LUN, 791, ERR = 720) R
      R = RSTDDV(INS)
      WRITE(LUN, 790, ERR = 720) R
      WRITE(LUN, 799, ERR = 720)
      IALLMN = 1
      RETURN
  720 IALLMN = 0
      RETURN      
  790 FORMAT(19H Standard deviation, 1X, F10.4)
  791 FORMAT(6H Modus, 14X, F10.4)
  792 FORMAT(7H Median, 13X, F10.4)
  793 FORMAT(17H Logarithmic mean, 3X, F10.4)
  794 FORMAT(15H Quadratic mean, 5X, F10.4)
  795 FORMAT(14H Harmonic mean, 6X, F10.4)
  796 FORMAT(15H Geometric mean, 5X, F10.4)
  797 FORMAT(16H Arithmetic mean, 4X, F10.4)
  798 FORMAT(9H Results:)
  799 FORMAT(1H )
      END
      
C *** CALCULATING ARITHMETIC MEAN ***
      FUNCTION RARIMN(INS)
      COMMON /GLBDAT/RNUMS(10)
      SUMM = 0
      DO 810 I = 1, INS
      SUMM = SUMM + RNUMS(I)
  810 CONTINUE
      RARIMN = SUMM / INS
      RETURN
      END
      
C *** CALCULATING GEOMETRIC MEAN ***
      FUNCTION RGEOMN(INS)
      COMMON /GLBDAT/RNUMS(10)
      PROD = 1
      DO 910 I = 1, INS
      PROD = PROD + RNUMS(I)
  910 CONTINUE
      RGEOMN = PROD**(1.0 / INS)
      RETURN
      END

C *** CALCULATING HARMONIC MEAN ***
      FUNCTION RHARMN(INS)
      COMMON /GLBDAT/RNUMS(10)
      SUMM = 0
      DO 1010 I = 1, INS
      SUMM = SUMM + RNUMS(I)**(-1)
 1010 CONTINUE
      RHARMN = SUMM / INS
      RETURN
      END

C *** CALCULATING QUADRATIC MEAN ***
      FUNCTION RQUAMN(INS)
      COMMON /GLBDAT/RNUMS(10)
      SUMM = 0
      DO 1110 I = 1, INS
      SUMM = SUMM + RNUMS(I)**2
 1110 CONTINUE
      RQUAMN = SQRT((1.0/INS) * SUMM)
      RETURN
      END

C *** CALCULATING LOGARITMIC MEAN ***
      FUNCTION RLOGMN(INS)
      COMMON /GLBDAT/RNUMS(10)
      IF (RNUMS(1) .EQ. RNUMS(2)) RLOGMN = RNUMS(1)
      IF (RNUMS(1) .NE. RNUMS(2)) RLOGMN = (RNUMS(2) - RNUMS(1)) /
     1(ALOG10(RNUMS(2)) - ALOG10(RNUMS(1)))
      RETURN
      END

C *** CALCULATING MEDIAN ***
      FUNCTION RMED(INS)
      DIMENSION RSNUMS(10)
      COMMON /GLBDAT/RNUMS(10)
C COPY DATA TO LOCAL ARRAY      
      DO 1310 I = 1, INS
      RSNUMS(I) = RNUMS(I)
 1310 CONTINUE
C SORT DATA IN LOCAL ARRAY
      K = INS - 1
      DO 1330 I = 1, K
      L = I + 1
      DO 1320 J = L, INS
      IF (RSNUMS(I) .LE. RSNUMS(J)) GOTO 1320
      TEMP = RSNUMS(I)
      RSNUMS(I) = RSNUMS(J)
      RSNUMS(J) = TEMP
 1320 CONTINUE
 1330 CONTINUE
C MEDIAN CALCULATION
      K = (INS + 1) / 2
      L = INS / 2
      M = INS / 2 + 1
      IF (MOD(INS, 2) .EQ. 1) RMED = RSNUMS(K)
      IF (MOD(INS, 2) .EQ. 0)
     1 RMED = (RSNUMS(L) + RSNUMS(M)) / 2.0
      RETURN
      END

C *** CALCULATING MODUS ***
      FUNCTION RMODUS(INS)
      COMMON /GLBDAT/RNUMS(10)
      IMXCNT = 0
      MODVAL = 0.0
      DO 1420 I = 1, INS
      ICNT = 0
      DO 1410 J = 1, INS
      IF (RNUMS(J) .EQ. RNUMS(I)) ICNT = ICNT + 1
 1410 CONTINUE
      IF (ICNT .LE. IMXCNT) GOTO 1420
      IMXCNT = ICNT
      MODVAL = RNUMS(I)
 1420 CONTINUE
      IF (IMXCNT .LE. 1) RMODUS = 0.0
      IF (IMXCNT .GT. 1) RMODUS = MODVAL
      RETURN
      END

C *** CALCULATING STANDARD DEVIATION ***
      FUNCTION RSTDDV(INS)
      COMMON /GLBDAT/RNUMS(10)
      RSUM = 0.0
      DO 1510 I = 1, INS
      RSUM = RSUM + RNUMS(I)
 1510 CONTINUE
      RMEAN = RSUM / INS
      RVAR = 0.0
      DO 1520 I = 1, INS
      RVAR = RVAR + (RNUMS(I) - RMEAN)**2
 1520 CONTINUE
      RVAR = RVAR / INS
      RSTDDV = SQRT(RVAR)
      RETURN
      END

