C ************************************************
C Modbus register number/address converter utility
C ************************************************

C *** MAIN SEGMENT ***
      PROGRAM MBCONV
C ** VARIABLES AND CONSTANTS **
      DOUBLE PRECISION DERROR, DRNUM, DOFFST
      INTEGER*4 ITEXT(4)
      REAL RINPUT
      COMMON/GLBVAR/IMITEM(4), IREGTS(10)
      DATA IVALID /255/
      DATA DOFFST /10000.0/
      DATA ITEXT /4H Reg,4Histe,4Hr ty,4Hpe: /
      DATA DERROR /7H ERROR: /
C ** MESSAGES **
 1000 FORMAT(63H MBConv v0.1 * Modbus register number/address converter
     1 utility)
 1001 FORMAT(47H (C) 2024 Pozsar Zsolt <http://www.pozsarzs.hu>)
 1002 FORMAT(16H Please select: )
 1003 FORMAT(52H Convert register number to register type/address: 1)
 1004 FORMAT(52H Convert register type/address to register number: 2)
 1005 FORMAT(52H Exit to OS:                                       q)
 1006 FORMAT(25H Please enter input data:)
 1007 FORMAT(19H -register number: )
 1008 FORMAT(27H -register type [c|d|i|h]: )
 1009 FORMAT(29H -register address [0-9998]: )
 1010 FORMAT(4A4,21Hdiscrete output coil.)
 1011 FORMAT(4A4,23Hdiscrete input contact.)
 1012 FORMAT(4A4,22Hanalog input register.)
 1013 FORMAT(4A4,24Hanalog holding register.)
 1014 FORMAT(19H Register number:  ,F6.0)
 1015 FORMAT(19H Register address: ,I5,1H.)
 1016 FORMAT(A8,17HWrong input data!)
 1017 FORMAT(A8,39HThe register type can be c, d, i and h.)
 1018 FORMAT(A8,35HThe register address can be 0-9998.)
 1019 FORMAT(A8,72HThe register number can be 1-9999 10001-19999, 30001-
     139999, 40001-49999.)
 1020 FORMAT(1H )
C ** PRINT HEADER, INPUT VALUES AND SELECT OPERATION MODE ** 
      WRITE(3, 1000)
      WRITE(3, 1001)
      WRITE(3, 1020)
C GET ARGUMENTS FROM STDIN
   10 WRITE(3, 1003)
      WRITE(3, 1004)
      WRITE(3, 1005)
      CALL SPLIT
      WRITE(3, 1002)
      READ(5, 20) IMODE
   20 FORMAT(A1)
      L = IMENU(IMODE)
      GOTO (30, 70, 240, 240, 10), L

C ** 1. CONVERT REGISTER NUMBER TO ADDRESS **
   30 WRITE(3, 1020)
C GET ARGUMENT FROM STDIN
      WRITE(3, 1006)
      WRITE(3, 1007)
      READ(5, 40, ERR = 230) DRNUM
   40 FORMAT(D5.0)
      IF (DRNUM .EQ. 0) GOTO 220
C CHECK REGISTER NUMBER
      IVALID = ICREGN(DRNUM)
      IF (IVALID .EQ. 255) GOTO 210
      IRTYPE = IVALID
      IF (DRNUM .EQ. 0) DRNUM = 1
      IRADDR = DRNUM - ((DOFFST * IVALID) + 1)
      WRITE(3, 1020)
C PRINT REGISTER NUMBER
      WRITE(3, 1014) DRNUM
C PRINT REGISTER TYPE
      L = IRTYPE + 1
      GOTO (51, 52, 53, 54, 55), L
   51 WRITE(3, 1010) ITEXT
      GOTO 60
   52 WRITE(3, 1011) ITEXT
      GOTO 60
   53 GOTO 60
   54 WRITE(3, 1012) ITEXT
      GOTO 60
   55 WRITE(3, 1013) ITEXT
      GOTO 60
C PRINT REGISTER ADDRESS
   60 WRITE(3, 1015) IRADDR
      GOTO 240

C ** 2. CONVERT ADDRESS TO REGISTER NUMBER **
   70 WRITE(3, 1020)
C GET ARGUMENTS FROM STDIN
      WRITE(3, 1006)
      WRITE(3, 1008)
      READ(5, 80) IINPUT
   80 FORMAT(A1)
      IVALID = ICREGT(IINPUT)
      IF (IVALID .EQ. 255) GOTO 200
      IRTYPE = IVALID
      WRITE(3, 1009)
      READ(5, 90, ERR = 230) RINPUT
   90 FORMAT(F4.0)
      IINPUT = RINPUT
      IF (IINPUT .EQ. 0) GOTO 230
C CHECK REGISTER ADDRESS
      IVALID = ICREGA(IINPUT)
      IF (IVALID .EQ. 255) GOTO 200
      IRADDR = IINPUT
C PRINT REGISTER TYPE
      L = IRTYPE + 1
      GOTO (101, 102, 103, 104, 105), L
  101 WRITE(3, 1010) ITEXT
      GOTO 110
  102 WRITE(3, 1011) ITEXT
      GOTO 110
  103 GOTO 110
  104 WRITE(3, 1012) ITEXT
      GOTO 110
  105 WRITE(3, 1013) ITEXT
      GOTO 110
C PRINT REGISTER ADDRESS
  110 WRITE(3, 1015) IRADDR
C PRINT REGISTER NUMBER
      DRNUM = (DOFFST * IRTYPE) + IRADDR + 1
      WRITE(3, 1014) DRNUM
      GOTO 240
C ERROR MESSAGES
  200 WRITE(3, 1017) DERROR
      GOTO 240
  210 WRITE(3, 1018) DERROR
      GOTO 240
  220 WRITE(3, 1019) DERROR
      GOTO 240
  230 WRITE(3, 1016) DERROR
      GOTO 240
C END OF PROGRAM
  240 WRITE(3, 1020)
      STOP
      END

C *** SEGMENT BLOCK DATA ***
      BLOCK DATA
      COMMON/GLBVAR/IMITEM(4), IREGTS(10)
      DATA IMITEM(1), IMITEM(2), IMITEM(3), IMITEM(4)/1H1,1H2,1HQ,1Hq/
      DATA IREGTS(1), IREGTS(2), IREGTS(3), IREGTS(4), IREGTS(5),
     1IREGTS(6), IREGTS(7), IREGTS(8), IREGTS(9), IREGTS(10)/
     21Hc,1Hd,1H_,1Hi,1Hh,1HC,1HD,1H_,1HI,1HH/
      END

C *** PRINT SPLITTER TO CONSOLE ***
      SUBROUTINE SPLIT
      WRITE(3, 300)
  300 FORMAT(1H , 51(1H-))
      RETURN
      END

C *** GET SELECTED MENUITEM, RESULT: 1..5 ***
      FUNCTION IMENU(ISLCT)
      COMMON/GLBVAR/IMITEM(4), IREGTS(10)
      DATA IVALID/5/
      DO 400 I = 1, 4
      IF (ISLCT .EQ. IMITEM(I)) IVALID = I
  400 CONTINUE
      IMENU = IVALID
      RETURN
      END

C *** CHECK REGISTER TYPE, RESULT: 0..4, 255 ***
      FUNCTION ICREGT(IREGT)
      COMMON/GLBVAR/IMITEM(4), IREGTS(10)
      DATA IVALID/255/
      DO 500 I = 0, 4
      IF (IREGT .EQ. IREGTS(I + 1)) IVALID = I
      IF (IREGT .EQ. IREGTS(I + 6)) IVALID = I
  500 CONTINUE
      IF (IVALID .EQ. 2) IVALID = 255
      ICREGT = IVALID
      RETURN
      END

C *** CHECK REGISTER ADDRESS, RESULT: 0, 255 ***
      FUNCTION ICREGA(IREGA)
      DATA IVALID/255/
      IF ((IREGA .GE. 0) .AND. (IREGA .LE. 9998)) IVALID = 0
      ICREGA = IVALID
      RETURN
      END      

C *** CHECK REGISTER NUMBER, RESULT: 0..4, 255 ***
      FUNCTION ICREGN(DREGN)
      DOUBLE PRECISION DREGN
      DATA IVALID/255/
      IF ((DREGN .GE. 1) .AND. (IREGN .LE. 9999)) IVALID = 0
      IF ((DREGN .GE. 10001) .AND. (DREGN .LE. 19999)) IVALID = 1
      IF ((DREGN .GE. 30001) .AND. (DREGN .LE. 39999)) IVALID = 3
      IF ((DREGN .GE. 40001) .AND. (DREGN .LE. 49999)) IVALID = 4
      ICREGN = IVALID
      RETURN
      END

