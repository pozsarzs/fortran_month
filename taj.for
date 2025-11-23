C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C taj.for
C TAJ number validator (Hungarian Health Insurance Number)
C ******************************************************************************

      PROGRAM TAJ
      INTEGER VALID
      INTEGER DIN, DOUT
      LOGICAL TAJNUM(9)
      COMMON /GLBLUN/DIN, DOUT
C SET I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
C SHOW HEADER
      CALL SPLIT(49)
      WRITE(DOUT, 98)
      CALL SPLIT(49)
C INPUT DATA
      VALID = 2
   10 WRITE(DOUT, 97)
      READ(DIN, 96) TAJNUM
      IF ((TAJNUM(1) .EQ. 81) .OR. (TAJNUM(1) .EQ. 113)) GOTO 80 
      CALL CHKTAJ(TAJNUM, VALID)
      GOTO (50, 40, 30, 20), VALID
      GOTO 10 
C RESULT
   20 WRITE(DOUT, 92)
      GOTO 10 
   30 WRITE(DOUT, 93)
      GOTO 10
   40 WRITE(DOUT, 94)
      GOTO 10
   50 WRITE(DOUT, 95)
      GOTO 10
C END OF PROGRAM
   80 WRITE(DOUT, 99)
      STOP
   92 FORMAT(1H+, 44H - This is too short, it should be 9 digits!)
   93 FORMAT(1H+, 37H - There are only numbers in the TAJ!)
   94 FORMAT(1H+, 32H - This is not valid TAJ number!)
   95 FORMAT(1H+, 28H - This TAJ number is valid.)
   96 FORMAT(9A1)
   97 FORMAT(1x, 26HEnter TAJ number ([q]uit):, 1X)
   98 FORMAT(1X, 49HHungarian Health Insurance Number (TAJ) validator)
   99 FORMAT(1H )
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

C *** CHECK TAJ NUMBER ***
C     RESULTS: 1: VALID, 2: INVALID, 3: BAD CHARS, 4: TOO SHORT
      SUBROUTINE CHKTAJ(TN, RESULT)
      LOGICAL TN(9)
      INTEGER I, J, CVD, RESULT
      RESULT = 2 
C CHECK LENGTH
      J = 0
      DO 200 I = 1, 9
      IF (TN(I) .EQ. 32) J = J + 1
  200 CONTINUE 
      IF (J .GT. 0) RESULT = 4
      IF (RESULT .NE. 2) RETURN  
C CHECK DIGITS
      J = 0
      DO 210 I = 1, 9
      IF ((TN(I) .LT. 48) .OR. (TN(I) .GT. 57)) J = J + 1 
  210 CONTINUE
      IF (J .GT. 0) RESULT = 3  
      IF (RESULT .NE. 2) RETURN
C CHECK CVD
      J = 0
      DO 220 I = 3, 10
      IF (MOD(I, 2) .NE. 0) J = J + (TN(I - 2) - 48) * 3
      IF (MOD(I, 2) .EQ. 0) J = J + (TN(I - 2) - 48) * 7
  220 CONTINUE
      CVD = MOD(J, 10)
      IF (CVD .EQ. TN(9) - 48) RESULT = 1
      RETURN
      END

