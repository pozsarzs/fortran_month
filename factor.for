C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C factor.for
C Factorial calculator
C ******************************************************************************

      PROGRAM FACTORIAL
      INTEGER DIN, DOUT
      DOUBLE PRECISION D
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
      WRITE(DOUT, 98)
C GET INPUT DATA
      WRITE(DOUT, 97)
      READ(DIN, 96, ERR = 78) I
      IF (I .LT. 1) GOTO 79
      D = FACT(I)
C WRITE RESULT
      WRITE(DOUT, 93) I, D
      GOTO 80
C ERROR: BAD INPUT VALUE
   79 WRITE(DOUT, 94)
C ERROR: BAD INPUT DATA
   78 WRITE(DOUT, 95)
      GOTO 80
C END OF PROGRAM
   80 WRITE(DOUT, 99)
      STOP

C ** FORMAT DECLARATIONS **
   93 FORMAT(I5, 2H!=, D24.16)
   94 FORMAT(34H Number must be greater than zero!)
   95 FORMAT(18H Wrong input data!)
   96 FORMAT(I5)
   97 FORMAT(15H Enter a value:)
   98 FORMAT(21H FACTORIAL CALCULATOR)
   99 FORMAT(1H )
      END

C *** FACTORIAL CALCULATOR ***
      DOUBLE PRECISION FUNCTION FACT(K)
      DOUBLE PRECISION F
      F = 1.0
      IF (K .EQ. 1) GOTO 110
      DO 100 L = 2, K 
      F = F * L
  100 CONTINUE
  110 FACT = F
      RETURN
      END

