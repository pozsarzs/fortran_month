C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C skeleton.for
C Program skeleton
C ******************************************************************************

      PROGRAM SKELETON
      INTEGER DIN, DOUT
      REAL A, Q

C     (..)

C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/

C     (..)

C GET DATA
      WRITE(DOUT, 97)
      READ(DIN, 95, ERR = 78) A
      IF (A .LT. 0) GOTO 79

C     (..)

C WRITE RESULT
      WRITE(DOUT, 93) A, Q
      GOTO 80
C ERROR: BAD INPUT VALUE
   79 WRITE(DOUT, 94)
C ERROR: BAD INPUT DATA
   78 WRITE(DOUT,95)
      GOTO 80
C END OF PROGRAM
   80 WRITE(DOUT, 99)
      STOP

C ** FORMAT DECLARATIONS **

C     (..)

   93 FORMAT(6H SQRT(, E10.5, 2H)=, E10.5)
   94 FORMAT(31H The number cannot be negative!)
   95 FORMAT(18H Wrong input data!)
   96 FORMAT(E10.5)
   97 FORMAT(35H Enter a value (with decimal point:)
   98 FORMAT(51H SQUARE ROOT CALCULATION WITH NEWTON–RAPHSON METHOD)
   99 FORMAT(1H )
      END
