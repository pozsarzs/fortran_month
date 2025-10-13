C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C nrmsqrt.for
C Square root calculation with Newtonâ€“Raphson method
C ******************************************************************************

      PROGRAM NRMSQRT
      INTEGER DIN, DOUT
      REAL A, Q
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
      WRITE(DOUT, 97)
      READ(DIN, 95, ERR = 78) A
      IF (A .LT. 0) GOTO 79
      Q = (A + 1.0) / 2.0
   10 Q = (A / Q + Q) / 2.0
      IF (ABS(Q**2 - A) .GE. A / 10.0**6) GOTO 10
C RESULT
      WRITE(DOUT, 94) A, Q
      GOTO 80
C ERROR: BAD DATA
   78 WRITE(DOUT,93)
      GOTO 80
C ERROR: NEGATIVE NUMBER
   79 WRITE(DOUT, 96)
C END OF PROGRAM
   80 WRITE(DOUT, 99)
      STOP

C ** FORMAT DECLARATIONS **
   93 FORMAT(18H Wrong input data!)
   94 FORMAT(6H SQRT(, E10.5, 2H)=, E10.5)
   95 FORMAT(E10.5)
   96 FORMAT(31H The number cannot be negative!)
   97 FORMAT(36H Enter a value (with decimal point):)
   98 FORMAT(51H SQUARE ROOT CALCULATION WITH NEWTON-€“RAPHSON METHOD)
   99 FORMAT(1H )
      END

