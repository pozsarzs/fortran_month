C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C fibonacc.for
C Calculating Fibonacci numbers up to the specified number
C ******************************************************************************

      PROGRAM FIBO
      INTEGER L, N, P, Q(3)
      INTEGER DIN, DOUT
      INTEGER LINE(10)
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/

C INPUT DATA
      WRITE(DOUT, 98)
      WRITE(DOUT, 97)
      READ(DIN, 92, ERR = 79) N
      IF (N .LT. 5) GOTO 78
      WRITE(DOUT, 99)
      WRITE(DOUT, 91)

C CALCULATE
      L = 1
      IDX = 0
   10 IF (IDX .GT. 1) GOTO 20
      Q(3) = IDX     
      GOTO 30
   20 Q(3) = Q(1) + Q(2)
   30 LINE(L) = Q(3)
      IDX = IDX + 1
      IF (Q(3) .GT. N) GOTO 80
      L = L + 1
      Q(1) = Q(2)
      Q(2) = Q(3)
      IF (L .LT. 11) GOTO 10

C WRITE TO CONSOLE
      WRITE(DOUT, 90), (LINE(I), I = 1, 10)
      L = 1
      GOTO 10

C WARNING: LOW VALUE
   78 WRITE(DOUT,93)
      LET N = 5
      GOTO 10

C ERROR: BAD DATA
   79 WRITE(DOUT,94)
      GOTO 81

C END OF PROGRAM
   80 J = L - 1
      IF (L .GT. 1) WRITE(DOUT, 90), (LINE(I), I = 1, J)
      WRITE(DOUT, 99)
   81 STOP      

C ** FORMAT DECLARATIONS **
   90 FORMAT(1H , 10(I5, 2H; ))
   91 FORMAT(19H Fibonacci numbers:)
   92 FORMAT(I5)
   93 FORMAT(20H Minimum value is 5.)
   94 FORMAT(18H Wrong input data!)
   97 FORMAT(8H Up to? )
   98 FORMAT(30H CALCULATING FIBONACCI NUMBERS)
   99 FORMAT(1H )
      END

