C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C primes.for
C Calculating prime numbers up to the specified number
C ******************************************************************************

      PROGRAM PRIMES
      INTEGER L, N, P, Q
      INTEGER DIN, DOUT
      INTEGER LINE(10)
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
      WRITE(DOUT, 98)
      WRITE(DOUT, 97)
      READ(DIN, 92, ERR = 79) N
      IF (N .LT. 5) GOTO 78
      WRITE(DOUT, 99)
      WRITE(DOUT, 91)
   10 LINE(1) = 2
      LINE(2) = 3
      L = 3
      P = 5
   20 Q = 3
   30 IF (Q**2 .GT. P) GOTO 50
      IF (P / Q .EQ. FLOAT(P) / FLOAT(Q)) GOTO 40
      Q = Q + 2
      GOTO 30
   40 P = P + 2
      IF (P .LE. N) GOTO 20
      GOTO 80
   50 LINE(L) = P
      L = L + 1
      IF (L .LT. 11) GOTO 40
      L = 1

C WRITE NUMBERS TO CONSOLE
      WRITE(DOUT, 90), (LINE(I), I = 1, 10)
      GOTO 40 

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
   91 FORMAT(15H Prime numbers:)
   92 FORMAT(I5)
   93 FORMAT(20H Minimum value is 5.)
   94 FORMAT(18H Wrong input data!)
   97 FORMAT(8H Up to? )
   98 FORMAT(26H CALCULATING PRIME NUMBERS)
   99 FORMAT(1H )
      END

