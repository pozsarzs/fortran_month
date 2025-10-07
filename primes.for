C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C primes.for
C Calculating prime numbers up to the specified number
C ******************************************************************************

      PROGRAM PRIMES
      INTEGER N, P, Q
      INTEGER DIN, DOUT
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
      WRITE(DOUT, 98)
      WRITE(DOUT, 97)
      READ(DIN, 92, ERR = 79) N
      IF (N .LT. 5) GOTO 78
      WRITE(DOUT, 99)
      WRITE(DOUT, 91)
   10 CALL WRITEOUT(DOUT, 2)
      CALL WRITEOUT(DOUT, 3)
      P = 5
   20 Q = 3
      IF (P / Q .EQ. FLOAT(P) / FLOAT(Q)) GOTO 40
   30 IF (Q**2 .GT. P) GOTO 50
      Q = Q + 2
      GOTO 30
   40 P = P + 2
      IF (P .LE. N) GOTO 20
      GOTO 80
   50 CALL WRITEOUT(DOUT, P)
      GOTO 40

C WARNING: LOW VALUE
   78 WRITE(DOUT,93)
      LET N = 5
      GOTO 10
C ERROR: BAD DATA
   79 WRITE(DOUT,94)
      GOTO 80
C END OF PROGRAM
   80 WRITE(DOUT, 99)             
      STOP      

C ** FORMAT DECLARATIONS **
   91 FORMAT(15H Prime numbers:)
   92 FORMAT(I5)
   93 FORMAT(20H Minimum value is 5.)
   94 FORMAT(18H Wrong input data!)
   97 FORMAT(8H Up to? )
   98 FORMAT(26H CALCULATING PRIME NUMBERS)
   99 FORMAT(1H )
      END

C *** WRITE NUMBER TO CONSOLE ***
      SUBROUTINE WRITEOUT(DEVICE, NUMBER)
      INTEGER DEVICE, NUMBER
      WRITE(DEVICE, 100) NUMBER
  100 FORMAT(I5, 1H;)
      RETURN      
      END
