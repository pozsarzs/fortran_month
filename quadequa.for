C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C quadequa.for
c Discriminant and root(s) of a quadratic equation f(x)=a*x^2+b*x+c, where a<>0
C ******************************************************************************

      PROGRAM QUADEQUA
C VARIABLES AND CONSTANTS
      INTEGER DIN, DOUT
      DIMENSION F(3)
      COMMON X(2)
C I/0 DEVICES
      DATA DIN /5/, DOUT /3/
C     DATA DIN /5/, DOUT /6/
      WRITE(DOUT, 98)
      WRITE(DOUT, 99)
C ENTER VALUES OF A, B, C
      WRITE(DOUT, 97)
      WRITE(DOUT, 99)
      DO 10 I=1,3
      READ(DIN, 95, ERR = 77) F(I)
   10 CONTINUE
      IF (F(1) .EQ. 0) GOTO 79
C DISCRIMINANT AND ROOT(S)
      D = DISCR(F(1), F(2), F(3))
      IF (D .LT. 0) GOTO 78
      CALL ROOTS(F(1), F(2), F(3), D)
      WRITE(DOUT, 99)
      WRITE(DOUT, 94)
      WRITE(DOUT, 93) F(1), F(2), F(3), D, X(1), X(2)
      GOTO 80
C ERROR: BAD DATA
   77 WRITE(DOUT, 91)
      GOTO 80
C ERROR: NO SOLUTION
   78 WRITE(DOUT, 92)
      GOTO 80
C A IS 0
   79 WRITE(DOUT, 96)
C END OF PROGRAM
   80 WRITE(DOUT, 99)
      STOP

C ** FORMAT DECLARATIONS **
   91 FORMAT(18H Wrong input data!)
   92 FORMAT(40H Has no solution among the real numbers!)
   93 FORMAT(1X, F7.2, 2X, F7.2, 2X, F7.2, 2X, F12.2, 2(2X, F10.2))
   94 FORMAT(8H A COEFF, 2X, 7HB COEFF, 2X, 7HC CONST, 2X, 
     112HDISCRIMINANT, 6X, 6HROOT_1, 6X, 6HROOT_2)
   95 FORMAT(F7.0)
   96 FORMAT(31H The value of A cannot be zero!)
   97 FORMAT(31H Enter three values (one/line):)
   98 FORMAT(49H DISCRIMINANT AND ROOT(S) OF A QUADRATIC EQUATION)
   99 FORMAT(1H )
      END

C *** DISCRIMINANT CALCULATION: D=B^2-4AC ***
      FUNCTION DISCR(A, B, C)
      DISCR = (B ** 2 - 4 * A * C)
      RETURN
      END

C *** ROOT(S) CALCULATION: X1,2=(-B+-SQRT(B^2-4AC))/2A ***
      SUBROUTINE ROOTS(A, B, C, D)
      COMMON X(2)
      X(1) = (-1 * B + SQRT(D)) / (2 * A)
      IF (D .EQ. 0) GOTO 200
      X(2) = (-1 * B - SQRT(D)) / (2 * A)
200   CONTINUE
      END

