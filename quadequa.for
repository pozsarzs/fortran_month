C ************************************************
C DISCRIMINANT AND ROOT(S) OF A QUADRATIC EQUATION
C   F(X)=A*X^2+B*X+C, WHERE A<>0
C *************************************************
      PROGRAM QUADEQUA
      DIMENSION F(3)
      COMMON X(2)
      WRITE(3, 98)
      WRITE(3, 99)
C ** ENTER VALUES OF A, B, C **
      WRITE(3, 97)
      WRITE(3, 99)
      DO 10 I=1,3
      READ(5, 95) F(I)
   10 CONTINUE
      IF (F(1) .EQ. 0) GOTO 79
C ** DISCRIMINANT AND ROOT(S) **
      D = DISCR(F(1), F(2), F(3))
      IF (D .LT. 0) GOTO 78
      CALL ROOTS(F(1), F(2), F(3), D)
      WRITE(3, 94)
      WRITE(3, 93) F(1), F(2), F(3), D, X(1), X(2)
      GOTO 80
C ** ERROR MESSAGES **
   78 WRITE(3, 92)
      GOTO 80
   79 WRITE(3, 96)
   80 WRITE(3, 99)
      STOP
C ** FORMAT DECLARATIONS **
   92 FORMAT(40H HAS NO SOLUTION AMONG THE REAL NUMBERS!)
   93 FORMAT(1X, F7.2, 2X, F7.2, 2X, F7.2, 2X, F12.2, 2(2X, F10.2))
   94 FORMAT(8H A COEFF, 2X, 7HB COEFF, 2X, 7HC CONST, 2X, 
     112HDISCRIMINANT, 6X, 6HROOT_1, 6X, 6HROOT_2)
   95 FORMAT(F7.0)
   96 FORMAT(31H THE VALUE OF A CANNOT BE ZERO!)
   97 FORMAT(31H ENTER THREE VALUES (ONE/LINE):)
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
