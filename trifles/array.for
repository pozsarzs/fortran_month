C ******************************************************************************
C CREATE AN ARRAY OF 5 ELEMENTS, FILL IT BY MULTIPLYING THE INDEX BY 2
C AND PRINT IT
C ******************************************************************************

      PROGRAM ARRAY
      INTEGER I, J(5)
      INTEGER DOUT
C I/0 DEVICES
      DATA DOUT /3/
C     DATA DOUT /6/
   10 FORMAT(6H INDEX, 1X, 5HVALUE)
   20 FORMAT(I5, 1X, I5)
      DO 30 I = 1, 5
      J(I) = 2 * I
   30 CONTINUE
      WRITE(DOUT, 10)
      DO 40 I = 1, 5
      WRITE(DOUT, 20) I, J(I)
   40 CONTINUE
      STOP
      END
 
