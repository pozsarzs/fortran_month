C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C multab.for
C Multiplication table
C ******************************************************************************

      PROGRAM MULTAB
C VARIABLES
      INTEGER DOUT
      DIMENSION IDIM(11)
      COMMON /GLBLUN/DOUT
      DATA DOUT/3/
C     DATA DOUT/6/
C HEADER
      DO 10 I = 1, 10
      IDIM(I) = I  
   10 CONTINUE
      WRITE(DOUT, 99), (IDIM(I), I = 1, 10)
      CALL SPLIT(44)
C TABLE
      DO 30 I = 1, 10
      IDIM(1) = I
      DO 20 J = 2, 11
      IDIM(J) = I * (J - 1)
   20 CONTINUE
      WRITE(DOUT, 98), (IDIM(J), J = 1, 11) 
   30 CONTINUE
C END OF PROGRAM
      STOP
C FORMAT DECLARATIONS
   98 FORMAT(1X, I2, 2H |,1X, 11(I3, 1X))
   99 FORMAT(3X, 2h |, 1X, 10(I3, 1X))
      END

C *** WRITE SPLITTER TO CONSOLE ***
      SUBROUTINE SPLIT(LENGTH)
      INTEGER DOUT
      INTEGER I, LINE(80)
      COMMON /GLBLUN/DOUT
      DATA LINE/80*1H-/
      WRITE(DOUT, 199), (LINE(I), I = 1, LENGTH)
      RETURN      
  199 FORMAT(1X, 80A1)
      END
