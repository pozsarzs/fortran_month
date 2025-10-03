C BASE CALCULATIONS: (5+3)*2=?
      PROGRAM BASECALC
      INTEGER I, J, K, L      
      DATA I/5/, J/3/, K/2/
   10 FORMAT(9H (5+3)*2=, I2)
      L = (I + J) * K
      WRITE(3, 10) L
      STOP
      END

