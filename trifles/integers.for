C WRITE VARIABLE'S VALUE TO CONSOLE
      PROGRAM INTEGERS
      INTEGER I, J, K
   10 FORMAT(3H I=, I2, 1X, 2HJ=, I2, 1X, 2HK=, I2)
   20 FORMAT(' I=', I2, 1X, 'J=', I2, 1X, 'K=', I2)
      I = 11
      J = 22
      K = 33
      WRITE(3, 10) I, J, K
      WRITE(3, 20) I, J, K
      STOP
      END

