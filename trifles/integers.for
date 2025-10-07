C WRITE VALUE OF VARIABLES TO CONSOLE

      PROGRAM INTEGERS
      INTEGER DOUT
      INTEGER I, J, K
C I/0 DEVICES
      DATA DOUT /3/
C     DATA DOUT /6/
   10 FORMAT(3H I=, I2, 1X, 2HJ=, I2, 1X, 2HK=, I2)
   20 FORMAT(' I=', I2, 1X, 'J=', I2, 1X, 'K=', I2)
      I = 11
      J = 22
      K = 33
      WRITE(DOUT, 10) I, J, K
      WRITE(DOUT, 20) I, J, K
      STOP
      END
