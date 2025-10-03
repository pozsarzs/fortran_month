C WRITE SQUARE OF NUMBERS 1..10
      PROGRAM SQUARE10
   10 FORMAT(7H NUMBER, 1X, 6HSQUARE)
   20 FORMAT(I6, 1X, I6)
      WRITE(3, 10)
      DO 30 I = 1, 10
      J = I ** 2
      WRITE(3, 20) I, J
   30 CONTINUE
      STOP
      END   

