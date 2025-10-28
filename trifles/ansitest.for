C MAKE COLOR TEXT ON ANSI TERMINAL

      PROGRAM ANSITE

      CALL COLRES(3)
      WRITE(3, 99)
      DO 10 I = 0, 7
      J = 7 - I
      CALL COLFGR(3, .TRUE., I)
      CALL COLBGR(3, .FALSE., J)
      WRITE(3, 98) I, J
   10 CONTINUE

      WRITE(3, 97)
      DO 20 I = 0, 15
      J = 15 - I
      IF (J .GT. 7) J = J - 8
      CALL COLFGR(3, .TRUE., I)
      CALL COLBGR(3, .FALSE., J)
      WRITE(3, 98) I, J    
   20 CONTINUE

      CALL COLRES(3)
      STOP
   97 FORMAT('0BRIGHT LETTERS ON DARK BACKGROUND:')
   98 FORMAT(' HELLO', 1X, I2, 1X, I2)
   99 FORMAT(' DARK LETTERS ON DARK BACKGROUND:')
      END

