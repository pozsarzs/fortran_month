C ******************************************************************************
C Fortran Month * source code example collection
C (C) 2025 Pozsar Zsolt <pozsarzs@gmail.com>, Licence: CC0 Universal v1.0
C ansilib.for
C Console control routines with ANSI Escape sequence
C ******************************************************************************

C *** GLOBAL VARIABLES ***
      BLOCK DATA
      INTEGER ESC
      INTEGER FG(8), BG(8)
      COMMON /GLBVAR/ESC
      COMMON /COLORS/FG, BG
      DATA ESC/27/
      DATA FG(1), FG(2), FG(3), FG(4), FG(5), FG(6), FG(7), FG(8)
     1 /30, 34, 32, 36, 31, 35, 33, 37/
      DATA BG(1), BG(2), BG(3), BG(4), BG(5), BG(6), BG(7), BG(8)
     2 /40, 44, 42, 46, 41, 45, 43, 47/
      END

C ******************************************************************************
C COLORS
C ******************************************************************************

C *** RESET COLORS ***
      SUBROUTINE COLRES(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 99) ESC 
      RETURN
   99 FORMAT('+', A1, '[0m') 
      END

C *** SET FOREGROUND COLOR ***
      SUBROUTINE COLFGR(LUN, RESET, ICOL)
      LOGICAL RESET
      INTEGER FG(8), BG(8), ESC
      COMMON /GLBVAR/ESC
      COMMON /COLORS/FG, BG
      I = ICOL + 1
      IF (I .LT. 1) I = 1
      IF (I .GT. 8) I = I - 8
      IF (RESET .EQ. .TRUE.) WRITE(LUN, 199) ESC
      IF (I .GT. 8) WRITE(LUN, 198) ESC, FG(I)
      IF (I .LE. 8) WRITE(LUN, 197) ESC, FG(I)
      RETURN                   
  197 FORMAT('+', A1, '[', I2, 'm')
  198 FORMAT('+', A1, '[1;', I2, 'm')                 
  199 FORMAT('+', A1, '[0m')      
      END

C *** SET BACKGROUND COLOR ***
      SUBROUTINE COLBGR(LUN, RESET, ICOL)
      LOGICAL RESET
      INTEGER FG(8), BG(8), ESC 
      COMMON /GLBVAR/ESC
      COMMON /COLORS/FG, BG
      I = ICOL + 1
      IF (I .LT. 1) I = 1
      IF (I .GT. 8) I = 8
      IF (RESET .EQ. .TRUE.) WRITE(LUN, 299) ESC
      WRITE(LUN, 298) ESC, BG(I)
      RETURN
  298 FORMAT('+', A1, '[', I2, 'm')
  299 FORMAT('+', A1, '[0m') 
      END

C *** INVERT COLORS ***
      SUBROUTINE COLINV(LUN)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 399) ESC
      RETURN
  399 FORMAT('+', A1, '[7m') 
      END

C ******************************************************************************
C ERASE FUNCTIONS
C ******************************************************************************

C *** ERASE FROM CURSOR TO END OF SCREEN ***
      SUBROUTINE ERASCE(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 499) ESC
      RETURN
  499 FORMAT('+', A1, '[0J') 
      END

C *** ERASE FROM CURSOR TO BEGINNING OF SCREEN ***
      SUBROUTINE ERASCH(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 599) ESC
      RETURN
  599 FORMAT('+', A1, '[1J') 
      END

C *** ERASE ENTIRE SCREEN ***
      SUBROUTINE ERASCR(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 699) ESC
      RETURN
  699 FORMAT('+', A1, '[2J') 
      END

C *** ERASE ENTIRE SCREEN AND THE SCROLLBACK BUFFER ***
      SUBROUTINE ERASCB(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 799) ESC
      RETURN
  799 FORMAT('+', A1, '[3J') 
      END

C *** ERASE FROM CURSOR TO END OF LINE ***
      SUBROUTINE ERALIE(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 899) ESC
      RETURN
  899 FORMAT('+', A1, '[0K') 
      END

C *** ERASE FROM CURSOR TO BEGINNING OF LINE ***
      SUBROUTINE ERALIH(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 999) ESC
      RETURN
  999 FORMAT('+', A1, '[1K') 
      END

C *** ERASE ENTIRE LINE ***
      SUBROUTINE ERALIN(LUN)
      INTEGER ESC
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1099) ESC
      RETURN
 1099 FORMAT('+', A1, '[2K') 
      END

C ******************************************************************************
C CURSOR CONTROL
C ******************************************************************************

C *** MOVE TO HOME OF SCREEN ***
      SUBROUTINE CURSCH(LUN)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1199) ESC
      RETURN
 1199 FORMAT('+', A1, '[H') 
      END

C *** MOVE CURSOR SPECIFIED POSITION ***
      SUBROUTINE CURPOS(LUN, MODE, IX, IY)
      INTEGER ESC
      LOGICAL MODE 
      COMMON /GLBVAR/ESC
      IF (MODE .EQ. .TRUE.) WRITE(LUN, 1299) ESC, IX, IY
      IF (MODE .EQ. .FALSE.) WRITE(LUN, 1298) ESC, IX, IY
      RETURN
 1298 FORMAT('+', A1, '[', I2, ';', I2, 'f') 
 1299 FORMAT('+', A1, '[', I2, ';', I2, 'H') 
      END

C *** MOVE CURSOR UP SPECIFIED LINE ***
      SUBROUTINE CURUP(LUN, ILINE)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1399) ESC, ILINE
      RETURN
 1399 FORMAT('+', A1, '[', I2, 'A') 
      END

C *** MOVE CURSOR DOWN SPECIFIED LINE ***
      SUBROUTINE CURDN(LUN, ILINE)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1499) ESC, ILINE
      RETURN
 1499 FORMAT('+', A1, '[', I2, 'B') 
      END

C *** MOVE CURSOR RIGHT SPECIFIED COLUMN ***
      SUBROUTINE CURRGH(LUN, ICOL)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1599) ESC, ICOL
      RETURN
 1599 FORMAT('+', A1, '[', I2, 'C') 
      END

C *** MOVE CURSOR LEFT SPECIFIED COLUMNS ***
      SUBROUTINE CURLFT(LUN, ICOL)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1699) ESC, ICOL
      RETURN
 1699 FORMAT('+', A1, '[', I2, 'D') 
      END

C *** MOVE CURSOR UP SPECIFIED LINE AND BEGINNING OF LINE ***
      SUBROUTINE CURUPH(LUN, ILINE)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1799) ESC, ILINE
      RETURN
 1799 FORMAT('+', A1, '[', I2, 'F') 
      END

C *** MOVE CURSOR DOWN SPECIFIED LINE AND BEGINNING OF LINE ***
      SUBROUTINE CURDNH(LUN, ILINE)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1899) ESC, ILINE
      RETURN
 1899 FORMAT('+', A1, '[', I2, 'E') 
      END

C *** MOVE CURSOR SPECIFIED COLUMN ***
      SUBROUTINE CURCOL(LUN, ICOL)
      INTEGER ESC 
      COMMON /GLBVAR/ESC
      WRITE(LUN, 1999) ESC, ICOL
      RETURN
 1999 FORMAT('+', A1, '[', I2, 'G') 
      END

C *** SAVE CURSOR POSITION (MODE: DEC/SCO - T/F) ***
      SUBROUTINE CURSAV(LUN, MODE)
      INTEGER ESC 
      LOGICAL MODE
      COMMON /GLBVAR/ESC
      IF (MODE .EQ. .TRUE.) WRITE(LUN, 2099) ESC
      IF (MODE .EQ. .FALSE.) WRITE(LUN, 2098) ESC
      RETURN
 2098 FORMAT('+', A1, '[s') 
 2099 FORMAT('+', A1, '[7') 
      END

C *** RESTORE CURSOR POSITION (MODE: DEC/SCO - T/F) ***
      SUBROUTINE CURRES(LUN, MODE)
      INTEGER ESC
      LOGICAL MODE 
      COMMON /GLBVAR/ESC
      IF (MODE .EQ. .TRUE.) WRITE(LUN, 2199) ESC
      IF (MODE .EQ. .FALSE.) WRITE(LUN, 2198) ESC
      RETURN
 2198 FORMAT('+', A1, '[u') 
 2199 FORMAT('+', A1, '[8') 
      END

