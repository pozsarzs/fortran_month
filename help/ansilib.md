# Console control routines with ANSI Escape sequence

## Color settings

|function            |subroutine              |parameters     |
|--------------------|------------------------|---------------|
|reset colors        |colres(lun)             |int            |
|set foreground color|colfgr(lun, reset, icol)|int, logic, int|
|set background color|colbgr(lun, reset, icol)|int, logic, int|
|invert colors       |colinv(lun)             |int            |


## Erase commands
|function                                     |subroutine |parameters|
|---------------------------------------------|-----------|----------|
|erase from cursor to end of screen           |erasce(lun)|int       |
|erase from cursor to beginning of screen     |erasch(lun)|int       |
|erase entire screen                          |erascr(lun)|int       |
|erase entire screen and the scrollback buffer|erascb(lun)|int       |
|erase from cursor to end of line             |eralie(lun)|int       |
|erase from cursor to beginning of line       |eralih(lun)|int       |
|erase entire line                            |eralin(lun)|int       |


## Cursor control
|function                                             |subroutine               | parameters         |
|-----------------------------------------------------|-------------------------|--------------------|
|move to home of screen                               |cursch(lun)              |int                 |
|move cursor specified position                       |curpos(lun, mode, ix, iy)|int, logic, int, int|
|move cursor up specified line                        |curup(lun, iline)        |int, int            |
|move cursor down specified line                      |curdn(lun, iline)        |int, int            |
|move cursor right specified column                   |currgh(lun, icol)        |int, int            |
|move cursor left specified columns                   |curlft(lun, icol)        |int, int            |
|move cursor up specified line and beginning of line  |curuph(lun, iline)       |int, int            |
|move cursor down specified line and beginning of line|curdnh(lun, iline)       |int, int            |
|move cursor specified column                         |curcol(lun, icol)        |int, int            |
|save cursor position (mode: dec/sco - t/f)           |cursav(lun, mode)        |int, int            |
|restore cursor position (mode: dec/sco - t/f)        |curres(lun, mode)        |int, int            |
