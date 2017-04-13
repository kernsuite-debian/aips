      SUBROUTINE ZPRPAS (PASS, BUFF, IERR)
C-----------------------------------------------------------------------
C! prompt user and read 12-character password (invisible) from CRT
C# Z Terminal System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   ZPRPAS prompts the user on his terminal with the prompt string
C   "Password: " and then reads back a 12-character "password" without
C   anything being visible on the screen.
C   Outputs:
C      PASS   C*12     Password - 12 unpacked characters: left
C                                 justigied and blank filled.
C      BUFF   I(256)   Scratch buffer (if needed)
C      IERR   I        Error code: 0 => ok
C                         ??? => I/O error of some sort
C   VAX version
C-----------------------------------------------------------------------
      CHARACTER PASS*12
      INTEGER   BUFF(256), IERR
C
      INTEGER   FIB$M_WRITE
      PARAMETER (FIB$M_WRITE = '00000100'X)
      CHARACTER DEVNAM*2
      HOLLERITH PASS2(20)
      INTEGER   SYS$ASSIGN, SYS$QIOW, SYS$DASSGN, FIBDES(2), FIB(4),
     *   CHAN, NBYT1, NBYT2, IOSTAT, FCODE, I
      INTEGER*2 IOSB(4)
      BYTE      IPASS(12), CRLF(2), PROMPT(12)
      INCLUDE '($IODEF)'
      EQUIVALENCE (IPASS, PASS2)
      DATA PROMPT /10,'P','A','S','S','W','O','R','D',' ',':',' '/
      DATA CRLF /13,10/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Use QIO to access terminal.
C                                       Make a FIB
      FIB(1) = FIB$M_WRITE
      FIBDES(1) = 16
      FIBDES(2) = %LOC(FIB)
C                                       Assign a channel
      DEVNAM = 'TT'
      IOSTAT = SYS$ASSIGN (DEVNAM, CHAN, , ,)
      IF (IOSTAT.NE.1) GO TO 980
C                                       Prompted read with no echo
      FCODE = IOR (IO$_READPROMPT, IO$M_NOECHO)
      NBYT1 = 20
      NBYT2 = 12
      DO 10 I = 1,12
         IPASS(I) = 32
 10      CONTINUE
      IOSTAT = SYS$QIOW ( , %VAL(CHAN), %VAL(FCODE), IOSB, , , PASS2,
     *   %VAL(NBYT1), , , PROMPT, %VAL(NBYT2))
      IF (IOSTAT.NE.1) GO TO 980
      NBYT1 = 1
      FCODE = IO$_WRITEVBLK
      IOSTAT = SYS$QIOW ( , %VAL(CHAN), %VAL(FCODE), IOSB, , , CRLF,
     *   %VAL(NBYT1), , , , )
C                                       Deaccess the terminal.
      FCODE = IO$_DEACCESS
      IOSTAT = SYS$QIOW ( , %VAL(CHAN), %VAL(FCODE), , , ,,,,,,)
C                                       Deassign the channel.
      IOSTAT = SYS$DASSGN (%VAL(CHAN))
      DO 20 I = 1,12
         IF (IPASS(I).LT.32) IPASS(I) = 32
 20      CONTINUE
      CALL H2CHR (12, 1, PASS2, PASS)
      GO TO 999
C                                       QIO error handling.
 980  CALL ZERROR ('ZPRPAS', IOSTAT, ' ', -999, .FALSE.)
      IERR = 1
C
 999  RETURN
      END
