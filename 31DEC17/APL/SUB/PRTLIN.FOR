      SUBROUTINE PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2, LINE, NLINE,
     *   IPAGE, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! prints line on printer or terminal with page-full handling, headers
C# Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2004-2005
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
C   PRTLIN handles actual printing on the line printer, disk file, or
C   CRT for tasks.  For the CRT, it also handles page-full user
C   communication.
C   Inputs:
C      OUTLUN  I       LUN for print device (open): 1 => line printer,
C                         3 => text (disk) file, 5 => terminal
C      OUTIND  I       FTAB pointer for print device
C      DOCRT   R       > 0. => use CRT, else line printer or disk
C                      For text-file output only
C                         <= -1.5 -> suppress form-feed characters
C                         <= -2.5 -> suppress all page headers
C      NC      I       Number characters in line
C      T1      C*132   Page title line 1, if ' ' then ignored
C      T2      C*132   Page title line 2, if ' ' then ignored
C      LINE    C*132   Text line, printed even if ' '
C   In/out:
C      NLINE   I       Number lines so far on page
C                         > 1000 => just ask about continuing (DOCRT
C                                   true).  Do not print anything.
C                         =  999 => print new page title(s) and LINE
C                                   but do not ask the user about
C                                   whether he wants to continue
C      IPAGE   I       Current page number
C                         = 0 => start of print job (user not asked on
C                                DOCRT true)
C   Output:
C      SCRTCH  C*(*)   Scratch core > 132
C      IERR    I       Error code: 0 => OK, -1 user asks to quit
C   Hints:
C      (1) First call: set IPAGE = 0, NLINE = (e.g.) 990
C      (2) Thereafter, leave NLINE and IPAGE alone in your code, unless
C      (3) to force a page break, reset NLINE to (e.g.) 990
C   where 990 is an arbitrary number < 999 and > number of lines on the
C   printer page.
C      (4) No special characters are allowed in T1, T2, and LINE
C-----------------------------------------------------------------------
      INTEGER   OUTLUN, OUTIND, NC, NLINE, IPAGE, IERR
      REAL      DOCRT
      CHARACTER SCRTCH*(*), T1*(*), T2*(*), LINE*(*)
C
      CHARACTER TIME*8, DATE*12, CPAGE*2, ONZE*1, SYSN*8, RLSN*9
      INTEGER   IT(6), IAC1, IAC2, IACL, IACT, JTRIM, ITRIM, NCOL, NNC
      LOGICAL   DOLINE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE ONZE
      DATA ONZE /' '/
C-----------------------------------------------------------------------
      DOLINE = .TRUE.
      NNC = LEN (LINE)
      NNC = MIN (NNC, NC)
      IACL = JTRIM (LINE(:NNC))
      IACL = MAX (IACL, 1)
C                                       line printer OR disk
      IF (DOCRT.LE.0.0) THEN
         IERR = 0
         IF (NLINE.GT.1000) GO TO 999
C                                       page break
         IF (NLINE.GE.PRTMAX-1) THEN
            CALL WORD (HSTNAM, SYSN)
            RLSN = '(' // RLSNAM(:7) // ')'
            IPAGE = IPAGE + 1
            NLINE = 2
            CALL ZDATE (IT(4))
            CALL ZTIME (IT(1))
            CALL TIMDAT (IT(1), IT(4), TIME, DATE)
            NNC = LEN (T1)
            NNC = MIN (NNC, NC)
            IAC1 = JTRIM (T1(:NNC))
            NNC = LEN (T1)
            NNC = MIN (NNC, NC)
            IAC2 = JTRIM (T2(:NNC))
            IF ((IACL.GT.1) .AND. (LINE(:IACL).EQ.T1(:IAC1)) .AND.
     *         (DOCRT.GT.-2.5)) DOLINE = .FALSE.
            IF ((IACL.GT.1) .AND. (LINE(:IACL).EQ.T2(:IAC2)) .AND.
     *         (DOCRT.GT.-2.5)) DOLINE = .FALSE.
C                                       line printer
            IF (OUTLUN.EQ.1) THEN
C                                       no form feed on first pass
C                                       form feeds on latter passes
               IF (NC.GT.100) THEN
                  WRITE (OUTLUN,1000,ERR=900) ONZE, SYSN, TSKNAM,
     *               RLSN, NLUSER, DATE, TIME, IPAGE
               ELSE
                  WRITE (OUTLUN,1001,ERR=900) ONZE, SYSN, TSKNAM,
     *               RLSN, NLUSER, DATE, TIME, IPAGE
                  END IF
               ONZE = '1'
               IF (IAC1.GT.0) THEN
                  WRITE (OUTLUN,1002,ERR=900) T1(:IAC1)
                  NLINE = NLINE + 1
                  END IF
               IF (IAC2.GT.0) THEN
                  WRITE (OUTLUN,1002,ERR=900) T2(:IAC2)
                  NLINE = NLINE + 1
                  END IF
               IF (NLINE.GT.2) THEN
                  WRITE (OUTLUN,1003,ERR=900)
                  NLINE = NLINE + 1
                  END IF
C                                       text file on disk
            ELSE IF (DOCRT.GT.-2.5) THEN
               IF (DOCRT.GT.-1.5) THEN
                  CALL ZFORMF (CPAGE)
               ELSE
                  CPAGE = ' '
                  END IF
               IF (NC.GT.100) THEN
                  WRITE (SCRTCH,1020,ERR=900) CPAGE, SYSN, TSKNAM,
     *               RLSN, NLUSER, DATE, TIME, IPAGE
               ELSE
                  WRITE (SCRTCH,1021,ERR=900) CPAGE, SYSN, TSKNAM,
     *               RLSN, NLUSER, DATE, TIME, IPAGE
                  END IF
C                                       Use ITRIM to allow form feed
               IACT = ITRIM (SCRTCH(:NC))
               CALL ZTXIO ('WRIT', OUTLUN, OUTIND, SCRTCH(:IACT), IERR)
               IF (IERR.NE.0) GO TO 999
               IF (IAC1.GT.0) THEN
                  CALL ZTXIO ('WRIT', OUTLUN, OUTIND, T1(:IAC1), IERR)
                  IF (IERR.NE.0) GO TO 999
                  NLINE = NLINE + 1
                  END IF
               IF (IAC2.GT.0) THEN
                  CALL ZTXIO ('WRIT', OUTLUN, OUTIND, T2(:IAC2), IERR)
                  IF (IERR.NE.0) GO TO 999
                  NLINE = NLINE + 1
                  END IF
               IF (NLINE.GT.2) THEN
                  SCRTCH(:1)  = ' '
                  CALL ZTXIO ('WRIT', OUTLUN, OUTIND, SCRTCH(:1), IERR)
                  IF (IERR.NE.0) GO TO 999
                  NLINE = NLINE + 1
                  END IF
               END IF
            END IF
         IF (DOLINE) THEN
            NLINE = NLINE + 1
            IF (OUTLUN.EQ.1) THEN
               WRITE (OUTLUN,1002,ERR=900) LINE(:IACL)
            ELSE
               CALL ZTXIO ('WRIT', OUTLUN, OUTIND, LINE(:IACL), IERR)
               END IF
            END IF
C                                       CRT terminal
      ELSE
C                                       page full: continue?
         IF (NLINE.GE.ABS(CRTMAX)-2) THEN
            IF ((NLINE.NE.999) .AND. (IPAGE.NE.0)) THEN
               WRITE (SCRTCH,1100)
               CALL ZTTYIO ('WRIT', OUTLUN, OUTIND, NC, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZTTYIO ('READ', OUTLUN, OUTIND, NC, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 999
               IF (SCRTCH(:1).EQ.'Q') IERR = -1
               IF (SCRTCH(:1).EQ.'q') IERR = -1
               IF (IERR.NE.0) GO TO 999
               CALL ZWINC (NCOL)
               IF (NLINE.GT.1000) GO TO 999
               END IF
C                                       Do new page with titles
            CALL WORD (HSTNAM, SYSN)
            RLSN = '(' // RLSNAM(:7) // ')'
            IPAGE = IPAGE + 1
            CALL ZDATE (IT(4))
            CALL ZTIME (IT(1))
            CALL TIMDAT (IT(1), IT(4), TIME, DATE)
            WRITE (SCRTCH,1110) SYSN, TSKNAM, RLSN, NLUSER, DATE, TIME,
     *         IPAGE
            IACT = JTRIM (SCRTCH(:NC))
            CALL ZTTYIO ('WRIT', OUTLUN, OUTIND, IACT, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            NLINE = 1
            NNC = LEN (T1)
            NNC = MIN (NNC, NC)
            IAC1 = JTRIM (T1(:NNC))
            IF (IAC1.GT.0) THEN
               CALL ZTTYIO ('WRIT', OUTLUN, OUTIND, IAC1, T1, IERR)
               IF (IERR.NE.0) GO TO 999
               NLINE = NLINE + 1
               END IF
            NNC = LEN (T1)
            NNC = MIN (NNC, NC)
            IAC2 = JTRIM (T2(:NNC))
            IF (IAC2.GT.0) THEN
               CALL ZTTYIO ('WRIT', OUTLUN, OUTIND, IAC2, T2, IERR)
               IF (IERR.NE.0) GO TO 999
               NLINE = NLINE + 1
               END IF
            IF ((IACL.GT.1) .AND. (LINE(:IACL).EQ.T1(:IAC1))) DOLINE =
     *         .FALSE.
            IF ((IACL.GT.1) .AND. (LINE(:IACL).EQ.T2(:IAC2))) DOLINE =
     *         .FALSE.
            END IF
C                                       actual output
         IF (DOLINE) THEN
            NLINE = NLINE + 1
            CALL ZTTYIO ('WRIT', OUTLUN, OUTIND, IACL, LINE, IERR)
            END IF
         END IF
      GO TO 999
C                                       Write to printer error
 900  IERR = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A1,5X,A8,3X,A5,A9,4X,I4,15X,A12,2X,A8,15X,'Page',I5/)
 1001 FORMAT (A1,1X,A8,2X,A5,A9,3X,I4,5X,A12,1X,A8,4X,'Page',I5/)
 1002 FORMAT (' ',A)
 1003 FORMAT (' ')
 1020 FORMAT (A,5X,A8,3X,A5,A9,4X,I4,15X,A12,2X,A8,15X,'Page',I5)
 1021 FORMAT (A,1X,A8,2X,A5,A9,3X,I4,5X,A12,1X,A8,4X,'Page',I5)
 1100 FORMAT ('Type Q to stop, just hit RETURN to continue')
 1110 FORMAT (1X,A8,2X,A5,A9,3X,I4,5X,A12,1X,A8,4X,'Page',I5)
      END
