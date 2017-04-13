      SUBROUTINE OFMLST (DOCRT, PRTNAM, N, OFM, IOFM)
C-----------------------------------------------------------------------
C! verbs setting, saving, and retrieving the TV OFM tables
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2000, 2004, 2007, 2011
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
C   does OFMLIST verb to list the current OFM on the line printer
C   or the terminal.
C   Inputs:
C      PRTNAM   C*48      File name to save print
C      N        I         First dimension of OFM, IOFM
C      OFM      R(N,3)    Current OFM
C   In/Out:
C      DOCRT    R         > 0 => CRT, > 72 => CRT width, <= 0 printer
C                         reset in abs value to width used
C   Outputs:
C      IOFM     I(N,3)    Integer scaled OFM
C   Common: DERR.INC
C                         returns error codes in ERRNUM, PNAME
C-----------------------------------------------------------------------
      INTEGER   N, IOFM(N,3)
      REAL      DOCRT, OFM(N,3)
      CHARACTER PRTNAM*48
C
      INTEGER   BUFFER(256), IERR, JERR, I, J, ILINE, PAGE, ICOL, II,
     *   I2, JJ, J1, J2, INCR, IFIND, NACROS, PLUN, LS, POTERR, NCOL
      LOGICAL   F, QUIT
      CHARACTER PRGNAM*6, TITL1*132, TITL2*132, PLINE*132, SCRTCH*132
C
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA PRGNAM /'OFMLST'/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) DOCRT = MIN (-1.0,
     *   DOCRT)
C                                       Set values for crt.
      IF (DOCRT.GT.0.0) THEN
         PLUN = 5
         CALL LSERCH ('SRCH', PLUN, IFIND, F, IERR)
         CALL ZWINC (NCOL)
         NACROS = DOCRT + 0.5
         IF ((NACROS.LE.72) .OR. (NACROS.GT.NCOL)) NACROS = NCOL
C                                       Set values for printer.
      ELSE
         IF (PRTNAM.EQ.' ') DOCRT = -1.0
         CALL LPOPEN (PRTNAM, DOCRT, PLUN, IFIND, NACROS, BUFFER, IERR)
         END IF
      POTERR = 57
      IF (IERR.NE.0) GO TO 990
      PAGE = 0
      ILINE = 900
      POTERR = 50
      TITL2 = ' '
C                                       Scale OFM values
      LS = 10
      J = OFMINP + 1
      DO 305 I = 1,J
         IOFM(I,1) = OFM(I,1) * 10000.0 + 0.5
         IOFM(I,2) = OFM(I,2) * 10000.0 + 0.5
         IOFM(I,3) = OFM(I,3) * 10000.0 + 0.5
 305     CONTINUE
C                                       Check for color OFM
      DO 310 I = 1,J
         IF (IOFM(I,1).NE.IOFM(I,2)) GO TO 330
         IF (IOFM(I,1).NE.IOFM(I,3)) GO TO 330
         IF (IOFM(I,2).NE.IOFM(I,3)) GO TO 330
 310     CONTINUE
C                                       Black and White:
      ICOL = NACROS / 12
      INCR = LS * ICOL
      TITL1 = ' '
      TITL1(25:) = 'Current OFM is Black and White'
      DO 320 I = 1,J,INCR
         IF ((I.EQ.1) .OR. (DOCRT.GT.-2.5)) THEN
            WRITE (PLINE,1310) ('  In-  Out  ', JJ = 1,ICOL)
            CALL PRTALN (PLUN, IFIND, DOCRT, NACROS, TITL1, TITL2,
     *         PLINE, ILINE, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 390
            END IF
         I2 = MIN (I+LS-1, J)
         DO 315 II = I,I2
            J1 = II - 1
            J2 = II + INCR - 1
            J2 = MIN (J2, J) - 1
            WRITE (PLINE,1311) (JJ, IOFM(JJ+1,1), JJ = J1,J2,LS)
            CALL PRTALN (PLUN, IFIND, DOCRT, NACROS, TITL1, TITL2,
     *         PLINE, ILINE, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 390
 315        CONTINUE
 320     CONTINUE
      GO TO 390
C                                       Color
 330  ICOL = NACROS / 24
      INCR = LS * ICOL
      TITL1 = ' '
      TITL1(25:) = 'Current OFM is in color (colour)'
      DO 340 I = 1,J,INCR
         IF ((I.EQ.1) .OR. (DOCRT.GT.-2.5)) THEN
            WRITE (PLINE,1330) ('  In- Blue:green:  red  ', JJ = 1,ICOL)
            CALL PRTALN (PLUN, IFIND, DOCRT, NACROS, TITL1, TITL2,
     *         PLINE, ILINE, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 390
            END IF
         I2 = MIN (I+LS-1, J)
         DO 335 II = I,I2
            J1 = II - 1
            J2 = II + INCR - 1
            J2 = MIN (J2, J) - 1
            WRITE (PLINE,1331) (JJ, IOFM(JJ+1,1), IOFM(JJ+1,2),
     *         IOFM(JJ+1,3), JJ = J1,J2,LS)
            CALL PRTALN (PLUN, IFIND, DOCRT, NACROS, TITL1, TITL2,
     *         PLINE, ILINE, PAGE, SCRTCH, QUIT)
            IF ((QUIT) .OR. (ERRNUM.NE.0)) GO TO 390
 335        CONTINUE
 340     CONTINUE
C                                       Close printer.
 390  IF (DOCRT.LE.0.0) CALL LPCLOS (PLUN, IFIND, ILINE, JERR)
      POTERR = 0
C                                       POPS error management
 990  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1310 FORMAT (11A12)
 1311 FORMAT (11(I5,'-',I5.4,1X))
 1330 FORMAT (5A24)
 1331 FORMAT (5(I5,'-',I5.4,':',I5.4,':',I5.4,1X))
      END
