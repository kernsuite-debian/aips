      SUBROUTINE CCMERG (DISK, CNO, INVER, OUTVER, INPCMP, OUTCMP,
     *   JBUFS, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Compresses a CLEAN component (CC) table
C# EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2011
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
C   CCMERG sorts AIPS CC tables to bring all components at the same
C   cell together, then it sums them, and finally it resorts the file
C   into the original order (by flux of the new components).
C   Inputs:
C      DISK     I      File disk number
C      CNO      I      File catalog number
C      JBUFS    I      Number words in BUFFER
C   In/out:
C      INVER    I      Input CC version number: 0 => MAXVER
C      OUTVER   I      Output CC version number: 0 => MAXVER+1
C   Output:
C      BUFFER   I(*)   sort buffer
C      INPCMP   I      Number components on input.
C      OUTCMP   I      Number components on output.
C      IRET     I      Error code
C   Common: /MAPHDR/ CATBLK for the affected image file
C   The routine assumes that the CATBLK is in this common already and
C   that the file has been opened in the catalog for WRITE.  (The image
C   file itself does not need to be open.)  The routine assumes that
C   the CFIL common is initialized especially IBAD (BADDISK).
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, OUTVER, JBUFS, INPCMP, OUTCMP, IRET
      REAL      BUFFER(*)
C
      CHARACTER CCTITL(7)*8, TYPE*2, PHNAME*48, CHTMP*2
      INTEGER   SCVER(2), EQUKOL(10), SUMKOL(10), MAXVER, KEY(2,4),
     *   TABUFF(512), BUFSZ, I, LUN(2), JCOL1, JP, IERR, NKEY, NCOL,
     *   NREC, DATP(128,2), EMAX, EMIN, IVER, INSCR, CATSAV(256),
     *   KEYSUB(2,2)
      REAL      TSTKOL(10), FKEY(2,2), EPS
      LOGICAL   T
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA TYPE /'CC'/
      DATA EPS, T /0.05, .TRUE./
      DATA LUN /16,17/
      DATA KEYSUB /4*1/
      DATA CCTITL /'DELTAX  ', 'DELTAY  ', 'MAJOR AX',
     *   'MINOR AX', 'POSANGLE', 'TYPE OBJ', 'FLUX    '/
C-----------------------------------------------------------------------
      INSCR = NSCR + 1
      IF (INSCR.LE.0) NSCR = 0
      IF (INSCR.LE.0) INSCR = 1
C                                       find version numbers
      CALL FNDEXT (TYPE, CATBLK, MAXVER)
      IF (MAXVER.GT.0) GO TO 10
         WRITE (MSGTXT,1000)
         GO TO 990
 10   IF ((INVER.LE.0) .OR. (INVER.GT.MAXVER)) INVER = MAXVER
      IF (OUTVER.LE.0) OUTVER = MAXVER + 1
      SCVER(1) = MAX (OUTVER, MAXVER+1)
      SCVER(2) = SCVER(1) + 1
C                                       Open table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', TYPE, DISK, CNO, INVER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR, TYPE, INVER
         GO TO 990
C                                       Number input components
 20   INPCMP = TABUFF(5)
C                                       Find columns
      CALL FILL (10, 0, EQUKOL)
      CALL FILL (10, 0, SUMKOL)
      JCOL1 = 2
      IF (TABUFF(10).GE.7) JCOL1 = 6
      CALL FNDCOL (JCOL1, CCTITL(1), 8, T, TABUFF, EQUKOL, IERR)
      IF (IERR.EQ.0) GO TO 25
         WRITE (MSGTXT,1020) IERR
         GO TO 990
 25   CALL FNDCOL (1, CCTITL(7), 8, T, TABUFF, SUMKOL, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
C                                       insort
 30   KEY(1,1) = EQUKOL(2)
      KEY(2,1) = 0
      KEY(1,2) = EQUKOL(1)
      KEY(2,2) = 0
C                                       outsort
      KEY(1,3) = -SUMKOL(1)
      KEY(2,3) = 0
      KEY(1,4) = EQUKOL(2)
      KEY(2,4) = 0
C                                       sort weights: not sum cols
      FKEY(1,1) = -1.0
      FKEY(2,1) = 0.0
      FKEY(1,2) = 1.0
      FKEY(2,2) = 0.0
C                                       tolerances
      TSTKOL(1) = EPS * ABS (CATR(KRCIC))
      TSTKOL(2) = EPS * ABS (CATR(KRCIC))
      TSTKOL(3) = MIN (TSTKOL(1), TSTKOL(2))
      TSTKOL(4) = TSTKOL(3)
      TSTKOL(5) = 1.0
      TSTKOL(6) = 0.0
C                                       Close the table file
      CALL TABIO ('CLOS', 0, 0, DATP, TABUFF, IERR)
C                                       Sort: to scratch (1)
      IRET = 0
      IF (INPCMP.GT.0) THEN
         BUFSZ = JBUFS * 2
         WRITE (MSGTXT,1030) SCVER(1)
         CALL MSGWRT (2)
         CALL TABSRT (DISK, CNO, TYPE, INVER, SCVER(1), KEY, KEYSUB,
     *      FKEY, TABUFF, CATBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Merge: scratch(2)
         WRITE (MSGTXT,1031) SCVER(2)
         CALL MSGWRT (2)
         CALL TABMRG (DISK, CNO, TYPE, SCVER(1), SCVER(2), EQUKOL,
     *      SUMKOL, TSTKOL, BUFFER, TABUFF, CATBLK, OUTCMP, IRET)
         IF (IRET.NE.0) GO TO 900
         CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, PHNAME, IERR)
         CALL ZDESTR (DISK, PHNAME, IERR)
C                                       Sort to output
         WRITE (MSGTXT,1032) OUTVER
         CALL MSGWRT (2)
         CALL TABSRT (DISK, CNO, TYPE, SCVER(2), OUTVER, KEY(1,3),
     *      KEYSUB, FKEY, TABUFF, CATBLK, IRET)
C                                       empty table
      ELSE IF (INVER.NE.OUTVER) THEN
         CALL ZPHFIL (TYPE, DISK, CNO, OUTVER, PHNAME, IERR)
         CALL ZDESTR (DISK, PHNAME, IERR)
         CALL TABCOP ('CC', INVER, OUTVER, LUN(1), LUN(2), DISK, DISK,
     *      CNO, CNO, CATBLK, TABUFF, TABUFF(257), IRET)
         END IF
C                                       extension file clean up
C                                       FNDEXT, TABINI called FXHDEX
 900  DO 910 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+I-1), CHTMP)
         IF (CHTMP.EQ.TYPE) THEN
            JP = KIVER + I - 1
            IF (CATBLK(JP).GT.MAXVER) THEN
               EMAX = CATBLK(JP)
               GO TO 920
               END IF
            END IF
 910     CONTINUE
      GO TO 999
 920  EMIN = MAXVER + 1
      IF (IRET.EQ.0) EMIN = MAX (EMIN, OUTVER+1)
C                                       delete the extras
      IF (EMIN.LE.EMAX) THEN
         CATBLK(JP) = EMIN - 1
         DO 930 IVER = EMIN,EMAX
            CALL ZPHFIL (TYPE, DISK, CNO, IVER, PHNAME, IERR)
            CALL ZDESTR (DISK, PHNAME, IERR)
            IF (IERR.GT.1) CATBLK(JP) = IVER
 930        CONTINUE
C                                       Update catalog
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', TABUFF, IERR)
         END IF
C                                       Delete scratch files
      IF (NSCR.LT.INSCR) THEN
         EMIN = NSCR - INSCR + 1
         CALL FILL (EMIN, 2, DATP)
         CALL COPY (256, CATBLK, CATSAV)
         CALL MAPCLR (EMIN, SCRVOL(INSCR), SCRCNO(INSCR), DATP, TABUFF)
         CALL COPY (256, CATSAV, CATBLK)
         NSCR = INSCR - 1
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 8
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR: NO CC FILES ASSOCIATED WITH IMAGE')
 1010 FORMAT ('ERROR:',I7,' OPENING ',A2,' FILE VERSION',I3)
 1020 FORMAT ('ERROR:',I7,' FINDING NEEDED CC FILE COLUMNS')
 1030 FORMAT ('Start sort to CC file scratch version',I4)
 1031 FORMAT ('Start merge to CC file scratch version',I4)
 1032 FORMAT ('Start sort to CC file output version',I4)
      END
