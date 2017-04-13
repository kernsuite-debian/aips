      SUBROUTINE INIT
C-----------------------------------------------------------------------
C! initializes symbol, procedure text tables, and commons for POPS
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2003
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
C   INIT initializes the K, LISTF, and commons for POPS.
C   The K array and LISTF are loaded from file ME1000nn (nn = NPOPS-1)
C   User commons are initialized by UINIT which is called by INIT.
C   LISTF is now kept in common rather than in a scratch area on disk.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, PHNAME*48
      INTEGER   FIND, IERR, MEMLUN, POTERR, NREC, IBEG, IEND, III, I
      LOGICAL   EXCL, NOMAP, WAIT
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA NOMAP, EXCL, WAIT /2*.FALSE., .TRUE./
      DATA PRGNAM /'INIT '/
      DATA MEMLUN /14/
C-----------------------------------------------------------------------
      CALL RFILL (5, HBLANK, KPAK)
      IUNIT = 1
      IF (NPOPS.GT.NINTRN) IUNIT = 3
      IF (TSKNAM(1:5).EQ.'AIPSC') IUNIT = 3
      KARLIM = NCHLIN
C                                       COMMON variables
      SLIM = 1000
      NEXTP = 0
      LPGM = 2
      KT = 51
      LPAGE = LBLOCK
      MPAGE = KBLOCK + LPAGE
      STORE2 = 0
C                                       Error indicators
      ERRLEV = 0
      ERRNUM = 0
C
      IDEBUG = -1
      CALL FILL (SLIM, 0, STACK)
      CALL FILL (SLIM, 0, CSTACK)
      CALL FILL (5, 0, IERROR)
      DO 5 NREC = 1,5
         PNAME(NREC) = ' '
 5       CONTINUE
C                                       Open pops memory file.
      CALL ZPHFIL ('ME', 1, 0, 0, PHNAME, IERROR)
      CALL ZOPEN (MEMLUN, FIND, 1, PHNAME, NOMAP, EXCL, WAIT, IERROR)
      POTERR = 61
      IF (IERROR(1).NE.0) GO TO 980
C                                       Read basic copy of K array
      IBEG = 1
      IEND = MPAGE - LPAGE
      III = 1
      DO 10 NREC = IBEG,IEND
         CALL ZFIO ('READ', MEMLUN, FIND, NREC, K(III), IERROR(2))
         IF (IERROR(2).NE.0) GO TO 970
         III = III + 256
 10      CONTINUE
C                                       Read basic LISTF array
      NREC = IEND + 1
      III = 1
      DO 30 I = 1,LPAGE
         CALL ZFIO ('READ', MEMLUN, FIND, NREC, LISTF(III), IERROR(2))
         IF (IERROR(2).NE.0) GO TO 970
         III = III + 256
         NREC = NREC + 1
 30      CONTINUE
C                                       Close file.
      CALL ZCLOSE (MEMLUN, FIND, IERR)
      POTERR = 49
      IF (IERR.NE.0) GO TO 980
C                                       Initialize.
C                                       One
      L = K(4)
      CALL HUNT (K, L, 1, 2, 1.0)
      TAG = L + 2
      ONE = TAG
C                                       Zero
      L = K(4)
      CALL HUNT (K, L, 1, 2, 0.0)
      TAG = L + 2
      ZERO = TAG
C                                       True
      L = K(1)
      CALL CHR2H (4, 'TRUE', 1, KPAK)
      CALL CHUNT (K, L, 4, 4, KPAK)
      TAG = K(L+2)
      TRUE = TAG
C                                       False
      L = K(1)
      CALL CHR2H (5, 'FALSE', 1, KPAK)
      CALL CHUNT (K, L, 5, 4, KPAK)
      TAG = K(L+2)
      FALSE = TAG
C                                       user commons
      CALL UINIT
      MODE = 0
      IPT = '>'
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C                                       Error handler.
 970  CALL ZCLOSE (MEMLUN, FIND, IERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
