      SUBROUTINE SUMARY (NUMCMP, CLNVOL, VER, CNO, CATBLK, LUNCLN,
     *   NCLEAN, X, Y, FLX, ICOUNT, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Accumulates and lists CLEAN components
C# EXT-appl Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   SUMARY is a diagonostic routine which reads a component file,
C   collects the flux density cleaned from the first NUMCMP locations
C   and then may list the results. Component locations are given
C   in degrees; flux densities in Janskys.
C   Input:
C      NUMCMP   I           Number of clean locations to be sumarized.
C      CLNVOL   I           Volumn number of the CLEAN components file.
C      VER      I           CC file version number.
C      CNO      I           Catalog slot number of clean map.
C      CATBLK   I(256)      Catalog header block of clean map.
C      LUNCLN   I           Logical unit number to use.
C      NCLEAN   I           Number of CLEAN components on the file.
C                           If 0 searches entire file.
C      BUFFER   I(768)      Buffer for reading CLEAN components file.
C   Output:
C      NCLEAN   I           No. CLEAN comps searched.
C      X        R(NUMCMP)   Array of the RA values.
C      Y        R(NUMCMP)   Array of the Dec values.
C      FLX      R(NUMCMP)   Array of the cell summed flux densities
C      ICOUNT   I           Number of cells actually populated.
C      IERR     I           Error code: 0 => ok, else TABINI or TABIO
C   WARNING: THIS IS A NON-STANDARD ROUTINEl IT USES A BRUTE FORCE SORT
C-----------------------------------------------------------------------
      INTEGER   IRNO, NCLEAN, ICOUNT, NUMCMP, K4, I4, IDX
      INTEGER   IERR, NREC, VER, CNO, CATBLK(256), LREC,
     *   CLNVOL, LUNCLN, BUFFER(768), NKEY, LNREC, IER
      REAL      FLX(*), RECORD(20), X(*), Y(*),  XX, YY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Skip if NUMCMP = 0
      IERR = 0
      ICOUNT = 0
      IF (NUMCMP.LE.0) GO TO 999
      CALL RFILL (20, 0.0, RECORD)
C                                       Open CLEAN component file.
      LREC = 3
      NREC = 1
      NKEY = 0
      CALL TABINI ('READ', 'CC', CLNVOL, CNO, VER, CATBLK, LUNCLN, NKEY,
     *    NREC, LNREC, BUFFER(513), BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
 10   IRNO = BUFFER(5)
      IF (NCLEAN.LE.0) NCLEAN = IRNO
      IF (NCLEAN.GT.IRNO) NCLEAN = IRNO
C                                      Read first record.
      IRNO = 1
 15   CALL TABIO ('READ', 0, IRNO, RECORD, BUFFER, IERR)
      IF (IERR.GT.0) GO TO 900
C                                      Check if point.
      IRNO = IRNO + 1
      IF ((RECORD(7).NE.0.0) .OR. (IERR.LT.0)) GO TO 15
      ICOUNT = 1
      X(1) = RECORD(2)
      Y(1) = RECORD(3)
      FLX(1) = RECORD(1)
      IF (NCLEAN.LE.IRNO-1) GO TO 900
C                                       loop over all clean comps
C                                       no matter what NUMCMP !!
      DO 200 K4 = IRNO,NCLEAN
C                                       Read
         CALL TABIO ('READ', 0, K4, RECORD, BUFFER, IERR)
         IF (IERR.GT.0) GO TO 900
C                                       Make sure point.
         IF ((RECORD(7).NE.0.0) .OR. (IERR.LT.0)) GO TO 200
            XX = RECORD(2)
            YY = RECORD(3)
C                                       does current point match
C                                       position of previous point
            DO 150 I4 = 1,ICOUNT
               IDX = I4
               IF ((X(I4).EQ.XX) .AND. (Y(I4).EQ.YY)) GO TO 160
 150           CONTINUE
C                                       No match found
            IF (ICOUNT.GE.NUMCMP) GO TO 200
               ICOUNT = ICOUNT + 1
               X(ICOUNT) = XX
               Y(ICOUNT) = YY
               FLX(ICOUNT) = RECORD(1)
               GO TO 200
C                                       Match found.
 160        CONTINUE
               FLX(IDX) = FLX(IDX) + RECORD(1)
 200     CONTINUE
C                                       Print summary
C                                       Close CLEAN component file.
 900  CALL TABIO ('CLOS', 0, IRNO, RECORD, BUFFER, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUMARY: ERROR',I3,' OPENING FILE ')
      END
