      SUBROUTINE GETNX (LUN, DISK, CNO, CATBLK, ISUB, BUFFER, NOSCAN,
     *   TSCAN)
C-----------------------------------------------------------------------
C! Make list of time intervals in an NX table
C# UV EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2013
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
C   GETNX reads the NX table and makes a list of scan boundaries
C   Inputs:
C      LUN      I      LUN to use
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   Header
C      ISUB     I      Limit to subarray ISUB - 0 -> all
C   Outputs
C      BUFFER   I(*)   Scratch buffer
C      NOSCAN   I      Number of times in TSCAN
C      TSCAN    R(*)   Time of scan boundaries
C      SSCAN    I(*)   Subarray of this interval
C-----------------------------------------------------------------------
      INTEGER   LUN, DISK, CNO, CATBLK(256), ISUB, BUFFER(*), NOSCAN
      REAL      TSCAN(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   VER, INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC), IERR,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, NROW, IROW
      REAL      TIME, DTIME, TEPS
      INCLUDE 'INCS:DMSG.INC'
C                                       8 milliseconds
      DATA TEPS /9.E-8/
C-----------------------------------------------------------------------
      VER = 1
      CALL NDXINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INDEX TABLE'
         GO TO 900
         END IF
      NROW = BUFFER(5)
      NOSCAN = 1
      DO 100 IROW = 1,NROW
         CALL TABNDX ('READ', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INDEX TABLE'
            GO TO 900
            END IF
         IF ((ISUB.LE.0) .OR. (SUBARR.LE.0) .OR. (ISUB.EQ.SUBARR)) THEN
            IF (NOSCAN.EQ.1) THEN
               TSCAN(NOSCAN) = TIME - 0.5 * DTIME - TEPS
            ELSE
               TSCAN(NOSCAN) = (TSCAN(NOSCAN) + TIME - 0.5*DTIME) / 2.0
               END IF
            TSCAN(NOSCAN+1) = TIME + 0.5 * DTIME + TEPS
            NOSCAN = NOSCAN + 1
            END IF
 100     CONTINUE
C
 900  IF (IERR.NE.0) THEN
         CALL MSGWRT (6)
         NOSCAN = 0
         END IF
      CALL TABNDX ('CLOS', BUFFER, INXRNO, NXKOLS, NXNUMV, TIME, DTIME,
     *   IDSOUR, SUBARR, VSTART, VEND, FREQID, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETNX ERROR',I4,' ON ',A)
      END
