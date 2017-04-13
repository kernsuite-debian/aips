      SUBROUTINE PDRGET (DISK, CNO, SUBA, LUN, CATBLK, NOIF, IREF, PD,
     *   BUFFER, IERR)
C-----------------------------------------------------------------------
C! Reads R-L phase differences and reference antenna from AN table
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009
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
C   Reads R-L phase differences and reference antenna from AN table
C   Inputs:
C      DISK     I      Disk number
C      CNO      I      Catalog slot number
C      SUBA     I      Subarray number (AN ver)
C      LUN      I      LUN to use.
C      CATBLK   I(256) Catalog header record
C      NOIF     I      Number of IFs
C   Input/output:
C      BUFFER   I(*)   I/O buffer
C   Output:
C      IREF     I      Reference antenna
C      PD       R(*)   Array of phase differences, 1 per IF
C      IERR     I      Error code , 0=> OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SUBA, LUN, CATBLK(256), NOIF, IREF,
     *   BUFFER(*), IERR
      REAL      PD(NOIF)
C
      CHARACTER REFLAB*8, PDLAB*8, KEYLAB*8
      INTEGER   IVER, LOCS, KEYTYP, NUMKEY, LOOP, SAVSUP
      REAL      KEYVAR(2)
      DOUBLE PRECISION KEYVAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (KEYVAR, KEYVAD)
      DATA REFLAB /'P_REFANT'/, PDLAB /'P_DIFF  '/
C-----------------------------------------------------------------------
C                                      Open AN extension file.
      SAVSUP = MSGSUP
      IVER = SUBA
      IERR = 0
      CALL ANTINI ('READ', BUFFER, DISK, CNO, IVER, CATBLK, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Read reference antenna NO.
      MSGSUP = 32000
      NUMKEY = 1
      LOCS = 1
      KEYTYP = 4
      CALL TABKEY ('READ', REFLAB, NUMKEY, BUFFER, LOCS, IREF, KEYTYP,
     *   IERR)
      IF (IERR.GT.20) THEN
C                                       Keyword not found
         IREF = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001) IERR, REFLAB
         GO TO 990
         END IF
C                                       Read IF phase differences
      KEYTYP = 2
      KEYLAB = PDLAB
      DO 100 LOOP = 1,NOIF
         WRITE(KEYLAB(7:8),2000) LOOP
         CALL TABKEY ('READ', KEYLAB, NUMKEY, BUFFER, LOCS, KEYVAR,
     *      KEYTYP, IERR)
C                                       Keyword not found
         IF (IERR.GT.20) THEN
            PD(LOOP) = 0.0
            IERR = 0
         ELSE IF (IERR.EQ.0) THEN
            IF (KEYTYP.EQ.1) THEN
               PD(LOOP) = KEYVAD
            ELSE
               PD(LOOP) = KEYVAR(1)
               END IF
         ELSE
            WRITE (MSGTXT,1001) IERR, KEYLAB
            GO TO 990
            END IF
 100     CONTINUE
      MSGSUP = SAVSUP
C                                      Close AN extension file
      CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1002) IERR
C                                       Error
 990  CALL MSGWRT (8)
      MSGSUP = SAVSUP
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PDRGET: ERROR',I3,' OPEN-FOR-WRITE AN FILE')
 1001 FORMAT ('PDRGET: ERROR',I3,' WRITING KEYWORD ', A)
 2000 FORMAT (I2.2)
 1002 FORMAT ('PDRGET: ERROR',I3,' CLOSING AN FILE')
      END
