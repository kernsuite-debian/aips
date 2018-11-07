      SUBROUTINE ANTOUT (DISK, CNO, SUBA, ANTENS, CATBLK, BUFFER, IERR)
C-----------------------------------------------------------------------
C! If ANTENS not set, find antennas that are out and sets ANTENS
C# EXT-appl UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   If the user has not set ANTENS then this routine will find those
C   antennas that are called "OUT" or have 0,0,0 for their location
C   and will fill ANTENS with a list of them (with a minus sign)
C   Inputs:
C      DISK     I        Disk number
C      CNO      I        Catalog number
C      SUBA     I        Subarray
C      CATBLK   I(256)   Catalog header
C   In/out:
C      ANTENS   I(50)    Antenna numbers
C      BUFFER   I(512)   Table buffer
C   Output:
C      IERR     I        Error code - but probably should not quit
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, SUBA, ANTENS(50), CATBLK(*), BUFFER(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INTEGER   I, J
C-----------------------------------------------------------------------
C                                       is ANTENS set?
      IERR = 0
      DO 10 I = 1,50
         IF (ANTENS(I).NE.0) GO TO 999
 10      CONTINUE
C                                       fill antenna info
      CALL GETANT (DISK, CNO, SUBA, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING ANTENNA INFO FOR OUTS'
         GO TO 990
         END IF
C                                       look for bad ones
      J = 0
      DO 30 I = 1,NSTNS
         IF (STNNAM(I).EQ.'OUT') THEN
            J = J + 1
            IF (J.LE.50) ANTENS(J) = -I
         ELSE IF ((STNX(I).EQ.0.0D0) .AND. (STNY(I).EQ.0.0D0) .AND.
     *      (STNZ(I).EQ.0.0D0)) THEN
            J = J + 1
            IF (J.LE.50) ANTENS(J) = -I
            END IF
 30      CONTINUE
      IF (J.GT.0) THEN
         WRITE (MSGTXT,1030) J, (-ANTENS(I), I = 1,MIN (8,J))
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTOUT: ERROR',I4,' ON ',A)
 1030 FORMAT ('ANTOUT found',I3,' bad antennas:',8I4)
      END
