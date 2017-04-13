      SUBROUTINE ZSSSXF (OP, DAT, NBYTES, BUFFER, NSBYTE, IERR)
C-----------------------------------------------------------------------
C! write/read to/from the SUN-Screen-Server (TV) device
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2011
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
C   ZSSSXF sends and receives data for the SunScreen Server via the
C   routine ZSSSX2.C.  The Fortran is to attach to the fortran common
C   FTAB and convert between local integer and 16-bit integer.
C   Inputs:
C      OP       I      XAS operation code number (see SSS.TXT)
C      DAT      I(4)   Basic data set
C      NBYTES   I      Number additional 8-bit bytes in BUFFER to send
C                      to XAS
C   In/out:
C      BUFFER   I(*)   Data buffer (n.b. treated as byte stream here -
C                      the data must be in FITS standard forms)
C   Output:
C      NSBYTE   I      Number 8-bit bytes of data returned from SSS in
C                      BUFFER
C      IERR     I      Error code: 0 => okay
C-----------------------------------------------------------------------
      INTEGER   OP, OPSWAP, DAT(4), NBYTES, BUFFER(*), NSBYTE, IERR
C
      INTEGER   DAT2(4)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (TVBROK.EQ.0) THEN
         CALL ZILI16 (4, DAT, 1, DAT2)
         CALL ZILI16 (1, OP, 1, OPSWAP)
         CALL ZSSSX2 (FTAB(TVIND+NMOFF), OP, OPSWAP, DAT2, NBYTES,
     *      BUFFER, NSBYTE, IERR)
         IF (IERR.EQ.333) TVBROK = IERR
         IF (IERR.EQ.444) TVBROK = IERR
C                                       TV connection NOT open
      ELSE IF (TVBROK.LT.0) THEN
         MSGTXT = 'TV CONNECTION not open'
C        CALL MSGWRT (6)
C                                       TV connection already broken
      ELSE
         MSGTXT = 'TV CONNECTION PREVIOUSLY BROKEN'
C        CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
