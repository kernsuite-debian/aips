      SUBROUTINE ROTFND (CATR, ROT, IERR)
C-----------------------------------------------------------------------
C! find the coordinate rotation angle from the catalog header
C# Header
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
C   ROTFND finds the map rotation angle from a given catalog block
C   Inputs:
C       CATR(*)   R    Map catalog header
C   Outputs:
C       ROT       R    Map rotation angle (degrees)
C       IERR      I    Error code. 0=>OK, 1=>couldn't find axis.
C  Programmer: W. D. Cotton, July 1982.
C-----------------------------------------------------------------------
      REAL      CATR(256), ROT
      INTEGER   IERR
C
      INTEGER   NTRY, I, IOFF
      CHARACTER TYPES(6)*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TYPES /'DEC ','DEC-','MM  ','GLON','ELON','Y   '/
      DATA NTRY /6/
C-----------------------------------------------------------------------
C                                       Loop thru possible axis types.
      DO 10 I = 1,NTRY
         CALL AXEFND (4, TYPES(I), KICTPN, CATR(KHCTP), IOFF, IERR)
         IF (IERR.EQ.0) GO TO 20
 10      CONTINUE
C                                       Failed
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         ROT = 0.0
         GO TO 999
C                                       Found proper axis.
 20   ROT = CATR (KRCRT+IOFF)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ROTFND: CAN''T FIND PROPER AXIS TYPE FOR MAP ROTATION')
      END
