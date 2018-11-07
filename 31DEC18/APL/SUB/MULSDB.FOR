      SUBROUTINE MULSDB (CATBLK, ISMULT)
C-----------------------------------------------------------------------
C! Determines if a uv file is multi- or single- source.
C# UV EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2011-2012
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
C   Determines if data set described by catlogue header CATBLK is
C   a single source, calibrated or multi-source uncalibrated data set.
C   The presence of a source table (type SU) is used to determine.
C    Input:
C       CATBLK   I(256)   Catalog header record.
C    Output:
C       ISMULT   L       .TRUE. if data set is a multi source, raw
C                              data set, else .FALSE.
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256)
      LOGICAL   ISMULT
C
      INTEGER   I, IOFF, IERR, NMSG
      CHARACTER SORC*8
      LOGICAL   ISSU, ISSORC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE NMSG
      DATA NMSG /0/
      DATA SORC /'SOURCE'/
C-----------------------------------------------------------------------
      CALL FNDEXT ('SU', CATBLK, I)
      ISSU = I.GT.0
      CALL AXEFND (8, SORC, CATBLK(KIPCN), CATBLK(KHPTP), IOFF, IERR)
      ISSORC = (IOFF.GE.0) .AND. (IERR.EQ.0)
      IF ((ISSU) .AND. (.NOT.ISSORC)) THEN
         NMSG = NMSG + 1
         IF (NMSG.LE.2) THEN
            MSGTXT = 'Source table but no source random parameter'
            CALL MSGWRT (6)
            END IF
      ELSE IF ((.NOT.ISSU) .AND. (ISSORC)) THEN
         MSGTXT = 'SOURCE RANDOM PARAMETER BUT NO SOURCE TABLE!'
         CALL MSGWRT (8)
         END IF
      ISMULT = (ISSU) .AND. (ISSORC)
C
 999  RETURN
      END
