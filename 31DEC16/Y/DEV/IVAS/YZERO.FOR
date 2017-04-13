      SUBROUTINE YZERO (CHAN, IERR)
C-----------------------------------------------------------------------
C! fill a TV memory plane with zeros
C# TV-util
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
C   YZERO fills an TV TV memory plane with zeros the fast way.
C   Inputs:
C      CHAN   I     channel # ( 1 - NGRAY+NGRAPH), 0 => all
C   Outputs:
C      IERR   I     error code of Z...XF: 0 - ok, 2 - input error
C   Version for IIS IVAS Model
C-----------------------------------------------------------------------
      INTEGER   CHAN, IERR
C
      INTEGER   FIVASGPHSET, FIVASVPSETUP, FIVASGPHVALUE, FIVASGPHMOVE,
     *   FIVASGPHFILL
      INTEGER   XS, YS, RCS(4), GCS(4), BCS(4), LX, JERR, IC
      LOGICAL   F
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 2
      IF ((CHAN.LT.0) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      IERR = 0
C                                       Everything:
      IF (CHAN.LE.0) THEN
         CALL MOVIST ('OFFF', 0, 0, 0, 0, IERR)
C                                       graphics
         JERR = FIVASGPHSET (0, 0, 1024, 1024, 0)
         IF (JERR.NE.0) GO TO 900
C                                       12-bit
         IF (NGRAY.LE.2) THEN
            JERR = FIVASVPSETUP (0, 0, 0, 30)
            IF (JERR.NE.0) GO TO 900
            JERR = FIVASVPSETUP (1, 1, 1, 30)
            IF (JERR.NE.0) GO TO 900
C                                       8-bit
         ELSE
            JERR = FIVASVPSETUP (0, 1, 2, 30)
            END IF
         IF (JERR.NE.0) GO TO 900
C                                       restore selection
         CALL YSPLIT ('READ', XS, YS, RCS, GCS, BCS, F, IERR)
         CALL YSPLIT ('WRIT', XS, YS, RCS, GCS, BCS, F, IERR)
C                                       selected portion: grey
      ELSE IF (CHAN.LE.NGRAY) THEN
         IC = 2 ** (CHAN-1)
         CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
         LX = CHAN - 1
         JERR = FIVASVPSETUP (LX, LX, LX, 30)
         IF (JERR.NE.0) GO TO 900
C                                       restore selection
         CALL YSPLIT ('READ', XS, YS, RCS, GCS, BCS, F, IERR)
         CALL YSPLIT ('WRIT', XS, YS, RCS, GCS, BCS, F, IERR)
C                                       Graphics plane
      ELSE
         LX = 15 - 2 ** (CHAN-NGRAY-1)
         JERR = FIVASGPHVALUE (LX, LX)
         IF (JERR.NE.0) GO TO 900
         JERR = FIVASGPHMOVE (0, 0, 0)
         IF (JERR.NE.0) GO TO 900
         JERR = FIVASGPHFILL (1024, 1024, 2)
         IF (JERR.NE.0) GO TO 900
         END IF
      GO TO 999
C                                       Error
 900  CALL YDOERR ('YZERO', JERR, IERR)
C
 999  RETURN
      END
