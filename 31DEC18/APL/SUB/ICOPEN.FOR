      SUBROUTINE ICOPEN (IPLANE, LUN, IND, IERR)
C-----------------------------------------------------------------------
C! opens image catalog for the specified image plane (call from Y only)
C# Catalog
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
C   Open the image catalog file appropriate to TV plane IPLANE:
C   This routine should only be called by YC.... routines since the
C   image catalog may not be on the local computer (as assumed by this
C   routine).  ONLY FOR TVs - no longer for TK devices.
C   Inputs:
C      IPLANE   I     Image plane # = 1 to NGRAY + NGRAPH + NTKDEV
C      LUN      I     Logical unit no. to use for file
C   Outputs:
C      IND      I     Pointer to FTAB for this file
C      IERR     I     Error return:  0 => o.k.
C                         10 => access not allowed for this POPS #
C                         11 => not for TK devices
C                         > 0 => error return from ZOPEN
C-----------------------------------------------------------------------
      INTEGER   IPLANE, LUN, IND, IERR
C
      CHARACTER INAME*48
      INTEGER   NC
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Is access allowed?
      IERR = 10
      NC = NGRAY + NGRAPH
      IF ((NTVDEV.LE.0) .AND. (IPLANE.LE.NC)) GO TO 999
      IF ((NTKDEV.LE.0) .AND. (IPLANE.GT.NC)) GO TO 999
C                                       Open TV image catalog
      IF (IPLANE.LE.NC) THEN
         NC = NTVDEV
         CALL ZPHFIL ('IC', 1, 0, NC, INAME, IERR)
         CALL ZOPEN (LUN, IND, 1, INAME, F, T, T, IERR)
      ELSE
         IERR = 11
         END IF
C
 999  RETURN
      END
