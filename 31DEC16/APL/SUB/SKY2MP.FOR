      SUBROUTINE SKY2MP (X, PIX, IERR)
C-----------------------------------------------------------------------
C! calls SETLOC, XYPIX to convert sky coordinates to map pixel locations
C# Coordinates
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
C   SKY2MP converts sky coordinates to map pixel locations using
C   subroutine XYPIX.  This routine just sets up the commons.
C   Input:
C      X     D(2)     x,y coordinate values
C   Outputs:
C      PIX   R(2)     x,y pixel values
C      IERR  I        0 ok, 3 undefined
C   Commons:
C      /MAPHDR/   input: standard catalog block: use IIDEP of image
C      /LOCATI/   output: values needed by XYPIX: built here via SETLOC
C-----------------------------------------------------------------------
      DOUBLE PRECISION X(2)
      REAL      PIX(2)
      INTEGER   IERR
C
      INTEGER   S2H(256)
      LOGICAL   SWAP
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ S2H
      DATA SWAP /.TRUE./
C-----------------------------------------------------------------------
C                                       set common values
      CALL SETLOC (S2H(IIDEP), SWAP)
C                                       do conversion
      CALL XYPIX (X(1), X(2), PIX(1), PIX(2), IERR)
C
 999  RETURN
      END
