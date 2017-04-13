      SUBROUTINE MP2SKY (PIX, X, IERR)
C-----------------------------------------------------------------------
C! calls SETLOC, XYVAL to convert image pixel to physical coordinates
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
C   MP2SKY converts map pixel locations to sky coordinates using
C   subroutine XYVAL.  This routine just sets up the commons.
C   Inputs:
C      PIX     R(2)     x,y pixel values
C   Output:
C      X       D(3)     x,y (& possibly z) coordinate values
C      IERR    I        0 ok, 1 out of range, 2 bad type, 3 undefined
C   Commons:
C      /MAPHDR/   standard catalog block: uses IIDEP so this routine
C                 is mostly useful with things from Image catalog
C      /LOCATI/   values needed by XYVAL: built here via SETLOC
C-----------------------------------------------------------------------
      REAL      PIX(2)
      DOUBLE PRECISION X(3)
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
      CALL XYVAL (PIX(1), PIX(2), X(1), X(2), X(3), IERR)
C
 999  RETURN
      END
