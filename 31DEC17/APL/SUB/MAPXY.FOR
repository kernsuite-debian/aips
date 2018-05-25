      SUBROUTINE MAPXY (LUN, WIN, ERROR)
C-----------------------------------------------------------------------
C! sets WaWa windows for a window in the top plane of an image
C# IO-WaWa
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
C   WaWa IO system: Set windows so that MAPIO returns a subrectangle
C   of the top plane of a map
C   Inputs:
C      LUN    I      Logical Unit No. of an open map
C      WIN    R(4)   Corners of rectangle.  If WIN(1)=0.0, whole top
C                    plane is taken.
C   Output:
C      ERROR  I      As returned by MAPWIN
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      INTEGER   LUN, ERROR
      REAL      WIN(4)
C
      INTEGER   I
      REAL      BLC(7), TRC(7)
C-----------------------------------------------------------------------
C                                       Set BLC, TRC default
      DO 10 I = 1,7
         BLC(I) = 1.0
         TRC(I) = 1.0
 10      CONTINUE
      TRC(1) = 24000.
      TRC(2) = 24000.
C                                       Set subrectangle top plane
      IF (WIN(1).LE.0.5) GO TO 20
         BLC(1) = WIN(1)
         BLC(2) = WIN(2)
         TRC(1) = WIN(3)
         TRC(2) = WIN(4)
C                                       Set IO parms,...
 20   CALL MAPWIN (LUN, BLC, TRC, ERROR)
C
 999  RETURN
      END
