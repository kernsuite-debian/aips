      SUBROUTINE SNRVAL (NPIX, SUBVAL, INB)
C-----------------------------------------------------------------------
C! substitutes specified value for magic blank value in a buffer
C# Map-util
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
C   SNRVAL substitutes a desired value for blanked pixels in a
C   floating point buffer.  If the substitution value is the magic
C   blanked pixel value, then SNRVAL returns very quickly.
C   Inputs:
C      NPIX    I          Number of pixels in buffer
C      SUBVAL  R          Value to substitute for blanked pixels
C   In/out:
C      INB     R(NPIX)    Buffer
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      SUBVAL, INB(*)
C
      INTEGER   I
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Quick returns
      IF ((NPIX.LE.0) .OR. (SUBVAL.EQ.FBLANK)) GO TO 999
C                                       Do it
         DO 10 I = 1,NPIX
            IF (INB(I).EQ.FBLANK) INB(I) = SUBVAL
 10         CONTINUE
C
 999  RETURN
      END
