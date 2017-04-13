      SUBROUTINE UVSIZE (LREC, NVIS, SIZE)
C-----------------------------------------------------------------------
C! returns size of a uv file
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   Computes the desired size of a uv file.
C   Inputs:
C      LREC   I   Number words of data in uv block (incl random parms)
C      NVIS   I   Number of visibilities
C   Output:
C      SIZE   I   Size of file in AIPS 256-word blocks
C-----------------------------------------------------------------------
      INTEGER   LREC, NVIS, SIZE
C
      DOUBLE PRECISION DS, DN
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DS = LREC
      DN = NVIS
      SIZE = (DS * DN - 1.0D0) / 256.0D0 + 5.0D0
C
 999  RETURN
      END
