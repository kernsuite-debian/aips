@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QGSP (I, NREG)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Read "S-pad" register
C# AP-util
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
C   Simulates the array processor subroutine which reads the
C   contents of a scratch-pad register.  Given an register
C   number, NREG, the register contents are  returned in I.
C    Input:
C       NREG  I  Spad register number
C    Output:
C       I     I  Contents of SPAD register NREG.
C    NOTE: this function will eventually be eliminated.
C-----------------------------------------------------------------------
      INTEGER   I, NREG
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      I = SPAD(NREG)
C
 999  RETURN
      END
