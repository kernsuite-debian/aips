      SUBROUTINE R2TX (NTHPO, CR0, CR1, CI0, CI1)
C-----------------------------------------------------------------------
C! VMS Pseudo AP routine: Radix 2 FFT butterfly
C# AP-FFT
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
C   Radix 2 Butterfly called by FFT842
C-----------------------------------------------------------------------
      REAL   CR0(*), CR1(*), CI0(*), CI1(*), R1, FI1
      INTEGER   NTHPO, K
C-----------------------------------------------------------------------
      DO 10 K = 1,NTHPO,4
         R1 = CR0(K) + CR1(K)
         CR1(K) = CR0(K) - CR1(K)
         CR0(K) = R1
         FI1 = CI0(K) + CI1(K)
         CI1(K) = CI0(K) - CI1(K)
         CI0(K) = FI1
 10      CONTINUE
 999  RETURN
      END
