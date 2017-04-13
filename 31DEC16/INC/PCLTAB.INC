C                                       Begin PCLTAB.INC
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
C                                       Include for CL table pointers
      INTEGER CLREV
C                                       CLREV = current CL revision number
      PARAMETER (CLREV = 10)
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer
      INTEGER   CLDTIM, CLRTMI, CLISID, CLIANT, CLISUB, CLIFQI, CLRIFR,
     *   CLDDEL, CLRDOP, CLRATM, CLRDAT,
     *   CLRMD1, CLRCK1, CLRDC1, CLRDS1, CLRDD1,
     *   CLRRE1, CLRIM1, CLRRA1, CLRDE1, CLRWE1, CLIRF1,
     *   CLRMD2, CLRCK2, CLRDC2, CLRDS2, CLRDD2,
     *   CLRRE2, CLRIM2, CLRRA2, CLRDE2, CLRWE2, CLIRF2
C                                       CLDTIM = Time
      PARAMETER (CLDTIM = 1)
C                                       CLRTMI = Time interval
      PARAMETER (CLRTMI = CLDTIM+1)
C                                       CLISID = Source ID.
      PARAMETER (CLISID = CLRTMI+1)
C                                       CLIANT = Antenna number
      PARAMETER (CLIANT = CLISID+1)
C                                       CLISUB = Subarray number
      PARAMETER (CLISUB = CLIANT+1)
C                                       CLIFQI = Freq. ID.
      PARAMETER (CLIFQI = CLISUB+1)
C                                       CLRIFR = Ions. Fara. rot.
      PARAMETER (CLRIFR = CLIFQI+1)
C                                       CLDDEL = Geo. Delay
      PARAMETER (CLDDEL = CLRIFR+1)
C                                       CLRDOP = Doppler offset.
      PARAMETER (CLRDOP = CLDDEL+1)
C                                       CLRATM = Atmos. delay
      PARAMETER (CLRATM = CLRDOP+1)
C                                       CLRDAT = Time deriv. CLRATM
      PARAMETER (CLRDAT = CLRATM+1)
C                                       CLRMD1 = Multi-band delay 1
      PARAMETER (CLRMD1 = CLRDAT+1)
C                                       CLRCK1 = Clock delay Stokes 1
      PARAMETER (CLRCK1 = CLRMD1+1)
C                                       CLRDC1 = Time deriv. CLRCK1
      PARAMETER (CLRDC1 = CLRCK1+1)
C                                       CLRDS1 = Dispersive delay 1
      PARAMETER (CLRDS1 = CLRDC1+1)
C                                       CLRDD1 = Time deriv. CLRDS1
      PARAMETER (CLRDD1 = CLRDS1+1)
C                                       CLRRE1 = Real part Stokes 1
      PARAMETER (CLRRE1 = CLRDD1+1)
C                                       CLRIM1 = Imag 1
      PARAMETER (CLRIM1 = CLRRE1+1)
C                                       CLRRA1 = Rate residual 1
      PARAMETER (CLRRA1 = CLRIM1+1)
C                                       CLRDE1 = Delay residual 1
      PARAMETER (CLRDE1 = CLRRA1+1)
C                                       CLRWE1 = Weight 1
      PARAMETER (CLRWE1 = CLRDE1+1)
C                                       CLIRF1 = Ref. antenna 1
      PARAMETER (CLIRF1 = CLRWE1+1)
C                                       CLRMD2 = Multi-band delay 2
      PARAMETER (CLRMD2 = CLIRF1+1)
C                                       CLRCK2 = Clock delay Stokes 2
      PARAMETER (CLRCK2 = CLRMD2+1)
C                                       CLRDC2 = Time deriv. CLRCK2
      PARAMETER (CLRDC2 = CLRCK2+1)
C                                       CLRDS2 = Dispersive delay 2
      PARAMETER (CLRDS2 = CLRDC2+1)
C                                       CLRDD2 = Time deriv. CLRDS2
      PARAMETER (CLRDD2 = CLRDS2+1)
C                                       CLRRE2 = Real part Stokes 2
      PARAMETER (CLRRE2 = CLRDD2+1)
C                                       CLRIM2 = Imag 2
      PARAMETER (CLRIM2 = CLRRE2+1)
C                                       CLRRA2 = Rate residual 2
      PARAMETER (CLRRA2 = CLRIM2+1)
C                                       CLRDE2 = Delay residual 2
      PARAMETER (CLRDE2 = CLRRA2+1)
C                                       CLRWE2 = Weight 2
      PARAMETER (CLRWE2 = CLRDE2+1)
C                                       CLIRF2 = Ref. antenna 2
      PARAMETER (CLIRF2 = CLRWE2+1)
C                                       End PCLTAB.INC
