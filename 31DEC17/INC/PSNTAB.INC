C                                       Begin PSNTAB.INC
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C                                       Include for SN table pointers
C                                       Include for CL table pointers
      INTEGER SNREV
C                                       SNREV = current SN revision number
      PARAMETER (SNREV = 11)
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer
      INTEGER   SNDTIM, SNRTMI, SNISID, SNIANT, SNISUB, SNIFQI, SNRIFR,
     *   SNINOD, SNRMD1, SNRDS1, SNRDD1, SNRRE1, SNRIM1, SNRRA1, SNRDE1,
     *   SNRWE1, SNIRF1, SNRMD2, SNRDS2, SNRDD2, SNRRE2, SNRIM2, SNRRA2,
     *   SNRDE2, SNRWE2, SNIRF2
C                                       SNDTIM = Time
      PARAMETER (SNDTIM = 1)
C                                       SNRTMI = Time interval
      PARAMETER (SNRTMI = SNDTIM+1)
C                                       SNISID = Source ID.
      PARAMETER (SNISID = SNRTMI+1)
C                                       SNIANT = Antenna number
      PARAMETER (SNIANT = SNISID+1)
C                                       SNISUB = Subarray number
      PARAMETER (SNISUB = SNIANT+1)
C                                       SNIFQI = Freq. ID.
      PARAMETER (SNIFQI = SNISUB+1)
C                                       SNRIFR = Ions. Fara. rot.
      PARAMETER (SNRIFR = SNIFQI+1)
C                                       SNINOD = Node number.
      PARAMETER (SNINOD = SNRIFR+1)
C                                       SNRMD1 = Multi-band delay 1
      PARAMETER (SNRMD1 = SNINOD+1)
C                                       SNRDS1 = Dispersive delay 1
      PARAMETER (SNRDS1 = SNRMD1+1)
C                                       SNRDD1 = Time deriv. SNRDS1
      PARAMETER (SNRDD1 = SNRDS1+1)
C                                       SNRRE1 = Real part Stokes 1
      PARAMETER (SNRRE1 = SNRDD1+1)
C                                       SNRIM1 = Imag 1
      PARAMETER (SNRIM1 = SNRRE1+1)
C                                       SNRDE1 = Delay residual 1
      PARAMETER (SNRDE1 = SNRIM1+1)
C                                       SNRRA1 = Rate residual 1
      PARAMETER (SNRRA1 = SNRDE1+1)
C                                       SNRWE1 = Weight 1
      PARAMETER (SNRWE1 = SNRRA1+1)
C                                       SNIRF1 = Ref. antenna 1
      PARAMETER (SNIRF1 = SNRWE1+1)
C                                       SNRMD2 = Multi-band delay 2
      PARAMETER (SNRMD2 = SNIRF1+1)
C                                       SNRDS2 = Dispersive delay 1
      PARAMETER (SNRDS2 = SNRMD2+1)
C                                       SNRDD2 = Time deriv. SNRDS2
      PARAMETER (SNRDD2 = SNRDS2+1)
C                                       SNRRE2 = Real part Stokes 1
      PARAMETER (SNRRE2 = SNRDD2+1)
C                                       SNRIM2 = Imag 2
      PARAMETER (SNRIM2 = SNRRE2+1)
C                                       SNRDE2 = Delay residual 2
      PARAMETER (SNRDE2 = SNRIM2+1)
C                                       SNRRA2 = Rate residual 2
      PARAMETER (SNRRA2 = SNRDE2+1)
C                                       SNRWE2 = Weight 2
      PARAMETER (SNRWE2 = SNRRA2+1)
C                                       SNIRF2 = Ref. antenna 2
      PARAMETER (SNIRF2 = SNRWE2+1)
C                                       End PSNTAB.INC
