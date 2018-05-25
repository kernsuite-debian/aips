C                                       Begin PTYTAB.INC
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
C                                       Include for TY table pointers
      INTEGER TYREV
C                                       TYREV = current TY revision number
      PARAMETER (TYREV = 10)
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer
      INTEGER   TYRTIM, TYRTMI, TYISID, TYIANT, TYISUB, TYIFQI, 
     *   TYRTS1, TYRTA1, TYRTS2, TYRTA2
C                                       TYRTIM = Time
      PARAMETER (TYRTIM = 1)
C                                       TYRTMI = Time interval
      PARAMETER (TYRTMI = TYRTIM+1)
C                                       TYISID = Source ID.
      PARAMETER (TYISID = TYRTMI+1)
C                                       TYIANT = Antenna number
      PARAMETER (TYIANT = TYISID+1)
C                                       TYISUB = Subarray number
      PARAMETER (TYISUB = TYIANT+1)
C                                       TYIFQI = Freq. ID.
      PARAMETER (TYIFQI = TYISUB+1)
C                                       TYRTS1 = Tsys 1
      PARAMETER (TYRTS1 = TYIFQI+1)
C                                       TYRTA1 = Tant 1
      PARAMETER (TYRTA1 = TYRTS1+1)
C                                       TYRTS2 = Tsys 2
      PARAMETER (TYRTS2 = TYRTA1+1)
C                                       TYRTA2 = Tant 2
      PARAMETER (TYRTA2 = TYRTS2+1)
C                                       End PTYTAB.INC
