C                                       Begin DOFV.INC
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2008
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
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       Include for OF table pointers
      INTEGER MAXOFC, NKEYOF, NKYOF2, NKYWOF, NKYWO2, MXSPOF, MXDPOF,
     *        OFREV
C                                       # cols in OF table
      PARAMETER (MAXOFC = 8)
C                                       # keywords in OF table
      PARAMETER (NKEYOF = 6)
C                                       2 x # keywords
      PARAMETER (NKYOF2 = NKEYOF * 2)
C                                       word length of keyword array
      PARAMETER (NKYWOF = 9)
C                                       half word length of keyw array
      PARAMETER (NKYWO2 = NKYWOF / 2 + 1)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPOF = 6 + 2*MAXIF)
C                                       in double precision words
      PARAMETER (MXDPOF = MXSPOF / 2)
C                                       Current table revision no.
      PARAMETER (OFREV = 2)
C                                       3rd character gives data type:
C                                       D=double, R=real, I=integer,
C                                       L=logical
      INTEGER   OFRTIM, OFISID, OFIANT, OFISUB, OFIFQI, OFIANF, OFIST1,
     *   OFIST2
C                                       OFRTIM = Time
      PARAMETER (OFRTIM = 1)
C                                       OFISID = Source ID.
      PARAMETER (OFISID = OFRTIM+1)
C                                       OFIANT = Antenna number
      PARAMETER (OFIANT = OFISID+1)
C                                       OFISUB = Subarray number
      PARAMETER (OFISUB = OFIANT+1)
C                                       OFIFQI = Freq. ID.
      PARAMETER (OFIFQI = OFISUB+1)
C                                       OFLANT = antenna based flag
      PARAMETER (OFIANF = OFIFQI+1)
C                                       OFLST1 = Status level pol 1
      PARAMETER (OFIST1 = OFIANF+1)
C                                       OFLST1 = Status level pol 2
      PARAMETER (OFIST2 = OFIST1+1)
C                                       End DOFV.INC
