C                                                          Include DXYG.
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
C                                       local variables for GEOM family
      REAL      XPARM(10), ZMX, ZMN, BLC1, BLC2, TRC1, TRC2,
     *   AA(3,3), AAI(3,3)
      INTEGER   BLNCNT(2)
      INTEGER   MORD, NPXI, NRWI, NPLANE, NPXO, NRWO, NPZI, IBLC1,
     *   IBLC2, ITRC1, ITRC2, ICALL, NLUN, ERROR
      LOGICAL   INVERT, AXONLY, ZEROFL, LBLANK
C                                       local variables for GEOM family
      COMMON /CXYG/ XPARM, ZMX,  ZMN, BLC1, BLC2, TRC1, TRC2, AA, AAI,
     *   BLNCNT, INVERT, AXONLY, ZEROFL, LBLANK, MORD, NPXI, NRWI,
     *   NPLANE, NPXO, NRWO, NPZI, IBLC1, IBLC2, ITRC1, ITRC2, ICALL,
     *   NLUN, ERROR
C                                                          End DXYG.
