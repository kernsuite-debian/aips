C                                       Include for Calc (CT) table
C----------------------------------------------------------------------
C;  Copyright (C) 2005
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
C----------------------------------------------------------------------
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       WX definitions
      INTEGER   MAXCTC, NKEYCT, NKYWCT, MXSPCT, MXDPCT, ICTREV
C                                       # cols in CT table
      PARAMETER (MAXCTC = 12)
C                                       # keywords in CT table
      PARAMETER (NKEYCT = 32)
C                                       word length of keyword array
      PARAMETER (NKYWCT = 57)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPCT = 26)
C                                       in double precision words
      PARAMETER (MXDPCT = (MXSPCT-1) / 2 + 1)
C                                       Current table revision no.
      PARAMETER (ICTREV = 3)
C                                       Keywords
      CHARACTER CTOBSC*8, CTDATE*8, CTSRV*8, CTCVER*8, CTAVER*8,
     *   CTIVER*8, CTEVER*8
      INTEGER   CTNSTK, CTSTK1, CTNIF, CTNCHN, CTTABV, CTEPO, CTTID1
      DOUBLE PRECISION CTRFRQ, CTCBW, CTRPIX, CTACCG, CTEFLA, CTERAD,
     *   CTMMSE, CTETID, CTGAUS, CTGMMO, CTGMDU, CTLOVH, CTLOVL, CTPDAT,
     *   CTRDAT, CTTSEC, CTGRVC, CTLIGH
C                                       Keyword commons
      COMMON /CTCHAR/ CTOBSC, CTDATE, CTSRV, CTCVER, CTAVER, CTIVER,
     *   CTEVER
      COMMON /CTKEYS/ CTRFRQ, CTCBW, CTRPIX, CTACCG, CTEFLA, CTERAD,
     *   CTMMSE, CTETID, CTGAUS, CTGMMO, CTGMDU, CTLOVH, CTLOVL, CTPDAT,
     *   CTRDAT, CTTSEC, CTGRVC, CTLIGH, CTNSTK, CTSTK1, CTNIF, CTNCHN,
     *   CTTABV, CTEPO, CTTID1
C                                       End include DCTV.INC
