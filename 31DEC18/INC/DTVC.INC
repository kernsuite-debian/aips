C                                                          Include DTVC.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
      INTEGER   NGRAY, NGRAPH, NIMAGE, MAXXTV(2), MAXINT, LUTOUT,
     *   OFMINP, OFMOUT, SCXINC, SCYINC, MXZOOM, TYPSPL, TVALUS, TVXMOD,
     *   TVYMOD, ISUNUM, TVIMPC, TVIMGC, CSIZTV(2), TVDUMS(7),
     *   TVZOOM(3), TVSCRX(16), TVSCRY(16), TVLIMG(4), TVSPLT(2),
     *   TVSPLM, TVSPLC, TYPMOV(16), WINDTV(4), ISYNCH, TVSCGX, TVSCGY,
     *   TVDUM2, YBUFF(160)
      REAL      TVGAMA
      COMMON /TVCHAR/ NGRAY, NGRAPH, NIMAGE, MAXXTV, MAXINT, LUTOUT,
     *   OFMINP, OFMOUT, SCXINC, SCYINC, MXZOOM, TYPSPL, TVALUS, TVXMOD,
     *   TVYMOD, ISUNUM, TVIMPC, TVIMGC, CSIZTV, TVGAMA,    TVDUMS,
     *   TVZOOM, TVSCRX, TVSCRY, TVLIMG, TVSPLT, TVSPLM, TVSPLC,
     *   TYPMOV, WINDTV, ISYNCH, TVSCGX, TVSCGY, TVDUM2, YBUFF
C                                                          End DTVC.
