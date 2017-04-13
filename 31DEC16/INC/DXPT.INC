C                                                          Include DXPT.
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
C                                       Local include for XPORT
C                                       Common /XPCOM/
      INTEGER   INLUN, ININD, INCNO, INDISK, IBLC(7), ITRC(7),
     *   IAXINC(7),  ISAMP, ICODE, WBUFF(512)
      REAL      SAMPTP, PIXRNG(2), PLEV, CLEV, CODETP
C                                       Common /MAPHDR/
      INTEGER   CATBLK(256)
      INTEGER   CAT3(128)
      REAL      CAT4(128)
      DOUBLE PRECISION
     *      CAT8(64)
C                                       Local include for XPORT
      COMMON /XPTCOM/ PIXRNG, PLEV, CLEV, SAMPTP, CODETP, INLUN,
     *   ININD, INCNO, INDISK, IBLC, ITRC, IAXINC, ISAMP, ICODE,
     *   WBUFF
      COMMON /MAPHDR/ CATBLK
C                                       Local include for XPORT
      EQUIVALENCE (CATBLK, CAT3, CAT4, CAT8)
C                                                          End DXPT.
