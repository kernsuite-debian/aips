C                                                          Include DCOM.
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
      INTEGER   HEAD2(256), HDR2(256)
      REAL      HEAD4(128), HDR4(128)
      DOUBLE PRECISION
     *      HEAD8(64),  HDR8(64)
      DOUBLE PRECISION
     *      SCALE1, SCALE2, OFFS1, OFFS2
      REAL      RBUFF(4096,2), FREQ1, FREQ2, LNFREQ, FBLANK, FMAX1,
     *   FMAX2, CPARM(20), ISNAME(3,2), ISCLAS(2,2), IDNAME(3),
     *   IDCLAS(2), XMIN, XMAX, FB1, FB2
      LOGICAL   BIN(2), XBLFLG, DOTEST, BFLAG
      INTEGER   IBLC(7), ITRC(7), NPI(7,2), NPO(7),
     *   JBLC(7), JTRC(7), FBL(2), ISLUN(2), ISIND(2), IDLUN, IDIND,
     *   ISSEQ(2), IDSEQ, NAX, NRDIM(2), CTYPE, IUSER
      COMMON /MAPHDR/ HEAD2, HDR2
      COMMON /COMBCM/ SCALE1, SCALE2, OFFS1, OFFS2,
     *   RBUFF, FREQ1, FREQ2, LNFREQ, FBLANK, FMAX1, FMAX2, CPARM,
     *   ISNAME, ISCLAS, IDNAME, IDCLAS, XMIN, XMAX, FB1, FB2,
     *   BIN, XBLFLG, DOTEST, BFLAG,
     *   FBL, IBLC, ITRC, JBLC, JTRC, NPI, NPO, ISLUN, ISIND, IDLUN,
     *   IDIND, ISSEQ, IDSEQ, NAX, NRDIM, CTYPE, IUSER
      EQUIVALENCE (HEAD2, HEAD4, HEAD8),  (HDR2, HDR4, HDR8)
C                                                          End DCOM.
