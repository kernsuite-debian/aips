C                                                          Include DKAL.
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
      INTEGER   NSCANS, LINT(200), ILOCV1, ILOCV2, ILOCV3, ILOCV4,
     *   NINT, GVOL, GCNO, GVER, NGREC, GBUF(1024), GREC(768),
     *   GCBLK(256), INITED, CATBLK(256), JBUFSZ, ISCRAT,
     *   CNO1, CNO2, CNO3
      REAL   USERID, NAME1(3), NAME2(3), NAME3(3), CLASS1(2),
     *   CLASS2(2), CLASS3(2), SPARE(5), SEQ1, SEQ2, SEQ3, DISK1,
     *   DISK2, DISK3, CVER, XNITER, BLMIN, BLMAX, EPS, XREF,
     *   XMINNO, XPR, XMODE, AVAC, XLEN, DOMOD, POINT, FLUX, DELTAX,
     *   DELTAY, PLOT, DOCAT, BADD(10), XVER,
     *   RBUF(2500), TAU, SCANL(200), TFIRST, TLAST, GM
      DOUBLE PRECISION
     *   GS, CNTA, S1, S2, SN
      COMMON/PRM/USERID,NAME1,CLASS1,SEQ1,DISK1,NAME2,CLASS2,
     *   SEQ2,DISK2,XVER,NAME3,CLASS3,SEQ3,DISK3,XNITER,
     *   BLMIN,BLMAX,EPS,XREF,XMINNO,XPR,XMODE,
     *   AVAC,XLEN,DOMOD,POINT,FLUX,DELTAX,DELTAY,PLOT,SPARE,
     *   DOCAT,BADD
      COMMON/BUFRS/RBUF
      COMMON/SCLIST/TAU,SCANL,NSCANS,LINT,JBUFSZ,ISCRAT,CNO1,
     *   CNO2,CNO3
      COMMON/FILINF/ILOCV1,ILOCV2,ILOCV3,ILOCV4,NINT
      COMMON/GFIL/TFIRST,TLAST,GS,CNTA,GM,GVOL,GCNO,GVER,NGREC,
     *   GBUF, GREC, GCBLK
      COMMON/GED/S1,S2,SN,INITED
      COMMON/MAPHDR/CATBLK
C                                                          End DKAL.
