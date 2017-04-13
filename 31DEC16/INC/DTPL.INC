C                                                          Include DTPL.
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
C                                       Include for TAPLT.
      REAL      USERID, NAMEIN(3), CLAIN(2), XSIN, XDISIN, INEXT, XVER,
     *   BCOUNT, ECOUNT, XINC, DOHIST, APARM(10), BPARM(10), CPARM(10)
      INTEGER   SBUF(1024), PBUF(256), TABUF(2048), NBUF, NKEY, NCOL,
     *   NREC
      DOUBLE PRECISION
     *      RSUM(2), DEG2R
      REAL      XYSCL(2), XYOFF(2), SUM(400), SUM2(400)
      INTEGER   IBCNT, IECNT, INC, BINCNT(400)
      INTEGER   SEQIN, DISKIN, TALUN, TYPEAX(2,2), EVER, PVER,
     *   TESTEM(2), CNO, NPARM, DTYPAX(2,2), ISUBAX(2,2), TAIND,
     *   OPTYPE(2)
      LOGICAL   SCALEM(2), SUMEM(2), TWOOF(2), ABSEM(2,2)
C                                       Include for TAPLT.
      COMMON /INPARM/ USERID, NAMEIN, CLAIN, XSIN, XDISIN, INEXT, XVER,
     *   BCOUNT, ECOUNT, XINC, DOHIST, APARM, BPARM, CPARM
      COMMON /BUFRS/ SBUF, PBUF, TABUF, NBUF, NKEY, NCOL, NREC
      COMMON /UVPCOM/ RSUM, DEG2R,    XYSCL, XYOFF, SUM, SUM2,
     *   IBCNT, IECNT, INC, BINCNT,   SCALEM, SUMEM, TWOOF, ABSEM,
     *   SEQIN, DISKIN, TALUN, TYPEAX, EVER, PVER, TESTEM, CNO, NPARM,
     *   DTYPAX, ISUBAX, TAIND, OPTYPE
C                                                          End DTPL.
