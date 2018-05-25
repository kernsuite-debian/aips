C                                                          Include DTFL.
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
C                                       Include for TAFLG.
      REAL      USERID, NAMEIN(3), CLAIN(2), XSIN, XDISIN, INEXT, XIVER,
     *   XOVER, BCOUNT, ECOUNT, XOPTYP, APARM(10), BPARM(10), CPARM(10)
      INTEGER   SBUF(1024), PBUF(256), TABUF(2048), NBUF, NKEY, NCOL,
     *   NREC
      DOUBLE PRECISION
     *      RSUM(2), DEG2R
      INTEGER   IBCNT, IECNT, IMCNT, NCOUNT
      REAL      OPFILE(6), TEXT(256)
      INTEGER   SEQIN, DISKIN, TALUN, IOP, TYPEAX(2,2), IVER, OVER,
     *   CNO, NPARM, DTYPAX(2,2), ISUBAX(2,2), TAIND, OPTYPE(2)
      LOGICAL   SUMEM(2), TWOOF(2), ABSEM(2,2)
C                                       Include for TAFLG.
      COMMON /INPARM/ USERID, NAMEIN, CLAIN, XSIN, XDISIN, INEXT, XIVER,
     *   XOVER, BCOUNT, ECOUNT, XOPTYP, APARM, BPARM, CPARM
      COMMON /BUFRS/ SBUF, PBUF, TABUF, NBUF, NKEY, NCOL, NREC
      COMMON /UVPCOM/ RSUM, DEG2R,  OPFILE, TEXT,  IBCNT, IECNT, IMCNT,
     *   NCOUNT, SUMEM, TWOOF, ABSEM,
     *   SEQIN, DISKIN, TALUN, IOP, TYPEAX, IVER, OVER, CNO, NPARM,
     *   DTYPAX, ISUBAX, TAIND, OPTYPE
C                                                          End DTFL.
