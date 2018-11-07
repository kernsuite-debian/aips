C                                                          Include DBLS.
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
      REAL      NAMEIN(3), CLAIN(2), XSEQIN, XDISKI, NAM2IN(3),
     *   CLA2IN(2), XSEQ2N, XDIS2I, BLC(7), TRC(7), BCHAN, DOALIN,
     *   DOINVR, PRTLEV, DOCRT, TVCHAN, RANGE(2), TVFUNC, TXINC, TYINC,
     *   BADD(10)
      DOUBLE PRECISION
     *      OLD8(64), CLP8(64)
      REAL      OLD4(128), CLP4(128)
      INTEGER   OLD3(128), CLP3(128)
      INTEGER   BUFF1(4096), BUFF2(4096), CATOLD(256), CATCLP(256),
     *   SEQIN, SEQ2IN, DISKIN, DIS2IN, NEWCNO, OLDCNO, JBUFSZ, NAXSKP,
     *   BBLC(7), BTRC(7), LUNI1, LUNI2, LUNO, INDI1, INDI2, INDO,
     *   TTYLUN, TTYIND, XCHRN(7), PRTLUN, PRTIND
      COMMON /INPARM/ NAMEIN, CLAIN, XSEQIN, XDISKI, NAM2IN, CLA2IN,
     *   XSEQ2N, XDIS2I, BLC, TRC, BCHAN, DOALIN, DOINVR, PRTLEV,
     *   DOCRT, TVCHAN, RANGE, TVFUNC, TXINC, TYINC, BADD
      COMMON /PARMS/ CATOLD, SEQIN, SEQ2IN, DISKIN, DIS2IN, NEWCNO,
     *   OLDCNO, JBUFSZ, NAXSKP, BBLC, BTRC, LUNI1, LUNI2, LUNO, INDI1,
     *   INDI2, INDO, TTYLUN, TTYIND, XCHRN, PRTLUN, PRTIND
      COMMON /IMBUF/ BUFF1, BUFF2
      COMMON /MAPHDR/ CATCLP
      EQUIVALENCE (CATOLD, OLD3, OLD4, OLD8)
      EQUIVALENCE (CATCLP, CLP3, CLP4, CLP8)
C                                                          End DBLS.
