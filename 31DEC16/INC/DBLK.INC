C                                                          Include DBLK.
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
     *   CLA2IN(2), XSEQ2N, XDIS2I, NAMOUT(3), CLAOUT(2), XSEQO,
     *   XDISKO, BLC(7), TRC(7), BCHAN, ECHAN, DOCAT, DOALIN, OPCODE,
     *   DPARM(10), DOINVR, TVCHAN, RANGE(2), TVFUNC, TXINC, TYINC
      DOUBLE PRECISION
     *      CAT8(64), OLD8(64), NEW8(64), CLP8(64)
      REAL      CAT4(128), OLD4(128), NEW4(128), CLP4(128), FBLANK
      INTEGER   CATBLK(256), BUFF1(4096), BUFF2(4096), CATOLD(256),
     *   CATNEW(256), CATCLP(256), SEQIN, SEQ2IN, SEQOUT, DISKIN,
     *   DIS2IN, DISKO, NEWCNO, NE2CNO, OLDCNO, JBUFSZ, ICODE, NAXSKP,
     *   BBLC(7), BTRC(7), NX2LIM(7), LUNI1, LUNI2, LUNO1, LUNO2, INDI1,
     *   INDI2, INDO1, INDO2, TTYLUN, TTYIND
      COMMON /INPARM/ NAMEIN, CLAIN, XSEQIN, XDISKI, NAM2IN, CLA2IN,
     *   XSEQ2N, XDIS2I, NAMOUT, CLAOUT, XSEQO, XDISKO, BLC, TRC,
     *   BCHAN, ECHAN, DOCAT, DOALIN, OPCODE, DPARM, DOINVR, TVCHAN,
     *   RANGE, TVFUNC, TXINC, TYINC
      COMMON /PARMS/ CATOLD, CATNEW, CATCLP, FBLANK, SEQIN, SEQ2IN,
     *   SEQOUT, DISKIN, DIS2IN, DISKO, NEWCNO, NE2CNO, OLDCNO, JBUFSZ,
     *   ICODE, NAXSKP, BBLC, BTRC, NX2LIM, LUNI1, LUNI2, LUNO1, LUNO2,
     *   INDI1, INDI2, INDO1, INDO2, TTYLUN, TTYIND
      COMMON /IMBUF/ BUFF1, BUFF2
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CAT4, CAT8)
      EQUIVALENCE (CATNEW, NEW4, NEW8)
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      EQUIVALENCE (CATCLP, CLP4, CLP8)
C                                                          End DBLK.
