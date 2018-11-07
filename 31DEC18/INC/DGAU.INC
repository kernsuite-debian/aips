C                                                          Include DGAU.
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
      REAL      NAMEIN(3), CLAIN(2), XSEQIN, XDISKI, NAMOUT(3),
     *   CLAOUT(2), XSEQO, XDISKO, BLC(7), TRC(7), YINC, ZINC, FCUT,
     *   BCHAN, ECHAN, DOCAT, DOSLIC, DOMODL, DORESI, RANGE(2), PLTYPE,
     *   PIXVAL, XNIT, XGAUSS, GMAX(4), GPOS(8), GWIDTH(12), DOMAX(4),
     *   DOPOS(8), DOWIDT(12), BADD(10)
      DOUBLE PRECISION
     *      CAT8(64), OLD8(64)
      REAL      CAT4(128), OLD4(128), FBLANK, PMIN(36), PMAX(36)
      INTEGER   CATBLK(256), BUFF1(4096), BUFF2(4096), CATOLD(256),
     *   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ, ICODE,
     *   JCODE, DOCOMP(14), GCODE
      COMMON /INPARM/ NAMEIN, CLAIN, XSEQIN, XDISKI, NAMOUT, CLAOUT,
     *   XSEQO, XDISKO, BLC, TRC, YINC, ZINC, FCUT, BCHAN, ECHAN, DOCAT,
     *   DOSLIC, DOMODL, DORESI, RANGE, PLTYPE, PIXVAL, XNIT, XGAUSS,
     *   GMAX, GPOS, GWIDTH, DOMAX, DOPOS, DOWIDT, BADD
      COMMON /PARMS/ CATOLD, FBLANK, PMAX, PMIN, SEQIN, SEQOUT, DISKIN,
     *   DISKO, NEWCNO, OLDCNO, JBUFSZ, ICODE, JCODE, DOCOMP, GCODE
      COMMON /BUFRS/ BUFF1, BUFF2
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CAT4, CAT8)
      EQUIVALENCE (CATOLD, OLD4, OLD8)
C                                                          End DGAU.
