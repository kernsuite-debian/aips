C                                                          Include DIVI.
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
      REAL      NAMEIN(3), CLAIN(2), SEQ1, DISK1, NAM2IN(3), CLA2IN(2),
     *   SEQ2, DISK2, BLC(7), TRC(7), DOALIN, XINC, YINC, ZINC,
     *   OPCODE, FUTYPE, DPARM(10)
      DOUBLE PRECISION
     *      XCAT8(64), YCAT8(64)
      REAL      XCAT4(128), YCAT4(128), FBLANK, BUFF1(4096), BUFF2(4096)
      INTEGER   XCAT2(256), YCAT2(256), XSEQIN, YSEQIN, XDISK, YDISK,
     *   XCNO, YCNO, JBUFSZ, ICODE, YBLC(7), YTRC(7), XLUN, YLUN, XIND,
     *   YIND, IXINC, IYINC, IZINC, XBINS, YBINS, IVER, PBUFF(256)
      COMMON /INPARM/ NAMEIN, CLAIN, SEQ1, DISK1, NAM2IN, CLA2IN,
     *   SEQ2, DISK2, BLC, TRC, DOALIN, XINC, YINC, ZINC, OPCODE,
     *   FUTYPE, DPARM
      COMMON /PARMS/ XCAT2, YCAT2, FBLANK, PBUFF, XSEQIN, YSEQIN, XDISK,
     *   YDISK, XCNO, YCNO, JBUFSZ, ICODE, YBLC, YTRC, XLUN, YLUN, XIND,
     *   YIND, IXINC, IYINC, IZINC, XBINS, YBINS, IVER
      COMMON /IMBUF/ BUFF1, BUFF2
      EQUIVALENCE (XCAT2, XCAT4, XCAT8)
      EQUIVALENCE (YCAT2, YCAT4, YCAT8)
C                                                          End DIVI.
