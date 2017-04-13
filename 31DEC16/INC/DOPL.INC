C                                                          Include DOPL.
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
      INTEGER   K(4318)
C                                        character strings
      REAL   NAMIN(3), CLSIN(2), TYPIN, NAMIN2(3), CLSIN2(2),
     *   TYPIN2, NAMIN3(3), CLSIN3(2), TYPIN3, NAMOUT(3),
     *   CLSOUT(2), EXTIN, EXTIN2, EXTIN3, TASK(2), COMBCD,
     *   TRANFN, SOURCE(2), STOKES, BAND, SORT, WTFN
C                                        numeric variables
      REAL   XTRUE, XFALSE, PRUSER, SEQIN, DSKIN, SEQIN2, DSKIN2,
     *   SEQIN3, DSKIN3, SEQOUT, DSKOUT, VERSN, VERSN2, VERSN3,
     *   BADDSK(10), TAPEIN, TAPOUT, NFILES, NMAPS, IWAIT, PRPRIO,
     *   BLCORN(7), TRCORN(7), XINC, YINC, MAPXY(2), MAPVAL,
     *   IRANGE(2), FACTOR, OFFSET, SNCUT, CTYPEX, CPARMX(10),
     *   CTYPEY, CPARMY(10), ROTATE, CLGAIN, CLNITR, CLFMIN, QUAL,
     *   IMCHAN, GRCHAN, TVLEVS, IMCORN(2), COLORS, TVXY(2),
     *   CLUSTV, NQUEUE, BTLINE, BTLINS, JOBNO, LABEL, PLEV, CLEV,
     *   LEVS(20), XYRATO, INVERS, CENTER, XZRATO, SKEWPL, DOCONT,
     *   DOVECT, IPLCUT, PPLCUT, OBSDST, MPPNTS(2), MAPFLD(2),
     *   MPSHFT(2), TAPER(2), BLLIM(2), UNFBOX, GRDCOR, ZEROSP(5),
     *   CLREST, CLBMAJ, CLBMIN, CLBPA, CLNBOX, CLWIND(4,10), WEOF,
     *   NDIG,
     *         STRA1, STRB1, STRC1, STRA2(2), STRB2(2), STRC2(2),
     *         STRA3(3), STRB3(3), STRC3(3), ARRAY1(10),
     *         ARRAY2(20,2), ARRAY3(3), SCALR1, SCALR2, SCALR3
      COMMON /CORE/ K, XTRUE, XFALSE, PRUSER, NAMIN, CLSIN, SEQIN,
     *   DSKIN, TYPIN, NAMIN2, CLSIN2, SEQIN2, DSKIN2, TYPIN2,
     *   NAMIN3, CLSIN3, SEQIN3, DSKIN3, TYPIN3, NAMOUT, CLSOUT,
     *   SEQOUT, DSKOUT, EXTIN, EXTIN2, EXTIN3, VERSN, VERSN2,
     *   VERSN3, BADDSK, TAPEIN, TAPOUT, NFILES, NMAPS, TASK,
     *   IWAIT, PRPRIO, BLCORN, TRCORN, XINC, YINC, MAPXY, MAPVAL,
     *   IRANGE, FACTOR, OFFSET, SNCUT, CTYPEX, CPARMX, CTYPEY,
     *   CPARMY, COMBCD, TRANFN, ROTATE, CLGAIN, CLNITR, CLFMIN,
     *   SOURCE, QUAL, STOKES, BAND, IMCHAN, GRCHAN, TVLEVS,
     *   IMCORN, COLORS, TVXY, CLUSTV, NQUEUE, BTLINE, BTLINS,
     *   JOBNO, LABEL, PLEV, CLEV, LEVS, XYRATO, INVERS, CENTER,
     *   XZRATO, SKEWPL, DOCONT, DOVECT, IPLCUT, PPLCUT, OBSDST,
     *   MPPNTS, MAPFLD, MPSHFT, SORT, TAPER, BLLIM, WTFN, UNFBOX,
     *   GRDCOR, ZEROSP, CLREST, CLBMAJ, CLBMIN, CLBPA, CLNBOX,
     *   CLWIND, WEOF, NDIG,
     *         STRA1, STRA2, STRA3, STRB1, STRB2, STRB3, STRC1,
     *         STRC2, STRC3, ARRAY1, ARRAY2, ARRAY3, SCALR1, SCALR2,
     *         SCALR3
C                                                          End DOPL.
