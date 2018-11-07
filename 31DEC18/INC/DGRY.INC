C                                                          Include DGRY.
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
      REAL      DOCONT, PRUSER, NAMIN(3), CLSIN(2), SEQIN, DSKIN,
     *   NAMIN2(3), CLSIN2(2), SEQIN2, DSKIN2, BLC(7), TRC(7), XYRATO,
     *   RANGE(2), TLABEL, DOGRID, PLEV, CLEV, LEVS(30), DOWDGE,
     *   DOCIRC, XINVER, STMULT
      INTEGER   IHDBUF(256), IHDBF2(256)
      REAL      RHDBUF(128), RHDBF2(256)
      DOUBLE PRECISION
     *      DHDBUF(64),  DHDBF2(64)
      INTEGER   IBUFF(4096), ILROW(2048)
      REAL      BUFF(2048),  RLROW(2048)
      LOGICAL   NOCONT, SAME, CSAME
      DOUBLE PRECISION
     *      GFAC, GOFF
      REAL      MULT, RANGE2(2), IGFILE(6)
      INTEGER   IMLUN, IMFIND, IMLUN2, IMFND2, IGLUN, IGFIND,
     *   IVOL, ISLOT, IVOL2, ISLOT2, ILPVAL, IHPVAL, ILABEL,
     *   GTYPE, CTYPE, IVER, INPRMS, IXWDGE, IYWDGE, INVER
      COMMON /MAPHDR/ IHDBUF, IHDBF2
      COMMON /INPARM/ DOCONT, PRUSER, NAMIN, CLSIN, SEQIN, DSKIN,
     *   NAMIN2, CLSIN2, SEQIN2, DSKIN2, BLC, TRC, XYRATO, RANGE,
     *   TLABEL, DOGRID, PLEV, CLEV, LEVS, DOWDGE, DOCIRC, XINVER,
     *   STMULT
      COMMON /CNTRBU/ IBUFF, RLROW
      COMMON /GREYPR/ GFAC, GOFF, MULT, RANGE2, IGFILE, NOCONT, SAME,
     *   CSAME, IMLUN, IMFIND, IMLUN2, IMFND2, IGLUN, IGFIND,
     *   IVOL, ISLOT, IVOL2, ISLOT2, ILPVAL, IHPVAL, ILABEL, GTYPE,
     *   CTYPE, IVER, INPRMS, IXWDGE, IYWDGE, INVER
      EQUIVALENCE (IHDBUF, RHDBUF, DHDBUF)
      EQUIVALENCE (IHDBF2, RHDBF2, DHDBF2)
      EQUIVALENCE (IBUFF, BUFF),     (ILROW, RLROW)
C                                                          End DGRY.
