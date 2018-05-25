C                                                          Include DMPR.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C                                       include for gridding
C                                       and correction routines.
C                                       NOTE: uses PARAMETER in DGDS.INC
      INTEGER   NXBEM(MAXFLD), NYBEM(MAXFLD), NXUNF, NYUNF, NXMAX,
     *   NYMAX, GUACNT, ICNTRX(MAXFLD), ICNTRY(MAXFLD), CTYPX, CTYPY,
     *   NUVCH, CHUV1, NCHAVG, UNFBOX, TVFLD, BORES(MAXFLD),
     *   BOBEM(MAXFLD), MDISKN, MSEQ, NBXFUN
      LOGICAL   DOZERO, DOTAPE, DOUNIF
      REAL      XFLD(MAXFLD), YFLD(MAXFLD), XPARM(10), YPARM(10),
     *   TAPERU, TAPERV, ZEROSP(5), GUARDB(2), FLDMAX(MAXFLD),
     *   FLDMIN(MAXFLD), BEMMAX(MAXFLD), XSHIFT(MAXFLD), YSHIFT(MAXFLD),
     *   BLMAX, BLMIN, GUAXAU, GUAXAV, GUAXBU, GUAXBV, WTPOWR, ROBUST
      CHARACTER MNAME*12, MCLASS*6
      DOUBLE PRECISION FREQUV
      COMMON /GRDCOM/ FREQUV,
     *   XFLD, YFLD, XPARM, YPARM, TAPERU, TAPERV, ZEROSP, GUARDB,
     *   GUAXAU, GUAXAV, GUAXBU, GUAXBV, FLDMAX, FLDMIN, BEMMAX, XSHIFT,
     *   YSHIFT, BLMAX, BLMIN, DOZERO, DOTAPE, DOUNIF, NXBEM, NYBEM,
     *   NXUNF, NYUNF, NXMAX, NYMAX, ICNTRX, ICNTRY, CTYPX, CTYPY,
     *   NUVCH, CHUV1, NCHAVG, UNFBOX, TVFLD, BORES, BOBEM, MDISKN,
     *   MSEQ, GUACNT, WTPOWR, ROBUST, NBXFUN
      COMMON /GRDCHR/ MNAME, MCLASS
C                                                          End DMPR.
