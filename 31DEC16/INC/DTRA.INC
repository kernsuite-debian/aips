C                                                          Include DTRA.
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
      REAL      USERID, INNAME(3), INCLAS(2), INSEQ, INDISK,
     *   OUTNAM(3), OUTCLS(2), OUTSEQ, OUTDSK, BLC(7), TRC(7),
     *   OPCODE(4), BADISK(10)
      REAL      RMAX, RMIN
      INTEGER   RIAX(7), RSGN(7), NPI(7), NSI(7), IMMAX, IMMIN,
     *   DONMAX, IMTYP, INLUN, ININD, OUTLUN, OUTIND, NBUF1, NBUF2,
     *   BUF(8192), BUF2(16384)
      COMMON /INPARM/ USERID, INNAME, INCLAS, INSEQ, INDISK, OUTNAM,
     *   OUTCLS, OUTSEQ, OUTDSK, BLC, TRC, OPCODE, BADISK
      COMMON /TRACOM/ RMIN, RMAX, IMMIN, IMMAX, DONMAX, IMTYP,
     *   RIAX, RSGN, NPI, NSI, INLUN, ININD, OUTLUN, OUTIND,
     *   NBUF1, NBUF2
      COMMON /TRABUF/ BUF, BUF2
C                                                          End DTRA.
