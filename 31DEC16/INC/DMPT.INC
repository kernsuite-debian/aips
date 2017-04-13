C                                                          Include DMPT.
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
C                                       Local include for MPORT
C                                       Common /MPCOM/
      INTEGER   IUSER, OUTSEQ, OUTDSK, OUTLUN, OUTIND, OUTCNO,
     *   ICODE, ICHAN, LTYPE, HISNUM, WBUFF(512)
      LOGICAL   DOTV, HISOVF
      REAL      OUTNAM(3), OUTCLS(2), TBLC(7), TTRC(7), PIXRNG,
     *   CODETP, TVCORN(2), HISBUF(20,64)
C                                       Common /MAPHDR/
      INTEGER   CATBLK(256)
      INTEGER   CAT3(128)
      REAL      CAT4(128)
      DOUBLE PRECISION
     *      CAT8(64)
C                                       Local include for MPORT
      COMMON /MPCOM/ OUTNAM, OUTCLS, CODETP, TBLC, TTRC, PIXRNG,
     *   TVCORN, HISBUF, DOTV, HISOVF, IUSER, OUTSEQ, OUTDSK,
     *   OUTLUN, OUTIND, OUTCNO, ICODE, ICHAN, LTYPE, HISNUM,
     *   WBUFF
      COMMON /MAPHDR/ CATBLK
C                                       Local include for MPORT
      EQUIVALENCE (CATBLK, CAT3, CAT4, CAT8)
C                                                          End DMPT.
