C                                                          Include DBKL.
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
C                                       Local include: BAKLD, ZBKLD*
      REAL      NTAPE4, NAME(3), CLASS(2), SEQ4, KVOL4, NFILES,
     *   NCOUNS, OPCODE
C                                       /BKLCM1/:
      INTEGER   TLUN, TIND, CNO, USER, NTAPE, NPARM, IVOL, ZLUN1, ZLUN2,
     *   LZDA00, LZMT0N, INS, OUTS
C                                       /BKLCM2/:
      CHARACTER ZDA00*80, ZBKNAM*30, BAKTXT*12, ZMT0N*24, BAKVER*60,
     *   TKNAME*6, DBLOG*7, BAKREC*60, INN*12, INCL*6, OUTN*12, OUTCL*6
C                                       /MAPHDR/:
      INTEGER   CATBLK(256)
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      COMMON /INPARM/ NTAPE4, NAME, CLASS, SEQ4, KVOL4, NFILES,
     *   NCOUNS, OPCODE
      COMMON /BKLCM1/ TLUN, TIND, CNO, USER, NTAPE, NPARM, IVOL, ZLUN1,
     *   ZLUN2, LZDA00, LZMT0N, INS, OUTS
      COMMON /BKLCM2/ ZDA00, ZBKNAM, BAKTXT, ZMT0N, BAKREC, BAKVER,
     *   TKNAME, DBLOG, INN, INCL, OUTN, OUTCL
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C                                                          End DBKL.
