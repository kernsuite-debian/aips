C                                                          Include DBCON
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
C                                       Local include for DBCON
      INTEGER   MAXSOU
C                                       MAXSOU = size of source table
      PARAMETER (MAXSOU=1000)
      INTEGER   SEQ1, SEQ2, SEQ3, DISK1, DISK2, DISK3, BUFSZ1, BUFSZ2,
     *   CAT1(256), CAT2(256), MAXA(2), FIXPOL, POLANT(4),
     *   I1LOCU, I1LOCV, I1LOCW, I1LOCT, I1LOCB, I1LOCS, J1LOCC,
     *   J1LOCS, J1LOCF, J1LOCD, J1LOCR, J1LOCI, INCS1, INCF1,
     *   INCIF1, ICOR01, NPARM1, LREC1, NCOR1, TYUVD1,
     *   I2LOCU, I2LOCV, I2LOCW, I2LOCT, I2LOCB, I2LOCS, J2LOCC,
     *   J2LOCS, J2LOCF, J2LOCR, J2LOCD, J2LOCI, INCS2, INCF2,
     *   INCIF2, ICOR02, NPARM2, LREC2, NCOR2, TYUVD2,
     *   IORD(2,100), JORD(2,2000), IDUM(2), NUMSOU, SOUTRA(MAXSOU)
      INTEGER   NVIS1, NVIS2
      REAL      NAME1(3), CLASS1(2), XS1, XD1, NAME2(3), CLASS2(2),
     *   XS2, XD2, NAME3(3), CLASS3(2), XS3, XD3, BUFF1(5000,2),
     *   BUFF2(5000), SOUR1(2), SOUR2(2), ISORT1, ISORT2,
     *   DOPOS, DOFRQ, DOARR, DUMC(6)
      DOUBLE PRECISION FREQ1, FREQ2, RA1, RA2, DEC1, DEC2
      COMMON /INPARM/ NAME1, CLASS1, XS1, XD1, NAME2, CLASS2, XS2,
     *   XD2, NAME3, CLASS3, XS3, XD3, DOPOS, DOFRQ, DUMC, DOARR,
     *   SEQ1, SEQ2, SEQ3, DISK1, DISK2, DISK3, BUFSZ1, BUFSZ2
      COMMON /BUFRS/ BUFF1, BUFF2
C                                       This common MUST match the one
C                                       in DUVH.INC
      COMMON /CATHDR/ CAT1, CAT2,
     *   FREQ1, RA1, DEC1, SOUR1, ISORT1, NVIS1, I1LOCU, I1LOCV, I1LOCW,
     *   I1LOCT, I1LOCB, I1LOCS, J1LOCC, J1LOCS, J1LOCF, J1LOCR,
     *   J1LOCD, J1LOCI, INCS1, INCF1, INCIF1, ICOR01, NPARM1,
     *   LREC1, NCOR1, TYUVD1,    IDUM,
     *   FREQ2, RA2, DEC2, SOUR2, ISORT2, NVIS2, I2LOCU, I2LOCV, I2LOCW,
     *   I2LOCT, I2LOCB, I2LOCS, J2LOCC, J2LOCS, J2LOCF, J2LOCR,
     *   J2LOCD, J2LOCI, INCS2, INCF2, INCIF2, ICOR02, NPARM2,
     *   LREC2, NCOR2, TYUVD2,
     *   IORD, JORD, MAXA, FIXPOL, POLANT, NUMSOU, SOUTRA
C                                                          End DBCON
