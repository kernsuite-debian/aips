C                                       Include for Calibration
C                                       Information table (CA)
C                                       handling routines.
C                                       NB: CA table is different
C                                       from regular AIPS CL table.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998
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
C
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C
C                                       CA definitions
      INTEGER   MAXCAC, NKEYCA, MAXWCA, MAXREC, MAXRCD
C                                       # cols in CA table
      PARAMETER (MAXCAC=26)
C                                       # keywords in CA table
      PARAMETER (NKEYCA=11)
C                                       max # words in header array
      PARAMETER (MAXWCA=20)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=200)
C                                       max size of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=100)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KTIM, KINT, KSID, KANT, KARR, KFID,
     *   KTS1, KTA1, KSE1, KPH1, KRT1, KDY1, KRL1, KIM1, KWT1, KRF1,
     *   KTS2, KTA2, KSE2, KPH2, KRT2, KDY2, KRL2, KIM2, KWT2, KRF2
      PARAMETER (KTIM=1)
      PARAMETER (KINT=2)
      PARAMETER (KSID=3)
      PARAMETER (KANT=4)
      PARAMETER (KARR=5)
      PARAMETER (KFID=6)
      PARAMETER (KTS1=7)
      PARAMETER (KTA1=8)
      PARAMETER (KSE1=9)
      PARAMETER (KPH1=10)
      PARAMETER (KRT1=11)
      PARAMETER (KDY1=12)
      PARAMETER (KRL1=13)
      PARAMETER (KIM1=14)
      PARAMETER (KWT1=15)
      PARAMETER (KRF1=16)
      PARAMETER (KTS2=17)
      PARAMETER (KTA2=18)
      PARAMETER (KSE2=19)
      PARAMETER (KPH2=20)
      PARAMETER (KRT2=21)
      PARAMETER (KDY2=22)
      PARAMETER (KRL2=23)
      PARAMETER (KIM2=24)
      PARAMETER (KWT2=25)
      PARAMETER (KRF2=26)
C                                       CA specific keywords
      INTEGER   ICARNO, NOANT, NOPOL
C                                       CA specific column variables
      INTEGER   SOURID, ANTNO, ARRNO, FREQID, REFAN1, REFAN2
      REAL      TINT, TSYS1(MAXIF), TANT1(MAXIF), SENS1(MAXIF),
     *   PHASE1(MAXIF), RATE1(MAXIF), DELAY1(MAXIF),
     *   REAL1(MAXIF), IMAG1(MAXIF), WGHT1(MAXIF),
     *   TSYS2(MAXIF), TANT2(MAXIF), SENS2(MAXIF),
     *   PHASE2(MAXIF), RATE2(MAXIF), DELAY2(MAXIF),
     *   REAL2(MAXIF), IMAG2(MAXIF), WGHT2(MAXIF)
      DOUBLE PRECISION TIME
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   CAKOLS(MAXCAC), CANUMV(MAXCAC), DTYP(MAXCAC)
      INTEGER   KLOCS(NKEYCA), KEYTYP(NKEYCA)
      INTEGER   KEYVAL(MAXWCA)
      REAL      KEYVAR(MAXWCA)
      HOLLERITH KEYVAH(MAXWCA)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
      COMMON /CATABV/ NKEY, NREC, DATP, NCOL, IPOINT, CAKOLS, CANUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /CAVALS/ TIME, TINT, TSYS1, TANT1, SENS1, PHASE1,
     *   RATE1, DELAY1, REAL1, IMAG1, WGHT1, TSYS2, TANT2, SENS2,
     *   PHASE2, RATE2, DELAY2, REAL2, IMAG2, WGHT2,
     *   ICARNO, NOANT, NOPOL, SOURID, ANTNO, ARRNO, FREQID,
     *   REFAN1, REFAN2
