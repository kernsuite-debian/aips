C                                       Include for Baseline Corr.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2008
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
C                                       table handling routines
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       BC definitions
      INTEGER   MAXBCC, NKEYBC, MAXWBC, MAXREC, MAXRCD
C                                       # cols in BC table
      PARAMETER (MAXBCC=9)
C                                       # keywords in BC table
      PARAMETER (NKEYBC=10)
C                                       max # words in header array
      PARAMETER (MAXWBC=20)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=6+4*MAXIF*4)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KTIM, KSOU, KARR, KANO, KFQI, KRM, KIM, KRA, KIA
      PARAMETER (KTIM=1)
      PARAMETER (KSOU=2)
      PARAMETER (KARR=3)
      PARAMETER (KANO=4)
      PARAMETER (KFQI=5)
      PARAMETER (KRM=6)
      PARAMETER (KIM=7)
      PARAMETER (KRA=8)
      PARAMETER (KIA=9)
C                                       BC specific keywords
      INTEGER   IBCRNO, NOANT
C                                       BC specific column variables
      INTEGER   ISRC, ANTS(2), IARRAY, IFQID
      REAL      TIME, RLMUL(4*MAXIF), IMMUL(4*MAXIF), RLADD(4*MAXIF),
     *   IMADD(4*MAXIF)
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   BCKOLS(MAXBCC), BCNUMV(MAXBCC), DTYP(MAXBCC)
      INTEGER   KLOCS(NKEYBC), KEYTYP(NKEYBC)
      INTEGER   KEYVAL(MAXWBC)
      REAL      KEYVAR(MAXWBC)
      HOLLERITH KEYVAH(MAXWBC)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
      COMMON /BCTABV/ NKEY, NREC, DATP, NCOL, IPOINT, BCKOLS, BCNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /BCVALS/ TIME, RLMUL, IMMUL, RLADD, IMADD,
     *   IBCRNO, NOANT, ISRC, ANTS, IARRAY, IFQID
