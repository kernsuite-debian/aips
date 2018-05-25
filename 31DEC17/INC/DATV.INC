C                                       Include for Antenna
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2009
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
C                                       Characteristics table
C                                       handling routines
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       AT definitions
      INTEGER   MAXATC, NKEYAT, MAXWAT, MAXREC, MAXRCD, MAXPCL
C                                       # cols in AT table
      PARAMETER (MAXATC=14)
C                                       # keywords in AT table
      PARAMETER (NKEYAT=11)
C                                       max # words in header array
      PARAMETER (MAXWAT=20)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=300)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       max number of polzn constants
      PARAMETER (MAXPCL=2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KTIM, KINT, KANN, KANO, KARR, KFRE, KNLV, KTYA, KPAA
     *   , KCLA, KTYB, KPAB, KCLB, KFWH
      PARAMETER (KTIM=1)
      PARAMETER (KINT=2)
      PARAMETER (KANN=3)
      PARAMETER (KANO=4)
      PARAMETER (KARR=5)
      PARAMETER (KFRE=6)
      PARAMETER (KNLV=7)
      PARAMETER (KTYA=8)
      PARAMETER (KPAA=9)
      PARAMETER (KCLA=10)
      PARAMETER (KTYB=11)
      PARAMETER (KPAB=12)
      PARAMETER (KCLB=13)
      PARAMETER (KFWH=13)
C                                       AT specific keywords
      INTEGER   NOPCAL, IATRNO
      CHARACTER POLTYP*8
C                                       AT specific column variables
      INTEGER   NOSTA, IARRAY, IFQID, NOLEVL
      CHARACTER ANNAME*8, POLTYA*1, POLTYB*1
      REAL      TINT, POLAA(MAXIF), POLAB(MAXIF), POLCA(MAXPCL,MAXIF),
     *   POLCB(MAXPCL,MAXIF), ANTFWH(MAXIF)
      DOUBLE PRECISION TIME
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   ATKOLS(MAXATC), ATNUMV(MAXATC), DTYP(MAXATC)
      INTEGER   KLOCS(NKEYAT), KEYTYP(NKEYAT)
      INTEGER   KEYVAL(MAXWAT)
      REAL      KEYVAR(MAXWAT)
      HOLLERITH KEYVAH(MAXWAT)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
      COMMON /ATTABV/ NKEY, NREC, DATP, NCOL, IPOINT, ATKOLS, ATNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /ATVALS/ TIME, TINT, POLAA, POLAB, POLCA, POLCB,
     *   NOPCAL, IATRNO, NOSTA, IARRAY, IFQID, NOLEVL, ANTFWH
      COMMON /ATCHAR/ POLTYP, ANNAME, POLTYA, POLTYB

