C                                       Include for CALC
C                                       Model table handling
C                                       routines.
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
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       CM definitions
      INTEGER   MAXCMC, NKEYCM, MAXWCM, MAXREC, MAXRCD, MAXPOL
C                                       # cols in CM table
      PARAMETER (MAXCMC=16)
C                                       # keywords in CM table
      PARAMETER (NKEYCM=12)
C                                       max # words in header array
      PARAMETER (MAXWCM=20)
C                                       Max order of polynomial
C                                       for geometrical parameters
      PARAMETER (MAXPOL=10)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=8+13*MAXIF+8*MAXPOL)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER   KTIM, KSOU, KANO, KARR, KFRE, KATM, KDATM,
     *   KGDL, KCLK1, KDCLK1, KDSP1, KDDSP1, KCLK2, KDCLK2,
     *   KDSP2, KDDSP2
      PARAMETER (KTIM=1)
      PARAMETER (KSOU=2)
      PARAMETER (KANO=3)
      PARAMETER (KARR=4)
      PARAMETER (KFRE=5)
      PARAMETER (KATM=6)
      PARAMETER (KDATM=7)
      PARAMETER (KGDL=8)
      PARAMETER (KCLK1=9)
      PARAMETER (KDCLK1=10)
      PARAMETER (KDSP1=11)
      PARAMETER (KDDSP1=12)
      PARAMETER (KCLK2=13)
      PARAMETER (KDCLK2=14)
      PARAMETER (KDSP2=15)
      PARAMETER (KDDSP2=16)
C                                       CM specific keywords
      INTEGER   ICMRNO, NOPOLZ, NPOLY
      REAL      REVNUM
C                                       CM specific column variables
      INTEGER   NOSTA, IARRAY, IFQID, ISRC
      REAL      ATMOS, DATMOS, CLOCK1, DCLK1, DISP1, DDISP1,
     *   CLOCK2, DCLK2, DISP2, DDISP2
      DOUBLE PRECISION TIME, GDELAY(MAXPOL)
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   CMKOLS(MAXCMC), CMNUMV(MAXCMC), DTYP(MAXCMC)
      INTEGER   KLOCS(NKEYCM), KEYTYP(NKEYCM)
      INTEGER   KEYVAL(MAXWCM)
      REAL      KEYVAR(MAXWCM)
      HOLLERITH KEYVAH(MAXWCM)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /CMTABV/ NKEY, NREC, DATP, NCOL, IPOINT, CMKOLS, CMNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /CMVALS/ TIME, GDELAY, ATMOS, DATMOS, CLOCK1, DCLK1,
     *   DISP1, DDISP1, CLOCK2, DCLK2, DISP2, DDISP2, REVNUM,
     *   ICMRNO, NOPOLZ, NPOLY, NOSTA, IARRAY, IFQID, ISRC
