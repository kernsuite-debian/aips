C                                       Include for Interferometer
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
C                                       IM definitions
      INTEGER   MAXIMC, NKEYIM, MAXWIM, MAXREC, MAXRCD, MAXPOL
C                                       # cols in IM table
      PARAMETER (MAXIMC=20)
C                                       # keywords in IM table
      PARAMETER (NKEYIM=12)
C                                       max # words in header array
      PARAMETER (MAXWIM=20)
C                                       Max order of polynomial
C                                       for geometrical parameters
      PARAMETER (MAXPOL=10)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=12 + MAXIF + 4*MAXPOL + 4*MAXIF*MAXPOL)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER   KTIM, KINT, KSOU, KANO, KARR, KFRE, KIFR, KFVR,
     *   KPDL1, KGDL1, KPRT1, KGRT1, KDSP1, KDDT1,
     *   KPDL2, KGDL2, KPRT2, KGRT2, KDSP2, KDDT2
      PARAMETER (KTIM=1)
      PARAMETER (KINT=2)
      PARAMETER (KSOU=3)
      PARAMETER (KANO=4)
      PARAMETER (KARR=5)
      PARAMETER (KFRE=6)
      PARAMETER (KIFR=7)
      PARAMETER (KFVR=8)
      PARAMETER (KPDL1=9)
      PARAMETER (KGDL1=10)
      PARAMETER (KPRT1=11)
      PARAMETER (KGRT1=12)
      PARAMETER (KDSP1=13)
      PARAMETER (KDDT1=14)
      PARAMETER (KPDL2=15)
      PARAMETER (KGDL2=16)
      PARAMETER (KPRT2=17)
      PARAMETER (KGRT2=18)
      PARAMETER (KDSP2=19)
      PARAMETER (KDDT2=20)
C                                       IM specific keywords
      INTEGER   IIMRNO, NOPOLZ, NPOLY
      REAL      REVNUM
C                                       IM specific column variables
      INTEGER   NOSTA, IARRAY, IFQID, ISRC
      REAL      TINT, IFR, FREQVR(MAXIF), DISP1, DDISP1, DISP2,
     *   DDISP2
      DOUBLE PRECISION TIME, PDELA1(MAXIF,MAXPOL), GDELA1(MAXPOL),
     *   PRATE1(MAXIF,MAXPOL), GRATE1(MAXPOL), PDELA2(MAXIF,MAXPOL),
     *   GDELA2(MAXPOL), PRATE2(MAXIF,MAXPOL), GRATE2(MAXPOL)

C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   IMKOLS(MAXIMC), IMNUMV(MAXIMC), DTYP(MAXIMC)
      INTEGER   KLOCS(NKEYIM), KEYTYP(NKEYIM)
      INTEGER   KEYVAL(MAXWIM)
      REAL      KEYVAR(MAXWIM)
      HOLLERITH KEYVAH(MAXWIM)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /IMTABV/ NKEY, NREC, DATP, NCOL, IPOINT,
     *   IMKOLS, IMNUMV, DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /IMVALS/ TIME, PDELA1, GDELA1, PRATE1, GRATE1, PDELA2,
     *   GDELA2, PRATE2, GRATE2,
     *   TINT, IFR, FREQVR, DISP1, DDISP1, DISP2, DDISP2,
     *   REVNUM, IIMRNO, NOPOLZ, NPOLY, NOSTA, IARRAY, IFQID, ISRC
