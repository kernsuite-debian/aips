C                                       Include for Bandpass table
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2002
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
C                                       handling routines
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       BT definitions
      INTEGER   MAXBTC, NKEYBT, MAXWBT, MAXREC, MAXRCD
C                                       # cols in BT table
      PARAMETER (MAXBTC=14)
C                                       # keywords in BT table
      PARAMETER (NKEYBT=13)
C                                       max # words in header array
      PARAMETER (MAXWBT=20)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=XBPRSZ)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KTIM, KINT, KSOU, KANO, KARR, KFQI, KBWD, KBFR,
     *   KRF1, KBR1, KBI1, KRF2, KBR2, KBI2
      PARAMETER (KTIM=1)
      PARAMETER (KINT=2)
      PARAMETER (KSOU=3)
      PARAMETER (KANO=4)
      PARAMETER (KARR=5)
      PARAMETER (KFQI=6)
      PARAMETER (KBWD=7)
      PARAMETER (KBFR=8)
      PARAMETER (KRF1=9)
      PARAMETER (KBR1=10)
      PARAMETER (KBI1=11)
      PARAMETER (KRF2=12)
      PARAMETER (KBR2=13)
      PARAMETER (KBI2=14)
C                                       BT specific keywords
      INTEGER   IBTRNO, NOANT, NOPOLZ, CHN1, NOBTCH
C                                       BT specific column variables
      INTEGER   ISRC, NOSTA, IARRAY, IFQID, REFAN1, REFAN2
      REAL      TINT, CHBW
      DOUBLE PRECISION TIME, BANDFR(MAXIF)
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   BTKOLS(MAXBTC), BTNUMV(MAXBTC), DTYP(MAXBTC)
      INTEGER   KLOCS(NKEYBT), KEYTYP(NKEYBT)
      INTEGER   KEYVAL(MAXWBT)
      REAL      KEYVAR(MAXWBT)
      HOLLERITH KEYVAH(MAXWBT)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
      COMMON /BTTABV/ NKEY, NREC, DATP, NCOL, IPOINT, BTKOLS, BTNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /BTVALS/ TIME, BANDFR, TINT, CHBW, IBTRNO, NOANT, NOPOLZ,
     *   CHN1, NOBTCH, ISRC, NOSTA, IARRAY, IFQID, REFAN1, REFAN2
