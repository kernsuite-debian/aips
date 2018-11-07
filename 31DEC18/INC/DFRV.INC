C                                       Include for Frequency table.
C                                       Generic data types
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
      INCLUDE 'INCS:PTAB.INC'
C                                       AC definitions
      INTEGER   MAXFRC, NKEYFR, MAXWFR, MAXREC, MAXRCD
C                                       # cols in FR table
      PARAMETER (MAXFRC=6)
C                                       # keywords in FR table
      PARAMETER (NKEYFR=9)
C                                       max # words in header array
      PARAMETER (MAXWFR=15)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=200)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KFRE, KBFQ, KCHW, KTBW, KSID, KBBC
      PARAMETER (KFRE=1)
      PARAMETER (KBFQ=2)
      PARAMETER (KCHW=3)
      PARAMETER (KTBW=4)
      PARAMETER (KSID=5)
      PARAMETER (KBBC=6)
C                                       FR specific keywords
      INTEGER   IFRRNO
C                                       FR specific column variables
      INTEGER   IFQID, ISIDEB(MAXIF), IBBC(MAXIF)
      REAL      CHWID(MAXIF), TBANDW(MAXIF)
      DOUBLE PRECISION BANDFR(MAXIF)
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   FRKOLS(MAXFRC), FRNUMV(MAXFRC), DTYP(MAXFRC)
      INTEGER   KLOCS(NKEYFR), KEYTYP(NKEYFR)
      INTEGER   KEYVAL(MAXWFR)
      REAL      KEYVAR(MAXWFR)
      HOLLERITH KEYVAH(MAXWFR)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /FRTABV/ NKEY, NREC, DATP, NCOL, IPOINT, FRKOLS, FRNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /FRVALS/ BANDFR, CHWID, TBANDW, IFRRNO, IFQID, ISIDEB,
     *   IBBC
