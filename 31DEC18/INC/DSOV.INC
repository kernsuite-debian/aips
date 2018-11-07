C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2012-2013
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
C                                       Include for Source
C                                       Information table (SO)
C                                       handling routines.
C                                       NB: SO table is different
C                                       from regular AIPS SU table.
C
C                                       Generic data types
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTAB.INC'
C
C                                       SO definitions
      INTEGER   MAXSOC, NKEYSO, MAXWSO, MAXREC, MAXRCD
C                                       # cols in SO table
      PARAMETER (MAXSOC=25)
C                                       # keywords in SO table
      PARAMETER (NKEYSO=9)
C                                       max # words in header array
      PARAMETER (MAXWSO=14)
C                                       max size of logical record
C                                       in single precision words
C                                       34 +  11 * NO_BAND
      PARAMETER (MAXREC=40+11*MAXIF)
C                                       max size of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=100)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KSID, KNAM, KQUA, KCOD, KFRQ, KIFX, KQFX, KUFX, KVFX,
     *   KALF, KFQO, KRAE, KDEE, KEQU, KRAA, KDEA, KSVL, KVTP, KVDF,
     *   KRFQ, KPMR, KPMD, KPAR, KRAO, KDEO
      PARAMETER (KSID=1)
      PARAMETER (KNAM=2)
      PARAMETER (KQUA=3)
      PARAMETER (KCOD=4)
      PARAMETER (KFRQ=5)
      PARAMETER (KIFX=6)
      PARAMETER (KQFX=7)
      PARAMETER (KUFX=8)
      PARAMETER (KVFX=9)
      PARAMETER (KALF=10)
      PARAMETER (KFQO=11)
      PARAMETER (KRAE=12)
      PARAMETER (KDEE=13)
      PARAMETER (KEQU=14)
      PARAMETER (KRAA=15)
      PARAMETER (KDEA=16)
      PARAMETER (KRAO=17)
      PARAMETER (KDEO=18)
      PARAMETER (KSVL=19)
      PARAMETER (KVTP=20)
      PARAMETER (KVDF=21)
      PARAMETER (KRFQ=22)
      PARAMETER (KPMR=23)
      PARAMETER (KPMD=24)
      PARAMETER (KPAR=25)
C
C                                       SO specific keywords
      INTEGER   ISORNO
C                                       SO specific column variables
      INTEGER   ISOUR, QUAL, FREQID
      REAL      IFLUX(MAXIF), QFLUX(MAXIF), UFLUX(MAXIF), VFLUX(MAXIF),
     *   ALPHA(MAXIF), PARLAX
      DOUBLE PRECISION FRQOFF(MAXIF), SYSVEL(MAXIF), RSTFRQ(MAXIF),
     *   RAEPO, DECEPO, RAAPP, DECAPP, RAOBS, DECOBS, EQUINX, PMRA,
     *   PMDEC
      CHARACTER SAUCE*16, CALCOD*4, VELTYP*8, VELDEF*8
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   SOKOLS(MAXSOC), SONUMV(MAXSOC), DTYP(MAXSOC)
      INTEGER   KLOCS(NKEYSO), KEYTYP(NKEYSO)
      INTEGER   KEYVAL(MAXWSO)
      REAL      KEYVAR(MAXWSO)
      HOLLERITH KEYVAH(MAXWSO)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /SOTABV/ NKEY, NREC, DATP, NCOL, IPOINT, SOKOLS, SONUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /SOVALS/ FRQOFF, SYSVEL, RSTFRQ, RAEPO, DECEPO, RAAPP,
     *   DECAPP, RAOBS, DECOBS, EQUINX, PMRA, PMDEC, IFLUX, QFLUX,
     *   UFLUX, VFLUX, ALPHA, PARLAX, ISORNO, ISOUR, QUAL, FREQID
      COMMON /SOCHAR/ SAUCE, CALCOD, VELTYP, VELDEF
