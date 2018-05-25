C                                       Include for Array Geometry
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
C                                       table handling routines
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       AG definitions
      INTEGER   MAXAGC, NKEYAG, MAXWAG, MAXREC, MAXRCD
C                                       # cols in AG table
      PARAMETER (MAXAGC=8)
C                                       # keywords in AG table
      PARAMETER (NKEYAG=24)
C                                       max # words in header array
      PARAMETER (MAXWAG=50)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=60)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=MAXREC/2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row.
      INTEGER    KANN, KSTA, KDER, KORB, KNOS, KMNT, KSTX, KDIA
      PARAMETER (KANN=1)
      PARAMETER (KSTA=2)
      PARAMETER (KDER=3)
      PARAMETER (KORB=4)
      PARAMETER (KNOS=5)
      PARAMETER (KMNT=6)
      PARAMETER (KSTX=7)
      PARAMETER (KDIA=8)
C                                       AG specific keywords
      INTEGER   NUMORB, IAGRNO
      CHARACTER ANAME*8, RDATE*8, FRAME*8, TIMSYS*8
      DOUBLE PRECISION  ARRAYC(3), SAFREQ, GSTIA0, DEGPDY
      REAL      POLARX, POLARY, UT1UTC, IATUTC
C                                       AG specific column variables
      INTEGER   NOSTA, MNTSTA
      CHARACTER ANNAME*8
      REAL      STAXOF(3), STADIA
      DOUBLE PRECISION STAXYZ(3), DERXYZ(3), ORBPRM(10)
C                                       Generic table variables
      INTEGER   NKEY, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   AGKOLS(MAXAGC), AGNUMV(MAXAGC), DTYP(MAXAGC)
      INTEGER   KLOCS(NKEYAG), KEYTYP(NKEYAG)
      INTEGER   KEYVAL(MAXWAG)
      REAL      KEYVAR(MAXWAG)
      HOLLERITH KEYVAH(MAXWAG)
      EQUIVALENCE (KEYVAL, KEYVAR, KEYVAH)
C
      COMMON /AGTABV/ NKEY, NREC, DATP, NCOL, IPOINT, AGKOLS, AGNUMV,
     *   DTYP, KLOCS, KEYTYP, KEYVAL
      COMMON /AGVALS/ ARRAYC, SAFREQ, GSTIA0, DEGPDY, STAXYZ, DERXYZ,
     *   ORBPRM, POLARX, POLARY, UT1UTC, IATUTC, STAXOF, NUMORB, IAGRNO,
     *   NOSTA, MNTSTA, STADIA
      COMMON /AGCHAR/ ANAME, RDATE, FRAME, TIMSYS, ANNAME
