C                                       Include for Over-the-Top
C                                       table handling routines
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2011
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
C                                       column data types
      INCLUDE 'INCS:PTAB.INC'
C                                       OT definitions
      INTEGER   MAXOTC, MAXWOT, MAXREC, MAXRCD
C                                       max # cols in OT table
      PARAMETER (MAXOTC=5)
C                                       max # words in header array
      PARAMETER (MAXWOT=12)
C                                       max size of logical record
C                                       in single precision words
      PARAMETER (MAXREC=200)
C                                       max siz of logical record
C                                       in double precision words
      PARAMETER (MAXRCD=100)
C                                       current OT revision number
      INTEGER OTREV
      PARAMETER (OTREV = 2)
C                                       Column numbering
C                                       3rd character gives data type:
      INTEGER   OTRTIM, OTRTMI, OTISID, OTIANT, OTLOTT
C
C                                       OTRTIM = Time
      PARAMETER (OTRTIM = 1)
C                                       OTRTMI = Time interval
      PARAMETER (OTRTMI = OTRTIM+1)
C                                       OTISID = Source ID.
      PARAMETER (OTISID = OTRTMI+1)
C                                       OTIANT = Antenna number
      PARAMETER (OTIANT = OTISID+1)
C                                       OTLOTT = Over the top?
      PARAMETER (OTLOTT = OTIANT+1)
C                                       OT specific column variables
      DOUBLE PRECISION TIME8
      REAL             DT
      INTEGER          JDSOUR, IANT
      LOGICAL          OTT
      COMMON /OTVALS/  TIME8, DT, JDSOUR, IANT, OTT
C                                       Generic table variables
      INTEGER   IOTRNO, NREC, DATP(128,2), NCOL, IPOINT
      INTEGER   OTKOLS(MAXOTC), OTNUMV(MAXOTC), DTYP(MAXOTC)
      LOGICAL   TDUBL
      COMMON /OTTABV/ IOTRNO, NREC, DATP, NCOL, IPOINT, OTKOLS, OTNUMV,
     *   DTYP, TDUBL

