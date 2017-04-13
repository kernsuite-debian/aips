      SUBROUTINE IOSET
C-----------------------------------------------------------------------
C! initialize tables and set buffer space for WaWa IO
C# IO-WaWa
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
C   WaWa IO package: Establish buffer space and initialize tables
C   for "hidden" i/o operations with 5 simultaneous maps open
C   Written by WaWA (Last Rev  : 4/21/81)
C-----------------------------------------------------------------------
      CHARACTER COMAND(12)*4, WNAMS(8)*6, RD1(12)*4
      INTEGER   POINT(13), NCOM, NPOINT, I
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (RD1, WRIT)
      DATA NCOM, NPOINT /12, 13/
C                                       Standard character opcodes
      DATA COMAND /'WRIT', 'READ', 'CLWR', 'CLRD', 'REST', 'OPEN',
     *   'CLOS', 'SRCH', 'INFO', 'UPDT', 'FINI', 'CSTA'/
C                                       Defs of pointers to FILTAB
      DATA POINT /1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 23, 30, 37/
C                                       Defs of subroutine names
      DATA WNAMS /'CATDIR', 'CATIO ', 'MINIT ', 'MDISK ',
     *            'ZCLOSE', 'ZCREAT', 'ZDESTR', 'ZOPEN '/
C-----------------------------------------------------------------------
C                                       How many map files?
      NFIL = 5
C                                       Initialize CDCH tables
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       How long is one hidden buffer?
      LREAL = MABFSS
C                                       Same number in integer form
      LINT  = LREAL
C                                       Clear all LUN slots
      EFIL = 5 + NFIL
      DO 10 I = 1,EFIL
         FILTAB(1,I) = 0
 10      CONTINUE
C                                       Transfer opcodes to common
      DO 15 I = 1,NCOM
         RD1(I) = COMAND(I)
 15      CONTINUE
      INDEF = FBLANK
C                                       Transfer subroutine names
      DO 20 I = 1,8
         SUBNAM(I) = WNAMS(I)
 20      CONTINUE
C                                       Transfer pointers to common
      CALL COPY (NPOINT, POINT, POLUN)
C                                       Initialize 'DIE' common.
      NCFILE = 0
      NSCR = 0
      CALL FILL (10, 0, IBAD)
C
 999  RETURN
      END
