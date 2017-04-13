      SUBROUTINE SCRNAM (NAMIN, NAMOUT)
C-----------------------------------------------------------------------
C! build scratch file name string in the WaWa form
C# IO-WaWa
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   WaWa IO system: Fill in the NAME and CLASS parts of a catalog
C   NAMESTRING to be appropriate for a scratch file.  The NAME will be
C   unique to the present task and initiator (NPOPS).  Output string
C   may be the same as input string.  The character strings are
C   packed.  NAME & CLASS are packed independently.
C   Inputs:
C      NAMIN   C*36   Input namestring
C   OUTPUTS:
C      NAMOUT  C*36   NAME,CLASS,CATTYPE,SEQ,VOL,USID for scratch
C                     file.  SEQ and VOL are copied from NAMIN.
C-----------------------------------------------------------------------
      CHARACTER NAMOUT*36, NAMIN*36
C
      INTEGER   IERR, SEQ, USEID, VOL
      CHARACTER NAME*12, CLASS*6, PTYPE*2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Copy SEQ, VOL
      CALL WAWA2A (NAMIN, NAME, CLASS, SEQ, PTYPE, VOL, USEID)
C                                       Use standard scratch name
      CALL BLDSNM (TSKNAM, NPOPS, NAME, CLASS, IERR)
C                                       Fill in type='SC'  usid=NLUSER
      PTYPE = 'SC'
      USEID = NLUSER
C                                       Return in name-string form
      CALL A2WAWA (NAME, CLASS, SEQ, PTYPE, VOL, USEID, NAMOUT)
C
 999  RETURN
      END
