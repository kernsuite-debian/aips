      SUBROUTINE BLDSNM (ITASK, IPOPS, NAME, CLASS, IERR)
C-----------------------------------------------------------------------
C! builds a name for a scratch file
C# Utility, Header
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
C   BLDSNM builds a scratch file name
C   Inputs: ITASK   C*6       Task owning file
C           IPOPS   I         AIPS number of the task
C   Output: NAME    C*12      AIPS logical name
C           CLASS   C*6       AIPS logical class
C           IERR    I         0 => ok, 1 => ITASK > 5 chars
C-----------------------------------------------------------------------
      CHARACTER ITASK*6, NAME*12, CLASS*6
      INTEGER   IPOPS, IERR
C
      CHARACTER CTASK*6, SCNAME*12
      DATA SCNAME /'SCRATCH FILE'/
C-----------------------------------------------------------------------
      NAME = SCNAME
      CALL BLDTNM (ITASK, IPOPS, CTASK, IERR)
      CLASS = CTASK
C
 999  RETURN
      END
