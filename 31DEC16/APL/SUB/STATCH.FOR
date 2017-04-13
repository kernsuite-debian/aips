      SUBROUTINE STATCH (OLDST, NEWST, IVOL, ISLOT, FTYPE, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Changes state of a file
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005
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
C   Routine to clear a file of one state and change it to another state.
C   Inputs:
C      OLDST    C*4    State to clear from the file ('READ', 'WRIT',
C                      else no clear)
C      NEWST    C*4    New state into which file should be placed
C                      ('READ', 'WRIT', else no new state)
C      IVOL     I      Volume number on which file resides
C      ISLOT    I      Catalogue number of file
C      FTYPE    C*2    Type of file, 'UV' or 'MA'
C      BUFFER   I(*)   Work buffer
C   Outputs:
C      IERR     I      Error code, 0 => OK, else from CATDIR with
C                      warnings reset to 0.
C-----------------------------------------------------------------------
      CHARACTER OLDST*4, NEWST*4, FTYPE*2
      INTEGER   IVOL, ISLOT, BUFFER(*), IERR
C
      CHARACTER CTEMP*12, CLRST*4
      INTEGER   IDUM
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       clear
      IF (OLDST.EQ.'READ') THEN
         CLRST = 'CLRD'
      ELSE IF (OLDST.EQ.'WRIT') THEN
         CLRST = 'CLWR'
      ELSE
         CLRST = ' '
         END IF
      IF (CLRST.NE.' ') THEN
         CALL CATDIR ('CSTA', IVOL, ISLOT, CTEMP, CTEMP, IDUM, FTYPE,
     *      IDUM, CLRST, BUFFER, IERR)
         IF (IERR.EQ.10) IERR = 0
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'BY', CLRST
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      IF ((NEWST.EQ.'READ') .OR. (NEWST.EQ.'WRIT')) THEN
         CALL CATDIR ('CSTA', IVOL, ISLOT, CTEMP, CTEMP, IDUM, FTYPE,
     *      IDUM, NEWST, BUFFER, IERR)
         IF (IERR.EQ.9) IERR = 0
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'TO', NEWST
            CALL MSGWRT (8)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STATCH: ERROR ',I3,' CHANGING STATUS ',A,1X,A4)
      END
