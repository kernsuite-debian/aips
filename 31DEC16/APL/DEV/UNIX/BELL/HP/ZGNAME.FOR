      SUBROUTINE ZGNAME (NAME, IERR)
C-----------------------------------------------------------------------
C! get name of current process
C# System
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
C   Get the name of the current process.
C   Inputs:
C      none
C   Output:
C      NAME   C*6   Current process name
C      IERR   I     Error return code: 0 => no error
C                      1 => error
C   HP-UX version - use function IGETARG.
C-----------------------------------------------------------------------
      CHARACTER NAME*6
      INTEGER IERR
C
C                                       Length of returned string, 
C                                       constant, loop variable
      INTEGER   GLEN, GZERO, I, J, JTRIM, IL
      PARAMETER (GZERO = 0)
C                                       Local storage
      CHARACTER LOCNAM*256
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get zero'th argument (argv[0]
C                                       which corresponds to the command
C                                       name).  
      GLEN = IGETARG (GZERO, LOCNAM, LEN(LOCNAM))
      IF ((GLEN.LE.1) .OR. (GLEN.GT.LEN(LOCNAM))) THEN
         IERR = 1
      ELSE
C                                       clean up name
         J = 0
         IL = JTRIM (LOCNAM)
         DO 10 I = 1,IL
            IF (LOCNAM(I:I).EQ.'/') J = I
 10         CONTINUE
         NAME = LOCNAM(J+1:)
      ENDIF
C
 999  RETURN
      END
