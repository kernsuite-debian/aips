      SUBROUTINE AIPINI (TTY, PRGNAM, IERR)
C-----------------------------------------------------------------------
C! does all AIPS initializations for a stand-alone program
C# Service System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   This program makes the initializations needed for a stand-alone task
C   using the AIPS data files, I/O system, message loging, etc.  It also
C   opens the terminal for use with ZTTYIO.
C   Inputs:
C      TTY      I(2)   (1) LUN to use for the terminal.
C      PRGNAM   C*6    program name
C   Output:
C      TTY      I(2)   (2) FTAB index of open terminal.
C      IERR     I      Error return - can't open terminal
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   TTY(2), IERR
C
      CHARACTER CANAME*48
      LOGICAL   T, F, FILAI2, FILAIP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Is this 'FILAI2' (only used in
C                                       VMS installation)
      FILAI2 = PRGNAM.EQ.'FILAI2'
C                                       Is this 'FILAIP' (used by non-
C                                       VMS installations)
      FILAIP = PRGNAM.EQ.'FILAIP'
C                                       Device characteristics common
C                                       (get parameters from disk file
C                                       unless this is FILAI2 or FILAIP)
      TSKNAM = PRGNAM
      IF ((FILAI2) .OR. (FILAIP)) THEN
         CALL ZDCHIN (F)
      ELSE
         CALL ZDCHIN (T)
         END IF
C                                       AIPS inits: message common
      CALL ZMYVER
C                                       Create accounting entry (unless
C                                       this is FILAI2 or FILAIP)
      IF ((.NOT.FILAI2) .AND. (.NOT.FILAIP)) CALL ACOUNT (1)
C                                       Image header pointers
      CALL VHDRIN
C                                       If this is VMS specific FILAI2,
C                                       open logical unit 6, else open
C                                       logical unit 5
      IF (FILAI2) THEN
         CALL ZMSGOP (TTY(1), TTY(2), 1, CANAME, IERR)
      ELSE
         CALL ZTTOPN (TTY(1), TTY(2), IERR)
         END IF
C
 999  RETURN
      END
