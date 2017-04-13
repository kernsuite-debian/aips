      SUBROUTINE MLREOF (FDVEC, TBIND, UNKNWN, BUFFER, IERR)
C-----------------------------------------------------------------------
C! advances tape to end of file and reports records read in TAPIO system
C# Tape
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
C   MLREOF advances the tape counting records until an end of file is
C   read.
C   Inputs:
C      FDVEC    I(50)  File descriptor vector.
C      TBIND    I      Buffer pointer
C   In/Out:
C      UNKNWN   I      Number of unknown records skipped
C   Output:
C      BUFFER   I(*)   I/O buffer.
C      IERR     I      0 => okay (incl. EOF), else tape trouble
C-----------------------------------------------------------------------
      INTEGER   FDVEC(50), TBIND, UNKNWN, BUFFER(*), IERR
C
      INTEGER   ERREOF, I, NC
      INCLUDE 'INCS:DMSG.INC'
      DATA ERREOF /4/
C-----------------------------------------------------------------------
      NC = 0
C                                        suppress messages
      MSGSUP = 32000
      DO 20 I = 1,2000
         NC = NC + 1
         UNKNWN = UNKNWN + 1
         CALL TAPIO ('READ', FDVEC, BUFFER, TBIND, IERR)
         IF (IERR.EQ.0) GO TO 20
            NC = NC - 1
            UNKNWN = UNKNWN - 1
            IF (IERR.NE.ERREOF) NC = -I
            GO TO 30
 20      CONTINUE
C                                        messages
 30   MSGSUP = 0
      WRITE (MSGTXT,1030) NC
      IF (NC.GE.2000) WRITE (MSGTXT,1031)
      IF (NC.EQ.0) WRITE (MSGTXT,1032)
      IF (NC.LT.0) WRITE (MSGTXT,1033) IERR
      CALL MSGWRT (5)
      IF (NC.GE.0) IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I7,' UNKNOWN RECORDS READ BEFORE EOF')
 1031 FORMAT ('END OF FILE NOT FOUND AFTER 2000 RECORDS')
 1032 FORMAT ('End of file read as expected')
 1033 FORMAT ('TAPE ERROR',I5,' WHILE SEARCHING FOR EOF')
      END
