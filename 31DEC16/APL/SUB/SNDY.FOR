      SUBROUTINE SNDY (NSCR, SCRVOL, SCRFIL)
C-----------------------------------------------------------------------
C! closes all files, then deletes all scratch files
C# IO-util
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
C   SNDY closes all possible files.  It then destroys the specified
C   (scratch) files.
C   Inputs:
C      NSCR     I            Number of scratch files to be destroyed
C      SCRVOL   I(NSCR)      Scratch file disk numbers
C      SCRFIL   C*48(NSCR)   Scratch file physical names.
C   Programmer = W. D. Cotton, August 1980.
C-----------------------------------------------------------------------
      CHARACTER  SCRFIL(*)*48
      INTEGER   NSCR, SCRVOL(*)
C
      INTEGER   IERR, IFIND, LUNMSG, I, J
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE., .FALSE./
      DATA LUNMSG /12/
C-----------------------------------------------------------------------
C                                       Close all files in FTAB.
      J = LUNMSG + 1
      DO 20 I = J,50
         CALL LSERCH ('SRCH', I, IFIND, F, IERR)
         IF (IERR.NE.0) CALL LSERCH ('SRCH', I, IFIND, T, IERR)
         IF (IERR.EQ.0) CALL ZCLOSE (I, IFIND, IERR)
   20    CONTINUE
      J = LUNMSG - 1
      DO 40 I = 1,J
         CALL LSERCH ('SRCH', I, IFIND, F, IERR)
         IF (IERR.NE.0) CALL LSERCH ('SRCH', I, IFIND, T, IERR)
         IF (IERR.EQ.0) CALL ZCLOSE (I, IFIND, IERR)
   40    CONTINUE
C                                       Destroy scratch files.
      IF (NSCR.LE.0) GO TO 999
         DO 50 I = 1,NSCR
            CALL ZDESTR (SCRVOL(I), SCRFIL(I), IERR)
            IF (IERR.EQ.0) GO TO 50
               WRITE (MSGTXT,1050) IERR, SCRFIL(I)
               CALL MSGWRT (8)
 50         CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT('SNDY: ERROR',I3,' COULD NOT DESTROY FILE ',A40)
      END
