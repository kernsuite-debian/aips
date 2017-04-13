      SUBROUTINE ZDICCL (LUN, FNAME, IRET)
C-----------------------------------------------------------------------
C! Close a Dicomed file and save or spool it.
C# Z
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
C   Close a Dicomed image file and save it under a user-specified name
C   or send it to the film recorder queue.
C
C   Inputs:
C      LUN          I         AIPS logical unit number for file.
C      FNAME        C*(*)     The name the image file will be stored
C                             under; if blank the file will be
C                             automatically placed in the Dicomed
C                             queue and will not be saved.
C
C   Output:
C      IRET         I         Return status:
C                                0 -> success
C                                anything else -> error
C
C   UNIX version:.
C-----------------------------------------------------------------------
      INTEGER   LUN, IRET
      CHARACTER FNAME*(*)
C                                       Max. length of file name.
      INTEGER    MAXFIL
      PARAMETER (MAXFIL = 256)
C
      CHARACTER FILNAM*(MAXFIL)
C
      INTEGER   I, JRET, XLNB
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Translate logical file name (if
C                                       any).
      IF (ITRIM(FNAME).GT.0) THEN
         I = INDEX (FNAME, ':')
         IF (I.EQ.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1000)
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL ZTRLOG (I, FNAME(1:I), MAXFIL, FILNAM, XLNB, JRET)
         IF (JRET.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT, 1001)
            CALL MSGWRT (8)
            GO TO 999
            END IF
         FILNAM(XLNB+1:) = '/' // FNAME(I+1:)
      ELSE
         FILNAM = ' '
         END IF
C
      CALL ZDICC2 (FILNAM, ITRIM(FILNAM), JRET)
      IF (JRET.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT, 1002) JRET
         CALL MSGWRT (8)
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MALFORMED FILE NAME IN ADVERB OUTPRINT')
 1001 FORMAT ('CAN NOT TRANSLATE LOGICAL FILENAME')
 1002 FORMAT ('CAN NOT OPEN FILE: ZDICC2 ERROR ', I4)
      END
