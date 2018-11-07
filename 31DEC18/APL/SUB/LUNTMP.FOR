      INTEGER FUNCTION LUNTMP (TYPE)
C-----------------------------------------------------------------------
C! Finds an available LUN for temporary use
C# IO
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2013-2014. 2017-2018
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
C   Finds an unused LUN of a type legitimate for your use
C   It should be closed in the calling routine since others may need it
C   Inputs:
C      TYPE     I   Type code: 0 map I/o to disk, 1 non-map I/O to disk
C                   2 => text I/O  may change DEVTAB
C                   3 as 2 but do NOT use standard text LUNs
C   Output:
C      LUNTMP   I   LUN to use - 0 => error
C-----------------------------------------------------------------------
      INTEGER   TYPE
C
      INTEGER   I, MLUN, LUN, IERR, FIND
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LUNTMP = 0
      IF ((TYPE.LT.0) .OR. (TYPE.GT.3)) GO TO 999
      MLUN = 128 - NTAPED
      IF (MLUN.GT.120) MLUN = 120
C                                       binary disk file
      IF (TYPE.LT.2) THEN
         DO 10 I = 1,MLUN
            LUN = MLUN + 1 - I
            IF (DEVTAB(LUN).EQ.0) THEN
               CALL LSERCH ('SRCH', LUN, FIND, .TRUE., IERR)
               IF (IERR.EQ.1) THEN
                  CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
                  IF (IERR.EQ.1) THEN
                     LUNTMP = LUN
                     GO TO 999
                     END IF
                  END IF
               END IF
 10         CONTINUE
C                                       text file
      ELSE
         MLUN = MIN (MLUN, 99)
C                                       try reserved ones
         IF (TYPE.LT.3) THEN
            DO 20 I = 1,MLUN
               LUN = MLUN + 1 - I
               IF (DEVTAB(LUN).EQ.3) THEN
                  CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
                  IF (IERR.EQ.1) THEN
                     LUNTMP = LUN
                     GO TO 999
                     END IF
                  END IF
 20            CONTINUE
            END IF
C                                       force one
         DO 30 I = 1,MLUN
            LUN = MLUN + 1 - I
            IF (DEVTAB(LUN).EQ.0) THEN
               CALL LSERCH ('SRCH', LUN, FIND, .TRUE., IERR)
               IF (IERR.EQ.1) THEN
                  CALL LSERCH ('SRCH', LUN, FIND, .FALSE., IERR)
                  IF (IERR.EQ.1) THEN
                     DEVTAB(LUN) = 3
                     LUNTMP = LUN
                     GO TO 999
                     END IF
                  END IF
               END IF
 30         CONTINUE
         END IF
C
 999  RETURN
      END
