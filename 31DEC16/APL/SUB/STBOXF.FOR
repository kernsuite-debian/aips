      SUBROUTINE STBOXF (BOXFIL, NSTAR, STPOS, STPARM, IRET)
C-----------------------------------------------------------------------
C! Read Stars cards from a BOXFILE
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2014
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
C   STBOXF reads the BOXFILE for W cards to set weights
C   Input:
C      BOXFIL   C*(*)    Box file name
C   Output:
C      NSTAR    I        Number of Star positions
C      STPOS    D(2,*)   Star RA, Dec
C      STPARM   R(4,*)   Star parameters
C      IRET     I        Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*)
      INTEGER   NSTAR, IRET
      DOUBLE PRECISION STPOS(2,*)
      REAL      STPARM(4,*)
C
      INTEGER   LUN, I, J, FIND, KBP, K
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('QRED', LUN, FIND, BOXFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
      NSTAR = 0
      DO 50 K = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 60
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       A weight line
         CALL CHTRIM (LINE, 132, LINE, J)
         IF ((LINE(:1).EQ.'S') .OR. (LINE(:1).EQ.'s'))  THEN
            KBP = 2
            DO 15 I = 1,2
               STPOS(I,NSTAR+1) = 0.0D0
               DO 10 J = 1,3
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) THEN
                     IF ((I.EQ.1) .AND. (J.EQ.1)) GO TO 50
                     WRITE (MSGTXT,1020) K, (I-1)*3+J
                     CALL MSGWRT (6)
                     IRET = 1
                     GO TO 999
                  ELSE IF (I.EQ.1) THEN
                     STPOS(I,NSTAR+1) = STPOS(I,NSTAR+1) + X /
     *                  (60.0D0 ** (J-1))
                  ELSE IF (J.EQ.1) THEN
                     STPOS(I,NSTAR+1) = X
                  ELSE IF (STPOS(I,NSTAR+1).LT.0.0D0) THEN
                     STPOS(I,NSTAR+1) = STPOS(I,NSTAR+1) - ABS(X) /
     *                  (60.0D0 ** (J-1))
                  ELSE
                     STPOS(I,NSTAR+1) = STPOS(I,NSTAR+1) + X /
     *                  (60.0D0 ** (J-1))
                     END IF
 10               CONTINUE
 15            CONTINUE
            NSTAR = NSTAR + 1
            STPOS(1,NSTAR) = STPOS(1,NSTAR) * 15.0D0
            CALL RFILL (4, 0.0, STPARM(1,NSTAR))
            DO 20 I = 1,4
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  GO TO 50
               ELSE
                  STPARM(I,NSTAR) = X
                  END IF
 20            CONTINUE
            END IF
 50      CONTINUE
C
 60   CALL ZTXCLS (LUN, FIND, IRET)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STBOXF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('STBOXF: PARSING ERROR ON LINE',I4,' FIELD',I2)
      END
