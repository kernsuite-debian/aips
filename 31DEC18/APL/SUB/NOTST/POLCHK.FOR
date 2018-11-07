      SUBROUTINE POLCHK (PPLOT, POLPLT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007, 2018
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
C  Routine to decide of the requested polzn plot type is valid.
C  Input:
C    PPLOT      C*8      requested pol operation, can be one of:
C                        ' ', 'RL/RR', 'LR/RR', 'LL/RR'
C                             'RL/LL', 'LR/LL', 'RR/LL'
C                             'Q/I', 'U/I', 'V/I'
C                        also linears
C  Output:
C    POLPLT     I        Integer plottype corresponding to the above
C                        0,    1,       2,       3,
C                              4,       5,       6
C                              7      8      9
C                        0 => don't perform the polzn operation.
C
C  Uses FNDPOL to determine if the operation is valid.
C-----------------------------------------------------------------------
      CHARACTER PPLOT*8
      INTEGER   POLPLT
C
      INTEGER   KLEN, IERR, ITRIM
      CHARACTER STKCHR*4
C-----------------------------------------------------------------------
      POLPLT = 0
C                                       Check validity of the string
      KLEN = ITRIM(PPLOT)
C                                       circular feeds
      IF ((KLEN.EQ.5) .AND. (PPLOT(3:3).EQ.'/')) THEN
         IF ((PPLOT(1:2).NE.'RL') .AND. (PPLOT(1:2).NE.'LR') .AND.
     *      (PPLOT(1:2).NE.'RR') .AND. (PPLOT(1:2).NE.'LL') .AND.
     *      (PPLOT(1:2).NE.'XY') .AND. (PPLOT(1:2).NE.'YX') .AND.
     *      (PPLOT(1:2).NE.'XX') .AND. (PPLOT(1:2).NE.'YY')) GO TO 999
         IF ((PPLOT(4:5).NE.'RL') .AND. (PPLOT(4:5).NE.'LR') .AND.
     *      (PPLOT(4:5).NE.'RR') .AND. (PPLOT(4:5).NE.'LL') .AND.
     *      (PPLOT(4:5).NE.'XY') .AND. (PPLOT(4:5).NE.'YX') .AND.
     *      (PPLOT(4:5).NE.'XX') .AND. (PPLOT(4:5).NE.'YY')) GO TO 999
         IF (PPLOT(1:2).EQ.PPLOT(4:5)) GO TO 999
C                                       Set up for FNDPOL
         IF ((PPLOT(1:2).EQ.'RR') .OR. (PPLOT(1:2).EQ.'LL') .OR.
     *      (PPLOT(1:2).EQ.'XX') .OR. (PPLOT(1:2).EQ.'YY')) THEN
            STKCHR = 'HALF'
         ELSE
            STKCHR = 'FULL'
            END IF
         CALL FNDPOL (STKCHR, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set up POLPLT
         IF (STKCHR.EQ.'HALF') THEN
            IF (PPLOT(1:2).EQ.'RR') POLPLT = 6
            IF (PPLOT(1:2).EQ.'LL') POLPLT = 3
            IF (PPLOT(1:2).EQ.'XX') POLPLT = 6
            IF (PPLOT(1:2).EQ.'YY') POLPLT = 3
         ELSE IF (STKCHR.EQ.'FULL') THEN
            IF (PPLOT(4:5).EQ.'RR') THEN
               IF (PPLOT(1:2).EQ.'RL') POLPLT = 1
               IF (PPLOT(1:2).EQ.'LR') POLPLT = 2
            ELSE IF (PPLOT(4:5).EQ.'XX') THEN
               IF (PPLOT(1:2).EQ.'XY') POLPLT = 1
               IF (PPLOT(1:2).EQ.'YX') POLPLT = 2
            ELSE
               IF (PPLOT(1:2).EQ.'RL') POLPLT = 4
               IF (PPLOT(1:2).EQ.'LR') POLPLT = 5
               IF (PPLOT(1:2).EQ.'XY') POLPLT = 4
               IF (PPLOT(1:2).EQ.'YX') POLPLT = 5
               END IF
            END IF
C                                       true stokes
      ELSE IF ((KLEN.EQ.3) .AND. (PPLOT(2:3).EQ.'/I')) THEN
         STKCHR = 'IQUV'
         CALL FNDPOL (STKCHR, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (PPLOT(1:1).EQ.'Q') POLPLT = 7
         IF (PPLOT(1:1).EQ.'U') POLPLT = 8
         IF (PPLOT(1:1).EQ.'V') POLPLT = 9
         END IF
C
 999  RETURN
      END
