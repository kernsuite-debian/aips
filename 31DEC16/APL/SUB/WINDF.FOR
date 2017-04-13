      SUBROUTINE WINDF (WIN, NBOXES, NFIELD, IMSIZE, BOXFIL, IERR)
C-----------------------------------------------------------------------
C! read clean boxes from text file
C# AP Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2005, 2007-2008
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
C   Fills the WIN array with clean box definitions taken from BOXFIL
C   Inputs:
C      BOXFIL   C*48        User provided file name containing box defs
C      NFIELD   I           Number of fields defined
C   In/Out:
C      WIN      I(4,*,*)    clean boxes - defaulted on in (4,FIELD,BOX)
C      NBOXES   I*(*)       Array containing number of boxes/field
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, WIN(4,NFIELD,*), NBOXES(*)
      INTEGER   IMSIZE(2,*), IERR
C
      INTEGER   LUN, I, J, ISUBF(MXNBOX), IFIELD, FIND, IPARM(5), KBP,
     *   I1, I2, IDD, LIMIT
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      LIMIT = MIN (MXNBOX, MXNBFL / MAX(1,NFIELD))
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                        Enter box parameters from file
      CALL FILL (MXNBOX, 0, ISUBF)
      IDD = 0
      DO 50 I = 1,100000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.2) GO TO 60
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       check for comments
         CALL CHTRIM (LINE, 132, LINE, J)
         IF (LINE(:1).LT.'0') GO TO 50
         IF (LINE(:1).GT.'9') GO TO 50
C                                       parse for 5 integers
C                                       field, blc, trc
         KBP = 1
         CALL FILL (5, 0, IPARM)
         DO 30 J = 1,5
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) THEN
               IF (J.EQ.1) GO TO 50
               WRITE (MSGTXT,1020) I, J
               CALL MSGWRT (6)
               IERR = 1
               GO TO 999
            ELSE
               IF (X.GE.0.0D0) THEN
                  IPARM(J) = X + 0.50D0
               ELSE
                  IPARM(J) = X - 0.50D0
                  END IF
               END IF
 30         CONTINUE
         IFIELD = IPARM(1)
         IF ((IFIELD.GE.1) .AND. (IFIELD.LE.NFIELD)) THEN
            IDD = IDD + 1
            ISUBF(IFIELD) = MAX (ISUBF(IFIELD),0) + 1
            IF (ISUBF(IFIELD).LE.LIMIT) THEN
               J = ISUBF(IFIELD)
C                                       circular
               IF (IPARM(2).LT.0) THEN
                  WIN(1,IFIELD,J) = -1
                  WIN(3,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(4),
     *               IMSIZE(1,IFIELD)-IPARM(3)))
                  WIN(4,IFIELD,J) = MAX (1+IPARM(3), MIN (IPARM(5),
     *               IMSIZE(2,IFIELD)-IPARM(3)))
                  WIN(2,IFIELD,J) = IPARM(3)
C                                       rectangular
               ELSE IF (IPARM(2).GT.0) THEN
                  WIN(1,IFIELD,J) = MAX (1, MIN (IPARM(2),
     *               IMSIZE(1,IFIELD)))
                  WIN(3,IFIELD,J) = MAX (1, MIN (IPARM(4),
     *               IMSIZE(1,IFIELD)))
                  IF (WIN(3,IFIELD,J).LT.WIN(1,IFIELD,J)) THEN
                     I1 = WIN(3,IFIELD,J)
                     WIN(3,IFIELD,J) = WIN(1,IFIELD,J)
                     WIN(1,IFIELD,J) = I1
                     END IF
                  WIN(2,IFIELD,J) = MAX (1, MIN (IPARM(3),
     *               IMSIZE(2,IFIELD)))
                  WIN(4,IFIELD,J) = MAX (1, MIN (IPARM(5),
     *               IMSIZE(2,IFIELD)))
                  IF (WIN(4,IFIELD,J).LT.WIN(2,IFIELD,J)) THEN
                     I1 = WIN(4,IFIELD,J)
                     WIN(4,IFIELD,J) = WIN(2,IFIELD,J)
                     WIN(2,IFIELD,J) = I1
                     END IF
C                                       no boxes this field
               ELSE
                  IF (ISUBF(IFIELD).GT.1) THEN
                     WRITE (MSGTXT,1030) IFIELD
                     CALL MSGWRT (6)
                  ELSE
                     ISUBF(IFIELD) = -1
                     END IF
                  END IF
               END IF
            END IF
 50      CONTINUE
 60   DO 70 I = 1,NFIELD
         IF (ISUBF(I).LT.0) NBOXES(I) = 0
         IF (ISUBF(I).GT.0) NBOXES(I) = MIN (LIMIT, ISUBF(I))
         IF (ISUBF(I).GT.LIMIT) THEN
            WRITE (MSGTXT,1060) I, LIMIT, ISUBF(I)
            CALL MSGWRT (6)
            END IF
 70      CONTINUE
      CALL ZTXCLS (LUN, FIND, I)
      IERR = 0
C
      IF (IDD.GT.0) THEN
         MSGTXT = 'WINDF: Number of clean boxes/field read as follows:'
         CALL MSGWRT (3)
         DO 80 J = 1,NFIELD,4
            I1 = J
            I2 = MIN (NFIELD, I1+3)
            WRITE (MSGTXT,1070) ('Fld', I, NBOXES(I), I = I1,I2)
            CALL MSGWRT (3)
 80         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WINDF: ERROR',I4,1X,A,'ING THE CLEAN BOXES TEXT FILE')
 1020 FORMAT ('WINDF: PARSING ERROR ON LINE',I4,' FIELD',I5)
 1030 FORMAT ('WINDF: FIELD',I5,' ZERO BOXES CODE IGNORED DUE TO',
     *   ' REAL BOXES READ')
 1060 FORMAT ('WARNING: FIELD',I5,' USED FIRST',I5,' OF',I6,
     *   ' BOXES IN FILE')
 1070 FORMAT (4(A3,I5.2,':',I5,2X))
      END
