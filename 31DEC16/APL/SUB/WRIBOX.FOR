      SUBROUTINE WRIBOX (BOXFIL, NBOXES, NFIELD, WIN, IERR)
C-----------------------------------------------------------------------
C! Writes BOXFILE text file of Clean windows
C# Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2002, 2009, 2012
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
C   Writes a text file with the Clean boxes in a suitable format to be
C   put back into IMAGR et al
C   Inputs:
C      BOXFIL   C*48            File name - created if new,
C                               over-written if old
C      NBOXES   I(NFIELD)       Number of boxes in field
C      NFIELD   I               Number of fields
C      WIN      I(4,NFIELD,*)   Clean windows
C   Outputs:
C      IERR     I               Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*)
      INTEGER   NBOXES(*), NFIELD, WIN(4,NFIELD,*), IERR
C
      INTEGER   NF, NB, LUN, LUN2, FIND, FIND2, MSGSAV, ABOX(4), I, J,
     *   COLP, JTRIM, IROUND, KBP, II
      CHARACTER SCNAME*256, LINE*32, TXLINE*132, ULOG*24, UNAME*48,
     *   TMLINE*132
      LOGICAL   SAVEBU
      DOUBLE PRECISION X
      REAL      RDUM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN, LUN2 /11, 10/
C-----------------------------------------------------------------------
      IERR = 0
      SCNAME = ' '
      IF ((NFIELD.GT.0) .AND. (BOXFIL.NE.' ')) THEN
         SAVEBU = .FALSE.
C                                       is there a file
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, BOXFIL, .FALSE., IERR)
         MSGSUP = MSGSAV
C                                       back it up
         IF (IERR.EQ.0) THEN
            COLP = INDEX (BOXFIL, ':')
            IF (COLP.LE.1) THEN
               CALL ZFULLN (' ', 'FITS', 'FILEBOX', SCNAME, IERR)
            ELSE
               ULOG = BOXFIL(:COLP-1)
               UNAME = BOXFIL(COLP+1:)
               CALL ZFULLN (' ', ULOG, UNAME, SCNAME, IERR)
               END IF
            IF (IERR.NE.0) GO TO 970
            CALL ZTXOPN ('QWRT', LUN2, FIND2, SCNAME, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 970
            DO 10 I = 1,100000
               CALL ZTXIO ('READ', LUN, FIND, TXLINE, IERR)
               IF (IERR.EQ.2) GO TO 20
               IF (IERR.NE.0) GO TO 965
               J = JTRIM (TXLINE)
               J = MAX (1, J)
               CALL ZTXIO ('WRIT', LUN2, FIND2, TXLINE(:J), IERR)
               IF (IERR.NE.0) GO TO 965
 10           CONTINUE
 20        SAVEBU = .TRUE.
C                                       close both
           CALL ZTXCLS (LUN, FIND, IERR)
           CALL ZTXCLS (LUN2, FIND2, IERR)
C                                       this is less dangerous!
           CALL ZTXZAP (LUN, BOXFIL, IERR)
           CALL ZTXOPN ('WRIT', LUN, FIND, BOXFIL, .FALSE., IERR)
           IF (IERR.NE.0) GO TO 971
           CALL ZTXOPN ('QRED', LUN2, FIND2, SCNAME, .FALSE., IERR)
           IF (IERR.NE.0) GO TO 970
C                                       read back backup to new orig.
            DO 40 I = 1,100000
               CALL ZTXIO ('READ', LUN2, FIND2, TXLINE, IERR)
               IF (IERR.EQ.2) GO TO 50
               IF (IERR.NE.0) GO TO 965
               CALL CHTRIM (TXLINE, 132, TMLINE, J)
               KBP = 1
               IF (TMLINE(:1).EQ.' ') GO TO 30
               IF (TMLINE(:1).LT.'0') GO TO 30
               IF (TMLINE(:1).GT.'9') GO TO 30
               CALL GETNUM (TMLINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 965
               RDUM = X
               J = IROUND (RDUM)
               IF ((J.GE.1) .AND. (J.LE.NFIELD)) GO TO 40
C                                       write to output
 30            J = JTRIM (TXLINE)
               J = MAX (1, J)
               CALL ZTXIO ('WRIT', LUN, FIND, TXLINE(:J), IERR)
               IF (IERR.NE.0) GO TO 965
 40            CONTINUE
 50         CALL ZTXCLS (LUN2, FIND2, IERR)
C                                       create a new BOXFILE
         ELSE
            CALL ZTXOPN ('WRIT', LUN, FIND, BOXFIL, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         DO 100 NF = 1,NFIELD
            II = 0
            DO 90 NB = 1,NBOXES(NF)
               CALL COPY (4, WIN(1,NF,NB), ABOX)
               IF ((ABOX(1).NE.0) .AND. (ABOX(2).GT.0) .AND.
     *            (ABOX(3).GT.0) .AND. (ABOX(4).GT.0)) THEN
                  WRITE (LINE,1010) NF, ABOX
                  CALL CHTRIM (LINE, 32, LINE, J)
                  CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IERR)
                  IF (IERR.NE.0) GO TO 970
                  II = II + 1
                  END IF
 90            CONTINUE
C                                       null
            IF (II.LE.0) THEN
               CALL FILL (4, 0, ABOX)
               WRITE (LINE,1010) NF, ABOX
               CALL CHTRIM (LINE, 32, LINE, J)
               CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IERR)
               IF (IERR.NE.0) GO TO 970
               END IF
 100        CONTINUE
         SAVEBU = .FALSE.
         GO TO 970
         END IF
      GO TO 999
C                                        Close downs
 965  CALL ZTXCLS (LUN2, FIND2, IERR)
 970  CALL ZTXCLS (LUN, FIND, IERR)
 971  IF (SCNAME.NE.' ') THEN
         IF (SAVEBU) THEN
            MSGTXT = 'OBOXFILE backup saved in file:'
            CALL MSGWRT (8)
            MSGTXT = SCNAME
            CALL MSGWRT (8)
         ELSE
            CALL ZTXZAP (LUN2, SCNAME, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I4,4I7)
      END
