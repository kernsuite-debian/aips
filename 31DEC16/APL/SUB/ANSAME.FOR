      SUBROUTINE ANSAME (IUDISK, IUCNO, CATBLK, ALLSUB)
C-----------------------------------------------------------------------
C! Do all antenna files match
C# EXT-util UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2009
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
C   checks that all antenna files are the same.
C   Inputs:
C      IUDISK   I   Disk number
C      IUCNO    I   Catalog number
C      CATBLK   I(256)   Catablog block
C   Output:
C      ALLSUB   L   True => they are the same
C-----------------------------------------------------------------------
      INTEGER   IUDISK, IUCNO, CATBLK(256)
      LOGICAL   ALLSUB
C
      INTEGER   NUMSUB, IS, IA, ANBUFF(512,2), ILUN(2), IERR, NANT
C                                       Modified include DANT: 2 bufs
      INCLUDE 'INCS:PUVD.INC'
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC,2), ANNUMV(MAXANC,2), NUMORB(2),
     *   NOPCAL(2), ANTNIF(2), ANFQID(2), IANRNO(2)
      CHARACTER ANAME(2)*8, RDATE(2)*8,TIMSYS(2)*8, XYZHAN(2)*8,
     *   TFRAME(2)*8
      REAL      POLRXY(2,2), UT1UTC(2), DATUTC(2)
      DOUBLE PRECISION  ARRAYC(3,2), GSTIA0(2), DEGPDY(2), SAFREQ(2)
C                                       Declarations for TABAN
      INTEGER   NOSTA(2), MNTSTA(2)
      CHARACTER ANNAME(2)*8, POLTYA(2)*2, POLTYB(2)*2
      REAL      STAXOF(2), POLAA(2), POLCA(2*MAXIF,2), POLAB(2),
     *   POLCB(2*MAXIF,2), DIAMAN(2), FWHMAN(MAXIF,2)
      DOUBLE PRECISION  STAXYZ(3,2), ORBPRM(6,2)
C                                       End modified include.
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /110,111/
C-----------------------------------------------------------------------
C                                       is there a question
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
C                                       yes: compare #1 with each
      IF (NUMSUB.GT.1) THEN
         DO 100 IS = 2,NUMSUB
            IA = 0
            CALL ANTINI ('READ', ANBUFF(1,2), IUDISK, IUCNO, IS,
     *         CATBLK, ILUN(2), IANRNO(2), ANKOLS(1,2), ANNUMV(1,2),
     *         ARRAYC(1,2), GSTIA0(2), DEGPDY(2), SAFREQ(2), RDATE(2),
     *         POLRXY(1,2), UT1UTC(2), DATUTC(2), TIMSYS(2), ANAME(2),
     *         XYZHAN(2), TFRAME(2), NUMORB(2), NOPCAL(2), ANTNIF(2),
     *         ANFQID(2), IERR)
            IF (IERR.EQ.2) GO TO 100
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'INIT', IS
               GO TO 990
               END IF
            CALL ANTINI ('READ', ANBUFF(1,1), IUDISK, IUCNO, 1,
     *         CATBLK, ILUN(1), IANRNO(1), ANKOLS(1,1), ANNUMV(1,1),
     *         ARRAYC(1,1), GSTIA0(1), DEGPDY(1), SAFREQ(1), RDATE(1),
     *         POLRXY(1,1), UT1UTC(1), DATUTC(1), TIMSYS(1), ANAME(1),
     *         XYZHAN(1), TFRAME(1), NUMORB(1), NOPCAL(1), ANTNIF(1),
     *         ANFQID(1), IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'INIT', 1
               GO TO 980
               END IF
C                                       Headers match?
            IF (ABS(ARRAYC(1,1)-ARRAYC(1,2)).GT.1.0D0) GO TO 900
            IF (ABS(ARRAYC(2,1)-ARRAYC(2,2)).GT.1.0D0) GO TO 900
            IF (ABS(ARRAYC(3,1)-ARRAYC(3,2)).GT.1.0D0) GO TO 900
            IF (ABS(GSTIA0(1)-GSTIA0(2)).GT.0.1D0) GO TO 900
            IF (ABS(SAFREQ(1)-SAFREQ(2)).GT.0.1D0) GO TO 900
            IF (RDATE(1).NE.RDATE(2)) GO TO 900
            IF (TIMSYS(1).NE.TIMSYS(2)) GO TO 900
            IF (ANAME(1).NE.ANAME(2)) GO TO 900
            IF (NUMORB(1).NE.NUMORB(2)) GO TO 900
            IF (ANTNIF(1).NE.ANTNIF(2)) GO TO 900
            IF (XYZHAN(1).NE.XYZHAN(2)) GO TO 900
            IF (TFRAME(1).NE.TFRAME(2)) GO TO 900
            NANT = ANBUFF(5,1)
            IF (NANT.NE.ANBUFF(5,2)) GO TO 900
C                                       read the an tables
            DO 90 IA = 1,NANT
               CALL TABAN ('READ', ANBUFF(1,1), IANRNO(1), ANKOLS(1,1),
     *            ANNUMV(1,1), ANNAME(1), STAXYZ(1,1), ORBPRM(1,1),
     *            NOSTA(1), MNTSTA(1), STAXOF(1), DIAMAN(1),
     *            FWHMAN(1,1), POLTYA(1), POLAA(1), POLCA(1,1),
     *            POLTYB(1), POLAB(1), POLCB(1,1), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ', 1
                  GO TO 970
                  END IF
               CALL TABAN ('READ', ANBUFF(1,2), IANRNO(2), ANKOLS(1,2),
     *            ANNUMV(1,2), ANNAME(2), STAXYZ(1,2), ORBPRM(1,2),
     *            NOSTA(2), MNTSTA(2), STAXOF(2), DIAMAN(1),
     *            FWHMAN(1,2), POLTYA(2), POLAA(2), POLCA(1,2),
     *            POLTYB(2), POLAB(2), POLCB(1,2), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ', IS
                  GO TO 970
                  END IF
C                                       compare
               IF (ANNAME(1).NE.ANNAME(2)) GO TO 900
               IF (ABS(STAXYZ(1,1)-STAXYZ(1,2)).GT.1.0D0) GO TO 900
               IF (ABS(STAXYZ(2,1)-STAXYZ(2,2)).GT.1.0D0) GO TO 900
               IF (ABS(STAXYZ(3,1)-STAXYZ(3,2)).GT.1.0D0) GO TO 900
               IF (ABS(DIAMAN(1)-DIAMAN(2)).GT.0.1) GO TO 900
               IF (NOSTA(1).NE.NOSTA(2)) GO TO 900
               IF (MNTSTA(1).NE.MNTSTA(2)) GO TO 900
               IF (POLTYA(1).NE.POLTYA(2)) GO TO 900
               IF (POLTYB(1).NE.POLTYB(2)) GO TO 900
 90            CONTINUE
            CALL TABIO ('CLOS', 0, IANRNO(1), ANBUFF(1,1), ANBUFF(1,1),
     *         IERR)
            CALL TABIO ('CLOS', 0, IANRNO(2), ANBUFF(1,2), ANBUFF(1,2),
     *         IERR)
 100        CONTINUE
         ALLSUB = .TRUE.
         END IF
      GO TO 999
C
 900  WRITE (MSGTXT,1900) IS, IA
 970  CALL MSGWRT (6)
      CALL TABIO ('CLOS', 0, IANRNO(1), ANBUFF(1,1), ANBUFF(1,1), IERR)
      CALL TABIO ('CLOS', 0, IANRNO(2), ANBUFF(1,2), ANBUFF(1,2), IERR)
      GO TO 999
C
 980  CALL MSGWRT (6)
      CALL TABIO ('CLOS', 0, IANRNO(2), ANBUFF(1,2), ANBUFF(1,2), IERR)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANSAME: ERROR',I5,1X,A,'ING AN TABLE',I4)
 1900 FORMAT ('ANSAME: AN TABLE',I4,' DOES NOT MATCH AN TABLE 1 AT',
     *   ' ANTENNA',I3)
      END
