      SUBROUTINE CONV3 (LUN, VOL, FIL, BO, XBUFF1, BUFSZ1,
     *   XBUFF2, BUFSZ2, NX, NY, IERR)
C-----------------------------------------------------------------------
C! Third of four routines to convolve two real images.
C# AP-FFT Math Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2006
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
C   CONV3 = third of four routines to convolve two real images.
C   This routine reads the convolving image file and the transformed
C   image from CONV2, multiplies them and does the first pass at
C   transforming back.  Intermediate results are expected and left in
C   the AP if space permits.
C   Input:
C     LUN(5)      I    LUNs for files
C     VOL(5)      I    Volume numbers for the files.
C     FIL(5)      C*48 Physical names for the files.
C     BO(5)       I    Block offsets for the files.
C     XBUFF1(),XBUFF2()  R    Work buffers for I/O
C     BUFSZ1,BUFSZ2  I    Size in bytes for XBUFF1 and XBUFF2
C     NX,NY          I    Number of grid cells in X and Y of maps.
C   Output:
C     IERR          I    Return error code, 0=>OK, otherwise failed.
C                        3=>image too small to FFT
C     Partially transformed and transposed file left in the AP or
C     on the WORK file if necessary.
C-----------------------------------------------------------------------
      CHARACTER FIL(5)*48, BADOP*4
      INTEGER   LUN(5), VOL(5), BO(5), BUFSZ1, BUFSZ2,
     *   NX, NY, IERR, IWIN(4), NX2, NY2
      REAL      XBUFF1(*), XBUFF2(*)
      INTEGER   FIND1, FIND2, BIND1, BIND2, BADFIL, WIN(4), IER,
     *   J, MPASS, NCOL, JAPWRD, I, IT, HALFNX, INDEX, ONENX, TWONY,
     *   JNDEX, APSIZ, ONENY, NWORD, JDIR, KNDEX, LNDEX
      DOUBLE PRECISION   XNDEX
      LOGICAL   MAP, WAIT, EXCL, ISNOT1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get power of 2 size of AP.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
C                                       Make sure min. dim .ge. 4
      IF ((NX.LT.4) .OR. (NY.LT.4)) THEN
           WRITE (MSGTXT,1000) NX,NY
           CALL MSGWRT (8)
           IERR = 3
           GO TO 999
           END IF
C                                       Determine number of AP loads
C                                       required
      NX2 = NX * 2
      NY2 = NY * 2
      XNDEX = NX2 / (1.0 * APSIZ)
      MPASS = NY2 * XNDEX + 0.0001
      MPASS = MAX (MPASS, 1)
      HALFNX = NX
      ONENX = NX2
      ONENY = NY2
      NWORD = 0
C                                       Open input and work file.
C                                       I/O only on multiple AP loads
      IF (MPASS.GT.1) THEN
         BADFIL = 1
         BADOP = 'OPEN'
         CALL ZOPEN (LUN(1), FIND1, VOL(3), FIL(3), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
         BADFIL = 2
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      BADOP = 'INIT'
C                                       COMPLEX to REAL transform
C                                       INIT read and write files.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NY2 * 2
      WIN(4) = NX
      BADFIL = 1
      NCOL = APSIZ / (NY2 * 2)
      NCOL = MIN (NCOL, NX)
      TWONY = 2 * NY2
      JDIR = -1
C                                       Since Complex=>real do last
C                                       row (NX/2+1) to pack with first.
C                                       For MPASS=1 this was done in
C                                       CONV2.
      INDEX = ONENY * 2
      JNDEX = INDEX
      IF (MPASS.GT.1) THEN
         BADOP = 'INIT'
         IWIN(1) = 1
         IWIN(3) = NY2 * 2
         IWIN(2) = (NX2 / 2) + 1
         IWIN(4) = (NX2 / 2) + 1
         CALL MINIT ('READ', LUN(1), FIND1, IWIN(3), IWIN(4), IWIN,
     *      XBUFF1, BUFSZ1, BO(1), IERR)
         IF (IERR.NE.0) GO TO 900
         BADOP = 'READ'
         CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            IT = NCOL
            GO TO 900
            END IF
         CALL QWR
         CALL QPUT (XBUFF1(BIND1), JNDEX, TWONY, 2)
         CALL QWD
C                                       FFT row
         CALL QCFFT (INDEX, ONENY, JDIR)
         BADOP = 'INIT'
         CALL MINIT ('READ', LUN(1), FIND1, WIN(3), WIN(4), WIN,
     *      XBUFF1, BUFSZ1, BO(1), IERR)
         IF (IERR.NE.0) GO TO 900
         BADFIL = 2
         CALL MINIT ('WRIT', LUN(2), FIND2, WIN(3), WIN(4), WIN,
     *      XBUFF2, BUFSZ2, BO(2), IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
         DO 600 I = 1,MPASS
            DO 560 J = 1,NCOL
               INDEX = (J - 1) * 2
               INDEX = INDEX * ONENY
               JNDEX = INDEX
               IF (MPASS.GT.1) THEN
                  CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     IT = I + J - 1
                     BADFIL = 1
                     BADOP = 'READ'
                     GO TO 900
                     END IF
                  CALL QWR
                  CALL QPUT (XBUFF1(BIND1), JNDEX, TWONY, 2)
                  CALL QWD
C                                       Row loaded, do FFT.
                  CALL QWR
                  END IF
C                                       Don't do FFT if it was done in
C                                       CONV2.
               ISNOT1 = (I*J) .NE.  1
               IF (ISNOT1 .OR. (MPASS.GT.1))
     *            CALL QCFFT (INDEX, ONENY, JDIR)
               IF (.NOT.ISNOT1 .AND. (MPASS.NE.1)) THEN
C                                       Pack first (real) and
C                                       last (imag.)
                  LNDEX = 1
                  KNDEX = TWONY
                  CALL QVMOV (KNDEX, 2, LNDEX, 2, ONENY)
                  END IF
 560           CONTINUE
C                                       Transpose.
            CALL APXPOS (NCOL, NY2, 0, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Write only on multiple AP loads
            IF (MPASS.GT.1) THEN
C                                       Write out AP to work file.
               DO 590 J = 1,NCOL
                  INDEX = (J - 1) * ONENY * 2
                  JNDEX = INDEX
                  CALL MDISK ('WRIT', LUN(2), FIND2, XBUFF2, BIND2,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     IT = I + J - 1
                     BADOP = 'WRIT'
                     BADFIL = 2
                     GO TO 900
                     END IF
                  CALL QGET (XBUFF2(BIND2), JNDEX, TWONY, 2)
                  CALL QWD
 590              CONTINUE
               END IF
C                                       Roll AP if necessary.
C                                       Roll whole AP memory if
C                                       MPASS=1 otherwise none.
            NWORD = 0
            IF (MPASS.EQ.1) NWORD = APSIZ
            CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            IF (IERR.NE.0) GO TO 999
 600        CONTINUE
C                                       Finish write.
C                                       Write only on multiple AP loads
         IF (MPASS.GT.1) THEN
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.NE.0) THEN
               BADFIL = 2
               BADOP = 'FINI'
               GO TO 900
               END IF
            END IF
         GO TO 990
C                                       Errors
 900  WRITE (MSGTXT,1900) BADOP, IERR, FIL(BADFIL)
      IF ((BADOP.EQ.'READ') .OR. (BADOP.EQ.'WRIT')) WRITE (MSGTXT,1901)
     *   BADOP, IERR, IT, FIL(BADFIL)
      CALL MSGWRT (8)
C                                       Close files.
 990  IF ((MPASS.GT.1) .AND. (BADOP.NE.'OPEN')) CALL ZCLOSE (LUN(2),
     *   FIND2, IER)
      IF ((MPASS.GT.1) .AND. (BADOP.NE.'OPEN') .OR. (BADFIL.EQ.2))
     *   CALL ZCLOSE (LUN(1), FIND1, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONV3: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('CONV3: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('CONV3: ',A4,' ERROR',I7,' ROW',I5,' FILE ',A)
      END
