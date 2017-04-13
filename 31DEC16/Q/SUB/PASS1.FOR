      SUBROUTINE PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, XBUFF1, BUFSZ1,
     *   XBUFF2, BUFSZ2, NX, NY, IERR)
C-----------------------------------------------------------------------
C! First of two routines to FFT an image file.
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
C   PASS1 = first pass of a 2-dimensional disk-based AP FFT  Several
C   rows are loaded into the AP, FFTed and then partially transposed
C   and written on the work file.  If the entire map will fit into the
C   AP the intermediate results are not written to the WORK file.
C   Note: for Complex to Real transforms, NX/2+1 rows are expected.
C   Input:
C     IDIR        I    -1 = reverse transform
C                       1 = forward transform, keep real part only.
C                       2 = forward transform, keep amplitudes only,
C                       3 = forward transform, keep full complex.
C                       (In this step, no difference between 1,2,3)
C     FULL        L    If .TRUE. then COMPLEX to COMPLEX transform,
C                      otherwise, half plane complex to real or reverse
C     LUN(3)      I    LUNs for files
C     VOL(3)      I    Volume numbers for the files.
C     FIL(3)      C*48 Physical names for the files.
C     BO(3)       I    Block offsets for the files.
C     XBUFF1(),XBUFF2()  R    Work buffers for I/O
C     BUFSZ1,BUFSZ2  I    Size in bytes for XBUFF1 and XBUFF2
C     NX,NY          I    Number of grid cells in X and Y of maps.
C   Output:
C     IERR          I    Return error code, 0=>OK, otherwise failed.
C                        3=>image too small to FFT
C     Partially transformed and transposed file left in the AP or
C     on the WORK file if necessary.
C-----------------------------------------------------------------------
      CHARACTER    FIL(3)*48, BADOP*4
      INTEGER   IDIR, LUN(3), VOL(3), BO(3), BUFSZ1, BUFSZ2,
     *   NX, NY, IHALF, IERR, IWIN(4), APSIZ
      REAL      XBUFF1(*), XBUFF2(*)
      INTEGER   FIND1, FIND2, BIND1, BIND2, BADFIL, WIN(4), IER,
     *   J, K, MPASS, NCOL, NROW, JAPWRD, ITEMP, I, ILIM, IT,
     *   HALFNX, INDEX, ONENX, TWONY, ONEROW, JNDEX, ONENY, NWORD, JDIR,
     *   KNDEX, LNDEX
      DOUBLE PRECISION XNDEX
      LOGICAL   FULL, MAP, WAIT, EXCL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
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
      XNDEX = APSIZ
      XNDEX = NX / XNDEX
      IF (FULL) XNDEX = XNDEX * 2.0D0
      MPASS = NY * XNDEX + 0.1
      MPASS = MAX (MPASS, 1)
      HALFNX = NX / 2
      ONENX = NX
      ONENY = NY
      NWORD = 0
C                                       Open input and work file.
      BADFIL = 1
      BADOP = 'OPEN'
      CALL ZOPEN (LUN(1), FIND1, VOL(1), FIL(1), MAP, EXCL, WAIT,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                        Writeonly on multiple AP loads
      IF (MPASS.GT.1) THEN
C                                        DEBUG WARNING MESSAGE
         MSGTXT = '***********************************************'
         CALL MSGWRT (9)
         MSGTXT = 'PASS1: USING MORE THAN ONE PASS!'
         CALL MSGWRT (9)
         MSGTXT = '***********************************************'
         CALL MSGWRT (9)
         BADFIL = 2
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      BADOP = 'INIT'
C                                       REAL TO COMPLEX, scramble data.
      IF ((IDIR.LT.0) .OR. FULL) GO TO 500
C                                       INIT output file.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
C                                        Write only for multiple AP
C                                        loads.
         IF (MPASS.LE.1) GO TO 35
            CALL MINIT ('WRIT', LUN(2), FIND2, NX, NY, WIN, XBUFF2,
     *         BUFSZ2, BO(2), IERR)
            IF (IERR.NE.0) GO TO 900
            WIN(2) = NY / 2 + 1
C                                       NROW = no. rows loaded at once.
 35      NROW = APSIZ / NX
         NROW = MIN (NROW, NY)
         ONEROW = NROW
C                                       Load second half first.
C                                       Jump to here for first half.
 40      CONTINUE
C                                       INIT Input file.
            CALL MINIT ('READ', LUN(1), FIND1, NX, NY, WIN, XBUFF1,
     *         BUFSZ1, BO(1), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 1
               GO TO 900
               END IF
            ILIM = MPASS / 2
            ILIM = MAX (ILIM, 1)
            DO 120 I = 1,ILIM
               DO 80 J = 1,NROW
                  CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     IT = (I - 1) * NROW + WIN(3) + J - 1
                     BADOP = 'READ'
                     BADFIL = 1
                     GO TO 900
                     END IF
                  K = J
C                                       Following to scramble input
C                                       if MPASS = 1.
                  IF (MPASS.GT.1) GO TO 70
                     K = J - NY / 2
                     IF (J.LE.NY/2) K = J + NY / 2
 70               INDEX = (K - 1) * ONENX
C                                       Load data in AP, scramble.
                  CALL QWR
                  CALL QPUT (XBUFF1(BIND1+NX/2), INDEX, HALFNX, 2)
                  JNDEX = INDEX + HALFNX
                  CALL QPUT (XBUFF1(BIND1), JNDEX, HALFNX, 2)
                  CALL QWD
C                                       Row loaded, do FFT.
                  CALL QRFFT (INDEX, ONENX, 1)
 80               CONTINUE
               CALL QWR
C                                       Transpose.
               IHALF = HALFNX
               CALL APXPOS (NROW, IHALF, 0, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Write out from AP to work file.
C                                       Writeonly on multiple AP loads.
               IF (MPASS.LE.1) GO TO 120
                  DO 110 J = 1,NROW
                     CALL MDISK ('WRIT', LUN(2), FIND2, XBUFF2, BIND2,
     *                  IERR)
                     IF (IERR.NE.0) THEN
                        IT = WIN(2) + I + J - 1
                        BADOP = 'WRIT'
                        BADFIL = 2
                        GO TO 900
                        END IF
                     INDEX = (J - 1) * ONENX
                     CALL QGET (XBUFF2(BIND2), INDEX, ONENX, 2)
                     CALL QWD
 110                 CONTINUE
C                                       Roll AP if necessary.
C                                       Roll whole AP memory if
C                                       MPASS=1 otherwise none.
 115           NWORD = 0
               IF (MPASS.EQ.1) NWORD = APSIZ
               CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Reset NWORD
               NWORD = 0
 120           CONTINUE
C                                       Check to see if finished.
            IF (WIN(2).EQ.1) GO TO 130
               WIN(2) = 1
               GO TO 40
C                                       Writeonly on multiple AP loads
 130     IF (MPASS.LE.1) GO TO 990
            BADFIL = 2
            BADOP = 'FINI'
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.NE.0) GO TO 900
            GO TO 990
C                                       COMPLEX to REAL or FULL
C                                       COMPLEX transform.
 500  CONTINUE
C                                       INIT read and write files.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NY * 2
         WIN(4) = NX / 2
         IF (FULL) WIN(4) = NX
         BADFIL = 1
         NCOL = APSIZ / (NY * 2)
         ITEMP = NX / 2
         IF (FULL) ITEMP = NX
         NCOL = MIN (NCOL, ITEMP)
         TWONY = 2 * NY
         JDIR = -1
         IF (FULL) JDIR = IDIR
C                                       If Complex=>real then do last
C                                       row (NX/2+1) to pack with first.
         IF (.NOT.FULL) THEN
            BADOP = 'INIT'
            IWIN(1) = 1
            IWIN(3) = NY * 2
            IWIN(2) = (NX / 2) + 1
            IWIN(4) = (NX / 2) + 1
            CALL MINIT ('READ', LUN(1), FIND1, IWIN(3), IWIN(4), IWIN,
     *         XBUFF1, BUFSZ1, BO(1), IERR)
            IF (IERR.NE.0) GO TO 900
            BADOP = 'READ'
            INDEX = ONENY * 2
            JNDEX = INDEX
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
            END IF
         BADOP = 'INIT'
         CALL MINIT ('READ', LUN(1), FIND1, WIN(3), WIN(4), WIN, XBUFF1,
     *      BUFSZ1, BO(1), IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Writeonly on multiple AP loads
         IF (MPASS.GT.1) THEN
            BADFIL = 2
            CALL MINIT ('WRIT', LUN(2), FIND2, WIN(3), WIN(4), WIN,
     *         XBUFF2, BUFSZ2, BO(2), IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         DO 600 I = 1,MPASS
            DO 560 J = 1,NCOL
               INDEX = (J-1) * 2 * ONENY
               JNDEX = INDEX
               CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1, IERR)
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
               CALL QCFFT (INDEX, ONENY, JDIR)
               IF ((I.GT.1) .OR. (J.GT.1) .OR. FULL) GO TO 560
C                                       Pack first (real) and
C                                       last (imag.)
                  LNDEX = 1
                  KNDEX = TWONY
                  CALL QVMOV (KNDEX, 2, LNDEX, 2, ONENY)
 560           CONTINUE
C                                       Transpose.
            CALL APXPOS (NCOL, NY, 0, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Writeonly on multiple AP loads
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
C                                       Reset NWORD
               NWORD = 0
 600        CONTINUE
C                                       Finish write.
C                                       Writeonly on multiple AP loads
         IF (MPASS.LE.1) GO TO 990
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.EQ.0) GO TO 990
               BADFIL = 2
               BADOP = 'FINI'
               GO TO 900
C                                       Errors
 900  WRITE (MSGTXT,1900) BADOP, IERR, FIL(BADFIL)
      IF ((BADOP.EQ.'READ') .OR. (BADOP.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1901) BADOP, IERR, IT
         CALL MSGWRT (8)
         WRITE (MSGTXT,1902) FIL(BADFIL)
         END IF
      CALL MSGWRT (8)
C                                       Close files.
 990  IF ((MPASS.GT.1) .AND. (BADOP.NE.'OPEN')) CALL ZCLOSE (LUN(2),
     *   FIND2, IER)
      IF ((BADOP.NE.'OPEN') .OR. (BADFIL.EQ.2)) CALL ZCLOSE (LUN(1),
     *   FIND1, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PASS1: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('PASS1: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('PASS1: ',A4,' ERROR',I7,' ROW',I5)
 1902 FORMAT ('   FILE = ',A)
      END
