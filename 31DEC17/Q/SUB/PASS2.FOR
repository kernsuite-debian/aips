      SUBROUTINE PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, XBUFF1, BUFSZ1,
     *   XBUFF2, BUFSZ2, NX, NY, FMAX, FMIN, IERR)
C-----------------------------------------------------------------------
C! Second of two routines to FFT an image file.
C# AP-FFT Math Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   PASS2 picks data from the appropriate locations in the work file
C   and completes the transpose.  Then the second transforms are
C   done and the data written to the output file.  Real maps returned
C   in unscrambled order and complex maps in scrambled order.
C   If the map will all fit in the AP PASS1 will have left it
C   fully transposed in the AP and the WORK file will not be read.
C   Note: for Real to Complex transforms, NX/2+1 rows are returned.
C   Inputs:
C      IDIR     I      -1 = reverse (complex to real) transform
C                       1 = forward transform, keeps only real part.
C                       2 = forward transform, keeps only amplitudes.
C                       3 = forward transform, keeps full complex.
C                       4 = forward transform, keeps full, zero at
C                           center of rows.
C                       If FULL is .TRUE. IDIR should be either +1 or -1
C      FULL     L       If .TRUE. then do a complex to complex transform
C                       otherwise half plane complex to real or reverse.
C      LUN      I(3)    LUNs for files
C      VOL      I(3)    Volume numbers for the files.
C      FIL      C(3)*48 Physical names for the files.
C      BO       I(3)    Block offsets for the files.
C      XBUFF1   R(*)    Work buffers for I/O
C      XBUFF2   R(*)    Work buffers for I/O
C      BUFSZ1   I       Size in bytes for XBUFF1
C      BUFSZ2   I       Size in bytes for XBUFF2
C      NX       I       Number of grid cells in X of maps.
C      NY       I       Number of grid cells in Y of maps.
C   Output:
C      FMAX     R       Maximum value in output file.
C                          Only set if FULL = .FALSE.
C      FMIN     R       Minimum value in output file.
C                          Only set if FULL = .FALSE.
C      IERR     I       Return error code, 0=>OK, otherwise failed.
C                          3 => Size less than 4 in one dimension.
C   The Transformed Map/File will be left in File 3.
C-----------------------------------------------------------------------
      INTEGER   IDIR, LUN(3), VOL(3), BO(3), BUFSZ1, BUFSZ2,
     *   NX, NY, IERR
      CHARACTER FIL(3)*48
      REAL      XBUFF1(*), XBUFF2(*), FMAX, FMIN
      LOGICAL   FULL
C
      CHARACTER BADOP*4
      INTEGER   JAPWRD, ITEMP, NSKIP, IWIN(4), IFIRST, NOROW, BLCNT,
     *   JDIR, IAP0, IAP2, LMXMN, LNDEX, KNDEX, NWORD, HALFNX, ONENX,
     *   TWONX, ONENY, TWONY, ONECOL, TWOCOL, ONEROW, TWOROW, NSHOV,
     *   OMPASS, NUM, INDEX2, NXTEMP, JNDEX, INDEX, IKEEP, APSIZ,
     *   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BADFIL, WIN(4), NBUF,
     *   MPASS, IFIN, IER, I, IC, J, K, LROW, JSTART, NCOL, NROW
      LOGICAL   MAP, WAIT, EXCL, OKAY
      REAL      TMAX(3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Determine power of 2 AP size.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
C                                       Make sure min. dim .ge. 4
      IF ((NX.LT.4) .OR. (NY.LT.4)) THEN
           WRITE (MSGTXT,1000) NX, NY
           CALL MSGWRT (8)
           IERR = 3
           GO TO 999
           END IF
C                                       Set a few parameters.
      NCOL = APSIZ / (NY * 2)
      ITEMP = NX / 2
      IF (FULL) ITEMP = NX
      NCOL = MIN (NCOL, ITEMP)
      NROW = APSIZ / NX
      IF (FULL) NROW = NROW / 2
      NROW = MIN (NROW, NY)
      MPASS = NY / NROW
      MPASS = MAX (MPASS, 1)
      HALFNX = NX / 2
      ONENX = NX
      TWONX = 2 * NX
      ONENY = NY
      TWONY = 2 * NY
      OMPASS = MPASS
      ONECOL = NCOL
      TWOCOL = 2 * NCOL
      ONEROW = NROW
      TWOROW = 2 * NROW
      FMAX = -1.0E6
      FMIN = 1.0E6
C                                       Open files
      BADOP = 'OPEN'
      BADFIL = 2
C                                       Read only for multiple AP loads
      IF (MPASS.GT.1) THEN
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL ZOPEN (LUN(3), FIND3, VOL(3), FIL(3), MAP, EXCL, WAIT,
     *   IERR)
      BADFIL = 3
      IF (IERR.NE.0) GO TO 900
      BADOP = 'INIT'
C                                       Determine length of read
      LROW = NROW * 2 * NCOL
      IF ((IDIR.LT.0) .AND. (.NOT.FULL)) GO TO 500
C                                       REAL TO COMPLEX or FULL
C                                       Setup for correct output mode.
         IKEEP = 2
         NUM = NY
         IF (IDIR.GE.3) THEN
            IKEEP = 1
            NUM = NY * 2
            END IF
         NXTEMP = TWONX
         JDIR = 1
         IF (FULL) THEN
            NUM = NY * 2
            IKEEP = 1
            NXTEMP = NX
            JDIR = IDIR
            END IF
C                                       Initialize output.
         WIN(1) = 1
         WIN(2) = 2
         WIN(3) = NUM
         WIN(4) = NX/2
         IF (FULL) THEN
            WIN(2) = 1
            WIN(4) = NX
            OKAY = .TRUE.
C                                       will disk IO work?
         ELSE
            OKAY = (2*NUM).GE.NBPS
            IF ((.NOT.OKAY) .AND. ((2*NUM*(NX/2+1)).GT.BUFSZ2)) THEN
               MSGTXT = 'BUFFER TOO SMALL FOR SHORT ROW FIX'
               CALL MSGWRT (8)
               OKAY = .TRUE.
               END IF
            END IF
         IF (MPASS.GT.1) OKAY = .TRUE.
         IF (OKAY) THEN
            CALL MINIT ('WRIT', LUN(3), FIND3, WIN(3), WIN(4), WIN,
     *         XBUFF2, BUFSZ2, BO(3), IERR)
            IF (IERR.NE.0) GO TO 900
         ELSE
            BIND3 = 1
            END IF
         JSTART = 1
C                                       Trap for row NX/2+1
         IF (.NOT.FULL) JSTART = 2
         DO 200 I = 1,MPASS
C                                       Initialize read with skip.
C                                       Read only for multiple AP loads
            IF (MPASS.GT.1) THEN
               NBUF = 1
               NOROW = MPASS * MPASS
               NSKIP = MPASS
               IFIRST = I
               CALL MINSK (LUN(2), FIND2, LROW, NOROW, IFIRST, NSKIP,
     *            XBUFF1, BUFSZ1, BO(2), NBUF, IERR)
               BADFIL = 2
               BADOP = 'INIT'
               IF (IERR.NE.0) GO TO 900
C                                       Determine number of words per
C                                       read.
               NSHOV = LROW / NBUF
               BLCNT = 0
C                                       Load NCOL columns into AP,
C                                       complete transpose.
               DO 60 J = 1,MPASS
                  IC = -1
C                                       Return to here if multiple
C                                       buffering used.
 50                  CALL MSKIP (LUN(2), FIND2, XBUFF1, BIND2, IFIN,
     *                  IERR)
                     IC = IC + 1
                     IF (IERR.NE.0) THEN
                        BADOP = 'READ'
                        GO TO 900
                        END IF
C                                       Load in proper place in AP.
                     INDEX = (J-1) * LROW + NSHOV * IC
                     CALL QPUT (XBUFF1(BIND2), INDEX, NSHOV, 2)
C                                       If read not finished loop.
                     IF (IFIN.NE.0) GO TO 50
 60               CONTINUE
C                                       Finish Transpose.
               CALL QWD
               CALL QVTRAN (OMPASS, ONECOL, 0, TWOROW)
               END IF
            CALL QWAIT
C                                       Do col transform, dump to disk
            DO 100 J = JSTART,NCOL
               INDEX = (J-1) * 2
               INDEX = INDEX * NY
               JNDEX = INDEX
C                                       FFT
               CALL QCFFT (INDEX, ONENY, JDIR)
C                                       if IDIR=4 swap ends
               IF (IDIR.EQ.4) THEN
                  INDEX2 = INDEX + ONENY
                  CALL QVSWAP (INDEX, 1, INDEX2, 1, ONENY)
                  END IF
C                                       If IDIR=2 convert to amplitudes
               IF (IDIR.EQ.2) THEN
                  CALL QCVMAG (INDEX, 2, INDEX, 2, ONENY)
                  CALL QVSQRT (INDEX, 2, INDEX, 2, ONENY)
                  END IF
C                                       Scale and compress if required.
               IF ((JDIR.GT.0) .AND. (IDIR.NE.4))
     *            CALL QVIDIV (INDEX, IKEEP, NXTEMP, ONENY, INDEX, 1,
     *               NUM)
               CALL QWR
C                                       Write output
               IF (OKAY) THEN
                  CALL MDISK ('WRIT', LUN(3), FIND3, XBUFF2, BIND3,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     BADOP = 'WRIT'
                     BADFIL = 3
                     GO TO 900
                     END IF
               ELSE
                  BIND3 = BIND3 + NUM
                  END IF
C                                       Dump to disk.
               CALL QGET (XBUFF2(BIND3), JNDEX, NUM, 2)
               JNDEX = JNDEX + NUM
               CALL QWD
               IF (.NOT.FULL) THEN
C                                       Find max. and min. for column
C                                       Get max ampl. for complex
                  IF (IDIR.GE.3) THEN
                     CALL QCVMAG (INDEX, 2, INDEX, 1, ONENY)
                     CALL QVSQRT (INDEX, 1, INDEX, 1, ONENY)
                     END IF
                  INDEX2 = INDEX + 2
                  CALL QMAXMI (INDEX, 1, INDEX, INDEX2, ONENY)
                  CALL QWR
                  END IF
 100           CONTINUE
C                                       Don't find max/min for FULL
            IF (.NOT.FULL) THEN
C                                       Find max. and min. for AP load.
               IAP0 = (JSTART - 1) * TWONY
               IAP2 = IAP0 + 2
               LMXMN = ONECOL + 1 - JSTART
               CALL QMAXMI (IAP0, TWONY, IAP0, IAP2, LMXMN)
               CALL QWR
               CALL QGET (TMAX, IAP0, 3, 2)
               CALL QWD
               FMAX = MAX (TMAX(1), FMAX)
               FMIN = MIN (TMAX(3), FMIN)
C                                       Deal with first and last rows.
               IF (JSTART.NE.1) THEN
                  JSTART = 1
C                                       Copy real of NX/2+1
                  LNDEX = 1
                  KNDEX = TWONY
                  CALL QVMOV (LNDEX, 2, KNDEX, 2, ONENY)
C                                       Clear imag. of 1
                  LNDEX = 1
                  CALL QVCLR (LNDEX, 2, ONENY)
C                                       Clear imag. of NX/2+1
                  LNDEX = KNDEX + 1
                  CALL QVCLR (LNDEX, 2, ONENY)
C                                       FFTs
                  LNDEX = 0
                  CALL QCFFT (LNDEX, ONENY, JDIR)
                  CALL QCFFT (KNDEX, ONENY, JDIR)
                  CALL QWR
C                                       if IDIR=4 swap ends
                  INDEX2 = LNDEX + ONENY
                  IF (IDIR.EQ.4) THEN
                     CALL QVSWAP (LNDEX, 1, INDEX2, 1, ONENY)
                     INDEX2 = KNDEX + ONENY
                     CALL QVSWAP (KNDEX, 1, INDEX2, 1, ONENY)
                     END IF
C                                       If IDIR=2 convert to amplitudes
                  IF (IDIR.EQ.2) THEN
                     CALL QCVMAG (LNDEX, 2, LNDEX, 2, ONENY)
                     CALL QVSQRT (LNDEX, 2, LNDEX, 2, ONENY)
                     CALL QCVMAG (KNDEX, 2, KNDEX, 2, ONENY)
                     CALL QVSQRT (KNDEX, 2, KNDEX, 2, ONENY)
                     END IF
C                                       Scale and compress if required.
                  IF ((JDIR.GT.0) .AND. (IDIR.NE.4)) THEN
                     CALL QVIDIV (LNDEX, IKEEP, TWONX, ONENY, LNDEX,
     *                  1, NUM)
                     CALL QVIDIV (KNDEX, IKEEP, TWONX, ONENY, KNDEX,
     *                  1, NUM)
                     END IF
                  CALL QWR
C                                       Setup to write row 1
                  IF (OKAY) THEN
                     IWIN(1) = 1
                     IWIN(2) = 1
                     IWIN(3) = NUM
                     IWIN(4) = 1
C                                       Open output file.
                     BADOP = 'OPEN'
                     CALL ZOPEN (LUN(1), FIND1, VOL(3), FIL(3), MAP,
     *                  EXCL, WAIT, IERR)
                     BADFIL = 3
                     IF (IERR.NE.0) GO TO 900
                  ELSE
                     BIND3 = 1
                     END IF
C                                       Write these two rows.
                  DO 160 J = 1,2
C                                       Init (Use input buffer)
                     IF (OKAY) THEN
                        CALL MINIT ('WRIT', LUN(1), FIND1, IWIN(3),
     *                     IWIN(4), IWIN, XBUFF1, BUFSZ1, BO(3), IERR)
                        IF (IERR.NE.0) THEN
                           BADOP = 'INIT'
                           BADFIL = 3
                           GO TO 900
                           END IF
                        END IF
 120                 INDEX = (J-1) * 2 * NY
                     JNDEX = INDEX
C                                       Copy to disk
                     IF (OKAY) THEN
                        CALL MDISK ('WRIT', LUN(1), FIND1, XBUFF1,
     *                     BIND1, IERR)
                        IF (IERR.NE.0) THEN
                           BADOP = 'WRIT'
                           BADFIL = 3
                           GO TO 900
                           END IF
                        CALL QGET (XBUFF1(BIND1), JNDEX, NUM, 2)
                     ELSE
                        CALL QGET (XBUFF2(BIND3), JNDEX, NUM, 2)
                        END IF
                     CALL QWD
C                                       Convert to amplitude for IDIR=3
                     IF (IDIR.EQ.3) THEN
                        CALL QCVMAG (INDEX, 2, INDEX, 1, ONENY)
                        CALL QVSQRT (INDEX, 1, INDEX, 1, ONENY)
                        END IF
C                                       Max., min.
                     INDEX2 = INDEX + 2
                     CALL QMAXMI (INDEX, 1, INDEX, INDEX2, ONENY)
                     CALL QWR
                     CALL QGET (TMAX, INDEX, 3, 2)
                     CALL QWD
                     FMAX = MAX (TMAX(1), FMAX)
                     FMIN = MIN (TMAX(3), FMIN)
C                                       Flush I/O
                     IF (OKAY) THEN
                        CALL MDISK ('FINI', LUN(1), FIND1, XBUFF1,
     *                     BIND1, IERR)
                        IF (IERR.NE.0) THEN
                           BADOP = 'FINI'
                           BADFIL = 3
                           GO TO 900
                           END IF
                        END IF
C                                       Set up for row NX/2+1
                     IF (J.LE.1) THEN
                        IWIN(2) = (NX/2) + 1
                        IWIN(4) = (NX/2) + 1
                        IF (.NOT.OKAY) BIND3 = 1 + (NX/2) * NUM
                        END IF
 160                 CONTINUE
C                                       Close file
                  IF (OKAY) CALL ZCLOSE (LUN(1), FIND1, IER)
                  END IF
               END IF
C                                       Roll AP if necessary.
C                                       Since NWORD = 0 will
C                                       at most release/assign AP
 190        IF (MPASS.GT.I)
     *         CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            IF (IERR.NE.0) GO TO 999
 200        CONTINUE
C                                       do full write short rows
         IF (.NOT.OKAY) THEN
            WIN(1) = 1
            WIN(2) = 1
            WIN(3) = NUM
            WIN(4) = NX/2 + 1
            CALL MINIT ('WRIT', LUN(3), FIND3, WIN(3), WIN(4), WIN,
     *         XBUFF2, BUFSZ2, BO(3), IERR)
            IF (IERR.NE.0) GO TO 900
            DO 210 I = 1,WIN(4)
               CALL MDISK ('WRIT', LUN(3), FIND3, XBUFF2, BIND3, IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'WRIT'
                  BADFIL = 3
                  GO TO 900
                  END IF
 210           CONTINUE
            END IF
C                                       Finish write.
         BADOP = ' '
         CALL MDISK ('FINI', LUN(3), FIND3, XBUFF2, BIND3, IERR)
         IF (IERR.NE.0) THEN
            BADOP = 'FINI'
            BADFIL = 3
            GO TO 900
            END IF
         GO TO 990
C                                       COMPLEX TO REAL
 500  CONTINUE
C                                       INIT output file.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
         CALL MINIT ('WRIT', LUN(3), FIND3, NX, NY, WIN, XBUFF2, BUFSZ2,
     *      BO(3), IERR)
         BADFIL = 3
         IF (IERR.NE.0) GO TO 900
C                                       Begin reading and processing.
         DO 600 I = 1,MPASS
C                                       Read only for multiple AP loads
            IF (MPASS.GT.1) THEN
C                                       Initialize read with skip.
C                                       Unscramble.
               IFIRST = I - MPASS / 2
               IF (IFIRST.LE.0) IFIRST = IFIRST + MPASS
               NBUF = 1
               NOROW = MPASS * MPASS
               NSKIP = MPASS
               CALL MINSK (LUN(2), FIND2, LROW, NOROW, IFIRST, NSKIP,
     *            XBUFF1, BUFSZ1, BO(2), NBUF, IERR)
               BADFIL = 2
               BADOP = 'INIT'
               IF (IERR.NE.0) GO TO 900
C                                       Determine number of words per
C                                       read.
               NSHOV = LROW / NBUF
               BLCNT = 0
               DO 560 J = 1,MPASS
                  IC = -1
C                                       Load NROW rows into the AP
C                                       Return to here if multiple reads
 540                 CALL MSKIP (LUN(2), FIND2, XBUFF1, BIND2, IFIN,
     *                  IERR)
                     IC = IC + 1
                     IF (IERR.NE.0) THEN
                        BADOP = 'READ'
                        GO TO 900
                        END IF
                     INDEX = (J-1) * LROW + NSHOV * IC
                     CALL QPUT (XBUFF1(BIND2), INDEX, NSHOV, 2)
                     CALL QWD
C                                       If read not finished, Loop.
                     IF (IFIN.NE.0) GO TO 540
 560              CONTINUE
C                                       Finish transpose.
               CALL QVTRAN (OMPASS, ONEROW, 0, TWOCOL)
               END IF
C                                       Do row transform and unscramble
C                                       data to disk.
            DO 590 J = 1,NROW
               K = J
C                                       Following to scramble if MPASS=1
               IF (MPASS.GT.1) GO TO 580
                  K = J - NY / 2
                  IF (J.LE.NY/2) K = J + NY / 2
 580           INDEX = (K - 1) * ONENX
C                                       Do FFT.
               CALL QRFFT (INDEX, ONENX, -1)
C                                        Unscramble.
               INDEX2 = INDEX + HALFNX
               CALL QVSWAP (INDEX, 1, INDEX2, 1, HALFNX)
               CALL MDISK ('WRIT', LUN(3), FIND3, XBUFF2, BIND3, IERR)
               IF (IERR.EQ.0) GO TO 585
                  BADOP = 'WRIT'
                  BADFIL = 3
                  GO TO 900
C                                       Get row.
 585           CALL QWR
               CALL QGET (XBUFF2(BIND3), INDEX, ONENX, 2)
               CALL QWD
C                                        Find row max.-min.
               INDEX2 = INDEX + 2
               CALL QMAXMI (INDEX, 1, INDEX, INDEX2, ONENX)
C
 590           CONTINUE
C                                       Find max and min for AP load.
            CALL QMAXV (0, ONENX, 0, ONEROW)
            CALL QMINV (2, ONENX, 2, ONEROW)
            CALL QWR
            CALL QGET (TMAX, 0, 3, 2)
            CALL QWD
            FMAX = MAX (TMAX(1), FMAX)
            FMIN = MIN (TMAX(3), FMIN)
C                                       Roll AP if necessary.
C                                       Since NWORD = 0 will
C                                       at most release/assign AP
            IF (MPASS.GT.I)
     *         CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            IF (IERR.NE.0) GO TO 999
 600        CONTINUE
C                                       Finish write.
         BADOP = '    '
         CALL MDISK ('FINI', LUN(3), FIND3, XBUFF2, BIND3, IERR)
         IF (IERR.EQ.0) GO TO 990
            BADOP = 'FINI'
            BADFIL = 3
C                                       Error report
 900  WRITE (MSGTXT,1900) BADOP, IERR, FIL(BADFIL)
      IF ((BADOP.EQ.'READ') .OR. (BADOP.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1901) BADOP, IERR, I, J
         CALL MSGWRT (8)
         WRITE (MSGTXT,1902) FIL(BADFIL)
         END IF
      CALL MSGWRT (8)
C                                       Close files
 990  IF ((MPASS.GT.1) .AND. ((BADOP.NE.'OPEN') .OR. (BADFIL.NE.2)))
     *   CALL ZCLOSE (LUN(2), FIND2, IER)
      IF (BADOP.NE.'OPEN') CALL ZCLOSE (LUN(3), FIND3, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PASS2: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('PASS2: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('PASS2: ',A4,' ERROR',I7,' I,J=',I3,I5)
 1902 FORMAT ('   FILE = ',A)
      END
