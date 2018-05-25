      SUBROUTINE CONV2 (LUN, VOL, FIL, BO, XBUFF1, BUFSZ1, XBUFF2,
     *   BUFSZ2, XBUFF3, BUFSZ3, NX, NY, FACTOR, IERR)
C-----------------------------------------------------------------------
C! Second of four routines to convolve two real images.
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
C   CONV2 = second of four routines to convolve two real images.
C   This routine reads data from the appropriate locations in the work
C   file (or the AP memory if sufficient) and completes the transpose.
C   Then the second transforms are done, the data is multiplied by
C   the convolving function and written to the output file.
C   If the map will all fit in the AP CONV1 will have left it
C   fully transposed in the AP and the WORK file will not be read.
C   If the data is left in memory, rows 1 and NX/2+1 are already
C   processed.
C   Input:
C      LUN      I(5)     LUNs for files
C      VOL      I(5)     Volume numbers for the files.
C      FIL      C(5)*48  Physical names for the files.
C      BO       I(5)     Block offsets for the files.
C      XBUFF1   R        Work buffers for I/O
C      XBUFF2   R        Work buffers for I/O
C      XBUFF3   R        Work buffers for I/O
C      NX       I        Number of grid cells in X of maps.
C      NY       I        Number of grid cells in Y of maps.
C      FACTOR   R        Normalization factor for convolving function;
C                        i.e. is multiplied by the transform of the
C                        convolving fn.
C   Output:
C      BUFSZ1   I        Size in bytes for XBUFF1
C      BUFSZ2   I        Size in bytes for XBUFF2
C      BUFSZ3   I        Size in bytes for XBUFF3
C      IERR     I        Return error code, 0 = >OK,  otherwise failed.
C                           3 => Size less than 4 in one dimension.
C   The Transformed Map/File will be left in File 3.
C-----------------------------------------------------------------------
      CHARACTER FIL(5)*48
      INTEGER   LUN(5), VOL(5), BO(5), BUFSZ1, BUFSZ2, BUFSZ3, NX, NY,
     *   IERR
      REAL      XBUFF1(*), XBUFF2(*), XBUFF3(*), FACTOR
C
      CHARACTER BADOP*4
      INTEGER   JAPWRD,  NSKIP, IWIN(4), IFIRST, NOROW, BLCNT, NX2, NY2,
     *   KOFF, JDIR, LNDEX, KNDEX, NWORD, HALFNX, ONENX, TWONX, ONENY,
     *   TWONY, ONECOL, TWOCOL, ONEROW, TWOROW, NSHOV, OMPASS, NUM,
     *   JNDEX, INDEX, I4TEMP, LOOP, FIND1, FIND2, FIND3, BIND1, BIND2,
     *   BIND3, BADFIL, WIN(4), FIND5, BIND5, APSIZ, NBUF, MPASS,
     *   IFIN, IER, I, IC, J, LROW, JSTART, NCOL, NROW
      REAL      XR, XI
      LOGICAL   MAP, WAIT, EXCL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Determine power of 2 AP size.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
      NWORD = APSIZ
C                                       Make sure min. dim .ge. 4
      IF ((NX.LT.4) .OR. (NY.LT.4)) THEN
           WRITE (MSGTXT,1000) NX,NY
           CALL MSGWRT (8)
           IERR = 3
           GO TO 999
           END IF
C                                       Set a few parameters.
      NX2 = NX * 2
      NY2 = NY * 2
      NCOL = APSIZ / (NY2 * 2.0)
      NCOL = MIN (NCOL, NX)
      NROW = APSIZ / NX2
      NROW = MIN (NROW, NY2)
      MPASS = NY2 / NROW
      MPASS = MAX (MPASS, 1)
      HALFNX = NX
      ONENX = NX2
      TWONX = 2 * NX2
      ONENY = NY2
      TWONY = 2 * NY2
      OMPASS = MPASS
      ONECOL = NCOL
      TWOCOL = 2 * NCOL
      ONEROW = NROW
      TWOROW = 2 * NROW
C                                       Open files
      BADOP = 'OPEN'
      BADFIL = 2
C                                       I/O only for multiple AP loads
      IF (MPASS.GT.1) THEN
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.NE.0) GO TO 900
         CALL ZOPEN (LUN(3), FIND3, VOL(3), FIL(3), MAP, EXCL, WAIT,
     *      IERR)
         BADFIL = 3
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Convolving fn.
      CALL ZOPEN (LUN(5), FIND5, VOL(5), FIL(5), MAP, EXCL, WAIT, IERR)
      BADFIL = 5
      IF (IERR.NE.0) GO TO 900
      BADOP = 'INIT'
C                                       Determine length of read
      LROW = NROW * 2 * NCOL
C                                       Setup for correct output mode.
      NUM = NY2 * 2
      JDIR = 1
C                                       Initialize output.
      WIN(1) = 1
      WIN(2) = 2
      WIN(3) = NUM
      WIN(4) = NX
C                                       Output
      BIND3 = 1
      IF (MPASS.GT.1) CALL MINIT ('WRIT', LUN(3), FIND3, WIN(3),
     *   WIN(4), WIN, XBUFF2, BUFSZ2, BO(3), IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Convolving image
      CALL MINIT ('READ', LUN(5), FIND5, WIN(3), WIN(4), WIN, XBUFF3,
     *   BUFSZ3, BO(5), IERR)
      BADFIL = 5
      IF (IERR.NE.0) GO TO 900
C                                       Trap for row NX/2+1
      JSTART = 2
      DO 300 I = 1,MPASS
C                                       Initialize read with skip.
C                                       Read only for multiple AP loads
         IF (MPASS.GT.1) THEN
C                                       Read only values not
C                                       corresponding to zeroed rows.
C                                       Since they were not written
C                                       just skip.
            NBUF = 1
            NOROW = MPASS * MPASS
            NSKIP = MPASS
            IFIRST = I
            CALL MINSK (LUN(2), FIND2, LROW, NOROW, IFIRST, NSKIP,
     *         XBUFF1, BUFSZ1, BO(2), NBUF, IERR)
            BADFIL = 2
            BADOP = 'INIT'
            IF (IERR.NE.0) GO TO 900
C                                       Determine number of words per re
            NSHOV = LROW / NBUF
            BLCNT = 0
C                                       Load NCOL columns into AP,
C                                       complete transpose.
            DO 60 J = 1,MPASS
               KOFF = J - MPASS/2
               IF (KOFF.LE.0) KOFF = KOFF + MPASS
               IF (((KOFF.LE.(MPASS/4)) .OR. (KOFF.GT.(3*MPASS/4)))
     *            .AND. (MPASS.GE.4)) THEN
C                                       Zero
                  INDEX = (J-1) * LROW
                  I4TEMP = ((1.0 * APSIZ) / MPASS) + 0.1
                  CALL QVCLR (INDEX, 1, I4TEMP)
                  GO TO 60
                  END IF
               IC = -1
C                                       Return to here if multiple
C                                       buffering used.
 50               CALL MSKIP (LUN(2), FIND2, XBUFF1, BIND2, IFIN, IERR)
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
 65         CALL QWAIT
C                                       Do col transform, dump to disk
            DO 100 J = JSTART,NCOL
               INDEX = (J-1) * 2 * NY2
               JNDEX = INDEX
C                                       FFT
               CALL QCFFT (INDEX, ONENY, JDIR)
C                                       Scale.
 75            CALL QVIDIV (INDEX, 1, TWONX, ONENY, INDEX, 1, NUM)
               CALL QWR
C                                       Write output
               IF (MPASS.GT.1) CALL MDISK ('WRIT', LUN(3), FIND3,
     *            XBUFF2, BIND3, IERR)
               IF (IERR.NE.0) THEN
                     BADOP = 'WRIT'
                     BADFIL = 3
                     GO TO 900
                     END IF
C                                       Dump to disk.
               CALL QGET (XBUFF2(BIND3), JNDEX, NUM, 2)
C                                       Read convolving fn
               CALL MDISK ('READ', LUN(5), FIND5, XBUFF3, BIND5, IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'READ'
                  BADFIL = 5
                  GO TO 900
                  END IF
               CALL QWD
C                                       Multiply
               DO 90 LOOP = 1,NUM,2
                  XR = FACTOR * XBUFF2(BIND3+LOOP-1)
                  XI = FACTOR * XBUFF2(BIND3+LOOP)
                  XBUFF2(BIND3+LOOP-1) = XR * XBUFF3(BIND5+LOOP-1) -
     *               XI * XBUFF3(BIND5+LOOP)
                  XBUFF2(BIND3+LOOP) = XR * XBUFF3(BIND5+LOOP) +
     *               XI * XBUFF3(BIND5+LOOP-1)
 90               CONTINUE
C                                       Put back if MPASS=1
               IF (MPASS.EQ.1) CALL QPUT (XBUFF2(BIND3), JNDEX,
     *            NUM, 2)
               IF (MPASS.EQ.1) CALL QWD
               JNDEX = JNDEX + NUM
 100           CONTINUE
C                                       Row NX/2 + 1
C                                       Deal with first and last rows.
C                                       Already packed into AP memory at
C                                       start loc. 0.
            IF (JSTART.EQ.1) GO TO 290
            JSTART = 1
C                                       Unpack
            IF (MPASS.EQ.1) GO TO 200
C                                       Disk based section:
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
C                                       Scale
            CALL QVIDIV (LNDEX, 1, TWONX, ONENY, LNDEX, 1, NUM)
            CALL QVIDIV (KNDEX, 1, TWONX, ONENY, KNDEX, 1, NUM)
            CALL QWR
C                                       Flush, reinit other output
C                                       buffer.
            CALL MDISK ('FINI', LUN(3), FIND3, XBUFF2, BIND3, IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'FINI'
               BADFIL = 3
               GO TO 900
               END IF
            IWIN(1) = WIN(1)
            IWIN(2) = NCOL + 1
            IWIN(3) = WIN(3)
            IWIN(4) = WIN(4)
            CALL MINIT ('WRIT', LUN(3), FIND3, IWIN(3), IWIN(4),
     *         IWIN, XBUFF2, BUFSZ2, BO(3), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 3
               GO TO 900
               END IF
C                                       Setup to write row 1
            IWIN(1) = 1
            IWIN(2) = 1
            IWIN(3) = NUM
            IWIN(4) = 1
C                                       Open output file.
            BADOP = 'OPEN'
            CALL ZOPEN (LUN(1), FIND1, VOL(3), FIL(3), MAP,
     *         EXCL, WAIT, IERR)
            BADFIL = 3
            IF (IERR.NE.0) GO TO 900
C                                       Write these two rows.
            DO 160 J = 1,2
C                                       Init (Use input buffer)
               CALL MINIT ('WRIT', LUN(1), FIND1, IWIN(3),
     *            IWIN(4), IWIN, XBUFF1, BUFSZ1, BO(3), IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'INIT'
                  BADFIL = 3
                  GO TO 900
                  END IF
C                                       Convolving function
               CALL MINIT ('READ', LUN(5), FIND5, IWIN(3),
     *               IWIN(4), IWIN, XBUFF3, BUFSZ3, BO(5), IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'INIT'
                  BADFIL = 5
                  GO TO 900
                  END IF
               INDEX = (J-1) * 2 * NY2
               JNDEX = INDEX
C                                       Copy to disk
               CALL MDISK ('WRIT', LUN(1), FIND1, XBUFF1, BIND1, IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'WRIT'
                  BADFIL = 3
                  GO TO 900
                  END IF
               CALL QGET (XBUFF1(BIND1), JNDEX, NUM, 2)
C                                       Read convolving fn
               CALL MDISK ('READ', LUN(5), FIND5, XBUFF3, BIND5, IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'READ'
                  BADFIL = 5
                  GO TO 900
                  END IF
               CALL QWD
C                                       Multiply
               DO 140 LOOP = 1,NUM,2
                  XR = FACTOR * XBUFF1(BIND1+LOOP-1)
                  XI = FACTOR * XBUFF1(BIND1+LOOP)
                  XBUFF1(BIND1+LOOP-1) =
     *               XR * XBUFF3(BIND5+LOOP-1) -
     *               XI * XBUFF3(BIND5+LOOP)
                  XBUFF1(BIND1+LOOP) =
     *               XR * XBUFF3(BIND5+LOOP) +
     *               XI * XBUFF3(BIND5+LOOP-1)
 140              CONTINUE
               JNDEX = JNDEX + NUM
 145           CONTINUE
C                                       Flush I/O
               IF (MPASS.GT.1) CALL MDISK ('FINI', LUN(1), FIND1,
     *            XBUFF1, BIND1, IERR)
               IF (IERR.NE.0) THEN
                  BADOP = 'FINI'
                  BADFIL = 3
                  GO TO 900
                  END IF
C                                       Set up for row NX/2+1
               IF (J.LE.1) THEN
                  IWIN(2) = NX + 1
                  IWIN(4) = NX + 1
                  END IF
 160           CONTINUE
C                                       Close file
            CALL ZCLOSE (LUN(1), FIND1, IER)
C                                       Reinit Convolving fn.
            WIN(2) = NCOL + 1
            CALL MINIT ('READ', LUN(5), FIND5, WIN(3), WIN(4),
     *         WIN, XBUFF3, BUFSZ3, BO(5), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 5
               GO TO 900
               END IF
            GO TO 290
C                                       Row NX/2+1 AP based:
C                                       Get packed row; use buffers
C                                       for storage.
 200        LNDEX = 0
            CALL QGET (XBUFF2, LNDEX, NUM, 2)
            CALL QWD
C                                       Clear imag. of 1
            LNDEX = 1
            CALL QVCLR (LNDEX, 2, ONENY)
C                                       FFT row 1
            LNDEX = 0
            JDIR = 1
            CALL QCFFT (LNDEX, ONENY, JDIR)
C                                       Scale
            CALL QVIDIV (LNDEX, 1, TWONX, ONENY, LNDEX, 1, NUM)
            CALL QWR
C                                       Get row 1
            CALL QGET (XBUFF1, LNDEX, NUM, 2)
            CALL QWD
C                                       Setup to multiply row 1
            IWIN(1) = 1
            IWIN(2) = 1
            IWIN(3) = NUM
            IWIN(4) = 1
C                                       Convolving function
            CALL MINIT ('READ', LUN(5), FIND5, IWIN(3), IWIN(4),
     *         IWIN, XBUFF3, BUFSZ3, BO(5), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 5
               GO TO 900
               END IF
            INDEX = 1
C                                       Read convolving fn
            CALL MDISK ('READ', LUN(5), FIND5, XBUFF3, BIND5, IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'READ'
               BADFIL = 5
               GO TO 900
               END IF
C                                       Multiply
            DO 220 LOOP = 1,NUM,2
               XR = FACTOR * XBUFF1(INDEX)
               XI = FACTOR * XBUFF1(INDEX+1)
               XBUFF1(INDEX) =
     *            XR * XBUFF3(BIND5+LOOP-1) -
     *            XI * XBUFF3(BIND5+LOOP)
               XBUFF1(INDEX+1) =
     *            XR * XBUFF3(BIND5+LOOP) +
     *            XI * XBUFF3(BIND5+LOOP-1)
               INDEX = INDEX + 2
 220           CONTINUE
C                                       Row NX/2+1
            LNDEX = 0
            CALL QPUT (XBUFF2(2), LNDEX, NUM, 2)
            CALL QWD
C                                       Clear imag. of NX/2+1
            LNDEX = 1
            CALL QVCLR (LNDEX, 2, ONENY)
C                                       FFT row NX/2+1
            LNDEX = 0
            JDIR = 1
            CALL QCFFT (LNDEX, ONENY, JDIR)
C                                       Scale
            CALL QVIDIV (LNDEX, 1, TWONX, ONENY, LNDEX, 1, NUM)
            CALL QWR
C                                       Get row NX/2+1
            CALL QGET (XBUFF2, LNDEX, NUM, 2)
            CALL QWD
C                                       Setup to multiply row NX/2+1
            IWIN(1) = 1
            IWIN(2) = NX + 1
            IWIN(3) = NUM
            IWIN(4) = NX + 1
C                                       Convolving function
            CALL MINIT ('READ', LUN(5), FIND5, IWIN(3), IWIN(4),
     *         IWIN, XBUFF3, BUFSZ3, BO(5), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 5
               GO TO 900
               END IF
            INDEX = 1
C                                       Read convolving fn
            CALL MDISK ('READ', LUN(5), FIND5, XBUFF3, BIND5, IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'READ'
               BADFIL = 5
               GO TO 900
               END IF
C                                       Multiply
            DO 250 LOOP = 1,NUM,2
               XR = FACTOR * XBUFF2(INDEX)
               XI = FACTOR * XBUFF2(INDEX+1)
               XBUFF2(INDEX) =
     *            XR * XBUFF3(BIND5+LOOP-1) -
     *            XI * XBUFF3(BIND5+LOOP)
               XBUFF2(INDEX+1) =
     *            XR * XBUFF3(BIND5+LOOP) +
     *            XI * XBUFF3(BIND5+LOOP-1)
               INDEX = INDEX + 2
 250           CONTINUE
C                                       FFT rows 1 and NX/2+1
            LNDEX = 0
            CALL QPUT (XBUFF1, LNDEX, NUM, 2)
            JDIR = -1
            CALL QWD
            CALL QCFFT (LNDEX, ONENY, JDIR)
            CALL QWR
            CALL QGET (XBUFF1, LNDEX, NUM, 2)
            CALL QWD
C                                       Row NX/2+1
            CALL QPUT (XBUFF2, LNDEX, NUM, 2)
            CALL QWD
            CALL QCFFT (LNDEX, ONENY, JDIR)
            CALL QWR
            CALL QGET (XBUFF2, LNDEX, NUM, 2)
            JDIR = 1
            CALL QWD
C                                       Pack
            DO 270 LOOP = 1,NUM,2
               XBUFF1(LOOP+1) = XBUFF2(LOOP)
 270           CONTINUE
C                                       Leave in AP
            CALL QPUT (XBUFF1, LNDEX, NUM, 2)
            CALL QWAIT
C                                       Roll AP if necessary.
 290        IF (MPASS.GT.1) NWORD = 0
            CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            NWORD = APSIZ
            IF (IERR.NE.0) GO TO 999
 300        CONTINUE
         IF (MPASS.EQ.1) GO TO 990
C                                       Finish write.
         BADOP = '    '
         CALL MDISK ('FINI', LUN(3), FIND3, XBUFF2, BIND3, IERR)
         IF (IERR.NE.0) THEN
            BADOP = 'FINI'
            BADFIL = 3
            GO TO 900
            END IF
         GO TO 990
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
      IF ((MPASS.GT.1) .AND. ((BADOP.NE.'OPEN') .OR. (BADFIL.NE.3)))
     *   CALL ZCLOSE (LUN(3), FIND3, IER)
      IF ((BADOP.NE.'OPEN') .OR. (BADFIL.NE.2))
     *   CALL ZCLOSE (LUN(5), FIND5, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONV2: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('CONV2: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('CONV2: ',A4,' ERROR',I7,' I,J=',I3,I5)
 1902 FORMAT ('      FILE ',A)
      END
