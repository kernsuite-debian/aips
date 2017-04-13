      SUBROUTINE CONV4 (LUN, VOL, FIL, BO, XBUFF1, BUFSZ1,
     *   XBUFF2, BUFSZ2, NX, NY, FMAX, FMIN, IERR)
C-----------------------------------------------------------------------
C! Fourth of four routines to convolve two real images.
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
C   CONV4 = last of four routines to convolve two real images.
C   Finishes the transpose of the COMPLEX to REAL transform
C   getting the data from either the scratch file (no. 4) of the AP
C   memory.  Only the inner quarter of the transform is fully processed.
C   results are written to file no. 5.
C   Input:
C     LUN(5)      I    LUNs for files
C     VOL(5)      I    Volume numbers for the files.
C     FIL(5)      C*12 Physical names for the files.
C     BO(5)       I    Block offsets for the files.
C     XBUFF1(),XBUFF2()  R    Work buffers for I/O
C     BUFSZ1,BUFSZ2  I    Size in bytes for XBUFF1 and XBUFF2
C     NX,NY          I    Number of grid cells in X and Y of maps.
C   Output:
C     FMAX           R    Maximum value in output file.
C     FMIN           R    Minimum value in output file.
C     IERR           I    Return error code, 0=>OK, otherwise failed.
C                         3=>Size less than 4 in one dimension.
C   The Transformed Map/File will be left in File 4.
C-----------------------------------------------------------------------
      CHARACTER FIL(5)*48, BADOP*4
      INTEGER   LUN(5), VOL(5), BO(5), BUFSZ1, BUFSZ2, NX, NY, IERR
      REAL      XBUFF1(*), XBUFF2(*), FMAX, FMIN, TMAX(3)
      INTEGER   FIND2, FIND3, BIND2, BIND3, BADFIL, WIN(4),  NBUF,
     *   MPASS, IFIN, IER, I, IC, J, K, LROW, NCOL, NROW, JAPWRD, NSKIP,
     *   IFIRST, NOROW, NX2, NY2, KROW, KOFF, NUMGOT, KNDEX, QUATNX,
     *   HALFNY, QUATNY, NWORD, HALFNX, ONENX, TWONX, ONENY, TWONY,
     *   ONECOL, TWOCOL, ONEROW, TWOROW, NSHOV, OMPASS, INDEX2, APSIZ,
     *   INDEX
      LOGICAL   MAP, WAIT, EXCL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Determine power of 2 AP size.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
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
      QUATNX = NX / 2
      HALFNX = NX
      QUATNY = NY / 2
      HALFNY = NY
      ONENX = NX*2
      TWONX = 2 * NX2
      ONENY = NY2
      TWONY = 2 * NY2
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
 10   CALL ZOPEN (LUN(3), FIND3, VOL(4), FIL(4), MAP, EXCL, WAIT, IERR)
      BADFIL = 3
      IF (IERR.NE.0) GO TO 900
      BADOP = 'INIT'
C                                       Determine length of read
      LROW = NROW * 2 * NCOL
C                                       COMPLEX to REAL transform
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
C                                       Unscramble.
            IFIRST = I - MPASS / 2
            IF (IFIRST.LE.0) IFIRST = IFIRST + MPASS
            KOFF = (IFIRST-1) * NROW
C                                       Don't do passes with no output
            IF ((MPASS.GE.4) .AND. ((I.LE.MPASS/4).OR.(I.GT.3*MPASS/4)))
     *         GO TO 600
C                                       Read only for multiple AP loads
            IF (MPASS.LE.1) GO TO 570
C                                       Initialize read with skip.
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
               DO 560 J = 1,MPASS
C                                       Load NROW rows into the AP
C                                       Return to here if multiple reads
                  IC = -1
 540                 CALL MSKIP (LUN(2), FIND2, XBUFF1, BIND2, IFIN,
     *                  IERR)
                     IC = IC + 1
                     IF (IERR.NE.0) THEN
                        BADOP = 'READ'
                        GO TO 900
                        END IF
 550                 INDEX = (J-1) * LROW + NSHOV * IC
                     CALL QPUT (XBUFF1(BIND2), INDEX, NSHOV, 2)
                     CALL QWD
C                                       If read not finished, Loop.
                     IF (IFIN.NE.0) GO TO 540
 560              CONTINUE
C                                       Finish transpose.
               CALL QVTRAN (OMPASS, ONEROW, 0, TWOCOL)
 570        CONTINUE
C                                       Do row transform and unscramble
C                                       data to disk.
            NUMGOT = 0
            DO 590 J = 1,NROW
               K = J
C                                       Following to scramble if MPASS=1
               IF (MPASS.LE.1) THEN
                  K = J - NY
                  IF (J.LE.NY) K = J + NY
                  END IF
 580           INDEX = (K - 1) * ONENX
C                                       Process/write inner quarter
               KROW = K + KOFF
               IF ((KROW.GT.QUATNY) .AND. (KROW.LE.HALFNY+QUATNY))
     *            GO TO 590
                  NUMGOT = NUMGOT + 1
C                                       Do FFT.
                  CALL QRFFT (INDEX, ONENX, -1)
C                                        Unscramble.
                  INDEX2 = INDEX + QUATNX
                  CALL QVMOV (INDEX, 1, INDEX2, 1, QUATNX)
                  KNDEX = INDEX2 + HALFNX
                  CALL QVMOV (KNDEX, 1, INDEX, 1, QUATNX)
                  CALL MDISK ('WRIT', LUN(3), FIND3, XBUFF2, BIND3,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     BADOP = 'WRIT'
                     BADFIL = 3
                     GO TO 900
                     END IF
C                                       Get row.
 585              CALL QWR
                  CALL QGET (XBUFF2(BIND3), INDEX, HALFNX, 2)
                  CALL QWD
C                                        Find row max.-min.
                  INDEX2 = INDEX + 2
                  CALL QMAXMI (INDEX, 1, INDEX, INDEX2, HALFNX)
                  CALL QWR
                  CALL QGET (TMAX, INDEX, 3, 2)
                  CALL QWD
                  FMAX = MAX (TMAX(1), FMAX)
                  FMIN = MIN (TMAX(3), FMIN)
C
 590           CONTINUE
C                                       Roll AP if necessary.
C                                       Since NWORD = 0 will
C                                       at most release/assign AP
            NWORD = 0
            IF (MPASS.GT.I)
     *         CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            IF (IERR.NE.0) GO TO 999
 600        CONTINUE
C                                       Finish write.
         BADOP = 'FINI'
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
 1000 FORMAT ('CONV4: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('CONV4: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('CONV4: ',A4,' ERROR',I7,' I,J=',I3,I5)
 1902 FORMAT ('      FILE ',A)
      END
