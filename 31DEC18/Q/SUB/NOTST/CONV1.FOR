      SUBROUTINE CONV1 (LUN, VOL, FIL, BO, XBUFF1, BUFSZ1, XBUFF2,
     *   BUFSZ2, NX, NY, IERR)
C-----------------------------------------------------------------------
C! First of four routines to convolve two real images.
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
C   CONV1 = first of four routines to convolve two real images.
C   In this routine the input image is read in, zero padded, row
C   transformed, partially transposed and written to disk if necessary.
C   Five file are used by CONV1-CONV4: 1=input NX x NY real image,
C   2&3 = scratch (work) file (4*NX x NY+2), 4=Output (NX x NY) file
C   and 5=FFT of convolving image (4*NX x NY+2).
C        If the entire map will fit into the AP the intermediate results
C   are not written to the WORK files.
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
      INTEGER   LUN(5), VOL(5), BO(5), BUFSZ1, BUFSZ2, NX, NY, IHALF,
     *   IERR, NX2, NY2, KOFF, KROW, JPASS
      REAL      XBUFF1(*), XBUFF2(*)
      INTEGER   FIND1, FIND2, BIND1, BIND2, BADFIL, WIN(4), IER, J, K,
     *   MPASS, NROW, JAPWRD,  I, ILIM, IT, APSIZ, HALFNX, INDEX, ONENX,
     *   ONEROW, JNDEX, ONENY, NWORD, TWONX, QUATNX, HALFNY, QUATNY
      DOUBLE PRECISION   XNDEX
      LOGICAL   MAP, WAIT, EXCL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get power of 2 size of AP.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
C                                       Make sure min. dim .ge. 4
      IF ((NX.GE.4) .AND. (NY.GE.4)) GO TO 5
           WRITE (MSGTXT,1000) NX,NY
           CALL MSGWRT (8)
           IERR = 3
           GO TO 999
C                                       Determine number of AP loads
C                                       required
C                                       Have to zero padd input.
 5    XNDEX = (NX * 2.0) / APSIZ
      MPASS = NY * XNDEX* 2 + 0.0001
C                                       DEBUG
      MPASS = MAX (MPASS, 1)
      IF (MPASS.GT.1) THEN
         MSGTXT = 'APCONV: MORE THAN 1 PASS REQUIRED'
         CALL MSGWRT (9)
         END IF
      NY2 = NY * 2
      NX2 = NX * 2
      TWONX = NX * 4
      HALFNX = NX
      QUATNX = NX / 2
      HALFNY = NY
      QUATNY = NY / 2
      ONENX = NX * 2
      ONENY = NY * 2
      NWORD = 0
C                                       Open input and work file.
      BADFIL = 1
      BADOP = 'OPEN'
      CALL ZOPEN (LUN(1), FIND1, VOL(1), FIL(1), MAP, EXCL, WAIT,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                        Write only on multiple AP loads
      IF (MPASS.LE.1) GO TO 20
         BADFIL = 2
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
 20   BADOP = 'INIT'
C                                       INIT output file.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX2
         WIN(4) = NY2
C                                        Write only for multiple AP
C                                        loads.
         IF (MPASS.LE.1) GO TO 35
            CALL MINIT ('WRIT', LUN(2), FIND2, NX2, NY2, WIN, XBUFF2,
     *         BUFSZ2, BO(2), IERR)
            IF (IERR.NE.0) GO TO 900
            WIN(2) = NY/2 + 1
 35       WIN(3) = NX
          WIN(4) = NY
C                                       NROW = no. rows loaded at once.
         NROW = APSIZ / NX2
         NROW = MIN (NROW, NY2)
         ONEROW = NROW
C                                       Load second half first.
C                                       Jump to here for first half.
 40      CONTINUE
C                                       INIT Input file.
            CALL MINIT ('READ', LUN(1), FIND1, NX, NY, WIN, XBUFF1,
     *         BUFSZ1, BO(1), IERR)
            IF (IERR.EQ.0) GO TO 50
               BADOP = 'INIT'
               BADFIL = 1
               GO TO 900
 50         ILIM = MPASS / 2
            ILIM = MAX (ILIM, 1)
            DO 120 I = 1,ILIM
               KOFF = ((I-1) * NY2) / MPASS
               IF ((MPASS.GT.1) .AND. (WIN(2).EQ.1)) KOFF = KOFF + NY
C                                       If this whole pass 0's skip
               JPASS = I + MPASS/2
               IF (WIN(2).EQ.1) JPASS = I
               IF (((JPASS.LE.(MPASS/4)) .OR. (JPASS.GT.(3*MPASS/4)))
     *           .AND. (MPASS.GE.4)) GO TO 120
               DO 80 J = 1,NROW
C                                       Compute index
                  K = J
C                                       Following to scramble input
C                                       if MPASS = 1.
                  IF (MPASS.EQ.1) THEN
                     K = J - NY
                     IF (J.LE.NY) K = J + NY
                     END IF
                  INDEX = (K - 1) * ONENX
C                                       Zero row
                  CALL QVCLR (INDEX, 1, ONENX)
C                                       Is this a real row?
                  KROW = KOFF + K
                  IF ((KROW.GT.QUATNY) .AND. (KROW.LE.HALFNY+QUATNY))
     *               GO TO 80
C                                       Read
                     CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1,
     *                  IERR)
                     IF (IERR.EQ.0) GO TO 60
                        IT = (I - 1) * NROW + WIN(3) + J - 1
                        BADOP = 'READ'
                        BADFIL = 1
                        GO TO 900
C                                       Load data in AP, scramble.
 60                  CALL QWR
                     CALL QPUT (XBUFF1(BIND1+NX/2), INDEX, QUATNX, 2)
                     JNDEX = INDEX + HALFNX + QUATNX
                     CALL QPUT (XBUFF1(BIND1), JNDEX, QUATNX, 2)
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
C                                       Write only on multiple AP loads.
               IF (MPASS.LE.1) GO TO 115
                  DO 110 J = 1,NROW
                     CALL MDISK ('WRIT', LUN(2), FIND2, XBUFF2, BIND2,
     *                  IERR)
                     IF (IERR.EQ.0) GO TO 100
                        IT = WIN(2) + I + J - 1
                        BADOP = 'WRIT'
                        BADFIL = 2
                        GO TO 900
 100                 INDEX = (J - 1) * ONENX
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
 120           CONTINUE
C                                       Check to see if finished.
            IF (WIN(2).EQ.1) GO TO 130
               WIN(2) = 1
               GO TO 40
C                                       Write only on multiple AP loads
 130     IF (MPASS.LE.1) GO TO 990
            BADFIL = 2
            BADOP = 'FINI'
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.NE.0) GO TO 900
            GO TO 990
C                                       Errors
 900  WRITE (MSGTXT,1900) BADOP, IERR, FIL(BADFIL)
      IF ((BADOP.EQ.'READ') .OR. (BADOP.EQ.'WRIT')) WRITE (MSGTXT,1901)
     *   BADOP, IERR, IT, FIL(BADFIL)
      CALL MSGWRT (8)
C                                       Close files.
 990  IF ((MPASS.GT.1) .AND. (BADOP.NE.'OPEN')) CALL ZCLOSE (LUN(2),
     *   FIND2, IER)
      IF ((BADOP.NE.'OPEN') .OR. (BADFIL.EQ.2)) CALL ZCLOSE (LUN(1),
     *   FIND1, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONV1: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('CONV1: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('CONV1: ',A4,' ERROR',I7,' ROW',I5,' FILE ',A)
      END
