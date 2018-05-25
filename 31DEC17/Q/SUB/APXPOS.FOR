      SUBROUTINE APXPOS (NROW, NCOL, START, IERR)
C-----------------------------------------------------------------------
C! In place transpose of complex array.
C# AP-fft
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   APXPOS does an in place AP transpose of an NROW by NCOL
C   complex array where either NROW / NCOL = an integer or
C   NCOL / NROW = an integer.  The array is first divided into
C   square blocks which are transposed by CSQTRN and then the rows of
C   the resulting square blocks are transposed to complete the job.
C    Input:
C      NROW   I   = number of rows.
C      NCOL   I   = number of columns ( no. reals in a row ).
C      START  I    AP starting address
C    Output:
C      IERR   I   = Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   NROW, NCOL, START, IERR
      INTEGER   SIZE1, CORNER, ROW, WRK, NNBLOK, NNCOL, NNROW, WRK2,
     *   I, ITEST, NBLOK
      REAL      TEST
      LOGICAL   FLIP, T, F
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check input values.
      TEST = REAL (NCOL) / REAL (NROW)
      IF (TEST.LT.1.0) TEST = 1.0 / TEST
      ITEST = TEST
      IF (ABS(TEST-REAL(ITEST)).LT.1.0E-4) GO TO 10
         WRITE (MSGTXT,1000) NROW, NCOL
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
 10   IERR = 0
      NNROW = NROW
      NNCOL = NCOL
C                                       Decide if blocks are vertically
C                                       or horizionally arrainged.
      NBLOK = NCOL / NROW
      SIZE1 = MIN (NCOL, NROW)
      FLIP = F
      WRK = NROW * 2
      IF (NBLOK.GE.1) GO TO 20
         WRK = NCOL * 2
         WRK = WRK * NCOL
         FLIP = T
         NBLOK = NROW / NCOL
 20   ROW = 2 * NCOL
C                                       Transpose square blocks.
      DO 30 I = 1,NBLOK
         CORNER = START + (I - 1) * WRK
         CALL QCSQTR (CORNER, SIZE1, ROW)
         CALL QWR
 30      CONTINUE
C                                       Transpose rows.
      WRK = 2 * NROW
      WRK2 = 2 * NCOL
      NNBLOK = NBLOK
      IF ((.NOT.FLIP) .AND. (NBLOK.NE.1)) CALL QVTRAN (NNROW, NNBLOK,
     *   START, WRK)
      IF (FLIP) CALL QVTRAN (NNBLOK, NNCOL, START, WRK2)
      CALL QWR
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('APXPOS: ERROR IN INPUT, NROW = ',I5,', NCOL = ',I5)
      END
