      SUBROUTINE GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
C-----------------------------------------------------------------------
C! Read row of an image opened with INTMIO
C# Map IO-util
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
C  This routine will read a row of an image file that has been opened
C  with and initialized with INTMIO.  The routine will copy the
C  row from the IO buffer to the user buffer.
C  Inputs:
C     IMSTUF    I(37)   IO pointers, LUNs, counters and such. They are
C                       set in INTMIO.
C  In/out:
C     IOBLK     R(*)   IO buffer.
C  Outputs:
C     ROW       R(*)   Output row of image.
C     EOF       L      TRUE means last row specified in INTMIO by the
C                      BLC, TRC arguments has been read.
C     IERR      I      Error code, 0=ok, others from MDISK.
C-----------------------------------------------------------------------
      REAL   ROW(*), IOBLK(*)
      LOGICAL   EOF
      INTEGER   IMSTUF(37), IERR,
     *   IBLKOF, IWIN(4), IMPOS, IVALS, I, K, NDIMS, I0, ISGN
C-----------------------------------------------------------------------
      EOF = .FALSE.
C                                       Update counters.
      IMSTUF(31) = IMSTUF(31) + IMSTUF(37)
      IF ((IMSTUF(37)*IMSTUF(31)).LE.(IMSTUF(37)*IMSTUF(20))) GO TO 100
C                                       We are at the end of a plane.
         NDIMS = IMSTUF(8)
         I0 = 22
C                                       Update current plane counters.
         DO 20 I = 1, NDIMS
            IMSTUF(30+I) = IMSTUF(I0-3)
            ISGN = 1
            IF (IMSTUF(I0-1).GT.IMSTUF(I0)) ISGN = -1
            IMSTUF(31+I) = IMSTUF(31+I) + ISGN
            IF ((ISGN*IMSTUF(31+I)).LE.(ISGN*IMSTUF(I0))) GO TO 30
            I0 = I0 + 2
 20         CONTINUE
C                                       We have finished all planes
            EOF = .TRUE.
            IERR = 0
            GO TO 999
C                                       Do MINI3 for this plane.
 30      CALL COMOFF (IMSTUF(8), IMSTUF(10), IMSTUF(32), IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 999
         IBLKOF = IBLKOF + 1
         IWIN(1) = IMSTUF(17)
         IWIN(2) = IMSTUF(19)
         IWIN(3) = IMSTUF(18)
         IWIN(4) = IMSTUF(20)
         CALL MINIT ('READ', IMSTUF(1), IMSTUF(2), IMSTUF(10),
     *      IMSTUF(11), IWIN, IOBLK, IMSTUF(6), IBLKOF, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Do actual read.
 100  CALL MDISK ('READ', IMSTUF(1), IMSTUF(2), IOBLK, IMPOS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Copy values into user buffer.
      K = IMPOS - 1
      IVALS = IMSTUF(9)
      DO 200 I = 1,IVALS
         ROW(I) = IOBLK(K+I)
 200     CONTINUE
C
 999  RETURN
      END
