      SUBROUTINE YMKCUR (TYPE, CURPAR, IERR)
C-----------------------------------------------------------------------
C! selects the form of the cursor to be displayed
C# Y3 TV-appl
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
C   Create and load into the programmable cursor memory an array to
C   generate a desired cursor form.
C   Inputs:
C      TYPE    I       Cursor shape. For now 1 = cross only
C      CURPAR  I(?)    Parameters defining size & orientation of cursor
C                      pattern
C   Output:
C      IERR    I       Error code: 0 =>okay, 2 => input error
C   Written NOV-83 for DeAnza by W. Jaffe for cross form cursor only
C-----------------------------------------------------------------------
      INTEGER   TYPE, CURPAR(*), IERR
C
      INCLUDE 'INCS:YDEA.INC'
      LOGICAL   T
      INTEGER   NBYTE, PACK(16), I, J, IX, IY, CUR0, CLUT, PROC1,
     *   AGCUR, FANG(4), ZOR
      INCLUDE 'INCS:DTVC.INC'
      DATA T /.TRUE./
C                               Cursor LUT register is #5
C                               Byte Array group for cursor is #3
C                               CUR0 points to 1st cursor board
C                               1st prog cursor is channel addr 0
      DATA CLUT, CUR0, AGCUR, PROC1 /5, 0, 3, 0/
C                               Bits pairs corresponding to the four
C                               cursor pixels contained in each byte
C     DATA FANG /Z3, ZC, Z30, ZC0/
      DATA FANG /3, 12, 48, 192/
      DATA NBYTE /32/
C-----------------------------------------------------------------------
C                                       error tests
      IERR = 2
C                                       max cursor xy size for deanza
      IF ((TYPE.NE.1) .OR. (CURPAR(1).LE.0) .OR. (CURPAR(1).GT.64) .OR.
     *    (CURPAR(2).LE.0) .OR. (CURPAR(2).GT.64)) GO TO 999
C                                       Zero a line
      CALL FILL (16, 0, PACK)
C                                       Write in Y-stripe
      DO 10 IY = 1,64
C                                       Cursor LUT address of this line
         YBUFF(CURLUT) = 16 * (IY-1)
C                                       Load into cursor LUT address reg
         CALL ZDEAXF ('LCR ', YBUFF(CURLUT), 2, CUR0, CLUT, T, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set 2-byte fang of central pixel
C                                       off for lines out of range; on
C                                       for rest
         IF ((IY.LT.33-CURPAR(2)/2) .OR. (IY.GT.32+CURPAR(2)/2)) THEN
            PACK(8) = 0
         ELSE
            PACK(8) = FANG(4)
            END IF
C                                       Load values
         CALL ZDEAXF ('LPA ', PACK, NBYTE, PROC1, AGCUR, T, IERR)
 10      CONTINUE
C                                       Write in X-stripe at center
C                                       The central pixel is blank
      DO 20 I = 1,16
         PACK(I) = 0
         DO 15 J = 1,4
            IX = 4 * I + J - 4
            IF ((IX.GE.32-CURPAR(1)/2) .AND. (IX.LT.31+CURPAR(1)/2)
     *          .AND. (IX.NE.32)) PACK(I) = ZOR (PACK(I), FANG(J))
 15         CONTINUE
 20      CONTINUE
C                                       Write it
      YBUFF(CURLUT) = 32 * 16
      CALL ZDEAXF ('LCR ', YBUFF(CURLUT), 2, CUR0, CLUT, T, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZDEAXF ('LPA ', PACK, NBYTE, PROC1, AGCUR, T, IERR)
C
 999  RETURN
      END
