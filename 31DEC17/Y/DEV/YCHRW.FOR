      SUBROUTINE YCHRW (CHAN, X, Y, STRING, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! writes characters into image and graphics planes
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   writes characters into image planes of the TV.  The format is 5 x 7
C   or a multiple thereof with one or more blanks all around.  The net
C   is set to match CSIZTV if possible.  We recommend CSIZTV = 7, 9 for
C   TVs of size around 512 square and CSIZTV = 14, 22 or so for TVs of
C   size around 1024 square.  This version will work on all TVs which
C   allow horizontal writing to the right.  It is a Y routine to allow
C   for hardware character generators on the TV.
C   Inputs:
C      CHAN    I       channel select  (1 to NGRAY + NGRAPH)
C      X       I       X position lower left corner first char.
C      Y       I       Y position lower left corner first char.
C      STRING  C*(*)   character string - length passed from Fortran
C   Output:
C      SCRTCH  I(>)    scratch buffer (dim = 14*count+8 < MAXXTV(1))
C      IERR    I       error code of Z...XF:0 - ok
C                                           2 - input error
C   Generic version: do 5x7 multiplied up and with some space around the
C   characters as specified by CSIZTV.
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   CHAN, X, Y, SCRTCH(*), IERR
C
      INTEGER   LENGTH, ICX, ICY, LINE, IBACK, ISPX1, ISPY1, BITIDX,
     *   IC, IT, MASK, ZAND, TABLE(5,97), TAB2(5,17), TAB3(5,16),
     *   TAB4(5,16), TAB5(5,16), TAB6(5,16), TAB7(5,16), JT, LT, KT,
     *   BITS(15), I, J, IANGL, LSCRCH(2048), II, JJ, COUNT, LX, LY,
     *   MULT, ISPX2, ISPY2, CSIZ(2)
      LOGICAL   T, F
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE  (TABLE(1, 1), TAB2),  (TABLE(1,18), TAB3),
     *             (TABLE(1,34), TAB4),  (TABLE(1,50), TAB5),
     *             (TABLE(1,66), TAB6),  (TABLE(1,82), TAB7)
      DATA T, F /.TRUE., .FALSE./
C                                        control chars all ?
C                                        blank !"#$%&'()*+,-./
      DATA TAB2/  32,  64,  69,  72,  48,
     *             0,   0,   0,   0,   0,
     *             0,   0, 121,   0,   0,
     *             0, 112,   0, 112,   0,
     *            20,  62,  20,  62,  20,
     *            18,  42, 127,  42,  36,
     *             2,  36,   8,  18,  32,
     *            54,  73,  85,  34,   5,
     *             0,   0, 112,   0,   0,
     *             0,  28,  34,  65,   0,
     *             0,  65,  34,  28,   0,
     *            20,   8,  62,   8,  20,
     *             8,   8,  62,   8,   8,
     *             0,   1,   6,   0,   0,
     *             0,   8,   8,   8,   0,
     *             0,   0,   1,   0,   0,
     *             2,   4,   8,  16,  32/
C                                        0123456789:;<=>?
      DATA TAB3/  62,  69,  73,  81,  62,
     *             0,  33, 127,   1,   0,
     *            35,  69,  73,  73,  49,
     *            66,  65,  73,  89, 102,
     *            12,  20,  36, 127,   4,
     *           114,  81,  81,  81,  78,
     *            30,  41,  73,  73,  70,
     *            64,  71,  72,  80,  96,
     *            54,  73,  73,  73,  54,
     *            49,  73,  73,  74,  60,
     *             0,   0,  18,   0,   0,
     *             0,   1,  22,   0,   0,
     *             8,  20,  34,  65,   0,
     *            20,  20,  20,  20,   0,
     *            65,  34,  20,   8,   0,
     *            32,  64,  69,  72,  48/
C
C                                       ABCDEFGHIJKLMNO
      DATA TAB4/  18,  37,  37,  37,  30,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *             0,  65, 127,  65,   0,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        PQRSTUVWXYZ[\]^_
      DATA TAB5/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   0, 127,  65,   0,
     *            32,  16,   8,   4,   2,
     *             0,  65, 127,   0,   0,
     *            16,  32,  64,  32,  16,
     *             1,   1,   1,   1,   1/
C                                        `abcdefghijklmno
      DATA TAB6/   0,  64,  32,  16,  0,
     *            31,  36,  68,  36,  31,
     *           127,  73,  73,  73,  34,
     *            62,  65,  65,  65,  34,
     *            65, 127,  65,  65,  62,
     *           127,  73,  73,  73,  65,
     *           127,  72,  72,  64,  64,
     *            62,  65,  65,  69,  39,
     *           127,   8,   8,   8, 127,
     *            65,  65, 127,  65,  65,
     *             2,   1,   1,   1, 126,
     *           127,   8,  20,  34,  65,
     *           127,   1,   1,   1,   1,
     *           127,  32,  24,  32, 127,
     *           127,  16,   8,   4, 127,
     *            62,  65,  65,  65,  62/
C                                        pqrstuvwxyz{ }~?
      DATA TAB7/ 127,  72,  72,  72,  48,
     *            62,  65,  69,  66,  61,
     *           127,  72,  76,  74,  49,
     *            50,  73,  73,  73,  38,
     *            64,  64, 127,  64,  64,
     *           126,   1,   1,   1, 126,
     *           112,  12,   3,  12, 112,
     *           126,   1,  14,   1, 126,
     *            99,  20,   8,  20,  99,
     *            96,  16,  15,  16,  96,
     *            67,  69,  73,  81,  97,
     *             0,   8,  54,  65,   0,
     *             0,   0, 127,   0,   0,
     *             0,  65,  54,   8,   0,
     *             4,   8,   4,   2,   4,
     *            32,  64,  69,  72,  48/
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      COUNT = LEN (STRING)
      LX = X + COUNT*CSIZTV(1) - 1
      LY = Y + CSIZTV(2) - 1
      IF ((Y.LT.1) .OR. (LY.GT.MAXXTV(2))) GO TO 990
      IF ((X.LT.1) .OR. (LX.GT.MAXXTV(1))) GO TO 990
      IF (COUNT.LE.0) GO TO 990
      IBACK = 0
      IF (CHAN.GT.NGRAY) IBACK = 0
      IANGL = 0
C                                       determine sizes
      CSIZ(1) = CSIZTV(1)
      CSIZ(2) = CSIZTV(2)
      MULT = MAXXTV(1) / 700 + 1
      ICX = 5
      ICY = 7
      ISPX1 = (CSIZ(1) - MULT * ICX) / 2
      ISPY1 = (CSIZ(2) - MULT * ICY) / 2
      IF ((ISPX1.LE.0) .OR. (ISPY1.LE.0)) THEN
         IF (MULT.GT.1) MULT = MULT - 1
         ISPX1 = (CSIZ(1) - MULT * ICX) / 2
         ISPY1 = (CSIZ(2) - MULT * ICY) / 2
         END IF
      ISPX2 = CSIZ(1) - MULT*ICX - ISPX1
      ISPY2 = CSIZ(2) - MULT*ICY - ISPY1
      IF ((ISPX1.LE.0) .OR. (ISPX2.LE.0) .OR. (ISPY1.LE.0) .OR.
     *   (ISPY2.LE.0)) THEN
         ISPX1 = MAX (1, ISPX1)
         ISPX2 = MAX (1, ISPX2)
         ISPY1 = MAX (1, ISPY1)
         ISPY2 = MAX (1, ISPY2)
         CSIZ(1) = MULT * ICX + ISPX1 + ISPX2
         CSIZ(2) = MULT * ICY + ISPY1 + ISPY2
         WRITE (MSGTXT,1000) CSIZ, CSIZTV
         CALL MSGWRT (6)
         END IF
C                                       set up
      LENGTH = COUNT * CSIZ(1)
      LINE = Y
C                                       extract needed parts of table
      BITIDX = 1
      DO 20 I = 1,COUNT
C                                       get standard ASCII char
C                                       in highly machine independent
         JT = NBITWD / 8
         CALL ZCLC8 (1, STRING(I:I), JT, LT)
         CALL ZI32IL (1, 1, LT, KT)
         IT = NBITWD - (JT-1)*8
         CALL ZGTBIT (IT, KT, BITS)
         CALL ZPTBIT (8, IC, BITS(IT-7))
C                                        all CTRL characters to 1
         IC = MAX (1, IC-30)
         CALL COPY (ICX, TABLE(1,IC), SCRTCH(BITIDX))
         BITIDX = BITIDX + ICX
 20      CONTINUE
C                                        blanks at bottom
      CALL FILL (LENGTH, IBACK, LSCRCH)
      DO 30 I = 1,ISPY1
         CALL YIMGIO ('WRIT', CHAN, X, LINE, IANGL, LENGTH, LSCRCH,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         LINE = LINE + 1
 30      CONTINUE
C                                        Character lines
      DO 50 I = 1,ICY
         MASK = 2 ** (I-1)
         JJ = 1
         BITIDX = 1
C                                        if mask bit on in char value
C                                        then set to -1 else to 0
         DO 40 J = 1,COUNT
            CALL FILL (ISPX1, IBACK, LSCRCH(BITIDX))
            BITIDX = BITIDX + ISPX1
            DO 35 II = 1,ICX
               IC = ZAND (MASK, SCRTCH(JJ))
               IF (IC.NE.0) IC = MAXINT
               IF (IC.EQ.0) IC = IBACK
               CALL FILL (MULT, IC, LSCRCH(BITIDX))
               BITIDX = BITIDX + MULT
               JJ = JJ + 1
 35            CONTINUE
            CALL FILL (ISPX2, IBACK, LSCRCH(BITIDX))
            BITIDX = BITIDX + ISPX2
 40         CONTINUE
         DO 45 J = 1,MULT
            CALL YIMGIO ('WRIT', CHAN, X, LINE, IANGL, LENGTH, LSCRCH,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            LINE = LINE + 1
 45         CONTINUE
 50      CONTINUE
C                                        blanks at top
      CALL FILL (LENGTH, IBACK, LSCRCH)
      DO 60 I = 1,ISPY2
         CALL YIMGIO ('WRIT', CHAN, X, LINE, IANGL, LENGTH,
     *      LSCRCH, IERR)
         LINE = LINE + 1
 60      CONTINUE
      GO TO 999
C
 990  WRITE (MSGTXT,1990) X, LX, Y, LY, COUNT
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YCHRW: USING CHAR SIZE',2I3,' NOT INPUT SIZE',2I3)
 1990 FORMAT ('YCHRW: BAD X,LX, Y,LY, COUNT =',5I7)
      END
