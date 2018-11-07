      SUBROUTINE GETGRI (L6, LI, NEWLIN, NGRIPE, GRIPE, GRARGS)
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
C     Get the next gripe from logical unit LI and return it in string
C     variable GRIPE with its number in integer NGRIPE.
C     Messages go out on unit L6.
C     A number of checks are made on the format of the gripes.
C     Uses integer function ITRIM.
C     DCW, NRAO-CV, 28Sept84.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     L6, LI, NGRIPE
      CHARACTER   GRIPE*(*), NEWLIN*(*), GRARGS(9)*(*)
C
      INTEGER     NLINE, I, K, KLINE, ITRIM, KGRIPE, ISTOP, I1, L
      CHARACTER   LINE*300, C*1
C-----------------------------------------------------------------------
C                                       initialize for next gripe:
 10   CONTINUE
      NGRIPE = 0
      NLINE = 0
      GRIPE = ' '
      KGRIPE = 0
C                                       Loop to assemble gripe:
 20   CONTINUE
         LINE = ' '
         READ (LI, 1020, END=30) LINE
         GO TO 40
 30      CONTINUE
            IF (NLINE.EQ.0) GO TO 999
            GO TO 100
 40      CONTINUE
C                                       ignore leading blank lines:
         IF ((LINE.EQ.' ').AND.(NLINE.EQ.0)) GO TO 20
C                                       trailing blank line terminates:
         IF (LINE.EQ.' ') GO TO 100
C                                       append non-blank line:
         NLINE = NLINE + 1
         KLINE = ITRIM (LINE)
         IF (KLINE.GT.80) WRITE (L6, 1040) NLINE, KLINE, LINE(1:30)
         IF (NLINE.LT.100) GO TO 50
            WRITE (L6, 1045) NLINE, KLINE, LINE(1:40)
            GO TO 900
 50      CONTINUE
         K = ITRIM (NEWLIN)
         LINE = LINE(1:KLINE) // NEWLIN(1:K)
         KLINE = KLINE + K
         IF (KGRIPE.GT.0) GO TO 60
            GRIPE = LINE
            GO TO 70
 60      CONTINUE
            GRIPE = GRIPE(1:KGRIPE) // LINE
 70      CONTINUE
         KGRIPE = KGRIPE + KLINE
         GO TO 20
C                                       Gripe assembled. Check it:
 100  CONTINUE
C                                       Ignore input/finalrun/end:
      IF ((INDEX (GRIPE(1:KGRIPE), '\input').EQ.0).AND.
     *    (INDEX (GRIPE(1:KGRIPE), '\finalrun').EQ.0).AND.
     *    (INDEX (GRIPE(1:KGRIPE), '\end').EQ.0)) GO TO 115
      IF ((INDEX (GRIPE(1:KGRIPE), '\count0=').NE.0).OR.
     *    (INDEX (GRIPE(1:KGRIPE), '\gripe').NE.0)) GO TO 110
         GO TO 10
 110  CONTINUE
         WRITE (L6, 1110) LI
         GO TO 900
 115  CONTINUE
C                                       Gripe must begin with count0:
      IF (GRIPE(1:8).EQ.'\count0=') GO TO 130
         WRITE (L6, 1120) GRIPE(1:30)
         GO TO 900
 130  CONTINUE
C                                       Gripe must contain \gripe:
      K = INDEX (GRIPE(1:KGRIPE), '\gripe')
      IF (K.NE.0) GO TO 140
         WRITE (L6, 1130) GRIPE(1:30)
         GO TO 900
 140  CONTINUE
C                                       Only one count0/gripe allowed:
      IF ((INDEX (GRIPE(K+1:KGRIPE), '\count0').EQ.0).AND.
     *    (INDEX (GRIPE(K+1:KGRIPE), '\gripe').EQ.0)) GO TO 150
         WRITE (L6, 1140) GRIPE(1:30)
         GO TO 900
 150  CONTINUE
C                                       Extract gripe number:
      NGRIPE = 0
      DO 160 I = 9, 20
         K = ICHAR (GRIPE(I:I))
         IF (K.EQ.32) GO TO 160
         IF ((K.LT.48).OR.(K.GT.57)) GO TO 170
         NGRIPE = NGRIPE * 10 + (K - 48)
 160     CONTINUE
      WRITE (L6, 1160) GRIPE(1:30)
      GO TO 900
 170  CONTINUE
      IF (NGRIPE.EQ.0) WRITE (L6, 1170) GRIPE(1:30)
C                                       Break out the arguments:
C                                         (and check brackets!)
      ISTOP = ITRIM (GRIPE)
      I = INDEX (GRIPE, '\gripe') + 5
      I1 = I
      DO 250 K = 1, 9
         L = 0
 210     CONTINUE
         I = I + 1
         IF (I.GT.ISTOP) GO TO 240
         C = GRIPE(I:I)
         IF (C.NE.'{') GO TO 220
            L = L + 1
            IF (L.EQ.1) I1 = I
            GO TO 210
 220     CONTINUE
         IF (C.NE.'}') GO TO 230
            L = L - 1
            IF (L.LT.0) GO TO 240
            IF (L.GT.0) GO TO 210
C                                       argument assembled:
               GRARGS(K) = GRIPE(I1:I)
               GO TO 250
 230     CONTINUE
         IF (L.GT.0) GO TO 210
            IF ((C.EQ.' ').OR.(C.EQ.'|')) GO TO 210
C                                       bracket problem:
 240        CONTINUE
            WRITE (L6, 1240)  NGRIPE, K,
     *                        GRIPE(I-10:I), GRIPE(I+1:I+10),
     *                        I, ISTOP, I1, L
            IF (L.GT.0) WRITE (L6, 1243)
            IF (L.LE.0) WRITE (L6, 1245)
            GO TO 260
 250     CONTINUE
 260  CONTINUE
C                                       Tests completed, go exit:
      GO TO 999
C                                       Abort:
 900  CONTINUE
      STOP 'GETGRI cannot continue.'
C                                       Normal exit:
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A)
 1040 FORMAT (' GETGRI: long line! NLINE=', I3, ', KLINE=', I3,
     *                                                   ', LINE=', A)
 1045 FORMAT (' GETGRI: too many lines! NLINE=', I3, ', KLINE=', I3,
     *                                                   ', LINE=', A)
 1110 FORMAT (' GETGRI: \gripe with \input, \finalrun or \end! LI=', I2)
 1120 FORMAT (' GETGRI: does not begin with \count0! line1=', A)
 1130 FORMAT (' GETGRI: no \gripe in gripe! line1=', A)
 1140 FORMAT (' GETGRI: two gripes in one?!? line1=', A)
 1160 FORMAT (' GETGRI: runaway numeric string? line1=', A)
 1170 FORMAT (' GETGRI: NGRIPE=0 !! line1=', A)
 1240 FORMAT (' GETGRI: bracket problem with gripe', I5,
     *                              ', argument', I2,
     * /,     ' GETGRI: text in neighborhood=', 5X, A, '^', A,
     * /,     ' GETGRI: I=', I4, ', ISTOP=', I4, ', I1=', I4,
     *                  ', L=', I2)
 1243 FORMAT (' GETGRI: either extra { or missing }.')
 1245 FORMAT (' GETGRI: either extra } or missing {.')
      END
      SUBROUTINE WRIGRI (L6, LFIL, NEWLIN, GRIPE)
C-----------------------------------------------------------------------
C     Routine to write out a gripe on a designated file.
C     The lines of the gripe are assumed to be delimited by the string
C     NEWLIN.
C     DCW, 30Sept84.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     L6, LFIL
      CHARACTER   NEWLIN*(*), GRIPE*(*)
C
      INTEGER     KNWLIN, KGRIPE, K, ITRIM
C-----------------------------------------------------------------------
      KNWLIN = ITRIM (NEWLIN)
      KGRIPE = ITRIM (GRIPE)
10    CONTINUE
         K = INDEX (GRIPE(1:KGRIPE), NEWLIN(1:KNWLIN))
         IF (K.EQ.0) STOP 'WRIGRI: something is terribly wrong!?!'
         WRITE (LFIL, 1010) GRIPE(1:(K-1))
         IF (K.GT.80) WRITE (L6, 1015) K, GRIPE(1:30)
         GRIPE = GRIPE((K+KNWLIN):(KGRIPE+1))
         KGRIPE = (KGRIPE - (K + KNWLIN)) + 1
         IF (KGRIPE.GT.0) GO TO 10
      WRITE (LFIL, 1020)
C-----------------------------------------------------------------------
 1010 FORMAT (A)
 1015 FORMAT (' WRIGRI: line', I3, ' chars long! beginning:', A)
 1020 FORMAT (1X)
      END
      SUBROUTINE PADTEX (L6, FILNAM)
C-----------------------------------------------------------------------
C     Routine to add default of ".TEX" to filename if no type present.
C     DCW, 30Sept84
C-----------------------------------------------------------------------
      INTEGER     L6, ITRIM
      CHARACTER   FILNAM*(*)
C
      IF (INDEX (FILNAM, '.').NE.0) GO TO 900
         FILNAM = FILNAM(1:ITRIM (FILNAM)) // '.TEX'
         WRITE (L6, 1010) FILNAM
 900  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' PADTEX:     filename assumed to be: ', A)
      END
      INTEGER FUNCTION ITRIM (STRING)
C-----------------------------------------------------------------------
C   Function to determine length of a string. I.e., it trims trailing
C   blanks. Use with calls like:
C         TRIMMED = GROSS(1:ITRIM(GROSS))
C   DCW, NRAO-CV, 30Nov82.
C-----------------------------------------------------------------------
      IMPLICIT  NONE
      CHARACTER STRING*(*)
C
      ITRIM = LEN (STRING) + 1
 10   CONTINUE
         ITRIM = ITRIM - 1
         IF (ITRIM.LT.1) GO TO 999
         IF (STRING(ITRIM:ITRIM).EQ.' ') GO TO 10
 999  RETURN
      END
      SUBROUTINE DCLCMD (L6, COMMAND, ISTAT)
C-----------------------------------------------------------------------
C     Execute a DCL command string. Failure signaled by nonzero ISTAT.
C     DCW, NRAO-CV, 07Mar85.
C-----------------------------------------------------------------------
      INTEGER     ISTAT, L6
      INTEGER     N72, ITRIM
      CHARACTER   COMMAND*(*)
C
      INCLUDE     '($SSDEF)'
      INTEGER     STATUS, LIB$SPAWN
      DATA N72 /72/
C-----------------------------------------------------------------------
      WRITE (L6, 1010) COMMAND(1:MIN0(ITRIM(COMMAND),N72))
      STATUS = LIB$SPAWN (COMMAND,,,,,,,,,)
      ISTAT = 0
      IF (STATUS.EQ.SS$_NORMAL) GO TO 20
         WRITE (L6, '('' STATUS='', Z8)') STATUS
         ISTAT = 1
 20   RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' DCLCMD: ', A)
      END
