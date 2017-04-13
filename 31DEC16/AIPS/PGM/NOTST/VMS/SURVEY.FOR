      PROGRAM SURVEY
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
C   Reads an ADR file and generates AIPS Site Survey forms for all
C   entries with the "CP" attribute. Actually, the output is the TEX
C   input to generate the forms. The "\h" lines are also recognized
C   and separate forms are generated for "\h1", "\h2", ..., with the
C   fields filled in from the keyword=value pairs in those lines.
C   DCW, NRAO-CV, 25Nov85 (based on program LABEL). mods08Jan87.
C        minor mods 10Dec87.
C   Corrected to not change ". " to ".~" on attribute lines because
C   it was connecting one keyword and it's info. with the next one,
C        12/88 by DCW and NDW.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     MXLINS, MXHIGH, MXWIDE, MXADDR, MXNET
      PARAMETER   (MXLINS=50, MXHIGH=99, MXWIDE=99, MXADDR=5, MXNET=3)
      INTEGER     L1, L2, L5, L6, ITRIM, J, I, KHIGH, KWIDE, KSKIP,
     *            K1, K2, L, KENTRY, K, MZIPBL, IHN, KE, KB, KK, NNET
      INTEGER     N1
      PARAMETER   (MZIPBL=4)
      CHARACTER   INP*30, FNAME*30, YN*1, BLASH*10, LOCAL*30,
     *            LINE*(MXWIDE), ADDR(MXHIGH)*(MXWIDE), CNTRY*30,
     *            ZIPBL*(MZIPBL), LIHN*4
      LOGICAL     RNO, LEOF, LLOCAL, LUPPER, LA, LHN, LHNONE, LAPREV
      DATA        LOCAL /'USA'/, ZIPBL /' '/
      DATA        N1 /1/
C-----------------------------------------------------------------------
      L1 = 1
      L2 = 2
      L5 = 5
      L6 = 6
C                                       Open input file:
 110  CONTINUE
      WRITE (L6, 1110)
      READ (L5, 1115, ERR=110) INP
      FNAME = INP(:ITRIM(INP)) // '.ADR'
      OPEN (UNIT=L1, FILE=FNAME, STATUS='OLD', ERR=117)
         WRITE (L6, 1116) FNAME
         GO TO 120
 117  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 110
 120  CONTINUE
C                                       Open output file:
      FNAME = INP(:ITRIM(INP)) // '.TEX'
      WRITE (L6, 1150) FNAME
      OPEN (UNIT=L2, FILE=FNAME, STATUS='NEW',
     *      CARRIAGECONTROL='LIST')
C                                       initialize for TEX:
      WRITE (L2, 1180)
      KHIGH = MXHIGH
      KWIDE = MXWIDE
      KSKIP = 0
      BLASH = 'athn'
C-----------------------------------------------------------------------
      LEOF = .FALSE.
      KENTRY = 0
C                                       Read next label:
 310  CONTINUE
      IF (LEOF) GO TO 910
         DO 320 I = 1, MXHIGH
            ADDR(I) = ' '
 320        CONTINUE
         K1 = 0
         K2 = 0
         LLOCAL = .FALSE.
C                                       Read next line of label:
 330     CONTINUE
         IF (LEOF) GO TO 310
         READ (L1, 1330, END=340) LINE
            GO TO 350
 340     CONTINUE
            LINE = ' '
            LEOF = .TRUE.
 350     CONTINUE
C                                       Looping logic:
         IF ((LINE.EQ.' ').AND.(K1.EQ.0)) GO TO 330
         K1 = K1 + 1
         IF ((LINE.EQ.' ').AND.(K1.GT.0)) GO TO 410
C                                       Backslash logic:
         IF (LINE(1:1).NE.'\') GO TO 358
            IF (BLASH.EQ.' ') GO TO 330
            IF (LINE(2:2).NE.' ') GO TO 356
               WRITE (L6, 1354) LINE
               GO TO 330
 356        CONTINUE
            IF (BLASH.EQ.'\') GO TO 358
            IF (INDEX (BLASH, LINE(2:2)).EQ.0) GO TO 330
 358     CONTINUE
C                                       Logic to delete 'LOCAL' lines:
         IF (LINE.NE.LOCAL) GO TO 359
            IF (LLOCAL) WRITE (L6, 1359) LOCAL(:ITRIM (LOCAL)), K1
            LLOCAL = .TRUE.
            GO TO 330
 359     CONTINUE
C                                       Kluges for first name(s):
         IF (K1.NE.2) GO TO 380
            IF (LINE.EQ.'[null]') GO TO 360
               LINE = LINE(:ITRIM(LINE)) // ' ' // ADDR(1)
               GO TO 370
 360        CONTINUE
               LINE = ADDR(1)
 370        CONTINUE
 380     CONTINUE
C                                       Transformations for TEX:
         CALL TEXFIX (LINE, '  ', ' ')
         CALL TEXFIX (LINE, ' & ', '~&~')
         CALL TEXFIX (LINE, '~&~', '~|AMPER|~')
         CALL TEXFIX (LINE, '&', '|AMPER|~')
         CALL TEXFIX (LINE, '|AMPER|', '\&')
         CALL TEXFIX (LINE, '%', '\char37 ')
         IF (LINE(1:1).NE.'\') CALL TEXFIX (LINE, '. ', '.~')
         CALL TEXFIX (LINE, '_', '\_')
         CALL TEXFIX (LINE, '#', '\#')
         CALL TEXFIX (LINE, '<', '$<$')
         CALL TEXFIX (LINE, '>', '$>$')
C                                       Now add the line to ADDR():
C                                       (note special case for first)
C                                       (name line, K1.EQ.2)
         IF (K2.GE.MXHIGH) WRITE (L6, 1385) MXHIGH, LINE
         IF (K1.NE.2) K2 = MIN (K2 + N1, MXHIGH)
         ADDR(K2) = LINE
         IF (ITRIM (ADDR(K2)).GT.KWIDE) WRITE (L6, 1390)
     *            K2, KWIDE, LINE(:ITRIM(LINE))
         GO TO 330
C-----------------------------------------------------------------------
C                                       Now to write it all out for TEX:
 410  CONTINUE
C                                       Loop over possible machines:
      LHNONE = .FALSE.
      LAPREV = .FALSE.
      LA = .FALSE.
      DO 490 IHN = 1, 9
         WRITE (LIHN, '(''\h'', I1)') IHN
C                                       will this machine need a form?
         LHN = .FALSE.
         DO 420 I = 1, K2
            IF (ADDR(I)(1:3).EQ.LIHN) LHN = .TRUE.
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:3).EQ.'\h ')) LHN = .TRUE.
            IF ((ADDR(I)(1:3).EQ.'\a ').AND.
     *            (INDEX (ADDR(I)(3:), ' CP ').NE.0)) LA = .TRUE.
 420        CONTINUE
         LHNONE = LHNONE.OR.LHN
         IF (.NOT.LA.AND..NOT.LHN) GO TO 490
         IF (LAPREV.AND..NOT.LHN) GO TO 490
         LAPREV = LAPREV.OR.LA
C                                       yes, it will!
 430     CONTINUE
         KENTRY = KENTRY + 1
         WRITE (L2, 1430) IHN
C                                       First, the address lines:
         J = 0
         DO 440 I = 1, K2
            IF (ADDR(I)(1:1).EQ.'\') GO TO 440
               J = J + 1
               IF (J.GT.(MXADDR+1)) WRITE (L6, 1435) (MXADDR+1), J,
     *                                       ADDR(I)(:ITRIM(ADDR(I)))
               WRITE (L2, 1440) 'ADDR', CHAR(ICHAR('A')-1+J),
     *                                 ADDR(I)(:ITRIM(ADDR(I)))
 440        CONTINUE
C                                       the telephone & network lines:
         NNET = 0
         DO 450 I = 1, K2
            IF (ADDR(I)(1:3).NE.'\t ') GO TO 443
               WRITE (L2, 1450) 'TEL', ADDR(I)(4:ITRIM(ADDR(I)))
               GO TO 450
 443        IF (ADDR(I)(1:3).NE.'\n ') GO TO 450
               NNET = NNET + 1
               IF (NNET.GT.MXNET) WRITE (L6, 1443) MXNET, NNET,
     *                                 ADDR(I)(:ITRIM(ADDR(I)))
               WRITE (L2, 1440) 'NET', CHAR(ICHAR('A')-1+NNET),
     *                                 ADDR(I)(4:ITRIM(ADDR(I)))
 450        CONTINUE
C                                       now for the hardware lines:
         DO 480 I = 1, K2
            IF (ADDR(I)(1:3).EQ.LIHN) GO TO 460
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:3).EQ.'\h ')) GO TO 460
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:4).EQ.'\h0 ')) GO TO 460
               GO TO 480
 460        CONTINUE
            J = 3
 465        CONTINUE
               J = J + 1
               IF (J.GT.MXWIDE) GO TO 480
               IF (ADDR(I)(J:J).EQ.' ') GO TO 465
C                                       non-blank field found:
               KB = INDEX (ADDR(I)(J:MXWIDE), ' ')
               KE = INDEX (ADDR(I)(J:J+KB-1), '=')
               IF ((KE.LT.2).OR.(KB.LT.(KE+1))) GO TO 475
C                                       write the field out:
               WRITE (L2, 1450) ADDR(I)(J:J+KE-2), ADDR(I)(J+KE:J+KB-2)
               J = J + KB - 1
               GO TO 465
 475        CONTINUE
               WRITE (L6, 1475) J, KE, KB, ADDR(I)(:ITRIM(ADDR(I)))
 480        CONTINUE
         WRITE (L2, 1490)
 490     CONTINUE
      IF ((LAPREV.AND..NOT.LHNONE).OR.(LHNONE.AND..NOT.LAPREV))
     *   WRITE (L6, 1420) LAPREV, LHNONE, ADDR(1)(:ITRIM(ADDR(1)))
      GO TO 310
C-----------------------------------------------------------------------
C                                       That's all, folks:
 910  CONTINUE
      CLOSE (UNIT=L1)
      WRITE (L2, 1905)
      CLOSE (UNIT=L2)
      WRITE (L6, 1910) KENTRY
C-----------------------------------------------------------------------
 1110 FORMAT (' Enter input address-list-file (w/o .ADR):', $)
 1115 FORMAT (A72)
 1116 FORMAT (' L1=', A)
 1117 FORMAT (' Cannot open file ', A)
 1130 FORMAT (' Do you want RNO output? (y/n):', $)
 1132 FORMAT (A1)
 1150 FORMAT (' L2=', A)
 1180 FORMAT ('\input aipswho:srvymac.tex', /, '\def\FINAL{T}')
 1330 FORMAT (A72)
 1354 FORMAT (' Poorly formatted backslash line:', /, 1X, A)
 1359 FORMAT (' Extra "', A, '" was seen at line', I3, ' of entry!')
 1385 FORMAT (' More than', I4, ' lines in an entry. Extra line:', /, A)
 1390 FORMAT (' Line', I3, ' of an entry is wider than', I3, ':', /,
     *         1X, A)
 1420 FORMAT (' CP/h mismatch for following entry: (CP=', L2, ', \h=',
     *         L2, ')', /, 1X, A)
 1430 FORMAT (/, '\clearform', /, '\def\IHN{', I1, '}')
 1435 FORMAT (' More than', I2, ' lines in address; J=', I2, ':',
     *         /, 1X, A)
 1440 FORMAT ('\def\', A, A1, '{', A, '}')
 1443 FORMAT (' More than', I2, ' network addresses; NNET=', I2, ':',
     *         /, 1X, A)
 1450 FORMAT ('\def\', A, '{', A, '}')
 1475 FORMAT (' Bad key=value field. J,KE,KB=', 3I5, /, 1X, A)
 1490 FORMAT ('\genform')
 1905 FORMAT (/, '\end')
 1910 FORMAT (' Number of forms generated =', I5)
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
      SUBROUTINE TEXFIX (BIGSTR, OLD, NEW)
C-----------------------------------------------------------------------
C     TEXFIX substitutes string NEW for instances of substring OLD in
C     string BIGSTR.
C     DCW, NRAO-CV, 01Apr86,08Jan87.
C-----------------------------------------------------------------------
      CHARACTER   BIGSTR*(*), OLD*(*), NEW*(*)
      INTEGER     I, K
      INTEGER     ITRIM
      CHARACTER   TEMP*120
C
      I = 1
 10   K = INDEX (BIGSTR(I:ITRIM(BIGSTR)), OLD)
      IF (K.LE.0) GO TO 20
         TEMP = BIGSTR(1:I+K-2) // NEW // BIGSTR(I+K+LEN(OLD)-1:)
         BIGSTR = TEMP
         I = I + K + LEN(NEW) - 1
         GO TO 10
 20   RETURN
      END
