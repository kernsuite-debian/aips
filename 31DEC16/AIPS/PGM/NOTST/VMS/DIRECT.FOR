      PROGRAM DIRECT
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
C   Reads an ADR file and generates AIPS Site Survey Directory for all
C   entries with the "CP" attribute. Actually, the output is the TEX
C   input to generate the directory. The "\h" lines are also recognized
C   and separate entries are generated for "\h1", "\h2", ...
C   A crossreference listing is prepared from the keyword=value pairs
C   in the \h entries.
C   DCW, NRAO-CV, 25Mar86,15Jul86 (based on programs LABEL and SURVEY).
C     08Jan87; improved sort 24Mar87; numerous fixes 20-30Apr87.
C     fix for blank hardware lines 24Nov87.
C   Added 'Telescope' to INSTIT (Institution) DATA list 05Apr89, NDW
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     MXLINS, MXHIGH, MXWIDE, MAXXCL
      PARAMETER   (MXLINS=50, MXHIGH=99, MXWIDE=99, MAXXCL=20)
      INTEGER     L1, L2, L3, L5, L6, ITRIM, J, I, KHIGH, KWIDE, KSKIP,
     *            K1, K2, L, KENTRY, K, MZIPBL, IHN, KE, KB, KK, KC, KF,
     *            ISTAT, KMACH, NMACH, IZ, FKF, L4, NXCL
      CHARACTER   NETSRV*12
      PARAMETER   (MZIPBL=4, IZ=0, NETSRV='Net-Service')
      CHARACTER   INP*30, FNAME*30, YN*1, BLASH*10, LOCAL*30,
     *            LINE*(MXWIDE), ADDR(MXHIGH)*(MXWIDE), CNTRY*30,
     *            ZIPBL*(MZIPBL), LIHN*4, CENTRY*4,
     *            COMMND*100, LSTKEY*20, KEY*20, VALUE*60,
     *            LSTVAL*60, NREF*9, CNMACH*9, CKMACH*9, KVALUE*60,
     *            CLNAME*72, CCNTRY*30, CZIP*30, CCITY*72, CINSTN*72,
     *            CSTRET*72, TEMP*80, TEMP2*80, XCLKEY(MAXXCL)*40,
     *            XCL*30, NETKEY*12
      LOGICAL     RNO, LEOF, LLOCAL, LUPPER, LA, LHN, LHNPRV, ONENET,
     *            AERROR
      DATA        LOCAL /'USA'/, ZIPBL /' '/
C-----------------------------------------------------------------------
      L1 = 1
      L2 = 2
      L3 = 3
      L5 = 5
      L6 = 6
C                                       Open input file:
 110  CONTINUE
      WRITE (L6, 1110)
      READ (L5, 1115, ERR=110) INP
      IF (INP.EQ.' ') INP = 'AIPSWHO:WHOGETS'
      FNAME = INP(:ITRIM(INP)) // '.ADR'
      OPEN (UNIT=L1, FILE=FNAME, STATUS='OLD', ERR=117)
         WRITE (L6, 1116) FNAME
         GO TO 120
 117  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 110
 120  CONTINUE
C                                       Open output file:
      I = INDEX (INP, ':')
      IF (I.GT.0) INP = INP(I+1:)
      I = INDEX (INP, ']')
      IF (I.GT.0) INP = INP(I+1:)
      FNAME = INP(:ITRIM(INP)) // '.TEX'
      WRITE (L6, 1150) FNAME
      OPEN (UNIT=L2, FILE=FNAME, STATUS='NEW',
     *      CARRIAGECONTROL='LIST')
C                                       Read xref excludelist file:
 160  CONTINUE
      WRITE (L6, 1160)
      READ (L5, 1115, ERR=160) XCL
      IF (XCL.EQ.' ') XCL = 'AIPSWHO:DRCTALLREF'
      FNAME = XCL(:ITRIM(XCL)) // '.DAT'
      OPEN (UNIT=L4, FILE=FNAME, STATUS='OLD', ERR=167)
         WRITE (L6, 1166) FNAME
         GO TO 170
 167  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 160
 170  CONTINUE
      NXCL = 0
 180  READ(L4,*,END=190) XCLKEY(NXCL+1)
         NXCL = NXCL + 1
         IF (NXCL.LT.MAXXCL) GO TO 180
         STOP 'DIRECT 180: NXCL.GE.MAXXCL!'
 190  CLOSE (UNIT=L4)
      WRITE (L6,*) 'cross-reference exclude-list NXCL=', NXCL
C                                       initialize for TEX:
      WRITE (L2, 1180) 'def\SECRET{T}% internal consumption only?'
      WRITE (L2, 1180) 'def\SHORT{F}%  short form for non-CPs?'
      WRITE (L2, 1180) 'def\FINAL{F}%  draft or final? (final date:)'
      WRITE (L2, 1180) 'if T\FINAL     \day=99\month=13\year=2001 \fi'
      WRITE (L2, 1185) 'def\INP{\uppercase{' // INP(:ITRIM(INP))
     *                                          // '}.ADR}%'
      WRITE (L2, 1185) 'input aipswho:drctmac.tex'
      WRITE (L2, 1330) '%  substitute "direct89" for "drctgenerc":'
      WRITE (L2, 1180) 'input aipswho:drctgenerc.tex'
      WRITE (L2, 1185) 'begindirectory'
      KHIGH = MXHIGH
      KWIDE = MXWIDE
      KSKIP = 0
      BLASH = 'atxhn'
C                                       Open scratch file:
      OPEN (UNIT=L3, FILE='UNSORTED.SCR', STATUS='NEW',
     *   CARRIAGECONTROL='LIST')
C-----------------------------------------------------------------------
      LEOF = .FALSE.
      KENTRY = 0
C                                       Read next label:
 310  CONTINUE
      IF (LEOF) GO TO 510
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
C                                       (codes must appear in BLASH!)
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
C                                       Now add the line to ADDR():
C                                       (note special case for first)
C                                       (name line, K1.EQ.2)
         IF (K2.GE.MXHIGH) WRITE (L6, 1385) MXHIGH, LINE
C         IF (K1.NE.2) K2 = MIN0 (K2 + 1, MXHIGH)
         K2 = MIN (K2 + 1, MXHIGH)
         ADDR(K2) = LINE
         IF (ITRIM (ADDR(K2)).GT.KWIDE) WRITE (L6, 1390)
     *            K2, KWIDE, LINE(:ITRIM(LINE))
         GO TO 330
C-----------------------------------------------------------------------
C                                       entry assembled. in directory?
 410  CONTINUE
      LA = .FALSE.
      LHN = .FALSE.
C                                       Is it a CP?
      DO 415   I = 1, K2
         IF ((ADDR(I)(1:3).EQ.'\a ').AND.
     *         (INDEX (ADDR(I)(3:), ' CP ').NE.0)) LA = .TRUE.
 415     CONTINUE
C                                       does it have any "\h" line(s)?
      DO 420 IHN = 1, 9
         WRITE (LIHN, '(''\h'', I1)') IHN
         DO 420 I = 1, K2
            IF (ADDR(I)(1:3).EQ.LIHN) LHN = .TRUE.
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:3).EQ.'\h ')) LHN = .TRUE.
 420     CONTINUE
C                                       Want BOTH CP and \h:
C                                       (but will take either)
      IF (.NOT.LA.AND..NOT.LHN) GO TO 495
      IF ((LA.AND..NOT.LHN).OR.(LHN.AND..NOT.LA))
     *   WRITE (L6, 1420) LA, LHN, ADDR(1)(:ITRIM(ADDR(1)))
C                                       Generate directory entry:
      WRITE (L2, 1430)
C                                       First, fix TEX special cases:
      DO 435 I = 1, K2
         CALL TEXFIX (ADDR(I), '  ', ' ')
C         CALL TEXFIX (ADDR(I), '. ', '.~')
         CALL TEXFIX (ADDR(I), '<', '$<$')
         CALL TEXFIX (ADDR(I), '>', '$>$')
         CALL TEXFIX (ADDR(I), '#', '\#')
         CALL TEXFIX (ADDR(I), '%', '\%')
         CALL TEXFIX (ADDR(I), '_', '\_')
         CALL TEXFIX (ADDR(I), '&', '\&')
         CALL TEXFIX (ADDR(I), '\& ', '\&\ ')
 435     CONTINUE
C                                       now for the address lines:
      J = 0
      DO 440 I = 1, K2
         IF (ADDR(I)(1:1).EQ.'\') GO TO 440
            TEMP = ADDR(I)
            J = J + 1
            IF (J.EQ.2) GO TO 440
            IF (J.GT.2) GO TO 438
C                                       Kluges for first name(s):
               IF (ADDR(2).EQ.'[null]') GO TO 438
                  TEMP2 = ADDR(2)(:ITRIM(ADDR(2))) // ' ' // TEMP
                  TEMP = TEMP2
 438        WRITE (L2, 1440) 'CPaddrline', TEMP(:ITRIM(TEMP))
 440     CONTINUE
C                                       parse ADDR():
      CALL APARSE (L6, K2, ADDR, LOCAL, AERROR,
     *               CLNAME, CCNTRY, CZIP, CCITY, CINSTN, CSTRET)
      IF (AERROR) GO TO 454
C                                       add LOCAL to printed address:
         IF (CCNTRY.NE.LOCAL) GO TO 453
            DO 452 I = 1, K2
               IF (ADDR(I).EQ.LOCAL) GO TO 453
 452           CONTINUE
            WRITE (L2, 1440) 'CPaddrline', LOCAL(:ITRIM(LOCAL))
 453     CONTINUE
C
         WRITE (L2, 1440) 'CPlastname', CLNAME(:ITRIM(CLNAME))
         CALL SRTSUB ('PUT', L3, 'CP Lastname', CLNAME, KENTRY+1)
         WRITE (L2, 1440) 'CPcountry', CCNTRY(:ITRIM(CCNTRY))
         CALL SRTSUB ('PUT', L3, 'Country', CCNTRY, KENTRY+1)
         WRITE (L2, 1440) 'CPzip', CZIP(:ITRIM(CZIP))
         WRITE (L2, 1440) 'CPcity', CCITY(:ITRIM(CCITY))
         WRITE (L2, 1440) 'CPinstitute', CINSTN(:ITRIM(CINSTN))
         CALL SRTSUB ('PUT', L3, 'Institute', CINSTN, KENTRY+1)
         WRITE (L2, 1440) 'CPstreet', CSTRET(:ITRIM(CSTRET))
 454  CONTINUE
C                                       phone,net,attribute lines:
      ONENET = .FALSE.
      DO 458 I = 1, K2
         IF (ADDR(I)(1:3).NE.'\t ') GO TO 455
            WRITE (L2, 1440) 'CPtelephone', ADDR(I)(4:ITRIM(ADDR(I)))
            GO TO 458
 455     IF (ADDR(I)(1:3).NE.'\x ') GO TO 456
            WRITE (L2, 1440) 'CPtelex', ADDR(I)(4:ITRIM(ADDR(I)))
            GO TO 458
 456     IF (ADDR(I)(1:3).NE.'\a ') GO TO 457
            WRITE (L2, 1440) 'CPattribute', ADDR(I)(4:ITRIM(ADDR(I)))
            CALL ATTSRT (L3, ADDR(I)(4:ITRIM(ADDR(I))), KENTRY+1)
            GO TO 458
 457     IF (ADDR(I)(1:3).NE.'\n ') GO TO 458
            WRITE (L2, 1440) 'CPnetaddr', ADDR(I)(4:ITRIM(ADDR(I)))
            CALL NETSRT (L3, ADDR(I)(4:ITRIM(ADDR(I))), KENTRY+1,
     *         NETSRV)
            IF (.NOT.ONENET) CALL SRTSUB ('PUT', L3, NETSRV,
     *         'service-on-at-least-one-net', KENTRY+1)
            ONENET = .TRUE.
 458     CONTINUE
C
      WRITE (L2, 1455)
C                                       now for the hardware lines:
      DO 490 IHN = IZ, 9
         LHNPRV = .FALSE.
         WRITE (LIHN, '(''\h'', I1)') IHN
         DO 480 I = 1, K2
            IF (ADDR(I).EQ.LIHN) ADDR(I) = LIHN // ' ?=?'
            IF ((IHN.EQ.1).AND.(ADDR(I).EQ.'\h'))
     *                           ADDR(I) = LIHN // ' ?=?'
            IF (ADDR(I)(1:3).EQ.LIHN) GO TO 460
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:3).EQ.'\h ')) GO TO 460
               GO TO 480
 460        CONTINUE
C                                       Another machine?
            IF (LHNPRV) GO TO 462
               KENTRY = KENTRY + 1
               WRITE (CENTRY, '(I4)') KENTRY
               KF = 0
 461           KF = KF + 1
               IF (CENTRY(KF:KF).EQ.' ') GO TO 461
               IF (IHN.EQ.0) GO TO 4615
                  WRITE (L2, 1460) 'CPmachine', CENTRY(KF:4)
                  GO TO 4617
 4615          WRITE (L2, 1460) 'CPsitedata', CENTRY(KF:4)
 4617          LHNPRV = .TRUE.
 462        IF (ADDR(I)(1:3).EQ.LIHN)
     *         WRITE (L2, 1465) ADDR(I)(5:MAX(ITRIM(ADDR(I)),5))
            IF ((IHN.EQ.1).AND.(ADDR(I)(1:3).EQ.'\h '))
     *         WRITE (L2, 1465) ADDR(I)(4:MAX(ITRIM(ADDR(I)),4))
            J = 3
 465        CONTINUE
               J = J + 1
               IF (J.GT.MXWIDE) GO TO 480
               IF (ADDR(I)(J:J).EQ.' ') GO TO 465
C                                       non-blank field found:
               KB = INDEX (ADDR(I)(J:MXWIDE), ' ')
               KE = INDEX (ADDR(I)(J:J+KB-2), '=')
               IF ((KE.LT.2).OR.(KB.LT.(KE+1))) GO TO 475
C                                       write the field(s) out:
               KF = KE + 1
 467           CONTINUE
                  KC = INDEX (ADDR(I)(J+KF-1:J+KB-2), ',')
                  IF (KC.EQ.0) KC = KB - KF + 1
                  KVALUE = ADDR(I)(J+KF-1:J+KF+KC-3)
                  CALL SRTSUB ('PUT', L3, ADDR(I)(J:J+KE-2), KVALUE,
     *                                                      KENTRY)
                  KF = KF + KC
                  IF (KF.LT.KB) GO TO 467
               J = J + KB - 1
               GO TO 465
 475        CONTINUE
               WRITE (L6, 1475) J, KE, KB, ADDR(I)(:ITRIM(ADDR(I)))
 480        CONTINUE
         IF (LHNPRV) WRITE (L2, 1455)
         IF (LHNPRV.AND.(IHN.EQ.0)) KENTRY = KENTRY - 1
 490     CONTINUE
      WRITE (L2, 1455)
 495  GO TO 310
C-----------------------------------------------------------------------
 510  CONTINUE
      WRITE (L2, 1185) 'enddirectory'
      CLOSE (UNIT=L1)
      WRITE (L6, 1510) KENTRY
      CLOSE (UNIT=L3)
C                                       Now to produce cross-reference:
      COMMND = 'Sort/Key=(Position:1,Size:72,Ascending)/Stable '
     *               // 'UNSORTED.SCR ' // 'SORTED.SCR'
      CALL DCLCMD (L6, COMMND, ISTAT)
      OPEN (UNIT=L3, FILE='SORTED.SCR', STATUS='OLD')
      WRITE (L2, 1185) 'begincrossref'
      LSTKEY = '[initkey]'
      LSTVAL = '[initval]'
 520  CALL SRTSUB ('GET', L3, KEY, VALUE, KMACH)
      IF (KMACH.EQ.-9999) GO TO 590
C                                       xref excludelist check:
         DO 525 I = 1, NXCL
            IF (KEY.EQ.XCLKEY(I)) GO TO 520
 525        CONTINUE
C
         IF (KEY.EQ.LSTKEY) GO TO 530
            IF (LSTVAL.NE.'[initval]') WRITE (L2, 1520)
     *                                    CNMACH(:ITRIM(CNMACH))
            IF (LSTKEY.NE.'[initkey]') WRITE (L2, 1455)
C                                       kluge suppresses cannisters:
            IF ((LSTKEY.EQ.'CAN').OR.(LSTKEY.EQ.'NAME'))
     *                           WRITE (L2, 1180) 'fi'
            WRITE (L2, '(1X)')
            IF (KEY.EQ.'CAN')    WRITE (L2, 1180) 'if T\SECRET%'
            IF (KEY.EQ.'NAME')   WRITE (L2, 1180) 'if F\SHORT%'
            WRITE (L2, 1460) 'XRkey', KEY(:ITRIM(KEY))
            LSTKEY = KEY
            LSTVAL = '[initval]'
            WRITE (L6, '('' KEY= '', A)') KEY(:ITRIM(KEY))
 530     CONTINUE
         IF (VALUE.EQ.LSTVAL) GO TO 540
            IF (LSTVAL.NE.'[initval]') WRITE (L2, 1520)
     *                                    CNMACH(:ITRIM(CNMACH))
            WRITE (L2, 1460) 'XRvalue', VALUE(:ITRIM(VALUE))
            LSTVAL = VALUE
            NMACH = 0
 540     CONTINUE
         CALL MAKNUM (KMACH, CKMACH)
         NREF = CKMACH
         WRITE (L2, 1440) 'XRmachine', NREF(:ITRIM(NREF))
         NMACH = NMACH + 1
         CALL MAKNUM (NMACH, CNMACH)
         GO TO 520
 590  CONTINUE
      CLOSE (UNIT=L3)
      IF (LSTVAL.NE.'[initval]') WRITE (L2, 1520)
     *                                 CNMACH(:ITRIM(CNMACH))
      IF (LSTKEY.NE.'[initkey]') WRITE (L2, 1455)
      WRITE (L2, 1185) 'endcrossref'
C                                       That's all, folks:
 910  CONTINUE
      WRITE (L2, 1185) 'vfill'
      WRITE (L2, 1185) 'eject'
      WRITE (L2, 1185) 'end'
      CLOSE (UNIT=L2)
      COMMND = 'Delete/Log ' // 'UNSORTED.SCR;*,SORTED.SCR;*'
      CALL DCLCMD (L6, COMMND, ISTAT)
C-----------------------------------------------------------------------
 1110 FORMAT (' Enter input address-list-file (w/o .ADR):', $)
 1115 FORMAT (A72)
 1116 FORMAT (' L1=', A)
 1117 FORMAT (' Cannot open file ', A)
 1150 FORMAT (' L2=', A)
 1160 FORMAT (' Enter input xref-exclude-file (w/o .DAT):', $)
 1166 FORMAT (' L4=', A)
 1180 FORMAT ('\', A)
 1185 FORMAT (/, '\', A)
 1330 FORMAT (A)
 1354 FORMAT (' Poorly formatted backslash line:', /, 1X, A)
 1359 FORMAT (' Extra "', A, '" was seen at line', I3, ' of entry!')
 1385 FORMAT (' More than', I4, ' lines in an entry. Extra line:', /, A)
 1390 FORMAT (' Line', I3, ' of an entry is wider than', I3, ':', /,
     *         1X, A)
 1420 FORMAT (' CP/h mismatch for following entry: (CP=', L2, ', \h=',
     *         L2, ')', /, 1X, A)
 1430 FORMAT (/, '\CP{', /, '\CPaddr{')
 1440 FORMAT ('\', A, '{', A, '}%')
 1455 FORMAT ('}%')
 1460 FORMAT ('\', A, '{', A, '}{%')
 1465 FORMAT (A)
 1475 FORMAT (' Bad key=value field. J,KE,KB=', 3I5, /, 1X, A)
 1510 FORMAT (' Number of machines =', I5)
 1520 FORMAT ('\XRnmach{', A, '}}%')
      END
      INTEGER*4 FUNCTION ITRIM (STRING)
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
      SUBROUTINE DCLCMD (L6, COMMND, ISTAT)
C-----------------------------------------------------------------------
C     Execute a DCL command string. Failure signaled by nonzero ISTAT.
C     DCW, NRAO-CV, 07Mar85.
C-----------------------------------------------------------------------
      INTEGER     ISTAT, L6
      CHARACTER   COMMND*(*)
C
      INCLUDE     '($SSDEF)'
      INTEGER     STATUS, LIB$SPAWN, ITRIM
C-----------------------------------------------------------------------
      WRITE (L6, 1010) COMMND(1:MIN0(ITRIM(COMMND),72))
      STATUS = LIB$SPAWN (COMMND,,,,,,,,,)
      ISTAT = 0
      IF (STATUS.EQ.SS$_NORMAL) GO TO 20
         WRITE (L6, '('' STATUS='', Z8)') STATUS
         ISTAT = 1
 20   RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (' DCLCMD: ', A)
      END
      SUBROUTINE TEXFIX (BIGSTR, OLD, NEW)
C-----------------------------------------------------------------------
C     TEXFIX substitutes string NEW for instances of substring OLD in
C     string BIGSTR.
C     DCW, NRAO-CV, 01Apr86,08Jan87.
C-----------------------------------------------------------------------
      CHARACTER   BIGSTR*(*), OLD*(*), NEW*(*)
      INTEGER     I, K, ITRIM
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
      SUBROUTINE MAKNUM (N, STR)
C-----------------------------------------------------------------------
C     MAKNUM formats integer N into string STR and removes leading
C     blanks. ITRIM can then be used to effectively remove trailing
C     blanks as well. DCW, NRAO-CV, 02Apr86.
C-----------------------------------------------------------------------
      INTEGER     N
      CHARACTER   STR*(*)
      CHARACTER   STRTMP*20
C
      WRITE (STRTMP, '(I10)') N
 10   IF ((STRTMP(1:1).NE.' ').OR.(STRTMP.EQ.' ')) GO TO 20
         STRTMP = STRTMP(2:)
         GO TO 10
 20   STR = STRTMP
      RETURN
      END
      SUBROUTINE SRTSUB (OP, L3, KEY, VALUE, KMACH)
C-----------------------------------------------------------------------
C     routine to write a record into the keyword-value-entry file which
C     gets sorted to produce the crossreference table of the Directory.
C-----------------------------------------------------------------------
      INTEGER     L3, KMACH
      CHARACTER   OP*(*), KEY*(*), VALUE*(*)
C
      INTEGER     KV1, KV2, ITRIM
      CHARACTER   CV1*1, CV2*60, MONTH(12)*3, SRTFMT*20, JUSTFY*100,
     *            LOCAL*100
      DATA        MONTH /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     *                  'AUG','SEP','OCT','NOV','DEC'/,
     *            SRTFMT /'(A17, A47, I8)'/,
     *            JUSTFY /' '/
C-----------------------------------------------------------------------
      IF (OP.EQ.'GET') GO TO 520
      IF (OP.NE.'PUT') STOP 'SRTSUB: unrec val of OP!'
      LOCAL = VALUE
      KV1 = ITRIM (LOCAL)
C                                       recode the values for sorting:
      DO 468 KV2 = 1, KV1
         CV1 = LOCAL(KV2:KV2)
         IF ((.NOT.((CV1.GE.'0').AND.(CV1.LE.'9')))
     *      .AND.(CV1.NE.'+').AND.(CV1.NE.'-')
     *      .AND.(CV1.NE.'.')) GO TO 469
 468     CONTINUE
C                                       it's a number; justify decimal:
      CV2 = LOCAL
      IF (LOCAL(1:1).EQ.'.') CV2 = '0' // LOCAL
      LOCAL = CV2
      CV2 = ' '
      KV2 = INDEX (LOCAL,'.')
      IF (KV2.LE.0) CV2(20-KV1:) = LOCAL
      IF (KV2.GT.0) CV2(20-KV2+1:) = LOCAL
      LOCAL = '<n>' // CV2
      GO TO 474
C                                       not a number; a date?
 469  CV1 = LOCAL(1:1)
      IF (.NOT.((CV1.GE.'0').AND.(CV1.LE.'9'))) GO TO 473
      CV1 = LOCAL(2:2)
      IF (.NOT.((CV1.GE.'0').AND.(CV1.LE.'9'))) GO TO 473
      CV2 = LOCAL(3:5)
      DO 470 KV2 = 1,12
         IF (CV2.EQ.MONTH(KV2)) GO TO 472
 470     CONTINUE
      GO TO 473
 472  CV1 = LOCAL(6:6)
      IF ((CV1.NE.'8').AND.(CV1.NE.'9')) GO TO 473
      CV1 = LOCAL(7:7)
      IF (.NOT.((CV1.GE.'0').AND.(CV1.LE.'9'))) GO TO 473
C                                       it's a date; recode it:
         WRITE (CV2,'(''<d> '',A,I2.2,A)') LOCAL(6:7), KV2,
     *                                       LOCAL(1:2)
         LOCAL = CV2
         GO TO 474
C                                       just a string:
 473  CV2 = LOCAL
      LOCAL = '<s> ' // CV2
C
 474  WRITE (L3, SRTFMT) KEY // JUSTFY, LOCAL // JUSTFY, KMACH
      GO TO 900
C-----------------------------------------------------------------------
 520  READ (L3, SRTFMT, END=590) KEY, LOCAL, KMACH
C                                       reconstruct values:
         IF (LOCAL(1:3).NE.'<s>') GO TO 522
            CV2 = LOCAL(5:)
            LOCAL = CV2
            GO TO 526
 522     IF (LOCAL(1:3).NE.'<n>') GO TO 524
            CV2 = LOCAL(5:)
 523        LOCAL = CV2
            IF (LOCAL(1:1).NE.' ') GO TO 526
               CV2 = LOCAL(2:)
               GO TO 523
 524     IF (LOCAL(1:3).NE.'<d>') STOP 'unrec value field code!!!'
            READ (LOCAL(7:8),'(I2)') KV1
            CV2 = LOCAL(9:10) // MONTH(KV1) // LOCAL(5:6)
            LOCAL = CV2
 526     CONTINUE
      VALUE = LOCAL
      GO TO 900
 590  KMACH = -9999
C
 900  RETURN
      END
      SUBROUTINE NETSRT (L3, ADDR, KMACH, NETSRV)
C-----------------------------------------------------------------------
C     routine to call SRTSUB with a network address.
C     tries to recognize domains so that cross-ref can have lists of
C     addresses by domain. DCW, 27Mar87.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     L3, KMACH
      CHARACTER   ADDR*(*), NETSRV*(*)
C
      INTEGER     I, J, K, ITRIM
      CHARACTER   PATTRN*10, KEY*20
C
C-----------------------------------------------------------------------
C
      I = ITRIM (ADDR)
C                                       BITNET?
      PATTRN= '.bitnet'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 15
      IF (INDEX(ADDR(:I),PATTRN(:J)).GT.0) GO TO 15
      PATTRN= '.netnorth'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 15
      PATTRN= '.cdn'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 15
      PATTRN= '@ac.uk'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 15
      GO TO 20
 15      KEY = 'Net=BITNET/EARN'
         CALL SRTSUB ('PUT', L3, NETSRV, 'BITNET/EARN-service', KMACH+1)
         GO TO 80
C                                       Internet?
 20   CONTINUE
      PATTRN = '.arpa'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 25
      PATTRN = '.edu'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 25
      PATTRN = '.com'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 25
      PATTRN = '.net'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 25
      PATTRN = '.gov'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 25
      GO TO 30
 25      KEY = 'Net=ARPA/Internet'
         CALL SRTSUB ('PUT', L3, NETSRV, 'ARPA/Internet-service',
     *                                                      KMACH+1)
         GO TO 80
C                                       UUCP?
 30   CONTINUE
      PATTRN= '.uucp'
      J = ITRIM (PATTRN)
      IF (ADDR(I-J+1:I).EQ.PATTRN(:J)) GO TO 35
      IF (INDEX(ADDR,'!').GT.0) GO TO 35
      GO TO 40
 35      KEY = 'Net=UUCP'
         CALL SRTSUB ('PUT', L3, NETSRV, 'UUCP-service', KMACH+1)
         GO TO 80
C                                       SPAN?
 40   CONTINUE
      IF ((INDEX(ADDR,'::').LE.0) .AND. (INDEX(ADDR,'.SPAN').LE.0)
     *   .AND. (INDEX(ADDR,'.span').LE.0) .AND.
     *   (INDEX(ADDR,'.HEPNET').LE.0) .AND.
     *   (INDEX(ADDR,'.hepnet').LE.0)) GO TO 50
         KEY = 'Net=SPAN/HEPNET'
         CALL SRTSUB ('PUT', L3, NETSRV, 'SPAN/HEPNET-service', KMACH+1)
         GO TO 80
C
 50   CONTINUE
      GO TO 70
C
 70   KEY = 'Net=Miscellaneous'
C
 80   CONTINUE
      CALL SRTSUB ('PUT', L3, KEY, ADDR, KMACH)
C
      RETURN
      END
      SUBROUTINE ATTSRT (L3, ADDR, KMACH)
C-----------------------------------------------------------------------
C     routine to call SRTSUB with attributes for crossref.
C     DCW, 27Mar87.
C-----------------------------------------------------------------------
      INTEGER     L3, KMACH
      CHARACTER   ADDR*(*)
C
      INTEGER     I, J, K, ITRIM
      CHARACTER   KEY*20, LOCAL*80, TEMP*80
C
C-----------------------------------------------------------------------
C
      LOCAL = ADDR
      I = ITRIM (LOCAL)
      KEY = 'Attributes'
      J = 0
 10   J = J + 1
      IF (LOCAL(1:1).NE.' ') GO TO 20
         TEMP = LOCAL(2:)
         LOCAL = TEMP
         GO TO 10
 20   K = INDEX (LOCAL, ' ')
      TEMP = LOCAL(:K-1)
      CALL SRTSUB ('PUT', L3, KEY, TEMP, KMACH)
      TEMP = LOCAL(K+1:)
      LOCAL = TEMP
      IF (LOCAL.NE.' ') GO TO 10
C
      RETURN
      END
      SUBROUTINE APARSE (L6, NADDR, ADDR, LOCAL, AERROR,
     *                     CLNAME, CCNTRY, CZIP, CCITY, CINSTN, CSTRET)
C-----------------------------------------------------------------------
C     routine to parse an ADDR() array of character strings in ADR
C     format to extract selected items (postal code, country,
C     "institution", city, last-name, ...)
C     DCW, NRAO-CV, 30Mar87.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     L6, NADDR
      LOGICAL     AERROR, LUPPER
      CHARACTER   ADDR(NADDR)*(*), LOCAL*(*), CLNAME*(*), CCNTRY*(*),
     *               CZIP*(*), CCITY*(*), CINSTN*(*), CSTRET*(*)
C
      INTEGER     ECODE, ITRIM, I, J, NINST, L, K, L2
      LOGICAL     DBG
      PARAMETER   (NINST = 60, DBG=.FALSE.)
      CHARACTER   INSTIT(NINST)*14, TEMP*72
C
      DATA  INSTIT /
     *   'Observatory',    'Obs.',  'Osservatorio',   'Observatoire',
     *   'Observatorium',  'Opservatoija',   'Observatories',
     *   'Sterrenwacht',   'Sterrewacht',    'Sternwarte',
     *   'Institute',      'Inst.',    'Institut', 'Instituto',
     *   'College',        'Coll.',
     *   'University',     'Univ.',    'Universite', 'Universidad',
     *   'Corporation',    'Corp.',    'Inc.',  'Ltd.',
     *   'Company',        'Co.',      'Center',   'Ctr.',  'Centre',
     *   'Laboratory',     'Lab.',     'Lab',   'Laboratorium',
     *   'Laboratories',   'Labs.',    'Labs',  'Division', 'Div.',
     *   'Department',     'Dept.',    'Unit',  'Group',
     *   'Systems',        'Society',  'Association', 'Assoc.',
     *   'Found.',         'Administration', 'Admin.',   'Headquarters',
     *   'School',         'Sch.',     'Planetarium',    'Plan.',
     *   'Magazine',       'Museum',   'Service', 'Services', 'Survey',
     *   'Telescope' /
C-----------------------------------------------------------------------
      AERROR = .FALSE.
      CZIP = ' '
      CCNTRY = ' '
      CINSTN = ' '
      CSTRET = ' '
      CCITY = ' '
      CLNAME = ' '
      IF (NADDR.LT.4) WRITE (L6, *) 'APARSE, warning: NADDR.LT.4 !'
      ECODE = 1
      IF (NADDR.LT.2) GO TO 980
      IF (.NOT.DBG) GO TO 4
         DO 3 I = 1, NADDR
 3       WRITE (6,'('' APARSE, ADDR('',I2,'')='',A)')
     *                                 I, ADDR(I)(:ITRIM(ADDR(I)))
 4    CONTINUE
C                                       Last name:
      ECODE = 2
      IF ((ADDR(1)(1:1).EQ.'\').OR.(ADDR(2)(1:1).EQ.'\')) GO TO 980
      CLNAME = ADDR(1)(:ITRIM (ADDR(1))) // ', ' //
     *                                       ADDR(2)(:ITRIM(ADDR(2)))
      IF (DBG) WRITE (6,*) 'CLNAME=', CLNAME(:ITRIM(CLNAME))
C                                       Country:
      J = NADDR
C                                       Look for last non-backslash:
 210  J = J - 1
      IF ((ADDR(J)(1:1).EQ.'\').AND.(J.GT.1)) GO TO 210
      ECODE = 210
      IF (J.LT.3) GO TO 980
C                                       Check for upper case:
C                                       (Default to local country)
      LUPPER = .FALSE.
      CCNTRY = LOCAL
      L = ITRIM (ADDR(J))
      IF (L.LE.0) GO TO 230
         DO 220 I = 1, L
            K = ICHAR (ADDR(J)(I:I))
            IF ( ((K.GE.ICHAR('A')).AND.(K.LE.ICHAR('Z')))
     *      .OR. ((K.GE.ICHAR('0')).AND.(K.LE.ICHAR('9')))
     *      .OR.  (K.EQ.ICHAR(' '))
     *      .OR.  (K.EQ.ICHAR('-')) ) GO TO 220
               GO TO 230
 220        CONTINUE
         LUPPER = .TRUE.
         CCNTRY = ADDR(J)
 230  CONTINUE
      IF (DBG) WRITE (6,*) 'CCNTRY=', CCNTRY
C                                       Postal Code:
      IF (.NOT.LUPPER) GO TO 320
 310     J = J - 1
         IF ((ADDR(J)(1:1).EQ.'\').AND.(J.GT.1)) GO TO 310
 320  ECODE = 320
      IF (J.LT.3) GO TO 980
      CCITY = ADDR(J)
C                                       parse "city, state zip":
      IF ((CCNTRY.NE.'USA').AND.(CCNTRY.NE.'CANADA')) GO TO 322
         I = INDEX (CCITY, ',')
         ECODE = 321
         IF (I.LE.0) GO TO 980
         IF (CCITY(I+1:I+1).NE.' ') GO TO 980
         K = INDEX (CCITY(I+2:), ' ')
         I = I + K + 1
         IF (I.GT.ITRIM(CCITY)) GO TO 980
         CZIP = CCITY(I+1:)
         TEMP = CCITY(:I-1)
         CCITY = TEMP
         GO TO 330
C                                       parse ENGLAND postal code:
 322  IF (CCNTRY.NE.'ENGLAND') GO TO 324
         ECODE = 323
C                                       locate last comma:
         I = INDEX (CCITY, ',')
         IF (I.LE.0) GO TO 980
         I = I + 1
         K = INDEX (CCITY(I:), ',')
         IF (K.GT.0) I = I + K + 1
         IF (INDEX (CCITY(I:), ',').NE.0) GO TO 980
C                                       must be followed by blank:
         IF (CCITY(I:I).NE.' ') GO TO 980
C                                       locate next-to-last blank:
         L2 = ITRIM(CCITY)
         K = 0
 323     L = I
         I = I + K
         K = INDEX (CCITY(I+1:L2), ' ')
         IF ((I.LT.L2).AND.(K.GT.0)) GO TO 323
         CZIP = CCITY(L+1:)
         TEMP = CCITY(:L-1)
         CCITY = TEMP
         GO TO 330
 324  CONTINUE
C                                       strip leading blanks:
 330  IF ((CZIP.EQ.' ').OR.(CZIP(1:1).NE.' ')) GO TO 340
 335     TEMP = CZIP(2:)
         CZIP = TEMP
         IF (CZIP(1:1).EQ.' ') GO TO 335
 340  CONTINUE
      IF (DBG) WRITE (6,*) 'CZIP=', CZIP(:ITRIM(CZIP)), ', CCITY=',
     *                           CCITY(:ITRIM(CCITY))
C                                       Institution:
C                                       (street accumulates leftovers)
      CINSTN = ' '
 410  J = J - 1
      IF ((ADDR(J)(1:1).EQ.'\').AND.(J.GT.0)) GO TO 410
      IF (J.LT.3) GO TO 425
      DO 420 I = 1, NINST
         IF (INDEX (ADDR(J), INSTIT(I)(:ITRIM(INSTIT(I)))).NE.0)
     *                                                GO TO 430
 420     CONTINUE
      IF (CSTRET.NE.' ') GO TO 422
         CSTRET = ADDR(J)
         GO TO 424
 422  TEMP = ADDR(J)(:ITRIM(ADDR(J))) // ', ' // CSTRET
      CSTRET = TEMP
 424  IF (J.GT.2) GO TO 410
 425  CINSTN = '(no institute?)'
      GO TO 490
 430  TEMP = '\N{Inst.}'
      I = ITRIM(TEMP)
      K = INDEX (ADDR(J), TEMP(:I))
      IF (K.EQ.0) GO TO 435
         TEMP = ADDR(J)(:K-1) // ADDR(J)(K+I:)
         ADDR(J) = TEMP
 435  CINSTN = ADDR(J)
 490  CONTINUE
      IF (DBG) WRITE (6,*) 'CINSTN=', CINSTN
C                                       finish off the 'street':
 510  J = J - 1
      IF (J.LT.3) GO TO 590
         IF (CSTRET.NE.' ') GO TO 520
            CSTRET = ADDR(J)(:ITRIM(ADDR(J)))
            GO TO 510
 520     CONTINUE
            TEMP = ADDR(J)(:ITRIM(ADDR(J))) // ', ' // CSTRET
            CSTRET = TEMP
            GO TO 510
 590  CONTINUE
C
      GO TO 990
C                                       oops---error:
 980  WRITE (L6,*) 'APARSE error', ECODE, ' for: "',
     *                                 CLNAME(:ITRIM(CLNAME)), '"'
      AERROR = .TRUE.
C
 990  RETURN
      END
