      PROGRAM MAKDIF
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
C   Program to read *.TEX produced by program DIRECT, and parse the
C   TeX-macro fields to produce a DIF-format file of the AIPS Site
C   Survey data. DCW, NRAO-CV, 12Mar87/21Apr/26Feb88.
C   Small mods to expand WTS table, and to pass AP ? data and nonradio
C     fraction to .DIF file correctly       AHB 06Mar89
C-----------------------------------------------------------------------
      IMPLICIT    NONE
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C                                       Local constants:
      INTEGER     MXRPTF, MXWIDE, MXHIGH
      LOGICAL     KDBG
      PARAMETER   (MXRPTF=7, MXWIDE=100, MXHIGH=7, KDBG=.FALSE.)
C                                       Local variables:
      INTEGER     ITRIM, J, I, K, ISTAT, NTUPLE, NADDR
      LOGICAL     CPMACH, CPSEEN, FOMACH
      CHARACTER   INP*30, FNAME*30, SPEC*30, TITLE*60, FIELD*80,
     *            LINE*(MXWIDE), TEMP*(MXWIDE), ADDR(MXHIGH)*(MXWIDE),
     *            MKEY*(MXWIDE), TEMP2*(MXWIDE)
C                                       commons for subroutine PUTWT:
      INTEGER     MXNWTS, MXWTFW
      PARAMETER   (MXNWTS=150, MXWTFW=40)
      INTEGER     NWTS
      CHARACTER   WCPU(MXNWTS)*(MXWTFW), WAP(MXNWTS)*(MXWTFW),
     *            WTY(MXNWTS)*1, WWT(MXNWTS)*(MXWTFW)
      COMMON      /PUTWT1/ NWTS
      COMMON      /PUTWT2/ WCPU, WAP, WTY, WWT
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C-----------------------------------------------------------------------
C
 110  CONTINUE
      WRITE (L6, 1110)
      READ (L5, 1115, ERR=110) INP
      IF (INP.EQ.' ') INP = 'WHOGETS'
      FNAME = INP(:ITRIM(INP)) // '.TEX'
      OPEN (UNIT=L1, FILE=FNAME, STATUS='OLD', ERR=117)
         WRITE (L6, 1116) 1, FNAME
         GO TO 120
 117  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 110
 120  CONTINUE
C                                       Read DIF specification file:
 130  CONTINUE
      WRITE (L6, 1130)
      READ (L5, 1115, ERR=130) SPEC
      IF (SPEC.EQ.' ') SPEC = 'AIPSWHO:MAKDIF'
      FNAME = SPEC(:ITRIM(SPEC)) // '.DAT'
      OPEN (UNIT=L2, FILE=FNAME, STATUS='OLD', ERR=137)
         WRITE (L6, 1116) 2, FNAME
         GO TO 140
 137  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 130
 140  CONTINUE
      READ (L2,*) TITLE
      IF (DBG) WRITE (L6,*) TITLE
      NVECTR = 0
 143  NVECTR = NVECTR + 1
         IF (NVECTR.GT.MXVCTR) STOP 'NVECTR.GT.MXVCTR !!'
         KEY(NVECTR) = ' '
         LABEL(NVECTR) = ' '
         FTYPE(NVECTR) = ' '
         LMACH(NVECTR) = ' '
         READ (L2,*,END=145) KEY(NVECTR), LABEL(NVECTR),
     *                              FTYPE(NVECTR), LMACH(NVECTR)
         IF ((KEY(NVECTR).NE.' ').OR.(LABEL(NVECTR).NE.' ')
     *      .OR.(FTYPE(NVECTR).NE.' ').OR.(LMACH(NVECTR).NE.' '))
     *      GO TO 143
 145  NVECTR = NVECTR -1
      WRITE (L6,*) 'NVECTR=', NVECTR
      CLOSE (UNIT=L2)
C                                       Read CPU AIPS-power table:
 150  CONTINUE
      WRITE (L6, 1150)
      READ (L5, 1115, ERR=130) SPEC
      IF (SPEC.EQ.' ') SPEC = 'AIPSWHO:CPUWTS'
      FNAME = SPEC(:ITRIM(SPEC)) // '.DAT'
      OPEN (UNIT=L2, FILE=FNAME, STATUS='OLD', ERR=157)
         WRITE (L6, 1116) 2, FNAME
         GO TO 160
 157  CONTINUE
         WRITE (L6, 1117) FNAME
         GO TO 150
 160  CONTINUE
      NWTS = 0
 163  CONTINUE
         NWTS = NWTS + 1
         IF (NWTS.GT.MXNWTS) STOP 'NWTS.GT.MXNWTS !!'
         WCPU(NWTS) = ' '
         READ (L2,*,END=165) WCPU(NWTS), WAP(NWTS), WTY(NWTS), WWT(NWTS)
         IF (WCPU(NWTS).EQ.' ') GO TO 165
C        IF (DBG) WRITE (L6,*) WCPU(NWTS), WAP(NWTS), WTY(NWTS),
C       *                                                      WWT(NWTS)
         GO TO 163
 165  NWTS = NWTS -1
      WRITE (L6,*) 'NWTS=', NWTS
      CLOSE (UNIT=L2)
C                                       Open scratch DIF file:
      I = INDEX (INP, ':')
      IF (I.GT.0) INP = INP(I+1:)
      I = INDEX (INP, ']')
      IF (I.GT.0) INP = INP(I+1:)
      FNAME = INP(:ITRIM(INP)) // '.SCR'
      WRITE (L6, 1116) 2, FNAME
      OPEN (UNIT=L2, FILE=FNAME, STATUS='NEW',
     *      CARRIAGECONTROL='LIST')
C                                       Open output DIF file:
      FNAME = INP(:ITRIM(INP)) // '.DIF'
      WRITE (L6, 1116) 3, FNAME
      OPEN (UNIT=L3, FILE=FNAME, STATUS='NEW',
     *      CARRIAGECONTROL='LIST')
C                                       Open scratch site file:
      FNAME = 'SITEMACH.TMP'
      WRITE (L6, 1116) 4, FNAME
      OPEN (UNIT=L4, FILE=FNAME, STATUS='SCRATCH', FORM='UNFORMATTED')
      REWIND L4
      ENDFILE L4
      REWIND L4
C                                       Open list-directed-read scratch:
      FNAME = 'DIRECTED.TMP'
      WRITE (L6, 1116) 7, FNAME
      OPEN (UNIT=L7, FILE=FNAME, STATUS='SCRATCH')
C                                       Read TeX, gen scratch DIF:
      CPSEEN = .FALSE.
      NTUPLE = 0
 200  CONTINUE
         READ (L1,'(A)', END=390) LINE
         IF (LINE(1:4).EQ.'\CP{') GO TO 202
         IF (LINE(1:8).EQ.'\CPaddr{') GO TO 210
         IF (LINE(1:12).EQ.'\CPsitedata{') GO TO 251
         IF (LINE(1:11).EQ.'\CPmachine{') GO TO 252
         IF (LINE.EQ.'\enddirectory') GO TO 390
         IF (LINE.EQ.'}%') GO TO 205
         IF (LINE.EQ.' ') GO TO 208
         WRITE (L6,*) 'MAKDIF skipped: ', LINE(:ITRIM(LINE))
         GO TO 200
C                                       \CP seen; initialize:
 202  DO 203 I = 1, NVECTR
 203     VALUE(I) = NA
      DO 204 I = 1, MXHIGH
 204     ADDR(I) = ' '
      NADDR = 0
      FOMACH = .FALSE.
      IF (CPSEEN) STOP 'CP seen while inside CP!?!'
      CPSEEN = .TRUE.
      GO TO 200
C                                       }% seen; ends CP:
 205  IF (.NOT.CPSEEN) STOP 'CP and } not matched!?!'
      CPSEEN = .FALSE.
C                                       go write out machines
C                                       from scratch file:
      GO TO 320
C                                       blanks between CPs:
 208  IF (CPSEEN) STOP 'blank line inside a CP!?!'
      GO TO 200
C                                       read CP information:
 210  READ (L1,'(A)',END=215) LINE
         IF (LINE(:12).EQ.'\CPaddrline{') GO TO 217
         IF (LINE(:13).EQ.'\CPtelephone{') GO TO 220
         IF (LINE(:9).EQ.'\CPtelex{') GO TO 222
         IF (LINE(:13).EQ.'\CPattribute{') GO TO 225
         IF (LINE(:11).EQ.'\CPnetaddr{') GO TO 230
         IF (LINE(:12).EQ.'\CPlastname{') GO TO 235
         IF (LINE(:11).EQ.'\CPcountry{') GO TO 237
         IF (LINE(:7).EQ.'\CPzip{') GO TO 239
         IF (LINE(:10).EQ.'\CPstreet{') GO TO 240
         IF (LINE(:8).EQ.'\CPcity{') GO TO 241
         IF (LINE(:13).EQ.'\CPinstitute{') GO TO 243
         IF (LINE.EQ.'}%') GO TO 248
 215  WRITE (L6,*) 'LINE=', LINE(:60)
      STOP 'unexpected input seen at 210'
 217  NADDR = NADDR + 1
      IF (NADDR.GT.MXHIGH) STOP 'NADDR.GT.MXHIGH!'
      ADDR(NADDR) = LINE(13:ITRIM(LINE)-2)
      GO TO 210
 220  TEMP = LINE(14:ITRIM(LINE)-2)
      CALL PUTVAL (' ', 'Telephone', TEMP)
      GO TO 210
 222  TEMP = LINE(10:ITRIM(LINE)-2)
      CALL PUTVAL (' ', 'Telex', TEMP)
      GO TO 210
 225  GO TO 210
 230  TEMP = LINE(12:ITRIM(LINE)-2)
      CALL PUTVAL ('\n', ' ', TEMP)
      CALL PUTVAL (' ', 'Networked ?', 'Y')
      CALL PUTVAL (' ', 'Conn.Mode', 'Net')
      GO TO 210
C                                       CPlastname:
 235  CALL PUTVAL (' ', 'Contact Person', LINE(13:ITRIM(LINE)-2))
      GO TO 210
C                                       CPcountry:
 237  CALL PUTVAL (' ', 'Country', LINE(12:ITRIM(LINE)-2))
      GO TO 210
C                                       CPzip:
 239  CALL PUTVAL (' ', 'ZIP', LINE(8:ITRIM(LINE)-2))
      GO TO 210
C                                       CPstreet:
 240  CALL PUTVAL (' ', 'Street', LINE(11:ITRIM(LINE)-2))
      GO TO 210
C                                       CPcity:
 241  CALL PUTVAL (' ', 'City', LINE(9:ITRIM(LINE)-2))
      GO TO 210
C                                       CPinstitute:
 243  CALL PUTVAL (' ', 'Institute', LINE(14:ITRIM(LINE)-2))
      GO TO 210
C                                       }% seen:
 248  CONTINUE
      GO TO 200
C-----------------------------------------------------------------------
C                                       \CPsitedata seen:
 251  J = 13
      CPMACH = .FALSE.
      GO TO  255
C                                       \CPmachine seen:
 252  J = 12
      CPMACH = .TRUE.
C                                       extract Directory machine #:
 255  DO 257 I = 1, NVECTR
         IF (((LMACH(I).NE.'Site-wide').AND.(LMACH(I).NE.'First-only'))
     *      .OR.((LMACH(I).EQ.'First-only').AND.FOMACH))
     *         VALUE(I) = NA
 257     CONTINUE
      IF (CPMACH) FOMACH = .TRUE.
      TEMP = LINE(J:)
      LINE = TEMP
      FIELD = LINE(:INDEX (LINE,'}')-1)
C     IF (DBG) WRITE (L6,*) 'machine=', FIELD(:ITRIM(FIELD))
      CALL PUTVAL ('MACHINE', ' ', FIELD)
C                                       read & parse key=value fields:
 260  READ (L1,'(A)', END=290) LINE
         IF (LINE.EQ.'}%') GO TO 310
         IF (DBG.AND.KDBG) WRITE (L6, *) 'L0="',LINE(:ITRIM(LINE)),'"'
 270     CONTINUE
         IF (DBG.AND.KDBG) WRITE (L6, *) 'L="',LINE(:ITRIM(LINE)),'"'
         I = INDEX(LINE,'=')
         IF (DBG.AND.KDBG) WRITE (L6, *) 'I=',I
         IF (I.LE.1) STOP 'no KEY ?!? (I.LE.1)'
         MKEY=LINE(:I-1)
         IF (DBG.AND.KDBG) WRITE (L6, *) 'MKEY="',MKEY(:ITRIM(MKEY)),'"'
         TEMP = LINE(I+1:)
         LINE = TEMP
         IF (DBG.AND.KDBG) WRITE (L6, *) 'L="',LINE(:ITRIM(LINE)),'"'
         I = INDEX(LINE,' ')
         IF (I.LE.1) STOP 'no VALUE ?!? (I.LE.1)'
         TEMP = LINE(:I-1)
         IF (DBG.AND.KDBG) WRITE (L6, *) 'TEMP="',TEMP(:ITRIM(TEMP)),'"'
C                                       parse multi-valued fields:
 280     J = INDEX(TEMP,',')
         IF (J.LE.0) GO TO 285
            TEMP2 = TEMP(:J-1)
            CALL PUTVAL (MKEY, ' ', TEMP2)
            TEMP2 = TEMP(J+1:)
            TEMP = TEMP2
            GO TO 280
 285     CALL PUTVAL (MKEY, ' ', TEMP)
         TEMP = LINE(I+1:)
         LINE = TEMP
         IF (LINE.NE.' ') GO TO 270
         GO TO 260
 290  STOP 'unexpected END seen in machine-reading loop'
C-----------------------------------------------------------------------
C                                       end of machine:
 310  IF (.NOT.CPMACH) GO TO 315
C                                       Decide power rating:
      CALL PUTWT
C                                       classify OS:
      CALL PUTOS
C                                       compute Reflex field values:
      CALL DOAHB1
C                                       write this machine to scratch:
      WRITE (L4) VALUE
 315  GO TO 200
C                                       Write out machines from scratch:
C                                       Here from main scan loop;
C                                       Applies AHB's normalization
C                                       formulae, calls PUTDIF:
 320  ENDFILE L4
      CALL DOAHB2 (NTUPLE)
      GO TO 200
C-----------------------------------------------------------------------
C                                       "enddirectory" seen:
 390  CONTINUE
      WRITE (L6,*) 'NTUPLE=', NTUPLE
      CLOSE (UNIT=L1)
C                                       Now copy scratch to output DIF:
      WRITE (L3,1410) TITLE(:ITRIM(TITLE))
      CALL MAKINT (NVECTR, TEMP)
      WRITE (L3,1420) 'VECTORS', TEMP(:ITRIM(TEMP))
      CALL MAKINT (NTUPLE, TEMP)
      WRITE (L3,1420) 'TUPLES', TEMP(:ITRIM(TEMP))
      DO 430 I = 1, NVECTR
         CALL MAKINT (I, TEMP)
 430     WRITE (L3,1430) TEMP(:ITRIM(TEMP)),
     *                     LABEL(I)(:ITRIM(LABEL(I)))
      WRITE (L3,1440)
C                                       loop to copy scratch to DIF:
      REWIND L2
      I = 0
      J = 0
 450  CONTINUE
         READ (L2,'(A)',END=460) LINE
         I = I + 1
         IF (MOD(I,1000).EQ.0) WRITE (L6,*) 'line ', I
         K = ITRIM(LINE)
         J = J + K
         WRITE (L3,'(A)') LINE(:K)
         GO TO 450
 460  CONTINUE
      WRITE (L3,1310) 'EOD'
      WRITE (L6,*) 'copied', I, ' lines,', J, ' bytes.'
C                                       That's all, folks:
      CLOSE (UNIT=L2, DISPOSE='DELETE')
      CLOSE (UNIT=L3)
      CLOSE (UNIT=L4, DISPOSE='DELETE')
      CLOSE (UNIT=L7, DISPOSE='DELETE')
C-----------------------------------------------------------------------
 1110 FORMAT (' Enter input Directory-file         (w/o .TEX):', $)
 1115 FORMAT (A72)
 1116 FORMAT (' L',I1,'=', A)
 1117 FORMAT (' Cannot open file ', A)
 1130 FORMAT (' Enter input DIF-specification-file (w/o .DAT):', $)
 1150 FORMAT (' Enter input [CPU+AP]-weights-file  (w/o .DAT):', $)
 1310 FORMAT ('-1,0', /, A)
 1410 FORMAT ('TABLE', /, '0,1', /, '"', A, '"')
 1420 FORMAT (A, /, '0,', A, /, '""')
 1430 FORMAT ('LABEL', /, '0,', A, /, '"', A, '"')
 1440 FORMAT ('DATA', /, '0,0', /, '""')
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
      SUBROUTINE MAKINT (N, STR)
C-----------------------------------------------------------------------
C     MAKINT formats integer N into string STR and removes leading
C     blanks. ITRIM can then be used to effectively remove trailing
C     blanks as well. DCW, NRAO-CV, 02Apr86.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
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
      SUBROUTINE PUTFLT (X, NFRACT, KARG, LARG)
C-----------------------------------------------------------------------
C     PUTFLT formats real X into STRTMP and removes leading
C     blanks and truncates the fraction to NFRACT digits,
C     and stores it into the VALUE() array by calling PUTVAL.
C     DCW, NRAO-CV, 29Feb88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      REAL        X
      INTEGER     NFRACT
      CHARACTER   KARG*(*), LARG*(*)
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C                                       Local variables:
      INTEGER     I, ITRIM
      CHARACTER   STRTMP*20, TEMP*20, C*1
C-----------------------------------------------------------------------
      WRITE (STRTMP, '(F20.8)') X + SIGN (1.0/(10.0**(NFRACT+1)), X)
      STRTMP((20-9+NFRACT+2):) = ' '
 10   IF ((STRTMP(1:1).NE.' ').OR.(STRTMP.EQ.' ')) GO TO 20
         TEMP = STRTMP(2:)
         STRTMP = TEMP
         GO TO 10
 20   I = ITRIM(STRTMP)
 30   C = STRTMP(I:I)
      IF ((C.NE.'0').AND.(C.NE.'.')) GO TO 50
         STRTMP(I:I) = ' '
         I = I - 1
         IF (STRTMP.EQ.'.') STRTMP = '0'
         IF ((I.GT.1).AND.(C.NE.'.')) GO TO 30
C
 50   CALL PUTVAL (KARG, LARG, NA)
      CALL PUTVAL (KARG, LARG, STRTMP)
C
      RETURN
      END
      SUBROUTINE GETFLT (KARG, LARG, X, VALNA)
C-----------------------------------------------------------------------
C     GETFLT calls GETVAL to fetch a value, and then
C     decodes it into real X.
C     DCW, NRAO-CV, 04Mar88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      CHARACTER   KARG*(*), LARG*(*)
      REAL        X, VALNA
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C
      INTEGER     I, ITRIM
      CHARACTER   STRTMP*20, TEMP*20, C*1
C-----------------------------------------------------------------------
      CALL GETVAL (KARG, LARG, STRTMP)
C
      IF (STRTMP.NE.NA) GO TO 105
         X = VALNA
         GO TO 990
C
 105  CONTINUE
         REWIND L7
         WRITE (L7, '(A, '' /'')') STRTMP
         REWIND L7
         READ (L7, *, ERR=110) X
            GO TO 120
 110     CONTINUE
            WRITE (L6, *) 'GETFLT: conversion error?!? STRTMP="',
     *         STRTMP(:ITRIM(STRTMP)), '"'
            X = 0.0
 120     CONTINUE
C
 990  CONTINUE
      RETURN
      END
      SUBROUTINE PUTVAL (KARG, LARG, CVAL0)
C-----------------------------------------------------------------------
C     Subroutine which inserts CVAL into the VALUE() array, by matching
C     KARG and/or LARG with KEY() and/or LABEL().
C     DCW, NRAO-CV, 17Mar87.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      CHARACTER   KARG*(*), LARG*(*), CVAL0*(*)
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C
      INTEGER     I1, I, J, ITRIM
      CHARACTER   CVAL*100, TEMP*100
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C-----------------------------------------------------------------------
C
C                                       clean up CVAL before using it:
      CVAL = CVAL0
C                                       remove {} pairs:
C                                       [this is now OBSOLETE!]
 10   I = INDEX (CVAL,'{')
      IF (I.LE.0) GO TO 40
         IF (I.EQ.1) TEMP = CVAL(I+1:)
         IF (I.GT.1) TEMP = CVAL(:I-1) // CVAL(I+1:)
         CVAL = TEMP
         J = ITRIM (CVAL) + 1
 20      J = J - 1
         IF (J.GT.0) GO TO 30
            DO 25 I = 1, NVECTR
               IF (LABEL(I).EQ.'Contact Person')
     *         WRITE (L6,1135) VALUE(I)(:ITRIM(VALUE(I)))
 25            CONTINUE
            WRITE (L6,*) 'PUTVAL: CVAL0="',CVAL0(:ITRIM(CVAL0)),
     *                '", CVAL="', CVAL(:ITRIM(CVAL)),'"'
            STOP 'PUTVAL: a matching } is missing!'
 30      IF (CVAL(J:J).NE.'}') GO TO 20
            IF (J.EQ.1) TEMP = CVAL(J+1:)
            IF (J.GT.1) TEMP = CVAL(:J-1) // CVAL(J+1:)
            CVAL = TEMP
            GO TO 10
C                                       remove all backslashes:
 40   I = INDEX (CVAL, '\')
      IF (I.LE.0) GO TO 50
         IF (I.EQ.1) TEMP = CVAL(I+1:)
         IF (I.GT.1) TEMP = CVAL(:I-1) // CVAL(I+1:)
         CVAL = TEMP
         GO TO 40
C                                       change tildes to blanks:
 50   I = INDEX (CVAL, '~')
      IF (I.LE.0) GO TO 60
         IF (I.EQ.1) TEMP = ' ' // CVAL(I+1:)
         IF (I.GT.1) TEMP = CVAL(:I-1) // ' ' // CVAL(I+1:)
         CVAL = TEMP
         GO TO 50
 60   I = INDEX (CVAL, '"')
      IF (I.LE.0) GO TO 70
         IF (I.EQ.1) TEMP = '''' // CVAL(I+1:)
         IF (I.GT.1) TEMP = CVAL(:I-1) // '''' // CVAL(I+1:)
         CVAL = TEMP
         GO TO 60
 70   CONTINUE
C-----------------------------------------------------------------------
C                                       must have *either* KARG or LARG:
      IF ((KARG.NE.' ').AND.(LARG.NE.' ')) STOP 'both KARG & LARG set!'
      IF ((KARG.EQ.' ').AND.(LARG.EQ.' ')) STOP 'both KARG & LARG null!'
      J = 0
 105  I1 = J + 1
      IF (KARG.EQ.' ') GO TO 120
C                                       KARG is set:
      DO 110 I = I1, NVECTR
         J = I
         IF (KEY(I).EQ.KARG) GO TO 150
 110     CONTINUE
      GO TO 140
C                                       LARG is set:
 120  DO 130 I = I1, NVECTR
         J = I
         IF (LABEL(I).EQ.LARG) GO TO 150
 130     CONTINUE
C                                       no KARG/LARG hit!?!
C                                       (This is really not so bad;
C                                       after all, the CAN keyword does
C                                       not appear in the DIF file!)
 140  IF (I1.EQ.1) GO TO 190
C      IF (.NOT.DBG) GO TO 146
C         DO 145 I = 1, NVECTR
C            IF (LABEL(I).EQ.'Contact Person')
C     *         WRITE (L6,1135) VALUE(I)(:ITRIM(VALUE(I)))
C 145        CONTINUE
C         WRITE (L6,1145) KARG(:ITRIM(KARG)), LARG(:ITRIM(LARG)),
C     *         CVAL(:MIN(ITRIM(CVAL),40)),
C     *         VALUE(I1-1)(:MIN(ITRIM(VALUE(I1-1)),40))
C 146  CONTINUE
      GO TO 190
C                                       made a hit:
 150  IF ((VALUE(J).NE.NA).AND.(CVAL.NE.NA)) GO TO 105
C                                       (NOTE: to change a value, first
C                                       store NA into it.)
      VALUE(J) = CVAL
C
 190  RETURN
C-----------------------------------------------------------------------
 1135 FORMAT (/, ' PUTVAL: for Contact Person="',A,'",')
 1145 FORMAT (' PUTVAL: cannot insert "',A,'"/"',A,'"="',A'"; prev="',
     *      A,'"')
      END
      SUBROUTINE GETVAL (KARG, LARG, CVAL)
C-----------------------------------------------------------------------
C     Subroutine to retrieve a value from the VALUE() array, by
C     matching KARG and/or LARG with KEY() and/or LABEL().
C     DCW, NRAO-CV, 29Feb88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      CHARACTER   KARG*(*), LARG*(*), CVAL*(*)
C
      INTEGER     I, J
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C-----------------------------------------------------------------------
C                                       must have *either* KARG or LARG:
      IF ((KARG.NE.' ').AND.(LARG.NE.' ')) STOP 'both KARG and LARG!'
      IF ((KARG.EQ.' ').AND.(LARG.EQ.' ')) STOP 'neither KARG nor LARG!'
      IF (KARG.EQ.' ') GO TO 210
C                                       KARG set:
         DO 120 I = 1, NVECTR
            J = I
            IF (KEY(I).EQ.KARG) GO TO 310
 120        CONTINUE
         STOP 'GETVAL: could not find KARG!'
C                                       LARG set:
 210  CONTINUE
         DO 220 I = 1, NVECTR
            J = I
            IF (LABEL(I).EQ.LARG) GO TO 310
 220        CONTINUE
         STOP 'GETVAL: cannot find LARG!'
C                                       got J, now fetch VALUE:
 310  CVAL = VALUE(J)
      RETURN
      END
      SUBROUTINE PUTWT
C-----------------------------------------------------------------------
C     Subroutine to set the "WT" parameter for a machine.
C     Value is determined by looking up the CPU and AP parameters, and
C     then doing a lookup in the WTS() table.
C     DCW, NRAO-CV, 18Mar87/26Feb88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C
      INTEGER     I, J, ITRIM
      REAL        WWTJ
      CHARACTER   MCPU*30, MAP*30
C                                       commons for subroutine PUTWT:
      INTEGER     MXNWTS, MXWTFW
      PARAMETER   (MXNWTS=150, MXWTFW=40)
      INTEGER     NWTS
      CHARACTER   WCPU(MXNWTS)*(MXWTFW), WAP(MXNWTS)*(MXWTFW),
     *            WTY(MXNWTS)*1, WWT(MXNWTS)*(MXWTFW)
      COMMON      /PUTWT1/ NWTS
      COMMON      /PUTWT2/ WCPU, WAP, WTY, WWT
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C-----------------------------------------------------------------------
C                                       get CPU and AP values:
      DO 110 I = 1, NVECTR
         J = I
         IF (KEY(I).EQ.'CPU') GO TO 120
 110     CONTINUE
      STOP 'PUTWT: "CPU" not found!?!'
 120  MCPU = VALUE(J)
      DO 130 I = 1, NVECTR
         J = I
         IF (KEY(I).EQ.'AP') GO TO 140
 130     CONTINUE
      STOP 'PUTWT: "AP" not found!?!'
 140  MAP = VALUE(J)
      VALUE(J) = NA
      IF (MAP.EQ.NA) MAP = ' '
C                                       look up in WT table:
      DO 150 I = 1, NWTS
         J = I
         IF ((MCPU.EQ.WCPU(I)).AND.(MAP.EQ.WAP(I))) GO TO 170
 150     CONTINUE
      WRITE (L6,1150) MCPU(:ITRIM(MCPU)), MAP(:ITRIM(MAP))
      GO TO 900
C                                       made a hit; put the WT:
 170  CALL PUTVAL (' ', 'WT', WWT(J))
C                                       and the AP Type:
      CALL PUTVAL (' ', 'AP ?', WTY(J))
C
 900  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1150 FORMAT (' Not in WT table: MCPU="',A,'", MAP="',A,'"')
      END
      SUBROUTINE DOAHB1
C-----------------------------------------------------------------------
C     Routine to compute Alan's Reflex formulae for a machine.
C     (Only the formulae that do not involve normalization over sets.)
C     DCW, NRAO-CV, 07Mar88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C                                       local temp variables:
      REAL        WT, AIPSFR, WTAIPS, VLAFR, WTVLA, NONRFR, WTNONR,
     *            AVUSERS, AVWT
      CHARACTER   CONTRY*(MXWFLD), GEOG*(MXWFLD)
C-----------------------------------------------------------------------
C                                       WTAIPS=WT * AIPS~Fract / 100
      CALL GETFLT (' ', 'WT', WT, 0.0)
      CALL GETFLT (' ', 'AIPS Fract', AIPSFR, 0.0)
      WTAIPS = WT * AIPSFR / 100.0
      CALL PUTFLT (WTAIPS, 2, ' ', 'WTAIPS')
C                                       WTVLA = WTAIPS * VLA~Fract / 100
      CALL GETFLT (' ', 'VLA Fract', VLAFR, 0.0)
      WTVLA = WTAIPS * VLAFR / 100.0
      CALL PUTFLT (WTVLA, 2, ' ', 'WTVLA')
C                                       WTNONR=WTAIPS*Nonrad~Fract/100
      CALL GETFLT (' ', 'Nonrad Fract', NONRFR, 0.0)
      WTNONR = WTAIPS * NONRFR / 100.0
      CALL PUTFLT (WTNONR, 2, ' ', 'WTNONR')
C                                   Geog = @CASE(Country...)
      CALL GETVAL (' ', 'Country', CONTRY)
      IF (CONTRY.EQ.'NRAO') THEN
         GEOG = 'NRAO'
      ELSE IF (CONTRY.EQ.LOCAL) THEN
         GEOG = LOCAL
      ELSE
         GEOG = 'non-' // LOCAL
      END IF
      CALL PUTVAL (' ', 'Geog', GEOG)
C                                       AVWT=@IF(AVUSERS>0,WT/AVUSERS,0)
      CALL GETFLT (' ', 'AVUSERS', AVUSERS, 0.0)
      AVWT = 0.0
      IF (AVUSERS.GT.0.0) AVWT = WT / AVUSERS
      CALL PUTFLT (AVWT, 2, ' ', 'AVWT')
C
      RETURN
      END
      SUBROUTINE DOAHB2 (NTUPLE)
C-----------------------------------------------------------------------
C     Routine to apply Alan's formulae to a Contact Person's machines.
C     These are the "normalization-by-site" formulae.
C     DCW, NRAO-CV, 26Feb88.
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INTEGER     NTUPLE
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C                                       local temp variables:
      INTEGER     NSITES, MXSITE, ITRIM, I
      PARAMETER   (MXSITE=10)
      REAL        USERS, WTAIPS, SWTAIP(MXSITE), CRAIPS, WTVLA,
     *            CRVLA, WTNONR, CRNONR
      CHARACTER   SITNAM*(MXWFLD), ISITE(MXSITE)*(MXWFLD), CP*(MXWFLD)
C-----------------------------------------------------------------------
C                                       Sum WTAIPS by sites:
      NSITES = 0
      REWIND L4
 110  READ (L4, END=150) VALUE
         CALL GETVAL (' ', 'Site Name', SITNAM)
         DO 120 I = 1, NSITES
            IF (SITNAM.EQ.ISITE(I)) GO TO 130
 120        CONTINUE
C                                       add this site to list:
         NSITES = NSITES + 1
         IF (NSITES.GT.MXSITE) STOP 'DOAHB2: NSITES.GT.MXSITE !'
         ISITE(NSITES) = SITNAM
         IF (SITNAM.EQ.NA) THEN
            CALL GETVAL (' ', 'Contact Person', CP)
            IF ((CP.EQ.' ').OR.(CP.EQ.NA)) STOP 'DOAHB2: CP not set!'
            WRITE (L6,*) 'DOAHB2: SITE not specified for CP="',
     *         CP(:ITRIM(CP)), '"'
         END IF
         SWTAIP(NSITES) = 0.0
         I = NSITES
C                                       accumulate weightsum:
 130     CALL GETFLT (' ', 'WTAIPS', WTAIPS, 0.0)
         SWTAIP(I) = SWTAIP(I) + WTAIPS
         GO TO 110
C
 150  IF (NSITES.EQ.1) GO TO 190
         CALL GETVAL (' ', 'Contact Person', CP)
         IF ((CP.EQ.' ').OR.(CP.EQ.NA)) STOP 'DOAHB2: CP not set!'
         DO 160 I = 1, NSITES
            WRITE (L6, '(''CP="'', A, ''", SITE="'', A,
     *         ''", SWTAIP='', F7.3)')
     *         CP(:MIN(MXWFLD,30)), ISITE(I)(:MIN(MXWFLD,30)), SWTAIP(I)
 160        CONTINUE
 190  CONTINUE
C-----------------------------------------------------------------------
C                                       Big loop to write the tuples:
      REWIND L4
 325  READ (L4, END=355) VALUE
C                                       Apply Alan's formulae:
C                                   #USERS = #USERS*WTAIPS/SUM(WTSUM)
         CALL GETFLT (' ', '#USERS', USERS, 0.0)
         CALL GETFLT (' ', 'WTAIPS', WTAIPS, 0.0)
         CALL GETVAL (' ', 'Site Name', SITNAM)
         DO 330 I = 1, NSITES
            IF (SITNAM.EQ.ISITE(I)) GO TO 335
 330        CONTINUE
         GO TO 340
 335     IF ((SWTAIP(I).GT.0.0).AND.(WTAIPS.GT.0.0))
     *      USERS = USERS * WTAIPS / SWTAIP(I)
         CALL PUTFLT (USERS, 2, ' ', '#USERS')
 340     CONTINUE
C                                   CRAIPS=@IF(#USERS>0,WTAIPS/#USERS,0)
         CRAIPS = 0.0
         IF (USERS.GT.0.0) CRAIPS = WTAIPS / USERS
         CALL PUTFLT (CRAIPS, 2, ' ', 'CRAIPS')
C                                   CRVLA=@IF(#USERS>0,WTVLA/#USERS,0)
         CALL GETFLT (' ', 'WTVLA', WTVLA, 0.0)
         CRVLA = 0.0
         IF (USERS.GT.0.0) CRVLA = WTVLA / USERS
         CALL PUTFLT (CRVLA, 2, ' ', 'CRVLA')
C                                   CRNONR=@IF(#USERS>0,WTNONR/#USERS,0)
         CALL GETFLT (' ', 'WTNONR', WTNONR, 0.0)
         CRNONR = 0.0
         IF (USERS.GT.0.0) CRNONR = WTNONR / USERS
         CALL PUTFLT (CRNONR, 2, ' ', 'CRNONR')
C                                       write this tuple out:
         CALL PUTDIF
C
         NTUPLE = NTUPLE +1
         IF(MOD(NTUPLE,10).EQ.0) WRITE (L6, *) 'NTUPLE=', NTUPLE
         GO TO 325
 355  REWIND L4
      ENDFILE L4
      REWIND L4
C
      RETURN
      END
      SUBROUTINE PUTOS
C-----------------------------------------------------------------------
C     Routine to analyze OS string and classify it into simple
C     categories: 'Unix', 'VMS', and 'other'.
C-----------------------------------------------------------------------
      IMPLICIT       NONE
C
      INTEGER     I, J, K, KA, KZ, JVALUE, ITRIM, KUPPER
      CHARACTER   OS*40, TEMP*40, OSTYPE*10
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C-----------------------------------------------------------------------
C                                       Look for OS field:
      DO 5 I = 1, NVECTR
         JVALUE = I
         IF (KEY(I).EQ.'OS') GO TO 7
 5       CONTINUE
      GO TO 990
 7    OS = VALUE(JVALUE)
C
      J = ITRIM (OS)
      KA = ICHAR('a')
      KZ = ICHAR('z')
      KUPPER = ICHAR('A')
      TEMP = ' '
      DO 10 I = 1, J
         K = ICHAR (OS(I:I))
         IF ((K.GE.KA).AND.(K.LE.KZ)) K = (K - KA) + KUPPER
         TEMP(I:I) = CHAR (K)
 10      CONTINUE
C
      OSTYPE = 'other'
      IF (INDEX (TEMP, 'UNIX').NE.0) OSTYPE = 'Unix'
      IF (INDEX (TEMP, 'VMS').NE.0) OSTYPE = 'VMS'
      VALUE(JVALUE) = OSTYPE
C
 990  RETURN
      END
      SUBROUTINE PUTDIF
C-----------------------------------------------------------------------
C     Subroutine to write the DIF output from VALUE().
C-----------------------------------------------------------------------
      IMPLICIT NONE
C                                       Some global constants:
      INTEGER     L1, L2, L3, L4, L5, L6, L7
      LOGICAL     DBG
      CHARACTER   LOCAL*(*), NA*(*)
      PARAMETER   (L1=1, L2=2, L3=3, L4=4, L5=5, L6=6, L7=7,
     *            DBG=.TRUE., LOCAL='USA', NA='-NA-NA-NA-')
C                                       commons for subroutine PUTVAL:
      INTEGER     MXVCTR, MXWKEY, MXWFLD
      PARAMETER   (MXVCTR=50, MXWKEY=15, MXWFLD=50)
      INTEGER     NVECTR
      CHARACTER   KEY(MXVCTR)*(MXWKEY), LABEL(MXVCTR)*(MXWFLD),
     *               VALUE(MXVCTR)*(MXWFLD), FTYPE(MXVCTR)*10,
     *               LMACH(MXVCTR)*10
      COMMON      /CPTVL1/ NVECTR
      COMMON      /CPTVL2/ KEY, LABEL, VALUE, FTYPE, LMACH
C
C                                       Local variables:
      INTEGER     I, ITRIM
      CHARACTER   TEMP*(MXWFLD)
C-----------------------------------------------------------------------
C                                       Write current VALUE tuple:
      WRITE (L2,1310) 'BOT'
      DO 350 I = 1, NVECTR
         IF ((FTYPE(I).NE.'Numeric').AND.(FTYPE(I).NE.'Text'))
     *                                    STOP 'illegal FTYPE!'
         IF (VALUE(I).EQ.NA) GO TO 330
C                                       the field has a value:
            TEMP = VALUE(I)
            IF (FTYPE(I).EQ.'Numeric')
     *                        WRITE (L2,1320) TEMP(:ITRIM(TEMP))
            IF (FTYPE(I).EQ.'Text')
     *                        WRITE (L2,1325) TEMP(:ITRIM(TEMP))
            GO TO 350
C                                       value "not available":
 330     CONTINUE
            IF (FTYPE(I).EQ.'Numeric') WRITE (L2, 1330)
            IF (FTYPE(I).EQ.'Text') WRITE (L2, 1335)
 350     CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1310 FORMAT ('-1,0', /, A)
 1320 FORMAT ('0,', A, /, 'V')
 1325 FORMAT ('1,0', /, '"', A, '"')
 1330 FORMAT ('0,0', /, 'NA')
 1335 FORMAT ('1,0', /, '""')
      END
