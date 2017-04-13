      PROGRAM GRKEY
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
C   This program reads one or more gripe files and constructs a series
C   of cross-reference/index listings, especially the 'keys'
C     which are embedded in the gripes. Optionally, it can also produce
C     TEX files of gripes sorted by person and key in order to simplify
C     the distribution of "tbc" gripes to the programmers.
C     DCW, NRAO-CV, Mar-Apr85.
C     Notes:
C     1. consider producing "finalrun{T}" sorted by gripers?
C-----------------------------------------------------------------------
      IMPLICIT    NONE
      INCLUDE     '($SSDEF)'
      INTEGER     MAXCHAR, MXKSTR, MXLGRI, MXPERS, NSKIP, NCOL, MXF,
     *            KEYFLD
      PARAMETER   (MAXCHAR=5000, MXKSTR=24, MXLGRI=201, MXPERS=20)
      PARAMETER   (NSKIP=MXKSTR+3, NCOL=6, MXF=20, KEYFLD=1+4+1)
      INTEGER     L5, L6, L1, ITRIM, ISTAT, NGRIPE, L4, I, J, L2,
     *   LGRIPE, STR$UPCASE, L3, L, K, LIB$DATE_TIME, L7,
     *   L8, LASTJ, MXLPRT, NKEYL, N1, N5, LL, KK
      CHARACTER   COMMAND*120, FILESPEC*40, L1FILE*(MXF), L4FILE*80,
     *   GRARGS(9)*(MAXCHAR), GRIPE*(MAXCHAR), NEWLIN,
     *   KEYSTR*(MXKSTR), L2FILE*(MXF), LASTKEY*(MXKSTR),
     *   FOLDED*(MXKSTR), PERSON*(MXPERS), LASTPER*(MXPERS),
     *   KGRIPE(MXLGRI)*(KEYFLD), CLASS*1, L3FILE*(MXF),
     *   SPECIAL*(MXPERS), CODE*1, KEYTMP*(MXKSTR+3),
     *   PERTMP*(MXPERS), PERTM2*(MXPERS), LASTHD*(MXPERS),
     *   DATETIM*23, MODDAT*17, DELETE*10, L7FILE*(MXF),
     *   L8FILE*(MXF), L7KEYF*(KEYFLD), L7LAST*(KEYFLD),
     *   CODTMP*1, KEYL(20)*(MXKSTR)
      LOGICAL     QUIT, EXTRA, TOTAL, ALLKEY, DOTEX, DOKEY
      DATA MXLPRT /MXLGRI/
      DATA N1,N5 /1,5/
C-----------------------------------------------------------------------
      NKEYL = 0
      L1 = 1
      L1FILE = 'GRKEYONE.TMP'
      L2 = 2
      L2FILE = 'GRKEYTWO.TMP'
      OPEN (UNIT=L2, FILE=L2FILE, STATUS='NEW',
     *   ACCESS='SEQUENTIAL', FORM='FORMATTED', RECORDTYPE='FIXED',
     *   RECL=(KEYFLD+MXKSTR+MXPERS+MXKSTR), CARRIAGECONTROL='LIST')
      L3 = 3
      L3FILE = 'GRKEYALL.LIS'
      OPEN (L3, FILE=L3FILE, STATUS='NEW')
      L4 = 4
      L5 = 5
      L6 = 6
      L7 = 7
      L7FILE = 'GRKEYISAM.TMP'
      L8 = 8
      NEWLIN = '|||'
      IF (LIB$DATE_TIME (DATETIM).NE.SS$_NORMAL) STOP 'Date failed!'
      WRITE (L3,1003)
      WRITE (L3,1007) DATETIM
C                                       Special listing?
 10   CONTINUE
         WRITE (L6,1010)
         SPECIAL = ' '
         READ (L5,1011,ERR=10,END=900) SPECIAL
 15   CONTINUE
         NKEYL = 0
         WRITE (L6,1015)
 16      NKEYL = NKEYL + 1
            WRITE (L6,1016) NKEYL
            READ (L5,1011,ERR=15,END=900) KEYL(NKEYL)
            IF (KEYL(NKEYL).NE.' ') THEN
               IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *            GO TO 990
               GO TO 16
               END IF
         NKEYL = NKEYL - 1
         IF ((SPECIAL.EQ.' ') .AND. (NKEYL.GT.0)) SPECIAL = 'Everyone'
C                                       Ask for listing options:
 20   CONTINUE
         WRITE (L6,1020)
         READ (L5,1021,ERR=20,END=900) TOTAL
 25   CONTINUE
         WRITE (L6,1025)
         READ (L5,1021,ERR=25,END=900) ALLKEY
 30   CONTINUE
         WRITE (L6,1030)
         READ (L5,1021,ERR=30,END=900) EXTRA
C                                       TEX file
      DOTEX = (SPECIAL.NE.' ') .OR. (NKEYL.GT.0)
      IF (DOTEX) THEN
         L8FILE = 'GRKEY' // SPECIAL(:ITRIM(SPECIAL)) // '.TEX'
         WRITE (L6,1035) L8FILE(:ITRIM(L8FILE))
         OPEN (L8, FILE=L8FILE, STATUS='NEW', ERR=10,
     *      CARRIAGECONTROL='LIST')
         WRITE (L8,1036)
         OPEN (L7, FILE=L7FILE, STATUS='SCRATCH', ACCESS='KEYED',
     *      CARRIAGECONTROL='LIST', FORM='FORMATTED',
     *      KEY=(1:KEYFLD:CHARACTER), ORGANIZATION='INDEXED',
     *      RECL = (KEYFLD+MAXCHAR), RECORDTYPE='FIXED')
         L7LAST = '<garp>'
         END IF
C                                       Get gripe file search string:
 40   WRITE (L6,1040)
      READ (L5,1011,ERR=40,END=900) FILESPEC
      IF (FILESPEC.EQ.' ') THEN
         IF (.NOT.DOTEX) FILESPEC = 'AIPSGRIP:GT%%%%,GX%%%%'
         IF (DOTEX) FILESPEC = 'AIPSGRIP:GT%%%%'
         END IF
C                                       Get directory of gripe files:
      COMMAND = 'DIR/NOHE/NOTR/VER=1/DAT/OUT=' //
     *   L1FILE(1:ITRIM(L1FILE)) // ' ' // FILESPEC
      CALL DCLCMD (L6, COMMAND, ISTAT)
      IF (ISTAT.NE.0) STOP 'Bad Status from DIR'
      OPEN (UNIT=L1, FILE=L1FILE, STATUS='OLD')
C                                       Loop to read gripe files:
 100  CONTINUE
         READ (L1,'(A64,/,21X,A17)',END=180) L4FILE, MODDAT
         WRITE (L3,'(1X,A,1X,A)') L4FILE(:ITRIM(L4FILE)), MODDAT
         WRITE (L6,'(1X,A,1X,A)') L4FILE(:ITRIM(L4FILE)), MODDAT
         OPEN (UNIT=L4, FILE=L4FILE, STATUS='OLD', ERR=980)
         I = INDEX (L4FILE, ']')
         CLASS = L4FILE(I+2:I+3)
C                                       loop to read the gripes:
 110     CONTINUE
            CALL GETGRI (L3, L4, NEWLIN, NGRIPE, GRIPE, GRARGS)
            IF (GRIPE.EQ.' ') GO TO 150
C                                       clean up PERSON string:
               PERSON = GRARGS(7)(2:(ITRIM(GRARGS(7))-1))
               IF (PERSON.EQ.' ') PERSON = '<blank>'
               IF (PERSON.EQ.'\ewg') PERSON = 'Eric'
               DO 115 K = 1,3
                  J = INDEX (PERSON, ',')
                  IF (J.NE.0) PERSON(J:J) = '/'
 115              CONTINUE
               DELETE = '   ??(())'
               KK = ITRIM (DELETE)
               DO 120 K = 1,KK
                  J = INDEX (PERSON, DELETE(K:K))
                  IF (J.EQ.1) PERSON = PERSON(J+1:)
                  IF (J.GT.1) PERSON = PERSON(:J-1) // PERSON(J+1:)
 120              CONTINUE
C                                       generate summary entries:
               KEYSTR = 'Final answers'
               IF (INDEX (GRARGS(9), '\newgripe').NE.0) KEYSTR =
     *            '\newgripe'
               IF (INDEX (GRARGS(9), '\tbcnew').NE.0) KEYSTR =
     *            '\tbcnew'
               IF ((KEYSTR(1:5).EQ.'Final') .AND.
     *            (INDEX (GRARGS(9), '\tbc').NE.0)) KEYSTR = '\tbc'
               IF ((KEYSTR(1:4).EQ.'\tbc') .AND.
     *            (INDEX (GRARGS(9), '\changed').NE.0)) KEYSTR =
     *             KEYSTR(:ITRIM(KEYSTR)) // '\changed'
               IF ((INDEX (GRARGS(9), '\changed').NE.0) .AND.
     *            (INDEX (GRARGS(9), '\tbc').EQ.0)) KEYSTR =
     *            '\changed [no \tbc]'
C                                       set suffix code:
               CODE = ' '
               IF (KEYSTR(1:5).EQ.'Final') CODE = 'f'
               IF (KEYSTR.EQ.'\tbcnew') CODE = 'n'
               IF (KEYSTR.EQ.'\tbc')   CODE = 't'
               IF (INDEX (KEYSTR, '\changed').NE.0) CODE = 'c'
               PERTMP = ' All gripes.'
               IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *            GO TO 990
               IF (TOTAL) WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *            KEYSTR, PERTMP, FOLDED
               KEYSTR = 'Complete list'
               IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *            GO TO 990
               IF (TOTAL) WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *            KEYSTR, PERTMP, FOLDED
C                                       all machines/sites:
               KEYSTR = '~' // GRARGS(2)
               IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *            GO TO 990
               IF (EXTRA) WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *            KEYSTR, PERTMP, FOLDED
C                                       all gripers:
               KEYSTR = '~~' // GRARGS(3)
               IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *            GO TO 990
               IF (EXTRA) WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *            KEYSTR, PERTMP, FOLDED
               IF (INDEX (GRIPE, '\key{').EQ.0) THEN
                  KEYSTR = '[no \key]'
                  IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *               GO TO 990
                  WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *               KEYSTR, PERTMP, FOLDED
                  END IF
C                                       loop to scan for keys:
               I = 1
 125           CONTINUE
                  J = INDEX (GRIPE(I:), '\key{')
                  IF ((J.EQ.0) .AND. (I.GT.1)) GO TO 110
                  IF (J.GT.0) THEN
                     I = I + J - 1
                     J = MAX (INDEX (GRIPE(I:), '}'), N5) - 2
                     KEYSTR = GRIPE(I+5:I+J)
                  ELSE
                     KEYSTR = '[no \key]'
                     I = 1
                     END IF
                  I = I + 4
                  IF (KEYSTR.EQ.' ') KEYSTR = '[empty \key{}]'
                  IF (KEYSTR(1:4).EQ.'\tt ') KEYSTR = KEYSTR(5:)
                  IF (STR$UPCASE(FOLDED, KEYSTR).NE.SS$_NORMAL)
     *               GO TO 990
C                                       desired key?
                  DOKEY = NKEYL.LE.0
                  IF (.NOT.DOKEY) THEN
                     DO 130 J = 1,NKEYL
                        IF (KEYSTR.EQ.KEYL(J)) DOKEY = .TRUE.
 130                    CONTINUE
                     END IF
                  IF (.NOT.DOKEY) GO TO 125
C                                       decide whether to print:
                  IF ((PERSON(1:1).EQ.' ') .OR.
     *               (INDEX(PERSON, '/').GT.0) .OR.
     *               (INDEX(PERSON, '?').GT.0) .OR.
     *               (INDEX(PERSON, '<').GT.0) .OR.
     *               (INDEX(PERSON, '.').GT.0) .OR.
     *               (INDEX(PERSON, ',').GT.0) .OR.
     *               (INDEX(PERSON, '\').GT.0) .OR.
     *               (ITRIM(PERSON).GE.MXPERS) .OR.
     *               (INDEX(KEYSTR, '\').GT.0) .OR.
     *               (INDEX(KEYSTR, '{').GT.0) .OR.
     *               (J+1.GT.MXKSTR))THEN
                     WRITE (L3,1124) CLASS, NGRIPE, CODE, KEYSTR,
     *                  PERSON
                     WRITE (L6,1124) CLASS, NGRIPE, CODE, KEYSTR,
     *                  PERSON
                     END IF
                  PERTMP = ' All keys.'
                  IF (ALLKEY) WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *               KEYSTR, PERTMP, FOLDED
C                 IF ((CODE.EQ.'f') .OR. (CODE.EQ.'c')) GO TO 125
C                                       loop for multiple-persons:
                  PERTMP = PERSON
                  CODTMP = CODE
 140              CONTINUE
                     J = INDEX (PERTMP, '/')
                     IF (J.EQ.0) J = ITRIM (PERTMP) + 1
                     PERTM2 = PERTMP(:J-1)
                     IF (PERTM2.EQ.'PatMoore') PERTM2 = 'Pat'
                     IF ((.NOT.DOTEX) .OR. (SPECIAL.EQ.PERTM2)
     *                     .OR. (SPECIAL.EQ.'Everyone')) THEN
                        WRITE (L2,1123) CLASS, NGRIPE, CODE,
     *                     KEYSTR, PERTM2, FOLDED
                        IF (DOTEX) THEN
                           WRITE (L7KEYF,1140) CLASS, NGRIPE, CODE
                           IF ((L7KEYF.NE.L7LAST) .AND. (CODE.NE.'f')
     *                        .AND. (CODE.NE.'c')) WRITE (L7,16051)
     *                        L7KEYF, GRIPE
                           L7LAST = L7KEYF
                           END IF
                        END IF
                     IF (PERTM2.EQ.PERTMP) PERTMP = ' '
                     IF (J.LT.MXPERS) PERTMP = PERTMP(J+1:)
                     CODTMP = 's'
                     IF (PERTMP.NE.' ') GO TO 140
                  GO TO 125
C                                       End of individual input file
 150     CLOSE (UNIT=L4)
         GO TO 100
C                                       End of reading inputs
 180  CLOSE (UNIT=L1)
      COMMAND = 'DEL/LOG ' // L1FILE(:ITRIM(L1FILE)) // '.*'
      CALL DCLCMD (L6, COMMAND, ISTAT)
      CLOSE (UNIT=L2, STATUS='KEEP')
C                                       Sort on the keystrings:
      WRITE (COMMAND,1160) (KEYFLD+MXKSTR+1), MXPERS,
     *            (KEYFLD+MXKSTR+MXPERS+1), MXKSTR, L2FILE, L2FILE
      CALL DCLCMD (L6, COMMAND, ISTAT)
      IF (ISTAT.NE.0) STOP 'The SORT gave a bad status!'
      COMMAND = 'PURGE/LOG ' // L2FILE
      CALL DCLCMD (L6, COMMAND, ISTAT)
      IF (ISTAT.NE.0) STOP 'The PURGE gave a bad status!'
C                                       Print the list:
      OPEN (L2, FILE=L2FILE, STATUS='OLD')
      QUIT = .FALSE.
      LASTKEY = ' '
      LGRIPE = 0
      LASTJ = - 1
      LASTHD = 'nothing'
      LASTPER = LASTHD
 210  CONTINUE
         READ (L2,1123,END=212,ERR=280) CLASS, J, CODE, KEYSTR,
     *                                                      PERSON
            IF ((J.NE.LASTJ).OR.(KEYSTR.NE.LASTKEY)) GO TO 7211
               DO 7210 L = L3,L6,(L6-L3)
 7210             WRITE (L,17210) CLASS, J, CODE
 7211       CONTINUE
            LASTJ = J
            LGRIPE = LGRIPE + 1
            WRITE (KGRIPE(LGRIPE),'(A1,I4.4,A1)') CLASS, J, CODE
            IF ((.NOT.DOTEX) .OR. (PERSON.EQ.' All gripes.')
     *         .OR. (PERSON.EQ.' All keys.')) GO TO 211
               IF ((CODE.EQ.'f') .OR. (CODE.EQ.'c')) GO TO 211
               READ (L7,16051,KEYEQ=KGRIPE(LGRIPE)) L7KEYF, GRIPE
               IF (L7KEYF.NE.KGRIPE(LGRIPE)) STOP 'Bad L7KEYF !?!'
               CALL WRIGRI (L3, L8, NEWLIN, GRIPE)
 211        CONTINUE
C            WRITE (L3,'('' debug:'',A,1X,A,1X,A)')
C     *                        KGRIPE(LGRIPE), KEYSTR, PERSON
            GO TO 213
 212     CONTINUE
            QUIT = .TRUE.
            KEYSTR = 'garbage'
            PERSON = KEYSTR
            LGRIPE = LGRIPE + 1
            KGRIPE(LGRIPE) = KEYSTR
 213     CONTINUE
         IF ((KEYSTR.EQ.LASTKEY) .AND. (PERSON.EQ.LASTPER) .AND.
     *      (LGRIPE.LT.MXLPRT)) GO TO 210
         IF (LASTPER.EQ.'nothing') GO TO 230
            IF (LASTPER.EQ.LASTHD) GO TO 220
               WRITE (L3,1007) DATETIM
               DO 214 L = L3,L6,(L6-L3)
 214              WRITE (L,1214) LASTPER
               LASTHD = LASTPER
 220        CONTINUE
            KEYTMP = '  -  -  -  -  -  -  -  -  -  -  -  -  -  -'
            KEYTMP(MXKSTR:) = '- ->'
            J = MIN (ITRIM (LASTKEY) + N1, MXKSTR)
            KEYTMP(:J) = LASTKEY(:J)
            DO 225 L = L3,L6,(L6-L3)
               J = MIN ((LGRIPE-N1), NCOL)
               WRITE (L,1220) KEYTMP, (KGRIPE(I), I = 1,J)
               IF (J.EQ.(LGRIPE-1)) GO TO 225
               DO 6224 J = (NCOL+1),(LGRIPE-1),NCOL
                  K = MIN ((J+NCOL-N1), (LGRIPE-N1))
 6224             WRITE (L,1221) (KGRIPE(I), I = J,K)
 225           CONTINUE
            KGRIPE(1) = KGRIPE(LGRIPE)
            LGRIPE = 1
 230     CONTINUE
         IF (QUIT) GO TO 290
         LASTKEY = KEYSTR
         LASTPER = PERSON
         GO TO 210
 280  STOP 'READ-Error at stmnt.210!'
 290  CONTINUE
      CLOSE (UNIT=L2)
      COMMAND = 'DEL/LOG ' // L2FILE(:ITRIM(L2FILE)) // '.*'
      CALL DCLCMD (L6, COMMAND, ISTAT)
      CLOSE (UNIT=L3, DISPOSE='KEEP')
      COMMAND = 'PUR/LOG ' // L3FILE(:ITRIM(L3FILE))
      CALL DCLCMD (L6, COMMAND, ISTAT)
      IF (DOTEX) THEN
         WRITE (L8,1290)
         CLOSE (UNIT=L8, DISPOSE='KEEP')
         COMMAND = 'PUR/LOG ' // L8FILE(:ITRIM(L8FILE))
         CALL DCLCMD (L6, COMMAND, ISTAT)
         CLOSE (UNIT=L7)
         END IF
C                                       Thats all, folks:
 900  CONTINUE
      COMMAND = 'DIR/DAT/SIZ GRKEY*'
      CALL DCLCMD (L6, COMMAND, ISTAT)
      GO TO 999
C                                       Error: stops
 980  STOP 'Could not OPEN the file!'
 990  STOP 'Casefold failed!'
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT (' GRKEY: person name for a special listing: ', $)
 1011 FORMAT (A40)
 1015 FORMAT (' GRKEY: keyword name(s) for a special listing, ' ' ends')
 1016 FORMAT (' GRKEY: key number',I4,': ', $)
 1020 FORMAT (' GRKEY: produce the  "complete" list? T/F: ', $)
 1021 FORMAT (L1)
 1025 FORMAT (' GRKEY: produce the   "All key" list? T/F: ', $)
 1030 FORMAT (' GRKEY: produce the site/griper list? T/F: ', $)
 1035 FORMAT (' GRKEY:      special listing file will be: ', A)
 1036 FORMAT ('\input aipsgrip:gripemac5.tex', /, '\def\finalrun{F}', /)
 1040 FORMAT (' GRKEY: enter gripe file specification(s): ', $)
C
 1003 FORMAT (' ^PY^-'/' ^IOP^IL0600^IS404^IC1000^IJ00450^IT00700^PN^-')
 1007 FORMAT ('1               Gripe key listing prepared  ', A, /)
 1123 FORMAT (A1, I4.4, A1, A, A, A)
 1124 FORMAT (1X, A1, I4.4, A1, 1X, A, 1X, A)
 1140 FORMAT (A1, I4.4, A1)
16051 FORMAT (A<KEYFLD>, A<MAXCHAR>)
17210 FORMAT (' GRKEY: gripe ', A1, I4.4, A1, ' is a duplicate!')
 1160 FORMAT ('SOR/K=(POS:', I2, ',SIZ:', I2, ',ASC)',
     *           '/K=(POS:', I2, ',SIZ:', I2, ',ASC)',
     *           '/K=(POS:2,SIZ:4,ASC)/STAB ', A, 1X, A)
 1214 FORMAT ( '                             ------------', /,
     *         '                             ',A,           /,
     *         '                             ------------', /)
 1220 FORMAT (' ',        A, 1X, <NCOL>(A, 2X))
 1221 FORMAT (' ', <NSKIP>X, 1X, <NCOL>(A, 2X))
 1290 FORMAT (/, '\end')
      END
