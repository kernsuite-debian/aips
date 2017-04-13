      PROGRAM EXPFIT
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
C   EXPFIT writes files of arbitrary text to tape in FITS format
C   in order to generate export tapes for the AIPS system.
C   The concept is that the DIRECTORY command of VMS
C   can be used to generate a file containing the names of all files
C   of potential interest. For example, the command:
C      Directory/NoHead/NoTrail/Ver=1/Out=DIRECTORY.DAT UMA0:[AIPS...]
C   will produce a list of file names, one to a line, with the
C   device and subdirectory information prefixed to each name.
C   EXPFIT is designed to read such a file. It will look at the names.
C   Only certain file types will be processed. It also looks at the
C   directory specification. Each separate directory will produce a
C   separate FITS file. The directory names are written to the file
C   in UNIX notation (slash delimiters).
C
C   The text of the files is 'compressed'. The algorithm used
C   is intended to accomplish three goals: 1) strip trailing blanks,
C   2) compress contiguous strings of  blanks, and 3) transmit tabs,
c   form feeds, and end-of-information without using non-printing
C   ASCII codes. The idea is to use the exclamation mark as an
C   escape code. The coding rules are:
C      1) !!         = exclamation itself.
C      2) !,         = Tab code, presumably the 8-col DEC convention.
C      3) !;         = Newline.
C      4) !+         = Form Feed.
C      5) !.         = End-of-Information.
C      6) !c to !z   = 3 to 26 blanks.
C      7) !A to !Z   = 27 to 52 blanks.
C   The program produces a listing which reports various discrepancies
C   seen in the text: long lines, tabs or FFs, and non-printing codes.
C   The column positions of the tabs, FFs, and non-printing codes
C   are the positions in the line as recorded on the tape, not as in
C   the input file.
C   Notes:
C      1. consider encoding the modification date into the FITS file.
C      2. detab the text? suppress FFs?
C   DCW, NRAO-CV, 30Nov82, 25July83.
C   EWG 8Apr86
C-----------------------------------------------------------------------
      INTEGER   FNL, LNL, NTACC, NTREJ, MLST, MLONG
      PARAMETER (FNL=80, LNL=256, NTACC=14, NTREJ=20, MLST=1500,
     *   MLONG=300)
      INTEGER   IBUFF(256), ITAPE, TLUN, TIND, IERR, IFILE, I1, NFILE,
     *   LF, LI, LL, LK, LP, LD, I, J, K, ND, MLL, M, ITRIM, NLINES,
     *   NFILES, NWROTE, NREJCT, NUNREC, NLONG, NBAD, NSKIP, IMON,
     *   IDAY, IYR, NHTFF, NBYTES, JJ, LONG(MLONG), LO, L, LS
      REAL      BADCHR(MLONG), HTFF(MLONG)
      CHARACTER FN*(FNL), F*(FNL), D*(FNL), DL*(FNL), DT*(FNL),
     *   LN*(LNL), FT*3, TEMP*50, YESQ*3, CDATE*88, BLANKS*(LNL),
     *   TACC(NTACC)*3, TREJ(NTREJ)*3, DS*(FNL), PHNAME*48, PRGNAM*6,
     *   WROTE(MLST)*13, REJECT(MLST)*13, UNREC(MLST)*13, SC*(LNL)
      LOGICAL   EX, EOF, BUSY, ISMC4, ISDOC, DOALL, ISCOS, ISUNIX,
     *   DOTHIS
C                                       FITS block declaration:
      CHARACTER B*2880, C*80, S*(200), OPER*4, CFILE*16, RESTRT*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /CARDSI/ LO, L, LS
      COMMON /CARDSC/ B, C, S
      DATA TACC /'FOR', 'MAR', 'INC', 'DAT', 'COM', 'HLP', 'VFC', 'AP ',
     *           'C  ', 'FTN', 'CFT', 'S  ', '001', 'OPT'/
      DATA TREJ /'EXE', 'OBJ', 'LIS', 'DIR', 'OLB', 'MAP', 'JOU', 'TMP',
     *           'ML',  'CPK', 'LIB', 'PRT', 'ERR', 'SAV', 'OLD', 'JNK',
     *           'CHK', 'UPD', 'LOG', 'SRC'/
      DATA BLANKS /' '/
      DATA LF, LI, LO, LL, LK, LP, LD
     *    / 1,  2,  3,  4,  5,  6, 7/
      DATA PRGNAM /'EXPFIT'/
C-----------------------------------------------------------------------
      CALL IDATE (IMON, IDAY, IYR)
      CDATE = ' '
      CALL DATE (CDATE(1:9))
      CALL TIME (CDATE(11:18))
      BUSY = .FALSE.
      DOALL = .FALSE.
      DL = '[...]'
      DT = '[...]'
      L = 0
      ND = 0
      NBYTES = 0
      NLINES = 0
      NFILES = 0
      NWROTE = 0
      NREJCT = 0
      NUNREC = 0
C                                       Check on restart problem
      TSKNAM = PRGNAM
      NLUSER = 1
      CALL ZDCHIN (.TRUE., IBUFF)
      WRITE (LP,1800)
      READ (LK,1801) ITAPE
      TLUN = 30 + ITAPE
      OPER = 'OPEN'
      CALL ZPHFIL ('MT', ITAPE, 0, 0, PHNAME, IERR)
      CALL ZTPOPN (TLUN, TIND, ITAPE, PHNAME, 'READ', IERR)
      IF (IERR.NE.0) GO TO 950
      OPER = 'REWI'
      IFILE = 0
      WRITE (LP,1802)
      CALL ZTAPE (OPER, TLUN, TIND, IFILE, IERR)
      IF (IERR.NE.0) GO TO 950
      WRITE (LP,1803)
      DO 805 IFILE = 1,200
         WRITE (OPER,1805) IFILE
         I1 = 2
         CFILE = 'AIPSTAPE' // OPER(I1:4) // '.LIS;1'
         INQUIRE (FILE=CFILE, EXIST=EX)
         IF (.NOT.EX) GO TO 810
 805     CONTINUE
 810  IFILE = IFILE - 1
      RESTRT = ' '
      IF (IFILE.GT.0) THEN
         RESTRT = '>'
         OPER = 'ADVF'
         WRITE (LP,1810) IFILE
         CALL ZTAPE (OPER, TLUN, TIND, IFILE, IERR)
         IF (IERR.NE.0) GO TO 950
         WRITE (LP,1811) IFILE
         END IF
      OPER = 'CLOS'
      CALL ZTPCLS (TLUN, TIND, IERR)
      IF (IERR.NE.0) GO TO 950
      NFILE = 0
C                                       Open filename & log files:
      OPEN (UNIT=LF, FILE='DIRECTORY.DAT', STATUS='OLD', READONLY)
      WRITE (LP,1044) 'EXPFIT began:', CDATE
      ND = IFILE
      IF (IFILE.GT.0) WRITE (LP,1815) IFILE
C                                       Read next file name:
 10   CONTINUE
         EOF = .TRUE.
         READ (LF,'(A)',END=21) FN
         EOF = .FALSE.
C                                       Strip device-code & version #:
         F = FN
         I = INDEX (F, '[')
         J = INDEX (F, ';')
         IF ((I.EQ.0) .OR. (J.EQ.0)) GO TO 45
         SC = F(I+1:J-1)
         F = SC
C                                       Extract directory:
         I = INDEX (F, ']')
         IF (I.EQ.0) GO TO 45
         D = F(1:I-1)
C                                       Extract the file name:
         SC = F(I+1:FNL)
         F = SC
C                                       Convert to UNIX notation:
         ISDOC = .FALSE.
         ISMC4 = .FALSE.
         ISUNIX = .FALSE.
         ISCOS = .FALSE.
         JJ = 0
 15      CONTINUE
            J = INDEX (D, '.')
            IF (J.EQ.0) GO TO 20
               D(J:J) = '/'
               IF (D(JJ+1:J-1).EQ.'DOC') ISDOC = .TRUE.
               IF (D(JJ+1:J-1).EQ.'TEXT') ISDOC = .TRUE.
               IF (D(JJ+1:J-1).EQ.'MC4') ISMC4 = .TRUE.
               IF (D(JJ+1:J-1).EQ.'COS') ISCOS = .TRUE.
               IF (D(JJ+1:J-1).EQ.'UNIX') ISUNIX = .TRUE.
               JJ = J
               GO TO 15
C                                       Do we want this file?
 20      J = ITRIM (D) + 1
         IF (D(JJ+1:J-1).EQ.'COS') ISCOS = .TRUE.
         IF (D(JJ+1:J-1).EQ.'UNIX') ISUNIX = .TRUE.
         IF (D(JJ+1:J-1).EQ.'DOC') ISDOC = .TRUE.
         IF (D(JJ+1:J-1).EQ.'TEXT') ISDOC = .TRUE.
         IF (D(JJ+1:J-1).EQ.'MC4') ISMC4 = .TRUE.
         IF (ISMC4) GO TO 10
C                                       Print summary lists:
         IF ((.NOT.BUSY) .OR. (D.EQ.DT)) GO TO 22
 21         IF (DT.NE.DL) THEN
               WRITE (LL,1020) DT(1:ITRIM(DT))
               WRITE (LP,1020) DT(1:ITRIM(DT))
               END IF
            DT = D
            JJ = MIN (NWROTE, MLST)
            IF (NWROTE.GT.0) THEN
               WRITE (LL,1070) NWROTE, 'written', (WROTE(I), I = 1,JJ)
               WRITE (LP,1070) NWROTE, 'written', (WROTE(I), I = 1,JJ)
               END IF
            JJ = MIN (NREJCT, MLST)
            IF (NREJCT.GT.0) THEN
               WRITE (LL,1070) NREJCT, 'rejected', (REJECT(I), I = 1,JJ)
               WRITE (LP,1070) NREJCT, 'rejected', (REJECT(I), I = 1,JJ)
               END IF
            JJ = MIN (NUNREC, MLST)
            IF (NUNREC.GT.0) THEN
               WRITE (LL,1070) NUNREC, 'unrecognized',
     *            (UNREC(I), I = 1,JJ)
               WRITE (LP,1070) NUNREC, 'unrecognized',
     *            (UNREC(I),  I = 1,JJ)
               END IF
            NWROTE = 0
            NREJCT = 0
            NUNREC = 0
            IF (EOF) GO TO 60
C                                       Extract the file type:
 22      I = INDEX (F, '.')
         IF (I.EQ.0) GO TO 45
         FT = F(I+1:FNL)
C                                       Accept this type?
         DO 25 I = 1,NTACC
            IF (FT.EQ.TACC(I)) GO TO 50
 25         CONTINUE
C                                       Reject this type?
         DO 30 I = 1,NTREJ
            IF (FT.EQ.TREJ(I)) GO TO 40
 30         CONTINUE
C                                       Special cases:
         IF (F(1:8).EQ.'EMACS64.') GO TO 40
         I = ITRIM (D)
         IF ((D(I-3:I).EQ.'HIST') .AND. (F(1:10).EQ.'CHANGE.DOC'))
     *      GO TO 50
         IF (D(I-3:I).EQ.'HIST') GO TO 40
         IF (D(I-5:I).EQ.'ERRORS') GO TO 40
         IF ((ISCOS) .AND. (FT.EQ.'JOB')) GO TO 50
         IF ((ISCOS) .AND. (FT.EQ.'UK')) GO TO 50
         IF ((ISUNIX) .AND. (FT.EQ.'SED')) GO TO 50
         IF ((ISUNIX) .AND. (FT.EQ.'AWK')) GO TO 50
         IF ((ISUNIX) .AND. (FT.EQ.'CSH')) GO TO 50
         IF ((ISUNIX) .AND. (FT.EQ.'SH')) GO TO 50
         IF (ISDOC) THEN
            IF (D(I-3:I).EQ.'GRIP') GO TO 40
            IF (D(I-2:I).EQ.'WHO') GO TO 40
            IF (D(I-3:I).EQ.'PUBL') THEN
               IF ((FT.EQ.'TEX') .AND. (F(1:4).EQ.'COOK')) GO TO 50
               IF ((FT.EQ.'MAC') .AND. (F(1:4).EQ.'COOK')) GO TO 50
               GO TO 40
               END IF
            IF (D(I-3:I).EQ.'TEXT') THEN
               IF (FT.EQ.'RNO') GO TO 50
               IF (FT.EQ.'TEX') GO TO 50
               IF (FT.EQ.' ') GO TO 50
               GO TO 40
               END IF
            END IF
C                                       Type not recognized!
         NUNREC = NUNREC + 1
         IF (NUNREC.LE.MLST) UNREC(NUNREC) = F
C                                       Inquire about it
         IF (DOALL) GO TO 50
            WRITE (LP,1035) FN(1:ITRIM(FN))
            READ (LK,1036) YESQ
            IF (YESQ.EQ.'All') THEN
               DOALL = .TRUE.
               GO TO 50
               END IF
            IF ((YESQ(:1).EQ.'Y') .OR. (YESQ(:1).EQ.'y')) GO TO 50
C                                       Reject this file:
 40      CONTINUE
            NREJCT = NREJCT + 1
            IF (NREJCT.LE.MLST) REJECT(NREJCT) = F
            GO TO 10
C                                       Bad file name?
 45      CONTINUE
            WRITE (LL,1044) 'Bad file name:', FN
            WRITE (LP,1044) 'Bad file name:', FN
            GO TO 10
C                                       Name OK, press on:
C                                       Does the file exist?
 50      INQUIRE (FILE=FN, EXIST=EX)
         IF (.NOT.EX) THEN
            WRITE (LL, 1044) 'Nonexistant file:', FN
            WRITE (LP, 1044) 'Nonexistant file:', FN
            GO TO 10
            END IF
C                                       New directory
         DS = D
         IF (D.EQ.DL) GO TO 90
            DL = D
            DT = D
            NFILE = NFILE + 1
            DOTHIS = NFILE.GT.IFILE
            IF (.NOT.DOTHIS) THEN
               WRITE (LP,1820) NFILE
               GO TO 10
               END IF
C                                       Finish old FITS file:
 60         IF (BUSY) THEN
               WRITE (C,1060) 'END     ', ' '
               CALL W
               WRITE (LD,1061) C
 65            IF (L.EQ.0) GO TO 70
                  C = ' '
                  CALL W
                  GO TO 65
 70            CLOSE (UNIT=LO)
               IF (EOF) GO TO 900
               WRITE (LL,1075)
               WRITE (LL,1900) 'files', NFILES, RESTRT, 'lines',
     *            NLINES, RESTRT, 'bytes', NBYTES, RESTRT
               WRITE (LP,1075)
               WRITE (LP,1900) 'files', NFILES, RESTRT, 'lines',
     *            NLINES, RESTRT, 'bytes', NBYTES, RESTRT
               CLOSE (UNIT=LL)
               CLOSE (UNIT=LD)
               BUSY = .FALSE.
               END IF
C                                       Start new FITS file:
 75         WRITE (OPER,1805) NFILE
            I1 = 2
            CFILE = 'AIPSTAPE' // OPER(I1:4) // '.LIS;1'
            OPEN (UNIT=LL, FILE=CFILE, STATUS='NEW')
            WRITE (LL, 1044) 'EXPFIT began:', CDATE
            WRITE (LL,1075)
            WRITE (LP,1075)
            ND = ND + 1
            WRITE (TEMP, '(''Directory'', I3.2, '':'')') ND
            WRITE (LL,1044) TEMP(1:ITRIM(TEMP)), D
            WRITE (LP,1044) TEMP(1:ITRIM(TEMP)), D
            WRITE (LL,'(1X)')
            WRITE (OPER,1805) NFILE
            I1 = 2
            CFILE = 'EXPFIT' // OPER(I1:4) // '.TXT;1'
            OPEN (UNIT=LD, FILE=CFILE, STATUS='NEW')
            WRITE (LD,1076) NFILE
            IF (ND.EQ.1) GO TO 300
            OPEN (UNIT=LO, FILE='AIPSTAPE:', STATUS='NEW', RECL=2880,
     *         ACCESS='SEQUENTIAL', FORM='FORMATTED', BUFFERCOUNT=2,
     *         RECORDTYPE='VARIABLE', CARRIAGECONTROL='NONE')
            BUSY = .TRUE.
            WRITE (C,1071) 'SIMPLE  ', .TRUE.,
     *         'FITS tape written by program EXPFIT.'
            L = 0
            CALL W
            WRITE (LD,1061) C
            WRITE (C,1072) 'BITPIX  ', 8, '(dummy value)'
            CALL W
            WRITE (LD,1061) C
            WRITE (C,1072) 'NAXIS   ', 0, ' '
            CALL W
            WRITE (LD,1061) C
            TEMP = 'NRAOCVAX'
            WRITE (C,1073) 'ORIGIN  ', TEMP(1:MAX(18,ITRIM(TEMP))), ' '
            CALL W
            WRITE (LD,1061) C
            WRITE (TEMP,'(I2.2, 2(''/'', I2.2))') IDAY, IMON, IYR
            WRITE (C,1073) 'DATE    ', TEMP(1:18),
     *         CDATE(1:ITRIM (CDATE))
            CALL W
            WRITE (LD,1061) C
            WRITE (C,1060) 'COMMENT ',
     *'!!=!; !,=Tab; !;=CR/LF; !+=FF; !.=EOI; !c=3 blanks; !Z=52 blanks'
            CALL W
            WRITE (LD,1061) C
C                                       drop first level
            J = INDEX (D, '/')
            IF (J.LE.0) J = 1
            I = ITRIM (D)
            SC = D(J:I)
            D = SC
            I = I - J + 1
            JJ = 1
 80         J = INDEX (D(JJ:I), '/')
               J = JJ + J - 1
               IF (J.LT.JJ) J = I + 1
               IF (J.GT.1) WRITE (C,'(A)') 'DIRECTOR= ''.' // D(1:J-1)
     *            // ''''
               IF (J.GT.1) THEN
                  CALL W
                  WRITE (LD,1061) C
                  END IF
               JJ = J + 1
               IF (JJ.LT.I) GO TO 80
            NWROTE = 0
            NREJCT = 0
            NUNREC = 0
C                                       Write the file to the tape:
 90      IF (.NOT.DOTHIS) GO TO 10
         WRITE (C,'(A)') 'FILENAME= ''' // F(1:ITRIM(F)) // ''''
         CALL W
         WRITE (LD,1061) C
         OPEN (UNIT=LI, FILE=FN, STATUS='OLD', READONLY, SHARED)
         NLONG = 0
         NHTFF = 0
         NBAD = 0
         K = 0
         MLL = 80
         IF (FT.EQ.'FOR') MLL = 72
         IF (FT.EQ.'INC') MLL = 72
         IF (FT.EQ.'HLP') MLL = 64
         IF (FT.EQ.'INP') MLL = 64
         S = ' '
         LS = 0
C                                       The copy & compress loop:
 100     CONTINUE
            READ (LI,'(A)',END=200) LN
            J = ITRIM (LN)
            K = K + 1
            IF (J.GT.MLL) THEN
               NLONG = NLONG + 1
               IF (NLONG.LE.MLONG) LONG(NLONG) = K
               END IF
C                                       Check for special characters:
            I = 0
 110        I = I + 1
            IF (I.LE.J) THEN
               M = ICHAR (LN(I:I))
C                                       Printing codes:
C                                       Is it exclamation?
               IF (LN(I:I).EQ.'!') THEN
                  SC = LN
                  LN(I+1:LNL) = '!' // SC(I+1:LNL)
C                                       Non-Printing codes:
C                                       Is it TAB?
               ELSE IF (M.EQ.9) THEN
                  SC = LN
                  LN(I:I) = '!'
                  LN(I+1:LNL) = ',' // SC(I+1:LNL)
                  NHTFF = MIN (NHTFF + 1, MLONG)
                  HTFF(NHTFF) = K + (I / 100.0)
C                                       Is it FF?
               ELSE IF (M.EQ.12) THEN
                  SC = LN
                  LN(I:I) = '!'
                  LN(I+1:LNL) = '+' // SC(I+1:LNL)
                  NHTFF = NHTFF + 1
                  IF (NHTFF.LE.MLONG) HTFF(NHTFF) = K + (I / 100.0)
C                                       Nonprinting codes:
               ELSE IF ((M.LE.31) .OR. (M.GE.127)) THEN
                  NBAD = NBAD + 1
                  IF (NBAD.LE.MLONG) BADCHR(NBAD) = K + (I / 100.0)
                  SC = LN
                  LN(I:LNL) = SC(I+1:LNL)
                  J = J - 2
                  I = I - 2
                  END IF
               J = J + 1
               I = I + 1
               GO TO 110
               END IF
C                                       Squeeze blanks:
 160        M = MIN (52, J)
 170        IF ((J.LT.1) .OR. (M.LT.1)) GO TO 190
               I = INDEX (LN(1:J), BLANKS(1:M))
               IF (I.GT.0) THEN
                  LN(I:I) = '!'
                  IF (M.LT.27) LN(I+1:I+1) = CHAR (ICHAR('a') + M - 1)
                  IF (M.GT.26) LN(I+1:I+1) = CHAR (ICHAR('A') + M - 27)
                  SC = LN
                  LN(I+2:LNL) = SC(I+M:LNL)
                  J = J - M + 2
                  M = MIN (M, J-2)
               ELSE
                  M = M - 1
                  IF (M.LE.2) GO TO 190
                  END IF
               GO TO 170
C                                       Write out the text:
 190        CALL MASH (LN(1:J))
            NBYTES = NBYTES + J
            NLINES = NLINES + 1
            GO TO 100
C                                       Add EOI mark:
 200     S(LS+1:) = '!.'
         LS = LS + 2
 210     IF (LS.LT.1) GO TO 220
            WRITE (C,1060) ' ', S(1:72)
            CALL W
            S = S(73:MAX (LS, 73))
            LS = LS - 72
            GO TO 210
 220     CLOSE (UNIT=LI)
         NFILES = NFILES + 1
         NWROTE = NWROTE + 1
         IF (NWROTE.LE.MLST) WROTE(NWROTE) = F
         WRITE (TEMP, '(''Lengths >'', I3.2, '':'')') MLL
         JJ = MIN (NLONG, MLONG)
         IF (NLONG.GT.0) THEN
            I = ITRIM (TEMP)
            WRITE (LL,1044) TEMP(1:I), F
            WRITE (LP,1044) TEMP(1:I), F
            WRITE (LL,1222) (LONG(I), I = 1,JJ)
            WRITE (LP,1222) (LONG(I), I = 1,JJ)
            END IF
         JJ = MIN (NBAD, MLONG)
         IF (NBAD.GT.0) THEN
            WRITE (LL,1044) 'Bad characters:', F
            WRITE (LP,1044) 'Bad characters:', F
            WRITE (LL,1220) (BADCHR(I), I = 1,JJ)
            WRITE (LP,1220) (BADCHR(I), I = 1,JJ)
            END IF
         IF (NHTFF.GT.0) THEN
            WRITE (LL,1044) 'Tabs or FFs:', F
            WRITE (LP,1044) 'Tabs or FFs:', F
            JJ = MIN (NHTFF, MLONG)
            WRITE (LL,1220) (HTFF(I), I = 1,JJ)
            WRITE (LP,1220) (HTFF(I), I = 1,JJ)
            END IF
 230     WRITE (LP,1230) DS(1:ITRIM(DS)) // ' / ' // F(1:ITRIM(F))
         GO TO 10
C                                       First file: unblocked
 300  CONTINUE
         BUSY = .FALSE.
         OPEN (UNIT=LO, FILE='AIPSTAPE:', STATUS='NEW', RECL=80,
     *      ACCESS='SEQUENTIAL', FORM='FORMATTED', BUFFERCOUNT=2,
     *      RECORDTYPE='VARIABLE', CARRIAGECONTROL='NONE')
         REWIND LO
         OPEN (UNIT=LI, FILE=FN, STATUS='OLD', READONLY, SHARED)
         NLINES = 0
 310     READ (LI,'(A)',END=320) LN
            WRITE (LO,'(A)') LN(1:80)
            WRITE (LD,1061) LN(1:80)
            NLINES = NLINES + 1
            GO TO 310
 320     CLOSE (UNIT=LO)
         CLOSE (UNIT=LI)
         DL = '[...]'
         WRITE (LL,1320) NLINES, F(1:ITRIM(F))
         WRITE (LP,1320) NLINES, F(1:ITRIM(F))
         CLOSE (UNIT=LL)
         CLOSE (UNIT=LD)
         GO TO 10
C                                       That's all, folks:
 900  WRITE (LL,1075)
      WRITE (LP,1075)
      WRITE (LL,1900) 'files', NFILES, RESTRT, 'lines', NLINES,
     *    RESTRT, 'bytes', NBYTES, RESTRT
      WRITE (LP,1900) 'files', NFILES, RESTRT, 'lines', NLINES,
     *    RESTRT, 'bytes', NBYTES, RESTRT
      CLOSE (UNIT=LF)
      CLOSE (UNIT=LL)
      CLOSE (UNIT=LD)
C                                       concatenate the files
      WRITE (LP,1910)
      DO 915 IFILE = 1,200
         WRITE (OPER,1805) IFILE
         I1 = 2
         CFILE = 'AIPSTAPE' // OPER(I1:4) // '.LIS;1'
         INQUIRE (FILE=CFILE, EXIST=EX)
         IF (.NOT.EX) GO TO 920
         IF (IFILE.EQ.1) OPEN (UNIT=LD, FILE='AIPSTAPE.LIS',
     *      STATUS='NEW')
         OPEN (UNIT=LL, FILE=CFILE, STATUS='OLD', READONLY)
 905     READ (LL,'(A)',END=910) C
            WRITE (LD,'(A)') C
            GO TO 905
 910     CLOSE (UNIT=LL)
 915     CONTINUE
 920  CLOSE (UNIT=LD)
      WRITE (LP,1920)
      DO 935 IFILE = 1,200
         WRITE (OPER,1805) IFILE
         I1 = 2
         CFILE = 'EXPFIT' // OPER(I1:4) // '.TXT;1'
         INQUIRE (FILE=CFILE, EXIST=EX)
         IF (.NOT.EX) GO TO 940
         IF (IFILE.EQ.1) OPEN (UNIT=LD, FILE='EXPFIT.TXT',
     *      STATUS='NEW')
         OPEN (UNIT=LL, FILE=CFILE, STATUS='OLD', READONLY)
 925     READ (LL,'(A)',END=930) C
            WRITE (LD,'(A)') C
            GO TO 925
 930     CLOSE (UNIT=LL)
 935     CONTINUE
 940  CLOSE (UNIT=LD)
      GO TO 999
C
 950  WRITE (LP,1950) IERR, OPER
C
 999  STOP
C-----------------------------------------------------------------------
 1020 FORMAT ('0',9X,'In directory: ',A)
 1035 FORMAT ('0DO WE KEEP (YES/NO) ',A)
 1036 FORMAT (A3)
 1044 FORMAT (10X,A20,1X,A49)
 1060 FORMAT (A8,A72)
 1061 FORMAT (' ',A80)
 1070 FORMAT (10X,I4,' files ',A,':',/(8X,5(1X,A13)))
 1071 FORMAT (A8,'= ',L20,' / ',A)
 1072 FORMAT (A8,'= ',I20,' / ',A)
 1073 FORMAT (A8,'= ''',A,''' / ',A)
 1075 FORMAT (9X,63('-'))
 1076 FORMAT (/11X,'FILE = ',I3//)
 1220 FORMAT (8X,8F8.2)
 1222 FORMAT (5X,8I8)
 1230 FORMAT (10X,'Wrote ',A)
 1320 FORMAT (' Wrote',I5,' unblocked lines in file 1 for ',A)
 1900 FORMAT (10X,'Total number of ',A,' written =',I9,3X,A)
 1910 FORMAT (5X,'Merge AIPSTAPEnnn.LIS;* into AIPSTAPE.LIS')
 1920 FORMAT (5X,'Merge EXPFITnnn.TXT;* into EXPFIT.TXT')
 1950 FORMAT (' AIPS Z-ROUTINE ERROR',I7,' OPER ',A)
 1800 FORMAT (' Enter AIPS tape drive number: '$)
 1801 FORMAT (I1)
 1802 FORMAT (' Rewinding the tape')
 1803 FORMAT (' Seeing if there are already AIPSTAPEnnn.LIS files')
 1805 FORMAT (I4.3)
 1810 FORMAT (' Begin skipping over first',I7,' files')
 1811 FORMAT (' End skipping over first',I7,' files')
 1815 FORMAT (' Read DIRECTORY.DAT, skipping first',I7,' files')
 1820 FORMAT (' Begin skip file',I7,' in list')
      END
      SUBROUTINE W
C-----------------------------------------------------------------------
C   Write a card to the FITS tape.
C-----------------------------------------------------------------------
      INTEGER        I
C                                       FITS block declaration:
      INTEGER        LO, L, LS
      CHARACTER      B*2880, C*80, S*(200)
      COMMON /CARDSI/ LO, L, LS
      COMMON /CARDSC/ B, C, S
C-----------------------------------------------------------------------
      IF (L.EQ.0) B = ' '
      I = L*80 + 1
      B(I:I+79) = C
      L = L + 1
      IF (L.LT.36) GO TO 999
         WRITE (LO,'(A)') B
         L = 0
C
 999  RETURN
      END
      SUBROUTINE MASH (T)
C-----------------------------------------------------------------------
C   Argument T is a string to be appended to the output text.
C-----------------------------------------------------------------------
      CHARACTER      T*(*)
C
      INTEGER        J, LO, L, LS
      CHARACTER      B*2880, C*80, S*(200)
      COMMON /CARDSI/ LO, L, LS
      COMMON /CARDSC/ B, C, S
C-----------------------------------------------------------------------
      J = LEN (T)
      IF (LS.EQ.0) S = T(1:J) // '!;'
      IF (LS.GT.0) S = S(1:LS) // T(1:J) // '!;'
      LS = LS + J + 2
  10  IF (LS.LT.72) GO TO 999
         WRITE (C,1010) ' ', S(1:72)
         CALL W
         S = S(73:MAX(LS,73))
         LS = LS - 72
         GO TO 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A8, A72)
      END
