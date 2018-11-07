      SUBROUTINE AU1A (BRANCH)
C-----------------------------------------------------------------------
C! does parameter display: INPUTS, SHOW, HELP, EXPLAIN
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2001-2002, 2004-2008, 2014-2015
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
C   AU1A performs the user assistance facilities: HELP, INP, and INPUTS.
C   HELP lists symbols by type or lists a text file whose member name
C   matches a user name.  INP is a fast version of INPUTS (no message
C   file used).  INPUTS lists the adverbs and their current values and
C   brief descriptions on the terminal.  EXPLAIN lists both the HELP and
C   an extended explain portion of the HELP file.  All 3 "files" are in
C   one text file (Inputs, Help, Explain order) separated by ------'s.
C   Inputs:
C      BRANCH   I   = 1 do HELP listings
C                   = 2 do fast INPUTS listing (called INP)
C                   = 3 do normal INPUTS listing
C                   = 4 do EXPLAIN (DOCRT controls device)
C                   = 5 SHOW: INPUTS for TELL operation
C                   = 6 APROPOS which HELPs have the string(s)
C                   = 7 ABOUT - show a list help file
C                   = 8 XHELP - hypertext help
C                   = 9 OUTPUTS listing
C   Inputs file function codes are in column 10 as:
C      ' '   GO
C      '*'   GO  TELL
C      '?'       TELL
C      '&'   GO  TELL  GET
C      '%'       TELL  GET
C      '$'   GO        GET
C      '@'             GET
C      other -> error message
C   INPUTS, TPUT, TGET and GO only do the GO adverbs, OUTPUTS only does
C   the GET adverbs, SHOW and TELL only do the TELL adverbs
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   MAXNAM, NUMCAT, NUMSYN
      PARAMETER (MAXNAM=500)
      PARAMETER (NUMCAT=42)
      PARAMETER (NUMSYN=33)
C
      INTEGER   NSYSAD, NSYSTA, NSYSDA, NSYSCH, NSYSIM
      PARAMETER (NSYSAD=36, NSYSTA=2, NSYSDA=9, NSYSCH=4, NSYSIM=11)
      CHARACTER TITL1*132, TITL2*132, PLINE*132, RSCR*132, PHNAME*48,
     *   IXMEM*12, LVERSN*48, AVERSN*48, JBUF*80, STRING*24, FNULL(4)*9,
     *   RDASH*4, TYPNAM(8)*10, SYSADV(NSYSAD)*8, PRGNAM*6, KARSAV*80,
     *   MSGSAV*80, CDUM*1, OPTELL*4, WORDS(12)*15, NAMES(MAXNAM)*10,
     *   TWORD*1, BWORD*1, AWORD*1, SNAMES(MAXNAM)*10, TNAME*10,
     *   CATEG(NUMCAT)*12, SCATEG(2,NUMSYN)*12, CODE*1, LDTASK*6
      INTEGER  POTERR, IERR, JTRIM, FRMT, INPRIO, NMATCC, IPTRL, JTR,
     *   SCRTCH(256), FLDLEN, DECLEN, COLLBN, COLUBN, COLCOM, HLPLUN,
     *   INPLUN, LUN, VOL, SEQ, VER, FIND, I, IEOF, ILOC, IPTR, ISIZE,
     *   IT, LXPTR, LWORD, ITAG, ITYPE, J, JJLIM, KBP, LL, LLTYP,
     *   LSIZE, M, MM, NDIM, NUM, NTYPES, TYPNUM(8,2), PIND, ITRIM,
     *   MSGLIM, JSIZE, KSIZE, NCHRS(4), LINE, PAGE, NACROS, PLUN, NCOL,
     *   COLSHO, IDUM, NUMWRD, NUMNAM, LNAMES(MAXNAM), LWORDS(12), ZOR,
     *   LDNUMB, DNUMB, LNUMB, LDPRIO, LLSIZE, LCRTMX
      REAL      LIM(2), RMAX, RMIN, LMAX, LMIN, DDCRT, SYSLIM(2), RDUM
      LOGICAL   WAIT, EQUAL, F, QUIT
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PUVD.INC'
      SAVE LDNUMB, LDTASK, LDPRIO
      COMMON /AIPSCR/ SCRTCH
      COMMON /AIPCCR/ TITL1, TITL2, PLINE, RSCR, PHNAME, JBUF, KARSAV,
     *   MSGSAV, LVERSN, AVERSN
      DATA NCHRS, FNULL /6,7,8,9, '*all 0 ', '*rest 0 ',
     *   '*all '' '' ', '*rest '' '''/
      DATA FLDLEN, DECLEN, LMAX, LMIN /9, 4, 1.0E16, -1.E16/
      DATA COLLBN, COLUBN, COLCOM, COLSHO /11, 23, 35, 10/
      DATA PRGNAM /'AU1A  '/
      DATA INPLUN, HLPLUN /11, 11/
      DATA SEQ, VER, VOL /0,0,1/
      DATA RDASH /'----'/
      DATA F /.FALSE./
      DATA WAIT /.TRUE./
      DATA NTYPES /8/
      DATA TYPNUM /4, 1, 2, 7, 1, 3, 3, 5,
     *             5, 7, 2, 7, 1, 3, 3, 5/
      DATA TYPNAM /'VERBS ', 'ADVERBS ', 'ARRAYS ', 'STRINGS ',
     *   'REALS ', 'PROCS ', 'PROCEDURES', 'PSEUDOS '/
      DATA SYSADV /'INTAPE  ', 'OUTTAPE ', 'INDISK  ', 'IN2DISK ',
     *   'IN3DISK ', 'IN4DISK ', 'DDISK', 'MDISK', 'TDISK', 'OUTDISK ',
     *   'OUT2DISK', 'BCHAN', 'ECHAN', 'CHANSEL', 'CHANNEL',
     *   'BLC', 'TRC', 'TBLC', 'TTRC', 'PIXXY', 'IMSIZE', 'BOX',
     *   'CLBOX', 'FLDSIZE', 'PIX2XY', 'UVSIZE',
     *   'BATQUE', 'NUMTELL', 'PRNUMBER', 'TVCHAN', 'TV2CHAN', 'GRCHAN',
     *   'GR2CHAN', 'TVCORN', 'TVXY', 'TVLEVS'/
      DATA CATEG /'ANALYSIS', 'AP', 'ASTROMETRY', 'BATCH',
     *   'CALIBRATION', 'CATALOG', 'COORDINATES', 'EDITING', 'EXT-APPL',
     *   'FITS', 'GENERAL', 'HARDCOPY', 'IMAGE', 'IMAGE-UTIL',
     *   'IMAGING', 'INTERACTIVE', 'MODELING', 'OBSOLETE', 'OOP',
     *   'ONED', 'OPTICAL', 'PARAFORM', 'PLOT', 'POLARIZATION', 'POPS',
     *   'SINGLEDISH', 'SPECTRAL', 'TABLE', 'TAPE', 'TV', 'TV-APPL',
     *   'UTILITY', 'UV', 'VLA', 'VLBI', 'ADVERB', 'INFORMATION',
     *   'PROCEDURE', 'PSEUDOVERB', 'RUN', 'TASK', 'VERB'/
      DATA SCATEG /'BATCHJOB', 'BATCH',    'CALIBRATE', 'CALIBRATION',
     *             'FILES', 'CATALOG',     'POSITION', 'COORDINATES',
     *             'FLAGGING', 'EDITING',  'EXTENSION', 'EXT-APPL',
     *             'PRINTING', 'HARDCOPY', 'PRINTER', 'HARDCOPY',
     *             'MAP', 'IMAGE',         'MAP-UTIL', 'IMAGE-UTIL',
     *             'MAPPING', 'IMAGING',   'MODELS', 'MODELING',
     *             'PLOTS', 'PLOT',        'PLOTTING', 'PLOT',
     *             'LANGUAGE', 'POPS',     'CUBE', 'SPECTRAL',
     *             'LINE', 'SPECTRAL',     'TABLES', 'TABLE',
     *             'TAPES', 'TAPE',        'TVS', 'TV',
     *             'UTILITIES', 'UTILITY', 'VISIBILITY', 'UV',
     *             'VLBA', 'VLBI',         'ADVERBS', 'ADVERB',
     *             'PARAMETERS', 'ADVERB', 'HELPS', 'INFORMATION',
     *             'PROCS', 'PROCEDURE',   'PROCEDURES', 'PROCEDURE',
     *             'PSEUDOVERBS', 'PSEUDOVERB', 'RUNFILES', 'RUN',
     *             'TASKS', 'TASK',        'VERBS', 'VERB',
     *             'SLICE', 'ONED'/
C-----------------------------------------------------------------------
      IEOF = 0
      IERR = 0
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C                                        get member name from stack
      IF (BRANCH.NE.10) THEN
         POTERR = 7
         IF (SP.LT.4) GO TO 980
         POTERR = 8
         IF ((STACK(SP).NE.2) .OR. (STACK(SP-3).NE.14)) GO TO 980
         IF (BRANCH.NE.6) THEN
            I = MIN (12, STACK(SP-2))
            CALL H2CHR (I, 1, C(STACK(SP-1)), IXMEM)
            SP = SP - 4
            END IF
         END IF
      LCRTMX = CRTMAX
      IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3)) THEN
         CALL ADVERB ('DOPRINT', 'R', 1, 0, IDUM, RDUM, LVERSN)
         IF (ERRNUM.NE.0) GO TO 980
         IF (RDUM.LE.-9.0) LCRTMX = 20000
         END IF
      CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
      IF (ERRNUM.NE.0) GO TO 980
C
      GO TO (100, 200, 200, 100, 200, 400, 100, 500, 200, 200), BRANCH
C-----------------------------------------------------------------------
C            H    E    L    P
C            E    X    P    L    A    I    N
C-----------------------------------------------------------------------
C                                        get operation nature
 100  IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000) .OR.
     *   (TSKNAM(1:5).EQ.'AIPSC')) GO TO 999
C                                        TASK is default help
      IF (IXMEM.EQ.' ') THEN
         CALL ADVERB ('TASK', 'C', 1, 8, IDUM, RDUM, IXMEM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      IF (IXMEM.EQ.' ') IXMEM = 'HELP'
      CALL ADVERB ('DOCRT', 'R', 1, 0, IDUM, DDCRT, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (BRANCH.EQ.1) DDCRT = DDCRT + 1.0
      IF (BRANCH.EQ.7) DDCRT = DDCRT + 1.0
      IF (DDCRT.EQ.0.0) DDCRT = 1.0
      PAGE = 0
      LINE = 900
      TITL2 = ' '
      PLINE = ' '
      IF (DDCRT.GT.0.0) THEN
         PLUN = 5
         CALL LSERCH ('SRCH', PLUN, PIND, F, IERR)
         CALL ZWINC (NCOL)
         NACROS = DDCRT + 0.5
         IF ((NACROS.LE.72.) .OR. (NACROS.GT.NCOL)) NACROS = NCOL
      ELSE
         CALL ADVERB ('OUTPRINT', 'C', 1, 48, IDUM, RDUM, PHNAME)
         IF (ERRNUM.NE.0) GO TO 980
         IF (PHNAME.EQ.' ') DDCRT = -1.0
         CALL LPOPEN (PHNAME, DDCRT, PLUN, PIND, NACROS, SCRTCH, IERR)
         END IF
      IF (BRANCH.EQ.1) NACROS = NACROS - 1
      MSGLIM = NACROS - 9
      POTERR = 10
      IF (IERR.NE.0) GO TO 980
      POTERR = 8
      ITYPE = 0
      IF (BRANCH.NE.7) THEN
         DO 105 I = 1,NTYPES
            IF (IXMEM.EQ.TYPNAM(I)) THEN
               ITYPE = TYPNUM(I,1)
               LLTYP = TYPNUM(I,2)
               END IF
 105        CONTINUE
         END IF
      IF ((ITYPE.EQ.0) .OR. (BRANCH.EQ.7)) GO TO 130
      I = ITRIM (IXMEM)
      IF (BRANCH.EQ.1) WRITE (TITL1,1100) 'Help on', IXMEM(:I), RLSNAM
      IF (BRANCH.EQ.4) WRITE (TITL1,1100) 'Explanation of', IXMEM(:I),
     *   RLSNAM
      IF (DDCRT.LE.-2.5) THEN
         CALL PRTALN (PLUN, PIND, DDCRT, NACROS, TITL1, TITL2, PLINE,
     *      TITL1, PAGE, RSCR, QUIT)
         IF (ERRNUM.NE.0) GO TO 180
         IF (QUIT) GO TO 175
         END IF
C                                        list symbol table by type
      DO 120 SYTYPE = ITYPE,LLTYP
         L = 1
         IF ((ITYPE.EQ.1) .AND. (LLTYP.EQ.7)) THEN
            IF ((SYTYPE.GT.2) .AND. (SYTYPE.LT.7)) GO TO 120
            END IF
C                                        blank line ?
         IF (SYTYPE.NE.ITYPE) THEN
            PLINE = ' '
            CALL PRTALN (PLUN, PIND, DDCRT, NACROS, TITL1, TITL2, PLINE,
     *         LINE, PAGE, RSCR, QUIT)
            IF (ERRNUM.NE.0) GO TO 180
            IF (QUIT) GO TO 175
            END IF
C                                        start new line
 110     KBP = 1
            PLINE = ' '
C                                        search symbol table
 115        L = K(L)
            IF (L.NE.0) THEN
               LL = K(L+1)/16
               MM = K(L+1) - 16*LL
               IF (SYTYPE.NE.MM) GO TO 115
C                                        copy name
               LP = L + 4
               CALL H2CHR (LL, 1, CH(LP), PLINE(KBP:))
               KBP = KBP + MAX (8, LL) + 2
               IF (KBP.LE.MSGLIM) GO TO 115
               END IF
            IF (KBP.GT.1) THEN
               CALL PRTALN (PLUN, PIND, DDCRT, NACROS, TITL1, TITL2,
     *            PLINE, LINE, PAGE, RSCR, QUIT)
               IF (ERRNUM.NE.0) GO TO 180
               IF (QUIT) GO TO 175
               END IF
            IF (L.NE.0) GO TO 110
 120     CONTINUE
      GO TO 175
C                                       ABOUT - do min match
 130  IF (BRANCH.EQ.7) THEN
C                                       main names first
         MM = ITRIM (IXMEM)
         IT = 0
         LL = 0
         DO 135 I = 1,NUMCAT
            IF (IXMEM.EQ.CATEG(I)) THEN
               IXMEM = 'ZZ' // CATEG(I)(:8)
               GO TO 150
               END IF
            IF (IXMEM(:MM).EQ.CATEG(I)(:MM)) THEN
               IT = IT + 1
               LNAMES(IT) = I
               END IF
 135        CONTINUE
C                                       a min match
         IF (IT.EQ.1) THEN
            IXMEM = 'ZZ' // CATEG(LNAMES(1))(:8)
            GO TO 150
C                                       too many matches
         ELSE IF (IT.GT.1) THEN
            DO 140 LL = 1,IT
               WRITE (MSGTXT,1135) CATEG(LNAMES(LL))
               CALL MSGWRT (6)
 140           CONTINUE
            WRITE (MSGTXT,1140) IXMEM(:MM)
            CALL MSGWRT (6)
            GO TO 180
            END IF
C                                       try synonyms
         IT = 0
         LL = 0
         DO 145 I = 1,NUMSYN
            IF (IXMEM.EQ.SCATEG(1,I)) THEN
               IXMEM = 'ZZ' // SCATEG(2,I)(:8)
               GO TO 150
               END IF
            IF (IXMEM(:MM).EQ.SCATEG(1,I)(:MM)) THEN
               IT = IT + 1
               LNAMES(IT) = I
               END IF
 145        CONTINUE
C                                       a min match
         IF (IT.EQ.1) THEN
            IXMEM = 'ZZ' // SCATEG(2,LNAMES(1))(:8)
            GO TO 150
C                                       too many matches
         ELSE IF (IT.GT.1) THEN
C                                       but they are equivalent?
            IF ((IT.EQ.2) .AND.
     *         (SCATEG(2,LNAMES(1)).EQ.SCATEG(2,LNAMES(2)))) THEN
               IXMEM = 'ZZ' // SCATEG(2,LNAMES(1))(:8)
               GO TO 150
               END IF
            DO 146 LL = 1,IT
               WRITE (MSGTXT,1145) SCATEG(1,LL), SCATEG(2,LL)
               CALL MSGWRT (6)
 146           CONTINUE
            WRITE (MSGTXT,1140) IXMEM(:MM)
         ELSE
            WRITE (MSGTXT,1146) IXMEM(:MM)
            END IF
         CALL MSGWRT (6)
         GO TO 180
         END IF
C                                       User verbs: text from file
 150  LUN = HLPLUN
      CALL ZPHFIL ('HE', VOL, SEQ, VER, PHNAME, IERR)
C                                       Allow min match
      CALL VERMAT (VOL, PHNAME, IXMEM, LVERSN, I, AVERSN, IERR)
      IF (IERR.NE.0) GO TO 180
      CALL ZTOPEN (LUN, FIND, VOL, PHNAME, IXMEM, AVERSN, WAIT, IERR)
      POTERR = 31
      IF (IERR.NE.0) GO TO 180
      I = ITRIM (IXMEM)
      IF (BRANCH.EQ.1) WRITE (TITL1,1100) 'Help on', IXMEM(:I), RLSNAM
      IF (BRANCH.EQ.4) WRITE (TITL1,1100) 'Explanation of', IXMEM(:I),
     *   RLSNAM
      IF (BRANCH.EQ.7) WRITE (TITL1,1100) 'About', IXMEM(3:I), RLSNAM
C                                       print once for -3
      IF (DDCRT.LE.-2.5) THEN
         CALL PRTALN (PLUN, PIND, DDCRT, NACROS, TITL1, TITL2, PLINE,
     *      TITL1, PAGE, RSCR, QUIT)
         IF (ERRNUM.NE.0) GO TO 180
         IF (QUIT) GO TO 175
         END IF
C                                        skip Inputs file
 160  CALL ZTREAD (LUN, FIND, PLINE, IERR)
         IF (IERR.EQ.2) GO TO 170
         IF (IERR.NE.0) GO TO 185
         IF (PLINE(1:8).NE.'--------') GO TO 160
C                                        read loop
 165  CALL ZTREAD (LUN, FIND, PLINE, IERR)
         IF (IERR.EQ.2) GO TO 170
         IF (IERR.NE.0) GO TO 185
         IF (PLINE(1:1).EQ.';') GO TO 165
         IF ((BRANCH.NE.1) .OR. (PLINE(1:8).NE.'--------')) THEN
            CALL PRTALN (PLUN, PIND, DDCRT, NACROS, TITL1, TITL2, PLINE,
     *         LINE, PAGE, RSCR, QUIT)
            IF (ERRNUM.NE.0) GO TO 185
            IF (.NOT.QUIT) GO TO 165
            END IF
 170  IF (DDCRT.LE.0.0) THEN
         CALL LPCLOS (PLUN, PIND, LINE, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
      CALL ZTCLOS (LUN, FIND, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C                                       Error closes: quit request
 175  IF (DDCRT.LE.0.0) CALL LPCLOS (PLUN, PIND, LINE, IERR)
      GO TO 999
C                                       Error closes: actual error
 180  IF (DDCRT.LE.0.0) CALL LPCLOS (PLUN, PIND, LINE, IERR)
      GO TO 980
 185  IF (DDCRT.LE.0.0) CALL LPCLOS (PLUN, PIND, LINE, IERR)
      CALL ZTCLOS (LUN, FIND, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       INP <name> (no message file)
C                                       INPUTS <name>
C                                       SHOW  <name>
C                                       OUTPUTS <name>
C                                       QINP
C-----------------------------------------------------------------------
 200  INPRIO = 2
      IF (BRANCH.EQ.2) INPRIO = 1
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000) .OR.
     *   (TSKNAM(1:5).EQ.'AIPSC')) INPRIO = 2
C                                       prime TV parameters
      IF (NGRAPH.LE.0) THEN
         IF (ISBTCH.EQ.32000) THEN
            NGRAY = 16
            NGRAPH = 8
            OFMINP = 16 * 2046
            MAXXTV(1) = 1024
            MAXXTV(2) = 1024
         ELSE
            MSGSUP = 32000
            CALL TVOPEN (SCRTCH, IERR)
            IF (IERR.EQ.0) CALL TVCLOS (SCRTCH, IERR)
            MSGSUP = 0
            END IF
         END IF
C                                        <NAME> in KARBUF -> IXMEM
      IF (IXMEM.EQ.' ') THEN
         CALL ADVERB ('TASK', 'C', 1, 8, IDUM, RDUM, IXMEM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C                                        QINP
      IF (BRANCH.EQ.10) THEN
         DNUMB = LDNUMB
         IXMEM = LDTASK
         INPRIO = LDPRIO
      ELSE IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3)) THEN
         LDNUMB = 0
         DNUMB = 0
         LDPRIO = INPRIO
      ELSE
         DNUMB = 0
         END IF
      QUIT = .FALSE.
C                                        Create name. Open file.
      LUN = INPLUN
      POTERR = 101
      CALL ZPHFIL ('HE', VOL, SEQ, VER, PHNAME, IERR)
C                                       Allow min match
      CALL VERMAT (VOL, PHNAME, IXMEM, LVERSN, I, AVERSN, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTOPEN (LUN, FIND, VOL, PHNAME, IXMEM, AVERSN, WAIT, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZWINC (NCOL)
      IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3)) LDTASK = IXMEM
      LNUMB = 0
C                                       Read 1st record (comment line).
C                                       and comment lines
      POTERR = 59
 205  CALL ZTREAD (LUN, FIND, JBUF, IERROR(1))
      IF (IERROR(1).NE.0) GO TO 970
      IF (JBUF(1:1).EQ.';') GO TO 205
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 970
         END IF
      ISIZE = 0
      LSIZE = 0
      IEOF = 0
      M = 0
      NUM = FLDLEN
C                                       second line: lead header
 210  CALL ZTREAD (LUN, FIND, JBUF, IERROR(1))
      IF (IERROR(1).NE.0) GO TO 970
      IF (JBUF(1:1).EQ.';') GO TO 210
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 970
         END IF
      MSGTXT = JBUF
      IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
C                                       Print INPUTS header.
      WRITE (MSGTXT,1210)
      IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
      WRITE (MSGTXT,1211) (RDASH, I = 1,16)
      IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
      LINE = 3
C                                       Show OPTELL on SHOW
      IF (BRANCH.EQ.5) THEN
         CALL ADVERB ('OPTELL', 'C', 1, 4, IDUM, RDUM, OPTELL)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (MSGTXT,1212) OPTELL
         CALL MSGWRT (INPRIO)
         CALL ADVERB ('NUMTELL', 'I', 1, 1, IDUM, RDUM, OPTELL)
         IF (ERRNUM.NE.0) GO TO 980
         WRITE (MSGTXT,1213) IDUM
         CALL MSGWRT (INPRIO)
         WRITE (MSGTXT,1211) (RDASH, I = 1,16)
         CALL MSGWRT (INPRIO)
         LINE = 6
         END IF
C
C                                        Read loop
 220  CALL ZTREAD (LUN, FIND, JBUF, IERROR)
         IF (JBUF(1:1).EQ.';') GO TO 220
         LNUMB = LNUMB + 1
C                                        end of file
         IF (JBUF(1:8).EQ.'--------') IERROR(1) = 2
         IF (IERROR(1).EQ.2) THEN
            IEOF = 999
C                                        finish any arrays
            M = 1
            IF (LSIZE.NE.0) GO TO 240
            IF (ISIZE.NE.0) GO TO 290
            GO TO 900
            END IF
         IF (IERROR(1).NE.0) GO TO 970
C                                        find first data on card
         I = JTRIM (JBUF)
C                                       Decode adverb usage
         CODE = JBUF(COLSHO:COLSHO)
C                                       go tell get
         IF (CODE.EQ.'&') THEN
C                                       -- tell get
         ELSE IF (CODE.EQ.'%') THEN
            IF ((BRANCH.NE.5) .AND. (BRANCH.NE.9)) GO TO 220
C                                       go ---- get
         ELSE IF (CODE.EQ.'$') THEN
            IF (BRANCH.EQ.5) GO TO 220
C                                       -- ---- get
         ELSE IF (CODE.EQ.'@') THEN
            IF (BRANCH.NE.9) GO TO 220
C                                       -- tell ---
         ELSE IF (CODE.EQ.'?') THEN
            IF (BRANCH.NE.5) GO TO 220
C                                       go ---- ---
         ELSE IF (CODE.EQ.' ') THEN
            IF ((BRANCH.EQ.5) .OR. (BRANCH.EQ.9)) GO TO 220
C                                       go tell --- code '*'
         ELSE IF (CODE.EQ.'*') THEN
            IF (BRANCH.EQ.9) GO TO 220
         ELSE
            MSGTXT = 'WRONG ADVERB CODE IN INPUTS'
            CALL MSGWRT (6)
            GO TO 220
            END IF
         JBUF(COLSHO:COLSHO) = ' '
         KARBUF = JBUF
         KBPTR = NMATCC (1, ' ', KARBUF)
         LXPTR = KBPTR
C                                        simple comment
         IF ((KBPTR.GE.COLLBN) .AND. (LSIZE.EQ.0) .AND. (ISIZE.EQ.0))
     *      THEN
            IF (LINE.GT.ABS(LCRTMX)-4) THEN
               KARSAV = KARBUF
               IF (LNUMB.GT.DNUMB) CALL SCHOLD (QUIT)
               LINE = 0
               IF (QUIT) GO TO 900
               CALL ZWINC (NCOL)
               KARBUF = KARSAV
               IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3) .OR. (BRANCH.EQ.10))
     *            LDNUMB = MAX (LDNUMB, LNUMB)
               END IF
            MSGTXT = JBUF
            IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
            LINE = LINE + 1
            GO TO 220
            END IF
C                                        new adverb or blank line
C                                        and unfinished array
         IF ((ISIZE.NE.0) .AND. ((KBPTR.LT.COLLBN) .OR. (KBPTR.
     *      GT.80))) M = 1
C                                        unfinished array in any case
         IF (LSIZE.NE.0) GO TO 240
         IF (ISIZE.NE.0) GO TO 290
C                                        Process the new line
 230     CALL GETFLD
         IF (ERRNUM.EQ.2) THEN
            MSGTXT = 'MISSING ADVERB: TRY A ''COMPRESS'' OR '
     *         // '''HELP NOADVERB'' FOR INFO'
            CALL MSGWRT (8)
            END IF
         IF (ERRNUM.NE.0) GO TO 970
C                                        character strings
         IF (SYTYPE.NE.7) GO TO 270
C                                       Check character strings
            CALL VERSTR (LOCSYM)
            IF (ERRNUM.NE.0) GO TO 970
            LL = K(LOCSYM+3)
            NDIM = K(LL+1)
            POTERR = 23
            IF (NDIM.LT.1) GO TO 970
            LSIZE = K(LL+3)
            IF (LSIZE.LE.0) GO TO 970
            LWORD = (LSIZE + 3) / 4
            ISIZE = K(LL) / LWORD
            KSIZE = ISIZE
            JSIZE = 0
            DO 235 J = 1,ISIZE
               M = TAG + (J-1) * LWORD
               EQUAL = .TRUE.
               DO 234 I = 1,LWORD
                  EQUAL = (EQUAL) .AND. (CH(M+I-1).EQ.HBLANK)
 234              CONTINUE
               IF (.NOT.EQUAL) JSIZE = J
 235           CONTINUE
            JSIZE = ISIZE - JSIZE
            M = 0
C                                        include unfinished here
 240        MSGTXT = ' '
               IPTR = 12
               IPTRL = 65
               IF (M.NE.1) THEN
                  IF (JBUF(35:).NE.' ') IPTRL = 35
                  MSGTXT = JBUF(:10)
                  MSGTXT(35:) = JBUF(35:)
                  END IF
               JJLIM = (IPTRL - IPTR) / (LSIZE+4)
               IF (JJLIM.GT.0) THEN
                  DO 245 J = 1,JJLIM
                     IF (ISIZE.EQ.0) GO TO 260
                     IF ((ISIZE.EQ.JSIZE) .AND. (JSIZE.GT.JJLIM-J+1))
     *                  GO TO 255
                     MSGTXT(IPTR:IPTR) = ''''
                     IPTR = IPTR + 1
                     CALL H2CHR (LSIZE, 1, CH(TAG), RSCR)
                     LLSIZE = JTRIM (RSCR)
                     LLSIZE = MAX (1, LLSIZE)
                     MSGTXT(IPTR:IPTR+LLSIZE-1) = RSCR(:LLSIZE)
                     IPTR = IPTR + LLSIZE
                     MSGTXT(IPTR:IPTR) = ''''
                     IPTR = IPTR + 3
                     TAG = TAG + LWORD
                     ISIZE = ISIZE - 1
 245                 CONTINUE
C                                       long strings
               ELSE
                     IF (ISIZE.EQ.JSIZE) GO TO 255
                     LSIZE = MIN (LSIZE, 132)
                     MM = 1
                     CALL H2CHR (LSIZE, 1, CH(TAG), TITL1)
                     LLSIZE = JTRIM (TITL1)
                     LLSIZE = MAX (1, LLSIZE)
                     TAG = TAG + LWORD
                     ISIZE = ISIZE - 1
                     MSGTXT(IPTR:IPTR) = ''''
                     IPTR = IPTR + 1
 250                 J = IPTRL - IPTR
                        IF (MM+J-1.GT.LLSIZE) J = LLSIZE - MM + 1
                        MSGTXT(IPTR:IPTR+J-1) = TITL1(MM:MM+J-1)
                        MM = MM + J
                        IF (MM.GT.LLSIZE) THEN
                           IPTR = IPTR + J
                           MSGTXT(IPTR:IPTR) = ''''
                           GO TO 260
                           END IF
C                                       print it out: ?
                        IF (LINE.GT.ABS(LCRTMX)-4) THEN
                           KARSAV = KARBUF
                           MSGSAV = MSGTXT
                           IF (LNUMB.GT.DNUMB) CALL SCHOLD (QUIT)
                           LINE = 0
                           IF (QUIT) GO TO 900
                           CALL ZWINC (NCOL)
                           KARBUF = KARSAV
                           MSGTXT = MSGSAV
                           IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3) .OR.
     *                        (BRANCH.EQ.10)) LDNUMB = MAX (LDNUMB,
     *                        LNUMB)
                           END IF
                        IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
                        LINE = LINE + 1
                        MSGTXT = ' '
                        IPTR = 12
                        GO TO 250
                     END IF
                  GO TO 260
C                                       rest null
 255              ISIZE = 0
                  IF (JSIZE.EQ.KSIZE) MSGTXT(IPTR:IPTR+NCHRS(3)-1) =
     *               FNULL(3)(1:NCHRS(3))
                  IF (JSIZE.LT.KSIZE) MSGTXT(IPTR:IPTR+NCHRS(4)-1) =
     *               FNULL(4)(1:NCHRS(4))
C                                       print it out: ?
 260              IF (LINE.GT.ABS(LCRTMX)-4) THEN
                     KARSAV = KARBUF
                     MSGSAV = MSGTXT
                     IF (LNUMB.GT.DNUMB) CALL SCHOLD (QUIT)
                     LINE = 0
                     IF (QUIT) GO TO 900
                     CALL ZWINC (NCOL)
                     KARBUF = KARSAV
                     MSGTXT = MSGSAV
                     IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3) .OR.
     *                  (BRANCH.EQ.10)) LDNUMB = MAX (LDNUMB, LNUMB)
                     END IF
C                                       actually print it
                  IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
                  LINE = LINE + 1
                  IF (ISIZE.EQ.0) LSIZE = 0
                  IF (M.EQ.0) GO TO 220
C                                        forced finish
                  IF (ISIZE.NE.0) GO TO 240
               IF (IEOF.EQ.999) GO TO 900
               M = 0
               IF (LXPTR.GT.80) GO TO 220
               GO TO 230
C                                        reals
 270        CONTINUE
               POTERR = 8
               IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2)) GO TO 970
               SYSLIM(1) = -1.0
               SYSLIM(2) = -2.0
               DO 275 I = 1,NSYSAD
                  IF (SYSADV(I).EQ.JBUF(1:8)) THEN
                     IF (I.LE.NSYSTA) THEN
                        SYSLIM(2) = NTAPED
                     ELSE IF (I.LE.NSYSTA+NSYSDA) THEN
                        SYSLIM(2) = NVOL
                     ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH) THEN
                        SYSLIM(2) = MAXCHA
                     ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH+NSYSIM) THEN
                        SYSLIM(2) = MAXIMG
                     ELSE
                        SYSLIM(2) = NINTRN + 1 + NBATQS
                        IF (SYSADV(I).EQ.'BATQUE') SYSLIM(2) = NBATQS
                        IF (SYSADV(I).EQ.'TVCHAN') SYSLIM(2) = NGRAY
                        IF (SYSADV(I).EQ.'TV2CHAN') SYSLIM(2) = NGRAY
                        IF (SYSADV(I).EQ.'GRCHAN') SYSLIM(2) = NGRAPH
                        IF (SYSADV(I).EQ.'GR2CHAN') SYSLIM(2) = NGRAPH
                        IF (SYSADV(I).EQ.'TVLEVS') SYSLIM(2) = OFMINP
                        IF (SYSADV(I).EQ.'TVXY') SYSLIM(2) =
     *                     MAX (MAXXTV(1), MAXXTV(2))
                        IF (SYSADV(I).EQ.'TVCORN') SYSLIM(2) =
     *                     MAX (MAXXTV(1), MAXXTV(2))
                        END IF
                     END IF
 275              CONTINUE
C                                        get limits
               ITAG = TAG
               ITYPE = SYTYPE
               ILOC = LOCSYM
               LIM(1) = LMIN
               LIM(2) = LMAX
 280           KBPTR = NMATCC (KBPTR, ' ', KARBUF)
               IF (KBPTR.LT.COLCOM) THEN
                  I = 2
                  IF (KBPTR.LT.COLUBN) I = 1
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 970
C                                        negative
                  IF (TAG.EQ.6) THEN
                     CALL GETFLD
                     IF (ERRNUM.NE.0) GO TO 970
                     LIM(I) = -C(TAG)
C                                        positive
                  ELSE
                     LIM(I) = C(TAG)
                     END IF
                  GO TO 280
                  END IF
C                                        convert #'s to characters
               ISIZE = 1
               IF (ITYPE.EQ.2) ISIZE = K(K(ILOC+3))
               IF (SYSLIM(2).GE.SYSLIM(1)) LIM(2) = SYSLIM(2)
               RMAX = LMIN
               RMIN = LMAX
               KSIZE = ISIZE
               JSIZE = 0
               DO 285 J = 1,ISIZE
                  M = ITAG + J - 1
                  IF (C(M).NE.0.0) JSIZE = J
                  RMAX = MAX (C(M), RMAX)
                  RMIN = MIN (C(M), RMIN)
 285              CONTINUE
               JSIZE = ISIZE - JSIZE
               M = 0
C                                        include unfinished arrays
 290           IPTR = 12
               JJLIM = 4
               MSGTXT = ' '
               IF (M.NE.1) THEN
                  JJLIM = 2
                  MSGTXT(1:10) = JBUF(1:10)
                  MSGTXT(35:) = JBUF(35:)
                  END IF
               DO 295 J = 1,JJLIM
                  IF (ISIZE.EQ.0) GO TO 310
                  IF ((ISIZE.EQ.JSIZE) .AND. (JSIZE.GT.JJLIM-J+1))
     *               GO TO 300
                  IT = FRMT (FLDLEN, DECLEN, C(ITAG), STRING)
                  MSGTXT(IPTR:IPTR+NUM-1) = STRING(1:NUM)
                  IPTR = IPTR + NUM + 3
                  ITAG = ITAG + 1
                  ISIZE = ISIZE - 1
 295              CONTINUE
               GO TO 310
C                                       rest null
 300           ISIZE = 0
               IF (JSIZE.EQ.KSIZE) MSGTXT(IPTR:IPTR+NCHRS(1)-1) =
     *            FNULL(1)(1:NCHRS(1))
               IF (JSIZE.LT.KSIZE) MSGTXT(IPTR:IPTR+NCHRS(2)-1) =
     *            FNULL(2)(1:NCHRS(2))
C                                        output
 310           IF (LINE.GT.ABS(LCRTMX)-4) THEN
                  KARSAV = KARBUF
                  MSGSAV = MSGTXT
                  IF (LNUMB.GT.DNUMB) CALL SCHOLD (QUIT)
                  LINE = 0
                  IF (QUIT) GO TO 900
                  CALL ZWINC (NCOL)
                  KARBUF = KARSAV
                  MSGTXT = MSGSAV
                  IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.3) .OR.
     *               (BRANCH.EQ.10)) LDNUMB = MAX (LDNUMB, LNUMB)
                  END IF
C                                        check limits
               IF (LNUMB.GE.DNUMB) CALL MSGWRT (INPRIO)
               LINE = LINE + 1
               CALL BOUNDS (RMIN, RMAX, LIM, IERR)
               IF (IERR.NE.0) LINE = LINE + 1
               IF (IERR.GT.2) LINE = LINE + 1
               IF (M.EQ.0) GO TO 220
C                                        forced finish of array
               IF (ISIZE.NE.0) GO TO 290
            IF (IEOF.EQ.999) GO TO 900
            M = 0
            IF (LXPTR.GT.80) GO TO 220
            GO TO 230
C-----------------------------------------------------------------------
C                                       APROPOS
C-----------------------------------------------------------------------
 400  CALL H2CHR (STACK(SP-2), 1, C(STACK(SP-1)), RSCR)
      SP = SP - 4
      I = MAXNAM
      CALL FILL (I, 0, LNAMES)
      NUMNAM = 0
      NUMWRD = 0
      TWORD = 'A'
      BWORD = 'Z'
C                                       List of words
      J = JTRIM (RSCR)
      I = 1
      LL = -1
      DO 410 I = 1,J
C                                       blank
         IF ((RSCR(I:I).EQ.' ') .OR. (RSCR(I:I).EQ.',')) THEN
            IF (LL.GT.0) THEN
               NUMWRD = NUMWRD + 1
               WORDS(NUMWRD) = RSCR(LL:I-1)
               AWORD = RSCR(LL:LL)
               IF (AWORD.GT.TWORD) TWORD = AWORD
               IF (AWORD.LT.BWORD) BWORD = AWORD
               END IF
            LL = -1
C                                       a letter
         ELSE
            IF (LL.LT.0) LL = I
            END IF
 410     CONTINUE
      IF (LL.GT.0) THEN
         NUMWRD = NUMWRD + 1
         WORDS(NUMWRD) = RSCR(LL:J)
         AWORD = RSCR(LL:LL)
         IF (AWORD.GT.TWORD) TWORD = AWORD
         IF (AWORD.LT.BWORD) BWORD = AWORD
         END IF
      DO 415 I = 1,NUMWRD
         LWORDS(I) = ITRIM (WORDS(I))
 415     CONTINUE
      IXMEM = 'LSAPROPO'
      LUN = HLPLUN
      CALL ZPHFIL ('HE', VOL, SEQ, VER, PHNAME, IERR)
C                                       Allow min match
      CALL VERMAT (VOL, PHNAME, IXMEM, LVERSN, I, AVERSN, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTOPEN (LUN, FIND, VOL, PHNAME, IXMEM, AVERSN, WAIT, IERR)
      POTERR = 31
      IF (IERR.NE.0) GO TO 980
C                                        skip "Inputs" file
 420  CALL ZTREAD (LUN, FIND, PLINE, IERR)
         IF (IERR.EQ.2) GO TO 495
         IF (IERR.NE.0) GO TO 495
         IF (PLINE(1:8).NE.'--------') GO TO 420
C                                        read loop
 425  CALL ZTREAD (LUN, FIND, PLINE, IERR)
         IF (IERR.EQ.2) GO TO 440
         IF (IERR.NE.0) GO TO 495
         JTR = JTRIM (PLINE)
         IF (PLINE(1:8).EQ.'--------') GO TO 440
         IF (PLINE(1:1).EQ.';') GO TO 425
         IF (PLINE(1:1).LT.BWORD) GO TO 425
         IF (PLINE(1:1).GT.TWORD) GO TO 440
         DO 435 I = 1,NUMWRD
            IF (WORDS(I)(:LWORDS(I)).EQ.PLINE(:LWORDS(I))) THEN
               LL = 2 ** (I - 1)
               DO 430 J = 1,NUMNAM
                  IF (PLINE(17:26).EQ.NAMES(J)) THEN
                     LNAMES(J) = ZOR (LNAMES(J), LL)
                     GO TO 425
                     END IF
 430              CONTINUE
               IF (NUMNAM.LE.MAXNAM) THEN
                  NUMNAM = NUMNAM + 1
                  LNAMES(NUMNAM) = LL
                  NAMES(NUMNAM) = PLINE(17:26)
                  END IF
               GO TO 425
               END IF
 435        CONTINUE
         GO TO 425
C                                       close list file
 440  CALL ZTCLOS (LUN, FIND, IERR)
C                                       compress the list
      IF (NUMWRD.GT.1) THEN
         J = 0
         LL = 2 ** NUMWRD - 1
         DO 445 I = 1,NUMNAM
            IF (LNAMES(I).GE.LL) THEN
               J = J + 1
               NAMES(J) = NAMES(I)
               END IF
 445        CONTINUE
         NUMNAM = J
         END IF
C                                       Got any
      IF (NUMNAM.LE.0) THEN
         MSGTXT = 'No matches found'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Sort
      DO 455 J = 1,NUMNAM
         TNAME = 'ZZZZZZZZZZ'
         DO 450 I = 1,NUMNAM
            IF (NAMES(I).LT.TNAME) THEN
               TNAME = NAMES(I)
               M = I
               END IF
 450        CONTINUE
         SNAMES(J) = NAMES(M)
         NAMES(M) = 'ZZZZZZZZZZ'
 455     CONTINUE
C                                       Display
      POTERR = 0
      DO 470 I = 1,NUMNAM
C                                       should be no duplicates anyway
         IF (I.GT.1) THEN
            IF (SNAMES(I).EQ.SNAMES(I-1)) GO TO 470
            END IF
         CALL ZTOPEN (LUN, FIND, VOL, PHNAME, SNAMES(I), AVERSN, WAIT,
     *      IERR)
         IF (IERR.NE.0) THEN
            POTERR = 31
            GO TO 470
            END IF
         TITL1 = SNAMES(I) // ' ** categories:'
         TITL2 = ' '
         J = 0
 460     CALL ZTREAD (LUN, FIND, PLINE, IERR)
            IF (IERR.EQ.2) GO TO 170
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1445) IERR, 'READ', SNAMES(I)
               CALL MSGWRT (6)
               POTERR = 50
               GO TO 465
               END IF
            IF (PLINE(:2).EQ.';#') THEN
               LL = ITRIM (PLINE)
               CALL CHLTOU (LL, PLINE)
               TITL1(27:) = PLINE(3:)
               J = J + 1
            ELSE IF (PLINE(:2).EQ.';!') THEN
               TITL2(3:) = PLINE(3:)
               J = J + 1
               END IF
            IF (J.LT.2) GO TO 460
 465     CALL ZTCLOS (LUN, FIND, IERR)
         MSGTXT = TITL1(:64)
         CALL MSGWRT (3)
         MSGTXT = TITL2(:64)
         CALL MSGWRT (3)
 470     CONTINUE
      GO TO 980
C                                       Error
 495  CALL ZTCLOS (LUN, FIND, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       XHELP
C-----------------------------------------------------------------------
 500  IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000) .OR.
     *   (TSKNAM(1:5).EQ.'AIPSC')) GO TO 999
      POTERR = 0
      IF (IXMEM.EQ.' ') IXMEM = 'INDEX'
      CALL ZXHELP (IXMEM(1:ITRIM (IXMEM)), IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       Okay close
 900  CALL ZTCLOS (LUN, FIND, IERR)
      GO TO 999
C                                        error return
  970 CALL ZTCLOS (LUN, FIND, IERR)
C
  980 IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A,' ',A,' in AIPS version ',A)
 1135 FORMAT ('NAME NOT UNIQUE: COULD BE ',A)
 1140 FORMAT ('NAME ',A,' INCORRECTLY SPECIFIED')
 1145 FORMAT ('NAME NOT UNIQUE: COULD BE ',A,' = ',A)
 1146 FORMAT ('NAME ',A,' NOT FOUND')
 1210 FORMAT ('Adverbs',5X,'Values',17X,'Comments')
 1211 FORMAT (20A4)
 1212 FORMAT ('OPTELL',6X,'''',A4,'''',17X,'TELL operation type')
 1213 FORMAT ('NUMTELL',4X,I5,19X,'TELL to POPS number')
 1445 FORMAT ('ERROR',I6,1X,A4,'ING HELP FILE FOR ',A)
      END
