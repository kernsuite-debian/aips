      PROGRAM SHOPH
C-----------------------------------------------------------------------
C! Prints the sorted output from SHOPA
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008, 2010, 2012
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
C   Special help-file version of program SHOPB, the Program to make
C   shopping lists from output of SHOPA. Input expected from file
C   "shopping.sort"  which may be generated from the output of SHOPA by
C         %sort +0 -4 -u -f -o shopping.sort SHOPA.OUT
C   Output is to files ZZcategory.TMP.  Then these files are read back
C   and changed into ZZcategory.HLP using pre-existing versions of these
C   files in $HLPFIL.  If the file has changed from the one in $HLPFIL
C   that fact is reported.
C      A logical or enviroment variable named MYDIR must be defined
C   before running SHOPA.  WARNING: THERE SHOULD BE NO *.TMP OR *.HLP
C   FILES IN THE DIRECTORY $MYDIR before SHOPH is run.
C-----------------------------------------------------------------------
      CHARACTER CATE*20, DIRECT*20, NAME*20, TYPE*20, ONELIN*100,
     *   INLINE*200, OUTLIN*200, INFIL*48, OUTFIL*48, CURCAT*20,
     *   LABEL*10, CATLST(100)*20
      INTEGER   IOERR, BUFFER(512), LUNI, LUNO, FINDI, FINDO, JTRIM,
     *   KBP, KBLIM, NMAX, NCHAR, NCAT, NC, LUNH, NCM, ICAT, FINDH,
     *   I2ERR
      LOGICAL   WASDIF, NOCHNG
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNI, LUNO, LUNH /16, 17, 18/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='SHOPH'
      NPOPS = 1
C      MSGKIL = 32000
C      MSGSUP = 32000
      CALL ZDCHIN (.FALSE., BUFFER)
C      MSGKIL = 32000
      DEVTAB(LUNI) = 3
      DEVTAB(LUNO) = 3
      DEVTAB(LUNH) = 3
      CURCAT = 'gibberish'
      NCAT = 0
      NCM = 0
C                                       Open input file
      INFIL = 'MYDIR:shopping.sort'
      CALL ZTXOPN ('READ', LUNI, FINDI, INFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Begin looping over file
 100     CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
         IF ((IOERR.EQ.2) .OR. (INLINE.EQ.' ')) GO TO 200
         IF (IOERR.NE.0) GO TO 900
C                                       Crunch line.
         KBP = 1
         KBLIM = JTRIM (INLINE)
C                                       Category
         NMAX = 20
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, CATE, NCHAR)
         CALL CHLTOU (NCHAR, CATE)
         NC = JTRIM (CATE)
         NC = MIN (NC, 8)
         CATE(NC+1:) = ' '
C                                       Name
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, NAME, NCHAR)
         IF (NAME(:2).EQ.'ZZ') GO TO 100
C                                       Directory
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, DIRECT, NCHAR)
C                                       Type
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, TYPE, NCHAR)
C                                       One liner
         NMAX = 100
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, ONELIN, NCHAR)
C                                       Remove double single quotes
         CALL NODBL (ONELIN)
C                                       New category?
         IF (CATE.NE.CURCAT) THEN
            IF (NCAT.GT.0) CALL ZTXCLS (LUNO, FINDO, IOERR)
            NCAT = NCAT + 1
            CURCAT = CATE
            CATLST(NCAT) = CURCAT
C                                       Open output file
            NC = JTRIM (CURCAT)
            NC = MIN (NC, 8)
            OUTFIL = 'MYDIR:ZZ' // CURCAT(1:NC) // '.TMP'
            CALL ZTXOPN ('QWRT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
            IF (IOERR.NE.0) GO TO 900
            END IF
         LABEL = NAME(:9)
         NC = JTRIM (ONELIN)
         OUTLIN = LABEL // ONELIN(1:NC)
         NC = NC + 10
         NCM = MAX (NCM, NC)
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
         IF (IOERR.NE.0) GO TO 900
C                                       Next entry
         GO TO 100
C                                       Normal end of first pass
 200  CALL ZTXCLS (LUNI, FINDI, IOERR)
      CALL ZTXCLS (LUNO, FINDO, IOERR)
      WRITE (MSGTXT,1200) NCM
      CALL MSGWRT (5)
      MSGTXT = 'Now compare new ZZ help files to $HLPFIL ones'
      CALL MSGWRT (2)
      NOCHNG = .TRUE.
      DO 250 ICAT = 1,NCAT
         CURCAT = CATLST(ICAT)
C                                       Open output file
         NC = JTRIM (CURCAT)
         NC = MIN (NC, 8)
         OUTFIL = 'MYDIR:ZZ' // CURCAT(1:NC) // '.HLP'
         CALL ZTXOPN ('QWRT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
         IF (IOERR.NE.0) GO TO 999
         INFIL = 'MYDIR:ZZ' // CURCAT(1:NC) // '.TMP'
         CALL ZTXOPN ('QRED', LUNI, FINDI, INFIL, .FALSE., IOERR)
         IF (IOERR.NE.0) GO TO 930
         INFIL = 'HLPFIL:ZZ' // CURCAT(1:NC) // '.HLP'
         CALL ZTXOPN ('QRED', LUNH, FINDH, INFIL, .FALSE., IOERR)
         IF ((IOERR.NE.0) .AND. (IOERR.NE.5)) GO TO 925
         WASDIF = .FALSE.
C                                       no $HLPFIL version
         IF (IOERR.EQ.5) THEN
            WASDIF = .TRUE.
            FINDH = -1
            OUTLIN = '; ' // CURCAT
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';-------------------------------------------' //
     *         '--------------------'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';! Information about ' // CURCAT
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';# INFORMATION GENERAL'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';  This software is the subject of a User' //
     *         ' agreement and is'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';  confidential in nature. It shall not be' //
     *         ' sold or otherwise'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';  made available or disclosed to third parties.'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ';-------------------------------------------' //
     *         '--------------------'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = CURCAT(:10) // 'LLLLLLLLLLLLUUUUUUUUUUUU ' //
     *         'CCCCCCCCCCCCCCCCCCCCCCCCCCCCC'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = '--------------------------------------------' //
     *         '--------------------'
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = 'List of verbs, adverbs, tasks in category ' //
     *         CURCAT
            NC = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            OUTLIN = ' '
            NC = 1
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            I2ERR = 0
C                                       Copy $HLPFIL precursor,
C                                       then check for differences
         ELSE
C                                       Copy until blank line
 210        CALL ZTXIO ('READ', LUNH, FINDH, OUTLIN, IOERR)
               IF (IOERR.NE.0) GO TO 920
               NC = JTRIM (OUTLIN)
               NC = MAX (1, NC)
               CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
               IF (IOERR.NE.0) GO TO 925
               IF (OUTLIN.NE.' ') GO TO 210
C                                       check for differences
 220        CALL ZTXIO ('READ', LUNH, FINDH, INLINE, IOERR)
            CALL ZTXIO ('READ', LUNI, FINDI, OUTLIN, I2ERR)
               IF ((IOERR.NE.0) .AND. (IOERR.NE.2)) GO TO 920
               IF ((I2ERR.NE.0) .AND. (I2ERR.NE.2)) GO TO 920
               IF ((IOERR.EQ.0) .AND. (I2ERR.EQ.0)) THEN
                  NC = JTRIM (INLINE)
                  NC = JTRIM (OUTLIN)
                  IF (INLINE.NE.OUTLIN) WASDIF = .TRUE.
                  NC = MAX (1, NC)
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
                  IF (IOERR.NE.0) GO TO 925
                  IF ((OUTLIN.NE.' ') .AND. (.NOT.WASDIF)) GO TO 220
               ELSE IF (I2ERR.NE.2) THEN
                  WASDIF = .TRUE.
                  NC = JTRIM (OUTLIN)
                  NC = MAX (1, NC)
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
                  IF (IOERR.NE.0) GO TO 925
                  END IF
            END IF
C                                       Copy any remainder
         IF (I2ERR.NE.0) GO TO 240
 230     CALL  ZTXIO ('READ', LUNI, FINDI, OUTLIN, I2ERR)
            IF (I2ERR.EQ.2) GO TO 240
            IF (I2ERR.NE.0) GO TO 920
            NC = JTRIM (OUTLIN)
            NC = MAX (1, NC)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:NC), IOERR)
            IF (IOERR.NE.0) GO TO 925
            GO TO 230
C                                       normal close
 240     IF (FINDH.GT.0) CALL ZTXCLS (LUNH, FINDH, IOERR)
         CALL ZTXCLS (LUNI, FINDI, IOERR)
         CALL ZTXCLS (LUNO, FINDO, IOERR)
         IF (.NOT.WASDIF) CATLST(ICAT) = ' '
 250     CONTINUE
C                                       List the changed ones
      DO 260 ICAT = 1,NCAT
         CURCAT = CATLST(ICAT)
         NC = JTRIM (CURCAT)
         IF (CURCAT.NE.' ') THEN
            NOCHNG = .FALSE.
            OUTFIL = 'ZZ' // CURCAT(1:NC) // '.HLP'
c           MSGTXT = 'Need to update help file ' // CURCAT
            MSGTXT = 'echo "update" | chkout $HLPFIL/ZZ' //
     *         CURCAT(1:MIN(8,NC)) // '.HLP'
            CALL MSGWRT (5)
            END IF
 260     CONTINUE
      IF (NOCHNG) THEN
         MSGTXT = 'All ZZ help files up to date'
         CALL MSGWRT (5)
         END IF
      GO TO 999
C                                       Error in part 1 - close files
 900  CALL ZTXCLS (LUNI, FINDI, IOERR)
      CALL ZTXCLS (LUNO, FINDO, IOERR)
      GO TO 999
C                                       part 2 section - done
 920  IF (FINDH.GT.0) CALL ZTXCLS (LUNH, FINDH, IOERR)
 925  CALL ZTXCLS (LUNI, FINDI, IOERR)
 930  CALL ZTXCLS (LUNO, FINDO, IOERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1200 FORMAT ('Maximum line length',I4)
      END
      SUBROUTINE GETSTR (KB, KBPLIM, NMAX, KBP, ISTR, NCHAR)
C-----------------------------------------------------------------------
C   GETSTR obtains a character value from a buffer
C   Inputs:
C      KB      C*80     character buffer
C      KBPLIM  I        size of buffer
C      NMAX    I        max string length in characters
C      KBP     I        start position in KB
C   Outputs:
C      KBP     I        start position in KB next field
C      ISTR    C*(*)    packed string, blank filled
C      NCHAR   I        # characters (0 => no string found)
C-----------------------------------------------------------------------
      CHARACTER KB*(*), ISTR*(*)
      INTEGER   NMAX, KBPLIM, KBP, NCHAR
C
      INTEGER   JB, JJ
      CHARACTER CHLAST*1
C-----------------------------------------------------------------------
      NCHAR = 0
      CHLAST = '?'
      ISTR(1:NMAX) = ' '
      IF (KBP.GT.KBPLIM) GO TO 999
C                                        skip leading blanks
 10   IF (KB(KBP:KBP).EQ.'''') GO TO 15
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
C                                        find end
 15   JB = KBP + 1
 20   KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 25
         IF ((KB(KBP:KBP+1).EQ.''' ') .AND. (CHLAST.NE.'''')) GO TO 25
         CHLAST = KB(KBP:KBP)
         GO TO 20
C                                        got it
 25   NCHAR = KBP - JB
      KBP = KBP + 1
      JJ = MIN (NCHAR, NMAX)
      IF (NCHAR.NE.0) ISTR(1:JJ) = KB(JB:JB+JJ-1)
      IF (NCHAR.GT.NMAX) GO TO 999
C                                        make null string ok
      IF (NCHAR.EQ.0) NCHAR = 1
      GO TO 999
C                                        error: too long
C
 999  RETURN
      END
      SUBROUTINE NODBL (LINE)
C-----------------------------------------------------------------------
C   Routine to replace a pair of single quotes with a single single
C   quote.
C   Input/output:
C      LINE  C*(*)  Text string
C-----------------------------------------------------------------------
      CHARACTER LINE*(*)
C
      INTEGER   LENG, LOOP, POINT1, POINT2
      CHARACTER CHAR1*1, TCHAR*1, CHLAST*1, CHTEMP*100
      DATA TCHAR /''''/
C-----------------------------------------------------------------------
C                                       Initialize
      LENG = LEN (LINE)
      POINT1 = 1
      POINT2 = 1
      CHTEMP = LINE
      CHLAST = '?'
      LINE = ' '
C                                       Remove double quotes
      DO 100 LOOP = 1,LENG
         CHAR1 = CHTEMP(POINT1:POINT1)
         IF ((CHAR1.NE.TCHAR) .OR. (CHAR1.NE.CHLAST)) THEN
            LINE(POINT2:POINT2) = CHAR1
            POINT2 = POINT2 + 1
            END IF
         POINT1 = POINT1 + 1
         CHLAST = CHAR1
 100     CONTINUE
C
 999  RETURN
      END
