      PROGRAM HLPA
C-----------------------------------------------------------------------
C! Extracts one liners and categories from help files for APROPOS list
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005, 2012, 2015
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
C   Program to extract the APROPOS list information from help files.
C   Reads HLPA.LIS for a list of files to process, one name per line;
C   the input files are assumed to have the name "name.HLP" .
C      A logical or enviroment variable named MYDIR must be defined
C   before running HLPA.
C      The output will be written to a file named HLPA.OUT and will
C   consist of one line per word for each file.  The output line
C   will be: <word> <name> as CHAR*15, 1x, CHAR*8.
C-----------------------------------------------------------------------
      INTEGER   NOWRDS
      PARAMETER (NOWRDS = 46)
      CHARACTER INLINE*100, OUTLIN*26, INFIL*48, OUTFIL*48, LFIL*48,
     *   SUBNAM*20, NAME*10, CATS(50)*15, NOWRD(NOWRDS)*15, CNAME*15
      INTEGER   LOOP, IOERR, BLNK1, LEND, NUMWRD, LEN, LCATS, I, J, IN,
     *   BUFFER(512), LUNI, LUNO, FINDI, FINDO, LUNL, FINDL, JTRIM, JT,
     *   DONE
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNI, LUNO, LUNL /16, 17, 18/
      DATA NOWRD /'ABOUT', 'AFTER', 'ALL', 'ALONG', 'ALSO', 'AN', 'AND',
     *   'ANOTHER', 'ANY', 'ARE', 'AS', 'AT', 'BASED', 'BE', 'BEFORE',
     *   'BETWEEN', 'BY', 'CAN', 'DOES', 'DURING', 'FOR', 'FROM',
     *   'GIVES', 'HAS', 'IN', 'INTO', 'IS', 'NOT', 'OF', 'ON', 'OR',
     *   'OTHER', 'SET', 'SETS', 'THE', 'TO', 'WHICH', 'WITH', 'VERB',
     *   'TASK', 'ADVERB', 'PSEUDOVERB', 'VERBS', 'TASKS', 'ADVERBS',
     *   'PSEUDOVERBS'/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='HLPA'
      NPOPS = 1
      MSGKIL = 32000
      MSGSUP = 32000
      CALL ZDCHIN (.FALSE., BUFFER)
      MSGKIL = 32000
      DEVTAB(LUNI) = 3
      DEVTAB(LUNO) = 3
      DEVTAB(LUNL) = 3
C                                       Get list of routines
      LFIL = 'MYDIR:HLPA.LIST'
      CALL ZTXOPN ('READ', LUNL, FINDL, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Open output file
      OUTFIL = 'MYDIR:HLPA.OUT'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       Begin looping over files
 100     CALL ZTXIO ('READ', LUNL, FINDL, SUBNAM, IOERR)
         IF (IOERR.NE.0) GO TO 900
         JT = JTRIM (SUBNAM)
         IF (SUBNAM.EQ.' ') GO TO 900
C                                       Find first blank or '.'
         BLNK1 = INDEX (SUBNAM,'.')
         IF (BLNK1.EQ.0) BLNK1 = INDEX (SUBNAM, ' ')
C                                       Form input names etc
         NAME = SUBNAM(1:BLNK1-1)
         IF (NAME(:2).EQ.'ZZ') GO TO 100
         IN = JTRIM (NAME)
         INFIL =  'HLPFIL:'// NAME(:IN) // '.HLP'
C                                       Open input file
         CALL ZTXOPN ('QRED', LUNI, FINDI, INFIL, .FALSE., IOERR)
         IF (IOERR.NE.0) GO TO 100
C                                       Digest file:
         DONE = 0
         NUMWRD = 0
C                                       Read line loop
 120        CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
C                                       End of file?
            IF (IOERR.EQ.2) GO TO 200
C                                       Error?
            IF (IOERR.GT.0) GO TO 190
            JT = JTRIM (INLINE)
C                                       Save one liner
            IF ((INLINE(:2).EQ.';!') .OR. (INLINE(:2).EQ.';#')) THEN
C                                       get words
               LEN = JT - 2
               LCATS = 15
               CALL RWORDS (INLINE(3:), LEN, NUMWRD, LCATS, CATS)
               DONE = DONE + 1
               IF (DONE.EQ.2) GO TO 200
               END IF
C                                       Next line
            GO TO 120
C                                       Error - close file
 190     WRITE (6,1590) SUBNAM
C                                       End - close file
 200     CALL ZTXCLS (LUNI, FINDI, IOERR)
         IF (IOERR.NE.0) GO TO 900
C                                       Write output
C                                       Drop duplicates
         LEND = NUMWRD - 1
         DO 220 LOOP = 1,LEND
            J = LOOP + 1
            DO 210 I = J,NUMWRD
               IF (CATS(LOOP).EQ.CATS(I)) CATS(I) = NOWRD(1)
 210           CONTINUE
 220        CONTINUE
C                                       task/verb name itself
         IN = JTRIM (NAME) + 16
         CNAME = NAME
         OUTLIN = CNAME // ' ' // NAME
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IN), IOERR)
         IF (IOERR.NE.0) GO TO 900
C                                       One line per category
         DO 240 LOOP = 1,NUMWRD
            DO 230 I = 1,NOWRDS
               IF (CATS(LOOP).EQ.NOWRD(I)) GO TO 240
 230           CONTINUE
            OUTLIN = CATS(LOOP) // ' ' // NAME
            IN = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:IN), IOERR)
            IF (IOERR.NE.0) GO TO 900
 240        CONTINUE
C                                       Next file
         GO TO 100
C                                       Done - close files
 900  CALL ZTXCLS (LUNO, FINDO, IOERR)
      CALL ZTXCLS (LUNL, FINDL, IOERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (A20)
 1590 FORMAT (' ERROR READING FILE ',A)
      END
      SUBROUTINE RWORDS (LINE, LEN, NWORD, LWORDS, WORDS)
C-----------------------------------------------------------------------
C   Routine to get a list of words from a string:
C   Inputs:
C      LINE   C*(*)      Line to be scanned
C      LEN    I          Length of LINE to be searched.
C   Inputs/outputs:
C      NWORD  I          Number of values in WORDS
C      LWORD  I          Max number of characters in WORDS
C      WORDS  C(*)*LWORD Array or words
C-----------------------------------------------------------------------
      INTEGER   LEN, NWORD, LWORDS
      CHARACTER LINE*(*), WORDS(*)*(*)
C
      INTEGER   NIGNOR
      PARAMETER (NIGNOR = 22)
      INTEGER   POINT, WEND, I
      CHARACTER TCHAR, IGNORE(NIGNOR)*1
      DATA IGNORE /'.', ',', '''', '"', '?', '(', ')', '[', ']', '{',
     *   '}', '!', '&', '/', '|', '\\', ':', '-', '>', '<', '=', ';'/
C-----------------------------------------------------------------------
C                                       go to upper case
      CALL CHLTOU (LEN, LINE)
C                                       remove punctuation
      DO 20 POINT = 1,LEN
         TCHAR = LINE(POINT:POINT)
         DO 10 I = 1,NIGNOR
            IF (TCHAR.EQ.IGNORE(I)) THEN
               LINE(POINT:POINT) = ' '
               GO TO 20
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Loop finding words
      POINT = 1
C                                       Find next nonblank or end
 100  IF (POINT.LT.LEN) THEN
         IF (LINE(POINT:POINT).NE.' ') THEN
C                                       Found start of word
            WEND = POINT + INDEX (LINE(POINT:LEN), ' ') - 1
C                                       Last on line?
            IF (WEND.LE.POINT) WEND = LEN + 1
C                                       Snatch words > 1 char
            IF (WEND-1.GT.POINT) THEN
               NWORD = NWORD + 1
               WORDS(NWORD)(1:LWORDS) = LINE(POINT:WEND-1)
               END IF
            POINT = WEND
C                                       Blank
         ELSE
            POINT = POINT + 1
            END IF
         GO TO 100
         END IF
C
 999  RETURN
      END
