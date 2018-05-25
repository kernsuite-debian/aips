      PROGRAM HLPB
C-----------------------------------------------------------------------
C! Modifies APROPOS list, replacing references to a help file
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2006, 2012
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
C   Program to change the APROPOS list information from a help file.
C   Reads the command line argument <name>, then replaces all references
C   to <name> in $UPDATE/LSAPROPO.HLP with the current information.  If
C   $HLPFIL/<name..HLP does not exist, then it only removes the old
C   info.  The output will be written to $UPDATE/LSAPROPO.NEW.
C-----------------------------------------------------------------------
      INTEGER   NOWRDS
      PARAMETER (NOWRDS = 46)
      CHARACTER INLINE*100, OUTLIN*26, INFIL*48, OUTFIL*48, ARGV*12,
     *   LFIL*48, CATS(50)*15, WORDS(50)*15, NOWRD(NOWRDS)*15,
     *   TSTWRD*15, TSTLIN*26
      INTEGER   LOOP, IOERR, NUMWRD, LEN, LCATS, I, J, JTRIM, JT, DONE,
     *   BUFFER(512), LUNI, LUNO, FINDI, FINDO, LUNL, FINDL, IRET
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
      TSKNAM ='HLPB'
      NPOPS = 1
      IRET = 8
      MSGKIL = 32000
      MSGSUP = 32000
      CALL ZDCHIN (.FALSE., BUFFER)
      MSGKIL = 32000
      DEVTAB(LUNI) = 3
      DEVTAB(LUNO) = 3
      DEVTAB(LUNL) = 3
      CALL GETARG (1, ARGV)
      I = JTRIM (ARGV)
      IF (I.LE.0) THEN
         MSGTXT = 'ARGUMENT MUST BE GIVEN'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (ARGV(:2).EQ.'ZZ') THEN
         MSGTXT = 'ZZ LIST FILES IGNORED'
         CALL MSGWRT (8)
         NUMWRD = 0
         GO TO 101
         END IF
C                                       read the help file
      INFIL = 'HLPFIL:' // ARGV(:I) // '.HLP'
      CALL ZTXOPN ('READ', LUNI, FINDI, INFIL, .FALSE., IOERR)
      NUMWRD = 0
      IF (IOERR.NE.0) GO TO 100
      NUMWRD = 1
      CATS(1) = ARGV(:I)
      DONE = 0
C                                       Read line loop
 10   CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
C                                       End of file?
         IF (IOERR.GT.0) GO TO 100
C                                       get words
         JT = JTRIM (INLINE)
         IF ((INLINE(:2).EQ.';!') .OR. (INLINE(:2).EQ.';#')) THEN
            LEN = JT - 2
            LCATS = 15
            CALL RWORDS (INLINE(3:), LEN, NUMWRD, LCATS, CATS)
            DONE = DONE + 1
            IF (DONE.EQ.2) GO TO 100
            END IF
C                                       Next line
         GO TO 10
C                                       Got words - sort them
 100  CALL ZTXCLS (LUNI, FINDI, IOERR)
 101  WORDS(1) = ' '
      IF (NUMWRD.GT.0) THEN
         DO 120 LOOP = 1,NUMWRD
            TSTWRD = 'ZZZZZZZZZZZZZZZ'
            DO 110 I = 1,NUMWRD
               IF (CATS(I).NE.' ') THEN
                  IF (CATS(I).LT.TSTWRD) THEN
                     J = I
                     TSTWRD = CATS(I)
                     END IF
                  END IF
 110           CONTINUE
            WORDS(LOOP) = TSTWRD
            CATS(J) = ' '
 120        CONTINUE
C                                       drop duplicates
         LEN = NUMWRD - 1
         DO 140 LOOP = 1,LEN
            J = LOOP + 1
            DO 130 I = J,NUMWRD
               IF (WORDS(LOOP).EQ.WORDS(I)) WORDS(I) = NOWRD(1)
 130           CONTINUE
 140        CONTINUE
C                                       drop prohibited words
         J = 0
         DO 160 LOOP = 1,NUMWRD
            DO 150 I = 1,NOWRDS
               IF (WORDS(LOOP).EQ.NOWRD(I)) GO TO 160
 150           CONTINUE
            J = J + 1
            CATS(J) = WORDS(LOOP)
 160        CONTINUE
         NUMWRD = J
         END IF
C                                       Open input list
      LFIL = 'UPDATE:LSAPROPO.HLP'
      CALL ZTXOPN ('READ', LUNL, FINDL, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Open output file
      OUTFIL = 'UPDATE:LSAPROPO.OUT'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
      J = 1
      TSTLIN = CATS(J) // ' ' // ARGV(:10)
C                                       look for start of data
 190  CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
         IF (IOERR.NE.0) GO TO 900
         I = JTRIM (INLINE)
         CALL ZTXIO ('WRIT', LUNO, FINDO, INLINE(:I), IOERR)
         IF (IOERR.NE.0) GO TO 900
         IF (INLINE(:8).NE.'--------') GO TO 190
C                                       looping over words in file
 200  CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
         IF (IOERR.NE.0) THEN
            IF (IOERR.EQ.2) IOERR = 0
            GO TO 900
            END IF
C                                       skip old ones
         IF (INLINE(17:26).EQ.ARGV(:10)) GO TO 200
C                                       Do we insert here?
 210     IF ((J.LE.NUMWRD) .AND. (TSTLIN.LT.INLINE)) THEN
            I = JTRIM (TSTLIN)
            CALL ZTXIO ('WRIT', LUNO, FINDO, TSTLIN(:I), IOERR)
            IF (IOERR.NE.0) GO TO 900
            J = J + 1
            TSTLIN = CATS(J) // ' ' // ARGV(:10)
            GO TO 210
            END IF
         OUTLIN = INLINE
         I = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:I), IOERR)
         IF (IOERR.NE.0) GO TO 900
         GO TO 200
C                                       Done - close files
 900  IF (IOERR.EQ.0) IRET = 0
      CALL ZTXCLS (LUNO, FINDO, IOERR)
      CALL ZTXCLS (LUNL, FINDL, IOERR)
C
 999  IF (IRET.GT.0) CALL ZEXIT (IRET)
      STOP
C-----------------------------------------------------------------------
 1000 FORMAT (A20)
 1100 FORMAT (' FILE TYPE HLP MUST BE SPECIFIED FOR ',A)
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
