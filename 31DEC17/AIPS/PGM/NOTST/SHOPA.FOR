      PROGRAM SHOPA
C-----------------------------------------------------------------------
C! Extracts one liners and catagories from text files
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2012
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
C   Program to extract the shopping list information from text files.
C   Reads SHOPA.LIS for a list of files to process, one name
C   per line.; the input files are assumed to have the name "name.type"
C   where type can be FOR ,C, HLP, MAR or TEX.  The first line of
C   SHOPA.LIS is expected to contain the name of the directory being
C   processed.
C      A logical or enviroment variable named MYDIR must be defined
C   before running SHOPA.
C      The output will be written to a file named SHOPA.OUT and will
C   consist of one line per category for each file.  The output line
C   will be:
C   <category> <name> <directory> <type> <one line description>
C-----------------------------------------------------------------------
      INTEGER MAXTYP
      PARAMETER (MAXTYP = 5)
      CHARACTER INLINE*100, ONELIN*100, OUTLIN*200, INFIL*48, OUTFIL*48,
     *   LFIL*48, SUBNAM*20, NAME*10, TYPE*10, DIRECT*40, CH1LIN*10,
     *   CHCAT*10, TYPES(MAXTYP)*10, TYP1LN(MAXTYP)*10, CATS(50)*20,
     *   TYPCAT(MAXTYP)*10
      INTEGER   LOOP, IOERR, ITYPE, SCRLEN, TYPLEN(MAXTYP), BLNK1,
     *   LEND, NUMCAT, LEN, LCATS, BUFFER(512), LUNI, LUNO, FINDI,
     *   FINDO, LUNL, FINDL, JTRIM, ISL, IST, JT
      LOGICAL   DONE, IS1LIN, ISCAT, GOT1LN, GOTCAT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TYPES  /'FOR','C','MAR','HLP','TEX'/
      DATA TYP1LN /'C!','/*!', ';!',';!', '%!'/
      DATA TYPCAT /'C#','/*#', ';#',';#', '%#'/
      DATA TYPLEN / 2,   3,     2,   2,    2/
      DATA LUNI, LUNO, LUNL /16, 17, 18/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='SHOPA'
      NPOPS = 1
      MSGKIL = 32000
      MSGSUP = 32000
      CALL ZDCHIN (.FALSE., BUFFER)
      MSGKIL = 32000
      DEVTAB(LUNI) = 3
      DEVTAB(LUNO) = 3
      DEVTAB(LUNL) = 3
C                                       Get list of routines
      LFIL = 'MYDIR:SHOPA.LIS'
      CALL ZTXOPN ('READ', LUNL, FINDL, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Get directory name
      CALL ZTXIO ('READ', LUNL, FINDL, DIRECT, IOERR)
      IF (IOERR.NE.0) GO TO 900
      JT = JTRIM (DIRECT)
C                                       Open output file
      OUTFIL = 'MYDIR:SHOPA.OUT'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       Begin looping over files
 100     CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
         IF (IOERR.NE.0) GO TO 900
         JT = JTRIM (INLINE)
         IF (INLINE.EQ.' ') GO TO 900
C                                       Is this a qualified name?
         ISL = INDEX (INLINE, '/')
         IST = ISL
 105     IF (IST.GT.0) THEN
            IST = INDEX (INLINE(ISL+1:), '/')
            ISL = ISL + IST
            GO TO 105
            END IF
         SUBNAM = INLINE(ISL+1:)
C                                       Find first blank or '.'
         BLNK1 = INDEX (SUBNAM,'.')
C                                       Form input names etc
         INFIL = DIRECT(1:JTRIM(DIRECT)) // ':'// SUBNAM
         LEND = BLNK1 + INDEX (SUBNAM(BLNK1+1:), ' ')
         NAME = SUBNAM(1:BLNK1-1)
         TYPE = SUBNAM(BLNK1+1:LEND)
C                                       Must have type
         IF (TYPE.EQ.'  ') THEN
            WRITE (6,1100) SUBNAM
            GO TO 100
            END IF
C                                       Set up for this type.
         ITYPE = 0
         DO 110 LOOP = 1,MAXTYP
            IF (TYPE.EQ.TYPES(LOOP)) ITYPE = LOOP
 110        CONTINUE
         IF (ITYPE.EQ.0) THEN
C                                       Unknown type
            WRITE (6,1110) SUBNAM
            GO TO 100
            END IF
         SCRLEN = TYPLEN(ITYPE)
         CH1LIN = TYP1LN(ITYPE)
         CHCAT = TYPCAT(ITYPE)
C                                       Open input file
         CALL ZTXOPN ('READ', LUNI, FINDI, INFIL, .FALSE., IOERR)
         IF (IOERR.NE.0) GO TO 100
C                                       Digest file:
         DONE = .FALSE.
         GOT1LN = .FALSE.
         GOTCAT = .FALSE.
         NUMCAT = 0
C                                       Read line loop
 200        CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
C                                       End of file?
            IF (IOERR.EQ.2) GO TO 600
C                                       Error?
            IF (IOERR.GT.0) GO TO 590
            JT = JTRIM (INLINE)
C                                       Want this one?
            IS1LIN = INLINE(1:SCRLEN) .EQ. CH1LIN(1:SCRLEN)
            ISCAT = INLINE(1:SCRLEN) .EQ. CHCAT(1:SCRLEN)
C                                       Save one liner
C            IF (IS1LIN) ONELIN = INLINE(SCRLEN+1:)
            IF (IS1LIN) CALL WONLIN (INLINE(SCRLEN+1:), ONELIN)
            GOT1LN = GOT1LN .OR. IS1LIN
C                                       Read categories
            IF (ISCAT) THEN
               LEN = JTRIM (INLINE(SCRLEN+1:))
               LCATS = 20
               CALL RWORDS (INLINE(SCRLEN+1:), LEN, NUMCAT, LCATS, CATS)
               END IF
            GOTCAT = GOTCAT .OR. ISCAT
C                                       Done? - may be several category
C                                       lines.
            DONE = GOT1LN .AND. GOTCAT .AND. (.NOT.ISCAT)
            IF (DONE) GO TO 600
C                                       Next line
            GO TO 200
C                                       Error - close file
 590     WRITE (6,1590) SUBNAM
C                                       End - close file
 600     CALL ZTXCLS (LUNI, FINDI, IOERR)
         IF (IOERR.NE.0) GO TO 900
C                                       Write output
         IF (DONE) THEN
C                                       One line per category
            DO 700 LOOP = 1,NUMCAT
               OUTLIN = '''' // CATS(LOOP)(1:JTRIM(CATS(LOOP))) // ''' '
     *            //
     *            '''' // NAME(1:JTRIM(NAME)) // ''' ' //
     *            '''' // DIRECT(1:JTRIM(DIRECT)) // ''' ' //
     *            '''' // TYPE(1:JTRIM(TYPE)) // ''' ' //
     *            '''' // ONELIN(1:JTRIM(ONELIN)) // ''''
               CALL ZTXIO ('WRIT', LUNO, FINDO,
     *            OUTLIN(1:JTRIM(OUTLIN)), IOERR)
               IF (IOERR.NE.0) GO TO 900
 700           CONTINUE
            END IF
C                                       Next file
         GO TO 100
C                                       Done - close files
 900  CALL ZTXCLS (LUNO, FINDO, IOERR)
      CALL ZTXCLS (LUNL, FINDL, IOERR)
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (A20)
 1100 FORMAT (' FILE TYPE MUST BE SPECIFIED FOR ',A)
 1110 FORMAT (' UNKNOWN FILE TYPE: ',A)
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
      INTEGER   POINT, WEND
C-----------------------------------------------------------------------
      POINT = 1
C                                       Loop finding words
C                                       Find next nonblank or end
 100  IF (POINT.GE.LEN) GO TO 999
      IF (LINE(POINT:POINT).NE.' ') THEN
C                                       Found start of word
         WEND = POINT + INDEX (LINE(POINT:LEN), ' ')
C                                       Last on line?
         IF (WEND.EQ.POINT) WEND = LEN + 1
C                                       Snatch word
         NWORD = NWORD + 1
         WORDS(NWORD)(1:LWORDS) = LINE(POINT:WEND-1)
         POINT = WEND
         GO TO 100
      ELSE
C                                       Blank
         POINT = POINT + 1
         GO TO 100
         END IF
C
 999  RETURN
      END
      SUBROUTINE WONLIN (INLINE, ONELIN)
C-----------------------------------------------------------------------
C   Routine to extract one line information from INLINE and writting it
C   to ONELIN.  Single quotes are doubled to preserve them inside of the
C   character string.
C   Input:
C      INLINE  C*(*)  Input text string
C   Output:
C      ONELINE C*(*)  Output text string
C-----------------------------------------------------------------------
      CHARACTER INLINE*(*), ONELIN*(*)
C
      INTEGER   LENG, LOOP, POINT
      CHARACTER CHAR1*1, TCHAR*1
      DATA TCHAR /''''/
C-----------------------------------------------------------------------
C                                       Initialize
      LENG = LEN (INLINE)
      ONELIN = ' '
      POINT = 1
C                                       Copy doubling single quotes.
      DO 100 LOOP = 1,LENG
         CHAR1 = INLINE(LOOP:LOOP)
         IF (CHAR1.EQ.TCHAR) THEN
            ONELIN(POINT:POINT+1) = TCHAR // TCHAR
            POINT = POINT + 2
         ELSE
            ONELIN(POINT:POINT) = CHAR1
            POINT = POINT + 1
            END IF
 100     CONTINUE
C
 999  RETURN
      END
