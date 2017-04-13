      PROGRAM SHOPB
C-----------------------------------------------------------------------
C! Prints the sorted output from SHOPA
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
C   Program to make shopping lists from output of SHOPA.
C   Input expected from file "shopping.sort"  which may be generated
C   from the output of SHOPA by:
C   %sort +0 -4 -u -f -o shopping.sort SHOPA.OUT
C   Output is to file shopping.list
C      A logical or enviroment variable named MYDIR must be defined
C   before running SHOPA.
C-----------------------------------------------------------------------
      CHARACTER CATE*20, DIRECT*20, NAME*20, TYPE*20, ONELIN*100,
     *   INLINE*200, OUTLIN*200, INFIL*48, OUTFIL*48, CURCAT*20,
     *   LABEL*20
      INTEGER   IOERR, BUFFER(512), LUNI, LUNO, FINDI, FINDO, JTRIM,
     *   KBP, KBLIM, NMAX, NCHAR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNI, LUNO /16, 17/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='SHOPB'
      NPOPS = 1
      MSGKIL = 32000
      MSGSUP = 32000
      CALL ZDCHIN (.FALSE., BUFFER)
      MSGKIL = 32000
      DEVTAB(LUNI) = 3
      DEVTAB(LUNO) = 3
      CURCAT = 'gibberish'
C                                       Open input file
      INFIL = 'MYDIR:shopping.sort'
      CALL ZTXOPN ('READ', LUNI, FINDI, INFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Open output file
      OUTFIL = 'MYDIR:shopping.list'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       Begin looping over file
 100     CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
         IF (IOERR.NE.0) GO TO 900
         KBLIM = JTRIM (INLINE)
         IF (INLINE.EQ.' ') GO TO 900
C                                       Crunch line.
         KBP = 1
C                                       Category
         NMAX = 20
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, CATE, NCHAR)
         CALL CHLTOU (NCHAR, CATE)
C                                       Name
         CALL GETSTR (INLINE, KBLIM, NMAX, KBP, NAME, NCHAR)
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
            OUTLIN = '     '
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:1), IOERR)
            IF (IOERR.NE.0) GO TO 900
            OUTLIN = ' *** New category = ' // CATE
            CURCAT = CATE
            CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:JTRIM(OUTLIN)),
     *         IOERR)
            IF (IOERR.NE.0) GO TO 900
            END IF
         LABEL = DIRECT(1:JTRIM(DIRECT)) // ':' //
     *         NAME(1:JTRIM(NAME)) // '.' // TYPE(1:JTRIM(TYPE))
         OUTLIN = LABEL // '  ' // ONELIN(1:JTRIM(ONELIN))
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:JTRIM(OUTLIN)),
     *      IOERR)
C         IF (IOERR.NE.0) GO TO 900
C         OUTLIN = '     ' // ONELIN(1:JTRIM(ONELIN))
C         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(1:JTRIM(OUTLIN)),
C     *      IOERR)
C         IF (IOERR.NE.0) GO TO 900
C                                       Next entry
         GO TO 100
C                                       Done - close files
 900  CALL ZTXCLS (LUNI, FINDI, IOERR)
      CALL ZTXCLS (LUNO, FINDO, IOERR)
C
 999  STOP
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
