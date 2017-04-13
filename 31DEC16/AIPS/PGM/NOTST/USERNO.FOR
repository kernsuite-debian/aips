      PROGRAM USERNO
C-----------------------------------------------------------------------
C! Converts userno.lis, userno.srt into pretty list for users
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Program to read in the sorted lists of users and their numbers
C      A logical or enviroment variable named MYDIR must be defined
C   before running USERNO.
C-----------------------------------------------------------------------
      INTEGER   NEQ
      PARAMETER (NEQ = 200)
C
      INTEGER NGNU
      PARAMETER (NGNU=23)
C
      CHARACTER INLINE*132, OUTLIN*132, OUTFIL*48, LFIL*48, ATIME*8,
     *   ADATE*12, UNAME*30, CUSER*5, GPLBRK(2)*35, GPLDAT*21,
     *   GPLTXT(2,NGNU)*35, GPLTX1(2,10)*35, GPLTX2(2,10)*35,
     *   GPLTX3(2,NGNU-20)*35
      INTEGER   IOERR, IDATE(3), ITIME(3), JJ, JTRIM, ILINE, IUSER,
     *   LUNO, FINDO, LUNI, FINDI, LUNT, FINDT, KBP, I
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (GPLTXT(1,1), GPLTX1(1,1))
      EQUIVALENCE (GPLTXT(1,11), GPLTX2(1,1))
      EQUIVALENCE (GPLTXT(1,21), GPLTX3(1,1))
      DATA LUNO, LUNI, LUNT /16, 17, 18/
      DATA GPLDAT /'  Copyright (C) 1995-'/
      DATA GPLBRK /'-----------------------------------',
     *             '-----------------------------------'/
      DATA GPLTX1 /'  Associated Universities, Inc. Was',
     *             'hington DC, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is free software; yo',
     *             'u can redistribute it and/or       ',
     *             '  modify it under the terms of the ',
     *             'GNU General Public License as      ',
     *             '  published by the Free Software Fo',
     *             'undation; either version 2 of      ',
     *             '  the License, or (at your option) ',
     *             'any later version.                 ',
     *             '                                   ',
     *             '                                   ',
     *             '  This program is distributed in th',
     *             'e hope that it will be useful,     ',
     *             '  but WITHOUT ANY WARRANTY; without',
     *             ' even the implied warranty of      ',
     *             '  MERCHANTABILITY or FITNESS FOR A ',
     *             'PARTICULAR PURPOSE.  See the       '/
      DATA GPLTX2 /'  GNU General Public License for mo',
     *             're details.                        ',
     *             '                                   ',
     *             '                                   ',
     *             '  You should have received a copy o',
     *             'f the GNU General Public           ',
     *             '  License along with this program; ',
     *             'if not, write to the Free          ',
     *             '  Software Foundation, Inc., 675 Ma',
     *             'ssachusetts Ave, Cambridge,        ',
     *             '  MA 02139, USA.                   ',
     *             '                                   ',
     *             '                                   ',
     *             '                                   ',
     *             '  Correspondence concerning AIPS sh',
     *             'ould be addressed as follows:      ',
     *             '         Internet email: aipsmail@n',
     *             'rao.edu.                           ',
     *             '         Postal address: AIPS Proje',
     *             'ct Office                          '/
      DATA GPLTX3 /'                         National R',
     *             'adio Astronomy Observatory         ',
     *             '                         520 Edgemo',
     *             'nt Road                            ',
     *             '                         Charlottes',
     *             'ville, VA 22903-2475 USA           '/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      TSKNAM ='USERNO'
      NPOPS = 1
      CALL ZDCHIN (.TRUE.)
      DEVTAB(LUNO) = 3
      DEVTAB(LUNI) = 3
      DEVTAB(LUNT) = 3
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
C                                       Get list by names
      LFIL = 'MYDIR:userno.srt'
      CALL ZTXOPN ('READ', LUNI, FINDI, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       get list by numbers
      LFIL = 'MYDIR:userno.lis'
      CALL ZTXOPN ('READ', LUNT, FINDT, LFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 999
C                                       Open output file
      OUTFIL = 'MYDIR:USERNO.LIS'
      CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       GPL stuff
      OUTLIN = ';' // GPLBRK(1) // GPLBRK(2)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      IF (IDATE(1).LT.200) IDATE(1) = IDATE(1) + 1900
      WRITE (OUTLIN,1000) GPLDAT, IDATE(1)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      DO 10 I = 1,NGNU
         OUTLIN = ';' // GPLTXT(1,I) // GPLTXT(2,I)
         JJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
         IF (IOERR.NE.0) GO TO 900
 10      CONTINUE
      OUTLIN = ';' // GPLBRK(1) // GPLBRK(2)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       header stuff: blank
      OUTLIN = ' '
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       header stuff: title
      OUTLIN = ' '
      OUTLIN(21:) = ':-OFFICIAL NRAO AIPS USER Numbers List-:'
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      OUTLIN = ' '
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       header stuff: date
      CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
      WRITE (OUTLIN,1010) ADATE, ATIME
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      OUTLIN = ' '
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       header stuff: col titles
      WRITE (OUTLIN,1011)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      WRITE (OUTLIN,1012)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
C                                       header stuff: divider
      WRITE (OUTLIN,1013)
      JJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:JJ), IOERR)
      IF (IOERR.NE.0) GO TO 900
      ILINE = 0
C                                       read loop
 20   ILINE = ILINE + 1
         CALL ZTXIO ('READ', LUNT, FINDT, INLINE, IOERR)
         IF (IOERR.EQ.2) GO TO 990
         IF (IOERR.GT.0) GO TO 900
C                                       parse lseft hand side
         JJ = JTRIM (INLINE)
         IF (JJ.LE.0) THEN
            WRITE (MSGTXT,1020) 'NULL LINE FOUND', ILINE, '.lis'
            GO TO 980
            END IF
         KBP = 1
         CALL GETNUM (INLINE, JJ, KBP, X)
         IF (X.EQ.DBLANK) THEN
            WRITE (MSGTXT,1020) 'BAD VALUE FOUND', ILINE, '.lis'
            GO TO 980
            END IF
         IUSER = X + 0.1D0
         UNAME = INLINE(KBP:)
         CALL CHTRIM (UNAME, 30, UNAME, JJ)
         WRITE (CUSER,1021) IUSER
         OUTLIN = CUSER
         OUTLIN(33-JJ:) = UNAME(:JJ)
         CALL ZEHEX (IUSER, 3, OUTLIN(35:37))
         OUTLIN(40:40) = '|'
C                                       alphabetical names
         CALL ZTXIO ('READ', LUNI, FINDI, INLINE, IOERR)
         IF (IOERR.EQ.2) THEN
            MSGTXT = 'SORTED LIST ENDS BEFORE UNSORTED ONE!'
            GO TO 980
            END IF
         IF (IOERR.GT.0) GO TO 900
C                                       parse right hand side
         JJ = JTRIM (INLINE)
         IF (JJ.LE.0) THEN
            WRITE (MSGTXT,1020) 'NULL LINE FOUND', ILINE, '.srt'
            GO TO 980
            END IF
         KBP = 1
         CALL GETNUM (INLINE, JJ, KBP, X)
         IF (X.EQ.DBLANK) THEN
            WRITE (MSGTXT,1020) 'BAD VALUE FOUND', ILINE, '.srt'
            GO TO 980
            END IF
         IUSER = X + 0.1D0
         UNAME = INLINE(KBP:)
         CALL CHTRIM (UNAME, 30, UNAME, JJ)
         OUTLIN(43:) = UNAME
         WRITE (CUSER,1021) IUSER
         OUTLIN(68:) = CUSER
         CALL ZEHEX (IUSER, 3, OUTLIN(77:79))
C                                       write it
         CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:79), IOERR)
         IF (IOERR.NE.0) GO TO 900
         GO TO 20
C                                       Done - close files
 900  IF (IOERR.NE.0) THEN
         WRITE (MSGTXT,1900) IOERR
         CALL MSGWRT (7)
         END IF
      GO TO 990
 980  CALL MSGWRT (8)
 990  CALL ZTXCLS (LUNO, FINDO, IOERR)
      CALL ZTXCLS (LUNI, FINDI, IOERR)
      CALL ZTXCLS (LUNT, FINDT, IOERR)
      WRITE (MSGTXT,1990) ILINE-1
      CALL MSGWRT (3)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT (';',A,I4)
 1010 FORMAT (30X,A,A)
 1011 FORMAT (' AIPS',29X,'EHEX |',28X,'AIPS   EHEX')
 1012 FORMAT (' No.',19X,'User name  ID   |  User name',18X,'No.',5X,
     *   'ID')
 1013 FORMAT (80('='))
 1020 FORMAT (A,' ON LINE',I6,' FILE ',A)
 1021 FORMAT (I5)
 1900 FORMAT ('ERROR',I7,' DOING READS OR WRITES')
 1990 FORMAT ('Wrote',I6,' user numbers')
      END
