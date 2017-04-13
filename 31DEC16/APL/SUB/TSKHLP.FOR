      SUBROUTINE TSKHLP (CHOIC, IC, HNAME, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007-2008, 2014
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
C   displays on the user's terminal those rows of the file HLPhname.HLP
C   which correspond to the item selected by CHOIC.  The items are
C   separated in the file by lines with C in column 1 and ----'s in col
C   2-72.  The name of the item is left-justified in the next line of
C   the file and is followed by as many lines of text as are needed to
C   describe the item.  This allows help info to be displayed inside a
C   task selecting only those sections which are needed for the moment
C   and responding to the version string (OLD, NEW TST only) used to
C   start the task.
C   If the name of the item in the HLPhname.HLP file is terminated by
C   "<>" then the item is considered to match if the characters
C   preceding the < match.
C   Inputs:
C      CHOIC    C*(*)   Item name - case-sensitive
C      IC       I       Compare IC characters of CHOIC.  0 => use ITRIM.
C      HNAME    C*(*)   Name to use for help file of name HLPhname.HLP
C                       ' ' => current task name  Uses only 5 chars
C   Output:
C      IERR     I       Error code: 0 => okay, 2 input error, > 0 I/O
C                       -1 CHOIC not found
C-----------------------------------------------------------------------
      CHARACTER CHOIC*(*), HNAME*(*)
      INTEGER   IC, IERR
C
      INTEGER   LUN, FIND, NLINE, JERR, ITRIM, NC, TLUN, TIND, JC,
     *   MNUM, IP, LP, K
      CHARACTER PHNAME*48, HLPNAM*8, VERSON*48, CBUF*72, MNAME*6,
     *   SEP*72, NUMB*8, DBUF*80
      LOGICAL   HDRLST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /11/
C-----------------------------------------------------------------------
C                                       test inputs
      IERR = 2
      NC = IC
      IF (NC.EQ.0) NC = ITRIM (CHOIC)
      IF ((NC.LE.0) .OR. (NC.GT.72)) GO TO 999
      MNAME = HNAME
      IF (MNAME.EQ.' ') MNAME = TSKNAM
      SEP = ' '
      SEP(11:24) = '- - - - - - - '
      SEP(25:38) = '- - - - - - - '
      SEP(39:52) = '- - - - - - - '
      SEP(53:62) = '- - - - - '
C                                       open file
      CALL ZPHFIL ('HE', 1, 0, 0, PHNAME, IERR)
      VERSON = VERNAM
      IF (VERSON.EQ.' ') VERSON = 'TST'
      HLPNAM = 'HLP' // MNAME(:5)
      CALL ZTOPEN (LUN, FIND, 1, PHNAME, HLPNAM, VERSON, .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'OPEN', HLPNAM
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NLINE = 0
      HDRLST = .FALSE.
C                                       read loop: look for section
C                                       demand full match
 10   CALL ZTREAD (LUN, FIND, CBUF, IERR)
         IF (IERR.EQ.2) THEN
            GO TO 20
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ', HLPNAM
            CALL MSGWRT (8)
            GO TO 990
         ELSE IF (CBUF(:8).EQ.'C-------') THEN
            HDRLST = .TRUE.
         ELSE IF (HDRLST) THEN
            JC = ITRIM (CBUF)
            IF (CBUF(JC-2:JC).EQ.'<n>') THEN
               JC = JC - 3
               NUMB = CHOIC(JC+1:)
            ELSE
               NUMB = ' '
               END IF
            JC = MIN (NC, JC)
            IF (CHOIC(:JC).EQ.CBUF(:JC)) GO TO 40
            HDRLST = .FALSE.
            END IF
         GO TO 10
C                                       close and reopen
 20   CALL ZTCLOS (LUN, FIND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, 'CLOSE', HLPNAM
         CALL MSGWRT (8)
         END IF
      CALL ZTOPEN (LUN, FIND, 1, PHNAME, HLPNAM, VERSON, .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'OPEN', HLPNAM
         CALL MSGWRT (8)
         GO TO 999
         END IF
      NLINE = 0
      HDRLST = .FALSE.
C                                       read loop: look for section
 30   CALL ZTREAD (LUN, FIND, CBUF, IERR)
         IF (IERR.EQ.2) THEN
            IERR = -1
            GO TO 990
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ', HLPNAM
            CALL MSGWRT (8)
            GO TO 990
         ELSE IF (CBUF(:8).EQ.'C-------') THEN
            HDRLST = .TRUE.
         ELSE IF (HDRLST) THEN
            JC = ITRIM (CBUF)
            IF (CBUF(JC-2:JC).EQ.'<n>') JC = JC - 3
            JC = MIN (NC, JC)
            IF (CHOIC(:JC).EQ.CBUF(:JC)) GO TO 40
            HDRLST = .FALSE.
            END IF
         GO TO 30
C                                       Found it: find or open TTY
 40   TLUN = 5
      CALL LSERCH ('SRCH', TLUN, TIND, .FALSE., IERR)
      IF (IERR.EQ.1) CALL ZTTOPN (TLUN, TIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) 'OPENING', IERR
         CALL MSGWRT (7)
         GO TO 990
         END IF
C                                       write the line
 50   NC = ITRIM (CBUF)
         NC = MAX (1, NC)
         IF (NUMB.NE.' ') THEN
            MNUM = ITRIM (NUMB)
            IP = 1
            LP = 1
 60         K = INDEX (CBUF(IP:NC), '<n>')
            IF (K.GT.0) THEN
               DBUF(LP:) = CBUF(IP:IP+K-1)
               LP = LP + K - 1
               DBUF(LP:) = NUMB
               LP = LP + MNUM
               IP = IP + K + 2
               GO TO 60
            ELSE IF (LP.GT.1) THEN
               DBUF(LP:) = CBUF(IP:)
               END IF
            IF (LP.GT.1) CBUF = DBUF
            NC = ITRIM (CBUF)
            NC = MAX (1, NC)
            END IF
         CALL ZTTYIO ('WRIT', TLUN, TIND, NC, CBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) 'WRITING TO', IERR
            CALL MSGWRT (7)
            GO TO 990
            END IF
C                                       read next line
         CALL ZTREAD (LUN, FIND, CBUF, IERR)
         IF (IERR.EQ.2) THEN
            IERR = 0
            GO TO 980
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'READ', HLPNAM
            CALL MSGWRT (8)
            GO TO 990
         ELSE IF (CBUF(:8).EQ.'C-------') THEN
            GO TO 980
            END IF
         GO TO 50
C                                       final separator
 980  NC = ITRIM (SEP)
      CALL ZTTYIO ('WRIT', TLUN, TIND, NC, SEP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) 'WRITING TO', IERR
         CALL MSGWRT (7)
         END IF
C
 990  CALL ZTCLOS (LUN, FIND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, 'CLOSE', HLPNAM
         CALL MSGWRT (8)
         END IF
      IF (IERR.EQ.-1) THEN
         MSGTXT = 'NO HELP FOR ' // CHOIC
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (2I2.2)
 1010 FORMAT ('ERROR ',I5,' DOING A ',A,' FOR FILE ',A)
 1020 FORMAT ('ERROR',I5,1X,A,' THE TERMINAL')
      END
