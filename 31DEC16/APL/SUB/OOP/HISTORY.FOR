C   History Utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran history utility routines
C# History Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2008
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
C   This module contains routines which can be used to manipulate
C   histories associated with objects that are disk resident.
C   Available functions are:
C
C   OHCOPY (in, out, iret)
C      Copies the history from object in the object out.  iret=0
C      indicates success. - also KEYCOP
C   OHWRIT (entry, out, iret)
C      Writes up to 66 characters from character string entry to the
C      history associated with object out.  Task name is prepended.
C   OHLIST (in, list, nlist, out, iret)
C      Writes the names and values of the members of object in specified
C      by the first nlist elements in the character string array list to
C      the history associated with object out.  iret=0 indicates
C      success.
C   OHTIME (out, iret)
C      Adds task name and time and date stamp to history associated with
C      object out.  iret=0 indicates success.  Assures that a history
C      file exists.
C-----------------------------------------------------------------------
      SUBROUTINE OHCOPY (IN, OUT, IRET)
C-----------------------------------------------------------------------
C   Copies the history from object in the object out.
C   If the underlying files are the same then no copy is made.
C   Inputs:
C      IN     C*?  Input object name.
C      OUT    C*?  Output object name.
C   Output:
C      IRET   I    Return code, 0=OK else an error occurred.
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IRET
C
      INTEGER   DISKI, DISKO, CNOI, CNOO, LUNI, LUNO, BUFNOI, BUFNOO,
     *   CAT(256), IERR, MSGSAV
      LOGICAL   NOCOPY
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Open objects and get info.
      CALL OBOPEN (IN, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Which buffer
      CALL OBINFO (IN, BUFNOI, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DISK, CNO
      CALL OBDSKC (IN, DISKI, CNOI, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       LUN
      CALL OBLUN (LUNI, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Output
      CALL OBOPEN (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Which buffer
      CALL OBINFO (OUT, BUFNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DISK, CNO
      CALL OBDSKC (OUT, DISKO, CNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       LUN
      CALL OBLUN (LUNO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Sync header
      CALL OBCSYN (OUT, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (OUT, CAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Initialize history routines
      CALL HIINIT (3)
C                                       Don't do copy it the two have
C                                       the same underlying files.
      NOCOPY = (DISKI.EQ.DISKO) .AND. (CNOI.EQ.CNOO)
C                                       Don't copy if output file exists
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL HIOPEN (LUNO, DISKO, CNOO, OBUFFR(1,BUFNOO), IERR)
      NOCOPY = NOCOPY .OR. (IERR.EQ.0)
      IF (IERR.EQ.0) CALL HICLOS (LUNO, .FALSE., OBUFFR(1,BUFNOO), IERR)
      MSGSUP = MSGSAV
C                                       Don't copy if input file doesn't
C                                       exist
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL HIOPEN (LUNI, DISKI, CNOI, OBUFFR(1,BUFNOI), IERR)
      NOCOPY = NOCOPY .OR. (IERR.NE.0)
      IF (IERR.EQ.0) CALL HICLOS (LUNI, .FALSE., OBUFFR(1,BUFNOI), IERR)
      MSGSUP = MSGSAV
C                                       Copy history
      IF (.NOT.NOCOPY) THEN
C                                       copy keywords too
         CALL KEYPCP (DISKI, CNOI, DISKO, CNOO, 0, ' ', IRET)
         IRET = 0
C                                       now copy HI
         CALL HISCOP (LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, CAT,
     *      OBUFFR(1,BUFNOI), OBUFFR(1,BUFNOO), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Close
         CALL HICLOS (LUNO, .TRUE., OBUFFR(1,BUFNOO), IRET)
         IF (IERR.NE.0) IRET = IERR
C                                       Update Catalog header
         CALL OBHPUT (OUT, CAT, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Free LUNs
      CALL OBLUFR (LUNI)
      CALL OBLUFR (LUNO)
C                                       Close objects
      CALL OBCLOS (IN, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL OBCLOS (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Put time label if no copy was
C                                       made.
      IF (NOCOPY) THEN
         CALL OHTIME (OUT, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR COPYING HISTORY'
      CALL  MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE OHWRIT (ENTRY, OUT, IRET)
C-----------------------------------------------------------------------
C   Writes up to 65-72 characters from character string entry to the
C   history associated ith object out.  Task name is prepended if it is
C   not at the start of ENTRY.
C   Inputs:
C      ENTRY  C*?   Text of history entry; comments should begin with a
C                   "/" .  Up to 65-72 characters used.
C      OUT    C*?   Output object name.
C   Output:
C      IRET   I     Return code, 0=OK else an error occurred.
C-----------------------------------------------------------------------
      CHARACTER ENTRY*(*), OUT*(*)
      INTEGER   IRET
C
      INTEGER   DISKO, CNOO, LUNO, BUFNOO, IERR
      CHARACTER HILINE*72
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Output
      CALL OBOPEN (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Which buffer
      CALL OBINFO (OUT, BUFNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DISK, CNO
      CALL OBDSKC (OUT, DISKO, CNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       LUN
      CALL OBLUN (LUNO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Initialize history routines
      CALL HIINIT (3)
C                                       Copy history
      CALL HIOPEN (LUNO, DISKO, CNOO, OBUFFR(1,BUFNOO), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Add entry
      IF (TSKNAM.EQ.ENTRY(:6)) THEN
         HILINE = ENTRY
      ELSE IF (TSKNAM(6:6).EQ.' ') THEN
         HILINE = TSKNAM // ENTRY
      ELSE
         HILINE = TSKNAM // ' ' // ENTRY
         END IF
      CALL HIADD (LUNO, HILINE, OBUFFR(1,BUFNOO), IERR)
C                                       Close
      CALL HICLOS (LUNO, .TRUE., OBUFFR(1,BUFNOO), IRET)
      IF (IERR.NE.0) IRET = IERR
C                                       Free LUN
      CALL OBLUFR (LUNO)
C                                       Close objects
      CALL OBCLOS (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL  MSGWRT (6)
C
 999  RETURN
      END
      SUBROUTINE OHLIST (IN, LIST, NLIST, OUT, IRET)
C-----------------------------------------------------------------------
C   Writes the names and values of the members of object in specified
C   by the first nlist elements in the character string array list to
C   the history associated with object out.
C   Inputs:
C      IN     C*?     Input object name.
C      LIST   C(?)*?  List of members of input object IN to copy to
C                     history of object OUT.
C      NLIST  I       Number of elements in LIST
C      OUT    C*?     Output object name.
C   Output:
C      IRET   I       Return code, 0=OK else an error occurred.
C-----------------------------------------------------------------------
      INTEGER   NLIST, IRET
      CHARACTER IN*(*), LIST(NLIST)*(*), OUT*(*)
C
      INTEGER   DISKO, CNOO, LUNO, BUFNOO, TYPE, DIM(7), I, ND1, ND2,
     *   ID1, ID2, IVAL(500), L, LC, INDEX, IC1, IC2, IERR, ITRIM, ILVAL
      REAL      RVAL(500)
      LOGICAL   BLNKED, LVAL(500)
      DOUBLE PRECISION DVAL(250)
      CHARACTER HILINE*72, LINE*120, CVAL*1000, CS*72
      EQUIVALENCE (IVAL, LVAL, RVAL, DVAL)
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Output
      CALL OBOPEN (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Which buffer
      CALL OBINFO (OUT, BUFNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DISK, CNO
      CALL OBDSKC (OUT, DISKO, CNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       LUN
      CALL OBLUN (LUNO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Initialize history routines
      CALL HIINIT (3)
C                                       Copy history
      CALL HIOPEN (LUNO, DISKO, CNOO, OBUFFR(1,BUFNOO), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Add entries
      DO 100 I = 1,NLIST
C                                       Fetch entry
         CALL OGET (IN, LIST(I), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Member name length
         L = ITRIM (LIST(I))
C                                       Get dimensionality
         ND1 = MAX (1, DIM(1))
         ND2 = MAX (1, DIM(2))
C                                       Characters are slightly
C                                       different.
         IF (TYPE.EQ.OOACAR) THEN
            ND1 = MAX (1, DIM(2))
            ND2 = MAX (1, DIM(3))
            END IF
         DO 50 ID2 = 1,ND2
            DO 40 ID1 = 1,ND1
               LINE = ' '
               INDEX = ID1 + (ID2-1) * ND1
C                                       Process by data type
C                                       Double precision
               IF (TYPE.EQ.OOADP) THEN
C                                       Don't write zeros.
                  BLNKED = ABS (DVAL(INDEX)) .LT. 1.0E-30
C                                       Get proper dimensionality
                  IF (ND2.GT.1) THEN
                     WRITE (LINE,1000) LIST(I)(1:L), ID1, ID2,
     *                  DVAL(INDEX)
                  ELSE IF (ND1.GT.1) THEN
                     WRITE (LINE,1001) LIST(I)(1:L), ID1, DVAL(INDEX)
                  ELSE
                     WRITE (LINE,1002) LIST(I)(1:L), DVAL(INDEX)
                     END IF
C                                       Real
               ELSE IF (TYPE.EQ.OOARE) THEN
C                                       Don't write zeros.
                  BLNKED = ABS (RVAL(INDEX)) .LT. 1.0E-30
C                                       Get proper dimensionality
                  IF (ND2.GT.1) THEN
                     WRITE (LINE,1003) LIST(I)(1:L), ID1, ID2,
     *                  RVAL(INDEX)
                  ELSE IF (ND1.GT.1) THEN
                     WRITE (LINE,1004) LIST(I)(1:L), ID1, RVAL(INDEX)
                  ELSE
                     WRITE (LINE,1005) LIST(I)(1:L), RVAL(INDEX)
                     END IF
C                                       Character
               ELSE IF (TYPE.EQ.OOACAR) THEN
                  IC1 = 1 + (INDEX-1) * DIM(1)
                  IC2 = IC1 + DIM(1) - 1
                  CS = CVAL(IC1:IC2)
C                                       Don't write blanks
                  BLNKED = CS .EQ. ' '
                  LC = ITRIM (CS)
C                                       Get proper dimensionality
                  IF (ND2.GT.1) THEN
                     WRITE (LINE,1006) LIST(I)(1:L), ID1, ID2, CS(1:LC)
                  ELSE IF (ND1.GT.1) THEN
                     WRITE (LINE,1007) LIST(I)(1:L), ID1, CS(1:LC)
                  ELSE
                     WRITE (LINE,1008) LIST(I)(1:L), CS(1:LC)
                     END IF
C                                       Integer
               ELSE IF (TYPE.EQ.OOAINT) THEN
C                                       Don't write zeros.
                  BLNKED = IVAL(INDEX) .EQ. 0
C                                       Get proper dimensionality
                  IF (ND2.GT.1) THEN
                     WRITE (LINE,1009) LIST(I)(1:L), ID1, ID2,
     *                  IVAL(INDEX)
                  ELSE IF (ND1.GT.1) THEN
                     WRITE (LINE,1010) LIST(I)(1:L), ID1, IVAL(INDEX)
                  ELSE
                     WRITE (LINE,1011) LIST(I)(1:L), IVAL(INDEX)
                     END IF
C                                       Logical
               ELSE IF (TYPE.EQ.OOALOG) THEN
                  BLNKED = .FALSE.
C                                       Convert to POPS true/false
                  IF (LVAL(INDEX)) THEN
                     ILVAL = 1
                  ELSE
                     ILVAL = -1
                     END IF
C                                       Get proper dimensionality
                  IF (ND2.GT.1) THEN
                     WRITE (LINE,1009) LIST(I)(1:L), ID1, ID2,
     *                  ILVAL
                  ELSE IF (ND1.GT.1) THEN
                     WRITE (LINE,1010) LIST(I)(1:L), ID1, ILVAL
                  ELSE
                     WRITE (LINE,1011) LIST(I)(1:L), ILVAL
                     END IF
C                                       Unknown skip
               ELSE
                  BLNKED = .TRUE.
                  END IF
               IF (.NOT.BLNKED) THEN
                  IF (TSKNAM(6:6).EQ.' ') THEN
                     HILINE = TSKNAM // LINE(:66)
                  ELSE
                     HILINE = TSKNAM // ' ' // LINE(:65)
                     END IF
                  CALL HIADD (LUNO, HILINE, OBUFFR(1,BUFNOO), IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 40            CONTINUE
 50         CONTINUE
 100     CONTINUE
C                                       Close
 200  CALL HICLOS (LUNO, .TRUE., OBUFFR(1,BUFNOO), IRET)
      IF (IERR.NE.0) IRET = IERR
C                                       Free LUN
      CALL OBLUFR (LUNO)
C                                       Close objects
      CALL OBCLOS (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL  MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,'(',I3,',',I3,') =', 1PD20.10)
 1001 FORMAT (A,'(',I3,') =', 1PD20.10)
 1002 FORMAT (A,' =', 1PD20.10)
 1003 FORMAT (A,'(',I3,',',I3,') =', 1PE12.5)
 1004 FORMAT (A,'(',I3,') =', 1PE12.5)
 1005 FORMAT (A,' =', 1PE12.5)
 1006 FORMAT (A,'(',I3,',',I3,') =''',A,'''')
 1007 FORMAT (A,'(',I3,') =''',A,'''')
 1008 FORMAT (A,' =''',A,'''')
 1009 FORMAT (A,'(',I3,',',I3,') =', I8)
 1010 FORMAT (A,'(',I3,') =', I8)
 1011 FORMAT (A,' =', I8)
      END
      SUBROUTINE OHTIME (OUT, IRET)
C-----------------------------------------------------------------------
C   Adds task name and time and date stamp to history associated with
C   object out.  Assures that a history file exists.
C   Inputs:
C      OUT    C*?   Output object name.
C   Output:
C      IRET   I     Return code, 0=OK else an error occurred.
C-----------------------------------------------------------------------
      CHARACTER OUT*(*)
      INTEGER   IRET
C
      INTEGER   DISKO, CNOO, LUNO, BUFNOO, IERR, CAT(256), MSGSAV,
     *   TIME(3), DATE(3)
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Output
      CALL OBOPEN (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Which buffer
      CALL OBINFO (OUT, BUFNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       DISK, CNO
      CALL OBDSKC (OUT, DISKO, CNOO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       LUN
      CALL OBLUN (LUNO, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Sync header
      CALL OBCSYN (OUT, IERR)
      IF (IRET.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (OUT, CAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Initialize history routines
      CALL HIINIT (3)
C                                       Open/create history - suppress
C                                       warning messages.
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL HICREA (LUNO, DISKO, CNOO, CAT, OBUFFR(1,BUFNOO), IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 990
C                                       Write time and date
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1100) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUNO, HILINE, OBUFFR(1,BUFNOO), IERR)
C                                       Close
      CALL HICLOS (LUNO, .TRUE., OBUFFR(1,BUFNOO), IRET)
      IF (IERR.NE.0) IRET = IERR
C                                       Save catalog header in case HI
C                                       file created.
      CALL OBHPUT (OUT, CAT, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Free LUN
      CALL OBLUFR (LUNO)
C                                       Close objects
      CALL OBCLOS (OUT, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'ERROR WRITING HISTORY FOR ' // OUT
      CALL  MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
      END
