      SUBROUTINE HISCOP (LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD,
     *   CNONEW, CATBLK, BUFER1, BUFER2, IERR)
C-----------------------------------------------------------------------
C! creates new history file and copies an old one to it
C# History
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
C   HISCOP copies one history file to another.  If the new history
C   file already exists the only action is to open it.  At finish
C   the old history file is closed; the new history file is open.
C   The task name, date, and time are entered on the new file.
C   Inputs:
C      LUNOLD   I        LUN for old history file.
C      LUNNEW   I        LUN for new history file.
C      VOLOLD   I        Vol. number for old history file.
C      VOLNEW   I        Vol. number for new history file.
C      CNOOLD   I        Catalog slot number of old history file.
C      CNONEW   I        Catalog slot number of new history file.
C   In/Out:
C      CATBLK   I(256)   Catalog header of map for new file.
C      BUFER1   I(256)   Work buffer, used for old file.
C      BUFER2   I(256)   Work buffer, new file; must be used in
C                        further HIADD calls until file is closed.
C   Output:
C      IERR     I        Return error code: 0 => OK.
C                           1 => could not open old history file.
C                           2 => could not copy old history file.
C                           3 => could not write time on new file
C                           4 => could not create/open new HI file.
C   NOTE: IERR < 3 is a warning only, = 3 serious, = 4 a real problem.
C   Calling programs should ignore IERR < 3, branch to HICLOS of the
C   new HI file on IERR = 3, and skip over all HI stuff on IERR = 4.
C-----------------------------------------------------------------------
      INTEGER   LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD, CNONEW,
     *   CATBLK(256), BUFER1(256), BUFER2(256), IERR
C
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INTEGER   IER, TIME(3), DATE(3), JERR, IPTR, ITEMP
      LOGICAL   F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 4
C                                       open existing "new" file
      IF ((VOLNEW.NE.VOLOLD) .OR. (CNONEW.NE.CNOOLD)) GO TO 10
         CALL HIOPEN (LUNNEW, VOLNEW, CNONEW, BUFER2, IER)
         IF (IER.EQ.0) IERR = 3
C                                       File should, but doesn't exist
C                                       try create, copy not possible
         IF (IER.EQ.0) GO TO 100
            IF (IER.NE.5) GO TO 999
               CALL HICREA (LUNNEW, VOLNEW, CNONEW, CATBLK, BUFER2,
     *            IER)
               IF (IER.NE.0) GO TO 999
                  IERR = 1
                  GO TO 100
C                                       create new HI file
 10   CONTINUE
         CALL HICREA (LUNNEW, VOLNEW, CNONEW, CATBLK, BUFER2, IER)
         IF (IER.NE.0) GO TO 999
         CALL HILOCT ('SRCH', LUNNEW, IPTR, IER)
         ITEMP = BUFER2(2)
C                                       copy old to new
         IERR = 1
         CALL HIOPEN (LUNOLD, VOLOLD, CNOOLD, BUFER1, IER)
         IF (IER.NE.0) GO TO 20
         IERR = 2
         CALL HICOPY (LUNOLD, BUFER1, LUNNEW, BUFER2, IER)
         CALL HICLOS (LUNOLD, F, BUFER1, JERR)
         IF (IER.EQ.0) IERR = 3
C                                       discard input if bad read
         IF (IER.EQ.0) GO TO 100
            IF (IER.LT.100) GO TO 20
               HITAB(IPTR+2) = 0
               HITAB(IPTR+3) = ITEMP
               HITAB(IPTR+6) = 1
               BUFER2(1) = 0
               BUFER2(2) = ITEMP
C                                       warning message
 20      CONTINUE
            WRITE (MSGTXT,1020)
            CALL MSGWRT (7)
C                                       Write time and date on new file
 100  CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1100) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUNNEW, HILINE, BUFER2, IER)
      IF ((IER.EQ.0) .AND. (IERR.EQ.3)) IERR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('UNABLE TO COPY OLD HISTORY TO NEW HI FILE')
 1100 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
      END
