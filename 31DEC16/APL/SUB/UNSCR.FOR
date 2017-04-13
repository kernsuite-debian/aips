      SUBROUTINE UNSCR
C-----------------------------------------------------------------------
C! delete all scratch files belonging to this tasl
C# IO-WaWa
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
C   WaWa IO system:  Delete all scratch files belonging to this task
C   and POPS number.
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
C
      CHARACTER DNAM*36, STAT*4, NAME*12, CLASS*6, PTYPE*2, OPER*4
      INTEGER   DVOL, SEQ, USID, NSUB, NOFIND, IERR, QUSID, CNO
      LOGICAL   LCH, F
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
      DATA NOFIND /5/
C-----------------------------------------------------------------------
C                                       Get basic namestring
      DVOL = 0
      SEQ = 0
      USID = 0
      PTYPE = 'SC'
      NAME = ' '
      CLASS = ' '
      CALL A2WAWA (NAME, CLASS, SEQ, PTYPE, DVOL, USID, DNAM)
      CALL SCRNAM (DNAM, DNAM)
      CALL WAWA2A (DNAM, NAME, CLASS, SEQ, PTYPE, DVOL, USID)
      QUSID = USID
C                                       Loop for all seq numbers
 10   CONTINUE
         DVOL = 0
         SEQ = 0
         USID = QUSID
         NSUB = 1
         CNO = 1
         OPER = 'SRCH'
         CALL CATDIR ('SRCN', DVOL, CNO, NAME, CLASS, SEQ, PTYPE,
     *      USID, STAT, WBUFF, IERR)
         IF (IERR.EQ.NOFIND) GO TO 999
            IF (IERR.NE.0) GO TO 980
C                                       Clear ALL status flags!
 20         CONTINUE
               OPER = 'INFO'
               CALL CATDIR ('INFO', DVOL, CNO, NAME, CLASS, SEQ,
     *            PTYPE, USID, STAT, WBUFF, IERR)
               OPER = 'CSTA'
               LCH = STAT.EQ.'REST'
               IF (LCH) GO TO 50
                  LCH = STAT.EQ.'READ'
                  IF (.NOT.LCH) GO TO 30
                     OPER = 'CSTA'
                     CALL CATDIR ('CSTA', DVOL, CNO, NAME, CLASS, SEQ,
     *                   PTYPE, USID, 'CLRD', WBUFF, IERR)
                     IF (IERR.NE.0) GO TO 980
                     GO TO 20
 30               CONTINUE
                     CALL CATDIR ('CSTA', DVOL, CNO, NAME, CLASS, SEQ,
     *                   PTYPE, USID, 'CLWR', WBUFF, IERR)
                     IF (IERR.NE.0) GO TO 980
                     GO TO 20
C                                       Actual file destroy
 50         CALL A2WAWA (NAME, CLASS, SEQ, PTYPE, DVOL, USID, DNAM)
            CALL FILDES (DNAM, F, CNO, STAT, IERR)
            GO TO 10
C                                       Error return
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, IERR
      CALL MSGWRT (8)
      CALL PRTNAM (DNAM, 8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('UNSCR: ',A6,2X,A4,' ERROR',I7)
      END
