      PROGRAM FIXCAT
C-----------------------------------------------------------------------
C! FIXCAT deletes entries in map catalogs
C# Utility Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   A service routine to delete entries in map catalogs.
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLAS*6, PTYPE*2, STAT*4, MSGBUF*80, PRGNAM*6
      INTEGER   IVOL, ICNO, ISEQ, IUSER, IBUF(256), TTYLUN, TTYIND,
     *   IERR, ICNO1, ICNO2, I, TTY(2), IARR(4)
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TTY(1), TTYLUN),  (TTY(2), TTYIND)
      DATA PRGNAM /'FIXCAT'/
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       require password
      CALL PASWRD (IBUF, IERR)
      IF (IERR.NE.0) GO TO 995
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
C                                       Loop to ask for vol, cat #
 10   WRITE (MSGBUF,1010)
      CALL INQINT (TTY, MSGBUF, 4, IARR, IERR)
      IF (IERR.GT.0) GO TO 990
      IF (IERR.LT.0) GO TO 995
      IVOL = IARR(1)
      NLUSER = IARR(2)
      ICNO1 = IARR(3)
      ICNO2 = IARR(4)
      IF (ICNO1.LE.0) GO TO 995
      IF (ICNO2.LT.ICNO1) ICNO2 = ICNO1
      DO 50 ICNO = ICNO1,ICNO2
         CALL CATDIR ('INFO', IVOL, ICNO, NAME, CLAS, ISEQ, PTYPE,
     *      IUSER, STAT, IBUF, IERR)
         IF (IERR.EQ.0) GO TO 20
            IF (IERR.EQ.6) GO TO 50
            WRITE (MSGTXT,1012) IERR
            CALL MSGWRT (7)
            GO TO 10
C                                       clear status
 20      IF (STAT.EQ.'REST') GO TO 30
            CALL CATDIR ('CSTA', IVOL, ICNO, NAME, CLAS, ISEQ, PTYPE,
     *         IUSER, 'CLWR', IBUF, IERR)
            DO 25 I = 1,100
               CALL CATDIR ('CSTA', IVOL, ICNO, NAME, CLAS, ISEQ, PTYPE,
     *             IUSER, 'CLRD', IBUF, IERR)
               IF (IERR.NE.0) GO TO 30
 25            CONTINUE
C                                       close it
 30      WRITE (MSGTXT,1030) IVOL, ICNO, NAME, CLAS, ISEQ, IUSER, PTYPE
         CALL MSGWRT (4)
         CALL CATDIR ('CLOS', IVOL, ICNO, NAME, CLAS, ISEQ, PTYPE,
     *      IUSER, STAT, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1031) IERR
            CALL MSGWRT (6)
            END IF
 50      CONTINUE
      GO TO 10
C                                       Terminal error
 990  WRITE (MSGTXT,1990) IERR
      CALL MSGWRT (8)
C
 995  NLUSER = 1
      CALL ZCLOSE (TTYLUN, TTYIND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('I close catalog entries')
 1010 FORMAT ('Enter Vol#, User, Cat# range (4 of I)  Q to quit')
 1012 FORMAT ('CATDIR INFO ERROR',I7)
 1030 FORMAT ('Closing vol,cno',I2,I4,4X,A12,'.',A6,'.',I4,
     *   2X,I4,1X,A2)
 1031 FORMAT ('CATDIR CLOSE ERROR',I7)
 1990 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
