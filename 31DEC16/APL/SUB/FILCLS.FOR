      SUBROUTINE FILCLS (LUN)
C-----------------------------------------------------------------------
C! close file opened by FILOPN, flushing write buffers, clearing catalog
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
C   WaWa IO system: Close a file opened by FILOPN, taking care of
C   catalog bookkeeping and flush last write buffers if any.
C   Inputs:
C      LUN       I      Logical unit no. of file to close
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      INTEGER   LUN
C
      CHARACTER CHTM12*12, CHTM6*6, DUM2*2, CLROP*4, OPER*4
      INTEGER   IFIL, IERR, NSUB, IOP, RDINT, WARN, IDUM
      LOGICAL   ASSOC, LWRIT, MAP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      DATA WARN /10/
C-----------------------------------------------------------------------
C                                       Is file in fact open?
      DO 10 IFIL = 1,EFIL
         IF (FILTAB(1,IFIL).EQ.LUN) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       Recover some info from FILTAB
 20   ASSOC = FILTAB(POASS,IFIL).EQ.1
      MAP = (LUN.GE.16) .AND. (LUN.LE.25)
      IOP = ABS (FILTAB(POIOP,IFIL))
      LWRIT = IOP.EQ.1
C                                       Flush out output buffers
      IF (.NOT. (LWRIT .AND. MAP)) GO TO 40
         OPER = 'FINI'
         NSUB = 4
         RDINT = LREAL * (IFIL - 6) + 1
         CALL MDISK ('FINI', LUN, FILTAB(POFIN,IFIL), RBUF(RDINT), IDUM,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       READ/WRIT reset status
 40   IF ((IOP.LT.1) .OR. (IOP.GT.2)) GO TO 50
         NSUB = 1
         OPER = 'CSTA'
         CLROP = 'CLRD'
         IF (LWRIT .AND. (.NOT.ASSOC)) CLROP = 'CLWR'
         CALL CATDIR ('CSTA', FILTAB(POVOL,IFIL), FILTAB(POCAT,IFIL),
     *      CHTM12, CHTM6, IDUM, DUM2, IDUM, CLROP, WBUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.WARN)) GO TO 980
C                                       Close the file
 50   NSUB = 5
      OPER = 'CLOS'
      CALL ZCLOSE (LUN, FILTAB(POFIN,IFIL), IERR)
      FILTAB(POLUN,IFIL) = 0
      IF (IERR.EQ.0) GO TO 999
C                                       Error return
 980  WRITE (MSGTXT,1980) SUBNAM(NSUB), OPER, LUN, IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('FILCLS: ',A6,2X,A4,' LUN',I4,' ERROR ',I6)
      END
