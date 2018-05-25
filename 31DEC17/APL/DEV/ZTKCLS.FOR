      SUBROUTINE ZTKCLS (IERR)
C-----------------------------------------------------------------------
C! close the TK device
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Close a Tektronix device
C   Inputs: LUN, FIND from common DTKS.INC
C   Output:
C      IERR     I   Error return code: 0 => no error
C                      1 => Non-zero ZTKCL2 error
C                      2 => Non-zero LSERCH error
C                      3 => both 1 and 2
C                      4 => invalid LUN
C   Generic version -  calls ZTKCL2 to perform the actual close.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   FCBOFF, IER, I, J
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Valid LUN?
      IF (TKLUN.EQ.7) GO TO 10
         IERR = 4
         WRITE (MSGTXT,1000) TKLUN
         GO TO 995
C                                       Locate LUN in FTAB.
 10   CALL LSERCH ('SRCH', TKLUN, TKFIND, F, IER)
      IF (IER.EQ.0) GO TO 20
         IERR = 2
         WRITE (MSGTXT,1010) IER, 'SRCH', TKLUN
         GO TO 995
C                                       Close device.
 20   FCBOFF = TKFIND + NMOFF
      CALL ZTKCL2 (FTAB(FCBOFF), IER)
      IF (IER.EQ.0) GO TO 30
         IERR = IERR + 1
         WRITE (MSGTXT,1020) IER
         CALL MSGWRT (7)
         CALL ZERROR ('ZTKCL2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *      F)
C                                       Remove FTAB entry.
 30   CALL LSERCH ('CLOS', TKLUN, TKFIND, F, IER)
      IF (IER.EQ.0) THEN
         CALL FSERCH (TKFIND, I, J, IER)
         IF (IER.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
      ELSE
         IERR = IERR + 2
         WRITE (MSGTXT,1010) IER, 'CLOS', TKLUN
         END IF
      TKFIND = 0
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTKCLS: LUN = ',I2,' INVALID FOR TEKTRONIX DEVICE')
 1010 FORMAT ('ZTKCLS: LSERCH ERROR = ',I1,' ON ',A4,' FOR LUN = ',I2)
 1020 FORMAT ('ZTKCLS: ZTKCL2 ERROR = ',I1)
      END
