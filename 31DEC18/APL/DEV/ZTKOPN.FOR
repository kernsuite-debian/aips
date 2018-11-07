      SUBROUTINE ZTKOPN (IERR)
C-----------------------------------------------------------------------
C! open a TK device
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2004
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
C   Open a Tektronix device (calls ZTKOP2 to perform the actual open).
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => LUN already in use
C                    2 => no such logical device
C                    3 => device not found
C                    4 => exclusive use denied
C                    5 => no room for LUN in FTAB
C                    6 => other open errors
C   Also sets up DTKS.INC common
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER PNAME*48
      HOLLERITH HNAME(12)
      INTEGER   FCBOFF, DELMAX, DLMAX2, DELCNT, DLCNT2, MAXTRY, IDELAY,
     *   WAITED, JERR, I, J
      LOGICAL   F
      REAL      TDELAY
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA IDELAY, DELMAX, DLMAX2 /2, 15, 10/
C-----------------------------------------------------------------------
      TKLUN = 7
      IERR = 0
      DELCNT = 0
      DLCNT2 = 0
      WAITED = 0
C                                       Allocate area in FTAB for LUN.
 10   CALL LSERCH ('OPEN', TKLUN, TKFIND, F, JERR)
      IF (JERR.EQ.0) GO TO 30
C                                       LUN already in use.
         IF (JERR.NE.2) GO TO 20
            IERR = 1
            WRITE (MSGTXT,1010) TKLUN
            GO TO 995
C                                       No room in FTAB for LUN.
 20      IF (JERR.NE.3) GO TO 30
            IERR = 5
            WRITE (MSGTXT,1020) TKLUN
            GO TO 995
C                                       Build physical file name for
C                                       Tektronix device.
 30   CALL ZPHFIL ('TK', 1, 0, 0, PNAME, IERR)
      IF (IERR.EQ.0) GO TO 50
         IERR = 6
         WRITE (MSGTXT,1030)
         GO TO 900
C                                       Open Tektronix device.
 50   FCBOFF = TKFIND + NMOFF
      IF (SYSTYP.EQ.'VMS') THEN
         CALL ZTKOP2 (FTAB(FCBOFF), PNAME, TKTYPE, IERR)
      ELSE
         CALL CHR2H (48, PNAME, 1, HNAME)
         CALL ZTKOP2 (FTAB(FCBOFF), HNAME, TKTYPE, IERR)
         END IF
C                                       store file name
      IF (IERR.EQ.0) THEN
         CALL FSERCH (TKFIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = PNAME
            IF (I.EQ.2) NONNAM(J) = PNAME
            IF (I.EQ.3) MAPNAM(J) = PNAME
            END IF
         TKPOS = 0
         CALL RFILL (TKSIZE, 0.0, TKBUFF)
         GO TO 999
      ELSE
C                                       Device busy.  Wait and try again
C                                       once with no message, then every
C                                       IDELAY secs up to DELMAX*DLMAX2
C                                       times.  Issue a message every
C                                       DELMAX*IDELAY secs.
         IF (IERR.EQ.4) THEN
            IF (WAITED.NE.IDELAY) GO TO 52
               WRITE (MSGTXT,1050)
               CALL MSGWRT (2)
               MAXTRY = DELMAX * DLMAX2
               WRITE (MSGTXT,1051) MAXTRY, IDELAY
               CALL MSGWRT (2)
 52         DELCNT = DELCNT + 1
            IF (DELCNT.GT.DELMAX) GO TO 55
               TDELAY = IDELAY * 1.0
               CALL ZDELAY (TDELAY, IERR)
               WAITED = WAITED + IDELAY
               GO TO 50
 55         DLCNT2 = DLCNT2 + 1
C                                       Device still busy message.
            IF (DLCNT2.LT.DLMAX2) THEN
               WRITE (MSGTXT,1055) WAITED
               CALL MSGWRT (2)
               DELCNT = 0
               GO TO 50
C                                       Device unavailable.
            ELSE
               WRITE (MSGTXT,1060)
               END IF
C                                       No such logical device.
         ELSE IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1070) PNAME(1:24)
C                                       No such device
         ELSE IF (IERR.EQ.3) THEN
            WRITE (MSGTXT,1080) PNAME(1:24)
C                                       Other open error.
         ELSE
            CALL ZERROR ('ZTKOP2', FTAB(FCBOFF+FCBERR), PNAME,
     *         FTAB(FCBOFF), F)
            GO TO 910
            END IF
         END IF
C                                       Error
 900  CALL MSGWRT (6)
C                                       Remove FTAB entry for LUN.
 910  CALL LSERCH ('CLOS', TKLUN, TKFIND, F, JERR)
      TKFIND = 0
      IF (JERR.EQ.0) GO TO 999
         IERR = IERR + 2
         WRITE (MSGTXT,1900) JERR, 'CLOS', TKLUN
C
 995  CALL MSGWRT (6)
      TKFIND = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZTKOPN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1020 FORMAT ('ZTKOPN: NO ROOM IN FTAB FOR LUN = ',I2)
 1030 FORMAT ('ZTKOPN: ZPHFIL ERROR FOR TYPE = TK VOL = 1',
     *   ' SEQ = 0 VER = 0')
 1050 FORMAT ('ZTKOPN: TEKTRONIX DEVICE IS BUSY')
 1051 FORMAT ('ZTKOPN: WILL KEEP TRYING UP TO ',I3,' TIMES AT ',I1,
     *   ' SECOND INTERVALS')
 1055 FORMAT ('ZTKOPN: STILL WAITING FOR TEKTRONIX DEVICE AFTER ',I3,
     *   ' SECONDS')
 1060 FORMAT ('ZTKOPN: TEKTRONIX DEVICE NOT AVAILABLE.')
 1070 FORMAT ('ZTKOPN: NO SUCH LOGICAL DEVICE = ',A24)
 1080 FORMAT ('ZTKOPN: TEKTRONIX DEVICE ',A24,' NOT FOUND')
 1900 FORMAT ('ZTKOPN: LSERCH ERROR = ',I1,' ON ',A4,' FOR LUN = ',I2)
      END
