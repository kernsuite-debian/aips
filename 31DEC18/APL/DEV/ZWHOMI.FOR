      SUBROUTINE ZWHOMI (IERR)
C-----------------------------------------------------------------------
C! determines AIPSxn task name; sets NPOPS, assigns TV and TK devices
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004
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
C   ZWHOMI determines the actual task name under which the present
C   version of AIPS is running.  It uses this information to set the
C   value of NPOPS in the common /MSGCOM/.  It then assigns the TV and
C   TK devices setting NTVDEV and NTKDEV in common /DCHCOM/.
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => process name is not AIPSx
C                    2 => illegal POPS number
C                    3 => other error
C   Generic version - uses ZGNAME, ZTRLOG
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   ITV, ITK, XLEN, XLNB, ITL, JTRIM, J1, J2, J, INTVD
      LOGICAL   REMOTE
      CHARACTER NAME*6, XLATED*24, XSPARE*24
      HOLLERITH HLATED(6), HSPARE(6)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Get name of this process.
      CALL ZGNAME (NAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         IERR = 3
         GO TO 995
         END IF
C                                       Trim trailing null, check length
      ITL = JTRIM (NAME)
      IF (ITL.LE.0) THEN
         WRITE (MSGTXT,1001)
         IERR = 1
         GO TO 995
         END IF
C                                       Are we at least AIPS?
      IF ((NAME(:4).NE.'AIPS') .AND. (NAME(:4).NE.'DAIP')) THEN
         WRITE (MSGTXT,1010) NAME
         IERR = 1
         GO TO 995
         END IF
C                                       Get POPS number from process
C                                       name.
      CALL ZHEX10 (NAME(5:5), NPOPS, IERR)
      IF ((NPOPS.LT.1) .OR. (NPOPS.GT.35) .OR. (IERR.NE.0)) THEN
         WRITE (MSGTXT,1020) NAME, NAME(5:5)
         IERR = 2
         GO TO 995
         END IF
C                                       Get TV and TK device
C                                       assignments.
      ITK = NTKDEV + 1
      XLEN = LEN (XLATED)
      CALL ZTRLOG (7, 'AIPSTVS', XLEN, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) 'AIPSTVS'
         IERR = 3
         GO TO 995
         END IF
      CALL TXTLEN (XLATED(:XLNB), J1, J2)
      ITV = 0
      DO 40 J = J1,J2
         ITV = 10 * ITV + ICHAR (XLATED(J:J)) - ICHAR ('0')
 40      CONTINUE
      ITV = MAX (0, ITV)
C                                       TV device: translate logical
      XLEN = LEN (XLATED)
      CALL ZTRLOG (5, 'TVDEV', XLEN, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) 'TVDEV'
         IERR = 3
         GO TO 995
         END IF
C                                       Get TV device number.
      IF ((XLATED(1:5).NE.'TVDEV') .OR. (XLNB.LT.6)) THEN
         MSGTXT = 'ZWHOMI: INVALID TV DEVICE = ' // XLATED(1:XLNB)
         CALL MSGWRT (8)
         NTVDEV = 0
      ELSE
         CALL ZHEX10 (XLATED(6:XLNB), NTVDEV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1035) XLATED(1:XLNB), XLATED(6:XLNB)
            CALL MSGWRT (8)
            NTVDEV = 0
            END IF
         END IF
C                                       Reset for POPS numbers
C                                       not allowed TV access.
      IF (NPOPS.GT.NTVACC) NTVDEV = 0
C                                       Guest accounts
      IF (NTVDEV.GT.ITV) THEN
         MSGTXT = 'You are not on a local TV device, welcome stranger'
         CALL MSGWRT (1)
         END IF
C                                       Tell user about TV assigment.
      WRITE (MSGTXT,1070) NTVDEV
      IF (NTVDEV.LE.0) WRITE (MSGTXT,1071)
      CALL MSGWRT (1)
C                                       Graphics device:
      NTKDEV = 1
      XLEN = LEN (XLATED)
      CALL ZTRLOG (5, 'TKDEV', XLEN, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) 'TKDEV'
         IERR = 3
         GO TO 995
         END IF
C                                       Get graphics device number.
      REMOTE = .FALSE.
C                                       Remote "tek" device, use extra
C                                       tek device slots beyond last
C                                       real TV device.
      IF (XLATED(1:XLNB).EQ.'REMOTE') THEN
         REMOTE = .TRUE.
         NTKDEV = ITV + NPOPS + 36
C                                       Reset TKDEV to TKDEVxx where xx
C                                       is the ehex TV number.
         XLATED = 'TKDEV'
         XSPARE = XLATED
         CALL ZEHEX (NTKDEV, 2, XLATED(6:7))
         CALL CHR2H (24, XSPARE, 1, HSPARE)
         CALL CHR2H (24, XLATED, 1, HLATED)
         CALL ZCRLOG (5, HSPARE, 7, HLATED, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ZWHOMI: CANNOT REDEFINE TKDEV TO '//XLATED(1:7)
            CALL MSGWRT(8)
            NTKDEV = 0
            END IF
C                                       local or pseudo local
      ELSE IF (ITK.GT.1) THEN
         IF ((XLATED(1:5).NE.'TKDEV') .OR. (XLNB.LT.6)) THEN
            MSGTXT = 'ZWHOMI: INVALID TK DEVICE = ' // XLATED(1:XLNB)
            CALL MSGWRT (8)
            NTKDEV = 0
         ELSE
            CALL ZHEX10 (XLATED(6:XLNB), NTKDEV, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1090) XLATED(1:XLNB), XLATED(6:XLNB)
               CALL MSGWRT (8)
               NTKDEV = 0
               END IF
            END IF
C                                       reassign for guests
         IF ((NTKDEV.EQ.INTVD) .AND. (INTVD.LT.NTVDEV)) THEN
            XSPARE = XLATED
            XLEN = LEN (XLATED)
            J2 = XLNB
            CALL ZTRLOG (J2, XSPARE, XLEN, XLATED, XLNB, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) XSPARE
               CALL MSGWRT (6)
            ELSE
               CALL ZEHEX (NTVDEV, 2, XSPARE(6:7))
               CALL CHR2H (24, XSPARE, 1, HSPARE)
               CALL CHR2H (24, XLATED, 1, HLATED)
               CALL ZCRLOG (7, HSPARE, XLNB, HLATED, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'ZWHOMI: CANNOT DEFINE ' // XSPARE
                  CALL MSGWRT (6)
                  END IF
               END IF
            NTKDEV = NTVDEV
            IF (IERR.NE.0) NTKDEV = 0
            END IF
         END IF
C
      IF ((NPOPS.GT.NTKACC) .AND. (.NOT.REMOTE)) NTKDEV = 0
C                                       Tell user about graphics
C                                       device assigment.
      WRITE (MSGTXT,1100) NTKDEV
      IF (NTKDEV.LE.0) WRITE (MSGTXT,1101)
      IF (REMOTE) WRITE (MSGTXT,1102) NTKDEV
      CALL MSGWRT (1)
      GO TO 999
C
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZWHOMI: Trouble getting process name')
 1001 FORMAT ('ZWHOMI: ZGNAME returns an EMPTY string for process name')
 1010 FORMAT ('ZWHOMI: Process name = ',A,' unrecognized')
 1020 FORMAT ('ZWHOMI: Process is ',A,' but POPS number = ',A,
     *   ' is INVALID')
 1030 FORMAT ('ZWHOMI: Cannot translate logical = ',A)
 1035 FORMAT ('ZWHOMI: TV device ',A,
     *   ' has INVALID device number = ',A)
 1070 FORMAT ('You are assigned TV device/server',I4)
 1071 FORMAT ('You are NOT assigned a TV device or server')
 1090 FORMAT ('ZWHOMI: Graphics device ',A,
     *   ' has INVALID device number = ',A)
 1100 FORMAT ('You are assigned graphics device/server',I4)
 1101 FORMAT ('You are NOT assigned a graphics device or server')
 1102 FORMAT ('Your terminal (tek-compatible) will be graphics device',
     *   I4)
      END
