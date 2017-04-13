      PROGRAM SETPAR
C-----------------------------------------------------------------------
C! stand-alone program to set system parameters in SP file
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2006, 2008, 2011
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
C   This program initializes or changes the system parameters kept in
C   the System Parameter (SP) file.  It is read by ZDCHIN to initialize
C   the values in common CDCH.
C-----------------------------------------------------------------------
      CHARACTER INAME*48, MSGBUF*80, PRGNAM*6
      REAL      ROBLK(256)
      INTEGER   IOBLK(256), ILUN, TTYIND, TTYLUN, I, I0, I1, I2, I3,
     *   I4, IERR, JERR, IND, IOPT, ISTDEV, J1, J2, J3, J4,
     *   SCRTCH(256), INIPRM(256)
      LOGICAL   PASPER, PASCHG, NOMAP, EXCL, WAIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (EXCL, WAIT),   (IOBLK, ROBLK)
      COMMON /TTYCOM/ TTYLUN, TTYIND
      DATA ISTDEV /14/
      DATA PRGNAM /'SETPAR'/
      DATA ILUN /27/
      DATA NOMAP, EXCL /.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      TTYLUN = 5
C                                       Password not entered
      PASPER = .FALSE.
C                                       Parameters not modified
      PASCHG = .FALSE.
C                                       Setup
      CALL AIPINI (TTYLUN, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Open parameter file
 50   CALL ZPHFIL ('SP', 1, 0, 0, INAME, IERR)
      CALL ZOPEN (ILUN, IND, 1, INAME, NOMAP, EXCL, WAIT, JERR)
      IF (IERR.NE.0 .OR. JERR.NE.0) THEN
         WRITE (MSGTXT, 1050) IERR, JERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Read initial parameters
      CALL ZFIO ('READ', ILUN, IND, 1, INIPRM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Determine init, change, quit
 100  WRITE (MSGBUF,1100)
      CALL INQINT (TTYLUN, MSGBUF, 1, IOPT, IERR)
      IF (IERR.LT.0) GO TO 100
      IF (IERR.GT.0) GO TO 980
      IF ((IOPT.GE.1) .AND. (IOPT.LE.4)) GO TO (200, 300, 400, 990),
     *   IOPT
C                                       Invalid entry.
 110  CONTINUE
         WRITE (MSGTXT,1110)
         CALL MSGWRT (6)
         GO TO 100
C                                       1 - Start Over
 200  CONTINUE
         CALL COPY (256, INIPRM, IOBLK)
         PASCHG = .TRUE.
         GO TO 350
C                                       2 - Change Parameters
 300  CONTINUE
         CALL ZFIO ('READ', ILUN, IND, 1, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         PASCHG = .FALSE.
C
 350  CALL QUEST (0, IOBLK, ROBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Enter option number
 360  WRITE (MSGBUF,1360)
      CALL INQINT (TTYLUN, MSGBUF, 1, IOPT, IERR)
      IF (IERR.LT.0) GO TO 360
      IF (IERR.GT.0) GO TO 980
         IF (IOPT.EQ.-1) GO TO 900
         IF (IOPT.EQ.0) GO TO 350
         PASCHG = .TRUE.
         CALL QUEST (IOPT, IOBLK, ROBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         GO TO 360
C                                       3 - Change DEVTAB values
 400  CONTINUE
         CALL ZFIO ('READ', ILUN, IND, 1, IOBLK, IERR)
         PASCHG = .FALSE.
         IF (IERR.NE.0) GO TO 990
C                                       Print DEVTAB values.
 410  DO 420 I = 1,10
         I0 = I + ISTDEV - 1
         I1 = I + ISTDEV + 9
         I2 = I + ISTDEV + 19
         I3 = I + ISTDEV + 29
         I4 = I + ISTDEV + 39
         J1 = I + 10
         J2 = I + 20
         J3 = I + 30
         J4 = I + 40
         WRITE (MSGBUF,1410) I, IOBLK(I0), J1, IOBLK(I1), J2,
     *      IOBLK(I2), J3, IOBLK(I3), J4, IOBLK(I4)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
 420     CONTINUE
C                                       Enter no for change, 0=p ...
 430  WRITE (MSGBUF,1360)
      CALL INQINT (TTYLUN, MSGBUF, 1, IOPT, IERR)
      IF (IERR.LT.0) GO TO 430
      IF (IERR.GT.0) GO TO 980
         IF (IOPT.EQ.-1) GO TO 900
         IF (IOPT.EQ.0) GO TO 410
C                                       Invalid entry.
         IF ((IOPT.GT.0) .AND. (IOPT.LE.50)) GO TO 450
 440        WRITE (MSGTXT,1110)
            CALL MSGWRT (7)
            GO TO 430
C                                       Read new DEVTAB value
 450     WRITE (MSGBUF,1450) IOPT
         CALL INQINT (TTYLUN, MSGBUF, 1, IOBLK(IOPT+ISTDEV-1), IERR)
         IF (IERR.LT.0) GO TO 450
         IF (IERR.GT.0) GO TO 980
         PASCHG = .TRUE.
         GO TO 430
C                                       Save record.
 900  IF (.NOT.PASCHG) GO TO 100
C                                       Require password
         IF (PASPER) GO TO 910
            CALL PASWRD (SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            PASPER = .TRUE.
C                                       write it
 910  CALL ZFIO ('WRIT', ILUN, IND, 1, IOBLK, IERR)
      GO TO 100
C                                       Terminal I/O error
 980  WRITE (MSGTXT,1980) IERR
      CALL  MSGWRT (8)
C                                       4 - Quit; close parm file
 990  IF (IND.NE.0) CALL ZCLOSE (ILUN, IND, IERR)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR!  System Parameter file open error:',2I7)
 1100 FORMAT ('Enter:  1=Start Over, 2=Change parameters,',
     *        ' 3=Change DEVTAB, 4=Quit')
 1110 FORMAT ('INVALID ENTRY.  TRY AGAIN.')
 1360 FORMAT ('Enter number to change or  0 = Print, -1 = Return')
 1410 FORMAT (I3,'.',I5,4(I7,'.',I5))
 1450 FORMAT ('Enter DEVTAB(',I2,')  using I format')
 1980 FORMAT ('ERROR',I7,' in terminal I/O')
      END
      SUBROUTINE QUEST (IOPT, IOBLK, ROBLK, IERR)
C-----------------------------------------------------------------------
C   Ask questions.
C   Input:
C      IOPT   I        question to ask. 0 = all. -1 = print.
C   In/Out:
C      IOBLK  I(256)   Block of parameter values: integers
C      ROBLK  R(256)   Block of parameter values: reals
C   Output:
C      IERR   I        Error code from terminal IO.
C   Note that ROBLK and IOBLK are expected to be equivalenced in the
C   calling routine.
C-----------------------------------------------------------------------
      INTEGER   IOPT, IOBLK(256), IERR
      REAL      ROBLK(256)
C
      INTEGER   TTYLUN, TTYIND, I, J1, J2, I1, I2, IMDISK(15), IVOL, J,
     *   K, KK(9), ADDRSZ, KMAX
      CHARACTER  MSGBUF*80, ANAME*20
      DOUBLE PRECISION DK(4)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /TTYCOM/ TTYLUN, TTYIND
C-----------------------------------------------------------------------
 10   IERR = 0
      IF (IOPT.EQ.0) GO TO 100
      IF ((IOPT.GE.1) .AND. (IOPT.LE.35)) GO TO (100, 110, 120, 130,
     *   140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250,
     *   260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370,
     *   380, 390, 400, 410, 420, 430, 440), IOPT
C                                       Invalid option.
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 999
C                                       # of large disks.
 100  WRITE (MSGBUF,1100) IOBLK(1)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 10
            IOBLK(1) = K
            END IF
         GO TO 999
         END IF
C                                       # of tape drives
 110  WRITE (MSGBUF,1110) IOBLK(2)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 10
            IOBLK(2) = K
            END IF
         GO TO 999
         END IF
C                                       # lines per CRT page.
 120  WRITE (MSGBUF,1120) IOBLK(3)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IOBLK(3) = K
            END IF
         GO TO 999
         END IF
C                                       # lines per print page.
 130  WRITE (MSGBUF,1130) IOBLK(4)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.10) .OR. (K.GT.300)) GO TO 10
            IOBLK(4) = K
            END IF
         GO TO 999
         END IF
C                                       # of batch queues
 140  WRITE (MSGBUF,1140) IOBLK(5)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.0) .OR. (K.GT.34-MAX(1,IOBLK(10)))) GO TO 10
            IOBLK(5) = K
            END IF
         GO TO 999
         END IF
C                                       Printer plotter.
C                                       X dots per page.
 150  WRITE (MSGBUF,1150) IOBLK(6)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.10) .OR. (K.GT.200000)) GO TO 10
            IOBLK(6) = K
            END IF
         GO TO 999
         END IF
C                                       Y dots per page.
 160  WRITE (MSGBUF,1160) IOBLK(7)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.10) .OR. (K.GT.200000)) GO TO 10
            IOBLK(7) = K
            END IF
         GO TO 999
         END IF
C                                       X dots per character.
 170  WRITE (MSGBUF,1170) IOBLK(8)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.3) .OR. (K.GT.200)) GO TO 10
            IOBLK(8) = K
            END IF
         GO TO 999
         END IF
C                                       Y dots per character.
 180  WRITE (MSGBUF,1180) IOBLK(9)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.3) .OR. (K.GT.200)) GO TO 10
            IOBLK(9) = K
            END IF
         GO TO 999
         END IF
C                                       # of interactive AIPSs
 190  WRITE (MSGBUF,1190) IOBLK(10)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 10
            IOBLK(10) = K
            IF ((IOBLK(10)+IOBLK(5).GT.34) .AND. (IOBLK(5).GT.0)) THEN
               K = 34 - IOBLK(10)
               WRITE (MSGBUF,1195) IOBLK(5), K
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
               END IF
            END IF
         GO TO 999
         END IF
C                                       # words in AP (in 1024's)
 200  WRITE (MSGBUF,1200) IOBLK(11)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            CALL ZADRSZ (ADDRSZ)
            KMAX = 3 * 128 * 1024
            IF (ADDRSZ.EQ.8) KMAX = 6 * 256 * 1024
            IF ((K.LT.0) .OR. (K.GT.KMAX)) GO TO 10
            IOBLK(11) = K
            END IF
         GO TO 999
         END IF
C                                       # of TV devices available.
 210  WRITE (MSGBUF,1210) IOBLK(12)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.0) .OR. (K.GT.1295)) GO TO 10
            IOBLK(12) = K
            END IF
         GO TO 999
         END IF
C                                       # of graphics devices
 220  WRITE (MSGBUF,1220) IOBLK(13)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.0) .OR. (K.GT.1295)) GO TO 10
            IOBLK(13) = K
            END IF
         GO TO 999
         END IF
C                                       # X dots per mm on printer.
 230  WRITE (MSGBUF,1230) ROBLK(71)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LT.0.1) .OR. (DK(1).GT.100.)) GO TO 10
            ROBLK(71) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       # X lines per mm on tek.
 240  WRITE (MSGBUF,1240) ROBLK(72)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LT.0.1) .OR. (DK(1).GT.100.)) GO TO 10
            ROBLK(72) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       # POPS allowed TV access
 250  WRITE (MSGBUF,1250) IOBLK(64)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 10
            IOBLK(64) = K
            END IF
         GO TO 999
         END IF
C                                       # POPS allowed TK access
 260  WRITE (MSGBUF,1260) IOBLK(65)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.35)) GO TO 10
            IOBLK(65) = K
            END IF
         GO TO 999
         END IF
C                                       # entries in private catlgs
 270  WRITE (MSGBUF,1270) IOBLK(66)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.1) .OR. (K.GT.46655)) GO TO 10
            IOBLK(66) = K
            END IF
         GO TO 999
         END IF
C                                       System name/Registration
 280  CALL H2CHR (20, 1, ROBLK(73), ANAME)
      WRITE (MSGBUF,1280) ANAME
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQSTR (TTYLUN, MSGBUF, 20, ANAME, IERR)
         IF (IERR.EQ.0) THEN
            IF (ANAME.EQ.' ') GO TO 10
            CALL CHR2H (20, ANAME, 1, ROBLK(73))
            END IF
         GO TO 999
         END IF
C                                       Maximum user number
 290  WRITE (MSGBUF,1290) IOBLK(67)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.10) .OR. (K.GT.46655)) GO TO 10
            IOBLK(67) = K
            END IF
         GO TO 999
         END IF
C                                       Disk data set TIMDEST limits
 300  J2 = 0
 301  J1 = J2 + 1
         J2 = J1 + 2
         IF (J2.GT.IOBLK(1)) J2 = IOBLK(1)
         I1 = J1 + 77
         I2 = J2 + 77
 302     WRITE (MSGBUF,1300) J1, J2, (ROBLK(I), I = I1,I2)
         IF (IOPT.EQ.0) THEN
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            K = I2 - I1 + 1
            CALL INQFLT (TTYLUN, MSGBUF, K, DK, IERR)
            IF (IERR.LT.0) GO TO 10
            IF (IERR.NE.0) GO TO 999
            DO 305 I = 1,K
               IF ((DK(I).LE.0.25) .OR. (DK(I).GT.365.)) GO TO 302
 305           CONTINUE
            DO 306 I = 1,K
               ROBLK(I1+I-1) = DK(I)
 306           CONTINUE
            END IF
         IF (J2.LT.IOBLK(1)) GO TO 301
         IF (IOPT.NE.0) GO TO 999
C                                       TIMDEST limit: SAVE/GET
 310  WRITE (MSGBUF,1310) ROBLK(93)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LE.3.0) .OR. (DK(1).GT.365.0)) GO TO 10
            ROBLK(93) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       TIMDEST/EXIT limit messages
 320  WRITE (MSGBUF,1320) ROBLK(94)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LE.1.0) .OR. (DK(1).GT.365.0)) GO TO 10
            ROBLK(94) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       TIMDEST limit scratch
 330  WRITE (MSGBUF,1330) ROBLK(95)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LE.1.0) .OR. (DK(1).GT.35.0)) GO TO 10
            ROBLK(95) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       TIMDEST limit: empty CA files
 340  WRITE (MSGBUF,1340) ROBLK(96)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LE.0.1) .OR. (DK(1).GT.5.0)) GO TO 10
            ROBLK(96) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       No AP batch starts ranges
 350  WRITE (MSGBUF,1350) ROBLK(97), ROBLK(98)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 2, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LT.0.) .OR. (DK(1).GT.24.)) GO TO 350
            IF ((DK(2).LT.0.) .OR. (DK(2).GT.24.)) GO TO 350
            IF (DK(2).LT.DK(1)) GO TO 350
            ROBLK(97) = DK(1)
            ROBLK(98) = DK(2)
         ELSE
            GO TO 999
            END IF
         END IF
 355  WRITE (MSGBUF,1355) ROBLK(99), ROBLK(100)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 2, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LT.0.) .OR. (DK(1).GT.24.)) GO TO 355
            IF ((DK(2).LT.0.) .OR. (DK(2).GT.24.)) GO TO 355
            IF (DK(2).LT.DK(1)) GO TO 355
            ROBLK(99) = DK(1)
            ROBLK(100) = DK(2)
            END IF
         GO TO 999
         END IF
C                                       AP: roll time, delay formula
 360  WRITE (MSGBUF,1360) ROBLK(101)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (DK(1).LE.1.) GO TO 360
            ROBLK(101) = DK(1)
         ELSE
            GO TO 999
            END IF
         END IF
 365  WRITE (MSGBUF,1365) ROBLK(102), ROBLK(103)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 2, DK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (DK(1).LE.0.1) GO TO 365
            ROBLK(102) = DK(1)
            ROBLK(103) = DK(2)
            END IF
         GO TO 999
         END IF
C                                       Maximum line printer width
 370  WRITE (MSGBUF,1370) IOBLK(68)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.72) .OR. (K.GT.132)) GO TO 10
            IOBLK(68) = K
            END IF
         GO TO 999
         END IF
C                                       Secondary AP space
 380  IF (IOPT.NE.0) THEN
         GO TO 999
         END IF
C                                       Max length short vector
 390  WRITE (MSGBUF,1390) IOBLK(70)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.3) .OR. (K.GT.2000)) GO TO 10
            IOBLK(70) = K
            END IF
         GO TO 999
         END IF
C                                       TK device screen size
 400  WRITE (MSGBUF,1400) IOBLK(224), IOBLK(225)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((KK(1).LT.300) .OR. (KK(1).GT.16384)) GO TO 10
            IF ((KK(2).LT.300) .OR. (KK(2).GT.16384)) GO TO 10
            CALL COPY (2, KK, IOBLK(224))
            END IF
         GO TO 999
         END IF
C                                       TK device char size
 410  WRITE (MSGBUF,1410) IOBLK(226), IOBLK(227)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((KK(1).LT.5) .OR. (KK(1).GT.100)) GO TO 10
            IF ((KK(1).LT.5) .OR. (KK(1).GT.100)) GO TO 10
            CALL COPY (2, KK, IOBLK(226))
            END IF
         GO TO 999
         END IF
C                                       Disk assignments
 420  CALL FILL (15, 0, IMDISK)
      IVOL = IOBLK(1)
      IVOL = MIN (IVOL, 15)
      IF ((IVOL.LE.0) .AND. (IOPT.NE.0)) GO TO 999
      IF (IVOL.LE.0) GO TO 430
      DO 422 I = 1,IVOL
         J1 = 103 + 8 * (I-1)
         DO 421 J = 1,8
            IF (IOBLK(J+J1).EQ.0) GO TO 422
            IF ((IOBLK(J+J1).LT.0) .AND. (J.GT.1)) GO TO 422
            IMDISK(I) = J
 421        CONTINUE
 422     CONTINUE
      WRITE (MSGBUF,1420)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 423 I = 1,IVOL
         J2 = MAX (1, IMDISK(I))
         J1 = 104 + 8 * (I-1)
         J2 = J1 + J2 - 1
         WRITE (MSGBUF,1421) I, (IOBLK(J), J = J1,J2)
         IF ((IOPT.EQ.0) .OR. (I.LT.IVOL)) THEN
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 423     CONTINUE
      IF (IOPT.EQ.0) GO TO 430
         CALL INQINT (TTYLUN, MSGBUF, 9, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            I = KK(1)
            IF ((I.LT.2) .OR. (I.GT.IVOL)) GO TO 420
            IF (KK(2).LT.-1) GO TO 420
            J1 = 104 + 8 * (I-1)
            CALL COPY (8, KK(2), IOBLK(J1))
            END IF
         GO TO 999
C                                       printer delete time
 430  WRITE (MSGBUF,1430) IOBLK(228)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 430
         IF (IERR.EQ.0) THEN
            IF ((K.LT.10) .OR. (K.GT.36000)) GO TO 430
            IOBLK(228) = K
            END IF
         GO TO 999
         END IF
C                                       Computer's speed (AIPSmark)
 440  WRITE (MSGBUF,1440) ROBLK(238)
      IF (IOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DK, IERR)
         IF (IERR.LT.0) GO TO 430
         IF (IERR.EQ.0) THEN
            IF ((DK(1).LT.0.01) .OR. (DK(1).GT.500.)) GO TO 440
            ROBLK(238) = DK(1)
            END IF
         GO TO 999
         END IF
C                                       Enough for now
 450  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('INVALID OPTION. RE-ENTER')
 1100 FORMAT ('  1  No. of AIPS data areas ("disks")   ',I6)
 1110 FORMAT ('  2  No. of tape drives                 ',I6)
 1120 FORMAT ('  3  No. of lines per CRT page          ',I6)
 1130 FORMAT ('  4  No. of lines per print page        ',I6)
 1140 FORMAT ('  5  No. of batch queues                ',I6)
 1150 FORMAT ('  6  Plotter no. of X dots per page     ',I6)
 1160 FORMAT ('  7  Plotter no. of Y dots per page     ',I6)
 1170 FORMAT ('  8  Plotter no. of X dots per character',I6)
 1180 FORMAT ('  9  Plotter no. of Y dots per character',I6)
 1190 FORMAT (' 10  No. of interactive AIPS            ',I6)
 1200 FORMAT (' 11  Max words in pseudo AP (in 1024s) ',I7)
 1210 FORMAT (' 12  No. of TV devices available        ',I6)
 1220 FORMAT (' 13  No. of graphics devices available  ',I6)
 1230 FORMAT (' 14  No. of X dots per mm on printer    ',F6.3)
 1240 FORMAT (' 15  No. of X dots per mm on tektronix  ',F6.3)
 1250 FORMAT (' 16  No. of POPS allowed access to TVs  ',I6)
 1260 FORMAT (' 17  No. of POPS allowed access to TKs  ',I6)
 1270 FORMAT (' 18  No. entries in private catalogs    ',I6)
 1280 FORMAT (' 19  Site name (A20)                    ',A20)
 1290 FORMAT (' 20  Maximum user number (<= 46655)     ',I6)
 1300 FORMAT (' 21  TIMDEST minima disks',I3,' -',I3,' days ',3F5.0)
 1310 FORMAT (' 22  TIMDEST limit: SAVE/GET files      ',F5.0)
 1320 FORMAT (' 23  TIMDEST/EXIT limit: messages       ',F5.0)
 1330 FORMAT (' 24  TIMDEST limit: scratch files       ',F5.0)
 1340 FORMAT (' 25  TIMDEST limit: empty CA files      ',F6.3)
 1350 FORMAT (' 26  No AP batch starts: weekend hours  ',2F6.3)
 1355 FORMAT ('     No AP batch starts: weekday hours  ',2F6.3)
 1360 FORMAT (' 27  AP roll interval minutes           ',F6.3)
 1365 FORMAT ('     AP patience: (1)*N+(2)*N*N minutes ',2F6.3)
 1370 FORMAT (' 28  Line printer width (72 - 132)      ',I6)
C1380 FORMAT (' 29  Pseudo-AP 2nd memory (1024s)       ',I6)
 1390 FORMAT (' 30  Max length of "short" vector       ',I6)
 1400 FORMAT (' 31  Graphics (TK) screen size: x, y    ',2I6)
 1410 FORMAT (' 32  Graphics (TK) character size: x, y ',2I6)
 1420 FORMAT (' 33  Disk & reserved users or -1 scratch (9 I)')
 1421 FORMAT ('     Disk',I3,' Users',8I5)
 1430 FORMAT (' 34  Delay time print file delete (secs)',I6)
 1440 FORMAT (' 35  Computer speed rating (AIPSmarks)  ',F6.2)
 1195 FORMAT (' *** REDUCE NUMBER BATCH QUEUES FROM',I3,' TO <=',I3)
 1287 FORMAT (' *** Registration no. ',I2,' (total ',I2,'/3) is ',A8)
 1288 FORMAT (' *** Registration failed!  UNREGISTERED status kept')
 1385 FORMAT (' *** Pseudo-AP 2nd memory limited to',I6,' not', I9)
      END
