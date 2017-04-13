      SUBROUTINE ZOPEN (LUN, FIND, IVOL, PNAME, MAP, EXCL, WAIT, IERR)
C-----------------------------------------------------------------------
C! open binary disk files and TTY devices
C# Z IO-basic
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
C   Open a binary disk file or tty.  Message files, line printers, text
C   files, tape devices, Tektronix devices and TV devices are NOT opened
C   using this routine (see ZMSGOP for message files, ZTOPEN for text
C   files, ZTPOPN for tape devices, ZTKOPN for Tektronix devices and the
C   device specific routine for TV devices, e.g., ZM70OP).  See LPOPEN
C   for a standard way to open line printers.
C   Inputs:
C      LUN     I      Logical unit number
C      IVOL    I      Disk volume containing file, 1,2,3,...
C      PNAME   C*48   Physical file name (from ZPHFIL)
C      MAP     L      Is this a "map" file?
C      EXCL    L      Exclusive use requested?
C      WAIT    L      Wait for exclusive use?
C   Output:
C      FIND    I      Index in FTAB to file control block for LUN
C      IERR    I      Error return code: 0 => no error
C                        1 => LUN already in use
C                        2 => file not found
C                        3 => volume/logical not found
C                        4 => exclusive use denied
C                        5 => no room for LUN in FTAB
C                        6 => other open errors
C   Generic version - uses ZDAOPN, ZLPOPN, ZTTOPN
C   No longer opens TV devices as of the 15MAR84 release.
C   No longer opens tape devices as of the 15APR87 release.
C   No longer opens Tektronix devices as of the 15OCT87 release.
C   No longer opens text files as of the 15OCT87 release.
C   No longer opens line printers as of the 15JAN91 release.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, IERR
      CHARACTER PNAME*(*)
      LOGICAL   MAP, EXCL, WAIT
C
      INTEGER   JERR, ERRLUN, ERRTER, FCBOFF, IMAP, IEXCL, DELMAX,
     *   DELCNT, DLMAX2, DLCNT2, I, J
      REAL      TDELAY
      HOLLERITH ANAME(12)
      LOGICAL   IAMOK
      CHARACTER CTEMP*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA ERRLUN, ERRTER, TDELAY, DELMAX, DLMAX2 /12, 6, 1.0, 30, 60/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 6
C                                       Valid LUN?
      IF ((LUN.NE.ERRLUN) .AND. (LUN.NE.ERRTER) .AND. (LUN.GT.0) .AND.
     *   (LUN.LE.50)) GO TO 10
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Does not handle TVs
 10   IF (DEVTAB(LUN).NE.4) GO TO 20
         WRITE (MSGTXT,1010)
         GO TO 995
C                                       Does not handle text files
 20   IF (DEVTAB(LUN).NE.3) GO TO 30
         WRITE (MSGTXT,1020)
         GO TO 995
C                                       Does not handle Tektronix I/O
 30   IF (LUN.NE.7) GO TO 40
         WRITE (MSGTXT,1030)
         GO TO 995
C                                       Does not handle tapes
 40   IF ((DEVTAB(LUN).NE.2) .AND. (PNAME(1:2).NE.'MT')) GO TO 50
         WRITE (MSGTXT,1040) LUN
         IF (PNAME(1:2).EQ.'MT') WRITE (MSGTXT,1041)
         GO TO 995
C                                       Allocate area in FTAB for LUN.
 50   IF ((LUN.EQ.5) .AND. (DEVTAB(5).EQ.1)) GO TO 80
      CALL LSERCH ('OPEN', LUN, FIND, MAP, JERR)
      IF (JERR.EQ.0) GO TO 60
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            IERR = 1
            WRITE (MSGTXT,1051) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            IERR = 5
            WRITE (MSGTXT,1052) LUN
         ELSE
            WRITE (MSGTXT,1050) JERR, 'OPEN', LUN, MAP
            END IF
         GO TO 995
C                                       Ok, so far.
 60   IERR = 0
      DELCNT = 0
      DLCNT2 = 0
C                                       Set integer map, exclusive use
      IMAP = 0
      IF (MAP) IMAP = 1
      IEXCL = 0
      IF (EXCL) IEXCL = 1
C                                       Disk file requiring map or non-
C                                       map file control block in FTAB?
      IF (DEVTAB(LUN).NE.0) GO TO 80
C                                       Determine offset in FTAB for the
C                                       file control block.
         IF (MAP) THEN
            FCBOFF = FIND + MOFF
         ELSE
            FCBOFF = FIND + NMOFF
            END IF
C                                       Volume (disk) number in range?
         IF ((IVOL.GT.0) .AND. (IVOL.LE.NVOL)) GO TO 65
            IERR = 3
            WRITE (MSGTXT,1060) IVOL
            GO TO 900
C                                       Check privilege
 65      I = INDEX (PNAME, ':')
         IF (I.GT.0) THEN
            CTEMP = PNAME(I+1:)
            IF (.NOT.IAMOK(IVOL,CTEMP)) THEN
               WRITE (MSGTXT,1065) CTEMP, IVOL, NLUSER
               IERR = 2
               GO TO 900
               END IF
            END IF
C                                       Open disk file.
 70      CALL ZDAOPN (FTAB(FCBOFF), PNAME, IMAP, IEXCL, IERR)
         IF (IERR.EQ.0) GO TO 100
C                                       File not found?
            IF (IERR.EQ.2) THEN
               WRITE (MSGTXT,1070) PNAME(1:24)
C                                       Volume not found?
            ELSE IF (IERR.EQ.3) THEN
               WRITE (MSGTXT,1071) PNAME(1:28)
C                                       File busy?
            ELSE IF (IERR.EQ.4) THEN
C                                       Wait and try again every TDELAY
C                                       seconds up to DELMAX*DLMAX2
C                                       times.  Issue a message every
C                                       DLMAX2 wait.
               IF (.NOT.WAIT) GO TO 73
                  DELCNT = DELCNT + 1
                  IF (DELCNT.LE.DELMAX) THEN
                     CALL ZDELAY (TDELAY, IERR)
                  ELSE
                     DLCNT2 = DLCNT2 + 1
                     IF (DLCNT2.GT.DLMAX2) GO TO 73
                        DELCNT = 0
                        WRITE (MSGTXT,1072) PNAME(1:24)
                        CALL MSGWRT (2)
                     END IF
                  GO TO 70
 73            WRITE (MSGTXT,1073) PNAME(1:24)
C                                       Other open error.
            ELSE
               CALL ZERROR ('ZDAOPN', FTAB(FCBOFF+FCBERR), PNAME,
     *            FTAB(FCBOFF), MAP)
               GO TO 910
               END IF
            GO TO 900
C                                       Open a Fortran device here.
 80   IF (DEVTAB(LUN).NE.1) GO TO 999
C                                       Printer?
         IF (LUN.EQ.1) THEN
            MSGTXT = 'ZOPEN: DOES NOT OPEN THE LINE PRINTER AS OF ' //
     *         '15JAN91'
            IERR = 6
            GO TO 900
C                                       Terminal?
         ELSE IF (LUN.EQ.5) THEN
            CALL ZTTOPN (LUN, FIND, IERR)
            IF (IERR.EQ.0) GO TO 100
               IF (FTAB(FIND+1+NFCBER).GT.0) CALL ZERROR ('ZTTOPN',
     *            FTAB(FIND+1+NFCBER), ' ', -999, .FALSE.)
               WRITE (MSGTXT,1080) IERR
               IERR = 6
               GO TO 900
C                                       Invalid DEVTAB(LUN).
         ELSE
            WRITE (MSGTXT,1090) LUN, DEVTAB(LUN)
            IERR = 6
            END IF
         GO TO 900
C                                       store file name
 100  CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = PNAME
         IF (I.EQ.2) NONNAM(J) = PNAME
         IF (I.EQ.3) MAPNAM(J) = PNAME
      ELSE
         WRITE (MSGTXT,1100) FIND, JERR
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 900  CALL MSGWRT (6)
C                                       Remove FTAB entry for LUN.
 910  IF (FIND.LE.0) GO TO 999
      CALL LSERCH ('CLOS', LUN, FIND, MAP, JERR)
      IF (JERR.EQ.0) GO TO 999
         IERR = IERR + 2
         WRITE (MSGTXT,1050) JERR, 'CLOS', LUN, MAP
C
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZOPEN: INVALID LUN = ',I2)
 1010 FORMAT ('ZOPEN: DOES NOT PERFORM TV OPENS AS OF 15MAR84')
 1020 FORMAT ('ZOPEN: DOES NOT PERFORM TEXT FILE OPENS AS OF 15OCT87')
 1030 FORMAT ('ZOPEN: DOES NOT PERFORM TEKTRONIX OPENS AS OF 15OCT87')
 1040 FORMAT ('ZOPEN: DOES NOT PERFORM DEVICE OPENS; LUN =',I5)
 1041 FORMAT ('ZOPEN: DOES NOT PERFORM TAPE OPENS AS OF 15APR87')
 1050 FORMAT ('ZOPEN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2,
     *   ' MAP = ',L1)
 1051 FORMAT ('ZOPEN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1052 FORMAT ('ZOPEN: NO ROOM IN FTAB FOR LUN = ',I2)
 1060 FORMAT ('ZOPEN: VOLUME NUMBER = ',I6,' OUT OF RANGE')
 1065 FORMAT ('ZOPEN: ',A2,' FILES ON DISK',I3,' NOT ALLOWED FOR USER',
     *   I5)
 1070 FORMAT ('ZOPEN: FILE ',A24,' NOT FOUND')
 1071 FORMAT ('ZOPEN: TROUBLE FINDING PATH TO FILE ',A)
 1072 FORMAT ('ZOPEN: STILL WAITING FOR FILE ',A24)
 1073 FORMAT ('ZOPEN: FILE ',A24,' NOT AVAILABLE')
 1080 FORMAT ('ZOPEN: ZTTOPN RETURNS ERROR',I5)
 1090 FORMAT ('ZOPEN: INVALID DEVTAB(',I2,') = ',I1)
 1100 FORMAT ('ZOPEN WARNING: FIND =',I5,' FSERCH ERR=',I5)
      END
