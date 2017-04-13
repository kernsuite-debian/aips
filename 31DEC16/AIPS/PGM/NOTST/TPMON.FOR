LOCAL INCLUDE 'TPMON.INC'
      LOGICAL   DEBUG
      INTEGER   NETBUF(16394), NTPDEV, BUFSW, BUFSR, TPFIND, TPILUN,
     *   TPLUN, THLEN, NTH, NENT
      PARAMETER (THLEN=10, NTH=512)
      HOLLERITH HETBUF(16394)
      LOGICAL   MOUNTD
C                                       See also ZVTPRO.FOR arguments
      HOLLERITH THOSTS(THLEN * NTH)
      COMMON /TPMONC/ NETBUF, NTPDEV, BUFSW, BUFSR, TPLUN, TPFIND,
     *   TPILUN, MOUNTD, DEBUG, THOSTS, NENT
      EQUIVALENCE (NETBUF, HETBUF)
LOCAL END
      PROGRAM TPMON
C-----------------------------------------------------------------------
C! drives true tape device from a distant computer
C# Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2002, 2004-2005, 2007
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
C   TPMON is intended to act as a "daemon" in that it is a program run
C   in stand-alone mode on a system.  Its purpose is to allow remote
C   systems to access local tape drives and disks.  TPMON receives tape
C   commands over a communication line (e.g., via a mailbox or socket)
C   parses them, calls the Z routines appropriate to the local tape or
C   disk devices, and returns over the communication lines the requested
C   parameters/data and an error code.
C
C   Unlike, say, the inetd and other daemons, it supports only one tape
C   device and chooses which by the process name under which it finds
C   itself running, i.e., if the process name is TPMON3 then it calls
C   for AIPS tape drive number 3.  TPMON0 is for pseudo-tape disk files
C   only.
C
C   Security is achieved via $NET0/TPHOSTS, a list of valid hosts from
C   which all TPMON daemons will accept connections.  Any connection
C   from other systems will be rejected.  A maximum of 512 hosts can be
C   extracted from TPHOSTS (set by NTH above)
C-----------------------------------------------------------------------
      INTEGER   IRET, LUN, FIND
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN /16/
C-----------------------------------------------------------------------
      DEBUG = .TRUE.
      DEBUG = .FALSE.
C                                       Create communication channel
      CALL TPMINI (LUN, FIND, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do the TP driving
      CALL TPMONI (FIND, IRET)
C                                       quit
 990  CALL TPMEXI (LUN, FIND, IRET)
C
 999  STOP
      END
      SUBROUTINE TPMINI (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   TPMINI starts up TPMON setting up the commons and the communication
C   channel (e.g., creating and opening the mailbox).
C   Inputs:
C      LUN    I      LUN to use for socket to remote machine
C   Output:
C      FIND   I      FTAB location reserved
C      IERR   I      Error code: 0 => ok, keep going
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   J, IDT(3), ITT(3), TPHLUN, TPIND, I, NDX
      CHARACTER PGM*6, TAPLOG*8, XLATED*256, HVAL*4, TNAME*20
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PGM /'TPMON '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      TSKNAM = PGM
      CALL VHDRIN
      MOUNTD = .FALSE.
      TPFIND = 0
      TPILUN = 0
      TPLUN = 0
C                                       get assigned tape (TPMONn => n)
      CALL THOAMI (TSKNAM, J, IERR)
      IF (IERR.EQ.0) THEN
         NTPDEV = J - 1
         NPOPS = J
      ELSE
         NTPDEV = 1
         NPOPS = 2
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C                                       message
      CALL ZDATE(IDT)
      CALL ZTIME(ITT)
      WRITE (MSGTXT, 1020) IDT(1),IDT(2),IDT(3), ITT(1),ITT(2),ITT(3)
      CALL MSGWRT (3)
      CALL ACOUNT (1)
C                                       Check that this is not a remote
C                                       device itself
      CALL ZEHEX (NTPDEV, 4, HVAL)
      IF (NTPDEV.GT.0) THEN
         TAPLOG = 'TAPE' // HVAL(4:4)
         CALL ZTRLOG (5, TAPLOG, 256, XLATED, J, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'unable to translate logical name ' // TAPLOG
         ELSE IF (XLATED.EQ.'REMOTE') THEN
            MSGTXT = 'TPMON may not be used on ''REMOTE'' tape devices'
         ELSE
            MSGTXT = ' '
            END IF
         IF (MSGTXT.NE.' ') THEN
            IERR = 8
            GO TO 990
            END IF
         END IF
C                                       Read in the TPHOSTS file
      TNAME = 'NET0:TPHOSTS'
      TPHLUN = 11
      CALL ZTXOPN ('QRED', TPHLUN, TPIND, TNAME, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TPMINI: Cannot open TPHOSTS file in NET0 area'
         IERR = 9
         GO TO 990
         END IF
C                                       Read the whole file in.
      NENT = 0
 20   CALL ZTXIO ('READ', TPHLUN, TPIND, XLATED, IERR)
C                                       End-of-file.
         IF (IERR.EQ.2) THEN
            IERR = 0
         ELSE IF (IERR.NE.0) THEN
            MSGTXT = 'TPMINI: Error reading TPHOSTS file'
            CALL MSGWRT (6)
         ELSE IF ((XLATED(1:1).EQ.'#') .OR. (XLATED.EQ.' ')) THEN
            GO TO 20
         ELSE
            IF (NENT.GE.NTH) THEN
               WRITE (MSGTXT, 1030) NTH
               CALL MSGWRT (6)
            ELSE
               NENT = NENT + 1
               CALL CHTRIM (XLATED, 80, XLATED, NDX)
               CALL CHLTOU (NDX, XLATED)
               CALL CHR2H (NDX, XLATED, 1, THOSTS(1+((NENT-1)*THLEN)))
               GO TO 20
               END IF
            END IF
C                                       Clear out rest of array
      XLATED = ' '
      NDX = 40
      IF (NDX.LT.NTH) THEN
         DO 30 I = NDX+1, NTH
            CALL CHR2H (NDX, XLATED, 1, THOSTS(1+((I-1)*THLEN)))
 30         CONTINUE
         END IF
C                                       Close TPHOSTS file
      CALL ZTXCLS (TPHLUN, TPIND, IERR)
C                                       call local, machine-dependent
C                                       routine: setup TP #, mailbox
      TAPLOG = 'aipsmt' // HVAL(4:4) // ':'
      CALL ZVTPRO (TAPLOG, LUN, THOSTS, THLEN, NENT, FIND, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1010) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('THOAMI error',I5,' must use defaults, incl. TP number 1')
 1010 FORMAT ('Unable to create socket.  ZVTPRO error',I6)
 1020 FORMAT ('Begins on ',I4,2('.',I2.2),' ',I2.2,2(':',I2.2))
 1030 FORMAT ('Too many entries in TPHOSTS; only reading first ', I5)
      END
      SUBROUTINE TPMEXI (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   TPMEXI is a simple close down routine for TPMON.
C   Inputs:
C      LUN    I      LUN in use for socket to remote machine
C      FIND   I      FTAB location reserved
C      IERR   I      Error code from other main subroutines
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   NLEV, JERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       close socket
      IF (FIND.GT.0) CALL ZVTPRC (LUN, FIND, JERR)
C                                       message
      IF (IERR.NE.0) THEN
         NLEV = 8
         WRITE (MSGTXT,1000) IERR
      ELSE
         NLEV = 4
         MSGTXT = 'Appears to have ended successfully'
         END IF
      CALL MSGWRT (NLEV)
C                                       accounting
      CALL ACOUNT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Purports to die of UNNATURAL CAUSE number',I7)
      END
      SUBROUTINE TPMONI (FIND, IRET)
C-----------------------------------------------------------------------
C   TPMONI actually drives the TP device: read (wait) for command;
C   parse the opcode and routine name; call the appropriate routine to
C   decode the parameters, to call the Z routine, and to encode the
C   return parameters; send the parameters back, and loop.
C   Inputs:
C      FIND   I      FTAB location reserved
C   Output:
C      IRET   I      Error code: 0 => ok
C                       else transmission has failed
C-----------------------------------------------------------------------
      INTEGER   FIND, IRET
C
      CHARACTER NAMES(5)*6, SUBNAM*6, PNAME*48
      INTEGER   IERR, INAM, NNAM, I
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA NNAM, NAMES /5, 'ZMOUNT', 'ZTAPE ', 'ZTPCLS', 'ZTPMIO',
     *   'ZTPOPN'/
C-----------------------------------------------------------------------
C                                       First time: skip return
      GO TO 50
C                                       return buffer, IERR
 10   CONTINUE
         I = BUFSR + 5
         IF (DEBUG) THEN
            WRITE (MSGTXT,4001) IERR, BUFSR
            CALL MSGWRT (2)
            END IF
         CALL ZILI32 (1, IERR, 5, NETBUF)
         CALL ZVTPRX (FIND, I, NTPDEV, THOSTS, THLEN, NENT, TPFIND,
     *      NETBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) SUBNAM, IRET
            GO TO 900
            END IF
C                                       read transmission
 50   I = 0
      CALL ZVTPRX (FIND, I, NTPDEV, THOSTS, THLEN, NENT, TPFIND, NETBUF,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 900
         END IF
C                                       unpack
      CALL ZC8CL (6, 1, HETBUF, SUBNAM)
      CALL ZI32IL (1, 3, NETBUF, BUFSW)
      CALL ZI32IL (1, 4, NETBUF, BUFSR)
      IF (DEBUG) THEN
         WRITE (MSGTXT,4000) BUFSW, BUFSR, SUBNAM
         CALL MSGWRT (2)
         END IF
      DO 70 INAM = 1,NNAM
         IF (SUBNAM.EQ.NAMES(INAM)) GO TO 80
 70      CONTINUE
      WRITE (MSGTXT,1070) SUBNAM
      CALL MSGWRT (6)
      IERR = 2
      GO TO 10
C                                       branch to interpret each
 80   GO TO (110, 120, 130, 140, 150), INAM
C-----------------------------------------------------------------------
 110  CONTINUE
         CALL AMOUNT (IERR)
         GO TO 10
 120  CONTINUE
         CALL ATAPE (IERR)
         GO TO 10
 130  CONTINUE
         CALL ATPCLS (IERR)
         GO TO 10
 140  CONTINUE
         CALL ATPMIO (IERR)
         GO TO 10
 150  CONTINUE
         CALL ZC8CL (48, 33, HETBUF, PNAME)
         IF (PNAME.EQ.'NET0:TPDEVS.LIST') THEN
            CALL ATPSHO (IERR)
         ELSE
            CALL ATPOPN (PNAME, IERR)
            END IF
         GO TO 10
C                                       Error recovery is in ZVTPRX
 900  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ON ',A6,' VTP-WRITE ERROR',I7)
 1050 FORMAT ('VTP-READ ERROR',I7)
 1070 FORMAT ('ROUTINE ',A6,' UNKNOWN - CONTINUING')
 4000 FORMAT ('NWRITE, NREAD',2I8,' operation ''',A,'''')
 4001 FORMAT ('Send back error code',I5,' with',I8,' data words')
      END
      SUBROUTINE THOAMI (INAME, INUM, IERR)
C-----------------------------------------------------------------------
C   THOAMI obtains the actual task name, compares it to the root task
C   name, and determines the number at the end, allowing 0 - F.
C   Inputs:
C      INAME  C*6   Root task name
C   Output:
C      INUM   I     Number
C      IERR   I     Error code: 0 => ok
C                      1 => illegal root
C                      2 => roots don't match
C                      3 => INUM out of range, illegal
C                      4 => other
C   Used first for TPMON.  Based on WHOAMI, but without the restriction
C   of being 1 - NINTRN + 1 + 2 * NBATQS.
C-----------------------------------------------------------------------
      CHARACTER INAME*6
      INTEGER   INUM, IERR
C
      INTEGER   IT, IP
      CHARACTER CNAME*6, CTEST*1
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Legal root?
      DO 10 IP = 1,6
         IF (INAME(IP:IP).EQ.' ') GO TO 20
 10      CONTINUE
      IERR = 1
      GO TO 999
C                                       Get real name
 20   IERR = 4
      CALL ZGNAME (CNAME, IT)
      IF (IT.NE.0) GO TO 999
C                                       Roots match?
      IP = IP - 1
      IERR = 2
      IF (IP.LT.1) GO TO 999
      IF (CNAME(1:IP).NE.INAME(1:IP)) GO TO 999
C                                       Get number
      IP = IP + 1
      CTEST = CNAME(IP:IP)
      CALL ZHEX10 (CTEST, INUM, IERR)
C
 999  RETURN
      END
      SUBROUTINE AMOUNT (IERR)
C-----------------------------------------------------------------------
C   Issue software mount or dismount for a given tape drive.
C   Inputs:
C      MOUNT    L       .TRUE. means mount, .FALSE. means dismount
C      IDRIVE   I       Tape drive number
C      IDENS    I       Density at which to mount tape (800, 1600, 6250)
C      MACHIN   C*(*)   Name of remote computer (iff TAPEn translates
C                       to REMOTE)
C      RDRIVE   I       Number of tape drive on remote machine (0->1)
C   Output:
C      IERR     I       Error return code: 0 => no error
C                          1 => error
C   Translates network data packet and mounts a tape.
C   Also check authorization.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   MOUNT, IDRIVE, IDENS, LPOPS, LUSER, SYSERR, SPOPS, SUSER
      LOGICAL   LMOUNT
      CHARACTER LMSG*80
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Unpack
      CALL ZI32IL (1, 6, NETBUF, MOUNT)
      CALL ZI32IL (1, 7, NETBUF, IDRIVE)
      CALL ZI32IL (1, 8, NETBUF, IDENS)
      CALL ZI32IL (1, 9, NETBUF, LPOPS)
      CALL ZI32IL (1, 10, NETBUF, LUSER)
      SYSERR = 0
      LMOUNT = MOUNT.EQ.1
C                                       Check
      IF (IDRIVE.NE.NTPDEV) THEN
         WRITE (MSGTXT,1000) IDRIVE, NTPDEV
         IERR = 99
         CALL MSGWRT (8)
         LMSG = MSGTXT
      ELSE IF ((BUFSW.NE.5) .OR. (BUFSR.NE.21)) THEN
         WRITE (MSGTXT,1001) BUFSW, BUFSR
         IERR = 98
         CALL MSGWRT (8)
         LMSG = MSGTXT
      ELSE IF (NTPDEV.LE.0) THEN
         MSGTXT = 'AMOUNT: PSEUDO-TAPE DISK IS NOT MOUNTED'
         IERR = 97
         CALL MSGWRT (8)
         LMSG = MSGTXT
      ELSE IF ((MOUNTD) .AND. (LMOUNT)) THEN
         MSGTXT = 'AMOUNT: TAPE IS ALREADY MOUNTED BY TPMON'
         IERR = 96
         CALL MSGWRT (8)
         LMSG = MSGTXT
      ELSE IF ((.NOT.MOUNTD) .AND. (.NOT.LMOUNT)) THEN
         MSGTXT = 'AMOUNT: TAPE WAS NOT MOUNTED BY TPMON'
         IERR = 96
         CALL MSGWRT (8)
         LMSG = MSGTXT
C                                       Do
      ELSE
         SPOPS = NPOPS
C         NPOPS = LPOPS
         SUSER = NLUSER
C         NLUSER = LUSER
         LMSG = ' '
         SYSERR = 0
         CALL ZMOUNT (LMOUNT, IDRIVE, IDENS, ' ', 0, LMSG, SYSERR, IERR)
         NPOPS = SPOPS
         NLUSER = SUSER
         IF (IERR.EQ.0) THEN
            MOUNTD = LMOUNT
            WRITE (MSGTXT,1010) LUSER
            CALL MSGWRT (2)
         ELSE
            WRITE (MSGTXT,1900) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Pack returns
      CALL ZILI32 (1, SYSERR, 6, NETBUF)
      CALL ZCLC8 (80, LMSG, 25, HETBUF)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AMOUNT: drive',I4,' not supported by TPMON for drive #',
     *   I3)
 1001 FORMAT ('AMOUNT: BUFSW, BUFSR =',2I4,' not the expected 5 and 21')
 1010 FORMAT ('AMOUNT: done for user',I6)
 1900 FORMAT ('AMOUNT: ZMOUNT RETURNS ERROR',I5)
      END
      SUBROUTINE ATAPE (IERR)
C-----------------------------------------------------------------------
C   Performs standard tape manipulating functions.
C   Inputs:
C       OP      C*4   Operation to be performed. 4 characters ASCII.
C                     'ADVF' = advance file marks
C                     'ADVR' = advance records
C                     'BAKF' = backspace file marks.
C                     'BAKR' = backspace records.
C                     'REWI' = rewind the tape on unit LUN
C                     'WEOF' = write end of file on unit LUN: writes 4
C                              EOFs, positions tape after first one
C                     'MEOF' = write 4 EOF marks on tape, position tape
C                              before the first one
C                     'EOM'  = advance to end-of-medium: WARNING - NOT
C                        SUPPORTED IN ALL VERSIONS
C       LUN     I     logical unit number == 129 - DriveNumber
C       FIND    I     FTAB pointer.  Drive number for MOUNT/DISMOUNT.
C       COUNT   I     Number of records or file marks to skip. On MOUNT
C                     this value is the density.
C   Outputs:
C       IERR  I       Error return: 0 => ok
C                        1 = File not open
C                        2 = Input specification error.
C                        3 = I/O error.
C                        4 = End Of File
C                        5 = Beginning Of Medium
C                        6 = End Of Medium
C   Translates network data packet and moves the tape.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, COUNT
      CHARACTER OP*4
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Unpack
      CALL ZC8CL (4, 21, HETBUF, OP)
      CALL ZI32IL (1, 7, NETBUF, LUN)
      CALL ZI32IL (1, 8, NETBUF, COUNT)
C                                       Check
      IF (LUN.NE.TPILUN) THEN
         WRITE (MSGTXT,1000) LUN, TPILUN
         IERR = 99
         CALL MSGWRT (8)
      ELSE IF ((BUFSW.NE.3) .OR. (BUFSR.NE.1)) THEN
         WRITE (MSGTXT,1001) BUFSW, BUFSR
         IERR = 98
         CALL MSGWRT (8)
      ELSE IF (NTPDEV.LE.0) THEN
         MSGTXT = 'ATAPE: ZTAPE NOT USED ON PSEUDO-TAPE DISK FILES'
         IERR = 97
         CALL MSGWRT (8)
      ELSE IF ((.NOT.MOUNTD) .AND. (NTPDEV.GT.0)) THEN
         MSGTXT = 'ATAPE: TAPE IS NOT MOUNTED'
         IERR = 96
         CALL MSGWRT (8)
      ELSE IF (TPFIND.LE.0) THEN
         MSGTXT = 'ATAPE: TAPE NOT OPENED BY TPMON'
         IERR = 95
         CALL MSGWRT (8)
C                                       Do
      ELSE
         CALL ZTAPE (OP, TPLUN, TPFIND, COUNT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR, OP
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Pack returns
      CALL ZILI32 (1, COUNT, 6, NETBUF)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ATAPE: LUN',I4,' NOT THAT OPENED (=',I3,')')
 1001 FORMAT ('ATAPE: BUFSW, BUFSR =',2I4,' not the expected 3 and 1')
 1900 FORMAT ('ATAPE: ZTAPE RETURNS ERROR',I5,' ON ',A)
      END
      SUBROUTINE ATPCLS (IERR)
C-----------------------------------------------------------------------
C   Close the tape drive associated with LUN as well as its disk control
C   file removing any exclusive use state and clear the corresponding
C   FTAB entries.  ZTPCL2 actually closes the tape drive and ZDACLS is
C   called to close the disk control file.  Also closes sequential type
C   disk files via ZTPCLD.
C   Inputs:
C      LUN    I   Logical unit number = 129 - DriveNumber for tape
C      FIND   I   Index in FTAB to file control block for LUN
C   Output:
C      IERR   I   Error return code: 0 => no error
C                    1 => close error
C                    2 => non-zero LSERCH error
C                    3 => both 1 and 2
C                    4 => invalid LUN
C   Translates network data packet and closes tape.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Unpack
      CALL ZI32IL (1, 6, NETBUF, LUN)
C                                       Check
      IF (LUN.NE.TPILUN) THEN
         WRITE (MSGTXT,1000) LUN, TPILUN
         IERR = 99
         CALL MSGWRT (8)
      ELSE IF ((BUFSW.NE.1) .OR. (BUFSR.NE.0)) THEN
         WRITE (MSGTXT,1001) BUFSW, BUFSR
         IERR = 98
         CALL MSGWRT (8)
      ELSE IF ((.NOT.MOUNTD) .AND. (NTPDEV.GT.0)) THEN
         MSGTXT = 'ATAPE: TAPE IS NOT MOUNTED'
         IERR = 96
         CALL MSGWRT (8)
      ELSE IF (TPFIND.LE.0) THEN
         MSGTXT = 'ATAPE: TAPE NOT OPENED BY TPMON'
         IERR = 95
         CALL MSGWRT (8)
C                                       Do
      ELSE
         CALL ZTPCLS (TPLUN, TPFIND, IERR)
         TPFIND = 0
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Pack returns
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ATAPE: LUN',I4,' NOT THAT OPENED (=',I3,')')
 1001 FORMAT ('ATPCLS: BUFSW, BUFSR =',2I4,' NOT THE EXPECTED 1 and 0')
 1900 FORMAT ('ATPCLS: ZTPCLS RETURNS ERROR',I5)
      END
      SUBROUTINE ATPMIO (IERR)
C-----------------------------------------------------------------------
C   Low level sequential access, large record, double buffered tape
C   device I/O.
C   Inputs:
C      OPER     C*4    Operation code 'READ' or 'WRIT'
C      LUN      I      Logical unit number == 129 - DriveNumber
C      FIND     I      Index in FTAB to file control block for LUN
C      NBYTES   I      Number of 8-bit bytes to transfer
C      BUFF     I(*)   I/O buffer
C      IBUFF    I      Buffer number to use (1 or 2)
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => file not open
C                         2 => input error
C                         3 => I/O error
C                         4 => end of file (no messages)
C   Translates network data packet and does IO.  Note that this one
C   forces the IO to be wait-mode via a call to ZTPWAT.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN, NBYTES, BUFNUM, BYTXFR, BUFFER(16384), ND, IO, IL,
     *   FCBOFF, RECNO
      CHARACTER OPER*4
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Unpack
      CALL ZC8CL (4, 21, HETBUF, OPER)
      CALL ZI32IL (1, 7, NETBUF, LUN)
      CALL ZI32IL (1, 8, NETBUF, NBYTES)
      CALL ZI32IL (1, 9, NETBUF, BUFNUM)
      CALL ZI32IL (1, 10, NETBUF, RECNO)
C                                       Check
      IF (LUN.NE.TPILUN) THEN
         WRITE (MSGTXT,1000) LUN, TPILUN
         IERR = 99
         CALL MSGWRT (8)
      ELSE IF ((BUFSW.NE.5) .AND. (OPER.EQ.'READ')) THEN
         WRITE (MSGTXT,1001) BUFSW
         IERR = 98
         CALL MSGWRT (8)
      ELSE IF ((BUFSR.NE.3) .AND. (OPER.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1001) BUFSR
         IERR = 98
         CALL MSGWRT (8)
      ELSE IF ((.NOT.MOUNTD) .AND. (NTPDEV.GT.0)) THEN
         MSGTXT = 'ATPMIO: TAPE IS NOT MOUNTED'
         IERR = 96
         CALL MSGWRT (8)
      ELSE IF (TPFIND.LE.0) THEN
         MSGTXT = 'ATPMIO: TAPE NOT OPENED BY TPMON'
         IERR = 95
         CALL MSGWRT (8)
      ELSE IF ((OPER.NE.'READ') .AND. (OPER.NE.'WRIT')) THEN
         MSGTXT = 'ATPMIO: UNKNOWN OPER = ''' // OPER // ''''
         IERR = 94
         CALL MSGWRT (8)
C                                       Do
      ELSE
         FTAB(TPFIND+5) = RECNO
         IL = NBITWD / 8
         IO = 39 / IL + 2
         ND = (NBYTES - 1) / IL + 1
         BYTXFR = 0
         IF (OPER.EQ.'WRIT') CALL COPY (ND, NETBUF(IO), BUFFER)
         CALL ZTPMIO (OPER, TPLUN, TPFIND, NBYTES, BUFFER, BUFNUM, IERR)
         IF (IERR.EQ.0) THEN
            CALL ZTPWAT (TPLUN, TPFIND, BUFNUM, BYTXFR, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1900) 'ZTPWAT', IERR
               CALL MSGWRT (7)
               END IF
         ELSE
            WRITE (MSGTXT,1900) 'ZTPMIO', IERR
            CALL MSGWRT (7)
            END IF
C                                       Pack returns
         FCBOFF = TPFIND + MOFF + (BUFNUM-1) * MFCB
         CALL ZILI32 (1, FTAB(FCBOFF+FCBERR), 6, NETBUF)
         CALL ZILI32 (1, BYTXFR, 7, NETBUF)
         CALL ZILI32 (1, FTAB(TPFIND+5), 8, NETBUF)
         IF (OPER.EQ.'READ') THEN
            IF (IERR.NE.0) THEN
               BUFSR = 3
            ELSE
               IO = 31 / IL + 2
               ND = (BYTXFR - 1) / IL + 1
               CALL COPY (ND, BUFFER, NETBUF(IO))
               BUFSR = 4 + (BYTXFR - 1) / 4
               END IF
            CALL ZILI32 (1, BUFSR, 4, NETBUF)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ATAPE: LUN',I4,' NOT THAT OPENED (=',I3,')')
 1001 FORMAT ('ATPMIO: BUFSW =',I4,' NOT THE EXPECTED 5')
 1900 FORMAT ('ATPMIO: ',A,' RETURNS ERROR',I5)
      END
      SUBROUTINE ATPOPN (PNAME, IERR)
C-----------------------------------------------------------------------
C   Open a tape drive (as well as its corresponding disk control file)
C   for sequential, "map" (double buffered, asynchronous) I/O or open
C   a pseudo-tape sequential disk file.  Exclusive use and wait to open
C   are assumed.  Uses a 'TP' disk "lock" file for real tapes.
C   Inputs:
C      LUN     I       Logical unit number (129 - NTAPED <= LUN < 129)
C                         => tape, else disk)
C      IVOL    I       Tape drive
C      PNAME   C*48    tape disk physical file name
C      OPER    C*4     'READ' => read only or 'WRIT' => read/write
C   Output:
C      FIND    I       Index in FTAB to file control block for LUN
C      IERR    I       Error return code: 0 => no error
C                         1 => LUN already in use
C                         2 => file not found
C                         3 => volume not found
C                         4 => exclusive use denied
C                         5 => no room for LUN in FTAB
C                         6 => other open errors
C   Translates network data packet and opens tape.
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PNAME*48
C
      INTEGER   LUN, IDRIVE
      CHARACTER OPER*4
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Unpack
      CALL ZI32IL (1, 6, NETBUF, LUN)
      CALL ZI32IL (1, 7, NETBUF, IDRIVE)
      CALL ZC8CL (4, 29, HETBUF, OPER)
C                                       LUN, IDRIVE are those of the
C                                       remote machine so can't check
C                                       Check
      IF ((NTPDEV.GT.0) .AND. (LUN.NE.129-IDRIVE)) THEN
         WRITE (MSGTXT,1001) LUN, IDRIVE
         IERR = 99
         CALL MSGWRT (8)
      ELSE IF ((BUFSW.NE.15) .OR. (BUFSR.NE.0)) THEN
         WRITE (MSGTXT,1002) BUFSW, BUFSR
         IERR = 98
         CALL MSGWRT (8)
      ELSE IF ((.NOT.MOUNTD) .AND. (NTPDEV.GT.0)) THEN
         MSGTXT = 'ATPOPN: TAPE IS NOT MOUNTED'
         IERR = 96
         CALL MSGWRT (8)
      ELSE IF (TPFIND.GT.0) THEN
         MSGTXT = 'ATPOPN: TAPE ALREADY OPENED BY TPMON'
         IERR = 95
         CALL MSGWRT (8)
C                                       Do
      ELSE
         TPILUN = LUN
         IF ((LUN.GE.94) .AND. (LUN.LT.129)) THEN
            TPLUN = 129 - NTPDEV
         ELSE
            TPLUN = LUN
            END IF
         CALL ZTPOPN (TPLUN, TPFIND, NTPDEV, PNAME, OPER, IERR)
         IF (IERR.NE.0) THEN
            TPFIND = 0
            WRITE (MSGTXT,1900) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ATPOPN: CLIENT LUN =',I3,' NOT RIGHT FOR DRIVE',I3)
 1002 FORMAT ('ATPOPN: BUFSW, BUFSR =',2I4,' NOT THE EXPECTED 15 and 0')
 1900 FORMAT ('ATPOPN: ZTPOPN RETURNS ERROR',I5)
      END
      SUBROUTINE ATPSHO (IERR)
C-----------------------------------------------------------------------
C   ATPSHO RETURNS the device name and description of all local tape
C   drives
C   Output:
C      IERR     I       Zero if all is well, 1 otherwise
C   PPM 19981202: cloned from PRINTR.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   IER2, ITP2, LUN, FIND, TLEN, JTRIM, IP
      LOGICAL   DUM
      CHARACTER MYHOST*12, LINE*80, AWORD*12
      INCLUDE 'TPMON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get name of this host (upcase)
      MYHOST = HSTNAM
      CALL CHLTOU (12, MYHOST)
C                                       For all requests, we try to get
C                                       info from TPDEVS.LIST in NET0:
      LUN = 3
      CALL ZTXOPN ('QRED', LUN, FIND, 'NET0:TPDEVS.LIST', DUM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         IERR = 1
         GO TO 990
         END IF
C                                       Initialise line counter
      ITP2 = 0
      IP = 21
C                                       Read loop
 100  CONTINUE
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
C                                       End of file?
         IF (IERR.EQ.2) GO TO 200
C                                       Other problem?
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) 1+ITP2, IERR
            CALL MSGWRT (7)
            GO TO 200
            END IF
C                                       Get rid of nulls
         TLEN = JTRIM (LINE)
C                                       Skip white space and comments
         IF (LINE.EQ.' ') GO TO 100
         IF (LINE(1:1).EQ.'#') GO TO 100
C                                       If this is not the right host,
C                                       skip it.
         CALL WORD (LINE, AWORD)
         CALL CHLTOU (12, AWORD)
         IF (AWORD.NE.MYHOST) GO TO 100
C                                       Otherwise, save details
         CALL ZCLC8 (80, LINE, IP, HETBUF)
         IP = IP + 80
         GO TO 100
C                                       End of loop: close the file
C                                       Reset EOF (expected) to no error
 200  IF (IERR.EQ.2) IERR = 0
      CALL ZTXCLS (LUN, FIND, IER2)
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT ('CANNOT OPEN TPDEVS.LIST FILE IN NET0, ERROR ',I5)
 1100 FORMAT ('ERROR READING TPDEVS.LIST LINE ',I5,' CODE ',I5)
      END
