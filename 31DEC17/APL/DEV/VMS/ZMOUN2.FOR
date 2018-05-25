      SUBROUTINE ZMOUN2 (MOUNT, IDRIVE, IDENS, NTAPED, NPOPS, NUSER,
     *   MSG, SYSERR, IERR)
C-----------------------------------------------------------------------
C! mount or dismount magnetic tape device
C# Z2 Tape
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
C   Issue software mount or dismount for a given tape drive - the lowest
C   level Z routine called by ZMOUNT.
C   Inputs:
C      MOUNT    I      Operation: 1 => mount, 0 => dismount
C      IDRIVE   I      Drive number
C      IDENS    I      Density
C      NTAPED   I      Number of tape drives
C      NPOPS    I      Pops number
C      NUSER    I      User AIPS number
C   Output:
C      MSG      C*80   Error message buffer
C      SYSERR   I      System error code (for ZERROR)
C      IERR     I      Error return code: 0 => no error
C                        1 => error
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   MOUNT, IDRIVE, IDENS, NTAPED, NPOPS, SYSERR, IERR,
     *            NUSER
      CHARACTER MSG*80
C
      INTEGER   ISTRUC(13)
      INTEGER*2 HSTRUC(26), IOSB(4)
      INTEGER   I, JERR, PID, MNTCNT, MYPID, FLAG, JDENS, MAXBLK,
     *   SYS$GETJPIW, SYS$GETDVIW, SYS$MOUNT, SYS$DISMOU
      CHARACTER DTAPE*5, PROCES*15
      HOLLERITH TAPE(2)
      INCLUDE '($MNTDEF)'
      INCLUDE '($JPIDEF)'
      INCLUDE '($DVIDEF)'
      EQUIVALENCE (ISTRUC, HSTRUC)
C-----------------------------------------------------------------------
      IERR = 0
      SYSERR = 0
      MSG = ' '
C                                       Construct character string
C                                       descriptor for tape logical name
      I = IDRIVE - 1
      DTAPE = 'MT00:'
      CALL ZEHEX (I, 1, DTAPE(4:4))
C                                       Dismount the tape
      IF (MOUNT.LE.0) THEN
         JERR = SYS$DISMOU (DTAPE,)
         IF (JERR.NE.1) SYSERR = JERR
C                                       Mount section.
      ELSE
C                                       Check for device already
C                                       mounted. VMS 3.2 gets very upset
C                                       over mounting tapes /FOR twice.
         PID = 0
         HSTRUC(1) = 4
         HSTRUC(2) = DVI$_PID
         ISTRUC(2) = %LOC (PID)
         ISTRUC(3) = 0
         HSTRUC(7) = 4
         HSTRUC(8) = DVI$_MOUNTCNT
         ISTRUC(5) = %LOC (MNTCNT)
         ISTRUC(6) = 0
         ISTRUC(7) = 0
         JERR = SYS$GETDVIW ( , , DTAPE, ISTRUC, , , ,)
C                                       Someone has tape mounted.
         IF (MNTCNT.GT.0) THEN
C                                       Get my PID
            HSTRUC(1) = 4
            HSTRUC(2) = JPI$_PID
            ISTRUC(2) = %LOC (MYPID)
            ISTRUC(3) = 0
            ISTRUC(4) = 0
            JERR = SYS$GETJPIW ( , , , ISTRUC, , ,)
C                                       I own tape.
            IF (PID.EQ.MYPID) THEN
               MSG = 'Tape drive already mounted by this process'
C                                       Someone else has tape.
            ELSE
               HSTRUC(1) = 15
               HSTRUC(2) = JPI$_PRCNAM
               ISTRUC(2) = %LOC (PROCES)
               ISTRUC(3) = 0
               ISTRUC(4) = 0
               JERR = SYS$GETJPIW ( , PID, , ISTRUC, IOSB, , )
               IF (JERR.NE.1) PROCES = 'ANOTHER USER'
               IF (IOSB(1).NE.1) PROCES = 'ANOTHER USER'
               MSG = 'TAPE DRIVE IS STILL ALLOCATED BY ' // PROCES
               IERR = 1
               END IF
            GO TO 999
C                                       Try to mount tape.
         ELSE
            CALL CHR2H (5, DTAPE, 1, TAPE)
            HSTRUC(1) = 8
            HSTRUC(2) = MNT$_DEVNAM
            ISTRUC(2) = %LOC (TAPE)
            ISTRUC(3) = 0
            FLAG = MNT$M_FOREIGN + MNT$M_MESSAGE + MNT$M_NOASSIST
            HSTRUC(7) = 4
            HSTRUC(8) = MNT$_FLAGS
            ISTRUC(5) = %LOC (FLAG)
            ISTRUC(6) = 0
            MAXBLK = 2048
            HSTRUC(13) = 4
            HSTRUC(14) = MNT$_BLOCKSIZE
            ISTRUC(8) = %LOC (MAXBLK)
            ISTRUC(9) = 0
            JDENS = IDENS
            HSTRUC(19) = 4
            HSTRUC(20) = MNT$_DENSITY
            ISTRUC(11) = %LOC (JDENS)
            ISTRUC(12) = 0
            ISTRUC(13) = 0
            JERR = SYS$MOUNT (ISTRUC)
            IF (JERR.NE.1) THEN
               SYSERR = JERR
               MSG = 'ERROR AROSE ON ACTUAL SYS$MOUNT CALL'
               IERR = 1
            ELSE
               MSG = 'Mounted on VMS tape drive'
               END IF
            END IF
         END IF
C
 999  RETURN
      END
