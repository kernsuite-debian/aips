      SUBROUTINE ACOUNT (IOP)
C-----------------------------------------------------------------------
C! Writes beginning and final entries in the AIPS accounting file
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2002, 2004, 2007
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
C   ACOUNT updates the AIPS accounting file.
C   Inputs:
C      IOP   I         1 => starting program, 2 => ending program
C                      3 => ending via abort from system/AIPS
C   Common:
C      /MSGCOM/ uses NLUSER, TSKNAM, NPOPS
C               sets (IOP=1), uses (IOP=2,3) NACOUN
C   Format of account file entry:
C      1 - 2  Task name Hollerith (4 chars/wd)
C          3  NPOPS + 100 * (1,2,3,4 as RLSNAM => OLD,NEW,TST,CVX)
C          4  NLUSER
C      5 - 6  Start time yy/mm/dd hh/mm/ss
C          7  IO count (true I)
C          8  Real time (floating)
C          9  CPU time (floating)
C-----------------------------------------------------------------------
      INTEGER   IOP
C
      INTEGER   IERR, NWPL, NLPR, NP, ALUN, AIND, IT(6), NREC, LREC,
     *   MREC, IOCNT, IOCNT0, IEQUIV, IV, ITRIM, IP, IH, IR
      REAL      RC, TI, RR, REQUIV
      LOGICAL   T, F, EQUAL
      CHARACTER PHNAME*48, READ*4, WRITE*4, AC*2, CHTEMP*6, CHVER(5)*3
      DOUBLE PRECISION    JD0, JD1, DMBYT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSUMIO.INC'
      INCLUDE 'INCS:DBBF.INC'
      EQUIVALENCE (IEQUIV, REQUIV)
      DATA AC, ALUN /'AC', 13/
      DATA T, F /.TRUE.,.FALSE./
      DATA READ, WRITE /'READ','WRIT'/
      DATA CHVER /'???','OLD','NEW','TST','CVX'/
C-----------------------------------------------------------------------
C                                       Open file, read rec 1
      CALL ZPHFIL (AC, 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (ALUN, AIND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      CALL ZFIO (READ, ALUN, AIND, 1, BBUFR1, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Pick up file descriptors
      LREC = BBUFR1(1)
      MREC = BBUFR1(2)
      NWPL = BBUFR1(3)
      NLPR = BBUFR1(4)
      IF ((NWPL.NE.9) .OR. (LREC.GT.MREC) .OR. (NLPR.NE.256/NWPL)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (10)
         GO TO 980
         END IF
C                                       Task ending now
      IF (((IOP.EQ.2) .OR. (IOP.EQ.3)) .AND. ((NACOUN.GE.1) .AND.
     *   (NACOUN.LE.LREC))) THEN
C                                       get record
         NREC = NACOUN / NLPR + 1
         IF (NREC.NE.1) THEN
            CALL ZFIO (READ, ALUN, AIND, NREC, BBUFR1, IERR)
            IF (IERR.NE.0) GO TO 960
            END IF
C                                       Double check identity
         NP = (NACOUN - (NREC-1)*NLPR) * NWPL + 1
         CALL H2CHR (6, 1, BBUFR1(NP), CHTEMP)
         EQUAL = TSKNAM(1:6).EQ.CHTEMP(1:6)
         EQUAL = EQUAL .AND. (NPOPS+100*MSGVER.EQ.BBUFR1(NP+2))
         EQUAL = EQUAL .AND. (NLUSER.EQ.BBUFR1(NP+3))
         IF (.NOT.EQUAL) THEN
            MSGTXT = 'ACOUNT: CLOSE PARAMETERS DON''T MATCH OPEN'
            CALL MSGWRT (6)
            IV = BBUFR1(NP+2) / 100 + 1
            IF ((IV.LE.0) .OR. (IV.GT.5)) IV = 1
            IP = MOD (BBUFR1(NP+2), 100)
            WRITE (MSGTXT,1030,ERR=30) 'Open ', BBUFR1(NP+3), IP,
     *         CHVER(IV), CHTEMP(:6)
 30         CALL MSGWRT(6)
            IV = MSGVER + 1
            IF ((IV.LE.0) .OR. (IV.GT.5)) IV = 1
            WRITE (MSGTXT,1030,ERR=970) 'Close', NLUSER, NPOPS,
     *         CHVER(IV), TSKNAM(:6)
            GO TO 970
            END IF
C                                       Insert final times: CPU
         IEQUIV = BBUFR1(NP+8)
         CALL ZCPU (TI, IOCNT)
         REQUIV = TI - REQUIV
         IF (IOP.EQ.3) REQUIV = -REQUIV
         BBUFR1(NP+8) = IEQUIV
         RC = REQUIV
C                                       Insert final times: real
         CALL CATIME (2, BBUFR1(NP+4), IT)
         CALL DAT2JD (IT, JD0)
         CALL ZDATE (IT(1))
         CALL ZTIME (IT(4))
         CALL DAT2JD (IT, JD1)
         RR = (JD1 - JD0) * 8.64D4
         IF (IOP.EQ.3) RR = -RR
         REQUIV = RR
         BBUFR1(NP+7) = IEQUIV
C                                       IO count
         IOCNT0 = BBUFR1(NP+6)
         IOCNT = IOCNT - IOCNT0
         DMBYT = (NRBYTE(1) + NWBYTE(1) + NRBYTE(2) + NWBYTE(2)) /
     *      (1024.0D0 * 1024.0D0)
         IOCNT = DMBYT + 0.5D0
         BBUFR1(NP+6) = IOCNT
         IF (IOP.EQ.2) THEN
            IV = MSGVER + 1
            IF ((IV.LE.0) .OR. (IV.GT.5)) IV = 1
            IP = RR + 0.5
            IH = ITRIM (HSTNAM)
            IR = ITRIM (RLSNAM)
            IF (IOCNT.GT.0) THEN
               WRITE (MSGTXT,1040,ERR=900) HSTNAM(1:MIN(8,IH)),
     *            RLSNAM(1:MIN(7,IR)), CHVER(IV), RC, IP, IOCNT
            ELSE
               WRITE (MSGTXT,1041,ERR=900) HSTNAM, RLSNAM(:7),
     *            CHVER(IV), RC, IP
               END IF
            CALL MSGWRT (5)
            IF (DBGAIP.GT.0) THEN
               WRITE (MSGTXT,1045) 'ZMIO READ', NRCOUN(1), NRBYTE(1)
               CALL MSGWRT (5)
               WRITE (MSGTXT,1045) 'ZMIO WRIT', NWCOUN(1), NWBYTE(1)
               CALL MSGWRT (5)
               WRITE (MSGTXT,1045) 'ZFIO READ', NRCOUN(2), NRBYTE(2)
               CALL MSGWRT (5)
               WRITE (MSGTXT,1045) 'ZFIO WRIT', NWCOUN(2), NWBYTE(2)
               CALL MSGWRT (5)
               END IF
            END IF
C                                       New entry
      ELSE
         NACOUN = LREC + 1
         BBUFR1(1) = NACOUN
C                                       set MSGVER
         CALL ZVERSN
C                                       Extend file
         IF (NACOUN.GT.MREC) THEN
            NP = 16
            CALL ZEXPND (ALUN, 1, PHNAME, NP, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR
               GO TO 970
               END IF
            MREC = MREC + NP * NLPR
            BBUFR1(2) = MREC
            END IF
C                                       Put back record 1
         NREC = NACOUN / NLPR + 1
         NP = (NACOUN - (NREC-1)*NLPR) * NWPL + 1
         IF (NREC.NE.1) THEN
            CALL ZFIO (WRITE, ALUN, AIND, 1, BBUFR1, IERR)
            IF (IERR.NE.0) GO TO 960
            IF (NP.GT.1) THEN
               CALL ZFIO (READ, ALUN, AIND, NREC, BBUFR1, IERR)
               IF (IERR.NE.0) GO TO 960
               END IF
            END IF
C                                       Insert parms
         CALL CHR2H (6, TSKNAM, 1, BBUFR1(NP))
         BBUFR1(NP+2) = NPOPS + 100 * MSGVER
         BBUFR1(NP+3) = NLUSER
         CALL CATIME (1, BBUFR1(NP+4), IT)
         CALL ZCPU (REQUIV, BBUFR1(NP+6))
         BBUFR1(NP+8) = IEQUIV
         REQUIV = 0.0
         BBUFR1(NP+7) = IEQUIV
         NRCOUN(1) = 0
         NWCOUN(1) = 0
         NRBYTE(1) = 0.0D0
         NWBYTE(1) = 0.0D0
         NRCOUN(2) = 0
         NWCOUN(2) = 0
         NRBYTE(2) = 0.0D0
         NWBYTE(2) = 0.0D0
         END IF
C                                       Write rec back
 900  CALL ZFIO (WRITE, ALUN, AIND, NREC, BBUFR1, IERR)
      IF (IERR.EQ.0) GO TO 980
C                                       Errors
 960  WRITE (MSGTXT,1960) IERR
 970  CALL MSGWRT (6)
C                                       Close
 980  CALL ZCLOSE (ALUN, AIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACCOUNT: OPEN ERROR',I7)
 1020 FORMAT ('ACCOUNT FILE DAMAGED.  NOTIFY THE AIPS ',
     *   'MANAGER IMMEDIATELY')
 1030 FORMAT ('At ',A,' user',I7,' pops',I4,' vers ',A3,' task ',A6)
 1040 FORMAT (A,' ',A,' ',A3,': Cpu=',F9.1,'  Real=',I7,'  IO=',I10)
 1041 FORMAT (A,' ',A,' ',A3,': Cpu=',F10.1,'  Real=',I8)
 1045 FORMAT (A,' COUNT',I10,'   BYTES  ',1PE12.5)
 1100 FORMAT ('ACCOUNT: EXPAND ERROR',I7)
 1960 FORMAT ('ACCOUNT: IO ERROR',I7)
      END
