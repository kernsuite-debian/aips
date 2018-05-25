      SUBROUTINE CATIO (OP, IVOL, CNO, CATBLK, STAT, BUFF, IERR)
C-----------------------------------------------------------------------
C! reads/writes header blocks in the catalog file
C# Catalog Header
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
C   CATIO reads or writes blocks in the map catalog header file.
C   Inputs:
C      OP       C*4      'READ' => get block into CATBLK
C                        'WRIT' => put CATBLK onto disk catalog
C                        'UPDT' => as WRIT but for use when the calling
C                                  program has previously set the
C                                  status to WRITE
C      IVOL     I        Disk volume containing catalog (1 rel)
C      CNO      I        Slot number of interest
C      CATBLK   I(256)   Array to be written on disk: WRIT, UPDT
C      STAT     C*4      Status desired for slot after operation
C                            'READ','WRIT','REST' where REST => no
C                            change of status is desired
C   Outputs:
C      CATBLK   I(256)   Array read from disk: READ
C      BUFF     I(256)   Working buffer
C      IERR     I        Error code: 0 => ok
C                           1 => cannot open catalog file
C                           2 => input parameter error
C                           3 => cannot read catalog file
C                           4 => cannot WRIT/UPDT: file is busy
C                           5 => did READ/UPDT, cannot add STAT = WRIT
C                           6 => Warning on READ, file writing
C                           7 => As 6, also added STAT=READ
C                           8 => As 6, STAT inconsistent or wrong
C                           9 => Warning: STAT inconsistent/wrong
C   The requested OP is performed unless IERR = 1 through 4.  The
C   final status requested is not set if IERR = 1 - 5, 8 - 9.  The
C   latter are probably unimportant.
C-----------------------------------------------------------------------
      CHARACTER OP*4, STAT*4
      INTEGER   IVOL, CNO, CATBLK(256), BUFF(256), IERR
C
      INTEGER   NREC, IWER(2,5), IER, CMAX, CMOD, NIND, OPSTAT, STSTAT,
     *   IOP, NOP, CIND, NSTAT, NISTAT, CLUN, INERR, OPNER, RDERR, NWPL,
     *   NLPR, NTIME, IT(6)
      LOGICAL   T, F, DOSTAT
      CHARACTER CLOP(3)*4, PHNAME*48, CB*2, OPCODE(3)*4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDRC.INC'
      DATA NOP /3/
      DATA OPCODE /'WRIT','READ','UPDT'/
      DATA CLOP   /'CLWR','CLRD','REST'/
      DATA OPNER, INERR, RDERR /1, 2, 3/
      DATA IWER /0,6, 6,7, 9,8, 5,5, 9,8/
      DATA CLUN, CB /15,'CB'/
      DATA NSTAT, NTIME /1,2/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       check OPcode
      DO 10 IOP = 1,NOP
         IF (OPCODE(IOP).EQ.OP) GO TO 15
 10      CONTINUE
      IERR = INERR
      WRITE (MSGTXT,1010) OP
      CALL MSGWRT (6)
      GO TO 999
C                                       open catalog file
 15   CALL CATOPN (IVOL, CIND, BUFF, CMAX, IERR)
      IF (IERR.EQ.3) GO TO 980
      IF (IERR.NE.0) THEN
         IERR = OPNER
         GO TO 999
         END IF
C                                       check validity of CNO
      IF ((CNO.LE.0) .OR. (CNO.GT.CMAX)) THEN
         IERR = INERR
         WRITE (MSGTXT,1015) CNO
         GO TO 970
         END IF
C                                       get directory block
      DOSTAT = DSKLOK(IVOL).NE.1
C                                       cat block change allowed?
      IF (DOSTAT) THEN
         NWPL = 10
         NLPR = 256 / NWPL
         CMOD = (CNO - 1) / NLPR
         NREC = 2 + CMOD
         NIND = 1 + NWPL * (CNO - NLPR*CMOD - 1)
         CALL ZFIO ('READ', CLUN, CIND, NREC, BUFF, IER)
         IF (IER.NE.0) THEN
            IERR = RDERR
            GO TO 980
            END IF
         NISTAT = NIND + NSTAT
         IF (IOP.NE.3) THEN
            CALL CHSTAT (OP, BUFF(NISTAT), OPSTAT)
         ELSE
            OPSTAT = 0
            IF (BUFF(NISTAT).GT.0) OPSTAT = 3
            END IF
C                                       error: map too busy
         IF (OPSTAT.EQ.3) THEN
            IERR = 4
            WRITE (MSGTXT,1020) CNO
            CALL MSGWRT (6)
C                                       reset operation status
         ELSE
            CALL CHSTAT (CLOP(IOP), BUFF(NISTAT), IER)
C                                       set requested final status
            CALL CHSTAT (STAT, BUFF(NISTAT), STSTAT)
            IERR = IWER(OPSTAT+1, STSTAT+1)
            IF ((OPSTAT.EQ.1) .AND. (STAT.EQ.'CLWR')) IERR = 0
            IF ((STSTAT.GT.2) .AND. (IERR.NE.0)) THEN
               WRITE (MSGTXT,1030) CNO
               CALL MSGWRT (6)
C                                       return directory to disk
C                                       with latest time
            ELSE
               CALL CATIME (1, BUFF(NIND+NTIME), IT)
               CALL ZFIO ('WRIT', CLUN, CIND, NREC, BUFF, IER)
               END IF
            END IF
         END IF
      CALL ZCLOSE (CLUN, CIND, IER)
C                                       Open header file
      CALL ZPHFIL (CB, IVOL, CNO, 1, PHNAME, IER)
      CALL ZOPEN (CLUN, CIND, IVOL, PHNAME, F, T, T, IER)
      IF (IER.NE.0) THEN
         IERR = OPNER
         GO TO 999
         END IF
C                                       do operation
      NREC = 1
      IF (IOP.EQ.2) THEN
         CALL ZFIO ('READ', CLUN, CIND, NREC, CATBLK, IER)
      ELSE
         CALL ZFIO ('WRIT', CLUN, CIND, NREC, CATBLK, IER)
         END IF
      IF (IER.NE.0) IERR = RDERR
      GO TO 980
C                                       error message out
 970  CALL MSGWRT (6)
C                                       close catalog/header file
 980  CALL ZCLOSE (CLUN, CIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CATIO: ILLEGAL OPCODE ',A4)
 1015 FORMAT ('CATIO: CATNO ',I6,' OUT OF RANGE')
 1020 FORMAT ('CATIO: CATBLK',I4,' IN USE, CAN''T WRITE')
 1030 FORMAT ('CATIO: CATBLK',I4,' IN USE, CAN''T CHANGE STATUS')
      END
