      SUBROUTINE TBTIME (START, STOP, IERR)
C-----------------------------------------------------------------------
C! find start/stop times of a uv data set
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   determines the start and stop times of a TB-sorted uv data set.  It
C   reads the index table if there is one or it reads the first and last
C   vis points.  It is similar to INDXIN and initializes INDEX file.
C   Inputs from common /SELCAL/
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The source numbers of sources included or
C                        excluded.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C   Output:
C      START        R    Start time (days)
C      STOP         R    Stop time in days
C      IERR         I    Return code, 0=>OK, otherwise INDEX file
C                        exists but cannot be read.
C   Output to common /SELCAL/:
C      INXRNO       I    Current INDEX file record number.
C                        If .LT. 0 then there is no index file.
C      NINDEX       I    Number of entries in the index table
C      FSTVIS       I    First visibility number of current scan.
C      LSTVIS       I    Last visibility number of current scan.
C      CURSOU       I    Current source number.
C      NXKOLS(MAXNXC) I   The column pointer array in order, TIME,
C                         TIME INTERVAL, SOURCE ID, SUBARRAY, START VIS,
C                         END VIS., FREQID
C      NXNUMV(MAXNXC) I   Element count in each column.
C      NXBUFF(*)    I    TABIO buffer for INDEX table.
C   Usage notes:
C      Requires - UVPGET to have been called
C               - SOUFIL to have been called (multi-source)
C               - uv header in common /MAPHDR/ (I think)
C-----------------------------------------------------------------------
      REAL      START, STOP
      INTEGER   IERR
C
      INTEGER   JERR, J, NXIDSO, NXSUBA, NIO, INDEXT, IUBIND, I, NXSTA,
     *   NXEND, NREAD, BO, FRSRED, FREQID
      LOGICAL   T, F, MATCH, TABLE, EXIST, FITASC
      REAL      NXTIME, NXDTIM, T1, T2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
C                                       Timerange
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)).EQ.0.0)
     *   TIMRNG(1) = -1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)).EQ.0.0)
     *   TIMRNG(5) = 1.0E6
C                                       Set time range.
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
C                                       Initialize output
      INXRNO = -1
      FSTVIS = 1
      LSTVIS = CATUV(KIGCN)
      CURSOU = 0
      IXLUN = 30
C                                       See if NX file exists.
      CALL ISTAB ('NX', IUDISK, IUCNO, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open NX table
      CALL NDXINI ('READ', NXBUFF, IUDISK, IUCNO, 1, CATUV, IXLUN,
     *   INXRNO, NXKOLS, NXNUMV, JERR)
      IF (JERR.GT.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000) 'OPEN', JERR
         GO TO 990
         END IF
C                                       Get number of scans
      NINDEX = NXBUFF(5)
C                                       Check if empty
      IF (NINDEX.LE.0) GO TO 800
      T2 = 1.E8
C                                       Locate first selected scan.
C                                       Loop through records
      DO 60 I = 1,NINDEX
         INXRNO = I
         IERR = 1
C                                       Read record.
         CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      NXTIME, NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID,
     *      IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) 'READ', IERR
            GO TO 990
            END IF
         IERR = 0
C                                       Check time range.
         IF (TSTART.GT.(NXTIME+NXDTIM/2.0)) GO TO 60
C                                       Check freq id
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0)) GO TO 60
C                                       Set values
         FSTVIS = NXSTA
         LSTVIS = NXEND
         IF (FSTVIS.LT.1) FSTVIS = 1
         IF (LSTVIS.LT.1) LSTVIS = 1
         CURSOU = NXIDSO
C                                       See if all sources selected.
         IF (NSOUWD.LE.0) GO TO 100
C                                       Search source lists
         MATCH = F
         DO 40 J = 1,NSOUWD
            MATCH = NXIDSO.EQ.SOUWAN(J)
            IF (MATCH) GO TO 50
 40         CONTINUE
C                                       See if found match
 50      IF ((MATCH.AND.DOSWNT) .OR. ((.NOT.MATCH).AND.(.NOT.DOSWNT)))
     *    GO TO 100
 60      CONTINUE
C                                       No valid data
      IERR = 5
      MSGTXT = 'TBTIME: NO DATA FOUND MEETING SELECTION CRITERIA'
      GO TO 990
C                                       return start time
 100  START = NXTIME - 0.51 * NXDTIM
      T1 = NXTIME
      IF (TSTART.GT.START) START = TSTART
      STOP = NXTIME + 0.51 * NXDTIM
      IF (STOP.GT.TEND) STOP = TEND
      INDEXT = I + 1
C                                       Get last included one
      IF (INDEXT.LE.NINDEX) THEN
         DO 160 I = INDEXT,NINDEX
            INXRNO = I
            IERR = 1
C                                       Read record.
            CALL TABNDX ('READ', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *         NXTIME, NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID,
     *         IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) 'READ', IERR
               GO TO 990
               END IF
            T2 = MIN (T2, NXTIME)
            IERR = 0
C                                       Check time range.
            IF ((NXTIME-NXDTIM/2.).GT.TEND) GO TO 160
C                                       Freq id
            IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0)) GO TO 160
C                                       See if all sources selected.
            IF (NSOUWD.LE.0) GO TO 150
C                                       Search source lists
            MATCH = F
            DO 130 J = 1,NSOUWD
               MATCH = NXIDSO.EQ.SOUWAN(J)
               IF (MATCH) GO TO 140
 130           CONTINUE
C                                       See if found match
 140        IF (((MATCH) .AND. (.NOT.DOSWNT)) .OR. ((.NOT.MATCH) .AND.
     *         (DOSWNT))) GO TO 160
C                                       matches: save value
 150        STOP = NXTIME + 0.51 * NXDTIM
            IF (STOP.GT.TEND) STOP = TEND
 160        CONTINUE
         END IF
      CALL TABIO ('CLOS', 0, INXRNO, UBUFF, NXBUFF, IERR)
      IF (T2.LT.T1) THEN
         MSGTXT = 'TIMES OUT OF ORDER'
         IERR = 10
         GO TO 990
         END IF
      GO TO 999
C                                       No INDEX file
 800  INXRNO = -1
      FSTVIS = 1
      LSTVIS = CATUV(KIGCN)
      CURSOU = 0
      NINDEX = 0
      NXBUFF(5) = NINDEX
C                                       open uv file
      IULUN = 25
C                                       Open UV file
      CALL ZPHFIL ('UV', IUDISK, IUCNO, 1, UFILE, IERR)
      CALL ZOPEN (IULUN, IUFIND, IUDISK, UFILE, T, F, T, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1800) 'OPEN', IERR
         GO TO 990
         END IF
C                                       Init I/O to uvfile
      NREAD = 1
      LRECIN = LREC
      UBUFSZ = UVBFSL * 2
C                                       read first vis
      FRSRED = FSTVIS - 1
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1810) 'INIT FIRST', IERR
         GO TO 980
         END IF
      CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1810) 'READ FIRST', IERR
         GO TO 980
         END IF
      T1 = UBUFF(IUBIND+ILOCT)
      START = T1
      IF (START.LT.TSTART) START = TSTART
C                                       read last vis
      FRSRED = LSTVIS - 1
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1810) 'INIT LAST', IERR
         GO TO 980
         END IF
      CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, IUBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1810) 'READ LAST', IERR
         GO TO 980
         END IF
      T2 = UBUFF(IUBIND+ILOCT)
      STOP = T2
      IF (STOP.GT.TEND) STOP = TEND
      IF (T2.LT.T1) THEN
         MSGTXT = 'TIMES OUT OF ORDER'
         IERR = 10
      ELSE
         GO TO 985
         END IF
C                                       Errors
 980  CALL MSGWRT (8)
 985  CALL ZCLOSE (IULUN, IUFIND, JERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TBTIME: ERROR',I3,' ',A,'ING INDEX TABLE')
 1800 FORMAT ('TBTIME: UNABLE TO ',A,' INPUT UV FILE, ERROR',I5)
 1810 FORMAT ('TBTIME: UNABLE TO ',A,' INPUT VISIBILITY RECORD, ERROR',
     *   I5)
      END
