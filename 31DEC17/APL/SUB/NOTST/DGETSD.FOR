      SUBROUTINE DGETSD (RPARM, VIS, TIMLST, IERR)
C-----------------------------------------------------------------------
C! Reads, selects single dish data, calibrates and edits.
C# Calibration Sdish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000-2001, 2007, 2015
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
C   Single dish calibration routine.
C   Reads next selected data record. Applies calibration and editing.
C   Inputs from common /SELCAL/:
C      INXRNO       I    Current INDEX file record number.
C                        If .LT. 0 then there is no index file.
C      NINDEX       I    Number of entriex in the index table
C      NXKOLS(6)    I    Pointer array for index records.
C      SUBARR       I    Subarray number desired, 0=>any.
C      TRANSL       L    If true, translate data to requested stokes.
C      JADR(2,*)    I    If TRANSL is .false. then the following are
C                        used:
C                        JADR(1,1) = input file INCS
C                        JADR(1,2) = input file INCF
C                        JADR(1,3) = input file INCIF
C                        JADR(1,4) = input file no. stokes' pixel
C      TSTART       R    Start time in days.
C      TEND         R    End time in days.
C      NPRMIN       I    No. random parameters in the input data
C      NRPARM       I    No. random parameters in output data.
C   Input/output:
C      TIMLST       R    Time of last record
C   Output:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      IERR       I    Return code, 0=OK, else UVDISK error number.
C   Output to common /SELCAL/:
C      FSTVIS       I    First visibility number of current scan.
C      LSTVIS       I    Last visibility number of current scan.
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      RPARM(*), VIS(3,*), TIMLST
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMAX, NIO, BIND, IBUFF(1024), IA1, IA2, IRET, J, BO, I,
     *   TIMKOL, INTKOL, SOUKOL, SUBKOL, FRSKOL, LSTKOL, RECI(MAXNXC),
     *   IUBIND, CURSUB, NREAD, FRSRED, LIMT, IBASE
      LOGICAL   DROP, F, SLCTD
      REAL      RECORD(MAXNXC), TIME, BASEN
      INTEGER   TBUFSZ
      PARAMETER (TBUFSZ = MAXCIF * 3)
      REAL      TBUFF(TBUFSZ)
      SAVE TBUFF
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (IBUFF, NXBUFF)
      EQUIVALENCE (NXKOLS, TIMKOL), (NXKOLS(2), INTKOL),
     *   (NXKOLS(3), SOUKOL), (NXKOLS(4), SUBKOL), (NXKOLS(5), FRSKOL),
     *   (NXKOLS(6), LSTKOL)
      DATA F /.FALSE./
      DATA BO /1/
C-----------------------------------------------------------------------
      TIME = -1.0E10
C                                       Read next record.
 100  CALL UVDISK ('READ', IULUN, IUFIND, UBUFF, NIO, BIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 990
         END IF
C                                       Check for end of scan
      IF (NIO.LE.0) GO TO 200
C                                       Check timerange
      TIME = UBUFF(BIND+ILOCT)
      IF (TIME.LT.TSTART) GO TO 100
C                                       Be sure not to go backwards in
C                                       time due to multiple subarrays
C                                       if applying calibration
      IF (((DOCAL) .OR. (DOFLAG)) .AND. (TIMORD)) THEN
         IF (TIME.LT.TIMLST) GO TO 100
         TIMLST = TIME
         IF (TIME.GT.TEND) GO TO 200
         END IF
      IF (TIME.GT.TEND) GO TO 100
C                                       Set beam
      IF (ILOCB.GE.0) THEN
         BASEN = UBUFF(BIND+ILOCB)
         IBASE = BASEN + 0.1
         IA1 = BASEN / 256.0 + 0.1
         IA2 = BASEN - 256 * IA1 + 0.1
         CURSUB = 1.5 + 100.0 * (BASEN - IBASE)
      ELSE IF ((ILOCA1.GE.0) .AND. (ILOCA2.GE.0) .AND. (ILOCSA.GE.0))
     *   THEN
         IA1 = UBUFF(BIND+ILOCA1) + 0.1
         IA2 = UBUFF(BIND+ILOCA2) + 0.1
         CURSUB = UBUFF(BIND+ILOCSA) + 0.1
      ELSE
         IA1 = 1
         IA2 = 1
         CURSUB = 1
         END IF
C                                       Selected beam?
      IF (.NOT.SLCTD (IA2, ANTENS, NANTSL, DOAWNT)) GO TO 100
C                                       Check subarray
      IF ((SUBARR.GT.0) .AND. (CURSUB.NE.SUBARR)) GO TO 100
C                                       Copy to output
      CALL RCOPY (NRPARM, UBUFF(BIND), RPARM)
C                                       Uncompress if needed to TBUFF
      J = LRECIN - NPRMIN
      IF (ISCMP) THEN
         NUMAX = 3
         CALL ZUVXPN (J, UBUFF(BIND+NPRMIN), UBUFF(BIND+KLOCWT), TBUFF)
      ELSE
         NUMAX = CATUV(KINAX)
         CALL RCOPY (J, UBUFF(BIND+NPRMIN), TBUFF)
         END IF
C                                       Do flagging
      DROP = F
      IF (DOFLAG) THEN
         CALL DATFLG (RPARM, TBUFF, DROP, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (DROP) GO TO 100
         END IF
C                                       Calibrate
      IF (DOCAL) THEN
         CALL DCALSD (IA2, TIME, UBUFF(BIND+ILOCU), UBUFF(BIND+ILOCV),
     *      TBUFF, DROP, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Get data in correct Stokes form.
      IF (.NOT.(DOFLAG .AND. DROP)) THEN
         CALL DGGET (TBUFF, NUMAX, TRANSL, MVIS, JADR,SELFAC, ALLWT,
     *      VIS, DROP)
         END IF
C                                       Check if data all flagged
      IF (DOFLAG .AND. DROP) GO TO 100
      GO TO 999
C                                       New scan.
C                                       Find next in INDEX file.
 200  IERR = 4
C                                       Check if INDEX file used.
      IF (INXRNO.LE.0) GO TO 999
      IF (INXRNO.GT.NINDEX) GO TO 999
C                                       See if time range exhausted.
      IF (TIME.GT.TEND) GO TO 999
C                                       Loop through records
      LIMT = INXRNO
      DO 260 I = LIMT,NINDEX
         INXRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, INXRNO, RECORD, IBUFF, IRET)
C                                       Check if flagged
         IF (IRET.LT.0) GO TO 260
C                                       Check error
         IF (IRET.GT.0) GO TO 999
         IERR = 0
C                                       Check time range.
         IF (TSTART.GT.(RECORD(TIMKOL)+RECORD(INTKOL))) GO TO 260
C                                       Check subarray
         IF ((SUBARR.GT.0) .AND. (RECI(SUBKOL).NE.SUBARR) .AND.
     *      (RECI(SUBKOL).GT.0)) GO TO 260
C                                       Set values
            FSTVIS = RECI(FRSKOL)
            LSTVIS = RECI(LSTKOL)
            GO TO 300
 260     CONTINUE
C                                       No more data selected.
      IERR = 4
      GO TO 999
C                                       Init I/O to uvfile
 300  INXRNO = INXRNO + 1
      NREAD = LSTVIS - FSTVIS + 1
      FRSRED = FSTVIS - 1
      CALL UVINIT ('READ', IULUN, IUFIND, NREAD, FRSRED, LRECIN, 1,
     *   UBUFSZ, UBUFF, BO, IUBIND, IERR)
      IF (IERR.LE.0) GO TO 100
         WRITE (MSGTXT,1300) IERR
         GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('DGETSD: ERROR',I3,' READING SD DATA')
 1300 FORMAT ('DGETSD: ERROR',I3,' INITIALIZING I/O TO SD DATA')
      END
