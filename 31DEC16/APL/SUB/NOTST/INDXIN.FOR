      SUBROUTINE INDXIN (IERR)
C-----------------------------------------------------------------------
C! Initializes index (NX) file, finds first scan selected.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2007, 2014
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
C   Initializes INDEX file, finds first scan selected.
C   If there is no INDEX file the first and last records are set to the
C   First and last records of the data file.
C    Inputs from common /SELCAL/
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The source numbers of sources included or
C                        excluded.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      INITVS       I    First vis to read in single/multi source file
C    Output:
C      IERR         I    Return code, 0=>OK, otherwise INDEX file
C                        exists but cannot be read.
C                        -1 => No matching data found
C    Output to common /SELCAL/:
C      INXRNO       I    Current INDEX file record number.
C                        If .LT. 0 then there is no index file.
C      NINDEX       I    Number of entriex in the index table
C      FSTVIS       I    First visibility number of current scan.
C      LSTVIS       I    Last visibility number of current scan.
C      CURSOU       I    Current source number.
C      NXKOLS(6)    I    The column pointer array in order, TIME,
C                        TIME INTERVAL, SOURCE ID, SUBARRAY, START VIS,
C                        END VIS.
C      NXNUMV(6)    I    Element count in each column.
C      NXBUFF(*)    I    TABIO buffer for INDEX table.
C-----------------------------------------------------------------------
      INTEGER   IERR, JERR, J, NXIDSO, NXSUBA
      LOGICAL   F, MATCH, TABLE, EXIST, FITASC
      INTEGER   I, NXSTA, NXEND, FREQID
      REAL      NXTIME, NXDTIM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize output
      INXRNO = -1
      FSTVIS = MAX (1, INITVS)
      LSTVIS = CATBLK(KIGCN)
      CURSOU = 0
C                                       See if NX file exists.
      CALL ISTAB ('NX', IUDISK, IUCNO, 1, IXLUN, NXBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open NX table
      CALL NDXINI ('READ', NXBUFF, IUDISK, IUCNO, 1, CATUV, IXLUN,
     *   INXRNO, NXKOLS, NXNUMV, JERR)
      IF (JERR.LE.0) GO TO 20
         IERR = 1
         WRITE (MSGTXT,1000) JERR
         GO TO 990
C                                       Get number of scans
 20   NINDEX = NXBUFF(5)
C                                       Check if empty
      IF (NINDEX.LE.0) THEN
         CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *      NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, I)
         GO TO 800
         END IF
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
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
         IERR = 0
C                                       Check time range.
         IF (TSTART.GT.(NXTIME+NXDTIM/2.0)) GO TO 60
         IF (TEND.LT.(NXTIME-NXDTIM/2.0)) GO TO 60
C                                       Check freq id
         IF ((FREQID.GT.0) .AND. (FREQID.NE.FRQSEL) .AND.
     *       (FRQSEL.GT.0)) GO TO 60
C                                       Check subarray
         IF ((SUBARR.GT.0) .AND. (NXSUBA.NE.SUBARR) .AND.
     *       (NXSUBA.GT.0)) GO TO 60
C                                       Check Initial vis number
         IF (INITVS.GT.NXEND) GO TO 60
C                                       Set values
         FSTVIS = MAX (NXSTA, INITVS)
         LSTVIS = NXEND
         IF (LSTVIS.LT.FSTVIS) GO TO 60
         IF (FSTVIS.LT.1) FSTVIS = 1
         IF (LSTVIS.LT.1) LSTVIS = 1
         CURSOU = NXIDSO
C                                       See if all sources selected.
         IF (NSOUWD.LE.0) GO TO 999
C                                       Search source lists
         MATCH = F
         DO 40 J = 1,NSOUWD
            MATCH = NXIDSO .EQ. SOUWAN(J)
            IF (MATCH) GO TO 50
 40         CONTINUE
C                                       See if found match
 50      IF ((MATCH.AND.DOSWNT) .OR. ((.NOT.MATCH).AND.(.NOT.DOSWNT)))
     *    GO TO 999
 60      CONTINUE
C                                       No valid data
      IERR = -1
      MSGTXT = 'INDXIN: NO DATA FOUND MEETING SELECTION CRITERIA'
      CALL MSGWRT (6)
      FSTVIS = 1
      LSTVIS = 0
C                                       Close NX file
      CALL TABNDX ('CLOS', NXBUFF, INXRNO, NXKOLS, NXNUMV, NXTIME,
     *   NXDTIM, NXIDSO, NXSUBA, NXSTA, NXEND, FREQID, I)
      INXRNO = -1
      GO TO 999
C                                       No INDEX file
 800  INXRNO = -1
      FSTVIS = MAX (1, INITVS)
      LSTVIS = CATBLK(KIGCN)
      CURSOU = 0
      NINDEX = 0
      NXBUFF(5) = NINDEX
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('INDXIN: ERROR',I3,' OPENING INDEX TABLE')
 1020 FORMAT ('INDXIN: ERROR',I3,' READING INDEX TABLE')
      END
