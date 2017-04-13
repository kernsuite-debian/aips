      SUBROUTINE CALSEL (DISK, CNO, TYPE, VER, CATBLK, IBUFF, NSOUWD,
     *   SOUWAN, DOSWNT, NANTWD, ANTWAN, DOAWNT, TIMRA, SUBA, FREQID,
     *   IRET)
C-----------------------------------------------------------------------
C! Deletes a selected subset of a CL or SN table.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2003, 2012
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
C   Routine to flag a selected subset of a CL or SN table.
C   Input:
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      TYPE     C*2      Table type (CL or SN)
C      VER      I        Table version.
C      CATBLK   I(256)   Catalog header block.
C      NSOUWD   I        Number of sources included or excluded.
C      DOSWNT   L        If .TRUE. then sources in SOUWAN are
C                        included, if .FALSE. then excluded.
C      SOUWAN   I(*)     The source numbers of sources included or
C                        excluded.
C      NANTWD   I        Number of antennas included or excluded.
C      ANTWAN   I(*)     The antenna numbers of antennas included or
C                        excluded.
C      DOSWNT   L        If .TRUE. then antennas in ANTWAN are included
C                        else they are excluded.
C      TIMRA    D(2)     First and last times to be considered. (days)
C      SUBA     I        Desired subarray, 0=>all.
C      FREQID   I        Desired freq. id number. 0=>all
C   Input/Output:
C      IBUFF    I(512)   Buffer for TABIO use.
C   Output:
C      IRET     I        Return code 0=OK, else failed.
C   Note: uses LUN 28
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, CATBLK(256), IBUFF(*), NSOUWD,
     *   SOUWAN(*), NANTWD, ANTWAN(*), SUBA, FREQID, IRET
      DOUBLE PRECISION TIMRA(2)
      LOGICAL   DOSWNT, DOAWNT
      CHARACTER TYPE*2
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRCODE, RECI(XCLRSZ), LUN, JERR, IVER, KOLS(5), NDROP,
     *   NKEY, TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, IRNO, ORNO,
     *   NUMREC, LOOP, MSGSAV, I , NREC, NCOL, DATP(128,2)
      LOGICAL   ALLSEL, SELECT, GOTIT, TABLE, EXIST, FITASC
      REAL      RECR(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      CHARACTER COLHED(5)*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (RECI, RECR, RECD)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), SOUKOL),
     *   (KOLS(3),ANTKOL), (KOLS(4), SUBKOL),   (KOLS(5),FRQKOL)
      DATA COLHED /'TIME ', 'SOURCE ID ', 'ANTENNA NO. ',
     *   'SUBARRAY ', 'FREQ ID '/
      DATA LUN /28/
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      NDROP = 0
      IRET = 0
C                                       Does table exist?
      CALL ISTAB (TYPE, DISK, CNO, VER, LUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((.NOT.EXIST) .OR. (.NOT.TABLE) .OR. (JERR.NE.0)) GO TO 999
C                                       Is all data selected?
C                                       Timerange
      ALLSEL = (TIMRA(1).LE.1.0D-10) .AND. (TIMRA(2).GE.1.0D10)
C                                       Subarray
      ALLSEL = ALLSEL .AND. (SUBA.LE.0)
C                                       Sources
      ALLSEL = ALLSEL .AND. (NSOUWD.LE.0)
C                                       Antennas
      ALLSEL = ALLSEL .AND. (NANTWD.LE.0)
C                                       FQ ID - have to check table
      IF (ALLSEL . AND. (FREQID.EQ.1)) THEN
         IVER = 1
         NKEY = 0
         NREC = 0
         NCOL = 0
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL TABINI ('READ', 'FQ', DISK, CNO, IVER, CATBLK, LUN,
     *      NKEY, NREC, NCOL, DATP, IBUFF, JERR)
         IF (JERR.EQ.0) CALL TABIO ('CLOS', IRCODE, IRNO, RECI, IBUFF,
     *      JERR)
         MSGSUP = MSGSAV
         ALLSEL = (IBUFF(5).LE.1) .OR. (JERR.NE.0)
      ELSE
         ALLSEL = ALLSEL .AND. (FREQID.LE.0)
         END IF
C                                       If all selected simply remove
C                                       table.
      IF (ALLSEL) THEN
         MSGTXT = 'CALSEL: Making room for new ' // TYPE // ' data by '
         CALL MSGWRT (2)
         WRITE (MSGTXT,1000) TYPE, VER
         CALL MSGWRT (2)
         CALL RMEXT (DISK, CNO, TYPE, VER, CATBLK, IBUFF, IRET)
         GO TO 999
         END IF
C                                       Open table
      NKEY = 0
      NREC = 0
      NCOL = 0
      CALL TABINI ('WRIT', TYPE, DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Number of records
      NUMREC = IBUFF(5)
C                                       Find column pointers.
      NKEY = 5
      CALL FNDCOL (NKEY, COLHED, 24, .TRUE., IBUFF, KOLS, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CALSEL: ERROR FINDING COLUMNS IN CAL TABLE'
         GO TO 990
         END IF
C                                       Convert pointers to something
C                                       useful.
      DO 50 LOOP = 1,NKEY
         KOLS(LOOP) = DATP(KOLS(LOOP),1)
 50      CONTINUE
C                                       Copy records which don't match
C                                       selection criteria.
      ORNO = 0
      DO 500 LOOP = 1,NUMREC
         IRNO = LOOP
C                                       Read input record
         CALL TABIO ('READ', IRCODE, IRNO, RECI, IBUFF, IRET)
         IF (IRET.LT.0) GO TO 500
         IF (IRET.NE.0) GO TO 900
C                                       Does this meet selection
C                                       criteria?
C                                       Subarray
         SELECT = ((SUBA.EQ.RECI(SUBKOL)) .OR. (RECI(SUBKOL).LE.0) .OR.
     *      (SUBA.LE.0))
C                                       FQ id.
         SELECT = SELECT .AND. ((RECI(FRQKOL).EQ.FREQID) .OR.
     *      (RECI(FRQKOL).LE.0) .OR. (FREQID.LE.0))
C                                       Timerange
         SELECT = SELECT .AND. ((RECD(TIMKOL).GE.TIMRA(1)) .AND.
     *      (RECD(TIMKOL).LE.TIMRA(2)))
C                                       Source
         IF (SELECT .AND. (NSOUWD.GT.0)) THEN
            GOTIT = .FALSE.
            DO 300 I = 1,NSOUWD
               GOTIT = GOTIT .OR. (RECI(SOUKOL).EQ.SOUWAN(I))
 300           CONTINUE
            IF (GOTIT .AND. (.NOT.DOSWNT)) SELECT = .FALSE.
            IF ((.NOT.GOTIT) .AND. DOSWNT) SELECT = .FALSE.
            END IF
C                                       Antenna
         IF (SELECT .AND. (NANTWD.GT.0)) THEN
            GOTIT = .FALSE.
            DO 350 I = 1,NANTWD
               GOTIT = GOTIT .OR. (RECI(ANTKOL).EQ.ABS(ANTWAN(I)))
 350           CONTINUE
            IF (GOTIT .AND. (.NOT.DOAWNT)) SELECT = .FALSE.
            IF ((.NOT.GOTIT) .AND. DOAWNT) SELECT = .FALSE.
            END IF
C                                       Copy to output
         IF (.NOT.SELECT) THEN
            ORNO = ORNO + 1
            IF (ORNO.LT.LOOP) THEN
               CALL TABIO ('WRIT', IRCODE, ORNO, RECI, IBUFF, IRET)
               IF (IRET.NE.0) GO TO 900
               END IF
            END IF
 500     CONTINUE
C                                       Close table
C                                       Number of records
      IBUFF(5) = ORNO
      CALL TABIO ('CLOS', IRCODE, LOOP, RECI, IBUFF, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Tell number dropped
      NDROP = NUMREC - ORNO
      IF (NDROP.GT.0) THEN
         MSGTXT = 'CALSEL: Making room for new ' // TYPE // ' data by '
         CALL MSGWRT (2)
         WRITE (MSGTXT,1500) NDROP, TYPE, VER
         CALL MSGWRT (2)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Deleting ',A,' table no. ',I4)
 1500 FORMAT ('Deleting ',I6,' records from ',A,' table no. ',I4)
 1900 FORMAT ('CALSEL: TABIO ERROR',I3)
      END
