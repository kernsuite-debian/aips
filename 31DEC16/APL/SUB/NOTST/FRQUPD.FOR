      SUBROUTINE FRQUPD (DISKIN, CNOIN, GUSE, IANT, TIME, CVLSOU, ISUB,
     *   IFQ, CATIN, CLSORT, FRQOFF, IRET)
C-----------------------------------------------------------------------
C! Examines CL table or FO table for frequency offsets.
C# Table IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2009, 2011-2013
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
C   Routine to obtain the freq. offset (if any) stored in the CL table.
C   Uses call to TABIO directly rather than TABCAL for efficiency.
C   Will use an FO table if present in preference to the CL table.
C   Inputs:
C      DISKIN   I        Volume number
C      CNOIN    I        File catalogue number
C      GUSE     I        Version number of CL table to use
C      IANT     I        Antenna number - must be > 0
C      TIME     R        Time of visibility record (days)
C      CVLSOU   I        Source number (from common /CVELCM/)
C      ISUB     I        Subarray desired: <= 0 -> all
C      IFQ      I        Frequency ID desired: <= 0 -> all
C      CATIN    I(256)   Catalogue header
C   Input from common /CURCL/
C      CURXLT   D(*)     Time of current CL entry for each antenna
C      CURXLI   R(*)     Interval of current CL entry
C      CURLOO   R(*)     Current lo-offset for each antenna
C      CURRNO   I(*)     Current CL record number for each antenna
C   Input/output:
C      CLSORT   L        True if CL table already sorted and opened
C                        if false, sorted no matter what the header
C                        says about the sort order
C   Outputs:
C      FRQOFF   D(*)     Freq. offset (Hz) - for each if
C      IRET     I        Error flag: = 0 => OK, -1 not found
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DISKIN, CNOIN, GUSE, IANT, CVLSOU, ISUB, IFQ,
     *   CATIN(256), IRET
      REAL      TIME
      DOUBLE PRECISION FRQOFF(MAXIF)
      LOGICAL   CLSORT
C
      INTEGER   IFNO, TIMKOL, SOUKOL, ANTKOL, DOPKOL, INTKOL, SUBKOL,
     *   FQKOL, NKEY, NREC, REC2(XCLRSZ), IFOFF, I, J, IPOINT, LIMIT,
     *   NCOL, MLOCIF, DATP(128,2), NUMIF, KOLS(7), KEY(2,2), NUMCLT,
     *   CATBLK(256), KEYSUB(2,2), JANT, LSTSOU, LUSE
      REAL      TIMLOW, TIMHI, REC4(XCLRSZ), FKEY(2,2), TINTER
      DOUBLE PRECISION REC8(XCLRSZ/2), TLAST
      HOLLERITH CATH(256)
      LOGICAL   T, F, NOCL
      CHARACTER COLHED(7)*24, CHIF*8, INEXT*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCVL.INC'
      EQUIVALENCE (REC2, REC4, REC8)
      EQUIVALENCE (TIMKOL,XLKOLS(1)),  (INTKOL,XLKOLS(2)),
     *   (SOUKOL,XLKOLS(3)), (ANTKOL,XLKOLS(4)), (SUBKOL,XLKOLS(5)),
     *   (FQKOL,XLKOLS(6)), (DOPKOL,XLKOLS(7))
      EQUIVALENCE (CATBLK, CATH)
      SAVE NOCL, TINTER
      DATA T, F /.TRUE., .FALSE./
      DATA COLHED /'TIME', 'TIME INTERVAL', 'SOURCE ID', 'ANTENNA NO.',
     *   'SUBARRAY', 'FREQ ID', 'DOPPOFF'/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA CHIF /'IF'/
      DATA TINTER /-1.0/
C-----------------------------------------------------------------------
      CALL COPY (256, CATIN, CATBLK)
      CALL AXEFND (8, CHIF, CATBLK(KIDIM), CATH(KHCTP), MLOCIF, IRET)
      NUMIF = 1
      IF ((MLOCIF.GT.0) .AND. (IRET.EQ.0)) NUMIF = CATBLK(KINAX+MLOCIF)
C                                       Open CL?
      IF (.NOT.CLSORT) THEN
         NOCL = F
         INEXT = 'FO'
         CALL FNDEXT (INEXT, CATBLK, NUMCLT)
         IF (NUMCLT.LE.0) THEN
            INEXT = 'CL'
            CALL FNDEXT (INEXT, CATBLK, NUMCLT)
            END IF
         IF (NUMCLT.EQ.0) NOCL = T
         IF (NOCL) GO TO 999
C                                       Initialize
         CALL FILL (MAXANT, 0, CURRNO)
C                                       Sort CL table if needed
         ICXLUN = 49
         NKEY = 0
         NREC = 0
         NCOL = 0
         IXLRNO = 1
         LUSE = MIN (GUSE, NUMCLT)
         CALL TABINI ('READ', INEXT, DISKIN, CNOIN, LUSE, CATBLK,
     *      ICXLUN, NKEY, NREC, NCOL, DATP, XLBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1005) IRET, 'CL', LUSE
            GO TO 990
            END IF
C                                       Get number of scans
         NXLINR = XLBUFF(5)
C                                       Check if empty
         IF (NXLINR.LE.0) THEN
            WRITE (MSGTXT,1010) INEXT
            IRET = 1
            GO TO 990
            END IF
C                                       Get column pointers
         NKEY = 7
         CALL FNDCOL (NKEY, COLHED, 24, .TRUE., XLBUFF, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 999
         IRET = 0
         CALL FILL (NKEY, 0, XLKOLS)
         CALL FILL (NKEY, 0, XLNUMV)
         DO 20 J = 1,NKEY
            IPOINT = KOLS(J)
            IF (IPOINT.NE.0) THEN
               XLKOLS(J) = DATP(IPOINT,1)
               XLNUMV(J) = DATP(IPOINT,2) / 10
               END IF
 20         CONTINUE
C                                       check for non-zero DOPOFF
         DO 40 I = 1,NXLINR
            IXLRNO = I
            CALL TABIO ('READ', 0, IXLRNO, REC4, XLBUFF, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
            ELSE IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, INEXT
               GO TO 990
            ELSE
               DO 30 IFNO = 1,NUMIF
                  IF (REC4(DOPKOL+IFNO-1).NE.0.0) GO TO 50
 30               CONTINUE
               END IF
 40         CONTINUE
         CALL TABIO ('CLOS', 0, IXLRNO, XLBUFF, XLBUFF, IRET)
         MSGTXT = 'All Doppler offsets are zero'
         CALL MSGWRT (2)
         CLSORT = .TRUE.
         NOCL = .TRUE.
         IRET = 0
         GO TO 999
C                                       SORT: ignore claimed order
 50      KEY(1,1) = KOLS(1)
         KEY(2,1) = 0
         KEY(1,2) = KOLS(4)
         KEY(2,2) = 0
C                                       Close table
         CALL TABIO ('CLOS', 0, IXLRNO, XLBUFF, XLBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Sort
         CALL TABSRT (DISKIN, CNOIN, INEXT, GUSE, GUSE, KEY, KEYSUB,
     *      FKEY, XLBUFF, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, INEXT
            GO TO 990
            END IF
         CLSORT = .TRUE.
C                                       Open calibration table
C                                       for use in FRQUPD
         NKEY = 0
         NREC = 0
         NCOL = 0
         IXLRNO = 1
         NOCL = F
         CALL TABINI ('READ', INEXT, DISKIN, CNOIN, GUSE, CATBLK,
     *      ICXLUN, NKEY, NREC, NCOL, DATP, XLBUFF, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1005) IRET, INEXT, GUSE
            NOCL = T
            GO TO 999
            END IF
         WRITE (MSGTXT,1130) INEXT, GUSE
         CALL MSGWRT (4)
C                                       Get number of scans
         NXLINR = XLBUFF(5)
C                                       Check if empty
         IF (NXLINR.LE.0) THEN
            WRITE (MSGTXT,1010) INEXT
            CALL MSGWRT (8)
            NOCL = T
            END IF
         END IF
C                                       Set frqoff to a default
      DO 80 IFNO = 1,NUMIF
         FRQOFF(IFNO) = 0.D0
 80      CONTINUE
C
      IF (NOCL) GO TO 999
C                                       do we know the interval?
      IF (TINTER.LE.0.0) THEN
         JANT = IANT
 90      TLAST = -1.0D0
         LSTSOU = -1
         DO 100 I = 1,NXLINR
            IXLRNO = I
            CALL TABIO ('READ', 0, IXLRNO, REC4, XLBUFF, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
            ELSE IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, INEXT
               GO TO 990
            ELSE IF (REC4(INTKOL).GT.0.0) THEN
               TINTER = REC4(INTKOL)
               GO TO 170
            ELSE
               IF (JANT.LE.0) JANT = REC2(ANTKOL)
               IF (JANT.EQ.REC2(ANTKOL)) THEN
                  IF (LSTSOU.NE.REC2(SOUKOL)) THEN
                     TLAST = REC8(TIMKOL)
                     LSTSOU = REC2(SOUKOL)
                  ELSE
                     IF (REC8(TIMKOL)-TLAST.GT.TINTER) TINTER =
     *                  REC8(TIMKOL) - TLAST
                     TLAST = REC8(TIMKOL)
                     END IF
                  END IF
               END IF
 100        CONTINUE
         IF (TINTER.LE.0.0) THEN
            IF (IANT.EQ.JANT) THEN
               JANT = 0
               GO TO 90
            ELSE
               TINTER = 1.0 / (60.0 * 24.0)
               END IF
            END IF
         END IF
C                                       Do we need to look through table
 170  IF (CURRNO(IANT).GT.0) THEN
         IF (TIME.LE.(CURXLT(IANT) + 0.5*CURXLI(IANT))) THEN
            DO 175 IFNO = 1, NUMIF
               FRQOFF(IFNO) = CURLOO(IANT,IFNO)
 175           CONTINUE
            GO TO 999
            END IF
         END IF
C                                       Read until selected time.
      IF (CURRNO(IANT).LE.0) THEN
         LIMIT = 1
      ELSE
         LIMIT = CURRNO(IANT)
         END IF
      DO 200 I = LIMIT,NXLINR
         IXLRNO = I
         CALL TABIO ('READ', 0, IXLRNO, REC4, XLBUFF, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 200
         ELSE IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, INEXT
            GO TO 990
            END IF
C                                       make sure interval reasonable
         REC4(INTKOL) = MAX (TINTER, REC4(INTKOL))
C                                       leave if this is not good
C                                       independent of antenna,source
         IF (REC8(TIMKOL)-REC4(INTKOL)/2.GT.TIME) GO TO 220
C                                       See if correct source
         IF (REC2(SOUKOL).NE.CVLSOU) GO TO 200
C                                       See if correct antenna
         JANT = REC2(ANTKOL)
         IF (JANT.NE.IANT) GO TO 200
C                                       check subarray
         IF ((REC2(SUBKOL).GT.0) .AND. (ISUB.GT.0) .AND.
     *      (REC2(SUBKOL).NE.ISUB)) GO TO 200
C                                       check freqid
         IF ((REC2(FQKOL).GT.0) .AND. (IFQ.GT.0) .AND.
     *      (REC2(FQKOL).NE.IFQ)) GO TO 200
C                                       Check time
         TIMLOW = REC8(TIMKOL) - (REC4(INTKOL)/2.0)
         TIMHI  = REC8(TIMKOL) + (REC4(INTKOL)/2.0)
         IF ((TIME.LE.TIMHI) .AND. (TIME.GE.TIMLOW)) THEN
            CURXLT(JANT) = REC8(TIMKOL)
            CURXLI(JANT) = REC4(INTKOL)
            CURRNO(JANT) = IXLRNO
C                                       Loop over IF
            DO 180 IFNO = 1,NUMIF
               IFOFF = IFNO - 1
               FRQOFF(IFNO) = REC4(DOPKOL+IFOFF)
               CURLOO(JANT,IFNO) = REC4(DOPKOL+IFOFF)
 180           CONTINUE
            GO TO 999
            END IF
 200     CONTINUE
C                                       did we find it
 220  IRET = -1
      IF (CURRNO(IANT).GT.0) THEN
         IF (TIME.LE.(CURXLT(IANT) + 0.5*CURXLI(IANT))) THEN
            DO 230 IFNO = 1, NUMIF
               FRQOFF(IFNO) = CURLOO(IANT,IFNO)
 230           CONTINUE
            IRET = 0
            END IF
         END IF
      GO TO 999
C                                       Error?
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('FRQUPD: ERROR',I3,' OPENING ',A2,' TABLE, VERSION',I5)
 1010 FORMAT ('FRQUPD: EMPTY ',A,' TABLE')
 1030 FORMAT ('FRQUPD: ERROR',I3,' SORTING ',A,' TABLE')
 1000 FORMAT ('FRQUPD: ERROR',I3,' READING ',A,' TABLE')
 1130 FORMAT ('Using ',A,' table ',I3,' to obtain time dependent freq',
     *   ' offsets')
      END
