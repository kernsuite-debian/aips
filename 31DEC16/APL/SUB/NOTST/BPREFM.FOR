      SUBROUTINE BPREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of BP table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2015
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
C   Routine to change the format of the BP table from one containing
C   13/10 columns to the 14/11 columns needed by the addition of the
C   FREQID column.
C   As of Sept 1, 1994 the IFFREQ column has become the CHN_SHIFT
C   column in order to deal with VLBA bandpasses. BPREFM will
C   change the column labels and if the values in the new column
C   are > 100.0 will set them to zero.
C   As of 25 November, 1997 it also takes a single LL polarization data
C   set and converts the 2 polarization BP table into a 1 pol table.
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to check/modify
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C
C   Note, routine will leave no trace of its operation, i.e. BP table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that number of columns has changed if that
C   is required.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, KEYCOL*24, LBTYPE*8
      INTEGER   BUFFER(512), OBUFF(512), IBPRNO, BPKOLS(MAXBPC),
     *   BPNUMV(MAXBPC),  NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN, OVER,
     *   OLUN, OKOLS(MAXBPC), ONUMV(MAXBPC), NBPROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, REFANT(2), OBPRNO, MSGSAV, IDUM, NKEY, LKEY,
     *   KOLS, IIF, NUMSHF, CATI(256), NCOR, TCOR0, NUMPO, NUMV, NUMP,
     *   KLOCS
      REAL      TIMEI, BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), BANDW, LOWSHF,
     *   DELSHF, CATR(256)
      DOUBLE PRECISION TIME, IFFREQ(MAXIF), CATD(128)
      HOLLERITH CATH(256)
      LOGICAL CHSTAT, LORDER, DOSTOK, DOIFFR, DOOLD
      SAVE BNDPAS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFFER, CATI, CATR, CATH, CATD)
      DATA OLUN /45/
C-----------------------------------------------------------------------
C                                       Check STOKES parms
      CALL COPY (256, CATBLK, CATI)
      CALL AXEFND (8, 'STOKES  ', CATI(KIDIM), CATH(KHCTP), KLOCS, I)
      NCOR = CATI(KINAX+KLOCS)
      TIME = CATD(KDCRV+KLOCS) + (1.0-CATR(KRCRP+KLOCS)) *
     *   CATR(KRCIC+KLOCS)
      IF (TIME.GT.0.0D0) TCOR0 = TIME + 0.5D0
      IF (TIME.LE.0.0D0) TCOR0 = TIME - 0.5D0
      DOSTOK = (TCOR0.EQ.-2) .AND. (NCOR.EQ.1)
C                                       Open BP file
      CALL BPINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, IBPRNO,
     *   BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, BCHAN, NUMSHF,
     *   LOWSHF, DELSHF, LBTYPE, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      DOSTOK = DOSTOK .AND. (NUMPOL.EQ.2)
C                                       Is the IFFREQ column still
C                                       present?
      NKEY = 1
      KEYCOL = 'IF FREQ'
      LKEY = 7
      LORDER = .TRUE.
      CALL FNDCOL (NKEY, KEYCOL, LKEY, LORDER, BUFFER, KOLS, IRET)
      DOIFFR = KOLS.GT.0
C                                       column number => format!
      IF (.NOT.DOIFFR) THEN
C                                       Is it new format?
         IF ((BUFFER(10).EQ.8+3*NUMPOL) .OR. (BUFFER(10).EQ.8+4*NUMPOL))
     *      THEN
            DOOLD = .FALSE.
C                                       Check is old format
         ELSE IF (BUFFER(10).NE.7+3*NUMPOL) THEN
            WRITE (MSGTXT,1000) BUFFER(10)
            IRET = 1
            GO TO 990
         ELSE
            DOOLD = .TRUE.
            END IF
         END IF
      IF ((.NOT.DOOLD) .AND. (.NOT.DOIFFR) .AND. (.NOT.DOSTOK)) THEN
         CALL TABIO ('CLOS', 0, IBPRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, OBUFF, IRET)
C                                       Change status
      IF (STAT.EQ.'READ') THEN
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLRD'
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'WRIT'
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C                                       # rows in old table
      NBPROW = BUFFER(5)
C                                       Open up new BP table
      OVER = 0
      NUMPO = NUMPOL
      IF (DOSTOK) NUMPO = 1
      CALL BPINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN, OBPRNO,
     *   OKOLS, ONUMV, NUMANT, NUMPO, NUMIF, NUMFRQ, BCHAN, NUMSHF,
     *   LOWSHF, DELSHF, LBTYPE, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop and copy
      NUMP = NUMPO * NUMIF * NUMFRQ + 1
      NUMV = (NUMP - 1) * 2
      DO 100 I = 1,NBPROW
         CALL TABBP ('READ', BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, TIME, TIMEI, SOURID, SUBA, ANTNO, BANDW,
     *      IFFREQ, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       Reset IFFREQ column if needed
         IF ((DOOLD) .OR. (DOIFFR)) THEN
            DO 10 IIF = 1,NUMIF
               IF (IFFREQ(IIF).GT.100.D0) IFFREQ(IIF) = 0.D0
 10            CONTINUE
            END IF
C                                       move pol 2 to pol 1
         IF (DOSTOK) THEN
            CALL RCOPY (NUMV, BNDPAS(1,NUMP), BNDPAS(1,1))
            CALL RCOPY (NUMIF, WEIGHT(NUMIF+1), WEIGHT(1))
            REFANT(1) = REFANT(2)
            END IF
C                                       write to new file
         CALL TABBP ('WRIT', OBUFF, OBPRNO, OKOLS, ONUMV, NUMIF, NUMFRQ,
     *      NUMPO, TIME, TIMEI, SOURID, SUBA, ANTNO, BANDW, IFFREQ,
     *      FREQID, REFANT, WEIGHT, BNDPAS, IRET)
         IF (IRET.GT.0) GO TO 999
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IBPRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OBPRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'BP', VER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('BP', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CATBLK, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'BP', OVER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'CLWR'
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE,
     *      IDUM, STAT, OBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1080) IRET, 'READ'
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPREFM: UNKNOWN BP FORMAT, # COLS = ',I3)
 1010 FORMAT ('BPREFM: ERROR ',I3,' RETURNED FROM BPINI')
 1020 FORMAT ('BPREFM: ERROR ',I3,' READING OLD FORMAT BP TABLE')
 1080 FORMAT ('BPREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
