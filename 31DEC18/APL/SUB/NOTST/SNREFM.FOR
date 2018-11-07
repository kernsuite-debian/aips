      SUBROUTINE SNREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of SN table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2012, 2015
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
C   Routine to change the format of the SN table prior to SNREV from
C   PSNTAB.INC
C   NOTE: routine uses LUNs 45 & 46 as a temporary logical unit number.
C      For versions prior to revision 10 the reference frequency for IF
C   phases is changed to the ref. channel in the IF rather than the
C   reference frequency for the array.
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
C   Note, routine will leave no trace of its operation, i.e. SN table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that the table will be in the new format.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, KEYW*8, BNDCOD(MAXIF)*8
      INTEGER BUFFER(1024), ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMANT, NUMPOL, NUMIF, NUMNOD, OVER, OLUN, REVNO,
     *   OBUFF(1024), OKOLS(MAXSNC), ONUMV(MAXSNC), NSNROW, I, IIF,
     *   SOURID, ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF),
     *   OSNRNO, MSGSAV, IDUM, KLOCS(1), KEYVAL(1), KEYTYP, JERR
      LOGICAL ISAPPL, CHSTAT, TABLE, EXIST, FITASC
      REAL    GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), DISP(2), DDISP(2), TWOPI
      DOUBLE PRECISION TIME
C                                       CHNDAT arguments
      INTEGER   FQLUN, FQNIF, FQBUFF(512), FQFQID, FQVER, FQSBND(MAXIF)
      REAL      FQFINC(MAXIF), FQPHS, FQRE, FQIM, FQTRE, FQTIM
      DOUBLE PRECISION FQFOFF(MAXIF)
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
      DATA TWOPI /6.283185308/
C-----------------------------------------------------------------------
C                                       Check existance
      CALL ISTAB ('SN', DISK, CNO, VER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IRET)
      IRET = 0
      IF (.NOT.EXIST) GO TO 999
C                                       Open SN file
      CALL SNINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Is it new format?
C                                       Read REVISION keyword
      KEYW = 'REVISION'
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', KEYW, 1, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *   JERR)
      MSGSUP = MSGSAV
      REVNO = -1
      IF ((JERR.EQ.0) .AND. (KLOCS(1).GT.0)) REVNO = KEYVAL(KLOCS(1))
C                                       Current revision (from PSNTAB)?
      IF (REVNO.GE.SNREV) THEN
         CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Reformat table; should be able
C                                       to read old tables with TABSN.
      WRITE (MSGTXT,1070) VER
      CALL MSGWRT (6)
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, CTEMP, CTEMP, IDUM, UTYPE, IDUM,
     *   STAT, OBUFF, IRET)
      IF (STAT.EQ.'READ') THEN
C                                       Change status
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
      NSNROW = BUFFER(5)
C                                       Setup
      FQFQID = -999
      FQLUN = 46
      FQVER = 1
C                                       Open up new SN table
      OVER = 0
      CALL SNINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN,
     *   OSNRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop and copy
      DO 800 I = 1, NSNROW
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.NE.0) GO TO 999
C                                       Modifications
         IF (REVNO.LT.10) THEN
C                                       New frequency table
            IF ((FREQID.GT.0) .AND. (FREQID.NE.FQFQID)) THEN
               FQFQID = FREQID
               CALL CHNDAT ('READ', FQBUFF, DISK, CNO, FQVER, CATBLK,
     *            FQLUN, FQNIF, FQFOFF, FQSBND, FQFINC, BNDCOD, FQFQID,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Change ref. frequency for IF
C                                       phase.
            DO 50 IIF = 1,FQNIF
               IF ((CREAL(1,IIF).NE.FBLANK) .AND.
     *            (DELAY(1,IIF).NE.FBLANK)) THEN
                  FQPHS = TWOPI * FQFOFF(IIF) * DELAY(1,IIF)
                  FQRE = COS (FQPHS)
                  FQIM = SIN (FQPHS)
                  FQTRE = CREAL(1,IIF)
                  FQTIM = CIMAG(1,IIF)
                  CREAL(1,IIF) = FQRE * FQTRE - FQIM * FQTIM
                  CIMAG(1,IIF) = FQRE * FQTIM + FQIM * FQTRE
                  END IF
 50            CONTINUE
            IF (NUMPOL.GT.1) THEN
               DO 60 IIF = 1,FQNIF
                  IF ((CREAL(2,IIF).NE.FBLANK) .AND.
     *               (DELAY(2,IIF).NE.FBLANK)) THEN
                     FQPHS = TWOPI * FQFOFF(IIF) * DELAY(2,IIF)
                     FQRE = COS (FQPHS)
                     FQIM = SIN (FQPHS)
                     FQTRE = CREAL(2,IIF)
                     FQTIM = CIMAG(2,IIF)
                     CREAL(2,IIF) = FQRE * FQTRE - FQIM * FQTIM
                     CIMAG(2,IIF) = FQRE * FQTIM + FQIM * FQTRE
                     END IF
 60               CONTINUE
               END IF
            END IF
C                                       Write new table.
         CALL TABSN ('WRIT', OBUFF, OSNRNO, OKOLS, ONUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
         IF (IRET.NE.0) GO TO 999
 800     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSNRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'SN', VER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('SN', OVER, VER, OLUN, LUN, DISK, DISK, CNO, CNO,
     *   CATBLK, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'SN', OVER, CATBLK, BUFFER, IRET)
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
 1010 FORMAT ('SNREFM: ERROR ',I3,' RETURNED FROM SNINI')
 1070 FORMAT ('SNREFM: Reformatting SN table ',I3)
 1080 FORMAT ('SNREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
