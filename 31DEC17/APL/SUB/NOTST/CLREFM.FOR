      SUBROUTINE CLREFM (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Checks existence of CL table, changes format if necessary
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2012
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
C   Routine to change the format of the CL table prior to CLREV from
C   PCLTAB.INC
C   NOTE: routine uses LUNs 45 &46 as a temporary logical unit number.
C      For versions prior to revision 10 the reference frequency for IF
C   phases is changed to the ref. channel in the IF rather than the
C   reference frequency for the array.  Also changes GEODELAY, GEOPHS,
C   and GEORATE to GEODELAY polynomial; ATMGD, DATMGD to ATMOS, DATMOS
C   and CLKPD, DCLKPD to CLOCK n, DCLOCK n.
C
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to check/modify
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C   Note, routine will leave no trace of its operation, i.e. CL table
C   will be closed on output and will have same number as one specified.
C   Difference will be only that the table will be in the new format.
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CTEMP*12, UTYPE*2, STAT*4, KEYW*8, OLDCOL(8)*24,
     *   BNDCOD(MAXIF)*8
      INTEGER BUFFER(512), ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   NUMANT, NUMPOL, NUMIF, NTERM, OVER, OLUN, OBUFF(512),
     *   OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID, ANTNO, SUBA,
     *   FREQID, REFA(2,MAXIF), OCLRNO, MSGSAV, IDUM, KLOCS(1), IIF,
     *   KEYVAL(1), KEYTYP, JERR, REVNO, KOLS(8), NKEY, NREC, NCOL,
     *   DATP(2,128), RTYPE, SCRTCH(XCLRSZ), LCLRNO
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2),
     *   CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), GMMOD, TWOPI, RVAL(MAXIF)
      LOGICAL CHSTAT, TABLE, EXIST, FITASC
      DOUBLE PRECISION TIME, GEODLY(20), DVAL(MAXIF)
C                                       CHNDAT arguments
      INTEGER   FQLUN, FQNIF, FQBUFF(512), FQFQID, FQVER, FQSBND(MAXIF)
      REAL      FQFINC(MAXIF), FQPHS, FQRE, FQIM, FQTRE, FQTIM
      DOUBLE PRECISION FQFOFF(MAXIF)
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA OLUN /45/
      DATA TWOPI /6.283185308/
      DATA OLDCOL /'GEOPHASE  ', 'GEORATE                 ',
     *   'ATMGD 1                 ', 'DATMGD 1                ',
     *   'CLKPD 1                 ', 'DCLKPD 1                ',
     *   'CLKPD 2                 ', 'DCLKPD 2                '/
C-----------------------------------------------------------------------
C                                       Check existance
      CALL ISTAB ('CL', DISK, CNO, VER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IRET)
      IRET = 0
      IF (.NOT.EXIST) GO TO 999
C                                       Open CL file
      CALL CALINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN, ICLRNO,
     *   CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Is it new format?
C                                       Read REVISION keyword
      KEYW = 'REVISION'
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABKEY ('READ', KEYW, 1, BUFFER, KLOCS, KEYVAL, KEYTYP,
     *   JERR)
      MSGSUP = MSGSAV
      REVNO = -1
      IF ((JERR.EQ.0) .AND. (KLOCS(1).GT.0)) REVNO = KEYVAL(KLOCS(1))
C                                       Current revision (from PCLTAB)?
      IF (REVNO.GE.CLREV) THEN
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IRET)
         GO TO 999
         END IF
C                                       Reformat table; should be able
C                                       to read old tables with TABCAL.
      WRITE (MSGTXT,1000) VER
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
      NCLROW = BUFFER(5)
C                                       Setup
      IF (REVNO.LT.10) THEN
         FQFQID = -999
         FQLUN = 46
         FQVER = 1
         NTERM = 3
C                                       Column pointers for obselete
C                                       cols.
         NKEY = 0
         NREC = 0
         NCOL = 0
         CALL TABINI ('READ', 'CL', DISK, CNO, VER, CATBLK, OLUN, NKEY,
     *      NREC, NCOL, DATP, OBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL FNDCOL (8, OLDCOL, 24, .TRUE., OBUFF, KOLS, IRET)
         IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 970
         CALL TABIO ('CLOS', 0, ICLRNO, OBUFF, OBUFF, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Open up new CL table
      OVER = 0
      CALL CALINI ('WRIT', OBUFF, DISK, CNO, OVER, CATBLK, OLUN,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      LCLRNO = 0
      DO 800 I = 1,NCLROW
         ICLRNO = I
         CALL TABCAL ('READ', BUFFER, ICLRNO, CLKOLS, CLNUMV, NUMPOL,
     *      NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C                                       Deselected record.
         IF (IRET.LT.0) GO TO 800
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Modifications
         IF (REVNO.LT.10) THEN
C                                       New frequency table?
            IF ((FREQID.GT.0) .AND. (FREQID.NE.FQFQID)) THEN
               FQFQID = FREQID
               CALL CHNDAT ('READ', FQBUFF, DISK, CNO, FQVER, CATBLK,
     *            FQLUN, FQNIF, FQFOFF, FQSBND, FQFINC, BNDCOD, FQFQID,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Read obselete columns.
            ICLRNO = I
C                                       GEOPHASE
            IF (KOLS(1).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(1), DATP, LCLRNO, BUFFER,
     *            RTYPE, DVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               GEODLY(2) = DVAL(1)
               END IF
C                                       GEORATE
            IF (KOLS(2).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(2), DATP, LCLRNO, BUFFER,
     *            RTYPE, DVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               GEODLY(3) = DVAL(1)
               END IF
C                                       ATMGD 1
            IF (KOLS(3).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(3), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               ATMOS = RVAL(1)
               END IF
C                                       DATMGD 1
            IF (KOLS(4).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(4), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               DATMOS = RVAL(1)
               END IF
C                                       CLKPD 1
            IF (KOLS(5).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(5), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               CLOCK(1) = RVAL(1)
               END IF
C                                       DCLKPD 1
            IF (KOLS(6).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(6), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               DCLOCK(1) = RVAL(1)
               END IF
C                                       CLKPD 1
            IF (KOLS(7).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(7), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               CLOCK(2) = RVAL(1)
               END IF
C                                       DCLKPD 1
            IF (KOLS(8).GT.0) THEN
               CALL GETCOL (ICLRNO, KOLS(8), DATP, LCLRNO, BUFFER,
     *            RTYPE, RVAL, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 980
               DCLOCK(2) = RVAL(1)
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
         CALL TABCAL ('WRIT', OBUFF, OCLRNO, OKOLS, ONUMV,
     *      NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 990
            END IF
 800     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OCLRNO, OBUFF, OBUFF, IRET)
C                                       Delete the original file
      CALL RMEXT (DISK, CNO, 'CL', VER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Copy new file to place
C                                       occupied by old one
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('CL', OVER, VER, OLUN, LUN, DISK, DISK,
     *   CNO, CNO, CATBLK, OBUFF, BUFFER, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 990
         END IF
C                                       Delete the now defunct
C                                       original output file
      CALL RMEXT (DISK, CNO, 'CL', OVER, CATBLK, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1070) IRET
         GO TO 990
         END IF
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
C                                       Error finding old columns
 970  MSGTXT = 'CLREFM: ERROR FINDING COLUMNS IN OLD CL TABLE'
      GO TO 990
C                                       Error reading old column
 980  MSGTXT = 'CLREFM: ERROR READING COLUMN IN OLD CL TABLE'
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLREFM: Reformatting CL table ',I3)
C 1000 FORMAT ('CLREFM: UNKNOWN CL FORMAT, # COLS = ',I3)
 1010 FORMAT ('CLREFM: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CLREFM: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CLREFM: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CLREFM: ERROR ',I3,' WRITING NEW TABLE')
 1050 FORMAT ('CLREFM: ERROR ',I3,' DELETING OLD TABLE')
 1060 FORMAT ('CLREFM: ERROR ',I3,' COPYING NEW TABLE')
 1070 FORMAT ('CLREFM: ERROR ',I3,' DELETING SCRATCH TABLE')
 1080 FORMAT ('CLREFM: ERROR ',I3,' CHANGING ',A4,' STATUS')
      END
