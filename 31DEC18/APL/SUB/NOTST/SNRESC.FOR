      SUBROUTINE SNRESC (DISK, CNO, VER, CATBLK, LUN, IRET)
C-----------------------------------------------------------------------
C! Applies mean gain modulus to an SN table reseting it in header
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2000, 2015
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
C   Applies mean gain modulus to an SN table reseting it from header
C   NOTE: routine uses LUN 45 as a temporary logical unit number.
C   Inputs:
C      DISK            I       Volume number
C      CNO             I       Catalogue number
C      VER             I       Version to check/modify
C      CATBLK(256)     I       Catalogue header
C      LUN             I       LUN to use
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISK, CNO, VER, CATBLK(256), LUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYWRD*8
      INTEGER   BUFFER(512), ISNRNO, SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMANT, NUMPOL, NUMIF, NUMNOD, IP, IPM, NSNROW, I, IIF, SOURID,
     *   ANTNO, SUBA, FREQID, NODENO, REFA(2,MAXIF), KLOCS, KEYTYP,
     *   NUMKEY
      LOGICAL ISAPPL, TABLE, EXIST, FITASC
      REAL    GMMOD, RANOD(25), DECNOD(25), TIMEI, IFR, MBDELY(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF), DISP(2), DDISP(2)
      DOUBLE PRECISION TIME
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Check existance
      CALL ISTAB ('SN', DISK, CNO, VER, LUN, BUFFER, TABLE, EXIST,
     *   FITASC, IRET)
      IRET = 0
      IF (.NOT.EXIST) GO TO 999
C                                       Open SN file
      CALL SNINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, ISNRNO,
     *   SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN SN FOR READ'
         GO TO 990
         END IF
C                                       No gain modulus to apply
      IF (ABS(GMMOD-1.0).LT.1.E-5) GO TO 900
C                                       Reformat table; should be able
C                                       to read old tables with TABSN.
      WRITE (MSGTXT,1010) VER, GMMOD
      CALL MSGWRT (4)
      GMMOD = 1.0 / GMMOD
C                                       # rows in old table
      NSNROW = BUFFER(5)
      IPM = MIN (2, NUMPOL)
C                                       Loop and copy
      DO 100 I = 1,NSNROW
         ISNRNO = I
         CALL TABSN ('READ', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *      TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *      MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *      REFA, IRET)
C                                       Deselected record.
         IF (IRET.GT.0) THEN
            GO TO 999
         ELSE IF (IRET.EQ.0) THEN
            DO 30 IIF = 1,NUMIF
               DO 20 IP = 1,IPM
                  IF ((CREAL(IP,IIF).NE.FBLANK) .AND.
     *               (DELAY(IP,IIF).NE.FBLANK)) THEN
                     CREAL(IP,IIF) = CREAL(IP,IIF) * GMMOD
                     CIMAG(IP,IIF) = CIMAG(IP,IIF) * GMMOD
                     END IF
 20               CONTINUE
 30            CONTINUE
C                                       re-write record
            ISNRNO = I
            CALL TABSN ('WRIT', BUFFER, ISNRNO, SNKOLS, SNNUMV, NUMPOL,
     *         TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO,
     *         MBDELY, DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT,
     *         REFA, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       update GMMOD
      GMMOD = 1.0
      KEYWRD = 'MGMOD'
      NUMKEY = 1
      KLOCS = 1
      KEYTYP = 2
      CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KLOCS, GMMOD, KEYTYP,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RESETTING MGMOD TO 1.0'
         GO TO 990
         END IF
C                                       close
 900  CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSE SN FOR READ'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNRESC: ERROR',I5,' AT ',A)
 1010 FORMAT ('SNRESC: Rescaling SN table',I5,' by mean gain',F7.3)
      END
