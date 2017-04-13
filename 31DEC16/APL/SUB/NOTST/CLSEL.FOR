      SUBROUTINE CLSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, TB, TE, NSOU, SOUIND,
     *   AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in a CL table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2007, 2011
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
C   Copies a subset of IFs in a CL table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                           if <= 0 then output value unchanged.
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      NSOU     I        Number of selected sources
C      SOUIND   I(*)     Array of sources indexes selected
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, IFQID, AN(*), NA, SOUIND(*),
     *   NSOU, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NUMPOL,
     *   NUMIF, OKOLS(MAXCLC), ONUMV(MAXCLC), NCLROW, I, SOURID, ANTNO,
     *   SUBA, FREQID, REFA(2,MAXIF), OCLRNO, NTERM, NEWNIF, IIF, JIF,
     *   IPOL, K, OVER, LBPOL, NEWPOL, LBIF
      REAL      GMMOD, TIMEI, IFR, ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), DOPOFF(MAXIF), CREAL(2,MAXIF),
     *   CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF)
      DOUBLE PRECISION TIME, GEODLY(10)
      LOGICAL   GOTIT, REFMT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open CL file
      CALL CALINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - LBIF + 1
      IF (NEWNIF.LE.0) THEN
         LBIF = 1
         NEWNIF = NUMIF
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         LBPOL = 1
         NEWPOL = NUMPOL
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       # rows in old table
      NCLROW = BUFFER(5)
C                                       Open up new CL table
      OVER = VER
      CALL CALINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCLRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWNIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NCLROW
         CALL TABCAL ('READ', BUFFER, ICLRNO, CLKOLS, CLNUMV,
     *      NUMPOL, NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *      DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
C                                       Error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Time selection
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
C                                       Sources selection
         IF ((NSOU.GT.0) .AND. (SOURID.GT.0)) THEN
            GOTIT = .FALSE.
            DO 20 K = 1,NSOU
               GOTIT = GOTIT .OR. (SOURID.EQ.SOUIND(K))
 20            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (ANTNO.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (ANTNO.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -1
C                                       Suba selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            DO 90 JIF = 1,NEWNIF
               IIF = JIF + LBIF - 1
               DOPOFF(JIF) = DOPOFF(IIF)
               DO 80 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  CREAL(IPOL,JIF) = CREAL(K,IIF)
                  CIMAG(IPOL,JIF) = CIMAG(K,IIF)
                  DELAY(IPOL,JIF) = DELAY(K,IIF)
                  RATE(IPOL,JIF) = RATE(K,IIF)
                  WEIGHT(IPOL,JIF) = WEIGHT(K,IIF)
                  REFA(IPOL,JIF) = REFA(K,IIF)
 80               CONTINUE
 90            CONTINUE
C
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
            CALL TABCAL ('WRIT', OBUFF, OCLRNO, OKOLS, ONUMV, NEWPOL,
     *         NEWNIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *         GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK,
     *         DISP, DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OCLRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted CL', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied CL', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('CLSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('CLSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('CLSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
