      SUBROUTINE MCSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, FREQID, TB, TE, NSOU, SOUIND,
     *   AN, NA, ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies an MC table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2007, 2011
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
C   Copies a subset of IFs in a MC table, can also modify the FQ ID
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
     *   LUNI, LUNO, BPOL, EPOL,  BIF, EIF, FREQID, NSOU, SOUIND(*),
     *   AN(*), NA, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION TIME, ATMOS, DATMOS, GDELAY, GRATE, CLOCK(2),
     *   RFREQ, DCLOCK(2)
      REAL      LOOFF(2,MAXIF), DLOOFF(2,MAXIF), DISP(2), DDISP(2),
     *   CHANBW, REFPIX, DELTAT
      INTEGER   NUMIF, SOURID, ANTNO, SUBA, FQID, IMCRNO, MCKOLI(21),
     *   MCNUMI(21), MCKOLO(21), MCNUMO(21), NSTOKE, STOKE1, NCHAN,
     *   NUMPOL, FFTSIZ, OVRSMP, ZEROPD, OMCRNO, LBPOL, NEWPOL, LBIF
      CHARACTER OBSCOD*8, RDATE*8, TAPER*8
      INTEGER   JIF, IIF, IPOL, NMCROW, I, NEWNIF, K, OVER
      LOGICAL   GOTIT, REFMT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Open IM file
      CALL MCINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IMCRNO,
     *   MCKOLI, MCNUMI, OBSCOD, RDATE, NSTOKE, STOKE1, NUMIF, NCHAN,
     *   RFREQ, CHANBW, REFPIX, NUMPOL, FFTSIZ, OVRSMP, ZEROPD, TAPER,
     *   DELTAT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWNIF = MIN (NUMIF, EIF) - LBIF + 1
      IF (NEWNIF.LE.0) THEN
         NEWNIF = NUMIF
         LBIF = 1
         END IF
      REFMT = (NEWNIF.NE.NUMIF) .OR. (NEWPOL.NE.NUMPOL)
C                                       Numbers of rows in old table
      NMCROW = BUFFER(5)
C                                       Open up new IM table
      OVER = VER
      CALL MCINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OMCRNO, MCKOLO, MCNUMO, OBSCOD, RDATE, NSTOKE, STOKE1, NEWNIF,
     *   NCHAN, RFREQ, CHANBW, REFPIX, NEWPOL, FFTSIZ, OVRSMP, ZEROPD,
     *   TAPER, DELTAT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NMCROW
         CALL MCTAB ('READ', BUFFER, IMCRNO, MCKOLI, MCNUMI, NUMPOL,
     *      NUMIF, TIME, SOURID, ANTNO, SUBA, FQID, ATMOS, DATMOS,
     *      GDELAY, GRATE, CLOCK, DCLOCK, LOOFF, DLOOFF, DISP, DDISP,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', I
            GO TO 990
            END IF
C                                       Time selection
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -1
C                                       FQ selection
         IF ((FQID.GT.0) .AND. (FREQID.GT.0) .AND. (FQID.NE.FREQID))
     *      IRET = -1
C                                       subarray selection
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -1
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
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            DO 50 JIF = 1,NEWNIF
               IIF = JIF + BIF - 1
               DO 40 IPOL = 1,NEWPOL
                  K = IPOL + LBPOL - 1
                  LOOFF(IPOL,JIF) = LOOFF(K,IIF)
                  DLOOFF(IPOL,JIF) = DLOOFF(K,IIF)
 40               CONTINUE
 50            CONTINUE
C
            IF (FREQID.GT.0) FQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
            CALL MCTAB ('WRIT', OBUFF, OMCRNO, MCKOLO, MCNUMO, NEWPOL,
     *         NEWNIF, TIME, SOURID, ANTNO, SUBA, FQID, ATMOS, DATMOS,
     *         GDELAY, GRATE, CLOCK, DCLOCK, LOOFF, DLOOFF, DISP, DDISP,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', OMCRNO
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IMCRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OMCRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted MC', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied MC', DISKI, CNOI, VER, DISKO,
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
 1000 FORMAT ('MCSEL: ERROR ',I3,1X,A,'ING TABLE ROW',I8)
 1010 FORMAT ('MCSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('MCSEL: ERROR ',I3,' INITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
