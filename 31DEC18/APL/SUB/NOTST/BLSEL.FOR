      SUBROUTINE BLSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, AN, NA, ISUB, JSUB, BIF, EIF, IFQID,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in a BL table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2011
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
C   Copies a subset of IFs in a BL table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I        LUN to use - in
C      LUNO     I        LUN to use - out
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C      BIF      I        BIF used
C      EIF      I        EIF used
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, AN(*), NA, BPOL, EPOL, ISUB, JSUB, BIF, EIF, IFQID,
     *   BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IBLRNO, BLKOLS(MAXBLC), BLNUMV(MAXBLC), OKOLS(MAXBLC),
     *   ONUMV(MAXBLC), NBLROW, I, OVER, ANT1, ANT2, SUBA, OBLRNO, K,
     *   NUMANT, NUMPOL, NUMIF, LNUMIF, SOURID, FREQID, J, NEWPOL, LBIF,
     *   LBPOL, L, M
      LOGICAL   REFMT, GOTIT
      REAL      TIME, FACMUL(2,2,MAXIF), FACADD(2,2,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open BL file
      CALL BLINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IBLRNO,
     *   BLKOLS, BLNUMV, NUMANT, NUMPOL, NUMIF, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NBLROW = BUFFER(5)
C                                       Open up new BL table
      OVER = VER
      LNUMIF = EIF - BIF + 1
      LBIF = BIF
      IF (LNUMIF.LE.0) THEN
         LNUMIF = NUMIF
         LBIF = 1
         END IF
      REFMT = LNUMIF.NE.NUMIF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         LBPOL = 1
         NEWPOL = NUMPOL
         END IF
      REFMT = REFMT .OR. (NEWPOL.NE.NUMPOL)
      CALL BLINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OBLRNO, OKOLS, ONUMV, NUMANT, NEWPOL, LNUMIF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NBLROW
         CALL TABBL ('READ', BUFFER, IBLRNO, BLKOLS, BLNUMV, NUMPOL,
     *      TIME, SOURID, SUBA, ANT1, ANT2, FREQID, FACMUL, FACADD,
     *      IRET)
C                                       Error reading table
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
C                                       check subarray, time
         ELSE IF (IRET.EQ.0) THEN
            IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (JSUB.NE.SUBA))
     *         IRET = -1
            IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *         (IFQID.NE.FREQID)) IRET = -1
C                                       antenna
            IF ((NA.GT.0) .AND. (ANT1.GT.0)) THEN
               GOTIT = .FALSE.
               DO 10 K = 1,NA
                  GOTIT = GOTIT .OR. (ANT1.EQ.AN(K))
 10               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            IF ((NA.GT.0) .AND. (ANT2.GT.0)) THEN
               GOTIT = .FALSE.
               DO 20 K = 1,NA
                  GOTIT = GOTIT .OR. (ANT2.EQ.AN(K))
 20               CONTINUE
               IF (.NOT.GOTIT) IRET = -1
               END IF
            END IF
C                                       This record dropped
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       This record kept
         ELSE
C                                       select IFs
            DO 50 J = 1,LNUMIF
               K = J + LBIF - 1
               DO 40 L = 1,NEWPOL
                  M = L + LBPOL - 1
                  FACMUL(1,L,J) = FACMUL(1,M,K)
                  FACMUL(2,L,J) = FACMUL(2,M,K)
 40               CONTINUE
 50            CONTINUE
C                                       Write new one
            IF ((JSUB.GE.0) .AND. (SUBA.GT.0)) SUBA = JSUB
            IF (IFQID.GT.0) FREQID = 1
            CALL TABBL ('WRIT', OBUFF, OBLRNO, OKOLS, ONUMV, NEWPOL,
     *         TIME, SOURID, SUBA, ANT1, ANT2, FREQID, FACMUL, FACADD,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IBLRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OBLRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted BL', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied BL', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('BLSEL: ERROR ',I3,' RETURNED FROM BLINI')
 1020 FORMAT ('BLSEL: ERROR ',I3,' RETURNED FROM TABBL')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
