      SUBROUTINE PDSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, BCHAN, ECHAN, IFQID, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies and renumbers the IFs in a PD table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007, 2010-2012, 2015
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
C   Copies and renumbers the IFs and channels in an PD table; can also
C   modify the FQ ID.
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
C      BCHAN    I        First channel
C      ECHAN    I        Last channel
C      IFQID    I        FQ ID to select (output FQID will be 1)
C                           if <= 0 then output FQ ID unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(512)   Work buffer
C      OBUFF    I(512)   Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BIF, EIF, BCHAN, ECHAN, IFQID, ISUB,
     *   JSUB, BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPDC
      PARAMETER (MAXPDC = 9)
C
      REAL      DTERMS(2,MAXCIF), PHDIFF(MAXCIF)
      INTEGER   IPDRNO, PDKOLS(MAXPDC), PDNUMV(MAXPDC), OKOLS(MAXPDC),
     *   ONUMV(MAXPDC), NPDROW, OPDRNO, SUBA, FREQID, REFANT(2), NUMANT,
     *   NUMPOL, NUMIF, NUMFRQ, ANT, I, J, K, L, JJ, NEWIF, NEWFRQ, IFA,
     *   IFB, IFREQA, IFREQB, INX, LNX, OVER, FOPEN, LBPOL, M, NEWPOL
      LOGICAL   REFMT
      CHARACTER POLTYP*8
      SAVE DTERMS, PHDIFF
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      FOPEN = 0
C                                       Open PD file
      CALL PDINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IPDRNO,
     *   PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT TABLE'
         GO TO 990
         END IF
      FOPEN = 1
      REFMT = .FALSE.
      IFREQA = MAX (BCHAN, 1)
      IFREQB = MIN (NUMFRQ, ECHAN)
      NEWFRQ = IFREQB - IFREQA + 1
C                                       New PD table will cover
C                                       IF range [IFA,IFB].
      IFA = MAX (1, BIF)
      IFB = MIN (NUMIF, EIF)
      NEWIF = IFB - IFA + 1
      IF (NEWIF.LE.0) THEN
         IFA = 1
         NEWIF = NUMIF
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NUMPOL, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         NEWPOL = NUMPOL
         LBPOL = 1
         END IF
C                                       # rows in old table
      NPDROW = BUFFER(5)
C                                       Open up new PD table
      OVER = VER
      CALL PDINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OPDRNO, OKOLS, ONUMV, NUMANT, NEWPOL, NEWIF, NEWFRQ, POLTYP,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN NEW TABLE'
         GO TO 990
         END IF
C                                       Loop and copy
      FOPEN = 2
      DO 300 I = 1,NPDROW
         CALL TABPD ('READ', BUFFER, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF, DTERMS,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OLD TABLE'
            GO TO 990
            END IF
C                                       Is this FQ ID selected ?
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -10
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -10
         IF ((IRET.LT.-3) .OR. (NEWFRQ.LE.0)) THEN
            REFMT = .TRUE.
C                                       Copy this record
         ELSE
            DO 40 L = 1,NEWPOL
               M = L + LBPOL - 1
               DO 30 J = 1,NEWIF
                  JJ = IFA + J - 1
                  INX = (J - 1) * NEWFRQ + (M - 1) * NEWFRQ * NEWIF
                  LNX = (JJ - 1) * NUMFRQ + (L - 1) * NUMFRQ * NUMIF +
     *               IFREQA - 1
                  IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
                  DO 20 K = 1,NEWFRQ
                     INX = INX + 1
                     LNX = LNX + 1
                     DTERMS(1,INX) = DTERMS(1,LNX)
                     DTERMS(2,INX) = DTERMS(2,LNX)
                     IF (L.EQ.1) PHDIFF(INX) = PHDIFF(LNX)
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
C                                       Set output FQ ID
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
C                                       Write output record.
            CALL TABPD ('WRIT', OBUFF, OPDRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, NEWPOL, ANT, SUBA, FREQID, REFANT, PHDIFF,
     *         DTERMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW TABLE'
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted PD', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied PD', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (6)
C                                       close
 995  IF (FOPEN.GT.0) CALL TABIO ('CLOS', 0, IPDRNO, BUFFER, BUFFER, I)
      IF (FOPEN.GT.1) CALL TABIO ('CLOS', 0, OPDRNO, OBUFF, OBUFF, J)
      IF (IRET.EQ.0) IRET = MAX (I, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PDSEL: ERROR ',I3,1X,A)
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
