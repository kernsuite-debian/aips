      SUBROUTINE GCSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BPOL, EPOL, BCHAN, ECHAN, BIF, EIF, IFQID, AN, NA,
     *   ISUB, JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in a GC table
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
C   Copies a subset of IFs in a GC table, can also modify the FQ ID
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BPOL     I        First polarization to copy
C      EPOL     I        Last polarization to copy
C      BCHAN    I        Start spectral channel
C      ECHAN    I        End spectral channel
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      IFQID    I        FQ ID to select (set to 1 on output)
C                        if <= 0 then output value unchanged.
C      AN       I(*)     Array of selected antennas
C      NA       I        Number of selected antennas
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      CATOUT   I(256)   Output catalog header
C      BUFFER   I(*)     Work buffer
C      OBUFF    I(*)     Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BPOL, EPOL, BCHAN, ECHAN, BIF, EIF, IFQID, AN(*),
     *   NA, ISUB, JSUB, BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INTEGER   IGCRNO, GCKOLS(MAXGCC), GCNUMV(MAXGCC), OKOLS(MAXGCC),
     *   ONUMV(MAXGCC), NGCROW, I, OGCRNO, IS, IIF, JIF, IPOL, K, OVER,
     *   NPOLGC, NTABGC, NOBAND, NOCHAN, NEWBND, NEWCHN, NEWPOL, LBIF,
     *   IANTGC, ISUBGC, IFQDGC, ITPGC(2,MAXIF), NTGC(2,MAXIF),
     *   IXTGC(2,MAXIF), IYTGC(2,MAXIF), LBPOL
      REAL      REFPIX, XVALGC(2,MAXIF), YVALGC(2,MAXIF,MXTBGC),
     *   GAINGC(2,MAXIF,MXTBGC), SENSGC(2,MAXIF)
      LOGICAL   GOTIT, REFMT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open GC file
      CALL GCINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *   IGCRNO, GCKOLS, GCNUMV, NPOLGC, NOBAND, NTABGC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IF's
      LBIF = MAX (1, BIF)
      NEWBND = MIN (NOBAND, EIF) - LBIF + 1
      IF (NEWBND.LE.0) THEN
         NEWBND = NOBAND
         LBIF = 1
         END IF
      LBPOL = MAX (1, BPOL)
      NEWPOL = MIN (2, MIN (NPOLGC, EPOL)) - LBPOL + 1
      IF (NEWPOL.LE.0) THEN
         LBPOL = 1
         NEWPOL = NPOLGC
         END IF
      NEWCHN = MAX (MIN (NOCHAN, ECHAN) - BCHAN + 1, 0)
      REFPIX = REFPIX - BCHAN + 1
      REFMT = (NEWBND.NE.NOBAND) .OR. (NEWCHN.NE.NOCHAN) .OR.
     *   (NEWPOL.NE.NPOLGC)
C                                       # rows in old table
      NGCROW = BUFFER(5)
C                                       Open up new GC table
      OVER = VER
      CALL GCINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OGCRNO, OKOLS, ONUMV, NEWPOL, NEWBND, NTABGC, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1,NGCROW
         CALL TABGC ('READ', BUFFER, IGCRNO, GCKOLS, GCNUMV, NPOLGC,
     *      NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC, IXTGC, IYTGC,
     *      XVALGC, YVALGC, GAINGC, SENSGC, IRET)
C                                       Error
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1030) IRET
            GO TO 990
            END IF
C                                       Antennas selection
         IF ((NA.GT.0) .AND. (IANTGC.GT.0)) THEN
            GOTIT = .FALSE.
            DO 30 K = 1,NA
               GOTIT = GOTIT .OR. (IANTGC.EQ.AN(K))
 30            CONTINUE
            IF (.NOT.GOTIT) IRET = -1
            END IF
C                                       FQ selection
         IF ((IFQDGC.GT.0) .AND. (IFQID.GT.0) .AND. (IFQDGC.NE.IFQID))
     *      IRET = -1
C                                       subarray selection
         IF ((ISUBGC.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.ISUBGC))
     *      IRET = -1
C                                       Is this record selected ?
         IF (IRET.LT.0) THEN
            REFMT = .TRUE.
C                                       Re-number IF's
         ELSE
            DO 90 JIF = 1,NEWBND
               IIF = JIF + LBIF - 1
               DO 80 IPOL = 1,NPOLGC
                  K = IPOL + LBPOL - 1
                  ITPGC(IPOL,JIF) = ITPGC(K,IIF)
                  NTGC(IPOL,JIF)  = NTGC(K,IIF)
                  IXTGC(IPOL,JIF) = IXTGC(K,IIF)
                  IYTGC(IPOL,JIF) = IYTGC(K,IIF)
                  XVALGC(IPOL,JIF) = XVALGC(K,IIF)
                  SENSGC(IPOL,JIF) = SENSGC(K,IIF)
                  DO 70 IS = 1,NTABGC
                     YVALGC(IPOL,JIF,IS) = YVALGC(K,IIF,IS)
                     GAINGC(IPOL,JIF,IS) = GAINGC(K,IIF,IS)
 70                  CONTINUE
 80               CONTINUE
 90            CONTINUE
C
            IF (IFQID.GT.0) IFQDGC = 1
            IF (JSUB.GE.0) ISUBGC = JSUB
            CALL TABGC ('WRIT', OBUFF, OGCRNO, OKOLS, ONUMV, NEWPOL,
     *         NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC, NTGC, IXTGC,
     *         IYTGC, XVALGC, YVALGC, GAINGC, SENSGC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
      IRET = 0
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IGCRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OGCRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted GC', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied GC', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('GCSEL: ERROR ',I3,' INITING OLD TABLE')
 1020 FORMAT ('GCSEL: ERROR ',I3,' INITING NEW TABLE')
 1030 FORMAT ('GCSEL: ERROR ',I3,' READING OLD TABLE')
 1040 FORMAT ('GCSEL: ERROR ',I3,' WRITING NEW TABLE')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
