      SUBROUTINE SUSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, IFQID, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C! Copies a subset of IFs in an SU table
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2007, 2012
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
C   Copies a subset of IFs in an SU table.
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      IFQID           I       FQ ID to select
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256),
     *   CATOUT(256), LUNI, LUNO, BIF, EIF, IFQID, BUFFER(*),
     *   OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   ISURNO, SUKOLS(MAXSUC), SUNUMV(MAXSUC), NUMIF,
     *   OKOLS(MAXSUC), ONUMV(MAXSUC), NSUROW, I, IIF, JIF, OSURNO,
     *   NEWNIF, IDSOU, QUAL, SUFQID, OVER
      LOGICAL   REFMT
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open SU file
      CALL SOUINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *  NUMIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       New no. of IFs
      NEWNIF = MAX (MIN (NUMIF, EIF) - BIF + 1, 0)
      REFMT = NEWNIF.NE.NUMIF
C                                       # rows in old table
      NSUROW = BUFFER(5)
C                                       Open up new SU table
      OVER = VER
      IF (SUFQID.EQ.-999) SUFQID = -1
C                                       Check selected FQ ID
      IF ((SUFQID.EQ.IFQID).AND.(IFQID.GT.0)) SUFQID = 1
      IF ((SUFQID.NE.IFQID).AND.(IFQID.GT.0)) SUFQID = -1
C
      CALL SOUINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *  NEWNIF, VELTYP, VELDEF, SUFQID, OSURNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NSUROW
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         REFMT = REFMT .OR. (IRET.LT.0)
C                                       De-selected record
         IF ((IRET.EQ.0) .AND. (NEWNIF.GT.0)) THEN
C                                       Select IFs
            DO 90 JIF = 1, NEWNIF
               IIF = JIF + BIF - 1
               FREQO(JIF) = FREQO(IIF)
               LSRVEL(JIF) = LSRVEL(IIF)
               LRESTF(JIF) = LRESTF(IIF)
               FLUX(1,JIF) = FLUX(1,IIF)
               FLUX(2,JIF) = FLUX(2,IIF)
               FLUX(3,JIF) = FLUX(3,IIF)
               FLUX(4,JIF) = FLUX(4,IIF)
 90            CONTINUE
C                                       Write new one
            CALL TABSOU ('WRIT', OBUFF, OSURNO, OKOLS, ONUMV, IDSOU,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, ISURNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OSURNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted SU', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied SU', DISKI, CNOI, VER, DISKO,
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
 1010 FORMAT ('SUSEL: ERROR ',I3,' RETURNED FROM SOUINI')
 1020 FORMAT ('SUSEL: ERROR ',I3,' RETURNED FROM TABSOU')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
