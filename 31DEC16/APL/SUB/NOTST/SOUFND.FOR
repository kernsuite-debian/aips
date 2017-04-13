      SUBROUTINE SOUFND (DISK, CNO, CATBLK, MAXSOU, SOURCE, SELQUA,
     *   SELCOD, NSOUWD, DOSWNT, SOUWAN, IERR)
C-----------------------------------------------------------------------
C! Fills in arrays of source numbers to be included or excluded.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2012
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
C   Fills in arrays of source numbers to be included or excluded.
C   Selection is by name, qualifier and Calcode.
C   If no SU table exists then NSOUWD=0 is returned.
C   Inputs:
C      DISK    I        Disk number for SU table
C      CNO     I        Catalog slot number of SU table
C      CATBLK  I(256)   Catalog header record
C      MAXSOU  I        Dimension of SOUWAN
C      SELQUA  I(*)     Source qualifiers to be selected, -1=>any.
C                       Elements correspond to elements of SOURCE.
C      SELCOD  C*4      Calibrator codes to select.
C                       '    '  => any,
C                       '*   ' => any non blank calibrator code.
C                       '-CAL' => blank only (no calibrators)
C                       anything else => matching CALcodes.
C      SOURCE  C(30)*16 Names of up to 30 sources, '*' or blank=>all
C                       First character of name '-' => all except those
C                       specified. '-' are NO LONGER removed.
C   Output:
C      NSOUWD  I        Number of sources included or excluded; if
C                       0 all sources are included.
C      DOSWNT  L        If .TRUE. then sources in SOUWAN are included
C                       If .FALSE. then excluded.
C      SOUWAN  I(*)     The source numbers of sources included or
C                       excluded.
C      IERR    I        Return code, 0=>OK, otherwise source file
C                       exists but cannot be read.
C                       1=TABIO problem, 2=no sources or calibrators
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), MAXSOU, SELQUA(*), NSOUWD,
     *   SOUWAN(MAXSOU), IERR
      CHARACTER SOURCE(*)*16, SELCOD*4
      LOGICAL   DOSWNT
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   JERR, IBUFF(1024), IDSOU, SUKOLS(MAXSUC), I1,
     *   SUNUMV(MAXSUC), QUAL, INOGRP, J, K, NSOU, NSOURC, I, ISURNO,
     *   BADCNT, LUN, IVER, SUFQID
      LOGICAL   T, F, TABLE, EXIST, FITASC, ALLSOU, DOCALC, DOQUAL,
     *   ANYCC, NOCAL, NMONLY
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF),
     *   RAOBS, DECOBS
      REAL      FLUX(4,MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                       See if SU file exists.
      DOSWNT = T
      NSOUWD = 0
      CALL ISTAB ('SU', DISK, CNO, 1, LUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 999
C                                       Open SU table
      IVER = 1
      CALL SOUINI ('READ', IBUFF, DISK, CNO, IVER, CATBLK, LUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   JERR)
      IF (JERR.GT.0) THEN
         WRITE (MSGTXT,1000) JERR
         GO TO 990
         END IF
C                                       Get number of sources.
      NSOURC = IBUFF(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 900
      ALLSOU = F
      DOQUAL = F
      NSOU = 0
C                                       Setup for CALCODE selection.
      ANYCC = SELCOD.EQ.'*   '
      DOCALC = (SELCOD.NE.'    ') .AND.
     *   ((SELCOD.NE.'*   ') .AND. (SELCOD.NE.'-CAL'))
      NOCAL = SELCOD.EQ.'-CAL'
C                                       Check if source excluded
C                                       or if all are included
      DO 30 J = 1,30
         IF (SOURCE(J)(1:1).EQ.'*') ALLSOU = .TRUE.
         IF (SOURCE(J)(1:1).EQ.'-') DOSWNT = .FALSE.
C                                       Find number of sources
         IF (SOURCE(J).NE.' ') NSOU = J
         DOQUAL = DOQUAL .OR. (SELQUA(J).NE.-1)
 30      CONTINUE
      NMONLY = (SELCOD.EQ.' ') .AND. (.NOT.DOQUAL)
C                                       Make sure need to look at table
      ALLSOU = ALLSOU .OR. (NSOU.LE.0)
      IF (ALLSOU .AND. NMONLY) GO TO 900
      IF (NSOU.LE.1) NSOU = 1
      BADCNT = 0
C                                       Loop through records
      DO 100 I = 1,NSOURC
         IERR = 1
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.LT.0) GO TO 100
         IF (JERR.GT.0) THEN
            IERR = JERR
            WRITE (MSGTXT,1020) JERR
            GO TO 990
            END IF
         IERR = 0
C                                       Search lists
         DO 80 J = 1,NSOU
C                                       Check qualifier
            IF (DOQUAL .AND. (QUAL.NE.SELQUA(J)) .AND. (SELQUA(J).GE.0))
     *         GO TO 80
C                                       Check CALCODE
            IF (ANYCC .AND. (CALCOD.EQ.'    ')) GO TO 80
            IF (NOCAL .AND. (CALCOD.NE.'    ')) GO TO 80
            IF (DOCALC .AND. (CALCOD.NE.SELCOD)) GO TO 80
C                                       Source list
            IF (.NOT.ALLSOU) THEN
               I1 = 1
               IF (SOURCE(J)(1:1).EQ.'-') I1 = 2
               IF (SOURCE(J)(I1:).NE.SOUNAM) THEN
                  IF (DOSWNT .OR. NMONLY) GO TO 80
               ELSE
                  IF (.NOT.(DOSWNT .OR. NMONLY)) GO TO 80
                  END IF
               END IF
C                                       Redundancy check
            IF (NSOUWD.GE.1) THEN
               DO 40 K = 1,NSOUWD
                  IF (SOUWAN(K).EQ.IDSOU) GO TO 80
 40               CONTINUE
               END IF
C                                       Add source
            IF (NSOUWD.LT.MAXSOU) THEN
               NSOUWD = NSOUWD + 1
               SOUWAN(NSOUWD) = IDSOU
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
C                                       Save source name (up to 30)
            IF (ALLSOU) THEN
               IF (NSOUWD.LE.29) SOURCE(NSOUWD) = SOUNAM
               IF (NSOUWD.EQ.30) SOURCE(NSOUWD) = 'Could be more'
               END IF
 80         CONTINUE
 100     CONTINUE
      IF (.NOT.NMONLY) DOSWNT = .TRUE.
C                                       Too many sources?
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1100) BADCNT, MAXSOU
         CALL MSGWRT (6)
         END IF
      IF (NSOUWD.LE.0) THEN
C                                       No sources found
         IERR = 2
         WRITE (MSGTXT,1101)
         GO TO 990
         END IF
      GO TO 900
C                                       No SOURCE file
C                                       Close file
 900  IF (JERR.LE.0) CALL TABIO ('CLOS', 0, I, IBUFF, IBUFF, JERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUFND: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('SOUFND: ERROR',I3,' READING SOURCE TABLE')
 1100 FORMAT ('SOUFND: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
 1101 FORMAT ('SOUFND: ALL SOURCES REJECTED BY SELECTION CRITERIA')
      END
