      SUBROUTINE SOUFIL (IERR)
C-----------------------------------------------------------------------
C! Fills in arrays of source numbers to be included or excluded.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2012-2013
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
C   Inputs from common /SELCAL/; also checks antennas to be selected.
C      SOURCS(30)   C*16 Names of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified. (- no longer removed)
C      CALSOU(30)   C*16 Names of up to 30 calibrators,
C                        '*' or blank =>all, first character of name '-'
C                        => all except those specified. (- no longer
C                        removed)
C      SELQUA       I    Source qualifiers to be selected, -1=>any.
C                        Applied to both SOURCS and CALSOU.
C      SELCOD       C*4  Calibrator codes to select.
C                        '    '  => any,
C                        '*   ' => any non blank calibrator code.
C                        '-CAL' => blank only (no calibrators)
C                        anything else => matching CALcodes.
C                        Applied to SOURCS or CALSOU as controlled by
C                        DOAPPL
C      DOAPPL       L    If true then selection of the sources in
C                        CALSOU is conditioned on SELCOD else
C                        selection of SOURCS is conditioned on SELCOD.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C                        (no longer made all positive)
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise source file
C                        exists but cannot be read.
C                        1=TABIO problem, 2=no sources or calibrators
C   Output to common /SELCAL/:
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The source numbers of sources included or
C                        excluded.
C      SOUWTB(30)   I    The SoUrce table row numbers corresponding
C                        to SOUWAN.
C      NCALWD       I    Number of calibrators included or excluded.
C      DOCWNT       L    If .TRUE. then calibrators in CALWAN are
C                        included, if .FALSE. then excluded.
C      CALWAN(30)   I    The source numbers of calibrators included or
C                        excluded.
C      CALWTB(30)   I    The SoUrce table row numbers corresponding
C                        to CALWAN.
C      NANTSL       I    Number of antennas selected/excluded in ANTENS
C                        0 = All included.
C      DOAWNT       L    If .TRUE. then antennas in ANTENS included.
C                        If .FALSE. then excluded.
C   Note: also uses FGBUFF and UBUFF from /SELCAL/
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   DATP(128,2), JERR, IBUFF(512), IDSOU, SUKOLS(MAXSUC), J,
     *   K, SUNUMV(MAXSUC), QUAL, INOGRP, NSOU, NCAL, NSOURC, I, ISURNO,
     *   I1, BADCNT, YSTBSZ, SUFQID
      LOGICAL   T, F, TABLE, EXIST, FITASC, ALLSOU, ALLCAL, DOCALC,
     *   DOQUAL, ANYCC, NOCAL, TAOPEN, NMONLY
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (DATP, UBUFF)
      DATA T, F /.TRUE.,.FALSE./
      DATA YSTBSZ /XSTBSZ/
C-----------------------------------------------------------------------
C                                       Setup for CALCODE and qualifier
C                                       selection.
      IERR = 0
      DOSWNT = T
      DOCWNT = T
      NSOUWD = 0
      NCALWD = 0
      TAOPEN = .FALSE.
C                                       See if SU file exists.
      CALL ISTAB ('SU', IUDISK, IUCNO, 1, IXLUN, IBUFF, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((.NOT.EXIST) .AND. (JERR.EQ.0)) GO TO 900
      IF ((.NOT.EXIST) .OR. (.NOT.TABLE) .OR. (JERR.NE.0)) THEN
         IERR = 1
         IF (.NOT.EXIST) THEN
            WRITE (MSGTXT,1000) JERR
         ELSE IF (TABLE) THEN
            WRITE (MSGTXT,1001) JERR
         ELSE
            MSGTXT = 'SU FILE DAMAGED - NO LONGER A TABLE'
            END IF
         GO TO 980
         END IF
C                                       Open SU table
      CALL SOUINI ('READ', IBUFF, IUDISK, IUCNO, 1, CATUV, IXLUN,
     *   INOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV,
     *   JERR)
      IF (JERR.NE.0) THEN
         IERR = 1
         WRITE (MSGTXT,1000) JERR
         GO TO 980
         END IF
C                                       Get number of sources.
 20   NSOURC = IBUFF(5)
      TAOPEN = .TRUE.
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 900
      ALLSOU = F
      ALLCAL = F
      NSOU = 0
      NCAL = 0
C                                       adverb parameters
      DOQUAL = SELQUA.NE.-1
      ANYCC = SELCOD.EQ.'*   '
      DOCALC = (SELCOD.NE.'    ') .AND.
     *   ((SELCOD.NE.'*   ') .AND. (SELCOD.NE.'-CAL'))
      NOCAL = SELCOD.EQ.'-CAL'
      NMONLY = .NOT.DOQUAL
      IF (.NOT.DOAPPL) NMONLY = (.NOT.DOQUAL) .AND. (SELCOD.EQ.' ')
C                                       Check if source/calib excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         ALLSOU = ALLSOU .OR. (SOURCS(J)(1:1).EQ.'*')
C                                       Find number of sources
         IF (SOURCS(J).NE.' ') NSOU = J
         IF (SOURCS(J)(1:1).EQ.'-') DOSWNT = F
C                                       Calibrator
         ALLCAL = ALLCAL .OR. (CALSOU(J)(1:1).EQ.'*')
C                                       Find number of calibrators
         IF (CALSOU(J).NE.' ') NCAL = J
         IF (CALSOU(J)(1:1).EQ.'-') DOCWNT = F
 30      CONTINUE
C                                       Make sure need to look at table
      ALLSOU = ALLSOU .OR. (NSOU.LE.0)
      ALLCAL = ALLCAL .OR. (NCAL.LE.0)
      IF ((ALLSOU.AND.ALLCAL) .AND.
     *   (.NOT.(DOCALC.OR.ANYCC.OR.NOCAL.OR.DOQUAL))) GO TO 900
      IF (NSOU.LE.1) NSOU = 1
      IF (NCAL.LE.1) NCAL = 1
      BADCNT = 0
C                                       Sources
       IF (ALLSOU .AND.
     *   ((.NOT.((.NOT.DOAPPL).AND.(DOCALC.OR.ANYCC.OR.NOCAL)))
     *   .AND. (.NOT.DOQUAL))) GO TO 110
C                                       Loop through records
      DO 100 I = 1,NSOURC
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1001) JERR
            GO TO 980
            END IF
C                                       Search lists
         DO 80 J = 1,NSOU
C                                       Check qualifier
            IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 80
C                                       Check CALCODE
            IF (.NOT.DOAPPL) THEN
               IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 80
               IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 80
               IF ((DOCALC) .AND. (CALCOD.NE.SELCOD)) GO TO 80
               END IF
C                                       Source list
            IF (.NOT.ALLSOU) THEN
               I1 = 1
               IF (SOURCS(J)(1:1).EQ.'-') I1 = 2
               IF (SOURCS(J)(I1:).NE.SOUNAM) THEN
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
            IF (NSOUWD.LT.YSTBSZ) THEN
               NSOUWD = NSOUWD + 1
               SOUWAN(NSOUWD) = IDSOU
               IF (NSOUWD.LE.30) SOUWTN(NSOUWD) = ISURNO
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 80         CONTINUE
 100     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1100) BADCNT, YSTBSZ
         CALL MSGWRT (6)
         END IF
      IF (.NOT.NMONLY) DOSWNT = .TRUE.
C                                       No sources found
      IF (NSOUWD.LE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1101)
         GO TO 980
         END IF
C                                       Calibrator sources
 110  BADCNT = 0
      IF (ALLCAL .AND. ((.NOT.(DOAPPL.AND.(DOCALC.OR.ANYCC.OR.NOCAL)))
     *   .AND. (.NOT.DOQUAL))) GO TO 900
      NMONLY = .NOT.DOQUAL
      IF (DOAPPL) NMONLY = (.NOT.DOQUAL) .AND. (SELCOD.EQ.' ')
C                                       Loop through records
      ISURNO = 1
      DO 200 I = 1,NSOURC
C                                       Read record
         CALL TABSOU ('READ', IBUFF, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1001) JERR
            GO TO 980
            END IF
C                                       Calibrator list
C                                       Search lists
         DO 180 J = 1,NCAL
C                                       Check qualifier
            IF ((DOQUAL) .AND. (QUAL.NE.SELQUA)) GO TO 180
C                                       Check CALCODE
            IF (DOAPPL) THEN
               IF ((ANYCC)  .AND. (CALCOD.EQ.' ')) GO TO 180
               IF ((NOCAL)  .AND. (CALCOD.NE.' ')) GO TO 180
               IF ((DOCALC) .AND. (CALCOD.NE.SELCOD)) GO TO 180
               END IF
            I1 = 1
            IF (CALSOU(J)(1:1).EQ.'-') I1 = 2
            IF (.NOT.ALLCAL) THEN
               IF (CALSOU(J)(I1:).NE.SOUNAM) THEN
                  IF (DOCWNT .OR. NMONLY) GO TO 180
               ELSE
                  IF (.NOT.(DOCWNT .OR. NMONLY)) GO TO 180
                  END IF
               END IF
C                                       Redundancy check
            IF (NCALWD.GE.1) THEN
               DO 160 K = 1,NCALWD
                  IF (CALWAN(K).EQ.IDSOU) GO TO 180
 160              CONTINUE
               END IF
C                                       Add calibrator
            IF (NCALWD.LT.YSTBSZ) THEN
               NCALWD = NCALWD + 1
               CALWAN(NCALWD) = IDSOU
               IF (NCALWD.LE.30) CALWTN(NCALWD) = ISURNO
C                                       Overflowed array
            ELSE
               BADCNT = BADCNT + 1
               END IF
 180        CONTINUE
 200     CONTINUE
      IF (BADCNT.GT.0) THEN
         WRITE (MSGTXT,1200) BADCNT, YSTBSZ
         CALL MSGWRT (6)
         END IF
C                                       No calibrators found
      IF (NCALWD.LE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1201)
         GO TO 980
         END IF
      IF (.NOT.NMONLY) DOCWNT = .TRUE.
      GO TO 900
C                                       No SOURCE file
C                                       Close file
 900  IF (TAOPEN) CALL TABIO ('CLOS', 0, I, UBUFF, IBUFF, JERR)
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 930 J = 1,MAXANT
         IF (ANTENS(J).LT.0) DOAWNT = F
         IF (NANTSL.GE.1) THEN
            DO 910 K = 1,NANTSL
               IF (ABS(ANTENS(J)).EQ.ABS(ANTENS(K))) ANTENS(J) = 0
 910           CONTINUE
            END IF
C                                       Check for multiple entries
         IF (ABS(ANTENS(J)).GE.1) NANTSL = J
 930     CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
      GO TO 999
C                                       Error
 980  CALL MSGWRT (8)
      IF (TAOPEN) CALL TABIO ('CLOS', 0, I, UBUFF, IBUFF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUFIL OR ISTAB: ERROR',I3,' OPENING SOURCE TABLE')
 1001 FORMAT ('SOUFIL/ISTAB: ERROR',I4,' READING SOURCE TABLE')
 1100 FORMAT ('SOUFIL: ',I5,' MORE SOURCES SELECTED THAN ',I6,
     *   ' ALLOWED')
 1101 FORMAT ('SOUFIL: ALL SOURCES REJECTED BY SELECTION CRITERIA')
 1200 FORMAT ('SOUFIL: ',I5,' MORE CALIBRATORS SELECTED THAN ',I6,
     *   ' ALLOWED')
 1201 FORMAT ('SOUFIL: ALL CALIBRATORS REJECTED BY SELECTION CRITERIA')
      END
