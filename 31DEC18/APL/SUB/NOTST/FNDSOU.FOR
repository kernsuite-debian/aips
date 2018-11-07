      SUBROUTINE FNDSOU (DISK, CNO, SOURCS, BUFFER, NSOUWD, DOSWNT,
     *   SOUWAN, IERR)
C-----------------------------------------------------------------------
C! Find source numbers for a list of sources.
C# EXT-appl Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C   Inputs:
C      DISK         I    Input disk number.
C      CNO          I    Catalog slot number
C      SOURCS(30)   C*16 Names of up to 30 sources, *=>all
C                        First character of name '-' => all except those
C                        specified.
C   Input from common /MAPHDR/
C      CATBLK(256)  I    Catalog header record.
C   Input/Output:
C      BUFFER(512)  I    I/O Buffer for source (SU) table
C   Output:
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN(30)   I    The source numbers of sources included or
C                        excluded.
C      IERR         I    Return code, 0=>OK, otherwise failed.
C   Note: uses AIPS LUN 29.
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, BUFFER(768), NSOUWD, SOUWAN(30), IERR
      LOGICAL   DOSWNT
      CHARACTER SOURCS(30)*16
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELDEF*8, VELTYP*8, CALCOD*4, SOUNAM*16
      INTEGER   CATBLK(256), JERR, IDSOU, SUKOLS(MAXSUC), IXLUN,
     *   SUNUMV(MAXSUC), QUAL, NUMIF, J, K, NSOURC, I, ISURNO, SUFQID,
     *   I1
      LOGICAL   T, F, TABLE, EXIST, FITASC, ALLSOU, ALLBLK
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   RAOBS, DECOBS, PMRA, PMDEC
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION    LSRVEL(MAXIF), FREQO(MAXIF), LRESTF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      DATA T, F /.TRUE.,.FALSE./
      DATA IXLUN /29/
C-----------------------------------------------------------------------
C                                       See if SU file exists.
      CALL ISTAB ('SU', DISK, CNO, 1, IXLUN, BUFFER, TABLE, EXIST,
     *   FITASC, JERR)
      IF ((JERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) GO TO 800
C                                       Open SU table
      CALL SOUINI ('READ', BUFFER, DISK, CNO, 1, CATBLK, IXLUN,
     *   NUMIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, JERR)
      IF (JERR.LE.0) GO TO 20
         WRITE (MSGTXT,1000) JERR
         GO TO 990
C                                       Get number of sources.
 20   NSOURC = BUFFER(5)
C                                       Check if empty
      IF (NSOURC.LE.0) GO TO 800
      DOSWNT = T
      NSOUWD = 0
      ALLSOU = F
      ALLBLK = T
C                                       Check if source excluded
C                                       or if all are included
      DO 30 J = 1,30
C                                       Sources
         ALLSOU = ALLSOU .OR. (SOURCS(J)(1:1).EQ.'*')
C                                       Check for blanks
         ALLBLK = ALLBLK .AND. (SOURCS(J).EQ.' ')
C                                       Check for "-"
C                                       Minus sign NOT removed
         IF (SOURCS(J)(1:1).EQ.'-') DOSWNT = F
 30      CONTINUE
C                                       See if all blanks or "*"
      IF ((ALLBLK) .OR. (ALLSOU)) GO TO 900
C                                       Loop through records
      DO 100 I = 1,NSOURC
         IERR = 1
C                                       Read record
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *      IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *      DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *      PMRA, PMDEC, JERR)
C                                       Check error
         IF (JERR.LE.0) GO TO 35
            WRITE (MSGTXT,1020) JERR
            GO TO 990
 35      IERR = 0
C                                       Search list
         DO 80 J = 1,30
            I1 = 1
            IF (SOURCS(J)(1:1).EQ.'-') I1 = 2
C                                       Got one - redundancy check
            IF (SOURCS(J)(I1:).EQ.SOUNAM) THEN
               DO 40 K = 1,NSOUWD
                  IF (SOUWAN(K).EQ.IDSOU) GO TO 100
 40               CONTINUE
C                                       Add source
               NSOUWD = NSOUWD + 1
               SOUWAN(NSOUWD) = IDSOU
               GO TO 100
               END IF
 80         CONTINUE
C                                       No match
 100     CONTINUE
C                                       Should have found matches:
      IF (NSOUWD.GT.0) GO TO 900
      IERR = 9
      WRITE (MSGTXT,1100)
      GO TO 990
C                                       No SOURCE file
 800  IERR = 10
      WRITE (MSGTXT,1800)
      GO TO 990
C                                       Close file
 900  IF (JERR.LE.0) CALL TABIO ('CLOS', 0, I, BUFFER, BUFFER, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FNDSOU: ERROR',I3,' OPENING SOURCE TABLE')
 1020 FORMAT ('FNDSOU: ERROR',I3,' READING SOURCE TABLE')
 1100 FORMAT ('FNDSOU: NO SOURCES SPECIFIED FOUND IN SU TABLE')
 1800 FORMAT ('FNDSOU: NO SU TABLE WAS FOUND.')
      END
