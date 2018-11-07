      SUBROUTINE SOURNU (SOURCE, QUAL, NSOUR, DISK, CNO, NID, BUFFER,
     *   ID, IRET)
C-----------------------------------------------------------------------
C! Look up source numbers for a list of names.
C# EXT-appl UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2004, 2012
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
C   Determines the source numbers of a list of source names from the
C   source table associated with a specified catalog entry.
C   Inputs:
C      SOURCE  C*16(*)   List of source names.
C                        If the first character of any source names
C                        begins with a "-", all sources EXCEPT those
C                        named will be returned ( the "-" will be
C                        ignored in determining the source name).
C                        Blank source names are ignored.  Names should
C                        be left justified, blank filled
C      QUAL      I(*)    SOURCE qualifiers, .lt. 0 => any.
C      NSOUR     I       Number of entries in SOURCE, may include
C                        blank names.
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C   Input/Output:
C      NID       I       On input the maximum number of elements to be
C                        filled in ID; on output, the number of elements
C                        in ID. 0 => all selected.
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Source ID numbers of selected sources,
C                        If ID(1)=0 then all sources are selected.
C      IRET      I       Return code. 0 => OK; else failed.
C                           -1 => source/qual specified and not found
C                                 no message is generated
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C   return.
C       Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      CHARACTER SOURCE(*)*16
      INTEGER   QUAL(*), NSOUR, DISK, CNO, NID, BUFFER(*), ID(*), IRET,Q
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   CAT(256), VER, LUN, IDKOL, SUKOL, I, IDSOU, SQUAL, J,
     *   MAXID, NUMIF, ISURNO, NUMREC, I4, SUFQID
      LOGICAL   EQUAL, DESEL, ALLSEL, GOTIT, ALLBLN
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
C-----------------------------------------------------------------------
C                                       Setup
      MAXID = NID
      NID = 0
      IRET = 0
C                                       Check if sources deselected or
C                                       if all sources are selected.
      DESEL = .FALSE.
      ALLSEL = .TRUE.
      ALLBLN = .TRUE.
      DO 10 I = 1,NSOUR
C                                       Check deselection
         DESEL = DESEL .OR. SOURCE(I)(1:1).EQ.'-'
C                                       Check if all blank (GvM, 1/93)
         ALLBLN = ALLBLN .AND. (SOURCE(I).EQ.' ')
C                                       Check if all blank, no qual
         ALLSEL = ALLSEL .AND. (SOURCE(I).EQ.' ') .AND. (QUAL(I).LT.0)
 10      CONTINUE
C                                       Check all selected case.
      ID(1) = 0
      IF (ALLSEL) GO TO 999
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', BUFFER, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Initialize SOURCE table.
 100  CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Get number of entries
120   NUMREC = BUFFER(5)
C                                       Loop through source records.
      DO 500 ISURNO = 1,NUMREC
C                                       Read record
         I4 = ISURNO
         CALL TABSOU ('READ', BUFFER, I4, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, SQUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
C                                       See is source record turned off
         IF (IRET.LT.0) GO TO 500
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Check if in list SOURCE.
 200     GOTIT = .FALSE.
         DO 300 J = 1,NSOUR
C                                       Sources selected.
            IF (.NOT.DESEL) THEN
               EQUAL = SOURCE(J).EQ.SOUNAM
C                                       Check QUAL
               Q = QUAL(J)
C                                       is qualifier OK, too?
               EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *            (QUAL(J).EQ.SQUAL))
C                                       cover blank source and
C                                       given qualifier
               EQUAL = EQUAL.OR.(QUAL(J).EQ.SQUAL.AND.ALLBLN)
               IF (EQUAL) THEN
                  IF ((NID+1).GT.MAXID) GO TO 310
                  NID = NID + 1
                  ID(NID) = IDSOU
                  GO TO 320
                  END IF
C                                       Deselected
            ELSE
C                                       Check for leading "-"
               IF (SOURCE(J)(1:1).EQ.'-') THEN
                  EQUAL = SOURCE(J)(2:16).EQ.SOUNAM(1:15)
               ELSE
                  EQUAL = SOURCE(J).EQ.SOUNAM
                  END IF
C                                       Check QUAL
               EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *            (QUAL(J).EQ.SQUAL))
               GOTIT = GOTIT .OR. EQUAL
               END IF
 300        CONTINUE
C                                       Source not deselected
         IF (DESEL .AND. (.NOT.GOTIT)) THEN
            IF ((NID+1).GT.MAXID) GO TO 310
            NID = NID + 1
            ID(NID) = IDSOU
            END IF
         GO TO 320
C                                       Too many sources selected
 310     CONTINUE
            WRITE (MSGTXT,1300) MAXID
            IRET = 5
            GO TO 990
 320     CONTINUE
 500     CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)
      IF (NID.EQ.0) IRET = -1
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SOURNU: ERROR ',I3,' READING CATBLK FOR SOURCE TABLE')
 1100 FORMAT ('SOURNU: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('SOURNU: ERROR ',I3,' READING SOURCE TABLE')
 1300 FORMAT ('SOURNU: MORE SOURCES SELECTED THAN MAX (',I5,')')
      END
