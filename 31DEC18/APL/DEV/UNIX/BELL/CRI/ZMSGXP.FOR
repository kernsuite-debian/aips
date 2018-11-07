      SUBROUTINE ZMSGXP (LUN, IVOL, PNAME, NREC, IERR)
C-----------------------------------------------------------------------
C! expand the message file
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   Increase the size of a message file (special version of ZEXPND that
C   writes error messages to the terminal only in order to avoid
C   recursion).
C   Inputs:
C      LUN     I      LUN of file open message file (must be 12)
C      IVOL    I      Disk volume containing file, 1,2,3,...
C      PNAME   C*48   Physical file name
C   In/Out:
C      NREC    I      # 256-integer records requested/received
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => input error
C                        2 => expansion error
C                        3 => ZEXIS2 error
C   UNICOS version - calls ZEXPN2, ZEXIS2.
C-----------------------------------------------------------------------
      INTEGER   LUN, IVOL, NREC, IERR
      CHARACTER PNAME*48
C
      INTEGER   FCBOFF, FIND, ERRLUN, ISIZE, LSIZE, JERR, SYSERR,
     *   LREC, KREC
      LOGICAL   MAP
      HOLLERITH ANAME(12)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA ERRLUN, MAP /12, .FALSE./
C-----------------------------------------------------------------------
C                                       Valid number of records.
      IERR = 1
      IF (NREC.GT.0) GO TO 10
         WRITE (6,1000) NREC
         GO TO 990
C                                       Valid volume?
 10   IF ((IVOL.GT.0) .OR. (IVOL.LE.NVOL)) GO TO 20
         WRITE (6,1010) IVOL
         GO TO 990
C                                       LUN must be ERRLUN.
 20   IF (LUN.EQ.ERRLUN) GO TO 30
         WRITE (6,1020) LUN, ERRLUN
         GO TO 990
C                                       Locate message file in FTAB.
 30   CALL LSERCH ('SRCH', LUN, FIND, MAP, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (6,1030) PNAME(1:24), LUN
         GO TO 990
C                                       Expand file.
 40   FCBOFF = FIND + NMOFF
C      CALL CHR2H (48, PNAME, 1, ANAME)
C                                       current size
C      CALL ZEXIS2 (ANAME, ISIZE, SYSERR, JERR)
      CALL ZEXIS2 (PNAME, ISIZE, SYSERR, JERR)
      IF (JERR.NE.0) THEN
         IERR = 3
         GO TO 999
         END IF
      LREC = NREC
      CALL ZEXPN2 (FTAB(FCBOFF), 0, NREC, IERR)
      IF (IERR.EQ.0) THEN
C         CALL ZEXIS2 (ANAME, LSIZE, SYSERR, JERR)
         CALL ZEXIS2 (PNAME, LSIZE, SYSERR, JERR)
         IF (JERR.NE.0) THEN
            IERR = 3
            GO TO 999
            END IF
         KREC = NREC
         NREC = LSIZE - ISIZE
         IF ((NREC.LT.KREC) .OR. (NREC.LT.LREC)) THEN
            WRITE (6,1040) LREC, KREC, NREC
            END IF
         GO TO 999
      ELSE
         IERR = 2
         WRITE (6,1045) PNAME(1:24), NREC
         END IF
C
 990  NREC = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZMSGXP: INVALID RECORD REQUEST = ',I6)
 1010 FORMAT (' ZMSGXP: INVALID VOLUME REQUEST = ',I6)
 1020 FORMAT (' ZMSGXP: INVALID LUN = ',I2,'. MUST BE ',I2)
 1030 FORMAT (' ZMSGXP: FILE ',A24,' NOT OPEN IN FTAB.  LUN = ',I2)
 1040 FORMAT (' ZMSGXP: REQUEST, CLAIMED, ACTUAL',3I7,' BLOCKS')
 1045 FORMAT (' ZMSGXP: EXPANSION FAILED ON FILE ',A24,' NREC = ',I6)
      END
