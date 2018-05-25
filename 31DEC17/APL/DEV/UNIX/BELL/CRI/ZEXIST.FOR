      SUBROUTINE ZEXIST (IVOL, PNAME, ISIZE, IERR)
C-----------------------------------------------------------------------
C! return file size and, consequently, whether file exists
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Determine if a file exists and if so, return its size in AIPS-bytes
C   (1/2 of a local integer).
C   Inputs:
C      IVOL     I      Disk volume containing file, 1,2,3,...
C      PNAME    C*48   Physical file name
C   Output:
C      ISIZE    I      File size in AIPS-blocks (256 local integers)
C      IERR     I      Error return code: 0 => file exists
C                         1 => file does not exist
C                         2 => inputs error
C                         3 => other error
C   UNICOS version - SCRTCH not used, calls ZEXIS2.
C-----------------------------------------------------------------------
      INTEGER   IVOL, ISIZE, IERR
      CHARACTER PNAME*48
C
      INTEGER   SYSERR, I
      CHARACTER CTEMP*2
      LOGICAL   IAMOK
      HOLLERITH ANAME(12)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      ISIZE = 0
      IERR = 0
C                                       Volume (disk) number in range?
      IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) THEN
         IERR = 2
         WRITE (MSGTXT,1000) IVOL
         GO TO 995
         END IF
C                                       privilege
      I = INDEX (PNAME, ':')
      IF (I.GT.0) THEN
         CTEMP = PNAME(I+1:)
         IF (.NOT.IAMOK(IVOL,CTEMP)) THEN
            WRITE (MSGTXT,1005) CTEMP, IVOL, NLUSER
            IERR = 2
            GO TO 995
            END IF
         END IF
C                                       Check for existence of file;
C                                       return its size in AIPS-bytes.
C 10   CALL CHR2H (48, PNAME, 1, ANAME)
 10   CALL ZEXIS2 (PNAME, ISIZE, SYSERR, IERR)
C                                       Error?
      IF (IERR.GE.3) THEN
         CALL ZERROR ('ZEXIS2', SYSERR, PNAME, -999, .FALSE.)
      ELSE IF (IERR.EQ.2) THEN
         WRITE (MSGTXT,1010) PNAME(1:34)
         END IF
C
 995  IF (IERR.EQ.2) CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZEXIST: VOLUME NUMBER = ',I6,' OUT OF RANGE')
 1005 FORMAT ('ZEXIST: ',A2,' FILES ON DISK',I3,' NOT ALLOWED FOR USER',
     *   I5)
 1010 FORMAT ('ZEXIST: CAN''T TRANSLATE NAME = ',A)
      END
