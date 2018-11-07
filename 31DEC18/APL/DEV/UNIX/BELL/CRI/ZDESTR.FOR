      SUBROUTINE ZDESTR (IVOL, PNAME, IERR)
C-----------------------------------------------------------------------
C! destroy a closed disk file
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Destroy (i.e., delete) a file.  The file should already be closed.
C   Inputs:
C      IVOL    I      Disk volume containing file, 1,2,3,...
C      PNAME   C*48   Physical file name (left justified)
C   Output:
C      IERR    I      Error return code: 0 => no error
C                        1 => file not found (no message)
C                        2 => device not found
C                        3 => file in use
C                        4 => other
C   UNICOS version.
C-----------------------------------------------------------------------
      INTEGER   IVOL, IERR
      CHARACTER PNAME*48
C
      INTEGER   SYSERR, I
      CHARACTER CTEMP*2
      LOGICAL   IAMOK
      HOLLERITH ANAME(12)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check privilege
      I = INDEX (PNAME, ':')
      IF (I.GT.0) THEN
         CTEMP = PNAME(I+1:)
         IF (.NOT.IAMOK(IVOL,CTEMP)) THEN
            WRITE (MSGTXT,1000) CTEMP, IVOL, NLUSER
            IERR = 2
            GO TO 990
            END IF
         END IF
C                                       Destroy the file.
C      CALL CHR2H (48, PNAME, 1, ANAME)
      CALL ZDEST2 (PNAME, SYSERR, IERR)
      IF (IERR.LT.2) GO TO 999
C                                       Logical device not found.
         IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1002) PNAME(1:4), IERR
C                                       File in use.
         ELSE IF (IERR.EQ.3) THEN
            WRITE (MSGTXT,1003) PNAME(1:24), IERR
C                                       Other destroy error.
         ELSE
            WRITE (MSGTXT,1004) PNAME(1:24), IERR
            CALL MSGWRT (6)
            CALL ZERROR ('ZDEST2', SYSERR, PNAME, -999, .FALSE.)
            GO TO 999
            END IF
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDESTR: ',A2,' FILES ON DISK',I3,' NOT ALLOWED FOR USER',
     *   I5)
 1002 FORMAT ('ZDESTR: NO SUCH LOGICAL DEVICE = ',A4,'. IERR=',I2)
 1003 FORMAT ('ZDESTR: FILE ',A24,' IN USE. IERR=',I2)
 1004 FORMAT ('ZDESTR: ERROR DESTROYING FILE ',A24,' IERR=',I2)
      END
