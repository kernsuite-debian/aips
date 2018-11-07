      SUBROUTINE ZRENAM (IVOL, OLDNAM, NEWNAM, IERR)
C-----------------------------------------------------------------------
C! rename a disk file
C# Z IO-util
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
C   Rename a disk file.
C   Inputs:
C      IVOL     I      Disk volume containing files, 1,2,3,...
C      OLDNAM   C*48   Old physical file name
C      NEWNAM   C*48   New physical file name
C   Output:
C      IERR     I      Error return code: 0 => ok
C                         2 => old file not found
C                         3 => volume not found
C                         4 => old file busy
C                         6 => new file name already exists
C                         7 => other
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   IVOL, IERR
      CHARACTER OLDNAM*48, NEWNAM*48
C
      HOLLERITH OLD(12), NEW(12)
      INTEGER   SYSERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL CHR2H (48, OLDNAM, 1, OLD)
      CALL CHR2H (48, NEWNAM, 1, NEW)
C                                       Rename the file.
      CALL ZRENA2 (OLD, NEW, SYSERR, IERR)
      IF (IERR.NE.0) THEN
C                                       File not found.
         IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1002) OLDNAM(1:24), IERR
C                                       Invalid device name(s).
         ELSE IF (IERR.EQ.3) THEN
            WRITE (MSGTXT,1003) OLDNAM(1:4), NEWNAM(1:4), IERR
C                                       File in use.
         ELSE IF (IERR.EQ.4) THEN
            WRITE (MSGTXT,1004) OLDNAM(1:24), IERR
C                                       New file name already exists.
         ELSE IF (IERR.EQ.6) THEN
            WRITE (MSGTXT,1006) NEWNAM(1:24), IERR
C                                       Other ZRENA2 error.
         ELSE
            WRITE (MSGTXT,1007) OLDNAM(1:24), NEWNAM(1:24), IERR
            CALL MSGWRT (6)
            CALL ZERROR ('ZRENA2', SYSERR, OLDNAM, -999, .FALSE.)
            GO TO 999
            END IF
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1002 FORMAT ('ZRENAM: FILE ',A24,' NOT FOUND. IERR=',I4)
 1003 FORMAT ('ZRENAM: NO SUCH LOGICAL DEVICE = ',A4,' OR ',A4,
     *   '. IERR=',I4)
 1004 FORMAT ('ZRENAM: FILE ',A24,' IN USE. IERR=',I4)
 1006 FORMAT ('ZRENAM: ',A24,' ALREADY EXISTS. IERR=',I4)
 1007 FORMAT ('ZRENAM: ',A24,1X,A24,' IERR=',I4)
      END
