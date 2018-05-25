      SUBROUTINE ZCREAT (IVOL, PNAME, RSIZE, MAP, ASIZE, IERR)
C-----------------------------------------------------------------------
C! creates a disk file
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
C   Create a disk file of a specified name and size reserving the
C   disk space.
C   Inputs:
C      IVOL     I        Disk volume containing file
C      PNAME    C*(*)    Physical file name
C      RSIZE    I        Requested size of the file in AIPS-blocks (256
C                        local integers)
C      MAP      L        Is this a "map" file?
C   Output:
C      ASIZE    I        Actual size of file in AIPS-blocks
C      IERR     I        Error return code: 0 => no error
C                           1 => file already exists
C                           2 => volume not found
C                           3 => insufficient space
C                           4 => other
C                           5 => forbidden (reserved)
C   UNICOS version.
C   New version - supports DASSGN(n,IVOL) giving -1 for scratch disk
C   (no UV or MA files) or > 0 restricting the user numbers.  If these
C   tests block the creation returns new error code and message is
C   issued.  All this done through IAMOK function.
C-----------------------------------------------------------------------
      CHARACTER PNAME*(*)
      INTEGER   IVOL, RSIZE, ASIZE, IERR
      LOGICAL   MAP
C
      CHARACTER TYPE*2
      HOLLERITH ANAME(12)
      INTEGER   ICOLON, SYSERR, ABLKS, I, IL, ITRIM
      LOGICAL   IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      IERR = 4
      IL = ITRIM (PNAME)
      IF (IL.GT.40) IL = 40
C                                       Valid file size?
      IF (RSIZE.LE.0) THEN
         WRITE (MSGTXT,1000) RSIZE
         GO TO 995
         END IF
C                                       Volume number in range?
 10   IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) THEN
         WRITE (MSGTXT,1010) IVOL, PNAME(:IL)
         GO TO 995
         END IF
C                                       Is file type allowed on scratch
C                                       volumes?
 20   ICOLON = INDEX (PNAME, ':') + 1
      TYPE = PNAME(ICOLON:ICOLON+1)
      IF (.NOT.IAMOK(IVOL, TYPE)) THEN
         WRITE (MSGTXT,1020) TYPE, IVOL, NLUSER
         IERR = 5
         GO TO 995
         END IF
C
 30   ABLKS = RSIZE
C                                       Attempt to create the file of
C                                       size ABLKS.
C 40   CALL CHR2H (48, PNAME, 1, ANAME)
 40    CALL ZCREA2 (PNAME, ABLKS, ASIZE, SYSERR, IERR)
C                                       File already exists?
      IF (IERR.EQ.1) THEN
         WRITE (MSGTXT,1040) PNAME(1:IL)
C                                       Can't find specified volume?
 50   ELSE IF (IERR.EQ.2) THEN
         WRITE (MSGTXT,1050) IVOL, PNAME(1:IL)
C                                       Insufficient space?
 60   ELSE IF (IERR.EQ.3) THEN
         WRITE (MSGTXT,1060) IVOL, PNAME(:IL)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1061) ABLKS
C                                       No or other error.
 70   ELSE
         IF (IERR.NE.0) CALL ZERROR ('ZCREA2', SYSERR, PNAME, -999, MAP)
         IF (IERR.EQ.5) IERR = 4
         GO TO 999
         END IF
C
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZCREAT: INVALID BYTE REQUEST = ',I11)
 1010 FORMAT ('ZCREAT: DISK',I3,' OUT OF RANGE FOR FILE ',A)
 1020 FORMAT ('ZCREAT: ',A2,' FILES ON DISK',I3,' NOT ALLOWED FOR USER',
     *   I5)
 1040 FORMAT ('ZCREAT: FILE ',A,' ALREADY EXISTS')
 1050 FORMAT ('ZCREAT: DISK',I3,' NOT FOUND FOR ',A)
 1060 FORMAT ('INSUFFICIENT SPACE ON DISK',I3,' FOR FILE ',A)
 1061 FORMAT ('REQUESTED SIZE OF ',I11,' BLOCKS FROM ZCREAT')
      END
