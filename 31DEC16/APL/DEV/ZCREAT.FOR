      SUBROUTINE ZCREAT (IVOL, PNAME, RSIZE, MAP, ASIZE, IERR)
C-----------------------------------------------------------------------
C! creates a disk file
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004, 2009, 2011
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
C      PNAME    C*48     Physical file name
C      RSIZE    I        Requested size of the file in AIPS-blocks
C                        (256 local Fortran integers)
C      MAP      L        Is this a "map" file?
C   Output:
C      ASIZE    I        Actual size of file in AIPS-blocks
C      IERR     I        Error return code: 0 => no error
C                           1 => file already exists
C                           2 => volume not found
C                           3 => insufficient space
C                           4 => other
C                           5 => forbidden (reserved)
C   Generic version
C   New version - supports DASSGN(n,IVOL) giving -1 for scratch disk
C   (no UV or MA files) or > 0 restricting the user numbers.  If these
C   tests block the creation returns new error code and message is
C   issued.  All this done through IAMOK function.
C   Includes quick creat parameter: 8-Nov-2011
C-----------------------------------------------------------------------
      CHARACTER PNAME*48
      INTEGER   IVOL, RSIZE, ASIZE, IERR
      LOGICAL   MAP
C
      CHARACTER TYPE*2, BLANK*4
      HOLLERITH ANAME(12), HVOLN(420)
      INTEGER   ICOLON, SYSERR, IL, ITRIM, VLEN, TOTBLK(35), FRBLKS(35),
     *   LCREAT
      LOGICAL   IAMOK
      REAL      PCTFUL(35)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCRE.INC'
      INCLUDE 'INCS:DDRC.INC'
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
      IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) THEN
         WRITE (MSGTXT,1010) IVOL, PNAME(:IL)
         GO TO 995
         END IF
C                                       Is file type allowed on scratch
C                                       volumes?
      ICOLON = INDEX (PNAME, ':') + 1
      TYPE = PNAME(ICOLON:ICOLON+1)
      IF ((.NOT.IAMOK(IVOL, TYPE)) .OR. (DSKLOK(IVOL).EQ.1)) THEN
         WRITE (MSGTXT,1020) TYPE, IVOL, NLUSER
         IERR = 5
         GO TO 995
         END IF
C                                       what disk space available
      BLANK = ' '
      CALL CHR2H (4, BLANK, 1, HVOLN(1))
      CALL RFILL (419, HVOLN(1), HVOLN(2))
      VLEN = 48
      CALL ZFRE2 (NVOL, HVOLN, VLEN, TOTBLK, PCTFUL, FRBLKS)
C                                       Did ZFRE2 work?
      IF ((TOTBLK(IVOL).GT.FRBLKS(IVOL)) .AND. (FRBLKS(IVOL).GT.0) .AND.
     *   ((RSIZE+1023)/1024.GE.FRBLKS(IVOL))) THEN
         WRITE (MSGTXT,1060) IVOL, PNAME(:IL)
         CALL MSGWRT (6)
         VLEN = (RSIZE - 1) / 1024 + 1
         WRITE (MSGTXT,1030) VLEN, FRBLKS(IVOL)
         IERR = 3
         GO TO 995
         END IF
C                                       Attempt to create the file of
C                                       size ABYTES.
      CALL CHR2H (48, PNAME, 1, ANAME)
      LCREAT = 0
      IF ((RSIZE.GT.1024) .AND. (QCREAT.GT.0)) LCREAT = 1
      CALL ZCREA2 (ANAME, RSIZE, LCREAT, ASIZE, SYSERR, IERR)
C                                       File already exists?
      IF (IERR.EQ.1) THEN
         WRITE (MSGTXT,1040) PNAME(1:IL)
C                                       Can't find specified volume?
      ELSE IF (IERR.EQ.2) THEN
         WRITE (MSGTXT,1050) IVOL, PNAME(1:IL)
C                                       Insufficient space?
      ELSE IF (IERR.EQ.3) THEN
         WRITE (MSGTXT,1060) IVOL, PNAME(:IL)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1061) RSIZE
C                                       No or other error.
      ELSE
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
 1030 FORMAT ('REQUESTED SIZE',I10,' AVAILABLE',I10,' MBYTES')
 1040 FORMAT ('ZCREAT: FILE ',A,' ALREADY EXISTS')
 1050 FORMAT ('ZCREAT: DISK',I3,' NOT FOUND FOR ',A)
 1060 FORMAT ('INSUFFICIENT SPACE ON DISK',I3,' FOR FILE ',A)
 1061 FORMAT ('REQUESTED SIZE OF ',I11,' BLOCKS FROM ZCREAT')
      END
