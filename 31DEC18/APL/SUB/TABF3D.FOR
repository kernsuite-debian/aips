      SUBROUTINE TABF3D (FORMAT, COUNT, TYPE, IERR)
C-----------------------------------------------------------------------
C! Determines repeat count and data type for FITS 3-D tables entries.
C# FITS Tape EXT-util Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006-2007
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
C   Determines the repeat count and data type from a format used by the
C   FITS 3-D tables.  The recognized data types are:
C       Format   Type code         Type
C         D        1         Double precision real
C         E        2         Single precision real
C         A        3         Character
C         J        4         Long integer (32 bits)
C         L        5         Logical
C         I        6         Short integer (16 bits)
C         X        7         bit array
C   Input:
C      FORMAT   C*8   Format, eg. '4E'
C   Output:
C      COUNT    I     Element count - default is 1
C      TYPE     I     Data type
C      IERR     I     Error code, 0 => OK,   else failed.
C-----------------------------------------------------------------------
      CHARACTER FORMAT*(*)
      INTEGER   COUNT, TYPE, IERR
C
      CHARACTER DTYCOD*14, NUMS*10, FTYPE*1
      INTEGER   TYCOD(14), NTYP, ITYP, ISUM, LOOP, INUM, INDEX, IPOINT
      INCLUDE 'INCS:DMSG.INC'
      DATA NTYP /14/
      DATA DTYCOD /'DdEeAaJjLlIiXx'/
      DATA TYCOD  / 1,1,2,2,3,3,4,4,5,5,6,6,7,7/
      DATA NUMS /'0123456789'/
C-----------------------------------------------------------------------
      IERR = 0
      COUNT = 0
      TYPE = 0
C                                       Initialize count
      ISUM = 0
C                                       Decode count
      DO 50 LOOP = 1,8
         INUM = INDEX(NUMS, FORMAT(LOOP:LOOP))
         IPOINT = LOOP
         IF (INUM.LE.0) GO TO 60
         ISUM = 10 * ISUM + (INUM - 1)
 50      CONTINUE
C                                       Shouldn't get here.
      IERR = 1
      GO TO 980
C                                       default is 1, but 0 is allowed
 60   IF (LOOP.EQ.1) ISUM = 1
      COUNT = ISUM
C                                       Find data type.
      FTYPE = FORMAT(IPOINT:IPOINT)
      ITYP = INDEX (DTYCOD(1:NTYP), FTYPE)
C                                       Shouldn't get here - invalid
C                                       type
      IF (ITYP.LE.0) THEN
         IERR = 1
         GO TO 980
         END IF
      TYPE = TYCOD(ITYP)
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1080) FORMAT
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('TABF3D: COULD NOT DECODE FORMAT ',A)
      END
