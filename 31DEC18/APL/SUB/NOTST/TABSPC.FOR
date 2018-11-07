      SUBROUTINE TABSPC (FORMAT, COUNT, IWIDTH, FRAC, TYPE, IERR)
C-----------------------------------------------------------------------
C! Determines repeat count and data type for FITS 3-D tables entries.
C# FITS Tape EXT-util Parsing
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
C   Determines the TABle SPeCification of a variable with from a FORMAT
C   type. The repeat count and data type from a format used by the
C   FITS 1-D and 3-D tables.  The recognized data types are:
C       Format   Type code         Type
C         D        1         Double precision real
C         E        2         Single precision real
C         A        3         Character
C         J        4         Long integer (32 bits)
C         L        5         Logical
C         I        6         Short integer (16 bits)
C         X        7         bit array
C   Input:
C      FORMAT   C*8   Format, eg. '4E15.6'
C   Output:
C      COUNT    I     Element count
C      IWIDTH   I     The width of the field.
C      FRAC     I     The fractional part of the field (E and D only).
C      TYPE     I     Data type
C      IERR     I     Error code, 0 => OK,   else failed.
C-----------------------------------------------------------------------
      CHARACTER FORMAT*(*)
      INTEGER   COUNT, IWIDTH, FRAC, TYPE, IERR
C
      CHARACTER DTYCOD*14, NUMS*10, FTYPE*1, ONECHR
      INTEGER   TYCOD(14), NTYP, ITYP, ISUM, LOOP, INUM, INDEX, IPOS,
     *   ISTART
      INCLUDE 'INCS:DMSG.INC'
      PARAMETER (NTYP=14)
      DATA DTYCOD /'DdEeAaJjLlIiXx'/
      DATA TYCOD  / 1,1,2,2,3,3,4,4,5,5,6,6,7,7/
      DATA NUMS /'0123456789'/
C-----------------------------------------------------------------------
      IERR = 1
      COUNT = 0
      TYPE = 0
C                                       Initialize count
      ISUM = 0
C                                       Decode count
      DO 50 LOOP = 1,8
         INUM = INDEX(NUMS, FORMAT(LOOP:LOOP))
         IPOS = LOOP
         IF (INUM.LE.0) GO TO 60
         ISUM = 10 * ISUM + (INUM - 1)
 50      CONTINUE
C                                       Shouldn't get here.
      GO TO 980
C                                       No counts are interpreted as 1 count
 60   IF (ISUM.EQ.0 .AND. IPOS.EQ.1 .AND. FORMAT(1:1).NE.'0') THEN
         COUNT = 1
      ELSE
         COUNT = ISUM
         END IF
C                                       Find data type.
      FTYPE = FORMAT(IPOS:IPOS)
      ITYP = INDEX (DTYCOD(1:NTYP), FTYPE)
C                                       Shouldn't get here - invalid
C                                       type
      IF (ITYP.LE.0) GO TO 980
      TYPE = TYCOD(ITYP)
C                                       Determine width.
      IWIDTH = 0
C                                       Width starts after Type
      ISTART = IPOS + 1
C                                       If no chars left exit
      IF (ISTART.GT.8) GO TO 980
      DO 300 IPOS = ISTART,8
         ONECHR = FORMAT(IPOS:IPOS)
         IF ((ONECHR.EQ.'.') .OR. (ONECHR.EQ.' ')) GO TO 400
         INUM = INDEX (NUMS, ONECHR)
C                                       if invalid character, jump to exit
         IF (INUM.LE.0) GO TO 980
         IWIDTH = (10 * IWIDTH) + (INUM - 1)
 300     CONTINUE
C                                       if not R*8 or R*4, we are finished
 400  IF (TYPE.GE.3) THEN
         FRAC = -1
         IERR = 0
         GO TO 999
         END IF
C                                       Determine fraction.
      FRAC = 0
      ISTART = IPOS + 1
      IF (ISTART.GT.8) GO TO 980
      DO 500 IPOS = ISTART,8
         ONECHR = FORMAT(IPOS:IPOS)
C                                       End of format, done
         IF (ONECHR.EQ.' ') THEN
            IERR = 0
            GO TO 999
            END IF
         INUM = INDEX (NUMS, ONECHR)
C                                       invalid number
         IF (INUM.LE.0) GO TO 980
         FRAC = (10 * FRAC) + (INUM - 1)
 500     CONTINUE
 
      IERR = 0
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1080) FORMAT
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('TABSPC: COULD NOT DECODE FORMAT ',A)
      END
