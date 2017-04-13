      SUBROUTINE ZIPACK (OPER, NVALS, LBUFF, PNTR, SBUFF)
C-----------------------------------------------------------------------
C! pack/unpack long integers into short integer buffer
C# Z IO-TV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003
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
C   Packs or unpacks integers from/to long-integer buffer to/from
C   short-integer buffer with/without byte swap.  Must work in place.
C   Inputs:
C      OPER     C*4      'LTOS' long to short - no swap
C                        'STOL' short to long - no swap
C                        'LOTS' long to short - byte swap
C                        'SOTL' short to long - byte swap
C      NVALS    I        Number words
C      PNTR     I        start position in I*2 buffer
C   In/Out:
C      LBUFF    I(*)     Long-integer buffer
C      SBUFF    I(*)     Short-integer buffer
C   Generic version - works on 32-bit machines
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NVALS, LBUFF(*), PNTR, SBUFF(*)
C
      INTEGER   CODE, LPNTR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (NVALS.LE.0) GO TO 999
      LPNTR = MAX (1, PNTR)
      IF (NBITWD.NE.32) THEN
         MSGTXT = 'ZIPACK NEEDS LOCAL DEVELOPMENT'
         CALL MSGWRT (10)
         STOP 'ZIPACK NEEDS LOCAL DEVELOPMENT'
         END IF
      CODE = BYTFLP
      IF ((OPER.EQ.'LTOS') .OR. (OPER.EQ.'STOL')) CODE = 2 * (CODE/2)
      IF ((OPER.EQ.'LTOS') .OR. (OPER.EQ.'LOTS')) CODE = CODE + 4
      CALL ZIPAC2 (NVALS, CODE, BYTFLP, LPNTR, SBUFF, LBUFF)
C
 999  RETURN
      END
