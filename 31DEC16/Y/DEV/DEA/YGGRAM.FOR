      SUBROUTINE YGGRAM (OP, CURSOR, RGBBUF, IERR)
C-----------------------------------------------------------------------
C! controls the TV graphics color assignments
C# Y3 TV-basic
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
C   YGGRAM reads/writes the TV graphics color assignment RAM.  The
C   data are packed in this look up table for colors as:
C         bit 15 (msb)   :   1 - graphics replace 0 - add to image
C         bits 10-14     :   red value for this graphics value
C         bits  5- 9     :   green
C         bits  0- 4     :   blue
C   Inputs:
C      OP      C*4       'READ' from DEANZA or 'WRIT' to DEANZA
C      CURSOR  L         T => set CURSOR ON colors
C                              else set CURSOR OFF colors
C   In/Out:
C      RGBBUF  I(256)    data array
C   Out:
C      IERR    I         error code of Z...XF : 0 => ok
C   DeAnza version: Nov-83  W. Jaffe
C-----------------------------------------------------------------------
      CHARACTER OP*4
      LOGICAL   CURSOR
      INTEGER   RGBBUF(256), IERR
C
      INCLUDE 'INCS:YDEA.INC'
      INTEGER   NB, CA, WG, OVRADR, CURON, CUROFF
      LOGICAL   WAIT
      DATA NB, WAIT /512, .TRUE./
C                                       Word Group 1 is VOC
C                                       Channel address 0 is 1st
C                                       Overlay LUT for VOC unit 0.
C                                       Other overlay LUTs are not used
      DATA CA, WG /0, 1/
C                                       Overlay byte addr 0 is start
C                                       of cursor on area.  Z200 and
C                                       Z400 are complicated double
C                                       cursor areas and are ignored
C                                       here.  Z600 is start of no
C                                       cursor on area.
      DATA CURON, CUROFF /0, 1536/
C-----------------------------------------------------------------------
      IF (CURSOR) THEN
         OVRADR = CURON
      ELSE
         OVRADR = CUROFF
         END IF
C                                       Set VOCLUT address
      CALL ZDEAXF ('LVR ', OVRADR, 2, 0, 3, WAIT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read or Write WORD array
      IF (OP.EQ.'READ') THEN
         CALL ZDEAXF ('RWA ', RGBBUF, NB, CA, WG, WAIT, IERR)
      ELSE IF (OP.EQ.'WRIT') THEN
         CALL ZDEAXF ('LWA ', RGBBUF, NB, CA, WG, WAIT, IERR)
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
