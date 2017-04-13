      SUBROUTINE YWINDO (OPER, WIND, IERR)
C-----------------------------------------------------------------------
C! set the current window (viewport) into the TV memory
C# Y1 TV-basic
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
C   YWINDO reads the current viewport into the TV memory.
C   Inputs:
C      OPER   C*4    'READ', 'WRIT'
C   In/out:
C      WIND   I(4)   BLC x,y, TRC x,y of window in TV pixels
C                    In: desired window ('WRIT' only)
C                    Out: actual window given
C   Output:
C      IERR    I     error code of Z...XF: 0 -> ok, 2 -> input error
C                       -1 => writes not allowed on this TV device
C   Generic version - returns full memory size.
C   'WRIT' =>  error = -1
C-----------------------------------------------------------------------
      CHARACTER OPER*(*)
      INTEGER   WIND(4), IERR
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IF (OPER.EQ.'READ') THEN
         IERR = 0
      ELSE IF (OPER.EQ.'WRIT') THEN
         IERR = -1
      ELSE
         IERR = 2
         MSGTXT = 'YWINDO: SUPPORTS READ/WRIT ONLY, NOT ' // OPER
         CALL MSGWRT (8)
         END IF
C                                       Full screen always
      WIND(1) = 1
      WIND(2) = 1
      WIND(3) = MAXXTV(1)
      WIND(4) = MAXXTV(2)
C
 999  RETURN
      END
