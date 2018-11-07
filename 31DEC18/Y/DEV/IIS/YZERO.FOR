      SUBROUTINE YZERO (CHAN, IERR)
C-----------------------------------------------------------------------
C! fill a TV memory plane with zeros
C# TV-util
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
C   YZERO fills an TV TV memory plane with zeros the fast way.
C   Inputs:
C      CHAN   I     channel # ( 1 - NGRAY+NGRAPH), 0 => all
C   Outputs:
C      IERR   I     error code of Z...XF: 0 - ok, 2 - input error
C   Version for IIS Models 70 and 75
C-----------------------------------------------------------------------
      INTEGER   CHAN, IERR
C
      LOGICAL   T, F
      INTEGER   IC, IB, BIT16
      INCLUDE 'INCS:DTVC.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BIT16 /-32768/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((CHAN.LT.0) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      IF (CHAN.LE.NGRAY) THEN
         IC = CHAN - 1
         IF (IC.GE.0) IC = 2**IC
         IB = -1
      ELSE
         IC = BIT16
         IB = 2 ** (CHAN-NGRAY-1)
         END IF
C                                       Do it
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
      CALL YFDBCK (1, IC, IB, 0, F, F, T, F, F, IERR)
C
 999  RETURN
      END
