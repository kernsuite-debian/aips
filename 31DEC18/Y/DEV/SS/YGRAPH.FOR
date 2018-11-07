      SUBROUTINE YGRAPH (OP, CHAN, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! turns TV graphics planes on and off
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   YGRAPH is used to turn graphics overlay planes on & off
C   Colors are controlled by YGRAFX and XAS.
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF'
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   SUN Screen Server Version:
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, SCRTCH(256), IERR
C
      INTEGER DAT(4), NWSD, OPCODE, NWVD
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF (CHAN.LT.1) GO TO 999
      IF (CHAN.GT.NGRAPH) GO TO 999
C                                        use GRAPH opcode
      OPCODE = 45
      DAT(1) = CHAN
      DAT(3) = 0
      DAT(4) = 0
      NWVD = 0
      IF (OP.EQ.'ONNN') THEN
         DAT(2) = 1
      ELSE
         DAT(2) = 0
         END IF
      CALL ZSSSXF (OPCODE, DAT, NWVD, SCRTCH, NWSD, IERR)
C
 999  RETURN
      END
