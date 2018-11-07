      SUBROUTINE YISDRM (OP, ADDR, NWORDS, GOADDR, BUFFER, IERR)
C-----------------------------------------------------------------------
C! read/write data memory of NRAO-ISU device
C# Y3 TV-appl
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
C   YISDRM is a special Y routine for the NRAO Image Storage Unit -
C   a device interposed between the host CPU and the IIS Model 70 TV.
C   YISMPM reads or writes the data memory of the device.
C   Inputs:
C      OP      C*4     'READ', 'WRIT' to micro processor
C      ADDR    I       Desired micro processor address
C      NWORDS  I       Number of words in BUFFER
C      GOADDR  I       Microprocessor address to branch on completion
C   In/Out:
C      BUFFER  I(NWORDS)   code for/from micro processor
C   Output:
C      IERR    I       Error code of Z...XF: 0 okay, 2 input error.
C   NO TV ISU VERSION
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   ADDR, NWORDS, GOADDR, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV IMAGE STORAGE UNIT CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
