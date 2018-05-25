      SUBROUTINE ZV20X2 (OPER, FCB, NBYTES, COMST, COMDB, BUFFER, IERR)
C-----------------------------------------------------------------------
C! does I/O to Comtal Vision 1/20 TV device
C# Z2 TV-IO
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
C   Performs the actual QIOW operation to transmit data to or from the
C   COMTAL VISION 1/20 TV.  For further information concerning the
C   input parameters, see the ZB.DOC writeup that comes with the COMTAL
C   device driver, ZBDRIVER.
C   Inputs:
C      OPER     C*4      Operation: 'READ', 'WRIT'
C      NBYTES   I        8-bit bytes in BUFFER to transfer
C      COMST    I        Value in the CSR word of the ZB interface.
C      COMDB    I        The value to be placed on the R lines.
C   In/out:
C      FCB      I(*)     File control block
C      BUFFER   I*2(*)   IO Buffer
C   Output:
C      IERR     I        Error code: 0 => okay, 3 IO error
C   Generic version - stub
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), NBYTES, COMST, COMDB, IERR
      INTEGER*2 BUFFER(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZV200X2 REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (9)
C
 999  RETURN
      END
