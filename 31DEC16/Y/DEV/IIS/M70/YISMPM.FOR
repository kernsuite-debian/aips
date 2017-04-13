      SUBROUTINE YISMPM (OP, ADDR, NWORDS, GOADDR, BUFFER, IERR)
C-----------------------------------------------------------------------
C! reads/writes microprocessor memory of the NRAO-ISU device
C# TV-appl
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
C   YISMPM is a special Y routine for the NRAO Image Storage Unit -
C   a device interposed between the host CPU and the IIS Model 70 TV.
C   YISMPM reads or writes the micro processor memory
C   Inputs:
C      OP      C*4       'READ", 'WRIT' to micro processor
C      ADDR    I         Desired micro processor address
C      NWORDS  I         Number of words in BUFFER
C      GOADDR  I         Microprocessor address to branch on completion
C   In/Out:
C      BUFFER  I(NWORDS) code for/from micro processor
C   Output:
C      IERR    I         Error code of Z...XF: 0 okay, 2 input error
C   IIS Model 70 version - ISU present.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   ADDR, NWORDS, GOADDR, BUFFER(*), IERR
C
      INTEGER   ISUOPC, HEADER(8), NB
      DATA ISUOPC /2/
C-----------------------------------------------------------------------
C                                       Build header
      HEADER(1) = 3
      HEADER(2) = -NWORDS
      HEADER(3) = ISUOPC
      IF (OP.EQ.'WRIT') HEADER(3) = ISUOPC - 1
      HEADER(4) = 0
      HEADER(5) = 0
      HEADER(6) = ADDR
      HEADER(7) = 0
      HEADER(8) = GOADDR
C                                       Do I/O
      NB = 2 * NWORDS
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C
 999  RETURN
      END
