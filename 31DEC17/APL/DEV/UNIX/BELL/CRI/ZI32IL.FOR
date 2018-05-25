      SUBROUTINE ZI32IL (NVAL, NP, INB, OUTB)
C-----------------------------------------------------------------------
C! convert FITS-standard 32-bit integers from buffer into local integers
C# Binary
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
C   Extract 32-bit, 2's complement integers from an input buffer and put
C   them into an output buffer in local large integer form.  This must
C   work even when the address of the input and output buffers is the
C   same.  The IBM order applies to the input (i.e., the most
C   significant part of the 32-bit integer is in the lower index of the
C   input buffer and the least significant part is in the higher index.
C   Inputs:
C      NVAL   I      # values to extract
C      NP     I      Starting position in the input buffer (1-relative)
C                    in units of 32-bit integers
C      INB    I(*)   Input buffer
C   Output:
C      OUTB   I(*)   Output buffer
C   UNICOS version (calls Cray's USICTC to perform the conversion).
C-----------------------------------------------------------------------
      INTEGER   NVAL, NP, INB(*), OUTB(*)
C
      INTEGER   IBYTE
C-----------------------------------------------------------------------
      IF (NVAL.LE.0) GO TO 999
C                                       Use Cray's USICTC (IBYTE =
C                                       1-relative byte starting
C                                       position in INB).
         IBYTE = (NP * 4) - 3
         CALL USICTC (INB, IBYTE, OUTB, NVAL, 4)
C
 999  RETURN
      END
