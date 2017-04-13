      SUBROUTINE UNYBUF (BUFSR, BUFFER, HEADER)
C-----------------------------------------------------------------------
C! unpacks a machine-independent integer buffer into local command line
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
C   UNYBUF is a routine for taking packaged TV commands in a machine
C   independent form from a previous transmission through a
C   communication line.  The integer parameter array is converted from
C   16-bit packed integers beginning at the 9th 16-bit integer in
C   HEADER to local integers.
C   Inputs:
C      BUFSR   I      Number I values in BUFFER
C      HEADER  I(*)   Machine-independent (FITS standard) parameters for
C                     communication, in 16-bit form.  Must be >= 9
C                     longer than BUFFER
C   Output:
C      BUFFER  I(*)   Buffer of parameter values - in local integer form
C-----------------------------------------------------------------------
      INTEGER   BUFSR, BUFFER(*), HEADER(*)
C-----------------------------------------------------------------------
C                                       Unpack the data:
      IF (BUFSR.GT.0) CALL ZI16IL (BUFSR, 9, HEADER, BUFFER)
C
 999  RETURN
      END
