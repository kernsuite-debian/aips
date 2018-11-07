      SUBROUTINE ZTAPIO (OPER, LN, NAME, FD, BYTREQ, BYTRED, BUF,
     *   SYSERR)
C-----------------------------------------------------------------------
C! tape operations for IMPFIT (compressed FITS transport tape)
C# Z Tape
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
C   tape operations for IMPFIT (compressed FITS transport tape)
C   Inputs:
C      OPER     C*4    'OPEN', 'READ', 'CLOS'
C      LN       I      length of name
C      NAME     C*(*)  physical file name
C      BYTREQ   I      bytes to be read
C   In/out:
C      FD       I(*)   file descriptor (set on OPEN, else used)
C   Output:
C      BYTRED   I      Bytes read on READ
C      BUF      I(*)   Data buffer read
C      SYSERR   I      System error code
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   LN, FD, BYTREQ, BYTRED, BUF(*), SYSERR
      CHARACTER OPER*4, NAME*(*)
C
C-----------------------------------------------------------------------
      WRITE (6,1000)
      SYSERR = 16
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZTAPIO REQUIRES LOCAL DEVELOPMENT FOR IMPFIT')
      END
