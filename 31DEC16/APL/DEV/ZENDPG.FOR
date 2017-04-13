      SUBROUTINE ZENDPG (LINE)
C-----------------------------------------------------------------------
C! advance printer if needed to avoid electrostatic-printer "burn-out"
C# Z Printer
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
C   Advance the line printer to avoid "burn-out" on electro-static type
C   devices.
C   Inputs:
C      LINE     I           # lines printed on page so far
C   Generic version, does a partial page.
C   We should consider putting this function in ZLPCL*.
C   Modcomps needed another page, UNIX seems to need no page advance.
C   This does well for VMS.
C-----------------------------------------------------------------------
      INTEGER    LINE
C
      INTEGER    LUNPRT
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNPRT /1/
C-----------------------------------------------------------------------
      IF (LINE.GT.0.7*PRTMAX) WRITE (LUNPRT,1000)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('1')
      END
