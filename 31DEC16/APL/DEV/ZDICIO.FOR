      SUBROUTINE ZDICIO (LUN, FLMTYP, ROWNUM, RED, GREEN, BLUE, WIDTH, 
     *                   IRET)
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
C! Write an image row to a Dicomed image file.
C# Z
C   This software is the subject of a User agreement and is cobfidential
C   in nature.  It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   Write an image row to a Dicomed image file.  If the filmtype is
C   B/W (FLMTYP = 2) then only the red channel is written.
C
C   Inputs:
C      LUN        I            Logical unit number of image file
C      FLMTYP     I            Filmtype: 0 -> colour positive
C                                        1 -> colour negative
C                                        2 -> B/W negative
C      ROWNUM     I            Number of row to write.  Bottom row
C                              is ROWNUM 1.
C      RED        I(*)         Red values for each pixel(each 0-255)
C      GREEN      I(*)         Green values for each pixel(each 0-255)
C      BLUE       I(*)         Blue values for each pixel (each 0-255)
C      WIDTH      I            Number of pixels in row (> 0)
C
C   Output:
C      IRET       I            Return status: 
C                                 0 -> success.
C
C  Generic version: stubbed.
C-----------------------------------------------------------------------
      INTEGER   LUN, FLMTYP, ROWNUM, RED(*), GREEN(*), BLUE(*), WIDTH,
     *          IRET
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      WRITE (MSGTXT, 1000)
      CALL MSGWRT (8)
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDICIO REQUIRES LOCAL DEVELOPMENT')
      END
