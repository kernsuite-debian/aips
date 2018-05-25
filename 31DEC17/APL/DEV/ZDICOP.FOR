      SUBROUTINE ZDICOP (LUN, SYSTEM, USERNO, FLMSIZ, FLMTYP, COPIES,
     *                   BPPIX, WIDTH, HEIGHT, COMMNT, IRET)
C-----------------------------------------------------------------------
C! Open a Dicomed image file and fill in the header
C# Z
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
C   Open a Dicomed image file and fill in the file header.
C
C   Inputs:
C      LUN         I          Logical unit number for file.
C      SYSTEM      C*(*)      The name of the computer system.
C      USERNO      I          AIPS user number.
C      FLMSIZ      I          Film size (assumed to be valid)
C                               0 -> 35mm
C                               1 -> 2"x2"
C                               2 -> 4"x5"
C      FLMTYP      I          Film type (assumed to be valid)
C                               0 -> colour positive
C                               1 -> colour negative
C                               2 -> B/W negative
C      COPIES      I          Number of copies to make (assumed to be
C                               greater than zero)
C      BPPIX       I          Number of bits per pixel (assumed to be
C                               8 or 24
C      WIDTH       I          Width of image in pixels (assumed to be
C                               greater than zero)
C      HEIGHT      I          Height of image in pixels (assumed to be
C                               greater than zero)
C      COMMNT      C*(*)      User's comments
C
C   Output:
C      IRET        I          Return status:
C                               0 -> success
C                               1 -> could not open file
C                               2 -> could not write header
C
C Generic version: stubbed.
C-----------------------------------------------------------------------
      CHARACTER SYSTEM*(*), COMMNT*(*)
      INTEGER   LUN, USERNO, FLMSIZ, FLMTYP, COPIES, BPPIX, WIDTH,
     *          HEIGHT, IRET
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 1
      WRITE (MSGTXT, 1000)
      CALL MSGWRT (8)
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDICOP REQUIRES LOCAL DEVELOPMENT')
      END
