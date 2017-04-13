      SUBROUTINE YTVCLS (BUF, IERR)
C-----------------------------------------------------------------------
C! close the TV, including TV device and TV control/parameter disk file
C# Y1 TV-basic
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
C   YTVCLS closes the TV device and the TV status disk file, updating
C   the information on the disk.  Actual device call done by YTVCL2.
C   Outputs:
C      BUF   I(256)     Scratch buffer
C      IERR  I          Error code : 0 => ok
C                          else as returned by ZFIO
C                          11 => close disk error
C                          12 => close device error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      INTEGER   IERR, BUF(256)
C
      INTEGER   JERR, IDUMMY
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
C                                       send closing info
      BUFSW = 0
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YTVCLS', BUFSW, BUFSR, IDUMMY, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       Close TV device
      CALL YTVCL2 (TVLUN, TVIND, JERR)
      IF (IERR.EQ.0) IERR = JERR
C
      TVLUN = 0
      TVLUN2 = 0
      TVIND2 = 0
      TVIND = 0
C
 999  RETURN
      END
