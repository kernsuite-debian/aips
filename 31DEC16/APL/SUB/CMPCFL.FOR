      SUBROUTINE CMPCFL
C-----------------------------------------------------------------------
C! Compresses unused entries from the end of the DFIL.INC commons.
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Compresses the DFIL.INC commons removing unused entries from the end
C   of the list  Interior entries may not be shifted since some routines
C   remember where things were in the list.
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Catalog flag arrays
 20   IF (NCFILE.GT.0) THEN
         IF ((FVOL(NCFILE).LE.0) .OR. (FCNO(NCFILE).LE.0) .OR.
     *      (FRW(NCFILE).LT.0)) THEN
            NCFILE = NCFILE - 1
            GO TO 20
            END IF
         END IF
C                                       Scratch files
 120  IF (NSCR.GT.0) THEN
         IF ((SCRVOL(NSCR).LE.0) .OR. (SCRCNO(NSCR).LE.0)) THEN
            NSCR = NSCR - 1
            GO TO 120
            END IF
         END IF
C
 999  RETURN
      END
