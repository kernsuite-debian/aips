      SUBROUTINE GETWIN (ILUN, INODIM, IBLC, ITRC, IERR)
C-----------------------------------------------------------------------
C! Get current window of file open in WAWA IO system.
C# IO-wawa
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
C  GETWIN (for use with WAWA IO only) will get the current window of
C  an open map file, and the number of dimensions.  Any of the 7
C  sets of window values which are unused will be filled with zeros.
C  INPUT:   ILUN   I   LUN this file was opened under.
C  OUTPUTS: INODIM I   number of dimensions for this file.
C           IBLC   I(7)   bottom left hand corner of window.
C           ITRC   I(7)   top right hand corner of window.
C           IERR   I   standard WAWA error code.
C-----------------------------------------------------------------------
      INTEGER   IBLC(7), ITRC(7)
      INTEGER   ILUN, IPTR, INODIM, IERR
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       Find proper LUN
      DO 100 IPTR = 1,EFIL
         IF (ILUN.EQ.FILTAB(POLUN,IPTR)) GO TO 200
 100     CONTINUE
C                                       File not found.
      IERR = 1
      GO TO 999
C                                       File found. Fill in values.
 200  CONTINUE
      INODIM = FILTAB(PODIM,IPTR)
      CALL FILL (7, 0, IBLC)
      CALL FILL (7, 0, ITRC)
      CALL COPY (INODIM, FILTAB(POBLC,IPTR), IBLC)
      CALL COPY (INODIM, FILTAB(POTRC,IPTR), ITRC)
C
 999  RETURN
      END
