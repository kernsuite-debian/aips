      SUBROUTINE PLGRY (IANGLE, NVAL, VALUES, IROW, IERR)
C-----------------------------------------------------------------------
C! draws grey scale commands in the plot file: Gary plot package
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   This routine will put draw grey scale commands in the plot file.
C   Inputs:
C      IANGLE  I      Angle code. 0 = horizontal, 1 = vertical.
C      NVAL    I      The number of grey scale pixel values.
C      VALUES  R(?)   Grey scale values.
C   Output:
C      IROW    I(*)   Work buffer - cal be equiv VALUES.
C      IERR    I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   IANGLE, NVAL, IROW(*), IERR
      REAL      VALUES(*)
C
      REAL      TEMP
      INTEGER   I, IROUND
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 50 I = 1,NVAL
         TEMP = VALUES(I)
         IF (TEMP.EQ.FBLANK) TEMP = RANGE(1)
         TEMP = MAX (RANGE(1), MIN (RANGE(2), TEMP))
         TEMP = GFAC * TEMP  +  GOFF
         IROW(I) = IROUND (TEMP)
 50      CONTINUE
C
      CALL GRAYPX (NVAL, IANGLE, IROW, PLTBLK, IERR)
C
 999  RETURN
      END
