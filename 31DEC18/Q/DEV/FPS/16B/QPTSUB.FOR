      SUBROUTINE QPTSUB (UV, VIS, IAPCC, LREC, NUMVIS, MCOMP, NVIS)
C-----------------------------------------------------------------------
C! FPS AP version: Subtracts transform of point model from UV data
C# AP-util
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
C   FPS AP version
C   Subtracts CLEAN components from visibility data.
C   Currently does only U, V, and W terms.
C    Inputs:
C     UV     I  Start address of UV vector
C     VIS    I  Start address of visibility vector
C     IAPCC  I  Start address of CLEAN component vector
C     LREC   I  Increment of UV and VIS
C     NUMVIS I  Number of visibilities
C     MCOMP  I  Number of CLEAN components
C     NVIS   I  Number of vis to correct
C                 assumed to be separated by three words
C     AP Locations 0 and 1 are used.
C     CLEAN component data are arrainged 6 words to the component:
C           1 = Flux density
C           2 = used.
C           3 = used.
C           4 = X
C           5 = Y
C           6 = Z
C-----------------------------------------------------------------------
      INTEGER UV, VIS, IAPCC, LREC, NUMVIS, MCOMP, NVIS
      INTEGER*2 IUV, IVIS, IIAPCC, ILREC, INUMVI, IMCOMP, INVIS
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UV.LT.32768) IUV = UV
      IF (UV.GE.32768) IUV = UV - 65536
      IF (VIS.LT.32768) IVIS = VIS
      IF (VIS.GE.32768) IVIS = VIS - 65536
      IF (IAPCC.LT.32768) IIAPCC = IAPCC
      IF (IAPCC.GE.32768) IIAPCC = IAPCC - 65536
      ILREC = LREC
      IF (NUMVIS.LT.32768) INUMVI = NUMVIS
      IF (NUMVIS.GE.32768) INUMVI = NUMVIS - 65536
      IF (MCOMP.LT.32768) IMCOMP = MCOMP
      IF (MCOMP.GE.32768) IMCOMP = MCOMP - 65536
      INVIS = NVIS
C                                        Call FPS routine.
      CALL PTSUB (IUV, IVIS, IIAPCC, ILREC, INUMVI, IMCOMP, INVIS)
C
 999  RETURN
      END
