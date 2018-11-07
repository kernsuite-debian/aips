      SUBROUTINE UVZRSP (WTOFF, VSOFF, LVIS, FLUX, WEIGHT, NCHGRD, BUFF)
C-----------------------------------------------------------------------
C! Puts zero spacing flux and weight into a UV Buffer.
C# UV-util
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
C   UV ZeRo SPace: Puts Zero spacing flux and weight into a UV Buffer.
C   Band width synthesis is handled by this routine
C   Inputs:
C      WTOFF    I      Offset to Weight of UV point
C      VSOFF    I      Offset to UV Visibility record
C      LVIS     I      Length of Visibility record
C      FLUX     R      Zero Spacing Flux
C      WEIGHT   R      Weight of Zero Spacing Flux
C      NCHGRD   I      Number of channels to Grid
C   Output:
C      BUFF     R(*)   Visibility record
C-----------------------------------------------------------------------
      INTEGER   WTOFF, VSOFF, LVIS, NCHGRD
      REAL      FLUX, WEIGHT, BUFF(*)
C-----------------------------------------------------------------------
      INTEGER JJ, JNDX
C-----------------------------------------------------------------------
      JNDX = 1
C                                       Init UV values to zero
      CALL RFILL (VSOFF, 0.0, BUFF)
C                                       If one weight per vis
      IF (LVIS.LE.2) BUFF(JNDX+WTOFF) = WEIGHT
C                                       index to start of first vis rec
C                                       for first channel to average
      JNDX = JNDX + VSOFF
C                                       for all channels to average
      DO 10 JJ = 1,NCHGRD
         BUFF(JNDX) = FLUX
         BUFF(JNDX+1) = 0.0
         IF (LVIS.GT.2) BUFF(JNDX+2) = WEIGHT
         JNDX = JNDX + LVIS
 10      CONTINUE
C
 999  RETURN
      END

