      SUBROUTINE VELPIX (VPIX, VEL, OBSFRQ, FPIX, FINT,
     *   RADIO, NPIX, NEWVEL)
C-----------------------------------------------------------------------
C! Calculate velocity of pixel.
C# General
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
C  Routine to calculate the velocity of any given pixel when given
C  some relevant information.
C
C  Inputs:
C      VPIX         R        Channel (pixel) number to which
C                            "old" velocity refers.
C      VEL          D        The "old" velocity (m/s)
C      OBSFRQ       D        The reference frequency (Hz)
C      FPIX         R        The channel (pixel) to which the
C                            reference frequency refers.
C      FINT         D        Signed frequency increment per
C                            channel (Hz)
C      RADIO        L        Logical flag which if .true. means
C                            the velocities are the defined
C                            in the radio sense, if .false. in the
C                            optical sense
C      NPIX         R        The channel (pixel) for which the
C                            velocity is desired.
C
C   Output:
C      NEWVEL       D        Velocity (m/s) of NPIX
C-----------------------------------------------------------------------
      LOGICAL   RADIO
      REAL     DELV, VPIX, FPIX, NPIX
      DOUBLE PRECISION  OBSFRQ, FINT, NUX, NEWVEL, VEL
      INCLUDE 'INCS:PSTD.INC'
      INTEGER   LSIGN
C-----------------------------------------------------------------------
      IF (NPIX.NE.VPIX) THEN
C                                       Setup for velocity definition
         LSIGN = 1
         IF (RADIO) LSIGN = -1
C                                       NUx i.e. freq of present
C                                       velocity ref. channel
         NUX = OBSFRQ + (FINT * (VPIX-FPIX))
C                                       Width of channel in m/s
         DELV = -FINT * (VELITE + LSIGN*VEL) / NUX
C                                       Calculate new velocity (m/s)
         NEWVEL = VEL + (DELV * (NPIX-VPIX))
      ELSE
         NEWVEL = VEL
         END IF
C
      RETURN
      END
