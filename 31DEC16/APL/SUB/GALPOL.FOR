      SUBROUTINE GALPOL (EPOCH, RAGP, DECGP, LONCP)
C-----------------------------------------------------------------------
C! returns the RA and Dec of the galactic center for an EPOCH
C# Coordinates
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
C   GALPOL returns the coordinates of the galactic center for the epoch
C   of observation.
C   Inputs:
C      EPOCH   R          Date of observation years (ie 1950, 2000 etc)
C   Output:
C      RAGP    D          Ra  of north galactic pole (degrees)
C      DECGP   D          Dec of north galactic pole (degrees)
C      LONCP   D          Longitude of North Celestial pole (degrees)
C-----------------------------------------------------------------------
      REAL      EPOCH
      DOUBLE PRECISION RAGP, DECGP, LONCP
C
      REAL      DAY
      PARAMETER (DAY=1./365.25)
C-----------------------------------------------------------------------
C                                       If less than a day from 1950.0
      IF (ABS(EPOCH-1950.).LT.DAY) THEN
         RAGP  = 192.25D0
         DECGP =  27.40D0
         LONCP = 123.00D0
C                                       these assume 2000.0
      ELSE
         RAGP  = 192.859375D0
         DECGP =  27.128643D0
         LONCP = 122.932018D0
         END IF
C
 999  RETURN
      END
