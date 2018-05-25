      SUBROUTINE SCALMM (NXPIXS, NYPIXS, XPXSEP, YPXSEP, SCALEF,
     *   XDOTMM, YDOTMM, ASPMM, SCALEX, SCALEY)
C-----------------------------------------------------------------------
C! computes plot scaling factors and plot scale in arc sec per mm
C# Plot-util
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
C   SCALMM will compute a plot scale given a requested arc sec / mm
C   scaling or compute the self-scaling effective arc sec/mm.  If ASPMM
C   is not zero, SCALMM will calculate a new SCALEX, SCALEY and return
C   these if smaller than the old values.  Otherwise this routine will
C   calculate a new ASPMM based on SCALEX, SCALEY.
C   Inputs:
C      NXPIXS  R   the number of X pixels.
C      NYPIXS  R   the number of Y pixels.
C      XPXSEP  R   the separation of X pixels (in arc secs).
C      YPXSEP  R   the separation of Y pixels (in arc secs).
C      SCALEF  R   the scale factor used in scaling plot file values.
C      XDOTMM  R   the X absolute units for the plot device in mm.
C      YDOTMM  R   the Y absolute units for the plot device in mm.
C   In/out:
C      ASPMM   R   arc sec per millimeters.
C      SCALEX  R   the x scale factor for plotting.
C      SCALEY  R   the Y scale factor for plotting.
C-----------------------------------------------------------------------
      REAL   NXPIXS, NYPIXS, XPXSEP, YPXSEP, SCALEF, XDOTMM, YDOTMM,
     *   ASPMM, SCALEX
C
      REAL   SCALEY, SCALX0, SCALY0
C-----------------------------------------------------------------------
C                                      Maybe calculate SCALEX, SCALEY.
      IF (ASPMM.LE.0.0) GO TO 20
         SCALX0 = ABS (NXPIXS * XPXSEP * XDOTMM / (ASPMM * SCALEF))
         SCALY0 = ABS (NYPIXS * YPXSEP * YDOTMM / (ASPMM * SCALEF))
C                                      Return new SCALEX, SCALEY.
         IF ((SCALX0.GT.SCALEX) .OR. (SCALY0.GT.SCALEY)) GO TO 20
            SCALEX = SCALX0
            SCALEY = SCALY0
            GO TO 999
C                                      Use old SCALEX, SCALEY
C                                      Return effective arc sec/mm
 20   CONTINUE
         ASPMM = ABS (NXPIXS * XPXSEP * XDOTMM) / (SCALEX * SCALEF)
C
 999  RETURN
      END
