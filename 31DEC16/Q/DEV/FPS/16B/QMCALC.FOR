      SUBROUTINE QMCALC (NGS, NPT, NV)
C-----------------------------------------------------------------------
C! FPS AP version: Compute model visibilities from CLEAN points.
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
C   computes model visibilities from CLEAN components.
C    Input:
C      NGS  I  Number of Gaussian components (Gaussian components
C                are not supported at present, so only NGS=0 will
C                work properly)
C      NPT  I  Number of point components
C      NV   I  Number of visibilities to compute
C   AP storage allocation:
C      first 7*NGS locations -- parameters of Gaussian compo-
C         nents
C      next 4*NPT locations -- parameters of point components.
C         The component parameter vectors (flux,x,y,z) are
C         assumed to have been stored one after another.  Fluxes
C         are to be in units of Janskys, and the position off-
C         sets from the visibility phase reference position are
C         to be in units of radians multiplied by 2 pi.
C      next location  --  number of visibility observations,
C         NOBS
C      next NOBS locations -- spatial frequency u-coordinates,
C         in units of wavelengths
C      next NOBS locations -- v-coordinates in wavelengths
C      next NOBS locations -- w-coordinates in wavelengths
C      next 2*NOBS locations -- MCALC stores its result, the
C         model visibilities, here
C      next 2*NOBS locations -- a work area
C-----------------------------------------------------------------------
      INTEGER NGS, NPT, NV
      INTEGER*2 INGS, INPT, INV
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (NGS.LT.32768) INGS = NGS
      IF (NGS.GE.32768) INGS = NGS - 65536
      IF (NPT.LT.32768) INPT = NPT
      IF (NPT.GE.32768) INPT = NPT - 65536
      IF (NV.LT.32768) INV = NV
      IF (NV.GE.32768) INV = NV - 65536
C                                        Call FPS routine.
      CALL MCALC (INGS, INPT, INV)
C
 999  RETURN
      END
