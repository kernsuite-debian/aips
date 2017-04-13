@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QMCALC (NGS, NPT, NV)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Compute model visibility from point model.
C# AP-appl UV Obselete
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
C   Pseudo-AP version
C   MCALC computes model visibilities from CLEAN components.
C   Input:
C      NGS  I  Number of Gaussian components (Gaussian components
C              are not supported at present, so only NGS=0 will
C              work properly)
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
      INTEGER   NGS, NPT, NV, INGS, INPT, INOBS,
     *   IU, IV, IW, IGS, IPT, IWK, I, IVMOD,
     *   M1, M2, M3, M4, M5
      DATA IGS /2/
C-----------------------------------------------------------------------
      IF (NV.LE.0) GO TO 999
C                                       Set up pointers, etc.
      IPT = IGS + (NGS * 7)
      INGS = IPT + (NPT * 4)
      INPT = INGS + 1
      INOBS = INPT + 1
      IU = INOBS + 1
      IV = IU + NV
      IW = IV + NV
      IVMOD = IW + NV
      IWK = IVMOD + 2 * NV
      M1 = IPT + 1
      M2 = M1 + 1
      M3 = M2 + 1
      M4 = IWK + 1
C                                       Loop
      DO 10 I = 1,NV
         CALL QVSMUL (M1, 4, IU, IWK, 2, NPT)
         CALL QVSMA (M2, 4, IV, IWK, 2, IWK, 2, NPT)
         CALL QVSMA (M3, 4, IW, IWK, 2, IWK, 2, NPT)
         CALL QCVEXP (IWK, 2, IWK, 2, NPT)
         CALL QCRVMU (IWK, 2, IPT, 4, IWK, 2, NPT)
         CALL QSVE (IWK, 2, IVMOD, NPT)
         M5 = IVMOD + 1
         CALL QSVE (M4, 2, M5, NPT)
C                                       Update pointers
         IU = IU + 1
         IV = IV + 1
         IW = IW + 1
         IVMOD = IVMOD + 2
 10      CONTINUE
C
 999  RETURN
      END
