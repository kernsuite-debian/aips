      SUBROUTINE ORBIT (AMETER, ECCEN, INCL, OMEGA, PERIG, MANREF, TIME,
     *   TRANOM, PROJA, PROJB, VA, VB, X, Y, Z, VX, VY, VZ)
C-----------------------------------------------------------------------
C! Find cartesian coordinates of an orbital antenna
C# UV VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   Find cartesian coordinates of an orbital antenna having the orbit
C   parameters. Influence of the moon and other planets is not taken
C   into account. Elliptical shape of the Earth is used.
C   Accuracy better than 10 cm.
C   Inputs:
C      Six parameters of the orbit at the reference time
C      AMETER       D        semi-major axis (m)
C      ECCEN        D        eccentricity
C      INCL         D        Inclination of orbit plane to equator (radians)
C      OMEGA        D        RA of the ascending node (radians)
C      PERIG        D        Angle of perigee to the ascending node (radians)
C      MANREF       D        Mean anomaly at the reference time (radians)
C      TIME         D        Time of orbit estimation. (relatevely to
C                            the reference time), in seconds
C   Outputs:
C      TRANOM       D        True anomaly (radians)
C      PROJA        D        Projection of the vector focus-body at
C                            the ellipse major axis
C      PROJB        D        Projection of the vector focus-body at
C                            the ellipse minor axis
C      VA           D        Projection of the velocity vector at
C                            the ellipse major axis
C      VB           D        Projection of the velocity vector at
C                            the ellipse minor axis
C      X            D        Projection of the vector focus-body at
C                            X-axis of equatorial coordinate system
C      Y            D        Projection of the vector focus-body at
C                            Y-axis of equatorial coordinate system
C      Z            D        Projection of the vector focus-body at
C                            Z-axis of equatorial coordinate system
C      VX           D        Projection of the velocity vector at
C                            X-axis of equatorial coordinate system
C      VY           D        Projection of the velocity vector at
C                            Y-axis of equatorial coordinate system
C      VZ           D        Projection of the velocity vector at
C                            Z-axis of equatorial coordinate system
C      The equations are taken from "Orbital Motion" by A.E. Roy
C----------------------------------------------------------------------
      DOUBLE PRECISION  AMETER, ECCEN, MANOM, TRANOM, PROJA, PROJB,
     *   VA, VB, EPSI, MANREF, DENOM, COSF, SINF, FIV, OMEGV,
     *   VX, VY, VZ, R, PERIOD, V, EANOM, FI, INCL, OMEGA, OMEG,
     *   PERIG, X, Y, Z, TIME, POSER, DMANOM, OMEGAT, PERIGT,
     *   MANRET, EN, SIN2I, ESQ, ASQ, DODT, DWDT, DMDT, RE, GM, HJ2
      INCLUDE 'INCS:PSTD.INC'
      DATA RE, GM, HJ2 /6378140.0D0, 3.986012D14, 1.08263D-3/
C-----------------------------------------------------------------------
C                                       recalculate the orbit parameter
C                                       to time moment TIME
C                                       mean angular velocity in 1/sec
      EN = DSQRT(GM/AMETER) / AMETER
      SIN2I = SIN(INCL)**2
      ESQ = 1.0D0 - ECCEN*ECCEN
      ASQ  = (AMETER / RE)**2
      DMDT = EN * (1.D0 + (0.75D0*HJ2*(2.D0-3.D0*SIN2I))
     *   / (ASQ*ESQ**1.5D0))
      DWDT = 3.D0*EN*HJ2 * (4.D0 - 5.D0*SIN2I) / (4.*ASQ*ESQ*ESQ)
      DODT = -1.5D0 * HJ2 * COS(INCL) *EN / (ASQ*ESQ*ESQ)
C                                       new ascending node
      OMEGAT = OMEGA + DODT*TIME
C                                       new arg. of periapsis
      PERIGT = PERIG + DWDT*TIME
C                                       new mean anomaly
      MANRET = MOD((MANREF + DMDT*TIME),TWOPI)
C
C                                       Period in hoursgo
      PERIOD = (TWOPI/EN)/3.6D3
C                                       Calculate mean anomaly at the
C                                       current time
      MANOM = MANRET
C
C
      POSER = 0.1
C                                       Poser- Requested position's
C                                       error, in meters
C                                       Recalculate satalite position
C                                       error into error of MANOM.
C                                       Speed of the satalite 10km/sec
      EPSI = (POSER*EN) / 1.0D4
C                                       solve Keppler equation to obtain
C                                       eccentric anomaly EANOM given mean
C                                       anomaly MANOM
C               MANOM = EANOM - ECCEN * SIN(EANOM)
      EANOM = MANOM
   10 CONTINUE
      EANOM = ECCEN*SIN(EANOM) + MANOM
C                                       Check the solution
      DMANOM = DABS(MANOM - (EANOM-ECCEN*SIN(EANOM)))
      IF (DMANOM .GT. EPSI) GO TO 10
C                                       calculate the radius-vector
C                                       length
      R = AMETER*(1 - ECCEN*DCOS(EANOM))
C                                       calculate velocity-vector length
      V = DSQRT(GM*(2.0/R - 1.0D0/AMETER))
C                                       calculate true anomaly
      DENOM = 1 - ECCEN*DCOS(EANOM)
      COSF = (DCOS(EANOM) - ECCEN) / DENOM
      SINF = (DSQRT(1-ECCEN*ECCEN) * DSIN(EANOM)) / DENOM
      TRANOM = DATAN2(SINF, COSF)
C                                       calculate angle from R to V
      FI = DASIN(DSQRT((AMETER*AMETER*(1-ECCEN*ECCEN)) /
     *   (R*(2*AMETER-R))))
      IF (MANOM .GT. PI) FI = PI - FI
C                                       calculate X, Y projection of R
      PROJA = R * DCOS(TRANOM)
      PROJB = R * DSIN(TRANOM)
C                                       calculate X, Y projection of V
      FIV = TRANOM + FI
      VA = V * DCOS(FIV)
      VB = V * DSIN(FIV)
C                                       Projection of vector Earth
C                                       center-body at the equatorial
C                                       system axis.
      OMEG = PERIGT + TRANOM
      X = R*(DCOS(OMEGAT)*DCOS(OMEG) -
     *   DCOS(INCL)*DSIN(OMEGAT)*DSIN(OMEG))
      Y = R*(DSIN(OMEGAT)*DCOS(OMEG) +
     *   DCOS(INCL)*DCOS(OMEGAT)*DSIN(OMEG))
      Z = R * DSIN(INCL)*DSIN(OMEG)
C                                       Projection of velocity vector
C                                       at the equatorial system axis.
      OMEGV = OMEG + FI
      VX = V * (DCOS(OMEGAT)*DCOS(OMEGV) -
     *   DCOS(INCL)*DSIN(OMEGAT)*DSIN(OMEGV))
      VY = V * (DSIN(OMEGAT)*DCOS(OMEGV) +
     *   DCOS(INCL)*DCOS(OMEGAT)*DSIN(OMEGV))
      VZ = V * DSIN(INCL)*DSIN(OMEGV)
      RETURN
C---------------------------------------------------------------------
      END
