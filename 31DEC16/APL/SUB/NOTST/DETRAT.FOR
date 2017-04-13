      SUBROUTINE DETRAT (TIME, RA, DEC, X, Y, Z, FREQ, FR)
C-----------------------------------------------------------------------
C! Determines natural fringe rate of an antenna
C# Calibration VLBI Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   Routine to determine the natural fringe rate for an antenna
C   Input:
C      TIME   R   UT (days) at which rate is to be determined
C      RA     D   RA (rads) at epoch
C      DEC    D   Dec (rads) at epoch
C      X      D   ntenna x-coord (m)
C      Y      D   antenna y-coord (m)
C      Z      D   antenna z-coord (m)
C      FREQ   D   reference frequency (Hz)
C   Output:
C      FR     D   natural fringe rate (Hz)
C   Converted from left-hand "VLBI" antenna system 21 Feb 2015
C-----------------------------------------------------------------------
      REAL     TIME
      DOUBLE PRECISION RA, DEC, X, Y, Z, FREQ, FR
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION SHA, BHA, CDEC, SDEC, CSHA, SSHA, SX, SY, SZ,
     *   U, V, DEL, OMEG, GSEC, GST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA GSEC /1.0027375D0/
C-----------------------------------------------------------------------
      OMEG = TWOPI / (24.D0 * 3600.D0)
      GST = GSTIAT + ((TIME * TWOPI) * GSEC)
      GST = MOD (GST, TWOPI)
C                                       Hour angle of source wrt x axis
      SHA = GST - RA
C                                       Hour angle of baseline wrt x
C                                       axis
      BHA = ATAN2(Y,X)
C                                       Calculate needed cos and sin
C                                       values
      CDEC = DCOS(DEC)
      SDEC = DSIN(DEC)
      CSHA = DCOS(SHA)
      SSHA = DSIN(SHA)
C                                       Source vector coordinates
      SZ = SDEC
      SX = CDEC * CSHA
      SY = CDEC * SSHA
C                                       Calculate u and v
      U = (X * SY + Y * SX) / CDEC
      V = Z * CDEC - (SX * X - SY * Y) * SDEC / CDEC
C                                       Calculate delay and fringe rate
      DEL = (X*SX + Y*SY + Z*SZ) / VELITE
      FR = U * OMEG * CDEC * FREQ / VELITE
C
      RETURN
      END
