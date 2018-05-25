      SUBROUTINE ZUVXPN (NCORR, VISIN, WTSCL, VISOUT)
C-----------------------------------------------------------------------
C! Expands packed visibility data and adds weight
C# Binary Z
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Routine to expand packed uv data to unpacked form.  One AIPS
C   logical uv data record is processed at a time.
C   Output array can be input array.
C    Inputs:
C     NCORR  I        Number of correlator values in data
C     VISIN  R(*)     Packed visibility data with local magic value
C                     blanking.
C     WTSCL  R(*)    "Weight" and "scale" random parameters for the
C                     packed record.
C    Output:
C     VISOUT  R(3,*)  Unpacked uv data as real, imag and weight per
C                     correlator.
C   Version for machines with 32-bit integers/reals
C-----------------------------------------------------------------------
      INTEGER   NCORR, VISIN(*)
      REAL      WTSCL(2), VISOUT(3,*)
C
      INTEGER   IBLANK, LOOP, ILOOP, NP, IVIS(2)
      REAL      SCL, WT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       Magic value
      DATA IBLANK /-32767/
C-----------------------------------------------------------------------
      IF (NCORR.LE.0) GO TO 999
C                                       Validity check
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP ' ZUVXPN NEEDS LOCAL DEVELOPMENT'
         END IF
      WT = WTSCL(1)
      SCL = WTSCL(2)
C                                       Unpack
      DO 200 LOOP = 1,NCORR
C                                       Work from end backward
         ILOOP = NCORR + 1 - LOOP
         NP = 2 * ILOOP - 1
         CALL ZIPACK ('STOL', 2, IVIS, NP, VISIN)
C                                       If value not blanked
         IF (IVIS(1).NE.IBLANK) THEN
            VISOUT(1,ILOOP) = IVIS(1) * SCL
            VISOUT(2,ILOOP) = IVIS(2) * SCL
            VISOUT(3,ILOOP) = WT
C                                       Else, blanked, zero output
         ELSE
            VISOUT(1,ILOOP) = 0.0
            VISOUT(2,ILOOP) = 0.0
            VISOUT(3,ILOOP) = 0.0
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZUVXPN: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END

