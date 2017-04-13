      SUBROUTINE ZUVPAK (NCORR, VISIN, WTSCL, VISOUT)
C-----------------------------------------------------------------------
C! Pack visibility data, 1 correlator per real with magic value blank.
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
C   Routine to pack uv data with magic value blanking.  One AIPS
C   logical uv data record is processed at a time.
C   Inputs:
C      NCORR  I       Number of correlator values in data
C      VISIN  R(3,*)  Unpacked uv data as real, imag and weight per
C                     correlator.
C   Output:
C      WTSCL  R(2)    "Weight" and "scale" random parameters for the
C                     packed record.
C      VISOUT R(*)    Packed visibility data with local magic value
C                     blanking.
C   Version for machines with 32-bit integers/reals
C-----------------------------------------------------------------------
      INTEGER   NCORR
      REAL      VISIN(3,*), WTSCL(2), VISOUT(*)
C
      INTEGER   LOOP, LIMIT, IBLANK, IVIS(2), NP
      REAL      MAXVIS, MAXWT, SCL, XR, XI
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       Magic blanking value
      DATA IBLANK /-32767/
C-----------------------------------------------------------------------
      IF (NCORR.LE.0) GO TO 999
C                                       Validity check
      IF (NBITWD.NE.32) THEN
         WRITE (MSGTXT,1000) NBITWD
         CALL MSGWRT (9)
         STOP ' ZUVPAK MISSING'
         END IF
      LIMIT = NCORR
C                                       Find max wt, vis
      MAXWT = 0.0
      MAXVIS = 0.0
      DO 100 LOOP = 1,LIMIT
         IF (VISIN(3,LOOP).GT.0.0) THEN
            MAXVIS = MAX (MAXVIS, ABS (VISIN(1,LOOP)),
     *         ABS (VISIN(2,LOOP)))
            MAXWT = MAX (MAXWT, VISIN(3,LOOP))
            END IF
 100     CONTINUE
C                                       Set weight and scaling
C                                       parameters
      WTSCL(1) = MAXWT
      WTSCL(2) = MAXVIS / 32760.
      SCL = 1.0
      IF (WTSCL(2).GT.1.0E-10) SCL = 1.0 / WTSCL(2)
C                                       Scale to integer
      NP = 1
      DO 200 LOOP = 1,LIMIT
         IF (VISIN(3,LOOP).GT.0.0) THEN
            XR = VISIN(1,LOOP) * SCL
            XI = VISIN(2,LOOP) * SCL
            IVIS(1) = XR + SIGN (0.5, XR)
            IVIS(2) = XI + SIGN (0.5, XI)
         ELSE
            IVIS(1) = IBLANK
            IVIS(2) = IBLANK
            END IF
         CALL ZIPACK ('LTOS', 2, IVIS, NP, VISOUT)
         NP = NP + 2
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZUVPAK: REQUIRES LOCAL DEVELOPMENT FOR NBITWD =',I7)
      END

