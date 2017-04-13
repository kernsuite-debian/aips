      SUBROUTINE ANTFND (ANTENS, NANTSL, DOAWNT)
C-----------------------------------------------------------------------
C! Counts selected antennas and determines if they are selected or not
C# EXT-appl Calibration UV
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
C   Counts selected antennas and determines if they are selected or not.
C   Inputs
C      ANTENS  I(*)  List of antennas selected, 0=>all,
C                    any negative => all except those specified
C                    VALUES NO LONGER CHANGED.
C   Output:
C      NANTSL   I    Number of antennas selected/excluded in ANTENS
C                    0 = All included.
C      DOAWNT   L    If .TRUE. then antennas in ANTENS included.
C                    If .FALSE. then excluded.
C-----------------------------------------------------------------------
      INTEGER   ANTENS(*), NANTSL
      LOGICAL   DOAWNT
C

      INCLUDE 'INCS:PUVD.INC'
      INTEGER LOOP, LOOP2
      LOGICAL T, F
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 30 LOOP = 1,MAXANT
         IF (ANTENS(LOOP).LT.0) DOAWNT = F
C                                       Redundancy check
         IF (NANTSL.GE.1) THEN
            DO 10 LOOP2 = 1,NANTSL
               IF (ABS(ANTENS(LOOP)).EQ.ABS(ANTENS(LOOP2)))
     *            ANTENS(LOOP) = 0
 10            CONTINUE
            END IF
        IF (ABS (ANTENS(LOOP)).GE.1) NANTSL = LOOP
 30     CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
C
 999  RETURN
      END
