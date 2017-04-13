      SUBROUTINE APFNSH ( USTRT, USTOP, UZERO, FRSTU, MO2, ROWSIZ,
     *   LROW, LOGRID, APSIZE, THETA)
C-----------------------------------------------------------------------
C! AP routine: Phase rotates rows of GRIDDED UV data in the AP
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999
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
C AP FiNiSH: Finishes UV data Gridding Process, phase rotates image
C to center of GRID after FFT
C   Inputs:
C      USTRT   I    Start U row to process
C      USTOP   I    Stop  U row to process
C      UZERO   I    Row number of minimum row in AP to be processed
C                   (Often Location of row U=0 in AP)
C      FRSTU   I    U cell containing max U baseline
C      MO2     I    Interpolation half width (Must be Odd)
C      ROWSIZ  I    Length of a Row in AP, number of REALs
C      LROW    I    Number of Cells in a Row (real and imaginary pairs)
C      LOGRID  I    Index of start of rows in AP (0 relative)
C      APSIZE  I    Size of AP in REALs
C      THETA   DP   Angle to rotate U=1 cell to center
C   Outputs:
C      AP memory modified
C-----------------------------------------------------------------------
      INTEGER   USTRT, USTOP, UZERO, FRSTU, MO2, ROWSIZ, LROW, LOGRID,
     *          APSIZE
      DOUBLE PRECISION THETA
C
      DOUBLE PRECISION DTHETA
      REAL TRIG(2)
      INTEGER IU, U, EROW
C-----------------------------------------------------------------------
C                                       wait for ap to finish
      CALL QWAIT
C                                       for rows to write from AP
      DO 100 IU = USTRT, USTOP
C                                       convert to decreasing U
         U = USTOP - IU + USTRT
C                                       if below Max U
         IF ((U.LT.(FRSTU+MO2)) .AND. (U.GE.UZERO)) THEN
C                                       Do final operations on grid
            EROW = LOGRID + ((U-UZERO)*ROWSIZ)
C                                       rows above top of AP are zero
            IF (EROW+ROWSIZ .GE. APSIZE) EROW = APSIZE-ROWSIZ-1
C                                       calc phase to rotate U=0 to edge
            DTHETA =  U * THETA
C                                       get trig functions of angle
            TRIG(1) = DCOS(DTHETA)
            TRIG(2) = DSIN(DTHETA)
C                                       put phase in ap
            CALL QPUT (TRIG, 0, 2, 2)
            CALL QWAIT
C                                       phase rotate and flip UV grid
            CALL Q1FIN (U, EROW, MO2, LROW)
            CALL QWAIT
C                                       end if below top of grid used
            END IF
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
      END
