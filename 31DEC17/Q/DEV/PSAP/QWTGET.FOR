      SUBROUTINE QWTGET (IU, IU1, IU2, ROW)
C-----------------------------------------------------------------------
C! Returns a row of the weight summing array
C# AP-appl Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Pseudo-AP version
C   Returns a row of the weight summing array.
C   Inputs:
C      IU       I      Desired row
C      IU1      I      Lowest row number in array
C      IU2      I      Highest row number in array
C   Output:
C      ROW      R(*)   Weight row size NX
C   APCORE on input - fixed address in ()
C      (0)      I      NU - number rows in array (# U values)
C      (1)      I      NV - number points in row of array (# V values)
C      (2)      I      MU - U-direction conv. function radius
C      (3)      I      MV - V-direction conv. function radius
C      (4)      I      NCH - Number spectral channels
C      (5)      I      U0 - row number for U = 0
C      (6)      I      V0 - column number of V = 0
C      (7)      I      GRDADD - Address of array for convolved weights
C      (8)      I      CNTADD - Address of array for counting samples
C                               -1 => none
C      (9)      I      FUNADD - Address of conv function array
C      (10)     I      FCTADD - Address of uv factors for channels
C      (11)     I      UVADDR - Address of uv value pair
C      (12)     I      WTADDR - Address of weights - each channel is
C                               separated by 3 words, next vis by LREC
C      (13)     I      SUMADD - Address of summing counters
C      UV       R(2)   U and V in cells at channel 1
C      FCT      R(*)   * = NCH - factors to convert U, V to cells at
C                      channel n
C      WTS      R(*)   * = NCH - weights to be convolved
C      FUN      R(*)   * = (MU+1)*(MV+1) convolving function (up-right
C                      quadrant)
C      GRD      R(*)   * = NV * # rows - convolved weight grid
C      CNT      R(*)   * = NV * # rows - count of samples (optional)
C      SUMS     R(2)   Summed on readback: # Vis, for avg cell weight
C-----------------------------------------------------------------------
      INTEGER   IU, IU1, IU2
      REAL      ROW(*)
C
      INTEGER   INDEX, I, NV
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      NV = APCORI(1+PSAPOF)
C                                       bad row
      IF ((IU.LT.IU1) .OR. (IU.GT.IU2)) THEN
         DO 10 I = 1,NV
            ROW(I) = 0.0
 10         CONTINUE
C                                       move row
      ELSE
         INDEX = APCORI(7+PSAPOF) + NV * (IU - IU1) + PSAPOF - 1
         DO 20 I = 1,NV
            INDEX = INDEX + 1
            ROW(I) = APCORE(INDEX)
 20         CONTINUE
         END IF
C
 999  RETURN
      END
