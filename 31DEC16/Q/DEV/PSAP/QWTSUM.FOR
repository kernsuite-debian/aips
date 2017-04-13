      SUBROUTINE QWTSUM (IU, IU1, IU2)
C-----------------------------------------------------------------------
C! Does Hermitian corrections & summing for Robust weighting
C# AP-appl Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999, 2006, 2012
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
C   Converts the summing arrays to include the Hermitian terms if needed
C   Does the summing for robust weighting - basically a sum over a count
C   array and a dot product of that count array with the weight sum
C   array.  Does one row at a time, depends on rows haveing odd number
C   of points.
C   Inputs:
C      IU       I      Desired row >= 0.  0 is last call.
C      IU1      I      Lowest row number in array
C      IU2      I      Highest row number in array
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
C      SUMS     R(2)   Summed on readback: # Vis, for avg cell weight
C      UV       R(2)   U and V in cells at channel 1
C      FCT      R(*)   * = NCH - factors to convert U, V to cells at
C                      channel n
C      WTS      R(*)   * = NCH - weights to be convolved
C      FUN      R(*)   * = (MU+1)*(MV+1) convolving function (up-right
C                      quadrant)
C      GRD      R(*)   * = NV * # rows - convolved weight grid
C      CNT      R(*)   * = NV * # rows - count of samples (optional)
C   APCORE - in/out
C      SUMS     R(2)   Summed on readback: # Vis, for avg cell weight
C-----------------------------------------------------------------------
      INTEGER   IU, IU1, IU2
C
      DOUBLE PRECISION WTS
      INTEGER*8 IC, ONE
      INTEGER   I, NV, NH
      LONGINT   JW, JW2, JC
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (IU.LT.0) GO TO 999
      NV = APCORI(1+PSAPOF)
      JW = APCORI(7+PSAPOF) + NV * (IU - IU1) + PSAPOF
      ONE = 1
C                                       Hermitian
      IF (IU.GE.IU1) THEN
         IF (IU.EQ.0) THEN
            NH = NV / 2
            JW2 = JW + NV - 1
            DO 10 I = 1,NH
               APCORE(JW) = APCORE(JW) + APCORE(JW2)
               APCORE(JW2) = APCORE(JW)
               JW = JW + 1
               JW2 = JW2 - 1
 10            CONTINUE
         ELSE IF (IU.LT.APCORI(2+PSAPOF)) THEN
            JW2 = APCORI(7+PSAPOF) + PSAPOF - NV * (IU + IU1) + NV - 1
            DO 20 I = 1,NV
               APCORE(JW) = APCORE(JW) + APCORE(JW2)
               JW = JW + 1
               JW2 = JW2 - 1
 20           CONTINUE
            END IF
C                                       counting turned on
         IF (APCORI(8+PSAPOF).GT.0) THEN
            JW = APCORI(7+PSAPOF) + NV * (IU - IU1) + PSAPOF - 1
            JC = APCORI(8+PSAPOF) + NV * (IU - IU1) + PSAPOF - 1
            IC = APCORI(APCORI(13+PSAPOF)+PSAPOF)
            WTS = APCORE(APCORI(13+PSAPOF)+1+PSAPOF)
            DO 30 I = 1,NV
               JW = JW + 1
               JC = JC + 1
               IC = IC + APCORI(JC)
               WTS = WTS + APCORI(JC) * APCORE(JW)
 30            CONTINUE
            APCORI(APCORI(13+PSAPOF)+PSAPOF) = IC
            APCORE(APCORI(13+PSAPOF)+1+PSAPOF) = WTS
            END IF
         END IF
C                                       row zero average
      IF ((IU.EQ.0) .AND. (APCORI(8+PSAPOF).GT.0)) THEN
         IC = APCORI(APCORI(13+PSAPOF)+PSAPOF)
         WTS = APCORE(APCORI(13+PSAPOF)+1+PSAPOF)
         APCORE(APCORI(13+PSAPOF)+1+PSAPOF) = WTS / MAX (ONE, IC)
         END IF
C
 999  RETURN
      END
