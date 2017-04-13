      SUBROUTINE QWTGRD (IU1, IU2, NVIS, LREC)
C-----------------------------------------------------------------------
C! Sums positive weights convolved by a function into array
C# AP-appl Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Sums positive weights convolved by a function into array
C   Inputs:
C      IU1      I      Lowest row number in array
C      IU2      I      Highest row number in array
C      NVIS     I      Number of Vis samples
C      LREC     I      Number words between U,V pairs, vis samples
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
C   APCORE - in/out
C      GRD      R(*)   * = NV * # rows - convolved weight grid
C      CNT      R(*)   * = NV * # rows - count of samples (optional)
C-----------------------------------------------------------------------
      INTEGER   IU1, IU2, NVIS, LREC
C
      DOUBLE PRECISION WT, WTS
      INTEGER   UU, VV, I, J, ICH, NCH, MU, MV, NV, IVIS, IVS
      LONGINT   IIVS, IUU, INDEX, II, IC, IG1, IG2, IG3, IG4, IC0
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      NCH = APCORI(4+PSAPOF)
      NV = APCORI(1+PSAPOF)
      MU = APCORI(2+PSAPOF)
      MV = APCORI(3+PSAPOF)
      IUU = APCORI(11+PSAPOF) + PSAPOF - 1
      IVS = APCORI(12+PSAPOF)
      DO 100 IVIS = 1,NVIS
         IIVS = IVS + PSAPOF
         DO 90 ICH = 1,NCH
C                                       valid weight
            WT = APCORE(IIVS)
            IIVS = IIVS + 3
            IF (WT.LE.0.0) GO TO 90
C                                       cell for this channel
            II = APCORI(10+PSAPOF) + PSAPOF - 1
            UU = APCORE(IUU+1) * APCORE(II+ICH) +
     *         0.5 + APCORI(5+PSAPOF)
            VV = APCORE(IUU+2) * APCORE(II+ICH) +
     *         0.5 + APCORI(6+PSAPOF)
            IF ((VV.LT.1+MV) .OR. (UU.LT.IU1+MU)) GO TO 90
            IF ((VV.GT.NV-MV) .OR. (UU.GT.IU2-MU)) GO TO 90
C                                       count
            INDEX = NV * (UU - IU1) + VV + PSAPOF
            IF (APCORI(8+PSAPOF).GE.0) THEN
               II = INDEX + APCORI(8+PSAPOF)
               APCORI(II) = APCORI(II) + 1
               END IF
C                                       grid:
            INDEX = APCORI(7+PSAPOF) + INDEX
            IC = APCORI(9+PSAPOF) + PSAPOF
C                                       center point
            APCORE(INDEX) = APCORE(INDEX) + WT * APCORE(IC)
C                                       center row
            IF (MV.GT.0) THEN
               IG2 = INDEX - 1
               IG1 = INDEX + 1
               IC = IC + 1
               DO 20 I = 1,MV
                  APCORE(IG1) = APCORE(IG1) + WT * APCORE(IC)
                  APCORE(IG2) = APCORE(IG2) + WT * APCORE(IC)
                  IG1 = IG1 + 1
                  IG2 = IG2 - 1
                  IC = IC + 1
 20               CONTINUE
               END IF
C                                       center column
            IF (MU.GT.0) THEN
               IG1 = INDEX + NV
               IG2 = INDEX - NV
               IC = APCORI(9+PSAPOF) + 1 + MV + PSAPOF
               DO 30 J = 1,MU
                  APCORE(IG1) = APCORE(IG1) + WT * APCORE(IC)
                  APCORE(IG2) = APCORE(IG2) + WT * APCORE(IC)
                  IG1 = IG1 + NV
                  IG2 = IG2 - NV
                  IC = IC + MV + 1
 30               CONTINUE
               END IF
C                                       rest of conv function
            IF ((MU.GT.0) .AND. (MV.GT.0)) THEN
               IC0 = APCORI(9+PSAPOF) + 1 + PSAPOF
               DO 50 J = 1,MU
                  IC = IC0 + J * (1 + MV)
                  IG1 = INDEX + J * NV + 1
                  IG2 = INDEX + J * NV - 1
                  IG3 = INDEX - J * NV - 1
                  IG4 = INDEX - J * NV + 1
                  DO 40 I = 1,MV
                     WTS = WT * APCORE(IC)
                     APCORE(IG1) = APCORE(IG1) + WTS
                     APCORE(IG2) = APCORE(IG2) + WTS
                     APCORE(IG3) = APCORE(IG3) + WTS
                     APCORE(IG4) = APCORE(IG4) + WTS
                     IG1 = IG1 + 1
                     IG2 = IG2 - 1
                     IG3 = IG3 - 1
                     IG4 = IG4 + 1
                     IC = IC + 1
 40                 CONTINUE
 50               CONTINUE
               END IF
 90        CONTINUE
        IUU = IUU + LREC
        IVS = IVS + LREC
 100    CONTINUE
C
 999  RETURN
      END



