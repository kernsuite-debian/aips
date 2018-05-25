      SUBROUTINE QRFT (UDATA, UFT, UPH0, NFT, NDATA)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Does real, inverse FT with arbitrary spacing.
C# AP-appl
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
C   QRFT computes a real, inverse fourier transform from arbitarily
C   spaced data.
C   Inputs:
C      UDATA  I    AP base address of input data.
C      UFT    I    AP base address of output F. T.
C      UPH0   I    AP base address of phase information for F. T.
C                   0=COS((TWOPI/(NG*NFT))*(1-ICENT)(1-BIAS))
C                   1=SIN((TWOPI/(NG*NFT))*(1-ICENT)(1-BIAS))
C                   2=COS((TWOPI/(NG*NFT))*(1-ICENT))
C                   3=SIN((TWOPI/(NG*NFT))*(1-ICENT))
C                   4=COS((TWOPI/(NG*NFT))*(1-BIAS))
C                   5=SIN((TWOPI/(NG*NFT))*(1-BIAS))
C                   6=COS((TWOPI/(NG*NFT)))
C                   7=SIN((TWOPI/(NG*NFT)))
C                   ICENT = center pixel of grid
C                   BIAS = center of data array (1 rel)
C                   NG = No. tabulated points per cell.
C      NFT    I    Number of FT points
C      NDATA  I    Number of data points.
C   Fourier transform is calculated by repeatedly applying a phase ramp
C   to the complex input vector and summing the result.
C   Usage note:  This routine uses work vector WKVECA
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   UDATA, UFT, UPH0, FT, NFT, NDATA, DPH, DPH0, DDPH, M2,
     *   I, JFT, LOOP, JA, JC
      REAL     TEMP, TEMPR, TEMPI, SSUM, PHASE, DPHASE, TWOPI
      INCLUDE 'INCS:DAPC.INC'
      DATA  M2 /2/
      DATA TWOPI /6.283185308/
C-----------------------------------------------------------------------
C                                       Set up addresses.
      FT = UFT
C                                       Check for data.
      IF (NDATA.LE.0) GO TO 999
C                                       Set pointers.
      DPH = UPH0 + 2
      DPH0 = DPH + 2
      DDPH = DPH0 + 2
C                                       Setup row
      CALL QPHSRO (UDATA, M2, UDATA, M2, UPH0, DPH, NDATA)
C                                       Get phase and delta phase.
      PHASE = ATAN2 (APCORE(DPH0+2), APCORE(DPH0+1)+1.0E-30)
      DPHASE = ATAN2 (APCORE(DDPH+2), APCORE(DDPH+1)+1.0E-30)
C                                       Make phase ramp
      JA = 1
      DO 20 LOOP = 1,NDATA
         TEMP = MOD ((PHASE + (LOOP-1) * DPHASE), TWOPI)
         WKVECA(JA+1) = SIN (TEMP)
         WKVECA(JA) = COS (TEMP)
         JA = JA + 2
 20      CONTINUE
C                                       Do Fourier transform.
C                                       Loop
      JFT = FT + 1
      DO 100 I = 1,NFT
C                                       Sum
         APCORE(JFT) = SSUM (NDATA, APCORE(UDATA+1), M2)
C                                       Rotate for next sum.
         JA = UDATA + 1
         JC = 1
C                                        (complex) multiply
      INCLUDE 'INCS:ZVND.INC'
         DO 50 LOOP = 1,NDATA
            TEMPR = APCORE(JA)*WKVECA(JC) - APCORE(JA+1)*WKVECA(JC+1)
            TEMPI = APCORE(JA)*WKVECA(JC+1) + APCORE(JA+1)*WKVECA(JC)
            APCORE(JA) = TEMPR
            APCORE(JA+1) = TEMPI
C                                        Update indices.
            JA = JA + 2
            JC = JC + 2
 50         CONTINUE
C                                       Update pointer
         JFT = JFT + 1
 100     CONTINUE
C
 999  RETURN
      END
