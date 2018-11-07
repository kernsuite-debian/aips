@PROCESS VECTOR DIR('@DIR')
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
C   FORTRAN version of FPS AP VFC routine
C   QRFT computes a real, inverse fourier transform from data with an
C   arbitrary spacing.
C   Inputs:
C      UDATA  I  AP base address of input data.
C                expects 2*NDATA words after UDATA of work space
C      UFT    I  AP base address of output F. T.
C      UPH0   I  AP base address of phase information for F. T.
C                  0=COS((TWOPI/(NG*NFT))*(1-ICENT)(1-BIAS))
C                  1=SIN((TWOPI/(NG*NFT))*(1-ICENT)(1-BIAS))
C                  2=COS((TWOPI/(NG*NFT))*(1-ICENT))
C                  3=SIN((TWOPI/(NG*NFT))*(1-ICENT))
C                  4=COS((TWOPI/(NG*NFT))*(1-BIAS))
C                  5=SIN((TWOPI/(NG*NFT))*(1-BIAS))
C                  6=COS((TWOPI/(NG*NFT)))
C                  7=SIN((TWOPI/(NG*NFT)))
C                  ICENT = center pixel of grid
C                  BIAS = center of data array (1 rel)
C                  NG = No. tabulated points per cell.
C      NFT    I  Number of FT points
C      NDATA  I  Number of data points.
C    Fourier transform is calculated by repeatedly applying a phase ramp
C   to the complex input vector and summing the result.
C-----------------------------------------------------------------------
      INTEGER   UDATA, UFT, UPH0, FT, NFT, NDATA, DPH, DPH0, DDPH,
     *   I, WORK, LOOP, DATA, INDEX, JA, JB
      REAL   SUMRE, PHASE, AMPL, DPHASE, TEMPR, TEMPI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Set up addresses.
      FT = UFT + 1
C                                       Check for data.
      IF (NDATA.LE.0) GO TO 999
C                                       Set pointers.
      DPH = UPH0 + 2
      DPH0 = DPH + 2
      DDPH = DPH0 + 2
      WORK = UDATA + 2 * NDATA + 1
      DATA = UDATA + 1
C                                       Phase ramp
      AMPL = SQRT (APCORE(UPH0+1)*APCORE(UPH0+1) +
     *   APCORE(UPH0+2)*APCORE(UPH0+2))
      PHASE = ATAN2 (APCORE(UPH0+2), APCORE(UPH0+1)+1.0E-30)
      DPHASE = ATAN2 (APCORE(DPH+2), APCORE(DPH+1)+1.0E-30)
      INDEX = WORK
C@DIR IGNORE RECRDEPS(APCORE)
      DO 20 LOOP = 1,NDATA
         APCORE(INDEX) = AMPL
         APCORE(INDEX+1) = PHASE + (LOOP-1) * DPHASE
         INDEX = INDEX + 2
 20      CONTINUE
C                                       Convert to rectangular
      INDEX = WORK - 1
      CALL QRECT (INDEX, 2, INDEX, 2, NDATA)
C                                       Multiply by ramp
      JA = DATA
      JB = WORK
C@DIR IGNORE RECRDEPS(APCORE)
      DO 40 LOOP = 1,NDATA
         TEMPR = APCORE(JA)*APCORE(JB) - APCORE(JA+1)*APCORE(JB+1)
         TEMPI = APCORE(JA)*APCORE(JB+1) + APCORE(JA+1)*APCORE(JB)
         APCORE(JA) = TEMPR
         APCORE(JA+1) = TEMPI
C                                        Update indices.
         JA = JA + 2
         JB = JB + 2
 40      CONTINUE
C                                       Set phase ramp for inner loop
      AMPL = SQRT (APCORE(DPH0+1)*APCORE(DPH0+1) +
     *   APCORE(DPH0+2)*APCORE(DPH0+2))
      PHASE = ATAN2 (APCORE(DPH0+2), APCORE(DPH0+1)+1.0E-30)
      DPHASE = ATAN2 (APCORE(DDPH+2), APCORE(DDPH+1)+1.0E-30)
      INDEX = WORK
C@DIR IGNORE RECRDEPS(APCORE)
      DO 60 LOOP = 1,NDATA
         APCORE(INDEX) = AMPL
         APCORE(INDEX+1) = PHASE + (LOOP-1) * DPHASE
         INDEX = INDEX + 2
 60      CONTINUE
C                                       Convert to rectangular
      INDEX = WORK - 1
      CALL QRECT (INDEX, 2, INDEX, 2, NDATA)
C                                       Do Fourier transform.
C                                       Loop
      DO 200 I = 1,NFT
C                                       Multiply by ramp and sum
         JA = DATA
         JB = WORK
         SUMRE = 0.0
C@DIR IGNORE RECRDEPS(APCORE)
         DO 100 LOOP = 1,NDATA
            SUMRE = SUMRE + APCORE(JA)
            TEMPR = APCORE(JA)*APCORE(JB) - APCORE(JA+1)*APCORE(JB+1)
            TEMPI = APCORE(JA)*APCORE(JB+1) + APCORE(JA+1)*APCORE(JB)
            APCORE(JA) = TEMPR
            APCORE(JA+1) = TEMPI
C                                           Update indices.
            JA = JA + 2
            JB = JB + 2
 100        CONTINUE
         APCORE(FT) = SUMRE
         FT = FT + 1
 200     CONTINUE
C
 999  RETURN
      END
