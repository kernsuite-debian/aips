      SUBROUTINE QRFT (UDATA, UFT, UPH0, NFT, NDATA)
C-----------------------------------------------------------------------
C! FPS AP version: Real inverse Fourier transform with arbitrary spacing
C# AP-util
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
C   FPS AP version
C   Computes a real, inverse fourier transform from arbitarily spaced
C   data.
C    Inputs:
C     UDATA  I  AP base address of input data.
C     UFT    I  AP base address of output F. T.
C     UPH0   I  AP base address of phase information for F. T.
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
C     NFT    I  Number of FT points
C     NDATA  I  Number of data points.
C   Fourier transform is calculated by repeatedly applying a phase ramp
C   to the complex input vector and summing the result.
C-----------------------------------------------------------------------
      INTEGER UDATA, UFT, UPH0, NFT, NDATA
      INTEGER*2 IUDATA, IUFT, IUPH0, INFT, INDATA
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (UDATA.LT.32768) IUDATA = UDATA
      IF (UDATA.GE.32768) IUDATA = UDATA - 65536
      IF (UFT.LT.32768) IUFT = UFT
      IF (UFT.GE.32768) IUFT = UFT - 65536
      IF (UPH0.LT.32768) IUPH0 = UPH0
      IF (UPH0.GE.32768) IUPH0 = UPH0 - 65536
      IF (NFT.LT.32768) INFT = NFT
      IF (NFT.GE.32768) INFT = NFT - 65536
      IF (NDATA.LT.32768) INDATA = NDATA
      IF (NDATA.GE.32768) INDATA = NDATA - 65536
C                                        Call FPS routine.
      CALL APRFT (IUDATA, IUFT, IUPH0, INFT, INDATA)
C
 999  RETURN
      END
