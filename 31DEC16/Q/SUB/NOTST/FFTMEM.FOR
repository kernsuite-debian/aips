      SUBROUTINE FFTMEM (GRID, NX, NY, NROWFT, IRET)
C-----------------------------------------------------------------------
C! In memory 2-D FFT
C# AP-util FFT
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
C   Does an inplace 2-D half plane complex to real FFT in AP memory.  
C   The outer NX/2+1-NROWFT "Y" FFTs may be omitted if these are all
C   zeroes.
C   Input is a uv grid as produced by GRDMEM and output is an image as
C   expected by GDCMEM.
C   This routine expects the center pixel to be NX/2, NY/2+1
C   Input:
C      GRID   I "AP" base address of grid
C      NX     I Number of rows in output image
C      NY     I Number of columns in output image
C      NROWFT I Number of "Y" FFTs.  0=> NX/2+1
C   Output:
C      IRET   I Return code. 0=>OK else transpose error.
C-----------------------------------------------------------------------
      INTEGER GRID, NX, NY, NROWFT, IRET
C
      INTEGER NFT, LOOP, IAP, LROW, FIRST, LAST, IAP2, HAFNX, HIGH, LOW
C-----------------------------------------------------------------------
C                                       How many Y rows to transform
      NFT = NROWFT
      IF (NFT.LE.0) NFT = NX/2 + 1
C                                       Y transforms.
      IAP = GRID
      LROW = 2 * NY
      DO 100 LOOP = 1,NFT
         CALL QCFFT (IAP, NY, -1)
         IAP = IAP + LROW
 100    CONTINUE
C                                       Pack first row real and last row
C                                       imaginary. 
      FIRST = GRID + 1
      LAST = FIRST + (NX/2) * NY * 2
      CALL QVMOV (LAST, 2, FIRST, 2, NY/2)
C                                       Transpose
      CALL APXPOS (NX/2, NY, GRID, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       X transforms.
      IAP = GRID
      LROW = NX
      DO 200 LOOP = 1,NY
         CALL QRFFT (IAP, NX, -1)
         IAP = IAP + LROW
 200     CONTINUE
C                                       Put center at center
C                                       Swap columns
      IAP = GRID
      LROW = NX
      HAFNX = NX/2
      IAP2 = IAP + HAFNX
      DO 300 LOOP = 1,NY
         CALL QVSWAP (IAP, 1, IAP2, 1, HAFNX)
         IAP = IAP + LROW
         IAP2 = IAP2 + LROW
 300     CONTINUE
C                                       Swap top and bottom
      LROW = NX
      LOW = GRID
      HIGH = LOW + (NY/2) * LROW
      DO 400 LOOP = 1,NY/2
         CALL QVSWAP (LOW, 1, HIGH, 1, LROW)
         LOW = LOW + LROW
         HIGH = HIGH + LROW
 400     CONTINUE
C
 999  RETURN
      END
