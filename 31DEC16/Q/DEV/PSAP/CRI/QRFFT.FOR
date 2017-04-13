      SUBROUTINE QRFFT (C, N, F)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Real-half plane complex FFT
C# AP-FFT
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
C   Does an in-place real-to-complex forward or complex-to-real inverse
C   Inputs:
C      C  I    Base address of source and destiantion vector
C      N  I    Real element count (power of 2)
C      F  I    flag, 1=>forward FFT, -1=> reverse FFT.
C   Input/Output in Common (Includes PAPC, DAPC)
C      WRKFFT(*)  R    Table used in FFT
C      FFTDIR     I    Direction of FFT, used to determine if
C                      WRKFFT needs to be initilized.
C      FFTSZE     I    Size of FFT, used to determine if WRKFFT
C                      needs to be initilized.
C   Usage notes:
C      The array WRKFFT is used to contain the tables necessary for
C   FFTs.  QRFFT initilizes this table if F .NE. FFTDIR or N .NE.
C   FFTSZE.
C      This routine uses work vector WKVECB
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   C, N, F, IF, LOOP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
      IF = -F
C                                       Complex to real
      IF (F.LE.0) THEN
C                                       Init table.
         IF ((F.NE.FFTDIR) .OR. (N.NE.FFTSZE)) THEN
            CALL CRFFT2 (1, IF, N, WKVECB, WRKFFT, WKVECB)
            FFTDIR = F
            FFTSZE = N
            END IF
C                                       Unpack
      INCLUDE 'INCS:ZVND.INC'
 10      DO 20 LOOP = 1,N
            WKVECB(LOOP) = APCORE(C+LOOP)
 20         CONTINUE
         WKVECB(N+2) = 0.0
         WKVECB(N+1) = WKVECB(2)
         WKVECB(2) = 0.0
         CALL CRFFT2 (0, IF, N, WKVECB, WRKFFT, APCORE(C+1))
C                                       Real to complex
      ELSE
C                                       Init table.
         IF ((F.NE.FFTDIR) .OR. (N.NE.FFTSZE)) THEN
            CALL RCFFT2 (1, IF, N, WKVECB, WRKFFT, WKVECB)
            FFTDIR = F
            FFTSZE = N
            END IF
         CALL RCFFT2 (0, IF, N, APCORE(C+1), WRKFFT, WKVECB)
C                                       Pack
         WKVECB(2) = WKVECB(N+1)
      INCLUDE 'INCS:ZVND.INC'
         DO 120 LOOP = 1,N
            APCORE(C+LOOP) = WKVECB(LOOP)
 120        CONTINUE
         END IF
C
 999  RETURN
      END
