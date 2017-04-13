      SUBROUTINE WTSHFT (WTS, NFRQ, DELI)
C-----------------------------------------------------------------------
C! Frequency shift weight spectra
C# UV IO-appl Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C  Routine to perform the frequency shift for spectral weights.
C  Routine will flag those channels which are shifted from one end
C  of the spectrum to the other.
C  Inputs:
C      WTS(*)        R        The weight spectrum to be
C                             shifted. Will also contain shifted
C                             result
C      NFRQ          I        No. freq. channels in spectrum
C      DELI          D        No. channels to shift spectrum
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      REAL    WTS(*)
      DOUBLE PRECISION DELI
      INTEGER   NFRQ, I, IROUND, MAXCH2, ISHFT, FRQOFF
      PARAMETER (MAXCH2=MAXCHA*2)
      REAL    WORK(MAXCH2), RDELI
      SAVE WORK
C-----------------------------------------------------------------------
      RDELI = DELI
      ISHFT = IROUND(RDELI)
      IF (ISHFT.EQ.0) GO TO 999
C                                       Shift data
C                                       positive shift
      IF (ISHFT.GT.0) THEN
         FRQOFF = NFRQ - ISHFT
         DO 100 I = 1, FRQOFF
            WORK(I+ISHFT) = WTS(I)
 100        CONTINUE
         DO 110 I = 1, ISHFT
            WORK(I) = -1.0
 110        CONTINUE
C                                       negative shift
      ELSE
         FRQOFF = NFRQ - ABS(ISHFT)
         DO 200 I = 1, FRQOFF
            WORK(I) = WTS(I-ISHFT)
 200        CONTINUE
         DO 210 I = 1, ISHFT
            WORK(FRQOFF+I) = -1.0
 210        CONTINUE
         END IF
C                                       Copy weight back to
C                                       WTS array
      CALL RCOPY (NFRQ, WORK, WTS)
C
 999  RETURN
      END
