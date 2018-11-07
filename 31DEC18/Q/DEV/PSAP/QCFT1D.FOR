      SUBROUTINE QCFT1D (SIGN, LEN, ARRAY, WSAVE)
C-----------------------------------------------------------------------
C! Pseudo AP routine: complex-to-complex 1D FFT
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2006
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
C   Replace the first LEN elements by either their unnormalized
C   discrete Fourier transform
C
C   for all j = 1 .. LEN
C      ARRAY'(j) = sum over k = 1 .. LEN of
C         ARRAY(j) * exp (SIGN * -i * (j-1) * (k-1) * 2 * pi / LEN)
C
C   where SIGN is +1 or -1.  Note that SIGN = +1 gives the Fourier
C   transform while SIGN = -1 gives an inverse Fourier transform.
C   The results of this routine are undefined if SIGN has any other
C   value.  Calling QCFT1D with SIGN = +1 and then with SIGN = -1
C   on has the effect of multiplying each element of the array by
C   LEN.
C
C   WSAVE must have been initialized for the specified value of LEN
C   (which must be positive) by calling QCFT1I and should not have
C   been modified.
C
C   The combination of QCFT1I and QCFT1D is should be used in
C   preference to the older routine QXFOUR for the following
C   reasons
C
C   1) QCFT1D may be used for any length of Fourier transform while
C      QXFOUR is limited to powers of two and has a fixed maximum
C      size.
C   2) The default implementation of QCFT1D is approximately
C      twice as fast as QXFOUR in most cases.  Optimized versions
C      for specific machines may be 3 to 4 times faster than
C      QXFOUR.
C
C   Input:
C      SIGN       I          Direction of transform
C      LEN        I          The length of the FFT
C      WSAVE      R(*)       Initialized workspace (from QCFT1I)
C
C   Input/Output:
C      ARRAY      C(*)       Array to be transformed: replaced by its
C                            transform
C-----------------------------------------------------------------------
      INTEGER   SIGN, LEN
      COMPLEX   ARRAY(*)
      REAL      WSAVE(*)
C-----------------------------------------------------------------------
C                                       Default version uses FFTPACK
      IF (SIGN.EQ.1) THEN
         CALL CFFTF (LEN, ARRAY, WSAVE)
      ELSE
         CALL CFFTB (LEN, ARRAY, WSAVE)
         END IF
C
 999  RETURN
      END
