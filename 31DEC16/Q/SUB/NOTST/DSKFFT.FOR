      SUBROUTINE DSKFFT (NR, NC, IDIR, HERM, LI, LW, LO, JBUFSZ, BUFF1,
     *   BUFF2, SMAX, SMIN, IERR)
C-----------------------------------------------------------------------
C! 2-D disk based FFT using AP.
C# Map AP-fft
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
C   DSKFFT is a disk based, two dimensional FFT.  If the FFT all fits
C   in AP memory then the intermediate result is not written to disk.
C   Input or output images in the sky plane are in the usual form
C   (i.e. center at the center, X the first axis).  Input or output
C   images in the uv plane are transposed (v the first axis) and the
C   center-at-the-edges convention with the first element of the array
C   the center pixel.    NOTE: Uses AIPS LUNs 23, 24, 25.
C   Inputs:
C      NR      I     The number of rows in input array (# columns in
C                    output).  When HERM is TRUE and IDIR=-1, NR is
C                    twice the number of complex rows in the input file
C      NC      I     The number of columns in input array (# rows in
C                    output).
C      IDIR    I     1 for forward (+i) transform, -1 for inverse (-i)
C                    transform.
C                    If HERM = .TRUE. the follwing are recognized:
C                       IDIR=1 keep real part only.
C                       IDIR=2 keep amplitudes only.
C                       IDIR=3 keep full complex (half plane)
C      HERM    L     When HERM = .FALSE., this routine does a complex to
C                    complex transform.
C                    When HERM = .TRUE. and IDIR = -1, it does a
C                    complex to real transform.  When HERM = .TRUE. and
C                    IDIR = 1, it does real to complex.
C      LI      I     File number in /CFILES/ of input.
C      LW      I     File number in /CFILES/ of work file (may equal LI)
C      LO      I     File number in /CFILES/ of output.
C      JBUFSZ  I     Size of BUFF1, BUFF2 in bytes.  Should be large
C                    at least 4096 R   words.
C   Output:
C      BUFF1   R(*)  Working buffer
C      BUFF2   R(*)  Working buffer
C      SMAX    R     For HERM=.TRUE. the maximum value in output file.
C      SMIN    R     For HERM=.TRUE. the minimum value in output file.
C      IERR    I     Return error code, 0 => okay, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NR, NC, IDIR, LI, LW, LO, JBUFSZ, IERR
      LOGICAL   HERM
      REAL      BUFF1(*), BUFF2(*), SMAX, SMIN
C
      INTEGER   LUN(3), VOL(3), BO(3), MC, MR
      LOGICAL   FULL
      CHARACTER FIL(3)*48
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA BO /1,1,1/
      DATA LUN /23,24,25/
C-----------------------------------------------------------------------
C                                       Fill arrays for PASS1/PASS2
      FULL = .NOT. HERM
      VOL(1) = SCRVOL(LI)
      VOL(2) = SCRVOL(LW)
      VOL(3) = SCRVOL(LO)
      CALL ZPHFIL ('SC', VOL(1), SCRCNO(LI), 1, FIL(1), IERR)
      CALL ZPHFIL ('SC', VOL(2), SCRCNO(LW), 1, FIL(2), IERR)
      CALL ZPHFIL ('SC', VOL(3), SCRCNO(LO), 1, FIL(3), IERR)
      MC = NC
      MR = NR
      IF (FULL) MC = NR
      IF (FULL) MR = NC
C                                       Do FFT.
      CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, JBUFSZ,
     *   BUFF2, JBUFSZ, NR, NC, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, JBUFSZ,
     *   BUFF2, JBUFSZ, MR, MC, SMAX, SMIN, IERR)
C
 999  RETURN
      END
