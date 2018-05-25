      SUBROUTINE APCONV (NX, NY, LI, LW1, LW2, LO, LC, FACTOR,
     *   JBUFSZ, BUFF1, BUFF2, BUFF3, SMAX, SMIN, IERR)
C-----------------------------------------------------------------------
C! Disk based 2-D convolution using FFTs.
C# AP-appl Map
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
C   APCONV is a disk based, two dimensional convolution routine.
C   The image to be convolved and the FFT of the convolving function
C   are passed to APCONV along with two scratch files.
C   NOTE: Uses AIPS LUNs 18, 23, 24, 25.
C   Inputs:
C      NX       I     The number of columns in the input image (must be
C                     a power of 2).
C      NY       I     The number of rows in the input image.
C      LI       I     File number in /CFILES/ of input.
C      LW1      I     File number in /CFILES/ of work file no. 1
C                        size = (4*NX x NY+2).
C      LW2      I     File number in /CFILES/ of work file no. 1
C                        size = (4*NX x NY+2).
C      LO       I     File number in /CFILES/ of output.
C      LC       I     File number in /CFILES/ of FFT of convolving fn.
C                        size = (4*NX x NY+2).
C      FACTOR   R     Normalization factor for convolving function; i.e.
C                     is multiplied by the transform of the convolving
C                     function
C      JBUFSZ   I     Size of BUFF1,2,3 in AIPS bytes.  Should be
C                     large, at least 8192 words.
C   Output:
C      BUFF1    R(*)  Working buffer
C      BUFF2    R(*)  Working buffer
C      BUFF3    R(*)  Working buffer
C      SMAX     R     Maximum value in the output file.
C      SMIN     R     Minimum value in the output file.
C      IERR     I     Return error code, 0 => OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, LI, LW1, LW2, LO, LC, JBUFSZ, IERR
      REAL      FACTOR, BUFF1(*), BUFF2(*), BUFF3(*), SMAX, SMIN
C
      CHARACTER FIL(5)*48
      INTEGER   LUN(5), VOL(5), BO(5)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA BO /5*1/
      DATA LUN /23,24,25,23,18/
C-----------------------------------------------------------------------
C                                       Fill arrays for CONVn
      VOL(1) = SCRVOL(LI)
      VOL(2) = SCRVOL(LW1)
      VOL(3) = SCRVOL(LW2)
      VOL(4) = SCRVOL(LO)
      VOL(5) = SCRVOL(LC)
      CALL ZPHFIL ('SC', VOL(1), SCRCNO(LI), 1, FIL(1), IERR)
      CALL ZPHFIL ('SC', VOL(2), SCRCNO(LW1), 1, FIL(2), IERR)
      CALL ZPHFIL ('SC', VOL(3), SCRCNO(LW2), 1, FIL(3), IERR)
      CALL ZPHFIL ('SC', VOL(4), SCRCNO(LO), 1, FIL(4), IERR)
      CALL ZPHFIL ('SC', VOL(5), SCRCNO(LC), 1, FIL(5), IERR)
C                                       Do convolution
      CALL CONV1 (LUN, VOL, FIL, BO, BUFF1, JBUFSZ, BUFF2, JBUFSZ,
     *   NX, NY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CONV2 (LUN, VOL, FIL, BO, BUFF1, JBUFSZ, BUFF2, JBUFSZ,
     *   BUFF3, JBUFSZ, NX, NY, FACTOR, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CONV3 (LUN, VOL, FIL, BO, BUFF1, JBUFSZ, BUFF2, JBUFSZ,
     *   NX, NY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CONV4 (LUN, VOL, FIL, BO, BUFF1, JBUFSZ, BUFF2, JBUFSZ,
     *   NX, NY, SMAX, SMIN, IERR)
C
 999  RETURN
      END
