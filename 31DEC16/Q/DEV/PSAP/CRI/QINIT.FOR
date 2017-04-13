      SUBROUTINE QINIT (I1, I2, APNUM)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Initialize "AP".
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
C   Initialize the AP.  For the pseudo-AP the size of the arrays in
C   COMMON /APCORE/ are defined and the AP roller common is
C   initialized. FFT values are cleared
C   Inputs:
C     I1  I    Dummy
C     I2  I    Dummy
C   Outputs:
C     I3  I    APNUM (AP number) (Neg. to indicate virtual AP, ie. not
C             to be rolled.
C    Output to common /BPROLC/
C            TRUEAP   L    True if a real AP (to be rolled) (FALSE)
C            XTLAST   D    Real time AP assigned (min).
C            DELTIM   D    Time interval between rolls (min).
C            DELAY    R    Time to delay task (seconds).
C     Output to Common (Includes DAPC, CAPC, and EAPC)
C            FFTDIR   I    Direction of FFT, used to determine if
C                          WRKFFT needs to be initilized.
C            FFTSZE   I    Size of FFT, used to determine if WRKFFT
C                          needs to be initilized.
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   I1, I2, APNUM
      INTEGER   NWORDS
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DBPR.INC'
C-----------------------------------------------------------------------
C                                       Mark as non-rollable AP
      APNUM = -1
C                                       Fill values into /BPROLC/
      TRUEAP = .FALSE.
C                                       Delay time = 10 sec.
      DELAY = 10.0
C                                       Time between rolls = 5 min.
      DELTIM = 5.0D0
C                                       Set blank common size for
C                                       AP memory and work vectors.
C                                       APCORE and WRKFFT only.
C      NWORDS = OFF2
      NWORDS = OFF2 + 12 * OFFDEL
      CALL QMEMSZ (NWORDS)
C                                       Clear FFT values
      FFTDIR = 0
      FFTSZE = 0
C
 999  RETURN
      END
