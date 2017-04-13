      SUBROUTINE QINIT (I1, I2, APNUM)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Initialize "AP".
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
C   Convex version.
C   Initialize the AP.  For the pseudo-AP the size of the arrays in
C   COMMON /APCORE/ are defined and the AP roller common is
C   initialized.
C   Inputs:
C      I1     I  Dummy
C      I2     I  Dummy
C   Outputs:
C      APNUM  I  AP number (Neg. to indicate virtual AP, ie. not
C                to be rolled.
C    Output to common /BPROLC/
C      TRUEAP L  True if a real AP (to be rolled) (FALSE)
C      XTLAST D  Real time AP assigned (min).
C      DELTIM D  Time interval between rolls (min).
C      DELAY  R  Time to delay task (seconds).
C-----------------------------------------------------------------------
      INTEGER   I1, I2, APNUM
C      INTEGER   GAMMA
      INCLUDE 'INCS:DDCH.INC'
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
C                                       Set time.
      XTLAST = 0.0
C                                       Set AP memory size.
      KAPWRD = PKPWRD
      KAP2WD = MIN (PKPWD2, KAP2WD)
      IF (KAP2WD.LE.0) KAP2WD = PKPWD2
C                                       Generate FFT lookup tables
C                                       big enough for 4096 FFT
C      GAMMA = 12
C      CALL PREFFT (GAMMA)
C
 999  RETURN
      END
