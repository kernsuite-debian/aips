      SUBROUTINE SET1VS (MODE, NCH, JADR, SFACT, ALLWT, JINC, IRET)
C-----------------------------------------------------------------------
C! Sets up pointer and weights arrays for selecting uv data.
C# UV Calibration
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
C   SET1VS setup the arrays JADR, SFACT and the flag ALLWT for
C   reformatting uv data as specified by MODE.  One visibility per
C   frequency channel will be returned by GET1VS. There is also a check
C   to make sure the desired data is available.  Calls to GET1VS will
C   reformat the data.  Needs values set by UVPGET.
C   Inputs:
C      MODE         I    Desired output data format:
C                        1 => I
C                        2 => Q
C                        3 => U
C                        4 => V
C                        5 => RCP
C                        6 => LCP
C      NCH          I    First line channel desired.
C   Output:
C      JADR(2)    I    Pointers to the first and second visibility
C                        input records to be used in the output record.
C      SFACT(2)     R    Factors to be multiplied by the first and
C                        second input vis's to make the output vis.
C      ALLWT        L    If true no flagged data is allowed.
C      JINC         I    Visibility increment.
C      IRET         I    Error flag. 0 =>OK, otherwise data unavailable.
C-----------------------------------------------------------------------
      INTEGER   MODE, JADR(2), JINC, IRET, JNCS, JNCF, NCH
      LOGICAL   ALLWT
      REAL      SFACT(2)
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IRET = 1
      JINC = INCF / 3
      ALLWT = .TRUE.
      IF ((MODE.LE.0) .OR. (MODE.GT.6)) GO TO 999
C                                       Set pointers.
      JNCS = INCS / 3
      JNCF = INCF / 3
C                                       Branch by MODE
      GO TO (100, 200, 300, 400, 500, 600), MODE
C                                       IPOL wanted, Check data type.
C                                       True IPOL, check.
 100  IF (ICOR0.LE.0) GO TO 120
         IF (ICOR0.GT.1) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (1-ICOR0) * JNCS + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
C                                       RR, LL etc., check.
 120  IF (ICOR0.LT.-2) GO TO 999
         JADR(1) = (NCH-1) * JNCF + 1
         JADR(2) = JADR(1) + JNCS
         SFACT(1) = 0.5
         SFACT(2) = 0.5
         ALLWT = .FALSE.
C                                       If only RR or LL use it.
         IF ((NCOR.LT.2) .OR. (ICOR0.NE.-1)) JADR(2) = JADR(1)
         GO TO 900
C                                       Q  - Check data type.
C                                       True stokes. Check.
 200  IF (ICOR0.LT.0) GO TO 270
         IF ((ICOR0+NCOR-1).LT.2) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (2-ICOR0) * JNCS + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
C                                       RL, LR - Check.
 270  IF ((NCOR.LT.2) .OR. (ICOR0.LT.-2)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (3+ICOR0) * JNCS + 1
         JADR(2) = JADR(1) + JNCS
         SFACT(1) = 0.5
         SFACT(2) = 0.5
         GO TO 900
C                                       U wanted - Check data type.
C                                       True stokes. Check.
 300  IF (ICOR0.LT.0) GO TO 370
         IF ((ICOR0+NCOR-1).LT.3) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (3-ICOR0) * JNCS + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
C                                       RL, LR - Check.
 370  IF ((NCOR.LT.3) .OR. (ICOR0.LT.-2)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (3+ICOR0) * JNCS + 1
         JADR(2) = JADR(1) + JNCS
         SFACT(1) = -0.5
         SFACT(2) = 0.5
         GO TO 900
C                                       VPOL wanted.
C                                       Check type.
C                                       True VPOL. Check.
 400  IF (ICOR0.LT.0) GO TO 420
         IF ((NCOR+ICOR0-1).LT.4) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (4-ICOR0) * JNCS + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
C                                       RR, LL - Check.
 420  IF ((NCOR.LT.2) .OR. (ICOR0.LT.-1)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (1+ICOR0) * JNCS + 1
         JADR(2) = JADR(1) + JNCS
         SFACT(1) = 0.5
         SFACT(2) = -0.5
         GO TO 900
C                                       RPOL, check type.
C                                       True stokes - check.
 500  IF (ICOR0.LE.0) GO TO 570
         IF ((ICOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (1-ICOR0) * JNCS + 1
         JADR(2) = (NCH-1) * JNCF + (4-ICOR0) * JNCS + 1
         SFACT(1) = 1.0
         SFACT(2) = 1.0
         GO TO 900
C                                       RR - check.
 570  IF (ICOR0.LT.-1) GO TO 999
         JADR(1) = (NCH-1) * JNCF + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
C                                       LPOL, check type.
C                                       True stokes - check.
 600  IF (ICOR0.LE.0) GO TO 630
         IF ((ICOR0.GT.1) .OR. (NCOR.LT.4)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (1-ICOR0) * JNCS + 1
         JADR(2) = (NCH-1) * JNCF + (4-ICOR0) * JNCS + 1
         SFACT(1) = 1.0
         SFACT(2) = -1.0
         GO TO 900
C                                       RR - check.
 630     IF (ICOR0.LT.-2) GO TO 999
         IF ((ICOR0.EQ.-1) .AND. (NCOR.EQ.1)) GO TO 999
         JADR(1) = (NCH-1) * JNCF + (2+ICOR0) * JNCS + 1
         JADR(2) = JADR(1)
         SFACT(1) = 1.0
         SFACT(2) = 0.0
         GO TO 900
 900  IRET = 0
C
 999  RETURN
      END
