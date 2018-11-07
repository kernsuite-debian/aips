@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QMENT (APBASE, DOGRDS, IMG, DEF, RES, STP, NELEM)
C-----------------------------------------------------------------------
C! Pseudo AP routine: MEM routine
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
C   Q-Routine for Maximum entropy programs. Calculates gradients
C   and their dot products.
C   Input :
C      APBASE   I  Base address in PS-AP, all others relative
C      DOGRDS   I  Do gradients ? If > 0 then yes else no.
C      IMG      I  Pointer in PS-AP to MEM image line
C      DEF      I  Pointer in PS-AP to default image line
C      RES      I  Pointer in PS-AP to residual image line
C      STP      I  Pointer in PS-AP to step image line
C      NELEM    I  Number of elements in a line
C   Input via AP :
C    Location        Variable
C      0            - ALPHA
C      1            - BETA
C      2              GGC
C      3              1.0
C  Input/Output via AP :
C   Location        Variable
C      4             GRDHSQ
C      5             GRDESQ
C      6             GRDFSQ
C      7             GRDHE
C      8             GRDHF
C      9             GRDEF
C     10             FLUX
C     11              RJ1
C    STP        step in mem image
C    All locations are relative to APBASE. Location 12 is used for
C scratch. Locations 13-16 are free for future use.
C Programmer : T.J. Cornwell                   January 1986
C-----------------------------------------------------------------------
      INTEGER   APBASE, ALP, BET, GGC, IMG, DEF, RES, STP, RH, WRK,
     *   RJ1, GRDS, NELEM, RONE
      INTEGER   GHS, GES, GFS, GHE, GHF, GEF, FLX, TMP, DOGRDS
C-----------------------------------------------------------------------
C                                      Pointers for variables
      ALP = APBASE
      BET = ALP + 1
      GGC = ALP + 2
      RONE = ALP + 3
C                                      Storage for gradients, etc
      GHS = ALP + 4
      GES = ALP + 5
      GFS = ALP + 6
      GHE = ALP + 7
      GHF = ALP + 8
      GEF = ALP + 9
      FLX = ALP + 10
      RJ1 = ALP + 11
      TMP = ALP + 12
C                                      Storage for work areas
      RH  = STP + NELEM
      WRK = RH  + NELEM
C
C            FLUX = FLUX + IMG
C
         CALL QSVE (IMG, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, FLX, 1, FLX, 1, 1)
C
C               RH = IMG / (1.0 + GGC*IMG)
C
         CALL QVSMSA (IMG, 1, GGC, RONE, RH, 1, NELEM)
         CALL QVDIV  (RH, 1, IMG, 1, RH, 1, NELEM)
C
C               GRADH = -LOG (IMG/DEF)
C
         CALL QVDIV (IMG, 1, DEF, 1, IMG, 1, NELEM)
         CALL QVLN  (IMG, 1, IMG, 1, NELEM)
C
C            GRADJ = GRADH - ALPHA*RES - BETA
C
         CALL QVSMA (RES, 1, ALP, IMG, 1, WRK, 1, NELEM)
         CALL QVSADD (WRK, 1, BET, WRK, 1, NELEM)
C
C            RJ1 = RJ1 + STP*GRADJ
C
         CALL QVMUL (STP, 1, WRK, 1, STP, 1, NELEM)
         CALL QSVE (STP, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, RJ1, 1, RJ1, 1, 1)
         IF (DOGRDS.LT.0) GO TO 999
         CALL QVMOV (WRK, 1, STP, 1, NELEM)
C
C            STP = GRADJ*RH
C
         CALL QVMUL (STP, 1, RH, 1, STP, 1, NELEM)
C
C               GFS = GFS + RH
C
         CALL QSVE (RH, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GFS, 1, GFS, 1, 1)
C
C               GHS = GHS + RH * GRADH**2
C
         CALL QVSQ (IMG, 1, WRK, 1, NELEM)
         CALL QVMUL (WRK, 1, RH, 1, WRK, 1, NELEM)
         CALL QSVE (WRK, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GHS, 1, GHS, 1, 1)
C
C               GES = GES + RH * GRADER**2
C
         CALL QVSQ (RES, 1, WRK, 1, NELEM)
         CALL QVMUL (WRK, 1, RH, 1, WRK, 1, NELEM)
         CALL QSVE (WRK, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GES, 1, GES, 1, 1)
C
C            GHE = GHE + RH * GRADER * GRADH
C
         CALL QVMUL (RES, 1, IMG, 1, WRK, 1, NELEM)
         CALL QVMUL (WRK, 1, RH, 1, WRK, 1, NELEM)
         CALL QSVE (WRK, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GHE, 1, GHE, 1, 1)
C
C            GEF = GEF + RH * GRADER
C
         CALL QVMUL (RES, 1, RH, 1, WRK, 1, NELEM)
         CALL QSVE (WRK, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GEF, 1, GEF, 1, 1)
C
C            GHF = GHF + RH * GRADH
C
         CALL QVMUL (IMG, 1, RH, 1, WRK, 1, NELEM)
         CALL QSVE (WRK, 1, TMP, NELEM)
         CALL QVADD (TMP, 1, GHF, 1, GHF, 1, 1)
C
  999    CONTINUE
         RETURN
         END
