      SUBROUTINE DATUVR (RPARM, VIS, DROP, IERR)
C-----------------------------------------------------------------------
C! Flags data based on UVRANGE
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   Flags data on UVRANGE
C   Inputs:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C   Output:
C      RPARM(*)   R    Random parameter array
C      VIS(3,*)   R    Visibility array
C      DROP       L    True if data all flagged.
C      IERR       I    Return code, 0=OK, else NXTFLG error number.
C-----------------------------------------------------------------------
      REAL      RPARM(*), VIS(3,*)
      LOGICAL   DROP
      INTEGER   IERR
C
      INTEGER   JIF, JCHAN, JPOLN, INDEX, STADD, NCHAN
      DOUBLE PRECISION UVWSCL, RR, UU0, VV0, RR0
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DCHND.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      DROP = .TRUE.
      UU0 = RPARM(1+ILOCU)
      VV0 = RPARM(1+ILOCV)
      RR0 = UU0 * UU0 + VV0 * VV0
      NCHAN = CATUV(KINAX+KLOCFY)
      DO 100 JIF = 1,CHNIF
         UVWSCL = 1.0D0 + FOFF(JIF) / UVFREQ
         UVWSCL = UVWSCL * UVWSCL
         RR = RR0 * UVWSCL
         IF ((RR.LT.UVRA(1)) .OR. (RR.GT.UVRA(2))) THEN
C                                       Loop over polarizations
            DO 40 JPOLN = 1,KNCOR
               STADD = (JPOLN-1) * KNCS + 1
               INDEX = STADD + (JIF-1) * KNCIF
C                                       Loop over channel
               DO 20 JCHAN = 1,NCHAN
C                                       Flag
                  VIS(3,INDEX) = - ABS (VIS(3,INDEX))
                  INDEX = INDEX + KNCF
 20               CONTINUE
 40            CONTINUE
         ELSE
            DROP = .FALSE.
            END IF
 100     CONTINUE
C
 999  RETURN
      END
