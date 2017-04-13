      SUBROUTINE DLINTR (RP, IEV, PP, IT, DOIT)
C-----------------------------------------------------------------------
C! interactive delays, cursor tests
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2015
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
C   DLINTR is called by interactive routines to delay the task when
C   nothing is happening (i.e. the user is thinking or out to lunch.)
C   It also USED TO prevent cursor wrap around, BUT NO MORE.
C   Inputs:
C      IEV    I      not = 0  => event has occurred
C      DOCOR  L      Scroll correction parameter for YCURSE
C      QUAD   I      quadrant parameter for YCURSE
C   In/out:
C      RP     R(2)   cursor position read (fixed on wraps)
C      PP     R(2)   previous cursor position
C      IT     I(3)   time of last action
C   Output:
C      DOIT   L      T => something has happened.
C-----------------------------------------------------------------------
      INTEGER   IEV, IT(3)
      LOGICAL   DOIT
      REAL      RP(2), PP(2)
C
      REAL      DTIME, SIXTY, DELFAC
      INTEGER   LT(3), IERR
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA SIXTY / 60.0/
C-----------------------------------------------------------------------
C                                       did anything happen after all
      DOIT = (IEV.NE.0) .OR. (RP(2).NE.PP(2)) .OR. (RP(1).NE.PP(1))
C                                       something happened
      IF (DOIT) THEN
         PP(1) = RP(1)
         PP(2) = RP(2)
         CALL ZTIME (IT)
C                                       nothing is happening
      ELSE
         CALL ZTIME (LT)
         IF (LT(1).LT.IT(1)) LT(1) = LT(1) + 24
         DELFAC = 0.03
         IF (AIPSMK.GT.0.2) DELFAC = DELFAC / SQRT (AIPSMK)
         DTIME = DELFAC * ( ((LT(1)-IT(1))*SIXTY + LT(2) - IT(2)) *
     *      SIXTY+ LT(3) - IT(3) - 1)
         DTIME = MAX (0.0 , MIN (120.0, DTIME))
         IF (DTIME.GT.0.005) CALL ZDELAY (DTIME, IERR)
         END IF
C
 999  RETURN
      END
