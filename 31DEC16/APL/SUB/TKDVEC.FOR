      SUBROUTINE TKDVEC (IN, X, Y, IERR)
C-----------------------------------------------------------------------
C! converts vector command to TK graphics commands
C# Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   TKDVEC converts TEK4012 vectors to actual commands to the TK buffer
C   Positions are assumed to be in bounds.
C   Inputs:
C      IN    I     1 => dark vector, 2 => bright vector
C      X     I     X coordinate value.
C      Y     I     Y coordinate value.
C   Output:
C      IERR  I     error code, 0=ok, 1=write error.
C   Common:
C      /TKSPCL/   in/out   TKBUFF, TKPOS
C-----------------------------------------------------------------------
      INTEGER   IN, X, Y, IERR
C
      INTEGER   IT
C-----------------------------------------------------------------------
C                                       If IN = 1 'dark vector'.
      IERR = 0
      IF (IN.EQ.1) CALL ZTKBUF (29, 1, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       High Y.
      IT = (Y - 1) / 32 + 32
      CALL ZTKBUF (IT, 2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Low Y.
      IT = MOD (Y-1, 32) + 96
      CALL ZTKBUF (IT, 2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       High X.
      IT = (X-1) / 32 + 32
      CALL ZTKBUF (IT, 2, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Low X.
      IT = MOD (X-1, 32) + 64
      CALL ZTKBUF (IT, 2, IERR)
C
 999  RETURN
      END
