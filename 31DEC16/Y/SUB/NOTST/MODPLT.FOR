      SUBROUTINE MODPLT (NPLO, NPT, XDATA, YDATA, BUFFER, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   Subroutine to plot model values obtained by MODEL
C   Inputs:
C      NPLO     I          type of y-axis
C                             1 --> amplitude
C                             2 --> phase
C                             3 --> u,v distance
C                             4 --> u,v p.a.
C                             5 --> time (iat days)
C                             6 --> u
C                             7 --> v
C                             8 --> w
C                             9 --> sine channel
C                            10 --> cosine channel
C                            11 --> GAST
C      NPT      I          Number of values
C      XDATA    R(NPT)     X values
C      YDATA    R(2,NPT)   Y values
C   In/Out
C      BUFFER   I(*)       Plot buffer
C   Outputs:
C      IERR     I          Error code
C   Programmer: L.B.Baath  Onsala Space Observatory  29 october 1982
C-----------------------------------------------------------------------
      INTEGER   NPLO, NPT, BUFFER(*), IERR
      REAL      XDATA(*), YDATA(*)
C
      LOGICAL   DOMDL
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 1
      DOMDL = (NPLO.EQ.1) .OR. (NPLO.EQ.2) .OR. (NPLO.EQ.9) .OR.
     *   (NPLO.EQ.10)
      IF (DOMDL) THEN
         CALL PLOTDA (XDATA, YDATA, NPT, NPLO, 0.0, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            END IF
C                                      error return
      ELSE
         WRITE (MSGTXT,1980) NPLO
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MODPLT: PLOTDA ERROR = ',I3)
 1980 FORMAT ('MODPLT: ERROR IN NPLO = ',I5)
      END
