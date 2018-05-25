      SUBROUTINE PLLINE (X, Y, IWRK, N, IRET)
C-----------------------------------------------------------------------
C! plot line beteen points
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2002, 2007
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
C   Routine to draw a line plot for data arrays X,Y
C   The input order of the arrays X and Y is ignored and the points are
C   connected in order of their X values.
C   Input:
C      N       I       Dimension of X, Y
C   Input/Output:
C      X       R(*)    Input array of X-data
C      Y       R(*)    Input array of Y-data
C      IWRK    I(*)    Integer work buffer for sort
C   Output:
C      IRET    I       Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER   N, IRET
      REAL      X(N), Y(N)
      INTEGER   IWRK(N)
C
      INCLUDE 'INCS:DPLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER I, J, IERR, ITMP
C-----------------------------------------------------------------------
C                                       Intialisation
      IRET = 0
      IF (N.LE.0) GO TO 999
C                                       Sort input arrays
      DO 10 I = 1,N
         IWRK(I) = I
 10      CONTINUE
C
      DO 30 I = 1,N-1
         DO 20 J = I+1,N
            IF (X(IWRK(J)).LT.X(IWRK(I))) THEN
               ITMP = IWRK(I)
               IWRK(I) = IWRK(J)
               IWRK(J) = ITMP
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       line type 2
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Go to first point
      CALL PLPGO (X(IWRK(1)), Y(IWRK(1)), IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Draw line segments
      DO 50 I = 2,N
         CALL PLDRW (X(IWRK(I)), Y(IWRK(I)), IERR)
         IF (IERR.NE.0) GO TO 970
 50      CONTINUE
      GO TO 999
C                                       Error in plot internals
 970  IRET = 1
      WRITE (MSGTXT,1970) IERR
C                                       Exit
 999  RETURN
C---------------------------------------------------------------------
 1970 FORMAT ('PLLINE: ERROR',I3,' FROM PLDRW')
      END
