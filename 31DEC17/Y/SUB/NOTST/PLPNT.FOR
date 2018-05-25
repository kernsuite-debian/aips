      SUBROUTINE PLPNT (X, Y, N, ISYM, SYMSIZ, IRET)
C-----------------------------------------------------------------------
C! plot one point
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
C   Plot a symbol at each point defined by an array of data values
C   Inputs:
C      X       R(*)    Array of x-values
C      Y       R(*)    Array of y-values
C      ISYM    I       Symbol type (1=plus sign, 2=X)
C      SYMSIZ  R       Symbol size (0..1) as a fraction of axis length
C   Output
C      IRET    I       Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER   N, ISYM, IRET
      REAL      X(N), Y(N), SYMSIZ
C
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DPLD.INC'
      REAL      XSIZ, YSIZ, XRNG, YRNG
      INTEGER   I, IERR
C----------------------------------------------------------------------
      IRET = 0
C                                       x-,y-range in values
      XRNG = AXINC(1,LOCNUM) * (PLTRC(1) - PLBLC(1)) / XPFACT
      YRNG = AXINC(2,LOCNUM) * (PLTRC(2) - PLBLC(2)) / YPFACT
C                                       line type 4
      CALL GLTYPE (4, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Loop through the points
      DO 20 I = 1, N
         XSIZ = ABS (XRNG) * SYMSIZ / 2.0
         YSIZ = ABS (YRNG) * SYMSIZ / 2.0
C                                       Case symbol of:
C                                       1: plus sign
         IF (ISYM.EQ.1) THEN
            CALL PLPGO (X(I), Y(I)+YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLDRW (X(I), Y(I)-YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLPGO (X(I)-XSIZ, Y(I), IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLDRW (X(I)+XSIZ, Y(I), IERR)
            IF (IERR.NE.0) GO TO 970
         ELSE IF (ISYM.EQ.2) THEN
            CALL PLPGO (X(I)+XSIZ, Y(I)+YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLDRW (X(I)-XSIZ, Y(I)-YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLPGO (X(I)+XSIZ, Y(I)-YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL PLDRW (X(I)-XSIZ, Y(I)+YSIZ, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
C
 20      CONTINUE
C                                       Error in plot internals
 970  IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1970) IERR
         CALL MSGWRT (8)
         END IF
C                                       Exit
 999  RETURN
C---------------------------------------------------------------------
 1970 FORMAT ('PLPNT: ERROR',I5,' RETURNED BY PLPGO OR PLDRW')
      END
