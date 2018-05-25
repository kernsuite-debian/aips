      SUBROUTINE STPUT (IRNO, IDIV, BUF, XPOS, YPOS, 
     *   DXPOS, DYPOS, POSANG, STTYPE, STRCHR, IERR)
C-----------------------------------------------------------------------
C! Add a star to an open, existing star table
C# EXT-util Map-util Utility
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
C   STPUT will add a star to an open, existing star table
C    INPUTS:
C      IRNO   I    Number of the star to add
C      IDIV   I    Axis to apply cos (dec) correction with
C      BUF    I    I/O buffer
C      XPOS   D    X position (angle degrees or pixel) of star
C      YPOS   D    Y position (angle degrees or pixel) of star 
C      DXPOS  R    X size     (angle degrees or pixel) of star 
C      DYPOS  R    Y size     (angle degrees or pixel) of star
C      POSANG R    Orientation angle of star (degrees)
C      STTYPE R    One of several star types (integer) 
C      STRCHR C*24 String label for the star
C   Output:
C      IERR   I    Tabio I/O write error
C-----------------------------------------------------------------------
      INTEGER IRNO, IDIV, BUF(*), IERR
      CHARACTER STRCHR*24
      DOUBLE PRECISION XPOS, YPOS, DXPOS, DYPOS, POSANG, STTYPE
C                                       Max Label Length
      INTEGER MXSTLB
      PARAMETER (MXSTLB=24)
      HOLLERITH  STLABL(6)
      DOUBLE PRECISION X, Y, DDX, DEGRAD
      REAL      DX, DY, PA, STT
      COMMON /STARSD/ X, Y, DX, DY, PA, STT, STLABL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       If doing cos dec correction
      IF ((IDIV.GE.1) .AND. (IDIV.LE.2)) THEN
         IF (IDIV.EQ.1) DDX = YPOS
         IF (IDIV.EQ.2) DDX = XPOS
C                                       Handle cos dec correction
         DEGRAD = ATAN (1.0D0) / 45.0D0
         DDX = ABS (COS (DDX*DEGRAD))
         IF (DDX.GE.1.D-8) THEN
            IF (IDIV.EQ.1) DXPOS = DXPOS / DDX
            IF (IDIV.EQ.2) DYPOS = DYPOS / DDX
            END IF
C                                       End if cos dec correction
         END IF
C                                       Put in common block for write
      X = XPOS
      Y = YPOS
      DX = DXPOS
      DY = DYPOS
      PA = POSANG
      STT = STTYPE
C                                       Convert label to hollerith
      CALL CHR2H(MXSTLB, STRCHR, 1, STLABL(1))
C                                       Do actual write
      CALL TABIO ('WRIT', 0, IRNO, X, BUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1150) IERR
         CALL MSGWRT (8)
         END IF
C
      RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('STPUT: ERROR',I4,' WRITING ST FILE')
      END

