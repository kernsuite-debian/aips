      SUBROUTINE HILUT (HC, IC, HCHANG, RPOS, IFLIP, IERR)
C-----------------------------------------------------------------------
C! calculates new LUTs for hue-intensity display and sends them to TV
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2008, 2014
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
C   HILUT calculates new look up tables for hue/intensity display and
C   send them to the TV.
C   Inputs:
C      HC      I       Channel select bit mask for hue image
C      IC      I       Channel select bit mask for intensity image
C      HCHANG  L       .TRUE. if hue LUTs are to be changed
C      RPOS    R(2)    Cursor position which determines the enhancement
C                      transformation
C      IFLIP   I       +1 or -1 for sign of transfer func. slope
C   Output:
C      IERR    I       Returns YLUT error code
C   Common: /AIPSCR/
C     Outputs:
C        BUFFER  I(TVMLUT)   Scratch buffer - returns the transfer
C                               function.
C        LUT     I(TVMLUT)   Scratch buffer
C     Inputs:
C        LOGLUT  I(TVMLUT)   Log look up table
C        RBUF    I(TVMLUT)   Look up table for red color gun func
C        GBUF    I(TVMLUT)   Look up table for green color gun func
C        BBUF    I(TVMLUT)   Look up table for blue color gun func
C-----------------------------------------------------------------------
      INTEGER   HC, IC, IFLIP, IERR
      LOGICAL   HCHANG
      REAL      RPOS(2)
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   I, J, LOGLUT(TVMLUT), RBUF(TVMLUT), GBUF(TVMLUT),
     *   BBUF(TVMLUT), BUFFER(TVMLUT), LUT(TVMLUT)
      REAL      X, SLOPE, OFFSET, RVAL
      LOGICAL   T, WASYNC
      INCLUDE 'INCS:DTVC.INC'
      COMMON /AIPSCR/ BUFFER, LUT, LOGLUT, RBUF, GBUF, BBUF
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      X = ((RPOS(1) - WINDTV(1) + 1)/ (WINDTV(3) - WINDTV(1) + 1) - 0.5)
     *   * 2.0 * MAXINT
      SLOPE = (WINDTV(4)-WINDTV(2)) / (3. * MAX (1., RPOS(2)-WINDTV(2)))
     *   * IFLIP
      OFFSET = (1.0-X) * SLOPE + MAXINT * (1-IFLIP) / 2
      J = MAXINT + 1
      X = MAXINT
      DO 10 I = 1,J
         RVAL = (I-1)*SLOPE + OFFSET + 0.5
         BUFFER(I) = MAX (1.0, MIN (X, RVAL))
 10      CONTINUE
      BUFFER(1) = 0
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', I)
C                                       Change the intensity mapping
      IF (HCHANG) GO TO 50
         DO 20 I = 1,J
            LUT(I) = LOGLUT(BUFFER(I)+1)
 20         CONTINUE
         CALL YLUT ('WRIT', IC, 7, T, LUT, IERR)
         GO TO 990
C                                       Change the hue mapping
C                                       Red
 50   CONTINUE
         DO 60 I = 1,J
            LUT(I) = RBUF(BUFFER(I)+1)
 60         CONTINUE
         CALL YLUT ('WRIT', HC, 4, T, LUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Green
         DO 70 I = 1,J
            LUT(I) = GBUF(BUFFER(I)+1)
 70         CONTINUE
         CALL YLUT ('WRIT', HC, 2, T, LUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Blue
         DO 80 I = 1,J
            LUT(I) = BBUF(BUFFER(I)+1)
 80         CONTINUE
         CALL YLUT ('WRIT', HC, 1, T, LUT, IERR)
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', I)
C
 999  RETURN
      END
