      SUBROUTINE YGRAFE (OP, BLOTCH, STATUS, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the graphics control register (IIS function)
C# Y3 TV-basic
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
C   YGRAFE reads/writes the TV graphics control register.  This
C   version does not support all the TV options: it forces the
C   black & white status monitor on and refuses to allow the disable
C   video, graphics, and cursor options.
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do it only during vertical retrace
C   In/Out:
C      BLOTCH  I     graphics plane used as blotch (1 - 7)
C      STATUS  I     graphics plane used as status (1 - 7)
C   Output:
C      IERR    I     error code of Z...XF : 0 => ok
C                                           2 => input error
C   IIS Model 75 version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   BLOTCH, STATUS, IERR
      LOGICAL   VRTRTC
C
      LOGICAL   COMAND
      INTEGER   UNIT, HEADER(8), BUFFER, BUFSIZ, BITS(16), NB, TMASK
      DATA  UNIT, COMAND, BUFSIZ /9, .TRUE., 1/
C                                       T-REG mask (hex 7FF)
      PARAMETER (TMASK = 2047)
C-----------------------------------------------------------------------
C                                       WRIT: test input, fill buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((STATUS.LT.1) .OR. (STATUS.GT.7)) GO TO 999
         IF ((BLOTCH.LT.1) .OR. (BLOTCH.GT.7)) GO TO 999
         CALL FILL (16, 0, BITS)
         NB = BLOTCH - 1
         CALL ZGTBIT (3, NB, BITS(6))
         NB = STATUS - 1
         CALL ZGTBIT (3, NB, BITS(2))
         BITS(1) = 1
         CALL ZPTBIT (16, BUFFER, BITS)
         END IF
C                                       Header & IO
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, 0, TMASK, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                       Return values
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL ZGTBIT (16, BUFFER, BITS)
         CALL ZPTBIT (3, STATUS, BITS(2))
         CALL ZPTBIT (3, BLOTCH, BITS(6))
         STATUS = STATUS + 1
         BLOTCH = BLOTCH + 1
         END IF
C
 999  RETURN
      END
