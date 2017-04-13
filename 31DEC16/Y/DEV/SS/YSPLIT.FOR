      SUBROUTINE YSPLIT (OP, XSPLT , YSPLT , RCHANS, GCHANS, BCHANS,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set channel selection by split-screen quadrant
C# TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2008
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
C   YSPLIT reads/writes the look up table/ split screen control
C   registers of the TV - turns channels on/off by quadrant
C   Quadrants are numbered CCW from top right!!!!
C   Inputs:
C      OP       C*4    'READ' from TV, 'WRIT' to TV
C      VRTRTC   L      T => do on vertical retrace only
C   In/Out:
C      XSPLT    I      X position of split (1-512, 1 => LHS)
C      YSPLT    I      Y position of split (1-512, 1 => bot)
C      RCHANS   I(4)   chan select bit mask 4 quadrants : red
C      GCHANS   I(4)   chan select bit mask 4 quadrants : green
C      BCHANS   I(4)   chan select bit mask 4 quadrants : blue
C   Output:
C      IERR     I      error code of Z...XF: 0 => ok, 2 => input error
C   SUN Screen Server Version
C      SSS : cannot have different images for different colors
C            > 1 gray not actually implemented now either
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INTEGER   DAT(4), BUFFER(12), NWSD, NWVD, OPCODE, J4A, I, MASK,
     *   ZAND
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                        write: set buffer
      IERR = 2
      IF (OP.EQ.'WRIT') THEN
         IF ((XSPLT.LT.1) .OR. (XSPLT.GT.MAXXTV(1))) GO TO 999
         IF ((YSPLT.LT.1) .OR. (YSPLT.GT.MAXXTV(2))) GO TO 999
         YBUFF(13) = XSPLT
         YBUFF(14) = YSPLT
         MASK = 2 ** NGRAY - 1
         DO 20 J4A = 1,4
            YBUFF(J4A+0) = ZAND (RCHANS(J4A), MASK)
            YBUFF(J4A+4) = ZAND (GCHANS(J4A), MASK)
            YBUFF(J4A+8) = ZAND (BCHANS(J4A), MASK)
 20         CONTINUE
         CALL YLOWON (YBUFF(1), I)
         TVSPLT(1) = XSPLT
         TVSPLT(2) = YSPLT
         OPCODE = 48
         DAT(1) = XSPLT
         DAT(2) = YSPLT
         DAT(3) = I + 1
         DAT(4) = 0
         CALL COPY (12, YBUFF, BUFFER)
         NWVD = 24
         CALL ZILI16 (12, BUFFER, 1, BUFFER)
         CALL ZSSSXF (OPCODE, DAT, NWVD, BUFFER, NWSD, IERR)
C                                       read comes from common only
      ELSE IF (OP.EQ.'READ') THEN
         XSPLT = YBUFF(13)
         YSPLT = YBUFF(14)
         CALL COPY (4, YBUFF(1), RCHANS)
         CALL COPY (4, YBUFF(5), GCHANS)
         CALL COPY (4, YBUFF(9), BCHANS)
         IERR = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('YSPLIT: DIFFERENT CHANNELS FOR DIFFERENT COLORS NOT',
     *   ' SUPPORTED')
      END
