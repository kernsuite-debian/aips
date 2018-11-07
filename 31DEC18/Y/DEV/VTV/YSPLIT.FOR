      SUBROUTINE YSPLIT (OP, XSPLT , YSPLT , RCHANS, GCHANS, BCHANS,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set channel selection by split-screen quadrant
C# Y1 TV-basic
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
C   YSPLIT reads/writes the look up table/ split screen control
C   registers of the TV - turns channels on/off by quadrant
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
C   Quadrants are numbered CCW from top right!!!!
C   Version for TV by communication
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = 0
      IF (VRTRTC) BUFFER(1) = 1
C                                        write: set buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((XSPLT.LT.1) .OR. (XSPLT.GT.MAXXTV(1))) GO TO 999
         IF ((YSPLT.LT.1) .OR. (YSPLT.GT.MAXXTV(2))) GO TO 999
         BUFFER(2) = XSPLT
         BUFFER(3) = YSPLT
         CALL COPY (4, RCHANS, BUFFER(4))
         CALL COPY (4, GCHANS, BUFFER(8))
         CALL COPY (4, BCHANS, BUFFER(12))
         BUFSW = 15
         BUFSR = 0
      ELSE
         BUFSW = 1
         BUFSR = 15
         END IF
C                                       do it
      CALL MKYBUF (OP, 'YSPLIT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                        reset common on write
      IF (IERR.EQ.0) THEN
         IF (OP.EQ.'WRIT') THEN
            TVSPLT(1) = XSPLT
            TVSPLT(2) = YSPLT
C                                        return data on read
         ELSE
            CALL UNYBUF (BUFSR, BUFFER, HEADER)
            XSPLT = BUFFER(2)
            YSPLT = BUFFER(3)
            CALL COPY (4, BUFFER(4), RCHANS)
            CALL COPY (4, BUFFER(8), GCHANS)
            CALL COPY (4, BUFFER(12), BCHANS)
            END IF
         END IF
C
 999  RETURN
      END
