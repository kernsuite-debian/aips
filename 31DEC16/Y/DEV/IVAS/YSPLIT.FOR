      SUBROUTINE YSPLIT (OP, XSPLT , YSPLT , RCHANS, GCHANS, BCHANS,
     *   VRTRTC, IERR)
C-----------------------------------------------------------------------
C! set channel selection by split-screen quadrant
C# TV-basic
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
C   Version for IIS IVAS - uses split 2 and 4 vertically as quadrants
C   1 and 2 resp.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INTEGER   MASK, ZAND, JERR, J4A, J4B, J4C, JR, JB, JG
      INTEGER   FIVASGMFREEZE, FIVASGPHSPLIT, FIVASGMDEFIMAGE,
     *   FIVASGMSELIMAGE
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                        write: set buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((XSPLT.LT.1) .OR. (XSPLT.GT.MAXXTV(1))) GO TO 999
         IF ((YSPLT.LT.1) .OR. (YSPLT.GT.MAXXTV(2))) GO TO 999
         CALL YVRTR (VRTRTC, IERR)
         IF (IERR.NE.0) GO TO 999
         JERR = FIVASGMFREEZE (1)
         IF (JERR.NE.0) GO TO 900
         J4A = 1024 - YSPLT
         JERR = FIVASGPHSPLIT (0, J4A, 0, YSPLT, 1)
         IF (JERR.NE.0) GO TO 850
         YBUFF(10) = YSPLT
         MASK = 2 ** NGRAY - 1
         DO 20 J4A = 1,3,2
            RCHANS(J4A) = ZAND (RCHANS(J4A), MASK)
            GCHANS(J4A) = ZAND (GCHANS(J4A), MASK)
            BCHANS(J4A) = ZAND (BCHANS(J4A), MASK)
            J4B = 1
            IF ((RCHANS(J4A).EQ.0) .AND. (BCHANS(J4A).EQ.0) .AND.
     *         (GCHANS(J4A).EQ.0)) J4B = -1
C                                       try
            IF ((RCHANS(J4A).EQ.0) .AND. (BCHANS(J4A).EQ.0) .AND.
     *         (GCHANS(J4A).EQ.0)) J4B = 0
            CALL YLOWON (RCHANS(J4A), JR)
            JG = JR
            JB = JR
            IF (NGRAY.GT.2) THEN
               CALL YLOWON (GCHANS(J4A), JG)
               CALL YLOWON (BCHANS(J4A), JB)
               END IF
            J4C = (5-J4A)/2
            JERR = FIVASGMDEFIMAGE (JR, JG, JB, J4B, J4C)
            IF (JERR.NE.0) GO TO 850
            JERR = FIVASGMSELIMAGE (J4B, J4C)
            IF (JERR.NE.0) GO TO 850
            J4C = (J4A+1)/2
            YBUFF(3+J4C) = RCHANS(J4A)
            YBUFF(5+J4C) = GCHANS(J4A)
            YBUFF(7+J4C) = BCHANS(J4A)
 20         CONTINUE
         JERR = FIVASGMFREEZE (0)
         IF (JERR.NE.0) GO TO 900
         TVSPLT(1) = XSPLT
         TVSPLT(2) = YSPLT
C                                        return data
      ELSE
         IERR = 0
         XSPLT = MAXXTV(1) / 2
         YSPLT = YBUFF(10)
         RCHANS(1) = YBUFF(4)
         RCHANS(2) = YBUFF(4)
         RCHANS(3) = YBUFF(5)
         RCHANS(4) = YBUFF(5)
         GCHANS(1) = YBUFF(6)
         GCHANS(2) = YBUFF(6)
         GCHANS(3) = YBUFF(7)
         GCHANS(4) = YBUFF(7)
         BCHANS(1) = YBUFF(8)
         BCHANS(2) = YBUFF(8)
         BCHANS(3) = YBUFF(9)
         BCHANS(4) = YBUFF(9)
         END IF
      GO TO 999
C                                       Errors
 850  CALL YDOERR ('YSPLIT', JERR, IERR)
      JERR = FIVASGMFREEZE (0)
      IF (JERR.EQ.0) GO TO 999
C
 900  CALL YDOERR ('YSPLIT', JERR, IERR)
C
 999  RETURN
      END
