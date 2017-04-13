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
C   Modified Nov-83 for DeAnza by Jaffe
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   XSPLT, YSPLT, RCHANS(4), GCHANS(4), BCHANS(4), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:YDEA.INC'
      INTEGER   NB, IRED, IGREEN, IBLUE, IGRAPH, NRED, NGREEN, NBLUE,
     *   NGRAF, LGRAPH, IFRAM, LSET, LUTOFF, J
      LOGICAL   WAIT
      INCLUDE 'INCS:DTVC.INC'
      DATA NB /16/
      DATA IRED, IGREEN, IBLUE, IGRAPH /0, 0, 4, 4/
      DATA NRED, NGREEN, NBLUE, NGRAF /1, 2, 1, 2/
C                                       If we want to disconnect a gun
C                                       point it to LUT #1 of channel 0
C                                       which we should have zeroed
C                                       previously = Z0020
      DATA LUTOFF, WAIT /32, .TRUE./
C-----------------------------------------------------------------------
C                                        write: set buffer
      IF (OP.EQ.'WRIT') THEN
         IERR = 2
         IF ((XSPLT.LT.1) .OR. (XSPLT.GT.MAXXTV(1))) GO TO 999
         IF ((YSPLT.LT.1) .OR. (YSPLT.GT.MAXXTV(2))) GO TO 999
         LGRAPH = NGRAY
C                                       DeAnza can only connect one
C                                       plane to each color gun, so
C                                       select the lowest one indicated
C                                       and set the correct byte
C                                       in the FCRs
         DO 10 IFRAM = 1,4
            CALL YLOWON (BCHANS(IFRAM), LSET)
            IF (LSET.GE.NGRAY) LSET = LUTOFF
            CALL ZPUTCH (LSET, YBUFF(FCR+IBLUE+IFRAM-1), NBLUE)
            CALL YLOWON (RCHANS(IFRAM), LSET)
            IF (LSET.GE.NGRAY) LSET = LUTOFF
            CALL ZPUTCH (LSET, YBUFF(FCR+IRED+IFRAM-1), NRED)
            CALL YLOWON (GCHANS(IFRAM), LSET)
            IF (LSET.GE.NGRAY) LSET = LUTOFF
            CALL ZPUTCH (LSET, YBUFF(FCR+IGREEN+IFRAM-1), NGREEN)
            CALL ZPUTCH (LGRAPH, YBUFF(FCR+IGRAPH+IFRAM-1), NGRAF)
 10         CONTINUE
C                                       Write out FCRs
         CALL ZDEAXF ('LVR ', YBUFF(FCR), NB, 0, 8, WAIT, IERR)
         IF (IERR.NE.0) GO TO 999
C
         YBUFF(XVSP) = XSPLT - 1
         YBUFF(YVSP) = YSPLT - 1
C                                       Write out VOC split registers
         CALL ZDEAXF ('LVR ', YBUFF(XVSP), 4, 0, 0, WAIT, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set common values
         TVSPLT(1) = XSPLT
         TVSPLT(2) = YSPLT
C                                       save requested channels
         J = CHANOW
         DO 20 IFRAM = 1,4
            YBUFF(J) = RCHANS(IFRAM)
            YBUFF(J+1) = GCHANS(IFRAM)
            YBUFF(J+2) = BCHANS(IFRAM)
            J = J + 3
 20         CONTINUE
C                                        return data
      ELSE IF (OP.EQ.'READ') THEN
C                                       Read FCRs
         CALL ZDEAXF ('RVR ', YBUFF(FCR), NB, 0, 8, WAIT, IERR)
C                                       Break up quadrants and colors
         DO 30 IFRAM = 1,4
            CALL ZGETCH (LSET, YBUFF(FCR+IFRAM+IGREEN-1), NGREEN)
            IF (LSET.GT.NGRAY-1) THEN
               GCHANS(IFRAM) = 0
            ELSE
               GCHANS(IFRAM) = 2**LSET
               END IF
C
            CALL ZGETCH (LSET, YBUFF(FCR+IFRAM+IRED-1), NRED)
            IF (LSET.GT.NGRAY-1) THEN
               RCHANS(IFRAM) = 0
            ELSE
               RCHANS(IFRAM) = 2**LSET
               END IF
C
            CALL ZGETCH (LSET, YBUFF(FCR+IFRAM+IBLUE-1), NBLUE)
            IF (LSET.GT.NGRAY-1) THEN
               BCHANS(IFRAM) = 0
            ELSE
               BCHANS(IFRAM) = 2**LSET
               END IF
 30         CONTINUE
C                                       Read split registers of VOC
         CALL ZDEAXF ('RVR ', YBUFF(XVSP), 4, 0, 0, WAIT, IERR)
         IF (IERR.NE.0) GO TO 999
         XSPLT = YBUFF(XVSP) + 1
         YSPLT = YBUFF(YVSP) + 1
C                                       Return requested channels
C                                       not the actual ones!
         J = CHANOW
         DO 40 IFRAM = 1,4
            RCHANS(IFRAM) = YBUFF(J)
            GCHANS(IFRAM) = YBUFF(J+1)
            BCHANS(IFRAM) = YBUFF(J+2)
            J = J + 3
 40         CONTINUE
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
