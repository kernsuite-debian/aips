      SUBROUTINE YLUT (OP, CHANNL, COLOR, VRTRTC, LUT, IERR)
C-----------------------------------------------------------------------
C! read/write channel-based look-up-table
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
C   YLUT reads/writes full channel look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      CHANNL  I      channel select bit mask
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      LUT     I(*)   look up table (dimension = MAXINT+1, values to
C                     LUTOUT are used)
C   Out:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C                        returns 2 on WRIT 12-bit colored table
C   Version for IIS Model IVAS - in 12-bit mode: uses 11 bits, 12th
C      determines channel inside the IVAS;
C      In 8-bit mode: there is no LUT: uses OFM
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHANNL, COLOR, LUT(*), IERR
      LOGICAL   VRTRTC
C
      INTEGER   I, ZAND, JERR, JB
      REAL      GAMINV, TEMP
      INTEGER   FIVASVPLUT, FIVASVPOFM
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Vertical retrace
      CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       12-bit mode
      IF (NGRAY.LE.2) THEN
C                                       write to TV
         IF (OP.EQ.'WRIT') THEN
            IF ((COLOR.NE.0) .AND. (COLOR.NE.7)) THEN
               IERR = 2
               MSGTXT = 'IVAS CANNOT PUT COLORS INTO THE 12-BIT LUT'
               CALL MSGWRT (7)
               GO TO 999
               END IF
            LUT(1) = 0
            LUT(2048) = 0
            IF (ZAND(CHANNL,1).NE.0) THEN
               JERR = FIVASVPLUT (LUT, 0, 2048, 1, 3)
               IF (JERR.NE.0) GO TO 900
               END IF
            IF (ZAND(CHANNL,2).NE.0) THEN
               JERR = FIVASVPLUT (LUT, 2048, 2048, 1, 3)
               IF (JERR.NE.0) GO TO 900
               END IF
C                                       Read (12 bit)
         ELSE
            IF (CHANNL.NE.2) THEN
               JERR = FIVASVPLUT (LUT, 0, 2048, 2, 1)
            ELSE
               JERR = FIVASVPLUT (LUT, 2048, 2048, 2, 1)
               END IF
            IF (JERR.NE.0) GO TO 900
            DO 10 I = 1,2048
               LUT(I) = MOD (LUT(I), 256)
 10            CONTINUE
            END IF
C                                       8-bit mode: no LUT!
      ELSE
C                                       write
         IF (OP.EQ.'WRIT') THEN
            JB = 0
            IF (ZAND(COLOR, 1).NE.0) JB = JB + 48
            IF (ZAND(COLOR, 2).NE.0) JB = JB + 12
            IF (ZAND(COLOR, 4).NE.0) JB = JB + 3
            GAMINV = 1.0
            IF (TVGAMA.GT.0.0001) GAMINV = 1.0 / TVGAMA
            DO 20 I = 1,256
               TEMP = MAX (0.0, MIN (1.0, LUT(I)/255.0))
               LUT(I) = (TEMP ** GAMINV) * 255.0 + 0.025
 20            CONTINUE
            JERR = FIVASVPOFM (LUT, 0, 256, 1, JB)
            IF (JERR.NE.0) GO TO 900
C                                       read
         ELSE
            JB = 1
            IF (ZAND(COLOR, 1).NE.0) JB = 16
            IF (ZAND(COLOR, 2).NE.0) JB = 4
            IF (ZAND(COLOR, 4).NE.0) JB = 1
            JERR = FIVASVPOFM (LUT, 0, 256, 2, JB)
            IF (JERR.NE.0) GO TO 900
            IF (TVGAMA.LT.0.001) TVGAMA = 1.0
            DO 160 I = 1,256
               LUT(I) = MOD (LUT(I), 256)
               IF (LUT(I).LT.0) LUT(I) = LUT(I) + 256
               TEMP = MAX (0.0, MIN (1.0, LUT(I)/255.0)) ** TVGAMA
               LUT(I) = TEMP * 255.0 + 0.001
 160           CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Error
 900  CALL YDOERR ('YLUT', JERR, IERR)
C
 999  RETURN
      END
