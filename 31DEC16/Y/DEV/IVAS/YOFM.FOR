      SUBROUTINE YOFM (OP, COLOR, VRTRTC, OFM, IERR)
C-----------------------------------------------------------------------
C! read/write all-channel look-up-table ("output function memory")
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
C   YOFM reads/writes full OFM look up tables to TV.
C   Inputs:
C      OP      C*4    'READ' from TV, 'WRIT' to TV
C      COLOR   I      color select bit mask (RGB <-> 421)
C      VRTRTC  L      T => do it only during vertical retrace
C   In/Out:
C      OFM     R(*)   look up table (dimension = OFMINP+1, values 0
C                     through 1 are used, others clipped)
C   Output:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   IVAS version: OFM is 8-bits in and out for the IVAS
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   COLOR, IERR
      REAL      OFM(*)
      LOGICAL   VRTRTC
C
      INTEGER   I, ZAND, JB, JERR, LBUF(256)
      REAL      GAMINV, TEMP
      INTEGER   FIVASVPOFM
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       write
      IF (OP.EQ.'WRIT') THEN
         OFM(1) = 0
         JB = 0
         IF (ZAND(COLOR, 1).NE.0) JB = JB + 48
         IF (ZAND(COLOR, 2).NE.0) JB = JB + 12
         IF (ZAND(COLOR, 4).NE.0) JB = JB + 3
         GAMINV = 1.0
         IF (TVGAMA.GT.0.0001) GAMINV = 1.0 / TVGAMA
         DO 10 I = 1,256
             TEMP = MAX (0.0, MIN (1.0, OFM(I)))
             LBUF(I) = (TEMP ** GAMINV) * 255.0 + 0.025
 10          CONTINUE
         JERR = FIVASVPOFM (LBUF, 0, 256, 1, JB)
         IF (JERR.NE.0) GO TO 900
C                                       read
      ELSE
         JB = 1
         IF (ZAND(COLOR, 1).NE.0) JB = 16
         IF (ZAND(COLOR, 2).NE.0) JB = 4
         IF (ZAND(COLOR, 4).NE.0) JB = 1
         JERR = FIVASVPOFM (LBUF, 0, 256, 2, JB)
         IF (JERR.NE.0) GO TO 900
         DO 20 I = 1,256
            LBUF(I) = MOD (LBUF(I), 256)
            IF (LBUF(I).LT.0) LBUF(I) = LBUF(I) + 256
            OFM(I) = MAX (0.0, MIN (1.0, LBUF(I)/255.0)) ** TVGAMA
 20         CONTINUE
         END IF
      GO TO 999
C                                       Error
 900  CALL YDOERR ('YOFM', JERR, IERR)
C
 999  RETURN
      END
