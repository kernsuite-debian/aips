      SUBROUTINE YOFM (OP, COLOR, VRTRTC, OFM, IERR)
C-----------------------------------------------------------------------
C! read/write all-channel look-up-table ("output function memory")
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
C   Modified NOV-83 for DeAnza by W. Jaffe; corrected EWG 12/85.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   COLOR, IERR
      REAL      OFM(*)
      LOGICAL   VRTRTC
C
      INTEGER   CA, NB, I, AG1, VOCN, VOCLUT, LUTADR, ICOL, MOD, LCOL,
     *   LOFM(512)
      REAL      TEMP, GAMINV
      LOGICAL   ON, T
      INCLUDE 'INCS:DTVC.INC'
      DATA T /.TRUE./
C                                Output LUT is byte array group # 1
C                                We only use VOC number 0
C                                VOC LUT address in VOC register #3
      DATA NB, AG1, VOCN, VOCLUT, LUTADR /1024, 1, 0, 3, 0/
C-----------------------------------------------------------------------
C                                       Update all 3 colors
      IF (OP.EQ.'WRIT') THEN
         GAMINV = 1.0
         IF (TVGAMA.GT.0.0001) GAMINV = 1.0 / TVGAMA
         DO 10 I = 1,256
             TEMP = MAX (0.0, MIN (1.0, OFM(I)))
             LOFM(I) = (TEMP ** GAMINV) * 255.0 + 0.025
 10          CONTINUE
C                                       Zero 2nd LUT section
         CALL FILL (256, 0, LOFM(257))
         LCOL = COLOR
         DO 15 ICOL = 1,3
            ON = (MOD(LCOL, 2).NE.0)
            LCOL = LCOL / 2
            IF (ON) THEN
C                                       Zero LUT address in VOC
               CALL ZDEAXF ('LVR ', LUTADR, 2, VOCN, VOCLUT, T, IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Set channel address to
C                                       match color
               CA = 3 - ICOL
               CALL ZDEAXF ('LPA ', LOFM, NB, CA, AG1, T, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 15         CONTINUE
C                                       Read opcode
      ELSE IF (OP.EQ.'READ') THEN
         LCOL = COLOR
C                                       Can only read 1 color, 1st on
         DO 20 ICOL = 1,3
            ON = (MOD(LCOL, 2).NE.0)
            LCOL = LCOL / 2
            IF (ON) THEN
C                                       Set LUT address
               CALL ZDEAXF ('LVR ', LUTADR, 2, VOCN, VOCLUT, T, IERR)
               IF (IERR.NE.0) GO TO 999
               CA = 3 - ICOL
               CALL ZDEAXF ('RPA ', LOFM, NB, CA, AG1, T, IERR)
               IF (IERR.NE.0) GO TO 999
               GO TO 30
               END IF
 20         CONTINUE
C                                       Move to output buffer
 30      DO 40 I = 1,256
           TEMP = (LOFM(I) / 255.0) ** TVGAMA
           OFM(I) = MAX (0.0, MIN (1.0, TEMP))
 40        CONTINUE
      ELSE
         IERR = 2
         END IF
C
 999  RETURN
      END
