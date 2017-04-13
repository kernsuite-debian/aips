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
C   Version for IIS Model 70 and 75
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   COLOR, IERR
      REAL      OFM(*)
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, BUFSIZ, NB, HEADER(8), LBUF(1024), I
      LOGICAL   COMAND
      REAL      GAMINV, TEMP, SCALE
      INCLUDE 'INCS:DTVC.INC'
      DATA  UNIT, COMAND, BUFSIZ /3,.FALSE.,1024/
C-----------------------------------------------------------------------
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 0, 0, COLOR, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      IF (OP.EQ.'WRIT') THEN
         GAMINV = 1.0
         IF (TVGAMA.GT.0.0001) GAMINV = 1.0 / TVGAMA
         SCALE = OFMOUT
         DO 10 I = 1,BUFSIZ
             TEMP = MAX (0.0, MIN (1.0, OFM(I)))
             LBUF(I) = (TEMP ** GAMINV) * SCALE + 0.025
 10          CONTINUE
         END IF
      CALL ZM70XF (OP, NB, HEADER, LBUF, IERR)
      IF ((OP.EQ.'READ') .AND. (IERR.EQ.0)) THEN
         SCALE = 1.0 / OFMOUT
         DO 20 I = 1,BUFSIZ
            TEMP = (LBUF(I) * SCALE) ** TVGAMA
            OFM(I) = MAX (0.0, MIN (1.0, TEMP))
 20         CONTINUE
         END IF
C
 999  RETURN
      END
