      SUBROUTINE YOFM (OP, COLOR, VRTRTC, OFM, IERR)
C-----------------------------------------------------------------------
C! read/write all-channel look-up-table ("output function memory")
C# Y1 TV-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2008
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
C   Version for SUN Screen Server
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   COLOR, IERR
      REAL      OFM(*)
      LOGICAL   VRTRTC
C
      INTEGER   TMPCLR, OPCODE, DAT(4), NWSD, BUFSIZ, LBUF(32768), I
      REAL      GAMINV, TEMP, SCALE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      DAT(1) = 0
      DAT(2) = 0
      DAT(3) = 0
      TMPCLR = COLOR
      DAT(4) = 0
      IERR = 2
      SCALE = OFMOUT
C                                       Read - one color
      IF (OP.EQ.'READ') THEN
         IF (TMPCLR.GE.4) THEN
            DAT(1) = 1
         ELSE IF (TMPCLR.GE.2) THEN
            DAT(2) = 1
         ELSE
            DAT(3) = 1
            END IF
C                                       was 44
         OPCODE = 38
         BUFSIZ = 0
         CALL ZSSSXF (OPCODE, DAT, BUFSIZ, LBUF, NWSD, IERR)
         BUFSIZ = OFMINP + 1
         IF (IERR.EQ.0) THEN
            IF (NWSD.NE.BUFSIZ) THEN
               WRITE (MSGTXT,1000) NWSD, BUFSIZ
               CALL MSGWRT (7)
            ELSE
               CALL ZI8IL (BUFSIZ, 1, LBUF, LBUF)
               NWSD = OFMINP + 1
               DO 10 I = 1,NWSD
                  TEMP = (LBUF(I) / SCALE) ** TVGAMA
                  OFM(I) = MAX (0.0, MIN (1.0, TEMP))
 10               CONTINUE
               END IF
            END IF
C                                       write: get sep colors
      ELSE IF (OP.EQ.'WRIT') THEN
         IF (TMPCLR.GE.4) THEN
            DAT(1) = 1
            TMPCLR = TMPCLR - 4
            END IF
         IF (TMPCLR.GE.2) THEN
            DAT(2) = 1
            TMPCLR = TMPCLR - 2
            END IF
         IF (TMPCLR.GE.1) THEN
            DAT(3) = 1
            TMPCLR = TMPCLR - 1
            END IF
         BUFSIZ = OFMINP + 1
         GAMINV = 1.0
         IF (TVGAMA.GT.0.0001) GAMINV = 1.0 / TVGAMA
         DO 20 I = 1,BUFSIZ
             TEMP = MAX (0.0, MIN (1.0, OFM(I)))
             LBUF(I) = (TEMP ** GAMINV) * SCALE + 0.025
 20          CONTINUE
         CALL ZILI8 (BUFSIZ, LBUF, 1, LBUF)
C                                       was 43
         OPCODE = 37
         CALL ZSSSXF (OPCODE, DAT, BUFSIZ, LBUF, NWSD, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YOFM: READ',I7,' VALUES OF',I7,' REQUESTED')
      END
