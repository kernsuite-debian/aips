      SUBROUTINE YLUT (OP, CHANNL, COLOR, VRTRTC, LUT, IERR)
C-----------------------------------------------------------------------
C! read/write channel-based look-up-table
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
C   SUN Screen Server Version
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHANNL, COLOR, LUT(*), IERR
      LOGICAL   VRTRTC
C
      INTEGER   DAT(4), OPCODE, NWSD, TMPCLR, BUFSIZ, BITS(16), I,
     *   NBYTES
      LOGICAL   WASYNC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      DAT(1) = 0
      DAT(2) = 0
      DAT(3) = 0
      TMPCLR = COLOR
      CALL ZGTBIT (16, CHANNL, BITS)
      IERR = 2
      BUFSIZ = MAXINT + 1
      NBYTES = 2 * BUFSIZ
C                                       Read - one color
      IF (OP.EQ.'READ') THEN
         IF (TMPCLR.GE.4) THEN
            DAT(1) = 1
         ELSE IF (TMPCLR.GE.2) THEN
            DAT(2) = 1
         ELSE
            DAT(3) = 1
            END IF
C                                       old code 42
         OPCODE = 40
         DAT(4) = 0
         DO 10 I = 1,NGRAY
            IF (BITS(I).GT.0) DAT(4) = I
 10         CONTINUE
         IF (DAT(4).LE.0) GO TO 999
         I = 0
         CALL ZSSSXF (OPCODE, DAT, I, LUT, NWSD, IERR)
         IF ((IERR.EQ.0) .AND. (NWSD.NE.NBYTES)) THEN
            WRITE (MSGTXT,1000) NWSD, NBYTES
            CALL MSGWRT (7)
            END IF
C                                       write: get sep colors
      ELSE IF (OP.EQ.'WRIT') THEN
         WASYNC = ISYNCH.EQ.0
         IF (WASYNC) CALL YHOLD ('ONNN', I)
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
C                                       old code 41
         OPCODE = 39
         CALL ZILI16 (BUFSIZ, LUT, 1, LUT)
         DO 20 I = 1,NGRAY
            IF (BITS(I).GT.0) THEN
               DAT(4) = I
               CALL ZSSSXF (OPCODE, DAT, NBYTES, LUT, NWSD, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
 20         CONTINUE
         IF (WASYNC) CALL YHOLD ('OFFF', I)
         END IF
C                                       convert/reconvert to local I
      IF (IERR.EQ.0) CALL ZI16IL (BUFSIZ, 1, LUT, LUT)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('YLUT: READ',I7,' VALUES OF',I7,' REQUESTED')
      END
