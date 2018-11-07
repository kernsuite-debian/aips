      SUBROUTINE YIMGIO (OP, CHAN, X, Y, IANGL, NPIX, IMAGE, IERR)
C-----------------------------------------------------------------------
C! read/write data to the TV grey and graphics memories
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
C   YIMGIO reads/writes a line of image data to the TV screen.  For
C   graphics overlay planes, the data are solely 0's and 1's in the
C   least significant bit of IMAGE after a READ.  For 'WRIT', all bits
C   of each word should be equal (i.e. all 1's or all 0's for graphics).
C   NOTE***** on 'WRIT', the buffer may be altered by this routine for
C   some IANGLs.
C   Inputs:
C      OP      C*4       'READ' from TV or 'WRIT' to TV
C      CHAN    I         channel number (1 to NGRAY+NGRAPH)
C      X       I         start pixel position
C      Y       I         end pixel position
C      IANGL   I         = 0 => horizontal (to right)
C                        = 1 => vertical (up the screen)
C                        = 2 => horizontal (to left)
C                        = 3 => vertical (down the screen)
C      NPIX    I         number of pixels
C   In/Out:
C      IMAGE   I(NPIX)   data (only no header)
C   Output:
C      IERR    I         error code of Z...XF - 0 => ok
C                                                  2 => input err
C   Virtual TV version (for TV by communication).
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, X, Y, IANGL, NPIX, IMAGE(*), IERR
C
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF (NPIX.LT.1) GO TO 900
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 900
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 900
      IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 900
      IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 900
      IF ((IANGL.LT.0) .OR. (IANGL.GT.3)) GO TO 900
      IF ((IANGL.EQ.0) .AND. (X+NPIX-1.GT.MAXXTV(1))) GO TO 900
      IF ((IANGL.EQ.1) .AND. (Y+NPIX-1.GT.MAXXTV(2))) GO TO 900
      IF ((IANGL.EQ.2) .AND. (X-NPIX+1.LT.1)) GO TO 900
      IF ((IANGL.EQ.3) .AND. (Y-NPIX+1.LT.1)) GO TO 900
C                                       do I/O
      BUFFER(1) = CHAN
      BUFFER(2) = X
      BUFFER(3) = Y
      BUFFER(4) = IANGL
      BUFFER(5) = NPIX
      IF (OP.EQ.'WRIT') THEN
         CALL COPY (NPIX, IMAGE, BUFFER(6))
         BUFSW = 5 + NPIX
         BUFSR = 0
      ELSE
         BUFSW = 5
         BUFSR = 5 + NPIX
         END IF
      CALL MKYBUF (OP, 'YIMGIO', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      IF ((IERR.EQ.0) .AND. (OP.EQ.'READ')) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         CALL COPY (NPIX, BUFFER(6), IMAGE)
         END IF
      GO TO 999
C                                       Input error
 900  WRITE (MSGTXT,1900) OP, CHAN, NPIX, X, Y, IANGL
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YIMGIO: OP,CHAN,NPIX,X,Y,ANGL ERROR: ',A4,I4,3I5,I3)
      END
