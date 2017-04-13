      SUBROUTINE YIMGIO (OP, CHAN, X, Y, IANGL, NPIX, IMAGE, IERR)
C-----------------------------------------------------------------------
C! read/write data to the TV grey and graphics memories
C# TV-basic
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
C   YIMGIO reads/writes a line of image data to the TV screen.  For
C   graphics overlay planes, the data are solely 0's and 1's in the
C   least significant bit of IMAGE after a READ.  For 'WRIT', all bits
C   of each word should be equal (i.e. all 1's or all 0's for graphics).
C   NOTE***** on 'WRIT', the buffer may be altered by this routine for
C   some IANGLs.--- not in SSS version
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
C   Version for SSS (Sun screen server), modified from IIS
C   Brian Glendenning; mod E Greisen
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, X, Y, IANGL, NPIX, IMAGE(*), IERR
C
      INTEGER   IB, DAT(4), OPCODE, NWSD, ITRY, NBYTES
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        check input
      ITRY = 0
      IERR = 2
      IF (NPIX.LT.1) GO TO 990
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 990
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 990
      IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 990
      IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 990
      IF ((IANGL.LT.0) .OR. (IANGL.GT.3)) GO TO 990
      IF ((IANGL.EQ.0) .AND. (X+NPIX-1.GT.MAXXTV(1))) GO TO 990
      IF ((IANGL.EQ.1) .AND. (Y+NPIX-1.GT.MAXXTV(2))) GO TO 990
      IF ((IANGL.EQ.2) .AND. (X-NPIX+1.LT.1)) GO TO 990
      IF ((IANGL.EQ.3) .AND. (Y-NPIX+1.LT.1)) GO TO 990
C                                        bit codes
      IF (CHAN.GT.NGRAY) THEN
         IB = 2 ** (CHAN-NGRAY-1)
      ELSE
         IB = -1
         END IF
C                                        codes to SSS
C                                        SSS handles all IANGLs
      DAT(1) = X
      DAT(2) = Y
      DAT(3) = CHAN
      DAT(4) = IANGL
      NBYTES = 2 * NPIX
C                                        write
      IF (OP.EQ.'WRIT') THEN
C                                       old code 21
         OPCODE = 19
         CALL ZILI16 (NPIX, IMAGE, 1, IMAGE)
         CALL ZSSSXF (OPCODE, DAT, NBYTES, IMAGE, NWSD, IERR)
C                                        read
      ELSE
C                                       old code 22
 22      OPCODE = 20
         ITRY = ITRY + 1
         IMAGE(1) = NPIX
         CALL ZILI16 (1, IMAGE, 1, IMAGE)
         CALL ZSSSXF (OPCODE, DAT, 2, IMAGE, NWSD, IERR)
         IF ((IERR.EQ.0) .AND. (NWSD.NE.NBYTES)) THEN
            IF (ITRY.EQ.1) GO TO 22
            WRITE (MSGTXT,1100) NWSD, NBYTES
            CALL MSGWRT (7)
            IERR = 4
            END IF
         END IF
C                                       convert/reconvert to local I
      IF (IERR.EQ.0) CALL ZI16IL (NPIX, 1, IMAGE, IMAGE)
      GO TO 999
C                                       Input error
 990  WRITE (MSGTXT,1900) OP, CHAN, NPIX, X, Y, IANGL
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('YIMGIO: READ',I7,' PIXELS OF REQUESTED',I7)
 1900 FORMAT ('YIMGIO: OP,CHAN,NPIX,X,Y,ANGL ERROR: ',A4,I4,3I5,I3)
      END
