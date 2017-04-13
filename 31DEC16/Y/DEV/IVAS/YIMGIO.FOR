      SUBROUTINE YIMGIO (OP, CHAN, X, Y, IANGL, NPIX, IMAGE, IERR)
C-----------------------------------------------------------------------
C! read/write data to the TV grey and graphics memories
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
C   Version for IIS Model IVAS
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, X, Y, IANGL, NPIX, IMAGE(*), IERR
C
      INTEGER   IX, IY, JANGL, N, I, IOFF, JJ, JB1, IXT, IB1, K, J,
     *   ZAND, LX, NX, IPL, ID, LY, IC, JERR, MODE, ITEMP,
     *   FIVASMAIMAGE, FIVASMAGRAPHICS
      INTEGER*2 LIMAGE(1024)
      LOGICAL   SWAP, VRTRTC
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA VRTRTC /.FALSE./
C-----------------------------------------------------------------------
C                                       check input
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
C                                       set position
      IX = X
      IY = Y
      SWAP = .FALSE.
      JANGL = IANGL
      IF ((IANGL.EQ.0) .AND. (TVXMOD.EQ.1)) GO TO 10
      IF ((IANGL.EQ.1) .AND. (TVYMOD.EQ.1)) GO TO 10
      IF ((IANGL.EQ.2) .AND. (TVXMOD.EQ.2)) GO TO 10
      IF ((IANGL.EQ.3) .AND. (TVYMOD.EQ.2)) GO TO 10
         JANGL = MOD (JANGL+2, 4)
         IF (IANGL.EQ.0) IX = X + NPIX - 1
         IF (IANGL.EQ.1) IY = Y + NPIX - 1
         IF (IANGL.EQ.2) IX = X - NPIX + 1
         IF (IANGL.EQ.3) IY = Y - NPIX + 1
         SWAP = .TRUE.
C                                       swap buffer order
         IF (OP.NE.'WRIT') GO TO 10
            N = NPIX / 2
            K = 1 + NPIX
            DO 5 I = 1,N
               J = IMAGE(I)
               IMAGE(I) = IMAGE(K-I)
               IMAGE(K-I) = J
 5             CONTINUE
C                                       set restrict
 10   CALL YVRTR (VRTRTC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Gray scales
      IF (CHAN.GT.NGRAY) GO TO 100
C                                       Write: prepare buffer
      IF (OP.EQ.'WRIT') THEN
C                                       12-bit grey scale: mask
C                                       bump channel 2 in high bit
         IOFF = 0
         IF ((CHAN.EQ.2) .AND. (NGRAY.LE.2)) IOFF = 2048
         J = 2047
         IF (NGRAY.GT.2) J = 255
         DO 20 I = 1,NPIX
            LIMAGE(I) = ZAND (IMAGE(I), J) + IOFF
 20         CONTINUE
         END IF
C                                       Prepare IO parms
      LX = IX - 1
      NX = NPIX
      IPL = 4095
      ID = 2
      IF (NGRAY.GT.2) IPL = 255
      MODE = 2
      IF (OP.EQ.'WRIT') MODE = 1
      IC = CHAN - 1
      LY = MAXXTV(2) - IY
      JERR = FIVASMAIMAGE (LIMAGE, NX, MODE, ID, NX, LX, LY, IC, IPL, 0)
      IF (JERR.NE.0) GO TO 900
      IF (OP.EQ.'WRIT') GO TO 999
C                                       Read
      J = 2047
      IF (NGRAY.GT.2) J = 255
      DO 40 I = 1,NPIX
         IMAGE(I) = LIMAGE(I)
         IMAGE(I) = ZAND (IMAGE(I), J)
 40      CONTINUE
C                                        fix vertical order
      IF (SWAP) THEN
         N = NPIX/2
         K = 1 + NPIX
         DO 50 I = 1,N
            J = IMAGE(I)
            IMAGE(I) = IMAGE(K-I)
            IMAGE(K-I) = J
 50         CONTINUE
         END IF
      GO TO 999
C                                       GRAPHICS
C                                       Write: prepare buffer
 100  IF (OP.NE.'WRIT') GO TO 140
C                                       go to packed, unsigned byte buf.
C                                       must start even pixel, npix even
         JJ = 1
         JB1 = 1
         IF ((BYTFLP.EQ.1) .OR. (BYTFLP.EQ.3)) JB1 = 2
         IXT = IX + NPIX + 1
         LIMAGE(JJ) = 0
         DO 120 I = IX,IXT
            IB1 = MOD (I-1, 2) + 1
            J = ZAND (IMAGE(I-IX+1), 15)
            IF (IB1.EQ.JB1) THEN
               LIMAGE(JJ) = LIMAGE(JJ) + 256 * J
            ELSE
               LIMAGE(JJ) = LIMAGE(JJ) + J
               END IF
            IF (IB1.NE.2) GO TO 120
               JJ = JJ + 1
               LIMAGE(JJ) = 0
 120        CONTINUE
C                                       Prepare IO parms
 140  IX = IX - 1
      NX = NPIX
      LX = (IX/2) * 2
      NX = ((IX+NPIX+1)/2) * 2 - LX
      IPL = 2 ** (CHAN-NGRAY-1)
      MODE = 2
      IF (OP.EQ.'WRIT') MODE = 1
      LY = MAXXTV(2) - IY
      JERR = FIVASMAGRAPHICS (LIMAGE, NX, MODE, NX, LX, LY, IPL)
      IF (JERR.NE.0) GO TO 900
      IF (OP.EQ.'WRIT') GO TO 999
C                                       go to packed, unsigned byte buf.
C                                       must start even pixel, npix even
         JJ = 1
         JB1 = 1
         IF ((BYTFLP.EQ.1) .OR. (BYTFLP.EQ.3)) JB1 = 2
         IXT = IX + NPIX
         IX = IX + 1
         J = 2 ** (CHAN - NGRAY - 1)
         DO 150 I = IX,IXT
            IB1 = MOD (I-1, 2) + 1
            ITEMP = LIMAGE(JJ)
            IF (IB1.EQ.JB1) THEN
               ITEMP = ITEMP / 256
            ELSE
               ITEMP = MOD (ITEMP, 256)
               END IF
            IMAGE(I-IX+1) = ZAND (ITEMP, J) / J
            IF (IB1.EQ.2) JJ = JJ + 1
 150        CONTINUE
C                                        fix vertical order
         IF (SWAP) THEN
            N = NPIX/2
            K = 1 + NPIX
            DO 160 I = 1,N
               J = IMAGE(I)
               IMAGE(I) = IMAGE(K-I)
               IMAGE(K-I) = J
 160           CONTINUE
            END IF
         GO TO 999
C                                       IVAS error
 900  CALL YDOERR ('YIMGIO', JERR, IERR)
      GO TO 999
C                                       Input error
 990  WRITE (MSGTXT,1990) OP, CHAN, NPIX, X, Y, IANGL
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('YIMGIO: OP,CHAN,NPIX,X,Y,ANGL ERROR: ',A4,I4,3I5,I3)
      END
