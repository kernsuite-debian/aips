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
C   Deanza version NOV-83 by W. Jaffe, rev E W Greisen Feb-84
C   IT supports all 4 angles without buffer swap. (rev Jaffe, EWG 6/85)
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, X, Y, IANGL, NPIX, IMAGE(*), IERR
C
      CHARACTER LOP*4
      INTEGER   IB, I, NCON, NBYTE, BITPAT, BCHAN, ITEMP, COMANG(4),
     *   IXMIN, IXMAX, IYMIN, IYMAX, ZAND
      LOGICAL   T, UPDATE, GRAY, MEMUP
      INCLUDE 'INCS:YDEA.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
      DATA NCON /10/
      DATA COMANG /0, 4, 1, 5/
C-----------------------------------------------------------------------
C                                        check input
      IERR = 2
      IF (NPIX.LT.1) GO TO 990
      IF ((OP.NE.'READ') .AND. (OP.NE.'WRIT')) GO TO 990
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 990
      IF ((X.LT.1) .OR. (X.GT.MAXXTV(1))) GO TO 990
      IF ((Y.LT.1) .OR. (Y.GT.MAXXTV(2))) GO TO 990
C                                       Compute edges of picture
      IXMIN = 1
      IXMAX = MAXXTV(1)
      IYMIN = 1
      IYMAX = MAXXTV(2)
      IF (IANGL.EQ.0) THEN
         IXMIN = X
         IXMAX = X + NPIX - 1
      ELSE IF (IANGL.EQ.1) THEN
         IYMIN = Y
         IYMAX = Y + NPIX - 1
      ELSE IF (IANGL.EQ.2) THEN
         IXMIN = X - NPIX + 1
         IXMAX = X
      ELSE IF (IANGL.EQ.3) THEN
         IYMIN = Y - NPIX + 1
         IYMAX = Y
      ELSE
         GO TO 990
         END IF
      IF ((IXMIN.LT.1) .OR. (IYMIN.LT.1) .OR. (IXMAX.GT.MAXXTV(1))
     *   .OR. (IYMAX.GT.MAXXTV(2))) GO TO 990
      LOP = 'WI  '
      IF (OP.EQ.'READ') LOP = 'RI  '
C                                Set up channel and bit masks for
C                                transfer.  Note that gray-plane memory
C                                registers should be correctly set by
C                                YINIT and are not checked here.
      GRAY = CHAN.LE.NGRAY
      IF (GRAY) THEN
         BCHAN  = 2 ** (CHAN-1)
      ELSE
         BCHAN = 2 ** NGRAY
         BITPAT = 2 ** (CHAN-NGRAY-1)
         END IF
C                                       Update interface registers
C                                       Check soft registers to
C                                       see if changes are needed
 30   UPDATE = .FALSE.
C                                       Load channel select reg
      CALL YTCOMP (YBUFF(CMR0), BCHAN, UPDATE)
C                                       Load X,Y reg
      ITEMP = X - 1
      CALL YTCOMP (YBUFF(XR), ITEMP, UPDATE)
      ITEMP = Y - 1
      CALL YTCOMP (YBUFF(YR), ITEMP, UPDATE)
C                                       Load control & resolution
      CALL YTCOMP (YBUFF(CONTRL), COMANG(IANGL+1), UPDATE)
      ITEMP = IXMAX - 1
      CALL YTCOMP (YBUFF(XMAX), ITEMP, UPDATE)
      ITEMP = IXMIN - 1
      CALL YTCOMP (YBUFF(XMIN), ITEMP, UPDATE)
      ITEMP = IYMIN - 1
      CALL YTCOMP (YBUFF(YMIN), ITEMP, UPDATE)
      ITEMP = IYMAX - 1
      CALL YTCOMP (YBUFF(YMAX), ITEMP, UPDATE)
C                                       If needed update hard registers
      IF (UPDATE) THEN
         CALL ZDEAXF ('LR  ', YBUFF(INTREG), IRBYTE, 1, NCON, T, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                        Update Memory registers
      MEMUP = .FALSE.
      IF (.NOT.GRAY) THEN
         CALL YTCOMP (YBUFF(BITPL+4*NGRAY), BITPAT, MEMUP)
         IF (MEMUP) THEN
            CALL ZDEAXF ('LMC ', YBUFF(MEMREG+4*NGRAY), MRBYTE, NGRAY,
     *         0, T, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                        Write or Read image
      NBYTE = 2 * NPIX
      CALL ZDEAXF (LOP, IMAGE, NBYTE, 1, 0, T, IERR)
C                                        Update X or Y soft
C                                        register to reflect automatic
C                                        hardware register update at
C                                        end of line.
      IF (IERR.EQ.0) THEN
         IF (MOD(IANGL,2).EQ.0) THEN
            YBUFF(YR) = YBUFF(YR) + 1
         ELSE
            YBUFF(XR) = YBUFF(XR) + 1
            END IF
C                                        fix graphics
         IF ((OP.NE.'WRIT') .AND. (.NOT.GRAY)) THEN
            IB = 2 ** (CHAN - NGRAY - 1)
            DO 40 I = 1,NPIX
               IMAGE(I) = ZAND (IMAGE(I), IB) / IB
 40            CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Input error
 990  WRITE (MSGTXT,1900) OP, CHAN, NPIX, X, Y, IANGL
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('YIMGIO: OP,CHAN,NPIX,X,Y,ANGL ERROR: ',A4,I4,3I5,I3)
      END
