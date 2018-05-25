      SUBROUTINE YINIT (SCRTCH, IERR)
C-----------------------------------------------------------------------
C! initialize everything about the TV
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
C   YINIT initializes the TV subunits and image catalog
C   Output:
C      SCRTCH  I(1024)    scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C   DeAnza version
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IERR
C
      LOGICAL   T, F
      INTEGER   I, J, K, KOFF, IC, IB, ICRED(4), NWORD, NCON,
     *   VOCWRD, TRESET, TRSET, ITEMP
      REAL      RBUF(256)
C                                       Memory LUT control reg
      INTEGER   GRALUT, GRFLUT
C                                       For cursor definition
      INTEGER   TYPCUR, CURPAR(5)
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:YDEA.INC'
      DATA T, F /.TRUE.,.FALSE./
C                                       VOC control word: Cursor enable
C                                       Graphics enable, XY hardware
C                                       split screen enable = Z4103
      DATA NCON, VOCWRD /10, 16643/
C                                       Memory LUT control word: Gray
C                                       scale-enable LUT, no graphics
C                                       Graphics-disable LUT, graphics
C                                       bit on = Z1000, Z0400
      DATA GRALUT, GRFLUT /4096, 1024/
C                                       Track ball reset, set
C     DATA TRESET, TRSET /Z0040, Z0061/
      DATA TRESET, TRSET /64, 97/
C-----------------------------------------------------------------------
C                                       setup non-channel units
C                                       Initialize interface registers
      NWORD = IRBYTE / 2
      CALL FILL (NWORD, 0, YBUFF(INTREG))
C                                       Resolution reg: single byte xfer
      YBUFF(RES) = 64
C                                       Max registers, full size
      YBUFF(XMAX)  = MAXXTV(1) - 1
      YBUFF(XAMAX) = YBUFF(XMAX)
      YBUFF(DX)    = YBUFF(XMAX)
      YBUFF(YMAX)  = MAXXTV(2) - 1
      YBUFF(YAMAX) = YBUFF(YMAX)
      YBUFF(DY)    = YBUFF(YMAX)
C                                       Write it out
      CALL ZDEAXF ('LR  ', YBUFF(INTREG), IRBYTE, 1, NCON, T, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up VOC
C                                       XY split=0
      CALL FILL (4, 0, YBUFF(VOCREG))
      YBUFF(VOCCON) = VOCWRD
      CALL ZDEAXF ('LVR ', YBUFF(VOCREG), VCBYTE, 0, 0, T, IERR)
C                                       cursor off but linked
      CALL YCRCTL ('WRIT', F, 256, 256, T, T, 0, I, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       OFM
      DO 10 I = 1,256
         RBUF(I) = (I - 1) / 255.0
 10      CONTINUE
      CALL YOFM ('WRIT', 7, F, RBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       zoom to null
      CALL YZOOMC (0, 256, 256, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Connect cursor to trackball
      CALL ZDEAXF ('LPR ', TRESET, 2, 0, 1, T, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZDEAXF ('LPR ', TRSET,  2, 0, 1, T, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       build plus sign cursor
      TYPCUR = 1
      CALL FILL (5, 0, CURPAR)
      CURPAR(1) = 64
      CURPAR(2) = 64
      CALL YMKCUR (TYPCUR, CURPAR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       channel level
C                                       construct bit mask
C                                       CHAN: luts and scroll
C                                       Set up bit mask
      I = 1
      J = NGRAY + NGRAPH
      IC = 2**J - 1
C                                       Reset memory registers,
      DO 20 K = 1,J
         IF (K.GT.NGRAY+1) GO TO 15
            KOFF = 4 * (K - 1)
C                                       Scroll zero
            YBUFF(KOFF+XSCZ) = 0
            YBUFF(KOFF+YSCZ) = 511
C                                       All bit planes enabled
            YBUFF(KOFF+BITPL) = -1
C                                       LUT control gray or graph
            IF (K.LE.NGRAY) THEN
               YBUFF(KOFF+MEMLUT) = GRALUT
            ELSE
               YBUFF(KOFF+MEMLUT) = GRFLUT
               END IF
C                                       Write it
            ITEMP = K - 1
            CALL ZDEAXF ('LMC ', YBUFF(KOFF+XSCZ), 8, ITEMP, 0, T, IERR)
            IF (IERR.NE.0) GO TO 999
            TVSCRX(K) = 0
            TVSCRY(K) = 0
C                                       ZERO memory
 15      CALL YZERO (K, IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       Fill LUTs, IC=channel bit mask
      DO 30 I = 1,256
         SCRTCH(I) = I - 1
 30      CONTINUE
      CALL YLUT ('WRIT', IC, 7, F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
C                                       RESTore common values
C                                       zoom, scroll, split pts
      CALL YGRAPH ('INIT', 8, SCRTCH, IERR)
      CALL FILL (256, 0, SCRTCH)
      CALL YGGRAM ('WRIT', F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YGRAPH ('ONNN', 8, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       split pts and chan 1 on
      CALL FILL (4, 1, ICRED)
      TVSPLT(1) = MAXXTV(1)/2
      TVSPLT(2) = MAXXTV(2)/2
      CALL YSPLIT ('WRIT', TVSPLT(1), TVSPLT(2), ICRED, ICRED, ICRED, F,
     *   IERR)
      CALL FILL (4, 1, TVLIMG)
      TVSPLM = 0
      TVSPLC = 0
C                                       Clear image catalog
      IB = NGRAY + NGRAPH
      DO 40 IC = 1,IB
         CALL YCINIT (IC, SCRTCH)
 40      CONTINUE
C
 999  RETURN
      END
