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
C   YINIT initializes the TV subunits doing everything
C   Output:
C      SCRTCH  I(1024)    scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C   Version for IIS Model 75  
C   Jan 91 **CAL**
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IERR
C
      REAL      SLOPE, RBUF(1024)
      LOGICAL   T, F, LDUMMY, TON, TLINKX, TLINKY
      INTEGER   I, J, IC, IB, ICRED(4), TX, TY, TBLINK
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Issue hardware Master Clear
      CALL YTVMC
C                                       ALU to feed OFM output
      CALL FILL (256, 0, SCRTCH)
      CALL FILL (16, 9, SCRTCH)
      IERR = 0
      IF (TVALUS.GT.0) THEN
         CALL YALUCT ('WRIT', F, 11, 11, SCRTCH(16), SCRTCH(1), F, F, F,
     *      F, LDUMMY, LDUMMY, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Zero additive constants
      CALL YCONST ('WRIT', 0, 0, 0, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Cursor off but linked
      TBLINK = 0
      TX = 256
      TY = 256
      TON = F
      TLINKX = T
      TLINKY = T
      CALL YCRCTL ('WRIT', TON, TX, TY, TLINKX, TLINKY, TBLINK,
     *   I, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear graphic LUT, set cursor
C                                       color
      CALL FILL (256, 0, SCRTCH)
      CALL YGGRAM ('WRIT', F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YGRAPH ('INIT', 8, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YGRAPH ('ONNN', 8, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YGRAFE ('WRIT', 3, 1, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       IFM: divide by 4
      DO 10 I = 1,1024
         SCRTCH(I) = (I-1) / 4
 10      CONTINUE
      DO 20 I = 1,4
         J = 1024 * (I-1) + 1
         CALL YIFM ('WRIT', J, 1024, F, F, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       OFM - assume 1 channel on
      SLOPE = 1.0 / LUTOUT
      J = LUTOUT + 1
      DO 30 I = 1,J
         RBUF(I) = (I - 1) * SLOPE
 30      CONTINUE
      I = J + 1
      IF (I+LUTOUT-1.LE.OFMINP) CALL RCOPY (J, RBUF(1), RBUF(I))
      I = J + I
      IF (I+LUTOUT-1.LE.OFMINP) CALL RCOPY (J, RBUF(1), RBUF(I))
      I = J + I
      IF (I+LUTOUT-1.LE.OFMINP) CALL RCOPY (J, RBUF(1), RBUF(I))
      CALL YOFM ('WRIT', 7, F, RBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Zoom to null
      CALL YZOOMC (0, 256, 256, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Shifts to zero
      CALL YSHIFT ('WRIT', 0, 0, 0, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Build plus sign cursor
      CALL FILL (64, 0, SCRTCH)
      SCRTCH(33) = -1
      DO 40 I = 1,64
         CALL YSTCUR ('WRIT', 1, I, 64, F, F, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
 40      CONTINUE
      CALL FILL (64, -1, SCRTCH)
      SCRTCH(33) = 0
      CALL YSTCUR ('WRIT', 1, 32, 64, F, F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Channel level
      IC = -1
      IB = -1
C                                       CHAN: luts and scroll
      SLOPE = (LUTOUT + 1.0) / (MAXINT + 1.0)
      J = MAXINT + 1
      DO 50 I = 1,J
         SCRTCH(I) = I * SLOPE - 0.5
 50      CONTINUE
      SCRTCH(1) = 0
      CALL YLUT ('WRIT', IC, 7, F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YSCROL (IC, 0, 0, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ZERO memory
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
      CALL YFDBCK (1, IC, IB, 0, F, F, T, F, F, IERR)
C                                       Split pts and chan 1 on
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
      DO 60 IC = 1,IB
         CALL YCINIT (IC, SCRTCH)
 60      CONTINUE
C                                       The following is needed to
C                                       set up the Model 75.
C                                       Set graphics subunit
      CALL YMAGIC (IERR)
C
 999  RETURN
      END
