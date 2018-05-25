      SUBROUTINE YINIT (SCRTCH, IERR)
C-----------------------------------------------------------------------
C! initialize everything about the TV
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
C   YINIT initializes the TV subunits: doing everything
C   Output:
C      SCRTCH  I(1024)    scratch buffer
C      IERR    I          error code of Z...XF - 0 => ok
C                                                2 => input error
C   IVAS version - with IVAS cursor shape, not our standard.
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IERR
C
      INTEGER   MODE, JERR, LX, LY, NX, MASK, CBUFF(2), I, J, IC,
     *   ICRED(4), LBLINK
      INTEGER   FIVASVPDEPTH, FIVASVPSETUP, FIVASCSSHAPE, FIVASFLUSH,
     *   FIVASGPHSET, FIVASGPHORIGIN, FIVASGMFREEZE, FIVASGPHWINDOW,
     *   FIVASMACURSOR
      LOGICAL   T, F, LON, LLINKX, LLINKY
      REAL      SLOPE, TDELAY, RBUF(256)
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TDELAY /1.5/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Common assummed initialized
C                                       Hardware Master Clear assummed
C                                       Set channel depth in bits
      IF (NGRAY.GT.2) THEN
         MODE = 8
      ELSE
         MODE = 12
         END IF
      JERR = FIVASVPDEPTH (MODE)
      IF (JERR.NE.0) GO TO 900
C                                       Major init: LUTs, OFMs
C                                       channel 3
      MASK = 0
      IF (NGRAY.GT.2) THEN
         MODE = 2
         JERR = FIVASVPSETUP (MODE, MODE, MODE, MASK)
         IF (JERR.NE.0) GO TO 900
         END IF
C                                       channel 2
 10   MODE = 1
      JERR = FIVASVPSETUP (MODE, MODE, MODE, MASK)
      IF (JERR.NE.0) GO TO 900
C                                       channel 1, selected
      MODE = 0
      JERR = FIVASVPSETUP (MODE, MODE, MODE, MASK)
      IF (JERR.NE.0) GO TO 900
      JERR = FIVASFLUSH
      CALL ZDELAY (TDELAY, IERR)
C                                       Drop restrictions
      CALL FILL (8, 0, YBUFF(2))
      YBUFF(1) = 1
      CALL YVRTR (F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Zero graphics memory
      LX = 0
      NX = 1024
      JERR = FIVASGPHSET (LX, LX, NX, NX, MASK)
      IF (JERR.NE.0) GO TO 900
C                                       Zero character memory
      LY = -1024
      JERR = FIVASGPHSET (LX, LY, NX, NX, MASK)
      IF (JERR.NE.0) GO TO 900
C                                       graphics origin
      JERR = FIVASGPHORIGIN (LX, LX)
      IF (JERR.NE.0) GO TO 900
C                                       set cursor pattern
      MASK = 0
      NX = 128
      LX = 1
      LY = 2
      JERR = FIVASCSSHAPE (1, NX, 2, 0)
      IF (JERR.NE.0) GO TO 900
      LY = 63
      LX = 15
      CBUFF(1) = 7
      CBUFF(2) = 14
      JERR = FIVASMACURSOR (CBUFF, 2, 1, LX, LY)
      IF (JERR.NE.0) GO TO 900
      LY = 64
      JERR = FIVASMACURSOR (CBUFF, 2, 1, LX, LY)
      IF (JERR.NE.0) GO TO 900
C                                       Graphics colors
      CALL FILL (4, 0, YBUFF(13))
      CALL YGRAPH ('INIT', 1, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 20 I = 1,4
         CALL YGRAPH ('OFFF', I, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
      CALL YGRAPH ('ONNN', 8, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set some common variables
      IC = -1
      CALL MOVIST ('OFFF', IC, 0, 0, 0, IERR)
      CALL FILL (16, 0, TVSCRX)
      CALL FILL (16, 0, TVSCRY)
      TVLIMG(1) = 1
      TVLIMG(2) = 1
      TVLIMG(3) = 1
      TVLIMG(4) = 1
      TVSPLM = 0
      TVSPLC = 0
      CALL FILL (4, 1, ICRED)
      TVSPLT(1) = MAXXTV(1)/2
      TVSPLT(2) = MAXXTV(2)/2
C                                       cursor off but linked
      LON = F
      LLINKX = T
      LLINKY = T
      LBLINK = 0
      CALL YCRCTL ('WRIT', LON, TVSPLT(1), TVSPLT(2), LLINKX, LLINKY,
     *   LBLINK, I, F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        zoom/scroll to null
      CALL YZOOMC (0, TVSPLT(1), TVSPLT(2), F, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        split pts and chan 1 on
      CALL YSPLIT ('WRIT', TVSPLT(1), TVSPLT(2), ICRED, ICRED, ICRED, F,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Disable "window"
      LX = 0
      NX = 16
      MODE = 0
      JERR = FIVASGPHWINDOW (LX, LX, NX, NX, MODE)
      IF (JERR.NE.0) GO TO 900
C                                       Set LUTs for us
      J = MAXINT + 1
      SLOPE = (LUTOUT + 1.0) / (MAXINT + 1.0)
      DO 60 I = 1,J
         SCRTCH(I) = I * SLOPE - 0.5
 60      CONTINUE
      I = (2 ** NGRAY) - 1
      J = 7
      CALL YLUT ('WRIT', I, J, F, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set OFM too to allow gamma
      DO 70 I = 1,256
         RBUF(I) = (I - 1) / 255.0
 70      CONTINUE
      CALL YOFM ('WRIT', 7, F, RBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear image catalog
      IC = NGRAY + NGRAPH
      DO 80 I = 1,IC
         CALL YCINIT (I, SCRTCH)
 80      CONTINUE
      GO TO 999
C                                       IVAS error
 800  CALL YDOERR ('YINIT', JERR, IERR)
      MASK = 0
      JERR = FIVASGMFREEZE (MASK)
      IF (JERR.EQ.0) GO TO 999
C
 900  CALL YDOERR ('YINIT', JERR, IERR)
C
 999  RETURN
      END
