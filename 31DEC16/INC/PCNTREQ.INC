C              ***  PCNTREQ.DCL  ***
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
C             VERSION 4.0  -  4 JANUARY 1985
C             Version 4.1  -  12 February 1987
C        THIS FILE CONTAINS THE REQ-DECLARATIONS FOR ACTPLT AND ACTPL2
C             VAX (AIPS - KONTR) VERSION
C
      DOUBLE PRECISION
     *   RA(100), DEC(100)
      REAL   PSCL,ICUT,PCUT
      REAL   C(30),SCX,SCY,DX,DY,H,W,SCL,SCS,SCP,DRAS,DDECS,XLL,YLL
      INTEGER   IBLC(7),ITRC(7),IMI,IMA,JMI,JMA,ISTRIP,NSTRIP,NC,
     1        NS,IRATE,NPEN(30),IDASH(30),OLDSTR(12),XLAB(12), YLAB(12)
      INTEGER   PPEN,PXINC,PYINC,IPMI,JPMI,JPMA,PUN(4)
      LOGICAL   GRID,POLVEC,RELCRD,PIXCRD,LFLG,BFLG,
     1          LLABEL,PCTFLG
      INTEGER   REQSIZ    !LENGTH OF BLK IN BYTES
      INTEGER   BLK(1030)
      EQUIVALENCE (BLK(1),RA(1)),(BLK(401),DEC(1))  !STAR POSITIONS
      EQUIVALENCE (BLK(801),NS)    !NO OF STAR POSITIONS TO PLOT
      EQUIVALENCE (BLK(802),NC)    !NO OF CONTOURS TO PLOT
      EQUIVALENCE (BLK(803),C(1))  !CONTOUR LEVELS
      EQUIVALENCE (BLK(863),NPEN(1))  !PEN FOR EACH CONTOUR
      EQUIVALENCE (BLK(893),IDASH(1)) !DASH CODE FOR EACH CONTOUR
      EQUIVALENCE (BLK(923),IRATE) !PLOTTING SPEED
      EQUIVALENCE (BLK(924),GRID)  !IF TRUE RA,DEC GRID WILL DE PLOTTED
      EQUIVALENCE (BLK(925),POLVEC)     !IF TRUE POLARIZATION VECTORS
C                                        WILL BE PLOTTED.
      EQUIVALENCE (BLK(926),RELCRD)     !IF TRUE PLOT WILL BE LABELED IN
C                                        RELATIVE C.
      EQUIVALENCE (BLK(927),LFLG)  !FLAG FOR OLD RA LABEL PLOTTED ON NEW
C                                        STRIP
      EQUIVALENCE (BLK(928),IMI,IBLC(1)),(BLK(935),IMA,ITRC(1)),
     1        (BLK(929),JMI),(BLK(936),JMA)    !BOUNDARY OF AREA TO BE
C                                        PLOTTED
      EQUIVALENCE (BLK(942),ISTRIP)     !NO OF POINTS IN X PER STRIP
      EQUIVALENCE (BLK(943),NSTRIP)     !NO OF STRIPS TO BE PLOTTED
      EQUIVALENCE (BLK(944),SCX),(BLK(946),SCY) !PLOT SCALES (ARCSEC/MM)
      EQUIVALENCE (BLK(948),DX),(BLK(950),DY)   !PLOT SCALES (CM/CELL)
      EQUIVALENCE (BLK(952),W),(BLK(954),H)  !TOTAL SIZE OF PLOT (CM)
      EQUIVALENCE (BLK(956),SCL)   !SCALE OF LABELING
      EQUIVALENCE (BLK(960),SCS)   !SCALE OF STAR MARKS
      EQUIVALENCE (BLK(964),SCP)   !SCALE OF POLARIZATION VECTORS
      EQUIVALENCE (BLK(966),DRAS)  !INTERVAL BETWEEN RA TICKS IN SECONDS
      EQUIVALENCE (BLK(968),DDECS) !INTERVAL BETWEEN DEC TICKS IN ARCSEC
      EQUIVALENCE (BLK(970),XLL)   !X-COORD OF OLD STRING
      EQUIVALENCE (BLK(972),YLL)   !Y-COORD OF OLD STRING
      EQUIVALENCE (BLK(974),OLDSTR)!OLD RA STRING FOR NEW STRIP
      EQUIVALENCE (BLK(986),LLABEL)!LONG OR SHORT LABEL
      EQUIVALENCE (BLK(987),PCTFLG)!CONTOUR LEVELS IN PERCENTAGES
      EQUIVALENCE (BLK(988),PIXCRD)!ANNOTATION IN PIXEL COORDS
      EQUIVALENCE (BLK(989),BFLG)  !BEAM TO BE PLOTTED?
      EQUIVALENCE (BLK(990),PSCL)  !SCALE OF POL. VECTORS
      EQUIVALENCE (BLK(992),PPEN)  !PEN NO. TO USE FOR POL. VECTORS
      EQUIVALENCE (BLK(993),ICUT)  !POL. VEC. CUT-OFF IN I-MAP
      EQUIVALENCE (BLK(995),PCUT)  !POL. VEC. CUT-OFF IN P-MAP
      EQUIVALENCE (BLK(997),PXINC) !POL. VEC. SPACING IN PIXELS
      EQUIVALENCE (BLK(998),PYINC) !IDEM IN Y DIRECTION
      EQUIVALENCE (BLK(999),IPMI)  !X PIXEL TO START NEXT POL. VEC.
      EQUIVALENCE (BLK(1000),JPMI)  !Y PIXEL OF FIRST POL. VEC.
      EQUIVALENCE (BLK(1001),JPMA)  !Y PIXEL OF LAST POL. VEC.
      EQUIVALENCE (BLK(1002),PUN(1))!UNITS OF POLI MAP
      EQUIVALENCE (BLK(1006),XLAB(1))    !ANNOTATION LABEL X-AXIS
      EQUIVALENCE (BLK(1018),YLAB(1))    !ANNOTATION LABEL Y-AXIS
      COMMON /CNTCTL/ BLK
