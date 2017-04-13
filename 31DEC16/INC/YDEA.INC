C                                               Begin YTPARM.INC
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
C               Parameter definitions specifying positions
C               in YBUFF (c.f. DTVC.INC) which correspond
C               to various registers in a DeAnza TV device.
      INTEGER   INTREG, IRBYTE, MEMREG, MRBYTE, VOCREG, VCBYTE,
     *          FCR,    FCBYTE, CURREG, CRBYTE, CHANOW, CHBYTE,
     *          GRPHCL, GRBYTE
      PARAMETER (INTREG = 1,          IRBYTE = 48,
     *           MEMREG = INTREG+24,  MRBYTE = 8,
     *           VOCREG = MEMREG+64 , VCBYTE = 8,
     *           FCR    = VOCREG+4 ,  FCBYTE = 16,
     *           CURREG = FCR   +8 ,  CRBYTE = 14,
     *           CHANOW = CURREG+7 ,  CHBYTE = 24,
     *           GRPHCL = CHANOW+12,  GRBYTE = 16)
C                                       Positions in INTERFACE REGISTERS
C                                       Relative to CONTROL REGIST (#10)
      INTEGER   CONTRL, RES, FG, BG, XR, XT, XMIN, XMAX,
     *          XAMIN, XAMAX, DX, XTEMP, YR, YT,   YMIN,
     *          YMAX, YAMIN, YAMAX, DY, YTEMP, CMR0, CMR0A
      PARAMETER (CONTRL=INTREG,   RES=INTREG+1,    FG=INTREG+2,
     *           BG=INTREG+3  ,   XR= INTREG+6,    XT=INTREG+7,
     *           XMIN=INTREG+8,   XMAX=INTREG+9,   XAMIN=INTREG+10,
     *           XAMAX=INTREG+11, DX=INTREG+12,    XTEMP=INTREG+13,
     *           YR=INTREG+14,    YT=INTREG+15,    YMIN=INTREG+16,
     *           YMAX=INTREG+17,  YAMIN=INTREG+18, YAMAX=INTREG+19,
     *           DY=INTREG+20,    YTEMP=INTREG+21, CMR0=INTREG+22,
     *           CMR0A=INTREG+23)
C                                       Positions in MEMORY REGISTERS
      INTEGER   XSCZ, YSCZ, BITPL, MEMLUT
      PARAMETER (XSCZ=MEMREG,    YSCZ = MEMREG+1,
     *           BITPL=MEMREG+2, MEMLUT=MEMREG+3)
C                                       Positions in VOC REGISTERS
      INTEGER   XVSP, YVSP, VOCCON, VOCLUT
      PARAMETER (XVSP=VOCREG,     YVSP=VOCREG+1,
     *           VOCCON=VOCREG+2, VOCLUT=VOCREG+3)
C                                       Positions in Cursor registers
      INTEGER   CURX1, CURY1, CURX2, CURY2, CURCON, CURLUT, CURBLI
      PARAMETER (CURX1=CURREG,   CURY1=CURREG+1,   CURX2=CURREG+2,
     *           CURY2=CURREG+3, CURCON=CURREG+4,  CURLUT=CURREG+5,
     *           CURBLI=CURREG+6)
