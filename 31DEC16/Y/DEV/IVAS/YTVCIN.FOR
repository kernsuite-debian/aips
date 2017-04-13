      SUBROUTINE YTVCIN
C-----------------------------------------------------------------------
C! initialize TV characteristics common (not needed much - see TVOPEN)
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
C   YTVCIN initializes the common which describes the characteristics
C   of the interactive display devices and the common which has the
C   current status parameters of the TV.
C   NOTE: These are default values only.  They are reset to the
C   current true values by a call to TVOPEN.
C   NOTE: YTVCIN resets the common values of TVZOOM and TVscroll, but
C   does not call the TV routines to force these to be true.  A
C   separate call to YINIT is required.
C   IVAS version: order of double statements set for "11" bit mode
C-----------------------------------------------------------------------
      INTEGER   NDUMS
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      DATA NDUMS /8/
C-----------------------------------------------------------------------
C                                       # gray image planes -
C                                       this selects 8 vs 12 bit
      NGRAY = 3
      NGRAY = 2
C                                       # graphics overlay planes
      NGRAPH = 4
C                                       # images / gray plane max
      NIMAGE = 256
C                                       screen X, Y dimensions
      MAXXTV(1) = 1024
      MAXXTV(2) = 1024
C                                       12-bit done as 11-bit to allow
C                                       2 LUTs; block last 1 to avoid
C                                       bug in graphics
      IF (NGRAY.EQ.2) THEN
         MAXINT = 2046
C                                       max gray intensity (8-bit)
      ELSE
         MAXINT = 255
         END IF
C                                       Max intensity out of LUT
      LUTOUT = 255
C                                       Max intensity into OFM
      OFMINP = 255
C                                       Max intensity out of OFM
      OFMOUT = 255
C                                       min scroll increment
      SCXINC = 1
      SCYINC = 1
C                                       max zoom (power of 2)
C                                       <0 => linear steps to 1-MXZOOM
      MXZOOM = -15
C                                       character size (XY) of TV
      CSIZTV(1) = 12
      CSIZTV(2) = 22
C                                       Type of TV split screen
C                                       0 none, 1 vert only, 2 hor
C                                       only, 3 either, 4 both
      TYPSPL = 2
C                                       Number of TV ALU units
      TVALUS = 0
C                                       Mode of loading TV: x,y
C                                          0 -> not allowed
C                                          1 -> ok in AIPS order
C                                          2 -> in reverse order ok
      TVXMOD = 1
      TVYMOD = 0
C                                       Number Image Storage Units
      ISUNUM = 0
C                                       gamma correction factor
      TVGAMA = 2.0
C                                       Number channels / color
      IF (NGRAY.EQ.2) THEN
         TVIMPC = 0
      ELSE
         TVIMPC = 1
         END IF
C                                       Scratch piece of Common
      CALL FILL (NDUMS, 0, TVDUMS)
C                                       YBUFF special area
      CALL FILL (160, 0, YBUFF)
C                                       (1) <= 0=>no transfer restrict
C                                       (2) blink rate code
C                                       (3) <= 0 => cursor off
C                                       (4) Red quad 1 bit select mask
C                                       (5) Red quad 2 bit select mask
C                                       (6) Gre quad 1 bit select mask
C                                       (7) Gre quad 2 bit select mask
C                                       (8) Blu quad 1 bit select mask
C                                       (9) Blu quad 2 bit select mask
C                                       (10) Y split point
C                                       (11-12) IVASCHAN number open
C                                       (13-16) >0 => gph chan on
      CALL FILL (16, 0, YBUFF)
C                                       (17) > 0 -> debug
      YBUFF(17) = 0
C-----------------------------------------------------------------------
C                                       TV lun, FTAB pos, map, buf#
      TVLUN = 9
      TVIND = 0
      TVLUN2 = 8
      TVIND2 = 0
      TVMAP = .FALSE.
      TVBFNO = 1
C                                       TV zoom
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
C                                       TV scrolls
      CALL FILL (16, 0, TVSCRX)
      CALL FILL (16, 0, TVSCRY)
C                                       TV last referenced image
      CALL FILL (4, 0, TVLIMG)
C                                       TV split point
      TVSPLT(1) = 1
      TVSPLT(2) = 1
C                                       Split mode: none to start
      TVSPLM = 0
      TVSPLC = 0
C                                       TV movie status
      CALL FILL (16, 0, TYPMOV)
C                                       window
      WINDTV(1) = 1
      WINDTV(2) = 1
      WINDTV(3) = MAXXTV(1)
      WINDTV(4) = MAXXTV(2)
C                                       spare control space
      CALL FILL (4, 0, TVDUM2)
C
 999  RETURN
      END
