C                                       Include for weather data
C                                       (WX) table.
C----------------------------------------------------------------------
C;  Copyright (C) 2001, 2003, 2016
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
C----------------------------------------------------------------------
C                                       Generic data types
      INCLUDE 'INCS:PTAB.INC'
C                                       WX definitions
      INTEGER MAXWXC, NKEYWX, NKYWX2, NKYWWX, NKYWW2, MXSPWX, MXDPWX,
     *   IWXREV
C                                       # cols in WX table
      PARAMETER (MAXWXC = 13)
C                                       # keywords in WX table
      PARAMETER (NKEYWX = 3)
C                                       2 x # keywords
      PARAMETER (NKYWX2 = NKEYWX * 2)
C                                       word length of keyword array
      PARAMETER (NKYWWX = 5)
C                                       half word length of keyw array
      PARAMETER (NKYWW2 = NKYWWX / 2 + 1)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPWX = 13)
C                                       in double precision words
      PARAMETER (MXDPWX = MXSPWX / 2 + 1)
C                                       Current table revision no.
      PARAMETER (IWXREV = 4)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives var. type (D=double;
C                                       R=real; I=integer).
      INTEGER WXDTIM, WXRINT, WXIANT, WXSUBA, WXRTMP, WXRPRS,
     *   WXRDWP, WXRVEL, WXRDIR, WXRGUS, WXRPRE, WXRH2O, WXRION
C                                       WXDTIM = Time
      PARAMETER (WXDTIM = 1)
C                                       WXRINT = Integration time
      PARAMETER (WXRINT = WXDTIM + 1)
C                                       WXIANT = antenna
      PARAMETER (WXIANT = WXRINT + 1)
C                                       WXSUBA = subarray
      PARAMETER (WXSUBA = WXIANT + 1)
C                                       WXRTMP = surface temperature
      PARAMETER (WXRTMP = WXSUBA + 1)
C                                       WXRPRS = surface pressure
      PARAMETER (WXRPRS = WXRTMP + 1)
C                                       WXRDWP = dew point
      PARAMETER (WXRDWP = WXRPRS + 1)
C                                       WXRVEL = wind velocity
      PARAMETER (WXRVEL = WXRDWP + 1)
C                                       WXRDIR = wind direction
      PARAMETER (WXRDIR = WXRVEL + 1)
C                                       WXRGUS = wind gusts
      PARAMETER (WXRGUS = WXRDIR + 1)
C                                       WXRPRE = precipitation
      PARAMETER (WXRPRE = WXRGUS + 1)
C                                       WXRH2O = water column
      PARAMETER (WXRH2O = WXRPRE + 1)
C                                       WXRION = ion column
      PARAMETER (WXRION = WXRH2O + 1)
C                                       End include DWXV.INC
