C                                       Include for spacecraft orbit
C                                       (OB) table.
C----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007
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
      INTEGER MAXOBC, NKEYOB, NKYOB2, MXSPOB, MXDPOB, IOBREV
C                                       # columns in OB table
      PARAMETER (MAXOBC = 8)
C                                       # keywords in OB table
      PARAMETER (NKEYOB = 1)
C                                       2 x # keywords
      PARAMETER (NKYOB2 = 2 * NKEYOB)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPOB = 24)
C                                       in double precision words
      PARAMETER (MXDPOB = MXSPOB / 2)
C                                       Current table revision no.
      PARAMETER (IOBREV = 2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives var. type (D=double;
C                                       R=real; I=integer).
      INTEGER OBIANT, OBISUB, OBDTIM, OBDPOS, OBDVEL, OBRANG, OBRECL,
     *   OBRORI
C                                       OBIANT = Antenna no.
      PARAMETER (OBIANT = 1)
C                                       OBISUB = Subarray no.
      PARAMETER (OBISUB = OBIANT + 1)
C                                       OBDTIM = Time
      PARAMETER (OBDTIM = OBISUB + 1)
C                                       OBDPOS = Spacecraft position
      PARAMETER (OBDPOS = OBDTIM + 1)
C                                       OBDVEL = Spacecraft velocity
      PARAMETER (OBDVEL = OBDPOS + 1)
C                                       OBRANG = Sun angle
      PARAMETER (OBRANG = OBDVEL + 1)
C                                       OBRECL = Eclipse flag
      PARAMETER (OBRECL = OBRANG + 1)
C                                       OBRORI = Orientation
      PARAMETER (OBRORI = OBRECL + 1)
C                                       End include
