C                                       Include for planetary position
C                                       (PO) table.
C----------------------------------------------------------------------
C;  Copyright (C) 2001
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
C                                       PO definitions
      INTEGER   MAXPOC, NKEYPO, NKYPO2, NKYWPO, MXSPPO, MXDPPO, IPOREV
C                                       # columns in PO table
      PARAMETER (MAXPOC = 5)
C                                       # keywords in PO table
      PARAMETER (NKEYPO = 2)
C                                       2 x # keywords
      PARAMETER (NKYPO2 = 2 * NKEYPO)
C                                       word length of keyword list
      PARAMETER (NKYWPO = 4)
C                                       Logical record length
C                                       in single precision words
      PARAMETER (MXSPPO = 10)
C                                       in double precision words
      PARAMETER (MXDPPO = MXSPPO / 2)
C                                       Current table revision no.
      PARAMETER (IPOREV = 1)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives var. type (D=double;
C                                       R=real; I=integer).
      INTEGER   PODTIM, POISID, PODRA, PODDEC, PODDST
C                                       PODTIM = Time
      PARAMETER (PODTIM = 1)
C                                       POISID = Source ID
      PARAMETER (POISID = PODTIM + 1)
C                                       PODRA = Source RA
      PARAMETER (PODRA = POISID + 1)
C                                       PODDEC = Source DEC
      PARAMETER (PODDEC = PODRA + 1)
C                                       PODDST = Source distance
      PARAMETER (PODDST = PODDEC + 1)
C                                       End include DPOV.INC
