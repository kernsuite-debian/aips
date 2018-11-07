C                                                          Include PAPC.
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
C                                       CRI version
C                                       Include for AP memory sizes
      INTEGER   APSIZE, PKPWRD, PKPWD2, PKPWDC
      INTEGER   MAXMAP, OFF1, OFF2, OFFDEL
C                                       1 Megabyte base size
C                                       "1.25 MWord" size
C     PARAMETER (APSIZE=262144)
C                                       64 Kword size
      PARAMETER (APSIZE=65536)
C                                       PKPWRD=primary AP size
      PARAMETER (PKPWRD=64)
C                                       PKPWD2=secondary memory
C                                       CRI 10 1/2-size sections more
C                                       CRI - hide the last 2 of the 12
      PARAMETER (PKPWD2=((APSIZE*6)/1024 - PKPWRD))
C                                       PKPWDC = clean memory
C                                       vector = base section only
      PARAMETER (PKPWDC = APSIZE)
C                                       OFF1 = Start of APCORE
C                                       OFF2 = Start of WKVEC1
C                                       OFFDEL = size of WKVECn
      PARAMETER (MAXMAP=8192)
      PARAMETER (OFF1=5*MAXMAP+1)
      PARAMETER (OFF2=OFF1+APSIZE+1)
      PARAMETER (OFFDEL=APSIZE/2+1)
C                                                          End PAPC.
