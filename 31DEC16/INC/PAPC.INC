C                                                          Include PAPC.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2004, 2008, 2015
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
C                                       Include for AP memory sizes
C                                       historic interest
C                                       20.25 Megaword = 81 Megabyte size
C     PARAMETER (APSIZE=21233664)
C                                       12.5 Megaword = 50 Megabyte size
C     PARAMETER (APSIZE=13107200)
C                                       5 Megaword = 20 Megabyte size
C     PARAMETER (APSIZE=5242880)
C                                       1.25 Megaword = 5 Megabyte size
C     PARAMETER (APSIZE=1310720)
C                                       1 Megaword = 4 Megabyte size
C     PARAMETER (APSIZE=1048576)
C                                       2 Megabyte size
C     PARAMETER (APSIZE=524288)
C                                       1 Megabyte size
C     PARAMETER (APSIZE=262144)
C                                       256 Kbyte size
C     PARAMETER (APSIZE=65536)
C                                       PKPWRD=primary AP size
C     PARAMETER (PKPWRD=64)
C                                       PKPWD2=secondary memory
C     PARAMETER (PKPWD2 = (APSIZE / 1024 - PKPWRD))
C                                       PKPWDC = clean memory
C                                       non-vector = all
C     PARAMETER (PKPWDC = APSIZE)
C                                       current parameters
      INTEGER   PSAPMN, PKPWRD
C                                       minimum dynamic AP size: kword
      PARAMETER (PSAPMN = 5120)
C                                       size of APCORE; > 1000 -> fixed
      PARAMETER (PKPWRD = 2)
C     PARAMETER (PKPWRD = 200000000)
C                                                          End PAPC.
