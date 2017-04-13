C                                                          Include DAPC.
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
C                                       Include for AP memory and work
C                                       Parameterized sizes
      INCLUDE 'INCS:PAPC.INC'

      REAL   APCORE(APSIZE+1), WKVEC1(APSIZE/2+1), WKVEC2(APSIZE/2+1),
     *   WKVEC3(APSIZE/2+1), WKVEC4(APSIZE/2+1), WKVEC5(APSIZE/2+1),
     *   WKVEC6(APSIZE/2+1), WKVEC7(APSIZE/2+1), WKVEC8(APSIZE/2+1),
     *   WKVEC9(APSIZE/2+1)
      INTEGER   APCORI(1), SPAD(16),
     *   IWVEC1(APSIZE/2+1), IWVEC2(APSIZE/2+1), IWVEC3(APSIZE/2+1),
     *   IWVEC4(APSIZE/2+1), IWVEC5(APSIZE/2+1), IWVEC6(APSIZE/2+1),
     *   IWVEC7(APSIZE/2+1), IWVEC8(APSIZE/2+1), IWVEC9(APSIZE/2+1)
C                                       Include for AP memory and work
C                                       C1 Version
      COMMON /APFAKE/ APCORE, WKVEC1, WKVEC2, WKVEC3, WKVEC4, WKVEC5,
     *   WKVEC6, WKVEC7, WKVEC8, WKVEC9
      COMMON /SPF/ SPAD
C                                       Include for AP memory.
C                                       C1 Version
      EQUIVALENCE (APCORE, APCORI),
     *   (WKVEC1, IWVEC1), (WKVEC2, IWVEC2), (WKVEC3, IWVEC3),
     *   (WKVEC4, IWVEC4), (WKVEC5, IWVEC5), (WKVEC6, IWVEC6),
     *   (WKVEC7, IWVEC7), (WKVEC8, IWVEC8), (WKVEC9, IWVEC9)
C                                                          End DAPC.
