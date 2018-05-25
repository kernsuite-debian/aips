      SUBROUTINE DTINDX (LANT, IFQID, INDX)
C-----------------------------------------------------------------------
C! Find (ant,fqid) key in simulated calibration data
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007
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
C   Find (ant,fqid) key in simulated calibration data
C   Input:
C      LANT     C*8    Antenna name.
C      IFQID    I      Freq. id.
C   Output:
C      INDX     I      Returned index (0 => not found)
C---------------------------------------------------------------------
      CHARACTER LANT*8
      INTEGER IFQID, INDX
C
      LOGICAL WFOUND
      INTEGER J
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Initialization
      INDX = 0
C
      J = 1
      WFOUND = .FALSE.
20    IF (J.GT.NDTAER) GO TO 50
         WFOUND = ((LANT.EQ.LDTAER(J)).AND.(IFQID.EQ.IDTEFQ(J)))
         IF (WFOUND) GO TO 50
            J = J + 1
            GO TO 20
C                                       Antenna, fq_id. not found
50    IF (.NOT.WFOUND) GO TO 999
C                                       Index in cal. err. arrays
      INDX = J
C                                       Exit
999   RETURN
      END
