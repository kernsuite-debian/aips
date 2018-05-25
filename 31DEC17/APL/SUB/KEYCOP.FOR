      SUBROUTINE KEYCOP (DISKIN, CNOIN, DISKO, CNOOUT, IERR)
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
C   Copy any file catalog keyword/value pairs from one file to another.
C   Inputs:
C      DISKIN  I  Input file disk number.
C      CNOIN   I  Input file catalog slot number
C      DISKO   I  Output file disk number.
C      CNOOUT  I  Output file catalog slot number
C   Outputs:
C      IERR    I  Error code, 0=OK else CATKEY error.
C-----------------------------------------------------------------------
      INTEGER   DISKIN, CNOIN, DISKO, CNOOUT, IERR
C
      INTEGER   XMXKEY
C                                       XMXKEY = max number of keywords
      PARAMETER (XMXKEY=100)
      CHARACTER KEYWRD(XMXKEY)*8
      INTEGER NUMKEY, LOCS(XMXKEY), KEYTYP(XMXKEY), VALUES(2*XMXKEY),
     *   BUFFER(256)
C-----------------------------------------------------------------------
C                                       Input and output the same?
      IF ((DISKIN.EQ.DISKO) .AND. (CNOIN.EQ.CNOOUT)) GO TO 999
C                                       Read old keyword/value pairs
      NUMKEY = XMXKEY
      CALL CATKEY ('ALL ', DISKIN, CNOIN, KEYWRD, NUMKEY, LOCS, VALUES,
     *   KEYTYP, BUFFER, IERR)
C                                       Write to output file
      IF ((IERR.EQ.0) .AND. (NUMKEY.GT.0))
     *   CALL CATKEY ('WRIT', DISKO, CNOOUT, KEYWRD, NUMKEY, LOCS,
     *   VALUES, KEYTYP, BUFFER, IERR)
C
 999  RETURN
      END
 
