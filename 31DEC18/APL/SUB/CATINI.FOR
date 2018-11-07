      SUBROUTINE CATINI (HEADER)
C-----------------------------------------------------------------------
C! initializes a catalog header block
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2017
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
C   Initializes an image header (or uv) with 0's blanks, 1's etc
C   Assumes that integer 0's are also real and double 0's
C   Output:
C      HEADER   I(256)   Filled header
C-----------------------------------------------------------------------
      INTEGER   HEADER(256)
C
      INTEGER   I, J, CATBLK(256)
      HOLLERITH CATH(256)
      REAL      CATR(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH)
C-----------------------------------------------------------------------
C                                       start with 0's
      CALL FILL (256, 0, CATBLK)
C                                       characters
      CALL RFILL (2, HBLANK, CATH(KHOBJ))
      CALL RFILL (2, HBLANK, CATH(KHTEL))
      CALL RFILL (2, HBLANK, CATH(KHINS))
      CALL RFILL (2, HBLANK, CATH(KHOBS))
      CALL RFILL (2, HBLANK, CATH(KHDOB))
      CALL RFILL (2, HBLANK, CATH(KHDMP))
      CALL RFILL (2, HBLANK, CATH(KHBUN))
C                                       random parameters
      J = KHPTP
      DO 10 I = 1,KIPTPN
         CALL RFILL (2, HBLANK, CATH(J))
         J = J + 2
 10      CONTINUE
C                                       axis parameters
      DO 20 I = 1,KICTPN
         J = I - 1
         CALL RFILL (2, HBLANK, CATH(KHCTP+2*J))
         CATR(KRCIC+J) = 1.0
         CATR(KRCRP+J) = 1.0
         CATBLK(KINAX+J) = 1
 20      CONTINUE
C                                       epoch, name
      CATR(KREPO) = 2000.0
      CALL RFILL (5, HBLANK, CATH(KHIMN))
      CATBLK(KIIMU) = NLUSER
      CATR(KRWTN) = 1.0
C                                       extensions
      CALL CATCLR (CATBLK)
C                                       return it
      CALL COPY (256, CATBLK, HEADER)
C
 999  RETURN
      END
