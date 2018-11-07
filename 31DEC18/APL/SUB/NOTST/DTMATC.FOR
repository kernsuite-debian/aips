      SUBROUTINE DTMATC (LSOURC, IANT, ISUBA, IFQID, JSOU, JANT, JSUBA,
     *   JFQID)
C-----------------------------------------------------------------------
C! Match (src, ant, suba, fqid) in simulated data arrays
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 2007
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
C   Match (src, ant, suba, fqid) in simulated data arrays
C   Input:
C      LSOURC   C*16   Source name
C      IANT     I      Antenna number
C      ISUBA    I      Subarray number
C      IFQID    I      Frequency id.
C   Output:
C      JSOU     I      Source index
C      JANT     I      Antenna index
C      JSUBA    I      Subarray index
C      JFQID    I      Freq. id. index
C---------------------------------------------------------------------
      CHARACTER LSOURC*16
      INTEGER IANT, ISUBA, IFQID, JSOU, JANT, JSUBA, JFQID
C
      INTEGER J
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Match source name in SU
C                                       table list
      JSOU = 0
      DO 50 J = 1, NDTSID
         IF (LSOURC.EQ.LDTSOU(J)) JSOU = J
50       CONTINUE
C                                       Match subarray number in list
C                                       of AN tables
      JSUBA = 0
      DO 100 J = 1, NDTSUB
         IF (ISUBA.EQ.IDTSUB(J)) JSUBA = J
100      CONTINUE
C                                       Match antenna number
      JANT = 0
      DO 150 J = 1, NDTANT(JSUBA)
         IF (IANT.EQ.IDTANO(J,JSUBA)) JANT = J
150      CONTINUE
C                                       Match frequency id.
      JFQID = 0
      DO 200 J = 1, NDTFQD
         IF (IFQID.EQ.IDTFQD(J)) JFQID = J
200      CONTINUE
C                                       Exit
999   RETURN
      END
