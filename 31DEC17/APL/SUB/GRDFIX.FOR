      SUBROUTINE GRDFIX (CXTYPE, CYTYPE, XPARM, YPARM, CELLSZ, BEMSZ)
C-----------------------------------------------------------------------
C! correct singledish handling of grid parameters
C# singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997
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
C   Changes input gridding parameters from arc sec to cells if needed,
C   then fills in defaults
C   Inputs:
C      CELLSZ   R(2)    Cell sizes in arc sec
C      BEMSZ    R(2)    Beam size in arc sec
C   In/out
C      CXTYPE   I       X convolution type
C      CYTYPE   I       Y convolution type
C      XPARM    R(10)   X convolution parameters
C      YPARM    R(10)   Y convolution parameters
C-----------------------------------------------------------------------
      INTEGER   CXTYPE, CYTYPE
      REAL      XPARM(10), YPARM(10), CELLSZ(2), BEMSZ(2)
C
      REAL      LBEM
C-----------------------------------------------------------------------
C                                       X axis
      IF ((CXTYPE.LE.-7) .AND. (CXTYPE.GE.-10)) CXTYPE = -5
      IF ((CXTYPE.LE.-17) .AND. (CXTYPE.GE.-20)) CXTYPE = -15
      IF ((CELLSZ(1).GT.0.0) .AND. (CXTYPE.LT.0)) THEN
         LBEM = BEMSZ(1)
         IF (LBEM.LE.0.0) LBEM = CELLSZ(1) * 3.0
         LBEM = LBEM / 3.0
         IF ((CXTYPE.EQ.-1) .OR. (CXTYPE.EQ.-11)) THEN
            IF (XPARM(1).LE.0.) XPARM(1) = 0.5 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
         ELSE IF ((CXTYPE.EQ.-2) .OR. (CXTYPE.EQ.-12)) THEN
            IF (XPARM(1).LE.0.0) XPARM(1) = 3.0 * LBEM
            IF (XPARM(2).LE.0.0) XPARM(2) = 1.0 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
            XPARM(2) = XPARM(2) / CELLSZ(1)
         ELSE IF ((CXTYPE.EQ.-3) .OR. (CXTYPE.EQ.-13)) THEN
            IF (XPARM(1).LE.0.0) XPARM(1) = 3.0  * LBEM
            IF (XPARM(2).LE.0.0) XPARM(2) = 1.14 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
            XPARM(2) = XPARM(2) / CELLSZ(1)
         ELSE IF ((CXTYPE.EQ.-4) .OR. (CXTYPE.EQ.-14)) THEN
            IF (XPARM(1).LE.0.0) XPARM(1) = 3.0  * LBEM
            IF (XPARM(2).LE.0.0) XPARM(2) = 1.55 * LBEM
            IF (XPARM(3).LE.0.0) XPARM(3) = 2.52 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
            XPARM(2) = XPARM(2) / CELLSZ(1)
            XPARM(3) = XPARM(3) / CELLSZ(1)
         ELSE IF ((CXTYPE.EQ.-5) .OR. (CXTYPE.EQ.-15)) THEN
            IF (XPARM(1).LE.0.0) XPARM(1) = 3.0 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
         ELSE IF ((CXTYPE.EQ.-6) .OR. (CXTYPE.EQ.-16)) THEN
            IF (XPARM(1).LE.0.0) XPARM(1) = 3.0  * LBEM
            IF (XPARM(2).LE.0.0) XPARM(2) = 1.55 * LBEM
            IF (XPARM(3).LE.0.0) XPARM(3) = 2.52 * LBEM
            XPARM(1) = XPARM(1) / CELLSZ(1)
            XPARM(2) = XPARM(2) / CELLSZ(1)
            XPARM(3) = XPARM(3) / CELLSZ(1)
            END IF
         CXTYPE = -CXTYPE
         END IF
C                                       Y axis
      IF ((CYTYPE.LE.-7) .AND. (CYTYPE.GE.-10)) CYTYPE = -5
      IF ((CYTYPE.LE.-17) .AND. (CYTYPE.GE.-20)) CYTYPE = -15
      IF ((CELLSZ(2).GT.0.0) .AND. (CYTYPE.LT.0)) THEN
         LBEM = BEMSZ(2)
         IF (LBEM.LE.0.0) LBEM = CELLSZ(2) * 3.0
         LBEM = LBEM / 3.0
         IF ((CYTYPE.EQ.-1) .OR. (CYTYPE.EQ.-11)) THEN
            IF (YPARM(1).LE.0.) YPARM(1) = 0.5 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
         ELSE IF ((CYTYPE.EQ.-2) .OR. (CYTYPE.EQ.-12)) THEN
            IF (YPARM(1).LE.0.0) YPARM(1) = 3.0 * LBEM
            IF (YPARM(2).LE.0.0) YPARM(2) = 1.0 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
            YPARM(2) = YPARM(2) / CELLSZ(2)
         ELSE IF ((CYTYPE.EQ.-3) .OR. (CYTYPE.EQ.-13)) THEN
            IF (YPARM(1).LE.0.0) YPARM(1) = 3.0  * LBEM
            IF (YPARM(2).LE.0.0) YPARM(2) = 1.14 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
            YPARM(2) = YPARM(2) / CELLSZ(2)
         ELSE IF ((CYTYPE.EQ.-4) .OR. (CYTYPE.EQ.-14)) THEN
            IF (YPARM(1).LE.0.0) YPARM(1) = 3.0  * LBEM
            IF (YPARM(2).LE.0.0) YPARM(2) = 1.55 * LBEM
            IF (YPARM(3).LE.0.0) YPARM(3) = 2.52 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
            YPARM(2) = YPARM(2) / CELLSZ(2)
            YPARM(3) = YPARM(3) / CELLSZ(2)
         ELSE IF ((CYTYPE.EQ.-5) .OR. (CYTYPE.EQ.-15)) THEN
            IF (YPARM(1).LE.0.0) YPARM(1) = 3.0 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
         ELSE IF ((CYTYPE.EQ.-6) .OR. (CYTYPE.EQ.-16)) THEN
            IF (YPARM(1).LE.0.0) YPARM(1) = 3.0  * LBEM
            IF (YPARM(2).LE.0.0) YPARM(2) = 1.55 * LBEM
            IF (YPARM(3).LE.0.0) YPARM(3) = 2.52 * LBEM
            YPARM(1) = YPARM(1) / CELLSZ(2)
            YPARM(2) = YPARM(2) / CELLSZ(2)
            YPARM(3) = YPARM(3) / CELLSZ(2)
            END IF
         CYTYPE = -CYTYPE
         END IF
C                                       now regular defaults
      CALL GRDFLT (CXTYPE, CYTYPE, XPARM, YPARM)
C
 999  RETURN
      END
