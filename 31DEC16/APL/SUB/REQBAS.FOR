      LOGICAL FUNCTION REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS)
C-----------------------------------------------------------------------
C! Apply ANTENNA and BASELINE selection adverbs to a baseline
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   REQBAS returns a value of .TRUE. if baseline I-J is in the set
C   defined by the ANTENNAS and BASELINE adverbs. On entry, IANT should
C   contain the absolute values of the ANTENNAS adverb values and IBAS
C   should contain the BASELINE adverb values; zeros may be removed from
C   the ANTENNAS and BASELINE adverb arrays as IANT and IBAS are
C   constructed and this practice is recommended for efficiency. The
C   exact selection rules are as follows:
C   NANT   NBAS      Result
C     0      0       T
C    >0      0       (I in IANT) .OR. (J in IANT)
C     0     >0       (I in IBAS) .OR. (J in IBAS)
C    >0     >0       ((I in IANT) .AND. (J in IBAS)) .OR.
C                    ((I in IBAS) .AND. (J in IANT))
C   If DESEL is True, .NOT.Result is returned instead.
C   Inputs:
C      I       I       Antenna number #1 (I .GT. 0)
C      J       I       Antenna number #2 (J .GT. 0)
C      DOSEL   L       .TRUE. if specified baseline are to be rejected
C      IANT    I(*)    Absolute values of ANTENNAS adverb list
C      NANT    I       Number of entries in IANT (NANT .GE. 0)
C      IBAS    I(*)    Absolute values of BASELINE adverb list
C      NBAS    I       Number of entries in IBAS (NBAS .GE. 0)
C-----------------------------------------------------------------------
      INTEGER   I, J, IANT(*), NANT, IBAS(*), NBAS
      LOGICAL   DESEL
C
      INTEGER   INDEX
      LOGICAL   IINANT, IINBAS, JINANT, JINBAS, RESULT
C-----------------------------------------------------------------------
C                                       both zero
      IF ((NANT.LE.0) .AND. (NBAS.LE.0)) THEN
         RESULT = .TRUE.
C                                       one or both non-zero
      ELSE
C                                       check ANTENNA array
         IINANT = .FALSE.
         JINANT = .FALSE.
         INDEX = 0
 10      IF (((.NOT.IINANT) .OR. (.NOT.JINANT)) .AND. (INDEX.LT.NANT))
     *      THEN
            INDEX = INDEX + 1
            IINANT = IINANT .OR. (I.EQ.IANT(INDEX))
            JINANT = JINANT .OR. (J.EQ.IANT(INDEX))
            GO TO 10
            END IF
C                                       check BASELINE array
         IINBAS = .FALSE.
         JINBAS = .FALSE.
         INDEX = 0
 20      IF (((.NOT.IINBAS) .OR. (.NOT.JINBAS)) .AND. (INDEX.LT.NBAS))
     *      THEN
            INDEX = INDEX + 1
            IINBAS = IINBAS .OR. (I.EQ.IBAS(INDEX))
            JINBAS = JINBAS .OR. (J.EQ.IBAS(INDEX))
            GO TO 20
            END IF
C                                       IINANT is true iff I occurs in
C                                       array section IANT(1:NANT) and
C                                       JINANT is true iff J occurs in
C                                       array section IANT(1:NANT).
         IF (NBAS.LE.0) THEN
            RESULT = IINANT .OR. JINANT
C                                       IINBAS is true iff I is in array
C                                       subsection IBAS(1:NBAS) and
C                                       JINBAS is true iff J is in array
C                                       subsection IBAS(1:NBAS).
         ELSE IF (NANT.LE.0) THEN
            RESULT = IINBAS .OR. JINBAS
C                                       require 1 in one and 1 in other
         ELSE
            RESULT = (IINANT.AND.JINBAS) .OR. (IINBAS.AND.JINANT)
            END IF
         END IF
C                                       return result or negative
      IF (DESEL) THEN
         REQBAS = .NOT.RESULT
      ELSE
         REQBAS = RESULT
         END IF
C
 999  RETURN
      END
