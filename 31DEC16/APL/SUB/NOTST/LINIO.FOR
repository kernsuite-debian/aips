      SUBROUTINE LINIO (OPCO, LUN, IND, BUFF, LX, ARR, IARR, BLANK, IER)
C-----------------------------------------------------------------------
C! Reads/writes line to/from an image.
C# IO-util Map
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
C   LINIO reads/writes one line of data from/to an image and stores the
C   results in a real array ARR, beginning at element IARR.  This is an
C   extended version of MDISK, so the reading order is determined by the
C   WIN parameter in MINIT.  LINIO handles blanked pixels.
C   Inputs:
C      OPCO    C*4    Opcode: 'READ', 'WRIT', or 'FINI'.
C      LUN     I      Logical unit number used in MINIT.
C      IND     I      Index from MAPOPN.
C      BUFF    R(*)   I/O buffer.
C      LX      I      Length of line to be read.
C      IARR    I      First array element of ARR.
C      BLANK   R      Value of blank pixel.
C   Input/Output:
C      ARR     R(*)   Array of data.
C   Output:
C      IER     I      Error code. 0=>OK, 2=>MDISK.
C-----------------------------------------------------------------------
      CHARACTER OPCO*4
      INTEGER   LUN, IND, LX, IARR, IER
      REAL      BUFF(*), ARR(*), BLANK
C
      INTEGER   IB, I, IA, BINDI
C-----------------------------------------------------------------------
      IER  = 0
C
      CALL MDISK (OPCO, LUN, IND, BUFF, BINDI, IER)
      IF (IER.EQ.0) GO TO 20
         IER = 2
         GO TO 999
 20   IF (OPCO.EQ.'FINI') GO TO 999
C                                        Read map
      IF (OPCO.EQ.'WRIT') GO TO 90
         DO 40 I = 1, LX
            IB = BINDI + I - 1
            IA = IARR  + I - 1
            IF (BUFF(IB).EQ.BLANK) THEN
               ARR(IA) = BLANK
            ELSE
               ARR(IA) = BUFF(IB)
               END IF
 40         CONTINUE
         GO TO 999
C                                        Write map
 90      DO 100 I = 1, LX
            IB = BINDI + I - 1
            IA = IARR  + I - 1
            IF (ARR(IA).EQ.BLANK) THEN
               BUFF(IB) = BLANK
            ELSE
               BUFF(IB) = ARR(IA)
               END IF
 100        CONTINUE
C
 999  RETURN
      END
