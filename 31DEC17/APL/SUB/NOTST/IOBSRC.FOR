      SUBROUTINE IOBSRC (IA1, IA2, GOT)
C-----------------------------------------------------------------------
C! Search for antennas in the current bandpass buffer.
C# UV Calibration Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2005
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
C   Routine to search an index array set up by BPGET for the
C   presence of antennae in an I/O buffer
C   Inputs:
C      IA1      I        Antenna 1
C      IA2      I        Antenna 2
C   Outputs:
C      GOT      L(2,3)   If true antenna is present, if (*,2) true,
C                        antenna is present twice.
C   From common
C      ANTNX    LI(3,*)  Index array contains 1 or 2 pointers for each
C                        antenna, if pointer = 0 antenna not present 1
C                        or 2 times.  Pointer is to time (vis 2 later).
C   To common
C      ANTPNT   LI(2,3)  Final index array to vis array skipping time
C                        and antenna elements.
C      BPGOT    I(2,3)   Antenna numbers
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2
      LOGICAL   GOT(2,*)
C
      INTEGER   I, J
      LOGICAL   T, F
      INCLUDE 'INCS:DBPC.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       init all pointers
      DO 20 J = 1,3
         DO 10 I = 1,2
            BPGOT(I,J) = 0
            ANTPNT(I,J) = 0
            GOT(I,J) = F
 10         CONTINUE
 20      CONTINUE
C                                       set them if there are data
      DO 40 I = 1,NANTBP
         IF ((I.EQ.IA1) .OR. (I.EQ.IA2)) THEN
            DO 30 J = 1,3
               IF (ANTNX(J,I).NE.0) THEN
                  IF (I.EQ.IA1) THEN
                     GOT(1,J) = T
                     BPGOT(1,J) = IA1
                     ANTPNT(1,J) = ANTNX(J,I) + 2
                     END IF
                  IF (I.EQ.IA2) THEN
                     GOT(2,J) = T
                     BPGOT(2,J) = IA2
                     ANTPNT(2,J) = ANTNX(J,I) + 2
                     END IF
                  END IF
 30            CONTINUE
            END IF
 40      CONTINUE
C
 999  RETURN
      END
