      SUBROUTINE QIHIST (A, I, C, N, NB, AMAX, AMIN)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Make histogram of an integer vector.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Pseudo-AP version
C   QIHIST computes the histogram of an integer vector.
C   Histogram element DATA - MIN where DATA is the intger data value is
C   incremented.
C   Inputs:
C      A     I  Source vector base address.  INTGER valued
C      I     I  A address increment.
C      C     I  Histogram base address
C                 Histogram must be cleared before first call.
C      N     I  Element count for A
C      NB    I  Number of bins in histogram
C      AMAX  I  Address of histogram maximum.  INTEGER valued
C      AMIN  I  Address of histogram minimum.  INTEGER valued
C-----------------------------------------------------------------------
      INTEGER   A, I, C, N, NB, AMAX, AMIN
C
      LONGINT   JA, JC, JAMAX, JAMIN, INDEX, LOOP, LEND, ITEMP
      INTEGER   XMIN, NMSG
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE NMSG
      DATA NMSG /0/
C-----------------------------------------------------------------------
C                                       Addresses to 1-rel
      JA = A + PSAPOF
      JC = C + PSAPOF
      JAMAX = AMAX + PSAPOF
      JAMIN = AMIN + PSAPOF
      XMIN = APCORI(JAMIN)
C                                        Compute histogram.
      IF (NB-1.NE.APCORI(JAMAX)-APCORI(JAMIN)) THEN
         WRITE (MSGTXT,1000) NB, APCORI(JAMIN), APCORI(JAMAX)
         IF (NMSG.LT.4) CALL MSGWRT (6)
         NMSG = NMSG + 1
         END IF
      LEND = JA + (N-1) * I
      ITEMP = JC + NB - 1
      DO 100 LOOP = JA,LEND,I
         INDEX = APCORI(LOOP) - XMIN + JC
         INDEX = MAX (JC, INDEX)
         INDEX = MIN (INDEX, ITEMP)
         APCORE(INDEX) = APCORE(INDEX) + 1.0
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QIHIST: # BINS',I9,' NOT EQUAL RANGE',2I9)
      END
