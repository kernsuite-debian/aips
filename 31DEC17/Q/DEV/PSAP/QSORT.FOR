      SUBROUTINE QSORT (REC, K1, K2, L, N, W1, W2)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Sort records.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Do a two key sort of a list of records into ascending order of the
C   keys.
C   Inputs:
C      REC  I Base address of record array
C      K1   I Index of first (slowest) sort key
C      K2   I Index of second (faster) sort key
C      L    I Length of a record
C      N    I Number of records.
C      W1   I Base address of REAL work array of length 2*N
C      W2   I Base address of INTEGER work array of length N
C-----------------------------------------------------------------------
      INTEGER   REC, K1, K2, L, N, W1, W2
C
      INCLUDE 'INCS:DAPC.INC'
      LONGINT   IREC, IW1, IW2
      INTEGER   NP2, IERR
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      IREC = REC + PSAPOF
      IW1 = W1 + PSAPOF
      IW2 = W2 + PSAPOF
      NP2 = N + 2
C                                       Try Quick Sort
      CALL QOSORT (APCORE(IREC), N, NP2, K1, K2, L, APCORE(IW1),
     *   APCORI(IW2), IERR)
      IF (IERR.NE.0) CALL QLSORT (APCORE(IREC), N, K1, K2, L)
C
 999  RETURN
      END
