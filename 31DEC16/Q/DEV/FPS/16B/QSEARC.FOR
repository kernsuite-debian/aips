      SUBROUTINE QSEARC (NF, NT, MF, MT, ND, NR, DATA)
C-----------------------------------------------------------------------
C! FPS AP version: VLBI fringe search with FFTs
C# AP-util
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
C   FPS AP version
C   A routine to do a gridded fringe search by means of  padded FFTs.
C   The number (size) of the FFTs is NF (MT) + NR (MF).
C    Inputs:
C     NF    I  No. frequency channels.
C     NT    I  No. time points.
C     MF    I  No. points for freq. FFT.
C     MT    I  No. points for time FFT.
C     ND    I  No. delay channels to search.
C     NR    I  No. rate channels to search.
C     DATA  I  Base address of data array (NT,NF)
C    Output: (in AP memory)
C    0 => 1 dim. array index for peak.
C    1 => peak amplitude
C    2 => peak phase.
C        The resulting delay and rate channels should have the
C        following offsets added:  (ND/2, NR/2)
C-----------------------------------------------------------------------
      INTEGER NF, NT, MF, MT, ND, NR, DATA
      INTEGER*2 INF, INT, IMF, IMT, IND, INR, IDATA
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (NF.LT.32768) INF = NF
      IF (NF.GE.32768) INF = NF - 65536
      IF (NT.LT.32768) INT = NT
      IF (NT.GE.32768) INT = NT - 65536
      IF (MF.LT.32768) IMF = MF
      IF (MF.GE.32768) IMF = MF - 65536
      IF (MT.LT.32768) IMT = MT
      IF (MT.GE.32768) IMT = MT - 65536
      IF (ND.LT.32768) IND = ND
      IF (ND.GE.32768) IND = ND - 65536
      IF (NR.LT.32768) INR = NR
      IF (NR.GE.32768) INR = NR - 65536
      IF (DATA.LT.32768) IDATA = DATA
      IF (DATA.GE.32768) IDATA = DATA - 65536
C                                        Call FPS routine.
      CALL SEARCH (INF, INT, IMF, IMT, IND, INR, IDATA)
C
 999  RETURN
      END
