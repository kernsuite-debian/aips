      SUBROUTINE QPTDIV (C, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS)
C-----------------------------------------------------------------------
C! FPS AP version: Divides UV data by transform of CLEAM point model.
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
C   Divides the model visibility derived from CLEAN components into
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.  The weights of the visibilities
C   are multiplied times  the amplitude of the model visibility.
C     Inputs:
C       C      I Base address of CLEAN components, increment=7.
C                       0 = Amplitude
C                       1 = used (PHASE)
C                       2 = used (REAL)
C                       3 = used (IMAG)
C                       4 = -2 * PI * X
C                       5 = -2 * PI * Y
C                       6 = -2 * PI * Z
C       U      I  Base address of U's, assumed followed by V, W
C       VS     I  Base address of vis rec. (real, imag, wt)
C       INCVS  I  Increment of VS for next visibility
C       INCF   I  Increment of VS for next frequency
C       INCS   I  Increment of VS for next IF (RR of LL)
C       NCOMP  I  Number of CLEAN components.
C       NVIS   I  Number of visibilities.
C       NF     I  Number of frequencies.
C       NS     I  Number of IF (usually 1 or 2)
C     Also uses AP locations 0 and 1 and expects an array
C     beginning in location 2 composed of:
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(0)   - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(NF-2) - 1.0
C-----------------------------------------------------------------------
      INTEGER C, U, VS, INCVS, INCS, INCF, NCOMP, NVIS,  NF, NS
      INTEGER*2 IC, IU, IVS, IINCVS, IINCS, IINCF, INCOMP, INVIS,
     *   INF, INS
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (C.LT.32768) IC = C
      IF (C.GE.32768) IC = C - 65536
      IF (U.LT.32768) IU = U
      IF (U.GE.32768) IU = U - 65536
      IF (VS.LT.32768) IVS = VS
      IF (VS.GE.32768) IVS = VS - 65536
      IINCVS = INCVS
      IINCF = INCF
      IINCS = INCS
      IF (NCOMP.LT.32768) INCOMP = NCOMP
      IF (NCOMP.GE.32768) INCOMP = NCOMP - 65536
      IF (NVIS.LT.32768) INVIS = NVIS
      IF (NVIS.GE.32768) INVIS = NVIS - 65536
      INF = NF
      INS = NS
C                                        Call FPS routine.
      CALL PTDIV (IC, IU, IVS, IINCVS, IINCF, IINCS, INCOMP, INVIS,
     *   INF, INS)
C
 999  RETURN
      END
