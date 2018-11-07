      SUBROUTINE QGASUB (MODL, U, VS, INCVS, INCF, INCS, NCOMP, NVIS,
     *   NF, NS, FLAG)
C-----------------------------------------------------------------------
C! FPS AP version: Subtracts transform of Gaussian model from UV data
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
C   FPS AP version.
C   Subtracts the model visibility derived from Gaussian components from
C   visibility records.  Arbitrary numbers of frequencies and
C   polarizations can be processed.
C     Inputs:
C       MODL  I  Base address of Gaussian components, increment=12
C                  0 = used (model amplitude)
C                  1 = used (PHASE)
C                  2 = used (REAL)
C                  3 = used (IMAG)
C                  4 = -2 * PI * X (radians)
C                  5 = -2 * PI * Y
C                  6 = -2 * PI * Z
C                  7 = A = -((COS(PA)*SIG(MAJ))**2 +
C                          (SIN(PA)*SIG(MIN))**2)
C                  8 = B = -((SIN(PA)*SIG(MAJ))**2 +
C                          (COS(PA)*SIG(MIN))**2)
C                  9 = C = -2*(SIN(PA)*COS(PA)*
C                           (SIG(MAJ)**2 - SIG(MIN)**2)
C                 10 = Amplitude
C                 11 = used (MAMP)
C       U     I  Base address of U's, assumed followed by V, W
C       VS    I  Base address of vis rec. (real, imag, wt)
C       INCVS I  Increment of VS for next visibility
C       INCF  I  Increment of VS for next frequency
C       INCS  I  Increment of VS for next Stokes (RR or LL)
C       NCOMP I  Number of Gaussian components.
C       NVIS  I  Number of visibilities.
C       NF    I  Number of frequencies.
C       NS    I  Number of Stokes (usually 1 or 2)
C       FLAG  I  If FLAG < 0 multiply model vis by i (SQRT(-1))
C     Also uses AP locations 0 and 1 and expects an array of length NS
C     beginning in location 2+NF composed of the correlator factors.
C     Beginning in location 2 should be an array of length NF :
C              Freq(0) / Freq(ref) - 1.0
C              Freq(1) / Freq(0)   - 1.0
C                      .
C                      .
C                      .
C              Freq(NF-1) / Freq(NF-2) - 1.0
C-----------------------------------------------------------------------
      INTEGER MODL, U, VS, INCVS, INCF, INCS, NCOMP, NVIS, NF, NS,
     *   FLAG
      INTEGER*2 IMODL, IU, IVS, IINCVS, IINCF, IINCS, INCOMP, INVIS,
     *   INF, INS, IFLAG
C-----------------------------------------------------------------------
C                                        Convert inputs to unsigned I*2
      IF (MODL.LT.32768) IMODL = MODL
      IF (MODL.GE.32768) IMODL = MODL - 65536
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
      IFLAG = FLAG
C                                        Call FPS routine.
      CALL GASUB (IMODL, IU, IVS, IINCVS, IINCF, IINCS, INCOMP, INVIS,
     *   INF, INS, IFLAG)
C
 999  RETURN
      END
