      SUBROUTINE AVGSMO (VIS, NUMPOL, NUMIF, NUMFRQ, NCHAVG, CHINC,
     *   JNCS, JNCIF, VISOUT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
C   Routine to boxcar average a spectrum in frequency to produce a
C   lower resolution spectrum.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt)
C      NUMPOL   I         Number of polarizations
C      NUMIF    I         Number of IF's
C      NUMFRQ   I         Number of spectral channels
C      NCHAVG   I         Number channels to average
C      CHINC    I         Increment in input channels between output
C      JNCS     I         Index for polzn in output array
C      JNCIF    I         Index for IF in output array
C   Output:
C      VISOUT   R(*)      Pseudo-continuum visibility
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, NUMIF, NUMFRQ, NCHAVG, CHINC, JNCS, JNCIF
      REAL      VIS(*), VISOUT(*)
C
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, I, OUTDEX, LOOPOF, NN
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Average
      DO 40 LOOPS = 1,NUMPOL
         DO 30 LOOPIF = 1,NUMIF
            LOOPOF = 0
            DO 20 LOOPF = 1,NUMFRQ,CHINC
               LOOPOF = LOOPOF + 1
               SUMWT = 0.0
               SUMRE = 0.0
               SUMIM = 0.0
               INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-1)*INCIF +
     *            (LOOPF-1) * INCF
               OUTDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-1)*JNCIF +
     *            (LOOPOF-1) * INCF
               NN = MIN (NCHAVG, NUMFRQ+1-LOOPF)
               DO 10 I = 1,NN
                  WT = VIS(INDEX+2)
                  IF (WT.GT.0.0) THEN
                     SUMRE = SUMRE + VIS(INDEX)*WT
                     SUMIM = SUMIM + VIS(INDEX+1)*WT
                     SUMWT = SUMWT + WT
                     END IF
                  INDEX = INDEX + INCF
 10               CONTINUE
               XNORM = 1.0
               IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
               VISOUT(OUTDEX) = SUMRE * XNORM
               VISOUT(OUTDEX+1) = SUMIM * XNORM
               VISOUT(OUTDEX+2) = SUMWT
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END

