      SUBROUTINE AVGCHN (VIS, NUMPOL, BCHAN, ECHAN, BIF, EIF, CHNSEL,
     *   JNCS, JNCIF, VISOUT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Routine to average a spectrum in frequency to produce a so-called
C   pseudo-continuum channel. The CHNSEL array is used to specify which
C   channels are required in the average.  It must be okay - it is not
C   checked here.
C   Inputs:
C      VIS      R(*)      Array containing the input visibility data
C                         (Re, Im, Wt) includes only BIF-EIF,
C                         BCHAN-ECHAN
C      NUMPOL   I         Number of polarizations
C      BCHAN    I         Start channel to accept
C      ECHAN    I         Final channel to accept
C      BIF      I         Start IF to accept
C      EIF      I         End IF to accept
C      CHNSEL   I(3,20,*) Array of start,stop,incr channel numbers by IF
C                         channel numbers in this array are absolute
C                         numbers i.e. not relative to BCHAN. BIF
C      JNCS     I         Index for polzn in output array
C      JNCIF    I         Index for IF in output array
C   Output:
C      VISOUT   R(*)      Pseudo-continuum visibility
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, BCHAN, ECHAN, BIF, EIF, CHNSEL(3,20,*),
     *   JNCS, JNCIF
      REAL      VIS(*), VISOUT(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPS, LOOPIF, LOOPF, INDEX, INP, OUTDEX
      REAL      SUMWT, SUMRE, SUMIM, WT, XNORM, CHWT
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       Select/average
      DO 40 LOOPS = 1,NUMPOL
         DO 30 LOOPIF = BIF,EIF
            SUMWT = 0.0
            SUMRE = 0.0
            SUMIM = 0.0
            INDEX = 1 + (LOOPS-1)*INCS + (LOOPIF-BIF)*INCIF
            OUTDEX = 1 + (LOOPS-1)*JNCS + (LOOPIF-BIF)*JNCIF
            DO 20 LOOPF = BCHAN,ECHAN
               CHWT = 1.0
               CALL WANTCH (CHNSEL(1,1,LOOPIF), LOOPF, CHWT)
               INP = INDEX + (LOOPF-BCHAN)*INCF
               WT = VIS(INP+2)
               IF ((WT.GT.0.0) .AND. (CHWT.GT.0.0)) THEN
                  SUMRE = SUMRE + VIS(INP)*WT
                  SUMIM = SUMIM + VIS(INP+1)*WT
                  SUMWT = SUMWT + WT
                  END IF
 20            CONTINUE
            XNORM = 1.0
            IF (SUMWT.GT.1.0E-10) XNORM = 1.0 / SUMWT
            VISOUT(OUTDEX) = SUMRE * XNORM
            VISOUT(OUTDEX+1) = SUMIM * XNORM
            VISOUT(OUTDEX+2) = SUMWT
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END

