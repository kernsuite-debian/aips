      SUBROUTINE UVMTYP (IFIELD, METHOD, NCHAN, DODFT, TIMDFT, TIMFFT)
C-----------------------------------------------------------------------
C! Determines relative CPU times for DFT or gridded interpolation.
C# AP-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2004, 2015
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
C   UVMTYP decides whether it is faster to do the DFT or FFT model
C   subtraction.  Calls a Q (QMTYP) routine to allow this decision to
C   be made on a machine dependent basis.
C   Input:
C      IFIELD   I    Field number; 0 -> all
C      METHOD   I    1=>gridded, -1=>DFT, 0=>chose.
C      NCHAN    I    Number of frequency channels to subtract.
C   Output:
C      DODFT    L    If true use DFT method.
C      TIMDFT   R    Estimate of time for DFT in sec.
C      TIMFFT   R    Estimate of time for FFT/grid. interpolation.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, METHOD, NCHAN
      LOGICAL   DODFT
      REAL      TIMDFT, TIMFFT
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NCC(MAXFLD), NXX(MAXFLD), NYY(MAXFLD), I4CHAN, I4FLD, I
      LOGICAL   LFORCE, FORCED
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      TIMDFT = 0.0
      TIMFFT = 0.0
C                                       Check if user specified.
      DODFT = METHOD.LT.0
C                                       Setup for QMTYP
      IF (METHOD.EQ.0) THEN
         I4CHAN = NCHAN
         IF (IFIELD.LE.0) THEN
            I4FLD = MFIELD
            DO 10 I = 1,MFIELD
               NCC(I) = NCLNG(I) - NSUBG(I) + 1
               NXX(I) = FLDSZ(1,I)
               NYY(I) = FLDSZ(2,I)
 10            CONTINUE
            LFORCE = FORCED (MFIELD, FLDSZ(1,1))
         ELSE
            I4FLD = 1
            NCC(1) = NCLNG(IFIELD) - NSUBG(IFIELD) + 1
            NXX(1) = FLDSZ(1,IFIELD)
            NYY(1) = FLDSZ(2,IFIELD)
            LFORCE = FORCED (1, FLDSZ(1,IFIELD))
            END IF
C                                       Compute times.
         CALL QMTYP (I4FLD, NCC, NVIS, I4CHAN, NXX, NYY, TIMDFT, TIMFFT)
C                                       See which one is faster.
         DODFT = (TIMDFT.LT.1.1*TIMFFT) .OR. LFORCE
         END IF
C
 999  RETURN
      END
