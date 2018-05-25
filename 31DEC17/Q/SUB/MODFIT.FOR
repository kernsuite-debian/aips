      SUBROUTINE MODFIT (IFIELD, MF, MC, ISUBG, ICLNG, LLREC, INMET,
     *   OUTMET, FACETS, NF, NC, NW)
C-----------------------------------------------------------------------
C! determines how much AP available for models
C# AP-util Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2008-2010, 2013
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
C   MODFIT determines how many models can be done at one time.  If all
C   facets for one or more frequency groups can be done, that mode will
C   Be selected.  Otherwise, if all frequency (groups) can be done for 1
C   or more facets, that mode will be selected.  It returns the actual
C   facets to use.
C   Inputs:
C      IFIELD   I      Start field number
C      MF       I      Max field number must be >= IFIELD
C      MC       I      Number frequency groups per field
C      ISUBG    I(*)   Start comp for next subtraction
C      ICLNG    I(*)   Last comp for subtraction
C      LLREC    I      length of uncompressed UV data record
C      INMET    C*4    Subtraction method - user specified
C   Outputs:
C      OUTMET   C*4    Subtraction method to be used: = INMET except
C                         if INMET = ' '
C      FACETS   I(*)   Facet numbers to use
C      NF       I      Can do NF full facets (if > 1)
C      NC       I      With NC channels (NF > 1, NC = MC, else may do
C                         only some of the channels in a single facet
C      NW       I      Total words in grids (>= number for 1st
C                         facet/chan)
C-----------------------------------------------------------------------
      INTEGER   IFIELD, MF, MC, ISUBG(*), ICLNG(*), LLREC, FACETS(*),
     *   NF, NC, NW
      CHARACTER INMET*(*), OUTMET*(*)
C
      INTEGER   APSIZE, EXCESS, LFIELD, NX, NY, ONEM, ONEF, NW0
      LOGICAL   DODFT
      REAL      TIMDFT, TIMFFT
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NCC(MAXFLD), NXX(MAXFLD), NYY(MAXFLD), I4CHAN, I4FLD,
     *   SCOMP, MNY
      LOGICAL   LFORCE, FORCED
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMOD.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      SCOMP = 0
      DO 10 LFIELD = IFIELD,MF
         NCC(LFIELD-IFIELD+1) = ICLNG(LFIELD) - ISUBG(LFIELD) + 1
         NXX(LFIELD-IFIELD+1) = FLDSZ(1,LFIELD)
         NYY(LFIELD-IFIELD+1) = FLDSZ(2,LFIELD)
         SCOMP = SCOMP + NCC(LFIELD-IFIELD+1)
 10      CONTINUE
      IF ((INMET.NE.'DFT') .AND. (INMET.NE.'GRID')) THEN
         I4CHAN = 1
         I4FLD = MF - IFIELD + 1
         LFORCE = FORCED (MFIELD, FLDSZ(1,IFIELD))
         CALL QMTYP (I4FLD, NCC, NVIS, I4CHAN, NXX, NYY, TIMDFT, TIMFFT)
         DODFT = (TIMDFT.LT.1.1*TIMFFT) .OR. LFORCE
         OUTMET = 'GRID'
         IF (DODFT) OUTMET = 'FFT'
      ELSE
         OUTMET = INMET
         END IF
      APSIZE = KAPWRD * 1024
      NW0 = 200 * 12 + 256 * (LLREC + 8) + 120 + MC + 4096
      EXCESS = APSIZE - NW0
C                                       GRIDded method
      IF (OUTMET.EQ.'GRID') THEN
C                                       all facet, nchan
         NW = 0
         NF = 0
         MNY = 0
         LFIELD = IFIELD - 1
 20      LFIELD = LFIELD + 1
         IF (LFIELD.LE.MF) THEN
            IF (ICLNG(LFIELD).GE.ISUBG(LFIELD)) THEN
               NX = ABS (FLDSZ(1,LFIELD) * 2.0)
               NY = ABS (FLDSZ(2,LFIELD) * 2.0)
               MNY = MAX (MNY, NY)
               ONEM = (NX + 24) * NY
               NW = NW + ONEM
               IF (NW.GT.EXCESS) GO TO 50
               NF = NF + 1
               FACETS(NF) = LFIELD
               END IF
            GO TO 20
            END IF
C                                       so all facets fit
C                                       number channels
         NW0 = NW0 + 5 * MNY
         EXCESS = APSIZE - NW0
         IF (NW.GT.0) NC = EXCESS / NW
         NC = MIN (NC, MC)
         NW = NC * NW + NW0 + 1024
         GO TO 999
C                                       all chan at once method
 50      LFIELD = IFIELD - 1
         NW = 0
         NF = 0
         NC = 0
         FACETS(1) = 0
 60      LFIELD = LFIELD + 1
         IF ((LFIELD.LE.MF) .AND. (ICLNG(LFIELD).GE.ISUBG(LFIELD))) THEN
            NX = ABS (FLDSZ(1,LFIELD) * 2.0)
            NY = ABS (FLDSZ(2,LFIELD) * 2.0)
            ONEM = (NX + 24) * NY
            ONEF = ONEM * MAX (MC, 1)
C                                       next facet fits fine
            IF (ONEF.LT.EXCESS-5*NY) THEN
               EXCESS = EXCESS - ONEF
               NW = NW + ONEF
               NC = MAX (1, MC)
               NF = NF + 1
               FACETS(NF) = LFIELD
               IF (NF.LT.MAXMOD) GO TO 60

C                                       first facet too much
            ELSE IF ((IFIELD.EQ.LFIELD) .AND. (ONEM.LT.EXCESS)) THEN
               NC = EXCESS / ONEM
               NW = NC + ONEM
               NF = 1
               FACETS(NF) = LFIELD
               END IF
            END IF
         NW = NW + NW0 + 5 * NY + 1024
C                                       DFT subtraction
      ELSE
C                                       assume Gaussians (worst case)
         SCOMP = SCOMP * 7
C                                       we must try to do all fields
C                                       can we do > 1 channel
         NW = 0
         NF = 0
         DO 110 LFIELD = IFIELD,MF
            FACETS(LFIELD-IFIELD+1) = LFIELD
 110        CONTINUE
         NC = EXCESS / SCOMP
         IF (NC.GT.0) NF = MF - IFIELD + 1
         NC = MIN (NC, MC)
         NW = NW0 + NC * SCOMP
         END IF
C
 999  RETURN
      END

