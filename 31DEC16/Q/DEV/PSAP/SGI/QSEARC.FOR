      SUBROUTINE QSEARC (NF, NT, MF, MT, ND, NR, APIAD, APINTR, APFIN)
C-----------------------------------------------------------------------
C! Pseudo AP routine: VLBI fringe search with FFT.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000
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
C   QSEARC is a routine to do a gridded fringe search by means of
C   padded FFTs.  The number (size) of the FFTs is NF (MT) +
C   NR (MF).
C   Inputs:
C      NF       I  No. frequency channels.
C      NT       I  No. time points.
C      MF       I  No. points for freq. FFT.
C      MT       I  No. points for time FFT.
C      ND       I  No. delay channels to search.
C      NR       I  No. rate channels to search.
C      APIAD    I   Base address input data: 2 * NT * NF words used
C      APINTR   I   Base address work area: 2 * NF * NR used
C      APFIN    I   Base address work areas: 2 * NR * ND + max(MT,MF)
C   Output: (in AP memory)
C      0 => 1 dim. array index for peak.
C      1 => peak amplitude
C      2 => peak phase.
C
C   SGI MP version.  Uses routines from complib.sgimath.
C-----------------------------------------------------------------------
      INTEGER   NF, NT, MF, MT, ND, NR, APIAD, APINTR, APFIN
C
      INTEGER   DDATA, INTER, FINAL, LOC1, MF2, MT2, NN, NR2, ND2, NT2,
     *   LOOP, PEAK, ITEMP, I, PTR
C
      INTEGER   MAXSIZ
      PARAMETER (MAXSIZ = 8192)
      COMPLEX   WORK(MAXSIZ), COEFF(MAXSIZ + 15)
C
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Set pointers.
      MF2 = MF * 2
      MT2 = MT * 2
      NT2 = NT * 2
      NR2 = NR * 2
      ND2 = ND * 2
      DDATA = APIAD
      INTER = APINTR
      FINAL = APFIN
      NN = NR * ND
C                                        Do first XFORM,
      CALL CFFT1DI (MT, COEFF)
C$DOACROSS LOCAL(PTR, ITEMP, I, WORK, LOOP), SHARED(MT, APCORE)
      DO 100 LOOP = 1, NF
         PTR = DDATA + NT2 * (LOOP - 1)
         DO 110 I = 1, MT
            IF (I .LE. NT) THEN
               WORK(I) = CMPLX (APCORE(PTR + 2 * (I - 1) + 1),
     *                          APCORE(PTR + 2 * (I - 1) + 2))
            ELSE
               WORK(I) = CMPLX (0.0, 0.0)
            END IF
  110    CONTINUE
C                                        Do time FFT.
         CALL CFFT1D (+1, MT, WORK, 1, COEFF)
C                                         Save first half
         ITEMP = NR / 2 + 1
         LOC1 = INTER + NR2 * (LOOP  - 1)
         DO 120 I = 1, ITEMP
            APCORE(LOC1 + 2 * (I - 1) + 1) = REAL (WORK(I))
            APCORE(LOC1 + 2 * (I - 1) + 2) = AIMAG (WORK(I))
  120    CONTINUE
C                                         Save second half
         LOC1 = LOC1 + ITEMP * 2
         ITEMP = NR / 2
         DO 130 I = 1, ITEMP
            APCORE(LOC1 + 2 * (I - 1) + 1) = REAL (WORK(MT-NR/2+I))
            APCORE(LOC1 + 2 * (I - 1) + 2) = AIMAG (WORK(MT-NR/2+I))
  130    CONTINUE
  100 CONTINUE
C                                         Second transform loop.
      CALL CFFT1DI (MF, COEFF)
C$DOACROSS LOCAL(PTR, ITEMP, I, WORK, LOOP), SHARED(MF, APCORE)
      DO 200 LOOP = 1, NR
         PTR = INTER + 2 * (LOOP - 1)
         DO 210 I = 1, MF
            IF (I .LE. NF) THEN
               WORK(I) = CMPLX (APCORE(PTR + NR2 * (I - 1) + 1),
     *                          APCORE(PTR + NR2 * (I - 1) + 2))
            ELSE
               WORK(I) = CMPLX (0.0, 0.0)
            END IF
  210    CONTINUE
C                                         Freq. FFT.
         CALL CFFT1D (+1, MF, WORK, 1, COEFF)
C                                         Save first half
         ITEMP = ND / 2 + 1
         LOC1 = FINAL + ND2 * (LOOP - 1)
         DO 220 I = 1, ITEMP
            APCORE(LOC1 + 2 * (I - 1) + 1) = REAL (WORK(I))
            APCORE(LOC1 + 2 * (I - 1) + 2) = AIMAG (WORK(I))
  220    CONTINUE
C                                         Save second half
         LOC1 = LOC1 + ITEMP * 2
         ITEMP = ND / 2
         DO 230 I = 1, ITEMP
            APCORE(LOC1 + 2 * (I - 1) + 1) = REAL (WORK(MF-ND/2+I))
            APCORE(LOC1 + 2 * (I - 1) + 2) = AIMAG (WORK(MF-ND/2+I))
  230    CONTINUE
  200 CONTINUE
C                                         Find max amp.
      CALL QCVMMA (FINAL, 2, 0, NN)
      PEAK = SPAD(15)
C                                         Save index in AP LOC 0 I
      APCORI(1) = SPAD(15)
C                                         Polar conversion.
      PEAK = (PEAK * 2) - 2
      LOC1 = FINAL + PEAK
      CALL QPOLAR (LOC1, 2, 1, 2, 1)
C
 999  RETURN
      END
