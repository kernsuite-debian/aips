      SUBROUTINE POLVIS (VIS, POLPLT, IPOLPT)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2016
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
C   Computes ratio of two polarizations in visibility data.  This
C   assumes that the vis data are either of the RR,LL,RL,LR form or of
C   the I,Q,U,V form and that the standard uv pointers are set.
C   Input:
C      POLPLT   I      Polzn operation:
C                         1 = RL/RR;  2 = LR/RR; 3 = LL/RR;
C                         4 = RL/LL;  5 = LR/LL; 6 = RR/LL
C                         7 = Q/I;    8 = U/I;   9 = V/I
C   Input/Output:
C      VIS      R(*)   Input visibility spectrum, output is the
C                      corrected spectrum.
C   Output:
C      IPOLPT   I      Pointer to output vis array of result polzn
C-----------------------------------------------------------------------
      REAL     VIS(*)
      INTEGER  POLPLT, IPOLPT
C
      INTEGER  IIF, IFRQ, NPTR, DPTR, NUMBIF, NUMFRQ, INPD, INPN, INDEX,
     *   NPTRS(6)
      REAL     DENOM, TEMPRD, TEMPID, TEMPRN, TEMPIN
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA NPTRS /2, 3, 1, 2, 3, 0/
C-----------------------------------------------------------------------
      NUMBIF = EIF - BIF + 1
      NUMFRQ = ECHAN - BCHAN + 1
      IF (INCS.EQ.1) INCS = 3
C                                       pointers
      IF (POLPLT.LE.6) THEN
         IF (POLPLT.LT.1) POLPLT = 3
         DPTR = (POLPLT-1) / 3
         NPTR = NPTRS(POLPLT)
      ELSE
         DPTR = 0
         NPTR = POLPLT - 6
         END IF
C                                       Set up output pointer
      IPOLPT = NPTR
C                                       Do the operation
      DO 20 IIF = 1,NUMBIF
         INDEX = 1 + (IIF-1) * INCIF - INCF
         DO 10 IFRQ = 1,NUMFRQ
            INDEX = INDEX + INCF
            INPD = INDEX + DPTR * INCS
            INPN = INDEX + NPTR * INCS
            IF ((VIS(INPD+2).GT.0.0) .AND. (VIS(INPN+2).GT.0.0)) THEN
               TEMPRD = VIS(INPD)
               TEMPID = VIS(INPD+1)
               TEMPRN = VIS(INPN)
               TEMPIN = VIS(INPN+1)
               DENOM = TEMPRD * TEMPRD + TEMPID * TEMPID
               IF (DENOM.EQ.0.0) DENOM = 1.0
               VIS(INPN) = (TEMPRN * TEMPRD + TEMPIN * TEMPID) / DENOM
               VIS(INPN+1) = (TEMPRD * TEMPIN - TEMPID * TEMPRN) / DENOM
            ELSE
               VIS(INPN+2) = -ABS(VIS(INPN+2))
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
