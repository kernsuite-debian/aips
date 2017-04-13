      SUBROUTINE SMOSP (VIS, IRET)
C-----------------------------------------------------------------------
C! Convolves a spectrum with a tabulated function.
C# UV Spectral Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2009, 2013, 2015
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
C   SMOSP convolves an input spectrum with a convolving look up table
C   established in common.
C   Inputs:
C      VIS      R(*)        Input spectrum, real, imag, weight.
C   Values from commons:
C      SMTAB    R(MAXSMO)   Convolution look-up table
C      SMOOTH   R(3)        (3) = 2*support-radius + 1
C      BCHANS   I           Start channel for smoothing
C      ECHANS   I           Stop channel for smoothing
C   Output:
C      VIS      R(*)        Output convolved spectrum.
C      IRET     I           Return code   0 => OK
C                              >0 => error, terminate.
C-----------------------------------------------------------------------
      REAL      VIS(*)
      INTEGER   IRET
C
      INTEGER   J, J1, J2, L, IOFF, IPOL, NPLZN, KLOCS, IIF, IFRQ,
     *   KPOL, INCPX, INDX, LSPECT, JERR, SUPRAD, INXINC
      REAL      P, Q, R, S, W
      HOLLERITH CATH(256)
      INCLUDE 'INCS:PUVD.INC'
      REAL      TEMP(3,MAXCHA)
      SAVE TEMP
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATH, CATUV)
C-----------------------------------------------------------------------
      IRET = 0
      SUPRAD = (SMOOTH(3) - 0.9) / 2.0
C                                          Set visibility increment
      INCPX = CATUV(KINAX)
      IF (INCPX.EQ.1) INCPX = 3
      LSPECT = ECHANS - BCHANS + 1
      CALL AXEFND (8, 'STOKES  ', CATUV(KIDIM), CATH(KHCTP), KLOCS,
     *   JERR)
      NPLZN = CATUV(KINAX+KLOCS)
      INXINC = KNCF * INCPX
C                                       Loop over IF's
      DO 100 IIF = BIF,EIF
         IOFF = (IIF-1) * KNCIF
C                                       Loop over polzns
         DO 90 IPOL = 1,NPLZN
            KPOL = (IPOL-1) * KNCS
C                                       Copy data to temp array
            INDX = ((IOFF + KPOL) + (BCHANS-1)*KNCF) * INCPX + 1
            DO 10 IFRQ = BCHANS,ECHANS
               TEMP(1,IFRQ) = VIS(INDX)
               TEMP(2,IFRQ) = VIS(INDX+1)
               TEMP(3,IFRQ) = VIS(INDX+2)
               INDX = INDX + INXINC
 10            CONTINUE
C                                          Convolve the data
            INDX = ((IOFF + KPOL) + (BCHAN-1)*KNCF) * INCPX + 1
            DO 30 IFRQ = BCHAN,ECHAN
               IF (TEMP(3,IFRQ).GT.0.0) THEN
                  J1 = MAX (IFRQ - SUPRAD, BCHANS)
                  J2 = MIN (IFRQ + SUPRAD, ECHANS)
                  P = 0.0
                  Q = 0.0
                  S = 0.0
                  R = 0.0
                  W = 0.0
C                                       channel weights not used in
C                                       smoothing values
C                                       (only net weight)
                  DO 20 J = J1,J2
                     IF (TEMP(3,J).GT.0.0) THEN
                        L = ABS(IFRQ-J) + 1
                        W = SMTAB(L)
                        P = W * TEMP(1,J) + P
                        Q = W * TEMP(2,J) + Q
                        R = W + R
                        S = (W * W / TEMP(3,J)) + S
                        END IF
 20                  CONTINUE
                  IF ((R.GT.0.0) .AND. (S.GT.0.0)) THEN
                     VIS(INDX) = P / R
                     VIS(INDX+1) = Q / R
                     VIS(INDX+2) = R * R / S
                  ELSE
                     VIS(INDX) = 0.0
                     VIS(INDX+1) = 0.0
                     VIS(INDX+2) = 0.0
                     END IF
                  END IF
               INDX = INDX + INXINC
 30            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
