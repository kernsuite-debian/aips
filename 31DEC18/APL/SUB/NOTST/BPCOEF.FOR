      SUBROUTINE BPCOEF (LBPTYP, CA, CB, INCA, INCB, N, FBLNK, DSHIFT,
     *   SPECA, SPECB, INSA, INSB, ICHAN, NCHAN, A, B, MODE, WAUTO,
     *   DPOLYN, NDIM, NMAX, WALL, IRET)
C----------------------------------------------------------------------
C! Compute a complex polynomial bandpass
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007
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
C   Compute a complex polynomial bandpass.
C   Inputs:
C      LBPTYP  C*8     BP coefficient type (eg CHEB_AP, CHEB_RI).
C      CA      R(*)    First coefficient array.
C      CB      R(*)    Second coefficient array.
C      INCA    I       Increment in CA.
C      INCB    I       Increment in CB.
C      N       I       Max. no. coeff. in CA or CB.
C      FBLNK   R       Blanked value.
C      DSHIFT  D       Shift to be applied before computing BP (chan.).
C      INSA    I       Increment in output spectrum (SPECA).
C      INSB    I       Increment in output spectrum (SPECB).
C      ICHAN   I       Start channel for output spectrum.
C      NCHAN   I       No. of channels in output spectrum.
C      A, B    R       Range over which coeff. are defined (chan.).
C      MODE    I       Output spectrum type (0=> (Re,Im);
C                      1=> original coeff. type).
C      WAUTO   L       True if AC bandpass. Will square the amplitude
C                      in this case to comply with existing BPASS
C                      convention.
C      NDIM    I       Maximum size of DPOLYN.
C      WALL    L       If true then save all polynomial values as a
C                      function of term_no. and channel in DPOLYN,
C                      rather than just those for the last channel.
C   Output:
C      SPECA   R(*)    Output spectrum (first array).
C      SPECB   R(*)    Output spectrum (second array).
C      DPOLYN  D(*,*)  Returned polynomial terms, indexed
C                      on (term_no., channel). See WALL above.
C      NMAX    I       Dimension of first axis in DPOLYN.
C      IRET    I       Return code (0 => ok; else error)
C----------------------------------------------------------------------
      CHARACTER LBPTYP*8
      LOGICAL   WAUTO, WALL
      INTEGER   INCA, INCB, N, INSA, INSB, ICHAN, NCHAN, MODE, NDIM,
     *   NMAX, IRET
      DOUBLE PRECISION DPOLYN(NDIM), DSHIFT
      REAL      CA(N), CB(N), FBLNK, SPECA(NCHAN), SPECB(NCHAN), A, B
C
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   WFNDA, WFNDB, WARN
      CHARACTER LTYPES(2)*8
      DOUBLE PRECISION DA, DB, DXVAL, DSUM1, DSUM2, DTMP
      INTEGER   NA, NB, I, NTYPES, K, KTYP, IA, IB, IAOUT, IBOUT, NSIZ,
     *   M
      SAVE WARN
      DATA LTYPES /'CHEB_AP ', 'CHEB_RI '/
      DATA NTYPES /2/
      DATA WARN /.TRUE./
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Match coeff. type.
      KTYP = 0
      DO 50 I = 1,NTYPES
         IF (LTYPES(I).EQ.LBPTYP) KTYP = I
50       CONTINUE
C                                       Unidentified type.
      IF (KTYP.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT,1050) LBPTYP
         GO TO 990
         END IF
C                                       Determine maximum number of
C                                       coeff. in each array.
      IA = 1
      IB = 1
      WFNDA = .FALSE.
      WFNDB = .FALSE.
      DO 150 I = 1,N
         IF ((CA(IA).EQ.FBLNK) .AND. (.NOT.WFNDA)) THEN
            NA = I - 1
            WFNDA = .TRUE.
            END IF
         IF ((CB(IB).EQ.FBLNK) .AND. (.NOT.WFNDB)) THEN
            NB = I - 1
            WFNDB = .TRUE.
            END IF
C                                       Increment coeff. pointers.
         IF (INCA.GT.0) IA = IA + INCA
         IF (INCB.GT.0) IB = IB + INCB
150      CONTINUE
C
      IF (.NOT.WFNDA) NA = N
      IF (.NOT.WFNDB) NB = N
      IF (INCA.LE.0) NA = 0
      IF (INCB.LE.0) NB = 0
      NMAX = MAX (NA, NB)
C                                       Check size of DPOLYN
      IF (WALL) THEN
         NSIZ = NMAX * NCHAN
      ELSE
         NSIZ = NMAX
         END IF
      IF (NSIZ.GT.NDIM) THEN
         IRET = 3
         WRITE (MSGTXT,1150)
         GO TO 990
         END IF
C                                       Compute output spectrum.
      IAOUT = 1
      IBOUT = 1
      DA = DBLE (A)
      DB = DBLE (B)
C
      DO 500 I = 1,NCHAN
C                                       Add channel shift.
         DXVAL = DBLE(I+ICHAN-1) - DSHIFT
         IF (DXVAL.LT.DA) DXVAL = DB - (DA - DXVAL)
         IF (DXVAL.GT.DB) DXVAL = DA + (DXVAL - DB)
C                                       Compute coefficients.
         M = (I - 1) * NMAX + 1
         IF (.NOT.WALL) M = 1
C                                       Case coeff_type of:
C                                       1,2: Chebyshev.
         IF (((KTYP.EQ.1).OR.(KTYP.EQ.2)).AND.(NMAX.GT.0)) THEN
            CALL CHEBY (DA, DB, DXVAL, DPOLYN(M), NMAX-1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1170) IRET
               GO TO 990
               END IF
            DPOLYN(M) = 0.5 * DPOLYN(M)
            END IF
C                                       Sum over coeff. (array one)
         DSUM1 = 0.0D0
         IA = 1
         DO 200 K = 1,NA
            DSUM1 = DSUM1 + DPOLYN(M+K-1) * CA(IA)
            IA = IA + INCA
200         CONTINUE
C                                       Sum over coeff. (array two)
         DSUM2 = 0.0D0
         IB = 1
         DO 250 K = 1,NB
            DSUM2 = DSUM2 + DPOLYN(M+K-1) * CB(IB)
            IB = IB + INCB
250         CONTINUE
C                                       Convert to (Re,Im) ?
         IF (MODE.EQ.0) THEN
C                                       Chebyshev ampl./phase.
            IF (KTYP.EQ.1) THEN
               DTMP = DSUM1
               DSUM1 = DSUM1 * COS (DSUM2)
               DSUM2 = DTMP * SIN (DSUM2)
               END IF
            END IF
C                                       Fill output spectrum
         IF (NMAX.LE.0) THEN
            SPECA(IAOUT) = FBLNK
            SPECB(IBOUT) = FBLNK
         ELSE
            SPECA(IAOUT) = DSUM1
            SPECB(IBOUT) = DSUM2
C                                       If WAUTO true then square
C                                       output amplitude to conform
C                                       to pre-existing BP table
C                                       convention.
            IF (WAUTO) THEN
C                                       (Re,Im) output
               IF ((MODE.EQ.0) .OR. ((MODE.EQ.1) .AND. (KTYP.EQ.2)))
     *            THEN
                  SPECA(IAOUT) = SPECA(IAOUT) ** 2 +
     *               SPECB(IAOUT) ** 2
C                                       (Amp, phase)
               ELSE
                  SPECA(IAOUT) = SPECA(IAOUT) ** 2
                  END IF
C                                       Check for real AC bandpass
               IF (SPECB(IBOUT).NE.0.0) THEN
                  SPECB(IBOUT) = 0.0
                  IF (WARN) THEN
                     WRITE (MSGTXT,1250)
                     CALL MSGWRT (6)
                     WARN = .FALSE.
                     END IF
                  END IF
               END IF
            END IF
C                                       Increment output pointers.
         IAOUT = IAOUT + INSA
         IBOUT = IBOUT + INSB
500      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (6)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1050  FORMAT ('BPCOEF: BP TYPE ',A8,' NOT RECOGNIZED')
1150  FORMAT ('BPCOEF: ARRAY DPOLYN TOO SMALL: CONTACT AIPS ADMIN.')
1170  FORMAT ('BPCOEF: ERROR',I4,' COMPUTING CHEBYSHEV POLYNOMIALS')
1250  FORMAT ('WARNING: using complex BP to correct AC DATA')
      END
