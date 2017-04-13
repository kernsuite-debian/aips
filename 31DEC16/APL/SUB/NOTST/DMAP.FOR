      SUBROUTINE DMAP (DAT, DEQ, DELDAT, DEQRA, DEQDEC, DAPRA, DAPDEC,
     *   NR)
C-----------------------------------------------------------------------
C! Compute apparent position from mean position
C# Math Coordinates
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
C   Given are the mean equinox, mean position, and any Julian date.
C   Calculated is the apparent position for that Julian date.
C   Inputs: DAT    D    Julian date of the apparent place wanted
C           DEQ    D    mean equinox of given mean place
C           DELDAT D    interpolation interval (days)
C           DEQRA  D    given mean RA
C           DEQDEC D    given mean DEC
C   Output: DAPRA  D    apparent RA
C           DAPDEC D    apparent DEC
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DAT, DEQ, DELDAT, DEQRA, DEQDEC, DAPRA, DAPDEC
      LOGICAL   NR
      INTEGER   IY, IM, ID, IWD, K
      DOUBLE PRECISION DS(4), DPRMAT(3,3), BDNS1(5), BDNS2(5),
     *   BDNDEL(5),
     *   SCRA(4), SCDEC(4), D1OLD, D2OLD, DEQOLD, DYQOLD, PI, TWOPI,
     *   PI2, DYQ, YQ, DYQRA, DYQDEC, PSIDEL, SPSI, SEPS, DELTIM, FACSC,
     *   APD, APMD, BPMC, SINRA, COSRA, TANDEC, DJ, DJP, X
      DATA D1OLD, D2OLD, DEQOLD, DYQOLD /4*0.D0/
C-----------------------------------------------------------------------
      PI = 4.D0 * ATAN(1.D0)
      TWOPI = PI + PI
      PI2 = .5D0 * PI
      CALL CLD (DAT, IY, IM, ID, IWD)
      DYQ = IY
      IF (IM.GE.7) DYQ = IY + 1
      YQ = DYQ
      IF ((DYQ.EQ.DYQOLD) .AND. (DEQ.EQ.DEQOLD)) GO TO 20
         DYQOLD = DYQ
         DEQOLD = DEQ
         IF (DYQ.NE.DEQ) CALL DPRE (DEQ, DYQ, DPRMAT)
 20   IF (DYQ.NE.DEQ) GO TO 30
         DYQRA = DEQRA
         DYQDEC = DEQDEC
         GO TO 40
 30   CONTINUE
         CALL DTRC (DPRMAT, DEQRA, DEQDEC, DYQRA, DYQDEC)
 40   IF ((DAT.GE.D1OLD) .AND. (DAT.LE.D2OLD)) GO TO 50
         D1OLD = DAT
         D2OLD = DAT + DELDAT
         CALL BDN (D1OLD, YQ, BDNS1, PSIDEL, SPSI, SEPS, NR)
         CALL BDN (D2OLD, YQ, BDNS2, PSIDEL, SPSI, SEPS, NR)
         DO 45 K = 1,5
            BDNDEL(K) = BDNS2(K) - BDNS1(K)
 45         CONTINUE
 50   CALL BSC (DAT, YQ, DYQRA, DYQDEC, SCRA, SCDEC)
      DELTIM = (DAT- D1OLD) / DELDAT
      DAPRA = DYQRA + BDNS1(5) + DELTIM * BDNDEL(5)
      DAPDEC = DYQDEC
      DO 60 K = 1,4
         FACSC = BDNS1(K) + DELTIM * BDNDEL(K)
         DS(K) = FACSC
         DAPRA = DAPRA + FACSC * SCRA(K)
         DAPDEC = DAPDEC + FACSC * SCDEC(K)
 60      CONTINUE
      APD = DS(1) + DS(4)
      IF (DYQDEC.LT.0D0) GO TO 70
         APMD = APD
         BPMC = DS(2) + DS(3)
         GO TO 80
 70   CONTINUE
         APMD = DS(1) - DS(4)
         BPMC = DS(2) - DS(3)
 80   SINRA = SIN (DYQRA)
      COSRA = COS (DYQRA)
      TANDEC = TAN (DYQDEC)
      DJ = (APD*SINRA + BPMC*COSRA) * (APMD*COSRA - BPMC*SINRA)
      DJP = -.5D0 * (APMD*SINRA + BPMC*COSRA) ** 2
      DAPRA = DAPRA + DJ * TANDEC ** 2
      DAPDEC = DAPDEC + DJP * TANDEC
      IF (ABS(DAPDEC).GT.PI2) DAPRA = DAPRA + PI
      DAPDEC = MOD (DAPDEC, TWOPI)
      IF (DAPDEC.GT.PI) DAPDEC = DAPDEC - TWOPI
      X = ABS(DAPDEC)
      IF (X.GT.PI2) X = PI - X
      DAPDEC = X * DAPDEC / ABS(DAPDEC)
      DAPRA = MOD (DAPRA, TWOPI)
C
 999  RETURN
      END
