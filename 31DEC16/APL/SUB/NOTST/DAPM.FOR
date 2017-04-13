      SUBROUTINE DAPM (DAT, DEQ, DELDAT, DAPRA, DAPDEC, DEQRA, DEQDEC,
     *   NR)
C-----------------------------------------------------------------------
C! Converts apparent to mean positions.
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
C   Given are Julian date and apparent RA and DEC for that date and any
C   mean equinox.  Calculated are mean RA and DEC for that mean equinox.
C   Inputs: DAT     D    Julian date of given app. place
C           DEQ     D    mean equinox wanted
C           DELDAT  D    interpolation interval (days).
C           DAPRA   D    apparent RA
C           DAPDEC  D    apparent DEC
C   Output: DEQRA   D    mean RA
C           DEQDEC  D    mean DEC
C-----------------------------------------------------------------------
      LOGICAL   NR
      DOUBLE PRECISION    DAT, DEQ, DELDAT, DAPRA, DAPDEC, DEQRA, DEQDEC
      INTEGER   IY, ID, IWD, K, I, IM
      DOUBLE PRECISION    DS(4), DPRMAT(3,3), BDNS1(5), BDNS2(5),
     *   BDNDEL(5),
     *   SCRA(4), SCDEC(4), D1OLD, D2OLD, DEQOLD, DYQOLD, PI, TWOPI,
     *   PI2, DYQ, YQ, PSIDEL, SPSI, SEPS, DYQRA, DYQDEC, DELTIM,
     *   DELRA, DELDEC, FACSC, APD, APMD, BPMC, SINRA, COSRA, TANDEC,
     *   DJ, DJP, X
      DATA D1OLD, D2OLD, DEQOLD, DYQOLD /4*0.D0/
C-----------------------------------------------------------------------
      PI = 4.D0 * ATAN(1.D0)
      TWOPI = PI + PI
      PI2 = 0.5D0 * PI
      CALL CLD (DAT, IY, IM, ID, IWD)
      DYQ = IY
      IF (IM.GE.7) DYQ = IY + 1
      YQ = DYQ
      IF ((DYQ.EQ.DYQOLD) .AND. (DEQ.EQ.DEQOLD)) GO TO 10
         DYQOLD = DYQ
         DEQOLD = DEQ
         IF (DYQ.NE.DEQ) CALL DPRE (DYQ, DEQ, DPRMAT)
 10   IF ((DAT.GE.D1OLD) .AND. (DAT.LE.D2OLD)) GO TO 20
         D1OLD = DAT
         D2OLD = DAT + DELDAT
         CALL BDN (D1OLD, YQ, BDNS1, PSIDEL, SPSI, SEPS, NR)
         CALL BDN (D2OLD, YQ, BDNS2, PSIDEL, SPSI, SEPS, NR)
         DO 15 K = 1,5
            BDNDEL(K) = BDNS2(K) - BDNS1(K)
 15         CONTINUE
 20   DYQRA = DAPRA
      DYQDEC = DAPDEC
      DELTIM = (DAT - D1OLD) / DELDAT
      DO 60 I = 1,2
         CALL BSC (DAT, YQ, DYQRA, DYQDEC, SCRA, SCDEC)
         DELRA = BDNS1(5) + DELTIM * BDNDEL(5)
         DELDEC = 0.D0
         DO 30 K = 1,4
            FACSC = BDNS1(K) + DELTIM* BDNDEL(K)
            DS(K) = FACSC
            DELRA = DELRA + FACSC * SCRA(K)
            DELDEC = DELDEC + FACSC * SCDEC(K)
 30         CONTINUE
         DYQRA = DAPRA - DELRA
         DYQDEC = DAPDEC - DELDEC
         APD = DS(1) + DS(4)
         IF (DAPDEC.LT.0D0) GO TO 40
            APMD = APD
            BPMC = DS(2) + DS(3)
            GO TO 50
 40      CONTINUE
            APMD = DS(1) - DS(4)
            BPMC = DS(2) - DS(3)
 50      SINRA = SIN(DAPRA)
         COSRA = COS(DAPRA)
         TANDEC = TAN(DAPDEC)
         DJ = (APD*SINRA + BPMC*COSRA) * (APMD*COSRA - BPMC*SINRA)
         DJP = -.5D0 * (APMD*SINRA + BPMC*COSRA)**2
         DYQRA = DYQRA - DJ * TANDEC**2
         DYQDEC = DYQDEC - DJP * TANDEC
         IF (ABS(DYQDEC).GT.PI2) DYQRA = DYQRA + PI
         DYQRA = MOD (DYQRA, TWOPI)
         DYQDEC = MOD (DYQDEC, TWOPI)
         IF (DYQDEC.GT.PI) DYQDEC = DYQDEC - TWOPI
         X = ABS(DYQDEC)
         IF (X.GT.PI2) X = PI - X
         DYQDEC = X * DYQDEC / ABS(DYQDEC)
 60      CONTINUE
      IF (DYQ.NE.DEQ) GO TO 70
         DEQRA = DYQRA
         DEQDEC = DYQDEC
         GO TO 999
 70   CONTINUE
         CALL DTRC (DPRMAT, DYQRA, DYQDEC, DEQRA, DEQDEC)
C
 999  RETURN
      END
