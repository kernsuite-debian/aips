      SUBROUTINE PBCALC (ANGLE, LAMBDA, ARRAY, PBPARM, BMFACT, OUTSID)
C-----------------------------------------------------------------------
C! Computes the (single-dish) primary beam power
C# Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2000-2001, 2003, 2007, 2009, 2011, 2016
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
C   Estimates the beam factor
C   Inputs:
C      ANGLE    D      Angle from pointing position (deg)
C      LAMBDA   D      Wavelength
C      ARRAY    C*8    Array name ' ' -> 'VLA'
C   In/out:
C      PBPARM   R(6)   Beam parameters: (1) flag, (2-6) parms
C   Outputs:
C      BMFACT   R      Beam value
C      OUTSID   L      True => outside useful region of beam
C-----------------------------------------------------------------------
      DOUBLE PRECISION ANGLE, LAMBDA
      CHARACTER ARRAY*8
      REAL      PBPARM(6), BMFACT
      LOGICAL   OUTSID
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DPBCALC.INC'
      INTEGER   I, J, K, I1, I2
      DOUBLE PRECISION X, BMULT, PLAMBD, RCUT, FRACT2
      REAL      VLATAB(6,8), VLAFBR(7), TABLE(6,8,2), FBREAK(7,2),
     *   ATTAB(6,8), ATFBR(7), ATCUT(8), VLACUT(8), CUTOFF(8,2),
     *   BMFAC2, EVLABB(9), EVLAFB(48,6), EVLAP(48), EVLAL(48),
     *   EVLAS(48), EVLAC(48), EVLAX(48), EVLAKU(48), EVLTAB(3,48,6),
     *   EVLTAP(3,48), EVLTAL(3,48), EVLTAS(3,48), EVLTAC(3,48),
     *   EVLTAX(3,48), EVLTKU(3,48), EVLTHI(3,3), EPBAND(14)
      INTEGER   NBR(4), JBAND, EVLNFR(6), DONMSG
      LOGICAL   FIRST
      CHARACTER CARRAY(4)*4
      EQUIVALENCE (VLATAB, TABLE(1,1,1))
      EQUIVALENCE (ATTAB,  TABLE(1,1,2))
      EQUIVALENCE (VLAFBR, FBREAK(1,1))
      EQUIVALENCE (ATFBR,  FBREAK(1,2))
      EQUIVALENCE (VLACUT, CUTOFF(1,1))
      EQUIVALENCE (ATCUT,  CUTOFF(1,2))
      EQUIVALENCE (EVLAP(1), EVLAFB(1,1))
      EQUIVALENCE (EVLAL(1), EVLAFB(1,2))
      EQUIVALENCE (EVLAS(1), EVLAFB(1,3))
      EQUIVALENCE (EVLAC(1), EVLAFB(1,4))
      EQUIVALENCE (EVLAX(1), EVLAFB(1,5))
      EQUIVALENCE (EVLAKU(1), EVLAFB(1,6))
      EQUIVALENCE (EVLTAP(1,1), EVLTAB(1,1,1))
      EQUIVALENCE (EVLTAL(1,1), EVLTAB(1,1,2))
      EQUIVALENCE (EVLTAS(1,1), EVLTAB(1,1,3))
      EQUIVALENCE (EVLTAC(1,1), EVLTAB(1,1,4))
      EQUIVALENCE (EVLTAX(1,1), EVLTAB(1,1,5))
      EQUIVALENCE (EVLTKU(1,1), EVLTAB(1,1,6))
      INCLUDE 'INCS:DMSG.INC'
      SAVE PLAMBD, BMULT, RCUT, FRACT2, J, JBAND, DONMSG
      DATA DONMSG /0/
      DATA CARRAY /'VLA', 'ATCA', 'USER', 'EVLA'/
      DATA PLAMBD /0.0D0/
      DATA NBR /7, 4, 0, 9/
      DATA VLAFBR /0.15, 1.1, 2.0, 6.0, 10.0, 18.0, 30./
      DATA VLATAB /-0.897, 2.71 , -0.242, 0.0, 0.0, 0.0,
     *            -0.935,  3.23 , -0.378, 0.0, 0.0, 0.0,
     *            -1.343,  6.579, -1.186, 0.0, 0.0, 0.0,
     *            -1.372,  6.940, -1.309, 0.0, 0.0, 0.0,
     *            -1.306,  6.253, -1.100, 0.0, 0.0, 0.0,
     *            -1.305,  6.155, -1.030, 0.0, 0.0, 0.0,
     *            -1.417,  7.332, -1.352, 0.0, 0.0, 0.0,
     *            -1.321,  6.185, -0.983, 0.0, 0.0, 0.0/
      DATA VLACUT /6300., 3650., 1800., 1800., 1900., 2000., 1900.,
     *   2400./
      DATA ATFBR /2.0, 3.0, 7.3, 11.0, 3*0/
      DATA ATTAB /-1.049,  4.238, -0.8473, 0.09073, -5.004E-3, 1.118E-3,
     *            -0.9942, 3.932, -0.7772, 0.08239, -4.429E-3, 9.899E-4,
     *            -1.075,  4.651, -1.035,  0.12274, -6.125E-3, 0.0,
     *            -0.9778, 3.875, -0.8068, 0.09414, -5.841E-3, 1.499E-3,
     *            -0.9579, 3.228, -0.3807, 0.0,      0.0,      0.0,
     *            18*0.0/
      DATA ATCUT /2500., 2900., 2500., 2750., 2700., 3*0.0/
      DATA EVLABB /0.1, 0.8, 2.0, 4.0, 8.0, 12.0, 18.0, 26.0, 38.0/
      DATA EVLNFR /14, 15, 15, 32, 32, 48/
      DATA EVLAP  /0.232, 0.246, 0.281, 0.296, 0.312, 0.328, 0.344,
     *   0.357, 0.382, 0.392, 0.403, 0.421, 0.458, 0.470, 34*0.0/
      DATA EVLAL /1.040, 1.104, 1.168, 1.232, 1.296, 1.360, 1.424,
     *   1.488, 1.552, 1.680, 1.744, 1.808, 1.872, 1.936, 2.000, 33*0.0/
      DATA EVLAS /2.052, 2.180, 2.436, 2.564, 2.692, 2.820, 2.948,
     *   3.052, 3.180, 3.308, 3.436, 3.564, 3.692, 3.820, 3.948, 33*0.0/
      DATA EVLAC /4.052, 4.180, 4.308, 4.436, 4.564, 4.692, 4.820,
     *   4.948, 5.052, 5.180, 5.308, 5.436, 5.564, 5.692, 5.820, 5.948,
     *   6.052, 6.148, 6.308, 6.436, 6.564, 6.692, 6.820, 6.948, 7.052,
     *   7.180, 7.308, 7.436, 7.564, 7.692, 7.820, 7.948, 16*0.0/
      DATA EVLAX /8.052, 8.180, 8.308, 8.436, 8.564, 8.692, 8.820,
     *   8.948, 9.052, 9.180, 9.308, 9.436, 9.564, 9.692, 9.820, 9.948,
     *   10.052, 10.180, 10.308, 10.436, 10.564, 10.692, 10.820, 10.948,
     *   11.052, 11.180, 11.308, 11.436, 11.564, 11.692, 11.820, 11.948,
     *   16*0.0/
      DATA EVLAKU /12.052, 12.180, 12.308, 12.436, 12.564, 12.692,
     *   12.820, 12.948, 13.052, 13.180, 13.308, 13.436, 13.564, 13.692,
     *   13.820, 13.948, 14.052, 14.180, 14.308, 14.436, 14.564, 14.692,
     *   14.820, 14.948, 15.052, 15.180, 15.308, 15.436, 15.564, 15.692,
     *   15.820, 15.948, 16.052, 16.180, 16.308, 16.436, 16.564, 16.692,
     *   16.820, 16.948, 17.052, 17.180, 17.308, 17.436, 17.564, 17.692,
     *   17.820, 17.948/
      DATA EVLTAP /-1.137, 5.19, -1.04,   -1.130, 5.04, -1.02,
     *   -1.106, 5.11, -1.10,  -1.125, 5.27, -1.14, -1.030, 4.44, -0.89,
     *   -0.980, 4.25, -0.87,  -0.974, 4.09, -0.76, -0.996, 4.23, -0.79,
     *   -1.002, 4.39, -0.88,  -1.067, 5.13, -1.12, -1.057, 4.90, -1.06,
     *   -1.154, 5.85, -1.33,  -0.993, 4.67, -1.04, -1.010, 4.85, -1.07,
     *   102*0.0/
      DATA EPBAND / 0.71, 0.77, 0.91, 0.96, 0.68, 0.69, 0.53, 0.51,
     *   0.64, 0.90, 0.87, 1.08, 0.88, 0.86/
      DATA EVLTAL /-1.529, 8.69, -1.88,  -1.486, 8.15, -1.68,
     *   -1.439, 7.53, -1.45, -1.450, 7.87, -1.63, -1.428, 7.62, -1.54,
     *   -1.449, 8.02, -1.74, -1.462, 8.23, -1.83, -1.455, 7.92, -1.63,
     *   -1.435, 7.54, -1.49, -1.443, 7.74, -1.57, -1.462, 8.02, -1.69,
     *   -1.488, 8.38, -1.83, -1.486, 8.26, -1.75, -1.459, 7.93, -1.62,
     *   -1.508, 8.31, -1.68, 99*0.0/
      DATA EVLTAS / -1.429, 7.52, -1.47, -1.389, 7.06, -1.33,
     *   -1.377, 6.90, -1.27, -1.381, 6.92, -1.26, -1.402, 7.23, -1.40,
     *   -1.433, 7.62, -1.54, -1.433, 7.46, -1.42, -1.467, 8.05, -1.70,
     *   -1.497, 8.38, -1.80, -1.504, 8.37, -1.77, -1.521, 8.63, -1.88,
     *   -1.505, 8.37, -1.75, -1.521, 8.51, -1.79, -1.534, 8.57, -1.77,
     *   -1.516, 8.30, -1.66, 99*0.0/
      DATA EVLTAC / -1.406, 7.41, -1.48, -1.385, 7.09, -1.36,
     *   -1.380, 7.08, -1.37, -1.362, 6.95, -1.35, -1.365, 6.92, -1.31,
     *   -1.339, 6.56, -1.17, -1.371, 7.06, -1.40, -1.358, 6.91, -1.34,
     *   -1.360, 6.91, -1.33, -1.353, 6.74, -1.25, -1.359, 6.82, -1.27,
     *   -1.380, 7.05, -1.37, -1.376, 6.99, -1.31, -1.405, 7.39, -1.47,
     *   -1.394, 7.29, -1.45, -1.428, 7.57, -1.57, -1.445, 7.68, -1.50,
     *   -1.422, 7.38, -1.38, -1.463, 7.94, -1.62, -1.478, 8.22, -1.74,
     *   -1.473, 8.00, -1.62, -1.455, 7.76, -1.53, -1.487, 8.22, -1.72,
     *   -1.472, 8.05, -1.67, -1.470, 8.01, -1.64, -1.503, 8.50, -1.84,
     *   -1.482, 8.19, -1.72, -1.498, 8.22, -1.66, -1.490, 8.18, -1.66,
     *   -1.481, 7.98, -1.56, -1.474, 7.94, -1.57, -1.448, 7.69, -1.51,
     *   48*0.0/
      DATA EVLTAX / -1.403, 7.21, -1.37, -1.398, 7.10, -1.32,
     *   -1.402, 7.16, -1.35, -1.400, 7.12, -1.32, -1.391, 6.95, -1.25,
     *   -1.409, 7.34, -1.49, -1.410, 7.36, -1.45, -1.410, 7.34, -1.43,
     *   -1.403, 7.20, -1.36, -1.396, 7.09, -1.31, -1.432, 7.68, -1.55,
     *   -1.414, 7.43, -1.47, -1.416, 7.45, -1.47, -1.406, 7.26, -1.39,
     *   -1.412, 7.36, -1.43, -1.409, 7.29, -1.39, -1.421, 7.46, -1.45,
     *   -1.409, 7.25, -1.36, -1.402, 7.13, -1.31, -1.399, 7.09, -1.29,
     *   -1.413, 7.37, -1.43, -1.412, 7.34, -1.41, -1.401, 7.12, -1.31,
     *   -1.401, 7.12, -1.31, -1.401, 7.12, -1.31, -1.394, 6.99, -1.24,
     *   -1.394, 7.01, -1.26, -1.391, 6.94, -1.22, -1.389, 6.92, -1.22,
     *   -1.386, 6.80, -1.15, -1.391, 6.88, -1.19, -1.399, 6.97, -1.22,
     *   48*0.0/
      DATA EVLTKU / -1.399, 7.17, -1.33, -1.392, 7.07, -1.31,
     *   -1.393, 7.19, -1.39, -1.393, 7.20, -1.40, -1.395, 7.19, -1.38,
     *   -1.397, 7.20, -1.37, -1.388, 7.06, -1.32, -1.397, 7.18, -1.36,
     *   -1.400, 7.27, -1.40, -1.406, 7.44, -1.50, -1.403, 7.37, -1.47,
     *   -1.392, 7.08, -1.31, -1.384, 6.94, -1.24, -1.382, 6.95, -1.25,
     *   -1.376, 6.88, -1.24, -1.384, 6.98, -1.28, -1.400, 7.36, -1.48,
     *   -1.397, 7.29, -1.45, -1.399, 7.32, -1.45, -1.396, 7.25, -1.42,
     *   -1.393, 7.20, -1.39, -1.384, 7.03, -1.31, -1.388, 7.06, -1.32,
     *   -1.393, 7.16, -1.37, -1.402, 7.38, -1.48, -1.407, 7.47, -1.53,
     *   -1.406, 7.41, -1.48, -1.399, 7.31, -1.44, -1.397, 7.28, -1.43,
     *   -1.401, 7.35, -1.46, -1.402, 7.34, -1.45, -1.399, 7.30, -1.44,
     *   -1.419, 7.59, -1.54, -1.419, 7.59, -1.52, -1.412, 7.40, -1.44,
     *   -1.407, 7.32, -1.40, -1.408, 7.32, -1.41, -1.410, 7.34, -1.40,
     *   -1.407, 7.27, -1.38, -1.423, 7.63, -1.55, -1.437, 7.87, -1.66,
     *   -1.438, 7.84, -1.64, -1.445, 7.98, -1.71, -1.452, 8.10, -1.77,
     *   -1.458, 8.13, -1.70, -1.456, 8.06, -1.72, -1.453, 8.00, -1.68,
     *   -1.452, 7.99, -1.69/
      DATA EVLTHI /-1.434, 7.56, -1.52, -1.430, 7.52, -1.45,
     *    -1.442, 7.59, -1.46/
C-----------------------------------------------------------------------
      BMFACT = 0.0
      PBFREQ = VELITE / LAMBDA / 1.0D9
      FIRST = LAMBDA.NE.PLAMBD
      PLAMBD = LAMBDA
C                                       Normal VLA bands
      IF (FIRST) THEN
C                                       Array
         MSGTXT = ' '
         IF (ARRAY.EQ.'VLA') THEN
            J = 1
         ELSE IF (ARRAY.EQ.'ATCA') THEN
            J = 2
         ELSE IF ((PBPARM(1).GT.0.0) .AND. (PBPARM(2).NE.0.0)) THEN
            J = 3
            RCUT = 1.E9
         ELSE IF ((ARRAY.EQ.' ') .OR. (ARRAY.EQ.'EVLA')) THEN
            J = 4
         ELSE
            MSGTXT = 'PBCALC: EVLA ANTENNA USED FOR ARRAY ' // ARRAY
            CALL MSGWRT (6)
            J = 4
            END IF
         IF (J.NE.DONMSG) THEN
            WRITE (MSGTXT,1000) CARRAY(J)
            CALL MSGWRT (3)
            DONMSG = J
            END IF
         BMULT = PBFREQ * 60.0D0
         JBAND = -1
C                                       which band
         PBFRQ1 = PBFREQ
         PBFRQ2 = 0.0D0
         IF ((J.EQ.1) .OR. (J.EQ.2)) THEN
            DO 10 I = 1,NBR(J)
               IF (PBFREQ.LT.FBREAK(I,J)) GO TO 100
 10            CONTINUE
            I = NBR(J) + 1
C                                       EVLA
         ELSE IF (J.EQ.4) THEN
            DO 20 I = 1,NBR(J)
               IF (PBFREQ.LT.EVLABB(I)) GO TO 25
 20            CONTINUE
            I = NBR(J) + 1
 25         JBAND = I
            K = JBAND - 1
            IF ((K.GE.1) .AND. (K.LE.6)) THEN
               DO 30 I = 1,EVLNFR(K)
                  IF (PBFREQ.LT.EVLAFB(I,K)) GO TO 35
 30               CONTINUE
               I = EVLNFR(K)
 35            I1 = I - 1
               I2 = I
               IF (I1.LT.1) THEN
                  I1 = 1
                  I2 = 2
                  END IF
               PBFRQ1 = EVLAFB(I1,K)
               PBFRQ2 = EVLAFB(I2,K)
               FRACT2 = (PBFREQ - EVLAFB(I1,K)) /
     *            (EVLAFB(I2,K) - EVLAFB(I1,K))
               END IF
            END IF
C                                         fault parameters
 100     IF (J.LT.3) RCUT = CUTOFF(I,J)
         IF ((PBPARM(1).GT.0.0) .AND. (PBPARM(2).NE.0.0)) THEN
            BMPARM(1) = PBPARM(2) * 1.0D-3
            BMPARM(2) = PBPARM(3) * 1.0D-7
            BMPARM(3) = PBPARM(4) * 1.0D-10
            BMPARM(4) = PBPARM(5) * 1.0D-13
            BMPARM(5) = PBPARM(6) * 1.0D-16
            BMPARM(6) = 0.0D0
         ELSE IF (J.EQ.4) THEN
            CALL DFILL (6, 0.0D0, BMPARM)
            CALL DFILL (6, 0.0D0, BMPRM2)
            IF ((K.GE.1) .AND. (K.LE.6)) THEN
               BMPARM(1) = EVLTAB(1,I1,K) * 1.0D-3
               BMPARM(2) = EVLTAB(2,I1,K) * 1.0D-7
               BMPARM(3) = EVLTAB(3,I1,K) * 1.0D-10
               BMPRM2(1) = EVLTAB(1,I2,K) * 1.0D-3
               BMPRM2(2) = EVLTAB(2,I2,K) * 1.0D-7
               BMPRM2(3) = EVLTAB(3,I2,K) * 1.0D-10
C                                       P band extra term
               IF (K.EQ.1) THEN
                  BMPARM(4) = EPBAND(I1) * 1.D-14
                  BMPRM2(4) = EPBAND(I2) * 1.D-14
                  END IF
            ELSE IF (K.GT.6) THEN
               BMPARM(1) = EVLTHI(1,JBAND-7) * 1.0D-3
               BMPARM(2) = EVLTHI(2,JBAND-7) * 1.0D-7
               BMPARM(3) = EVLTHI(3,JBAND-7) * 1.0D-10
               END IF
         ELSE
            BMPARM(1) = TABLE(1,I,J) * 1.0D-3
            BMPARM(2) = TABLE(2,I,J) * 1.0D-7
            BMPARM(3) = TABLE(3,I,J) * 1.0D-10
            BMPARM(4) = TABLE(4,I,J) * 1.0D-13
            BMPARM(5) = TABLE(5,I,J) * 1.0D-16
            BMPARM(6) = TABLE(6,I,J) * 1.0D-20
            END IF
         FIRST = .FALSE.
         PBPARM(1) = 1.0
         PBPARM(2) = BMPARM(1) * 1.0D+3
         PBPARM(3) = BMPARM(2) * 1.0D+7
         PBPARM(4) = BMPARM(3) * 1.0D+10
         PBPARM(5) = BMPARM(4) * 1.0D+13
         PBPARM(6) = BMPARM(5) * 1.0D+16
         END IF
C                                       compute beam value
      X = (ANGLE * BMULT) ** 2
      BMFACT = 1.0 + (BMPARM(1) + (BMPARM(2) + (BMPARM(3) + (BMPARM(4) +
     *   (BMPARM(5) + BMPARM(6) * X) * X) * X) * X) * X) * X
      IF ((J.EQ.4) .AND. (JBAND.GE.2) .AND. (JBAND.LE.7)) THEN
         BMFAC2 =  1.0 + (BMPRM2(1) + (BMPRM2(2) + (BMPRM2(3) +
     *      (BMPRM2(4) + (BMPRM2(5) + BMPRM2(6) * X) * X) * X) * X) * X)
     *      * X
         BMFACT = (1.-FRACT2) * BMFACT + FRACT2 * BMFAC2
         END IF
C                                       is it useful?
      IF (J.EQ.4) THEN
         IF ((JBAND.GE.3) .AND. (JBAND.LE.9)) THEN
            OUTSID = BMFACT.LT.0.05
         ELSE
            OUTSID = BMFACT.LT.0.1
            END IF
      ELSE
         OUTSID = (X.GT.RCUT) .OR. (BMFACT.LT.0.01)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PBCALC using ',A,' beam parameters')
      END
