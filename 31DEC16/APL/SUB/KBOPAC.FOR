      SUBROUTINE KBOPAC (NIF, FREQS, OPACKB, ELEV, ZOPAC, COPACS)
C-----------------------------------------------------------------------
C! Calculate opacity multipliers vs FREQ based on K-Band opacity
C# UV Calibration VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 2010, 2012
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
C   Routine to calculate estimates of opacity based on K-Band opacity
C   Function derived by Josh Marvil and Fraser Owen
C   Inputs
C      NIF      I      Number frequencies
C      FREQS    D(*)   Frequencies in Hz
C      OPACKB   R      K-band zenith opacity from standard models
C      ELEV     R      Elevation
C   Outputs:
C      ZOPAC    R(*)   Zenith opacity for each IF
C      COPACS   R(*)   Multipliers for each IF at this elevation
C-----------------------------------------------------------------------
      INTEGER   NIF
      REAL      OPACKB, ELEV, ZOPAC(*), COPACS(*)
      DOUBLE PRECISION FREQS(*)
C
      INTEGER   I, J
      REAL      Z1, Z2, TAUFUN(2,197), PWV, D
C                                       values for 1.0 - 50.0 GHz
C                                       interval of 0.25 GHz
      DATA TAUFUN /
     *      5.272, 0.000,   5.464, 0.000,   5.579, 0.001,
     *      5.654, 0.002,   5.707, 0.003,   5.748, 0.005,
     *      5.780, 0.006,   5.808, 0.008,   5.832, 0.009,
     *      5.855, 0.011,   5.876, 0.013,   5.897, 0.016,
     *      5.918, 0.018,   5.939, 0.021,   5.960, 0.023,
     *      5.981, 0.026,   6.003, 0.030,   6.026, 0.033,
     *      6.049, 0.037,   6.073, 0.040,   6.098, 0.044,
     *      6.123, 0.048,   6.150, 0.052,   6.178, 0.057,
     *      6.206, 0.061,   6.235, 0.066,   6.266, 0.071,
     *      6.297, 0.077,   6.330, 0.082,   6.364, 0.088,
     *      6.398, 0.094,   6.435, 0.100,   6.473, 0.107,
     *      6.522, 0.114,   6.550, 0.122,   6.590, 0.129,
     *      6.635, 0.137,   6.726, 0.146,   6.722, 0.155,
     *      6.766, 0.164,   6.835, 0.174,   6.864, 0.185,
     *      6.905, 0.196,   6.954, 0.207,   7.003, 0.220,
     *      7.055, 0.233,   7.109, 0.247,   7.164, 0.262,
     *      7.220, 0.277,   7.277, 0.294,   7.336, 0.312,
     *      7.396, 0.331,   7.458, 0.352,   7.521, 0.374,
     *      7.586, 0.398,   7.661, 0.423,   7.726, 0.451,
     *      7.785, 0.482,   7.854, 0.516,   7.924, 0.553,
     *      8.000, 0.593,   8.081, 0.638,   8.144, 0.688,
     *      8.219, 0.744,   8.296, 0.806,   8.375, 0.876,
     *      8.456, 0.955,   8.540, 1.045,   8.634, 1.148,
     *      8.724, 1.265,   8.819, 1.400,   8.920, 1.557,
     *      9.028, 1.738,   9.147, 1.949,   9.281, 2.195,
     *      9.435, 2.483,   9.615, 2.820,   9.830, 3.214,
     *     10.090, 3.670,  10.404, 4.194,  10.776, 4.786,
     *     11.204, 5.436,  11.669, 6.117,  12.129, 6.778,
     *     12.523, 7.328,  12.782, 7.638,  12.872, 7.625,
     *     12.815, 7.351,  12.667, 6.924,  12.491, 6.426,
     *     12.335, 5.914,  12.256, 5.420,  12.146, 4.961,
     *     12.134, 4.546,  12.171, 4.176,  12.249, 3.849,
     *     12.362, 3.562,  12.503, 3.312,  12.689, 3.093,
     *     12.859, 2.902,  13.010, 2.735,  13.210, 2.589,
     *     13.419, 2.461,  13.638, 2.349,  13.864, 2.250,
     *     14.097, 2.163,  14.342, 2.087,  14.586, 2.019,
     *     14.838, 1.960,  15.098, 1.907,  15.367, 1.861,
     *     15.653, 1.820,  16.030, 1.784,  16.215, 1.752,
     *     16.506, 1.724,  16.820, 1.700,  17.345, 1.679,
     *     17.567, 1.660,  17.879, 1.644,  18.096, 1.630,
     *     18.423, 1.619,  18.769, 1.609,  19.127, 1.601,
     *     19.495, 1.595,  19.871, 1.590,  20.262, 1.587,
     *     20.663, 1.585,  21.078, 1.584,  21.501, 1.584,
     *     21.939, 1.585,  22.390, 1.587,  22.853, 1.590,
     *     23.256, 1.594,  23.747, 1.598,  24.254, 1.604,
     *     24.777, 1.610,  25.317, 1.616,  25.875, 1.624,
     *     26.451, 1.631,  27.058, 1.640,  26.713, 1.649,
     *     27.065, 1.658,  27.674, 1.668,  28.319, 1.678,
     *     28.991, 1.689,  29.692, 1.700,  30.431, 1.712,
     *     31.355, 1.724,  31.152, 1.736,  31.939, 1.749,
     *     32.756, 1.762,  33.606, 1.776,  34.491, 1.789,
     *     35.414, 1.803,  36.384, 1.818,  37.363, 1.832,
     *     38.973, 1.847,  40.074, 1.862,  41.222, 1.878,
     *     42.420, 1.894,  43.671, 1.910,  44.978, 1.926,
     *     46.345, 1.942,  47.775, 1.959,  52.349, 1.976,
     *     53.994, 1.993,  55.726, 2.011,  57.631, 2.028,
     *     59.460, 2.046,  61.421, 2.064,  63.577, 2.083,
     *     65.854, 2.101,  68.610, 2.120,  71.062, 2.139,
     *     73.653, 2.158,  76.416, 2.178,  79.303, 2.197,
     *     82.334, 2.217,  85.573, 2.237,  89.014, 2.257,
     *     92.743, 2.277,  96.640, 2.298, 100.796, 2.319,
     *    105.234, 2.340, 109.982, 2.361, 115.072, 2.382,
     *    120.538, 2.404, 126.423, 2.426, 132.773, 2.448,
     *    139.643, 2.470, 147.095, 2.492, 155.207, 2.515,
     *    164.067, 2.538, 173.788, 2.561, 184.509, 2.584,
     *    196.415, 2.608, 209.874, 2.632/
C-----------------------------------------------------------------------
      PWV = -1.710 + 136.47 * OPACKB
      DO 20 I = 1,NIF
         J = FREQS(I)/0.25D9 + 0.01
         J = J - 3
         J = MAX (1, MIN (196, J))
         D = FREQS(I)/0.25D9 - (J+3)
         Z1 = TAUFUN(1,J) + TAUFUN(2,J) * PWV
         Z2 = TAUFUN(1,J+1) + TAUFUN(2,J+1) * PWV
         ZOPAC(I) = (Z1 + D * (Z2 - Z1)) / 1000.0
         COPACS(I) = ZOPAC(I) / SIN (ELEV)
         COPACS(I) = SQRT (EXP (COPACS(I)))
 20      CONTINUE
C
 999  RETURN
      END
