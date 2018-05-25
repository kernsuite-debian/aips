      SUBROUTINE SPHFN (IALF, IM, IFLAG, ETA, PSI, IERR)
C-----------------------------------------------------------------------
C! Evaluate rational approx. to selected spheriodial functions.
C# Math
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
C   SPHFN is a subroutine to evaluate rational approximations to
C   selected zero-order spheroidal functions, psi(c,eta), which are, in
C   a sense defined in VLA Scientific Memorandum No. 132, optimal for
C   gridding interferometer data.  The approximations are taken from
C   VLA Computer Memorandum No. 156.  The parameter c is related to the
C   support width, m, of the convoluting function according to
C   c=pi*m/2.  The parameter alpha determines a weight function in the
C   definition of the criterion by which the function is optimal.
C   SPHFN incorporates approximations to 25 of the spheroidal func-
C   tions, corresponding to 5 choices of m (4, 5, 6, 7, or 8 cells)
C   and 5 choices of the weighting exponent (0, 1/2, 1, 3/2, or 2).
C   Inputs:
C      IALF     I           Selects the weighting exponent, alpha
C                           (IALF = 1, 2, 3, 4, and 5 correspond to
C                           alpha = 0, 1/2, 1, 3/2, and 2, resp.).
C      IM       I           Selects the support width m, (=IM) and,
C                           correspondingly, the parameter c of the
C                           spheroidal function (only the choices 4,
C                           5, 6, 7, and 8 are allowed).
C      IFLAG    I           Chooses whether the spheroidal function
C                           itself, or its Fourier transform, is to be
C                           approximated.  The latter is appropriate
C                           for gridding, and the former for the u-v
C                           plane convolution.  The two differ by a
C                           factor (1-eta**2)**alpha.  IFLAG less than
C                           or equal to zero chooses the function
C                           appropriate for gridding, and IFLAG positive
C                           chooses its Fourier transform.
C      ETA      R           Eta, as the argument of the spheroidal
C                           function, is a variable which ranges from 0
C                           at the center of the convoluting function to
C                           1 at its edge (also from 0 at the center of
C                           the gridding correction function to unity at
C                           the edge of the map).
C   Output:
C      PSI      R           function value which, on entry to the
C                           subroutine, was to have been computed.
C      IERR     I           Error return code:
C                              0  =>  No error
C                              1  =>  IALF out of range
C                              2  =>  IM out of range
C                              3  =>  ABS(ETA).GT.1
C                             12  =>  IALF and IM both out of range
C                             13  =>  IALF and ETA both illegal
C                             23  =>  IM and ETA both illegal
C                            123  =>  IALF, IM, and ETA all illegal
C-----------------------------------------------------------------------
      INTEGER   IALF, IM, IFLAG, IERR
      REAL      ETA, PSI
      INTEGER   J, K
      REAL      ALPHA(5), ETA2, X, P4(5,5), Q4(2,5), P5(7,5), Q5(5),
     *   P6L(5,5), Q6L(2,5), P6U(5,5), Q6U(2,5), P7L(5,5), Q7L(2,5),
     *   P7U(5,5), Q7U(2,5), P8L(6,5), Q8L(2,5), P8U(6,5), Q8U(2,5)
      INCLUDE 'INCS:DMSG.INC'
      DATA ALPHA /0.0, 0.5, 1.0, 1.5, 2.0/
      DATA P4 /
     *    1.584774E-2, -1.269612E-1,  2.333851E-1, -1.636744E-1,
     *    5.014648E-2,  3.101855E-2, -1.641253E-1,  2.385500E-1,
     *   -1.417069E-1,  3.773226E-2,  5.007900E-2, -1.971357E-1,
     *    2.363775E-1, -1.215569E-1,  2.853104E-2,  7.201260E-2,
     *   -2.251580E-1,  2.293715E-1, -1.038359E-1,  2.174211E-2,
     *    9.585932E-2, -2.481381E-1,  2.194469E-1, -8.862132E-2,
     *    1.672243E-2 /
      DATA Q4 /
     *    4.845581E-1,  7.457381E-2,  4.514531E-1,  6.458640E-2,
     *    4.228767E-1,  5.655715E-2,  3.978515E-1,  4.997164E-2,
     *    3.756999E-1,  4.448800E-2 /
      DATA P5 /
     *    3.722238E-3, -4.991683E-2,  1.658905E-1, -2.387240E-1,
     *    1.877469E-1, -8.159855E-2,  3.051959E-2,  8.182649E-3,
     *   -7.325459E-2,  1.945697E-1, -2.396387E-1,  1.667832E-1,
     *   -6.620786E-2,  2.224041E-2,  1.466325E-2, -9.858686E-2,
     *    2.180684E-1, -2.347118E-1,  1.464354E-1, -5.350728E-2,
     *    1.624782E-2,  2.314317E-2, -1.246383E-1,  2.362036E-1,
     *   -2.257366E-1,  1.275895E-1, -4.317874E-2,  1.193168E-2,
     *    3.346886E-2, -1.503778E-1,  2.492826E-1, -2.142055E-1,
     *    1.106482E-1, -3.486024E-2,  8.821107E-3 /
      DATA Q5 /
     *    2.418820E-1,  2.291233E-1,  2.177793E-1,  2.075784E-1,
     *    1.983358E-1 /
      DATA P6L /
     *    5.613913E-2, -3.019847E-1,  6.256387E-1, -6.324887E-1,
     *    3.303194E-1,  6.843713E-2, -3.342119E-1,  6.302307E-1,
     *   -5.829747E-1,  2.765700E-1,  8.203343E-2, -3.644705E-1,
     *    6.278660E-1, -5.335581E-1,  2.312756E-1,  9.675562E-2,
     *   -3.922489E-1,  6.197133E-1, -4.857470E-1,  1.934013E-1,
     *    1.124069E-1, -4.172349E-1,  6.069622E-1, -4.405326E-1,
     *    1.618978E-1 /
      DATA Q6L /
     *    9.077644E-1,  2.535284E-1,  8.626056E-1,  2.291400E-1,
     *    8.212018E-1,  2.078043E-1,  7.831755E-1,  1.890848E-1,
     *    7.481828E-1,  1.726085E-1 /
      DATA P6U /
     *    8.531865E-4, -1.616105E-2,  6.888533E-2, -1.109391E-1,
     *    7.747182E-2,  2.060760E-3, -2.558954E-2,  8.595213E-2,
     *   -1.170228E-1,  7.094106E-2,  4.028559E-3, -3.697768E-2,
     *    1.021332E-1, -1.201436E-1,  6.412774E-2,  6.887946E-3,
     *   -4.994202E-2,  1.168451E-1, -1.207733E-1,  5.744210E-2,
     *    1.071895E-2, -6.404749E-2,  1.297386E-1, -1.194208E-1,
     *    5.112822E-2 /
      DATA Q6U /
     *    1.101270E+0,  3.858544E-1,  1.025431E+0,  3.337648E-1,
     *    9.599102E-1,  2.918724E-1,  9.025276E-1,  2.575336E-1,
     *    8.517470E-1,  2.289667E-1 /
      DATA P7L /
     *    2.460495E-2, -1.640964E-1,  4.340110E-1, -5.705516E-1,
     *    4.418614E-1,  3.070261E-2, -1.879546E-1,  4.565902E-1,
     *   -5.544891E-1,  3.892790E-1,  3.770526E-2, -2.121608E-1,
     *    4.746423E-1, -5.338058E-1,  3.417026E-1,  4.559398E-2,
     *   -2.362670E-1,  4.881998E-1, -5.098448E-1,  2.991635E-1,
     *    5.432500E-2, -2.598752E-1,  4.974791E-1, -4.837861E-1,
     *    2.614838E-1 /
      DATA Q7L /
     *    1.124957E+0,  3.784976E-1,  1.075420E+0,  3.466086E-1,
     *    1.029374E+0,  3.181219E-1,  9.865496E-1,  2.926441E-1,
     *    9.466891E-1,  2.698218E-1 /
      DATA P7U /
     *    1.924318E-4, -5.044864E-3,  2.979803E-2, -6.660688E-2,
     *    6.792268E-2,  5.030909E-4, -8.639332E-3,  4.018472E-2,
     *   -7.595456E-2,  6.696215E-2,  1.059406E-3, -1.343605E-2,
     *    5.135360E-2, -8.386588E-2,  6.484517E-2,  1.941904E-3,
     *   -1.943727E-2,  6.288221E-2, -9.021607E-2,  6.193000E-2,
     *    3.224785E-3, -2.657664E-2,  7.438627E-2, -9.500554E-2,
     *    5.850884E-2 /
      DATA Q7U /
     *    1.450730E+0,  6.578685E-1,  1.353872E+0,  5.724332E-1,
     *    1.269924E+0,  5.032139E-1,  1.196177E+0,  4.460948E-1,
     *    1.130719E+0,  3.982785E-1 /
      DATA P8L /
     *    1.378030E-2, -1.097846E-1,  3.625283E-1, -6.522477E-1,
     *    6.684458E-1, -4.703556E-1,  1.721632E-2, -1.274981E-1,
     *    3.917226E-1, -6.562264E-1,  6.305859E-1, -4.067119E-1,
     *    2.121871E-2, -1.461891E-1,  4.185427E-1, -6.543539E-1,
     *    5.904660E-1, -3.507098E-1,  2.580565E-2, -1.656048E-1,
     *    4.426283E-1, -6.473472E-1,  5.494752E-1, -3.018936E-1,
     *    3.098251E-2, -1.854823E-1,  4.637398E-1, -6.359482E-1,
     *    5.086794E-1, -2.595588E-1 /
      DATA Q8L /
     *    1.076975E+0,  3.394154E-1,  1.036132E+0,  3.145673E-1,
     *    9.978025E-1,  2.920529E-1,  9.617584E-1,  2.715949E-1,
     *    9.278774E-1,  2.530051E-1 /
      DATA P8U /
     *    4.290460E-5, -1.508077E-3,  1.233763E-2, -4.091270E-2,
     *    6.547454E-2, -5.664203E-2,  1.201008E-4, -2.778372E-3,
     *    1.797999E-2, -5.055048E-2,  7.125083E-2, -5.469912E-2,
     *    2.698511E-4, -4.628815E-3,  2.470890E-2, -6.017759E-2,
     *    7.566434E-2, -5.202678E-2,  5.259595E-4, -7.144198E-3,
     *    3.238633E-2, -6.946769E-2,  7.873067E-2, -4.889490E-2,
     *    9.255826E-4, -1.038126E-2,  4.083176E-2, -7.815954E-2,
     *    8.054087E-2, -4.552077E-2 /
      DATA Q8U /
     *    1.379457E+0,  5.786953E-1,  1.300303E+0,  5.135748E-1,
     *    1.230436E+0,  4.593779E-1,  1.168075E+0,  4.135871E-1,
     *    1.111893E+0,  3.744076E-1 /
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check inputs.
      IF ((IALF.LT.1) .OR. (IALF.GT.5)) IERR = 1
      IF ((IM.LT.4) .OR. (IM.GT.8)) IERR = 2 + 10 * IERR
      IF (ABS(ETA).GT.1.) IERR = 3 + 10 * IERR
      IF (IERR.NE.0) GO TO 900
C                                       So far, so good.
      ETA2 = ETA**2
      J = IALF
      K = IM - 3
C                                       Branch on support width.
      GO TO (100, 200, 300, 400, 500), K
C                                       Support width = 4 cells.
 100  X = ETA2 - 1.0
      PSI = (P4(1,J) + X * (P4(2,J) + X * (P4(3,J) + X * (P4(4,J) +
     *   X * P4(5,J))))) / (1.0 + X * (Q4(1,J) + X * Q4(2,J)))
      GO TO 800
C                                       Support width = 5 cells.
 200  X = ETA2 - 1.0
      PSI = (P5(1,J) + X * (P5(2,J) + X * (P5(3,J) + X * (P5(4,J) +
     *   X * (P5(5,J) + X * (P5(6,J) + X * P5(7,J)))))))
     *   / (1.0 + X * Q5(J))
      GO TO 800
C                                       Support width = 6 cells.
 300  IF (ABS(ETA).GT.0.75) GO TO 350
         X = ETA2 - 0.5625
         PSI = (P6L(1,J) + X * (P6L(2,J) + X * (P6L(3,J) +
     *      X * (P6L(4,J) + X * P6L(5,J))))) / (1.0 + X * (Q6L(1,J) +
     *      X * Q6L(2,J)))
         GO TO 800
 350  X = ETA2 - 1.0
      PSI = (P6U(1,J) + X * (P6U(2,J) + X * (P6U(3,J) + X * (P6U(4,J) +
     *   X * P6U(5,J))))) / (1.0 + X * (Q6U(1,J) + X * Q6U(2,J)))
      GO TO 800
C                                       Support width = 7 cells.
 400  IF (ABS(ETA).GT.0.775) GO TO 450
         X = ETA2 - 0.600625
         PSI = (P7L(1,J) + X * (P7L(2,J) + X * (P7L(3,J) +
     *      X * (P7L(4,J) + X * P7L(5,J))))) / (1.0 + X * (Q7L(1,J) +
     *      X * Q7L(2,J)))
         GO TO 800
 450  X = ETA2 - 1.0
      PSI = (P7U(1,J) + X * (P7U(2,J) + X * (P7U(3,J) + X * (P7U(4,J) +
     *   X * P7U(5,J))))) / (1.0 + X * (Q7U(1,J) + X * Q7U(2,J)))
      GO TO 800
C                                       Support width = 8 cells.
 500  IF (ABS(ETA).GT.0.775) GO TO 550
         X = ETA2 - .600625
         PSI = (P8L(1,J) + X * (P8L(2,J) + X * (P8L(3,J) +
     *      X * (P8L(4,J) + X * (P8L(5,J) + X * P8L(6,J)))))) /
     *      (1.0 + X * (Q8L(1,J) + X * Q8L(2,J)))
         GO TO 800
 550  X = ETA2 - 1.0
      PSI = (P8U(1,J) + X * (P8U(2,J) + X * (P8U(3,J) + X * (P8U(4,J) +
     *   X * (P8U(5,J) + X * P8U(6,J)))))) / (1.0 + X * (Q8U(1,J) +
     *   X * Q8U(2,J)))
C                                       Normal return.
 800  IF ((IFLAG.GT.0) .OR. (IALF.EQ.1) .OR. (ETA.EQ.0.0)) GO TO 999
         IF (ABS(ETA).EQ.1.) GO TO 850
            PSI = (1.0 - ETA2)**ALPHA(IALF) * PSI
            GO TO 999
 850     PSI = 0.0
         GO TO 999
C                                       Error exit.
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SPHFN: INPUT ERROR = ',I6)
      END
