LOCAL INCLUDE 'TARS.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   RMTSIZ
      PARAMETER (RMTSIZ = 8193)
C
      INTEGER   HSIZE, FSIZE, HSIZER, FSIZER, NITER, KLIM, KMN, KMX
      REAL      FLUX, GAIN, CELL
      DOUBLE PRECISION RERMT(RMTSIZ), IMRMT(RMTSIZ), GAUSS(RMTSIZ)
      REAL      CPARM(10)
      CHARACTER NAMIN(2)*12, CLSIN(2)*6, INFILE*48, OFILE*48, OPCODE*4
      LOGICAL   CLFOUR, UNCLFO, DOCONV, DOGAUS, DORES, CLPLRE, CLONLY
      INTEGER   DOUTFI, SCRTCH(256)
C
      INTEGER   NL2AX, L2SIZE
      PARAMETER (L2SIZE = 16384)
      DOUBLE PRECISION ARRL2(L2SIZE), W(L2SIZE), WSUM, L2MEAN
      COMMON /TARSYN/ ARRL2, W, RERMT, IMRMT, GAUSS, WSUM, L2MEAN, NL2AX
C                                       size of Larry's file
      INTEGER   LARSIZ, LAROUT
      PARAMETER (LARSIZ = 16384)
      PARAMETER (LAROUT = 10000)
      DOUBLE PRECISION FRELAR(LARSIZ), ULAR(LARSIZ), QLAR(LARSIZ),
     *   RMMO(20), AMPMO(20), PHASMO(20), OUTRE(LAROUT), OUTIM(LAROUT)
      COMMON /LARRY/ FRELAR, ULAR, QLAR, RMMO, AMPMO, PHASMO,
     *   OUTRE, OUTIM
C
      COMMON /TARSCM/ SCRTCH, CPARM, HSIZE, FSIZE, HSIZER, FSIZER, CELL,
     *   FLUX, GAIN, CLFOUR, UNCLFO, DOCONV, DOGAUS, DORES, CLPLRE,
     *   CLONLY, DOUTFI, NITER, KLIM, KMN, KMX
      COMMON /TARSCH/ NAMIN, CLSIN, INFILE, OFILE, OPCODE
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM TARS
C-----------------------------------------------------------------------
C! carry out faraday rotation measure synthesis.
C# Map-util SPECTRAL POLARIZATION ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2012, 2014-2015
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
C   TARS carries out Faraday rotation synthesis using input file of
C   Q,U as a function of frequency and output file as a function of
C   RM for one pixel at RA, DEC
C   Inputs: (from AIPS)
C   INFILE      Input file
C   OUTFILE     Output file
C   APARM.......Parameters needed for algorithm:
C     APARM(1)  Number of pixels at half of Fourier transform output
C     APARM(2)  cell size in 1/m^2
C     APARM(3)  >0 RMTF output
C     APARM(4)  0=> CLEANed Fourier transform
C               1=> unCLEANed Fourier  transform
C               2=> 3 parameters of the only maximum at the the Fourier
C                   transform
C     APARM(5)  0=> original(shifted back) RE/IM are sent out
C               1=> the shifted RE/IM are sent out
C               2=> amplitudes of the data are sent out
C     APARM(6)  is not used
C     APARM(7)  0=> convolve the clean components
C               1=> no convolve
C     APARM(8)  0=> use the Gaussian as the convolve function
C               1=> use the Re of RMTF as the convolve function
C     APARM(9)  full width of Gaussian convolve function, at 0.5
C               level, in 1/m^2,  0 => 1
C     APARM(10) send residual to the output?
C                   1 => yes
C                   0 => regular output
C   GAIN      Gain in the CLEAN
C   NITER     Maximum number of clean components
C   FLUX      Minimum flux of clean
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IERR
      INCLUDE 'TARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGNAM /'TARS  '/
C-----------------------------------------------------------------------
C                                        Initialize input maps
      CALL TARSIN (PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
C
      IF (OPCODE.EQ.'CMPL') THEN
         CALL TARSXX (QLAR(1), ULAR(1), CPARM, OUTRE, OUTIM)
      ELSE
         CALL TARSCL (QLAR(1), ULAR(1), CPARM, .TRUE., OUTRE, OUTIM)
         END IF
C
 995  CALL DIE (IERR, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE TARSIN (PRGNAM, IERR)
C-----------------------------------------------------------------------
C   TARSIN gets the inputs for TARS, opens and checks the input images,
C   creates the output image(s), and prepares parameters in common for
C   the later stages of TARS.
C   Inputs:
C      PRGNAM   C*6    Program name
C   Outputs into TARS.INC:
C      NL2AX    I      Number of points at the array ARRL2
C      ARRL2    R(*)   Array of lambda squares
C      W        R(*)   Array of weghts
C      L2MEAN   R      Mean value of lambda square
C   Outputs:
C      IER      I      Error return: 0-->  Okay
C                         3-->  Cannot create and open output file
C                         2-->  Cannot open either input map
C                         1-->  Error in getting input parameters
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IERR
C
      INTEGER   I, IRETCD, INPRMS, IROUND, K, NROW, MLINE, ICOMP
      LOGICAL   T
      REAL      APARM(10), GAINN, NNITER, FLUXX
      DOUBLE PRECISION ARGQU, PHARAD, FREQV, RERMTF, IMRMTF, ARG,
     *   HWIDTH, AMAX, ALAST, A, B, L2MIN, L2MAX, FI
      HOLLERITH  XINFIL(12), XOFILE(12), XOPCOD
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'TARS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      COMMON /INPARM/ XINFIL, XOFILE, APARM, GAINN, NNITER, FLUXX,
     *   XOPCOD
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
C                                        Get inputs from AIPS
      INPRMS = 38
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XINFIL, SCRTCH, IERR)
      IRETCD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         END IF
      IF (RQUICK) CALL RELPOP (IRETCD, SCRTCH, I)
      IF (IRETCD.NE.0) GO TO 999
C                                       size of the Fourier transform
      HSIZE = APARM(1)
      FSIZE = 2*HSIZE + 1
C                                       Number of iteration at
C                                       components subtraction
      NITER = NNITER
      IF (NITER.EQ.0) NITER = 1
      FLUX = FLUXX
      GAIN = GAINN
      IF (GAIN.EQ.0) GAIN = 0.1
C                                       different options of the outputs
C                                       APARM(4)
      IF (APARM(3).GT.0.0) THEN
         CLFOUR = .FALSE.
         UNCLFO = .TRUE.
      ELSE
         CLFOUR = APARM(4).LE.0.0
         UNCLFO = APARM(4).GT.0.0
         END IF
C                                       send CLEAN + RESIDUAL to
C                                       output: APARM(10)=0
      CLPLRE = IROUND(APARM(10)).LE.0
C                                       send CLEAN ONLY to
C                                       output: APARM(10)=1
      CLONLY = IROUND(APARM(10)).EQ.1
C                                       send RESIDUAL of clean to
C                                       output: APARM(10)=2
      DORES = IROUND(APARM(10)).GE.2
C                                       Send convolved clean components
C                                       to the output images or just
C                                       the components themself
      DOCONV = APARM(7).LT.0.5
C                                       If NOCLEAN (just FOURIER) or
C                                       use the 3 parameters, then
C                                       no CONVOLUTION
      IF (APARM(4).GT.0.0) DOCONV = .FALSE.
C                                       Use the Gaussian convolve
C                                       function or the RE of RMTF
      DOGAUS = APARM(8).LT.0.5
C                                       infile, outfil
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IF (UNCLFO) OPCODE = ' '
      IF (APARM(3).GT.0.0) OPCODE = ' '
C                                       calculate the output file?
      DOUTFI = -1
      IF (OFILE.NE.' ') DOUTFI = 1
C                                       Create set of selected lambda^2
C                                       using the input table
      IF (APARM(6).LT.0.5) THEN
         NROW = 1.E6
      ELSE
         NROW = APARM(6)
         END IF
C                                       read the input data to get
C                                       frequiencies and Q and U
      CALL DFILL (L2SIZE, 1.0D0, W)
      CALL DFILL (LARSIZ, 0.0D0, QLAR)
      CALL DFILL (LARSIZ, 0.0D0, ULAR)
      CALL GETLAR (INFILE, NROW, NL2AX, MLINE, FRELAR, QLAR, ULAR, W,
     *   RMMO, AMPMO, PHASMO, IERR)
      IF (IERR.NE.0) GO TO 999
      I = 0
      L2MEAN = 0.0D0
      WSUM = 0.0D0
      L2MIN = 1.0D10
      L2MAX = 1.0D-10
C
      DO 30 I = 1, NL2AX
         WSUM = WSUM + W(I)
         FREQV = FRELAR(I)
         ARRL2(I) = (299792458.D0/FREQV)**2
C                                       recalculate QLAR, ULAR for
C                                       the model
         IF (MLINE.GT.0) THEN
            DO 20 ICOMP = 1,MLINE
               PHARAD = DG2RAD * PHASMO(ICOMP)
               ARGQU = 2.0D0 * RMMO(ICOMP) * ARRL2(I) + PHARAD
               QLAR(I) = QLAR(I) + AMPMO(ICOMP) * COS(ARGQU)
               ULAR(I) = ULAR(I) + AMPMO(ICOMP) * SIN(ARGQU)
 20            CONTINUE
            END IF
C
C       QLAR(I)=5.*COS(-2*550.*ARRL2(I)+PI/3) + 3.*COS(-2*800.*ARRL2(I))
C       ULAR(I)=5.*SIN(-2*550.*ARRL2(I)+PI/3) + 3.*SIN(-2*800.*ARRL2(I))
C
         IF (ARRL2(I).GT.L2MAX) L2MAX = ARRL2(I)
         IF (ARRL2(I).LT.L2MIN) L2MIN = ARRL2(I)
         L2MEAN = L2MEAN + W(I)*ARRL2(I)
 30      CONTINUE
C                                       cell size at the output
      CELL = APARM(2)
      IF (CELL.LT.0.01) CELL = PI/(4*(L2MAX-L2MIN))
      APARM(2) = CELL
      CALL RCOPY (10, APARM, CPARM)
C
      L2MEAN = L2MEAN / WSUM
C                                       Printout the value of L2MEAN
      WRITE (MSGTXT,1030) L2MEAN
      CALL MSGWRT (7)
C
C                                       Subtract the mean L^2
      DO 40 I = 1,NL2AX
         ARRL2(I) = ARRL2(I) - L2MEAN
 40      CONTINUE
C                                       calculate RMTF with double
C                                       points for the component
C                                       located at the edge
      HSIZER = 2*HSIZE
      FSIZER = 2*HSIZER + 1
      DO 60 K = 1,FSIZER
         RERMTF = 0.0D0
         IMRMTF = 0.0D0
         FI = (K-HSIZER-1) * CELL
         DO 50 I = 1,NL2AX
            ARG = 2.0D0 * FI * ARRL2(I)
C                                       DFT with exp-(ARG)
C                                       calculate RMTF
            RERMTF = RERMTF +  W(I)*COS(ARG)
            IMRMTF = IMRMTF -  W(I)*SIN(ARG)
 50         CONTINUE
         RERMT(K) = RERMTF/WSUM
         IMRMT(K) = IMRMTF/WSUM
 60      CONTINUE
C                                       evaluate the convolved
C                                       function as Gaussian
      IF (DOGAUS) THEN
C                                       Half width of the Gaussian
         HWIDTH = APARM(9) / 2.0
C                                       Find fullwidth
         IF (HWIDTH.LT.0.2D0) THEN
            AMAX = RERMT(HSIZER+1)
            B = AMAX
            K = HSIZER + 1
            DO 65 I = 1,HSIZER
               A = (RERMT(K-I) + RERMT(K+I)) / 2.0
               IF (A.GT.0.5D0*AMAX) THEN
                  B = A
               ELSE
                  HWIDTH = (I - (0.5D0-A) / (B-A)) * CELL
                  GO TO 70
                  END IF
 65            CONTINUE
            HWIDTH = 1
            END IF
 70      APARM(9) = 2.0 * HWIDTH
         WRITE (MSGTXT,1070) APARM(9)
         CALL MSGWRT (4)
         CALL RFILL (FSIZER, 0.0, GAUSS)
         DO 75 K = 1,FSIZER
            FI = (K-HSIZER-1) * CELL / HWIDTH
            IF (ABS(FI).LT.5.) THEN
               GAUSS(K) = EXP (-0.6931472D0 * (FI**2))
               KMN = MIN (KMN, K)
               KMX = K
               END IF
 75         CONTINUE
      ELSE
         KMN = HSIZER+1 - HSIZE
         KMX = HSIZER+1 + HSIZE
         END IF
C                                       width of center of beam
      IF (OPCODE.EQ.'CMPL') THEN
         AMAX = SQRT (RERMT(HSIZER+1)**2 + IMRMT(HSIZER+1)**2)
         ALAST = AMAX
         DO 80 I = 1,HSIZER
            K = HSIZER + 1 - I
            A = SQRT (RERMT(K)**2 + IMRMT(K)**2)
            K = HSIZER + 1 + I
            B = SQRT (RERMT(K)**2 + IMRMT(K)**2)
            IF ((A.GT.1.01*ALAST) .OR. (B.GT.1.01*ALAST) .OR.
     *         (A.LT.0.45*AMAX) .OR. (B.LT.0.45*AMAX)) THEN
               KLIM = MAX (1, I-1)
               GO TO 999
            ELSE
               ALAST = (A + B) / 2.0
               END IF
 80         CONTINUE
         KLIM = HSIZER/2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TARSIN: COULD NOT GET PARMS.  IER=',I7)
 1030 FORMAT ('L2MEAN = ',  F10.5, ' m^2')
 1070 FORMAT ('TARSIN: using restoring Gaussian of FWHM',F9.3,' 1/m^2')
      END
      SUBROUTINE TARSCL (V1, V2, APARM, DOPRT, R1, R2)
C-----------------------------------------------------------------------
C   TARSCL does a peak-in-the-amplitude Clean of a data row.
C   Inputs:
C      V1     R(NX)   Real values of the row
C      V2     R(NX)   Imaginary values of the row
C      APARM  R(10)   Inputs parameters
C   Outputs:
C      R1     R(NX)   Real values of the transformed/Clean row
C      R2     R(NX)   Imaginary values of the transformed/Clean row
C-----------------------------------------------------------------------
      REAL      APARM(10)
      LOGICAL   DOPRT
      DOUBLE PRECISION V1(*), V2(*), R1(*), R2(*)
C
      INTEGER   ARSIZE
      PARAMETER (ARSIZE = 4097)
c
      INTEGER   LUNPR, PFIND, NCH, ITRIM, I, K, L, KL, KMAXAM, ITER,
     *   KOFMAX, IERR, KOFF
      DOUBLE PRECISION AMPMAX, REF, IMF, FI, ARG, AMP, REFAR(ARSIZE),
     *   IMFAR(ARSIZE), REFAMP, IMFAMP, PHASE, PHASEC, REFF, IMFF,
     *   TEMPRE, TEMPIM, RECOMP, IMCOMP, RECOUT, IMCOUT, REFMUL, IMFMUL,
     *   RC1(ARSIZE), RC2(ARSIZE), AMPRES, RERES, IMRES, RTEMP
      LOGICAL   BLANK
      CHARACTER LINE*80
      INCLUDE 'TARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DO 20 I = 1,NL2AX
         BLANK = (V1(I).EQ.DBLANK) .OR. (V2(I).EQ.DBLANK)
         IF (BLANK) THEN
            V1(I) = 0.0D0
            V2(I) = 0.0D0
C                                       to get RMTF
         ELSE IF (APARM(3).GT.0.0) THEN
            V1(I) = 1.0D0
            V2(I) = 0.0D0
            END IF
  20     CONTINUE
C                                       evaluate the Fourier transorm of
C                                       the complex array V1(I)+J*V2(I)
      AMPMAX = -1.0D+10
      DO 60 K = 1, FSIZE
         REF = 0
         IMF = 0
         FI = (K-HSIZE-1) * CELL
         DO 50 I = 1,NL2AX
            ARG = 2.0D0 * FI * ARRL2(I)
C                                       DFT with exp-(ARG)
            REF = REF + W(I) * (V1(I)*COS(ARG) + V2(I)*SIN(ARG))
            IMF = IMF + W(I) * (-V1(I)*SIN(ARG) + V2(I)*COS(ARG))
   50       CONTINUE
         REF = REF / WSUM
         IMF = IMF / WSUM
         REFAR(K) = REF
         IMFAR(K) = IMF
C                                       Find max amplitude
         AMP = SQRT (REF*REF + IMF*IMF)
         IF (AMP.GT.AMPMAX) THEN
            AMPMAX = AMP
            KMAXAM = K
            REFAMP = REF
            IMFAMP = IMF
            END IF
C                                       zero output
         IF (.NOT.UNCLFO) THEN
            R1(K) = 0.0D0
            R2(K) = 0.0D0
C                                       generate uncleaned output
         ELSE
C                                       convert to the AMP, PHASE
            PHASE = ATAN2 (IMF, REF)
C                                       subtract the phase at L2MEAN
C                                       to get the original data
C                                       corresponded to not shifted
C                                       L^2 set
            PHASEC = PHASE - 2 * FI * L2MEAN
            REFF = AMP * COS(PHASEC)
            IMFF = AMP * SIN(PHASEC)
C                                       store the output at
C                                       the TEMPRE, TEMPIM
C                                       shifted back data (original
            IF (APARM(5).LE.0.0) THEN
               TEMPRE = REFF
               TEMPIM = IMFF
C                                       shifted data
            ELSE IF (APARM(5).LT.1.5) THEN
               TEMPRE = REF
               TEMPIM = IMF
C                                       Amplitudes
            ELSE
               TEMPRE = REFF
               TEMPIM = IMFF
               END IF
            R1(K) = TEMPRE
            R2(K) = TEMPIM
            END IF
 60      CONTINUE
C                                       CLEAN
      IF (.NOT.UNCLFO) THEN
         ITER = 0
C                                       cycle by iteration(components)
 100     IF ((ITER.LT.NITER) .AND. (AMPMAX.GE.FLUX)) THEN
            ITER = ITER + 1
C                                       subtracted component
            RECOMP = GAIN * REFAMP
            IMCOMP = GAIN * IMFAMP
            PHASE  = ATAN2 (IMFAMP, REFAMP)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
            FI = (KMAXAM-HSIZE-1) * CELL
            PHASEC = PHASE - 2 * FI * L2MEAN
            REFF = AMPMAX * COS(PHASEC)
            IMFF = AMPMAX * SIN(PHASEC)
            RECOUT = GAIN * REFF
            IMCOUT = GAIN * IMFF
C                                       iteration (component) is found
            R1(KMAXAM) = R1(KMAXAM) + RECOUT
            R2(KMAXAM) = R2(KMAXAM) + IMCOUT
            IF (DOPRT) THEN
               WRITE (MSGTXT,1100)
               IF (ITER.EQ.1) CALL MSGWRT (4)
               WRITE (MSGTXT,1101) ITER, AMPMAX, PHASE*RAD2DG, KMAXAM
               CALL MSGWRT (4)
               END IF
C                                       Subtract the RMTF multiplied by
C                                       the value of RE/IM at max amp
C                                       from RE,IM of the subtracted
C                                       component located at
C                                       K = KMAXAM
            AMPMAX = -1.0E+10
            KOFF = KMAXAM - HSIZER - 1
            DO 120 K = 1,FSIZE
C                                       KOFMAX position of RMTF's max
C                                       relatively AMP max
               KOFMAX = K - KOFF
C                                       The RE, Im parts for the current
C                                       Multiply the positioned RMTF
C                                       by the subtracted component
               REFMUL = RECOMP*RERMT(KOFMAX) - IMCOMP*IMRMT(KOFMAX)
               IMFMUL = RECOMP*IMRMT(KOFMAX) + IMCOMP*RERMT(KOFMAX)
C                                       Subtract the component
               REFAR(K) = REFAR(K) - REFMUL
               IMFAR(K) = IMFAR(K) - IMFMUL
C                                       Find position of maximum of
C                                       amplitude
               AMP = SQRT (REFAR(K)*REFAR(K) + IMFAR(K)*IMFAR(K))
               IF (AMP.GT.AMPMAX) THEN
                  AMPMAX = AMP
                  KMAXAM = K
                  REFAMP = REFAR(K)
                  IMFAMP = IMFAR(K)
                  END IF
  120          CONTINUE
            GO TO 100
            END IF
C                                       End of CLEAN
C                                       Convolve the found set of
C                                       components with the RE of RMTF
C                                       or with the given Gaussian
         IF (DOCONV .AND. (CLONLY .OR. CLPLRE)) THEN
            CALL RFILL (FSIZE, 0.0, RC1)
            CALL RFILL (FSIZE, 0.0, RC2)
            DO 140 L = 1,FSIZE
               IF ((R1(L).NE.0.0) .OR. (R2(L).NE.0.0)) THEN
                  DO 130 KL = KMN,KMX
C                                       KL position of RMTF's max
C                                       relatively K
                     K = L - KL + HSIZER+1
                     IF ((K.GE.1) .AND. (K.LE.FSIZE)) THEN
                        IF (DOGAUS) THEN
                           RC1(K) = RC1(K) + GAUSS(KL)*R1(L)
                           RC2(K) = RC2(K) + GAUSS(KL)*R2(L)
                        ELSE
                           RC1(K) = RC1(K) + RERMT(KL)*R1(L)
                           RC2(K) = RC2(K)+ RERMT(KL)*R2(L)
                           END IF
                        END IF
 130                 CONTINUE
                  END IF
  140          CONTINUE
C                                       move back
            DO 160 K = 1, FSIZE
               R1(K) = RC1(K)
               R2(K) = RC2(K)
  160          CONTINUE
            END IF
C
         IF ((DORES) .OR. (CLPLRE)) THEN
            DO 180 K = 1,FSIZE
               PHASE = ATAN2 (IMFAR(K), REFAR(K))
               AMPRES = SQRT (IMFAR(K)**2 + REFAR(K)**2)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
               FI = (K-HSIZE-1) * CELL
               PHASEC = PHASE - 2 * FI * L2MEAN
C                                       Store the shifted back residual
               RERES = AMPRES * COS(PHASEC)
               IMRES = AMPRES * SIN(PHASEC)
C                                       Send the residual into the
C                                       output
               IF (DORES) THEN
                  R1(K) = RERES
                  R2(K) = IMRES
C                                       Send the residual plus convolved
C                                       into the output
               ELSE IF (CLPLRE) THEN
                  R1(K) = R1(K) + RERES
                  R2(K) = R2(K) + IMRES
                  END IF
  180          CONTINUE
            END IF
         END IF
C                                       calculate amplitudes
      IF (APARM(5).GE.1.5) THEN
         DO 200 K = 1, FSIZE
            RTEMP = R1(K)
            R1(K) = SQRT (R1(K)*R1(K) + R2(K)*R2(K))
            R2(K) = RAD2DG * ATAN2 (R2(K), RTEMP)
  200       CONTINUE
         END IF
C                                       open file for record the outputs
      IF (DOUTFI.GT.0) THEN
         LUNPR = 3
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR OPENING OUTPUT TEXT FILE'
            CALL MSGWRT (7)
            DOUTFI = -1
            END IF
         END IF
C                                       header of the output file:AMP
C                                       PHAS
      IF (APARM(5).GE.1.5) THEN
         WRITE (MSGTXT,2001)
      ELSE
         WRITE (MSGTXT,2005)
         END IF
      IF (DOUTFI.LE.0) THEN
         CALL MSGWRT (4)
      ELSE
         LINE = MSGTXT
         NCH = ITRIM (LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR WRITING OUTPUT TEXT FILE'
            IF (IERR.GT.0) CALL MSGWRT (7)
            DOUTFI = 0
            END IF
         END IF
C                                       record the lines
      DO 300 K = 1, FSIZE
         IF (APARM(5).GE.1.5) THEN
            WRITE (MSGTXT,2011)  K, (K-HSIZE-1)*CELL, R1(K), R2(K)
         ELSE
            WRITE (MSGTXT,2015)  K, (K-HSIZE-1)*CELL, R1(K), R2(K)
            END IF
         IF (DOUTFI.LE.0) THEN
            CALL MSGWRT (4)
         ELSE
            LINE = MSGTXT
            NCH = ITRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR WRITING OUTPUT TEXT FILE'
               IF (IERR.GT.0) CALL MSGWRT (7)
               DOUTFI = 0
               END IF
            END IF
  300    CONTINUE
C                                       close output file
      IF (DOUTFI.GE.0) CALL ZTXCLS (LUNPR, PFIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (' Iter  Amplitude  Phase Pixel')
 1101 FORMAT (I5,1PE11.3,0PF7.1,I6)
 2001 FORMAT (';   K',8X,'RM',8X,'10^6 AMP',9X,'PHASE')
 2005 FORMAT (';   K',8X,'RM',9X,'10^6 RE',7X,'10^6 IM')
 2011 FORMAT (I5,F12.1,6PF14.2,0PF14.2)
 2015 FORMAT (I5,F12.1,6PF14.2,6PF14.2)
      END
      SUBROUTINE GETLAR (FILE, NROW, NLINE, MLINE, FREQ, Q, U, WW, RM,
     *   AMP, PHAS, IERR)
C-----------------------------------------------------------------------
C  This subroutine reads, from an input file specified by name "file",
C  the 3 sequences at each row
C                  of FREQ
C                  of O
C                  of U
C?  may be in future: of given number (NL2AX) weights.
C  Inputs:
C    FILE     C*48  File name
C    NROW     number of row to pick up off the file
C  Outputs
C    NLINE    number of chosen rows in the file
C    MLINE    number of lines at the model
C    RM       array of rotation measures at the model
C    AMP      array of the amplitudes at the model
C    PHAS     array of phases at the model
C    FREQ     array of frequencies at the Larry's file
C    U        array of U at the Larry's file
C    Q        array of Q at the Larry's file
C  Outputs:
C    IERR     I     Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   NROW, IERR
      DOUBLE PRECISION FREQ(*), U(*), Q(*), WW(*), RM(*), AMP(*),
     *   PHAS(*)
C
      INTEGER   LUN, ILINE, IWEIGT, NLINE, KWEIGT, INLINE, MLINE, FIND,
     *   NBYTES, KBP, JTRIM
      DOUBLE PRECISION X
      CHARACTER LINE*80
      INCLUDE 'TARS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Open input text file for read
      LUN = 10
      CALL ZTXOPN ('READ', LUN, FIND, FILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1001)
         GO TO 990
         END IF
C                                       Get weight values
      KBP = 1
      NBYTES = 80
C
      IWEIGT = 0
      KWEIGT = 0
      ILINE = 0
      MLINE = 0
C                                       next line
 20   CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
      INLINE = 0
C
C                                       get the file end
      IF (IERR.EQ.2) THEN
         IERR = 0
         ILINE = ILINE + 1
         GO TO 970
         END IF
      IF (IERR.NE.0) GO TO 980
C                                       deal with TABs and junk
      NBYTES = JTRIM (LINE)
C                                       skip blank lines
      IF (NBYTES.EQ.0) GO TO 20
C                                       skip line with comments
      IF (LINE(1:1).EQ.';')  GO TO 20
C                                       Get values
      IF (LINE(1:1).EQ.'M') THEN
         KBP = 2
         MLINE = MLINE + 1
      ELSE
         KBP = 1
         ILINE = ILINE + 1
         END IF
C                                       cycle in the line
 30   CALL GETNUM (LINE, NBYTES, KBP, X)
      IF (X.EQ.DBLANK) GO TO 30
      INLINE = INLINE + 1
      IF (LINE(1:1).EQ.'M') THEN
C                                       The first sympol is "M"
C                                       Get values
         IF (INLINE.EQ.1) RM(MLINE) = X
         IF (INLINE.EQ.2) AMP(MLINE) = X
         IF (INLINE.EQ.3) PHAS(MLINE) = X
C                                       go to the next line
         IF (KBP.GT.NBYTES) GO TO 20
C                                       next number in the line
         GO TO 30
C                                       The first symbol is not "M"
      ELSE
         IF (INLINE.EQ.1) FREQ(ILINE) = X
         IF (INLINE.EQ.2) Q(ILINE) = X
         IF (INLINE.EQ.3) U(ILINE) = X
         IF (INLINE.EQ.4) WW(ILINE) = X
         END IF
C                                       go to the next line
      IF (KBP.GT.NBYTES) GO TO 20
      IF (ILINE.GT.NROW) GO TO 970
C                                       next number in the line
      GO TO 30
C                                       Number of chousen lines
  970 NLINE = ILINE - 1
C                                       close input file
      CALL ZTXCLS (LUN, FIND, IERR)
C
      GO TO 999
C                                       Infile read error
 980  WRITE (MSGTXT,1003) IERR
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ERROR ',I3,' OPENING WEIGHT TEXT FILE')
 1003 FORMAT ('ERROR ',I3,' READING ANTENNA INFO TEXT FILE')
      END
      SUBROUTINE TARSXX (VRE, VIM, APARM, RRE, RIM)
C-----------------------------------------------------------------------
C   TARSXX does an experimental Complex pattern-match Clean
C   Inputs:
C      VRE     R(NX)   Real part of input row
C      VIM     R(NX)   Imaginary part of input row
C      APARM   R(10)   Up to 10 inputs parameters
C   Outputs:
C      RRE     R(NX)   Real part of tranformed and Cleaned row
C      RIM     R(NX)   Imaginary part of tranformed and Cleaned row
C-----------------------------------------------------------------------
      REAL      APARM(10)
      DOUBLE PRECISION VRE(*), VIM(*), RRE(*), RIM(*)
C
      INTEGER   ARSIZE
      PARAMETER (ARSIZE = 4097)
c
      INTEGER   LUNPR, PFIND, NCH, ITRIM, I, K, L, KL, KMAXAM, ITER,
     *   KOFMAX, IERR
      DOUBLE PRECISION CONVRE, CONVIM, RC1(ARSIZE), RC2(ARSIZE), REF,
     *   FI, PHASE, PHASEC, REFF, IMFF, AMPMAX, RTEMP, REFAR(ARSIZE),
     *   IMFAR(ARSIZE), REFAMP, IMFAMP, REFMUL, IMFMUL, RECOMP, IMCOMP,
     *   RECOUT, IMCOUT, AMPRES, RERES, IMRES, PHOFF, IMF, ARG
      CHARACTER LINE*80
      INCLUDE 'TARS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      DO 20 I = 1,NL2AX
         IF ((VRE(I).EQ.DBLANK) .OR. (VIM(I).EQ.DBLANK)) THEN
            VRE(I) = 0.0D0
            VIM(I) = 0.0D0
            END IF
 20      CONTINUE
C                                       Find Fourier transorm of the
C                                       complex array VRE(I)+J*VIM(I)
      AMPMAX = 0.0D0
      DO 30 K = 1,FSIZE
         REF = 0.0D0
         IMF = 0.0D0
         FI = (K-HSIZE-1) * CELL
C                                       DFT with exp-(ARG)
         DO 25 I = 1,NL2AX
            ARG = 2.0D0 * FI * ARRL2(I)
            REF = REF + W(I)*(VRE(I)*COS(ARG) + VIM(I)*SIN(ARG))
            IMF = IMF + W(I)*(-VRE(I)*SIN(ARG) + VIM(I)*COS(ARG))
 25         CONTINUE
         REFAR(K) = REF / WSUM
         IMFAR(K) = IMF / WSUM
 30      CONTINUE
      AMPMAX = SQRT (AMPMAX)
C                                       start Clean
C
      ITER = 0
C                                       cycle by iteration(components)
C
 100  ITER = ITER + 1
C                                       find next component
      IF (ITER.LE.NITER) THEN
         CALL XXCLEN (REFAR, IMFAR, REFAMP, IMFAMP, KMAXAM, PHOFF)
C                                       subtracted component
         RECOMP = GAIN * REFAMP
         IMCOMP = GAIN * IMFAMP
         PHASE  = ATAN2 (IMFAMP, REFAMP)
         AMPMAX = SQRT (REFAMP*REFAMP + IMFAMP*IMFAMP)
         END IF
C                                       if big enough
      IF ((ITER.LE.NITER) .AND. (AMPMAX.GE.FLUX)) THEN
         WRITE (MSGTXT,1100)
         IF (ITER.EQ.1) CALL MSGWRT (4)
         WRITE (MSGTXT,1101) ITER, AMPMAX, PHASE*RAD2DG, KMAXAM, PHOFF
         CALL MSGWRT (4)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
         FI = (KMAXAM - HSIZE - 1) * CELL
         PHASEC = PHASE - 2.0D0 * FI * L2MEAN
         REFF = AMPMAX * COS (PHASEC)
         IMFF = AMPMAX * SIN (PHASEC)
         RECOUT = GAIN * REFF
         IMCOUT = GAIN * IMFF
C                                       iteration (component) are found
         RRE(KMAXAM) = RRE(KMAXAM) + RECOUT
         RIM(KMAXAM) = RIM(KMAXAM) + IMCOUT
C                                       Subtract the RMTF
C                                       multiplyed by the value of
C                                       RE/IM at max amplitude from
C                                       RE/IM
C                                       RE,IM of the subtracted
C                                       component located at
C                                       K = KMAXAM
         DO 110 K = 1,FSIZE
C                                       KOFMAX position of RMTF's max
C                                       relatively AMP max
            KOFMAX = K-KMAXAM+HSIZER+1
C                                       The RE, Im parts for the current
C                                       Multiply the positioned RMTF
C                                       by the subtracted component
            REFMUL = RECOMP*RERMT(KOFMAX) - IMCOMP*IMRMT(KOFMAX)
            IMFMUL = RECOMP*IMRMT(KOFMAX) + IMCOMP*RERMT(KOFMAX)

C                                       Subtract the component
            REFAR(K) = REFAR(K) - REFMUL
            IMFAR(K) = IMFAR(K) - IMFMUL
 110        CONTINUE
C                                       go to the next iteration
         GO TO 100
         END IF
C                                       End of CLEAN
C                                       Convolve the found set of
C                                       components with the RE of RMTF
C                                       or with the given Gaussian
      IF (DOCONV .AND. (CLONLY .OR. CLPLRE)) THEN
         DO 140 K = 1,FSIZE
            CONVRE = 0.0D0
            CONVIM = 0.0D0
            DO 130 L = 1,FSIZE
C                                       KL position of RMTF's max
C                                       relatively K
               KL = L - K + HSIZER+1
               IF (DOGAUS) THEN
                  CONVRE = CONVRE + GAUSS(KL)*RRE(L)
                  CONVIM = CONVIM + GAUSS(KL)*RIM(L)
               ELSE
                  CONVRE = CONVRE + RERMT(KL)*RRE(L)
                  CONVIM = CONVIM + RERMT(KL)*RIM(L)
                  END IF
  130          CONTINUE
C                                       Convolution at the pixel K done
            RC1(K) = CONVRE
            RC2(K) = CONVIM
  140       CONTINUE
         CALL DPCOPY (FSIZE, RC1, RRE)
         CALL DPCOPY (FSIZE, RC2, RIM)
         END IF
C
      IF ((DORES) .OR. (CLPLRE)) THEN
         DO 180 K = 1,FSIZE
            PHASE = ATAN2 (IMFAR(K), REFAR(K))
            AMPRES = SQRT (IMFAR(K)**2 + REFAR(K)**2)
C                                       subtract the phase at L2MEAN
C                                       to get the original component
C                                       corresponded to not shifted
C                                       L^2 set
            FI = (K-HSIZE-1) * CELL
            PHASEC = PHASE - 2.0D0 * FI * L2MEAN
C                                       Store the shifted back residual
            RERES = AMPRES * COS(PHASEC)
            IMRES = AMPRES * SIN(PHASEC)
C                                       Send the residual as output
            IF (DORES) THEN
               RRE(K) = RERES
               RIM(K) = IMRES
C                                       Send the residual plus convolved
C                                       into the output
            ELSE IF (CLPLRE) THEN
               RRE(K) = RRE(K) + RERES
               RIM(K) = RIM(K) + IMRES
               END IF
  180       CONTINUE
         END IF
C                                       calculate amplitudes
      IF (APARM(5).GE.1.5) THEN
         DO 200 K = 1,FSIZE
            RTEMP = RRE(K)
            RRE(K) = SQRT (RRE(K)*RRE(K) + RIM(K)*RIM(K))
            RIM(K) = RAD2DG * ATAN2 (RIM(K), RTEMP)
  200       CONTINUE
         END IF
C                                       open file for record the outputs
      IF (DOUTFI.GT.0) THEN
         LUNPR = 3
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR OPENING OUTPUT TEXT FILE'
            CALL MSGWRT (7)
            DOUTFI = -1
            END IF
         END IF
C                                       header of the output file:AMP
C                                       PHAS
      IF (APARM(5).GE.1.5) THEN
         WRITE (MSGTXT,2001)
      ELSE
         WRITE (MSGTXT,2005)
         END IF
      IF (DOUTFI.LE.0) THEN
         CALL MSGWRT (4)
      ELSE
         LINE = MSGTXT
         NCH = ITRIM (LINE)
         CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'ERROR WRITING OUTPUT TEXT FILE'
            IF (IERR.GT.0) CALL MSGWRT (7)
            DOUTFI = 0
            END IF
         END IF
C                                       record the lines
      DO 300 K = 1, FSIZE
         IF (APARM(5).GE.1.5) THEN
            WRITE (MSGTXT,2011)  K, (K-HSIZE-1)*CELL, RRE(K), RIM(K)
         ELSE
            WRITE (MSGTXT,2015)  K, (K-HSIZE-1)*CELL, RRE(K), RIM(K)
            END IF
         IF (DOUTFI.LE.0) THEN
            CALL MSGWRT (4)
         ELSE
            LINE = MSGTXT
            NCH = ITRIM (LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'ERROR WRITING OUTPUT TEXT FILE'
               IF (IERR.GT.0) CALL MSGWRT (7)
               DOUTFI = 0
               END IF
            END IF
  300    CONTINUE
C                                       close output file
      IF (DOUTFI.GE.0) CALL ZTXCLS (LUNPR, PFIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (' Iter  Amplitude  Phase Pixel  PhOff')
 1101 FORMAT (I5,1PE11.3,0PF7.1,I6,0PF7.1)
 2001 FORMAT (';   K',8X,'RM',8X,'10^6 AMP',7X,'PHASE')
 2005 FORMAT (';   K',8X,'RM',9X,'10^6 RE',7X,'10^6 IM')
 2011 FORMAT (I5,F12.1,6PF14.2,0PF12.2)
 2015 FORMAT (I5,F12.1,6PF14.2,6PF14.2)
      END
      SUBROUTINE XXCLEN (RERES, IMRES, REAMP, IMAMP, KMAX, PHOFF)
C-----------------------------------------------------------------------
C   XXCLEN tries a pattern-matching Clean to the input values at
C   various phase shifts and returns the location and value of the
C   found maximum.
C   Inputs:
C      RERES   R(*)   Real values of the row
C      IMRES   R(*)   Imaginary values of the
C   Outputs:
C      REAMP   R      Real value at the component
C      IMAMP   R      Imaginary value at the component
C      KMAX    I      pixel position of the maximum
C      PHOFF   R      phase offset from the input at the maximum
C-----------------------------------------------------------------------
      INTEGER   KMAX
      DOUBLE PRECISION RERES(*), IMRES(*), REAMP, IMAMP, PHOFF
C
      INTEGER   ARSIZE
      PARAMETER (ARSIZE = 4097)
C
      INCLUDE 'TARS.INC'
      INTEGER   I, J, K, L, LM, KM, J1, J2
      DOUBLE PRECISION PH, TRE(ARSIZE), TIM(ARSIZE), RMUL, IMUL, SUM,
     *   LREAMP, LIMAMP, AMPM

      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      AMPM = 0.0D0
      LM = -1
      KM = -1
      LREAMP = 0.0D0
      LIMAMP = 0.0D0
C                                       loop over phase offsets
      DO 100 L = 1,3600
         IF (L.EQ.1) THEN
            PH = 0.0D0
            CALL DPCOPY (FSIZE, RERES, TRE)
            CALL DPCOPY (FSIZE, IMRES, TIM)
         ELSE
            PH = (L - 1) * 0.100D0
            RMUL = COS (PH * DG2RAD)
            IMUL = SIN (PH * DG2RAD)
C                                       multiply by exp(-j ph)
            DO 20 K = 1,FSIZE
               TRE(K) = RERES(K) * RMUL + IMRES(K) * IMUL
               TIM(K) = IMRES(K) * RMUL - RERES(K) * IMUL
 20            CONTINUE
            END IF
         DO 50 K = 1,FSIZE
            SUM = 0.0D0
            J1 = MAX (1, K-KLIM)
            J2 = MIN (FSIZE, K+KLIM)
            DO 30 J = J1,J2
               I = HSIZER + 1 + J - K
               SUM = SUM + TRE(J) * RERMT(I) + TIM(J) * IMRMT(I)
 30            CONTINUE
            IF (SUM.GT.AMPM) THEN
               LM = L
               KM = K
               LREAMP = TRE(K)
               LIMAMP = TIM(K)
               AMPM = SUM
               END IF
 50         CONTINUE
 100     CONTINUE
C                                       return answers
      PHOFF = (LM - 1) * 0.100D0
      AMPM = SQRT (LREAMP*LREAMP + LIMAMP*LIMAMP)
      REAMP = AMPM * COS (PHOFF * DG2RAD)
      IMAMP = AMPM * SIN (PHOFF * DG2RAD)
      KMAX = KM
C
 999  RETURN
      END
