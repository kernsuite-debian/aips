LOCAL INCLUDE 'HOLGRP'
      INTEGER   MX, MXFAC, NLG, MXEQ
C                                       Maximum map size
      PARAMETER (MX = 512)
      PARAMETER (MXFAC = 2)
C                                       Beam double grid
      PARAMETER (NLG = MXFAC*MX)
      PARAMETER (MXEQ = MX/MXFAC)
LOCAL END
LOCAL INCLUDE 'HOLGRMX'
      INCLUDE 'HOLGRP'
C                                       Try to save core
      REAL      WAMP(NLG,NLG), WPHA(NLG,NLG)
      COMPLEX   VCPLX(NLG,NLG)
      INTEGER   PATCH(MX,MX)
      REAL      AAMP(MX,MX), APHA(MX,MX), PAMP(MX,MX), VAMP(MX,MX),
     *          VDEV(MX,MX), VPHA(MX,MX), WGT(MX,MX), PPHA(MX,MX),
     *          UNIF(MX,MX), PHAMOD(MX,MX)
      COMPLEX   ACPLX(MX,MX), SCPLX(MX,MX)
      COMMON /BUFRS/ WAMP, WPHA, VCPLX
C                                       Depends on the factor 2 above
C                                       VAMP/VPHA must NOT equiv VCPLX
      EQUIVALENCE (PATCH, PHAMOD(1,1))
      EQUIVALENCE (VAMP,  WAMP(1,1))
      EQUIVALENCE (VPHA,  WAMP(1,1+MXEQ))
      EQUIVALENCE (PAMP,  WAMP(1,1+2*MXEQ))
      EQUIVALENCE (PPHA,  WAMP(1,1+3*MXEQ))
      EQUIVALENCE (VDEV,  WPHA(1,1))
      EQUIVALENCE (WGT,   WPHA(1,1+1*MXEQ))
      EQUIVALENCE (UNIF,  WPHA(1,1+2*MXEQ))
      EQUIVALENCE (ACPLX, VCPLX(1,1))
      EQUIVALENCE (SCPLX, VCPLX(1,1+MXEQ))
C                                       wastes a little space
C                                       AAMP/APHA must equiv VCPLX
      EQUIVALENCE (AAMP,  VCPLX(1,1+2*MXEQ))
      EQUIVALENCE (APHA,  VCPLX(1,1+3*MXEQ))
LOCAL END
LOCAL INCLUDE 'HOLGR'
C                                       Local include for HOLGR.
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'HOLGRMX'
      LOGICAL    DODFT, DOXY, LINEAR, OPT(10), NOXYZ, NOZERO, UNIWT,
     *           X0Y0Z0
      INTEGER    LUNVIS, NPIX, INDVIS, ZIPA, ZIPV
      HOLLERITH  XINFIL(12), XOUTNA(3), XOPTYP
      REAL       XOUTDI, TAPER(2), XMAG, FREQ, EL0, DIAM, SUBDIA, FOCUS,
     *           X0, Y0, Z0, XANT, XSTOK, MAPSZ, XNPIX, LMMIN, LMMAX,
     *           AMPSCL, XFT, XYMIN, XYMAX, XCNTRL, XLINEA, LPRM(5),
     *           MPRM(5), XOPT(10), XOFFS(4), DISCR, DUMMY(3)
      COMPLEX    XTALK
      REAL       LAMBDA, PHSGN
      CHARACTER  INFILE*48, OUTNAM*36, OPTYPE*4
      COMMON /INPARM/ XINFIL, XOUTNA, XOUTDI, TAPER, XOPTYP, XMAG, FREQ,
     *   EL0, DIAM, SUBDIA, FOCUS, X0, Y0, Z0, XANT, XSTOK, MAPSZ,
     *   XNPIX, LMMIN, LMMAX, AMPSCL, XFT, XYMIN, XYMAX, XCNTRL, XLINEA,
     *   LPRM, MPRM, XOPT, XTALK, XOFFS, DISCR, DUMMY
      COMMON /INTLOG/ NPIX, PHSGN, DODFT, ZIPA, X0Y0Z0, ZIPV, DOXY,
     *   LINEAR, OPT, LAMBDA, LUNVIS, NOXYZ, NOZERO, UNIWT, INDVIS
      COMMON /INCHAR/ INFILE, OUTNAM, OPTYPE
LOCAL END
      PROGRAM HOLGR
C-----------------------------------------------------------------------
C! Read and process holography visibility data
C# UV ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2015
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
C   Driver task for the holography package under AIPS.
C   Adverbs:
C      INFILE(12)  Input visibility file name.
C      OUTNAME(3)  Output image name.
C      OUTDISK     Output disk number.
C      LMTAPER     Taper to apply in gridding.
C      OPTYPE      The feed model (prime focus or subreflector).
C      FACTOR      The magnification (for subreflector model).
C      APARM(10)   Operating parameters.
C                      1: Observing frequency (GHz).
C                      2: Satellite elevation, degrees.
C                      3: Antenna diameter, in metres.
C                      4: Subreflector diameter, in metres.
C                      5: Focal length, in metres.
C                      6: Default offset of the antenna vertex in x
C                         from the intersection of the azimuth and
C                         elevation axes, in metres.  See BPARM(9).
C                      7: As for 6 in the y direction.
C                      8: As for 6 in the z direction.
C                      9: 10000*refant+100*scanant+IF#
C                     10: Stokes (1-4; RR,LL,RL,LR)
C  mjk (4/feb/93) : aparm(9 & 10) are used in generating the input
C  and output filenames if triggered by INFILE = 'AREA:'.
C      BPARM(10)   Data reduction parameters.
C                      1: Required map size, in metres.
C                      2: Number of pixels on a side of the output
C                         map (power of 2, maximum 512).
C                    3,4: Range of |l| and |m| to include.  Negative
C                         values denote a range of SQRT(l*l + m*m).
C                      5: Amplitude scaling factor (0 -> 1).
C                      6: Fourier transform control.  If negative,
C                         the phase read from the data file is
C                         negated.  If the absolute value is 2, a
C                         direct (slow) Fourier transform will be
C                         done, otherwise, an FFT.
C                    7,8: Range of |l| and |m| used in correcting
C                         for pointing, focus, and feed offset.
C                         Negative values denote a range of
C                         SQRT(l*l + m*m).  See also BPARM(9).
C                      9: Decimal encoded control parameters
C                            1: inhibit phase unwrapping of the
C                               A_PHA map,
C                           10: inhibit determination of a phase
C                               ramp in the (l,m) data and use
C                               APARM(6,7,8) instead.
C                          100: inhibit phase unwrapping of the
C                               V_PHA map,
C                         1000: inhibit correction for pointing,
C                               focus, and feed offset;
C                         2000: inhibit focus and feed offset only.
C                         4000: inhibit the phase offset term.
C                          In general, you want to leave = 0.
C                     10: +1 for logarithmic amplitudes,
C                         -1 for linear amplitudes
C      CPARM(10)   Parameters for the regridding operation.
C                      1: Type of interpolation to apply in l,
C                          1: Pillbox,
C                          2: Exponential,
C                          3: Sinc,
C                          4: Sinc*Exponential,
C                          5: Spheroidal (default).
C   >>>>                   NEGATIVE means natural weighting
C                      2: Support size in l.
C                    3-5: Parameters defining the interpolation
C                         function in l.
C                   6-10: Corresponding parameters for m.
C      DPARM(10)   Output option flags.  The particular map will be
C                  stored if the associated DPARM is non-zero.  If
C                  all of the DPARM are zero, DPARM(4,5) are assumed
C                  to be set.
C                    1,2: Regridded amplitude and phase of the
C                         observed antenna beam pattern.
C                      3: Weights used in the regridding procedure.
C                    4,5: Derived amplitude and phase of the voltage
C                         distribution across the antenna aperture.
C                    6,7: Amplitude and phase of the point-spread
C                         function.  This indicates the blurring in
C                         the derived voltage distribution.
C                      8: Focus model corrections, in degrees.
C                      9: Map of the surface deviations of the
C                         antenna.
C                     10: The interpolated antenna power pattern.
C                         Oversampling is supported.  See HELP
C      XPARM(10)   Additional data reduction parameters.
C                    1,2: Cross-talk correction, amp & phase.
C                    3-6: Offset beam correction, azimuth and elevation
C                         offsets (deg), amplitude factor and phase
C                         offset (deg).
C   Notes:
C      1) The coordinate systems used here are as follows:
C
C         Antenna aperture coordinates are described by a right-handed
C         system, (x,y,z), centred on the point of intersection of the
C         azimuth and elevation axes.  The xy-plane is parallel to the
C         aperture plane; the x-axis is parallel to the elevation axis
C         and increases to the right as the dish is seen from above.
C         The y-axis increases towards the top of the dish; the z-axis
C         points skyward more-or-less along the optical axis.
C
C         (l,m,n) are the direction cosines which correspond to the
C         (x,y,z) coordinates.  Note that the (l,m) plane is projected
C         onto the sky with the l-axis in the direction of decreasing
C         azimuth and the n-axis towards increasing elevation.  It is
C         therefore left-handed as seen from the earth.
C
C      2) Prefix usage for the arrays:
C         A  denotes the antenna pattern, measured or interpolated.
C         S  denotes the sampling function in (l,m) space.
C         V  denotes the derived voltage distribution across the
C            antenna aperture.
C         P  denotes the point-spread function of the derived voltage
C            distribution.
C
C         A,V and S,P are Fourier transform pairs.
C
C   Authors: Mike Kesteven & Mark Calabretta, Australia Telescope.
C      Origin; 1987/11     Code last modified; 1996/08/20
C-----------------------------------------------------------------------
      INTEGER   IERR, KMAX, MVIS, NH, NVIS, ILG, IT, MXINC, MFAC, ID(3),
     *          WSTAT(6), XVIS
      REAL      AZPEAK, CELLLM, CELLXY, DFX, DFY, DFZ, DL, DM, DP0, DPX,
     *   DPY, DX0, DY0, DZ0, ELPEAK, F1, FAZ, FEL, FL, FM, FX, FY, FZ,
     *   L1, LKRNL(0:700), LPEAK, MGAIN(5), MKRNL(0:700), MPEAK, P0,
     *   PAMPMX, PAZ, PEL, PINC, PL, PM, PX, PY, RMAX, RMIN, RMS, RMS0,
     *   TGAIN(5), VAMPMX
      CHARACTER DATOBS*8, DATMAP*8, HISTRY(50)*72, INSTRM*8, OBJECT*8,
     *   OBSERV*8, TELESC*8, CHUV(2)*8, CHXY(2)*8, CHAZEL(2)*8,
     *   CHUNIT(5)*8
      INCLUDE 'HOLGR'
      REAL      XFTK(0:MX), YFTK(0:MX)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA DATOBS, INSTRM, OBJECT, TELESC, OBSERV /5*' '/
      DATA CHXY, CHUV, CHAZEL /'X','Y','U','V','Az','El'/
      DATA CHUNIT /'VOLTS','DEGREES','METERS','DB',' '/
C-----------------------------------------------------------------------
C                                       Initialization.
      LUNVIS = 3
      CALL HOLINI (IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
      CALL ZDATE (ID)
      WRITE (DATMAP,1001) ID
C                                       Cell sizes in (x,y) and (l,m).
      CELLXY = MAPSZ / NPIX
      CELLLM = LAMBDA / MAPSZ
C                                       Radii in pixel units.
      RMIN = (SUBDIA/2.0) / CELLXY
      RMAX = (DIAM/2.0) / CELLXY
C                                       Interpolate the visibilities
C                                       onto a regular grid.
      DL = 0.01
      DM = 0.01
      CALL KERNEL (LPRM, DL, LKRNL)
      CALL KERNEL (MPRM, DM, MKRNL)
      CALL GRID (NPIX, EL0, CELLLM, LKRNL, LPRM(2), DL, MKRNL, MPRM(2),
     *   DM, TAPER, XTALK, XOFFS, NVIS, MVIS, XVIS, AAMP, APHA, ACPLX,
     *   SCPLX, WGT, LPEAK, MPEAK, UNIWT, UNIF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       History text.
      WRITE (HISTRY(1),1020) TSKNAM, INFILE
      WRITE (HISTRY(2),1030) TSKNAM, FREQ, LAMBDA
      WRITE (HISTRY(3),1040) TSKNAM, EL0
      WRITE (HISTRY(4),1050) TSKNAM, DIAM
      WRITE (HISTRY(5),1060) TSKNAM, SUBDIA
      WRITE (HISTRY(6),1070) TSKNAM, FOCUS
      WRITE (HISTRY(7),1080) TSKNAM, MVIS, NVIS
      NH = 7
      IF (XOFFS(3).NE.0.0) THEN
         WRITE (HISTRY(8), 1081) TSKNAM, XVIS
         WRITE (HISTRY(9), 1082) TSKNAM, XOFFS(1), XOFFS(2)
         WRITE (HISTRY(10),1083) TSKNAM, XOFFS(3), XOFFS(4)*RAD2DG
         NH = 10
         END IF
C                                       Convert peak position to arcmin.
      AZPEAK = -ASIN (LPEAK/ COS(EL0*DG2RAD)) * RAD2DG * 60.0
      WRITE (MSGTXT,1085) AZPEAK
      CALL MSGWRT (3)
      NH = NH + 1
      HISTRY(NH) = TSKNAM // MSGTXT
      ELPEAK = ASIN (MPEAK) * RAD2DG * 60.0
      WRITE (MSGTXT,1086) ELPEAK
      CALL MSGWRT (3)
      NH = NH + 1
      HISTRY(NH) = TSKNAM // MSGTXT
C                                       Unwrap antenna pattern phase?
      IF (ZIPA.GT.0) THEN
         CALL ZIPAN (ZIPA, NPIX, RMAX, RMIN, WGT, AAMP, APHA, PATCH,
     *      WSTAT)
         WRITE (MSGTXT,1090) WSTAT(1)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C
         WRITE (MSGTXT,1094) WSTAT(2)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C
         WRITE (MSGTXT,1096) WSTAT(3)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C
         IF (ZIPA.GT.1) THEN
            WRITE (MSGTXT,1098) WSTAT(4)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C
            WRITE (MSGTXT,1094) WSTAT(5)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C
            WRITE (MSGTXT,1100) WSTAT(6)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            END IF
         END IF
C                                       Correct the visibilities for the
C                                       antenna offset.
      IF (X0Y0Z0) THEN
         CALL XYZOFS (LAMBDA, CELLLM, NPIX, WGT, APHA, ACPLX, X0, Y0,
     *      Z0, DX0, DY0, DZ0, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1110)
            CALL MSGWRT (6)
            END IF
         END IF
      IF (X0Y0Z0) THEN
         WRITE (MSGTXT,1120) X0, DX0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1130) Y0, DY0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1140) Z0, DZ0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
      ELSE
         WRITE (MSGTXT,1150) X0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1160) Y0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1170) Z0
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         END IF
      NH = NH + 1
      WRITE (HISTRY(NH),1180) TSKNAM, MAPSZ, NPIX
      NH = NH + 1
      WRITE (HISTRY(NH),1190) TSKNAM, LMMIN, LMMAX
      NH = NH + 1
      WRITE (HISTRY(NH),1200) TSKNAM, AMPSCL
      IF (DODFT) THEN
         IF (PHSGN.LT.0) WRITE (HISTRY(NH),1210) TSKNAM
         IF (PHSGN.GT.0) WRITE (HISTRY(NH),1220) TSKNAM
      ELSE
         IF (PHSGN.LT.0) WRITE (HISTRY(NH),1230) TSKNAM
         IF (PHSGN.GT.0) WRITE (HISTRY(NH),1240) TSKNAM
         END IF
      NH = NH + 1
      WRITE (HISTRY(NH),1250) TSKNAM, XYMIN, XYMAX
      NH = NH + 1
      WRITE (HISTRY(NH),1260) TSKNAM, LPRM
      NH = NH + 1
      WRITE (HISTRY(NH),1270) TSKNAM, MPRM
      NH = NH + 1
      WRITE (HISTRY(NH),1280) TSKNAM, OPT
C                                       Save the regridded maps if
C                                       required.  Store the amplitude
C                                       part of the antenna pattern.
      IF (OPT(1)) THEN
         OUTNAM(13:18) = 'A_AMP'
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, FREQ, CHUNIT(1), CHUV(1), NPIX, -CELLLM, CHUV(2),
     *      NPIX, CELLLM, AAMP, HISTRY, NH, MVIS, IERR)
         END IF
C                                       Store phase part of ant pattern
      IF (OPT(2)) THEN
         OUTNAM(13:18) = 'A_PHA'
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, FREQ, CHUNIT(2), CHUV(1), NPIX, -CELLLM, CHUV(2),
     *      NPIX, CELLLM, APHA, HISTRY, NH, MVIS, IERR)
         END IF
C                                       Store the weighting function
      IF (OPT(3)) THEN
         OUTNAM(13:18) = 'WGT'
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, FREQ, CHUNIT(5), CHUV(1), NPIX, -CELLLM, CHUV(2),
     *      NPIX, CELLLM, WGT, HISTRY, NH, MVIS, IERR)
         END IF
C                                       Compute the aperture voltage
C                                       distribution and point-spread
C                                       function.
      IF (OPT(4) .OR. OPT(5) .OR. OPT(6) .OR. OPT(7) .OR. OPT(8). OR.
     *   OPT(9) .OR. OPT(10)) THEN
C                                       Direct Fourier Transform.
         IF (DODFT) THEN
            CALL DFT (LAMBDA, NPIX, CELLXY, CELLXY, XTALK, VAMP, VPHA,
     *         PAMP, PPHA, VAMPMX, IERR)
C                                       Compute the gridding correction
         ELSE
            KMAX = NINT(LPRM(2)/DL)
            CALL FTKRNL (LKRNL, DL, KMAX, NPIX, XFTK)
            KMAX = NINT(MPRM(2)/DM)
            CALL FTKRNL (MKRNL, DM, KMAX, NPIX, YFTK)
C                                       Fast Fourier Transform.
            IF (OPT(4) .OR. OPT(5) .OR. OPT(8) .OR. OPT(9) .OR.
     *         OPT(10)) THEN
               CALL HOLFFT (-1, NPIX, ACPLX, VAMP, VPHA)
               CALL GRIDCR (XFTK, YFTK, NPIX, VAMP, VAMPMX)
               END IF
            IF (OPT(6) .OR. OPT(7)) THEN
               CALL HOLFFT (-1, NPIX, SCPLX, PAMP, PPHA)
               CALL GRIDCR (XFTK, YFTK, NPIX, PAMP, PAMPMX)
               END IF
            END IF
C                                       Report resulting amplitude.
         WRITE (MSGTXT,1290) VAMPMX
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C                                       Blank shadowed and empty areas
         CALL BLANK (RMAX, RMIN, NPIX, VPHA)
C                                       Unwrap the phase map.
         IF (ZIPV.GT.0) THEN
            CALL UNWRAP (ZIPV, NPIX, RMAX, RMIN, VAMP, DISCR, VPHA,
     *         PATCH, WSTAT, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'Error unwrapping the V_PHA phase map.'
               CALL MSGWRT (6)
               END IF
C
            WRITE (MSGTXT,1300) WSTAT(1)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C
            WRITE (MSGTXT,1304) WSTAT(2)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C
            WRITE (MSGTXT,1306) WSTAT(3)
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C
            IF (ZIPV.GT.1) THEN
               WRITE (MSGTXT,1308) WSTAT(4)
               CALL MSGWRT (3)
               NH = NH + 1
               HISTRY(NH) = TSKNAM // MSGTXT
C
               WRITE (MSGTXT,1304) WSTAT(5)
               CALL MSGWRT (3)
               NH = NH + 1
               HISTRY(NH) = TSKNAM // MSGTXT
C
               WRITE (MSGTXT,1310) WSTAT(6)
               CALL MSGWRT (3)
               NH = NH + 1
               HISTRY(NH) = TSKNAM // MSGTXT
               END IF
            END IF
C                                       Correct phase for pointing,
C                                       focus, and feed offset.
         IF (DOXY) THEN
            CALL FLATPH (OPTYPE, XMAG, NPIX, LAMBDA, FOCUS, XYMIN,
     *         XYMAX, CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ,
     *         DP0, DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXYZ,
     *         NOZERO, PHAMOD)
            IF (OPTYPE.EQ.'PFOC') THEN
               MSGTXT = 'Prime focus phase model'
            ELSE
               WRITE (MSGTXT,1330) XMAG
               END IF
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
C                                       Convert offsets to
C                                       direction cosines.
            PL = -(LAMBDA*PX/360.0)/CELLXY
            PM = -(LAMBDA*PY/360.0)/CELLXY
            FL = -(0.001*FX/FOCUS)*0.85
            FM = -(0.001*FY/FOCUS)*0.85
C                                       Convert to (Az,El) offsets
C                                       in arcmin.
            PAZ = -ASIN (PL / COS(EL0*DG2RAD)) * RAD2DG * 60.0
            PEL =  ASIN (PM) * RAD2DG * 60.0
            FAZ = -ASIN (FL / COS(EL0*DG2RAD)) * RAD2DG * 60.0
            FEL =  ASIN (FM) * RAD2DG * 60.0
C
            CALL BLANK (RMAX, RMIN, NPIX, PHAMOD)
            WRITE (MSGTXT,1340) P0, DP0
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1350) PX, DPX
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1360) PY, DPY
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1370) FX, DFX
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1380) FY, DFY
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1390) FZ, DFZ
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            MSGTXT = 'Equivalent pointing offsets:'
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1400) PAZ
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1410) PEL
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1420) FAZ
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1430) FEL
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1440) PAZ+FAZ
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1450) PEL+FEL
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1460) RMS0
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
            WRITE (MSGTXT,1470) RMS
            CALL MSGWRT (3)
            NH = NH + 1
            HISTRY(NH) = TSKNAM // MSGTXT
         ELSE
            NH = NH + 1
            HISTRY(NH) = TSKNAM // 'Phase correction suppressed.'
            END IF
C                                       Compute surface deviation map.
         CALL SURDEV (CELLXY, LAMBDA, FOCUS, NPIX, VPHA, VDEV)
C                                       Compute beam gain at obs freq
         F1 = 1.0
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, LAMBDA, VAMP, VPHA, F1,
     *      MGAIN(1), TGAIN(1))
         WRITE (MSGTXT,1480) MGAIN(1)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1490) TGAIN(1)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C                               Compute the zero-rms gain
         F1 = 0.0
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, LAMBDA, VAMP, VPHA, F1,
     *      MGAIN(1), TGAIN(1))
         WRITE (MSGTXT,1500) MGAIN(1)
         CALL MSGWRT (3)
C                                       K band estimate
         L1 = 0.0133
         F1 = LAMBDA/L1
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, L1, VAMP, VPHA, F1,
     *      MGAIN(2), TGAIN(2))
         WRITE (MSGTXT,1510) MGAIN(2)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1520) TGAIN(2)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, L1, VAMP, PHAMOD, F1,
     *      MGAIN(2), TGAIN(2))
         WRITE (MSGTXT,1530) MGAIN(2)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C                                       Q band estimate
         L1 = 0.007
         F1 = LAMBDA/L1
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, L1, VAMP, VPHA, F1,
     *    MGAIN(3), TGAIN(3))
         WRITE (MSGTXT,1540) MGAIN(3)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
         WRITE (MSGTXT,1550) TGAIN(3)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C                               Compute Gain loss due to feed offset
         CALL ANGAIN (NPIX, MAPSZ, RMAX, RMIN, L1, VAMP, PHAMOD, F1,
     *    MGAIN(3), TGAIN(3))
         WRITE (MSGTXT,1560) MGAIN(3)
         CALL MSGWRT (3)
         NH = NH + 1
         HISTRY(NH) = TSKNAM // MSGTXT
C                                       Store amplitude part of voltage
C                                       distribution
         IF (OPT(4)) THEN
            OUTNAM(13:18) = 'V_AMP'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(1), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VAMP, HISTRY, NH, MVIS, IERR)
            END IF
C                                       Store phase part of voltage
C                                       distribution
         IF (OPT(5)) THEN
            OUTNAM(13:18) = 'V_PHA'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VPHA, HISTRY, NH, MVIS, IERR)
            END IF
C                                       Store amplitude part of point
C                                       spread function
         IF (OPT(6)) THEN
            OUTNAM(13:18) = 'P_AMP'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(1), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PAMP, HISTRY, NH, MVIS, IERR)
            END IF
C                                       Store phase part of point spread
C                                       function
         IF (OPT(7)) THEN
            OUTNAM(13:18) = 'P_PHA'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PPHA, HISTRY, NH, MVIS, IERR)
            END IF
C                                       Store the model corrections
         IF (OPT(8)) THEN
            OUTNAM(13:18) = 'MODEL'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PHAMOD, HISTRY, NH, MVIS, IERR)
            END IF
C                                       Store the surface deviation map
         IF (OPT(9)) THEN
            OUTNAM(13:18) = 'V_DEV'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, FREQ, CHUNIT(3), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VDEV, HISTRY, NH, MVIS, IERR)
            END IF
         END IF
C                                       Compute the power pattern
      IF (OPT(10)) THEN
         MFAC = XOPT(10) + 0.75
         MFAC = MAX (MFAC, MXFAC)
         IT = (MXFAC*MX)/NPIX
         IF (MFAC.GT.IT) THEN
            MXINC = 2
            MFAC = IT
         ELSE
            MXINC = 1
            END IF
         ILG = MFAC*NPIX
         CALL BEAM (NPIX, ILG, MXINC, VAMP, VPHA, AAMP, APHA, VCPLX,
     *      WAMP, WPHA)
C                                       Store the power pattern.
         OUTNAM(13:18) = 'A_PWR'
         PINC = (RAD2DG*CELLLM*NPIX)/(MXINC*ILG)
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, FREQ, CHUNIT(4), CHAZEL(1), NPIX, -PINC, CHAZEL(2),
     *      NPIX, PINC, AAMP, HISTRY, NH, MVIS, IERR)
         END IF
C                                       Compute the visibility coordinat
C                                       Clean up.
 990  CALL TSKEND (IERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' INITIATING HOLGR.')
 1001 FORMAT (I4.4,I2.2,I2.2)
 1010 FORMAT ('ERROR',I4,' READING/GRIDING THE VISIBILITIES.')
 1020 FORMAT (A,' Visibility file: ',A)
 1030 FORMAT (A,'    Observing frequency:',F9.4,' GHz, wavelength:',
     *   F10.6,' m')
 1040 FORMAT (A,'    Satellite elevation:',F8.3,' deg')
 1050 FORMAT (A,'       Antenna diameter:',F8.3,' m')
 1060 FORMAT (A,'  Subreflector diameter:',F8.3,' m')
 1070 FORMAT (A,'           Focal length:',F8.3,' m')
 1080 FORMAT (A,'      Visibilities used:',I7,' of',I7)
 1081 FORMAT (A,'        Offset beam vis:',I7)
 1082 FORMAT (A,'    Offset beam (Az,El):',2F8.3)
 1083 FORMAT (A,'  Offset beam amp/phase:',2F8.3)
 1085 FORMAT ('    Source offset in Az:',F10.2,' arcmin')
 1086 FORMAT ('    Source offset in El:',F10.2,' arcmin')
 1090 FORMAT (' Initial A_PHA wrapping:',I6)
 1094 FORMAT ('      Number of patches:',I6)
 1096 FORMAT ('         A_PHA wrapping:',I6)
 1098 FORMAT ('     Number of closures:',I6)
 1100 FORMAT ('   Final A_PHA wrapping:',I6)
 1110 FORMAT ('Error correcting for the antenna offset, continuing.')
 1120 FORMAT ('    Antenna offset in x:',F8.3,F7.3,' m')
 1130 FORMAT ('    Antenna offset in y:',F8.3,F7.3,' m')
 1140 FORMAT ('    Antenna offset in z:',F8.3,F7.3,' m')
 1150 FORMAT ('    Antenna offset in x:',F8.3,' m (as specified)')
 1160 FORMAT ('    Antenna offset in y:',F8.3,' m (as specified)')
 1170 FORMAT ('    Antenna offset in z:',F8.3,' m (as specified)')
 1180 FORMAT (A,'               Map size:',F8.3,' m,',I5,' pixels')
 1190 FORMAT (A,'            (l,m) range:',2F10.5)
 1200 FORMAT (A,'    Amplitude scaled by:',F8.3)
 1210 FORMAT (A,'      Fourier transform: DFT with phase negation')
 1220 FORMAT (A,'      Fourier transform: DFT without phase negation')
 1230 FORMAT (A,'      Fourier transform: FFT with phase negation')
 1240 FORMAT (A,'      Fourier transform: FFT without phase negation')
 1250 FORMAT (A,'            (x,y) range:',2F10.5,' m')
 1260 FORMAT (A,'Interpolation type in l: ',5F7.3)
 1270 FORMAT (A,'Interpolation type in m: ',5F7.3)
 1280 FORMAT (A,'    Output option flags: ',10L1)
 1290 FORMAT ('         Amplitude peak:',1P,E12.3)
 1300 FORMAT (' Initial V_PHA wrapping:',I6)
 1304 FORMAT ('      Number of patches:',I6)
 1306 FORMAT ('         V_PHA wrapping:',I6)
 1308 FORMAT ('     Number of closures:',I6)
 1310 FORMAT ('   Final V_PHA wrapping:',I6)
 1330 FORMAT ('Subreflector phase model with factor:',F6.2)
 1340 FORMAT ('           Phase offset:',F10.2,F9.2,' deg')
 1350 FORMAT ('    Phase gradient in x:',F10.2,F9.2,' deg/cell')
 1360 FORMAT ('    Phase gradient in y:',F10.2,F9.2,' deg/cell')
 1370 FORMAT (' Feed is offset in x by:',F10.2,F9.2,' mm')
 1380 FORMAT (' Feed is offset in y by:',F10.2,F9.2,' mm')
 1390 FORMAT (' Feed is offset in z by:',F10.2,F9.2,' mm')
 1400 FORMAT ('   Dish component in Az:',F10.2,' arcmin')
 1410 FORMAT ('   Dish component in El:',F10.2,' arcmin')
 1420 FORMAT ('   Feed component in Az:',F10.2,' arcmin')
 1430 FORMAT ('   Feed component in El:',F10.2,' arcmin')
 1440 FORMAT ('    Source offset in Az:',F10.2,' arcmin')
 1450 FORMAT ('    Source offset in El:',F10.2,' arcmin')
 1460 FORMAT (' Pre-fit weighted rms half-path error:',F8.3,' mm')
 1470 FORMAT ('Post-fit weighted rms half-path error:',F8.3,' mm')
 1480 FORMAT ('      Measured gain with observed illumination:',
     *  F7.2,' dB')
 1490 FORMAT ('    Theoretical gain with uniform illumination:',
     *  F7.2,' dB')
 1500 FORMAT ('                    Gain with no panel errors :',
     *  F7.2,' dB')
 1510 FORMAT ('22.5 GHz estimated gain (observed illumination) :',
     *  F7.2,' dB')
 1520 FORMAT ('22.5 GHz theoretical gain (uniform illumination):',
     *  F7.2,' dB')
 1530 FORMAT ('22.5 GHz estimated gain with only feed offset   :',
     *  F7.2,' dB')
 1540 FORMAT ('Q-band estimated gain (observed illumination) :',
     *  F7.2,' dB')
 1550 FORMAT ('Q-band theoretical gain (uniform illumination):',
     *  F7.2,' dB')
 1560 FORMAT ('Q-band estimated gain with only feed offset   :',
     *  F7.2,' dB')
      END
      SUBROUTINE HOLINI (IERR)
C-----------------------------------------------------------------------
C   HOLINI reads adverbs for HOLGR.
C   Returned:
C      IERR     I      Error status, 0 means success.
C   Returned in commons INPARM, INTLOG, INCHAR:
C      INFILE   C*48   Input visibility file name.
C      OUTNAM   C*36   WAWA image namestring.
C      FREQ     R      Observing frequency, in GHz.
C      LAMBDA   R      Observing wavelength, in metres.
C      EL0      R      Satellite elevation, in degrees.
C      DIAM     R      Antenna diameter, in metres.
C      SUBDIA   R      Subreflector diameter, in metres.
C      FOCUS    R      Focal length, in metres.
C      X0       R      Offset of the antenna vertex in (x,y,z)
C      Y0              from the intersection of the azimuth and
C      Z0              elevation axes, in metres.
C      MAPSZ    R      Size of the map, in metres.
C      NPIX     I      Number of pixels on a side of the map.
C      LMMIN    R      Range of |l| and |m| to include.  Negative
C      LMMAX           values denote a range of SQRT(l*l + m*m).
C      AMPSCL   R      Amplitude scaling factor.
C      PHSGN    R      Factor by which to multiply the measured phase.
C      DODFT    L      If true do a DFT, else an FFT.
C      XYMIN    R      Range of |x| and |y| used in correcting for
C      XYMAX    R      pointing, focus, and feed offset. Negative
C                      values denote a range of SQRT(x*x + y*y).
C      ZIPA     I      Antenna pattern map (A_PHA) unwrapping control
C                      flag
C                         0: No unwrapping.
C                         1: Unwrap closed discontinuities (PHAZIP).
C                         2: Perform closure operation (PHCLOS).
C      X0Y0Z0   L      If true, determine a phase ramp in the (l,m) data
C                      and correct for it.  Otherwise use (x0,y0,z0).
C      ZIPV     I      Antenna aperture plane (V_PHA) unwrapping control
C                      flag
C                         0: No unwrapping.
C                         1: Unwrap closed discontinuities (PHAZIP).
C                         2: Perform closure operation (PHCLOS).
C      DOXY     L      If true, correct the V_PHA map for pointing,
C                      focus, and feed offset.
C      LINEAR   L      If true, the input amplitudes are linear, else
C                      logarithmic
C      LPRM     R(5)   Interpolation parameters in l.
C      MPRM     R(5)   Interpolation parameters in m.
C      UNIWT    L      Uniform weighting flag
C      OPT      L(10)  Output option flags.
C      XTALK    CX     Cross-talk correction.
C      XOFFS    R(4)   Offset beam correction, azimuth and elevation
C                      offsets (deg), amplitude factor and phase
C                      offset (radian).
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   ANY
      INTEGER   CONTRL, I, IA, IB, IIF, IROUND, IS, J, JTRIM, LN, OUTDI
      REAL      AMP, PHA
      CHARACTER CHOUTN*12, PRGNAM*6, STOK(4)*2
      INCLUDE 'HOLGR'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA PRGNAM /'HOLGR '/
      DATA STOK /'RR', 'LL', 'RL', 'LR'/
C-----------------------------------------------------------------------
C                                       Get the adverbs and restart AIPS
      CALL TSKBEG (PRGNAM, 70, XINFIL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING ADVERB VALUES'
         GO TO 995
         END IF
C                                       Decode the input file name.
      CALL H2CHR (48, 1, XINFIL, INFILE)
C                                       Construct default file name
      IA = XANT + 0.1
      IB = IA/10000
      IIF = MOD (IA, 100)
      IA = IA/100
      IA = MOD (IA, 100)
      IA = MAX (1, MIN (IA, MAXANT))
      IB = MAX (0, MIN (IB, MAXANT))
      IIF = MAX (0, MIN (IIF, MAXIF))
      IS = XSTOK + 0.1
      IS = MAX (1, MIN (IS, 4))
      IF (INFILE.EQ.'ANT' .OR. INFILE.EQ.' ') INFILE = 'FITS:'
      LN = JTRIM (INFILE)
      IF (INFILE(LN:LN).EQ.':') THEN
         WRITE (INFILE,1010) IA, IB, STOK(IS), IIF
         END IF
C                                       get frequency from INFILE
      CALL FRQGET (INFILE, LUNVIS, FREQ, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, ' FINDING FREQUENCY IN INFILE'
         GO TO 995
         END IF
C                                       Output file name (the class will
C                                       be filled in by HOLGR).
      CALL H2CHR (12, 1, XOUTNA, CHOUTN)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF (OPTYPE.NE.'SUBR') OPTYPE = 'PFOC'
C                                       also fix the output filename
      IF (CHOUTN.EQ.' ') CHOUTN = 'HOLO'
      LN = JTRIM (CHOUTN)
      IF (LN.LE.5) WRITE (CHOUTN(6:),1011) IA, IB, STOK(IS)
      OUTDI = IROUND (XOUTDI)
      CALL A2WAWA (CHOUTN, ' ', 0, 'MA', OUTDI, NLUSER, OUTNAM)
C                                       Observing parameters.
C                                       Observing frequency (GHz), and
C                                       wavelength (m).
      IF (FREQ.LE.0.0) THEN
         MSGTXT = 'HOLINI: The observing frequency MUST be specified.'
         GO TO 990
         END IF
      LAMBDA = VELITE/(FREQ*1.0E9)
C                                       Antenna diameter, in metres,
      IF (DIAM.LE.0.0) THEN
         MSGTXT = 'HOLINI: The antenna diameter MUST be specified.'
         GO TO 990
         END IF
C                                       Subreflector diameter, in metres
      IF (SUBDIA.LE.0.0) THEN
         MSGTXT = 'HOLINI: The subreflector diameter MUST be specified.'
         GO TO 990
         END IF
C                                       Focal length, in metres
      IF (FOCUS.LE.0.0) THEN
         MSGTXT = 'HOLINI: The focal length MUST be specified.'
         GO TO 990
         END IF
C                                       Compute the visibility coordinat
C                                       Data reduction parameters.
C                                       Actual map size, in metres.
      IF (MAPSZ.LE.0.0) THEN
         MSGTXT = 'HOLINI: The map size MUST be specified.'
         GO TO 990
         END IF
C                                       Compute the visibility coordinat
C                                       Output must be power of 2
      NPIX = IROUND (XNPIX)
      IF (NPIX.EQ.0) NPIX = MX
      LN = NPIX
      IF (NPIX.GT.MX) NPIX = MX
      IF (NPIX.LT.32) NPIX = 32
      I = 16
 10   I = I*2
      IF (I.GE.MX) GO TO 15
         IF (NPIX.GT.I .AND. NPIX.LT.I*2) NPIX = I*2
         GO TO 10
 15   IF (NPIX.NE.LN) THEN
         WRITE (MSGTXT,1015) LN, NPIX
         CALL MSGWRT (4)
         END IF
C                                       Amplitude scaling factor.
      IF (AMPSCL.EQ.0.0) AMPSCL = 1.0
C                                       Fourier transform control.
      CONTRL = IROUND (XFT)
      IF (CONTRL.EQ.0) CONTRL = 1
C                                       Phase negation.
      PHSGN = SIGN(1, CONTRL)
C                                       DFT or FFT?
      DODFT = ABS(CONTRL).EQ.2
C                                       Decode control parameters.
      CONTRL = IROUND (XCNTRL)
      NOZERO = .FALSE.
      NOXYZ  = .FALSE.
      DOXY   = .TRUE.
      ZIPV   = 2
      X0Y0Z0 = .TRUE.
      ZIPA   = 2
      IF (CONTRL.GE.4000) THEN
         NOZERO = .TRUE.
         CONTRL = MOD(CONTRL,4000)
         END IF
      IF (CONTRL.GE.2000) THEN
         NOXYZ = .TRUE.
         CONTRL = MOD(CONTRL,2000)
         END IF
      IF (CONTRL.GE.1000) THEN
         DOXY = .FALSE.
         CONTRL = MOD(CONTRL,1000)
         END IF
      IF (CONTRL.GE.200) THEN
         ZIPV = 0
         CONTRL = MOD(CONTRL,200)
         END IF
      IF (CONTRL.GE.100) THEN
         ZIPV = 1
         CONTRL = MOD(CONTRL,100)
         END IF
      IF (CONTRL.GE.10) THEN
         X0Y0Z0 = .FALSE.
         CONTRL = MOD(CONTRL,10)
         END IF
      IF (CONTRL.GE.2) THEN
         ZIPA = 0
         CONTRL = MOD(CONTRL,2)
         END IF
      IF (CONTRL.GE.1) THEN
         ZIPA = 1
         END IF
C                                       Input amp's linear OR log
      LINEAR = XLINEA.LT.0.0
      IF (X0Y0Z0) THEN
         X0 = 0.0
         Y0 = 0.0
         Z0 = 0.0
         END IF
C                                       Uniform weighting?
      IF (LPRM(1).LT.-0.01 .OR. MPRM(1).LT.-0.01) THEN
         UNIWT = .FALSE.
         LPRM(1) = ABS(LPRM(1))
         MPRM(1) = ABS(MPRM(1))
      ELSE
         UNIWT = .TRUE.
         END IF
C                                       Output option flags.
      ANY = .FALSE.
      DO 90 J = 1,10
         OPT(J) = XOPT(J).GT.0.0
         IF (OPT(J)) ANY = .TRUE.
 90      CONTINUE
      IF (.NOT.ANY) THEN
         OPT(4) = .TRUE.
         OPT(5) = .TRUE.
         END IF
C                                       Cross-talk correction.
      AMP = REAL(XTALK)
      PHA = AIMAG(XTALK)*DG2RAD
      XTALK = AMP*CMPLX(COS(PHA), SIN(PHA))
C                                       Offset beam correction.
      XOFFS(4) = XOFFS(4)*DG2RAD
      GO TO 999
C                                       Error.
 990  IERR = 1
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HOLINI: ERROR',I3,' ON ',A)
 1010 FORMAT ('FITS:HOLO',I2.2,'-',I2.2,A2,I2.2)
 1011 FORMAT (I2.2,'-',I2.2,A2)
 1015 FORMAT ('HOLINI: Grid extent changed from',I5,' to',I5)
      END
      SUBROUTINE FRQGET (INFILE, LUN, FREQ, IERR)
C-----------------------------------------------------------------------
C   FRQGET reads INFILE for the frequency
C   Inputs:
C      INFILE   C*(*)   Text file name
C      LUN      I       LUN to use
C   Outputs:
C      FREQ     R       Frequency in GHz (changed if found)
C      IERR     I       0 -> ok and found, -1 => ok not found
C                       else error
C-----------------------------------------------------------------------
      CHARACTER INFILE*(*)
      INTEGER   LUN, IERR
      REAL      FREQ
C
      INTEGER   IND, J
      CHARACTER STR*132
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZTXOPN ('READ', LUN, IND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, ' OPEN INFILE'
         GO TO 990
         END IF
 20   CALL ZTXIO ('READ', LUN, IND, STR, IERR)
      IF (IERR.EQ.2) THEN
         GO TO 100
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ INFILE'
         GO TO 990
      ELSE IF (STR(:1).EQ.'#') THEN
         J = INDEX (STR, 'Freq =')
         IF (J.GT.0) THEN
            J = J + 6
            READ (STR(J:),1020) FREQ
            GO TO 110
            END IF
         END IF
      GO TO 20
C                                       end of file
 100  IERR = -1
 110  CALL ZTXCLS (LUN, IND, J)
      GO TO 999
C
 990  CALL MSGWRT (8)
      IF (IND.GT.0) CALL ZTXCLS (LUN, IND, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FRQGET ERROR',I4,' ON ',A)
 1020 FORMAT (F13.9)
      END
      SUBROUTINE KERNEL (PRM, DL, KRNL)
C-----------------------------------------------------------------------
C   KERNEL constructs the kernel used for interpolation.
C
C   Given and returned:
C      PRM      R(5)   Parameters defining the interpolation function
C                        1: Type of interpolation function,
C                           1: Pillbox,
C                           2: Exponential,
C                           3: Sinc,
C                           4: Sinc*Exponential,
C                           5: Spheroidal (default).
C                        2: Support size in l.
C                      3-5: Parameters defining the function.
C                      On return PRM contains the actual parameters.
C      DL       R      Separation between the elements of KRNL in units
C                      of the cell spacing.
C
C   Returned:
C      KRNL     R(*)   Array containing one half of the interpolation
C                      function.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Jan/21. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      REAL      PRM(5), DL, KRNL(0:*)
C
      INTEGER   IALPHA, IERR, IFLAG, INTYP, ISUPP, J, JMAX
      REAL      ETA, L, PSI
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Type of interpolation to apply.
       INTYP = NINT(PRM(1))
       IF (INTYP.LT.1 .OR. INTYP.GT.5) THEN
          PRM(1) = 5.0
          INTYP = 5
          END IF
C                                       Set default parameters.
C                                       Pillbox.
       IF (INTYP.EQ.1) THEN
          IF (PRM(2).LE.0.0) PRM(2) = 0.5
C                                       Exponential.
       ELSE IF (INTYP.EQ.2) THEN
          IF (PRM(2).LE.0.0) PRM(2) = 3.0
          IF (PRM(3).LE.0.0) PRM(3) = 1.0
          IF (PRM(4).LE.0.0) PRM(4) = 2.0
C                                       Sinc.
      ELSE IF (INTYP.EQ.3) THEN
         IF (PRM(2).LE.0.0) PRM(2) = 3.0
         IF (PRM(3).LE.0.0) PRM(3) = 1.14
C                                       Sinc*Exponential.
      ELSE IF (INTYP.EQ.4) THEN
         IF (PRM(2).LE.0.0) PRM(2) = 3.0
         IF (PRM(3).LE.0.0) PRM(3) = 1.55
         IF (PRM(4).LE.0.0) PRM(4) = 2.52
         IF (PRM(5).LE.0.0) PRM(5) = 2.0
C                                       Spheroidal.
      ELSE IF (INTYP.EQ.5) THEN
         IF (PRM(2).LE.0.0) PRM(2) = 3.0
         IF (PRM(3).LE.0.0) PRM(3) = 1.0
C                                       Check legality
         ISUPP  = NINT(2.0*PRM(2))
         IF (ISUPP.LT.4) ISUPP = 4
         IF (ISUPP.GT.8) ISUPP = 8
         PRM(2) = ISUPP/2.0
         IALPHA = NINT(2.0*PRM(3) + 1.0)
         IF (IALPHA.LT.1) IALPHA = 1
         IF (IALPHA.GT.5) IALPHA = 5
         PRM(3) = (IALPHA-1)/2.0
         IFLAG  = 0
         END IF
C                                       Radius of support.
      JMAX = NINT(PRM(2)/DL)
      IF (JMAX.GT.700) JMAX = 700
      PRM(2) = JMAX*DL
C                                       Pillbox.
      IF (INTYP.EQ.1) THEN
         DO 10 J = 0, JMAX
            L = J*DL
            KRNL(J) = 1.0
            IF (J.EQ.JMAX) KRNL(J) = 0.5
 10         CONTINUE
C                                       Exponential.
      ELSE IF (INTYP.EQ.2) THEN
         DO 20 J = 0, JMAX
            L = J*DL
            KRNL(J) = EXP(-((L/PRM(3))**PRM(4)))
 20         CONTINUE
C                                       Sinc.
      ELSE IF (INTYP.EQ.3) THEN
         KRNL(0) = 1.0
         DO 30 J = 1, JMAX
            L = J*DL
            KRNL(J) = SIN(PI*L/PRM(3))/(PI*L/PRM(3))
 30         CONTINUE
C                                       Sinc*Exponential.
      ELSE IF (INTYP.EQ.4) THEN
         KRNL(0) = 1.0
         DO 40 J = 1, JMAX
            L = J*DL
            KRNL(J) = SIN(PI*L/PRM(3))/(PI*L/PRM(3))*
     *         EXP(-((L/PRM(4))**PRM(5)))
 40         CONTINUE
C                                       Compute the visibility coordinat
C                                       Spheroidal.
      ELSE IF (INTYP.EQ.5) THEN
         DO 50 J = 0, JMAX
            L = J*DL
            ETA = REAL(J)/REAL(JMAX)
            CALL SPHFN (IALPHA, ISUPP, IFLAG, ETA, PSI, IERR)
            KRNL(J) = PSI
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRID (NPIX, EL0, CELLLM, LKRNL, SL, DL, MKRNL, SM, DM,
     *   TAPER, XTALK, XOFFS, NVIS, MVIS, XVIS, AAMP, APHA, ACPLX,
     *   SCPLX, WGT, LPEAK, MPEAK, UNIWT, UNIF, IERR)
C-----------------------------------------------------------------------
C   GRID grids the visibility data for the holography routines.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C      EL0      R        Satellite elevation, in degrees.
C      CELLLM   R        Cell spacing in the (l,m) plane.
C      LKRNL    R(*)     Array containing one half of the
C                        interpolation function in l.
C      SL       R        Support radius for LKRNL, in cell units.
C      DL       R        Separation between the elements of LKRNL in
C                        units of the cell spacing.
C      MKRNL    R(*)     Array containing one half of the
C                        interpolation function in m.
C      SM       R        Support radius for MKRNL, in cell units.
C      DM       R        Separation between the elements of MKRNL in
C                        units of the cell spacing.
C      TAPER    R(2)     Taper type and width to 0.5, in cells.
C      XTALK    CX       Cross-talk correction.
C      XOFFS    R(4)     Azimuth and elevation offset (deg), amplitude
C                        factor and phase correction (radian) for offset
C                        beam correction.
C
C   Returned:
C      NVIS     I        Number of visibilities read.
C      MVIS     I        Number of visibilities used.
C      XVIS     I        Number of offset visibilities subtracted.
C      AAMP     R(NPIX,NPIX)
C                        The amplitude and phase of the antenna pattern,
C                        in a form suitable for output.
C      APHA     R(NPIX,NPIX)
C                        The amplitude and phase of the antenna pattern,
C                        in a form suitable for output.
C      ACPLX    CX(NPIX,NPIX)
C                        Regridded antenna pattern.
C      SCPLX    CX(NPIX,NPIX)
C                        Regridded sampling function.
C      WGT      R(NPIX,NPIX)
C                        Weight function from the interpolation.
C      LPEAK    R        Beam peak position.
C      MPEAK    R
C      UNIF     R(NPIX,NPIX)
C                        Uniform weighting summing area
C      IERR     I        Error status, 0 means success.
C
C   Called: {HGETVS}
C
C   Algorithm:
C      Convolution with a kernel of limited support size.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/11      Code last modified; 1996/07/17
C-----------------------------------------------------------------------
      LOGICAL   UNIWT
      INTEGER   IERR, MVIS, NPIX, NVIS, XVIS
      REAL      AAMP(NPIX,NPIX), APHA(NPIX,NPIX), CELLLM, DL, DM, EL0,
     *          SL, SM, TAPER(2), LKRNL(0:*), UNIF(NPIX,NPIX), LPEAK,
     *          MKRNL(0:*), MPEAK, WGT(NPIX,NPIX), XOFFS(4)
      COMPLEX   ACPLX(NPIX,NPIX), SCPLX(NPIX,NPIX), XTALK
C
      LOGICAL   DOOFFS, FRESH
      INTEGER   IL, IL1, IL2, ILMAX, IM, IMMAX, IM1, IM2, IOFFL, IOFFM,
     *          IROUND, ML, MM, NHALF, NMISS, TAPTYP
      REAL      AMP, AMPMAX, AZOFF, COSAZO, COSEL, COSEL0, CS, EL,
     *          ELOFF, FACT, L, M, N, OFFL, OFFM, PHA, SHFTL, SHFTM,
     *          SINAZO, SINEL, SINEL0, SN, T1, T2, TAPARM, WT
      COMPLEX   VIS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialization.
      DOOFFS = XOFFS(3).NE.0.0
C                                       Zero the output arrays.
      DO 20 IM = 1, NPIX
         DO 10 IL = 1, NPIX
            ACPLX(IL,IM) = (0.0, 0.0)
            SCPLX(IL,IM) = (0.0, 0.0)
            UNIF(IL,IM) = 0.0
            WGT(IL,IM)  = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Taper type
      TAPTYP = IROUND (TAPER(1))
      IF (TAPTYP.LE.0 .OR. TAPTYP.GT.3) TAPTYP = 0
      IF (TAPER(2).LE.3.) TAPER(2) =  NPIX/3.0
      IF (TAPTYP.EQ.1) THEN
         TAPARM = -ALOG(2.0)/(CELLLM*TAPER(2))**2
      ELSE IF (TAPTYP.EQ.2) THEN
         TAPARM = -ALOG(2.0)/(CELLLM*TAPER(2))
      ELSE IF (TAPTYP.EQ.3) THEN
         TAPARM = 0.5/(CELLLM*TAPER(2))
         END IF
C                                       Uniform weighting
      IF (UNIWT) THEN
C                                       loop through all visibilities
         NVIS = 0
         MVIS = 0
         NMISS = 0
 30      CALL HGETVS (NVIS, AZOFF, ELOFF, L, M, N, AMP, PHA, IERR)
         IF (IERR.EQ.0) THEN
            IL = NINT(L/CELLLM) + 1
            IF (IL.LT.1) IL = IL + NPIX
            IM = NINT(M/CELLLM) + 1
            IF (IM.LT.1) IM = IM + NPIX
            IF (IL.LT.1 .OR. IL.GT.NPIX .OR.
     *          IM.LT.1 .OR. IM.GT.NPIX) THEN
               NMISS = NMISS + 1
            ELSE
               UNIF(IL,IM) = UNIF(IL,IM) + 1.0
               MVIS = MVIS + 1
               END IF
            GO TO 30
            END IF
         IF (NMISS.GT.0) THEN
            WRITE (MSGTXT,1030) NMISS
            CALL MSGWRT (6)
            END IF
         IF (MVIS.LE.0) THEN
            MSGTXT = 'NO VALID DATA FOUND: CHECK INPUTS'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         END IF
      NHALF = NPIX/2
C                                       Interpolate the visibilities
C                                       onto the (l,m) plane.
C                                       Loop through the data file.
      NVIS = 0
      MVIS = 0
      XVIS = 0
      NMISS = 0
 40   CALL HGETVS (NVIS, AZOFF, ELOFF, L, M, N, AMP, PHA, IERR)
         IF (IERR.NE.0) GO TO 70
         FRESH = .TRUE.
C                                       Get grid coordinates.
 45      IL = NINT(L/CELLLM) + 1
         IF (IL.LT.1) IL = IL + NPIX
         IM = NINT(M/CELLLM) + 1
         IF (IM.LT.1) IM = IM + NPIX
         IF (IL.LT.1 .OR. IL.GT.NPIX .OR. IM.LT.1 .OR. IM.GT.NPIX) THEN
            IF (FRESH) NMISS = NMISS + 1
            GO TO 40
            END IF
C                                       Uniform weighting?
         IF (UNIWT) THEN
            IF (UNIF(IL,IM).LE.0.0) GO TO 40
            FACT = 1.0/UNIF(IL,IM)
         ELSE
            FACT = 1.0
            END IF
C                                       taper
         IF (TAPTYP.EQ.1) THEN
            FACT = FACT*EXP(TAPARM*(L*L + M*M))
         ELSE IF (TAPTYP.EQ.2) THEN
            FACT = FACT*EXP(TAPARM*SQRT(L*L + M*M))
         ELSE IF (TAPTYP.EQ.3) THEN
            FACT = FACT*(1.0 - TAPARM*SQRT(L*L + M*M))
            END IF
         FACT = MAX(0.0, FACT)
C                                       Range of pixels within the
C                                       support size.  Ensure that IL1
C                                       and IM1 are rounded up, with IL2
C                                       and IM2 rounded down.
         IL1 = INT(L/CELLLM - SL - 9999.0) + 9999
         IL2 = INT(L/CELLLM + SL + 9999.0) - 9999
         IM1 = INT(M/CELLLM - SM - 9999.0) + 9999
         IM2 = INT(M/CELLLM + SM + 9999.0) - 9999
         IF (IL1.LT.-NHALF) IL1 = -NHALF
         IF (IL1.GE.NHALF)  GO TO 40
         IF (IL2.LT.-NHALF) GO TO 40
         IF (IL2.GE.NHALF)  IL2 = NHALF - 1
         IF (IM1.LT.-NHALF) IM1 = -NHALF
         IF (IM1.GE.NHALF)  GO TO 40
         IF (IM2.LT.-NHALF) GO TO 40
         IF (IM2.GE.NHALF)  IM2 = NHALF - 1
C                                       Count this visibility.
         IF (FRESH) THEN
            MVIS = MVIS + 1
         ELSE
            XVIS = XVIS + 1
            END IF
C                                       Compute the complex visibility.
         CS  = AMP*COS(PHA)
         SN  = AMP*SIN(PHA)
         VIS = CMPLX(CS, SN)
C                                       Add visibility into the grid.
         DO 60 IM = IM1, IM2
            MM = IM + 1
            IF (MM.LT.1) MM = MM + NPIX
            OFFM  = IM - M/CELLLM
            IOFFM = ABS(NINT(OFFM/DM))
            DO 50 IL = IL1, IL2
               ML = IL + 1
               IF (ML.LT.1) ML = ML + NPIX
               OFFL  = IL - L/CELLLM
               IOFFL = ABS(NINT(OFFL/DL))
               WT = LKRNL(IOFFL)*MKRNL(IOFFM)*FACT
               ACPLX(ML,MM) = ACPLX(ML,MM) + VIS*WT
               SCPLX(ML,MM) = SCPLX(ML,MM) + (1.0,0.0)*WT
               WGT(ML,MM)   =   WGT(ML,MM) + WT
 50            CONTINUE
 60         CONTINUE
C                                       Offset beam subtraction.
         IF (DOOFFS .AND. FRESH) THEN
            AZOFF = AZOFF + XOFFS(1)
            ELOFF = ELOFF + XOFFS(2)
C
            EL = EL0 + ELOFF
            COSEL0 = COS(EL0*DG2RAD)
            SINEL0 = SIN(EL0*DG2RAD)
            COSAZO = COS(AZOFF*DG2RAD)
            SINAZO = SIN(AZOFF*DG2RAD)
            COSEL  = COS(EL*DG2RAD)
            SINEL  = SIN(EL*DG2RAD)
C
            L =  SINAZO*COSEL0
            M = -COSAZO*COSEL0*SINEL + SINEL0*COSEL
            N =  COSAZO*COSEL0*COSEL + SINEL0*SINEL
C
            AMP = -AMP * XOFFS(3)
            PHA =  PHA + XOFFS(4)
            FRESH = .FALSE.
            GO TO 45
            END IF
      GO TO 40
C                                       done
 70   IF (NMISS.GT.0 .AND. .NOT.UNIWT) THEN
         WRITE (MSGTXT,1030) NMISS
         CALL MSGWRT (6)
         END IF
      IF (MVIS.LE.0) THEN
         MSGTXT = 'NO VALID DATA FOUND: CHECK INPUTS'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
C
      WRITE (MSGTXT,1070) MVIS, NVIS
      CALL MSGWRT (3)
      IF (DOOFFS) THEN
         WRITE (MSGTXT,1080) XVIS
         CALL MSGWRT (3)
         END IF
C
      IF (IERR.EQ.-1) IERR = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'GRID: ERROR READING THE VISIBILITY FILE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Apply cross-talk correction.
      IF (XTALK.NE.(0.0,0.0)) THEN
         DO 90 IM = 1, NPIX
            DO 80 IL = 1, NPIX
               ACPLX(IL,IM) = ACPLX(IL,IM) - XTALK
 80            CONTINUE
 90         CONTINUE
         END IF
C                                       Compute the amplitude and phase,
C                                       and reorganize them and the
C                                       weight for output.
      AMPMAX = 0.0
      MM = NPIX/2
      DO 110 IM = 1, NPIX/2
         MM = MM + 1
         ML = NPIX/2
         DO 100 IL = NPIX, 1, -1
            ML = ML + 1
            IF (ML.GT.NPIX) ML = ML - NPIX
C
            T1 = REAL(ACPLX(ML,MM))
            T2 = AIMAG(ACPLX(ML,MM))
            AAMP(IL,IM) = SQRT(T1*T1 + T2*T2)
            IF (AAMP(IL,IM).GT.AMPMAX) THEN
               ILMAX = IL
               IMMAX = IM
               AMPMAX = AAMP(IL,IM)
               END IF
            IF (T1.EQ.0.0 .AND. T2.EQ.0.0) THEN
               APHA(IL,IM) = 0.0
            ELSE
               APHA(IL,IM) = ATAN2(T2, T1)*RAD2DG
               END IF
C
            T1 = REAL(ACPLX(IL,IM))
            T2 = AIMAG(ACPLX(IL,IM))
            AAMP(ML,MM) = SQRT(T1*T1 + T2*T2)
            IF (AAMP(ML,MM).GT.AMPMAX) THEN
               ILMAX = ML
               IMMAX = MM
               AMPMAX = AAMP(ML,MM)
               END IF
            IF (T1.EQ.0.0 .AND. T2.EQ.0.0) THEN
               APHA(ML,MM) = 0.0
            ELSE
               APHA(ML,MM) = ATAN2(T2, T1)*RAD2DG
               END IF
C
            WT = WGT(IL,IM)
            WGT(IL,IM) = WGT(ML,MM)
            WGT(ML,MM) = WT
 100        CONTINUE
 110     CONTINUE
C
      SHFTM = 1.0
      DO 130 IM = 1, NPIX
         SHFTL = SHFTM
         DO 120 IL = 1, NPIX
            ACPLX(IL,IM) = SHFTL*ACPLX(IL,IM)
            SCPLX(IL,IM) = SHFTL*SCPLX(IL,IM)
            SHFTL = -SHFTL
 120        CONTINUE
         SHFTM = -SHFTM
 130     CONTINUE
C                                       Find peak position.
      LPEAK = (AAMP(ILMAX-1,IMMAX) - AAMP(ILMAX+1,IMMAX))/
     *        2.0*(AAMP(ILMAX+1,IMMAX) - 2.0*AAMP(ILMAX,IMMAX) +
     *        AAMP(ILMAX-1,IMMAX))
      MPEAK = (AAMP(ILMAX,IMMAX-1) - AAMP(ILMAX,IMMAX+1))/
     *        2.0*(AAMP(ILMAX,IMMAX+1) - 2.0*AAMP(ILMAX,IMMAX) +
     *        AAMP(ILMAX,IMMAX-1))
      LPEAK = -(ILMAX+LPEAK - NHALF)*CELLLM
      MPEAK =  (IMMAX+MPEAK - NHALF - 1)*CELLLM
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('GRID:',I7,' SAMPLES WERE OUTSIDE THE GRID AREA')
 1070 FORMAT ('Gridded',I7,' visibilities of',I7,' read.')
 1080 FORMAT ('Gridded',I7,' offset visibilities.')
      END
      SUBROUTINE ZIPAN (ZIPA, NPIX, RMAX, RMIN, WGT, AAMP, APHA, PATCH,
     *   WSTAT)
C-----------------------------------------------------------------------
C   ZIPAN unwraps the antenna pattern phase map.
C
C   Given:
C      ZIPA     I        Antenna pattern map (A_PHA) unwrapping control
C                        flag
C                           0: No unwrapping.
C                           1: Unwrap closed discontinuities (PHAZIP).
C                           2: Perform closure operation (PHCLOS).
C      NPIX     I        Number of pixels on a side of the map.
C      RMAX     R        The antenna radius, in cell units.
C      RMIN     R        Sub-reflector radius, in cell units.
C      WGT      R(NPIX,NPIX)
C                        Weight function from the interpolation.
C      AAMP     R(NPIX,NPIX)
C                        The amplitude of the antenna pattern.
C
C   Given and returned:
C      APHA     R(NPIX,NPIX)
C                        The antenna pattern phase.
C      PATCH    I(NPIX,NPIX)
C                        Scratch array.
C
C   Returned:
C      WSTAT    I(6)     Unwrapping statistics:
C                           1) Initial phase unwrapping index.
C                           2) Number of patches unwrapped before
C                              contour closing.
C                           3) Intermediate phase unwrapping index.
C                           4) Number of contour closures.
C                           5) Number of patches unwrapped after contour
C                              closing.
C                           6) Final phase unwrapping index.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1996/07/18  Code last modified; 1996/08/20
C-----------------------------------------------------------------------
      INTEGER   NPIX, PATCH(NPIX,NPIX), WSTAT(6), ZIPA
      REAL      AAMP(NPIX,NPIX), APHA(NPIX,NPIX), RMAX, RMIN,
     *          WGT(NPIX,NPIX)
C
      INTEGER   IERR, IL, IM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set zero weight pixels to blank
      DO 20 IM = 1, NPIX
         DO 10 IL = 1, NPIX
            IF (WGT(IL,IM).EQ.0.0) APHA(IL,IM) = FBLANK
 10         CONTINUE
 20      CONTINUE
C                                       Do phase unwrapping.
      CALL UNWRAP (ZIPA, NPIX, RMAX, RMIN, AAMP, 0.0, APHA, PATCH,
     *   WSTAT, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ZIPAN: Error unwrapping the A_PHA phase map.'
         CALL MSGWRT (6)
         END IF
C                                       Set blanked pixels to zero.
      DO 40 IM = 1, NPIX
         DO 30 IL = 1, NPIX
            IF (WGT(IL,IM).EQ.0.0) APHA(IL,IM) = 0.0
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XYZOFS (LAMBDA, CELLLM, NPIX, WGT, APHA, ACPLX, X0, Y0,
     *   Z0, DX0, DY0, DZ0, IERR)
C-----------------------------------------------------------------------
C   XYZOFS determines the offset of the antenna vertex in (x,y,z) from
C   the intersection of the azimuth and elevation axes and corrects the
C   antenna pattern phase for it.
C
C   Given:
C      LAMBDA   R        Observing wavelength, in metres.
C      CELLLM   R        Cell spacing in the (l,m) plane.
C      NPIX     I        Number of pixels on a side of the map.
C      WGT      R(NPIX,NPIX)
C                        Weight function from the interpolation.
C
C   Given and returned:
C      APHA     R(NPIX,NPIX)
C                        Phase of the antenna pattern.  Corrected for
C                        the phase ramp on return.
C      ACPLX    CX(NPIX,NPIX)
C                        Complex antenna pattern.  Corrected for the
C                        phase ramp on return.
C
C   Returned:
C      X0       R        Offset of the antenna vertex in (x,y,z) from
C                        the intersection of the azimuth and elevation
C                        axes, in metres.
C      Y0       R        y component of above
C      Z0       R        z component of above
C      DX0      R        Standard error in X0
C      DY0      R        Standard error in Y0
C      DZ0      R        Standard error in Z0
C      IERR     I        Error status, 0 means success,
C
C   Algorithm:
C      The effect on the antenna pattern phase is of the form phase
C      offset = l*x0 + m*y0 + n*z0.  Since n is close to unity, this
C      corresponds to the addition of a ramp in the phase map.
C
C   Notes:
C      1) XYZOFS assumes that the phase map has already been unwrapped.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Nov/07. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, CELLLM, WGT(NPIX,NPIX), APHA(NPIX,NPIX), X0, Y0,
     *          Z0, DX0, DY0, DZ0
      COMPLEX   ACPLX(NPIX,NPIX)
C
      INTEGER   IL, IM, JL, JM, ML, MM, NPRM
      REAL      FIT, D(3,3), NS, PHA, R(3), SSQ, SSQRES, SUM, L, M, N,
     *          VARRES, VARY, VX(3), X(3)
      COMPLEX   CPHA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Determine (x0,y0,z0).
C                                       Initialize.
      ML = NPIX/2
      MM = NPIX/2 + 1
      NS     = 0.0
      SUM    = 0.0
      SSQ    = 0.0
      R(1)   = 0.0
      R(2)   = 0.0
      R(3)   = 0.0
      D(1,1) = 0.0
      D(1,2) = 0.0
      D(1,3) = 0.0
      D(2,2) = 0.0
      D(2,3) = 0.0
      D(3,3) = 0.0
C                                       Loop through the antenna pattern
C                                       phase map.
      DO 20 IM = 1, NPIX
         DO 10 IL = 1, NPIX
C                                       Check validity.
            IF (WGT(IL,IM).GT.0.0) THEN
C                                       Compute coordinates.
               L = -(IL-ML)*CELLLM
               M =  (IM-MM)*CELLLM
               N = SQRT(1.0 - L*L - M*M)
               PHA = APHA(IL,IM)
C                                       Accumulate statistics.
               NS     = NS   + 1.0
               SUM    = SUM  + PHA
               SSQ    = SSQ  + PHA*PHA
               R(1)   = R(1) + PHA*L
               R(2)   = R(2) + PHA*M
               R(3)   = R(3) + PHA*N
               D(1,1) = D(1,1) + L*L
               D(1,2) = D(1,2) + L*M
               D(1,3) = D(1,3) + L*N
               D(2,2) = D(2,2) + M*M
               D(2,3) = D(2,3) + M*N
               D(3,3) = D(3,3) + N*N
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Compute the least squares
C                                       solution for (x0,y0,z0)
      NPRM = 3
      CALL LEASQR (NPRM, NS, SUM, SSQ, R, D, X, VX, SSQRES, VARRES,
     *   VARY, FIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      X0  = X(1)
      Y0  = X(2)
      Z0  = X(3)
C                                       Apply the correction.
      DO 50 IM = 1, NPIX
         JM = IM + MM - 1
         IF (JM.GT.NPIX) JM = JM - NPIX
         DO 40 IL = 1, NPIX
            IF (WGT(IL,IM).NE.0.0) THEN
C                                       Compute visibility coordinates
               L = -(IL-ML)*CELLLM
               M =  (IM-MM)*CELLLM
               N = SQRT(1.0 - L*L - M*M)
C                                       Fix the antenna pattern phase
               PHA = -(L*X0 + M*Y0 + N*Z0)
               APHA(IL,IM) = APHA(IL,IM) + PHA
C                                       Fix the complex antenna pattern
               JL = IL + ML - 1
               IF (JL.GT.NPIX) JL = JL - NPIX
               PHA  = PHA*DG2RAD
               CPHA = CMPLX(COS(PHA), SIN(PHA))
               ACPLX(JL,JM) = CPHA*ACPLX(JL,JM)
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Rescale to metres.
      X0  = X(1)*LAMBDA/360.0
      Y0  = X(2)*LAMBDA/360.0
      Z0  = X(3)*LAMBDA/360.0
      DX0 = SQRT(VX(1))*LAMBDA/360.0
      DY0 = SQRT(VX(2))*LAMBDA/360.0
      DZ0 = SQRT(VX(3))*LAMBDA/360.0
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('XYZOFS: Error',I4,' solving the normal equations.')
      END
      SUBROUTINE HGETVS (NVIS, AZOFF, ELOFF, L, M, N, AMP, PHA, IERR)
C-----------------------------------------------------------------------
C   HGETVS reads the next visibility.
C
C   Given (via common INPARM, INTLOG, INCHAR):
C      INFILE      C*48  Input visibility file name.
C      LUNVIS      I     Logical unit number to use for the input
C                        visibility file.
C      LAMBDA      R     Observing wavelength, in metres.
C      EL0         R     Satellite elevation, in degrees.
C      LMMIN       R     Range of |l| and |m| to include.  Negative
C      LMMAX       R     values denote a range of SQRT(l*l + m*m).
C      LINEAR      L     If true, the input amplitudes are linear,
C                        else logarithmic.
C      AMPSCL      R     Amplitude scaling factor.
C      PHSGN       R     Factor by which to multiply the measured
C                        phase.
C      X0,Y0,Z0    R     Offset of the antenna vertex in (x,y,z)
C                        from the intersection of the azimuth and
C                        elevation axes, in metres.
C
C   Given and returned:
C      NVIS        I     Progressive number of visibilities read.
C                        If given as zero, the file will be opened.
C
C   Returned:
C      AZOFF       R     Azimuth offset, deg.
C      ELOFF       R     Elevation offset, deg.
C      L,M,N       R     The (l,m,n) coordinates of the visibility.
C      AMP         R     Visibility amplitude, volts.
C      PHA         R     Visibility phase, radians.
C      IERR        I     Error status, 0: success,
C                           -1: end of file.
C
C   Notes:
C      1) The azimuth and elevation offsets in the data file refer to
C         the position of the telescope beam on the sky.
C
C         A POSITIVE azimuth offset samples a point in the antenna
C         pattern at a NEGATIVE azimuth offset and this corresponds
C         to a POSITIVE value of l (see note in HOLGR prologue).
C
C         Conversely, a POSITIVE elevation offset samples the antenna
C         pattern at a NEGATIVE elevation offset which corresponds to
C         a NEGATIVE value of m.
C
C-----------------------------------------------------------------------
      INTEGER   NVIS, IERR
      REAL      L, M, N, AMP, PHA
C
      INTEGER   JTRIM, KBP, KBLIM
      REAL      AZOFF, COSAZO, COSEL, COSEL0, EL, ELOFF, RLM, SINAZO,
     *          SINEL, SINEL0
      DOUBLE PRECISION DBLX
      CHARACTER STR*80
      INCLUDE 'HOLGR'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Open the holography data file
      IF (NVIS.EQ.0) THEN
         CALL ZTXOPN ('QRED', LUNVIS, INDVIS, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Loop until get valid visibility
 40   CALL ZTXIO ('READ', LUNVIS, INDVIS, STR, IERR)
         IF (IERR.EQ.2) GO TO 70
         IF (IERR.NE.0) GO TO 50
         IF (STR(:1).EQ.'!') GO TO 40
         IF (STR(:1).EQ.'#') GO TO 40
C                                       Parse the sample data: az, el,
C                                       amp, phase
         KBP = 1
         KBLIM = JTRIM (STR)
         CALL GETNUM (STR, KBLIM, KBP, DBLX)
         IF (DBLX.EQ.DBLANK) GO TO 50
         AZOFF = DBLX
         CALL GETNUM (STR, KBLIM, KBP, DBLX)
         IF (DBLX.EQ.DBLANK) GO TO 50
         ELOFF = DBLX
         CALL GETNUM (STR, KBLIM, KBP, DBLX)
         IF (DBLX.EQ.DBLANK) GO TO 50
         AMP = DBLX
         CALL GETNUM (STR, KBLIM, KBP, DBLX)
         IF (DBLX.EQ.DBLANK) GO TO 50
         PHA = DBLX
         NVIS = NVIS + 1
C                                       Compute visibility coordinates
         IF (EL0.GT.0) THEN
C                                       relative -> absolute elevation
            EL = EL0 + ELOFF
            COSEL0 = COS(EL0*DG2RAD)
            SINEL0 = SIN(EL0*DG2RAD)
            COSAZO = COS(AZOFF*DG2RAD)
            SINAZO = SIN(AZOFF*DG2RAD)
            COSEL  = COS(EL*DG2RAD)
            SINEL  = SIN(EL*DG2RAD)
C
            L =  SINAZO*COSEL0
            M = -COSAZO*COSEL0*SINEL + SINEL0*COSEL
            N =  COSAZO*COSEL0*COSEL + SINEL0*SINEL
C                                       Input is L, M already
         ELSE
            L = AZOFF
            M = ELOFF
            N = 0.0
            END IF
         RLM = SQRT(L*L + M*M)
C                                       Data point included?
         IF (LMMIN.GT.0.0) THEN
            IF (ABS(L).LT.LMMIN) GO TO 40
            IF (ABS(M).LT.LMMIN) GO TO 40
         ELSE IF (LMMIN.LT.0.0) THEN
            IF (L*L+M*M.LT.LMMIN*LMMIN) GO TO 40
            END IF
         IF (LMMAX.GT.0.0) THEN
            IF (ABS(L).GT.LMMAX) GO TO 40
            IF (ABS(M).GT.LMMAX) GO TO 40
         ELSE IF (LMMAX.LT.0.0) THEN
            IF (L*L+M*M.GT.LMMAX*LMMAX) GO TO 40
            END IF
C                                       Amplitude scaled to volts
      IF (LINEAR) THEN
         AMP = AMPSCL*AMP
C                                       Amplitude from power dB to volts
      ELSE
         AMP = 10.0**(AMP/20.0)
         END IF
      PHA = PHSGN*PHA - 360.0*(L*X0 + M*Y0 + N*Z0)/LAMBDA
      PHA = PHA*DG2RAD
      GO TO 999
C                                       Error Exit
 50   IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1050) NVIS
         CALL MSGWRT (8)
      ELSE IF (DBLX.EQ.DBLANK) THEN
         WRITE (MSGTXT,1055) NVIS
         CALL MSGWRT (8)
         IERR = 10
         END IF
      GO TO 999
C                                       End of file
 70   CALL ZTXCLS (LUNVIS, INDVIS, IERR)
      IERR = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('HGETVS: ERROR READING VISIBILITY FILE AT VIS',I8)
 1055 FORMAT ('ERROR PARSING DATA CARD AT VIS',I8)
      END
      SUBROUTINE DFT (LAMBDA, NPIX, DX, DY, XTALK, V1, V2, P1, P2, VMAX,
     *   IERR)
C-----------------------------------------------------------------------
C   DFT does a direct Fourier transform of the holography visibility
C   data, producing amplitude and phase maps of the resolution function
C   as well as the aperture voltage distribution.
C
C   Given:
C      LAMBDA   R        Observing wavelength, in metres.
C      NPIX     I        Number of pixels on a side of the map.
C      DX       R        Map cell spacing in X and Y, in metres.
C      DY       R        Map cell spacing in X and Y, in metres.
C      XTALK    CX       Cross-talk correction.
C
C   Returned:
C      V1       R(NPIX,NPIX)
C                        Amplitude of the aperture voltage distribution,
C                        reorganized for output.
C      V2       R(NPIX,NPIX)
C                        Phase of the aperture voltage distribution,
C                        reorganized for output.
C      P1       R(NPIX,NPIX)
C                        Amplitude of the point spread function,
C                        reorganized for output.
C      P2       R(NPIX,NPIX)
C                        Phase of the point spread function, reorganized
C                        for output.
C      IERR     I        Error status, 0 means success.
C
C   Algorithm:
C      Direct Fourier transform of the visibility data.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      DX, DY, LAMBDA, VMAX, P1(NPIX,NPIX), P2(NPIX,NPIX),
     *          V1(NPIX,NPIX), V2(NPIX,NPIX)
      COMPLEX   XTALK
C
      INCLUDE 'HOLGRP'
      INTEGER   IX, IY, NX, NY, NVIS
      REAL      AMP, AZOFF, ELOFF, L, M, N, PHA, T1, T2
      DOUBLE PRECISION ML, MM, RHO, RHO0, PHI, CL, CM, DV1(MX,MX),
     *   DV2(MX,MX), DP1(MX,MX), DP2(MX,MX), DAMP, DPHA, DL, DM,
     *   DXRE, DXIM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                        Zero the output arrays.
      DO 20 IY = 1, NPIX
         DO 10 IX = 1, NPIX
            DV1(IX,IY) = 0.0D0
            DV2(IX,IY) = 0.0D0
            DP1(IX,IY) = 0.0D0
            DP2(IX,IY) = 0.0D0
 10         CONTINUE
 20      CONTINUE
C                                       Coordinates of the map centre.
      NX = NPIX/2 + 1
      NY = NPIX/2 + 1
      ML = -DX * TWOPI / LAMBDA
      MM = -DY * TWOPI / LAMBDA
      DXRE = REAL (XTALK)
      DXIM = AIMAG (XTALK)
C                                       Loop thru visibility data file
      NVIS = 0
 30   CALL HGETVS (NVIS, AZOFF, ELOFF, L, M, N, AMP, PHA, IERR)
      IF (IERR.EQ.0) THEN
         DL = L
         DM = M
         DAMP = AMP
         DPHA = PHA
C                                       Radians per cell spacing in L
C                                       and M. (Negative for a minus-i
C                                       transform.)
         CL = DL * ML
         CM = DM * MM
C                                       Phase term for (IX,IY) = (0,0).
         RHO0 = - (CL * NX + CM * NY)
C                                       Loop over the map points.
         DO 50 IY = 1,NPIX
            RHO = RHO0 + CM*IY
            DO 40 IX = 1,NPIX
C                                       Point-spread function.
               RHO = RHO + CL
               DP1(IX,IY) = DP1(IX,IY) + COS(RHO)
               DP2(IX,IY) = DP2(IX,IY) + SIN(RHO)
C                                       Antenna pattern.
               PHI = DPHA + RHO
               DV1(IX,IY) = DV1(IX,IY) + DAMP*COS(PHI) - DXRE
               DV2(IX,IY) = DV2(IX,IY) + DAMP*SIN(PHI) - DXIM
 40            CONTINUE
 50         CONTINUE
         IF (MOD(NVIS,50).EQ.0) THEN
            WRITE (MSGTXT,1050) NVIS
            CALL MSGWRT (2)
            END IF
         GO TO 30
         END IF
C                                       done
      WRITE (MSGTXT,1050) NVIS
      CALL MSGWRT (2)
      IF (IERR.EQ.-1) IERR = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'DFT: Error reading the visibility file; continuing.'
         CALL MSGWRT (6)
         END IF
C                                        Convert the real and imaginary
C                                        parts to amplitude and phase
      VMAX = 0.0
      DO 110 IY = 1,NPIX
         DO 100 IX = 1, NPIX
C                                        Antenna pattern.
            T1 = DV1(IX,IY)
            T2 = DV2(IX,IY)
            V1(IX,IY) = SQRT (T1*T1 + T2*T2)
            IF ((T1.EQ.0.0) .AND. (T2.EQ.0.0)) THEN
               V2(IX,IY) = 0.0
            ELSE
               VMAX = MAX (VMAX, V1(IX,IY))
               V2(IX,IY) = ATAN2 (T2, T1) * RAD2DG
               END IF
C                                        Point-spread function.
            T1 = DP1(IX,IY)
            T2 = DP2(IX,IY)
            P1(IX,IY) = SQRT (T1*T1 + T2*T2)
            IF ((T1.EQ.0.0) .AND. (T2.EQ.0.0)) THEN
               P2(IX,IY) = 0.0
            ELSE
               P2(IX,IY) = ATAN2(T2, T1)*RAD2DG
               END IF
 100        CONTINUE
 110     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('DFT: Processed',I6,' visibilities.')
      END
      SUBROUTINE HOLFFT (DIR, NPIX, VIS, AMP, PHA)
C-----------------------------------------------------------------------
C   HOLFFT does a fast Fourier transform of the visibility data for
C   HOLGR.
C
C   Given:
C      DIR      I        Sign of the complex phase in Fourier integral
C      NPIX     I        Dimensions of the square array.
C
C   Given and returned:
C      VIS      CX(NPIX,NPIX)
C                        Array containing the complex visibilities.
C                        Returned as the transpose of the Fourier
C                        transform of the input array.
C      AMP      R(NPIX,NPIX)
C      PHA      R(NPIX,NPIX)
C                        Arrays containing the amplitude and phase parts
C                        of the Fourier transform of the input array.
C
C   Called: APLNOT: {FFTMC}
C
C   Algorithm:
C      Does the FFT on the input complex array then converts it to
C      amplitude and phase.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Dec.    Code last modified; 1989/Nov/02.
C-----------------------------------------------------------------------
      INTEGER    DIR, NPIX
      COMPLEX    VIS(NPIX,NPIX)
      REAL       AMP(NPIX,NPIX), PHA(NPIX,NPIX)
C
      INTEGER    IL, IM
      REAL       CS, SN
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Do the FFT.
      CALL FFTMC (DIR, NPIX, NPIX, 1, VIS)
C                                       Extract the amplitude and phase.
      DO 20 IM = 1,NPIX
         DO 10 IL = 1,NPIX
            CS =  REAL (VIS(IL,IM))
            SN = AIMAG (VIS(IL,IM))
            AMP(IL,IM) = SQRT (CS*CS + SN*SN)
            IF ((SN.EQ.0.0) .AND. (CS.EQ.0.0)) THEN
               PHA(IL,IM) = 0.0
            ELSE
               PHA(IL,IM) = ATAN2 (SN, CS) * RAD2DG
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FTKRNL (KRNL, DL, KMAX, NPIX, FTK)
C-----------------------------------------------------------------------
C   FTKRNL computes the Fourier transform of the convolution function
C   used for interpolating data onto the map grid.  This may then be
C   used to apply the gridding correction in the map plane.
C
C   Given:
C      KRNL   R(*)   Array containing one half of the interpolation
C                    function.
C      DL     R      Separation between the elements of KRNL in units of
C                    the cell spacing.
C      KMAX   I      Location of the last non-zero element of KRNL.
C      NPIX   I      Number of pixels on a side of the output map.
C
C   Returned:
C      FTK    R(*)   Fourier transform of KRNL evaluated at the map grid
C                    points.
C
C   Algorithm: Does an exact calculation as described by
C      Greisen, E.W., 1979. VLA Scientific Memorandum No. 131.
C
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Jan/22. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   KMAX, NPIX
      REAL      DL, FTK(0:*), KRNL(0:*)
C
      INTEGER   IX, IL
      REAL      X, T
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Loop over the map pixels.
      DO 20 IX = 0,NPIX/2
         X = REAL(IX)/REAL(NPIX)
         T = 2.0*PI*DL*X
C                                       Do the cosine transform.
         FTK(IX) = KRNL(0)
         DO 10 IL = 1,KMAX
            FTK(IX) = FTK(IX) + 2.0*KRNL(IL)*COS(IL*T)
 10         CONTINUE
C                                       Correct for the table lookup.
         IF (IX.NE.0) FTK(IX) = FTK(IX) * SIN (PI*DL*X) / (PI*DL*X)
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GRIDCR (XFTK, YFTK, NPIX, AMP, AMPMAX)
C-----------------------------------------------------------------------
C   GRIDCR applies the gridding correction to the amplitude map and
C   normalizes it to have a peak value of 1.0 volt.
C
C   Given:
C      XFTK     R(*)     The gridding correction in the X direction.
C      YFTK     R(*)     The gridding correction in the Y direction.
C      NPIX     I        Number of pixels on a side of the map.
C
C   Given and returned:
C      AMP      R(NPIX,NPIX)
C                        The amplitude map.
C      AMPMAX   R        The normalisation constant
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Jan/22. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      AMP(NPIX,NPIX), AMPMAX, XFTK(0:*), YFTK(0:*)
C
      INTEGER   IX, IY, JX, JY, NHALF
C-----------------------------------------------------------------------
C                                       Initialize.
      NHALF = NPIX/2 + 1
      AMPMAX = -1.0
C                                       Apply the correction.
      DO 20 IY = 1, NPIX
         JY = ABS(IY-NHALF)
C
         DO 10 IX = 1,NPIX
            JX = ABS(IX-NHALF)
            AMP(IX,IY) = AMP(IX,IY)/(XFTK(JX)*YFTK(JY))
            AMPMAX = MAX(AMP(IX,IY), AMPMAX)
 10         CONTINUE
 20      CONTINUE
C                                       Normalize.
      DO 40 IY = 1,NPIX
         DO 30 IX = 1,NPIX
            AMP(IX,IY) = AMP(IX,IY)/AMPMAX
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BLANK (RMAX, RMIN, NPIX, MAP)
C-----------------------------------------------------------------------
C   BLANK blanks a map beyond the antenna diameter and beneath the
C   subreflector.
C
C   Given:
C      RMAX     R        The antenna radius, in cell units.
C      RMIN     R        Sub-reflector radius, in cell units.
C      NPIX     I        Number of pixels on a side of the map.
C
C   Given and returned:
C      MAP      R(*,*)   The map to be blanked.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/04/11  Code last modified; 1996/08/20
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      RMAX, RMIN, MAP(NPIX,NPIX)
C
      INTEGER   IX, IX0, IY, IY0
      REAL      R
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Apply pixel blanking.
      IX0  = NPIX/2 + 1
      IY0  = NPIX/2 + 1
      DO 20 IY = 1,NPIX
         DO 10 IX = 1,NPIX
            R = SQRT(REAL((IX-IX0)**2 + (IY-IY0)**2))
            IF (R.GT.RMAX .OR. R.LT.RMIN) MAP(IX,IY) = FBLANK
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE UNWRAP (ZIP, NPIX, RMAX, RMIN, AMP, DISCR, PHA, PATCH,
     *   WSTAT, IERR)
C-----------------------------------------------------------------------
C   UNWRAP is a driver for the phase unwrapping routines.
C-----------------------------------------------------------------------
C
C   Given:
C      ZIP      I        Unwrapping control flag
C                           1: Unwrap closed discontinuities (PHAZIP).
C                           2: Perform closure operation (PHCLOS).
C      NPIX     I        Number of pixels on a side of the map.
C      RMAX     R        The antenna radius, in cell units.
C      RMIN     R        Sub-reflector radius, in cell units.
C      AMP      R(NPIX,NPIX)
C                        Amplitude part of the phase map to be
C                        rectified.
C      DISCR    R        Amplitude threshhold; pixels with amplitude
C                        below below this level will be blanked.
C
C   Given and returned:
C      PHA      R(NPIX,NPIX)
C                        The phase map to be rectified.
C      PATCH    I(NPIX,NPIX)
C                        Scratch array.
C
C   Returned:
C      WSTAT    I(6)     Unwrapping statistics:
C                           1) Initial phase unwrapping index.
C                           2) Number of patches unwrapped before
C                              contour closing.
C                           3) Intermediate phase unwrapping index.
C                           4) Number of contour closures.
C                           5) Number of patches unwrapped after contour
C                              closing.
C                           6) Final phase unwrapping index.
C      IERR     I        Error status, 0 means success,
C
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1996/07/19  Code last modified; 1996/07/30
C-----------------------------------------------------------------------
      INTEGER   IERR, NPIX, PATCH(NPIX,NPIX), WSTAT(6), ZIP, TMP
      REAL      AMP(NPIX,NPIX), DISCR, PHA(NPIX,NPIX), RMAX, RMIN
C-----------------------------------------------------------------------
C                                       Initial phase wrapping index.
      CALL PHAWRP (NPIX, PHA, WSTAT(1))
C                                       Do initial unwrapping.
      IF (ZIP.GT.0) THEN
         CALL PHAZIP (NPIX, AMP, DISCR, PHA, PATCH, WSTAT(2), IERR)
C                                       Intermediate wrapping index.
         CALL PHAWRP (NPIX, PHA, WSTAT(3))
         END IF
C
      IF (ZIP.GT.1) THEN
C                                       Do contour closing.
         CALL PHCLOS (NPIX, RMAX, RMIN, PHA, PATCH, WSTAT(4), IERR)
C                                       Do final unwrapping.
         CALL PHAZIP (NPIX, AMP, DISCR, PHA, PATCH, WSTAT(5), IERR)
C                                       Do contour closing.
         CALL PHCLOS (NPIX, RMAX, RMIN, PHA, PATCH, WSTAT(4), IERR)
C                                       Do final unwrapping.
         CALL PHAZIP (NPIX, AMP, DISCR, PHA, PATCH, TMP, IERR)
         WSTAT(5) = WSTAT(5) + TMP
C                                       Final phase wrapping index.
         CALL PHAWRP (NPIX, PHA, WSTAT(6))
         END IF
C
 999  RETURN
      END
      SUBROUTINE PHAWRP (NPIX, PHA, WNDX)
C-----------------------------------------------------------------------
C   PHAWRP calculates the wrapping index of a phase map.
C
C   Given:
C      NPIX   I        Number of pixels on a side of the map.
C      PHA    R(NPIX,NPIX)
C                      The phase map.
C
C   Returned:
C      WNDX   I        The phase wrapping index.
C
C   Algorithm:
C      Simple count of the number of discontinuities between
C      neighbouring rows and columns.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/04/11  Code last modified; 1996/07/08
C-----------------------------------------------------------------------
      INTEGER   NPIX, WNDX
      REAL      PHA(NPIX,NPIX)
C
      INTEGER   IX, IY
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Scan through the map.
      WNDX = 0
      DO 20 IY = 1, NPIX
         DO 10 IX = 1, NPIX
            IF (PHA(IX,IY).EQ.FBLANK) GO TO 10
C                                       Look left.
            IF (IX.GT.1) THEN
               IF (PHA(IX-1,IY).EQ.FBLANK) GO TO 10
               IF (ABS(PHA(IX,IY)-PHA(IX-1,IY)).GT.180.0)
     *            WNDX = WNDX + 1
               END IF
C                                       Look below.
            IF (IY.GT.1) THEN
               IF (PHA(IX,IY-1).EQ.FBLANK) GO TO 10
               IF (ABS(PHA(IX,IY)-PHA(IX,IY-1)).GT.180.0)
     *            WNDX = WNDX + 1
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PHAZIP (NPIX, AMP, DISCR, PHA, PATCH, NPAT, IERR)
C-----------------------------------------------------------------------
C   PHAZIP unwraps a phase map.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C      AMP      R(NPIX,NPIX)
C                        Amplitude part of the phase map to be
C                        rectified.
C      DISCR    R        Amplitude threshhold; pixels with amplitude
C                        below below this level will be blanked.
C
C   Given and returned:
C      PHA      R(NPIX,NPIX)
C                        The phase map to be rectified.
C      PATCH    I(NPIX,NPIX)
C                        Scratch array.
C      NPAT     I        Number of patches found.
C
C   Retured:
C      IERR     I        Error status, 0 means success,
C
C   Called: {}
C
C   Algorithm:
C      An equivalence operator is defined such that two pixels are
C      equivalent iff there is a path between them which does not cross
C      a discontinuity.  The path is restricted to horizontal and
C      vertical steps between neighbouring pixels.  A discontinuity is
C      defined as a phase step of greater than 180 degrees.
C
C      This equivalence operator partitions the phase map into a number
C      of equivalence classes referred to as "patches".  Adjacent
C      patches can be made equal to each other via the addition or
C      subtraction of an integral multiple of 360 degrees.
C
C      This algorithm is reliable.  However, it does not resolve open-
C      ended discontinuities.  It may or may not succeed in resolving
C      disguised open-ended discontinuities such as the following
C      (involving a pair of cloaked singletons, c.f. PHCLOS) where
C      only one of the two patches may be resolved:
C
C               +-------+-------+-------+-------+
C               |       |       |       |       |
C               |   0   |   0   |   0   |   0   |
C               |       |       |       |       |
C               +-------+=======*=======+-------+
C               |       %       %       %       |
C               |   0   %  200  % -200  %   0   |
C               |       %       %       %       |
C               +-------+=======*=======+-------+
C               |       |       |       |       |
C               |   0   |   0   |   0   |   0   |
C               |       |       |       |       |
C               +-------+-------+-------+-------+
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/04/05  Code last modified; 1996/07/19
C-----------------------------------------------------------------------
      INTEGER   IERR, NPIX, PATCH(NPIX,NPIX)
      REAL      AMP(NPIX,NPIX), DISCR, PHA(NPIX,NPIX)
C
C     Maximum number of patches.
      INTEGER   MAXPAT
      PARAMETER (MAXPAT = 1000)
C
      INTEGER   IBLNK, IPAT, ITER, IX, IXMAX(MAXPAT), IXMIN(MAXPAT),
     :          IY, IYMAX(MAXPAT), IYMIN(MAXPAT), JPAT, MPAT, NPAT
      REAL      STEP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Initialize equivalence map.
      IBLNK = 0
      DO 20 IY = 1, NPIX
         DO 10 IX = 1, NPIX
            IF (PHA(IX,IY).EQ.FBLANK) THEN
               PATCH(IX,IY) = -1
               IBLNK = IBLNK + 1
            ELSE IF (AMP(IX,IY).LE.DISCR) THEN
               PATCH(IX,IY) = -1
               IBLNK = IBLNK + 1
            ELSE
               PATCH(IX,IY) = 0
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Find all patches.
      IPAT = 0
      DO 250 NPAT = 1, MAXPAT
C        Find the first pixel in this patch.
         DO 40 IY = 1, NPIX
            DO 30 IX = 1, NPIX
               IF (PATCH(IX,IY).EQ.0) THEN
                  PATCH(IX,IY) = NPAT
                  IXMIN(NPAT) = IX
                  IXMAX(NPAT) = IX
                  IYMIN(NPAT) = IY
                  IYMAX(NPAT) = IY
                  IPAT = IPAT + 1
                  GO TO 60
                  END IF
 30            CONTINUE
 40         CONTINUE
C
         WRITE (MSGTXT, 50) NPAT
 50      FORMAT ('PHAZIP: Couldn''t find first pixel in patch',I6,'.')
         CALL MSGWRT (6)
         IERR = 1
         GO TO 999
C                                       Find pixels in the same patch.
 60      DO 230 ITER = 1, 10
            JPAT = 0
C                                       Scan left-to-right from bottom
C                                       to top.
            DO 90 IY = IYMIN(NPAT), NPIX
               DO 70 IX = IXMIN(NPAT), NPIX
                  IF (PATCH(IX,IY).EQ.0) THEN
                     IF (IX.GT.1) THEN
C                                       Look left.
                        IF (ABS(PHA(IX,IY)-PHA(IX-1,IY)).LT.180.0) THEN
                           IF (PATCH(IX-1,IY).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMAX(NPAT) = MAX(IX,IXMAX(NPAT))
                              IYMAX(NPAT) = MAX(IY,IYMAX(NPAT))
                              JPAT = JPAT + 1
                              GO TO 70
                              END IF
                           END IF
                        END IF
C
                     IF (IX.GT.IXMAX(NPAT)) GO TO 80
C
                     IF (IY.GT.1) THEN
C                                       Look below.
                        IF (ABS(PHA(IX,IY)-PHA(IX,IY-1)).LT.180.0) THEN
                           IF (PATCH(IX,IY-1).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMAX(NPAT) = MAX(IX,IXMAX(NPAT))
                              IYMAX(NPAT) = MAX(IY,IYMAX(NPAT))
                              JPAT = JPAT + 1
                              END IF
                           END IF
                        END IF
                     END IF
 70               CONTINUE
 80            IF (IY.GT.IYMAX(NPAT)) GO TO 100
 90            CONTINUE
C                                       Scan bottom-to-top from right
C                                       to left.
 100        DO 130 IX = IXMAX(NPAT), 1, -1
               DO 110 IY = IYMIN(NPAT), NPIX
                  IF (PATCH(IX,IY).EQ.0) THEN
                     IF (IY.GT.1) THEN
C                                       Look below.
                        IF (ABS(PHA(IX,IY)-PHA(IX,IY-1)).LT.180.0) THEN
                           IF (PATCH(IX,IY-1).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMIN(NPAT) = MIN(IX,IXMIN(NPAT))
                              IYMAX(NPAT) = MAX(IY,IYMAX(NPAT))
                              JPAT = JPAT + 1
                              GO TO 110
                              END IF
                           END IF
                        END IF
C
                     IF (IY.GT.IYMAX(NPAT)) GO TO 120
C
                     IF (IX.LT.NPIX) THEN
C                                       Look right.
                        IF (ABS(PHA(IX,IY)-PHA(IX+1,IY)).LT.180.0) THEN
                           IF (PATCH(IX+1,IY).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMIN(NPAT) = MIN(IX,IXMIN(NPAT))
                              IYMAX(NPAT) = MAX(IY,IYMAX(NPAT))
                              JPAT = JPAT + 1
                              END IF
                           END IF
                        END IF
                     END IF
 110              CONTINUE
 120           IF (IX.LT.IXMIN(NPAT)) GO TO 140
 130           CONTINUE
C                                       Scan right-to-left from top to
C                                       bottom.
 140        DO 170 IY = IYMAX(NPAT), 1, -1
               DO 150 IX = IXMAX(NPAT), 1, -1
                  IF (PATCH(IX,IY).EQ.0) THEN
                     IF (IX.LT.NPIX) THEN
C                                       Look right.
                        IF (ABS(PHA(IX,IY)-PHA(IX+1,IY)).LT.180.0) THEN
                           IF (PATCH(IX+1,IY).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMIN(NPAT) = MIN(IX,IXMIN(NPAT))
                              IYMIN(NPAT) = MIN(IY,IYMIN(NPAT))
                              JPAT = JPAT + 1
                              GO TO 150
                              END IF
                           END IF
                        END IF
C
                     IF (IX.LT.IXMIN(NPAT)) GO TO 160
C
                     IF (IY.LT.NPIX) THEN
C                                       Look above.
                        IF (ABS(PHA(IX,IY)-PHA(IX,IY+1)).LT.180.0) THEN
                           IF (PATCH(IX,IY+1).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMIN(NPAT) = MIN(IX,IXMIN(NPAT))
                              IYMIN(NPAT) = MIN(IY,IYMIN(NPAT))
                              JPAT = JPAT + 1
                              END IF
                           END IF
                        END IF
                     END IF
 150              CONTINUE
 160           IF (IY.LT.IYMIN(NPAT)) GO TO 180
 170           CONTINUE
C                                       Scan top-to-bottom from left to
C                                       right.
 180        DO 210 IX = IXMIN(NPAT), NPIX
               DO 190 IY = IYMAX(NPAT), 1, -1
                  IF (PATCH(IX,IY).EQ.0) THEN
                     IF (IY.LT.NPIX) THEN
C                                       Look above.
                        IF (ABS(PHA(IX,IY)-PHA(IX,IY+1)).LT.180.0) THEN
                           IF (PATCH(IX,IY+1).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMAX(NPAT) = MAX(IX,IXMAX(NPAT))
                              IYMIN(NPAT) = MIN(IY,IYMIN(NPAT))
                              JPAT = JPAT + 1
                              GO TO 190
                              END IF
                           END IF
                        END IF
C
                     IF (IY.LT.IYMIN(NPAT)) GO TO 200
C
                     IF (IX.GT.1) THEN
C                                       Look left.
                        IF (ABS(PHA(IX,IY)-PHA(IX-1,IY)).LT.180.0) THEN
                           IF (PATCH(IX-1,IY).EQ.NPAT) THEN
                              PATCH(IX,IY) = NPAT
                              IXMAX(NPAT) = MAX(IX,IXMAX(NPAT))
                              IYMIN(NPAT) = MIN(IY,IYMIN(NPAT))
                              JPAT = JPAT + 1
                              END IF
                           END IF
                        END IF
                     END IF
 190              CONTINUE
 200           IF (IX.GT.IXMAX(NPAT)) GO TO 220
 210           CONTINUE
C                                       Did we get anything?
 220        IPAT = IPAT + JPAT
            IF (IBLNK+IPAT.EQ.NPIX*NPIX) GO TO 300
            IF (JPAT.EQ.0) GO TO 250
 230        CONTINUE
C                                       Dropping out of the loop
C                                       indicates an error.
         WRITE (MSGTXT, 240) NPAT
 240     FORMAT ('PHAZIP: Too many iterations for patch',I6,'.')
         CALL MSGWRT (6)
         IERR = 1
         GO TO 999
 250     CONTINUE
C
      MSGTXT = 'PHAZIP: Too many patches.'
      CALL MSGWRT (6)
C                                       Unwrap the patches.
 300  DO 400 MPAT = 2, NPAT
         DO 320 IY = IYMIN(MPAT), IYMAX(MPAT)
            DO 310 IX = IXMIN(MPAT), IXMAX(MPAT)
               IF (PATCH(IX,IY).EQ.MPAT) THEN
C                                       Find a neighbour from a lower
C                                       patch.
                  IF (IX.GT.1) THEN
                     IF (PATCH(IX-1,IY).GT.0) THEN
                        IF (PATCH(IX-1,IY).LT.MPAT) THEN
                           STEP = PHA(IX,IY) - PHA(IX-1,IY)
                           GO TO 340
                           END IF
                        END IF
                     END IF
                  IF (IY.GT.1) THEN
                     IF (PATCH(IX,IY-1).GT.0) THEN
                        IF (PATCH(IX,IY-1).LT.MPAT) THEN
                           STEP = PHA(IX,IY) - PHA(IX,IY-1)
                           GO TO 340
                           END IF
                        END IF
                     END IF
                  IF (IX.LT.NPIX) THEN
                     IF (PATCH(IX+1,IY).GT.0) THEN
                        IF (PATCH(IX+1,IY).LT.MPAT) THEN
                           STEP = PHA(IX,IY) - PHA(IX+1,IY)
                           GO TO 340
                           END IF
                        END IF
                     END IF
                  IF (IY.LT.NPIX) THEN
                     IF (PATCH(IX,IY+1).GT.0) THEN
                        IF (PATCH(IX,IY+1).LT.MPAT) THEN
                           STEP = PHA(IX,IY) - PHA(IX,IY+1)
                           GO TO 340
                           END IF
                        END IF
                     END IF
                  END IF
 310           CONTINUE
 320        CONTINUE
C                                       Dropping out of the loop
C                                       indicates an error.
         WRITE (MSGTXT, 330) MPAT
 330     FORMAT ('PHAZIP: Couldn''t unwrap patch',I6,'.')
         CALL MSGWRT (6)
         GO TO 400
C                                       What's the step?
 340     STEP = 360.0*INT((STEP + SIGN(180.0,STEP))/360.0)
C                                       Adjust all pixels in this patch.
         DO 360 IY = IYMIN(MPAT), IYMAX(MPAT)
            DO 350 IX = IXMIN(MPAT), IXMAX(MPAT)
               IF (PATCH(IX,IY).EQ.MPAT) THEN
                  PHA(IX,IY) = PHA(IX,IY) - STEP
                  END IF
 350           CONTINUE
 360        CONTINUE
 400     CONTINUE
C
      IERR = 0
 999  RETURN
      END
      SUBROUTINE PHCLOS (NPIX, RMAX, RMIN, PHA, PATCH, NCLOS, IERR)
C-----------------------------------------------------------------------
C   PHCLOS searches for singleton pairs and tries to re-pair them so as
C   to minimize the total distance between them.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C      RMAX     R        The antenna radius, in cell units.
C      RMIN     R        Sub-reflector radius, in cell units.
C
C   Given and returned:
C      PHA      R(NPIX,NPIX)
C                        The phase map to be rectified.
C      PATCH    I(NPIX,NPIX)
C                        Scratch array.
C
C   Retured:
C      NCLOS    I        Number of closures.
C      IERR     I        Error status, 0 means success.
C
C   Algorithm:
C      The sides of a pixel and their ends are coded as follows:
C
C                                  + 8 -
C                                +-------+
C                              - |       | +
C                              1 |       | 4
C                              + |       | -
C                                +-------+
C                                  - 2 +
C
C      For example, SIDE = -8 indicates the right end of the top side.
C
C      A "singleton" is the point of termination of a discontinuity,
C      for example as marked by the asterisk in the following diagrams:
C
C
C               +-------+-------+         +-------+-------+
C               |       |       |         |       |       |
C               |  150  |  225  |         |   75  |   0   |
C               |       |       | +       |       |       | -
C               +-------*-----------      +-------*-----------
C               |       |       | -       |       |       | +
C               |   75  |   0   |         |  150  |  225  |
C               |       |       |         |       |       |
C               +-------+-------+         +-------+-------+
C                positive parity           negative parity
C
C                             Naked singleton
C
C      Singletons have the following properties:
C         1) Location, always with half-integral x-, and y-coordinates.
C         2) Positive or negative parity.
C         3) Direction.
C
C      Neither the location nor the parity can be changed by addition or
C      subtraction of 360 degrees to any of the neighbouring pixels.
C
C      Pairs of singletons connected by a discontinuity have opposite
C      parity.
C
C      The parity and direction of a naked singleton is coded as
C      follows:
C
C                      +2                        -2
C                       |                         |
C                     + | -                     - | +
C                       |                         |
C                 -     |     +             +     |     -
C            +3 --------*-------- +1   -3 --------*-------- -1
C                 +     |     -             -     |     +
C                       |                         |
C                     - | +                     + | -
C                       |                         |
C                      +4                        -4
C
C      In the above diagrams the singleton is denoted by an asterisk
C      and the +/- pairs denote the phase step.
C
C      A naked singleton can be transformed into a "cloaked singleton"
C      and visa versa:
C
C               +-------+-------+         +-------+-------+
C               |       |       |         |       |       |
C               |  150  |  225  |         |  150  |  225  |
C             - |       |       | +     + |       |       | +
C            -----------*-----------   -----------*-----------
C             + |       |       | -     - |       |       | -
C               |  435  |   0   |         | -285  |   0   |
C               |       |       |         |       |       |
C               +-------|-------+         +-------|-------+
C                     + | -                     - | +
C                       |                         |
C
C                            Cloaked singleton
C
C      PHCLOS assumes that the map has no cloaked singletons and tries
C      not to create any.
C
C      A pseudo-singleton is formed where one or both of the adjacent
C      pixels is blank:
C
C               +-------+-------+         +-------+-------+
C               |       |       |         |       |       |
C               | blank |  225  |         |   75  |   0   |
C               |       |       | +       |       |       | -
C               +-------*-----------      +-------*-----------
C               |       |       | -       |       |       | +
C               | blank |   0   |         | blank |  225  |
C               |       |       |         |       |       |
C               +-------+-------+         +-------+-------+
C
C                             Pseudo-singleton
C
C      Pseudo-singletons can be moved along the perimeter of the blank
C      patch they are associated with and can be created or destroyed
C      in pairs.
C
C      A "doubleton" is like a singleton but with a double step:
C
C               +-------+-------+         +-------+-------+
C               |       |       |         |       |       |
C               |  300  |  450  |         |  150  |   0   |
C               |       |       | ++      |       |       | --
C               +-------*-----------      +-------*-----------
C               |       |       | --      |       |       | ++
C               |  150  |   0   |         |  300  |  450  |
C               |       |       |         |       |       |
C               +-------+-------+         +-------+-------+
C                positive parity           negative parity
C
C                             Naked doubleton
C
C      We assume that the data contains no doubletons.  Tripletons
C      cannot occur.
C
C   Author: Mark Calabretta, Australia Telescope National Facility.
C   Origin; 1996/07/19  Code last modified; 1996/08/20
C-----------------------------------------------------------------------
      INTEGER   IERR, NCLOS, NPIX, PATCH(NPIX,NPIX)
      REAL      PHA(NPIX,NPIX), RMAX, RMIN
C
      INTEGER   MAXEND
      PARAMETER (MAXEND = 2048)
C
      INTEGER   EDGE, I, IBLNK, IEND, ITER, IX, IY, J, JEND, JPAT, JX,
     *          JY, KX, KY, MEND, NEND, NPAT, PARY(-8:8), RANK(MAXEND),
     *          SIDE, XOFF(-8:8), YOFF(-8:8)
      REAL      D, DMIN, DX, DY, ENDS(10,MAXEND), LEN, LEN1, LEN2, R1,
     *          R2, S, TEMP, X0, X1, X2, Y0, Y1, Y2
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA XOFF /+1, 0,0,0, +1, 0, -1, -1, 0, -1, +1, 0, +1, 0,0,0, -1/
      DATA YOFF /+1, 0,0,0, -1, 0, -1, +1, 0, -1, -1, 0, +1, 0,0,0, +1/
      DATA PARY /-3, 0,0,0, -2, 0, -1, -4, 0, +2, +3, 0, +4, 0,0,0, +1/
C-----------------------------------------------------------------------
C                                       Initialize.
      IBLNK = 0
      DO 20 IY = 1, NPIX
         DO 10 IX = 1, NPIX
            IF (PHA(IX,IY).EQ.FBLANK) THEN
               PATCH(IX,IY) = 1
               IBLNK = IBLNK + 1
            ELSE
               PATCH(IX,IY) = 0
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Categorize blank patches.
      NPAT = 0
 30   IF (IBLNK.GT.0) THEN
C                                       Find the first pixel in this
C                                       blank patch.
         NPAT = NPAT - 1
         DO 50 IY = 1, NPIX
            DO 40 IX = 1, NPIX
               IF (PATCH(IX,IY).EQ.1) THEN
                  PATCH(IX,IY) = NPAT
                  IBLNK = IBLNK - 1
                  GO TO 70
                  END IF
 40            CONTINUE
 50         CONTINUE
C
         WRITE (MSGTXT, 60) NPAT
 60      FORMAT ('PHCLOS: Couldn''t find first pixel in patch',I6,'.')
         CALL MSGWRT (6)
         IERR = 1
         GO TO 999
C                                       Find all pixels in this blank
C                                       patch.
 70      DO 160 ITER = 1, 10
            JPAT = 0
C                                       Scan left-to-right from bottom
C                                       to top.
            DO 90 IY = 1, NPIX
               DO 80 IX = 1, NPIX
                  IF (PATCH(IX,IY).EQ.1) THEN
                     IF (IX.GT.1) THEN
C                                       Look left.
                        IF (PATCH(IX-1,IY).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           GO TO 80
                           END IF
                        END IF
C
                     IF (IY.GT.1) THEN
C                                       Look below.
                        IF (PATCH(IX,IY-1).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           END IF
                        END IF
                     END IF
 80               CONTINUE
 90            CONTINUE
C                                       Scan bottom-to-top from right
C                                       to left.
            DO 110 IX = NPIX, 1, -1
               DO 100 IY = 1, NPIX
                  IF (PATCH(IX,IY).EQ.1) THEN
                     IF (IY.GT.1) THEN
C                                       Look below.
                        IF (PATCH(IX,IY-1).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           GO TO 100
                           END IF
                        END IF
C
                     IF (IX.LT.NPIX) THEN
C                                       Look right.
                        IF (PATCH(IX+1,IY).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           END IF
                        END IF
                     END IF
 100              CONTINUE
 110           CONTINUE
C                                       Scan right-to-left from top to
C                                       bottom.
            DO 130 IY = NPIX, 1, -1
               DO 120 IX = NPIX, 1, -1
                  IF (PATCH(IX,IY).EQ.1) THEN
                     IF (IX.LT.NPIX) THEN
C                                       Look right.
                        IF (PATCH(IX+1,IY).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           GO TO 120
                           END IF
                        END IF
C
                     IF (IY.LT.NPIX) THEN
C                                       Look above.
                        IF (PATCH(IX,IY+1).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           END IF
                        END IF
                     END IF
 120              CONTINUE
 130           CONTINUE
C                                       Scan top-to-bottom from left to
C                                       right.
            DO 150 IX = 1, NPIX
               DO 140 IY = NPIX, 1, -1
                  IF (PATCH(IX,IY).EQ.1) THEN
                     IF (IY.LT.NPIX) THEN
C                                       Look above.
                        IF (PATCH(IX,IY+1).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           GO TO 140
                           END IF
                        END IF
C
                     IF (IX.GT.1) THEN
C                                       Look left.
                        IF (PATCH(IX-1,IY).EQ.NPAT) THEN
                           PATCH(IX,IY) = NPAT
                           JPAT = JPAT + 1
                           END IF
                        END IF
                     END IF
 140              CONTINUE
 150           CONTINUE
C                                       Did we get anything?
            IF (JPAT.EQ.0) GO TO 30
            IBLNK = IBLNK - JPAT
            IF (IBLNK.EQ.0) GO TO 200
 160        CONTINUE
C                                       Dropping out of the loop
C                                       indicates an error.
         WRITE (MSGTXT, 170) NPAT
 170     FORMAT ('PHCLOS: Too many iterations for patch',I6,'.')
         CALL MSGWRT (6)
         IERR = 1
         GO TO 999
         END IF
C                                       Mark all pixels which have a
C                                       discontinuity.
 200  DO 220 IY = 1, NPIX
         DO 210 IX = 1, NPIX
            IF (PATCH(IX,IY).LT.0) GO TO 210
C                                       Look to the left.
            IF (IX.GT.1) THEN
               IF (PATCH(IX-1,IY).GE.0) THEN
                  IF (PHA(IX-1,IY)-PHA(IX,IY).GT.180.0) THEN
                     PATCH(IX,IY) = PATCH(IX,IY) + 1
                     END IF
                  END IF
               END IF
C                                       Look below.
            IF (IY.GT.1) THEN
               IF (PATCH(IX,IY-1).GE.0) THEN
                  IF (PHA(IX,IY-1)-PHA(IX,IY).GT.180.0) THEN
                     PATCH(IX,IY) = PATCH(IX,IY) + 2
                     END IF
                  END IF
               END IF
C                                       Look to the right.
            IF (IX.LT.NPIX) THEN
               IF (PATCH(IX+1,IY).GE.0) THEN
                  IF (PHA(IX+1,IY)-PHA(IX,IY).GT.180.0) THEN
                     PATCH(IX,IY) = PATCH(IX,IY) + 4
                     END IF
                  END IF
               END IF
C                                       Look above.
            IF (IY.LT.NPIX) THEN
               IF (PATCH(IX,IY+1).GE.0) THEN
                  IF (PHA(IX,IY+1)-PHA(IX,IY).GT.180.0) THEN
                     PATCH(IX,IY) = PATCH(IX,IY) + 8
                     END IF
                  END IF
               END IF
 210        CONTINUE
 220     CONTINUE
C                                       Find the end points for all
C                                       open-ended discontinuities.
      NEND = 0
      DO 300 IY = 1, NPIX
         DO 290 IX = 1, NPIX
            IF (PATCH(IX,IY).LE.0) GO TO 290
C                                       Do we have an end point?
            EDGE = 0
            DO 280 ITER = 1, 2
               IF (MOD(PATCH(IX,IY),2).EQ.1) THEN
C                                       Discontinuity on the left
C                                       (so IX > 1).
                  IF (MOD(PATCH(IX,IY)/8,2).NE.1) THEN
C                                       End point above?
                     SIDE = -1
                     IF (IY.EQ.NPIX) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX,IY+1),2).NE.1) THEN
                        IF (MOD(PATCH(IX-1,IY+1)/2,2).NE.1) THEN
                           IF (PATCH(IX,IY+1).LT.0) THEN
                              EDGE = PATCH(IX,IY+1)
                           ELSE IF (PATCH(IX-1,IY+1).LT.0) THEN
                              EDGE = PATCH(IX-1,IY+1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                  ELSE IF (MOD(PATCH(IX,IY)/2,2).NE.1) THEN
C                                       End point below?
                     SIDE = 1
                     IF (IY.EQ.1) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX,IY-1),2).NE.1) THEN
                        IF (MOD(PATCH(IX-1,IY-1)/8,2).NE.1) THEN
                           IF (PATCH(IX,IY-1).LT.0) THEN
                              EDGE = PATCH(IX,IY-1)
                           ELSE IF (PATCH(IX-1,IY-1).LT.0) THEN
                              EDGE = PATCH(IX-1,IY-1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                     END IF
                  END IF
C
               IF (MOD(PATCH(IX,IY)/2,2).EQ.1) THEN
C                                       Discontinuity below (so IY > 1).
                  IF (MOD(PATCH(IX,IY),2).NE.1) THEN
C                                       End point to the left?
                     SIDE = -2
                     IF (IX.EQ.1) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX-1,IY)/2,2).NE.1) THEN
                        IF (MOD(PATCH(IX-1,IY-1)/4,2).NE.1) THEN
                           IF (PATCH(IX-1,IY).LT.0) THEN
                              EDGE = PATCH(IX-1,IY)
                           ELSE IF (PATCH(IX-1,IY-1).LT.0) THEN
                              EDGE = PATCH(IX-1,IY-1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                  ELSE IF (MOD(PATCH(IX,IY)/4,2).NE.1) THEN
C                                       End point to the right?
                     SIDE = 2
                     IF (IX.EQ.NPIX) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX+1,IY)/2,2).NE.1) THEN
                        IF (MOD(PATCH(IX+1,IY-1),2).NE.1) THEN
                           IF (PATCH(IX+1,IY).LT.0) THEN
                              EDGE = PATCH(IX+1,IY)
                           ELSE IF (PATCH(IX+1,IY-1).LT.0) THEN
                              EDGE = PATCH(IX+1,IY-1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                     END IF
                  END IF
C
               IF (MOD(PATCH(IX,IY)/4,2).EQ.1) THEN
C                                       Discontinuity on the right
C                                       (so IX < NPIX).
                  IF (MOD(PATCH(IX,IY)/2,2).NE.1) THEN
C                                       End point below?
                     SIDE = -4
                     IF (IY.EQ.1) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX,IY-1)/4,2).NE.1) THEN
                        IF (MOD(PATCH(IX+1,IY-1)/8,2).NE.1) THEN
                           IF (PATCH(IX,IY-1).LT.0) THEN
                              EDGE = PATCH(IX,IY-1)
                           ELSE IF (PATCH(IX+1,IY-1).LT.0) THEN
                              EDGE = PATCH(IX+1,IY-1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                  ELSE IF (MOD(PATCH(IX,IY)/8,2).NE.1) THEN
C                                       End point above?
                     SIDE = 4
                     IF (IY.EQ.NPIX) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX,IY+1)/4,2).NE.1) THEN
                        IF (MOD(PATCH(IX+1,IY+1)/2,2).NE.1) THEN
                           IF (PATCH(IX,IY+1).LT.0) THEN
                              EDGE = PATCH(IX,IY+1)
                           ELSE IF (PATCH(IX+1,IY+1).LT.0) THEN
                              EDGE = PATCH(IX+1,IY+1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                     END IF
                  END IF
C
               IF (MOD(PATCH(IX,IY)/8,2).EQ.1) THEN
C                                       Discontinuity above
C                                       (so IY < NPIX).
                  IF (MOD(PATCH(IX,IY)/4,2).NE.1) THEN
C                                       End point to the right?
                     SIDE = -8
                     IF (IX.EQ.NPIX) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX+1,IY)/8,2).NE.1) THEN
                        IF (MOD(PATCH(IX+1,IY+1),2).NE.1) THEN
                           IF (PATCH(IX+1,IY).LT.0) THEN
                              EDGE = PATCH(IX+1,IY)
                           ELSE IF (PATCH(IX+1,IY+1).LT.0) THEN
                              EDGE = PATCH(IX+1,IY+1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                  ELSE IF (MOD(PATCH(IX,IY),2).NE.1) THEN
C                                       End point to the left?
                     SIDE = 8
                     IF (IX.EQ.1) THEN
                        EDGE = 1
                        GO TO 230
                        END IF
                     IF (MOD(PATCH(IX-1,IY)/8,2).NE.1) THEN
                        IF (MOD(PATCH(IX-1,IY+1)/4,2).NE.1) THEN
                           IF (PATCH(IX-1,IY).LT.0) THEN
                              EDGE = PATCH(IX-1,IY)
                           ELSE IF (PATCH(IX-1,IY+1).LT.0) THEN
                              EDGE = PATCH(IX-1,IY+1)
                              END IF
                           GO TO 230
                           END IF
                        END IF
                     END IF
                  END IF
C                                       Not an end point.
               GO TO 290
C                                       Found an end point.
 230           NEND = NEND + 1
               IF (NEND.GT.MAXEND) THEN
                  WRITE (MSGTXT, 240)
 240              FORMAT ('PHCLOS: Too many end points.')
                  CALL MSGWRT (6)
                  IERR = 1
                  GO TO 999
                  END IF
C                                       Record the positive parity end
C                                       as the first of the pair.
               IF (PARY(SIDE).GT.0) THEN
                  ENDS(1,NEND) = IX + XOFF(SIDE)*0.5
                  ENDS(2,NEND) = IY + YOFF(SIDE)*0.5
                  ENDS(3,NEND) = PARY(SIDE)
                  ENDS(4,NEND) = EDGE
               ELSE
                  ENDS(5,NEND) = IX + XOFF(SIDE)*0.5
                  ENDS(6,NEND) = IY + YOFF(SIDE)*0.5
                  ENDS(7,NEND) = PARY(SIDE)
                  ENDS(8,NEND) = EDGE
                  END IF
C                                       Trace the discontinuity to the
C                                       other end.
               PATCH(IX,IY) = PATCH(IX,IY) - ABS(SIDE)
C
               SIDE = -SIDE
               EDGE = 0
               JX = IX
               JY = IY
               DO 260 J = 1, 10000
                  IF (ABS(SIDE).EQ.1) THEN
C                                       Discontinuity on the left
C                                       (so JX > 1).
                     IF (SIDE.LT.0) THEN
C                                       Look above.
                        IF (JY.EQ.NPIX) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX-1,JY+1)/2,2).EQ.1) THEN
                           JX = JX - 1
                           JY = JY + 1
                           SIDE = -2
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY+1),2).EQ.1) THEN
                           JY = JY + 1
                           SIDE = -1
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/8,2).EQ.1) THEN
                           SIDE = -8
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX,JY+1).LT.0) THEN
                           EDGE = PATCH(JX,JY+1)
                        ELSE IF (PATCH(JX-1,JY+1).LT.0) THEN
                           EDGE = PATCH(JX-1,JY+1)
                           END IF
                     ELSE
C                                       Look below.
                        IF (JY.EQ.1) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX-1,JY-1)/8,2).EQ.1) THEN
                           JX = JX - 1
                           JY = JY - 1
                           SIDE = 8
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY-1),2).EQ.1) THEN
                           JY = JY - 1
                           SIDE = 1
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/2,2).EQ.1) THEN
                           SIDE = 2
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX,JY-1).LT.0) THEN
                           EDGE = PATCH(JX,JY-1)
                        ELSE IF (PATCH(JX-1,JY-1).LT.0) THEN
                           EDGE = PATCH(JX-1,JY-1)
                           END IF
                        END IF
                  ELSE IF (ABS(SIDE).EQ.2) THEN
C                                       Discontinuity below (so JY > 1).
                     IF (SIDE.LT.0) THEN
C                                       Look left.
                        IF (JX.EQ.1) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX-1,JY-1)/4,2).EQ.1) THEN
                           JX = JX - 1
                           JY = JY - 1
                           SIDE = -4
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX-1,JY)/2,2).EQ.1) THEN
                           JX = JX - 1
                           SIDE = -2
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY),2).EQ.1) THEN
                           SIDE = -1
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX-1,JY).LT.0) THEN
                           EDGE = PATCH(JX-1,JY)
                        ELSE IF (PATCH(JX-1,JY-1).LT.0) THEN
                           EDGE = PATCH(JX-1,JY-1)
                           END IF
                     ELSE
C                                       Look right.
                        IF (JX.EQ.NPIX) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX+1,JY-1),2).EQ.1) THEN
                           JX = JX + 1
                           JY = JY - 1
                           SIDE = 1
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX+1,JY)/2,2).EQ.1) THEN
                           JX = JX + 1
                           SIDE = 2
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/4,2).EQ.1) THEN
                           SIDE = 4
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX+1,JY).LT.0) THEN
                           EDGE = PATCH(JX+1,JY)
                        ELSE IF (PATCH(JX+1,JY-1).LT.0) THEN
                           EDGE = PATCH(JX+1,JY-1)
                           END IF
                        END IF
                  ELSE IF (ABS(SIDE).EQ.4) THEN
C                                       Discontinuity on the right
C                                       (so JX < NPIX).
                     IF (SIDE.LT.0) THEN
C                                       Look below.
                        IF (JY.EQ.1) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX+1,JY-1)/8,2).EQ.1) THEN
                           JX = JX + 1
                           JY = JY - 1
                           SIDE = -8
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY-1)/4,2).EQ.1) THEN
                           JY = JY - 1
                           SIDE = -4
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/2,2).EQ.1) THEN
                           SIDE = -2
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX,JY-1).LT.0) THEN
                           EDGE = PATCH(JX,JY-1)
                        ELSE IF (PATCH(JX+1,JY-1).LT.0) THEN
                           EDGE = PATCH(JX+1,JY-1)
                           END IF
                     ELSE
C                                       Look above.
                        IF (JY.EQ.NPIX) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX+1,JY+1)/2,2).EQ.1) THEN
                           JX = JX + 1
                           JY = JY + 1
                           SIDE = 2
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY+1)/4,2).EQ.1) THEN
                           JY = JY + 1
                           SIDE = 4
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/8,2).EQ.1) THEN
                           SIDE = 8
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX,JY+1).LT.0) THEN
                           EDGE = PATCH(JX,JY+1)
                        ELSE IF (PATCH(JX+1,JY+1).LT.0) THEN
                           EDGE = PATCH(JX+1,JY+1)
                           END IF
                        END IF
                  ELSE IF (ABS(SIDE).EQ.8) THEN
C                                       Discontinuity above
C                                       (so JY < NPIX).
                     IF (SIDE.LT.0) THEN
C                                       Look right.
                        IF (JX.EQ.NPIX) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX+1,JY+1),2).EQ.1) THEN
                           JX = JX + 1
                           JY = JY + 1
                           SIDE = -1
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX+1,JY)/8,2).EQ.1) THEN
                           JX = JX + 1
                           SIDE = -8
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY)/4,2).EQ.1) THEN
                           SIDE = -4
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX+1,JY).LT.0) THEN
                           EDGE = PATCH(JX+1,JY)
                        ELSE IF (PATCH(JX+1,JY+1).LT.0) THEN
                           EDGE = PATCH(JX+1,JY+1)
                           END IF
                     ELSE
C                                       Look left.
                        IF (JX.EQ.1) THEN
                           EDGE = 1
                           GO TO 270
                           END IF
                        IF (MOD(PATCH(JX-1,JY+1)/4,2).EQ.1) THEN
                           JX = JX - 1
                           JY = JY + 1
                           SIDE = 4
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX-1,JY)/8,2).EQ.1) THEN
                           JX = JX - 1
                           SIDE = 8
                           GO TO 250
                           END IF
                        IF (MOD(PATCH(JX,JY),2).EQ.1) THEN
                           SIDE = 1
                           GO TO 250
                           END IF
C
                        IF (PATCH(JX-1,JY).LT.0) THEN
                           EDGE = PATCH(JX-1,JY)
                        ELSE IF (PATCH(JX-1,JY+1).LT.0) THEN
                           EDGE = PATCH(JX-1,JY+1)
                           END IF
                        END IF
                     END IF
C                                       Reached the other end.
                  GO TO 270
C                                       Got the next segment.
 250              PATCH(JX,JY) = PATCH(JX,JY) - ABS(SIDE)
 260              CONTINUE
C                                       Record the other end point.
 270           IF (PARY(SIDE).GT.0) THEN
                  ENDS(1,NEND) = JX + XOFF(SIDE)*0.5
                  ENDS(2,NEND) = JY + YOFF(SIDE)*0.5
                  ENDS(3,NEND) = PARY(SIDE)
                  ENDS(4,NEND) = EDGE
               ELSE
                  ENDS(5,NEND) = JX + XOFF(SIDE)*0.5
                  ENDS(6,NEND) = JY + YOFF(SIDE)*0.5
                  ENDS(7,NEND) = PARY(SIDE)
                  ENDS(8,NEND) = EDGE
                  END IF
               DX = ENDS(5,NEND) - ENDS(1,NEND)
               DY = ENDS(6,NEND) - ENDS(2,NEND)
               ENDS(9,NEND)  = ABS(DX) + ABS(DY)
               ENDS(10,NEND) = SQRT(DX*DX + DY*DY)
C
 280           CONTINUE
 290        CONTINUE
 300     CONTINUE
C                                       Try to rematch pairs to reduce
C                                       the total distance between them.
 340  LEN = 99999.0
      DO 360 IEND = 1, NEND
         LEN1 = 0.0
         DO 350 JEND = 1, NEND
            LEN2 = ENDS(10,JEND)
            IF (LEN2.GT.LEN1 .AND. LEN2.LE.LEN) THEN
               IF (LEN2.EQ.LEN .AND. JEND.LE.RANK(IEND-1)) GO TO 350
               RANK(IEND) = JEND
               LEN1 = LEN2
               END IF
 350        CONTINUE
         LEN = LEN1
 360     CONTINUE
C
      DO 380 I = 1, NEND-1
         IEND = RANK(I)
         DO 370 J = I+1, NEND
            JEND = RANK(J)
C
            LEN = ENDS(10,IEND) + ENDS(10,JEND)
            IF (ENDS(8,JEND).LT.0 .AND.
     *          ENDS(8,JEND).EQ.ENDS(4,IEND)) THEN
               LEN1 = 0.0
            ELSE
               DX = ENDS(5,JEND) - ENDS(1,IEND)
               DY = ENDS(6,JEND) - ENDS(2,IEND)
               LEN1 = SQRT(DX*DX + DY*DY)
               END IF
            IF (LEN1.GE.LEN) GO TO 370
C
            IF (ENDS(8,IEND).LT.0 .AND.
     *          ENDS(8,IEND).EQ.ENDS(4,JEND)) THEN
               LEN2 = 0.0
            ELSE
               DX = ENDS(5,IEND) - ENDS(1,JEND)
               DY = ENDS(6,IEND) - ENDS(2,JEND)
               LEN2 = SQRT(DX*DX + DY*DY)
               END IF
C
            IF (LEN1+LEN2.LT.LEN) THEN
C                                       Swap the end-points.
               TEMP = ENDS(5,IEND)
               ENDS(5,IEND) = ENDS(5,JEND)
               ENDS(5,JEND) = TEMP
               TEMP = ENDS(6,IEND)
               ENDS(6,IEND) = ENDS(6,JEND)
               ENDS(6,JEND) = TEMP
               TEMP = ENDS(7,IEND)
               ENDS(7,IEND) = ENDS(7,JEND)
               ENDS(7,JEND) = TEMP
               TEMP = ENDS(8,IEND)
               ENDS(8,IEND) = ENDS(8,JEND)
               ENDS(8,JEND) = TEMP
               ENDS(9,IEND) = ABS(ENDS(5,IEND)-ENDS(1,IEND)) +
     *                        ABS(ENDS(6,IEND)-ENDS(2,IEND))
               ENDS(9,JEND) = ABS(ENDS(5,JEND)-ENDS(1,JEND)) +
     *                        ABS(ENDS(6,JEND)-ENDS(2,JEND))
               ENDS(10,IEND) = LEN1
               ENDS(10,JEND) = LEN2
C
               GO TO 340
               END IF
 370        CONTINUE
 380     CONTINUE
C                                       Minimize the distance between
C                                       singletons connected to a blank.
      DO 430 IEND = 1, NEND
C        Is one end or the other blank?
         IF (ENDS(4,IEND).LT.0.0 .OR. ENDS(8,IEND).LT.0.0) THEN
            IF (ENDS(4,IEND).LT.0.0) THEN
               IF (ENDS(8,IEND).LT.0.0) GO TO 430
C                                       The pseudo-singleton is at the
C                                       positive parity end.
               X0 = ENDS(5,IEND)
               Y0 = ENDS(6,IEND)
               X1 = ENDS(1,IEND)
               Y1 = ENDS(2,IEND)
               EDGE = NINT(ENDS(4,IEND))
            ELSE
C                                       The pseudo-singleton is at the
C                                       negative parity end.
               X0 = ENDS(1,IEND)
               Y0 = ENDS(2,IEND)
               X1 = ENDS(5,IEND)
               Y1 = ENDS(6,IEND)
               EDGE = NINT(ENDS(8,IEND))
               END IF
C                                       Define a region to search for
C                                       closer blanks.
            JX = NINT(X0 - ENDS(10,IEND) - 0.5)
            JY = NINT(Y0 - ENDS(10,IEND) - 0.5)
            KX = NINT(X0 + ENDS(10,IEND) + 0.5)
            KY = NINT(Y0 + ENDS(10,IEND) + 0.5)
            IF (JX.LT.1) JX = 1
            IF (JY.LT.1) JY = 1
            IF (KX.GT.NPIX) KX = NPIX
            IF (KY.GT.NPIX) KY = NPIX
C                                       Search for the closest blank
C                                       pixel from the same patch.
            X2 = X1
            Y2 = Y1
            DMIN = ENDS(10,IEND)
            DO 420 IX = JX, KX
               DO 410 IY = JY, KY
                  IF (PATCH(IX,IY).EQ.EDGE) THEN
                     DX = IX - X0
                     DY = IY - Y0
                     D = SQRT(DX*DX + DY*DY)
                     IF (D.LT.DMIN) THEN
C                                       Found one.
                        IF (X2.LT.X0) THEN
                           X2 = IX + 0.5
                        ELSE
                           X2 = IX - 0.5
                           END IF
                        IF (Y2.LT.Y0) THEN
                           Y2 = IY + 0.5
                        ELSE
                           Y2 = IY - 0.5
                           END IF
                        DMIN = D
                        END IF
                     END IF
 410              CONTINUE
 420           CONTINUE
C
            IF (X2.EQ.X1 .AND. Y2.EQ.Y1) GO TO 430
C                                       Relocate the pseudo-singleton.
            S = (X1-X0)*(Y2-Y0) - (X2-X0)*(Y1-Y0)
            IF (NINT(ENDS(4,IEND)).EQ.EDGE) THEN
C                                       Positive parity.
               ENDS(1,IEND) = X2
               ENDS(2,IEND) = Y2
               IF (S.LT.0.0) THEN
                  ENDS(3,IEND) = 5
               ELSE
                  ENDS(3,IEND) = 6
                  END IF
            ELSE
C                                       Negative parity.
               ENDS(5,IEND) = X2
               ENDS(6,IEND) = Y2
               IF (S.GT.0.0) THEN
                  ENDS(7,IEND) = -5
               ELSE
                  ENDS(7,IEND) = -6
                  END IF
               END IF
C
            DX = X2 - X0
            DY = Y2 - Y0
            ENDS(9,IEND)  = NINT(ABS(DX) + ABS(DY))
            ENDS(10,IEND) = SQRT(DX*DX + DY*DY)
            END IF
 430     CONTINUE
C                                       Try rerouting each end of a
C                                       discontinuity to an edge.
      MEND = NEND
      DO 530 IEND = 1, NEND
C                                       Check that both ends are non-
C                                       blank.
         IF (ENDS(4,IEND).LT.0.0 .OR. ENDS(8,IEND).LT.0.0) GO TO 530
C
         X0 = NPIX/2 + 1
         Y0 = NPIX/2 + 1
         X1 = ENDS(1,IEND) - X0
         Y1 = ENDS(2,IEND) - Y0
         R1 = SQRT(X1*X1 + Y1*Y1)
         X2 = ENDS(5,IEND) - X0
         Y2 = ENDS(6,IEND) - Y0
         R2 = SQRT(X2*X2 + Y2*Y2)
C
         IF ((RMAX-R2)+(RMAX-R1).LT.ENDS(10,IEND)) THEN
            X1 = AINT(X1*RMAX/R1 + X0)
            Y1 = AINT(Y1*RMAX/R1 + Y0)
            X2 = AINT(X2*RMAX/R2 + X0)
            Y2 = AINT(Y2*RMAX/R2 + Y0)
            TEMP = -1.0
         ELSE IF ((R2-RMIN)+(R1-RMIN).LT.ENDS(10,IEND)) THEN
            X1 = AINT(X1*RMIN/R1 + X0)
            Y1 = AINT(Y1*RMIN/R1 + Y0)
            X2 = AINT(X2*RMIN/R2 + X0)
            Y2 = AINT(Y2*RMIN/R2 + Y0)
            TEMP = -2.0
         ELSE
C                                       No win.
            GO TO 530
            END IF
C
         X1 = X1 + SIGN(0.5,X1)
         Y1 = Y1 + SIGN(0.5,Y1)
         X2 = X2 + SIGN(0.5,X2)
         Y2 = Y2 + SIGN(0.5,Y2)
C
         MEND = MEND + 1
         S = (ENDS(1,IEND)-ENDS(5,IEND))*(Y2-ENDS(6,IEND)) -
     *       (X2-ENDS(5,IEND))*(ENDS(2,IEND)-ENDS(6,IEND))
         ENDS(1,MEND) = X2
         ENDS(2,MEND) = Y2
         IF (S.LT.0.0) THEN
            ENDS(3,MEND) = 5
         ELSE
            ENDS(3,MEND) = 6
            END IF
         ENDS(4,MEND) = TEMP
         ENDS(5,MEND) = ENDS(5,IEND)
         ENDS(6,MEND) = ENDS(6,IEND)
         ENDS(7,MEND) = ENDS(7,IEND)
         ENDS(8,MEND) = ENDS(8,IEND)
         DX = ENDS(5,MEND) - ENDS(1,MEND)
         DY = ENDS(6,MEND) - ENDS(2,MEND)
         ENDS(9,MEND)  = NINT(ABS(DX) + ABS(DY))
         ENDS(10,MEND) = SQRT(DX*DX + DY*DY)
C
         S = (ENDS(5,IEND)-ENDS(1,IEND))*(Y1-ENDS(2,IEND)) -
     *       (X1-ENDS(1,IEND))*(ENDS(6,IEND)-ENDS(2,IEND))
         ENDS(5,IEND) = X1
         ENDS(6,IEND) = Y1
         IF (S.GT.0.0) THEN
            ENDS(7,IEND) = -5
         ELSE
            ENDS(7,IEND) = -6
            END IF
         ENDS(8,IEND) = TEMP
         DX = ENDS(5,IEND) - ENDS(1,IEND)
         DY = ENDS(6,IEND) - ENDS(2,IEND)
         ENDS(9,IEND)  = NINT(ABS(DX) + ABS(DY))
         ENDS(10,IEND) = SQRT(DX*DX + DY*DY)
 530     CONTINUE
      NEND = MEND
C                                       Close open-ended
C                                       discontinuities.
      NCLOS = 0
      DO 600 IEND = 1, NEND
C                                       Uncomment for debugging info.
C        WRITE (MSGTXT, 610)
C    *      ENDS(1,IEND), ENDS(2,IEND),
C    *      NINT(ENDS(3,IEND)), NINT(ENDS(4,IEND)),
C    *      ENDS(5,IEND), ENDS(6,IEND),
C    *      NINT(ENDS(7,IEND)), NINT(ENDS(8,IEND)),
C    *      NINT(ENDS(9,IEND)), ENDS(10,IEND)
C610     FORMAT (2F7.1,2I4,2X,2F7.1,2I4,2X,I5,F7.1)
C        CALL MSGWRT (3)
C
C                                       Let the perimeter of the blank
C                                       patch do the work.
         IF (ENDS(4,IEND).LT.0.0 .AND. ENDS(8,IEND).LT.0.0) THEN
            IF (ENDS(4,IEND).EQ.ENDS(8,IEND)) GO TO 600
            END IF
C                                       Build a wall across it.
         CALL DAMNIT (NPIX, PHA, ENDS(1,IEND))
         NCLOS = NCLOS + 1
 600     CONTINUE
C
      IERR = 0
 999  RETURN
      END
      SUBROUTINE DAMNIT (NPIX, PHA, ENDS)
C-----------------------------------------------------------------------
C   DAMNIT connects pairs of naked singletons by the shortest path
C   without converting them to cloaked singletons.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C
C   Given and returned:
C      PHA      R(NPIX,NPIX)
C                        The phase map to be rectified.
C      ENDS     R(10)    Information about the singleton pairs:
C                           1) X-coordinate of the positive parity end.
C                           2) Y-coordinate of the positive parity end.
C                           3) Parity (positive) and direction code.
C                           4) Blank patch number for pseudo-singletons.
C                           5) X-coordinate of the negative parity end.
C                           6) Y-coordinate of the negative parity end.
C                           7) Parity (negative) and direction code.
C                           8) Blank patch number for pseudo-singletons.
C                           9) L1 distance between ends.
C                          10) L2 distance between ends.
C
C   Author: Mark Calabretta, Australia Telescope National Facility.
C   Origin; 1996/08/06  Code last modified; 1996/08/14
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      ENDS(10), PHA(NPIX,NPIX)
C
      INTEGER   DIR, IX, IY, J, JQ, JX, JY, K, LX, LY, MODE
      REAL      DX, DY, P0, P1, P2, P3, P4, X, Y
C
      INCLUDE 'INCS:DDCH.INC'
C
C     Statement functions to make life easier.
      REAL      CYCLE, MATCH
      CYCLE(P1,P2) = NINT((P1-P2)/360.0)*360.0
      MATCH(P1,P2) = P1 - CYCLE(P1,P2)
C-----------------------------------------------------------------------
      LX = NINT(ENDS(5) - ENDS(1))
      LY = NINT(ENDS(6) - ENDS(2))
      IF (LX.GT.0 .AND. ABS(LY).LE.LX) THEN
         DIR = 1
      ELSE IF (LY.GT.0 .AND. ABS(LX).LE.LY) THEN
         DIR = 2
      ELSE IF (LX.LT.0 .AND. ABS(LY).LT.ABS(LX)) THEN
         DIR = 3
      ELSE IF (LY.LT.0 .AND. ABS(LX).LE.ABS(LY)) THEN
         DIR = 4
         END IF
C
      JQ = NINT(ENDS(3))
      CALL FIXEND (NPIX,  LX,  LY, ENDS(1), ENDS(2), JQ, PHA, JX, JY)
      ENDS(3) = JQ
C
      K = MAX(ABS(LX), ABS(LY))
      IF (K.GT.0) THEN
         DX = REAL(LX)/K
         DY = REAL(LY)/K
C
         DO 20 J = 1, K-1
            X = ENDS(1) + (J + 0.4999)*DX
            Y = ENDS(2) + (J + 0.4999)*DY
C
            IF (DIR.EQ.1) THEN
               IX = NINT(X)
               IY = NINT(Y - 0.5)
               IF (IY.EQ.JY) THEN
                  MODE = 0
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX+1,JY)
                  P2 = PHA(JX+1,JY+1)
                  P3 = PHA(JX,JY+1)
               ELSE IF (IY.GT.JY) THEN
                  MODE = 1
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX+1,JY)
                  P2 = PHA(JX+1,JY+1)
                  P3 = PHA(JX+1,JY+2)
                  P4 = PHA(JX,JY+2)
               ELSE
                  MODE = -1
                  P0 = PHA(JX,JY+1)
                  P1 = PHA(JX+1,JY+1)
                  P2 = PHA(JX+1,JY)
                  P3 = PHA(JX+1,JY-1)
                  P4 = PHA(JX,JY-1)
                  END IF
            ELSE IF (DIR.EQ.2) THEN
               IX = NINT(X + 0.5)
               IY = NINT(Y)
               IF (IX.EQ.JX) THEN
                  MODE = 0
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX,JY+1)
                  P2 = PHA(JX-1,JY+1)
                  P3 = PHA(JX-1,JY)
               ELSE IF (IX.LT.JX) THEN
                  MODE = 1
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX,JY+1)
                  P2 = PHA(JX-1,JY+1)
                  P3 = PHA(JX-2,JY+1)
                  P4 = PHA(JX-2,JY)
               ELSE
                  MODE = -1
                  P0 = PHA(JX-1,JY)
                  P1 = PHA(JX-1,JY+1)
                  P2 = PHA(JX,JY+1)
                  P3 = PHA(JX+1,JY+1)
                  P4 = PHA(JX+1,JY)
                  END IF
            ELSE IF (DIR.EQ.3) THEN
               IX = NINT(X)
               IY = NINT(Y + 0.5)
               IF (IY.EQ.JY) THEN
                  MODE = 0
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX-1,JY)
                  P2 = PHA(JX-1,JY-1)
                  P3 = PHA(JX,JY-1)
               ELSE IF (IY.LT.JY) THEN
                  MODE = 1
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX-1,JY)
                  P2 = PHA(JX-1,JY-1)
                  P3 = PHA(JX-1,JY-2)
                  P4 = PHA(JX,JY-2)
               ELSE
                  MODE = -1
                  P0 = PHA(JX,JY-1)
                  P1 = PHA(JX-1,JY-1)
                  P2 = PHA(JX-1,JY)
                  P3 = PHA(JX-1,JY+1)
                  P4 = PHA(JX,JY+1)
                  END IF
            ELSE IF (DIR.EQ.4) THEN
               IX = NINT(X - 0.5)
               IY = NINT(Y)
               IF (IX.EQ.JX) THEN
                  MODE = 0
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX,JY-1)
                  P2 = PHA(JX+1,JY-1)
                  P3 = PHA(JX+1,JY)
               ELSE IF (IX.GT.JX) THEN
                  MODE = 1
                  P0 = PHA(JX,JY)
                  P1 = PHA(JX,JY-1)
                  P2 = PHA(JX+1,JY-1)
                  P3 = PHA(JX+2,JY-1)
                  P4 = PHA(JX+2,JY)
               ELSE
                  MODE = -1
                  P0 = PHA(JX+1,JY)
                  P1 = PHA(JX+1,JY-1)
                  P2 = PHA(JX,JY-1)
                  P3 = PHA(JX-1,JY-1)
                  P4 = PHA(JX-1,JY)
                  END IF
               END IF
C
            IF (MODE.EQ.0) THEN
               IF (P1.NE.FBLANK .AND. P0.NE.FBLANK) P1 = MATCH(P1,P0)
               IF (P2.NE.FBLANK .AND. P3.NE.FBLANK) P2 = MATCH(P2,P3)
            ELSE IF (MODE.EQ.1) THEN
               IF (P1.NE.FBLANK .AND. P0.NE.FBLANK) P1 = MATCH(P1,P0)
               IF (P2.NE.FBLANK) THEN
                  IF (P1.NE.FBLANK) P2 = MATCH(P2,P1)
                  IF (P3.NE.FBLANK) THEN
                     P3 = MATCH(P3,P2) + 360.0
                     IF (P4.NE.FBLANK) P4 = MATCH(P4,P3)
                     END IF
                  END IF
            ELSE
               IF (P1.NE.FBLANK .AND. P0.NE.FBLANK) P1 = MATCH(P1,P0)
               IF (P2.NE.FBLANK) THEN
                  IF (P1.NE.FBLANK) P2 = MATCH(P2,P1)
                  IF (P3.NE.FBLANK) THEN
                     P3 = MATCH(P3,P2) - 360.0
                     IF (P4.NE.FBLANK) P4 = MATCH(P4,P3)
                     END IF
                  END IF
               END IF
C
            IF (DIR.EQ.1) THEN
               IF (IY.EQ.JY) THEN
                  PHA(JX,JY) = P0
                  PHA(JX+1,JY) = P1
                  PHA(JX+1,JY+1) = P2
                  PHA(JX,JY+1) = P3
               ELSE IF (IY.GT.JY) THEN
                  PHA(JX,JY) = P0
                  PHA(JX+1,JY) = P1
                  PHA(JX+1,JY+1) = P2
                  PHA(JX+1,JY+2) = P3
                  PHA(JX,JY+2) = P4
               ELSE
                  PHA(JX,JY+1) = P0
                  PHA(JX+1,JY+1) = P1
                  PHA(JX+1,JY) = P2
                  PHA(JX+1,JY-1) = P3
                  PHA(JX,JY-1) = P4
                  END IF
            ELSE IF (DIR.EQ.2) THEN
               IF (IX.EQ.JX) THEN
                  PHA(JX,JY) = P0
                  PHA(JX,JY+1) = P1
                  PHA(JX-1,JY+1) = P2
                  PHA(JX-1,JY) = P3
               ELSE IF (IX.LT.JX) THEN
                  PHA(JX,JY) = P0
                  PHA(JX,JY+1) = P1
                  PHA(JX-1,JY+1) = P2
                  PHA(JX-2,JY+1) = P3
                  PHA(JX-2,JY) = P4
               ELSE
                  PHA(JX-1,JY) = P0
                  PHA(JX-1,JY+1) = P1
                  PHA(JX,JY+1) = P2
                  PHA(JX+1,JY+1) = P3
                  PHA(JX+1,JY) = P4
                  END IF
            ELSE IF (DIR.EQ.3) THEN
               IF (IY.EQ.JY) THEN
                  PHA(JX,JY) = P0
                  PHA(JX-1,JY) = P1
                  PHA(JX-1,JY-1) = P2
                  PHA(JX,JY-1) = P3
               ELSE IF (IY.LT.JY) THEN
                  PHA(JX,JY) = P0
                  PHA(JX-1,JY) = P1
                  PHA(JX-1,JY-1) = P2
                  PHA(JX-1,JY-2) = P3
                  PHA(JX,JY-2) = P4
               ELSE
                  PHA(JX,JY-1) = P0
                  PHA(JX-1,JY-1) = P1
                  PHA(JX-1,JY) = P2
                  PHA(JX-1,JY+1) = P3
                  PHA(JX,JY+1) = P4
                  END IF
            ELSE IF (DIR.EQ.4) THEN
               IF (IX.EQ.JX) THEN
                  PHA(JX,JY) = P0
                  PHA(JX,JY-1) = P1
                  PHA(JX+1,JY-1) = P2
                  PHA(JX+1,JY) = P3
               ELSE IF (IX.GT.JX) THEN
                  PHA(JX,JY) = P0
                  PHA(JX,JY-1) = P1
                  PHA(JX+1,JY-1) = P2
                  PHA(JX+2,JY-1) = P3
                  PHA(JX+2,JY) = P4
               ELSE
                  PHA(JX+1,JY) = P0
                  PHA(JX+1,JY-1) = P1
                  PHA(JX,JY-1) = P2
                  PHA(JX-1,JY-1) = P3
                  PHA(JX-1,JY) = P4
                  END IF
               END IF
C
            JX = IX
            JY = IY
 20         CONTINUE
         END IF
C
      JQ = NINT(ENDS(7))
      CALL FIXEND (NPIX, -LX, -LY, ENDS(5), ENDS(6), JQ, PHA, JX, JY)
      ENDS(7) = JQ
C
 999  RETURN
      END
      SUBROUTINE FIXEND (NPIX, LX, LY, X0, Y0, PARITY, PHA, IX, IY)
C-----------------------------------------------------------------------
C   FIXEND reorients a singleton towards the specified direction.  A
C   cloaked singleton may be given but a naked singleton will be
C   returned.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C      LX,LY    I        Direction components in x and y.
C      X0,Y0    R        Coordinates of the singleton.
C      PARITY   I        The parity of the singleton; only the sign is
C                        used.
C
C   Given and returned:
C      PHA      R(NPIX,NPIX)
C                        The phase map to be rectified.
C      IX,IY    I        Coordinates of the pixel on the downward side
C                        of the phase step.
C
C
C   Algorithm:
C      The pixels to be fixed are copied out of the phase map and
C      labelled so that the singleton, marked with an asterisk in the
C      following diagram, is desired to travel to the right:
C
C                       +-------+-------+-------+
C                       |       |       |       |
C                       |   P2  |   P1  |   R1  |
C                       |       |       |       |
C                       +-------*-------+-------+
C                       |       |       |       |
C                       |   P3  |   P4  |   R4  |
C                       |       |       |       |
C                       +-------+-------+-------+
C
C                            Pixel numbering
C
C      P1, P2, P3, and P4 may be modified but R1 and R4 are used only
C      for reference.  The principle difficulty is in determining the
C      reference level for P1 and P4.  P2 and P3 are adjusted to make
C      the singleton naked.
C
C      Once P1, P2, P3, and P4 have been adjusted they are copied back
C      to the phase map.
C
C   Notes:
C      1)
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1996/08/06  Code last modified; 1996/08/27
C-----------------------------------------------------------------------
      INTEGER   IX, IY, LX, LY, NPIX, PARITY
      REAL      PHA(NPIX,NPIX), X0, Y0
C
      REAL      C1, C4, P1, P2, P3, P4, R1, R4
      INTEGER   JX, JY, LXY
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C
C     Statement functions to make life easier.
      REAL      CYCLE, MATCH
      CYCLE(P1,P2) = NINT((P1-P2)/360.0)*360.0
      MATCH(P1,P2) = P1 - CYCLE(P1,P2)
C-----------------------------------------------------------------------
      IF (PARITY.EQ.0) RETURN
C
      P1 = FBLANK
      P2 = FBLANK
      P3 = FBLANK
      P4 = FBLANK
      R1 = FBLANK
      R4 = FBLANK
C                                       Rotate so that the
C                                       discontinuity travels towards
C                                       the right.
      IF (LX.GT.0 .AND. ABS(LY).LE.LX) THEN
C                                       N.B. includes (LX,LY) = (+n,+n)
C                                       and (+n,-n).
         JX = NINT(X0 + 0.5)
         JY = NINT(Y0 + 0.5)
         P1 = PHA(JX,JY)
         IF (JY.GT.1) P4 = PHA(JX,JY-1)
         IF (JX.GT.1) THEN
            P2 = PHA(JX-1,JY)
            IF (JY.GT.1) P3 = PHA(JX-1,JY-1)
            END IF
         IF (JX.LT.NPIX) THEN
            R1 = PHA(JX+1,JY)
            IF (JY.GT.1) R4 = PHA(JX+1,JY-1)
            END IF
         LXY = LY/LX
      ELSE IF (LY.GT.0 .AND. ABS(LX).LE.LY) THEN
C                                       N.B. includes (LX,LY) = (-n,+n)
C                                       but not (+n,+n).
         JX = NINT(X0 - 0.5)
         JY = NINT(Y0 + 0.5)
         P1 = PHA(JX,JY)
         IF (JX.LT.NPIX) P4 = PHA(JX+1,JY)
         IF (JY.GT.1) THEN
            P2 = PHA(JX,JY-1)
            IF (JX.LT.NPIX) P3 = PHA(JX+1,JY-1)
            END IF
         IF (JY.LT.NPIX) THEN
            R1 = PHA(JX,JY+1)
            IF (JX.LT.NPIX) R4 = PHA(JX+1,JY+1)
            END IF
         LXY = -LX/LY
      ELSE IF (LX.LT.0 .AND. ABS(LY).LT.ABS(LX)) THEN
C                                       N.B. excludes (LX,LY) = (-n,+n)
C                                       and (-n,-n).
         JX = NINT(X0 - 0.5)
         JY = NINT(Y0 - 0.5)
         P1 = PHA(JX,JY)
         IF (JY.LT.NPIX) P4 = PHA(JX,JY+1)
         IF (JX.LT.NPIX) THEN
            P2 = PHA(JX+1,JY)
            IF (JY.LT.NPIX) P3 = PHA(JX+1,JY+1)
            END IF
         IF (JX.GT.1) THEN
            R1 = PHA(JX-1,JY)
            IF (JY.LT.NPIX) R4 = PHA(JX-1,JY+1)
            END IF
         LXY = LY/LX
      ELSE IF (LY.LT.0 .AND. ABS(LX).LE.ABS(LY)) THEN
C                                       N.B. includes (LX,LY) = (-n,-n)
C                                       but not (+n,-n).
         JX = NINT(X0 + 0.5)
         JY = NINT(Y0 - 0.5)
         P1 = PHA(JX,JY)
         IF (JX.GT.1) P4 = PHA(JX-1,JY)
         IF (JY.LT.NPIX) THEN
            P2 = PHA(JX,JY+1)
            IF (JX.GT.1) P3 = PHA(JX-1,JY+1)
            END IF
         IF (JY.GT.1) THEN
            R1 = PHA(JX,JY-1)
            IF (JX.GT.1) R4 = PHA(JX-1,JY-1)
            END IF
         LXY = -LX/LY
         END IF
C                                       Point the singleton towards the
C                                       desired direction.
      IF (P1.NE.FBLANK .AND. P4.NE.FBLANK) THEN
C                                       The usual case, should we adjust
C                                       P1 or P4?
         IF (PARITY.GT.0) THEN
            IF (PARITY.LE.4) THEN
               IF (LXY.EQ.1) THEN
                  P1 = MATCH(P1,P4) + 360.0
               ELSE
                  P4 = MATCH(P4,P1) - 360.0
                  END IF
            ELSE IF (PARITY.EQ.5) THEN
               P4 = MATCH(P4,P1) - 360.0
            ELSE IF (PARITY.EQ.6) THEN
               P1 = MATCH(P1,P4) + 360.0
               END IF
         ELSE
            IF (PARITY.GE.-4) THEN
               IF (LXY.EQ.1) THEN
                  P4 = MATCH(P4,P1) + 360.0
               ELSE
                  P1 = MATCH(P1,P4) - 360.0
                  END IF
            ELSE IF (PARITY.EQ.-5) THEN
               P1 = MATCH(P1,P4) - 360.0
            ELSE IF (PARITY.EQ.-6) THEN
               P4 = MATCH(P4,P1) + 360.0
               END IF
            END IF
C                                       Uncloak any cloaked singleton
C                                       created on the right.
         IF (R1.NE.FBLANK .AND. R4.NE.FBLANK) THEN
C                                       The usual case, R1 and R4 non-
C                                       blank.
            C1 = CYCLE(P1,R1)
            C4 = CYCLE(P4,R4)
            IF (C1.GT.0.0 .AND. C4.GT.0.0) THEN
               P1 = P1 - MIN(C1,C4)
               P4 = P4 - MIN(C1,C4)
            ELSE IF (C1.LT.0.0 .AND. C4.LT.0.0) THEN
               P1 = P1 + MAX(C1,C4)
               P4 = P4 + MAX(C1,C4)
               END IF
         ELSE IF (R1.EQ.FBLANK .AND. R4.EQ.FBLANK) THEN
C                                       Reconciliation unnecessary.
         ELSE IF (R1.EQ.FBLANK) THEN
            C4 = CYCLE(P4,R4)
            IF (LXY.EQ.-1) C4 = C4 - 360.0
            P1 = P1 - C4
            P4 = P4 - C4
         ELSE IF (R4.EQ.FBLANK) THEN
            C1 = CYCLE(P1,R1)
            IF (LXY.EQ.1) C1 = C1 - 360.0
            P1 = P1 - C1
            P4 = P4 - C1
            END IF
      ELSE IF (P1.EQ.FBLANK .AND. P4.EQ.FBLANK) THEN
C                                       Not much we can do here.
      ELSE IF (P1.EQ.FBLANK) THEN
         IF (P3.NE.FBLANK) THEN
            P4 = MATCH(P4,P3)
         ELSE IF (R4.NE.FBLANK) THEN
            P4 = MATCH(P4,R4)
            IF (LXY.EQ.-1) P4 = P4 - 360.0
            END IF
      ELSE IF (P4.EQ.FBLANK) THEN
         IF (P2.NE.FBLANK) THEN
            P1 = MATCH(P1,P2)
         ELSE IF (R1.NE.FBLANK) THEN
            P1 = MATCH(P1,R1)
            IF (LXY.EQ.1) P1 = P1 - 360.0
            END IF
         END IF
C                                       Uncloak any cloaked singleton
C                                       created by the above.
      IF (P1.NE.FBLANK .AND. P2.NE.FBLANK) P2 = MATCH(P2,P1)
      IF (P3.NE.FBLANK .AND. P4.NE.FBLANK) P3 = MATCH(P3,P4)
      IF (P2.NE.FBLANK .AND. P3.NE.FBLANK) THEN
         IF (P1.EQ.FBLANK) THEN
            P2 = MATCH(P2,P3)
         ELSE IF (P4.EQ.FBLANK) THEN
            P3 = MATCH(P3,P2)
         ELSE
            IF (CYCLE(P2,P3).NE.0.0) THEN
               WRITE (MSGTXT, 10) X0, Y0
 10            FORMAT ('FIXEND: Assertion failed',2F7.1)
               CALL MSGWRT (3)
               END IF
            END IF
         END IF
C                                       De-rotate the map.
      IF (LX.GT.0 .AND. ABS(LY).LE.LX) THEN
         PHA(JX,JY) = P1
         IF (JY.GT.1) PHA(JX,JY-1) = P4
         IF (JX.GT.1) THEN
            PHA(JX-1,JY) = P2
            IF (JY.GT.1) PHA(JX-1,JY-1) = P3
            END IF
         IF (PARITY.LT.0) THEN
            IX = NINT(X0 + 0.5)
            IY = NINT(Y0 + 0.5)
            PARITY = -1
         ELSE
            IX = NINT(X0 + 0.5)
            IY = NINT(Y0 - 0.5)
            PARITY = +1
            END IF
      ELSE IF (LY.GT.0 .AND. ABS(LX).LE.LY) THEN
         PHA(JX,JY) = P1
         IF (JX.LT.NPIX) PHA(JX+1,JY) = P4
         IF (JY.GT.1) THEN
            PHA(JX,JY-1) = P2
            IF (JX.LT.NPIX) PHA(JX+1,JY-1) = P3
            END IF
         IF (PARITY.LT.0) THEN
            IX = NINT(X0 - 0.5)
            IY = NINT(Y0 + 0.5)
            PARITY = -2
         ELSE
            IX = NINT(X0 + 0.5)
            IY = NINT(Y0 + 0.5)
            PARITY = +2
            END IF
      ELSE IF (LX.LT.0 .AND. ABS(LY).LT.ABS(LX)) THEN
         PHA(JX,JY) = P1
         IF (JY.LT.NPIX) PHA(JX,JY+1) = P4
         IF (JX.LT.NPIX) THEN
            PHA(JX+1,JY) = P2
            IF (JY.LT.NPIX) PHA(JX+1,JY+1) = P3
            END IF
         IF (PARITY.LT.0) THEN
            IX = NINT(X0 - 0.5)
            IY = NINT(Y0 - 0.5)
            PARITY = -3
         ELSE
            IX = NINT(X0 - 0.5)
            IY = NINT(Y0 + 0.5)
            PARITY = +3
            END IF
      ELSE IF (LY.LT.0 .AND. ABS(LX).LE.ABS(LY)) THEN
         PHA(JX,JY) = P1
         IF (JX.GT.1) PHA(JX-1,JY) = P4
         IF (JY.LT.NPIX) THEN
            PHA(JX,JY+1) = P2
            IF (JX.GT.1) PHA(JX-1,JY+1) = P3
            END IF
         IF (PARITY.LT.0) THEN
            IX = NINT(X0 + 0.5)
            IY = NINT(Y0 - 0.5)
            PARITY = -4
         ELSE
            IX = NINT(X0 - 0.5)
            IY = NINT(Y0 - 0.5)
            PARITY = +4
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE FLATPH (TYPE, XMAG, NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *   CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXYZ, NOZERO, PHAMOD)
C-----------------------------------------------------------------------
C   FLATPH corrects the phase of the aperture voltage for pointing,
C   focus, and feed offset errors using least squares.
C
C   Given:
C      TYPE     C*4      Focus type
C                           PFOC: prime focus
C                           SUBR: subreflector
C      XMAG     R        Magnification factor for Cassegrain feeds.
C      NPIX     I        Number of pixels on a side of the map.
C      LAMBDA   R        Observing wavelength, in metres.
C      FOCUS    R        Nominal focal length, in metres.
C      XYMIN    R        Range of |x| and |y| used in correcting for
C      XYMAX    R        pointing, focus, and feed offset. Negative
C                        values denote a range of SQRT(x*x + y*y).
C      CELLXY   R        Map cell spacing, in metres.
C      VAMP     R(NPIX,NPIX)
C                        Aperture voltage amplitude map.
C      VPHA     R(NPIX,NPIX)
C                        Aperture voltage phase map.
C      NOXYZ    L        Inhibit focus and feed offset
C
C   Returned:
C      P0       R        Constant offset removed, degrees.
C      PX       R        Least squares estimates of the phase ramp
C      PY       R        in the X and Y directions, in degrees/cell
C      FX       R        The feed offset, derived focal position is at
C      FY       R        (-FX,-FY,FOCUS-FZ), millimetres.
C      FZ       R
C      DP0      R        Standard error in P0.
C      DPX      R        Standard error in PX.
C      DPY      R        Standard error in PY.
C      DFX      R        Standard error in FX.
C      DFY      R        Standard error in FY.
C      DFZ      R        Standard error in FZ.
C      RMS      R        Weighted Half-path rms error, in millimetres.
C      RMS0     R        Pre-fit weighted half-path error, mm.
C      IERR     I        Error status, 0 means success.
C
C   Algorithm:
C      Weighted least squares fit to the formulation given by the
C      unpublished work by John Ruze, "Small Displacements in Parabolic
C      Reflectors", 1969, MIT.
C
C   Notes:
C      1)  Focus and feed offset inhibited if NOXYZ = .true.
C      2)  Phase offset term inhibited if NOZERO = .true.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/11     Code last modified; 1996/07/08
C-----------------------------------------------------------------------
      LOGICAL   NOXYZ, NOZERO
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *          VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *          DFX, DFY, DFZ, RMS0, RMS, PHAMOD(NPIX,NPIX), XMAG
      CHARACTER TYPE*4
C
      INTEGER   NP
      PARAMETER (NP=6)
      INTEGER   I, IDR2, IDX, IDY, IX, IX0, IR2MAX, IR2MIN, IXYMAX,
     *          IXYMIN, IY, IY0, J
      REAL      CORR, D(NP,NP), FIT, FP, MEAN, NS, PH, Q, QP, R(NP), R4,
     *          RP, RR, S, SP, SSQ, SSQRES, SUM, VARRES, VARY, VX(NP),
     *          WT, X(NP), XF, XP, YF, YP, ZF
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (TYPE.EQ.'SUBR') THEN
         IF (XMAG.LE.0.0) XMAG = 10.0
         END IF
C                                       Initialize.
      IXYMIN = ABS(XYMIN/CELLXY)
      IXYMAX = ABS(XYMAX/CELLXY)
      IR2MIN = (XYMIN*XYMIN)/(CELLXY*CELLXY)
      IR2MAX = (XYMAX*XYMAX)/(CELLXY*CELLXY)
C                                       Focal length in cellular units.
      FP = FOCUS/CELLXY
C                                       Half-path wavelength scaling
      R4 = LAMBDA/720.0
      NS  = 0.0
      SUM = 0.0
      SSQ = 0.0
      DO 30 I = 1,NP
         R(I) = 0.0
         DO 20 J = 1,NP
            D(I,J) = 0.0
 20         CONTINUE
 30      CONTINUE
      RR = NPIX/2.0 + 1.0
C                                       Compute pre-fit rms error
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS0)
C                                       Loop through the map.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 50 IY = 1, NPIX
         IDY = ABS (IY - IY0)
C                                       Check absolute limits.
         IF (XYMIN.GT.0.0 .AND. IDY.LT.IXYMIN) GO TO 50
         IF (XYMAX.GT.0.0 .AND. IDY.GT.IXYMAX) GO TO 50
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF (XYMAX.LT.0.0 .AND. IDY*IDY.GT.IR2MAX) GO TO 50
         DO 40 IX = 1,NPIX
C                                       Ignore blanked pixels.
            IF (VPHA(IX,IY).EQ.FBLANK) GO TO 40
C                                       Check for inclusion.
            IDX  = ABS(IX-IX0)
            IDR2 = IDX*IDX + IDY*IDY
C                                       Inner limits.
            IF (XYMIN.GT.0.0) THEN
               IF (IDX.LT.IXYMIN) GO TO 40
            ELSE IF (XYMIN.LT.0.0) THEN
               IF (IDR2.LT.IR2MIN) GO TO 40
               END IF
C                                       Outer limits.
            IF (XYMAX.GT.0.0) THEN
               IF (IDX.GT.IXYMAX) GO TO 40
            ELSE IF (XYMAX.LT.0.0) THEN
               IF (IDR2.GT.IR2MAX) GO TO 40
               END IF
C                                       Evaluate variables (in cellular
C                                       units).
            PH = VPHA(IX,IY)
            WT = VAMP(IX,IY)
            XP = IX - IX0
            YP = IY - IY0
            RP = SQRT(XP*XP + YP*YP)
            Q = RP/(2.0*FP)
            S = 1.0/(1.0 + Q*Q)
            IF (TYPE.EQ.'PFOC') THEN
               XF = 2.0*(XP/RP)*Q*S
               YF = 2.0*(YP/RP)*Q*S
               ZF = -(1.0 - Q*Q)*S
            ELSE
               QP = Q/XMAG
               SP = 1.0/(1.0 + QP*QP)
               XF = 2.0*(XP/RP)*(Q*S - QP*SP)
               YF = 2.0*(YP/RP)*(Q*S - QP*QP)
               ZF = -(1.0 - Q*Q)*S - (1.0 - QP*QP)*SP
               END IF
C                                       Accumulate statistics.
            NS     = NS  + WT
            SUM    = SUM + PH*WT
            SSQ    = SSQ + PH*PH*WT
            R(1)   = R(1) + PH*WT
            R(2)   = R(2) + PH*XP*WT
            R(3)   = R(3) + PH*YP*WT
            R(4)   = R(4) + PH*XF*WT
            R(5)   = R(5) + PH*YF*WT
            R(6)   = R(6) + PH*ZF*WT
            D(1,1) = D(1,1) + WT
            D(1,2) = D(1,2) + XP*WT
            D(1,3) = D(1,3) + YP*WT
            D(1,4) = D(1,4) + XF*WT
            D(1,5) = D(1,5) + YF*WT
            D(1,6) = D(1,6) + ZF*WT
            D(2,2) = D(2,2) + XP*XP*WT
            D(2,3) = D(2,3) + XP*YP*WT
            D(2,4) = D(2,4) + XP*XF*WT
            D(2,5) = D(2,5) + XP*YF*WT
            D(2,6) = D(2,6) + XP*ZF*WT
            D(3,3) = D(3,3) + YP*YP*WT
            D(3,4) = D(3,4) + YP*XF*WT
            D(3,5) = D(3,5) + YP*YF*WT
            D(3,6) = D(3,6) + YP*ZF*WT
            D(4,4) = D(4,4) + XF*XF*WT
            D(4,5) = D(4,5) + XF*YF*WT
            D(4,6) = D(4,6) + XF*ZF*WT
            D(5,5) = D(5,5) + YF*YF*WT
            D(5,6) = D(5,6) + YF*ZF*WT
            D(6,6) = D(6,6) + ZF*ZF*WT
 40         CONTINUE
 50      CONTINUE
C                                       Disable the focus and feed
C                                       offset if requested
      IF (NOXYZ) THEN
        DO 70 I = 4, 6
           R(I) = 0.0
           DO 60 J = 1,NP
              D(J,I) = 0.0
              D(I,J) = 0.0
 60           CONTINUE
 70         CONTINUE
         END IF
C                                       Disable the zero offset term,
C                                       if requested
      IF (NOZERO) THEN
         R(1) = 0.0
         DO 80 J = 1,NP
            D(1,J) = 0.0
            D(J,1) = 0.0
 80         CONTINUE
         END IF
C                                       Compute least squares solution.
      CALL LEASQR (NP, NS, SUM, SSQ, R, D, X, VX, SSQRES, VARRES, VARY,
     *   FIT, IERR)
C                                       Pick up answers
      P0 = X(1)
      PX = X(2)
      PY = X(3)
      FX = X(4)
      FY = X(5)
      FZ = X(6)
      DP0 = SQRT(VX(1))
      DPX = SQRT(VX(2))
      DPY = SQRT(VX(3))
      DFX = SQRT(VX(4))
      DFY = SQRT(VX(5))
      DFZ = SQRT(VX(6))
C                                       Apply the correction.
      DO 100 IY = 1,NPIX
         DO 90 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RP = SQRT(XP*XP + YP*YP)
               Q = RP/(2.0*FP)
               S = 1.0/(1.0 + Q*Q)
               IF (TYPE.EQ.'PFOC') THEN
                  XF = 2.0*(XP/RP)*Q*S
                  YF = 2.0*(YP/RP)*Q*S
                  ZF = -(1.0 - Q*Q)*S
               ELSE
                  QP = Q/XMAG
                  SP = 1.0/(1.0 + QP*QP)
                  XF = 2.0*(XP/RP)*(Q*S - QP*SP)
                  YF = 2.0*(YP/RP)*(Q*S - QP*SP)
                  ZF = -(1.0 - Q*Q)*S - (1.0 - QP*QP)*SP
                  END IF
               CORR = P0 + PX*XP + PY*YP + FX*XF + FY*YF + FZ*ZF
               VPHA(IX,IY) = VPHA(IX,IY) - CORR
               PHAMOD(IX,IY) = CORR
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Rescale feed offsets to mm.
      FX  = 1000.0*LAMBDA*FX/360.0
      FY  = 1000.0*LAMBDA*FY/360.0
      FZ  = 1000.0*LAMBDA*FZ/360.0
      DFX = 1000.0*LAMBDA*DFX/360.0
      DFY = 1000.0*LAMBDA*DFY/360.0
      DFZ = 1000.0*LAMBDA*DFZ/360.0
C                                       Compute the weighted half-path
C                                       error (as RMS)
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN,
     *  IR2MAX, VAMP, VPHA, R4, MEAN, RMS)
C-----------------------------------------------------------------------
      INTEGER   IDR2, IDX, IDY, IR2MAX, IR2MIN, IX, IX0, IXYMAX, IXYMIN,
     *          IY, IY0, NPIX
      REAL      MEAN, R4, RMS, VAMP(NPIX,NPIX), VPHA(NPIX,NPIX), WT,
     *          XYMAX, XYMIN
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Compute the weighted half-path
C                                       error (as RMS)
C                                       Loop through the map.
      MEAN = 0.0
      RMS = 0.0
      WT  = 0.0
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 140 IY = 1,NPIX
         IDY = ABS(IY-IY0)
C                                       Check absolute limits.
         IF (XYMIN.GT.0.0 .AND. IDY.LT.IXYMIN) GO TO 140
         IF (XYMAX.GT.0.0 .AND. IDY.GT.IXYMAX) GO TO 140
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF (XYMAX.LT.0.0 .AND. IDY*IDY.GT.IR2MAX) GO TO 140
         DO 130 IX = 1,NPIX
            IF (VPHA(IX,IY).EQ.FBLANK) GO TO 130
C                                       Check for inclusion.
            IDX  = ABS(IX-IX0)
            IDR2 = IDX*IDX + IDY*IDY
C                                       Inner limits.
            IF (XYMIN.GT.0.0) THEN
               IF (IDX.LT.IXYMIN) GO TO 130
            ELSE IF (XYMIN.LT.0.0) THEN
               IF (IDR2.LT.IR2MIN) GO TO 130
               END IF
C                                       Outer limits.
            IF (XYMAX.GT.0.0) THEN
               IF (IDX.GT.IXYMAX) GO TO 130
            ELSE IF (XYMAX.LT.0.0) THEN
               IF (IDR2.GT.IR2MAX) GO TO 130
               END IF
            MEAN = MEAN + VAMP(IX,IY)*VPHA(IX,IY)*R4
            RMS = RMS + VAMP(IX,IY)*((VPHA(IX,IY)*R4)**2)
            WT  = WT  + VAMP(IX,IY)
 130        CONTINUE
 140     CONTINUE
C
      IF (WT.GT.0) THEN
         MEAN = MEAN/WT
         RMS = RMS/WT - MEAN*MEAN
         IF (RMS.GT.0) RMS = SQRT(RMS)
C                                               Convert to mm.
         MEAN = 1000.0*MEAN
         RMS  = 1000.0*RMS
      ELSE
         RMS = -99.0
         END IF
C
      RETURN
      END
      SUBROUTINE SURDEV (CELLXY, LAMBDA, FOCUS, NPIX, VPHA, DEV)
C-----------------------------------------------------------------------
C   SURDEV computes the surface deviation map from the phase of the
C   aperture voltage distribution function.
C
C   Given:
C      CELLXY   R        Map cell spacing, in metres.
C      LAMBDA   R        Observing wavelength, in metres.
C      FOCUS    R        Focal length, in metres.
C      NPIX     I        Number of pixels on a side of the map.
C      VPHA     R(NPIX,NPIX)
C                        Aperture voltage phase map.
C
C   Returned:
C      DEV      R(NPIX,NPIX)
C                        Surface deviation map.
C
C   Algorithm:
C      The pathlength seen by an incoming signal is phase shifted on
C      both incidence and reflection from the antenna surface. Moreover,
C      away from the collimation axis of the antenna, the path deviates
C      from the normal to the antenna surface, this being the direction
C      in which the displacement is required.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Apr/11  Code last modified; 1992/Nov/18
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      CELLXY, LAMBDA, FOCUS, VPHA(NPIX,NPIX), DEV(NPIX,NPIX)
C
      INTEGER   IX, IX0, IY, IY0
      REAL      A, B, C, R
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialization.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      A = FOCUS/CELLXY
      B = (LAMBDA/360.0)/(4.0*A)
      C = 4.0*A*A
C                                       Loop over the map.
      DO 20 IY = 1,NPIX
         DO 10 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               R = SQRT(REAL((IX-IX0)**2 + (IY-IY0)**2))
               DEV(IX,IY) = B*VPHA(IX,IY)*SQRT(R*R + C)
            ELSE
               DEV(IX,IY) = FBLANK
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *   DATMAP, FREQ, UNITS, AXIS1, NX1, PINC1, AXIS2, NX2, PINC2,
     *   RDAT, HISTRY, NHIST, MVIS, IERR)
C-----------------------------------------------------------------------
C   HOLOUT saves the maps as AIPS image files with history as well.
C
C   Given:
C      OUTNAM   C*36      WAWA image namestring.
C      OBJECT   C*8       Object name.
C      TELESC   C*8       Telescope name.
C      INSTRM   C*8       Instrument name (receiver, correlator).
C      OBSERV   C*8       Observers.
C      DATOBS   C*8       Observation date.
C      DATMAP   C*8       Map date.
C      UNITS    C*8       Map units.
C      AXIS1    C*8       Title for axis 1.
C      NX1      I         Number of pixels on axis 1.
C      PINC1    R         Pixel increment on axis 1.
C      AXIS2    C*8       Title for axis 2.
C      NX2      I         Number of pixels on axis 2.
C      PINC2    R         Pixel increment on axis 2.
C      RDAT     R(NX1,NX2)
C                         Array containing the image.
C      HISTRY   C*72(*)   Internal file containing history text.
C      NHIST    I         Number of records in HISTRY.
C
C      MVIS     I         Number of vis used
C   Returned:
C      IERR     I         Error status, 0 means success.
C
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1992/Mar/12
C-----------------------------------------------------------------------
      CHARACTER OUTNAM*36, OBJECT*8, TELESC*8, INSTRM*8, OBSERV*8,
     *   DATOBS*8, DATMAP*8, UNITS*8, AXIS1*8, AXIS2*8, HISTRY(*)*(*)
      INTEGER   NX1, NX2, NHIST, MVIS, IERR
      REAL      FREQ, PINC1, PINC2, RDAT(NX1,NX2)
C
      INTEGER   BUFF(256), CNO, HILUN, I1, I2, IRET, ISEQ, IUSER, IVOL,
     *   NH, NHISTF, OLUN, NUMKEY, LOCS, KEYTYP, BUFF2(256)
      REAL      VMAX, VMIN
      CHARACTER CLASS*6, NAME*12, PTYPE*2, KEYWRD*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA OLUN, HILUN, NHISTF /18, 27, 1/
C-----------------------------------------------------------------------
C                                       Fill in the catalog header
      CALL CHR2H (8, OBJECT, 1, CATH(KHOBJ))
      CALL CHR2H (8, TELESC, 1, CATH(KHTEL))
      CALL CHR2H (8, INSTRM, 1, CATH(KHINS))
      CALL CHR2H (8, OBSERV, 1, CATH(KHOBS))
      CALL CHR2H (8, DATOBS, 1, CATH(KHDOB))
      CALL CHR2H (8, DATMAP, 1, CATH(KHDMP))
      CALL CHR2H (8, UNITS,  1, CATH(KHBUN))
      CATR(KREPO) = 0.0
      CATR(KRBLK) = FBLANK
      CATBLK(KIPCN) = 0
      CATBLK(KIDIM) = 3
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       First axis parameters.
      CALL CHR2H (8, AXIS1, 1, CATH(KHCTP))
      CATD(KDCRV) = 0.0D0
      CATR(KRCIC) = PINC1
      IF (PINC1.GE.0.0) THEN
         CATR(KRCRP) = NX1/2 + 1
      ELSE
         CATR(KRCRP) = NX1/2
         END IF
      CATBLK(KINAX) = NX1
C                                       Second axis parameters.
      CALL CHR2H (8, AXIS2, 1, CATH(KHCTP+2))
      CATD(KDCRV+1) = 0D0
      CATR(KRCIC+1) = PINC2
      CATR(KRCRP+1) = NX2/2 + 1
      CATBLK(KINAX+1) = NX2
C                                       Third axis parameters.
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
      CATD(KDCRV+2) = FREQ * 1.D9
      CATR(KRCIC+2) = 0.0
      CATR(KRCRP+2) = 1.0
      CATBLK(KINAX+2) = 1
C                                       Create and open the map file.
      CALL MAPCR (OUTNAM, OUTNAM, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING MAP FILE'
         GO TO 990
         END IF
C                                       Compute the visibility coordinat
C                                       Open the map file.
      CALL OPENCF (OLUN, OUTNAM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING MAP FILE'
         GO TO 990
         END IF
C                                       Extract the data from the array
C                                       and write the AIPS map.
      VMIN = +1.0E38
      VMAX = -1.0E38
      DO 50 I2 = 1, NX2
C                                       Find the maximum and minimum.
         DO 30 I1 = 1, NX1
            IF (RDAT(I1,I2).NE.FBLANK) THEN
               IF (RDAT(I1,I2).LT.VMIN) VMIN = RDAT(I1,I2)
               IF (RDAT(I1,I2).GT.VMAX) VMAX = RDAT(I1,I2)
               END IF
 30         CONTINUE
C                                       Write it to the AIPS file.
         CALL MAPIO ('WRIT', OLUN, RDAT(1,I2), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR, I2
            GO TO 990
            END IF
 50      CONTINUE
C                                       Update the map header.
      CATR(KRDMX) = VMAX
      CATR(KRDMN) = VMIN
      CALL SAVHDR (OLUN, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'UPDATING THE CATALOG HEADER'
         GO TO 990
         END IF
C                                       Close the output file.
      CALL FILCLS (OLUN)
C                                       Write the history file.
      CNO = FILTAB(POCAT, 6)
      CALL HIINIT (NHISTF)
      CALL WAWA2A (OUTNAM, NAME, CLASS, ISEQ, PTYPE, IVOL, IUSER)
C                                       Add output name.
      CALL HICREA (HILUN, IVOL, CNO, CATBLK, BUFF, IERR)
      CALL HENCOO (TSKNAM, NAME, CLASS, ISEQ, IVOL, HILUN, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'ADDING IMAGE NAME TO HISTORY'
         GO TO 990
         END IF
C                                       Add history text.
      DO 100 NH = 1, NHIST
         CALL HIADD (HILUN, HISTRY(NH), BUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING TO HISTORY FILE'
            GO TO 990
            END IF
 100     CONTINUE
C                                       add keyword for PANEL
      IF (MVIS.GT.0) THEN
         KEYWRD = 'VisUsed'
         NUMKEY = 1
         LOCS = 1
         KEYTYP = 4
         CALL CATKEY ('WRIT', IVOL, CNO, KEYWRD, NUMKEY, LOCS, MVIS,
     *      KEYTYP, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING VisUsed KEYWORD'
            CALL MSGWRT (7)
            END IF
         END IF
C                                       Normal end.
      IRET = 0
      GO TO 995
C                                       Error exit
 990  IRET = 1
      CALL MSGWRT (8)
      CALL FILCLS (OLUN)
C
 995  CALL HICLOS (HILUN, .TRUE., BUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HOLOUT: ERROR',I3,' ON ', A)
 1040 FORMAT ('HOLOUT: ERROR',I3,' WRITING LINE NUMBER',I6)
      END
      SUBROUTINE BEAM (NPIX, NLG, CMPRS, VAMP, VPHA, AAMP, APHA, VCPLX,
     *   WAMP, WPHA)
C-----------------------------------------------------------------------
C   BEAM evaluates the antenna power pattern.
C
C   Given:
C      NPIX    I        Number of pixels on a side of the map.
C      NLG     I        Number of pixels on a side of the beam fields
C      CMPRS   I        Select every CMPRS cells from VAMP,VPHA
C      VAMP    R(NPIX,NPIX)
C                       The amplitude of the voltage distribution across
C                       the antenna aperture.
C      VPHA    R(NPIX,NPIX)
C                       The phase of the voltage distribution across
C                       the antenna aperture.
C
C   Returned:
C      AAMP    R(NPIX,NPIX)
C                       The amplitude of the antenna pattern.
C      APHA    R(NPIX,NPIX)
C                       The phase of the antenna pattern.
C      VCPLX   C(NLG,NLG)
C                       Complex array for beam computation
C      WAMP    R(NLG,NLG)
C                       Real array for beam amplitude
C      WPHA    R(NLG,NLG)
C                       Real array for beam phase
C
C   Called:
C      {HOLFFT}
C
C   Algorithm:
C      Interpolation is done by compressing the voltage distribution map
C      and imbedding it in a larger array.  This is effectively a
C      multiplication by a pillbox function, and corresponds to a
C      convolution by a sinc function in the Fourier transform domain.
C
C   Author: Mike Kesteven, Australia Telescope.
C   Origin; 1987/12      Code last modified; 1996/07/09
C-----------------------------------------------------------------------
      INTEGER   NPIX, NLG, CMPRS
      REAL      VAMP(NPIX,NPIX), VPHA(NPIX,NPIX), AAMP(NPIX,NPIX),
     *          APHA(NPIX,NPIX), WAMP(NLG,NLG), WPHA(NLG,NLG)
      COMPLEX   VCPLX(NLG,NLG)
C
      INTEGER   IX, IY, JX, JY, K
      REAL      CS, SHFTX, SHFTY, SN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
      DO 20 JY = 1, NLG
         DO 10 JX = 1, NLG
            VCPLX(JX,JY) = (0.0,0.0)
 10         CONTINUE
 20      CONTINUE
C                                       Compress and imbed the NPIXxNPIX
C                                       map in a NLGxNLG array.
      K = NLG - NPIX/(2*CMPRS)
      JY = K
      DO 40 IY = 1, NPIX, CMPRS
         JY = JY + 1
         IF (JY.GT.NLG) JY = JY - NLG
         JX = K
         DO 30 IX = 1, NPIX, CMPRS
            JX = JX + 1
            IF (JX.GT.NLG) JX = JX - NLG
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               CS = VAMP(IX,IY)*COS(VPHA(IX,IY)*DG2RAD)
               SN = VAMP(IX,IY)*SIN(VPHA(IX,IY)*DG2RAD)
               VCPLX(JX,JY) = CMPLX(CS,SN)
               END IF
 30         CONTINUE
 40      CONTINUE
C                                       Apply a phase gradient to centre
C                                       the beam.
      SHFTY = 1.0
      DO 60 JY = 1, NLG
         SHFTX = SHFTY
         DO 50 JX = 1, NLG
            VCPLX(JX,JY) = SHFTX*VCPLX(JX,JY)
            SHFTX = -SHFTX
 50         CONTINUE
         SHFTY = -SHFTY
 60      CONTINUE
C                                       Transform.
      CALL HOLFFT (1, NLG, VCPLX, WAMP, WPHA)
C                                       Extract the beam, convert to dB
      K = (NLG - NPIX)/2
      DO 80 IY = 1, NPIX
         JY = K + IY
         DO 70 IX = NPIX, 1, -1
            JX = K + IX
            AAMP(IX,IY) = WAMP(JX,JY)
            APHA(IX,IY) = WPHA(JX,JY)
            IF (AAMP(IX,IY).LE.0.0) THEN
               AAMP(IX,IY) = FBLANK
            ELSE
               AAMP(IX,IY) = 20.0*LOG10(AAMP(IX,IY))
               END IF
 70         CONTINUE
 80      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ANGAIN (NPIX, MAPSZ, RMAX, RMIN, LAMBDA, VAMP, VPHA,
     *   F1, MGAIN, TGAIN)
C-----------------------------------------------------------------------
C   ANGAIN computes the gain of an antenna given the aperture voltage
C   distribution.
C
C   Given:
C      NPIX     I        Number of pixels on a side of the map.
C      MAPSZ    R      Size of the map, in metres.
C      RMAX     R        The antenna radius, in cell units.
C      RMIN     R        Sub-reflector radius, in cell units.
C      LAMBDA   R        Wavelength of the observation, in metres.
C      VAMP     R(NPIX,NPIX)
C                        Amplitude of the voltage distribution across
C                        the antenna aperture.
C      VPHA     R(NPIX,NPIX)
C                        Phase of the voltage distribution across the
C                        antenna aperture.
C      F1       R        Scaling factor used to extrapolate in freq
C
C   Returned:
C      MGAIN    R        Measured gain, in dB.
C      TGAIN    R        Theoretical gain, in dB.
C
C   Author: Mike Kesteven, Australia Telescope.
C   Origin; 1987/11/11  Code last modified; 1996/08/20
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      F1, LAMBDA, MAPSZ, MGAIN, RMAX, RMIN, TGAIN,
     *          VAMP(NPIX,NPIX), VPHA(NPIX,NPIX)
C
      INTEGER   IX, IX0, IY, IY0, R
      REAL      AMAX, CELLSZ, CS1, CS2, FACT, SN1, SN2
      COMPLEX   INT1, INT2, INT3, INT4, TMP1, TMP2
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      CELLSZ = MAPSZ/NPIX
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      INT1 = (0.0, 0.0)
      INT2 = (0.0, 0.0)
      INT3 = (0.0, 0.0)
      INT4 = (0.0, 0.0)
C                                       Integrate.
      DO 20 IY = 1, NPIX
         DO 10 IX = 1, NPIX
            R = SQRT(REAL((IX-IX0)**2) + REAL((IY-IY0)**2))
            IF (R.LE.RMAX .AND. R.GE.RMIN) THEN
               CS1 = VAMP(IX,IY)*COS(F1*VPHA(IX,IY)*DG2RAD)
               SN1 = VAMP(IX,IY)*SIN(F1*VPHA(IX,IY)*DG2RAD)
               CS2 = 1.0
               SN2 = 0.0
               TMP1 = CMPLX(CS1, SN1)
               TMP2 = CMPLX(CS2, SN2)
               INT1 = INT1 + TMP1
               INT2 = INT2 + TMP1*CONJG(TMP1)
               INT3 = INT3 + TMP2
               INT4 = INT4 + TMP2*CONJG(TMP2)
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       Normalize.
      FACT  = CELLSZ/LAMBDA
      AMAX  = INT1*CONJG(INT1)/INT2
      AMAX  = AMAX*FACT*FACT
      MGAIN = AMAX*4.0*PI
      MGAIN = 10.0*LOG10(MGAIN)
      AMAX  = INT3*CONJG(INT3)/INT4
      AMAX  = AMAX*FACT*FACT
      TGAIN = AMAX*4.0*PI
      TGAIN = 10.0*LOG10(TGAIN)
C
 999  RETURN
      END
