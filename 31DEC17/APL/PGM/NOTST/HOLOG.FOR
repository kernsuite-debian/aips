LOCAL INCLUDE 'HOLOGP'
      INTEGER   MX, MXFAC, NLG, MXEQ
C                                       Maximum map size
      PARAMETER (MX = 512)
      PARAMETER (MXFAC = 2)
C                                       Beam double grid
      PARAMETER (NLG = MXFAC * MX)
      PARAMETER (MXEQ = MX / MXFAC)
LOCAL END
LOCAL INCLUDE 'HOLOGMX'
      INCLUDE 'HOLOGP'
C                                       Try to save core
      REAL      WAMP(NLG,NLG), WPHA(NLG,NLG)
      COMPLEX   VCPLX(NLG,NLG)
      REAL      AAMP(MX,MX), APHA(MX,MX), PAMP(MX,MX), VAMP(MX,MX),
     *   VDEV(MX,MX), VPHA(MX,MX), WGT(MX,MX), PPHA(MX,MX),
     *   UNIF(MX,MX), PHAMOD(MX,MX), PV(MX,MX), ARE(MX,MX), AIM(MX,MX)
      COMPLEX   ACPLX(MX,MX), SCPLX(MX,MX)
      COMMON /BUFRS/ WAMP, WPHA, VCPLX, PHAMOD, PV, AAMP, APHA, ARE,
     *   AIM, PAMP,VAMP, VDEV, VPHA, WGT, PPHA, UNIF, ACPLX, SCPLX
C                                       Depends on the factor 2 above
C                                       VAMP/VPHA must NOT equiv VCPLX
C     EQUIVALENCE (VAMP, WAMP(1,1))
C     EQUIVALENCE (VPHA, WAMP(1,1+MXEQ))
C     EQUIVALENCE (PAMP, WAMP(1,1+2*MXEQ))
C     EQUIVALENCE (PPHA, WAMP(1,1+3*MXEQ))
C     EQUIVALENCE (VDEV, WPHA(1,1))
C     EQUIVALENCE (WGT,  WPHA(1,1+1*MXEQ))
C     EQUIVALENCE (UNIF, WPHA(1,1+2*MXEQ))
C     EQUIVALENCE (ACPLX, VCPLX(1,1))
C     EQUIVALENCE (SCPLX, VCPLX(1,1+MXEQ))
C                                       wastes a little space
C                                       AAMP/APHA must equiv VCPLX
C     EQUIVALENCE (AAMP,  VCPLX(1,1+2*MXEQ))
C     EQUIVALENCE (APHA,  VCPLX(1,1+3*MXEQ))
LOCAL END
LOCAL INCLUDE 'HOLOG'
C                                       Local include for HOLOG.
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'HOLOGMX'
      LOGICAL   DODFT, DOMOD, LINEAR, OPT(10), NOXY, NOFOC, UNIWT,
     *   ZIPV, NOTILT, NOPNT
      INTEGER   LUNVIS, NPIX, INDVIS, ISTOKE
      HOLLERITH XINFIL(12), XOUTNA(3), XOPTYP
      REAL      XOUTDI, TAPER(2), XMAG, REFREQ, FREQ, EL0, DIAM, SUBDIA,
     *   FOCUS, U0, V0, W0, XANT, XSTOK, MAPSZ, XNPIX, UVMIN, UVMAX,
     *   AMPSCL, XFT, XYMIN, XYMAX, XCNTRL, XLINEA, UPRM(5), VPRM(5),
     *   DOVECT, XOPT(10), LAMBDA, PHSGN, XNORM, VPARM(30)
      REAL      HFREQ
      CHARACTER INFILE*48, OUTNAM*36, OPTYPE*4, DATOBS*8, TELESC*8
      COMMON /INPARM/ XINFIL, XOUTNA, XOUTDI, TAPER, XOPTYP, XMAG,
     *   REFREQ, FREQ, EL0, DIAM, SUBDIA, FOCUS, U0, V0, W0, XANT,
     *   XSTOK, MAPSZ, XNPIX, UVMIN, UVMAX, AMPSCL, XFT, XYMIN, XYMAX,
     *   XCNTRL, XLINEA, UPRM, VPRM, XNORM, DOVECT, XOPT, VPARM
      COMMON /INTLOG/ NPIX, PHSGN, DODFT, DOMOD, ZIPV, NOTILT, HFREQ,
     *   LINEAR, OPT, LAMBDA, LUNVIS, NOXY, NOFOC, UNIWT, INDVIS, NOPNT,
     *   ISTOKE
      COMMON /INCHAR/ INFILE, OUTNAM, OPTYPE, DATOBS, TELESC
LOCAL END
      PROGRAM HOLOG
C-----------------------------------------------------------------------
C! Read and process holography visibility data
C# UV ANALYSIS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2007, 2015-2017
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
C   Rick Perley version of HOLGR
C   Adverbs:
C      INFILE(12)  Input visibility file name.
C      OUTNAME(3)  Output image name.
C      OUTDISK     Output disk number.
C      UVTAPER     Taper to apply in gridding
C      OPTYPE      The feed model (prime focus or subreflector)
C      FACTOR      The magnification (for subreflector model)
C      APARM(10)   Operating parameters.
C                      1: Observing frequency (GHz).
C                      2: Satellite elevation, degrees.
C                      3: Antenna diameter, in meters.
C                      4: Subreflector diameter, in meters.
C                      5: Focal length, in meters.
C                      6: Inactive
C                      7: Inactive
C                      8: Inactive
C                      9: 10000*refant+100*scanant+IF#
C                     10: Stokes (1-4; RR,LL,RL,LR)
C  mjk (4/feb/93) : aparm(9 & 10) are used in generating the input
C  and output filenames if triggered by INFILE = 'AREA:'.
C  RAP (29/mar/02): APARM(3 & 4) are used to blank the output V_PHASE
C  MODEL, and V_DEV maps. Blanked areas are not used in focus/subr. fits.
C
C  In the following, 'l' and 'm' refer to angular variables
C  (direction sines w.r.t. pointing direction).  'x' and 'y' are the
C  antenna surface spatial coordinates, in meters. In the code,
C  Mark regrettably chose to use 'u' and 'v' for 'l' and 'm'.
C  BPARM(3,4) control the resolution of the derived antenna surface maps.
C  BPARM(7,8) control the antenna surface which is used to fit the prime
C  focus/subreflector phase models.
C
C      BPARM(10)   Data reduction parameters.
C                      1: Required map size, in meters.
C                      2: Number of pixels on a side of the output
C                         map (power of 2, min. 32, max. 512).
C                    3,4: Range of |l| and |m| to include.  Negative
C                         values denote (min, max) of SQRT(l*l + m*m).
C                         Units are inverse sine of the angles.
C                      5: Amplitude scaling factor (0 -> 1).
C                      6: Fourier transform control.  If negative,
C                         the phase read from the data file is
C                         negated.  If the absolute value is 2, a
C                         direct (slow) Fourier transform will be
C                         done, otherwise, an FFT.
C                    7,8: Range of |x| and |y| used in correcting
C                         for pointing, focus, and feed offset.
C                         Negative values denote (min, max) of
C                         SQRT(x*x + y*y).  Units in meters.
C                      9: Decimal encoded control parameters
C                            1: Enable subreflector tilt model
C                           10: Disable phase slope model (pointing)
C                          100: inhibit local phase unwrapping of the
C                               V_PHA map,
C                         1000: Fit no model at all.  Output maps
C                               show raw beam data.
C                         2000: inhibit subr/focus (x,y) offset model
C                         4000: inhibit focus (z) model
C                          In general, you want to leave = 0.
C                     10: +1 for logarithmic amplitudes,
C                         .LE.0 for linear amplitudes
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
C                         observed antenna voltage beam pattern.
C                      3: Weights used in the regridding procedure.
C                    4,5: Derived amplitude and phase of the voltage
C                         distribution across the antenna aperture.
C                    6,7: Amplitude and phase of the point-spread
C                         function.  This indicates the blurring in
C                         the derived voltage distribution.
C                      8: Focus/subr. model corrections, in degrees.
C                      9: Map of the surface deviations of the
C                         antenna, units are millimeters.
C                     10: The interpolated antenna power pattern.
C                         Oversampling is supported.  See HELP
C   Notes:
C   1) Prefix usage for the arrays:
C         A  denotes the antenna voltage radiation pattern.
C         S  denotes the sampling function in (u,v) space.
C         V  denotes the derived voltage distribution across the
C            antenna aperture.
C         P  denotes the point-spread function of the derived voltage
C            distribution.
C
C         A,V and S,P are Fourier transform pairs.
C
C   Authors: Mike Kesteven & Mark Calabretta, Australia Telescope.
C      Origin; 1987/Nov     Code last modified; 1992/Mar/12
C   Modifications made by Rick Perley, (NRAO-VLA) March 1996.
C   Included Subreflector Tilt (RAP) March 2002.
C-----------------------------------------------------------------------
      INTEGER   IERR, KMAX, MVIS, NH, NVIS, ILG, MXINC, MFAC, IT, ID(3)
      REAL      CELLUV, CELLXY, DFX, DFY, DFZ, DP0, DPX, DPY, DU,
     *   DV, FX, FY, FZ, TX, TY, DTX, DTY, MGAIN(5), P0, PINC,
     *   VAMPMX, PAMPMX, PX, PY, RMS0, RMS, TGAIN(5), UKRNL(0:700),
     *   VKRNL(0:700), F1, L1
      CHARACTER DATMAP*8, HISTRY(100)*72, INSTRM*8, OBJECT*8, OBSERV*8,
     *   CHUV(2)*8, CHXY(2)*8, CHAZEL(2)*8, CHUNIT(5)*8
      LOGICAL   DOVP
      INCLUDE 'HOLOG'
      REAL      XFTK(0:MX), YFTK(0:MX)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA INSTRM, OBJECT, OBSERV /3*' '/
      DATA CHXY, CHUV, CHAZEL /'X','Y','L','M','Az','El'/
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
      WRITE (DATMAP,1010) ID
      DOVP = (VPARM(1).NE.0.0) .OR. (VPARM(2).NE.0.0) .OR.
     *   (VPARM(3).NE.0.0) .OR. (VPARM(4).NE.0.0) .OR. (VPARM(5).NE.0.0)
     *   .OR. (VPARM(6).NE.0.0) .OR. (VPARM(7).NE.0.0) .OR.
     *   (VPARM(8).NE.0.0)
C                                       Cell sizes in (x,y) and (u,v)
C                                       (x,y) in antenna coords
C                                       (u,v) in angular coords
C                                       MAPSZ is mapsize in meters.
      CELLXY = MAPSZ / NPIX
      CELLUV = LAMBDA / MAPSZ
C                                       Interpolate the visibilities
C                                       onto a regular grid.
      DU = 0.01
      DV = 0.01
      CALL KERNEL (UPRM, DU, UKRNL)
      CALL KERNEL (VPRM, DV, VKRNL)
      CALL GRID (NPIX, CELLUV, UKRNL, UPRM(2), DU, VKRNL, VPRM(2), DV,
     *   TAPER, XNORM, NVIS, MVIS, AAMP, APHA, ARE, AIM, ACPLX, SCPLX,
     *   WGT, UNIWT, UNIF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Write history text.
      NH = 1
      WRITE (HISTRY(NH),1100) TSKNAM, INFILE
      NH = NH + 1
      WRITE (HISTRY(NH),1110) TSKNAM, MVIS, NVIS
      NH = NH + 1
      WRITE (HISTRY(NH),1120) TSKNAM, FREQ, LAMBDA
      NH = NH + 1
      WRITE (HISTRY(NH),1130) TSKNAM, EL0
      NH = NH + 1
      WRITE (HISTRY(NH),1140) TSKNAM, DIAM
      NH = NH + 1
      WRITE (HISTRY(NH),1150) TSKNAM, SUBDIA
      NH = NH + 1
      WRITE (HISTRY(NH),1160) TSKNAM, FOCUS
      NH = NH + 1
      WRITE (HISTRY(NH),1250) TSKNAM, MAPSZ, NPIX
      NH = NH + 1
      WRITE (HISTRY(NH),1260) TSKNAM, UVMIN, UVMAX
      NH = NH + 1
      WRITE (HISTRY(NH),1270) TSKNAM, AMPSCL
      NH = NH + 1
      IF (DODFT) THEN
         IF (PHSGN.LT.0) WRITE (HISTRY(NH),1280) TSKNAM
         IF (PHSGN.GT.0) WRITE (HISTRY(NH),1290) TSKNAM
      ELSE
         IF (PHSGN.LT.0) WRITE (HISTRY(NH),1300) TSKNAM
         IF (PHSGN.GT.0) WRITE (HISTRY(NH),1310) TSKNAM
         END IF
      NH = NH + 1
      WRITE (HISTRY(NH),1320) TSKNAM, XYMIN, XYMAX
      NH = NH + 1
      WRITE (HISTRY(NH),1330) TSKNAM, UPRM
      NH = NH + 1
      WRITE (HISTRY(NH),1340) TSKNAM, VPRM
      NH = NH + 1
      WRITE (HISTRY(NH),1350) TSKNAM, OPT
C                                       Save the regridded maps if
C                                       required.  Store the amplitude
C                                       part of the antenna pattern.
      IF (OPT(1)) THEN
         IF (DOVECT.GT.0.0) THEN
            OUTNAM(13:18) = 'A_REAL'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(1), CHUV(1), NPIX, CELLUV, CHUV(2),
     *         NPIX, CELLUV, ARE, HISTRY, NH, MVIS, ISTOKE, IERR)
         ELSE
            OUTNAM(13:18) = 'A_AMP'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(1), CHUV(1), NPIX, CELLUV, CHUV(2),
     *         NPIX, CELLUV, AAMP, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
         END IF
C                                       Store phase part of ant pattern
      IF (OPT(2)) THEN
         IF (DOVECT.GT.0.0) THEN
            OUTNAM(13:18) = 'A_IMAG'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(1), CHUV(1), NPIX, CELLUV, CHUV(2),
     *         NPIX, CELLUV, AIM, HISTRY, NH, MVIS, ISTOKE, IERR)
         ELSE
            OUTNAM(13:18) = 'A_PHA'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(2), CHUV(1), NPIX, CELLUV, CHUV(2),
     *         NPIX, CELLUV, APHA, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
         END IF
C                                       Store the weighting function
      IF (OPT(3)) THEN
         OUTNAM(13:18) = 'WGT'
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, HFREQ, CHUNIT(5), CHUV(1), NPIX, CELLUV, CHUV(2),
     *      NPIX, CELLUV, WGT, HISTRY, NH, MVIS, ISTOKE, IERR)
         END IF
C                                       Compute the aperture voltage
C                                       distribution and point-spread
C                                       function.
      IF (OPT(4) .OR. OPT(5) .OR. OPT(6) .OR. OPT(7) .OR. OPT(8). OR.
     *   OPT(9) .OR. OPT(10)) THEN
C                                       Direct Fourier Transform.
         IF (DODFT) THEN
            CALL DFT (LAMBDA, NPIX, CELLXY, CELLXY, VAMP, VPHA, PAMP,
     *         PPHA, VAMPMX, IERR)
C                                       Compute the gridding correction
         ELSE
            KMAX = NINT(UPRM(2)/DU)
            CALL FTKRNL (UKRNL, DU, KMAX, NPIX, XFTK)
            KMAX = NINT(VPRM(2)/DV)
            CALL FTKRNL (VKRNL, DV, KMAX, NPIX, YFTK)
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
C                                       Resulting amplitude announce
         WRITE (MSGTXT,1355) VAMPMX
         CALL MSGWRT (3)
C                                       Blank shadowed and empty areas
         CALL BLANK (CELLXY, DIAM, SUBDIA, NPIX, VPHA)
C                                       Adjust for phase ambiguities
C        IF (ZIPV) CALL NITPIX(NPIX,VPHA,PV)
C                                       Correct phase for pointing,
C                                       focus, and feed offset.
         MSGTXT = 'OPTYPE IS ''' // OPTYPE // ''''
         CALL MSGWRT (8)
         IF (DOMOD) THEN
            IF (OPTYPE.EQ.'PFOC') THEN
               IF (DOVP) THEN
                  CALL FLATMO (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOPNT, PHAMOD, VPARM)
               ELSE
                  CALL FLATPH (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOPNT, PHAMOD)
                  END IF
               NH = NH + 1
               WRITE (HISTRY(NH),1385) TSKNAM
            ELSE IF (OPTYPE.EQ.'SURP') THEN
               IF (DOVP) THEN
                  CALL FLTMO3 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOTILT, NOPNT, XMAG, PHAMOD, TX, TY, DTX,
     *               DTY, VPARM)
               ELSE
                  CALL FLTPH3 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOTILT, NOPNT, XMAG, PHAMOD, TX, TY, DTX,
     *               DTY)
                  END IF
               NH = NH + 1
               WRITE (HISTRY(NH),1387) TSKNAM, XMAG
            ELSE
               IF (DOVP) THEN
                  CALL FLTMO2 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOTILT, NOPNT, XMAG, PHAMOD, TX, TY, DTX,
     *               DTY, VPARM)
               ELSE
                  CALL FLTPH2 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX,
     *               CELLXY, VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0,
     *               DPX, DPY, DFX, DFY, DFZ, RMS0, RMS, IERR, NOXY,
     *               NOFOC, NOTILT, NOPNT, XMAG, PHAMOD, TX, TY, DTX,
     *               DTY)
                  END IF
               NH = NH + 1
               WRITE (HISTRY(NH),1386) TSKNAM, XMAG
               END IF
            MSGTXT = HISTRY(NH)
            CALL MSGWRT (8)
            CALL BLANK (CELLXY, DIAM, SUBDIA, NPIX, PHAMOD)
            WRITE (MSGTXT,1390) P0, DP0
            CALL MSGWRT (3)
            WRITE (MSGTXT,1400) PX, DPX
            CALL MSGWRT (3)
            WRITE (MSGTXT,1410) PY, DPY
            CALL MSGWRT (3)
            IF (OPTYPE.NE.'PFOC') THEN
               WRITE (MSGTXT,1420) FX, DFX
               CALL MSGWRT (3)
               WRITE (MSGTXT,1430) FY, DFY
               CALL MSGWRT (3)
               WRITE (MSGTXT,1440) FZ, DFZ
               CALL MSGWRT (3)
               WRITE (MSGTXT, 1446) TX, DTX
               CALL MSGWRT (3)
               WRITE (MSGTXT, 1447) TY, DTY
            ELSE
               WRITE (MSGTXT,1421) FX, DFX
               CALL MSGWRT (3)
               WRITE (MSGTXT,1431) FY, DFY
               CALL MSGWRT (3)
               WRITE (MSGTXT,1443) FZ, DFZ
               END IF
            CALL MSGWRT (3)
            WRITE (MSGTXT,1441) RMS0
            CALL MSGWRT (3)
            WRITE (MSGTXT,1442) RMS
            CALL MSGWRT (3)
            END IF
C                                       Compute surface deviation map.
         CALL SURDEV (CELLXY, LAMBDA, FOCUS, NPIX, VPHA, VDEV)
C                                       Compute beam gain at obs freq
         F1 = 1.0
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, LAMBDA, VAMP, VPHA, F1,
     *      MGAIN(1), TGAIN(1))
         WRITE (MSGTXT,1450) MGAIN(1)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1460) TGAIN(1)
         CALL MSGWRT (3)
C                               Compute gain loss due to feed offset
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, LAMBDA, VAMP, PHAMOD,
     *           F1, MGAIN(1), TGAIN(1))
         WRITE (MSGTXT,1453) MGAIN(1)
         CALL MSGWRT (3)
C                                       K band estimate
         L1 = 0.013
         F1 = LAMBDA / L1
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, L1, VAMP, VPHA, F1,
     *      MGAIN(2), TGAIN(2))
         WRITE (MSGTXT,1451) MGAIN(2)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1461) TGAIN(2)
         CALL MSGWRT (3)
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, L1, VAMP, PHAMOD, F1,
     *      MGAIN(2), TGAIN(2))
         WRITE (MSGTXT,1454) MGAIN(2)
         CALL MSGWRT (3)
C                                       Q band estimate
         L1 = 0.007
         F1 = LAMBDA / L1
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, L1, VAMP, VPHA, F1,
     *    MGAIN(3), TGAIN(3))
         WRITE (MSGTXT,1452) MGAIN(3)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1462) TGAIN(3)
         CALL MSGWRT (3)
C                               Compute Gain loss due to feed offset
         CALL ANGAIN (NPIX, MAPSZ, DIAM, SUBDIA, L1, VAMP, PHAMOD, F1,
     *    MGAIN(3), TGAIN(3))
         WRITE (MSGTXT,1455) MGAIN(3)
         CALL MSGWRT (3)
C                                       Update history.
         NH = NH + 1
         WRITE (HISTRY(NH),1470) TSKNAM, VAMPMX
         IF (DOMOD) THEN
            NH = NH + 1
            WRITE (HISTRY(NH),1500) TSKNAM, P0, DP0
            NH = NH + 1
            WRITE (HISTRY(NH),1510) TSKNAM, PX, DPX
            NH = NH + 1
            WRITE (HISTRY(NH),1520) TSKNAM, PY, DPY
            IF (OPTYPE.NE.'PFOC') THEN
               NH = NH + 1
               WRITE (HISTRY(NH),1530) TSKNAM, FX, DFX
               NH = NH + 1
               WRITE (HISTRY(NH),1540) TSKNAM, FY, DFY
               NH = NH + 1
               WRITE (HISTRY(NH),1550) TSKNAM, FZ, DFZ
               NH = NH + 1
               WRITE (HISTRY(NH),1555) TSKNAM, TX, DTX
               NH = NH + 1
               WRITE (HISTRY(NH),1556) TSKNAM, TY, DTY
            ELSE
               NH = NH + 1
               WRITE (HISTRY(NH),1531) TSKNAM, FX, DFX
               NH = NH + 1
               WRITE (HISTRY(NH),1541) TSKNAM, FY, DFY
               NH = NH + 1
               WRITE (HISTRY(NH),1551) TSKNAM, FZ, DFZ
               END IF
         ELSE
            NH = NH + 1
            WRITE (HISTRY(NH),1560) TSKNAM
            END IF
         NH = NH + 1
         WRITE (HISTRY(NH),1562) TSKNAM, RMS
         NH = NH + 1
         WRITE (HISTRY(NH),1570) TSKNAM, MGAIN(1)
         NH = NH + 1
         WRITE (HISTRY(NH),1580) TSKNAM, TGAIN(1)
C                                       Store amplitude part of voltage
C                                       distribution
         IF (OPT(4)) THEN
            OUTNAM(13:18) = 'V_AMP'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(1), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VAMP, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
C                                       Store phase part of voltage
C                                       distribution
         IF (OPT(5)) THEN
            OUTNAM(13:18) = 'V_PHA'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VPHA, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
C                                       Store amplitude part of point
C                                       spread function
         IF (OPT(6)) THEN
            OUTNAM(13:18) = 'P_AMP'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(1), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PAMP, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
C                                       Store phase part of point spread
C                                       function
         IF (OPT(7)) THEN
            OUTNAM(13:18) = 'P_PHA'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PPHA, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
C                                       Store the model corrections
         IF (OPT(8)) THEN
            OUTNAM(13:18) = 'MODEL'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(2), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, PHAMOD, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
C                                       Store the surface deviation map
         IF (OPT(9)) THEN
            OUTNAM(13:18) = 'V_DEV'
            CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *         DATMAP, HFREQ, CHUNIT(3), CHXY(1), NPIX, CELLXY, CHXY(2),
     *         NPIX, CELLXY, VDEV, HISTRY, NH, MVIS, ISTOKE, IERR)
            END IF
         END IF
C                                       Compute the power pattern
      IF (OPT(10)) THEN
         MFAC = XOPT(10) + 0.75
         MFAC = MAX (MFAC, MXFAC)
         IT = (MXFAC * MX) / NPIX
         IF (MFAC.GT.IT) THEN
            MXINC = 2
            MFAC = IT
         ELSE
            MXINC = 1
            END IF
         ILG = MFAC * NPIX
         CALL BEAM (NPIX, ILG, MXINC, VAMP, VPHA, AAMP, APHA, VCPLX,
     *      WAMP, WPHA)
C                                       Store the power pattern.
         OUTNAM(13:18) = 'A_PWR'
         PINC = (RAD2DG * CELLUV * NPIX) / (MXINC * ILG)
         CALL HOLOUT (OUTNAM, OBJECT, TELESC, INSTRM, OBSERV, DATOBS,
     *      DATMAP, HFREQ, CHUNIT(4), CHAZEL(1), NPIX, PINC, CHAZEL(2),
     *      NPIX, PINC, AAMP, HISTRY, NH, MVIS, ISTOKE, IERR)
         END IF
C                                       Compute the visibility coordinat
C                                       Clean up.
 990  CALL TSKEND (IERR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' INITIATING HOLOG.')
 1010 FORMAT (I4.4,I2.2,I2.2)
 1020 FORMAT ('ERROR',I4,' READING/GRIDING THE VISIBILITIES.')
 1100 FORMAT (A,'Visibility file: ',A)
 1110 FORMAT (A,'       Visibilities used:',I6,' of',I6)
 1120 FORMAT (A,'     Observing frequency:',F11.6,' GHz,  Wavelength:',
     *   F10.6,' m')
 1130 FORMAT (A,'     Satellite elevation:',F8.3,' deg')
 1140 FORMAT (A,'        Antenna diameter:',F8.3,' m')
 1150 FORMAT (A,'   Subreflector diameter:',F8.3,' m')
 1160 FORMAT (A,'            Focal length:',F8.3,' m')
 1250 FORMAT (A,'                Map size:',F8.3,' m,',I5,' pixels')
 1260 FORMAT (A,'Antenna scan angle range:',2F10.5,' radians')
 1270 FORMAT (A,'     Amplitude scaled by:',F8.3)
 1280 FORMAT (A,'       Fourier transform: DFT with phase negation')
 1290 FORMAT (A,'       Fourier transform: DFT without phase negation')
 1300 FORMAT (A,'       Fourier transform: FFT with phase negation')
 1310 FORMAT (A,'       Fourier transform: FFT without phase negation')
 1320 FORMAT (A,'   Antenna surface range:',2F10.5,' m')
 1330 FORMAT (A,' Interpolation type in l: ',5F7.3)
 1340 FORMAT (A,' Interpolation type in m: ',5F7.3)
 1350 FORMAT (A,'     Output option flags: ',10L1)
 1355 FORMAT ('AMPLITUDE PEAK:',1P,E14.5)
 1385 FORMAT (A,'Prime focus phase model used')
 1386 FORMAT (A,'Subreflector phase model with Magnification:',F6.2)
 1387 FORMAT (A,'Subreflector with reference pointing Magnification:',
     *   F6.2)
 1390 FORMAT ('                Phase offset:',F10.2,F9.2,' deg.')
 1400 FORMAT ('  Equiv. pointing error in X:',F10.2,F9.2,' arcmin')
 1410 FORMAT ('  Equiv. pointing error in Y:',F10.2,F9.2,' arcmin')
 1420 FORMAT ('    Subreflector offset in X:',F10.2,F9.2,' mm.')
 1421 FORMAT ('     Prime focus offset in X:',F10.2,F9.2,' mm.')
 1430 FORMAT ('    Subreflector offset in Y:',F10.2,F9.2,' mm.')
 1431 FORMAT ('     Prime focus offset in Y:',F10.2,F9.2,' mm.')
 1440 FORMAT ('    Subreflector focus error:',F10.2,F9.2,' mm.')
 1443 FORMAT ('     Prime focus offset in Z:',F10.2,F9.2,' mm.')
 1446 FORMAT ('    Subreflector tilt in X  :',F10.2,F9.2,' eff. mm')
 1447 FORMAT ('    Subreflector tilt in Y  :',F10.2,F9.2,' eff. mm')
 1441 FORMAT (' Pre-fit weighted rms half-path error:',F10.3,' mm.')
 1442 FORMAT ('Post-fit weighted rms half-path error:',F10.3,' mm.')
 1450 FORMAT ('     Measured gain with observed illumination  :',
     *  F7.2,' dB,')
 1451 FORMAT (' K-band Estimated gain (observed illumination) :',
     *  F7.2,' dB,')
 1452 FORMAT (' Q-band Estimated gain (observed illumination) :',
     *  F7.2,' dB,')
 1453 FORMAT (' Gain with no panel errors and feed offset in  :',
     *  F7.2,' dB,')
 1454 FORMAT (' K-band Est. Gain with only feed offset        :',
     *  F7.2,' dB,')
 1455 FORMAT (' Q-band Est. Gain with only feed offset        :',
     *  F7.2,' dB,')
 1460 FORMAT ('     Theoretical gain with uniform illumination:',
     *  F7.2,' dB.')
 1461 FORMAT (' K-band Theoretical gain (uniform illumination):',
     *  F7.2,' dB.')
 1462 FORMAT (' Q-band Theoretical gain (uniform illumination):',
     *  F7.2,' dB.')
 1470 FORMAT (A,'Amplitude Peak:',1P,E14.5)
 1500 FORMAT (A,'           Phase offset:',F10.2,F9.2,' deg')
 1510 FORMAT (A,'  Equiv. Pointing Error in X:',F10.2,F9.2,' arcmin')
 1520 FORMAT (A,'  Equiv. Pointing Error in Y:',F10.2,F9.2,' arcmin')
 1530 FORMAT (A,'    Subreflector offset in X:',F10.2,F9.2,' mm')
 1531 FORMAT (A,'     Prime focus offset in X:',F10.2,F9.2,' mm')
 1540 FORMAT (A,'    Subreflector offset in Y:',F10.2,F9.2,' mm')
 1541 FORMAT (A,'     Prime focus offset in Y:',F10.2,F9.2,' mm')
 1550 FORMAT (A,'    Subreflector focus error:',F10.2,F9.2,' mm')
 1551 FORMAT (A,'     Prime focus offset in Z:',F10.2,F9.2,' mm')
 1555 FORMAT (A,'    Subreflector tilt in X:',F10.2,F9.2,'eff. mm.')
 1556 FORMAT (A,'    Subreflector tilt in Y:',F10.2,F9.2,'eff. mm.')
 1560 FORMAT (A,'       Phase correction: suppressed')
 1562 FORMAT (A, '       Weighted Half-path error:',F10.3,' mm.')
 1570 FORMAT (A,'          Measured gain:',F7.2,' dB')
 1580 FORMAT (A,'       Theoretical gain:',F7.2,' dB')
      END
      SUBROUTINE HOLINI (IERR)
C-----------------------------------------------------------------------
C   HOLINI reads adverbs for HOLOG.
C   Output:
C      IERR     I      Error status, 0 means success.
C   Output in commons INPARM, INTLOG, INCHAR:
C      INFILE   C*48   Input visibility file name.
C      OUTNAM   C*36   WAWA image namestring.
C      FREQ     R      Observing frequency, in GHz.
C      LAMBDA   R      Observing wavelength, in meters.
C      EL0      R      Satellite elevation, in degrees.
C      DIAM     R      Antenna diameter, in meters.
C      SUBDIA   R      Subreflector diameter, in meters.
C      FOCUS    R      Focal length, in meters.
C      MAPSZ    R      Size of the map, in meters.
C      NPIX     I      Number of pixels on a side of the map.
C      UVMIN    R      Range of |l| and |m| to include.  Negative
C      UVMAX           values denote (min,max) of SQRT(l*l + m*m).
C      AMPSCL   R      Amplitude scaling factor.
C      PHSGN    R      Factor by which to multiply the measured phase.
C      DODFT    L      If true do a DFT, else an FFT.
C      XYMIN    R      Range of |x| and |y| used in correcting for
C      XYMAX    R      pointing, focus, and feed offset. Negative
C                      values denote (min,max) of r = SQRT(x*x + y*y).
C      ZIPV     L      If true, perform local phase unwrapping in
C                      the antenna aperture plane (V_PHA).
C      DOMOD    L      If true, correct the V_PHA map for pointing,
C                      focus, and feed offset.
C      LINEAR   L      If true, the input amplitudes are linear, else
C                      logarithmic
C      UPRM     R(5)   Interpolation parameters in u.
C      VPRM     R(5)   Interpolation parameters in v.
C      OPT      L(10)  Output option flags.
C      UNIWT    L      Uniform weighting flag
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   ANY
      INTEGER   CONTRL, IROUND, J, IS, IA, IB, LN, JTRIM, OUTDI, I, IIF
      CHARACTER CHOUTN*12, PRGNAM*6, STOK(4)*2
      INCLUDE 'HOLOG'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PUVD.INC'
      DATA PRGNAM /'HOLOG '/
      DATA STOK /'RR', 'LL', 'RL', 'LR'/
C-----------------------------------------------------------------------
C                                       Get the adverbs and restart AIPS
      CALL TSKBEG (PRGNAM, 93, XINFIL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, ' READING ADVERB VALUES'
         GO TO 995
         END IF
C                                       Decode the input file name.
      CALL H2CHR (48, 1, XINFIL, INFILE)
C                                       Construct default file name
      IA = XANT + 0.1
      IB = IA / 10000
      IIF = MOD (IA, 100)
      IA = IA / 100
      IA = MOD (IA, 100)
      IA = MAX (1, MIN (IA, MAXANT))
      IB = MAX (0, MIN (IB, MAXANT))
      IIF = MAX (0, MIN (IIF, MAXIF))
      IS = XSTOK + 0.1
      IS = MAX (1, MIN (IS, 4))
      IF ((INFILE.EQ.'ANT') .OR. (INFILE.EQ.' ')) INFILE = 'FITS:'
C                                       get frequency from INFILE
      CALL FRQGET (INFILE, LUNVIS, FREQ, DATOBS, TELESC, ISTOKE, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, ' FINDING FREQUENCY IN INFILE'
         GO TO 995
         END IF
C                                       Output file name (the class will
C                                       be filled in by HOLOG).
      CALL H2CHR (12, 1, XOUTNA, CHOUTN)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
C                                       also fix the output filename
      IF (CHOUTN.EQ.' ') CHOUTN = 'HOLO'
      LN = JTRIM (CHOUTN)
      IF (LN.LE.5) WRITE (CHOUTN(6:),1011) IA, IB, STOK(IS)
      OUTDI = IROUND (XOUTDI)
      CALL A2WAWA (CHOUTN, ' ', 0, 'MA', OUTDI, NLUSER, OUTNAM)
C                                       Observing parameters.
C                                       Observing frequency (GHz), and
C                                       wavelength (m).
      HFREQ = FREQ
      IF (REFREQ.GT.0.0) FREQ = REFREQ
      IF (FREQ.LE.0.0) THEN
         MSGTXT = 'HOLINI: The observing frequency MUST be specified.'
         GO TO 990
         END IF
      LAMBDA = VELITE / (FREQ * 1.0E9)
C                                       Antenna diameter, in meters,
      IF (DIAM.LE.0.0) THEN
         MSGTXT = 'HOLINI: Antenna diameter must be specified'
         GO TO 990
         END IF
C                                       Subreflector diameter, in meters
      IF (SUBDIA.LE.0.0) THEN
         SUBDIA = 4.0
         MSGTXT = 'HOLINI: Subreflector diameter must be specified'
         GO TO 990
         END IF
C                                       Focal length, in meters
      IF (FOCUS.LE.0.0) THEN
         MSGTXT = 'HOLINI: The focal length MUST be specified.'
         GO TO 990
         END IF
C                                       Compute the visibility coordinat
C                                       Data reduction parameters.
C                                       Actual map size, in meters.
      IF (MAPSZ.LE.0.0) THEN
         MSGTXT = 'HOLINI: The map size MUST be specified.'
         GO TO 990
         END IF
C                                       Compute the visibility coordinat
C                                       Output must be power of 2
      NPIX = IROUND (XNPIX)
      IF (NPIX.EQ.0) NPIX =128
      LN = NPIX
      IF (NPIX.GT.MX) NPIX = MX
      IF(NPIX.LT.32) NPIX = 32
      I = 16
 10   I = I * 2
      IF (I.GE.MX) GO TO 15
         IF ((NPIX.GT.I) .AND. (NPIX.LT.I*2)) NPIX = I * 2
         GO TO 10
 15   IF (NPIX.NE.LN) THEN
         WRITE (MSGTXT,1015) LN, NPIX
         CALL MSGWRT (4)
         END IF
C                                       Amplitude scaling factor.
      IF (ABS(AMPSCL).LE.0.01) AMPSCL = 1.0
C                                       Fourier transform control.
      CONTRL = IROUND (XFT)
      IF (CONTRL.EQ.0) CONTRL = -1
C                                       Phase negation.
      PHSGN = SIGN(1, CONTRL)
C                                       DFT or FFT?
      DODFT = ABS(CONTRL).EQ.2
C                                       Decode control parameters.
C                                       Set defaults first.
      CONTRL = IROUND (XCNTRL)
      NOXY = .FALSE.
      NOPNT = .FALSE.
      DOMOD  = .TRUE.
      ZIPV = .TRUE.
      NOTILT = .TRUE.
      NOFOC = .FALSE.
      IF (CONTRL.GE.4000) THEN
         NOFOC = .TRUE.
         CONTRL = CONTRL - 4000
         END IF
      IF (CONTRL.GE.2000) THEN
         NOXY = .TRUE.
         CONTRL = CONTRL - 2000
         END IF
      IF (CONTRL.GE.1000) THEN
         DOMOD = .FALSE.
         CONTRL = CONTRL - 1000
         END IF
      IF (CONTRL.GE.100) THEN
         ZIPV = .FALSE.
         CONTRL = CONTRL - 100
         END IF
      IF (CONTRL.GE.10) THEN
         NOPNT = .TRUE.
         CONTRL = CONTRL - 10
         END IF
      IF (CONTRL.GE.1) THEN
         NOTILT = .FALSE.
         CONTRL = CONTRL - 1
         END IF
C                                       Input amp's linear OR log
      LINEAR = XLINEA.LT.0.5
C                                       Uniform weighting?
      IF ((UPRM(1).LT.-0.01) .OR. (VPRM(1).LT.-0.01)) THEN
         UNIWT = .FALSE.
         UPRM(1) = ABS (UPRM(1))
         VPRM(1) = ABS (VPRM(1))
      ELSE
         UNIWT = .TRUE.
         END IF
C                                       Compute the visibility coordinat
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
      GO TO 999
C                                       Error
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
      SUBROUTINE FRQGET (INFILE, LUN, FREQ, DATOBS, TELESC, ISTOK, IERR)
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
      CHARACTER INFILE*(*), DATOBS*(*), TELESC*(*)
      INTEGER   LUN, ISTOK, IERR
      REAL      FREQ
C
      INTEGER   IND, J
      CHARACTER STR*132, ANTSTR*2
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZTXOPN ('READ', LUN, IND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, ' OPEN INFILE'
         GO TO 990
         END IF
      DATOBS = ' '
      TELESC = ' '
      ISTOK = 0
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
            J = INDEX (STR, 'Antenna =')
            IF (J.GT.0) THEN
               J = J + 10
               ANTSTR = STR(J:J+1)
               TELESC = 'EA' // ANTSTR
               END IF
            J = INDEX (STR, 'DATE-OBS =')
            IF (J.GT.0) THEN
               J = J + 12
               DATOBS = STR(J:J+7)
               END IF
            J = INDEX (STR, 'Stokes')
            IF (J.GT.0) THEN
               J = J + 10
               ANTSTR = STR(J:J+1)
               IF (ANTSTR(2:2).EQ.'_') ANTSTR(2:2) = ' '
               IF (ANTSTR.EQ.'I') ISTOK = 1
               IF (ANTSTR.EQ.'Q') ISTOK = 2
               IF (ANTSTR.EQ.'U') ISTOK = 3
               IF (ANTSTR.EQ.'V') ISTOK = 4
               IF (ANTSTR.EQ.'RR') ISTOK = -1
               IF (ANTSTR.EQ.'LL') ISTOK = -2
               IF (ANTSTR.EQ.'RL') ISTOK = -3
               IF (ANTSTR.EQ.'LR') ISTOK = -4
               IF (ANTSTR.EQ.'XX') ISTOK = -5
               IF (ANTSTR.EQ.'YY') ISTOK = -6
               IF (ANTSTR.EQ.'XY') ISTOK = -7
               IF (ANTSTR.EQ.'YX') ISTOK = -8
               END IF
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
      SUBROUTINE KERNEL (PRM, DU, KRNL)
C-----------------------------------------------------------------------
C   KERNEL constructs the kernel used for interpolation.
C   In/out:
C      PRM      R(5)   Parameters defining the interpolation function
C                        1: Type of interpolation function,
C                           1: Pillbox,
C                           2: Exponential,
C                           3: Sinc,
C                           4: Sinc*Exponential,
C                           5: Spheroidal (default).
C                        2: Support size in u.
C                      3-5: Parameters defining the function.
C                      On return PRM contains the actual parameters.
C      DU       R      Separation between the elements of KRNL in units
C                      of the cell spacing.
C   Output:
C      KRNL     R(*)   Array containing one half of the interpolation
C                      function.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Jan/21. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      REAL      PRM(5), DU, KRNL(0:*)
C
      INTEGER   IALPHA, IERR, IFLAG, ISUPP, J, JMAX, INTYP
      REAL      ETA, PSI, U
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Type of interpolation to apply.

       INTYP = NINT (PRM(1))
       IF ((INTYP.LT.1) .OR. (INTYP.GT.5)) THEN
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
         ISUPP  = NINT (2.0*PRM(2))
         IF (ISUPP.LT.4) ISUPP = 4
         IF (ISUPP.GT.8) ISUPP = 8
         PRM(2) = ISUPP/2.0
         IALPHA = NINT (2.0*PRM(3) + 1.0)
         IF (IALPHA.LT.1) IALPHA = 1
         IF (IALPHA.GT.5) IALPHA = 5
         PRM(3) = (IALPHA-1)/2.0
         IFLAG  = 0
         END IF
C                                       Radius of support.
      JMAX = NINT (PRM(2) / DU)
      IF (JMAX.GT.700) JMAX = 700
      PRM(2) = JMAX * DU
C                                       Pillbox.
      IF (INTYP.EQ.1) THEN
         DO 10 J = 0,JMAX
            U = J * DU
            KRNL(J) = 1.0
            IF (J.EQ.JMAX) KRNL(J) = 0.5
 10         CONTINUE
C                                       Exponential.
      ELSE IF (INTYP.EQ.2) THEN
         DO 20 J = 0,JMAX
            U = J * DU
            KRNL(J) = EXP (-((U / PRM(3)) ** PRM(4)))
 20         CONTINUE
C                                       Sinc.
      ELSE IF (INTYP.EQ.3) THEN
         KRNL(0) = 1.0
         DO 30 J = 1,JMAX
            U = J * DU
            KRNL(J) = SIN (PI *U / PRM(3)) / (PI * U / PRM(3))
 30         CONTINUE
C                                       Sinc*Exponential.
      ELSE IF (INTYP.EQ.4) THEN
         KRNL(0) = 1.0
         DO 40 J = 1,JMAX
            U = J * DU
            KRNL(J) = SIN (PI * U / PRM(3)) / (PI * U / PRM(3)) *
     *         EXP (-((U / PRM(4)) ** PRM(5)))
 40         CONTINUE
C                                       Compute the visibility coordinat
C                                       Spheroidal.
      ELSE IF (INTYP.EQ.5) THEN
         DO 50 J = 0,JMAX
            U = J * DU
            ETA = REAL (J) / REAL (JMAX)
            CALL SPHFN (IALPHA, ISUPP, IFLAG, ETA, PSI, IERR)
            KRNL(J) = PSI
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRID (N, CELLUV, UKRNL, SU, DU, VKRNL, SV, DV, TAPER,
     *   XNORM, NVIS, MVIS, AAMP, APHA, ARE, AIM, ACPLX, SCPLX, WGT,
     *   UNIWT, UNIF, IERR)
C-----------------------------------------------------------------------
C   GRID grids the visibility data for the holography routines.  It also
C   unwraps the antenna pattern phase map if requested.
C   Inputs:
C      N        I        Number of pixels on a side of the map.
C      CELLUV   R        Cell spacing in the uv plane.
C      UKRNL    R(*)     Array containing one half of the
C                        interpolation function in u.
C      SU       R        Support radius for UKRNL, in cell units.
C      DU       R        Separation between the elements of UKRNL in
C                        units of the cell spacing.
C      VKRNL    R(*)     Array containing one half of the
C                        interpolation function in v.
C      SV       R        Support radius for VKRNL, in cell units.
C      DV       R        Separation between the elements of VKRNL in
C                        units of the cell spacing.
C      TAPER    R(2)     Taper type and width to 0.5 in cells
C   Output:
C      NVIS     I        Number of visibilities read.
C      MVIS     I        Number of visibilities used.
C      AAMP     R(N,N)   The amplitude of the antenna pattern,
C                        in a form suitable for output.
C      APHA     R(N,N)   The phase of the antenna pattern,
C                        in a form suitable for output.
C      ARE      R(N,N)   Real part of antenna pattern
c      AIM      r(n,n)   Imaginary part of antenna pattern
C      ACPLX    CX(N,N)  Regridded antenna pattern.
C      SCPLX    CX(N,N)  Regridded sampling function.
C      WGT      R(N,N)   Weight function from the interpolation.
C      UNIF     R(N,N)   Uniform weighting summing area
C      IERR     I        Error status, 0 means success.
C   Called: {HGETVS, PHAPOT, PHAZIP}
C   Algorithm:
C      Convolution with a kernel of limited support size.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   N, NVIS, MVIS, IERR
      REAL      CELLUV, UKRNL(0:*), SU, DU, VKRNL(0:*), SV, DV,
     *   TAPER(2), XNORM, AAMP(N,N), APHA(N,N), ARE(N,N), AIM(N,N),
     *   WGT(N,N), UNIF(N,N)
      LOGICAL   UNIWT
      COMPLEX   ACPLX(N,N), SCPLX(N,N)
C
      INTEGER   IOFFU, IOFFV, IU, IU1, IU2, IV, IV1, IV2, MU, MV, NHALF,
     *   NMISS, NOKAY, TAPTYP, IROUND
      REAL      AMP, CS, OFFU, OFFV, PHA, SHFTU, SHFTV, SN, T1, T2, U,
     *   V, W, WT, FACT, TAPARM, AMAX, PMAX
      COMPLEX   VIS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialization.
C                                       Zero the output arrays.
      DO 20 IV = 1,N
         DO 10 IU = 1,N
            ACPLX(IU,IV) = (0.0, 0.0)
            SCPLX(IU,IV) = (0.0, 0.0)
            UNIF(IU,IV) = 0.0
            WGT(IU,IV)  = 0.0
 10         CONTINUE
 20      CONTINUE
C                                       Taper type
      TAPTYP = IROUND (TAPER(1))
      IF ((TAPTYP.LE.0) .OR. (TAPTYP.GT.3)) TAPTYP = 0
      IF (TAPER(2).LE.3.) TAPER(2) = N / 3.0
      IF (TAPTYP.EQ.1) THEN
         TAPARM = -ALOG (2.0) / (CELLUV * TAPER(2)) ** 2
      ELSE IF (TAPTYP.EQ.2) THEN
         TAPARM = -ALOG (2.0) / (CELLUV * TAPER(2))
      ELSE IF (TAPTYP.EQ.3) THEN
         TAPARM = 0.5 / (CELLUV * TAPER(2))
         END IF
C                                       Uniform weighting
      IF (UNIWT) THEN
C                                       loop through all visibilities
         NVIS = 0
         NMISS = 0
         NOKAY = 0
 25      CALL HGETVS (NVIS, MVIS, U, V, W, AMP, PHA, IERR)
         IF (IERR.EQ.0) THEN
            IU = NINT (U/CELLUV) + 1
            IF (IU.LT.1) IU = IU + N
            IV = NINT (V/CELLUV) + 1
            IF (IV.LT.1) IV = IV + N
            IF ((IU.LT.1) .OR. (IV.LT.1) .OR. (IU.GT.N) .OR. (IV.GT.N))
     *         THEN
               NMISS = NMISS + 1
            ELSE
               UNIF(IU,IV) = UNIF(IU,IV) + 1.0
               NOKAY = NOKAY + 1
               END IF
            GO TO 25
            END IF
         IF (NMISS.GT.0) THEN
            WRITE (MSGTXT,1025) NMISS
            CALL MSGWRT (6)
            END IF
         IF (NOKAY.LE.0) THEN
            MSGTXT = 'NO VALID DATA FOUND: CHECK INPUTS'
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         END IF
      NHALF = N/2
C                                       Interpolate the visibilities
C                                       onto the uv plane.
C                                       Loop through the data file.
      NVIS = 0
      NMISS = 0
      NOKAY = 0
 30   CALL HGETVS (NVIS, MVIS, U, V, W, AMP, PHA, IERR)
         IF (IERR.NE.0) GO TO 60
C                                       uniform weighting, if applicable
         FACT = 1.0
         IU = NINT (U/CELLUV) + 1
         IF (IU.LT.1) IU = IU + N
         IV = NINT (V/CELLUV) + 1
         IF (IV.LT.1) IV = IV + N
         IF ((IU.LT.1) .OR. (IV.LT.1) .OR. (IU.GT.N) .OR. (IV.GT.N))
     *      THEN
            NMISS = NMISS + 1
            GO TO 30
         ELSE
            IF (UNIWT) FACT = 1.0 / UNIF(IU,IV)
            END IF
C                                       taper
         IF (TAPTYP.EQ.1) THEN
            FACT = FACT * EXP (TAPARM * (U*U + V*V))
         ELSE IF (TAPTYP.EQ.2) THEN
            FACT = FACT * EXP (TAPARM * SQRT (U*U + V*V))
         ELSE IF (TAPTYP.EQ.3) THEN
            FACT = FACT * (1.0 - TAPARM * SQRT (U*U + V*V))
            END IF
         FACT = MAX (0.0, FACT)
C                                       Range of pixels within the
C                                       support size.  Ensure that IU1
C                                       and IV1 are rounded up, with IU2
C                                       and IV2 rounded down.
         IU1 = INT(U/CELLUV - SU - 9999.0) + 9999
         IU2 = INT(U/CELLUV + SU + 9999.0) - 9999
         IV1 = INT(V/CELLUV - SV - 9999.0) + 9999
         IV2 = INT(V/CELLUV + SV + 9999.0) - 9999
         IF (IU1.LT.-NHALF) IU1 = -NHALF
         IF (IU1.GE.NHALF)  GO TO 30
         IF (IU2.LT.-NHALF) GO TO 30
         IF (IU2.GE.NHALF)  IU2 = NHALF - 1
         IF (IV1.LT.-NHALF) IV1 = -NHALF
         IF (IV1.GE.NHALF)  GO TO 30
         IF (IV2.LT.-NHALF) GO TO 30
         IF (IV2.GE.NHALF)  IV2 = NHALF - 1
         NOKAY = NOKAY + 1
C                                       Compute the complex visibility.
         CS  = AMP * COS(PHA)
         SN  = AMP * SIN(PHA)
         VIS = CMPLX (CS, SN)
C                                       Add visibility into the grid.
         DO 50 IV = IV1,IV2
            MV = IV + 1
            IF (MV.LT.1) MV = MV + N
            OFFV  = IV - V/CELLUV
            IOFFV = ABS(NINT(OFFV/DV))
            DO 40 IU = IU1,IU2
               MU = IU + 1
               IF (MU.LT.1) MU = MU + N
               OFFU  = IU - U/CELLUV
               IOFFU = ABS(NINT(OFFU/DU))
               WT = UKRNL(IOFFU) * VKRNL(IOFFV) * FACT
               ACPLX(MU,MV) = ACPLX(MU,MV) + VIS * WT
               SCPLX(MU,MV) = SCPLX(MU,MV) + (1.0,0.0) * WT
               WGT(MU,MV)   =   WGT(MU,MV) + WT
 40            CONTINUE
 50         CONTINUE
         GO TO 30
C                                       done
 60   IF ((NMISS.GT.0) .AND. (.NOT.UNIWT)) THEN
         WRITE (MSGTXT,1025) NMISS
         CALL MSGWRT (6)
         END IF
      IF (NOKAY.LE.0) THEN
         MSGTXT = 'NO VALID DATA FOUND: CHECK INPUTS'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
      WRITE (MSGTXT,1060) NOKAY, NVIS
      CALL MSGWRT (3)
      MVIS = NOKAY
      IF (IERR.EQ.-1) IERR = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'GRID: ERROR READING THE VISIBILITY FILE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Compute the amplitude and phase,
C                                       and reorganize them and the
C                                       weight for output.
      MV = N/2
      DO 100 IV = 1,N/2
         MV = MV + 1
         MU = N/2
         DO 90 IU = 1,N
            MU = MU + 1
            IF (MU.GT.N) MU = MU - N
            T1 = REAL (ACPLX(MU,MV))
            T2 = AIMAG (ACPLX(MU,MV))
            AAMP(IU,IV) = SQRT (T1*T1 + T2*T2)
            ARE(IU,IV) = T1
            AIM(IU,IV) = T2
            IF ((T1.EQ.0.0) .AND. (T2.EQ.0.0)) THEN
               APHA(IU,IV) = 0.0
            ELSE
               APHA(IU,IV) = ATAN2 (T2, T1) * RAD2DG
               END IF
            T1 = REAL (ACPLX(IU,IV))
            T2 = AIMAG (ACPLX(IU,IV))
            AAMP(MU,MV) = SQRT (T1*T1 + T2*T2)
            ARE(MU,MV) = T1
            AIM(MU,MV) = T2
            IF ((T1.EQ.0.0) .AND. (T2.EQ.0.0)) THEN
               APHA(MU,MV) = 0.0
            ELSE
               APHA(MU,MV) = ATAN2 (T2, T1) * RAD2DG
               END IF
            WT = WGT(IU,IV)
            WGT(IU,IV) = WGT(MU,MV)
            WGT(MU,MV) = WT
  90        CONTINUE
 100     CONTINUE
      SHFTV = 1.0
      DO 120 IV = 1,N
         SHFTU = SHFTV
         DO 110 IU = 1,N
            ACPLX(IU,IV) = SHFTU*ACPLX(IU,IV)
            SCPLX(IU,IV) = SHFTU*SCPLX(IU,IV)
            SHFTU = -SHFTU
 110        CONTINUE
         SHFTV = -SHFTV
 120     CONTINUE
C                                       Replace empties with blanks
C                                       find maximum amp and its phase
      AMAX = -10000.
      DO 140 IV = 1,N
         DO 130 IU = 1,N
            IF (WGT(IU,IV).EQ.0.0) THEN
               APHA(IU,IV) = FBLANK
               AAMP(IU,IV) = FBLANK
               ARE(IU,IV)  = FBLANK
               AIM(IU,IV)  = FBLANK
            ELSE
               AAMP(IU,IV) = AAMP(IU,IV) / WGT(IU,IV)
               ARE(IU,IV) = ARE(IU,IV) / WGT(IU,IV)
               AIM(IU,IV) = AIM(IU,IV) / WGT(IU,IV)
               IF (AAMP(IU,IV).GT.AMAX) THEN
                  AMAX = AAMP(IU,IV)
                  PMAX = APHA(IU,IV)
                  END IF
               END IF
 130        CONTINUE
 140     CONTINUE
C                                       normalize
      IF ((AMAX.GT.0.0) .AND. (XNORM.GT.0.0)) THEN
         WRITE (MSGTXT,1140) AMAX
         CALL MSGWRT (4)
         DO 160 IV = 1,N
            DO 150 IU = 1,N
               IF (WGT(IU,IV).GT.0.0) THEN
                  AAMP(IU,IV) = AAMP(IU,IV) / AMAX
                  APHA(IU,IV) = APHA(IU,IV) - PMAX
                  IF (APHA(IU,IV).GT.180.0) APHA(IU,IV) = APHA(IU,IV) -
     *               360.0
                  IF (APHA(IU,IV).LT.-180.0) APHA(IU,IV) = APHA(IU,IV) +
     *               360.0
                  END IF
 150           CONTINUE
 160        CONTINUE
      ELSE
         MSGTXT = 'Gridded amplitude and phase not normalized'
         CALL MSGWRT (4)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('GRID:',I7,' SAMPLES WERE OUTSIDE THE GRID AREA')
 1060 FORMAT ('Gridded',I7,' visibilities of',I7,' read')
 1140 FORMAT ('Gridded amplitude normalized by',F7.3)
      END
      SUBROUTINE HGETVS (NVIS, MVIS, U, V, W, AMP, PHA, IERR)
C-----------------------------------------------------------------------
C   HGETVS reads the next visibility.
C   Given (via common INPARM, INTLOG, INCHAR):
C      INFILE      C*48  Input visibility file name.
C      LUNVIS      I     Logical unit number to use for the input
C                        visibility file.
C      LAMBDA      R     Observing wavelength, in meters.
C      EL0         R     Satellite elevation, in degrees.
C      UVMIN       R     Range of |l| and |m| to include.  Negative
C      UVMAX       R     values denote a range of SQRT(l*l + m*m).
C      LINEAR      L     If true, the input amplitudes are linear,
C                        else logarithmic.
C      AMPSCL      R     Amplitude scaling factor.
C      PHSGN       R     Factor by which to multiply the measured
C                        phase.
C   Given and returned:
C      NVIS        I     Progressive number of visibilities read.
C                        If given as zero, the file will be opened.
C    Returned:
C      MVIS        I     Number of visibilities actually used.
C      U,V,W       R     The (l,m,n) coordinates of the visibility.
C      AMP         R     Visibility amplitude, volts.
C      PHA         R     Visibility phase, radians.
C      IERR        I     Error status, 0: success,
C                           -1: end of file.
C-----------------------------------------------------------------------
      INTEGER   NVIS, MVIS, IERR
      REAL      U, V, W, AMP, PHA
C
      INTEGER   JTRIM, KBP, KBLIM
      REAL      AZOFF, COSAZO, COSEL, COSEL0, RUV, EL, ELOFF, SINAZO,
     *   SINEL, SINEL0
      CHARACTER STR*80
      DOUBLE PRECISION DBLX
      INCLUDE 'HOLOG'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Open the holography data file
      IF (NVIS.EQ.0) THEN
         CALL ZTXOPN ('QRED', LUNVIS, INDVIS, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 999
         MVIS = 0
         END IF
C                                       Loop until get valid visibility
 40   CALL ZTXIO ('READ', LUNVIS, INDVIS, STR, IERR)
         IF (IERR.EQ.2) GO TO 70
         IF (IERR.NE.0) GO TO 50
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
         IF (EL0.GT.0.1) THEN
C                                       relative -> absolute elevation
            EL = EL0 + ELOFF
            COSEL0 = COS(EL0*DG2RAD)
            SINEL0 = SIN(EL0*DG2RAD)
            COSAZO = COS(AZOFF*DG2RAD)
            SINAZO = SIN(AZOFF*DG2RAD)
            COSEL  = COS(EL*DG2RAD)
            SINEL  = SIN(EL*DG2RAD)
            U =  SINAZO*COSEL0
            V = -COSAZO*COSEL0*SINEL + SINEL0*COSEL
            W =  COSAZO*COSEL0*COSEL + SINEL0*SINEL
C                                       input is U, V already
         ELSE
            U = AZOFF
            V = ELOFF
            W = 0.
            END IF
         RUV = SQRT (U*U + V*V)
C                                       data point included?
         IF (UVMIN.GT.0.0) THEN
            IF (ABS(U).LT.UVMIN) GO TO 40
            IF (ABS(V).LT.UVMIN) GO TO 40
         ELSE IF (UVMIN.LT.0.0) THEN
            IF (U*U+V*V.LT.UVMIN*UVMIN) GO TO 40
            END IF
         IF (UVMAX.GT.0.0) THEN
            IF (ABS(U).GT.UVMAX) GO TO 40
            IF (ABS(V).GT.UVMAX) GO TO 40
         ELSE IF (UVMAX.LT.0.0) THEN
            IF (U*U+V*V.GT.UVMAX*UVMAX) GO TO 40
            END IF
C                                       Amplitude scaled to volts
      IF (LINEAR) THEN
         AMP = AMPSCL*AMP
C                                       Amplitude from power dB to volts
      ELSE
         AMP = 10.0**(AMP/20.0)
         END IF
      PHA = PHSGN*PHA * DG2RAD
      MVIS = MVIS + 1
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
      SUBROUTINE DFT (LAMBDA, N, DX, DY, V1, V2, P1, P2, VMAX, IERR)
C-----------------------------------------------------------------------
C   DFT does a direct Fourier transform of the holography visibility
C   data, producing amplitude and phase maps of the resolution function
C   as well as the aperture voltage distribution.
C   Inputs:
C      LAMBDA   R        Observing wavelength, in meters.
C      N        I        Number of pixels on a side of the map.
C      DX       R        Map cell spacing in X and Y, in meters.
C      DY       R        Map cell spacing in X and Y, in meters.
C   Outputs:
C      V1       R(N,N)   Amplitude of the aperture voltage distribution,
C                        reorganized for output.
C      V2       R(N,N)   Phase of the aperture voltage distribution,
C                        reorganized for output.
C      P1       R(N,N)   Amplitude of the point spread function,
C                        reorganized for output.
C      P2       R(N,N)   Phase of the point spread function, reorganized
C                        for output.
C      IERR     I        Error status, 0 means success.
C   Algorithm:
C      Direct Fourier transform of the visibility data.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   N, IERR
      REAL      LAMBDA, DX, DY, V1(N,N), V2(N,N), P1(N,N), P2(N,N), VMAX
C
      INCLUDE 'HOLOGP'
      INTEGER   IX, IY, MVIS, NX, NY, NVIS
C     REAL      CU, CV, PHI, RHO, RHO0, T1, T2, U, V, W, AMP, PHA, MU,
C    *   MV
      REAL      T1, T2, U, V, W, AMP, PHA
      DOUBLE PRECISION MU, MV, RHO, RHO0, PHI, CU, CV, DV1(MX,MX),
     *   DV2(MX,MX), DP1(MX,MX), DP2(MX,MX), DAMP, DPHA, DU, DV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
C                                       Zero the output arrays.
      DO 20 IY = 1,N
         DO 10 IX = 1,N
            DV1(IX,IY) = 0.0D0
            DV2(IX,IY) = 0.0D0
            DP1(IX,IY) = 0.0D0
            DP2(IX,IY) = 0.0D0
 10         CONTINUE
 20      CONTINUE
      VMAX = 0.0
C                                       Coordinates of the map centre.
      NX = N/2 + 1
      NY = N/2 + 1
      MU = -DX * TWOPI / LAMBDA
      MV = -DY * TWOPI / LAMBDA
C                                       Loop thru visibility data file
      NVIS = 0
 30   CALL HGETVS (NVIS, MVIS, U, V, W, AMP, PHA, IERR)
      IF (IERR.EQ.0) THEN
         DU = U
         DV = V
         DAMP = AMP
         DPHA = PHA
C                                       Radians per cell spacing in U
C                                       and V. (Negative for a minus-i
C                                       transform.)
C        CU = -DX * (2.0 * PI * U/LAMBDA)
C        CV = -DY * (2.0 * PI * V/LAMBDA)
         CU = MU * DU
         CV = MV * DV
C                                       Phase term for (IX,IY) = (0,0).
         RHO0 = -(CU * NX + CV * NY)
C                                       Loop over the map points.
         DO 50 IY = 1,N
            RHO = RHO0 + CV * IY
            DO 40 IX = 1,N
C                                       Point-spread function.
               RHO = RHO + CU
               DP1(IX,IY) = DP1(IX,IY) + COS(RHO)
               DP2(IX,IY) = DP2(IX,IY) + SIN(RHO)
C                                       Antenna pattern.
               PHI = DPHA + RHO
               DV1(IX,IY) = DV1(IX,IY) + DAMP * COS(PHI)
               DV2(IX,IY) = DV2(IX,IY) + DAMP * SIN(PHI)
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
      DO 110 IY = 1,N
         DO 100 IX = 1,N
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
               P2(IX,IY) = ATAN2 (T2, T1) * RAD2DG
               END IF
 100        CONTINUE
 110     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('DFT: Processed',I6,' visibilities.')
      END
      SUBROUTINE HOLFFT (DIR, N, VIS, AMP, PHA)
C-----------------------------------------------------------------------
C   HOLFFT does a fast Fourier transform of the visibility data for
C   HOLOG.
C   Inputs:
C      DIR      I        Sign of the complex phase in Fourier integral
C      N        I        Dimensions of the square array.
C   In/out:
C      VIS      CX(N,N)  Array containing the complex visibilities.
C                        Returned as the transpose of the Fourier
C                        transform of the input array.
C      AMP      R(N,N)   Arrays containing the amplitude and phase parts
C                        of the Fourier transform of the input array.
C      PHA      R(N,N)   Arrays containing the amplitude and phase parts
C                        of the Fourier transform of the input array.
C   Called: APLNOT: {FFTMC}
C   Algorithm:
C      Does the FFT on the input complex array then converts it to
C      amplitude and phase.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Dec.    Code last modified; 1989/Nov/02.
C-----------------------------------------------------------------------
      INTEGER    DIR, N
      COMPLEX    VIS(N,N)
      REAL       AMP(N,N), PHA(N,N)
C
      INTEGER    IU, IV
      REAL       CS, SN
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Do the FFT.
      CALL FFTMC (DIR, N, N, 1, VIS)
C                                       Extract the amplitude and phase.
      DO 20 IV = 1,N
         DO 10 IU = 1,N
            CS =  REAL (VIS(IU,IV))
            SN = AIMAG (VIS(IU,IV))
            AMP(IU,IV) = SQRT (CS*CS + SN*SN)
            IF ((SN.EQ.0.0) .AND. (CS.EQ.0.0)) THEN
               PHA(IU,IV) = 0.0
            ELSE
               PHA(IU,IV) = ATAN2 (SN, CS) * RAD2DG
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FTKRNL (KRNL, DU, KMAX, NPIX, FTK)
C-----------------------------------------------------------------------
C   FTKRNL computes the Fourier transform of the convolution function
C   used for interpolating data onto the map grid.  This may then be
C   used to apply the gridding correction in the map plane.
C   Inputs:
C      KRNL   R(*)   Array containing one half of the interpolation
C                    function.
C      DU     R      Separation between the elements of KRNL in units of
C                    the cell spacing.
C      KMAX   I      Location of the last non-zero element of KRNL.
C      NPIX   I      Number of pixels on a side of the output map.
C   Outputs:
C      FTK    R(*)   Fourier transform of KRNL evaluated at the map grid
C                    points.
C   Algorithm: Does an exact calculation as described by
C      Greisen, E.W., 1979. VLA Scientific Memorandum No. 131.
C   Author: Mark Calabretta, Australia Telescope.
C      Origin; 1988/Jan/22. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   KMAX, NPIX
      REAL      DU, FTK(0:*), KRNL(0:*)
C
      INTEGER   IX, IU
      REAL      X, T
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Loop over the map pixels.
      DO 20 IX = 0,NPIX/2
         X = REAL(IX)/REAL(NPIX)
         T = 2.0 * PI * DU * X
C                                       Do the cosine transform.
         FTK(IX) = KRNL(0)
         DO 10 IU = 1,KMAX
            FTK(IX) = FTK(IX) + 2.0 * KRNL(IU) * COS(IU*T)
 10         CONTINUE
C                                       Correct for the table lookup.
         IF (IX.NE.0) FTK(IX) = FTK(IX) * SIN(PI*DU*X)/(PI * DU * X)
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GRIDCR (XFTK, YFTK, NPIX, AMP, AMPMAX)
C-----------------------------------------------------------------------
C   GRIDCR applies the gridding correction to the amplitude map and
C   normalizes it to have a peak value of 1.0 volt.
C   Inputs:
C      XFTK     R(*)     The gridding correction in the X direction.
C      YFTK     R(*)     The gridding correction in the Y direction.
C      NPIX     I        Number of pixels on a side of the map.
C   in/out:
C      AMP      R(N,N)   The amplitude map.
C      AMPMAX   R        The normalisation constant
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Jan/22. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      AMP(NPIX,NPIX), AMPMAX, XFTK(0:*), YFTK(0:*)
C
      INTEGER   IX, IY, JX, JY, M
C-----------------------------------------------------------------------
C                                       Initialize.
      M = NPIX/2 + 1
      AMPMAX = -1.0
C                                       Apply the correction.
      DO 20 IY = 1,NPIX
         JY = ABS (IY-M)

         DO 10 IX = 1,NPIX
            JX = ABS (IX-M)
            AMP(IX,IY) = AMP(IX,IY) / (XFTK(JX) * YFTK(JY))
            AMPMAX = MAX (AMP(IX,IY), AMPMAX)
 10         CONTINUE
 20      CONTINUE
C                                       Normalize.
      DO 40 IY = 1,NPIX
         DO 30 IX = 1,NPIX
            AMP(IX,IY) = AMP(IX,IY) / AMPMAX
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BLANK (CELLXY, DIAM, SUBDIA, NPIX, MAP)
C-----------------------------------------------------------------------
C   BLANK blanks a map beyond the antenna diameter and beneath the
C   subreflector.
C   Inputs:
C      CELLXY   R        Map cell spacing, in meters.
C      DIAM     R        The actual antenna diameter, in meters.
C      SUBDIA   R        Sub-reflector diameter, in meters.
C      NPIX     I        Number of pixels on a side of the map.
C   In/out:
C      MAP      R(*,*)   The map to be blanked.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Apr/11. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      CELLXY, DIAM, SUBDIA, MAP(NPIX,NPIX)
C
      INTEGER   IX, IX0, IY, IY0
      REAL      R, RMIN, RMAX
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Apply pixel blanking.
      IX0  = NPIX/2 + 1
      IY0  = NPIX/2 + 1
      RMIN = (SUBDIA / 2.0) / CELLXY
      RMAX = (DIAM / 2.0) / CELLXY
      DO 20 IY = 1,NPIX
         DO 10 IX = 1,NPIX
            R = SQRT (REAL((IX-IX0)**2 + (IY-IY0)**2))
            IF ((R.GT.RMAX) .OR. (R.LT.RMIN)) MAP(IX,IY) = FBLANK
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE NITPIX (NPIX, PHA, PV)
C-----------------------------------------------------------------------
C   NITPIX attempts to reduce the potential energy in a phase map on a
C   pixel-by-pixel basis.
C   Inputs:
C      NPIX   I        Number of pixels on a side of the map.
C   Output:
C      PHA    R(*,*)   The adjusted phase map.
C   Algorithm:
C      Imagine that adjacent pixels are connected by "rubber bands" of
C      unstretched length 180 degrees.  The potential is computed as the
C      square of the length exceeding this.  Immediate neighbours both
C      vertically and horizontally are given unit weight.  Diagonally
C      adjacent pixels are given weight 1/2.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1988/Apr/11. Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   NPIX
      REAL      PHA(NPIX,NPIX), VTOT, PV(NPIX,NPIX)
C
      INTEGER   COUNT, IPHA, ITER, IX, IY, JX, JX1, JX2, JY, JY1, JY2
      REAL      PHA0, STRAIN, VPRV, VSUM
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Compute potential energy map
C                                       Initialize
      VTOT = 0.0
C                                       Loop over rows.
      DO 40 IY = 1, NPIX
C                                       Neighbouring pixels in y-dir
         JY1 = MAX (IY-1, 1)
         JY2 = MIN (IY+1, NPIX)
C                                       Scan down the row.
         DO 30 IX = 1,NPIX
C                                       Ignore blanked pixels.
            PV(IX,IY) = FBLANK
            IF (PHA(IX,IY).EQ.FBLANK) GO TO 30
C                                       Neighbouring pixels in x-dir
            JX1 = MAX (IX-1, 1)
            JX2 = MIN (IX+1, NPIX)
C                                       Look at each neighbour
            COUNT = 0
            VSUM  = 0.0
            PHA0  = PHA(IX,IY)
            DO 20 JY = JY1,JY2
               DO 10 JX = JX1,JX2
                  IF ((JX.EQ.IX) .AND. (JY.EQ.IY)) GO TO 10
C                                       Ignore blanked neighbours.
                  IF (PHA(JX,JY).EQ.FBLANK) GO TO 10
                  COUNT = COUNT + 1
C                                       Compute the potential energy
                  STRAIN = ABS(PHA0 - PHA(JX,JY)) - 180.0
                  IF (STRAIN.GT.0.0) THEN
                     IF ((JX.EQ.IX) .OR. (JY.EQ.IY)) THEN
                        VSUM = VSUM + STRAIN * STRAIN
                     ELSE
                        VSUM = VSUM + STRAIN * STRAIN / 2
                        END IF
                     END IF
 10               CONTINUE
 20            CONTINUE
            IF (COUNT.GT.0) THEN
               PV(IX,IY) = VSUM
               VTOT = VTOT + VSUM
               END IF
 30         CONTINUE
 40      CONTINUE
C                                       Start nit-picking.  Allow up to
C                                       NPIX iterations.
      VPRV = 2.0*VTOT
      DO 120 ITER = 1,NPIX
C                                       Quit if the energy was not
C                                       reduced.
         IF (VTOT.GE.VPRV) GO TO 999
         VPRV = VTOT
C                                       Loop for each row.
         DO 110 IY = 1,NPIX
C                                       Neighbouring pixels in y-dir
            JY1 = MAX (IY-1, 1)
            JY2 = MIN (IY+1, NPIX)
C                                       Scan down the row.
            DO 100 IX = 1,NPIX
C                                       Ignore blanked or zero energy
C                                       pixels
               IF (PV(IX,IY).EQ.0.0)  GO TO 100
               IF (PV(IX,IY).EQ.FBLANK) GO TO 100
C                                       Neighbouring pixels in x-dir
               JX1 = MAX (IX-1, 1)
               JX2 = MIN (IX+1, NPIX)
C                                       Add or subtract 360 degrees and
C                                       recompute the energy
               DO 90 IPHA = -1,1,2
                  PHA0 = PHA(IX,IY) + 360.0*IPHA
C                                       Look at each neighbour.
                  VSUM = 0.0
                  DO 60 JY = JY1,JY2
                     DO 50 JX = JX1,JX2
                        IF ((JX.EQ.IX) .AND. (JY.EQ.IY)) GO TO 50
C                                       Ignore blanked neighbours.
                        IF (PHA(JX,JY).EQ.FBLANK) GO TO 50
C                                       Compute the potential energy
                        STRAIN = ABS(PHA0 - PHA(JX,JY)) - 180.0
                        IF (STRAIN.GT.0.0) THEN
                           IF ((JX.EQ.IX) .OR. (JY.EQ.IY)) THEN
                              VSUM = VSUM + STRAIN * STRAIN
                           ELSE
                              VSUM = VSUM + STRAIN * STRAIN / 2
                              END IF
                           END IF
 50                     CONTINUE
 60                  CONTINUE
C                                       Update the maps if the energy
C                                       was reduced
                  IF (VSUM.LT.PV(IX,IY)) THEN
                     DO 80 JY = JY1,JY2
                        DO 70 JX = JX1,JX2
                           IF ((JX.EQ.IX) .AND. (JY.EQ.IY)) GO TO 70
C                                       Ignore blanked neighbours
                           IF (PHA(JX,JY).EQ.FBLANK) GO TO 70
C                                       Compute old potential energy
                           STRAIN = ABS(PHA(IX,IY) - PHA(JX,JY)) - 180.0
                           IF (STRAIN.GT.0.0) THEN
                              IF ((JX.EQ.IX) .OR. (JY.EQ.IY)) THEN
                                 PV(JX,JY) = PV(JX,JY) - STRAIN*STRAIN
                              ELSE
                                 PV(JX,JY) = PV(JX,JY) - STRAIN*STRAIN/2
                                 END IF
                              END IF
C                                       Compute new potential energy
                           STRAIN = ABS(PHA0 - PHA(JX,JY)) - 180.0
                           IF (STRAIN.GT.0.0) THEN
                              IF ((JX.EQ.IX) .OR. (JY.EQ.IY)) THEN
                                 PV(JX,JY) = PV(JX,JY) + STRAIN*STRAIN
                              ELSE
                                 PV(JX,JY) = PV(JX,JY) + STRAIN*STRAIN/2
                                 END IF
                              END IF
 70                        CONTINUE
 80                     CONTINUE
                     PHA(IX,IY) = PHA0
                     VTOT = VTOT - 2.0*(PV(IX,IY) - VSUM)
                     PV(IX,IY) = VSUM
                     GO TO 100
                     END IF
 90               CONTINUE
 100           CONTINUE
 110        CONTINUE
 120     CONTINUE
 999  RETURN
      END
      SUBROUTINE FLATPH (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOPNT, PHAMOD)
C-----------------------------------------------------------------------
C   FLATPH corrects the grading phase for pointing, focus, and feed
C   offset errors using least squares.
C   Inputs:
C      NPIX     I        Number of pixels on a side of the map.
C      LAMBDA   R        Observing wavelength, in meters.
C      FOCUS    R        Nominal focal length, in meters.
C      XYMIN    R        Range of |x| and |y| used in correcting for
C      XYMAX    R        pointing, focus, and feed offset. Negative
C                        values denote a range of SQRT(x*x + y*y).
C      CELLXY   R        Map cell spacing, in meters.
C      VAMP     R(*,*)   Grading amplitude map.
C      VPHA     R(*,*)   Grading phase map.
C      NOXY     L        Disable fit for subreflector offset
C      NOPNT    L        Disable phase gradient fit (pointing offset)
C      NOFOC    L        Disable fit for subreflector focus (z)
C   Output:
C      P0       R        Constant offset removed, degrees.
C      PX       R        Least squares estimates of the phase ramp
C      PY       R        in the X and Y directions, in degrees / cell
C      FX       R        The derived focal position is at
C      FY       R        (FX,FY,FOCUS+FZ), millimeters.
C      FZ       R
C      DP0      R        Standard error in P0.
C      DPX      R        Standard error in PX
C      DPY      R        Standard error in PY
C      DFX      R        Standard error in FX
C      DFY      R        Standard error in FY
C      DFZ      R        Standard error in FZ
C      RMS      R        Weighted Half-path rms error, in millimeters.
C      RMS0     R        Pre-fit weighted half-path error, mm.
C      IERR     I        Error status, 0 means success.
C   Algorithm: Weighted least squares fit.
C   Notes:
C      1)  Feed offset inhibited if NOXY = .true.
C      2)  Feed focus inhibited if NOFOC = .true.
C      3)  Pointing offset inhibited if NOPNT = .true.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1989/Nov/01. mjk, 28/1/93
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, PHAMOD(NPIX,NPIX)
      LOGICAL   NOXY, NOFOC, NOPNT
C
      INTEGER   NP
      PARAMETER (NP=6)
      INTEGER   I, IDR2, IDX, IDY, IX, IX0, IR2MAX, IR2MIN, IXYMAX,
     *   IXYMIN, IY, IY0, J
      REAL      CORR, FIT, FP, M(NP,NP), NS, PH, R(NP), SUM, SSQ,
     *   SSQRES, VARRES, VARY, VX(NP), WT, X(NP), XF, XP, YF, YP, ZF,
     *   R4, MEAN, RAD, RR, ANG, Q, DENOM
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
      IXYMIN = ABS (XYMIN / CELLXY)
      IXYMAX = ABS (XYMAX / CELLXY)
      IR2MIN = (XYMIN * XYMIN) / (CELLXY * CELLXY)
      IR2MAX = (XYMAX * XYMAX) / (CELLXY * CELLXY)
C                                       Focal length in cellular units.
      FP = FOCUS/CELLXY
C                                       Half-path wavelength scaling
      R4 = LAMBDA / 720.0
      NS  = 0.0
      SUM = 0.0
      SSQ = 0.0
      DO 20 I = 1,NP
         R(I) = 0.0
         DO 10 J = 1,NP
            M(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
      RR = NPIX/2. + 1.
C                                       Compute pre-fit rms error
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *  VAMP, VPHA, R4, MEAN, RMS0)
C                                       Loop through the map.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 40 IY = 1,NPIX
         IDY = ABS (IY - IY0)
C                                       Check absolute limits.
         IF ((XYMIN.GT.0.0) .AND. (IDY.LT.IXYMIN)) GO TO 40
         IF ((XYMAX.GT.0.0) .AND. (IDY.GT.IXYMAX)) GO TO 40
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF (XYMAX.LT.0.0 .AND. IDY*IDY.GT.IR2MAX) GO TO 40
         DO 30 IX = 1,NPIX
C                                       Ignore blanked pixels.
            IF (VPHA(IX,IY).EQ.FBLANK) GO TO 30
C                                       Check for inclusion.
            IDX  = ABS(IX-IX0)
            IDR2 = IDX*IDX + IDY*IDY
C                                       Inner limits.
            IF (XYMIN.GT.0.0) THEN
               IF (IDX.LT.IXYMIN) GO TO 30
            ELSE IF (XYMIN.LT.0.0) THEN
               IF (IDR2.LT.IR2MIN) GO TO 30
               END IF
C                                       Outer limits.
            IF (XYMAX.GT.0.0) THEN
               IF (IDX.GT.IXYMAX) GO TO 30
            ELSE IF (XYMAX.LT.0.0) THEN
               IF (IDR2.GT.IR2MAX) GO TO 30
               END IF
C                                       Evaluate variables (in cellular
C                                       units).
            PH = VPHA(IX,IY)
            WT = VAMP(IX,IY)
            XP = IX - IX0
            YP = IY - IY0
            RAD = SQRT(XP*XP+YP*YP)
            ANG = ATAN2(YP,XP)
            Q = RAD/(2.*FP)
            DENOM = 1. + Q*Q
            XF = -2.*Q*COS(ANG)/DENOM
            YF = -2.*Q*SIN(ANG)/DENOM
            ZF = (1.-Q*Q)/DENOM
C                                       Accumulate statistics.
            NS     = NS  + WT
            SUM    = SUM + PH * WT
            SSQ    = SSQ + PH * PH * WT
            R(1)   = R(1) + PH * WT
            R(2)   = R(2) + PH * XP * WT
            R(3)   = R(3) + PH * YP * WT
            R(4)   = R(4) + PH * XF * WT
            R(5)   = R(5) + PH * YF * WT
            R(6)   = R(6) + PH * ZF * WT
            M(1,1) = M(1,1) + WT
            M(1,2) = M(1,2) + XP * WT
            M(1,3) = M(1,3) + YP * WT
            M(1,4) = M(1,4) + XF * WT
            M(1,5) = M(1,5) + YF * WT
            M(1,6) = M(1,6) + ZF * WT
            M(2,2) = M(2,2) + XP * XP * WT
            M(2,3) = M(2,3) + XP * YP * WT
            M(2,4) = M(2,4) + XP * XF * WT
            M(2,5) = M(2,5) + XP * YF * WT
            M(2,6) = M(2,6) + XP * ZF * WT
            M(3,3) = M(3,3) + YP * YP * WT
            M(3,4) = M(3,4) + YP * XF * WT
            M(3,5) = M(3,5) + YP * YF * WT
            M(3,6) = M(3,6) + YP * ZF * WT
            M(4,4) = M(4,4) + XF * XF * WT
            M(4,5) = M(4,5) + XF * YF * WT
            M(4,6) = M(4,6) + XF * ZF * WT
            M(5,5) = M(5,5) + YF * YF * WT
            M(5,6) = M(5,6) + YF * ZF * WT
            M(6,6) = M(6,6) + ZF * ZF * WT
 30         CONTINUE
 40      CONTINUE
C                                       Disable the focus and feed
C                                       offset if requested
      IF (NOXY) THEN
        DO 43 I = 4,5
           R(I) = 0.
           DO 42 J = 1,NP
              M(J,I) = 0.
              M(I,J) = 0.
 42        CONTINUE
 43     CONTINUE
      END IF
      IF (NOFOC) THEN
        R(6) = 0.
        DO 44 J = 1,NP
           M(J,6) = 0.
           M(6,J) = 0.
 44     CONTINUE
      END IF
      IF (NOPNT) THEN
        DO 45 I = 2,3
           R(I) = 0.
           DO 46 J = 1,NP
              M(J,I) = 0.
              M(I,J) = 0.
 46        CONTINUE
 45     CONTINUE
      END IF
C                                       Compute least squares solution.
      CALL LEASQR (NP, NS, SUM, SSQ, R, M, X, VX, SSQRES, VARRES, VARY,
     *   FIT, IERR)
C                                       Pick up answers
      P0 = X(1)
      PX = X(2)
      PY = X(3)
      FX = X(4)
      FY = X(5)
      FZ = X(6)
      DP0 = SQRT (VX(1))
      DPX = SQRT (VX(2))
      DPY = SQRT (VX(3))
      DFX = LAMBDA * SQRT (VX(4))/.36
      DFY = LAMBDA * SQRT (VX(5))/.36
      DFZ = LAMBDA * SQRT (VX(6))/.36
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP+YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               DENOM = 1. + Q*Q
               XF = -2.*Q*COS(ANG)/DENOM
               YF = -2.*Q*SIN(ANG)/DENOM
               ZF = (1.-Q*Q)/DENOM
               CORR = P0 + PX*XP + PY*YP + FX*XF + FY*YF + FZ*ZF
               VPHA(IX,IY) = VPHA(IX,IY) - CORR
               PHAMOD(IX,IY) = CORR
               END IF
 50         CONTINUE
 60      CONTINUE
C                                  Rescale feed offsets to millimeters
      FX = LAMBDA * FX/.36
      FY = LAMBDA * FY/.36
      FZ = LAMBDA * FZ/.36
C                                 Rescale phase slope to offset angle
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                                       Compute the weighted half-path
C                                       error (as RMS)
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *  VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE FLATMO (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOPNT, PHAMOD, VPARM)
C-----------------------------------------------------------------------
C   FLATPH corrects the grading phase for pointing, focus, and feed
C   offset errors using the users values
C   Inputs:
C      NPIX     I        Number of pixels on a side of the map.
C      LAMBDA   R        Observing wavelength, in meters.
C      FOCUS    R        Nominal focal length, in meters.
C      XYMIN    R        Range of |x| and |y| used in correcting for
C      XYMAX    R        pointing, focus, and feed offset. Negative
C                        values denote a range of SQRT(x*x + y*y).
C      CELLXY   R        Map cell spacing, in meters.
C      VAMP     R(*,*)   Grading amplitude map.
C      VPHA     R(*,*)   Grading phase map.
C      NOXY     L        Disable fit for subreflector offset
C      NOPNT    L        Disable phase gradient fit (pointing offset)
C      NOFOC    L        Disable fit for subreflector focus (z)
C      VPARM    R(6)     p0,px,py,fx,fy,fz
C   Output:
C      P0       R        Constant offset removed, degrees.
C      PX       R        Least squares estimates of the phase ramp
C      PY       R        in the X and Y directions, in degrees / cell
C      FX       R        The derived focal position is at
C      FY       R        (FX,FY,FOCUS+FZ), millimeters.
C      FZ       R
C      DP0      R        Standard error in P0.
C      DPX      R        Standard error in PX
C      DPY      R        Standard error in PY
C      DFX      R        Standard error in FX
C      DFY      R        Standard error in FY
C      DFZ      R        Standard error in FZ
C      RMS      R        Weighted Half-path rms error, in millimeters.
C      RMS0     R        Pre-fit weighted half-path error, mm.
C      IERR     I        Error status, 0 means success.
C   Algorithm: Weighted least squares fit.
C   Notes:
C      1)  Feed offset inhibited if NOXY = .true.
C      2)  Feed focus inhibited if NOFOC = .true.
C      3)  Pointing offset inhibited if NOPNT = .true.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1989/Nov/01. mjk, 28/1/93
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, PHAMOD(NPIX,NPIX), VPARM(*)
      LOGICAL   NOXY, NOFOC, NOPNT
C
      INTEGER   IX, IX0, IR2MAX, IR2MIN, IXYMAX, IXYMIN, IY, IY0, NA
      REAL      CORR, FP, XF, XP, YF, YP, ZF, R4, MEAN, RAD, ANG, Q,
     *   DENOM, AVGAMP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
      IXYMIN = ABS (XYMIN / CELLXY)
      IXYMAX = ABS (XYMAX / CELLXY)
      IR2MIN = (XYMIN * XYMIN) / (CELLXY * CELLXY)
      IR2MAX = (XYMAX * XYMAX) / (CELLXY * CELLXY)
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      RMS0 = -1
C                                       Focal length in cellular units.
      FP = FOCUS/CELLXY
      R4 = LAMBDA / 720.0
C                                       Pick up answers
      P0 = VPARM(1)
      PX = VPARM(2)*CELLXY*360./LAMBDA/57.296/60.0
      PY = VPARM(3)*CELLXY*360./LAMBDA/57.296/60.0
      FX = VPARM(4) * 0.36 / LAMBDA
      FY = VPARM(5) * 0.36 / LAMBDA
      FZ = VPARM(6) * 0.36 / LAMBDA
      DP0 = 0.0
      DPX = 0.0
      DPY = 0.0
      DFX = 0.0
      DFY = 0.0
      DFZ = 0.0
C                                       average amp
      IF (VPARM(10).GT.0.0) THEN
         NA = 0
         AVGAMP = 0.0
         DO 20 IY = 1,NPIX
            DO 10 IX = 1,NPIX
               IF ((VPHA(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).GE.VPARM(10))) THEN
                  AVGAMP = AVGAMP + VAMP(IX,IY)
                  NA = NA + 1
                  END IF
 10            CONTINUE
 20         CONTINUE
         IF (NA.GT.0) AVGAMP = AVGAMP / NA
         END IF
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP+YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               DENOM = 1. + Q*Q
               XF = -2.*Q*COS(ANG)/DENOM
               YF = -2.*Q*SIN(ANG)/DENOM
               ZF = (1.-Q*Q)/DENOM
               CORR = P0 + PX*XP + PY*YP + FX*XF + FY*YF + FZ*ZF
               IF (VPARM(9).GT.0.0) THEN
                  VPHA(IX,IY) = CORR
               ELSE
                  VPHA(IX,IY) = VPHA(IX,IY) - CORR
                  END IF
               IF ((VPARM(10).GT.0.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *            THEN
                  IF (VAMP(IX,IY).LT.VPARM(10)) THEN
                     VAMP(IX,IY) = 0.0
                  ELSE
                     VAMP(IX,IY) = AVGAMP
                     END IF
                  END IF
               PHAMOD(IX,IY) = CORR
            ELSE IF ((VPARM(10).GT.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *         THEN
               VAMP(IX,IY) = 0.0
               END IF
 50         CONTINUE
 60      CONTINUE
C                                  Rescale feed offsets to millimeters
      FX = LAMBDA * FX/.36
      FY = LAMBDA * FY/.36
      FZ = LAMBDA * FZ/.36
C                                 Rescale phase slope to offset angle
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                                       Compute the weighted half-path
C                                       error (as RMS)
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *  VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE SURDEV (CELLXY, LAMBDA, FOCUS, NPIX, VPHA, DEV)
C-----------------------------------------------------------------------
C   SURDEV computes the surface deviation map from the phase of the
C   grading function.
C   Inputs:
C      CELLXY   R        Map cell spacing, in meters.
C      LAMBDA   R        Observing wavelength, in meters.
C      FOCUS    R        Focal length, in meters.
C      NPIX     I        Number of pixels on a side of the map.
C      VPHA     R(*,*)   Grading phase map.
C   Output:
C      DEV      R(*,*)   Surface deviation map.
C   Algorithm:
C      The pathlength seen by an incoming signal is phase shifted on
C      both incidence and reflection from the antenna surface. Moreover,
C      away from the collimation axis of the antenna, the path deviates
C      from the normal to the antenna surface, this being the direction
C      in which the displacement is required.
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
      A = FOCUS / CELLXY
      B = (LAMBDA/360.0) / (4.0*A)
      C = 4.0 * A * A
C                                       Loop over the map.
      DO 20 IY = 1,NPIX
         DO 10 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               R = SQRT (REAL ((IX-IX0)**2 + (IY-IY0)**2))
               DEV(IX,IY) = B * VPHA(IX,IY) * SQRT (R*R + C)
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
     *   RDAT, HISTRY, NHIST, MVIS, ISTOKE, IERR)
C-----------------------------------------------------------------------
C   HOLOUT saves the maps as AIPS image files with history as well.
C   Inputs:
C      OUTNAM   C*36      WAWA image namestring.
C      OBJECT   C*8       Object name.
C      TELESC   C*8       Telescope name.
C      INSTRM   C*8       Instrument name (receiver, correlator).
C      OBSERV   C*8       Observers
C      DATOBS   C*8       Observation date.
C      DATMAP   C*8       Map date.
C      FREQ     R         Frequency in GHz
C      UNITS    C*8       Map units.
C      AXIS1    C*8       Title for axis 1.
C      NX1      I         Number of pixels on axis 1.
C      PINC1    R         Pixel increment on axis 1.
C      AXIS2    C*8       Title for axis 2.
C      NX2      I         Number of pixels on axis 2.
C      PINC2    R         Pixel increment on axis 2.
C      RDAT     R(*,*)    Array containing the image.
C      HISTRY   C*72(*)   Internal file containing history text.
C      NHIST    I         Number of records in HISTRY.
C      MVIS     I         Number of vis used
C      ISTOKE   I         Stokes value 0 -> none
C   Output:
C      IERR     I         Error status, 0 means success.
C   Author: Mark Calabretta, Australia Telescope.
C   Origin; 1987/Nov.    Code last modified; 1992/Mar/12
C-----------------------------------------------------------------------
      CHARACTER OUTNAM*36, OBJECT*8, TELESC*8, INSTRM*8, OBSERV*8,
     *   DATOBS*8, DATMAP*8, UNITS*8, AXIS1*8, AXIS2*8, HISTRY(*)*(*)
      INTEGER   NX1, NX2, NHIST, MVIS, ISTOKE, IERR
      REAL      FREQ, PINC1, PINC2, RDAT(NX1,NX2)
C
      INCLUDE 'HOLOGP'
      INTEGER   BUFF(256), CNO, HILUN, I1, I2, IRET, ISEQ, IUSER, IVOL,
     *   NH, NHISTF, OLUN, NUMKEY, LOCS, KEYTYP, BUFF2(256)
      REAL      VMAX, VMIN, TBUFF(MX)
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
      IF (ISTOKE.NE.0) CATBLK(KIDIM) = 4
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       First axis parameters.
      CALL CHR2H (8, AXIS1, 1, CATH(KHCTP))
      CATD(KDCRV) = 0.0D0
      CATR(KRCIC) = PINC1
C                                       reverse axis
C                                       look from front
      CATR(KRCRP) = NX1 - NX1/2
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
C                                       Fourth axis parameters.
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+6))
      CATD(KDCRV+3) = ISTOKE
      CATR(KRCIC+3) = 1.0
      CATR(KRCRP+3) = 1.0
      CATBLK(KINAX+3) = 1
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
      DO 50 I2 = 1,NX2
C                                       Find the maximum and minimum.
         DO 30 I1 = 1,NX1
            IF (RDAT(I1,I2).NE.FBLANK) THEN
               IF (RDAT(I1,I2).LT.VMIN) VMIN = RDAT(I1,I2)
               IF (RDAT(I1,I2).GT.VMAX) VMAX = RDAT(I1,I2)
               END IF
C                                       reverse axis
C                                       look from front
            TBUFF(NX1+1-I1) = RDAT(I1,I2)
 30         CONTINUE
C                                       Write it to the AIPS file.
         CALL MAPIO ('WRIT', OLUN, TBUFF, IERR)
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
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATING HISTORY FILE'
         GO TO 990
         END IF
      CALL HENCOO (TSKNAM, NAME, CLASS, ISEQ, IVOL, HILUN, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'ADDING IMAGE NAME TO HISTORY'
         GO TO 990
         END IF
C                                       Add history text.
      DO 100 NH = 1,NHIST
         IF (HISTRY(NH).NE.' ') THEN
            CALL HIADD (HILUN, HISTRY(NH), BUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITING TO HISTORY FILE'
               GO TO 990
               END IF
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
      SUBROUTINE BEAM (N, NLG, CMPRS, VAMP, VPHA, AAMP, APHA, VCPLX,
     *   WAMP, WPHA)
C-----------------------------------------------------------------------
C   BEAM evaluates the antenna power pattern.
C   Inputs:
C      N       I        Number of pixels on a side of the map.
C      NLG     I        Number of pixels on a side of the beam fields
C      CMPRS   I        Select every CMPRS cells from VAMP,VPHA
C      VAMP    R(N,N)   The amplitude of the voltage distribution across
C                       the antenna aperture.
C      VPHA    R(N,N)   The phase of the voltage distribution across
C                       the antenna aperture.
C   Outputs:
C      AAMP    R(N,N)   The amplitude of the antenna pattern.
C      APHA    R(N,N)   The phase of the antenna pattern.
C      VCPLX   C(*,*)   Complex array for beam computation
C      WAMP    R(*,*)   Real array for beam amplitude
C      WPHA    R(*,*)   Real array for beam phase
C   Called:
C      {HOLFFT}
C   Algorithm:
C      Interpolation is done by compressing the voltage distribution map
C      and imbedding it in a larger array.  This is effectively a
C      multiplication by a pillbox function, and corresponds to a
C      convolution by a sinc function in the Fourier transform domain.
C   Author: Mike Kesteven, Australia Telescope.
C   Origin; 1987/Dec.    Code last modified; 1989/Nov/01.
C-----------------------------------------------------------------------
      INTEGER   N, NLG, CMPRS
      REAL      VAMP(N,N), VPHA(N,N), AAMP(N,N), APHA(N,N),
     *   WAMP(NLG,NLG), WPHA(NLG,NLG)
      COMPLEX    VCPLX(NLG,NLG)
C
      INTEGER    IX, IY, JX, JY, K
      REAL       CS, SHFTX, SHFTY, SN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize.
      DO 20 JY = 1,NLG
         DO 10 JX = 1,NLG
            VCPLX(JX,JY) = (0.0,0.0)
 10         CONTINUE
 20      CONTINUE
C                                       Compress and imbed the NxN map
C                                       in a NLGxNLG array
      K = NLG - N/(2*CMPRS)
      JY = K
      DO 40 IY = 1,N,CMPRS
         JY = JY + 1
         IF (JY.GT.NLG) JY = JY - NLG
         JX = K
         DO 30 IX = 1,N,CMPRS
            JX = JX + 1
            IF (JX.GT.NLG) JX = JX - NLG
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               CS = VAMP(IX,IY) * COS (VPHA(IX,IY)*DG2RAD)
               SN = VAMP(IX,IY) * SIN (VPHA(IX,IY)*DG2RAD)
               VCPLX(JX,JY) = CMPLX(CS,SN)
               END IF
 30         CONTINUE
 40      CONTINUE
C                                       Apply a phase gradient to center
C                                       the beam.
      SHFTY = 1.0
      DO 60 JY = 1,NLG
         SHFTX = SHFTY
         DO 50 JX = 1,NLG
            VCPLX(JX,JY) = SHFTX * VCPLX(JX,JY)
            SHFTX = -SHFTX
 50         CONTINUE
         SHFTY = -SHFTY
 60      CONTINUE
C                                       Transform.
      CALL HOLFFT (1, NLG, VCPLX, WAMP, WPHA)
C                                       Extract the beam, convert to dB
      K = (NLG - N) / 2
      DO 80 IY = 1,N
         JY = K + IY
         DO 70 IX = 1,N
            JX = K + IX
            AAMP(IX,IY) = WAMP(JX,JY)
            APHA(IX,IY) = WPHA(JX,JY)
            IF (AAMP(IX,IY).LE.0.0) THEN
               AAMP(IX,IY) = FBLANK
            ELSE
               AAMP(IX,IY) = 20.0 * LOG10 (AAMP(IX,IY))
               END IF
 70         CONTINUE
 80      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ANGAIN (N, MAPSZ, DIAM, SUBDIA, LAMBDA, VAMP, VPHA,
     *   F1, MGAIN, TGAIN)
C-----------------------------------------------------------------------
C   ANGAIN computes the gain of an antenna given the aperture voltage
C   distribution.
C   Inputs:
C      N        I        Number of pixels on a side of the map.
C      MAPSZ    R        Size of the map, in meters.
C      DIAM     R        Antenna diameter, in meters.
C      SUBDIA   R        sub-reflector diameter, in m
C      LAMBDA   R        Wavelength of the observation, in meters.
C      VAMP     R(N,N)   amplitude of the voltage distribution across
C                        the antenna aperture.
C      VPHA     R(N,N)   phase of the voltage distribution across the
C                        antenna aperture.
C      F1       R        Scaling factor used to extrapolate in freq
C   Outputs:
C      MGAIN    R        Measured gain, in dB.
C      TGAIN    R        Theoretical gain, in dB.
C   Author: Mike Kesteven, Australia Telescope.
C   Origin; 1987/Nov/11  Code last modified; 1992/Mar/12
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      MAPSZ, DIAM, SUBDIA, LAMBDA, VAMP(N,N), VPHA(N,N), F1,
     *   MGAIN, TGAIN
C
      INTEGER   IX, IX0, IY, IY0, R
      REAL      AMAX, CELLSZ, CS1, CS2, FACT, RMAX, SN1, SN2, RMIN
      COMPLEX   INT1, INT2, INT3, INT4, TMP1, TMP2
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Initialize
      CELLSZ = MAPSZ / N
      IX0 = N/2 + 1
      IY0 = N/2 + 1
      RMAX = (N / MAPSZ) * (DIAM / 2.0)
      RMIN = (N / MAPSZ) * (SUBDIA / 2.0)
      INT1 = (0.0, 0.0)
      INT2 = (0.0, 0.0)
      INT3 = (0.0, 0.0)
      INT4 = (0.0, 0.0)
C                                       Integrate.
      DO 20 IY = 1,N
         DO 10 IX = 1,N
            R = SQRT (REAL((IX-IX0)**2) + REAL((IY-IY0)**2))
            IF ((R.LE.RMAX) .AND. (R.GE.RMIN)) THEN
               CS1 = VAMP(IX,IY) * COS (F1*VPHA(IX,IY)*DG2RAD)
               SN1 = VAMP(IX,IY) * SIN (F1*VPHA(IX,IY)*DG2RAD)
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
      FACT = CELLSZ / LAMBDA
      AMAX  = INT1 * CONJG (INT1) / INT2
      AMAX  = AMAX * FACT * FACT
      MGAIN = AMAX * 4.0 * PI
      MGAIN = 10.0 * LOG10 (MGAIN)
      AMAX  = INT3 * CONJG (INT3) / INT4
      AMAX  = AMAX * FACT * FACT
      TGAIN = AMAX * 4.0 * PI
      TGAIN = 10.0 * LOG10 (TGAIN)
C
 999  RETURN
      END
      SUBROUTINE FLTPH3 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOTILT, NOPNT, XMAG,
     *   PHAMOD, TX, TY, DTX, DTY)
C-----------------------------------------------------------------------
C     FLATPH2 corrects the grading phase for pointing, focus, and feed
C     offset errors using least squares, and a model incorporating
C     subreflector position errors.  Includes reference pointing
C
C  This is a revised version of the task, offering a two-reflector
C  solution.  M. Kesteven, 6/12/1994
C
C  The formulation is in terms of the Ruze expressions (the unpublished
C  lecture notes : Small Displacements in Parabolic Antennas, 1969).
C
C  At present, this requires the magnification to be HARDWIRED -
C  see the data statement.
C
C     Given:
C          NPIX        I     Number of pixels on a side of the map.
C          LAMBDA      R     Observing wavelength, in meters.
C          FOCUS       R     Nominal focal length, in meters.
C          XYMIN       R     Range of |x| and |y| used in correcting for
C      and XYMAX       R     pointing, focus, and feed offset. Negative
C                            values denote a range of SQRT(x*x + y*y).
C          CELLXY      R     Map cell spacing, in meters.
C          VAMP(N,N)   R     Grading amplitude map.
C          VPHA(N,N)   R     Grading phase map.
C          NOPNT       L     Disable phase slope (pointing offset)
C          NOXY        L     Disable subreflector offset model
C          NOFOC       L     Disable subreflector focus (z) model
C          NOTILT      L     Enable subreflector rotation model.
C
C     Returned:
C          P0          R     Constant offset removed, degrees.
C          PX,PY       R     Least squares estimates of the phase ramp
C                            in the X and Y directions, in degrees per
C                            cell.
C          FX,FY,FZ    R     The derived focal position is at
C                            (FX,FY,FOCUS+FZ), millimeters.
C          TX,TY       R     Tilt of subreflector in X, Y axes.
C          DP0         R     Standard error in P0.
C          DPX,DPY     R     Standard error in PX, and PY.
C          DFX,DFY,DFZ R     Standard error in FX, FY, and PZ.
C          DTX,DTY     R     Standard error in TX, TY.
C          RMS         R     Weighted Half-path rms error, in mm.
C          RMS0        R     Pre-fit weighted half-path error, mm.
C          IERR        I     Error status, 0 means success.
C          PHMOD(N,N)  R     Model phase, due to subref. offsets
C          VPHA(N,N)   R     Phase map corrected for subr. offsets.
C
C     Called:
C          APLNOT: {LEASQR}
C
C     Algorithm:
C          Weighted least squares fit.
C
C     Notes:
C       1)  Subreflector offset inhibited if NOXY = .true.
C       2)  Subreflector focus model inhibited if NOFOC = .true.
C       3)  Subreflector tilt inhibited if NOTILT = .true.
C       4)  Phase slope (pointing offset) inhibited if NOPNT = .true.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C          mjk, 28/1/93
C          RAP, 27/05/08
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, XMAG, PHAMOD(NPIX,NPIX),TX, TY,
     *   DTX, DTY
      LOGICAL   NOXY, NOFOC, NOTILT, NOPNT
C
      INTEGER   NP
      PARAMETER (NP=8)
      INTEGER   I, IDR2, IDX, IDY, IX, IX0, IR2MAX, IR2MIN, IXYMAX,
     *   IXYMIN, IY, IY0, J
      REAL      CORR, FIT, FP, M(NP,NP), NS, PH, R(NP), SUM, SSQ,
     *   SSQRES, VARRES, VARY, VX(NP), WT, X(NP), XF, XP, YF, YP, ZF,
     *   R4, MEAN, RAD, RR, MAG, FEQUIV, ANG, Q, QP, DENOM, DENOMP,
     *   XT, YT, XQ
      INCLUDE 'INCS:DDCH.INC'
      DATA      MAG/10.0/
C-----------------------------------------------------------------------
C                                       Initialize.
      IF (XMAG.LE.0.0) XMAG = MAG
      IXYMIN = ABS(XYMIN/CELLXY)
      IXYMAX = ABS(XYMAX/CELLXY)
      IR2MIN = (XYMIN*XYMIN)/(CELLXY*CELLXY)
      IR2MAX = (XYMAX*XYMAX)/(CELLXY*CELLXY)
C                                       Focal length in cellular units
      FP = FOCUS/CELLXY
      FEQUIV = XMAG * FP
C                                       Half-path wavelength scaling
      R4 = LAMBDA / 720.0
      NS  = 0.0
      SUM = 0.0
      SSQ = 0.0
      DO 20 I = 1,NP
         R(I) = 0.0
         DO 10 J = 1,NP
            M(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
      RR = NPIX/2. + 1.
C                                       Calculate pre-fit rms.
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS0)
C                                       Loop through the map.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 40 IY = 1,NPIX
         IDY = ABS(IY-IY0)
C                                       Check absolute limits.
         IF (XYMIN.GT.0.0 .AND. IDY.LT.IXYMIN) GO TO 40
         IF (XYMAX.GT.0.0 .AND. IDY.GT.IXYMAX) GO TO 40
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF ((XYMAX.LT.0.0) .AND. (IDY*IDY.GT.IR2MAX)) GO TO 40
         DO 30 IX = 1,NPIX
C                                       Ignore blanked pixels.
            IF (VPHA(IX,IY).EQ.FBLANK) GO TO 30
C                                       Check for inclusion.
            IDX  = ABS(IX-IX0)
            IDR2 = IDX*IDX + IDY*IDY
C                                       Inner limits.
            IF (XYMIN.GT.0.0) THEN
               IF (IDX.LT.IXYMIN) GO TO 30
            ELSE IF (XYMIN.LT.0.0) THEN
               IF (IDR2.LT.IR2MIN) GO TO 30
               END IF
C                                       Outer limits.
            IF (XYMAX.GT.0.0) THEN
               IF (IDX.GT.IXYMAX) GO TO 30
            ELSE IF (XYMAX.LT.0.0) THEN
               IF (IDR2.GT.IR2MAX) GO TO 30
               END IF
C                                       Evaluate variables (in cells)
            PH = VPHA(IX,IY)
            WT = VAMP(IX,IY)
            XP = IX - IX0
            YP = IY - IY0
            RAD = SQRT(XP*XP + YP*YP)
            ANG = ATAN2(YP,XP)
            Q = RAD/(2.*FP)
            QP = Q/XMAG
            DENOM = 1.+Q*Q
            DENOMP = 1.+QP*QP
C           XQ = (0.3 - 0.7 * Q * Q) * Q
            XQ = 0.3 * Q
            ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
            XF = -2.* COS(ANG) * (XQ/DENOM - QP/DENOMP)
            YF = -2.* SIN(ANG) * (XQ/DENOM - QP/DENOMP)
            XT = -2.* COS(ANG) * (Q/DENOM + Q/DENOMP)
            YT = -2.* SIN(ANG) * (Q/DENOM + Q/DENOMP)
C       write(6,100) xp*cellxy,yp*cellxy,rad*cellxy,ang*57.3,q,qp,
C     1      denom,denomp,zf,xf,yf,xt,yt
C100    format(1x,3f5.1,f5.0,9f5.2)
C                                  Generate the Design Matrix.
            NS     = NS  + WT
            SUM    = SUM + PH*WT
            SSQ    = SSQ + PH*PH*WT
            R(1)   = R(1) + PH*WT
            R(2)   = R(2) + PH*XP*WT
            R(3)   = R(3) + PH*YP*WT
            R(4)   = R(4) + PH*XF*WT
            R(5)   = R(5) + PH*YF*WT
            R(6)   = R(6) + PH*ZF*WT
            R(7)   = R(7) + PH*XT*WT
            R(8)   = R(8) + PH*YT*WT
            M(1,1) = M(1,1) + WT
            M(1,2) = M(1,2) + XP*WT
            M(1,3) = M(1,3) + YP*WT
            M(1,4) = M(1,4) + XF*WT
            M(1,5) = M(1,5) + YF*WT
            M(1,6) = M(1,6) + ZF*WT
            M(1,7) = M(1,7) + XT*WT
            M(1,8) = M(1,8) + YT*WT
            M(2,2) = M(2,2) + XP*XP*WT
            M(2,3) = M(2,3) + XP*YP*WT
            M(2,4) = M(2,4) + XP*XF*WT
            M(2,5) = M(2,5) + XP*YF*WT
            M(2,6) = M(2,6) + XP*ZF*WT
            M(2,7) = M(2,7) + XP*XT*WT
            M(2,8) = M(2,8) + XP*YT*WT
            M(3,3) = M(3,3) + YP*YP*WT
            M(3,4) = M(3,4) + YP*XF*WT
            M(3,5) = M(3,5) + YP*YF*WT
            M(3,6) = M(3,6) + YP*ZF*WT
            M(3,7) = M(3,7) + YP*XT*WT
            M(3,8) = M(3,8) + YP*YT*WT
            M(4,4) = M(4,4) + XF*XF*WT
            M(4,5) = M(4,5) + XF*YF*WT
            M(4,6) = M(4,6) + XF*ZF*WT
            M(4,7) = M(4,7) + XF*XT*WT
            M(4,8) = M(4,8) + XF*YT*WT
            M(5,5) = M(5,5) + YF*YF*WT
            M(5,6) = M(5,6) + YF*ZF*WT
            M(5,7) = M(5,7) + YF*XT*WT
            M(5,8) = M(5,8) + YF*YT*WT
            M(6,6) = M(6,6) + ZF*ZF*WT
            M(6,7) = M(6,7) + ZF*XT*WT
            M(6,8) = M(6,8) + ZF*YT*WT
            M(7,7) = M(7,7) + XT*XT*WT
            M(7,8) = M(7,8) + XT*YT*WT
            M(8,8) = M(8,8) + YT*YT*WT
 30         CONTINUE
 40      CONTINUE
C                                       Disable the subreflector
C                                       tilt term if requested
      IF (NOTILT) THEN
         DO 46 I = 7,8
            R(I) = 0.
            DO 47 J = 1, NP
               M(J,I) = 0.
               M(I,J) = 0.
 47            CONTINUE
 46         CONTINUE
         END IF
C                                       Disable the focus and feed
C                                       offset if requested

      IF (NOXY) THEN
         DO 43 I = 4, 5
            R(I) = 0.
            DO 42 J = 1, NP
               M(J,I) = 0.
               M(I,J) = 0.
 42         CONTINUE
 43      CONTINUE
      END IF
      IF (NOFOC) THEN
         R(6)=0.
         DO 44 J = 1,NP
           M(J,6)=0.
           M(6,J)=0.
 44      CONTINUE
      END IF

      IF (NOPNT) THEN
        DO 45 I = 2,3
           R(I) = 0.
           DO 48 J = 1,NP
              M(J,I) = 0.
              M(I,J) = 0.
 48        CONTINUE
 45     CONTINUE
      END IF
C                                       Compute the least squares
C                                       solution.
      CALL LEASQR (NP, NS, SUM, SSQ, R, M, X, VX, SSQRES, VARRES, VARY,
     *   FIT, IERR)
C
      P0 = X(1)
      PX = X(2)
      PY = X(3)
      FX = X(4)
      FY = X(5)
      FZ = X(6)
      TX = X(7)
      TY = X(8)
      DP0 = SQRT(VX(1))
      DPX = SQRT(VX(2))
      DPY = SQRT(VX(3))
      DFX = LAMBDA*SQRT(VX(4))/.36
      DFY = LAMBDA*SQRT(VX(5))/.36
      DFZ = LAMBDA*SQRT(VX(6))/.36
      DTX = LAMBDA*SQRT(VX(7))/.36
      DTY = LAMBDA*SQRT(VX(8))/.36
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP + YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               QP = Q/XMAG
               DENOM = 1.+Q*Q
               DENOMP = 1.+QP*QP
               XQ = (0.3 - 0.7 * Q * Q) * Q
               ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
               XF = -2.* COS(ANG) * (XQ/DENOM - QP/DENOMP)
               YF = -2.* SIN(ANG) * (XQ/DENOM - QP/DENOMP)
               XT = -2.* COS(ANG) * (Q/DENOM + Q/DENOMP)
               YT = -2.* SIN(ANG) * (Q/DENOM + Q/DENOMP)
               CORR = P0 + PX*XP + PY*YP + FX*XF + FY*YF + FZ*ZF + TX*XT
     *            + TY*YT
               VPHA(IX,IY) = VPHA(IX,IY) - CORR
               PHAMOD(IX,IY) = CORR
            END IF
 50      CONTINUE
 60   CONTINUE
C                                       Rescale feed offsets to mm.
      FX = LAMBDA*FX/.36
      FY = LAMBDA*FY/.36
      FZ = LAMBDA*FZ/.36
C                                       Rescale subr. tilts to 'mm.'
      TX = LAMBDA*TX/.36
      TY = LAMBDA*TY/.36
C                               Rescale phase slope to pointing offset
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                               Compute the post-fit surface rms
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE FLTMO3 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOTILT, NOPNT, XMAG,
     *   PHAMOD, TX, TY, DTX, DTY, VPARM)
C-----------------------------------------------------------------------
C     FLATPH2 corrects the grading phase for pointing, focus, and feed
C     offset errors using user's model, and a model incorporating
C     subreflector position errors.
C
C  This is a revised version of the task, offering a two-reflector
C  solution.  M. Kesteven, 6/12/1994
C
C  The formulation is in terms of the Ruze expressions (the unpublished
C  lecture notes : Small Displacements in Parabolic Antennas, 1969).
C
C  At present, this requires the magnification to be HARDWIRED -
C  see the data statement.
C
C     Given:
C          NPIX        I     Number of pixels on a side of the map.
C          LAMBDA      R     Observing wavelength, in meters.
C          FOCUS       R     Nominal focal length, in meters.
C          XYMIN       R     Range of |x| and |y| used in correcting for
C      and XYMAX       R     pointing, focus, and feed offset. Negative
C                            values denote a range of SQRT(x*x + y*y).
C          CELLXY      R     Map cell spacing, in meters.
C          VAMP(N,N)   R     Grading amplitude map.
C          VPHA(N,N)   R     Grading phase map.
C          NOPNT       L     Disable phase slope (pointing offset)
C          NOXY        L     Disable subreflector offset model
C          NOFOC       L     Disable subreflector focus (z) model
C          NOTILT      L     Enable subreflector rotation model.
C
C     Returned:
C          P0          R     Constant offset removed, degrees.
C          PX,PY       R     Least squares estimates of the phase ramp
C                            in the X and Y directions, in degrees per
C                            cell.
C          FX,FY,FZ    R     The derived focal position is at
C                            (FX,FY,FOCUS+FZ), millimeters.
C          TX,TY       R     Tilt of subreflector in X, Y axes.
C          DP0         R     Standard error in P0.
C          DPX,DPY     R     Standard error in PX, and PY.
C          DFX,DFY,DFZ R     Standard error in FX, FY, and PZ.
C          DTX,DTY     R     Standard error in TX, TY.
C          RMS         R     Weighted Half-path rms error, in mm.
C          RMS0        R     Pre-fit weighted half-path error, mm.
C          IERR        I     Error status, 0 means success.
C          PHMOD(N,N)  R     Model phase, due to subref. offsets
C          VPHA(N,N)   R     Phase map corrected for subr. offsets.
C
C     Called:
C          APLNOT: {LEASQR}
C
C     Algorithm:
C          Weighted least squares fit.
C
C     Notes:
C       1)  Subreflector offset inhibited if NOXY = .true.
C       2)  Subreflector focus model inhibited if NOFOC = .true.
C       3)  Subreflector tilt inhibited if NOTILT = .true.
C       4)  Phase slope (pointing offset) inhibited if NOPNT = .true.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C          mjk, 28/1/93
C          RAP, 27/05/08
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, XMAG, PHAMOD(NPIX,NPIX),TX, TY,
     *   DTX, DTY, VPARM(*)
      LOGICAL   NOXY, NOFOC, NOTILT, NOPNT
C
      INTEGER   IX, IX0, IR2MAX, IR2MIN, IXYMAX, IXYMIN, IY, IY0, NA
      REAL      CORR, FP, XF, XP, YF, YP, ZF, R4, MEAN, RAD, MAG, ANG,
     *   Q, QP, DENOM, DENOMP, XT, YT, AVGAMP, XQ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA      MAG/10.0/
C-----------------------------------------------------------------------
C                                       Initialize.
      IF (XMAG.LE.0.0) XMAG = MAG
      IXYMIN = ABS(XYMIN/CELLXY)
      IXYMAX = ABS(XYMAX/CELLXY)
      IR2MIN = (XYMIN*XYMIN)/(CELLXY*CELLXY)
      IR2MAX = (XYMAX*XYMAX)/(CELLXY*CELLXY)
C                                       Focal length in cellular units
      FP = FOCUS/CELLXY
C                                       Half-path wavelength scaling
      R4 = LAMBDA / 720.0
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
C
      P0 = VPARM(1)
      PX = VPARM(2)*CELLXY*360./LAMBDA/57.296/60.0
      PY = VPARM(3)*CELLXY*360./LAMBDA/57.296/60.0
      FX = VPARM(4) * 0.36 / LAMBDA
      FY = VPARM(5) * 0.36 / LAMBDA
      FZ = VPARM(6) * 0.36 / LAMBDA
      TX = VPARM(7) * 0.36 / LAMBDA
      TY = VPARM(8) * 0.36 / LAMBDA
      DP0 = 0.0
      DPX = 0.0
      DPY = 0.0
      DFX = 0.0
      DFY = 0.0
      DFZ = 0.0
      DTX = 0.0
      DTY = 0.0
      IF (VPARM(10).GT.0.0) THEN
         NA = 0
         AVGAMP = 0.0
         DO 20 IY = 1,NPIX
            DO 10 IX = 1,NPIX
               IF ((VPHA(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).GE.VPARM(10))) THEN
                  AVGAMP = AVGAMP + VAMP(IX,IY)
                  NA = NA + 1
                  END IF
 10            CONTINUE
 20         CONTINUE
         IF (NA.GT.0) AVGAMP = AVGAMP / NA
         write (msgtxt,4000) avgamp
         call msgwrt (8)
 4000    format ('Average amplitude',F6.3)
         END IF
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP + YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               QP = Q/XMAG
               DENOM = 1.+Q*Q
               DENOMP = 1.+QP*QP
               XQ = (0.3 - 0.7 * Q * Q) * Q
               ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
               XF = -2.* COS(ANG) * (XQ/DENOM - QP/DENOMP)
               YF = -2.* SIN(ANG) * (XQ/DENOM - QP/DENOMP)
               XT = -2.* COS(ANG) * (Q/DENOM + Q/DENOMP)
               YT = -2.* SIN(ANG) * (Q/DENOM + Q/DENOMP)
               CORR = P0 + PX*XP + PY*YP + FX*XF + FY*YF + FZ*ZF + TX*XT
     *            + TY*YT
               IF (VPARM(9).GT.0.0) THEN
                  VPHA(IX,IY) = CORR
               ELSE
                  VPHA(IX,IY) = VPHA(IX,IY) - CORR
                  END IF
               IF ((VPARM(10).GT.0.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *            THEN
                  IF (VAMP(IX,IY).LT.VPARM(10)) THEN
                     VAMP(IX,IY) = 0.0
                  ELSE
                     VAMP(IX,IY) = AVGAMP
                     END IF
                  END IF
               PHAMOD(IX,IY) = CORR
            ELSE IF ((VPARM(10).GT.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *         THEN
               VAMP(IX,IY) = 0.0
               END IF
 50         CONTINUE
 60      CONTINUE
C                                       Rescale feed offsets to mm.
      FX = LAMBDA*FX/.36
      FY = LAMBDA*FY/.36
      FZ = LAMBDA*FZ/.36
C                                       Rescale subr. tilts to 'mm.'
      TX = LAMBDA*TX/.36
      TY = LAMBDA*TY/.36
C                               Rescale phase slope to pointing offset
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                               Compute the post-fit surface rms
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE FLTPH2 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOTILT, NOPNT, XMAG,
     *   PHAMOD, TX, TY, DTX, DTY)
C-----------------------------------------------------------------------
C     FLATPH2 corrects the grading phase for pointing, focus, and feed
C     offset errors using least squares, and a model incorporating
C     subreflector position errors.
C
C  This is a revised version of the task, offering a two-reflector
C  solution.  M. Kesteven, 6/12/1994
C
C  The formulation is in terms of the Ruze expressions (the unpublished
C  lecture notes : Small Displacements in Parabolic Antennas, 1969).
C
C  At present, this requires the magnification to be HARDWIRED -
C  see the data statement.
C
C     Given:
C          NPIX        I     Number of pixels on a side of the map.
C          LAMBDA      R     Observing wavelength, in meters.
C          FOCUS       R     Nominal focal length, in meters.
C          XYMIN       R     Range of |x| and |y| used in correcting for
C      and XYMAX       R     pointing, focus, and feed offset. Negative
C                            values denote a range of SQRT(x*x + y*y).
C          CELLXY      R     Map cell spacing, in meters.
C          VAMP(N,N)   R     Grading amplitude map.
C          VPHA(N,N)   R     Grading phase map.
C          NOPNT       L     Disable phase slope (pointing offset)
C          NOXY        L     Disable subreflector offset model
C          NOFOC       L     Disable subreflector focus (z) model
C          NOTILT      L     Enable subreflector rotation model.
C
C     Returned:
C          P0          R     Constant offset removed, degrees.
C          PX,PY       R     Least squares estimates of the phase ramp
C                            in the X and Y directions, in degrees per
C                            cell.
C          FX,FY,FZ    R     The derived focal position is at
C                            (FX,FY,FOCUS+FZ), millimeters.
C          TX,TY       R     Tilt of subreflector in X, Y axes.
C          DP0         R     Standard error in P0.
C          DPX,DPY     R     Standard error in PX, and PY.
C          DFX,DFY,DFZ R     Standard error in FX, FY, and PZ.
C          DTX,DTY     R     Standard error in TX, TY.
C          RMS         R     Weighted Half-path rms error, in mm.
C          RMS0        R     Pre-fit weighted half-path error, mm.
C          IERR        I     Error status, 0 means success.
C          PHMOD(N,N)  R     Model phase, due to subref. offsets
C          VPHA(N,N)   R     Phase map corrected for subr. offsets.
C
C     Called:
C          APLNOT: {LEASQR}
C
C     Algorithm:
C          Weighted least squares fit.
C
C     Notes:
C       1)  Subreflector offset inhibited if NOXY = .true.
C       2)  Subreflector focus model inhibited if NOFOC = .true.
C       3)  Subreflector tilt inhibited if NOTILT = .true.
C       4)  Phase slope (pointing offset) inhibited if NOPNT = .true.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C          mjk, 28/1/93
C          RAP, 27/05/08
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, XMAG, PHAMOD(NPIX,NPIX),TX, TY,
     *   DTX, DTY
      LOGICAL   NOXY, NOFOC, NOTILT, NOPNT
C
      INTEGER   NP
      PARAMETER (NP=8)
      INTEGER   I, IDR2, IDX, IDY, IX, IX0, IR2MAX, IR2MIN, IXYMAX,
     *   IXYMIN, IY, IY0, J
      REAL      CORR, FIT, FP, M(NP,NP), NS, PH, R(NP), SUM, SSQ,
     *   SSQRES, VARRES, VARY, VX(NP), WT, X(NP), XF, XP, YF, YP, ZF,
     *   R4, MEAN, RAD, RR, MAG, FEQUIV, ANG, Q, QP, DENOM, DENOMP,
     *   XT, YT
      INCLUDE 'INCS:DDCH.INC'
      DATA      MAG/10.0/
C-----------------------------------------------------------------------
C                                       Initialize.
      IF (XMAG.LE.0.0) XMAG = MAG
      IXYMIN = ABS(XYMIN/CELLXY)
      IXYMAX = ABS(XYMAX/CELLXY)
      IR2MIN = (XYMIN*XYMIN)/(CELLXY*CELLXY)
      IR2MAX = (XYMAX*XYMAX)/(CELLXY*CELLXY)
C                                       Focal length in cellular units
      FP = FOCUS/CELLXY
      FEQUIV = XMAG * FP
C                                       Half-path wavelength scaling
      R4 = LAMBDA / 720.0
      NS  = 0.0
      SUM = 0.0
      SSQ = 0.0
      DO 20 I = 1,NP
         R(I) = 0.0
         DO 10 J = 1,NP
            M(I,J) = 0.0
 10         CONTINUE
 20      CONTINUE
      RR = NPIX/2. + 1.
C                                       Calculate pre-fit rms.
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS0)
C                                       Loop through the map.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 40 IY = 1,NPIX
         IDY = ABS(IY-IY0)
C                                       Check absolute limits.
         IF (XYMIN.GT.0.0 .AND. IDY.LT.IXYMIN) GO TO 40
         IF (XYMAX.GT.0.0 .AND. IDY.GT.IXYMAX) GO TO 40
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF ((XYMAX.LT.0.0) .AND. (IDY*IDY.GT.IR2MAX)) GO TO 40
         DO 30 IX = 1,NPIX
C                                       Ignore blanked pixels.
            IF (VPHA(IX,IY).EQ.FBLANK) GO TO 30
C                                       Check for inclusion.
            IDX  = ABS(IX-IX0)
            IDR2 = IDX*IDX + IDY*IDY
C                                       Inner limits.
            IF (XYMIN.GT.0.0) THEN
               IF (IDX.LT.IXYMIN) GO TO 30
            ELSE IF (XYMIN.LT.0.0) THEN
               IF (IDR2.LT.IR2MIN) GO TO 30
               END IF
C                                       Outer limits.
            IF (XYMAX.GT.0.0) THEN
               IF (IDX.GT.IXYMAX) GO TO 30
            ELSE IF (XYMAX.LT.0.0) THEN
               IF (IDR2.GT.IR2MAX) GO TO 30
               END IF
C                                       Evaluate variables (in cells)
            PH = VPHA(IX,IY)
            WT = VAMP(IX,IY)
            XP = IX - IX0
            YP = IY - IY0
            RAD = SQRT(XP*XP + YP*YP)
            ANG = ATAN2(YP,XP)
            Q = RAD/(2.*FP)
            QP = Q/XMAG
            DENOM = 1.+Q*Q
            DENOMP = 1.+QP*QP
            ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
            XF = -2.*COS(ANG)*(Q/DENOM - QP/DENOMP)
            YF = -2.*SIN(ANG)*(Q/DENOM - QP/DENOMP)
            XT = -2.*COS(ANG)*(Q/DENOM + Q/DENOMP)
            YT = -2.*SIN(ANG)*(Q/DENOM + Q/DENOMP)
C       write(6,100) xp*cellxy,yp*cellxy,rad*cellxy,ang*57.3,q,qp,
C     1      denom,denomp,zf,xf,yf,xt,yt
C100    format(1x,3f5.1,f5.0,9f5.2)
C                                  Generate the Design Matrix.
            NS     = NS  + WT
            SUM    = SUM + PH*WT
            SSQ    = SSQ + PH*PH*WT
            R(1)   = R(1) + PH*WT
            R(2)   = R(2) + PH*XP*WT
            R(3)   = R(3) + PH*YP*WT
            R(4)   = R(4) + PH*XF*WT
            R(5)   = R(5) + PH*YF*WT
            R(6)   = R(6) + PH*ZF*WT
            R(7)   = R(7) + PH*XT*WT
            R(8)   = R(8) + PH*YT*WT
            M(1,1) = M(1,1) + WT
            M(1,2) = M(1,2) + XP*WT
            M(1,3) = M(1,3) + YP*WT
            M(1,4) = M(1,4) + XF*WT
            M(1,5) = M(1,5) + YF*WT
            M(1,6) = M(1,6) + ZF*WT
            M(1,7) = M(1,7) + XT*WT
            M(1,8) = M(1,8) + YT*WT
            M(2,2) = M(2,2) + XP*XP*WT
            M(2,3) = M(2,3) + XP*YP*WT
            M(2,4) = M(2,4) + XP*XF*WT
            M(2,5) = M(2,5) + XP*YF*WT
            M(2,6) = M(2,6) + XP*ZF*WT
            M(2,7) = M(2,7) + XP*XT*WT
            M(2,8) = M(2,8) + XP*YT*WT
            M(3,3) = M(3,3) + YP*YP*WT
            M(3,4) = M(3,4) + YP*XF*WT
            M(3,5) = M(3,5) + YP*YF*WT
            M(3,6) = M(3,6) + YP*ZF*WT
            M(3,7) = M(3,7) + YP*XT*WT
            M(3,8) = M(3,8) + YP*YT*WT
            M(4,4) = M(4,4) + XF*XF*WT
            M(4,5) = M(4,5) + XF*YF*WT
            M(4,6) = M(4,6) + XF*ZF*WT
            M(4,7) = M(4,7) + XF*XT*WT
            M(4,8) = M(4,8) + XF*YT*WT
            M(5,5) = M(5,5) + YF*YF*WT
            M(5,6) = M(5,6) + YF*ZF*WT
            M(5,7) = M(5,7) + YF*XT*WT
            M(5,8) = M(5,8) + YF*YT*WT
            M(6,6) = M(6,6) + ZF*ZF*WT
            M(6,7) = M(6,7) + ZF*XT*WT
            M(6,8) = M(6,8) + ZF*YT*WT
            M(7,7) = M(7,7) + XT*XT*WT
            M(7,8) = M(7,8) + XT*YT*WT
            M(8,8) = M(8,8) + YT*YT*WT
 30         CONTINUE
 40      CONTINUE
C                                       Disable the subreflector
C                                       tilt term if requested
      IF (NOTILT) THEN
         DO 46 I = 7,8
            R(I) = 0.
            DO 47 J = 1, NP
               M(J,I) = 0.
               M(I,J) = 0.
 47            CONTINUE
 46         CONTINUE
         END IF
C                                       Disable the focus and feed
C                                       offset if requested

      IF (NOXY) THEN
         DO 43 I = 4, 5
            R(I) = 0.
            DO 42 J = 1, NP
               M(J,I) = 0.
               M(I,J) = 0.
 42         CONTINUE
 43      CONTINUE
      END IF
      IF (NOFOC) THEN
         R(6)=0.
         DO 44 J = 1,NP
           M(J,6)=0.
           M(6,J)=0.
 44      CONTINUE
      END IF

      IF (NOPNT) THEN
        DO 45 I = 2,3
           R(I) = 0.
           DO 48 J = 1,NP
              M(J,I) = 0.
              M(I,J) = 0.
 48        CONTINUE
 45     CONTINUE
      END IF
C                                       Compute the least squares
C                                       solution.
      CALL LEASQR (NP, NS, SUM, SSQ, R, M, X, VX, SSQRES, VARRES, VARY,
     *   FIT, IERR)
C
      P0 = X(1)
      PX = X(2)
      PY = X(3)
      FX = X(4)
      FY = X(5)
      FZ = X(6)
      TX = X(7)
      TY = X(8)
      DP0 = SQRT(VX(1))
      DPX = SQRT(VX(2))
      DPY = SQRT(VX(3))
      DFX = LAMBDA*SQRT(VX(4))/.36
      DFY = LAMBDA*SQRT(VX(5))/.36
      DFZ = LAMBDA*SQRT(VX(6))/.36
      DTX = LAMBDA*SQRT(VX(7))/.36
      DTY = LAMBDA*SQRT(VX(8))/.36
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP + YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               QP = Q/XMAG
               DENOM = 1.+Q*Q
               DENOMP = 1.+QP*QP
               ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
               XF = -2.*COS(ANG)*(Q/DENOM - QP/DENOMP)
               YF = -2.*SIN(ANG)*(Q/DENOM - QP/DENOMP)
               XT = -2.*COS(ANG)*(Q/DENOM + Q/DENOMP)
               YT = -2.*SIN(ANG)*(Q/DENOM + Q/DENOMP)
               CORR = P0+PX*XP+PY*YP+FX*XF+FY*YF+FZ*ZF+TX*XT+TY*YT
               VPHA(IX,IY) = VPHA(IX,IY) - CORR
               PHAMOD(IX,IY) = CORR
            END IF
 50      CONTINUE
 60   CONTINUE
C                                       Rescale feed offsets to mm.
      FX = LAMBDA*FX/.36
      FY = LAMBDA*FY/.36
      FZ = LAMBDA*FZ/.36
C                                       Rescale subr. tilts to 'mm.'
      TX = LAMBDA*TX/.36
      TY = LAMBDA*TY/.36
C                               Rescale phase slope to pointing offset
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                               Compute the post-fit surface rms
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE FLTMO2 (NPIX, LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY,
     *   VAMP, VPHA, P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY, DFX,
     *   DFY, DFZ, RMS0, RMS, IERR, NOXY, NOFOC, NOTILT, NOPNT, XMAG,
     *   PHAMOD, TX, TY, DTX, DTY, VPARM)
C-----------------------------------------------------------------------
C     FLATPH2 corrects the grading phase for pointing, focus, and feed
C     offset errors using user's model, and a model incorporating
C     subreflector position errors.
C
C  This is a revised version of the task, offering a two-reflector
C  solution.  M. Kesteven, 6/12/1994
C
C  The formulation is in terms of the Ruze expressions (the unpublished
C  lecture notes : Small Displacements in Parabolic Antennas, 1969).
C
C  At present, this requires the magnification to be HARDWIRED -
C  see the data statement.
C
C     Given:
C          NPIX        I     Number of pixels on a side of the map.
C          LAMBDA      R     Observing wavelength, in meters.
C          FOCUS       R     Nominal focal length, in meters.
C          XYMIN       R     Range of |x| and |y| used in correcting for
C      and XYMAX       R     pointing, focus, and feed offset. Negative
C                            values denote a range of SQRT(x*x + y*y).
C          CELLXY      R     Map cell spacing, in meters.
C          VAMP(N,N)   R     Grading amplitude map.
C          VPHA(N,N)   R     Grading phase map.
C          NOPNT       L     Disable phase slope (pointing offset)
C          NOXY        L     Disable subreflector offset model
C          NOFOC       L     Disable subreflector focus (z) model
C          NOTILT      L     Enable subreflector rotation model.
C
C     Returned:
C          P0          R     Constant offset removed, degrees.
C          PX,PY       R     Least squares estimates of the phase ramp
C                            in the X and Y directions, in degrees per
C                            cell.
C          FX,FY,FZ    R     The derived focal position is at
C                            (FX,FY,FOCUS+FZ), millimeters.
C          TX,TY       R     Tilt of subreflector in X, Y axes.
C          DP0         R     Standard error in P0.
C          DPX,DPY     R     Standard error in PX, and PY.
C          DFX,DFY,DFZ R     Standard error in FX, FY, and PZ.
C          DTX,DTY     R     Standard error in TX, TY.
C          RMS         R     Weighted Half-path rms error, in mm.
C          RMS0        R     Pre-fit weighted half-path error, mm.
C          IERR        I     Error status, 0 means success.
C          PHMOD(N,N)  R     Model phase, due to subref. offsets
C          VPHA(N,N)   R     Phase map corrected for subr. offsets.
C
C     Called:
C          APLNOT: {LEASQR}
C
C     Algorithm:
C          Weighted least squares fit.
C
C     Notes:
C       1)  Subreflector offset inhibited if NOXY = .true.
C       2)  Subreflector focus model inhibited if NOFOC = .true.
C       3)  Subreflector tilt inhibited if NOTILT = .true.
C       4)  Phase slope (pointing offset) inhibited if NOPNT = .true.
C
C     Author:
C          Mark Calabretta, Australia Telescope.
C          Origin; 1987/Nov.    Code last modified; 1989/Nov/01.
C          mjk, 28/1/93
C          RAP, 27/05/08
C-----------------------------------------------------------------------
      INTEGER   NPIX, IERR
      REAL      LAMBDA, FOCUS, XYMIN, XYMAX, CELLXY, VAMP(NPIX,NPIX),
     *   VPHA(NPIX,NPIX), P0, PX, PY, FX, FY, FZ, DP0, DPX, DPY,
     *   DFX, DFY, DFZ, RMS0, RMS, XMAG, PHAMOD(NPIX,NPIX),TX, TY,
     *   DTX, DTY, VPARM(*)
      LOGICAL   NOXY, NOFOC, NOTILT, NOPNT
C
      INTEGER   IX, IX0, IR2MAX, IR2MIN, IXYMAX, IXYMIN, IY, IY0, NA
      REAL      CORR, FP, XF, XP, YF, YP, ZF, R4, MEAN, RAD, MAG, ANG,
     *   Q, QP, DENOM, DENOMP, XT, YT, AVGAMP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA      MAG/10.0/
C-----------------------------------------------------------------------
C                                       Initialize.
      IF (XMAG.LE.0.0) XMAG = MAG
      IXYMIN = ABS(XYMIN/CELLXY)
      IXYMAX = ABS(XYMAX/CELLXY)
      IR2MIN = (XYMIN*XYMIN)/(CELLXY*CELLXY)
      IR2MAX = (XYMAX*XYMAX)/(CELLXY*CELLXY)
C                                       Focal length in cellular units
      FP = FOCUS/CELLXY
C                                       Half-path wavelength scaling
      R4 = LAMBDA / 720.0
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
C
      P0 = VPARM(1)
      PX = VPARM(2)*CELLXY*360./LAMBDA/57.296/60.0
      PY = VPARM(3)*CELLXY*360./LAMBDA/57.296/60.0
      FX = VPARM(4) * 0.36 / LAMBDA
      FY = VPARM(5) * 0.36 / LAMBDA
      FZ = VPARM(6) * 0.36 / LAMBDA
      TX = VPARM(7) * 0.36 / LAMBDA
      TY = VPARM(8) * 0.36 / LAMBDA
      DP0 = 0.0
      DPX = 0.0
      DPY = 0.0
      DFX = 0.0
      DFY = 0.0
      DFZ = 0.0
      DTX = 0.0
      DTY = 0.0
      IF (VPARM(10).GT.0.0) THEN
         NA = 0
         AVGAMP = 0.0
         DO 20 IY = 1,NPIX
            DO 10 IX = 1,NPIX
               IF ((VPHA(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).NE.FBLANK) .AND.
     *            (VAMP(IX,IY).GE.VPARM(10))) THEN
                  AVGAMP = AVGAMP + VAMP(IX,IY)
                  NA = NA + 1
                  END IF
 10            CONTINUE
 20         CONTINUE
         IF (NA.GT.0) AVGAMP = AVGAMP / NA
         write (msgtxt,4000) avgamp
         call msgwrt (8)
 4000    format ('Average amplitude',F6.3)
         END IF
C                                       Apply the correction.
      DO 60 IY = 1,NPIX
         DO 50 IX = 1,NPIX
            IF (VPHA(IX,IY).NE.FBLANK) THEN
               XP = IX - IX0
               YP = IY - IY0
               RAD = SQRT(XP*XP + YP*YP)
               ANG = ATAN2(YP,XP)
               Q = RAD/(2.*FP)
               QP = Q/XMAG
               DENOM = 1.+Q*Q
               DENOMP = 1.+QP*QP
               ZF = (1.-Q*Q)/DENOM + (1.-QP*QP)/DENOMP
               XF = -2.*COS(ANG)*(Q/DENOM - QP/DENOMP)
               YF = -2.*SIN(ANG)*(Q/DENOM - QP/DENOMP)
               XT = -2.*COS(ANG)*(Q/DENOM + Q/DENOMP)
               YT = -2.*SIN(ANG)*(Q/DENOM + Q/DENOMP)
               CORR = P0+PX*XP+PY*YP+FX*XF+FY*YF+FZ*ZF+TX*XT+TY*YT
               IF (VPARM(9).GT.0.0) THEN
                  VPHA(IX,IY) = CORR
               ELSE
                  VPHA(IX,IY) = VPHA(IX,IY) - CORR
                  END IF
               IF ((VPARM(10).GT.0.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *            THEN
                  IF (VAMP(IX,IY).LT.VPARM(10)) THEN
                     VAMP(IX,IY) = 0.0
                  ELSE
                     VAMP(IX,IY) = AVGAMP
                     END IF
                  END IF
               PHAMOD(IX,IY) = CORR
            ELSE IF ((VPARM(10).GT.0) .AND. (VAMP(IX,IY).NE.FBLANK))
     *         THEN
               VAMP(IX,IY) = 0.0
               END IF
 50         CONTINUE
 60      CONTINUE
C                                       Rescale feed offsets to mm.
      FX = LAMBDA*FX/.36
      FY = LAMBDA*FY/.36
      FZ = LAMBDA*FZ/.36
C                                       Rescale subr. tilts to 'mm.'
      TX = LAMBDA*TX/.36
      TY = LAMBDA*TY/.36
C                               Rescale phase slope to pointing offset
      PX = PX/CELLXY/360*LAMBDA*57.296*60.
      PY = PY/CELLXY/360*LAMBDA*57.296*60.
C                               Compute the post-fit surface rms
      CALL SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN, IR2MAX,
     *   VAMP, VPHA, R4, MEAN, RMS)
C
 999  RETURN
      END
      SUBROUTINE SRFRMS (NPIX, XYMIN, XYMAX, IXYMIN, IXYMAX, IR2MIN,
     *   IR2MAX, VAMP, VPHA, R4, MEAN, RMS)
C -----------------------------------------------------------------------
      INTEGER   NPIX, IXYMIN, IXYMAX, IR2MIN, IR2MAX, IX0, IY0, IY, IX,
     *   IDY, IDX, IDR2
      REAL      XYMIN, XYMAX, VAMP(NPIX,NPIX), VPHA(NPIX,NPIX), MEAN,
     *   RMS, WT, R4
      INCLUDE 'INCS:DDCH.INC'
C -----------------------------------------------------------------------
C                                       Compute the weighted half-path
C                                       error (as RMS)
C                                       Loop through the map.
      MEAN = 0.
      RMS = 0.
      WT  = 0.
      IX0 = NPIX/2 + 1
      IY0 = NPIX/2 + 1
      DO 140 IY = 1,NPIX
         IDY = ABS (IY-IY0)
C                                       Check absolute limits.
         IF ((XYMIN.GT.0.0) .AND. (IDY.LT.IXYMIN)) GO TO 140
         IF ((XYMAX.GT.0.0) .AND. (IDY.GT.IXYMAX)) GO TO 140
C                                       Is this row of pixels outside
C                                       the outer ring?
         IF ((XYMAX.LT.0.0) .AND. (IDY*IDY.GT.IR2MAX)) GO TO 140
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
      IF (WT.GT.0) THEN
         MEAN = MEAN / WT
         RMS = RMS/WT - MEAN*MEAN
         IF (RMS.GT.0) RMS = SQRT (RMS)
C                                               Convert to mm.
         MEAN = MEAN*1000.
         RMS = RMS * 1000.
      ELSE
         RMS = -99
         END IF
C
      RETURN
      END
