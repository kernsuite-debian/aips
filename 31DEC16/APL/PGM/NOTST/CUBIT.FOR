LOCAL INCLUDE 'CUBSIZ.INC'
      INTEGER   MAXPRM
      PARAMETER (MAXPRM = 15)
LOCAL END
LOCAL INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBSIZ.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO, JBUFSZ,
     *   CATOLD(256), FIX(MAXPRM), FPNUM(MAXPRM)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOT(2), XVCODE,
     *   XDCODE
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, VCODE*4,
     *   DCODE*4, FSTRNG(MAXPRM)*24, PSTRNG(MAXPRM)*24
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), TOL,
     *   FPARM(30), CPARM(10), DPARM(10), VPARM(30), ERRV,
     *   RPARM(30), XCUT, OLDR(256)
      HOLLERITH OLDH(256)
      DOUBLE PRECISION OLDD(128)
      EQUIVALENCE (CATOLD, OLDR, OLDH, OLDD)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOT,
     *   XSEQO, XDISKO, BLC, TRC, TOL, XVCODE, XDCODE, FPARM, CPARM,
     *   DPARM, VPARM, RPARM, XCUT, ERRV
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, VCODE, DCODE,
     *   FSTRNG, PSTRNG
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, FIX, FPNUM
LOCAL END
LOCAL INCLUDE 'CUBPRM.INC'
      REAL      BUFF1(16384), BUFF2(16384), DSKMAX, DSKMIN, COSMIN,
     *   COSMAX, SINMIN, SINMAX, CONST, DELVEL, WIDTH
      COMMON /PARM1/ CONST, DELVEL, WIDTH
      COMMON /BUFRS/ BUFF1, BUFF2
      COMMON /LIMITS/ DSKMIN, DSKMAX, COSMIN, COSMAX, SINMIN, SINMAX
LOCAL END
LOCAL INCLUDE 'CUBDIM.INC'
      INTEGER   NXOUT, NYOUT, NVOUT
      COMMON /ALSMTH/ NXOUT, NYOUT, NVOUT
LOCAL END
      PROGRAM CUBIT
C-----------------------------------------------------------------------
C!  Task to model the HI velocity and density dist'ns in galaxies
C#  Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2007, 2010, 2012, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;  Developed by Judith Irwin, University of Toronto  1989-2000
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
C   CUBIT was developed from the AIPS template program, TAFFY.   If
C   pixel blanking is desired it should first be done in BLANK or some
C   other appropriate task.  The  image should then be transposed (eg.
C   task TRANS) if necessary such that the first axis is VELO in m/s
C   and second and third axes are RA and DEC, respectively.
C
C   CUBIT is linked with the same subroutines as TAFFY but
C   in addition, the subroutines LSTSQ1 and LSTSQ need to be
C   added to the appropriate apl sublibrary and several other
C   subroutines need to be recompiled -- see README.DOC for
C   directions.  There are also currently, limits on the size
C   of the cube which can be inputted. To increase the size of the
C   input cube, see README.DOC as well.
C
C   Aug/89 version changes made -- fixed up problem with gaussian
C          when z (see FCN) boundaries are both +ve and -ve --
C          turned back on the JJ=2 switch which had accidentally
C          been turned off.
C   November/89 changes made -- Fixed up problem along minor axis
C          which didn't allow +ve RX to correspond to -ve vel.
C   December/89 version -- Completely revamped subroutine Density
C          to correctly  deal with multiplication of R fcn by
C          z fcn.  An approximation is required for the integration.
C          Changes have been made to the Deltas which determine the
C          Derivatives.  Also, when incrementing the FPARMs to
C          calculate the derivatives, checks have been installed
C          to ensure that they are still in bounds. The rotation
C          curve maximum position has been constrained to be within
C          three times the disk radius.
C   January/90 version -- Fixed up RC case to allow a declining
C          rotation curve.
C   June/91 modifications -- readjusted boundaries when value is
C          out of bounds -- both for FPARM and derivative
C          -- adjusted code to avoid underflows in computing density
C          -- changed logistics of code for IFLAG >1 such that out
C          of bounds criteria are applied only once
C          -- adjusted acceptance criteria in density subroutine
C          to allow new value when increments small -- depends on
C          PIXSTD
C          -- tweaked the increments for derivative calculations
C          again
C          -- adjusted output format to allow an extra figure
C          -- changed connection between LSTSQ and FCN for a case
C          with blanking to ensure that on the first call the deltas
C          are set even if IFLAG .ne. 2.
C          -- in incrementing for derivatives, included code which
C          avoids round off errors near boundaries
C          -- Adjusted upper limit on rmax to be 10*dskmax
C          -- Corrected derp/derm error which divided by different
C          increments
C          -- Ensured that FPARM(10) and FPARM(11) are fixed if
C          the density is constant
C     1May/92 version (Queen's University) -- modifications to work
C          with AIPS versions 15OCT89 or later.  Modified the cal-
C          culation for the case close to the minor axis so that
C          emission can extend into two rather than just one channel.
C     15May92 version -- made modifications so that a ring density
C          distribution can be included (like sept91 in old aips).
C          Different scale lengths are allowed for radii on either
C          side of the ring.  Another FPARM is needed for this.
C          Added additional constraint that FPARM(10) should not be
C          greater than 10*DSKMAX.
C          In DENSTY:  constrained ZINC so that it doesn't fall below
C          1e-30
C     1June92 version -- added velocity smoothing option - UNFINISHED
C     1Aug92 version -- After CUBIT.FOR deleted accidentally!  Start
C          with unfinished vel smoothing version.  Made improvements
C          to the selection criteria in subroutine DENSTY
C     Dec/92 -- starting again to check vel smoothing
C     Feb/93 -- corrected error in subroutine SMOOTH/SMTH1.  A scaling
C          factor was required to get the intensity correct.  The error
C          related to the fact that I is in Jy/beam, whereas the cal-
C          culation was done per pixel and then smoothed to the beam
C          area.  Consequently a multiplicative factor of beam area(in
C          pixels) was introduced erroneously during smoothing.  All
C          was introduced erroneously during smoothing.  All previous
C          CUBIT's have been in error by this amount.  The only
C          affected value is density which would have been low by
C          this same factor.
C      **TO CORRECT DENSITY VALUES FROM PREVIOUS VERIONS, MULTIPLY BY
C          BMAJ*BMIN (IN PIXELS^2)  (NO FACTOR OF PI)**
C      Aug/93 -- introduced a velocity dispersion option
C      Apr/95 -- introduced a flux cutoff so that low level emission
C          could be ignored in the model
C          Also included an exponential ring distribution
C      Jun/00 -- fixing up some syntax, (one comma was omitted which
C        only produced a warning previously but caused the compile
C        to fail under linux g77)
C        Fixed up the warning (now a fail) about differing buffr lengths
C        for the array WA and BUFFR1 -- this was done by eliminating
C        the equivalence between WA and BUFF1 in SENDMA
C        Had to change the length of BUFF1, BUFF2, XBUFF1, AND XBUFF2
C        to 16384 to be compatible with new aips.  Also changed JBUFSZ.
C        Finally, note that LSTSQ could not recognize FBLANK.  had to
C        replace the previous ddch.inc with the new one in LSTSQ
C      Jul/00 -- this version attempts to model a 2-component vertical
C        distribution of density,
C       i.e. n(R,z)=n(R,0)*[fn1 exp(-h/b1) + fn2 exp(-h^2/b2^2)] fn1+fn2=1
C       so we are adding 3 new parameters
C       Also fixed a little problem with the velocity smoothing (VSMTH)
C       -- i.e. some forbidden velocities are allowed after vel. smoothing
C      Aug/00 -- fixed some boundary problems near vsys and fixed a
C           problem with sb
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      FACTOR         TOL           Convergence criterion to stop
C                                   iteration (0 => .001)
C      VCODE          VCODE         Velocity code 'CV','SB','BR','RC'
C      DCODE          DCODE         Density code 'CD','EX','GS','RG'
C      FPARM(MAXPRM)  FPARM         Initial guesses to parameters.
C                                   Parameters to be fitted
C                                   1: central X pixel position (RA)
C                                   2: central Y pixel position (DEC)
C                                   3: position angle of receding
C                                      major axis (degrees)
C                                   4: inclination angle (degrees)
C                                   5: systemic velocity (km/s)
C                                        (should be positive)
C                                   6: Vmax - max rotation velocity
C                                      (km/s)-OR-scale factor for RC
C                                      (for RC, 0=>1)
C                                   7: Rmax - R at which Vmax occurs
C                                             (arcsec) 0 => dparm(2)
C                                   8: m - Brandt index 0 => 1
C                                   9: max density (/cm**3)
C                                   10: radial density scale length
C                                       (arcsec)
C                                   11: perp EX density scale length
C                                       (") 0 => DPARM(7) if
C                                       cparm(4)=1,2
C                                   12: radial density scale length
C                                       for r<r0 (DCODE=RG only)
C                                   13. perp GS dens scale length
C                                      (") 0 => DPARM(7) if
C                                       cparm(4)=3
C                                   14. nf1 - fractional EX perp
C                                       dens (0=>1 if cparm(4)=1,2)
C                                   15. nf2 - fractional GS perp
C                                       dens (0=>1 if cparm(4)=3)
C
C      CPARM(10)      CPARM         1: 0 => write residuals
C                                      1 => write model
C                                   2: Distance to galaxy (Mpc)
C                                   3: Parameters to be held fixed
C                                      (Sum of 2**i), where i is
C                                      the parameter number. eg.
C                                      Parameters 1,2,5 fixed =>
C                                      CPARM(3)=2**1+2**2+2**5=37
C                                   4: Perpendicular density dist'n
C                                      number (1,2,3,4) 0=>1
C                                   5: Positon of ring center for RG
C                                   6: FWHM of gaussian for velocity
C                                      smoothing (km/s)
C                                   7: In case of RG, 2==> Exp
C                                      3==> Gauss and 0==> Gauss
C      DPARM(10)      DPARM         1: minimum limit to radius (arcsec)
C                                   2: maximum limit to radius  "
C                                      must be < or = R(disk).
C                                   3: min limit to cos(psi) (measured
C                                      from receding major axis)
C                                   4: max limit to cos(psi)
C                                   5: min limit to sin(psi)
C                                   6: max limit to sin(psi)
C                                      all 0 => all quadrants
C                                   7: height cutoff (arcsec)
C                                      specify!
C      VPARM(30)      VPARM         User specified rotation curve(km/s)
C                                   Use if VCODE='RC'
C      RPARM(30)      RPARM         Radii corresponding to specified
C                                   rotation curve (arcsec)
C      ICUT           XCUT          Cutoff (Jy/beam) for ignoring
C                                   input data
C      PIXSTD         ERRV          Estimated rms uncertainty at each
C                                   pixel. (Jy/ba) 0 => .001
C
C   Programmer J. A. Irwin - for CUBIT
C              E. W. Greisen - overhaul to modern practises (2007)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   N, IRET, IT
      LONGINT   LPDATA, LPBLNK, LPARRA, IPARRA
      REAL      DATA(2)
      INTEGER   JBLANK(2), NWRDAT, NWRDAR, IERR
      DOUBLE PRECISION DARRAY(2)
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'CUBDIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /LEAST/ IT
      DATA PRGM /'CUBIT '/
C-----------------------------------------------------------------------
C                                       get inputs, create output
      IT = 0
      CALL CUBITI (PRGM, N, IRET)
C                                       get memory as needed
      IF (IRET.EQ.0) THEN
         NWRDAT = (NVOUT * NXOUT * NYOUT + 10) / 1024 + 1
         NWRDAR = (2 * 3 * NXOUT * NYOUT + 10) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWRDAT, DATA, LPDATA, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NWRDAT, JBLANK,
     *      LPBLNK, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', PRGM, NWRDAR, DARRAY,
     *      IPARRA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, NWRDAT, NWRDAR
            CALL MSGWRT (8)
         ELSE
            IF (IPARRA.GE.0) THEN
               LPARRA = 1 + (IPARRA+1)/2
            ELSE
               LPARRA = 1 + IPARRA / 2
               END IF
            END IF
         END IF
C                                       Call routine that reads input
C                                       file, sends data to the least
C                                       squares subroutine and writes
C                                       output file
      IF (IRET.EQ.0) THEN
         CALL CUBITM (N, DATA(1+LPDATA), JBLANK(1+LPBLNK), NXOUT, NYOUT,
     *      DARRAY(LPARRA), IRET)
         CALL ZMEMRY ('FREE', PRGM, NWRDAT, DATA, LPDATA, IERR)
         CALL ZMEMRY ('FREE', PRGM, NWRDAT, JBLANK, LPBLNK, IERR)
         CALL ZMEMRY ('FREE', PRGM, NWRDAR, DARRAY, IPARRA, IERR)
C                                       history update subprogram
         IF (IRET.EQ.0) CALL CUBITH (IRET)
         END IF
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETTING',2I9,' KILOWORDS OF MEMORY')
      END
      SUBROUTINE CUBITI (PRGN, N, IRET)
C-----------------------------------------------------------------------
C   CUBITI gets input parameters for CUBIT, creates an output file,
C   and sets some of the default parameters.
C   Input:
C      PRGN   C*6   Program name
C   Output:
C      N      I     Number of free parameters for later fit.
C      IRET   I     Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Subprograms called:
C      AIPS sublibraries....ZDCHIN, VHDRIN, GTPARM, MSGWRT, RELPOP,
C                           CATDIR, CATIO, COPY, MAKOUT, CHR2H, H2CHR,
C                           WINDOW, MCREAT
C      CUBIT subroutine.....NEWHED
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   N, IRET
C
      CHARACTER STAT*4, MTYPE*2, CODESV(4)*4, CODESD(4)*4
      INTEGER   I, L, NFIX, IERR, NPARM, IROUND
      DOUBLE PRECISION SFREQ, CMASEC, JY2DEG
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBSIZ.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'CUBDIM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CODESV /'CV  ', 'SB  ', 'BR  ', 'RC  '/
      DATA CODESD /'CD  ', 'EX  ', 'GS  ', 'RG  '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * 16384
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 143
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOT, CLAOUT)
      CALL H2CHR (4, 1, XVCODE, VCODE)
      CALL H2CHR (4, 1, XDCODE, DCODE)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Rename and check limits,
C                                       Set defaults.
C                                       (Dparm 1,2 will be checked
C                                       after new cat header is
C                                       generated)
      IF (FPARM(8).EQ.0.0) FPARM(8) = 1.0
      COSMIN = DPARM(3)
      COSMAX = DPARM(4)
      SINMIN = DPARM(5)
      SINMAX = DPARM(6)
      IF ((COSMIN.EQ.0.0) .AND. (COSMAX.EQ.0.0) .AND. (SINMIN.EQ.0.0)
     *   .AND. (SINMAX.EQ.0.0)) THEN
          COSMAX = 1.0
          COSMIN = -1.0
          SINMAX = 1.0
          SINMIN = -1.0
          END IF
      IF (DPARM(7).EQ.0.0) THEN
         MSGTXT = 'MUST SPECIFY DPARM(7)'
         IRET = -1
         GO TO 990
         END IF
C                                       Set some defaults
C                                       Make constant dens the default
      IF ((CPARM(4).NE.2.0) .AND. (CPARM(4).NE.3.0) .AND.
     *   (CPARM(4).NE.4.0)) CPARM(4) = 1.0
      IF ((DCODE.NE.CODESD(2)) .AND. (DCODE.NE.CODESD(3)) .AND.
     *   (DCODE.NE.CODESD(4))) DCODE = CODESD(1)
      IF (FPARM(11).EQ.0.0) FPARM(11) = DPARM(7)
      IF (FPARM(13).EQ.0.0) FPARM(13) = DPARM(7)
      IF (CPARM(4).EQ.4.0) THEN
         IF (FPARM(14).EQ.0.0) FPARM(14)=0.5
         IF (FPARM(15).EQ.0.0) FPARM(15)=0.5
         IF (ABS(FPARM(14)+FPARM(15)-1.0).GE.1E-6) THEN
            MSGTXT = '*WARNING* FPARM(14)+FPARM(15).NE.1.0'
            CALL MSGWRT (8)
            END IF
         END IF
      IF ((FPARM(14).EQ.0.0) .OR. (FPARM(14).GT.1.0)) FPARM(14) = 1.0
      IF ((FPARM(15).EQ.0.0) .OR. (FPARM(15).GT.1.0)) FPARM(15) = 1.0
      IF (DPARM(7).LT.FPARM(11)) THEN
         MSGTXT = '*WARNING* MAX 1/2 DISK THICKNESS IS < FPARM(11)'
         CALL MSGWRT (8)
         END IF
      IF (TOL.EQ.0.0)  TOL  = 0.001
      IF (ERRV.EQ.0.0) ERRV = 0.001
C                                       Check if there's a flux cutoff
       IF (XCUT.NE.0.0) THEN
         XCUT = ABS (XCUT)
         WRITE (MSGTXT,1411) XCUT
         CALL MSGWRT (8)
         MSGTXT = ' but included in output cube'
         CALL MSGWRT (8)
         END IF
C                                       Determine which parameters
C                                       are to be held fixed. If
C                                       FIX(L)=1, then parameter L
C                                       is fixed.  If FIX(L)=0, the
C                                       parameter is allowed to vary
C                                       N is the number of free parms
      NFIX = CPARM(3) + .01
      N = MAXPRM
      DO 20 I = 1,N
         FIX(I) = 0
 20      CONTINUE
      DO 25 I = 1,N
         L = MAXPRM - I + 1
         IF (NFIX.GE.2**L) THEN
            FIX(L) = 1
            N = N - 1
            NFIX = NFIX - 2**L
            END IF
 25      CONTINUE
C                                      Check that Cparm(3) was okay
      IF (NFIX.NE.0) THEN
         MSGTXT = 'ILLEGAL VALUE OF CPARM(3)'
         IRET = -1
         GO TO 990
         END IF
C                                    If one of fparm 14,15 are fixed
C                                    then the other must also be fixed
      IF ((CPARM(4).EQ.4.0) .AND. (FIX(14).NE.FIX(15)) .AND.
     *   ((FIX(14).EQ.1) .OR. (FIX(15).EQ.1))) THEN
         FIX(14) = 1
         FIX(15) = 1
         N = N - 1
         END IF
C                                      If vcode is RC ensure that
C                                      FPARM7 and FPARM8 are fixed
      IF (VCODE.EQ.CODESV(4)) THEN
C                                      Factor = 1 by default
         IF (FPARM(6).EQ.0.0) FPARM(6) = 1.0
         IF (FIX(7).EQ.0) THEN
            FIX(7) = 1
            N = N - 1
            I = 7
            WRITE (MSGTXT,1190) I
            CALL MSGWRT (3)
            END IF
         IF (FIX(8).EQ.0) THEN
            FIX(8) = 1
            N = N - 1
            I = 8
            WRITE (MSGTXT,1190) I
            CALL MSGWRT (3)
            END IF
         END IF
C                                      Fix FPARM(10) if
C                                      RADIAL density is constant
      IF ((DCODE.EQ.CODESD(1)) .AND. (FIX(10).EQ.0)) THEN
         FIX(10) = 1
         N = N - 1
         I = 10
         WRITE (MSGTXT,1190) I
         CALL MSGWRT (3)
         END IF
C                                      Fix FPARM(11,13,14,15) if
C                                      VERT density is constant
      IF (CPARM(4).LT.4) THEN
         DO 30 I = 11,15
            IF (FIX(I).EQ.0) THEN
               IF (((CPARM(4).EQ.1) .AND. (I.GE.11)) .OR.
     *            ((CPARM(4).EQ.2) .AND. (I.GE.13)) .OR.
     *            ((CPARM(4).EQ.3) .AND. ((I.EQ.11) .OR. (I.GE.14))))
     *            THEN
                  FIX(I) = 1
                  N = N - 1
                  WRITE (MSGTXT,1190) I
                  CALL MSGWRT (3)
                  END IF
               END IF
 30         CONTINUE
         END IF
C                                      Fix FPARM(12) if dcode.ne.rg
      IF ((DCODE.NE.CODESD(4)) .AND. (FIX(12).EQ.0)) THEN
         FIX(12) = 1
         N = N - 1
         I = 12
         WRITE (MSGTXT,1190) I
         CALL MSGWRT (3)
         END IF
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE='MA'
C
      CALL CATDIR  ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN,'      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up some defaults for
C                                       ring distribution
      IF (DCODE.EQ.CODESD(4)) THEN
         IF (FPARM(12).EQ.0.0) FPARM(12) = FPARM(10)
         IF (CPARM(7).EQ.0.0) CPARM(7) = 3.0
         END IF
C
C                                        Check dparm 1,2 now
      DSKMIN = DPARM(1)
      DSKMAX = DPARM(2)
      IF (DPARM(2).EQ.0.0) DSKMAX = ABS (1024.0*CATR(KRCIC+1)*3600.0)
      IF ((DSKMAX.LE.DSKMIN) .OR. (DSKMIN.LT.0.0)) THEN
         IRET = 4
         MSGTXT = 'ILLEGAL VALUES OF DPARM(1) AND/OR DPARM(2)'
         GO TO 990
         END IF
      IF (FPARM(7).EQ.0.0) FPARM(7) = DSKMAX
      IF (DSKMAX.LT.FPARM(7)) THEN
         MSGTXT = '*WARNING* MAX DISK RADIUS IS < FPARM(7)'
         CALL MSGWRT (6)
         END IF
C                                      Check Rparms, Vparms
      IF (VCODE.EQ.CODESV(4)) THEN
         DO 40 I = 1,29
            IF ((RPARM(I).GE.RPARM(I+1)) .AND. (RPARM(I).LT.DSKMAX))
     *         THEN
               IRET = 4
               MSGTXT = 'RPARM MUST INCREASE OUT TO DPARM(2)'
               GO TO 990
               END IF
            IF ((VPARM(I).EQ.0.0) .AND. (RPARM(I).GT.0.0)) IRET = 100
            IF ((RPARM(I+1).EQ.0.0) .AND. (I.GT.1)) GO TO 50
 40         CONTINUE
 50      IF (IRET.EQ.100) THEN
            MSGTXT = '*WARNING* YOU MAY HAVE OMITTED SOME VPARMS'
            CALL MSGWRT (8)
            IRET = 0
            END IF
         END IF
C
      IRET = 4
      WRITE (MSGTXT,1105) (DPARM(I), I = 1,5)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1106) (DPARM(I), I = 6,7)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1100) (CPARM(I), I = 1,5)
      CALL MSGWRT (5)
      WRITE(MSGTXT,1101) (CPARM(I), I = 6,7)
      CALL MSGWRT (5)
C                                       Get user modification to CATBLK
      CALL NEWHED (IRET)
      IF (IRET.NE.0) GO TO 999
      MSGTXT = 'Initial FPARM:'
      CALL MSGWRT (5)
      FSTRNG(1) = 'X center pixel'
      FSTRNG(2) = 'Y center pixel'
      FSTRNG(3) = 'Position angle'
      FSTRNG(4) = 'Inclination'
      FSTRNG(5) = 'System velocity'
      FSTRNG(6) = 'Max rotation velocity'
      FSTRNG(7) = 'Radius of Vmax "'
      FSTRNG(8) = 'Brandt index'
      FSTRNG(9) = 'Max density'
      FSTRNG(10) = 'Radial dens scale lng " '
      FSTRNG(11) = 'Perp EX dens scale lng "'
      FSTRNG(12) = 'RG inner scale length " '
      FSTRNG(13) = 'Perp GS dens scale lng "'
      FSTRNG(14) = 'nf1 EX fraction density '
      FSTRNG(15) = 'nf2 GS fraction density '
      DO 60 I = 1,MAXPRM
         WRITE (MSGTXT,1090) I, FSTRNG(I), FPARM(I)
         CALL MSGWRT (5)
 60      CONTINUE
      WRITE (MSGTXT,1070) (BLC(I), I = 1,7)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1080) (TRC(I), I = 1,7)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1110) DSKMAX, DSKMIN
      CALL MSGWRT (5)
      WRITE (MSGTXT,1050) TOL
      CALL MSGWRT (5)
      WRITE (MSGTXT,1060) VCODE
      CALL MSGWRT (5)
      WRITE (MSGTXT,1290) DCODE
      CALL MSGWRT (5)
      IF (DCODE.EQ.CODESD(4)) THEN
         IF (CPARM(7).GT.2.8) THEN
            MSGTXT = '     ring is Gaussian'
            CPARM(7) = 3.0
         ELSE
            MSGTXT = '     ring is exponential'
            CPARM(7) = 2.0
            END IF
         CALL MSGWRT (5)
         END IF
      IF (CPARM(4).GE.4.0) THEN
         WRITE (MSGTXT,1305) CODESD(2), CODESD(3)
         CALL MSGWRT (5)
      ELSE
         WRITE (MSGTXT,1300) CODESD(IFIX(CPARM(4)))
         CALL MSGWRT (5)
         END IF
      WRITE (MSGTXT,1220) (MAXPRM-N)
      CALL MSGWRT (5)
      IF (VCODE.EQ.CODESV(4)) THEN
         MSGTXT = 'Rotation curve specified by VPARM, RPARM'
         CALL MSGWRT (5)
         END IF
C                                       Calculate the constant for
C                                       converting from n(cm**-3),
C                                       dz(arcsec), dV(km/s), to I
C                                       [ Jy/(pixel area)]
      IF (CPARM(2).EQ.0.0) THEN
         IRET = -1
         WRITE (MSGTXT,1250) CPARM(2)
         GO TO 990
         END IF
      CMASEC = CPARM(2) * 1.4959459E19
      SFREQ     = 1420.40575E6 - ((CATD(KDCRV)/2.997924562E8)
     *           *1420.40575E6)
      JY2DEG = (CATR(KRBMJ)*3600.0*CATR(KRBMN)*3600.0*SFREQ**2)
     *           /1.22214E24
      CONST    = CMASEC * JY2DEG / 1.823E18
      WRITE (MSGTXT,1120) CONST
      CALL MSGWRT (5)
C                                       Determine the velocity in-
C                                       crement in km/s from the
C                                       catalog header
      DELVEL = ABS (CATR(KRCIC)) / 1000.0
      WRITE (MSGTXT,1230) DELVEL
      CALL MSGWRT (5)
C                                       Check to see if velocity
C                                       smoothing is required
C                                       and convert to pixels to
C                                       access VSMTH
      IF (CPARM(6).GT.0.0) THEN
         WIDTH = CPARM(6) / DELVEL
         IF ((WIDTH.LT.0.1) .OR. (WIDTH.LE.((TRC(1)-BLC(1)+1.0)/3.0)))
     *      THEN
            MSGTXT = 'Vel FWHM must be > 0.1 or < [row_out/3] channels'
            CALL MSGWRT (5)
            CPARM(6) = 0.0
            MSGTXT = '*WARNING* VELOCITY SMOOTHING IGNORED*'
            CALL MSGWRT (6)
         ELSE
            WRITE (MSGTXT,1237) WIDTH
            CALL MSGWRT (5)
C                                       Print a warning if FWHM is
C                                       less than the channel width
            IF (WIDTH.LT.1.0) THEN
               MSGTXT = '*WARNING* Vel smoothing is < one channel'
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
C                                       Ensure that size of each
C                                       RA-DEC plane is less than
C                                       maximum allowed.
C     IF ((CATBLK(KINAX+1).GT.MAXXPT) .OR. (CATBLK(KINAX+2).GT.MAXYPT))
C    *   THEN
C        WRITE (MSGTXT,1010) MAXXPT, MAXYPT
C        GO TO 990
C        END IF
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Lengths of output array
      NXOUT = CATBLK(KINAX+1)
      NYOUT = CATBLK(KINAX+2)
      NVOUT = CATBLK(KINAX)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CUBITI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1030 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1040 FORMAT ('CUBITI: ERROR',I3,' CREATING OUTPUT FILE')
 1050 FORMAT ('Convergence criterion = ', E10.3)
 1060 FORMAT ('Form of rotation curve is ',A4)
 1070 FORMAT ('BLC = ',7(F5.1,2X))
 1080 FORMAT ('TRC = ',7(F5.1,2X))
 1090 FORMAT ('(',I2,')  ',A,F10.4)
 1100 FORMAT ('Initial CPARM: ',5(F9.3,1X))
 1101 FORMAT ('               ',2(F9.3,1X))
 1105 FORMAT ('Initial DPARM: ',5(F9.3,1X))
 1106 FORMAT ('               ',2(F9.3,1X))
 1110 FORMAT ('DSKMAX = ',F10.2,3X,'DSKMIN = ',F9.2,3X,'arcsec')
 1120 FORMAT ('[cm**-3 arcsec]/[km/s] X ',E9.4,' = Jy/beam')
 1190 FORMAT ('FPARM(',I2,') is fixed and not used')
 1220 FORMAT (I3,' parameters are fixed')
 1230 FORMAT ('Incremental velocity is ',F8.4,1X,'km/s')
 1237 FORMAT ('FWHM of vel smoothing Gaussian = ',F8.3,' channels')
 1250 FORMAT ('Is the galaxy really',1X,F3.1,1X,'Mpc away??')
 1290 FORMAT ('Radial density distribution is ',1X,A4)
 1300 FORMAT ('Perpendicular density distribution is ',1X,A4)
 1305 FORMAT ('Perpendicular density distribution is ',1X,A4,'+',2X,A4)
 1411 FORMAT ('Ignoring all ABS(DATA) <  ', F8.4, 2X, 'Jy/beam in fit')
      END
      SUBROUTINE CUBITM (N, DATA, JBLANK, MX, MY, DARRAY, IRET)
C-----------------------------------------------------------------------
C   CUBITM reads a cube into an array, DATA, calls the subroutine
C   LSTSQ1 (which has been modified from LMSTR1) to do a least squares
C   fit to the data using input parameters and then writes either the
C   residuals or the model (RESULT(J)) into a new cube.
C   Input :
C      N        I      Number of free parameters
C      MX       I      X output dimension
C      MY       I      Y output dimension
C   Output:
C      DATA     R(*)   Input data
C      JBLANK   I(*)   # blanked points before pixel of interest
C      DARRAY   D(*)   Work array (MX,MY,3)
C      IRET     I      Return code, 0 => OK, otherwise abort.
C   Subprograms called:
C      AIPS sublibraries...ZPHFIL, ZOPEN, COMOF3, MINI3, MDISK,
C                          MSGWRT, ZCLOSE, CATIO
C                          QRFAC, COVAR
C      CUBIT subroutine....FCN,.LSTSQ1
C-----------------------------------------------------------------------
      INTEGER   N, JBLANK(*), MX, MY, IRET
      REAL      DATA(*)
      DOUBLE PRECISION DARRAY (MX,MY,3)
C
      INCLUDE 'CUBSIZ.INC'
      CHARACTER IFILE*48
      INTEGER   J, JM, M, MTOT, JBL, IFLAG, K, NICT, NICTP, IROUND,
     *   LUNI, LUNO, NYI, NXI, WINI(4),NXO, NYO, WINO(4), BOI,
     *   BOO, LIM2, LIM3, LIM4, LIM5, LIM6,LIM7, I1, I2, I3, I4, I5, I6,
     *   I7, IPOS(7), CORN(7), BOTEMP,LIMO, LIMIT, IBIND, OBIND, INDI,
     *   INDO, LIM1, I, L, IT, IM1,OUTCNT, INFO, IPVT(MAXPRM),
     *   IPVT2(MAXPRM), NWRDRE
      REAL      OUTMAX, OUTMIN, XBUFF1(16384), XBUFF2(16384),
     *   RAD, ERROR(MAXPRM), PI, ZSUM, ERF, ZE, ZG
      DOUBLE PRECISION X(MAXPRM), DERP(MAXPRM), DERM(MAXPRM),
     *   FJAC(MAXPRM,MAXPRM), WA1(MAXPRM), WA2(MAXPRM), WA3(MAXPRM),
     *   RESULT(2), STOL
      LONGINT   LPRESU, IPRESU, LP
      LOGICAL   T, F, BLNKD
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /LEAST/ IT
      COMMON /JLEAST/ JBL, NICT, NICTP, MTOT
      EQUIVALENCE  (BUFF1, XBUFF1), (BUFF2, XBUFF2)
      DATA LUNI, LUNO /16,17/
      DATA ERROR /MAXPRM*0.0/
      DATA T, F /.TRUE.,.FALSE./
      DATA PI /3.1415926536/
C-----------------------------------------------------------------------
C     SECTION A -- READ INPUT CUBE INTO ARRAY, DATA.
C
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = .FALSE.
      RAD   = 180.0 / PI
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
C                                      Initialize counters
C                                      M, J, # unblanked pts.
C                                      MTOT, JM, total # pts.
      M    = 0
      MTOT = 0
      J    = 0
      JM   = 0
C                                       Loop
      DO 700 I7 = 1,LIM7
         CORN(7) = I7
         IPOS(7) = BLC(7) + I7 - 0.9
         DO 600 I6 = 1,LIM6
            CORN(6) = I6
            IPOS(6) = BLC(6) + I6 - 0.9
            DO 500 I5 = 1,LIM5
               CORN(5) = I5
               IPOS(5) = BLC(5) + I5 - 0.9
               DO 400 I4 = 1,LIM4
                  CORN(4) = I4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  DO 300 I3 = 1,LIM3
                     CORN(3) = I3
                     IPOS(3) = BLC(3) + I3 - 0.9
C                                       Init. files, first input.
                     CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3),
     *                  BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1099) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'READ', IRET
                        GO TO 990
                        END IF
                     OUTCNT = NYO
C                                       Start to loop over the plane
                     DO 220 I2 = 1,LIM2
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'READ', IRET
                           GO TO 990
                           END IF
                        DO 165 I1 = 1,LIM1
                           JM = JM + 1
C                          IF (JM.GT.MAXTOT) THEN
C                             IRET = -1
C                             WRITE (MSGTXT,1300) MAXTOT
C                             GO TO 990
C                             END IF
                           DATA(JM) = XBUFF1(IBIND+I1-1)
                           IF (DATA(JM).NE.FBLANK) J = J + 1
 165                       CONTINUE
                        OUTCNT = OUTCNT - 1
                        IF (OUTCNT.LT.0) THEN
                           MSGTXT = 'CUBITM: OUTCNT.LT.0! TOO MANY'
     *                        // ' OUTPUT ROWS RETURNED'
                           GO TO 990
                           END IF
 220                    CONTINUE
                     IF (OUTCNT.GT.0) THEN
                        MSGTXT = 'CUBITM: NOT ENOUGH ROWS RETURNED'
                        GO TO 990
                        END IF
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
      MSGTXT = 'Input cube read'
      CALL MSGWRT (5)
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      M    = J
      MTOT = JM
C                                       Write total # pts and
C                                       # unblanked pts.
      WRITE (MSGTXT,1280) MTOT, M
      CALL MSGWRT (5)
C                                       get memory for RESULT
      NWRDRE = (2 * M + 10) / 1024 + 1
      CALL ZMEMRY ('GET ', 'CUBITM', NWRDRE, RESULT, IPRESU, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1700) IRET, NWRDRE
         GO TO 990
         END IF
      IF (IPRESU.GE.0) THEN
         LPRESU = 1 + (IPRESU+1)/2
      ELSE
         LPRESU = 1 + IPRESU / 2
         END IF
C-----------------------------------------------------------------------
C
C     SECTION B - DO LEAST SQUARES FIT
C
      IF (N.GT.0) THEN
C                                       Initialize X's
         DO 720 L = 1,N
            X(L) = 0.0D0
 720        CONTINUE
C
C                                       Put FPARM's to be fitted
C                                       into X(I).  Index I: parms
C                                       to be fitted.  Index L:
C                                       total # parameters.
         L = 0
         DO 730 I = 1,N
 725        L = L + 1
            IF (FIX(L).EQ.1) THEN
               WRITE (MSGTXT,1390) L, FSTRNG(L), FPARM(L)
               CALL MSGWRT (5)
               GO TO 725
            ELSE
               X(I) = FPARM(L)
               PSTRNG(I) = FSTRNG(L)
               FPNUM(I) = L
               END IF
 730        CONTINUE
         DO 735 I = L+1,MAXPRM
            WRITE (MSGTXT,1390) I, FSTRNG(I), FPARM(I)
            CALL MSGWRT (5)
 735        CONTINUE
C                                       let someone else do the work
         IFLAG = 0
         STOL = TOL * 1.0D0
         CALL LSTSQ1 (M, N, X, RESULT(LPRESU), FJAC, MAXPRM, STOL, INFO,
     *      IPVT, DATA, JBLANK, MX, MY, DARRAY)
         IF (INFO.EQ.-100) THEN
            MSGTXT = 'LSTSQ1 (FCN) ERROR'
            GO TO 990
            END IF
         MSGTXT = 'Least squares fit completed'
         CALL MSGWRT (5)
C                                       Put final fitted X(I)'s back
C                                       into FPARM's
         L = 0
         DO 750 I = 1,N
 740        L = L + 1
            IF (FIX(L).EQ.1) GO TO 740
            FPARM(L) = X(I)
 750        CONTINUE
         END IF
C                                        Final call to FCN with
C                                        FLAG=-1 computes model
C                                        or residuals.
      NICT = 0
      NICTP = 0
      IFLAG = -1
      K = N
      IF (N.EQ.0) K = MAXPRM
      CALL FCN (M, K, X, RESULT(LPRESU), DERP, DERM, IFLAG, DATA,
     *   JBLANK, MX, MY, DARRAY)
      IF (IFLAG.EQ.-100) THEN
         MSGTXT = 'FINAL CALL FCN ERROR'
         GO TO 990
         END IF
C                                       Print out no. of problem
C                                       iterations for RC case
      IF (NICT.NE.0) THEN
         WRITE (MSGTXT,1460) NICT
         CALL MSGWRT (8)
         END IF
      IF (NICTP.NE.0) THEN
         WRITE (MSGTXT,1470) NICTP
         CALL MSGWRT (8)
         END IF
C                                       Don't print out solutions
C                                       if no fit is being done
C                                       Print out the solution
      IF (N.NE.0) THEN
         WRITE (MSGTXT,1400) INFO
         CALL MSGWRT (5)
C-----------------------------------------------------------------------
C
C     SECTION C - ERROR ANALYSIS
C
C
C                                        Computation covariance
C                                        matrix, prepare calling of
C                                        COVAR, zero lower triangle
C                                        of FJAC
         DO 760 I = 2,N
            IM1 = I - 1
            DO 755 L = 1,IM1
               FJAC(I,L) = 0.0
 755           CONTINUE
 760        CONTINUE
         CALL QRFAC (N, N, FJAC, MAXPRM, T, IPVT2, N, WA1, WA2, WA3)
         DO 765 I = 1,N
            FJAC(I,I) = WA1(I)
            L = IPVT2(I)
            IPVT2(I) = IPVT(L)
 765        CONTINUE
         STOL = 1.0D-6
         CALL COVAR (N, FJAC, MAXPRM, IPVT2, STOL, WA1)
         DO 770 I = 1,N
            FJAC(I,I) = ERRV * SQRT(FJAC(I,I))
 770        CONTINUE
C                                        Associate correct error values
C                                        with FPARMs
         L = 0
         DO 780 I = 1,N
 775        L = L + 1
            ERROR(L) = 0.0
            IF (FIX(L).EQ.1) GO TO 775
            ERROR(L) = FJAC(I,I)
 780        CONTINUE
C                                        Print out values w errors
         WRITE (MSGTXT,1410) IT
         CALL MSGWRT (5)
         DO 790 I = 1,MAXPRM
            WRITE (MSGTXT,1440) I, FSTRNG(I), FPARM(I), ERROR(I)
            IF (FIX(I).EQ.1) MSGTXT(43:51) = '   fixed '
            CALL MSGWRT (5)
 790        CONTINUE
         END IF
      ZSUM = PI * 0.3086 * CPARM(2) * FPARM(9) / 1.8 / 3.6
      IF (CPARM(4).EQ.1.0) THEN
         ZSUM = ZSUM * 2.0 * DPARM(7)
      ELSE
         ZE = 2.0 * FPARM(11) * FPARM(14) *
     *      (1.0 - EXP (-DPARM(7)/FPARM(11)))
         ZG = SQRT (PI) * FPARM(13) * FPARM(15) *
     *      ERF (DPARM(7)/FPARM(13))
         IF (CPARM(4).EQ.2.0) THEN
            ZSUM = ZSUM * ZE
         ELSE IF (CPARM(4).EQ.3.0) THEN
            ZSUM = ZSUM * ZG
         ELSE IF (CPARM(4).EQ.4.0) THEN
            ZSUM = ZSUM * (ZE + ZG)
            END IF
         END IF
      WRITE (MSGTXT,1790) ZSUM
      CALL MSGWRT (5)
      IF (DCODE.EQ.'CD') THEN
         MSGTXT = 'times  1.0      independent of radius'
      ELSE IF (DCODE.EQ.'EX') THEN
         WRITE (MSGTXT,1791) FPARM(10)
      ELSE IF (DCODE.EQ.'GS') THEN
         WRITE (MSGTXT,1792) FPARM(10)
      ELSE IF ((DCODE.EQ.'RG') .AND. (CPARM(7).EQ.2.0)) THEN
         WRITE (MSGTXT,1793) CPARM(5), FPARM(12), '< ', CPARM(5)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1794) CPARM(5), FPARM(10), '>=', CPARM(5)
         CALL MSGWRT (5)
         MSGTXT = '    where R=radius in asec'
      ELSE IF ((DCODE.EQ.'RG') .AND. (CPARM(7).EQ.3.0)) THEN
         WRITE (MSGTXT,1795) CPARM(5), FPARM(12), '< ', CPARM(5)
         CALL MSGWRT (5)
         WRITE (MSGTXT,1795) CPARM(5), FPARM(10), '>=', CPARM(5)
         CALL MSGWRT (5)
         MSGTXT = '    where R=radius in asec'
         END IF
      CALL MSGWRT (5)
      MSGTXT = 'If you use the results of this task in a paper, please'
     *   // ' reference'
      CALL MSGWRT (5)
      MSGTXT = 'Irwin, J. A. 1994, ApJ, 429, 618'
      CALL MSGWRT (5)
C-----------------------------------------------------------------------
C
C     SECTION D - WRITE RESULT INTO OUTPUT CUBE
C
C                                        Open and init output file
C                                        for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Initialize counters
      J  = 0
      JM = 0
      JBL= 0
C                                       Loop
      DO 870 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 860 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 850 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 840 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 830 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
C                                       Initialize output file
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3),
     *                  BOTEMP, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1099) IRET
                        GO TO 990
                        END IF
                     BOO = BOTEMP + 1
                     CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO,
     *                  BUFF2, JBUFSZ, BOO, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'WRIT', IRET
                        GO TO 990
                        END IF
C                                       Loop over the plane
                     DO 820 I2 = 1,LIM2
                        IPOS(2) = BLC(2) + I2 - 0.9
                        IPOS(1) = IROUND(BLC(1))
C
                        CALL MDISK ('WRIT', LUNO, INDO, XBUFF2, OBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'WRIT', IRET
                           GO TO 990
                           END IF
                        DO 810 I1 = 1,LIM1
                           JM = JBL + J + 1
                           IF (DATA(JM).NE.FBLANK) THEN
                              J = J + 1
                              LP = J - 1 + LPRESU
                              XBUFF2(OBIND+I1-1) = RESULT(LP)
                           ELSE
                              XBUFF2(OBIND+I1-1) = FBLANK
                              JBL = JBL + 1
                              END IF
 810                       CONTINUE
C                                       Check max, min, blanking.
                        LIMIT = OBIND + LIMO
                        DO 815 I1 = OBIND,LIMIT
                           IF (XBUFF2(I1).EQ.FBLANK) THEN
                              BLNKD = .TRUE.
                           ELSE
                              OUTMAX = MAX (OUTMAX, XBUFF2(I1))
                              OUTMIN = MIN (OUTMIN, XBUFF2(I1))
                              END IF
 815                       CONTINUE
 820                    CONTINUE
C                                       Flush buffer.
                     CALL MDISK ('FINI', LUNO, INDO, XBUFF2, OBIND,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1120) 'FINI', IRET
                        GO TO 990
                        END IF
 830                 CONTINUE
 840              CONTINUE
 850           CONTINUE
 860        CONTINUE
 870     CONTINUE
      MSGTXT = 'Output cube written'
      CALL MSGWRT (5)
C                                      Write total # output pts and #
C                                      unblanked output pts
      WRITE (MSGTXT,1290) JM, J
      CALL MSGWRT (5)
C                                      Update CATBLK
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', DATA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
C                                       Mark blanking in CATBLK
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Close output map
      CALL ZCLOSE (LUNO, INDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180)IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CUBITM: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('CUBITM: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('CUBITM: COMOF3 ERROR',I3)
 1100 FORMAT ('CUBITM: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('CUBITM: ',A4,' ERROR',I3)
 1180 FORMAT ('CUBITM: ZCLOSE ERROR',I3)
 1260 FORMAT ('CUBITM: CATIO ERROR',I3,' UPDATING CATBLK')
 1280 FORMAT ('#  input pts =',I8,1X,'# unblanked input pts =',I8)
 1290 FORMAT ('# output pts =',I8,1X,'# unblanked output pts =',I8)
 1390 FORMAT ('Fixed FPARM(',I2,') (',A,') =',F12.3)
 1400 FORMAT ('LMSTR INFORMATION PARAMETER =',I4)
 1410 FORMAT ('After',I4,' iterations the solution is FPARM (Errors)')
 1440 FORMAT (I2,2X,A,F11.4,'  (',F9.4,')')
 1460 FORMAT ('** Rot curve fit is only approximate for',I8,'pts')
 1470 FORMAT ('Rot curve fit is accurate for',I8,'pts')
 1700 FORMAT ('CUBITM: ERROR',I3,' GETTING',I10,' KILOWORDS OF MEMORY')
 1790 FORMAT ('Vertical column density',F8.4,' * 10^20 /cm^2')
 1791 FORMAT ('times  exp (-R/',F9.4,')  where R=radius in asec')
 1792 FORMAT ('times  exp (-(R/',F9.4,')^2)  where R=radius in asec')
 1793 FORMAT ('times  exp (-(',F9.4,'-R)/',F9.4,')    R ',A,F9.4)
 1794 FORMAT ('times  exp (-(R-',F9.4,')/',F9.4,')    R ',A,F9.4)
 1795 FORMAT ('times  exp (-((R-',F9.4,')/',F9.4,')^2)    R ',A,F9.4)
      END
      SUBROUTINE CUBITH (IRET)
C-----------------------------------------------------------------------
C   CUBITH will update
C   the history of the output map.
C
C   Output: IRET     I     Error code. 0=ok
C   Subprograms called:
C      AIPS SUBLIBRARIES...HIINIT, HISCOP, MSGWRT, HENCO1, HENCOO,
C                          HIADD, HICLOS, CATIO
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CODESV(4)*4, NOTTYP*2
      INTEGER   JBL, NICT, NICTP, MTOT, LUN1, LUN2, IERR, I, IT, ICOUNT,
     *   I1
      REAL      XBUFF2(256), OUTPUT(5)
      LOGICAL   T
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /LEAST/ IT
      COMMON /JLEAST/ JBL, NICT, NICTP, MTOT
      EQUIVALENCE (BUFF2, XBUFF2)
      DATA CODESV /'CV  ', 'SB  ', 'BR  ', 'RC  '/
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       WRITE HISTORY.
      CALL HIINIT (3)
C                                       COPY/OPEN HISTORY FILE.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       NEW HISTORY
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC, TRC
      WRITE (MSGTXT,2000)TSKNAM,(BLC(I),I=1,3),(TRC(I),I=1,3)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       VCODE, DCODE
      WRITE (MSGTXT,2002) TSKNAM, VCODE, DCODE
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      LIST ALL CPARM'S
      WRITE (MSGTXT,2006) TSKNAM, (CPARM(I),I=1,5)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2006) TSKNAM, (CPARM(I),I=6,10)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      LIST ALL DPARM'S
      WRITE (MSGTXT,2008) TSKNAM, (DPARM(I),I=1,5)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2008) TSKNAM, (DPARM(I),I=6,10)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      SHOW VALUE of ICUT
      WRITE (MSGTXT,2009) TSKNAM, XCUT
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      NOTE ROTATION CURVE FOR
C                                      VCODE=RC
      IF (VCODE.EQ.CODESV(4)) THEN
         DO 15 ICOUNT = 1,2
            IF (ICOUNT.EQ.1) WRITE (MSGTXT,1050) TSKNAM
            IF (ICOUNT.EQ.2) WRITE (MSGTXT,1060) TSKNAM
            CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
            DO 10 I1 = 1,30,5
               IF (ICOUNT.EQ.1) CALL RCOPY (5, VPARM(I1), OUTPUT)
               IF (ICOUNT.EQ.2) CALL RCOPY (5, RPARM(I1), OUTPUT)
               WRITE (MSGTXT,1070) TSKNAM, (OUTPUT(I), I = 1,5)
               CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 20
 10            CONTINUE
 15         CONTINUE
         END IF
C                                      Write number of iterations
      WRITE (MSGTXT,2010) TSKNAM, IT
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      List final fitted parameters
      WRITE (MSGTXT,2011) TSKNAM
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2012) TSKNAM, (FPARM(I), I = 1,4)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2012) TSKNAM, (FPARM(I), I = 5,8)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2012) TSKNAM, (FPARM(I), I = 9,12)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (MSGTXT,2013) TSKNAM, (FPARM(I), I = 13,15)
      CALL HIADD (LUN2, MSGTXT, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      GO TO 25
C                                       HI error
 20   MSGTXT = 'HISTORY UPDATE ERROR'
      CALL MSGWRT (7)
C                                       Close HI file
 25   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAFHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1050 FORMAT (A6,'USER SPECIFIED ROTATION CURVE--VPARMS:')
 1060 FORMAT (A6,'RPARMS:')
 1070 FORMAT (A6,5(F8.2))
 2000 FORMAT (A6,'BLC =',3(F6.0,','),1X,'TRC =',3(F6.0,','))
 2002 FORMAT (A6,'VCODE = ''',A4,'''',1X,' DCODE = ''',A4,'''')
 2006 FORMAT (A6,'CPARM = ',5(F8.1))
 2008 FORMAT (A6,'DPARM = ',5(F8.1))
 2009 FORMAT (A6,'ICUT = ', F8.4)
 2010 FORMAT (A6,'/ FINAL SOLUTION REQUIRED',I4,1X,'ITERATIONS')
 2011 FORMAT (A6,'FPARM = ',5X,'/  FINAL VALUES')
 2012 FORMAT (A6,4(F10.4,1X))
 2013 FORMAT (A6,3(F10.4,1X))
      END
      SUBROUTINE NEWHED (IRET)
C-----------------------------------------------------------------------
C   The following functions are performed in NEWHED:
C       1) Modifying the catalog header block to represent the output
C            CATBLK(KIDIM)   = the number of axes,
C            CATBLK(KINAX+i) = the dimension of each axis
C       2) Checking the input image and/or input parameters.
C             For example, checking that the first axis is velocity.
C       3) Setting default values of some of the input parameters
C          Most of the defaults are set in CUBITI.  Here, the default
C          VCODE, and DCODE are set
C   The catalog block is updated when the history file is written.
C   Output:
C     IRET      I        Return error code, 0=>OK, otherwise abort.
C   Input (COMMON):
C      CATBLK   I(256)   Output catalog header, also CATR,CATD,CATH
C      CATOLD   I(256)   Input catalog header, also OLDR,OLDD,OLDH
C   Output:
C      CATBLK   I(256)   Modified output catalog header.
C   Subprograms called:
C      AIPS sublibraries...CHPACK, AXEFND, CHR2H, MSGWRT
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER ATYPES(3)*8, FCHARS(3)*4, CODESV(4)*4, CODESD(4)*4,
     *   UNITS*8, TEMP*8
      INTEGER   LIMIT, I, FIRSTI, FIRSTO, NTYPES, IOFF, IERR,
     *   INC, INDEX, NCHTYP(3)
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'VELO','VELO','FELO'/
C                                       value of VCODEs and DCODEs
      DATA CODESV /'CV  ','SB  ','BR  ','RC  '/
      DATA CODESD /'CD  ','EX  ','GS  ','RG  '/
C                                       Output units
      DATA UNITS /'JY/BEAM '/
C                                       Allowed number of axis types
C                                       and types. NCHTYP refers to the
C                                       number of characters which need
C                                     to match the specified axis type.
      DATA NTYPES /3/
      DATA ATYPES /'VELOCITY','VELO    ','FELO    '/
      DATA NCHTYP /3*3/
C-----------------------------------------------------------------------
C                                       Set default VCODE
      IF ((VCODE.NE.CODESV(2)) .AND. (VCODE.NE.CODESV(3)) .AND.
     *   (VCODE.NE.CODESV(4))) VCODE = CODESV(1)
C                                       check parameters
      IF ((VCODE.NE.CODESV(1)) .AND. ((FPARM(6).EQ.0.0) .OR.
     *   (FPARM(7).EQ.0.0))) THEN
         MSGTXT = 'FPARM(6) OR FPARM(7) NOT SPECIFIED'
         IRET = -1
         GO TO 990
         END IF
      IF ((VCODE.EQ.CODESV(4)) .AND. (RPARM(2).EQ.0.0) .AND.
     *   (VPARM(2).EQ.0.0)) THEN
          MSGTXT = 'USER SPECIFIED ROT CURVE REQUIRES VPARM, ' //
     *       'RPARM.NE.0'
          IRET=-1
          GO TO 990
          END IF
       IF ((DCODE.NE.CODESD(1)) .AND. (FPARM(10).EQ.0.0)) THEN
          WRITE (MSGTXT,1070) DCODE
          IRET = -1
          GO TO 990
          END IF
C                                       Set output units.
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       Check allowed axis types
      DO 30 I = 1,NTYPES
         CALL AXEFND (NCHTYP(I), ATYPES(I), KICTPN, OLDH(KHCTP),
     *      IOFF, IERR)
         IF (IERR.EQ.0) GO TO 40
 30      CONTINUE
      IOFF = -1
C                                       IOFF is axis number
C                                       Check if axis first
 40   IF (IOFF.NE.0) THEN
         IRET = 1
         IF (IOFF.GT.0) MSGTXT = 'VELOCITY IS NOT FIRST AXIS, ' //
     *      'TRANSPOSE IMAGE'
         IF (IOFF.LT.0) MSGTXT = 'REQUIRED FIRST AXIS (VELOCITY)' //
     *      ' NOT FOUND IN IMAGE'
         GO TO 990
         END IF
C                                       Set axes in output CATBLK.
      FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
      INC = 2
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX-1+I) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP-1+I) = OLDR(KRCRP-1+I) - BLC(I) + 1.0
         CATR(KRCIC-1+I) = CATR(KRCIC-1+I)
         CATD(KDCRV-1+I) = OLDD(KDCRV-1+I)
         IF (CATBLK(KIALT).NE.0) THEN
            INDEX = KHCTP + (I-1) * INC
            CALL H2CHR (4, 1, CATH(INDEX), TEMP)
            IF ((TEMP(1:4).EQ.FCHARS(1)) .OR. (TEMP(1:4).EQ.FCHARS(2))
     *         .OR. (TEMP(1:4).EQ.FCHARS(3)))
     *         CATR(KRARP) = CATR(KRARP) - BLC(I) + 1.0
            END IF
 80      CONTINUE
C                                       Put other checks here.
C                                       Check inclination range.
      IF ((FPARM(4).GE.89.1) .OR. (FPARM(4).LE.0.9)) THEN
         IRET = -1
         MSGTXT = 'REQUIRE 1 <= FPARM(4) <= 89'
         GO TO 990
         END IF
C                                       Ensure that the central
C                                       position specified is
C                                       inside the subimage window.
      IF ((FPARM(1).LT.BLC(2)) .OR. (FPARM(1).GT.TRC(2)) .OR.
     *   (FPARM(2).LT.BLC(3)) .OR. (FPARM(2).GT.TRC(3))) THEN
         MSGTXT = 'CENTER POSITION IS OUTSIDE OF WINDOW'
         IRET = -1
         GO TO 990
         END IF
      IF ((DCODE.EQ.CODESD(4)) .AND. (CPARM(5).EQ.0.0)) THEN
         MSGTXT = '**WARNING** CPARM(5) NOT SPECIFIED'
         CALL MSGWRT (6)
         DCODE = CODESD(3)
         MSGTXT = '*CHANGING DCODE TO GS*'
         IRET = 0
         GO TO 990
         END IF
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error or warning
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('SPECIFY FPM(10) FOR',1X,A4)
      END
      SUBROUTINE FCN (M, N, X, RESULT, DERP, DERM, IFLAG, DATA, JBLANK,
     *   MX, MY, DARRAY)
C-----------------------------------------------------------------------
C   FCN calculates the model (or residual) using parameters passed to
C   it via LSTSQ or CUBITM.  When called from LSTSQ, the non-linear
C   least squares fit routine, with IFLAG = 1, one call to FCN results
C   in a return of M values of the residuals, RESULT(J) (the loop over
C   M points is done in FCN).  When IFLAG is greater than 1, one call
C   to FCN results in a return of N values of DER(K) for a single point
C   (the loop over M points is done in LSTSQ).  After the least squares
C   fit is completed a final call to FCN occurs from CUBITM (in this
C   case, IFLAG = -1).
C   Input :
C      M         I      Number of unblanked data points
C      N         I      Number of free parameters
C      X         D(N)   Value of each parameter
C      IFLAG     I      =  1 to compute residuals
C                       >  1 to compute derivatives
C                            for calls through LSTSQ
C                       = -1 to compute residuals or model depending
C                            on CPARM(1) for final call through CUBITM
C      DATA     R(M)    Input data
C      JBLANK   I(*)    # blanked points before pixel of interest
C      MX       I       Output X dimension
C      MY       I       Output Y dimension
C   Output:
C      DARRAY   D(*)    (MX,MY,3) arrays for smoothing
C      RESULT   D(M)    RESULT contains the residuals (model - data)
C                       for the purposes of the least squares fit.
C                       or the model on the final call if CPARM(1)=1
C      DERP     D(15)   Model value + increment in pt IFLAG-1
C      DERM(    D(15)   Model value - increment in pt IFLAG-1
C      IFLAG    I       = -100 => ERROR
C   Subprograms called:
C      AIPS sublibraries...MSGWRT
C      CUBIT subroutines....VELITY, DENSTY
C   *NOTE* Z is a line of sight distance -
C   not a distance perpendicular to the plane.
C-----------------------------------------------------------------------
      INCLUDE 'CUBSIZ.INC'
      INTEGER   M, N, MX, MY, JBLANK(*), IFLAG
      REAL      DATA(*)
      DOUBLE PRECISION X(N), RESULT(M), DERP(MAXPRM), DERM(MAXPRM),
     *   DARRAY(MX,MY,3)
C
      CHARACTER CODESV(4)*4
      INTEGER   J, JM, JBL, JTOT, NAXIS1, NAXIS2, NAXIS3, NICT, NICTP,
     *   KM, KBL, MTOT, IHOLD, ITRACK(5), IRET, I, II, JJ, ISWCH, IT,
     *   IROUND, MMM, L, K, KX, LX, IDER, KSWCH, INTLIN, IIJJ, LE, LG,
     *   KK
      REAL      SINI, COSI, COS2I, SINPSI, COSPSI, SINPA, COSPA,
     *   RRA, RDEC, R, RX, RY, RAD, VMAXLM, VTEMP, DZDRVL(2), ZMAXIT,
     *   ZLIM(2,2), RVEL, Z(3,2), DELTAZ(2), VELIM(3), DELTAX, A, B,
     *   ALPHA, XP, YP, RYMAX(2), RYMIN(2), SAVE1, SAVE2, FACT
      DOUBLE PRECISION DENS(2), PI, DELTA(MAXPRM), DELTP, DELTM, BMODEL,
     *   XIN
      DOUBLE PRECISION ROWIN(2048), ROWOUT(2048)
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'CUBDIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE KSWCH
      COMMON /LEAST/ IT
      COMMON /JLEAST/ JBL, NICT, NICTP, MTOT
      COMMON /DEL/ DELTA
      COMMON /JBLK/ IHOLD
      EQUIVALENCE  (CATOLD, OLDR, OLDH, OLDD)
      DATA KSWCH /0/
      DATA CODESV /'CV  ', 'SB  ', 'BR  ', 'RC  '/
      DATA PI /3.1415926536/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Lengths of output array
      NXOUT = CATBLK(KINAX+1)
      NYOUT = CATBLK(KINAX+2)
      NVOUT = CATBLK(KINAX)
      IF (IFLAG.LE.1) INTLIN = NVOUT
C                                       Skip this setup if it's already
C                                       been done once
      IF (KSWCH.EQ.0) THEN
C                                       Create an array to remember how;
C                                       many blanked pts came before the
C                                       pixel of interest. Must be done
C                                       in same order as data read in.
         JM = 0
         JBL = 0
         DO 3 JJ = 1,NYOUT
            DO 2 II = 1,NXOUT
               DO 1 KK = 1,NVOUT
                  JM = JM + 1
                  IF (DATA(JM).EQ.FBLANK) JBL = JBL + 1
                  JBLANK(JM) = JBL
 1                CONTINUE
 2             CONTINUE
 3          CONTINUE
C                                       Check
         IF (JM.NE.MTOT) THEN
            IRET = -1
            WRITE (MSGTXT,1005) JM
            END IF
C                                       Check
         IF (JBL+M.NE.JM) THEN
            IRET = -1
            WRITE (MSGTXT,1005) JBL+JM
            GO TO 990
            END IF
         KSWCH = 1
         END IF
      RAD = 180.0 / PI
      IDER = 0
C                                       Check for error condition
      IF (IFLAG.EQ.-100) GO TO 999
C                                       On last call, just apply the
C                                       fitted FPARMs in the COMMON
      IF (IFLAG.EQ.-1) GO TO 70
C                                  If this is a derivative computation
C                                      skip to the derivative section
      IF (IFLAG.LT.2) THEN
C                                      Check for out of bounds FPARMs at
C                                      each iteration
C                                      LX loops over parameters(N=#FREE)
         L  = 0
         LE = 0
         LG = 0
         DO 10 LX = 1,N
 9          L = L + 1
            IF (FIX(L).EQ.1) GO TO 9
            XIN = X(LX)
            IF (L.EQ.4) THEN
               IF (X(LX).GT.89.05) X(LX) = 89.05
               IF (X(LX).LT.0.001) X(LX) = 0.001
            ELSE IF ((L.EQ.7) .OR. (L.EQ.8)) THEN
               X(LX) = MAX (1.0D-3, X(LX))
            ELSE IF ((L.EQ.7) .OR. (L.EQ.10)) THEN
               X(LX) = MIN (10.0D0*DSKMAX, X(LX))
            ELSE IF ((L.EQ.10).OR. (L.EQ.12)) THEN
               X(LX) = MAX (1.0D-3, X(LX))
            ELSE IF ((L.EQ.14) .OR. (L.EQ.15)) THEN
               X(LX) = MIN (1.0D0, X(LX))
            ELSE IF (L.NE.3) THEN
               X(LX) = MAX (1.0D-5, X(LX))
               END IF
            IF (XIN.NE.X(LX)) THEN
               WRITE (MSGTXT,1030) IFLAG, L, X(LX)
               CALL MSGWRT (8)
               END IF
            IF (CPARM(4).EQ.4.0) THEN
               IF (L.EQ.14) LE = LX
               IF (L.EQ.15) LG = LX
               END IF
 10         CONTINUE
C                                       Ensure that fp14+fp15=1, (recall
C                                       that either f14,f15 are both
C                                       fixed or they are both free)
         IF ((LE.NE.0) .OR. (LG.NE.0)) THEN
C                                       Take the larger value and ensure
C                                       that it stays larger. If they're
C                                       equal, let the exp disk dominate
            IF (X(LE).GE.X(LG)) X(LE) = 1.0 - X(LG)
            IF (X(LE).LT.X(LG)) X(LG) = 1.0 - X(LE)
            END IF
C                                      Update FPARM.  FPARM is used for
C                                      calculations.  X is used to com-
C                                      municate with LMSTR, and for in-
C                                      termediate output.
C                                      I: number of parameters to be
C                                         fitted
C                                      L: total number of parameters
         L = 0
         DO 20 I = 1,N
 15         L = L + 1
            IF (FIX(L).EQ.1) GO TO 15
            FPARM(L) = X(I)
 20         CONTINUE
         GO TO 70
         END IF
C                                      Start derivative computation
C                                      Initialize DERP,DERM
      DO 30 LX = 1,N
         DERP(LX) = 0.0D0
         DERM(LX)  = 0.0D0
 30      CONTINUE
C                                      On the first call for derivatives
C                                      set the values of DELTA
C                                      There are N values of DELTA(LX)
      L  = 0
      DO 35 LX = 1,N
 34      L  = L  + 1
         IF (FIX(L).EQ.1) GO TO 34
C                                     Skip this section if you've
C                                     already hit it once
C                                     Adjusting Deltas (June/91)
C                                     Try to make each delta meaningful
C                                     Position-want total increment to
C                                     be a pixel
         IF (DELTA(LX).EQ.0.0) THEN
            IF ((L.EQ.1) .OR. (L.LE.2)) THEN
               DELTA(LX) = 0.5D0
C                                     Angles - 3deg in PA, 1deg in i
            ELSE IF (L.EQ.3) THEN
               DELTA(LX) = 3.0D0
            ELSE IF (L.EQ.4) THEN
               DELTA(LX) = 1.0D0
C                                     Vsys, Vmax-incr. less than
C                                     half a channel, or f=0.005
            ELSE IF (L.EQ.5) THEN
               DELTA(LX) = DELVEL / 3.0D0
            ELSE IF ((L.EQ.6) .AND. (VCODE.NE.CODESV(4))) THEN
               DELTA(LX) = DELVEL / 5.0D0
            ELSE IF ((L.EQ.6) .AND. (VCODE.EQ.CODESV(4))) THEN
               DELTA(LX) = 0.005
C                                     Rmax->avg pixel sep in arcsec
            ELSE IF (L.EQ.7) THEN
               DELTA(LX) = (ABS (CATR(KRCIC+1)) + ABS (CATR(KRCIC+2)))
     *            * 3600.0 / 1.5
C                                     m - 0.4 of value to lower lim.
            ELSE IF (L.EQ.8) THEN
               DELTA(LX) = MAX( 0.4 * FPARM(L), 0.075 )
C                                     no-10% of value
            ELSE IF (L.EQ.9) THEN
               DELTA(LX) = 0.1D0 * FPARM(L)
C                                     ro-1 pixel separation
            ELSE IF (L.EQ.10) THEN
               DELTA(LX) = (ABS (CATR(KRCIC+1)) + ABS (CATR(KRCIC+2)))
     *            * 3600.0 / 2.0
C                                     zo-less than half of avg
C                                     pixel separation
            ELSE IF ((L.EQ.11) .OR. (L.EQ.13)) THEN
               DELTA(LX) = (ABS (CATR(KRCIC+1)) + ABS (CATR(KRCIC+2)))
     *            * 3600.0 / 5.0
C                                     r1 - 1 pixel separation
            ELSE IF (L.EQ.12) THEN
               DELTA(LX) = (ABS (CATR(KRCIC+1)) + ABS (CATR(KRCIC+2)))
     *            * 3600.0 / 2.0
C                                July/00, extra increments for new
C                                parms 14 and 15
C                                make the increment 10% of value
            ELSE IF ((L.EQ.14) .OR. (L.EQ.15)) THEN
               DELTA(LX) = 0.1D0 * FPARM(L)
               END IF
            END IF
 35      CONTINUE
      J  = IFLAG - 1
C                                      KX is deriv # of X
      KX = 0
C                                      Increment counters once per
C                                      call for derivative calc
      JM  = J + JBLANK(IHOLD)
      IF (DATA(JM).EQ.FBLANK) THEN
         IRET = -1
         WRITE (MSGTXT,1050) JM, J
         GO TO 990
         END IF
C                                      Begin loop for derivatives
C*********************************************************************
 50   DELTP = 0.0D0
      DELTM = 0.0D0
C                                      KX is the deriv # corresponding
C                                      to the X's (ie. the FPARMs to be
C                                      varied)
      KX = KX + 1
C                                      Only one FPARM is incremented
C                                      at a time to calc derivatives.
C                                      Increment FPARM's by +
C                                      or - the corresponding DELTA.
C                                      IDER keeps track of # of passes
C                                      through the code (twice for one
C                                      derivative).
  52  IDER = IDER + 1
C                                      K is the derivative number cor-
C                                      responding to the FPARMs
      K = (IDER-1)/2 + 1
      IF (FIX(K).EQ.1) GO TO 52
C                                      Increment up first
      IF (MOD(IDER,2).EQ.1) THEN
         FPARM(K) = FPARM(K) + DELTA(KX)
         DELTP = DELTA(KX)
C                                      Check for out of bounds values
C                                      and adjust if necessary
         IF ((K.EQ.4) .AND. (FPARM(K).GT.89.05)) THEN
            DELTP = DELTA(KX)-(FPARM(K)-89.05)
            FPARM(K) = 89.05
C                                     Avoid round off errors
            IF ((DELTP.LT.0.0) .AND. (ABS(DELTP/FPARM(K)).LT.1.0E-4))
     *         DELTP = 0.0
         ELSE IF ((K.EQ.7) .AND. (FPARM(K).GT.10.0*DSKMAX)) THEN
            DELTP = DELTA(KX) - (FPARM(K) - 10.0*DSKMAX)
            FPARM(K) = 10.0*DSKMAX
C                                     Avoid round off errors
            IF ((DELTP.LT.0.0) .AND. (ABS(DELTP/FPARM(K)).LT.1.0E-4))
     *         DELTP = 0.0
         ELSE IF ((K.EQ.8) .AND. (FPARM(K).GT.15.5)) THEN
            DELTP = DELTA(KX) - (FPARM(K) - 15.5)
            FPARM(K) = 15.5
            IF ((DELTP.LT.0.0) .AND. (ABS(DELTP/FPARM(K)).LT.1.0E-4))
     *         DELTP = 0.0
            END IF
C                                     Now increment down
      ELSE
         FPARM(K) = FPARM(K) - DELTP - DELTA(KX)
         DELTM = DELTA(KX)
C                                     Check again -- out of bounds
C                                     no boundary on position angle
         IF ((K.EQ.4) .OR. (K.EQ.7) .OR. (K.EQ.8)) THEN
            IF (FPARM(K).LT.1E-4 ) THEN
               DELTM = FPARM(K) + DELTA(KX) - 1.E-4
               FPARM(K) = 1E-4
               IF (DELTM.LT.0.0 .AND. ABS(DELTM/FPARM(K)).LT.1.0E-4)
     *             DELTM = 0.0
               END IF
         ELSE IF (K.NE.3) THEN
            IF (FPARM(K).LE.1E-8) THEN
               DELTM = FPARM(K) + DELTA(KX) - 1.0E-8
               FPARM(K) = 1.0E-8
               IF ((DELTM.LT.0.0) .AND. (ABS(DELTM/FPARM(K)).LT.1.0E-4))
     *            DELTM = 0.0
               END IF
            END IF
         END IF
C                     NOTE; do not need boundaries on fparm14,15
C                     derivative calc should be okay. just need
C                     to ensure that the central values themselves
C                     have f14+f15=1
C                                    Check that deltas are okay
      IF ((DELTM.LT.0.0) .OR. (DELTP.LT.0.0)) THEN
         IRET = -1
         WRITE (MSGTXT,1070) K, DELTP, DELTM
         GO TO 990
         END IF
C                                       Calculate sin and cos of
C                                       the position angle
  70  SINPA = SIN (FPARM(3)/RAD)
      COSPA = COS (FPARM(3)/RAD)
C                                    Determine trig fcns of inclination
      COSI = COS (FPARM(4)/RAD)
      COS2I = COSI**2
      SINI = SIN (FPARM(4)/RAD)
C                                       Determine max line of sight dis-
C                                       tance based on inclination and
C                                       disk thickness (arcsec).
      ZMAXIT = ABS(DPARM(7)/COSI)
C                                       Calculate the resolution in the
C                                       major axis direction (arcsec).
      ALPHA  = FPARM(3) - CATR(KRBPA)
      A      = (CATR(KRBMJ) * 3600.0) / 2.0
      B      = (CATR(KRBMN) * 3600.0) / 2.0
      XP     = A * COS (ALPHA/RAD)
      YP     = B * SIN (ALPHA/RAD)
      DELTAX = SQRT (XP**2 + YP**2)
C                                       Don't initialize counters or
C                                       increment again when calculating
C                                       derivatives.
      IF (IFLAG.GE.2) GO TO 90
C                                       Only work with unblanked points
C                                       J counts unblanked points to M
C                                       JM counts total # pts
C                                       JBL counts blanked pts
C                                       JTOT keeps track of # pts for
C                                       which intensities are actually
C                                       calculated
         JM   = 0
         JBL  = 0
         JTOT = 0
         J = 0
C                                        Initialize counters to find
C                                        the number of intervals (for
C                                        DENSITY) > 10, 100, 500
         DO 81 MMM = 1,5
            ITRACK(MMM) = 0
 81         CONTINUE
C                                        Begin loop for calc of RESULT
C**********************************************************************
 80   CONTINUE
      J = J + 1
  82     JM = J + JBL
         IF (DATA(JM).EQ.FBLANK) THEN
            JBL = JBL + 1
            GO TO 82
            END IF
C                                       Find plane, column and row #
C                                       (in pixels) of the pt at JM.
C                                       Note integer arithmetic.
  90     NAXIS3 = ((JM - 1)/(NVOUT * NXOUT)) + 1
         NAXIS2 = (((JM - ((NAXIS3 - 1) * NXOUT * NVOUT))-1) / NVOUT)+1
         NAXIS1 = JM - ((NAXIS3 - 1) * NXOUT * NVOUT) - ((NAXIS2 - 1)*
     *     NVOUT)
C                                       Convert to 'distance' from
C                                       specified center position in
C                                       real units.
         NAXIS3 = NAXIS3 + IROUND(BLC(3)) - 1
         NAXIS2 = NAXIS2 + IROUND(BLC(2)) - 1
C                                        DEC and RA offset s in arcsec
         RDEC = REAL(NAXIS3) - FPARM(2)
         RDEC = RDEC * CATR(KRCIC+2) * 3600.0
         RRA  = REAL(NAXIS2) - FPARM(1)
         RRA  = RRA * CATR(KRCIC+1) * (-3600.0)
         R = SQRT((RRA**2) + (RDEC**2))
C                                        VEL offset in km/s for input
C                                        map in m/s and parameter VSYS
C                                        in km/s.
         RVEL = (REAL(NAXIS1) - CATR(KRCRP)) * CATR(KRCIC)
     *           + CATD(KDCRV)
         RVEL = (RVEL/1000.0) - FPARM(5)
C                                       If the point is at the center
C                                       use the minor axis calculation.
         IF ((ABS(RDEC).LE.1E-37) .AND. (ABS(RRA).LE.1E-37)) THEN
            RX = 0.0
            RY = 0.0
         ELSE
C                                       Rotate the X (positional) axis
C                                       so that the X axis (starting pt)
C                                       is the receding major axis of
C                                       the galaxy as specified through
C                                       the parameter, FPARM3 (position
C                                       angle).  Angles, PSI, are
C                                       CCW, like the unit circle from
C                                       the receding maj axis.
            COSPSI = (COSPA * RDEC/R) - (SINPA * RRA/R)
            SINPSI = -((COSPA * RRA/R) + (SINPA * RDEC/R))
C                                       Only deal with the quadrant
C                                       specified. (Round off errors
C                                       must be dealt with)
            IF ((COSPSI.LT.COSMIN-1.E-7) .OR. (COSPSI.GT.COSMAX+1.E-7)
     *        .OR. (SINPSI.LT.SINMIN-1.E-7) .OR.
     *         (SINPSI.GT.SINMAX+1.E-7)) GO TO 499
            RX = R * COSPSI
            RY = R * SINPSI
            END IF
C                                       RX can't be > DSKMAX
         IF (ABS(RX).GT.DSKMAX) GO TO 499
C                                       RY can't be > the projected disk
C                                       width + an amt. for the disk
C                                       thickness
         IF (ABS(RY).GT.(DPARM(7)*SINI + SQRT(DSKMAX**2-RX**2)*COSI))
     *      GO TO 499
C                                       Exclude centrally located points
C                                       if less than dskmin in plane of
C                                       galaxy
         IF ((ABS(RX).LT.DSKMIN) .AND. (ABS(RY/COSI).LT.DSKMIN))
     *      GO TO 499
C                                       Even though RX,RY are in right
C                                       range, a large Z value could
C                                       put the pt. outside of the disk.
C                                       So limits must be set on Z
C                                       depending on whether the line of
C                                       sight distance 'emerges out' of
C                                       the disk through the thickness
C                                       or along the length.
C                                       ZMAXIT: Z has emerged through
C                                       the disk thickness (height
C                                       important)
C                                       OTHER: Z has emerged through
C                                       the disk end (DSKMAX important)
         RYMAX(1) = SQRT (DSKMAX**2 - RX**2)
         RYMIN(1) = 0.0
         IF (ABS(RX).LT.DSKMIN) RYMIN(1) = SQRT (DSKMIN**2 - RX**2)
         RYMAX(2) = - RYMAX(1)
         RYMIN(2) = - RYMIN(1)
         DO 118 JJ = 1,2
            ZLIM(1,JJ) = (RYMAX(JJ) - ABS(RY/COSI)) / SINI
            ZLIM(2,JJ) = (RYMIN(JJ) - ABS(RY/COSI)) / SINI
            DO 117 II = 1,2
               FACT = -1.0
               IF (ZLIM(II,JJ).GE.0.0) FACT = 1.0
               ZLIM(II,JJ) = FACT * MIN(ZMAXIT, ABS(ZLIM(II,JJ)))
 117           CONTINUE
C                                      Make sure the max value has
C                                      II=1, min has II=2
            SAVE1 = MAX(ZLIM(1,JJ), ZLIM(2,JJ))
            SAVE2 = MIN(ZLIM(1,JJ), ZLIM(2,JJ))
            ZLIM(1,JJ) = SAVE1
            ZLIM(2,JJ) = SAVE2
 118        CONTINUE
C                                      Define velocity values for points
C                                     (1 & 3) on either side of point of
C                                      interest (2).
         VELIM(1) = RVEL + DELVEL/2.0
         VELIM(2) = RVEL
         VELIM(3) = RVEL - DELVEL/2.0
C                                      For the minor axis (ie. if pt is
C                                      within 1/2 a half-max beam width
C                                      from RX=0) use the case where all
C                                      the gas is within one channel
         IF (ABS(RX).LT.DELTAX/2.0) GO TO 400
C                                      Can't have +ve velocities
C                                      with -ve RX's and vice versa
C                                      Allow for channel straddling
C                                      Vsys later
         IF (VELIM(1).GE.0.0 .AND. VELIM(3).GE.0.0 .AND. RX.LT.0.0)
     *       GO TO 499
         IF (VELIM(1).LE.0.0 .AND. VELIM(3).LE.0.0 .AND. RX.GT.0.0)
     *       GO TO 499
C                                      Call VELITY to determine the
C                                      max limiting velocity at an RX.
C                                      ISWCH=1 => return VMAXLM
C                                      ISWCH=2 => return Z(II,JJ)
         ISWCH = 1
         CALL VELITY (ISWCH, RX, RY, VELIM, VMAXLM, Z, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'VELITY ERROR'
            GO TO 990
            END IF
C                                      'Solid body' case requires a
C                                       different route. V is inde-
C                                       pendent of Z.
         IF (ISWCH.EQ.10) THEN
C                                       This part for 'SB' has been
C                                       changed in aug/00 ver
            IF (RX.GE.0.0) THEN
               VTEMP = VMAXLM
            ELSE
               VTEMP = VMAXLM*(-1.0)
               END IF
            IF (((VELIM(1).LE.VTEMP) .AND. (VELIM(3).GT.VTEMP)) .OR.
     *         ((VELIM(3).LE.VTEMP) .AND. (VELIM(1).GT.VTEMP)))
     *         GO TO 410
         ELSE
C                                       Subscripts II denote the point
C                                       of interest (II=2) and points
C                                       (II=1,3) correspond to a half
C                                       channel width on either side of
C                                       it.  Velocities should be in
C                                       range 0 < RVEL <or= VMAXLM
            DO 125 II = 1,3
C              IF ((ABS(VELIM(II)).GT.0.1) .AND.(ABS(VELIM(II))
C    *            .LE.VMAXLM)) GO TO 130
C                      Changed the boundary in the aug/00 version
               IF ((ABS(VELIM(II)).GT.0.0) .AND.
     *            (ABS(VELIM(II)).LE.VMAXLM)) GO TO 130
 125           CONTINUE
            END IF
         GO TO 440
C                                       Set limits for cases near the
C                                       'edge' and allow for channel
C                                       which straddles Vsys.
C                                       Adjusted boundaries slightly in
C                                       aug/00 ver - Doesn't work
C                                       perfectly yet for RC
 130     DO 140 II = 1,3
            IF (RX.LE.0.0) THEN
               VELIM(II) = MAX (-VMAXLM, MIN (-1.0E-5, VELIM(II)))
               IF ((VELIM(II).GT.-1.0) .AND. (VCODE.EQ.CODESV(4)))
     *            VELIM(II) = -DELVEL/4.0
            ELSE
               VELIM(II) = MIN (VMAXLM, MAX (1.0E-5, VELIM(II)))
               IF ((VELIM(II).LT.1.0) .AND. (VCODE.EQ.CODESV(4)))
     *            VELIM(II) = DELVEL/4.0
               END IF
 140        CONTINUE
C                                       Call to calculate Z(II,JJ) =
C                                       distance along line of sight +ve
C                                       or -ve from centre of disk for a
C                                       given VELIM(II)
C                                         ISWCH=1 => VMAXLM returned
C                                         ISWCH=2 => Z(II,JJ) returned
C                                       Two values of Z are possible for
C                                       a given VELIM (JJ=1,2)
         ISWCH = 2
         CALL VELITY (ISWCH, RX, RY, VELIM, VMAXLM, Z, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'VELITY ERROR'
            GO TO 990
            END IF
C                                       If both points straddling the
C                                       point of interest (2) are
C                                       outside of the disk, then the
C                                       intensity will be zero.
         DO 190 JJ = 1,2
C                                       If all pts (1 to 3) are out of
C                                       range, zero the pt.
            IF ((ZLIM(1,JJ).NE.ZLIM(2,JJ)) .AND. (ISWCH.NE.-3) .AND.
     *         (ISWCH.NE.(-JJ))) THEN
               DO 160 II = 1,3
                  IF ((Z(II,JJ).LE.ZLIM(1,JJ)) .AND.
     *               (Z(II,JJ).GE.ZLIM(2,JJ))) GO TO 170
 160              CONTINUE
               IF ((Z(1,JJ).GT.ZLIM(1,JJ)) .AND.
     *            (Z(3,JJ).LT.ZLIM(2,JJ))) GO TO 170
               IF ((Z(3,JJ).GT.ZLIM(1,JJ)) .AND.
     *            (Z(1,JJ).LT.ZLIM(2,JJ))) GO TO 170
               END IF
            DO 165 II = 1,3
               Z(II,JJ) = 0.0
 165           CONTINUE
            DZDRVL(JJ) = 0.0
            GO TO 190
C                                      Calculate dz/drvel
C                                      Constrain the straddling points
C                                      to be within the disk and cal-
C                                      culate the radial distance at
C                                      the center of the plane.
 170        DO 180 II = 1,3
               IF (Z(II,JJ).GT.ZLIM(1,JJ)) Z(II,JJ) = ZLIM(1,JJ)
               IF (Z(II,JJ).LT.ZLIM(2,JJ)) Z(II,JJ) = ZLIM(2,JJ)
 180           CONTINUE
            DELTAZ(JJ) = ABS(Z(1,JJ) - Z(3,JJ))
C                                      Calculate the derivative dz/dv
            DZDRVL(JJ) = DELTAZ(JJ) / DELVEL
 190        CONTINUE
         IF ((DZDRVL(1) + DZDRVL(2)).EQ.0.0) GO TO 440
         GO TO 430
C-----------------------------------------------------------------------
C                                       CALCULATION FOR CASE WHERE
C                                       ALL GAS IS NEAR VSYS
C
C                                       All gas along minor axis should
C                                       be near Vsys.
 400     IF (ABS(RVEL).GT.DELVEL/2.0) GO TO 440
C                                      Calculation for all emission
C                                      in a single channel
 410     CONTINUE
         DO 420 JJ = 1,2
            Z(1,JJ) = ZLIM(1,JJ)
            Z(3,JJ) = ZLIM(2,JJ)
            DELTAZ(JJ) = ABS(Z(1,JJ) - Z(3,JJ))
            DZDRVL(JJ) = DELTAZ(JJ) / DELVEL
 420        CONTINUE
C-----------------------------------------------------------------------
C                                       Determine the density at the
C                                       point
 430     CONTINUE
            CALL DENSTY (RX, RY, Z, DENS, IRET, ITRACK)
            IF (IRET.NE.0) THEN
               MSGTXT = 'DENSTY ERROR'
               GO TO 990
               END IF
C                                      Determine the model intensity
C                                      and/or residual in Jy/beam
C                                   If there are two points within the
C                                   disk for a given rvel (ie. if both
C                                      +ve and -ve sqrts result in z's
C                                within the disk) then the intensities
C                                      are additive.
            BMODEL = CONST * (DENS(1)*DZDRVL(1) + DENS(2)*DZDRVL(2))
C                                       Smoothing done in LSTSQ when
C                                       calculating derivatives
            IF (IFLAG.GE.2) GO TO 600
C                                       Count up pts where the intensity
C                                       is actually calculated
            JTOT = JTOT + 1
            GO TO 450
C                                       Set bmodel to a distinguishing
C                                       value so that it can be included
C                                       in the residual calc. later.
 440        BMODEL = 1.0E35
C                                       If this is the last pass through
C                                       and we want model only, let
C                                       bmodel be zero.
C                                       Smoothing in LSTSQ for deriv-
C                                       atives
            IF (IFLAG.GE.2) GO TO 600
C                                       Store BMODEL in RESULT before
C                                       smoothing
 450     RESULT(J) = BMODEL
         GO TO 500
C
 499     IF (IFLAG.GE.2) GO TO 580
         RESULT(J) = 0.0

C                                      Put some checks in here
 500     IF (J.LT.M) GO TO 80
C*********************************************************************
C                                       Print warning if there were too
C                                       many density intervals
      IF (ITRACK(4).GE.200) THEN
         WRITE (MSGTXT,1080) ITRACK(4)
         CALL MSGWRT (8)
         END IF
C                                       After RESULT has been calculated
C                                       for all pts, do the smoothing
      CALL SMOOTH (M, RESULT, IFLAG, DATA, JBLANK, MX, MY, DARRAY, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SPATIAL SMOOTHING ERROR'
         GO TO 990
         END IF
C                                      Skip vel smoothing if desired
      IF (CPARM(6).GT.0.0) THEN
C                                      VEL smoothing, prepare to send
C                                      rows to vsmth
         JM = 0
 489     IF (J.LT.M) THEN
            DO 490 IIJJ = 1,NVOUT
               JM = JM + 1
               J = JM - JBLANK(JM)
               IF (DATA(JM).EQ.FBLANK) THEN
                  ROWIN(IIJJ) = FBLANK
C                                     there are additional blanks in
C                                     RESULT after SMOOTH
               ELSE
                  ROWIN(IIJJ) = RESULT(J)
                  END IF
 490           CONTINUE
            CALL VSMTH (NVOUT, ROWIN, ROWOUT, IFLAG, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'VELOCITY SMOOTHING ERROR'
               GO TO 990
               END IF
C                                    Set the pointer at the beginning
C                                    of the row again
            JM = JM - NVOUT
            DO 495 IIJJ = 1,NVOUT
               JM = JM + 1
               J = JM - JBLANK(JM)
               IF (DATA(JM).NE.FBLANK) RESULT(J) = ROWOUT(IIJJ)
 495           CONTINUE
            GO TO 489
            END IF
         END IF
C
C                                       If it's the last pass through
C                                       and we want the model only,
C                                       we're finished now
      IF ((CPARM(1).EQ.1) .AND. (IFLAG.EQ.-1)) GO TO 999
C
C                                       Compute residuals using smoothed
C                                       data
C
      KM = 0
      KBL = 0
      KK = 0
      DO 507 KK = 1,M
 504     KM = KK + KBL
         IF (DATA(KM).EQ.FBLANK) THEN
            KBL = KBL+1
            GO TO 504
            END IF
C                                       Compute residuals (MODEL-DATA)
C                                       Any newly blanked value of
C                                       RESULT from SMOOTH should
C                                       be set to zero to send back
C                                       to the least squares routine
         IF (RESULT(KK).EQ.FBLANK) THEN
C                                       Don't want to pass blanks back
C                                       to the least squares routine
            IF (IFLAG.NE.-1) RESULT(KK) = 0.0
         ELSE IF (RESULT(KK).NE.0.0) THEN
            IF ((RESULT(KK).GT.1.0E34) .AND.
     *         (RESULT(KK).LT.1.0E36)) RESULT(KK) = 0.0
C                                       Calculate residuals
            RESULT(KK) = RESULT(KK) - DATA(KM)
C
C                                       If there's a flux cutoff and
C                                       it's not the last pass through
C                                       then set the residual to zero
C                                       if it's less than the cutoff
            IF ((ABS(DATA(KM)).LT.XCUT) .AND. (IFLAG.NE.-1))
     *         RESULT(KK) = 0.0
            END IF
 507     CONTINUE
C                                       If it's the last pass through
C                                       we're finished
      IF (IFLAG.NE.-1) THEN
C                                       After RESULT loop, set JBL to
C                                       zero for beginning of der calc
         JBL = 0
C                                       Increment the iteration counter
         IT = IT + 1
C                                       Print out solutions after each
C                                       iteration
         WRITE (MSGTXT,2000) IT, JTOT
         CALL MSGWRT (5)
         DO 550 L = 1,N
            WRITE (MSGTXT,2010) FPNUM(L), PSTRNG(L), X(L)
            CALL MSGWRT (5)
 550        CONTINUE
         END IF
      GO TO 999
C                                       Calculate derivatives
 580  BMODEL = 0.0
 600  CONTINUE
C                                       Make sure there are no huge
C                                       values when calculating ders
      IF ((BMODEL.GT.1.0E34) .AND. (BMODEL.LT.1.0E36)) BMODEL = 0.0
      IF (K.NE.(IDER-2)/2 +1) THEN
         DERP(KX)= BMODEL
C                                       Loop back for a second pass
C                                       through (same derivative #)
         GO TO 52
C      ************
      ELSE
         DERM(KX) = BMODEL
C                                       Now divide by the sum of the
C                                       increments
         DERP(KX) = DERP(KX)/(DELTP+DELTM)
         DERM(KX) = DERM(KX)/(DELTP+DELTM)
C                                       Put FPARM back to its ori-
C                                       ginal value before the incre-
C                                       ments
         FPARM(K) = FPARM(K) + DELTM
         IF (KX.LT.N) GO TO 50
         END IF
C**********************************************************************
C                                       End derivative loop
C
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IFLAG = -100
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('# OF PTS=',I7,1X,'.NEQ. ORIGINAL')
 1030 FORMAT ('IFLAG=',I6,'  *WARNING*CHANGING FPARM(',I2,') TO',E12.4)
 1050 FORMAT ('DATA(',I7,') IS BLANKED WHEN J=',I7)
 1070 FORMAT ('DER',I4,'HAS INCREMENTS',E14.3,2X,E14.3)
 1080 FORMAT ('*WARNING*',I6,1X,'PTS REQUIRED >500 DENSTY INTERVALS')
 2000 FORMAT ('*** ',I3,1X,'iterations',I8,
     *   ' non-zero points in model ***')
 2010 FORMAT (I2,2X,A,1X,F12.5)
      END
      SUBROUTINE VELITY (ISWCH, RX, RY, VELIM, VMAXLM, Z, IRET)
C-----------------------------------------------------------------------
C   VELITY calculates the maximum velocity permissible for a given
C   X axis distance (RX) (ie. the value from the assumed rotation curve
C   when RY=0), VMAXLM, when ISWCH = 1.This subroutine also calculates
C   the LINE OF SIGHT distance, Z(II,JJ), from Z=0 at the center of the
C   disk for a given VELIM(II), RX and RY, when ISWCH=2.  Note that
C   two values of Z may exist for any VELIM if the disk is inclined
C   enough or thick enough.
C   Inputs:
C      ISWCH     I        1 => return VMAXLM
C                         2 => return Z(II,JJ)
C                         anything else => terminate
C      RX        R        Distance along major axis (arcsec) to
C                         pt of interest
C      RY        R        Distance along minor axis
C      VELIM     R(3)     Radial velocity of pt (2) and points (1) and
C                         (3) half a channel width on either side of
C                         it.  Velocities are relative to Vsys.
C   Outputs:
C      VMAXLM    R        Max limiting velocity from the rotation curve
C                         for a given RX.  (corresponds to RY=0, Z=0)
C      Z         R(3,2)   Line of sight distance from disk center
C                         corresponding to VELIM(II) using the +ve
C                         (JJ=1) and -ve (JJ=2) sqrts.
C      ISWCH     I        ISWCH = 10 for SB rotation curve.
C                         If out of bounds (for RC) return ISWCH as
C                         negative.
C       IRET     I        Error code - 0 = okay, anything else = DIE
C   Subprograms called:
C      AIPS sublibraries......MSGWRT
C-----------------------------------------------------------------------
      INTEGER   ISWCH, IRET
      REAL      RX, RY, VELIM(3), VMAXLM, Z(3,2)
C
      CHARACTER CODESV(4)*4
      INTEGER   JBL, NICT,NICTP, MTOT, II, JJ, ICT, I
      REAL      RAD, SINI, COSI, PI, RR, VRSAVE, VR, FRAC, RRSAVE, DIFF
      DOUBLE PRECISION FSQRT(3,2)
      INCLUDE 'CUBIT.INC'
      INCLUDE 'CUBPRM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /JLEAST/ JBL, NICT, NICTP,MTOT
      DATA PI /3.1415926536/
      DATA CODESV /'CV  ','SB  ','BR  ','RC  '/
C-----------------------------------------------------------------------
      RAD = 180.0/PI
      SINI = SIN(FPARM(4)/RAD)
      COSI = COS(FPARM(4)/RAD)
C                                        ISWCH = 1
C                                        Calculate VMAXLM
      IF (ISWCH.EQ.1) THEN
C                                        Constant velocity case
         IF (VCODE.EQ.CODESV(1)) THEN
            VMAXLM = ABS(FPARM(6))
C                                        Solid body case
         ELSE IF (VCODE.EQ.CODESV(2)) THEN
C                                        Change the switch to re-
C                                        route the code for SB case.
            ISWCH = 10
            VMAXLM = ABS (FPARM(6) * RX / FPARM(7))
C                                        Brandt curve case
         ELSE IF (VCODE.EQ.CODESV(3)) THEN
            IF (FPARM(8).LT.1.0E-30) FPARM(8) = 0.0
            IF (FPARM(8).GT.15) THEN
               FPARM(8) = 15
               WRITE(MSGTXT,1045) FPARM(8)
               CALL MSGWRT (8)
               END IF
            VMAXLM = (ABS(RX) / FPARM(7)) ** FPARM(8)
            VMAXLM = 1.0/3.0 + (2.0/3.0) * VMAXLM
            VMAXLM = VMAXLM ** (3.0 / (2.0 * FPARM(8)))
            VMAXLM = ABS(FPARM(6) * RX) / (FPARM(7) * VMAXLM)
C                                        User specified rotation curve
C                                        VMAXLM cannot be rigorous
C                                        at this point because R is
C                                        unknown
         ELSE
            VMAXLM = 0.0
            DO 10 I = 1,30
               VMAXLM = MAX (VMAXLM, ABS(VPARM(I)))
 10            CONTINUE
            VMAXLM = VMAXLM * FPARM(6)
            END IF
         VMAXLM = VMAXLM * SINI
C                                        ISWCH = 2
C                                        Calculate Z(II,JJ)
      ELSE IF (ISWCH.EQ.2) THEN
         DO 200 JJ = 1,2
            DO 195 II = 1,3
C                                        Constant velocity case
               IF (VCODE.EQ.CODESV(1)) THEN
                  IF (JJ.EQ.1) THEN
                     FSQRT(II,JJ) = SQRT ((FPARM(6) * SINI / VELIM(II))
     *                  ** 2 - 1.0)
                  ELSE
                     FSQRT(II,2) = -1.0 * FSQRT(II,1)
                     END IF
                  Z(II,JJ) = (ABS(RX)*FSQRT(II,JJ)-ABS(RY/COSI))/SINI
                  GO TO 195
C                                        Solid body case
C                                        V is independent of Z so
C                                        Z can't be calculated.
               ELSE IF (VCODE.EQ.CODESV(2)) THEN
                  MSGTXT = 'VELITY: SB CASE SHOULD NOT GET HERE'
                  GO TO 990
C                                        Brandt curve case
               ELSE IF (VCODE.EQ.CODESV(3)) THEN
C                                        A small round off error
C                                        necessitates the next step
                  IF (ABS(ABS(VELIM(II)/VMAXLM)-1.0).LE.1.0E-3) THEN
                     FSQRT(II,JJ) = 0.0
                  ELSE IF (JJ.NE.1) THEN
                     FSQRT(II,2) = -1.0 * FSQRT(II,1)
                  ELSE
                     FSQRT(II,JJ) = ((ABS(FPARM(6) * SINI) / VELIM(II))
     *                  * (RX / FPARM(7))) ** (2 * FPARM(8) / 3.0)
                     FSQRT(II,JJ) = (ABS(3.0/2.0 * FSQRT(II,JJ) - 0.5))
     *                  ** (2.0 / FPARM(8))
                     IF ((FPARM(7)**2*FSQRT(II,JJ)-(RX**2)).GE.0.0) THEN
                        FSQRT(II,JJ) = SQRT ((FPARM(7) ** 2 / (RX **2))
     *                     * FSQRT(II,JJ) - 1.0)
                     ELSE
                        FSQRT(II,JJ) = 0.0
                        END IF
                     END IF
C                                       Calculate Z
                  Z(II,JJ) = (ABS(RX)*FSQRT(II,JJ)-ABS(RY/COSI))/SINI
                  GO TO 195
C                                       User specified rotation curve
               ELSE
                  ICT = 0
                  Z(II,JJ) = 0.0
                  VRSAVE = 0.0
                  RRSAVE = 0.0
                  VR = 0.0
C                                       Must iterate to find correct R
C                                       Start by assuming Z=0
                  RR = SQRT (RX**2 + (RY/COSI)**2)
 130              ICT = ICT + 1
                  DIFF = ABS (RR-RRSAVE)
C                                       Assume no solution if > 200
C                                       iterations required
                  IF (ICT.GT.200) GO TO 142
 142              NICT = NICT + 1
                  GO TO 144
C                                       Stop iterations if change in
C                                       RR is less than half the average
C                                       beam resolution AND change in
C                                       VR is less than half the
C                                       channel separation
                  IF ((DIFF.LT.(CATR(KRBMJ)+CATR(KRBMN))*3600.0/4.0)
     *              .AND. (ICT.GT.2) .AND.
     *              (ABS(VR-VRSAVE)*FPARM(6).LT.ABS(CATR(KRCIC)/2000.)))
     *              GO TO 143
C                                       Stop iterations if iteration
C                                       no. is > 50 and vel change is
C                                       better than vel res. and RR
C                                       change is better than spatial
C                                       resolution
                  IF ((DIFF.LT.(CATR(KRBMJ)+CATR(KRBMN))*3600.0/2.0)
     *               .AND. (ABS(VR-VRSAVE)*FPARM(6).LT.ABS(CATR(KRCIC)
     *               /1000.0)) .AND. (ICT.GT.50)) GO TO 143
                  RRSAVE = RR
                  VRSAVE = VR
C                                       Find VR from RR
                  VR = (RR/RPARM(1)) * VPARM(1)
                  IF (RR.GE.RPARM(1)) THEN
                     DO 135 I = 1,30
                        IF ((I.EQ.30) .OR. (RPARM(I+1).EQ.0.0)) THEN
                           VR = VPARM(I)
                           GO TO 140
                        ELSE IF (RR.GE.RPARM(I) .AND. RR.LT.RPARM(I+1))
     *                        THEN
                           FRAC = (RR-RPARM(I))/(RPARM(I+1)-RPARM(I))
                           VR = (VPARM(I+1)-VPARM(I))*FRAC + VPARM(I)
                           IF (ABS(VR-VPARM(I)).GT.
     *                        ABS(VPARM(I+1)-VPARM(I))) THEN
                              MSGTXT =
     *                           'ROTATION CURVE INTERPOLATION ERROR'
                              GO TO 990
                              END IF
                           GO TO 140
                           END IF
 135                    CONTINUE
                     END IF
 140                 VR = VR * FPARM(6)
C                                       Compute RR from VR
                  RR = VR * SINI * ABS (RX/VELIM(II))
C                                       Continue iterating
                  GO TO 130
C                                       a solution
 143              NICTP = NICTP + 1
C                                       Let final RR be average of
C                                       current RR and previous RR
 144              RR = (RR+RRSAVE) / 2.0
C                                       Save RR in FSQRT
                  FSQRT(II,JJ) = RR
C
                  END IF
 195           CONTINUE
 200        CONTINUE
C                                       Need to check vel limits
C                                       for RC case
         IF (VCODE.EQ.CODESV(4)) THEN
            ISWCH = 0
            DO 400 JJ = 1,2
               DO 300 II = 1,3
C                                       RR was stored in FSQRT.
C                                       If any RR of the 3 is
C                                       within range, then
C                                       continue
                  IF (FSQRT(II,JJ).GE.ABS(RX) .AND.
     *               FSQRT(II,JJ).LE.DSKMAX) GO TO 330
 300              CONTINUE
C                                        If ALL RR of the 3 are
C                                        smaller than RX, then
C                                        pt is out of range.
C                                        Set iswch so that pt in
C                                        FCN goes to zero.
               ISWCH = ISWCH - JJ
               GO TO 400
C                                        Set limits for cases near
C                                        the edge as before in FCN
C                                        only work with RR,RX rather
C                                        than VELIM VMAXLM
 330           DO 350 II = 1,3
                  IF (FSQRT(II,JJ).LT.ABS(RX)) FSQRT(II,JJ) = ABS(RX)
                  IF (FSQRT(II,JJ).GT.DSKMAX) FSQRT(II,JJ) = DSKMAX
 350              CONTINUE
C                                        Now calculate Z
               DO 370 II = 1,3
                  FSQRT(II,JJ) = FSQRT(II,JJ)**2 - RX**2
C                                        A small roundoff error
C                                        necessitates the next step
                  IF (FSQRT(II,JJ).LT.0.0) FSQRT(II,JJ) = 0.0
                  FSQRT(II,JJ) = SQRT (FSQRT(II,JJ))
                  IF (JJ.EQ.2) FSQRT(II,JJ) = -FSQRT(II,JJ)
                  Z(II,JJ) = (FSQRT(II,JJ)-ABS(RY/COSI))/SINI
 370              CONTINUE
 400           CONTINUE
            END IF
      ELSE
         MSGTXT = 'ILLEGAL VALUE OF ISWCH'
         GO TO 990
         END IF
      GO TO 999
C                                        Error
 990  IRET = -1
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1045 FORMAT ('**WARNING** CHANGING FPARM(8) TO ',F10.4)
      END
      SUBROUTINE DENSTY (RX, RY, Z, DENS, IRET, ITRACK)
C-----------------------------------------------------------------------
C   DENSTY computes the density at the radial distance, RZ, and height
C   above or below the plane, ZCOSI.  Options include constant density,
C   exponential density (with scale length), gaussian density (with
C   scale length) and a ring distribution which can either be an
C   exponential on both sides or gaussian on both sides.
C   The density is computed by integrating the fcn over
C   the line of sight distance, Z, where the 'fcn' refers to the radial
C   and perpendicular fcns multiplied together.  eg.  EXGS case:
C   take the integral from z1 to z2 of exp(-RZ/FPARM(10))*exp(-ZCOSI**
C   2/FPARM(11)**2) and then divide by z2-z1.  The integration is
C   done numerically -- z is integrated in steps such that no change
C   in the fcn is greater than 10%. ie. z can stretch or shrink to
C   ensure that the change in fcn is not more than 10%
C   Inputs:
C      RX       R        Distance along major axis (arcsec) to point
C                        of interest.
C      RY       R        Distance along minor axis.
C      Z        R(3,2)   Distance along the line of sight from the
C                        center of the galaxy's plane.  May be +ve
C                        or -ve.
C   Outputs:
C      DENS     D(2)     Density corresponding to the above positions
C                        depending on the density dist'ns chosen in
C                        the input parameters.
C      IRET     I        Error code - 0 = okay, anything else = die
C      ITRACK   I(5)     Keeps track of how many points required
C                        more than 10, 100 or 500 intervals in z.
C   Subprograms called:
C      AIPS sublibraries......MSGWRT
C-----------------------------------------------------------------------
      REAL      RX, RY, Z(3,2)
      INTEGER   IRET, ITRACK(5)
      DOUBLE PRECISION   DENS(2)
C
      CHARACTER CODESD(4)*4
      INTEGER   JJ, ICOUNT, ISIDE, ICSAVE, LAST, ICHECK, IRNEW, IROLD
      REAL      RZ, SINI, COSI, PI, RAD
      DOUBLE PRECISION ZDENS, RDENS, SUMT, RX2, ARYC, ZLO, ZHI, ZINC,
     *   ZSIDE, ZINCT, FUNCO, FUNCN, FUNC, ZNEW, AREA, RSCALE, EDENS,
     *   GDENS
      INCLUDE 'CUBIT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PI /3.1415926536/
      DATA CODESD /'CD  ','EX  ','GS  ','RG  '/
C-----------------------------------------------------------------------
C
      RAD  = 180.0 / PI
      SINI = SIN (FPARM(4)/RAD)
      COSI = COS (FPARM(4)/RAD)
      RX2 = RX**2
      ARYC = ABS (RY/COSI)
C                                      Start loop over JJ
      DO 300 JJ = 1,2
C                                      Initialize some counters
         ICOUNT = 0
         ISIDE = 0
         SUMT = 0.0
         IRNEW = 0
         IROLD = 0
         DENS(JJ) = 0.0
C                                       Density is zero if point is
C                                       degenerate
         IF (Z(1,JJ).EQ.Z(3,JJ)) GO TO 300
C                                       Take a different route if
C                                       the Z limits are of different
C                                       sign ie. the Z interval cross
C                                       es the center of the plane
         IF (Z(1,JJ)*Z(3,JJ).LT.0.0) THEN
            ISIDE = 1
            ZLO = MIN(Z(1,JJ),0.0)
            ZHI = MAX(Z(1,JJ),0.0)
         ELSE
            ZLO = MIN(Z(1,JJ),Z(3,JJ))
            ZHI = MAX(Z(1,JJ),Z(3,JJ))
            END IF
C                                       Initial Z increment and
C                                       initialize other values
 5       ZINC = ABS(ZHI-ZLO)
         IF (ISIDE.EQ.1) ZSIDE = ZINC
         ZINCT = ZINC
         FUNCO = 0.0
         FUNCN = 0.0
         ZNEW = ZLO
         ICOUNT = 0
         LAST = 0
C                                       This starts the looping
C                                       Save the old Fcn to compare
 10      FUNCO = FUNCN
         IROLD = IRNEW
         ICHECK = 0
C                                       Compute radial distance
 20      RZ = SQRT(RX2+(ARYC+ZNEW*SINI)**2)
C                                       Compute radial fcn
         RDENS = 0.0
         IF (DCODE.EQ.CODESD(1)) THEN
             RDENS = 1.0
         ELSE IF (DCODE.EQ.CODESD(2)) THEN
             IF (RZ.LE.34.0*FPARM(10)) RDENS = EXP (-RZ/FPARM(10))
         ELSE IF (DCODE.EQ.CODESD(3)) THEN
            IF (RZ**2.LE.34.0*(FPARM(10)**2)) RDENS =
     *         EXP (-(RZ**2)/(FPARM(10))**2)
         ELSE IF (DCODE.EQ.CODESD(4)) THEN
            IF (RZ.LT.CPARM(5)) THEN
               RSCALE = FPARM(12)
               IRNEW = -1
            ELSE
               RSCALE = FPARM(10)
               IRNEW = 1
               END IF
C                                       CPARM(7) distinguishes
C                                       between a gaussian or
C                                       an exponential ring
C                                       CPARM(7)=2==> EXP
C                                       CPARM(7)=3==> GAUSS
C                                       If more than 34 scale
C                                       lengths, set to zero
C                                       Gaussian ring
            IF (CPARM(7).GT.2.5) THEN
               IF ((RZ-CPARM(5))**2.LE.(34.0*(RSCALE**2))) RDENS =
     *            EXP(-((RZ-CPARM(5))**2)/(RSCALE**2))
C                                       Exponential ring
            ELSE
               IF (ABS(RZ-CPARM(5)).LE.34.0*ABS(RSCALE)) RDENS =
     *            EXP(-(ABS(RZ-CPARM(5)))/ABS(RSCALE))
               END IF
         ELSE
            MSGTXT = 'DCODE ''' // DCODE // ''' NOT PROGRAMMED YET'
            GO TO 995
            END IF
C                                      Compute perp. fcn
 50      IF (CPARM(4).EQ.1.0) THEN
            ZDENS = 1.0
         ELSE
            EDENS = 0.0
            GDENS = 0.0
            IF (ABS(ZNEW*COSI).LE.34.0*FPARM(11)) EDENS = FPARM(14) *
     *         EXP (-ABS (ZNEW*COSI) / FPARM(11))
            IF ((ZNEW*COSI)**2.LE.34.0*(FPARM(11)**2)) GDENS = FPARM(15)
     *         * EXP (-((ZNEW*COSI)**2) / (FPARM(13))**2)
            IF (CPARM(4).EQ.2) ZDENS = EDENS
            IF (CPARM(4).EQ.3) ZDENS = GDENS
            IF (CPARM(4).EQ.4) ZDENS = EDENS+GDENS
            END IF
C                                      Compute the fcn
         FUNCN = RDENS * ZDENS
C                                       If it's the first time
C                                       through, fcn must be
C                                       calculated again
         IF (ICOUNT.GT.0) THEN
C                                     Test acceptance criteria
C                                     if previous and new values are
C                                     on same 'side' of fcn OR if
C                                     they are on opposide sides but
C                                     close to R0
            IF ((DCODE.EQ.CODESD(4)) .AND. (IRNEW.NE.IROLD)) THEN
               IF ((ABS(RZ-CPARM(5)).LT.RSCALE/1000.0) .OR.
     *            (ZINC.LT.(ABS(ZHI-ZLO)/200.0))) THEN
                  RDENS = 1.0
                  IRNEW = -1.0 * IRNEW
                  GO TO 50
                  END IF
C                                      If new and old funcs are zero,
C                                      accept them and carry on
            ELSE
               IF ((FUNCN.EQ.0.0) .AND. (FUNCO.EQ.0.0)) GO TO 100
C                                      If new and old funcs are both
C                                      non-zero, use acceptance crit.
               IF ((FUNCN.EQ.0.0) .OR. (FUNCO.EQ.0.0)) THEN
C                                      If one is zero and one isn't,
C                                      accept them only if the last
C                                      Zinc is less than 1/200 of the
C                                      total range.
                  IF (ZINC.LE.ABS((ZHI-ZLO)/200.0)) GO TO 100
C                                      Criteria for
C                                      accepting the fcn value
C                                      a) if new fcn differs from
C                                         old fcn by less than 5%
               ELSE
                  IF ((2.0*ABS(FUNCN-FUNCO)/(FUNCN+FUNCO).LE.0.05) .OR.
C                                      Don't allow ZINC to go below
C                                      1/200 of ZHI-ZLO
     *               (ZINC.LT.(ABS(ZHI-ZLO)/200.0))) GO TO 100
                  END IF
               END IF
C                                      If fcn has changed too much
C                                      decrement zinc and try again
            ZINC = ZINC/2.0
            ZNEW = ZNEW-ZINC
            ICHECK = ICHECK + 1
            IF (ICHECK.GT.3000) GO TO 989
            GO TO 20

C                                      It's only the last point
C                                      if the above condition is met
C                                      Compute area and sum with
C                                      previous area
 100        IF (ZNEW.EQ.ZHI) LAST = 1
            AREA = ((FUNCN+FUNCO)/2.0)*ZINC
            SUMT = SUMT+AREA
            IF (DCODE.EQ.CODESD(4) .AND. RDENS.EQ.1.0) IRNEW = -1.0
     *         *IRNEW
            END IF
         ICOUNT = ICOUNT + 1

         IF ((ICOUNT.EQ.1) .OR. (LAST.NE.1)) THEN
C                                      If successful, increment zinc
C                                      and try again
            IF (ICOUNT.NE.1) ZINC = ZINC * 2.0
            ZNEW = ZNEW + ZINC
            IF (ABS((ZNEW-ZHI)/(ZHI-ZLO)).LT.1.0E-5) THEN
               ZNEW = ZHI
C                                      If it's out of bounds modify
C                                      ZINC and ZNEW
            ELSE IF (ZNEW.GT.ZHI) THEN
               ZINC = ZINC-(ZNEW-ZHI)
               ZNEW = ZHI
               END IF
           GO TO 10
           END IF
C                                      If the integration is done in
C                                      2 sections, keep track of sums
         ISIDE = ISIDE+2
         IF (ISIDE.EQ.3) THEN
            ICSAVE = ICOUNT
            ZLO = MIN(Z(3,JJ),0.0)
            ZHI = MAX(Z(3,JJ),0.0)
            GO TO 5
            END IF
         IF (ISIDE.NE.2) THEN
            ZINCT = ZSIDE+ZINCT
            ICOUNT = ICOUNT+ICSAVE
            END IF
C                                      Compute total integral divided
C                                      by total z distance+final value
         FUNC = SUMT/ZINCT
C                                      Avoid underflows
         IF ((FUNC.GT.0.0) .AND.
     *      ((LOG10(FPARM(9))+LOG10(FUNC)).GE.-15.0))
     *      DENS(JJ) = FPARM(9)*FUNC
 300     CONTINUE
C                                      Keep track of counter values
      IF (ICOUNT.GT.10) ITRACK(1) = ITRACK(1) + 1
      IF (ICOUNT.GT.50) ITRACK(2) = ITRACK(2) + 1
      IF (ICOUNT.GT.100) ITRACK(3) = ITRACK(3) + 1
      IF (ICOUNT.GE.500) ITRACK(4) = ITRACK(4) + 1
      IF (ICOUNT.GE.1000) THEN
         MSGTXT = '>1000 LOS INTERVALS REQUIRED TO COMPUTE DENS'
         GO TO 995
         END IF
      GO TO 999
C
 989  WRITE (MSGTXT,1030) FUNCN, FUNCO, ZINC, ZNEW
      CALL MSGWRT (8)
      WRITE (MSGTXT,1035) Z(1,JJ), Z(3,JJ), ZLO, ZHI
      CALL MSGWRT (8)
      WRITE (MSGTXT,1040) ZDENS, RDENS, RZ, FPARM(11)
C                                      ERROR
 995  CALL MSGWRT (8)
      IRET = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('FN=',E11.3,'FO=',E11.3,'ZI=',E11.3,'ZN=',E11.3)
 1035 FORMAT ('Z(1)=',E11.3,'Z(3)=',E11.3,'ZL=',E11.3,'ZH=',E11.3)
 1040 FORMAT ('ZD=',E11.3,'RD=',E11.3,'RZ=',E11.3,'F11=',E11.3)
      END
      SUBROUTINE SMOOTH (M, RESULT, IFLAG, DATA, JBLANK, MX, MY,
     *   DARRAY, IRET)
C-----------------------------------------------------------------------
C   SMOOTH takes all the modelled values calculated at each pixel and
C   smooths them spatially with the beam specified in the (output =
C   input) header.  The *input* pixel diameter is actually assumed to
C   be an input gaussian beam instead of a square pixel in order to
C   compute the convolving size.  This doesn't matter since the kernel
C   is properly normalized.  The factors are okay and no approximation
C   has been made.
C   Inputs:
C      M        I      Number of unblanked input pts
C      RESULT   D(M)   Array containing modelled values for each pixel
C      IFLAG    I      Flag indicated whether this is the 1st or last
C                      pass through
C      DATA     R(M)   Input data
C      JBLANK   I(*)   # blanked points before pixel of interest
C      MX       I      Output X dimension
C      MY       I      Output Y dimension
C   Outputs:
C      RESULT   D(M)   Array containing the smoothed modelled values
C                      for each pixel
C      DARRAY   D(*)   (MX,MY,3) arrays for smoothing
C      IRET     I      Error code - 0 = okay
C                                           - anything else = terminate
C   Subprograms called:
C        CUBIT subroutine.......GDECON, GASTPX, KERNEL
C-----------------------------------------------------------------------
      INTEGER   M, IFLAG, MX, MY, JBLANK(*), IRET
      REAL      DATA(*)
      DOUBLE PRECISION RESULT(M), DARRAY(MX,MY,3)
C
      INCLUDE 'CUBSIZ.INC'
      INTEGER   NX, NY, J, JMC, IHOLD, IT, I0, I1, I2, I3, J0, J1, J2,
     *   J3, NX2, NY2, II, JJ, KK
      REAL      DEGRA, AMAJ, AMIN, APA, DMAJ, DMIN, DPA, DX, DY, ARB, X,
     *   CMAJ, CMIN, CPA, BMAJ, BMIN, BPA, XK(25,25)
      INCLUDE   'CUBIT.INC'
      INCLUDE   'CUBDIM.INC'
      INCLUDE   'INCS:DHDR.INC'
      INCLUDE   'INCS:DMSG.INC'
      INCLUDE   'INCS:DCAT.INC'
      INCLUDE   'INCS:DDCH.INC'
      COMMON /LEAST/ IT
      COMMON /JBLK/  IHOLD
      COMMON /SMTH/  NY, NX, NY2, NX2, I1, I2, J1, J2, XK
      DATA DEGRA /57.2957795/
C-----------------------------------------------------------------------
C                                       Initialize
      IF (IT.LE.0) THEN
C                                       Desired beamsize:
C                                       Convert from header values in
C                                       degrees to arcsec and radians +
C                                       pixel increment from degrees to
C                                       radians
         AMAJ = CATR(KRBMJ) * 3600.0
         AMIN = CATR(KRBMN) * 3600.0
         APA  = CATR(KRBPA) / DEGRA
         DX   = -CATR(KRCIC+1)/DEGRA
         DY   = CATR(KRCIC+2)/DEGRA
C                                       Check ra axis sign
         IF (CATR(KRCIC+1).GE.0.0) THEN
            MSGTXT = '**WARNING** RA AXIS HAS WRONG SIGN'
            CALL MSGWRT (8)
            END IF
C                                       "BEAMSIZE"  = pixel size
         BMAJ = ABS (CATR(KRCIC+1)*3600.0)
         BMIN = ABS (CATR(KRCIC+2)*3600.0)
         BPA  = 3.1415926536
         IF (BMAJ.LT.BMIN) THEN
            X = BMAJ
            BMAJ = BMIN
            BMIN = X
            BPA = 0.0
            END IF
C                                       Deconvolve present beam from
C                                       desired beam to get convolution
C                                       size
         CALL GDECON (AMAJ, AMIN, APA, BMAJ, BMIN, BPA, CMAJ, CMIN, CPA)
         IF ((CMAJ.LE.0.0) .OR. (CMIN.LE.0.0)) THEN
            IRET = -1
            MSGTXT = 'OUTPUT BEAM SMALLER THAN PIXEL SIZE'
            GO TO 990
            END IF
C                                       Convert from arcsec,rad, to
C                                       pixels,rad
         CALL GASTPX (AMAJ, AMIN, APA, DMAJ, DMIN, DPA, DX, DY)
C                                       in original SMOTH, everything is
C                                       eventually scaled by the ratios
C                                       of the new to the old beam areas
C                                       This does not apply here since
C                                       the pixel units are Jy/new-beam
         ARB = 1.0
         CALL GASTPX (CMAJ, CMIN, CPA, DMAJ, DMIN, DPA, DX, DY)
         IF (ABS(DMAJ/DMIN-1.0).LE.0.01) DPA = 0.0
C                                       Compute kernel
         CALL KERNEL (XK, NX, NY, ARB, DMAJ, DMIN, DPA, IRET)
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.-100) MSGTXT = 'CONVOLVING FCN TOO SMALL'
            IF (IRET.EQ.100)  MSGTXT = 'CONVOLVING FCN TOO BIG'
            GO TO 990
            END IF
         MSGTXT = 'Spatial smoothing kernel computed'
         CALL MSGWRT (5)
C                                       Compute half size of kernel
         NX2 = NX/2
         NY2 = NY/2
C                                       Lengths of output array
         NXOUT = CATBLK(KINAX+1)
         NYOUT = CATBLK(KINAX+2)
         NVOUT = CATBLK(KINAX)
C                                       Determine start, stop pixel #
         I1 = NX2 + 1
         I2 = NXOUT - NX2
         I0 = I1 - 1
         I3 = I2 + 1
         J1 = NY2 + 1
         J2 = NYOUT - NY2
         J0 = J1 - 1
         J3 = J2 + 1
C                                       Check
         IF ((I1.GT.I2) .OR. (J1.GT.J2)) THEN
            IRET = -1
            MSGTXT = 'OUTPUT WINDOW SMALLER THAN BEAM SIZE'
            GO TO 990
            END IF
         END IF
C                                       Begin loop for convolution
      DO 100 KK = 1,NVOUT
C                                       Insert RESULT into TRESLT
         DO 40 JJ = 1,NYOUT
            DO 30 II = 1,NXOUT
               JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
               IF (DATA(JMC).NE.FBLANK) THEN
                  J = JMC - JBLANK(JMC)
                  DARRAY(II,JJ,1) = RESULT(J)
               ELSE
                  DARRAY(II,JJ,1) = 0.0D0
                  END IF
 30           CONTINUE
 40        CONTINUE
C                                       Smooth TRESLT
         CALL SMTH1 (KK, DARRAY, DATA, MX, MY, DARRAY(1,1,3), IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SMTH1 ERROR'
            GO TO 990
            END IF
C                                       Put TRESLT back into RESULT
         DO 60 JJ = 1,NYOUT
            DO 50 II = 1,NXOUT
               JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
               IF (DATA(JMC).NE.FBLANK) THEN
                  J = JMC - JBLANK(JMC)
                  RESULT(J) = DARRAY(II,JJ,1)
C                                       Get rid of high values for last
C                                       pass through of model only if
C                                       there will be no Vel smoothing
                  IF ((RESULT(J).GT.1.0E34) .AND. (RESULT(J).LT.1.0E36)
     *               .AND. (CPARM(1).EQ.1) .AND. (IFLAG.EQ.-1) .AND.
     *               (CPARM(6).EQ.0.0)) RESULT(J) = 0.0
                  END IF
 50            CONTINUE
 60         CONTINUE
C                                       End plane convolution.
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SMTH1 (KK, TRESLT, DATA, MX, MY, DARRAY, IRET)
C-----------------------------------------------------------------------
C   SMTH1 does the smoothing over a single plane.
C   Inputs:
C      KK                  I   pixel value of plane (V)
C      TRESLT(NXOUT,NYOUT) D   Array containing unsmoothed input pts
C   Outputs:
C      TRESLT(NXOUT,NYOUT) D   Array containing smoothed output pts
C      IRET                I   Error code - 0=okay
C                                 anything else=terminate
C-----------------------------------------------------------------------
      INCLUDE 'CUBSIZ.INC'
      INTEGER   KK, MX, MY, IRET
      REAL      DATA(*)
      DOUBLE PRECISION TRESLT(MX,MY), DARRAY(MX,MY)
C
      INTEGER   JMC, JM, NX, NY, II, JJ, MJ, LI, NX2, NY2, LL, MM, NT,
     *   I1, I2, J1, J2
      REAL      XK(25,25), X, Y
      INCLUDE 'CUBDIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /SMTH/ NY, NX, NY2, NX2, I1, I2, J1, J2, XK
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialize DARRAY for each plane
      DO 20 JJ = 1,NYOUT
         DO 10 II = 1,NXOUT
            DARRAY(II,JJ) = 0.0D0
 10         CONTINUE
 20      CONTINUE
C                                       Start loop over plane
      DO 200 JJ = J1,J2
         DO 195 II = I1,I2
C                                       Index for point of interest
            JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
            IF (DATA(JMC).NE.FBLANK) THEN
               X = 0.0
C                                       Start sum over kernel
               MJ = JJ - NY2 - 1
C                                       Keep track of the number of
C                                       times result is 1.0e35
C                                       (ie. bmodel is zero) in NT
               NT = 0
               DO 190 MM = 1,NY
                  MJ = MJ + 1
                  LI = II - NX2 - 1
                  DO 180 LL = 1,NX
                     LI = LI + 1
                     JM = KK + ((LI-1) + (MJ-1)*NXOUT) * NVOUT
C                                       If any pt within kernel
C                                       area is blanked, blank
C                                       the pt of interest
                     IF (DATA(JM).EQ.FBLANK) THEN
                        DARRAY(II,JJ) = FBLANK
                        GO TO 195
                        END IF
C                                       We don't want to convolve a
C                                       value of 1.0e35 so change to
C                                       zero and keep track
                     IF ((TRESLT(LI,MJ).GT.1.0E34) .AND.
     *                  (TRESLT(LI,MJ).LT.1.0E36)) THEN
                        Y = 0
                        NT = NT+1
                     ELSE
                        Y = TRESLT(LI,MJ)
                        END IF
                     X = X + XK(LL,MM) * Y
 180                 CONTINUE
 190              CONTINUE
C                                       Store the final value in
C                                       DARRAY for each plane
               DARRAY(II,JJ) = X

C                                       If every pt within convolving
C                                       kernel was 1.0e35,
C                                       then make the result 1.0e35
               IF (NT.EQ.NY*NX) DARRAY(II,JJ) = 1.0D35
               END IF
 195        CONTINUE
 200     CONTINUE
C                                       After convolving each plane
C                                       put DARRAY back into TRESLT
C                                       Remember there will be more
C                                       blanked values of RESULT
C                                       than before smoothing
      DO 250 JJ = 1,NYOUT
         DO 240 II = 1,NXOUT
            JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
C                                       If the value was converted
C                                       to zero BECAUSE of the
C                                       smoothing, ensure that it
C                                       will still be subtracted
C                                       from the data in FCN
            IF (DATA(JMC).NE.FBLANK) THEN
               IF ((TRESLT(II,JJ).NE.0.0) .AND. (DARRAY(II,JJ).EQ.0.0))
     *            THEN
                  TRESLT(II,JJ) = 1.0E35
               ELSE
                  TRESLT(II,JJ) = DARRAY(II,JJ)
                  END IF
               END IF
 240        CONTINUE
 250     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE KERNEL (XK, NX, NY, AREA, DMAJ, DMIN, DPA, ERROR)
C-----------------------------------------------------------------------
C   KERNEL calculates the convolution kernel for SMOTH.
C   Inputs:
C      AREA    R         Scaling factor for kernel
C      DMAJ    R          FWHM major axis of Gaussian
C      DMIN    R          FWHM minor axis of Gaussian
C      DPA     R          Position angle of Gaussian (radians)
C   Outputs:
C      XK      R(25,25)   Kernel
C      NX      I          Length of kernel in X (adj. array dim.)
C      NY      I          Length of kernel in Y (adj. array dim.)
C      ERROR   I          Error code:   0 => OK
C                            100 => Convolving function too big
C                                   for 25x25 kernel
C                           -100 => Convolving function too small
C   EPS contains the criterion for setting the size of the kernel, and
C   for deciding when the 25x25 kernel is too small.  The convolving
C   function is considered too small if the size of the kernel is
C   reduced to 1 in either direction.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      XK(25,25), AREA, DMAJ, DMIN, DPA
C
      REAL      X, Y, Z, SP, CP, L1, L2, A, B, TC, BYS, TCY, EPS,
     *   TENEPS, XMI(12), XMA(12), YMI(12), YMA(12), SUM
      INTEGER   NXR, NYR, ERROR, I, J, K, JJ, NXMIN, NXMAX, NYMIN, NYMAX
      DATA EPS /0.01/
C-----------------------------------------------------------------------
C                                       Set constants
      NX = 25
      NY = 25
      TENEPS = 10.0 * EPS
      SP = SIN (DPA)
      CP = COS (DPA)
      L1 = 4.0 * LOG(2.0) / (DMAJ*DMAJ)
      L2 = 4.0 * LOG(2.0) / (DMIN*DMIN)
      TC = 2.0 * SP * CP * (L2-L1)
      SP = SP*SP
      CP = CP*CP
      A = L1*SP + L2*CP
      B = L1*CP + L2*SP
C                                       Fill full kernel (symmetric)
C
      DO 20 J = 1,13
         Y = J - 13
         BYS = B * Y * Y
         TCY = TC * Y
         DO 10 I = 1,25
            X = I - 13
            Z = A*X*X + TCY*X + BYS
            XK(I,J) = 0.0
            IF (Z.LT.70.0) XK(I,J) = EXP (-Z)
            XK(26-I,26-J) = XK(I,J)
 10         CONTINUE
 20      CONTINUE
C                                       Determine size of kernel
      CALL KEREXT (XK, NX, NY, XMI, XMA, YMI, YMA)
      NXMAX = 0
      NXMIN = 0
      NXMAX = 0
      NYMIN = 0
      NYMAX = 0
      DO 110 K = 1,12
         IF (XMI(K).LT.TENEPS) NXMIN = K
         IF (XMA(K).LT.EPS) NXMAX = K
         IF (YMI(K).LT.TENEPS) NYMIN = K
         IF (YMA(K).LT.EPS) NYMAX = K
 110     CONTINUE
      NXR = MIN(NXMIN,NXMAX)
      NYR = MIN(NYMIN,NYMAX)
      ERROR = 0
      IF ((NXR.GE.12) .OR. (NYR.GE.12)) ERROR = -100
      IF ((NXMIN.LE.0) .OR. (NYMIN.LE.0)) ERROR = 100
      IF (ERROR.NE.0) GO TO 999
      NX = NX - 2*NXR
      NY = NY - 2*NYR
C                                       Reduce size of kernel
      SUM = 0.0
      DO 130 J = 1,NY
         JJ = J + NYR
         DO 120 I = 1,NX
            XK(I,J) = XK(I+NXR,JJ)
            SUM = SUM + XK(I,J)
 120        CONTINUE
 130     CONTINUE
C                                 SCALE KERNEL
C                                 the kernel is properly normalized
C                                 AREA=1
C
      SUM = AREA / SUM
      DO 150 J = 1,NY
         DO 140 I = 1,NX
            XK(I,J) = XK(I,J)*SUM
 140        CONTINUE
 150     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE KEREXT (XK, NX, NY, XMI, XMA, YMI, YMA)
C-----------------------------------------------------------------------
C   KEREXT finds the min and max along rows and columns in the kernel
C   Copied from the AIPS task 'SMOTH'
C   Inputs:
C      XK    R(NX,NY)   Kernel
C      NX    I          Size of kernel in X (adj. array dim.)
C      NY    I          Size of kernel in Y (adj. array dim.)
C   Outputs:
C      XMI   R(?)       Minima along columns
C      XMA   R(?)       Maxima along columns
C      YMI   R(?)       Minima along rows
C      YMA   R(?)       Maxima along rows
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      XK(NX,NY), XMI(*), XMA(*), YMI(*), YMA(*)
C
      INTEGER   NX2, NY2, I, J
      REAL      X, Y, Z
C-----------------------------------------------------------------------
      NX2 = NX/2
      NY2 = NY/2
C                                       First, the columns
      DO 20 I = 1,NX2
         X = 1.0
         Y = 0.0
         DO 10 J = 1,NY
            Z = XK(I,J)
            IF (Z.LT.X) X = Z
            IF (Z.GT.Y) Y = Z
 10         CONTINUE
         XMI(I) = X
         XMA(I) = Y
 20      CONTINUE
C                                       Then, the rows
      DO 40 J = 1,NY2
         X = 1.0
         Y = 0.0
         DO 30 I = 1,NX
            Z = XK(I,J)
            IF (Z.LT.X) X = Z
            IF (Z.GT.Y) Y = Z
 30         CONTINUE
         YMI(J) = X
         YMA(J) = Y
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GPXTAS (BA1, BB1, BP1, BA2, BB2, BP2, DX, DY)
C-----------------------------------------------------------------------
C   GPXTAS converts a Gaussian with parameters in pixels (cells)
C      - Halfwidths BA1, BB1; position angle BP1 (radians) -
C   to one with parameters in arcseconds
C      - Halfwidths BA2, BB2; position angle BP2 (radians) -
C   DX and DY are the cell sizes in RADIANS.
C   It has been coded by Arnold Rots in September 1980.
C-----------------------------------------------------------------------
      REAL      BA1, BB1, BP1, BA2, BB2, BP2, DX, DY
C
      REAL      T, L1, L2, M1, M2, SP, CP, C, U, V, W, PI, ARSRAD
      INTEGER   IPA, I1000, I500
      DATA PI /3.141592654/, ARSRAD /206264.8062/
      DATA I1000 /1000/
C-----------------------------------------------------------------------
      BP2 = BP1
      I500 = I1000 / 2
      IPA = NINT(BP1 / PI * REAL(I1000))
      IF (MOD(IPA,I1000).EQ.0) THEN
         BA2 = BA1 * DY * ARSRAD
         BB2 = BB1 * DX * ARSRAD
      ELSE IF (MOD(IPA,I500).EQ.0) THEN
         BA2 = BA1 * DX * ARSRAD
         BB2 = BB1 * DY * ARSRAD
      ELSE
         T = DX / DY
         CP = COS (BP1)
         SP = SIN (BP1)
         L1 = 1.0 / (BA1 * BA1)
         L2 = 1.0 / (BB1 * BB1)
         C = T * SP * CP * (L2-L1)
         SP = SP * SP
         CP = CP * CP
         T = T * T
C                                       THE REAL WORK
         U = (SP + T*CP) * L1
         V = (CP + T*SP) * L2
         W = SQRT ((U-V) * (U-V) + 4.0*SP*CP*(1.0-T)*(1.0-T)*L1*L2)
         M1 = 0.5 * (U + V - W)
         M2 = 0.5 * (U + V + W)
         BA2 = SQRT (1.0 / M1) * DX * ARSRAD
         BB2 = SQRT (1.0 / M2) * DX * ARSRAD
         BP2 = BP1
         M1 = SP * L1 + CP * L2 - M1
         IF ((C.NE.0.0) .OR. (M1.NE.0.0)) BP2 = ATAN2 (C, M1)
         IF (BP2.LT.0.0) BP2 = BP2 + PI
         END IF
C
 999  RETURN
      END
      SUBROUTINE GDECON (BA1, BB1, BP1, BA2, BB2, BP2, BA3, BB3, BP3)
C-----------------------------------------------------------------------
C   GDECON deconvolves one gaussian (e.g. observed; parameters BA1,BB1,
C   BP1) with another (e.g. beam; parameters BA2,BB2,BP2).  The result
C   is a new gaussian (e.g. true source; parameters BA3,BB3,BP3).
C   It has been coded by Arnold Rots in September 1980; copied from the
C   AIPS task 'SMOTH'
C-----------------------------------------------------------------------
      REAL   BA1, BB1, BP1, BA2, BB2, BP2, BA3, BB3, BP3
C
      REAL   D0, D1, D2, A, B
C-----------------------------------------------------------------------
      D0 = BA1*BA1 - BB1*BB1
      D2 = BA2*BA2 - BB2*BB2
      D1 = MAX (0.0, D0*D0+D2*D2-2.0*D0*D2*COS(2.0*(BP1-BP2)))
      D1 = SQRT (D1)
      BP3 = 0.0
      A = D0 * SIN (2.0*BP1) - D2 * SIN (2.0*BP2)
      B = D0 * COS (2.0*BP1) - D2 * COS (2.0*BP2)
      IF ((A.NE.0.0) .OR. (B.NE.0.0)) BP3 = 0.5 * ATAN2 (A, B)
      A = BA1*BA1 + BB1*BB1 - BA2*BA2 - BB2*BB2
      BA3 = MAX (0.0, 0.5*(A+D1))
      BA3 = SQRT (BA3)
      BB3 = MAX (0.0, 0.5*(A-D1))
      BB3 = SQRT (BB3)
      IF (BP3.LT.0.0) BP3 = BP3 + 3.141592654
C
 999  RETURN
      END
      SUBROUTINE GASTPX (BA1, BB1, BP1, BA2, BB2, BP2, DX, DY)
C---------------------------------------------------------------
C   Converts Gaussian in arc seconds to one in cells
C   Inputs:
C      BA1   R   Input Gaussian: Major axis in arc sec
C      BB1   R                   Minor axis in arc sec
C      BP1   R                   Position angle in radians
C      DX    R   X pixel separation in radians
C      DY    R   Y pixel separation in radians
C   Outputs
C      BA2   R   Output Gaussian: Major axis in cells
C      BB2   R                    Minor axis in cells
C      BP2   R                    Position angle in radians
C   It has been coded by Arnold Rots in September 1980; copied
C   from the AIPS task 'SMOTH'
C-----------------------------------------------------------------------
      REAL       BA1, BB1, BP1, BA2, BB2, BP2, DX, DY
C
      REAL       RASA
      DATA RASA /206264.8062/
C-----------------------------------------------------------------------
      BA2 = BA1 / (RASA*DX)
      BB2 = BB1 / (RASA*DX)
      BP2 = BP1
      CALL GPXTAS (BA2, BB2, BP2, BA2, BB2, BP2, DY, DX)
C   NOTE : THE ARGUMENTS IN THIS PARAMETER LIST ARE CORRECT!!
      BA2 = BA2 / (DY*RASA)
      BB2 = BB2 / (DY*RASA)
      RETURN
      END
      SUBROUTINE VSMTH (LROW, DATIN, DATOUT, IFLAG, IRET)
C-----------------------------------------------------------------------
C   VSMTH convolves an input row with a convolving look up table
C   established in common.
C   Inputs:
C      DATIN    D(*)    Input row, magic value blanked.
C   Values from commons:
C      FBLANK   R         Value of blanked pixel.
C      CPARM    R(10)     Input adverb array.
C      TAB      R(5000)   Convolution look-up table
C      SUPRAD   R         Conv. function support radius (old cells)
C      IXDIV    I         # divisions / old cell in look-up table
C      IFLAG    I         flag to indicate whether this is the last
C                         pass through of (all) the data (not just the
C                         row)
C   Output:
C      DATOUT   D(*)      Output row.
C      IRET     I         Return code   0 => OK
C                            >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   LROW, IFLAG, IRET
      DOUBLE PRECISION DATIN(LROW), DATOUT(LROW)
C
      INTEGER   TABSIZ, IXDIV, N, I, L, J, J1, J2, NT, IT
      REAL      CONST, DELVEL, TAB(5000), WCUTOF, FX, Y, W, S, XJ, X,
     *   XDIV, SUPRAD, WIDTH
      DOUBLE PRECISION TEMP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CUBIT.INC'
      COMMON /LEAST/ IT
      COMMON /PARM1/ CONST, DELVEL, WIDTH
      COMMON /VPARM1/ TAB, WCUTOF, SUPRAD, IXDIV
      DATA TABSIZ /5000/
C-----------------------------------------------------------------------
      IRET = 0
C                                      If tables, etc, have already
C                                      been set up, don't do it again
      IF ((IT.LE.0) .AND. ((WCUTOF.GE.0.015) .OR. (WCUTOF.LE.0.005)))
     *   THEN
C                                       Convolution: parms & tables
C                                       Set window over which the
C                                       gaussian has meaning to 3 X
C                                       the HWHM of the gaussian
         SUPRAD = 3.0 * WIDTH / 2.0
C
         IXDIV = (TABSIZ - 1) / (SUPRAD + 0.1)
         IF (IXDIV.GT.500) IXDIV = 500
C                                       WCUTOF = .99 demands a true
C                                       convolution.  .01 smooths over
C                                       blanked pixels
C                                     Set it for smoothing over blanks
         WCUTOF = 0.01
         CALL RFILL (TABSIZ, 0.0, TAB)
         N = 1 + SUPRAD*IXDIV + 0.99
         FX = 2.0 / WIDTH
         Y = 1.0 / IXDIV
         TAB(1) = 1.0
C                                       Compute look-up table
C                                       for gaussian
         FX = -LOG(2.0) * FX * FX
         DO 10 I = 2,N
            X = Y * (I - 1)
            TAB(I) = EXP(FX * X * X)
 10      CONTINUE
C                                       Normalize integral
         W = -TAB(1)
         DO 15 I = 1,N,IXDIV
            W = W + 2.0 * TAB(I)
 15         CONTINUE
         DO 20 I = 1,N
            TAB(I) = TAB(I) / W
 20         CONTINUE
         MSGTXT = 'Lookup table for velocity smoothing computed'
         CALL MSGWRT (5)
         END IF
C                                        Start the convolution
      XJ = 0.0
      XDIV = IXDIV
      DO 100 I = 1,LROW
         XJ = XJ + 1.0
         J1 = XJ - SUPRAD + 0.9999
         J2 = XJ + SUPRAD
         J1 = MAX (1, J1)
         J2 = MIN (LROW, J2)
         S = 0.0
         W = 0.0
C                                    keep track of how many times
C                                    result is 1e35
         NT = 0
C                **NB** DO NOT automatically skip zeroed points
C                because after velocity smoothing there could
C                be some emission at forbidden velocities
C            But skip zeroed points if there's no velocity smoothing
         IF ((DATIN(I).EQ.0.0) .AND. (CPARM(6).EQ.0.0)) THEN
            DATOUT(I) = 0.0
         ELSE
C                                    if input=blank, output=blank
            IF (DATIN(I).NE.FBLANK) THEN
               DO 30 J = J1,J2
                  TEMP = DATIN(J)
                  IF (TEMP.EQ.FBLANK) THEN
                     NT = NT + 1
                  ELSE
                     IF ((TEMP.GT.1.0E34) .AND. (TEMP.LT.1.0E36)) TEMP
     *                  = 0.0
                     IF (TEMP.EQ.0.0) NT = NT + 1
                     L = ABS (XJ-J) * XDIV + 1.5
                     S = TEMP * TAB(L) + S
                     W = TAB(L) + W
                     END IF
 30               CONTINUE
               END IF
C                                        blank the pts which fall
C                                        below the cutoff
            DATOUT(I) = FBLANK
            IF (W.GT.WCUTOF) DATOUT(I) = S / W
C                                        *All* pts in kernel=zero or
C                                        blank => output to 1e35
C                                        to be subtracted in FCN
            IF (NT.EQ.(J2-J1+1)) DATOUT(I)=1.0E35
C                                        Any newly zero'd pts should be
C                                        set high for subtraction later
            IF (DATOUT(I).EQ.0.0) DATOUT(I)=1.0E35
C                                        If it's the last pass through,
C                                        zero the high points
            IF ((DATOUT(I).GT.1.0E34) .AND. (DATOUT(I).LT.1.0E36) .AND.
     *         (IFLAG.EQ.-1) .AND. (CPARM(1).EQ.1)) DATOUT(I) = 0.0
C                                         Velocity smoothing decreases
C                                         I and spreads it out over a
C                                         larger V. However, since the
C                                         density, n, is calculated for
C                                         the presmoothed data, there is
C                                         no need to compensate here.
C                                         ..so the next lines are
C                                         commented out.
C            IF ((DATOUT(I).GT.1.0E34 .AND. DATOUT(I).LT.1.0E36)
C     *          .OR. DATOUT(I).EQ.FBLANK) GO TO 30
C                                         The factor 1.06892 is the
C                                         difference between the inte-
C                                         gral of a gaussian over its
C                                         FWHM and the integral of a
C                                         gaussian from -infty to +infty
C            IF (WIDTH.GT.1.0) DATOUT(I) = DATOUT(I) * WIDTH * 1.06892
C            IF (WIDTH.LE.1.0 .AND. WIDTH.GE.0.83) DATOUT(I)
C     *            = DATOUT(I) * 1.06892
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE LSTSQ1 (M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   DATA, JBLANK, MX, MY, DARRAY)
C-----------------------------------------------------------------------
C     The purpose of LSTSQ1 is to minimize the sum of the squares of
C     M nonlinear functions in N variables by a modification of the
C     Levenberg-Marquardt algorithm which users minimum storage.
C     This is done by using the more general least-squares solver
C     LSTSQ. The user must provide a subroutine which calculates the
C     functions and the rows of Jacobian.
C
C     Subprograms called:
C        User-supplied ...... FCN
C                      ...... SMTH1
C        Minpack-supplied ... LSTSQ (LMSTR modified by user)
C
C     Argonne National Laboratory. Minpack project. March 1980.
C     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Functions:
C      INPUTS:  FCN     The user-supplied subroutine which calculates
C                       the functions and the rows of the Jacobian.
C               SMTH1   The user supplied subroutine which smooths over
C                       an RA/DEC plane of the cube
C         SUBROUTINE FCN (M, N, X, FVEC, FJROW1, FJROW2, IFLAG)
C         SUBROUTINE SMTH1 (KK, ARRAYP, IRET)
C
C         INTEGER M,N, IFLAG
C         DOUBLE PRECISION X(N),FVEC(M),FJROW(N)
C         ----------
C         IF IFLAG = 1 CALCULATE THE FUNCTIONS AT X AND
C         RETURN THIS VECTOR IN FVEC.
C         IF IFLAG <> 1 CALCULATE THE (IFLAG - 1) ST ROW OF THE
C         JACOBIAN AT X AND RETURN THIS VECTOR IN FJROW.
C         ----------
C         RETURN
C         END
C
C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS
C         THE USER WANTS TO TERMINATE EXECUTION OF LMSTR1.
C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.
C**********************************************************************
C           M       I The number of functions (adj. array dim.).
C
C           N       I The number of variables. N must not exceed M
C                   (adj. array dim.).
C
C           X       D(N)   contains an initial estimate of the
C                   solution vector.
C
C           TOL     D   termination occurs when the algorithm
C                   estimates either that the relative
C                   error in the sum of squares is at most TOL or that
C                   the relative error between X and the solution is at
C                   most TOL.
C
C  OUTPUTS: X       D(N)   contains the final estimate of the
C                   solution vector.
C
C           FVEC    D(M)   contains the functions evaluated at
C                   the output X.
C
C           FJAC    D(N,N)   The upper N by N submatrix of FJAC
C                   contains an upper triangular matrix R
C                   such that
C
C                    T     T           T
C                   P *(JAC *JAC)*P = R *R,
C
C                   where P is a permutation matrix and JAC is the final
C                   calculated Jacobian. Column J of P is column IPVT(J)
C                   (see below) of the identity matrix. The lower
C                   triangular part of FJAC contains information
C                   generated during the computation of R.
C
C          LDFJAC   I variable not less than N which specifies the
C                   leading dimension of the array FJAC (adj. array
C                   dim.)
C
C          INFO     I   If the user has terminated execution, INFO is
C                   set to the (negative) value of IFLAG. SEE
C                   description of FCN. otherwise, INFO is set
C                   as follows:
C
C                   INFO = 0  Improper input parameters.
C
C                   INFO = 1  Algorithm estimates that the relative
C                   error in the sum of squares is at most TOL.
C
C                   INFO = 2  Algorithm estimates that the relative
C                   error between X and the solution is at most TOL.
C
C                   INFO = 3  Conditions for INFO = 1 and INFO = 2
C                   both hold.
C
C                   INFO = 4  FVEC is orthogonal to the columns of the
C                   Jacobian to machine precision.
C
C                   INFO = 5  Number of calls to FCN with IFLAG = 1 HAS
C                   reached 100*(N+1).
C
C                   INFO = 6  TOL is too small. No further reduction in
C                   the sum of squares is possible.
C
C                   INFO = 7  TOL is too small. no further improvement
C                   in the approximate solution X is possible.
C
C          IPVT     I(N)   Defines a permutation matrix P such that
C                   JAC*P = Q*R,
C                   where JAC is the final calculated Jacobian, Q is
C                   orthogonal (not stored), and R is upper triangular.
C                   Column J of P is column IPVT(J) of the identity
C                   matrix.
C-----------------------------------------------------------------------
      INTEGER   M, N, LDFJAC, INFO, IPVT(N), MX, MY, JBLANK(*)
      REAL      DATA(*)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), TOL,
     *   DARRAY(MX,MY,3)
C
      INCLUDE 'CUBSIZ.INC'
      LONGINT   LPWA, IPWA, LPHA
      INTEGER   MAXFEV, MODE, NFEV, NJEV, NPRINT, NWRDWA, IERR, NWRDHA
      DOUBLE PRECISION FACTOR, FTOL, GTOL, XTOL, ZERO, WA1(MAXPRM),
     *   WA2(MAXPRM), WA3(MAXPRM), WA4(MAXPRM), WA5(MAXPRM), WA(2)
      REAL      HARRAY(2)
      INCLUDE 'CUBDIM.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA FACTOR, ZERO /1.0D2, 0.0D0/
C-----------------------------------------------------------------------
      INFO = 0
C                                       Check the input parameters for
C                                       for errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.N) .OR. (TOL.LT.ZERO))
     *   THEN
         MSGTXT = 'LSTSQ1 INPUT PARAMETER ERROR'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1000) N, M, LDFJAC, TOL
         CALL MSGWRT (8)
C                                       okay - do it
      ELSE
C                                       Get scratch memory
         NWRDWA = (2 * M + 10) / 1024 + 1
         CALL ZMEMRY ('GET ', 'LSTSQ1', NWRDWA, WA, IPWA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, NWRDWA
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IF (IPWA.GE.0) THEN
            LPWA = 1 + (IPWA+1)/2
         ELSE
            LPWA = 1 + IPWA / 2
            END IF
         NWRDHA = (NXOUT * NYOUT * 30) / 1024 + 1
         CALL ZMEMRY ('GET ', 'LSTSQ1', NWRDHA, HARRAY, LPHA, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, NWRDWA
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Call LSTSQ.
         MAXFEV = 100*(N + 1)
         FTOL = TOL
         XTOL = TOL
         GTOL = ZERO
         MODE = 1
         NPRINT = 0
         CALL LSTSQ (M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL, GTOL,
     *      MAXFEV, WA1, MODE, FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT,
     *      WA2, WA3, WA4, WA5, WA(LPWA), NXOUT, NYOUT, HARRAY(1+LPHA),
     *      HARRAY(1+LPHA+NWRDHA/3), DATA, JBLANK, DARRAY)
         IF (INFO.EQ.8) INFO = 4
C                                       free memory
         CALL ZMEMRY ('FREE', 'LSTSQ1', NWRDWA, WA, IPWA, IERR)
         CALL ZMEMRY ('FREE', 'LSTSQ1', NWRDHA, HARRAY, LPHA, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('  N, M, LDFJAC, TOL =',I3,I10,I4,1PE10.3)
 1010 FORMAT ('LSTSQ1: ERROR',I3,' GETTING',I10,' WORDS OF MEMORY')
      END
      SUBROUTINE LSTSQ (M, N, X, FVEC, FJAC, LDFJAC, FTOL, XTOL, GTOL,
     *   MAXFEV, DIAG, MODE, FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT,
     *   QTF, WA1, WA2, WA3, WA4, MX, MY, HARAYP, HARAYM, DATA, JBLANK,
     *   DARRAY)
C-----------------------------------------------------------------------
C   The purpose of LSTSQ is to minimize the sum of the squares of
C   M nonlinear functions in N variables by a modification of
C   the Levenberg-Marquardt algorithm which uses minimum storage.
C   The user must provide a subroutine which calculates the functions
C   and the rows of the Jacobian.
C   June/91 -- added the criterion that if all derivatives are zero,
C     the residual value should be as well.
C   Subprograms called
C       User-supplied ...... FCN
C                     ...... SMTH1
C       Minpack-supplied ... DPMPAR,ENORM,LMPAR,QRFAC
C       FORTRAN-supplied ... DABS,DMAX1,DMIN1,DSQRT,MOD
C   Argonne National Laboratory. Minpack project. March 1980.
C   Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
C   Inputs:
C      M        I      The number of functions (adj. array dim.).
C      N        I      The number of variables. N must not exceed M
C      X        D(N)   Contains an initial estimate of solution vector.
C      FTOL     D      Termination occurs when both the actual and
C                      predicted relative reductions in the sum of
C                      squares are at most FTOL.  Therefore, FTOL
C                      measures the relative error desired in the sum
C                      of squares.
C      XTOL     D      Termination occurs when the relative error
C                      between two consecutive iterates is at most XTOL
C                      Therefore, XTOL measures the relative error
C                      desired in the approximate solution.
C      GTOL     D      Termination occurs when the cosine of the angle
C                      between FVEC and any column of the Jacobian is at
C                      most GTOL in absolute value. Therefore, GTOL
C                      measures the orthogonality desired between the
C                      function vector and the columns of the Jacobian.
C      MAXFEV   I      Termination occurs when the number of calls to
C                      FCN with IFLAG = 1 has reached MAXFEV.
C      DIAG     D(N)   If MODE = 1 (see below), DIAG is internally set.
C                      If MODE = 2, DIAG must contain positive entries
C                      that serve as multiplicative scale factors for
C                      the variables.
C      MODE     I      If MODE = 1, the variables will be scaled
C                      internally.  If MODE = 2, the scaling is
C                      specified by the input DIAG. Other values of mode
C                      are equivalent to MODE = 1.
C      FACTOR   D      Used in determining the initial step bound.  This
C                      bound is set to the product of FACTOR and the
C                      Euclidean norm of DIAG*X if nonzero, or else to
C                      FACTOR itself. In most cases FACTOR should lie in
C                      the interval (.1,100.0).  100.0 is a generally
C                      recommended value.
C      NPRINT   I      Enables controlled printing of iterates if it is
C                      positive. In this case, FCN is called with IFLAG
C                      = 0 at the beginning of the first iteration and
C                      every NPRINT iterations thereafter and
C                      immediately prior to return, with X, FVEC, and
C                      FJAC available for printing. FVEC and FJAC should
C                      not be altered. If NPRINT is not positive, no
C                      special calls of FCN with IFLAG = 0 are made.
C      LDFJAC   I      Variable not less than N which specifies the
C                      leading dimension of the array FJAC
C      DATA     R(*)   Input data (NVOUT, NXOUT, NYOUT)
C      JBLANK   I(*)   # blanked points before pixel of interest,
C                      alligned with DATA
C   Outputs:
C      X        D(N)   Contains the final estimate of solution vector.
C      FVEC     D(M)   Contains the functions evaluated at the output X
C      FJAC     D(N,N) The N by N matrix FJAC contains an upper
C                      triangular matrix R with diagonal elements of
C                      nonincreasing magnitude such that
C
C                          T     T           T
C                         P *(JAC *JAC)*P = R *R,
C
C                      where P is a permutation matrix and JAC is the
C                      final calculated Jacobian. Column J of P is
C                      column IPVT(J) (see below) of the identity
C                      matrix. The lower trapezoidal part of FJAC
C                      contains information generated during the
C                      computation of R.
C      INFO     I      If the user has terminated execution, INFO is
C                      set to the (negative) value of IFLAG. SEE
C                      description of FCN. otherwise, INFO is set as:
C                      = 0  Improper input parameters.
C                      = 1  Algorithm estimates that the relative
C                           error in the sum of squares is at most FTOL
C                      = 2  Algorithm estimates that the relative error
C                           between X and the solution is at most XTOL.
C                      = 3  Conditions for INFO = 1 and INFO = 2 both
C                           hold.
C                      = 4  The cosine of the angle between FVEC and
C                           any column of the Jacobian is at most GTOL
C                           in absolute value.
C                      = 5  Number of calls to FCN with IFLAG = 1 has
C                           reached MAXFEV.
C                      = 6  FTOL is too small. No further reduction in
C                           the sum of squares is possible.
C                      = 7  XTOL is too small. No further improvement
C                           in the approximate solution X is possible.
C                      = 8  GTOL is too small. FVEC is orthogonal to
C                           the columns of the Jacobian to machine
C                           precision.
C      NFEV     I      Number of calls to FCN with IFLAG = 1.
C      NJEV     I      The number of calls to FCN with IFLAG = 2.
C      IPVT     I(N)   Defines a permutation matrix P such that
C                          JAC*P = Q*R,
C                      where JAC is the final calculated Jacobian, Q is
C                      orthogonal (not stored), and R is upper
C                      triangular with diagonal elements of
C                      nonincreasing magnitude.  Column J of P is column
C                      IPVT(J) of the identity matrix.
C
C      QTF      D(N)   contains the first N elements of the vector
C                      (Q transpose)*FVEC.
C      WA1      D(N)   work array.
C      WA2      D(N)   work array.
C      WA3      D(N)   work array.
C      WA4      D(M)   work array.
C      DARRAY   D(*)   work array (NXOUT, NYOUT, 3)
C      HARAYP   R(*)   work array (NXOUT, NYOUT, Nmax)
C      HARAYM   R(*)   work array (NXOUT, NYOUT, Nmax)
C-----------------------------------------------------------------------
      INCLUDE 'CUBSIZ.INC'
      INTEGER   M, N, LDFJAC, MAXFEV, MODE, NPRINT, INFO, NFEV, NJEV,
     *   IPVT(N), MX, MY, JBLANK(*)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), FTOL, XTOL, GTOL,
     *   DIAG(N), FACTOR, QTF(N), WA1(N), WA2(N), WA3(N), WA4(M),
     *   DARRAY(MX,MY,3)
      REAL      HARAYP(MX,MY,MAXPRM), HARAYM(MX,MY,MAXPRM), DATA(*)
     *
C
      INCLUDE 'CUBDIM.INC'
      INTEGER   IJ, I, IFLAG, JMC, J0, IHOLD, ITER, J, L, IRET, KK, NNN,
     *   JJ, II
      LOGICAL   SING, T
      DOUBLE PRECISION ACTRED, DELTA, DIRDER, EPSMCH, FNORM, FNORM1,
     *   GNORM, ONE, PAR, PNORM, PRERED, P1, P5, P25, P75, P0001, RATIO,
     *   SUM, TEMP, TEMP1, TEMP2, XNORM, ZERO
      DOUBLE PRECISION    DPMPAR, ENORM
      DOUBLE PRECISION    DERP(MAXPRM), DERM(MAXPRM)
      INCLUDE 'INCS:DDCH.INC'
      COMMON /JBLK/ IHOLD
      DATA T /.TRUE./
      DATA ONE, P1, P5, P25, P75, P0001, ZERO
     *   /1.0D0, 1.0D-1, 5.0D-1, 2.5D-1, 7.5D-1, 1.0D-4, 0.0D0/
C-----------------------------------------------------------------------
C                                       EPSMCH is the machine precision
      EPSMCH = DPMPAR(1)
C
      INFO = 0
      IFLAG = 0
      NFEV = 0
      NJEV = 0
C                                       Check the input parameters for
C                                       errors.
      IF ((N.LE.0) .OR. (M.LT.N) .OR. (LDFJAC.LT.N) .OR. (FTOL.LT.ZERO)
     *   .OR. (XTOL.LT.ZERO) .OR. (GTOL.LT.ZERO) .OR. (MAXFEV.LE.0) .OR.
     *   (FACTOR.LE.ZERO)) GO TO 980
      IF (MODE.EQ.2) THEN
         DO 10 J = 1,N
            IF (DIAG(J).LE.ZERO) GO TO 980
 10         CONTINUE
         END IF
C                                       Evaluate the function at the
C                                       starting point and calculate
C                                       its norm.
      IFLAG = 1
C                                       Nothing happens with DERP or
C                                       DERM on this call
      CALL FCN (M, N, X, FVEC, DERP, DERM, IFLAG, DATA, JBLANK, MX, MY,
     *   DARRAY)
      IF (IFLAG.EQ.-100) GO TO 980
      NFEV = 1
      IF (IFLAG.LT.0) GO TO 980
      FNORM = ENORM (M, FVEC)
C                                       Initialize Levenberg-Marquardt
C                                       parameter and iteration counter
      PAR = ZERO
      ITER = 1
C                                       Beginning of the outer loop.
 30   CONTINUE
C                                       If requested, call FCN to
C                                       enable printing of iterates.
         IF (NPRINT.GT.0) THEN
            IFLAG = 0
            IF (MOD(ITER-1,NPRINT).EQ.0) CALL FCN (M, N, X, FVEC,
     *         DERP,DERM, IFLAG, DATA, JBLANK, MX, MY, DARRAY)
            IF (IFLAG.LT.0) GO TO 980
            END IF
C                                       Compute the QR factorization
C                                       of the Jacobian matrix
C                                       calculated one row at a time,
C                                       while simultaneously
C                                       forming (Q transpose)*FVEC
C                                       and storing the first
C                                       N components in QTF.
         DO 60 J = 1,N
            QTF(J) = ZERO
            DO 50 I = 1,N
               FJAC(I,J) = ZERO
 50            CONTINUE
 60         CONTINUE
         IFLAG = 2
C*******************************************sectioning off new part
C                                      Start loop over cube
         DO 800 KK = 1,NVOUT
C                                      Compute model over plane
            DO 400 JJ = 1,NYOUT
               DO 390 II = 1,NXOUT
                  JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
                  IF (DATA(JMC).NE.FBLANK) THEN
                     J0 = JMC - JBLANK(JMC)
                     IFLAG = J0 + 1
C                                      Return N 'plus values' in
C                                      DERP and N 'minus values'
C                                      in DERM for a single pt.
C                                      FVEC shouldn't change from
C                                      before.
                     IHOLD = JMC
                     IF (IHOLD.EQ.0) IFLAG = -100
                     CALL FCN (M, N, X, FVEC, DERP, DERM, IFLAG, DATA,
     *                  JBLANK, MX, MY, DARRAY)
                     IF (IFLAG.EQ.-100) GO TO 980
C                                      Store the values in an array.
                     DO 380 NNN=1,N
                        HARAYP(II,JJ,NNN) = DERP(NNN)
                        HARAYM(II,JJ,NNN) = DERM(NNN)
 380                    CONTINUE
                     END IF
 390              CONTINUE
 400           CONTINUE
C                                      Start loop for smoothing
            DO 475 NNN = 1,N
               DO 430 JJ = 1,NYOUT
                  DO 425 II = 1,NXOUT
                     DARRAY(II,JJ,1) = HARAYP(II,JJ,NNN)
                     DARRAY(II,JJ,2) = HARAYM(II,JJ,NNN)
 425                 CONTINUE
 430              CONTINUE
C                                       Smooth over the plane and
C                                       for a single parameter
               CALL SMTH1 (KK, DARRAY(1,1,1), DATA, MX, MY,
     *            DARRAY(1,1,3), IRET)
C                                       CHECK FOR ERROR CONDITION
               CALL SMTH1 (KK, DARRAY(1,1,2), DATA, MX, MY,
     *            DARRAY(1,1,3), IRET)
C                                       CHECK FOR ERROR CONDITION
C                                       Compute the derivatives
C                                       for each pt in the plane
C                                       and a single parameter
               DO 450 JJ = 1,NYOUT
                  DO 445 II = 1,NXOUT
                     JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
                     J0 = JMC - JBLANK(JMC)
                     IF (DATA(JMC).NE.FBLANK) THEN
C                                      Blanks could have been
C                                      returned in ARRAY so change
C                                      them to zeros.
                        IF (DARRAY(II,JJ,1).EQ.FBLANK) DARRAY(II,JJ,1) =
     *                     0.0D0
                        IF (DARRAY(II,JJ,2).EQ.FBLANK) DARRAY(II,JJ,2) =
     *                     0.0D0
C                                      High values could have been
C                                      returned so change them
C                                      to zeros.
                        IF ((DARRAY(II,JJ,1).GT.1.0E34) .AND.
     *                     (DARRAY(II,JJ,1).LT.1.0E36)) DARRAY(II,JJ,1)
     *                     = 0.0D0
                        IF ((DARRAY(II,JJ,2).GT.1.0E34) .AND.
     *                     (DARRAY(II,JJ,2).LT.1.0E36)) DARRAY(II,JJ,2)
     *                     = 0.0D0
C                                      Put computed derivative
C                                      into HARAYP
                        HARAYP(II,JJ,NNN) = DARRAY(II,JJ,1) -
     *                     DARRAY(II,JJ,2)
C                                      If either value was zero
C                                      set the deriv to zero
                        IF ((DARRAY(II,JJ,1).EQ.0.0D0) .OR.
     *                     (DARRAY(II,JJ,2).EQ.0.0D0)) HARAYP(II,JJ,NNN)
     *                     = 0.0
                        END IF
 445                 CONTINUE
 450              CONTINUE
 475           CONTINUE
C                                      Now all derivatives have been
C                                      calculated for every pt in
C                                      a single plane.
C                                      We now want to enter ALL
C                                      derivatives into RWUPDT
C                                      pt by pt.
            DO 500 JJ = 1,NYOUT
               DO 495 II = 1,NXOUT
                  JMC = KK + ((II-1) + (JJ-1)*NXOUT) * NVOUT
                  IF (DATA(JMC).NE.FBLANK) THEN
                     J0 = JMC - JBLANK(JMC)
                     DO 480 NNN = 1,N
                        WA3(NNN) = HARAYP(II,JJ,NNN)
 480                    CONTINUE
C                                  Make the derivative zero
C                                  if the residual (FVEC) is zero
C                                  Don't let the residual
C                                  have a value if all derivatives
C                                  are zero
                     TEMP = 0.0
                     DO 485 NNN = 1,N
                        IF (WA3(NNN).NE.0.0) THEN
                           IF (FVEC(J0).EQ.0.0) THEN
                              WA3(NNN)=0.0
                           ELSE
                              TEMP=FVEC(J0)
                              GO TO 490
                              END IF
                           END IF
 485                    CONTINUE
 490                 CALL RWUPDT (N, FJAC, LDFJAC, WA3, QTF, TEMP, WA1,
     *                  WA2)
                     END IF
 495              CONTINUE
 500           CONTINUE
C                                      Finished.  Now go back and
C                                      start on next plane
 800        CONTINUE
C*******************************************sectioning off new part
         NJEV = NJEV + 1
C                                       If the Jacobian is rank
C                                       deficient, call QRFAC to
C                                       reorder its columns and update
C                                       the components of QTF.
         SING = .FALSE.
         DO 80 J = 1,N
            IF (FJAC(J,J).EQ.ZERO) SING = .TRUE.
            IPVT(J) = J
            IJ = J
            WA2(J) = ENORM (IJ, FJAC(1,J))
 80         CONTINUE
         IF (SING) THEN
            CALL QRFAC (N, N, FJAC, LDFJAC, T, IPVT, N, WA1, WA2, WA3)
            DO 120 J = 1,N
               IF (FJAC(J,J).NE.ZERO) THEN
                  SUM = ZERO
                  DO 90 I = J,N
                     SUM = SUM + FJAC(I,J) * QTF(I)
 90                  CONTINUE
                  TEMP = -SUM / FJAC(J,J)
                  DO 100 I = J,N
                     QTF(I) = QTF(I) + FJAC(I,J) * TEMP
 100                 CONTINUE
                  END IF
               FJAC(J,J) = WA1(J)
 120           CONTINUE
            END IF
C                                       On the first iteration and if
C                                       mode is 1, scale according
C                                       to the norms of the columns
C                                       of the initial Jacobian.
C
         IF (ITER.EQ.1) THEN
            IF (MODE.NE.2) THEN
               DO 140 J = 1,N
                  DIAG(J) = WA2(J)
                  IF (WA2(J).EQ.ZERO) DIAG(J) = ONE
 140              CONTINUE
               END IF
C                                       On the first iteration,
C                                       calculate the norm of the scaled
C                                       X and initialize the step bound
C                                       DELTA.
            DO 160 J = 1,N
               WA3(J) = DIAG(J) * X(J)
 160           CONTINUE
            XNORM = ENORM (N, WA3)
            DELTA = FACTOR * XNORM
            IF (DELTA.EQ.ZERO) DELTA = FACTOR
            END IF
C                                       Compute the norm of the scaled
C                                       gradient.
         GNORM = ZERO
         IF (FNORM.NE.ZERO) THEN
            DO 200 J = 1,N
               L = IPVT(J)
               IF (WA2(L).NE.ZERO) THEN
                  SUM = ZERO
                  DO 180 I = 1,J
                     SUM = SUM + FJAC(I,J) * (QTF(I)/FNORM)
 180                 CONTINUE
                  GNORM = MAX (GNORM, ABS (SUM/WA2(L)))
                  END IF
 200           CONTINUE
            END IF
C                                       Test for convergence of the
C                                       gradient norm.
         IF (GNORM.LE.GTOL) INFO = 4
         IF (INFO.NE.0) GO TO 980
C                                       Rescale if necessary.
         IF (MODE.NE.2) THEN
            DO 220 J = 1,N
               DIAG(J) = MAX (DIAG(J), WA2(J))
 220           CONTINUE
            END IF
C                                       Beginning of the inner loop.
 240     CONTINUE
C                                       Determine the Levenberg-
C                                       Marquardt parameter.
            CALL LMPAR (N, FJAC, LDFJAC, IPVT, DIAG, QTF, DELTA, PAR,
     *         WA1, WA2, WA3, WA4)
C                                       Store the direction P and
C                                       X + P. Calculate the norm of P.
            DO 250 J = 1,N
               WA1(J) = -WA1(J)
               WA2(J) = X(J) + WA1(J)
               WA3(J) = DIAG(J)*WA1(J)
 250           CONTINUE
            PNORM = ENORM(N,WA3)
C                                       On the first iteration, adjust
C                                       the initial step bound.
            IF (ITER.EQ.1) DELTA = MIN (DELTA, PNORM)
C                                       Evaluate the function at X + P
C                                       and calculate its norm.
            IFLAG = 1
C                                       There should be no change to
C                                       DERP, DERM on this call
            CALL FCN (M, N, WA2, WA4, DERP, DERM, IFLAG, DATA, JBLANK,
     *         MX, MY, DARRAY)
            NFEV = NFEV + 1
            IF (IFLAG.LT.0) GO TO 980
            FNORM1 = ENORM (M, WA4)
C                                       Compute the scaled actual
C                                       reduction.
            ACTRED = -ONE
            IF (P1*FNORM1.LT.FNORM) ACTRED = ONE - (FNORM1/FNORM)**2
C                                       Compute the scaled predicted
C                                       reduction and the scaled
C                                       directional derivative.
            DO 270 J = 1,N
               WA3(J) = ZERO
               L = IPVT(J)
               TEMP = WA1(L)
               DO 260 I = 1,J
                  WA3(I) = WA3(I) + FJAC(I,J) * TEMP
 260              CONTINUE
 270           CONTINUE
            TEMP1 = ENORM (N, WA3) / FNORM
            TEMP2 = (SQRT(PAR) * PNORM) / FNORM
            PRERED = TEMP1**2 + TEMP2**2 / P5
            DIRDER = -(TEMP1**2 + TEMP2**2)
C                                       Compute the ratio of the actual
C                                       to the predicted reduction.
            RATIO = ZERO
            IF (PRERED.NE.ZERO) RATIO = ACTRED / PRERED
C                                       Update the step bound.
            IF (RATIO.LE.P25) THEN
               IF (ACTRED.GE.ZERO) TEMP = P5
               IF (ACTRED.LT.ZERO)
     *            TEMP = P5 * DIRDER / (DIRDER + P5*ACTRED)
               IF ((P1*FNORM1.GE.FNORM) .OR. (TEMP.LT.P1)) TEMP = P1
               DELTA = TEMP * MIN (DELTA, PNORM/P1)
               PAR = PAR / TEMP
            ELSE IF ((PAR.EQ.ZERO) .OR. (RATIO.GE.P75)) THEN
               DELTA = PNORM / P5
               PAR = P5 * PAR
               END IF
C                                       Test for successful iteration.
            IF (RATIO.GE.P0001) THEN
C                                       Successful iteration. update X,
C                                       FVEC, and their norms.
               DO 310 J = 1,N
                  X(J) = WA2(J)
                  WA2(J) = DIAG(J) * X(J)
 310              CONTINUE
               DO 320 I = 1,M
                  FVEC(I) = WA4(I)
 320              CONTINUE
               XNORM = ENORM (N, WA2)
               FNORM = FNORM1
               ITER = ITER + 1
               END IF
C                                       Tests for convergence.
            IF ((ABS(ACTRED).LE.FTOL) .AND. (PRERED.LE.FTOL) .AND.
     *         (P5*RATIO.LE.ONE)) INFO = 1
            IF (DELTA.LE.XTOL*XNORM) INFO = 2
            IF ((ABS(ACTRED).LE.FTOL) .AND. (PRERED.LE.FTOL) .AND.
     *         (P5*RATIO.LE.ONE) .AND. (INFO.EQ.2)) INFO = 3
            IF (INFO.NE.0) GO TO 980
C                                       Tests for termination and
C                                       stringent tolerances.
            IF (NFEV.GE.MAXFEV) INFO = 5
            IF ((ABS(ACTRED).LE.EPSMCH) .AND. (PRERED.LE.EPSMCH)
     *         .AND. (P5*RATIO.LE.ONE)) INFO = 6
            IF (DELTA.LE.EPSMCH*XNORM) INFO = 7
            IF (GNORM.LE.EPSMCH) INFO = 8
            IF (INFO.NE.0) GO TO 980
C                                       End of the inner loop.  Repeat
C                                       if iteration unsuccessful.
            IF (RATIO.LT.P0001) GO TO 240
C                                       End of the outer loop.
         GO TO 30
C                                       Termination, either normal or
C                                       user imposed.
 980  IF (IFLAG.LT.0) INFO = IFLAG
      IFLAG = 0
      IF (NPRINT.GT.0) CALL FCN (M, N, X, FVEC, DERP, DERM, IFLAG, DATA,
     *   JBLANK, MX, MY, DARRAY)
C
 999  RETURN
      END
