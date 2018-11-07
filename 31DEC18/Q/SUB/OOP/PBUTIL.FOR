C   Frequency dependent antenna beam utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran Freq. dependent ant. beam utility mod.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2006-2010, 2015-2016
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
C   This module provides utilities for imaging related functions which
C   include the effects of frequency dependant primary antenna patterns.
C   The model of the antenna pattern used is that it is a uniformly
C   illuminated disk.   The parameters required are:
C      DOPBFM     L    If true then do modeling with frequency dependent
C                       primary beam corrections.
C      PBFREQ     D(*) Frequencies (Hz) going into the average.
C      PBFSIZ     R    Antenna diameter (m)
C      PBOMITCC   I    Omit some of CCs depending on position in beam
C   These parameters are passed attached to the input uv data in each
C   case.
C      For uv model computations a CC table is generated for each
C   frequency channel in which the fluxes, assumed an average of all
C   channels, are corrected to that channel.  The model computation is
C   done for each channel independently.
C      For imaging operations, each channel is imaged seperately,
C   corrected and then averaged.
C
C   Public functions:
C   PBFSUB (uvin, uvout, ifield, mfield, cname, chanl, nchan, ierr)
C      Subtract the FT of a model from a uv data set.
C   PBFDIV (uvin, uvout, mfield, cname, chanl, nchan, ierr)
C      Divide the FT of a model into a uv data set.
C
C   Private functions:
C   PBFCCT (ifield, nfield, image, iccver, ifreq, nfreq, pbfinc, mfreq,
C      pbfreq, sifreq, pbfsiz, pbparm, spix, iclng, isubg, occver,
C      outcc, ierr)
C      Make temporary, frequency scaled CC table.
C   PBFSCI (ifield, nfield, image, ichan, ifreq, nfreq, pbfinc, mfreq,
C      pbfreq, sifreq,  pbfsiz, pbparm, spix, tmpimg, ierr)
C      Make temporary, frequency scaled image.
C   PBFACT (nfreq, pbfreq, pbfsiz, ifreq, pbfinc, mfreq, radius, sindex,
C      sifreq, mfreq, pbparm)
C      Function which returns relative primary beam correction.
C   SPVALU (spix, image, x, y, sindex, sifreq)
C      return spectral index from spix image at x,y of image
C-----------------------------------------------------------------------
LOCAL INCLUDE 'SPIXCC.INC'
      INTEGER   MAXSPC
      PARAMETER (MAXSPC = 1000000)
      REAL      SIVALS(2,MAXSPC)
      INTEGER   SIFLD, SIMAX
      COMMON /SPIXCC/ SIVALS, SIMAX, SIFLD
LOCAL END
      SUBROUTINE PBFSUB (UVIN, UVOUT, IFIELD, NFIELD, IMAGE, CHANL,
     *   NCHAN, IERR)
C-----------------------------------------------------------------------
C   Frequency dependent primary beam utility
C   Subtract the Fourier transform of an image model from a uv data set.
C   Can use either a set of clean components or an image.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      IFIELD  I    Field to do now (0 -> all)
C      NFIELD  I    Number of fields
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      DOPBFM     L    If true then do modeling with frequency dependent
C                       primary beam corrections.
C      PBFREQ     D(*) Frequencies (Hz) going into the average.
C      PBFSIZ     R    Antenna diameter (m)
C      PBOMITCC   I    Omit CCs based on position in beam
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R     Model factor (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MSGSAV, TYPE, DIM(7), ICCVER(MAXFLD), ISUBG(MAXFLD),
     *   ICHAN, CCVER(MAXFLD), NFREQ, IFREQ, NCCB, NCCV, DUMMY, LFIELD,
     *   LF1, LF2, PBFINC, I, PBOMIT, NGROUP, MGROUP, IGROUP, NFACET,
     *   NW, LFN, IFN, NGR, IGR, LLREC, ICLNG(MAXFLD), IDUM
      LOGICAL   IDOPT, DOPBFM, OUTSID, GETNCC, MFREQ
      REAL      IPTFLX, PTFLX, PTRAOF, PTDCOF, PBFSIZ, PBFACT, RADIUS,
     *   SINDEX(2), PBPARM(6), BMFACT
      DOUBLE PRECISION PBFREQ(MAXCIF), FQTOL, LAMBDA, SIFREQ, MODFRQ,
     *   CRVAL(7)
      CHARACTER ICMOD*4, ARRAY*8, TEMP(MAXFLD)*32, TIMAGE(MAXFLD)*32,
     *   UVTMP*32, CDUMMY*1, SPIX(2)*32, CMET*4, TMPTAB*32, UMET*4,
     *   CTYPE(7)*8
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMOD.INC'
      INTEGER   IFRGRP(MAXMOD), PBFGRP(MAXMOD), FACETS(MAXMOD)
      CHARACTER MODCHR(MAXMOD)*32
      SAVE SIFREQ, PBFREQ, MODFRQ, TIMAGE, MFREQ
      DATA SIFREQ /1.D9/
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Get control info.
C                                       Frequency scaling parameters (no
C                                       defaults)
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'DOPBFM', TYPE, DIM, DOPBFM, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Check for SPIX corr
      SPIX(2) = ' '
      CALL OUVGET (UVIN, 'SPIXIMAGE', TYPE, DIM, DUMMY, SPIX(1), IERR)
      IF (IERR.EQ.1) THEN
         SPIX(1) = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (SPIX(1).NE.' ') THEN
         CALL OUVGET (UVIN, 'SPIXCURV', TYPE, DIM, DUMMY, SPIX(2), IERR)
         IF (IERR.EQ.1) THEN
            SPIX(2) = ' '
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       frequencies - need for both
      CALL OUVGET (UVIN, 'PBFREQ', TYPE, DIM, PBFREQ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         SPIX(1) = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      NFREQ = DIM(1) - CHANL + 1
      CALL OUVGET (UVIN, 'FQTOL', TYPE, DIM, SINDEX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         SINDEX(1) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      FQTOL = MAX (0.1, SINDEX(1)) * 1000.0
      CALL OUVGET (UVIN, 'PBFSIZ', TYPE, DIM, PBFSIZ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PBOMITCC', TYPE, DIM, PBOMIT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         PBOMIT = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOPBFM) PBOMIT = 0
C                                       primary beam parameters
      ARRAY = ' '
      IF (DOPBFM) THEN
         CALL OUVGET (UVIN, 'TELESCOPE', TYPE, DIM, DUMMY, ARRAY, IERR)
         IF (IERR.EQ.1) THEN
            ARRAY = ' '
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
      IF ((ARRAY.EQ.'VLA') .OR. (ARRAY.EQ.'ATCA') .OR.
     *      (ARRAY.EQ.'EVLA')) THEN
         I = CHANL + NCHAN/2
         LAMBDA = VELITE / PBFREQ(I)
         PBPARM(1) = 1.0
         CALL PBCALC (0.0D0, LAMBDA, ARRAY, PBPARM, BMFACT, OUTSID)
      ELSE
         PBPARM(1) = -1.0
         END IF
C
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = NFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, ICCVER, CDUMMY, IERR)
      NCCV = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCV = MAXFLD
         IERR = 0
         CALL FILL (MAXFLD, 1, ICCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, ISUBG, CDUMMY, IERR)
      NCCB = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCB = MAXFLD
         IERR = 0
         CALL FILL (NCCB, 1, ISUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, ICLNG, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CALL FILL (NCCB, 0, ICLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model type
      CALL OUVGET (UVIN, 'MODMODEL', TYPE, DIM, DUMMY, ICMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         ICMOD = 'CC  '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model method
      CALL OUVGET (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMET = 'DFT '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LLREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       model one frequency
      CALL OUVGET (UVIN, 'MODSFREQ', TYPE, DIM, MFREQ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MFREQ = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      MODFRQ = 0.0D0
      IF (MFREQ) THEN
         CALL IMGOPN (IMAGE(1), 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
C                                       get ref freq
         CALL IMDGET (IMAGE(1), 'CTYPE', TYPE, DIM, IDUM, CTYPE, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMDGET (IMAGE(1), 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         DO 5 I = 1,7
            IF (CTYPE(I).EQ.'FREQ') THEN
               MODFRQ = CRVAL(I)
               END IF
 5          CONTINUE
         IF (MODFRQ.LE.0.0D0) THEN
            MSGTXT = 'FREQUENCY NOT FOUND IN MODEL IMAGE'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 995
            END IF
         CALL OCLOSE (IMAGE(1), IERR)
         END IF
C                                       Point model
      CALL OUVGET (UVIN, 'MODDOPT ', TYPE, DIM, IDOPT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         IDOPT = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (IDOPT) THEN
C                                       Flux
         CALL OUVGET (UVIN, 'MODPTFLX ', TYPE, DIM, IPTFLX, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            IPTFLX = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X offset
         CALL OUVGET (UVIN, 'MODPTXOF ', TYPE, DIM, PTRAOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTRAOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y offset
         CALL OUVGET (UVIN, 'MODPTYOF ', TYPE, DIM, PTDCOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTDCOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
      MSGSUP = MSGSAV
      SINDEX(1) = 0.0
      SINDEX(2) = 0.0
      IF (.NOT.DOPBFM) PBFSIZ = 1.0E-5
C                                       Save image names
      GETNCC = .TRUE.
      DO 10 LFIELD = 1,NFIELD
         TIMAGE(LFIELD) = IMAGE(LFIELD)
         IF (ICLNG(LFIELD).GT.0) GETNCC = .FALSE.
 10      CONTINUE
C                                       How many channels do we do?
      ICHAN = CHANL
      PBFINC = 0
      MGROUP = 0
 15   ICHAN = ICHAN + PBFINC
      IF (ICHAN.LT.CHANL+NCHAN) THEN
         MGROUP = MGROUP + 1
         DO 20 I = ICHAN,CHANL+NCHAN-1
            IF (ABS(PBFREQ(ICHAN)-PBFREQ(I)).GT.FQTOL) GO TO 25
 20         CONTINUE
         I = CHANL + NCHAN
 25      PBFINC = I - ICHAN
         PBFINC = MAX (1, PBFINC)
         PBFGRP(MGROUP) = PBFINC
         IFRGRP(MGROUP) = ICHAN - CHANL + 1
         GO TO 15
         END IF
C                                       get needed comps
      GETNCC = GETNCC .AND. (ICMOD.EQ.'CC')
      IF (GETNCC) THEN
         TMPTAB = 'Temporary table for PBFSUB'
         DO 40 LFIELD = 1,NFIELD
            CALL IM2TAB (IMAGE(LFIELD), TMPTAB, 'CC', ICCVER(LFIELD),
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABOPN (TMPTAB, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, ICLNG(LFIELD),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABCLO (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABDES (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
 40         CONTINUE
         END IF
C                                       Gridded CC - what to aim for?
C                                       can we combine?
      UVTMP = UVIN
      NGROUP = 1
      IF ((.NOT.IDOPT) .AND. (ICMOD.EQ.'CC')) THEN
         CALL MODFIT (LF1, LF2, MGROUP, ISUBG, ICLNG, LLREC, CMET, UMET,
     *      FACETS, NFACET, NGROUP, NW)
         IF (NFACET*NGROUP.GT.MAXMOD) NGROUP = 1
         END IF
C                                       new mode selected:
      IF (NGROUP.GT.1) THEN
         IF (CMET.NE.UMET) THEN
            TYPE = OOACAR
            DIM(1) = 4
            DIM(2) = 1
            CALL OUVPUT (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, UMET, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       loop in sections
         IGROUP = 1
 60      IF (LF1.LE.LF2) THEN
            CALL MODFIT (LF1, LF2, MGROUP, ISUBG, ICLNG, LLREC, UMET,
     *         UMET, FACETS, NFACET, NGROUP, NW)
 70         NGR = NGROUP
            IF (IGROUP+NGR-1.GT.MGROUP) NGR = MGROUP - IGROUP + 1
            DO 80 LFN = 1,NFACET
               IFN = FACETS(LFN)
C                                       must prepare with freq as
C                                       inner loop, want to use as
C                                       freq outer loop
               DO 75 IGR = 1,NGR
                  MODMAX = (IGR - 1) * NFACET + LFN
                  IFREQ = IFRGRP(IGROUP+IGR-1)
                  ICHAN = IFREQ + CHANL - 1
                  PBFINC = PBFGRP(IGROUP+IGR-1)
C                                       Create temp tables
                  CALL PBFCCT (IFN, NFIELD, IMAGE, ICCVER, IFREQ, NFREQ,
     *               PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ,
     *               PBFSIZ, PBPARM, PBOMIT, SPIX, ICLNG, ISUBG, CCVER,
     *               TEMP, IERR)
                  IF (IERR.NE.0) GO TO 995
                  MODFLD(MODMAX) = IFN
                  MODCHN(MODMAX) = ICHAN
                  MODNCH(MODMAX) = PBFINC
                  MODCCV(MODMAX) = CCVER(IFN)
                  MODCCB(MODMAX) = ISUBG(IFN)
                  MODCHR(MODMAX) = TEMP(IFN)
 75               CONTINUE
 80            CONTINUE
            MODMAX = NGR * NFACET
C                                       Reset start no. component.
            DIM(1) = NCCB
            DIM(2) = 1
            CALL OUVPUT (UVTMP, 'MODCCBEG', OOAINT, DIM, ISUBG,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OUMSUB (UVTMP, UVOUT, IFIELD, NFIELD, TIMAGE, ICHAN,
     *         PBFINC, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Input for following channels =
C                                       output.
            UVTMP = UVOUT
C                                       Delete temporaries
            DO 90 LFN = 1,MODMAX
               CALL ZAP (MODCHR(MODMAX+1-LFN), IERR)
               IF (IERR.NE.0) GO TO 990
 90            CONTINUE
            IGROUP = IGROUP + NGR
            IF (IGROUP.LE.MGROUP) GO TO 70
            IGROUP = 1
            LF1 = MODFLD(MODMAX) + 1
            GO TO 60
            END IF
         MODMAX = 0
C                                       Loop over 1 channel at a time
      ELSE
         UMET = CMET
         IGROUP = 0
 110     IGROUP = IGROUP + 1
         IF (IGROUP.LE.MGROUP) THEN
C                                       Frequency number
            IFREQ = IFRGRP(IGROUP)
            PBFINC = PBFGRP(IGROUP)
            ICHAN = IFREQ + CHANL - 1
C                                       Prepare model
            IF (IDOPT) THEN
C                                       Point
               RADIUS = SQRT (PTRAOF*PTRAOF + PTDCOF*PTDCOF) / 3600.0
               PTFLX = IPTFLX
               IF (DOPBFM) PTFLX = IPTFLX * PBFACT (NFREQ,
     *            PBFREQ(CHANL), PBFSIZ, IFREQ, PBFINC, MFREQ, RADIUS,
     *            SINDEX, SIFREQ, MODFRQ, PBPARM, 0)
               DIM(1) = 1
               DIM(2) = 1
               CALL OUVPUT (UVTMP, 'MODPTFLX ', OOARE, DIM, PTFLX,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
C                                       CC model
            ELSE IF (ICMOD.EQ.'CC') THEN
C                                       Create temp tables
               CALL PBFCCT (IFIELD, NFIELD, IMAGE, ICCVER, IFREQ, NFREQ,
     *            PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ, PBFSIZ,
     *            PBPARM, PBOMIT, SPIX, ICLNG, ISUBG, CCVER, TEMP, IERR)
               IF (IERR.NE.0) GO TO 995
C                                       CC version
               DIM(1) = NCCV
               DIM(2) = 1
               CALL OUVPUT (UVTMP, 'MODCCVER', OOAINT, DIM, CCVER,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Reset start no. component.
               DIM(1) = NCCB
               CALL OUVPUT (UVTMP, 'MODCCBEG', OOAINT, DIM, ISUBG,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Scale images to TEMP
            ELSE
               CALL PBFSCI (IFIELD, NFIELD, IMAGE, ICHAN, IFREQ, NFREQ,
     *            PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ, PBFSIZ,
     *            PBPARM, PBOMIT, SPIX, TEMP, IERR)
               DO 180 LFIELD = LF1,LF2
                  TIMAGE(LFIELD) = TEMP(LFIELD)
 180              CONTINUE
               END IF
            CALL OUMSUB (UVTMP, UVOUT, IFIELD, NFIELD, TIMAGE, ICHAN,
     *         PBFINC, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Input for following channels =
C                                       output.
            UVTMP = UVOUT
C                                       Delete temporaries
            IF (.NOT.IDOPT) THEN
               DO 190 LFIELD = LF1,LF2
                  CALL ZAP (TEMP(LF1+LF2-LFIELD), IERR)
                  IF (IERR.NE.0) GO TO 990
 190              CONTINUE
               END IF
            GO TO 110
            END IF
         END IF
C                                       Restore inputs modified.
      MSGSUP = MSGSAV
C                                       CC version
      DIM(1) = NCCV
      DIM(2) = 1
      CALL OUVPUT (UVIN, 'MODCCVER', OOAINT, DIM, ICCVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Flux
      IF (IDOPT) THEN
         DIM(1) = 1
         CALL OUVPUT (UVIN, 'MODPTFLX ', OOARE, DIM, IPTFLX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       restore original method
      IF (CMET.NE.UMET) THEN
         TYPE = OOACAR
         DIM(1) = 4
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Done
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      MSGTXT = 'PBFSUB: ERROR SUBTRACTING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'PBFSUB: FROM ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PBFDIV (UVIN, UVOUT, NFIELD, IMAGE, CHANL, NCHAN, IERR)
C-----------------------------------------------------------------------
C   Frequency dependent primary beam utility
C   Divide the Fourier transform of an image model into a uv data set.
C   Can use either a set of clean components or an image.
C   Inputs:
C      UVIN    C*?  Name of input uvdata object.
C      UVOUT   C*?  Name of output uvdata object, will be created if
C                   necessary as a scratch file.
C      NFIELD  I    Number of fields
C      IMAGE   C(*)*? array of image names to subtract.
C      CHANL   I    First channel in uv data to process
C      NCHAN   I    Number of uv channels to process
C   Inputs attached to UVIN (defaulted if not present).
C      DOPBFM     L    If true then do modeling with frequency dependent
C                       primary beam corrections.
C      PBFREQ     D(*) Frequencies (Hz) going into the average.
C      PBFSIZ     R    Antenna diameter (m)
C      PBOMITCC   I    Omit CCs based on position in beam
C      MODCCVER  I(*)  CC version number for each image (1, i.e.
C                      must specify for line data.)
C      MODCCBEG  I(*)  First component per field (1)
C      MODCCEND  I(*)  Highest component per field (highest)
C      MODNONEG  L     If true stop at first component (.false.)
C      MODFLUX   R     Lowest abs(CC flux) to include
C      MODMODEL  C*4   Model type to use, 'CC  ', 'IMAG' ('CC')
C      MODMETH   C*4   Model method 'GRID', 'DFT ', '    '=> chose (' ')
C      MODFACT   R     Model factor (default = 1.0)
C      MODDOMSG  L     If true give progress reports (.false.)
C      MODDOPT   L     If true use point model (.false.)*
C      MODPTFLX  R     Point model flux in Jy, (0.0)
C      MODPTXOF  R     Point model "x" offset in arcsec (0.0)
C      MODPTYOF  R     Point model "y" offset in arcsec (0.0)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   NFIELD, CHANL, NCHAN, IERR
      CHARACTER UVIN*(*), UVOUT*(*), IMAGE(NFIELD)*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MSGSAV, TYPE, DIM(7), ICCVER(MAXFLD), ISUBG(MAXFLD),
     *   ICHAN, I, CCVERS(MAXFLD), NFREQ, IFREQ, NCCB, NCCV, DUMMY,
     *   PBFINC, PBOMIT, NGROUP, MGROUP, IGROUP, NFACET, NW, LFN, IFN,
     *   NGR, IGR, LLREC, LF1, LF2, ICLNG(MAXFLD), LFIELD, IDUM
      LOGICAL   IDOPT, DOPBFM, OUTSID, GETNCC, MFREQ
      REAL      IPTFLX, PTFLX, PTRAOF, PTDCOF, PBFSIZ, PBFACT, RADIUS,
     *   SINDEX(2), PBPARM(6), BMFACT
      DOUBLE PRECISION PBFREQ(MAXCIF), FQTOL, LAMBDA, SIFREQ, MODFRQ,
     *   CRVAL(7)
      CHARACTER ICMOD*4, ARRAY*8, TEMP(MAXFLD)*32, TIMAGE(MAXFLD)*32,
     *   UVTMP*32, CDUMMY*1, SPIX(2)*32, CMET*4, TMPTAB*32, UMET*4,
     *   CTYPE(7)*8
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMOD.INC'
      INTEGER   IFRGRP(MAXMOD), PBFGRP(MAXMOD), FACETS(MAXMOD)
      CHARACTER MODCHR(MAXMOD)*32
      SAVE SIFREQ, PBFREQ, MODFRQ, TIMAGE, MFREQ
      DATA SIFREQ /1.D9/
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
      LF1 = 1
      LF2 = NFIELD
C                                       Get control info.
C                                       Frequency scaling parameters (no
C                                       defaults)
      MSGSUP = 32000
      CALL OUVGET (UVIN, 'DOPBFM', TYPE, DIM, DOPBFM, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Check for SPIX corr
      SPIX(2) = ' '
      CALL OUVGET (UVIN, 'SPIXIMAGE', TYPE, DIM, DUMMY, SPIX(1), IERR)
      IF (IERR.EQ.1) THEN
         SPIX(1) = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (SPIX(1).NE.' ') THEN
         CALL OUVGET (UVIN, 'SPIXCURV', TYPE, DIM, DUMMY, SPIX(2), IERR)
         IF (IERR.EQ.1) THEN
            SPIX(2) = ' '
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       freqs needed for both
      CALL OUVGET (UVIN, 'PBFREQ', TYPE, DIM, PBFREQ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         SPIX(1) = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      NFREQ = DIM(1) - CHANL + 1
      CALL OUVGET (UVIN, 'FQTOL', TYPE, DIM, SINDEX, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         SINDEX(1) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      FQTOL = MAX (0.1, SINDEX(1)) * 1000.0
      CALL OUVGET (UVIN, 'PBFSIZ', TYPE, DIM, PBFSIZ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         DOPBFM = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OUVGET (UVIN, 'PBOMITCC', TYPE, DIM, PBOMIT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         PBOMIT = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      IF (.NOT.DOPBFM) PBOMIT = 0
C                                       primary beam parameters
      ARRAY = ' '
      IF (DOPBFM) THEN
         CALL OUVGET (UVIN, 'TELESCOPE', TYPE, DIM, DUMMY, ARRAY, IERR)
         IF (IERR.EQ.1) THEN
            ARRAY = ' '
            IERR = 0
            END IF
         IF (IERR.NE.0) GO TO 990
         END IF
      IF ((ARRAY.EQ.'VLA') .OR. (ARRAY.EQ.'ATCA') .OR.
     *   (ARRAY.EQ.'EVLA')) THEN
         I = CHANL + NCHAN/2
         LAMBDA = VELITE / PBFREQ(I)
         PBPARM(1) = 1.0
         CALL PBCALC (0.0D0, LAMBDA, ARRAY, PBPARM, BMFACT, OUTSID)
      ELSE
         PBPARM(1) = -1.0
         END IF
C                                       CC version default = 1
      CALL OUVGET (UVIN, 'MODCCVER', TYPE, DIM, ICCVER, CDUMMY, IERR)
      NCCV = DIM(1)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NCCV = MAXFLD
         CALL FILL (MAXFLD, 1, ICCVER)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Start component
      CALL OUVGET (UVIN, 'MODCCBEG', TYPE, DIM, ISUBG, CDUMMY, IERR)
      NCCB = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCB = MAXFLD
         IERR = 0
         CALL FILL (NCCB, 1, ISUBG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       highest component
      CALL OUVGET (UVIN, 'MODCCEND', TYPE, DIM, ICLNG, CDUMMY, IERR)
      NCCB = DIM(1)
      IF (IERR.EQ.1) THEN
         NCCB = MAXFLD
         IERR = 0
         CALL FILL (NCCB, 0, ICLNG)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model type
      CALL OUVGET (UVIN, 'MODMODEL', TYPE, DIM, DUMMY, ICMOD, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         ICMOD = 'CC  '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Model method
      CALL OUVGET (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMET = 'DFT '
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       LREC
      CALL UVDGET (UVIN, 'LREC', TYPE, DIM, LLREC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       model one frequency
      CALL OUVGET (UVIN, 'MODSFREQ', TYPE, DIM, MFREQ, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         MFREQ = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      MODFRQ = 0.0D0
      IF (MFREQ) THEN
         CALL IMGOPN (IMAGE(1), 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
C                                       get ref freq
         CALL IMDGET (IMAGE(1), 'CTYPE', TYPE, DIM, IDUM, CTYPE, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMDGET (IMAGE(1), 'CRVAL', TYPE, DIM, CRVAL, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         DO 5 I = 1,7
            IF (CTYPE(I).EQ.'FREQ') THEN
               MODFRQ = CRVAL(I)
               END IF
5           CONTINUE
         IF (MODFRQ.LE.0.0D0) THEN
            MSGTXT = 'FREQUENCY NOT FOUND IN MODEL IMAGE'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 995
            END IF
         CALL OCLOSE (IMAGE(1), IERR)
         END IF
C                                       Point model
      CALL OUVGET (UVIN, 'MODDOPT ', TYPE, DIM, IDOPT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         IDOPT = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (IDOPT) THEN
C                                       Flux
         CALL OUVGET (UVIN, 'MODPTFLX ', TYPE, DIM, IPTFLX, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            IPTFLX = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       X offset
         CALL OUVGET (UVIN, 'MODPTXOF ', TYPE, DIM, PTRAOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTRAOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Y offset
         CALL OUVGET (UVIN, 'MODPTYOF ', TYPE, DIM, PTDCOF, CDUMMY,
     *      IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            PTDCOF = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
      MSGSUP = MSGSAV
      SINDEX(1) = 0.0
      SINDEX(2) = 0.0
      IF (.NOT.DOPBFM) PBFSIZ = 1.E-5
C                                       Save image names
      GETNCC = .TRUE.
      DO 10 I = 1,NFIELD
         TIMAGE(I) = IMAGE(I)
         IF (ICLNG(I).GT.0) GETNCC = .FALSE.
 10      CONTINUE
C                                       How many channels do we do?
      ICHAN = CHANL
      PBFINC = 0
      MGROUP = 0
 15   ICHAN = ICHAN + PBFINC
      IF (ICHAN.LT.CHANL+NCHAN) THEN
         MGROUP = MGROUP + 1
         DO 20 I = ICHAN,CHANL+NCHAN-1
            IF (ABS(PBFREQ(ICHAN)-PBFREQ(I)).GT.FQTOL) GO TO 25
 20         CONTINUE
         I = CHANL + NCHAN
 25      PBFINC = I - ICHAN
         PBFINC = MAX (1, PBFINC)
         PBFGRP(MGROUP) = PBFINC
         IFRGRP(MGROUP) = ICHAN - CHANL + 1
         GO TO 15
         END IF
C                                       get needed comps
      GETNCC = GETNCC .AND. (ICMOD.EQ.'CC')
      IF (GETNCC) THEN
         TMPTAB = 'Temporary table for PBFSUB'
         DO 40 LFIELD = 1,NFIELD
            CALL IM2TAB (IMAGE(LFIELD), TMPTAB, 'CC', ICCVER(LFIELD),
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABOPN (TMPTAB, 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABGET (TMPTAB, 'NROW', TYPE, DIM, ICLNG(LFIELD),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABCLO (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL TABDES (TMPTAB, IERR)
            IF (IERR.NE.0) GO TO 990
 40         CONTINUE
         END IF
C                                       Gridded CC - what to aim for?
C                                       can we combine?
C                                       Must do all fields even if
C                                       only one channel
      UVTMP = UVIN
      NGROUP = 1
      IF ((.NOT.IDOPT) .AND. (ICMOD.EQ.'CC')) THEN
         CALL MODFIT (LF1, LF2, 1, ISUBG, ICLNG, LLREC, CMET, UMET,
     *      FACETS, NFACET, NGROUP, NW)
         IF (NFACET.EQ.NFIELD) THEN
            CALL MODFIT (LF1, LF2, MGROUP, ISUBG, ICLNG, LLREC, CMET,
     *         UMET, FACETS, NFACET, NGROUP, NW)
            IF (NFACET.LT.NFIELD) NGROUP = 1
            IF (NFACET*NGROUP.GT.MAXMOD) NGROUP = 1
            END IF
         END IF
C                                       new mode selected:
C                                       force gridded for now
      IF (NGROUP.GT.1) THEN
         IF (CMET.NE.UMET) THEN
            TYPE = OOACAR
            DIM(1) = 4
            DIM(2) = 1
            CALL OUVPUT (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, UMET, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       loop in sections
         IGROUP = 1
         CALL MODFIT (LF1, LF2, MGROUP, ISUBG, ICLNG, LLREC, UMET, UMET,
     *      FACETS, NFACET, NGROUP, NW)
 70      NGR = NGROUP
         IF (IGROUP+NGR-1.GT.MGROUP) NGR = MGROUP - IGROUP + 1
         DO 80 LFN = 1,NFACET
            IFN = FACETS(LFN)
            DO 75 IGR = 1,NGR
               MODMAX = (IGR - 1) * NFACET + LFN
               IFREQ = IFRGRP(IGROUP+IGR-1)
               ICHAN = IFREQ + CHANL - 1
               PBFINC = PBFGRP(IGROUP+IGR-1)
C                                       Create temp tables
               CALL PBFCCT (IFN, NFIELD, IMAGE, ICCVER, IFREQ, NFREQ,
     *            PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ, PBFSIZ,
     *            PBPARM, PBOMIT, SPIX, ICLNG, ISUBG, CCVERS, TEMP,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               MODFLD(MODMAX) = IFN
               MODCHN(MODMAX) = ICHAN
               MODNCH(MODMAX) = PBFINC
               MODCCV(MODMAX) = CCVERS(IFN)
               MODCCB(MODMAX) = ISUBG(IFN)
               MODCHR(MODMAX) = TEMP(IFN)
 75            CONTINUE
 80         CONTINUE
C                                       Reset start no. component.
         DIM(1) = NCCB
         DIM(2) = 1
         CALL OUVPUT (UVTMP, 'MODCCBEG', OOAINT, DIM, ISUBG, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OUMDIV (UVTMP, UVOUT, NFIELD, TIMAGE, ICHAN, PBFINC, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Input for following channels =
C                                       output.
         UVTMP = UVOUT
C                                       Delete temporaries
         DO 90 LFN = 1,MODMAX
            CALL ZAP (MODCHR(MODMAX+1-LFN), IERR)
            IF (IERR.NE.0) GO TO 990
 90         CONTINUE
         IGROUP = IGROUP + NGR
         IF (IGROUP.LE.MGROUP) GO TO 70
         MODMAX = 0
C                                       Loop over 1 channel at a time
      ELSE
         IGROUP = 0
 110     IGROUP = IGROUP + 1
         IF (IGROUP.LE.MGROUP) THEN
C                                       Frequency number
            IFREQ = IFRGRP(IGROUP)
            PBFINC = PBFGRP(IGROUP)
            ICHAN = IFREQ + CHANL - 1
C                                       Prepare model
            IF (IDOPT) THEN
C                                       Point
               RADIUS = SQRT (PTRAOF*PTRAOF + PTDCOF*PTDCOF) / 3600.0
               PTFLX = IPTFLX
               IF (DOPBFM) PTFLX = IPTFLX * PBFACT (NFREQ,
     *            PBFREQ(CHANL), PBFSIZ, IFREQ, PBFINC, MFREQ, RADIUS,
     *            SINDEX, SIFREQ, MODFRQ, PBPARM, 0)
               DIM(1) = 1
               DIM(2) = 1
               CALL OUVPUT (UVTMP, 'MODPTFLX ', OOARE, DIM, PTFLX,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
C                                       CC model
            ELSE IF (ICMOD.EQ.'CC') THEN
C                                       Create temp tables
               CALL PBFCCT (0, NFIELD, IMAGE, ICCVER, IFREQ, NFREQ,
     *            PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ, PBFSIZ,
     *            PBPARM, PBOMIT, SPIX, ICLNG, ISUBG, CCVERS, TEMP,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
C                                       CC version
               DIM(1) = NCCV
               DIM(2) = 1
               CALL OUVPUT (UVTMP, 'MODCCVER', OOAINT, DIM, CCVERS,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Reset start no. component.
               DIM(1) = NCCB
               CALL OUVPUT (UVTMP, 'MODCCBEG', OOAINT, DIM, ISUBG,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Scale images to TEMP
            ELSE
               CALL PBFSCI (0, NFIELD, IMAGE, ICHAN, IFREQ, NFREQ,
     *            PBFINC, MFREQ, PBFREQ(CHANL), SIFREQ, MODFRQ, PBFSIZ,
     *            PBPARM, PBOMIT, SPIX, TEMP, IERR)
               DO 180 I = 1,NFIELD
                  TIMAGE(I) = TEMP(I)
 180              CONTINUE
               END IF
            CALL OUMDIV (UVTMP, UVOUT, NFIELD, TIMAGE, ICHAN, PBFINC,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Input for following channels =
C                                       output.
            UVTMP = UVOUT
C                                       Delete temporaries
            IF (.NOT.IDOPT) THEN
               DO 190 I = 1,NFIELD
                  CALL ZAP (TEMP(1+NFIELD-I), IERR)
                  IF (IERR.NE.0) GO TO 990
 190              CONTINUE
               END IF
            GO TO 110
            END IF
         END IF
C                                       Restore inputs modified.
      MSGSUP = MSGSAV
C                                       CC version
      DIM(1) = NCCV
      DIM(2) = 1
      CALL OUVPUT (UVIN, 'MODCCVER', OOAINT, DIM, ICCVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Flux
      IF (IDOPT) THEN
         DIM(1) = 1
         CALL OUVPUT (UVIN, 'MODPTFLX ', OOARE, DIM, IPTFLX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       restore original method
      IF (CMET.NE.UMET) THEN
         TYPE = OOACAR
         DIM(1) = 4
         DIM(2) = 1
         CALL OUVPUT (UVIN, 'MODMETH ', TYPE, DIM, DUMMY, CMET, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Done
      MSGSUP = MSGSAV
      GO TO 999
C                                       Error
C                                       Give suppressed error
 995  MSGSUP = MSGSAV
      CALL MSGWRT (8)
 990  MSGSUP = MSGSAV
      MSGTXT = 'PBFDIV: ERROR DIVIDING ' // IMAGE(1)
      CALL MSGWRT (8)
      MSGTXT = 'PBFDIV: INTO ' // UVIN
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE PBFCCT (IFIELD, NFIELD, IMAGE, ICCVER, IFREQ, NFREQ,
     *   PBFINC, MFREQ, PBFREQ, SIFREQ, MODFRQ, PBFSIZ, PBPARM, PBOMIT,
     *   SPIX, ICLNG, ISUBG, OCCVER, OUTCC, IERR)
C-----------------------------------------------------------------------
C   Private to frequency dependent primary beam utility module.
C   Makes temporary, frequency scaled CC table for a given channel.
C   Inputs:
C      IFIELD    I         Field to do now (o all)
C      NFIELD    I         Number of fields (elements in IMAGE)
C      IMAGE     C(*)*32   Names of field images
C      ICCVER    I(*)      Input CC table numbers
C      IFREQ     I         Channel number
C      NFREQ     I         Number of frequencies in PBFREQ
C      PBFINC    I         Number frequencies to do this time
C      MFREQ     L         T -. use MODFRQ for model, else use all
C                          channels as source of model
C      MODFRQ    D         Frequency of model image/CC
C      PBFREQ    D(*)      Frequencies (Hz) going into the average.
C      PBFSIZ    R         Antenna diameter (m) large if not dopbfm
C      PBPARM    R(6)      Primary beam parms (1) > 0 => use (2-6)
C      PBOMIT    I         Omit (1) all in, (2) some in, (3) some out
C                             (4) all out - else omit nothing
C      SPIX      C(2)*32   Image of spectral index
C      ICLNG     I(*)      Max CC to process (field)
C      ISUBG     i(*)      First CC to process (field)
C   In/Out:
C      SIFREQ    D         Spectral index reference frequency
C   Outputs:
C      OCCVER    I(*)      Output temporary CC table numbers
C      OUTCC     C(*)*32   Output CC table objects.
C      IERR      I         Return code, 0=> OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, ICCVER(*), IFREQ, NFREQ, PBFINC,
     *   PBOMIT, ICLNG(*), ISUBG(*), OCCVER(*), IERR
      LOGICAL   MFREQ
      CHARACTER IMAGE(*)*32, OUTCC(*)*32, SPIX(2)*32
      REAL      PBFSIZ, PBPARM(6)
      DOUBLE PRECISION PBFREQ(NFREQ), SIFREQ, MODFRQ
C
      INTEGER   LFIELD, NROW, I, HIGHCC, ROW, IROW, OROW, TYPE, NCOL,
     *   DIM(7), LF1, LF2
      REAL      X, Y, FLUX, PARMS(20), RADIUS, PBFACT, SINDEX(2),
     *   CRPIX(7), CDELT(7), XX, YY
      CHARACTER INPCC*32, CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SPIXCC.INC'
C-----------------------------------------------------------------------
      IF ((PBFSIZ.GE.0.01) .AND. (SPIX(1).NE.' ')) THEN
         MSGTXT = 'PBFCCT correct CC for beam & spec index'
      ELSE IF (SPIX(1).NE.' ') THEN
         MSGTXT = 'PBFCCT correcting CC for spectral index'
      ELSE IF (PBFSIZ.GE.0.01) THEN
         MSGTXT = 'PBFCCT correcting CC for primary beam'
      ELSE
         MSGTXT = 'PBFCCT correcting CC for nothing at all'
         END IF
      IERR = 0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = NFIELD
         WRITE (MSGTXT(41:),1000) IFREQ, IFREQ+PBFINC-1
         IF (NFIELD.GT.1) THEN
            SIFLD = -1
            SIMAX = -1
         ELSE
            IF (SIFLD.NE.1) SIMAX = 0
            SIFLD = 1
            END IF
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         WRITE (MSGTXT(41:),1001) LF1, IFREQ, IFREQ+PBFINC-1
         IF (IFIELD.NE.SIFLD) SIMAX = 0
         SIFLD = IFIELD
         END IF
      CALL MSGWRT (2)
      SINDEX(1) = 0.0
      SINDEX(2) = 0.0
C                                       Loop over field
      DO 600 LFIELD = LF1,LF2
C                                       coordinates
         CALL IMDGET (IMAGE(LFIELD), 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDGET (IMAGE(LFIELD), 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Make temp, input CC
         INPCC = 'TEMP input CC for PBFCCT'
         CALL IM2TAB (IMAGE(LFIELD), INPCC, 'CC', ICCVER(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get highest CC number
         CALL TBLHIV (INPCC, HIGHCC, IERR)
         IF (IERR.NE.0) GO TO 990
         HIGHCC = HIGHCC + 1
C                                       Object name
         WRITE (OUTCC(LFIELD),1010) LFIELD, IFREQ
C                                       Make output
         CALL IM2TAB (IMAGE(LFIELD), OUTCC(LFIELD), 'CC', HIGHCC, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy headers
         CALL COPHED (INPCC, OUTCC(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Open inputs
         CALL OCCINI (INPCC, 'READ', IROW, NCOL, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OCCINI (OUTCC(LFIELD), 'WRIT', OROW, NCOL, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       How many?
         CALL TABGET (INPCC, 'NROW', TYPE, DIM, NROW, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy/correct table
         DO 500 I = 1,NROW
C                                       Read input
            ROW = I
            CALL OTABCC (INPCC, 'READ', ROW, NCOL, X, Y, FLUX, TYPE,
     *         PARMS, IERR)
            IF (IERR.NE.0) GO TO 990
            IF ((I.GE.ISUBG(LFIELD)) .AND. (I.LE.ICLNG(LFIELD))) THEN
               XX = X / CDELT(1) + CRPIX(1)
               YY = Y / CDELT(2) + CRPIX(2)
C                                       spectral index
               IF (SPIX(1).NE.' ') THEN
                  IF (I.LE.SIMAX) THEN
                     SINDEX(1) = SIVALS(1,I)
                     SINDEX(2) = SIVALS(2,I)
                  ELSE
                     CALL SPVALU (SPIX, IMAGE(LFIELD), XX, YY, SINDEX,
     *                  SIFREQ)
                     IF (SIMAX.GE.0) THEN
                        SIMAX = SIMAX + 1
                        SIVALS(1,SIMAX) = SINDEX(1)
                        SIVALS(2,SIMAX) = SINDEX(2)
                        SIMAX = MIN (SIMAX, MAXSPC-1)
                        END IF
                     END IF
                  END IF
C                                       Correct
               CALL PSNANG (IMAGE(LFIELD), X, Y, RADIUS, IERR)
               IF (IERR.EQ.0) FLUX = FLUX * PBFACT (NFREQ, PBFREQ,
     *            PBFSIZ, IFREQ, PBFINC, MFREQ, RADIUS, SINDEX, SIFREQ,
     *            MODFRQ, PBPARM, PBOMIT)
               END IF
C                                       Write
            ROW = I
            CALL OTABCC (OUTCC(LFIELD), 'WRIT', ROW, NCOL, X, Y, FLUX,
     *         TYPE, PARMS, IERR)
            IF (IERR.NE.0) GO TO 990
 500        CONTINUE
C                                       Get output table version
         CALL TABGET (OUTCC(LFIELD), 'VER', TYPE, DIM, OCCVER(LFIELD),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Close tables
         CALL OTABCC (INPCC, 'CLOS', ROW, NCOL, X, Y, FLUX, TYPE, PARMS,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL OTABCC (OUTCC(LFIELD), 'CLOS', ROW, NCOL, X, Y, FLUX,
     *      TYPE, PARMS, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Delete temp object
         CALL TABDES (INPCC, IERR)
         IF (IERR.NE.0) GO TO 990
 600     CONTINUE
      GO TO 999
C                                       Error
 990  MSGTXT = 'PBFCCT: ERROR CORRECTING CC TABLE'
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('All fields ch',I5,'-',I5)
 1001 FORMAT ('Field',I5,' ch',I5,'-',I5)
 1010 FORMAT ('Temp table',I5,I6,' for PBFCCT')
      END
      SUBROUTINE PBFSCI (IFIELD, NFIELD, IMAGE, ICHAN, IFREQ, NFREQ,
     *   PBFINC, MFREQ, PBFREQ, SIFREQ, MODFRQ, PBFSIZ, PBPARM, PBOMIT,
     *   SPIX, TMPIMG, IERR)
C-----------------------------------------------------------------------
C   Private to frequency dependent primary beam utility module.
C   Makes temporary, frequency scaled image for a given channel.
C   Copies control information needed for uv model computation.
C   Inputs:
C      IFIELD    I         Field to do now
C      NFIELD    I         Number of fields (elements in IMAGE)
C      IMAGE     C(*)*32   Names of field images
C      ICHAN     I         Channel number
C      IFREQ     I         PBFREQ frequency number
C      NFREQ     I         Number of frequencies in PBFREQ
C      PBFINC    I         Number frequencies to do this time
C      MFREQ     L         T -. use MODFRQ for model, else use all
C                          channels as source of model
C      MODFRQ    D         Frequency of model image/CC
C      PBFREQ    D(*)      Frequencies (Hz) going into the average.
C      PBFSIZ    R         Antenna diameter (m) (large if not dopbfm)
C      PBPARM    R(6)      Primary beam parms (1) > 0 => use (2-6)
C      PBOMIT    I         Omit (1) all in, (2) some in, (3) some out
C                             (4) all out - else omit nothing
C      SPIX      C(2)*32   Image of spectral index
C   In/Out:
C      SIFREQ    D         Spectral index reference frequency
C   Outputs:
C      TMPIMG    C(*)*32   Output Image objects.
C      IERR      I         Return code, 0=> OK else failed.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, ICHAN, IFREQ, NFREQ, PBFINC, PBOMIT,
     *   IERR
      LOGICAL   MFREQ
      CHARACTER IMAGE(*)*32, TMPIMG(*)*32, SPIX(2)*32
      REAL      PBFSIZ, PBPARM(6)
      DOUBLE PRECISION PBFREQ(NFREQ), SIFREQ, MODFRQ
C
      INTEGER   NKEY
C                                       NKEY = no. keywords to copy
      PARAMETER (NKEY = 4)
      INTEGER   LFIELD, TYPE, DIM(7), NAXIS(7), I, J, BLC(7), TRC(7),
     *   BUFNO, LF1, LF2
      REAL      RADIUS, PBFACT, X, Y, CRPIX(7), CDELT(7), SINDEX(2), XX,
     *   YY
      CHARACTER KEYS(NKEY)*8, BUFOBJ*32, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      DATA KEYS /'MODMODEL', 'MODMETH', 'MODFACT', 'MODDOMSG'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create temporary object to use
C                                       for a buffer
      BUFOBJ = 'Object for buffer for PBFSCI'
      CALL OBCREA (BUFOBJ, 'IMAGE', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Open for buffer
      CALL OBOPEN (BUFOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Which buffer?
      CALL OBINFO (BUFOBJ, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 990
      SINDEX(1) = 0.0
      SINDEX(2) = 0.0
C                                       Loop over field
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = NFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      DO 600 LFIELD = LF1,LF2
C                                       Object name
         WRITE (TMPIMG(LFIELD), 1000) LFIELD
C                                       Make output
         CALL ARDGET (IMAGE(LFIELD), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       1 channel
         NAXIS(3) = 1
         CALL IMGSCR (TMPIMG(LFIELD), NAXIS, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy control info
         CALL IN2OBJ (IMAGE(LFIELD), NKEY, KEYS, KEYS, TMPIMG(LFIELD),
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Set window in input (1 channel)
         CALL FILL (7, NAXIS, BLC)
         BLC(1) = 1
         BLC(2) = 1
         BLC(3) = ICHAN
         CALL FILL (7, NAXIS, TRC)
         TRC(3) = ICHAN
         DIM(1) = 7
         DIM(2) = 1
         CALL ARDPUT (IMAGE(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ARDPUT (IMAGE(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Assume pointing at ref. pixel.
         CALL IMDGET (IMAGE(LFIELD), 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDGET (IMAGE(LFIELD), 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Copy/correct image
         DO 500 J = 1,NAXIS(2)
            CALL ARREAD (IMAGE(LFIELD), DIM, OBUFFR(1,BUFNO), IERR)
            IF (IERR.NE.0) GO TO 990
            YY = J
            Y = (YY - CRPIX(2)) * CDELT(2)
            DO 400 I = 1,NAXIS(1)
               XX = I
               X = (XX - CRPIX(1)) * CDELT(1)
C                                       spectral index
               IF (SPIX(1).NE.' ') CALL SPVALU (SPIX, IMAGE(LFIELD), XX,
     *            YY, SINDEX, SIFREQ)
               CALL PSNANG (IMAGE(LFIELD), X, Y, RADIUS, IERR)
               OBUFFR(1,BUFNO) = OBUFFR(1,BUFNO) * PBFACT (NFREQ,
     *            PBFREQ, PBFSIZ, IFREQ, PBFINC, MFREQ, RADIUS, SINDEX,
     *            SIFREQ, MODFRQ, PBPARM, PBOMIT)
 400           CONTINUE
            CALL ARRWRI (TMPIMG(LFIELD), DIM, OBUFFR(1,BUFNO), IERR)
            IF (IERR.NE.0) GO TO 990
 500        CONTINUE
C                                       Close arrays
         CALL ARRCLO (IMAGE(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ARRCLO (TMPIMG(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 990
 600     CONTINUE
C                                       Close and destroy temp obj.
      CALL OBCLOS (BUFOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBFREE (BUFOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'PBFSCI: ERROR CORRECTING IMAGE' // IMAGE(1)
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Temp image',I5,' for PBFSCI')
      END
      REAL FUNCTION PBFACT (NFREQ, PBFREQ, PBFSIZ, IFREQ, PBFINC,
     *   MFREQ, RADIUS, SINDEX, SIFREQ, MODFRQ, PBPARM, PBOMIT)
C-----------------------------------------------------------------------
C   Private to frequency dependent primary beam utility module.
C   Calculates the relative gain (normalized to unity at the pointing
C   position) of the primary beam at angular offset RADIUS (deg)
C   from the pointing position and for observing frequency FREQ (Hz).
C   The power pattern (2 * J1(X) / X) ** 2 of a uniformly illuminated
C   circular aperture is used, since it fits the observations better
C   than the standard PBCOR beam does.  If the relative gain is less
C   than PBMIN = 0.05, it is set to PBMIN.
C      VSCALE is a measured constant inversely proportional to the
C   VLA primary beamwidth, which is assumed to scale as 1./freq.
C   VSCALE = 4.487E-9 corresponds to a 29.4 arcmin fwhm at 1.47 GHz.
C   The actual scale is determined from the antenna size (PBFSIZ).
C   XMAX = value of X yielding PB = PBMIN = 0.05, beyond which the
C   series approximation loses accuracy.
C      NOTE: This routine is probably only useful for the VLA but might
C   be OK for a homogenous array of uniformly illuminated antennas where
C   the beam scales from the VLA beam by the ratio of antenna diameters.
C   Inputs:
C      NFREQ     I       Number of frequencies in PBFREQ
C      PBFREQ    D(*)    Frequencies (Hz) going into the average.
C      PBFSIZ    R       Antenna diameter (m)
C      IFREQ     I       Index in PBFREQ of current frequency
C      PBFINC    I       Number frequencies to do this time
C      MFREQ     L       True => model from MODFRQ only,
C                        else from sum of all channels
C      SIDFRQ    D       Frequency of spectral index
C      MODFRQ    D       Frequency of model image/CC
C      RADIUS    R       Distance from pointing center in deg.
c      SINDEX    R(2)    Spectral index
C      PBPARM    R(6)    Primary beam parms (1) > 0 => use (2-6)
C      SIFREQ    D       Frequency of spectral index images
C      MODFRQ    D       Frequency of model used if MFREQ true
C      PBOMIT    I       Omit (1) all in, (2) some in, (3) some out
C                           (4) all out - else omit nothing
C   Outputs:
C      PBFACT    R       Relative gain correction
C-----------------------------------------------------------------------
      INTEGER   NFREQ, IFREQ, PBFINC, PBOMIT
      LOGICAL   MFREQ
      REAL      PBFSIZ, RADIUS, SINDEX(2), PBPARM(6)
      DOUBLE PRECISION PBFREQ(NFREQ), SIFREQ, MODFRQ
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, I2, JFREQ1, JFREQ2
      LOGICAL   ALLIN, ALLOUT
      REAL      SUM, PBMIN, XMAX, ASIZE, SUM2
      DOUBLE PRECISION X, U, C1, C2, C3, C4, C5, C6, VSCALE, SCALE, PB,
     *   F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C                                       Coefficients C from Abramowitz
C                                       and Stegun, eq. 9.4.4
      DATA C1 /-0.56249985D0/, C2 /0.21093573D0/, C3 /-0.03954289D0/
      DATA C4 /0.00443319D0/, C5 /-0.00031761D0/, C6 /0.00001109D0/
      DATA PBMIN /0.05/, VSCALE /4.487D-9/, XMAX /3.00751/
C-----------------------------------------------------------------------
      ASIZE = PBFSIZ
      IF (ASIZE.LE.0.0) ASIZE = 25.0
C                                       Beam scale size at 1.47 GHz
      SCALE = VSCALE * 25.0D0 / ASIZE
      SUM = 0.0
      SUM2 = 0.0
      I2 = IFREQ + PBFINC - 1
      I2 = MAX (IFREQ, MIN (NFREQ, I2))
C                                       Compute antenna power gains
      ALLIN = .TRUE.
      ALLOUT = .TRUE.
      IF (SIFREQ.LE.0.0D0) SIFREQ = 1.D9
      IF (MFREQ) THEN
         JFREQ1 = 0
         JFREQ2 = 0
         IF (ASIZE.LT.0.01) THEN
            PB = 1.0
C                                       implements PBCALC locally
         ELSE IF (PBPARM(1).GT.0.0) THEN
            X = (MODFRQ * 60.0D0 / 1.D9 * RADIUS) ** 2
            PB = 1.0 + ((PBPARM(2)/1.D3) + ((PBPARM(3)/1.D7) +
     *         ((PBPARM(4)/1.D10) + ((PBPARM(5)/1.D13) +
     *         (PBPARM(6)/1.D16) * X) * X) * X) * X) * X
C                                       general model
         ELSE
            X = SCALE * RADIUS * MODFRQ
            IF (X.LT.XMAX) THEN
               U = X * X / 9.0D0
               PB = 0.5 + U*(C1 + U*(C2 + U*(C3 + U*(C4 + U*(C5 +
     *            U*C6)))))
               PB = 4.* PB * PB
            ELSE
               PB = PBMIN
               END IF
            END IF
         IF (PB.LE.PBMIN) THEN
            PB = PBMIN
            ALLIN = .FALSE.
         ELSE
            ALLOUT = .FALSE.
            END IF
         IF ((SINDEX(1).NE.0.0) .OR. (SINDEX(2).NE.0.0)) THEN
            F = LOG10 (MODFRQ/SIFREQ)
            F = (SINDEX(1) + SINDEX(2) * F) * F
            F = 10.0 ** (F)
            IF (F.LE.0.0) F = 1.0
            F = MAX (0.001D0, MIN (1000.0D0, F))
            PB = PB * F
            END IF
         SUM = PB
      ELSE
         JFREQ1 = 1
         JFREQ2 = NFREQ
         END IF
      DO 50 I = 1,NFREQ
         IF (((I.GE.JFREQ1) .AND. (I.LE.JFREQ2)) .OR.
     *      ((I.GE.IFREQ) .AND. (I.LE.I2))) THEN
            IF (ASIZE.LT.0.01) THEN
               PB = 1.0
C                                       implements PBCALC locally
            ELSE IF (PBPARM(1).GT.0.0) THEN
               X = (PBFREQ(I) * 60.0D0 / 1.D9 * RADIUS) ** 2
               PB = 1.0 + ((PBPARM(2)/1.D3) + ((PBPARM(3)/1.D7) +
     *            ((PBPARM(4)/1.D10) + ((PBPARM(5)/1.D13) +
     *            (PBPARM(6)/1.D16) * X) * X) * X) * X) * X
C                                       general model
            ELSE
               X = SCALE * RADIUS * PBFREQ(I)
               IF (X.LT.XMAX) THEN
                  U = X * X / 9.0D0
                  PB = 0.5 + U*(C1 + U*(C2 + U*(C3 + U*(C4 + U*(C5 +
     *               U*C6)))))
                  PB = 4.* PB * PB
               ELSE
                  PB = PBMIN
                  END IF
               END IF
            IF (PB.LE.PBMIN) THEN
               PB = PBMIN
               ALLIN = .FALSE.
            ELSE
               ALLOUT = .FALSE.
               END IF
            IF ((SINDEX(1).NE.0.0) .OR. (SINDEX(2).NE.0.0)) THEN
               F = LOG10 (PBFREQ(I)/SIFREQ)
               F = (SINDEX(1) + SINDEX(2) * F) * F
               F = 10.0 ** (F)
               IF (F.LE.0.0) F = 1.0
               F = MAX (0.001D0, MIN (1000.0D0, F))
               PB = PB * F
               END IF
            IF ((I.GE.JFREQ1) .AND. (I.LE.JFREQ2)) SUM = SUM + PB
            IF ((I.GE.IFREQ) .AND. (I.LE.I2)) SUM2 = SUM2 + PB
            END IF
 50      CONTINUE
      IF (PBOMIT.EQ.1) THEN
         IF (ALLIN) SUM2 = 0.0
      ELSE IF (PBOMIT.EQ.2) THEN
         IF (.NOT.ALLOUT) SUM2 = 0.0
      ELSE IF (PBOMIT.EQ.3) THEN
         IF (.NOT.ALLIN) SUM2 = 0.0
      ELSE IF (PBOMIT.EQ.4) THEN
         IF (ALLOUT) SUM2 = 0.0
         END IF
      SUM2 = SUM2 / (I2-IFREQ+1)
C                                       Compute relative gain
      IF (SUM.LE.0.0) THEN
         PBFACT = 1.0
      ELSE
         PBFACT = (JFREQ2-JFREQ1+1) * SUM2 / SUM
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPVALU (SPIX, IMAGE, X, Y, SINDEX, SIFREQ)
C-----------------------------------------------------------------------
C   Return spectral index from SPIX image at X,Y of IMAGE
C   Inputs:
C      SPIX     C(2)*32   Spectral index image name
C      IMAGE    C*(*)     Image have coordinate X,Y in degrees
C      X        R         "X" position in pixels
C      Y        R         "Y" position in pixels
C   In/out
C      SIFREQ   D         Reference frequency for spectral index
C   Output:
C      SINDEX   R(2)      Spectral index: 0 on error
C-----------------------------------------------------------------------
      CHARACTER SPIX(2)*32, IMAGE*(*)
      REAL      X, Y, SINDEX(2)
      DOUBLE PRECISION SIFREQ
C
      CHARACTER SSPIX*32, SSPIX2*32, CDUM*1, CTYPE(7)*8
      INTEGER   MSGSAV, IERR, I, DIM(7), TYPE, NAX(7), IDUM, KWORDS,
     *   NAX2(7)
      REAL      XYZI(7), XYZO(7), INTRAD, SPIMG1(2), SPIMG2(2)
      LONGINT   PSPIM1, PSPIM2
      DOUBLE PRECISION CRVAL(7)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE SSPIX, INTRAD, NAX, SSPIX2, KWORDS, PSPIM1, PSPIM2, NAX2
      DATA SSPIX, SSPIX2, XYZI /2*' ', 0.0, 0.0, 5*1.0/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      SINDEX(1) = 0
      SINDEX(2) = 0
C                                       spix is not current
      IF (SSPIX.NE.SPIX(1)) THEN
C                                       close old one
         MSGSUP = 32000
         IF (SSPIX.NE.' ') THEN
            CALL OCLOSE (SSPIX, IERR)
            CALL ZMEMRY ('FREE', 'SPVAL1', KWORDS, SPIMG1, PSPIM1, IERR)
            END IF
         MSGSUP = MSGSAV
C                                       make and open new one
         CALL CREATE (SPIX(1), 'IMAGE', IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMGOPN (SPIX(1), 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
         SSPIX = SPIX(1)
         MSGSUP = 32000
         CALL OGET (SSPIX, 'SPIXRADIUS', TYPE, DIM, INTRAD, CDUM, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            INTRAD = 1
            END IF
         IF (IERR.NE.0) GO TO 990
         INTRAD = MAX (0.0, MIN (12.0, INTRAD)) - 0.0001
         CALL ARDGET (SPIX(1), 'NAXIS', TYPE, DIM, NAX, CDUM, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       get ref freq
         CALL IMDGET (SPIX(1), 'CTYPE', TYPE, DIM, IDUM, CTYPE, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMDGET (SPIX(1), 'CRVAL', TYPE, DIM, CRVAL, CDUM, IERR)
         IF (IERR.NE.0) GO TO 990
         SIFREQ = -1.D9
         DO 10 I = 1,7
            IF (CTYPE(I).EQ.'FREQ') THEN
               SIFREQ = CRVAL(I)
               END IF
 10         CONTINUE
         IF (SIFREQ.LE.0.0D0) THEN
            MSGTXT = 'FREQUENCY NOT FOUND IN SPECTRAL INDEX IMAGE'
            CALL MSGWRT (8)
            SIFREQ = 1.D9
            IERR = 10
            GO TO 990
            END IF
         KWORDS = (NAX(1) * NAX(2) - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', 'SPVAL1', KWORDS, SPIMG1, PSPIM1, IERR)
         IF (IERR.NE.0) THEN
            MSGTXT = 'SPVALU CANNOT GET MEMORY FOR SPIX IMAGE'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 990
            END IF
         CALL SPIMGT (SSPIX, NAX(1), NAX(2), SPIMG1(1+PSPIM1), IERR)
         IF (IERR.NE.0) GO TO 990
         MSGTXT = 'Read into memory ' // SSPIX
         CALL MSGWRT (2)
         END IF
C                                       curvature
      IF (SPIX(2).NE.SSPIX2) THEN
C                                       close old one
         MSGSUP = 32000
         IF (SSPIX2.NE.' ') THEN
            CALL OCLOSE (SSPIX2, IERR)
            CALL ZMEMRY ('FREE', 'SPVAL2', KWORDS, SPIMG2, PSPIM2, IERR)
            END IF
         MSGSUP = MSGSAV
C                                       make and open new one
         IF (SPIX(2).NE.' ') THEN
            CALL CREATE (SPIX(2), 'IMAGE', IERR)
            IF (IERR.NE.0) GO TO 990
            CALL IMGOPN (SPIX(2), 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
            SSPIX2 = SPIX(2)
            CALL ARDGET (SPIX(2), 'NAXIS', TYPE, DIM, NAX2, CDUM, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL IMDGET (SPIX(2), 'CTYPE', TYPE, DIM, IDUM, CTYPE, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL IMDGET (SPIX(2), 'CRVAL', TYPE, DIM, CRVAL, CDUM, IERR)
            IF (IERR.NE.0) GO TO 990
            DO 20 I = 1,7
               IF (CTYPE(I).EQ.'FREQ') THEN
                  IF (ABS(SIFREQ-CRVAL(I)).GT.1.D4) THEN
                     WRITE (MSGTXT,1015) SIFREQ, CRVAL(I)
                     CALL MSGWRT (8)
                     END IF
                  END IF
 20            CONTINUE
            KWORDS = (NAX2(1) * NAX2(2) - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'SPVAL2', KWORDS, SPIMG2, PSPIM2, IERR)
            IF (IERR.NE.0) THEN
                MSGTXT = 'SPVALU CANNOT GET MEMORY FOR SPIX CURV IMAGE'
               CALL MSGWRT (8)
               IERR = 10
               GO TO 990
               END IF
            CALL SPIMGT (SSPIX2, NAX2(1), NAX2(2), SPIMG2(1+PSPIM2),
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            MSGTXT = 'Read into memory ' // SSPIX2
            CALL MSGWRT (2)
            END IF
         END IF
C                                       input position
      XYZI(1) = X
      XYZI(2) = Y
      CALL PSNCVT (IMAGE, XYZI, SSPIX, XYZO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       window
      IF ((XYZO(1).GE.1.0) .AND. (XYZO(1).LE.NAX(1)) .AND.
     *   (XYZO(2).GE.1.0) .AND. (XYZO(2).LE.NAX(2))) THEN
         CALL SPVAGT (NAX(1), NAX(2), SPIMG1(1+PSPIM1), XYZO,
     *      INTRAD, SINDEX(1), IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       curvature
      IF (SSPIX2.NE.' ') THEN
         CALL PSNCVT (IMAGE, XYZI, SSPIX2, XYZO, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((XYZO(1).GE.1.0) .AND. (XYZO(1).LE.NAX2(1)) .AND.
     *      (XYZO(2).GE.1.0) .AND. (XYZO(2).LE.NAX2(2))) THEN
            CALL SPVAGT (NAX2(1), NAX2(2), SPIMG2(1+PSPIM2),
     *         XYZO, INTRAD, SINDEX(2), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  MSGTXT = 'SPVALU: ERROR FINDING SPECTRAL INDEX'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1015 FORMAT ('SPECTRAL INDEX FREQUENCIES DIFFER',2(1PE13.6))
      END
      SUBROUTINE SPIMGT (SPIX, NX, NY, IMAGE, IERR)
C-----------------------------------------------------------------------
C   SPIMGT fills the array with the specified image
C   Inputs:
C      SPIX    C*(*)   Image name
C      NX      I       Number X pixels
C      NY      I       Number Y pixels
C   Outputs
C      IMAGE   R(*)    image
C      IERR    I       error code
C-----------------------------------------------------------------------
      CHARACTER SPIX*(*)
      INTEGER   NX, NY, IERR
      REAL      IMAGE(NX,*)
C
      INTEGER   BLC(7), TRC(7), DIM(7), IY
      CHARACTER CDUM*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      CALL FILL (7, 1, BLC)
      CALL FILL (7, 1, TRC)
      TRC(1) = NX
      TRC(2) = NY
      DIM(1) = 7
      DIM(2) = 1
      CALL ARDPUT (SPIX, 'BLC', OOAINT, DIM, BLC, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (SPIX, 'TRC', OOAINT, DIM, TRC, CDUM, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARROPN (SPIX, 'READ', IERR)
      IF (IERR.NE.0) GO TO 990
      DO 20 IY = 1,NY
         CALL ARREAD (SPIX, DIM, IMAGE(1,IY), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, IY
            CALL MSGWRT (8)
            GO TO 990
            END IF
 20      CONTINUE
      CALL ARRCLO (SPIX, IERR)
      IF (IERR.EQ.0) GO TO 999
C
 990  MSGTXT = 'SPVALU: ERROR FINDING SPECTRAL INDEX'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPIMGT: ERROR',I5,'READING SPIX ROW',I6)
      END
      SUBROUTINE SPVAGT (NX, NY, IMAGE, XY, RAD, VALUE, IERR)
C-----------------------------------------------------------------------
C   Averages a value from an image
C   Inputs:
C      NX      I       Number X pixels
C      NY      I       Number Y pixels
C      IMAGE   R(*)    image
C      XY      R(2)    X,Y coordinate of center
C      RAD     R       radiuas
C   Outputs:
C      VALUE   R       interpolated or averaged value
C      IERR    I       error code
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IERR
      REAL      IMAGE(NX,*), XY(2), RAD, VALUE
C
      INTEGER   I, J, II, JJ, BLC(2), TRC(2), N, IROUND
      REAL      AV(2,2), RR, XX, YY, SUM
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
      VALUE = 0.0
      I = IROUND (XY(1) - RAD)
      I = MAX (1, MIN (NX, I))
      BLC(1) = I
      I = IROUND (XY(1) + RAD)
      I = MAX (1, MIN (NX, I))
      TRC(1) = I
      I = IROUND (XY(2) - RAD)
      I = MAX (1, MIN (NY, I))
      BLC(2) = I
      I = IROUND (XY(2) + RAD)
      I = MAX (1, MIN (NY, I))
      TRC(2) = I
      II = TRC(1) - BLC(1) + 1
      JJ = TRC(2) - BLC(2) + 1
C                                       linear interpolation
      IF ((II.LE.2) .AND. (JJ.LE.2)) THEN
         DO 30 J = 1,JJ
            CALL RCOPY (II, IMAGE(BLC(1),BLC(2)+J-1), AV(1,J))
 30         CONTINUE
         IF (II.EQ.2) THEN
            IF (AV(1,1).EQ.FBLANK) THEN
               AV(1,1) = AV(2,1)
            ELSE IF (AV(2,1).NE.FBLANK) THEN
               AV(1,1) = (XY(1) - BLC(1)) * AV(1,1) +
     *            (TRC(1) - XY(1)) * AV(2,1)
               END IF
            IF (JJ.EQ.2) THEN

               IF (AV(1,2).EQ.FBLANK) THEN
                  AV(1,2) = AV(2,2)
               ELSE IF (AV(2,2).NE.FBLANK) THEN
                  AV(1,2) = (XY(1) - BLC(1)) * AV(1,2) +
     *               (TRC(1) - XY(1)) * AV(2,2)
                  END IF
               END IF
            END IF
         IF (JJ.EQ.2) THEN
            IF (AV(1,1).EQ.FBLANK) THEN
               AV(1,1) = AV(1,2)
            ELSE IF (AV(1,2).NE.FBLANK) THEN
               AV(1,1) = (XY(2) - BLC(2)) * AV(1,1) +
     *            (TRC(2) - XY(2)) * AV(1,2)
               END IF
            END IF
         IF (AV(1,1).NE.FBLANK) VALUE = AV(1,1)
C                                       average circle
      ELSE
         N = 0
         RR = RAD * RAD
         SUM = 0.0
         DO 50 J = BLC(2),TRC(2)
            YY = J - XY(2)
            YY = YY * YY
            IF (YY.LE.RR) THEN
               DO 40 I = BLC(1),TRC(1)
                  XX = I - XY(1)
                  XX = XX * XX
                  IF (XX+YY.LE.RR) THEN
                     IF (IMAGE(I,J).NE.FBLANK) THEN
                        N = N + 1
                        SUM = SUM + IMAGE(I,J)
                        END IF
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         IF (N.GT.0) VALUE = SUM / N
         END IF
C
 999  RETURN
      END
