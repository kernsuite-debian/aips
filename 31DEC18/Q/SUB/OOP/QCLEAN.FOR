C   CLEAN Class "Q" routine Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "CLEAN" class "Q" routine library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2018
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
C    The "Q" routine utility module contains most of the functional
C    parts of the Clean class.  Since these use "Q" routines they cannot
C    be in the main Clean class module.
C       This class supports two kinds of Clean:
C    1) the "IMAGE" clean in which all operations are done on images or
C    their FFT.  CLNIM will do a full IMAGE Clean.
C    Only one field is allowed as the algorithm is inadequate for more.
C    2) "UV" Clean in which the model subtraction at major cycles is
C    done on the ungridded uv data.  CLNUV will do a full UV Clean.
C    Multiple fields are allowed.
C       Cleans are done using the Barry Clark version of Clean (aka
C    Cotton-Schwab for UV Cleans).   Only one Clean object is allowed to
C    be opened at a time.
C    Class public members are defined in $APLOOP/CLEAN.FOR.
C
C   Module private data:
C      ACTIVE      L        Clean common active
C      MFIELD      I        Number of fields (max. 1 for IMAGE Clean)
C      MMFELD      I        Number of fields cleaned (>= MFIELD)
C      CNAME       C(*)*32  Names of residual/Clean images
C      DBNAME      C(*)*32  Name of dirty beam image.
C      WORK1       C*32     Scratch image
C      WORK2       C*32     Scratch image
C      NCLNG       I(*)     Number of components per field.
C      NSUBG       I(*)     Number of components subtracted per field.
C      CCDISK      I(*)     Disk numbers of the fields.
C      CCCNO       I(*)     Catalog slot numbers of the fields.
C      CCVER       I(*)     Version numbers of the fields.
C      CELLSG      R(2)     Grid increment in RA, Dec (asec)
C      IMSIZE      I(2,*)   Image size of each field in pixels.
C      ICNTRX      I(*)     Image center X pixel per field
C      ICNTRY      I(*)     Image center Y pixel per field
C      XPOFF       R(*)     X ref. pixel offset from center shift per
C                           field (deg)
C      YPOFF       R(*)     Y ref. pixel offset from center shift per
C                           field (deg)
C      MROTAT      R        Coordinate rotation (deg)
C      NEWBEM      L        If true make a new beam when starting a
C                           Clean.
C      NXBEM       I(*)     Number of cells in "x" dimension of beam
C      NYBEM       I(*)     Number of cells in "y" dimension of beam
C      NBMHIS      I        Number of levels in image histogram = NRSBIN
C      BMHIS       I(2060)  An array whose elements have values between
C                           1 and NBMHIS + 1 indicating the maximum abs.
C                           exterior sidelobe for a beam patch whose
C                           size corresponds to the array index.
C                           e.g. for a beam patch of half size I the
C                           maximum fractional absolute sidelobe level
C                           exterior to the  beam patch is BMHIS(I) /
C                           NBMHIS
C      BMAJ        R        Major axis size (FWHP in sec).
C      BMIN        R        Minor axis size (FWHP in sec).
C      BPA         R        Position angle of major axis (degrees)
C      COMRES      R(*)     Clean component diameter (fwhm in degrees)
C      MNFFLX      R(*)     Min flux for each field
C      USEFIT      L        If true use fitted beam
C      GAUSAA      R        Coefficient of u**2
C      GAUSBB      R        Coefficient of u*v
C      GAUSCC      R        Coefficient of v**2
C      NBOXES      I(*)     Number of boxes given for field
C      WIN         I(4,*)   Boxes for fields
C                           WIN(1,*)=-1 indicates a round box of
C                           width WIN(2,*) pixels centered on
C                           pixel (WIN(3,*), WIN(4,*))
C      ISUNBX      L        T => there are UNBOXS
C      UNBOXS      I(*)     Number of UNClean boxes given for field
C      UNWIN       I(4,*)   UNClean Boxes for fields: areas to avoid
C                           WIN(1,*)=-1 indicates a round box of
C                           width WIN(2,*) pixels centered on
C                           pixel (WIN(3,*), WIN(4,*))
C      RESHIS      R(16382) The histogram of the distribution of pixel
C                           values.
C      NRSBIN      I        Number of bins in RESHIS
C      NEDHIS      I        Do we need a new histogram: -1 no, 0 all, i
C      LASTIT      I        No. components in last major cycle
C      RESMAX      R        Max. abs. residual map value.
C      RSSMAX      R(*)     Max abs residual value by field
C      MAXRES      I        Max. number of residuals loaded.
C      MAXPCH      I        Maximum beam patch size allowed.
C      MINPCH      I        Minimum beam patch size allowed.
C      PATCH       I        Beam patch size (max. distance from the
C                           center)
C      MAPLIM      R        Minimum abs. map level to be considered.
C                           Only values > than MAPLIM should be used.
C      CCFLIM      R        Fraction of peak residual loaded at which
C                           one should stop.
C      CCVLIM      R        Stop at the flux level for a component in
C                           addition to the MAPLIM*(1+ATLIM) test
C      ATLIMI      R        Initial value for ATLIM
C      NRESKP      I        Number of residuals to skip when there are
C                           too many to fit.
C      APRESD      I        first location of the residuals.
C      APCFLD(*)   I        start addresses of residuals for each field
C      APCLCN(*)   I        element count for each field.
C      GAIN        R        Clean loop gain.
C      PHAT        R        Prussian helmet spike size (default=0)
C      MINFLX      R        Min. residual flux. (default = 0)
C      OVRLAP      I        Form of Clean and restore - 0 no overlap,
C                           1 - Clean together, allow overlap
C                           2 - Clean 1 field/cycle, allow overlap
C      SDICLN      I        -2 -> SDI Clean forbidden, -1 allowed by TV
C                           0 allowed by data, >0 SDI now being done
C                           2 BGC allowed by TV
C      SDIGN       R
C      CCFILT      R(2)     Filter components < CCFILT(1) summed in
C                           radius CCFILT(2) pixels
C      NOREST      L        If .true. don't restore components to
C      FACTOR      R        Clean speed up factor.
C      TFLUXG      R        Total Clean flux
C      FLUXG       R(*)     Clean flux for each field.
C      RESNUM      I        Number of residuals loaded.
C      CLNLIM      I        Maximum number of clean components desired.
C      FINISH      L        .TRUE. If minimum clean component flux
C                           or maximum iteration encountered, otherwise
C                           .FALSE.
C      FSTCLN      L        Set to false to indicate this is not the
C                           first clean cycle on this channel.
C      CHANN       I        Image Channel number to process.
C      APBEAM      I        Start location for beam patch
C      GRID        C*32     Name of grid object for image plane Clean.
C      DOSCAL      L        If true then scale residuals
C      BMSSZ       I(2)     Half widths of box to determine beam area.
C      PCCMIN      R        Minimum component; used in divergence test
C                           in CLACLN.
C      NCCMIN      R        Min component so far in this major cycle
C      QUIT        L        A TELL has ordered a QUIT
C      MXULST      I        IMAGRPRM(18) limit number facets imaged
C                           at once when searching for next one
C      AUTOBX      R(6)     (1) # auto boxes, (2/3) island/peak cuts
C                           (4) clip wrt MAX, (5) extend boxes
C                           (6) edgeskip
C      ELIMAX      R        when auto boxing, the max abs in the
C                           residuals inside inscribed ellipse
C      ELIMXR      R(10)    when auto boxing, the max abs in the
C                           residuals inside inscribed ellipse per
C                           resolution
C      ELIMXF      R(*)     max residual in inscribed ellipse by field
C   Used for IMAGE Clean only
C      DNAME       C(*)*32  Names of Dirty images
C      WFIRST      L        If true GRID is uninitialised
C      TRANFN      C*32     Name of transfer function (FT of beam) for
C                           image plane Clean.
C   Used for UV Clean only:
C      UVDATA      C*32     residual UV data object.
C      UVCHAN      I        First channel number in uv data to
C                           process.
C      NCHAV       I        Number of channels to average.
C
C   Public functions:
C      CLNINI (name, ctype, ierr)
C         Initializes Clean.
C      CLNIM (name, ierr)
C         Full image plane Clark Clean.
C      CLNUV (name, domax, ierr)
C         Full ungridded uv plane Cotton-Schwab Clean.
C      CLNCYC (name, ifield, ierr)
C         One major cycle of B. Clark Clean.
C
C   Private functions:
C      CLNUV1 (name, domax, ierr)
C         OVERLAP < 2 inner portion of CLNUV
C      CLNUV2 (name, domax, ierr)
C         OVERLAP >= 2 inner portion of CLNUV
C      FNDMAX (name, bmscal, tvfld, ierr)
C         Finds next field from max residual
C      CLNSRT (name, cname, uvdata, ierr)
C         Sort the UV data for CLNUV?
C      CLNTV  (name, ifield, ierr)
C         TV display and interaction
C      CLABOX (name, ifield, ierr)
C         do autoboxing
C      CLBCHK (tvname, maxx, tvwnd, tvfld, ierr)
C         Check TBLC, TTRC, TXINC, TYINC for image field against WIN and
C         the TV size.  It forces TBLC, TTRC to encompass the full set
C         of windows and then sets TXINC and TYINC to allow the full
C         image to be loaded to the tv.
C      CLNTEL (name, ierr)
C         do TELL operation during cleaning
C      CLBHIS (iret)
C         determines the beam maximum and the histogram of maximum beam
C         value outside of a given beam patch.
C      CLRHIS (name, ifield, iret)
C         Returns maximum pixel value and histogram of pixel values in
C         the residual images.
C      CDECID (ifield, doit)
C         Determines a beam patch and limiting map value (PATCH and
C         MAPLIM) which will optimize the use of memory.
C      CLBSHV (ifield, iret)
C         Loads the beam patch into memory.
C      CLMPAC (ifield, iret)
C         Takes points from the residual map greater than MAPLIM and
C         places them with their addresses into memory
C      CLSCRS (iret)
C         Scales the residuals by the ratio of the restoring beam area
C         to the dirty beam area (defined in some region) to insure that
C         the residuals are in the same physical units as the restored
C         components.
C      CLREST (iret)
C         Convolves components with restoring beam and adds to
C         residual images.
C      CLOVER (iret)
C         Restores components from each field to all other fields.
C      CLRGRD (ifield, tgrid, iret)
C         restores Clean components from field IFIELD to TGRID file.
C         This is done by transforming the Clean components, multiplying
C         by the appropriate Gaussian function and placing in the TGRID
C         file.
C      CLCCRM (ifield, dosum, dosub, aplo, apbuf, first, number, nload,
C         nx, ny, iret)
C         loads Clean components for field IFIELD into the "AP" in
C         preparation for transformation to the data plane.
C      CLGFIT (iret)
C         Fits a Gaussian to the inner portion of a dirty beam
C      CLACLN (iret)
C         Does the minor cycles for a single pass of Clark Clean.
C      CLASDI (iret)
C         Does the minor cycles for a single pass of SDI Clean.
C      CLFILT (iret)
C         Filters out weak, isolated CCs; copies initial data;
C         removes full CC files from (new) residual data
C      CLGAUS (ifield)
C         Initializes structures in memory for convolving Gaussians.
C      CLCSUM
C         Sums flux densities in CC files.
C      CLGRID (ierr)
C         Accumulates FT of components times the negative of the
C         transfer function.
C      CLRMAX (ifield, nfield, absmax, winmax, fldmax, fldavg, ierr)
C         Find the maximum absolute pixel value of a set of images.
C      CLIINF (ierr)
C         Update and check image info in common.
C
C  New useful keywords in object:
C     NFLDRSTR   integer # fields to re-compute at the end and restore
C                must be > 0 and < 'NIMAGES' value
C     INDATA     Character name of object containing a copy of the
C                initial UV data.  '-' => make one in CLNUV.  This must
C                be non blank to allow CC filtering.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'QCLEAN.INC'
C                                       Include for Clean class.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PCLN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DAPM.INC'
C
      INTEGER   MFIELD, NCLNG(MAXFLD), NSUBG(MAXFLD), CCDISK(MAXFLD),
     *   CCCNO(MAXFLD), CCVER(MAXFLD), IMSIZE(2,MAXFLD), ICNTRX(MAXFLD),
     *   ICNTRY(MAXFLD), NXBEM(MAXFLD), NYBEM(MAXFLD), NBMHIS, NFPRES,
     *   BMHIS(2060), NBOXES(MAXFLD), WIN(4,MXNBFL), NRSBIN, MAXRES,
     *   MAXPCH, MINPCH, NEDHIS, PATCH, NRESKP, APRESD, APCFLD(MAXFLD),
     *   APCLCN(MAXFLD), RESNUM, CLNLIM, CHANN, APBEAM, UVCHAN, NCHAV,
     *   BMSSZ(2), LASTIT, OVRLAP, SDICLN, ALLOK, MMFELD, NUMRES, NEDH1,
     *   LAXRES, SDINOW, MXULST, UNBOXS(MAXFLD), UNWIN(4,4*MAXFLD),
     *   PIXFLD
      LOGICAL   ACTIVE, FINISH, FSTCLN, WFIRST, NOREST, USEFIT, NEWBEM,
     *   DOSCAL, DO3DIM, FFIELD, AFIELD, QUIT, ONEBEM, UNBOXD, ISUNBX
      REAL      CELLSG(2), XPOFF(MAXFLD), YPOFF(MAXFLD), MROTAT,
     *   BMAJ(MAXFLD), BMIN(MAXFLD), BPA(MAXFLD), GAUSAA, GAUSBB,
     *   GAUSCC, RESHIS(16384), RESMAX, ACTRES, MAPLIM, CCFLIM, CCVLIM,
     *   ATLIMI, GAIN, PHAT, MINFLX, FACTOR, TFLUXG, PIXRNG(2),
     *   FLUXG(MAXFLD), PCCMIN, ROW1(MABFSS), ROW2(MABFSS),
     *   ROW3(MABFSS), NCCMIN, RSSMAX(MAXFLD), RSSAVG(MAXFLD), SDIGN,
     *   CCFILT(2), FLDCCM(MAXFLD), COMRES(MAXFLD), BMSCP, BMINAR,
     *   MRCTRL(5), RASH(MAXFLD), DECSH(MAXFLD), RETRY, MNFFLX(MAXFLD),
     *   SDILIM, IGNORE(MAXFLD), AUTOBX(6), ELIMAX, ELIMXR(10),
     *   ELIMXF(MAXFLD), HBMAJ(MAXFLD), HBMIN(MAXFLD), HBPA(MAXFLD),
     *   OVRSW
      CHARACTER DNAME(MAXFLD)*32, UVDATA*32, CNAME(MAXFLD)*32,
     *   DBNAME(MAXFLD)*32, GRID*32, TRANFN*32, WORK1*32, WORK2*32,
     *   ISHELP*6, INDATA*32
      COMMON /CLNPRI/ CELLSG, XPOFF, YPOFF, MROTAT, BMAJ, BMIN, BPA,
     *   GAUSAA, GAUSBB, GAUSCC, RESHIS, RESMAX, ACTRES, RSSMAX,
     *   RSSAVG, MAPLIM, CCFLIM, CCVLIM, ATLIMI, GAIN, PHAT, MINFLX,
     *   FACTOR, TFLUXG, FLUXG, PCCMIN, NCCMIN, ROW1, ROW2, ROW3,
     *   MFIELD, NCLNG, NSUBG, CCDISK, CCCNO, CCVER, IMSIZE, ICNTRX,
     *   ICNTRY, NXBEM, NYBEM, NBMHIS, BMHIS, NBOXES, WIN, NRSBIN,
     *   MAXRES, MAXPCH, MINPCH, PATCH, NRESKP, APRESD, APCFLD, APCLCN,
     *   RESNUM, CLNLIM, CHANN, APBEAM, UVCHAN, NCHAV, BMSSZ, PIXRNG,
     *   FINISH, ACTIVE, FSTCLN, WFIRST, NOREST, USEFIT, NEWBEM, DOSCAL,
     *   LASTIT, NFPRES, DO3DIM, NEDHIS, FFIELD, AFIELD, OVRLAP, SDICLN,
     *   SDINOW, SDIGN, CCFILT, FLDCCM, ALLOK, COMRES, BMSCP, BMINAR,
     *   MRCTRL, RASH, DECSH, QUIT, MMFELD, RETRY, MNFFLX, NUMRES,
     *   NEDH1, LAXRES, SDILIM, IGNORE, MXULST, ONEBEM, AUTOBX, ELIMAX,
     *   ELIMXR, ELIMXF, HBMAJ, HBMIN, HBPA, UNBOXD, ISUNBX, UNBOXS,
     *   UNWIN, PIXFLD, OVRSW
      COMMON /CLNPRC/ DNAME, UVDATA, CNAME, DBNAME, GRID, TRANFN, WORK1,
     *   WORK2, INDATA, ISHELP
C                                                         End QCLEAN.INC
LOCAL END
      SUBROUTINE CLNINI (NAME, CTYPE, IERR)
C-----------------------------------------------------------------------
C   Public
C   Initializes a Clean object.
C   Determines field info, beam histogram.
C   Inputs:
C      NAME        C*?   The name of the Clean object.
C      CTYPE       C*?   Clean subtraction type 'UV' or 'IMAGE'
C   Output to common:
C      MFIELD      I        Number of fields
C      DNAME       C(*)*32  Names of Dirty images
C      CNAME       C(*)*32  Names of residual/Clean images
C      CCDISK      I(*)     Disk numbers of the fields.
C      CCCNO       I(*)     Catalog slot numbers of the fields.
C      CCVER       I(*)     Version numbers of the fields.
C      DBNAME      C(*)*32  Name of dirty beam image.
C      GRID        C*32     Name of grid object for image plane Clean.
C      TRANFN      C*32     Name of transfer function (FT of beam) for
C                           image plane Clean.
C      WORK1       C*32     Scratch image
C      WORK2       C*32     Scratch image
C      NRSBIN      I        Number of bins in RESHIS
C      NBMHIS      I        Number of levels in beam histogram
C      NBOXES      I(*)     Number of boxes given for field
C      WIN         I(4,*)   Boxes for Field
C      IMSIZE      I(2,*)   Image sizes
C      CELLSG      R(2)     Grid increment in RA, Dec (asec)
C      MROTAT      R        Coordinate rotation.
C      ICNTRX      I(*)     Image center X pixel per field
C      ICNTRY      I(*)     Image center Y pixel per field
C      XPOFF       R(*)     X ref. pixel offset from center shift per
C                           field (deg)
C      YPOFF       R(*)     Y ref. pixel offset from center shift per
C                           field (deg)
C      CHANN       I        Frequency channel to be cleaned
C      NCLNG       I(*)     Number of previous components per field.
C      CLNLIM      I        Maximum number of clean components desired.
C      FACTOR      R        Clean speed up factor.
C      GAIN        R        Clean loop gain.
C      PHAT        R        Prussian helment spike size (default=0)
C      MINFLX      R        Min. residual flux.
C      MINPCH      I        Min. Beam patch size. (default=121)
C      MAXPCH      I        Max. Beam patch size. (default=1001)
C      NOREST      L        If .true. don't restore components to
C      FINISH      L        Initialized to .FALSE.
C      TFLUXG      R        Total Clean flux
C      FLUXG       R(*)     Clean flux for each field.
C      DOSCAL      L        If true then scale residuals
C      BMSSZ       I(2)     Half widths of box to determine beam area.
C      PCCMIN      R        Minimum component; used in divergence test
C                           in CLACLN.
C      NCCMIN      R        Minimum component this major cycle
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), CTYPE*(*)
      INTEGER   IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   TYPE, IDIM(7), IFIELD, IBOX, NAXIS(7), MAXNX, MAXNY, I,
     *   BLC(7), TRC(7), MINX, MINY, MAXX, MAXY, MSGSAV, TOP, BOT, IP,
     *   RIGHT, LEFT, DUMMY, PRODCT, BFIELD, IROUND, HPROD, J, K
      REAL      CDELT(7), CROTA(7), CRPIX(7), BMA, BMI, XBMAJ, XBMIN,
     *   IMPARM(20)
      CHARACTER CDUMMY*1, PREFIX*5, CLNAME*8, KEYW*8
      LOGICAL   FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Mark as active
      ACTIVE = .TRUE.
C                                       Mark as having interactive help
C                                       file
      ISHELP = 'CLEAN'
C                                       Not done
      FINISH = .FALSE.
      QUIT = .FALSE.
C                                       Initialize names to blank
      UVDATA = '    '
      GRID = '    '
      TRANFN = '    '
      INDATA = '    '
      WORK1 = '    '
      WORK2 = '    '
      TFLUXG = 0.0
      DO 10 I = 1,MAXFLD
         DBNAME(I) = '    '
         DNAME(I) = '    '
         CNAME(I) = '    '
         FLUXG(I) = 0.0
         IGNORE(I) = 1.0
         IF (FIRST) THEN
            HBMAJ(I) = 0
            HBMIN(I) = 0
            HBPA(I) = 0
            END IF
 10      CONTINUE
      FIRST = .FALSE.
      NEWBEM = .TRUE.
      PIXRNG(1) = 1.E6
      PIXRNG(2) = -1.E6
      PIXFLD = -1
      CALL RFILL (MAXFLD, 1.E12, FLDCCM)
      ELIMAX = 0.0
      CALL RFILL (10, 0.0, ELIMXR)
      CALL RFILL (MAXFLD, 0.0, ELIMXF)
C                                       Number of bins in residual
C                                       histogram.
      NRSBIN = 16384
      NEDHIS = 0
      NEDH1 = 0
C                                       GRID uninitialized
      WFIRST = .TRUE.
C                                       Find CHANNEL number
      CALL CLNGET (NAME, 'CHANNEL', TYPE, IDIM, CHANN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CHANN = MAX (1, CHANN)
C                                       Find number of fields
      CALL CLNGET (NAME, 'NIMAGES', TYPE, IDIM, MFIELD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      MSGSUP = 32000
      CALL CLNGET (NAME, 'NUMRES', TYPE, IDIM, NUMRES, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         NUMRES = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      NUMRES = MAX (1, NUMRES)
      NFPRES = MFIELD / NUMRES
      MSGSUP = 32000
      CALL CLNGET (NAME, 'ALLOKAY', TYPE, IDIM, ALLOK, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         ALLOK = -1
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSUP = 32000
      CALL CLNGET (NAME, 'DO3DIMAG', TYPE, IDIM, DO3DIM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         DO3DIM = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSUP = 32000
      CALL CLNGET (NAME, 'ONEBEAM', TYPE, IDIM, ONEBEM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         ONEBEM = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
      MSGSUP = 32000
      CALL CLNGET (NAME, 'OVERLAP', TYPE, IDIM, OVRLAP, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         OVRLAP = -1
         END IF
      IF (IERR.NE.0) GO TO 995
      SDICLN = 0
      MSGSUP = 32000
      CALL CLNGET (NAME, 'SDIGAIN', TYPE, IDIM, SDIGN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         SDIGN = -1.0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SDIGN.LE.0.0) SDICLN = -2
      MSGSUP = 32000
      CALL CLNGET (NAME, 'CCFILTER', TYPE, IDIM, CCFILT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         CCFILT(1) = 0.0
         CCFILT(2) = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Get Dirty object names
      IF (CTYPE(1:5).EQ.'IMAGE') THEN
         CALL CLNGET (NAME, 'DIRTYI', TYPE, IDIM, DUMMY, DNAME, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get UVdata object names
      ELSE
         CALL CLNGET (NAME, 'UVDATA', TYPE, IDIM, DUMMY, UVDATA, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Uv data channel
         CALL CLNGET (NAME, 'UVCHAN', TYPE, IDIM, UVCHAN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (UVCHAN.LE.0) UVCHAN = 1
C                                       Number of channels to grid
         CALL CLNGET (NAME, 'NCHAV', TYPE, IDIM, NCHAV, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (NCHAV.LE.0) NCHAV = 1
         END IF
C                                       Get Clean object names
      CALL CLNGET (NAME, 'CLEANI', TYPE, IDIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Get Dirty beam object name
      CALL CLNGET (NAME, 'DIRTBEAM', TYPE, IDIM, DUMMY, DBNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Need to make a new beam?
      CALL CLNGET (NAME, 'DOBEAM', TYPE, IDIM, NEWBEM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       find IGNORE
      MSGSUP = 32000
      CALL CLNGET (NAME, 'IGNORE', TYPE, IDIM, IGNORE, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) IERR = 0
      IF (IERR.NE.0) GO TO 995
C                                       Find windows
      CALL CLNGET (NAME, 'NBOXES', TYPE, IDIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'WINDOW', TYPE, IDIM, WIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       UNClean boxes
      ISUNBX = .TRUE.
      UNBOXD = .TRUE.
      CALL FILL (MAXFLD, 0, UNBOXS)
      CALL FILL (4*MAXFLD, 0, UNWIN)
      MSGSUP = 32000
      CALL CLNGET (NAME, 'UNBOXES', TYPE, IDIM, UNBOXS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         ISUNBX = .FALSE.
         UNBOXD = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (ISUNBX) THEN
         MSGSUP = 32000
         CALL CLNGET (NAME, 'UNWINDOW', TYPE, IDIM, UNWIN, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            ISUNBX = .FALSE.
            UNBOXD = .FALSE.
            CALL FILL (MAXFLD, 0, UNBOXS)
            END IF
         IF (IERR.NE.0) GO TO 995
         END IF
      IF (ISUNBX) THEN
         DO 20 I = 1,MFIELD
            K = (I-1)*MFIELD
            DO 15 J = UNBOXS(I),1,-1
               IF (UNWIN(1,K+J).NE.0) GO TO 25
               UNBOXS(I) = J - 1
 15            CONTINUE
 20         CONTINUE
         ISUNBX = .FALSE.
         END IF
C                                       AUTOBOX
 25   MSGSUP = 32000
      CALL OGET (NAME, 'AUTOBOX', TYPE, IDIM, AUTOBX, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         AUTOBX(1) = 0.0
         AUTOBX(2) = 3.0
         AUTOBX(3) = 5.0
         AUTOBX(4) = 0.1
         AUTOBX(5) = 1
         AUTOBX(6) = 5
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 995
      AUTOBX(1) = IROUND (AUTOBX(1))
      AUTOBX(2) = AUTOBX(2)
      AUTOBX(3) = AUTOBX(3)
      AUTOBX(4) = AUTOBX(4)
      AUTOBX(5) = IROUND (AUTOBX(5))
      AUTOBX(6) = IROUND (AUTOBX(6))
      AUTOBX(1) = MIN (50.0, AUTOBX(1))
      IF (AUTOBX(2).LE.1.5) AUTOBX(2) = 3.
      IF (AUTOBX(3).LT.AUTOBX(2)) AUTOBX(3) = AUTOBX(2) + 2.
      IF (AUTOBX(4).LT.0.01) AUTOBX(4) = 0.1
      IF (AUTOBX(4).GT.0.90) AUTOBX(4) = 0.1
      IF (AUTOBX(5).LT.-1.0) AUTOBX(5) = 1.0
      IF (AUTOBX(5).GT.6.0) AUTOBX(5) = 6.0
      IF (AUTOBX(6).LT.1.0) AUTOBX(6) = 5.0
      IF (AUTOBX(6).GT.IMSIZE(1,1)/20.0) AUTOBX(6) = 5.0
C                                       IMAGRPRM
      MSGSUP = 32000
      CALL OGET (NAME, 'IMPARM', TYPE, IDIM, IMPARM, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
         CCFLIM = MAX (0.0, MIN (0.9, IMPARM(19)))
         IMPARM(19) = CCFLIM
         RETRY = IMPARM(20)
         IF (RETRY.LE.0.0) RETRY = 1.05
         IF (RETRY.LT.1.0) RETRY = 1.0 / RETRY
         IF (RETRY.GT.5.0) RETRY = 1.05
         IMPARM(20) = RETRY
         IMPARM(11) = MAX (0.0, MIN (IMPARM(11), 1.0))
         IMPARM(12) = MAX (0.0, MIN (IMPARM(12), 0.1))
         IMPARM(13) = MAX (0.0, MIN (IMPARM(13), 1.0))
         IMPARM(14) = MAX (0.0, MIN (IMPARM(14), 1.0))
         IMPARM(15) = MAX (0.0, MIN (IMPARM(15), 1.0))
         IMPARM(16) = MAX (0.0, IMPARM(16))
         MXULST = IMPARM(18) + 0.1
         IF (MXULST.LE.0) MXULST = 10
         IMPARM(18) = MXULST
         CALL OPUT (NAME, 'IMPARM', TYPE, IDIM, IMPARM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         BMSCP = IMPARM(11)
         CALL RCOPY (5, IMPARM(12), MRCTRL)
      ELSE
         RETRY = 1.005
         BMSCP = 0.0
         CALL RFILL (5, 0.0, MRCTRL)
         END IF
C                                       Minimum flux target
      CALL CLNGET (NAME, 'MINFLUX', TYPE, IDIM, MINFLX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Fill field info
      MAXNX = 0
      MAXNY = 0
      CALL OGET (CNAME(1), 'RASHIFT', TYPE, IDIM, RASH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (CNAME(1), 'DECSHIFT', TYPE, IDIM, DECSH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      DO 100 IFIELD = 1,MFIELD
C                                       Component diameter
         MSGSUP = 32000
         CALL OGET (CNAME(IFIELD), 'COMPDIAM', TYPE, IDIM,
     *      COMRES(IFIELD), CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            COMRES(IFIELD) = 0.0
            END IF
         IF (IERR.NE.0) GO TO 995
         MSGSUP = 32000
         CALL OGET (CNAME(IFIELD), 'MINFLUX', TYPE, IDIM,
     *      MNFFLX(IFIELD), CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            MNFFLX(IFIELD) = 0.0
            IDIM(1) = 1
            IDIM(2) = 1
            TYPE = OOARE
            END IF
         IF (IERR.NE.0) GO TO 995
         IF (MNFFLX(IFIELD).LE.0.0) MNFFLX(IFIELD) = MINFLX
         CALL OPUT (CNAME(IFIELD), 'MINFLUX', TYPE, IDIM,
     *      MNFFLX(IFIELD), CDUMMY, IERR)
C                                       disk, cno
         CALL OBDSKC (CNAME(IFIELD), CCDISK(IFIELD), CCCNO(IFIELD),
     *      IERR)
         IF (IERR.NE.0) GO TO 995
C                                       CC version
         CALL CLNGET (NAME, 'VERSION', TYPE, IDIM, CCVER(IFIELD),
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get size
         CALL ARDGET (CNAME(IFIELD), 'NAXIS', TYPE, IDIM, NAXIS, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Set image size
         IMSIZE(1,IFIELD) = NAXIS(1)
         IMSIZE(2,IFIELD) = NAXIS(2)
         MAXNX = MAX (MAXNX, NAXIS(1))
         MAXNY = MAX (MAXNY, NAXIS(2))
C                                       Set Image center
         ICNTRX(IFIELD) = NAXIS(1) / 2
         ICNTRY(IFIELD) = NAXIS(2) / 2 + 1
C                                       Get Cell spacings
         CALL IMDGET (CNAME(IFIELD), 'CDELT', TYPE, IDIM, CDELT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CELLSG(1) = CDELT(1) * 3600.0
         CELLSG(2) = CDELT(2) * 3600.0
C                                       Get rotation
         CALL IMDGET (CNAME(IFIELD), 'CROTA', TYPE, IDIM, CROTA, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         MROTAT = CROTA(2)
C                                       Get reference pixels
         CALL IMDGET (CNAME(IFIELD), 'CRPIX', TYPE, IDIM, CRPIX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Get offset of center from
C                                       reference pixel
         XPOFF(IFIELD) = (CRPIX(1) - ICNTRX(IFIELD)) * CELLSG(1) / 3600.
         YPOFF(IFIELD) = (CRPIX(2) - ICNTRY(IFIELD)) * CELLSG(2) / 3600.
C                                       Select plane
         BLC(1) = 1
         BLC(2) = 1
         BLC(3) = CHANN
         TRC(1) = NAXIS(1)
         TRC(2) = NAXIS(2)
         TRC(3) = CHANN
         IDIM(1) = 7
         IDIM(2) = 1
         CALL ARDPUT (CNAME(IFIELD), 'BLC', OOAINT, IDIM, BLC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARDPUT (CNAME(IFIELD), 'TRC', OOAINT, IDIM, TRC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Dirty images if necessary
         IF (CTYPE(1:5).EQ.'IMAGE') THEN
            CALL ARDPUT (DNAME(IFIELD), 'BLC', OOAINT, IDIM, BLC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL ARDPUT (DNAME(IFIELD), 'TRC', OOAINT, IDIM, TRC,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       Check ROW? size
         IF (IMSIZE(1,IFIELD).GT.MABFSS) THEN
            IERR = 5
            MSGTXT = 'CLNINI: INTERNAL BUFFERS TOO SMALL'
            GO TO 990
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Default window
C                                       Inner half for image plane
C                                       Clean.
         IF (CTYPE(1:5).EQ.'IMAGE') THEN
            MINX = IMSIZE(1,IFIELD) / 4 + 1
            MAXX = 3 * IMSIZE(1,IFIELD) / 4
            MINY = IMSIZE(2,IFIELD) / 4 + 1
            MAXY = 3 * IMSIZE(2,IFIELD) / 4 - 1
C                                       All but edges for UV clean
         ELSE
            MINX = 6
            MAXX = IMSIZE(1,IFIELD) - 5
            MINY = 6
            MAXY = IMSIZE(2,IFIELD) - 5
            END IF
         IP = MIN (MXNBOX, MXNBFL/MFIELD)
         NBOXES(IFIELD) = MIN (NBOXES(IFIELD), IP)
         DO 80 IBOX = 1,NBOXES(IFIELD)
            IP = (IBOX-1) * MFIELD + IFIELD
C                                       Make sure box set
            IF ((WIN(1,IP).LE.0) .AND. (WIN(2,IP).LE.0) .AND.
     *         (WIN(3,IP).LE.0) .AND. (WIN(4,IP).LE.0)) THEN
               WIN(1,IP) = MINX
               WIN(2,IP) = MINY
               WIN(3,IP) = MAXX
               WIN(4,IP) = MAXY
               END IF
C                                       Set window (check defaults)
            IF (WIN(1,IP).NE.-1) THEN
               WIN(1,IP) = MAX (6, WIN(1,IP))
               WIN(2,IP) = MAX (6, WIN(2,IP))
               WIN(3,IP) = MIN (IMSIZE(1,IFIELD) - 5, WIN(3,IP))
               WIN(4,IP) = MIN (IMSIZE(2,IFIELD) - 5, WIN(4,IP))
               IF ((WIN(4,IP).LT.WIN(2,IP)) .OR.
     *            (WIN(3,IP).LT.WIN(1,IP))) THEN
                  WRITE (MSGTXT,1080) IBOX, IFIELD
                  IERR = 8
                  GO TO 990
                  END IF
C                                       Allow round window
C                                       Round window can't exceed image
            ELSE
               WIN(2,IP) = MAX (1, WIN(2,IP))
               RIGHT = IMSIZE(1,IFIELD) - WIN(3,IP)
               LEFT =  WIN(3,IP) - 1
               TOP = IMSIZE(2,IFIELD) - WIN(4,IP)
               BOT =  WIN(4,IP) - 1
               IF ((WIN(3,IP).LE.1) .OR. (WIN(3,IP).GE.IMSIZE(1,IFIELD))
     *            .OR. (WIN(4,IP).GE.IMSIZE(2,IFIELD)) .OR.
     *            (WIN(4,IP).LE.1)) THEN
                  WRITE (MSGTXT,1080) IBOX, IFIELD
                  IERR = 8
                  GO TO 990
                  END IF
               IF (WIN(2,IP).GT.MIN (RIGHT, LEFT, TOP, BOT)) THEN
                  WIN(2,IP) = MIN (WIN(2,IP), RIGHT, LEFT, TOP, BOT)
C                                       Give warning
                  MSGTXT = 'Warning: round window reduced to keep in' //
     *               'side image'
                  CALL MSGWRT (5)
                  END IF
               END IF
 80         CONTINUE
 100     CONTINUE
C                                       Get size of BEAM
      DO 110 IFIELD = 1,MFIELD
         BFIELD = IFIELD
         IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
         CALL ARDGET (DBNAME(BFIELD), 'NAXIS', TYPE, IDIM, NAXIS,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         MAXNX = MAX (MAXNX, NAXIS(1))
         MAXNY = MAX (MAXNY, NAXIS(2))
         NXBEM(IFIELD) = NAXIS(1)
         NYBEM(IFIELD) = NAXIS(2)
 110     CONTINUE
C                                       Save NBOXES
      IDIM(1) = MFIELD
      IDIM(2) = 1
      IDIM(3) = 0
      CALL CLNPUT (NAME, 'NBOXES', OOAINT, IDIM, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Save WINDOW
      IDIM(1) = 4 * MFIELD
      IDIM(2) = MIN (MXNBOX, MXNBFL/MFIELD)
      CALL CLNPUT (NAME, 'WINDOW', OOAINT, IDIM, WIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Save image centers to UVDATA
      IF (CTYPE(1:2).EQ.'UV') THEN
         IDIM(1) = MFIELD
         IDIM(2) = 1
         CALL OUVPUT (UVDATA, 'CENTERX', OOAINT, IDIM, ICNTRX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OUVPUT (UVDATA, 'CENTERY', OOAINT, IDIM, ICNTRY, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Save CC version numbers to
C                                       UVDATA
      IF (CTYPE(1:2).EQ.'UV') THEN
         IDIM(1) = MFIELD
         IDIM(2) = 1
         CALL OUVPUT (UVDATA, 'MODCCVER', OOAINT, IDIM, CCVER, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Get total number of components
      CALL CLNGET (NAME, 'NITER', TYPE, IDIM, CLNLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      LASTIT = 0.1 * CLNLIM + 0.9
      IF ((MFIELD.GT.1) .AND. (OVRLAP.LE.0) .AND. (CLNLIM.GT.0)) THEN
         MSGTXT = 'OVERLAP >= 1 IS RECOMMENDED IN MULTI-FIELD CLEANING'
         CALL MSGWRT (6)
         END IF
C                                       Get number of components for
C                                       restart.
      CALL CLNGET (NAME, 'BCOMP', TYPE, IDIM, NCLNG, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Init number of components
C                                       subtracted.
      CALL FILL (MAXFLD, 1, NSUBG)
C                                       Get loop gain
      CALL CLNGET (NAME, 'GAIN', TYPE, IDIM, GAIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (GAIN.LE.0.0) GAIN = 0.1
C                                       Get Speedup factor
      CALL CLNGET (NAME, 'FACTOR', TYPE, IDIM, FACTOR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Prussian hat size
      CALL CLNGET (NAME, 'PHAT', TYPE, IDIM, PHAT, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Minimum BEAM patch
      CALL CLNGET (NAME, 'MINPATCH', TYPE, IDIM, MINPCH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (MINPCH.LE.0) MINPCH = 121
C                                       Maximum BEAM patch
      CALL CLNGET (NAME, 'MAXPATCH', TYPE, IDIM, MAXPCH, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (MAXPCH.LE.0) MAXPCH = 2001
C                                       Maximum number of residuals
      CALL CLNGET (NAME, 'MAXNRES', TYPE, IDIM, MAXRES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (MAXRES.LE.0) MAXRES = 20000
C                                       No restore flag
      CALL CLNGET (NAME, 'NORESTORE', TYPE, IDIM, NOREST, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Restoring beam
      CALL CLNGET (NAME, 'BEAM.BMAJ', TYPE, IDIM, BMAJ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'BEAM.BMIN', TYPE, IDIM, BMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'BEAM.BPA', TYPE, IDIM, BPA, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Scale residuals?
      CALL CLNGET (NAME, 'SCALERES', TYPE, IDIM, DOSCAL, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'BMSCLSZ', TYPE, IDIM, BMSSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Convert to asec
      BMAJ(1) = BMAJ(1) * 3600.0
      BMIN(1) = BMIN(1) * 3600.0
      CALL RFILL (MAXFLD-1, BMAJ(1), BMAJ(2))
      CALL RFILL (MAXFLD-1, BMIN(1), BMIN(2))
      CALL RFILL (MAXFLD-1, BPA(1), BPA(2))
C                                       Use fitted or specified value?
      USEFIT = (BMAJ(1).LE.0.0) .OR. (BMIN(1).LE.0.0)
C                                       Don't restore unless Cleaning
      NOREST = NOREST .OR. (CLNLIM.LE.0)
C                                       Do previous scratch files exist?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CLNGET (NAME, 'INDATA', TYPE, IDIM, DUMMY, INDATA, IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
         INDATA = '    '
         END IF
      CALL CLNGET (NAME, 'WORK1', TYPE, IDIM, DUMMY, WORK1, IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
         WORK1 = '    '
         END IF
      CALL CLNGET (NAME, 'WORK2', TYPE, IDIM, DUMMY, WORK2, IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
         WORK2 = '    '
         END IF
      CALL CLNGET (NAME, 'GRID', TYPE, IDIM, DUMMY, GRID, IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
         GRID = '    '
         END IF
      CALL CLNGET (NAME, 'TRANFN', TYPE, IDIM, DUMMY, TRANFN, IERR)
      IF (IERR.NE.0) THEN
         IERR = 0
         TRANFN = '    '
         END IF
      MSGSUP = MSGSAV
      IF (CCFILT(1).EQ.0.0) THEN
         INDATA = ' '
         END IF
C                                       Create scratch files, make large
C                                       enough for largest field or beam
      CALL FILL (7, 1, NAXIS)
      NAXIS(1) = MAXNX + 2
      NAXIS(2) = MAXNY
C                                       WORK1
      IDIM(1) = LEN (WORK1)
      IDIM(2) = 1
      IDIM(3) = 0
      IF (WORK1.EQ.'    ') THEN
         WORK1 = 'Clean Work file 1'
         CALL IMGSCR (WORK1, NAXIS, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Save name
         CALL CLNPUT (NAME, 'WORK1', OOACAR, IDIM, DUMMY, WORK1, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       WORK2
      IF (WORK2.EQ.'    ') THEN
         WORK2 = 'Clean Work file 2'
         CALL IMGSCR (WORK2, NAXIS, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Save name
         CALL CLNPUT (NAME, 'WORK2', OOACAR, IDIM, DUMMY, WORK2, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Select beam plane
      BLC(1) = 1
      BLC(2) = 1
      BLC(3) = CHANN
      TRC(3) = CHANN
      IDIM(1) = 7
      IDIM(2) = 1
      DO 120 IFIELD = 1,MFIELD
         TRC(1) = NXBEM(IFIELD)
         TRC(2) = NYBEM(IFIELD)
         BFIELD = IFIELD
         IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
         CALL ARDPUT (DBNAME(BFIELD), 'BLC', OOAINT, IDIM, BLC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARDPUT (DBNAME(BFIELD), 'TRC', OOAINT, IDIM, TRC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
 120     CONTINUE
C                                       New beam?
      IF (NEWBEM) THEN
C                                       Make beam if necessary for UV
C                                       Clean
         IF ((CTYPE(1:2).EQ.'UV') .AND. (ALLOK.LE.0)) THEN
            CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *         .TRUE., UVCHAN, NCHAV, CHANN, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       Do beam histogram.
         NBMHIS = NRSBIN
         CALL CLBHIS (NAME, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Fit beam
         CALL CLGFIT (IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Save beam in Clean images
      IDIM(1) = 1
      IDIM(2) = 1
C                                       product type
      PRODCT = 1
      IF (NOREST) PRODCT = 3
      IF (CLNLIM.LE.0) PRODCT = 0
C                                       Beam parameters
      BMINAR = 1.E12
      DO 200 IFIELD = 1,MFIELD
         BMA = BMAJ(IFIELD) / 3600.0
         BMI = BMIN(IFIELD) / 3600.0
         IF (.NOT.USEFIT) THEN
            BMA = SQRT (BMA*BMA + COMRES(IFIELD)*COMRES(IFIELD))
            BMI = SQRT (BMI*BMI + COMRES(IFIELD)*COMRES(IFIELD))
            BMAJ(IFIELD) = BMA * 3600.0
            BMIN(IFIELD) = BMI * 3600.0
C                                       Scale
            XBMAJ = BMAJ(IFIELD)
            CALL METSCA (XBMAJ, PREFIX, IERR)
            XBMIN = BMIN(IFIELD)
            IF (XBMAJ.NE.0.0) XBMIN = XBMIN * XBMAJ / BMAJ(IFIELD)
            WRITE (MSGTXT,1120) IFIELD, XBMAJ, XBMIN, PREFIX,
     *         BPA(IFIELD)
            CALL MSGWRT (4)
            END IF
         BMINAR = MIN (BMINAR, BMAJ(IFIELD)*BMIN(IFIELD))
C                                       force beam to header for now
         CALL IMGOPN (CNAME(IFIELD), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.PRODUCT', TYPE, IDIM,
     *      HPROD, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         IF ((HPROD.NE.PRODCT) .OR. (HBMAJ(IFIELD).LE.0.0) .OR.
     *      (HBMIN(IFIELD).LE.0.0)) THEN
            HBMAJ(IFIELD) = BMA
            HBMIN(IFIELD) = BMI
            HBPA(IFIELD) = BPA(IFIELD)
            END IF
         CALL IMPUT (CNAME(IFIELD), 'BEAM.PRODUCT', OOAINT, IDIM,
     *      PRODCT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BMAJ', OOARE, IDIM, BMA,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BMIN', OOARE, IDIM, BMI,
     *      CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BPA', OOARE, IDIM,
     *      BPA(IFIELD), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMGCLO (CNAME(IFIELD), IERR)
         IF (IERR.NE.0) GO TO 995
 200     CONTINUE
C                                       Minimum CC flux
      PCCMIN = 1.0E12
      NCCMIN = 1.0E12
      FFIELD = .TRUE.
      AFIELD = .TRUE.
C                                       Declare CCFLUX, CCTOTAL header
C                                       keywords for the image class
      CLNAME = 'IMAGE'
      KEYW = 'CCFLUX'
      CALL OBVHKW (CLNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 995
      KEYW = 'CCTOTAL'
      CALL OBVHKW (CLNAME, KEYW, OOARE, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLNINI: ERROR OPENING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('Field',I5,' use FWHM= ',F8.3,' x ',F8.3,1X,A,
     *   'arcsec, PA=',F7.1)
 1080 FORMAT ('BOX',I5,' FIELD',I5,' ILLEGAL - QUITTING')
      END
      SUBROUTINE CLNIM (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Does a full image plane Clark Clean, does initialization
C   Inputs:
C      NAME  C*?   The name of the Clean object.
C   Inputs from common:
C      DNAME       C(*)*32  Names of Dirty images
C      CNAME       C(*)*32  Names of residual/Clean images
C      DBNAME      C(*)*32  Name of dirty beam image.
C      GRID        C*32     Name of grid object for image plane Clean.
C      TRANFN      C*32     Name of transfer function (FT of beam) for
C                           image plane Clean.
C      WORK1       C*32     Scratch image
C      WORK2       C*32     Scratch image
C      CHANN       I        Frequency channel to be cleaned
C      NCLNG       I(*)     Number of previous components per field.
C      NXBEM       I(*)     Number of cells in "x" dimension of beam
C      NYBEM       I(*)     Number of cells in "y" dimension of beam
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   NAXIS(7), IDIM(7), DUMMY, LFIELD
      REAL      ABSMAX
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Initialize
      CALL CLNINI (NAME, 'IMAGE', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Transfer fn
C                                       Create scratch object
      IF (TRANFN.EQ.' ') THEN
         TRANFN = 'Image Clean transfer fn.'
C                                       Define TRANFN
         CALL FILL (7, 0, NAXIS)
         NAXIS(1) = NYBEM(1) * 2
         NAXIS(2) = NXBEM(1) / 2 + 1
         CALL IMGSCR (TRANFN, NAXIS, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Save name
         IDIM(1) = LEN (TRANFN)
         IDIM(2) = 1
         IDIM(3) = 0
         CALL CLNPUT (NAME, 'TRANFN', OOACAR, IDIM, DUMMY, TRANFN,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Do FFT, Beam plane selected in
C                                       CLNINI.
      CALL ARRFFT (3, DBNAME(1), WORK1, TRANFN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Sum components
      CALL CLCSUM
C                                       Initialize residual
      IF (NCLNG(1).GE.1) THEN
         WRITE (MSGTXT,1200) NCLNG(1)
         CALL MSGWRT (4)
C                                       Subtract initial components
C                                       Grid
         CALL CLGRID (IERR)
         IF (IERR.NE.0) GO TO 995
C                                       FFT
         CALL ARRFFT (-1, GRID, WORK1, WORK2, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Subtract components from dirty
         CALL ARRADD (DNAME(1), WORK2, CNAME(1), IERR)
         IF (IERR.NE.0) GO TO 995
C                                       If not restarting Clean
C                                       copy dirty to residual
      ELSE
         CALL ARRCOP (DNAME(1), CNAME(1), IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       TV
      FINISH = NCLNG(1).GE.CLNLIM
      LFIELD = 0
      CALL CLNTV (NAME, LFIELD, IERR)
      IF (IERR.GT.1) GO TO 995
C                                       Set initial RESMAX
      CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG, IERR)
      IF (IERR.NE.0) GO TO 995
      RESMAX = ABS (ACTRES)
C                                       Done?
      IF (FINISH) GO TO 200
C                                       Begin Clean loop
C                                       Find components
 100     CALL CLNCYC (NAME, 0, IERR)
         PCCMIN = NCCMIN
         NCCMIN = 1.E12
C                                       Any more components?
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 200
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Subtract components
C                                       Grid, FT, multiply by transffer
C                                       function
         CALL CLGRID (IERR)
         IF (IERR.NE.0) GO TO 995
C                                       FFT
         CALL ARRFFT (-1, GRID, WORK1, WORK2, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Subtract components from dirty
         CALL ARRADD (DNAME(1), WORK2, CNAME(1), IERR)
         IF (IERR.NE.0) GO TO 995
C                                       TELL
         IF (.NOT.FINISH) THEN
            CALL CLNTEL (NAME, ' ', IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
C                                       TV
         IF (.NOT.FINISH) THEN
            LFIELD = 0
            CALL CLNTV (NAME, LFIELD, IERR)
            IF (IERR.GT.1) GO TO 995
            END IF
C                                       Done?
         IF (.NOT.FINISH) GO TO 100
C                                       Clean done - scale residuals?
 200  IF (DOSCAL) CALL CLSCRS (IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Restore?
      IF (.NOT.NOREST) THEN
         CALL CLREST (IERR)
         IF (IERR.NE.0) GO TO 995
C                                       TV
         LFIELD = 0
         FINISH = .TRUE.
         CALL CLNTV (NAME, LFIELD, IERR)
         IERR = 0
         END IF
C                                       Save GRID name
      IF (GRID.NE.' ') THEN
         IDIM(1) = LEN (GRID)
         IDIM(2) = 1
         IDIM(3) = 0
         CALL CLNPUT (NAME, 'GRID', OOACAR, IDIM, DUMMY, GRID, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      GO TO 999
C                                       Error
 995  MSGTXT = 'CLNIM : ERROR CLEANING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('Restarting Clean at',I9,' components')
      END
      SUBROUTINE CLNUV (NAME, DOMAX, IERR)
C-----------------------------------------------------------------------
C   Public
C   Full ungridded uv plane Cotton-Schwab Clean
C   Inputs:
C      NAME    C*?   The name of the Clean object.
C      DOMAX   L     Read through image to set header max min
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      LOGICAL   DOMAX
      INTEGER   IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   IDIM(7), I, SUMCC, DUMMY, IFIELD, LFIELD, TFIELD, TVFLD,
     *   MSGSAV, TYPE, DIM(7), NFILT
      REAL      ABSMAX, XFLUX, RTEMP, FF, BMSCAL(MAXFLD), CLOFNB, TX,
     *   MFMULT
      LOGICAL   FMJCYC
      CHARACTER CC*4, CDUMMY*1, PREFIX*5
      DATA CC /'CC  '/
C-----------------------------------------------------------------------
      SDINOW = SDICLN
      IERR = 0
      NFILT = 0
C                                       Initialize
      CALL CLNINI (NAME, 'UV', IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Sum components
      CALL CLCSUM
C                                       Set up for model subtraction
      IDIM(1) = LEN (CC)
      IDIM(2) = 1
      CALL OUVPUT (UVDATA, 'MODMODEL', OOACAR, IDIM, DUMMY, CC, IERR)
      IF (IERR.NE.0) GO TO 995
      IDIM(1) = MFIELD
      CALL OUVPUT (UVDATA, 'MODCCBEG', OOAINT, IDIM, NSUBG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Initialize residual
      CALL ISUM (MFIELD, NCLNG, SUMCC)
C                                       is a sort required?
C                                       component subtraction method
      IF ((CLNLIM.GT.0) .OR. (SUMCC.GT.0)) THEN
         CALL CLNSRT (NAME, CNAME(1), UVDATA, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Is copy needed of input data
      IF (INDATA.EQ.'-') THEN
         MSGTXT = 'QCLEAN: Make copy of data for filtering'
         CALL MSGWRT (8)
         INDATA = 'Saved copy of initial UV data'
         CALL UVRSCR (UVDATA, INDATA, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      IF (((CLNLIM.GT.0) .OR. (SUMCC.GT.0)) .AND. (INDATA.NE.' ')) THEN
         CALL CLNSRT (NAME, CNAME(1), INDATA, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       subtract initial components
      IF (SUMCC.GE.1) THEN
         WRITE (MSGTXT,1000) SUMCC
         CALL MSGWRT (4)
         CALL CLSCAL (.TRUE., IERR)
         IF (IERR.NE.0) GO TO 995
C                                       ALL okay: update first # to
C                                       subtract
         IF (ALLOK.GT.1) THEN
            DO 10 I = 1,MFIELD
               NSUBG(I) = NCLNG(I) + 1
 10            CONTINUE
            IDIM(1) = MFIELD
            CALL OUVPUT (UVDATA, 'MODCCBEG', OOAINT, IDIM, NSUBG,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       compress and filter
         ELSE
            IF ((SUMCC.LT.CLNLIM) .AND. (INDATA.NE.' ') .AND.
     *         (CCFILT(1).NE.0.0)) THEN
C                                       Compress the CC files
               CALL CLMERG (IERR)
               IF (IERR.NE.0) GO TO 995
C                                       Filter CCs
               CALL CLFILT (IERR)
               IF (IERR.NE.0) GO TO 995
               IDIM(1) = MFIELD
               IDIM(2) = 1
               CALL FILL (MAXFLD, 1, NSUBG)
               CALL OUVPUT (UVDATA, 'MODCCBEG', OOAINT, IDIM, NSUBG,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
               CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
C                                       Initialize residual
               CALL ISUM (MFIELD, NCLNG, SUMCC)
               WRITE (MSGTXT,1005) SUMCC
               CALL MSGWRT (4)
               NFILT = NFILT + 1
               END IF
C                                       Subtract initial components
            CALL UVSUBM (UVDATA, UVDATA, 0, MFIELD, CNAME, UVCHAN,
     *         NCHAV, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
         END IF
C                                       Make new residual image(s)
      CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *   .FALSE., UVCHAN, NCHAV, CHANN, IERR)
      IF (IERR.NE.0) GO TO 995
      MFMULT = 1.05
C                                       Update image info.
      CALL CLIINF (IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Set initial RESMAX
      CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG, IERR)
      IF (IERR.NE.0) GO TO 995
      RESMAX = ABS (ACTRES)
      IF (RESMAX.EQ.0.0) RESMAX = ELIMAX
C                                       TV
      FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MFMULT*MINFLX) .OR.
     *   ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
C                                       TELL
      IF (.NOT.FINISH) THEN
         CALL CLNTEL (NAME, UVDATA, IERR)
         IF (IERR.NE.0) GO TO 995
         FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MFMULT*MINFLX) .OR.
     *      ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
         END IF
      IF (FINISH) THEN
         IF (SUMCC.GE.CLNLIM) THEN
            IF (SUMCC.GT.CLNLIM) THEN
               WRITE (MSGTXT,1010) SUMCC, 'exceeds', CLNLIM
            ELSE IF (SUMCC.GT.0) THEN
               WRITE (MSGTXT,1010) SUMCC, 'reaches', CLNLIM
            ELSE
               MSGTXT = 'No Cleaning requested'
               END IF
            CALL MSGWRT (4)
            END IF
         IF (RESMAX.LE.MFMULT*MINFLX) THEN
            XFLUX = RESMAX
            ABSMAX = MFMULT*MINFLX
            IF (RESMAX.NE.0.0) THEN
                CALL METSCA (XFLUX, PREFIX, I)
                ABSMAX = ABSMAX * XFLUX / RESMAX
            ELSE
                CALL METSCA (ABSMAX, PREFIX, I)
               END IF
            WRITE (MSGTXT,1011) XFLUX, ABSMAX, PREFIX
            CALL MSGWRT (4)
            END IF
         IF ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) THEN
            XFLUX = ACTRES
            CALL METSCA (XFLUX, PREFIX, I)
            WRITE (MSGTXT,1012) XFLUX, PREFIX
            CALL MSGWRT (4)
            END IF
         END IF
      CALL RFILL (MFIELD, 1.0, BMSCAL)
C                                       hide grubby in another layer
      IF (OVRLAP.GE.2) THEN
         CALL CLNUV2 (NAME, NFILT, FMJCYC, IERR)
         IF ((IERR.EQ.0) .AND. (.NOT.FINISH) .AND. (OVRLAP.EQ.1))
     *      CALL CLNUV1 (NAME, NFILT, FMJCYC, IERR)
      ELSE
         CALL CLNUV1 (NAME, NFILT, FMJCYC, IERR)
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Compress the CC files
C                                       Initialize residual
      CALL ISUM (MFIELD, NCLNG, SUMCC)
C                                       Filter CCs
      IF (SUMCC.GT.0) THEN
         IF ((INDATA.NE.' ') .AND. (CCFILT(1).NE.0.0)) THEN
            CALL DOFILT (.TRUE., FMJCYC, SUMCC, IERR)
            IF (IERR.NE.0) GO TO 995
            WRITE (MSGTXT,1200) SUMCC
            CALL MSGWRT (4)
C                                       just merge them
         ELSE IF ((INDATA.NE.' ') .OR. (OVRLAP.GT.0) .OR.
     *      (SDICLN.GT.-2)) THEN
            CALL CLMERG (IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
         END IF
C                                       Finish only some of the fields?
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CLNGET (NAME, 'NFLDRSTR', TYPE, DIM, I, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         I = MFIELD
         IERR = 0
         END IF
C                                       restore all resolutions instead
      I = MFIELD
      MMFELD = MFIELD
      IF ((I.GT.0) .AND. (I.LT.MFIELD)) THEN
         WRITE (MSGTXT,1205) I, MFIELD
         CALL MSGWRT (5)
         MFIELD = I
         I = I / NFPRES
         DIM(1) = 1
         DIM(2) = 1
         CALL OPUT (CNAME(1), 'NUMRES', OOAINT, DIM, I, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Make new residual image
      IF (.NOT.FMJCYC) THEN
         CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *      .FALSE., UVCHAN, NCHAV, CHANN, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Clean done - scale residuals?
      IF (DOSCAL) THEN
         CALL CLSCRS (IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Restore?
      IF (.NOT.NOREST) THEN
         CALL CLREST (IERR)
         IF (IERR.NE.0) GO TO 995
         IF (OVRLAP.GT.0) THEN
            CALL CLOVER (IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
         CALL CLSCAL (.FALSE., IERR)
         IF (IERR.NE.0) GO TO 995
         IF (DOMAX) CALL CLMXMN (IERR)
C                                       Find max for overlap 2 mode
         IF (MFIELD.GT.1) THEN
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.NE.0) TVFLD = 0
            IF (TVFLD.GT.0) THEN
               CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               RESMAX = -1.0E-9
               RTEMP = -1.0E-9
               IFIELD = 0
               DO 210 LFIELD = 1,MFIELD
                  IF (IGNORE(LFIELD).GT.-0.5) THEN
                     TX = MAX (RSSMAX(LFIELD), ELIMXF(LFIELD))
                     FF = CLOFNB (TX, RSSAVG(LFIELD), BMSCAL(LFIELD))
                     IF ((FF.GT.RTEMP) .AND.
     *                  (RSSMAX(LFIELD).GT.MNFFLX(LFIELD))) THEN
                        IFIELD = LFIELD
                        RTEMP = FF
                        RESMAX = RSSMAX(LFIELD)
                        END IF
                     END IF
 210              CONTINUE
               IF (IFIELD.GT.0) TVFLD = IFIELD
               DIM(1) = 1
               DIM(2) = 1
               CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            END IF
C                                       TV
         TFIELD = 0
         FINISH = .TRUE.
         CALL CLNTV (NAME, TFIELD, IERR)
C                                       display total flux by field
         DIM(1) = 1
         DIM(2) = 1
         DO 220 LFIELD = 1,MFIELD
            XFLUX = FLUXG(LFIELD)
            CALL OPUT (CNAME(LFIELD), 'CCFLUX', OOARE, DIM, XFLUX,
     *         CDUMMY, IERR)
            CALL OPUT (CNAME(LFIELD), 'CCTOTAL', OOARE, DIM, TFLUXG,
     *         CDUMMY, IERR)
            CALL METSCA (XFLUX, PREFIX, I)
            WRITE (MSGTXT,1210) LFIELD, XFLUX, PREFIX
            CALL MSGWRT (3)
 220        CONTINUE
C                                       check max min anyway
      ELSE
         CALL CLSCAL (.FALSE., IERR)
         IF (IERR.NE.0) GO TO 995
         IF (DOMAX) CALL CLMXMN (IERR)
         END IF
      CALL CLCGEX (IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Save GRID name
      IF (GRID.NE.' ') THEN
         IDIM(1) = LEN (TRANFN)
         IDIM(2) = 1
         IDIM(3) = 0
         CALL CLNPUT (NAME, 'GRID', OOACAR, IDIM, DUMMY, GRID, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      GO TO 999
C                                       Error
 995  MSGTXT = 'CLNUV : ERROR CLEANING ' // NAME
      IF (IERR.NE.99) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Restarting Clean at',I9,' components')
 1005 FORMAT ('Resuming Clean with',I9,' filtered components')
 1010 FORMAT ('Total Clean components',I9,1X,A,' limit',I8)
 1011 FORMAT ('Peak residual in window',F8.3,' less than limit',F8.3,1X,
     *   A5,'Jy')
 1012 FORMAT ('Peak residual in window',F8.3,' less than zero ',A5,'Jy')
 1200 FORMAT ('Removing ',I9,' remaining components from initial data')
 1205 FORMAT ('Restoring only',I5,' of the',I5,' fields')
 1210 FORMAT ('Field',I5,' final Clean flux',F10.3,1X,A5,'Jy')
      END
      SUBROUTINE CLNCYC (NAME, IFIELD, IERR)
C-----------------------------------------------------------------------
C   Public
C   Does a single cycle of Clark or SDI Clean.  Residual assumed in
C   CNAME.
C   Inputs:
C      NAME     C*?    The name of the Clean object.
C      IFIELD   I      Field to do, 0 => all at once
C   Inputs from common:
C      NCLNG    I(*)   Number of components per field.
C      CLNLIM   I      Maximum number of clean components desired.
C   Output to common:
C      FINISH   L      TRUE if Clean done (only on IFIELD=0,1)
C   Output:
C      IERR     I      Error return code, 0=OK,
C                      -1 => no further components (only on IFIELD=0,1)
C                      -2 => no windows in this field
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IFIELD, IERR
C
      CHARACTER CDUMMY*1
      LOGICAL   APOPEN, DOIT
      INTEGER   KAP, LFIELD, DIM(7), TOTCMP, JERR, LF1, LF2
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      IERR = 0
      APOPEN = .FALSE.
C                                       SC file times
      CALL TOUCH
C                                       Clean common active?
      IF (.NOT.ACTIVE) THEN
         IERR = 5
         MSGTXT = 'CLNCYC: CLEAN INACTIVE'
         GO TO 990
         END IF
C                                       Any more components?
      CALL ISUM (MFIELD, NCLNG, TOTCMP)
      IF ((TOTCMP.GE.CLNLIM) .AND. ((FFIELD) .OR. (IFIELD.LE.0))) THEN
         FINISH = .TRUE.
         IERR = -1
         GO TO 999
         END IF
C                                       Any to do ?
      IF ((IFIELD.GT.0) .AND. (NBOXES(IFIELD).LE.0)) THEN
         IERR = -2
         GO TO 999
         END IF
C                                       Residual histogram
      IF (NEDHIS.GE.0) THEN
         CALL CLRHIS (NAME, NEDHIS, NEDH1, IERR)
         IF ((NEDHIS.EQ.0) .AND. (FFIELD) .AND. (IERR.EQ.-2))
     *      FINISH = .TRUE.
         IF (IERR.LT.0) GO TO 998
         IF (IERR.NE.0) GO TO 995
         NEDHIS = -1
         END IF
C                                       Determine PATCH and MAPLIM
      CALL CDECID (IFIELD, DOIT)
      IF (.NOT.DOIT) THEN
         IERR = -2
         GO TO 999
         END IF
C                                       Clark Clean
      IF (SDINOW.LE.0) THEN
C                                       Load beam patch.
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CLNCYC: DID NOT GET AP MEMORY'
            CALL MSGWRT (8)
            IERR = 9
            GO TO 995
            END IF
         CALL APOBJ ('OPEN', 'CLNCYC', IERR)
         IF (IERR.NE.0) GO TO 995
         APOPEN = .TRUE.
         CALL CLBSHV (IFIELD, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Load residuals.
         CALL CLMPAC (IFIELD, IERR)
         IF (IERR.EQ.-1) THEN
            IERR = -2
            GO TO 998
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Find components.
         CALL CLACLN (IFIELD, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL QRLSE
         CALL APOBJ ('CLOS', 'CLNCYC', IERR)
         APOPEN = .FALSE.
C                                       Find SDI components
      ELSE
         CALL CLASDI (IFIELD, IERR)
         IF (IERR.EQ.-1) THEN
            IERR = -2
            GO TO 998
            END IF
         IF (IERR.NE.0) GO TO 995
C                                       Back to BGC if non-force
         IF (SDICLN.EQ.1) SDICLN = 0
         END IF
C                                       Save fluxes in imaging commons
      CALL OUSETF (TFLUXG, MFIELD, FLUXG)
C                                       Change status to write (from
C                                       destroy on fail if set)
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (NAME, 'TFLUX', OOARE, DIM, TFLUXG, CDUMMY, IERR)
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      DO 200 LFIELD = LF1,LF2
         CALL IMGOPN (CNAME(LFIELD), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMCDES (CNAME(LFIELD), 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Update number of iterations
         CALL IMPUT (CNAME(LFIELD), 'BEAM.NITER', OOAINT, DIM,
     *      NCLNG(LFIELD), CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OPUT (CNAME(LFIELD), 'CFLUX', OOARE, DIM, FLUXG(LFIELD),
     *      CDUMMY, IERR)
         CALL IMGCLO (CNAME(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 995
 200     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLNCYC: ERROR CLEANING ' // NAME
      CALL MSGWRT (8)
 998  IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('CLOS', 'CLNCYC', JERR)
         END IF
C
 999  RETURN
      END
C-----------------------------------------------------------------------
C
C   Private functions:
C
C-----------------------------------------------------------------------
      SUBROUTINE CLNUV1 (NAME, NFILT, FMJCYC, IERR)
C-----------------------------------------------------------------------
C   Private - for OVERLAP <= 1.
C   Full ungridded uv plane Cotton-Schwab Clean
C   Inputs:
C      NAME    C*?   The name of the Clean object.
C   In/out:
C      NFILT   I     Number of filterings
C   Output:
C      FMJCYC  L     T => all images current w UV work file
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      LOGICAL   FMJCYC
      INTEGER   NFILT, IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   IDIM(7), I, SUMCC, IFIELD, LFIELD, LERR, TFIELD, NZERO,
     *   FIELDS(MAXFLD), TVFLD, MSGSAV, TYPE, DIM(7), NUMCYC, NGAUSS
      REAL      ORESMX, ABSMAX, XFLUX, RTEMP, FF, BMSCAL(MAXFLD),
     *   TSSMAX(MAXFLD), BMSMAX, CLOFNB, LBMSCP, TX, MFMULT
      LOGICAL   FILTRS
      CHARACTER CDUMMY*1, PREFIX*5
C-----------------------------------------------------------------------
      MFMULT = 1.05
      LBMSCP = 0.0
      NUMCYC = 0
      NZERO = 0
      TFIELD = 0
      NGAUSS = MFIELD / NFPRES
      IF (AUTOBX(1).GT.0.5) THEN
         CALL CLABOX (NAME, TFIELD, IERR)
         IF (IERR.GT.1) GO TO 995
         END IF
      CALL BOXFIX (NAME, MFIELD, NGAUSS, CNAME, IMSIZE, DBGAIP, .TRUE.,
     *   NBOXES, WIN, UNBOXS, UNWIN, IERR)
      CALL CLNTV (NAME, TFIELD, IERR)
      IF (IERR.GT.1) GO TO 995
      CALL BOXFIX (NAME, MFIELD, NGAUSS, CNAME, IMSIZE, DBGAIP, .TRUE.,
     *   NBOXES, WIN, UNBOXS, UNWIN, IERR)
C                                       Check maxima w new windows
      CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG, IERR)
      IF (IERR.NE.0) GO TO 995
      RESMAX = ABS (ACTRES)
      FMJCYC = .TRUE.
C                                       Begin Clean loop
 100  IF (.NOT.FINISH) THEN
C                                       scaling for multi-res
         BMSMAX = -1.0
         IF (BMSCP.GE.0.0) THEN
            IF (BMSCP.NE.LBMSCP) THEN
               WRITE (MSGTXT,1100) BMSCP
               CALL MSGWRT (2)
               LBMSCP = BMSCP
               END IF
            DO 105 LFIELD = 1,MFIELD
               RTEMP = BMAJ(LFIELD) * BMIN(LFIELD)
               IF (RTEMP.LE.0.0) RTEMP = 1.0
               BMSCAL(LFIELD) = 1.0 / (RTEMP**BMSCP)
               BMSMAX = MAX (BMSMAX, BMSCAL(LFIELD))
 105           CONTINUE
            END IF
C                                       3DIMAG - 1 field at a time
         IF ((.NOT.ONEBEM) .OR. (MFIELD.GT.MAXAFL)) THEN
            ORESMX = RESMAX
            NEDHIS = 0
            NEDH1 = 0
            NCCMIN = 1.E12
            FFIELD = .TRUE.
            AFIELD = .FALSE.
C                                       sort fields
            CALL RCOPY (MFIELD, RSSMAX, TSSMAX)
            DO 120 LFIELD = 1,MFIELD
               ABSMAX = -1.0E-9
               I = 0
               DO 110 IFIELD = 1,MFIELD
                  IF (TSSMAX(IFIELD)*BMSCAL(IFIELD).GT.ABSMAX) THEN
                     I = IFIELD
                     ABSMAX = TSSMAX(I) * BMSCAL(I)
                     END IF
 110              CONTINUE
               FIELDS(LFIELD) = I
               TSSMAX(I) = MIN (-TSSMAX(I), -2.0E-9)
 120           CONTINUE
            DO 125 LFIELD = 1,MFIELD
               IFIELD = FIELDS(LFIELD)
C                                       Find components
               CALL CLNCYC (NAME, IFIELD, IERR)
               RESMAX = ORESMX
               MFMULT = 1.0
C                                       Any more components?
               IF (IERR.LT.0) THEN
                  IF ((FFIELD) .AND. (IERR.LE.-1) .AND. (IERR.GE.-2)
     *               .AND. (FINISH)) THEN
                     IERR = 0
                     GO TO 200
                  ELSE
                     IERR = 0
                     GO TO 125
                     END IF
                  END IF
               IF (IERR.NE.0) GO TO 995
               FFIELD = .FALSE.
 125           CONTINUE
            IF (FFIELD) THEN
               MSGTXT = 'No fields with components: quitting'
               CALL MSGWRT (3)
               FINISH = .TRUE.
               DO 130 I = 1,MFIELD
                  IF (NBOXES(I).GT.0) GO TO 200
 130              CONTINUE
               MSGTXT = 'No fields have boxes in which to Clean'
               CALL MSGWRT (3)
               GO TO 200
               END IF
C                                       How many components?
            IDIM(1) = MFIELD
            IDIM(2) = 1
            CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Subtract components
            CALL UVSUBM (UVDATA, UVDATA, 0, MFIELD, CNAME, UVCHAN,
     *         NCHAV, IERR)
            IF (IERR.NE.0) GO TO 995
            PCCMIN = NCCMIN
C                                       Make new residual images
            CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *         .FALSE., UVCHAN, NCHAV, CHANN, IERR)
            IF (IERR.NE.0) GO TO 995
            MFMULT = 1.05
C                                       Old way: all fields at once
         ELSE
C                                       Find components
            NEDHIS = 0
            NEDH1 = 0
            CALL CLNCYC (NAME, 0, IERR)
            PCCMIN = NCCMIN
            NCCMIN = 1.E12
C                                       Any more components?
            IF (IERR.LT.0) THEN
               FINISH = .TRUE.
               IERR = 0
               DO 140 I = 1,MFIELD
                  IF (NBOXES(I).GT.0) GO TO 200
 140              CONTINUE
               MSGTXT = 'No fields have boxes in which to Clean'
               CALL MSGWRT (3)
               GO TO 200
               END IF
            IF (IERR.NE.0) GO TO 995
C                                       How many components?
            IDIM(1) = MFIELD
            IDIM(2) = 1
            CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Subtract components
            CALL UVSUBM (UVDATA, UVDATA, 0, MFIELD, CNAME, UVCHAN,
     *         NCHAV, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Make new residual image
            NUMCYC = 0
            CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *         .FALSE., UVCHAN, NCHAV, CHANN, IERR)
            IF (IERR.NE.0) GO TO 995
            MFMULT = 1.05
            END IF
C                                       Any more components?
         CALL ISUM (MFIELD, NCLNG, SUMCC)
C                                       Reset initial RESMAX
         IF (FMJCYC) THEN
            CALL CLRMAX (0, MFIELD, ABSMAX, XFLUX, RSSMAX, RSSAVG, IERR)
            IF (IERR.NE.0) GO TO 995
            RESMAX = ABS (XFLUX)
            IF ((ACTRES.GT.0.0) .OR. (MINFLX.GE.0.0)) ACTRES = XFLUX
            END IF
         IF (RESMAX.EQ.0.0) RESMAX = ELIMAX
         FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MFMULT*MINFLX) .OR.
     *      ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
C                                       TELL
         IF (.NOT.FINISH) THEN
            CALL CLNTEL (NAME, UVDATA, IERR)
            IF (IERR.NE.0) GO TO 995
            FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MFMULT*MINFLX)
     *         .OR. ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
            END IF
         IF (FINISH) THEN
            IF (SUMCC.GE.CLNLIM) THEN
               IF (SUMCC.GT.CLNLIM) THEN
                  WRITE (MSGTXT,1010) SUMCC, 'exceeds', CLNLIM
               ELSE IF (SUMCC.GT.0) THEN
                  WRITE (MSGTXT,1010) SUMCC, 'reaches', CLNLIM
               ELSE
                  MSGTXT = 'No Cleaning requested'
                  END IF
               CALL MSGWRT (4)
               END IF
            IF (RESMAX.LE.MFMULT*MINFLX) THEN
               XFLUX = RESMAX
               ABSMAX = MFMULT*MINFLX
               IF (RESMAX.NE.0.0) THEN
                  CALL METSCA (XFLUX, PREFIX, I)
                  ABSMAX = ABSMAX * XFLUX / RESMAX
               ELSE
                  CALL METSCA (ABSMAX, PREFIX, I)
                  END IF
               WRITE (MSGTXT,1011) XFLUX, ABSMAX, PREFIX
               CALL MSGWRT (4)
               END IF
            IF ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) THEN
               XFLUX = ACTRES
               CALL METSCA (XFLUX, PREFIX, I)
               WRITE (MSGTXT,1012) XFLUX, PREFIX
               CALL MSGWRT (4)
               END IF
            END IF
C                                       TV
         IF (FMJCYC) THEN
            TFIELD = 0
            IF (AUTOBX(1).GT.0.5) THEN
               CALL CLABOX (NAME, TFIELD, IERR)
               IF (IERR.GT.1) GO TO 995
               END IF
            CALL BOXFIX (NAME, MFIELD, NGAUSS, CNAME, IMSIZE, DBGAIP,
     *         .TRUE., NBOXES, WIN, UNBOXS, UNWIN, IERR)
            CALL CLNTV (NAME, TFIELD, IERR)
            IF (IERR.GT.1) GO TO 995
            CALL BOXFIX (NAME, MFIELD, NGAUSS, CNAME, IMSIZE, DBGAIP,
     *         .TRUE., NBOXES, WIN, UNBOXS, UNWIN, IERR)
C                                       Check maxima w new windows
            IF (.NOT.FINISH) THEN
C                                       filter
               IF ((IERR.EQ.-3) .AND. (INDATA.NE.' ') .AND.
     *            (CCFILT(1).NE.0.0)) THEN
                  LERR = IERR
                  CALL DOFILT (.TRUE., FMJCYC, SUMCC, IERR)
                  IF (IERR.NE.0) GO TO 995
                  NFILT = NFILT + 1
                  WRITE (MSGTXT,1005) SUMCC
                  CALL MSGWRT (4)
                  NUMCYC = 0
                  RTEMP = CCFILT(1)
                  CCFILT(1) = 0.0
C                                       Find max for overlap 2 mode
                  IF (OVRLAP.GE.2) THEN
                     MSGSAV = MSGSUP
                     MSGSUP = 32000
                     CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD,
     *                  CDUMMY, IERR)
                     MSGSUP = MSGSAV
                     IF (IERR.NE.0) TVFLD = 0
                     IF (TVFLD.GT.0) THEN
                        CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX,
     *                     RSSAVG, IERR)
                        IF (IERR.NE.0) GO TO 995
                        RESMAX = -1.0E-9
                        RTEMP = -1.0E-9
                        IFIELD = 0
                        DO 180 LFIELD = 1,MFIELD
                           IF (IGNORE(LFIELD).GT.-0.5) THEN
                              TX = MAX (RSSMAX(LFIELD), ELIMXF(LFIELD))
                              FF = CLOFNB (TX, RSSAVG(LFIELD),
     *                           BMSCAL(LFIELD))
                              IF ((RSSMAX(LFIELD).GT.MNFFLX(LFIELD))
     *                           .AND. (FF.GT.RTEMP)) THEN
                                 IFIELD = LFIELD
                                 RTEMP = FF
                                 RESMAX = RSSMAX(LFIELD)
                                 END IF
                              END IF
 180                       CONTINUE
                        IF (IFIELD.GT.0) TVFLD = IFIELD
                        DIM(1) = 1
                        DIM(2) = 1
                        CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM,
     *                     TVFLD, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 995
                        END IF
                     END IF
                  TFIELD = 0
                  IF (AUTOBX(1).GT.0.5) THEN
                     CALL CLABOX (NAME, TFIELD, IERR)
                     IF (IERR.GT.1) GO TO 995
                     END IF
                  CALL CLNTV (NAME, TFIELD, IERR)
                  IF (IERR.GT.1) GO TO 995
                  CCFILT(1) = RTEMP
                  IERR = LERR
                  END IF
               CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               RESMAX = ABS (ACTRES)
               END IF
            END IF
         GO TO 100
         END IF
C                                       Compress the CC files
C                                       Initialize residual
 200  FILTRS = .FALSE.
      IF ((INDATA.NE.' ') .AND. (CCFILT(1).NE.0.0) .AND. (NFILT.LT.3))
     *   THEN
         MSGSUP = 32000
         CALL OGET (NAME, 'CCFILTRS', TYPE, DIM, FILTRS, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            FILTRS = .FALSE.
            END IF
         END IF
      IF (FILTRS) THEN
         NFILT = NFILT + 5
         MSGTXT = 'Forcing a filter and resuming Clean'
         CALL MSGWRT (5)
         FINISH = .FALSE.
         QUIT = .FALSE.
         DO 205 I = 1,MAXFLD
            IF (IGNORE(I).GT.-1.5) IGNORE(I) = 1.0
 205        CONTINUE
         CALL DOFILT (.TRUE., FMJCYC, SUMCC, IERR)
         IF (IERR.NE.0) GO TO 995
         WRITE (MSGTXT,1005) SUMCC
         CALL MSGWRT (4)
         GO TO 100
         END IF
      GO TO 999
C
C                                       Error
 995  MSGTXT = 'CLNUV1 : ERROR CLEANING ' // NAME
      IF (IERR.NE.99) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('Resuming Clean with',I9,' filtered components')
 1010 FORMAT ('Total Clean components',I9,1X,A,' limit',I8)
 1011 FORMAT ('Peak residual in window',F8.3,' less than limit',F8.3,1X,
     *   A5,'Jy')
 1012 FORMAT ('Peak residual in window',F8.3,' less than zero ',A5,'Jy')
 1100 FORMAT ('CLNUV1: Beam scaling power now',F6.3)
      END
      SUBROUTINE CLNUV2 (NAME, NFILT, FMJCYC, IERR)
C-----------------------------------------------------------------------
C   Private - for OVERLAP >= 2
C   Full ungridded uv plane Cotton-Schwab Clean
C   Inputs:
C      NAME    C*?   The name of the Clean object.
C   In/out:
C      NFILT   I     Number of filterings
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   NFILT, IERR
      LOGICAL   FMJCYC
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   SZLIST
      PARAMETER (SZLIST = 10)
      INTEGER   IDIM(7), I, SUMCC, IFIELD, LFIELD, LERR, TFIELD,
     *   TVFLD, MSGSAV, TYPE, DIM(7), NUMCYC, NFORCE, CURENT(MAXFLD),
     *   NZERO, KFIELD(SZLIST), JCOUNT, MXLIST, APSIZE, NEED, PFIELD,
     *   PASSBX, JFIELD, NNBEST, NONE(1)
      REAL      ORESMX, ABSMAX, XFLUX, RTEMP, BMSCAL(MAXFLD), BMSMAX,
     *   LBMSCP, BEST, PBEST, CLOFNB, ABEST, TX, SWITCH, RATIO
      LOGICAL   DOCLEN, WASOME, FORCED, EXIST, FILTRS, DOIMG
      CHARACTER CDUMMY*1, PREFIX*5
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE NNBEST
      DATA NNBEST /0/
C-----------------------------------------------------------------------
      NONE(1) = 0
      LBMSCP = 0.0
      NUMCYC = 0
      FORCED = .FALSE.
      NFORCE = 0
      NZERO = 0
      CALL FILL (MFIELD, 2, CURENT)
C                                       how many fields at once
C                                       what will fit???
      NEED = (UVBFSS + 1900) / 1024 + 1
      APSIZE = KAPWRD - NEED
      NEED = 256 * 65
      DO 10 LFIELD = 1,MFIELD
         NEED = MAX (2*IMSIZE(2,LFIELD)*(IMSIZE(1,LFIELD)/2+1), NEED)
 10      CONTINUE
      NEED = NEED / 1024 + 1
      NEED = APSIZE / NEED
      MXLIST = MIN (SZLIST, NEED)
C                                       is UV data set large???
      CALL OBFSIZ (UVDATA, EXIST, NEED, IERR)
      IF (IERR.NE.0) THEN
         MXLIST = 4
      ELSE
         NEED = NEED / 4
         IF (NEED.LT.KAPWRD) MXLIST = 4
         IF (NEED.LT.KAPWRD/3) MXLIST = 2
         IF (NEED.LT.KAPWRD/8) MXLIST = 1
         END IF
      MXLIST = MIN (MXLIST, MFIELD)
      MXLIST = MAX (1, MXLIST)
C                                       pick field
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD, CDUMMY, IERR)
      IF (IERR.NE.0) TVFLD = 0
      CALL CLNGET (NAME, 'OVRSWTCH', TYPE, DIM, OVRSW, CDUMMY, IERR)
      IF (IERR.NE.0) OVRSW = 0.0
C                                       multi-scale : no switch
      IF (NUMRES.GT.1) OVRSW = 0.0
      MSGSUP = MSGSAV
      IERR = 0
      IF (TVFLD.GT.0) THEN
C                                       scaling for multi-res
         IF (BMSCP.GE.0.0) THEN
            DO 80 LFIELD = 1,MFIELD
               RTEMP = BMAJ(LFIELD) * BMIN(LFIELD)
               IF (RTEMP.LE.0.0) RTEMP = 1.0
               BMSCAL(LFIELD) = 1.0 / (RTEMP**BMSCP)
 80            CONTINUE
            END IF
         CALL FNDMAX (NAME, BMSCAL, TVFLD, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      TFIELD = 0
      PASSBX = 0
      IF (AUTOBX(1).GT.0.5) THEN
         CALL CLABOX (NAME, TFIELD, IERR)
         IF (IERR.GT.1) GO TO 995
         END IF
      CALL CLNTV (NAME, TFIELD, IERR)
      IF (IERR.GT.1) GO TO 995
C                                       Check maxima w new windows
      CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       how many fields at once
      CALL CLRMS (MFIELD, NONE, RSSMAX, ELIMXF, RSSAVG, BMSCAL, RATIO)
      IF ((RATIO.GT.2.) .AND. (MXULST.GT.1)) THEN
         MXULST = MXULST - 1
         IF (RATIO.GT.4.) MXULST = MXULST - 1
         IF (RATIO.GT.8.) MXULST = MXULST - 1
         IF (RATIO.GT.12.) MXULST = MXULST - 1
         IF (RATIO.GT.16.) MXULST = MXULST - 1
         IF (MXULST.EQ.3) MXULST = 2
         MXULST = MAX (2, MXULST)
         END IF
      RESMAX = ABS (ACTRES)
      FMJCYC = .TRUE.
      WASOME = .FALSE.
C                                       DEBUG
      SWITCH = ABS (OVRSW)
C                                       Begin Clean loop
 100  IF (.NOT.FINISH) THEN
C                                       scaling for multi-res
         BMSMAX = -1.0
         IF (BMSCP.GE.0.0) THEN
            IF (BMSCP.NE.LBMSCP) THEN
               WRITE (MSGTXT,1100) BMSCP
               CALL MSGWRT (2)
               LBMSCP = BMSCP
               END IF
            DO 105 LFIELD = 1,MFIELD
               RTEMP = BMAJ(LFIELD) * BMIN(LFIELD)
               IF (RTEMP.LE.0.0) RTEMP = 1.0
               BMSCAL(LFIELD) = 1.0 / (RTEMP**BMSCP)
               BMSMAX = MAX (BMSMAX, BMSCAL(LFIELD))
 105           CONTINUE
            END IF
C                                       3DIMAG - 1 field & overlapped
         NCCMIN = 1.E12
         FFIELD = .TRUE.
C                                       sort fields
 135     FORCED = TFIELD.GT.0
         IF (TFIELD.EQ.0) THEN
            CALL CLBSTF (MFIELD, RSSMAX, RSSAVG, IGNORE, BMSCAL,
     *         MINFLX, MNFFLX, ELIMXF, RESMAX, JCOUNT, KFIELD, BEST)
            JCOUNT = MIN (JCOUNT, MXLIST)
            JCOUNT = MIN (JCOUNT, MXULST)
            JCOUNT = MAX (1, JCOUNT)
            IFIELD = KFIELD(1)
            IF (IFIELD.LE.0) THEN
               MSGTXT = 'No fields left to Clean'
               CALL MSGWRT (3)
               FINISH = .TRUE.
               DO 139 I = 1,MFIELD
                  IF (NBOXES(I).GT.0) GO TO 200
 139              CONTINUE
               MSGTXT = 'because there were no boxes allowed for' //
     *            ' Cleaning'
               CALL MSGWRT (3)
               GO TO 200
               END IF
C                                       if within margin of known
C                                       use the known
            IF ((PFIELD.GT.0) .AND. (CURENT(IFIELD).LT.2) .AND.
     *         (CURENT(PFIELD).GE.2)) THEN
               IF (BEST.LT.PBEST*RETRY) THEN
                  IFIELD = PFIELD
C                                       increase on a retry
               ELSE
                  MXULST = MAX (4, MXULST+1)
                  END IF
               END IF
         ELSE
            IFIELD = TFIELD
            TFIELD = 0
            END IF
C                                       make new image
         DOCLEN = .TRUE.
         IF ((OVRLAP.GT.2) .AND. (NUMCYC.GE.OVRLAP)) THEN
            DOCLEN = .FALSE.
            IERR = -99
            MSGTXT = 'Re-image all fields automatically'
            CALL MSGWRT (2)
         ELSE
            IF (.NOT.FMJCYC) THEN
 140           IF (CURENT(IFIELD).LT.2) THEN
                  IF (FORCED) THEN
                     JCOUNT = 1
                     CALL FILL (SZLIST, 0, KFIELD)
                     KFIELD(1) = TFIELD
                     END IF
                  DO 141 I = 1,JCOUNT
                     IF ((KFIELD(I).GT.0) .AND.
     *                  (CURENT(KFIELD(I)).LT.2)) THEN
                        KFIELD(I) = KFIELD(I) + 1000000
                     ELSE
                        CALL FILL (SZLIST+1-I, 0, KFIELD(I))
                        END IF
 141                 CONTINUE
                  CALL OUVIMG (UVDATA, KFIELD, MFIELD, CNAME, DBNAME,
     *               WORK1, WORK2, .FALSE., UVCHAN, NCHAV, CHANN, IERR)
                  IF (IERR.NE.0) GO TO 995
                  PBEST = 0.0
                  PFIELD = 0
                  DO 142 I = 1,JCOUNT
                     IF (KFIELD(I).GT.1000000) THEN
                        IFIELD = KFIELD(I) - 1000000
                        CURENT(IFIELD) = 2
                        IF (IGNORE(IFIELD).GT.-0.5) THEN
                           CALL CLRMAX (IFIELD, MFIELD, ABSMAX, XFLUX,
     *                        RSSMAX, RSSAVG, IERR)
                           TX = MAX (RSSMAX(IFIELD), ELIMXF(IFIELD))
                           ABEST = CLOFNB (TX, RSSAVG(IFIELD),
     *                        BMSCAL(IFIELD))
                           IF (ABEST.GT.PBEST) THEN
                              PFIELD = IFIELD
                              PBEST = ABEST
                              END IF
                           END IF
                        END IF
 142                 CONTINUE
C                                       reduce number of facets
C                                       imaged at one time
                  IF (JCOUNT.GT.2) THEN
                     CALL CLRMS (JCOUNT, KFIELD, RSSMAX, ELIMXF, RSSAVG,
     *                  BMSCAL, RATIO)
                     IF (RATIO.GT.2.) NNBEST = NNBEST + 1
                     IF ((RATIO.GT.4.) .OR. (NNBEST.GE.4)) THEN
                        MXULST = MXULST - 1
                        IF (RATIO.GT.8.) MXULST = MXULST - 1
                        IF (RATIO.GT.12.) MXULST = MXULST - 1
                        IF (RATIO.GT.16.) MXULST = MXULST - 1
                        IF (MXULST.EQ.3) MXULST = 2
                        MXULST = MAX (2, MXULST)
                        NNBEST = 0
                        END IF
                     END IF
                  TFIELD = 0
                  IF (.NOT.FORCED) GO TO 135
                  END IF
               PBEST = -ABS(PBEST)
               LFIELD = IFIELD
               JFIELD = LFIELD
               IF (AUTOBX(1).GT.0.5) THEN
                  PASSBX = PASSBX + 1
                  IF (PASSBX.GT.15) THEN
                     JFIELD = 0
                     PASSBX = 0
                     END IF
                  CALL CLABOX (NAME, JFIELD, IERR)
                  IF (IERR.GT.1) GO TO 995
                  END IF
               CALL CLNTV (NAME, IFIELD, IERR)
               IF (IERR.GT.1) GO TO 995
               IF (FINISH) GO TO 200
C                                       current field shut off
               IF (IGNORE(IFIELD).LT.1.0) THEN
                  WRITE (MSGTXT,1145) IFIELD
                  CALL MSGWRT (2)
                  GO TO 135
                  END IF
C                                       force a field choice
               LERR = IERR
               DOCLEN = IERR.GE.0
               IF ((IERR.EQ.-3) .AND. (INDATA.NE.' ')) WASOME = .TRUE.
               IF (DOCLEN) CALL CLRMAX (JFIELD, MFIELD, ABSMAX,
     *            XFLUX, RSSMAX, RSSAVG, IERR)
               IF ((LERR.EQ.0) .AND. (IFIELD.NE.LFIELD)) THEN
                  FORCED = .TRUE.
                  TFIELD = IFIELD
                  GO TO 140
                  END IF
               END IF
            END IF
C                                       Find components
         IF (DOCLEN) THEN
            IF (BMSMAX.GT.1.001*BMSCAL(IFIELD)) BMSCP =
     *         MAX (0.0, BMSCP - MRCTRL(1))
            PCCMIN = FLDCCM(IFIELD)
            NEDH1 = IFIELD
            NEDHIS = 0
            CALL CLNCYC (NAME, IFIELD, IERR)
            IF (IERR.GT.0) GO TO 995
            IF (IERR.EQ.0) THEN
               DO 143 I = 1,MFIELD
                  IF (IGNORE(I).EQ.0.0) IGNORE(I) = 1.0
 143           CONTINUE
               NZERO = MAX (0, NZERO-1)
               END IF
            NUMCYC = NUMCYC + 1
            END IF
         ORESMX = RESMAX
C                                       Imaging failure? Redo all
         DOCLEN = IERR.GE.0
         IF (.NOT.DOCLEN) THEN
            IF (.NOT.WASOME) THEN
               MSGTXT = '*****************************************'
               CALL MSGWRT (8)
               CALL MSGWRT (8)
               MSGTXT = 'INFINITE LOOP CONDITION: OVERLAP -> 1'
               CALL MSGWRT (8)
               MSGTXT = '*****************************************'
               CALL MSGWRT (8)
               CALL MSGWRT (8)
               IF (NUMRES.GT.1) THEN
                  MSGTXT = 'BUT WE CANNOT DO THAT IN MULTI-SCALE'
                  CALL MSGWRT (8)
               ELSE
                  OVRLAP = 1
                  IF ((OVRSW.LT.0.0) .AND. (.NOT.ONEBEM)) THEN
                     ONEBEM = .TRUE.
                     MSGTXT = 'Also switching to ONEBEAM = TRUE'
                     CALL MSGWRT (5)
                     END IF
                  END IF
               CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1,
     *            WORK2, .FALSE., UVCHAN, NCHAV, CHANN, IERR)
               IF (IERR.NE.0) GO TO 995
               IF (NUMRES.LE.1) GO TO 999
               END IF
C                                       don't redo a bad forced
            IF ((FORCED) .AND. (NFORCE.LE.5)) THEN
               MSGTXT = 'CLEAN ERROR ON FORCED FIELD - TRY AGAIN'
               CALL MSGWRT (6)
               TFIELD = 0
               NFORCE = NFORCE + 1
               IF (IGNORE(IFIELD).GT.-1.5) IGNORE(IFIELD) = 0.0
               GO TO 135
               END IF
            NFORCE = 0
C                                       0 found, mess and retry
            IF ((IERR.EQ.-2) .AND. (NZERO.LE.10)) THEN
               NZERO = NZERO + 2
               MSGTXT = 'CLEAN NO IMAGE PIXELS - TRY AGAIN'
               CALL MSGWRT (6)
               TFIELD = 0
               IF (IGNORE(IFIELD).GT.-1.5) IGNORE(IFIELD) = 0.0
               IERR = 0
               GO TO 135
               END IF
            NZERO = 0
            DO 144 I = 1,MFIELD
               IF (IGNORE(I).EQ.0.0) IGNORE(I) = 1.0
 144           CONTINUE
C                                       filter
            IF ((INDATA.NE.' ') .AND. (CCFILT(1).NE.0)) THEN
               LERR = IERR
C                                       do filtering
               CALL DOFILT (.FALSE., FMJCYC, SUMCC, IERR)
               IF (IERR.NE.0) GO TO 995
               IERR = LERR
               WRITE (MSGTXT,1005) SUMCC
               CALL MSGWRT (4)
               NFILT = NFILT + 1
               CALL FILL (MFIELD, 0, CURENT)
               END IF
            DOIMG = .FALSE.
            DO 1440 I = 1,MFIELD
               IF (CURENT(I).LT.2) DOIMG = .TRUE.
 1440          CONTINUE
            I = IERR
            NUMCYC = 0
            IF (DOIMG) THEN
               CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME,
     *            WORK1, WORK2, .FALSE., UVCHAN, NCHAV, CHANN, IERR)
               IF (IERR.NE.0) GO TO 995
               CALL FILL (MFIELD, 2, CURENT)
            ELSE
               MSGTXT = 'All fields current, no re-image'
               CALL MSGWRT (2)
               END IF
            IF ((FFIELD) .AND. (I.EQ.-1) .AND. (FINISH)) GO TO 200
            FMJCYC = .TRUE.
            WASOME = .FALSE.
C                                       Subtract model
         ELSE
C                                       How many components?
            IDIM(1) = MFIELD
            IDIM(2) = 1
            CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Subtract components
            CALL UVSUBM (UVDATA, UVDATA, IFIELD, MFIELD, CNAME,
     *         UVCHAN, NCHAV, IERR)
            IF (IERR.NE.0) GO TO 995
            PCCMIN = NCCMIN
            FMJCYC = .FALSE.
            WASOME = .TRUE.
            IF (IFIELD.NE.0) ELIMXF(IFIELD) = 0.
            CALL FILL (MFIELD, 0, CURENT)
C                                       estimate residual of ifield
            IF (RSSMAX(IFIELD).GT.0.0) RSSAVG(IFIELD) =
     *         RSSAVG(IFIELD) * ORESMX / RSSMAX(IFIELD)
            RSSMAX(IFIELD) = ORESMX
            RESMAX = -1.E9
            DO 145 I = 1,MFIELD
               RESMAX = MAX (RESMAX, RSSMAX(I))
 145           CONTINUE
            END IF
C                                       Any more components?
         CALL ISUM (MFIELD, NCLNG, SUMCC)
C                                       Reset initial RESMAX
         IF (FMJCYC) THEN
            CALL CLRMAX (0, MFIELD, ABSMAX, XFLUX, RSSMAX, RSSAVG, IERR)
            IF (IERR.NE.0) GO TO 995
            RESMAX = ABS (XFLUX)
            IF ((ACTRES.GT.0.0) .OR. (MINFLX.GE.0.0)) ACTRES = XFLUX
            END IF
         IF (RESMAX.EQ.0.0) RESMAX = ELIMAX
         FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MINFLX) .OR.
     *      ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
C                                       DEBUG
         IF ((ELIMAX.LT.SWITCH) .AND. (MFIELD.GT.1) .AND.
     *      (MFIELD.EQ.NFPRES) .AND. (.NOT.FINISH)) THEN
            OVRLAP = 1
            MSGTXT = 'Switching to OVERLAP = 1 from OVERLAP >= 2'
            CALL MSGWRT (5)
            IF ((OVRSW.LT.0.0) .AND. (.NOT.ONEBEM)) THEN
               ONEBEM = .TRUE.
               MSGTXT = 'Also switching to ONEBEAM = TRUE'
               CALL MSGWRT (5)
               END IF
            CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME,
     *         WORK1, WORK2, .FALSE., UVCHAN, NCHAV, CHANN, IERR)
            IF (IERR.NE.0) GO TO 995
            GO TO 999
            END IF
C                                       TELL
         IF (.NOT.FINISH) THEN
            CALL CLNTEL (NAME, UVDATA, IERR)
            IF (IERR.NE.0) GO TO 995
            FINISH = (SUMCC.GE.CLNLIM) .OR. (RESMAX.LT.MINFLX) .OR.
     *         ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) .OR. (QUIT)
            SWITCH = ABS (OVRSW)
            END IF
         IF (FINISH) THEN
            IF (SUMCC.GE.CLNLIM) THEN
               IF (SUMCC.GT.CLNLIM) THEN
                  WRITE (MSGTXT,1010) SUMCC, 'exceeds', CLNLIM
               ELSE IF (SUMCC.GT.0) THEN
                  WRITE (MSGTXT,1010) SUMCC, 'reaches', CLNLIM
               ELSE
                  MSGTXT = 'No Cleaning requested'
                  END IF
               CALL MSGWRT (4)
               END IF
            IF (RESMAX.LE.MINFLX) THEN
               XFLUX = RESMAX
               ABSMAX = MINFLX
               IF (RESMAX.NE.0.0) THEN
                  CALL METSCA (XFLUX, PREFIX, I)
                  ABSMAX = ABSMAX * XFLUX / RESMAX
               ELSE
                  CALL METSCA (ABSMAX, PREFIX, I)
                  END IF
               WRITE (MSGTXT,1011) XFLUX, ABSMAX, PREFIX
               CALL MSGWRT (4)
               END IF
            IF ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) THEN
               XFLUX = ACTRES
               CALL METSCA (XFLUX, PREFIX, I)
               WRITE (MSGTXT,1012) XFLUX, PREFIX
               CALL MSGWRT (4)
               END IF
            END IF
C                                       TV
         IF (FMJCYC) THEN
C                                       Find max for overlap 2 mode
            MSGSAV = MSGSUP
            MSGSUP = 32000
            CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.NE.0) TVFLD = 0
            IF (TVFLD.GT.0) THEN
               CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX,
     *            RSSAVG, IERR)
               IF (IERR.NE.0) GO TO 995
               CALL FNDMAX (NAME, BMSCAL, TVFLD, IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            TFIELD = 0
            IF (AUTOBX(1).GT.0.5) THEN
               PASSBX = 0
               CALL CLABOX (NAME, TFIELD, IERR)
               IF (IERR.GT.1) GO TO 995
               END IF
            CALL CLNTV (NAME, TFIELD, IERR)
            IF (IERR.GT.1) GO TO 995
C                                       Check maxima w new windows
            IF (.NOT.FINISH) THEN
C                                       filter
               IF ((IERR.EQ.-3) .AND. (INDATA.NE.' ') .AND.
     *            (CCFILT(1).NE.0.0)) THEN
                  LERR = IERR
                  CALL DOFILT (.TRUE., FMJCYC, SUMCC, IERR)
                  IF (IERR.NE.0) GO TO 995
                  NFILT = NFILT + 1
                  WRITE (MSGTXT,1005) SUMCC
                  CALL MSGWRT (4)
                  NUMCYC = 0
                  CALL FILL (MFIELD, 2, CURENT)
                  RTEMP = CCFILT(1)
                  CCFILT(1) = 0.0
C                                       Find max for overlap 2 mode
                  MSGSAV = MSGSUP
                  MSGSUP = 32000
                  CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD,
     *               CDUMMY, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.NE.0) TVFLD = 0
                  IF (TVFLD.GT.0) THEN
                     CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX,
     *                  RSSAVG, IERR)
                     IF (IERR.NE.0) GO TO 995
                     CALL FNDMAX (NAME, BMSCAL, TVFLD, IERR)
                     IF (IERR.NE.0) GO TO 995
                     END IF
                  TFIELD = 0
                  CALL CLNTV (NAME, TFIELD, IERR)
                  IF (IERR.GT.1) GO TO 995
                  CCFILT(1) = RTEMP
                  IERR = LERR
                  END IF
               CALL CLRMAX (0, MFIELD, ABSMAX, ACTRES, RSSMAX, RSSAVG,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               RESMAX = ABS (ACTRES)
               END IF
            END IF
         GO TO 100
         END IF
C                                       Compress the CC files
C                                       Initialize residual
 200  FILTRS = .FALSE.
      IF ((INDATA.NE.' ') .AND. (CCFILT(1).NE.0.0) .AND. (NFILT.LT.3))
     *   THEN
         MSGSUP = 32000
         CALL OGET (NAME, 'CCFILTRS', TYPE, DIM, FILTRS, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.1) THEN
            IERR = 0
            FILTRS = .FALSE.
            END IF
         END IF
      IF (FILTRS) THEN
         NFILT = NFILT + 5
         MSGTXT = 'Forcing a filter and resuming Clean'
         CALL MSGWRT (5)
         FINISH = .FALSE.
         QUIT = .FALSE.
         DO 205 I = 1,MAXFLD
            IF (IGNORE(I).GT.-1.5) IGNORE(I) = 1.0
 205        CONTINUE
         CALL DOFILT (.TRUE., FMJCYC, SUMCC, IERR)
         IF (IERR.NE.0) GO TO 995
         WRITE (MSGTXT,1005) SUMCC
         CALL MSGWRT (4)
         GO TO 100
         END IF
      GO TO 999
C                                       Error
 995  MSGTXT = 'CLNUV2 : ERROR CLEANING ' // NAME
      IF (IERR.NE.99) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('Resuming Clean with',I9,' filtered components')
 1010 FORMAT ('Total Clean components',I9,1X,A,' limit',I8)
 1011 FORMAT ('Peak residual in window',F8.3,' less than limit',F8.3,1X,
     *   A5,'Jy')
 1012 FORMAT ('Peak residual in window',F8.3,' less than zero ',A5,'Jy')
 1100 FORMAT ('CLNUV2: Beam scaling power now',F6.3)
 1145 FORMAT ('Field',I5,' turned off: find another field')
      END
      SUBROUTINE DOFILT (DOIMAG, FMJCYC, SUMCC, IERR)
C-----------------------------------------------------------------------
C   Combines the operations needed to do a filtering for convenience
C   Inputs
C      DOIMAG   L      Remake all images
C   Outputs:
C      FMJCYC   L      T => images are current
C      SUMCC    I      Number CCs left
C      IERR     I      Error code
C-----------------------------------------------------------------------
      LOGICAL   DOIMAG, FMJCYC
      INTEGER   SUMCC, IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   IDIM(7)
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C                                       Compress the CC files
      CALL CLMERG (IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Filter CCs
      CALL CLFILT (IERR)
      IF (IERR.NE.0) GO TO 995
      FMJCYC = .FALSE.
      CALL UVRSCR (INDATA, UVDATA, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 995
      IDIM(1) = MFIELD
      IDIM(2) = 1
      CALL FILL (MAXFLD, 1, NSUBG)
      CALL OUVPUT (UVDATA, 'MODCCBEG', OOAINT, IDIM, NSUBG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OUVPUT (UVDATA, 'MODCCEND', OOAINT, IDIM, NCLNG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Initialize residual
      CALL ISUM (MFIELD, NCLNG, SUMCC)
C                                       subtract remaining components
      IF (SUMCC.GE.1) THEN
         CALL UVSUBM (UVDATA, UVDATA, 0, MFIELD, CNAME, UVCHAN, NCHAV,
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      IF (DOIMAG) THEN
         CALL OUVIMG (UVDATA, 0, MFIELD, CNAME, DBNAME, WORK1, WORK2,
     *      .FALSE., UVCHAN, NCHAV, CHANN, IERR)
         IF (IERR.NE.0) GO TO 995
         FMJCYC = .TRUE.
      ELSE
         FMJCYC = .FALSE.
         END IF
      GO TO 999
C
 995  MSGTXT = 'DOFILT: ERROR FILTERING ' // UVDATA
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE FNDMAX (NAME, BMSCAL, TVFLD, IERR)
C-----------------------------------------------------------------------
C   Finds the field with  highest residual and saves it in TVFIELD
C   Inputs:
C      NAME     C*(*)   Clean object name
C      BMSCAL   R(*)    Scaling due to beam size of each field
C   In/out:
C      TVFLD    I       Selected TV Field
C   Output:
C      IERR     I       Error code from CLNPUT
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      REAL      BMSCAL(*)
      INTEGER   TVFLD, IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   IFIELD, LFIELD, DIM(7)
      REAL      FF, RTEMP, CLOFNB, TX
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      RESMAX = -1.0E9
      RTEMP = -1.0E9
      IFIELD = 0
      DO 10 LFIELD = 1,MFIELD
         IF (IGNORE(LFIELD).GT.-0.5) THEN
            TX = MAX (RSSMAX(LFIELD), ELIMXF(LFIELD))
            FF = CLOFNB (TX, RSSAVG(LFIELD), BMSCAL(LFIELD))
            IF ((FF.GT.RTEMP) .AND.
     *         (RSSMAX(LFIELD).GT.MNFFLX(LFIELD))) THEN
               IFIELD = LFIELD
               RTEMP = FF
               RESMAX = RSSMAX(LFIELD)
               END IF
            END IF
 10      CONTINUE
      IF (IFIELD.GT.0) TVFLD = IFIELD
      DIM(1) = 1
      DIM(2) = 1
      CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY, IERR)
C
 999  RETURN
      END
      SUBROUTINE CLNSRT (NAME, CNAME, UVDATA, IERR)
C-----------------------------------------------------------------------
C   Private
C   Sort the UV data for CLNUV?
C   Inputs:
C      NAME    C*?   The name of the Clean object.
C      CNAME   C*?   The name of the first output clean image.
C      UVDATA  C*?   The name of the UVdata object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), CNAME*(*), UVDATA*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, DUMMY, IMSIZE(2,MAXFLD), LLREC, TYPE, DIM(7),
     *   MSGSAV, NEED, I1, I2, I3
      REAL      MAXUU
      CHARACTER CDUMMY*1, CMETH*4, SORD*2
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
C                                       is a sort required?
C                                       component subtraction method
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OUVGET (UVDATA, 'MODMETH', TYPE, DIM, DUMMY, CMETH, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         CMETH = ' '
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (CMETH(:3).NE.'DFT ') THEN
         CALL UVDGET (UVDATA, 'SORTORD', TYPE, DIM, DUMMY, SORD, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (SORD(1:1).NE.'X') THEN
C                                       Frequencies
            CALL OGET (NAME, 'NCHAV', TYPE, DIM, NCHAVG, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL UVFRQS (UVDATA, FREQUV, FREQG, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL IMGET (CNAME, 'IMSIZE', TYPE, DIM, IMSIZE, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            NXMAX = 0
            NYMAX = 0
            DO 10 I = 1,MAXFLD
               NXMAX = MAX (NXMAX, IMSIZE(1,I))
               NYMAX = MAX (NYMAX, IMSIZE(2,I))
 10            CONTINUE
            CALL UVDGET (UVDATA, 'LREC', TYPE, DIM, LLREC, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Decide
            MAXUU = 0.0
            MSGSUP = 32000
            CALL GRDFIT (3, LLREC, FREQG, MAXUU, CMETH, NEED, IERR)
            MSGSUP = MSGSAV
            IF ((IERR.LT.0) .OR. (IERR.EQ.1)) THEN
               CALL QRLSE
               I1 = NEED / 1024 + 4
               I1 = I1 * 1.05
               CALL QINIT (I1, I2, I3)
               IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
                  MSGTXT = 'CLNSRT: FAILED TO GET NEEDED MEMORY'
                  CALL MSGWRT (8)
                  IERR = 8
                  GO TO 990
                  END IF
               MSGSUP = 32000
               CALL GRDFIT (3, LLREC, FREQG, MAXUU, CMETH, NEED, IERR)
               MSGSUP = MSGSAV
               END IF
            IF ((IERR.LT.0) .OR. (IERR.EQ.1)) THEN
               CALL QRLSE
               I1 = NEED / 1024 + 4
               I1 = I1 * 1.05
               CALL QINIT (I1, I2, I3)
               IF ((I3.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
                  MSGTXT = 'CLNSRT: FAILED TO GET NEEDED MEMORY'
                  CALL MSGWRT (8)
                  IERR = 8
                  GO TO 990
                  END IF
               CALL GRDFIT (3, LLREC, FREQG, MAXUU, CMETH, NEED, IERR)
               END IF
            IF (IERR.GT.1) THEN
               GO TO 995
            ELSE IF (IERR.EQ.1) THEN
               MSGTXT = 'PROBLEM AS POSED WILL NOT FIT EVEN IF DATA ARE'
     *            // ' SORTED'
               GO TO 990
C                                       Do sort
            ELSE IF (IERR.LT.0) THEN
               MSGTXT = 'CLNUV : Sorting data to make them fit'
               CALL MSGWRT (2)
               DIM(1) = 2
               DIM(2) = 1
               SORD = 'XY'
               CALL OUVPUT (UVDATA, 'SORT', OOACAR, DIM, DUMMY, SORD,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               DIM(1) = 1
               CALL OUVPUT (UVDATA, 'ROTATE', OOARE, DIM, 0.0, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               CALL OUVSRT (UVDATA, UVDATA, IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLNSRT: ERROR SORTING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLNTV (NAME, IFIELD, IERR)
C-----------------------------------------------------------------------
C   Private
C   Does a TV display and interaction
C   Inputs:
C      NAME     C*?       The name of the Clean object.
C   Inputs from common:
C      MFIELD   I         Number of fields (max. 1 for IMAGE Clean)
C      CNAME    C(*)*32   Names of residual/Clean imagesC
C   In/Out:
C      IFIELD   I         In: Restrict to field IFIELD
C                         Out: user demands field IFIELD instead
C   Output to common:
C      FINISH   L         .TRUE. If Clean done
C      NBOXES   I(*)      Number of boxes given for field
C      WIN      I(4,*)    Boxes for fields
C                           WIN(1,*)=-1 indicates a round box of
C                           width WIN(2,*) pixels centered on
C                           pixel (WIN(3,*), WIN(4,*))
C   Output:
C      IERR     I         0 all okay, 1 TV error, 99 user wants abort
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IFIELD, IERR
C
      CHARACTER CDUMMY*1, STATUS*4, TVNAME*32, CHOICS(91)*16, OBXFIL*48,
     *   MSGBUF*72, TITLE*80, PREFIX*5, CHNUMB*6
      INTEGER   TVFLD, MSGSAV, TYPE, DIM(7), GRCS(8,5), TVCS(16), I, J,
     *   NCOL, NROWS(3), TIMLIM, CHS, TVBUTT, JERR, TBLC(7), DUMMY, K,
     *   MTYPE, LERR, LFIELD, LF1, LF2, NGRY, NGRPH, INX, MAXX(2),
     *   TVWND(4), CSIZE(2), TTY(2), KBP, GRSTAT(8), TVSTAT(16),
     *   TVGRCH(3), SCHOIS, NGAUSS, LIMTIM(2), NTITLE, SIDSEP, JTRIM
      REAL      XFLUX
      DOUBLE PRECISION X
      LOGICAL   LEAVE(82), REBOXD, NEWMEN, INTROK
      INCLUDE 'QCLEAN.INC'
      SAVE TIMLIM
      DATA CHOICS /'ABORT TASK', 'TURN OFF DOTV', 'STOP CLEANING',
     *   'OFFZOOM', 'OFFTRANS', 'OFFCOLOR', 'TVFIDDLE', 'TVTRAN',
     *   'TVPSEUDO', 'TVFLAME', 'TVZOOM', 'CURVALUE', 'IMSTAT',
     *   'SET WINDOW', 'RESET WINDOW', 'TVBOX', 'REBOX', 'DELBOX',
     *   'UNBOX', 'CONTINUE CLEAN', 'CHECK BOXES', ' ', 69*' '/
      DATA TIMLIM /-1000/
C-----------------------------------------------------------------------
C                                       restore init values
      CHOICS(19) = 'UNBOX'
      CHOICS(20) = 'CONTINUE CLEAN'
      CHOICS(21) = 'CHECK BOXES'
C                                       set menu values
      CALL LFILL (91, .TRUE., LEAVE)
      CALL LFILL (16, .FALSE., LEAVE(4))
      IF (.NOT.UNBOXD) THEN
         LEAVE(19) = .TRUE.
         CHOICS(19) = CHOICS(20)
         CHOICS(20) = CHOICS(21)
         CHOICS(21) = ' '
         END IF
      SDINOW = SDICLN
C                                       does the TV need to run?
      IERR = 0
      TTY(1) = 5
      TTY(2) = 0
      TITLE = ' '
      REBOXD = .FALSE.
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) TVFLD = 0
      IF (TVFLD.LE.0) GO TO 999
      MSGSUP = 32000
      CALL CLNGET (NAME, 'INTACTOK', TYPE, DIM, INTROK, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) INTROK = .FALSE.
      MSGSUP = 32000
      CALL CLNGET (NAME, 'TVGRCHAN', TYPE, DIM, TVGRCH, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) CALL FILL (3, 8, TVGRCH)
      IF (TVGRCH(1).LT.4) TVGRCH(1) = 0
      IF (TVGRCH(1).GT.7) TVGRCH(1) = 5
      IF (TVGRCH(2).LT.4) TVGRCH(2) = 0
      IF (TVGRCH(2).GT.7) TVGRCH(2) = 7
      IF (TVGRCH(3).LT.4) TVGRCH(3) = 0
      IF (TVGRCH(3).GT.7) TVGRCH(3) = 6
      MSGSUP = 32000
      CALL CLNGET (NAME, 'TVTIMLIM', TYPE, DIM, LIMTIM, CDUMMY, LERR)
      MSGSUP = MSGSAV
      IF (LERR.NE.0) THEN
         LIMTIM(1) = 600
         LIMTIM(2) = 30
         END IF
      IF (LIMTIM(1).LE.0) LIMTIM(1) = 600
      IF (LIMTIM(2).LE.0) LIMTIM(2) = 30
      IF (TIMLIM.LT.0) THEN
         TIMLIM = LIMTIM(1)
      ELSE
         TIMLIM = LIMTIM(2)
         END IF
C                                       open TV device
      TVNAME = 'Clean residual TV display'
      CALL TVDCRE (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 990
      CALL TVDOPN (TVNAME, STATUS, LERR)
      IF (LERR.NE.0) GO TO 985
C                                       choices
      IF (TVFLD.GT.MFIELD) TVFLD = 1
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         IF (IFIELD.GT.MFIELD) IFIELD = 1
         LF1 = IFIELD
         LF2 = IFIELD
         TVFLD = IFIELD
         END IF
C                                       Open terminal for conversation
      IF (INTROK) THEN
         CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      LERR)
         IF (LERR.NE.0) THEN
            TTY(2) = 0
            WRITE (MSGTXT,1035) LERR
            CALL MSGWRT (8)
            GO TO 980
            END IF
         TTY(2) = MAX (1, TTY(2))
         END IF
      DIM(1) = 1
      DIM(2) = 1
      CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 985
      NCOL = 2
      NROWS(1) = 15
      NROWS(2) = 6
      IF (MFIELD.LE.1) NROWS(2) = 5
      IF (.NOT.UNBOXD) NROWS(2) = NROWS(2) - 1
      NROWS(3) = 0
      IF ((LF2-LF1.GE.16) .AND. (LF2-LF1.LE.64)) THEN
         NROWS(1) = NROWS(1) + NROWS(2)
         NROWS(2) = 0
         END IF
      J = NROWS(1) + NROWS(2)
      IF (SDINOW.GT.0) THEN
         J = J + 1
         CHOICS(J) = 'FORCE BGC CLEAN'
         NROWS(2) = NROWS(2) + 1
      ELSE IF (SDINOW.GE.-1) THEN
         J = J + 1
         CHOICS(J) = 'FORCE SDI CLEAN'
         NROWS(2) = NROWS(2) + 1
         END IF
      IF ((CCFILT(1).NE.0.0) .AND. (CCFILT(2).GE.0.5)) THEN
         J = J + 1
         LEAVE(J) = .FALSE.
         CHOICS(J) = 'FILTER COMPS'
         NROWS(2) = NROWS(2) + 1
         END IF
      IF (MFIELD.GT.1) THEN
         IF ((MFIELD.GT.1) .AND. (IFIELD.GT.0)) THEN
            J = J + 1
            CHOICS(J) = 'REMAKE IMAGES'
            NROWS(2) = NROWS(2) + 1
            END IF
         IF ((INTROK) .AND. (OVRLAP.GE.2)) THEN
            J = J + 1
            CHOICS(J) = 'FORCE A FIELD'
            NROWS(2) = NROWS(2) + 1
            END IF
         IF (OVRLAP.GE.2) THEN
            J = J + 1
            WRITE (CHNUMB,1000) TVFLD
            IF (IGNORE(TVFLD).EQ.-1.0) THEN
               CHOICS(J) = 'ALLOW FLD' // CHNUMB
            ELSE
               CHOICS(J) = 'STOP FLD' // CHNUMB
               END IF
            CALL REFRMT (CHOICS(J), ' ', INX)
            SCHOIS = J
            NROWS(2) = NROWS(2) + 1
            END IF
         IF (LF2-LF1+1.GT.64) THEN
            J = J + 1
            CHOICS(J) = 'SELECT NEW FIELD'
            NROWS(2) = NROWS(2) + 1
         ELSE IF (LF2.GT.LF1) THEN
            DO 10 LFIELD = LF1,LF2
               J = J + 1
               WRITE (CHNUMB,1000) LFIELD
               CHOICS(J) = 'SELECT FLD' // CHNUMB
               CALL REFRMT (CHOICS(J), ' ', INX)
 10            CONTINUE
            IF (LF2-LF1.GE.32) THEN
               NCOL = 3
               NROWS(3) = MFIELD / 2
               NROWS(2) = J - NROWS(1) - NROWS(3)
               IF (NROWS(2)-NROWS(1).GT.5) THEN
                  J = (NROWS(2) - NROWS(1) - 3) / 3
                  NROWS(1) = NROWS(1) + 2 * J
                  NROWS(2) = NROWS(2) - J
                  NROWS(3) = NROWS(3) - J
                  END IF
            ELSE
               NROWS(2) = J - NROWS(1)
               IF (NROWS(2)-NROWS(1).GT.5) THEN
                  J = (NROWS(2) - NROWS(1) - 4) / 2
                  NROWS(1) = NROWS(1) + J
                  NROWS(2) = NROWS(2) - J
                  END IF
               END IF
            END IF
         END IF
C                                       off graphics
      DO 11 I = 1,8
         GRCS(I,1) = I
 11      CONTINUE
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFG (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       learn about TV
      CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       desired ones
      CALL FILL (40, 0, GRCS)
      GRCS(1,1) = 1
      GRCS(2,1) = 2
      GRCS(1,2) = MIN (3, NGRPH-1)
      GRCS(1,3) = MIN (2, NGRPH-1)
      IF (UNBOXD) GRCS(1,4) = MIN (4, NGRPH-1)
      CALL COPY (3, TVGRCH, GRCS(1,5))
C                                       tell TV what chans, parent
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
C                                       off other channels, zoom, scroll
      CALL FILL (7, 0, TBLC)
      TBLC(3) = CHANN
      DO 12 I = 1,15
         TVCS(I) = I+1
 12      CONTINUE
      TVCS(16) = 0
      DIM(1) = 16
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFF (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFZ (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFS (TVNAME, .TRUE., LERR)
      IF (LERR.NE.0) GO TO 980
C                                       specify our channel
      CALL FILL (16, 0, TVCS)
      TVCS(1) = 1
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS, CDUMMY, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVOFS (TVNAME, .TRUE., LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = LEN (NAME)
      CALL TVDPUT (TVNAME, 'TVPARENT', OOACAR, DIM, DUMMY, NAME, LERR)
      IF (LERR.NE.0) GO TO 980
C                                       clear the TV channel
      CALL OTVCLC (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVCLG (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVON (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      IF (TIMLIM.GT.31) THEN
         CALL OTVOFT (TVNAME, LERR)
         IF (LERR.NE.0) GO TO 980
         CALL OTVOFC (TVNAME, LERR)
         IF (LERR.NE.0) GO TO 980
         END IF
C                                       select and show TVFLD
      DIM(1) = LEN (CNAME(1))
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY, CNAME(TVFLD),
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = 1
      CALL TVDPUT (TVNAME, 'WINLOAD', OOAINT, DIM, GRCS(1,2), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL TVDPUT (TVNAME, 'UWINLOAD', OOAINT, DIM, GRCS(1,4), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL TVDPUT (TVNAME, 'XWINLOAD', OOAINT, DIM, TVGRCH(3), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL CLBCHK (TVNAME, TVFLD, LERR)
      IF (LERR.NE.0) GO TO 980
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5), CDUMMY,
     *   LERR)
      IF (LERR.NE.0) GO TO 980
      CALL OTVLOD (TVNAME, LERR)
      IF (LERR.NE.0) GO TO 980
      IF (FINISH) GO TO 980
      NEWMEN = .FALSE.
C                                       loop to menu
 100  MTYPE = -1
C                                       Force new menu writing
      IF (NEWMEN) THEN
         CALL TVDOKA (TVNAME, TVSTAT, GRSTAT, IERR)
         IF (IERR.NE.0) GO TO 980
         GRSTAT(GRCS(1,1)) = 1
         CALL TVDRST (TVNAME, TVSTAT, GRSTAT, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       format menu title
      IF (MFIELD.GT.1) THEN
         XFLUX = FLUXG(TVFLD)
         CALL METSCA (XFLUX, PREFIX, LERR)
         WRITE (TITLE,1100) TVFLD, RASH(TVFLD), DECSH(TVFLD), XFLUX,
     *      PREFIX, NCLNG(TVFLD)
         IF (TITLE(23:23).EQ.' ') TITLE(23:23) = '0'
         IF (TITLE(24:28).EQ.'.0000') TITLE(24:28) = ' '
         IF (TITLE(26:28).EQ.'000') TITLE(26:28) = ' '
         IF (TITLE(27:28).EQ.'00') TITLE(27:28) = ' '
         IF (TITLE(28:28).EQ.'0') TITLE(28:28) = ' '
         IF (TITLE(35:35).EQ.' ') TITLE(35:35) = '0'
         IF (TITLE(36:40).EQ.'.0000') TITLE(36:40) = ' '
         IF (TITLE(38:40).EQ.'000') TITLE(38:40) = ' '
         IF (TITLE(39:40).EQ.'00') TITLE(39:40) = ' '
         IF (TITLE(40:40).EQ.'0') TITLE(40:40) = ' '
         IF (TITLE(53:53).EQ.' ') TITLE(53:53) = '0'
         IF (TITLE(54:57).EQ.'.000') TITLE(55:57) = ' '
         IF (TITLE(56:57).EQ.'00') TITLE(56:57) = ' '
         IF (TITLE(57:57).EQ.'0') TITLE(57:57) = ' '
         CALL REFRMT (TITLE, ' ', LERR)
         END IF
C                                       do menu read
      NTITLE = 1
      SIDSEP = 5
      CALL TVDMEN (TVNAME, MTYPE, NCOL, NROWS, GRCS, 0, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS, TVBUTT, LERR)
      IF (LERR.NE.0) GO TO 980
         TIMLIM = LIMTIM(2)
         IF (TVBUTT.LE.0) THEN
            MSGTXT = 'Menu read timed out'
            CALL MSGWRT (2)
            GO TO 980
            END IF
C                                       Call back: fiddle LUTs
C                                       OFFZOOM
         IF (CHOICS(CHS).EQ.'OFFZOOM') THEN
            CALL OTVOFZ (TVNAME, LERR)
C                                       OFFTRANS
         ELSE IF (CHOICS(CHS).EQ.'OFFTRANS') THEN
            CALL OTVOFT (TVNAME, LERR)
C                                       OFFCOLOR
         ELSE IF (CHOICS(CHS).EQ.'OFFCOLOR') THEN
            CALL OTVOFC (TVNAME, LERR)
C                                       TVFIDDLE
         ELSE IF (CHOICS(CHS).EQ.'TVFIDDLE') THEN
            CALL OTVFID (TVNAME, LERR)
C                                       TVTRAN
         ELSE IF (CHOICS(CHS).EQ.'TVTRAN') THEN
            CALL OTVTRA (TVNAME, LERR)
C                                       TVPSEUDO
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL OTVPSU (TVNAME, LERR)
C                                       TVPHLAME
         ELSE IF (CHOICS(CHS).EQ.'TVFLAME') THEN
            CALL OTVFLA (TVNAME, LERR)
C                                       TVZOOM
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL OTVZOM (TVNAME, LERR)
C                                       CURVALUE
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,3),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVALU (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       IMSTAT
         ELSE IF (CHOICS(CHS).EQ.'IMSTAT') THEN
            CALL CLSTAT (NAME, TVFLD, IERR)
            IF (IERR.NE.0) GO TO 980
C                                       set window
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVWIN (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVOFG (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVCLC (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLBCHK (TVNAME, TVFLD, LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, LERR)
C                                       reset window
         ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (CNAME(TVFLD), 'TBLC', OOAINT, DIM, TBLC, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OPUT (CNAME(TVFLD), 'TTRC', OOAINT, DIM, TBLC, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVCLC (TVNAME, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLBCHK (TVNAME, TVFLD, LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, LERR)
C                                       UNBOX
         ELSE IF (CHOICS(CHS).EQ.'UNBOX') THEN
            UNBOXS(TVFLD) = 0
            DIM(1) = MFIELD
            DIM(2) = 1
            CALL OPUT (NAME, 'UNBOXES', OOAINT, DIM, UNBOXS, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,4),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVUBX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'UNBOXES', TYPE, DIM, UNBOXS, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'UNWINDOW', TYPE, DIM, UNWIN, CDUMMY,
     *         LERR)
            REBOXD = .TRUE.
C                                       TVBOX
         ELSE IF (CHOICS(CHS).EQ.'TVBOX') THEN
            CDUMMY = 'N'
            IF (NBOXES(TVFLD).EQ.1) CDUMMY = 'Y'
            IF ((NBOXES(TVFLD).GT.1) .AND. (INTROK)) THEN
               WRITE (MSGBUF,1105) NBOXES(TVFLD)
               J = JTRIM (MSGBUF)
               CALL INQSTR (TTY, MSGBUF, J, MSGTXT, IERR)
               IF (IERR.EQ.0) THEN
                  I = JTRIM (MSGTXT)
                  CALL CHTRIM (MSGTXT, I, MSGTXT, J)
                  IF ((MSGTXT(:1).EQ.'y') .OR. (MSGTXT(:1).EQ.'Y'))
     *               CDUMMY = 'Y'
                  END IF
               END IF
            IF (CDUMMY.EQ.'Y') NBOXES(TVFLD) = 0
            IF (NBOXES(TVFLD).GT.0) THEN
               MSGTXT = 'Doing REBOX rather than TVBOX to save boxes'
               CALL MSGWRT (2)
               END IF
            DIM(1) = MFIELD
            DIM(2) = 1
            CALL OPUT (NAME, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVBOX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'WINDOW', TYPE, DIM, WIN, CDUMMY, LERR)
            REBOXD = .TRUE.
C                                       REBOX
         ELSE IF (CHOICS(CHS).EQ.'REBOX') THEN
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVBOX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'WINDOW', TYPE, DIM, WIN, CDUMMY, LERR)
            REBOXD = .TRUE.
C                                       DELBOX
         ELSE IF (CHOICS(CHS).EQ.'DELBOX') THEN
            NBOXES(TVFLD) = -NBOXES(TVFLD)
            DIM(1) = MFIELD
            DIM(2) = 1
            CALL OPUT (NAME, 'NBOXES', OOAINT, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
C                                       ignore error
            CALL OTVBOX (TVNAME, LERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 980
            CALL CLNGET (NAME, 'WINDOW', TYPE, DIM, WIN, CDUMMY, LERR)
            REBOXD = .TRUE.
            CALL CLBCHK (TVNAME, TVFLD, LERR)
            IF (LERR.NE.0) GO TO 980
            DIM(1) = 8
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *         CDUMMY, LERR)
            IF (LERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, LERR)
C                                       Check boxes & fix
         ELSE IF (CHOICS(CHS).EQ.'CHECK BOXES') THEN
            NGAUSS = MFIELD / NFPRES
            J = NBOXES(TVFLD)
            CALL BOXFIX (NAME, MFIELD, NGAUSS, CNAME, IMSIZE, 1,
     *         .FALSE., NBOXES, WIN, UNBOXS, UNWIN, LERR)
            IF ((NBOXES(TVFLD).NE.J) .AND. (LERR.EQ.0)) THEN
               CALL OTVCLC (TVNAME, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL CLBCHK (TVNAME, TVFLD, LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 8
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *            CDUMMY, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL OTVLOD (TVNAME, LERR)
               END IF
            IF (LERR.NE.0) GO TO 980
            REBOXD = .TRUE.
C                                       Switch to BGC Mode
         ELSE IF (CHOICS(CHS).EQ.'FORCE BGC CLEAN') THEN
            SDINOW = -1
            CHOICS(CHS) = 'FORCE SDI CLEAN'
            NEWMEN = .TRUE.
C                                       Switch to SDI Mode
         ELSE IF (CHOICS(CHS).EQ.'FORCE SDI CLEAN') THEN
            SDINOW = 2
            CHOICS(CHS) = 'FORCE BGC CLEAN'
            NEWMEN = .TRUE.
C                                       Remake all images
         ELSE IF (CHOICS(CHS).EQ.'REMAKE IMAGES') THEN
            MSGTXT = 'Recomputing all fields'
            CALL MSGWRT (2)
            IERR = -99
            GO TO 980
C                                       Remake all images
         ELSE IF (CHOICS(CHS).EQ.'FILTER COMPS') THEN
            MSGTXT = 'Filter & recompute all fields'
            CALL MSGWRT (2)
            IERR = -3
            GO TO 980
C                                       Continue cleaning
         ELSE IF (CHOICS(CHS).EQ.'CONTINUE CLEAN') THEN
            MSGTXT = 'Clean continuing'
            CALL MSGWRT (2)
            GO TO 980
C                                       Stop cleaning
         ELSE IF (CHOICS(CHS).EQ.'STOP CLEANING') THEN
            MSGTXT = 'Stop cleaning - done enough I guess!'
            CALL MSGWRT (4)
            FINISH = .TRUE.
            QUIT = .TRUE.
            GO TO 980
C                                       Stop cleaning
         ELSE IF (CHOICS(CHS).EQ.'ABORT TASK') THEN
            MSGTXT = 'Aborting task - gone wrong I guess!'
            CALL MSGWRT (4)
            FINISH = .TRUE.
            IERR = 99
            GO TO 980
C                                       Stop TV display
         ELSE IF (CHOICS(CHS).EQ.'TURN OFF DOTV') THEN
            MSGTXT = 'Turning off DOTV option - use TELL to turn on'
            CALL MSGWRT (4)
            TVFLD = 0
            DIM(1) = 1
            DIM(2) = 1
            CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY,
     *         LERR)
            GO TO 980
C                                       Force a field
         ELSE IF (CHOICS(CHS).EQ.'FORCE A FIELD') THEN
            MSGBUF = 'Enter field number to be used next'
            CALL INQINT (TTY, MSGBUF, 1, I, LERR)
            IF (LERR.GT.0) GO TO 980
            IF ((I.GT.0) .AND. (I.LE.MFIELD) .AND. (LERR.EQ.0) .AND.
     *         ((I.NE.TVFLD) .OR. (LF2.NE.LF1))) THEN
               IFIELD = I
               WRITE (MSGTXT,1110) IFIELD
               CALL MSGWRT (3)
               IERR = 0
               GO TO 980
               END IF
            LERR = 0
C                                       Force STOP Clean this field
         ELSE IF (CHOICS(CHS)(:8).EQ.'STOP FLD') THEN
            KBP = 9
            CALL GETNUM (CHOICS(CHS), 16, KBP, X)
            I = X + 0.001
            IF (IGNORE(I).GT.-1.5) IGNORE(I) = -1.0
            WRITE (CHNUMB,1000) TVFLD
            CHOICS(SCHOIS) = 'ALLOW FLD' // CHNUMB
            CALL REFRMT (CHOICS(SCHOIS), ' ', INX)
            NEWMEN = .TRUE.
C                                       Undo STOP Clean this field
         ELSE IF (CHOICS(CHS)(:9).EQ.'ALLOW FLD') THEN
            KBP = 10
            CALL GETNUM (CHOICS(CHS), 16, KBP, X)
            I = X + 0.001
            IF (IGNORE(I).GT.-1.5) IGNORE(I) = 1.0
            WRITE (CHNUMB,1000) TVFLD
            CHOICS(SCHOIS) = 'STOP FLD' // CHNUMB
            CALL REFRMT (CHOICS(SCHOIS), ' ', INX)
            NEWMEN = .TRUE.
C                                       SELECT FIELD n
         ELSE IF (CHOICS(CHS)(:7).EQ.'SELECT ') THEN
            IF (CHOICS(CHS)(:10).EQ.'SELECT FLD') THEN
               KBP = 11
               CALL GETNUM (CHOICS(CHS), 16, KBP, X)
               I = X + 0.001
               LERR = 0
            ELSE
               MSGBUF = 'Enter field number to be selected'
               CALL INQINT (TTY, MSGBUF, 1, I, LERR)
               IF (LERR.GT.0) GO TO 980
               END IF
            IF ((I.LE.0) .OR. (I.GT.MFIELD) .OR. (LERR.NE.0)) I = TVFLD
            LERR = 0
            IF (TVFLD.NE.I) THEN
               CALL OTVCLC (TVNAME, LERR)
               IF (LERR.NE.0) GO TO 980
               TVFLD = I
               DIM(1) = LEN (CNAME(TVFLD))
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY,
     *            CNAME(TVFLD), LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 1
               CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY,
     *            LERR)
               CALL CLBCHK (TVNAME, TVFLD, LERR)
               IF (LERR.NE.0) GO TO 980
               DIM(1) = 8
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,5),
     *            CDUMMY, LERR)
               IF (LERR.NE.0) GO TO 980
               CALL OTVLOD (TVNAME, LERR)
               IF (IGNORE(TVFLD).EQ.-1.0) THEN
                  WRITE (CHNUMB,1000) TVFLD
                  CHOICS(SCHOIS) = 'ALLOW FLD' // CHNUMB
                  CALL REFRMT (CHOICS(SCHOIS), ' ', INX)
               ELSE
                  WRITE (CHNUMB,1000) TVFLD
                  CHOICS(SCHOIS) = 'STOP FLD' // CHNUMB
                  CALL REFRMT (CHOICS(SCHOIS), ' ', INX)
                  END IF
               NEWMEN = .TRUE.
               END IF
            END IF
         IF (LERR.LE.0) GO TO 100
C                                       close downs
 980  CALL TVDCLO (TVNAME, JERR)
C                                       turn off on error
      IF (LERR.NE.0) THEN
         IERR = MAX (1, IERR)
         TVFLD = -1
         DIM(1) = 1
         DIM(2) = 1
         CALL CLNPUT (NAME, 'TVFIELD', OOAINT, DIM, TVFLD, CDUMMY, JERR)
C                                       update Common
      ELSE
         IF (UNBOXD) THEN
            CALL CLNGET (NAME, 'UNBOXES', TYPE, DIM, UNBOXS, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 985
            CALL CLNGET (NAME, 'UNWINDOW', TYPE, DIM, UNWIN, CDUMMY,
     *         LERR)
            IF (LERR.NE.0) GO TO 985
            ISUNBX = .TRUE.
            DO 982 I = 1,MFIELD
               K = (I-1)*MFIELD
               DO 981 J = UNBOXS(I),1,-1
                  IF (UNWIN(1,K+J).NE.0) GO TO 983
                  UNBOXS(I) = J - 1
 981              CONTINUE
 982           CONTINUE
            ISUNBX = .FALSE.
            END IF
 983     CALL CLNGET (NAME, 'NBOXES', TYPE, DIM, NBOXES, CDUMMY, LERR)
         IF (LERR.EQ.0) CALL CLNGET (NAME, 'WINDOW', TYPE, DIM, WIN,
     *      CDUMMY, LERR)
         IF ((LERR.EQ.0) .AND. (REBOXD)) THEN
            MSGSUP = 32000
            CALL OGET (NAME, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL,
     *         LERR)
            MSGSUP = MSGSAV
            IF ((LERR.EQ.0) .AND. (OBXFIL.NE.' ')) THEN
               CALL QCWRBX (OBXFIL, NBOXES, MFIELD, WIN, UNBOXS, UNWIN,
     *            LERR)
               IF (LERR.NE.0) THEN
                  MSGTXT = 'CLNTV: OBOXFILE TURNED OFF DUE TO ERRORS'
                  CALL MSGWRT (6)
                  OBXFIL = ' '
                  CALL OPUT (NAME, 'OBOXFILE', TYPE, DIM, DUMMY,
     *               OBXFIL, LERR)
                  END IF
               END IF
            END IF
         END IF
 985  CALL TVDDES (TVNAME, JERR)
 990  IF (LERR.NE.0) THEN
         MSGTXT = 'CLNTV : ERROR DISPLAYING ' // NAME
         CALL MSGWRT (6)
         END IF
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I6)
 1035 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('FIELD',I5,' SHIFT',2F12.4,' CLEANED',F9.3,1X,A5,'JY',I9,
     *   ' CC')
 1105 FORMAT ('DELETE',I7,' CURRENT BOXES? ANSWER Y/N')
 1110 FORMAT ('Cleaning field',I5,' in next cycle')
      END
      SUBROUTINE CLBCHK (TVNAME, TVFLD, IERR)
C-----------------------------------------------------------------------
C   Check TBLC, TTRC, TXINC, TYINC for image field against WIN and the
C   TV size.  It forces TBLC, TTRC to encompass the full set of windows
C   and then sets TXINC and TYINC to allow the full image to be loaded
C   to the tv.
C   Inputs:
C      TVNAME   C*?      Open TV device object name
C      TVFLD    I        Field number
C   Output:
C      IERR     I        Error code
C   Input from common:
C      CNAME    C(*)*?   Field object names
C      IMSIZE   I(2,*)   Field image size
C      NBOXES   I(*)     Number of boxes
C      WIN      I(4,*)   Clean boxes
C-----------------------------------------------------------------------
      CHARACTER TVNAME*(*)
      INTEGER   TVFLD, IERR
C
      INTEGER   TYPE, DIM(7), TTRC(7), TBLC(7), TXINC, TYINC, MSGSAV, I,
     *   J, LBLC(2), LTRC(2), NGRY, NGRPH, MAXX(2), TVWND(4), CSIZE(2),
     *   NX, NY, IBR, IP
      LOGICAL   DOIT
      REAL      FMAX, FMIN
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       learn about TV
      CALL OTVPRM (TVNAME, NGRY, NGRPH, MAXX, TVWND, CSIZE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       read current parms
      I = 2
      MSGSUP = 32000
      CALL OGET (CNAME(TVFLD), 'TBLC', TYPE, DIM, TBLC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 1, TBLC)
         I = I - 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      TBLC(3) = CHANN
      MSGSUP = 32000
      CALL OGET (CNAME(TVFLD), 'TTRC', TYPE, DIM, TTRC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 1, TTRC)
         TTRC(1) = IMSIZE(1,TVFLD)
         TTRC(2) = IMSIZE(2,TVFLD)
         I = I - 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      TTRC(3) = CHANN
C                                       check WIN
      TBLC(1) = MAX (1, TBLC(1))
      TBLC(2) = MAX (1, TBLC(2))
      IF (TTRC(1).LE.TBLC(1)) TTRC(1) = IMSIZE(1,TVFLD)
      IF (TTRC(2).LE.TBLC(2)) TTRC(2) = IMSIZE(2,TVFLD)
      J = NBOXES(TVFLD)
      IF ((I.GT.0) .AND. (J.GT.0)) THEN
         LTRC(1) = 0
         LTRC(2) = 0
         LBLC(1) = 100000
         LBLC(2) = 100000
         DO 20 I = 1,J
            IP = (I - 1) * MFIELD + TVFLD
            IF (WIN(1,IP).EQ.-1) THEN
               LBLC(1) = MIN (LBLC(1), WIN(3,IP)-WIN(2,IP))
               LBLC(2) = MIN (LBLC(2), WIN(4,IP)-WIN(2,IP))
               LTRC(1) = MAX (LTRC(1), WIN(3,IP)+WIN(2,IP))
               LTRC(2) = MAX (LTRC(2), WIN(4,IP)+WIN(2,IP))
            ELSE
               LBLC(1) = MIN (LBLC(1), WIN(1,IP))
               LBLC(2) = MIN (LBLC(2), WIN(2,IP))
               LTRC(1) = MAX (LTRC(1), WIN(3,IP))
               LTRC(2) = MAX (LTRC(2), WIN(4,IP))
               END IF
 20         CONTINUE
         IF ((LBLC(1).LT.TBLC(1)) .OR. (LBLC(2).LT.TBLC(2)) .OR.
     *      (LTRC(1).GT.TTRC(1)) .OR. (LTRC(2).GT.TTRC(2))) THEN
            MSGTXT = 'Warning: not all boxes fit on display screen'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       set increment
      TXINC = (TTRC(1) - TBLC(1) + 1) / MAX (MAXX(1), 1) + 1
      TYINC = (TTRC(2) - TBLC(2) + 1) / MAX (MAXX(2), 1) + 1
      IF ((TXINC.EQ.1) .AND. (TYINC.EQ.1)) THEN
         TXINC = MAXX(1) / (TTRC(1) - TBLC(1) + 1)
         IF (TXINC.GT.1) THEN
C           IF (TXINC*(TTRC(1)-TBLC(1)+1).GT.520) TXINC = MAX (1,
C    *         TXINC-1)
            TXINC = - MIN (TXINC, 8)
            END IF
         TYINC = MAXX(2) / (TTRC(2) - TBLC(2) + 1)
         IF (TYINC.GT.1) THEN
C           IF (TYINC*(TTRC(2)-TBLC(2)+1).GT.520) TYINC = MAX (1,
C    *         TYINC-1)
            TYINC = - MIN (TYINC, 8)
            END IF
         END IF
      IF (TYINC.LT.TXINC) TYINC = TXINC
      IF (TXINC.LT.TYINC) TXINC = TYINC
C                                       check this!
      IF (TXINC.GT.1) THEN
         I = (TTRC(1) - TBLC(1)) / TXINC
         TTRC(1) = TBLC(1) + I * TXINC
         IF (TTRC(1).LT.LTRC(1)) THEN
            TTRC(1) = MIN (TTRC(1) + TXINC, IMSIZE(1,TVFLD))
            I = (TTRC(1) - TBLC(1)) / TXINC
            J = TBLC(1) + I * TXINC
            IF (J.LT.LTRC(1)) THEN
               TBLC(1) = TTRC(1) - (I+1)*TXINC
               TBLC(1) = MAX(1, TBLC(1))
               END IF
            END IF
         END IF
      IF (TYINC.GT.1) THEN
         I = (TTRC(2) - TBLC(2)) / TYINC
         TTRC(2) = TBLC(2) + I * TYINC
         IF (TTRC(2).LT.LTRC(2)) THEN
            TTRC(2) = MIN (TTRC(2) + TYINC, IMSIZE(2,TVFLD))
            I = (TTRC(2) - TBLC(2)) / TYINC
            J = TBLC(2) + I * TYINC
            IF (J.LT.LTRC(2)) THEN
               TBLC(2) = TTRC(2) - (I+1)*TYINC
               TBLC(2) = MAX(1, TBLC(2))
               END IF
            END IF
         END IF
C                                       window too small?
      DOIT = .FALSE.
      IF (TXINC.GT.0) THEN
         NX = (TTRC(1) - TBLC(1)) / TXINC
      ELSE
         NX = (TTRC(1) - TBLC(1)) * ABS (TXINC)
         END IF
      IF (TYINC.GT.0) THEN
         NY = (TTRC(2) - TBLC(2)) / TYINC
      ELSE
         NY = (TTRC(2) - TBLC(2)) * ABS (TYINC)
         END IF
      IF ((TVWND(3)-TVWND(1)+1.LT.NX) .AND. (NX.LT.MAXX(1))) THEN
         DOIT = .TRUE.
         TVWND(1) = MAXX(1) / 2 - NX/2
         TVWND(1) = MAX (1, TVWND(1))
         IBR = MIN (TVWND(1), 61) - 1
         TVWND(3) = TVWND(1) + NX - 1 + IBR
         TVWND(1) = TVWND(1) - IBR
         TVWND(1) = MAX (1, TVWND(1))
         TVWND(3) = MIN (TVWND(3), MAXX(1))
         END IF
      IF ((TVWND(4)-TVWND(2)+1.LT.NY) .AND. (NY.LT.MAXX(2))) THEN
         DOIT = .TRUE.
         TVWND(2) = MAXX(2) / 2 - NY/2
         TVWND(2) = MAX (1, TVWND(2))
         IBR = MIN (TVWND(2), 61) - 1
         TVWND(4) = TVWND(2) + NY + IBR
         TVWND(2) = TVWND(2) - IBR
         TVWND(2) = MAX (1, TVWND(2))
         TVWND(4) = MIN (TVWND(4), MAXX(2))
         END IF
      IF (DOIT) THEN
         CALL TVDTVW (TVNAME, 'WRITE', TVWND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       check pixrange
      CALL IMGET (CNAME(TVFLD), 'FIELDMIN', TYPE, DIM, FMIN, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGET (CNAME(TVFLD), 'FIELDMAX', TYPE, DIM, FMAX, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      IF (TVFLD.NE.PIXFLD) THEN
         PIXRNG(1) = FMIN
         PIXRNG(2) = FMAX
         PIXFLD = TVFLD
      ELSE
         PIXRNG(1) = MIN (PIXRNG(1), FMIN)
         PIXRNG(2) = MAX (PIXRNG(2), FMAX)
         IF (FMAX-FMIN.LT.0.33*(PIXRNG(2)-PIXRNG(1))) THEN
            PIXRNG(1) = MAX (FMIN, PIXRNG(1))
            PIXRNG(2) = MIN (FMAX, PIXRNG(2))
            END IF
         END IF
C                                       Announce
      IF (TXINC*TYINC.EQ.1) THEN
         WRITE (MSGTXT,1020) TVFLD, PIXRNG
      ELSE IF (TXINC.GT.1) THEN
         WRITE (MSGTXT,1021) TVFLD, PIXRNG, TXINC
      ELSE
         WRITE (MSGTXT,1022) TVFLD, PIXRNG, -TXINC
         END IF
      CALL MSGWRT (2)
C                                       put in object
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (CNAME(TVFLD), 'TXINC', OOAINT, DIM, TXINC, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (CNAME(TVFLD), 'TYINC', OOAINT, DIM, TYINC, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 2
      CALL OPUT (CNAME(TVFLD), 'PIXRANGE', OOARE, DIM, PIXRNG, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 7
      CALL OPUT (CNAME(TVFLD), 'TBLC', OOAINT, DIM, TBLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OPUT (CNAME(TVFLD), 'TTRC', OOAINT, DIM, TTRC, CDUMMY, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Loading field',I5,' to TV from',1PE11.3,' to',1PE11.3)
 1021 FORMAT ('Loading field',I5,' from',1PE10.2,' to',1PE10.2,
     *   ' every',I3,' pixel')
 1022 FORMAT ('Loading field',I5,' from',1PE10.2,' to',1PE10.2,
     *   ' interp by',I2)
      END
      SUBROUTINE CLABOX (NAME, IFIELD, IERR)
C-----------------------------------------------------------------------
C   CLABOX does the autoboxing operation
C   Inputs:
C      NAME     C*(*)   The name of the Clean object
C      IFIELD   I       Choice of facet, 0 => all
C   Output:
C      IERR     I       Error code: 1 non-fatal, > 1 quit
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IFIELD, IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   LFIELD, LF1, LF2, BLC(7), TRC(7), DIM(7), LBOX, IBOX,
     *   IP, WINS(4,MXNBOX), K, MAXDIM, DIM2(7), DIM1(7), TYPE1, TYPE2,
     *   TYPE, DUMMY, MSGSAV, NR, UWINS(4,MXNBOX), UBOX
      REAL      IMAGE(2)
      LONGINT   PIMAGE
      CHARACTER OBXFIL*48
      LOGICAL   REBOXD
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL CLNGET (NAME, 'NBOXES', TYPE1, DIM1, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'WINDOW', TYPE2, DIM2, WIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (IFIELD.GT.0) THEN
         LF1 = IFIELD
         LF2 = IFIELD
      ELSE
         LF1 = 1
         LF2 = MFIELD
         END IF
      MAXDIM = 0
      DO 10 LFIELD = LF1,LF2
         MAXDIM = MAX (MAXDIM, IMSIZE(1,LFIELD)*IMSIZE(2,LFIELD))
 10      CONTINUE
      REBOXD = .FALSE.
C                                       allocate memory for work
      MAXDIM = (MAXDIM-1)/1024 + 1
      CALL ZMEMRY ('GET ', 'CLABOX', MAXDIM, IMAGE, PIMAGE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'DYNAMIC MEMORY INADEQUATE FOR AUTO-BOXING'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 995
         END IF
      DO 100 LFIELD = LF1,LF2
         IF (IGNORE(LFIELD).GE.0.0) THEN
            BLC(1) = 1
            BLC(2) = 1
            TRC(1) = IMSIZE(1,LFIELD)
            TRC(2) = IMSIZE(2,LFIELD)
            BLC(3) = CHANN
            TRC(3) = CHANN
            DIM(1) = 7
            DIM(2) = 1
            CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Open pixel array
            CALL ARROPN (CNAME(LFIELD), 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
C                                       pick up current boxes
            LBOX = NBOXES(LFIELD)
            DO 20 IBOX = 1,LBOX
               IP = (IBOX-1) * MFIELD + LFIELD
               CALL COPY (4, WIN(1,IP), WINS(1,IBOX))
 20            CONTINUE
            UBOX = UNBOXS(LFIELD)
            DO 25 IBOX = 1,UBOX
               IP = (IBOX-1) * MFIELD + LFIELD
               CALL COPY (4, UNWIN(1,IP), UWINS(1,IBOX))
 25            CONTINUE
            NR = (LFIELD - 1) / NFPRES + 1
C                                       do real work
            CALL CLABXW (CNAME(LFIELD), LFIELD, IMSIZE(1,LFIELD),
     *         IMSIZE(2,LFIELD), AUTOBX, ELIMXR(NR), IMAGE(1+PIMAGE),
     *         LBOX, WINS, UBOX, UWINS, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OCLOSE (CNAME(LFIELD), IERR)
C                                       report changes
            DO 30 IBOX = NBOXES(LFIELD)+1,LBOX
               IP = (IBOX-1) * MFIELD + LFIELD
               CALL COPY (4, WINS(1,IBOX), WIN(1,IP))
               WRITE (MSGTXT,1020) LFIELD, (WINS(K,IBOX), K = 1,4)
               CALL MSGWRT (3)
               REBOXD = .TRUE.
 30            CONTINUE
            NBOXES(LFIELD) = LBOX
            END IF
 100     CONTINUE
C                                       store away
      IF (REBOXD) THEN
         CALL CLNPUT (NAME, 'NBOXES', TYPE1, DIM1, NBOXES, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL CLNPUT (NAME, 'WINDOW', TYPE2, DIM2, WIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         MSGSUP = 32000
         CALL OGET (NAME, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IERR)
         MSGSUP = MSGSAV
         IF ((IERR.EQ.0) .AND. (OBXFIL.NE.' ')) THEN
            CALL QCWRBX (OBXFIL, NBOXES, MFIELD, WIN, UNBOXS, UNWIN,
     *         IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'CLABOX: OBOXFILE TURNED OFF DUE TO ERRORS'
               CALL MSGWRT (6)
               OBXFIL = ' '
               CALL OPUT (NAME, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL,
     *            IERR)
               END IF
            END IF
         END IF
      IERR = 0
C                                       free memory
 990  CALL ZMEMRY ('FREE', 'CLABOX', MAXDIM, IMAGE, PIMAGE, LFIELD)
C                                       turn off autobox
 995  IF (IERR.NE.0) THEN
         AUTOBX(1) = 0.0
         MSGTXT = 'AUTO-BOXING TURNED OFF DUE TO ERROR'
         CALL MSGWRT (8)
         IERR = 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Field',I5,' add box',4I6)
      END
      SUBROUTINE CLABXW (IN, FNUM, NX, NY, AUTOBX, ELIMXR, IMAGE, NBOX,
     *   WINS, UBOX, UWINS, IERR)
C-----------------------------------------------------------------------
C   CLABXW does the hard work: reads the image, finds the rms, finds
C   the islands, selects the boxes
C   Inputs:
C      IN       C*(*)    Input image object already open
C      FNUM     I        Facet number
C      NX       I        Number X pixels
C      NY       I        Number Y pixels (rows)
C      AUTOBX   R(6)     Parameters: # boxes, island cut, peak cut,
C                           limit wrt max, extend, edgskp
C   In/Out:
C      ELIMXR   R        Max residual in all facets inside inscribed
C                           ellipse for this resolution
C   Outputs:
C      IMAGE    R(*,*)   Memory to use for image
C      NBOX     I        Number boxes found
C      WINS     I(4,*)   Boxes
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      INTEGER   FNUM, NX, NY, NBOX, WINS(4,*), UBOX, UWINS(4,*), IERR
      REAL      AUTOBX(6), ELIMXR, IMAGE(NX,NY)
C
      INTEGER   IX, IY, DIM(7), NPASS, NPK, PKWIN(4,4096), IBOX, NISLND,
     *   IPK, II, I, IROUND, JJ, SN, IB, IX1, IX2, IY1, IY2
      REAL      ACTN, T, X, Y, R, RX, CX, CY, FMAX(4096), A, B
      DOUBLE PRECISION RSP, RSM, TT, SS, SQ, RM, RS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IBOX = NBOX
      II = 4 * 4096
      CALL FILL (II, 0, PKWIN)
C                                       read the image
      DO 10 IY = 1,NY
         CALL ARREAD (IN, DIM, IMAGE(1,IY), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IY, FNUM
            GO TO 990
            END IF
 10      CONTINUE
      CALL ARRCLO (IN, IERR)
C                                       blank outside ellipse
      A = (NX - 2*AUTOBX(6)) / 2.0
      B = (NY - 2*AUTOBX(6)) / 2.0
      R = A * B
      R = R * R
      DO 20 IY = 1,NY
         Y = (IY - NY/2.0) * A
         Y = Y * Y
         DO 15 IX = 1,NX
            X = (IX - NX/2.0) * B
            X = X * X
            IF (X+Y.GT.R) IMAGE(IX,IY) = FBLANK
 15         CONTINUE
 20      CONTINUE
C                                       blank UNClean boxes
      DO 35 IB = 1,UBOX
C                                       rectangular box
         IF (UWINS(1,IB).NE.-1) THEN
            JJ = UWINS(3,IB) - UWINS(1,IB) + 1
            IX = UWINS(1,IB)
            DO 25 IY = UWINS(2,IB),UWINS(4,IB)
               CALL RFILL (JJ, FBLANK, IMAGE(IX,IY))
 25            CONTINUE
C                                       circular box
         ELSE
            R = UWINS(2,IB)
            R = R * R
            IX1 = UWINS(3,IB) - UWINS(2,IB) - 1
            IX2 = UWINS(3,IB) + UWINS(2,IB) + 1
            IY1 = UWINS(4,IB) - UWINS(2,IB) - 1
            IY2 = UWINS(4,IB) + UWINS(2,IB) + 1
            DO 30 IY = IY1,IY2
               Y = IY - UWINS(4,IB)
               Y = Y * Y
               DO 27 IX = IX1, IX2
                  X = IX - UWINS(3,IB)
                  X = X * X
                  IF (X+Y.LE.R) IMAGE(IX,IY) = FBLANK
 27               CONTINUE
 30            CONTINUE
            END IF
 35      CONTINUE
C                                       we need to find it
      RSP = 1.D10
      RSM = -1.D10
      RX = 0.0
      DO 60 NPASS = 1,7
         SS = 0.0D0
         SQ = 0.0D0
         SN = 0
         DO 50 IY = 1,NY
            DO 40 IX = 1,NX
               T = IMAGE(IX,IY)
               IF (T.NE.FBLANK) THEN
                  RX = MAX (RX, ABS(T))
                  TT = T
                  IF ((TT.LT.RSP) .AND. (TT.GT.RSM)) THEN
                     SS = SS + TT
                     SQ = SQ + TT * TT
                     SN = SN + 1
                     END IF
                  END IF
 40            CONTINUE
 50         CONTINUE
         IF (SN.LE.0.0D0) THEN
            RSP = RSP + 3.0D0 * RS
            RSM = RSP - 3.0D0 * RS
         ELSE
            RM = SS / SN
            SQ = SQ / SN
            RS = SQ - RM * RM
            RS = SQRT (MAX (0.0D0, RS))
            RS = MAX (RS, 0.01D0*RM)
            RSP = RM + 3.0D0 * RS
            RSM = RM - 4.0D0 * RS
            END IF
 60      CONTINUE
      ACTN = RS
C     WRITE (MSGTXT,1060) FNUM, ACTN
C     CALL MSGWRT (3)
C                                       find islands a la SAD
      ACTN = ACTN * AUTOBX(2)
C                                       parameter: diagonal allowed
      NISLND = 4096
      II = 0
 61   CALL ISLAND (NISLND, NX, NY, IMAGE, ACTN, PKWIN, NPK)
      IF (NPK.GT.4050) THEN
         II = II + 1
         ACTN = ACTN * AUTOBX(3) / AUTOBX(2)
         IF (II.LT.4) GO TO 61
         END IF
      II = 0
C                                       find maxima, discard
      ACTN = ACTN * AUTOBX(3) / AUTOBX(2)
      ELIMXR = MAX (ELIMXR, RX)
      RX = AUTOBX(4) * ELIMXR
      RX = MAX (RX, ACTN)
      DO 80 IPK = 1,NPK
         RM = 0.0
         IF ((PKWIN(1,IPK).LE.0) .OR. (PKWIN(2,IPK).LE.0) .OR.
     *      (PKWIN(3,IPK).LT.PKWIN(1,IPK)) .OR. (PKWIN(3,IPK).GT.NX)
     *      .OR. (PKWIN(4,IPK).LT.PKWIN(2,IPK)) .OR.
     *      (PKWIN(4,IPK).GT.NY)) GO TO 80
         DO 70 IY = PKWIN(2,IPK),PKWIN(4,IPK)
            DO 65 IX = PKWIN(1,IPK),PKWIN(3,IPK)
               IF ((IMAGE(IX,IY).NE.FBLANK) .AND.
     *            (ABS(IMAGE(IX,IY)).GT.RM)) THEN
                  RM = ABS (IMAGE(IX,IY))
                  CX = IX
                  CY = IY
                  END IF
 65            CONTINUE
 70         CONTINUE
C                                       peak > peak cutoff
         IF (RM.GT.RX) THEN
C                                       is it in a box already?
            DO 75 I = 1,IBOX
               IF (WINS(1,I).EQ.-1) THEN
                  RS = SQRT ((CX-WINS(3,I))**2 + (CY-WINS(4,I))**2)
                  IF (RS.LE.WINS(2,I)) GO TO 80
               ELSE
                  IF ((CX.GE.WINS(1,I)) .AND. (CX.LE.WINS(3,I)) .AND.
     *               (CY.GE.WINS(2,I)) .AND. (CY.LE.WINS(4,I))) GO TO 80
                  END IF
 75            CONTINUE
C                                       count it
            II = II + 1
            CALL COPY (4, PKWIN(1,IPK), PKWIN(1,II))
            FMAX(II) = RM
            END IF
 80      CONTINUE
C                                       make boxes
      NPK = II
      JJ = IROUND (AUTOBX(5))
      DO 90 IPK = 1,NPK
C                                       find strongest
         RM = 0
         DO 85 I = 1,NPK
            IF (FMAX(I).GT.RM) THEN
               II = I
               RM = FMAX(I)
               END IF
 85         CONTINUE
         IX = PKWIN(3,II) - PKWIN(1,II) + 1
         IY = PKWIN(4,II) - PKWIN(2,II) + 1
         CX = (PKWIN(3,II) + PKWIN(1,II)) / 2.0
         CY = (PKWIN(4,II) + PKWIN(2,II)) / 2.0
C                                       drop single points unless strong
C                                       parameter MINWIDTH=2
         IF (((IX.LT.2) .OR. (IY.LT.2)) .AND. (RM.LT.2.5*ACTN)) GO TO 89
C                                       circle
         IF ((ABS(IX-IY).LE.1) .AND. (MIN(IX,IY).LE.8)) THEN
             IX = (MAX (IX,IY) + 1) / 2
             NBOX = NBOX + 1
             WINS(1,NBOX) = -1
             WINS(2,NBOX) = IX + JJ
             WINS(3,NBOX) = IROUND (CX)
             WINS(4,NBOX) = IROUND (CY)
C                                       rectangle
         ELSE
            NBOX = NBOX + 1
            WINS(1,NBOX) = MAX (1, PKWIN(1,II) - JJ)
            WINS(2,NBOX) = MAX (1, PKWIN(2,II) - JJ)
            WINS(3,NBOX) = MIN (NX, PKWIN(3,II) + JJ)
            WINS(4,NBOX) = MIN (NY, PKWIN(4,II) + JJ)
            END IF
C                                       bail if reached upper limit
         IX = NBOX - IBOX
         IF (IX-IROUND(AUTOBX(1)).GE.0) GO TO 999
 89      FMAX(II) = 0
 90      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLABXW: ERROR DOING ',A,' AT ROW',I7,' FACET',I7)
C1060 FORMAT ('Facet',I5,' using RMS',1PE12.4)
      END
      SUBROUTINE CLSTAT (NAME, IFIELD, IERR)
C-----------------------------------------------------------------------
C   CLSTAT does the IMSTAT function
C   Inputs:
C      NAME     C*(*)   The name of the Clean object
C      IFIELD   I       Choice of facet, 0 => all
C   Output:
C      IERR     I       Error code: 1 non-fatal, > 1 quit
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IFIELD, IERR
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   LFIELD, LF1, LF2, BLC(7), TRC(7), DIM(7), LBOX, IBOX,
     *   IP, WINS(4,MXNBOX), MAXDIM, DIM2(7), DIM1(7), TYPE1, TYPE2,
     *   MSGSAV, NR, UWINS(4,MXNBOX), UBOX
      REAL      IMAGE(2), AREA
      LONGINT   PIMAGE
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL CLNGET (NAME, 'NBOXES', TYPE1, DIM1, NBOXES, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL CLNGET (NAME, 'WINDOW', TYPE2, DIM2, WIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (IFIELD.GT.0) THEN
         LF1 = IFIELD
         LF2 = IFIELD
      ELSE
         LF1 = 1
         LF2 = MFIELD
         END IF
      MAXDIM = 0
      DO 10 LFIELD = LF1,LF2
         MAXDIM = MAX (MAXDIM, IMSIZE(1,LFIELD)*IMSIZE(2,LFIELD))
 10      CONTINUE
C                                       allocate memory for work
      MAXDIM = (MAXDIM-1)/1024 + 1
      CALL ZMEMRY ('GET ', 'CLSTAT', MAXDIM, IMAGE, PIMAGE, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'DYNAMIC MEMORY INADEQUATE FOR IMSTAT'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      DO 100 LFIELD = LF1,LF2
         IF (IGNORE(LFIELD).GE.0.0) THEN
            BLC(1) = 1
            BLC(2) = 1
            TRC(1) = IMSIZE(1,LFIELD)
            TRC(2) = IMSIZE(2,LFIELD)
            BLC(3) = CHANN
            TRC(3) = CHANN
            DIM(1) = 7
            DIM(2) = 1
            CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Open pixel array
            CALL ARROPN (CNAME(LFIELD), 'READ', IERR)
            IF (IERR.NE.0) GO TO 990
C                                       pick up current boxes
            LBOX = NBOXES(LFIELD)
            DO 20 IBOX = 1,LBOX
               IP = (IBOX-1) * MFIELD + LFIELD
               CALL COPY (4, WIN(1,IP), WINS(1,IBOX))
 20            CONTINUE
            UBOX = UNBOXS(LFIELD)
            DO 25 IBOX = 1,UBOX
               IP = (IBOX-1) * MFIELD + LFIELD
               CALL COPY (4, UNWIN(1,IP), UWINS(1,IBOX))
 25            CONTINUE
            NR = (LFIELD - 1) / NFPRES + 1
            AREA = 0.0
            IF ((CELLSG(1).NE.0.0) .AND. (CELLSG(2).NE.0.0)) AREA =
     *         1.1331 * BMAJ(LFIELD) * BMIN(LFIELD) /
     *         ABS (CELLSG(1) * CELLSG(2))
C                                       do real work
            CALL CLSTAW (CNAME(LFIELD), LFIELD, IMSIZE(1,LFIELD),
     *         IMSIZE(2,LFIELD), IMAGE(1+PIMAGE), UBOX, UWINS, AREA,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            CALL OCLOSE (CNAME(LFIELD), IERR)
            END IF
 100     CONTINUE
C                                       free memory
 990  CALL ZMEMRY ('FREE', 'CLSTAT', MAXDIM, IMAGE, PIMAGE, LFIELD)
 995  IERR = 0
C
 999  RETURN
      END
      SUBROUTINE CLSTAW (IN, FNUM, NX, NY, IMAGE, UBOX, UWINS, AREA,
     *   IERR)
C-----------------------------------------------------------------------
C   CLABXW does the hard work: reads the image, finds the rms,
C   reports the results
C   Inputs:
C      IN       C*(*)    Input image object already open
C      FNUM     I        Facet number
C      NX       I        Number X pixels
C      NY       I        Number Y pixels (rows)
C      AREA     R        Clean beam area in pixels
C   Outputs:
C      IMAGE    R(*,*)   Memory to use for image
C      IERR     I        Error code
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      INTEGER   FNUM, NX, NY, UBOX, UWINS(4,*), IERR
      REAL      IMAGE(NX,NY), AREA
C
      INTEGER   IX, IY, DIM(7), NPASS, JJ, SN, IB, IX1, IX2, IY1, IY2,
     *   TN
      REAL      T, X, Y, R, RX, A, B
      DOUBLE PRECISION RSP, RSM, TT, SS, SQ, RM, RS, TS, TQ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       read the image
      DO 10 IY = 1,NY
         CALL ARREAD (IN, DIM, IMAGE(1,IY), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IY, FNUM
            GO TO 990
            END IF
 10      CONTINUE
      CALL ARRCLO (IN, IERR)
C                                       blank outside ellipse
      A = (NX - 10) / 2.0
      B = (NY - 10) / 2.0
      R = A * B
      R = R * R
      DO 20 IY = 1,NY
         Y = (IY - NY/2.0) * A
         Y = Y * Y
         DO 15 IX = 1,NX
            X = (IX - NX/2.0) * B
            X = X * X
            IF (X+Y.GT.R) IMAGE(IX,IY) = FBLANK
 15         CONTINUE
 20      CONTINUE
C                                       blank UNClean boxes
      DO 35 IB = 1,UBOX
C                                       rectangular box
         IF (UWINS(1,IB).NE.-1) THEN
            JJ = UWINS(3,IB) - UWINS(1,IB) + 1
            IX = UWINS(1,IB)
            DO 25 IY = UWINS(2,IB),UWINS(4,IB)
               CALL RFILL (JJ, FBLANK, IMAGE(IX,IY))
 25            CONTINUE
C                                       circular box
         ELSE
            R = UWINS(2,IB)
            R = R * R
            IX1 = UWINS(3,IB) - UWINS(2,IB) - 1
            IX2 = UWINS(3,IB) + UWINS(2,IB) + 1
            IY1 = UWINS(4,IB) - UWINS(2,IB) - 1
            IY2 = UWINS(4,IB) + UWINS(2,IB) + 1
            DO 30 IY = IY1,IY2
               Y = IY - UWINS(4,IB)
               Y = Y * Y
               DO 27 IX = IX1, IX2
                  X = IX - UWINS(3,IB)
                  X = X * X
                  IF (X+Y.LE.R) IMAGE(IX,IY) = FBLANK
 27               CONTINUE
 30            CONTINUE
            END IF
 35      CONTINUE
C                                       we need to find it
      RSP = 1.D10
      RSM = -1.D10
      RX = 0.0
      DO 60 NPASS = 1,7
         SS = 0.0D0
         SQ = 0.0D0
         SN = 0
         TS = 0.0D0
         TQ = 0.0D0
         TN = 0
         DO 50 IY = 1,NY
            DO 40 IX = 1,NX
               T = IMAGE(IX,IY)
               IF (T.NE.FBLANK) THEN
                  RX = MAX (RX, ABS(T))
                  TT = T
                  IF ((TT.LT.RSP) .AND. (TT.GT.RSM)) THEN
                     SS = SS + TT
                     SQ = SQ + TT * TT
                     SN = SN + 1
                     END IF
                  TS = TS + TT
                  TQ = TQ + TT * TT
                  TN = TN + 1
                  END IF
 40            CONTINUE
 50         CONTINUE
         IF (SN.LE.0.0D0) THEN
            RSP = RSP + 3.0D0 * RS
            RSM = RSP - 3.0D0 * RS
         ELSE
            RM = SS / SN
            SQ = SQ / SN
            RS = SQ - RM * RM
            RS = SQRT (MAX (0.0D0, RS))
            RS = MAX (RS, 0.01D0*RM)
            RSP = RM + 3.0D0 * RS
            RSM = RM - 4.0D0 * RS
            END IF
         IF (TN.GT.0) THEN
            TS = TS / TN
            TQ = TQ / TN - TS * TS
            TQ = SQRT (MAX (0.0D0, TQ))
            END IF
 60      CONTINUE
      WRITE (MSGTXT,1060) FNUM, RM, RS
      CALL MSGWRT (3)
      WRITE (MSGTXT,1061) FNUM, TS, TQ
      CALL MSGWRT (3)
      IF (AREA.GT.0.0) THEN
         TS = TS * TN / AREA
         WRITE (MSGTXT,1062) FNUM, TS
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLSTAW: ERROR DOING ',A,' AT ROW',I7,' FACET',I7)
 1060 FORMAT ('Facet',I5,'  robust mean',1PE12.4,'  rms',1PE12.4)
 1061 FORMAT ('Facet',I5,'  full   mean',1PE12.4,'  rms',1PE12.4)
 1062 FORMAT ('Facet',I5,'  total flux ',1PE12.4,'  Jy')
      END
      SUBROUTINE CLNTEL (NAME, UVNAME, IRET)
C-----------------------------------------------------------------------
C   CLNTEL does a TELL operation for the Clean object if specified
C   Input:
C      NAME     C*?    The name of the Clean object
C      UVNAME   C*?    Name of UC data set if any
C   Output:
C      IRET     I      0 except if task ordered to abort
C   Output in common:
C   Output in Clean object:
C      TVFIELD  I      Field to show on TV - only change no <-> yes
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), UVNAME*(*)
      INTEGER   IRET
C
      INTEGER   IERR, TELDIM(2,25), TELTYP(25), NTELL, DIM(7), TYPE, I,
     *   DUMMY, MSGSAV, IVALU(4), TVFLD, IFIELD, IROUND, LENTIM(2),
     *   LFIELD
      CHARACTER TELOBJ*32, TELKEY(25)*8, CDUMMY*1, OPTELL*4, CVALUE*48
      REAL      BEAM(2), VALUE(4), IMPARM(20), PBFSIZ, IM2PRM(40),
     *   FLXRES(10)
      LOGICAL   DOPBFM, FILTRS
      INCLUDE 'QCLEAN.INC'
      EQUIVALENCE (IVALU, VALUE)
C-----------------------------------------------------------------------
      IRET = 0
      MSGSAV = MSGSUP
C                                       get TELL adverbs
      MSGSUP = 32000
      CALL OGET (NAME, 'TELADVRB', TYPE, DIM, DUMMY, TELKEY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DIM(1).NE.8) GO TO 990
      NTELL = DIM(2)
      CALL OGET (NAME, 'TELLDIMS', TYPE, DIM, TELDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DIM(1).NE.2) GO TO 990
      IF (NTELL.NE.DIM(2)) GO TO 990
      CALL OGET (NAME, 'TELLTYPE', TYPE, DIM, TELTYP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DIM(1).NE.NTELL) GO TO 990
      CALL OGET (NAME, 'TELLNAME', TYPE, DIM, DUMMY, TELOBJ, IERR)
      IF (IERR.EQ.1) THEN
         TELOBJ = 'Clean TELL object'
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = MSGSAV
C                                       create TELL object and call TELL
      CALL INTELL (NTELL, TELKEY, TELTYP, TELDIM, TELOBJ, OPTELL, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       process output: die now
      IF (OPTELL.NE.' ') THEN
C                                       Save OPTELL on TELOBJ
         DIM(1) = LEN (OPTELL)
         DIM(2) = 1
         CALL OPUT (TELOBJ, 'OPTELL', OOACAR, DIM, DUMMY, OPTELL, IERR)
         IF (IERR.NE.0) GO TO 990
         MSGTXT = 'CLNTEL: TELL operation code ' // OPTELL //
     *      ' received'
         CALL MSGWRT (4)
         IF (OPTELL.EQ.'ABOR') IRET = 1
         IF (OPTELL.EQ.'QUIT') THEN
            FINISH = .TRUE.
            QUIT = .TRUE.
            END IF
C                                       change parameters
         IF (OPTELL.EQ.'CHAN') THEN
C                                       DOTV: if present and changing
            MSGSUP = 32000
            CALL CLNGET (NAME, 'TVFIELD', TYPE, DIM, TVFLD, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) TVFLD = 0
            CALL OGET (TELOBJ, 'DOTV', TYPE, DIM, IVALU, CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               IF (((IVALU(1).GT.0) .AND. (TVFLD.LE.0)) .OR.
     *            ((IVALU(1).LE.0) .AND. (TVFLD.GT.0))) THEN
                  TVFLD = IVALU(1)
                  CALL CLNPUT (NAME, 'TVFIELD', TYPE, DIM, TVFLD,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
C                                       GAIN
            CALL OGET (TELOBJ, 'GAIN', TYPE, DIM, IVALU, CDUMMY, IERR)
            IF ((IERR.EQ.0) .AND. (VALUE(1).GT.0.0)) THEN
               GAIN = VALUE(1)
               CALL CLNPUT (NAME, 'GAIN', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       FLUX
            CALL OGET (TELOBJ, 'FLUX', TYPE, DIM, IVALU, CDUMMY, IERR)
            IF ((IERR.EQ.0) .AND. (NUMRES.GT.1)) THEN
               CALL OGET (TELOBJ, 'FGAUSS', TYPE, DIM, FLXRES, CDUMMY,
     *            I)
               IF (I.NE.0) CALL RFILL (NUMRES, VALUE, FLXRES)
            ELSE
               CALL RFILL (NUMRES, VALUE(1), FLXRES)
               END IF
            IF (IERR.EQ.0) THEN
               LFIELD = MFIELD / NUMRES
               TYPE = OOARE
               DIM(1) = 1
               DO 10 IFIELD = 1,MFIELD
                  I = (IFIELD - 1) / LFIELD + 1
                  IF (FLXRES(I).GT.0.0) THEN
                     MNFFLX(IFIELD) = FLXRES(I)
                     CALL OPUT (CNAME(IFIELD), 'MINFLUX', TYPE, DIM,
     *                  MNFFLX(IFIELD), CDUMMY, IERR)
                     IF (IERR.NE.0) GO TO 990
                     END IF
 10               CONTINUE
               IF (VALUE(1).GT.0.0) THEN
                  MINFLX = VALUE(1)
                  CALL CLNPUT (NAME, 'MINFLUX', TYPE, DIM, VALUE,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
C                                       FACTOR
            CALL OGET (TELOBJ, 'FACTOR', TYPE, DIM, IVALU, CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               FACTOR = VALUE(1)
               CALL CLNPUT (NAME, 'FACTOR', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       beam
            CALL OGET (TELOBJ, 'BMAJ', TYPE, DIM, IVALU, CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               BEAM(1) = VALUE(1)
               CALL OGET (TELOBJ, 'BMIN', TYPE, DIM, IVALU, CDUMMY,
     *            IERR)
               IF (IERR.EQ.0) THEN
                  BEAM(2) = VALUE(1)
                  CALL OGET (TELOBJ, 'BPA', TYPE, DIM, IVALU, CDUMMY,
     *               IERR)
                  IF ((IERR.EQ.0) .AND. (BEAM(1).GT.0.) .AND.
     *               (BEAM(2).GT.0.)) THEN
                     CALL RFILL (MAXFLD, VALUE(1), BPA)
                     CALL RFILL (MAXFLD, BEAM(1), BMAJ)
                     CALL RFILL (MAXFLD, BEAM(2), BMIN)
                     BEAM(1) = BEAM(1) / 3600.
                     BEAM(2) = BEAM(2) / 3600.
                     BMINAR = 1.E12
                     DO 20 IFIELD = 1,MFIELD
                        VALUE(1) = SQRT (BEAM(1)**2 + COMRES(IFIELD)**2)
                        VALUE(2) = SQRT (BEAM(2)**2 + COMRES(IFIELD)**2)
                        BMAJ(IFIELD) = VALUE(1) * 3600.0
                        BMIN(IFIELD) = VALUE(2) * 3600.0
                        BMINAR = MIN (BMINAR, BMAJ(IFIELD)*BMIN(IFIELD))
                        CALL IMGOPN (CNAME(IFIELD), 'WRIT', IERR)
                        IF (IERR.NE.0) GO TO 990
                        CALL IMPUT (CNAME(IFIELD), 'BEAM.BMAJ', TYPE,
     *                     DIM, VALUE(1), CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
                        CALL IMPUT (CNAME(IFIELD), 'BEAM.BMIN', TYPE,
     *                     DIM, VALUE(2), CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
                        CALL IMPUT (CNAME(IFIELD), 'BEAM.BPA', TYPE,
     *                     DIM, BPA, CDUMMY, IERR)
                        IF (IERR.NE.0) GO TO 990
                        CALL IMGCLO (CNAME(IFIELD), IERR)
                        IF (IERR.NE.0) GO TO 990
 20                     CONTINUE
                     END IF
                  END IF
               END IF
C                                       MINPATCH
            CALL OGET (TELOBJ, 'MINPATCH', TYPE, DIM, IVALU, CDUMMY,
     *         IERR)
            IF ((IERR.EQ.0) .AND. (IVALU(1).GT.0)) THEN
               MINPCH = IVALU(1)
               CALL CLNPUT (NAME, 'MINPATCH', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       NITER
            CALL OGET (TELOBJ, 'NITER', TYPE, DIM, IVALU, CDUMMY,
     *         IERR)
            IF ((IERR.EQ.0) .AND. (IVALU(1).GT.0)) THEN
               CLNLIM = IVALU(1)
               CALL CLNPUT (NAME, 'NITER', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       MAXPIXEL
            CALL OGET (TELOBJ, 'MAXPIXEL', TYPE, DIM, IVALU, CDUMMY,
     *         IERR)
            IF ((IERR.EQ.0) .AND. (IVALU(1).GT.0)) THEN
               MAXRES = IVALU(1)
               CALL CLNPUT (NAME, 'MAXNRES', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       CMETHOD
            CALL OGET (TELOBJ, 'CMETHOD', TYPE, DIM, DUMMY, CVALUE,
     *         IERR)
            IF ((IERR.EQ.0) .AND. (UVNAME.NE.' ')) THEN
               CALL OPUT (UVNAME, 'MODMETH', TYPE, DIM, DUMMY, CVALUE,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       OBOXFILE
            CALL OGET (TELOBJ, 'OBOXFILE', TYPE, DIM, DUMMY, CVALUE,
     *         IERR)
            IF (IERR.EQ.0) THEN
               IF (CVALUE.NE.' ') THEN
                  CALL OBXFIX (NAME, CVALUE, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
C                                       OVERLAP switch
            CALL OGET (TELOBJ, 'OVRSWTCH', TYPE, DIM, VALUE, CDUMMY,
     *         IERR)
            IF ((IERR.EQ.0) .AND. (NUMRES.LE.1)) THEN
               OVRSW = VALUE(1)
               CALL CLNPUT (NAME, 'OVRSWTCH', TYPE, DIM, VALUE, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
C                                       IMAGRPRM
            MSGSUP = 32000
            CALL OGET (TELOBJ, 'IMAGRPRM', TYPE, DIM, IMPARM, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.0) THEN
               IF ((IMPARM(19).GT.0.0) .AND. (IMPARM(19).LE.0.9)) CCFLIM
     *            = IMPARM(19)
               IMPARM(19) = CCFLIM
               IF (IMPARM(20).GT.0.0) THEN
                  RETRY = IMPARM(20)
                  IF (RETRY.LE.0.0) RETRY = 1.05
                  IF (RETRY.LT.1.0) RETRY = 1.0 / RETRY
                  IF (RETRY.GT.5.0) RETRY = 1.05
                  END IF
               IMPARM(20) = RETRY
               IMPARM(11) = MAX (0.0, MIN (IMPARM(11), 1.0))
               IMPARM(12) = MAX (0.0, MIN (IMPARM(12), 0.1))
               IMPARM(13) = MAX (0.0, MIN (IMPARM(13), 1.0))
               IMPARM(14) = MAX (0.0, MIN (IMPARM(14), 1.0))
               IMPARM(15) = MAX (0.0, MIN (IMPARM(15), 1.0))
               IMPARM(16) = MAX (0.0, IMPARM(16))
               MXULST = IMPARM(18) + 0.1
               IF (MXULST.LE.0) MXULST = 10
               IMPARM(18) = MXULST
               CALL OPUT (NAME, 'IMPARM', TYPE, DIM, IMPARM, CDUMMY,
     *            IERR)
               IF (IERR.EQ.0) THEN
                  CALL RCOPY (5, IMPARM(12), MRCTRL)
                  IF (IMPARM(11).GT.0.0) BMSCP = IMPARM(11)
                  END IF
C                                       SDI Clean allowed
               SDIGN = IMPARM(4)
               DIM(1) = 1
               DIM(2) = 1
               CALL OPUT (NAME, 'SDIGAIN', OOARE, DIM, SDIGN, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               IF ((SDICLN.LE.-2) .AND. (SDIGN.GT.0.0)) THEN
                  SDICLN = 0
                  SDINOW = SDICLN
                  END IF
C                                       CC filter allowed
               DIM(1) = 2
               IF (INDATA.NE.' ') THEN
                  FILTRS = IMPARM(9).LT.0.0
                  IMPARM(9) = ABS (IMPARM(9))
                  IF (IMPARM(9).LT.1.1) IMPARM(9) = 3.1
                  CCFILT(1) = IMPARM(8)
                  CCFILT(2) = IMPARM(9)
                  CALL OPUT (NAME, 'CCFILTER', OOARE, DIM, CCFILT,
     *               CDUMMY, IERR)
                  IF (IERR.NE.0) GO TO 990
                  DIM(1) = 1
                  CALL OPUT (NAME, 'CCFILTRS', OOALOG, DIM, FILTRS,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 990
                  DIM(1) = 2
                  END IF
C                                       Scale residuals?
               DOSCAL = IMPARM(5).GT.0.0
               BMSSZ(1) = IROUND (IMPARM(6))
               BMSSZ(2) = IROUND (IMPARM(7))
               IF (BMSSZ(1).LE.0) BMSSZ(1) = 5
               IF (BMSSZ(2).LE.0) BMSSZ(2) = 5
               CALL OPUT (NAME, 'BMSCLSZ', OOAINT, DIM, BMSSZ, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               DIM(1) = 1
               CALL OPUT (NAME, 'SCALERES', OOALOG, DIM, DOSCAL,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       primary beam correction
               PBFSIZ = IMPARM(1)
               DOPBFM = PBFSIZ.GT.0.0
               CALL OPUT (UVNAME, 'DOPBFM', OOALOG, DIM, DOPBFM, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               CALL OPUT (UVNAME, 'PBFSIZ', OOARE, DIM, PBFSIZ, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
C                                       IM2PARM
            MSGSUP = 32000
            CALL OGET (TELOBJ, 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY,
     *         IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.0) THEN
               AUTOBX(1) = IROUND (IM2PRM(1))
               AUTOBX(2) = IM2PRM(2)
               AUTOBX(3) = IM2PRM(3)
               AUTOBX(4) = IM2PRM(4)
               AUTOBX(5) = IROUND (IM2PRM(5))
               AUTOBX(6) = IROUND (IM2PRM(6))
               AUTOBX(1) = MIN (25.0, AUTOBX(1))
               IF (AUTOBX(2).LE.1.5) AUTOBX(2) = 3.
               IF (AUTOBX(3).LT.AUTOBX(2)) AUTOBX(3) = AUTOBX(2) + 2.
               IF (AUTOBX(4).LT.0.01) AUTOBX(4) = 0.1
               IF (AUTOBX(4).GT.0.90) AUTOBX(4) = 0.1
               IF (AUTOBX(5).LT.0.0) AUTOBX(5) = 1.0
               IF (AUTOBX(5).GT.6.0) AUTOBX(5) = 6.0
               IF (AUTOBX(6).LT.1.0) AUTOBX(6) = 5.0
               IF (AUTOBX(6).GT.IMSIZE(1,1)/20.0) AUTOBX(6) = 5.0
               DIM(1) = 6
               CALL OPUT (NAME, 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               LENTIM(1) = IM2PRM(8)
               LENTIM(2) = IM2PRM(9)
               DIM(1) = 2
               CALL OPUT (NAME, 'TVTIMLIM', OOAINT, DIM, LENTIM, CDUMMY,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
         END IF
C
 990  MSGSUP = MSGSAV
C
 999  RETURN
      END
      SUBROUTINE OBXFIX (NAME, CVALUE, IERR)
C-----------------------------------------------------------------------
C   OBXFIX is intended to handle a TELL of a possibly new OBOXFILE name
C   Inputs:
C      NAME     C*(*)   Object name
C      CVALUE   C*(*)   TELL value of OBOXFILE
C   Outputs"
C      IERR     I       Error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), CVALUE*(*)
      INTEGER   IERR
C
      INTEGER   TYPE, DIM(7), JTRIM, ITIME(3), IDATE(3), LUN1, LUN2,
     *   IND1, IND2, I, J, DUMMY, MSGSAV
      CHARACTER OBXFIL*48, TBXFIL*48, TXLINE*132
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /10,11/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       get current OBOXFILE
      CALL OGET (NAME, 'OBOXFILE', TYPE, DIM, DUMMY, OBXFIL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       what sort of new name
      I = JTRIM (CVALUE)
      IF (I.LE.0) GO TO 999
      IF (CVALUE(I:I).EQ.':') THEN
         IF ((OBXFIL(:I).EQ.CVALUE(:I)) .AND.
     *      (OBXFIL(I+1:I+8).EQ.'OBOXfile')) GO TO 999
         CALL ZTIME (ITIME)
         CALL ZDATE (IDATE)
         TBXFIL = CVALUE
         WRITE (TBXFIL(I+1:),1010) IDATE, ITIME
      ELSE
         TBXFIL = CVALUE
         IF (TBXFIL.EQ.OBXFIL) GO TO 999
         END IF
C                                       now open and copy
      CALL ZTXOPN ('QRED', LUN1, IND1, OBXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OLD BOXFILE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      MSGSUP = 32000
      CALL ZTXZAP (LUN2, TBXFIL, I)
      MSGSUP = MSGSAV
      CALL ZTXOPN ('WRIT', LUN2, IND2, TBXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN NEW BOXFILE'
         GO TO 990
         END IF
      DO 10 I = 1,100000
         CALL ZTXIO ('READ', LUN1, IND1, TXLINE, IERR)
         IF (IERR.EQ.2) THEN
            IERR = 0
            GO TO 20
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ OLD BOXFILE'
            GO TO 980
            END IF
         J = JTRIM (TXLINE)
         J = MAX (1, J)
         CALL ZTXIO ('WRIT', LUN2, IND2, TXLINE(:J), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE NEW BOXFILE'
            GO TO 980
            END IF
 10      CONTINUE
 20   CALL OPUT (NAME, 'OBOXFILE', TYPE, DIM, DUMMY, TBXFIL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'PUT NEW BOXFILE IN OBJECT'
         GO TO 980
         END IF
C
 980  IF (IERR.NE.0) CALL MSGWRT (8)
      CALL ZTXCLS (LUN2, IND2, I)
      CALL ZTXCLS (LUN1, IND1, I)
      IF ((IERR.EQ.0) .AND. (OBXFIL(:13).EQ.'HOME:OBOXtemp')) THEN
         CALL ZTXZAP (LUN1, OBXFIL, I)
         END IF
      GO TO 999
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
      CALL ZTXCLS (LUN1, IND1, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBXFIX: ERROR',I4,' ON ',A)
 1010 FORMAT ('OBOXfile',I4,2I2.2,'.',3I2.2)
      END
      SUBROUTINE CLFILT (IRET)
C-----------------------------------------------------------------------
C   CLFILT removes isolated weak Clean components from all of the CC
C   files.
C   Outputs:
C      IRET   I     Error code
C   Output to Common:
C      NCLNG   I(*)   Number of CCs in each field
C      TFLUXG  R      Total CC flux
C      FLUXG   R(*)   CC Flux per field
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'QCLEAN.INC'
      INCLUDE  'INCS:DAPC.INC'
      LONGINT   GPTR, JI, IPTR
      INTEGER   APSIZ, NPX, NPY, NGX, NGY, S, LFIELD, MROW, IX, IY, I,
     *   IY1, IY2, LCCVER, TCCVER, NROW, NNROW, NUMCOL, TYPE, DIM(7),
     *   IROUND, CCROW, INNROW, J1, J2
      REAL      R, RY, XX, YY, FF, PARMS(5), CUTOFF, AX, AY, XFLUX,
     *   PFLUX, TPFLUX
      LOGICAL   DOMAGN
      CHARACTER CCTAB*32, CCTEMP*32, CDUMMY*1, PREFIX*5
C-----------------------------------------------------------------------
      IRET = 0
      TFLUXG = 0.0
      CALL QCLNSZ (APSIZ)
      CUTOFF = ABS (CCFILT(1))
      DOMAGN = CCFILT(1).LT.0.0
      S = CCFILT(2) + 0.0001
      NPX = 2 * S + 1
      NPY = 2 * S + 1
      MSGTXT = 'Filtering out weak isolated Clean components'
      CALL MSGWRT (3)
      IF (DOMAGN) THEN
         WRITE (MSGTXT,1001) CUTOFF, NPX
      ELSE
         WRITE (MSGTXT,1000) CUTOFF, NPX
         END IF
      CALL MSGWRT (3)
      CCTAB = 'Temp input CC for CLFILT'
      CCTEMP = 'Temp filtered CC for CLFILT'
      TPFLUX = 0.0
C                                       Create convolution function
      JI = PSAPOF
      DO 10 IY = 1,NPY
         RY =  (IY - S - 1.0)**2
         DO 5 IX = 1,NPX
            R = RY + (IX - S - 1.0)**2
            IF (SQRT(R).LE.CCFILT(2)) THEN
               APCORE(JI) = 1.0D0
            ELSE
               APCORE(JI) = 0.0D0
               END IF
            JI = JI + 1
 5          CONTINUE
 10      CONTINUE
C                                       Loop over fields w CCs
      GPTR = NPX * NPY + PSAPOF
      CALL ISUM (MFIELD, NCLNG, INNROW)
      DO 100 LFIELD = 1,MFIELD
         FLUXG(LFIELD) = 0.0
         IF (NCLNG(LFIELD).GT.0) THEN
            NGX = IMSIZE(1,LFIELD)
            NGY = IMSIZE(2,LFIELD)
            MROW = (APSIZ - NPX*NPY - 2) / NGX
            MROW = MIN (MROW, NGY)
            LCCVER = CCVER(LFIELD)
            CALL IM2TAB (CNAME(LFIELD), CCTAB, 'CC', LCCVER, IRET)
            IF (IRET.NE.0) GO TO 995
            TCCVER = 0
            CALL IM2TAB (CNAME(LFIELD), CCTEMP, 'CC', TCCVER, IRET)
            IF (IRET.NE.0) GO TO 995
            IY2 = 0
C                                       Do a swath of the grid
 20         IY1 = IY2 + 1
            IY2 = MIN (IY1+MROW-1, NGY)
            IF (IY2.GE.IY1) THEN
               I = NGX * (IY2 - IY1 + 1)
               CALL DFILL (I, 0.0D0, APCORE(GPTR))
C                                       Open CC file(s)
               IF (IY1.EQ.1) THEN
                  NUMCOL = 3
                  IF (COMRES(LFIELD).GT.0.0) NUMCOL = 7
                  CALL OCCINI (CCTAB, 'WRIT', CCROW, NUMCOL, IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL OGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY,
     *               IRET)
                  IF (IRET.NE.0) GO TO 995
                  NNROW = 0
                  CALL OCCINI (CCTEMP, 'WRIT', CCROW, NUMCOL, IRET)
                  IF (IRET.NE.0) GO TO 995
                  END IF
C                                       convolve to grid
               DO 30 I = 1,NROW
                  CCROW = I
                  CALL OTABCC (CCTAB, 'READ', CCROW, NUMCOL, XX, YY, FF,
     *               TYPE, PARMS, IRET)
                  IF (IRET.GT.0) GO TO 995
                  IF (IRET.EQ.0) THEN
                     AY = ICNTRY(LFIELD) + (YY + YPOFF(LFIELD)) * 3600.
     *                  / CELLSG(2)
                     IY = IROUND (AY)
                     IF ((IY.GE.IY1-S) .AND. (IY.LE.IY2+S)) THEN
                        AX = ICNTRX(LFIELD) + (XX + XPOFF(LFIELD)) *
     *                     3600. / CELLSG(1)
                        IX = IROUND (AX)
                        IPTR = (IY - S - IY1) * NGX + GPTR
                        J1 = MAX (IY1, MIN (IY2, IY-S))
                        J2 = MAX (IY1, MIN (IY2, IY+S))
                        J1 = J1 - (IY - S - 1)
                        J2 = J2 - (IY - S - 1)
                        CALL CLGRCV (NPX, J1, J2, APCORE(PSAPOF), NGX,
     *                     IX-S, APCORE(IPTR), FF)
                        END IF
                     END IF
 30               CONTINUE
C                                       Absolute value test
               IF (DOMAGN) THEN
                  JI = GPTR
                  IX = NGX * (IY2 - IY1 + 1)
                  DO 35 IY = 1,IX
                     APCORE(JI) = ABS (APCORE(JI))
                     JI = JI + 1
 35                  CONTINUE
                  END IF
C                                       Re-read, filter, write
               PFLUX = 0.0
               DO 40 I = 1,NROW
                  CCROW = I
                  CALL OTABCC (CCTAB, 'READ', CCROW, NUMCOL, XX, YY, FF,
     *               TYPE, PARMS, IRET)
                  IF (IRET.GT.0) GO TO 995
                  IF (IRET.EQ.0) THEN
                     PFLUX = PFLUX + FF
                     AY = ICNTRY(LFIELD) + (YY + YPOFF(LFIELD)) * 3600.
     *                  / CELLSG(2)
                     IY = IROUND (AY)
                     IF ((IY.GE.IY1) .AND. (IY.LE.IY2)) THEN
                        AX = ICNTRX(LFIELD) + (XX + XPOFF(LFIELD)) *
     *                     3600. / CELLSG(1)
                        IX = IROUND (AX)
                        IPTR = (IY - IY1) * NGX + GPTR + IX - 1
                        IF (APCORE(IPTR).GE.CUTOFF) THEN
                           NNROW = NNROW + 1
                           CCROW = NNROW
                           CALL OTABCC (CCTEMP, 'WRIT', CCROW, NUMCOL,
     *                        XX, YY, FF, TYPE, PARMS, IRET)
                           IF (IRET.NE.0) GO TO 995
                           END IF
                        END IF
                     END IF
 40               CONTINUE
               GO TO 20
               END IF
C                                       Copy back
            DO 50 I = 1,NNROW
               CCROW = I
               CALL OTABCC (CCTEMP, 'READ', CCROW, NUMCOL, XX, YY, FF,
     *            TYPE, PARMS, IRET)
               IF (IRET.NE.0) GO TO 995
               FLUXG(LFIELD) = FLUXG(LFIELD) + FF
               TFLUXG = TFLUXG + FF
               CCROW = I
               CALL OTABCC (CCTAB, 'WRIT', CCROW, NUMCOL, XX, YY, FF,
     *            TYPE, PARMS, IRET)
               IF (IRET.NE.0) GO TO 995
 50            CONTINUE
C                                       Close
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (CCTAB, 'NROW', OOAINT, DIM, NNROW, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 995
            CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, XX, YY, FF, TYPE,
     *         PARMS, IRET)
            IF (IRET.NE.0) GO TO 995
            CALL OTABCC (CCTEMP, 'CLOS', CCROW, NUMCOL, XX, YY, FF,
     *         TYPE, PARMS, IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Delete temporary CC object
            CALL TABDES (CCTAB, IRET)
            CALL TABZAP (CCTEMP, IRET)
            TPFLUX = TPFLUX + PFLUX
            XFLUX = FLUXG(LFIELD)
            IF (XFLUX.NE.0.0) THEN
               CALL METSCA (XFLUX, PREFIX, IRET)
               PFLUX = PFLUX * XFLUX / FLUXG(LFIELD)
            ELSE
               CALL METSCA (PFLUX, PREFIX, IRET)
               END IF
            WRITE (MSGTXT,1050) LFIELD, NCLNG(LFIELD), PFLUX, PREFIX
            CALL MSGWRT (3)
            WRITE (MSGTXT,1051) NNROW, XFLUX, PREFIX
            CALL MSGWRT (3)
            NCLNG(LFIELD) = NNROW
            CALL IMGOPN (CNAME(LFIELD), 'WRIT', IRET)
            IF (IRET.NE.0) GO TO 995
            CALL IMPUT (CNAME(LFIELD), 'BEAM.NITER', OOAINT, DIM,
     *         NCLNG(LFIELD), CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 995
            CALL IMGCLO (CNAME(LFIELD), IRET)
            IF (IRET.NE.0) GO TO 995
            END IF
 100     CONTINUE
C                                       Save in imaging commons
      CALL OUSETF (TFLUXG, MFIELD, FLUXG)
      XFLUX = TFLUXG
      IF (XFLUX.NE.0.0) THEN
         CALL METSCA (XFLUX, PREFIX, IRET)
         TPFLUX = TPFLUX * XFLUX / TFLUXG
      ELSE
         CALL METSCA (TPFLUX, PREFIX, IRET)
         END IF
      CALL ISUM (MFIELD, NCLNG, NNROW)
      WRITE (MSGTXT,1100) INNROW, TPFLUX, PREFIX
      CALL MSGWRT (3)
      WRITE (MSGTXT,1101) NNROW, XFLUX, PREFIX
      CALL MSGWRT (3)
      GO TO 999
C
 995  MSGTXT = 'CLFILT: ERROR FILTERINGING ' // CNAME(LFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Filter below',F10.6,' Jy in',I3,'-pixel diameter area')
 1001 FORMAT ('Filter abs value below',F10.6,' Jy in',I3,
     *   '-pixel diameter area')
 1050 FORMAT ('Field',I5,' filtered',I11,' components of',F10.3,1X,
     *   A5,'Jy')
 1051 FORMAT (16X,'to',I11,' components of',F10.3,1X,A5,'Jy')
 1100 FORMAT ('All fields totaled',I11,' components of',F10.3,1X,A5,
     *   'Jy')
 1101 FORMAT (9X,'now total',I11,' components of',F10.3,1X,A5,'Jy')
      END
      SUBROUTINE CLGRCV (NCX, J1, J2, CONV, NGX, IGX, GRID, FF)
C-----------------------------------------------------------------------
C   CLGRCV convolves a point to a grid
C   Inputs:
C      NCX    I        X-dimen of CONV
C      J1     I        First Y index to use
C      J2     I        Last Y index to use
C      CONV   D(*,*)   Convolving function
C      NGX    I        X-dimension of grid
C      IGX    I        Initial X pixel of GRID that is changed (1 rel)
C      FF     R        Sample to be gridded
C   In/Out:
C      GRID   D(*,*)   Grid at which row 1 (J1=1) would be put
C   Assumes that CONV points at (1,1) of convolving function, GRID
C   points at the first row to be altered when J1 = 1.
C-----------------------------------------------------------------------
      INTEGER   NCX, NGX, IGX, J1, J2
      DOUBLE PRECISION CONV(NCX,*), GRID(NGX,*)
      REAL       FF
C
      INTEGER   I, J, K, K1, K2
C-----------------------------------------------------------------------
      K1 = MAX (1, IGX)
      K2 = MIN (NGX, NCX+IGX-1)
      DO 20 J = J1,J2
         DO 10 K = K1,K2
            I = K - IGX + 1
            GRID(K,J) = GRID(K,J) + FF * CONV(I,J)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE CLBHIS (NAME, IRET)
C-----------------------------------------------------------------------
C   CLBHIS determines the beam maximum and the histogram of maximum beam
C   value outside of a given beam patch.
C   Inputs:
C      NAME  C*?   The name of the Clean object.
C   Input from common:
C      NBMHIS     I    Number of levels in BMHIS.
C      DBNAME     C(*)*32 Name dirty beam object. - first used
C   Input/Output to common:
C      MAXPCH     I    maximum beam patch size to be considered.
C   Output to common:
C      NXBEM      I(*) "X" dimension of beam
C      NYBEM      I(*) "Y" dimension of beam
C      BMHIS      I    An array whose elements have values between 1
C                      and NBMHIS + 1 indicating the maximum abs.
C                      exterior sidelobe for a beam patch whose size
C                      corresponds to the array index.
C                      e.g. for a beam patch of half size I the maximum
C                      fractional absolute sidelobe level exterior to
C                      the beam patch is BMHIS(I) / NBMHIS.
C   Output:
C      IRET  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IRET
C
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   DIM(3), TYPE, NAXIS(7), I, J, IT1, IT2, IT3, IXCEN,
     *   IYCEN, MAXMUM, IPCH, IXTEMP, IYTEMP, BLC(2), TRC(2), APSIZE,
     *   IYT2, MAXRAD
      REAL      TMPLST, BMAX
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
C
C                                       Clean common active?
      IF (.NOT.ACTIVE) THEN
         IRET = 5
         MSGTXT = 'CLBHIS: CLEAN INACTIVE'
         GO TO 990
         END IF
C                                       Zero histogram.
      CALL FILL (2060, 0, BMHIS)
C                                       Clear any window
      CALL ARRCWI (DBNAME(1), IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
      CALL ARROPN (DBNAME(1), 'READ', IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Get size
      CALL ARDGET (DBNAME(1), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IRET)
C                                       Set image size
      NXBEM(1) = NAXIS(1)
      NYBEM(1) = NAXIS(2)
      CALL QCLNSZ (APSIZE)
      IT1 = SQRT ((APSIZE - 1026.0) / 2.0) - 9
      MAXPCH = MIN (2048, MAXPCH, IT1)
      IT1 = NXBEM(1) / 2
      IT2 = NYBEM(1) / 2 - 1
      IT3 = MAXPCH + 1
      MAXPCH = MIN (IT1, IT2, IT3) - 1
C                                       Check to be sure MAXPCH is
C                                       permitted.
      MAXMUM = MIN (MAXPCH, IT1, IT2)
      IYCEN = NYBEM(1) / 2 + 1
      IXCEN = NXBEM(1) / 2 + 1
C                                       Ignore outer beam pixels if
C                                       possible.
      IF ((NXBEM(1).GT.2*MAXMUM) .AND. (NYBEM(1).GT.2*MAXMUM)) THEN
         BLC(1) = 5
         TRC(1) = NXBEM(1) - 5
         BLC(2) = 5
         TRC(2) = NYBEM(1) - 5
      ELSE
         BLC(1) = 1
         TRC(1) = NXBEM(1)
         BLC(2) = 1
         TRC(2) = NYBEM(1)
         END IF
C                                       Initialize accumulators
      CALL RFILL (MAXMUM, 0.0, ROW2)
C                                       do not take corners in hist.
      MAXRAD = MAX (NXBEM(1), NYBEM(1)) / 2
      MAXRAD = MAXRAD * MAXRAD
      DO 60 J = 1,NYBEM(1)
         CALL ARREAD (DBNAME(1), DIM, ROW1, IRET)
         IF (IRET.NE.0) GO TO 995
         IYTEMP = J - IYCEN
         IYTEMP = ABS (IYTEMP) - 1
         IYT2 = IYTEMP * IYTEMP
         IF ((IYT2.LT.MAXRAD) .AND. (J.GE.BLC(2)) .AND. (J.LE.TRC(2)))
     *      THEN
            DO 40 I = BLC(1),TRC(1)
C                                      Compute min. beam patch including
C                                      this pixel. Adjust the index so
C                                      that order in array gives the
C                                      largest EXTERIOR sidelobe.
               IXTEMP = I - IXCEN
               IXTEMP = ABS (IXTEMP) - 1
               IF (IXTEMP*IXTEMP+IYT2.LE.MAXRAD) THEN
                  IPCH = MAX (IYTEMP, IXTEMP, 1)
                  IPCH = MIN (IPCH, MAXMUM)
C                                       Replace current value of
C                                       ROW2(IPCH) if abs. value of
C                                       this pixel is larger.
                  ROW2(IPCH) = MAX (ROW2(IPCH), ABS(ROW1(I)))
                  END IF
 40            CONTINUE
            END IF
 60      CONTINUE
C                                       Close beam
      CALL ARRCLO (DBNAME(1), IRET)
      IF (IRET.NE.0) GO TO 995
C                                       watch for pathology
      BMAX = 0.0
      DO 70 I = 2,MAXMUM
         BMAX = MAX (BMAX, ROW2(I))
 70      CONTINUE
      IF (BMAX.GT.1.0) THEN
         MSGTXT = 'BEAM HISTOGRAM PATHOLOGICAL - DEALING WITH IT'
         CALL MSGWRT (7)
         TMPLST = BMAX
         DO 80 I = 1,MAXMUM
            IF (ROW2(I).LT.TMPLST) THEN
               TMPLST = ROW2(I)
            ELSE
               ROW2(I) = TMPLST
               END IF
 80         CONTINUE
         END IF
C                                       Make sure ROW2 for each beam
C                                       patch is.GE.that for larger
C                                       beam patches.
      DO 170 I = 1,MAXMUM - 1
         J = MAXMUM - I + 1
         IF (ROW2(J-1).LT.ROW2(J)) ROW2(J-1) = ROW2(J)
 170     CONTINUE
C                                       Scale BMHIS to range (1,NBMHIS+1)
      TMPLST = ROW2(1)
      DO 180 I = 1,MAXPCH
         IF (ROW2(I).GT.1.0E-5) THEN
            TMPLST = ROW2(I)
         ELSE
            ROW2(I) = TMPLST
            END IF
         BMHIS(I) = ROW2(I) * NBMHIS + 1.5
 180     CONTINUE
C                                       max dynamic range
      IF (CCFLIM.LE.0.0) THEN
         CCFLIM = (ROW2(5) + ROW2(MINPCH))
         CCFLIM = MAX (0.0, MIN (0.5, CCFLIM))
         WRITE (MSGTXT,1100) CCFLIM
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLBHIS: ERROR MAKING BEAM HISTOGRAM FOR ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CLBHIS: minimum component',F6.3,' of current peak')
      END
      SUBROUTINE CLRHIS (NAME, IFIELD, JFIELD, IRET)
C-----------------------------------------------------------------------
C   Returns maximum pixel value and histogram of pixel values in the
C   residual images.
C      Makes a single pass thru the residuals unless the initial guess
C   of the residual maximum is in error by more than 20%.
C   Inputs:
C      NAME     C*?        The name of the Clean object.
C      IFIELD   I          Field number; 0 -> all
C      JFIELD   I          Actual field number
C   Input from common:
C      NRSBIN   I          Number of bins in RESHIS.
C      MFIELD   I          Number of fields present.
C      CNAME    C(*)*32    Names of associated clean (residual) images.
C      NBOXES   I(*)       Number of boxes given for field
C      WIN      I(4,*)     Boxes for Field WIN(1,*)=-1 indicates a round
C                          box of width WIN(2,*) pixels centered on
C                          pixel (WIN(3,*), WIN(4,*))
C      IMSIZE   I(2,*)     Image sizes
C      CHANN    I          Frequency channel to be cleaned
C      RESMAX   R          Max. abs. residual map value .
C   Output to common:
C      RESHIS   R(*)       The histogram of the distribution of pixel
C                          values.
C   Output:
C      IRET     I          Error return code, 0=OK  -2 -> no pixels
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IFIELD, JFIELD, IRET
C
      INTEGER   LFIELD, IBOX, DIM(7), BLC(7), TRC(7), I, J, NNX, NNY,
     *   INDEX, LIMIT, MAXX, MAXY, MINX, MINY, IX, IY, IDIS2, MXDIS2,
     *   LF1, LF2, IP, NUMCNT, JJ, JX, JY
      LOGICAL   ROUND, DOUNBX, UNBROW, UNBCOL
      REAL      FACT, TRUMAX, JRHIS(16384)
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Clean common active?
      IF (.NOT.ACTIVE) THEN
         IRET = 5
         MSGTXT = 'CLRHIS: CLEAN INACTIVE'
         GO TO 990
         END IF
      TRUMAX = -1.0
C                                       Zero histogram.
 100  CALL RFILL (NRSBIN, 0.0, RESHIS)
      CALL RFILL (NRSBIN, 0.0, JRHIS)
      NUMCNT = 0
      LAXRES = MAXRES
      IF (IFIELD.LE.0) THEN
         IF ((JFIELD.GT.0) .AND. (NUMRES.GT.1)) THEN
            LF1 = ((JFIELD-1) / NFPRES) * NFPRES + 1
            LF2 = LF1 + NFPRES - 1
            LAXRES = LAXRES + MRCTRL(5) * BMAJ(JFIELD) * BMIN(JFIELD) /
     *         BMINAR
         ELSE
            LF1 = 1
            LF2 = MFIELD
            END IF
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         LAXRES = LAXRES + MRCTRL(5) * BMAJ(IFIELD) * BMIN(IFIELD) /
     *      BMINAR
         END IF
C                                       Fill histogram
      FACT = NRSBIN / RESMAX
      DO 200 LFIELD = LF1,LF2
C                                       skip fields that are done
         IF ((IFIELD.LE.0) .AND. ((RSSMAX(LFIELD).LE.MINFLX) .OR.
     *      (RSSMAX(LFIELD).LE.MNFFLX(LFIELD)))) GO TO 200
         IF (IGNORE(LFIELD).LT.-0.5) GO TO 200
C                                       Loop over boxes
         DO 180 IBOX = 1,NBOXES(LFIELD)
            IP = (IBOX - 1) * MFIELD + LFIELD
C                                       Set window
            ROUND = WIN(1,IP) .EQ. -1
            IF (ROUND) THEN
               MXDIS2 = WIN(2,IP)**2
               MINX = WIN(3,IP) - WIN(2,IP)
               MINY = WIN(4,IP) - WIN(2,IP)
               MAXX = WIN(3,IP) + WIN(2,IP)
               MAXY = WIN(4,IP) + WIN(2,IP)
            ELSE
               MINX = WIN(1,IP)
               MINY = WIN(2,IP)
               MAXX = WIN(3,IP)
               MAXY = WIN(4,IP)
               END IF
C                                       Be careful
            BLC(1) = MAX (1, MIN (MINX, MAXX))
            BLC(2) = MAX (1, MIN (MINY, MAXY))
            TRC(1) = MIN (IMSIZE(1,LFIELD), MAX (MINX, MAXX))
            TRC(2) = MIN (IMSIZE(2,LFIELD), MAX (MINY, MAXY))
            BLC(3) = CHANN
            TRC(3) = CHANN
            NNX = TRC(1) - BLC(1) + 1
            NNY = TRC(2) - BLC(2) + 1
            IF ((NNX.LE.0) .OR. (NNY.LE.0)) GO TO 180
            DIM(1) = 7
            DIM(2) = 1
            CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
            CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
            CALL ARROPN (CNAME(LFIELD), 'READ', IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Loop over window doing
C                                       histogram.
            DO 160 J = 1,NNY
               CALL ARREAD (CNAME(LFIELD), DIM, ROW1, IRET)
               IF (IRET.NE.0) GO TO 995
               JY = BLC(2) + J - 1
               IF (ISUNBX) THEN
                  DOUNBX = UNBROW (LFIELD, JY)
               ELSE
                  DOUNBX = .FALSE.
                  END IF
               IF (ROUND) THEN
                  IY = JY - WIN(4,IP)
C                                       Round box
                  DO 140 I = 1,NNX
                     JX = BLC(1) + I - 1
                     IF (DOUNBX) THEN
                        IF (UNBCOL (LFIELD, JX, JY)) GO TO 140
                        END IF
                     IX = JX - WIN(3,IP)
                     IDIS2 = IX*IX + IY*IY
                     IF (IDIS2.LE.MXDIS2) THEN
                        INDEX = ABS (ROW1(I)) * FACT + 0.5
                        INDEX = MAX (1, INDEX)
                        INDEX = MIN (INDEX, NRSBIN)
                        RESHIS(INDEX) = RESHIS(INDEX) + 1.0
                        IF ((LFIELD.EQ.JFIELD) .OR. (JFIELD.EQ.0))
     *                     JRHIS(INDEX) = JRHIS(INDEX) + 1.0
                        NUMCNT = NUMCNT + 1
C                                       Find true max.
                        TRUMAX = MAX (TRUMAX, ABS (ROW1(I)))
                        END IF
 140                 CONTINUE
C                                       Rectangular box
               ELSE
                  DO 150 I = 1,NNX
                     JX = BLC(1) + I - 1
                     IF (DOUNBX) THEN
                        IF (UNBCOL (LFIELD, JX, JY)) GO TO 150
                        END IF
                     INDEX = ABS (ROW1(I)) * FACT + 0.5
                     INDEX = MAX (1, INDEX)
                     INDEX = MIN (INDEX, NRSBIN)
                     RESHIS(INDEX) = RESHIS(INDEX) + 1.0
                     IF ((LFIELD.EQ.JFIELD) .OR. (JFIELD.EQ.0))
     *                  JRHIS(INDEX) = JRHIS(INDEX) + 1.0
                     NUMCNT = NUMCNT + 1
C                                       Find true max.
                     TRUMAX = MAX (TRUMAX, ABS (ROW1(I)))
 150                 CONTINUE
                  END IF
 160           CONTINUE
C                                       Close
            CALL ARRCLO (CNAME(LFIELD), IRET)
            IF (IRET.NE.0) GO TO 995
 180        CONTINUE
 200     CONTINUE
C                                       No fields included
      IF (NUMCNT.LE.0) THEN
         MSGTXT = 'CLRHIS: NO FIELDS THIS PASS ABOVE MINIMUM FLUX'
         CALL MSGWRT (6)
         IRET = -2
         GO TO 999
         END IF
C                                       If actual max, abs value too far
C                                       off, do it again
      IF (ABS (TRUMAX-RESMAX) .GT. 0.2*ABS (RESMAX)) THEN
         RESMAX = TRUMAX
         GO TO 100
         END IF
C                                       Integrate histogram
      LIMIT = NRSBIN - 1
      JJ = NRSBIN + 1
      DO 300 I = 1,LIMIT
         J = NRSBIN - I + 1
         IF (JRHIS(J).LE.2) JJ = J
         RESHIS(J-1) = RESHIS(J-1) + RESHIS(J)
         JRHIS(J-1) = JRHIS(J-1) + JRHIS(J)
 300     CONTINUE
C                                       half way point
      JJ = JJ - 1
      IF (MOD(JJ,2).EQ.1) THEN
         JJ = (JJ + 1) / 2
         SDILIM = JRHIS(JJ)
      ELSE
         JJ = JJ / 2
         SDILIM = (JRHIS(JJ) + JRHIS(JJ+1)) / 2.0
         END IF
      IF (JRHIS(1).GT.0) THEN
         SDILIM = SDILIM / JRHIS(1)
      ELSE
         SDILIM = 0.0
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLRHIS: ERROR IN RESID. HISTOGRAM FOR ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CDECID (IFIELD, DOIT)
C-----------------------------------------------------------------------
C   CDECID determines a beam patch and limiting map value (PATCH and
C   MAPLIM) which will optimize the use of the available memory.
C   If more residuals are in the top histogram bin than will fit then
C   NRESKP is set to a value larger than 1 and indicates the number of
C   residuals to CLMPAC to skip between putting values in the AP.
C   Input:
C      IFIELD I     Field being imaged 0 -> 1
C   Output:
C      DOIT   L     There are points to do
C   Inputs from common:
C      BMHIS  I     max. fractional abs. of sidelobes external to
C                   beam patch I + 1.  See CLBHIS for details.
C      RESHIS I     Histogram of abs residual map values.
C                   See CLRHIS for details.
C      NBMHIS I     Number of levels for fraction of the peak.
C      RESMAX R     Max. abs. residual map value .
C      MAXPCH I     Maximum beam patch size allowed.
C      MFIELD I        Number of fields present.
C      NBOXES I(*)     Number of boxes given for field
C      WIN    I(4,*)   Boxes for Field WIN(1,*)=-1 indicates a round box
C                      of width WIN(2,*) pixels centered on pixel
C                      (WIN(3,*), WIN(4,*))
C      MINPCH I     Minimum beam patch size allowed.
C      LAXRES I     Max. number of residuals loaded.
C      CLNLIM I     Maximum number of clean components desired.
C   Output to common:
C      PATCH  I     Beam patch size (max. distance from the center)
C      MAPLIM R     Minimum abs. map level to be considered.
C                   Only values GREATER than MAPLIM should be
C                   used.
C      NRESKP I     Number of residuals to skip between putting them
C                   into the AP when there are too many to fit.
C      SDICLN I     If in=0 and LAXRES/MAPLIM < 1+GAIN, then set it
C                   to 1 to start SDI Cleaning
C-----------------------------------------------------------------------
      INTEGER   IFIELD
      LOGICAL   DOIT
C
      CHARACTER PREFIX*5
      INTEGER   I, J, JLEV, K, KPAT, MLOOP, IT1, IT2, IAPBSZ, BFIELD,
     *   ITEMP, CORE, APSIZE, MXAPRS, IPMIN, IPMAX, LF1, LF2, IP,
     *   LINPCH
      LOGICAL   LERR
      REAL      XFLUX
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DOIT = .FALSE.
      NRESKP = 1
      CALL QCLNSZ (APSIZE)
      IT1 = MAXPCH * 2 + 3
      BFIELD = MAX (1, IFIELD)
      IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
C                                       CLACLN buffer, field structs
      I = 130 + 2 + 1 + 6 * MAXFLD
C                                       safety
      I = I + 64
      IAPBSZ = MAX (IT1, I)
C                                       Set start location for beam.
      APBEAM = IAPBSZ
C                                       Determine max PATCH used
      IPMAX = 0
      IPMIN = 1000000
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      DO 20 I = LF1,LF2
         DO 10 J = 1,NBOXES(I)
            IP = (J - 1) * MFIELD + I
            IF (WIN(1,IP).EQ.-1) THEN
               IT1 = WIN(2,IP) * 2
               IT2 = IT1
            ELSE
               IT1 = WIN(3,IP) - WIN(1,IP) + 1
               IT2 = WIN(4,IP) - WIN(2,IP) + 1
               END IF
            IPMAX = MAX (IPMAX, IT1, IT2)
            IPMIN = MIN (IPMIN, IT1, IT2)
 10         CONTINUE
 20      CONTINUE
      IF (IPMAX.LT.IPMIN) GO TO 999
C                                       Loop thru beam patch sizes to
C                                       find the largest which, with
C                                       necessary  map points will fit
C                                       in the AP.
C                                       Minimum LINPCH is 3.
      LINPCH = MIN (MINPCH, IPMAX)
      IT1 = MAXPCH - 5
      LINPCH = MAX (MIN (LINPCH, IT1), 3)
      MLOOP = MAXPCH - LINPCH + 1
C                                       Maximum residuals for minor
C                                       cycle
      MXAPRS = (APSIZE - IAPBSZ - ((LINPCH+1.)*2.*LINPCH))/3.
C                                       Require a few residuals.
      MXAPRS = MAX (MXAPRS, 10)
C                                       Hard upper limit
      MXAPRS = MIN (MXAPRS, LAXRES)
C                                       For all allowed beam patches
      DO 50 I = 1,MLOOP
         KPAT = MAXPCH - I + 1
C                                       Ignore beam patches with no info
         IF (BMHIS(KPAT).LE.0) GO TO 50
C                                       Amount of AP core for beam
            CORE = KPAT + 1
            CORE = CORE * (2 * CORE- 1)
C                                       Use value in BMHIS as an index
C                                       for RESHIS.
            JLEV = MIN (BMHIS(KPAT), NBMHIS)
            IF (JLEV.GE.2) THEN
C                                       Make sure some data to be loaded
               ITEMP = JLEV
               DO 30 K = 2,ITEMP
                  IF (RESHIS(JLEV).GT.0.) GO TO 40
                     JLEV = JLEV - 1
 30               CONTINUE
               END IF
C                                       Compute total AP core for this
C                                       PATCH - MAPLIM combination.
 40         CORE = CORE + 3.0 * RESHIS(JLEV) + 0.5
C                                       See if this will fit and if so
C                                       jump out of the loop.
            IF ((CORE.LT.APSIZE - IAPBSZ) .AND.
     *         (RESHIS(JLEV).LE.LAXRES)) GO TO 70
 50      CONTINUE
C                                       If the program gets here, no
C                                       beam patch - map limit would
C                                       fit.  Use minimum beam patch
C                                       and determine which map limit
C                                       will fit.
      KPAT = LINPCH
      DO 60 I = 1,NBMHIS
         JLEV = I
         CORE = KPAT + 1
         CORE = (CORE * (2 * CORE - 1)) + (3 * RESHIS(JLEV))
C                                       If all points fit in AP
C                                       Jump out and use this RMS level
         IF (CORE .LE. APSIZE-IAPBSZ .AND.
     *      RESHIS(JLEV).LE.MXAPRS) GO TO 70
C                                       For all binnng levels
 60      CONTINUE
C                                       If program gets here there is a
C                                       serious problem. e.g. residual
C                                       map has a constant (or nearly)
C                                       value, set NRESKP.
      KPAT = LINPCH - 1
C                                       Use next to highest bin
      JLEV = NBMHIS - 1
C                                       Let user know
      MSGTXT = 'CDECID: CANNOT OBTAIN BEAM PATCH - MAP LIMIT PROPERLY'
      CALL MSGWRT (8)
C                                       A solution was found.
C                                       No smaller than minimum
 70   PATCH = MAX (KPAT+1, LINPCH)
C                                       No bigger than maximum
      PATCH = MIN (PATCH, (NXBEM(BFIELD)/2)-1, (NYBEM(BFIELD)/2)-1,
     *   MAXPCH)
C                                       Min. flux residual to load in AP
      MAPLIM = (RESMAX * (JLEV-0.5) ) / (NBMHIS - 1)
      IF (IFIELD.GT.0) THEN
         CCVLIM = RSSMAX(IFIELD)
      ELSE
         CCVLIM = RESMAX
         END IF
      IF (CCVLIM/MAPLIM.GT.3.0) THEN
         ATLIMI = 0.05*(CCVLIM-MAPLIM) / MAPLIM
      ELSE IF (CCVLIM/MAPLIM.GT.1.5) THEN
         ATLIMI = 0.02*(CCVLIM-MAPLIM) / MAPLIM
      ELSE
         ATLIMI = 0.01*(CCVLIM-MAPLIM) / MAPLIM
         END IF
      CCVLIM = CCVLIM * CCFLIM
C                                       If map is flat, skip a few
      NRESKP = MAX (1., RESHIS(JLEV-1) / REAL (MXAPRS))
C                                       Switch modes?
      IF (MAPLIM.GT.0.0) THEN
         IF (SDILIM.GT.SDIGN) THEN
            IF (SDICLN.EQ.0) THEN
               IF (SDINOW.EQ.0) SDINOW = 1
               SDICLN = 1
               END IF
         ELSE IF (SDICLN.EQ.1) THEN
            IF (SDINOW.EQ.1) SDINOW = 0
            SDICLN = 0
            END IF
         IF (SDICLN.GT.-2) THEN
            WRITE (MSGTXT,1070) SDILIM
            CALL MSGWRT (2)
            END IF
         IF (SDINOW.GT.0) THEN
            IF (IFIELD.GT.0) THEN
               MAPLIM = RSSMAX(IFIELD) * MAX (0.3333, CCFLIM)
            ELSE
               MAPLIM = RESMAX * MAX (0.3333, CCFLIM)
               END IF
            MSGTXT = 'Note: SDI Clean forced, ' //
     *         'would normally be doing BGC'
            IF (SDICLN.LE.0) CALL MSGWRT (6)
         ELSE
            MSGTXT = 'Note: BGC Clean forced, ' //
     *         'would normally be doing SDI'
            IF (SDICLN.GT.0) CALL MSGWRT (6)
            END IF
         END IF
C                                       Display the results.
      XFLUX = MAPLIM
      CALL METSCA (XFLUX, PREFIX, LERR)
      IF (SDINOW.LE.0) THEN
         IF (NRESKP.GT.1) THEN
            WRITE (MSGTXT,1056) NRESKP
            CALL MSGWRT (8)
            END IF
         WRITE (MSGTXT,1990) (PATCH*2)-1, XFLUX, PREFIX
      ELSE
         WRITE (MSGTXT,1991) XFLUX, PREFIX
         END IF
      CALL MSGWRT (2)
      DOIT = .TRUE.
C
 999  RETURN
C-----------------------------------------------------------------------
 1056 FORMAT ('CDECID: Will load every ',I4,' th. residual')
 1070 FORMAT ('CDECID: fraction above RESMAX/2 =',F9.5)
 1990 FORMAT ('BGC Clean: using',I5,' cell beam + residuals > ',F8.2,
     *   1X,A5,'Jy')
 1991 FORMAT ('SDI Clean using residuals > ',F8.2,1X,A5,'Jy')
      END
      SUBROUTINE CLBSHV (IFIELD, IRET)
C-----------------------------------------------------------------------
C   CLBSHV loads the beam patch into memory.
C   Input:
C      IFIELD  I    Field number
C   Inputs from common:
C      APBEAM  I    Start location for beam patch
C      PATCH   I    Beam patch half size (cells).
C      DBNAME  C*32 Name dirty beam object. IFIELD or 1 used on ONEBEM
C      NXBEM   I    "X" dimension of beam
C      NYBEM   I    "Y" dimension of beam
C      CHANN   I    Frequency channel
C      PHAT    R    Prussian helmet spike size
C   Output to common:
C      APRESD  I    start location of the residuals.
C   Output:
C      IRET    I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, IRET
C
      INTEGER   PPATCH, TWOPAT, IPTR, PTRI, PTRJ, APSIZ, CORE, BLC(7),
     *   TRC(7), DIM(7), I, BFIELD
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Limit Clean "AP" memory
      BFIELD = MAX (1, IFIELD)
      IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
      CALL QCLNSZ (APSIZ)
      PPATCH = PATCH
      BLC(1) = NXBEM(BFIELD) / 2
      BLC(2) = NYBEM(BFIELD) / 2 - PATCH + 2
      BLC(3) = CHANN
      TRC(1) = NXBEM(BFIELD) / 2 + PATCH - 1
      TRC(2) = NYBEM(BFIELD) / 2 + PATCH
      TRC(3) = CHANN
      TWOPAT = 2 * PATCH - 1
C                                      Check to make sure the beam
C                                      patch will fit in the AP.
      PTRJ = APBEAM
      CORE = APBEAM + (PATCH * TWOPAT)
      IF (CORE.GT.APSIZ) THEN
         IRET = 6
         WRITE (MSGTXT,1000) CORE
         GO TO 990
         END IF
C                                       Set window
      DIM(1) = 7
      DIM(2) = 1
      DIM(3) = 0
      CALL ARDPUT (DBNAME(BFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARDPUT (DBNAME(BFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
      CALL ARROPN (DBNAME(BFIELD), 'READ', IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Loop thru beam & shove into AP
      PTRI = PTRJ
      DO 50 I = 1, TWOPAT
C                                      Read row of the beam
         CALL ARREAD (DBNAME(BFIELD), DIM, ROW1, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Add Prussian Helmet
         IF ((I.EQ.PATCH) .AND. (PHAT.GT.0.0)) ROW1(1) = ROW1(1) + PHAT
C                                      Load row into AP.
         IPTR = PTRI
         CALL QWR
         CALL QPUT (ROW1, 2, PPATCH, 2)
         CALL QWD
C                                       Move to the proper location.
         CALL QVMOV (2, 1, IPTR, TWOPAT, PPATCH)
         PTRI = PTRI + 1
 50      CONTINUE
C                                       Close
      CALL ARRCLO (DBNAME(BFIELD), IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Compute starting address for
C                                       residuals.
      APRESD = ((PPATCH - 1) * TWOPAT) + IPTR + 2
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLBSHV: ERROR LOADING BEAM FOR ' // DBNAME(BFIELD)
      CALL MSGWRT (8)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLBSHV: YOU NEED',I8,' WORDS OF MEMORY ')
      END
      SUBROUTINE CLMPAC (IFIELD, IRET)
C-----------------------------------------------------------------------
C   CLMPAC takes points from the residual map greater than MAPLIM
C   and places them with their addresses into the array processor.
C   Input:
C      IFIELD   I         Field to do, 0 => all
C   Input from commons:
C      MFIELD   I         Number of fields present.
C      CNAME    C(*)*32   Names of associated clean (residual) images.
C      NBOXES   I(*)      Number of boxes given for field
C      WIN      I(4,*)    Boxes for Field: WIN(1,*)=-1 indicates a
C                         round box of width WIN(2,*) pixels centered
C                         on pixel (WIN(3,*), WIN(4,*))
C      CHANN    I         Frequency channel (plane) imaged
C      IMSIZE   I(2,*)    Image sizes
C      NRESKP   I         Number of residuals to skip between putting
C                         them into the AP when there are too many to
C                         fit.
C   Output to common:
C      APCFLD   I(*)      start addresses of residuals for each field
C      APCLCN   I(*)      element count for each field.
C      RESNUM   I         number of residuals loaded
C   Output:
C      IRET     I         Return error code, 0=>OK, otherwise error.
C                         -1 => none loaded in this field
C   Residual map points greater than MAPLIM and their addresses
C   are placed in memory.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, IRET
C
      INTEGER   IX, IY, IBOX, NX, NY, LFIELD, SKPCNT, LOX, HIX, LOY,
     *   HIY, JPTR, JPTRP1, LPTR, IPTR, PTRJ, APSIZ, BLC(7), TRC(7),
     *   IXOFF, IYOFF, I, J, MAXX, MAXY, MINX, MINY, DIM(7), MXX, MXY,
     *   MNX, MNY, MXDIS2, IDIS2, IIX, IIY, RESCNT, LF1, LF2, RESTOT,
     *   NPIX, MPIX, IP, NUMCC
      LOGICAL   ROUND, DOUNBX, UNBROW, UNBCOL
      REAL      V(2048), X(2048), Y(2048)
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (X(1), ROW2(1)), (Y(1), ROW2(2049)),
     *   (V(1), ROW3(1))
C-----------------------------------------------------------------------
C                                       Limit Clean "AP" memory
      CALL QCLNSZ (APSIZ)
C                                       loop limits
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
C                                       Get first field size
      NX = IMSIZE(1,1)
      NY = IMSIZE(2,1)
C                                       Find max,min Y in boxes
      MINX = 30000
      MAXY = -30000
      MINY = 30000
      MAXX = -30000
      NPIX = 0
      NUMCC = -LAXRES-1
      DO 20 LFIELD = LF1,LF2
         DO 10 IBOX = 1,NBOXES(LFIELD)
            IP = (IBOX-1) * MFIELD + LFIELD
            ROUND = WIN(1,IP).EQ.-1
            IF (ROUND) THEN
               MNX = WIN(3,IP) - WIN(2,IP)
               MNY = WIN(4,IP) - WIN(2,IP)
               MXX = WIN(3,IP) + WIN(2,IP)
               MXY = WIN(4,IP) + WIN(2,IP)
               MPIX = ((MXX-MNX+1) * (MXY-MNY+1) * 3.14/4.0) + 0.5
            ELSE
               MNX = WIN(1,IP)
               MNY = WIN(2,IP)
               MXX = WIN(3,IP)
               MXY = WIN(4,IP)
               MPIX = (MXX-MNX+1) * (MXY-MNY+1)
               END IF
            IF (MPIX.EQ.1) NPIX = 1
            MAXX = MAX (MAXX, MNX, MXX)
            MAXY = MAX (MAXY, MNY, MXY)
            MINX = MIN (MINX, MNX, MXX)
            MINY = MIN (MINY, MNY, MXY)
 10         CONTINUE
 20      CONTINUE
C                                       Initialize pointer
      JPTR = APRESD
      RESCNT = 0
      RESTOT = 0
C                                       Loop thru fields.
      DO 200 LFIELD = LF1,LF2
C                                       Always take first residual
         SKPCNT = NRESKP
C                                       Initialize field residual info
         APCLCN(LFIELD) = 0
         PTRJ = JPTR
         APCFLD(LFIELD) = PTRJ
C                                       Set window
C                                       Be careful
         BLC(1) = MAX (1, MIN (MINX, MAXX))
         BLC(2) = MAX (1, MIN (MINY, MAXY))
         TRC(1) = MIN (IMSIZE(1,LFIELD), MAX (MINX, MAXX))
         TRC(2) = MIN (IMSIZE(2,LFIELD), MAX (MINY, MAXY))
         BLC(3) = CHANN
         TRC(3) = CHANN
         IXOFF = BLC(1) - 1
         IYOFF = BLC(2) - 1
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 0
         CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
         CALL ARROPN (CNAME(LFIELD), 'READ', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Set some constants.
         NX = TRC(1) - BLC(1) + 1
         NY = TRC(2) - BLC(2) + 1
C                                       Loop thru map looking for points
C                                       greater than MAPLIM.
         IPTR = 0
         DO 70 J = 1,NY
            IY = J + IYOFF
C                                      Read row of the map.
            CALL ARREAD (CNAME(LFIELD), DIM, ROW1, IRET)
            IF (IRET.NE.0) GO TO 995
            IF (ISUNBX) THEN
               DOUNBX = UNBROW (LFIELD, IY)
            ELSE
               DOUNBX = .FALSE.
               END IF
C                                       Loop over boxes
            DO 60 IBOX = 1,NBOXES(LFIELD)
               IP = (IBOX-1) * MFIELD + LFIELD
               ROUND = WIN(1,IP).EQ.-1
               IF (ROUND) THEN
                  MXDIS2 = WIN(2,IP)**2
                  IIY = IY - WIN(4,IP)
                  MNX = WIN(3,IP) - WIN(2,IP)
                  MNY = WIN(4,IP) - WIN(2,IP)
                  MXX = WIN(3,IP) + WIN(2,IP)
                  MXY = WIN(4,IP) + WIN(2,IP)
               ELSE
                  MNX = WIN(1,IP)
                  MNY = WIN(2,IP)
                  MXX = WIN(3,IP)
                  MXY = WIN(4,IP)
                  END IF
C                                       Set range of pixel numbers
               LOY = MIN (MNY, MXY)
               HIY = MAX (MNY, MXY)
               IF ((IY.GE.LOY) .AND. (IY.LE.HIY)) THEN
                  LOX = MIN (MNX, MXX)
                  HIX = MAX (MNX, MXX)
C                                       Loop down the row.
                  DO 50 IX = LOX,HIX
                     IF (DOUNBX) THEN
                        IF (UNBCOL (LFIELD, IX, IY)) GO TO 50
                        END IF
C                                       Test for round box
                     IF (ROUND) THEN
                        IIX = IX - WIN(3,IP)
                        IDIS2 = IIX*IIX + IIY*IIY
                        IF (IDIS2.GT.MXDIS2) GO TO 50
                        END IF
                     I = IX - IXOFF
C                                       Check flux
                     IF (ABS (ROW1(I)).LT.MAPLIM) GO TO 50
C                                       Skip residuals if necessary
                     SKPCNT = SKPCNT + 1
                     IF (SKPCNT.GE.NRESKP) SKPCNT = 0
                     IF (SKPCNT.NE.0) GO TO 50
C                                       Check if AP full.
                     PTRJ = JPTR
                     IF (PTRJ+IPTR*3.GE.APSIZ) GO TO 80
                     NUMCC = NUMCC + 1
                     IF (NUMCC.EQ.0) THEN
                        MSGTXT = 'WARNING: > MAXPIXELS LOADED TO AP!!'
                        CALL MSGWRT (8)
                        END IF
                     IPTR = IPTR + 1
                     V(IPTR) = ROW1(I)
                     X(IPTR) = IX
                     Y(IPTR) = IY
                     RESCNT = RESCNT + 1
C                                       Check to see if buffer full.
                     IF (IPTR.GE.256) THEN
C                                       Load
                        JPTRP1 = JPTR + 2
                        CALL QWR
                        CALL QPUT (V, 2, 256, 2)
                        CALL QWD
                        CALL QVMOV (2, 1, JPTRP1, 3, 256)
                        CALL QWR
                        CALL QPUT (Y, 2, 256, 2)
                        JPTRP1 = JPTR + 1
                        CALL QWD
                        CALL QVMOV (2, 1, JPTRP1, 3, 256)
                        CALL QWR
                        CALL QPUT (X, 2, 256, 2)
                        CALL QWD
                        CALL QVMOV (2, 1, JPTR, 3, 256)
                        IPTR = 0
                        JPTR = JPTR + 3 * 256
                        APCLCN(LFIELD) = APCLCN(LFIELD) + 256
                     END IF
 50                  CONTINUE
                  END IF
 60            CONTINUE
 70         CONTINUE
         GO TO 90
C                                       If program gets here AP filled.
 80      MSGTXT = 'CLMPAC: NOT ENOUGH ROOM FOR ALL MAP POINTS '
         CALL MSGWRT (8)
C                                      Write last buffer into memory.
 90      IF (IPTR.GT.0) THEN
C                                       Check if AP full.
            PTRJ = JPTR
            IF (PTRJ+(IPTR*3).GE.APSIZ) THEN
               IPTR = (APSIZ - PTRJ) / 3
               IPTR = MAX (0, IPTR)
               END IF
            LPTR = IPTR
            JPTRP1 = JPTR + 2
            CALL QWR
            CALL QPUT (V, 2, LPTR, 2)
            CALL QWD
            CALL QVMOV (2, 1, JPTRP1, 3, LPTR)
            CALL QWR
            CALL QPUT (Y, 2, IPTR, 2)
            JPTRP1 = JPTR + 1
            CALL QWD
            CALL QVMOV (2, 1, JPTRP1, 3, LPTR)
            CALL QWR
            CALL QPUT (X, 2, LPTR, 2)
            CALL QWD
            CALL QVMOV (2, 1, JPTR, 3, LPTR)
            JPTR = JPTR + (3 * IPTR)
            APCLCN(LFIELD) = APCLCN(LFIELD) + IPTR
            END IF
C                                       Ignore fields with 1 residual
         IF ((APCLCN(LFIELD).LE.1) .AND. (NPIX.NE.1) .AND. (LF2.GT.LF1))
     *      APCLCN(LFIELD) = 0
         RESTOT = RESTOT + APCLCN(LFIELD)
C                                       Close residual
         CALL ARRCLO (CNAME(LFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       End of loop over fields
 200     CONTINUE
C                                       Set and print number of points.
      JPTR = JPTR - APRESD
      RESNUM = RESCNT
      WRITE (MSGTXT,1200) RESNUM
      CALL MSGWRT (2)
C                                       Check that some data loaded
      IF ((RESNUM.LE.0) .OR. (RESTOT.LE.0)) THEN
         IF ((IFIELD.LE.0) .OR. (MFIELD.EQ.1)) THEN
            IRET = 1
            MSGTXT = 'CLMPAC: NO RESIDUAL POINTS LOADED INTO AP, I QUIT'
            GO TO 990
         ELSE
            IRET = -1
            END IF
         END IF
      GO TO 999
C                                       Error occured.
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLMPAC: ERROR LOADING RESIDUALS FOR ' // CNAME(1)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (I8,' Residual map points loaded')
      END
      SUBROUTINE CLSCRS (IRET)
C-----------------------------------------------------------------------
C  Scales the residuals by the ratio of the restoring beam area to the
C  dirty beam area (defined in some region) to insure that the residuals
C  are in the same physical units as the restored components.
C   Input from commons:
C      DBNAME     C(*)*32   Name of dirty beam image. first used
C      NXBEM      I         "X" dimension of beam
C      NYBEM      I         "Y" dimension of beam
C      CHANN      I         Frequency channel to be cleaned
C      BMSSZ      I(2)      "x" and "y" half widths of the box in which
C                           to determine the dirty beam area.
C      MFIELD     I         Number of fields present.
C      CNAME      C(*)*32   Names of associated clean (residual) images.
C      IMSIZE     I(2,*)    Image sizes
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, IXCEN, IYCEN,
     *   NUMPIX
      REAL      SCALE, DTMEAN, DBAREA, RBAREA, CELLSI(7), TBMAJ, TBMIN
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      IFIELD = 1
C                                       Determine dirty beam area
      IXCEN = NXBEM(1) / 2
      IYCEN = NYBEM(1) / 2 + 1
C                                       Set box for dirty beam integral
      BLC(1) = IXCEN - BMSSZ(1)
      BLC(2) = IYCEN - BMSSZ(2)
      BLC(3) = CHANN
      TRC(1) = IXCEN + BMSSZ(1)
      TRC(2) = IYCEN + BMSSZ(2)
      TRC(3) = CHANN
      DIM(1) = 7
      DIM(2) = 1
      CALL ARDPUT (DBNAME(1), 'BLC', OOAINT, DIM, BLC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARDPUT (DBNAME(1), 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Get beam statistics
      CALL ARSSET (DBNAME(1), IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARSGET (DBNAME(1), 'DATAMEAN', TYPE, DIM, DTMEAN, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARSGET (DBNAME(1), 'NUMPIXEL', TYPE, DIM, NUMPIX, CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARRCWI (DBNAME(1), IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Dirty beam area in pixels.
      DBAREA = DTMEAN * NUMPIX
C                                       Get cell size
      CALL IMDGET (DBNAME(1), 'CDELT', TYPE, DIM, CELLSI, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CELLSI(1) = MAX (1.0E-8, ABS (CELLSI(1)))
      CELLSI(2) = MAX (1.0E-8, ABS (CELLSI(2)))
C                                       Loop over fields scaling
      DO 100 IFIELD = 1,MFIELD
C                                       Get Clean restoring beam
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMAJ', TYPE, DIM, TBMAJ,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMIN', TYPE, DIM, TBMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Restoring beam area
         RBAREA = 1.1331 * TBMAJ * TBMIN / (CELLSI(1) * CELLSI(2))
C                                       Scaling
         IF (DBAREA.LE.1.0E-5) DBAREA = RBAREA
         SCALE = RBAREA / DBAREA
C                                       Message
         IF (SCALE.NE.1.0) THEN
            WRITE (MSGTXT,1000) IFIELD, SCALE
            CALL MSGWRT (3)
            END IF
C                                       Do whole plane
         CALL ARRCWI (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Scale
         CALL ARRSMU (CNAME(IFIELD), SCALE, CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 995  MSGTXT = 'CLSCRS: ERROR SCALING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Beginning with field',I5,' scaling residuals by ',F10.5)
      END
      SUBROUTINE CLSCAL (UNDO, IRET)
C-----------------------------------------------------------------------
C   Scales the full restored image to the header Clean beam units
C   Input:
C      UNDO     L         Undo the scaling?
C   Input from commons:
C      MFIELD   I         Number of fields present.
C      CNAME    C(*)*32   Names of associated clean (residual) images.
C   Output to common:
C   Output:
C      IRET     I         Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      LOGICAL   UNDO
      INTEGER   IRET
C
      INTEGER   IFIELD, DIM(7), TYPE, MSGSAV
      REAL      SCALE, TBMAJ, TBMIN, HDRBM(3), TBPA
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
C                                       Loop over fields scaling
      MSGSAV = MSGSUP
      DO 100 IFIELD = 1,MFIELD
C                                       Get Clean restoring beam
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMAJ', TYPE, DIM, TBMAJ,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMIN', TYPE, DIM, TBMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.BPA', TYPE, DIM, TBPA,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         MSGSUP = 32000
         CALL IMGET (CNAME(IFIELD), 'HDRBEAM', TYPE, DIM, HDRBM,
     *      CDUMMY, IRET)
         MSGSUP = 0
         IF (IRET.EQ.1) THEN
            IRET = 0
            HDRBM(1) = 0.0
            HDRBM(2) = 0.0
            HDRBM(3) = 0.0
            TYPE = OOARE
            DIM(1) = 3
            END IF
         IF (IRET.NE.0) GO TO 995
         IF ((HDRBM(1).LE.0.0) .OR. (HDRBM(2).LE.0.0) .OR.
     *      (HBMAJ(IFIELD).NE.HDRBM(1)) .OR.
     *      (HBMIN(IFIELD).NE.HDRBM(2))) THEN
            HDRBM(1) = HBMAJ(IFIELD)
            HDRBM(2) = HBMIN(IFIELD)
            HDRBM(3) = HBPA(IFIELD)
            IF ((HDRBM(1).LE.0.0) .OR. (HDRBM(2).LE.0.0)) THEN
               HDRBM(1) = TBMAJ
               HDRBM(2) = TBMIN
               HDRBM(3) = TBPA
               HBMAJ(IFIELD) = TBMAJ
               HBMAJ(IFIELD) = TBMIN
               HBPA(IFIELD) = TBPA
               END IF
            CALL OPUT (CNAME(IFIELD), 'HDRBEAM', TYPE, DIM, HDRBM,
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 995
            END IF
         SCALE = (HDRBM(1) * HDRBM(2)) /  (TBMAJ * TBMIN)
         IF (UNDO) SCALE = 1.0 / SCALE
C                                       Message
         IF (SCALE.NE.1.0) THEN
            IF (UNDO) THEN
               WRITE (MSGTXT,1000) IFIELD, SCALE
            ELSE
               WRITE (MSGTXT,1001) IFIELD, SCALE
               END IF
            CALL MSGWRT (3)
            END IF
C                                       Do whole plane
         CALL ARRCWI (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Scale
         CALL ARRSMU (CNAME(IFIELD), SCALE, CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 995  MSGTXT = 'CLSCAL: ERROR SCALING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' rescaling by ',F10.5,' to unfix units')
 1001 FORMAT ('Field',I5,' rescaling by ',F10.5,' to fix units')
      END
      SUBROUTINE CLCGEX (IRET)
C-----------------------------------------------------------------------
C   CLCGEX forces the Header Clean beam parameters back into the header
C   It also writes the CG tables.
C   Inputs from common:
C      UVDATA   C*(*)   Name of UV data object
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IFIELD, DIM(7), TYPE, ICGRNO, VER, I, MSGSAV
      CHARACTER CDUMMY*1, TMPTAB*32
      REAL      BMA, BMN, BP
      INCLUDE 'QCLEAN.INC'
      DOUBLE PRECISION FREQ, FREQG(MAXCIF), AVFREQ
      SAVE FREQG
C-----------------------------------------------------------------------
      TMPTAB = 'Temporary table for CLCGEX CG'
      IFIELD = 1
      CALL UVFRQS (UVDATA, FREQ, FREQG, IRET)
      IF (IRET.NE.0) GO TO 995
      FREQ = 0.0D0
      DO 10 I = 1,NCHAV
         FREQ = FREQ + FREQG(I)
 10      CONTINUE
      FREQ = FREQ / NCHAV
C                                       header keyword
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL OGET (CNAME(1), 'AVERFREQ', TYPE, DIM, AVFREQ, CDUMMY, IRET)
      MSGSUP = MSGSAV
      IF ((IRET.EQ.0) .AND. (AVFREQ.GT.0.0D0)) FREQ = AVFREQ
      IRET = 0
      DO 100 IFIELD = 1,MFIELD
C                                       force beam to header for now
         CALL IMGOPN (CNAME(IFIELD), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       get current beam
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMAJ', TYPE, DIM, BMA,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.BMIN', TYPE, DIM, BMN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGET (CNAME(IFIELD), 'BEAM.BPA', TYPE, DIM, BP,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       put header beam
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BMAJ', OOARE, DIM,
     *      HBMAJ(IFIELD), CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BMIN', OOARE, DIM,
     *      HBMIN(IFIELD), CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.BPA', OOARE, DIM,
     *      HBPA(IFIELD), CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       CG table write
         CALL IMGCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 990
         VER = 1
         CALL IM2TAB (CNAME(IFIELD), TMPTAB, 'CG', VER, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OCGINI (TMPTAB, 'WRIT', ICGRNO, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OTABCG (TMPTAB, 'WRIT', ICGRNO, FREQ, BMA, BMN, BP, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL OTABCG (TMPTAB, 'CLOS', ICGRNO, FREQ, BMA, BMN, BP, IRET)
         IF (IRET.NE.0) GO TO 990
         CALL TABDES (TMPTAB, IRET)
         IF (IRET.NE.0) GO TO 990
 100     CONTINUE
      GO TO 999
C
 990  MSGTXT = 'CLCGEX: CG TABLE ERROR ON ' // CNAME(IFIELD)
      CALL MSGWRT (8)
      GO TO 999
C
 995  MSGTXT = 'CLCGEX: HEADER ERROR ON ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLREST (IRET)
C-----------------------------------------------------------------------
C  CLREST computes the Clean components convolved with the restoring
C  beam and adds it to each residual image.  Sums total clean flux.
C   Input from commons:
C      MFIELD     I         Number of fields present.
C      CNAME     C(*)*32   Names of associated clean (residual) images.
C      IMSIZE     I(2,*)    Image sizes
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, DUMMY
      CHARACTER TGRID*32, DATYPE*8, CDUMMY*1
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Restoring Clean components'
      CALL MSGWRT (3)
C                                       Loop over fields.
      DO 100 IFIELD = 1,MFIELD
C                                       Make sure that there are
C                                       components.
         IF (NCLNG(IFIELD).GT.0) THEN
C                                       Grid transform of restoring
C                                       beam.
            CALL CLRGRD (IFIELD, TGRID, IRET)
C                                       Check if CC found.
            IF (IRET.EQ.10) GO TO 100
            IF (IRET.NE.0) GO TO 995
C                                       Work array must be real
            DATYPE = 'REAL'
            DIM(1) = 8
            DIM(2) = 1
            CALL ARDPUT (WORK2, 'DATATYPE', OOACAR, DIM, DUMMY, DATYPE,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
C                                       FFT
            CALL ARRFFT (-1, TGRID, WORK1, WORK2, IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Set window for residual/restored
            CALL ARDGET (CNAME(IFIELD), 'BLC', TYPE, DIM, BLC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
            CALL ARDGET (CNAME(IFIELD), 'TRC', TYPE, DIM, TRC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
            BLC(1) = 1
            BLC(2) = 1
            BLC(3) = CHANN
            TRC(1) = IMSIZE(1,IFIELD)
            TRC(2) = IMSIZE(2,IFIELD)
            TRC(3) = CHANN
            CALL ARDPUT (CNAME(IFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
            CALL ARDPUT (CNAME(IFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Add to residual
            CALL ARRADD (CNAME(IFIELD), WORK2, CNAME(IFIELD), IRET)
            IF (IRET.NE.0) GO TO 995
            END IF
 100     CONTINUE
      IRET = 0
      GO TO 999
C
 995  MSGTXT = 'CLREST: ERROR RESTORING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLMERG (IRET)
C-----------------------------------------------------------------------
C   CLMERG merges the CC files, summing all components on the same cell
C   reducing the total number of components.
C   Input from commons:
C      MFIELD   I         Number of fields present.
C      CNAME    C(*)*32   Names of associated clean (residual) images.
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'QCLEAN.INC'
      INTEGER   IFIELD, NSKOL, LCCVER, NROW, NUMCOL, DIM(7), TYPE, CCROW
      CHARACTER CCTAB*32, MKOL(2)*32, SKOL(1)*32, SRTKOL(2)*32,
     *   CDUMMY*1
      REAL      TOLER(2), PARMS(5), DUM
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Merging the Clean components files'
      CALL MSGWRT (3)
C                                       Loop over fields.
      DO 100 IFIELD = 1,MFIELD
C                                       Merge CC TABLE
C                                       Temporary CC object
         CCTAB = 'Temp CC table for CLMERG'
         LCCVER = CCVER(IFIELD)
         CALL IM2TAB (CNAME(IFIELD), CCTAB, 'CC', LCCVER, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Limit to NCLNG
         NUMCOL = 3
         IF (COMRES(IFIELD).GT.0.0) NUMCOL = 7
         CALL OCCINI (CCTAB, 'WRIT', CCROW, NUMCOL, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL OGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         IF ((NROW.GT.NCLNG(IFIELD)) .AND. (NCLNG(IFIELD).GE.0)) THEN
            WRITE (MSGTXT,1000) IFIELD, NROW, NCLNG(IFIELD)
            CALL MSGWRT (3)
            NROW = NCLNG(IFIELD)
            CALL OPUT (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 995
            END IF
         CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, DUM, DUM, DUM, TYPE,
     *      PARMS, IRET)
C                                       merge the table
         MKOL(1) = 'DELTAX'
         MKOL(2) = 'DELTAY'
C                                       Axis increments
         TOLER(1) = 0.5 * ABS (CELLSG(1)) / 3600.
         TOLER(2) = 0.5 * ABS (CELLSG(2)) / 3600.
         SKOL(1) = 'FLUX'
         NSKOL = 1
         SRTKOL(1) = '-ABS:FLUX'
         SRTKOL(2) = '-ABS:FLUX'
         CALL TBLMRG (CCTAB, MKOL, TOLER, SKOL, NSKOL, SRTKOL, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Now how big is it?
         CALL OCCINI (CCTAB, 'READ', CCROW, NUMCOL, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL OGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, DUM, DUM, DUM, TYPE,
     *      PARMS, IRET)
C                                       Delete temporary CC object
         CALL TABDES (CCTAB, IRET)
         WRITE (MSGTXT,1010) IFIELD, NCLNG(IFIELD), NROW
         CALL MSGWRT (3)
         NCLNG(IFIELD) = NROW
         CALL IMGOPN (CNAME(IFIELD), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'BEAM.NITER', OOAINT, DIM,
     *      NCLNG(IFIELD), CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 100     CONTINUE
      GO TO 999
C
 995  MSGTXT = 'CLMERG: ERROR MERGING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' truncated  from',I10,' to',I10,' components')
 1010 FORMAT ('Field',I5,' compressed from',I10,' to',I10,' components')
      END
      SUBROUTINE CLOVER (IRET)
C-----------------------------------------------------------------------
C   CLOVER computes the Clean components convolved with the restoring
C   beam from those fields overlapping the current field and adds them
C   in to the image.
C   Input from commons:
C      MMFELD   I         Number of fields present.
C      MFIELD   I         Number of fields to be restored
C      CNAME    C(*)*32   Names of associated clean (residual) images.
C      IMSIZE   I(2,*)    Image sizes
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'QCLEAN.INC'
      LONGINT   NCP
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, NOVR, OVR(MAXFLD),
     *   NCNV, JERR, NC, LCCVER, NROW, LFIELD, IX, IX1, IX2, IY,
     *   IY1, IY2, I, CCROW, NUMCOL, NNX, NNY, MSGSAV, APSIZE, JFIELD
      CHARACTER CDUMMY*1, CCTAB*32, PREFIX*5, OUTEMP*32
      REAL      XYI(7), XYL(7), XYC(2,4), AA, BB, CC, SR, CR, PARMS(5),
     *   X, Y, F, D, FSUM, FSCALE, CCFLUX
      LOGICAL   FIRST
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (MMFELD.LE.1) GO TO 999
      MSGTXT = 'Restoring overlapped Clean components'
      CALL MSGWRT (3)
      FIRST = .TRUE.
      MSGSAV = MSGSUP
C                                       Loop over fields.
      DO 100 IFIELD = 1,MMFELD
C                                       IF there are components.
         CALL FILL (MAXFLD, 0, OVR)
         NOVR = 0
         IF (NCLNG(IFIELD).GT.0) THEN
C                                       Are there overlaps
            CALL RFILL (7, 1.0, XYI)
            DO 20 LFIELD = 1,MFIELD
               IF (LFIELD.NE.IFIELD) THEN
                  XYI(1) = 1.0
                  XYI(2) = 1.0
                  MSGSUP = 32000
                  CALL PSNCVT (CNAME(IFIELD), XYI, CNAME(LFIELD), XYL,
     *               IRET)
                  MSGSUP = MSGSAV
                  IF (IRET.NE.0) GO TO 20
                  CALL RCOPY (2, XYL, XYC(1,1))
                  XYI(1) = IMSIZE(1,IFIELD)
                  MSGSUP = 32000
                  CALL PSNCVT (CNAME(IFIELD), XYI, CNAME(LFIELD), XYL,
     *               IRET)
                  MSGSUP = MSGSAV
                  IF (IRET.NE.0) GO TO 20
                  CALL RCOPY (2, XYL, XYC(1,2))
                  XYI(2) = IMSIZE(2,IFIELD)
                  MSGSUP = 32000
                  CALL PSNCVT (CNAME(IFIELD), XYI, CNAME(LFIELD), XYL,
     *               IRET)
                  MSGSUP = MSGSAV
                  IF (IRET.NE.0) GO TO 20
                  CALL RCOPY (2, XYL, XYC(1,3))
                  XYI(1) = 1.0
                  MSGSUP = 32000
                  CALL PSNCVT (CNAME(IFIELD), XYI, CNAME(LFIELD), XYL,
     *               IRET)
                  MSGSUP = MSGSAV
                  IF (IRET.NE.0) GO TO 20
                  CALL RCOPY (2, XYL, XYC(1,4))
                  IF ((XYC(1,1).LT.1.0) .AND. (XYC(1,2).LT.1.0) .AND.
     *               (XYC(1,3).LT.1.0) .AND. (XYC(1,4).LT.1.0)) GO TO 20
                  IF ((XYC(1,1).GT.IMSIZE(1,LFIELD)) .AND.
     *               (XYC(1,2).GT.IMSIZE(1,LFIELD)) .AND.
     *               (XYC(1,3).GT.IMSIZE(1,LFIELD)) .AND.
     *               (XYC(1,4).GT.IMSIZE(1,LFIELD))) GO TO 20
                  IF ((XYC(2,1).LT.1.0) .AND. (XYC(2,2).LT.1.0) .AND.
     *               (XYC(2,3).LT.1.0) .AND. (XYC(2,4).LT.1.0)) GO TO 20
                  IF ((XYC(2,1).GT.IMSIZE(2,LFIELD)) .AND.
     *               (XYC(2,2).GT.IMSIZE(2,LFIELD)) .AND.
     *               (XYC(2,3).GT.IMSIZE(2,LFIELD)) .AND.
     *               (XYC(2,4).GT.IMSIZE(2,LFIELD))) GO TO 20
                   OVR(LFIELD) = 1
                   NOVR = NOVR + 1
                   END IF
 20             CONTINUE
            END IF
C                                       There are overlaps
         IF (NOVR.GT.0) THEN
            IF (FIRST) THEN
               CALL APOBJ ('OPEN', 'CLOVER', IRET)
               IF (IRET.NE.0) GO TO 995
               FIRST = .FALSE.
               END IF
C                                       Merge CC TABLE
C                                       Temporary CC object
            CCTAB = 'Temp CC table for CLOVER'
            LCCVER = CCVER(IFIELD)
            CALL IM2TAB (CNAME(IFIELD), CCTAB, 'CC', LCCVER, IRET)
            IF (IRET.NE.0) GO TO 995
C                                       Loop through images
            DO 90 LFIELD = 1,MFIELD
               IF (BMAJ(IFIELD)*BMIN(IFIELD).GT.
     *            BMAJ(LFIELD)*BMIN(LFIELD)) THEN
                  JFIELD = IFIELD
               ELSE
                  JFIELD = LFIELD
                  END IF
C                                       Prepare parameters
               SR = SIN ((BPA(JFIELD)+MROTAT)*DG2RAD)
               CR = COS ((BPA(JFIELD)+MROTAT)*DG2RAD)
               AA = ((CR/BMIN(JFIELD))**2 + (SR/BMAJ(JFIELD))**2) *
     *            (CELLSG(1)**2) * 4.0 * LOG (2.0)
               BB = ((SR/BMIN(JFIELD))**2 + (CR/BMAJ(JFIELD))**2) *
     *            (CELLSG(2)**2) * 4.0 * LOG (2.0)
               CC = (1.0 / (BMIN(JFIELD)**2) - 1.0 / (BMAJ(JFIELD)**2))
     *            * SR * CR * ABS(CELLSG(1)*CELLSG(2)) * 8.0 * LOG (2.0)
               NCNV = 4 * BMAJ(JFIELD) / MIN (ABS(CELLSG(1)),
     *            ABS(CELLSG(2))) + 0.75
               NCNV = (NCNV / 2)
               NCP = PSAPOF
               NC = 0
C                                       Read overlapped components to AP
C                                       memory converting pixels
               IF (OVR(LFIELD).EQ.1) THEN
                  FSCALE = BMAJ(JFIELD) * BMIN(JFIELD)
                  IF (FSCALE.LE.0.0) THEN
                     FSCALE = 1.0
                  ELSE
                     FSCALE = BMAJ(LFIELD) * BMIN(LFIELD) / FSCALE
                     IF (FSCALE.LE.0.0) FSCALE = 1.0
                     END IF
                  CALL OCCINI (CCTAB, 'READ', CCROW, NUMCOL, IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL OGET (CCTAB, 'NROW', TYPE, DIM, NROW, CDUMMY,
     *               IRET)
                  IF (IRET.NE.0) GO TO 995
                  FSUM = 0.0
                  DO 30 I = 1,NROW
                     CCROW = I
                     CALL OTABCC (CCTAB, 'READ', CCROW, NUMCOL, XYI(1),
     *                  XYI(2), CCFLUX, TYPE, PARMS, IRET)
                     IF (IRET.GT.0) GO TO 995
                     APCORE(NCP) = CCFLUX * FSCALE
                     IF (IRET.EQ.0) THEN
                        XYI(1) = ICNTRX(IFIELD) + (XYI(1) +
     *                     XPOFF(IFIELD)) * 3600. / CELLSG(1)
                        XYI(2) = ICNTRY(IFIELD) + (XYI(2) +
     *                     YPOFF(IFIELD)) * 3600. / CELLSG(2)
                        MSGSUP = 32000
                        CALL PSNCVT (CNAME(IFIELD), XYI, CNAME(LFIELD),
     *                     XYL, IRET)
                        MSGSUP = MSGSAV
                        IF ((IRET.EQ.0) .AND. (XYL(1).GT.-NCNV) .AND.
     *                     (XYL(2).GT.-NCNV) .AND.
     *                     (XYL(1).LT.IMSIZE(1,LFIELD)+NCNV) .AND.
     *                     (XYL(2).LT.IMSIZE(2,LFIELD)+NCNV)) THEN
                           APCORE(NCP+1) = XYL(1)
                           APCORE(NCP+2) = XYL(2)
                           FSUM = FSUM + APCORE(NCP)
                           NCP = NCP + 3
                           NC = NC + 1
                           APSIZE = PSAPNW * 1024
                           IF (NCP-PSAPOF+1.GT.APSIZE-3) THEN
                              WRITE (MSGTXT,1020) NC, IFIELD, LFIELD
                              CALL MSGWRT (6)
                              GO TO 40
                              END IF
                           END IF
                        END IF
 30                  CONTINUE
 40               CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, XYI(1),
     *               XYI(2), CCFLUX, TYPE, PARMS, IRET)
                  END IF
C                                       convolve list to image
               IF (NC.GT.0) THEN
                  FSUM = FSUM / FSCALE
                  FLUXG(LFIELD) = FLUXG(LFIELD) + FSUM
                  CALL METSCA (FSUM, PREFIX, I)
                  IF ((IFIELD.LE.999) .AND. (LFIELD.LE.999)) THEN
                     WRITE (MSGTXT,1040) FSUM, PREFIX, NC, IFIELD,
     *                  LFIELD
                  ELSE
                     WRITE (MSGTXT,1041) FSUM, PREFIX, NC, IFIELD,
     *                  LFIELD
                     END IF
                  CALL MSGWRT (3)
                  CALL FILL (7, 0, BLC)
                  CALL FILL (7, 0, TRC)
                  BLC(1) = 1
                  BLC(2) = 1
                  BLC(3) = CHANN
                  TRC(1) = IMSIZE(1,LFIELD)
                  TRC(2) = IMSIZE(2,LFIELD)
                  TRC(3) = CHANN
                  NNX = IMSIZE(1,LFIELD)
                  NNY = IMSIZE(2,LFIELD)
                  DIM(1) = 7
                  DIM(2) = 1
                  CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC,
     *               CDUMMY, IRET)
                  IF (IRET.NE.0) GO TO 995
C                                       make temp output object
                  OUTEMP = 'Temporary image'
                  CALL OCOPY (CNAME(LFIELD), OUTEMP, IRET)
                  IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
                  CALL ARROPN (CNAME(LFIELD), 'WRIT', IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL ARROPN (OUTEMP, 'READ', IRET)
                  IF (IRET.NE.0) GO TO 995
                  DO 70 IY = 1,NNY
                     IY1 = IY - NCNV
                     IY2 = IY + NCNV
                     CALL ARREAD (OUTEMP, DIM, ROW1, IRET)
                     IF (IRET.NE.0) GO TO 995
C                                       loop through comps to see if any
C                                       APPLY
                     NCP = PSAPOF - 1
                     DO 60 I = 1,NC
                        NCP = NCP + 3
                        IF ((APCORE(NCP).GE.IY1) .AND.
     *                     (APCORE(NCP).LE.IY2)) THEN
                           Y = APCORE(NCP)
                           X = APCORE(NCP-1)
                           F = APCORE(NCP-2)

                           IX1 = X + 1.0 - NCNV
                           IX1 = MAX (1, IX1)
                           IX2 = X + NCNV
                           IX2 = MIN (NNX, IX2)
                           DO 50 IX = IX1,IX2
                              D = (IX-X)*(IX-X)*AA + (IY-Y)*(IY-Y)*BB +
     *                           (IX-X)*(IY-Y)*CC
                              ROW1(IX) = ROW1(IX) + F * EXP(-D)
 50                           CONTINUE
                           END IF
 60                     CONTINUE
                     CALL ARRWRI (CNAME(LFIELD), DIM, ROW1, IRET)
                     IF (IRET.NE.0) GO TO 995
 70                  CONTINUE
                  CALL ARRCLO (CNAME(LFIELD), IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL ARRCLO (OUTEMP, IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL OCLOSE (CNAME(LFIELD), IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL OCLOSE (OUTEMP, IRET)
                  IF (IRET.NE.0) GO TO 995
                  CALL DESTRY (OUTEMP, IRET)
                  IF (IRET.NE.0) GO TO 995
                  END IF
 90            CONTINUE
C                                       Delete temporary CC object
            CALL TABDES (CCTAB, IRET)
            END IF
 100     CONTINUE
      IF (.NOT.FIRST) CALL APOBJ ('FREE', 'CLOVER', IRET)
      IRET = 0
      GO TO 999
C
 995  MSGTXT = 'CLOVER: ERROR RESTORING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
      IF (.NOT.FIRST) CALL APOBJ ('FREE', 'CLOVER', JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('MEMORY FILLED WITH',I9,' OVERLAPPED COMPONENTS, FIELDS',
     *   2I4)
 1040 FORMAT ('Restore',F8.3,1X,A5,'Jy (',I6,' CCs) from field',I4,
     *   ' to field',I4)
 1041 FORMAT ('Restr',F8.3,1X,A5,'Jy (',I6,' CCs) from field',I5,
     *   ' to field',I5)
      END
      SUBROUTINE CLRGRD (IFIELD, TGRID, IRET)
C-----------------------------------------------------------------------
C   CLRGRD restores Clean components from field IFIELD to TGRID file.
C   This is done by transforming the Clean components, multiplying by
C   the appropriate Gaussian function and placing in the TGRID file.
C   If several passes must be used then scratch array object WORK2 is
C   used to accumulate the grid.
C   NOTE: this routine directly accesses "AP" memory.
C   Inputs:
C      IFIELD  I       Field number to process
C   Inputs in common:
C      CNAME  C(*)*32 Names of associated clean (residual) images.
C      NCLNG   I(*)    Number of components per field
C      BMAJ    R       Major axis size (FWHP in sec).
C      BMIN    R       Minor axis size (FWHP in sec).
C      BPA     R       Position angle of major axis (degrees).
C      GAUSAA  R       Coefficient of u**2
C      GAUSBB  R       Coefficient of u*v
C      GAUSCC  R       Coefficient of v**2
C   Outputs:
C      TGRID    C*?     Name of scratch IMAGE object for results.
C      IRET    I       Return code, 0=>OK
C-----------------------------------------------------------------------
      CHARACTER TGRID*(*)
      INTEGER   IFIELD, IRET
C
      INTEGER   NLOAD, I, JLIM, NX, NY, IAPGAU, IAPEX1, IAPEXP, IAPRES,
     *   IAPGRD, IAPCMP, IAPCMS, NUMBER, APBUF, ONENY, TWONY, ISIZE,
     *   MSIZE, WRK1, WRK2, APSIZ, FIRST, NUMCLN, MAXCMP, KAP, NPASS,
     *   IPASS, DIM(7), NDIM, NAXIS(7), TRC(7), BLC(7), NCR(7), DUMMY,
     *   JERR
      REAL      TEMP
      CHARACTER IGRID*32, OGRID*32, XGRID*32, CDUMMY*1
      LOGICAL   APOPEN
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      NX = IMSIZE(1,IFIELD)
      NY = IMSIZE(2,IFIELD)
      APOPEN = .FALSE.
C                                       Define GRID half plane complex
C                                       image.
      NDIM = 2
      NAXIS(1) = NY
      NAXIS(2) = NX / 2 + 1
      CALL FILL (7, 1, BLC)
      CALL COPY (7, NAXIS, TRC)
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      IF (GRID.EQ.' ') THEN
         GRID = 'Grid for Component restoration'
         CALL FILL (7, 1, NCR)
C                                       Find biggest field
         MSIZE = 0
         JLIM = IFIELD
         DO 10 I = 1,MFIELD
            NCR(1) = IMSIZE(1,I) + 2
            NCR(2) = IMSIZE(2,I)
            CALL MAPSIZ (7, NCR, ISIZE)
            IF (ISIZE.GT.MSIZE) THEN
               JLIM = I
               MSIZE = ISIZE
               END IF
 10         CONTINUE
         NCR(1) = IMSIZE(1,JLIM) + 2
         NCR(2) = IMSIZE(2,JLIM)
         CALL IMGSCR (GRID, NCR, IRET)
         IF (IRET.NE.0) GO TO 995
         END IF
C                                       Determine Area assignments in AP
C                                       Note: corresponding definitions
C                                       in CLGAUS.
      ONENY = NY
      TWONY = 2*NY
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
C                                       IAPRES = row restoring function
      IAPRES = 100
C                                       IAPEXP = Gaussian work array
      IAPEXP = IAPRES + WRK1
C                                       IAPCMP = FT of components
      IAPCMP = IAPEXP + WRK2
C                                       IAPGAU = Array of Gaussian
C                                       factors in X
      IAPGAU = IAPCMP + WRK2
C                                       IAPEX1 = Array of Gaussian
C                                       factors in Y
      IAPEX1 = IAPGAU + WRK1
C                                       IAPGRD = Resultant uv grid
      IAPGRD = IAPEX1 + WRK1
C                                       IAPCMS = CC structure array
      IAPCMS = IAPGRD + (2 * NY) + 1
C                                       Number of components
      NUMCLN = NCLNG(IFIELD)
C                                       open AP at desired size or larger
      APSIZ = IAPCMS + 5 * NUMCLN
      APSIZ = APSIZ / 1024 + 2
      CALL QINIT (APSIZ, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'CLRGRD: FAILED TO GET AP MEMORY'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 995
         END IF
      CALL APOBJ ('OPEN', 'CLNGRD', IRET)
      IF (IRET.NE.0) GO TO 995
      APOPEN = .TRUE.
C                                       How much "AP" memory
      APSIZ = 1024 * PSAPNW
C                                       Determine maximum number of comp
C                                       per pass.
      MAXCMP = (APSIZ - IAPCMS) / 5.0
C                                       Make sure "AP" big enough.
      IF (MAXCMP.LT.MIN(10,NUMCLN)) THEN
         MSGTXT = 'CLRGRD: MEMORY TOO SMALL FOR IMAGE'
         IRET = 5
         GO TO 990
         END IF
C                                       How many passes?
      NPASS = (1.0 * NUMCLN) / (1.0 * MAXCMP) + 0.9999
      NPASS = MAX (1, NPASS)
      CALL ARROPN (GRID, 'WRIT', IRET)
      IF (IRET.NE.0) GO TO 995
      DIM(1) = 1
      CALL ARDPUT (GRID, 'NDIM', OOAINT, DIM, NDIM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      DIM(1) = 7
      CALL ARDPUT (GRID, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARRCWI (GRID, IRET)
      IF (IRET.NE.0) GO TO 995
      DIM(1) = 8
      CALL ARDPUT (GRID, 'DATATYPE', OOACAR, DIM, DUMMY, 'COMPLEX',
     *   IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ARRCLO (GRID, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Prepare WORK2 if necessary
      IF (NPASS.GT.1) THEN
         CALL ARROPN (WORK2, 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
         DIM(1) = 1
         CALL ARDPUT (WORK2, 'NDIM', OOAINT, DIM, NDIM, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         DIM(1) = 7
         CALL ARDPUT (WORK2, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARRCWI (WORK2, IRET)
         IF (IRET.NE.0) GO TO 995
         DIM(1) = 8
         CALL ARDPUT (WORK2, 'DATATYPE', OOACAR, DIM, DUMMY, 'COMPLEX',
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARRCLO (WORK2, IRET)
         IF (IRET.NE.0) GO TO 995
         END IF
C                                       Want grid to end up in GRID
C                                       Even number of passes
      IF ((NPASS/2)*2.EQ.NPASS) THEN
         IGRID = GRID
         OGRID = WORK2
C                                       Odd number
      ELSE
         IGRID = WORK2
         OGRID = GRID
         END IF
      FIRST = 1
      APBUF = 2
C                                       Determine size of AP buffer for
C                                       components.
      NLOAD = IAPGAU - 3
C                                       Prepare work arrays in AP.
      CALL CLGAUS (IFIELD)
C                                       Begin component loop.
      IPASS = 1
 20   CONTINUE
C                                       Load Clean components this pass.
         NUMBER = MIN (MAXCMP, NUMCLN)
         NUMBER = NUMBER
         CALL CLCCRM (IFIELD, .FALSE., .TRUE., IAPCMS, APBUF, FIRST,
     *      NUMBER, NLOAD, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Prepare IAPEXP using weight left
C                                       in AP loc 0 by CLGAUS.
         CALL QVCLR (IAPEXP, 1, ONENY)
         CALL QVSADD (IAPEXP, 1, 0, IAPEXP, 1, ONENY)
C                                       Begin loop thru grid
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                       Generate restoring factors.
            TEMP = (-0.5 * GAUSCC * (I - 1.0) ** 2 )
            CALL QPUT (TEMP, 1, 1, 2)
            CALL QWAIT
            CALL QVSADD (IAPGAU, 1, 1, IAPRES, 1, ONENY)
            CALL QVADD (IAPRES, 1, IAPEXP, 1, IAPRES, 1, ONENY)
            CALL QVEXP (IAPRES, 1, IAPRES, 1, ONENY)
C                                       Prepare IAPEXP for next row.
            CALL QVADD (IAPEXP, 1, IAPEX1, 1, IAPEXP, 1, ONENY)
            CALL QWR
C                                       Clear IAPCMP.
            CALL QVCLR (IAPCMP, 1, TWONY)
C                                       Do direct transform in X.
            CALL QDIRAD (IAPCMS, 5, IAPCMP, NUMBER)
            CALL QWR
C                                       Do FFT in Y.
            CALL QCFFT (IAPCMP, ONENY, 1)
            CALL QWR
C                                       Multiply by restoring function.
            CALL QCRVMU (IAPCMP, 2, IAPRES, 1, IAPCMP, 2, ONENY)
            CALL QWR
C                                       Initialize or read previous
            IF (IPASS.EQ.1) THEN
C                                       Clear IAPGRD
               WRK1 =  (2 * NY)
               CALL QVCLR (IAPGRD, 1, WRK1)
            ELSE
C                                       Read previous
               CALL ARDRED (IGRID, DIM, APCORE(PSAPOF+IAPGRD), IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
C                                       Add to GRID file.
            CALL QVADD (IAPCMP, 1, IAPGRD, 1, IAPGRD, 1, TWONY)
            CALL QWR
C                                       Write grid
            DIM(1) = NY
            CALL ARDWRI (OGRID, DIM, APCORE(PSAPOF+IAPGRD), IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Rotate DFT for next pass.
            CALL QCVMUL (IAPCMS+1, 5, IAPCMS+3, 5, IAPCMS+1, 5, NUMBER,
     *         1)
            CALL QWR
 150        CONTINUE
C                                       End of this pass.
C                                       Close objects
         CALL ARRCLO (OGRID, IRET)
         IF (IRET.NE.0) GO TO 995
         IF (IPASS.GT.1) THEN
            CALL ARRCLO (IGRID, IRET)
            IF (IRET.NE.0) GO TO 995
            END IF
C                                       Swap input and output grids
         TGRID = OGRID
         XGRID = IGRID
         IGRID = OGRID
         OGRID = XGRID
C                                       If another pass needed
         FIRST = FIRST + NUMBER
         NUMCLN = NUMCLN - NUMBER
         IPASS = IPASS + 1
C                                       Check for more Clean components.
         IF (FIRST.LE.NCLNG(IFIELD)) GO TO 20
      CALL APOBJ ('CLOS', 'CLNGRD', JERR)
      CALL QRLSE
      GO TO 999
C
 990  CALL MSGWRT (8)
 995  IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('CLOS', 'CLNGRD', JERR)
         END IF
      MSGTXT = 'CLRGRD: ERROR RESTORING ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLCCRM (IFIELD, DOSUM, DOREST, APLO, APBUF, FIRST,
     *   NUMBER, NLOAD, IRET)
C-----------------------------------------------------------------------
C   CLCCRM loads Clean components for field IFIELD into the "AP" in
C   preparation for transformation to the data plane.  The signs of the
C   cell offsets are adjusted for flips made to the image.
C   Clean components loaded into the AP in blocks of 5 words arrainged
C   as follows:
C         0 = Y (integer form)
C         1 = FLUX * cos(UX)
C         2 = FLUX * sin(UX)
C         3,4 = cos, sin(X)
C   Inputs:
C      IFIELD   I     Current field number.
C      DOSUM    L     If true sum flux densities.
C      DOREST   L     If true restoring components
C      APLO     I     AP start location for the component array.
C      APBUF    I     AP start location for the buffer.
C      FIRST    I     First Clean component to be loaded.
C      NLOAD    I     AP buffer size.
C   Input in common:
C      CNAME    C(*)*?   Clean objects per field
C      CCVER    I(*)     Version numbers of the fields.
C      IMSIZE   I(2,*)   Image size per field.
C      ICNTRX   I(*)     Image center X pixel per field
C      ICNTRY   I(*)     Image center Y pixel per field
C      XPOFF    R(*)     X ref. pixel offset from center shift per
C                        field (deg)
C      YPOFF    R(*)     Y ref. pixel offset from center shift per
C                        field (deg)
C   Output in common:
C      ROW1     R(*)  Work array
C      ROW2     R(*)  Work array
C      ROW3     R(*)  Work array
C   Input/output:
C     NUMBER    I     Number of clean components loaded.
C   Output:
C     IRET      I     Return error code, 0=OK, 10=>no Comps found,
C                                       otherwise fatal error.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, APLO, APBUF, FIRST, NUMBER, NLOAD, IRET
      LOGICAL   DOSUM, DOREST
C
      CHARACTER CCTAB*32
      INTEGER   IPOINT, ICOUNT, JCOUNT, LIMIT, NCOUNT, I,
     *   APLOC, APLOC1, APLOC3, APLOC4, NNCNT, APLOC2, CCROW, NUMCOL,
     *   MTYPE, NX, NY
      REAL      CFLUX(2048), TEMP(6), TWOPIX, RAC(2048), DECC(2048),
     *   XCEN, YCEN, X, Y, FLUX, PARMS(3)
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'QCLEAN.INC'
      EQUIVALENCE (ROW1, CFLUX), (ROW2, RAC),  (ROW3, DECC)
C-----------------------------------------------------------------------
C                                       Field size
      NX = IMSIZE(1,IFIELD)
      NY = IMSIZE(2,IFIELD)
C                                       Determine center.
         XCEN = ICNTRX(IFIELD)
         YCEN = ICNTRY(IFIELD)
         XCEN = ((XCEN - IMSIZE(1,IFIELD)/2) * CELLSG(1))
     *      / 3600. + XPOFF(IFIELD)
         YCEN = ((YCEN - IMSIZE(2,IFIELD)/2 - 1) * CELLSG(2))
     *      / 3600. + YPOFF(IFIELD)
C                                       Restoring use NX/2+1, NY/2+1 as
C                                       center
      IF (DOREST) THEN
         XCEN = XCEN - CELLSG(1) / 3600.0
         END IF
C                                       Make CC table object
      CCTAB = 'Temporary CC table for CLCCRM'
      CALL IM2TAB (CNAME(IFIELD), CCTAB, 'CC', CCVER(IFIELD), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Open Clean component file.
      CALL OCCINI (CCTAB, 'READ', CCROW, NUMCOL, IRET)
      IF (IRET.NE.0) GO TO 990
      MTYPE = 0
C                                       Init pointers and counters
      APLOC = APLO
      ICOUNT = 0
      JCOUNT = 0
      IPOINT = FIRST
C                                       Begin loading into computer buff
C                                       Make sure to load in small
C                                       enough pieces.
      LIMIT = MABFSS / 8
      IF (LIMIT.GT.NUMBER) LIMIT = NUMBER
      LIMIT = MIN (LIMIT, NLOAD, 2048)
C                                       Jump to here if more passes
C                                       necessary
 30   CONTINUE
         NCOUNT = 0
         DO 50 I = 1,LIMIT
            CCROW = IPOINT
            CALL OTABCC (CCTAB, 'READ', CCROW, NUMCOL, X, Y, FLUX,
     *         MTYPE, PARMS, IRET)
            IF (IRET.LT.0) GO TO 45
            IF (IRET.GT.0) GO TO 990
C                                       Deal with component.
            IF ((NUMCOL.EQ.3) .OR. (MTYPE.LE.1)) THEN
               NCOUNT = NCOUNT + 1
               CFLUX(NCOUNT) = FLUX
               RAC(NCOUNT) = X + XCEN
               DECC(NCOUNT) = Y + YCEN
               IF (DOSUM) THEN
                  FLUXG(IFIELD) = FLUXG(IFIELD) + FLUX
                  TFLUXG = TFLUXG + FLUX
                  END IF
               END IF
 45         IPOINT = IPOINT + 1
 50         CONTINUE
         IF (NCOUNT.LE.0) GO TO 55
         NNCNT = NCOUNT
C                                       Form AP indexes.
         APLOC4 = APLOC + 4
C                                       Load data into AP buffer APBUF.
C                                       Load flux density (FLUX*cos(ux))
         CALL QPUT (CFLUX, APBUF, NNCNT, 2)
         CALL QWD
         APLOC1 = APLOC + 1
         CALL QVMOV (APBUF, 1, APLOC1, 5, NNCNT)
C                                        Load and fix dec cell numbers.
         CALL QWR
         CALL QPUT (DECC, APBUF, NNCNT, 2)
         CALL QWD
         APLOC2 = APLOC + 2
C                                       Move values before fixing.
         CALL QVMOV (APBUF, 1, APLOC, 5, NNCNT)
         CALL QWR
C                                       Set constants in AP.
         TEMP(1) = (-NY/2.) * CELLSG(2) / 3600.
         TEMP(2) = (NY/2.0-1.0) * CELLSG(2) / 3600.
         TEMP(3) = NY * CELLSG(2) / 3600.
         TEMP(4) = 2.0 / (CELLSG(2) / 3600.)
         TEMP(5) = 0.0
         TEMP(6) = -0.5 * CELLSG(2) / 3600.
         CALL QPUT (TEMP, 1, 6, 2)
         CALL QWD
C                                       Put declinations in the range
C                                       (0,NY-1) and multiply by 2,fix.
C                                       Use APLOC2 for temporary use.
         CALL QVFILL (6, APLOC2, 5, NNCNT)
         CALL QVCLIP (APLOC, 5, 1, 2, APLOC, 5, NNCNT)
         CALL QLVGT (APLOC2, 5, APLOC, 5, APLOC2, 5, NNCNT)
         CALL QVSMUL (APLOC2, 5, 3, APLOC2, 5, NNCNT)
         CALL QVADD (APLOC2, 5, APLOC, 5, APLOC, 5, NNCNT)
         CALL QVSMAF (APLOC, 5, 4, 5, APLOC, 5, NNCNT)
C                                       Load and float RA cell numbers.
         CALL QWR
         CALL QPUT (RAC, APBUF, NNCNT, 2)
         CALL QWD
         APLOC3 = APLOC + 3
         CALL QVMOV (APBUF, 1, APLOC3, 5, NNCNT)
C                                       Store -2*PI/NX/CELLSG(1)
         TWOPIX = (-TWOPI/NX) / (CELLSG(1) / 3600.)
         CALL QWR
         CALL QPUT (TWOPIX, 1, 1, 2)
         CALL QWD
C                                       Scale RA's by -2*PI/NX/CELLSG(1)
         CALL QVSMUL (APLOC3, 5, 1, APLOC3, 5, NNCNT)
C                                       Clear APLOC+2 (FLUX*sin(ux) ).
         CALL QVCLR (APLOC2, 5, NNCNT)
C                                       Take sine and cosine of
C                                       RA to 3 and 4.
         CALL QVSIN (APLOC3, 5, APLOC4, 5, NNCNT)
         CALL QVCOS (APLOC3, 5, APLOC3, 5, NNCNT)
 55      ICOUNT = ICOUNT + LIMIT
         JCOUNT = JCOUNT + NCOUNT
C                                       If load complete close CC
C                                       file and return.
C                                       Update APLOC.
         APLOC = APLOC + (NNCNT * 5)
         IF (ICOUNT.GE.NUMBER) GO TO 60
            IF (NUMBER-ICOUNT.LT.LIMIT) LIMIT = NUMBER - ICOUNT
C                                       Return for another load.
            GO TO 30
C                                     Make sure CC loaded.
 60   IF (JCOUNT.LE.0) IRET = 10
      NUMBER = JCOUNT
C                                       Close CC table
      CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, X, Y, FLUX,
     *   MTYPE, PARMS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Delete temporary CC object
      CALL TABDES (CCTAB, IRET)
      IF (IRET.NE.0) GO TO 990
      GO TO 999
C
 990  CALL MSGWRT (8)
      MSGTXT = 'CLCCRM: ERROR READING COMPONENTS FOR ' // CNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLMXMN (IRET)
C-----------------------------------------------------------------------
C   CLMXMN checks and sets the max min in the image header for the full
C   image up to the current channel.
C   Input from commons:
C      MFIELD     I         Number of fields present.
C      CNAME     C(*)*32   Names of associated clean (residual) images.
C      IMSIZE     I(2,*)    Image sizes
C   Output to common:
C   Output:
C      IRET       I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IFIELD, BLC(7), TRC(7), DIM(7), TYPE, LOOP7, LOOP6,
     *   LOOP5, LOOP4, LOOP3, LOOP2, LOOP1, LROW, NAXIS(7)
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      REAL      DATMAX, DATMIN, FLDMAX, FLDMIN, VALUE, ROW(MABFSS)
      SAVE ROW
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Checking image max/min'
      CALL MSGWRT (3)
C                                       Loop over fields.
      DO 800 IFIELD = 1,MFIELD
C                                       Set window for residual/restored
         CALL ARDGET (CNAME(IFIELD), 'BLC', TYPE, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDGET (CNAME(IFIELD), 'TRC', TYPE, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         BLC(1) = 1
         BLC(2) = 1
         BLC(3) = 1
         TRC(1) = IMSIZE(1,IFIELD)
         TRC(2) = IMSIZE(2,IFIELD)
         TRC(3) = 0
         CALL ARDPUT (CNAME(IFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (CNAME(IFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open array and get I/O stream
         CALL ARROPN (CNAME(IFIELD), 'READ', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Window
         CALL ARRWIN (CNAME(IFIELD), BLC, TRC, NAXIS, IRET)
         IF (IRET.NE.0) GO TO 995
         DATMAX = -1.0E25
         DATMIN = 1.0E25
         FLDMAX = -1.0E25
         FLDMIN = 1.0E25
C                                       Loop over array
         LROW = TRC(1) - BLC(1) + 1
         DO 700 LOOP7 = BLC(7),TRC(7)
            DO 600 LOOP6 = BLC(6),TRC(6)
               DO 500 LOOP5 = BLC(5),TRC(5)
                  DO 400 LOOP4 = BLC(4),TRC(4)
                     DO 300 LOOP3 = BLC(3),TRC(3)
                        DO 200 LOOP2 = BLC(2),TRC(2)
C                                       read row
         CALL ARREAD (CNAME(IFIELD), DIM, ROW, IRET)
         IF (IRET.GT.0) GO TO 995
C                                       Do statistics
         DO 120 LOOP1 = 1,LROW
            VALUE = ROW(LOOP1)
            IF (VALUE.NE.FBLANK) THEN
               DATMAX = MAX (DATMAX, VALUE)
               DATMIN = MIN (DATMIN, VALUE)
               IF (LOOP3.EQ.CHANN) THEN
                  FLDMAX = MAX (FLDMAX, VALUE)
                  FLDMIN = MIN (FLDMIN, VALUE)
                  END IF
               END IF
 120        CONTINUE
 200                       CONTINUE
 300                    CONTINUE
 400                 CONTINUE
 500              CONTINUE
 600           CONTINUE
 700        CONTINUE
C                                       Close file
         CALL ARRCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Save values in ARRAY_STAT
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
         CALL ARSPUT (CNAME(IFIELD), 'DATAMAX', OOARE, DIM, DATMAX,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARSPUT (CNAME(IFIELD), 'DATAMIN', OOARE, DIM, DATMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'FIELDMAX', OOARE, DIM, FLDMAX,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'FIELDMIN', OOARE, DIM, FLDMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       force max/min to disk
         CALL IMGOPN (CNAME(IFIELD), 'WRIT', IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'DATAMAX', OOARE, DIM, DATMAX,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMPUT (CNAME(IFIELD), 'DATAMIN', OOARE, DIM, DATMIN,
     *      CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 995
         CALL IMGCLO (CNAME(IFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 800     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 995  MSGTXT = 'CLMXMN: ERROR DETERMING STAT. FOR ' // CNAME(IFIELD)
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE CLGFIT (IRET)
C-----------------------------------------------------------------------
C   CLGFIT fits an eliptical Gaussian to the dirty beam.
C   If peak of beam is too narrow to fit a default circular Gaussian is
C   used. A grid of up to 5 X 11 points is used for the fit; only points
C   within the half power points are used.  To avoid degenerate
C   cases some of the allowed points are ignored.
C   Solution is by least squares to a linearized gaussian.
C   Inputs:
C   Inputs from common:
C      DBNAME  C*32 Name dirty beam object. - first used
C      NXBEM   I    "X" dimension of beam
C      NYBEM   I    "Y" dimension of beam
C      CELLSG  R(2) Grid increment in RA (asec) (negative) and dec.
C      MROTAT  R    Coordinate rotation.
C      USEFIT  L    If true use fitted beam
C   Outputs in common:
C      BMAJ    R    Major axis size (FWHP in sec).
C      BMIN    R    Minor axis size (FWHP in sec).
C      BPA     R    Position angle of major axis (degrees).
C      ROW1    R(*) Scratch array
C   Outputs:
C      IRET    I    Return error code 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, IFIELD, IFLIP, IJK, ILAST, IROW, J, K, L, BLC(7),
     *   TRC(7), DIM(7), ICENX, ICENY, TYPE, MSGSAV, BFIELD
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, BMJLOC, BMNLOC,
     *   BPALOC
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      IRET = 0
      MSGSAV = MSGSUP
      IF (USEFIT) THEN
         XFACT = ABS (CELLSG(1))
C                                       Loop over beams
         DO 100 IFIELD = 1,MFIELD
C                                       is it already recorded
            BFIELD = IFIELD
            IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
            MSGSUP = 32000
            CALL IMGET (CNAME(BFIELD), 'BEAM.BMAJ', TYPE, DIM, BMJLOC,
     *         CDUMMY, IRET)
            MSGSUP = MSGSAV
            IF (IRET.EQ.1) THEN
               IRET = 0
               BMJLOC = 0.0
               END IF
            IF (IRET.NE.0) GO TO 995
            MSGSUP = 32000
            CALL IMGET (CNAME(BFIELD), 'BEAM.BMIN', TYPE, DIM, BMNLOC,
     *         CDUMMY, IRET)
            MSGSUP = MSGSAV
            IF (IRET.EQ.1) THEN
               IRET = 0
               BMNLOC = 0.0
               END IF
            IF (IRET.NE.0) GO TO 995
            MSGSUP = 32000
            CALL IMGET (CNAME(BFIELD), 'BEAM.BPA', TYPE, DIM, BPALOC,
     *         CDUMMY, IRET)
            MSGSUP = MSGSAV
            IF (IRET.EQ.1) THEN
               IRET = 0
               BPALOC = -10000.0
               END IF
            IF (IRET.NE.0) GO TO 995
C                                       onebem and multi-res
            IF (((BMJLOC.LE.0.0) .OR. (BMNLOC.LE.0.0) .OR.
     *         (BPALOC.LE.-360.)) .AND. (BFIELD.LT.IFIELD)) THEN
               BMJLOC = BMAJ(BFIELD) / 3600.0
               BMNLOC = BMIN(BFIELD) / 3600.0
               BPALOC = BPA(BFIELD)
               END IF
C                                       fit if needed
            IF ((BMJLOC.LE.0.0) .OR. (BMNLOC.LE.0.0) .OR.
     *         (BPALOC.LE.-360.)) THEN
               IFLIP = 1
               ICENX = NXBEM(BFIELD) / 2
               ICENY = NYBEM(BFIELD) / 2 + 1
               BLC(1) = ICENX - 5
               BLC(2) = ICENY
               BLC(3) = CHANN
               TRC(1) = ICENX + 5
               TRC(2) = ICENY + 5
               TRC(3) = CHANN
C                                       Set window
               DIM(1) = 7
               DIM(2) = 1
               DIM(3) = 0
               CALL ARDPUT (DBNAME(BFIELD), 'BLC', OOAINT, DIM, BLC,
     *            CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 995
               CALL ARDPUT (DBNAME(BFIELD), 'TRC', OOAINT, DIM, TRC,
     *            CDUMMY, IRET)
               IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
               CALL ARROPN (DBNAME(BFIELD), 'READ', IRET)
               IF (IRET.NE.0) GO TO 995
C                                       Zero work arrays.
               DO 20 I = 1,3
                  Y(I) = 0.0
                  DO 10 J = 1,3
                     X(I,J) = 0.0
 10                  CONTINUE
 20               CONTINUE
C                                       Loop through rows.
               DO 70 I = 1,6
C                                       Read row.
                  CALL ARREAD (DBNAME(BFIELD), DIM, ROW1, IRET)
                  IF (IRET.NE.0) GO TO 995
C                                       Loop down row doing alternate
C                                       halves. Go only to first
C                                       decending 0.35 from center.
                  DO 65 IJK = 1,2
                     IFLIP = - IFLIP
                     ILAST = 6 - IFLIP
                     DO 60 J = IJK,6
                        IROW = 6 + (J-1) * IFLIP
                        IF ((ROW1(IROW).LT.0.35) .AND.
     *                     (ROW1(IROW).LT.ROW1(ILAST))) GO TO 65
                        IF (ROW1(IROW).GE.0.35) THEN
                           ILAST = IROW
C                                       Compute displacements from
C                                       center.
                           DX = IFLIP * (J-1.0) * CELLSG(1) / XFACT
                           DY = (1.0-I) * CELLSG(2) / XFACT
C                                       Compute partials WRT C1,C2,C3
                           P(1) = DX * DX
                           P(2) = DY * DY
                           P(3) = DX * DY
C                                       Sum partials into X matrix and
C                                       Y vector.
                           DO 50 K = 1,3
                              Y(K) = Y(K) - LOG (ROW1(IROW)) * P(K)
                              DO 40 L = 1,3
                                 X(K,L) = X(K,L) + P(K) * P(L)
 40                              CONTINUE
 50                           CONTINUE
                           END IF
 60                     CONTINUE
 65                  CONTINUE
 70               CONTINUE
C                                       Close beam
               CALL ARRCLO (DBNAME(BFIELD), IRET)
               IF (IRET.NE.0) GO TO 995
C                                       Fit beam
               CALL FITBM (IFIELD, X, Y, CELLSG, MROTAT, BMJLOC,
     *            BMNLOC, BPALOC)
C                                       Convert to arc sec
            ELSE
               BMJLOC = BMJLOC * 3600.
               BMNLOC = BMNLOC * 3600.
               END IF
C                                       Record values
            BMAJ(IFIELD) = BMJLOC
            BMIN(IFIELD) = BMNLOC
            BPA(IFIELD)  = BPALOC
 100        CONTINUE
         END IF
      GO TO 999
C
 995  MSGTXT = 'CLGFIT: ERROR FITTING BEAM ' // DBNAME(IFIELD)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLACLN (IFIELD, IRET)
C-----------------------------------------------------------------------
C   CLACLN does a limited clean of the Barry Clark variety using the
C   brightest residuals and a limited beam stored in the "AP".
C   Components are written to the appropriate CC tables.
C   Will handle up to MAXFLD subfields.
C   Inputs:
C      IFIELD   I      Field number being done: 0 -> all
C   Inputs from common:
C      APCFLD   I(*)   start addresses of residuals for each field
C      APCLCN   I(*)   element count for each field.
C      GAIN     R      Clean loop gain.
C      FACTOR   R      Clean speed up factor.
C      RESNUM   I      Number of map points in the AP.
C      APRESD   I      first location in the AP of the residuals.
C      NCLNG    I(*)   First clean component to use; 0 for first clean
C      CLNLIM   I      Maximum number of clean components desired.
C      MINFLX   R      Min. residual flux.
C   Output to common:
C      NCLNG    I(*)   Current clean component number.
C      FINISH   L      .TRUE. If minimum clean component flux density
C                      or maximum iteration encountered, otherwise
C                      .FALSE.
C      RESMAX   R      Maximum abs. residual (Jy).
C      FSTCLN   L      Set to false to indicate this is not the first
C                      clean cycle on this channel.
C      ROW1     R(*)   Scratch array
C      ROW2     R(*)   Scratch array
C      ROW3     R(*)   Scratch array
C   Output:
C      IRET     I      Return error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, IRET
C
      CHARACTER PREFIX*5
      LOGICAL   LERR, IPOL
      INTEGER   IROUND, LFIELD, LF1, LF2, NCBUFF, IPOINT, IX, IY,
     *   NCOMPC, LENCBU, APFDV, APFDL, MTYPE, ICCRNO, TOTCC, NUMCOL,
     *   ITLIM, SCRTCH(256), FTOTCC, KFIELD
      REAL      ATLIM, XFAC, XFLUX, XPROD, FNFLD, CCMIN, CCFLUX,
     *   X, Y, FLUX, PARMS(3), PFAC, LGAIN
      DOUBLE PRECISION CATD(256)
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DHDR.INC'
      INTEGER   CLBUFF(512,MAXAFL), LUNC(MAXAFL), IP(MAXFLD), CAT(256),
     *   CCKOLS(MAXCCC), CCNUMV(MAXCCC), STRTOT
      REAL      FDVEC(6,MAXAFL), STRFLX(MAXAFL)
      EQUIVALENCE (FNFLD, ROW3(1)),     (FDVEC, ROW3(2))
C     EQUIVALENCE (SCRTCH, ROW1(1)),
C     *   (FNFLD, ROW1(257)),     (FDVEC, ROW1(258))
      EQUIVALENCE (CAT, CATD)
      SAVE ATLIM
C                                       LENCBU, NCOMPC control the
C                                       no. CC per AP call.
      DATA LENCBU, NCOMPC /130, 32/
C-----------------------------------------------------------------------
      IF (IFIELD.LE.1) FINISH = .FALSE.
      ITLIM = 0.1 * CLNLIM + 0.91
      IF (LASTIT.LE.0) LASTIT = ITLIM
      ITLIM = MIN (ITLIM, LASTIT)
      ITLIM = CLNLIM - ITLIM/2
C                                       Set FSTCLN
      FSTCLN = .FALSE.
C                                       Open Clean component files.
      IX = 2 * MAXPCH + 3
      TOTCC = 0
      FTOTCC = 0
C                                       Zero Field descriptor vector
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
         FNFLD = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         FNFLD = 1
         CALL ISUM (MFIELD, NCLNG, FTOTCC)
         FTOTCC = FTOTCC - NCLNG(LF1)
         END IF
C                                       Loop over fields.
      KFIELD = 1
      DO 20 LFIELD = LF1,LF2
         LGAIN = GAIN
         IF (LF2.NE.LF1) KFIELD = LFIELD
C                                       Fill field descriptor vector.
         IF (IFIELD.LE.0) THEN
            FDVEC(1,KFIELD) = APCFLD(LFIELD)
            FDVEC(2,KFIELD) = APCLCN(LFIELD)
            FDVEC(3,KFIELD) = 0.0
            FDVEC(4,KFIELD) = 0.0
            FDVEC(5,KFIELD) = 0.0
            FDVEC(6,KFIELD) = LGAIN
         ELSE
            LGAIN = BMAJ(LFIELD) * BMIN(LFIELD)
            IF (LGAIN.LE.0.0) THEN
               LGAIN = GAIN
            ELSE
               LGAIN = GAIN * ((BMINAR / LGAIN) ** MRCTRL(2))
               END IF
            FDVEC(1,1) = APCFLD(LFIELD)
            FDVEC(2,1) = APCLCN(LFIELD)
            FDVEC(3,1) = 0.0
            FDVEC(4,1) = 0.0
            FDVEC(5,1) = 0.0
            FDVEC(6,1) = LGAIN
            END IF
C                                       Track change from start flux
         STRFLX(KFIELD) = ABS (FLUXG(LFIELD))
C                                       Read CAT.
         CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), CAT, 'REST',
     *      SCRTCH, IRET)
         IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1000) IRET, LFIELD
            GO TO 990
            END IF
         NUMCOL = 3
         IF (COMRES(LFIELD).GT.0.0) NUMCOL = 7
         LUNC(KFIELD) = KFIELD + 50
         CALL CCMINI ('WRIT', CLBUFF(1,KFIELD), CCDISK(LFIELD),
     *      CCCNO(LFIELD), CCVER(LFIELD), CAT, LUNC(KFIELD), ICCRNO,
     *      CCKOLS, CCNUMV, NUMCOL, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Get number of existing CC.
         IP(KFIELD) = MIN (CLBUFF(5,KFIELD), NCLNG(LFIELD))
         TOTCC = TOTCC + IP(KFIELD)
         FTOTCC = FTOTCC + IP(KFIELD)
C                                       Stokes' TYPE
C                                       Note: this involves some
C                                       assumptions.
         IPOL = CATD(KDCRV+3) .LE. 1.1D0
 20      CONTINUE
C                                       Check for iteration limit.
      IF (TOTCC.GE.CLNLIM) GO TO 250
      IF ((FTOTCC.GE.CLNLIM) .AND. (AFIELD)) GO TO 250
C                                       Prepare to Clean
      STRTOT = TOTCC
      ROW2(1) = PATCH
      ROW2(2) = PATCH
      CALL QPUT (ROW2, 0, 6, 2)
C                                       Put field descriptor vector
C                                       into AP
      APFDL = 1 + FNFLD * 6
      APFDV = LENCBU + 2
      CALL QWD
      CALL QPUT (FNFLD, APFDV, APFDL, 2)
C                                       addresses are integer
      APFDV = APFDV + 1
      DO 25 LFIELD = LF1,LF2
         IF (LF2.NE.LF1) KFIELD = LFIELD
         CALL QWD
         CALL QPUT (APCFLD(LFIELD), APFDV, 1, 1)
         APFDV = APFDV + 6
 25      CONTINUE
C                                       Start Cleaning.
      CALL QWD
      APFDV = LENCBU + 2
C                                       Init Clean component Min flux
      CCMIN = 1.0E12
C                                       Speed up clean.
      XFAC = MAPLIM / MAX (RESMAX, 1.0E-20)
      IF (XFAC.LT.0.0) XFAC = -XFAC
      PFAC = FACTOR * (1 - MRCTRL(3) * (1.0 -
     *   1.0/((BMAJ(LF1)*BMIN(LF1)/BMINAR)**MRCTRL(4))))
      WRITE (MSGTXT,1025) LF1, PFAC, LGAIN
      IF (NUMRES.GT.1) CALL MSGWRT (2)
      XPROD = XFAC**PFAC
      XFAC = XPROD
      IF (FFIELD) ATLIM = ATLIMI
      NCBUFF = NCOMPC
C                                       Begin loop.
 30      NCBUFF = NCBUFF + 1
C                                       See if time to get more from
C                                       the AP.
         IF (NCBUFF.GT.NCOMPC) THEN
C                                       Clean.
            CALL QMULCL (2, APFDV, APBEAM, 0, NCOMPC)
            CALL QWR
C                                       Get load of components
            CALL QGET (ROW2, 0, LENCBU, 2)
            CALL QWD
            NCBUFF = 1
            END IF
C                                       Process a Component.
         IPOINT = 3 + (NCBUFF-1) * 4
C                                       Get field and position.
         IX = IROUND (ROW2(IPOINT+1))
         IY = IROUND (ROW2(IPOINT+2))
         IF (IFIELD.LE.0) THEN
            LFIELD = IROUND (ROW2(IPOINT+3))
         ELSE
            LFIELD = IFIELD
            END IF
         IF (LF2.NE.LF1) KFIELD = LFIELD
C                                       Check field number.
         IF ((LFIELD.LT.LF1) .OR. (LFIELD.GT.LF2)) THEN
            IRET = 5
            WRITE (MSGTXT,1040) LFIELD, LF1, LF2
            GO TO 990
            END IF
         X = ((IX - ICNTRX(LFIELD)) * CELLSG(1)) / 3600. - XPOFF(LFIELD)
         Y = ((IY - ICNTRY(LFIELD)) * CELLSG(2)) / 3600. - YPOFF(LFIELD)
         FLUX = ROW2(IPOINT) * LGAIN
         FLUXG(LFIELD) = FLUXG(LFIELD) + FLUX
         TFLUXG = TFLUXG + FLUX
C                                       Stored RA and Dec refer to
C                                       the cataloged Clean map.
         MTYPE = 0
         TOTCC = TOTCC + 1
         FTOTCC = FTOTCC + 1
         IP(KFIELD) = IP(KFIELD) + 1
         ICCRNO = IP(KFIELD)
         IF (COMRES(LFIELD).GT.0.0) THEN
            MTYPE = 1
            PARMS(1) = COMRES(LFIELD)
            PARMS(2) = COMRES(LFIELD)
            PARMS(3) = 0.0
         ELSE
            MTYPE = 0
            END IF
         CALL TABCCM ('WRIT', CLBUFF(1,KFIELD), ICCRNO, CCKOLS, CCNUMV,
     *      NUMCOL, X, Y, FLUX, MTYPE, PARMS, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Test CC flux/number limits
         XFLUX = ROW2(IPOINT)
         CCFLUX = XFLUX
C                                       Record Min CC Magnitude
         CCMIN = MIN (CCMIN, ABS(XFLUX))
         FLDCCM(LFIELD) = MIN (FLDCCM(LFIELD), ABS(XFLUX))
C                                       Check for iteration limit.
         IF (TOTCC.GE.CLNLIM) GO TO 110
         IF ((FTOTCC.GE.CLNLIM) .AND. (AFIELD)) GO TO 110
C                                       Check for minimum Clean flux
         IF (ABS (XFLUX).LT.MINFLX) GO TO 90
         IF ((IFIELD.GT.0) .AND. (ABS(XFLUX).LT.MNFFLX(IFIELD)))
     *      GO TO 90
C                                       IF flux is negative, stop at neg
         IF ((MINFLX.LT.0.0) .AND. (XFLUX.LT.0.0)) GO TO 90
C                                       Stop if clean is diverging
         IF (ABS(XFLUX).GT.2.*CCMIN) GO TO 90
C                                       Check minimum algorithm flux
C                                       Don't stop in last 5%
         IF ((ABS (XFLUX).LT.MAPLIM*(1.0+ATLIM)) .AND.
     *      (TOTCC.LT.ITLIM)) GO TO 100
C                                       Check max dynamic range
         IF (ABS (XFLUX).LT.CCVLIM) GO TO 105
C                                       If flux is dropping, jump out
C                                       To do a proper sub
         IF (IPOL .AND. (ABS(FLUXG(LFIELD)).LT.0.8*STRFLX(KFIELD))
     *      .AND. (CCMIN.GT.1.3*PCCMIN)) THEN
C                                       Attempt fix major and minor
C                                       Cycle differences
            MSGTXT = 'SOMETHING IS GOING WRONG - ABANDON CLEAN'
            CALL MSGWRT(5)
            FINISH = .TRUE.
            GO TO 100
            END IF
         STRFLX(KFIELD) = MAX (STRFLX(KFIELD), ABS(FLUXG(LFIELD)))
C                                       Loop back for next clean.
         IF (FFIELD) ATLIM = ATLIM + XFAC / FTOTCC
         GO TO 30
C                                       Finished minor Clean loop.
C                                       (Note XFLUX modifed by METSCA)
 90   CALL METSCA (XFLUX, PREFIX, LERR)
      IF (IFIELD.LE.0) THEN
         WRITE (MSGTXT,1090) XFLUX, PREFIX, TOTCC
      ELSE
         WRITE (MSGTXT,1091) IFIELD, XFLUX, PREFIX, TOTCC
         END IF
      GO TO 120
 100  CALL METSCA (XFLUX, PREFIX, LERR)
      IF (IFIELD.LE.0) THEN
         WRITE (MSGTXT,1100) XFLUX, PREFIX, TOTCC
      ELSE
         WRITE (MSGTXT,1101) IFIELD, XFLUX, PREFIX, TOTCC
         END IF
      GO TO 120
 105  CALL METSCA (XFLUX, PREFIX, LERR)
      IF (IFIELD.LE.0) THEN
         WRITE (MSGTXT,1105) XFLUX, PREFIX, TOTCC
      ELSE
         WRITE (MSGTXT,1106) IFIELD, XFLUX, PREFIX, TOTCC
         END IF
      GO TO 120
 110  CALL METSCA (XFLUX, PREFIX, LERR)
      IF (IFIELD.LE.0) THEN
         WRITE (MSGTXT,1110) XFLUX, PREFIX, TOTCC
      ELSE
         WRITE (MSGTXT,1111) IFIELD, XFLUX, PREFIX, TOTCC
         END IF
C                                       Save maximum residual
 120  CALL MSGWRT (4)
      ACTRES = ROW2(IPOINT)
      RESMAX = ABS (ACTRES)
      LASTIT = TOTCC - STRTOT
C                                       See if finished.
      IF ((FTOTCC.GE.CLNLIM) .AND. (AFIELD)) FINISH = .TRUE.
      IF (RESMAX.LE.MINFLX) FINISH = .TRUE.
      IF ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) FINISH = .TRUE.
C                                       Stop if clean is diverging
      IF (ABS(CCFLUX).GT.2.*CCMIN) THEN
         FINISH = .TRUE.
         MSGTXT = 'Residual Peak Noise is Increasing'
         CALL MSGWRT(5)
         MSGTXT = 'Clean has begun to diverge, Stopping'
         CALL MSGWRT(5)
C                                       Tell user min peak residual
         XFLUX = CCMIN
         CALL METSCA (XFLUX, PREFIX, LERR)
         WRITE(MSGTXT,1115,ERR=150) XFLUX, PREFIX
         CALL MSGWRT(5)
         END IF
C                                       If stop at negative component
      IF ((MINFLX.LT.0.0) .AND. (CCFLUX.LT.0.0)) THEN
         FINISH = .TRUE.
         MSGTXT = 'Negative Clean Component Found, Stopping'
         CALL MSGWRT(4)
         END IF
C                                       Jump here on print error
 150  CONTINUE
C                                       Output last buffers and close.
      DO 200 LFIELD = LF1,LF2
         IF (LF2.NE.LF1) KFIELD = LFIELD
C                                       Write field info.
         XFLUX = FLUXG(LFIELD)
         CALL METSCA (XFLUX, PREFIX, LERR)
C                                       Set NCLNG to the current clean
C                                       counter
         NCLNG(LFIELD) = IP(KFIELD)
         IF (MFIELD.GT.1) THEN
            WRITE (MSGTXT,1125) LFIELD, XFLUX, PREFIX, NCLNG(LFIELD)
            IF (FINISH) THEN
               CALL MSGWRT (5)
            ELSE
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Set number of CC
         CLBUFF(5,KFIELD) = NCLNG(LFIELD)
C                                       Close
         CALL TABCCM ('CLOS', CLBUFF(1,KFIELD), ICCRNO, CCKOLS, CCNUMV,
     *      NUMCOL, X, Y, FLUX, MTYPE, PARMS, IRET)
         IF (IRET.NE.0) GO TO 995
 200     CONTINUE
C                                       Record Pass CC Minimum
      NCCMIN = MIN (CCMIN, NCCMIN)
C                                       Write total flux density
      XFLUX = TFLUXG
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1120) XFLUX, PREFIX, FTOTCC
      IF (FINISH) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Output last buffers and close.
 250  DO 260 LFIELD = LF1,LF2
         IF (LF2.NE.LF1) KFIELD = LFIELD
C                                       Set NCLNG to the current clean
C                                       counter
         NCLNG(LFIELD) = IP(KFIELD)
C                                       Set number of CC
         CLBUFF(5,KFIELD) = NCLNG(LFIELD)
C                                       Close
         CALL TABCCM ('CLOS', CLBUFF(1,KFIELD), ICCRNO, CCKOLS, CCNUMV,
     *      NUMCOL, X, Y, FLUX, MTYPE, PARMS, IRET)
         IF (IRET.NE.0) GO TO 995
 260     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLACLN: FINDING COMPONENTS'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLACLN: CATIO ERROR',I3,' FIELD ',I5)
 1025 FORMAT ('Field',I5,'  FACTOR',F8.4,'  GAIN',F8.5)
 1040 FORMAT ('CLACLN: ILLEGAL FIELD NUMBER',I3,', NOT',I3,'-',I3)
 1090 FORMAT ('Reached min Clean flux density =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1091 FORMAT ('Field',I5,' min Clean flux    =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1100 FORMAT ('Reached minimum algorithm flux =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1101 FORMAT ('Field',I5,' min algorithm flux=',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1105 FORMAT ('Reached minimum allowed flux   =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1106 FORMAT ('Field',I5,' min allowed flux  =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1110 FORMAT ('Reached Iter. limit, Max resid =',F9.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1111 FORMAT ('Field',I5,' Iter limit, Max resid=',F8.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1115 FORMAT ('Minimum residual peak was      =',F9.3,1X,A5,'Jy')
 1120 FORMAT ('Total Cleaned flux density     =',F9.3,1X,A5,'Jy',
     *   I9,' comps')
 1125 FORMAT ('Field',I5,' Clean flux density=',F9.3,1X,A5,'Jy',
     *   I9,' comps')
      END
      SUBROUTINE CLASDI (IFIELD, IRET)
C-----------------------------------------------------------------------
C   CLASDI does a limited clean of the Steer-Dewdney-Ito variety using
C   the brightest residuals.
C   Components are written to the appropriate CC tables.
C   Will handle up to MAXFLD subfields.
C   Inputs:
C      IFIELD   I      Field number being done: 0 -> all
C   Inputs from common:
C      RESNUM   I      Number of map points in the AP.
C      NCLNG    I(*)   First clean component to use; 0 for first clean
C      CLNLIM   I      Maximum number of clean components desired.
C      MINFLX   R      Min. residual flux.
C   Output to common:
C      NCLNG    I(*)   Current clean component number.
C      FINISH   L      .TRUE. If minimum clean component flux density
C                      or maximum iteration encountered, otherwise
C                      .FALSE.
C      RESMAX   R      Maximum abs. residual (Jy).
C      FSTCLN   L      Set to false to indicate this is not the first
C                      clean cycle on this channel.
C      ROW1     R(*)   Scratch array
C      ROW2     R(*)   Scratch array
C      ROW3     R(*)   Scratch array
C   Output:
C      IRET     I      Return error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, IRET
C
      CHARACTER PREFIX*5, CDUMMY*1
      LOGICAL   LERR, ROUND, DOUNBX, UNBROW, UNBCOL
      LONGINT   LPTR, JPTR
      INTEGER   LFIELD, LF1, LF2, IX, IY, MTYPE, ICCRNO, TOTCC, NUMCOL,
     *   SCRTCH(256), FTOTCC, CLBUFF(512), LUNC, CAT(256), MINX, MINY,
     *   MAXX, MAXY, NPIX, IBOX, MNX, MNY, MXX, MXY, BLC(7), TRC(7),
     *   DIM(7), IXOFF, IYOFF, NX, NY, MXDIS2, IDIS2, IIY, LOY, HIY,
     *   LOX, HIX, I, J, MPIX, IIX, BPATCH, TPATCH, NBY1, NBY2, NBX1,
     *   NBX2, MAPTR, MAPEND, IPTR, II, II1, II2, JJ, IOFF, IP, BFIELD
      REAL      XFLUX, X, Y, FLUX, PARMS(3), MXXRES, VALRES, SUM, XX
      DOUBLE PRECISION CATD(256)
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DAPC.INC'
      INTEGER   CCP, CCKOLS(MAXCCC), CCNUMV(MAXCCC)
      EQUIVALENCE (CAT, CATD)
      DATA BPATCH /25/
C-----------------------------------------------------------------------
      TPATCH = 2 * BPATCH + 1
C                                       Limit Clean "AP" memory
      MXXRES = 0.
      IF (IFIELD.LE.1) FINISH = .FALSE.
C                                       Set FSTCLN
      FSTCLN = .FALSE.
      TOTCC = 0
      FTOTCC = 0
C                                       loop limits
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         CALL ISUM (MFIELD, NCLNG, FTOTCC)
         FTOTCC = FTOTCC - NCLNG(LF1)
         END IF
C                                       Loop thru fields.
      DO 90 LFIELD = LF1,LF2
         BFIELD = LFIELD
         IF (ONEBEM) BFIELD = ((BFIELD-1)/NFPRES) * NFPRES + 1
C                                       Read beam
         BLC(1) = NXBEM(BFIELD) / 2 - BPATCH
         BLC(2) = NYBEM(BFIELD) / 2 +1 - BPATCH
         BLC(3) = CHANN
         TRC(1) = NXBEM(BFIELD) / 2 + BPATCH
         TRC(2) = NYBEM(BFIELD) / 2 + 1 + BPATCH
         TRC(3) = CHANN
C                                       Set window
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 0
         CALL ARDPUT (DBNAME(BFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (DBNAME(BFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
         CALL ARROPN (DBNAME(BFIELD), 'READ', IRET)
         IF (IRET.NE.0) GO TO 995
         IPTR = 11
         NBX1 = TPATCH
         NBY1 = TPATCH
         NBX2 = 1
         NBY2 = 1
         DO 20 IY = 1,TPATCH
C                                      Read row of the beam
            CALL ARREAD (DBNAME(BFIELD), DIM, ROW1, IRET)
            IF (IRET.NE.0) GO TO 995
            DO 10 IX = 1,TPATCH
               IF (ROW1(IX).LT.0.10) THEN
                  ROW1(IX) = 0.0
               ELSE
                  NBX1 = MIN (NBX1, IX)
                  NBX2 = MAX (NBX2, IX)
                  NBY1 = MIN (NBY1, IY)
                  NBY2 = MAX (NBY2, IY)
                  END IF
 10            CONTINUE
            CALL QWR
            CALL QPUT (ROW1, IPTR, TPATCH, 2)
            CALL QWD
            IPTR = IPTR + TPATCH
 20         CONTINUE
         MAPTR = IPTR + 10
C                                       Close
         CALL ARRCLO (DBNAME(BFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Read CAT.
         CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), CAT, 'REST',
     *      SCRTCH, IRET)
         IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1000) IRET, LFIELD
            GO TO 990
            END IF
C                                       Open Clean component files.
         IF (COMRES(LFIELD).GT.0.0) THEN
            NUMCOL = 7
            MTYPE = 1
            PARMS(1) = COMRES(LFIELD)
            PARMS(2) = COMRES(LFIELD)
            PARMS(3) = 0.0
         ELSE
            NUMCOL = 3
            MTYPE = 0
            END IF
         LUNC = 51
         CALL CCMINI ('WRIT', CLBUFF, CCDISK(LFIELD), CCCNO(LFIELD),
     *      CCVER(LFIELD), CAT, LUNC, ICCRNO, CCKOLS, CCNUMV, NUMCOL,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Get number of existing CC.
         CCP = MIN (CLBUFF(5), NCLNG(LFIELD))
         TOTCC = TOTCC + CCP
         FTOTCC = FTOTCC + CCP
C                                       Find max,min Y in boxes
         MINX = 30000
         MAXY = -30000
         MINY = 30000
         MAXX = -30000
         NPIX = 0
         DO 30 IBOX = 1,NBOXES(LFIELD)
            IP = (IBOX-1) * MFIELD + LFIELD
            ROUND = WIN(1,IP).EQ.-1
            IF (ROUND) THEN
               MNX = WIN(3,IP) - WIN(2,IP)
               MNY = WIN(4,IP) - WIN(2,IP)
               MXX = WIN(3,IP) + WIN(2,IP)
               MXY = WIN(4,IP) + WIN(2,IP)
               MPIX = ((MXX-MNX+1) * (MXY-MNY+1) * 3.14/4.0) + 0.5
            ELSE
               MNX = WIN(1,IP)
               MNY = WIN(2,IP)
               MXX = WIN(3,IP)
               MXY = WIN(4,IP)
               MPIX = (MXX-MNX+1) * (MXY-MNY+1)
               END IF
            IF (MPIX.EQ.1) NPIX = 1
            MAXX = MAX (MAXX, MNX, MXX)
            MAXY = MAX (MAXY, MNY, MXY)
            MINX = MIN (MINX, MNX, MXX)
            MINY = MIN (MINY, MNY, MXY)
 30         CONTINUE
C                                       Set window
C                                       Be careful
         BLC(1) = MAX (1, MIN (MINX, MAXX))
         BLC(2) = MAX (1, MIN (MINY, MAXY))
         TRC(1) = MIN (IMSIZE(1,LFIELD), MAX (MINX, MAXX))
         TRC(2) = MIN (IMSIZE(2,LFIELD), MAX (MINY, MAXY))
         BLC(3) = CHANN
         TRC(3) = CHANN
         IXOFF = BLC(1) - 1
         IYOFF = BLC(2) - 1
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 0
         CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
         CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Open pixel array
         CALL ARROPN (CNAME(LFIELD), 'READ', IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Set some constants.
         NX = TRC(1) - BLC(1) + 1
         NY = TRC(2) - BLC(2) + 1
C                                       Loop thru map looking for points
C                                       greater than MAPLIM.
         CALL QPUT (0.0, 0, 1, 2)
         IY = TPATCH * NX
         CALL QVFILL (0, MAPTR, 1, IY)
         MAPEND = MAPTR + NX * TPATCH - 1
         DO 70 J = 1,NY+BPATCH
            IY = (TPATCH-1) * NX
            CALL QVMOV (MAPEND-NX, -1, MAPEND, -1, IY)
            CALL RFILL (NX, 0.0, ROW2)
            IY = J + IYOFF
C                                      Read row of the map.
            IF (J.LE.NY) THEN
               CALL ARREAD (CNAME(LFIELD), DIM, ROW1, IRET)
               IF (IRET.NE.0) GO TO 995
C                                       Loop over boxes
               DO 50 IBOX = 1,NBOXES(LFIELD)
                  IP = (IBOX-1) * MFIELD + LFIELD
                  ROUND = WIN(1,IP) .EQ. -1
                  IF (ROUND) THEN
                     MXDIS2 = WIN(2,IP)**2
                     IIY = IY - WIN(4,IP)
                     MNX = WIN(3,IP) - WIN(2,IP)
                     MNY = WIN(4,IP) - WIN(2,IP)
                     MXX = WIN(3,IP) + WIN(2,IP)
                     MXY = WIN(4,IP) + WIN(2,IP)
                  ELSE
                     MNX = WIN(1,IP)
                     MNY = WIN(2,IP)
                     MXX = WIN(3,IP)
                     MXY = WIN(4,IP)
                     END IF
C                                       Set range of pixel numbers
                  LOY = MIN (MNY, MXY)
                  HIY = MAX (MNY, MXY)
                  IF ((IY.GE.LOY) .AND. (IY.LE.HIY)) THEN
                     IF (ISUNBX) THEN
                        DOUNBX = UNBROW (LFIELD, IY)
                     ELSE
                        DOUNBX = .FALSE.
                        END IF
                     LOX = MIN (MNX, MXX)
                     HIX = MAX (MNX, MXX)
C                                       Loop down the row.
                     DO 40 IX = LOX,HIX
                        IF (DOUNBX) THEN
                           IF (UNBCOL (LFIELD, IX, IY)) GO TO 40
                           END IF
C                                       Test for round box
                        IF (ROUND) THEN
                           IIX = IX - WIN(3,IP)
                           IDIS2 = IIX*IIX + IIY*IIY
                           IF (IDIS2.GT.MXDIS2) GO TO 40
                           END IF
                        I = IX - IXOFF
C                                       Check flux
                        IF (ROW1(I).GE.MAPLIM) THEN
                           ROW2(I) = ROW1(I) - MAPLIM
                        ELSE IF (ROW1(I).LE.-MAPLIM) THEN
                           ROW2(I) = ROW1(I) + MAPLIM
                           END IF
 40                     CONTINUE
                     END IF
 50               CONTINUE
               END IF
            CALL QPUT (ROW2, MAPTR, NX, 2)
C                                       look for components
            IF (J.GT.BPATCH) THEN
               DO 65 I = 1,NX
                  LPTR = NX * BPATCH + I + MAPTR + PSAPOF - 1
                  IF (ABS(APCORE(LPTR)).GT.1.E-9) THEN
                     FLUX = APCORE(LPTR)
                     SUM = 0.0
                     IOFF = I - BPATCH - 1
                     II1 = MAX (NBX1, 1-IOFF)
                     II2 = MIN (NBX2, NX-IOFF)
                     DO 60 JJ = NBY1,NBY2
                        LPTR = (JJ - 1) * NX + MAPTR + II1 - 2 + IOFF +
     *                     PSAPOF
                        JPTR = (JJ - 1) * TPATCH + 11 + II1 - 2 + PSAPOF
                        DO 55 II = II1,II2
                           LPTR = LPTR + 1
                           JPTR = JPTR + 1
                           SUM = SUM + APCORE(LPTR) * APCORE(JPTR)
 55                        CONTINUE
 60                     CONTINUE
                     XX = 1.0
                     IF (SUM.NE.0.0) XX = ABS (FLUX / SUM)
                     XX = MAX (0.001, MIN (0.5, XX))
                     FLUX = XX * FLUX
                     X = ((I + IXOFF - ICNTRX(LFIELD)) * CELLSG(1)) /
     *                  3600. - XPOFF(LFIELD)
                     Y = ((J - 1 - BPATCH + IYOFF - ICNTRY(LFIELD)) *
     *                  CELLSG(2)) / 3600. - YPOFF(LFIELD)
                     FLUXG(LFIELD) = FLUXG(LFIELD) + FLUX
                     TFLUXG = TFLUXG + FLUX
C                                       Stored RA and Dec refer to
C                                       the cataloged Clean map.
                     TOTCC = TOTCC + 1
                     FTOTCC = FTOTCC + 1
                     CCP = CCP + 1
                     ICCRNO = CCP
                     CALL TABCCM ('WRIT', CLBUFF, ICCRNO, CCKOLS,
     *                  CCNUMV, NUMCOL, X, Y, FLUX, MTYPE, PARMS, IRET)
                     IF (IRET.NE.0) GO TO 995
                     LPTR = NX * BPATCH + I + MAPTR + PSAPOF - 1
                     FLUX = APCORE(LPTR) - 0.5 * FLUX / XX
                     IF (ABS(FLUX).GT.MXXRES) THEN
                        MXXRES = ABS(FLUX)
                        VALRES = FLUX
                        END IF
                     END IF
 65               CONTINUE
               END IF
 70         CONTINUE
C                                       Write field info.
         XFLUX = FLUXG(LFIELD)
         CALL METSCA (XFLUX, PREFIX, LERR)
C                                       Set NCLNG to the current clean
C                                       counter
         NCLNG(LFIELD) = CCP
         IF (MFIELD.GT.1) THEN
            WRITE (MSGTXT,1050) LFIELD, XFLUX, PREFIX, NCLNG(LFIELD)
            IF (FINISH) THEN
               CALL MSGWRT (5)
            ELSE
               CALL MSGWRT (3)
               END IF
            END IF
C                                       Set number of CC
         CLBUFF(5) = NCLNG(LFIELD)
C                                       Close
         CALL TABCCM ('CLOS', CLBUFF(1), ICCRNO, CCKOLS, CCNUMV,
     *      NUMCOL, X, Y, FLUX, MTYPE, PARMS, IRET)
         IF (IRET.NE.0) GO TO 995
C                                       Close residual
         CALL ARRCLO (CNAME(LFIELD), IRET)
         IF (IRET.NE.0) GO TO 995
 90      CONTINUE
C                                       Save maximum residual
      ACTRES = VALRES
      RESMAX = MXXRES + MAPLIM
C                                       See if finished.
      IF ((FTOTCC.GE.CLNLIM) .AND. (AFIELD)) FINISH = .TRUE.
      IF (RESMAX.LE.MINFLX) FINISH = .TRUE.
      IF ((IFIELD.GT.0) .AND. (RESMAX.LE.MNFFLX(IFIELD))) FINISH=.TRUE.
      IF ((ACTRES.LT.0.0) .AND. (MINFLX.LT.0.0)) FINISH = .TRUE.
C                                       Write max residual
      XFLUX = RESMAX
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1120) XFLUX, PREFIX
      CALL MSGWRT (3)
C                                       Write total flux density
      XFLUX = TFLUXG
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1121) XFLUX, PREFIX, FTOTCC
      IF (FINISH) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLASDI: FINDING COMPONENTS'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLASDI: CATIO ERROR',I3,' FIELD ',I5)
 1050 FORMAT ('Field',I5,' Cleaned flux density=',F9.3,1X,A5,'Jy',
     *   I9,' comps')
 1120 FORMAT ('Peak ESTIMATED SDI residual',F9.3,1X,A5,'Jy')
 1121 FORMAT ('Total Cleaned flux density    =',F9.3,1X,A5,'Jy',
     *   I9,' comps')
      END
      SUBROUTINE CLGAUS (IFIELD)
C-----------------------------------------------------------------------
C   CLGAUS creates the arrays in the "AP" necessary for restoring with
C   an eliptical gaussian.
C   Inputs:
C      IFIELD    I    Field number for the map.
C   Inputs from common:
C      IMSIZE    I(2,*) Image size per field
C      BMAJ      R      Major Axis (FWHP in cells)
C      BMIN      R      Minor axis size (FWHP in cells).
C      BPA       R      Position angle of restoring beam (deg ).
C      CELLSG    R(2)   "x" and "y" cell spacings in asec.
C      MROTAT    R      Coordinate rotation (deg).
C   Outputs in common:
C      GAUSAA    R      Coefficient of u**2
C      GAUSBB    R      Coefficient of u*v
C      GAUSCC    R      Coefficient of v**2
C      ROW1      R(*)   Scratch array
C      Normalization constant for the restoring Gaussian left in
C                      AP location 0.
C-----------------------------------------------------------------------
      INTEGER   IFIELD
C
      INCLUDE 'INCS:PSTD.INC'
      REAL      WT, WT1, XNX2, XNY2, XNXNY, TEMP, AK, AM, AN, TA, TB,
     *   NORFAC
      INTEGER   I, JLIM, K, NX, NY, IAPRES, IAPGAU, IAPEXP, IAPEX1,
     *   IAPGRD, IAPCMP, IAPCMS, IAPRS1, ONENY, TWONY, WRK1, WRK2
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      NX = IMSIZE(1,IFIELD)
      NY = IMSIZE(2,IFIELD)
C                                       Set AP assignments.
C                                       Note: corresponding definitions
C                                       in CLRGRD.
      ONENY = NY
      TWONY = 2 * ONENY
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
C                                       IAPRES = row restoring function
      IAPRES = 100
C                                       IAPEXP = Gaussian work array
      IAPEXP = IAPRES + WRK1
C                                       IAPCMP = FT of components
      IAPCMP = IAPEXP + WRK2
C                                       IAPGAU = Array of Gaussian
C                                       factors in X
      IAPGAU = IAPCMP + WRK2
C                                       IAPEX1 = Array of Gaussian
C                                       factors in Y
      IAPEX1 = IAPGAU + WRK1
C                                       IAPGRD = Resultant uv grid
      IAPGRD = IAPEX1 + WRK1
C                                       IAPCMS = CC structure array
      IAPCMS = IAPGRD + (NX / 2 + 1) * (2 * NY) + 1
C                                       Do a little arithmetic to put
C                                       the beam parameters into useable
C                                       form. Convert to sigma, then to
C                                       coeficients of U**2, U*V, and
C                                       V**2 the last of which is return
C                                       for later use.
      TA = BMAJ(IFIELD) * PI / 1.1774
      TB = BMIN(IFIELD) * PI / 1.1774
      AM = COS ((BPA(IFIELD)+MROTAT)*DG2RAD)
      AN = SIN ((BPA(IFIELD)+MROTAT)*DG2RAD)
      XNX2 = NX * CELLSG(1)
      XNY2 = NY * CELLSG(2)
      XNXNY = ABS (XNX2 * XNY2)
      XNX2 = XNX2 ** 2
      XNY2 = XNY2 ** 2
      GAUSAA = (TA*TA*AM*AM + TB*TB*AN*AN) / (XNY2)
      GAUSCC = (TA*TA*AN*AN + TB*TB*AM*AM) / (XNX2)
      GAUSBB = ((TB*TB-TA*TA) * AN*AM) / (XNXNY )
C                                       Create array IAPEX1.
      DO 10 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         ROW1(I) = (-GAUSBB * K)
 10      CONTINUE
C                                       Load IAPEX1 into AP.
      CALL QPUT (ROW1, IAPEX1, ONENY, 2)
      CALL QWD
C                                       Create array IAPGAU.
      DO 20 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         AK = K
         ROW1(I) = -0.5 * GAUSAA * AK * AK
 20      CONTINUE
C                                       Load IAPGAU into AP.
      CALL QPUT (ROW1, IAPGAU, ONENY, 2)
      CALL QWD
C                                       Compute normalization factor.
C                                       Initialize array IAPEXP.
      CALL QPUT (0.0, 0, 1, 2)
      CALL QWD
      CALL QVCLR (IAPEXP, 1, ONENY)
      CALL QVSADD (IAPEXP, 1, 0, IAPEXP, 1, ONENY)
C                                       Initialize IAPRES for summing
C                                       wt.
      IAPRS1 = IAPRES - 1
      WRK1 = ONENY + 1
      CALL QVCLR (IAPRS1, 1, WRK1)
      CALL QWR
C                                       Calculate sum of the weights.
      JLIM = NX / 2 + 1
      DO 40 I = 1,JLIM
         TEMP = -0.5 * GAUSCC * (I - 1.0) ** 2
         CALL QPUT (TEMP, 1, 1, 2)
         CALL QWD
         CALL QVSADD (IAPGAU, 1, 1, IAPRES, 1, ONENY)
         CALL QVADD (IAPRES, 1, IAPEXP, 1, IAPRES, 1, ONENY)
         CALL QVEXP (IAPRES, 1, IAPRES, 1, ONENY)
         CALL QSVE (IAPRS1, 1, IAPRS1, WRK1)
C                                       Prepare IAPEXP for next pass.
         CALL QVADD (IAPEXP, 1, IAPEX1, 1, IAPEXP, 1, ONENY)
         CALL QWR
C                                       Get intermediate sum.
         IF (I.EQ.1) CALL QGET (WT1, IAPRS1, 1, 2)
 40      CONTINUE
C                                       Get sum of the weights.
      CALL QGET (WT, IAPRS1, 1, 2)
      CALL QWD
C                                       Compute weight over whole plane.
      WT = 2.0 * WT - WT1
      NORFAC = LOG (1.0 / WT)
C                                       Store WT in AP loc 0.
      CALL QPUT (NORFAC, 0, 1, 2)
      CALL QWD
C
      RETURN
      END
      SUBROUTINE CLCSUM
C-----------------------------------------------------------------------
C   Subroutine to sum the flux densities of the Clean components in
C   the CC files.
C   Inputs:
C   Inputs from common:
C      MFIELD     I    Number of fields
C      NCLNG      I(*) Number of components per field.
C      CNAME     C(*)*32 Clean object names.
C      CCVER      I(*) Version numbers of the CC files per field.
C      NCLNG      I(*) Number of components per field
C   Output to common:
C      FLUXG      R(*) Sum of the clean components per field.
C      TFLUXG     R    Sum of components in all fields.
C-----------------------------------------------------------------------
C
      INTEGER   LIMIT, I, IFIELD, IERR, MTYPE, CCROW, NUMCOL
      REAL      X, Y, FLUX, PARMS(10)
      CHARACTER CCTAB*32
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      TFLUXG = 0.0
C                                       Loop over fields
      DO 100 IFIELD = 1,MFIELD
         FLUXG(IFIELD) = 0.0
C                                       Make CC table object
         CCTAB = 'Temporary CC table for CLCSUM'
         CALL IM2TAB (CNAME(IFIELD), CCTAB, 'CC', CCVER(IFIELD), IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Read Clean component file.
         IF (NCLNG(IFIELD).GT.0) THEN
            CALL OCCINI (CCTAB, 'READ', CCROW, NUMCOL, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       Loop summing components
            LIMIT = NCLNG(IFIELD)
            MTYPE = 0
            DO 60 I = 1,LIMIT
               CALL OTABCC (CCTAB, 'READ', CCROW, NUMCOL, X, Y, FLUX,
     *            MTYPE, PARMS, IERR)
               IF (IERR.LT.0) GO TO 60
               IF (IERR.GT.0) GO TO 90
C                                       Sum point & Gauss components.
               IF ((MTYPE.EQ.0) .OR. (MTYPE.EQ.1)) THEN
                  FLUXG(IFIELD) = FLUXG(IFIELD) + FLUX
                  TFLUXG = TFLUXG + FLUX
                  END IF
 60            CONTINUE
C                                       make sure it exists
         ELSE IF (CLNLIM.GT.0) THEN
            NUMCOL = 3
            IF (COMRES(IFIELD).GT.0.0) NUMCOL = 7
            CALL OCCINI (CCTAB, 'WRIT', CCROW, NUMCOL, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       skip it
         ELSE
            GO TO 100
            END IF
C                                       Close CC table
 90      CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, X, Y, FLUX, MTYPE,
     *      PARMS, IERR)
C                                       Delete temporary CC object
         CALL TABDES (CCTAB, IERR)
C                                       End of loop over fields
 100     CONTINUE
C                                       Save in imaging commons
      CALL OUSETF (TFLUXG, MFIELD, FLUXG)
C
 999  RETURN
      END
      SUBROUTINE CLGRID (IERR)
C-----------------------------------------------------------------------
C   Routine for image plane Cleaning only and can only process a single
C   field.
C      CLGRID Fourier transforms the Clean components and multiplies
C   times the negative of the weighting function so that when
C   retransformed and added to the Dirty map the current residual map
C   results.  Results are accumulated in object GRID so that only new
C   components need be transformed.
C      Note: needs sufficient "AP" memory to hold entire grid.
C   Input:
C   Input in common:
C      WFIRST L      .TRUE. iff this is first write on GRID. GRID will
C                    be created and initialized.
C      NCLNG  I(*)    highest number component to transform
C      NSUBG  I(*)    first Clean component to transform.
C      CNAME C(*)*32 Clean object names (only first used)
C      GRID   C*32    Grid work object, initialized if WFIRST.
C      TRANFN C*32    Transfer function.
C   Output in common:
C      ROW1   R(*)    Scratch array
C   Output:
C      IERR   I       Return code, 0=> OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   NLOAD, I, JLIM, PLANE(7), NUMCLN, FIRST, IAPOUT,
     *   MAXCMP, JNUM, IAPGRD, IAPSIZ, IAPWTS, IAPCMP, IAPCMS, APBUF,
     *   NUMBER, TWONY, ONENY, WRK1, WRK2, NX, NY, DIM(7), NDIM,
     *   NAXIS(7), BLC(7), TRC(7), KAP, NCR(7), DUMMY, JERR
      CHARACTER CDUMMY*1
      LOGICAL   DOSUM, APOPEN
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA APBUF /1/
C-----------------------------------------------------------------------
      DOSUM = .FALSE.
      APOPEN = .FALSE.
C                                       Set window in objects
      NX = IMSIZE(1,1)
      NY = IMSIZE(2,1)
C                                       Define GRID half plane complex
C                                       image.
      IF (WFIRST) THEN
         NDIM = 2
         NAXIS(1) = NY
         NAXIS(2) = NX / 2 + 1
         CALL FILL (7, 1, BLC)
         CALL COPY (7, NAXIS, TRC)
         DIM(1) = 1
         DIM(2) = 1
         DIM(3) = 0
C                                       Create object if necessary
         IF (GRID.EQ.' ') THEN
            GRID = 'Image plane Clean GRID object'
            CALL FILL (7, 1, NCR)
            NCR(1) = NX + 2
            NCR(2) = NY
            CALL IMGSCR (GRID, NCR, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
         CALL ARROPN (GRID, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARDPUT (GRID, 'NDIM', OOAINT, DIM, NDIM, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         DIM(1) = 7
         CALL ARDPUT (GRID, 'NAXIS', OOAINT, DIM, NAXIS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARRCWI (GRID, IERR)
         IF (IERR.NE.0) GO TO 995
         DIM(1) = 8
         CALL ARDPUT (GRID, 'DATATYPE', OOACAR, DIM, DUMMY, 'COMPLEX',
     *      IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARRCLO (GRID, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Determine Area assignments in AP
      ONENY = NY
      TWONY = 2 * NY
C                                       IAPWTS = transfer function
      IAPWTS = 100
C                                       IAPCMP = FT of components
      IAPCMP = IAPWTS + TWONY + 1
C                                       IAPGRD = whole GRID array
      IAPGRD = IAPCMP + TWONY + 1
C                                       IAPCMS = Components structures.
      IAPCMS = IAPGRD + (NX / 2 + 1) * (2 * NY) + 1
C                                       Determine AP buffer size.
      NLOAD = IAPGRD - 3
C                                       Determine number of components
C                                       to be Cleaned.
      NUMCLN = NCLNG(1) - NSUBG(1)
      IAPSIZ = IAPCMS + 5 * NUMCLN
      IAPSIZ = IAPSIZ / 1024 + 2
      CALL QINIT (IAPSIZ, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         MSGTXT = 'CLGRID: FAILED TO GET AP MEMORY'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 995
         END IF
      CALL APOBJ ('OPEN', 'CLGRID', IERR)
      IF (IERR.NE.0) GO TO 995
      APOPEN = .TRUE.
C                                       Get size of AP
      IAPSIZ = PSAPNW * 1024
C                                       Determine maximum number of comp
C                                       per pass.
      MAXCMP = (IAPSIZ - IAPCMS) / 5.0
C                                       Make sure "AP" big enough.
      IF (MAXCMP.LT.MIN(10,NUMCLN)) THEN
         MSGTXT = 'CLGRID: MEMORY TOO SMALL FOR IMAGE'
         IERR = 5
         GO TO 990
         END IF
C                                       Clear IAPGRD or load GRID
      IF (WFIRST) THEN
         WRK1 = (NX / 2 + 1) * (2 * NY)
         CALL QVCLR (IAPGRD, 1, WRK1)
      ELSE
         CALL FILL (7, 1, PLANE)
         CALL IMG2AP (GRID, PLANE, IAPGRD, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Begin component loop.
 10   CONTINUE
C                                       IAPOUT = AP address of result
         IAPOUT = IAPGRD
         FIRST = NSUBG(1) + 1
         NUMBER = MAXCMP
         IF (NUMCLN.LT.NUMBER) NUMBER = NUMCLN
C                                       Load NUMBER components this pass
         JNUM = NUMBER
         APBUF = 1
         CALL CLCCRM (1, DOSUM, .FALSE., IAPCMS, APBUF, FIRST, NUMBER,
     *      NLOAD, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Open transfer function
         CALL ARROPN (TRANFN, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Begin loop thru grid.
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                       Do direct transform in X.
            CALL QVCLR (IAPCMP, 1, TWONY)
            CALL QDIRAD (IAPCMS, 5, IAPCMP, NUMBER)
C                                       Read Transfer function
            CALL ARREAD (TRANFN, DIM, ROW1, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Do FFT in Y.
            CALL QWR
            CALL QCFFT (IAPCMP, ONENY, 1)
C                                       Load weights into AP.
            CALL QPUT (ROW1, IAPWTS, TWONY, 2)
            CALL QWD
C                                       Multiply by transfer fn
            CALL QCVMUL (IAPCMP, 2, IAPWTS, 2, IAPCMP, 2, ONENY, 1)
            CALL QWR
C                                       Subtract from previous GRID file
            CALL QVSUB (IAPCMP, 1, IAPOUT, 1, IAPOUT, 1, TWONY)
            IAPOUT = IAPOUT + NY * 2
C                                       Rotate DFT for next pass.
            WRK1 = IAPCMS + 1
            WRK2 = IAPCMS + 3
            CALL QCVMUL (WRK1, 5, WRK2, 5, WRK1, 5, NUMBER, 1)
            CALL QWR
 150        CONTINUE
C                                       End of this pass, close transfer
C                                       fn.
         CALL ARRCLO (TRANFN, IERR)
         IF (IERR.NE.0) GO TO 995
         NSUBG(1) = NSUBG(1) + JNUM
         NUMCLN = NUMCLN - JNUM
C                                       Check for more Clean components.
         WFIRST = .FALSE.
         IF (NSUBG(1).LT.NCLNG(1)) GO TO 10
C                                       Copy grid to disk
      CALL FILL (7, 1, PLANE)
      CALL AP2IMG (IAPGRD, GRID, PLANE, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL APOBJ ('CLOS', 'CLGRID', JERR)
      CALL QRLSE
      GO TO 999
C
 990  CALL MSGWRT (8)
 995  IF (APOPEN) THEN
         CALL QRLSE
         CALL APOBJ ('CLOS', 'CLGRID', JERR)
         END IF
      MSGTXT = 'CLGRID: ERROR SUBRTACTING ' // CNAME(1)
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLRMAX (IFIELD, NFIELD, ACTMAX, WINMAX, FLDMAX, FLDAVG,
     *   IERR)
C-----------------------------------------------------------------------
C   Find the maximum pixel value of a set of images.
C   Inputs:
C      TYPE     I      0 => over full images, 1 over windows
C      IFIELD   I      Field to do: 0 => all
C      NFIELD   I      Number of fields in CNAME
C   In/out - initialized only if IFIELD = 0
C      ACTMAX   R      Peak pixel value in abs value (but can be
C                      negative)
C      WINMAX   R      maximum absolute pixel value in Clean windows
C      FLDMAX   R(*)   max abs value by field in windows
C      FLDAVG   R(*)   average abs value in windows
C   Output:
C      IERR     I      Return code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, NFIELD, IERR
      REAL      ACTMAX, WINMAX, FLDMAX(*), FLDAVG(*)
C
      INTEGER   LFIELD, TYPE, DIM(7), LF1, LF2, IBOX, BLC(7), TRC(7),
     *   I, J, NNX, NNY, MAXX, MAXY, MINX, MINY, IX, IY, IDIS2, MXDIS2,
     *   NPTS, IP, IROUND, NSUM(10), NR, JX, JY
      LOGICAL   ROUND, DOUNBX, UNBROW, UNBCOL
      REAL      FMAX, FMIN, WMAX, WMIN, WSUM, R, X, Y, SUM(10), SUMS(10)
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = NFIELD
         ACTMAX = 0.0
         WINMAX = 0.0
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      CALL RFILL (10, 0.0, SUM)
      CALL RFILL (10, 0.0, SUMS)
      CALL FILL (10, 0, NSUM)
C                                       Loop over fields
      DO 100 LFIELD = LF1,LF2
         ELIMXF(LFIELD) = 0.0
         IF (IGNORE(LFIELD).LT.-1.5) GO TO 100
         CALL IMGET (CNAME(LFIELD), 'FIELDMAX', TYPE, DIM, FMAX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         CALL IMGET (CNAME(LFIELD), 'FIELDMIN', TYPE, DIM, FMIN, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF (ABS(FMIN).GT.ABS(ACTMAX)) ACTMAX = FMIN
         IF (ABS(FMAX).GT.ABS(ACTMAX)) ACTMAX = FMAX
         NR = (LFIELD - 1) / NFPRES + 1
C                                       do inscribed ellipse
C                                       always since need ELIMAX
         I = IROUND (AUTOBX(6))
         IF (I.LE.0) I = 5
         MINX = 1 + I
         MINY = 1 + I
         MAXX = IMSIZE(1,LFIELD) - I
         MAXY = IMSIZE(2,LFIELD) - I
         BLC(1) = MINX
         BLC(2) = MINY
         TRC(1) = MAXX
         TRC(2) = MAXY
         BLC(3) = CHANN
         TRC(3) = CHANN
         NNX = TRC(1) - BLC(1) + 1
         NNY = TRC(2) - BLC(2) + 1
         DIM(1) = 7
         DIM(2) = 1
         CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990

         CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Open pixel array
         CALL ARROPN (CNAME(LFIELD), 'READ', IERR)
         IF (IERR.NE.0) GO TO 990
         R = (NNX * NNY) / 2.0
         R = R * R
         WMAX = 0.0
C                                       Loop over window finding max/min
         DO 20 J = 1,NNY
            CALL ARREAD (CNAME(LFIELD), DIM, ROW1, IERR)
            IF (IERR.NE.0) GO TO 990
            Y = (J - NNY/2.0) * NNX
            Y = Y * Y
            DO 10 I = 1+1,NNX-1
               X = (I - NNX/2.0) * NNY
               X = X * X
               IF (X+Y.LE.R) THEN
                  WMAX = MAX (WMAX, ABS(ROW1(I-1)+ROW1(I)+ROW1(I+1)))
                  SUM(NR) = SUM(NR) + ROW1(I)
                  SUMS(NR) = SUMS(NR) + ROW1(I) * ROW1(I)
                  NSUM(NR) = NSUM(NR) + 1
                  END IF
 10            CONTINUE
 20         CONTINUE
         ELIMXF(LFIELD) = WMAX / 3.0
         CALL ARRCLO (CNAME(LFIELD), IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Loop over boxes
         IF ((LFIELD.GE.LF1) .AND. (LFIELD.LE.LF2)) THEN
            WMAX = 0.0
            WMIN = 0.0
            NPTS = 0
            WSUM = 0.0
            DO 90 IBOX = 1,NBOXES(LFIELD)
               IP = (IBOX-1) * MFIELD + LFIELD
C                                       Set window
               ROUND = WIN(1,IP) .EQ. -1
               IF (ROUND) THEN
                  MXDIS2 = WIN(2,IP)**2
                  MINX = WIN(3,IP) - WIN(2,IP)
                  MINY = WIN(4,IP) - WIN(2,IP)
                  MAXX = WIN(3,IP) + WIN(2,IP)
                  MAXY = WIN(4,IP) + WIN(2,IP)
               ELSE
                  MINX = WIN(1,IP)
                  MINY = WIN(2,IP)
                  MAXX = WIN(3,IP)
                  MAXY = WIN(4,IP)
                  END IF
C                                       Be careful
               BLC(1) = MAX (1, MINX)
               BLC(2) = MAX (1, MINY)
               TRC(1) = MIN (IMSIZE(1,LFIELD), MAXX)
               TRC(2) = MIN (IMSIZE(2,LFIELD), MAXY)
               BLC(3) = CHANN
               TRC(3) = CHANN
               NNX = TRC(1) - BLC(1) + 1
               NNY = TRC(2) - BLC(2) + 1
               IF ((NNX.LE.0) .OR. (NNY.LE.0)) GO TO 90
               DIM(1) = 7
               DIM(2) = 1
               CALL ARDPUT (CNAME(LFIELD), 'BLC', OOAINT, DIM, BLC,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990

               CALL ARDPUT (CNAME(LFIELD), 'TRC', OOAINT, DIM, TRC,
     *            CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Open pixel array
               CALL ARROPN (CNAME(LFIELD), 'READ', IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Loop over window finding max/min
               DO 50 J = 1,NNY
                  CALL ARREAD (CNAME(LFIELD), DIM, ROW1, IERR)
                  IF (IERR.NE.0) GO TO 990
                  JY = BLC(2) + J - 1
                  IF (ISUNBX) THEN
                     DOUNBX = UNBROW (LFIELD, JY)
                  ELSE
                     DOUNBX = .FALSE.
                     END IF
                  IF (ROUND) THEN
                     IY = JY - WIN(4,IP)
C                                       Round box
                     DO 30 I = 1,NNX
                        JX = BLC(1) + I - 1
                        IF (DOUNBX) THEN
                           IF (UNBCOL (LFIELD, JX, JY)) GO TO 30
                           END IF
                        IX = JX - WIN(3,IP)
                        IDIS2 = IX*IX + IY*IY
                        IF (IDIS2.LE.MXDIS2) THEN
                           WMAX = MAX (WMAX, ROW1(I))
                           WMIN = MIN (WMIN, ROW1(I))
                           WSUM = WSUM + ABS (ROW1(I))
                           NPTS = NPTS + 1
                           END IF
 30                     CONTINUE
C                                       Rectangular box
                  ELSE
                     DO 40 I = 1,NNX
                        IF (DOUNBX) THEN
                           JX = BLC(1) + I - 1
                           IF (UNBCOL (LFIELD, JX, JY)) GO TO 40
                           END IF
                        WMAX = MAX (WMAX, ROW1(I))
                        WMIN = MIN (WMIN, ROW1(I))
                        WSUM = WSUM + ABS (ROW1(I))
                        NPTS = NPTS + 1
 40                     CONTINUE
                     END IF
 50               CONTINUE
C                                       Close
               CALL ARRCLO (CNAME(LFIELD), IERR)
               IF (IERR.NE.0) GO TO 990
 90            CONTINUE
            FLDAVG(LFIELD) = 0.0
            IF (NPTS.GT.0) FLDAVG(LFIELD) = WSUM / NPTS
            FLDMAX(LFIELD) = MAX (WMAX, -WMIN)
            WINMAX = MAX (WINMAX, FLDMAX(LFIELD))
            DIM(1) = 1
            DIM(2) = 0
            CALL IMPUT (CNAME(LFIELD), 'FIELDRES', OOARE, DIM,
     *         FLDMAX(LFIELD), CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 100     CONTINUE
C                                       find overall maximum
      CALL RFILL (10, 0.0, ELIMXR)
      DO 110 LFIELD = 1,NFIELD
         IF (IGNORE(LFIELD).GE.0.0) THEN
            NR = (LFIELD - 1) / NFPRES + 1
            ELIMXR(NR) = MAX (ELIMXR(NR), ELIMXF(LFIELD))
            END IF
 110     CONTINUE
      ELIMAX = 0
      DO 120 NR = 1,NUMRES
         IF (NSUM(NR).GT.0) THEN
            SUM(NR) = SUM(NR) / NSUM(NR)
            SUMS(NR) = SUMS(NR) / NSUM(NR)
            SUMS(NR) = SUMS(NR) - SUM(NR) * SUM(NR)
            SUMS(NR) = SQRT (MAX (0.0, SUMS(NR)))
            IF ((ELIMXR(NR).LE.1.05*SUMS(NR)*AUTOBX(3)) .AND.
     *         (AUTOBX(1).GT.0.5)) THEN
               ELIMXR(NR) = 0.0
               LF1 = (NR - 1) * NFPRES + 1
               CALL RFILL (NFPRES, 0.0, ELIMXF(LF1))
               END IF
            ELIMAX = MAX (ELIMAX, ELIMXR(NR))
            END IF
 120     CONTINUE
      IF (AUTOBX(1).LE.0.5) CALL RFILL (MFIELD, 0.0, ELIMXF)
      GO TO 999
C                                       Error
 990  MSGTXT = 'CLRMAX: ERROR FINDING MAX ABS PIXEL VALUE'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE CLRMS (NFIELD, KF, FLDMAX, ELIMAX, FLDAVG, BMSCAL,
     *   RATIO)
C-----------------------------------------------------------------------
C   Find the ration of the average field score and its rms
C   Inputs:
C      NFIELD   I      Number of fields in CNAME or KF
C      KF       i(*)   List of fields KF(1)=0 => do all
C      FLDMAX   R(*)   max abs value by field in windows
C      ELIMAX   R(*)   max in inscribed ellipse
C      FLDAVG   R(*)   average abs value in windows
C      BMSCAL   R(*)   beam scale factor
C   Output:
C      RATIO    R      Average score / rms of scores
C      IERR     I      Return code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   NFIELD, KF(*)
      REAL      FLDMAX(*), ELIMAX(*), FLDAVG(*), BMSCAL(*), RATIO
C
      INTEGER   NV, IFIELD, I
      REAL      TX, A, CLOFNB
      DOUBLE PRECISION AS, ASS
C-----------------------------------------------------------------------
      NV = 0
      AS = 0.0D0
      ASS = 0.0D0
C                                       all fields
      IF (KF(1).LE.0) THEN
         DO 20 IFIELD = 1,NFIELD
            TX = MAX (FLDMAX(IFIELD), ELIMAX(IFIELD))
            A = CLOFNB (TX, FLDAVG(IFIELD), BMSCAL(IFIELD))
            IF (A.GT.0.0) THEN
               AS = AS + A
               ASS = ASS + A * A
               NV = NV + 1
               END IF
 20         CONTINUE
C                                       list of fields
      ELSE
         DO 30 I = 1,NFIELD
            IFIELD = KF(I)
            IF (IFIELD.GT.1000000) IFIELD = IFIELD - 1000000
            IF (IFIELD.GT.0) THEN
               TX = MAX (FLDMAX(IFIELD), ELIMAX(IFIELD))
               A = CLOFNB (TX, FLDAVG(IFIELD), BMSCAL(IFIELD))
               IF (A.GT.0.0) THEN
                  AS = AS + A
                  ASS = ASS + A * A
                  NV = NV + 1
                  END IF
               END IF
 30         CONTINUE
         END IF
      IF (NV.GT.0) THEN
         AS = AS / NV
         ASS = ASS / NV - AS * AS
         ASS = SQRT (MAX (0.0D0, ASS))
         END IF
      IF (ASS.GT.0.0D0) THEN
         RATIO = AS / ASS
      ELSE
         RATIO = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLIINF (IERR)
C-----------------------------------------------------------------------
C   Update image info in common.  A check is made to insure that the
C   size and cell spacing of the image has not changed.
C   Inputs:
C   Input in common:
C      MFIELD      I        Number of fields
C      CNAME       C(*)*32  Names of residual/Clean images
C   Output in common:
C      XPOFF       R(*)     X ref. pixel offset from center shift per
C                           field (deg)
C      YPOFF       R(*)     Y ref. pixel offset from center shift per
C                           field (deg)
C      MROTAT      R        Coordinate rotation (deg)
C   Output:
C      IERR       I    Return code, 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LFIELD, TYPE, DIM(7), NAXIS(7)
      REAL      CDELT(7), CROTA(7), CRPIX(7)
      CHARACTER CDUMMY*1
      INCLUDE 'QCLEAN.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Loop over fields
      DO 100 LFIELD = 1,MFIELD
C                                       Check size
         CALL ARDGET (CNAME(LFIELD), 'NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF ((NAXIS(1).NE.IMSIZE(1,LFIELD)) .OR.
     *      (NAXIS(1).NE.IMSIZE(1,LFIELD))) THEN
            MSGTXT = 'CLIINF: IMAGE SIZE HAS CHANGED.'
            IERR = 6
            GO TO 985
            END IF
C                                       Check Cell spacings
         CALL IMDGET (CNAME(LFIELD), 'CDELT', TYPE, DIM, CDELT, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         IF ((ABS (CELLSG(1)-CDELT(1)*3600.0).GT.1.0E-6*ABS(CELLSG(1)))
     *      .OR. (ABS (CELLSG(2)-CDELT(2) * 3600.0).GT.
     *       1.0E-6*ABS(CELLSG(2)))) THEN
            MSGTXT = 'CLIINF: IMAGE CELL SPACING HAS CHANGED.'
            IERR = 6
            GO TO 985
            END IF
C                                       Get rotation
         CALL IMDGET (CNAME(LFIELD), 'CROTA', TYPE, DIM, CROTA, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         MROTAT = CROTA(2)
C                                       Get reference pixels
         CALL IMDGET (CNAME(LFIELD), 'CRPIX', TYPE, DIM, CRPIX, CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Get offset of center from
C                                       reference pixel
         XPOFF(LFIELD) = (CRPIX(1) - ICNTRX(LFIELD)) * CELLSG(1) / 3600.
         YPOFF(LFIELD) = (CRPIX(2) - ICNTRY(LFIELD)) * CELLSG(2) / 3600.
 100     CONTINUE
      GO TO 999
C                                       Error
 985  CALL MSGWRT (7)
 990  MSGTXT = 'CLIINF: ERROR FINDING MAX ABS PIXEL VALUE'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE CLBSTF (NFIELD, RSSMAX, RSSAVG, IGNORE, BMSCAL, MINFLX,
     *    MNFFLX, ELIMXF, RESMAX, JCOUNT, LFLIST, FFMAX)
C-----------------------------------------------------------------------
C   Routine to decide which field to clean next.
C   Inputs:
C      NFIELD   I      Number of fields total
C      RSSMAX   R(*)   Field maximum residual
C      RSSAVG   R(*)   Field average residual
C      IGNORE   R(*)   Field acceptablility - should be 1
C      BMSCAL   R(*)   Field beam scaling factor
C      MINFLX   R      Minimum acceptable component flux
C      MNFFLX   R(*)   min acceptable component flux by field
C      ELIMXF   R(*)   max in inscribed ellipse if autoboxing, else 0
C   Outputs:
C      RESMAX   R      Peak residual of field LFLIST(1)
C      JCOUNT   I      Count of things in LFLIST
C      FFMAX    R      Max FF value found
C      LFLIST   I(*)   Field numbers in order: at least dimension 10
C-----------------------------------------------------------------------
      INTEGER   NFIELD, JCOUNT, LFLIST(*)
      REAL      RSSMAX(NFIELD), RSSAVG(NFIELD), BMSCAL(NFIELD), MINFLX,
     *   RESMAX, MNFFLX(NFIELD), IGNORE(NFIELD), ELIMXF(NFIELD), FFMAX
C
      INTEGER   SZLIST
      PARAMETER (SZLIST = 10)
      INTEGER   LFIELD
      REAL      CLOFNB, FF, TX, TA, FFLIST(SZLIST), TI
C-----------------------------------------------------------------------
      RESMAX = -1.0E-9
      CALL FILL (SZLIST, 0, LFLIST)
C                                       Find maximum objective function
      JCOUNT = 0
      DO 20 LFIELD = 1,NFIELD
         TX = RSSMAX(LFIELD) * IGNORE(LFIELD)
         TA = RSSAVG(LFIELD) * IGNORE(LFIELD)
         TI = ELIMXF(LFIELD) * IGNORE(LFIELD)
         TX = MAX (TX, TI)
         FF = CLOFNB (TX, TA, BMSCAL(LFIELD))
         IF ((TX.GT.MINFLX) .AND. (TX.GT.MNFFLX(LFIELD)))
     *      CALL CLORDR (LFIELD, FF, JCOUNT, FFLIST, LFLIST)
 20      CONTINUE
C                                      If nothing acceptable use a
C                                      slightly relaxed criterion
      IF (JCOUNT.EQ.0) THEN
         DO 40 LFIELD = 1,NFIELD
            TX = RSSMAX(LFIELD) * IGNORE(LFIELD)
            TI = ELIMXF(LFIELD) * IGNORE(LFIELD)
            TX = MAX (TX, TI)
            FF = TX * BMSCAL(LFIELD)
            IF ((TX.GT.MINFLX) .AND. (TX.GT.MNFFLX(LFIELD)))
     *         CALL CLORDR (LFIELD, FF, JCOUNT, FFLIST, LFLIST)
 40         CONTINUE
         END IF
C                                       simply return answer
      IF (JCOUNT.GE.1) THEN
         RESMAX = MAX (RSSMAX(LFLIST(1)), ELIMXF(LFLIST(1)))
         FFMAX = FFLIST(1)
         END IF
C
 999  RETURN
      END
      REAL FUNCTION CLOFNB (RSSMAX, RSSAVG, BMSCAL)
C-----------------------------------------------------------------------
C   Function to compute the objective function for deciding which field
C   to clean next.
C      CLOFNB = (0.95 * RSSMAX + 0.05 * RSSAVG) * BMSCAL
C   Inputs:
C      RSSMAX   R  Field maximum residual
C      RSSAVG   R  Field average residual
C      BMSCAL   R  Field beam scaling factor
C   Returns
C      The objective function for deciding the best field to clean next.
C-----------------------------------------------------------------------
      REAL      RSSMAX, RSSAVG, BMSCAL
C
C-----------------------------------------------------------------------
      CLOFNB = (0.95 * RSSMAX + 0.05 * RSSAVG) * BMSCAL
C
 999  RETURN
      END
      SUBROUTINE CLORDR (LFIELD, FF, COUNT, FFLIST, LFLIST)
C-----------------------------------------------------------------------
C   Routine to build an ordered list up to 10 long
C   Inputs:
C      LFIELD   I       Field number
C      FF       R       Function value for this field
C   In/Out:
C      COUNT    I       Number of items in lists
C      FFLIST   R(10)   List of FF values in order
C      LFLIST   I(10)   List of corresponding field number
C-----------------------------------------------------------------------
      INTEGER   LFIELD, COUNT, LFLIST(*)
      REAL      FF, FFLIST(*)
C
      INTEGER   SZLIST
      PARAMETER (SZLIST = 10)
      INTEGER   I, J
C-----------------------------------------------------------------------
C                                       null case
      IF (COUNT.EQ.0) THEN
         COUNT = COUNT + 1
         LFLIST(1) = LFIELD
         FFLIST(1) = FF
C                                       Find where this goes in list
      ELSE
         DO 10 I = 1,COUNT
            IF (FF.GT.FFLIST(I)) GO TO 20
 10         CONTINUE
C                                       lowest yet
         IF (COUNT.LT.SZLIST) THEN
            COUNT = COUNT + 1
            LFLIST(COUNT) = LFIELD
            FFLIST(COUNT) = FF
            END IF
         GO TO 999
C                                       need to insert
 20      COUNT = MIN (SZLIST, COUNT+1)
         DO 40 J = COUNT,I+1,-1
            LFLIST(J) = LFLIST(J-1)
            FFLIST(J) = FFLIST(J-1)
 40         CONTINUE
         LFLIST(I) = LFIELD
         FFLIST(I) = FF
         END IF
C
 999  RETURN
      END
      SUBROUTINE BOXFIX (CNAME, MFIELD, NGAUSS, FNAME, IMSIZE, PRTLEV,
     *   DOFILE, NBOXES, WIN, UNBOXS, UNWIN, IRET)
C-----------------------------------------------------------------------
C   Checks for Clean boxes that overlap between different facets of the
C   same Gaussian width.  The assumptions are that the facets are
C   numbered 1-NFACET for the 1st Gaussian width, NFACET+1-2*NFACET for
C   the second Gaussian width, etc with NFACET = MFIELD / NGAUSS.
C   BOXFIX uses BOXCHK to list the overlaps and then deletes the smaller
C   box from the list.
C   Inputs:
C      CNAME    C*(*)           name of Object w window info
C      MFIELD   I               Total number facets = NFACET*NGAUSS
C      NGAUSS   I               Number of Gaussian widths
C      FNAME    C(MFIELD)*(*)   Facet image names
C      IMSIZE   I(2,MFIELD)     image pixel size
C      PRTLEV   I               0 - relatively quiet, 1 - list overlaps
C      DOFILE   L               Update the OBOXFILE if needed
C      UNBOXS   I(*)            Number UNClean boxes per file
C      UNWIN    I(4,MFIELD,*)   UNClean boxes
C   In/Out:
C      NBOXES   I(*)            Number boxes in each facet also changed
C                                  in CNAME
C      WIN      I(4,MFIELD,*)   Clean boxes also changed in CNAME
C   Output:
C      IRET     I               Error if not 0
C-----------------------------------------------------------------------
      INTEGER   MFIELD, NGAUSS, IMSIZE(2,MFIELD), PRTLEV,
     *   NBOXES(MFIELD), WIN(4,MFIELD,*), UNBOXS(MFIELD),
     *   UNWIN(4,MFIELD,*), IRET
      LOGICAL   DOFILE
      CHARACTER CNAME*(*), FNAME(*)*(*)
C
      INTEGER   NOV, LOV, FOV(5,1000), I, J, K, IB, JB, MSGSAV, TYPE1,
     *   DIM1(7), TYPE2, DIM2(7), DUMMY, LERR
      CHARACTER CDUMMY*1, OBXFIL*48
      REAL      XI, XJ, CXI, CXJ, CYI, CYJ
      LOGICAL   CHANGD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      LOV = 1000
      CALL CLNGET (CNAME, 'NBOXES', TYPE1, DIM1, NBOXES, CDUMMY, IRET)
      IF (IRET.EQ.0) CALL CLNGET (CNAME, 'WINDOW', TYPE2, DIM2, WIN,
     *   CDUMMY, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'BOXFIX CANNOT GET INPUT BOXES'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CHANGD = .FALSE.
C                                       loop if way too many overlaps
 10   NOV = LOV
      MSGSAV = MSGSUP
C                                       messages on
      MSGSUP = 1
      IF ((PRTLEV.GT.0) .AND. (.NOT.CHANGD)) MSGSUP = 0
      CALL BOXCHK (MFIELD, NGAUSS, FNAME, IMSIZE, NBOXES, WIN, NOV, FOV,
     *   IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      IF (NOV.GT.0) THEN
         DO 20 K = 1,NOV
            IB = FOV(1,K)
            I  = FOV(2,K)
            JB = FOV(3,K)
            J  = FOV(4,K)
            IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *         (FOV(5,K).GT.1)) THEN
               IF (WIN(1,I,IB).LT.0) THEN
                  XI = PI * WIN(2,I,IB) * WIN(2,I,IB)
                  CXI = WIN(3,I,IB)
                  CYI = WIN(4,I,IB)
               ELSE
                  XI = (WIN(3,I,IB) - WIN(1,I,IB) + 1.) *
     *               (WIN(4,I,IB) - WIN(2,I,IB) + 1.)
                  CXI = (WIN(1,I,IB) + WIN(3,I,IB)) / 2.0
                  CYI = (WIN(2,I,IB) + WIN(4,I,IB)) / 2.0
                  END IF
               IF (WIN(1,J,JB).LT.0) THEN
                  XJ = PI * WIN(2,J,JB) * WIN(2,J,JB)
                  CXJ = WIN(3,J,JB)
                  CYJ = WIN(4,J,JB)
               ELSE
                  XJ = (WIN(3,J,JB) - WIN(1,J,JB) + 1.) *
     *               (WIN(4,J,JB) - WIN(2,J,JB) + 1.)
                  CXJ = (WIN(1,J,JB) + WIN(3,J,JB)) / 2.0
                  CYJ = (WIN(2,J,JB) + WIN(4,J,JB)) / 2.0
                  END IF
               IF (XJ.GT.1.1*XI) THEN
                  WIN(1,I,IB) = -999
               ELSE IF (XI.GT.1.1*XJ) THEN
                  WIN(1,J,JB) = -999
               ELSE
                  XI = (IMSIZE(1,I)/2.0-CXI)**2 +
     *               (IMSIZE(2,I)/2.0-CYI)**2
                  XJ = (IMSIZE(1,J)/2.0-CXJ)**2 +
     *               (IMSIZE(2,J)/2.0-CYJ)**2
                  IF (XJ.LT.0.90*XI) THEN
                     WIN(1,I,IB) = -999
                  ELSE IF (XI.LT.0.9*XJ) THEN
                     WIN(1,J,JB) = -999
                  ELSE IF (I.GT.J) THEN
                     WIN(1,I,IB) = -999
                  ELSE
                     WIN(1,J,JB) = -999
                     END IF
                  END IF
               END IF
 20         CONTINUE
         DO 40 I = 1,MFIELD
            JB = 0
            DO 30 IB = 1,NBOXES(I)
               IF (WIN(1,I,IB).GT.-99) THEN
                  JB = JB + 1
                  IF (JB.LT.IB) THEN
                     WIN(1,I,JB) = WIN(1,I,IB)
                     WIN(2,I,JB) = WIN(2,I,IB)
                     WIN(3,I,JB) = WIN(3,I,IB)
                     WIN(4,I,JB) = WIN(4,I,IB)
                     END IF
                  END IF
 30            CONTINUE
            WRITE (MSGTXT,1030) I, NBOXES(I), JB
            IF (NBOXES(I).NE.JB) THEN
               CHANGD = .TRUE.
               CALL MSGWRT (4)
               END IF
            NBOXES(I) = JB
 40         CONTINUE
         IF (NOV.EQ.LOV) GO TO 10
         END IF
C                                       any remining overlaps incl 1 pt
      MSGSUP = 1
      CALL BOXCHK (MFIELD, NGAUSS, FNAME, IMSIZE, NBOXES, WIN, NOV, FOV,
     *   IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      DO 50 K = 1,NOV
         IB = FOV(1,K)
         I  = FOV(2,K)
         JB = FOV(3,K)
         J  = FOV(4,K)
         IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *      (FOV(5,K).GE.1)) THEN
            WRITE (MSGTXT,1040) IB, I, JB, J, FOV(5,K)
            IF ((I.NE.J) .OR. (FOV(5,K).GT.1)) CALL MSGWRT (4)
            END IF
 50      CONTINUE
C                                       update object and even oboxfile
      IF (CHANGD) THEN
         CALL CLNPUT (CNAME, 'NBOXES', TYPE1, DIM1, NBOXES, CDUMMY,
     *      LERR)
         IF (LERR.EQ.0) CALL CLNPUT (CNAME, 'WINDOW', TYPE2, DIM2, WIN,
     *      CDUMMY, LERR)
         IF (LERR.NE.0) THEN
            MSGTXT = 'BOXFIX NABLE TO UPDATE OBJECT BOXES'
            CALL MSGWRT (7)
         ELSE IF (DOFILE) THEN
            MSGSUP = 32000
            CALL OGET (CNAME, 'OBOXFILE', TYPE1, DIM1, DUMMY, OBXFIL,
     *         LERR)
            MSGSUP = MSGSAV
            IF ((LERR.EQ.0) .AND. (OBXFIL.NE.' ')) THEN
               CALL QCWRBX (OBXFIL, NBOXES, MFIELD, WIN, UNBOXS, UNWIN,
     *            LERR)
               IF (LERR.NE.0) THEN
                  MSGTXT = 'FIXBOX UNABLE TO UPDATE OBOXFILE CORRECTLY'
                  CALL MSGWRT (7)
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('BOXFIX: Field',I5,' number boxes reduced from',I5,' to',
     *   I5)
 1040 FORMAT ('Box',I5,' facet',I5,' overlaps box',I5,' facet',I5,' at',
     *   I2,' points')
      END
      SUBROUTINE QCWRBX (BOXFIL, NBOXES, NFIELD, WIN, UNBOXS, UNWIN,
     *   IERR)
C-----------------------------------------------------------------------
C   Writes a text file with the Clean boxes and UNClean boxes in a
C   suitable format to be put back into IMAGR et al
C   Inputs:
C      BOXFIL   C*48            File name - created if new,
C                               over-written if old
C      NBOXES   I(NFIELD)       Number of boxes in field
C      NFIELD   I               Number of fields
C      WIN      I(4,NFIELD,*)   Clean windows
C      UNBOXS   I(NFIELD)       Number of UNClean boxes in field
C      UNWIN    I(4,NFIELD,*)   UNClean windows
C   Outputs:
C      IERR     I               Error code
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*(*)
      INTEGER   NBOXES(*), NFIELD, WIN(4,NFIELD,*), UNBOXS(*),
     *   UNWIN(4,NFIELD,*), IERR
C
      INTEGER   NF, NB, LUN, LUN2, FIND, FIND2, MSGSAV, ABOX(4), I, J,
     *   COLP, JTRIM, IROUND, KBP, II
      CHARACTER SCNAME*256, LINE*36, TXLINE*132, ULOG*24, UNAME*48,
     *   TMLINE*132
      LOGICAL   SAVEBU
      DOUBLE PRECISION X
      REAL      RDUM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN, LUN2 /11, 10/
C-----------------------------------------------------------------------
      IERR = 0
      SCNAME = ' '
      IF ((NFIELD.GT.0) .AND. (BOXFIL.NE.' ')) THEN
         SAVEBU = .FALSE.
C                                       is there a file
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, FIND, BOXFIL, .FALSE., IERR)
         MSGSUP = MSGSAV
C                                       back it up
         IF (IERR.EQ.0) THEN
            COLP = INDEX (BOXFIL, ':')
            IF (COLP.LE.1) THEN
               CALL ZFULLN (' ', 'FITS', 'FILEBOX', SCNAME, IERR)
            ELSE
               ULOG = BOXFIL(:COLP-1)
               UNAME = BOXFIL(COLP+1:)
               CALL ZFULLN (' ', ULOG, UNAME, SCNAME, IERR)
               END IF
            IF (IERR.NE.0) GO TO 970
            CALL ZTXOPN ('QWRT', LUN2, FIND2, SCNAME, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 970
            DO 10 I = 1,100000
               CALL ZTXIO ('READ', LUN, FIND, TXLINE, IERR)
               IF (IERR.EQ.2) GO TO 20
               IF (IERR.NE.0) GO TO 965
               J = JTRIM (TXLINE)
               J = MAX (1, J)
               CALL ZTXIO ('WRIT', LUN2, FIND2, TXLINE(:J), IERR)
               IF (IERR.NE.0) GO TO 965
 10            CONTINUE
 20         SAVEBU = .TRUE.
C                                       close both
            CALL ZTXCLS (LUN, FIND, IERR)
            CALL ZTXCLS (LUN2, FIND2, IERR)
C                                       this is less dangerous!
            CALL ZTXZAP (LUN, BOXFIL, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'COULD NOT DELETE ' // BOXFIL
               CALL MSGWRT (6)
               END IF
            CALL ZTXOPN ('WRIT', LUN, FIND, BOXFIL, .FALSE., IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'COULD NOT CREATE ' // BOXFIL
               CALL MSGWRT (6)
               GO TO 971
               END IF
            CALL ZTXOPN ('QRED', LUN2, FIND2, SCNAME, .FALSE., IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'COULD NOT OPEN FOR READ ' // SCNAME
               CALL MSGWRT (6)
               GO TO 970
               END IF
C                                       read back backup to new orig.
            DO 40 I = 1,100000
               CALL ZTXIO ('READ', LUN2, FIND2, TXLINE, IERR)
               IF (IERR.EQ.2) GO TO 50
               IF (IERR.NE.0) THEN
                  MSGTXT = 'COULD NOT READ ' // SCNAME
                  CALL MSGWRT (6)
                  GO TO 965
                  END IF
               CALL CHTRIM (TXLINE, 132, TMLINE, J)
               IF ((TMLINE(:1).EQ.'U') .OR. (TMLINE(:1).EQ.'U')) THEN
                  KBP = 2
               ELSE
                  KBP = 1
                  IF (TMLINE(:1).EQ.' ') GO TO 30
                  IF (TMLINE(:1).LT.'0') GO TO 30
                  IF (TMLINE(:1).GT.'9') GO TO 30
                  END IF
               CALL GETNUM (TMLINE, 132, KBP, X)
               IF (X.EQ.DBLANK) THEN
                  MSGTXT = 'ERROR PARSING ' // TMLINE
                  CALL MSGWRT (6)
                  GO TO 965
                  END IF
               RDUM = X
               J = IROUND (RDUM)
               IF ((J.GE.1) .AND. (J.LE.NFIELD)) GO TO 40
C                                       write to output
 30            J = JTRIM (TXLINE)
               J = MAX (1, J)
               CALL ZTXIO ('WRIT', LUN, FIND, TXLINE(:J), IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'COULD NOT WRITE ' // BOXFIL
                  CALL MSGWRT (6)
                  GO TO 965
                  END IF
 40            CONTINUE
 50         CALL ZTXCLS (LUN2, FIND2, IERR)
C                                       create a new BOXFILE
         ELSE
            CALL ZTXOPN ('WRIT', LUN, FIND, BOXFIL, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         DO 100 NF = 1,NFIELD
C                                       Clean boxes
            II = 0
            DO 60 NB = 1,NBOXES(NF)
               CALL COPY (4, WIN(1,NF,NB), ABOX)
               IF ((ABOX(1).NE.0) .AND. (ABOX(2).GT.0) .AND.
     *            (ABOX(3).GT.0) .AND. (ABOX(4).GT.0)) THEN
                  WRITE (LINE,1050) NF, ABOX
                  CALL CHTRIM (LINE, 36, LINE, J)
                  CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IERR)
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'COULD NOT 1 WRITE ' // BOXFIL
                     CALL MSGWRT (6)
                     GO TO 970
                     END IF
                  II = II + 1
                  END IF
 60            CONTINUE
C                                       or null bux
            IF (II.LE.0) THEN
               CALL FILL (4, 0, ABOX)
               WRITE (LINE,1050) NF, ABOX
               CALL CHTRIM (LINE, 36, LINE, J)
               CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'COULD NOT 2 WRITE ' // BOXFIL
                  CALL MSGWRT (6)
                  GO TO 970
                  END IF
               END IF
C                                       UNClean boxes
            DO 70 NB = 1,UNBOXS(NF)
               CALL COPY (4, UNWIN(1,NF,NB), ABOX)
               IF ((ABOX(1).NE.0) .AND. (ABOX(2).GT.0) .AND.
     *            (ABOX(3).GT.0) .AND. (ABOX(4).GT.0)) THEN
                  WRITE (LINE,1060) NF, ABOX
                  CALL CHTRIM (LINE, 36, LINE, J)
                  CALL ZTXIO ('WRIT', LUN, FIND, LINE(:J), IERR)
                  IF (IERR.NE.0) THEN
                     MSGTXT = 'COULD NOT 3 WRITE ' // BOXFIL
                     CALL MSGWRT (6)
                     GO TO 970
                     END IF
                  END IF
 70            CONTINUE
 100        CONTINUE
         SAVEBU = .FALSE.
         GO TO 970
         END IF
      GO TO 999
C                                        Close downs
 965  CALL ZTXCLS (LUN2, FIND2, IERR)
 970  CALL ZTXCLS (LUN, FIND, IERR)
 971  IF (SCNAME.NE.' ') THEN
         IF (SAVEBU) THEN
            MSGTXT = 'OBOXFILE backup saved in file:'
            CALL MSGWRT (8)
            MSGTXT = SCNAME
            CALL MSGWRT (8)
         ELSE
            CALL ZTXZAP (LUN2, SCNAME, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT (I4,4I7)
 1060 FORMAT ('U ',I4,4I7)
      END
      LOGICAL FUNCTION UNBROW (LFIELD, ROW)
C-----------------------------------------------------------------------
C   UNBROW returns true when the specified row is present in some
C   UNClean box (do NOT Clean here box)
C   Inputs:
C      LFIELD   I   Field number
C      ROW      I   Row number
C   Outputs:
C      UNBROW   L   T => row inside an UNClean box
C-----------------------------------------------------------------------
      INTEGER   LFIELD, ROW
C
      INTEGER   I, K
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      UNBROW = .TRUE.
      K = (LFIELD - 1) * MFIELD
      DO 20 I = 1,UNBOXS(LFIELD)
         K = K + 1
         IF (UNWIN(1,K).EQ.-1) THEN
            IF ((ROW.GE.UNWIN(4,K)-WIN(2,K)) .AND.
     *         (ROW.LE.UNWIN(4,K)+WIN(2,K))) GO TO 999
         ELSE
            IF ((ROW.GE.UNWIN(2,K)) .AND. (ROW.LE.UNWIN(4,K))) GO TO 999
            END IF
 20      CONTINUE
      UNBROW = .FALSE.
C
 999  RETURN
      END
      LOGICAL FUNCTION UNBCOL (LFIELD, COL, ROW)
C-----------------------------------------------------------------------
C   UNBROW returns true when the specified pixel is present in some
C   UNClean box (do NOT Clean here box)
C   Inputs:
C      LFIELD   I   Field number
C      ROW      I   Row number
C   Outputs:
C      UNBROW   L   T => col and row inside an UNClean box
C-----------------------------------------------------------------------
      INTEGER   LFIELD, COL, ROW
C
      INTEGER   I, K, X, Y, R
      INCLUDE 'QCLEAN.INC'
C-----------------------------------------------------------------------
      UNBCOL = .TRUE.
      K = (LFIELD - 1) * MFIELD
      DO 20 I = 1,UNBOXS(LFIELD)
         K = K + 1
         IF (UNWIN(1,K).EQ.-1) THEN
            R = UNWIN(2,K) * UNWIN(2,K)
            X = COL - UNWIN(3,K)
            Y = ROW - UNWIN(4,K)
            IF (X*X+Y*Y.LE.R) GO TO 999
         ELSE
            IF ((ROW.GE.UNWIN(2,K)) .AND. (ROW.LE.UNWIN(4,K)) .AND.
     *         (COL.GE.UNWIN(1,K)) .AND. (COL.LE.UNWIN(3,K))) GO TO 999
            END IF
 20      CONTINUE
      UNBCOL = .FALSE.
C
 999  RETURN
      END
