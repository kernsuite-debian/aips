C    CLEAN Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "CLEAN" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2009-2010, 2015
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
C    A CLEAN Object contains one or more image object and allows access
C    to CLEAN deconvolution products.  Only one CLEAN object is allowed
C    to be opened at a time. CLEANs may be restarted from a previous
C    result.
C       Two types of CLEAN are supported in this package: 1) the "Clark"
C    CLEAN in which all components are subtracted using images and
C    their FFTs. and 2) the Cotton-Schwab CLEAN for which the
C    components are subtracted from the ungridded visibility data.
C    The method is indicated using CLEANTYP and the useage of these two
C    methods differs somewhat both in the values attached to the clean
C    object and the cleaning routines (CLNIM for "IMAGE" and CLNUV for
C    "UV") are used to perform the CLEAN.
C       For the CLARK or "IMAGE" clean a dirty image must be provided.
C    Since this method can work for only a single image only a single
C    dirty image can be provided.  Since the clean image is a simple
C    derivative of the dirty image it is cloned from the dirty image by
C    the CLEAN process if it does not previously exist.
C       The Cotton-Schwab or 'UV' method can CLEAN several fields from
C    the same set of UV data.  For this method both the UV data object
C    and the CLEAN objects must be specified (size etc.) although the
C    CLEAN object need not have it's underlying files created prior to
C    opening the CLEAN object.  Routine U2IDES (UVUTIL module) is useful
C    for defining the image produced from a uvdata.  Any calibration,
C    editing, selection and conversion to Stokes' "I" should be done
C    prior to calling CLNUV.  These steps can be done with UV2SCR and
C    OUNFWT. The UV data object is modified during the CLEAN process;
C    the residual data is returned.  If not CLEANing is specified
C    (NITER =0)  then only the image and beam are made.
C       The residuals can optionally be scaled to the same physical
C    units as the restored components before the restoration is done.
C    This is requested via SCALRES and uses the parameters BMSCLSZ to
C    define the region of the beam in which to determine its area.
C
C       Note: Most of the useful functions of the class are in module
C    $QOOP/QCLEAN.FOR as they  use the "Array Processor".
C       Usage notes:
C    1) Before use a CLEAN object must be "opened" using CLNOPN.  Only
C    one CLEAN object is allowed to be "open" at a time.
C    2) Complete CLEANs can be done using CLNIM for Image CLEANs and
C    CLNUV for UV data CLEANs.  Multiple channels require seperate calls
C    to CLNIM or CLNUV but don't require separate opens and closes.
C    3) A CLEAN object must be closed (CLNCLO) before another can be
C    opened. Closing the CLEAN object causes any scratch files used in
C    the CLEAN to be destroyed.
C    4) Control parameters needed for imaging the uvdata in the "UV"
C    clean are attached to the first CLEAN and BEAM image object.
C    Inputs attached to CLEANI(1): (defaulted where approproate)
C      FTTYPE    C*4   Fourier transform type 'FFT' or 'DFT'. ('FFT')
C      IMSIZE    I(2,*) Image size per field (no default)
C      CELLSIZE  R(2)  Cellsize in arcseconds in X and Y (no default)
C      CHTYPE    C*4   'LINE',  or 'SUM ' for imaging ('SUM')
C      SHIFT     R(2)  Shift in arcsec (DFT imaging)
C      RASHIFT   R(*)  X position shift in arcseconds per field (0) FFT
C      DECSHIFT  R(*)  Y position shift in arcseconds per field (0) FFT
C      CENTERX   I(*)  Center X pixel position per field (std default)
C      CENTERY   I(*)  Center Y pixel position per field (std default)
C      CTYPX     I     X convolving function type (std default)
C      XPARM     R(10) X convolving function parameters( std default)
C      CTYPY     I     Y convolving function type (std default)
C      YPARM     R(10) Y convolving function parameters (std default)
C      DOZERO    L     IF true do Zero spacing flux (do if value given)
C      ZEROSP    R(5)  Zero spacing parameters (no zero spacing flux)
C      TFLUXG    R     Total flux to be subtracted from ZEROSP (0.0)
C      DOTAPER   L     If true taper (do if non zero taper given)
C      UVTAPER   R(2)  X and Y taper values (no taper)
C    Inputs attached to DIRTBEAM:
C      IMSIZE    I(2)  Size of beam (no default)
C
C    Class public members:
C    The following must be specified before opening the object:
C      CLEANTYP    C*8          'IMAGE' or 'UV', ' ' => Image
C    The following are for uv plane cleans only:
C      NIMAGE      I             Number of associated images (called
C                                fields in class)
C      UVDATA      C*32          UV data, contents will be current
C                                residual data.
C      UVCHAN      I             First channel number in uv data to
C                                process.
C      NCHAV       I             Number of channels to average.
C    The following are for image plane cleans only:
C      DIRTYI      C(*)*32       Names of associated dirty images =
C                                objects of type 'IMAGE', Should have
C                                OUTNAME, OUTCLASS, OUTSEQ, OUTDISK set
C                                to specify CLEANI
C    The following are for all CLEANs:
C      CHANNEL     I             Selected frequency channel. (default=1)
C      VERSION     I             CC file version number = CHANNEL for
C                                spectral data. (default = new)
C      DIRTBEAM    C*32          Name of dirty beam 'IMAGE' object.
C      CLEANI      C(*)*32       Names of associated clean images =
C                                objects of type 'IMAGE', May be either
C                                residual or restored.
C      BEAM        Base class    Beam object (default = fit)
C                                NOTE: Sizes are in degrees NOT asec.
C      NITER       I             Total number of iterations.
C                                (default=200)
C      BCOMP       I(*)          Restart using BCOMP components per
C                                field. (default = 0)
C      GAIN        R             Loop gain (default=0.2)
C      PHAT        R             Prussian helment spike size (default=0)
C      MINPATCH    I             Minimum BEAM-Half Width
C      MAXPATCH    I             Maximum BEAM-Half Width
C      MINFLUX     R             Min. residual flux. (default = 0)
C      FACTOR      R             CLEAN depth factor. (default = 0)
C      NORESTOR    L             If .true. don't restore components to
C                                CLEANI. (default = .false.)
C      DOBEAM      L             If true make a new dirty beam when
C                                starting a CLEAN (default = .true.)
C      NBOXES      I(*)          Number of windows (max. 10) per field.
C      WINDOW      I(4,50,*)     CLEAN windows, one per IMAGE (default =
C                                1 window per field excluding outer 5
C                                pixels).
C                                WINDOW(1,*)=-1 indicates a round box of
C                                width WINDOW(2,*) pixels centered on
C                                pixel (WINDOW(3,*), WINDOW(4,*))
C      SCALERES    L             If true scale residuals by ratio of
C                                dirty to restoring beam areaa (false)
C      BMSCLSZ     I(2)          Halfwidth in x and y of box around beam
C                                center to be used to determine dirty
C                                beam area. (5,5)
C      MAXNRES     I             Maximum number of residuals considered
C                                each CLEAN cycle (20,000)
C   The following are available only after CLEANing
C      NCOMP       I(*)          Number of components per IMAGE
C      TFLUX       R             Total flux
C      FLUX        R(*)          Flux for each field
C      GRID        C*32          Name of grid object for image plane
C                                CLEAN.
C      TRANFN      C*32          Name of transfer function (FT of beam)
C                                for image plane CLEAN.
C      WORK1       C*32          Name of scratch image object
C      WORK2       C*32          Name of scratch image object
C
C   Class  private data:
C      ACTIVE      L        If true a CLEAN object is active. (open)
C      ONEBEM      L        One beam per resolution
C      NFIELD      I        Number of fields per resolution
C      MFIELD      I        Number of fields
C      CTYPE       C*8      CLEAN type 'IMAGE' or 'UV'
C      UVDATA      C*32     Name of UV data object
C      DNAME       C(*)*32  Names of Dirty images
C      CNAME       C(*)*32  Names of residual/CLEAN images
C      DBNAME      C(*)*32  Name of dirty beam image.
C
C   Public functions:
C     CLNCRE (name, ierr)
C        Creates an Clean object with name "name".
C     CLNDES (name, ierr)
C        Destroys the Clean object with name "name";
C        quasi-permanent  forms are unaffected.
C     CLNZAP (name, ierr)
C        Destroys the Clean object with name "name";
C        quasi-permanent forms (CLEAN images only) are deleted.
C     CLNOPN (name, status, ierr)
C        Opens an Clean object.  Checks for valid data.
C     CLNCLO (name, ierr)
C        Closes an Clean object. Destroy scratch files.
C     CLNGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     CLNPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C
C   Private functions:
C      CLBGET (name, keywrd, type, dim, value, valuec, ierr)
C         Fetches member of a base class of Clean class
C      CLBPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Stores member of a base class of Clean class
C-----------------------------------------------------------------------
LOCAL INCLUDE 'CLEAN.INC'
C                                       Include for CLEAN class.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   ACTIVE, ONEBEM
      INTEGER   MFIELD, NFIELD
      CHARACTER CTYPE*8, UVDATA*32, DNAME(MAXFLD)*32, CNAME(MAXFLD)*32,
     *   DBNAME(MAXFLD)*32
      COMMON /CLNCCM/ MFIELD, NFIELD, ONEBEM, ACTIVE
      COMMON /CLNMCC/ CTYPE, UVDATA, DNAME, CNAME, DBNAME
C                                                          End CLEAN.INC
LOCAL END
      SUBROUTINE CLNCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an CLEAN object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'CLEAN.INC'
      INTEGER  IDIM(7), BCOMP(MAXFLD), BMSSZ(2), DUMMY(1), IVALUE(1)
      REAL     RVALUE(1)
      LOGICAL  LVALUE(1)
      CHARACTER CDUMMY*1
      EQUIVALENCE (IVALUE, RVALUE, LVALUE)
      DATA BCOMP /MAXFLD * 0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create AIPS object
      CALL OBCREA (NAME, 'CLEAN   ', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize
      IDIM(1) = 1
      IDIM(2) = 1
      IDIM(3) = 0
C                                       Clean type
      CTYPE = '        '
      IDIM(1) = LEN (CTYPE)
      CALL CLNPUT (NAME, 'CLEANTYP', OOACAR, IDIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default = 1 field
      IDIM(1) = 1
      IVALUE(1) = 1
      CALL CLNPUT (NAME, 'NIMAGE', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default = UV channel 1
      IDIM(1) = 1
      CALL CLNPUT (NAME, 'UVCHAN', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default = grid 1 channel
      IDIM(1) = 1
      CALL CLNPUT (NAME, 'NCHAV', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default CHANNEL = 1
      CALL CLNPUT (NAME, 'CHANNEL', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default VERSION = 0 (new)
      IVALUE(1) = 0
      CALL CLNPUT (NAME, 'VERSION', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default NITER = 200
      IVALUE(1) = 200
      CALL CLNPUT (NAME, 'NITER', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default GAIN = 0.2
      RVALUE(1) = 0.2
      CALL CLNPUT (NAME, 'GAIN', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default MINPATCH = 0
      IVALUE(1) = 0
      CALL CLNPUT (NAME, 'MINPATCH', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default MAXPATCH = 0
      CALL CLNPUT (NAME, 'MAXPATCH', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default PHAT = 0.0
      RVALUE(1) = 0.0
      CALL CLNPUT (NAME, 'PHAT', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default MINFLUX = 0.0
      CALL CLNPUT (NAME, 'MINFLUX', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default FACTOR = 0.0
      CALL CLNPUT (NAME, 'FACTOR', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default NORESTOR = false
      LVALUE(1) = .FALSE.
      CALL CLNPUT (NAME, 'NORESTOR', OOALOG, IDIM, IVALUE, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default DOBEAM = true
      LVALUE(1) = .TRUE.
      CALL CLNPUT (NAME, 'DOBEAM', OOALOG, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default beam size = 0
      RVALUE(1) = 0.0
      CALL CLNPUT (NAME, 'BEAM.BMAJ', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CLNPUT (NAME, 'BEAM.BMIN', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CLNPUT (NAME, 'BEAM.BPA', OOARE, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default MAXNRES = 20,000
      IVALUE(1) = 20000
      CALL CLNPUT (NAME, 'MAXNRES', OOAINT, IDIM, IVALUE, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default residual scaling =
C                                       false.
      LVALUE(1) =.FALSE.
      CALL CLNPUT (NAME, 'SCALERES', OOALOG, IDIM, IVALUE, CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default beam half width = 5
      BMSSZ(1) = 5
      BMSSZ(2) = 5
      IDIM(1) = 2
      CALL CLNPUT (NAME, 'BMSCLSZ', OOAINT, IDIM, BMSSZ, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Default BCOMP = 0
      IDIM(1) = MAXFLD
      CALL CLNPUT (NAME, 'BCOMP', OOAINT, IDIM, BCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CLNDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the CLEAN object with name "name"; quasi-permanent
C   forms are unaffected.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C-----------------------------------------------------------------------
      IERR = 0
C                                       Close
      CALL CLNCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE CLNZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the CLEAN object with name "name"; quasi-permanent
C   forms are deleted.  Only deletes CLEAN image.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'CLEAN.INC'
      INTEGER   DIM(3), TYPE, I, DUMMY(1)
      CHARACTER CDUMMY*1
C-----------------------------------------------------------------------
      IERR = 0
C                                       Close
      CALL CLNCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find number of fields
      CALL CLNGET (NAME, 'NIMAGES', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      MFIELD = DUMMY(1)
C                                       Destroy CLEAN image
      CALL CLNGET (NAME, 'CLEANI', TYPE, DIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 20 I = 1,MFIELD
         CALL IMGZAP (CNAME(I), IERR)
         IF (IERR.NE.0) GO TO 999
 20      CONTINUE
C                                       Free Object slot
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE CLNOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a CLEAN file.  Marks CLEAN common as active. Forces creation
C   of CLEANI objects and underlying structures.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   Not used.
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'CLEAN.INC'
      INTEGER  DIM(7), TYPE, I, BCOMP(MAXFLD), CCROW, NUMCOL, BFIELD,
     *   MTYPE, NCOMPS, CCVER, DUMMY(1), MSGSAV, NITER
      REAL     X, Y, FLUX, PARMS(10)
      CHARACTER CREAD*4, CCTAB*32, STAT*4, CDUMMY*1
      LOGICAL LDUMMY(1)
      EQUIVALENCE (DUMMY, LDUMMY)
      DATA CREAD /'READ'/
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       CLEAN common must be inactive
      IF (ACTIVE) THEN
         IERR = 1
         MSGTXT = 'CLNOPN: ATTEMPT TO ACTIVATE SECOND CLEAN OBJECT'
         GO TO 990
         END IF
      ACTIVE = .TRUE.
C                                       CLEAN type
      CALL CLNGET (NAME, 'CLEANTYP', TYPE, DIM, DUMMY, CTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (CTYPE.EQ.' ') CTYPE = 'IMAGE'
C                                       Find number of fields
      CALL CLNGET (NAME, 'NITER', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      NITER = DUMMY(1)
C                                       Find number of fields
      CALL CLNGET (NAME, 'NIMAGES', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      MFIELD = DUMMY(1)
      MSGSUP = 32000
      CALL CLNGET (NAME, 'NUMRES', TYPE, DIM, DUMMY, CDUMMY, IERR)
      MSGSUP = MSGSAV
      NFIELD = DUMMY(1)
      IF (IERR.EQ.1) THEN
         IERR = 0
         NFIELD = 1
         END IF
      IF (IERR.NE.0) GO TO 995
      NFIELD = MFIELD / MAX (1, NFIELD)
      MSGSUP = 32000
      CALL CLNGET (NAME, 'ONEBEAM', TYPE, DIM, DUMMY, CDUMMY, IERR)
      MSGSUP = MSGSAV
      ONEBEM = LDUMMY(1)
      IF (IERR.EQ.1) THEN
         IERR = 0
         ONEBEM = .FALSE.
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Find number restart components
      CALL CLNGET (NAME, 'BCOMP', TYPE, DIM, BCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       CC version number
      CALL CLNGET (NAME, 'VERSION', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CCVER = DUMMY(1)
C                                       Get object names
C                                       There may not be dirty images
      IF (CTYPE(1:5).EQ.'IMAGE') THEN
         CALL CLNGET (NAME, 'DIRTYI', TYPE, DIM, DUMMY, DNAME, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE IF (CTYPE(1:2).EQ.'UV') THEN
         CALL CLNGET (NAME, 'UVDATA', TYPE, DIM, DUMMY, UVDATA, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
      CALL CLNGET (NAME, 'DIRTBEAM', TYPE, DIM, DUMMY, DBNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      BFIELD = DIM(2)
      CALL CLNGET (NAME, 'CLEANI', TYPE, DIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Force full instantiation (open
C                                       and close all image objects)
C                                       Dirty beam
C                                       Beam may not be created for uv
C                                       clean.
      IF (CTYPE(1:5).EQ.'IMAGE') THEN
         STAT = 'READ'
      ELSE
         STAT = 'WRIT'
         END IF
      DO 10 I = 1,BFIELD
         IF ((.NOT.ONEBEM) .OR. (MOD(I,NFIELD).EQ.1) .OR. (NFIELD.EQ.1))
     *      THEN
            CALL IMGOPN (DBNAME(I), STAT, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL IMGCLO (DBNAME(I), IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Uvdata
            IF (CTYPE(1:2).EQ.'UV') THEN
               CALL IMGOPN (DBNAME(I), CREAD, IERR)
               IF (IERR.NE.0) GO TO 995
               CALL IMGCLO (DBNAME(I), IERR)
               IF (IERR.NE.0) GO TO 995
               END IF
            END IF
 10      CONTINUE
      DO 20 I = 1,MFIELD
C                                       Dirty image
         IF (CTYPE(1:5).EQ.'IMAGE') THEN
            CALL IMGOPN (DNAME(I), CREAD, IERR)
            IF (IERR.NE.0) GO TO 995
            CALL IMGCLO (DNAME(I), IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
         STAT = 'WRIT'
         CALL IMGOPN (CNAME(I), STAT, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMGCLO (CNAME(I), IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Clean component tables
         IF (NITER.GT.0) THEN
            CCTAB = 'Temporary table object for CLNOPN'
            CALL IM2TAB (CNAME(I), CCTAB, 'CC', CCVER, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       what kind of CC file
            MSGSUP = 32000
            CALL OGET (CNAME(I), 'COMPDIAM', TYPE, DIM, X, CDUMMY, IERR)
            MSGSUP = MSGSAV
            IF (IERR.EQ.1) THEN
               IERR = 0
               X = 0.0
               END IF
            IF (IERR.NE.0) GO TO 995
            NUMCOL = 3
            IF (X.GT.0.0) NUMCOL = 7
C                                       Open CLEAN component file -
C                                       force creation.
            CALL OCCINI (CCTAB, 'WRIT', CCROW, NUMCOL, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Get number of existing
C                                       components.
            CALL TABGET (CCTAB, 'NROW', TYPE, DIM, NCOMPS, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
            BCOMP(I) = MIN (BCOMP(I), NCOMPS)
C                                       Get actual table version
            CALL TABGET (CCTAB, 'VER', TYPE, DIM, CCVER, CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Close CC table
            CALL OTABCC (CCTAB, 'CLOS', CCROW, NUMCOL, X, Y, FLUX,
     *         MTYPE, PARMS, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       Delete temporary CC object
            CALL TABDES (CCTAB, IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 20      CONTINUE
C                                       Save CC version number
      DIM(1) = 1
      DIM(2) = 1
      DUMMY(1) = CCVER
      CALL CLNPUT (NAME, 'VERSION', OOAINT, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Reset number restart components
      DIM(1) = MAXFLD
      DIM(2) = 1
      CALL CLNPUT (NAME, 'BCOMP', OOAINT, DIM, BCOMP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  MSGTXT = 'CLNOPN: ERROR OPENING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLNCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes CLEAN updating disk resident information; deletes scratch
C   files.
C   Closes any open CLEAN IMAGE objects. Marks CLEAN common as inactive.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'CLEAN.INC'
      CHARACTER SNAME*32, CDUMMY*1
      INTEGER   BUFNO, DIM(7), TYPE, I, MSGSAV, DUMMY(1)
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       No action if inactive
      IF (.NOT.ACTIVE) GO TO 999
C                                       Close any open CLEAN objects
C                                       Find number of fields
      CALL CLNGET (NAME, 'NIMAGES', TYPE, DIM, DUMMY, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      MFIELD = DUMMY(1)
      CALL CLNGET (NAME, 'CLEANI', TYPE, DIM, DUMMY, CNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      DO 20 I = 1,MFIELD
C                                       See if open (buffer assigned)
         CALL OBINFO (CNAME(I), BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         IF (BUFNO.GT.0) THEN
            CALL IMGCLO (CNAME(I), IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 20      CONTINUE
C                                       Mark CLEAN common as inactive.
      ACTIVE = .FALSE.
C                                       Destroy scratch files
C                                       WORK1
      MSGSUP = 32000
      CALL CLNGET (NAME, 'WORK1', TYPE, DIM, DUMMY, SNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNAME = '    '
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNAME.NE.'    ') CALL IMGZAP (SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      SNAME = '   '
      CALL CLNPUT (NAME, 'WORK1', OOACAR, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       WORK2
      MSGSUP = 32000
      CALL CLNGET (NAME, 'WORK2', TYPE, DIM, DUMMY, SNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNAME = '    '
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNAME.NE.'    ') CALL IMGZAP (SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      SNAME = '   '
      CALL CLNPUT (NAME, 'WORK2', OOACAR, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       TRANFN
      MSGSUP = 32000
      CALL CLNGET (NAME, 'TRANFN', TYPE, DIM, DUMMY, SNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNAME = '    '
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNAME.NE.'    ') CALL IMGZAP (SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      SNAME = '   '
      CALL CLNPUT (NAME, 'TRANFN', OOACAR, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       GRID
      MSGSUP = 32000
      CALL CLNGET (NAME, 'GRID', TYPE, DIM, DUMMY, SNAME, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         SNAME = '    '
         END IF
      IF (IERR.NE.0) GO TO 995
      IF (SNAME.NE.'    ') CALL IMGZAP (SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      SNAME = '   '
      CALL CLNPUT (NAME, 'GRID', OOACAR, DIM, DUMMY, SNAME, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGSUP = MSGSAV
      MSGTXT = 'CLNCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLNGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns the dimensionality and value(s) associated with a given
C   keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C   Outputs:
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL CLBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) THEN
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL  OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLNPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
      IERR = 0
C                                       Is this a base class?
      CALL CLBPUT (NAME, KEYWRD, TYPE, DIM(1), VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
      IF (IERR.EQ.1) THEN
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
C
C   Private functions:
C
      SUBROUTINE CLBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then fetch the
C   value (array) for a specified member
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find., 2=Failed
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 1)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'BEAM'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'CLBGET: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call appropriate access
C                                       function:
      GO TO (110), IMEM
C                                       BEAM
 110     CALL BEMGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE CLBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Private
C   If KEYWRD refers to a recognized member base class then store the
C   value (array) for a specified member
C   Inputs:
C      NAME    C*?   The name of the object.
C      KEYWRD  C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of value, an axis dimension of zero
C                    means that that dimension and higher are
C                    undefined.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM, VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
C
      INTEGER   POINT, NMEMS, IMEM, LOOP, JERR
      PARAMETER (NMEMS = 1)
      CHARACTER MEMBER*16, MEMS(NMEMS)*16
      INCLUDE 'INCS:DMSG.INC'
      DATA MEMS /'BEAM'/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Save member name
      POINT = INDEX (KEYWRD, '.')
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized base
C                                       classes.
      IMEM = -1
      DO 10 LOOP = 1,NMEMS
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       If no base class and member not
C                                       recognized then return.
      IF ((IMEM.LE.0) .AND. (POINT.LE.0)) GO TO 999
C                                       Find it?, if not, complain and
C                                       die.
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'CLBPUT: UNKNOWN MEMBER: ' // MEMBER // ' IN ' // NAME
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call appropriate access
C                                       function:
      GO TO (110),  IMEM
C                                       File name
 110     CALL BEMPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      JERR)
         GO TO 900
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
