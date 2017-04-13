      SUBROUTINE MAKMAP (IFIELD, DISKI, CNOSCI, DISKO, CNOSCO, SCRGRD,
     *   SCRWRK, CHANUV, CHANIM, DOCREA, DOINIT, DOBEAM, DOSEL, DOGCOR,
     *   CHINC, JBUFSZ, BUFFER, IRET)
C-----------------------------------------------------------------------
C! Makes image or beam from uv data set.
C# AP-util AP-fft UV Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-2000, 2006-2009, 2012, 2014-2015
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
C   MAKMAP makes an image or a dirty beam given a uv data set.  The data
C   may either calibrated or uncalibrated (raw) data and calibration
C   and various selection criteria may be (optionally) applied. The
C   weights of the data must be set before entering this routine.
C      The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,XPARM,YPARM.  The defaults for
C   these values are filled in by a call to GRDFLT.  The gridded data
C   are phase rotated so that the map center comes out at location
C   ICNTRX,ICNTRY.  If requested, a uv taper is applied to the
C   visibility weights before gridding.  If necessary, a three dimension
C   phase reference position  shift is done in AP1GRD.
C      If more than one channels are to be gridded together, GR3DUV
C   as many channels at a time as is possible with the available memory.
C   This bandwidth synthesis (BS) process may use the SCRWRK file.
C   For bandwidth synthesis both the CNOSCO and SCRWRK files should be
C   big enough for an extra m rows, where m is the half width of the
C   X convolving function.
C   Zero spacing flux densities are gridded if provided.
C      The final image will be normalized and (optionally) corrected for
C   the effects of the gridding convolution function.
C
C   Input uv data file in UV file CNOSCI, DISKI
C   Output image file in image file CNOSCO, DISKO
C   Inputs:
C      IFIELD      I     Field number to map, if 0 then make a beam.
C      DISKI       I     Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI      I     Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISKO       I     Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCO      I     Output file catalog slot number or /CFILES/
C                        scratch file number.  If DOCREA is FALSE and
C                        DISKO=0 and CNOSCO=0 a scratch file is created.
C      SCRGRD      I     Grid scratch file number, will be set if the
C                        file is created, (DOINIT=TRUE)
C      SCRWRK      I     Work scratch file number, will be set if the
C                        file is created, (DOINIT=TRUE)
C      CHANUV      I     Channel number to grid.  If DOSEL=TRUE
C                        then this is 1-rel wrt the selected data.
C      CHANIM      I     Channel number of output image.
C      DOCREA      L     If TRUE, Create/catalog output image file.
C      DOINIT      L     If TRUE, initialize scratch files, set defaults
C                        for convolving functions.  Should be TRUE on
C                        first call, and FALSE there after.
C      DOBEAM      L     If TRUE, grid the beam before gridding the
C                        field.  See usage notes.
C      DOSEL       L     If true, data need to be reformatted to a
C                        single Stokes' type.  If TRUE, the cataloged
C                        file NAME, CLASS etc should be filled into
C                        UNAME, UCLAS, UDISK, USEQ in common /SELCAL/
C      DOGCOR      L     If TRUE, correct image for gridding convolution
C                        correction function.  (Normally .TRUE.)
C      JBUFSZ      I     Size in bytes of buffers. Dimension of BUFFER
C                        must be at least 4096 R.
C   From commons: (Includes DGDS and DMPR)
C      MFIELD       I    The number of fields which are going to to be
C                        imaged (excluding any beam). MUST be filled in.
C      FLDSZ(2,*)   I    Dimension of map in RA, Dec (cells) of each
C                        field.  MUST be completely filled in before the
C                        DOINIT=TRUE call if the output file (either
C                        image or scratch) is to be created or zeroed
C                        if the files already exist.
C      DOUNIF       L    If TRUE, apply Uniform weighting. Should be
C                        TRUE on only the first call, otherwise it will
C                        be applied again.
C      WTPOWR       R    Power to take weights to.  Set to 1.0 to avoid
C                        application regardless of DOUNIF's value.
C      NCHAVG       I    Number of channels to grid together for
C                        bandwidth synthesis.
C      CTYPX,CTYPY  I    Convolving function types for RA and Dec
C      XPARM(10)    R    Convolving function parameters for RA
C                        XPARM(1) = support half width.
C      YPARM(10)    R    Convolving function parameters for Dec.
C      UVRNG(2)     R    Minimum and maximum baseline lengths in
C                        1000's wavelengths. 0's => all
C      XSHIFT(16)   R    Shift in X (after rotation) in asec.
C                        in projected coordinates. 1 per field.
C      YSHIFT(16)   R    Shift in Y (after rotation) in asec.
C                        in projected coordinates. 1 per field.
C      STOKES       C*4  Stokes types wanted.
C                        'I','Q','U','V','R','L'
C      DOZERO       L    If true then do zero spacing flux.
C      ZEROSP(5)    R    Zero spacing flux, 1=>flux density (Jy)
C                        5 => weight to use.
C                        polarization.
C      TFLUXG       R    The total flux density removed from the data,
C                        this will be subtracted from the zero spacing
C                        flux before gridding.
C      DOTAPE       L    True if taper requested.
C      TAPERU,TAPERV R   TAPER ( to 30%) in u and v (kilolamda)
C                        Non-zero means to do a weighting pass
C   The following must be provided if DUNIF is TRUE.
C      UNFBOX       I    Half width of unif. wt. counting box size.
C      NXUNF,NYUNF  I    Dimension (cells) of the map in RA and Dec
C                        to be used to set uniform weighting.
C      NBXFUN       I    Function used when UNFBOX > 0.
C      ROBUST       R    Robustness parameter in uniform weighting.
C   The following must be provided if DOSEL is FALSE( common /MAPHDR/):
C     CATBLK(256)   I    Catalog header for uv data input file.
C                        (used on DOINIT=TRUE call and weighting)
C   The following must be provided if DOCREA is TRUE (includes DMPR,
C    DGDS)
C      MNAME           C*12 Output image name.
C      MCLASS          C*6  Output image class.
C                           (If more than 1 field the last 2 char
C                           are used to encode the field number)
C      MDISK           I    Desired image file output disk
C      MSEQ            I    Desired image file output sequence no.
C   The following must be provided if the output file is to be created;
C   either by setting DOCREA=TRUE or DISKO=CNOSCO=0.
C      FLDSZ(2,*)      I    Dimension of map in RA, Dec (cells)
C      NXBEM,NYBEM     I    Dimension (cells) of beam.
C      CELLSG(2)       R    The cell spacing in X and Y in arcseconds.
C      XSHIFT(*)       R    Shift in X (after rotation) in asec.
C                           in projected coordinates. 1 per field.
C      YSHIFT(*)       R    Shift in Y (after rotation) in asec.
C                           in projected coordinates. 1 per field.
C      ICNTRX,ICNTRY(*) I   The center pixel in X and Y for each
C                           field. 0 values cause the default.
C   The following must be provided if DOCREA is FALSE and output
C   files already exist. (Includes DGDS).
C      CCDISK(16)   I    Disk numbers of the output images.
C                        (Must be zeroed if not filled in.)
C      CCCNO(16)    I    Catalog slot numbers of output images.
C                        (Must be zeroed if not filled in.)
C   The following must be provided if DOSEL is true.
C   (Includes DSEL.INC)
C      UNAME        C*12 AIPS name of input file.
C      UCLAS        C*6  AIPS class of input file.
C      UDISK        R    AIPS disk of input file.
C      USEQ         R    AIPS sequence of input file.
C      FGVER        I    FLAG file version number, if .le. 0 then
C                        NO flagging is applied.
C      SOURCS(1)   C*16  Name of desired source.
C      TIMRNG(8)    R    Start day, hour, min, sec, end day, hour,
C                        min,sec. 0's => all
C      STOKES       C*4  Stokes types wanted.
C                        'I','Q','U','V','R','L'
C      BCHAN        I    First channel number selected, 1 rel. to first
C                        channel in data base. 0 => all
C      ECHAN        I    Last channel selected. 0=>all
C      BIF          I    First IF number selected, 1 rel. to first
C                        IF in data base. 0 => all
C      EIF          I    Last IF selected. 0=>all
C      FRQSEL       I    FQ id selected.
C      DOCAL        L    If true apply calibration, else not.
C   The following must be provided if DOCAL is TRUE.
C      ANTENS(50)   I    List of antennas selected, 0=>all,
C                        any negative => all except those specified
C      GAUSE        I    GAIN (CL or SN) file version number to use.
C   Output:
C      DISKI       I     UV data file disk if data reformatted.
C      CNOSCI      I     Reformatted uv data scratch file number
C                        to be used in subsequent calls.
C      DISKO       I     Output image file disk number if output file.
C                        created and/or cataloged (DOCREA=TRUE
C                        or input DISKO=0 and CNOSCO=0).
C      CNOSCO      I     Output image file catalog slot number
C                        or scratch file number if output file created.
C      SCRGRD      I     Grid scratch file number, will be set if the
C                        file is created, (DOINIT=TRUE)
C      SCRWRK      I     Work scratch file number, will be set if the
C                        file is created, (DOINIT=TRUE)
C      DOSEL       L     Set to FALSE if data reformatted.
C      DOBEAM      L     Set to DO3DIM.
C      DOINIT      L     Set to FALSE.
C      BUFFER(*)   R     Working buffer
C      IRET        I     Return error code. 0=>OK, error otherwise.
C   Output in Common:
C      DOUNIF       L    Set to FALSE if uniform weighting applied.
C      MNAME       C*12  Output image name. (defaults applied)
C      MCLASS      C*6   Output image class (defaults applied)
C      MDISK       I     Desired image file output disk
C                        (defaults applied)
C      MSEQ        I     Desired image file output sequence no.
C                        (defaults applied)
C      FLDMAX(*)   R     Maximum pixel value in field.
C      FLDMIN(*)   R     Minimum pixel value in field.
C   The following are filled in if a output file is created:
C      CCDISK(16)  I     Disk numbers of the output images.
C      CCCNO(16)   I     Catalog slot numbers of output images.
C   Useage Notes:
C    1) The input uvdata file is, with one exception, assumed to be
C     accurately described by the contents of CATR and the common
C     /UVHDR/ (include DUVH).  The exception is that the u, v and
C     w may refer to a different frequency.  The reference frequency for
C     the u,v and w terms is taken from the input CATBLK in the DOINIT
C     TRUE call unless the data is reformatted (DOSEL=TRUE).
C     In this latter case this frequency is obtained from UVGET call.
C     If DOSEL = TRUE the input value of CATBLK is ignored.
C    2) Information about the output image is obtained from the
C     catalog header for the relevant file.  If MAKMAP makes the
C     output file this information is filled in.  If MAKMAP does not
C     make the output image file then this information must be filled
C     in before hand.  Routine IMCREA will help do this.  Note: even
C     scratch files are cataloged and thus have a catalog header.
C     If MAKMAP does not create the output files, CCDISK(IFIELD) and
C     CCCNO(IFIELD) should give their disk and catalog slot number
C     before the call to MAKMAP.
C    3) only one polarization can be processed and the input data
C     to the gridding routine is assumed to be in the desired Stokes'
C     type (i.e. I, Q, U, V etc.).
C        If DOSEL = TRUE the input data will be selected, calibrated
C     and reformatted as specified in common (include DSEL).
C     Only Stokes' types I,Q,U,V,R,L should be used.
C        Multiple channels may be gridded together a la bandwidth
C     synthesis by specifying NCHAVG > 1. One channel of several
C     channels may be gridded specified by CHANUV.
C    4) If DOSEL=FALSE on the first call (i.e. the data is not
C     reformatted),  the random parameters in the data should include,
C     in order, u, v, w, weight (optional), time (optional) and baseline
C     (optional).  While the last are optional and not used, the last
C     words of random parameters are used as work space and, if they
C     are missing, u, v, and w may be clobbered.  The weights are
C     required but may be passed either as random parameters or as
C     part of the regular data array, CATR should tell which.
C     If DOSEL=TRUE is used these conditions will be satisfied.
C    5) The necessary image normalization constant for proper
C     normalization of the FFTed image is produced only by gridding the
C     beam.  If a beam is to be made, it should be done first; in this
C     case DOBEAM should be FALSE in all calls.  If a beam is not
C     desired then the first call to MAKMAP should have DOBEAM TRUE and
C     FALSE on subsequent calls.  Note MAKMAP sets DOBEAM to FALSE.
C     NOTE WELL: TO GET PROPER BEAM IMAGES CALL MAKMAP WITH IFIELD<0 TO
C     MAKE ALL BEAMS FIRST AND THEN USE DOBEAM FALSE.  DOBEAM TRUE gets
C     the weighting needed but not a proper beam image.
C    6) Much of the control information used by MAKMAP is passed to and
C     stored in commons.  The calling routine should have the following
C     includes:
C     DHDR.INC, DUVH.INC, DFIL.INC, DMPR.INC, DGDS.INC, DSEL.INC
C     NOTE: care should be taken that the contents of these commons
C     not be clobbered by overlaying.
C    7) If calibration is applied then up to 8 map and 3 non map files
C     will be open at once; this should be reflected in the call to
C     ZDCHIN and the dimension of FTAB in the main routine of the
C     calling program.  MAKMAP may use AIPS LUNs 16, 17, 18, 19,
C     20, 21, 22, 23, 24, 25, 28, 29, 30.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, DISKI, CNOSCI, DISKO, CNOSCO, SCRGRD, SCRWRK,
     *   CHANUV, CHANIM, JBUFSZ, IRET, CHINC
      LOGICAL   DOCREA, DOINIT, DOBEAM, DOSEL, DOGCOR
      REAL      BUFFER(*)
C
      CHARACTER PREMAX*5, PREMIN*5, KEYW*8
      INTEGER   J, NP(7), TVOL, TCNO, SAX(7), PLARR(5), NAX, IERR, NIF,
     *   LLUN(3), IBPOIN, KBUFSZ, LBUFSZ, NNX, NNY, KAP, SIZE, IVER,
     *   OVER, LUN1, LUN2, QVER, LUNQ, BUFFQ(256), CATQ(256), JERR, I,
     *   LOCS, KEYTYP, MSGSAV, LFIELD, MBUFSZ, NEED
      LOGICAL   HERM, LERR, T, F, TABLE, FQEXIS, FITASC
      REAL   DUM1, DUM2, XFMAX, XFMIN, YFMAX, YFMIN, CATR(256)
      DOUBLE PRECISION CATD(128), XRA, XDEC, NOISR, SUMWTI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   ISB(MAXIF)
      DOUBLE PRECISION FQOFF(MAXIF), FQQFF
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DAPM.INC'
      REAL FINC(MAXIF), EBUFF(4*MABFSS)
      EQUIVALENCE (CATBLK, CATR, CATD)
      SAVE CATQ, EBUFF
      DATA T, F, HERM /.TRUE.,.FALSE.,.TRUE./
      DATA LLUN /16,17,18/
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = T
      LUNQ = 49
      QVER = 1
      LFIELD = MAX (1, ABS(IFIELD))
C                                       Get u,v,w frequency from old
C                                       CATBLK in case the data is not
C                                       reformatted.
      IF (DOINIT) THEN
C                                       Just to be sure - call VHDRIN
         CALL VHDRIN
         FREQUV = CATD(KDCRV+JLOCF)
         END IF
C                                       Reformat uv data.
      IF (DOSEL) THEN
C                                       Remove the IF table, if it
C                                       exists
         TVOL = DISKI
         TCNO = CNOSCI
         IF ((TVOL.LE.0) .AND. (CNOSCI.LE.0)) GO TO 10
         IF (TVOL.LE.0) THEN
            TVOL = SCRVOL(CNOSCI)
            TCNO = SCRCNO(CNOSCI)
            END IF
         CALL ISTAB ('FQ', TVOL, TCNO, QVER, LUNQ, BUFFQ, TABLE, FQEXIS,
     *      FITASC, JERR)
         IF (FQEXIS) CALL RMEXT (TVOL, TCNO, 'FQ', QVER, CATQ, BUFFQ,
     *      JERR)
C                                       Open input uv file.
 10      DOSEL = F
         CALL UVGET ('INIT', BUFFER, BUFFER(NRPARM+1), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Freq. of u,v,w
         FREQUV = UVFREQ
         CALL CALCOP (DISKI, CNOSCI, BUFFER, JBUFSZ, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy relevant portion of IF
C                                       table.
         IVER = 1
         OVER = 1
         LUN1 = LLUN(1)
         LUN2 = LLUN(2)
         IF (DISKI.LE.0) THEN
            TVOL = SCRVOL(CNOSCI)
            TCNO = SCRCNO(CNOSCI)
         ELSE
            TVOL = DISKI
            TCNO = CNOSCI
            END IF
C                                       Read old IF table
         CALL CHNDAT ('READ', BUFFER, IUDISK, IUCNO, IVER, CATUV, LUN1,
     *      NIF, FQOFF, ISB, FINC, BNDCOD, FRQSEL, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1003) IRET
            GO TO 990
            END IF
C                                       Update info for fixing FREQG(*)
         NIF = EIF - BIF + 1
         FQQFF = FQOFF(BIF)
         DO 20 J = BIF,EIF
            FQOFF(J) = FQOFF(J) - FQQFF
 20         CONTINUE
C                                       Write new FQ table
         CALL CHNDAT ('WRIT', BUFFER, TVOL, TCNO, OVER, CATBLK, LUN1,
     *      NIF, FQOFF(BIF), ISB(BIF), FINC(BIF), BNDCOD(BIF), FRQSEL,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1004) IRET
            GO TO 990
            END IF
C                                       Save CATBLK
         CALL COPY (256, CATBLK, CATQ)
C                                       Copy uv Catalog header
         CALL COPY (256, CATBLK, SCRBLK)
C                                       Init Frequency Table
         CALL FRQTAB (TVOL, TCNO, LUN1, CATBLK, FRQSEL, BUFFER, IRET)
         END IF
C                                       Get max. - min. baselines.
      BLMIN = UVRNG(1)
      BLMAX = UVRNG(2)
C                                       Buffer pointers, sizes etc;
C                                       use POLCAL from UVGET commons
      KBUFSZ = MABFSS * 2 * 2
      IBPOIN = 2 * MABFSS + 1
      LBUFSZ = MIN (KBUFSZ, JBUFSZ)
      MBUFSZ = MIN (2*KBUFSZ, JBUFSZ)
C                                       Initialize scratch files.
      IF (DOINIT) THEN
         CALL SETGRD (SCRGRD, SCRWRK, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Set defaults for convolving fn.
         CALL GRDFLT (CTYPX, CTYPY, XPARM, YPARM)
C                                       Copy uv Catalog header
         CALL COPY (256, CATBLK, SCRBLK)
C                                       Initialize DHDR.INC common
         CALL UVPGET (IERR)
         DOINIT = F
         END IF
C                                       Check  NCHAVG
      NCHAVG = MAX(1, NCHAVG)
C                                       Create output image file.
      IF (DOCREA) THEN
         CALL IMCREA (IFIELD, T, DISKO, CNOSCO, CHINC, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Save in common
         IF (IFIELD.GT.0) THEN
            CCCNO(IFIELD) = CNOSCO
            CCDISK(IFIELD) = DISKO
         ELSE
            CNOBEM(LFIELD) = CNOSCO
            BEMVOL(LFIELD) = DISKO
            END IF
         END IF
C                                       Create output scratch file.
      IF ((DISKO.LE.0).AND.(CNOSCO.LE.0)) THEN
         IF (IFIELD.LE.0) THEN
            NP(1) = NXBEM(LFIELD)
            NP(2) = NYBEM(LFIELD)
         ELSE
            NP(1) = FLDSZ(1,IFIELD)
            NP(2) = FLDSZ(2,IFIELD) + 2.1 + YPARM(1) * 2
            END IF
C                                       Determine file size
         CALL MAPSIZ (2, NP, SIZE)
C                                       Form catalog header.
         CALL IMCREA (IFIELD, F, DISKO, CNOSCO, CHINC, BUFFER, IRET)
C                                       Make GRID (SCRGRD) file.
         CALL SCREAT (SIZE, BUFFER, IRET)
         CNOSCO = NSCR
         IF (IFIELD.GT.0) THEN
            CCCNO(IFIELD) = SCRCNO(NSCR)
            CCDISK(IFIELD) = SCRVOL(NSCR)
         ELSE
            CNOBEM(LFIELD) = SCRCNO(NSCR)
            BEMVOL(LFIELD) = SCRVOL(NSCR)
            END IF
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) MSGTXT ='TOO LITTLE DISK SPACE FOR ' //
     *         'IMAGE SCRATCH FILE'
            IF (IRET.GT.1) MSGTXT ='TROUBLE CREATING IMAGE SCRATCH FILE'
            GO TO 990
            END IF
         END IF
C                                       Set block offsets.
      CALL FILL (5, 1, PLARR)
      PLARR(1) = CHANIM
      NAX = 2
      IF (CHANIM.GT.1) NAX = 3
C                                       Beam block offset.
      IF (IFIELD.LE.0) THEN
         SAX(1) = NXBEM(LFIELD)
         SAX(2) = NYBEM(LFIELD)
         CALL COMOFF (NAX, SAX, PLARR, BOBEM(LFIELD), IERR)
         BOBEM(LFIELD) = BOBEM(LFIELD) + 1
C                                       Map block offset.
      ELSE
         SAX(1) = FLDSZ(1,IFIELD)
         SAX(2) = FLDSZ(2,IFIELD)
         CALL COMOFF (NAX, SAX, PLARR, BORES(IFIELD), IERR)
         BORES(IFIELD) = BORES(IFIELD) + 1
         END IF
C                                       Gridding parameters.
      CALL ROTFND (SCRBLK, UVROT, LERR)
      MAPROT = UVROT
C                                       Image:
      IF (IFIELD.GT.0) THEN
C                                       Field offsets.
         CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD), MAPROT,
     *      XRA, XDEC)
C                                       set shift terms for field cent.
C                                       -NCP projection
         IF (TYPUVD.EQ.1) THEN
            CALL SHINCP (RA, DEC, MAPROT, XRA, XDEC, DXCG(IFIELD),
     *         DYCG(IFIELD), DZCG(IFIELD))
C                                       -SIN projection
         ELSE
            CALL SHISIN (RA, DEC, MAPROT, XRA, XDEC, DXCG(IFIELD),
     *         DYCG(IFIELD), DZCG(IFIELD))
            END IF
C                                       Scaling from lambda to cells
         SCLUG(IFIELD) = 1.0 / (RAD2AS / (FLDSZ(1,IFIELD) *
     *      ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
         SCLVG(IFIELD) = - 1.0 / (RAD2AS / (FLDSZ(2,IFIELD)*CELLSG(2)))
         SCLWG(IFIELD) = 1.0
C                                       cellsize -> FLDVU
         XFLD(IFIELD) = ABS (CELLSG(1)) * FLDSZ(1,IFIELD)
         YFLD(IFIELD) = CELLSG(2) * FLDSZ(2,IFIELD)
C                                       Map center.
         ICNTRX(IFIELD) = FLDSZ(1,IFIELD) / 2
         ICNTRY(IFIELD) = FLDSZ(2,IFIELD) / 2 + 1
C                                       Beam:
      ELSE
C                                       Scaling from lambda to cells
         SCLUG(1) = 1.0 / (RAD2AS / (FLDSZ(1,LFIELD) *
     *      ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
         SCLVG(1) = - 1.0 / (RAD2AS / (FLDSZ(2,LFIELD)*CELLSG(2)))
         SCLWG(1) = 1.0
         END IF
C                                       First channel.
      CHUV1 = CHANUV
C                                       Input UV data NOISE ratio
      IF (DISKI.LE.0) THEN
         TVOL = SCRVOL(CNOSCI)
         TCNO = SCRCNO(CNOSCI)
      ELSE
         TVOL = DISKI
         TCNO = CNOSCI
         END IF
      MSGSAV = MSGSUP
      MSGSUP = 32000
      KEYW = 'WTNOISE'
      I = 1
      CALL CATKEY ('REED', TVOL, TCNO, KEYW, I, LOCS, NOISR, KEYTYP,
     *   BUFFER, IERR)
      IF (IERR.NE.0) NOISR = -1.0D0
      KEYW = 'SUMWTIN'
      I = 1
      CALL CATKEY ('REED', TVOL, TCNO, KEYW, I, LOCS, SUMWTI, KEYTYP,
     *   BUFFER, IERR)
      IF (IERR.NE.0) SUMWTI = -1.0D0
      MSGSUP = MSGSAV
C                                       Grid beam if necessary
      IF ((DOBEAM) .AND. (IFIELD.GT.0)) THEN
         I = -LFIELD
         CALL GR3DUV (I, DISKI, CNOSCI, SCRGRD, SCRBLK, MBUFSZ,
     *      FRQSEL, EBUFF, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 999
         DOBEAM = ONEBEM
         END IF
C                                       Make grid.
      CALL GR3DUV (IFIELD, DISKI, CNOSCI, SCRGRD, SCRBLK, MBUFSZ,
     *   FRQSEL, EBUFF, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       FFT
      IF (IFIELD.LE.0) THEN
         NNX = NXBEM(LFIELD)
         NNY = NYBEM(LFIELD)
      ELSE
         NNX = FLDSZ(1,IFIELD)
         NNY = FLDSZ(2,IFIELD)
         END IF
      NEED = 2 * NNY * (NNX/2 + 1)
      NEED = NEED / 1024
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         IRET = 10
         MSGTXT = 'MAKMAP: DID NOT GET ANY AP MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL DSKFFT (NNX, NNY, -1, HERM, SCRGRD, SCRWRK, SCRGRD,
     *   KBUFSZ, EBUFF, EBUFF(IBPOIN), DUM1, DUM2, IRET)
      CALL QRLSE
      IF (IRET.NE.0) GO TO 999
C                                       gridding correct/histogram
      CALL GRDCOR (IFIELD, DOGCOR, 0, SCRGRD, DISKO, CNOSCO, YFMAX,
     *   YFMIN, LBUFSZ, EBUFF, EBUFF(IBPOIN), BUFFER, IRET)
C                                       Max, min messages.
      IF (IFIELD.GT.0) THEN
         FLDMAX(IFIELD) = YFMAX
         FLDMIN(IFIELD) = YFMIN
         END IF
      XFMAX = YFMAX
      XFMIN = YFMIN
      CALL METSCA (XFMAX, PREMAX, LERR)
      CALL METSCA (XFMIN, PREMIN, LERR)
      IF (IFIELD.LE.0) THEN
         WRITE (MSGTXT,1000) LFIELD, XFMIN, PREMIN, XFMAX, PREMAX
      ELSE
         WRITE (MSGTXT,1001) IFIELD, XFMIN, PREMIN, XFMAX, PREMAX
         END IF
      CALL MSGWRT (4)
C                                       Update CATBLK.
      IRET = 0
      IF (IFIELD.LE.0) THEN
         TVOL = BEMVOL(LFIELD)
         TCNO = CNOBEM(LFIELD)
      ELSE
         TVOL = CCDISK(IFIELD)
         TCNO = CCCNO(IFIELD)
         END IF
      CALL CATIO ('READ', TVOL, TCNO, CATBLK, 'REST', BUFFER, IRET)
      IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1002) IRET, IFIELD
         CALL MSGWRT (6)
         IRET = 0
         GO TO 60
         END IF
C                                       Reset extrema if necessary
      IF ((CHANIM.EQ.1) .OR. (CATBLK(KINAX+2).EQ.1)) THEN
         CATR(KRDMX) = YFMAX
         CATR(KRDMN) = YFMIN
      ELSE
         CATR(KRDMX) = MAX (CATR(KRDMX), YFMAX)
         CATR(KRDMN) = MIN (CATR(KRDMN), YFMIN)
         END IF
C                                       Fix up image header.
      CALL IMHCOR (IFIELD, CHINC)
C                                       Rewrite header
      CALL CATIO ('UPDT', TVOL, TCNO, CATBLK, 'REST', BUFFER, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1050) IRET, IFIELD
         CALL MSGWRT (6)
         IRET = 0
         END IF
C                                       put NOISR in MA header
      IF (NOISR.GE.1.0) THEN
         KEYW = 'WTNOISE'
         I = 1
         LOCS = 1
         KEYTYP = 1
         CALL CATKEY ('WRIT', TVOL, TCNO, KEYW, I, LOCS, NOISR, KEYTYP,
     *      BUFFER, IERR)
         END IF
C                                       put SUMWTIN in MA header
      IF (SUMWTI.GT.0.0) THEN
         KEYW = 'SUMWTIN'
         I = 1
         LOCS = 1
         KEYTYP = 1
         CALL CATKEY ('WRIT', TVOL, TCNO, KEYW, I, LOCS, SUMWTI, KEYTYP,
     *      BUFFER, IERR)
         END IF
C                                       Remove any destroy-on-failure
C                                       flags
 60   DO 90 J=1,NCFILE
         IF ((CNOSCO.EQ.FCNO(J)) .AND. (FVOL(J).EQ.DISKO)) FRW(J) = 1
 90      CONTINUE
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' Beam min = ',F6.1,1X,A5,'Jy, max = ',F6.1,1X,
     *   A5,'Jy')
 1001 FORMAT ('Field',I5,' min = ',F6.1,1X,A5,'Jy',',max = ',F6.1,1X,
     *   A5,'Jy')
 1002 FORMAT ('MAKMAP: ERROR',I3,' READING CATBLK FIELD',I4)
 1003 FORMAT ('MAKMAP: ERROR',I5,' READING OLD IF TABLE')
 1004 FORMAT ('MAKMAP: ERROR',I5,' WRITING NEW IF TABLE')
 1050 FORMAT ('MAKMAP: ERROR',I3,' UPDATING CATBLK FIELD',I4)
      END
