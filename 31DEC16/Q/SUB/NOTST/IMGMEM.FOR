      SUBROUTINE IMGMEM (NFIELD, FIELDS, TAPERS, DISKI, CNOSCI, CHANUV,
     *   CHANIM, CATUVR, DOGCOR, FREQID, CHINC, JBUFSZ, BUFF1, XTREMA,
     *   BEAMS, IRET)
C-----------------------------------------------------------------------
C! Images a uv data set by gridding, FFTing and correcting in memory.
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2006-2008, 2014
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
C   Images a uv data set by gridding, FFTing and correcting in memory.
C   The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,XPARM,YPARM.
C   This routine will image > 1 image at one time so long as those
C   images have the same spectral channels etc.  They may be a mix of
C   beams, convolved beams, and images of different sizes.  There must
C   be sufficient that all of the uv grid (and image) and sufficient
C   work space fit in the "AP" memory.  If NX(i) and NY(i) are the
C   image dimensions, LREC is the number of words in the visibility, and
C   n is the convolving function width (usually 7) then the "AP"  memory
C   required is:
C      (10*LREC) + (2 * N * 100) + 500 + Sum[ (2*NY(i)*(NX(i)/2+1)) ]
C   The gridded data is phase rotated so that the map center comes out
C   at location ICNTRX,ICNTRY.  If requested, a uv taper is applied to
C   the visibility weights before gridding.  If necessary, a three
C   dimension phase reference position  shift is done in Q1GRD.
C      When the gridding is finished the resultant grid is FFTed in
C   place and gridding corrections are applied before writing the image
C   to disk.
C      Zero spacing flux densities are gridded if provided.
C   Uses AIPS LUNs 18
C   Inputs:
C      NFIELD      I     Number of fields in FIELDS
C      FIELDS      I(*)  Field number to grid, if 0 then grid a beam.
C                           < 0 => a beam of field abs (FIELDS(i))
C                                  keep BEMMAX, fit beam
C                           < -100000 => don't keep BEMMAX
C      TAPERS      R(2,*)  U,V taper parameters for FIELDS
C      DISKI       I     Input UV file disk number for catalogd files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI      I     Input UV  file catalog slot number or /CFILES/
C                        scratch file number.
C      CHANUV      I     Channel number to grid.  If DOSEL=TRUE
C                        then this is 1-rel wrt the selected data.
C      CHANIM      I     Channel number of output image.
C      CATUVR(256) R     UV data catalog header record.
C      DOGCOR      L     If true make gridding correction.
C      FREQID      I     Freq ID number, if it exists.
C      JBUFSZ      I     Size in aips bytes of buffers. Dimension of
C                        BUFF1,2,3  must be at least 4096 words
C   From commons: (Includes DGDS, DMPR, DUVH, CGDS, CMPR, CUVH)
C      NVIS         I    Number of visibility records (/UVHDR/)
C      LREC         I    Number of (real) words per visibility record
C                        (/UVHDR/)
C      NCHAVG       I    Number of frequency channels to grid
C                        together.
C      FLDSZ(2,*)   I    Dimension of map in RA, Dec (cells)
C      CELLSG(2)    R    The cell spacing in X and Y in arcseconds.
C      CHUV1        I    First channel number in file to grid
C                        (1 relative)
C      FREQ         D    Reference frequency (Hz) (/UVHDR/)
C      JLOCF        I    0 relative number of the frequency axis,
C                        (/UVHDR/)
C      TFLUXG       R    The total flux density removed from the data,
C                        this will be subtracted from the zero spacing
C                        flux before gridding.
C      CTYPX,CTYPY  I    Convolving function types for RA and Dec
C      XPARM(10)    R    Convolving function parameters for RA
C                        XPARM(1) = support half width.
C      YPARM(10)    R    Convolving function parameters for Dec.
C      BORES(*)     I    Block offset desired in output file for
C                        an image, 1 per field. (1 rel.)
C      BOBEM        I    Block offset desired in output file for
C                        beam. (1 rel.)
C      BLMAX        R    Maximum baseline length allowed in 1000s of
C                        wavelengths.
C      BLMIN        R    Minimum baseline length allowed in 1000s of
C                        wavelengths.
C      DOZERO       L    If true then do zero spacing flux.
C      ZEROSP(5)    R    Zero spacing flux, 1=>flux density (Jy)
C                        5 => weight to use.
C                        polarization.
C      NXBEM,NYBEM  I    The size of the BEAM in pixels.
C      FREQG(*)     D    Frequencies of the channels
C      FREQUV       D    Reference frequency for u,v, and w.
C      NGRDAT       L    If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C   The following must be provided if NGRDAT is .TRUE.
C      XFLD,YFLD(*)    R    Field of view in RA and Dec (arcseconds)
C      DXCG,DYCG,DZCG  R    2*pi*(delta ra, delta dec, and delta z)
C                           to be used in AP1GRD to shift positions.
C                           (u,v and w are in cells). one per field.
C      SCLUG,SCLVG,SCLWG R   Conversion factors for u,v and w from
C                           wavelengths at the reference frequency
C                           to cells. one set per field.
C      ICNTRX,ICNTRY(*) I   The center pixel in X and Y for each
C                           field.
C      MAPROT,UVROT     R   Rotations of image and uv data set
C   The following must be provided:
C      CCDISK(*)  I     Disk numbers of the output images.
C      CCCNO(*)   I     Catalog slot numbers of output images.
C      BEMVOL(*)  I     Disk numbers of the output beams
C      CNOBEM(*)  I     Catalog slot numbers of output beams.
C   Output:
C      BUFF1       R     Working buffer
C      XTREMA      R(2,*)  Min/max of fields 1-NFIELD
C      BEAMS       R(3,*)  Fit beam parameters for regular beams
C      IRET        I     Return error code. 0=>OK, error otherwise.
C   Input/Output via common:
C      BEMMAX      R     Sum of weights = normalization factor
C   Usage Notes:
C    1) The input uvdata file is, with one exception, assumed to be
C     accurately described by the contents of CATUVR and the common
C     /UVHDR/ (includes DUVH, CUVH).  The exception is that the
C     frequencies of the channels are given by the common array FREQG.
C     The u,v, and w are assumes to be given by the common variable
C     FREQUV.
C    3) if NGRDAT is .FALSE. then the properties (e.g. shift) of the
C     desired output image are assumed to be described in the catalog
C     header of the existant file pointed to by CCDISK,CCCNO(IFIELD).
C    4) only one polarization will be processed and the input data is
C     assumed to be in the desired Stokes' type (i.e. I, Q, U, V etc.)
C    5) the random parameters in the data should include, in order,
C     u, v, w, weight (optional), time (optional) and baseline
C     (optional).  While the last are optional and not used, the last
C     words of random parameters are used as work space and, if they
C     are missing, u, v, and w may be clobbered.  The weights are
C     required but may be passed either as random parameters or as
C     part of the regular data array, CATUVR should tell which.
C    6) The necessary image normalization constant for proper
C     normalization of the FFTed image is produced only by a call
C     with IFIELD=0 to grid the sampling function.  Therefore,
C     IMGMEM must be called to grid the sampling function IRREGARDLESS
C     of whether or not a beam will be produced.
C    7) Multiple IFs can be processed using the common frequency table
C     FREQG
C-----------------------------------------------------------------------
      INTEGER   NFIELD, FIELDS(*), DISKI, CNOSCI, CHANUV, CHANIM,
     *   JBUFSZ, FREQID, IRET, CHINC
      LOGICAL   DOGCOR
      REAL      TAPERS(2,*), BUFF1(*), CATUVR(256), XTREMA(2,*),
     *   BEAMS(3,*)
C
      INTEGER   MXIMAG
      PARAMETER (MXIMAG = 1000)
      INTEGER   KAP, NX, NY, TVOL, TCNO, LERR, NAX, IFIELD, LFIELD,
     *   SAX(7), PLARR(7), IERR, LOCS, KEYTYP, I, MSGSAV, DISKO, CNOSCO,
     *   NROWFT(MXIMAG), GRID(MXIMAG)
      REAL      YFMAX, YFMIN, XFMAX, XFMIN, CATR(256), SWTS(MXIMAG)
      CHARACTER PREMAX*5, PREMIN*5, KEYW*8
      DOUBLE PRECISION XRA, XDEC, NOISR, SUMWTI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (CATBLK, CATR)
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = .TRUE.
C                                       Grab AP (already grabbed ?)
C                                       size should be okay here
C                                       set in OUVIMG
      CALL QINIT (0, 0, KAP)
C                                       check anyway
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'IMGMEM: DID NOT GET AP MEMORY NEEDED'
         IRET = 8
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Setup
C                                       Get max. - min. baselines.
      BLMIN = UVRNG(1)
      BLMAX = UVRNG(2)
C                                       Set defaults for convolving fn.
      CALL GRDFLT (CTYPX, CTYPY, XPARM, YPARM)
C                                       Set DUVH.INC common
      CALL COPY (256, CATUVR, CATBLK)
      CALL COPY (256, CATUVR, SCRBLK)
      CALL UVPGET (LERR)
      FREQUV = FREQ
C                                       First channel.
      CHUV1 = CHANUV
C                                       Check  NCHAVG
      NCHAVG = MAX(1, NCHAVG)
C                                       Set block offsets.
      CALL FILL (5, 1, PLARR)
      PLARR(1) = CHANIM
      NAX = 2
      IF (CHANIM.GT.1) NAX = 3
C                                       Gridding parameters.
      CALL ROTFND (CATUVR, UVROT, LERR)
      MAPROT = UVROT
C                                       Loop over images
      DO 100 LFIELD = 1,NFIELD
         IFIELD = FIELDS(LFIELD)
C                                       image
         IF (IFIELD.GT.0) THEN
C                                       Map block offset.
            SAX(1) = FLDSZ(1,IFIELD)
            SAX(2) = FLDSZ(2,IFIELD)
            CALL COMOFF (NAX, SAX, PLARR, BORES(IFIELD), LERR)
            BORES(IFIELD) = BORES(IFIELD) + 1
C                                       Field offsets.
            CALL XYSHFT (RA, DEC, XSHIFT(IFIELD), YSHIFT(IFIELD),
     *         MAPROT, XRA, XDEC)
C                                       set shift terms for field cent.
C                                       -NCP projection
            IF (TYPUVD.EQ.1) THEN
               CALL SHINCP (RA, DEC, MAPROT, XRA, XDEC, DXCG(IFIELD),
     *            DYCG(IFIELD), DZCG(IFIELD))
C                                       -SIN projection
            ELSE
               CALL SHISIN (RA, DEC, MAPROT, XRA, XDEC, DXCG(IFIELD),
     *            DYCG(IFIELD), DZCG(IFIELD))
                END IF
C                                       Scaling from lambda to cells
            SCLUG(IFIELD) = 1.0 / (RAD2AS / (FLDSZ(1,IFIELD) *
     *         ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
            SCLVG(IFIELD) = - 1.0 / (RAD2AS/(FLDSZ(2,IFIELD)*CELLSG(2)))
            SCLWG(IFIELD) = 1.0
C                                       cellsize -> FLDVU
            XFLD(IFIELD) = ABS (CELLSG(1)) * FLDSZ(1,IFIELD)
            YFLD(IFIELD) = CELLSG(2) * FLDSZ(2,IFIELD)
C                                       Map center.
            ICNTRX(IFIELD) = FLDSZ(1,IFIELD) / 2
            ICNTRY(IFIELD) = FLDSZ(2,IFIELD) / 2 + 1
C                                       beam
         ELSE
            IFIELD = MAX (1, -IFIELD)
            IF (IFIELD.GT.1000000) IFIELD = IFIELD - 1000000
C                                       Beam block offset.
            SAX(1) = NXBEM(IFIELD)
            SAX(2) = NYBEM(IFIELD)
            CALL COMOFF (NAX, SAX, PLARR, BOBEM(IFIELD), LERR)
            BOBEM(IFIELD) = BOBEM(IFIELD) + 1
            END IF
 100     CONTINUE
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
     *   BUFF1, IERR)
      IF (IERR.NE.0) NOISR = -1.0D0
      KEYW = 'SUMWTIN'
      I = 1
      CALL CATKEY ('REED', TVOL, TCNO, KEYW, I, LOCS, SUMWTI, KEYTYP,
     *   BUFF1, IERR)
      IF (IERR.NE.0) SUMWTI = -1.0D0
      MSGSUP = MSGSAV
C                                       Grid data.
      CALL GRDMEM (NFIELD, FIELDS, TAPERS, DISKI, CNOSCI, CATUVR,
     *   JBUFSZ, FREQID, BUFF1, GRID, NROWFT, SWTS, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       FFT
      DO 200 LFIELD = 1,NFIELD
         IFIELD = FIELDS(LFIELD)
         IF (IFIELD.GT.0) THEN
            NX = FLDSZ(1,IFIELD)
            NY = FLDSZ(2,IFIELD)
            DISKO = CCDISK(IFIELD)
            CNOSCO = CCCNO(IFIELD)
         ELSE
            IFIELD = MAX (1, ABS (IFIELD))
            IF (IFIELD.GT.1000000) IFIELD = IFIELD - 1000000
            NX = NXBEM(IFIELD)
            NY = NYBEM(IFIELD)
            DISKO = BEMVOL(IFIELD)
            CNOSCO = CNOBEM(IFIELD)
            END IF
         CALL FFTMEM (GRID(LFIELD), NX, NY, NROWFT(LFIELD), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Correct/normalize image
         IF (FIELDS(LFIELD).GT.0) THEN
            CALL GRCMEM (IFIELD, DOGCOR, GRID(LFIELD), BEMMAX(IFIELD),
     *         DISKO, CNOSCO, YFMAX, YFMIN, JBUFSZ, BUFF1, IRET)
         ELSE
            CALL GRCMEM (-IFIELD, DOGCOR, GRID(LFIELD), SWTS(LFIELD),
     *         DISKO, CNOSCO, YFMAX, YFMIN, JBUFSZ, BUFF1, IRET)
            END IF
         IF (IRET.GT.0) GO TO 999
C                                       Max, min messages.
         IF (IFIELD.GT.0) THEN
            FLDMAX(IFIELD) = YFMAX
            FLDMIN(IFIELD) = YFMIN
            END IF
         XFMAX = YFMAX
         XFMIN = YFMIN
         CALL METSCA (XFMAX, PREMAX, LERR)
         CALL METSCA (XFMIN, PREMIN, LERR)
         IF (FIELDS(LFIELD).LE.0) THEN
            WRITE (MSGTXT,1000) IFIELD, XFMIN, PREMIN, XFMAX, PREMAX
         ELSE
            WRITE (MSGTXT,1001) IFIELD, XFMIN, PREMIN, XFMAX, PREMAX
            END IF
         CALL MSGWRT (4)
C                                       fit beam
         IF ((FIELDS(LFIELD).LE.0) .AND. (FIELDS(LFIELD).GT.-1000000))
     *      CALL BEMFIT (IFIELD, JBUFSZ, BUFF1, BEAMS(1,LFIELD), IRET)
C                                       Update CATBLK.
         IRET = 0
         IF (FIELDS(LFIELD).LE.0) THEN
            TVOL = BEMVOL(IFIELD)
            TCNO = CNOBEM(IFIELD)
         ELSE
            TVOL = CCDISK(IFIELD)
            TCNO = CCCNO(IFIELD)
            END IF
         CALL CATIO ('READ', TVOL, TCNO, CATBLK, 'REST', BUFF1, IRET)
         IF ((IRET.GT.0) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1002) IRET, IFIELD
            CALL MSGWRT (6)
            IRET = 0
            GO TO 999
            END IF
C                                       Reset extrema if necessary
         IF ((CHANIM.EQ.1) .OR. (CATBLK(KINAX+2).EQ.1)) THEN
            CATR(KRDMX) = -1.0E20
            CATR(KRDMN) = 1.0E20
            END IF
C                                       Set extrema
         CATR(KRDMN) = MIN (CATR(KRDMN), YFMIN)
         CATR(KRDMX) = MAX (CATR(KRDMX), YFMAX)
         XTREMA(1,LFIELD) = CATR(KRDMN)
         XTREMA(2,LFIELD) = CATR(KRDMX)
C                                       Fix up image header.
         IF (FIELDS(LFIELD).LT.0) THEN
            CALL IMHCOR (-IFIELD, CHINC)
         ELSE
            CALL IMHCOR (IFIELD, CHINC)
            END IF
C                                       Rewrite header
         CALL CATIO ('UPDT', TVOL, TCNO, CATBLK, 'REST', BUFF1, IRET)
         IF ((IRET.GT.0) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1050) IRET, IFIELD
            CALL MSGWRT (6)
            IRET = 0
            END IF
C                                       put NOISR in MA header
         IF (NOISR.GE.1.0D0) THEN
            KEYW = 'WTNOISE'
            I = 1
            LOCS = 1
            KEYTYP = 1
            CALL CATKEY ('WRIT', TVOL, TCNO, KEYW, I, LOCS, NOISR,
     *         KEYTYP, BUFF1, IERR)
            END IF
C                                       put SUMWTIN in MA header
         IF (NOISR.GE.1.0D0) THEN
            KEYW = 'SUMWTIN'
            I = 1
            LOCS = 1
            KEYTYP = 1
            CALL CATKEY ('WRIT', TVOL, TCNO, KEYW, I, LOCS, SUMWTI,
     *         KEYTYP, BUFF1, IERR)
            END IF
 200     CONTINUE
C                                       Release AP
      CALL QRLSE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' Beam min = ',F6.1,1X,A5,'Jy, max = ',F6.1,1X,
     *   A5,'Jy')
 1001 FORMAT ('Field',I5,' min = ',F6.1,1X,A5,'Jy',',max = ',F6.1,1X,A5,
     *   'Jy')
 1002 FORMAT ('IMGMEM: ERROR',I3,' READING CATBLK FIELD',I4)
 1050 FORMAT ('IMGMEM: ERROR',I3,' UPDATING CATBLK FIELD',I4)
      END
