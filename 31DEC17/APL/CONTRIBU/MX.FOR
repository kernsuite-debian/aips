LOCAL INCLUDE 'MX.INC'
C                                       Local include for MX
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Histogram, <= 2 x LBUFSZ
      INTEGER   HBUFSZ
      PARAMETER (HBUFSZ = 16384)
C                                       NOTE: uses PARAMETER in DGDS.INC
      INTEGER   SEQIN, SEQOUT, DISIN, DISOUT, ICHAN, NUMCH, SEQ2, DISK2,
     *   BCHAN, ECHAN, CHINC, MINPCH, WINDOW(4,MAXFLD), WIN1(4,50),
     *   NUMBIN, BMHIS(1026), NBOXES, MAXPIX, LASTIT
      HOLLERITH XNAMEI(3), XCLASI(2), XNAME2(3), XCLAS2(2), XNAMOU(3),
     *   XSTOK, XXNATW, XCMETH
      CHARACTER NAMEIN*12, CLASIN*6, NAME2*12, CLASS2*6, NAMOUT*12,
     *   STOKES*4, XNATWT*4, CMETH*4
      REAL      XSEQ, XDISIN, XBCHAN, XECHAN, XSEQ2, XDIS2, XRCHAN,
     *   XNCAVG, XCHINC, XBIF, XEIF, XDISO, XSEQO, XNFLD, CELLS(2),
     *   XFLDSZ(2,MAXAFL), XSHFT(MAXAFL), YSHFT(MAXAFL), XNBOX,
     *   XBOX(4,50), XTAPEU, XTAPEV, XBLMIN, XBLMAX,  XUNFB, XMNPCH,
     *   XZERO(5), XTYPE, YTYPE, XXPARM(10), YYPARM(10), XSIZE(2), GAIN,
     *   FMIN, BMAJ, BMIN, BPA, PHAT, FACTOR, XDOTV, XBADD(10), XLNLIM,
     *   XBCOMP(MAXAFL), DXCOR(MAXFLD), DYCOR(MAXFLD), MAPLIM,
     *   GAUSAA, GAUSBB, GAUSCC, NORFAC(MAXFLD), GMAX(MAXFLD),
     *   GMIN(MAXFLD),  TVREMX, TVREMN, RESMAX, RESHIS(HBUFSZ),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS), XMAXPX, XGUARD(2),
     *   BMMAX, BMMIN, TVFMAX, TVFMIN
      LOGICAL   OLD(MAXFLD), FINISH, DOREST, FSTCLN, SKPSUB, DOBOX,
     *   UNITWT, VLBIWT
      INTEGER   MODE, PATCH, MAXPCH, NRESKP, CATTV(256), FRQSEL, JBUFSZ,
     *   CATOLD(256), CNOOLD, METHOD, SCRGRD, SCRWRK, RCHAN, APSIZ,
     *   CATSCR(256), CNOSCR, APCLCN(MAXFLD), BIF, EIF, RESNUM, APBEAM,
     *   APRESD, XCOUNT, CLNCNT, CLNSTR, CLNLIM, APCFLD(MAXFLD)
      CHARACTER UVSCNM*48
      COMMON /ADVERB/ XNAMEI, XCLASI, XSEQ, XDISIN, XNAME2, XCLAS2,
     *   XSEQ2, XDIS2, XBCHAN, XECHAN, XRCHAN, XNCAVG, XCHINC, XSTOK,
     *   XBIF, XEIF, XNAMOU, XDISO, XSEQO, CELLS, XSIZE, XNFLD, XFLDSZ,
     *   XSHFT, YSHFT, XNBOX, XBOX, XTAPEU, XTAPEV, XBLMIN, XBLMAX,
     *   XXNATW, XUNFB, XZERO, XTYPE, YTYPE, XXPARM, YYPARM, GAIN, FMIN,
     *   XMNPCH, XLNLIM, XBCOMP, BMAJ, BMIN, BPA, PHAT, FACTOR, XDOTV,
     *   XCMETH, XGUARD, XMAXPX, XBADD
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, JBUFSZ
      COMMON /MXCHR/ UVSCNM, NAMEIN, CLASIN, NAME2, CLASS2, NAMOUT,
     *   STOKES, XNATWT, CMETH
      COMMON /MXCOM/  CATOLD, CATTV, CATSCR, DXCOR, DYCOR,
     *   MAPLIM, GAUSAA, GAUSBB, GAUSCC, NORFAC, GMAX, GMIN, TVREMX,
     *   TVREMN, RESMAX, RESHIS, APSIZ, XCOUNT, CLNCNT, CLNLIM, CLNSTR,
     *   APCFLD, RESNUM, APBEAM, APRESD, OLD, FINISH, DOREST, FSTCLN,
     *   SKPSUB, DOBOX, UNITWT, VLBIWT, MODE, SEQIN, SEQOUT, DISIN,
     *   DISOUT, DISK2, SEQ2, ICHAN, NUMCH, METHOD, NRESKP, CNOOLD,
     *   CNOSCR, BIF, EIF, FRQSEL, SCRGRD, SCRWRK, BCHAN, ECHAN, RCHAN,
     *   CHINC, MINPCH, MAXPCH, PATCH, APCLCN, WINDOW, WIN1, NUMBIN,
     *   BMHIS, NBOXES, MAXPIX, LASTIT, BMMAX, BMMIN, TVFMAX, TVFMIN
C                                                          End MX.
LOCAL END
      PROGRAM MX
C-----------------------------------------------------------------------
C! Images and deconvolves (CLEAN) uv data.
C# UV Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2002, 2005-2008
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
C   MX is an implementation of the "MIRVed battery powered CLEAN".
C   A number of sub fields can be handled simultaneously; the fields
C   are individually mapped and a list of CLEAN components derived by
C   the method of CLARK used in APCLN using all of the sub fields.
C   the components found are then subtracted from the ungridded uv data
C   and residual maps made of the fields etc.  This should allow
C   "full field" CLEANing so only the regions of interest need be
C   mapped.  Output images are floating point pixels.
C   Adverbs:
C      Adverb name  Prgm. name       Description / Default
C      INNAME       NAMEIN       Input uv data file name
C      INCLASS      CLASIN       Input uv data file class.
C      INSEQ        SEQIN        Input uv data file sequence no.
C      INDISK       DISIN        Input uv data file disk no.
C      IN2NAME      NAME2        Intermediate uv data file name
C      IN2CLASS     CLASS2       Intermediate uv data file class
C      IN2SEQ       SEQ2         Intermediate uv data file seq
C      IN2DISK      DISK2        Intermediate uv data file disk
C      BCHAN        BCHAN        First channel number, (def=1)
C      ECHAN        ECHAN        Highest channel number to image.
C      CHANNEL      RCHAN        Restart with channel CHANNEL
C      NPOINT       NCHAVG       Number of channels to grid at a time
C                                for wideband synthesis.
C      CHINC        CHINC        Channel increment for processing
C      STOKES       STOKES       Stokes parameter to be processed.
C                                Default = 'I'
C      BIF          BIF          First IF in average.
C      EIF          EIF          Highest IF in average.
C  Output file definition.
C      OUTNAME      NAMOUT       Output file name(s). There will be one
C                                output image for each subfield with
C                                class 'qCLNnn' where n=0 to 63.
C                                and q=I,Q,U,V,R or L as necessary.
C                                Default is INNAME
C      OUTDISK      DISOUT       Disk drive for output. default is
C                                Highest number with enough room.
C      OUTSEQ       OUTSEQ       standard default.
C  Mapping specifications
C      CELLSIZE       CELLS      The cellsize in aseconds.
C      IMSIZE         NXMIN,NYMIN Minimum image size
C      NFIELD         MFIELD     The number of sub fields to image.
C      FLDSIZE(2,64)  FLDSZ      The requested windows in the sub-
C                                fields
C      RASHIFT(64)    XSHIFT      The shift in RA of each sub field.
C      DECSHIFT(64)   YSHIFT      The shift in DEC.
C      NBOXES         NBOXES      Number of CLEAN boxes specified
C                                 for Field 1. 0=>ignore
C      BOXES(4,50)    WIN1(4,50)  BLC, TRC for boxes for field 1.
C      UVTAPER        TAPERU,TAPERV,ITAP
C                                The tapering function
C      UVRANGE        BLMIN,BLMAX Maximum and minimum projected
C                                baseline lengths.
C      UVWTFN         NATWT      Weighting function desired.
C                                Default is 'UN'
C      UVBOX          UNFBOX     Size of the uniform weighting box.
C      ZEROSP         ZEROSP(5)  Zero spacing flux density and
C                                weight.
C      XTYPE          CTYPX      Convolving function type in X
C                                default is  5=Spheriodal
C      YTYPE          CTYPY      Convolving function in Y.
C                                default = XTYPE
C      XPARM          XPARM      Convolving function parameters for X.
C      YPARM          YPARM                  "                  for Y.
C      GUARD          XGUARD     No points in guard fraction of edge of
C                                UV plane
C  CLEANing information.
C      GAIN           GAIN        CLEAN loop gain
C      FLUX           FMIN        Minimum CLEAN component(Jy)
C      MINPCH         MINPCH      Minimum beam patch size.
C      NITER          CLNLIM      Maximum number of CLEAN components
C      BCOMP          CLNSTR      Restart using first CLNSTR comps.
C                                 BCOMP gives the number per field.
C      BMAJ           BMAJ        Major axis of CLEAN restoring beam
C                                 sigma of Gaussian in asec.
C      BMIN           BMIN        Minor axis size of CLEAN beam.
C      BPA            BPA         Position angle (deg) of BMAJ
C      PHAT           PHAT        Prussian hat for dirty Beam.
C      FACTOR         FACTOR      Speed up factor
C      MAXPIXEL       MAXPIX      Maximum pixels searched in each major
C                                 cycle
C  Miscellaneous
C      DOTV           TVFLD        If .gt. 0 show field DOTV-1
C      CMETHOD        METHOD      'DFT'=>dft,'GRID'=gridded sub.
C                                 other = chose the fastest
C      BADDISK(10)    IBAD        Disk drives to avoid for scratch
C                                 files.
C   MXIN sets the AP size and the rest of the task lives with it
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, TVPASS, LUN
      LOGICAL   DOSUM, T
      DOUBLE PRECISION FREQIF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      DATA PRGM /'MX    '/
      DATA T /.TRUE./, LUN/18/
C-----------------------------------------------------------------------
C                                       Free AP memory
      CALL QRLSE
      TVPASS = 0
C                                       Get input Adverbs.
      CALL MXIN (PRGM, FREQIF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       No 3D imaging
      DO3DIM = .FALSE.
C                                       Initialize, create output
C                                       scratch files etc.
      CALL MXINIT (FREQIF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Begin frequency loop
      DO 100 ICHAN = RCHAN,ECHAN,CHINC
C                                       Select data.
         CALL MXSEL (ICHAN, IRET)
         IF (IRET.NE.0) GO TO 990
         DOSUM = CLNSTR.GT.0
C                                       Uniform weight if requested.
         CHUV1 = 1
         NGRDAT = T
         IF (DOUNIF .AND. (.NOT.SKPSUB)) THEN
C                                       Fill Frequency table
            CALL FRQTAB (DISK2, CNOSCR, LUN, CATSCR, FRQSEL, BUFF1,IRET)
C                                       Choose Type of Weighting
            IF (ISORT.EQ.'XY') THEN
               CALL UVUNIF (DISK2, CNOSCR, DISK2, CNOSCR, SCRWRK,
     *            CATSCR, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
            ELSE
               CALL UVTBUN (DISK2, CNOSCR, DISK2, CNOSCR,
     *            CATSCR, JBUFSZ, BUFF1, BUFF2, IRET)
               END IF
            IF (IRET.NE.0) GO TO 990
            END IF
C                                       Do beam.
         CALL MXBEAM (IRET)
         IF (IRET.NE.0) GO TO 990
         FINISH = CLNLIM.LE.0
C                                       If starting at CLEAN beginning
C                                       or restoring with new beam
         IF (CLNSTR.EQ.0) THEN
C                                       Map subfields.
            CALL MXMAP (IRET)
            IF (IRET.NE.0) GO TO 990
C                                       If only IMAP, jump to display
            IF (FINISH) GO TO 70
            END IF
C                                       Check for restore only.
         FINISH = FINISH .OR. (CLNSTR.GE.CLNLIM)
C                                       restart?
         IF (CLNSTR.GT.0) GO TO 50
C                                       Start Major CLEANing loop
C                                       Residual display if desired.
 30         CALL TOUCH
            IF (TVFLD.GT.0) CALL MXDISP (TVPASS)
            FINISH = FINISH .OR. (TVPASS.EQ.32700)
            IF (FINISH) GO TO 70
C                                       Check TELL file
            TVPASS = 2
            CALL MXTELL (TVFLD, TVPASS, IRET)
            IF (IRET.NE.0) GO TO 990
            IF (FINISH) GO TO 70
C                                       CLEAN.
            CALL MXCLN (IRET)
            IF (IRET.NE.0) GO TO 990
            DOSUM = .FALSE.
C                                       Restart here
C                                       Subtract components.
 50         CALL TOUCH
            CALL MXSUB (DOSUM, IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Map subfields.
            CALL MXTELL (TVFLD, TVPASS, IRET)
            IF (IRET.NE.0) GO TO 990
            CALL MXMAP (IRET)
            IF (IRET.NE.0) GO TO 990
C                                       If not finished, loop
            IF (.NOT.FINISH) GO TO 30
C                                       Finished, created restored
C                                       maps.
 70         IF (DOREST) CALL MXREST (IRET)
            IF (IRET.NE.0) GO TO 990
C                                       Restored display if desired.
            TVPASS = 3
            TVREMX = FLDMAX(MAX(1,TVFLD))
            TVREMN = FLDMIN(MAX(1,TVFLD))
            IF (TVFLD.GT.0) CALL MXDISP (TVPASS)
C                                       Print stats for channel.
            CALL MXSTAT
 100     CONTINUE
C                                       Clean up.
      CALL MXOUT (IRET)
C                                       All done.
 990  CALL DIE (IRET, BUFF1)
C
      STOP
      END
      SUBROUTINE MXIN (PRGM, FREQIF, IRET)
C-----------------------------------------------------------------------
C   MXIN gets the input parameters, does some checks and sets
C   some default values.
C   Output:
C     FREQIF  D    Frequency Offset for IF
C     IRET    I    error code:  0=>OK,   8=>bad input parameter.
C    Definitions of MODE
C      1 = I
C      2 = Q
C      3 = U
C      4 = V
C      5 = RCP
C      6 = LCP
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQIF
C-----------------------------------------------------------------------
C                                       Degrees and arc sec to radians
      INCLUDE 'INCS:PSTD.INC'
      CHARACTER PRGM*6, BLANK*6, CLAOUT*6, RSTOKE(6)*4, UTYPE*2
      INTEGER   I, IL, IK, J, IT1, IRET, IF, FIND,IER, NPARM, IX, IY,
     *   NX, NY, IP2, IADDX, IADDY, MAXBEM, MXMIN, MYMIN, IROUND, NIF,
     *   IIVER, NEED, KAP
      REAL      OLDR(256), TEMP, ARG
      DOUBLE PRECISION XDEC, XRA
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE    PRECISION FOFF(MAXIF)
      INTEGER   ISBAND(MAXIF)
      REAL      FINC(MAXIF)
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATOLD, OLDR)
      DATA RSTOKE /'I   ','Q   ','U   ','V   ','R   ','L   '/
      DATA BLANK /'      '/
C-----------------------------------------------------------------------
C                                       system commons
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Initialize common
      IRET = 0
      NCFILE = 0
      NSCR = 0
      FRQSEL = -1
      NUMBIN = HBUFSZ - 10
      RESMAX = 0.0
      CLNSTR = 0
      JBUFSZ = 2  * UVBFSS
      TEMP = -1.0E-20
      CALL RFILL (MAXFLD, TEMP, FLDMAX)
      BMMAX = TEMP
      TEMP = 1.0E20
      CALL RFILL (MAXFLD, TEMP, FLDMIN)
      BMMIN = TEMP
C                                       file common
      CALL FILL (20, 0, SCRVOL)
      LUNS(1) = 16
      LUNS(2) = 18
      LUNS(3) = 19
      LUNS(4) = 20
      LUNS(5) = 21
      LUNS(6) = 22
C                                       get task parms
      NPARM = 290 + 5 * MAXAFL
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
 20   IF (RQUICK) CALL RELPOP (IRET, BUFF1, IER)
      IF (IRET.NE.0) GO TO 990
C                                       Will be used in MXSEL
      SEQIN = IROUND (XSEQ)
      SEQ2 = IROUND (XSEQ2)
      DISIN = IROUND (XDISIN)
      DISK2 = IROUND (XDIS2)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLASS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XXNATW, XNATWT)
      CALL H2CHR (4, 1, XCMETH, CMETH)
C                                       Get catalog block,
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISIN, NAMEIN, CLASIN, SEQIN, UTYPE, NLUSER,
     *   LUNS(1), FIND, CNOOLD, CATOLD, BUFF1, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
C                                        Update CFIL.INC
 40   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISIN
      FCNO(NCFILE) = CNOOLD
      FRW(NCFILE) = 0
C                                       Close file
      CALL ZCLOSE (LUNS(1), FIND, IER)
C                                       Get info.
      CALL COPY (256, CATOLD, CATBLK)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check UV data type
      IF (TYPUVD.GT.0) THEN
C                                       Not interferometer data.
         IRET = 5
         MSGTXT = 'ERROR: DATA IS NOT INTERFEROMETER TYPE DATA'
         GO TO 990
         END IF
C                                       Save ref. freq. for u,v,w
      FREQUV = FREQ
C                                       interprete input parms
C                                       Get parameters.
      DISOUT = IROUND (XDISO)
      SEQOUT = IROUND (XSEQO)
C                                       Number of components for restart
C                                       Summed from BCOMP later.
      CLNSTR = 0
C                                       Total number of components.
      CLNLIM = XLNLIM + 0.001
      LASTIT = 0.1 * CLNLIM + 0.91
C                                       Default NAMEOUT = NAMEIN
      CALL MAKOUT (NAMEIN, CLASIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
C                                       Default is I pol.
      MODE = 1
C                                       Decode Stokes parms.
      DO 50 I = 1,6
         IF (RSTOKE(I)(1:1).EQ.STOKES(1:1)) MODE = I
 50      CONTINUE
      STOKES = RSTOKE(MODE)
C                                       Start channel # (spectral line)
      BCHAN = MAX( IROUND (XBCHAN), 1)
C                                       Restart at channel RCHAN
      RCHAN = BCHAN
      IF (XRCHAN.GT.XBCHAN) RCHAN = IROUND (XRCHAN)
C                                       Default high chann=RCHAN
      ECHAN = RCHAN
      IF (XECHAN.GT.XRCHAN) ECHAN = IROUND (XECHAN)
C                                       Channel increment
      CHINC = 1
      IF (XCHINC.GT.0.6) CHINC = IROUND (XCHINC)
C                                       Number of channels to grid/
C                                       process at a time.
      NCHAVG = 1
      IF (XNCAVG.GT.0.6) NCHAVG = IROUND (XNCAVG)
C                                       Check that request compatable
C                                       with data.
      IRET = 0
      IF (ECHAN.GT.CATOLD(KINAX+JLOCF)) THEN
         IRET = 9
         WRITE (MSGTXT,1060) CATOLD(KINAX+JLOCF), ECHAN
         GO TO 990
         END IF
C                                       Check that request compatable
C                                       with data.
      IF (NCHAVG.GT.(CATOLD(KINAX+JLOCF)-BCHAN+1)) THEN
         IRET = 9
         WRITE (MSGTXT,1060) CATOLD(KINAX+JLOCF), NCHAVG+BCHAN-1
         GO TO 990
         END IF
C                                       Number of output channels.
      NUMCH = ((ECHAN - BCHAN + 1 - NCHAVG) / CHINC) + 1
      NUMCH = MAX (NUMCH, 1)
      IF (NUMCH.EQ.1) CHINC = NCHAVG
C                                       Check IFs present
      IF (JLOCIF.LT.0) THEN
C                                       No IFs, set to safe values
         BIF = 1
         EIF = 1
         FREQIF = 0.0
      ELSE
C                                       else get IF range
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
C                                       limit IF to valid range.
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
C                                       Get Frequency of first IF used
         IIVER = 1
C                                       Read IF frequency table
         CALL CHNDAT ('READ', BUFF1, DISIN, CNOOLD, IIVER, CATOLD,
     *       LUNS(1), NIF, FOFF, ISBAND, FINC, FRQSEL, IRET)
C                                       Freq offset for first IF used
         FREQIF = (FOFF(BIF) + FOFF(EIF))/2.0D0
         END IF
C                                       Get  modeling method
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Copy shift info
      CALL RCOPY (MAXAFL, XSHFT, XSHIFT)
      CALL RCOPY (MAXAFL, YSHFT, YSHIFT)
C                                       Last channel number.
      ECHAN = BCHAN + NUMCH * CHINC - 1
C                                       Get header rotation.
      CALL ROTFND (OLDR, MAPROT, IRET)
      UVROT = MAPROT
C                                       Field sizes
      MFIELD = IROUND (XNFLD)
      MFIELD = MAX (1, MIN (MAXAFL, MFIELD))
      XNFLD = MFIELD
      NXMAX = 0
      NYMAX = 0
      NXUNF = IROUND (XSIZE(1))
      NYUNF = IROUND (XSIZE(2))
      MXMIN = 32000
      MYMIN = 32000
C                                       Initialize over sampling factors
      OSFX = 2.0
      OSFY = 2.0
      NEED = 0
      DO 100 IF = 1,MFIELD
         FLDSZ(1,IF) = IROUND (XFLDSZ(1,IF))
         FLDSZ(2,IF) = IROUND (XFLDSZ(2,IF))
         IF (FLDSZ(1,IF).LE.0) FLDSZ(1,IF) = NXUNF - 10
         IF (FLDSZ(2,IF).LE.0) FLDSZ(2,IF) = NYUNF - 10
C                                       Windows
         WINDOW(1,IF) = 1
         WINDOW(2,IF) = 1
         WINDOW(3,IF) = FLDSZ(1,IF)
         WINDOW(4,IF) = FLDSZ(2,IF)
C                                       Get next higher power of 2
         ARG = FLDSZ(1,IF)
         IP2 = LOG (ARG) / LOG (2.0) + 0.9999
C                                       If cleaning, double size.
         IF (CLNLIM.GT.0) THEN
C                                       If image < 128, double size
            IF (IP2.LT.7) IP2 = IP2 + 1
            END IF
         FLDSZ(1,IF) = 2 ** IP2
         FLDSZ(1,IF) = MAX (FLDSZ(1,IF), NXUNF)
         ARG = FLDSZ(2,IF)
         IP2 = LOG (ARG) / LOG (2.0) + 0.9999
C                                       If cleaning, double size.
         IF (CLNLIM.GT.0) THEN
C                                       If image < 128, double size
            IF (IP2.LT.7) IP2 = IP2 + 1
            END IF
         FLDSZ(2,IF) = 2 ** IP2
         FLDSZ(2,IF) = MAX (FLDSZ(2,IF), NYUNF)
C                                       Check that legal values.
         NX = FLDSZ(1,IF)
         NY = FLDSZ(2,IF)
C                                       Check interpolation over
C                                       sampling factor
         IF (NX.GT.2048) OSFX = 1.0
         IF (NY.GT.2048) OSFY = 1.0
C                                       Center window
         IADDX = (NX - WINDOW(3,IF) + WINDOW(1,IF) - 1) / 2
         IADDY = (NY - WINDOW(4,IF) + WINDOW(2,IF) - 1) / 2
         WINDOW(1,IF) = WINDOW(1,IF) + IADDX
         WINDOW(2,IF) = WINDOW(2,IF) + IADDY
         WINDOW(3,IF) = WINDOW(3,IF) + IADDX
         WINDOW(4,IF) = WINDOW(4,IF) + IADDY
C                                       Don't allow Cleaning within 3
C                                       cells of the edge
         WINDOW(1,IF) = MAX (4, WINDOW(1,IF))
         WINDOW(2,IF) = MAX (4, WINDOW(2,IF))
         IT1 = NX - 3
         WINDOW(3,IF) = MIN (IT1, WINDOW(3,IF))
         IT1 = NY - 3
         WINDOW(4,IF) = MIN (IT1, WINDOW(4,IF))
C                                       Check if (NX,NY) is valid.
         CALL POWER2 (NX, IX)
         CALL POWER2 (NY, IY)
C                                       Invalid (NX,NY)
         IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG) .OR.
     *      (NY.NE.IY) .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) THEN
            WRITE (MSGTXT,1070) NX, NY
            IRET = 8
            GO TO 990
            END IF
         NEED = MAX (NEED, NY * (NX/2 + 1))
C                                       Flip sign of RA cellsize=>
C                                       the increment.
         CELLSG(1) = CELLS(1)
         CELLSG(2) = CELLS(2)
         CELLSG(1) = -ABS (CELLSG(1))
C                                       cellsize -> FLDVU
         XFLD(IF) = ABS (CELLSG(1)) * NX
         YFLD(IF) = CELLSG(2) * NY
         IF ((XFLD(IF).LE.0.0) .OR. (YFLD(IF).LE.0.0)) THEN
            MSGTXT = 'MXIN: FIELD OF VIEW IS <= ZERO'
            IRET = 8
            GO TO 990
            END IF
C                                       Map center.
         ICNTRX(IF) = NX / 2
         ICNTRY(IF) = NY / 2 + 1
C                                       Set for shift
         CALL XYSHFT (RA, DEC, XSHIFT(IF), YSHIFT(IF), MAPROT, XRA,
     *      XDEC)
         XPOFF(IF) = XRA - RA
         YPOFF(IF) = XDEC - DEC
C                                       set shift terms for field cent.
C                                       -NCP projection
         IF (TYPUVD.EQ.-1) THEN
            CALL SHINCP (RA, DEC, MAPROT, XRA, XDEC, DXCG(IF), DYCG(IF),
     *         DZCG(IF))
C                                       -SIN projection
         ELSE
            CALL SHISIN (RA, DEC, MAPROT, XRA, XDEC, DXCG(IF), DYCG(IF),
     *         DZCG(IF))
            END IF
C                                       Field offsets.
         CALL SHFCRP (TYPUVD, RA, DEC, MAPROT, XRA, XDEC, DXCOR(IF),
     *      DYCOR(IF))
         DXCOR(IF) = DXCOR(IF) * NX /  XFLD(IF)
         DYCOR(IF) = DYCOR(IF) * NY /  YFLD(IF)
C                                       Conversion to cells done in
C                                       gridding routine in AP.
         SCLUG(IF) = 1.0 / (RAD2AS / (NX * ABS (CELLSG(1))))
C                                       Flip sign on v to make maps come
C                                       out upside down.
         SCLVG(IF) = - 1.0 / (RAD2AS / (NY * CELLSG(2)))
         SCLWG(IF) = 1.0
C                                       Find max/min field size.
         NXMAX = MAX (NX, NXMAX)
         NYMAX = MAX (NY, NYMAX)
         MXMIN = MIN (NX, MXMIN)
         MYMIN = MIN (NY, MYMIN)
C                                       Get min. grid. SCLUG,SCLVG
         IF (NX.LE.MXMIN) SCLUM = SCLUG(IF)
         IF (NY.LE.MYMIN) SCLVM = SCLVG(IF)
C                                       Initialize gaussian normal-
C                                       zation factor for restoring
C                                       beam.
         NORFAC(IF) = -1.0E20
C                                       Initialize max/min
         GMAX(IF) = -1.0E20
         GMIN(IF) = 1.0E20
 100     CONTINUE
C                                       set AP size
      NEED = (4 * NEED) / 1024
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = 0
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = NEED / 4
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'MXIN UNABLE TO GET ENOUGH AP MEMORY'
            IRET = 8
            GO TO 990
            END IF
         END IF
      CALL QCLNSZ (APSIZ)
      CALL QRLSE
C                                       Minimum beam patch.
      MINPCH = IROUND (XMNPCH)
      MAXPIX = IROUND (XMAXPX)
C                                       Default if below min possible
      IF (MINPCH.LT.3) MINPCH = 127
C                                       Max PATCH fitting in AP size.
      MAXPCH = SQRT ((APSIZ - 1026.0) / 2.0) - 1
      MAXPCH = MIN (1024, MAXPCH)
C                                       Set Max Clean beam size
      MAXBEM = 4 * (MAXPCH + 1)
      MAXBEM = 2 ** INT( (LOG(1.0*MAXBEM) / LOG(2.0)) + 0.999 )
C                                       If cleaning, use 512 beam max
      MAXBEM = MIN (MAXBEM, 512)
C                                       Make Min less than Max
      MINPCH = MIN (MINPCH, MAXPCH)
C                                       TV
      TVFLD = IROUND (XDOTV)
      IF (TVFLD.GT.MFIELD) TVFLD = 1
      IF (NTVDEV.LE.0) TVFLD = 0
      TVFMAX = 0.0
      TVFMIN = 0.0
C                                       Beam size.
      NXBEM(1) = MIN (NXMAX, MAXBEM)
      NYBEM(1) = MIN (NYMAX, MAXBEM)
C                                       If no CLEAN make beam the
C                                       same as the largest map.
      IF (CLNLIM.LT.0.1) NXBEM(1) = NXMAX
      IF (CLNLIM.LT.0.1) NYBEM(1) = NYMAX
      NXUNF = MXMIN
      NYUNF = MYMIN
C                                       Taper.
      TAPERU = MAX (0.0, XTAPEU)
      TAPERV = MAX (0.0, XTAPEV)
      DOTAPE = ((TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0))
C                                       Check defaults for BLLIM
      BLMIN = MAX (0.0, XBLMIN)
      BLMAX = XBLMAX
      IF (BLMAX.LE.0.0) BLMAX = 1.0E15
C                                       Zero spacing flux density
      CALL RCOPY (5, XZERO, ZEROSP)
      DOZERO = (ZEROSP(1).NE.0.0) .AND. (ZEROSP(5).GT.0.0)
C                                       Force no Gauss option
      CALL RFILL (3, 0.0, ZEROSP(2))
C                                       Convolution types
      CTYPX = IROUND (XTYPE)
      CTYPY = IROUND (YTYPE)
C                                       Weighting information.
      CALL RCOPY (10, XXPARM, XPARM)
      CALL RCOPY (10, YYPARM, YPARM)
      UNFBOX = IROUND (XUNFB)
C                                       Uniform or natural Weight
      DOUNIF = XNATWT(1:1) .NE. 'N'
C                                       Weights set to Unity
      UNITWT = XNATWT(2:2) .EQ. 'O' .OR. XNATWT(1:1) .EQ. 'O'
C                                       Take the Forth Root of Weight
      VLBIWT = XNATWT(2:2) .EQ. 'V' .OR. XNATWT(1:1) .EQ. 'V'
C                                       Tell user
      IF (UNITWT) THEN
         MSGTXT = 'Resetting Visibility weights to One'
         CALL MSGWRT(5)
         END IF
      IF (VLBIWT) THEN
         MSGTXT = 'Resetting Weights to Fourth Root of Input Weight'
         CALL MSGWRT(5)
         END IF
C                                       Restoring wanted.
      DOREST = (BMAJ .GT. -1.0E-10) .AND. (CLNLIM .GT. 0.1)
C                                       Default GAIN
      IF (GAIN.LE.1.0E-5) GAIN = 0.1
C                                       Minimum Prussian hat=0.0
      IF (PHAT.LT.0.0) PHAT = 0.0
C                                       Bad disks.
      DO 110 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 110     CONTINUE
C                                       Check sort order.
      IF (ISORT(1:1) .NE. 'X') THEN
         WRITE (MSGTXT,1110) ISORT(1:2), 'X*'
         CALL MSGWRT(5)
         END IF
C                                       Check boxes for field 1.
      NBOXES = IROUND (XNBOX)
      IF (NBOXES.LE.0) GO TO 170
         IL = 1
         DO 160 I = 1,50
            IK = 0
C                                       If no values are specified
C                                       ignore.
            DO 150 J = 1,4
               WIN1(J,IL) = IROUND (XBOX(J,I))
               IF (WIN1(J,IL).GT.0) IK = IK + 1
 150           CONTINUE
C                                       Check bounds
            IF (WIN1(1,IL).LT.0) WIN1(1,IL) = 5
            IF (WIN1(2,IL).LT.0) WIN1(2,IL) = 5
            IF (WIN1(3,IL).GT.FLDSZ(1,1)) WIN1(3,IL) = FLDSZ(1,1) - 5
            IF (WIN1(4,IL).GT.FLDSZ(2,1)) WIN1(4,IL) = FLDSZ(2,1) - 5
            IF (IK.GT.0) IL = IL + 1
 160        CONTINUE
         IL = IL - 1
         NBOXES = MIN (IL, NBOXES)
 170  DOBOX = NBOXES.GT.0
C                                       Fill in gridding fn defaults
      CALL GRDFLT (CTYPX, CTYPY, XPARM, YPARM)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TROUBLE GETTING PARMS: IERR=',I7)
 1030 FORMAT ('ERROR',I3,' FINDING UVDATA')
 1060 FORMAT ('TOO FEW SPECTRAL CHANNELS IN DATA=',I3,' WANT=',I3)
 1070 FORMAT ('MXIN: NX,NY = ',2I6,' ARE ILLEGAL')
 1110 FORMAT ('MXIN  : Sort =''',A2,''' not ''',A2,
     *  ''', Using Un-sorted gridding')
      END
      SUBROUTINE MXINIT (FREQIF, IRET)
C-----------------------------------------------------------------------
C   MXINIT performs initialization functions for MX.  Mostly creates
C   output image and scratch files.
C   Input
C     FREQIF     D      Frequency Offset for IF
C   Output
C     IRET       I      = 0  = > ok,  = 1  = > fatal error
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQIF
      INTEGER IRET
C-----------------------------------------------------------------------
      CHARACTER  RTEMP*6, ITYP*2, STAT*4, RANTYP(6)*8, ISTOK(6)*2,
     *   LL*8, MM*8, CFREQ*8, STOKE*8, JYBM*8, UVWORK*6, PROJ*4
      HOLLERITH SCRH(256)
      INTEGER   IMODE, II1, II2, II3, LIMIT, NP(2), II, I, J, MX, MY,
     *   IERR, LRECC, IBUFF1(1024), LUN, OLDNAX(7), OLDIM, IFR, INDEX,
     *   IF, IROUND, SEQ
      INTEGER   I4T, IBCOMP, ISIZE
      LOGICAL   ISOLD, F, DOPROJ
      REAL      OLDCIC(7), OLDR(256), OLDXSH, OLDYSH, SCRR(256)
      DOUBLE PRECISION OLDD(128), SCRD(128), F8, STOK(7), OLDCRV(7),
     *   UVWSTK
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATOLD, OLDR, OLDD),    (BUFF1, IBUFF1),
     *   (CATSCR, SCRR, SCRD, SCRH)
C                                         Rand. parm. names.
      DATA RANTYP /'UU-L    ','VV-L    ','WW-L    ',
     *   'TIME1   ','BASELINE','        '/
      DATA LUN /27/
      DATA ISTOK /'I ','Q ','U ','V ','R ','L '/
      DATA F /.FALSE./
      DATA LL, MM, CFREQ, STOKE, JYBM, UVWORK
     *   /'RA---SIN', 'DEC--SIN', 'FREQ    ', 'STOKES  ',
     *   'JY/BEAM ', 'UVWORK'/
      DATA STOK /0.0D0,1.0D0,2.0D0,3.0D0,4.0D0,-1.0D0,-2.0D0/
C-----------------------------------------------------------------------
      CLNCNT = 0
      DOPTMD = F
      NONEG = F
      LIMFLX = -1.0
C                                       Find highest sequence number
      LIMIT = MFIELD + 1
C                                       If creating default sequence
      IF (SEQOUT.LE.0) THEN
         DO 4 I = 1,LIMIT
            CCNO = 0
            SEQ = 0
            II = 0
            ITYP = '  '
            J = I - 1
            IF (I.EQ.1) THEN
               RTEMP = 'IBM'
               J = 1
            ELSE IF (CLNLIM.LT.0.1) THEN
               RTEMP = 'IIM'
            ELSE
               RTEMP = 'ICL'
               END IF
            IF (J.LE.999) THEN
               WRITE (RTEMP(4:6),1004) J
            ELSE
               WRITE (RTEMP(3:6),1005) J
               END IF
C                                       Set stokes to correct value
            RTEMP(1:1) = ISTOK(MODE)(1:1)
            CALL CATDIR ('SRCH', II, CCNO, NAMOUT, RTEMP, SEQ, ITYP,
     *         NLUSER, STAT, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               SEQ = 0
               IF (IRET.NE.5) THEN
                  WRITE (MSGTXT,1010) IRET
                  GO TO 990
                  END IF
               END IF
            SEQOUT = MAX (SEQOUT, SEQ)
 4          CONTINUE
         SEQOUT = SEQOUT + 1
C                                       End if creating default sequence
         END IF
C                                       Create output files
      DO 110 I = 1,LIMIT
C                                       Calc Field Number
         IF = I - 1
C                                       Construct headers
         CALL COPY (256, CATOLD, CATBLK)
C                                       check if this is a
C                                       beam (I=1)
         IF (I.EQ.1) THEN
C                                       Beam.
            CATR(KRCRP) = NXBEM(1) / 2
            CATR(KRCRP+1) = NYBEM(1) / 2 + 1
            CATR(KRXSH) = 0.0
            CATR(KRYSH) = 0.0
            CATBLK(KINAX) = NXBEM(1)
            CATBLK(KINAX+1) = NYBEM(1)
            IMODE = 1
C                                       Map.
         ELSE
            CATR(KRCRP) = ICNTRX(IF) + DXCOR(IF)
            CATR(KRCRP+1) = FLDSZ(2,IF) - ICNTRY(IF) + 2 - DYCOR(IF)
            CATR(KRXSH) = CATR(KRXSH) + XPOFF(IF)
            CATR(KRYSH) = CATR(KRYSH) + YPOFF(IF)
C                                       Reset XPOFF,YPOFF for tangent
C                                       plane offsets.
            XPOFF(IF) = (CATR(KRCRP) - ICNTRX(IF)) * CELLSG(1) / 3600.
            YPOFF(IF) = (CATR(KRCRP+1) - ICNTRY(IF)) * CELLSG(2) / 3600.
            CATBLK(KINAX) = FLDSZ(1,IF)
            CATBLK(KINAX+1) = FLDSZ(2,IF)
            IMODE = MODE + 1
            END IF
         CALL CHR2H (8, JYBM, 1, CATH(KHBUN))
C                                       Get projected coordinates
         INDEX = ILOCU * 2
         CALL H2CHR (4, 1, CATH(KHPTP+INDEX+1), PROJ)
         DOPROJ = PROJ .NE. '    '
         CALL CHR2H (8, LL, 1, CATH(KHCTP))
         IF (DOPROJ) CALL CHR2H (4, PROJ, 5, CATH(KHCTP))
         CALL CHR2H (8, MM, 1, CATH(KHCTP+2))
         IF (DOPROJ) CALL CHR2H (4, PROJ, 5, CATH(KHCTP+2))
         CALL CHR2H (8, CFREQ, 1, CATH(KHCTP+4))
         CALL CHR2H (8, STOKE, 1, CATH(KHCTP+6))
         CATD(KDCRV) = RA
         CATD(KDCRV+1) = DEC
C                                       Preserve reference freq
         CATD(KDCRV+2) = FREQ + FREQIF
C                                       Stokes' type.
         CATD(KDCRV+3) = STOK(IMODE)
         CATR(KRCIC) =  CELLSG(1) / 3.6D3
         CATR(KRCIC+1) = CELLSG(2) / 3.6D3
         CATR(KRCIC+2) = OLDR(KRCIC+JLOCF) * CHINC
         CATR(KRCIC+3) = 1.D0
C                                       Assume frequency at center
C                                       of bandpass.
         CATR(KRCRP+2) = (OLDR(KRCRP+JLOCF) - BCHAN -
     *      ((NCHAVG-1.0)/2.0)) / CHINC + 1.0
C                                       Alternate frequency axis ref
C                                       pixel.
         CATR(KRARP) = (CATR(KRARP)  - BCHAN - (NCHAVG-1.0)/2.0) /
     *      CHINC + 1.0
         CATR(KRCRP+3) = 1.
         CATR(KRCRT+1) = MAPROT
         CATR(KRCRT+0) = 0.0
         CATR(KRCRT+2) = 0.
         CATR(KRCRT+3) = 0.
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
         CATR(KRBLK) = 0.
         CATBLK(KIGCN) = 0
         CATBLK(KIPCN) = 0
         CATBLK(KIDIM) = 4
C                                       Number of spectral channels
         CATBLK(KINAX+2) = NUMCH
         CATBLK(KINAX+3) = 1
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         J = I-1
         IF (I.EQ.1) THEN
            RTEMP = 'IBM'
            J = 1
         ELSE IF (CLNLIM.LT.0.1) THEN
            RTEMP = 'IIM'
         ELSE
            RTEMP = 'ICL'
            END IF
         IF (J.LE.999) THEN
            WRITE (RTEMP(4:6),1004) J
         ELSE
            WRITE (RTEMP(3:6),1005) J
            END IF
C                                       Set stokes to correct value
         RTEMP(1:1) = ISTOK(MODE)(1:1)
         CALL CHR2H (6, RTEMP, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMU) = NLUSER
C                                       Set sequence number.
         CATBLK(KIIMS) = SEQOUT
C                                       Create output map file.
         CCNO = 1
         CALL MCREAT (DISOUT, CCNO, BUFF1, IRET)
         ISOLD = .FALSE.
         IF (IRET.NE.0) THEN
            ISOLD = .TRUE.
C                                       If file already exists
            IF (IRET.EQ.2) THEN
C                                       Existing file: check it out
               OLDIM = CATBLK(KIDIM)
               DO 40 II = 1,KICTPN
                  OLDNAX(II) = CATBLK(KINAX+II-1)
                  OLDCRV(II) = CATD(KDCRV+II-1)
                  OLDCIC(II) = CATR(KRCIC+II-1)
 40               CONTINUE
               OLDXSH = CATR(KRXSH)
               OLDYSH = CATR(KRYSH)
               CALL CATIO ('READ', DISOUT, CCNO, CATBLK, 'REST', BUFF1,
     *            IRET)
               IF (OLDIM.NE.CATBLK(KIDIM)) GO TO 60
C                                       Get max,min
               IF ((IF.GE.1) .AND. (RCHAN.GT.BCHAN))
     *            GMAX(IF) = CATR (KRDMX)
               IF ((IF.GE.1) .AND. (RCHAN.GT.BCHAN))
     *            GMIN(IF) = CATR (KRDMN)
               DO 50 II = 1,OLDIM
                  IF (OLDNAX(II).NE.CATBLK(KINAX+II-1)) GO TO 60
                  IF (ABS (OLDCRV(II)-CATD(KDCRV+II-1)).GT.
     *               0.05 * ABS (CATR(KRCIC+II-1))) GO TO 60
                  IF (ABS (OLDCIC(II)-CATR(KRCIC+II-1)) .GT.
     *               0.05 * ABS (CATR(KRCIC+II-1))) GO TO 60
 50              CONTINUE
               IF (ABS (OLDXSH-CATR(KRXSH)) .GT. 0.05 *
     *            ABS (CATR(KRCIC))) GO TO 60
               IF (ABS (OLDYSH-CATR(KRYSH)) .GT. 0.05 *
     *            ABS (CATR(KRCIC+1))) GO TO 60
C                                       It matches, mark WRITE
               CALL CATIO ('WRIT', DISOUT, CCNO, CATBLK, 'WRIT', BUFF1,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1050) IRET
                  CALL MSGWRT (7)
                  END IF
C                                       If here, files are same
               GO TO 70
C                                       Not same file !!!
 60            CONTINUE
               IRET = 8
            MSGTXT = 'OLD CLEAN MAP NOT COMPATABLE WITH CURRENT REQUEST'
               GO TO 990
            ELSE
C                                       Else Map Create error
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
C                                       End IF Map Create error
            END IF
C                                       Mark in CFIL.INC
 70      NCFILE = NCFILE + 1
         FCNO(NCFILE) = CCNO
         FVOL(NCFILE) = DISOUT
         FRW(NCFILE) = 2
         IF (ISOLD) FRW(NCFILE) = 1
C                                       Beam.
         IF (I.EQ.1) THEN
            CNOBEM(1) = CCNO
C                                       Map.
         ELSE
            CCCNO(IF) = CCNO
            CCDISK(IF) = DISOUT
            OLD(IF) = ISOLD
C                                       get old CLEAN beam if no
C                                       new one is given.
            IF ((BMAJ.LE.0.0) .AND. OLD(IF)) THEN
               BMIN = CATR(KRBMN) * 3600.
               BPA = CATR(KRBPA)
               BMAJ = CATR(KRBMJ) * 3600.
               END IF
C                                       If this field to be displayed
C                                       copy CATBLK.
            IF (IF.EQ.TVFLD) CALL COPY (256, CATBLK, CATTV)
C                                       Create/get ver # for CC file.
            LRECC = 3
C                                       Number of components for field.
            IBCOMP = XBCOMP(IF) + 0.5
            IF (IBCOMP.LE.0) IBCOMP = 0
C                                       Record Shift type
            CALL CATKEY ('WRIT', DISOUT, CCNO, 'SHIFTYPE', 1, 1, 2, 4,
     *         BUFF1, IRET)
C                                       Don't make CC tables if not
C                                       CLEANing.
            IF (CLNLIM.GT.0) THEN
               II1 = BCHAN
               II2 = ECHAN
               II3 = CHINC
               IF (II2.LT.II1) II2 = II1
               DO 100 IFR = II1,II2,II3
                  CCVER(IF) = (IFR - II1) / II3 + 1
                  CALL CCINI (LUN, LRECC, DISOUT, CCCNO(IF), CCVER(IF),
     *               CATBLK, IBUFF1, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1080) IRET
                     GO TO 990
                     END IF
                  I4T = IBUFF1(5)
C                                       Check no. CC comps.
                  IF (IFR.EQ.RCHAN) NCLNG(IF) = I4T
                  IF (NCLNG(IF).GT.IBCOMP) NCLNG(IF) = IBCOMP
                  IF (I4T.GT.IBCOMP) I4T = IBCOMP
                  IBUFF1(5) = I4T
                  IF (IFR.EQ.RCHAN) CLNCNT = CLNCNT + NCLNG(IF)
C                                       Close.
                  CALL TABIO ('CLOS', 0, LRECC, BUFF2, IBUFF1, IRET)
 100              CONTINUE
C                                       Update CATBLK
               CALL CATIO ('UPDT', DISOUT, CCCNO(IF), CATBLK, 'REST',
     *               BUFF1, IRET)
            IF ((IRET.GT.0) .AND. (IRET.LE.4)) THEN
               WRITE (MSGTXT,1050) IRET
               GO TO 990
               END IF
            END IF
         END IF
 110     CONTINUE
C                                       Check starting iteration no.
      CLNSTR = CLNCNT
C                                       Catalog uv data file.
      CALL COPY (256, CATOLD, CATSCR)
C                                       Revise info.
C                                       complex axis.
      CATSCR(KINAX) = 3
C                                       Frequency
      CATSCR(KINAX+JLOCF) = NCHAVG
C                                       Stokes'
      CATSCR(KINAX+JLOCS) = 1
      UVWSTK = CATD(KDCRV+3)
      SCRD(KDCRV+JLOCS) = UVWSTK
      SCRR(KRCRP+JLOCS) = 1.0
      SCRR(KRCIC+JLOCS) = 1.0
C                                       Random parameter
      CATSCR(KIPCN) = 5
      DO 120 I = 1,6
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RANTYP(I), 1, SCRH(INDEX))
 120     CONTINUE
C                                       Create/catalog
C                                       Names. etc.
      CALL MAKOUT (NAMOUT, CLASIN, SEQIN, UVWORK, NAME2, CLASS2, SEQ2)
      CALL CHR2H (12, NAME2, KHIMNO, SCRH(KHIMN))
      CALL CHR2H (6, CLASS2, KHIMCO, SCRH(KHIMC))
      CATSCR(KIIMS) = SEQ2
      CATSCR(KINIT) = 0
C                                       Create output file.
      CNOSCR = 1
      FRW(NCFILE+1) = 2
      CALL RCOPY (256, CATR, BUFF3)
      CALL COPY (256, CATSCR, CATBLK)
      CALL UVCREA (DISK2, CNOSCR, BUFF1, IERR)
      IF (IERR.NE.0) THEN
C                                       IF file already exists
         IF (IERR.EQ.2) THEN
            IERR = 0
C                                       Recover existing CATBLK
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISK2, CNOSCR, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1050) IERR
               CALL MSGWRT (6)
               END IF
         ELSE
C                                       Else tell user UV scratch error
            WRITE (MSGTXT,1140) IERR
            GO TO 990
            END IF
C                                       End If no create error
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOSCR
      FVOL(NCFILE) = DISK2
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Get name of file.
      CALL ZPHFIL ('UV', DISK2, CNOSCR, 1, UVSCNM, IERR)
      CALL COPY (256, CATBLK, CATSCR)
      CALL RCOPY (256, BUFF3, CATR)
      SEQ2 = CATSCR(KIIMS)
      IERR = 0
C                                       Check uv work file
C                                       Position
      F8 = ((OLDD(KDCRV+JLOCR) - SCRD(KDCRV+JLOCR)) ** 2) +
     *   ((OLDD(KDCRV+JLOCD) - SCRD(KDCRV+JLOCD)) ** 2)
C                                       Must be within 1 arcsec
      IF (F8 .GT. ((1.0D0/3600.0D0)**2)) GO TO 170
C                                       Number of rand parm must be 5
      IF (CATSCR(KIPCN) .NE. 5) GO TO 170
C                                       Complex axis dimension 3
      IF (CATSCR(KINAX) .NE. 3) GO TO 170
C                                       Same polarization
      IF (ABS (SCRD(KDCRV+JLOCS)-UVWSTK).GT.0.1) GO TO 170
C                                       OK
      GO TO 190
C                                       UV work file incompatable
C                                       with current use.
 170     MSGTXT = 'UV WORK FILE INCONSISTENT WITH CURRENT USE'
         IRET = 1
         GO TO 990
C                                       Create scratch files.
C                                         Input uv data=LUNS(1)
C                                       Assignments in /CFILE/:
C                                       SCRGRD = Grid/Map for largest.
C                                         LUNS(4)
C                                       SCRWRK = Work, like 4
C                                         LUNS(5)
C
C                                       Grid. file.
 190  MX = NXMAX + 2
      MY = NYMAX + 2
C                                       Increase size if possible for
C                                       interpolation
      IF ((NXMAX.LT.4096) .AND. ((METHOD.GE.0) .AND.
     *   (CLNLIM.GT.0)))  MX = MX * 2
      IF ((NYMAX.LT.4096) .AND. ((METHOD.GE.0) .AND.
     *   (CLNLIM.GT.0)))  MY = MY * 2
      NP(1) = MIN (MX, MY) + 2
      NP(2) = MAX (MX, MY)
C                                       Add extra rows for bandwidth
C                                       synthesis.
      NP(1) = NP(1) + 2 * IROUND (MAX (XPARM(1), 1.0))
C                                       Determine file size
      CALL MAPSIZ (2, NP, ISIZE)
C                                       Make GRID file.
      CALL SCREAT (ISIZE, IBUFF1, IRET)
      SCRGRD = NSCR
      IF (IRET.NE.0) THEN
         MSGTXT = 'TOO LITTLE DISK SPACE FOR SCRATCH FILE'
         IF (IRET .NE. 1) MSGTXT = 'TROUBLE CREATING SCRATCH FILE'
         GO TO 990
         END IF
C                                       WORK file (SCRWRK)
      CALL SCREAT (ISIZE, IBUFF1, IRET)
      SCRWRK = NSCR
      IF (IRET.NE.0) THEN
         MSGTXT = 'TOO LITTLE DISK SPACE FOR SCRATCH FILE'
         IF (IRET .NE. 1) MSGTXT = 'TROUBLE CREATING SCRATCH FILE'
         GO TO 990
         END IF
      GO TO 999
C                                       error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1004 FORMAT (I3.3)
 1005 FORMAT (I4.4)
 1010 FORMAT ('ERROR',I3,' SCANNING OUTPUT FILES')
 1020 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1050 FORMAT ('CATIO ERROR',I3)
 1080 FORMAT ('ERROR',I3,' CREATING CC EXTENSION FILE')
 1140 FORMAT ('ERROR',I3,' CREATING/CATALOGING UV WORK FILE')
      END
      SUBROUTINE MXSEL (ICH, IRET)
C-----------------------------------------------------------------------
C   MXSEL reads the uv data and converts to the form expected in the
C   rest of the program. Does various frequency dependent bookeeping
C   functions:
C    1) Sets block offsets for residual and beam files.
C    2) Sets FFRAC = frequency correction.
C    3) Initializes FLUXG sums and counts.
C    4) Sets FSTCLN true to indicate that this is the first CLEAN
C       cycle on this channel.
C   Output:
C      IRET   I    Return error code: 0=>OK, otherwise an error occured.
C   Useage note: Currently this routine will only use up to MAXCIF
C   bandwidth frequency channels.
C-----------------------------------------------------------------------
      INTEGER   ICH, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      CHARACTER STOKS(6)*4, FILE*48
      HOLLERITH OLDH(256)
      INTEGER   LRECO, LENIN, INIO, FIND1, FIND2, NAX, NIOUT, BIND1,
     *   BIND2, IPOINT, OPOINT, OCNT, J, I, SAX(7), PLARR(5), IERR,
     *   MVIS(8), IBCHAN, IECHAN, PMODE, NFLD, IROUND, BO, VO, IIVER,
     *   OOVER, INTIND, WTOFF, CMPNT, CMPNT2, QVER, LUNQ, BUFFQ(256),
     *   JERR, VALUES(2), NIF, IA1, IA2
      LOGICAL   ISCMP, TABLE, FITASC, FQEXIS, WAUTO
      REAL      BLEN, BLMN2, BLMX2, DU, DV,
     *   DW, UMAX, VMAX, SCRR(256), OLDR(256), VSREC(4096), UU, VV, WW,
     *   FFRAC, MAXBL2, MINBL2, MINTO2, IFFREQ, GCORMN,
     *   MAXU, IFFRMX, IFFRMN, AUMAX, AVMAX, BUMAX, BVMAX, UTFACT,
     *   VTFACT
      LOGICAL   T, F, ALLWT, DROP
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION SCRD(128), FOFF(MAXIF), GCOR, GCOR2, GCORMX,
     *   SFREQS(MAXIF)
      INTEGER   NDECMP, DECMP(2,MAXIF*4), BCHANS, ECHANS, ISBAND(MAXIF),
     *   JADR(2,4)
      REAL      FINC(MAXIF), WORKR(MAXIF), SFACT(2,4), VS(3,MAXCIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      REAL      INBUF(UVBFSS), OUTBUF(UVBFSS), XPNBUF(UVBFSS)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUF, BUFF1),     (OUTBUF, BUFF2),
     *   (CATOLD, OLDR, OLDH)
      EQUIVALENCE (CATSCR, SCRR, SCRD)
      EQUIVALENCE (XPNBUF, BUFF3)
      EQUIVALENCE (UU, VSREC(1)), (VV, VSREC(2)), (WW, VSREC(3)),
     *   (VS, VSREC(6))
      DATA VO, BO /0, 1/
      DATA STOKS /'I   ','Q   ','U   ','V   ','RR  ','LL  '/
      DATA T, F /.TRUE.,.FALSE./
      DATA BCHANS, ECHANS /0, 0/
C                                       set half width of grid interp.
      DATA MINTO2 /6.0/
      DATA SFREQS /MAXIF*0.0D0/
C-----------------------------------------------------------------------
C                                       Initialisation
      WAUTO = .FALSE.
C                                       Set max no. fields
      NFLD = MAXFLD
C                                       Initialize some common variables
      CALL RFILL (NFLD, 0.0, FLUXG)
      IF (ICH.GT.RCHAN) CLNSTR = 0
      TFLUXG = 0.0
      RESMAX = -0.02
      FSTCLN = T
C                                       Reset /UVHDR/
      CALL RCOPY (256, CATR, BUFF3)
      CALL COPY (256, CATOLD, CATBLK)
      CALL UVPGET (IRET)
C                                       Setup to select visibilities.
      IBCHAN = ICH
C                                       Number of channels to grid
      NCHAVG = MAX (IROUND (XNCAVG), 1)
      IECHAN = IBCHAN + NCHAVG - 1
C                                       If past end of channels
      IF (IECHAN.GT.CATOLD(KINAX+JLOCF)) THEN
         IECHAN = CATOLD(KINAX+JLOCF)
C                                       If channels left
         IF (IBCHAN.LE.CATOLD(KINAX+JLOCF)) THEN
C                                       Average remainder
            WRITE(MSGTXT,1020) IBCHAN, IECHAN
            CALL MSGWRT(5)
         ELSE
            WRITE(MSGTXT,1010) 'BEGIN', IBCHAN, IECHAN
            IRET = 5
            GO TO 980
C                                       End if channels left
            END IF
C                                       End if past end of channels
         END IF
C                                       Number of channels to grid
      NCHAVG = IECHAN - IBCHAN + 1
      NCHAVG = NCHAVG * (EIF - BIF + 1)
C                                       Init. for select data.
      CALL DGINIT (STOKS(MODE), IBCHAN, IECHAN, BIF, EIF, MVIS, JADR,
     *   SFACT, ALLWT, PMODE, IRET)
      IF (IRET.GT.0) THEN
         MSGTXT = 'MXSEL: REQUESTED DATA NOT AVAILABLE IN DATABASE'
         GO TO 980
         END IF
C                                       Compressed Input?
      ISCMP = CATOLD(KINAX).EQ.1
      IF (ISCMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATOLD(KIPCN), OLDH(KHPTP), WTOFF,
     *      IERR)
C                                       Must have this one
         IF ((IERR.NE.0) .OR. (WTOFF.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            GO TO 980
            END IF
C                                       Get data decompression pointers
         CALL CMPARM (BIF, EIF, IBCHAN, IECHAN, BCHANS, ECHANS,
     *      NDECMP, DECMP)
         END IF
C                                       Restore CATBLK
      CALL RCOPY (256, BUFF3, CATR)
      NAX = 2
      CALL FILL (7, 0, SAX)
      CALL FILL (5, 1, PLARR)
      PLARR(1) = (ICH - BCHAN + 1.0) / CHINC + 0.999
      IF (NUMCH.GT.1) NAX = 3
C                                       Set BO for residual files.
      DO 10 I = 1,MFIELD
C                                       Set NCLNG, NSUBG
         NSUBG(I) = 1
         IF (ICH.GT.RCHAN) NCLNG(I) = 0
C                                       Set CC ver.
         CCVER(I) = (ICH - BCHAN) / CHINC + 1
         SAX(1) = FLDSZ(1,I)
         SAX(2) = FLDSZ(2,I)
         CALL COMOFF (NAX, SAX, PLARR, BORES(I), IRET)
         BORES(I) = BORES(I) + 1
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1001) IRET, I, ICH
            GO TO 980
            END IF
 10      CONTINUE
C                                       BEAM BO
      SAX(1) = NXBEM(1)
      SAX(2) = NYBEM(1)
      CALL COMOFF (NAX, SAX, PLARR, BOBEM(1), IRET)
      BOBEM(1) = BOBEM(1) + 1
      LRECO =  5 + (3 * NCHAVG)
C                                       Determine some constants.
      DU = RAD2AS / (MIN(NXUNF,NXBEM(1)) * ABS (CELLSG(1)))
      DV = RAD2AS / (MIN(NYUNF,NYBEM(1)) * CELLSG(2))
      DW = 1.0
C                                       Set limits.
      UMAX = (MIN(NXUNF,NXBEM(1))/2-1.0) * DU
      VMAX = (MIN(NYUNF,NYBEM(1))/2-1.0) * DV
C                                       Fix for gridding
C                                       3.0 corresponds to the
C                                       interpolation fn half
C                                       width.
      AUMAX = MAX (MINTO2, XPARM(1)) * DU / UMAX
      AVMAX = MAX (MINTO2, YPARM(1)) * DV / VMAX
C                                       taper factors
      UTFACT = 1.0
      VTFACT = 1.0
      IF ((DOTAPE) .AND. (TAPERU.GT.0.0)) UTFACT = EXP (LOG(0.3) *
     *   ((UMAX*0.7) / (1000. * TAPERU))**2)
      IF ((DOTAPE) .AND. (TAPERV.GT.0.0)) VTFACT = EXP (LOG(0.3) *
     *   ((VMAX*0.7) / (1000. * TAPERV))**2)
      UTFACT = SQRT (UTFACT)
      VTFACT = SQRT (VTFACT)
C                                       UVGRID does do guard band here
      GUARDB(1) = 0.0
      GUARDB(2) = 0.0
C                                       default guard band widths
      IF (XGUARD(1).LT.-0.001) THEN
         XGUARD(1) = AUMAX
      ELSE IF ((XGUARD(1).LE.0.001) .OR. (XGUARD(1).GT.0.9)) THEN
         XGUARD(1) = 0.30 * UTFACT
         END IF
      XGUARD(1) = MAX (XGUARD(1), AUMAX)
      IF (XGUARD(2).LT.-0.001) THEN
         XGUARD(2) = AVMAX
      ELSE IF ((XGUARD(2).LE.0.001) .OR. (XGUARD(2).GT.0.9)) THEN
         XGUARD(2) = 0.30 * VTFACT
         END IF
      XGUARD(2) = MAX (XGUARD(2), AVMAX)
C                                       Max is edge - guard
      UMAX = UMAX * (1.0 - XGUARD(1))
      VMAX = VMAX * (1.0 - XGUARD(2))
C                                       Get Baseline limits each pass
      BLMIN = MAX (0.0, XBLMIN)
      BLMAX = XBLMAX
      IF (BLMAX.LE.0.0) BLMAX = 1.0E16
C
      BLMX2 = 1.0E6 * BLMAX * BLMAX
      BLMN2 = 1.0E6 * BLMIN * BLMIN
      AUMAX = 0.0
      AVMAX = 0.0
      BUMAX = 0.0
      BVMAX = 0.0
C                                       Frequency correction fact.
      IF (OLDR(KRCIC+JLOCF).LT.0.0) THEN
         GCOR = (FREQUV + (ICH - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQUV
      ELSE
         GCOR = (FREQUV + (ICH + CHINC - 1 - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQUV
         END IF
C                                       Init frequency scale factor
      IFFRMX = 1.0
      IFFRMN = 1.0
C                                       If IF entries, find max offset
      IF (JLOCIF.GE.0) THEN
         IIVER = 1
C                                       Read IF frequency table
         CALL CHNDAT ('READ', BUFF1, DISIN, CNOOLD, IIVER, CATOLD,
     *       LUNS(5), NIF, FOFF, ISBAND, FINC, FRQSEL, IRET)
C                                       If IF table found
         IF (IRET.EQ.0) THEN
C                                       For All IFs
            DO 20 I = BIF, EIF
C                                       Calc freq for IF
               IFFREQ = (FREQUV + FOFF(I))/FREQUV
C                                       IF only one IF
               IF (BIF.EQ.EIF) THEN
C                                       Use the correct factor
                  IFFRMX = IFFREQ
                  IFFRMN = IFFREQ
               ELSE
C                                       Else Find Max GCOR factor
                  IFFRMX = MAX( IFFREQ, IFFRMX)
                  IFFRMN = MIN( IFFREQ, IFFRMN)
                  END IF
 20            CONTINUE
            END IF
C                                       End if must deal with IFs
         END IF
C                                       reset limit for highest freq.
C                                       Keep extrema of factors
      GCORMX = GCOR * IFFRMX
      GCORMN = GCOR * IFFRMN
      UMAX = UMAX / GCORMX
      VMAX = VMAX / GCORMX
      GCOR2  = GCORMX**2
      MAXU   = 0
      MAXBL2 = 0
      MINBL2 = 1E30
      FFRAC = GCORMX - 1.0D0
C                                       ICH is the current channel no.
      IF (ICH .GT. CATOLD(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1010) 'CURRENT', ICH, CATOLD(KINAX+JLOCF)
         IRET = 8
         GO TO 980
         END IF
C                                       Determine length of input
C                                       and buffer.
      LENIN = ((JBUFSZ-2*NBPS) / (2.0 * 2)) / LREC
      LENIN = MAX (1, LENIN)
C                                       Check if restarting with work
C                                       uv file.
      SKPSUB = (CLNSTR.EQ.CATSCR(KINIT)) .AND. (ICH.EQ.RCHAN) .AND.
     *   (CLNSTR.GT.0)
      IF (SKPSUB) THEN
C                                       send message if using old file
         MSGTXT = 'Restarting using old UV work file'
         CALL MSGWRT (5)
C                                       Get sums of CLEAN components.
         CALL MXCSUM (BUFF1)
C                                       Don't subrtact components again
         DO 30 I = 1,MFIELD
            NSUBG(I) = NCLNG(I) + 1
 30         CONTINUE
         GO TO 240
         END IF
C                                       Open UV work file.
      CALL ZOPEN (LUNS(2), FIND2, DISK2, UVSCNM, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, 'WRIT'
         GO TO 980
         END IF
C                                       Open vis file.
      CALL ZPHFIL ('UV', DISIN, CNOOLD, 1, FILE, IRET)
      CALL ZOPEN (LUNS(1), FIND1, DISIN, FILE, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, 'READ'
         GO TO 980
         END IF
C                                       Init files.
C                                       Input file opened in MXINIT
      INIO = 0
      CALL UVINIT ('READ', LUNS(1), FIND1, NVIS, VO, LREC, INIO, JBUFSZ,
     *   INBUF, BO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) 'READ', IRET
         GO TO 980
         END IF
      NIOUT = 0
      CALL UVINIT ('WRIT', LUNS(2), FIND2, NVIS, VO, LRECO, NIOUT,
     *   JBUFSZ, OUTBUF, BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) 'WRIT', IRET
         GO TO 980
         END IF
      OPOINT = BIND2
      OCNT = 0
      XCOUNT = 0
C                                       If not restarting uvwork
      IF (.NOT.SKPSUB) THEN
C                                       Copy the AN table
         CALL TABCOP ( 'AN', 0, 0, LUNS(1), LUNS(2), DISIN,
     *      DISK2, CNOOLD, CNOSCR, CATSCR, BUFF3, BUFF3(512), IRET)
         IRET = 0
C                                       End if not restarting uvwork
         END IF
C                                       Initialize visibilities.
      DO 80 I = 1,NCHAVG
         VS(1,I) = 0.0
         VS(2,I) = 0.0
         VS(3,I) = 0.0
 80      CONTINUE
C                                       Begin loop reading and convertin
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUNS(1), FIND1, INBUF, INIO, BIND1, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 980
 110     IPOINT = BIND1
         IF (INIO.LE.0) GO TO 210
C                                       Loop thru buffer.
         DO 200 I = 1,INIO
C                                       Convert record. Correct for
C                                       frequency in UVGRID in AP.
            UU = INBUF(IPOINT+ILOCU)
            VV = INBUF(IPOINT+ILOCV)
            WW = INBUF(IPOINT+ILOCW)
C                                       Put time and baseline in spare
C                                       words - not currently used.
            VSREC(4) = INBUF(IPOINT+ILOCT)
            VSREC(5) = INBUF(IPOINT+ILOCB)
C                                       Extract antenna numbers
            IA1 = VSREC(5) / 256.0 + 0.1
            IA2 = VSREC(5) - 256.0 * IA1 + 0.1
C                                       Check if data desired.
            BLEN = (UU*UU + VV*VV)*GCOR2
            IF ((BLEN.LT.BLMN2) .OR. (BLEN.GT.BLMX2)) GO TO 190
            AUMAX = MAX (AUMAX, ABS(UU))
            AVMAX = MAX (AVMAX, ABS(VV))
            IF ((ABS(UU).GE.UMAX) .OR. (ABS(VV).GE.VMAX)) GO TO 190
            BUMAX = MAX (BUMAX, ABS(UU))
            BVMAX = MAX (BVMAX, ABS(VV))
C                                       Get visibilities.
C                                       Compressed data - decompress
            IF (ISCMP) THEN
               DO 120 J = 1,NDECMP
                  CMPNT = IPOINT + NRPARM + DECMP(2,J)
                  CMPNT2 = 1 + DECMP(2,J) * 3
                  CALL ZUVXPN (DECMP(1,J), INBUF(CMPNT),
     *               INBUF(IPOINT+WTOFF), XPNBUF(CMPNT2))
 120              CONTINUE
               INTIND = 3
               CALL DGGET (XPNBUF, INTIND, .TRUE., MVIS, JADR, SFACT,
     *            ALLWT, VS, DROP)
            ELSE
C                                       Normal data
               INTIND = CATOLD(KINAX)
               CALL DGGET (INBUF(IPOINT+NRPARM), INTIND, .TRUE., MVIS,
     *            JADR, SFACT, ALLWT, VS, DROP)
               END IF
            IF (DROP) GO TO 190
C                                       Warn if autocorrelation data
               IF (IA1.EQ.IA2) WAUTO = .TRUE.
C                                       record max, min Baseline
               MAXBL2 = MAX(BLEN,MAXBL2)
               MINBL2 = MIN(BLEN,MINBL2)
               MAXU   = MAX(MAXU,ABS(UU))
C                                       Put u,v,w on proper side of grid
               IF (UU.LT.0.0) THEN
                  UU = -UU
                  VV = -VV
                  WW = -WW
                  DO 140 J = 1,NCHAVG
                     VS(2,J) = -VS(2,J)
 140                 CONTINUE
                  END IF
C                                       If setting weight to one
               IF (UNITWT) THEN
                  DO 145 J = 1,NCHAVG
                     VS(3,J) = SIGN(1.0,VS(3,J))
 145                 CONTINUE
                  END IF
C                                       If using VLBI weighting
               IF (VLBIWT) THEN
                  DO 147 J = 1,NCHAVG
                     VS(3,J) = SIGN(SQRT(SQRT(ABS(VS(3,J)))),VS(3,J))
 147                 CONTINUE
                  END IF
C                                       Copy to output, note use of
C                                       EQUIVALENCE (may confuse
C                                       vectorizer)
      INCLUDE 'INCS:ZVND.INC'
               DO 160 J = 1,LRECO
                  OUTBUF(OPOINT+J-1) = VSREC(J)
 160              CONTINUE
C                                       Count vis. rec.
               XCOUNT = XCOUNT + 1
C                                       Check if time for a write.
               OPOINT = OPOINT + LRECO
               OCNT = OCNT + 1
               IF (OCNT.LT.NIOUT) GO TO 190
C                                       Time for write.
                  NIOUT = OCNT
                  CALL UVDISK ('WRIT', LUNS(2), FIND2, OUTBUF, NIOUT,
     *               BIND2, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1100) 'WRIT', IRET
                     GO TO 980
                     END IF
                  OPOINT = BIND2
                  OCNT = 0
C                                       Update input buffer.
 190        IPOINT = IPOINT + LREC
 200        CONTINUE
C                                       Loop back for next input buffer.
         GO TO 100
C                                       Flush output buffer.
 210  NIOUT = -OCNT
      CALL UVDISK ('FLSH', LUNS(2), FIND2, OUTBUF, NIOUT, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IRET
         GO TO 980
         END IF
C
      BLMAX = SQRT(MAXBL2)/1.E3
C                                       Set min baseline for lowest freq
      BLMIN = SQRT(MINBL2)*(GCORMN/GCORMX)/1.E3
C                                       Use input limit, if smaller
      BLMIN = MIN (BLMIN, XBLMIN)
C                                       copy to values buffer
      CALL RCOPY ( 1, MAXU*1.01, VALUES(1))
C                                       Record Max Baseline in header
      CALL CATKEY ('WRIT', DISK2, CNOSCR, 'MAXBLINE', 1, 1,
     *      VALUES, 2, OUTBUF, IERR)
C                                       copy to values buffer
      CALL RCOPY( 1, BLMIN*0.99E3, VALUES(1))
C                                       Record Min Baseline in header
      CALL CATKEY ('WRIT', DISK2, CNOSCR, 'MINBLINE', 1, 1,
     *      VALUES, 2, OUTBUF, IERR)
C                                       Close files.
      CALL ZCLOSE (LUNS(2), FIND2, IRET)
      CALL ZCLOSE (LUNS(1), FIND1, IRET)
C                                       Autocorrelation data ?
      IF (WAUTO) THEN
         MSGTXT = '**** WARNING autocorrelation data selected ****'
         CALL MSGWRT (6)
         MSGTXT = '**** De-select using UVRANG if necessary ****'
         CALL MSGWRT (6)
         END IF
C                                       data outside grid?
      IF ((AUMAX.GT.UMAX) .OR. (AVMAX.GT.VMAX)) THEN
         MSGTXT = '**** WARNING data discarded outside usable part of'
     *      // ' UV plane ****'
         CALL MSGWRT (6)
         IF (AUMAX.GT.UMAX) THEN
            UU = UMAX/AUMAX * ABS(CELLSG(1))
            WRITE (MSGTXT,1210) 'U', AUMAX, UMAX
            CALL MSGWRT (6)
            WRITE (MSGTXT,1211) UU, ABS(CELLSG(1))
            CALL MSGWRT (6)
            WRITE (MSGTXT,1212) 'U', XGUARD(1)
            CALL MSGWRT (6)
            END IF
         IF (AVMAX.GT.VMAX) THEN
            VV = VMAX/AVMAX * ABS(CELLSG(2))
            WRITE (MSGTXT,1210) 'V', AVMAX, VMAX
            CALL MSGWRT (6)
            WRITE (MSGTXT,1211) VV, CELLSG(2)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1212) 'V', XGUARD(2)
            CALL MSGWRT (6)
            END IF
         END IF
      IF ((BUMAX.GT.(1.-0.35*UTFACT)*(MIN(NXUNF,NXBEM(1))/2-1.0)*DU)
     *   .OR.
     *   (BVMAX.GT.(1.-0.35*VTFACT)*(MIN(NYUNF,NYBEM(1))/2-1.0)*DV))
     *   THEN
         MSGTXT = '**** WARNING data included out of inner portion'
     *      // ' of UV plane ****'
         CALL MSGWRT (6)
         MSGTXT = '**** Watch for high-frequency & other poor ' //
     *      'cleaning effects ****'
         CALL MSGWRT (6)
         END IF
C                                       Get least lambda limit
      UMAX = MIN (UMAX, VMAX)
C                                       Tell how much data to be used.
      WRITE (MSGTXT,1220,ERR=240) XCOUNT, NVIS, MIN(UMAX,SQRT(BLMX2))
      IF (XCOUNT.EQ.NVIS) WRITE (MSGTXT,1221,ERR=220) NVIS
      CALL MSGWRT (4)
 220  CONTINUE
C                                        Check that some data given
      IF (XCOUNT.LE.1) THEN
         IRET = 8
         MSGTXT = 'MXSEL: NO VALID DATA FOUND'
         GO TO 980
         END IF
C                                       Set new no. vis.
      CATSCR(KIGCN) = XCOUNT
C                                       Jump here if restarting UVWORK
 240  CONTINUE
C                                       Write time message.
      CALL MXTIMR ('Got data    ')
C                                       Copy FQ table
      IIVER = 1
      OOVER = 1
      IF (JLOCIF.GE.0) THEN
C                                       Remove old FQ table, if one
C                                       exists
         QVER = 1
         LUNQ = 49
         CALL ISTAB ('FQ', DISK2, CNOSCR, QVER, LUNQ, BUFFQ,
     *       TABLE, FQEXIS, FITASC, JERR)
         IF (FQEXIS) CALL RMEXT (DISK2, CNOSCR, 'FQ', QVER,
     *      CATSCR, BUFFQ, JERR)
         CALL CHNCOP (IIVER, OOVER, LUNS(5), LUNS(6), DISIN, DISK2,
     *      CNOOLD, CNOSCR, CATOLD, CATSCR, BIF, EIF, FRQSEL, SFREQS,
     *      BUFF1, BUFF2, BUFF3, WORKR, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Update uv work header
      IF (JLOCIF.GE.0) CATSCR(KINAX+JLOCIF) = EIF - BIF + 1
      SCRR(KRCRP+JLOCF) = OLDR(KRCRP+JLOCF) - ICH + 1.0
      CALL CATIO ('UPDT', DISK2, CNOSCR, CATSCR, 'REST', BUFF1, IERR)
      IF ((IERR.NE.0) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1240) IERR
         CALL MSGWRT (6)
         END IF
C                                       Update /UVHDR/ to work file
      CALL RCOPY (256, CATR, BUFF3)
      CALL COPY (256, CATSCR, CATBLK)
      MSGSUP = 32000
      CALL UVPGET (IRET)
      MSGSUP = 0
      CALL RCOPY (256, BUFF3, CATR)
      IRET = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('MXSEL: COMOFF ERROR',I3,' FIELD ',I3,' CHANNEL',I5)
 1010 FORMAT ('MXSEL: ERROR! ',A,' CHANNEL ',I4,' > MAX=',I4)
 1020 FORMAT ('MXSEL: Warning! Averaging only channels ',I4,' to ',I4)
 1030 FORMAT ('MXSEL: OPEN ERROR ',I3,' ON ',A4)
 1050 FORMAT ('MXSEL: INIT FOR ',A4,' ERROR ',I3)
 1100 FORMAT ('MXSEL: ',A4,' ERROR ',I3)
 1210 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1211 FORMAT ('**** Use cellsize <',F10.5,' not',F10.5,
     *   ' to get all data ****')
 1212 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
 1220 FORMAT ('Using ONLY ',I8,' of ',I8,' Vis. < ',1PE9.2,' Lambdas')
 1221 FORMAT ('Using all ',I9,' Visibilities')
 1240 FORMAT ('MXSEL: ERROR',I4,' UPDATING UV WORK HEADER')
      END
      SUBROUTINE MXBEAM (IRET)
C-----------------------------------------------------------------------
C   MXBEAM makes the current beam, fits a gaussian if necessary and
C   fills the beam histogram.
C  Output:  IRET  I    Return error code, 0=>Ok, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET, J, IERR, NAP
      LOGICAL   HERM, T
      REAL      DUM1, DUM2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA HERM, T /.TRUE.,.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Make grid.
      CHUV1 = 1
      NGRDAT = T
C                                       Choose Gridding method by sort
      IF (ISORT(1:1) .EQ. 'X') THEN
C                                       Use X* sort technique
         CALL UVGRID (-1, SCRWRK, DISK2, CNOSCR, SCRGRD,
     *      CATSCR, JBUFSZ, FRQSEL, BUFF1, BUFF2, BUFF3, IRET)
      ELSE
C                                       Do unsorted gridding
         CALL UVGRTB (-1, SCRWRK, DISK2, CNOSCR, SCRGRD,
     *      CATSCR, JBUFSZ, FRQSEL, BUFF1, BUFF2, BUFF3, IRET)
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       FFT
      CALL QINIT (0, 0, NAP)
      IF ((NAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXBEAM: UANBLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
      CALL DSKFFT (NXBEM(1), NYBEM(1), -1, HERM, SCRGRD, SCRWRK, SCRGRD,
     *   JBUFSZ, BUFF1, BUFF2, DUM1, DUM2, IRET)
      CALL QRLSE
      IF (IRET.NE.0) GO TO 999
C                                       Gridding correct/histogram
      CALL QINIT (0, 0, NAP)
      IF ((NAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXBEAM: UANBLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
      CALL MXBHIS (IRET)
      CALL QRLSE
      IF (IRET.NE.0) THEN
         MSGTXT = 'MXBEAM: WARNING ! Bad Beam Value.'
         CALL MSGWRT(5)
         MSGTXT = 'MXBEAM: Try Smaller Cell Size.  Continuing'
         CALL MSGWRT(5)
         IRET = 0
         END IF
C                                       Fit gaussian
      CALL MXGFIT (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Timing message
      CALL MXTIMR ('Beam made   ')
C                                       Save clean map catalog
C                                       header.
      CALL RCOPY (256, CATR, BUFF3)
C                                       Update catalog header.
      CALL CATIO ('READ', DISOUT, CNOBEM(1), CATBLK, 'REST', BUFF1,
     *   IERR)
      IF ((IERR.EQ.0) .OR. (IERR.GT.4)) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
 10   CATR(KRDMX) = BMMAX
      CATR(KRDMN) = BMMIN
      CALL CATIO ('UPDT', DISOUT, CNOBEM(1), CATBLK, 'REST', BUFF1,
     *   IERR)
      IF ((IERR.EQ.0) .OR. (IERR.GT.4)) GO TO 20
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
C                                       Restore clean map header.
 20   CALL COPY (256, BUFF3, CATR)
C                                       Remove any destroy-on-failure
C                                       flag.
      DO 30 J=1,NCFILE
         IF ((CNOBEM(1).EQ.FCNO(J)) .AND. (FVOL(J).EQ.DISOUT))
     *      FRW(J) = 1
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXBEAM: ERROR',I3,' UPDATING BEAM CATALOG HEADER')
      END
      SUBROUTINE MXMAP (IRET)
C-----------------------------------------------------------------------
C   MXMAP makes the current residual, normalizes and corrects the map
C   to the correct scratch file plane and updates the pixel histogram
C   distribution.  Also keeps track of the current RESMAX using the
C   maximum value from DSKFFT.  Note: this may change during the
C   computation of the histogram if more than one subfield is being
C   used.
C  Output:  IRET    I    Return error code, 0=>Ok, otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER PREMAX*5, PREMIN*5
      INTEGER   IRET, IFIELD, J, KAP
      LOGICAL   HERM, LERR, T
      REAL      DUM1, DUM2, XFMAX, XFMIN, TEST, CLNMAX
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA HERM /.TRUE./
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       Loop over fields.
      DO 100 IFIELD = 1,MFIELD
C                                       Make grid.
         CHUV1 = 1
         NGRDAT = T
C                                       Choose Gridding method by sort
         IF (ISORT(1:1) .EQ. 'X') THEN
C                                       Use X* sort technique
            CALL UVGRID (IFIELD, SCRWRK, DISK2, CNOSCR, SCRGRD,
     *         CATSCR, JBUFSZ, FRQSEL, BUFF1, BUFF2, BUFF3, IRET)
         ELSE
C                                       Do unsorted gridding
            CALL UVGRTB (IFIELD, SCRWRK, DISK2, CNOSCR, SCRGRD,
     *         CATSCR, JBUFSZ, FRQSEL, BUFF1, BUFF2, BUFF3, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
C                                       FFT
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'MXMAP: UANBLE TO GET AP MEMORY BACK'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 999
            END IF
         CALL DSKFFT (FLDSZ(1,IFIELD), FLDSZ(2,IFIELD), -1, HERM,
     *      SCRGRD, SCRWRK, SCRGRD, JBUFSZ, BUFF1, BUFF2,
     *      DUM1, DUM2, IRET)
         CALL QRLSE
         IF (IRET.NE.0) GO TO 999
C                                       If first cycle update RESMAX.
         DUM1 = MAX (ABS (DUM1), ABS(DUM2)) / BEMMAX(1)
         IF ((FSTCLN) .AND. (IFIELD.EQ.1)) RESMAX = MAX (RESMAX, DUM1)
C                                       gridding correct/histogram
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'MXMAP: UANBLE TO GET AP MEMORY BACK'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 999
            END IF
         CALL MXRHIS (IFIELD, CLNMAX, IRET)
         CALL QRLSE
C                                       Make sure RESMAX OK for field 1.
         TEST = ABS (RESMAX) / (CLNMAX)
C                                       RESMAX should not be within 0.2
C                                       of the clean window maximum.
         IF (((.NOT.FSTCLN) .OR. (IFIELD.GT.1)) .OR.
     *      ((TEST.GT.0.8).AND.(TEST.LT.1.2))) GO TO 40
C                                       Redo gridding correct/histogram
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'MXMAP: UANBLE TO GET AP MEMORY BACK'
               CALL MSGWRT (8)
               IRET = 10
               GO TO 999
               END IF
            RESMAX = CLNMAX
            CALL MXRHIS (IFIELD, CLNMAX, IRET)
            CALL QRLSE
 40      IF (IRET.NE.0) GO TO 999
C                                       Max, min messages.
         XFMAX = FLDMAX(IFIELD)
         XFMIN = FLDMIN(IFIELD)
         CALL METSCA (XFMAX, PREMAX, LERR)
         CALL METSCA (XFMIN, PREMIN, LERR)
         WRITE (MSGTXT,1000) IFIELD, XFMIN, PREMIN, XFMAX, PREMAX
         CALL MSGWRT (4)
C                                       Update CATBLK.
         IRET = 0
         CALL CATIO ('READ', DISOUT, CCCNO(IFIELD), CATBLK, 'REST',
     *      BUFF1, IRET)
         IF ((IRET.EQ.0) .OR. (IRET.GT.4)) GO TO 50
            WRITE (MSGTXT,1001) IRET, IFIELD
            CALL MSGWRT (6)
            IRET = 0
            GO TO 60
 50      CATR(KRDMX) = MAX (GMAX(IFIELD), FLDMAX(IFIELD))
         CATR(KRDMN) = MIN (GMIN(IFIELD), FLDMIN(IFIELD))
         CATBLK(KINIT) = NCLNG(IFIELD)
         CALL CATIO ('UPDT', DISOUT, CCCNO(IFIELD), CATBLK, 'REST',
     *      BUFF1, IRET)
         IF ((IRET.LE.0) .OR. (IRET.GT.4)) GO TO 60
            WRITE (MSGTXT,1050) IRET, IFIELD
            CALL MSGWRT (6)
            IRET = 0
C                                       Remove any destroy-on-failure
C                                       flags
 60      DO 90 J=1,NCFILE
            IF ((CCCNO(IFIELD).EQ.FCNO(J)) .AND. (FVOL(J).EQ.DISOUT))
     *         FRW(J) = 1
 90         CONTINUE
 100     CONTINUE
C                                       Timing message
      CALL MXTIMR ('Maps made   ')
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Field',I5,' min = ',F7.2,1X,A5,'Jy, max =   ',F7.2,1X,A5,
     *   'Jy')
 1001 FORMAT ('MXMAP: ERROR',I3,' READING CATBLK FIELD',I3)
 1050 FORMAT ('MXMAP: ERROR',I3,' UPDATING CATBLK FIELD',I3)
      END
      SUBROUTINE MXDISP (TVPASS)
C-----------------------------------------------------------------------
C   MXDISP displays the current residual map of field number
C   TVFLD on the TV, showing inner portion only if that's
C   all that will fit.
C   Inputs:  TVPASS  I     code: 0,1 => clear screen, else don't
C                                0,3 => don't question the user about
C                                quitting
C   Output:  TVPASS  I     code: 32700 => user wants to quit cleaning
C-----------------------------------------------------------------------
      CHARACTER  PREFIX*5, FILE*48
      HOLLERITH SCATH(256)
      INTEGER   TVPASS, WIN(4), MY, FIND, BIND, IERR,
     *   ICH, S2H(256), IQ, IB, I, MX2, MX, KAP
      INTEGER   IWIN(4), IY
      REAL      XN(4), SCATR(256), TD, RPOS(2), XFLUX, TVLMAX, TVLMIN
      LOGICAL   MAP, EXCL, WAIT, LERR, F
      DOUBLE PRECISION SCATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   JROW(MAXIMG)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (JROW(1), BUFF1(1))
      EQUIVALENCE (S2H, SCATR, SCATH, SCATD, BUFF1(MAXIMG+1))
      DATA MAP, EXCL, WAIT / .TRUE., 2*.TRUE./
C-----------------------------------------------------------------------
      ICH = 1
      IF (TVFLD.LE.0) GO TO 999
C                                       Grab AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXDISP: UANBLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL TVOPEN (BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
      IF (TVPASS.LE.1) THEN
         CALL TVSET (ICH, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            CALL MSGWRT (6)
            GO TO 998
            END IF
         END IF
      IF (TVFMAX.LE.TVFMIN) THEN
         TVFMAX = FLDMAX(TVFLD)
         TVFMIN = FLDMIN(TVFLD)
         END IF
      IF (TVFMAX.LE.TVFMIN) THEN
         TVFMAX = ABS(FLDMAX(TVFLD))
         TVFMIN = -ABS(FLDMIN(TVFLD))
         END IF
      TVLMAX = TVFMAX - TVFMIN
      IF (0.333*TVLMAX.GT.FLDMAX(TVFLD)-FLDMIN(TVFLD)) THEN
         TVFMIN = FLDMIN(TVFLD)
         TVFMAX = FLDMAX(TVFLD)
         END IF
      IF (FLDMAX(TVFLD).GT.TVFMAX) TVFMAX = FLDMAX(TVFLD)
      IF (FLDMIN(TVFLD).LT.TVFMIN) TVFMIN = FLDMIN(TVFLD)
      IF (FLDMAX(TVFLD)*3.0.LT.TVFMAX) TVFMAX = FLDMAX(TVFLD)
      IF (FLDMIN(TVFLD)*3.0.GT.TVFMIN) TVFMIN = FLDMIN(TVFLD)
      TVLMAX = TVFMAX - TVFMIN
      XN(1) = TVFMIN
      XN(2) = TVFMAX
      XN(3) = (MAXINT - 0.02) / TVLMAX
      XN(4) = .51 - TVFMIN * XN(3)
      CALL QPUT (XN, 0, 4, 2)
C                                       Write scaling factor
      XFLUX = TVLMAX
      CALL METSCA (XFLUX, PREFIX, LERR)
      TVLMIN = TVFMIN * XFLUX / TVLMAX
      TVLMAX = TVFMAX * XFLUX / TVLMAX
      WRITE (MSGTXT,1020) TVLMIN, TVLMAX, PREFIX
      CALL MSGWRT (1)
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = FLDSZ(1,TVFLD)
      WIN(4) = FLDSZ(2,TVFLD)
      DO 70 I = 1,2
         IWIN(I) = (MAXXTV(I) - WIN(I+2) + WIN(I) + 1)/2
         IF (IWIN(I).GE.1) GO TO 50
            IWIN(I) = 1
            WIN(I)  = (WIN(I+2) + WIN(I) - MAXXTV(I) + 1)/2
            GO TO 60
 50      IWIN(I+2) = IWIN(I) + WIN(I+2) - WIN(I)
         IF (IWIN(I+2).LE.MAXXTV(I)) GO TO 70
 60      IWIN(I+2) = MAXXTV(I)
         WIN(I+2) = WIN(I) + IWIN(I+2) - IWIN(I)
 70   CONTINUE
C                                       Prepare to read map.
      CALL ZPHFIL ('MA', DISOUT, CCCNO(TVFLD), 1, FILE, IERR)
      CALL ZOPEN (LUNS(6), FIND, DISOUT, FILE, MAP, EXCL, WAIT, IERR)
      CALL MINIT ('READ', LUNS(6), FIND, FLDSZ(1,TVFLD), FLDSZ(2,TVFLD),
     *   WIN, BUFF2, JBUFSZ, BORES(TVFLD), IERR)
      MX = WIN(3) - WIN(1) + 1
      MY =  WIN(4) - WIN(2)+ 1
C                                       loop, passing map to TV.
      DO 100 I = 1,MY
         IY = I + IWIN(2) - 1
         CALL MDISK ('READ', LUNS(6), FIND, BUFF2, BIND, IERR)
         IF (IERR.NE.0) GO TO 110
         CALL QPUT (BUFF2(BIND), 4, MX, 2)
         CALL QWD
         CALL QVCLIP (4, 1, 0, 1, 4, 1, MX)
         CALL QVSMSA (4, 1, 2, 3, 4, 1, MX)
         CALL QVFIX (4, 1, 4, 1, MX)
         CALL QWR
         CALL QGET (JROW, 4, MX, 1)
         CALL QWD
C                                       Send row to IIS.
         MX2 = MX
         CALL YIMGIO ('WRIT', ICH, IWIN, IY, 0, MX2, JROW, IERR)
         IF (IERR.NE.0) GO TO 110
 100     CONTINUE
 110  CALL ZCLOSE (LUNS(6), FIND, IERR)
C                                       Release the AP
      CALL QRLSE
C                                       Image catalog
      CALL COPY (256, CATTV, S2H)
      S2H(IIVOL) = 0
      S2H(IICNO) = 0
      CALL FILL (5, 1, S2H(IIDEP))
      CALL COPY (4, IWIN, S2H(IICOR))
      CALL COPY (4, WIN, S2H(IIWIN))
      CALL CHR2H (2, '  ', 1, SCATH(IITRA))
      SCATR(IRRAN) = TVFMIN
      SCATR(IRRAN+1) = TVFMAX
      SCATR(KRDMN) = FLDMIN(TVFLD)
      SCATR(KRDMX) = FLDMAX(TVFLD)
      CALL YCWRIT (ICH, IWIN, S2H, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1110) IERR
         CALL MSGWRT (6)
         END IF
C                                       Ask user to quit?
      IF ((TVPASS.EQ.0) .OR. (TVPASS.EQ.3)) GO TO 998
         MSGTXT = 'Hit button D within 15 seconds to stop cleaning now'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A, B, or C to continue sooner'
         CALL MSGWRT (1)
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2.0
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2.0
         TD = 0.2
         CALL YCURSE ('ONNN', F, F, RPOS, IQ, IB, IERR)
         IF (IERR.NE.0) GO TO 998
         DO 130 I = 1,75
            CALL ZDELAY (TD, IERR)
            CALL YCURSE ('READ', F, F, RPOS, IQ, IB, IERR)
            IF (IB.GT.7) GO TO 140
            IF (IB.GT.0) GO TO 135
            IF (IERR.NE.0) GO TO 135
 130        CONTINUE
 135     MSGTXT = 'Continuing'
         CALL MSGWRT (1)
         GO TO 150
C                                       Wants to quit
 140     TVPASS = 32700
         MSGTXT = 'TV button D hit: have done enough I guess'
         CALL MSGWRT (3)
C                                       Off cursor
 150     CALL YCURSE ('OFFF', F, F, RPOS, IQ, IB, IERR)
 998  CALL TVCLOS (BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CAN''T OPEN TV IER=',I6)
 1010 FORMAT ('IMCLEAR ERROR =',I6)
 1020 FORMAT ('MXDISP: Display range =',2F10.3,1X,A5,'Jy')
 1110 FORMAT ('CAN''T UPDATE IMAGE CATALOG  IER=',I6)
      END
      SUBROUTINE MXCLN (IRET)
C-----------------------------------------------------------------------
C  MXCLN loads the beam patch and residual points into the AP and
C  does a limited CLEAN.  Fields are identified in this limited CLEAN
C  by adding an offset to the apparent grid position.
C   X offset = 4096 * (mod (IFIELD-1, 4))
C   Y offset = 4096 * ((IFIELD-1) / 4)
C  Also the CLEAN components are found 32 at a time.
C   Output:   IRET  I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET, KAP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Determine PATCH and MAPLIM
      CALL MDECID
C                                       Load beam patch.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXCLN: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
      CALL MXBSHV (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Load residuals.
      CALL MXMPAC (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Find components.
      CALL MXACLN (IRET)
      CALL QRLSE
      IF (IRET.NE.0) GO TO 999
C                                       Timing message
      CALL MXTIMR ('CLEAN cycle ')
C
 999  RETURN
      END
      SUBROUTINE MXSUB (DOSUM, IRET)
C-----------------------------------------------------------------------
C   MXSUB set up for calls to subtract components.
C   All un subtracted data processed in one call.
C   Inputs:
C      DOSUM       L     If true sum the flux in each field
C      JBUFSZ      I      Size of buffer of vis I/O.
C   Output:
C      IRET        I      Return code, 0 => ok, otherwise not.
C-----------------------------------------------------------------------
      INTEGER   IRET, CHANXX, IFF, IERR, ICTEMP
      REAL      SCRR(256), TIMDFT, TIMFFT
      DOUBLE PRECISION SCRD(128)
      LOGICAL   DOSUM, DOMSG, T, F, DODFT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      EQUIVALENCE (CATSCR, SCRR, SCRD)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check if subraction wanted
      IF (SKPSUB) GO TO 300
C                                       Set up for subtraction
      DOMSG = F
      CHANXX = 1
      FACGRD(1) = 1.0
      FACGRD(2) = 1.0
      CCROT = 1.0
      SSROT = 0.0
      NGRDAT = T
      COMPDT = .FALSE.
      DATDIV = .FALSE.
      ICTEMP = ICOR0
      ICOR0 = SCRD(KDCRV+JLOCS) + (1.0-SCRR(KRCRP+JLOCS))*
     *   SCRR(KRCIC+JLOCS) + 0.1
C                                       Check method
      CALL UVMTYP (0, METHOD, NCHAVG, DODFT, TIMDFT, TIMFFT)
C                                       DFT model.
      IF (.NOT.DODFT) GO TO 100
C                                       Subtract model.
         CALL VISDFT ('SUB ', CHANXX, NCHAVG, DISK2, CNOSCR, DISK2,
     *      CNOSCR, 0, DOSUM, DOMSG, CATSCR, JBUFSZ, BUFF1, BUFF2,
     *      BUFF3,  IRET)
         GO TO 200
C                                       Gridded interpolation method.
 100  MSGTXT = 'Gridded-interpolation model computation'
      CALL MSGWRT (4)
C                                       Gridded-interpolation
      DOFFT = F
      CALL GRDSUB (DOSUM, SCRGRD, SCRWRK, DISK2, CNOSCR, DISK2, CNOSCR,
     *   0, CHANXX, NCHAVG, CATSCR, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
C                                       Check errors
 200  IF (IRET.EQ.0) GO TO 250
         WRITE (MSGTXT,1200) IRET
         GO TO 990
C                                       Timing message
 250  CALL MXTIMR ('Comps subbed')
C                                       If necessary sum no. comps.
      IF (CLNSTR.GT.0) GO TO 270
         DO 260 IFF = 1,MFIELD
            CLNSTR = CLNSTR + NCLNG(IFF)
 260        CONTINUE
 270  CONTINUE
      ICOR0 = ICTEMP
C                                       Update uv work header
      CATSCR(KINIT) = CLNSTR
      CALL CATIO ('UPDT', DISK2, CNOSCR, CATSCR, 'REST', BUFF1, IERR)
      IF ((IERR.EQ.0) .OR. (IERR.GT.4)) GO TO 300
         WRITE (MSGTXT,1270) IERR
         CALL MSGWRT (6)
C                                       Subtract next time
 300  SKPSUB = F
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('MXSUB: ERROR',I3,' SUBTRACTING COMPONENTS')
 1270 FORMAT ('MXSUB: ERROR',I3,' UPDATING UV WORK CATBLK')
      END
      SUBROUTINE MXSTAT
C-----------------------------------------------------------------------
C  MXSTAT gives some statistics about the result of CLEANing a
C  frequency channel.  Also gets field max/min
C-----------------------------------------------------------------------
      CHARACTER PREFIX*5, PREMIN*5
      INTEGER   IF, IOCNT(2), ITIME(3)
      REAL      TIME, XFLUX, XXMIN
      LOGICAL   LERR
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Give time info.
      CALL ZTIME (ITIME)
      CALL ZCPU (TIME, IOCNT)
      WRITE (MSGTXT,1000) ICHAN, ITIME, TIME
      CALL MSGWRT (2)
C                                       Total flux density
      XFLUX = TFLUXG
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1001) XFLUX, PREFIX
C                                       IF map was cleaned
      IF (TFLUXG .NE. 0.0) CALL MSGWRT (5)
C                                       Loop thru subfields.
      DO 100 IF = 1,MFIELD
C                                       Get field global max/min
         GMAX(IF) = MAX (GMAX(IF), FLDMAX(IF))
         GMIN(IF) = MIN (GMIN(IF), FLDMIN(IF))
         XFLUX = GMAX(IF)
         CALL METSCA (XFLUX, PREFIX, LERR)
         XXMIN = GMIN(IF)
         CALL METSCA (XXMIN, PREMIN, LERR)
         WRITE (MSGTXT,1002) IF, XXMIN, PREMIN, XFLUX, PREFIX
         CALL MSGWRT (4)
 100     CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('***** Channel ',I4,' done ****:',I3.2,2(':',I2.2),
     *   ' CPU time=',F10.2,' *****')
 1001 FORMAT ('Total CLEANed flux density          =  ',F8.2,1X,A5,'Jy')
 1002 FORMAT ('Field',I5,' min = ',F7.2,1X,A5,'Jy, max =  ',F8.2,1X,A5,
     *   'Jy')
      END
      SUBROUTINE MXREST (IRET)
C-----------------------------------------------------------------------
C  MXREST computes the CLEAN components convolved with the restoring
C  beam and adds it to each residual image.  Sums total clean flux.
C   Output: IRET  I    Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET, IF, KAP
      LOGICAL   HERM
      REAL      DUM1, DUM2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA HERM /.TRUE./
C-----------------------------------------------------------------------
C                                       force full TVload range
      TVFMAX = 0.0
      TVFMIN = 0.0
      TFLUXG = 0.0
C                                       Loop over fields.
      DO 100 IF = 1,MFIELD
C                                       Zero flux
         FLUXG(IF) = 0.0
C                                       Make sure that there are
C                                       components.
         IF (NCLNG(IF).LE.0) GO TO 100
C                                       Grid transform of restoring
C                                       beam.
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'MXREST: UNABLE TO GET AP MEMORY BACK'
               CALL MSGWRT (8)
               IRET = 10
               GO TO 999
               END IF
            CALL MXRGRD (IF, IRET)
            CALL QRLSE
C                                       Check if CC found.
            IF (IRET.EQ.10) GO TO 100
            IF (IRET.NE.0) GO TO 999
C                                       FFT
            CALL QINIT (0, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
               MSGTXT = 'MXREST: UNABLE TO GET AP MEMORY BACK'
               CALL MSGWRT (8)
               IRET = 10
               GO TO 999
               END IF
            CALL DSKFFT (FLDSZ(1,IF), FLDSZ(2,IF), -1, HERM,
     *         SCRGRD, SCRWRK, SCRGRD, JBUFSZ, BUFF1, BUFF2,
     *         DUM1, DUM2, IRET)
            CALL QRLSE
            IF (IRET.NE.0) GO TO 999
C                                       Add to residual
            CALL MXADD (IF, IRET)
            IF (IRET.NE.0) GO TO 999
 100        CONTINUE
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE MXRGRD (IFIELD, IRET)
C-----------------------------------------------------------------------
C   MXRGRD restores CLEAN components to GRD file.  This is done by
C   transforming the CLEAN components, multiplying by the appropriate
C   Gaussian function and placing in the the GRD file.
C   INPUT:
C     IFIELD I   = FIELD number for grid.
C     CLNSTR I   = highest number component to transform
C     CLEAN components for field IFIELD.
C  OUTPUT:
C     Transform of additional CLEAN components times the restoring fn
C     left in GRD file.
C-----------------------------------------------------------------------
      INTEGER   IFIELD, IRET
C
      INTEGER   FLIST(22,2), NLOAD, I, JLIM, NX, NY, IT1,
     *   NAPGAU, NAPEX1, NAPEXP, NAPRES, NAPGRD, NAPCMP, NAPCMS, NUMBER,
     *   APBUF, ONENY, TWONY, WRK1, WRK2, FIRST, NUMCLN, MAXCMP, NUMBR4,
     *   JNUM, IAPSIZ
      LOGICAL   T, F, WFIRST
      REAL      TEMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA APBUF /1/
      DATA T, F  /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      WFIRST = T
      NX = FLDSZ(1,IFIELD)
      NY = FLDSZ(2,IFIELD)
      IAPSIZ = 1024 * PSAPNW
C                                       Determine Area assignments in AP
      ONENY = NY
      TWONY = 2*NY
      NAPRES = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPEXP = NAPRES + WRK1
      NAPGRD = NAPEXP + WRK1
      NAPCMP = NAPGRD + WRK2
      NAPGAU = NAPCMP + WRK2
      NAPEX1 = NAPGAU + WRK1
      NAPCMS = NAPEX1 + WRK1
C                                       Set up for APIO
C                                       Init.
      CALL FILL (22, 0, FLIST)
C                                       LUNs
      FLIST(1,1) = LUNS(4)
C                                       /CFILES/ numbers
      FLIST(2,1) = SCRGRD
C                                       Size
      FLIST(5,1) = NY * 2
      FLIST(6,1) = NX / 2 + 1
C                                       Buffer size
      FLIST(13,1) = JBUFSZ
C                                       Second file
      CALL COPY (22, FLIST(1,1), FLIST(1,2))
      FLIST(1,2) = LUNS(5)
      FLIST(2,2) = SCRWRK
C                                       Determine maximum number of comp
C                                       per pass.
      MAXCMP = (IAPSIZ - NAPCMS) / 5
      NUMCLN = NCLNG(IFIELD)
      FIRST = 1
      APBUF = 2
C                                       Determine size of AP buffer for
      NLOAD = NAPGAU - 3
C                                       Prepare work arrays in AP.
      CALL MXGAUS (IFIELD)
C                                       Roll AP if necessary
      CALL QROLL (NAPCMS, BUFF3, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Begin component loop.
 10   CONTINUE
         NUMBR4 = MAXCMP
         IF (NUMCLN.LT.NUMBR4) NUMBR4 = NUMCLN
         NUMBER = NUMBR4
C                                       Load CLEAN components this pass.
         JNUM = NUMBR4
         CALL MXCCRM (IFIELD, T, F, NAPCMS, APBUF, FIRST, NUMBR4,
     *      NLOAD, NX, NY, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Prepare NAPEXP using weight left
C                                       in AP loc 0 by MXGAUS.
         CALL QVCLR (NAPEXP, 1, ONENY)
         CALL QVSADD (NAPEXP, 1, 0, NAPEXP, 1, ONENY)
C                                       Clear NAPGRD
         CALL QVCLR (NAPGRD, 1, TWONY)
         CALL QWR
C                                       Begin loop thru map.
         JLIM = NX / 2 + 1
         DO 150 I = 1,JLIM
C                                        Generate restoring factors.
            TEMP = (-0.5 * GAUSCC * (I - 1.0) ** 2 )
            CALL QPUT (TEMP , 1, 1, 2)
            CALL QWAIT
            CALL QVSADD (NAPGAU, 1, 1, NAPRES, 1, ONENY)
            CALL QVADD (NAPRES, 1, NAPEXP, 1, NAPRES, 1, ONENY)
            CALL QVEXP (NAPRES, 1, NAPRES, 1, ONENY)
C                                       Prepare NAPEXP for next pass.
            CALL QVADD (NAPEXP, 1, NAPEX1, 1, NAPEXP, 1, ONENY)
            CALL QWR
C                                       Clear NAPCMP.
            CALL QVCLR (NAPCMP, 1, TWONY)
C                                       Do direct transform in X.
            CALL QDIRAD (NAPCMS, 5, NAPCMP, NUMBER)
            CALL QWR
C                                       Do FFT in Y.
            CALL QCFFT (NAPCMP, ONENY, 1)
            CALL QWR
C                                       If not first pass read old grid
            IF (WFIRST) GO TO 100
               CALL APIO ('READ', FLIST(1,1), NAPGRD, BUFF2, IRET)
               IF (IRET.EQ.0) GO TO 100
                  WRITE (MSGTXT,1080) IRET,I
                  GO TO 990
 100        CALL QWD
C                                       Multiply by restoring function.
            CALL QCRVMU (NAPCMP, 2, NAPRES, 1, NAPCMP, 2, ONENY)
            CALL QWR
C                                       Add to GRID file.
            CALL QVADD (NAPCMP, 1, NAPGRD, 1, NAPCMP, 1, TWONY)
            CALL QWR
C                                       Write file back to disk.
            CALL APIO ('WRIT', FLIST(1,2), NAPCMP, BUFF1, IRET)
            IF (IRET.EQ.0) GO TO 110
               WRITE (MSGTXT,1090) IRET, I
               GO TO 990
C                                       Rotate DFT for next pass.
 110        WRK1 = NAPCMS + 1
            WRK2 = NAPCMS + 3
            CALL QCVMUL (WRK1, 5, WRK2, 5, WRK1, 5, NUMBER, 1)
            CALL QWR
 150        CONTINUE
C                                       End of this pass, close files.
         CALL APIO ('CLOS', FLIST(1,1), NAPGRD, BUFF2, IRET)
         IF (IRET.EQ.0) GO TO 170
            WRITE (MSGTXT,1160) IRET
            CALL MSGWRT (8)
 170     CALL APIO ('CLOS', FLIST(1,2), NAPCMP, BUFF1, IRET)
         IF (IRET.EQ.0) GO TO 180
            WRITE (MSGTXT,1160) IRET
            CALL MSGWRT (8)
 180     FIRST = FIRST + JNUM
         NUMCLN = NUMCLN - JNUM
         WFIRST = F
C                                       Switch WRK and GRID files.
         IT1 = SCRGRD
         SCRGRD = SCRWRK
         SCRWRK = IT1
         FLIST(2,1) = SCRGRD
         FLIST(2,2) = SCRWRK
C                                       Check if time to roll AP
         IF (FIRST.LE.NCLNG(IFIELD)) THEN
            CALL QROLL (IAPSIZ, BUFF3, JBUFSZ, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Check for more CLEAN components.
         IF (FIRST.LE.NCLNG(IFIELD)) GO TO 10
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('MXRGRD: WRITE ERROR ',I3,' GRID ROW ',I6)
 1080 FORMAT ('MXRGRD: ERROR ',I3,' READ GRID ROW ',I6)
 1160 FORMAT ('MXRGRD: ERROR ',I3,' CLOSING GRID FILE')
      END
      SUBROUTINE MXCCRM (IFIELD, DOSUM, DOSUB, APLO, APBUF, FIRST,
     *   NUMBER, NLOAD, NX, NY, IRET)
C-----------------------------------------------------------------------
C   MXCCRM loads CLEAN components for field IFIELD into the
C   Array Processor in preparation for transformation to the
C   data plane. If MXCCRM starts from the first component, the flux
C   density of the CLEAN components is summed.
C   The signs of the cell offsets are adjusted for flips made to
C   the map.
C      Can now handle CC split onto a number of CC extension files
C   if the clean is a single channel.
C   Input:
C     IFIELD I   = Current field number.
C     DOSUM  L   = If true sum flux densities.
C     DOSUB  L   = If true don't correct to center (NY/2+1,NY/2+1)
C     APLO   I   = AP start location for the component array.
C     APBUF  I   = AP start location for the buffer.
C     FIRST  I   = First CLEAN component to be loaded.
C     NUMBER I   = Number of CLEAN components to be loaded.
C     NLOAD  I   = AP buffer size.
C     NX,NY  I   = Size of image.
C   Output:
C     NUMBER  I     Number of clean components loaded.
C     CLEAN components loaded into the AP in blocks of 5 words
C     arrainged as follows:
C         0 = Y (integer form)
C         1 = FLUX * cos(UX)
C         2 = FLUX * sin(UX)
C         3,4 = cos, sin(X)
C     IRET     I    Return error code, 0=OK, 10=>no Comps found,
C                                       otherwise fatal error.
C-----------------------------------------------------------------------
      CHARACTER   KEYS(4)*8
      INTEGER   FIRST, NUMBER, IPOINT, ICOUNT, JCOUNT
      INTEGER   IFIELD, IERR, IRET, NREC, NLOAD, LIMIT,
     *   NCOUNT, I, LREC, LUNC, VER, NX, NY,
     *   NKEY, KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL
      INTEGER   APLOC, APLO, APBUF, APLOC1, APLOC3,
     *   APLOC4, NNCNT, APLOC2
      LOGICAL   DOSUM, DOSUB, F
      REAL      CFLUX(2048), TEMP(6), TWOPIX, RAC(2048), DECC(2048),
     *   RECORD(7), XCEN, YCEN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (BUFF2(1), CFLUX(1)),      (BUFF3(1), RAC(1)),
     *   (BUFF3(2049), DECC(1))
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ','TYPE OBJ'/
      DATA F /.FALSE./
      DATA LUNC /28/
C-----------------------------------------------------------------------
C                                       Determine beam center.
      XCEN = ICNTRX(IFIELD)
      YCEN = ICNTRY(IFIELD)
      XCEN = ((XCEN - FLDSZ(1,IFIELD)/2 - 1) * CELLSG(1))
     *    / 3600. + XPOFF(IFIELD)
      YCEN = ((YCEN - FLDSZ(2,IFIELD)/2 - 1) * CELLSG(2))
     *    / 3600. + YPOFF(IFIELD)
      IF (DOSUB) XCEN = XPOFF(IFIELD)
      IF (DOSUB) YCEN = YPOFF(IFIELD)
C                                       Open CLEAN component file.
      LREC = 3
      NREC = 1
      NKEY = 0
      JCOUNT = 0
      CALL CATIO ('READ', DISOUT, CCCNO(IFIELD), CATBLK, 'REST', BUFF1,
     *   IRET)
      IF ((IRET.EQ.0) .OR. (IRET.GT.4)) GO TO 5
         WRITE (MSGTXT,1000) IRET
         GO TO 990
 5    VER = CCVER(IFIELD)
      IF (VER.LT.1) VER = 1
      CALL TABINI ('READ', 'CC', DISOUT, CCCNO(IFIELD), VER, CATBLK,
     *   LUNC, NKEY, NREC, LREC, BUFF2, BUFF1, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1005) IRET
         GO TO 990
 10   CALL RFILL (7, 0.0, RECORD)
      NKEY = 3
      TYPKOL = 7
      FLXKOL = 1
      RAKOL = 2
      DECKOL = 3
      IF (LREC.GT.3) NKEY = 4
      CALL FNDCOL (NKEY, KEYS, 8, F, BUFF1, KOLS, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET, IFIELD
         CALL MSGWRT (7)
 20   APLOC = APLO
      ICOUNT = 0
      IPOINT = FIRST
C                                       Begin loading into computer buff
C                                       Make sure to load in small
C                                       enough pieces.
      LIMIT = JBUFSZ / 8
      IF (LIMIT.GT.NUMBER) LIMIT = NUMBER
      LIMIT = MIN (LIMIT, NLOAD, 2048)
C                                       Jump to here if more passes nece
 30   CONTINUE
         NCOUNT = 0
         DO 50 I = 1,LIMIT
            CALL TABIO ('READ', 0, IPOINT, RECORD, BUFF1, IRET)
            IF (IRET.LE.0) GO TO 40
               WRITE (MSGTXT,1030) IRET, IPOINT
               GO TO 990
C                                       Deal with component.
 40         IF (IRET.LT.0) GO TO 45
            IF ((LREC.GT.3) .AND. (RECORD(TYPKOL).NE.0.0)) GO TO 45
               NCOUNT = NCOUNT + 1
               CFLUX(NCOUNT) = RECORD(FLXKOL)
               RAC(NCOUNT) = RECORD(RAKOL) + XCEN
               DECC(NCOUNT) = RECORD(DECKOL) + YCEN
               IF (DOSUM) FLUXG(IFIELD) = FLUXG(IFIELD) + RECORD(FLXKOL)
               IF (DOSUM) TFLUXG = TFLUXG + RECORD(FLXKOL)
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
C                                       Close CLNFIL.
      CALL TABIO ('CLOS', 0, IPOINT, RECORD, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1070) IERR
         GO TO 990
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXCCRM: ERROR',I3,' READING CATBLK ')
 1005 FORMAT ('MXCCRM: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('MXCCRM: ERROR',I3,' FINDING REQUIRED CC COLUMNS FIELD',
     *   I5)
 1030 FORMAT ('MXCCRM: READ ERROR',I3,' RECORD ',I8)
 1070 FORMAT ('MXCCRM: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE MXOUT (IRET)
C-----------------------------------------------------------------------
C   MXOUT Updates map headers and writes history.
C    Output:  IRET  I    Return error code, 0=>OK otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(9)*2
      INTEGER   IRET, IFIELD, IERR, NONOT, LUN1, LUN2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NONOT /9/, LUN1, LUN2 /28,29/
      DATA NOTTYP /'CH','SU','AN','NX','FG','GA','CL','SN','FQ'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Update / write histories.
C                                       Loop over fields.
      DO 500 IFIELD = 1,MFIELD
C                                       Read CATBLK.
         CALL CATIO ('READ', DISOUT, CCCNO(IFIELD), CATBLK, 'REST',
     *      BUFF1, IRET)
         IF ((IRET.EQ.0) .OR. (IRET.GT.4)) GO TO 410
            WRITE (MSGTXT,1000) IRET, IFIELD
            CALL MSGWRT (8)
            GO TO 999
C                                       Fill in CLEAN header info
 410      CATBLK(KINIT) = NCLNG(IFIELD)
          CATR(KRBMJ) = BMAJ / 3600.
          CATR(KRBMN) = BMIN / 3600.
          CATR(KRBPA) = BPA
C                                       Map type.
         CATBLK(KITYP) = 1
         IF (.NOT.DOREST) CATBLK(KITYP) = 3
         IF ((BMAJ.GT.0.0) .AND. (BMAJ.LT.0.001*CELLSG(2)))
     *      CATBLK(KITYP) = 4
C                                       Max/min
         CATR(KRDMX) = GMAX(IFIELD)
         CATR(KRDMN) = GMIN(IFIELD)
C                                       Do HIstory
         CALL MXHIS (IFIELD)
         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISIN, DISOUT, CNOOLD,
     *      CCCNO(IFIELD), CATBLK, BUFF1, BUFF2, IERR)
C                                       Copy tables
 500     CONTINUE
C                                       BEAM history
      CALL CATIO ('READ', DISOUT, CNOBEM(1), CATBLK, 'REST', BUFF1,
     *   IRET)
      IF ((IRET.EQ.0) .OR. (IRET.GT.4)) GO TO 510
         WRITE (MSGTXT,1000) IRET, IFIELD
         CALL MSGWRT (8)
         GO TO 999
 510  CATR(KRDMX) = BMMAX
      CATR(KRDMN) = BMMIN
      CALL MXHIS (-1)
C                                       Reset NCFILE
C                                       Two files marked are input
C                                       and work uv data files.
      NCFILE = 2
      FCNO(NCFILE) = CNOSCR
      FVOL(NCFILE) = DISK2
      FRW(NCFILE) = 1
      IRET = 0
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXOUT: ERROR',I3,' READING CATBLK, FIELD',I6)
      END
      SUBROUTINE MXBHIS (IRET)
C-----------------------------------------------------------------------
C   MXBHIS determines the normalization constant, does the gridding
C   and normalization correction and determines the largest
C   exterior sidelobe as a function of the beam patch size.
C   Beam assumed in /CFILE/ location SCRGRD and left in BEAM file.
C   INPUT:
C        MAXPCH I   = maximum beam patch size to be considered.
C        NXBEM,NYBEM
C               I   = X and Y dimensions of the full beam map.(cells).
C        NUMBIN I   = Number of levels considered for the fraction
C                     of the peak.
C   OUTPUT:
C        BEMMAX R   = The normalization constant.
C        BMHIS  I   = An array whose elements have values between 1
C                     and NUMBIN + 1 indicating the maximum abs.
C                     exterior sidelobe for a beam patch whose size
C                     corresponds to the array index.
C                     e.g. for a beam patch of half size I the maximum
C                     fractional absolute sidelobe level exterior to the
C                     beam patch is BMHIS(I) / NUMBIN  .
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   FIND1, BIND1, IRET, BO, IPCH,
     *   I, ITEMP, J, JJ, LIMIT, MAXMUM, NXHAF, NYHAF, IT1, IT2, IT3,
     *   JCNTRX, JCNTRY, IY, JWIN(4), FIND2, BIND2, IMAP, IXC, IYC,
     *   IWORK, ONENX, ONENY, YCORR, KBLC(2), KTRC(2), IAPMAX, IAPMIN,
     *   KAP
      LOGICAL   MAP, EXCL, WAIT
      REAL      BIMAX, TEMP(3), TMPLST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      REAL      BMBUF(UVBFSS)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (BMBUF(1), BUFF3(1))
      DATA BO /1/
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      ONENX = NXBEM(1)
      ONENY = NYBEM(1)
      IMAP = 0
      IXC = IMAP + ONENX
      IYC = IXC + ONENX
      IWORK = IYC + ONENY
      IAPMIN = IWORK + 1
      IAPMAX = IAPMIN + 2
C                                       Compute Fourier Transform of
C                                       the gridding function in the AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXBHIS: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
C
      JCNTRX = NXBEM(1) / 2
      JCNTRY = NYBEM(1) / 2 + 1
      CALL GRDTAB (NXBEM(1), JCNTRX, CTYPX, XPARM, IXC, IWORK, BUFF1)
      CALL GRDTAB (NYBEM(1), JCNTRY, CTYPY, YPARM, IYC, IWORK, BUFF1)
C                                      Open beam file (read).
      CALL ZPHFIL ('SC', SCRVOL(SCRGRD), SCRCNO(SCRGRD), 1, FILE, IRET)
      CALL ZOPEN (LUNS(3), FIND1, SCRVOL(SCRGRD), FILE, MAP, EXCL, WAIT,
     *   IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                      Open beam file (write).
 10   CALL ZPHFIL ('MA', DISOUT, CNOBEM(1), 1, FILE, IRET)
      CALL ZOPEN (LUNS(4), FIND2, DISOUT, FILE, MAP, EXCL, WAIT, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Get center of beam.
 20   JWIN(1) = JCNTRX
      JWIN(2) = JCNTRY
      JWIN(3) = JWIN(1) + 1
      JWIN(4) = JWIN(2) + 2
      CALL MINIT ('READ', LUNS(3), FIND1, NXBEM(1), NYBEM(1), JWIN,
     *   BUFF1, JBUFSZ, BO, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   CALL MDISK ('READ', LUNS(3), FIND1, BUFF1, BIND1, IRET)
C                                       Get beam center value.
      BEMMAX(1) = BUFF1(BIND1)
C                                       Divide X gridding correction by
C                                       the peak of the beam to
C                                       normalize.
      IF (BEMMAX(1).LE.0.0) BEMMAX(1) = 1.0
      BIMAX = 1.0 / BEMMAX(1)
      CALL QPUT (BIMAX, IMAP, 1, 2)
      CALL QWD
      CALL QVSMUL (IXC, 1, IMAP, IXC, 1, ONENX)
C                                       Search entire beam map.
C                                       Center is at (NXBEM/2+1,
C                                       NYBEM/2+1).
      NYHAF = NYBEM(1) / 2 + 1
      NXHAF = NXBEM(1) / 2 + 1
      IT1 = NXBEM(1) / 2 - 1
      IT2 = NYBEM(1) / 2 - 1
      IT3 = MAXPCH + 1
      MAXPCH = MIN (IT1, IT2, IT3) - 1
C                                       Check to be sure MAXPCH is
C                                       permitted.
      MAXMUM = MIN (MAXPCH, IT1, IT2)
C                                       Set window for beam.
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = NXBEM(1)
      JWIN(4) = NYBEM(1)
C                                      Initialize beam map for read.
       CALL MINIT ('READ', LUNS(3), FIND1, NXBEM(1), NYBEM(1), JWIN,
     *    BUFF1, JBUFSZ, BO, IRET)
       IF (IRET.EQ.0) GO TO 40
          WRITE (MSGTXT,1010) IRET
          GO TO 990
C                                      Initialize beam map for write.
 40    CALL MINIT ('WRIT', LUNS(4), FIND2, NXBEM(1), NYBEM(1), JWIN,
     *    BUFF2, JBUFSZ, BOBEM(1), IRET)
       IF (IRET.NE.0) THEN
          WRITE (MSGTXT,1010) IRET
          GO TO 990
          END IF
C                                       Ignore outer beam pixels if
C                                       possible in histogram
      IF ((NXBEM(1).GT.2*MAXMUM) .AND. (NYBEM(1).GT.2*MAXMUM)) THEN
         KBLC(1) = 5
         KTRC(1) = NXBEM(1) - 5
         KBLC(2) = 5
         KTRC(2) = NYBEM(1) - 5
      ELSE
         KBLC(1) = 1
         KTRC(1) = NXBEM(1)
         KBLC(2) = 1
         KTRC(2) = NYBEM(1)
         END IF
C                                      Initialize BMHIS to zero.
      CALL RFILL (MAXMUM, 0.0, BMBUF)
      CALL QWR
C                                      Loop thru map looking for max.
C                                      abs. sidelobe level on each beam
C                                      patch boundary.
      DO 100 I = 1,NYBEM(1)
         IY = I - JCNTRY + 1
C                                      Read row of the map.
         CALL MDISK ('READ', LUNS(3), FIND1, BUFF1, BIND1, IRET)
         IF (IRET.EQ.0) GO TO 70
            WRITE (MSGTXT,1020) IRET,I
            GO TO 990
C                                      Write row back.
 70      CALL MDISK ('WRIT', LUNS(4), FIND2, BUFF2, BIND2, IRET)
         IF (IRET.EQ.0) GO TO 80
            WRITE (MSGTXT,1070) IRET,I
            GO TO 990
 80      CALL QPUT (BUFF1(BIND1), IMAP, ONENX, 2)
         YCORR = IYC + I - 1
         CALL QWD
C                                       Correct map for gridding and
C                                       normalize. X correct.
         CALL QVMUL (IMAP, 1, IXC, 1, IMAP, 1, ONENX)
C                                       Y correction.
         CALL QVSMUL (IMAP, 1, YCORR, IMAP, 1, ONENX)
C                                       Find max and min in row.
         CALL QMAXV (IMAP, 1, IAPMAX, ONENX)
         CALL QMINV (IMAP, 1, IAPMIN, ONENX)
C                                       Read corrected beam.
         CALL QWR
         CALL QGET (BUFF2(BIND2), IMAP, ONENX, 2)
         CALL QGET (TEMP, IAPMIN, 3, 2)
         CALL QWD
C                                       Save max and min.
         BMMAX = MAX (BMMAX, TEMP(3))
         BMMIN = MIN (BMMIN, TEMP(1))
C                                       Only do historgram on half
         IF ((IY.LT.0) .OR. (I.GT.KTRC(2))) GO TO 100
C                                       Loop down the row.
C                                       Contains vector dependencies
      INCLUDE 'INCS:ZVD.INC'
         DO 90 J = KBLC(1), KTRC(1)
            JJ = BIND2 + J - 1
C                                      Compute min. beam patch including
C                                      this pixel. Adjust the index so
C                                      that order in array gives the
C                                      largest EXTERIOR sidelobe.
            ITEMP = J - NXHAF
            ITEMP = ABS (ITEMP) - 1
            IPCH = MAX (IY, ITEMP, 1)
            IPCH = MIN (IPCH, MAXMUM)
C                                      Replace current value of
C                                      BMBUF(IPCH) if abs. value of
C                                      this pixel is larger.
            BMBUF(IPCH) = MAX (BMBUF(IPCH), ABS(BUFF2(JJ)))
 90         CONTINUE
 100     CONTINUE
C                                       Release the AP.
      CALL QRLSE
C                                      Flush output
      CALL MDISK ('FINI', LUNS(4), FIND2, BUFF2, BIND2, IRET)
      IF (IRET.EQ.0) GO TO 150
         WRITE (MSGTXT,1070) IRET,I
         GO TO 990
C                                      Close beam file.
 150  CALL ZCLOSE (LUNS(4), FIND2, IRET)
      CALL ZCLOSE (LUNS(3), FIND1, IRET)
      IF (IRET.EQ.0) GO TO 160
         WRITE (MSGTXT,1150) IRET
         CALL MSGWRT (8)
C                                      Make sure BMBUF for each beam
C                                      patch is.GE.that for larger
C                                      beam patches.
 160  LIMIT = MAXMUM - 1
      DO 170 I = 1,LIMIT
         J = MAXMUM - I + 1
         IF (BMBUF(J-1).LT.BMBUF(J)) BMBUF(J-1) = BMBUF(J)
C                                      Check that value. le. 1.0
         IF (ABS (BMBUF(J-1)) .LE. 1.05) GO TO 170
            WRITE (MSGTXT,1160) BMBUF(J-1)
            IRET = 6
            GO TO 990
 170     CONTINUE
C                                      Scale BMHIS to range (1,NUMBIN+1)
      TMPLST = BMBUF(1)
C                                       Propogate last non zero value.
      DO 180 I = 1,MAXPCH
         IF (BMBUF(I).GT.1.0E-5) THEN
            TMPLST = BMBUF(I)
         ELSE
            BMBUF(I) = TMPLST
            END IF
         BMHIS(I) = BMBUF(I) * NUMBIN + 1.5
 180     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXBHIS: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('MXBHIS: ERROR',I3,' INIT FILE ')
 1020 FORMAT ('MXBHIS: READ ERROR',I3,' ROW ',I5)
 1070 FORMAT ('MXBHIS: WRITE ERROR',I3,' ROW ',I5)
 1150 FORMAT ('MXBHIS: ERROR',I3,' CLOSING FILE ')
 1160 FORMAT ('MXBHIS: BAD BEAM VALUE =',1PE10.3)
      END
      SUBROUTINE MXGFIT (IRET)
C-----------------------------------------------------------------------
C   MXGFIT fits an eliptical Gaussian to the dirty beam
C   and checks that the peak of the beam is 1.0.  If peak of beam is
C   too narrow to fit a default circular Gaussian is used.
C   A grid of up to 5 X 11 points is used for the fit; only points
C   within the half power points are used.  To avoid degenerate
C   cases some of the allowed points are ignored.
C   Solution is by least squares to a linearized gaussian.
C   Beam R   map assumed in beam file.
C   INPUT:  The beam map centered at (NXBEM/2,NYBEM/2+1)
C     CELLSG(1)  R   Grid increment in RA (asec) (negative)
C     CELLSG(2)  R   Grid spacing in Dec (asec.)
C     MAPROT     R Coordinate rotation.
C   OUTPUT:
C     BMAJ      R    Major axis size (FWHP in sec).
C     BMIN      R    Minor axis size (FWHP in sec).
C     BPA       R    Position angle of major axis (degrees).
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      LOGICAL   MAP, EXCL, WAIT
      INTEGER   FIND, BIND, WIN(4), IRET, I, IFLIP, IJK, ILAST, IROW, J,
     *   K, L
      REAL      X(3,3), Y(3), P(3), DX, DY, XFACT, BMJLOC, BMNLOC,
     *   BPALOC
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MX.INC'
      REAL      BMAP(UVBFSS)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (BMAP(1), BUFF1(1))
      DATA MAP, EXCL, WAIT / .TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      IFLIP = 1
      XFACT = ABS (CELLSG(1))
C                                       Open beam file.
      CALL ZPHFIL ('MA', DISOUT, CNOBEM(1), 1, FILE, IRET)
      CALL ZOPEN (LUNS(3), FIND, DISOUT, FILE, MAP,
     *   EXCL, WAIT, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                       Zero work arrays.
 10   DO 20 I = 1,3
         Y(I) = 0.0
         DO 20 J = 1,3
            X(I,J) = 0.0
 20         CONTINUE
C                                       Set window,only need half map
      WIN(1) = NXBEM(1) / 2 - 5
      WIN(2) = NYBEM(1) / 2 + 1
      WIN(3) = NXBEM(1) / 2 + 5
      WIN(4) = NYBEM(1) / 2 + 5 + 1
      CALL MINIT ('READ', LUNS(3), FIND, NXBEM(1), NYBEM(1), WIN, BUFF1,
     *   JBUFSZ, BOBEM(1), IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                       Loop through rows.
 30   DO 70 I = 1,6
C                                       Read row.
         CALL MDISK ('READ', LUNS(3), FIND, BUFF1, BIND, IRET)
         IF (IRET.EQ.0) GO TO 40
            WRITE (MSGTXT,1030) IRET, I
            CALL MSGWRT (8)
            GO TO 999
C                                       Loop down row doing alternate
C                                       halves. go only to first
C                                       decending 0.35 from center.
 40      DO 65 IJK = 1,2
            IFLIP = - IFLIP
            ILAST = BIND + 5 - IFLIP
            DO 60 J = IJK,6
               IROW = BIND + 5 + (J-1) * IFLIP
               IF ((BUFF1(IROW).LT.0.35) .AND. (BUFF1(IROW).LT.
     *            BUFF1(ILAST))) GO TO 65
               IF (BUFF1(IROW).LT.0.35) GO TO 60
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
                     Y(K) = Y(K) - LOG (BUFF1(IROW)) * P(K)
                     DO 50 L = 1,3
                        X(K,L) = X(K,L) + P(K) * P(L)
 50                     CONTINUE
 60            CONTINUE
 65         CONTINUE
 70      CONTINUE
C                                       Fit beam
      CALL FITBM (0, X, Y, CELLSG, MAPROT, BMJLOC, BMNLOC, BPALOC)
C                                       If BMAJ is not set,
C                                       Record values
      IF (BMAJ.LE.0.0) THEN
         BMAJ = BMJLOC
         BMIN = BMNLOC
         BPA  = BPALOC
         END IF
C                                       Close
      CALL ZCLOSE (LUNS(3), FIND, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXGFIT: ERROR ',I3,' OPENING FILE ')
 1020 FORMAT ('MXGFIT: ERROR ',I3,' INIT FILE ')
 1030 FORMAT ('MXGFIT: READ ERROR ',I3,' ROW ',I5)
      END
      SUBROUTINE MXRHIS (IFIELD, CLNMAX, IRET)
C-----------------------------------------------------------------------
C   MXRHIS normalizes and gridding corrects a residual map
C   and determines the histogram of pixel values.
C   Map assumed in /CFILE/ location SCRGRD and in left in
C   the output image file at Block offset given in BORES(IFIELD)
C    Input:
C      IFIELD   I    The subfield number, if = 1 the histogram is
C                    zero filled first.
C    Input form common:
C      DOBOX      L    TRUE if boxes specified for field 1
C      NBOXES     I    Number of boxes given for field 1
C      WIN1(4,50) I    Boxes for Field 1
C    Output:
C      CLNMAX   R    The maximum absolute value in the clean window.
C      RESHIS   R    The histogram of the distribution of pixel
C                    values.
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   FIND1, BIND1, IRET, BO,  I, J, IFIELD, LIMIT,
     *   WWIN1, WWIN2, WWIN3, WWIN4, JWIN(4), FIND2, BIND2,
     *   IBOX, LIMBOX
      INTEGER   IMAP, IXC, IYC, IWORK, ONENX, ONENY, YCORR, PNUMIN,
     *   IAPHIS, ISTART, MX, IAPMIN, IAPMAX, IAPHMX, IAPHMN, KAP, IAPSIZ
      LOGICAL   MAP, EXCL, WAIT, DOBX
      REAL      CLNMAX, BIMAX, TEMP(5), TEMPMX
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
      DATA BO /1/
C-----------------------------------------------------------------------
C                                       If this is the TV field do
C                                       max/min.
      IF (IFIELD.EQ.TVFLD) TVREMX = -1.0E20
      IF (IFIELD.EQ.TVFLD) TVREMN = 1.0E20
      FLDMAX(IFIELD) = -1.0E20
      FLDMIN(IFIELD) =  1.0E20
      CLNMAX = -1.0E20
      ONENX = FLDSZ(1,IFIELD)
      ONENY = FLDSZ(2,IFIELD)
      IMAP = 0
      IXC = IMAP + ONENX
      IYC = IXC + ONENX
      IWORK = IYC + ONENY
      IAPMIN = IYC + ONENY
      IAPMAX = IAPMIN + 2
      IAPHMN = IAPMAX + 2
      IAPHMX = IAPHMN + 1
      IAPHIS = IAPHMX + 1
      PNUMIN = NUMBIN
      IAPSIZ = IAPHIS + PNUMIN
C                                       On first call zero histogram.
      IF (IFIELD.LE.1) CALL RFILL (NUMBIN, 0.0, RESHIS)
C                                       Compute Fourier Transform of
C                                       the gridding function in the AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MXRHIS: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IRET = 10
         GO TO 999
         END IF
C
      CALL GRDTAB (FLDSZ(1,IFIELD), ICNTRX(IFIELD), CTYPX, XPARM,
     *   IXC, IWORK, BUFF1)
      CALL GRDTAB (FLDSZ(2,IFIELD), ICNTRY(IFIELD), CTYPY, YPARM,
     *   IYC, IWORK, BUFF1)
C                                      Open grid file (read).
      CALL ZPHFIL ('SC', SCRVOL(SCRGRD), SCRCNO(SCRGRD), 1, FILE, IRET)
      CALL ZOPEN (LUNS(5), FIND1, SCRVOL(SCRGRD), FILE, MAP, EXCL, WAIT,
     *   IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                      Open residual file (write).
 10   CALL ZPHFIL ('MA', DISOUT, CCCNO(IFIELD), 1, FILE, IRET)
      CALL ZOPEN (LUNS(6), FIND2, DISOUT, FILE,
     *    MAP, EXCL, WAIT, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Divide X gridding correction by
C                                       the peak of the beam to
C                                       normalize.
 20   BIMAX = 1.0 / BEMMAX(1)
      CALL QPUT (BIMAX, IMAP, 1, 2)
      CALL QWD
C                                       Load Histogram max/min
      TEMP(1) = 0.0
      TEMP(2) = RESMAX
      CALL QPUT (TEMP, IAPHMN, 2, 2)
C                                       Load current histogram.
      CALL QPUT (RESHIS, IAPHIS, PNUMIN, 2)
      CALL QWD
      CALL QVSMUL (IXC, 1, IMAP, IXC, 1, ONENX)
C                                       Set window.
      JWIN(1) = 1
      JWIN(2) = 1
      JWIN(3) = FLDSZ(1,IFIELD)
      JWIN(4) = FLDSZ(2,IFIELD)
C                                      Initialize residual map for read.
       CALL MINIT ('READ', LUNS(5), FIND1, FLDSZ(1,IFIELD),
     *    FLDSZ(2,IFIELD), JWIN, BUFF1, JBUFSZ, BO, IRET)
       IF (IRET.EQ.0) GO TO 40
          WRITE (MSGTXT,1010) IRET
          GO TO 990
C                                      Initialize residual map for
C   write.
 40    CALL MINIT ('WRIT', LUNS(6), FIND2, FLDSZ(1,IFIELD),
     *    FLDSZ(2,IFIELD), JWIN, BUFF2, JBUFSZ, BORES(IFIELD), IRET)
       IF (IRET.EQ.0) GO TO 50
          WRITE (MSGTXT,1010) IRET
          GO TO 990
 50   CALL QWR
C                                       Setup for clean window(s)
      DOBX = DOBOX .AND. (IFIELD.EQ.1)
      WWIN1 = MIN (WINDOW(1,IFIELD), WINDOW(3,IFIELD))
      WWIN3 = MAX (WINDOW(1,IFIELD), WINDOW(3,IFIELD))
      WWIN2 = MIN (WINDOW(2,IFIELD), WINDOW(4,IFIELD))
      WWIN4 = MAX (WINDOW(2,IFIELD), WINDOW(4,IFIELD))
C                                       Loop thru map correcting for
C                                       the convolution function and
C                                       making the pixel histogram.
      LIMIT = FLDSZ(2,IFIELD)
      DO 100 I = 1,LIMIT
C                                      Read row of the map.
         CALL MDISK ('READ', LUNS(5), FIND1, BUFF1, BIND1, IRET)
         IF (IRET.EQ.0) GO TO 70
            WRITE (MSGTXT,1020) IRET,I
            GO TO 990
C                                      Write row back.
 70      CALL MDISK ('WRIT', LUNS(6), FIND2, BUFF2, BIND2, IRET)
         IF (IRET.EQ.0) GO TO 80
            WRITE (MSGTXT,1070) IRET,I
            GO TO 990
 80      CALL QPUT (BUFF1(BIND1), IMAP, ONENX, 2)
         YCORR = IYC + I - 1
         CALL QWD
C                                       Correct map for gridding and
C                                       normalize. X correct.
         CALL QVMUL (IMAP, 1, IXC, 1, IMAP, 1, ONENX)
C                                       Y correction.
         CALL QVSMUL (IMAP, 1, YCORR, IMAP, 1, ONENX)
C                                       Read corrected residual.
C                                       Find max and min in row.
         CALL QMAXV (IMAP, 1, IAPMAX, ONENX)
         CALL QMINV (IMAP, 1, IAPMIN, ONENX)
C                                       Read results.
         CALL QWR
         CALL QGET (BUFF2(BIND2), IMAP, ONENX, 2)
         CALL QGET (TEMP, IAPMIN, 3, 2)
         CALL QWD
C                                       Save max and min.
         FLDMAX(IFIELD) = MAX (FLDMAX(IFIELD), TEMP(3))
         FLDMIN(IFIELD) = MIN (FLDMIN(IFIELD), TEMP(1))
C                                       Compute histogram of residuals.
C                                       If field 1 and DOBOX - loop
         LIMBOX = 1
         IF (DOBX) LIMBOX = NBOXES
         DO 95 IBOX = 1,LIMBOX
            IF (.NOT.DOBX) GO TO 85
C                                       Set box
               WWIN1 = MIN (WIN1(1,IBOX), WIN1(3,IBOX))
               WWIN3 = MAX (WIN1(1,IBOX), WIN1(3,IBOX))
               WWIN2 = MIN (WIN1(2,IBOX), WIN1(4,IBOX))
               WWIN4 = MAX (WIN1(2,IBOX), WIN1(4,IBOX))
C                                       Make sure row goes thru window
 85         IF ((I.LT.WWIN2) .OR. (I.GT.WWIN4)) GO TO 95
               ISTART = IMAP + WWIN1 - 1
               MX = WWIN3 - WWIN1 + 1
C                                       Get max/min in window.
               CALL QMAXV (ISTART, 1, IAPMAX, MX)
               CALL QMINV (ISTART, 1, IAPMIN, MX)
               CALL QWR
               CALL QGET (TEMP, IAPMIN, 3, 2)
               CALL QWD
C                                       Get window MAX (ABS)
               TEMPMX = MAX (ABS (TEMP(1)), ABS (TEMP(3)))
               CLNMAX = MAX (CLNMAX, TEMPMX)
C                                       If this residual to be
C                                       displayed, get max/min
               IF (IFIELD.NE.TVFLD) GO TO 90
                  TVREMN = MIN (TVREMN, TEMP(1))
                  TVREMX = MAX (TVREMX, TEMP(3))
 90            CALL QVABS (ISTART, 1, ISTART, 1, MX)
               CALL QHIST (ISTART, 1, IAPHIS, MX, PNUMIN, IAPHMX,
     *            IAPHMN)
               CALL QWR
 95            CONTINUE
C                                       Check if time for roller
            IF ((MOD (I, 128).NE.0) .OR. (I.EQ.LIMIT)) GO TO 100
               CALL QROLL (IAPSIZ, BUFF3, JBUFSZ, IRET)
               IF (IRET.NE.0) GO TO 999
 100        CONTINUE
C                                       Get histogram.
      CALL QGET (RESHIS, IAPHIS, PNUMIN, 2)
      CALL QWD
C                                       Release the AP.
      CALL QRLSE
C                                      Flush output
      CALL MDISK ('FINI', LUNS(6), FIND2, BUFF2, BIND2, IRET)
      IF (IRET.EQ.0) GO TO 150
         WRITE (MSGTXT,1070) IRET,I
         GO TO 990
C                                      Close residual file.
 150  CALL ZCLOSE (LUNS(6), FIND2, IRET)
      CALL ZCLOSE (LUNS(5), FIND1, IRET)
      IF (IRET.EQ.0) GO TO 160
         WRITE (MSGTXT,1150) IRET
         CALL MSGWRT (8)
C                                       If last field integrate
C                                       histogram.
 160  IF (IFIELD.LT.MFIELD) GO TO 999
         LIMIT = NUMBIN - 1
         DO 200 I = 1,LIMIT
            J = NUMBIN - I + 1
            RESHIS(J-1) = RESHIS(J-1) + RESHIS(J)
 200        CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXRHIS: ERROR',I3,' OPENING FILE ')
 1010 FORMAT ('MXRHIS: ERROR',I3,' INIT FILE ')
 1020 FORMAT ('MXRHIS: READ ERROR',I3,' ROW ',I5)
 1070 FORMAT ('MXRHIS: WRITE ERROR',I3,' ROW ',I5)
 1150 FORMAT ('MXRHIS: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE MDECID
C-----------------------------------------------------------------------
C   MDECID determines a beam patch and limiting map value (PATCH and
C   MAPLIM) which will optimize the use of the array processor.
C   If more residuals are in the top histogram bin than will fit then
C   NRESKP is set to a value larger than 1 and indicates the number of
C   residuals to MXMPAC to skip between putting values in the AP.
C   Inputs:
C     BMHIS  I     max. fractional abs. of sidelobes external to
C                  beam patch I + 1.  See MXBHIS for details.
C     RESHIS I     Histogram of abs residual map values.
C                  See MXRHIS for details.
C     NUMBIN I     Number of levels for fraction of the peak.
C     RESMAX I     Max. abs. residual map value .
C     MAXPCH I     Maximum beam patch size allowed.
C     MINPCH I     Minimum beam patch size allowed.
C   Output:
C     PATCH  I     Beam patch size (max. distance from the center)
C     MAPLIM R     Minimum abs. map level to be considered.
C                  Only values GREATER than MAPLIM should be
C                  used.
C     NRESKP I     Number of residuals to skip between putting them
C                  into the AP when there are too many to fit.
C-----------------------------------------------------------------------
      CHARACTER PREFIX*5
      INTEGER   I, IPMAP, JLEV, K, KPAT, MLOOP, IT1, IT2, IAPBSZ,
     *   ITEMP, CORE, MXAPCC, MAXRES
      LOGICAL   LERR
      REAL      XFLUX
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA MAXRES /20050/
C-----------------------------------------------------------------------
      NRESKP = 1
      IT1 = MAXPCH * 2 + 3
C                                       CLACLN buffer, field structs
      I = 130 + 2 + 1 + 6 * MAXFLD
C                                       safety
      I = I + 64
      IAPBSZ = MAX (IT1, I)
C                                       Set start location for beam.
      APBEAM = IAPBSZ
C                                       Determine max PATCH for
C                                       MFIELDS=1.
      IT1 = WINDOW(3,1) - WINDOW(1,1) + 1
      IT2 = WINDOW(4,1) - WINDOW(2,1) + 1
      IPMAP = MAX (IT1, IT2)
C                                       Loop thru beam patch sizes to
C                                       find the largest which, with
C                                       necessary  map points will fit
C                                       in the AP.
C                                       Minimum MINPCH is 3.
      MINPCH = MIN (MINPCH, IT1, IT2)
      IT1 = MAXPCH - 5
      MINPCH = MAX ( MIN (MINPCH, IT1), 3)
      MLOOP = MAXPCH - MINPCH + 1
C                                       Maximum CCs for minor cycle
      MXAPCC = (APSIZ - IAPBSZ - ((MINPCH+1)*2*MINPCH))/3
C                                       Require at few CCs
      MXAPCC = MAX (MXAPCC, 10)
C                                       set maximum # pixels searched
      IF ((MAXPIX.LT.1000) .OR. (MAXPIX.GT.500000)) MAXPIX = MAXRES
      MAXPIX = MIN (MAXPIX, MXAPCC)
C                                       For all allowed beam patches
      DO 50 I = 1,MLOOP
         KPAT = MAXPCH - I + 1
C                                       Ignore beam patches with no info
         IF (BMHIS(KPAT).LE.0) GO TO 50
C                                       Amount of AP core for beam
            CORE = KPAT + 1
            CORE = CORE * (2 * CORE - 1)
C                                       Use value in BMHIS as an index
C                                       for RESHIS.
            JLEV = BMHIS(KPAT)
            IF (JLEV.LT.2) GO TO 35
C                                       Make sure some data to be loaded
               ITEMP = JLEV
               DO 30 K = 2,ITEMP
                  IF (RESHIS(JLEV).GT.0.) GO TO 35
                     JLEV = JLEV - 1
 30               CONTINUE
C                                       Compute total AP core for this
C                                       PATCH - MAPLIM combination.
 35         CORE = CORE + 3.0 * RESHIS(JLEV) + 0.5
C                                       See if this will fit and if so
C                                       jump out of the loop.
            IF ((CORE.LT.APSIZ-IAPBSZ) .AND. (RESHIS(JLEV).LE.MAXPIX))
     *         GO TO 60
 50      CONTINUE
C                                       If the program gets here, no
C                                       beam patch - map limit would
C                                       fit.  Use minimum beam patch
C                                       and determine which map limit
C                                       will fit.
      KPAT = MINPCH
      DO 55 I = 1,NUMBIN
         JLEV = I
         CORE = KPAT + 1
         CORE = (CORE * (2 * CORE - 1)) + (3 * RESHIS(JLEV))
C                                       If all points fit in AP
C                                       Jump out and use this RMS level
         IF ((CORE.LT.APSIZ-IAPBSZ) .AND. (RESHIS(JLEV).LE.MAXPIX))
     *      GO TO 60
C                                       For all binnng levels
   55    CONTINUE
C                                       If program gets here there is a
C                                       serious problem. e.g. residual
C                                       map has a constant (or nearly)
C                                       value, set NRESKP.
      KPAT = MINPCH - 1
C                                       Use next to highest bin
      JLEV = NUMBIN - 1
C                                       Let user know
      MSGTXT = 'MDECID: CANNOT OBTAIN BEAM PATCH - MAP LIMIT PROPERLY'
      CALL MSGWRT (8)
C                                       A solution was found.
 60   CONTINUE
C                                       If not the highest levels
      IF (JLEV.LT.NUMBIN-1) THEN
         ITEMP = JLEV
C                                       For all higher levels
         DO 80 K = ITEMP, NUMBIN-1
C                                       If higher level has many points
            IF (RESHIS(K).GT.MAXPIX) JLEV = K
C                                       End all higher levels loop
 80      CONTINUE
C                                       End if highest levels
         END IF
C                                       Min. flux residual to load in AP
      MAPLIM = (RESMAX * (JLEV-0.5) ) / (NUMBIN - 1)
C                                       No smaller than minimum
      PATCH = MAX (KPAT+1, MINPCH)
C                                       No bigger than maximum
      PATCH = MIN (PATCH, (NXBEM(1)/2)-1, (NYBEM(1)/2)-1, MAXPCH)
C                                       If map is flat, skip a few
      NRESKP = MAX (1., RESHIS(JLEV-1) / REAL(MAXPIX))
C                                       Display the results.
      IF (NRESKP.GT.1) THEN
         WRITE (MSGTXT,1056) NRESKP
         CALL MSGWRT (8)
         END IF
      XFLUX = MAPLIM
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1990) (PATCH*2)-1,XFLUX, PREFIX
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1056 FORMAT ('MDECID: Will load every ',I4,' th. residual')
 1990 FORMAT ('Using',I5,' cell CLN beam + residuals > ',F8.2,1X,A5,
     *        'Jy')
      END
      SUBROUTINE MXBSHV (IRET)
C-----------------------------------------------------------------------
C   MXBSHV loads the beam patch into the array processor.
C   Beam assumed in output beam file.
C   Input :
C      PATCH  I   = Beam patch half size (cells).
C   Output:
C      IRET   I   = Return error code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   FIND, BIND, IRET, I, WIN(4)
      INTEGER   PPATCH, TWOPAT, IPTR, PTRI, PTRJ, CORE
      LOGICAL   MAP, EXCL, WAIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                      Set window for beam patch.
      WIN(1) = NXBEM(1) / 2
      WIN(2) = NYBEM(1) / 2 - PATCH + 2
      WIN(3) = NXBEM(1) / 2 + PATCH - 1
      WIN(4) = NYBEM(1) / 2 + PATCH
      PPATCH = PATCH
      TWOPAT = 2 * PATCH - 1
C                                      Check to make sure the beam
C                                      patch will fit in the AP.
      PTRJ = APBEAM
      CORE = APBEAM + (PATCH * TWOPAT)
      IF (CORE.LT.APSIZ) GO TO 10
         WRITE (MSGTXT,1000) CORE
         CALL MSGWRT (8)
         GO TO 999
C                                      Open beam file.
 10   CALL ZPHFIL ('MA', DISOUT, CNOBEM(1) , 1, FILE, IRET)
      CALL ZOPEN (LUNS(3), FIND, DISOUT, FILE, MAP, EXCL,
     *   WAIT, IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                      Initialize beam map.
 20   CALL MINIT ('READ', LUNS(3), FIND, NXBEM(1), NYBEM(1), WIN, BUFF1,
     *   JBUFSZ, BOBEM(1), IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                       Loop thru beam & shove into AP
 30   PTRI = PTRJ
      DO 50 I = 1, TWOPAT
C                                      Read row of the map.
         CALL MDISK ('READ', LUNS(3), FIND, BUFF1, BIND, IRET)
         IF (IRET.EQ.0) GO TO 40
            WRITE (MSGTXT,1030) IRET, I
            CALL MSGWRT (8)
            GO TO 999
C                                       Add Prussian Helmet
 40      IF ((I.EQ.PATCH) .AND. (PHAT.GT.0.0))
     *      BUFF1(BIND) = BUFF1(BIND) + PHAT
C                                      Load row into AP.
         IPTR = PTRI
         CALL QWR
         CALL QPUT (BUFF1(BIND), 2, PPATCH, 2)
         CALL QWD
C                                       Move to the proper location.
         CALL QVMOV (2, 1, IPTR, TWOPAT, PPATCH)
         PTRI = PTRI + 1
 50      CONTINUE
C                                       Compute starting address for
C                                       residuals.
      APRESD = ((PPATCH - 1) * TWOPAT) + IPTR + 2
      CALL ZCLOSE (LUNS(3), FIND, IRET)
      IF (IRET.EQ.0) GO TO 100
         WRITE (MSGTXT,1050) IRET
         CALL MSGWRT (8)
         GO TO 999
C                                       Roll AP if necessary.
 100  CALL QROLL (APRESD, BUFF1, JBUFSZ, IRET)
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXBSHV: YOU NEED ',F7.0,' WORDS OF AP CORE ')
 1010 FORMAT ('MXBSHV: ERROR',I3,' OPENING FILE ')
 1020 FORMAT ('MXBSHV: ERROR',I3,' INIT FILE ')
 1030 FORMAT ('MXBSHV: READ ERROR',I3,' ROW ',I5)
 1050 FORMAT ('MXBSHV: ERROR',I3,' CLOSING FILE ')
      END
      SUBROUTINE MXMPAC (IRET)
C-----------------------------------------------------------------------
C   MXMPAC takes points from the residual map greater than MAPLIM
C   and places them with their addresses into the array processor.
C   Input from commons:
C      DOBOX      L    TRUE if boxes specified for field 1
C      NBOXES     I    Number of boxes given for field 1
C      WIN1(4,50) I    Boxes for Field 1
C      NRESKP     I    Number of residuals to skip between putting
C                      them into the AP when there are too many to
C                      fit.
C   Output:
C      APCFLD(64) I   start addresses of residuals for each field
C      APCLCN(64) I   element count for each field.
C      IRET   I     Return error code, 0=>OK, otherwise error.
C   Residual map points greater than MAPLIM and their addresses are
C   placed in the array processor.
C-----------------------------------------------------------------------
      CHARACTER FILE*48
      INTEGER   FIND, BIND, IRET, I, II, J, JJ, MX, MY, IBOX, LIMBOX,
     *   NX, NY, IF, IXOFF, IYOFF, SKPCNT, WIN(4),
     *   LOX, HIX, LOY, HIY, MAXY, MINY, MAXX
      INTEGER   JPTR, JPTRP1, LPTR, IPTR, PTRJ
      LOGICAL   MAP, EXCL, WAIT, DOBX
      REAL      V(2048), X(2048), Y(2048)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (X(1), BUFF2(1)), (Y(1), BUFF2(2049)),
     *   (V(1), BUFF3(1))
      DATA MAP, EXCL, WAIT /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Get first field size
      NX = FLDSZ(1,1)
      NY = FLDSZ(2,1)
C                                       Find max,min Y in boxes
      MAXY = -30000
      MINY = 30000
      MAXX = -30000
      DO 10 IBOX = 1,NBOXES
         MAXX = MAX (MAXX, WIN1(1,IBOX), WIN1(3,IBOX))
         MAXY = MAX (MAXY, WIN1(2,IBOX), WIN1(4,IBOX))
         MINY = MIN (MINY, WIN1(2,IBOX), WIN1(4,IBOX))
 10      CONTINUE
      IF (MAXY.LE.0) MAXY = MAX (WINDOW(2,1), WINDOW(4,1))
      IF (MINY.LE.0) MINY = MAX (WINDOW(2,1), WINDOW(4,1))
C                                       If invalid box
      IF ((MAXY.GT.NY .OR. MAXX.GT.NX) .AND. DOBOX) THEN
         MSGTXT = 'FIELD 1 BOXES LARGER THAN IMAGE; IGNORING BOXES!!!'
         CALL MSGWRT(5)
         DOBOX = .FALSE.
         END IF
C                                       Initialize pointer
      JPTR = APRESD
C                                       Loop thru fields.
      DO 200 IF = 1, MFIELD
C                                       Always take first residual
         SKPCNT = NRESKP
C                                       Initialize field residual info
         APCLCN(IF) = 0
         PTRJ = JPTR
         APCFLD(IF) = PTRJ
C                                       Open Residual file.
         CALL ZPHFIL ('MA', DISOUT, CCCNO(IF), 1, FILE, IRET)
         CALL ZOPEN (LUNS(6), FIND, DISOUT, FILE, MAP, EXCL,
     *      WAIT, IRET)
         IF (IRET.EQ.0) GO TO 20
            WRITE (MSGTXT,1010) IRET, IF
            GO TO 990
C                                       Set some constants.
 20      NX = FLDSZ(1,IF)
         NY = FLDSZ(2,IF)
C                                       If field 1 and BOX set - loop
         LIMBOX = 1
         DOBX = DOBOX .AND. (IF.EQ.1)
         IF (DOBX) LIMBOX = NBOXES
         WIN(1) = 1
         WIN(2) = MINY
         WIN(3) = NX
         WIN(4) = MAXY
         IF (.NOT.DOBX) CALL COPY (4, WINDOW(1,IF), WIN)
C                                      Compute offset.
         IXOFF = WIN(1) - 1
         IYOFF = WIN(2) - 1
         MX = (WIN(3) - WIN(1)) + 1
         MY = (WIN(4) - WIN(2)) + 1
         IF (MX.LT.0) MX = -MX + 2
         IF (MY.LT.0) MY = -MY + 2
C                                      Initialize residual map.
         CALL MINIT ('READ', LUNS(6), FIND, NX, NY, WIN, BUFF1, JBUFSZ,
     *      BORES(IF), IRET)
         IF (IRET.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IRET, IF
            GO TO 990
C                                       Loop thru map looking for points
C                                       greater than MAPLIM.
 30      IPTR = 0
         DO 70 I = 1,MY
            II = I + IYOFF
C                                      Read row of the map.
            CALL MDISK ('READ', LUNS(6), FIND, BUFF1, BIND, IRET)
            IF (IRET.EQ.0) GO TO 40
               WRITE (MSGTXT,1030) IRET, I, IF
               GO TO 990
C                                       Loop over boxes
 40         DO 60 IBOX = 1,LIMBOX
               LOY = MIN (WIN1(2,IBOX), WIN1(4,IBOX))
               HIY = MAX (WIN1(2,IBOX), WIN1(4,IBOX))
               IF (HIY.LE.0) HIY = MAX (WINDOW(2,1), WINDOW(4,1))
               IF (LOY.LE.0) LOY = MAX (WINDOW(2,1), WINDOW(4,1))
               IF (DOBX .AND. ((II.LT.LOY) .OR. (II.GT.HIY))) GO TO 60
               LOX = 1
               HIX = MX
               IF (DOBX) LOX = MIN (WIN1(1,IBOX), WIN1(3,IBOX))
               IF (DOBX) HIX = MAX (WIN1(1,IBOX), WIN1(3,IBOX))
C                                       Loop down the row.
               DO 50 J = LOX,HIX
                  JJ = BIND + J - 1
C                                       Check flux
                  IF (ABS (BUFF1(JJ)).LT.MAPLIM) GO TO 50
C                                       Skip residuals if necessary
                  SKPCNT = SKPCNT + 1
                  IF (SKPCNT.GE.NRESKP) SKPCNT = 0
                  IF (SKPCNT.NE.0) GO TO 50
C                                       Check if AP full.
                     PTRJ = JPTR
                     IF (PTRJ+IPTR*3.GE.APSIZ) GO TO 80
                     IPTR = IPTR + 1
                     V(IPTR) = BUFF1(JJ)
                     X(IPTR) = J + IXOFF
                     Y(IPTR) = I + IYOFF
C                                      Check to see if buffer full.
                     IF (IPTR.LT.256) GO TO 50
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
                        APCLCN(IF) = APCLCN(IF) + 256
 50                  CONTINUE
 60               CONTINUE
 70            CONTINUE
         GO TO 90
C                                       If program gets here AP filled.
 80      MSGTXT = 'MXMPAC: NOT ENOUGH ROOM FOR ALL MAP POINTS '
         CALL MSGWRT (8)
C                                      Write last buffer into the AP.
 90      IF (IPTR.LE.0) GO TO 100
C                                       Check if AP full.
         PTRJ = JPTR
         IF (PTRJ+(IPTR*3).GE.APSIZ) THEN
            IPTR = (APSIZ - PTRJ) / 3
            IF (IPTR.LE.0) GO TO 100
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
            APCLCN(IF) = APCLCN(IF) + IPTR
            CONTINUE
C                                       Don't ignore fields with 1
C                                       residual
 100     APCLCN(IF) = MAX (0, APCLCN(IF))
C                                      Close residual file.
         CALL ZCLOSE (LUNS(6), FIND, IRET)
         IF (IRET.EQ.0) GO TO 200
            WRITE (MSGTXT,1100) IRET, IF
            CALL MSGWRT (8)
 200        CONTINUE
C                                       Roll AP if necessary.
      CALL QROLL (JPTR, BUFF1, JBUFSZ, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set and print number of points.
      JPTR = JPTR - APRESD
      RESNUM = JPTR / 3
      WRITE (MSGTXT,1200) RESNUM
      CALL MSGWRT (2)
C                                       Check that some data loaded
      IF (RESNUM.GT.0) GO TO 999
         IRET = 1
         MSGTXT = 'NO RESIDUAL POINTS LOADED INTO AP, I QUIT'
C                                       Error occured.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MXMPAC: ERROR',I3,' OPENING FILE, FIELD',I5)
 1020 FORMAT ('MXMPAC: ERROR',I3,' INIT FILE, FIELD',I5)
 1030 FORMAT ('MXMPAC: READ ERROR',I3,' ROW ',I5,' FIELD ',I5)
 1100 FORMAT ('MXMPAC: ERROR',I3,' CLOSING FILE, FIELD',I5)
 1200 FORMAT ('MXMPAC:',I7,' Residual map points loaded into AP')
      END
      SUBROUTINE MXADD (IFIELD, IRET)
C-----------------------------------------------------------------------
C   MXADD adds the image in the GRD file to image IFIELD.
C   Also finds maximum/minimum
C   Input:  IFIELD  I    Field number of image.
C   Output: IRET    I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      CHARACTER FILE*48, SFILE*48
      INTEGER   IRET, IFIELD
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO,
     *   WIN(4), NX, NY, J, J1, I
      LOGICAL   T, F
      REAL   XMIN, XMAX, XVAL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
      DATA WIN /4*0/
C-----------------------------------------------------------------------
      NX = FLDSZ(1,IFIELD)
      NY = FLDSZ(2,IFIELD)
      XMAX = -1.0E20
      XMIN = 1.0E20
C                                        Open and init residual.
      CALL ZPHFIL ('MA', DISOUT, CCCNO(IFIELD), 1, FILE, IRET)
      CALL ZOPEN (LUNS(4), FIND1, DISOUT, FILE, T, F, T, IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET, 'READ', IFIELD
         GO TO 990
 10   CALL MINIT ('READ', LUNS(4), FIND1, NX, NY, WIN, BUFF1, JBUFSZ,
     *   BORES(IFIELD), IRET)
      IF (IRET.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IRET, 'READ', IFIELD
         GO TO 990
C                                        Open and init restored.
 20   CALL ZPHFIL ('SC', SCRVOL(SCRGRD), SCRCNO(SCRGRD), 1, SFILE, IRET)
      CALL ZOPEN (LUNS(5), FIND2, SCRVOL(SCRGRD), SFILE, T, F, T, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1000) IRET, 'READ', IFIELD
         GO TO 990
 30   CALL MINIT ('READ', LUNS(5), FIND2, NX, NY, WIN, BUFF2, JBUFSZ,
     *   BO, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1010) IRET, 'READ', IFIELD
         GO TO 990
C                                        Open and init OUTPUT
 40   CALL ZOPEN (LUNS(6), FIND3, DISOUT, FILE, T, F, T, IRET)
      IF (IRET.EQ.0) GO TO 50
         WRITE (MSGTXT,1000) IRET, 'WRIT', IFIELD
         GO TO 990
 50   CALL MINIT ('WRIT', LUNS(6), FIND3, NX, NY, WIN, BUFF3, JBUFSZ,
     *   BORES(IFIELD), IRET)
      IF (IRET.EQ.0) GO TO 60
         WRITE (MSGTXT,1010) IRET, 'WRIT', IFIELD
         GO TO 990
C                                        Loop, multiplying.
 60   DO 110 I = 1, NY
C                                        Read RESIDUAL
         CALL MDISK ('READ', LUNS(4), FIND1, BUFF1, BIND1, IRET)
         IF (IRET.EQ.0) GO TO 70
            WRITE (MSGTXT,1060) IRET, 'READ', IFIELD
            GO TO 990
C                                        Read restored CC's.
 70      CALL MDISK ('READ', LUNS(5), FIND2, BUFF2, BIND2, IRET)
         IF (IRET.EQ.0) GO TO 80
            WRITE (MSGTXT,1060) IRET, 'READ', IFIELD
            GO TO 990
C                                        Write Restored image.
 80      CALL MDISK ('WRIT', LUNS(6), FIND3, BUFF3, BIND3, IRET)
         IF (IRET.EQ.0) GO TO 90
            WRITE (MSGTXT,1060) IRET, 'WRIT', IFIELD
            GO TO 990
C                                        Add.
 90      DO 100 J = 1, NX
            J1 = J - 1
            XVAL = BUFF1(BIND1+J1) + BUFF2(BIND2+J1)
            BUFF3(BIND3+J1) = XVAL
C                                       Find extrema.
            XMAX = MAX (XMAX, XVAL)
            XMIN = MIN (XMIN, XVAL)
 100        CONTINUE
 110     CONTINUE
C                                       Set extrema for field.
      FLDMAX(IFIELD) = XMAX
      FLDMIN(IFIELD) = XMIN
C                                        Flush buffer.
      CALL MDISK ('FINI', LUNS(6), FIND3, BUFF3, BIND3, IRET)
      IF (IRET.EQ.0) GO TO 120
         WRITE (MSGTXT,1060) IRET, 'FINI', IFIELD
         GO TO 990
C                                        Close files.
 120  CALL ZCLOSE (LUNS(4), FIND1, IRET)
      CALL ZCLOSE (LUNS(5), FIND2, IRET)
      CALL ZCLOSE (LUNS(6), FIND3, IRET)
      IRET = 0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXADD: ERROR',I3,' OPEN FOR ',A4,' FIELD',I3)
 1010 FORMAT ('MXADD: ERROR',I3,' INIT FOR ',A4,' FIELD',I3)
 1060 FORMAT ('MXADD: ERROR',I3,1X,A4,'ING FIELD',I3)
      END
      SUBROUTINE MXHIS (IFIELD)
C-----------------------------------------------------------------------
C   MXHIS creates a history file for the map and enters the values
C   of the parameters controling the map and the name of the output
C   file.
C   Input:
C      IFIELD           I    Field number, if = 0 then do beam.
C   Input via Commons:
C      CCCNO(IFIELD)    I    Catalog slot numbers for maps.
C      CATBLK(256)      I    Catalog header block for map
C      FLDSZ(2,*)       I    Map dimension (cells) RA and Dec.
C      XFLD,YFLD(IFIELD)R    Field of view (arcsec.) RA and Dec.
C      XSHIFT(IFIELD)    R    Eastward shift of map center.(arcsec)
C      YSHIFT(IFIELD)    R    Northward shift of map center.(arcsec)
C      DOBOX            L    TRUE if boxes specified for field 1
C      NBOXES           I    Number of boxes given for field 1
C      WIN1(4,50)       I    Boxes for Field 1
C      TAPERU,TAPERV    R    Tapers in U and V
C      BLMIN,BLMAX      R    Max. and Min. UV values.
C      NATWT            I    = 1 for natural weighting, otherwise unif.
C      UNFBOX           I    Halfwidth of Unif. wt. smoothing box.
C      DOZERO           L    TRUE if zero spacing flux density
C                            specified.
C      ZEROSP(5)        R    weight for zero spacing flux density.
C      ZEROSP(4)        R    Zero spacing flux densities(I,Q,U,V).
C      CTYPX,CTYPY      I    Convolving fn. type in RA and Dec.
C      XPARM,YPARM(10)  R    Convolving fn. parameters.
C      STOKES           C*4  Polarization maps requested,
C      FREQ             D    Frequency of observation (Hz).
C      NVIS             I    Number of visibility obs. used.
C      ICHAN            I    Number of first freq. chan. in this run
C      NUMCH            I    Number of frequency chan. in this run.
C      XNCAVG           R    Number of input channels per output chan.
C      BIF,EIF          I    IF range gridded.
C      ICNTRY(IFIELD)   I    Center pixel in Dec.
C      NCLNG(IFIELD)     D    Number of components this field.
C      GAIN             R    Loop gain used.
C      BMAJ             R    Major axis size
C      BMIN             R    Minor axis size
C      BPA              R    Restoring beam position angle.
C      FACTOR           R    CLEAN speedup factor.
C   Output: ONE history file.
C-----------------------------------------------------------------------
      CHARACTER  ATIME*8, ADATE*12, CHCONV(6)*8
      INTEGER   LUN(2), IRET, J, CNOOUT, IF, I, IFIELD
      INTEGER   TIME(3), DATE(3)
      LOGICAL   T, EXIST
      CHARACTER HILINE*80, SOURSE*8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T /.TRUE./, LUN /29,30/
      DATA CHCONV /'Pill box', 'Expontl ', 'Sinx / x',
     *             'Exp*sinc', 'Spheroid', 'Bess*Exp'/
C-----------------------------------------------------------------------
C                                       Copy/open map file.
      IF (IFIELD.GT.0) THEN
         CNOOUT = CCCNO(IFIELD)
         IF = IFIELD
      ELSE
         CNOOUT = CNOBEM(1)
         IF = 1
         END IF
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Init HIstory
      CALL HIINIT (2)
C                                       Check if map history exists
      CALL FNDEXT ('HI', CATBLK, I)
      EXIST = I.GT.0
C                                       Check if HI file exists.
      IF (EXIST) THEN
C                                       Restart - open file
         CALL HIOPEN (LUN(2), DISOUT, CNOOUT, BUFF1, IRET)
         IF (IRET.EQ.5) GO TO 20
         IF (IRET.EQ.0) THEN
            WRITE (HILINE,1999) TSKNAM, RLSNAM, ADATE, ATIME
            CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 100
C                                       Now ready to write new entries.
               GO TO 40
            END IF
         WRITE (MSGTXT,1010) IRET, IFIELD
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       copy keywords
 20   CALL KEYCOP (DISIN, CNOOLD, DISOUT, CNOOUT, IRET)
C                                       copy history
      CALL HISCOP (LUN(1), LUN(2), DISIN, DISOUT, CNOOLD, CNOOUT,
     *   CATBLK, BUFF2, BUFF1, IRET)
      IF (IRET.GT.2) THEN
         WRITE (MSGTXT,1000) IRET, IFIELD
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Write mapname
 40   CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, DISIN, LUN(2),
     *   BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       uv work file
      CALL HENCO2 (TSKNAM, NAME2, CLASS2, SEQ2, DISK2, LUN(2),
     *   BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Source name.
      CALL H2CHR (8, 1, CATH(KHOBJ), SOURSE)
      WRITE (HILINE,2001) TSKNAM, SOURSE
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Field number.
      IF (IFIELD.GT.0) THEN
         WRITE (HILINE,2039) TSKNAM, IFIELD, MFIELD
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Start and # of channels.
      WRITE (HILINE,2022) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       No. chan. added
      WRITE (HILINE,2002) TSKNAM, NCHAVG
      CALL HIADD (LUN(2),HILINE,BUFF1,IRET)
      IF (IRET.NE.0) GO TO 100
C                                       IF range
      IF (JLOCIF.GE.0) THEN
         WRITE (HILINE,2044) TSKNAM, BIF, EIF
         CALL HIADD (LUN(2),HILINE,BUFF1,IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Map size.
      IF (IFIELD.GT.0) THEN
         WRITE (HILINE,2005) TSKNAM, FLDSZ(1,IFIELD),
     *      FLDSZ(2,IFIELD)
      ELSE
         WRITE (HILINE,2005) TSKNAM, NXBEM(1), NYBEM(1)
         END IF
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Map window
      IF ((IFIELD.GT.0) .AND. (IF.GT.0)) THEN
         WRITE (HILINE,2036) TSKNAM, WINDOW(1,IF), WINDOW(2,IF),
     *      WINDOW(3,IF), WINDOW(4,IF)
         IF ((IFIELD.NE.1) .OR. (.NOT.DOBOX))
     *      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
C                                       Field of view
         WRITE (HILINE,2006) TSKNAM, XFLD(IF), YFLD(IF)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      WRITE (HILINE,2106) TSKNAM, CELLSG(1), CELLSG(2)
      CALL HIADD (LUN(2),  HILINE,  BUFF1,  IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Frequency,polarization
      WRITE (HILINE,2007) TSKNAM, CATD(KDCRV+2), STOKES
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Translation
      IF (IFIELD.GT.0) THEN
         WRITE (HILINE,2008) TSKNAM, XSHIFT(IF), YSHIFT(IF)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Number of boxes.
      IF ((DOBOX) .AND. (IFIELD.GE.1)) THEN
         WRITE (HILINE,2050) TSKNAM, NBOXES
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
C                                       Windows.
         DO 50 I = 1,NBOXES
            WRITE (HILINE,2051) TSKNAM, I, (WIN1(J,I), J = 1,4)
            CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 100
 50         CONTINUE
         END IF
C                                       Taper
      WRITE (HILINE,2009) TSKNAM, TAPERU, TAPERV
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Annulus
      WRITE (HILINE,2010) TSKNAM, BLMIN, BLMAX
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Weighting,
      IF (.NOT.DOUNIF) WRITE (HILINE,2011) TSKNAM
      IF (DOUNIF) WRITE (HILINE,2012) TSKNAM, UNFBOX
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Zero spacing flux density
      IF (DOZERO) THEN
         WRITE (HILINE,2013) TSKNAM, ZEROSP(1), ZEROSP(5)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
C                                       If other elements non-zero
         IF (ZEROSP(2).NE.0.0.OR.ZEROSP(3).NE.0.0) THEN
            WRITE (HILINE,2014) TSKNAM, ZEROSP(2), ZEROSP(3),
     *         ZEROSP(4)
            CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
            END IF
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Convolving function.
      WRITE (HILINE,2120) TSKNAM, CHCONV(CTYPX), CHCONV(CTYPY)
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,2015) TSKNAM, CTYPX, (XPARM(J), J = 1,3)
      CALL HIADD (LUN(2), HILINE,  BUFF1,  IRET)
      IF (IRET.NE.0) GO TO 100
C                                       skip lines of zeros
      IF (XPARM(4).NE.0.0 .OR. XPARM(5).NE.0.0 .OR. XPARM(6).NE.0.0 .OR.
     *    XPARM(7).NE.0.0 .OR. XPARM(8).NE.0.0 .OR. XPARM(9).NE.0.0 .OR.
     *    XPARM(10).NE.0.0) THEN
         WRITE (HILINE,2016) TSKNAM, (XPARM(J), J = 4,7)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,2016) TSKNAM, (XPARM(J), J = 8,10)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      WRITE (HILINE,2017) TSKNAM, CTYPY, (YPARM(J), J = 1,3)
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      IF (YPARM(4).NE.0.0 .OR. YPARM(5).NE.0.0 .OR. YPARM(6).NE.0.0 .OR.
     *    YPARM(7).NE.0.0 .OR. YPARM(8).NE.0.0 .OR. YPARM(9).NE.0.0 .OR.
     *    YPARM(10).NE.0.0) THEN
         WRITE (HILINE,2016) TSKNAM, (YPARM(J), J = 4,7)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,2016) TSKNAM, (YPARM(J), J = 8,10)
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       Guard bands
      WRITE (HILINE,2018) TSKNAM, XGUARD
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       residual limit
      WRITE (HILINE,2019) TSKNAM, MAXPIX
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Number of visibility pts. used.
      WRITE (HILINE,2020) TSKNAM, NVIS
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       If BEAM skip CLEAN info.
      IF (IFIELD.LE.0) GO TO 200
C                                       Number of components CLEANed.
      WRITE (HILINE,2030) TSKNAM, NCLNG(IF)
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      WRITE (HILINE,2031) TSKNAM, CLNSTR
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Loop gain.
      WRITE (HILINE,2032) TSKNAM, GAIN
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Speed up factor
      WRITE (HILINE,2033) TSKNAM, FACTOR
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Flux minimum.
      WRITE (HILINE,2034) TSKNAM, FMIN
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       CLEAN restoring beam
      WRITE (HILINE,2035) TSKNAM, BMAJ, BMIN, BPA
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Min beam patch
      I = MINPCH + 1
      WRITE (HILINE,2038) TSKNAM, I
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       CLEANed flux.
      WRITE (HILINE,2040) TSKNAM, FLUXG(IF)
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Restart iteration
      WRITE (HILINE,2041) TSKNAM, CLNCNT
      IF (CLNCNT.GT.0.0)
     *   CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       restart channel
      IF (RCHAN.GT.BCHAN) THEN
         WRITE (HILINE,2042) TSKNAM, RCHAN
         CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
C                                       CMETHOD
      WRITE (HILINE,2043) TSKNAM, CMETH
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       Sum of weights
      WRITE (HILINE,2060) TSKNAM, BEMMAX(1)
      CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 100
      GO TO 200
C                                       An error occurred.
 100     WRITE (MSGTXT,1100) IRET, IFIELD
         CALL MSGWRT (6)
C                                       Close file.
 200  CALL HICLOS (LUN(2), T, BUFF1, IRET)
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISOUT, CNOOUT, CATBLK, 'CLWR', BUFF1, IRET)
      IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1200) IRET, IFIELD
         CALL MSGWRT (8)
         END IF
C                                       If not restarting
      IF ((.NOT.SKPSUB) .AND. (CNOOUT.EQ.CNOBEM(1))) THEN
C                                       Create the UVWORK HI table
         CALL HICREA (LUN(2), DISK2, CNOSCR, CATSCR, BUFF1, IRET)
         IF (IRET.EQ.0) THEN
            WRITE (HILINE,1999) TSKNAM, RLSNAM, ADATE, ATIME
            CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
            IRET = 0
C                                       Write mapname
            CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, DISIN, LUN(2),
     *         BUFF1, IRET)
C                                       Start and # of channels.
            WRITE (HILINE,2022) TSKNAM, BCHAN, ECHAN
            CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
C                                       No. chan. added
            WRITE (HILINE,2002) TSKNAM, NCHAVG
            CALL HIADD (LUN(2),HILINE,BUFF1,IRET)
C                                       IF range
            IF (JLOCIF.GE.0) THEN
               WRITE (HILINE,2044) TSKNAM, BIF, EIF
               CALL HIADD (LUN(2),HILINE,BUFF1,IRET)
               END IF
C                                       Frequency,polarization
               WRITE (HILINE,2007) TSKNAM, CATD(KDCRV+2), STOKES
               CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
C                                       Taper
               WRITE (HILINE,2009) TSKNAM, TAPERU, TAPERV
               CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
C                                       Annulus
               WRITE (HILINE,2010) TSKNAM, BLMIN, BLMAX
               CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
C                                       Weighting,
               IF (.NOT.DOUNIF) WRITE (HILINE,2011) TSKNAM
               IF (DOUNIF) WRITE (HILINE,2012) TSKNAM, UNFBOX
               CALL HIADD (LUN(2), HILINE, BUFF1, IRET)
C                                       End if HI Created OK
            END IF
         CALL HICLOS (LUN(2), T, BUFF1, IRET)
         IRET = 0
C                                       End if creating UVWORK HI
         END IF
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXHIS: ERROR',I3,' CREATING HISTORY FILE, FIELD',I5)
 1010 FORMAT ('MXHIS: ERROR',I3,' OPENING HISTORY FILE, FIELD',I5)
 1100 FORMAT ('MXHIS: ERROR',I3,' WRITING HISTORY FILE, FIELD',I5)
 1200 FORMAT ('MXHIS: CATIO ERROR',I3,' FIELD',I5)
 1999 FORMAT (A6,'Release =''',A7,' ''  /********* Restart ',
     *   A12,2X,A8)
 2001 FORMAT (A6,' SOURCE=''',A8,'''')
 2002 FORMAT (A6,' NPOINTS =',I6,' /No. channels averaged')
 2005 FORMAT (A6,' IMSIZE  =',I6,', ',I6,' /Image size')
 2006 FORMAT (A6,' MAPFLD  =',E12.5,', ',E12.5,' /Field of view')
 2106 FORMAT (A6,' CELLSIZE=',E12.5,', ',E12.5,
     *   ' /Cell size arc sec.')
 2007 FORMAT (A6,' FREQ    =',E14.7,' STOKES=''',A4,'''')
 2008 FORMAT (A6,' SHIFT   =',F10.4,', ',F10.4,' /Arc sec')
 2009 FORMAT (A6,' UVTAPER =',E12.5,', ',E12.5,
     *   ' /UV taper kilolambda')
 2010 FORMAT (A6,' UVRANGE =',E12.5,', ',E12.5,
     *   ' /UV annulus kilolambda')
 2011 FORMAT (A6,' UVWTFN  =''NA'' /Natural weighting')
 2012 FORMAT (A6,' UVWTFN  =''  '' , UVBOX =',I5,' /Uniform weight')
 2013 FORMAT (A6,' ZEROSP  =',E12.5,', Weight =',E12.5)
 2014 FORMAT (A6,' ZSP(2-4)=',E12.5,', ',E12.5,', ',E12.5)
 2015 FORMAT (A6,' XTYPE =',I3,' XPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2016 FORMAT (A6,' , ',E12.5,', ',E12.5,', ',E12.5,', ',E12.5)
 2017 FORMAT (A6,' YTYPE =',I3,' YPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2018 FORMAT (A6,' GUARD=',F7.4,',',F7.4,
     *   ' /U,V guard bands in UV-plane half widths')
 2019 FORMAT (A6,' MAXPIXEL=',I10,' / max # residual pixels ',
     *   'searched per cycle')
 2020 FORMAT (A6,' /Number of vis. points used   = ',I9)
 2022 FORMAT (A6,' BCHAN   =',I6,' ECHAN=',I6,
     *   ' /First and highest chan.')
 2120 FORMAT (A6,' /Convolution functions - U: ',A8,3X,
     *   'V: ',A8)
 2030 FORMAT (A6,' /Number components this field = ',I9)
 2031 FORMAT (A6,' NITER  = ',I9,' /Total number components')
 2032 FORMAT (A6,' GAIN   = ',F7.4,' /CLEAN loop gain factor')
 2033 FORMAT (A6,' FACTOR =',F5.2,' /Speed-up factor')
 2034 FORMAT (A6,' FLUX = ',E13.5,' /Min. CLEAN comp. (Jy)')
 2035 FORMAT (A6,' BMAJS=',F10.5,' BMINS=',F10.5,' BPAD=',F8.3,
     *   ' /CLEAN beam')
 2036 FORMAT (A6,' BOX     =',3(I6,','),I6,' /CLEAN window')
 2038 FORMAT (A6,' MINPATCH= ',I6,' /Min allowed beam patch radius')
 2039 FORMAT (A6,' /Field number ',I4,' of ',I4)
 2040 FORMAT (A6,' /CLEANed flux this field =',F10.5,' Jy')
 2041 FORMAT (A6,' BITER   = ',I9,' /Restart component number')
 2042 FORMAT (A6,' CHANNEL = ',I9,' /Restart channel number')
 2043 FORMAT (A6,' CMETHOD = ',1H',A4,1H',' /Model method')
 2044 FORMAT (A6,' BIF     = ',I6,' EIF = ',I4,' /IF range')
 2050 FORMAT (A6,' NBOXES  = ',I6,' /No. CLEAN windows')
 2051 FORMAT (A6,' BOX(',I2,') =',3(I6,','),I6,
     *   ' /CLEAN window')
 2060 FORMAT (A6,' /Sum of gridding weights = ',1PE12.5)
      END
      SUBROUTINE MXACLN (IRET)
C-----------------------------------------------------------------------
C   MXACLN does a limited clean of the Barry Clark variety using the
C   highest map points and limited beam stored in the AP.
C   Computes 32 CLEAN components per AP call.
C   Will handle up to 64 subfields.
C   Input:
C     APCFLD(64) I    = start addresses of residuals for each field
C     APCLCN(64) I    = element count for each field.
C     GAIN   R   = Clean loop gain.
C     RESNUM I   = Number of map points in the AP.
C     APRESD I   = first location in the AP of the residuals.
C     CLNSTR I   = First clean component to do. (0 for first clean).
C     CLNLIM I   = Maximum number of clean components desired.
C
C     Output:
C     CLNSTR I   = Current clean component number.
C     FINISH L   = .TRUE. If minimum clean component flux density
C                  or maximum iteration encountered, otherwise
C                  = .FALSE.
C     RESMAX R   = Maximum abs. residual (Jy).
C     FSTCLN L   = Set to false to indicate this is not the first
C                  clean cycle on this channel.
C     Lists of the clean components associated with each subfield/
C     frequency.
C     IRET    I    Return error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER KEYS(4)*8, PREFIX*5
      LOGICAL   LERR, TR, FA
      INTEGER   NREC, IROUND, LREC, IF, NCBUFF, IPOINT, NKEY, KOLS(4),
     *   RAKOL, DECKOL, FLXKOL, TYPKOL, IRET, IX, IY, ITLIM, ITER,
     *   NCOMPC, LENCBU, APFDV, APFDL, TEMP, STRTOT
      REAL      ATLIM, RECORD(7), XFAC, XFLUX, XPROD, XCENTR, YCENTR,
     *   FNFLD, CCMIN, CCFLUX, PCCMIN, CLNFLX, CHANGE, MINCHG
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   WARN(MAXFLD)
      INTEGER   CLBUFF(512,MAXAFL), LUNC(MAXFLD), IP(MAXFLD)
      REAL      FDVEC(6,MAXAFL), STRFLX(MAXFLD)
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      SAVE CCMIN, PCCMIN
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
C                                       Note: this equivalence uses
C                                       BUFF3 as well
      EQUIVALENCE (FNFLD, BUFF2(1)),     (FDVEC, BUFF2(2))
      EQUIVALENCE (CLBUFF, BUFF2(2+6*MAXAFL))
C                                       LENCBU, NCOMPC control the
C                                       no. CC per AP call.
      DATA LENCBU, NCOMPC /130, 32/
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ', 'TYPE OBJ'/
      DATA TR, FA /.TRUE.,.FALSE./
C                                       Init min and Previous Min CC
      DATA CCMIN, PCCMIN/1.0E12,1.0E12/
C-----------------------------------------------------------------------
      ITLIM = 0.1 * CLNLIM + 0.91
      IF (LASTIT.LE.0) LASTIT = ITLIM
      ITLIM = MIN (ITLIM, LASTIT)
      ITLIM = CLNLIM - ITLIM/2
C                                       Set FSTCLN
      FSTCLN = .FALSE.
C                                       Open CLEAN component files.
      TEMP = CLNLIM - CLNSTR + 10
      NREC = MIN (TEMP, 16384)
      LREC = 3
      IX = 2 * MAXPCH + 3
      NKEY = 0
C                                       Zero Field descriptor vector
      FNFLD = MFIELD
C                                       Loop over fields.
      DO 20 IF = 1,MFIELD
C                                       Fill field descriptor vector.
         FDVEC(1,IF) = APCFLD(IF)
         FDVEC(2,IF) = APCLCN(IF)
         FDVEC(3,IF) = 0.0
         FDVEC(4,IF) = 0.0
         FDVEC(5,IF) = 0.0
         FDVEC(6,IF) = GAIN
C                                       Set Warning (CC files full) flag
         WARN(IF) = TR
C                                       Track change from start flux
         STRFLX(IF) = ABS( FLUXG(IF))
C                                       Read CATBLK.
         CALL CATIO ('READ', DISOUT, CCCNO(IF), CATBLK, 'REST', BUFF1,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.LE.4)) THEN
            WRITE (MSGTXT,1000) IRET, IF
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CCVER(IF) = (ICHAN - BCHAN) / CHINC + 1
         IF (CCVER(IF).LT.1) CCVER(IF) = 1
         LUNC(IF) = 50 + IF
         CALL TABINI ('WRIT', 'CC', DISOUT, CCCNO(IF), CCVER(IF),
     *      CATBLK, LUNC(IF), NKEY, NREC, LREC, BUFF1, CLBUFF(1,IF),
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, IF
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Get number of existing CC.
         IP(IF) = CLBUFF(5,IF)
         NKEY = 3
         TYPKOL = 7
         FLXKOL = 1
         RAKOL = 2
         DECKOL = 3
         IF (LREC.GT.3) NKEY = 4
         CALL FNDCOL (NKEY, KEYS, 8, FA, CLBUFF(1,IF), KOLS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1016) IRET, IF
            CALL MSGWRT (7)
            END IF
 20      CONTINUE
      CLNSTR = CLNSTR + 1
      CALL RFILL (7, 0.0, RECORD)
      XCENTR = CATR(KRCRP)
      YCENTR = CATR(KRCRP+1)
C                                       Prepare to CLEAN
      BUFF1(1) = PATCH
      BUFF1(2) = PATCH
      CALL QPUT (BUFF1, 0, 6, 2)
C                                       Put field descriptor vector
C                                       into AP
      APFDL = 1 + MFIELD * 6
      APFDV = LENCBU + 2
      CALL QWD
      CALL QPUT (FNFLD, APFDV, APFDL, 2)
C                                       addresses are integer
      APFDV = APFDV + 1
      DO 25 IF = 1,MFIELD
         CALL QWD
         CALL QPUT (APCFLD(IF), APFDV, 1, 1)
         APFDV = APFDV + 6
 25      CONTINUE
C                                       Start CLEANing.
      CALL QWD
      APFDV = LENCBU + 2
      CALL QMULCL (2, APFDV, APBEAM, 0, NCOMPC)
      ITER = CLNSTR
      STRTOT = ITER - 1
C                                       Init Clean component Min flux
      CCMIN = 1.0E12
C                                       Note inital clean flux
      CLNFLX = TFLUXG
C                                       If first pass
      IF (ITER.LE.1) THEN
C                                       If FMIN is very negative
         IF (FMIN.LT.-99) THEN
C                                       FMIN is min % milli-change
            MINCHG = ABS(FMIN)/(100.*1000.)
            FMIN = 0.
            WRITE (MSGTXT,1030) MINCHG*100
            CALL MSGWRT(3)
         ELSE
            MINCHG = 0.0
            END IF
C                                       On start of clean, reinit CC Min
         PCCMIN = 1.0E12
C                                       End if first pass
         END IF
C                                       Speed up clean.
      XFAC = MAPLIM / MAX( RESMAX, 1.0E-20)
      IF (XFAC.LT.0.0) XFAC = -XFAC
      XPROD = XFAC**FACTOR
      XFAC = XPROD
      ATLIM = 0.0
      NCBUFF = NCOMPC
C                                       Begin loop.
 30   NCBUFF = NCBUFF + 1
C                                       See if time to get more from
C                                       the AP.
      IF (NCBUFF.LE.NCOMPC) GO TO 40
         CALL QWR
C                                       Check AP roller.
         CALL QROLL (APSIZ, BUFF1, JBUFSZ, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Get next load of comp.
         CALL QGET (BUFF1, 0, LENCBU, 2)
         CALL QWD
C                                       Start CLEANing again.
         CALL QMULCL (2, APFDV, APBEAM, 0, NCOMPC)
         NCBUFF = 1
C                                       Process a Component.
 40      IPOINT = 3 + (NCBUFF-1) * 4
C                                       Get field and position.
         IX = IROUND (BUFF1(IPOINT+1))
         IY = IROUND (BUFF1(IPOINT+2))
         IF = IROUND (BUFF1(IPOINT+3))
C                                       Check field number.
         IF ((IF.GT.0) .AND. (IF.LE.MFIELD)) GO TO 42
            IRET = 5
            WRITE (MSGTXT,1040) IF, MFIELD
            CALL MSGWRT (8)
            GO TO 999
 42      RECORD(RAKOL) = ((IX - ICNTRX(IF)) * CELLSG(1)) / 3600.
     *      - XPOFF(IF)
         RECORD(DECKOL) = ((IY - ICNTRY(IF)) * CELLSG(2)) / 3600.
     *      - YPOFF(IF)
         FLUXG(IF) = FLUXG(IF) + BUFF1(IPOINT) * GAIN
         TFLUXG = TFLUXG + BUFF1(IPOINT) * GAIN
C                                       Stored RA and Dec refer to
C                                       the catalogd CLEAN map.
         RECORD(FLXKOL) = BUFF1(IPOINT) * GAIN
         RECORD(TYPKOL) = 0.0
         NCLNG(IF) = NCLNG(IF) + 1
         IP(IF) = IP(IF) + 1
         CALL TABIO ('WRIT', 0, IP(IF), RECORD, CLBUFF(1,IF), IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Test CC flux/number limits
         XFLUX = BUFF1(IPOINT)
         CCFLUX = XFLUX
C                                       Record Min CC Magnitude
         CCMIN = MIN( CCMIN, ABS(XFLUX))
C                                       Check for iteration limit.
         IF (ITER.GE.CLNLIM) GO TO 110
C                                       Check for minimum CLEAN flux
         IF (ABS (XFLUX).LT.FMIN) GO TO 90
C                                       IF flux is negative, stop at neg
         IF (FMIN.LT.0.0 .AND. XFLUX.LT.0.0) GO TO 90
C                                       Stop if clean is diverging
         IF (ABS(XFLUX) .GT. 2.*CCMIN) GO TO 90
C                                       Check minimum algorithm flux
C                                       Don't stop in last 5%
         IF ((ABS (XFLUX).LT.MAPLIM*(1.0+ATLIM)) .AND.
     *    (ITER.LE.ITLIM)) GO TO 100
C                                       If flux is dropping, jump out
C                                       To do a proper sub
         IF ((ABS(FLUXG(IF)) .LT. 0.9*STRFLX(IF)) .AND.
     *      CCMIN.GT.1.1*PCCMIN) THEN
C                                       Attempt fix major and minor
C                                       Cycle differences
            MSGTXT = 'PROBABLE MAJOR - MINOR CYCLE differences'
            CALL MSGWRT(3)
            MSGTXT = 'LEAVING MINOR CYCLE Early'
            CALL MSGWRT(3)
            GO TO 100
            END IF
         STRFLX(IF) = MAX( STRFLX(IF), ABS(FLUXG(IF)))
C                                       Loop back for next clean.
         ITER = ITER + 1
         ATLIM = ATLIM + XFAC / ITER
         GO TO 30
C                                       Finished minor CLEAN loop.
C                                       (Note XFLUX modifed by METSCA)
 90   CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1090) XFLUX, PREFIX, ITER
      CALL MSGWRT (4)
      GO TO 120
 100  CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1100) XFLUX, PREFIX, ITER
      CALL MSGWRT (4)
      GO TO 120
 110  CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1110) XFLUX, PREFIX, ITER
      CALL MSGWRT (4)
C                                       Save maximum residual
 120  RESMAX = ABS (BUFF1(IPOINT))
      LASTIT = ITER - STRTOT
C                                       See if finished.
      FINISH = .FALSE.
      IF (ITER.GE.CLNLIM) FINISH = .TRUE.
      IF (RESMAX.LE.FMIN) FINISH = .TRUE.
C                                       Stop if clean is diverging
      IF (ABS(CCFLUX) .GT. 2.*CCMIN) THEN
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
      IF (FMIN.LT.0.0 .AND. CCFLUX.LT.0.0 .AND. .NOT.FINISH) THEN
         FINISH = .TRUE.
         MSGTXT = 'Negative Clean Component Found, Stopping'
         CALL MSGWRT(4)
         END IF
C                                       If slow progress, stop
      CHANGE = CLNFLX - TFLUXG
C                                       if nozero total flux, scale
      IF (ABS(TFLUXG).GT.1.0E-15) CHANGE = CHANGE / TFLUXG
C                                       if small change, flag stop
      IF (ABS(CHANGE).LT.MINCHG.AND..NOT.FINISH) THEN
         FINISH = .TRUE.
         MSGTXT = 'Flux density Change is small in a Major Cycle, '//
     *      'stopping'
         CALL MSGWRT(5)
         END IF
C                                       Write total flux density
      XFLUX = TFLUXG
      CALL METSCA (XFLUX, PREFIX, LERR)
      WRITE (MSGTXT,1120,ERR=150) XFLUX, PREFIX
      IF (FINISH) THEN
         CALL MSGWRT (5)
      ELSE
         CALL MSGWRT (3)
         END IF
C                                       Jump here on print error
 150  CONTINUE
C                                       Output last buffers and close.
      DO 200 IF = 1,MFIELD
C                                       Write field info.
         XFLUX = FLUXG(IF)
         CALL METSCA (XFLUX, PREFIX, LERR)
         IF (MFIELD.GT.1) THEN
            WRITE (MSGTXT,1125) IF, XFLUX, PREFIX, NCLNG(IF)
            IF (FINISH) THEN
               CALL MSGWRT (5)
            ELSE
               CALL MSGWRT (3)
               END IF
            END IF
         CALL TABIO ('CLOS', 0, NCLNG(IF), RECORD, CLBUFF(1,IF), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1015) IRET, IF
            CALL MSGWRT (8)
            END IF
 200     CONTINUE
C                                       Set CLNSTR to the current clean
C                                       counter (NCOMP - 1 ).
      CLNSTR = ITER
C                                       Record Pass CC Minimum
      PCCMIN = MIN( CCMIN, PCCMIN)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXACLN: CATIO ERROR',I3,' FIELD ',I5)
 1010 FORMAT ('MXACLN: ERROR',I3,' OPENING CC FILE, FIELD',I5)
 1015 FORMAT ('MXACLN: ERROR',I3,' CLOSING CC FILE, FIELD',I5)
 1016 FORMAT ('MXACLN: ERROR',I3,' FINDING REQUIRED CC COLUMNS FIELD',
     *   I5)
 1030 FORMAT ('Stopping Clean if Major Cycle Flux density change < ',
     *   F10.5,' %')
 1040 FORMAT ('MXACLN: ILLEGAL FIELD NUMBER',I3,', NOT 1-',I3)
 1090 FORMAT ('Reached min. CLEAN flux density=',F8.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1100 FORMAT ('Reached minimum algorithm flux =',F8.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1110 FORMAT ('Reached Iter. limit, Max. resid=',F8.3,1X,A5,'Jy',
     *   ' iter=',I9)
 1115 FORMAT ('Minimum residual peak was       = ',F8.3,1X,A5,'Jy')
 1120 FORMAT ('Total CLEANed flux density          =  ',F9.3,1X,A5,'Jy')
 1125 FORMAT ('Field',I5,' CLEANed flux density =',F8.3,1X,A5,'Jy',
     *   I9,' comps')
      END
      SUBROUTINE MXGAUS (IFIELD)
C-----------------------------------------------------------------------
C   MXGAUS creates the arrays in the AP necessary for restoring with
C   an eliptical gaussian.
C   Input:
C     IFIELD    I    Field number for the map.
C     BMAJ      R    Major Axis (FWHP in cells)
C     BMIN      R    Minor axis size (FWHP in cells).
C     BPA       R    Position angle of restoring beam (deg ).
C     CELLSG(1)  R    RA grid spacing (asec).
C     CELLSG(2)  R    Dec grid spacing (asec).
C     MAPROT    R    Coordinate rotation (deg).
C   OUTPUT:
C     GAUSAA     R    Coefficient of u**2
C     GAUSBB     R    Coefficient of u*v
C     GAUSCC     R    Coefficient of v**2
C     Normalization constant for the restoring Gaussian left in
C                  AP location 0.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PSTD.INC'
      REAL      WT, WT1, XNX2, XNY2, XNXNY, TEMP, AK, AM, AN, TA, TB
      INTEGER   I, JLIM, K, IFIELD, NX, NY, NAPRES, NAPGAU, NAPEXP,
     *   NAPEX1, NAPGRD, NAPCMP, NAPCMS, NAPRS1, ONENY, TWONY, WRK1,
     *   WRK2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMPR.INC'
C-----------------------------------------------------------------------
      NX = FLDSZ(1,IFIELD)
      NY = FLDSZ(2,IFIELD)
C                                       Set AP assignments.
      ONENY = NY
      TWONY = 2 * ONENY
      NAPRES = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPEXP = NAPRES + WRK1
      NAPGRD = NAPEXP + WRK1
      NAPCMP = NAPGRD + WRK2
      NAPGAU = NAPCMP + WRK2
      NAPEX1 = NAPGAU + WRK1
      NAPCMS = NAPEX1 + WRK1
C                                       Do a little arithmetic to put
C                                       the beam parameters into useable
C                                       form. Convert to sigma, then to
C                                       coeficients of U**2, U*V, and
C                                       V**2 the last of which is return
C                                       for later use.
      TA = BMAJ * PI / 1.1774
      TB = BMIN * PI / 1.1774
      AM = COS ((BPA+MAPROT)*DG2RAD)
      AN = SIN ((BPA+MAPROT)*DG2RAD)
      XNX2 = NX * CELLSG(1)
      XNY2 = NY * CELLSG(2)
      XNXNY = ABS (XNX2 * XNY2)
      XNX2 = XNX2 ** 2
      XNY2 = XNY2 ** 2
      GAUSAA = (TA*TA*AM*AM + TB*TB*AN*AN) / (XNY2)
      GAUSCC = (TA*TA*AN*AN + TB*TB*AM*AM) / (XNX2)
      GAUSBB = ((TB*TB-TA*TA) * AN*AM) / (XNXNY )
C                                       Create array NAPEX1.
      DO 10 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         BUFF1(I) = (-GAUSBB * K)
 10      CONTINUE
C                                       Load NAPEX1 into AP.
      CALL QPUT (BUFF1, NAPEX1, ONENY, 2)
      CALL QWD
C                                       Create array NAPGAU.
      DO 20 I = 1,NY
         K = I - 1
         IF (I.GT.NY/2) K = K - NY
         AK = K
         BUFF1(I) = -0.5 * GAUSAA * AK * AK
 20      CONTINUE
C                                       Load NAPGAU into AP.
      CALL QPUT (BUFF1, NAPGAU, ONENY, 2)
      CALL QWD
C                                       Compute normalization factor.
C                                       Check if already done this
C                                       field.
      IF (NORFAC(IFIELD).GT.-1.0E19) GO TO 200
C                                       Initialize array NAPEXP.
         CALL QPUT (0.0, 0, 1, 2)
         CALL QWD
         CALL QVCLR (NAPEXP, 1, ONENY)
         CALL QVSADD (NAPEXP, 1, 0, NAPEXP, 1, ONENY)
C                                       Initialize NAPRES for summing
C                                       wt.
         NAPRS1 = NAPRES - 1
         WRK1 = ONENY + 1
         CALL QVCLR (NAPRS1, 1, WRK1)
         CALL QWR
C                                       Calculate sum of the weights.
         JLIM = NX / 2 + 1
         DO 40 I = 1,JLIM
            TEMP = -0.5 * GAUSCC * (I - 1.0) ** 2
            CALL QPUT (TEMP, 1, 1, 2)
            CALL QWD
            CALL QVSADD (NAPGAU, 1, 1, NAPRES, 1, ONENY)
            CALL QVADD (NAPRES, 1, NAPEXP, 1, NAPRES, 1, ONENY)
            CALL QVEXP (NAPRES, 1, NAPRES, 1, ONENY)
            CALL QSVE (NAPRS1, 1, NAPRS1, WRK1)
C                                       Prepare NAPEXP for next pass.
            CALL QVADD (NAPEXP, 1, NAPEX1, 1, NAPEXP, 1, ONENY)
            CALL QWR
C                                       Get intermediate sum.
            IF (I.EQ.1) CALL QGET (WT1, NAPRS1, 1, 2)
 40         CONTINUE
C                                       Get sum of the weights.
         CALL QGET (WT, NAPRS1, 1, 2)
         CALL QWD
C                                       Compute weight over whole plane.
         WT = 2.0 * WT - WT1
         NORFAC(IFIELD) = LOG (1.0 / WT)
C                                       Store WT in AP loc 0.
 200  CALL QPUT (NORFAC(IFIELD), 0, 1, 2)
      CALL QWD
C
      RETURN
      END
      SUBROUTINE MXTIMR (MSG)
C-----------------------------------------------------------------------
C  MXTIMR writes a label and time information.
C   Input:  MSG   C*12    Label
C-----------------------------------------------------------------------
      CHARACTER MSG*12
      INTEGER   ITIME(3), IOCNT(2)
      REAL      TIME
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL ZTIME (ITIME)
      CALL ZCPU (TIME,IOCNT)
      WRITE (MSGTXT,1000,ERR=999) MSG, ITIME, TIME
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A12, ' Time=',I3.2,2(':',I2.2),' CPU time=',F10.2)
      END
      SUBROUTINE MXCSUM (BUFFER)
C-----------------------------------------------------------------------
C   Subroutine to sum the flux densities of the CLEAN components in
C   a the CC files describes in the /MAPDES/
C     Inputs:
C       BUFFER(2048)   R    Work buffer
C     Inputs from /MAPDES/:
C       MFIELD         I    Number of fields
C       NCLNG(*)       I    Number of components per field.
C       CCDISK(*)      I    Disk numbers of the fields.
C       CCCNO(*)       I    Catalog slot numbers of the fields.
C       CCVER(*)       I    Version numbers of the fields.
C     Output to /MAPDES/:
C       FLUXG(*)       R    Sum of the clean components per field.
C       TFLUXG         R    Sum of components in all fields.
C-----------------------------------------------------------------------
      CHARACTER KEYS(4)*8
      INTEGER   LIMIT, I
      INTEGER   IFIELD, IERR, IRET, NREC, LUNC,  LREC,
     *   NKEY, KOLS(4), RAKOL, DECKOL, FLXKOL, TYPKOL
      LOGICAL   F
      REAL      BUFFER(768), RECORD(7)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL)
      DATA KEYS /'DELTAX  ','DELTAY  ','FLUX    ','TYPE OBJ'/
      DATA F /.FALSE./
      DATA LUNC /28/
C-----------------------------------------------------------------------
C                                       Loop over fields
      DO 100 IFIELD = 1,MFIELD
C                                       Open CLEAN component file.
         LREC = 3
         NREC = 1
         NKEY = 0
         CALL CATIO ('READ', CCDISK(IFIELD), CCCNO(IFIELD), KLNBLK,
     *      'REST', BUFFER, IRET)
         IF ((IRET.EQ.0) .OR. (IRET.GT.4)) GO TO 10
            WRITE (MSGTXT,1000) IRET, IFIELD
            GO TO 100
 10      CALL TABINI ('READ', 'CC', CCDISK(IFIELD), CCCNO(IFIELD),
     *      CCVER(IFIELD), KLNBLK, LUNC, NKEY, NREC, LREC,
     *      BUFFER(513), BUFFER, IRET)
         IF (IRET.EQ.0) GO TO 20
            WRITE (MSGTXT,1010) IRET, IFIELD
            GO TO 100
 20      CALL RFILL (7, 0.0, RECORD)
         NKEY = 3
         TYPKOL = 7
         IF (LREC.GT.3) NKEY = 4
         CALL FNDCOL (NKEY, KEYS, 8, F, BUFFER, KOLS, IRET)
         IF (IRET.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IRET, IFIELD
            CALL MSGWRT (8)
            GO TO 90
C                                       Loop summing components
 30      LIMIT = NCLNG(IFIELD)
         DO 60 I = 1,LIMIT
            CALL TABIO ('READ', 0, I, RECORD, BUFFER, IRET)
            IF (IRET.LE.0) GO TO 40
               WRITE (MSGTXT,1030) IRET, I, IFIELD
               CALL MSGWRT (8)
               GO TO 90
C                                       Sum component.
 40         IF (IRET.LT.0) GO TO 50
            IF ((LREC.GT.3) .AND. (RECORD(TYPKOL).NE.0.0)) GO TO 50
               FLUXG(IFIELD) = FLUXG(IFIELD) + RECORD(FLXKOL)
               TFLUXG = TFLUXG + RECORD(FLXKOL)
 50         CONTINUE
 60         CONTINUE
C                                       Close CLNFIL.
 90      CALL TABIO ('CLOS', 0, I, RECORD, BUFFER, IERR)
         IF (IERR.EQ.0) GO TO 100
            WRITE (MSGTXT,1090) IERR, IFIELD
            GO TO 990
 100     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MXCSUM: ERROR',I3,' READING CATBLK FIELD',I5)
 1010 FORMAT ('MXCSUM: ERROR',I3,' OPENING FILE FIELD',I5)
 1020 FORMAT ('MXCSUM: ERROR',I3,' FINDING REQUIRED CC COLUMNS FIELD',
     *   I5)
 1030 FORMAT ('MXCSUM: READ ERROR',I3,' RECORD ',I8,' FIELD',I5)
 1090 FORMAT ('MXCSUM: ERROR',I3,' CLOSING FILE FIELD',I5)
      END
      SUBROUTINE MXTELL (TVFLD, TVPASS, IRET)
C-----------------------------------------------------------------------
C   MXTELL checks the TELL file to see if anything is waiting for MX.
C   If so, it picks up the parms (via GTTELL), interprets them, and
C   sends them back to the rest of the task to handle.
C   In/out:   TVFLD   I     Field to display, <= 0 no TV
C             TVPASS  I     =1 => do TV display with init and ask
C   Output:   IRET    I     0 => okay, 99 => abort now
C-----------------------------------------------------------------------
      CHARACTER OPTELL*4
      INTEGER   TVFLD, TVPASS, IRET
      INTEGER   NPARMS, SCRTCH(256), IERR, ITV, IROUND
      REAL      XGN, XFLUX, XMINPT, XNITER, XBMAJ, XBMIN, XBPA, XFCTR,
     *   XXDOTV, XXMAXP
      HOLLERITH XMETHD
      LOGICAL   EQUAL
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'MX.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MXTLL/ XGN, XFLUX, XMINPT, XNITER, XBMAJ, XBMIN, XBPA,
     *   XFCTR, XXDOTV, XMETHD, XXMAXP
C-----------------------------------------------------------------------
C                                       Check TELL file
      IRET = 0
      NPARMS = 11
      CALL GTTELL (NPARMS, OPTELL, XGN, SCRTCH, IERR)
      WRITE (MSGTXT,1000) OPTELL
      IF (IERR.NE.0) CALL MSGWRT (5)
      IF (IERR.EQ.2) FINISH = .TRUE.
      IF (IERR.EQ.3) IRET = 99
      IF (IERR.NE.1) GO TO 999
C                                       install parms
      IF (XGN.GT.1.0E-5) GAIN = XGN
      FMIN = XFLUX
      IF (RESMAX.LE.FMIN) FINISH = .TRUE.
      IF (XMINPT.GT.3) MINPCH = XMINPT + 0.001
C                                       number iterations, quit
      IF (XNITER.GE.1.0) CLNLIM = XNITER + 0.01
      IF (CLNSTR.GT.CLNLIM) FINISH = .TRUE.
C                                       beam, no restore
      IF ((XBMAJ.EQ.0.0) .OR. (XBMIN.EQ.0.0)) GO TO 20
         DOREST = (XBMAJ.GT.-0.01) .AND. (CLNLIM.GE.1)
         IF (XBMAJ.GE.0.0) THEN
            BMAJ = XBMAJ
            BMIN = XBMIN
            BPA = XBPA
            END IF
 20   FACTOR  = XFCTR
C                                       model method
      CALL H2CHR (4, 1, XMETHD, CMETH)
      EQUAL = CMETH .EQ. 'DFT '
      IF ((EQUAL) .AND. (METHOD.GE.0)) METHOD = -1
      EQUAL = CMETH .EQ. 'GRID'
      IF ((EQUAL) .AND. (METHOD.GE.0)) METHOD = 1
      EQUAL = CMETH .EQ. '    '
      IF ((EQUAL) .AND. (METHOD.GE.0)) METHOD = 0
C                                       TV display: which field
      ITV = IROUND (XXDOTV)
      IF (NTVDEV.LE.0) ITV = 0
      IF (ITV.GT.XNFLD) ITV = 1
      IF ((TVPASS.LT.10) .AND. (ITV.NE.TVFLD)) TVPASS = 1
      TVFLD = ITV
C                                       maximum pixels searched
      MAXPIX = XXMAXP + 0.5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Received ',A4,' operation from TELL')
      END
