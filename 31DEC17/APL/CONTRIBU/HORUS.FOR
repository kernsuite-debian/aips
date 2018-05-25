LOCAL INCLUDE 'HORUS.INC'
C                                       Local include for HORUS
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, CNOIN, SEQOUT, CHINC, DISKIN, DISKO, NUMHIS,
     *   JBUFSZ, SCRTCH(512)
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XOPTY,
     *   XNAMOU(3), XUVWT
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSTOK*4, OPTYPE*4,
     *   NAMOUT*12, HISCRD(10)*64, UVWT*2
      REAL      XSIN, XDISIN, XTIME(8), XUVRA(2), XBAND, XFREQ, XQUAL,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XCHINC, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XSOUT,
     *   XDISO, XIMSI(2), XCELLS(2), XSH(2), XTAPER(2), XUVBOX,
     *   XZERO(5), XXTYP, XYTYP, XXPARM(10), XYPARM(10), XGUARD(2),
     *   XBADD(10), BUFF1(MABFSS), UMAX, VMAX, SUMAX, SVMAX
      COMMON /BUFRS/ BUFF1, SCRTCH, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XQUAL,
     *   XTIME, XUVRA, XXSTOK, XOPTY, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XBCHAN, XECHAN, XCHINC, XSUBA, XDOCAL, XGUSE, XDOPOL, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH, XNAMOU, XSOUT, XDISO, XIMSI,
     *   XCELLS, XSH, XTAPER, XUVWT, XUVBOX, XZERO, XXTYP, XYTYP,
     *   XXPARM, XYPARM, XGUARD, XBADD,
     *   SEQIN, SEQOUT, DISKIN, DISKO, CHINC, CNOIN, NUMHIS, UMAX, VMAX,
     *   SUMAX, SVMAX
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XSTOK, UVWT, OPTYPE, NAMOUT,
     *   HISCRD
LOCAL END
      PROGRAM HORUS
C-----------------------------------------------------------------------
C! Applies calibration and/or editing and images uv data.
C# UV Map Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006, 2008
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
C   Images a uv data file with optional editing and calibration.
C   Adverbs:
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      SOURCES....Source list.  One output file will be made per
C                 source.  '*' = all; a "-" before a source name
C                 means all except ANY source named.
C      TIMERANG...Time range of the data to be imaged. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      UVRANGE....(Minimum,Maximum) baseline (kilolambda) in map.
C      STOKES.....The desired Stokes type of the output data:
C                 'I','V','Q', or 'U'
C      OPTYPE.....The type of image, 'SUM' = all channels in the
C                 same image, 'LINE' = each channel in a separate
C                 plane.  For 'LINE' only IF=BIF is processed.
C      BIF........First IF to image. 0=>all.
C      EIF........Highest IF to image. 0=>all higher than BIF
C      BCHAN......First channel to image. 0=>all.
C      ECHAN......Highest channel to image. 0=>all higher than BCHAN
C      SUBARRAY...Subarray number to image. 0=>all.
C      DOCALIB....If true (>0) then calibrate the data using
C                 information in the specified Cal (CL or SN).
C      GAINUSE....version number of the CL table to apply to
C                 multisource files or the SN table for single
C                 source files.  0 => highest.
C      DOPOL......If > 0 then correct data for instrumental
C                 polarization as represented in the AN table.  This
C                 correction  is only useful if PCAL has been run or
C                 feed polarization parameters have been otherwise
C                 obtained.
C      BLVER......Version number of the baseline based calibration
C                 (BL) table to appply. <0 => apply no BL table,
C                 0 => highest.
C      FLAGVER....specifies the version of the flagging table to be
C                 applied. 0 => highest numbered table.
C                 <0 => no flagging to be applied.
C      DOBAND.....If true (>0) then correct the data for the shape of
C                 the antenna bandpasses using the BP table specified
C                 by BPVER. The correction has three modes:
C                 (a) if DOBAND=1 all entries for an antenna in the
C                 table are averaged together before correcting the data
C                 (b) if DOBAND=2 the entry nearest in time is used to
C                 correct the data.
C                 (c) if DOBAND=3 the table entries are interpolated in
C                 time and the data are then corrected.
C      BPVER......Specifies the version of the BP table to be applied
C                 plotted (if APARM(8)=2) or applied (if DOBAND > 0
C                 and APARM(8) .NE. 2). 0 => highest numbered table.
C                 <0 => no bandpass correction to be applied.
C      SMOOTH.....Specifies the type of spectral smoothing to be
C                 applied to a uv database . The default is not to apply
C                 any smoothing. The elements of SMOOTH are as follows:
C                 SMOOTH(1) = type of smoothing to apply:
C                   0 => no smoothing
C                   1 => Hanning
C                   2 => Gaussian
C                   3 => Boxcar
C                   4 => Sinc (i.e. sin(x)/x)
C                 SMOOTH(2) = the "diameter" of the function, i.e.
C                   width between first nulls of Hanning triangle
C                   and sinc function, FWHM of Gaussian, width of
C                   Boxcar. Defaults (if < 0.1) are 4, 2, 2 and 3
C                   channels for SMOOTH(1) = 1 - 4.
C                 SMOOTH(3) = the diameter over which the convolving
C                   function has value - in channels.
C                   Defaults: 1, 3, 1, 4 times SMOOTH(2) used when
C                   input SMOOTH(3) < net SMOOTH(2).
C      OUTNAME....1st 4 characters of the output image name. The
C                 following 8 are the 1st 8 of the source name.
C      OUTSEQ.....Output image name (seq. #).   0 => highest unique
C      OUTDISK....Disk drive # of output image. 0 => highest with
C                 space for the file.
C      IMSIZE.....(X,Y) image size in pixels.  Power of 2.
C      CELLSIZE...(X,Y) pixel separation in asec.
C      SHIFT......(X,Y) shift of map center from phase center in
C                 asec.  Map center = Phase center + shift. If X>0
C                 & Y>0, source shifts to south-west (down & right).
C      UVTAPER....(U,V) gaussian taper (kilolambda) at 30% level
C      UVWTFN.....Weighting function of (u-v) place.
C                 blank=>Uniform; 'NA'=>Natural
C      UVBOX......(U,V) box size for smoothing.  Only used if data
C                 XY sorted
C      ZEROSP.....Zero-spacing value of I,Q,U,V, Weight
C      XTYPE......Convolution function type in X-direction
C                 1=Pillbox, 2=exponential, 3=Sinc, 4=Exp*Sinc,
C                 5=Spheroidal.  <= 0 or > 5  -> 5.
C      YTYPE.....Convolution function type in Y-direction
C      XPARM.....Array containing parameters for XTYPE.
C                See HELP UVnTYPE when n=convolution type.
C      YPARM.....Array containing parameters for YTYPE.
C      BADDISK....A list of disks on which scratch files are not to
C                 be placed.  This will not affect the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   NUMSOU, IRET
      LOGICAL   DOWANT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INTEGER   SULIST(XSTBSZ)
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'HORUS '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL MAPRIN (PRGM, DOWANT, NUMSOU, SULIST, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop over sources.
      CALL MAPRUV (DOWANT, NUMSOU, SULIST, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE MAPRIN (PRGN, DOWANT, NUMSOU, SULIST, JERR)
C-----------------------------------------------------------------------
C   MAPRIN gets input parameters for HORUS, finds input file and
C   prepares the list of sources.  All selection criteria
C   except the source name is filled into the commons in DSEL.INC,
C   DMPR.INC and DGDS.INC
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      DOWANT  L    If true sources listed are selected
C      NUMSOU  I    Number of sources to process, 0=>all
C      SULIST  I(*) Source number list.
C      JERR    I    Error code: 0 => ok
C                             5 => catalog troubles
C                             8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PSTD.INC'
      CHARACTER PRGN*6
      LOGICAL   DOWANT
      INTEGER   NUMSOU, SULIST(*), JERR
C
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   NPARM, IROUND, IERR, I, NX, NY, LUN, IX, IY
      REAL      CATR(256), AUMAX, AVMAX, UTFACT, VTFACT, DU, DV
      LOGICAL   F, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Init
      JBUFSZ = MABFSS * 2
      NUMHIS = 0
      JERR = 0
      CALL SELINI
      DO3DIM = .FALSE.
C                                       Get input parameters.
C                                       Fixed PPM 1996.09.30: was 212
      NPARM = 213
      CALL SETUP (PRGN, NPARM, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
      JERR = 5
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
      CALL H2CHR (2, 1, XUVWT, UVWT)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
C                                       Get CATBLK from old file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Save input header
      CALL RCOPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       BADDISK
      DO 60 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 60      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 70 I = 1,30
         SOURCS(I) = XSOUR(I)
         CALSOU(I) = '                '
 70      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = '    '
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = 0.0
      UVRNG(2) = 0.0
      STOKES = XSTOK
      IF ((STOKES(1:1).NE.'I') .AND. (STOKES(1:1).NE.'Q') .AND.
     *   (STOKES(1:1).NE.'U') .AND. (STOKES(1:1).NE.'V') . AND.
     *   (STOKES(1:1).NE.'R') .AND. (STOKES(1:1).NE.'L')) STOKES = 'I'
      XSTOK = STOKES
C                                       Trap for Stokes R or L
      IF (STOKES.EQ.'R   ') STOKES = 'RR  '
      IF (STOKES.EQ.'L   ') STOKES = 'LL  '
      IF ((OPTYPE.NE.'SUM ') .AND. (OPTYPE.NE.'LINE')) THEN
         OPTYPE = 'LINE'
         IF (CATBLK(KINAX+JLOCF).LE.1) OPTYPE = 'SUM '
         END IF
      CHINC = IROUND (XCHINC)
      IF (CHINC.LE.0) CHINC = 1
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF (OPTYPE.EQ.'LINE') THEN
         IF (EIF.GT.BIF) THEN
            WRITE (MSGTXT,1070) EIF, BIF
            CALL MSGWRT (6)
            END IF
         EIF = BIF
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      DOAPPL = F
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Imaging parameters
      MFIELD = 1
      DO3DIM = .FALSE.
      FLDSZ(1,1) = IROUND (XIMSI(1))
      FLDSZ(2,1) = IROUND (XIMSI(2))
C                                       Check image size
      NX = FLDSZ(1,1)
      NY = FLDSZ(2,1)
      CALL POWER2 (NX, IX)
      CALL POWER2 (NY, IY)
      IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG) .OR. (NY.NE.IY)
     *   .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) THEN
         WRITE (MSGTXT,1075) NX, NY
         JERR = 9
         GO TO 990
         END IF
      NCHAVG = 1
      IF (OPTYPE.EQ.'SUM ') NCHAVG = (ECHAN - BCHAN + 1) *
     *   (EIF - BIF + 1)
      UNFBOX = IROUND (XUVBOX)
      NXUNF = NX
      NYUNF = NY
      CTYPX = IROUND (XXTYP)
      CTYPY = IROUND (XYTYP)
      DO 80 I = 1,10
         XPARM(I) = XXPARM(I)
         YPARM(I) = XYPARM(I)
 80      CONTINUE
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      DOZERO = (ABS (XZERO(1)) + ABS (XZERO(2)) + ABS (XZERO(3)) +
     *   ABS (XZERO(4)) + ABS (XZERO(5))) .GT. 1.0E-20
      DO 85 I = 1,5
         ZEROSP(I) = XZERO(I)
 85      CONTINUE
      TFLUXG = 0.0
      DOTAPE = (XTAPER(1) + XTAPER(2)) .GT. 1.0E-10
      TAPERU = XTAPER(1)
      TAPERV = XTAPER(2)
      NXBEM(1) = FLDSZ(1,1)
      NYBEM(1) = FLDSZ(2,1)
      XSHIFT(1) = XSH(1)
      YSHIFT(1) = XSH(2)
      CELLSG(1) = XCELLS(1)
      CELLSG(2) = XCELLS(2)
C                                       Set limits.
      DU = RAD2AS / (NX * ABS (CELLSG(1)))
      DV = RAD2AS / (NY * ABS (CELLSG(2)))
      UMAX = (NX/2 - 1.0) * DU
      VMAX = (NY/2 - 1.0) * DV
C                                       Fix for gridding
C                                       3.0 corresponds to the
C                                       interpolation fn half
C                                       width.
      AUMAX = MAX (6.0, XPARM(1)) * DU / UMAX
      AVMAX = MAX (6.0, YPARM(1)) * DV / VMAX
C                                       taper factors
      UTFACT = 1.0
      VTFACT = 1.0
      IF ((DOTAPE) .AND. (TAPERU.GT.0.0)) UTFACT = EXP (LOG(0.3) *
     *   ((UMAX*0.7) / (1000. * TAPERU))**2)
      IF ((DOTAPE) .AND. (TAPERV.GT.0.0)) VTFACT = EXP (LOG(0.3) *
     *   ((VMAX*0.7) / (1000. * TAPERV))**2)
      UTFACT = SQRT (UTFACT)
      VTFACT = SQRT (VTFACT)
C                                       default guard band widths
      IF (XGUARD(1).LT.-0.001) THEN
         XGUARD(1) = AUMAX
      ELSE IF ((XGUARD(1).LE.0.001) .OR. (XGUARD(1).GT.0.9)) THEN
         XGUARD(1) = 0.30 * UTFACT
         END IF
      IF (XGUARD(2).LT.-0.001) THEN
         XGUARD(2) = AVMAX
      ELSE IF ((XGUARD(2).LE.0.001) .OR. (XGUARD(2).GT.0.9)) THEN
         XGUARD(2) = 0.30 * VTFACT
         END IF
C                                       UVGRID does do guard band
C                                       for HORUS
      GUARDB(1) = MAX (XGUARD(1), AUMAX)
      GUARDB(2) = MAX (XGUARD(2), AVMAX)
      SUMAX = UMAX * (1.0 - 0.35 * UTFACT)
      SVMAX = VMAX * (1.0 - 0.35 * VTFACT)
      UMAX = UMAX * (1.0 - GUARDB(1))
      VMAX = VMAX * (1.0 - GUARDB(2))
C
      DXTIME = 1.0 / 86400.0
C                                       Get source list
      IUDISK = FVOL(1)
      IUCNO = FCNO(1)
      IXLUN = 28
      CALL SOUFIL (JERR)
      IF (JERR.NE.0) GO TO 999
      DOWANT = DOSWNT
      NUMSOU = NSOUWD
      CALL COPY (30, SOUWAN, SULIST)
C                                       Reset values in /SELCAL/
      DO 90 I = 1,30
         SOURCS(I) = ' '
 90      CONTINUE
      NSOUWD = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('WARNING: EIF',I3,' CHANGED TO BIF',I3,
     *   '.  ONLY ONE IF IN LINE MODE')
 1075 FORMAT ('IMAGE SIZE ',I5,'X ',I5,' IS ILLEGAL')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE MAPRUV (DOWANT, NUMSOU, SULIST, IRET)
C-----------------------------------------------------------------------
C   MAPRUV loops through the selected sources and images them.
C   The history and relevant tables are also copied.
C   Input:
C      DOWANT  L     If true sources listed are selected
C      NUMSOU  I     Number of sources to process, 0=>all
C      SULIST  I(*)  Source number list.
C   Output:
C       IRET   I     Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   DOWANT
      INTEGER   NUMSOU, SULIST(*), IRET
C
      CHARACTER NAME*12, VELTYP*8, VELDEF*8, CALCOD*4, CHVEL(4)*4,
     *   CLAOUT*6
      HOLLERITH CATH(256), CATUH(256)
      LOGICAL   T, F, DOAVG, DOAPPT, SMILE, SINGLE, TABLE, EXIST,
     *   FITASC, CREA
      INTEGER    SOUCUR, MAXSOU, SLOOP, DSKB, DSKM, BCNO, MCNO, IFLD,
     *   IERR, SBUFF(512), I, SLUN, IDSOU, QUAL, ERRCNT, INOGRP, TMPVER,
     *   SUBTMP, CATSAV(256), ITEMP, SAVBND, SUFQID, DPOSAV
      REAL      RPARM, VIS, CATR(256), CATSR(256), CATUVR(256)
      DOUBLE PRECISION  BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, CATD(128), OLDFRQ, AVGFRQ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      DOUBLE PRECISION   LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      REAL     FLUX(4,MAXIF)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),   (CATUV, CATUH, CATUVR)
      EQUIVALENCE (SCRBLK, CATSR)
      DATA CHVEL /'LSR ','BARY','HELI','RADI'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      ERRCNT = 0
      SMILE = F
      TMPVER = CLVER
      DOAPPT = DOAPPL
      DOAVG = OPTYPE .EQ. 'SUM '
C                                       If averaging data get average
      IF (DOAVG) THEN
         CALL MAPRFQ (AVGFRQ)
      ELSE
         AVGFRQ = FREQ
         END IF
C                                       Get input reference freq.
      OLDFRQ = CATD(KDCRV+JLOCF)
C                                       Check if single source file
      SLUN = 27
      CALL ISTAB ('SU', FVOL(1), FCNO(1), 1, SLUN, SBUFF, TABLE, EXIST,
     *   FITASC, IERR)
      SINGLE = (.NOT.EXIST) .OR. (.NOT.TABLE)
C                                       Open source table
      IF (.NOT.SINGLE) THEN
         CALL SOUINI ('READ', SBUFF, FVOL(1), FCNO(1), 1, CATUV, SLUN,
     *      INOGRP, VELTYP, VELDEF, SUFQID, SOUCUR, SUKOLS, SUNUMV,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         MAXSOU = SBUFF(5)
C                                       Setup for single source case:
      ELSE
         MAXSOU = 1
         NUMSOU = 0
         DO 80 I = 1,30
            SOURCS(I) = '                '
 80         CONTINUE
         END IF
C                                       Loop here over sources
      DO 500 SLOOP = 1,MAXSOU
         SOUCUR = SLOOP
C                                       Read source table
 100     IF (.NOT.SINGLE) THEN
            CALL TABSOU ('READ', SBUFF, SOUCUR, SUKOLS, SUNUMV, IDSOU,
     *         SOURCS, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       See if this source wanted.
         IF (NUMSOU.GT.0) THEN
            DO 120 I = 1,NUMSOU
               IF ((IDSOU.EQ.SULIST(I)) .AND. DOWANT) GO TO 140
               IF ((IDSOU.EQ.SULIST(I)) .AND. (.NOT.DOWANT)) GO TO 500
               IF (IDSOU.EQ.SULIST(I)) GO TO 500
 120           CONTINUE
            END IF
         IF (DOWANT .AND. (NUMSOU.GT.0)) GO TO 500
C                                       Create header, fiddle tables etc
 140     SUBTMP = SUBARR
         SUBARR = 0
         DPOSAV = DOPOL
         DOPOL = -1
         SAVBND = DOBAND
         DOBAND = 0
         CALL UVGET ('INIT', RPARM, VIS, IERR)
         DOPOL = DPOSAV
         DOAPPL = F
         CLVER = CLUSE
         IF (IERR.NE.0) GO TO 450
C                                       Close again
         CALL UVGET ('CLOS', RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 450
         DOBAND = SAVBND
C                                       Put new values in CATBLK.
         NAME = NAMOUT
         CLAOUT = '      '
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, 'IBEM  ', NAME, CLAOUT,
     *      SEQOUT)
C                                       Name after source.
         IF (.NOT.SINGLE) THEN
            IF (NAMOUT(1:4).EQ.'    ') THEN
               NAME = SOURCS(1)(1:12)
            ELSE
               NAME = NAMOUT(1:4) // SOURCS(1)(1:8)
               END IF
            END IF
C                                       Put in common
         MNAME = NAME
         MCLASS = CLAOUT
         MDISKN = DISKO
         MSEQ = SEQOUT
C                                       Velocity etc. info IF=BIF; ref.
C                                       pixel is set in IMCREA.
         IF (.NOT.SINGLE) THEN
            CATD(KDRST) = RESTFQ(BIF)
            CATD(KDARV) = LSRVEL(BIF)
C                                       Velocity reference frame
            ITEMP = 3
            IF (VELTYP(1:4).EQ.CHVEL(1)) ITEMP = 1
            IF (VELTYP(1:4).EQ.CHVEL(2)) ITEMP = 2
            IF (VELDEF(1:4).EQ.CHVEL(3)) ITEMP = 2
            IF (VELDEF(1:4).EQ.CHVEL(4)) ITEMP = ITEMP + 256
            CATBLK(KIALT) = ITEMP
            END IF
C                                       Make sure there is data
         IF (CATBLK(KIGCN).LE.0) GO TO 450
C                                       In case averaging in frequency
         IF (DOAVG) THEN
            FREQ = AVGFRQ
            END IF
C                                       Create output files
         CALL COPY (256, CATBLK, SCRBLK)
         IFLD = 0
         CREA = T
         DSKB = DISKO
         MCLASS = STOKES(1:1) // 'BEAM '
C                                       Reset Frequency reference pixel
C                                       since IMCREA will comput it
C                                       including the effects of
C                                       averaging.
         CATSR(KRCRP+JLOCF) = CATUVR(KRCRP+KLOCFY)
C                                       Beam
         CALL IMCREA (IFLD, CREA, DSKB, BCNO, CHINC, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 450
         CALL KEYCOP (DISKIN, CNOIN, DSKB, BCNO, IERR)
C                                       Note: freq. ref pixel is set in
C                                       the history routine.
         BEMVOL(1) = DSKB
         CNOBEM(1) = BCNO
         IFLD = 1
         CREA = T
         DSKM = DISKO
         MCLASS = STOKES(1:1) // 'MAP  '
C                                       Image
         CALL IMCREA (IFLD, CREA, DSKM, MCNO, CHINC, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 450
         CALL KEYCOP (DISKIN, CNOIN, DSKM, MCNO, IERR)
C                                       Note: freq. ref pixel is set in
C                                       the history routine.
         CCDISK(1) = DSKM
         CCCNO(1) = MCNO
C                                       Save CATBLK - UVGET will modify
         CALL COPY (256, CATBLK, CATSAV)
C                                       Set subarray(s)
         SUBARR = SUBTMP
C                                       Make image and beam
         CALL MAPR (DSKB, BCNO, DSKM, MCNO, IERR)
         SMILE = SMILE .OR. (IERR.EQ.0)
         IF (IERR.NE.0) GO TO 450
C                                       History
         CLVER = TMPVER
         DOAPPL = DOAPPT
         CALL MAPRHI (DSKM, MCNO, F)
         CALL MAPRHI (DSKB, BCNO, T)
         CLVER = CLUSE
         DOAPPL = F
         NCFILE = NCFILE - 2
         GO TO 500
C                                       Error, close input file
C                                       then resume.
 450        CALL UVGET ('CLOS', RPARM, VIS, IERR)
            ERRCNT = ERRCNT + 1
C                                       Tell which source
            WRITE (MSGTXT,1450) SOURCS(1)
            CALL MSGWRT (6)
 500     CONTINUE
C                                       Make sure bad files
C                                       destroyed
      IRET = 1
      IF (SMILE) IRET = 0
C                                       Close source table
      IF (.NOT.SINGLE)
     *   CALL TABIO ('CLOS', 1, SOUCUR, SBUFF, SBUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('CANNOT OVERWRITE OLD FILE')
 1450 FORMAT ('PROBLEM WITH SOURCE: ',A)
      END
      SUBROUTINE MAPRFQ (AVGFRQ)
C-----------------------------------------------------------------------
C   Routine to determine the average frequency of data to be averaged
C   into an image.  Uses FRQTAB to get table of frequencies.
C   Output:
C      AVGFRQ    D  The average frequency
C-----------------------------------------------------------------------
      DOUBLE PRECISION AVGFRQ
C
      INTEGER   LUN, IRET, KINCIF, KINCF, JIF, JFRQ, FRQPNT, NUMFRQ
      DOUBLE PRECISION SUM
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /29/
C-----------------------------------------------------------------------
      AVGFRQ = FREQ
C                                       Get frequency table
      CALL FRQTAB (IUDISK, IUCNO, LUN, CATUV, FRQSEL, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Increments
      KINCIF = 1
      IF ((KLOCIF.GE.0) .AND. (KLOCIF.GT.KLOCFY))
     *   KINCIF = CATUV(KINAX+KLOCFY)
      KINCF = 1
      IF ((KLOCIF.GE.0) .AND. (KLOCFY.GT.KLOCIF))
     *   KINCF = CATUV(KINAX+KLOCIF)
C                                       Sum frequencies
      NUMFRQ = 0
      SUM = 0.0D0
      DO 40 JIF = BIF,EIF
         DO 40 JFRQ = BCHAN,ECHAN
            NUMFRQ = NUMFRQ + 1
            FRQPNT = (JIF-1) * KINCIF + (JFRQ-1) * KINCF + 1
            SUM = SUM + FREQG(FRQPNT)
 40         CONTINUE
      AVGFRQ = SUM / NUMFRQ
C
 999  RETURN
      END
      SUBROUTINE MAPRHI (DISK, CNO, ISBEM)
C-----------------------------------------------------------------------
C   MAPRHI copies and updates history file.  It also copies any tables
C   extension files.
C    Input:
C     DISK   I  Output file disk number
C     CNO    I  Output file catalog slot number.
C     ISBEM  L  True if beam => don't copy tables
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO
      LOGICAL   ISBEM
C
      CHARACTER NOTTYP(12)*2, HILINE*72, LABEL*8, UTYPE*2, NAME*12,
     *   CLASS*6, CHCONV(6)*8
      HOLLERITH CATH(256)
      INTEGER   IERR, I, LUN1, LUN2, NONOT, I1, I2
      REAL      CATR(256), CATUVR(256)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE (CATBLK, CATH, CATR),   (CATUV, CATUVR)
      DATA LUN1, LUN2 /28,29/
      DATA T /.TRUE./
      DATA CHCONV /'Pill box', 'Expontl ', 'Sinx / x',
     *             'Exp*Sinc', 'Spheroid', 'Bess*Exp'/
      DATA NONOT /12/
      DATA NOTTYP /'CL','FG','NX','SU','SN','CC','CH','BP','AN','BL',
     *   'FQ', 'TY'/
C-----------------------------------------------------------------------
C                                        Read CATBLK.
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', SCRTCH, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) GO TO 999
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISK, CNOIN, CNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
      CALL HENCOO (TSKNAM, NAME, CLASS, CATBLK(KIIMS), DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
      CALL HITIME (TSTART, TEND, LUN2, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IF range
      WRITE (HILINE,2001) TSKNAM, BIF, EIF
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Chan range
      WRITE (HILINE,2002) TSKNAM, BCHAN, ECHAN, CHINC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPTYPE
      WRITE (HILINE,2003) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flagging
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,2004) TSKNAM, FGVER
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Translation
      WRITE (HILINE,2008) TSKNAM, XSHIFT(1), YSHIFT(1)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Taper
      WRITE (HILINE,2009) TSKNAM, TAPERU, TAPERV
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Annulus
      WRITE (HILINE,2010) TSKNAM, UVRNG
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Weighting,
      WRITE (HILINE,2011) TSKNAM, UVWT, UNFBOX
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Zero spacing flux density
      IF (DOZERO) THEN
         WRITE (HILINE,2013) TSKNAM, ZEROSP
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Convolving function.
      WRITE (HILINE,2014) TSKNAM, CHCONV(CTYPX), CHCONV(CTYPY)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2015) TSKNAM, CTYPX, (XPARM(I), I = 1,3)
      CALL HIADD (LUN2, HILINE,  SCRTCH,  IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2016) TSKNAM, (XPARM(I), I = 4,7)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2016) TSKNAM, (XPARM(I), I = 8,10)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2017) TSKNAM, CTYPY, (YPARM(I), I = 1,3)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2016) TSKNAM, (YPARM(I), I = 4,7)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2016) TSKNAM, (YPARM(I), I = 8,10)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
C                                        Spectral smoothing
      IF (SMOOTH(1).GT.0.5) THEN
         I1 = SMOOTH(1) + 0.5
         I2 = SMOOTH(3) + 0.5
         WRITE (HILINE,2020) TSKNAM, I1, SMOOTH(2), I2
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Calibration
      IF (DOCAL) THEN
C                                       Table
         WRITE (HILINE,2021) TSKNAM, CLUSE
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Polzn correction
      IF (XDOPOL.GT.0.0) THEN
         WRITE (HILINE,2022) TSKNAM, DOPOL
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BL table
      IF (XBLVER.GE.0.0) THEN
         WRITE (HILINE,2023) TSKNAM, BLVER
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       BP table
      IF (XDOBND.GT.0.0) THEN
         WRITE (HILINE,2024) TSKNAM, DOBAND
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,2025) TSKNAM, BPVER
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                      Add any other history
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 150 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 200
 150        CONTINUE
         END IF
      NUMHIS = 0
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                       Copy tables
      IF (.NOT.ISBEM) CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN,
     *   DISK,  CNOIN, CNO, CATBLK, SCRTCH, SCRTCH(257), IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Clear status
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISK, CNO, NAME, CLASS, CATBLK(KIIMS),
     *   UTYPE, NLUSER, 'CLWR', SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPRHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 2001 FORMAT (A6,' BIF =',I4,', EIF =',I4,'/ IF range')
 2002 FORMAT (A6,' BCHAN =',I4,', ECHAN =',I4,' CHINC =',I4,
     *   '/ Chan range')
 2003 FORMAT (A6,' OPTYPE =''',A,''' / cont/line flag')
 2004 FORMAT (A6,' / Edited using FG table version',I3)
 2005 FORMAT (A6,' STOKES = ''',A4,''' / Stokes type')
 2008 FORMAT (A6,' SHIFT=',F10.4,', ',F10.4)
 2009 FORMAT (A6,' UVTAPER=',E12.5,', ',E12.5,
     *   ' /UV taper kilolambda')
 2010 FORMAT (A6,' UVRANGE=',E12.5,', ',E12.5,
     *   ' /UV annulus kilolambda')
 2011 FORMAT (A6,' UVWTFN=''',A,''' , UVBOX =',I5,' /Uniform weight')
 2013 FORMAT (A6,' ZEROSP=',F10.3,', ',F10.3,', ',F10.3,', ',F10.3,
     *   ', ',F10.3)
 2014 FORMAT (A6,' /Convolution functions - U: ',A8,3X,
     *   'V: ',A8)
 2015 FORMAT (A6,' XTYPE =',I3,' XPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2016 FORMAT (A6,' , ',E12.5,', ',E12.5,', ',E12.5,', ',E12.5)
 2017 FORMAT (A6,' YTYPE =',I3,' YPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2020 FORMAT (A6,' SMOOTH = ',I1,',',F6.1,',',I4,
     *   ' / Spectral smoothing parms')
 2021 FORMAT (A6,' GAINUSE =',I3,' / CL table')
 2022 FORMAT (A6,' DOPOL=',I2,', polarization correction made')
 2023 FORMAT (A6,' BL table ',I3,' applied to data')
 2024 FORMAT (A6,' BP correction done, DOBAND = ',I2)
 2025 FORMAT (A6,' BP correction used BP table ',I2)
      END
      SUBROUTINE MAPR (DSKB, BCNO, DSKM, MCNO, IRET)
C-----------------------------------------------------------------------
C   MAPR images the data looping over channels, data is calibrated in
C   blocks of 8 channels.
C   Input:
C      DKSBEM  I  Disk number of beam
C      BCNO  I  Catalog slot number of beam
C      DKSMAP  I  Disk number of image
C      MCNO  I  Catalog slot number of image
C   Output:
C      IRET    I  Return error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   DSKB, BCNO, DSKM, MCNO, IRET
C
      INTEGER   IFRW(50), IFLD, SCRGRD, SCRWRK,  LOOP, LOOP2,
     *   NPASS, BCSAV, ECSAV, NCHPP, LIMIT, DSKTMP, KNOTMP, CHANUM,
     *   CHANIM
      LOGICAL   T, F, CREA, DOINIT, DOBEAM, DOSEL, DOGCOR, DOLINE
      REAL      UU, VV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'HORUS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T, F /.TRUE.,.FALSE./
C                                       No. channels per cal. pass.
      DATA NCHPP /8/
C-----------------------------------------------------------------------
C                                       Initial values
      CREA = F
      DOINIT = T
      DOBEAM = F
      DOSEL = T
      DOGCOR = T
      BCSAV = BCHAN
      ECSAV = ECHAN
      DSKTMP = 0
      KNOTMP = 0
      SCRWRK = 0
      SCRGRD = 0
      DOLINE = OPTYPE.EQ.'LINE'
C                                       Number of passes
      NPASS = 1
      IF (DOLINE) NPASS = ((ECHAN - BCHAN) / NCHPP) + 1
C                                       Outer loop
      DO 500 LOOP = 1,NPASS
C                                       Select no. channels.
         DOSEL = T
         LIMIT = 1
         IF (DOLINE) THEN
            BCHAN = BCSAV + (LOOP-1) * NCHPP
            ECHAN = BCHAN + NCHPP - 1
            ECHAN = MIN (ECHAN, ECSAV)
            LIMIT = ECHAN - BCHAN + 1
            END IF
         DO 400 LOOP2 = 1,LIMIT
            DOUNIF = UVWT.NE.'NA'
C                                       Channel number
            CHANIM = 1
            IF (DOLINE) THEN
               CHANUM = BCHAN + LOOP2 - 1
               CHANIM = ((CHANUM - BCSAV) / CHINC) + 1
C                                       This channel wanted
               IF (MOD ((CHANUM-BCSAV), CHINC) .NE. 0) GO TO 400
               WRITE (MSGTXT,1000) CHANUM
               CALL MSGWRT (4)
               END IF
C                                       Beam
            IFLD = 0
            CALL OLDMAP (IFLD, DSKTMP, KNOTMP, DSKB, BCNO, SCRGRD,
     *         SCRWRK, LOOP2, CHANIM, CREA, DOINIT, DOBEAM, DOSEL,
     *         DOGCOR, CHINC, JBUFSZ, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Image
            IFLD = 1
            DOINIT = F
            DOSEL = F
            CALL OLDMAP (IFLD, DSKTMP, KNOTMP, DSKM, MCNO, SCRGRD,
     *         SCRWRK, LOOP2, CHANIM, CREA, DOINIT, DOBEAM, DOSEL,
     *         DOGCOR, CHINC, JBUFSZ, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
 400        CONTINUE
         IF (LOOP.EQ.1) THEN
C                                       data outside grid?
            IF ((GUAXAU.GT.UMAX) .OR. (GUAXAV.GT.VMAX)) THEN
               MSGTXT = '**** WARNING data discarded outside usable'
     *            // ' part of UV plane ****'
               CALL MSGWRT (6)
               IF (GUAXAU.GT.UMAX) THEN
                  UU = UMAX/GUAXAU * ABS(CELLSG(1))
                  WRITE (MSGTXT,1410) 'U', GUAXAU, UMAX
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1411) UU, ABS(CELLSG(1))
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1412) 'U', XGUARD(1)
                  CALL MSGWRT (6)
                  END IF
               IF (GUAXAV.GT.VMAX) THEN
                  VV = VMAX/GUAXAV * ABS(CELLSG(2))
                  WRITE (MSGTXT,1410) 'V', GUAXAV, VMAX
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1411) VV, CELLSG(2)
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1412) 'V', XGUARD(2)
                  CALL MSGWRT (6)
                  END IF
               END IF
            IF ((GUAXBU.GT.SUMAX) .OR. (GUAXBV.GT.SVMAX)) THEN
               MSGTXT = '**** WARNING data included out of inner'
     *            // ' portion of UV plane ****'
               CALL MSGWRT (6)
               MSGTXT = '**** Watch for high-frequency & other'
     *            // ' poor cleaning effects ****'
               CALL MSGWRT (6)
               END IF
            END IF
 500     CONTINUE
C                                       Restore  channel range
         BCHAN = BCSAV
         ECHAN = ECSAV
C                                       History
      NUMHIS = 1
      IF (NVIS.LE.GUACNT) THEN
         WRITE (HISCRD(NUMHIS),1500) NVIS
      ELSE
         WRITE (HISCRD(NUMHIS),1501) GUACNT, NVIS
         END IF
      MSGTXT = HISCRD(NUMHIS)
      CALL MSGWRT (4)
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1502) BEMMAX(1)
      MSGTXT = HISCRD(NUMHIS)
C                                       Delete scratch files
      IF (NSCR.GT.0) THEN
         DO 900 LOOP = 1,NSCR
            IFRW(LOOP) = 2
 900        CONTINUE
         CALL MAPCLR (NSCR, SCRVOL, SCRCNO, IFRW, SCRTCH)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Starting channel ',I5)
 1410 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1411 FORMAT ('**** Use cellsize <',F10.5,' not',F10.5,
     *   ' to get all data ****')
 1412 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
 1500 FORMAT ('Used',I10,' visibilities')
 1501 FORMAT ('Used ONLY',I10,' out of',I10,' visibilities')
 1502 FORMAT ('Sum of weights = ', 1PE15.5)
      END
