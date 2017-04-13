LOCAL INCLUDE 'SPFLG.BUF'
C                                        Local include for SPFLG buffers
      INTEGER   ABUF, IOBUF, MAXBUF, DIMB1, DIMB2, BIGBUF, BIGPT(6)
      COMMON /BUFSIZ/ ABUF, IOBUF, MAXBUF, DIMB1, DIMB2, BIGBUF, BIGPT
LOCAL END
LOCAL INCLUDE 'SPFLG.INC'
C                                                          Include DSPF
C                                       Local include for SPFLG
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION FREQIF, CATID(128), NPOINT
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK,
     *   CATIH(256), FCOPER(2), FCSFLG, FCREAS(6)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSTOK*4, TTIME(2)*12,
     *   STKFLG*4, USTFLG*4, SNAMES(XSTBSZ)*16
      REAL      XSIN, XDISIN, XDOCAT, XIN2S, XIN2D, XDOHST, XTIME(8),
     *   XBAND, XFREQ, XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT(50),
     *   XBASE(50), XUVRA(2), XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER,
     *   XBLVER, XFLAG, XFGOUT, XDOBND, XBPVER, XSMOTH(3), DPARM(10),
     *   DOCENT, XBADD(10),
     *   RPARM(20), VIS(3,256), PIXRNG(2,5), START, STOP, TIMES(32769),
     *   MTIMES(32679), CATIR(256), FCTIME(2), FCLIPR(2), FCTVAV,
     *   FCTVSC, TXPND, CSMOTH, LCSMOT
      LOGICAL   ISINGL, DESEL, LQUICK, MENUOK, DOWEDG, DOSOUR, GPH1OK,
     *   GPH3OK, GPH4OK, DOLABL
      INTEGER   NFAIL, NNFLAG, DISKIN, SEQIN, CNOIN, INEXT, INVER,
     *   IMSIZE(2), BUFFER(512), CATIMG(256), NXANT, LLOCSU, NXBASL,
     *   IXANT(50), IXBASL(50), SMODE, SEQOUT, LBASL, LTYPE, LSMOO,
     *   NUMAN(1025), DISKOU, CNOOUT, LBASLV, LWINTV(4,4), TVFILE,
     *   LSTOKS, PLSMOO, PLSTOK, PLBASL, PLTYPE, FCVERS, FCLUN,
     *   FCBUF(512), FCNUMB, FCBASL(2), FCIF(2), FCCHAN(2), FCTVTY,
     *   FCTVBL, FCTVIF, FCTVST, FCTVWI(4), MAXSOU, ILSTOK(4),
     *   MSOU(32769), FCSOUR, IBL0, MBL, NOANTS(3,MXBASE), LSCAN,
     *   PLSCAN, SCFILE, JBUFSZ, DOBASL, OFGVER, ITRTYP, LSNAME, FGFLAG,
     *   MAXLAB, XYCENT(2), TFORM, STRANS, INSNUM, MAXMEN, DOIFS,
     *   LCIF(2), IPIECE, NPIECE, MPIECE, NSTOKS
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XDOCAT, XIN2S,
     *   XIN2D, XDOHST, XXSOUR, XXCALC, XTIME, XXSTOK, XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT, XBASE, XUVRA, XSUBA,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFGOUT, XDOBND,
     *   XBPVER, XSMOTH, DPARM, DOCENT, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XSTOK, TTIME, STKFLG,
     *   USTFLG, SNAMES
      COMMON /UVIMGC/ CATIMG
      COMMON /INFOLS/ FREQIF, NPOINT, RPARM, VIS, PIXRNG, START, STOP,
     *   TIMES, MTIMES, NFAIL, NNFLAG, ISINGL, DESEL, DOBASL, DOIFS,
     *   LQUICK, MENUOK, DISKIN, SEQIN, CNOIN, INEXT, INVER, IMSIZE,
     *   BUFFER, NXANT, NXBASL, IXANT, IXBASL, SMODE, SEQOUT, LBASLV,
     *   LBASL, LTYPE, LSMOO, NUMAN, DISKOU, CNOOUT, LWINTV, TVFILE,
     *   LSTOKS, PLSMOO, PLSTOK, PLBASL, PLTYPE, FCVERS, FCLUN, FCBUF,
     *   ILSTOK, MAXSOU, MSOU, DOWEDG, IBL0, MBL, NOANTS, LLOCSU,
     *   DOSOUR, LSCAN, PLSCAN, SCFILE, GPH1OK, GPH3OK, JBUFSZ, TXPND,
     *   OFGVER, ITRTYP, LSNAME, FGFLAG, GPH4OK, DOLABL, MAXLAB, XYCENT,
     *   TFORM, STRANS, INSNUM, MAXMEN, CSMOTH, LCSMOT, LCIF, IPIECE,
     *   NPIECE, MPIECE, NSTOKS
      COMMON /FCTABL/ FCTIME, FCLIPR, FCTVAV, FCTVSC, FCOPER, FCSFLG,
     *   FCREAS, FCNUMB, FCBASL, FCCHAN, FCIF, FCSOUR, FCTVTY, FCTVBL,
     *   FCTVIF, FCTVST, FCTVWI
      EQUIVALENCE (CATIMG, CATIR, CATIH, CATID)
C                                                          End DSPF
LOCAL END
      PROGRAM SPFLG
C-----------------------------------------------------------------------
C! Interactive uv data editor for spectroscopic data
C# UV Calibration EXT-appl TV-appl Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2016
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
C   Task SPFLG grids uv data in TB sort order to make an image of the
C   selected form (amp, phase, rms) of the data.  It then displays the
C   grid, suitably smoothed to fit, on the TV and offers enhancement
C   and editing options.  The resulting editing commands are applied to
C   the input data set.  This image is channel-time-bl-Stokes-IF, where
C   TVFLG has channel and bl switched.
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAMEIN        File name to be imaged
C      INCLASS        CLAIN         File class to be imaged
C      INSEQ          SEQIN         File sequence number
C      INDISK         DISKIN        Disk volume on which file resides
C      DOCAT          XDOCAT        Catalog main grid file? 2 => quick
C                                   return, grid file and exit
C      IN2SEQ         XIN2S         Sequence number of cataloged grid
C      IN2DISK        XIN2D         Disk number of catalogued disk
C      SOURCES        XSOUR(4,30)   Sources selected
C      CALCODE        SELCAL        which calibrator codes to include
C      TIMERANG       XTIME(8)      Timerange
C      STOKES         XSTOK         Stokes' parameter
C      SELBAND        XBAND         Bandwidth to select (kHz)
C      SELFREQ        XFREQ         Frequency to select (MHz)
C      FREQID         XFQID         Freq. ID to select.
C      BIF            XBIF          IF number: begin
C      EIF            XEIF          IF number: end
C      BCHAN          XBCHAN        Channel number: begin
C      ECHAN          XECHAN        Channel number: end
C      ANTENNAS       XANT(50)      Antenna numbers
C      BASELINE       XBASE(50)     Antenna numbers to pair up
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      SUBARRAY       SUBARR        Subarray: 0 => all
C      DOCALIB        DOCAL         Calibrate?
C      GAINUSE        GAUSE         CL version to apply.
C      FLAGVER        FGVER         Flag table version
C      DOBAND                       Bandpass calibration?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      DPARM          DPARM         Control info.:
C                                   (1) init display type: amp, phase...
C                                   (3) True => use ant-pair
C                                   (4) divide by flux
C                                   (6) y-axis interval in seconds
C                                   (7) init IF to display
C                                   (8) initial channel to diplay
C                                   (9,10) init pixrange in display
C      BADDISK        XBADD(10)     Disks to avoid for scratch
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      LOGICAL   ONEIF
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SPFLG.BUF'
      INTEGER   PABUF, PIOBUF, PMAXB, PDIMB1, PDIMB2, PI, PJ
      PARAMETER (PABUF = 120)
      PARAMETER (PMAXB = 8)
      PARAMETER (PIOBUF = (UVBFSS))
      PARAMETER (PDIMB1 = (MAXCIF))
      PARAMETER (PDIMB2 = (MAXCIF+10))
      PARAMETER (PI = (PMAXB * (PIOBUF + 2*PDIMB1) + 3*MAXCIF + 5))
      PARAMETER (PJ = ((3*PABUF+2) * PDIMB2 + 3*PABUF + 3 * PIOBUF))
C     REAL      BIGBOY(PBIGB)
C     PARAMETER (PBIGB = (PJ))
      REAL      BIGBOY(2)
      INTEGER   IPI, IPJ, KIGBOY(2)
      LONGINT   PBIGB
      EQUIVALENCE (BIGBOY, KIGBOY)
      DATA PRGM /'SPFLG '/
C-----------------------------------------------------------------------
      ABUF = PABUF
      IOBUF = PIOBUF
      JBUFSZ = 2 * IOBUF
      MAXBUF = PMAXB
      DIMB1 = PDIMB1
      DIMB2 = PDIMB2
      IPI = PI
      IPJ = PJ
      BIGBUF = MAX (IPI, IPJ)
      BIGPT(1) = 1
      BIGPT(2) = BIGPT(1) + DIMB2
      BIGPT(3) = BIGPT(2) + DIMB2
      BIGPT(4) = BIGPT(3) + 3 * ABUF * (DIMB2+1)
      BIGPT(5) = BIGPT(4) + IOBUF
      BIGPT(6) = BIGPT(5) + IOBUF
C                                       get inputs, ...
      CALL SPFLIN (PRGM, ONEIF, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do the gridding
      CALL SPFLSC (BIGBUF, BIGBOY, KIGBOY, PBIGB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do the editing
      IF (.NOT.RQUICK) THEN
         CALL SPFLGR (ONEIF, BIGBUF, BIGBOY(BIGPT(1)+PBIGB),
     *      KIGBOY(BIGPT(1)+PBIGB), IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Do history
         CALL SPFLHI (BIGBOY(BIGPT(4)+PBIGB), BIGBOY(BIGPT(5)+PBIGB))
         END IF
C                                       close down
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SPFLIN (PRGM, ONEIF, IRET)
C-----------------------------------------------------------------------
C   SPFLIN gets the inputs for SPFLG.
C   Inputs:
C      PRGM   C*6   Task name
C   Output:
C      ONEIF  L     T => input has <= 1 IF
C      IRET   I     Error code: 0 ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      LOGICAL   ONEIF
      INTEGER   IRET
C
      CHARACTER STAT*4, CLSOUT*6, SCLASS*6, UTYPE*2, MTYPE*2, KEYS(4)*8
      INTEGER   NPARM, IERR, IROUND, I, LUN, LOCS(4), VALUES(4),
     *   KEYTYP(4)
      LOGICAL   MATCH
      REAL      CATUR(256), CATR(256)
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATUV, CATUR),  (CATBLK, CATR)
      DATA SCLASS /'SPFLGR'/
      DATA KEYS /'BIF','EIF','BCHAN','ECHAN'/
C-----------------------------------------------------------------------
      FCLUN = 27
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 284
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      LQUICK = RQUICK
      RQUICK = .FALSE.
      IF (IRET.NE.0) GO TO 999
      IF ((NPOPS.GT.NINTRN) .OR. (NTVDEV.LE.0)) THEN
         IRET = 4
         IF (NPOPS.GT.NINTRN) THEN
            MSGTXT = 'TV TASKS ARE RESERVED FOR INTERACTIVE USERS'
         ELSE
            MSGTXT = 'YOU HAVE NOT BEEN ASSIGNED A TV'
            END IF
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL RFILL (10, 0.0, PIXRNG)
      MAXLAB = 3
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.5) THEN
            WRITE (MSGTXT,1015) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *         NLUSER
         ELSE
            WRITE (MSGTXT,1016) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
            END IF
         GO TO 990
         END IF
      IF (STAT.NE.'REST') THEN
         MSGTXT = 'STATUS OF INPUT IS ' // STAT //
     *      '. CLRSTAT OR WAIT UNTIL FILE ISN''T IN USE'
         IERR = 7
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      LLOCSU = ILOCSU
      STRANS = ICOR0
      ISINGL = LLOCSU.LT.0
C                                       Is this image TB?
      IF (ISORT(1:1).NE.'T') THEN
         IRET = 2
         MSGTXT = 'DATA IN ' // ISORT //
     *      ' SORT ORDER, NOT THE REQUIRED T* ORDER'
         GO TO 990
         END IF
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      IUDISK = DISKIN
      USEQ = SEQIN
      IUSEQ = SEQIN
      DO 30 I = 1,30
         SOURCS(I) = XSOUR(I)
 30      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      CALL H2CHR (4, 1, XXCALC, SELCOD)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      IF (XSTOK.EQ.' ') XSTOK = 'HALF'
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (ECHAN-BCHAN.LT.3) THEN
         BCHAN = ECHAN - BCHAN + 1
         WRITE (MSGTXT,1030) BCHAN
         IRET = 8
         GO TO 990
         END IF
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
         ONEIF = .TRUE.
      ELSE
         BIF = IROUND (XBIF)
         BIF = MAX (1, BIF)
         BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
         EIF = IROUND (XEIF)
         IF ((EIF.LT.BIF) .OR. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *      EIF = CATBLK(KINAX+JLOCIF)
         ONEIF = CATBLK(KINAX+JLOCIF).LE.1
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      TXPND = MAX (0.0, DPARM(5)) / (24.0 * 3600.0)
C                                       Set cal flag
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = (DPARM(2).GT.0.0) .OR. (TYPUVD.GT.0)
      DOXCOR = DPARM(2).LT.2.0
C                                       Antennas
      CALL SETANT (50, XANT, XBASE, NXANT, NXBASL, IXANT, IXBASL, DESEL)
      IF ((NXANT.LE.0) .AND. (NXBASL.GT.0)) THEN
         CALL COPY (NXBASL, IXBASL, IXANT)
         NXANT = NXBASL
         NXBASL = 0
         END IF
      CALL FILL (50, 0, ANTENS)
C                                       limit to one subarray
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) THEN
         MSGTXT = 'using subarray 1'
         CALL MSGWRT (6)
         SUBARR = 1
         END IF
C                                       ext. file versions
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
C                                        Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
      CALL COPY (256, CATBLK, CATUV)
      GPH4OK = .FALSE.
      DOLABL = .FALSE.
C                                       locate old grid file
      SEQOUT = IROUND (XIN2S)
      DISKOU = IROUND (XIN2D)
      CNOOUT = 1
      IF (XDOCAT.LE.0.0) SEQOUT = 0
      IF ((XDOCAT.GT.0.0) .AND. (SEQOUT.GT.0)) THEN
         CLSOUT = SCLASS
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', DISKOU, CNOOUT, NAMEIN, CLSOUT, SEQOUT,
     *      MTYPE, NLUSER, STAT, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            IF (IERR.NE.5) THEN
               WRITE (MSGTXT,1015) IERR, NAMEIN, CLSOUT, SEQOUT,
     *            DISKOU, NLUSER
            ELSE
               WRITE (MSGTXT,1016) NAMEIN, CLSOUT, SEQOUT, DISKOU,
     *            NLUSER
               END IF
            CALL MSGWRT (6)
            SEQOUT = -SEQOUT
         ELSE
            CALL CATIO ('READ', DISKOU, CNOOUT, CATBLK, 'WRIT', BUFFER,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1020) IERR
               IRET = 5
               GO TO 990
               END IF
            CALL COPY (256, CATBLK, CATIMG)
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKOU
            FCNO(NCFILE) = CNOOUT
            FRW(NCFILE) = 1
C                                       set BCHAN, BIF, antennas
            CALL CATKEY ('READ', DISKOU, CNOOUT, KEYS, 4, LOCS, VALUES,
     *         KEYTYP, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1021) IERR
               IRET = 5
               GO TO 990
               END IF
            BCHAN = VALUES(3)
            ECHAN = VALUES(4)
            IF (JLOCIF.GE.0) THEN
               BIF = VALUES(1)
               EIF = VALUES(2)
            ELSE
               BIF = 1
               EIF = 1
               END IF
            END IF
         END IF
C                                       Quick return mode?
      IF ((XDOCAT.GT.1.5) .AND. (SEQOUT.LE.0)) THEN
         RQUICK = .TRUE.
         CALL RELPOP (IRET, BUFFER, IERR)
         END IF
      MAXMEN = 480
      CSMOTH = 0.0
      LCSMOT = 0.0
      GO TO 999
C                                       message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I5,' DISK=',
     *   I3,' USID=',I5)
 1016 FORMAT (A12,'.',A6,'.',I5,' DISK=',I3,' USID=',I5,' NOT FOUND')
 1020 FORMAT ('SPFLIN: ERROR',I3,' READING CATBLK ')
 1021 FORMAT ('SPFLIN: ERROR',I3,' READING WORK FILE KEYWORDS')
 1030 FORMAT (I2,' CHANNELS ARE TOO FEW TO BE USEFUL, RESET BCHAN',
     *   ' AND ECHAN')
      END
      SUBROUTINE SPFLSC (BIGDIM, BIGBOY, KIGBOY, PBIGB, IRET)
C-----------------------------------------------------------------------
C   SPFLSC grids the uv data into a SC file by figuring out the start
C   and stop times, the image size, and calling GRIDTC.  It makes the
C   list of antenna numbers versus baseline grid number.
C   Input:
C      BIGDIM   I      Total size of BIGBOY
C   Output:
C      BIGBOY   R(*)   Large IO buffers
C      KIGBOY   I(*)   Large IO buffers - equivalenced
C      IRET     I   Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   BIGDIM, KIGBOY(*), IRET
      REAL      BIGBOY(*)
      LONGINT   PBIGB
C
      CHARACTER OUTNAM*12, OUTCLS*6, SCNAME*9, TELESC*8
      REAL      LTEMP, TEMP
      INTEGER   II, I, NV, NSK, IY, LY, ISOU, LSOU, LINE, PLINE, IROUND,
     *   J, IAR, IANT, JANT, IFILL, ISUB, NBIN(11), NCOUNT, JJ, JBOTM,
     *   NWORDS
      LOGICAL   FIRST
      DOUBLE PRECISION CTIME, PTIME, DELTAT, TIME, LTIME, TSIGMA
      HOLLERITH CATH(256)
      LONGINT   P1, PI, PJ, PII
      INCLUDE 'SPFLG.INC'
      INTEGER   NODATA(MAXANT,MAXANT)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'SPFLG.BUF'
      EQUIVALENCE (CATH, CATUV)
C-----------------------------------------------------------------------
C                                       get time range
      FIRST = .TRUE.
      IUCNO = CNOIN
      IXLUN = 28
      INSNUM = 0
      CALL SOUFIL (IRET)
      IF ((NSOUWD.EQ.1) .AND. (DOSWNT)) INSNUM = SOUWAN(1)
      IF ((XDOCAT.LE.0.0) .OR. (SEQOUT.LE.0)) THEN
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR IN LIST OF SOURCES'
            GO TO 990
            END IF
      ELSE
         NSOUWD = 0
         DOSWNT = .TRUE.
         END IF
      CALL TBTIME (START, STOP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       restore input names
      DO 5 I = 1,30
         SOURCS(I) = XSOUR(I)
 5       CONTINUE
C                                       get BIGBOY IO memory
      NWORDS = (UVBFSS - 1)/1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1200) IRET, 'GETTING INITIAL I/O MEMORY'
         GO TO 990
         END IF
      P1 = 1 + PBIGB
C                                       build image size
      IMSIZE(1) = 1
      LTEMP = DPARM(1)
      DPARM(1) = 6.0
      DPARM(2) = 0.0
      DPARM(5) = 1.0
      IF (DPARM(6).LE.0.0) DPARM(6) = 10.
      DELTAT = DPARM(6) / (24. * 3600.)
      CALL H2CHR (8, 1, CATH(KHTEL), TELESC)
      TSIGMA = DELTAT / 11.0D0
      II = START / TSIGMA
      START = II * TSIGMA
      II = STOP / TSIGMA + 1.0D0
      STOP = II * TSIGMA
C                                       image name
      NPOINT = 0.0D0
      NFAIL = 0
      OUTCLS = 'SPFLGR'
      IF ((XDOCAT.LE.0.0) .OR. (SEQOUT.LE.0)) THEN
         I = MAXANT * MAXANT
         IF (DESEL) THEN
             IFILL = 0
             ISUB = -1000000000
         ELSE
             ISUB = 0
             IFILL = -1000000000
            END IF
         IF (NXANT.LE.0) THEN
            CALL FILL (I, 0, NODATA)
         ELSE
            CALL FILL (I, IFILL, NODATA)
            DO 20 IANT = 1,NXANT
               I = IXANT(IANT)
               IF (I.GT.0) THEN
C                                       with all antennas
                  IF (NXBASL.LE.0) THEN
                     DO 10 J = 1,MAXANT
                        NODATA(J,I) = ISUB
                        NODATA(I,J) = ISUB
 10                     CONTINUE
                  ELSE
C                                       with list of antennas
                     DO 15 JANT = 1,NXBASL
                        J = IXBASL(JANT)
                        IF (J.GT.0) THEN
                           NODATA(J,I) = ISUB
                           NODATA(I,J) = ISUB
                           END IF
 15                     CONTINUE
                     END IF
                  END IF
 20            CONTINUE
            END IF
         MSGTXT = 'Begin finding a list of times & baselines to enter'
     *      // ' the grid'
         CALL MSGWRT (2)
C                                       autocorr not okay
         IF (.NOT.DOACOR) THEN
            MSGTXT = 'Total-power spectra will not be displayed'
            CALL MSGWRT (2)
            DO 25 IANT = 1,MAXANT
               NODATA(IANT,IANT) = -1000000000
 25            CONTINUE
            END IF
C                                       crosscorr not okay
         IF (.NOT.DOXCOR) THEN
            MSGTXT = 'Cross-power spectra will not be displayed'
            CALL MSGWRT (2)
            DO 27 IANT = 1,MAXANT-1
               DO 26 JANT = IANT+1,MAXANT
                  NODATA(IANT,JANT) = -1000000000
                  NODATA(JANT,IANT) = -1000000000
 26               CONTINUE
 27            CONTINUE
            END IF
C                                       open UV file
C                                       open with flags, sources
         CALL UVGET ('INIT', RPARM, BIGBOY(P1), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1025) 'INIT', IRET
            GO TO 990
            END IF
         NCOUNT = 0
         CALL FILL (11, 0, NBIN)
C                                       loop thru 1st 1000 samples
C                                       read buffer
 30      CALL UVGET ('READ', RPARM, BIGBOY(P1), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1025) 'READ', IRET
            GO TO 990
C                                       loop thru buffer
         ELSE IF (IRET.EQ.0) THEN
            TIME = RPARM(1+ILOCT)
            IF (TIME.GT.STOP) GO TO 45
            IF (TIME.GE.START) THEN
               II = TIME / TSIGMA + 0.50D0
               JJ = MOD (II, 11) + 1
               NCOUNT = NCOUNT + 1
               NBIN(JJ) = NBIN(JJ) + 1
               END IF
            IF (NCOUNT.LT.10000) GO TO 30
         ELSE
            IRET = 0
            END IF
 45      CALL UVGET ('CLOS', RPARM, BIGBOY(P1), IRET)
C                                       check times
         II = NBIN(1)
         JJ = 1
         DO 50 J = 2,11
            IF (NBIN(J).GT.II) THEN
               II = NBIN(J)
               JJ = J
               END IF
 50         CONTINUE
         IF (II.LT.NCOUNT/5) THEN
            MSGTXT = 'Warning: times badly distributed wrt averaging'
     *         // ' time'
            CALL MSGWRT (6)
            END IF
         JBOTM = JJ - 6
         IF (JBOTM.LT.0) JBOTM = JBOTM + 11
C                                       Re-init for actual time
C                                       measurements
         CALL UVGET ('INIT', RPARM, BIGBOY(P1), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1025) 'INIT', IRET
            GO TO 990
            END IF
         LY = 0
         LSOU = -2
         LINE = 0
         NV = 0
C                                       loop thru data:
C                                       read buffer
 130     CALL UVGET ('READ', RPARM, BIGBOY(P1), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1025) 'READ', IRET
            GO TO 990
C                                       process data
         ELSE IF (IRET.EQ.0) THEN
            TIME = RPARM(1+ILOCT)
            IF (TIME.LT.START) GO TO 130
            NV = NV + 1
            IF (TIME.GT.STOP) GO TO 195
            II = TIME / TSIGMA + 0.5D0
            TIME = II * TSIGMA
            IF (FIRST) THEN
               JJ = MOD (II, 11)
               IF (JJ.LT.JBOTM) THEN
                  II = II + JBOTM - JJ - 11
               ELSE
                  II = II + JBOTM - JJ
                  END IF
               CTIME = II * TSIGMA - DELTAT
               LTIME = CTIME
               FIRST = .FALSE.
               END IF
            IY = (TIME - CTIME) / DELTAT + LY + 0.001
            ISOU = 0
            IF (ILOCSU.GE.0) ISOU = IROUND (RPARM(1+ILOCSU))
            IF (ISOU.LE.0) ISOU = INSNUM
            IF (ILOCB.GE.0) THEN
               TEMP = RPARM(1+ILOCB)
               I = TEMP + 0.1
               IAR = 100.0 * (TEMP - I) + 1.5
               IANT = I / 256
               JANT = MOD (I, 256)
            ELSE
               IANT = RPARM(1+ILOCA1) + 0.1
               JANT = RPARM(1+ILOCA2) + 0.1
               IAR = RPARM(1+ILOCSA) + 0.1
               END IF
            IF (IANT.GT.JANT) THEN
               NODATA(JANT,IANT) = NODATA(JANT,IANT) + 1
            ELSE
               NODATA(IANT,JANT) = NODATA(IANT,JANT) + 1
               END IF
C                                       time backwards !
            IF (IY.LT.LY) THEN
               IRET = 8
               WRITE (MSGTXT,1130) TIME, LTIME
               CALL MSGWRT (8)
               WRITE (MSGTXT,1131) NV, IY, LY
               GO TO 990
C                                       time advances
            ELSE IF (IY.GT.LY) THEN
               NSK = MIN (5, IY-LY)
               PLINE = LINE + 1
               PTIME = CTIME
               CTIME = CTIME + (IY-LY) * DELTAT
               LINE = LINE + NSK
               IF (LINE.GT.32767) THEN
                  IRET = 8
                  WRITE (MSGTXT,1135) LINE
                  GO TO 990
                  END IF
               MTIMES(LINE) = CTIME
               MSOU(LINE) = ISOU
               LSOU = ISOU
               IF (NSK.GT.1) THEN
                  MTIMES(PLINE) = PTIME + DELTAT
                  MSOU(PLINE) = -1
                  NSK = NSK - 2
                  IF (NSK.GT.0) THEN
                     DO 150 I = 1,NSK
                        MTIMES(PLINE+I) = MTIMES(PLINE) + I *
     *                     (MTIMES(LINE)-MTIMES(PLINE)) / (NSK+1.)
                        MSOU(PLINE+I) = -1
 150                    CONTINUE
                     END IF
                  END IF
               LY = IY
C                                       same time, different source!
            ELSE IF (ISOU.NE.LSOU) THEN
               LINE = LINE + 1
               IF (LINE.GT.32676) THEN
                  IRET = 8
                  WRITE (MSGTXT,1135) LINE
                  GO TO 990
                  END IF
               II = TIME / TSIGMA + 0.5D0
               TIME = II * TSIGMA
               JJ = MOD (II, 11)
               IF (JJ.LT.JBOTM) THEN
                  II = II + JBOTM - JJ - 11
               ELSE
                  II = II + JBOTM - JJ
                  END IF
               CTIME = II * TSIGMA
               MTIMES(LINE) = CTIME
               IF (MTIMES(LINE).LE.MTIMES(LINE-1)) MTIMES(LINE) =
     *            MTIMES(LINE-1) + TSIGMA/100.0
               MSOU(LINE) = ISOU
               LSOU = ISOU
               END IF
C                                       update pointers
            LTIME = TIME
            GO TO 130
         ELSE
            IRET = 0
            END IF
 195     CALL UVGET ('CLOS', RPARM, BIGBOY(P1), IRET)
         WRITE (MSGTXT,1195) LINE
         IF (LINE.LE.0) THEN
            CALL MSGWRT (8)
            MSGTXT = 'CHECK DATA SELECTION ADVERBS'
            IRET = 4
            GO TO 990
            END IF
         CALL MSGWRT (3)
         PLINE = LINE
         DO 200 I = 1,10
            LINE = LINE + 1
            MTIMES(LINE) = MTIMES(PLINE) + (LINE-PLINE) * DELTAT
            MSOU(LINE) = -1
 200        CONTINUE
C                                       grid it now
         IMSIZE(2) = LINE - 5
         IF (XDOCAT.LE.0.0) THEN
            SCNAME = 'temporary'
         ELSE
            SCNAME = 'cataloged'
            END IF
         MSGTXT = 'begin gridding the data to a ' // SCNAME // ' ' //
     *      OUTCLS // ' file'
         CALL MSGWRT (2)
C                                       free small memory
         CALL ZMEMRY ('FREE', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET, 'FREEING INITIAL I/O MEMORY'
            GO TO 990
            END IF
C                                       can we do full plane grids
         IMSIZE(1) = (ECHAN - BCHAN + 1) * (EIF - BIF + 1) * 3 + 3
         NWORDS = (IMSIZE(1)*IMSIZE(2) - 1) / 1024 + 1
         I = KAPWRD / NWORDS
C                                       get BIGBOY memory
         IF (I.LE.8) THEN
            NWORDS = (BIGDIM - 1)/1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'GETTING NORMAL BIG MEMORY'
               GO TO 990
               END IF
C                                       pointers
            I = 1 + DIMB1 * MAXBUF
            J = I + DIMB1 * MAXBUF
            II = J + 3*MAXCIF + 5
            P1 = 1 + PBIGB
            PI = I + PBIGB
            PJ = J + PBIGB
            PII = II + PBIGB
            CALL GRIDTC (DPARM, IMSIZE, MAXANT, NODATA, MSOU, MTIMES,
     *         MAXBUF, DIMB1, IOBUF, 1.0, OUTNAM, OUTCLS, SEQOUT,
     *         DISKOU, CNOOUT, MXBASE, NOANTS, MBL, NPOINT, NFAIL,
     *         INSNUM, KIGBOY(P1), BIGBOY(PI), BIGBOY(PII), BIGBOY(PJ),
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       new method
        ELSE
C                                       get multiple planes
            NWORDS = I * NWORDS
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'GETTING FULL PLANES MEMORY'
               GO TO 990
               END IF
C                                       do it
            P1 = 1 + PBIGB
            CALL GRIDSP (DPARM, IMSIZE, MAXANT, NODATA, MSOU, MTIMES,
     *         I, IMSIZE(1), IMSIZE(2), 1.0, OUTNAM, OUTCLS, SEQOUT,
     *         DISKOU, CNOOUT, MXBASE, NOANTS, MBL, NPOINT, NFAIL,
     *         INSNUM, BIGBOY(P1), IRET)
            IF (IRET.NE.0) GO TO 999
C                                       free full planes
            CALL ZMEMRY ('FREE', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'FREEING FULL PLANES MEMORY'
               GO TO 990
               END IF
C                                       get usual BIGBOY
            NWORDS = (BIGDIM - 1)/1024 + 1
            CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'GETTING NORMAL BIG MEMORY'
               GO TO 990
               END IF
           END IF
      ELSE
         CALL SPFBLT (DISKOU, CNOOUT, MBL, NOANTS, CATIMG, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       get usual BIGBOY
         NWORDS = (BIGDIM - 1)/1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, BIGBOY, PBIGB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET, 'GETTING NORMAL BIG MEMORY'
            GO TO 990
            END IF
         END IF
      GO TO 995
C
 990  CALL MSGWRT (8)
      GO TO 999
 995  DPARM(1) = LTEMP
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFLSC: TBTIME RETURNS ERROR',I5)
 1025 FORMAT ('CAN''T ',A,' THE INPUT UV FILE, ERROR',I6)
 1130 FORMAT ('TIMES OUT OF ORDER: ',1PE13.6,' < ',1PE13.6)
 1131 FORMAT ('AT VIS #',I12,' APPARENT ROW',I6,' <',I6)
 1135 FORMAT ('TOO MANY TIMES: APPARENT LINE NUMBER ',I6)
 1195 FORMAT ('Found',I6,' time intervals to grid, so')
 1200 FORMAT ('ZMEMRY ERROR',I3,' ON ',A)
      END
      SUBROUTINE SPFLGR (ONEIF, BIGDIM, BIGBOY, KIGBOY, IRET)
C-----------------------------------------------------------------------
C   SPFLGR is the main action routine of SPFLG.  It takes the TV and
C   displays the gridded SC file (smoothed to fit), offers options to
C   enhance the display, selected and redisplay a subimage, and edit
C   (flag) the data.
C   Input:
C      ONEIF    L      T => there is only one IF
C      BIGDIM   I      Total size of BIGBOY
C   Output:
C      BIGBOY   R(*)   Large IO buffers
C      KIGBOY   I(*)   Large IO buffers
C      IRET     I   Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      LOGICAL   ONEIF
      INTEGER   BIGDIM, KIGBOY(*), IRET
      REAL      BIGBOY(BIGDIM)
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER CHTYPE(9)*8, CHSDEF(8)*2, CTEMP*4, ROUTIN*6, MSGBUF*72,
     *   CHST(13)*2, PHNAME*48, REAZON*24
      INTEGER   TTY(2), JERR, SCRTCH(MAXIMG), ICOL, IROW, MTRY, JTRIM,
     *   LTEMP(2), NTRY, IMGWIN(4), IGR, IPL, ITEMP, NX, IX, I, ALUN,
     *   NY, ANTERR, DATE(3), TIME(3), SLUN, ISIZE, IROUND, PLIMG(2),
     *   IY, PWIND(4), IT2, LUN0, FIND0, IB1, KBANT, KEANT, SVZOOM(3),
     *   IB2
      REAL      DTIME, PRPOS(2,10), BLCORN(7), TRCORN(7), TEMP,
     *   RSCR(MAXIMG)
      LOGICAL   T, F, EQUAL, NOFLAG, OKCORN(4), DOSTOK, FIRSTV
      DOUBLE PRECISION DTEMP(2)
      EQUIVALENCE (RSCR, SCRTCH)
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'SPFLG.BUF'
      DATA ALUN, SLUN, LUN0 /28, 29, 16/
      DATA T, F /.TRUE.,.FALSE./
      DATA DTIME /3.0/
      DATA CHTYPE /'AMPLTUDE', 'PHASE   ', 'RMS AMPL', 'RMS/MEAN',
     *   'RMS VAMP', 'VRMS/AVG', 'VEC DIFF', 'AMP DIFF', 'PHS DIFF'/
      DATA CHST /'YX','XY','YY','XX', 'LR','RL','LL','RR', '??',
     *   'I','Q','U ','V '/
      DATA CHSDEF /'RR','LL','RL','LR','XX','YY','XY','YX'/
C-----------------------------------------------------------------------
      XYCENT(1) = 0
      XYCENT(2) = 0
      FIRSTV = .TRUE.
      TTY(1) = 5
      MTRY = 20
      NTRY = MTRY + 1
      CALL FILL (16, 0, LWINTV)
      CALL RFILL (20, 0.0, PRPOS)
      LTYPE = DPARM(1) + 1.0001
      IF ((LTYPE.LT.1) .OR. (LTYPE.GT.4)) LTYPE = 1
      PIXRNG(1,LTYPE) = DPARM(9)
      PIXRNG(2,LTYPE) = DPARM(10)
      LSTOKS = 1
      PLSTOK = 0
      PLBASL = 0
      LBASL = 1
      IF ((DPARM(8).GE.1) .AND. (DPARM(8).LE.CATBLK(KINAX+3)))
     *   LBASL = DPARM(8)
      LSMOO = 1
      DOBASL = 0
      IF (ONEIF) THEN
         DOIFS = 1
      ELSE
         DOIFS = -1
         END IF
      LCIF(1) = 0
      LCIF(2) = 0
      DOWEDG = .FALSE.
      DOSOUR = .FALSE.
      ITRTYP = 1
      TEMP = CATIR(KRCIC+1)
      IF (TEMP.LT.9.9) THEN
         TFORM = 1
      ELSE
         TFORM = 0
         END IF
C                                       Open terminal
      TTY(2) = 0
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, F, T, T, IRET)
      IF (IRET.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1000) IRET
         GO TO 980
         END IF
C                                       open TV
 10   CALL TVOPEN (BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         IF (IRET.NE.4) GO TO 970
         IF (NTRY.GE.MTRY) THEN
            MSGBUF = 'TV is busy: do we wait? Y or N'
            CALL INQSTR (TTY, MSGBUF, 4, CTEMP, IRET)
            IF ((IRET.NE.0) .AND. (IRET.NE.10)) GO TO 950
            CALL CHLTOU (4, CTEMP)
            IF ('Y'.NE.CTEMP(1:1)) GO TO 970
            NTRY = 0
            END IF
         NTRY = NTRY + 1
         CALL ZDELAY (DTIME, JERR)
         GO TO 10
         END IF
C                                       create TV scratch file
      CALL COPY (256, CATBLK, CATIMG)
      CATBLK(KINAX) = (CATBLK(KINAX) - 3) / 3
      WRITE (MSGTXT,1020) CATBLK(KINAX), CATBLK(KINAX+1)
      CALL MSGWRT (2)
      CATBLK(KINAX) = CATBLK(KINAX) + 3
      CATBLK(KINAX+2) = 1
      CATBLK(KINAX+3) = 1
      CATBLK(KINAX+4) = 1
      CATBLK(KINAX+5) = 1
      CATBLK(KINAX+6) = 1
      CALL MAPSIZ (CATBLK(KIDIM), CATBLK(KINAX), ISIZE)
      CALL SCREAT (ISIZE, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1021) IRET
         CALL MSGWRT (8)
         IRET = 3
         GO TO 990
         END IF
      TVFILE = NSCR
C                                       floating buffer SC file
      CATBLK(KINAX) = (CATBLK(KINAX) - 2) * 3
      CALL MAPSIZ (CATBLK(KIDIM), CATBLK(KINAX), ISIZE)
      CALL SCREAT (ISIZE, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1022) IRET
         CALL MSGWRT (8)
         IRET = 3
         GO TO 990
         END IF
      SCFILE = NSCR
      CATBLK(KINAX) = CATBLK(KINAX) / 3 + 2
C                                       init the TV
      CALL YINIT (SCRTCH, IRET)
      ROUTIN = 'YINIT'
      IF (IRET.NE.0) GO TO 940
      CALL COPY (3, TVZOOM, SVZOOM)
C                                       turn on graphics
      ROUTIN = 'YSLECT'
      IF (NGRAPH.GT.1) THEN
         IGR = 1 + NGRAY
         CALL YSLECT ('ONNN', IGR, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 940
         END IF
      IF (NGRAPH.GT.2) THEN
         IGR = 3 + NGRAY
         CALL YSLECT ('ONNN', IGR, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 940
         END IF
C                                       get antenna info
      NUMAN(1) = 0
      IMGWIN(1) = 1
      IMGWIN(3) = (CATIMG(KINAX) - 3) / 3
      CALL GETBLO (ALUN, NUMAN, ANTERR)
C                                       Stokes in data
      CALL FILL (4, 0, ILSTOK)
      NSTOKS = CATIMG(KINAX+2)
      DO 25 I = 1,NSTOKS
         TEMP = (I - CATIR(KRCRP+2)) * CATIR(KRCIC+2) + CATID(KDCRV+2)
         ILSTOK(I) = IROUND (TEMP)
 25      CONTINUE
C                                       Stokes translated?
      IF ((ICOR0.GT.0) .AND. (STRANS.LT.0)) THEN
         STRANS = 1
      ELSE IF ((ICOR0.LT.0) .AND. (STRANS.GT.0)) THEN
         STRANS = -1
      ELSE
         STRANS = 0
         END IF
C                                       Default STKFLG
      IF (STRANS.NE.0) THEN
         USTFLG = 'FULL'
      ELSE IF (ICOR0.GT.0) THEN
         USTFLG = 'IQUV'
      ELSE
         USTFLG = 'FULL'
         IB1 = -ILSTOK(1)
         IF (CATIMG(KINAX+2).EQ.1) THEN
            USTFLG = CHSDEF(IB1)
         ELSE IF ((CATIMG(KINAX+2).EQ.2) .AND. (CATUV(KINAX+1).LT.4))
     *      THEN
            USTFLG = CHSDEF(IB1)
         ELSE
            IF (IB1.EQ.1) USTFLG = 'NOLL'
            IF (IB1.EQ.2) USTFLG = 'NORR'
            IF (IB1.EQ.5) USTFLG = 'NOYY'
            IF (IB1.EQ.6) USTFLG = 'NOXX'
            END IF
         END IF
C                                       get 1s and 0s flag
      CALL MKSTOK (STRANS, ILSTOK, USTFLG, STKFLG, IRET)
      IRET = 0
C                                       get source names
      MAXSOU = 0
      LSNAME = 0
      IF (LLOCSU.GE.0) THEN
         DO 50 I = 1,XSTBSZ
            MSGSUP = 32000
            CALL GETSOU (I, IUDISK, IUCNO, CATUV, SLUN, JERR)
            MSGSUP = 0
            MAXSOU = MAXSOU + 1
            IF (JERR.EQ.0) THEN
               SNAMES(MAXSOU) = SNAME
            ELSE
               SNAMES(MAXSOU) = 'NOT IN SU TABLE'
               END IF
            LSNAME = MAX (LSNAME, JTRIM (SNAMES(MAXSOU)))
 50         CONTINUE
      ELSE
         CALL H2CHR (8, 1, CATIH(KHOBJ), SNAMES)
         LSNAME = JTRIM (SNAMES(1))
         END IF
C                                       set default flagging reason
      REAZON = 'SPFLG:date time'
      CALL CHR2H (24, REAZON, 1, FCREAS)
C                                       read in the master grid times
C                                       open master grid file
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
      CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1060) IRET
         GO TO 899
         END IF
      NY = CATIMG(KINAX+1)
      CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX), NY, 0, BIGBOY,
     *   JBUFSZ, 1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1065) 'INIT', IRET
         GO TO 899
         END IF
      DO 70 IY = 1,NY
         CALL MDISK ('READ', LUN0, FIND0, BIGBOY, IB1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1065) 'READ', IRET
            GO TO 899
            END IF
         TEMP = BIGBOY(IB1)
         IF (TEMP.NE.FBLANK) THEN
            MSOU(IY) = IROUND (TEMP)
            IF (MSOU(IY).LE.0) MSOU(IY) = INSNUM
         ELSE
            MSOU(IY) = -1
            END IF
         TEMP = BIGBOY(IB1+1)
         IF (TEMP.NE.FBLANK) THEN
            MTIMES(IY) = TEMP
         ELSE IF (IY.EQ.1) THEN
            MTIMES(IY) = START - CATIR(KRCIC+1) / (2. * 24. * 3600.)
         ELSE
            MTIMES(IY) = MTIMES(IY-1) + CATIR(KRCIC+1) / (24. * 3600.)
            END IF
 70      CONTINUE
      MTIMES(NY+1) = MTIMES(NY) + CATIR(KRCIC+1) / (24. * 3600.)
      MTIMES(NY+2) = MTIMES(NY+1) + CATIR(KRCIC+1) / (24. * 3600.)
      MTIMES(NY+3) = MTIMES(NY+2) + CATIR(KRCIC+1) / (24. * 3600.)
      MSOU(NY+1) = -1
      MSOU(NY+2) = -1
      MSOU(NY+3) = -1
      START = MTIMES(1)
      STOP = MTIMES(NY+1)
      CALL ZCLOSE (LUN0, FIND0, IRET)
C                                       create/find the FC table
      FCVERS = 0
      CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NNFLAG.GT.0) THEN
         WRITE (MSGTXT,1070) FCNUMB, NNFLAG
         CALL MSGWRT (2)
         END IF
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       branch to load function to start
      IROW = 11
      LSMOO = (CATIMG(KINAX+1) - 1) / (0.75 * MAXXTV(2)) + 1
      IF (LSMOO.LT.1) LSMOO = 1
      IPIECE = 1
      NPIECE = 1
      MPIECE = CATIMG(KINAX+1)
      LSCAN = MIN (30, 3 * LSMOO)
      LSCAN = MAX (LSCAN, LSMOO+2)
      MENUOK = .FALSE.
      GPH1OK = .TRUE.
      GPH3OK = .TRUE.
      GO TO 300
C                                       Set new choice
 90   CALL SPFCHS (PRPOS(1,1), IMGWIN, PWIND, SVZOOM, SCRTCH, RSCR,
     *   ICOL, IROW, IRET)
      ROUTIN = 'SPFCHS'
      IF (IRET.NE.0) GO TO 940
      GO TO (100, 200, 300, 400, 900), ICOL
C                                       enhancement functions
 100     IF ((IROW.GE.1) .AND. (IROW.LE.6)) THEN
            CALL SPFUNC (IROW, PRPOS(1,2), SCRTCH, IRET)
            ROUTIN = 'SPFUNC'
            IF ((IROW.EQ.1) .OR. (IROW.EQ.4)) CALL COPY (3, TVZOOM,
     *         SVZOOM)
C                                       wedge
         ELSE IF (IROW.EQ.7) THEN
            DOWEDG = .NOT.DOWEDG
            IPL = 1
            CALL SPFWED (DOWEDG, PLTYPE, IPL, SCRTCH, BIGBOY, IRET)
            ROUTIN = 'SPFWED'
            IF (IRET.GT.0) GO TO 940
            GPH4OK = .FALSE.
            CALL SPFLAB (IPL, PWIND, SCRTCH, BIGBOY, IRET)
            ROUTIN = 'SPFLAB'
C                                       log/linear load
         ELSE IF (IROW.EQ.8) THEN
            ITRTYP = ITRTYP + 1
            IF (ITRTYP.GT.4) ITRTYP = 1
            MENUOK = .FALSE.
C                                       unflagging functions
         ELSE IF ((IROW.GE.9) .AND. (IROW.LE.11)) THEN
            CALL SPFUNF (IROW, TTY, PWIND, BIGBOY(BIGPT(6)), BIGBOY,
     *         KIGBOY, SCRTCH, IRET)
            ROUTIN = 'SPFUNF'
C                                       list baselines
         ELSE IF (IROW.EQ.12) THEN
            IF (CATIMG(KINAX+3).GT.2) THEN
               WRITE (MSGBUF,1130) CATIMG(KINAX+3)
               CALL INQINT (TTY, MSGBUF, 2, LTEMP, IRET)
               IF (IRET.LT.0) GO TO 190
               IF (IRET.GT.0) GO TO 950
               KBANT = MAX (1, MIN (LTEMP(1), CATIMG(KINAX+3)))
               KEANT = MIN (LTEMP(2), CATIMG(KINAX+3))
               IF (KEANT.LT.KBANT) KEANT = CATIMG(KINAX+3)
            ELSE
               KBANT = 1
               KEANT = CATIMG(KINAX+3)
               END IF
            MSGTXT = 'Baseline  Antenna1 - Antenna2 / Subarray'
            CALL MSGWRT (2)
            DO 135 I = KBANT,KEANT
               WRITE (MSGTXT,1135) I, NOANTS(1,I), NOANTS(2,I), SUBARR
               CALL MSGWRT (2)
 135           CONTINUE
C                                       Select user string
         ELSE IF (IROW.EQ.13) THEN
            CALL GTREAS (TTY, REAZON)
            IF (REAZON.NE.' ') CALL CHR2H (24, REAZON, I, FCREAS)
C                                       Do labeling
         ELSE IF (IROW.EQ.14) THEN
            DOLABL = .NOT.DOLABL
            IPL = 1
            CALL SPFLAB (IPL, PWIND, SCRTCH, BIGBOY, IRET)
            ROUTIN = 'SPFLAB'
            END IF
C
         IF (IRET.GT.0) GO TO 940
         IF ((IRET.EQ.0) .AND. ((IROW.EQ.10) .OR. (IROW.EQ.11) .OR.
     *      (IROW.EQ.8))) GO TO 335
         GO TO 90
C                                       Non-numeric inputs
 190     MSGTXT = 'NON-NUMERIC INPUT - return to menu'
         CALL MSGWRT (6)
         GO TO 90
C                                       window set / load
C                                       manually entered windows
 200     IF (IROW.LE.2) THEN
            NX = (CATIMG(KINAX) - 3) / 3
            IF (IROW.EQ.1) WRITE (MSGBUF,1200) 1, NX, CATIMG(KINAX+1)
            IF (IROW.EQ.2) WRITE (MSGBUF,1201) 1, NX, CATIMG(KINAX+1)
            CALL INQINT (TTY, MSGBUF, 2, LTEMP, IRET)
            IF (IRET.LT.0) GO TO 190
            IF (IRET.GT.0) GO TO 950
            IF (IROW.EQ.1) THEN
               IF (LTEMP(1).LT.1) LTEMP(1) = 1
               IF (LTEMP(1).GT.NX) GO TO 200
               IF (LTEMP(2).LT.1) LTEMP(2) = 1
               IF (LTEMP(2).GT.CATIMG(KINAX+1)) GO TO 200
               IMGWIN(1) = LTEMP(1)
               IMGWIN(2) = LTEMP(2)
            ELSE
               IF (LTEMP(1).GT.NX) LTEMP(1) = NX
               IF (LTEMP(1).LE.IMGWIN(1)) GO TO 200
               IF (LTEMP(2).GT.CATIMG(KINAX+1)) LTEMP(2) =
     *            CATIMG(KINAX+1)
               IF (LTEMP(2).LE.IMGWIN(2)) GO TO 200
               IMGWIN(3) = LTEMP(1)
               IMGWIN(4) = LTEMP(2)
               END IF
C                                       enter pixranges
         ELSE IF (IROW.LE.6) THEN
            IX = IROW - 2
            WRITE (MSGBUF,1220) CHTYPE(IX)
            CALL INQFLT (TTY, MSGBUF, 2, DTEMP, IRET)
            IF (IRET.LT.0) GO TO 190
            IF (IRET.GT.0) GO TO 950
            PIXRNG(1,IX) = DTEMP(1)
            PIXRNG(2,IX) = DTEMP(2)
C                                       time averaging select
         ELSE IF (IROW.EQ.7) THEN
 230        TEMP = CATIR(KRCIC+1)
            WRITE (MSGBUF,1230) TEMP
            CALL INQINT (TTY, MSGBUF, 1, ITEMP, IRET)
            IF (IRET.GT.0) GO TO 950
            IF (IRET.LT.0) GO TO 190
            IF ((ITEMP.LT.1) .OR. (ITEMP.GT.CATIMG(KINAX+1)))
     *         GO TO 230
            IF (LSMOO.NE.ITEMP) THEN
               LSMOO = ITEMP
               MPIECE = 0.75 * MAXXTV(2)
               ITEMP = (CATIMG(KINAX+1)-1) / LSMOO + 1
               IF (ITEMP.LT.MPIECE) THEN
                  NPIECE = 1
                  MPIECE = ITEMP
               ELSE
                  NPIECE = (1.125 * ITEMP) / MPIECE + 1
                  END IF
               MPIECE = MPIECE * LSMOO
               IPIECE = 1
               IMGWIN(2) = 1
               IMGWIN(4) = MIN (MPIECE, CATIMG(KINAX+1))
               MENUOK = .FALSE.
               END IF
C                                       Scan averaging select
         ELSE IF (IROW.EQ.8) THEN
 235        TEMP = CATIR(KRCIC+1)
            WRITE (MSGBUF,1235) TEMP
            CALL INQINT (TTY, MSGBUF, 1, ITEMP, IRET)
            IF (IRET.GT.0) GO TO 950
            IF (IRET.LT.0) GO TO 190
            IF ((ITEMP.LT.1) .OR. (ITEMP.GT.CATIMG(KINAX+1)))
     *         GO TO 235
            LSCAN = ITEMP
C                                       baseline select
         ELSE IF (IROW.EQ.9) THEN
 240        IF (CATIMG(KINAX+3).GT.2) THEN
               WRITE (MSGBUF,1240) CATIMG(KINAX+3)
               CALL INQINT (TTY, MSGBUF, 1, ITEMP, IRET)
               IF (IRET.GT.0) GO TO 950
               IF (IRET.LT.0) GO TO 190
               IF ((ITEMP.LT.1) .OR. (ITEMP.GT.CATIMG(KINAX+3)))
     *            GO TO 240
               LBASL = ITEMP
            ELSE IF (CATIMG(KINAX+3).EQ.2) THEN
               LBASL = 3 - LBASL
               END IF
C                                       set Stokes mask
         ELSE IF (IROW.EQ.10) THEN
 250        IF (ILSTOK(1).LE.-5) THEN
               WRITE (MSGTXT,1250) 'XX/YY/XY/YX'
            ELSE IF (ILSTOK(1).LE.-1) THEN
               WRITE (MSGTXT,1250) 'RR/LL/RL/LR'
            ELSE
               WRITE (MSGTXT,1250) 'I/Q/U/V'
               END IF
            CALL MSGWRT (1)
            MSGBUF = 'Enter Stokes flag string or mask: 4 chars '
     *         // 'must begin in col 1'
 251        CALL INQSTR (TTY, MSGBUF, 4, USTFLG, IRET)
            IF (IRET.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG, TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 251
               END IF
            IF (IRET.NE.0) GO TO 950
            CALL CHLTOU (4, USTFLG)
C                                       get 1s and 0s flag
            CALL MKSTOK (STRANS, ILSTOK, USTFLG, STKFLG, IRET)
            IF (IRET.NE.0) THEN
                MSGTXT = 'STOKES FLAG ''' // USTFLG //
     *             ''' NOT RECOGNIZED OR INAPROPRIATE'
                CALL MSGWRT (6)
                IRET = 0
                GO TO 250
                END IF
            IF (.NOT.DOSTOK (ILSTOK, STKFLG, PLSTOK)) THEN
               MSGTXT = '****  NEW STOKES FLAG DOES NOT INCLUDE ' //
     *            'CURRENT STOKES  ****'
               CALL MSGWRT (7)
               IF (DOSTOK (ILSTOK, STKFLG, LSTOKS)) THEN
                  MSGTXT = '****  new Stokes flag does include new ' //
     *               'Stokes - do a LOAD  ****'
                  CALL MSGWRT (3)
                  END IF
               END IF
C                                       channel smoothing
         ELSE IF (IROW.EQ.11) THEN
            MSGBUF = 'Enter channel smooth FWHM in channels'
            CALL INQFLT (TTY, MSGBUF, 1, DTEMP, IRET)
            IF (IRET.LT.0) GO TO 190
            IF (IRET.GT.0) GO TO 950
            CSMOTH = DTEMP(1)
            CSMOTH = MAX (0.0, CSMOTH)
C                                       all planes flags
         ELSE IF (IROW.EQ.12) THEN
            IF (MAXSOU.GT.0) DOSOUR = .NOT.DOSOUR
         ELSE IF (IROW.EQ.13) THEN
            DOBASL = MOD (DOBASL+1, 4)
         ELSE IF (IROW.EQ.14) THEN
            IF (ONEIF) THEN
               DOIFS = 1
            ELSE
               DOIFS = DOIFS + 1
               IF (DOIFS.GT.1) DOIFS = -1
               IF ((DOIFS.EQ.0) .AND. (EIF-BIF.LE.1)) DOIFS = 1
C                                       which IFs?
               IF (DOIFS.EQ.0) THEN
 260              WRITE (MSGBUF,1260) BIF, EIF
                  CALL INQINT (TTY, MSGBUF, 2, LTEMP, IRET)
                  IF (IRET.LT.0) GO TO 190
                  IF (IRET.GT.0) GO TO 950
                  IF ((LTEMP(1).LT.BIF) .OR. (LTEMP(1).GT.EIF) .OR.
     *               (LTEMP(2).LT.LTEMP(1)) .OR. (LTEMP(2).GT.EIF))
     *               GO TO 260
                  LCIF(1) = LTEMP(1)
                  LCIF(2) = LTEMP(2)
               ELSE
                  LCIF(1) = 0
                  LCIF(2) = 0
                  END IF
               END IF
            END IF
         GO TO 90
C                                       data selection
C                                       data conversion
 300     IF ((IROW.EQ.15) .AND. (NPIECE.EQ.1)) IROW = 17
         IF (IROW.LE.9) THEN
            IF ((TYPUVD.LE.0) .OR. (IROW.NE.2)) LTYPE = IROW
C                                       Stokes select (switch)
         ELSE IF (IROW.EQ.10) THEN
            NSTOKS = CATIMG(KINAX+2)
            IF (NSTOKS.GT.1) THEN
               LSTOKS = MOD (LSTOKS, NSTOKS) + 1
C                                       try to fix
               IF (.NOT.DOSTOK (ILSTOK, STKFLG, LSTOKS)) THEN
                  IB2 = ILSTOK(LSTOKS)
                  IF (IB2.EQ.-1) THEN
                     STKFLG = '1011'
                     USTFLG = 'NOLL'
                  ELSE IF (IB2.EQ.-2) THEN
                     STKFLG = '0111'
                     USTFLG = 'NORR'
                  ELSE IF (IB2.EQ.-5) THEN
                     STKFLG = '1010'
                     USTFLG = 'NOYY'
                  ELSE IF (IB2.EQ.-6) THEN
                     STKFLG = '0111'
                     USTFLG = 'NOXX'
                  ELSE
                     STKFLG = '1111'
                     USTFLG = 'FULL'
                     END IF
                  MSGTXT = 'CHANGING STOKES FLAG TO INCLUDE NEW' //
     *               ' STOKES'
                  CALL MSGWRT (2)
                  END IF
C                                       check anyway
               IF (.NOT.DOSTOK (ILSTOK, STKFLG, LSTOKS)) THEN
                  MSGTXT = '*****  CURRENT STOKES FLAG DOES NOT'  //
     *               ' INCLUDE NEW STOKES  ****'
                  CALL MSGWRT (7)
                  END IF
               END IF
C                                       clear window
         ELSE IF (IROW.EQ.11) THEN
            IMGWIN(1) = 1
            IMGWIN(2) = 1
            IMGWIN(3) = (CATIMG(KINAX) - 3) / 3
            IMGWIN(4) = CATIMG(KINAX+1)
C                                       set window
         ELSE IF (IROW.EQ.12) THEN
            IRET = 0
            IF (NGRAPH.LE.1) MENUOK = .FALSE.
            IF (NGRAPH.LT.3) THEN
               IGR = MIN (1, NGRAPH)
               IPL = IGR + NGRAY
               IF (.NOT.GPH1OK) THEN
                  CALL YZERO (IPL, IRET)
                  GPH1OK = .TRUE.
                  END IF
            ELSE
               IGR = 3
               IPL = IGR + NGRAY
               IF (.NOT.GPH3OK) THEN
                  CALL YZERO (IPL, IRET)
                  GPH3OK = .TRUE.
                  END IF
               END IF
            ROUTIN = 'YZERO'
            IF (IRET.NE.0) GO TO 940
            CALL SPFBOX (IGR, 1, BLCORN, TRCORN, XYCENT, SCRTCH, IRET)
            ROUTIN = 'SPFBOX'
            IF (IRET.NE.0) GO TO 940
            OKCORN(1) = BLCORN(1).GE.1.0
            OKCORN(2) = BLCORN(2).GE.1.0
            OKCORN(3) = TRCORN(1).GE.1.0
            OKCORN(4) = TRCORN(2).GE.1.0
            IMGWIN(1) = BLCORN(1) + 0.5 + PLIMG(1)
            IMGWIN(3) = TRCORN(1) + 0.5 + PLIMG(1)
            IF (.NOT.OKCORN(1)) IMGWIN(1) = PWIND(1)
            IF (.NOT.OKCORN(3)) IMGWIN(3) = PWIND(3)
            IMGWIN(2) = PWIND(2)
            NY = CATIMG(KINAX+1) + 1
            IF (OKCORN(2)) THEN
               IY = IROUND (BLCORN(2))
               TEMP = TIMES(IY)
               DO 310 I = 1,NY
                  IF (MTIMES(I).GT.TEMP) THEN
                     IMGWIN(2) = MAX (I-1, 1)
                     GO TO 315
                     END IF
 310              CONTINUE
               END IF
 315        IMGWIN(4) = PWIND(4)
            IF (OKCORN(4)) THEN
               IY = IROUND (TRCORN(2))
               TEMP = TIMES(IY)
               DO 320 I = 1,NY
                  IF (MTIMES(I).GT.TEMP) THEN
                     IMGWIN(4) = MAX (I-1, 1)
                     GO TO 325
                     END IF
 320              CONTINUE
               END IF
 325        IF ((IMGWIN(1).LT.1) .OR. (IMGWIN(2).LT.1) .OR.
     *         (IMGWIN(3).LT.1) .OR. (IMGWIN(4).LT.1) .OR.
     *         (IMGWIN(3).GT.(CATIMG(KINAX)-3)/3) .OR.
     *         (IMGWIN(4).GT.CATIMG(KINAX+1)) .OR.
     *         (IMGWIN(1).GE.IMGWIN(3)) .OR. (IMGWIN(2).GE.IMGWIN(4)))
     *         THEN
                  WRITE (MSGTXT,1325) IMGWIN
                  CALL MSGWRT (1)
C                                       Does full x fit - yes
            ELSE
               I = (CATIMG(KINAX) - 3) / 3
               IF (I.LE.MAXXTV(1)) THEN
                  IF ((IMGWIN(1).LE.3) .OR. (IMGWIN(1).LE.(I/100.)))
     *               IMGWIN(1) = 1
                  IF ((IMGWIN(3).GE.I-2) .OR. (IMGWIN(1).GE.(0.99*I)))
     *               IMGWIN(3) = I
                  END IF
               I = BCHAN - 1
               IMGWIN(1) = IMGWIN(1) + I
               IMGWIN(3) = IMGWIN(3) + I
               WRITE (MSGTXT,1326) IMGWIN
               CALL MSGWRT (2)
               IMGWIN(1) = IMGWIN(1) - I
               IMGWIN(3) = IMGWIN(3) - I
               END IF
C                                       last piece
         ELSE IF (IROW.EQ.15) THEN
            IPIECE = IPIECE - 1
            IF (IPIECE.LE.0) IPIECE = NPIECE
            I = CATIMG(KINAX+1)
            I = (I - MPIECE) / (NPIECE-1)
            IMGWIN(2) = 1 + (IPIECE-1) * I
            IMGWIN(4) = MPIECE + (IPIECE-1) * I
            WRITE (MSGTXT,1330) IPIECE, NPIECE
            CALL MSGWRT (2)
C                                       next piece
         ELSE IF (IROW.EQ.16) THEN
            IPIECE = MOD (IPIECE, NPIECE) + 1
            I = CATIMG(KINAX+1)
            I = (I - MPIECE) / (NPIECE-1)
            IMGWIN(2) = 1 + (IPIECE-1) * I
            IMGWIN(4) = MPIECE + (IPIECE-1) * I
            WRITE (MSGTXT,1330) IPIECE, NPIECE
            CALL MSGWRT (2)
            END IF
C                                       smooth and load
         IF ((IROW.LT.11) .OR. (IROW.GT.17)) GO TO 90
            IF (IROW.EQ.14) LBASL = MOD (LBASL, CATIMG(KINAX+3)) + 1
            IF (IROW.EQ.13) THEN
               LBASL = LBASL - 1
               IF (LBASL.LE.0) LBASL = CATIMG(KINAX+3)
               END IF
 335        I = BCHAN - 1
            IMGWIN(1) = IMGWIN(1) + I
            IMGWIN(3) = IMGWIN(3) + I
            WRITE (MSGTXT,1335) CHTYPE(LTYPE), IMGWIN
            CALL MSGWRT (2)
            IMGWIN(1) = IMGWIN(1) - I
            IMGWIN(3) = IMGWIN(3) - I
            TEMP = CATID(KDCRV+2) + (LSTOKS - CATIR(KRCRP+2)) *
     *         CATIR(KRCIC+2)
            IT2 = IROUND (TEMP) + 9
            IF ((IT2.LT.1) .OR. (IT2.GT.13)) IT2 = 9
            TEMP = CATIR(KRCIC+1) * LSMOO
            WRITE (MSGTXT,1336) CHST(IT2), LBASL, TEMP
            CALL MSGWRT (2)
            IPL = 1
            CALL SPFOAD (IPL, IMGWIN, DIMB2, ABUF, IOBUF, SCRTCH,
     *         BIGBOY(1), BIGBOY(BIGPT(2)), KIGBOY(BIGPT(3)),
     *         BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)), BIGBOY(BIGPT(6)),
     *         IRET)
            ROUTIN = 'SPFOAD'
            IF (IRET.GT.100) GO TO 990
            IF (IRET.GT.0) GO TO 940
C                                       valid data -> TV display
            IF ((IRET.EQ.0) .OR. (FIRSTV)) THEN
               FIRSTV = .FALSE.
               LBASLV = NOANTS(3,LBASL)
               PLTYPE = LTYPE
               PLSTOK = LSTOKS
               PLSMOO = LSMOO
               PLSCAN = LSCAN
               IF (PLBASL.NE.LBASL) THEN
                  IF (DOBASL.EQ.1) THEN
                     IF (NOANTS(1,PLBASL).EQ.NOANTS(2,LBASL)) DOBASL = 2
                  ELSE IF (DOBASL.EQ.2) THEN
                     IF (NOANTS(2,PLBASL).EQ.NOANTS(1,LBASL)) DOBASL = 1
                     END IF
                  PLBASL = LBASL
                  END IF
               PLIMG(1) = IMGWIN(1) - 1
               PLIMG(2) = IMGWIN(2) - 1
               CALL COPY (4, IMGWIN, PWIND)
C                                       FC table
               FCTVTY = LTYPE
               FCTVBL = LBASL
               FCTVIF = 0
               FCTVST = IT2 - 9
               CALL COPY (4, IMGWIN, FCTVWI)
               FCTVAV = LSMOO * CATIR(KRCIC+1)
               FCTVSC = LSCAN * CATIR(KRCIC+1)
               END IF
            GPH4OK = .FALSE.
            ROUTIN = 'SPFLAB'
            CALL SPFLAB (IPL, PWIND, SCRTCH, BIGBOY, IRET)
            IF (IRET.GT.0) GO TO 940
            GO TO 90
C                                       flagging
C                                       FC table
 400     IF (DOBASL.EQ.3) THEN
            FCBASL(1) = 0
            FCBASL(2) = 0
         ELSE IF (DOBASL.GT.0) THEN
            FCBASL(1) = -NOANTS(DOBASL,LBASL)
            FCBASL(2) = -SUBARR
         ELSE
            FCBASL(1) = LBASLV
            FCBASL(2) = LBASLV
            END IF
         FCIF(1) = 0
         FCIF(2) = 0
         CALL CHR2H (4, STKFLG, 1, FCSFLG)
         IF (IROW.LE.7) THEN
            CALL SPFLAG (IROW, PWIND, TTY, SCRTCH, BIGBOY(BIGPT(4)),
     *         BIGBOY(BIGPT(5)), IRET)
         ELSE
            I = IROW - 7
            CALL SPFCLP (I, PWIND, TTY, SCRTCH, BIGBOY, KIGBOY, IRET)
            END IF
         IF (IRET.GT.0) GO TO 990
         IF ((IROW.EQ.10) .AND. (IRET.EQ.0)) GO TO 335
         GO TO 90
C                                       message first
 899     CALL MSGWRT (8)
C                                       Do flagging and Exit
 900     CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         DATE(1) = -DATE(1)
         CALL TIMDAT (TIME, DATE, TTIME(2), TTIME)
C                                       Mark temp file as scratch
         IF (XDOCAT.LE.0.0) THEN
            NSCR = NSCR + 1
            SCRVOL(NSCR) = DISKOU
            SCRCNO(NSCR) = CNOOUT
            END IF
         IF (NNFLAG.LE.0) THEN
            MSGTXT = 'No flagging commands were prepared'
            CALL MSGWRT (4)
         ELSE
            WRITE (MSGBUF,1900) NNFLAG
            CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF, JERR)
            IF (JERR.NE.0) GO TO 950
            IB1 = 0
 910        MSGBUF = 'Do you wish to enter them in the data?  Y/N'
 911        CALL INQSTR (TTY, MSGBUF, 4, CTEMP, JERR)
            IF (JERR.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG, TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 911
               END IF
            IF (JERR.NE.0) GO TO 950
            IB1 = IB1 + 1
            CALL CHLTOU (4, CTEMP)
            EQUAL = 'Y'.EQ.CTEMP(1:1)
            NOFLAG = 'N'.EQ.CTEMP(1:1)
            IF ((.NOT.EQUAL) .AND. (.NOT.NOFLAG)) GO TO 910
            IF (NOFLAG) THEN
               IF (XDOCAT.LE.0.0) THEN
                  IF (IB1.LE.5) THEN
                     MSGBUF = 'WARNING: THESE COMMANDS ARE ABOUT TO BE'
     *                  // ' LOST, so again:'
                     CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, MSGBUF,
     *                  JERR)
                     IF (JERR.NE.0) GO TO 950
                     IB1 = IB1 + 5
                     GO TO 910
                  ELSE
                     MSGTXT = 'SO BE IT'
                     CALL MSGWRT (6)
                     END IF
                  END IF
               NNFLAG = 0
            ELSE
               MSGTXT = 'Begin actually flagging the data'
               CALL MSGWRT (2)
               IF (LQUICK) CALL RELPOP (IRET, BUFFER, JERR)
               CALL TVCLOS (BUFFER, JERR)
               RQUICK = LQUICK
               CALL SPFMRK (BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)), JERR)
               IF (IRET.EQ.0) IRET = JERR
               GO TO 995
               END IF
            END IF
         GO TO 990
C                                       error
 940  WRITE (MSGTXT,1940) IRET, ROUTIN
      CALL MSGWRT (8)
      GO TO 990
 950  WRITE (MSGTXT,1950) JERR
      CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = JERR
      GO TO 990
 970  CALL MSGWRT (8)
      CALL ZCLOSE (TTY(1), TTY(2), JERR)
      GO TO 999
 980  CALL MSGWRT (8)
      GO TO 999
C                                       closes
 990  CALL TVCLOS (BUFFER, JERR)
 995  CALL ZCLOSE (TTY(1), TTY(2), JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT OPEN YOUR TERMINAL: ERROR',I5)
 1010 FORMAT ('CANNOT OPEN THE TV DEVICE: ERROR',I5)
 1020 FORMAT ('Basic UV image is',I5,I6,' pixels in X,Y (Ch,T)')
 1021 FORMAT ('CANNOT CREATE TV IMAGE SCRATCH FILE, ERROR',I5)
 1022 FORMAT ('CANNOT CREATE FLOATING AVERAGE SCRATCH FILE, ERROR',I5)
 1060 FORMAT ('ERROR',I6,' OPENING MASTER GRID FILE TO READ TIMES')
 1065 FORMAT ('ERROR',I6,1X,A4,'ING MASTER GRID FILE FOR TIMES')
 1070 FORMAT ('Existing FC table has',I5,' flag commands, with',I6,
     *   ' total flags')
 1130 FORMAT ('Enter baseline number range to display from 1 to',I5)
 1135 FORMAT (3X,I3,7X,I3.2,3X,'-',3X,I3.2,3X,'/',3X,I3.2)
 1200 FORMAT ('Enter BLC in ranges',I3,'-',I4,' , 1-',I5,
     *   ' (2 integers)')
 1201 FORMAT ('Enter TRC in ranges',I3,'-',I4,' , 1-',I5,
     *   ' (2 integers)')
 1220 FORMAT ('Enter ',A8,' TV-load pixel intensity range (2 reals)')
 1230 FORMAT ('Enter averaging interval in units of',F7.2,
     *   ' seconds, (integer)')
 1235 FORMAT ('Enter scan length in units of',F7.2,
     *   ' seconds, (integer)')
 1240 FORMAT ('Enter baseline pixel number between 1 and',I5,
     *   ' (integer)')
 1250 FORMAT ('Current Stokes order is taken as ',A)
 1260 FORMAT ('Enter desired IF range from',I3,' to',I3)
 1325 FORMAT ('Bad window BLC',2I5,' TRC',2I6,' try again')
 1326 FORMAT ('Set window BLC',I5,I6,' TRC',I5,I6)
 1330 FORMAT ('loading piece',I3,' of',I3)
 1335 FORMAT ('Loading ',A8,' over window BLC',I5,I6,' TRC',I5,I6)
 1336 FORMAT ('with Stokes ',A2,' baseline',I4,' smoothed to',F8.2,
     *   ' seconds')
 1900 FORMAT (I8,' Flagging commands have been prepared')
 1940 FORMAT ('TELEVISION I/O ERROR',I5,' FROM ',A)
 1950 FORMAT ('TERMINAL I/O ERROR',I5)
      END
      SUBROUTINE GTREAS (TTY, REAZON)
C-----------------------------------------------------------------------
C   GTREAS maintains a user-entered list [RLIST] of reasons to attach to
C   the flag table.  RLIST(1) defaults to the string 'SPFLG:date time'
C   which is filled in with the date and time that the flagging info was
C   generated.
C   Input:
C      TTY     I(2)   variable describing the Text and Message terminals
C   Output:
C      REAZON  C*24   reason selected by user to be attached to flags
C                     ' ' user bailed out
C   Internal:
C      RLIST C(10)*24 internally maintined list of reasons entered by
C                     user,  note that the list is REINITIALIZED when
C                     the calling program calls GTREAS for the first
C                     time and hence is NOT saved/recovered
C                     between different invocations of the calling
C                     program
C-----------------------------------------------------------------------
      INTEGER MREAS
      PARAMETER (MREAS=10)
      CHARACTER REAZON*24, RLIST(MREAS)*24, MSGBUF*72
      INTEGER IREAS, NREAS, I, J, IRET, TTY(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'SPFLG.BUF'
      SAVE RLIST
      DATA IREAS, NREAS /1,1/
      DATA RLIST /MREAS*' '/
C-----------------------------------------------------------------------
      RLIST(1) = 'SPFLG:date time'
      REAZON = ' '
C                                        show current list of reasons
 10   WRITE (MSGTXT,1010) IREAS, NREAS
      CALL MSGWRT (1)
      DO 20 I = 1,NREAS
         WRITE (MSGTXT,1015) I, RLIST(I)
         CALL MSGWRT (1)
 20      CONTINUE
C                                           what does the user want to
C                                           do?
 30   WRITE (MSGBUF,1030) NREAS
      CALL INQINT (TTY, MSGBUF, 1, I, IRET)
      IF (IRET.NE.0) GO TO 30
      IF (ABS(I).GT.NREAS) GO TO 30
      IF ((I.EQ.0) .AND. (NREAS.EQ.MREAS)) THEN
         WRITE (MSGTXT,1035) MREAS
         CALL MSGWRT (6)
         GO TO 30
         END IF
      IF (I.EQ.-1) THEN
         MSGTXT = 'Sorry, string #1 cannot be replaced'
         CALL MSGWRT (6)
         GO TO 30
         END IF
C                                       I=0: add new reason,
C                                       I<0: replace old one
      IF (I.LE.0) THEN
 40      MSGBUF = 'Enter new string text'
         CALL INQSTR (TTY, MSGBUF, 24, REAZON, IRET)
         IF (IRET.EQ.10) THEN
            MSGTXT = 'STRING TOO LONG, TRY AGAIN'
            CALL MSGWRT (7)
            GO TO 40
            END IF
         IF (IRET.NE.0) GO TO 40
         WRITE (MSGTXT,1040) REAZON
         CALL MSGWRT (2)
C                                           confirm selected reason

         IF (I.EQ.0) THEN
            WRITE (MSGBUF,1045) NREAS+1
         ELSE
            WRITE (MSGBUF,1045) -I
            END IF
         CALL INQINT (TTY, MSGBUF, 1, J, IRET)
         IF (J.EQ.-1) GO TO 999
         IF (J.NE.1) GO TO 10
         IF (I.EQ.0) THEN
            NREAS = NREAS + 1
            I = -NREAS
            END IF
         IREAS = -I
         RLIST(IREAS) = REAZON
      ELSE
         IREAS = I
         END IF
      REAZON = RLIST(IREAS)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Currently using reason string #',I2,' of ',I2)
 1015 FORMAT ('Reason string #', I2, ' = ''',A24,'''')
 1030 FORMAT ('Choose string # to use [1:',I2,
     *  '] (<0 to replace,0 for new)')
 1035 FORMAT ('Sorry maximum of ',I2,' reasons, choose again')
 1040 FORMAT ('You entered ''',A24,'''')
 1045 FORMAT ('Confirm new text for string # ',I2,
     *   ' (1=yes,0=no,-1=cancel)')
      END
      SUBROUTINE SPFCHS (RPOS, IMGWIN, WIND, SVZOOM, SCRTCH, RSCR, ICOL,
     *   IROW, IRET)
C-----------------------------------------------------------------------
C   handles the choice of operation to perform
C   In/out:
C      RPOS     R(2)   cursor position to start and end
C   Input:
C      IMGWIN   I(4)   window of next load
C      WIND     I(4)   window of last load
C      SVZOOM   I(3)   zoom parameters
C   Output:
C      SCRTCH   I(*)   scratch buffer (>1024)
C      ICOL     I      col number of choice
C      IROW     I      row number of choice
C      IRET     I      TV I/O error (no message)
C-----------------------------------------------------------------------
      REAL      RPOS(2), RSCR(*)
      INTEGER   IMGWIN(4), WIND(4), SVZOOM(3), IROW, ICOL, SCRTCH(*),
     *   IRET
C
      INTEGER   MAXR
      PARAMETER (MAXR=17)
C
      CHARACTER PS*132, CHOIC1(MAXR)*18, CHOIC2(MAXR)*18,
     *   CHOIC3(MAXR)*18, CHOIC4(MAXR)*18, CHOIC5(MAXR)*18, CHTYPE(9)*8,
     *   FLGTYP(5)*7, CHOICE(MAXR,5)*18, ROUTIN*6, STRING*180, CT(12)*1,
     *   CHST(13)*2, SUTYP(2)*10, STRIN2*100, BLFLAG*8, PRSTR*16,
     *   TRTYP(4)*4
      REAL      PPOS(2), TEMP, CATR(256)
      INTEGER   IX, IY, NC, GR1, GR2, LCOL, LROW, NROW, NCOL, PCOL, I,
     *   NROWS(5), NCHM(5), LCH(MAXR,5), NCH(MAXR,5), IXC(5), IYC(MAXR),
     *   IXP(5), IYP(5), QUAD, IBUT, LX, LY, ITW(3), IROUND, JERR, PROW,
     *   NEDGE, MASK, IT2, IWIN(4), ISU, NACROS, IZOOM(3), ZAND, MENUCH,
     *   MENU0, NCHAR, NCHAR2, NCT, ITRIM, NC1, NC2, NC3, NC4, NCC, GR4,
     *   IPL, LROWS(5)
      LOGICAL   F, DOIT, DOZOOM(MAXR,5)
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATBLK, CATR)
      EQUIVALENCE (CHOICE(1,1), CHOIC1),  (CHOICE(1,2), CHOIC2),
     *   (CHOICE(1,3), CHOIC3),  (CHOICE(1,4), CHOIC4),
     *   (CHOICE(1,5), CHOIC5)
      SAVE IXC, IYC
      DATA CHST /'YX','XY','YY','XX', 'LR','RL','LL','RR', '??',
     *   'I','Q','U ','V '/
      DATA F /.FALSE./
      DATA NCOL, LROWS /5, 14,14,17,10,1/
      DATA NCHM /10,18,18,15,4/
      DATA LCH /7, 8, 8, 8, 8, 8,10, 9,10,10,10,10,10,10, 0, 0, 0,
     *          9, 9,18,18,18,18,17,15,14,17,15,18,18,18, 0, 0, 0,
     *         17,13,11,16,16,17,18,17,18,17,17,17,18,18,15,15, 4,
     *         10,12, 9,15,15,11,14,14,15,12, 0, 0, 0, 0, 0, 0, 0,
     *          4,16*0/
      DATA DOZOOM /3*.FALSE., 3*.TRUE., 2*.FALSE., 2*.TRUE., 7*.FALSE.,
     *   17*.FALSE.,
     *   10*.FALSE., 2*.TRUE., 5*.FALSE.,
     *   10*.TRUE., 7*.FALSE.,
     *   17*.FALSE./
      DATA CHOIC1
     *   /'OFFZOOM           ', 'OFFTRANS          ',
     *    'OFFCOLOR          ', 'TVFIDDLE          ',
     *    'TVTRANSF          ', 'TVPSEUDO          ',
     *    'DO WEDGE ?        ', 'LOAD SQRT         ',
     *    'LIST FLAGS        ', 'UNDO FLAGS        ',
     *    'REDO FLAGS        ', 'LIST BASLS        ',
     *    'SET REASON        ', 'DO LABEL ?        ',
     *    '                  ', '                  ',
     *    '                  '/
      DATA CHOIC2
     *   /'ENTER BLC         ', 'ENTER TRC         ',
     *    'ENTER AMP PIXRANGE', 'ENTER PHS PIXRANGE',
     *    'ENTER RMS PIXRANGE', 'ENTER R/M PIXRANGE',
     *    'ENTER SMOOTH TIME ', 'ENTER SCAN TIME   ',
     *    'ENTER BASELINE    ', 'ENTER STOKES FLAG ',
     *    'ENTER CH SMOOTH   ', 'SWITCH SOURCE FLAG',
     *    'SWITCH BASLIN FLAG', 'SWITCH ALL-IF FLAG',
     *    '                  ', '                  ',
     *    '                  '/
      DATA CHOIC3
     *   /'DISPLAY AMPLITUDE ', 'DISPLAY PHASE     ',
     *    'DISPLAY RMS       ', 'DISPLAY RMS/MEAN  ',
     *    'DISPLAY VECT RMS  ', 'DISPLAY VRMS/VAVG ',
     *    'DISPLAY AMP V DIFF', 'DISPLAY AMPL DIFF ',
     *    'DISPLAY PHASE DIFF', 'DISPLAY STOKES    ',
     *    'OFF WINDOW + LOAD ', 'SET WINDOW + LOAD ',
     *    'LOAD LAST BASELINE', 'LOAD NEXT BASELINE',
     *    'LOAD LAST PIECE   ', 'LOAD NEXT PIECE   ',
     *    'LOAD              '/
      DATA CHOIC4
     *   /'FLAG PIXEL        ', 'FLAG/CONFIRM      ',
     *    'FLAG AREA         ', 'FLAG TIME RANGE   ',
     *    'FLAG CHANNEL-DT   ', 'FLAG A TIME       ',
     *    'FLAG A CHANNEL    ', 'CLIP BY SET #S    ',
     *    'CLIP INTERACTIV   ', 'CLIP BY FORM      ',
     *    '                  ', '                  ',
     *    '                  ', '                  ',
     *    '                  ', '                  ',
     *    '                  '/
      DATA CHOIC5
     *   /'EXIT              ', 16*' '/
      DATA CHTYPE /'AMPLTUDE', 'PHASE   ', 'RMS AMPL', 'RMS/MEAN',
     *   'RMS VAMP', 'VRMS/AVG', 'VEC DIFF', 'AMP DIFF', 'PHS DIFF'/
      DATA FLGTYP /'ALL-BL', 'ALL-IF', 'ONE-BL', 'ONE-IF','> 1-IF'/
      DATA SUTYP /'ALL-SOURCE', 'ONE-SOURCE'/
      DATA TRTYP /'LOG ','SQRT','LOG2','LIN '/
C-----------------------------------------------------------------------
      IROW = 0
      ICOL = 0
      CALL COPY (NCOL, LROWS, NROWS)
      NROW = MAXR
      I = NCOL * NROW
      CALL COPY (I, LCH, NCH)
      IF (NPIECE.GT.1) THEN
         CHOIC3(MAXR-2) = 'LOAD LAST PIECE'
         CHOIC3(MAXR-1) = 'LOAD NEXT PIECE'
         NCH(NROWS(3)-2,3) = 15
         NCH(NROWS(3)-1,3) = 15
      ELSE
         CHOIC3(MAXR-2) = 'LOAD'
         NCH(NROWS(3)-2,3) = 4
         NROWS(3) = NROWS(3) - 2
         END IF
C                                       set spacing
      NEDGE = (CSIZTV(1) + 1) / 2
      IF (NEDGE.LT.2) NEDGE = 2
      NC = 0
      DO 5 I = 1,NCOL
         NC = NC + NCHM(I)
 5       CONTINUE
 6    MENUCH = 2 * (2 + NEDGE + (NCOL-1) * (1+NEDGE)) + CSIZTV(1) * NC
      IF (MENUCH.GT.MAXXTV(1)) THEN
         NEDGE = NEDGE - 1
         IF (NEDGE.GT.0) GO TO 6
            MSGTXT = 'TV SCREEN TOO NARROW.  BUY A BIGGER ONE'
            CALL MSGWRT (9)
            IRET = 8
            GO TO 999
         END IF
      CALL ZTIME (ITW)
      PROW = 0
      PCOL = 0
      IZOOM(1) = 0
      IZOOM(2) = MAXXTV(1)/2
      IZOOM(3) = MAXXTV(2)/2
      IF ((TVZOOM(1).NE.IZOOM(1)) .OR. (TVZOOM(2).NE.IZOOM(2)) .OR.
     *   (TVZOOM(3).NE.IZOOM(3))) THEN
         CALL YZOOMC (IZOOM(1), IZOOM(2), IZOOM(3), F, IRET)
         ROUTIN = 'YZOOMC'
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       check window of TV
      CALL YWINDO ('READ', WINDTV, IRET)
      ROUTIN = 'YWINDO'
      IF (IRET.NE.0) GO TO 990
      IF ((WINDTV(1).NE.LWINTV(1,1)) .OR. (WINDTV(2).NE.LWINTV(2,1))
     *   .OR. (WINDTV(3).NE.LWINTV(3,1)) .OR.
     *   (WINDTV(4).NE.LWINTV(4,1))) THEN
         MENUOK = .FALSE.
         GPH4OK = .FALSE.
         END IF
      CALL COPY (4, WINDTV, LWINTV(1,1))
C                                       init the display
      IF (NGRAPH.LT.2) THEN
         GR2 = NGRAY + 1
         GR4 = GR2
         GR1 = 0
C                                       set highlight graphics
      ELSE
         GR2 = NGRAY + 2
         GR4 = GR2
         IF (NGRAPH.GE.4) GR4 = NGRAY + 4
         GR1 = 1 + NGRAY
         IF (.NOT.GPH1OK) THEN
            CALL YZERO (GR1, IRET)
            ROUTIN = 'YZERO'
            IF (IRET.NE.0) GO TO 900
            GPH1OK = .TRUE.
            END IF
         MASK = 2 ** (GR1 - 1)
         MASK = ZAND (MASK, TVLIMG(1))
         IF (MASK.EQ.0) THEN
            CALL YSLECT ('ONNN', GR1, 0, SCRTCH, IRET)
            ROUTIN = 'YSLECT'
            IF (IRET.NE.0) GO TO 900
            END IF
         END IF
C                                       turn on cursor
      QUAD = -1
      IF ((RPOS(1).LE.WINDTV(1)) .OR. (RPOS(1).GT.WINDTV(3)))
     *   RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      IF ((RPOS(2).LE.WINDTV(2)) .OR. (RPOS(2).GT.WINDTV(4)))
     *   RPOS(2) = WINDTV(4) - 10 - 3 * CSIZTV(2)
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 900
C                                       turn on graphics
      CALL YHOLD ('ONNN', I)
      MASK = 2 ** (GR2 - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GR2, 0, SCRTCH, IRET)
         ROUTIN = 'YSLECT'
         IF (IRET.NE.0) GO TO 900
         END IF
      IF ((.NOT.GPH4OK) .AND. (GR4.GT.GR2)) THEN
         CALL YZERO (GR4, IRET)
         ROUTIN = 'YZERO'
         IF (IRET.NE.0) GO TO 900
         IF (DOLABL) THEN
            IPL = 1
            CALL SPFLAB (IPL, WIND, SCRTCH, RSCR, IRET)
            ROUTIN = 'SPFLAB'
            IF (IRET.NE.0) GO TO 900
            END IF
         END IF
      MASK = 2 ** (GR4 - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GR4, 0, SCRTCH, IRET)
         ROUTIN = 'YSLECT'
         IF (IRET.NE.0) GO TO 900
         END IF
C                                       init the graphics display
      IF (.NOT.MENUOK) THEN
         NC1 = 0
         NC2 = 0
         NC3 = 0
         NC4 = 0
         CALL YZERO (GR2, IRET)
         ROUTIN = 'YZERO'
         IF (IRET.NE.0) GO TO 900
C                                       write to planes: strings
         MENU0 = 4 + WINDTV(1)
         IF (MENU0+MENUCH.GT.MAXXTV(1)) MENU0 = MAXXTV(1) - MENUCH
         ROUTIN = 'IMCHAR'
         IX = 3 + NEDGE + MENU0
         DO 20 LCOL = 1,NCOL
            IY = WINDTV(4) - 12 - NEDGE - CSIZTV(2)
            NROW = NROWS(LCOL)
            IXC(LCOL) = IX
            DO 10 LROW = 1,NROW
               IYC(LROW) = IY
               NC = NCH(LROW,LCOL)
               PS = CHOICE(LROW,LCOL)(1:NC)
               IF ((LROW.EQ.8) .AND. (LCOL.EQ.1))
     *            PS = 'LOAD ' // TRTYP(ITRTYP)
               CALL IMCHAR (GR2, IX, IY, 0, 0, PS(1:NC), SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 900
               IY = IY - 2*NEDGE - CSIZTV(2)
 10            CONTINUE
            IX = IX + 2 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
 20         CONTINUE
C                                       write border lines
         IX = 1 + MENU0
         IY = WINDTV(4) - 11
         DO 30 LCOL = 1,NCOL
            LX = 4 + 2*NEDGE + NCHM(LCOL)*CSIZTV(1)
            LY = NROWS(LCOL) * (2*NEDGE + CSIZTV(2)) + 4
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IF (LCOL.EQ.4) MAXMEN = IXP(2) + 5 - WINDTV(1)
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT ('ONNN', GR2, 5, IXP, IYP, SCRTCH, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 900
            IX = IX + 1
            IY = IY - 1
            LX = LX - 2
            LY = LY - 2
            IXP(1) = IX
            IYP(1) = IY
            IXP(2) = IX + LX - 1
            IYP(2) = IYP(1)
            IXP(3) = IXP(2)
            IYP(3) = IY - LY + 1
            IXP(4) = IXP(1)
            IYP(4) = IYP(3)
            IXP(5) = IXP(1)
            IYP(5) = IYP(1)
            CALL IMVECT ('ONNN', GR2, 5, IXP, IYP, SCRTCH, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 900
            IX = IX + LX - 1
            IY = IY + 1
 30         CONTINUE
         END IF
C                                       change variable part
      NSTOKS = CATIMG(KINAX+2)
      I = MOD (LSTOKS, NSTOKS) + 1
      I = ILSTOK(I) + 9
      IF ((I.LT.1) .OR. (I.GT.13)) I = 9
      IY = IYC(10)
      IX = IXC(3) + 15 * CSIZTV(1)
      CALL IMCHAR (GR2, IX, IY, 0, 0, CHST(I)(:2), SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       status line
      IX = 5 + 2*NEDGE
      IY = CSIZTV(2) + WINDTV(2)
      IX = IX + WINDTV(1)
      WRITE (BLFLAG,1025) NOANTS(1,PLBASL), NOANTS(2,PLBASL)
      IF (MOD(DOBASL,2).EQ.1) BLFLAG(7:8) = '**'
      IF (DOBASL.GE.2) BLFLAG(4:5) = '**'
      LY = 4
      IF (DOIFS.EQ.1) LY = 2
      IF (DOIFS.EQ.0) THEN
         LY = 5
         WRITE (FLGTYP(LY),1029) LCIF
         END IF
      ISU = 1
      IF (DOSOUR) ISU = 2
      CT(1) = '*'
      CT(2) = '*'
      CT(3) = '*'
      CT(4) = '*'
      CT(5) = '*'
      CT(6) = '*'
      CT(7) = '*'
      CT(8) = '*'
      CT(9) = '*'
      CT(10) = '*'
      CT(11) = '*'
      CT(12) = '*'
      IF (LTYPE.EQ.PLTYPE) CT(1) = '_'
      IF (LBASL.EQ.PLBASL) CT(2) = '_'
      IF (LSMOO.EQ.PLSMOO) CT(4) = '_'
      IF (IMGWIN(1).EQ.WIND(1)) CT(5) = ' '
      IF (IMGWIN(2).EQ.WIND(2)) CT(6) = '_'
      IF (IMGWIN(3).EQ.WIND(3)) CT(7) = ' '
      IF (IMGWIN(4).EQ.WIND(4)) CT(8) = '_'
      IF (LSTOKS.EQ.PLSTOK) CT(10) = '_'
      IF (LSCAN.EQ.PLSCAN) CT(11) = '_'
      IF (LCSMOT.EQ.CSMOTH) CT(12) = '_'
      TEMP = CATID(KDCRV+2) + (PLSTOK-CATIR(KRCRP+2)) * CATIR(KRCIC+2)
      IT2 = IROUND (TEMP) + 9
      IF ((IT2.LT.1) .OR. (IT2.GT.13)) IT2 = 9
      IWIN(1) = WIND(1) + BCHAN - 1
      IWIN(3) = WIND(3) + BCHAN - 1
      IWIN(2) = WIND(2)
      IWIN(4) = WIND(4)
C                                       choose form that fits
      CALL PRFRMT (CATR(IRRAN), PRSTR)
      ROUTIN = 'IMCHAR'
      NACROS = (WINDTV(3) - IX) / CSIZTV(1)
      NCT = ITRIM (CHTYPE(PLTYPE))
      WRITE (STRING,1030) CHTYPE(PLTYPE)(:NCT), CT(1), PRSTR, PLBASL,
     *   NOANTS(1,PLBASL), NOANTS(2,PLBASL), SUBARR, CT(2), PLSMOO,
     *   CT(4), IWIN(1), CT(5), IWIN(2), CT(6), IWIN(3), CT(7), IWIN(4),
     *   CT(8), SUTYP(ISU), BLFLAG, FLGTYP(LY), PLSCAN, CT(11),
     *   CSMOTH, CT(12), CHST(IT2), CT(10), USTFLG
      CALL REFRMT (STRING, '_', NCHAR)
      IF (NACROS.GE.NCHAR) THEN
         NCC = MAX (NC1, NCHAR)
         NC1 = NCHAR
         CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 900
         MAXLAB = 1
      ELSE
         WRITE (STRING,1035) IWIN(1), CT(5), IWIN(2), CT(6), IWIN(3),
     *      CT(7), IWIN(4), CT(8), PLSMOO, CT(4), PLSCAN, CT(11),
     *      CSMOTH, CT(12), CHST(IT2), CT(10), USTFLG
         CALL REFRMT (STRING, '_', NCHAR)
         WRITE (STRIN2,1036) CHTYPE(PLTYPE)(:NCT), CT(1), PRSTR, PLBASL,
     *      NOANTS(1,PLBASL), NOANTS(2,PLBASL), SUBARR, CT(2),
     *      SUTYP(ISU), BLFLAG, FLGTYP(LY)
         CALL REFRMT (STRIN2, '_', NCHAR2)
         IF (NACROS.GT.MAX(NCHAR,NCHAR2)) THEN
            NCC = MAX (NC1, NCHAR)
            NC1 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            NCC = MAX (NC2, NCHAR2)
            NC2 = NCHAR2
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRIN2(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            MAXLAB = 2
         ELSE IF (NACROS.GE.51) THEN
            WRITE (STRING,1040) PLSCAN, CT(11), CSMOTH, CT(12),
     *         CHST(IT2), CT(10), USTFLG, BLFLAG
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC1, NCHAR)
            NC1 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            WRITE (STRING,1041) IWIN(1), CT(5), IWIN(2), CT(6), IWIN(3),
     *         CT(7), IWIN(4), CT(8), SUTYP(ISU), FLGTYP(LY)
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC2, NCHAR)
            NC2 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            WRITE (STRING,1042) CHTYPE(PLTYPE)(:NCT), CT(1), PRSTR,
     *         PLBASL, NOANTS(1,PLBASL), NOANTS(2,PLBASL), SUBARR,
     *         CT(2), PLSMOO, CT(4)
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC3, NCHAR)
            NC3 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            MAXLAB = 3
         ELSE IF (NACROS.GE.39) THEN
            WRITE (STRING,1045) PLSMOO, CT(4), SUTYP(ISU), BLFLAG,
     *         FLGTYP(LY)
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC1, NCHAR)
            NC1 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            WRITE (STRING,1046) PLSCAN, CT(11), CSMOTH, CT(12),
     *         CHST(IT2), CT(10), USTFLG
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC2, NCHAR)
            NC2 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            WRITE (STRING,1047) IWIN(1), CT(5), IWIN(2), CT(6), IWIN(3),
     *         CT(7), IWIN(4), CT(8)
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC3, NCHAR)
            NC3 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            IY = IY + NEDGE + CSIZTV(2)
            WRITE (STRING,1048) CHTYPE(PLTYPE)(:NCT), CT(1), PLBASL,
     *         NOANTS(1,PLBASL), NOANTS(2,PLBASL), SUBARR, CT(2)
            CALL REFRMT (STRING, '_', NCHAR)
            NCC = MAX (NC4, NCHAR)
            NC4 = NCHAR
            CALL IMCHAR (GR4, IX, IY, 0, 0, STRING(:NCC), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 900
            MAXLAB = 4
         ELSE
            MSGTXT = 'WINDOW IS TOO NARROW FOR STATUS DISPLAYS !'
            CALL MSGWRT (7)
            END IF
         END IF
C                                       read a choice
      CALL YHOLD ('OFFF', I)
      MENUOK = .TRUE.
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      MSGTXT = 'Press buttons A, B, or C to choose an operation'
      CALL MSGWRT (1)
      MSGTXT = 'Press button D for on-line help'
      CALL MSGWRT (1)
C                                        read until cursor moves
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 50
C                                        find the choice
         IX = IROUND (RPOS(1))
         IY = IROUND (RPOS(2))
         ICOL = 0
         DO 55 LCOL = 1,NCOL
            IF (IX.GE.IXC(LCOL)-2) ICOL = LCOL
 55         CONTINUE
         IF (ICOL.LE.0) GO TO 50
         IROW = 0
         NROW = NROWS(ICOL)
         DO 60 LROW = 1,NROW
            IF (IY.GT.IYC(NROW+1-LROW)-2) IROW = NROW+1-LROW
 60         CONTINUE
         IF (IROW.EQ.0) GO TO 50
         IF (GR1.GT.0) THEN
            IF ((PCOL.NE.ICOL) .OR. (PROW.NE.IROW) .OR. ((IBUT.GT.0)
     *         .AND. (IBUT.LE.7))) THEN
C                                       restore choice
              ROUTIN = 'IMCHAR'
               IF ((PCOL.GT.0) .AND. (PROW.GT.0)) THEN
                  CALL YHOLD ('ONNN', I)
                  NC = NCH(PROW,PCOL)
                  PS(1:NC) = ' '
                  CALL IMCHAR (GR1, IXC(PCOL), IYC(PROW), 0, 0,
     *               PS(1:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 800
                  PS(1:NC) = CHOICE(PROW,PCOL)(1:NC)
                  IF ((PROW.EQ.8) .AND. (PCOL.EQ.1))
     *               PS = 'LOAD ' // TRTYP(ITRTYP)
                  CALL IMCHAR (GR2, IXC(PCOL), IYC(PROW), 0, 0,
     *               PS(1:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 800
C                                       change variable part
                  IF ((PCOL.EQ.3) .AND. (PROW.EQ.10)) THEN
                     NSTOKS = CATIMG(KINAX+2)
                     I = MOD (LSTOKS, NSTOKS) + 1
                     I = ILSTOK(I) + 9
                     IF ((I.LT.1) .OR. (I.GT.13)) I = 9
                     IY = IYC(10)
                     IX = IXC(3) + 15 * CSIZTV(1)
                     CALL IMCHAR (GR2, IX, IY, 0, 0, CHST(I)(:2),
     *                  SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 900
                     END IF
                  CALL YHOLD ('OFFF', I)
                  END IF
C                                       highlight choice
               IF ((IBUT.LE.0) .OR. (IBUT.GT.7)) THEN
                  NC = NCH(IROW,ICOL)
                  PS(1:NC) = CHOICE(IROW,ICOL)(1:NC)
                  IF ((IROW.EQ.8) .AND. (ICOL.EQ.1))
     *               PS = 'LOAD ' // TRTYP(ITRTYP)
                  CALL IMCHAR (GR1, IXC(ICOL), IYC(IROW), 0, 0,
     *               PS(1:NC), SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 800
C                                       change variable part
                  IF ((ICOL.EQ.3) .AND. (IROW.EQ.10)) THEN
                     NSTOKS = CATIMG(KINAX+2)
                     I = MOD (LSTOKS, NSTOKS) + 1
                     I = ILSTOK(I) + 9
                     IF ((I.LT.1) .OR. (I.GT.13)) I = 9
                     IY = IYC(10)
                     IX = IXC(3) + 15 * CSIZTV(1)
                     CALL IMCHAR (GR1, IX, IY, 0, 0, CHST(I)(:2),
     *                  SCRTCH, IRET)
                     IF (IRET.NE.0) GO TO 900
                     END IF
                  END IF
               PCOL = ICOL
               PROW = IROW
               END IF
            END IF
C                                       leave on button A, B, C
         IF (IBUT.GE.8) THEN
            NC = NCH(IROW,ICOL)
            IF (CHOICE(IROW,ICOL)(:15).EQ.'DISPLAY STOKES ') NC = 15
            PS = CHOICE(IROW,ICOL)(1:NC)
            IF ((IROW.EQ.8) .AND. (ICOL.EQ.1))
     *         PS = 'LOAD ' // TRTYP(ITRTYP)
            CALL TSKHLP (PS, NC, ' ', JERR)
            IBUT = 0
         ELSE IF (IBUT.GT.0) THEN
            GO TO 800
            END IF
         GO TO 50
C                                       turn off the cursor
 800  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
C                                       turn off the choices
 900  IF (DOZOOM(IROW,ICOL)) THEN
         CALL YHOLD ('ONNN', I)
         CALL YSLECT ('OFFF', GR2, 0, SCRTCH, JERR)
         IF (IRET.EQ.0) THEN
            CALL COPY (3, SVZOOM, TVZOOM)
            CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, JERR)
            END IF
         CALL YHOLD ('OFFF', I)
         END IF
C                                       force buffer to TV
      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, JERR)
C
 990  IF (IRET.EQ.0) GO TO 999
         WRITE (MSGTXT,1990) IRET, ROUTIN
         CALL MSGWRT (6)
         CALL YHOLD ('OFFF', I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('BL=',I2.2,'-',I2.2)
 1029 FORMAT ('IF',I2.2,'-',I2.2)
 1030 FORMAT (A,A1,1X,A,' _Bl',I5,'(',I2.2,'-',I2.2,'/',I2.2,')',A1,1X,
     *   'Avg',I5,A1,1X,'BLC',2(I5,A1),1X,'TRC',2(I5,A1),3('__',A),
     *   ' _Scan',I5,A1,' CSmooth',F5.1,A1,' Show ',A2,A1,' flag ',A4)
 1035 FORMAT ('BLC',2(I5,A1),1X,'TRC',2(I5,A1),' Avg',I5,A1,' Scan',I5,
     *  A1,' CSmooth',F5.1,A1,' Show ',A2,A1,' flag ',A4)
 1036 FORMAT (A,A1,1X,A,' _Bl',I5,'(',I2.2,'-',I2.2,'/',I2.2,')',A1,1X,
     *   A,2('__',A))
 1040 FORMAT ('Scan',I5,A1,' CSmooth',F5.1,A1,' Show ',A2,A1,' flag ',
     *   A4,'__',A)
 1041 FORMAT ('BLC',2(I5,A1),1X,'TRC',2(I5,A1),'__',A,'__',A)
 1042 FORMAT (A,A1,1X,A,' _Bl',I5,'(',I2.2,'-',I2.2,'/',I2.2,')',A1,1X,
     *   'Avg',I5,A1)
 1045 FORMAT ('Avg',I5,A1,3('__',A))
 1046 FORMAT ('Scan',I5,A1,' CSmooth',F5.1,A1,' Show ',A2,A1,' flag ',
     *   A4)
 1047 FORMAT ('BLC',2(I6,A1),3X,'TRC',2(I6,A1))
 1048 FORMAT (A,A1,2X,'Bl',I5,'(',I2.2,'-',I2.2,'/',I2.2,')',A1)
 1990 FORMAT ('SPFCHS: TV I/O ERROR',I7,' FROM ',A)
      END
      SUBROUTINE SPFUNC (BRANCH, PPOS, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   performs TV enhancement functions for SPFLG
C   Inputs:
C      BRANCH   I         1 => off zoom
C                         2 => off black and white transfer
C                         3 => off pseudo color
C                         4 => do TVFIDDLE
C                         5 => do TVtransfer (black and white)
C                         6 => do TVpseudo (various pseudo colors)
C   In/out:
C      PPOS     I(2,9)    previous cursor positions for various modes:
C                         1 for transfer, 2-4 for pseudo's
C   Output:
C      SCRTCH   I(*)      scratch buffer: > 3072 (IENHNS)
C      IRET     I         TV error (no message)
C-----------------------------------------------------------------------
      INTEGER   BRANCH, SCRTCH(*), IRET
      REAL      PPOS(2,9)
C
      INCLUDE 'INCS:PTVC.INC'
      LOGICAL   F
      INTEGER   I, J, IC, ICOLOR, NLEVS, GR1, II, JJ
      REAL      SLOPE, OFFS, RBUF(TVMOFM)
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.6)) GO TO 999
      GO TO (100, 200, 300, 400, 500, 600), BRANCH
C-----------------------------------------------------------------------
C                                       off zoom
 100  CONTINUE
         I = TVZOOM(1)
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         IF (I.NE.0) THEN
            CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IRET)
         ELSE
            IRET = 0
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                       off transfer: channel 1
 200  CONTINUE
         IC = 2 ** NGRAY - 1
         ICOLOR = 7
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 210 I = 1,NLEVS
            SCRTCH(I) = I * SLOPE - 0.5
 210        CONTINUE
         CALL YLUT ('WRIT', IC, ICOLOR, F, SCRTCH, IRET)
         GO TO 999
C-----------------------------------------------------------------------
C                                       off pseudo (off colors)
 300  CONTINUE
         I = OFMINP + 1
         ICOLOR = 7
         CALL RFILL (I, 0.0, RBUF)
         NLEVS = LUTOUT + 1
         IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 310 I = 1,NLEVS
            RBUF(I) = I * SLOPE - OFFS
 310        CONTINUE
         I = OFMINP + 1
         JJ = NLEVS
         I = I / NLEVS
         DO 311 II = 2,I
            IF (JJ+NLEVS.LE.I) CALL RCOPY (NLEVS, RBUF, RBUF(JJ+1))
            JJ = JJ + NLEVS
 311        CONTINUE
         CALL YOFM ('WRIT', ICOLOR, F, RBUF, IRET)
         GO TO 999
C-----------------------------------------------------------------------
C                                       TVFIDDLE
 400  CONTINUE
         NLEVS = LUTOUT + 1
         IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
         IC = 2 ** NGRAY - 1
         CALL TVFIDL (IC, NLEVS, SCRTCH, IRET)
         GO TO 999
C-----------------------------------------------------------------------
C                                       TVTRANSF
 500  CONTINUE
         IF (NGRAPH.LE.1) MENUOK = .FALSE.
         IF (.NOT.GPH1OK) THEN
            J = NGRAY + 1
            CALL YZERO (J, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         GPH1OK = .FALSE.
         MSGTXT = 'Cursor x position controls intercept'
         CALL MSGWRT (1)
         MSGTXT = 'Cursor y position controls slope'
         CALL MSGWRT (1)
         MSGTXT = 'Hit buttons A or B to turn plot off or back on'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to reverse sign of slope'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to exit TV transfer and return to menu'
         CALL MSGWRT (1)
C                                        off graphics
         IF (NGRAPH.LE.2) THEN
            GPH1OK = .FALSE.
            GR1 = 1 + NGRAY
            CALL YSLECT ('OFFF', GR1, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE IF (NGRAPH.EQ.3) THEN
            MENUOK = .FALSE.
            GR1 = 2 + NGRAY
            CALL YZERO (GR1, IRET)
            IF (IRET.NE.0) GO TO 999
         ELSE IF (NGRAPH.EQ.4) THEN
            GPH3OK = .FALSE.
            GR1 = 3 + NGRAY
            CALL YSLECT ('OFFF', GR1, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                        hide this mess in subroutine
         J = 1
         IC = 1
         ICOLOR = 7
         CALL IENHNS (IC, ICOLOR, J, PPOS(1,1), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 999
C                                        clean up again
         IF (NGRAPH.LE.4) THEN
            CALL YZERO (GR1, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL YSLECT ('ONNN', GR1, 0, SCRTCH, IRET)
            IF (NGRAPH.LE.2) GPH1OK = .TRUE.
            IF (NGRAPH.EQ.4) GPH3OK = .TRUE.
            END IF
         GO TO 999
C-----------------------------------------------------------------------
C                                       TVPSEUDO
C                                       Button A: RGB triangles
C                                       Button B: Loops in hue
C                                       Button C: color contours
 600  CONTINUE
         NLEVS = LUTOUT + 1
         IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
         CALL TVPSUD (NLEVS, RBUF, IRET)
C
 999  RETURN
      END
      SUBROUTINE SPFLAG (BRANCH, IMGWIN, TTY, SCRTCH, BUFF, BUFF2, IRET)
C-----------------------------------------------------------------------
C   does various forms of interactive TV flagging using the currently
C   displayed file
C   Inputs:
C      BRANCH   I      1 => flag pixel
C                      2 => flag pixel and confirm
C                      3 => flag area CH-T
C                      4 => flag timerange
C                      5 => flag one channel, timerange
C                      6 => flag time
C                      7 => flag channel
C      IMGWIN   I(4)   currently loaded image window
C   Output:
C      BUFF     R(*)   IO buffer
C      BUFF2    R(*)   IO buffer
C      SCRTCH   I(*)   TV scratch buffer
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   BRANCH, IMGWIN(4), TTY(2), SCRTCH(*), IRET
      REAL      BUFF(*), BUFF2(*)
C
      INCLUDE 'SPFLG.INC'
      CHARACTER FIXIT*4, CHTEMP*4, ROUTIN*6, STRING*20, MSGBUF*72,
     *   PHNAME*48, OPERS(7)*8, BLNAME*16
      INTEGER   GR1, GR3, NPIX, NROW, NEDGE, MAG, IX0, IY0, IX, IY,
     *   LUN1, LUN2, FIND1, FIND2, QUAD, IBUT, ITW(3), TVX, TVY, J, I,
     *   IWIN(4), NB, JERR, IXP(5), IERR, IYL0, KST, IYP(5), DOBLC,
     *   LUN0, FIND0, I1, I2, LBUT, LUN3, FIND3, NRPL, IYL, NCTI,
     *   IYH, IB1, IB2, IXL, IXH, ISOU, IBLKOF, IDEPTH(5), KBBASL,
     *   KEBASL, KBASL, NFLAGD, IYH0, LTVX, LTVY, LSOU, NSNUMS, SL,
     *   SNUMS(XSTBSZ), ISNUM, MASK, ZAND, NCHAN, MCHAN, LIF, I4, IXINC,
     *   LCHAN, IROUND, LDOIFS, LMGWIN(4), LMGCOR(4), PIXLIM(2)
      LOGICAL   DOANOT, DOIT, T, F, BLANKD, DOBOX, DOTV, DOSTOK, FIX
      REAL      CATR(256), RPOS(2), TEMP, PPOS(2), CORN(2), PIXVAL,
     *   TIM1, TIM2
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA FIXIT /'FXIT'/
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN0, LUN1, LUN2, LUN3 /16,17,18,19/
      DATA OPERS /'PIXEL', 'PIXEL', 'AREA', 'TIMERANG', 'CHANL-DT',
     *   'TIME', 'CHANNEL'/
C-----------------------------------------------------------------------
      BLNAME = ' '
      LTVX = -1
      LTVY = -1
      LSOU = -2
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      IXINC = CATBLK(IICOR+2) - CATBLK(IICOR)
      IXINC = MAX (1, IXINC)
      IXINC = (CATBLK(IIWIN+2) - CATBLK(IIWIN)) / IXINC
      IXINC = MAX (1, IXINC)
      NCHAN = ECHAN - BCHAN + 1
      LCHAN = NCHAN / IXINC
      CALL COPY (4, CATBLK(IIWIN), LMGWIN)
      CALL COPY (4, CATBLK(IICOR), LMGCOR)
C                                       cursor on
      QUAD = -1
      RPOS(1) = (LMGCOR(1) + LMGCOR(3)) / 2
      RPOS(2) = (LMGCOR(2) + LMGCOR(4)) / 2
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
C                                       pixel limits
      RPOS(1) = 1
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      CALL IMA2MP (RPOS, CORN)
      PIXLIM(1) = CORN(1)
      RPOS(1) = MAXXTV(1)
      RPOS(2) = (LMGCOR(2) + LMGCOR(4)) / 2
      CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
      CALL IMA2MP (RPOS, CORN)
      PIXLIM(2) = CORN(1) + 0.9
C                                       init controls
      DOBOX = (BRANCH.GE.3) .AND. (BRANCH.LE.5)
      CALL FILL (5, 1, IXP)
      CALL FILL (5, 1, IYP)
      NB = 2
      DOTV = DOSTOK (ILSTOK, STKFLG, PLSTOK)
      IF (.NOT.DOTV) THEN
         MSGTXT = 'WARNING: flag command does not apply to displayed'
     *      // ' Stokes'
         CALL MSGWRT (6)
         END IF
C                                       FC table
      FCLIPR(1) = 0.0
      FCLIPR(2) = 0.0
      CALL CHR2H (8, OPERS(BRANCH), 1, FCOPER)
C                                       NO background blanking here
      GR1 = 1 + NGRAY
      GR3 = 1 + NGRAY
      IF (NGRAPH.GT.2) GR3 = 3 + NGRAY
C                                       Zero AND ON the graphics
      FIND0 = 0
      FIND1 = 0
      FIND2 = 0
      FIND3 = 0
      IF (NGRAPH.LE.1) MENUOK = .FALSE.
      CALL YHOLD ('ONNN', I)
      IF (.NOT.GPH1OK) THEN
         CALL YZERO (GR1, IRET)
         ROUTIN = 'YZERO'
         IF (IRET.NE.0) GO TO 800
         END IF
      IF ((GR3.NE.GR1) .AND. (.NOT.GPH3OK)) THEN
         CALL YZERO (GR3, IRET)
         IF (IRET.NE.0) GO TO 800
         END IF
      DOANOT = .TRUE.
      ROUTIN = 'YSLECT'
      MASK = 2 ** (GR1 - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GR1, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         END IF
      MASK = 2 ** (GR3 - 1)
      MASK = ZAND (MASK, TVLIMG(1))
      IF (MASK.EQ.0) THEN
         CALL YSLECT ('ONNN', GR3, 0, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         END IF
      CALL YHOLD ('OFFF', I)
C                                       CURVAL display location
      SL = MAX (11+2*TFORM, LSNAME)
      NPIX = SL * CSIZTV(1)
      NEDGE = (2 * MAXXTV(1)) / 512
      IF (NEDGE.LT.2) NEDGE = 2
      NROW = 3
      IF (MAXSOU.GT.0) NROW = NROW + 1
      NROW = NROW * (2*NEDGE + CSIZTV(2))
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
      IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
      IX0 = MAX (1, IX0)
      IY0 = MAX (1, IY0)
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
C                                       set to top row of text
      IX0 = IX0 + NEDGE
      IY0 = IY0 + 5*NEDGE + 2*CSIZTV(2)
      IF (MAXSOU.GT.0) IY0 = IY0 + 2*NEDGE + CSIZTV(2)
C                                       get image header
      IX = (WINDTV(1) + WINDTV(3)) / 2
      IY = (WINDTV(2) + WINDTV(4)) / 2
      IF (XYCENT(1).GT.0) IX = XYCENT(1)
      IF (XYCENT(2).GT.0) IY = XYCENT(2)
      CALL YCREAD (1, IX, IY, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       open TV image SC file
      CALL ZPHFIL ('SC', SCRVOL(TVFILE), SCRCNO(TVFILE), 1, PHNAME,
     *   IRET)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(TVFILE), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN (R)', IRET
         GO TO 990
         END IF
      CALL ZOPEN (LUN3, FIND3, SCRVOL(TVFILE), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN (W)', IRET
         GO TO 990
         END IF
C                                       open master grid file
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
      CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) 'OPEN (W)', IRET
         GO TO 900
         END IF
      CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1015) 'OPEN (R)', IRET
         GO TO 900
         END IF
C                                       prepare interaction
      RPOS(1) = IX
      RPOS(2) = IY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL ZTIME (ITW)
C                                       instructions: Buttons
 90   IF (.NOT.DOBOX) THEN
         MSGTXT = 'Hit button A or B to mark flagged position, '
     *      // 'loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark flagged position, return'
     *      // ' to menu'
      ELSE
         MSGTXT = 'Hit button A to switch between BLC and TRC'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button B to mark final box, loop for more'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button C to mark final box, return to menu'
         END IF
      CALL MSGWRT (1)
C                                       instructions: Button D
      MSGTXT = 'Hit button D to exit - no further flagging'
      CALL MSGWRT (1)
      NFLAGD = 0
      DOBLC = 0
C                                        read until cursor moves
 100  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      ROUTIN = 'YCURSE'
      IF (IRET.NE.0) GO TO 800
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 100
C                                       button D
         IF (IBUT.GE.8) GO TO 910
C                                       get TV and uv image pixels
         CALL YCURSE (FIXIT, F, T, RPOS, QUAD, LBUT, IRET)
         CALL IMA2MP (RPOS, CORN)
         IF (CORN(1).LT.CATBLK(IIWIN)) CORN(1) = CATBLK(IIWIN)
         IF (CORN(2).LT.CATBLK(IIWIN+1)) CORN(2) = CATBLK(IIWIN+1)
         IF (CORN(1).GT.CATBLK(IIWIN+2)) CORN(1) = CATBLK(IIWIN+2)
         IF (CORN(2).GT.CATBLK(IIWIN+3)) CORN(2) = CATBLK(IIWIN+3)
C                                       restrain DOIFS=0 to IFs
         IF (DOIFS.EQ.0) THEN
            FIX = .FALSE.
            I1 = LCIF(2) - BIF + 1
            I1 = I1 * NCHAN -1 - IMGWIN(1) + 5
            IF (I1.LT.CORN(1)) THEN
               FIX = .TRUE.
               CORN(1) = I1
               END IF
            I1 = LCIF(1) - BIF
            I1 = I1 * NCHAN - IMGWIN(1) + 5
            IF (I1.GT.CORN(1)) THEN
               FIX = .TRUE.
               CORN(1) = I1
               END IF
            IF (FIX) THEN
               IF ((CORN(1).GT.PIXLIM(2)) .OR. (CORN(1).LT.PIXLIM(1)))
     *            THEN
                  WRITE (MSGTXT,1100) LCIF
                  CALL MSGWRT (6)
                  GO TO 910
                  END IF
               CALL MP2IMA (CORN, RPOS)
               CALL YCURSE ('ONNN', F, F, RPOS, QUAD, I1, IRET)
               ROUTIN = 'YCURSE'
               IF (IRET.NE.0) GO TO 800
               END IF
            END IF
         TVX = IROUND (CORN(1))
         TVY = IROUND (CORN(2))
         CALL MP2IMA (CORN, RPOS)
C                                       Do CURVALUE anotation
         IF (DOANOT) THEN
            CALL YHOLD ('ONNN', I)
            GPH1OK = .FALSE.
            IWIN(1) = 1
            IWIN(2) = TVY
            IWIN(3) = TVX
            IWIN(4) = TVY
            CALL MINIT ('READ', LUN2, FIND2, CATBLK(KINAX),
     *         CATBLK(KINAX+1), IWIN, BUFF, JBUFSZ, 1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT (R)', IRET
               GO TO 900
               END IF
            CALL MDISK ('READ', LUN2, FIND2, BUFF, I, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'READ', IRET
               GO TO 900
               END IF
            PIXVAL = BUFF(I+TVX-1)
            ISOU = BUFF(I) + 0.01
            IF ((ISOU.LE.0) .OR. (ISOU.GT.MAXSOU)) ISOU = INSNUM
            BLANKD = PIXVAL.EQ.FBLANK
            IY = IY0
            ROUTIN = 'IMCHAR'
C                                       source name
            IF (MAXSOU.GT.0) THEN
               IF (ISOU.NE.LSOU) THEN
                  IF (ISOU.GT.0) THEN
                     CALL IMCHAR (GR1, IX0, IY, 0, 0,
     *                  SNAMES(ISOU)(:SL), SCRTCH, IRET)
                  ELSE
                     CALL IMCHAR (GR1, IX0, IY, 0, 0, BLNAME, SCRTCH,
     *                  IRET)
                     END IF
                  IF (IRET.NE.0) GO TO 800
                  END IF
               LSOU = ISOU
               IY = IY - 2*NEDGE - CSIZTV(2)
               END IF
C                                       time
            IF (TVY.NE.LTVY) THEN
               TEMP = (TIMES(TVY) + TIMES(TVY+1)) / 2.0
               CALL TORMAT (TEMP, TFORM, STRING, NCTI)
               CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(:NCTI), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            LTVY = TVY
            IY = IY - 2*NEDGE - CSIZTV(2)
C                                       channel/IF
            IF (LTVX.NE.TVX) THEN
               IX = TVX + IMGWIN(1) - 5
               I1 = MOD (IX, NCHAN) + BCHAN
               I2 = IX / NCHAN + BIF
               WRITE (STRING,1115) I1, I2
               CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:SL), SCRTCH,
     *            IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            IY = IY - 2*NEDGE - CSIZTV(2)
            LTVX = TVX
C                                       flux
            IF (BLANKD) THEN
               STRING = 'Blanked'
            ELSE IF (ABS(PIXVAL).LT.9.99) THEN
               WRITE (STRING,1127) PIXVAL
            ELSE IF (ABS(PIXVAL).LT.1000.) THEN
               WRITE (STRING,1126) PIXVAL
            ELSE
               WRITE (STRING,1125) PIXVAL
               END IF
            CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:8), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 800
            CALL YHOLD ('OFFF', I)
            END IF
C                                       clear old graph 3 lines
         IF ((BRANCH.GE.3) .AND. (GR3.GT.0)) THEN
            GPH3OK = .FALSE.
            I = 2
            IF (DOBOX) I = 5
            IF ((DOBOX) .AND. (DOBLC.EQ.0)) I = 3
            CALL IMVECT ('OFFF', GR3, I, IXP, IYP, SCRTCH, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 800
C                                       flag a channel
            IF (BRANCH.EQ.7) THEN
               IXP(1) = IROUND (RPOS(1))
               IXP(2) = IXP(1)
               CORN(2) = LMGWIN(2)
               CALL MP2IMA (CORN, RPOS)
               IYP(1) = IROUND (RPOS(2))
               CORN(2) = LMGWIN(4)
               CALL MP2IMA (CORN, RPOS)
               IYP(2) = IROUND (RPOS(2))
               IYP(1) = MAX (IYP(1), LMGCOR(2))
               IYP(2) = MIN (IYP(2), LMGCOR(4))
C                                       flag a time
            ELSE IF (BRANCH.EQ.6) THEN
               IYP(1) = IROUND (RPOS(2))
               IYP(2) = IYP(1)
C                                       all IFs
               IF (DOIFS.EQ.1) THEN
                  TVX = LMGWIN(1)
                  CORN(1) = TVX
                  CALL MP2IMA (CORN, RPOS)
                  IXP(1) = IROUND (RPOS(1))
                  CORN(1) = LMGWIN(3)
                  CALL MP2IMA (CORN, RPOS)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(1) = MAX (LMGCOR(1), IXP(1))
                  IXP(2) = MIN (LMGCOR(3), IXP(2))
C                                           some IFs ?? ok?
               ELSE IF (DOIFS.EQ.0) THEN
                  I1 = LCIF(1) - BIF
                  TVX = I1 * NCHAN - IMGWIN(1) + 5
                  CORN(1) = TVX
                  CALL MP2IMA (CORN, RPOS)
                  IXP(1) = IROUND (RPOS(1))
                  I1 = LCIF(2) - BIF + 1
                  TVX = I1 * NCHAN -1 - IMGWIN(1) + 5
                  CORN(1) = TVX
                  CALL MP2IMA (CORN, RPOS)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(1) = MAX (LMGCOR(1), IXP(1))
                  IXP(2) = MIN (LMGCOR(3), IXP(2))
C                                       current IF
               ELSE
                  I1 = (TVX + IMGWIN(1) - 5)
                  I1 = I1 / NCHAN
                  TVX = I1 * NCHAN - IMGWIN(1) + 5
                  CORN(1) = TVX
                  CALL MP2IMA (CORN, RPOS)
                  IXP(1) = IROUND (RPOS(1))
                  CORN(1) = CORN(1) + NCHAN - 1
                  CALL MP2IMA (CORN, RPOS)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(1) = MAX (LMGCOR(1), IXP(1))
                  IXP(2) = MIN (LMGCOR(3), IXP(2))
                  END IF
C                                       one channel, timerange
            ELSE IF (BRANCH.EQ.5) THEN
C                                       init blc
               IF (DOBLC.EQ.0) THEN
                  I = 3
                  IYP(1) = IROUND (RPOS(2))
                  IYP(2) = IYP(1)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(3) = IXP(2)
                  CORN(2) = LMGWIN(4)
                  CALL MP2IMA (CORN, RPOS)
                  IYP(3) = IROUND (RPOS(2))
                  IYP(3) = MIN (IYP(3), LMGCOR(4))
                  IF (DOIFS.EQ.1) THEN
                     CORN(1) = LMGWIN(1)
                  ELSE IF (DOIFS.EQ.0) THEN
                     I1 = LCIF(1) - BIF
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                  ELSE
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     END IF
                  CALL MP2IMA (CORN, RPOS)
                  IXP(1) = IROUND (RPOS(1))
                  IXP(1) = MAX (LMGCOR(1), IXP(1))
C                                       blc
               ELSE IF (DOBLC.EQ.1) THEN
                  IYP(1) = IROUND (RPOS(2))
                  IYP(2) = IYP(1)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(3) = IXP(2)
                  IF (DOIFS.EQ.-1) THEN
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     IXP(1) = MAX (LMGCOR(1), IXP(1))
                     CORN(1) = CORN(1) + NCHAN - 1
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                     IXP(4) = MIN (LMGCOR(3), IXP(4))
                     IXP(5) = IXP(4)
                     END IF
C                                       trc
               ELSE
                  IYP(3) = IROUND (RPOS(2))
                  IYP(4) = IYP(3)
                  IYP(5) = IYP(4)
                  IXP(2) = IROUND (RPOS(1))
                  IXP(3) = IXP(2)
                  IF (DOIFS.EQ.1) THEN
                     CORN(1) = LMGWIN(3)
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                  ELSE IF (DOIFS.EQ.0) THEN
                     I1 = LCIF(2) - BIF + 1
                     TVX = I1 * NCHAN -1 - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                  ELSE
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     IXP(1) = MAX (LMGCOR(1), IXP(1))
                     CORN(1) = CORN(1) + NCHAN - 1
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                     END IF
                  IXP(4) = MIN (LMGCOR(3), IXP(4))
                  IXP(5) = IXP(4)
                  END IF
C                                       start a box
            ELSE IF (DOBLC.EQ.0) THEN
               I = 3
               IYP(2) = IROUND (RPOS(2))
               IYP(3) = IYP(2)
C                                       timerange
               IF (BRANCH.EQ.4) THEN
                  IYP(1) = IYP(2)
                  IF (DOIFS.EQ.1) THEN
                     TVX = LMGWIN(1)
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     CORN(1) = LMGWIN(3)
                     CALL MP2IMA (CORN, RPOS)
                     IXP(3) = IROUND (RPOS(1))
                  ELSE IF (DOIFS.EQ.0) THEN
                     I1 = LCIF(1) - BIF
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     I1 = LCIF(2) - BIF + 1
                     TVX = I1 * NCHAN -1 - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(3) = IROUND (RPOS(1))
                  ELSE
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     CORN(1) = CORN(1) + NCHAN - 1
                     CALL MP2IMA (CORN, RPOS)
                     IXP(3) = IROUND (RPOS(1))
                     END IF
                  IXP(1) = MAX (LMGCOR(1), IXP(1))
                  IXP(3) = MIN (LMGCOR(3), IXP(3))
               ELSE
                  IXP(1) = IROUND (RPOS(1))
                  CORN(1) = LMGWIN(3)
                  IF (DOIFS.EQ.0) CORN(1) = LCIF(2)*NCHAN - IMGWIN(1)+5
                  CORN(2) = LMGWIN(4)
                  CALL MP2IMA (CORN, RPOS)
                  IYP(1) = IROUND (RPOS(2))
                  IXP(3) = IROUND (RPOS(1))
                  IYP(1) = MIN (IYP(1), LMGCOR(4))
                  IXP(3) = MIN (IXP(3), LMGCOR(3))
                  END IF
               IXP(2) = IXP(1)
C                                       redo blc
            ELSE IF (DOBLC.EQ.1) THEN
               IF (BRANCH.EQ.4) THEN
                  IF (DOIFS.LT.0) THEN
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     CORN(1) = CORN(1) + NCHAN - 1
                     CALL MP2IMA (CORN, RPOS)
                     IXP(3) = IROUND (RPOS(1))
                     IXP(1) = MAX (LMGCOR(1), IXP(1))
                     IXP(3) = MIN (LMGCOR(3), IXP(3))
                     IXP(4) = IXP(3)
                     END IF
               ELSE
                  IXP(1) = IROUND (RPOS(1))
                  END IF
               IXP(2) = IXP(1)
               IXP(5) = IXP(1)
               IYP(2) = IROUND (RPOS(2))
               IYP(3) = IYP(2)
C                                       trc
            ELSE
               IF (BRANCH.EQ.4) THEN
                  IF (DOIFS.LT.0) THEN
                     I1 = (TVX + IMGWIN(1) - 5)
                     I1 = I1 / NCHAN
                     TVX = I1 * NCHAN - IMGWIN(1) + 5
                     CORN(1) = TVX
                     CALL MP2IMA (CORN, RPOS)
                     IXP(1) = IROUND (RPOS(1))
                     CORN(1) = CORN(1) + NCHAN - 1
                     CALL MP2IMA (CORN, RPOS)
                     IXP(3) = IROUND (RPOS(1))
                     IXP(1) = MAX (LMGCOR(1), IXP(1))
                     IXP(3) = MIN (LMGCOR(3), IXP(3))
                     IXP(2) = IXP(1)
                     IXP(5) = IXP(1)
                     END IF
               ELSE
                  IXP(3) = IROUND (RPOS(1))
                  END IF
               IXP(4) = IXP(3)
               IYP(4) = IROUND (RPOS(2))
               IYP(5) = IYP(4)
               IYP(1) = IYP(4)
               END IF
            CALL IMVECT ('ONNN', GR3, I, IXP, IYP, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 800
            END IF
C                                       check buttons
         IF (IBUT.EQ.0) GO TO 100
C                                       button B, C = A w/o a TRC
         IF ((DOBOX) .AND. (DOBLC.LE.0)) IBUT = 1
C                                       switch BLC/TRC in box mode
         IF ((IBUT.EQ.1) .AND. (DOBOX)) THEN
            IF (DOBLC.LE.0) THEN
               DOBLC = 1
               I = 3
               CALL IMVECT ('OFFF', GR3, I, IXP, IYP, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 800
C                                       area
               IF (BRANCH.EQ.3) THEN
                  IYP(1) = IYP(2) + 10
                  IYP(4) = IYP(1)
                  IYP(5) = IYP(1)
                  IXP(3) = IXP(1) + 10
                  IXP(4) = IXP(3)
                  IXP(5) = IXP(1)
C                                       timerange
               ELSE IF (BRANCH.EQ.4) THEN
                  IYP(1) = IYP(2) + 10
                  IYP(4) = IYP(1)
                  IYP(5) = IYP(1)
                  IXP(4) = IXP(3)
                  IXP(5) = IXP(1)
C                                       channel-dt
               ELSE
                  IYP(3) = IYP(2) + 10
                  IYP(4) = IYP(3)
                  IYP(5) = IYP(4)
                  IF (DOIFS.EQ.-1) THEN
                     IXP(4) = IXP(1) + NCHAN - 1
                  ELSE IF (DOIFS.EQ.0) THEN
                     CORN(1) = LCIF(2)*NCHAN - IMGWIN(1)+5
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                  ELSE
                     CORN(1) = LMGWIN(3)
                     CALL MP2IMA (CORN, RPOS)
                     IXP(4) = IROUND (RPOS(1))
                     END IF
                  IXP(4) = MIN (IXP(4), LMGCOR(3))
                  IXP(5) = IXP(4)
                  END IF
               I = 5
               CALL IMVECT ('ONNN', GR3, I, IXP, IYP, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            DOBLC = 3 - DOBLC
            I = 2*DOBLC
            RPOS(1) = IXP(I)
            IF (BRANCH.EQ.5) RPOS(1) = IXP(2)
            RPOS(2) = IYP(I)
            CALL YCURSE ('ONNN', F, T, RPOS, QUAD, LBUT, IRET)
            IF (IRET.NE.0) THEN
               ROUTIN = 'YCURSE'
               IF (IRET.NE.2) GO TO 800
               CALL YCURSE ('ONNN', F, F, RPOS, QUAD, LBUT, IRET)
               IF (IRET.NE.0) GO TO 800
               END IF
            GO TO 100
            END IF
C                                       confirm the pixel
         IF (BRANCH.EQ.2) THEN
            IF (.NOT.DOANOT) THEN
               TEMP = (TIMES(TVY) + TIMES(TVY+1)) / 2.0
               CALL TORMAT (TEMP, TFORM, STRING, NCTI)
               IX = TVX + BCHAN + IMGWIN(1) - 5
               END IF
            WRITE (MSGBUF,1200) IX, STRING(:NCTI)
 101        CALL INQSTR (TTY, MSGBUF, 4, CHTEMP, IRET)
            IF (IRET.EQ.10) THEN
               MSGTXT = 'STRING TOO LONG, TRY AGAIN'
               CALL MSGWRT (7)
               GO TO 101
               END IF
            IF (IRET.NE.0) GO TO 850
            CALL CHLTOU (4, CHTEMP)
            IF (CHTEMP(1:1).NE.'Y') THEN
               IF (IBUT.GE.4) GO TO 910
               GO TO 100
               END IF
            END IF
C                                       erase graphics first
         I = 2
         IF (DOBOX) I = 5
         IF ((BRANCH.GT.2) .AND. (GR3.GT.0)) THEN
            CALL IMVECT ('OFFF', GR3, I, IXP, IYP, SCRTCH, IRET)
            ROUTIN = 'IMVECT'
            IF (IRET.NE.0) GO TO 800
            END IF
C                                       clear data from TV
         LDOIFS = DOIFS
         IF (EIF.LE.BIF) LDOIFS = -1
         IF (BRANCH.EQ.7) THEN
            IXP(4) = IXP(2)
            IYP(4) = IYP(2)
            IYP(2) = IYP(1)
         ELSE IF (BRANCH.EQ.6) THEN
            IXP(4) = IXP(2)
            IXP(2) = IXP(1)
            IYP(4) = IYP(2)
         ELSE IF (BRANCH.EQ.5) THEN
            IXP(4) = IXP(2)
            IYP(4) = IYP(3)
         ELSE IF (BRANCH.LT.3) THEN
            IXP(2) = IROUND (RPOS(1))
            IXP(4) = IXP(2)
            IYP(2) = IROUND (RPOS(2))
            IYP(4) = IYP(2)
            END IF
         IF (IXP(2).GT.IXP(4)) THEN
            I = IXP(2)
            IXP(2) = IXP(4)
            IXP(4) = I
            END IF
         IF (IYP(2).GT.IYP(4)) THEN
            I = IYP(2)
            IYP(2) = IYP(4)
            IYP(4) = I
            END IF
C                                       test for excess area
         IF ((LDOIFS.GE.0) .AND. (BRANCH.EQ.3)) THEN
               RPOS(1) = IXP(2)
               RPOS(2) = IYP(2)
               CALL IMA2MP (RPOS, CORN)
               TVX = IROUND (CORN(1))
               I1 = TVX + IMGWIN(1) - 5
               I1 = I1 / NCHAN
               RPOS(1) = IXP(4)
               RPOS(2) = IYP(4)
               CALL IMA2MP (RPOS, CORN)
               TVX = IROUND (CORN(1))
               I2 = TVX + IMGWIN(1) - 5
               I2 = I2 / NCHAN
               IF (I2.NE.I1) THEN
                  IF (LDOIFS.EQ.1) THEN
                     MSGTXT = 'AREA crosses IF boundary, ignoring' //
     *                  ' ALL-IF'
                  ELSE
                     MSGTXT = 'AREA crosses IF boundary, ignoring' //
     *                  ' multiple IFs'
                     END IF
                  CALL MSGWRT (6)
                  LDOIFS = -1
                  END IF
               END IF
C                                       black out flagged data
         IF (DOTV) THEN
            CALL YHOLD ('ONNN', I)
            IF (LDOIFS.GE.0) THEN
               RPOS(1) = IXP(2)
               RPOS(2) = IYP(2)
               CALL IMA2MP (RPOS, CORN)
               TVX = IROUND (CORN(1))
               I1 = TVX + IMGWIN(1) - 5
               I1 = I1 / NCHAN
               IXP(2) = IXP(2) - I1 * LCHAN
               I2 = IXP(2)
               IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.6)) THEN
                  IXP(4) = IXP(2) + LCHAN - 1
               ELSE
                  RPOS(1) = IXP(4)
                  RPOS(2) = IYP(4)
                  CALL IMA2MP (RPOS, CORN)
                  TVX = IROUND (CORN(1))
                  I1 = TVX + IMGWIN(1) - 5
                  I1 = I1 / NCHAN
                  IXP(4) = IXP(4) - I1 * LCHAN
                  END IF
               I4 = IXP(4)
               DO 260 LIF = BIF,EIF
                  IF ((LDOIFS.EQ.1) .OR. ((LDOIFS.EQ.0) .AND.
     *               (LIF.GE.LCIF(1)) .AND. (LIF.LE.LCIF(2)))) THEN
                     IF ((IXP(4).GE.1) .AND. (IXP(2).LE.MAXXTV(1))) THEN
                        IXP(3) = MAX (1, IXP(2))
                        IXP(5) = MIN (MAXXTV(1), IXP(4))
                        CALL YFILL (1, IXP(3), IYP(2), IXP(5), IYP(4),
     *                     0, SCRTCH, IRET)
                        IF (IRET.NE.0) GO TO 265
                        END IF
                     END IF
                  IXP(2) = IXP(2) + LCHAN
                  IXP(4) = IXP(4) + LCHAN
 260              CONTINUE
               IXP(2) = I2
               IXP(4) = I4
            ELSE
               CALL YFILL (1, IXP(2), IYP(2), IXP(4), IYP(4), 0, SCRTCH,
     *            IRET)
               END IF
 265        CALL YHOLD ('OFFF', I)
            ROUTIN = 'YFILL'
            IF (IRET.NE.0) GO TO 800
            END IF
         DOBLC = 0
         RPOS(1) = PPOS(1)
         RPOS(2) = PPOS(2)
         ROUTIN = 'YCURSE'
         CALL YCURSE ('OFFF', F, F, RPOS, QUAD, LBUT, IRET)
         IF (IRET.NE.0) GO TO 800
C                                       set corners
         RPOS(1) = IXP(2)
         RPOS(2) = IYP(2)
         CALL IMA2MP (RPOS, CORN)
         IXL = IROUND (CORN(1))
         IXL = IXL - 3
         IF (LDOIFS.GE.0) THEN
            IXL = IXL + (EIF+1) * NCHAN
            IXL = MOD (IXL-1, NCHAN) + 1
            END IF
         IF (IXL.LT.1) IXL = 1
         RPOS(1) = IXP(4)
         CALL IMA2MP (RPOS, CORN)
         IXH = IROUND (CORN(1))
         IF (IXH.GT.CATBLK(KINAX)) IXH = CATBLK(KINAX)
         IXH = IXH - 3
         IF (LDOIFS.GE.0) THEN
            IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.6)) THEN
               IXH = IXL + NCHAN - 1
            ELSE
               IXH = IXH + (EIF+1) * NCHAN
               IXH = MOD (IXH-1, NCHAN) + 1
               END IF
            END IF
C                                       clear data from TV file
         NRPL = NBPS / (NB * CATBLK(KINAX))
         IF (NRPL.LT.1) NRPL = 1
         IWIN(1) = 1
         IWIN(3) = CATBLK(KINAX)
         IXH = IXH - IXL + 1
         IXL = IXL - 1
         IF (LDOIFS.GE.0) IXL = MOD (IXL, NCHAN)
         RPOS(1) = IXP(2)
         RPOS(2) = IYP(2)
         CALL IMA2MP (RPOS, CORN)
         IYL = CORN(2) + 0.01
         IF (IYL.LT.1) IYL = 1
         RPOS(2) = IYP(4)
         CALL IMA2MP (RPOS, CORN)
         IYH = IROUND (CORN(2))
         IF (IYH.GT.CATBLK(KINAX+1)) IYH = CATBLK(KINAX+1)
         IWIN(2) = ((IYL - 1) / NRPL) * NRPL + 1
         IWIN(4) = ((IYH - 1 + NRPL) / NRPL) * NRPL
         IF (IWIN(2).LT.1) IWIN(2) = 1
         IF (IWIN(2).GT.CATBLK(KINAX+1)) IWIN(2) = CATBLK(KINAX+1)
         IF (IWIN(4).LT.1) IWIN(4) = 1
         IF (IWIN(4).GT.CATBLK(KINAX+1)) IWIN(4) = CATBLK(KINAX+1)
         IF (DOTV) THEN
            CALL MINIT ('READ', LUN2, FIND2, CATBLK(KINAX),
     *         CATBLK(KINAX+1), IWIN, BUFF, JBUFSZ, 1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT (R)', IRET
               GO TO 900
               END IF
            CALL MINIT ('WRIT', LUN3, FIND3, CATBLK(KINAX),
     *         CATBLK(KINAX+1), IWIN, BUFF2, JBUFSZ, 1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT (W)', IRET
               GO TO 900
               END IF
            I1 = IWIN(2)
            I2 = IWIN(4)
            DO 280 IY = I1,I2
               CALL MDISK ('READ', LUN2, FIND2, BUFF, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'READ', IRET
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN3, FIND3, BUFF2, IB2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'WRIT', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATBLK(KINAX), BUFF(IB1), BUFF2(IB2))
               IF ((IY.GE.IYL) .AND. (IY.LE.IYH)) THEN
                  IB2 = IB2 + 3
                  IF (LDOIFS.EQ.-1) THEN
                     CALL RFILL (IXH, FBLANK, BUFF2(IB2+IXL))
C                                       ?? okay?
                  ELSE
                     DO 275 LIF = BIF,EIF
                        IF ((LDOIFS.EQ.1) .OR. ((LDOIFS.EQ.0) .AND.
     *                     (LIF.GE.LCIF(1)) .AND. (LIF.LE.LCIF(2))))
     *                     CALL RFILL (IXH, FBLANK, BUFF2(IB2+IXL))
                        IB2 = IB2 + NCHAN
 275                    CONTINUE
                     END IF
                  END IF
 280           CONTINUE
            CALL MDISK ('FINI', LUN3, FIND3, BUFF2, IB2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'FINI', IRET
               GO TO 900
               END IF
            END IF
C                                       clear the uv file
         NRPL = NBPS / (NB * CATIMG(KINAX))
         IF (NRPL.LT.1) NRPL = 1
C                                       open FC table
         CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         FCNUMB = FCNUMB + 1
         IF (IRET.NE.0) GO TO 999
         NSNUMS = 0
         SNUMS(1) = 0
         IWIN(1) = 1
         IWIN(3) = CATIMG(KINAX)
         IF (((BRANCH.EQ.4) .OR. (BRANCH.EQ.6)) .AND. (LDOIFS.GE.1))
     *      THEN
            IXL = 0
            IXH = NCHAN
         ELSE
            IXL = IXL + IMGWIN(1) - 1
            IF (LDOIFS.GE.0) IXL = MOD (IXL-1, NCHAN) + 1
            IXL = IXL * 3
            END IF
         IYL0 = IYL
         IYH0 = IYH
         IF (BRANCH.EQ.7) THEN
            IYL = 1
            IYH = CATIMG(KINAX+1)
         ELSE
            TIM1 = TIMES(IYL0)
            TIM2 = TIMES(IYH0+1)
            CALL GETIME (TIM1, TIM2, IYL, IYH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1280) TIM1, TIM2, IYL, IYH
               CALL MSGWRT (8)
               GO TO 325
               END IF
            END IF
         IWIN(2) = ((IYL - 1) / NRPL) * NRPL + 1
         IWIN(4) = ((IYH - 1 + NRPL) / NRPL) * NRPL
         IF (IWIN(2).LT.1) IWIN(2) = 1
         IF (IWIN(2).GT.CATIMG(KINAX+1)) IWIN(2) = CATIMG(KINAX+1)
         IF (IWIN(4).LT.1) IWIN(4) = 1
         IF (IWIN(4).GT.CATIMG(KINAX+1)) IWIN(4) = CATIMG(KINAX+1)
         IF (DOBASL.GT.0) THEN
            KBBASL = 1
            KEBASL = CATIMG(KINAX+3)
         ELSE
            KBBASL = PLBASL
            KEBASL = PLBASL
            END IF
         DO 321 KBASL = KBBASL,KEBASL
            IF ((DOBASL.EQ.1) .OR. (DOBASL.EQ.2)) THEN
               KST = NOANTS(DOBASL,PLBASL)
               IF ((KST.NE.NOANTS(1,KBASL)) .AND.
     *            (KST.NE.NOANTS(2,KBASL))) GO TO 321
               END IF
         DO 320 KST = 1,4
            IF (.NOT.DOSTOK (ILSTOK, STKFLG, KST)) GO TO 320
            IDEPTH(1) = KST
            IDEPTH(2) = KBASL
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1015) 'INIT (R)', IRET
               GO TO 900
               END IF
            CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF2, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1015) 'INIT (W)', IRET
               GO TO 900
               END IF
            I1 = IWIN(2)
            I2 = IWIN(4)
            DO 315 IY = I1,I2
               CALL MDISK ('READ', LUN0, FIND0, BUFF, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1015) 'READ', IRET
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BUFF2, IB2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1015) 'WRITE', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF(IB1), BUFF2(IB2))
               IF ((IY.GE.IYL) .AND. (IY.LE.IYH)) THEN
                  IF (DOSOUR) THEN
                     ISNUM = BUFF2(IB2) + 0.01
                     IF (ISNUM.LE.0) ISNUM = INSNUM
                     IF (ISNUM.GT.0) THEN
                        IF (NSNUMS.GT.0) THEN
                           DO 290 IX = 1,NSNUMS
                              IF (ISNUM.EQ.SNUMS(IX)) GO TO 295
 290                          CONTINUE
                           END IF
                        NSNUMS = NSNUMS + 1
                        SNUMS(NSNUMS) = ISNUM
                        END IF
                     END IF
 295              J = IB2 + IXL + 5
                  IF (LDOIFS.GE.0) THEN
                     DO 299 IX = 1,IXH
                        I4 = J
                        DO 298 LIF = BIF,EIF
                           IF ((LDOIFS.EQ.1) .OR. ((LDOIFS.EQ.0) .AND.
     *                        (LIF.GE.LCIF(1)) .AND. (LIF.LE.LCIF(2))))
     *                        THEN
                              IF (BUFF2(I4).EQ.0.0) THEN
                                 NFLAGD = NFLAGD + 1
                                 BUFF2(I4) = FCNUMB
                                 END IF
                              END IF
                           I4 = I4 + 3*NCHAN
 298                       CONTINUE
                        J = J + 3
 299                    CONTINUE
                  ELSE
                     DO 300 IX = 1,IXH
                        IF (BUFF2(J).EQ.0.0) THEN
                           NFLAGD = NFLAGD + 1
                           BUFF2(J) = FCNUMB
                           END IF
                        J = J + 3
 300                    CONTINUE
                     END IF
                  END IF
 315           CONTINUE
            CALL MDISK ('FINI', LUN1, FIND1, BUFF2, IB2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1015) 'FINI', IRET
               GO TO 900
               END IF
 320        CONTINUE
 321        CONTINUE
C                                       record the flag(s)
 325     NSNUMS = MAX (1, NSNUMS)
         IF (NFLAGD.GT.0) THEN
            IF ((BRANCH.EQ.4) .OR. (BRANCH.EQ.6)) THEN
               FCCHAN(1) = 0
               FCCHAN(2) = 0
               IX = IXL / 3
               I1 = IX / NCHAN + BIF
               I2 = I1
            ELSE IF ((BRANCH.EQ.5) .OR. (BRANCH.EQ.7)) THEN
               IX = IXL / 3
               I1 = IX / NCHAN + BIF
               I2 = I1
               FCCHAN(1) = MOD (IX, NCHAN) + BCHAN
               FCCHAN(2) = FCCHAN(1)
            ELSE
               IX = IXL / 3
               I1 = IX / NCHAN + BIF
               FCCHAN(1) = MOD (IX, NCHAN) + BCHAN
               IX = IX + IXH - 1
               I2 = IX / NCHAN + BIF
               MCHAN = MOD (IX, NCHAN) + BCHAN
               IF (I2.EQ.I1) THEN
                  FCCHAN(2) = MCHAN
               ELSE
                  FCCHAN(2) = ECHAN
                  END IF
               END IF
            IF (BRANCH.EQ.7) THEN
               FCTIME(1) = START
               FCTIME(2) = STOP
            ELSE
               FCTIME(1) = TIM1
               FCTIME(2) = TIM2
               END IF
            FCIF(1) = LCIF(1)
            FCIF(2) = LCIF(2)
            DO 335 I = I1,I2
               IF ((DOIFS.EQ.-1) .OR. (I2.GT.I1)) THEN
                  FCIF(1) = I
                  FCIF(2) = I
                  END IF
               IF (I.GT.I1) FCCHAN(1) = 1
               IF ((I.EQ.I2) .AND. (I2.GT.I1)) FCCHAN(2) = MCHAN
               DO 330 IX = 1,NSNUMS
                  NNFLAG = NNFLAG + 1
                  FCSOUR = MAX (0, SNUMS(IX))
                  CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF, IRET)
                  IF (IRET.NE.0) GO TO 999
 330              CONTINUE
 335           CONTINUE
C                                       success message
            WRITE (MSGTXT,1330) NFLAGD
            CALL MSGWRT (3)
C                                       failed message
         ELSE
            MSGTXT = 'WARNING: No previously unflagged samples were ' //
     *         'flagged ********'
            CALL MSGWRT (6)
            MSGTXT = 'So no entry was made in the Flag Command table'
            CALL MSGWRT (6)
            END IF
C                                       close FC file
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       reinit the TV
         CALL FILL (5, 1, IXP)
         CALL FILL (5, 1, IYP)
         RPOS(1) = PPOS(1)
         RPOS(2) = PPOS(2)
         PPOS(1) = 0.
         PPOS(2) = 0.
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, LBUT, IRET)
         ROUTIN = 'YCURSE'
         IF (IRET.NE.0) GO TO 800
         IF (IBUT.GE.4) GO TO 910
         GO TO 90
C                                       TV error
 800  WRITE (MSGTXT,1800) IRET, ROUTIN
      GO TO 900
C                                       TTY error
 850  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1850) IRET
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'Return to menu, non-numeric input'
         IRET = 0
         END IF
C                                       Error message first
 900  CALL MSGWRT (8)
C                                       off cursor
 910  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, LBUT, JERR)
      IF (FIND0.GT.0) CALL ZCLOSE (LUN0, FIND0, JERR)
      IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, JERR)
      IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, JERR)
      IF (FIND3.GT.0) CALL ZCLOSE (LUN3, FIND3, JERR)
C                                       erase graphics first
      CALL YHOLD ('ONNN', I)
      I = 2
      IF (DOBOX) I = 5
      IF ((BRANCH.GT.2) .AND. (GR3.GT.0)) THEN
         CALL IMVECT ('OFFF', GR3, I, IXP, IYP, SCRTCH, JERR)
         IF (JERR.EQ.0) GPH3OK = .TRUE.
         END IF
      IY = IY0
      NROW = 3
      IF (MAXSOU.GT.0) NROW = 4
      JERR = 0
      DO 915 I = 1,NROW
         IF (JERR.EQ.0) CALL IMCHAR (GR1, IX0, IY, 0, 0, BLNAME, SCRTCH,
     *      JERR)
         IY = IY - 2*NEDGE - CSIZTV(2)
 915     CONTINUE
      IF (JERR.EQ.0) GPH1OK = .TRUE.
      CALL YHOLD ('OFFF', I)
      GO TO 999
C                                       error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('SPFLAG: UNABLE TO READ IMAGE HEADER, ERROR',I5)
 1010 FORMAT ('SPFLAG: UNABLE TO ',A,' TV SCRATCH FILE, ERROR',I5)
 1015 FORMAT ('SPFLAG: UNABLE TO ',A,' MAIN GRID FILE, ERROR',I5)
 1100 FORMAT ('CURRENT ZOOM DOES NOT INCLUDE IFS',2I4)
 1115 FORMAT ('Ch',I5,'/',I2)
 1125 FORMAT (F8.1)
 1126 FORMAT (F8.3)
 1127 FORMAT (F8.5)
 1200 FORMAT ('Flag Channel=',I5,'  Time=',A,3X,'Y or N ?')
 1280 FORMAT ('SPFLAG: GETIME ERROR T1,T2,IYL,IYH=',2(1PE12.4),2I6)
 1330 FORMAT ('Flagged',I11,' more samples in the master grid file')
 1800 FORMAT ('TV ERROR ',I6,' IN ',A)
 1850 FORMAT ('TERMINAL ERROR',I5)
      END
      SUBROUTINE SPFCLP (BRANCH, IMGWIN, TTY, SCRTCH, BIGBOY, KIGBOY,
     *   IRET)
C-----------------------------------------------------------------------
C   does flagging of all pixels in the TV image outside a user-set range
C   of intensities.  That range is set in SPFCLP.
C   Inputs:
C      BRANCH   I      1 => clip levels entered on terminal
C                      2 => clip levels entered interactively
C                      3 => clip range of channels/IFs based on the
C                           clip parms of previous clip.
C      IMGWIN   I(4)   currently loaded image window
C   Output:
C      SCRTCH   I(*)   TV scratch buffer
C      BIGBOY   R(*)   Large IO buffers
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   BRANCH, IMGWIN(4), TTY(2), KIGBOY(*), SCRTCH(*), IRET
      REAL      BIGBOY(*)
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER ROUTIN*6, MSGBUF*72, STRING*12, DSTKFG*4, OP*8,
     *   TTKFLG*4, STKFS(4)*4, CHST(13)*2
      HOLLERITH CATH(256)
      INTEGER   GR1, NPIX, NROW, NEDGE, MAG, IX0, IY0, IX, IY, I, QUAD,
     *   IBUT, ITW(3), J, JERR, I1, I2, ICL, ALUT(TVMLUT,3),
     *   LLUT(TVMLUT), CLSUBS, LBUT, NOFF, DLTYPE, DLSTOK, DLSMOO,
     *   DLBASL, DWIND(4), PNFLAG, IFL, FFL, PFCNUM, KBBASL, KEBASL,
     *   TLBL, TLST, ITEMP(2), DLSCAN, ZAND, MASK, IPL
      LOGICAL   DOIT, F, T, DONMSG, TDOBL, TDOIF, DOSTOK
      REAL      CATR(256), RPOS(2), PPOS(2), RMIN, RMAX, TVMIN, TVMAX,
     *   CLMIN, CLMAX, CLIMIT(2)
      DOUBLE PRECISION CATD(128), DCLIM(2)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SPFLG.BUF'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (CLIMIT(1), CLMIN),    (CLIMIT(2), CLMAX)
      DATA T, F /.TRUE.,.FALSE./
      DATA CHST /'YX','XY','YY','XX', 'LR','RL','LL','RR', '??',
     *   'I','Q','U ','V '/
C-----------------------------------------------------------------------
      CALL YWINDO ('READ', WINDTV, IRET)
      IF (IRET.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IRET = 0
         END IF
      NOFF = MAX (WINDTV(3)-WINDTV(1), WINDTV(4)-WINDTV(2))
      NOFF = 0.015 * NOFF + 0.5
      NOFF = MAX (2, NOFF)
      DONMSG = .FALSE.
      GR1 = 1 + NGRAY
C                                       get image header
      IX = (WINDTV(1) + WINDTV(3)) / 2
      IY = (WINDTV(2) + WINDTV(4)) / 2
      IF (XYCENT(1).GT.0) IX = XYCENT(1)
      IF (XYCENT(2).GT.0) IY = XYCENT(2)
      CALL YCREAD (1, IX, IY, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       get the clip levels
      RMIN = PIXRNG(1,5)
      RMAX = PIXRNG(2,5)
      TVMIN = CATR(IRRAN)
      TVMAX = CATR(IRRAN+1)
C                                       ask user min and max
      IF (BRANCH.EQ.1) THEN
         IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) THEN
            WRITE (MSGBUF,1010) RMIN, RMAX
            CALL INQFLT (TTY, MSGBUF, 2, DCLIM, IRET)
C                                       ask user only max on rms's
         ELSE
            DCLIM(1) = -1.0E-3
            WRITE (MSGBUF,1015) RMAX
            CALL INQFLT (TTY, MSGBUF, 1, DCLIM(2), IRET)
            END IF
         IF (IRET.NE.0) GO TO 850
         CLIMIT(1) = DCLIM(1)
         CLIMIT(2) = DCLIM(2)
C                                       TV interaction
      ELSE IF (BRANCH.EQ.2) THEN
         CLMIN = -1.0E-3
         IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) CLMIN = RMIN -
     *      0.01 * (RMAX - RMIN)
         CLMAX = RMAX + 0.01 * (RMAX - RMIN)
C                                       clear and select graphics
         IF (NGRAPH.LE.1) MENUOK = .FALSE.
         IF (.NOT.GPH1OK) THEN
            ROUTIN = 'YZERO'
            CALL YZERO (GR1, IRET)
            IF (IRET.NE.0) GO TO 800
            END IF
         MASK = 2 ** (GR1 - 1)
         MASK = ZAND (MASK, TVLIMG(1))
         IF (MASK.EQ.0) THEN
            ROUTIN = 'YSLECT'
            CALL YSLECT ('ONNN', GR1, 0, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 800
            END IF
C                                       CURVAL display location
         NPIX = 11 * CSIZTV(1)
         NEDGE = (2 * MAXXTV(1)) / 512
         IF (NEDGE.LT.2) NEDGE = 2
         NROW = 2 * (2*NEDGE + CSIZTV(2))
         MAG = 1 + TVZOOM(1)
         IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
         IX0 = WINDTV(1) - (MAG-1)/2
         IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
         IF (MAG.GT.1) IY0 = IY0 + MAG
         IX0 = (IX0 - TVZOOM(2))/MAG + TVZOOM(2)
         IY0 = (IY0 - TVZOOM(3))/MAG + TVZOOM(3)
         IX0 = MAX (1, IX0)
         IY0 = MAX (1, IY0)
         IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = MAXXTV(1) - NPIX + 1
         IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
C                                       set to top row of text
         IX0 = IX0 + NEDGE
         IY0 = IY0 + 3*NEDGE + CSIZTV(2)
C                                       get current LUTs
         ROUTIN = 'YLUT'
         CALL YLUT ('READ', 1, 1, F, ALUT(1,1), IRET)
         IF (IRET.NE.0) GO TO 800
         CALL YLUT ('READ', 1, 2, F, ALUT(1,2), IRET)
         IF (IRET.NE.0) GO TO 800
         CALL YLUT ('READ', 1, 4, F, ALUT(1,3), IRET)
         IF (IRET.NE.0) GO TO 800
C                                       cursor on
         QUAD = -1
         CLSUBS = 2
         RPOS(1) = NOFF + WINDTV(1) + (WINDTV(3) - WINDTV(1)- 2.*NOFF) *
     *      (CLIMIT(CLSUBS) - RMIN) / (RMAX - RMIN)
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         IF (RPOS(1).LT.WINDTV(1)) RPOS(1) = MAX (WINDTV(1), 2)
         IF (RPOS(1).GT.WINDTV(3)) RPOS(1) = WINDTV(3)
         CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
         ROUTIN = 'YCURSE'
         IF (IRET.NE.0) GO TO 800
         PPOS(1) = 0.0
         PPOS(2) = 0.0
         CALL ZTIME (ITW)
         IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) THEN
            MSGTXT = 'Hit button A or B to switch between upper and'
     *         // ' lower clip limits'
            CALL MSGWRT (1)
            END IF
         MSGTXT = 'Hit button C to do the clipping, then go to the menu'
         CALL MSGWRT (1)
         MSGTXT = 'Hit button D to return to the menu without clipping'
         CALL MSGWRT (1)
C                                        read until cursor moves
 30      CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
            ROUTIN = 'YCURSE'
            IF (IRET.NE.0) GO TO 800
            CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
            IF (.NOT.DOIT) GO TO 30
C                                       button D - exit now
C                                       button C do flag now
            IF (IBUT.GE.4) GO TO 50
C                                       respond to position
            CLIMIT(CLSUBS) = (RPOS(1) - WINDTV(1) - NOFF) *
     *          (RMAX - RMIN) / (WINDTV(3) - WINDTV(1) - 2.*NOFF) + RMIN
C                                       impact on LUTs
            J = MAXINT + 1
            I1 = MAXINT * (CLIMIT(1)-TVMIN) / (TVMAX-TVMIN) + 1.5
            IF (I1.LT.1) I1 = 1
            IF (I1.GT.J) I1 = J
            I2 = MAXINT * (CLIMIT(2)-TVMIN) / (TVMAX-TVMIN) + 1.5
            IF (I2.LT.1) I2 = 1
            IF (I2.GT.J) I2 = J
            I2 = I2 - I1 + 1
            CALL FILL (J, 0, LLUT)
            DO 40 I = 1,3
               ICL = 2 ** (I-1)
               CALL COPY (I2, ALUT(I1,I), LLUT(I1))
               ROUTIN = 'YLUT'
               CALL YLUT ('WRIT', 1, ICL, T, LLUT, IRET)
               IF (IRET.NE.0) GO TO 800
 40            CONTINUE
C                                       CURV-like display
            CALL YHOLD ('ONNN', JERR)
            ROUTIN = 'IMCHAR'
            IY = IY0
            WRITE (STRING,1040) CLIMIT(1)
            CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:11), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 800
            GPH1OK = .FALSE.
            IY = IY - 2*NEDGE - CSIZTV(2)
            WRITE (STRING,1040) CLIMIT(2)
            CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:11), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 800
            CALL YHOLD ('OFFF', JERR)
C                                       on button check window
            IF (IBUT.GT.0) THEN
               CALL YWINDO ('READ', WINDTV, IRET)
               IF (IRET.NE.0) THEN
                  WINDTV(1) = 1
                  WINDTV(2) = 1
                  WINDTV(3) = MAXXTV(1)
                  WINDTV(4) = MAXXTV(2)
                  IRET = 0
                  END IF
C                                       buttons A, B switch which one
               IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) CLSUBS =
     *            3 - CLSUBS
               RPOS(1) = NOFF + WINDTV(1) + (CLIMIT(CLSUBS) - RMIN) *
     *            (WINDTV(3) - WINDTV(1) - 2.*NOFF) / (RMAX - RMIN)
               RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
               IF (RPOS(1).LT.WINDTV(1)) RPOS(1) = MAX (WINDTV(1), 2)
               IF (RPOS(1).GT.WINDTV(3)) RPOS(1) = WINDTV(3)
               CALL YCURSE ('ONNN', F, F, RPOS, QUAD, LBUT, IRET)
               ROUTIN = 'YCURSE'
               IF (IRET.NE.0) GO TO 800
               PPOS(1) = 0.0
               PPOS(2) = 0.0
               END IF
            GO TO 30
C                                       clean up the TV:
C                                       put back current LUTs
 50      ROUTIN = 'YLUT'
         CALL YLUT ('WRIT', 1, 1, F, ALUT(1,1), IRET)
         IF (IRET.NE.0) GO TO 800
         CALL YLUT ('WRIT', 1, 2, F, ALUT(1,2), IRET)
         IF (IRET.NE.0) GO TO 800
         CALL YLUT ('WRIT', 1, 4, F, ALUT(1,3), IRET)
         IF (IRET.NE.0) GO TO 800
C                                       clear graphics
         IY = IY0
         STRING = ' '
         CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:11), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         IY = IY - 2*NEDGE - CSIZTV(2)
         CALL IMCHAR (GR1, IX0, IY, 0, 0, STRING(1:11), SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 800
         GPH1OK = .TRUE.
C                                       off cursor
         RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
         RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
         ROUTIN = 'YCURSE'
         CALL YCURSE ('OFFF', F, F, RPOS, QUAD, LBUT, IRET)
         IF (IRET.NE.0) GO TO 800
C                                       Button D exits no clip
         IF (IBUT.GE.8) GO TO 910
         END IF
C                                       Do a clipping:
C                                       announce the range
      IF (BRANCH.LE.2) THEN
         IF (CLMIN.GE.CLMAX) GO TO 980
         IF ((CLMIN.LE.RMIN) .AND. (CLMAX.GE.RMAX)) GO TO 980
         IF ((CLMAX.LE.RMIN) .OR. (CLMIN.GE.RMAX)) GO TO 980
         IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) THEN
            WRITE (MSGTXT,1090) CLIMIT
         ELSE
            WRITE (MSGTXT,1091) CLMAX
            END IF
         CALL MSGWRT (2)
         CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         FCNUMB = FCNUMB + 1
         CALL SPFLIP (IMGWIN, CLIMIT, FCBUF, SCRTCH, BIGBOY(BIGPT(3)),
     *      BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)), BIGBOY(BIGPT(6)), IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
         GO TO 999
         END IF
C                                       based on old
      IF (BRANCH.EQ.3) THEN
C                                       inquire which flag
         CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         FFL = 1
         IF (FCNUMB.GT.1) THEN
            WRITE (MSGBUF,1100) FCNUMB
            CALL INQINT (TTY, MSGBUF, 1, FFL, IRET)
            IF (IRET.NE.0) GO TO 850
            IF ((FFL.LT.1) .OR. (FFL.GT.FCNUMB)) THEN
               IRET = -1
               WRITE (MSGTXT,1105) FFL, FCNUMB
               CALL MSGWRT (6)
               GO TO 999
               END IF
            END IF
C                                       save load parms
         DLTYPE = LTYPE
         DLSTOK = LSTOKS
         DLSMOO = LSMOO
         DLSCAN = LSCAN
         DLBASL = LBASL
         CALL COPY (4, IMGWIN, DWIND)
         DSTKFG = STKFLG
C                                       open FC table
         CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         PNFLAG = NNFLAG
         PFCNUM = FCNUMB
         DO 110 IFL = 1,PNFLAG
            CALL TABIO ('READ', 0, IFL, FCTIME, FCBUF, IRET)
            IF (IRET.GT.0) GO TO 951
            IF (IRET.LT.0) GO TO 110
            IF (FCNUMB.EQ.FFL) GO TO 120
 110        CONTINUE
         IRET = -1
         WRITE (MSGTXT,1110) FFL
         GO TO 950
C                                       found requested command
 120     CALL H2CHR (8, 1, FCOPER, OP)
         IF (OP.NE.'CLIP ') THEN
            CALL SPFCLI (1)
            IRET = -1
            MSGTXT = 'ABOVE FLAG COMMAND IS NOT A CLIP COMMAND:'
     *         // ' USE ''LIST FLAGS'''
            GO TO 950
            END IF
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         MSGTXT = 'Pattern flag command is:'
         CALL MSGWRT (2)
         CALL SPFCLI (2)
C                                       save pattern parms
         LTYPE = FCTVTY
         LSMOO = FCTVAV / CATIR(KRCIC+1) + 0.1
         LSCAN = FCTVSC / CATIR(KRCIC+1) + 0.1
         CALL COPY (4, FCTVWI, IMGWIN)
         TLST = (FCTVST - CATID(KDCRV+2)) / CATIR(KRCIC+2) +
     *      CATIR(KRCRP+2) + 0.1
         TLBL = FCTVBL
         TDOBL = FCBASL(1).EQ.0
         TDOIF = FCIF(1).EQ.0
         CALL H2CHR (4, 1, FCSFLG, TTKFLG)
         CLIMIT(1) = FCLIPR(1)
         CLIMIT(2) = FCLIPR(2)
         IF ((TDOBL) .OR. (CATIMG(KINAX+3).LE.1)) THEN
            KBBASL = 1
            KEBASL = CATIMG(KINAX+3)
         ELSE
            WRITE (MSGBUF,1132) CATIMG(KINAX+3)
            CALL INQINT (TTY, MSGBUF, 2, ITEMP, IRET)
            IF (IRET.NE.0) GO TO 850
            KBBASL = MAX (1, MIN (ITEMP(1), CATIMG(KINAX+3)))
            IF (ITEMP(2).LT.KBBASL) ITEMP(2) = CATIMG(KINAX+3)
            KEBASL = MAX (1, MIN (ITEMP(2), CATIMG(KINAX+3)))
            END IF
C                                       ask about Stokes maybe
         STKFS(1) = TTKFLG
         STKFS(2) = TTKFLG
         STKFS(3) = TTKFLG
         STKFS(4) = TTKFLG
         NSTOKS = CATIMG(KINAX+2)
         IF (NSTOKS.GT.1) THEN
            DO 140 I = 1,NSTOKS
               IF (.NOT.DOSTOK(ILSTOK, TTKFLG, I)) THEN
                  J = ILSTOK(I) + 9
                  WRITE (MSGBUF,1135) CHST(J)
 139              CALL INQSTR (TTY, MSGBUF, 4, TTKFLG, IRET)
                  IF (IRET.EQ.10) THEN
                     MSGTXT = 'STRING TOO LONG, TRY AGAIN'
                     CALL MSGWRT (7)
                     GO TO 139
                     END IF
                  IF (IRET.NE.0) GO TO 850
                  CALL CHLTOU (4, TTKFLG)
                  CALL MKSTOK (STRANS, ILSTOK, TTKFLG, STKFS(I), IRET)
                  IF (IRET.NE.0) THEN
                     STKFS(I) = ' '
                     IRET = 0
                  ELSE
                     IF (.NOT.DOSTOK (ILSTOK, STKFS(I), I))
     *                  STKFS(I) = ' '
                     END IF
                  END IF
 140           CONTINUE
            END IF
         DO 165 LBASL = KBBASL,KEBASL
            DO 160 LSTOKS = 1,NSTOKS
               IF ((LBASL.EQ.TLBL) .AND. (LSTOKS.EQ.TLST)) GO TO 160
               IF (STKFS(LSTOKS).EQ.' ') GO TO 160
               J = ILSTOK(LSTOKS) + 9
               WRITE (MSGTXT,1150) CHST(J), LBASL
               CALL MSGWRT (2)
C                                       load TV
               IPL = 1
               CALL SPFOAD (IPL, IMGWIN, DIMB2, ABUF, IOBUF, SCRTCH,
     *            BIGBOY(1), BIGBOY(BIGPT(2)), KIGBOY(BIGPT(3)),
     *            BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)), BIGBOY(BIGPT(6)),
     *            IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1152) IRET
                  GO TO 950
               ELSE IF (IRET.LT.0) THEN
                  GO TO 160
                  END IF
               PLTYPE = LTYPE
               PLSTOK = LSTOKS
               PLSMOO = LSMOO
               PLSCAN = LSCAN
               PLBASL = LBASL
               LBASLV = NOANTS(3,LBASL)
C                                       open table
               CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG,
     *            FCNUMB, NNFLAG, FCBUF, IRET)
               IF (IRET.NE.0) GO TO 999
               FCNUMB = FCNUMB + 1
C                                       record in FC table
               FCTVTY = LTYPE
               FCTVBL = LBASL
               FCTVIF = 0
               FCTVST = ILSTOK(LSTOKS)
               CALL COPY (4, IMGWIN, FCTVWI)
               FCTVAV = LSMOO * CATIR(KRCIC+1)
               FCTVSC = LSCAN * CATIR(KRCIC+1)
               IF (TDOBL) THEN
                  FCBASL(1) = 0
                  FCBASL(2) = 0
               ELSE
                  FCBASL(1) = LBASLV
                  FCBASL(2) = LBASLV
                  END IF
               FCIF(1) = 0
               FCIF(2) = 0
               CALL CHR2H (4, STKFS(LSTOKS), 1, FCSFLG)
C                                       do flagging
               IF ((PLTYPE.LT.3) .OR. (PLTYPE.GT.6)) THEN
                  WRITE (MSGTXT,1090) CLIMIT
               ELSE
                  WRITE (MSGTXT,1091) CLMAX
                  END IF
               CALL MSGWRT (2)
               CALL SPFLIP (IMGWIN, CLIMIT, FCBUF, SCRTCH,
     *            BIGBOY(BIGPT(3)), BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)),
     *            BIGBOY(BIGPT(6)), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1154) IRET
                  GO TO 950
                  END IF
C                                       close table each time to save
               CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
               IF (IRET.NE.0) GO TO 999
 160           CONTINUE
 165        CONTINUE
C                                       recover load parms
         LTYPE = DLTYPE
         LSTOKS = DLSTOK
         LSMOO = DLSMOO
         LSCAN = DLSCAN
         LBASL = DLBASL
         CALL COPY (4, DWIND, IMGWIN)
         STKFLG = DSTKFG
         END IF
      GO TO 999
C                                       TV error
 800  WRITE (MSGTXT,1800) IRET, ROUTIN
      GO TO 900
C                                       TTY error
 850  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1850) IRET
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'FORMAT ERROR: RETURN TO MENU'
         END IF
      CALL MSGWRT (8)
      GO TO 999
C                                       error message first
 900  CALL MSGWRT (8)
C                                       off cursor
 910  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, LBUT, JERR)
      CALL YHOLD ('OFFF', JERR)
      GO TO 999
C                                       close down table
 950  CALL MSGWRT (8)
 951  CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, JERR)
      GO TO 999
C                                       bad range
 980  WRITE (MSGTXT,1980) CLIMIT
C                                       error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFCLP: UNABLE TO READ IMAGE HEADER, ERROR',I5)
 1010 FORMAT ('Enter clip lower/upper limits in range',2(1PE12.4),
     *   '  Q quits')
 1015 FORMAT ('Enter clip upper limit (1 real) in range 0.0 to',
     *   1PE12.4,'  Q quits')
 1040 FORMAT (1PE11.3)
 1090 FORMAT ('Begin clipping outside range',2(1PE12.4))
 1091 FORMAT ('Begin clipping above',1PE12.4)
 1100 FORMAT ('Enter clip command number (1 integer) <=',I5)
 1105 FORMAT ('CLIP COMMAND NUMBER',I5,' NOT IN RANGE 1 -',I5,
     *   ' RETURN TO MENU')
 1110 FORMAT ('CLIP COMMAND NUMBER',I5,' NOT FOUND: USE ''LIST FLAGS''')
 1132 FORMAT ('Enter range of baseline numbers within 1 through',I5,
     *   ' (2 I''s)')
 1135 FORMAT ('Enter Stokes flag pattern applied to Stokes ',A2,
     *   ' (4 chars left justified)')
 1150 FORMAT ('Begin work on Stokes ',A2,' baseline',I5)
 1152 FORMAT ('SPFCLP: ERROR',I5,' RETURNED BY SPFOAD')
 1154 FORMAT ('SPFCLP: ERROR',I5,' RETURNED BY SPFLIP')
 1800 FORMAT ('TV ERROR ',I6,' IN ',A)
 1850 FORMAT ('TERMINAL ERROR',I5)
 1980 FORMAT ('CLIP RANGE',2(1PE12.4),' NOT GOOD, EXIT CLIP')
      END
      SUBROUTINE SPFLIP (IMGWIN, CLIMIT, LFCBUF, SCRTCH, BUFF0, BUFF1,
     *   BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C   does flagging of all pixels in the TV image outside a user-set range
C   of intensities.  That range is set in SPFCLP.
C   Inputs:
C      IMGWIN   I(4)   currently loaded image window
C      CLIMIT   R(2)   clip limits to set
C   In/out:
C      LFCBUF   I(512) Open FC table to write
C   Output:
C      SCRTCH   I(*)   TV scratch buffer
C      BUFF0    R(*)   IO buffer
C      BUFF1    R(*)   IO buffer
C      BUFF2    R(*)   IO buffer
C      BUFF3    R(*)   IO buffer
C      IRET     I      error code
C   Requires CATBLK = TV image in /MAPHDR/
C-----------------------------------------------------------------------
      INTEGER   IMGWIN(4), LFCBUF(512), SCRTCH(*), IRET
      REAL      CLIMIT(2), BUFF0(*), BUFF1(*), BUFF2(*), BUFF3(*)
C
      INCLUDE 'INCS:DSEL.INC'
      CHARACTER PHNAME*48, OPERS*8, ROUTIN*6
      HOLLERITH CATH(256)
      INTEGER   IB0, I, LUN1, LUN2, FIND1, FIND2, J, IR, JERR, IXP, IYP,
     *   LUN0, FIND0, IROUND, I1, I2, LUN3, FIND3, IYL, IYH, IB1, IB2,
     *   IB3, IBLKOF, IDEPTH(5), KBBASL, KEBASL, KBASL, KBIF, KEIF, KIF,
     *   IWIN(4), NROWS, NX, NY, ILSMOO, IYB1(MAXCIF), IERR, NCHAN,
     *   IYB2(MAXCIF), KST, KBST, KEST, NCC, IYSU(MAXCIF), LSU, ICH,
     *   IIF
      LOGICAL   T, F, ISLAST, SSLAST, DOTV, DOSTOK
      REAL      CATR(256), RPOS(2), CORN(2), TIM1, CLMIN, CLMAX, TIM2
      DOUBLE PRECISION CATD(128)
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN0, LUN1, LUN2, LUN3 /16,17,18,19/
      DATA OPERS /'CLIP '/
C-----------------------------------------------------------------------
      CALL CHR2H (8, OPERS, 1, FCOPER)
      CLMIN = CLIMIT(1)
      CLMAX = CLIMIT(2)
      NCHAN = ECHAN - BCHAN + 1
      I = MAXCIF
      CALL FILL (I, -1, IYB1)
      CALL FILL (I, -1, IYB2)
      FIND0 = 0
      FIND1 = 0
      FIND2 = 0
      FIND3 = 0
C                                       Do the clipping:
      FCLIPR(1) = CLMIN
      FCLIPR(2) = CLMAX
C                                       open TV image SC file
      CALL ZPHFIL ('SC', SCRVOL(TVFILE), SCRCNO(TVFILE), 1, PHNAME,
     *   IRET)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(TVFILE), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'OPEN (R)', IRET
         GO TO 990
         END IF
      CALL ZOPEN (LUN3, FIND3, SCRVOL(TVFILE), PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'OPEN (W)', IRET
         GO TO 990
         END IF
C                                       open master file
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
      CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) 'OPEN (W)', IRET
         GO TO 900
         END IF
      CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) 'OPEN (R)', IRET
         GO TO 900
         END IF
      IF ((DOBASL.EQ.1) .OR. (DOBASL.EQ.2)) THEN
         KEBASL = 0
         KBBASL = CATIMG(KINAX+3)+1
         DO 120 KBASL = 1,CATIMG(KINAX+3)
            KST = NOANTS(DOBASL,PLBASL)
            IF ((KST.EQ.NOANTS(1,KBASL)) .OR.
     *         (KST.EQ.NOANTS(2,KBASL))) THEN
               KBBASL = MIN (KBBASL, KBASL)
               KEBASL = MAX (KEBASL, KBASL)
               END IF
 120        CONTINUE
         IF (KEBASL.EQ.0) THEN
            MSGTXT = 'SOMETHING WRONG IN SELECTING BASELINES'
            GO TO 900
            END IF
      ELSE IF (DOBASL.GT.0) THEN
         KBBASL = 1
         KEBASL = CATIMG(KINAX+3)
      ELSE
         KBBASL = PLBASL
         KEBASL = PLBASL
         END IF
      DOTV = DOSTOK (ILSTOK, STKFLG, PLSTOK)
      IF (.NOT.DOTV) THEN
         MSGTXT = 'WARNING: flag command does not apply to displayed'
     *      // ' Stokes'
         CALL MSGWRT (6)
         END IF
      CALL DOSTKS (ILSTOK, STKFLG, KBST, KEST)
      NCC = 0
      CALL YHOLD ('ONNN', JERR)
      DO 305 KBASL = KBBASL,KEBASL
         IF ((DOBASL.EQ.1) .OR. (DOBASL.EQ.2)) THEN
            KST = NOANTS(DOBASL,PLBASL)
            IF ((KST.NE.NOANTS(1,KBASL)) .AND.
     *         (KST.NE.NOANTS(2,KBASL))) GO TO 305
            END IF
      DO 300 KST = KBST,KEST
         ISLAST = (KBASL.EQ.KEBASL) .AND. (KST.EQ.KEST) .AND. (DOTV)
         IF (.NOT.DOSTOK (ILSTOK, STKFLG, KST)) GO TO 300
C                                       init for read/write TV SC file
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = CATBLK(KINAX)
         IWIN(4) = CATBLK(KINAX+1)
         CALL MINIT ('READ', LUN2, FIND2, CATBLK(KINAX),
     *      CATBLK(KINAX+1), IWIN, BUFF2, JBUFSZ, 1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT (R)', IRET
            GO TO 900
            END IF
         IF (ISLAST) THEN
            CALL MINIT ('WRIT', LUN3, FIND3, CATBLK(KINAX),
     *         CATBLK(KINAX+1), IWIN, BUFF3, JBUFSZ, 1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'INIT (R)', IRET
               GO TO 900
               END IF
            END IF
C                                       open master file
         IWIN(3) = CATIMG(KINAX)
         IWIN(4) = CATIMG(KINAX+1)
         NROWS = 0
         IDEPTH(1) = KST
         IDEPTH(2) = KBASL
         IDEPTH(3) = 1
         IDEPTH(4) = 1
         IDEPTH(5) = 1
         CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
         CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *      CATIMG(KINAX+1), IWIN, BUFF0, JBUFSZ, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) 'INIT (R)', IRET
            GO TO 900
            END IF
         CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *      CATIMG(KINAX+1), IWIN, BUFF1, JBUFSZ, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) 'INIT (W)', IRET
            GO TO 900
            END IF
C                                       skip through lower border of
C                                       master file
         NY = IMGWIN(2) - 1
         IF (NY.LE.0) GO TO 170
            DO 160 J = 1,NY
               NROWS = NROWS + 1
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'READ', IRET
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BUFF1, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'WRITE', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BUFF1(IB1))
 160           CONTINUE
C                                       read TV scratch file
 170     NY = CATBLK(KINAX+1)
         NX = CATBLK(KINAX) - 3
         DO 210 J = 1,NY
            CALL MDISK ('READ', LUN2, FIND2, BUFF2, IB2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'READ', IRET
               GO TO 900
               END IF
            IF (ISLAST) THEN
               CALL MDISK ('WRIT', LUN3, FIND3, BUFF3, IB3, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRITE', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATBLK(KINAX), BUFF2(IB2), BUFF3(IB3))
               END IF
            TIM1 = BUFF2(IB2+1)
            TIM2 = BUFF2(IB2+2)
            CALL GETIME (TIM1, TIM2, IYL, IYH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1180) TIM1, TIM2, NROWS
               CALL MSGWRT (8)
               GO TO 210
               END IF
            IF ((IYL.NE.NROWS) .AND. (IYL.NE.NROWS+1)) THEN
               WRITE (MSGTXT,1185) NROWS, IYL, TIM1, TIM2
               CALL MSGWRT (6)
               END IF
            IYL = NROWS + 1
            ILSMOO = IYH - IYL + 1
            IF (NROWS+ILSMOO.GT.CATIMG(KINAX+1)) ILSMOO =
     *         CATIMG(KINAX+1) - NROWS
            LSU = 0
            DO 205 IR = 1,ILSMOO
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               NROWS = NROWS + 1
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'READ', IRET
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BUFF1, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'WRITE', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BUFF1(IB1))
               IF (DOSOUR) THEN
                  LSU = BUFF1(IB1) + 0.01
                  IF (LSU.LE.0) LSU = INSNUM
                  END IF
               SSLAST = (ISLAST) .AND. (IR.EQ.ILSMOO)
               DO 200 I = 1,NX
                  I1 = IB2 + I + 2
                  ICH = MOD (I-1, NCHAN)
                  IIF = (I-1) / NCHAN
                  IF (BUFF2(I1).EQ.FBLANK) GO TO 200
C                                       point inside range now
                  IF ((BUFF2(I1).GE.CLMIN) .AND. (BUFF2(I1).LE.CLMAX))
     *               THEN
C                                       but previously bad
C                                       (only true if SSLAST)
                     IF (IYB1(I).GT.0) THEN
C                                       add to flag table
                        NNFLAG = NNFLAG + 1
                        FCCHAN(1) = ICH + IMGWIN(1) + BCHAN - 1
                        FCCHAN(2) = FCCHAN(1)
                        IF (DOIFS.EQ.-1) THEN
                           FCIF(1) = IIF + BIF
                           FCIF(2) = IIF + BIF
                        ELSE IF (DOIFS.EQ.0) THEN
                           FCIF(1) = LCIF(1)
                           FCIF(2) = LCIF(2)
                           END IF
                        FCTIME(1) = TIMES(IYB1(I))
                        FCTIME(2) = TIMES(IYB2(I)+1)
                        FCSOUR = MAX (IYSU(I), 0)
                        IYB1(I) = -1
                        IYB2(I) = -1
                        CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, LFCBUF,
     *                     IRET)
                        IF (IRET.NE.0) GO TO 999
                        END IF
C                                       point outside range now
                  ELSE
                     IF (DOIFS.EQ.1) THEN
                        KBIF = 1
                        KEIF = EIF - BIF + 1
                     ELSE IF (DOIFS.EQ.0) THEN
                        KBIF = LCIF(1) - BIF + 1
                        KEIF = LCIF(2) - BIF + 1
                     ELSE
                        KBIF = IIF + 1
                        KEIF = KBIF
                        END IF
                     DO 185 KIF = KBIF,KEIF
                        I2 = ICH + IMGWIN(1) + NCHAN * (KIF-1)
                        I2 = IB1 + 3 * I2 + 2
                        IF (BUFF1(I2).EQ.0.0) THEN
                           BUFF1(I2) = FCNUMB
                           NCC = NCC + 1
                           END IF
 185                    CONTINUE
C                                       clear TV file
                     IF (SSLAST) THEN
                        DO 190 KIF = KBIF,KEIF
                           I1 = IB3 + ICH + 3 + NCHAN * (KIF-1)
                           BUFF3(I1) = FBLANK
C                                       clear TV
                           CORN(1) = ICH + 4 + NCHAN * (KIF-1)
                           CORN(2) = J
                           CALL MP2IMA (CORN, RPOS)
                           IXP = IROUND (RPOS(1))
                           IYP = IROUND (RPOS(2))
                           IF ((IXP.GE.1) .AND. (IXP.LE.MAXXTV(1)) .AND.
     *                        (IYP.GE.1) .AND. (IYP.LE.MAXXTV(2))) THEN
                              ROUTIN = 'YFILL'
                              CALL YFILL (1, IXP, IYP, IXP, IYP, 0,
     *                           SCRTCH, IRET)
                              IF (IRET.NE.0) GO TO 800
                              END IF
 190                       CONTINUE
C                                       source changed
                        IF ((IYB1(I).GT.0) .AND. (LSU.NE.IYSU(I))) THEN
C                                       add to flag table
                           NNFLAG = NNFLAG + 1
                           FCCHAN(1) = ICH + IMGWIN(1) + BCHAN - 1
                           FCCHAN(2) = FCCHAN(1)
                           IF (DOIFS.EQ.-1) THEN
                              FCIF(1) = IIF + BIF
                              FCIF(2) = IIF + BIF
                           ELSE IF (DOIFS.EQ.0) THEN
                              FCIF(1) = LCIF(1)
                              FCIF(2) = LCIF(2)
                              END IF
                           FCTIME(1) = TIMES(IYB1(I))
                           FCTIME(2) = TIMES(IYB2(I)+1)
                           FCSOUR = MAX (0, IYSU(I))
                           IYB1(I) = -1
                           CALL TABIO ('WRIT', 0, NNFLAG, FCTIME,
     *                        LFCBUF, IRET)
                           IF (IRET.NE.0) GO TO 999
                           END IF
                        IYB2(I) = J
                        IF (IYB1(I).LE.0) IYB1(I) = J
                        IYSU(I) = LSU
                        END IF
                     END IF
 200              CONTINUE
 205           CONTINUE
 210        CONTINUE
C                                       copy last of UV master image
         NROWS = CATIMG(KINAX+1) - NROWS
         IF (NROWS.LE.0) GO TO 230
            DO 220 J = 1,NROWS
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'READ', IRET
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BUFF1, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1110) 'WRITE', IRET
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BUFF1(IB1))
 220           CONTINUE
C                                       finish the writing
 230     IF (.NOT.ISLAST) GO TO 295
            CALL MDISK ('FINI', LUN3, FIND3, BUFF3, IB3, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'FINI', IRET
               GO TO 900
               END IF
C                                       any entries for flagtable?
            DO 240 I = 1,NX
               IF (IYB1(I).GT.0) THEN
                  ICH = MOD (I-1, NCHAN)
                  IIF = (I-1) / NCHAN
                  NNFLAG = NNFLAG + 1
                  FCCHAN(1) = ICH + IMGWIN(1) + BCHAN - 1
                  FCCHAN(2) = FCCHAN(1)
                  IF (DOIFS.EQ.-1) THEN
                     FCIF(1) = IIF + BIF
                     FCIF(2) = FCIF(1)
                  ELSE IF (DOIFS.EQ.0) THEN
                     FCIF(1) = LCIF(1)
                     FCIF(2) = LCIF(2)
                     END IF
                  FCTIME(1) = TIMES(IYB1(I))
                  FCTIME(2) = TIMES(IYB2(I)+1)
                  FCSOUR = MAX (0, IYSU(I))
                  CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, LFCBUF, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 240           CONTINUE
 295     CALL MDISK ('FINI', LUN1, FIND1, BUFF1, IB1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1110) 'FINI', IRET
            GO TO 900
            END IF
 300     CONTINUE
 305     CONTINUE
      IF (NCC.GT.0) THEN
         WRITE (MSGTXT,1300) NCC
         CALL MSGWRT (3)
      ELSE
         MSGTXT = 'NO PREVIOUSLY UNFLAGGED PIXELS DELETED IN'
     *      // ' MASTER GRID'
         CALL MSGWRT (6)
         END IF
      GO TO 910
C                                       TV error
 800  WRITE (MSGTXT,1800) IRET, ROUTIN
      GO TO 900
C                                       error message first
 900  CALL MSGWRT (8)
C                                       off cursor
 910  IF (FIND0.GT.0) CALL ZCLOSE (LUN0, FIND0, JERR)
      IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, JERR)
      IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, JERR)
      IF (FIND3.GT.0) CALL ZCLOSE (LUN3, FIND3, JERR)
      CALL YHOLD ('OFFF', JERR)
      GO TO 999
C                                       error message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('SPFLIP: UNABLE TO ',A,' TV SCRATCH FILE, ERROR',I5)
 1110 FORMAT ('SPFLIP: UNABLE TO ',A,' MAIN GRID FILE, ERROR',I5)
 1180 FORMAT ('SPFLIP: GETIME ERROR: T1,T2,NROWS=',2(1PE12.4),I6)
 1185 FORMAT ('SPFLIP UNEXPECTED NROWS, IYL, T1, T2=',2I6,2(1PE12.4))
 1300 FORMAT ('Deleted',I9,' pixels in master grid')
 1800 FORMAT ('TV ERROR ',I6,' IN ',A)
      END
      SUBROUTINE SPFUNF (BRANCH, TTY, IMGWIN, BUFF0, BIGBOY, KIGBOY,
     *   SCRTCH, IRET)
C-----------------------------------------------------------------------
C   does unflag-related operations on the master grid.
C   Inputs:
C      BRANCH   I      =9  => list flag commands
C                      =10 => undo flag commands
C                      =11 => re-flag master grid based on current FC
C                             file
C      TTY      I(2)   LUN, IND of open terminal to talk to user
C      IMGWIN   I(4)   Currently loaded image window
C   Output:
C      BUFF0    R(*)   IO buffer (overlap end of BIGBOY okay)
C      BIGBOY   R(*)   Large IO buffers
C      KIGBOY   I(*)   Large IO buffers
C      SCRTCH   I(*)   scratch buffer for TV
C      IRET     I      Error return: > 0 => quit
C                         -1 => nothing really done
C-----------------------------------------------------------------------
      REAL      BUFF0(*), BIGBOY(*)
      INTEGER   BRANCH, TTY(2), IMGWIN(4), KIGBOY(*), SCRTCH(*), IRET
C
      INTEGER   NCC, I, J, IERR, J1, FCNLIM(2), FCNCNT, FCNFLG(50),
     *   LUN0, LUN1, FIND0, NX, FIND1, EST, KIF, KBASL, KST, IWIN(4),
     *   IDEPTH(5), IBLKOF, NY, IB0, IB1, I1, I2, IROUND, K, NCF, J2,
     *   NVER, FCNWAS, IFL1, PNFLAG, DLTYPE, DLSTOK, DLSMOO, DLBASL,
     *   DWIND(4), FCBUF2(512), IXL, IXH, IYL, IYH, LUNFC2, IY, NRPL,
     *   KBBASL, KEBASL, KBIF, KEIF, NCFLAG, DLSCAN, IOFF, NCHAN, IPL
      REAL      CLIMIT(2)
      LOGICAL   T, F, DOSTOK
      CHARACTER OP*8, MSGBUF*72, PHNAME*48, DSTKFG*4
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'SPFLG.BUF'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN0, LUN1, LUNFC2 /16, 17, 26/
C-----------------------------------------------------------------------
      IRET = 0
      IF ((BRANCH.LT.9) .OR. (BRANCH.GT.11)) GO TO 999
      NCHAN = ECHAN - BCHAN + 1
C                                       open the FC file
      CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      NCFLAG = FCNUMB + 1
      IF ((FCNUMB.LE.0) .OR. (NNFLAG.LE.0)) THEN
         MSGTXT = 'No entries in the FC table to LIST, UNDO, or REDO'
         CALL MSGWRT (6)
         IRET = -1
         GO TO 900
         END IF
C                                       list
 100  IF (BRANCH.EQ.9) THEN
         IF (FCNUMB.LE.1) THEN
            FCNLIM(1) = 1
            FCNLIM(2) = 1
         ELSE
            WRITE (MSGBUF,1100) FCNUMB
            CALL INQINT (TTY, MSGBUF, 2, FCNLIM, IRET)
            IF (IRET.NE.0) GO TO 850
            FCNLIM(1) = MAX (1, MIN (FCNLIM(1), FCNUMB))
            IF (FCNLIM(2).LT.FCNLIM(1)) FCNLIM(2) = FCNUMB
            FCNLIM(2) = MAX (1, MIN (FCNLIM(2), FCNUMB))
            END IF
         NCF = 0
         NCC = 0
         DO 150 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            IF (IRET.LT.0) GO TO 150
            IF (IRET.NE.0) GO TO 900
            IF ((FCNUMB.LT.FCNLIM(1)) .OR. (FCNUMB.GT.FCNLIM(2)))
     *         GO TO 150
            IF (FCNUMB.EQ.NCF) THEN
               NCC = NCC + 1
            ELSE
C                                       give number flags
               IF (NCC.GT.1) THEN
                  WRITE (MSGTXT,1101) NCC
                  CALL MSGWRT (3)
                  END IF
C                                       new flag kind
               NCC = 1
               NCF = FCNUMB
               CALL SPFCLI (3)
               END IF
 150        CONTINUE
C                                       give number flags
         IF (NCC.GT.1) THEN
            WRITE (MSGTXT,1101) NCC
            CALL MSGWRT (3)
            END IF
C                                       UNDO 1 or more flags
      ELSE IF (BRANCH.EQ.10) THEN
         FCNLIM(1) = FCNUMB
         FCNLIM(2) = 1
         IF (FCNUMB.LE.1) THEN
            I = 2
            FCNFLG(1) = 1
         ELSE
            DO 210 I = 1,50
               WRITE (MSGBUF,1200) I, FCNUMB
 205           CALL INQINT (TTY, MSGBUF, 1, FCNFLG(I), IRET)
               IF (IRET.NE.0) GO TO 850
               IF (FCNFLG(I).GT.FCNUMB) GO TO 205
               IF (FCNFLG(I).LE.0) GO TO 215
               FCNLIM(1) = MIN (FCNLIM(1), FCNFLG(I))
               FCNLIM(2) = MAX (FCNLIM(2), FCNFLG(I))
 210           CONTINUE
            I = 51
            END IF
 215     FCNCNT = I - 1
         IF (FCNCNT.LE.0) GO TO 900
C                                       flag them in FC file
         NCC = 0
         I1 = 0
         J1 = 0
         NCF = 0
         DO 230 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            IF (IRET.LT.0) GO TO 230
            IF (IRET.NE.0) GO TO 900
            I2 = I1
            I1 = I
            J2 = J1
            J1 = FCNUMB
            IF ((FCNUMB.LT.FCNLIM(1)) .OR. (FCNUMB.GT.FCNLIM(2)))
     *         GO TO 230
               IF (FCNLIM(1).EQ.FCNLIM(2)) GO TO 225
                  DO 220 J = 1,FCNCNT
                     IF (FCNUMB.EQ.FCNFLG(J)) GO TO 225
 220                 CONTINUE
                  GO TO 230
C                                       flag the line
 225           NCC = NCC + 1
               CALL TABIO ('FLAG', 0, I, FCTIME, FCBUF, IRET)
               IF (IRET.NE.0) GO TO 900
               I1 = I2
               J1 = J2
               IF (NCF.NE.FCNUMB) THEN
                  NCF = FCNUMB
                  MSGTXT = '******** Undoing :'
                  CALL MSGWRT (2)
                  CALL SPFCLI (2)
                  END IF
 230        CONTINUE
         IF (NCC.LE.0) THEN
            MSGTXT = 'NO LINES DELETED IN THE FC FILE, RETURN TO MENU'
            CALL MSGWRT (6)
            IRET = -1
            GO TO 900
         ELSE
            WRITE (MSGTXT,1231) NCC
            CALL MSGWRT (2)
            END IF
C                                       reduce number of records
         IF (I1.LT.NNFLAG) THEN
            I2 = NNFLAG - I1
            WRITE (MSGTXT,1232) I2, I1
            CALL MSGWRT (3)
            FCBUF(5) = I1
            NNFLAG = I1
            J2 = NCFLAG
            NCFLAG = J1 + 1
            WRITE (MSGTXT,1233) J2, NCFLAG
            CALL MSGWRT (3)
            END IF
C                                       open master file
         MSGTXT = 'WARNING: checking whole master file takes a while'
         CALL MSGWRT (1)
         CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
         CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1250) IRET, 'OPEN W'
            GO TO 900
            END IF
         CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1250) IRET, 'OPEN R'
            GO TO 900
            END IF
         NCC = 0
         EST = CATIMG(KINAX+2)
         KEBASL = CATIMG(KINAX+3)
         DO 290 KBASL = 1,KEBASL
         DO 290 KST = 1,EST
C                                       init master file
            IWIN(1) = 1
            IWIN(2) = 1
            IWIN(3) = CATIMG(KINAX)
            IWIN(4) = CATIMG(KINAX+1)
            IDEPTH(1) = KST
            IDEPTH(2) = KBASL
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF0, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'INIT R'
               GO TO 900
               END IF
            CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BIGBOY, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'INIT W'
               GO TO 900
               END IF
            NY = CATIMG(KINAX+1)
            NX = (CATIMG(KINAX) - 3) / 3
            DO 250 J = 1,NY
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1250) IRET, 'READ'
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BIGBOY, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1250) IRET, 'WRITE'
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BIGBOY(IB1))
               DO 245 I = 1,NX
                  I1 = IB1 + 3*I + 2
                  IF (BIGBOY(I1).EQ.FBLANK) GO TO 245
                  I2 = IROUND (BIGBOY(I1))
                  IF ((I2.LT.FCNLIM(1)) .OR. (I2.GT.FCNLIM(2)))
     *               GO TO 245
                     IF (FCNLIM(1).EQ.FCNLIM(2)) GO TO 240
                        DO 235 K = 1,FCNCNT
                           IF (I2.EQ.FCNFLG(K)) GO TO 240
 235                       CONTINUE
                        GO TO 245
 240                 NCC = NCC + 1
                     BIGBOY(I1) = 0.0
 245              CONTINUE
 250           CONTINUE
C                                       finish the writing
            CALL MDISK ('FINI', LUN1, FIND1, BIGBOY, IB1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'FINI'
               GO TO 900
               END IF
 290        CONTINUE
         IF (NCC.LE.0) THEN
            MSGTXT = 'NO PIXELS RESTORED IN THE MASTER GRID FILE,'
     *         // ' RETURN TO MENU'
            CALL MSGWRT (6)
            IRET = -1
         ELSE
            WRITE (MSGTXT,1290) NCC
            CALL MSGWRT (2)
            END IF
         CALL ZCLOSE (LUN0, FIND0, IERR)
         CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       Redo the remaining flags
      ELSE IF (BRANCH.EQ.11) THEN
C                                       find first flag in FC file
         NCF = 10000000
         DO 305 I = 1,NNFLAG
            CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
            IF (IRET.GT.0) GO TO 900
            IF (IRET.LT.0) THEN
               NCF = FCNUMB
               GO TO 310
               END IF
 305        CONTINUE
         MSGTXT = 'All flag commands seem to be redone already'
         CALL MSGWRT (6)
         IRET = -1
         GO TO 900
C                                       there is some to do
 310     WRITE (MSGTXT,1310) NCF
         CALL MSGWRT (1)
C                                       open master file
         CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
         CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1250) IRET, 'OPEN W'
            GO TO 900
            END IF
         CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1250) IRET, 'OPEN R'
            GO TO 900
            END IF
C                                       clear master file high flags
         NCC = 0
         EST = CATIMG(KINAX+2)
         KEBASL = CATIMG(KINAX+3)
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = CATIMG(KINAX)
         IWIN(4) = CATIMG(KINAX+1)
         DO 325 KBASL = 1,KEBASL
         DO 325 KST = 1,EST
            IDEPTH(1) = KST
            IDEPTH(2) = KBASL
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF0, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'INIT R'
               GO TO 900
               END IF
            CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BIGBOY, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'INIT W'
               GO TO 900
               END IF
            DO 320 J = 1,NY
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1250) IRET, 'READ'
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BIGBOY, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1250) IRET, 'WRITE'
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BIGBOY(IB1))
               DO 315 I = 1,NX
                  I1 = IB1 + 3*I + 2
                  IF (BIGBOY(I1).NE.FBLANK) THEN
                     I2 = IROUND (BIGBOY(I1))
                     IF (I2.GE.NCF) THEN
                        NCC = NCC + 1
                        BIGBOY(I1) = 0.0
                        END IF
                     END IF
 315              CONTINUE
 320           CONTINUE
C                                       finish the writing
            CALL MDISK ('FINI', LUN1, FIND1, BIGBOY, IB1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, 'FINI'
               GO TO 900
               END IF
 325        CONTINUE
         IF (NCC.LE.0) THEN
            MSGTXT = 'NO PIXELS WERE FLAGGED AFTER THE LOWEST'
     *         // ' UNDONE COMMAND'
            CALL MSGWRT (6)
            IRET = -1
            GO TO 900
         ELSE
            WRITE (MSGTXT,1326) NCC
            CALL MSGWRT (2)
            END IF
         MSGTXT = 'Now reapply the remaining commands'
         CALL MSGWRT (1)
         NRPL = NBPS / (2 * CATIMG(KINAX))
         IF (NRPL.LT.1) NRPL = 1
C                                       make a new FC file
         NVER = FCVERS + 1
         CALL SPFCOP (LUNFC2, DISKOU, CNOOUT, NVER, CATIMG, I, J,
     *      FCBUF2, IRET)
         IF (IRET.NE.0) GO TO 100
C                                       save load parms
         DLTYPE = LTYPE
         DLSTOK = LSTOKS
         DLSMOO = LSMOO
         DLSCAN = LSCAN
         DLBASL = LBASL
         CALL COPY (4, IMGWIN, DWIND)
         DSTKFG = STKFLG
C                                       init counters
         PNFLAG = NNFLAG
         NNFLAG = 0
         NCFLAG = 0
         FCNWAS = 0
         NX = (CATIMG(KINAX) - 3) / 3
         NY = CATIMG(KINAX+1)
         DO 390 IFL1 = 1,PNFLAG
            CALL TABIO ('READ', 0, IFL1, FCTIME, FCBUF, IRET)
            IF (IRET.GT.0) GO TO 951
            IF (IRET.LT.0) GO TO 390
C                                       copy early records
            IF (NCF.GT.FCNUMB) THEN
               IF (FCNUMB.NE.FCNWAS) THEN
                  FCNWAS = FCNUMB
                  NCFLAG = NCFLAG + 1
                  END IF
               FCNUMB = NCFLAG
               NNFLAG = NNFLAG + 1
               CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF2, IRET)
               IF (IRET.NE.0) GO TO 951
C                                       reapply flags
            ELSE
               CALL H2CHR (8, 1, FCOPER, OP)
               CALL H2CHR (4, 1, FCSFLG, STKFLG)
C                                       "simple" rectangles
               IF (OP.NE.'CLIP') THEN
                  IF (FCNUMB.NE.FCNWAS) NCFLAG = NCFLAG + 1
                  FCNWAS = FCNUMB
                  FCNUMB = NCFLAG
                  NNFLAG = NNFLAG + 1
                  CALL TABIO ('WRIT', 0, NNFLAG, FCTIME, FCBUF2, IRET)
                  IF (IRET.NE.0) GO TO 951
C                                       X-coordinate list
                  IF (OP.EQ.'CHANL-DT') THEN
                     IXL = FCCHAN(1) - BCHAN + 1
                     IXH = IXL
                  ELSE IF ((OP.EQ.'TIME') .OR. (OP.EQ.'TIMERANG')) THEN
                     IXL = 1
                     IXH = NCHAN
                  ELSE
                     IXL = FCCHAN(1) - BCHAN + 1
                     IXH = FCCHAN(2) - BCHAN + 1
                     END IF
                  NCC = 0
                  CALL GETIME (FCTIME(1), FCTIME(2), IYL, IYH, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1335) FCTIME, IYL, IYH
                     CALL MSGWRT (8)
                     GO TO 365
                     END IF
                  IWIN(1) = 1
                  IWIN(2) = 1
                  IWIN(2) = ((IYL - 1) / NRPL) * NRPL + 1
                  IWIN(4) = ((IYH - 1 + NRPL) / NRPL) * NRPL
                  IWIN(2) = MAX (1, MIN (IWIN(2), NY))
                  IWIN(4) = MAX (1, MIN (IWIN(4), NY))
                  IF (FCIF(1).LE.0) THEN
                     KBIF = BIF
                     KEIF = EIF
                  ELSE
                     KBIF = FCIF(1)
                     KEIF = FCIF(2)
                     END IF
                  IF (FCBASL(1).LE.0) THEN
                     KBBASL = 1
                     KEBASL = CATIMG(KINAX+3)
                  ELSE
                     KBBASL = 2
                     KEBASL = 1
                     DO 330 KBASL = 1,MBL
                        IF(FCBASL(1).EQ.NOANTS(3,KBASL)) KBBASL = KBASL
                        IF(FCBASL(2).EQ.NOANTS(3,KBASL)) KEBASL = KBASL
 330                    CONTINUE
                     END IF
                  MSGTXT = '******** Restoring :'
                  CALL MSGWRT (2)
                  CALL SPFCLI (2)
                  DO 361 KBASL = KBBASL,KEBASL
                     IF ((FCBASL(1).LT.0) .AND.
     *                  (-FCBASL(1).NE.NOANTS(1,KBASL)) .AND.
     *                  (-FCBASL(1).NE.NOANTS(2,KBASL))) GO TO 361
                  DO 360 KST = 1,4
                     IF (.NOT.DOSTOK (ILSTOK, STKFLG, KST)) GO TO 360
                     IDEPTH(1) = KST
                     IDEPTH(2) = KBASL
                     IDEPTH(3) = 1
                     IDEPTH(4) = 1
                     IDEPTH(5) = 1
                     CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH,
     *                  IBLKOF, IRET)
                     IBLKOF = IBLKOF + 1
                     CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *                  CATIMG(KINAX+1), IWIN, BUFF0, JBUFSZ, IBLKOF,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1250) IRET, 'INIT R'
                        GO TO 950
                        END IF
                     CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *                  CATIMG(KINAX+1), IWIN, BIGBOY, JBUFSZ, IBLKOF,
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1250) IRET, 'INIT W'
                        GO TO 950
                        END IF
                     I1 = IWIN(2)
                     I2 = IWIN(4)
                     DO 350 IY = I1,I2
                        CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1250) IRET, 'READ'
                           GO TO 950
                           END IF
                        CALL MDISK ('WRIT', LUN1, FIND1, BIGBOY, IB1,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1250) IRET, 'WRITE'
                           GO TO 950
                           END IF
                        CALL RCOPY (CATIMG(KINAX), BUFF0(IB0),
     *                     BIGBOY(IB1))
                        IF ((IY.GE.IYL) .AND. (IY.LE.IYH) .AND.
     *                     (IXL.GT.0)) THEN
                           DO 345 KIF = KBIF,KEIF
                              IOFF = IB1 + 2 + 3 * NCHAN * (KIF-BIF)
                              DO 340 I = IXL,IXH
                                 I1 = IOFF + 3*I
                                 IF (BIGBOY(I1).EQ.0.0) THEN
                                    NCC = NCC + 1
                                    BIGBOY(I1) = FCNUMB
                                    END IF
 340                             CONTINUE
 345                          CONTINUE
                           END IF
 350                    CONTINUE
C                                       finish the writing
                     CALL MDISK ('FINI', LUN1, FIND1, BIGBOY, IB1, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1250) IRET, 'FINI'
                        GO TO 950
                        END IF
 360                 CONTINUE
 361                 CONTINUE
 365              WRITE (MSGTXT,1365) NNFLAG, NCC
                  CALL MSGWRT (2)
C                                       CLIP: call SPFOAD, SPFLIP
C                                       on 1st one only
               ELSE IF (FCNUMB.NE.FCNWAS) THEN
                  FCNWAS = FCNUMB
                  NCFLAG = NCFLAG + 1
                  FCNUMB = NCFLAG
                  MSGTXT = '******** Restoring :'
                  CALL MSGWRT (2)
                  CALL SPFCLI (2)
C                                       close uv data file
                  CALL ZCLOSE (LUN0, FIND0, IERR)
                  CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       create flag against TV file
                  LTYPE = FCTVTY
                  LSTOKS = (FCTVST - CATID(KDCRV+2)) / CATIR(KRCIC+2) +
     *               CATIR(KRCRP+2) + 0.1
                  LSMOO = FCTVAV / CATIR(KRCIC+1) + 0.1
                  LSCAN = FCTVSC / CATIR(KRCIC+1) + 0.1
                  LBASL = FCTVBL
                  CALL COPY (4, FCTVWI, IMGWIN)
                  IPL = 1
                  CALL SPFOAD (IPL, IMGWIN, DIMB2, ABUF, IOBUF, SCRTCH,
     *               BIGBOY(1), BIGBOY(BIGPT(2)), KIGBOY(BIGPT(3)),
     *               BIGBOY(BIGPT(4)), BIGBOY(BIGPT(5)),
     *               BIGBOY(BIGPT(6)), IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1370) IRET
                     GO TO 950
                  ELSE IF (IRET.LT.0) THEN
                     GO TO 390
                     END IF
                  PLTYPE = LTYPE
                  PLSTOK = LSTOKS
                  PLSMOO = LSMOO
                  PLSCAN = LSCAN
                  PLBASL = LBASL
C                                       do flagging
                  CLIMIT(1) = FCLIPR(1)
                  CLIMIT(2) = FCLIPR(2)
                  CALL SPFLIP (IMGWIN, CLIMIT, FCBUF2, SCRTCH,
     *               BIGBOY(BIGPT(3)), BIGBOY(BIGPT(4)),
     *               BIGBOY(BIGPT(5)), BIGBOY(BIGPT(6)), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1371) IRET
                     GO TO 950
                     END IF
C                                       reopen uv files
                  CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1250) IRET, 'OPEN W'
                     GO TO 950
                     END IF
                  CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1250) IRET, 'OPEN R'
                     GO TO 950
                     END IF
                  END IF
               END IF
 390        CONTINUE
C                                       close uv data file
         CALL ZCLOSE (LUN0, FIND0, IERR)
         CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       recover load parms
         LTYPE = DLTYPE
         LSTOKS = DLSTOK
         LSMOO = DLSMOO
         LSCAN = DLSCAN
         LBASL = DLBASL
         CALL COPY (4, DWIND, IMGWIN)
         STKFLG = DSTKFG
C                                       kill old one
         CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IERR)
         CALL TABIO ('CLOS', 0, J1, FCTIME, FCBUF2, IERR)
         CALL ZPHFIL ('FC', DISKOU, CNOOUT, FCVERS, PHNAME, IERR)
         CALL ZDESTR (DISKOU, PHNAME, IRET)
         FCVERS = NVER
         GO TO 999
         END IF
      GO TO 900
C                                       TTY error
 850  IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1850) IRET
         CALL MSGWRT (8)
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'FORMAT ERROR: RETURN TO MENU'
         CALL MSGWRT (6)
         END IF
C                                       close FC file
 900  CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IERR)
      GO TO 999
C                                       close 2 FC files on error
 950  CALL MSGWRT (8)
 951  CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IERR)
      CALL TABIO ('CLOS', 0, J1, FCTIME, FCBUF2, IERR)
C                                       kill new one
      CALL ZPHFIL ('FC', DISKOU, CNOOUT, NVER, PHNAME, IERR)
      CALL ZDESTR (DISKOU, PHNAME, IERR)
      CALL DELEXT ('FC', DISKOU, CNOOUT, 'WRWR', BUFFER, BUFFER(257),
     *   NVER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Enter range of flag commands to display (2 I) from 1 to',
     *   I4)
 1101 FORMAT (8X,'Required',I6,' individual flagging instructions')
 1200 FORMAT ('Enter the',I3,'th flag number to delete <=',I4,
     *   ' (one I), 0 ends list')
 1231 FORMAT ('Deleted',I6,' lines in FC table -',
     *   ' now clear flags in grid file')
 1232 FORMAT ('Dropping',I5,' lines from end of FC table, size now',
     *   I6,' lines')
 1233 FORMAT ('Next flag number changed from',I5,' to',I5)
 1250 FORMAT ('SPFUNF: ERROR',I5,' DOING ',A,' ON MASTER GRID')
 1290 FORMAT ('Restored ',I10,' pixels in the master grid file')
 1310 FORMAT ('Begin by clearing commands >',I4,' from master grid')
 1326 FORMAT ('Restored',I9,' pixels flagged after the lowest undone',
     *   ' command')
 1335 FORMAT ('SPFUNF: GETIME ERROR T1,T2,IYL,IYH=',2(1PE12.4),2I6)
 1365 FORMAT ('New flag command line',I5,' flagged',I6,' pixels')
 1370 FORMAT ('ERROR',I6,' LOADING TV TO REDO CLIP OPERATION')
 1371 FORMAT ('ERROR',I6,' REDOING CLIP OPERATION')
 1850 FORMAT ('TERMINAL ERROR',I5)
      END
      SUBROUTINE SPFCLI (MSGLEV)
C-----------------------------------------------------------------------
C   displays contents of an FC table line on the message file/terminal
C   Inputs:
C      MSGLEV   I   message level to use
C   Common:
C      Must have the FC line in its common
C-----------------------------------------------------------------------
      INTEGER   MSGLEV
C
      INTEGER   IBL1, IBL2, JBL1, JBL2, IS1, IERR, I, NT1, NT2
      REAL      T1, T2
      CHARACTER OP*8, STR*16, CHTYPE(10)*8, CHST(13)*2, REAZON*24,
     *   TS1*20, TS2*20
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA CHST /'YX','XY','YY','XX', 'LR','RL','LL','RR', '??',
     *   'I','Q','U ','V '/
      DATA CHTYPE /'AMPLTUDE', 'PHASE   ', 'RMS AMPL', 'RMS/MEAN',
     *   'RMS VAMP', 'VRMS/AVG', 'VEC DIFF', 'AMP DIFF', 'PHS DIFF',
     *   '????????'/
C-----------------------------------------------------------------------
      CALL H2CHR (24, 1, FCREAS, REAZON)
      CALL H2CHR (8, 1, FCOPER, OP)
      IF ((OP.NE.'CHANNEL') .AND. (OP.NE.'CLIP')) THEN
         T1 = MAX (START, MIN (STOP, FCTIME(1)))
         CALL TORMAT (T1, TFORM, TS1, NT1)
         T2 = MAX (START, MIN (STOP, FCTIME(2)))
         CALL TORMAT (T2, TFORM, TS2, NT2)
         END IF
      IF ((OP.EQ.'PIXEL') .OR. (OP.EQ.'CHANL-DT')) THEN
         WRITE (MSGTXT,1010) FCNUMB, OP, FCCHAN(1), TS1(:NT1), TS2(:NT2)
      ELSE IF (OP.EQ.'TIME') THEN
         WRITE (MSGTXT,1020) FCNUMB, OP, TS1(:NT1), TS2(:NT2)
      ELSE IF (OP.EQ.'CHANNEL') THEN
         WRITE (MSGTXT,1030) FCNUMB, OP, FCCHAN(1)
      ELSE IF (OP.EQ.'TIMERANG') THEN
         WRITE (MSGTXT,1020) FCNUMB, OP, TS1(:NT1), TS2(:NT2)
      ELSE IF (OP.EQ.'AREA') THEN
         WRITE (MSGTXT,1060) FCNUMB, OP, FCCHAN
         CALL MSGWRT (MSGLEV)
         WRITE (MSGTXT,1061) TS1(:NT1), TS2(:NT2)
      ELSE IF (OP.EQ.'CLIP') THEN
         I = FCTVTY
         IF ((I.LT.1) .OR. (I.GT.9)) I = 10
         WRITE (MSGTXT,1070) FCNUMB, OP, CHTYPE(I), FCLIPR
         CALL MSGWRT (MSGLEV)
         JBL2 = FCTVST + 9
         IF ((JBL2.LT.1) .OR. (JBL2.GT.13)) JBL2 = 9
         IBL2 = FCTVAV + 0.5
         IS1 = SUBARR
         IBL1 = NOANTS(1,FCTVBL)
         JBL1 = NOANTS(2,FCTVBL)
         WRITE (MSGTXT,1071) CHST(JBL2), FCTVIF, IBL1, JBL1, IS1, IBL2
         CALL MSGWRT (MSGLEV)
         IF ((I.GE.5) .AND. (I.LE.7)) THEN
            WRITE (MSGTXT,1072) FCTVWI, IBL2
         ELSE
            WRITE (MSGTXT,1073) FCTVWI
            END IF
         END IF
      CALL MSGWRT (MSGLEV)
      WRITE (MSGTXT,1080)
      CALL H2CHR (4, 1, FCSFLG, MSGTXT(22:25))
      IF (FCIF(1).LE.0) THEN
         MSGTXT(32:34) = 'All'
      ELSE
         WRITE (STR,1084) FCIF(1), FCIF(2)
         MSGTXT(32:37) = STR(:6)
         END IF
      IF (FCBASL(1).EQ.0) THEN
         MSGTXT(51:) = 'All'
      ELSE IF (FCBASL(1).LT.0) THEN
         IBL1 = -FCBASL(1)
         WRITE (STR,1082) IBL1
         MSGTXT(51:) = STR
      ELSE
         CALL GETBLN (FCBASL(1), NUMAN, IBL1, JBL1, IS1, IERR)
         WRITE (STR,1083) IBL1, JBL1, IS1
         MSGTXT(51:) = STR
         END IF
      CALL MSGWRT (MSGLEV)
C                                       source selection
      MSGTXT = ' '
      IF (FCSOUR.LE.0) THEN
         MSGTXT(9:) = 'Flag all sources meeting these criteria'
      ELSE
         MSGTXT(9:) = 'Flag only sources included in this TVFLG'
         END IF
      CALL MSGWRT (MSGLEV)
C                                       reason
      IF ((REAZON.NE.' ') .AND. (REAZON.NE.'SPFLG:date time')) THEN
         MSGTXT = 'Reason = ''' // REAZON // ''''
         CALL MSGWRT (MSGLEV)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I3,2X,A8,1X,'Ch =',I5,' Time range ',A,' - ',A)
 1020 FORMAT (I3,2X,A8,1X,'All channels at T = ',A,' - ',A)
 1030 FORMAT (I3,2X,A8,1X,'Channel',I5,'  at all times')
 1060 FORMAT (I3,2X,A8,1X,'Channel',I5,'  through channel =',I5)
 1061 FORMAT (14X,' Time range ',A,' - ',A)
 1070 FORMAT (I3,2X,A8,1X,'Type ',A8,' clip range',2(1PE13.5))
 1071 FORMAT (14X,'Based on Stokes ',A2,', IF',I3,', Bl',I3.2,'-',I2.2,
     *   '/',I2.2,', Avg',I4,' s')
 1072 FORMAT (23X,'Window',4I6,'   Scan',I4)
 1073 FORMAT (23X,'Window',4I6)
 1080 FORMAT (8X,'Flag: Stokes ',4X,',  IF=',6X,',  Baseline=')
 1082 FORMAT (I3.2,' - *')
 1083 FORMAT (I3.2,' -',I3.2,' /',I3.2)
 1084 FORMAT (I3.2,'-',I2.2)
      END
      SUBROUTINE SPFMRK (BUFF1, BUFF2, IRET)
C-----------------------------------------------------------------------
C   places the flagging commands of SPFLG into the flag table for multi-
C   source data and flags single-source data.
C   Output:
C      BUFF1    R(*)   scratch
C      BUFF2    R(*)   scratch
C      IRET    I       error code
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      BUFF1(*), BUFF2(*)
C
      INCLUDE 'SPFLG.INC'
      HOLLERITH CATH(256)
      CHARACTER REASON*24
      INTEGER   LUN, KSUBA, NBASE, KBIF, KEIF, KBCH, KECH, IERR, IROUND,
     *   XA1(100), XA2(100), I, J, J1, J2, IANT, JANT, IARR, LUN1, LUN2,
     *   JERR, FLGCNT, KEY(2,2), LCOR0, IFG, NFG, KEYSUB(2,2), LUNTMP
      LOGICAL   ISCOMP
      REAL      XT1, XT2, TIME, CATR(256), FKEY(2,2)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      DATA LUN1, LUN2 /16, 17/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       sort the FC table
      FGFLAG = 0
      KEY(1,1) = 3
      KEY(1,2) = 0
      KEY(2,1) = 0
      KEY(2,2) = 0
      FKEY(1,1) = 1.0
      FKEY(1,2) = 0.0
      FKEY(2,1) = 0.0
      FKEY(2,2) = 0.0
      CALL COPY (256, CATIMG, CATBLK)
      CALL TABSRT (DISKOU, CNOOUT, 'FC', FCVERS, FCVERS, KEY, KEYSUB,
     *   FKEY, FCBUF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      FLGCNT = 0
      TIME = CATD(KDCRV+2) + (1.0 - CATR(KRCRP+2)) * CATR(KRCIC+2)
      LCOR0 = IROUND (TIME)
      FLGCNT = 0
C                                       is it multisource
      CALL COPY (256, CATUV, CATBLK)
      CALL UVPGET (JERR)
      ISCOMP = CATBLK(KINAX).EQ.1
      CALL MULSDB (CATBLK, ISINGL)
      ISINGL = .NOT.ISINGL
      CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      FGVER = IROUND (XFLAG)
      OFGVER = IROUND (XFGOUT)
      CALL FNDEXT ('FG', CATBLK, IFG)
      IF (FGVER.GT.IFG) FGVER = -1
      IF (FGVER.EQ.0) FGVER = IFG
      IF ((OFGVER.LE.0) .OR. (OFGVER.GT.IFG)) OFGVER = IFG + 1
C                                       output FG version
      WRITE (MSGTXT,1005) OFGVER
      CALL MSGWRT (4)
C                                       copy old FG file
      IF ((FGVER.GT.0) .AND. (OFGVER.GT.IFG)) THEN
         CALL TABCOP ('FG', FGVER, OFGVER, LUN1, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFF1, BUFF2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1001) IRET
            GO TO 990
            END IF
         END IF
C                                       loop through flags
      LUN = LUNTMP (1)
      DO 80 I = 1,NNFLAG
         CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
         IF (IRET.LT.0) GO TO 80
         IF (IRET.NE.0) GO TO 999
         KBCH = FCCHAN(1)
         KECH = FCCHAN(2)
         KBIF = FCIF(1)
         KEIF = FCIF(2)
         XT1 = MAX (FCTIME(1), START-1.0) - TXPND
         XT2 = MIN (FCTIME(2), STOP+1.0)  + TXPND
         NBASE = 0
         KSUBA = -1
         J1 = FCBASL(1)
         J2 = FCBASL(2)
         CALL H2CHR (24, 1, FCREAS, REASON)
         IF (REASON.EQ.'SPFLG:date time') THEN
            WRITE (REASON,1010) TSKNAM, TTIME
            END IF
         CALL H2CHR (4, 1, FCSFLG, USTFLG)
         CALL CVSTOK (LCOR0, USTFLG, ICOR0, STKFLG)
C                                       range of baselines
         IF ((J1.GT.0) .AND. (J2.GT.0)) THEN
            DO 40 J = J1,J2
               CALL GETBLN (J, NUMAN, IANT, JANT, IARR, IERR)
               IF (IERR.NE.0) GO TO 40
               IF ((IARR.NE.KSUBA) .OR. (NBASE.GE.100)) THEN
                  IF (NBASE.GT.0) THEN
                     CALL FLAGUP ('FLAG', LUN, IUDISK, IUCNO, OFGVER,
     *                  FGBUFF, IFGRNO, FGKOLS, FGNUMV, FCSOUR, 1,
     *                  KSUBA, FRQSEL, NBASE, XA1, XA2, XT1, XT2, KBIF,
     *                  KEIF, KBCH, KECH, STKFLG, REASON, NFG, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) IRET
                        GO TO 990
                        END IF
                     FGFLAG = FGFLAG + NFG
                     END IF
                  NBASE = 0
                  END IF
               KSUBA = IARR
               NBASE = NBASE + 1
               IF (IANT.GT.JANT) THEN
                  XA2(NBASE) = IANT
                  XA1(NBASE) = JANT
               ELSE
                  XA1(NBASE) = IANT
                  XA2(NBASE) = JANT
                  END IF
 40            CONTINUE
            IF (NBASE.GT.0) THEN
               CALL FLAGUP ('FLAG', LUN, IUDISK, IUCNO, OFGVER, FGBUFF,
     *            IFGRNO, FGKOLS, FGNUMV, FCSOUR, 1, KSUBA, FRQSEL,
     *            NBASE, XA1, XA2, XT1, XT2, KBIF, KEIF, KBCH, KECH,
     *            STKFLG, REASON, NFG, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) IRET
                  GO TO 990
                  END IF
               FGFLAG = FGFLAG + NFG
               END IF
C                                       single baseline descriptor
         ELSE
            NBASE = 1
            KSUBA = -J2
            XA1(1) = -J1
            XA2(1) = 0
            CALL FLAGUP ('FLAG', LUN, IUDISK, IUCNO, OFGVER, FGBUFF,
     *         IFGRNO, FGKOLS, FGNUMV, FCSOUR, 1, KSUBA, FRQSEL, NBASE,
     *         XA1, XA2, XT1, XT2, KBIF, KEIF, KBCH, KECH, STKFLG,
     *         REASON, NFG, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) IRET
               GO TO 990
               END IF
            FGFLAG = FGFLAG + NFG
            END IF
 80      CONTINUE
      CALL FLAGUP ('CLOS', LUN, IUDISK, IUCNO, OFGVER, FGBUFF, IFGRNO,
     *   FGKOLS, FGNUMV, FCSOUR, 1, KSUBA, FRQSEL, NBASE, XA1, XA2,
     *   XT1, XT2, KBIF, KEIF, KBCH, KECH, STKFLG, REASON, NFG, IRET)
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
      IRET = 0
      WRITE (MSGTXT,1080) FGFLAG
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' SORTING THE FC TABLE')
 1001 FORMAT ('ERROR',I5,' COPYING OLD FLAGS',I3,' TO NEW FG TABLE',I3)
 1005 FORMAT ('Writing flagging information in FG table ',I3)
 1010 FORMAT (A6,A9,1X,A8)
 1015 FORMAT ('SPFMRK: FLAGUP RETURNS ERROR',I5)
 1080 FORMAT ('Wrote',I8,' flags in the FG table')
      END
      SUBROUTINE SPFLHI (BUFF0, BUFF1)
C-----------------------------------------------------------------------
C   SPFLHI adds to the history file of the input UV data set info on
C   what was flagged.  It then removes that flagging info from the
C   flag command file and from the master grid, when these are kept in
C   the image catalog.
C-----------------------------------------------------------------------
      REAL      BUFF0(*), BUFF1(*)
C
      CHARACTER HILINE*72
      INTEGER   HLUNI, IERR, I, I1, I2, PLBL, PIF, J, IANT1, JANT1,
     *   IARR1, IRET, J2, IANT2, JANT2, IROUND, IARR2, PLSU, LUN0, LUN1,
     *   FIND0, FIND1, NCC, KST, EST, KBASL, KEBASL, IWIN(4), IDEPTH(5),
     *   IBLKOF, NX, NY, IB0, IB1, PLBL2, NCT1, NCT2
      REAL      DTEMP
      LOGICAL   SAVE, T, F
      CHARACTER PHNAME*48, TS1*20, TS2*20
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA HLUNI, LUN0, LUN1 /28, 16, 17/
      DATA SAVE, T, F /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       history only if some flagged
      IF ((NNFLAG.LE.0) .OR. (XDOHST.LE.-9.5)) GO TO 200
      CALL HIINIT (3)
      CALL HIOPEN (HLUNI, DISKIN, CNOIN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Task message
      WRITE (HILINE,1000) TSKNAM, RLSNAM, TTIME
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Task parms
      DO 10 I = 1,30
         IF ((SOURCS(I).NE.' ') .AND. ((I.LT.2) .OR.
     *      (SOURCS(I).NE.SOURCS(I-1)))) THEN
            I1 = 1
            IF (SOURCS(I)(1:1).EQ.'-') I1 = 2
            IF (DOSWNT) WRITE (HILINE,1001) TSKNAM, SOURCS(I)(I1:)
            IF (.NOT.DOSWNT) WRITE (HILINE,1002) TSKNAM, SOURCS(I)(I1:)
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
 10      CONTINUE
C                                       start and stop times
      CALL HITIME (START, STOP, HLUNI, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes, subarray, IF, chan
      WRITE (HILINE,1015) TSKNAM, STOKES, SUBARR
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      IF ((BIF.NE.1) .OR. (EIF.NE.1)) THEN
         WRITE (HILINE,1016) TSKNAM, BIF, EIF
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF ((BCHAN.NE.1) .OR. (ECHAN.NE.1)) THEN
         WRITE (HILINE,1017) TSKNAM, BCHAN, ECHAN
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF ((UVRNG(1).GT.0.0) .OR. (UVRNG(2).GT.0.0)) THEN
         WRITE (HILINE,1018) TSKNAM, UVRNG
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (DPARM(4).GT.0.0) THEN
         WRITE (HILINE,1019) TSKNAM
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       times
      IF (TXPND.GT.0.0) THEN
         TXPND = TXPND * 24.0 * 3600.0
         WRITE(HILINE,1023) TSKNAM, TXPND
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       calibration tables
      IF (FGVER.GT.0) THEN
         WRITE (HILINE,1020) TSKNAM, FGVER
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (DOCAL) THEN
         WRITE (HILINE,1021) TSKNAM, CLUSE
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (DOBL) THEN
            WRITE (HILINE,1022) TSKNAM, BLVER
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
      I1 = 1
      IF ((DESEL) .AND. (NXANT.GT.0)) THEN
         WRITE (HILINE,1030) TSKNAM
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NXANT.LE.0) THEN
         WRITE (HILINE,1031) TSKNAM
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
 35   I2 = I1 + 12
      IF (I2.GT.NXANT) I2 = NXANT
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1035) TSKNAM, (IXANT(I), I = I1,I2)
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 35
         END IF
      I1 = 1
      WRITE (HILINE,1040) TSKNAM
      IF (NXBASL.LE.0) WRITE (HILINE,1041) TSKNAM
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
 45   I2 = I1 + 12
      IF (I2.GT.NXBASL) I2 = NXBASL
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1045) TSKNAM, (IXBASL(I), I = I1,I2)
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 45
         END IF
C                                       points included/dropped
      WRITE (HILINE,1050) TSKNAM, OFGVER
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (NPOINT.GT.0.0D0) THEN
         WRITE (HILINE,1051) TSKNAM, NPOINT
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NFAIL.GT.0) THEN
         WRITE (HILINE,1052) TSKNAM, NFAIL
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       flagging commands
      WRITE (HILINE,1055) TSKNAM, TSKNAM, TTIME
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1056) TSKNAM, FGFLAG
      CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       option to list details
      IF (XDOHST.LE.0.0) GO TO 100
      PLBL = -1
      PLBL2 = -1
      PIF = -1
      PLSU = -1
      CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *   NNFLAG, FCBUF, IRET)
      IF (IRET.NE.0) GO TO 100
      DO 80 I = 1,NNFLAG
         CALL TABIO ('READ', 0, I, FCTIME, FCBUF, IRET)
         IF (IRET.LT.0) GO TO 80
         IF (IRET.NE.0) GO TO 100
         J = FCBASL(1)
         J2 = FCBASL(2)
         IF ((J.NE.PLBL) .OR. (J2.NE.PLBL2)) THEN
            IF (J.EQ.0) THEN
               IARR1 = 0
               HILINE = TSKNAM //
     *            'ANT = 0, 0, 0          / All baselines'
            ELSE IF (J.LT.0) THEN
               IANT1 = -J
               IARR1 = -J2
               WRITE (HILINE,1061) TSKNAM, IANT1, IARR1
            ELSE
               CALL GETBLN (J, NUMAN, IANT1, JANT1, IARR1, IRET)
               CALL GETBLN (J2, NUMAN, IANT2, JANT2, IARR2, IRET)
               WRITE (HILINE,1062) TSKNAM, IANT1, JANT1, IARR1, IANT2,
     *            JANT2, IARR2
               END IF
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            PLBL = J
            PLBL2 = J2
            END IF
         J = FCIF(1)
         IF (J.NE.PIF) THEN
            WRITE (HILINE,1063) TSKNAM, J, FCIF(2)
            IF (J.LE.0) WRITE (HILINE,1064) TSKNAM
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            PIF = J
            END IF
         IF (FCTIME(1).GT.-1.E5) THEN
            DTEMP = MAX (START, MIN (STOP, FCTIME(1)))
            CALL TORMAT (DTEMP, TFORM, TS1, NCT1)
            DTEMP = MAX (START, MIN (STOP, FCTIME(2)))
            CALL TORMAT (DTEMP, TFORM, TS2, NCT2)
            END IF
         IF (FCTIME(1).GT.-1.E5) THEN
            WRITE (HILINE,1067) TSKNAM, FCCHAN, TS1(:NCT1), TS2(:NCT2)
         ELSE
            WRITE (HILINE,1068) TSKNAM, FCCHAN
            END IF
         CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       source number
         J = FCSOUR
         IF (J.NE.PLSU) THEN
            IF (J.GT.0) THEN
               WRITE (HILINE,1075) TSKNAM, J, SNAMES(J)
            ELSE
               WRITE (HILINE,1076) TSKNAM
               END IF
            CALL HIADD (HLUNI, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            PLSU = J
            END IF
 80      CONTINUE
      CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
C                                       Close HI file
 100  CALL HICLOS (HLUNI, SAVE, BUFFER, IERR)
C                                       Clear FC file
 200  IF ((NNFLAG.GT.0) .AND. (XDOCAT.GT.0.0)) THEN
         MSGTXT = 'Removing applied flags from the master grid file'
         CALL MSGWRT (2)
         CALL SPFCOP (FCLUN, DISKOU, CNOOUT, FCVERS, CATIMG, FCNUMB,
     *      NNFLAG, FCBUF, IRET)
         IF (IRET.EQ.0) THEN
            FCBUF(5) = 0
            CALL TABIO ('CLOS', 0, NNFLAG, FCTIME, FCBUF, IRET)
            END IF
C                                       open master file
         CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IRET)
         CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET, 'OPEN W'
            GO TO 900
            END IF
         CALL ZOPEN (LUN0, FIND0, DISKOU, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET, 'OPEN R'
            GO TO 900
            END IF
         NCC = 0
         EST = CATIMG(KINAX+2)
         KEBASL = CATIMG(KINAX+3)
         DO 230 KBASL = 1,KEBASL
         DO 230 KST = 1,EST
C                                       init master file
            IWIN(1) = 1
            IWIN(2) = 1
            IWIN(3) = CATIMG(KINAX)
            IWIN(4) = CATIMG(KINAX+1)
            IDEPTH(1) = KST
            IDEPTH(2) = KBASL
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('READ', LUN0, FIND0, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF0, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'INIT R'
               GO TO 900
               END IF
            CALL MINIT ('WRIT', LUN1, FIND1, CATIMG(KINAX),
     *         CATIMG(KINAX+1), IWIN, BUFF1, JBUFSZ, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'INIT W'
               GO TO 900
               END IF
            NY = CATIMG(KINAX+1)
            NX = (CATIMG(KINAX) - 3) / 3
            DO 220 J = 1,NY
               CALL MDISK ('READ', LUN0, FIND0, BUFF0, IB0, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1200) IRET, 'READ'
                  GO TO 900
                  END IF
               CALL MDISK ('WRIT', LUN1, FIND1, BUFF1, IB1, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1200) IRET, 'WRITE'
                  GO TO 900
                  END IF
               CALL RCOPY (CATIMG(KINAX), BUFF0(IB0), BUFF1(IB1))
               DO 210 I = 1,NX
                  I1 = IB1 + 3*I + 2
                  IF (BUFF1(I1).NE.FBLANK) THEN
                     I2 = IROUND (BUFF1(I1))
                     IF (I2.GT.0) THEN
                        NCC = NCC + 1
                        BUFF1(I1) = FBLANK
                        END IF
                     END IF
 210              CONTINUE
 220           CONTINUE
C                                       finish the writing
            CALL MDISK ('FINI', LUN1, FIND1, BUFF1, IB1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1200) IRET, 'FINI'
               GO TO 900
               END IF
 230        CONTINUE
         IF (NCC.LE.0) THEN
            MSGTXT = 'NO PIXELS FULLY REMOVED FROM THE MASTER GRID FILE'
            CALL MSGWRT (6)
         ELSE
            WRITE (MSGTXT,1230) NCC
            CALL MSGWRT (2)
            END IF
         CALL ZCLOSE (LUN0, FIND0, IERR)
         CALL ZCLOSE (LUN1, FIND1, IERR)
         END IF
      GO TO 999
C                                       Error
 900  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A9,2X,A8)
 1001 FORMAT (A6,'SOURCES=''',A16,'''',9X,'/ Source name included')
 1002 FORMAT (A6,'SOURCES=''',A16,'''',9X,'/ Source name excluded')
 1015 FORMAT (A6,'STOKES=''',A4,'''  SUBARRAY=',I3)
 1016 FORMAT (A6,'BIF=',I4,2X,'EIF=',I4,5X,'/ Range of IF axis')
 1017 FORMAT (A6,'BCHAN=',I4,2X,'ECHAN=',I4,5X,'/ Range of freq
     *   axis')
 1018 FORMAT (A6,'UVRANGE = ',2(1PE13.5),5X,
     *   '/ Range of uv kilo lambda')
 1019 FORMAT (A6,'/ Divided by the source flux')
 1020 FORMAT (A6,'FLAGVER=',I3,5X,'/ FLAG table used')
 1021 FORMAT (A6,'GAINUSE=',I3,5X,'/ CL table used')
 1022 FORMAT (A6,'BLVER=',I3,5X,'/ Baseline table used')
 1023 FORMAT (A6,'TXPND=',F7.2,1X,'/ Expand times by in secs')
 1030 FORMAT (A6,'/ All antennas except:')
 1031 FORMAT (A6,'/ All antennas')
 1035 FORMAT (A6,'ANTENNAS=',I3,12(',',I3),',')
 1040 FORMAT (A6,'/ with')
 1041 FORMAT (A6,'/ with all antennas')
 1045 FORMAT (A6,'BASELINES=',I3,12(',',I3),',')
 1050 FORMAT (A6,'OFGVER =',I4,5X,'/ output flag table version')
 1051 FORMAT (A6,'/ Included',F13.0,' points in the grid')
 1052 FORMAT (A6,'/ Dropped ',I12,'  points off the grid')
 1055 FORMAT (A6,'REASON = ''',A5,1X,A9,1X,A8,'''')
 1056 FORMAT (A6,'/ Number FG table records written',I8)
 1061 FORMAT (A6,'ANT = ',I2,', 0,',I4,10X,'/ All with 1 antenna')
 1062 FORMAT (A6,'ANT = ',3(I2,','),I4,2(',',I2))
 1063 FORMAT (A6,'IF =',I3,' -',I3)
 1064 FORMAT (A6,'IF = 0',10X,'/ All IFs')
 1067 FORMAT (A6,'CHAN =',I5,',',I5,'  TIMERANG= ',A,1X,A)
 1068 FORMAT (A6,'CHAN =',I5,',',I5,
     *   '  TIMERANG=  00/00:00:00,  00/00:00:00')
 1075 FORMAT (A6,'SOURCE =',I5,10X,'/ Source number for ',A)
 1076 FORMAT (A6,'SOURCE =    0',10X,'/ All source numbers')
 1200 FORMAT ('SPFLHI: ERROR',I5,' DOING ',A,' ON MASTER GRID')
 1230 FORMAT ('Removed fully',I10,' pixels from the master grid file')
      END
      SUBROUTINE SPFBOX (IG, IPL, BBLC, BTRC, XYCENT, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   SPFBOX uses a graphics plane to show the user a rectangular box as
C   it is set with the cursor.
C   Inputs:
C      IG      I       graphics plane to use
C      IPL     I       TV grey plane in use
C   Output:
C      BBLC    R(7)    Bottom left corners
C      BTRC    R(7)    Top right corners
C      SCRTCH  I(*)    Scratch buffer: > X dimension (>1280)
C      IERR    I       Error code
C   Common:
C      /MAPHDR/ used - returns TV image header.
C-----------------------------------------------------------------------
      INTEGER   IG, IPL, XYCENT(2), SCRTCH(*), IERR
      REAL      BBLC(7), BTRC(7)
C
      INTEGER   ICH, ITW(3), IL, IX(5), IY(5), QUAD, IBUT, I, JERR,
     *   LTVSC(2), IPOS, NERR, IX0, IY0, LMGCOR(4)
      REAL      PPOS(2), RPOS(2)
      LOGICAL   T, F, DOIT, ONGR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
C                                       Init
      CALL YWINDO ('READ', WINDTV, IERR)
      IF (IERR.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IERR = 0
         END IF
      CALL COPY (4, CATBLK(IICOR), LMGCOR)
C                                       get image header
      IX0 = (WINDTV(1) + WINDTV(3)) / 2
      IY0 = (WINDTV(2) + WINDTV(4)) / 2
      IF (XYCENT(1).GT.0) IX0 = XYCENT(1)
      IF (XYCENT(2).GT.0) IY0 = XYCENT(2)
      CALL YCREAD (IPL, IX0, IY0, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      ICH = NGRAY + IG
      CALL ZTIME (ITW)
C                                       turn on graphics if needed
      LTVSC(1) = TVSCGX
      LTVSC(2) = TVSCGY
      I = 2 ** (NGRAY+IG-1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      IF (.NOT.ONGR) THEN
         CALL YSLECT ('ONNN', ICH, 0, SCRTCH, IERR)
         IPOS = 1
         IF (IERR.NE.0) GO TO 900
         END IF
      IPOS = 2
C                                       Init BLC of new box
      MSGTXT = 'Set B.L.C. : button A, B, C, or D to change to T.R.C.'
      CALL MSGWRT (1)
      IX(1) = (WINDTV(1) + WINDTV(3)) / 2
      IX(2) = IX(1)
      IX(3) = MAXXTV(1)
      IX(4) = IX(3)
      IX(5) = IX(4)
      IY(1) = MAXXTV(2)
      IY(2) = (WINDTV(2) + WINDTV(4)) / 2
      IY(3) = IY(2)
      IY(4) = IY(2)
      IY(5) = IY(4)
      RPOS(1) = IX(2)
      RPOS(2) = IY(2)
      IL = 1
C                                       No scroll correction
      QUAD = -1
C                                       ON cursor at desired position
 45   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 900
      IF (IERR.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 3
      IF (IERR.NE.0) GO TO 900
C                                       Cursor read loop
 50   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 4
         IF (IERR.NE.0) GO TO 900
         IF (RPOS(1).LT.CATBLK(IICOR  )) RPOS(1) = LMGCOR(1)
         IF (RPOS(2).LT.LMGCOR(2)) RPOS(2) = LMGCOR(2)
         IF (RPOS(1).GT.LMGCOR(3)) RPOS(1) = LMGCOR(3)
         IF (RPOS(2).GT.LMGCOR(4)) RPOS(2) = LMGCOR(4)
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       Erase current box
         CALL IMVECT ('OFFF', ICH, 5, IX(1), IY(1), SCRTCH, IERR)
         IPOS = 5
         IF (IERR.NE.0) GO TO 900
C                                       New corners: bottom
         IF (IL.NE.2) THEN
            IX(1) = RPOS(1) + 0.01
            IX(2) = IX(1)
            IY(2) = RPOS(2) + 0.01
            IY(3) = IY(2)
            IF (IL.EQ.1) THEN
               IY(4) = IY(3)
               IY(5) = IY(4)
            ELSE
               IX(5) = IX(1)
               END IF
C                                       top: regular boxes
         ELSE
            IX(3) = RPOS(1) + 0.81
            IX(4) = IX(3)
            IY(1) = RPOS(2) + 0.81
            IY(4) = IY(1)
            IY(5) = IY(1)
            IX(5) = IX(1)
            END IF
C                                       draw all boxes
         CALL IMVECT ('ONNN', ICH, 5, IX(1), IY(1), SCRTCH, IERR)
         IPOS = 6 + 100
         IF (IERR.NE.0) GO TO 900
C                                       Respond to buttons
         IF (IBUT.EQ.0) GO TO 50
C                                       switch to TRC all buttons
            IF (IL.NE.1) GO TO 80
               IL = 2
               MSGTXT = 'Set T.R.C. : button A or B to repeat'
     *            // ' other corner'
               CALL MSGWRT (1)
               MSGTXT = 'Push C or D to exit'
               CALL MSGWRT (1)
               RPOS(1) = RPOS(1) + 10.0
               RPOS(2) = RPOS(2) + 10.0
               IF (RPOS(1).GT.MAXXTV(1)) RPOS(1) = MAXXTV(1)
               IF (RPOS(2).GT.MAXXTV(2)) RPOS(2) = MAXXTV(2)
               GO TO 45
C                                       switch to other corn, but A
 80         IF (IBUT.LE.2) THEN
               IF (IL.NE.3) THEN
                  IL = 3
                  RPOS(1) = IX(2)
                  RPOS(2) = IY(2)
                  MSGTXT = 'Reset B.L.C. : button A or B to repeat'
     *               // ' other corner'
               ELSE
                  IL = 2
                  RPOS(1) = IX(4)
                  RPOS(2) = IY(4)
                  MSGTXT = 'Reset T.R.C. : button A or B to repeat'
     *               // ' other corner'
                  END IF
               CALL MSGWRT (1)
               MSGTXT = 'Push C or D to exit'
               CALL MSGWRT (1)
               GO TO 45
               END IF
C                                       force real BLC, TRC
         CALL IMVECT ('OFFF', ICH, 5, IX(1), IY(1), SCRTCH, IERR)
         IF (IX(2).GT.IX(4)) THEN
            JERR = IX(2)
            IX(2) = IX(4)
            IX(4) = JERR
            END IF
         IF (IY(2).GT.IY(4)) THEN
            JERR = IY(2)
            IY(2) = IY(4)
            IY(4) = JERR
            END IF
C                                       BLCs with scroll now
         RPOS(1) = IX(2)
         RPOS(2) = IY(2)
         CALL YCUCOR (RPOS, QUAD, BBLC(1), IERR)
         IF (IERR.GT.1) GO TO 900
         IF (IERR.EQ.1) NERR = NERR + 1
C                                       TRC
         RPOS(1) = IX(4)
         RPOS(2) = IY(4)
         CALL YCUCOR (RPOS, QUAD, BTRC(1), IERR)
         IF (IERR.GT.1) GO TO 900
         IF (IERR.EQ.1) NERR = NERR + 1
C                                       correct for BL#, times
      BBLC(1) = BBLC(1) - 3
      BTRC(1) = BTRC(1) - 3
      IERR = 0
C                                       Off cursor, graphics, scroll
C                                       leave graphics on
 900  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1900) IERR, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' READING TV-IMAGE CATALOG')
 1900 FORMAT ('SPFBOX: ERROR CODE',I7,' AT',I5)
      END
      SUBROUTINE GETBLO (LUN, NUMAN, IRET)
C-----------------------------------------------------------------------
C   GETBLO returns an array giving the order of baselines by length
C   Inputs:
C      LUN      I         LUN to use in IO
C   In/out:
C      NUMAN    I(1025)   array for figuring baseline numbers
C                             if (NUMAN(1)>0) then 2nd entry
C   Output:
C      IRET     I         Error
C   Uses includes D/CSEL.INC contents.
C-----------------------------------------------------------------------
      INTEGER   LUN, NUMAN(*), IRET
C
      INTEGER   BUFFER(512), I, J, L
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      L = NUMAN(1)
      IF ((L.GT.0) .AND. (NUMAN(514+L).GT.0) .AND. (L.LE.49)) GO TO 999
C                                       first entry:
C                                       get number ants / array
      CALL GETNAN (IUDISK, IUCNO, CATUV, LUN, BUFFER, NUMAN, IRET)
      IF ((IRET.NE.0) .OR. (NUMAN(1).LE.0)) THEN
         IF ((IRET.NE.10) .OR. (TYPUVD.LE.0)) THEN
            WRITE (MSGTXT,1000) IRET, NUMAN(1)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1001) IUDISK, IUCNO
            CALL MSGWRT (6)
            IF (IRET.NE.10) GO TO 999
            END IF
         END IF
C                                       get cumulative number
      J = 0
      L = NUMAN(1)
      DO 10 I = 1,L
         NUMAN(513+I) = J
         J = J + (NUMAN(1+I) * (NUMAN(1+I) + 3)) / 2
 10      CONTINUE
      NUMAN(514+L) = J
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING NUMBER OF ANTENNAS',I5)
 1001 FORMAT ('CHECK DISK',I3,' CATALOG #',I6,' FOR MISSING OR',
     *   ' FAULTY AN FILE')
      END
      SUBROUTINE GETBLN (IBL, NUMAN, IANT, JANT, IARR, IRET)
C-----------------------------------------------------------------------
C   returns the antennas associated with a baseline "number"
C   Inputs:
C      IBL      I         baseline number - ordered by length or #
C      NUMAN    I(1025)   subarray descriptor array
C   Output:
C      IANT     I         antenna number 1
C      JANT     I         antenna number 2
C      IARR     I         subarray number
C      IRET     I         = 1 => can't find length # in table
C-----------------------------------------------------------------------
      INTEGER   IBL, NUMAN(*), IANT, JANT, IARR, IRET
C
      INTEGER   I, JBL, L, LL
C-----------------------------------------------------------------------
      IRET = 2
      L = NUMAN(1)
      LL = NUMAN(514+L)
      IF ((IBL.LT.1) .OR. (IBL.GT.LL)) GO TO 990
C                                       on length type, find baseline
C                                       number
      JBL = IBL
      IRET = 1
      IF ((JBL.LT.1) .OR. (JBL.GT.LL)) GO TO 990
      IRET = 0
C                                       find subarray
      IARR = 0
      DO 10 I = 1,L
         LL = L + 1 - I
         IF (JBL.LT.NUMAN(514+LL)) IARR = LL
 10      CONTINUE
      IF (IARR.LE.0) THEN
         IRET = 2
         GO TO 990
         END IF
C                                       find antennas
      JBL = JBL - NUMAN(513+IARR)
      LL = NUMAN(1+IARR)
      DO 20 I = 1,LL
         L = (I-1) * (2*LL + 4 - I) / 2
         IF (L.LT.JBL) IANT = I
 20      CONTINUE
      JANT = JBL - (IANT-1)*(2*LL+4-IANT)/2 + IANT - 1
C
      IF ((IANT.LT.1) .OR. (IANT.GT.LL)) IRET = 3
      IF ((JANT.LT.1) .OR. (JANT.GT.LL)) IRET = 3
C
 990  IF (IRET.NE.0) THEN
         IANT = 0
         JANT = 0
         IARR = -IRET
         END IF
C
 999  RETURN
      END
      SUBROUTINE GETIME (T1, T2, IY1, IY2, IERR)
C-----------------------------------------------------------------------
C   finds the Y pixel range in the master grid file corresponding to the
C   time range given
C   Inputs:
C      T1     R   Start time in days
C      T2     R   End time in days
C   Output:
C      IY1    I   First row inside T1 - T2 (>= T1)
C      IY2    I   Last row inside T1 - T2  (<  T2)
C      IERR   I   Error code: 0 => ok
C                    1 => rows not found
C                    2 => between cracks
C-----------------------------------------------------------------------
      REAL      T1, T2
      INTEGER   IY1, IY2, IERR
C
      INTEGER   I, NY, J
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IERR = 1
      NY = CATIMG(KINAX+1) + 1
      IF ((T1.GT.MTIMES(NY+1)) .OR. (T2.LT.MTIMES(1))) GO TO 999
C                                       find start
      DO 10 I = 2,NY
         IF (MTIMES(I).GT.T1) GO TO 20
 10      CONTINUE
      I = NY
 20   IY1 = I - 1
      IY2 = I - 1
      IERR = 0
C                                       2 sources same time
      IF (T2.LE.T1) THEN
         IF (MTIMES(IY1).EQ.MTIMES(IY1-1)) THEN
            IY1 = IY1 - 1
            IY2 = IY2 - 1
            END IF
C                                       time advances
      ELSE
         J = IY1 + 1
         NY = NY - 1
         IF (J.LE.NY) THEN
            DO 30 I = J,NY
               IF (MTIMES(I).GE.T2) GO TO 999
               IY2 = I
 30            CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPFCOP (LUN, VOL, CNO, VER, CATBLK, FCNUM, LASTR, BUF,
     *   IERR)
C-----------------------------------------------------------------------
C   creates and/or opens for writing (and reading) a specified FC table
C   for Flag Commands from SPFLG
C   Inputs:
C      LUN     I         Logical unit number to use
C      VOL     I         Disk number
C      CNO     I         Catalog number
C   In/out:
C      VER     I         Input: desired version number 0 -> highest
C                           existing or new
C                        Output: that used
C      CATBLK  I(256)    File catalog header block
C   Output:
C      FCNUM   I         Highest current flag command number
C      LASTR   I         Highest current record written
C      BUF     I(512)    Required for later calls to TABIO
C      IERR    I         Error codes from TABINI or TABIO
C-----------------------------------------------------------------------
      INTEGER   LUN, VOL, CNO, VER, CATBLK(256), FCNUM, LASTR,
     *   BUF(512), IERR
C
      INTEGER   IRNO, NKEY, NREC, ITITLE(8), LBUF(256), CCODE(17), NCOL,
     *   RECORD(35), NUMBP
      HOLLERITH HTITLE(8)
      CHARACTER TTITLE*32, CTITLE(17)*8, UNITS(17)*8, TITLE*24
      REAL      RECORR(35)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITITLE, HTITLE),  (RECORR, RECORD)
      DATA TTITLE /'AIPS SPFLG FLAG COMMAND TABLE   '/
      DATA CTITLE /'FLAGNUMB', 'FLAGOPER', 'FLAGTIME', 'FLAGBL  ',
     *   'FLAGCHAN', 'FLAGIF  ', 'FLAGSTOK', 'FLAGSOUR', 'CLIPRANG',
     *   'TVTYPE  ', 'TVCHAN  ', 'TVIF    ', 'TVSTOKES', 'TVWINDOW',
     *   'TVTIMAVG', 'TVSCAN  ', 'REASON'/
      DATA UNITS /2*' ', 'DAYS', 3*' ', 'BIT MASK', ' ', 'FLUX ',
     *   3*' ', 'STOKES', ' ', 2*'SECONDS ', ' '/
      DATA CCODE /14, 83, 22, 24, 24, 24, 43, 14, 22, 14, 14, 14, 14,
     *   44, 12, 12, 243/
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = 17
      NKEY = 1
      NREC = 500
      CALL FILL (256, 0, LBUF)
      CALL COPY (NCOL, CCODE, LBUF(129))
C                                       Version number
      IF (VER.LE.0) CALL FNDEXT ('FC', CATBLK, VER)
C                                       create/open
      CALL TABINI ('WRIT', 'FC', VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, LBUF, BUF, IERR)
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, VER
         CALL MSGWRT (8)
C                                       pre-existing file
      ELSE IF (IERR.EQ.0) THEN
         NUMBP = LBUF(1)
         IF (BUF(5).GT.0) THEN
            CALL TABIO ('READ', 0, BUF(5), RECORR, BUF, IERR)
            FCNUM = RECORD(NUMBP)
            LASTR = BUF(5)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1010) IERR, VER, BUF(5)
               CALL MSGWRT (8)
               END IF
         ELSE
            FCNUM = 0
            LASTR = 0
            END IF
C                                       New file created
      ELSE
         FCNUM = 0
         LASTR = 0
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, HTITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
C                                       write units
         DO 30 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, HTITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUF(101))
         END IF
      IERR = MAX (0, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING Flag-Command TABLE VERSION',I4)
 1010 FORMAT ('ERROR',I5,' READING Flag-Command TABLE VERSION',I4,
     *   ' RECORD',I8)
      END
      SUBROUTINE DOSTKS (ISTVAL, STKFLG, ISTLO, ISTHI)
C-----------------------------------------------------------------------
C   returns the DO loop limits for the Stokes flag loop on the uv file
C   Not all Stokes in between are necessarily to be accessed.
C   Inputs:
C      ISTVAL   I(4)   Stokes vales of 2 planes of master grid
C      STKFLG   C*4    Flag command string
C   Output:
C      ISTLO    I      Low limit over Stokes axis (0 => none)
C      ISTHI    I      Upper limit of Stokes loop (0 => none)
C-----------------------------------------------------------------------
      INTEGER   ISTVAL(4), ISTLO, ISTHI
      CHARACTER STKFLG*4
C
      LOGICAL   DOIT, DOSTOK
      INTEGER   I
C-----------------------------------------------------------------------
      ISTLO = 0
      ISTHI = 0
      DO 10 I = 1,4
         DOIT = DOSTOK (ISTVAL, STKFLG, I)
         IF (DOIT) THEN
            IF (ISTLO.EQ.0) ISTLO = I
            ISTHI = I
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      LOGICAL FUNCTION DOSTOK (ISTVAL, STKFLG, IST)
C-----------------------------------------------------------------------
C   returns whether the current Stokes flag pattern applies to this
C   stokes axis position
C   Inputs:
C      ISTVAL   I(4)   Stokes values in master file
C      STKFLG   C*4    Flag command string (1's and 0's only)
C      IST      I      Current stokes axis position
C   Output:
C      DOSTOK   L      Current Stokes included in STKFLG ?
C-----------------------------------------------------------------------
      INTEGER   ISTVAL(4), IST
      CHARACTER STKFLG*4
C
      INTEGER   I
C-----------------------------------------------------------------------
      I = ABS (ISTVAL(IST))
      IF (I.GT.4) I = I - 4
      DOSTOK = (I.GT.0) .AND. (STKFLG(I:I).EQ.'1')
C
 999  RETURN
      END
      SUBROUTINE MKSTOK (STRANS, ISTVAL, USTFLG, STKFLG, IRET)
C-----------------------------------------------------------------------
C   Converts the user's character string for Stokes flag into 1'a and
C   0's if possible and reasonable.  Note that the 1 and 0 string
C   assumes that the first correlator is I, RR, or XX.  Finding the
C   correct flag for the current actual Stokes is done by DOSTOK in
C   IBLED and by the flag routines in the calibration package.
C   Inputs:
C      ISTVAL   I(4)   Stokes values in master file
C      USTFLG   C*4    User's flag command string
C   Output:
C      STKFLG   C*4    Flag command string: 1s and 0s
C      IRET     I      Error code: 0 ok, 1 unrecognized string
C-----------------------------------------------------------------------
      INTEGER   STRANS, ISTVAL(4), IRET
      CHARACTER USTFLG*4, STKFLG*4
C
      CHARACTER CHSTO1(15)*4, CHSTOI(7)*4, CHSTOR(9)*4, CHSTOX(9)*4
      INTEGER   I
      DATA CHSTO1 /'1000', '0100', '0010', '0001', '1100', '1010',
     *   '1001', '0110', '0101', '0011', '1110', '1101', '1011', '0111',
     *   '1111'/
      DATA CHSTOI /'I', 'Q', 'U', 'V', 'IQU', 'IQUV', 'IV'/
      DATA CHSTOR / 'RR', 'LL', 'RL', 'LR', 'HALF', 'NOLL', 'NORR',
     *   'RRLL', 'RLLR'/
      DATA CHSTOX / 'XX', 'YY', 'XY', 'YX', 'HALF', 'NOYY', 'NOXX',
     *   'XXYY', 'XYYX'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       tranlated => FULL always
      IF (STRANS.NE.0) THEN
         STKFLG = '1111'
         USTFLG = 'FULL'
         GO TO 999
         END IF
C                                       already 1s and 0s
      DO 10 I = 1,15
         IF (USTFLG.EQ.CHSTO1(I)) THEN
            STKFLG = USTFLG
            GO TO 999
            END IF
 10      CONTINUE
C                                       FULL
      IF (USTFLG.EQ.'FULL') THEN
         STKFLG = '1111'
         GO TO 999
         END IF
C                                       check it out
      IF (ISTVAL(1).GT.0) THEN
         DO 20 I = 1,7
            IF (USTFLG.EQ.CHSTOI(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.5) STKFLG = CHSTO1(11)
               IF (I.EQ.6) STKFLG = CHSTO1(15)
               GO TO 999
               END IF
 20         CONTINUE
      ELSE IF (ISTVAL(1).LE.-5) THEN
         DO 30 I = 1,9
            IF (USTFLG.EQ.CHSTOX(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.6) STKFLG = CHSTO1(13)
               IF (I.EQ.7) STKFLG = CHSTO1(14)
               IF (I.EQ.8) STKFLG = CHSTO1(5)
               IF (I.EQ.9) STKFLG = CHSTO1(10)
               GO TO 999
               END IF
 30         CONTINUE
      ELSE
         DO 40 I = 1,7
            IF (USTFLG.EQ.CHSTOR(I)) THEN
               STKFLG = CHSTO1(I)
               IF (I.EQ.6) STKFLG = CHSTO1(13)
               IF (I.EQ.7) STKFLG = CHSTO1(14)
               IF (I.EQ.8) STKFLG = CHSTO1(5)
               IF (I.EQ.9) STKFLG = CHSTO1(10)
               GO TO 999
               END IF
 40         CONTINUE
         END IF
      IRET = 1
C
 999  RETURN
      END
      SUBROUTINE CVSTOK (INT, INSTOK, OUT, OUTSTK)
C-----------------------------------------------------------------------
C   converts a Stokes flag mask (0s and 1s) from 1st Stokes type to 2nd
C   Inputs:
C      INT      I     Input type: ICOR0 value
C      INSTOK   C*4   Input stokes flag mask
C      OUT      I     Output type: ICOR0 value for output
C   Outputs:
C     OUTSTK    C*4   Stokes flag mask to use with output type
C-----------------------------------------------------------------------
      INTEGER   INT, OUT
      CHARACTER INSTOK*4, OUTSTK*4
C
C-----------------------------------------------------------------------
      OUTSTK = '0000'
C                                       input I type
      IF (INT.GT.0) THEN
         IF (OUT.GT.0) THEN
            OUTSTK = INSTOK
         ELSE IF (OUT.GE.-4) THEN
            IF (INSTOK(1:1).EQ.'1') OUTSTK(1:2) = '11'
            IF (INSTOK(2:2).EQ.'1') OUTSTK(3:4) = '11'
            IF (INSTOK(3:3).EQ.'1') OUTSTK(3:4) = '11'
            IF (INSTOK(4:4).EQ.'1') OUTSTK(1:2) = '11'
         ELSE
            IF (INSTOK(1:3).NE.'000') OUTSTK(1:2) = '11'
            IF (INSTOK(2:4).NE.'000') OUTSTK(3:4) = '11'
            END IF
C                                       input RR type
      ELSE IF (INT.GE.-4) THEN
         IF (OUT.GT.0) THEN
            IF (INSTOK(1:2).EQ.'11') OUTSTK(1:1) = '1'
            IF ((INSTOK(1:1).EQ.'1') .OR. (INSTOK(2:2).EQ.'1'))
     *         OUTSTK(4:4) = '1'
            IF ((INSTOK(3:3).EQ.'1') .OR. (INSTOK(4:4).EQ.'1'))
     *         OUTSTK(2:3) = '11'
         ELSE IF (OUT.GE.-4) THEN
            OUTSTK = INSTOK
         ELSE
            IF (INSTOK.NE.'0000') OUTSTK = '1111'
            END IF
C                                       input XX type
      ELSE
         IF (OUT.GT.0) THEN
            IF ((INSTOK(1:1).EQ.'1') .OR. (INSTOK(2:2).EQ.'1'))
     *         OUTSTK(1:1) = '1'
            IF ((INSTOK(3:3).EQ.'1') .OR. (INSTOK(4:4).EQ.'1'))
     *         OUTSTK(4:4) = '1'
            IF (INSTOK.NE.'0000') OUTSTK(2:3) = '11'
         ELSE IF (OUT.GE.-4) THEN
            IF (INSTOK.NE.'0000') OUTSTK = '1111'
         ELSE
            OUTSTK = INSTOK
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPFBLT (VOL, CNO, MBL, NOANTS, CATBLK, IERR)
C-----------------------------------------------------------------------
C   Create and write or read the table which converts pixel number to
C   baseline numbers.
C   Inputs:
C      VOL      I        Disk number
C      CNO      I        Catalog number
C   In/out:
C      MBL      I        Number of baselines
C      NOANTS   I(3,*)   Baseline numbers: iant, jant, bl#
C      CATBLK   I(256)   Image header
C   Output:
C      IERR     I        Error code
C-----------------------------------------------------------------------
      INTEGER   VOL, CNO, MBL, NOANTS(3,*), CATBLK(256), IERR
C
      INTEGER   IRNO, NKEY, NREC, ITITLE(8), LBUF(256), CCODE(3), NCOL,
     *   BUF(512), VER, LUN
      HOLLERITH HTITLE(8)
      CHARACTER TTITLE*32, CTITLE(3)*8, UNITS(3)*8, TITLE*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (ITITLE, HTITLE)
      DATA TTITLE /'AIPS SPFLG BASELINE LIST TABLE'/
      DATA CTITLE /'ANTENNA1', 'ANTENNA2', 'BASELINE'/
      DATA UNITS /3*' '/
      DATA CCODE /3*14/
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Init parameters
      NCOL = 3
      NKEY = 1
      NREC = MBL
      IF (NREC.LE.0) NREC = 1
      CALL FILL (256, 0, LBUF)
      CALL COPY (NCOL, CCODE, LBUF(129))
C                                       Version number
      VER = 1
C                                       create/open
      CALL TABINI ('WRIT', 'BL', VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, LBUF, BUF, IERR)
C                                       Error
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, VER
         GO TO 990
C                                       pre-existing file
      ELSE IF (IERR.EQ.0) THEN
         MBL = BUF(5)
         IF (MBL.GT.0) THEN
            DO 10 IRNO = 1,MBL
               CALL TABIO ('READ', 0, IRNO, NOANTS(1,IRNO), BUF, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1005) IERR, 'READ', VER, IRNO
                  GO TO 990
                  END IF
 10            CONTINUE
         ELSE
            WRITE (MSGTXT,1010)
            IERR = 8
            GO TO 990
            END IF
C                                       New file created
      ELSE
C                                       write column titles
         DO 20 IRNO = 1,NCOL
            TITLE = CTITLE(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 3, IRNO, ITITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 20         CONTINUE
C                                       write units
         DO 30 IRNO = 1,NCOL
            TITLE = UNITS(IRNO)
            CALL CHR2H (24, TITLE, 1, HTITLE)
            CALL TABIO ('WRIT', 4, IRNO, ITITLE, BUF, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
C                                       table title
         CALL CHR2H (32, TTITLE, 1, HTITLE)
         CALL COPY (8, ITITLE, BUF(101))
C                                       write data
         DO 40 IRNO = 1,MBL
            CALL TABIO ('WRIT', 0, IRNO, NOANTS(1,IRNO), BUF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1005) IERR, 'WRIT', VER, IRNO
               GO TO 990
               END IF
 40         CONTINUE
         END IF
      CALL TABIO ('CLOS', 0, IRNO, NOANTS(1,IRNO), BUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) IERR, 'CLOS', VER, IRNO
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING Baseline-list TABLE VERSION',I4)
 1005 FORMAT ('ERROR',I5,1X,A4,'ING Baseline-list TABLE VERSION',I4,
     *   ' RECORD',I8)
 1010 FORMAT ('BASELINE-LIST TABLE HAS NO ROWS => THERE ARE NO DATA')
      END
      SUBROUTINE SPFWED (DOIT, PLTYPE, IPL, SCRTCH, RBUF, IRET)
C-----------------------------------------------------------------------
C   writes or erases a step wedge from SPFLG's display
C   Inputs:
C      DOWEDG   L      T => write a wedge, f => erase one
C      PLTYPE   I      Type of current TV image: 1 amp, 2 phase, ...
C      IPL      I      TV plane to use
C   Output:
C      SCRTCH   I(*)   Buffer for TV use
C      RBUF     R(*)   Real data buffer
C      IRET     I      Error code: 0 okay, < 0 warning, > 0 quit
C-----------------------------------------------------------------------
      LOGICAL   DOIT
      INTEGER   PLTYPE, IPL, SCRTCH(*), IRET
      REAL      RBUF(*)
C
      INTEGER   IERR, LPL, NROW, NPIX, IX0, IY0, I
      LOGICAL   UNIQUE, ABOVE
      REAL      X, Y
      CHARACTER LINTYP*2, CORTYP(10)*8
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA CORTYP /'AMPLTUDE', 'PHASE   ', 'RMS AMPL', 'RMS/MEAN',
     *   'RMS VAMP', 'RMS/VAVG', 'VEC DIFF', 'AMP DIFF', 'PHS DIFF',
     *   '????????'/
C-----------------------------------------------------------------------
C                                       Write a wedge
      IF (DOIT) THEN
         NROW = 16
         CALL YFIND (NGRAY, 'MA', LPL, UNIQUE, CATBLK, SCRTCH, IERR)
         IF ((IERR.NE.0) .OR. (LPL.NE.IPL)) THEN
            IRET = -1
            IF (IERR.EQ.3) IRET = 3
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         IY0 = CATBLK(IICOR+1) - CSIZTV(2)/2. - NROW
         IF (IY0.LT.2) THEN
            IY0 = CATBLK(IICOR+3) + CSIZTV(2)/2. + 0.5
            ABOVE = .TRUE.
         ELSE
            ABOVE = .FALSE.
            END IF
         IF (IY0+NROW.GE.MAXXTV(2)-2) THEN
            IRET = -1
            WRITE (MSGTXT,1005)
            GO TO 990
            END IF
         NPIX = CATBLK(IICOR+2) - CATBLK(IICOR) + 1
         IF (NPIX.LT.128) THEN
            CATBLK(IICOR) = CATBLK(IICOR) - (128 - NPIX) / 2
            IF (CATBLK(IICOR).LT.1) CATBLK(IICOR) = 1
            CATBLK(IICOR+2) = CATBLK(IICOR) + 127
            IF (CATBLK(IICOR+2).GT.MAXXTV(1)) THEN
               CATBLK(IICOR+2) = MAXXTV(1)
               CATBLK(IICOR) = CATBLK(IICOR+2) - 127
               END IF
            NPIX = CATBLK(IICOR+2) - CATBLK(IICOR) + 1
            END IF
         IX0 = CATBLK(IICOR)
         IF (CATR(IRRAN).EQ.CATR(IRRAN+1)) THEN
            CATR(IRRAN) = CATR(IRRAN) - 0.5
            CATR(IRRAN+1) = CATR(IRRAN+1) + 0.5
            END IF
         CATR(KRDMN) = CATR(IRRAN)
         CATR(KRDMX) = CATR(IRRAN+1)
         X = CATR(KRDMN)
         Y = CATR(KRDMX)
         Y = (Y - X) / (NPIX - 1)
         IF (Y.EQ.0.0) CATR(IRRAN) = 0.0
         IF (Y.EQ.0.0) CATR(IRRAN+1) = 10.0
         DO 10 I = 1,NPIX
            RBUF(I) = X + (I-1.0)*Y
 10         CONTINUE
C                                        Scale buffer like image
         CALL H2CHR (2, 1, CATH(IITRA), LINTYP)
         CALL ISCALE (LINTYP, MAXINT, CATR(IRRAN), NPIX, 1, RBUF,
     *      SCRTCH)
C                                       header stuff (non-zero)
         CALL CHR2H (2, 'WE', KHPTYO, CATH(KHPTY))
         LPL = PLTYPE
         IF ((LPL.LT.1) .OR. (LPL.GT.9)) LPL = 10
         CALL CHR2H (8, CORTYP(LPL), 1, CATH(KHCTP))
C                                       Erase a wedge
      ELSE
         CALL YFIND (NGRAY, 'WE', LPL, UNIQUE, CATBLK, SCRTCH, IERR)
         IF ((IERR.NE.0) .OR. (LPL.NE.IPL)) THEN
            IRET = -1
            IF (IERR.EQ.3) IRET = 3
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         IY0 = CATBLK(IICOR+1)
         NROW = CATBLK(IICOR+3) - IY0 + 1
         IX0 = CATBLK(IICOR)
         NPIX = CATBLK(IICOR+2) - IX0 + 1
         CALL FILL (NPIX, 0, SCRTCH)
C                                       header stuff (zero)
         CALL RFILL (5, HBLANK, CATH(KHIMN))
         CATBLK(KIIMS) = 0
         CATBLK(IIVOL) = 0
         CATBLK(IICNO) = 0
         CATR(KRDMX) = SCRTCH(NPIX)
         CATR(KRDMN) = SCRTCH(1)
         CATR(IRRAN) = CATR(KRDMN)
         CATR(IRRAN+1) = CATR(KRDMX)
         CALL RFILL (2, HBLANK, CATH(KHBUN))
         CALL RCOPY (2, CATH(KHBUN), CATH(KHCTP))
         CATBLK(KINIT) = 0
         CALL CHR2H (2, 'ZZ', KHPTYO, CATH(KHPTY))
         END IF
C                                       rest of header
      CATBLK(IICOR  ) = IX0
      CATBLK(IICOR+1) = IY0
      CATBLK(IICOR+2) = IX0 + NPIX - 1
      CATBLK(IICOR+3) = IY0 + NROW - 1
      CALL COPY (4, CATBLK(IICOR), CATBLK(IIWIN))
      CATR(KRCRP) = (CATBLK(IICOR) + CATBLK(IICOR+2)) / 2.0
      CATD(KDCRV) = (CATR(KRDMX) + CATR(KRDMN)) / 2.0
      CATR(KRCIC) = (CATR(KRDMX) - CATR(KRDMN)) / (CATBLK(IICOR+2) -
     *   CATBLK(IICOR))
      I = 2 * (KICTPN-1)
      CALL RFILL (I, HBLANK, CATH(KHCTP+2))
      IF (ABOVE) THEN
         CALL CHR2H (4, 'WETT', 1, CATH(KHCTP+I))
      ELSE
         CALL CHR2H (4, 'WEBB', 1, CATH(KHCTP+I))
         END IF
      CATR(KRCIC+1) = 0.0
      CATR(KRCRP+1) = CATBLK(IICOR+1) - 1
      CATD(KDCRV+1) = 0.0
C                                        Load to TV
      IY0 = IY0 - 1
      DO 110 I = 1,NROW
         IY0 = IY0 + 1
         CALL YIMGIO ('WRIT', IPL, IX0, IY0, 0, NPIX, SCRTCH, IERR)
         IF (IERR.GT.0) THEN
            IRET = IERR
            IF (IRET.EQ.2) IRET = -1
            WRITE (MSGTXT,1110) IERR, IY0
            GO TO 990
            END IF
 110     CONTINUE
C                                        Update catalog
      CALL YCWRIT (IPL, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFWED: ERROR FINDING IMAGE IN CATALOG',I5)
 1005 FORMAT ('SPFWED: CAN''T FIT THE WEDGE ON THE TV AROUND THE',
     *   ' CURRENT IMAGE WINDOW')
 1100 FORMAT ('SPFWED: ERROR FINDING WEDGE IN CATALOG',I5)
 1110 FORMAT ('SPFWED: YIMGIO ERROR',I5,' ROW',I5)
      END
      SUBROUTINE SPFLAB (IPL, IMGWIN, SCRTCH, RBUF, IRET)
C-----------------------------------------------------------------------
C   SPFLAB draws labels in graphics plane 4.
C   Inputs:
C      IPL      I      Image plane containing image and wedge
C      IMGWIN   I(4)   BLC,TRC of loaded image
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IPL, IMGWIN(4), SCRTCH(*), IRET
      REAL      RBUF(*)
C
      INTEGER   IGR, IERR, CATHDR(256), ILAB, LPL, IY0, IXCRN(4), NX,
     *   IYCRN(4), NY, I, IDX, IYL, IX0, NC, NCTI, NI, IA, I2, NCHAN,
     *   OXCRN(5), OYCRN(5), IXINC, LBIF, LEIF, LBCHAN, LECHAN, IL, IH
      REAL      TEMP
      LOGICAL   UNIQUE, ABOVE, DOGRID, IBELOW
      DOUBLE PRECISION T, DT
      CHARACTER STRING*20
      INCLUDE 'SPFLG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
      IRET = 0
      LOCNUM = 1
      IF (NGRAPH.LT.4) GO TO 999
      IGR = 4 + NGRAY
      NCHAN = ECHAN - BCHAN + 1
      LBIF = (IMGWIN(1) - 1) / NCHAN + BIF
      LEIF = (IMGWIN(3) - 1) / NCHAN + BIF
C                                       turn off
      IF (.NOT.DOLABL) THEN
         CALL YZERO (IGR, IRET)
         GPH4OK = .FALSE.
         CALL YSLECT ('OFFF', IGR, 0, SCRTCH, IRET)
C                                       turn on
      ELSE IF (GPH4OK) THEN
         CALL YSLECT ('ONNN', IGR, 0, SCRTCH, IRET)
C                                       redo
      ELSE
         GPH4OK = .TRUE.
         CALL YZERO (IGR, IRET)
         IF (IRET.GT.0) GO TO 999
         CALL YSLECT ('ONNN', IGR, 0, SCRTCH, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       image header needed now
         CALL YFIND (NGRAY, 'MA', LPL, UNIQUE, CATHDR, SCRTCH, IERR)
         IF ((IERR.NE.0) .OR. (LPL.NE.IPL)) THEN
            IRET = -1
            IF (IERR.EQ.3) IRET = 3
            WRITE (MSGTXT,1000) IERR, 'FINDING IMAGE HEADER FROM TV'
            GO TO 990
            END IF
C                                       wedge label
         IBELOW = .TRUE.
         IF (DOWEDG) THEN
            CALL YFIND (NGRAY, 'WE', LPL, UNIQUE, CATBLK, SCRTCH, IERR)
            IF ((IERR.NE.0) .OR. (LPL.NE.IPL)) THEN
               IRET = -1
               IF (IERR.EQ.3) IRET = 3
               WRITE (MSGTXT,1000) IERR, 'FINDING WEDGE TO LABEL'
               GO TO 990
               END IF
            ABOVE = CATBLK(IICOR+1).GT.CATHDR(IICOR+1)
            IBELOW = ABOVE
C                                       fit?
            IY0 = CATBLK(IICOR+1) - 1.5 * CSIZTV(2)
            IF (ABOVE) IY0 = CATBLK(IICOR+3) + 0.5*CSIZTV(2) + 0.5
            IF ((IY0.LT.1) .OR. (IY0+CSIZTV(2).GT.MAXXTV(2))) THEN
               MSGTXT = 'SPFLAB: NOT ROOM FOR WEDGE LABEL'
               CALL MSGWRT (6)
               GO TO 100
               END IF
            ILAB = 7
            DOGRID = .TRUE.
            LABTYP(LOCNUM) = 0
            IGR = 4
            CALL IAXIS1 (RBUF, ILAB, IGR, 1, DOGRID, IERR)
            END IF
C                                       label image
 100     CALL COPY (256, CATHDR, CATBLK)
         OXCRN(1) = MAX (1, CATBLK(IICOR) - 1)
         OXCRN(2) = MIN (MAXXTV(1), CATBLK(IICOR+2) + 1)
         OXCRN(3) = OXCRN(2)
         OXCRN(4) = OXCRN(1)
         OXCRN(5) = OXCRN(1)
         OYCRN(1) = MAX (1, CATBLK(IICOR+1) - 1)
         OYCRN(2) = OYCRN(1)
         OYCRN(3) = MIN (MAXXTV(2), CATBLK(IICOR+3) + 1)
         OYCRN(4) = OYCRN(3)
         OYCRN(5) = OYCRN(1)
         IGR = 4 + NGRAY
         CALL IMVECT ('ONNN', IGR, 5, OXCRN, OYCRN, RBUF, IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
C                                       Y axis
         NY = CATBLK(KINAX+1)
         DT = (TIMES(NY) - TIMES(1))
         IF (DT.GT.2.) THEN
            DT = 3600.0D0 * 24.0
            NC = 3
            NI = 1
         ELSE
            DT = DT * 24.0D0
            IF (DT.GT.20.) THEN
               DT = 36000.D0
               NC = 5
               NI = 1
            ELSE IF (DT.GT.3.) THEN
               DT = 3600.D0
               NI = 4
               NC = 5
            ELSE
               DT = DT * 60.D0
               NI = 4
               IF (DT.GT.30) THEN
                  DT = 600.D0
                  NC = 8
               ELSE IF (DT.GT.3.) THEN
                  DT = 60.D0
                  NC = 8
               ELSE
                  DT = DT * 60.D0
                  NC = 11 + 2*TFORM
                  IF (DT.GT.20) THEN
                     DT = 10.0D0
                  ELSE
                     DT = 1.0D0
                     END IF
                  END IF
               END IF
            END IF
         IX0 = CATBLK(IICOR) - (NC-NI+1.5)*CSIZTV(1)
         DT = DT / (24.0D0 * 3600.0D0)
         I = TIMES(1) / DT + 0.0001
         T = I * DT
         IDX = (CATBLK(IICOR+2) - CATBLK(IICOR)) / 30.0 + 0.5
         IXCRN(1) = OXCRN(1)
         IXCRN(2) = IXCRN(1) + IDX
         IXCRN(3) = OXCRN(2)
         IXCRN(4) = IXCRN(3) - IDX
         IYL = -1000
 110     T = T + DT
         IF (T.LE.TIMES(NY)) THEN
            DO 120 I = 1,NY-1
               IF ((T.GE.TIMES(I)) .AND. (T.LE.TIMES(I+1))) THEN
                  IYCRN(1) = CATBLK(IICOR+1) + I - 1
                  IYCRN(2) = IYCRN(1)
                  IYCRN(3) = IYCRN(1)
                  IYCRN(4) = IYCRN(1)
                  CALL IMVECT ('ONNN', IGR, 2, IXCRN(1), IYCRN(1), RBUF,
     *               IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  CALL IMVECT ('ONNN', IGR, 2, IXCRN(3), IYCRN(3), RBUF,
     *               IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  IF (IX0.GT.CSIZTV(1)/2) THEN
                     TEMP = T
                     CALL TORMAT (TEMP, TFORM, STRING, NCTI)
                     IY0 = IYCRN(1) - CSIZTV(2)/2
                     IF (IY0.GT.IYL+1) THEN
                        IF ((IX0.GE.1) .AND. (IY0.GE.1) .AND.
     *                     (IX0.LE.MAXXTV(1)-(NC-NI+1)*CSIZTV(1)) .AND.
     *                     (IY0.LE.MAXXTV(2)-CSIZTV(2))) THEN
                           CALL IMCHAR (IGR, IX0, IY0, 0, 0,
     *                        STRING(NI:NC), SCRTCH, IRET)
                           IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                           END IF
                        IYL = IY0 + CSIZTV(2)
                        END IF
                     END IF
                  GO TO 110
                  END IF
 120           CONTINUE
            END IF
C                                       X axis
         IY0 = CATBLK(IICOR+1) - 1.5 * CSIZTV(2)
         IF (.NOT.IBELOW) IY0 = CATBLK(IICOR+3) + 0.5 * CSIZTV(2)
         IXINC = CATBLK(IICOR+2) - CATBLK(IICOR)
         IXINC = MAX (1, IXINC)
         IXINC = (CATBLK(IIWIN+2) - CATBLK(IIWIN)) / IXINC
         IXINC = MAX (1, IXINC)
         IF ((IY0.GT.1) .AND. (IY0+CSIZTV(2).LE.MAXXTV(2))) THEN
            IDX = (CATBLK(IICOR+3) - CATBLK(IICOR+1)) / 30.0 + 0.5
            IYCRN(1) = OYCRN(1)
            IYCRN(2) = IYCRN(1) + IDX
            IYCRN(3) = OYCRN(3)
            IYCRN(4) = IYCRN(3) - IDX
            NX = CATBLK(KINAX)
            IYL = -1000
            IF (NCHAN.GT.600) THEN
               DT = 200
            ELSE IF (NCHAN.GT.300) THEN
               DT = 100
            ELSE IF (NCHAN.GT.60) THEN
               DT = 20
            ELSE
               DT = 10
               END IF
            DO 140 I2 = LBIF,LEIF
               IF (I2.EQ.LBIF) THEN
                  LBCHAN = BCHAN + IMGWIN(1) - 1 - (I2-BIF)*NCHAN
               ELSE
                  LBCHAN = BCHAN
                  END IF
               IF (I2.EQ.LEIF) THEN
                  LECHAN = IMGWIN(3) - (LEIF-BIF)*NCHAN + BCHAN - 1
               ELSE
                  LECHAN = ECHAN
                  END IF
               IA = LBCHAN / DT
               T = IA * DT
               IF (T.LT.LBCHAN) T = T + DT
               T = T - DT
 130           T = T + DT
               IF (T.LE.LECHAN) THEN
                  I = (T - BCHAN + (I2-BIF) * NCHAN -
     *               (IMGWIN(1)-1)) / IXINC
                  IXCRN(1) = CATBLK(IICOR) + I
                  IXCRN(2) = IXCRN(1)
                  IXCRN(3) = IXCRN(1)
                  IXCRN(4) = IXCRN(1)
                  CALL IMVECT ('ONNN', IGR, 2, IXCRN(1), IYCRN(1), RBUF,
     *               IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  CALL IMVECT ('ONNN', IGR, 2, IXCRN(3), IYCRN(3), RBUF,
     *               IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  I = T + 0.1
                  WRITE (STRING,1125) I
                  CALL CHTRIM (STRING, 8, STRING, NI)
                  IX0 = (0.5 - NI) * CSIZTV(1) + IXCRN(1)
                  IF (IX0.GT.IYL) THEN
                     IF ((IX0.GE.1) .AND. (IY0.GE.1) .AND.
     *                  (IX0.LE.MAXXTV(1)-NI*CSIZTV(1)) .AND.
     *                  (IY0.LE.MAXXTV(2)-CSIZTV(2))) THEN
                        CALL IMCHAR (IGR, IX0, IY0, 0, 0, STRING(:NI),
     *                     SCRTCH, IRET)
                        IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                        END IF
                     IYL = IX0 + (NI+1)*CSIZTV(1)
                     END IF
                  GO TO 130
                  END IF
 140           CONTINUE
            IF (BIF.NE.EIF) THEN
               IYCRN(1) = OYCRN(3)
               IYCRN(2) = MIN (MAXXTV(2), IYCRN(1)+3*CSIZTV(2))
               IYCRN(3) = OYCRN(1)
               IYCRN(4) = MAX (1, IYCRN(3)-4*CSIZTV(2))
               IY0 = OYCRN(1) - 3*CSIZTV(2)
               IYL = -1000
               IA = OYCRN(3) + 0.5 * CSIZTV(2) + 0.5
               IF (IA+CSIZTV(2).GE.MAXXTV(2)) IA = 2 * MAXXTV(2)
               IF (DOWEDG) THEN
                  IF (IBELOW) THEN
                     IA = 2 * MAXXTV(2)
                  ELSE
                     IY0 = -1000
                     IA = IA + 1.33 * CSIZTV(2)
                     END IF
                  END IF
               DO 150 I2 = LBIF,LEIF
                  IF (I2.EQ.LBIF) THEN
                     LBCHAN = BCHAN + IMGWIN(1) - 1 - (I2-BIF)*NCHAN
                  ELSE
                     LBCHAN = BCHAN
                     END IF
                  IF (I2.EQ.LEIF) THEN
                     LECHAN = IMGWIN(3) - (LEIF-BIF)*NCHAN + BCHAN - 1
                  ELSE
                     LECHAN = ECHAN
                     END IF
                  IF (I2.NE.LBIF) THEN
                     I = ((I2-BIF) * NCHAN - (IMGWIN(1)-1)) / IXINC
                     IL = I
                     IXCRN(1) = CATBLK(IICOR) + I
                     IXCRN(2) = IXCRN(1)
                     IXCRN(3) = IXCRN(1)
                     IXCRN(4) = IXCRN(1)
                     CALL IMVECT ('ONNN', IGR, 2, IXCRN(1), IYCRN(1),
     *                  RBUF, IRET)
                     IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                     CALL IMVECT ('ONNN', IGR, 2, IXCRN(3), IYCRN(3),
     *                  RBUF, IRET)
                     IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  ELSE
                     IL = 1
                     END IF
                  IF (I2.NE.LEIF) THEN
                     I = (ECHAN - BCHAN + (I2-BIF) * NCHAN -
     *                  (IMGWIN(1)-1)) / IXINC
                     IH = I
                     IXCRN(1) = CATBLK(IICOR) + I
                     IXCRN(2) = IXCRN(1)
                     IXCRN(3) = IXCRN(1)
                     IXCRN(4) = IXCRN(1)
                     CALL IMVECT ('ONNN', IGR, 2, IXCRN(1), IYCRN(1),
     *                  RBUF, IRET)
                     IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                     CALL IMVECT ('ONNN', IGR, 2, IXCRN(3), IYCRN(3),
     *                  RBUF, IRET)
                     IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                  ELSE
                     IH = (LECHAN - BCHAN + (I2-BIF) * NCHAN -
     *                  (IMGWIN(1)-1)) / IXINC
                     END IF
                  I = (IH + IL) / 2
                  WRITE (STRING,1125) I2
                  CALL CHTRIM (STRING, 8, STRING, NI)
                  IX0 = -NI * 0.5 * CSIZTV(1) + CATBLK(IICOR) + I
                  IF (IX0.GT.IYL) THEN
                     IF ((IY0.GT.1) .AND. (IY0.LT.MAXXTV(2)-CSIZTV(2)))
     *                  THEN
                        CALL IMCHAR (IGR, IX0, IY0, 0, 0, STRING(:NI),
     *                     SCRTCH, IRET)
                        IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                        END IF
                     IF ((IA.GT.1) .AND. (IA.LT.MAXXTV(2)-CSIZTV(2)))
     *                  THEN
                        CALL IMCHAR (IGR, IX0, IA, 0, 0, STRING(:NI),
     *                     SCRTCH, IRET)
                        IF ((IRET.NE.0) .AND. (IRET.NE.2)) GO TO 999
                        END IF
                     IYL = IX0 + (NI+1)*CSIZTV(1)
                     END IF
 150              CONTINUE
               END IF
            END IF
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPFLAB: ERROR',I3,' ON ',A)
 1125 FORMAT (I8)
      END
      SUBROUTINE SPFOAD (IPL, IMGWIN, DIMBUF, ABUF, IOBUF, SCRTCH,
     *   SBUFF, SB, NBUFF, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C   loads the image smoothing across LSMOO times and converting to
C   currently desired type of display
C   Inputs:
C      IPL     I            channel to use
C      IMGWIN  I(4)         window into raw image
C      DIMBUF   I           dimension of SBUFF, NBUFF, 2nd axis of SB -1
C      ABUF     I           dimension of 3rd axis of SB
C      IOBUF    I           dimension of BUFF1, BUFF2, BUFF3
C   Output:
C      SCRTCH   I(*)        scratch buffer
C      SBUFF    R(DIMBUF)   summing buffer
C      SB       R(3,(DIMBUF+1),ABUF)   circular buffer for rolling
C                           window averaging
C      NBUFF    I(DIMBUF)   counting buffer
C      BUFF1    R(IOBUF)    IO buffer
C      BUFF2    R(IOBUF)    IO buffer
C      BUFF3    R(IOBUF)    IO buffer
C      IRET     I           error code: > 0 => TV error, > 100 IO error
C                              = -99 => no valid data found (warning)
C   In/Out: INCLUDE 'SPFLG' - many parameters used
C      returns TIMES(32769) list of times by TV-file row
C   Expected values:
C      DIMBUF = MAXCHA + 10
C      ABUF = 50
C      IOBUF = UVBFSS
C-----------------------------------------------------------------------
      INTEGER   IPL, IMGWIN(4), DIMBUF, ABUF, IOBUF, SCRTCH(*), IRET
      REAL      SBUFF(DIMBUF), SB(3,(DIMBUF+1),ABUF), BUFF1(IOBUF),
     *   BUFF2(IOBUF), BUFF3(IOBUF)
      INTEGER   NBUFF(DIMBUF)
C
      CHARACTER PHNAME*48, CUNITS(2)*8, TRTYP(4)*2
      HOLLERITH CATH(256)
      INTEGER   NX, NY, TVWIN(4), LUN1, LUN2, FIND1, FIND2, I, IZ, NBYT,
     *   IBLKOF, IDEPTH(5), IX, IY, IR, BIND1, BIND2, IWIN(4), IJ, NIN,
     *   COUNT, IERR, IMCORN(4), INC(2), NINY, IROUND, LOOP, NTIMS, I1,
     *   I2, ISOU, LSOU, NBLN, LUN4, FIND4, BIND4, IK, JL, JJ, J, NSB,
     *   NWRIT, NSROWN(2,32768), J1, NS, INLSC, LTRTYP, NCHAN,
     *   CATSAV(256)
      REAL      LBLC(7), LTRC(7), CATR(256), TEMP, TEMP2, TEMP3, RMAX,
     *   RMIN, TIM, LTIM, DELTIM, T1, T2, CONFUN(1001)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T, DOIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SPFLG.INC'
      REAL      IMBUF(8192), IMBUFS(8192)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA T /.TRUE./
      DATA LUN1, LUN2, LUN4 /16,17,18/
      DATA CUNITS /'DEGREES ','RATIO   '/
      DATA TRTYP /'LN','LG','SQ','L2'/
C-----------------------------------------------------------------------
      INLSC = LSCAN
      IF ((ABUF.LT.25) .OR. (DIMBUF.LT.MAXCHA)) THEN
         I = MAXCHA + 10
         WRITE (MSGTXT,1000) ABUF, DIMBUF
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      MSGTXT = 'First compute the smoothed image of desired type'
      CALL MSGWRT (1)
      CALL COPY (256, CATBLK, CATSAV)
C                                       build catalog header
      DELTIM = (CATIR(KRCIC+1) * LSMOO - 0.0001) / (24. * 3600.)
      CALL COPY (256, CATIMG, CATBLK)
      CATR(KRCRP) = (CATIR(KRCRP)-4)/3 - IMGWIN(1) + 5.0
      CATR(KRCRP+1) = CATIR(KRCRP+1) - IMGWIN(2) + 1
      CATR(KRCRP+2) = CATIR(KRCRP+2) - LSTOKS + 1
      CATR(KRCRP+3) = 1.0
      CATD(KDCRV+3) = NOANTS(3,LBASL)
      CATR(KRCRP+1) = (CATR(KRCRP+1) - 1.) / LSMOO + 1.0 -
     *   (LSMOO-1.)/(2.*LSMOO)
      CATR(KRCIC) = 3.0 * CATIR(KRCIC)
      CATR(KRCIC+1) = CATIR(KRCIC+1) * LSMOO
      LTRTYP = MAX (1, MIN (4, ITRTYP))
      NCHAN = ECHAN - BCHAN + 1
C                                       compute # rows out
C                                       and pointers to rows
      I1 = IMGWIN(2)
      I2 = IMGWIN(4)
      IF (LSMOO.LE.1) THEN
         NY = 0
         DO 10 I = I1,I2
            NY = NY + 1
            SCRTCH(NY) = I
 10         CONTINUE
C                                       multi-source is messy
      ELSE
         NY = 0
         IZ = 0
         LTIM = -1.E5
         LSOU = -2
         DO 20 I = I1,I2
            ISOU = MSOU(I)
            TIM = MTIMES(I)
            IF (IZ.LE.0) THEN
               NY = NY + 1
               IZ = 1
               SCRTCH(NY) = I
               LSOU = ISOU
               LTIM = TIM
               IF (LSOU.LT.0) THEN
                  NBLN = NBLN + 1
               ELSE
                  NBLN = 0
                  END IF
            ELSE
C                                       if within time and same source
C                                       or blanked source
               IF ((TIM-LTIM.LT.DELTIM) .AND. ((ISOU.EQ.LSOU) .OR.
     *            ((ISOU.LT.0) .AND. (LSOU.GE.0)))) THEN
                  IZ = MOD (IZ + 1, LSMOO)
C                                       skip excess blanks
               ELSE IF ((ISOU.LT.0) .AND. (NBLN.GT.4)) THEN
                  IZ = 1
C                                       new row
               ELSE
                  NY = NY + 1
                  SCRTCH(NY) = I
                  IZ = 1
                  LSOU = ISOU
                  LTIM = TIM
                  IF (LSOU.LT.0) THEN
                     NBLN = NBLN + 1
                  ELSE
                     NBLN = 0
                     END IF
                  END IF
               END IF
 20         CONTINUE
         END IF
      SCRTCH(NY+1) = IMGWIN(4) + 1
C                                       open output file
      CALL ZPHFIL ('SC', SCRVOL(TVFILE), SCRCNO(TVFILE), 1, PHNAME,
     *   IERR)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(TVFILE), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN', 'OUTPUT TV', IERR
         GO TO 990
         END IF
C                                       open smoothed file
      IF (LTYPE.GT.6) THEN
         CALL ZPHFIL ('SC', SCRVOL(SCFILE), SCRCNO(SCFILE), 1, PHNAME,
     *      IERR)
         CALL ZOPEN (LUN4, FIND4, SCRVOL(SCFILE), PHNAME, T, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'OPEN', 'INPUT SMOOTHED', IERR
            GO TO 990
            END IF
         END IF
C                                       open input file
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IERR)
      CALL ZOPEN (LUN1, FIND1, DISKOU, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN', 'INPUT MASTER', IERR
         GO TO 990
         END IF
C                                       input dimensions
      NX = IMGWIN(3) - IMGWIN(1) + 1
      NINY = IMGWIN(4) - IMGWIN(2) + 1
      NBYT = 2 * IOBUF
      IDEPTH(1) = LSTOKS
      IDEPTH(2) = LBASL
      IDEPTH(3) = 1
      IDEPTH(4) = 1
      IDEPTH(5) = 1
      IBLKOF = 1
C                                       write smoothed file
      IF (LTYPE.GT.6) THEN
C                                       list of rows for smooth
 100     LSCAN = MAX (LSCAN, LSMOO+2)
         JJ = LSCAN - LSMOO
         TEMP = CATIR(KRCIC+1) * JJ / (24. * 3600.)
         DO 120 IY = 1,NY
            I1 = SCRTCH(IY)
            I2 = SCRTCH(IY+1) - 1
            NS = MSOU(I1)
            T1 = MTIMES(I1) - TEMP
            T2 = MTIMES(I2+1) + TEMP
            DO 105 J = 1,JJ
               IF (MTIMES(I1-J).LT.T1) GO TO 106
               IF ((MSOU(I1-J).NE.NS) .AND. (MSOU(I1-J).GE.0)) GO TO 106
 105           CONTINUE
            J = JJ + 1
 106        NSROWN(1,IY) = I1 - J + 1
            DO 110 J = 1,JJ
               IF (MTIMES(I2+J).GT.T2) GO TO 111
               IF ((MSOU(I2+J).NE.NS) .AND. (MSOU(I2+J).GE.0)) GO TO 111
 110           CONTINUE
            J = JJ + 1
 111        NSROWN(2,IY) = I2 + J - 1
 115        IF (NSROWN(2,IY)-NSROWN(1,IY)+1.LE.LSCAN) GO TO 120
               NSROWN(2,IY) = NSROWN(2,IY) - 1
               IF (NSROWN(2,IY)-NSROWN(1,IY)+1.LE.LSCAN) GO TO 120
                  NSROWN(1,IY) = NSROWN(1,IY) + 1
                  GO TO 115
 120        CONTINUE
C                                       check that buffers will hold
         JL = 0
         DO 130 IY = 1,NY
            J1 = IY + 1
            DO 125 J = J1,NY
               IF (NSROWN(2,IY)-NSROWN(1,J).GE.0) THEN
                  JL = MAX (JL, J-IY)
               ELSE
                  GO TO 130
                  END IF
 125           CONTINUE
 130        CONTINUE
         JL = JL + 1
         IF (JL.GT.ABUF) THEN
            I = LSCAN - JL + ABUF
            IF (I.LT.LSMOO+2) THEN
               WRITE (MSGTXT,1130) LSMOO, LSCAN
               CALL MSGWRT (6)
               IRET = -99
               CALL ZCLOSE (LUN1, FIND1, IERR)
               CALL ZCLOSE (LUN2, FIND2, IERR)
               CALL ZCLOSE (LUN4, FIND4, IERR)
               GO TO 999
               END IF
            LSCAN = I
            GO TO 100
            END IF
         WRITE (MSGTXT,1131) LSCAN
         CALL MSGWRT (2)
         MSGTXT = 'This was reduced due to memory limitations'
         IF (INLSC.GT.LSCAN) CALL MSGWRT (2)
C                                       windows
         NSB = 3 * NX + 3
         CATBLK(KINAX) = NSB
         CATBLK(KINAX+1) = NY
         CATBLK(KINAX+2) = 1
         CATBLK(KINAX+3) = 1
         CATBLK(KINAX+4) = 1
         TVWIN(1) = 1
         TVWIN(2) = 1
         TVWIN(3) = NSB
         TVWIN(4) = NY
         IWIN(1) = 1
         IWIN(2) = MAX (1, NSROWN(1,1))
         IWIN(3) = 3 * IMGWIN(3) + 3
         IWIN(4) = MIN (CATIMG(KINAX+1), NSROWN(2,NY))
         NINY = IWIN(4) - IWIN(2) + 1
C                                       init the IO
         CALL MINIT ('WRIT', LUN4, FIND4, TVWIN(3), NY, TVWIN, BUFF3,
     *      NBYT, IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'INIT', 'OUTPUT SMOOTHED', IERR
            GO TO 990
            END IF
C                                       init IO from input
         CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *      IERR)
         IBLKOF = IBLKOF + 1
         CALL MINIT ('READ', LUN1, FIND1, CATIMG(KINAX),
     *      CATIMG(KINAX+1), IWIN, BUFF1, NBYT, IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'INIT', 'INPUT MASTER', IERR
            GO TO 990
            END IF
C                                       init circular buffers
         DO 135 IY = 1,ABUF
            CALL RFILL (NSB, 0.0, SB(1,1,IY))
 135        CONTINUE
         NWRIT = 1
         JL = 1
C                                       read input master grid loop
         DO 160 IY = 1,NINY
            IK = IY - 1 + IWIN(2)
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'READ', 'INPUT MASTER', IERR, IK
               GO TO 990
               END IF
C                                       any buffers done?
 140        IF (IK.GT.NSROWN(2,NWRIT)) THEN
C                                       "write" row
               CALL MDISK ('WRIT', LUN4, FIND4, BUFF3, BIND4, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1010) 'WRITE', 'OUTPUT SMOOTH', IERR,
     *               NWRIT
                  GO TO 990
                  END IF
C                                       move data to out buffer
               CALL RCOPY (NSB, SB(1,1,JL), BUFF3(BIND4))
               CALL RFILL (NSB, 0.0, SB(1,1,JL))
               NWRIT = NWRIT + 1
C                                       advance row
               IF (NWRIT.GT.NY) GO TO 180
               JL = MOD (JL, ABUF) + 1
               GO TO 140
               END IF
C                                       add into buffers
            DO 155 J = 1,ABUF
               J1 = NWRIT + J - 1
               IF ((IK.GE.NSROWN(1,J1)) .AND. (IK.LE.NSROWN(2,J1))) THEN
                  JJ = JL + J - 1
                  IF (JJ.GT.ABUF) JJ = JJ - ABUF
                  IZ = 3*IMGWIN(1) + BIND1
C                                       scalar average
                  IF (LTYPE.EQ.8) THEN
                     DO 145 IX = 1,NX
                        IF (BUFF1(IZ+2).EQ.0.0) THEN
                           IJ = IX + 1
                           TEMP = BUFF1(IZ)*BUFF1(IZ) +
     *                        BUFF1(IZ+1)*BUFF1(IZ+1)
                           SB(1,IJ,JJ) = SB(1,IJ,JJ) + SQRT (TEMP)
                           SB(3,IJ,JJ) = SB(3,IJ,JJ) + 1.0
                           END IF
                        IZ = IZ + 3
 145                    CONTINUE
C                                       vector average
                  ELSE
                     DO 150 IX = 1,NX
                        IF (BUFF1(IZ+2).EQ.0.0) THEN
                           IJ = IX + 1
                           SB(1,IJ,JJ) = SB(1,IJ,JJ) + BUFF1(IZ)
                           SB(2,IJ,JJ) = SB(2,IJ,JJ) + BUFF1(IZ+1)
                           SB(3,IJ,JJ) = SB(3,IJ,JJ) + 1.0
                           END IF
                        IZ = IZ + 3
 150                    CONTINUE
                     END IF
                  END IF
 155           CONTINUE
 160        CONTINUE
C                                       more rows to run to output
 170     IF (NWRIT.GT.NY) GO TO 180
C                                       "write" row
            CALL MDISK ('WRIT', LUN4, FIND4, BUFF3, BIND4, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'WRITE', 'OUTPUT SMOOTH', IERR,
     *            NWRIT
               GO TO 990
               END IF
C                                       move data to out buffer
            CALL RCOPY (NSB, SB(1,1,JL), BUFF3(BIND4))
            NWRIT = NWRIT + 1
C                                       advance row
            JL = MOD (JL, ABUF) + 1
            GO TO 170
C                                       last flush
 180     CALL MDISK ('FINI', LUN4, FIND4, BUFF3, BIND4, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'FINISH', 'OUTPUT SMOOTH', IERR, NWRIT
            GO TO 990
            END IF
C                                       end smoothing
         END IF
C                                       final output windows
      IBLKOF = 1
      CATBLK(KINAX) = NX + 3
      CATBLK(KINAX+1) = NY
      CATBLK(KINAX+2) = 1
      CATBLK(KINAX+3) = 1
      CATBLK(KINAX+4) = 1
      TVWIN(1) = 1
      TVWIN(2) = 1
      TVWIN(3) = NX + 3
      TVWIN(4) = NY
      IWIN(1) = 1
      IWIN(2) = IMGWIN(2)
      IWIN(3) = 3 * IMGWIN(3) + 3
      IWIN(4) = IMGWIN(4)
C                                       init IO to output
      CALL MINIT ('WRIT', LUN2, FIND2, TVWIN(3), NY, TVWIN, BUFF2, NBYT,
     *   IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'INIT', 'OUTPUT TV', IERR
         GO TO 990
         END IF
      IF (LTYPE.GT.6) THEN
         TVWIN(3) = (TVWIN(3) - 2) * 3
         CALL MINIT ('READ', LUN4, FIND4, TVWIN(3), NY, TVWIN, BUFF3,
     *      NBYT, IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'INIT', 'INPUT SMOOTHED', IERR
            GO TO 990
            END IF
         END IF
C                                       init IO from input
      CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF, IERR)
      IBLKOF = IBLKOF + 1
      CALL MINIT ('READ', LUN1, FIND1, CATIMG(KINAX), CATIMG(KINAX+1),
     *   IWIN, BUFF1, NBYT, IBLKOF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'INIT', 'INPUT MASTER', IERR
         GO TO 990
         END IF
C                                       loop over output rows
      RMAX = -1.E10
      RMIN = 1.E10
      NIN = 0
      IF (LCSMOT.NE.CSMOTH) CONFUN(1) = -1.
      DO 800 IY = 1,NY
C                                       "write" row
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) 'WRITE', 'OUTPUT TV', IERR, IY
            GO TO 990
            END IF
C                                       get smoothed row
         IF (LTYPE.GT.6) THEN
            CALL MDISK ('READ', LUN4, FIND4, BUFF3, BIND4, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'WRITE', 'OUTPUT TV', IERR, IY
               GO TO 990
               END IF
            END IF
C                                       Zero accumulators
         BUFF2(BIND2) = 0.0
         BUFF2(BIND2+NX+1) = 0.0
         BUFF2(BIND2+NX+2) = 0.0
         NTIMS = 0
         IF ((LTYPE.EQ.5) .OR. (LTYPE.EQ.6)) THEN
            DO 550 LOOP = 1,NX
               BUFF2(BIND2+LOOP) = 0.0
               SBUFF(LOOP) = 0.0
               NBUFF(LOOP) = 0
               IMBUF(LOOP) = 0.0
               IMBUFS(LOOP) = 0.0
 550           CONTINUE
         ELSE
            DO 551 LOOP = 1,NX
               BUFF2(BIND2+LOOP) = 0.0
               SBUFF(LOOP) = 0.0
               NBUFF(LOOP) = 0
 551           CONTINUE
            END IF
C                                       loop over rows in sum
         LSOU = -2
         I1 = SCRTCH(IY)
         I2 = SCRTCH(IY+1) - 1
         DO 600 IR = I1,I2
            NIN = NIN + 1
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) 'READ', 'INPUT MASTER', IERR, IR
               GO TO 990
               END IF
C                                       Source number
            IF (BUFF1(BIND1).NE.FBLANK) THEN
               ISOU = IROUND (BUFF1(BIND1))
            ELSE
               ISOU = -1
               END IF
            IF (LSOU.LT.0) LSOU = ISOU
            BUFF2(BIND2) = LSOU
C                                       time
            IF (BUFF1(BIND1+1).NE.FBLANK) THEN
               IF (NTIMS.LE.0) THEN
                  BUFF2(BIND2+1) = BUFF1(BIND1+1)
                  BUFF2(BIND2+2) = BUFF1(BIND1+2)
               ELSE
                  BUFF2(BIND2+1) = MIN (BUFF2(BIND2+1),
     *               BUFF1(BIND1+1))
                  BUFF2(BIND2+2) = MAX (BUFF2(BIND2+2),
     *               BUFF1(BIND1+2))
                  END IF
               NTIMS = NTIMS + 1
               END IF
C                                       sum row
            IZ = 3*IMGWIN(1) + BIND1
C                                       Data in antenna order
C                                       RMS values
      INCLUDE 'INCS:ZVND.INC'
            IF ((LTYPE.EQ.3) .OR. (LTYPE.EQ.4)) THEN
               DO 560 IX = 1,NX
                  IF (BUFF1(IZ+2).EQ.0.0) THEN
                     IJ = BIND2 + IX + 2
                     NBUFF(IX) = NBUFF(IX) + 1
                     TEMP = BUFF1(IZ)*BUFF1(IZ) +
     *                  BUFF1(IZ+1)*BUFF1(IZ+1)
                     BUFF2(IJ) = BUFF2(IJ) + SQRT(TEMP)
                     SBUFF(IX) = SBUFF(IX) + TEMP
                     END IF
                  IZ = IZ + 3
 560              CONTINUE
C                                       Vector RMS values
            ELSE IF ((LTYPE.EQ.5) .OR. (LTYPE.EQ.6)) THEN
      INCLUDE 'INCS:ZVND.INC'
               DO 565 IX = 1,NX
                  IF (BUFF1(IZ+2).EQ.0.0) THEN
                     IJ = BIND2 + IX + 2
                     NBUFF(IX) = NBUFF(IX) + 1
                     BUFF2(IJ) = BUFF2(IJ) + BUFF1(IZ)
                     SBUFF(IX) = SBUFF(IX) + BUFF1(IZ)*BUFF1(IZ)
                     IMBUF(IX) = IMBUF(IX) + BUFF1(IZ+1)
                     IMBUFS(IX) = IMBUFS(IX) + BUFF1(IZ+1) * BUFF1(IZ+1)
                     END IF
                  IZ = IZ + 3
 565              CONTINUE
C                                       other data types
            ELSE
      INCLUDE 'INCS:ZVND.INC'
               DO 570 IX = 1,NX
                  IF (BUFF1(IZ+2).EQ.0.0) THEN
                     IJ = BIND2 + IX + 2
                     NBUFF(IX) = NBUFF(IX) + 1
                     BUFF2(IJ) = BUFF2(IJ) + BUFF1(IZ)
                     SBUFF(IX) = SBUFF(IX) + BUFF1(IZ+1)
                     END IF
                  IZ = IZ + 3
 570              CONTINUE
               END IF
 600        CONTINUE
C                                       average and convert:
         IJ = BIND2 + 3
         IK = BIND4 + 3
         IF (IY.LE.32768) THEN
            IF (NTIMS.GT.0) THEN
               TIMES(IY) = BUFF2(BIND2+1)
               TIMES(IY+1) = BUFF2(BIND2+2)
            ELSE IF (IY.GT.1) THEN
               BUFF2(BIND2+1) = TIMES(IY)
               TIMES(IY+1) = TIMES(IY) + CATR(KRCIC+1) / (24. * 3600.)
               BUFF2(BIND2+2) = TIMES(IY+1)
            ELSE
               TIMES(IY) = START - CATR(KRCIC+1) / (24. * 3600. * 2.)
               BUFF2(BIND2+1) = TIMES(IY)
               TIMES(IY+1) = TIMES(IY) + CATR(KRCIC+1) / (24. * 3600.)
               BUFF2(BIND2+2) = TIMES(IY+1)
               END IF
         ELSE
            IF (NTIMS.LE.0) THEN
               TEMP = TIMES(32768) + CATR(KRCIC+1) * (IY - 32768.) /
     *            (24. * 3600.)
               BUFF2(BIND2+1) = TEMP
               BUFF2(BIND2+2) = TEMP + CATR(KRCIC+1) / (24. * 3600.)
               END IF
            END IF
C                                       Amplitude
         IF (LTYPE.EQ.1) THEN
      INCLUDE 'INCS:ZVND.INC'
            IF (TYPUVD.LE.0) THEN
               DO 650 IX = 1,NX
                  IF (NBUFF(IX).GT.0) THEN
                     COUNT = MAX (1, NBUFF(IX))
                     BUFF2(IJ) = SQRT (BUFF2(IJ)*BUFF2(IJ) +
     *                  SBUFF(IX)*SBUFF(IX)) / COUNT
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  IJ = IJ + 1
 650              CONTINUE
            ELSE
      INCLUDE 'INCS:ZVND.INC'
               DO 655 IX = 1,NX
                  IF (NBUFF(IX).GT.0) THEN
                     COUNT = MAX (1, NBUFF(IX))
                     BUFF2(IJ) = BUFF2(IJ) / COUNT
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  IJ = IJ + 1
 655              CONTINUE
               END IF
C                                        phase
         ELSE IF (LTYPE.EQ.2) THEN
            LTRTYP = 1
      INCLUDE 'INCS:ZVND.INC'
            DO 670 IX = 1,NX
               IF ((BUFF2(IJ).NE.0.0) .OR. (SBUFF(IX).NE.0.0)) THEN
                  BUFF2(IJ) = ATAN2 (SBUFF(IX), BUFF2(IJ)) * 57.29578
               ELSE
                  BUFF2(IJ) = FBLANK
                  END IF
               IJ = IJ + 1
 670           CONTINUE
C                                       RMS
      INCLUDE 'INCS:ZVND.INC'
         ELSE IF (LTYPE.EQ.3) THEN
            DO 680 IX = 1,NX
               IF (NBUFF(IX).GT.1) THEN
                  TEMP2 = BUFF2(IJ) / NBUFF(IX)
                  SBUFF(IX) = SBUFF(IX) / NBUFF(IX)
                  BUFF2(IJ) = SQRT (MAX (0.0, SBUFF(IX) - TEMP2*TEMP2))
               ELSE
                  IF (NBUFF(IX).EQ.1) THEN
                     BUFF2(IJ) = 0.0
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  END IF
               IJ = IJ + 1
 680           CONTINUE
C                                       Vector RMS
         ELSE IF (LTYPE.EQ.5) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 690 IX = 1,NX
               IF (NBUFF(IX).GT.1) THEN
                  BUFF2(IJ) = BUFF2(IJ) / NBUFF(IX)
                  SBUFF(IX) = SBUFF(IX) / NBUFF(IX)
                  IMBUF(IX) = IMBUF(IX) / NBUFF(IX)
                  IMBUFS(IX) = IMBUFS(IX) /NBUFF(IX)
                  TEMP2 = BUFF2(IJ)
                  BUFF2(IJ) = SBUFF(IX) - TEMP2*TEMP2 + IMBUFS(IX) -
     *               IMBUF(IX)*IMBUF(IX)
                  BUFF2(IJ) = SQRT (MAX (0.0, BUFF2(IJ)))
               ELSE
                  IF (NBUFF(IX).EQ.1) THEN
                     BUFF2(IJ) = 0.0
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  END IF
               IJ = IJ + 1
 690           CONTINUE
C                                       RMS / mean
         ELSE IF (LTYPE.EQ.4) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 700 IX = 1,NX
               IF (NBUFF(IX).GT.1) THEN
                  TEMP2 = BUFF2(IJ) / NBUFF(IX)
                  SBUFF(IX) = SBUFF(IX) / NBUFF(IX)
                  IF (ABS (TEMP2).LT.1.0E-10) TEMP2 = 1.0
                  TEMP3 = SBUFF(IX) / (TEMP2*TEMP2) - 1.0
                  BUFF2(IJ) = SQRT (MAX (0.0, TEMP3))
               ELSE
                  IF (NBUFF(IX).EQ.1) THEN
                     BUFF2(IJ) = 0.0
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  END IF
               IJ = IJ + 1
 700           CONTINUE
C                                       RMS / mean
         ELSE IF (LTYPE.EQ.6) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 710 IX = 1,NX
               IF (NBUFF(IX).GT.1) THEN
                  BUFF2(IJ) = BUFF2(IJ) / NBUFF(IX)
                  SBUFF(IX) = SBUFF(IX) / NBUFF(IX)
                  IMBUF(IX) = IMBUF(IX) / NBUFF(IX)
                  IMBUFS(IX) = IMBUFS(IX) /NBUFF(IX)
                  TEMP2 = BUFF2(IJ) * BUFF2(IJ) + IMBUF(IX) * IMBUF(IX)
                  BUFF2(IJ) = SBUFF(IX) + IMBUFS(IX) - TEMP2
                  IF (TEMP2.LT.1.E-10) TEMP2 = 1.0
                  BUFF2(IJ) = SQRT (MAX (0.0, BUFF2(IJ)/TEMP2))
               ELSE
                  IF (NBUFF(IX).EQ.1) THEN
                     BUFF2(IJ) = 0.0
                  ELSE
                     BUFF2(IJ) = FBLANK
                     END IF
                  END IF
               IJ = IJ + 1
 710           CONTINUE
C                                       Vector amplitude difference
      INCLUDE 'INCS:ZVND.INC'
         ELSE IF (LTYPE.EQ.7) THEN
            DO 720 IX = 1,NX
               IF ((NBUFF(IX).GT.0) .AND. (BUFF3(IK+2).GT.0.0)) THEN
                  COUNT = MAX (1, NBUFF(IX))
                  TEMP = BUFF2(IJ) / COUNT - BUFF3(IK) / BUFF3(IK+2)
                  TEMP2 = SBUFF(IX) / COUNT - BUFF3(IK+1) / BUFF3(IK+2)
                  BUFF2(IJ) = SQRT (TEMP*TEMP + TEMP2*TEMP2)
               ELSE
                  BUFF2(IJ) = FBLANK
                  END IF
               IJ = IJ + 1
               IK = IK + 3
 720        CONTINUE
C                                       Amplitude difference
      INCLUDE 'INCS:ZVND.INC'
         ELSE IF (LTYPE.EQ.8) THEN
            DO 730 IX = 1,NX
               IF ((NBUFF(IX).GT.0) .AND. (BUFF3(IK+2).GT.0.0)) THEN
                  COUNT = MAX (1, NBUFF(IX))
                  TEMP = SQRT (BUFF2(IJ)*BUFF2(IJ) +
     *               SBUFF(IX)*SBUFF(IX)) / COUNT -
     *               BUFF3(IK) / BUFF3(IK+2)
                  BUFF2(IJ) = ABS (TEMP)
               ELSE
                  BUFF2(IJ) = FBLANK
                  END IF
               IJ = IJ + 1
               IK = IK + 3
 730        CONTINUE
C                                       phase
         ELSE IF (LTYPE.EQ.9) THEN
            LTRTYP = 1
      INCLUDE 'INCS:ZVND.INC'
            DO 740 IX = 1,NX
               IF ((NBUFF(IX).GT.0) .AND. (BUFF3(IK+2).GT.0.0)) THEN
                  TEMP = 0.0
                  IF ((BUFF3(IK).NE.0.0) .OR. (BUFF3(IK+1).NE.0.0))
     *               TEMP = ATAN2 (BUFF3(IK+1), BUFF3(IK))
                  TEMP2 = 0.0
                  IF ((BUFF2(IJ).NE.0.0) .OR. (SBUFF(IX).NE.0.0))
     *               TEMP2 = ATAN2 (SBUFF(IX), BUFF2(IJ))
                  TEMP3 = (TEMP2 - TEMP) * 57.29578
                  IF (TEMP3.LE.-180.0) TEMP3 = TEMP3 + 360.0
                  IF (TEMP3.GT.180.0) TEMP3 = TEMP3 - 360.0
                  BUFF2(IJ) = ABS (TEMP3)
               ELSE
                  BUFF2(IJ) = FBLANK
                  END IF
               IJ = IJ + 1
               IK = IK + 3
 740           CONTINUE
            END IF
C                                       frequency smooth
         CALL SPSMOT (CSMOTH, NX, NCHAN, IMGWIN(1), BUFF2(BIND2+3),
     *      CONFUN)
C                                       Max
         IJ = BIND2 + 2
         DO 770 IX = 1,NX
            IF (BUFF2(IX+IJ).NE.FBLANK) RMAX = MAX (RMAX, BUFF2(IX+IJ))
 770        CONTINUE
C                                       Min
         DO 780 IX = 1,NX
            IF (BUFF2(IX+IJ).NE.FBLANK) RMIN = MIN (RMIN, BUFF2(IX+IJ))
 780        CONTINUE
 800     CONTINUE
      TEMP = 0.005 * (RMAX - RMIN)
      RMAX = RMAX + TEMP
      RMIN = RMIN - TEMP
      LCSMOT = CSMOTH
C                                       "write" last row
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) 'FINISH', 'OUTPUT TV', IERR
         GO TO 990
         END IF
      CALL ZCLOSE (LUN1, FIND1, IERR)
      IF (LTYPE.GT.6) CALL ZCLOSE (LUN4, FIND4, IERR)
      TIMES(NY+1) = TIMES(NY) + CATR(KRCIC+1) / (24. * 3600.)
      TIMES(NY+2) = TIMES(NY+1) + CATR(KRCIC+1) / (24. * 3600.)
      TIMES(NY+3) = TIMES(NY+2) + CATR(KRCIC+1) / (24. * 3600.)
C                                       No valid data found
      IF (RMAX.LE.RMIN) THEN
         IF ((LTYPE.GE.3) .AND. (LTYPE.LE.6)) THEN
            MSGTXT = 'SPFOAD: NO VALID RMS''S FOUND - INCREASE ' //
     *         'AVERAGING TIME'
         ELSE
            MSGTXT = 'SPFOAD: NO VALID PIXELS FOUND - RESET WINDOW OR'
     *         // ' BASELINE'
            END IF
         CALL MSGWRT (2)
         CALL ZCLOSE (LUN2, FIND2, IERR)
         CALL COPY (256, CATSAV, CATBLK)
         IRET = -99
         GO TO 999
         END IF
C                                       Finish image catalog header
      I = LTYPE
      IF ((I.EQ.5) .OR. (I.EQ.6)) I = I - 2
      IF ((I.EQ.7) .OR. (I.EQ.8)) I = 1
      IF (I.EQ.9) I = 2
      CALL RNGSET (PIXRNG(1,I), RMAX, RMIN, CATR(IRRAN))
      PIXRNG(1,5) = RMIN
      PIXRNG(2,5) = RMAX
      CATBLK(IIVOL) = 0
      CATBLK(IICNO) = 0
      CALL CHR2H (2, TRTYP(LTRTYP), 1, CATH(IITRA))
      IF ((LTYPE.EQ.2) .OR. (LTYPE.EQ.9)) CALL CHR2H (8, CUNITS(1),
     *   1, CATH(KHBUN))
      IF (LTYPE.EQ.4) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      IF (LTYPE.EQ.6) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      CALL RFILL (7, 1.0, LBLC)
      CALL RFILL (7, 1.0, LTRC)
      LBLC(1) = 4.
      LTRC(1) = NX + 3
      LTRC(2) = NY
      IJ = -1
      INC(1) = (CATBLK(KINAX)-4) / MAXXTV(1) + 1
      INC(2) = 1
C                                       Init
      TVWIN(1) = 0
      XYCENT(1) = 0
      CALL YWINDO ('READ', WINDTV, IERR)
      IF (IERR.NE.0) THEN
         WINDTV(1) = 1
         WINDTV(2) = 1
         WINDTV(3) = MAXXTV(1)
         WINDTV(4) = MAXXTV(2)
         IERR = 0
         TVWIN(2) = 0
      ELSE
         TVWIN(2) = WINDTV(2) + 16 + (MAXLAB*1.5 + 4.5)*CSIZTV(2) + 0.5
         IF (TVWIN(2)+NY-1.GT.WINDTV(4)) TVWIN(2) = 0
         IF (DOCENT.LE.0.0) THEN
            TVWIN(1) = WINDTV(3) - 6 - NX
            TVWIN(1) = MIN (TVWIN(1), MAXMEN+3+WINDTV(1))
            XYCENT(1) = TVWIN(1) + (NX-1)/2
            END IF
         END IF
      IF (TVWIN(2).LE.0) THEN
         XYCENT(2) = (WINDTV(2) + WINDTV(4)) / 2
      ELSE
         XYCENT(2) = TVWIN(2) + (NY-1) / 2
         END IF
      WRITE (MSGTXT,1800) CATR(IRRAN), CATR(IRRAN+1)
      CALL MSGWRT (2)
      IF (INC(1).GT.1) THEN
         WRITE (MSGTXT,1805) INC(1)
         CALL MSGWRT (2)
         END IF
      TEMP = 0.02 * ABS (RMAX-RMIN)
      DOIT = ABS (RMIN-CATR(IRRAN)).GT.TEMP
      DOIT = (ABS (RMAX-CATR(IRRAN+1)).GT.TEMP) .OR. DOIT
C                                       Tell range
      IF (DOIT) THEN
         WRITE (MSGTXT,1806) RMIN, RMAX
         CALL MSGWRT (2)
         END IF
      IRET = WINDTV(4)-WINDTV(2)+1
      IF (IRET.LT.NY) THEN
         WRITE (MSGTXT,1807) NY, IRET
         CALL MSGWRT (6)
         END IF
      CALL YSLECT ('ONNN', IPL, 0, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL YZERO (IPL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL YCINIT (IPL, BUFFER)
C                                       Load image on TV.
      CALL TVWIND (IJ, INC, LBLC, LTRC, IPL, TVWIN, IMCORN, IRET)
      CALL TVLOAD (LUN2, FIND2, IPL, INC, TVWIN, IMCORN, NBYT, BUFF2,
     *   IRET)
      CALL ZCLOSE (LUN2, FIND2, IX)
      IF ((IRET.EQ.0) .AND. (DOWEDG)) THEN
         CALL SPFWED (DOWEDG, LTYPE, IPL, SCRTCH, BUFF2, IRET)
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (8)
      IRET = 104
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BUFFER DIMS ABUF, DIMBUF =',I4,I7,' EXPECTED 50',I7)
 1010 FORMAT ('SPFOAD: UNABLE TO ',A,1X,A,' FILE, ERROR',2I4)
 1130 FORMAT ('SMOOTH LENGTH',I4,' TOO LONG FOR SCAN LENGTH',I4)
 1131 FORMAT ('Using scan length',I4,' for rolling-buffer average')
 1800 FORMAT ('Now load the TV memory from',1PE12.4,' TO',1PE12.4)
 1805 FORMAT ('***** WARNING: ONLY EVERY',I3,' CHANNEL IS DISPLAYED',
     *   ' *****')
 1806 FORMAT ('Current data range is',1PE12.4,' to',1PE12.4)
 1807 FORMAT ('Warning: image has',I7,' rows, TV only',I5,' rows')
      END
      SUBROUTINE SPSMOT (CW, NX, NCHAN, IMGWIN, ROW, CONFUN)
C-----------------------------------------------------------------------
C   SPSMOT does frequency smoothing
C   Inputs:
C      CW       R      FWHM in channels of Gaussian
C      NX       I      Number points in ROW
C      NCHAN    I      Number channels per IF
C      IMGWIN   I      starting spot in row
C   In/Out:
C      ROW      R(*)   Data row
C      CONFUN   R(*)   Convolving function (1) <= 0 -> recompute
C-----------------------------------------------------------------------
      INTEGER   NX, NCHAN, IMGWIN
      REAL      CW, ROW(*), CONFUN(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER   I, J, J1, J2, LC, JMIN, JMAX, IX
      REAL      V(MAXCHA), W(MAXCHA), WT, A
C-----------------------------------------------------------------------
      IF (CW.GT.0.0) THEN
         CALL RFILL (NX, 0.0, V)
         CALL RFILL (NX, 0.0, W)
         IF (CONFUN(1).LT.1.0) THEN
            A = -4.0 * LOG(2.0) / (CW * CW)
            J2 = 3. * CW + 1
            DO 10 I = 2,J2
               CONFUN(I) = EXP (A * (I-2) * (I-2))
               IF (CONFUN(I).GT.0.005) CONFUN(1) = I
 10            CONTINUE
            END IF
         JMIN = 1
         IX = IMGWIN - 1
         LC = MOD (IX, NCHAN)
         JMAX = NCHAN - LC
         DO 30 I = 1,NX
            LC = LC + 1
            IF (LC.GT.NCHAN) THEN
               LC = LC - NCHAN
               JMIN = JMIN + NCHAN
               JMAX = JMAX + NCHAN
               END IF
            J = CONFUN(1) + 0.1
            J = J - 2
            J1 = I - J
            J2 = I + J
            J1 = MAX (1, JMIN)
            J2 = MIN (NX, JMAX)
            DO 20 J = J1,J2
               IF (ROW(J).NE.FBLANK) THEN
                  WT = CONFUN (ABS(I-J)+2)
                  V(I) = V(I) + WT * ROW(J)
                  W(I) = W(I) + WT
                  END IF
 20            CONTINUE
 30         CONTINUE
         DO 40 I = 1,NX
            IF (W(I).GT.0.0) THEN
               ROW(I) = V(I) / W(I)
            ELSE
               ROW(I) = FBLANK
               END IF
 40         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE GRIDTC (DPARM, IMSIZE, MAXANS, NODATA, MSOU, MTIMES,
     *   MAXBUF, DIMBUF, IOBUF, XDOCAT, OUTNAM, OUTCLS, SEQOUT, DISKOU,
     *   CNOOUT, MXBASL, NOANTS, MBL, NPOINT, NFAIL, INSNUM, NBUFF,
     *   SBUFF, RBUFF, VBUFF, IRET)
C-----------------------------------------------------------------------
C   GRIDTC makes a gridded image of the UV data in TB order.
C   Inputs:
C      DPARM    R(10)   Control parms:
C                          (1) 0,1,2 => amp, phase, amp rms gridded
C                              3 amp rms/mean gridded
C                              4, 5 => real, imag part of vis
C                              6 => store real and imag for SPFLG
C                          (2) > scalar avg, else vector
C                          (3) > 0 B -> ant. pair (grid twice)
C                          (4) > 0 => divide amp by source flux
C                          (5) x-axis interval
C                          (6) y-axis interval
C      IMSIZE   I(2)    Output image size (x,y)
C      MAXANS   I       Size of NODATA = max # antennas
C      NODATA   I(m,m)  Count of samples by baseline: m = MAXANS
C      MXBASL   I       Dimension of NOANTS (max # baselines)
C      MAXBUF   I       Number of buffers to use
C      DIMBUF   I       Size of summing 1 buffer
C      IOBUF    I       Size of 1 IO buffer
C      XDOCAT   R       > 0 => mark the new image as permanent in the
C                       DFIL commons, else as temporary
C   In/out:
C      MSOU     I(*)    Source numbers that go with times - set to -1
C                          when times set to regular intervals
C      MTIMES   R(*)    (* = IMSIZE(2)) start times for each row
C                          MTIMES(2) < MTIMES(1) < 0 => set regular
C                          times from the first datum
C      OUTNAM   C*12    Output image name
C      OUTCLS   C*6     Output image class or 'SC  ',' '
C      SEQOUT   I       Output image seq number, if scratch, is DFIL
C                       number of SC file
C      DISKOU   I       Output image disk
C   Output:
C      CNOOUT   I       Output image catalog number
C      NOANTS   I(3,M)  m = MXBASL: (1,) IANT (2,) JANT (>= IANT),
C                          (3,) baseline number
C      MBL      I       max number baselines with data
C      NPOINT   D       Vis included in grid note double precision
C      NFAIL    I       Vis included but off the grid
C      NBUFF    I(*)    Counting buffer (DIMBUF,MAXBUF)
C      SBUFF    R(*)    Summing buffer (DIMBUF,MAXBUF)
C      RBUFF    R(*)    IO buffer (IOBUF,MAXBUF)
C      VBUFF    R(*)    IO BUFFER (3*MAXCIF+5)
C      IRET     I       Error code: 0 ok, else die
C   Expected:
C      MAXBUF = 8       max. no buffers
C      DIMBUF = MAXCHA  max channels
C      IOBUF  = UVBFSS   Words of IO buffer
C-----------------------------------------------------------------------
      INTEGER   IMSIZE(2), MAXANS, NODATA(MAXANS,MAXANS), MSOU(*),
     *   MAXBUF, DIMBUF, IOBUF, SEQOUT, DISKOU, CNOOUT, MXBASL,
     *   NOANTS(3,MXBASL), MBL, NFAIL, INSNUM, IRET
      DOUBLE PRECISION NPOINT
      REAL      DPARM(10), XDOCAT, MTIMES(*), SBUFF(DIMBUF,MAXBUF),
     *   RBUFF(IOBUF,MAXBUF), VBUFF(*)
      INTEGER   NBUFF(DIMBUF,MAXBUF)
      CHARACTER OUTNAM*12, OUTCLS*6
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MB
      PARAMETER (MB = 8)
      CHARACTER PHNAME*48, CUNITS(2)*8, CHTEMP*2, TYPTMP*2, KEYS(4)*8,
     *   CHSTOK(23)*4
      HOLLERITH CATH(256), CATSH(256), CATIH(256)
      INTEGER   LTYPE(2), I, J, K, IERR, JERR, CATSAV(256), LUNO, LIF,
     *   IBLKOF, NBYT, IDEPTH(5), NCOL, II, NROW, LR, IROW, ITYP, LCH,
     *   OLDSOU, IROUND, NUMAN(1025), ISOFF1, LBL, LNCS, IV, LNCIF,
     *   CATIMG(256), IANT, JANT, IARR, COLMUL, IVIS, ISOFF2, LSTSOU,
     *   NCOL2, LNCFIL, COLUP, LSUNUM, LLSUN, LLARR, LNCF,
     *   LBVIS, NSTOK, IB, NUMBUF, LIBL, IBUFF, LSTINC, IVISO, MROW,
     *   NBL, NNVIS, INCBUF, JBUFF, OFFBUF, MUMBUF, LUN(MB), NCOLMX,
     *   FIND(MB), IBPTR(MB), IIV, LOCS(4), VALUES(4), KEYTYP(4), NCHAN,
     *   NIF, NPTS
      LOGICAL   T, F, ISINGL, TABLE, EXIST, FITASC, MULTIS, WASSOU,
     *   ISEOF, FIRST
      REAL      CATR(256), CATSR(256), VALUE1, VALUE2, VALUE3, TEMP,
     *   CATIR(256), RMAX, RMIN, TIMLST, RPARM(20), SPIX(3)
      DOUBLE PRECISION    CATD(128), CATSD(128), CATID(128), TSIGMA,
     *   NNNV, REFREQ, REFPIX
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCHND.INC'
      COMMON /UVIMGC/ CATIMG
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),
     *   (CATSAV, CATSH, CATSR, CATSD), (CATIMG, CATIH, CATIR, CATID)
      DATA LUNO /26/
      DATA T, F /.TRUE.,.FALSE./
      DATA CUNITS /'DEGREES ','RATIO   '/
      DATA CHSTOK /'I','Q','U','V','IQU','IQUV','IV','QU', 'RR','LL',
     *   'RL','LR','RRLL','RLLR','XX','YY','XY','YX','XXYY','XYYX',
     *   'HALF','FULL','CROS'/
      DATA KEYS /'BIF','EIF','BCHAN','ECHAN'/
      DATA LOCS /1,2,3,4/
      DATA KEYTYP /4*4/
C-----------------------------------------------------------------------
      DO 10 I = 1,MB
         LUN(I) = 55 + I
 10      CONTINUE
      NCOLMX = MAXCIF
      IF ((MAXBUF.GT.MB) .OR. (DIMBUF.LT.NCOLMX)) THEN
         WRITE (MSGTXT,1000) MAXBUF, DIMBUF, NCOLMX
         CALL MSGWRT (8)
         IRET = 2
         GO TO 999
         END IF
      NCOLMX = DIMBUF
      IRET = 0
      LSTSOU = -1
      WASSOU = .FALSE.
      FIRST = (MTIMES(1).LT.0.0) .AND. (MTIMES(2).LE.MTIMES(1))
      LLSUN = INSNUM
      LLARR = 0
      OLDSOU = 0
      NPOINT = 0.0D0
      NFAIL = 0
      ITYP = IROUND (DPARM(1)) + 1
      IF ((ITYP.EQ.1) .AND. (DPARM(2).GT.0.0)) ITYP = 0
C                                       ITYP = output data type:
C                                       0 = amp (scalar average)
C                                       1 = amp, 2 = phase, 3 = rms
C                                       4 = amp rms / mean
C                                       5 = real, 6 = imag,
C                                       7 = real & imag & flag #
      IF (ITYP.EQ.7) THEN
         COLMUL = 3
         COLUP = 3
      ELSE
         COLMUL = 1
         COLUP = 0
         END IF
      LTYPE(1) = 1
      LTYPE(2) = 2
      CALL H2CHR (2, 1, CATBLK(KITYP), CHTEMP)
      IF (CHTEMP(:1).NE.'T') THEN
         WRITE (MSGTXT,1005) CHTEMP
         IRET = 2
         GO TO 990
         END IF
C                                       Open the UV file
      TYPTMP = 'UV'
      CALL MAPOPN ('READ', IUDISK, UNAME, UCLAS, IUSEQ, TYPTMP, NLUSER,
     *   LUN, FIND, IUCNO, CATBLK, VBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN INPUT UV', IRET
         IRET = 1
         GO TO 990
         END IF
C                                       and close (UVGET does open)
      CALL MAPCLS ('READ', IUDISK, IUCNO, LUN, FIND, CATBLK, F, VBUFF,
     *   IERR)
C                                       Do we want multi-source?
C                                       only for flux scaling here
      EXIST = .FALSE.
      IF (DPARM(4).GT.0.0) CALL ISTAB ('SU', IUDISK, IUCNO, 1, LUN,
     *   VBUFF, TABLE, EXIST, FITASC, IERR)
      ISINGL = (.NOT.EXIST) .OR. (IERR.NE.0)
      ISINGL = ISINGL .OR. ((ILOCSU.LT.0) .AND. (INSNUM.LE.0))
      IF (ISINGL) DPARM(4) = -1.0
      MULTIS = ILOCSU.GE.0
C                                       Get number antennas
      IF ((LTYPE(1).EQ.1) .AND. (DPARM(3).GT.0.0)) LTYPE(1) = 0
      CALL GETNAN (IUDISK, IUCNO, CATBLK, LUN, VBUFF, NUMAN, IRET)
      IF (((IRET.NE.0) .AND. (IRET.NE.10)) .OR. (NUMAN(1).LE.0)) THEN
         WRITE (MSGTXT,1015) IRET, NUMAN(1)
         IRET = 4
         GO TO 990
         END IF
      J = 0
      LIF = NUMAN(1)
      DO 15 I = 1,LIF
         NUMAN(513+I) = J
         J = J + (NUMAN(1+I) * (NUMAN(1+I) + 3)) / 2
 15      CONTINUE
      NUMAN(514+LIF) = J
C                                       Fill in antenna/baseline array
      DO 20 I = 1,MXBASL
         NOANTS(1,I) = 0
         NOANTS(2,I) = 0
         NOANTS(3,I) = 0
 20      CONTINUE
      MBL = 0
      DO 30 IANT = 1,MAXANS
         DO 25 JANT = IANT,MAXANS
            IF (NODATA(IANT,JANT).GT.0) THEN
               MBL = MBL + 1
               NOANTS(1,MBL) = IANT
               NOANTS(2,MBL) = JANT
               NOANTS(3,MBL) = NUMAN(513+SUBARR) + JANT - IANT + 1
     *            + (IANT-1) * (2*NUMAN(1+SUBARR) + 4 - IANT) / 2
               END IF
 25         CONTINUE
 30      CONTINUE
      IF (MBL.LE.0) THEN
         IRET = 8
         MSGTXT = 'GRIDTC: NO BASELINES WITH DATA HAVE BEEN FOUND'
         GO TO 990
         END IF
C                                       Build image header:
C                                       Misc.
      CALL COPY (256, CATBLK, CATSAV)
      CALL UVPGET (IRET)
      IF (ITYP.EQ.2) CALL CHR2H (8, CUNITS(1), 1, CATH(KHBUN))
      IF (ITYP.EQ.4) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      CATBLK(KIGCN) = 1
      CATBLK(KIPCN) = 0
      CATR(KRBLK) = FBLANK
      CATBLK(KIIMU) = NLUSER
      CATBLK(KITYP) = 0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C                                       Coordinates
      CATBLK(KIDIM) = 7
C                                       Baseline
      CATD(KDCRV+3) = NOANTS(3,1)
      CATBLK(KINAX+3) = MBL
      NPTS = MBL
      CATR(KRCRT+3) = 0.0
      CATR(KRCIC+3) = 1.0
      CATR(KRCRP+3) = 1.0
      CALL CHR2H (8, 'BASELINE', 1, CATH(KHCTP+6))
C                                       FREQ * IF
      NCHAN = ECHAN - BCHAN + 1
      NIF = EIF - BIF + 1
      CATBLK(KINAX) = NCHAN * NIF
      REFREQ = CATSD(KDCRV+JLOCF)
      REFPIX = CATSR(KRCRP+JLOCF)
      CATD(KDCRV) = CATSD(KDCRV+JLOCF)
      CATR(KRCIC) = CATSR(KRCIC+JLOCF) / 3.0
      CATR(KRCRP) = 3.0 * (CATSR(KRCRP+JLOCF) - BCHAN) + 4
      CATR(KRCRT) = 0.0
      NNVIS = CATBLK(KINAX)
C                                       IF
      CATR(KRCRT+4) = 0.0
      IF (JLOCIF.GE.0) THEN
         CATBLK(KINAX+4) = 1
         CATD(KDCRV+4) = CATSD(KDCRV+JLOCIF)
         CATR(KRCIC+4) = CATSR(KRCIC+JLOCIF)
         CATR(KRCRP+4) = CATSR(KRCRP+JLOCIF) - BIF + 1
         I = JLOCIF * 2
         CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+8))
         CALL CHR2H (8, 'FREQ /IF', 1, CATH(KHCTP))
      ELSE
         CATBLK(KINAX+4) = 1
         CATD(KDCRV+4) = 1.0D0
         CATR(KRCIC+4) = 1.0
         CATR(KRCRP+4) = 1.0
         CALL CHR2H (8, 'IF      ', 1, CATH(KHCTP+8))
         CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP))
         END IF
C                                       Stokes
      J = 0
      DO 35 I = 1,23
         IF (STOKES.EQ.CHSTOK(I)) J = I
 35      CONTINUE
      IF ((J.LE.0) .AND. (ICOR0.LT.-5)) J = 16
      IF ((J.LE.0) .AND. (ICOR0.LT.-4)) J = 19
      IF ((J.LE.0) .AND. (ICOR0.LT.-1)) J = 10
      IF ((J.LE.0) .AND. (ICOR0.LT.0)) J = 13
      IF ((J.LE.0) .AND. (ICOR0.GT.0)) J = 1
      IF (NCOR.EQ.1) THEN
         TEMP = CATSD(KDCRV+JLOCS) + (1.0-CATSR(KRCRP+JLOCS)) *
     *      CATSR(KRCIC+JLOCS)
         IF (TEMP.LT.-4.) THEN
            J = 10 - IROUND (TEMP)
         ELSE IF (TEMP.LT.0.) THEN
            J = 8 - IROUND (TEMP)
         ELSE
            J = IROUND (TEMP)
            END IF
      ELSE IF ((NCOR.EQ.2) .AND. (J.EQ.22)) THEN
         IF (ICOR0.LT.-4) THEN
            J = 19
            IF (ICOR0.LT.-6) J = 20
         ELSE IF (ICOR0.LT.0) THEN
            J = 13
            IF (ICOR0.LT.-2) J = 14
         ELSE
            J = 7
            END IF
         END IF
      IF (J.LE.0) J = 1
      IF (J.EQ.5) J = 1
      IF (J.EQ.6) J = 1
      IF (ICOR0.LT.-4) THEN
         IF (J.EQ.23) J = 20
         IF (J.EQ.21) J = 19
      ELSE
         IF (J.EQ.23) J = 14
         IF (J.EQ.21) J = 13
         END IF
      IF (J.LE.4) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = J
         CATR(KRCIC+2) = 1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.7) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 3.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.8) THEN
         STOKES = CHSTOK(5)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 2.0D0
         CATR(KRCIC+2) = 1.0
         ISOFF1 = 1
         ISOFF2 = 2
      ELSE IF (J.LE.12) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 8.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.13) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -1.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.14) THEN
         STOKES = CHSTOK(14)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -3.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 2
         ISOFF2 = 3
      ELSE IF (J.LE.18) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 10.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.19) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -5.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.20) THEN
         STOKES = CHSTOK(20)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -7.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 2
         ISOFF2 = 3
      ELSE IF (J.EQ.22) THEN
         STOKES = CHSTOK(22)
         CATBLK(KINAX+2) = 4
         CATD(KDCRV+2) = ICOR0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
         END IF
      NSTOK = CATBLK(KINAX+2)
      NPTS = NPTS * NSTOK
      LBVIS = 3 * NSTOK
      CATR(KRCRP+2) = 1.0
      CATR(KRCRT+2) = 0.0
      I = JLOCS * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+4))
C                                       warning message
      IF ((.NOT.DOCAL) .AND. (ICOR0.LE.0) .AND. (J.LE.8) .AND.
     *   (ILOCSU.GE.0)) THEN
         MSGTXT = 'WARNING: CONVERSION TO STOKES BEFORE CALIBRATION'
     *      // ' IS MEANINGLESS'
         CALL MSGWRT (2)
         END IF
C                                       RA, dec
      CATBLK(KINAX+5) = 1
      CATD(KDCRV+5) = CATSD(KDCRV+JLOCR)
      CATR(KRCIC+5) = CATSR(KRCIC+JLOCR)
      CATR(KRCRP+5) = CATSR(KRCRP+JLOCR)
      CATR(KRCRT+5) = CATSR(KRCRT+JLOCR)
      I = JLOCR * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+10))
      CATBLK(KINAX+6) = 1
      CATD(KDCRV+6) = CATSD(KDCRV+JLOCD)
      CATR(KRCIC+6) = CATSR(KRCIC+JLOCD)
      CATR(KRCRP+6) = CATSR(KRCRP+JLOCD)
      CATR(KRCRT+6) = CATSR(KRCRT+JLOCD)
      I = JLOCD * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+12))
C                                       2nd axis: Time
      CATBLK(KINAX+1) = IMSIZE(2)
      IF (DPARM(6).EQ.0.0) DPARM(6) = 1.0
      CATR(KRCIC+1) = ABS (DPARM(6)) / (24. * 3600.)
      TSIGMA = CATR(KRCIC+1) / 11.0D0
      CATD(KDCRV+1) = (MTIMES(1) + MTIMES(2)) / 2.0
      CATR(KRCRP+1) = 1.0
      CATR(KRCRT+1) = 0.0
      CALL CHR2H (8, 'TIME    ', 1, CATH(KHCTP+2))
      CATBLK(KINAX) = CATBLK(KINAX) * COLMUL + COLUP
      IMSIZE(1) = CATBLK(KINAX)
      IMSIZE(2) = CATBLK(KINAX+1)
      NPTS = NPTS * IMSIZE(1) * IMSIZE(2)
      MROW = CATBLK(KINAX+1)
C                                       Create output map file
      LNCFIL = 0
C                                       Cataloged MA file output
      SEQOUT = ABS (SEQOUT)
      CALL MAKOUT (UNAME, UCLAS, IUSEQ, ' ', OUTNAM, OUTCLS, SEQOUT)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       support BADDISK
      IIV = 0
      IF (DISKOU.GT.0) THEN
         IV = DISKOU
         IIV = IV
         CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
      ELSE
         DO 65 IV = 1,NVOL
            DO 60 I = 1,10
               IF (IV.EQ.IBAD(I)) GO TO 65
 60            CONTINUE
            MSGSUP = 32000
            IIV = IV
            CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
            MSGSUP = 0
            IF (IRET.NE.1) GO TO 70
 65         CONTINUE
         IRET = 1
         END IF
 70   SEQOUT = CATBLK(KIIMS)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'CREATE OUTPUT IMAGE', IRET
         IF (IRET.EQ.1) THEN
            CALL MSGWRT (8)
            MSGTXT = 'NO DISK SPACE ON ALLOWED DISKS'
            IF (IIV.LE.0) MSGTXT = 'BADDISK LEAVES NO DISKS TO USE'
            END IF
         IRET = 3
         GO TO 990
         END IF
      DISKOU = IV
C                                       mark cataloged
      IF (XDOCAT.GT.0.0) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKOU
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = 2
         LNCFIL = NCFILE
         VALUES(1) = BIF
         VALUES(2) = EIF
         VALUES(3) = BCHAN
         VALUES(4) = ECHAN
         CALL CATKEY ('WRIT', DISKOU, CNOOUT, KEYS, 4, LOCS, VALUES,
     *      KEYTYP, VBUFF, IERR)
C                                       mark as temporary
      ELSE
         NSCR = NSCR + 1
         SCRVOL(NSCR) = DISKOU
         SCRCNO(NSCR) = CNOOUT
         END IF
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IERR)
C                                       Create BL table
      CALL SPFBLT (DISKOU, CNOOUT, MBL, NOANTS, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'CREATE BASELINE TABLE', IRET
         GO TO 990
         END IF
C                                       Loop over IF, freq
      NBYT = 2 * IOBUF
      NCOL = CATBLK(KINAX)
      NCOL2 = (CATBLK(KINAX) - COLUP) / COLMUL
      CALL COPY (256, CATBLK, CATIMG)
      CALL COPY (256, CATSAV, CATBLK)
      RMAX = -1.E23
      RMIN = 1.E23
      LSTINC = CATIMG(KINAX+2)
      IB = MAXBUF / LSTINC
      NNNV = 0.0D0
C                                       Use buffers for IFs.
      NUMBUF = LSTINC
      LIBL = MAXBUF / NUMBUF
      LIBL = MIN (MBL, LIBL)
      DO 500 LBL = 1,MBL,LIBL
C                                       Init i/o to uv file
         IROW = 1
         NBL = MIN (LBL+LIBL-1, MBL)
         INCBUF = LSTINC
         MUMBUF = (NBL - LBL + 1) * INCBUF
         NUMBUF = MAX (NUMBUF, MUMBUF)
C                                       set ANTENS
         CALL FILL (50, 0, ANTENS)
         K = 0
         DO 55 LR = LBL,NBL
            DO 40 I = 1,K
               IF (ANTENS(I).EQ.NOANTS(1,LR)) GO TO 45
 40            CONTINUE
            K = K + 1
            ANTENS(K) = NOANTS(1,LR)
 45         DO 50 I = 1,K
               IF (ANTENS(I).EQ.NOANTS(2,LR)) GO TO 55
 50            CONTINUE
            K = K + 1
            ANTENS(K) = NOANTS(2,LR)
 55         CONTINUE
         IF (NPTS.GT.10000000) THEN
            WRITE (MSGTXT,1055) (NOANTS(1,LR), NOANTS(2,LR),
     *         LR = LBL,NBL)
            CALL MSGWRT (2)
            END IF
         RPARM(1) = FBLANK
         TIMLST = -1.E10
         CALL UVGET ('INIT', RPARM, VBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'INIT INPUT UV', IRET
            IRET = 5
            GO TO 990
            END IF
         LNCS = INCS
         LNCF = INCF
         LNCIF = INCIF
C                                       init image I/O
         DO 80 IBUFF = 1,MUMBUF
C                                       Open output files on first pass
            IF (LBL.EQ.1) THEN
               CALL ZOPEN (LUN(IBUFF), FIND(IBUFF), DISKOU, PHNAME, T,
     *            F, T, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'OPEN OUTPUT IMAGE', IRET
                  IRET = 4
                  GO TO 990
                  END IF
               END IF
            LR = (IBUFF-1) / LSTINC
            IDEPTH(1) = MOD (IBUFF-1, LSTINC) + 1
            IDEPTH(2) = LBL + (IBUFF-1) / LSTINC
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IERR)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('WRIT', LUN(IBUFF), FIND(IBUFF), CATIMG(KINAX),
     *         CATIMG(KINAX+1), 0, RBUFF(1,IBUFF), NBYT, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT OUTPUT IMAGE', IRET
               IRET = 4
               GO TO 990
               END IF
 80         CONTINUE
C                                       current row = 0
         NROW = 0
C                                       Read first record
 100     CALL DATGET (RPARM, VBUFF(5), TIMLST, IRET)
         ISEOF = (IRET.EQ.4) .AND. (NNNV.GT.0.0D0)
         IF ((NNNV.EQ.0.0D0) .AND. (IRET.EQ.4)) THEN
            IF (LBL.LT.MBL) ISEOF = .TRUE.
            END IF
         IF (ISEOF) GO TO 450
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'READ INPUT UV', IRET
            IRET = 5
            GO TO 990
            END IF
C                                       set regular times by default
         IF (FIRST) THEN
            II = RPARM(1+ILOCT) / TSIGMA + 0.5D0
            TEMP = II * TSIGMA
            CATID(KDCRV+1) = TEMP
            II = CATIMG(KINAX+1) + 1
            DO 105 I = 1,II
               MSOU(I) = -1
               MTIMES(I) = TEMP + (I - 1.5) * CATIR(KRCIC+1)
 105           CONTINUE
            FIRST = .FALSE.
            END IF
C                                       find row for integration
         TEMP = RPARM(1+ILOCT)
         IF ((IROW.LT.1) .OR. (IROW.GT.MROW)) IROW = 1
         DO 110 LR = IROW,MROW
            IF ((TEMP.GE.MTIMES(LR)) .AND. (TEMP.LT.MTIMES(LR+1)))
     *         GO TO 115
 110        CONTINUE
         NFAIL = NFAIL + 1
         GO TO 100
 115     IROW = LR
C                                       New row, allow new source
         IF (IROW.GT.NROW) THEN
            LLSUN = INSNUM
            LLARR = 0
            END IF
C                                       get source number
         IERR = 0
         IRET = 0
         LSUNUM = 0
         IF (MULTIS) THEN
            IF (ILOCSU.GE.0) THEN
               LSUNUM = RPARM(ILOCSU+1) + 0.1
            ELSE
               IF (NSOUWD.EQ.1) LSUNUM = SOUWAN(1)
               END IF
            END IF
C                                       check antennas
         IF (ILOCB.GE.0) THEN
            TEMP = RPARM(ILOCB+1)
            JANT = TEMP + 0.1
            IARR = 100.0 * (TEMP - JANT) + 1.5
            IANT = JANT / 256
            JANT = JANT - 256 * IANT
         ELSE
            IANT = RPARM(1+ILOCA1) + 0.1
            JANT = RPARM(1+ILOCA2) + 0.1
            IARR = RPARM(1+ILOCSA) + 0.1
            END IF
         IF (IANT.GT.JANT) THEN
            J = IANT
            IANT = JANT
            JANT = J
            END IF
C                                       Do we want it?
         DO 180 LR = LBL,NBL
            IF ((NOANTS(1,LR).EQ.IANT) .AND.
     *         (NOANTS(2,LR).EQ.JANT)) GO TO 190
 180        CONTINUE
         GO TO 100
C                                       Save record, 1=IANT, 2=JANT,
C                                       3=array, 4=time
 190     VBUFF(1) = IANT
         VBUFF(2) = JANT
         VBUFF(3) = IARR
         VBUFF(4) = RPARM(1+ILOCT)
C                                       Check source
         OFFBUF = (LR - LBL) * INCBUF
         IF (MULTIS) THEN
            IF (LLSUN.LE.0) THEN
               LLSUN = LSUNUM
               LLARR = IARR
            ELSE
               IF (IARR.LT.LLARR) THEN
                  LLSUN = LSUNUM
                  LLARR = IARR
                  END IF
C                                       Different source in integration
               IF ((IARR.EQ.LLARR) .AND. (LLSUN.NE.LSUNUM)) THEN
                  WASSOU = .TRUE.
                  GO TO 100
                  END IF
               END IF
            END IF
C                                       Vis wanted
         NPOINT = NPOINT + 1.0D0
C                                       Average, write row
         IF (IROW.GT.NROW) THEN
            CALL GTBWRT (OLDSOU, MSOU, MTIMES, NCOL2, NCOLMX, IOBUF,
     *         ITYP, LUN, FIND, F, IROW, NROW, MUMBUF, RBUFF, SBUFF,
     *         NBUFF, IBPTR, RMAX, RMIN, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1230) IRET
               IRET = 4
               GO TO 990
               END IF
            END IF
C                                       Get source flux
         IF ((.NOT.ISINGL) .AND. (LSTSOU.NE.LLSUN)) THEN
            CALL GETSOU (LLSUN, IUDISK, IUCNO, CATSAV, LUNO, IERR)
            DO 240 LIF = BIF,EIF
               IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 240           CONTINUE
            LSTSOU = LLSUN
            IF (DPARM(4).GT.1.5) THEN
               IV = IROUND (DPARM(4)) - 1
               CALL FNDSPX (IUDISK, IUCNO, LLSUN, FRQSEL, CATSAV, IV,
     *            SPIX, IERR)
            ELSE
               SPIX(1) = 0.0
               SPIX(2) = 0.0
               SPIX(3) = 0.0
               END IF
            END IF
C                                       Process visibility data.
C                                       First entry:
C                                       Loop over buffer
         DO 300 JBUFF = 1,INCBUF
            IF (INCBUF.EQ.4) THEN
               IVIS = (JBUFF - 1) * LNCS
            ELSE
               IVIS = LNCS * ISOFF1
               IF (MOD(JBUFF-1,LSTINC).EQ.1) IVIS = LNCS * ISOFF2
               END IF
            IVIS = IVIS + 5 + ((JBUFF-1)/LSTINC) * LNCIF
            IVISO = IVIS
            IBUFF = JBUFF + OFFBUF
C                                       Divide by source flux
            IF (.NOT.ISINGL) THEN
               IF (SPIX(2).EQ.0.0) THEN
                  DO 245 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN + BIF
                        VALUE1 = 1.0 / FLUX(1,LIF)
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 245                 CONTINUE
C                                       incl spectral index
               ELSE IF ((DPARM(4).LT.2.5) .OR. (SPIX(1).LE.0.0)) THEN
                  DO 246 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN
                        LCH = IV - 1 - LIF * NCHAN + BCHAN
                        LIF = LIF + BIF
                        VALUE1 = ((REFREQ + FOFF(LIF) + FINC(LIF) *
     *                     (LCH - REFPIX)) / (REFREQ + FOFF(LIF) +
     *                     FINC(LIF) * (NCHAN/2.0 - REFPIX))) ** SPIX(2)
                        VALUE1 = 1.0 / (FLUX(1,LIF) * VALUE1)
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 246                 CONTINUE
C                                       incl spectral index
               ELSE
                  DO 247 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN
                        LCH = IV - 1 - LIF * NCHAN + BCHAN
                        LIF = LIF + BIF
                        VALUE1 = (REFREQ + FOFF(LIF) + FINC(LIF) *
     *                     (LCH - REFPIX)) / 1.D9
                        VALUE1 = LOG10 (VALUE1)
                        VALUE1 = SPIX(2)*VALUE1 + SPIX(3)*VALUE1*VALUE1
                        VALUE1 = 1.0 / (SPIX(1) * (10.0**VALUE1))
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 247                 CONTINUE
                  END IF
               END IF
            IVIS = IVISO
C                                       Amp (scalar avg) or rms
            J = IBPTR(IBUFF) - COLMUL + COLUP
            K = IBPTR(IBUFF)
            IF ((ITYP.EQ.0) .OR. (ITYP.EQ.3) .OR. (ITYP.EQ.4)) THEN
               DO 250 IV = 1,NNVIS
                  J = J + COLMUL
                  IF (VBUFF(IVIS+2).GT.0.0) THEN
                     IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF)=0.0
                     VALUE1 = VBUFF(IVIS)
                     VALUE2 = VBUFF(IVIS+1)
                     VALUE3 =  SQRT (VALUE1*VALUE1 + VALUE2*VALUE2)
                     RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VALUE3
                     SBUFF(IV,IBUFF) = SBUFF(IV,IBUFF) + VALUE3*VALUE3
                     NBUFF(IV,IBUFF) = NBUFF(IV,IBUFF) + 1
                     NNNV = NNNV + 1.0D0
                     END IF
                  IVIS = IVIS + LNCF
 250              CONTINUE
C                                       other data types
            ELSE
               DO 260 IV = 1,NNVIS
                  J = J + COLMUL
                  IF (VBUFF(IVIS+2).GT.0.0) THEN
                     IF (RBUFF(J,IBUFF).EQ.FBLANK) RBUFF(J,IBUFF) = 0.0
                     RBUFF(J,IBUFF) = RBUFF(J,IBUFF) + VBUFF(IVIS)
                     SBUFF(IV,IBUFF) = SBUFF(IV,IBUFF) + VBUFF(IVIS+1)
                     NBUFF(IV,IBUFF) = NBUFF(IV,IBUFF) + 1
                     NNNV = NNNV + 1.0D0
                     END IF
                  IVIS = IVIS + LNCF
 260              CONTINUE
               END IF
C                                       Second entry:
 300        CONTINUE
C                                       Get next vis buffer load
         OLDSOU = LLSUN
         IF (.NOT.ISEOF) GO TO 100
C                                       write rest of plane(s)
 450     IROW = CATIMG(KINAX+1)
         CALL GTBWRT (OLDSOU, MSOU, MTIMES, NCOL2, NCOLMX, IOBUF, ITYP,
     *      LUN, FIND, T, IROW, NROW, MUMBUF, RBUFF, SBUFF, NBUFF,
     *      IBPTR, RMAX, RMIN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1230) IRET
            IRET = 4
            GO TO 990
            END IF
C                                       Close the uv I/O too
         CALL UVGET ('CLOS', RPARM, VBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'CLOSE INPUT UV', IRET
            GO TO 990
            END IF
C                                       End of IF loop
 500     CONTINUE
C                                       Close file(s)
      DO 520 I = 1,NUMBUF
         CALL ZCLOSE (LUN(I), FIND(I), JERR)
 520     CONTINUE
C                                       finish the header
      CALL COPY (256, CATIMG, CATBLK)
      CATR(KRDMX) = RMAX
      CATR(KRDMN) = RMIN
      CATD(KDCRV+1) = CATD(KDCRV+1) * 24.D0 * 3600.D0
      CATR(KRCIC+1) = CATR(KRCIC+1) * 24. * 3600.
      CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', VBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1520) IERR
         CALL MSGWRT (6)
         END IF
C                                       Summary messages
      WRITE (MSGTXT,1521) NPOINT
      CALL MSGWRT (4)
      WRITE (MSGTXT,1522) NFAIL
      CALL MSGWRT (4)
      IF (WASSOU) THEN
         MSGTXT = 'Some data were dropped to avoid averaging different'
     *      // ' sources!'
         CALL MSGWRT (4)
         END IF
      IF ((NPOINT.GT.0.0D0) .AND. (LNCFIL.GT.0)) FRW(LNCFIL) = 1
      IF (NPOINT.LE.0.0D0) IRET = 6
      GO TO 999
C                                       Error message print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRIDTC: MAXBUF',I3,' > 8 OR DIMBUF',I6,' <',I6)
 1005 FORMAT ('GRIDTC: SORT ORDER ''',A2,''' NOT FULLY RECOGNIZED',
     *   ' - USE UVSRT')
 1010 FORMAT ('GRIDTC: UNABLE TO ',A,' FILE - ERROR',I5)
 1015 FORMAT ('GRIDTC: UNABLE TO READ ANTENNAS INFO - ERROR',2I5)
 1055 FORMAT ('Grid baselines',8(I3,'-',I2))
 1230 FORMAT ('GRIDTC: ERROR',I5,' FROM GTBWRT')
 1520 FORMAT ('ERROR',I5,' UPDATING THE CATALOG HEADER')
 1521 FORMAT ('Included',F13.0,' points in the grid')
 1522 FORMAT ('Dropped ',I12,'  points off the grid')
      END
      SUBROUTINE GRIDSP (DPARM, IMSIZE, MAXANS, NODATA, MSOU, MTIMES,
     *   MAXBUF, DIMX, DIMY, XDOCAT, OUTNAM, OUTCLS, SEQOUT, DISKOU,
     *   CNOOUT, MXBASL, NOANTS, MBL, NPOINT, NFAIL, INSNUM, SBUFF,
     *   IRET)
C-----------------------------------------------------------------------
C   GRIDSP makes a gridded image of the UV data in TB order.
C   Inputs:
C      DPARM    R(10)   Control parms:
C                          (1) 0,1,2 => amp, phase, amp rms gridded
C                              3 amp rms/mean gridded
C                              4, 5 => real, imag part of vis
C                              6 => store real and imag for SPFLG
C                          (2) > scalar avg, else vector
C                          (3) > 0 B -> ant. pair (grid twice)
C                          (4) > 0 => divide amp by source flux
C                          (5) x-axis interval
C                          (6) y-axis interval
C      IMSIZE   I(2)    Output image size (x,y)
C      MAXANS   I       Size of NODATA = max # antennas
C      NODATA   I(m,m)  Count of samples by baseline: m = MAXANS
C      MXBASL   I       Dimension of NOANTS (max # baselines)
C      MAXBUF   I       Number of buffers to use
C      DIMBUF   I       Size of summing 1 buffer
C      IOBUF    I       Size of 1 IO buffer
C      XDOCAT   R       > 0 => mark the new image as permanent in the
C                       DFIL commons, else as temporary
C   In/out:
C      MSOU     I(*)    Source numbers that go with times - set to -1
C                          when times set to regular intervals
C      MTIMES   R(*)    (* = IMSIZE(2)) start times for each row
C                          MTIMES(2) < MTIMES(1) < 0 => set regular
C                          times from the first datum
C      OUTNAM   C*12    Output image name
C      OUTCLS   C*6     Output image class or 'SC  ',' '
C      SEQOUT   I       Output image seq number, if scratch, is DFIL
C                       number of SC file
C      DISKOU   I       Output image disk
C   Output:
C      CNOOUT   I       Output image catalog number
C      NOANTS   I(3,M)  m = MXBASL: (1,) IANT (2,) JANT (>= IANT),
C                          (3,) baseline number
C      MBL      I       max number baselines with data
C      NPOINT   D       Vis included in grid note double precision
C      NFAIL    I       Vis included but off the grid
C      SBUFF    R(*)    Summing buffer (DIMX,DIMY,#baselines=MAXBUF)
C      IRET     I       Error code: 0 ok, else die
C   Expected:
C      MAXBUF = > 8     max number planes at once
C      DIMX,DIMY        X,Y dimension of each plane - row DIMY for
c                       counting
C-----------------------------------------------------------------------
      INTEGER   IMSIZE(2), MAXANS, NODATA(MAXANS,MAXANS), MSOU(*),
     *   MAXBUF, DIMX, DIMY, SEQOUT, DISKOU, CNOOUT, MXBASL,
     *   NOANTS(3,MXBASL), MBL, NFAIL, INSNUM, IRET
      DOUBLE PRECISION NPOINT
      REAL      DPARM(10), XDOCAT, MTIMES(*), SBUFF(DIMX,DIMY,*)
      CHARACTER OUTNAM*12, OUTCLS*6
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER PHNAME*48, CUNITS(2)*8, CHTEMP*2, TYPTMP*2, KEYS(4)*8,
     *   CHSTOK(23)*4
      HOLLERITH CATH(256), CATSH(256), CATIH(256)
      INTEGER   LTYPE(2), I, J, K, IERR, JERR, CATSAV(256), LUNO, LIF,
     *   IBLKOF, NBYT, IDEPTH(5), NCOL, II, NROW, LR, LCH,
     *   IROW, ITYP, OLDSOU, IROUND, NUMAN(1025), ISOFF1, LBL, LNCS, IV,
     *   LNCIF, LNCF, CATIMG(256), IANT, JANT, IARR, COLMUL, IVIS,
     *   ISOFF2, LSTSOU, NCOL2, LNCFIL, COLUP, LSUNUM, LLSUN, LLARR,
     *   LBVIS, NSTOK, IB, NUMBUF, LIBL, IBUFF, LSTINC, IVISO, MROW,
     *   NBL, NNVIS, INCBUF, JBUFF, OFFBUF, MUMBUF, LUN, NCOLMX, FIND,
     *   IIV, LOCS(4), VALUES(4), KEYTYP(4), NCHAN, NIF, NPTS,
     *   JR, KR, IY, IX, IBIND
      LOGICAL   T, F, ISINGL, TABLE, EXIST, FITASC, MULTIS, WASSOU,
     *   ISEOF, FIRST
      REAL      CATR(256), CATSR(256), VALUE1, TEMP, CATIR(256), RMAX,
     *   RMIN, TIMLST, RPARM(20), VBUFF(UVBFSS), SPIX(3)
      DOUBLE PRECISION    CATD(128), CATSD(128), CATID(128), TSIGMA,
     *   NNNV, REFREQ, REFPIX
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCHND.INC'
      COMMON /UVIMGC/ CATIMG
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),
     *   (CATSAV, CATSH, CATSR, CATSD), (CATIMG, CATIH, CATIR, CATID)
      DATA LUNO /26/
      DATA T, F /.TRUE.,.FALSE./
      DATA CUNITS /'DEGREES ','RATIO   '/
      DATA CHSTOK /'I','Q','U','V','IQU','IQUV','IV','QU', 'RR','LL',
     *   'RL','LR','RRLL','RLLR','XX','YY','XY','YX','XXYY','XYYX',
     *   'HALF','FULL','CROS'/
      DATA KEYS /'BIF','EIF','BCHAN','ECHAN'/
      DATA LOCS /1,2,3,4/
      DATA KEYTYP /4*4/
C-----------------------------------------------------------------------
      LUN = 55
      NCOLMX = DIMX
      IRET = 0
      LSTSOU = -1
      WASSOU = .FALSE.
      FIRST = (MTIMES(1).LT.0.0) .AND. (MTIMES(2).LE.MTIMES(1))
      LLSUN = INSNUM
      LLARR = 0
      OLDSOU = 0
      NPOINT = 0.0D0
      NFAIL = 0
      ITYP = IROUND (DPARM(1)) + 1
      IF ((ITYP.EQ.1) .AND. (DPARM(2).GT.0.0)) ITYP = 0
C                                       ITYP = output data type:
C                                       0 = amp (scalar average)
C                                       1 = amp, 2 = phase, 3 = rms
C                                       4 = amp rms / mean
C                                       5 = real, 6 = imag,
C                                       7 = real & imag & flag #
      IF (ITYP.EQ.7) THEN
         COLMUL = 3
         COLUP = 3
      ELSE
         IRET = 10
         MSGTXT = 'GRIDSP CALLED WITH BAD ITYP - ONLY 7 ALLOWED'
         GO TO 990
C        COLMUL = 1
C        COLUP = 0
         END IF
      LTYPE(1) = 1
      LTYPE(2) = 2
      CALL H2CHR (2, 1, CATBLK(KITYP), CHTEMP)
      IF (CHTEMP(:1).NE.'T') THEN
         WRITE (MSGTXT,1005) CHTEMP
         IRET = 2
         GO TO 990
         END IF
C                                       Open the UV file
      TYPTMP = 'UV'
      CALL MAPOPN ('READ', IUDISK, UNAME, UCLAS, IUSEQ, TYPTMP, NLUSER,
     *   LUN, FIND, IUCNO, CATBLK, VBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'OPEN INPUT UV', IRET
         IRET = 1
         GO TO 990
         END IF
C                                       and close (UVGET does open)
      CALL MAPCLS ('READ', IUDISK, IUCNO, LUN, FIND, CATBLK, F, VBUFF,
     *   IERR)
C                                       Do we want multi-source?
C                                       only for flux scaling here
      EXIST = .FALSE.
      IF (DPARM(4).GT.0.0) CALL ISTAB ('SU', IUDISK, IUCNO, 1, LUN,
     *   VBUFF, TABLE, EXIST, FITASC, IERR)
      ISINGL = (.NOT.EXIST) .OR. (IERR.NE.0)
      ISINGL = ISINGL .OR. ((ILOCSU.LT.0) .AND. (INSNUM.LE.0))
      IF (ISINGL) DPARM(4) = -1.0
      MULTIS = ILOCSU.GE.0
C                                       Get number antennas
      IF ((LTYPE(1).EQ.1) .AND. (DPARM(3).GT.0.0)) LTYPE(1) = 0
      CALL GETNAN (IUDISK, IUCNO, CATBLK, LUN, VBUFF, NUMAN, IRET)
      IF (((IRET.NE.0) .AND. (IRET.NE.10)) .OR. (NUMAN(1).LE.0)) THEN
         WRITE (MSGTXT,1015) IRET, NUMAN(1)
         IRET = 4
         GO TO 990
         END IF
      J = 0
      LIF = NUMAN(1)
      DO 15 I = 1,LIF
         NUMAN(513+I) = J
         J = J + (NUMAN(1+I) * (NUMAN(1+I) + 3)) / 2
 15      CONTINUE
      NUMAN(514+LIF) = J
C                                       Fill in antenna/baseline array
      DO 20 I = 1,MXBASL
         NOANTS(1,I) = 0
         NOANTS(2,I) = 0
         NOANTS(3,I) = 0
 20      CONTINUE
      MBL = 0
      DO 30 IANT = 1,MAXANS
         DO 25 JANT = IANT,MAXANS
            IF (NODATA(IANT,JANT).GT.0) THEN
               MBL = MBL + 1
               NOANTS(1,MBL) = IANT
               NOANTS(2,MBL) = JANT
               NOANTS(3,MBL) = NUMAN(513+SUBARR) + JANT - IANT + 1
     *            + (IANT-1) * (2*NUMAN(1+SUBARR) + 4 - IANT) / 2
               END IF
 25         CONTINUE
 30      CONTINUE
      IF (MBL.LE.0) THEN
         IRET = 8
         MSGTXT = 'GRIDSP: NO BASELINES WITH DATA HAVE BEEN FOUND'
         GO TO 990
         END IF
C                                       Build image header:
C                                       Misc.
      CALL COPY (256, CATBLK, CATSAV)
      CALL UVPGET (IRET)
      IF (ITYP.EQ.2) CALL CHR2H (8, CUNITS(1), 1, CATH(KHBUN))
      IF (ITYP.EQ.4) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      CATBLK(KIGCN) = 1
      CATBLK(KIPCN) = 0
      CATR(KRBLK) = FBLANK
      CATBLK(KIIMU) = NLUSER
      CATBLK(KITYP) = 0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C                                       Coordinates
      CATBLK(KIDIM) = 7
C                                       Baseline
      CATD(KDCRV+3) = NOANTS(3,1)
      CATBLK(KINAX+3) = MBL
      NPTS = MBL
      CATR(KRCRT+3) = 0.0
      CATR(KRCIC+3) = 1.0
      CATR(KRCRP+3) = 1.0
      CALL CHR2H (8, 'BASELINE', 1, CATH(KHCTP+6))
C                                       FREQ * IF
      NCHAN = ECHAN - BCHAN + 1
      NIF = EIF - BIF + 1
      CATBLK(KINAX) = NCHAN * NIF
      REFREQ = CATSD(KDCRV+JLOCF)
      REFPIX = CATSR(KRCRP+JLOCF)
      CATD(KDCRV) = CATSD(KDCRV+JLOCF)
      CATR(KRCIC) = CATSR(KRCIC+JLOCF) / 3.0
      CATR(KRCRP) = 3.0 * (CATSR(KRCRP+JLOCF) - BCHAN) + 4
      CATR(KRCRT) = 0.0
      NNVIS = CATBLK(KINAX)
C                                       IF
      CATR(KRCRT+4) = 0.0
      IF (JLOCIF.GE.0) THEN
         CATBLK(KINAX+4) = 1
         CATD(KDCRV+4) = CATSD(KDCRV+JLOCIF)
         CATR(KRCIC+4) = CATSR(KRCIC+JLOCIF)
         CATR(KRCRP+4) = CATSR(KRCRP+JLOCIF) - BIF + 1
         I = JLOCIF * 2
         CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+8))
         CALL CHR2H (8, 'FREQ /IF', 1, CATH(KHCTP))
      ELSE
         CATBLK(KINAX+4) = 1
         CATD(KDCRV+4) = 1.0D0
         CATR(KRCIC+4) = 1.0
         CATR(KRCRP+4) = 1.0
         CALL CHR2H (8, 'IF      ', 1, CATH(KHCTP+8))
         CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP))
         END IF
C                                       Stokes
      J = 0
      DO 35 I = 1,23
         IF (STOKES.EQ.CHSTOK(I)) J = I
 35      CONTINUE
      IF ((J.LE.0) .AND. (ICOR0.LT.-5)) J = 16
      IF ((J.LE.0) .AND. (ICOR0.LT.-4)) J = 19
      IF ((J.LE.0) .AND. (ICOR0.LT.-1)) J = 10
      IF ((J.LE.0) .AND. (ICOR0.LT.0)) J = 13
      IF (NCOR.EQ.1) THEN
         TEMP = CATSD(KDCRV+JLOCS) + (1.0-CATSR(KRCRP+JLOCS)) *
     *      CATSR(KRCIC+JLOCS)
         IF (TEMP.LT.-4.) THEN
            J = 10 - IROUND (TEMP)
         ELSE IF (TEMP.LT.0.) THEN
            J = 8 - IROUND (TEMP)
         ELSE
            J = IROUND (TEMP)
            END IF
      ELSE IF ((NCOR.EQ.2) .AND. (J.EQ.22)) THEN
         IF (ICOR0.LT.-4) THEN
            J = 19
         ELSE IF (ICOR0.LT.0) THEN
            J = 13
         ELSE
            J = 7
            END IF
         END IF
      IF (J.LE.0) J = 1
      IF (J.EQ.5) J = 1
      IF (J.EQ.6) J = 1
      IF (ICOR0.LT.-4) THEN
         IF (J.EQ.23) J = 20
         IF (J.EQ.21) J = 19
      ELSE
         IF (J.EQ.23) J = 14
         IF (J.EQ.21) J = 13
         END IF
      IF (J.LE.4) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = J
         CATR(KRCIC+2) = 1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.7) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 3.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.8) THEN
         STOKES = CHSTOK(5)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = 2.0D0
         CATR(KRCIC+2) = 1.0
         ISOFF1 = 1
         ISOFF2 = 2
      ELSE IF (J.LE.12) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 8.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.13) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -1.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.14) THEN
         STOKES = CHSTOK(14)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -3.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 2
         ISOFF2 = 3
      ELSE IF (J.LE.18) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 1
         CATD(KDCRV+2) = 10.0D0 - J
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = -1
      ELSE IF (J.EQ.19) THEN
         STOKES = CHSTOK(J)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -5.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
      ELSE IF (J.EQ.20) THEN
         STOKES = CHSTOK(20)
         CATBLK(KINAX+2) = 2
         CATD(KDCRV+2) = -7.0D0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 2
         ISOFF2 = 3
      ELSE IF (J.EQ.22) THEN
         STOKES = CHSTOK(22)
         CATBLK(KINAX+2) = 4
         CATD(KDCRV+2) = ICOR0
         CATR(KRCIC+2) = -1.0
         ISOFF1 = 0
         ISOFF2 = 1
         END IF
      NSTOK = CATBLK(KINAX+2)
      NPTS = NPTS * NSTOK
      LBVIS = 3 * NSTOK
      CATR(KRCRP+2) = 1.0
      CATR(KRCRT+2) = 0.0
      I = JLOCS * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+4))
C                                       warning message
      IF ((.NOT.DOCAL) .AND. (ICOR0.LE.0) .AND. (J.LE.8) .AND.
     *   (ILOCSU.GE.0)) THEN
         MSGTXT = 'WARNING: CONVERSION TO STOKES BEFORE CALIBRATION'
     *      // ' IS MEANINGLESS'
         CALL MSGWRT (2)
         END IF
C                                       RA, dec
      CATBLK(KINAX+5) = 1
      CATD(KDCRV+5) = CATSD(KDCRV+JLOCR)
      CATR(KRCIC+5) = CATSR(KRCIC+JLOCR)
      CATR(KRCRP+5) = CATSR(KRCRP+JLOCR)
      CATR(KRCRT+5) = CATSR(KRCRT+JLOCR)
      I = JLOCR * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+10))
      CATBLK(KINAX+6) = 1
      CATD(KDCRV+6) = CATSD(KDCRV+JLOCD)
      CATR(KRCIC+6) = CATSR(KRCIC+JLOCD)
      CATR(KRCRP+6) = CATSR(KRCRP+JLOCD)
      CATR(KRCRT+6) = CATSR(KRCRT+JLOCD)
      I = JLOCD * 2
      CALL CHCOPY (8, 1, CATSH(KHCTP+I), 1, CATH(KHCTP+12))
C                                       2nd axis: Time
      CATBLK(KINAX+1) = IMSIZE(2)
      IF (DPARM(6).EQ.0.0) DPARM(6) = 1.0
      CATR(KRCIC+1) = ABS (DPARM(6)) / (24. * 3600.)
      TSIGMA = CATR(KRCIC+1) / 11.0D0
      CATD(KDCRV+1) = (MTIMES(1) + MTIMES(2)) / 2.0
      CATR(KRCRP+1) = 1.0
      CATR(KRCRT+1) = 0.0
      CALL CHR2H (8, 'TIME    ', 1, CATH(KHCTP+2))
      CATBLK(KINAX) = CATBLK(KINAX) * COLMUL + COLUP
      IMSIZE(1) = CATBLK(KINAX)
      IMSIZE(2) = CATBLK(KINAX+1)
      NPTS = NPTS * IMSIZE(1) * IMSIZE(2)
      MROW = CATBLK(KINAX+1)
C                                       Create output map file
      LNCFIL = 0
C                                       Cataloged MA file output
      SEQOUT = ABS (SEQOUT)
      CALL MAKOUT (UNAME, UCLAS, IUSEQ, ' ', OUTNAM, OUTCLS, SEQOUT)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       support BADDISK
      IIV = 0
      IF (DISKOU.GT.0) THEN
         IV = DISKOU
         IIV = IV
         CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
      ELSE
         DO 65 IV = 1,NVOL
            DO 60 I = 1,10
               IF (IV.EQ.IBAD(I)) GO TO 65
 60            CONTINUE
            MSGSUP = 32000
            IIV = IV
            CALL MCREAT (IV, CNOOUT, VBUFF, IRET)
            MSGSUP = 0
            IF (IRET.NE.1) GO TO 70
 65         CONTINUE
         IRET = 1
         END IF
 70   SEQOUT = CATBLK(KIIMS)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'CREATE OUTPUT IMAGE', IRET
         IF (IRET.EQ.1) THEN
            CALL MSGWRT (8)
            MSGTXT = 'NO DISK SPACE ON ALLOWED DISKS'
            IF (IIV.LE.0) MSGTXT = 'BADDISK LEAVES NO DISKS TO USE'
            END IF
         IRET = 3
         GO TO 990
         END IF
      DISKOU = IV
C                                       mark cataloged
      IF (XDOCAT.GT.0.0) THEN
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKOU
         FCNO(NCFILE) = CNOOUT
         FRW(NCFILE) = 2
         LNCFIL = NCFILE
         VALUES(1) = BIF
         VALUES(2) = EIF
         VALUES(3) = BCHAN
         VALUES(4) = ECHAN
         CALL CATKEY ('WRIT', DISKOU, CNOOUT, KEYS, 4, LOCS, VALUES,
     *      KEYTYP, VBUFF, IERR)
C                                       mark as temporary
      ELSE
         NSCR = NSCR + 1
         SCRVOL(NSCR) = DISKOU
         SCRCNO(NSCR) = CNOOUT
         END IF
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IERR)
C                                       Create BL table
      CALL SPFBLT (DISKOU, CNOOUT, MBL, NOANTS, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'CREATE BASELINE TABLE', IRET
         GO TO 990
         END IF
C                                       Loop over IF, freq
      NBYT = 2 * UVBFSS
      NCOL = CATBLK(KINAX)
      NCOL2 = (CATBLK(KINAX) - COLUP) / COLMUL
      CALL COPY (256, CATBLK, CATIMG)
      CALL COPY (256, CATSAV, CATBLK)
      RMAX = -1.E23
      RMIN = 1.E23
      LSTINC = CATIMG(KINAX+2)
      IB = MAXBUF / LSTINC
      NNNV = 0.0D0
C                                       Use buffers for IFs.
      NUMBUF = LSTINC
      LIBL = MAXBUF / NUMBUF
      LIBL = MIN (MBL, LIBL)
      DO 500 LBL = 1,MBL,LIBL
C                                       Init i/o to uv file
         IROW = 1
         NBL = MIN (LBL+LIBL-1, MBL)
         INCBUF = LSTINC
         MUMBUF = (NBL - LBL + 1) * INCBUF
         NUMBUF = MAX (NUMBUF, MUMBUF)
C                                       zero summing array
         LR = DIMX * DIMY
         DO 38 JR = LBL,NBL
            DO 37 JBUFF = 1,INCBUF
               KR = (JR - LBL) * INCBUF + JBUFF
               CALL RFILL (LR, FBLANK, SBUFF(1,1,KR))
 37            CONTINUE
 38         CONTINUE
C                                       set ANTENS
         CALL FILL (50, 0, ANTENS)
         K = 0
         DO 55 LR = LBL,NBL
            DO 40 I = 1,K
               IF (ANTENS(I).EQ.NOANTS(1,LR)) GO TO 45
 40            CONTINUE
            K = K + 1
            ANTENS(K) = NOANTS(1,LR)
 45         DO 50 I = 1,K
               IF (ANTENS(I).EQ.NOANTS(2,LR)) GO TO 55
 50            CONTINUE
            K = K + 1
            ANTENS(K) = NOANTS(2,LR)
 55         CONTINUE
         WRITE (MSGTXT,1055) LBL, NBL
C            WRITE (MSGTXT,1055) (NOANTS(1,LR), NOANTS(2,LR),
C     *         LR = LBL,NBL)
         CALL MSGWRT (2)
         RPARM(1) = FBLANK
         TIMLST = -1.E10
         CALL UVGET ('INIT', RPARM, VBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'INIT INPUT UV', IRET
            IRET = 5
            GO TO 990
            END IF
         LNCS = INCS
         LNCF = INCF
         LNCIF = INCIF
C                                       current row = 0
         NROW = 0
C                                       Read first record
 100     CALL DATGET (RPARM, VBUFF(5), TIMLST, IRET)
         ISEOF = (IRET.EQ.4) .AND. (NNNV.GT.0.0D0)
         IF ((NNNV.EQ.0.0D0) .AND. (IRET.EQ.4)) THEN
            IF (LBL.LT.MBL) ISEOF = .TRUE.
            END IF
         IF (ISEOF) GO TO 390
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'READ INPUT UV', IRET
            IRET = 5
            GO TO 990
            END IF
C                                       set regular times by default
         IF (FIRST) THEN
            II = RPARM(1+ILOCT) / TSIGMA + 0.5D0
            TEMP = II * TSIGMA
            CATID(KDCRV+1) = TEMP
            II = CATIMG(KINAX+1) + 1
            DO 105 I = 1,II
               MSOU(I) = -1
               MTIMES(I) = TEMP + (I - 1.5) * CATIR(KRCIC+1)
 105           CONTINUE
            FIRST = .FALSE.
            END IF
C                                       find row for integration
         TEMP = RPARM(1+ILOCT)
         IF ((IROW.LT.1) .OR. (IROW.GT.MROW)) IROW = 1
         DO 110 LR = IROW,MROW
            IF ((TEMP.GE.MTIMES(LR)) .AND. (TEMP.LT.MTIMES(LR+1)))
     *         GO TO 115
 110        CONTINUE
         NFAIL = NFAIL + 1
         GO TO 100
 115     IROW = LR
C                                       New row, allow new source
         IF (IROW.GT.NROW) THEN
            LLSUN = INSNUM
            LLARR = 0
            END IF
C                                       get source number
         IERR = 0
         IRET = 0
         LSUNUM = 0
         IF (MULTIS) THEN
            IF (ILOCSU.GE.0) THEN
               LSUNUM = RPARM(ILOCSU+1) + 0.1
            ELSE
               IF (NSOUWD.EQ.1) LSUNUM = SOUWAN(1)
               END IF
            END IF
C                                       check antennas
         IF (ILOCB.GE.0) THEN
            TEMP = RPARM(ILOCB+1)
            JANT = TEMP + 0.1
            IARR = 100.0 * (TEMP - JANT) + 1.5
            IANT = JANT / 256
            JANT = JANT - 256 * IANT
         ELSE
            IANT = RPARM(1+ILOCA1) + 0.1
            JANT = RPARM(1+ILOCA2) + 0.1
            IARR = RPARM(1+ILOCSA) + 0.1
            END IF
         IF (IANT.GT.JANT) THEN
            J = IANT
            IANT = JANT
            JANT = J
            END IF
C                                       Do we want it?
         DO 180 LR = LBL,NBL
            IF ((NOANTS(1,LR).EQ.IANT) .AND.
     *         (NOANTS(2,LR).EQ.JANT)) GO TO 190
 180        CONTINUE
         GO TO 100
C                                       if new row, average last one
 190     IF (IROW.GT.NROW) THEN
            IF (NROW.GT.0) THEN
               DO 210 JR = LBL,NBL
                  DO 205 JBUFF = 1,INCBUF
                     J = 1
                     KR = (JR - LBL) * INCBUF + JBUFF
                     SBUFF(J,NROW,KR) = OLDSOU
                     SBUFF(J+1,NROW,KR) = MTIMES(NROW)
                     SBUFF(J+2,NROW,KR) = MTIMES(NROW+1)
                     J = J + 3
                     DO 200 I = 1,NCOL2
                        IF (SBUFF(J+2,NROW,KR).NE.FBLANK) THEN
                           SBUFF(J,NROW,KR) = SBUFF(J,NROW,KR) /
     *                        SBUFF(J+2,NROW,KR)
                           SBUFF(J+1,NROW,KR) = SBUFF(J+1,NROW,KR) /
     *                        SBUFF(J+2,NROW,KR)
                           SBUFF(J+2,NROW,KR) = 0.0
                        ELSE
                           SBUFF(J,NROW,KR) = FBLANK
                           SBUFF(J+1,NROW,KR) = FBLANK
                           SBUFF(J+2,NROW,KR) = FBLANK
                           END IF
                        J = J + 3
 200                    CONTINUE
 205                 CONTINUE
 210              CONTINUE
               END IF
            END IF
C                                       Save record, 1=IANT, 2=JANT,
C                                       3=array, 4=time
         VBUFF(1) = IANT
         VBUFF(2) = JANT
         VBUFF(3) = IARR
         VBUFF(4) = RPARM(1+ILOCT)
C                                       Check source
         OFFBUF = (LR - LBL) * INCBUF
         IF (MULTIS) THEN
            IF (LLSUN.LE.0) THEN
               LLSUN = LSUNUM
               LLARR = IARR
            ELSE
               IF (IARR.LT.LLARR) THEN
                  LLSUN = LSUNUM
                  LLARR = IARR
                  END IF
C                                       Different source in integration
               IF ((IARR.EQ.LLARR) .AND. (LLSUN.NE.LSUNUM)) THEN
                  WASSOU = .TRUE.
                  GO TO 100
                  END IF
               END IF
            END IF
C                                       Vis wanted
         NPOINT = NPOINT + 1.0D0
C                                       Get source flux
         IF ((.NOT.ISINGL) .AND. (LSTSOU.NE.LLSUN)) THEN
            CALL GETSOU (LLSUN, IUDISK, IUCNO, CATSAV, LUNO, IERR)
            DO 240 LIF = BIF,EIF
               IF (FLUX(1,LIF).LE.1.E-10) FLUX(1,LIF) = 1.0
 240           CONTINUE
            LSTSOU = LLSUN
            IF (DPARM(4).GT.1.5) THEN
               IV = IROUND (DPARM(4)) - 1
               CALL FNDSPX (IUDISK, IUCNO, LLSUN, FRQSEL, CATSAV, IV,
     *            SPIX, IERR)
            ELSE
               SPIX(1) = 0.0
               SPIX(2) = 0.0
               SPIX(3) = 0.0
               END IF
            END IF
C                                       Process visibility data.
C                                       First entry:
C                                       Loop over buffer
         DO 300 JBUFF = 1,INCBUF
            IF (INCBUF.EQ.4) THEN
               IVIS = (JBUFF - 1) * LNCS
            ELSE
               IVIS = LNCS * ISOFF1
               IF (MOD(JBUFF-1,LSTINC).EQ.1) IVIS = LNCS * ISOFF2
               END IF
            IVIS = IVIS + 5 + ((JBUFF-1)/LSTINC) * LNCIF
            IVISO = IVIS
            IBUFF = JBUFF + OFFBUF
C                                       Divide by source flux
            IF (.NOT.ISINGL) THEN
               IF (SPIX(2).EQ.0.0) THEN
                  DO 245 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN + BIF
                        VALUE1 = 1.0 / FLUX(1,LIF)
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 245                 CONTINUE
C                                       incl spectral index
               ELSE IF ((DPARM(4).LT.2.5) .OR. (SPIX(1).LE.0.0)) THEN
                  DO 246 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN
                        LCH = IV - 1 - LIF * NCHAN + BCHAN
                        LIF = LIF + BIF
                        VALUE1 = ((REFREQ + FOFF(LIF) + FINC(LIF) *
     *                     (LCH - REFPIX)) / (REFREQ + FOFF(LIF) +
     *                     FINC(LIF) * (NCHAN/2.0 - REFPIX))) ** SPIX(2)
                        VALUE1 = 1.0 / (FLUX(1,LIF) * VALUE1)
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 246                 CONTINUE
C                                       incl spectral index
               ELSE
                  DO 247 IV = 1,NNVIS
                     IF (VBUFF(IVIS+2).GT.0.0) THEN
                        LIF = (IV-1) / NCHAN
                        LCH = IV - 1 - LIF * NCHAN + BCHAN
                        LIF = LIF + BIF
                        VALUE1 = (REFREQ + FOFF(LIF) + FINC(LIF) *
     *                     (LCH - REFPIX)) / 1.D9
                        VALUE1 = LOG10 (VALUE1)
                        VALUE1 = SPIX(2)*VALUE1 + SPIX(3)*VALUE1*VALUE1
                        VALUE1 = 1.0 / (SPIX(1) * (10.0**VALUE1))
                        VBUFF(IVIS) = VBUFF(IVIS) * VALUE1
                        VBUFF(IVIS+1) = VBUFF(IVIS+1) * VALUE1
                        VBUFF(IVIS+2) = VBUFF(IVIS+2) / VALUE1 / VALUE1
                        END IF
                     IVIS = IVIS + LNCF
 247                 CONTINUE
                  END IF
               END IF
            IVIS = IVISO
C                                       Amp (scalar avg) or rms
            J = 0
            KR = (LR - LBL) * INCBUF + JBUFF
            DO 260 IV = 1,NNVIS
               J = J + 3
               IF (VBUFF(IVIS+2).GT.0.0) THEN
                  IF (SBUFF(J+3,IROW,KR).EQ.FBLANK) THEN
                     SBUFF(J+1,IROW,KR) = VBUFF(IVIS)
                     SBUFF(J+2,IROW,KR) = VBUFF(IVIS+1)
                     SBUFF(J+3,IROW,KR) = 1.0
                  ELSE
                     SBUFF(J+1,IROW,KR) = SBUFF(J+1,IROW,KR) +
     *                  VBUFF(IVIS)
                     SBUFF(J+2,IROW,KR) = SBUFF(J+2,IROW,KR) +
     *                  VBUFF(IVIS+1)
                     SBUFF(J+3,IROW,KR) = SBUFF(J+3,IROW,KR) + 1.0
                     END IF
                  NNNV = NNNV + 1.0D0
                  END IF
               IVIS = IVIS + LNCF
 260           CONTINUE
C                                       Second entry:
 300        CONTINUE
C                                       Get next vis buffer load
         OLDSOU = LLSUN
         NROW = IROW
         IF (.NOT.ISEOF) GO TO 100
C                                       average last data row
 390     DO 410 JR = LBL,NBL
            DO 405 JBUFF = 1,INCBUF
               J = 1
               KR = (JR - LBL) * INCBUF + JBUFF
               SBUFF(J,NROW,KR) = OLDSOU
               SBUFF(J+1,NROW,KR) = MTIMES(NROW)
               SBUFF(J+2,NROW,KR) = MTIMES(NROW+1)
               J = J + 3
               DO 400 I = 1,NCOL2
                  IF (SBUFF(J+2,NROW,KR).NE.FBLANK) THEN
                     SBUFF(J,NROW,KR) = SBUFF(J,NROW,KR) /
     *                  SBUFF(J+2,NROW,KR)
                     SBUFF(J+1,NROW,KR) = SBUFF(J+1,NROW,KR) /
     *                  SBUFF(J+2,NROW,KR)
                     SBUFF(J+2,NROW,KR) = 0.0
                  ELSE
                     SBUFF(J,NROW,KR) = FBLANK
                     SBUFF(J+1,NROW,KR) = FBLANK
                     SBUFF(J+2,NROW,KR) = FBLANK
                     END IF
                  J = J + 3
 400              CONTINUE
 405           CONTINUE
 410        CONTINUE
C                                       Close the uv I/O too
         CALL UVGET ('CLOS', RPARM, VBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) 'CLOSE INPUT UV', IRET
            GO TO 990
            END IF
C                                       init image I/O
C                                       Open output files on first pass
         IF (LBL.EQ.1) THEN
            CALL ZOPEN (LUN, FIND, DISKOU, PHNAME, T, F, T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'OPEN OUTPUT IMAGE', IRET
               IRET = 4
               GO TO 990
               END IF
            END IF
         DO 450 KR = 1,MUMBUF
            LR = (KR-1) / LSTINC
            IDEPTH(1) = MOD (KR-1, LSTINC) + 1
            IDEPTH(2) = LBL + (KR-1) / LSTINC
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IERR)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('WRIT', LUN, FIND, CATIMG(KINAX),
     *         CATIMG(KINAX+1), 0, VBUFF, NBYT, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'INIT OUTPUT IMAGE', IRET
               IRET = 4
               GO TO 990
               END IF
            DO 430 IY = 1,DIMY
               CALL MDISK ('WRIT', LUN, FIND, VBUFF, IBIND, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1010) 'WRITE OUTPUT FILE', IRET
                  GO TO 990
                  END IF
               SBUFF(1,IY,KR) = MSOU(IY)
               SBUFF(2,IY,KR) = MTIMES(IY)
               SBUFF(3,IY,KR) = MTIMES(IY+1)
               CALL RCOPY (DIMX, SBUFF(1,IY,KR), VBUFF(IBIND))
               DO 420 IX = 4,DIMX,3
                  IF (SBUFF(IX,IY,KR).NE.FBLANK) THEN
                     RMAX = MAX (RMAX, SBUFF(IX,IY,KR))
                     RMIN = MIN (RMIN, SBUFF(IX,IY,KR))
                     END IF
                  IF (SBUFF(IX+1,IY,KR).NE.FBLANK) THEN
                     RMAX = MAX (RMAX, SBUFF(IX+1,IY,KR))
                     RMIN = MIN (RMIN, SBUFF(IX+1,IY,KR))
                     END IF
 420              CONTINUE
 430           CONTINUE
            CALL MDISK ('FINI', LUN, FIND, VBUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1010) 'FINISH OUTPUT FILE', IRET
               GO TO 990
               END IF
 450        CONTINUE
 500     CONTINUE
C                                       Close file
      CALL ZCLOSE (LUN, FIND, JERR)
C                                       finish the header
      CALL COPY (256, CATIMG, CATBLK)
      CATR(KRDMX) = RMAX
      CATR(KRDMN) = RMIN
      CATD(KDCRV+1) = CATD(KDCRV+1) * 24.D0 * 3600.D0
      CATR(KRCIC+1) = CATR(KRCIC+1) * 24. * 3600.
      CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', VBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1520) IERR
         CALL MSGWRT (6)
         END IF
C                                       Summary messages
      WRITE (MSGTXT,1521) NPOINT
      CALL MSGWRT (4)
      WRITE (MSGTXT,1522) NFAIL
      CALL MSGWRT (4)
      IF (WASSOU) THEN
         MSGTXT = 'Some data were dropped to avoid averaging different'
     *      // ' sources!'
         CALL MSGWRT (4)
         END IF
      IF ((NPOINT.GT.0.0D0) .AND. (LNCFIL.GT.0)) FRW(LNCFIL) = 1
      IF (NPOINT.LE.0.0D0) IRET = 6
      GO TO 999
C                                       Error message print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('GRIDSP: SORT ORDER ''',A2,''' NOT FULLY RECOGNIZED',
     *   ' - USE UVSRT')
 1010 FORMAT ('GRIDSP: UNABLE TO ',A,' FILE - ERROR',I5)
 1015 FORMAT ('GRIDSP: UNABLE TO READ ANTENNAS INFO - ERROR',2I5)
 1055 FORMAT ('GRIDSP: Grid baseline numbers',I5,' through',I5)
 1520 FORMAT ('ERROR',I5,' UPDATING THE CATALOG HEADER')
 1521 FORMAT ('Included',F13.0,' points in the grid')
 1522 FORMAT ('Dropped ',I12,'  points off the grid')
      END
      SUBROUTINE PRFRMT (PR, STR)
C-----------------------------------------------------------------------
C   Formats a pixrange pair
C   Inputs:
C      PR    R(2)   Pix range
C   Output
C      STR   C*15   Formatted string
C-----------------------------------------------------------------------
      REAL      PR(2)
      CHARACTER STR*(*)
C
      REAL      PMAX, PSCA
      INTEGER   I1, I2, I
      LOGICAL   PFLAG
      CHARACTER PREFIX*8
C-----------------------------------------------------------------------
      PMAX = MAX (ABS(PR(1)), ABS(PR(2)))
      IF (PMAX.EQ.0.0) PMAX = 1.0
      PSCA = PMAX
      CALL METSCA (PSCA, PREFIX, PFLAG)
      I2 = 8
      IF (PSCA.GE.9.9995) I2 = 7
      IF (PSCA.GE.99.9995) I2 = 6
      PSCA = PSCA / PMAX
C                                       1st value
      PMAX = PR(1) * PSCA
      WRITE (PREFIX,1000) PMAX
      I1 = 0
 10   I1 = I1 + 1
      IF ((PREFIX(I1:I1).EQ.' ') .AND. (I1.LT.8)) GO TO 10
      STR = '(' // PREFIX(I1:I2) // '-'
      I = I2 - I1 + 4
C                                       2nd value
      PMAX = PR(2) * PSCA
      WRITE (PREFIX,1000) PMAX
      I1 = 0
 20   I1 = I1 + 1
      IF ((PREFIX(I1:I1).EQ.' ') .AND. (I1.LT.8)) GO TO 20
      STR(I:) = PREFIX(I1:I2) // ')'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (F8.3)
      END
      SUBROUTINE TORMAT (TIME, TFORM, STRING, NC)
C-----------------------------------------------------------------------
C   TORMAT formats the time string
C   Inputs
C      TIME     R       Time in days
C      TFORM    I       Number digits after decimal
C   Outputs:
C      STRING   C*(*)   DD/HH:MM:SS or DD/HH:MM:SS.ssss
C      NC       I       Number chars in STRING
C-----------------------------------------------------------------------
      INTEGER   TFORM, NC
      REAL      TIME
      CHARACTER STRING*(*)
C
      INTEGER   IT(4), JT(3)
      REAL      RT
      CHARACTER LC*20
C-----------------------------------------------------------------------
      IF (TFORM.LE.0) THEN
         CALL TODHMS (TIME, IT)
         WRITE (LC,1000) IT
         NC = 11
      ELSE
         CALL TIDHMS (TIME, TFORM, JT, RT)
         WRITE (LC,1010) JT, RT
         IF (LC(10:10).EQ.' ') LC(10:10) = '0'
         NC = 12 + TFORM
         END IF
      STRING = LC(1:NC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2.2,'/',I2.2,2(':',I2.2))
 1010 FORMAT (I2.2,'/',2(I2.2,':'),F9.6)
      END
