LOCAL INCLUDE 'UVIMG.INC'
C                                       Local include for UVIMG
      DOUBLE PRECISION FREQIF, NPOINT, NGRID
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XOUTCL(2), XXSTOK,
     *   XXSOUR(4,30), XSORT
      CHARACTER NAMEIN*12, CLAIN*6, OUTNAM*12, OUTCLS*6, XSTOK*4,
     *   XSOUR(30)*16, DOSORT(2)*1
      REAL      XSIN, XDISIN, XOUTSE, XOUTDK, XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XBCHAN, XECHAN, XANT(50), XBASE(50),
     *   XUVRA(2), XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), DPARM(10), XIMSIZ(2), XNCHAV,
     *   XCHINC, XXTYPE, XYTYPE, XXPARM(10), XYPARM(10), XBADD(10),
     *   RPARM(20)
      LOGICAL   ISINGL, DESEL
      INTEGER   DISKIN, SEQIN, CNOIN, DISKOU, SEQOUT, CNOOUT, INEXT,
     *   INVER, IMSIZE(2), BUFFER(512), CATIMG(256), NXANT, NXBASL,
     *   IXANT(50), IXBASL(50), NFAIL, NANTSK, NCHAV, CHINC, LTYPE(2),
     *   NFLAGD, ROUND, CTYPEX, CTYPEY
      REAL      XPARM(10), YPARM(10)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XOUTCL,
     *   XOUTSE, XOUTDK, XXSOUR, XTIME, XXSTOK, XBAND, XFREQ,  XFQID,
     *   XBIF, XEIF, XBCHAN, XECHAN, XANT, XBASE, XUVRA, XSUBA, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH,
     *   DPARM, XIMSIZ, XNCHAV, XCHINC, XSORT, XXTYPE, XYTYPE, XXPARM,
     *   XYPARM, XBADD
      COMMON /CHRCOM/ NAMEIN, CLAIN, OUTNAM, OUTCLS, XSTOK, XSOUR,
     *   DOSORT
      COMMON /UVIMGC/ CATIMG
      COMMON /INFOLS/ FREQIF, NPOINT, NGRID, RPARM, NFAIL, NANTSK,
     *   ISINGL, DESEL, DISKIN, SEQIN, CNOIN, DISKOU, SEQOUT, CNOOUT,
     *   INEXT, INVER, IMSIZE, BUFFER, NXANT, NXBASL, IXANT, IXBASL,
     *   NCHAV, CHINC, LTYPE, ROUND, NFLAGD, CTYPEX, CTYPEY, XPARM,
     *   YPARM
LOCAL END
      PROGRAM UVIMG
C-----------------------------------------------------------------------
C! Grids uv data into image form
C# UV Map Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2003, 2006, 2008-2013
C;  Copyright (C) 2015-2016
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
C   Task UVIMG grids uv data in the present sort order to make an
C   image of the selected form (amp, phase, rms) of the data
C   Inputs:
C      AIPS Adverb   Prg. Name          Description
C      INNAME         NAME          File name to be imaged
C      INCLASS        CLASS         File class to be imaged
C      INSEQ          SEQ           File sequence number
C      INDISK         DISK          Disk volume on which file resides
C      OUTNAME        OUTNAM        Output image file name
C      OUTCLASS       OUTCLS        Output image file class
C      OUTSEQ         SEQOUT        Output image file seq
C      OUTDISK        DISKOU        Output image file disk
C      SOURCES        XSOUR(30)     Sources selected
C      TIMERANG       XTIME(8)      Timerange
C      STOKES         XSTOK         Stokes' parameter
C      BIF            BIF           IF number
C      BCHAN          BCHAN         Channel number
C      ANTENNAS       XANT(50)      Antenna numbers
C      UVRANGE        UVRANG        Range of UV in 1000's wavelengths
C      SUBARRAY       SUBARR        Subarray
C      DOCALIB        DOCAL         Calibrate?
C      GAINUSE        GAUSE         CL version to apply.
C      FLAGVER        FGVER         Flag table version
C      DOBAND                       Apply bandpass calibration?
C      BPVER                        BP table to apply
C      SMOOTH                       Smoothing function
C      DPARM          DPARM         Control info.
C      IMSIZE         IMSIZE        Output image size
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UVIMG '/
C-----------------------------------------------------------------------
C                                       get inputs, ...
      CALL UVIMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do the gridding
      CALL UVIMGR (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do history, extensions
      CALL UVIMHI
C                                       close down
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UVIMIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   UVIMIN gets the inputs for UVIMG.
C   Inputs:
C      PRGM   C*6   Task name
C   Output:
C      IRET   I     Error code: 0 ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   NPARM, IERR, IROUND, I, LUN
      LOGICAL   MATCH
      HOLLERITH CATH(256)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATH, CATBLK)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 311
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SEQOUT = IROUND (XOUTSE)
      DISKOU = IROUND (XOUTDK)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, OUTNAM)
      CALL H2CHR (6, 1, XOUTCL, OUTCLS)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
C                                       Get CATBLK.
      CNOIN = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, PTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.5)  THEN
            WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, PTYPE,
     *         DISKIN, NLUSER
         ELSE
            WRITE (MSGTXT,1011) NAMEIN, CLAIN, SEQIN, PTYPE, DISKIN,
     *         NLUSER
            END IF
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
C                                       sort option
      CALL H2CHR (1, 2, XSORT, DOSORT(1))
      IF (DOSORT(1).EQ.' ') CALL H2CHR (1, 2, CATH(KITYP), DOSORT(1))
      CALL H2CHR (1, 1, XSORT, DOSORT(2))
      IF (DOSORT(2).EQ.' ') CALL H2CHR (1, 1, CATH(KITYP), DOSORT(2))
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      IUDISK = DISKIN
      USEQ = SEQIN
      IUSEQ = SEQIN
      DO 40 I = 1,30
         SOURCS(I) = XSOUR(I)
 40      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = XSTOK
      IF ((STOKES.NE.'I') .AND. (STOKES.NE.'Q') .AND. (STOKES.NE.'U')
     *   .AND. (STOKES.NE.'V') .AND. (STOKES.NE.'RR') .AND.
     *   (STOKES.NE.'LL') .AND. (STOKES.NE.'RL') .AND. (STOKES.NE.'LR')
     *   .AND. (STOKES.NE.'XX') .AND. (STOKES.NE.'YY') .AND.
     *   (STOKES.NE.'XY') .AND. (STOKES.NE.'YX')) STOKES = 'I'
      MSGTXT = 'GRIDDING STOKES = ''' // STOKES // ''''
      CALL MSGWRT (2)
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      I = ECHAN - BCHAN + 1
      NCHAV = XNCHAV + 0.1
      NCHAV = MAX (1, MIN (NCHAV, I))
      CHINC = XCHINC + 0.1
      IF (CHINC.LE.0) CHINC = NCHAV
      IF (NCHAV.GE.I) CHINC = I
      CHINC = MIN (I, CHINC)
      I = (I - NCHAV) / CHINC + 1
      ECHAN = BCHAN + (I-1) * CHINC + NCHAV - 1
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
C                                       Antennas
      CALL SETANT (50, XANT, XBASE, NXANT, NXBASL, IXANT, IXBASL,
     *   DESEL)
C
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      CALL RCOPY (3, XSMOTH, SMOOTH)
      DO 80 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 80      CONTINUE
      IMSIZE(1) = IROUND (XIMSIZ(1))
      IMSIZE(2) = IROUND (XIMSIZ(2))
      IF (IMSIZE(1).LT.8) IMSIZE(1) = 512
      IF (IMSIZE(2).LT.8) IMSIZE(2) = 512
      IF ((DPARM(1).LT.0.) .OR. (DPARM(1).GT.6.49)) DPARM(1) = 0.0
      IF (DPARM(8).LE.0.0) THEN
         XXTYPE = 0.0
         XYTYPE = 0.0
         END IF
      CTYPEX = XXTYPE + 0.01
      CTYPEY = XYTYPE + 0.01
      ROUND = -1
      IF (CTYPEX.GT.10) THEN
         ROUND = 1
         CTYPEY = CTYPEX
      ELSE IF ((CTYPEX.GT.0) .AND. (CTYPEY.GT.0)) THEN
         ROUND = 0
         CTYPEY = MOD (CTYPEY, 10)
         END IF
      CALL RCOPY (10, XXPARM, XPARM)
      CALL RCOPY (10, XYPARM, YPARM)
      IF (ROUND.GE.0) CALL GRDFLT (CTYPEX, CTYPEY, XPARM, YPARM)
      GO TO 999
C                                       message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVIMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' (',A2,') DISK',
     *   I3,' USID',I5)
 1011 FORMAT ('CAN''T FIND ',A12,'.',A6,'.',I4,' (',A2,') DISK=',
     *   I3,' USID=',I5)
 1020 FORMAT ('UVIMIN: ERROR',I3,' READING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE UVIMGR (IRET)
C-----------------------------------------------------------------------
C   UVIMGR is the main action routine of UVIMG.  If the input UV file is
C   in TB order, it calls GRIDTB to make a B(x-axis), T(y-axis) image.
C   Else it does the work itself using the users' inputs and the
C   internal subroutine GRIDUV.
C   Output:
C      IRET    I      Error code: 0 => okay, else die.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CHTM2*2
      INTEGER   MSOU(32768), INSNUM, NWORDS, NCONV
      REAL      MTIMES(32768), VIMAG1(2), VIMAG2(2), CONV(2), TT,
     *   NIMAG(2)
      LONGINT   PVIMG1, PVIMG2, PNIMAG, PCONV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   MAXBUF, IOBUF, DIMBUF
      PARAMETER (MAXBUF = 8)
      PARAMETER (IOBUF = 2*MAXIMG)
      PARAMETER (DIMBUF = (2 * MAXANT * MAXANT))
      INTEGER   NBUFF(DIMBUF,MAXBUF)
      REAL      RBUFF(IOBUF,MAXBUF), SBUFF(DIMBUF,MAXBUF)
C-----------------------------------------------------------------------
C                                       Is this image TB?
      CALL H2CHR (2, 1, CATBLK(KITYP), CHTM2)
      IF ((CHTM2.EQ.'TB') .AND. (DOSORT(1).EQ.'T') .AND.
     *   (DOSORT(2).EQ.'B') .AND. (DPARM(1).LE.5.49) .AND. (ROUND.LT.0))
     *   THEN
         MTIMES(1) = -1.0
         MTIMES(2) = -2.0
         INSNUM = 0
         CALL GRIDTB (DPARM, IMSIZE, NCHAV, CHINC, NXANT, NXBASL,
     *      IXANT, IXBASL, DESEL, MSOU, MTIMES, MAXBUF, DIMBUF, IOBUF,
     *      1.0, OUTNAM, OUTCLS, SEQOUT, DISKOU, CNOOUT, NPOINT, NFAIL,
     *      INSNUM, NANTSK, NBUFF, SBUFF, RBUFF, IRET)
C                                       Okay - do it ourselves
      ELSE
         NWORDS = (IMSIZE(1) * IMSIZE(2) - 1) / 1024 + 2
         CALL ZMEMRY ('GET ', 'UVIMGR', NWORDS, VIMAG1, PVIMG1, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'UVIMGR', NWORDS, VIMAG2,
     *      PVIMG2, IRET)
         IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'UVIMGR', NWORDS, NIMAG,
     *      PNIMAG, IRET)
         IF (IRET.EQ.0) THEN
            TT = MAX (XPARM(1), XPARM(2))
            NCONV = TT + 0.991
            NCONV = 100 * (2 * NCONV + 1) + 1
            NWORDS = (NCONV*NCONV - 1) / 1024 + 2
            CALL ZMEMRY ('GET ', 'UVIMGR', NWORDS, CONV, PCONV, IRET)
            END IF
         IF (IRET.NE.0) THEN
            MSGTXT = 'COULD NOT GET NEEDED DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         CALL CONSET (NCONV, CONV(1+PCONV), RBUFF)
         CALL GRIDUV (NCONV, CONV(1+PCONV), IMSIZE, VIMAG1(1+PVIMG1),
     *      VIMAG2(1+PVIMG2), NIMAG(1+PNIMAG), IOBUF, RBUFF, IRET)
         END IF
C
 999  RETURN
      END
      SUBROUTINE CONSET (NCONV, CONV, BUFF)
C-----------------------------------------------------------------------
C   CONSET deals with setting up the convolution function
C   Inputs:
C      NCONV   I      Size of convolution function
C   Outputs:
C      CONV    R(*)   Convolution array (NCONV, NCONV)
C      BUFF    R(*)   scratch (NCONV)
C-----------------------------------------------------------------------
      INTEGER   NCONV
      REAL      CONV(NCONV,*), BUFF(*)
C
      INCLUDE 'UVIMG.INC'
      INTEGER   KTYPE, I, NROW, LIM, NMAX, IALF, IM, IER, LIMIT, STEP,
     *   J, IRAD, CINC, CENT
      REAL      PARM(10), UMAX, XINC, U, ABSU, P1, P2, ETA, PSI, BESSJ1,
     *   V, RADIUS, SUM
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       get standard defaults
      I = NCONV * NCONV
      CALL RFILL (I, 0.0, CONV)
C                                       compute function: X
      KTYPE = MOD (CTYPEX, 10)
      CALL COPY (10, XPARM, PARM)
      CINC = 100
      CENT = (NCONV + 1) / 2
      STEP = 1
C                                       X or Y
 5    NROW = MAX (PARM(1), 1.0)
      NROW = 2 * NROW + 1
      LIM = NROW * CINC + 1
      UMAX = PARM(1)
      XINC = 1.0 / CINC
      CALL RFILL (NCONV, 0.0, BUFF)
C                                       Pill box function
      IF (KTYPE.EQ.1) THEN
         DO 10 I = 1,NCONV
            U = (I-CENT) * XINC
            ABSU = ABS (U)
            IF (ABSU.EQ.UMAX) THEN
               BUFF(I) = 0.5
            ELSE IF (ABSU.LT.UMAX) THEN
               BUFF(I) = 1.0
               END IF
 10         CONTINUE
C                                       Exponential function.
      ELSE IF (KTYPE.EQ.2) THEN
         P1 = 1.0 / PARM(2)
         DO 20 I = 1,NCONV
            U = (I-CENT) * XINC
            ABSU = ABS (U)
            IF (ABSU.LE.UMAX) BUFF(I) = EXP (-((P1*ABSU) ** PARM(3)))
 20         CONTINUE
C                                       Sinc function.
      ELSE IF (KTYPE.EQ.3) THEN
         P1 = PI / PARM(2)
         DO 30 I = 1,NCONV
            U = (I-CENT) * XINC
            ABSU = ABS (U)
            IF (ABSU.EQ.0) THEN
               BUFF(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFF(I) = SIN (P1*ABSU) / (P1*ABSU)
               END IF
 30         CONTINUE
C                                       EXP * SINC convolving fn.
      ELSE IF (KTYPE.EQ.4) THEN
         P1 = PI / PARM(2)
         P2 = 1.0 / PARM(3)
         DO 40 I = 1,NCONV
            U = (I - CENT) * XINC
            ABSU = ABS (U)
C                                       Check for central point.
            IF (ABSU.LT.XINC) THEN
               BUFF(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFF(I) = SIN(U*P1) / (U*P1) *
     *            EXP (-((ABSU * P2) ** PARM(4)))
               END IF
 40         CONTINUE
C                                       Spherodial wave function
      ELSE IF (KTYPE.EQ.5) THEN
         NMAX = PARM(1)/XINC + 0.1
C                                       Compute function
         IALF = 2.0 * PARM(2) + 1.1
         IM = 2.0 * PARM(1) + 0.1
         IALF = MAX (1, MIN (5, IALF))
         IM = MAX (4, MIN (8, IM))
         DO 50 I = 1,NMAX
            ETA = REAL (I-1) / REAL (NMAX-1)
            CALL SPHFN (IALF, IM, 0, ETA, PSI, IER)
            BUFF(CENT+I-1) = PSI
 50         CONTINUE
C                                       Fill in other half
         LIMIT = CENT-1
         DO 52 I = 1,LIMIT
            BUFF(CENT-I) = BUFF(CENT+I)
 52         CONTINUE
C                                       EXP * SINC convolving fn.
      ELSE IF (KTYPE.EQ.6) THEN
         P1 = PI / PARM(2)
         P2 = 1.0 / PARM(3)
         DO 60 I = 1,NCONV
            U = (I - CENT) * XINC
            ABSU = ABS (U)
C                                       Check for central point.
            IF (ABSU.LT.XINC) THEN
               BUFF(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFF(I) = 2.0 * BESSJ1(U*P1) / (U*P1) *
     *            EXP (-((ABSU * P2) ** PARM(4)))
               END IF
 60         CONTINUE
         END IF
C                                       loop for Y
      IF ((STEP.EQ.1) .AND. (CTYPEX.LT.11)) THEN
         CALL RCOPY (NCONV, BUFF, CONV(1,CENT))
         STEP = 2
         CALL RCOPY (10, YPARM, PARM)
         KTYPE = MOD (CTYPEY, 10)
         GO TO 5
         END IF
C                                       rectangular full buffer
      LIMIT = CENT-1
      IF (ROUND.EQ.0) THEN
         DO 110 J = 1,LIMIT
            CONV(CENT,CENT-J) = BUFF(CENT-J)
            CONV(CENT,CENT+J) = BUFF(CENT+J)
            DO 100 I = 1,LIMIT
               CONV(CENT-I,CENT-J) = CONV(CENT-I,CENT) * BUFF(CENT-J)
               CONV(CENT+I,CENT-J) = CONV(CENT-I,CENT-J)
               CONV(CENT+I,CENT+J) = CONV(CENT-I,CENT-J)
               CONV(CENT-I,CENT+J) = CONV(CENT-I,CENT-J)
 100           CONTINUE
 110        CONTINUE
C                                       round
      ELSE IF (ROUND.EQ.1) THEN
         DO 140 J = 0,LIMIT
            V = J
            DO 130 I = 0,LIMIT
               U = I
               RADIUS = SQRT (U*U + V*V)
               IF (RADIUS*XINC.LE.UMAX) THEN
                  IRAD = RADIUS + 0.5 + CENT
                  CONV(CENT-I,CENT-J) = BUFF(IRAD)
                  CONV(CENT+I,CENT-J) = BUFF(IRAD)
                  CONV(CENT-I,CENT+J) = BUFF(IRAD)
                  CONV(CENT+I,CENT+J) = BUFF(IRAD)
                  END IF
 130           CONTINUE
 140        CONTINUE
         END IF
C                                       normalize
      SUM = 0.0
      DO 160 J = 1,NCONV
         DO 150 I = 1,NCONV
            SUM = SUM + CONV(I,J)
 150        CONTINUE
 160     CONTINUE
      SUM = SUM * XINC * XINC
      DO 180 J = 1,NCONV
         DO 170 I = 1,NCONV
            CONV(I,J) = CONV(I,J) / SUM
 170        CONTINUE
 180     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GRIDUV (NCONV, CONV, NXY, VIMAG1, VIMAG2, NIMAG, IOBUF,
     *   RBUFF, IRET)
C-----------------------------------------------------------------------
C   GRIDUV uses the user's inputs to make a gridded image of the UV data
C   Input:
C      NCONV   I      Full size of convolution array
C      CONV    R(*)   Convolution array (NCONV,NCOV)
C      NXY     I(2)   X, Y dimension of image
C      IOBUF   I      Size of IO buffer
C   Output:
C      VIMAG1  R(*)   Summing buffer (NXY(1),NXY(2))
C      VIMAG2  R(*)   Summing buffer (NXY(1),NXY(2))
C      NIMAG   R(*)   Summing buffer (NXY(1),NXY(2))
C      RBUFF   R(*)   Output buffer
C      IRET    I      Error code: 0 => okay
C-----------------------------------------------------------------------
      INTEGER   NCONV, NXY(2), IOBUF, IRET
      REAL      CONV(NCONV,*), VIMAG1(NXY(1),*), VIMAG2(NXY(1),*),
     *   NIMAG(NXY(1),*), RBUFF(*)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PHNAME*48, ITYPES(11)*1, AXTYPE(12)*8, CUNITS(3)*8,
     *   PTYPE*2, BNDCOD(MAXIF)*8
      HOLLERITH CATH(256), CATSH(256)
      INTEGER   NTYPES, I, J, LUN, FIND, IERR, CATSAV(256),
     *   LUNO, FINDO, LIF, LCHAN, NIFS, CHVER, IBLKOF, NBYT, IDEPTH(5),
     *   MPTR, LBIF, LEIF, LBCHAN, LECHAN, NROW, NCOL, ITYP, IROUND,
     *   NUMAN(1025), NAX, IHMODE(13), ISBAND(MAXIF), NAXST, KCH, IP,
     *   IX, IY
      LOGICAL   T, FIRST, DOTWO, MULTI
      REAL      CATR(256), CATSR(256), VALUE1, VALUE2, VALUE, DIVIDR,
     *   CATIR(256), RMAX, RMIN, OVIS(3,2), UVMULT, FINC(MAXIF),
     *   VIS(3,MAXCIF), RROW(2), RCOL(2)
      DOUBLE PRECISION CATD(128), CATSD(128), CATID(128), CFREQ(MAXIF),
     *   DNP
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD),
     *   (CATSAV, CATSH, CATSR, CATSD), (CATIMG, CATIR, CATID)
      DATA NTYPES, ITYPES /11,'B','T','R','P','W','U','V','X',
     *   'Y','Z','M'/
C                                       are these ok?
      DATA AXTYPE /'ANT PAIR', 'BASELINE', 'TIME    ',
     *   'UVRADIUS', 'UVPOSANG', 'W       ', 'U       ',
     *   'V       ', 'ABS(U)  ', 'ABS(V)  ', 'ABS(U)  ',
     *   'ABS(V)  '/
      DATA LUN, LUNO /25, 26/
      DATA T /.TRUE./
      DATA IHMODE /1, 4, 2, 3, 1, 1, 1, -1, -2, -3, -4, -1, -1/
      DATA CUNITS /'DEGREES','RATIO','SAMPLES'/
C-----------------------------------------------------------------------
      IRET = 0
      NPOINT = 0.0D0
      NGRID = 0.0D0
      NFAIL = 0
      NANTSK = 0
      NFLAGD = 0
      FIRST = .TRUE.
      DO 10 I = 1,2
         LTYPE(I) = -1
         DO 5 J = 1,NTYPES
            IF (DOSORT(I).EQ.ITYPES(J)) LTYPE(I) = J
 5          CONTINUE
 10      CONTINUE
      IF ((LTYPE(1).LE.0) .OR. (LTYPE(2).LE.0)) THEN
         WRITE (MSGTXT,1010) DOSORT(2), DOSORT(1)
         IRET = 2
         GO TO 990
         END IF
C                                       Open the UV file
      PTYPE = 'UV'
      CALL MAPOPN ('READ', IUDISK, UNAME, UCLAS, IUSEQ, PTYPE, NLUSER,
     *   LUN, FIND, CNOIN, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING INPUT UV DATA SET'
         IRET = 1
         GO TO 990
         END IF
C                                       and close (UVGET does open)
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      FVOL(NCFILE) = IUDISK
      CALL ZCLOSE (LUN, FIND, IERR)
C                                       multi-source?
      CALL MULSDB (CATBLK, MULTI)
C                                       Get number antennas
      IF ((LTYPE(1).EQ.1) .OR. (LTYPE(2).EQ.1)) THEN
         IF ((LTYPE(1).EQ.1) .AND. (DPARM(3).GT.0.)) LTYPE(1) = 0
         IF ((LTYPE(2).EQ.1) .AND. (DPARM(3).GT.0.)) LTYPE(2) = 0
         DOTWO = (LTYPE(1).EQ.0) .OR. (LTYPE(2).EQ.0)
         CALL GETNAN (IUDISK, CNOIN, CATBLK, LUN, BUFFER, NUMAN, IERR)
         IF ((IERR.NE.0) .OR. (NUMAN(1).LE.0)) THEN
            WRITE (MSGTXT,1030) IERR, NUMAN(1)
            IRET = 4
            GO TO 990
            END IF
         J = 0
         LIF = NUMAN(1)
         DO 40 I = 1,LIF
            NUMAN(513+I) = J
            IF (.NOT.DOTWO) THEN
               J = J + (NUMAN(1+I) * (NUMAN(1+I) + 3)) / 2
            ELSE
               J = J + NUMAN(1+I) * (NUMAN(1+I) + 1)
               END IF
 40         CONTINUE
         NUMAN(514+LIF) = J
         END IF
      DNP = 1.0D0
      IF ((LTYPE(1).EQ.0) .OR. (LTYPE(2).EQ.0))  DNP = 2.0D0
      IF ((LTYPE(1).EQ.6) .AND. (LTYPE(2).EQ.7)) DNP = 2.0D0
      IF ((LTYPE(1).EQ.7) .AND. (LTYPE(2).EQ.6)) DNP = 2.0D0
C                                       Build image header:
C                                       Misc.
      CALL COPY (256, CATBLK, CATSAV)
      ITYP = IROUND (DPARM(1)) + 1
      IF ((ITYP.EQ.1) .AND. (DPARM(2).GT.0.5)) ITYP = 0
      IF (ITYP.EQ.2) CALL CHR2H (8, CUNITS(1), 1, CATH(KHBUN))
      IF (ITYP.EQ.4) CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
      IF (ITYP.EQ.7) CALL CHR2H (8, CUNITS(3), 1, CATH(KHBUN))
      CATBLK(KIGCN) = 1
      CATBLK(KIPCN) = 0
      CATR(KRBLK) = FBLANK
      CATBLK(KIIMU) = NLUSER
      CATBLK(KITYP) = 0
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
C                                       Fish frequency information
C                                       from CH/FQ tables or header
      CHVER = 1
      CALL CHNDAT ('READ', BUFFER, IUDISK, CNOIN, CHVER, CATSAV,
     *   LUN, NIFS, CFREQ, ISBAND, FINC, BNDCOD, FRQSEL, IERR)
      IF (IERR.NE.0) THEN
        WRITE (MSGTXT,1000) IERR, 'GETTING FQ TABLE INFO'
        CALL MSGWRT (6)
        GO TO 999
        END IF
C                                       Coordinates
C                                       FREQ
      CATBLK(KIDIM) = 6
      NAX = 3
      CATBLK(KINAX+2) = (ECHAN - BCHAN) / CHINC + 1
      CATD(KDCRV+2) = CATSD(KDCRV+JLOCF) + CFREQ(BIF)
C                                       Incr. could change with IF ?
C                                       At least get the inc. from the
C                                       selected IF
      CATR(KRCIC+2) = FINC(BIF) * CHINC
      CATR(KRCRP+2) = (CATSR(KRCRP+JLOCF) - BCHAN - (NCHAV-1.0)/2.0) /
     *   CHINC + 1.0
      CATR(KRCRT+2) = 0.0
      J = 2 * 2
      I = JLOCF * 2
      CATH(KHCTP+J) = CATSH(KHCTP+I)
      CATH(KHCTP+J+1) = CATSH(KHCTP+I+1)
C                                       IF
      IF (JLOCIF.GE.0) THEN
         NAX = 4
         CATBLK(KIDIM) = 7
         CATBLK(KINAX+3) = EIF - BIF + 1
         CATD(KDCRV+3) = CATSD(KDCRV+JLOCIF)
         CATR(KRCIC+3) = CATSR(KRCIC+JLOCIF)
         CATR(KRCRP+3) = 1.0
         CATR(KRCRT+3) = 0.0
         J = 3 * 2
         I = JLOCIF * 2
         CATH(KHCTP+J) = CATSH(KHCTP+I)
         CATH(KHCTP+J+1) = CATSH(KHCTP+I+1)
         END IF
C                                       Stokes
      CATBLK(KINAX+NAX) = 1
      NAXST = NAX
      CATD(KDCRV+NAXST) = CATSD(KDCRV+JLOCS)
      CATR(KRCIC+NAX) = 1.0
      CATR(KRCRP+NAX) = 1.0
      CATR(KRCRT+NAX) = 0.0
      J = NAX * 2
      I = JLOCS * 2
         CATH(KHCTP+J) = CATSH(KHCTP+I)
         CATH(KHCTP+J+1) = CATSH(KHCTP+I+1)
      NAX = NAX + 1
C                                       RA, dec
      CATBLK(KINAX+NAX) = 1
      CATD(KDCRV+NAX) = CATSD(KDCRV+JLOCR)
      CATR(KRCIC+NAX) = CATSR(KRCIC+JLOCR)
      CATR(KRCRP+NAX) = CATSR(KRCRP+JLOCR)
      CATR(KRCRT+NAX) = CATSR(KRCRT+JLOCR)
      J = NAX * 2
      I = JLOCR * 2
      CATH(KHCTP+J) = CATSH(KHCTP+I)
      CATH(KHCTP+J+1) = CATSH(KHCTP+I+1)
      NAX = NAX + 1
      CATBLK(KINAX+NAX) = 1
      CATD(KDCRV+NAX) = CATSD(KDCRV+JLOCD)
      CATR(KRCIC+NAX) = CATSR(KRCIC+JLOCD)
      CATR(KRCRP+NAX) = CATSR(KRCRP+JLOCD)
      CATR(KRCRT+NAX) = CATSR(KRCRT+JLOCD)
      J = NAX * 2
      I = JLOCD * 2
      CATH(KHCTP+J) = CATSH(KHCTP+I)
      CATH(KHCTP+J+1) = CATSH(KHCTP+I+1)
      NAX = NAX - 2
C                                       First 2 axes
      IF ((LTYPE(2).GT.7) .AND. (LTYPE(1).EQ.8)) LTYPE(1) = 6
      IF ((LTYPE(2).GT.7) .AND. (LTYPE(1).EQ.9)) LTYPE(1) = 7
      IF ((LTYPE(2).GT.7) .AND. (LTYPE(1).EQ.10)) LTYPE(1) = 6
      IF ((LTYPE(2).GT.7) .AND. (LTYPE(1).EQ.11)) LTYPE(1) = 7
      DO 55 I = 1,2
         CATBLK(KINAX+I-1) = IMSIZE(I)
         IF (DPARM(4+I).EQ.0.0) DPARM(4+I) = 1.0
         CATR(KRCIC+I-1) = ABS (DPARM(4+I))
         IF ((LTYPE(I).GE.8) .AND. (LTYPE(I).LE.9)) CATR(KRCIC+I-1) =
     *      -ABS (DPARM(4+I))
         IF (LTYPE(I).LE.1) CATR(KRCIC+I-1) = 1.0
         IF (LTYPE(I).EQ.2) CATR(KRCIC+I-1) = CATR(KRCIC+I-1) /
     *      (24. * 3600.)
C                                       Time must come from data
         CATD(KDCRV+I-1) = 0.0D0
         IF (LTYPE(I).LE.1) CATD(KDCRV+I-1) = 1.0D0
         IF ((LTYPE(I).LE.1) .AND. (SUBARR.GT.0)) CATD(KDCRV+I-1) =
     *      NUMAN(513+SUBARR) + 1.0D0
         IF (LTYPE(I).LE.1) CATBLK(KINAX+I-1) = NUMAN(514+NUMAN(1)) -
     *      CATD(KDCRV+I-1) + 1.00001
         CATR(KRCRP+I-1) = 1.0
         IF ((LTYPE(I).GE.4) .AND. (LTYPE(I).LE.7)) CATR(KRCRP+I-1) =
     *      (CATBLK(KINAX+I-1)+1.0) / 2.0
         IF ((LTYPE(I).GE.8) .AND. (LTYPE(I).LE.9)) CATR(KRCRP+I-1) =
     *      CATBLK(KINAX+I-1)
         CATR(KRCRT+I-1) = 0.0
         J = (I-1) * 2
         CALL CHR2H (8, AXTYPE(LTYPE(I)+1), 1, CATH(KHCTP+J))
 55      CONTINUE
C                                       Create output map file
      CALL MAKOUT (UNAME, UCLAS, IUSEQ, '      ', OUTNAM, OUTCLS,
     *   SEQOUT)
      CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, OUTCLS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      CALL MCREAT (DISKOU, CNOOUT, BUFFER, IERR)
      SEQOUT = CATBLK(KIIMS)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CREATE OUTPUT IMAGE'
         IRET = 3
         GO TO 990
         END IF
C                                       Open output file
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKOU
      FCNO(NCFILE) = CNOOUT
      FRW(NCFILE) = 2
      CALL ZPHFIL ('MA', DISKOU, CNOOUT, 1, PHNAME, IERR)
      CALL ZOPEN (LUNO, FINDO, DISKOU, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT IMAGE'
         IRET = 4
         GO TO 990
         END IF
C                                       Loop over IF, freq
      NBYT = IOBUF * 2
      LBIF = BIF
      LEIF = EIF
      LBCHAN = BCHAN
      LECHAN = ECHAN
      NCOL = CATBLK(KINAX)
      NROW = CATBLK(KINAX+1)
      CALL COPY (256, CATBLK, CATIMG)
      CALL COPY (256, CATSAV, CATBLK)
      RMAX = -1.E23
      RMIN = 1.E23
      DO 225 LIF = LBIF,LEIF
         WRITE (MSGTXT,1055) LIF
         CALL MSGWRT (2)
         DO 220 LCHAN = LBCHAN,LECHAN,CHINC
            IP = NCOL * NROW
            CALL RFILL (IP, 0.0, VIMAG1)
            CALL RFILL (IP, 0.0, VIMAG2)
            CALL RFILL (IP, 0.0, NIMAG)
C                                       init image IO
            IDEPTH(1) = (LCHAN - LBCHAN) / CHINC + 1
            IDEPTH(2) = LIF - LBIF + 1
            IDEPTH(3) = 1
            IDEPTH(4) = 1
            IDEPTH(5) = 1
            CALL COMOFF (CATIMG(KIDIM), CATIMG(KINAX), IDEPTH, IBLKOF,
     *         IERR)
            IBLKOF = IBLKOF + 1
            CALL MINIT ('WRIT', LUNO, FINDO, CATIMG(KINAX),
     *         CATIMG(KINAX+1), 0, RBUFF, NBYT, IBLKOF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'INIT WRITE TO OUTPUT'
               IRET = 4
               GO TO 990
               END IF
C                                       Init i/o to uv file
            BIF = LIF
            EIF = LIF
            BCHAN = LCHAN
            ECHAN = LCHAN + NCHAV - 1
            RPARM(1) = FBLANK
            DPARM(4) = -1.0
            CALL UVGET ('INIT', RPARM, VIS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'INIT READ OF INPUT UV DATA'
               IRET = 5
               GO TO 990
               END IF
C                                       Stokes value
            CATID(KDCRV+NAXST) = CATD(KDCRV+JLOCS)
C                                       read uv data loop
 100        CALL UVGET ('READ', RPARM, VIS, IERR)
            IF ((IERR.NE.4) .AND. (IERR.GE.0)) THEN
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READING UV DATA'
                  IRET = 5
                  GO TO 990
                  END IF
C                                       get first time
               IF (FIRST) THEN
                  FIRST = .FALSE.
                  VALUE = RPARM(ILOCT+1)
                  IF (LTYPE(1).EQ.2) CATID(KDCRV) = VALUE
                  IF (LTYPE(2).EQ.2) CATID(KDCRV+1) = VALUE
                  END IF
C                                       test for flagged
               DO 150 KCH = 1,NCHAV
                  IF (VIS(3,KCH).GT.0) THEN
                     UVMULT = 1.0D0 + (CFREQ(LIF) +
     *                  (LCHAN+KCH-1-CATSR(KRCRP+JLOCF)) * FINC(LIF)) /
     *                  UVFREQ
C                                       get image pixel
                     CALL GETPIX (NUMAN, UVMULT, VIS(1,KCH), RCOL, RROW,
     *                  OVIS, IERR)
                     IF (IERR.LT.0) THEN
                        NFAIL = NFAIL + 1
                     ELSE IF (IERR.EQ.1) THEN
                        NANTSK = NANTSK + 1
                     ELSE IF (IERR.EQ.2) THEN
                        NFLAGD = NFLAGD + 1
                     ELSE IF (IERR.EQ.0) THEN
                        NPOINT = NPOINT + DNP
C                                       Put in the current pixel
                        DO 120 IP = 1,2
                           IX = IROUND (RCOL(IP))
                           IY = IROUND (RROW(IP))
                           IF ((IX.GT.0) .AND. (IY.GT.0)) THEN
                              NGRID = NGRID + 1.0D0
                              VALUE1 = OVIS(1,IP)
                              VALUE2 = OVIS(2,IP)
                              IF ((ITYP.EQ.0) .OR. (ITYP.EQ.3) .OR.
     *                           (ITYP.EQ.4)) VALUE1 =
     *                           SQRT (VALUE1*VALUE1 + VALUE2*VALUE2)
                              IF ((ITYP.EQ.3) .OR. (ITYP.EQ.4))
     *                           VALUE2 = VALUE1 * VALUE1
                              IF (ROUND.LT.0) THEN
                                 VIMAG1(IX,IY) = VIMAG1(IX,IY) + VALUE1
                                 VIMAG2(IX,IY) = VIMAG2(IX,IY) + VALUE2
                                 NIMAG(IX,IY) = NIMAG(IX,IY) + 1.0
                              ELSE
                                 CALL DOCON (NCONV, CONV, RCOL(IP),
     *                              RROW(IP), VALUE1, VALUE2, NXY,
     *                              VIMAG1, VIMAG2, NIMAG)
                                 END IF
                              END IF
 120                       CONTINUE
                        END IF
                     END IF
 150              CONTINUE
               GO TO 100
               END IF
C                                       write out plane
            DO 200 IY = 1,NROW
               CALL MDISK ('WRIT', LUNO, FINDO, RBUFF, MPTR, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1130) IERR, IY
                  IRET = 4
                  GO TO 990
                  END IF
               DO 190 IX = 1,NCOL
                  J = MPTR + IX - 1
                  DIVIDR = NIMAG(IX,IY)
                  IF (DPARM(7).GT.0.0) DIVIDR = 1.0
                  IF (NIMAG(IX,IY).LE.0.0) THEN
                     IF (ITYP.EQ.7) THEN
                        RBUFF(J) = 0.0
                     ELSE
                        RBUFF(J) = FBLANK
                        END IF
C                                       scalar amp
                  ELSE IF (ITYP.EQ.0) THEN
                     RBUFF(J) = VIMAG1(IX,IY) / DIVIDR
C                                       vector amp
                  ELSE IF (ITYP.EQ.1) THEN
                     RBUFF(J) = SQRT (VIMAG1(IX,IY)**2+VIMAG2(IX,IY)**2)
     *                  / DIVIDR
C                                       vector phase
                  ELSE IF (ITYP.EQ.2) THEN
                     IF ((VIMAG1(IX,IY).EQ.0.) .AND.
     *                  (VIMAG2(IX,IY).EQ.0.)) THEN
                        RBUFF(J) = FBLANK
                     ELSE
                        RBUFF(J) = ATAN2 (VIMAG2(IX,IY), VIMAG1(IX,IY))
     *                     * 57.29578
                        END IF
C                                       rms amp
                  ELSE IF (ITYP.EQ.3) THEN
                     IF (NIMAG(IX,IY).EQ.1.0) THEN
                        RBUFF(J) = 0.0
                     ELSE
                        VIMAG1(IX,IY) = VIMAG1(IX,IY) / DIVIDR
                        VIMAG2(IX,IY) = VIMAG2(IX,IY) / DIVIDR
                        RBUFF(J) = VIMAG2(IX,IY) - VIMAG1(IX,IY)**2
                        RBUFF(J) = SQRT (MAX(0.0, RBUFF(J)))
                        END IF
C                                       rms amp / mean amp
                  ELSE IF (ITYP.EQ.4) THEN
                     IF (NIMAG(IX,IY).EQ.1.0) THEN
                        RBUFF(J) = 0.0
                     ELSE
                        VIMAG1(IX,IY) = VIMAG1(IX,IY) / DIVIDR
                        VIMAG2(IX,IY) = VIMAG2(IX,IY) / DIVIDR
                        RBUFF(J) = VIMAG2(IX,IY) / VIMAG1(IX,IY)**2 - 1.
                        RBUFF(J) = SQRT (MAX(0.0, RBUFF(J)))
                        END IF
C                                       real part
                  ELSE IF (ITYP.EQ.5) THEN
                     RBUFF(J) = VIMAG1(IX,IY) / DIVIDR
C                                       imaginary part
                  ELSE IF (ITYP.EQ.6) THEN
                     RBUFF(J) = VIMAG2(IX,IY) / DIVIDR
C                                       count
                  ELSE IF (ITYP.EQ.7) THEN
                     RBUFF(J) = NIMAG(IX,IY)
                     END IF
                  IF (RBUFF(J).NE.FBLANK) THEN
                     RMAX = MAX (RMAX, RBUFF(J))
                     RMIN = MIN (RMIN, RBUFF(J))
                     END IF
 190              CONTINUE
 200           CONTINUE
C                                       finish image plane IO
            CALL MDISK ('FINI', LUNO, FINDO, RBUFF, MPTR, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'FINISH WRITE IO'
               IRET = 4
               GO TO 990
               END IF
C                                       Close the uv IO too
            CALL UVGET ('CLOS', RPARM, VIS, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSING THE UV DATA'
               GO TO 990
               END IF
 220        CONTINUE
 225     CONTINUE
C                                       finish the header
      CALL COPY (256, CATIMG, CATBLK)
      IF (PMODE.GT.0) CATD(KDCRV+NAX) = IHMODE(PMODE)
      CATR(KRDMX) = RMAX
      CATR(KRDMN) = RMIN
C                                       single source out
      IF ((MULTI) .AND. (NSOUWD.EQ.1) .AND. (DOSWNT)) THEN
         CALL GETSOU (SOUWAN(1), DISKIN, CNOIN, CATUV, LUN, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'GETTING INFO OF THE ONE SOURCE'
            CALL MSGWRT (8)
         ELSE
            CALL CHR2H (8, SNAME, 1, CATH(KHOBJ))
            J = CATBLK(KIDIM) - 2
            CATD(KDCRV+J) = RAEPO * RAD2DG
            CATD(KDCRV+J+1) = DECEPO * RAD2DG
            END IF
         END IF
C                                       make an FQ table
      IF (CATBLK(KIDIM).EQ.7) THEN
         UVFREQ = CFREQ(LBIF)
         DO 250 J = LBIF,LEIF
            CFREQ(J) = CFREQ(J) - UVFREQ
            FINC(J) = FINC(J) * CHINC
 250        CONTINUE
         CHVER = 1
         NIFS = LEIF - LBIF + 1
         CALL CHNDAT ('WRIT', BUFFER, DISKOU, CNOOUT, CHVER, CATBLK,
     *      LUN, NIFS, CFREQ(LBIF), ISBAND(LBIF), FINC(LBIF),
     *      BNDCOD(LBIF), FRQSEL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING FQ TABLE INFO'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       save header
      CALL CATIO ('UPDT', DISKOU, CNOOUT, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'UPDATING HEADER ON DISK'
         CALL MSGWRT (6)
         END IF
C                                       summary
      WRITE (MSGTXT,1230) NPOINT
      CALL MSGWRT (4)
      WRITE (MSGTXT,1231) NGRID
      CALL MSGWRT (4)
      NFAIL = NFAIL + NPOINT - NGRID + 0.1D0
      WRITE (MSGTXT,1232) NFAIL
      CALL MSGWRT (4)
      WRITE (MSGTXT,1233) NANTSK
      CALL MSGWRT (4)
      WRITE (MSGTXT,1234) NFLAGD
      IF (NFLAGD.GT.0) CALL MSGWRT (4)
      IF (NGRID.LE.0.0D0) IRET = 6
      GO TO 999
C                                       Error message print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRIDUV: ERROR',I5,' ON ',A)
 1010 FORMAT ('SORT ORDER ''',2A1,''' NOT FULLY RECOGNIZED - USE UVSRT')
 1030 FORMAT ('GRIDUV: UNABLE TO READ ANTENNAS INFO - ERROR',2I5)
 1055 FORMAT ('GRIDUV: Begin IF',I4)
 1130 FORMAT ('GRIDUV: UNABLE TO WRITE OUTPUT IMAGE FILE - ERROR',I5,
     *   ' ROW',I5)
 1230 FORMAT ('Found   ',F13.0,' points to grid')
 1231 FORMAT ('Included',F13.0,' points in the grid')
 1232 FORMAT ('Dropped ',I12,'  points off the grid')
 1233 FORMAT ('Dropped ',I12,'  points due to antennas/baseline')
 1234 FORMAT ('Dropped ',I12,'  points due to flagging')
      END
      SUBROUTINE GETPIX (NUMAN, UVMULT, VIS, RCOL, RROW, OVIS, IERR)
C-----------------------------------------------------------------------
C   GETPIX returns the output image pixel.
C   Inputs:
C      NUMAN    I(10225) (1) # subarrays (2-513) # ant / subarray
C                        (514-1025) sum #baselines subarrays < subar
C      UVMULT   R        Scale factor for U, V, W
C   In:
C      VIS      R(3,*)   Visibilities
C   Output:
C      RCOL     R(2)     Column number
C      RROW     R(2)     Row number
C      OVIS     R(3,2)   Vis values to grid
C      IERR     I        error: -1 not in image
C                                0 in image
C                               +1 not desired
C   COMMONS:
C      In:     CATIMG  I        Image header
C      LTYPE    I(2)     Axis types - B, T, R, P, W, U, V, X, Y, Z, M
C                        for values 1 - 11
C      In/out  RPARM   R(*)     Random parameters
C-----------------------------------------------------------------------
      INTEGER   NUMAN(*), IERR
      REAL      UVMULT, VIS(3,*), RCOL(2), RROW(2), OVIS(3,2)
C
      INTEGER   I, J, IANT, JANT, IARR, ITEMP, NP, IP
      LOGICAL   REQBAS
      REAL      VALUE(2), U, V, W, CATIR(256), TEMP
      DOUBLE PRECISION CATID(128)
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATIMG, CATIR, CATID)
C-----------------------------------------------------------------------
      RCOL(1) = -1
      RCOL(2) = -1
      RROW(1) = -1
      RROW(2) = -1
      IERR = -1
      U = RPARM(ILOCU+1) * UVMULT
      V = RPARM(ILOCV+1) * UVMULT
      W = RPARM(ILOCW+1) * UVMULT
      CALL RCOPY (3, VIS, OVIS(1,1))
      CALL RCOPY (3, VIS, OVIS(1,2))
      IF (CATIR(KRCIC).EQ.0.0) GO TO 999
      IF (CATIR(KRCIC+1).EQ.0.0) GO TO 999
C                                       Check baseline
      IF (ILOCB.GE.0) THEN
         TEMP = RPARM(ILOCB+1)
         ITEMP = TEMP + 0.1
         IARR = 100.0 * (TEMP - ITEMP) + 1.5
         IANT = ITEMP / 256 + 0.1
         JANT = ITEMP - 256 * IANT + 0.1
      ELSE
         IANT = RPARM(ILOCA1+1)
         JANT = RPARM(ILOCA2+1)
         IARR = RPARM(ILOCSA+1)
         END IF
      IF (IANT.GT.JANT) THEN
         J = IANT
         IANT = JANT
         JANT = J
         U = -U
         V = -V
         W = -W
         OVIS(2,1) = -OVIS(2,1)
         OVIS(2,2) = -OVIS(2,2)
         END IF
C                                       how many samples
      NP = 1
      IF ((LTYPE(1).EQ.0) .OR. (LTYPE(2).EQ.0)) NP = 2
      IF ((LTYPE(1).EQ.6) .AND. (LTYPE(2).EQ.7)) NP = 2
      IF ((LTYPE(1).EQ.7) .AND. (LTYPE(2).EQ.6)) NP = 2
C                                       check for forced Hermitian
      IF (((LTYPE(2).EQ.8) .AND. (U.LT.0)) .OR.
     *   ((LTYPE(2).EQ.10) .AND. (U.LT.0)) .OR.
     *   ((LTYPE(2).EQ.9) .AND. (V.LT.0)) .OR.
     *   ((LTYPE(2).EQ.11) .AND. (V.LT.0))) THEN
         OVIS(2,1) = -OVIS(2,1)
         U = -U
         V = -V
         END IF
C                                       Do we want it?
      IF (.NOT.REQBAS (IANT, JANT, DESEL, IXANT, NXANT, IXBASL, NXBASL))
     *   THEN
         IERR = 1
      ELSE IF (VIS(3,1).LE.0.0) THEN
         IERR = 2
C                                       get values
      ELSE
         DO 100 IP = 1,NP
            DO 20 I = 1,2
               VALUE(I) = 0.
               IF (LTYPE(I).EQ.0) VALUE(I) = NUMAN(513+IARR) +
     *            (IANT-1) * (NUMAN(1+IARR) + 1) + JANT
               IF (LTYPE(I).EQ.1) VALUE(I) = NUMAN(513+IARR) + JANT -
     *            IANT + (IANT-1) * (2*NUMAN(1+IARR) + 4 - IANT) / 2 + 1
               IF (LTYPE(I).EQ.2) VALUE(I) = RPARM(ILOCT+1)
               IF (LTYPE(I).EQ.3) VALUE(I) = SQRT (U*U + V*V)
               IF (LTYPE(I).EQ.4) VALUE(I) = 57.29578 * ATAN2 (V, U)
               IF (LTYPE(I).EQ.5) VALUE(I) = W
               IF (LTYPE(I).EQ.6) VALUE(I) = U
               IF (LTYPE(I).EQ.7) VALUE(I) = V
               IF (LTYPE(I).EQ.8) VALUE(I) = ABS (U)
               IF (LTYPE(I).EQ.9) VALUE(I) = ABS (V)
               IF (LTYPE(I).EQ.10) VALUE(I) = ABS (U)
               IF (LTYPE(I).EQ.11) VALUE(I) = ABS (V)
 20            CONTINUE
C                                       get pixels: column
            TEMP = (VALUE(1) - CATID(KDCRV)) / CATIR(KRCIC) +
     *         CATIR(KRCRP)
            IF ((TEMP.GE.0.5) .AND. (TEMP.LT.CATIMG(KINAX)+0.5))
     *         RCOL(IP) = TEMP
C                                       row
            IF (RCOL(IP).GE.0.0) THEN
               TEMP = (VALUE(2) - CATID(KDCRV+1)) / CATIR(KRCIC+1) +
     *            CATIR(KRCRP+1)
               IF ((TEMP.GE.0.5) .AND. (TEMP.LT.CATIMG(KINAX+1)+0.5))
     *            RROW(IP) = TEMP
               END IF
C                                       switch for 2nd parameter
            I = JANT
            JANT = IANT
            IANT = I
            U = -U
            V = -V
 100        CONTINUE
         IERR = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE DOCON (NCONV, CONV, RCOL, RROW, V1, V2, NXY, VIMAG1,
     *   VIMAG2, NIMAG)
C-----------------------------------------------------------------------
C   DOCON convoles a value to the grids
C   Inputs:
C      RCOL     R      Column of sample
C      RROW     R      Row of sample
C      V1       R      First value (real part, ...)
C      V2       R      Second value (imag part, ...)
C      NXY      I(2)   X, Y dimenaions of images
C   In/out
C      VIMAG1   R(*)   Image of first parameter
C      VIMAG2   R(*)   Image of seond parameter
C      NIMAG    R(*)   Sum of convolutional weights
C-----------------------------------------------------------------------
      INTEGER   NCONV, NXY(2)
      REAL      CONV(NCONV,*), RCOL, RROW, V1, V2, VIMAG1(NXY(1),*),
     *   VIMAG2(NXY(1),*), NIMAG(NXY(1),*)
C
      INTEGER   I, J, IX, IY, LX, LY, KX, KY, IROUND, CENT, NOFF, J1,
     *   J2, I1, I2
      REAL      C
C-----------------------------------------------------------------------
      CENT = (NCONV + 1) / 2
      NOFF = NCONV / 200
      IX = RCOL + 0.5
      IY = RROW + 0.5
      LX = IROUND (100.0 * (IX - RCOL))
      LY = IROUND (100.0 * (IY - RROW))
      J1 = MAX (1-IY, -NOFF)
      I1 = MAX (1-IX, -NOFF)
      J2 = MIN (NXY(2)-IY, NOFF)
      I2 = MIN (NXY(1)-IX, NOFF)
      DO 20 J = J1,J2
         KY = CENT + J*100 + LY
         DO 10 I = I1,I2
            KX = CENT + I*100 + LX
            C = CONV(KX,KY)
            VIMAG1(IX+I,IY+J) = VIMAG1(IX+I,IY+J) + C * V1
            VIMAG2(IX+I,IY+J) = VIMAG2(IX+I,IY+J) + C * V2
            NIMAG(IX+I,IY+J) = NIMAG(IX+I,IY+J) + C
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE UVIMHI
C-----------------------------------------------------------------------
C   UVIMHI copies the history file from the input UV data set to the
C   output image and adds the local parameters.  It also copies other
C   extension files of type AN.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER HILINE*72, CTYP(7)*12, AXTYPE(12)*8
      INTEGER   HLUNI, HLUNO, IERR, I, I1, I2, ITYP, IROUND, NVER
      LOGICAL   SAVE
      INCLUDE 'UVIMG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA HLUNI, HLUNO /28,29/
      DATA SAVE /.TRUE./
      DATA CTYP /'Scalar ampl.', 'Vector ampl.', 'Vector phase',
     *   'Amplitud RMS', 'Amp RMS/mean', 'Real part   ',
     *   'Imag. part  '/
      DATA AXTYPE /'Ant pair', 'Baseline', 'Time    ',
     *   'UVradius', 'UVposang', 'W       ', 'U       ',
     *   'V       ', 'abs(U)  ', 'abs(V)  ', 'abs(U)  ',
     *   'abs(V)  '/
C-----------------------------------------------------------------------
C                                       copy header keywords
      CALL KEYCOP (DISKIN, CNOIN, DISKOU, CNOOUT, IERR)
      CALL HIINIT (3)
C                                       Create and copy history file.
      CALL HISCOP (HLUNI, HLUNO, DISKIN, DISKOU, CNOIN, CNOOUT,
     *   CATBLK, BUFFER(257), BUFFER, IERR)
      IF (IERR.GT.3) GO TO 110
      IF (IERR.EQ.3) GO TO 100
C                                       Add UVIMG history.
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, HLUNO,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, OUTNAM, OUTCLS, SEQOUT, DISKOU, HLUNO,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (HLUNO, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Task messages
      IF (DPARM(4).GT.0.0) THEN
         WRITE (HILINE,1010) TSKNAM
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       channel selection
      WRITE (HILINE,1025) TSKNAM, NCHAV
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1026) TSKNAM, CHINC
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       ANTENNA, BASELINE
      I1 = 1
      IF ((DESEL) .AND. (NXANT.GT.0)) THEN
         WRITE (HILINE,1030) TSKNAM
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NXANT.LE.0) THEN
         WRITE (HILINE,1031) TSKNAM
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
 35   I2 = I1 + 12
      IF (I2.GT.NXANT) I2 = NXANT
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1035) TSKNAM, (IXANT(I), I = I1,I2)
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 35
         END IF
      I1 = 1
      WRITE (HILINE,1040) TSKNAM
      IF (NXBASL.LE.0) WRITE (HILINE,1041) TSKNAM
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
 45   I2 = I1 + 12
      IF (I2.GT.NXBASL) I2 = NXBASL
      IF (I2.GE.I1) THEN
         WRITE (HILINE,1045) TSKNAM, (IXBASL(I), I = I1,I2)
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         I1 = I2 + 1
         GO TO 45
         END IF
C                                       data type
      ITYP = IROUND (DPARM(1)) + 2
      IF ((ITYP.EQ.2) .AND. (DPARM(2).GT.0.5)) ITYP = 1
      WRITE (HILINE,1050) TSKNAM, CTYP(ITYP)
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       axis type
      WRITE (HILINE,1055) TSKNAM, 'X', DOSORT(1), AXTYPE(LTYPE(1)+1)
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1055) TSKNAM, 'Y', DOSORT(2), AXTYPE(LTYPE(2)+1)
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       points included/dropped
      WRITE (HILINE,1061) TSKNAM, NPOINT
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1062) TSKNAM, NGRID
      CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (NFAIL.GT.0) THEN
         WRITE (HILINE,1063) TSKNAM, NFAIL
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NANTSK.GT.0) THEN
         WRITE (HILINE,1064) TSKNAM, NANTSK
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (NFLAGD.GT.0) THEN
         WRITE (HILINE,1065) TSKNAM, NFLAGD
         CALL HIADD (HLUNO, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Close HI file
 100  CALL HICLOS (HLUNO, SAVE, BUFFER, IERR)
C                                       Copy AN files
 110  NVER = 0
      CALL TABCOP ('AN', 0, NVER, HLUNI, HLUNO, DISKIN, DISKOU,
     *   CNOIN, CNOOUT, CATBLK, BUFFER(1), BUFFER(257), IERR)
      NVER = 0
      CALL TABCOP ('CH', 0, NVER, HLUNI, HLUNO, DISKIN, DISKOU,
     *   CNOIN, CNOOUT, CATBLK, BUFFER(1), BUFFER(257), IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A6,'/ Divided by the source flux')
 1025 FORMAT (A6,'NCHAV =',I5,'   / channels averaged in each plane')
 1026 FORMAT (A6,'CHINC =',I5,'   / channels skipped between planes')
 1030 FORMAT (A6,'/ All antennas except:')
 1031 FORMAT (A6,'/ All antennas')
 1035 FORMAT (A6,'ANTENNAS=',I3,12(',',I3),',')
 1040 FORMAT (A6,'/ With')
 1041 FORMAT (A6,'/ With all antennas')
 1045 FORMAT (A6,'BASELINES=',I3,12(',',I3),',')
 1050 FORMAT (A6,'/ Data type = ',3A4)
 1055 FORMAT (A6,A,'TYPE = ''',A1,'''  / axis type = ',A)
 1061 FORMAT (A6,'/ Found   ',F13.0,' points to grid')
 1062 FORMAT (A6,'/ Included',F13.0,' points in the grid')
 1063 FORMAT (A6,'/ Dropped ',I8,' points off the grid')
 1064 FORMAT (A6,'/ Dropped ',I8,' points due to antennas/baseline')
 1065 FORMAT (A6,'/ Dropped ',I8,' points due to flagging')
      END
