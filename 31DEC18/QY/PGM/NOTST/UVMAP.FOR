LOCAL INCLUDE 'UVMAP.INC'
C                                       Local include for UVMAP
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   ITV, NOGCOR, DISIN, DISOUT, MODE, NATWT, SEQIN, UNFBOX,
     *   NX, NY, CTYPX, CTYPY, ITAP, IZSP, ICNTRX, ICNTRY, INCNO,
     *   INDS(10), OUTSEQ, NCH, LRECIN, BUFSZ,
     *   NUMCH, CATOLD(256), ISCR(3), UVSCR, NUMMAP
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMOU(3), XSTOK, XWTFN
      CHARACTER NAMEIN*12, CLASIN*6, NAMOUT*12, STOKES*4, WTFN*4
      REAL      XSEQIN, XDISO, XDISIN, USETV, XUNFBX, XNX, XNY, XFLD,
     *   YFLD, XSHIFT, YSHIFT, TAPERU, TAPERV, BLMIN, BLMAX, ZSP(4),
     *   WTZSP, XGCOR, CTYPEX, CTYPEY, CPARMX(10), CPARMY(10),
     *   XGUARD(2), BADD(10), WROW(UVBFSS), XSEQ,  APSIZ, BANWID, XNCH,
     *   XNOCH, XSH, YSH, DXCOR, DYCOR, DXC, DYC, DZC, SCLU, SCLV, SCLW,
     *   MAPROT
      LOGICAL   NOSHFT
      COMPLEX   CBUF(UVBFSS), CBUF2(UVBFSS)
      COMMON /MAPCHR/ NAMEIN, CLASIN, NAMOUT, STOKES, WTFN
      COMMON /MAPPRM/ CATOLD, APSIZ, BANWID, XSH, YSH, DXCOR, DYCOR,
     *   DXC, DYC, DZC, SCLU, SCLV, SCLW, NOSHFT,
     *   ITV, NOGCOR, DISIN, DISOUT, OUTSEQ, NATWT, SEQIN, UNFBOX,
     *   NX, NY, CTYPX, CTYPY, ITAP, IZSP, ICNTRX, ICNTRY,
     *   INCNO, INDS, NCH, NUMCH, MODE, LRECIN,
     *   ISCR, UVSCR, NUMMAP, MAPROT
      COMMON /XPARM/ XNAMEI, XCLASI, XSEQIN, XDISIN, XNCH, XNOCH,
     *   XNAMOU, XDISO, XSEQ, XSTOK, XNX, XNY, XFLD, YFLD, XSHIFT,
     *   YSHIFT, TAPERU, TAPERV, BLMIN, BLMAX, XWTFN, XUNFBX, XGCOR,
     *   USETV, ZSP, WTZSP, CTYPEX, CTYPEY,  CPARMX, CPARMY, XGUARD,
     *   BADD
      COMMON /BUFRS/ CBUF, CBUF2, WROW, BUFSZ
C                                                          End UVMAP
LOCAL END
      PROGRAM UVMAP
C-----------------------------------------------------------------------
C! Image a UV data set
C# UV Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-2000, 2005-2006, 2008, 2015
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
C   UVMAP computes a map or set of maps from a presorted visibility
C   data base.
C   Adverbs:
C      Adverb name      size    Prgm. name       Default
C      INNAME         R(2)      NAMEIN
C      INCLASS        R(2)      CLASIN
C      INSEQ          R         SEQIN
C      INDISK         R         DISIN
C      CHANNEL        R         NCH              0
C      NUMCHAN        R         NUMCH            1
C      OUTNAME        R(3)      NAMOUT           NAMEIN
C      OUTDISK        R         DISOUT
C      OUTSEQ         R         OUTSEQ           next highest.
C      STOKES         C*4       ISTOK           'I'
C      IMSIZE         R(2)      NX,NY
C      CELLSIZE       R(2)      XFLD,YFLD
C      SHIFT          R(2)      XSHIFT,YSHIFT
C      UVTAPER        R(2)      TAPERU,TAPERV,ITAP
C      UVRANGE        R(2)      BLMIN,BLMAX
C      UVWTFN         R         NATWT            'UN'
C      UVBOX          R         UNFBOX
C      DOGRIDCR       R         NOGCOR
C      DOTV           R         ITV
C      ZEROSP         R(5)      ZSP,WTZSP,IZSP
C      XTYPE          R         CTYPX            5=Spheriodal
C      YTYPE          R         CTYPY            XTYPE
C      XPARM          R(10)     CPARMX           set in GRDFLT
C      YPARM          R(10)     CPARMY           set in GRDFLT
C      GUARD          R(2)      XGUARD           guard band in uv plane
C      BADDISK        R(10)     IBAD
C-----------------------------------------------------------------------
      INTEGER   IRET, ITIME(3), IOCNT(2), NPASS, MTYPE(10), MLOC(10),
     *   MAPNT, I, JBUFSZ, KAP, NEED
      LOGICAL   HERM
      REAL      FFRAC(10), TIME, BMAX, DUM1, DUM2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DAPM.INC'
      DATA HERM /.FALSE./
C-----------------------------------------------------------------------
C                                       Init program, interpret parms
      CALL MAPINI (NPASS, MTYPE, MLOC, FFRAC, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Read visibilities to internal
      CALL VISRD (IRET)
      IF (IRET.NE.0) GO TO 900
      CALL ZTIME (ITIME)
      CALL ZCPU (TIME, IOCNT)
      WRITE (MSGTXT,1000) ITIME, TIME
      CALL MSGWRT (1)
C                                       Uniform weight.
      IF (NATWT.NE.1) CALL UNIF (FFRAC(1), IRET)
      IF (IRET.NE.0) GO TO 900
      NEED = NY * (NX/2 + 1)
      NEED = (4 * NEED) / 1024
      DO 50 I = 1,NPASS
C                                       Grid data
         CALL CONGRD (MTYPE(I), MLOC(I), FFRAC(I), IRET)
         IF (IRET.NE.0) GO TO 900
         CALL ZTIME (ITIME)
         CALL ZCPU (TIME, IOCNT)
         WRITE (MSGTXT,1020) ITIME, TIME
         CALL MSGWRT (1)
C                                       Display sampling on TV
         IF ((I.EQ.1) .AND. (ITV.EQ.1)) CALL UVDISP
C                                       Do Fourier transform
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'UNABLE TO GET AP MEMORY IN MAIN LOOP'
            CALL MSGWRT (8)
            IRET = 10
            GO TO 900
            END IF
         JBUFSZ = 4  * UVBFSS
         CALL DSKFFT (NX, NY, -1, HERM, ISCR(2), ISCR(2), ISCR(1),
     *      JBUFSZ, CBUF, CBUF2, DUM1, DUM2, IRET)
         IF (IRET.NE.0) GO TO 900
         CALL QRLSE
         CALL ZTIME (ITIME)
         CALL ZCPU (TIME, IOCNT)
         WRITE (MSGTXT,1021) ITIME, TIME
         CALL MSGWRT (1)
C                                       Scale & write output maps
         MAPNT = I * 2
         CALL MAPOUT (MAPNT, FFRAC(I), BMAX, IRET)
         IF (IRET.NE.0) GO TO 900
         CALL ZTIME (ITIME)
         CALL ZCPU (TIME, IOCNT)
         WRITE (MSGTXT,1022) ITIME, TIME
         CALL MSGWRT (1)
 50   CONTINUE
C                                       If finished OK then only UV file
C                                       is left marked.
      NCFILE = 1
C                                       Close down
 900  CALL DIE (IRET, CBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Got vis data   at',I3,2(':',I2),' CPU time',F10.2)
 1020 FORMAT ('Finished gridding',I3,2(':',I2),' CPU time',F10.2)
 1021 FORMAT ('Finished fft   at',I3,2(':',I2),' CPU time',F10.2)
 1022 FORMAT ('Maps stored    at',I3,2(':',I2),' CPU time',F10.2)
      END
      SUBROUTINE MAPINI (NPASS, MTYPE, MLOC, FFRAC, IRET)
C-----------------------------------------------------------------------
C   MAPINI performs initialization functions for UVMAP
C   Output
C     NPASS      I     Number of FFT passes.
C     MTYPE(*)   I     Map type for each pass, (see CONGRD for details)
C     MLOC(*)    I     Offset in visibility record for first vis in
C                      each FFT pass. (used by CONGRD)
C     FFRAC(*)   R     Frequency correction factor(-1) for each FFT pass
C     IRET       I      = 0  = > ok,  = 1  = > fatal error
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, LL*8, MM*8, CFREQ*8, CSTOK*8, JYBM*8,
     *   CCLASS(9)*8, PROJ*4, UTYPE*2
      HOLLERITH OLDH(256)
      INTEGER   IERR, NPARM, IRET, IER, IMODE, IV, NPASS, MTYPE(10),
     *   MLOC(10), ICNO, ISIZE, ICLTYP(9,40), IMTYPE(5,20), IMLOC(5,10),
     *   II, I, NP(2), IROUND, INDEX, NEED, KAP
      LOGICAL   DOPROJ
      REAL      FFRAC(10), OLDR(256), OUFREQ
      DOUBLE PRECISION OLDD(128), STOK(9)
C      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      DATA NPARM, PRGNAM /68, 'UVMAP '/
      DATA   LL,          MM,      CFREQ,      CSTOK,       JYBM
     *   /'RA---SIN', 'DEC--SIN', 'FREQ    ', 'STOKES  ', 'JY/BEAM '/
      DATA STOK /1.0D0,0.0D0,2.0D0,3.0D0,4.0D0,1.0D0,0.0D0,1.0D0,0.0D0/
      DATA CCLASS /'IIM001  ','IBM001  ','QIM001  ',
     *   'UIM001  ','VIM001  ','RIM001  ',
     *   'RBM001  ','LIM001  ','LBM001  '/
      DATA ICLTYP /1,2,7*0,  1,2,3,4,5*0,  1,2,3,4,5,4*0, 1,2,5,6*0,
     *   6,7,7*0,  8,9,7*0,  6,7,8,6*0,  6,7,8,9,5*0,  18*0,
     *   1,2,7*1,  1,2,7*1,  1,2,7*1,  1,2,7*1,
     *   1,2,7*1,  1,2,7*1,  1,2,7*1,  1,2,7*1,  18*0,
     *   6,7,7*6,  6,7,7*6,  6,7,7*6,  6,7,7*6,
     *   6,7,7*6,  6,7,7*6,  6,7,7*6,  6,7,7*6,  18*0,
     *   8,9,7*8,  8,9,7*8,  8,9,7*8,  8,9,7*8,
     *   8,9,7*8,  8,9,7*8,  8,9,7*8,  8,9,7*8,  18*0/
      DATA IMLOC /1,0,0,0,0, 1,2,0,0,0, 1,2,4,0,0, 1,2,0,0,0,
     *   1,0,0,0,0, 1,0,0,0,0, 1,2,0,0,0, 1,2,0,0,0, 5*0,
     *   1,2,4,6,8/
       DATA IMTYPE/1,0,0,0,0, 1,2,0,0,0, 1,2,3,0,0, 1,3,0,0,0,
     *   1,0,0,0,0, 1,0,0,0,0, 1,3,0,0,0, 1,1,0,0,0, 10*0,
     *   1,0,0,0,0, 1,3,0,0,0, 1,4,0,0,0, 1,4,3,0,0, 1,4,4,0,0,
     *   1,4,4,3,0, 1,4,4,4,0, 1,4,4,4,3, 10*0/
C-----------------------------------------------------------------------
      IRET = 0
      NCFILE = 0
      NSCR = 0
C                                       file common
      DO 10 I = 1,10
         SCRVOL(I) = 0
         LUNS(I) = 15 + I
 10      CONTINUE
C                                       system commons
      CALL ZDCHIN (.TRUE., CBUF)
      CALL VHDRIN
      BUFSZ = UVBFSS  * 4
C                                       get task parms
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XNAMEI, CBUF(1), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         CALL MSGWRT (8)
         END IF
      IF (RQUICK) CALL RELPOP (IRET, CBUF, IER)
      IF (IRET.NE.0) GO TO 990
C                                       Get catalog block, open file
C                                       Will be used and closed in VISRD
      SEQIN = IROUND (XSEQIN)
      DISIN = IROUND (XDISIN)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (4, 1, XSTOK, STOKES)
      CALL H2CHR (4, 1, XWTFN, WTFN)
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISIN, NAMEIN, CLASIN, SEQIN, UTYPE,
     *   NLUSER, LUNS(2), INDS(2), INCNO, CATOLD, CBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                        Update /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISIN
      FCNO(NCFILE) = INCNO
      FRW(NCFILE) = 0
C                                       Get info.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Disallow packed uv data
      IF (CATBLK(KINAX).EQ.1) THEN
         IRET = 9
         MSGTXT = 'ERROR: I cannot process packed UV data, use SPLIT'
         GO TO 990
         END IF
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check UV data type
      IF (TYPUVD.GT.0) THEN
C                                       Not interferometer data.
         IRET = 5
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       interprete input parms
      CALL DPARM (NPASS, OUFREQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1041) IRET
         GO TO 990
         END IF
      BANWID = OLDR(KRCIC+JLOCF)
      DO 100 I = 1,NUMMAP
C                                       Construct headers
         CALL COPY (256, CATOLD, CATBLK)
C                                       Check input projection type
         INDEX = ILOCU * 2
         CALL H2CHR (4, 1, CATH(KHPTP+INDEX+1), PROJ)
         DOPROJ = PROJ .NE.'    '
         CALL CHR2H (8, JYBM, 1, CATH(KHBUN))
         CALL CHR2H (8, LL, 1, CATH(KHCTP))
         CALL CHR2H (8, MM, 1, CATH(KHCTP+2))
         CALL CHR2H (8, CFREQ, 1, CATH(KHCTP+4))
         CALL CHR2H (8, CSTOK, 1, CATH(KHCTP+6))
         IF (DOPROJ) CALL CHR2H (4, PROJ, 5, CATH(KHCTP))
         IF (DOPROJ) CALL CHR2H (4, PROJ, 5, CATH(KHCTP+2))
         CATD(KDCRV) = RA
         CATD(KDCRV+1) = DEC
C                                       Frequency
         CATD(KDCRV+2) = FREQ + (NCH - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)
C                                       Check for line maps.
         II = I
         IF (II.EQ.1) II = 2
         IF (MODE.GT.10) CATD(KDCRV+2) = FREQ + (NCH + II - 2 -
     *      OLDR(KRCRP+JLOCF)) * OLDR(KRCIC+JLOCF)
C                                       Check for MODE=8 (override freq)
         IF ((MODE.EQ.8) .AND. (I.GT.2))
     *      CATD(KDCRV+2) = FREQ + OUFREQ
C                                       Check for MODE=6 with override.
         IF ((MODE.EQ.6) .AND. (ABS(OUFREQ).GT.1.0E-5))
     *      CATD(KDCRV+2) = FREQ + OUFREQ
C                                       Stokes' type.
         IMODE = ICLTYP(I,MODE)
         CATD(KDCRV+3) = STOK(IMODE)
         CATR(KRCIC) = -XFLD / NX / 3.6D3
         CATR(KRCIC+1) = YFLD / NY / 3.6D3
         CATR(KRCIC+2) = BANWID
         CATR(KRCIC+3) = 1.D0
         CATR(KRCRP) = ICNTRX + DXCOR
         CATR(KRCRP+1) = NY - ICNTRY + 2 - DYCOR
         CATR(KRCRP+2) = 1.
         CATR(KRCRP+3) = 1.
         CALL ROTFND (OLDR, CATR(KRCRT+1), IERR)
         CATR(KRCRT+0) = 0.0
         CATR(KRCRT+2) = 0.
         CATR(KRCRT+3) = 0.
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
         CATR(KRBLK) = 0.
         CATR(KRXSH) = CATR(KRXSH) + XSH
         CATR(KRYSH) = CATR(KRYSH) + YSH
         CATBLK(KIGCN) = 0
         CATBLK(KIPCN) = 0
         CATBLK(KIDIM) = 4
         CATBLK(KINAX) = NX
         CATBLK(KINAX+1) = NY
         CATBLK(KINAX+2) = 1
         CATBLK(KINAX+3) = 1
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CCLASS(IMODE), KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMU) = NLUSER
C                                       Set sequence number.
         CATBLK(KIIMS) = OUTSEQ
         IF ((MODE.GE.10) .AND. (I.GT.2))
     *      CATBLK(KIIMS) = OUTSEQ + I - 2
C                                       Create output map file.
         CALL MCREAT (DISOUT, ICNO, WROW, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1070) IRET
            GO TO 990
            END IF
C                                       Mark in /CFILE/
         NCFILE = NCFILE + 1
         FCNO(NCFILE) = ICNO
         FVOL(NCFILE) = DISOUT
         FRW(NCFILE) = 2
C                                       If I = 1 get actual OUTSEQ
         IF (I.EQ.1) OUTSEQ = CATBLK(KIIMS)
 100     CONTINUE
C                                       Save UV CATBLK
      CALL COPY (256, CATOLD, CATBLK)
C                                       Set MTYPE
      IF (MODE.LE.10) CALL COPY (5, IMTYPE(1,MODE), MTYPE)
      IMODE = MOD (MODE, 10) + 10
      IF (MODE.GE.11) CALL COPY (5, IMTYPE(1,IMODE), MTYPE)
C                                       Set MLOC
      IF (MODE.LE.10) CALL COPY (5, IMLOC(1,MODE), MLOC)
      IF (MODE.GE.11) CALL COPY (5, IMLOC(1,10), MLOC)
C                                       Set FFRAC
C                                       FFRAC = frequency correction
C                                       factor - 1
      IF (MODE.LE.10) THEN
         DO 110 IV = 1,5
            FFRAC(IV) = ((NCH - OLDR(KRCRP+JLOCF)) *
     *         OLDR(KRCIC+JLOCF)) / FREQ
 110        CONTINUE
      ELSE
         FFRAC(1) = ((NCH - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQ
         FFRAC(2) = ((NCH + 1 - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQ
         IF (NUMCH.GE.3) FFRAC(2) = ((NCH + 1.5 -
     *      OLDR(KRCRP+JLOCF)) * OLDR(KRCIC+JLOCF)) / FREQ
         FFRAC(3) = ((NCH + 3 - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQ
         IF (NUMCH.GE.5) FFRAC(3) = ((NCH + 3.5 -
     *      OLDR(KRCRP+JLOCF)) * OLDR(KRCIC+JLOCF)) / FREQ
         FFRAC(4) = ((NCH + 5 - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQ
         IF (NUMCH.GE.7) FFRAC(4) = ((NCH + 5.5 -
     *      OLDR(KRCRP+JLOCF)) * OLDR(KRCIC+JLOCF)) / FREQ
         FFRAC(5) = ((NCH + 7 - OLDR(KRCRP+JLOCF)) *
     *      OLDR(KRCIC+JLOCF)) / FREQ
         END IF
      IF (MODE.EQ.8) FFRAC(2) = OUFREQ / FREQ
      IF ((MODE.EQ.6) .AND. (ABS(OUFREQ).GT.1.0E-5))
     *   FFRAC(1) = OUFREQ / FREQ
C                                       Create scratch: UV grid & work;
C                                       other for FFT output
      CALL UVSIZE (LRECIN, NVIS, ISIZE)
      CALL SCREAT (ISIZE, WROW, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1130) IRET
         GO TO 990
         END IF
      UVSCR = NSCR
      NP(1) = 2 * NX
      NP(2) = NY
      CALL MAPSIZ (2, NP, ISIZE)
      DO 150 I = 1,2
         CALL SCREAT (ISIZE, WROW, IRET)
         ISCR(I) = NSCR
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.1) WRITE (MSGTXT,1140)
            IF (IRET.GT.1) WRITE (MSGTXT,1141)
            GO TO 990
            END IF
 150     CONTINUE
C                                       set AP size
      NEED = 4 * NY * (NX/2 + 1)
      NEED = NEED / 1024
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = 0
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = (NY * (NX/2 + 1)) / 2
         NEED = NEED / 1024
         NEED = MIN (16 * 1024, NEED) + 4
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            MSGTXT = 'MAPINI: CANNOT GET NEEDED MEMORY'
            IERR = 8
            GO TO 990
            END IF
         END IF
      APSIZ = 1024 * PSAPNW
      CALL QRLSE
C                                       Set gridding function defaults.
      CALL GRDFLT (CTYPX, CTYPY, CPARMX, CPARMY)
      GO TO 999
C                                       error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TROUBLE GETTING PARMS: IERR=',I7)
 1040 FORMAT ('ERROR: DATA IS NOT INTERFEROMETER TYPE DATA')
 1041 FORMAT ('TROUBLE INTERPRETING PARMS: IERR=',I7)
 1030 FORMAT ('ERROR',I3,' FINDING UVDATA')
 1070 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1130 FORMAT ('ERROR',I3,' CREATING UV SCRATCH FILE')
 1140 FORMAT ('TOO LITTLE DISK SPACE FOR SCRATCH FILE')
 1141 FORMAT ('TROUBLE CREATING SCRATCH FILE')
      END
      SUBROUTINE DPARM (NPASS, OUFREQ, IER)
C-----------------------------------------------------------------------
C   DPARM decodes integer and some character input parameters and sets
C   some default values.
C   NOTE: LRECIN has a minimum size of 8 to allow a work space in
C   the AP for tapering and shifting in CONGRD
C   Output:
C     NPASS  I    Number of FFT passes required.
C     OUFREQ R    Override frequency.
C     IER    I    error code:  0=>OK,   8=>bad input parameter.
C-----------------------------------------------------------------------
      CHARACTER BLANK*6, STEMP*6, RSTOKE(10)*4
      INTEGER   MLEN(40), MMAP(40), MNPASS(40), NPASS, I, IER, IROUND,
     *   IERR, IX, IY
      REAL      EPS, OUFREQ, OLDR(128)
      DOUBLE PRECISION XRA, XDEC
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (CATOLD, OLDR)
      DATA RSTOKE /'I   ','IQU ','IQUV','IV','R   ','L   ','RL  ',
     *   'LINE','RLIN','LLIN'/
      DATA EPS /0.01/
      DATA MLEN /8,10,12,8,8,8,8,10,0,0,
     *   8,8,10,12,14,16,18,20,0,0,
     *   8,8,10,12,14,16,18,20,0,0,
     *   8,8,10,12,14,16,18,20,0,0/
      DATA MMAP /2,4,5,3,2,2,3,4,0,0,
     *   2,3,4,5,6,7,8,9,0,0,
     *   2,3,4,5,6,7,8,9,0,0,
     *   2,3,4,5,6,7,8,9,0,0/
      DATA MNPASS /1,2,3,2,1,1,2,2,0,0,
     *   1,2,2,3,3,4,4,5,0,0,
     *   1,2,2,3,3,4,4,5,0,0,
     *   1,2,2,3,3,4,4,5,0,0/
C-----------------------------------------------------------------------
      IER = 0
C                                       Get parameters.
      DISOUT = IROUND (XDISO)
      OUTSEQ = IROUND (XSEQ)
C                                       Default NAMEOUT = NAMEIN
      STEMP = '      '
      CALL MAKOUT (NAMEIN, CLASIN, SEQIN, BLANK, NAMOUT, STEMP, OUTSEQ)
C                                       Decode Stokes parms.
      DO 10 I = 1,10
         MODE = I
         IF (RSTOKE(MODE).EQ.STOKES) GO TO 20
 10      CONTINUE
      MODE = 1
C                                       Channel # (spectral line)
 20   NCH = 1
      IF (XNCH.GT.0.0) NCH = XNCH + EPS
C                                       Number of channels.
      NUMCH = 1
      IF (XNOCH.GT.0.0) NUMCH = XNOCH + EPS
      IF (NUMCH.GT.8)   NUMCH = 8
C                                       Check line MODE
      IF (MODE.EQ.8)  MODE = 10 + NUMCH
      IF (MODE.EQ.9)  MODE = 20 + NUMCH
      IF (MODE.EQ.10) MODE = 30 + NUMCH
C                                       Check LL freq. overide
      IF ((MODE.EQ.7) .AND. (CPARMX(9).GT.0.5)) MODE = 8
C                                       Set # maps, # FFT, record length
      NUMMAP = MMAP(MODE)
      NPASS  = MNPASS(MODE)
      LRECIN = MLEN(MODE)
C                                       TV
      ITV = 0
      IF (USETV.GT.0.0) ITV = 1
      IF (NTVDEV.LE.0) ITV = 0
C                                       Weighting.
      NATWT = 1
      IF (WTFN.NE.'NA  ') NATWT = 0
C                                       MAPPNTS (NX,NY)
      NX = XNX + EPS
      NY = XNY + EPS
C                                       Check if (NX,NY) is valid.
      CALL POWER2 (NX, IX)
      CALL POWER2 (NY, IY)
C                                       Invalid (NX,NY)
      IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG) .OR.
     *   (NY.NE.IY) .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) THEN
         WRITE (MSGTXT,1030) NX, NY
         CALL MSGWRT (8)
         IER = 8
         GO TO 999
         END IF
C                                       cellsize -> FLDVU
      XFLD = XFLD * NX
      YFLD = YFLD * NY
      IF ((XFLD.LE.0.0) .OR. (YFLD.LE.0.0)) THEN
         WRITE (MSGTXT,1040)
         CALL MSGWRT (8)
         IER = 8
         GO TO 999
         END IF
C                                       Taper.
      TAPERU = MAX (0.0, TAPERU)
      TAPERV = MAX (0.0, TAPERV)
      ITAP = 0
      IF ((TAPERU.GT.0.0) .OR. (TAPERV.GT.0.0)) ITAP = 2
C                                       Map center.
      ICNTRX = NX / 2
      ICNTRY = NY / 2 + 1
C                                       Check defaults for BLLIM
      BLMIN = MAX (0.0, BLMIN)
      IF (BLMAX.LE.0.0) BLMAX = 1.0E15
C                                       Zero spacing flux densities.
      IZSP = 0
      IF ((ZSP(1).NE.0.0) .OR. (ZSP(2).NE.0.0) .OR. (ZSP(3).NE.0.0)
     *   .OR. (ZSP(4).NE.0.0)) IZSP = 1
C                                       Gridding correction.
      NOGCOR = 0
      IF (XGCOR.LE.0.) NOGCOR = 1
C                                       Convolution types
      CTYPX = CTYPEX + SIGN (EPS, CTYPEX)
      CTYPY = CTYPEY + SIGN (EPS, CTYPEY)
      UNFBOX = XUNFBX + 0.1
C                                       Bad disks.
      DO 60 I = 1,10
         IBAD(I) = BADD(I) + EPS
 60      CONTINUE
C                                       Override frequency.
C                                       Fix VLA problem here.
      OUFREQ = 0.0D0
      IF (MODE.EQ.8) OUFREQ = CPARMX(10)
      IF ((MODE.EQ.6) .AND. (CPARMX(9).GE.0.5)) OUFREQ = CPARMX(10)
C                                       Set for shift
      NOSHFT = (XSHIFT.EQ.0.0) .AND. (YSHIFT.EQ.0.0)
C                                       Get header shift and coor.
C                                       offset.
      CALL ROTFND (OLDR, MAPROT, IERR)
      MAPROT = MAPROT * DG2RAD
      XSH = COS (MAPROT) * XSHIFT - SIN (MAPROT) * YSHIFT
      YSH = SIN (MAPROT) * XSHIFT + COS (MAPROT) * YSHIFT
C                                       Field offsets.
      MAPROT = MAPROT / DG2RAD
      CALL XYSHFT (RA, DEC, XSHIFT, YSHIFT, MAPROT, XRA, XDEC)
      XSH = XRA - RA
      YSH = XDEC - DEC
      CALL SHFCRP (TYPUVD, RA, DEC, MAPROT, XRA, XDEC, DXCOR, DYCOR)
      DXCOR = DXCOR * NX /  XFLD
      DYCOR = DYCOR * NY /  YFLD
C                                       Check sort order.
      IF (ISORT(1:1).NE.'X') THEN
         WRITE (MSGTXT,1060) ISORT, 'X*'
         CALL MSGWRT (8)
         IER = 8
         GO TO 999
         END IF
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('DPARM: NX,NY = ',2I6,' ARE ILLEGAL')
 1040 FORMAT ('DPARM: FIELD OF VIEW IS <= ZERO')
 1060 FORMAT ('DPARM: SORT ORDER =',A2,' NOT ',A2)
      END
      SUBROUTINE VISRD (IER)
C-----------------------------------------------------------------------
C   VISRD gets necessary information from the catalog header and
C   reads the uv data and converts to the form expected in the rest
C   of the program.
C   Output:
C      IER   I    Return error code: 0=>OK, otherwise an error occured.
C-----------------------------------------------------------------------
      INTEGER   IER, LRECO, LENIN, JADR(2,8), ICHK, I, J, STYPE, INIO,
     *   NIOUT, BIND1, BIND2, MVIS, IFNUM, IBUFSZ, IPOINT, OPOINT,
     *   OCNT, BO, VO
      CHARACTER PHNAME*48
      REAL      WT, INBUF(1), OUTBUF(1), BLEN, BLMN2, BLMX2, DU, DV, DW,
     *   UMAX, VMAX, GCOR, SFACT(2,8), VSREC(20), UU, VV, WW, AUMAX,
     *   AVMAX, BUMAX, BVMAX, UTFACT, VTFACT
      LOGICAL   T, F, ALLWT
      INTEGER   XCOUNT
      COMPLEX   VS(8), CMPLX, CONJG
      DOUBLE PRECISION XRA, XDEC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUF, CBUF),     (OUTBUF, CBUF2)
      EQUIVALENCE (UU, VSREC(1)), (VV, VSREC(2)), (WW, VSREC(3)),
     *   (WT, VSREC(4)), (VS, VSREC(5))
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IBUFSZ = UVBFSS * 4
      LRECO = LRECIN
C                                       Determine some constants.
      DU = 2.06264E5 / XFLD
      DV = 2.06264E5 / YFLD
      DW = 1.0
C                                       Set limits.
      UMAX = (NX/2-0.5) * DU
      VMAX = (NY/2-0.5) * DV
      AUMAX = MAX (6.0, CPARMX(1)) * DU / UMAX
      AVMAX = MAX (6.0, CPARMY(1)) * DV / VMAX
C                                       taper factors
      UTFACT = 1.0
      VTFACT = 1.0
      IF (TAPERU.GT.0.0) UTFACT = EXP (LOG(0.3) *
     *   ((UMAX*0.7) / (1000. * TAPERU))**2)
      IF (TAPERV.GT.0.0) VTFACT = EXP (LOG(0.3) *
     *   ((VMAX*0.7) / (1000. * TAPERV))**2)
      UTFACT = SQRT (UTFACT)
      VTFACT = SQRT (VTFACT)
C                                       default guard band widths
      IF (XGUARD(1).LT.-0.001) THEN
         XGUARD(1) = AUMAX
      ELSE IF ((XGUARD(1).LE.0.001) .OR. (XGUARD(1).GT.0.9)) THEN
         XGUARD(1) = 0.3 * UTFACT
         END IF
      XGUARD(1) = MAX (XGUARD(1), AUMAX)
      IF (XGUARD(2).LT.-0.001) THEN
         XGUARD(2) = AVMAX
      ELSE IF ((XGUARD(2).LE.0.001) .OR. (XGUARD(2).GT.0.9)) THEN
         XGUARD(2) = 0.3 * VTFACT
         END IF
      XGUARD(2) = MAX (XGUARD(2), AVMAX)
C                                       Max is edge - guard
      UMAX = UMAX * (1.0 - XGUARD(1))
      VMAX = VMAX * (1.0 - XGUARD(2))
      BLMX2 = 1.0E6 * BLMAX * BLMAX
      BLMN2 = 1.0E6 * BLMIN * BLMIN
      AUMAX = 0.0
      AVMAX = 0.0
      BUMAX = 0.0
      BVMAX = 0.0
C                                       Correct to highest frequency.
C                                       BUG IF CIC IS < 0
      GCOR = (FREQ + (NCH+NUMCH-1 - CATR(KRCRP+JLOCF)) *
     *   CATR(KRCIC+JLOCF)) / FREQ
C                                       Check for MODE=8
      IF (MODE.EQ.8) GCOR = (FREQ + MAX (0.0, CPARMX(10))) / FREQ
      UMAX = UMAX / GCOR
      VMAX = VMAX / GCOR
      BLMX2 = BLMX2 / (GCOR * GCOR)
      BLMN2 = BLMN2 / (GCOR * GCOR)
C                                       Rotate and shift
      NOSHFT = T
      IF ((XSHIFT.NE.0.0) .OR. (YSHIFT.NE.0.0)) NOSHFT = F
C                                       calc U,V,W field shift terms
C                                       Field offsets.
      CALL XYSHFT (RA, DEC, XSHIFT, YSHIFT, MAPROT, XRA, XDEC)
C                                       set shift terms for field cent.
C                                       -NCP projection
      IF (TYPUVD.EQ.-1) THEN
         CALL SHINCP (RA, DEC, MAPROT, XRA, XDEC, DXC, DYC, DZC)
C                                       -SIN projection
      ELSE
         CALL SHISIN (RA, DEC, MAPROT, XRA, XDEC, DXC, DYC, DZC)
         END IF
C                                       Use opposite phase convention
      DXC = -DXC
      DYC = -DYC
      DZC = -DZC
C                                       NCH is the start channel no.
      IF (NCH.GT.CATBLK(KINAX+JLOCF)) THEN
         WRITE (MSGTXT,1000) NCH, CATBLK(KINAX+JLOCF)
         IER = 8
         GO TO 980
         END IF
      STYPE = MODE
      IF (MODE.EQ.8) STYPE = 7
C                                       For now IF=1
      IFNUM = 1
      CALL SETVIS (STYPE, NCH, IFNUM, MVIS, JADR, SFACT, ALLWT, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1010)
         GO TO 980
         END IF
C                                       Check record length.
      ICHK = 4 + MVIS*2
      ICHK = MAX (8, ICHK)
C                                       Special case.
      IF (MODE.EQ.8) ICHK = LRECIN
      IF (LRECIN.NE.ICHK) THEN
         WRITE (MSGTXT,1015) LRECIN, ICHK
         IER = 9
         GO TO 980
         END IF
C                                       Determine length of input
C                                       and buffer.
      LENIN = 0
C                                       Open scratch file.
      CALL ZPHFIL ('SC', SCRVOL(UVSCR), SCRCNO(UVSCR), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(1), INDS(1), SCRVOL(UVSCR), PHNAME, T, T, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 980
         END IF
C                                       Init files.
C                                       Input file opened in MAPINI
      INIO = LENIN
      CALL UVINIT ('READ', LUNS(2), INDS(2), NVIS, VO, LREC, INIO,
     *   IBUFSZ, INBUF, BO, BIND2, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1050) 'READ', IER
         GO TO 980
         END IF
      NIOUT = 0
      CALL UVINIT ('WRIT', LUNS(1), INDS(1), NVIS, VO, LRECO, NIOUT,
     *   IBUFSZ, OUTBUF, BO, BIND1, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1050) 'WRIT', IER
         GO TO 980
         END IF
      OPOINT = BIND1
      OCNT = 0
      XCOUNT = 0
C                                       Initialize visibilities.
      DO 80 I = 1,8
         VS(I) = CMPLX (0.,0.)
 80      CONTINUE
C                                       Conversion to cells done in
C                                       gridding routine in AP.
      SCLU = 1.0 / DU
C                                       Flip sign on v to make maps come
C                                       out upside down.
      SCLV = - 1.0 / DV
      SCLW = 1.0 / DW
C                                       Begin loop reading and convertin
 100  CONTINUE
C                                       Read buffer.
         CALL UVDISK ('READ', LUNS(2), INDS(2), INBUF, INIO, BIND2, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IER
            GO TO 980
            END IF
         IPOINT = BIND2
         IF (INIO.LE.0) GO TO 210
C                                       Loop thru buffer.
         DO 200 I = 1,INIO
C                                       Convert record. Correct for
C                                       frequency in CONGRD in AP.
            UU = INBUF(IPOINT+ILOCU)
            VV = INBUF(IPOINT+ILOCV)
            WW = INBUF(IPOINT+ILOCW)
C                                       Check if data desired.
            BLEN = UU*UU + VV*VV
            IF ((BLEN.LT.BLMN2) .OR. (BLEN.GT.BLMX2)) GO TO 190
            AUMAX = MAX (AUMAX, ABS(UU))
            AVMAX = MAX (AVMAX, ABS(VV))
            IF ((ABS(UU).GE.UMAX) .OR. (ABS(VV).GE.VMAX)) GO TO 190
            BUMAX = MAX (BUMAX, ABS(UU))
            BVMAX = MAX (BVMAX, ABS(VV))
C                                       Get visibilities.
            CALL GETVIS (STYPE, MVIS, JADR, SFACT, ALLWT,
     *         INBUF(IPOINT+NRPARM), WT, VS, IER)
            IF (IER.NE.0) GO TO 190
C                                       Put u,v,w on proper side of grid
            IF (UU.LT.0.0) THEN
               UU = -UU
               VV = -VV
               WW = -WW
               DO 140 J = 1,MVIS
                  VS(J) = CONJG (VS(J))
 140              CONTINUE
               END IF
C                                       Copy to output, note use of
C                                       EQUIVALENCE.
            DO 160 J = 1,LRECO
               OUTBUF(OPOINT+J-1) = VSREC(J)
 160           CONTINUE
C                                       Count vis. rec.
            XCOUNT = XCOUNT + 1
C                                       Check if time for a write.
            OPOINT = OPOINT + LRECO
            OCNT = OCNT + 1
C                                       Time for write.
            IF (OCNT.GE.NIOUT) THEN
               NIOUT = OCNT
               CALL UVDISK ('WRIT', LUNS(1), INDS(1), OUTBUF, NIOUT,
     *            BIND1, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT', IER
                  GO TO 980
                  END IF
               OPOINT = BIND1
               OCNT = 0
               END IF
C                                       Update input buffer.
 190        IPOINT = IPOINT + LREC
 200        CONTINUE
C                                       Loop back for next input buffer.
         GO TO 100
C                                       Flush output buffer.
 210  NIOUT = -OCNT
      CALL UVDISK ('FLSH', LUNS(1), INDS(1), OUTBUF, NIOUT, BIND1, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IER
         GO TO 980
         END IF
C                                        Check that some data given
      IF (XCOUNT.LT.1) THEN
         IER = 8
         WRITE (MSGTXT,1220)
         GO TO 980
         END IF
C                                       data outside grid?
      IF ((AUMAX.GT.UMAX) .OR. (AVMAX.GT.VMAX)) THEN
         MSGTXT = '**** WARNING data discarded outside usable part of'
     *      // ' UV plane ****'
         CALL MSGWRT (6)
         IF (AUMAX.GT.UMAX) THEN
            VV = ABS (XFLD / NX)
            UU = UMAX/AUMAX * VV
            WRITE (MSGTXT,1230) 'U', AUMAX, UMAX
            CALL MSGWRT (6)
            WRITE (MSGTXT,1231) UU, VV
            CALL MSGWRT (6)
            WRITE (MSGTXT,1232) 'U', XGUARD(1)
            CALL MSGWRT (6)
            END IF
         IF (AVMAX.GT.VMAX) THEN
            VV = ABS (YFLD / NY)
            UU = VMAX/AVMAX * VV
            WRITE (MSGTXT,1230) 'V', AVMAX, VMAX
            CALL MSGWRT (6)
            WRITE (MSGTXT,1231) UU, VV
            CALL MSGWRT (6)
            WRITE (MSGTXT,1232) 'V', XGUARD(2)
            CALL MSGWRT (6)
            END IF
         END IF
      IF ((BUMAX.GT.(1-0.35*UTFACT)*(NX/2-0.5)*DU) .OR.
     *   (BVMAX.GT.(1-0.35*VTFACT)*(NY/2-0.5)*DV)) THEN
         MSGTXT = '**** WARNING data included out of inner portion'
     *      // ' of UV plane ****'
         CALL MSGWRT (6)
         MSGTXT = '**** Watch for high-frequency & other poor ' //
     *      'cleaning effects ****'
         CALL MSGWRT (6)
         END IF
C                                       Get least lambda limit
      UMAX = MIN (UMAX, VMAX)
      UMAX = MIN (UMAX, SQRT(BLMX2))
C                                       Tell how much data to be used.
      WRITE (MSGTXT,1235,ERR=240) XCOUNT, NVIS, UMAX
      IF (XCOUNT.EQ.NVIS) WRITE (MSGTXT,1236,ERR=240) NVIS
 240  CALL MSGWRT (4)
C                                        Set new no. vis.
      NVIS = XCOUNT
C                                       Close files.
      CALL ZCLOSE (LUNS(1), INDS(1), IER)
      CALL ZCLOSE (LUNS(2), INDS(2), IER)
      IER = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VISRD: CHANNEL ',I4,' .GT. MAX=',I4)
 1010 FORMAT ('VISRD: REQUESTED DATA NOT AVAILABLE IN DATABASE')
 1015 FORMAT ('VISRD: EXPECTED LRECL =',I3,' GOT ',I4)
 1020 FORMAT ('VISRD: OPEN ERROR ',I3,' ON ',A4)
 1050 FORMAT ('VISRD: INIT FOR ',A4,' ERROR ',I3)
 1100 FORMAT ('VISRD: ',A4,' ERROR ',I3)
 1220 FORMAT ('VISRD: NO VALID DATA FOUND')
 1230 FORMAT ('**** Actual ',A,'max',1PE11.4,' exceeds limit',1PE11.4,
     *   8X,'****')
 1231 FORMAT ('**** Use cellsize <',F10.5,' not',F10.5,
     *   ' to get all data ****')
 1232 FORMAT ('**** using a ',A,' guard band of',F7.3,' of a radius',
     *   13X,'****')
 1235 FORMAT ('Using ONLY',I9,' of',I9,' Vis. < ',1PE9.2,' Lambdas')
 1236 FORMAT ('Using all ',I9,' Visibilities')
      END
      SUBROUTINE UVDISP
C-----------------------------------------------------------------------
C   UVDISP displays the sampled cells on the TV - filling in a maxint
C   for sampled cells and zero for unsampled.  Uses channel 1.
C-----------------------------------------------------------------------
      INTEGER   IROW(1024), WIN(4), BLKOF, ICH,
     *   IX, IY, L, M, L2, M2, II, I, IJ, IND, NNY2,
     *   J, LL2, ITS, IER, IX0, IY0, JX, JY, IANGL, ISCR3
      LOGICAL   T
      CHARACTER PHNAME*48
      REAL      XX
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (WROW, IROW)
      EQUIVALENCE (ISCR3, ISCR(2))
      DATA T /.TRUE./
      DATA BLKOF/1/
C-----------------------------------------------------------------------
C                                       Open TV, clear screen, catlg
      ICH = 1
      CALL TVOPEN (IROW, IER)
      IF (IER.NE.0) GO TO 999
      CALL TVSET (ICH, IROW, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1000) IER
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                       Open Grid file
      CALL ZPHFIL ('SC', SCRVOL(ISCR3), SCRCNO(ISCR3), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(ISCR3), INDS(ISCR3), SCRVOL(ISCR3), PHNAME, T,
     *   T, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (6)
         GO TO 990
         END IF
C                                       Init IO
C                                       leave in transposed order
      IF ((TVYMOD.NE.1) .AND. (TVYMOD.NE.2)) THEN
         IX = (NX - 1) / MAXXTV(2) + 1
         IY = (NY - 1) / MAXXTV(1) + 1
         IX = MAX (IX, IY)
         IY = IX
         L = (NX-1) / IX + 1
         L2 = L * IX
         L2 = MIN (L2, NX)
         M = (NY-1) / IY + 1
         M2 = M * IY
         M2 = MIN (M2, NY)
         IX0 = (MAXXTV(2) - L) / 2
         IF (IX0.LT.0) IX0 = 0
         IY0 = (MAXXTV(1) - M) / 2
         IF (IY0.LT.0) IY0 = 0
         IANGL = 0
      ELSE
C                                       transpose by write TV cols
         IX = (NX - 1) / MAXXTV(1) + 1
         IY = (NY - 1) / MAXXTV(2) + 1
         IX = MAX (IX, IY)
         IY = IX
         L = (NX-1) / IX + 1
         L2 = L * IX
         L2 = MIN (L2, NX)
         M = (NY-1) / IY + 1
         M2 = M * IY
         M2 = MIN (M2, NY)
         IX0 = (MAXXTV(1) - L) / 2
         IF (IX0.LT.0) IX0 = 0
         IY0 = (MAXXTV(2) - M) / 2
         IF (IY0.LT.0) IY0 = 0
         IANGL = 1
         IF (TVYMOD.EQ.2) IANGL = 3
         END IF
      JX = IY0 + 1
      JY = JX
      IF (TVYMOD.EQ.2) JY = MAXXTV(2) - IY0
C                                       Set window for first half and
C                                       the size of rows in the grid
C                                       image; note that each pixel in
C                                       the grid image is a COMPLEX
C                                       number while MINIT/MDISK deal
C                                       with REALS
      WIN(1) = 1
      WIN(2) = L2 / 2 + 1
      WIN(3) = M2 * 2
      WIN(4) = L2
      NNY2 = NY * 2
C                                       Write on TV center at center.
      L = 1
      DO 60 II = 1,2
         CALL MINIT ('READ', LUNS(ISCR3), INDS(ISCR3), NNY2, NX, WIN,
     *      CBUF, BUFSZ, BLKOF, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1015) IER
            GO TO 970
            END IF
C                                       Reset WIN for first half
         WIN(2) = 1
         WIN(4) = L2 / 2
C                                       Load to TV
         LL2 = L2 / 2
         DO 50 I = 1,LL2
            CALL MDISK ('READ', LUNS(ISCR3), INDS(ISCR3),
     *         CBUF, IND, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1025) IER
               GO TO 970
               END IF
C                                      IND points to start of row in a
C                                      REAL buffer - convert for COMPLEX
C                                      buffer.
            IND = IND / 2 + 1
            ITS = I-1
            IF (MOD (ITS,IX).EQ.0) THEN
               IF (IANGL.EQ.0) JY = IX0 + L
               IF (TVYMOD.EQ.2) JX = IX0 + L
               IF (TVYMOD.EQ.1) JX = MAXXTV(1) + 1 - IX0 - L
               L = L + 1
               IJ = M2 / (2 * IY)
               DO 40 J = 1,M2,IY
                  XX = ABS (REAL (CBUF(IND-1+J)))
     *               + ABS (AIMAG (CBUF(IND-1+J)))
                  IJ = IJ + 1
                  IF (IJ.GT.M2/IY) IJ = 1
                  IROW(IJ) = 0
                  IF (XX.GT.1.0E-15) IROW(IJ) = MAXINT
 40               CONTINUE
               CALL YIMGIO ('WRIT', ICH, JX, JY, IANGL, M, IROW, IER)
               IF (IER.NE.0) THEN
                  WRITE (MSGTXT,1040) IER
                  GO TO 970
                  END IF
               END IF
 50         CONTINUE
 60      CONTINUE
      GO TO 980
C                                       returns
 970  CALL MSGWRT (6)
 980  CALL ZCLOSE (LUNS(ISCR3), INDS(ISCR3), IER)
 990  CALL TVCLOS (IROW, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDISP: CANT CLEAR TV, IER=',I7)
 1010 FORMAT ('UVDISP: TROUBLE OPENING GRID FILE, IER=',I7)
 1015 FORMAT ('UVDISP: TROUBLE IN MINIT, IER=',I7)
 1025 FORMAT ('UVDISP: READ ERROR WITH MDISK, IER=',I7)
 1040 FORMAT ('UVDISP: TV IO ERROR=',I7)
      END
      SUBROUTINE MAPOUT (MAPNT, FFRAC, BMAX, IER)
C-----------------------------------------------------------------------
C   MAPOUT reads the FFT output file, corrects the map for the Fourier
C   transform of the convolving function used to grid, and writes out
C   the maps to a scratch file. PNLPUT is then called to copy the map
C   to the catalogd file and MAPHIS is called to write the history
C   file.
C   Inputs:
C     MAPNT     I    Pointer in /CFILES/ to first output map.
C                    Note: UVDATA should be first in /CFILES/
C     FFRAC     R    Freq. correction factor (-1) for this cycle.
C   Input/Output:
C     BMAX      R    Map normalization factor. Determined from the
C                    center pixel in BEAM map
C   Output:
C     IER       I    Error code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   BLKOF, LUNH(2),IP1, IP2, ICH, KVOL(2), KCNO(2), I, IND,
     *   JWIN(4), MAPNT, KBUFS1, KBUFS2, IBUFF(2048), K, ITEMP, ISCR1,
     *   IER
      INTEGER   INDEX, IMAP, ONENX, TWONX, KTEMP1, KTEMP2,
     *   IXC, KMAP, IMX1, IMX2, JMX1, JMX2, IWORK, KAP,
     *   IYC, IMN1, IMN2, JMN1, JMN2, ONENY, IMAPP1, JMAP
      LOGICAL   T, TWOMAP
      CHARACTER PHNAME*48
      REAL      XMAX(2), XMIN(2), PKR(2), BUFF(4096), RBUFF(4096),
     *   BMAX, BIMAX, FFRAC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (BUFF, CBUF),         (RBUFF, CBUF2, IBUFF)
      EQUIVALENCE (ISCR1, ISCR(1))
      DATA T /.TRUE./
      DATA BLKOF /1/
      DATA LUNH /27,28/
C-----------------------------------------------------------------------
      TWOMAP = MAPNT .LE. NUMMAP
      KBUFS1 = 2  * UVBFSS
      KBUFS2 = 2  * UVBFSS
C                                        Initialize history.
      CALL HIINIT (2)
C                                       AP Addresses
      ONENX = NX
      TWONX = NX * 2
      ONENY = NY
      KTEMP1 = NY + 1
      IMAP = 0
      IMAPP1 = IMAP + 1
      KMAP = IMAP + TWONX
      JMAP = KMAP + ONENX
      IXC = JMAP + ONENX
      IYC = IXC + ONENX
      IMX1 = IYC + ONENY
      IWORK = IMX1
      IMX2 = IMX1 + KTEMP1
      IMN1 = IMX2 + KTEMP1
      IMN2 = IMN1 + KTEMP1
      JMX1 = IMN2 + KTEMP1
      JMX2 = JMX1 + 1
      JMN1 = JMX2 + 1
      JMN2 = JMN1 + 1
C                                       Compute Fourier Transform of
C                                       the gridding function in the AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'MAPOUT: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IER = 8
         GO TO 995
         END IF
C                                       Ones fill convolving fn.
C                                       correction
      XMAX(1) = 1.0
      CALL QPUT (XMAX, IMAP, 1, 2)
      CALL QWD
      CALL QVFILL (IMAP, IXC, 1, ONENX)
      CALL QVFILL (IMAP, IYC, 1, ONENY)
C
      IF (NOGCOR.NE.1) THEN
         CALL GRDTAB (NX, ICNTRX, CTYPX, CPARMX, IXC, IWORK, IBUFF)
         CALL GRDTAB (NY, ICNTRY, CTYPY, CPARMY, IYC, IWORK, IBUFF)
         END IF
C                                       Open, init files.
C                                       FFT file
      CALL ZPHFIL ('SC', SCRVOL(ISCR1), SCRCNO(ISCR1), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(1), INDS(1), SCRVOL(ISCR1), PHNAME, T, T, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1010) IER
         GO TO 995
         END IF
C                                       Get center of beam(first pass).
      IF (MAPNT.EQ.2) THEN
         JWIN(2) = ICNTRY
         ITEMP = (ICNTRX * 2 - 1) / TWONX
         JWIN(2) = JWIN(2) + ITEMP
         JWIN(1) = ICNTRX * 2  - ITEMP * TWONX
         JWIN(3) = JWIN(1) + 1
         JWIN(4) = JWIN(2) + 1
         JWIN(3) = MIN (JWIN(3), TWONX)
         JWIN(4) = MIN (JWIN(4), NY)
         CALL MINIT ('READ', LUNS(1), INDS(1), TWONX, NY, JWIN,
     *      BUFF, KBUFS1, BLKOF, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1020) IER
            GO TO 995
            END IF
         CALL MDISK ('READ', LUNS(1), INDS(1), BUFF, IND, IER)
C                                       Get beam center value.
         BMAX = BUFF(IND)
C                                       Reset JWIN
         JWIN(1) = 0
         JWIN(2) = 0
         JWIN(3) = 0
         JWIN(4) = 0
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1070) IER
            GO TO 995
            END IF
         END IF
      CALL MINIT ('READ', LUNS(1), INDS(1), TWONX, NY, JWIN, BUFF,
     *   KBUFS1, BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 995
         END IF
C                                       First map file.
      CALL ZPHFIL ('MA', DISOUT, FCNO(MAPNT), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(2), INDS(2), DISOUT, PHNAME, T, T, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1010) IER
         GO TO 995
         END IF
      CALL MINIT ('WRIT', LUNS(2), INDS(2), NX, NY, JWIN, RBUFF, KBUFS2,
     *   BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 995
         END IF
C                                       Second map file.
      IF (TWOMAP) THEN
         CALL ZPHFIL ('MA', DISOUT, FCNO(MAPNT+1), 1, PHNAME, IER)
         CALL ZOPEN (LUNS(3), INDS(3), DISOUT, PHNAME, T, T, T, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1010) IER
            GO TO 995
            END IF
         CALL MINIT ('WRIT', LUNS(3), INDS(3), NX, NY, JWIN, WROW,
     *      KBUFS2, BLKOF, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1020) IER
            GO TO 995
            END IF
         END IF
C                                       Divide X gridding correction by
C                                       the peak of the beam to
C                                       normalize.
      IF (BMAX.LE.0.0) BMAX = 1.0
      BIMAX = 1.0 / BMAX
      CALL QPUT (BIMAX, IMAP, 1, 2)
      CALL QWD
      CALL QVSMUL (IXC, 1, IMAP, IXC, 1, ONENX)
C                                       Loop thru map normalizing,
C                                       gridding correcting and
C                                       writing the maps to
C                                       separate scratch files.
      DO 120 I = 1,NY
         INDEX = IMAP
         CALL MDISK ('READ', LUNS(1), INDS(1), BUFF, IND, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1070) IER
            GO TO 995
            END IF
         CALL QWR
         CALL QPUT (BUFF(IND), INDEX, TWONX, 2)
C                                       Correct map for gridding and
C                                       normalize.
C                                       Writes.
         CALL MDISK ('WRIT', LUNS(2), INDS(2), RBUFF, IP1, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1080) IER
            GO TO 995
            END IF
         IF (TWOMAP) THEN
           CALL MDISK ('WRIT', LUNS(3), INDS(3), WROW, IP2, IER)
           IF (IER.NE.0) THEN
              WRITE (MSGTXT,1080) IER
              GO TO 995
              END IF
           END IF
         KTEMP2 = I - 1
         KTEMP1 = IYC + KTEMP2
         CALL QWD
         CALL QCRVMU (IMAP, 2, IXC, 1, IMAP, 2, ONENX)
C                                       Split into two maps
         CALL QVSMUL (IMAP, 2, KTEMP1, KMAP, 1, ONENX)
         IF (TWOMAP) CALL QVSMUL (IMAPP1, 2, KTEMP1, JMAP, 1, ONENX)
C                                       Read maps to scratch files.
         CALL QWR
         CALL QGET (RBUFF(IP1), KMAP, ONENX, 2)
         CALL QWD
C                                       Get second map.
         IF (TWOMAP) THEN
            CALL QGET (WROW(IP2), JMAP, ONENX, 2)
            CALL QWD
            END IF
C                                       Find extrema
         IWORK = I - 1
         KTEMP1 = IMX1 + IWORK
         CALL QMAXV (KMAP, 1, KTEMP1, ONENX)
         KTEMP2 = IMX2 + IWORK
         IF (TWOMAP) CALL QMAXV (JMAP, 1, KTEMP2, ONENX)
         KTEMP1 = IMN1 + IWORK
         CALL QMINV (KMAP, 1, KTEMP1, ONENX)
         KTEMP2 = IMN2 + IWORK
         IF (TWOMAP) CALL QMINV (JMAP, 1, KTEMP2, ONENX)
 120     CONTINUE
C                                       Flush output buffers and close
C                                       files.
         CALL MDISK ('FINI', LUNS(2), INDS(2), RBUFF, IP1, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1080) IER
            GO TO 995
            END IF
         IF (TWOMAP) CALL MDISK ('FINI', LUNS(3), INDS(3), WROW, IP2,
     *      IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1080) IER
            GO TO 995
            END IF
         CALL ZCLOSE (LUNS(2), INDS(2), IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1140) IER
            GO TO 995
            END IF
            IF (TWOMAP) CALL ZCLOSE (LUNS(3), INDS(3), IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1140) IER
            GO TO 995
            END IF
C                                       Close grid file
      CALL ZCLOSE (LUNS(1), INDS(1), IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1160) IER
         GO TO 995
         END IF
      CALL QMAXV (IMX1, 1, JMX1, ONENY)
      IF (TWOMAP) CALL QMAXV (IMX2, 1, JMX2, ONENY)
      CALL QMINV (IMN1, 1, JMN1, ONENY)
      IF (TWOMAP) CALL QMINV (IMN2, 1, JMN2, ONENY)
      CALL QWR
      CALL QGET (BUFF, JMX1, 4, 2)
      CALL QWD
C                                       Release AP
      CALL QRLSE
      XMAX(1) = BUFF(1)
      XMAX(2) = BUFF(2)
      XMIN(1) = BUFF(3)
      XMIN(2) = BUFF(4)
      IF (XMAX(1).EQ.XMIN(1)) XMAX(1) = XMIN(1) + 1.0
      IF (XMAX(2).EQ.XMIN(2)) XMAX(2) = XMIN(2) + 1.0
      DO 180 K = 1,2
         PKR(K) = XMAX(K)
         IF (ABS (XMIN(K)).GT.ABS (XMAX(K))) PKR(K) = XMIN(K)
 180     CONTINUE
      IF ((MAPNT.EQ.2) .OR. (MODE.GE.10)) WRITE (MSGTXT,2004) PKR(1)
      IF ((MAPNT.EQ.2) .AND. ((MODE.EQ.5) .OR. (MODE.EQ.7) .OR.
     *   (MODE.EQ.8))) WRITE (MSGTXT,2002) PKR(1)
      IF ((MAPNT.EQ.2) .AND. (MODE.EQ.6)) WRITE (MSGTXT,2003) PKR(1)
      IF ((MAPNT.EQ.4) .AND. ((MODE.EQ.7) .OR. (MODE.EQ.8)))
     *    WRITE (MSGTXT,2003) PKR(1)
      IF ((MAPNT.EQ.4) .AND. (MODE.LE.3)) WRITE (MSGTXT,2005) PKR(1)
      IF (((MODE.EQ.3) .AND. (MAPNT.EQ.6)) .OR. ((MODE.EQ.4) .AND.
     *   (MAPNT.EQ.4))) WRITE (MSGTXT,2007) PKR(1)
      CALL MSGWRT (3)
      IF (TWOMAP) THEN
         WRITE (MSGTXT,2004) PKR(2)
         IF ((MAPNT.EQ.2) .OR. ((MAPNT.EQ.4) .AND. (MODE.EQ.8)))
     *      WRITE (MSGTXT,2008) BMAX
         IF ((MODE.LE.3) .AND. (MAPNT.EQ.4)) WRITE (MSGTXT,2006)
     *      PKR(2)
         CALL MSGWRT (3)
         END IF
C                                       First map.
C                                       Get CATBLK
      CALL CATIO ('READ', DISOUT, FCNO(MAPNT), CATBLK, 'REST', RBUFF,
     *   IER)
      WRITE (MSGTXT,1200) IER
      IF ((IER.GE.1).AND.(IER.LE.3)) GO TO 995
C                                       Set max. - min.
      CATR(KRDMX) = XMAX(1)
      CATR(KRDMN) = XMIN(1)
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISOUT, FCNO(MAPNT), CATBLK, 'REST', RBUFF,
     *   IER)
      WRITE (MSGTXT,1200) IER
      IF ((IER.GE.1).AND.(IER.LE.3)) GO TO 995
C                                       First map history.
      KCNO(1) = INCNO
      KCNO(2) = FCNO(MAPNT)
      KVOL(1) = DISIN
      KVOL(2) = DISOUT
C                                       Set spectral channel.
      ICH = NCH
      IF (MODE.GT.10) ICH = ICH + MAPNT - 3
      CALL MAPHIS (ICH, KCNO, KVOL, LUNH, FFRAC, IBUFF, PKR(1), BMAX)
C                                       Remove mark-for-destroy-on-fail
      FRW(MAPNT) = 0
C                                       Second map.
C                                       Get CATBLK
      IF (TWOMAP) THEN
         CALL CATIO ('READ', DISOUT, FCNO(MAPNT+1), CATBLK, 'REST',
     *      BUFF, IER)
         WRITE (MSGTXT,1200) IER
         IF ((IER.GE.1).AND.(IER.LE.3)) GO TO 995
C                                       Set max. - min.
         CATR(KRDMX) = XMAX(2)
         CATR(KRDMN) = XMIN(2)
C                                       Update CATBLK
         CALL CATIO ('UPDT', DISOUT, FCNO(MAPNT+1), CATBLK, 'REST',
     *      BUFF, IER)
         WRITE (MSGTXT,1200) IER
         IF ((IER.GE.1).AND.(IER.LE.3)) GO TO 995
C                                       Second map history.
         KCNO(2) = FCNO(MAPNT+1)
         IF ((MODE.GT.10) .AND. (MAPNT.GT.2)) ICH = ICH + 1
         CALL MAPHIS (ICH, KCNO, KVOL, LUNH, FFRAC, IBUFF, PKR(2), BMAX)
C                                       Remove destroy-on-failure flag
         FRW(MAPNT+1) = 0
         END IF
      GO TO 999
C                                       Error.
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MAPOUT: TROUBLE OPENING GRID FILE, IER=',I3)
 1020 FORMAT ('MAPOUT: TROUBLE IN MINIT, IER=',I3)
 1070 FORMAT ('MAPOUT: READ ERROR WITH MDISK, IER=',I3)
 1080 FORMAT ('MAPOUT: WRITE ERROR WITH MDISK, IER=',I3)
 1140 FORMAT ('MAPOUT: TROUBLE CLOSING MAP FILE, IER=',I3)
 1160 FORMAT ('MAPOUT: TROUBLE CLOSING GRID FILE, IER=',I3)
 1200 FORMAT ('MAPOUT: CATIO ERROR ',I3)
 2002 FORMAT ('Peak R Flux = ',1PE10.3,' Jy.')
 2003 FORMAT ('Peak L Flux = ',1PE10.3,' Jy.')
 2004 FORMAT ('Peak I Flux = ',1PE10.3,' Jy.')
 2005 FORMAT ('Peak Q Flux = ',1PE10.3,' Jy.')
 2006 FORMAT ('Peak U Flux = ',1PE10.3,' Jy.')
 2007 FORMAT ('Peak V Flux = ',1PE10.3,' Jy.')
 2008 FORMAT ('Sum of weights =',1PE12.5)
      END
      SUBROUTINE MAPHIS (ICH, ICNO, VOL, LUN, FFRAC, BUFFER, PEAKFX,
     *   SUMWT)
C-----------------------------------------------------------------------
C   MAPHIS creates a history file for the map and enters the values
C   of the parameters controling the map and the name of the output
C   file.
C    Input:
C      ICH              I    Frequency channel number
C      ICNO(2)          I    Catalog slot numbers for maps.
C                            1 = Input(UV), 2 = output(MA).
C      VOL(2)           I    Volumn numbers for maps.
C      LUN(2)           I    LUN for history file I/O.
C      FFRAC            R    Frequency correction factor - 1
C      BUFFER(512)      I    Work buffer.
C      PEAKFX           R    Peak flux in map.
C      SUMWT            R    Sum of weights in the map
C   Input via Commons:
C      CATBLK(256)      I    Catalog header block for map
C      NX,NY            I    Map dimension (cells) RA and Dec.
C      XFLD,YFLD        R    Field of view (arcsec.) RA and Dec.
C      XSHIFT           R    Eastward shift of map center.(arcsec)
C      YSHIFT           R    Northward shift of map center.(arcsec)
C      TAPERU,TAPERV    R    Tapers in U and V
C      BLMIN,BLMAX      R    Max. and Min. UV values.
C      NATWT            I    = 1 for natural weighting, otherwise unif.
C      UNFBOX           I    Halfwidth of Unif. wt. smoothing box.
C      IZSP             I    = 1 if zero spacing flux density specified.
C      WTZSP            R    weight for zero spacing flux density.
C      ZSP(4)           R    Zero spacing flux densities(I,Q,U,V).
C      CTYPX,CTYPY      I    Convolving fn. type in RA and Dec.
C      CPARMX,CPARMY(10)R    Convolving fn. parameters.
C      NOGCOR           I    = 1 if no gridding correction to be made.
C      STOKES           C*4  Polarization maps requested,
C      FREQ             D    Frequency of observation (Hz).
C      NVIS             I    Number of visibility obs. used.
C      NCH              I    Number of first freq. chan. in this run
C      NUMCH            I    Number of frequency chan. in this run.
C      ICNTRY           I    Center pixel in Dec.
C    Output: One history file.
C-----------------------------------------------------------------------
      CHARACTER  XNAME*12, XCLASS*6, ATIME*8, ADATE*12, CHCONV(6)*8,
     *   HILINE*72, MTYPE*2
      INTEGER   ICNO(2), VOL(2), LUN(2), BUFFER(512), ICH, IERR, J
      INTEGER   TIME(3), DATE(3)
      LOGICAL   T
      REAL      FFRAC, PEAKFX, SUMWT, X, Y
      DOUBLE PRECISION DFRAC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T /.TRUE./
      DATA CHCONV /'Pill box', 'Expontl ', 'Sinx / x',
     *             'Exp*Sinc', 'Spheroid', 'Bess*Exp'/
C-----------------------------------------------------------------------
C                                       copy keywords
      CALL KEYCOP (VOL(1), ICNO(1), VOL(2), ICNO(2), IERR)
C                                       Copy/open map file.
      CALL HISCOP (LUN(1), LUN(2), VOL(1), VOL(2), ICNO(1), ICNO(2),
     *   CATBLK, BUFFER(257), BUFFER, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Get current time/date
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,2000) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Write mapname
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), XNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), XCLASS)
      CALL HENCOO (TSKNAM, XNAME, XCLASS, CATBLK(KIIMS),
     *   VOL(2), LUN(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Source name.
      WRITE (HILINE,2001) TSKNAM, SOURCE
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Spectral channel
      WRITE (HILINE,2002) TSKNAM, ICH
      CALL HIADD (LUN(2),HILINE,BUFFER,IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Start and # of channels.
      WRITE (HILINE,2022) TSKNAM, NCH, NUMCH
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Map size.
      WRITE (HILINE,2005) TSKNAM, NX, NY
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Field of view
      WRITE (HILINE,2006) TSKNAM, XFLD, YFLD
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      X = XFLD / NX
      Y = YFLD / NY
      WRITE (HILINE,2106) TSKNAM, X, Y
      CALL HIADD (LUN(2),  HILINE,  BUFFER,  IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Frequency,polarization
      WRITE (HILINE,2007) TSKNAM, CATD(KDCRV+2), STOKES
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Translation
      WRITE (HILINE,2008) TSKNAM, XSHIFT, YSHIFT
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Taper
      WRITE (HILINE,2009) TSKNAM, TAPERU, TAPERV
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Annulus
      WRITE (HILINE,2010) TSKNAM, BLMIN, BLMAX
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Weighting,
      IF (NATWT.EQ.1) WRITE (HILINE,2011) TSKNAM
      IF (NATWT.NE.1) WRITE (HILINE,2012) TSKNAM, UNFBOX
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Zero spacing flux density
      IF (IZSP.EQ.1) THEN
         WRITE (HILINE,2013) TSKNAM, WTZSP
         CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,2014) TSKNAM, ZSP
         CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Convolving function.
      WRITE (HILINE,2120) TSKNAM, CHCONV(CTYPX), CHCONV(CTYPY)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2015) TSKNAM, CTYPX, (CPARMX(J), J = 1,3)
      CALL HIADD (LUN(2), HILINE,  BUFFER,  IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2016) TSKNAM, (CPARMX(J), J = 4,7)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2016) TSKNAM, (CPARMX(J), J = 8,10)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2017) TSKNAM, CTYPY, (CPARMY(J), J = 1,3)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2016) TSKNAM, (CPARMY(J), J = 4,7)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,2016) TSKNAM, (CPARMY(J), J = 8,10)
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Gridding corrected?
      IF (NOGCOR.EQ.1) WRITE (HILINE,2018) TSKNAM
      IF (NOGCOR.NE.1) WRITE (HILINE,2019) TSKNAM
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Guard bands
      WRITE (HILINE,2030) TSKNAM, XGUARD
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Number of visibility pts. used.
      WRITE (HILINE,2020) TSKNAM, NVIS
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Peak flux density
      WRITE (HILINE,2023) TSKNAM, PEAKFX
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Frequency correction factor.
      DFRAC = 1.0D0 + FFRAC
      WRITE (HILINE,2021) TSKNAM, DFRAC
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sum of griding weights
      WRITE (HILINE,2025) TSKNAM, SUMWT
      CALL HIADD (LUN(2), HILINE, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 200
C                                       An error occurred.
 100     WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
C                                       Close file.
 200  CALL HICLOS (LUN(2), T, BUFFER, IERR)
C                                       Clear write status.
      MTYPE = 'MA'
      CALL CATDIR ('CSTA', VOL(2), ICNO(2), XNAME, XCLASS,
     *   CATBLK(KIIMS), MTYPE, NLUSER, 'CLWR', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAPHIS: ERROR',I3,' CREATING HISTORY FILE')
 1100 FORMAT ('MAPHIS: ERROR',I3,' WRITING HISTORY FILE')
 2000 FORMAT (A6,'/ Map created by user ',I5,'  at ',A12,1X,A8)
 2001 FORMAT (A6,' SOURCE=''',A8,'''')
 2002 FORMAT (A6,' CHANNEL =',I4,' /Frequency channel')
 2005 FORMAT (A6,' IMSIZE=',I5,', ',I5,' /Image size')
 2006 FORMAT (A6,' MAPFLD=',E12.5,', ',E12.5,' /Field of view')
 2106 FORMAT (A6,' CELLSIZE=',E12.5,', ',E12.5,
     *   ' / Cell size arc sec.')
 2007 FORMAT (A6,' FREQ=',E14.7,' STOKES=''',A4,'''')
 2008 FORMAT (A6,' SHIFT=',F10.4,', ',F10.4)
 2009 FORMAT (A6,' UVTAPER=',E12.5,', ',E12.5,
     *   ' /UV taper kilolambda')
 2010 FORMAT (A6,' UVRANGE=',E12.5,', ',E12.5,
     *   ' /UV annulus kilolambda')
 2011 FORMAT (A6,' UVWTFN=''NA'' /Natural weighting')
 2012 FORMAT (A6,' UVWTFN=''  '' , UVBOX =',I5,' /Uniform weight')
 2013 FORMAT (A6,' WTZSP=',F10.2,' /Weight for 0 spacing flux')
 2014 FORMAT (A6,' ZEROSP=',F10.3,', ',F10.3,', ',F10.3,', ',F10.3)
 2015 FORMAT (A6,' XTYPE =',I3,' XPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2016 FORMAT (A6,' , ',E12.5,', ',E12.5,', ',E12.5,', ',E12.5)
 2017 FORMAT (A6,' YTYPE =',I3,' YPARM =',E12.5,', ',E12.5,', ',
     *   E12.5)
 2018 FORMAT (A6,' DOGRIDCR = -1 /No gridding correction')
 2019 FORMAT (A6,' DOGRIDCR = 1  /Gridding corrected')
 2020 FORMAT (A6,' /Number of vis. points used = ',I9)
 2021 FORMAT (A6,' /Frequency factor = ',1PD20.12)
 2022 FORMAT (A6,' CHANNEL1=',I4,' NOCH=',I4,
     *   ' /First and no. chan.')
 2023 FORMAT (A6,' /Peak in map =',1PE12.5,' Jy')
 2025 FORMAT (A6,' / Sum of gridding weights =',1PE12.5)
 2030 FORMAT (A6,' GUARD=',F7.4,',',F7.4,
     *   ' /U,V guard bands in UV-plane half widths')
 2120 FORMAT (A6,' /Convolution functions - U: ',A8,3X,
     *   'V: ',A8)
      END
      SUBROUTINE CONGRD (NTYPE, ILOC, FFRAC, IER)
C-----------------------------------------------------------------------
C   CONGRD convolves uv data onto a grid using AP routines.
C   The visibilities are convolved onto the grid using the convolving
C   function specified by CTYPX,CTYPY,CPARMX,CPARMY.
C   The gridded data is phase rotated so that the map center comes out
C   at location ICNTRX,ICNTRY (set in a DATA statment).  If requested
C   a uv taper is applied to the visibility weights before gridding.
C   If NOSHFT is .FALSE. a three dimension phase reference position
C   shift is done in GRIDAP.
C   Zero spacing flux densities are gridded if provided.
C   For NTYPE = 1 the grid file, when transformed, will give the IPOL
C   and IBEM maps, NTYPE = 2 produces QPOL and UPOL maps , NTYPE = 3
C   results in a single map (VPOL or 1 line channel) and NTYPE=4
C   gives two line maps.
C   Output grid file in scratch file number ISCR(3).
C   Inputs:
C      NTYPE        I    Polarization type, 1 = IPOL,IBEM,
C                            2 = QPOL,UPOL, 3 = VPOL, 4=two line maps.
C      ILOC         I    Offset of first vis. record.
C      FFRAC        R    Factor - 1 to multiply by u,v,w
C   From commons:
C      NVIS         I    Number of visibility records
C      NX,NY        I    Dimension of map in RA, Dec (cells)
C      CTYPX,CTYPY  I    Convolving function types for RA and Dec
C      CPARMX(10)   R    Convolving function parameters for RA
C                        CPARMX(1) = support half width.
C      CPARMY(10)   R    Convolving function parameters for Dec.
C      IZSP         I    = 1 if zero spacing flux densities given.
C      WTZSP        R    Weight for zero spacing flux densities.
C      ZSP(4)       R    Zero spacing fluxes, I, Q, U, V (Jy)
C      ITAP         I    = 1 if taper requested
C      TAPERU,TAPERV R   TAPER ( to 30%) in u and v (kilolamda)
C      XFLD,YFLD    R    Field of view in RA and Dec (arcseconds)
C      NOSHFT       L    If .TRUE. apply no shift to position.
C      DXC,DYC,DZC  R    -2*pi*(delta ra, delta dec, and delta z)
C                        to be used in GRIDAP to shift positions.
C                        (u,v and w are in cells).
C      SCLU,SCLV,SCLW C*2 Conversion factors for u,v and w from
C                        wavelengths at the reference frequency
C                        to cells.
C      SCRVOL(10)   I    Volumn numbers of scratch files.
C                        1 = visibility file, 2 = grid file.
C      SCRCNO(10)   I    Catalog numbers of the scratch files.
C      APSIZ        R    Memory size of the AP in words.
C   Output:
C      IER          I    Return error code. 0=>OK, error otherwise.
C-----------------------------------------------------------------------
      INTEGER   JBUFSZ, BLKOF, NNY2, JWIN(4), NTYPE,
     *   BIND1, BIND2, ILENBU, I, IBIND, IFACT, II, INCNT, INPTR, IU,
     *   JNPTR, LIM, NIO, NPOINT, NX2, ISCR2, IER, ILOC
      INTEGER   VO, BO, KAP, U, GRID, ROW, NUM, MO2, CY, CX, VIS, WT,
     *   END1, END2, NEXTRW, VVIS, NMOV, IDATA, ITEMP, UV, CINC,
     *   LLREC, NO2, M, LROW, TOLROW, TYP, TYPE, I4TMP, MAXREC, CNT
      LOGICAL   T, F, ENDROW
      CHARACTER PHNAME*48
      REAL      XTEMP(16), BUFF(1), UMIN, UMAX,
     *   RBUFF1(4096), RBUFF2(4096), TUC, TVC, XMAX, SSCLU,
     *   UU1, UU2, UU3, FFRAC
      DOUBLE PRECISION WX, WY, TWOPI, GFACT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (WROW, BUFF),  (RBUFF1, CBUF),  (RBUFF2, CBUF2)
      EQUIVALENCE (ISCR2, ISCR(2))
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO, BLKOF, UV /0, 1, 1, 16/
C-----------------------------------------------------------------------
      IER = 0
      CINC = 100
      LLREC = LRECIN
      SSCLU = FFRAC * SCLU + SCLU
      GFACT = 1.0D0 / ((1.0D0 + FFRAC) * SCLU)
C                                       Set I/O length.
      ILENBU = 0
C                                       To simplify the map output
C                                       routine, shift the map center
C                                       here in the gridding routine.
C                                       ICNTRX and ICNTRY define the
C                                       pixel number of the map center,
C                                       counted from the front of the
C                                       file.
      TWOPI = 8D0 * ATAN(1D0)
      WX = (-TWOPI * (ICNTRX-1)) / NX
      WY = (-TWOPI * (ICNTRY-1)) / NY
C                                       Convert taper widths to
C                                       convenient constants.
      TUC = 0.0
      TVC = 0.0
      IF (ITAP.NE.0) THEN
         IF (TAPERU.GT.0.) TUC = LOG(.3) / (TAPERU * 1E3 /
     *      206265. * XFLD) ** 2
         IF (TAPERV.GT.0.) TVC = LOG(.3) / (TAPERV * 1E3 /
     *      206265. * YFLD) ** 2
         END IF
C                                       Open files. Use UVINIT and
C                                       UVDISK to read the
C                                       visibility file,
C                                       MINIT and MDISK to write the
C                                       grid file.
      CALL ZPHFIL ('SC', SCRVOL(UVSCR), SCRCNO(UVSCR), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(1), INDS(1), SCRVOL(UVSCR), PHNAME, T, T, T, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1010) IER
         GO TO 995
         END IF
C                                       GRID file must be opened twice,
C                                       once for each half. Conjugate
C                                       points are written in the two
C                                       halves.
      CALL ZPHFIL ('SC', SCRVOL(ISCR2), SCRCNO(ISCR2), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(2), INDS(2), SCRVOL(ISCR2), PHNAME, T, F, T, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 995
         END IF
      CALL ZOPEN (LUNS(3), INDS(3), SCRVOL(ISCR2), PHNAME, T, F, T, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 995
         END IF
      JBUFSZ = UVBFSS * 2
C                                       Init UV file.
      CALL UVINIT ('READ', LUNS(1), INDS(1), NVIS, VO, LRECIN, ILENBU,
     *   JBUFSZ, BUFF, BO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         GO TO 995
         END IF
C                                       Init GRID file (both halves).
      JBUFSZ = 4 * UVBFSS
      NNY2 = NY * 2
      JWIN(1) = 1
      JWIN(2) = NX/2
      JWIN(3) = NNY2
      JWIN(4) = 1
      CALL MINIT ('WRIT', LUNS(2), INDS(2), NNY2, NX, JWIN, RBUFF1,
     *   JBUFSZ, BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         GO TO 995
         END IF
      JWIN(2) = NX / 2 + 1
      JWIN(4) = NX
      CALL MINIT ('WRIT', LUNS(3), INDS(3), NNY2, NX, JWIN, RBUFF2,
     *   JBUFSZ, BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         GO TO 995
         END IF
C                                       Dummy 1st write (to make small
C                                       maps work).
      CALL MDISK ('WRIT', LUNS(3), INDS(3), RBUFF2, BIND2, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1160) IER
         GO TO 995
         END IF
      NX2 = NX / 2
C                                       Make sure an ODD number of rows
C                                       is being kept in the AP.
      NO2 = MAX (CPARMY(1), 1.0) + 0.1
      M = MAX (CPARMX(1), 1.0) + 0.1
      M = M * 2 + 1
      MO2 = M / 2
C                                       Setup for AP griding
C                                       Set AP pointers
      IFACT = 4
      GRID = APSIZ + 0.5
      I4TMP = M * IFACT
      GRID = GRID - I4TMP * NY - 1
      I4TMP = IFACT * (M - 1)
      ROW = GRID + I4TMP * NY
      NEXTRW = NY
      NEXTRW = ROW + 2 * NEXTRW
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'CONGRD: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IER = 8
         GO TO 995
         END IF
      NUM = NY
      NUM = IFACT * NUM * M
      CALL QVCLR (GRID, 1, NUM)
      CY = 2 * NO2 + 1
      CY = GRID - 100 * CY - 1
      CX = CY - 100 * M - 1
      VIS = UV + 4 + (ILOC - 1) * 2
      WT = UV + 3
C                                       Set other AP values.
      LROW = NY
      TOLROW = 2 * NY
      TYPE = NTYPE
C                                       TYP negative  = > apply taper.
      TYP = NTYPE
      IF (NTYPE.EQ.4) TYP = 2
      IF (ITAP.GT.0) TYP = -TYP
C                                       Set gridding convolution tables
      CALL CONVFN (CX, CTYPX, CPARMX, CBUF)
      CALL CONVFN (CY, CTYPY, CPARMY, CBUF)
C                                       Set constants in AP.
      XTEMP(1) = COS ((NX / 2 - 1) * WX)
      XTEMP(2) = SIN ((NX / 2 - 1) * WX)
      XTEMP(3) = COS (WY)
      XTEMP(4) = SIN (WY)
      XTEMP(5) = COS (-WX)
      XTEMP(6) = SIN (-WX)
      XTEMP(7) = 1.0
      XTEMP(8) = 0.0
      XTEMP(9) = TUC
      XTEMP(10) = TVC
      XTEMP(11) = FFRAC*SCLU + SCLU
      XTEMP(12) = FFRAC*SCLV + SCLV
      XTEMP(13) = FFRAC*SCLW + SCLW
      XTEMP(14) = DXC / SCLU
      XTEMP(15) = DYC / SCLV
      XTEMP(16) = DZC / SCLW
      CALL QPUT (XTEMP, 0, 16, 2)
C                                       Determine the maximum number
C                                       of visibility points which
C                                       fit in the AP.
      MAXREC = (CX - 16) / LRECIN - 5
C                                       Be sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         XMAX = - MAXREC * LRECIN
         WRITE (MSGTXT,1070) XMAX
         IER = 1
         GO TO 995
         END IF
      END1 = ROW - 1
      END2 = NY
      END2 = END1 + END2 * IFACT
      NMOV = IFACT * (M - 1)
      NMOV = NMOV * NY
C                                       Read first visibility record
      CALL UVDISK ('READ', LUNS(1), INDS(1), BUFF, NIO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1080) IER
         GO TO 995
         END IF
      INPTR = IBIND
      INCNT = 1
C                                       Loop through grid
      DO 200 I = 1,NX2
         IU = NX2 - I
         UMIN = (IU - 0.5) * GFACT
         UMAX = (IU + 0.5) * GFACT
         IDATA = UV
         CNT = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
  100    ENDROW = T
         NPOINT = 0
C                                       Check if all data read.
      IF (NIO.LE.0) GO TO 140
C                                       Make sure there is some data on
C                                       this row.
         IF (BUFF(INPTR).LT.UMIN) GO TO 140
C                                       Check if end of row occures in
C                                       this record.
         LIM = INCNT + MAXREC - CNT - 1
         LIM = MIN (LIM, NIO)
         JNPTR = INPTR
C                                       Check for missorted data.
         DO 110 II = INCNT,LIM
            IF (BUFF(JNPTR).LT.UMIN) GO TO 120
               IF (BUFF(JNPTR).GT.UMAX) THEN
                   UU1 = BUFF(JNPTR) * SSCLU
                   UU2 = UMIN * SSCLU
                   UU3 = UMAX * SSCLU
                   WRITE (MSGTXT,1100) UU1,UU2,UU3
                   CALL MSGWRT (1)
                   END IF
               NPOINT = NPOINT + 1
               JNPTR = JNPTR + LRECIN
 110           CONTINUE
C                                       Rest of record is on same row.
         ENDROW = F
 120     CONTINUE
         CNT = CNT + NPOINT
C                                       Load into AP.
         CALL QWR
         ITEMP = NPOINT * LRECIN
         CALL QPUT(BUFF(INPTR), IDATA, ITEMP, 2)
         IDATA = IDATA + ITEMP
         INCNT = INCNT + NPOINT
         INPTR = INPTR + NPOINT * LRECIN
C                                       Check if AP full or row finished
         IF ((ENDROW) .OR. (CNT.GE.MAXREC)) GO TO 140
C                                       Read next record.
  125    INCNT = 1
C                                       Check if all records read.
         IF (NIO.LE.0) THEN
            ENDROW = T
C                                       Read again and Loop back
         ELSE
            CALL UVDISK ('READ', LUNS(1), INDS(1), BUFF, NIO, IBIND,
     *         IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1130) IER
               GO TO 995
               END IF
            INPTR = IBIND
            GO TO 100
            END IF
C                                       Grid data
 140     IF (CNT.GT.0)  THEN
            CALL QWAIT
            ITEMP = - CNT
            IF (.NOT.NOSHFT) ITEMP = - ITEMP
            CALL QGRIDA (UV ,VIS, WT, LLREC, GRID, CY, CX, CINC, NO2,
     *         M,LROW, ITEMP, TYP)
            CALL QWR
            IDATA = UV
            CNT = 0
C                                       Check if row finished.
            IF (INCNT.LT.NIO) GO TO 100
            IF (.NOT.ENDROW) GO TO 125
            END IF
C                                       Row finished, process.
         U = IU + M / 2
C                                       Don't write rows before the
C                                       start of the grid file.
         IF (U.LT.NX2) THEN
            CALL QWAIT
            CALL QFINGR (U, ROW, MO2, LROW, TYPE)
C                                       Writes
            CALL MDISK ('WRIT', LUNS(2), INDS(2), RBUFF1, BIND1, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1160) IER, I
               GO TO 995
               END IF
            CALL MDISK ('WRIT', LUNS(3), INDS(3), RBUFF2, BIND2, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1160) IER, I
               GO TO 995
               END IF
            CALL QWAIT
C                                       Read out gridded data.
            CALL QGET (RBUFF1(BIND1), ROW, TOLROW, 2)
            CALL QGET (RBUFF2(BIND2), NEXTRW, TOLROW, 2)
            CALL QWAIT
            END IF
C                                       Prepare AP for next row.
         IF (IU.NE.0) CALL QVMOV(END1, -1, END2, -1, NMOV)
         CALL QWAIT
         ITEMP = NY
         ITEMP = ITEMP * IFACT
         IF (IU.NE.0) CALL QVCLR (GRID, 1, ITEMP)
         IF (U.GE.NX2) GO TO 200
 200     CONTINUE
C                                       Do zero spacing flux densities
      IF (IZSP.EQ.1) THEN
         IF (NTYPE.EQ.1) VVIS = UV + 4
         IF (NTYPE.EQ.2) VVIS = UV + 6
         IF (NTYPE.EQ.3) VVIS = UV + 10
         IF (NTYPE.EQ.4) VVIS = UV + 4
         XTEMP(1) = 0.0
         XTEMP(2) = 0.0
         XTEMP(3) = 0.0
         XTEMP(4) = WTZSP
         XTEMP(5) = ZSP(1)
         XTEMP(6) = 0.0
         XTEMP(7) = ZSP(2)
         XTEMP(8) = 0.0
         XTEMP(9) = ZSP(3)
         XTEMP(10) = 0
         XTEMP(11) = ZSP(4)
         XTEMP(12) = 0.0
         CALL QWAIT
         CALL QPUT (XTEMP, UV, 12, 2)
         ITEMP = -1
         IF (.NOT.NOSHFT) ITEMP = 1
         CALL QWAIT
         CALL QGRIDA (UV, VVIS, WT, LLREC, GRID, CY, CX, CINC, NO2,
     *      M, LROW, ITEMP, TYP)
         CALL QWAIT
         END IF
C                                       Finish reading out grid
      LIM = M / 2
      DO 300 I = 1,LIM
         U = LIM - I
         ROW = IFACT * (M - I - 1)
         ROW = GRID + ROW * NY
         NEXTRW = NY
         NEXTRW = ROW + 2 * NEXTRW
C                                       Grid row.
         CALL QWAIT
         CALL QFINGR (U, ROW, MO2, LROW, TYPE)
C                                       Writes
         CALL MDISK ('WRIT', LUNS(2), INDS(2), RBUFF1, BIND1, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1160) IER, I
            GO TO 995
            END IF
C                                       Last conjugate row goes to grid
C                                       row NX/2+1.
         IF (U.EQ.0) THEN
C                                       Flush and re-init output #2.
            CALL MDISK ('FINI', LUNS(3), INDS(3), RBUFF2, BIND2, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1160) IER, I
               GO TO 995
               END IF
            JWIN(2) = NX / 2 + 1
            JWIN(4) = NX
            CALL MINIT ('WRIT', LUNS(3), INDS(3), NNY2, NX, JWIN,
     *         RBUFF2, JBUFSZ, BLKOF, IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1040) IER
               GO TO 995
               END IF
            END IF
         CALL MDISK ('WRIT', LUNS(3), INDS(3), RBUFF2, BIND2, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1160) IER, I
            GO TO 995
            END IF
C                                       Zero extra row if U = 0.
         CALL QWAIT
         IF (U.EQ.0) CALL QVCLR (NEXTRW, 1, TOLROW)
         CALL QWAIT
         CALL QGET (RBUFF1(BIND1), ROW, TOLROW, 2)
         CALL QGET (RBUFF2(BIND2), NEXTRW, TOLROW, 2)
         CALL QWAIT
 300     CONTINUE
      CALL QRLSE
C                                       Finish writes
      CALL MDISK ('FINI', LUNS(2), INDS(2), RBUFF1, BIND1, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1160) IER, I
         GO TO 995
         END IF
      CALL MDISK ('FINI', LUNS(3), INDS(3), RBUFF2, BIND2, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1160) IER, I
         GO TO 995
         END IF
C                                       Close files
      DO 350 I = 1,3
         CALL ZCLOSE(LUNS(I), INDS(I), IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1350) IER
            GO TO 995
            END IF
 350     CONTINUE
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CONGRD: TROUBLE OPENING UV DATA FILE, IER=',I3)
 1020 FORMAT ('CONGRD: TROUBLE OPENING GRID FILE, IER=',I3)
 1040 FORMAT ('CONGRD: TROUBLE IN MINIT, IER=',I3)
 1070 FORMAT ('CONGRD: ',F8.0,' TOO FEW AP WORDS AVAILABLE')
 1080 FORMAT ('CONGRD: READ ERROR IN VISIBILITY FILE, IER=',I3)
 1100 FORMAT ('IU=',F9.3,' NOT IN RANGE ',2F9.3,',DATA MISSORTED')
 1130 FORMAT ('CONGRD: ERROR',I3,' READING VIS. RECORD ')
 1160 FORMAT ('CONGRD: ERROR',I3,' WRITING GRID ROW ',I5)
 1350 FORMAT ('CONGRD: TROUBLE CLOSING FILE, IER=',I3)
      END
      SUBROUTINE UNIF (FFRAC, IER)
C-----------------------------------------------------------------------
C   UNIF computes uniform weighting corrections and applies them to
C   the weights in the visibility data base.  The visibility weights
C   are divided by the number of visibilities occuring in cells within
C   a box of half width UNFBOX centered on the cell in which a given
C   visibility resides.
C   Inputs:
C      FFRAC       R    u,v,w frequency correction factor -1.0
C      SCLU,SCLV   C*2  u and v scaling to cells at ref. frequency.
C      NX,NY       I    Dimensions ( cells) of the map in RA and Dec
C      LUNS(5)     I    Logical unit numbers of the scratch files
C                       1 = visibility
C                       2 = Grid, used to store counts.
C      SCRVOL(5)   I    Volumn numbers of the scratch files.
C      SCRCNO(5)   I    Catalog numbers of the scratch files.
C      UNFBOX      I    Half width of unif. wt. counting box size.
C      NVIS        I    Number of visibility measurments.
C   Also the visibility data.
C   Output:
C      IER         I    Return error code, 0=>OK, error otherwise.
C   Also the visibility data with weights uniform weighted.
C-----------------------------------------------------------------------
      INTEGER   IER, BLKOF, WIN(4), I, II,
     *   IBIND, JBIND, KBIND, OPTR, U, JBUFSZ, KBUFSZ,
     *   HALFNX, ILENBU, INCNT, IU, J, JNPTR, KROW, LIM, ISCR1, INIO,
     *   LIMIT, NIO, NIOUT, NPOINT, NX2, NY2, NY1, INDEX, INPTR
      INTEGER   BO, VO, AMAX, AMIN, GRID, ROW, NROW, ROW2, IV,
     *   END1, END2, END3, WMAX, NMOV, IROW, WMIN, NUM, IDATA,
     *   KTEMP, LLREC, JNY, ITEMP, APSCLV, KAP, MAXREC, CNT, UV
      LOGICAL   T, F, ENDROW
      CHARACTER PHNAME*48
      REAL      TEMP(10), XMAX, SSCLV, SSCLU,
     *   BUFF(1), OBUFF(1), BUFF2(4096), UMIN, FFRAC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVMAP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (CBUF,BUFF), (OBUFF,CBUF2), (BUFF2,WROW)
      EQUIVALENCE (ISCR1, ISCR(1))
      DATA T, F/.TRUE.,.FALSE./
      DATA WIN, UV /4*0, 10/
      DATA BO, VO /1, 0/
      DATA BLKOF /1/
C-----------------------------------------------------------------------
      NY1 = NY - 1
      JNY = NY
      HALFNX = NX / 2
      ILENBU = 0
      LLREC = LRECIN
C                                       Prepare for reads and writes.
C                                       Open visibility and grid files.
C                                       Open visibility file.
      CALL ZPHFIL ('SC', SCRVOL(UVSCR), SCRCNO(UVSCR), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(1), INDS(1), SCRVOL(UVSCR), PHNAME, T, F, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1020) IER
         GO TO 995
         END IF
C                                       Open grid file.
      CALL ZPHFIL ('SC', SCRVOL(ISCR1), SCRCNO(ISCR1), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(2), INDS(2), SCRVOL(ISCR1), PHNAME, T, T, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1030) IER
         GO TO 995
         END IF
C                                       Init files.
      KBUFSZ = UVBFSS * 4
      JBUFSZ = UVBFSS * 2
C                                       Init. vis file.
      CALL UVINIT ('READ', LUNS(1), INDS(1), NVIS, VO, LRECIN, ILENBU,
     *   KBUFSZ, BUFF, BO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         GO TO 995
         END IF
C                                       Init. grid file.
      CALL MINIT ('WRIT', LUNS(2), INDS(2), NY, HALFNX, WIN, BUFF2,
     *   JBUFSZ, BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1050) IER
         GO TO 995
         END IF
C                                       Load AP values
C                                       Maximum v.
      AMAX = 0
      TEMP(1) = NY / 2 - 1
C                                       Minimum v.
      AMIN = 1
      TEMP(2) = - NY / 2
C                                       Minimum count.
      WMIN = 2
      TEMP(3) = 1.0
C                                       Maximum count (to avoid I
C                                       problems)
      WMAX = 3
      TEMP(4) = 1.0E20
      IV = UV + 1
C                                       Scaling factors for u, v
C                                       to cells.
      APSCLV = 4
      TEMP(5) = SCLV + FFRAC * SCLV
      SSCLV = SCLV + FFRAC * SCLV
      SSCLU = 1.0 / (SCLU + FFRAC * SCLU)
C                                       Grab AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'UNIF: UNABLE TO GET AP MEMORY BACK'
         CALL MSGWRT (8)
         IER = 8
         GO TO 995
         END IF
      CALL QPUT (TEMP, 0, 10, 2)
      CALL QWD
C                                       Set pointers for AP
      NROW = 2 * UNFBOX + 1
      GRID = (APSIZ - 1) - NROW * NY
C                                       Set pointer for temporary row
      ROW = GRID - NY
C                                       Set pointers for output buffer
      ROW2 = ROW - NY
      END1 = ROW2 + NY - 1
C                                       Set pointer for shifting grid
      END2 = GRID + NROW * NY
      END3 = END2 - NY
C                                      Determine no. points to shift.
      NMOV = (NROW - 1) * NY
C                                       Clear AP
      NUM = NROW * NY
      CALL QVCLR (GRID, 1, NUM)
      CALL QWR
      CALL QWD
C                                       Determine max. no. of vis.
C                                       points which will fit in AP.
      MAXREC = (ROW2 - 10) / LRECIN - 5
C                                       Make sure MAXREC.GT.0
      IF (MAXREC.LE.0) THEN
         XMAX = - MAXREC * LRECIN
         WRITE (MSGTXT,1060) XMAX
         IER = 1
         GO TO 995
         END IF
C                                       Read first visibility record.
      INCNT = 1
      CALL UVDISK ('READ', LUNS(1), INDS(1), BUFF, NIO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1070) IER
         GO TO 995
         END IF
      INPTR = IBIND
      NX2 = NX / 2
      NY2 = NY / 2
C                                       Begin counting loop.
      DO 200 I = 1,NX2
         IU = NX2 - I
         UMIN = (IU - 0.5) * SSCLU
         IDATA = UV
         CNT = 0
C                                       Return to here if more than one
C                                       record is loaded at a time.
  100    ENDROW = T
         NPOINT = 0
C                                       Check if all data read.
      IF (NIO.LE.0) GO TO 140
C                                       Make sure there is some data on
C                                       this row.
         IF (BUFF(INPTR).LT.UMIN) GO TO 140
C                                       Check if end of row occures in
C                                       this record.
         LIM = INCNT + MAXREC - CNT - 1
         LIM = MIN (LIM, NIO)
         JNPTR = INPTR
         DO 110 II = INCNT,LIM
            IF (BUFF(JNPTR).LT.UMIN) GO TO 120
               NPOINT = NPOINT + 1
               JNPTR = JNPTR + LRECIN
  110          CONTINUE
C                                       Rest of record is on same row.
         ENDROW = F
  120    CONTINUE
         CNT = CNT + NPOINT
C                                       Load into AP.
         CALL QWR
         KTEMP = NPOINT * LRECIN
         CALL QPUT (BUFF(INPTR), IDATA, KTEMP, 2)
         IDATA = IDATA + KTEMP
         INPTR = INPTR + NPOINT * LRECIN
         INCNT = INCNT + NPOINT
C                                       Check if AP full or row finished
         IF ((ENDROW) .OR. (CNT.GE.MAXREC)) GO TO 140
C                                       Read next record.
  125    INCNT = 1
C                                       Check if all records read.
         IF (NIO.LE.0) ENDROW = T
         IF (NIO.LE.0) GO TO 140
            CALL UVDISK ('READ', LUNS(1), INDS(1), BUFF, NIO, IBIND,
     *         IER)
            IF (IER.NE.0) THEN
               WRITE (MSGTXT,1070) IER
               GO TO 995
               END IF
            INPTR = IBIND
C                                       Loop back
            GO TO 100
  140       CONTINUE
C                                       Grid data
         IF (CNT.LE.0) GO TO 145
         CALL QWD
C                                       Use HIST for counting.
C                                       Scale v to cells.
         CALL QVSMUL (IV,LLREC, APSCLV, IV, LLREC, CNT)
         CALL QHIST (IV, LLREC, GRID, CNT, JNY, AMAX, AMIN)
         IDATA = UV
         CNT = 0
C                                       Check if row finished.
         IF (ENDROW) GO TO 145
         IF (INCNT.LT.NIO) GO TO 100
         GO TO 125
C                                       Row finished, process.
  145    CONTINUE
         U = IU + NROW / 2
C                                       If U = 0 conjugate row.
         IF (U.EQ.0) THEN
            KTEMP = GRID + 1
            ITEMP = NY - 1
            CALL QVMOV (KTEMP, 1, END1, -1, ITEMP)
            CALL QVMOV (GRID, 1, ROW2, 1, 1)
            CALL QVADD (GRID, 1, ROW2, 1, GRID, 1, JNY)
            END IF
C                                       Don't write rows before the
C                                       start of the grid file.
         IF (U.GE.NX2) GO TO 180
C                                       Sum rows.
         CALL MDISK ('WRIT', LUNS(2), INDS(2), BUFF2, JBIND, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1160) IER, I
            GO TO 995
            END IF
         CALL QVMOV (GRID, 1, ROW, 1, JNY)
         IF (UNFBOX.GT.0) THEN
            LIMIT = NROW
            DO 160 J = 2,LIMIT
               IROW = GRID + JNY * (J-1)
               CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNY)
 160           CONTINUE
            END IF
C                                       Boxsum sum of rows.
         CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNY)
C                                       Make sure values reasonable.
         CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNY)
C                                       Read out row.
         CALL QWR
         CALL QGET (BUFF2(JBIND), ROW2, JNY, 2)
         CALL QWD
C                                       Check if last row.
         IF (I.EQ.NX2) GO TO 200
C                                       Prepare AP for next row.
  180       CALL QVMOV (END3, -1, END2, -1, NMOV)
            CALL QVCLR (GRID, 1, JNY)
  200    CONTINUE
      IF (UNFBOX.LE.0) GO TO 305
C                                       Finish reading out grid
      DO 300 I = 1,UNFBOX
         CALL MDISK ('WRIT', LUNS(2), INDS(2), BUFF2, JBIND, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1160) IER, I
            GO TO 995
            END IF
C                                       Add conjugate to next row.
         IROW = GRID + JNY * I
         KTEMP = IROW + 1
         ITEMP = NY - 1
         CALL QVMOV (KTEMP, 1, END1, -1, ITEMP)
         CALL QVMOV (IROW, 1, ROW2, 1, 1)
         CALL QVADD (IROW, 1, ROW2, 1, IROW, 1, JNY)
C                                       Sum rows.
         CALL QVMOV (GRID, 1, ROW, 1, JNY)
         LIMIT = NROW - I
         DO 210 J = 2,LIMIT
            IROW = GRID + JNY * (J-1)
            CALL QVADD (IROW, 1, ROW, 1, ROW, 1, JNY)
  210       CONTINUE
C                                       Boxsum sum of rows.
         CALL QBOXSU (ROW, 1, NROW, ROW2, 1, JNY)
C                                       Make sure values reasonable.
         CALL QVCLIP (ROW2, 1, WMIN, WMAX, ROW2, 1, JNY)
C                                       Read row back out.
         CALL QWR
         CALL QGET (BUFF2(JBIND), ROW2, JNY, 2)
         CALL QWD
  300    CONTINUE
  305 CONTINUE
C                                       Finish write
      CALL MDISK ('FINI', LUNS(2), INDS(2), BUFF2, JBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1160) IER, I
         GO TO 995
         END IF
      CALL QRLSE
C
C                                       Apply corrections.
C
C                                       Open visibility file for writing
      CALL ZPHFIL ('SC', SCRVOL(UVSCR), SCRCNO(UVSCR), 1, PHNAME, IER)
      CALL ZOPEN (LUNS(3), INDS(3), SCRVOL(UVSCR), PHNAME, T, F, T, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1320) IER
         GO TO 995
         END IF
C                                       Initialize vis. file for read.
      CALL UVINIT ('READ', LUNS(1), INDS(1), NVIS, VO, LRECIN, ILENBU,
     *   KBUFSZ, BUFF, BO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1040) IER
         GO TO 995
         END IF
C                                       Initialize vis. file for write.
      CALL UVINIT ('WRIT', LUNS(3), INDS(3), NVIS, VO, LRECIN, ILENBU,
     *   KBUFSZ, OBUFF, BO, KBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1340) IER
         GO TO 995
         END IF
      OPTR = KBIND
C                                       Initialize grid file for read.
      CALL MINIT ('READ', LUNS(2), INDS(2), NY, HALFNX, WIN, BUFF2,
     *   JBUFSZ, BLKOF, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1050) IER
         GO TO 995
         END IF
C                                       Read first row of grid.
      KROW = NX / 2 - 1
      CALL MDISK ('READ', LUNS(2), INDS(2), BUFF2, JBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1360) IER, KROW
         GO TO 995
         END IF
      UMIN = (KROW - 0.5) * SSCLU
C                                       Begin weighting loop.
  400 CONTINUE
C                                       Read vis record.
      CALL UVDISK ('READ', LUNS(1),INDS(1), BUFF, INIO, IBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1070) IER
         GO TO 995
         END IF
      INPTR = IBIND
C                                       Loop thru record.
      IF (INIO.LE.0) GO TO 510
      DO 500 I = 1,INIO
C                                       Copy vis.
         DO 420 J = 1,LRECIN
            OBUFF(OPTR+J-1) = BUFF(INPTR+J-1)
  420       CONTINUE
C                                       See if new grid row needed,
C                                       if so find next grid row.
  430    IF (OBUFF(OPTR).GE.UMIN) GO TO 450
C                                       Read next grid row.
         KROW = KROW - 1
C                                       Check if gone too far.
         IF (KROW.LT.0) THEN
            WRITE (MSGTXT,1435) KROW
            CALL MSGWRT (7)
            GO TO 450
            END IF
         CALL MDISK ('READ', LUNS(2), INDS(2), BUFF2, JBIND, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1360) IER, KROW
            GO TO 995
            END IF
         UMIN = (KROW - 0.5) * SSCLU
C                                       Check if this row desired.
         GO TO 430
  450    CONTINUE
C                                       Apply natural weight correction
C                                       to weight.
         INDEX = OBUFF(OPTR+1) * SSCLV + NY2 + 0.5
         INDEX = MIN (INDEX, NY1)
         OBUFF(OPTR+3) = OBUFF(OPTR+3) / BUFF2(INDEX+JBIND)
         OPTR = OPTR + LRECIN
         INPTR = INPTR + LRECIN
  500    CONTINUE
C                                      Write
      NIOUT = INIO
      CALL UVDISK ('WRIT', LUNS(3), INDS(3), OBUFF, NIOUT, KBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1505) IER
         GO TO 995
         END IF
      OPTR = KBIND
C                                       loop back.
      GO TO 400
C                                       Finish write.
 510  CONTINUE
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNS(3), INDS(3), OBUFF, NIOUT, KBIND, IER)
      IF (IER.NE.0) THEN
         WRITE (MSGTXT,1505) IER
         GO TO 995
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNS(1), INDS(1), IER)
      CALL ZCLOSE (LUNS(2), INDS(2), IER)
      CALL ZCLOSE (LUNS(3), INDS(3), IER)
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('UNIF: ERROR',I3,' OPENING VISIBILITY FILE')
 1030 FORMAT ('UNIF: ERROR',I3,' OPENING GRID FILE FOR WRITE')
 1040 FORMAT ('UNIF: ERROR',I3,' INIT. VIS. FILE FOR READ')
 1050 FORMAT ('UNIF: ERROR',I3,' INIT GRID FILE FOR WRITE')
 1060 FORMAT ('UNIF:',F8.0,' TOO FEW AP WORDS AVAILABLE')
 1070 FORMAT ('UNIF: ERROR',I3,' READING VIS RECORD')
 1160 FORMAT ('UNIF: ERROR',I3,' WRITING GRID ROW',I5)
 1320 FORMAT ('UNIF: ERROR',I3,' OPEN-FOR-WRITE VIS. FILE.')
 1340 FORMAT ('UNIF: ERROR',I3,' INIT.-FOR-WRITE VIS. FILE')
 1360 FORMAT ('UNIF: ERROR',I3,' READING GRID ROW',I5)
 1435 FORMAT ('UNIF: ATTEMPTED TO READ ROW',I6)
 1505 FORMAT ('UNIF: ERROR',I3,' WRITING VIS. RECORD')
      END
