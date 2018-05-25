LOCAL INCLUDE 'EVAUV.INC'
C                                                          Include EVAUV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   UVHIST, MAHIST
      PARAMETER (UVHIST = 302)
      PARAMETER (MAHIST = 402)
C                                       NOTE: uses PARAMETER in DGDS.INC
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XANT(50), XBASE(50),  XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH(3), XNMAPS, XNGAUS, XS2, XDISK2, XVER, XBCOMP(MAXAFL),
     *   XNCOMP(MAXAFL), XFLUX, SMODEL(7), SPIX(4), DOOUT, XSO, XDISKO,
     *   DOHIST, XCELL(2), APARM(10), PXRANG(2), XLABEL, XDOTV,
     *   XGRCH, BADD(10), XTRPRM(10), RPARM(30)
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK,
     *   XNAME2(3), XCLAS2(2), XCMETH, XOUTNM(3), XFUNC
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, CMETH*4, CMOD*4,
     *   OUTNAM*12, FUNCTY*2, MUNITS*8
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS),
     *   BUFF4(UVBFSS), HISTA(UVHIST,2), HISTR(UVHIST,2), RSCALE,
     *   MSCALE(2,2), MFIT(2), REMXN(2,2), IMMXN(2,2)
      INTEGER   NCOMP(MAXFLD), BCOMP(MAXFLD), UVBLK(256), DISKIN, DISK2,
     *   OLDCNO, CLNCNO, JBUFSZ, SEQIN, SEQ2, VER, CHAN, NCHAN, ISTOKE,
     *   METHOD, MODEL, LBIF, LEIF, CH1, ISLFRQ, NGAUSS, SC1FIL, SC2FIL,
     *   HISTM(MAHIST,2), NPARM, IBUFF1(UVBFSS), IBUFF4(UVBFSS), WVOL,
     *   WCNO, WSCI, INCATB(256)
      DOUBLE PRECISION INCATD(128)
      EQUIVALENCE (INCATD, INCATB)
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF4, BUFF4)
      COMMON /INFO/ INCATB, UVBLK, BCOMP, NCOMP, DISKIN, DISK2, OLDCNO,
     *   CLNCNO, ISTOKE, JBUFSZ, SEQIN, SEQ2, VER, CHAN, NCHAN, METHOD,
     *   MODEL, LBIF, LEIF, CH1, ISLFRQ, NGAUSS, SC1FIL, SC2FIL, HISTA,
     *   HISTR, HISTM, RSCALE, MSCALE, NPARM, MFIT, REMXN, IMMXN, WVOL,
     *   WCNO, WSCI
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, BUFF4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XANT, XBASE, XSUBA, XBIF,
     *   XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH, XNMAPS, XNGAUS,
     *   XNAME2, XCLAS2, XS2, XDISK2, XVER, XBCOMP, XNCOMP, XFLUX,
     *   XCMETH, SMODEL, SPIX, DOOUT, XOUTNM, XSO, XDISKO, DOHIST,
     *   XCELL, APARM, PXRANG, XFUNC, XLABEL, XDOTV, XGRCH, BADD,
     *   XTRPRM, RPARM
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAME2, CLAS2, CMETH, CMOD, MUNITS,
     *   OUTNAM, FUNCTY
C                                                          End EVAUV
LOCAL END
LOCAL INCLUDE 'DSPX.INC'
C                                       include for spectral index
C                                       used with SMODEL
      REAL      PTSPIX(4)
      LOGICAL   DOPTSP
      COMMON /PTSPEC/ PTSPIX, DOPTSP
LOCAL END
      PROGRAM EVAUV
C-----------------------------------------------------------------------
C! Subtracts & divides a model into uv data - evaluates statistics
C# UV AP-appl Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2009-2012, 2014-2015, 2017
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
C   EVAUV is a task to subtract a source model from and divide a source
C   model into a uv-data base.  It then does statistics on the results.
C   It also does statistics on the model images.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLASIN        Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      NMAPS          MFIELD        Number of input images.
C      NGAUSS         NGAUS         Number Gaussians in NMAPS
C      IN2NAME        NAME2         Name of map with CLEAN components.
C      IN2CLASS       CLAS2         Class of map with CLEAN components.
C      IN2SEQ         SEQ2          Seq. of map with CLEAN components.
C      IN2DISK        DISK2         Vol. of map with CLEAN components.
C      INVER          VER           Version no. of CC file.
C      BCOMP(64)      BCOMP         Start clean component to sub.
C                                   1 per field.
C      NCOMP(64)      NCOMP         Last Clean component no to sub.
C                                      1 per field, 0 => all
C      CMETHOD        METHOD        Modeling method:
C                                   'DFT' = FDT method
C                                   'GRID' = gridded FFT method.
C                                   '    ' chose fastest.
C      CMODEL         MODEL         Model type, 'COMP'=>CC
C                                   'IMAG'=> image.
C      BADDISK        IBAD          Disk nos. to avoid for scratch files
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, UTYPE*2
      INTEGER   IRET, IERR, N, DSK1, DSK2, CNO1, CNO2
      LOGICAL   DOMSG
      DOUBLE PRECISION DMAP(1025,1025)
      REAL      SCRIMG(1025,1025,2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'EVAUV.INC'
      INCLUDE 'DSPX.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      REAL    BUFF5(MAXIF)
      DATA PRGM /'EVAUV '/
      DATA DOMSG /.TRUE./
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output scratch files
      CALL EVAUVI (PRGM, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .FALSE.
         GO TO 990
         END IF
      DATDIV = .TRUE.
      IF (SC1FIL.GT.0) THEN
         DSK1 = 0
         CNO1 = SC1FIL
         DSK2 = 0
         CNO2 = SC2FIL
      ELSE
         DSK1 = FVOL(-SC1FIL)
         CNO1 = FCNO(-SC1FIL)
         DSK2 = FVOL(-SC2FIL)
         CNO2 = FCNO(-SC2FIL)
         END IF
C                                       Process data
      CALL UVMDVS (-WVOL, WSCI, DSK1, CNO1, DSK2, CNO2, MODEL,
     *   METHOD, DOMSG, CHAN, NCHAN, JBUFSZ, ISLFRQ, BUFF1, BUFF2,
     *   BUFF3, IBUFF4, BUFF5, IRET)
C                                       do image stats: get UV size
      IF ((IRET.EQ.0) .AND. (.NOT.DOPTMD)) CALL EVAIMG (NGAUSS, MAHIST,
     *   PXRANG, MSCALE, MUNITS, HISTM, MFIT, RPARM, BUFF1, IRET)
      IF (.NOT.DOPTMD) CALL UNSETG (BUFF2)
C                                       do UV stats
      IF (IRET.EQ.0) CALL EVAUVS (IRET)
C                                       do plots
      IF (IRET.EQ.0) THEN
         IF (RQUICK) THEN
            CALL PTPARM (30, RPARM, BUFF1, IERR)
            CALL RELPOP (IRET, BUFF1, IERR)
            END IF
         N = APARM(3) + 0.5
         N = MIN (N, 1025)
         N = (N / 2) * 2 + 1
         CALL EVAPLT (N, DMAP, SCRIMG(1,1,1), SCRIMG(1,1,2), IRET)
         IRET = MAX (0, IRET)
C                                       Put input file in READ
         IF (DOHIST.GT.0.0) THEN
            FRW(1) = 0
            UTYPE = 'UV'
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *         UTYPE, NLUSER, 'CLWR', BUFF1, IERR)
            CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *         UTYPE, NLUSER, 'READ', BUFF1, IERR)
            END IF
      ELSE
         RQUICK = .FALSE.
         END IF
C                                       HI files etc
      IF ((IRET.EQ.0) .AND. (DOOUT.GT.0.0)) CALL EVAUVH
C                                       Close down
 990  IF (.NOT.RQUICK) CALL PTPARM (30, RPARM, BUFF1, IERR)
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE EVAUVI (PRGN, IERR)
C-----------------------------------------------------------------------
C   EVAUVI gets input parameters for EVAUV and creates output scratch
C   files.
C   Inputs:
C      PRGN   C*6   Task name
C   Output:
C      IERR   I     Error code: non-zero => quit
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IERR
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   JERR, INMETH, I, IBUFF(512), IROUND, MXFLD, ISIZE,
     *   DISKO, SEQOUT
      LOGICAL   T, F, WASOME
      DOUBLE PRECISION DEMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'EVAUV.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DSPX.INC'
      EQUIVALENCE (IBUFF, BUFF2)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      MXFLD = MAXAFL
      ISLFRQ = -1
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IERR = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 61 + 2 * MXFLD + 254
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .FALSE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (JERR.NE.0) GO TO 999
      IERR = 5
      CALL RFILL (30, 0.0, RPARM)
C                                       Crunch input parameters.
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (4, 1, XCMETH, CMETH)
      CMOD = ' '
      CALL H2CHR (2, 1, XFUNC, FUNCTY)
      CALL H2CHR (12, 1, XOUTNM, OUTNAM)
C                                       File sequence numbers
      SEQIN = IROUND (XSIN)
      SEQ2 = IROUND (XS2)
C                                       File disk numbers
      DISKIN = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
C                                       CC file version number
      VER = IROUND (XVER)
C                                       Default LEIF when have CATBLK
C                                       Number of fields
      MFIELD = 1
      IF (XNMAPS.GT.0.0) MFIELD = IROUND (XNMAPS)
      NGAUSS = XNGAUS
      NGAUSS = MAX (1, NGAUSS)
      LIMFLX = XFLUX
C                                       Start component number
      NONEG = F
      WASOME = F
      DO 8 I = 1,MFIELD
         IF (I.LE.MAXAFL) THEN
            BCOMP(I) = XBCOMP(I) + 0.1
            BCOMP(I) = MAX (BCOMP(I), 1)
            NCOMP(I) = ABS (XNCOMP(I)) + 0.1
            IF (XNCOMP(I).LE.-0.5) NONEG = T
            IF (NCOMP(I).GT.0) WASOME = T
         ELSE
            BCOMP(I) = 1
            NCOMP(I) = 0
            IF (WASOME) NCOMP(I) = 1000000000
            END IF
 8       CONTINUE
C                                       Factor
      FACGRD(1) = 1.0
      FACGRD(2) = 1.0
C                                       Disks to avoid for scratch
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
C                                       Get  modeling method
      METHOD = 0
      IF (CMETH.EQ.'DFT ') METHOD = -1
      IF (CMETH.EQ.'GRID') METHOD = 1
C                                       Get  model type
      MODEL = 0
      IF (CMOD.EQ.'COMP') MODEL = 1
      IF (CMOD.EQ.'IMAG') MODEL = 2
      IF (CMOD.EQ.'SUBI') MODEL = 3
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, 'UV',
     *      DISKIN, NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, INCATB)
C                                       Get uv header info.
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       copy file with cal/flag/etc
      CALL EVCOPY (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reject multisource files (it is
C                                       not strictly necessary to do
C                                       this but input files can not
C                                       contain more than one source
C                                       and EVAUV would need to verify
C                                       this here)
      IF (ILOCSU.GE.0) THEN
         MSGTXT = 'INPUT FILE MUST BE A SINGLE-SOURCE FILE.'
         CALL MSGWRT (8)
         MSGTXT = 'USE SPLIT TO EXTRACT DESIRED DATA.'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
C
C                                       channel number
      CHAN = 1
      NCHAN = CATBLK(KINAX+JLOCF)
C                                       IFs
      LBIF = 1
      LEIF = 1
C                                       Deal with IFs
      IF (JLOCIF.GE.0) THEN
         LEIF = CATBLK(KINAX+JLOCIF)
C                                       Offset CHAN for IF
         IF (JLOCIF.LT.JLOCF) THEN
C                                       do all IFs if all ch in this
C                                       axis order
            IF (NCHAN.GT.1) THEN
               LBIF = 1
               LEIF = CATBLK(KINAX+JLOCIF)
               END IF
            CH1 = CATBLK(KINAX+JLOCIF) * (CHAN-1) + LBIF
C                                       chan first usually
         ELSE
            CH1 = CATBLK(KINAX+JLOCF) * (LBIF-1) + CHAN
            END IF
C                                       Change NCHAN to include IFs
         NCHAN = NCHAN * (LEIF - LBIF + 1)
C                                       Change Start CHAN for   IFs
         CHAN  = CH1
C                                       Reset INCF
         INCF = MIN (INCF, INCIF)
         END IF
C                                       Check order of u,v,w
      IF (((ILOCV-ILOCU).NE.1) .OR. ((ILOCW-ILOCV).NE.1)) THEN
         WRITE (MSGTXT,1070) ILOCU, ILOCV, ILOCW
         IERR = 1
         GO TO 990
         END IF
C                                       Setup common for modeling
C                                       routines
C                                       Uv header block
      CALL COPY  (256, CATBLK, UVBLK)
      INMETH = METHOD
      IF (APARM(1).LE.0.0) APARM(1) = 5.0
      IF (APARM(2).LE.0.0) APARM(2) = 10.0
      IF (APARM(3).LE.0.0) THEN
         DEMP = NCHAN
         DEMP = DEMP * NVIS
         DEMP = SQRT (DEMP/10.0D0)
         DEMP = MAX (50.0D0, MIN (1024.0D0, DEMP))
         APARM(3) = DEMP
         END IF
      I = APARM(4) + 0.1
      I = MIN (I, 33)
      I = (I / 2) * 2 + 1
      APARM(4) = I
C                                       Make sure method='GRID' for
C                                       image model.
      IF (MODEL.GE.2) THEN
         MSGTXT = 'Warning: GRID used for Images models'
         IF (METHOD.EQ.-1) CALL MSGWRT(5)
         METHOD = 1
         END IF
C                                       Check model
      DOPTMD = ABS (SMODEL(1)) .GT. 1.0E-20
      PTFLX = SMODEL(1)
      PTRAOF = SMODEL(2)
      PTDCOF = SMODEL(3)
      PARMOD(1) = SMODEL(4)
      PARMOD(2) = SMODEL(5)
      PARMOD(3) = SMODEL(6)
      PARMOD(4) = SMODEL(7)
      IF (DOPTMD) THEN
         CALL RCOPY (4, SPIX, PTSPIX)
C                                       get spectrum
         IF (PTFLX.LT.0.0) THEN
            CALL GETSPX (PTFLX, PTSPIX, IERR)
            IF (IERR.NE.0) GO TO 999
            SMODEL(1) = PTFLX
            CALL COPY (2, PTSPIX, SPIX)
            END IF
         DOPTSP = (SPIX(1).NE.0.0) .OR. (SPIX(2).NE.0.0)
         RPARM(26) = SMODEL(1)
         CALL RCOPY (4, SPIX, RPARM(27))
      ELSE
         CALL RFILL (4, 0.0, PTSPIX)
         CALL RFILL (5, 0.0, RPARM(26))
         DOPTSP = .FALSE.
         END IF
      ISTOKE = 1
C                                       If NONEG or DOPTMD use DFT
      IF (DOPTMD) THEN
         METHOD = -1
         WRITE (MSGTXT,1090) SMODEL(1), SMODEL(2), SMODEL(3)
         CALL MSGWRT (4)
         MSGTXT = 'IGNORING MODEL IMAGE ' // NAME2 // CLAS2
         IF ((NAME2.NE.' ') .OR. (CLAS2.NE.' ')) CALL MSGWRT (7)
         IF (DOPTSP) THEN
            WRITE (MSGTXT,1091) PTSPIX
            CALL MSGWRT (4)
            END IF
         END IF
C                                       Warn user if changing METHOD
      MSGTXT = '*** WARNING: OVERRIDING SPECIFIED COMPUTATION METHOD'
      IF ((INMETH.NE.METHOD) .AND. (INMETH.NE.0)) CALL MSGWRT (6)
C                                       Get info on model file(s)
      IF (DOPTMD) THEN
         DO3DIM = .FALSE.
      ELSE
         CALL SETGDS (WVOL, WCNO, NAME2, CLAS2, SEQ2, DISK2, MFIELD,
     *      VER, NCOMP, BCOMP, MODEL, METHOD, BUFF1, BUFF2, ISTOKE,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         IF (MODEL.EQ.3) THEN
            MSGTXT = 'Using sub-images for the source model'
         ELSE IF (MODEL.EQ.2) THEN
            MSGTXT = 'Using images for the source model'
         ELSE
            MSGTXT = 'Using Clean Component source model'
            END IF
         CALL MSGWRT (3)
         CALL FACSET (WVOL, WCNO, 1, 0, MODEL, FACGRD, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Create scratch files
      IF (DOOUT.LE.0.0) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         CALL SCREAT (ISIZE, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'SCRATCH'
            GO TO 990
            END IF
         SC1FIL = NSCR
         CALL SCREAT (ISIZE, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'SCRATCH'
            GO TO 990
            END IF
         SC2FIL = NSCR
C                                       cataloged files out
      ELSE
         CALL COPY (256, UVBLK, CATBLK)
         SEQOUT = IROUND (XSO)
         DISKO = IROUND (XDISKO)
         IF (OUTNAM.EQ.' ') OUTNAM = NAMEIN
         CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, 'EVAUVS', KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
         CCNO = 1
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'CATALOGED'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = 2
         SC1FIL = -NCFILE
C                                       copy keywords
         CALL KEYCOP (WVOL, WCNO, DISKO, CCNO, IERR)
C                                       division file
         CALL COPY (256, UVBLK, CATBLK)
         SEQOUT = IROUND (XSO)
         DISKO = IROUND (XDISKO)
         CALL CHR2H (12, OUTNAM, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, 'EVAUVD', KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQOUT
         CCNO = 1
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'CATALOGED'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = CCNO
         FRW(NCFILE) = 2
         SC2FIL = -NCFILE
C                                       copy keywords
         CALL KEYCOP (WVOL, WCNO, DISKO, CCNO, IERR)
         CALL COPY (256, UVBLK, CATBLK)
         END IF
C                                       Put input file in READ
      IF (DOHIST.GT.0.0) THEN
         STAT = 'WRIT'
         FRW(1) = 1
      ELSE
         STAT = 'READ'
         END IF
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IERR = 0
      GO TO 999
C                                       Error messages output
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVAUVI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,2X,A2,' DISK=',I3,' USID=',I4)
 1020 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('WRONG ORDER FOR U, V, W =',3I4)
 1090 FORMAT ('Using SMODEL =',F10.5,2F12.5)
 1091 FORMAT ('With spectral index =',4F9.4)
 1130 FORMAT ('ERROR',I3,' CREATING OUTPUT ',A,' FILE')
      END
      SUBROUTINE GETSPX (PTFLX, PTSPIX, IERR)
C-----------------------------------------------------------------------
C   GETSPX tries to get the flux and spectral index parameters from the
C   SU table
C   In/out:
C      PTFLX    R      Flux at obs frequency: -2 in -> curvature
C   Output
C      PTSPIX   R(2)   Spectral index, curvature
C      IERR     I      Failed
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      PTFLX, PTSPIX(*)
C
      INCLUDE 'INCS:DSEL.INC'
      REAL      SPIX(4)
      INTEGER   NTERM
      DOUBLE PRECISION DT, X
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NTERM = 1
      IF (PTFLX.LT.-1.5) NTERM = 2
      CALL FNDSPX (IUDISK, IUCNO, SOUWAN, FRQSEL, CATUV, NTERM, SPIX,
     *   IERR)
      IF (FREQ.LE.0.0D0) THEN
         IERR = 10
         MSGTXT = 'GETSPX: FREQ IS ZERO QUITTING'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DT = LOG10 (FREQ/1.D9)
      X = SPIX(2) * DT + SPIX(3) * DT * DT
      PTFLX = SPIX(1) * (10.0D0 ** X)
      IF (NTERM.LE.1) THEN
         PTSPIX(1) = SPIX(2)
         PTSPIX(2) = 0.0
      ELSE
         PTSPIX(1) = SPIX(2) + 2.0D0 * DT * SPIX(3)
         PTSPIX(2) = SPIX(3)
         END IF
C
 999  RETURN
      END
      SUBROUTINE EVCOPY (IRET)
C-----------------------------------------------------------------------
C   EVCOPY interprets the input adverbs that apply data selection,
C   flagging, and calibration.  It creates a scratch file to receive
C   the data sp processed and fills it.
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'EVAUV.INC'
      INTEGER   NXANT, NXBAS, IXANT(50), IXBAS(50), LUN, NFREQ, FIND,
     *   BO, LENBU, BIND, VO, IPTRO, NIOLIM, NIOUT, IA1, IA2, CNOSCR,
     *   NCOPY, IROUND, ISIZE, IERR, I
      LOGICAL   DESEL, T, F, REQBAS, MATCH
      REAL      VPARM(20), BASEN
      CHARACTER NAME*48
      DOUBLE PRECISION XCOUNT
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Info for UVGET:
      CALL SELINI
C                                       Put selection criteria into
C                                       correct common.
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         IRET = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 78
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      ISLFRQ = FRQSEL
C                                       Find baselines to copy
      CALL SETANT (50, XANT, XBASE, NXANT, NXBAS, IXANT, IXBAS, DESEL)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', VPARM, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN WITH UVGET'
         GO TO 990
         END IF
C                                       Determine size.
      LENBU = 1
      CALL UVSIZE (LREC, NVIS, ISIZE)
C                                       Create output file if necessary
C                                       Create scratch file.
      CALL SCREAT (ISIZE, BUFF1, IRET)
      CNOSCR = NSCR
      IF (IRET.NE.0) THEN
         IF (IRET.EQ.1) THEN
            MSGTXT = 'EVCOPY: TOO LITTLE DISK SPACE FOR SCRATCH FILE'
         ELSE
            WRITE (MSGTXT,1000) IRET, 'CREATING SCRATCH FILE'
            END IF
         GO TO 990
         END IF
C                                       Update CATBLK: ignore error
      CALL CATIO ('UPDT', SCRVOL(CNOSCR), SCRCNO(CNOSCR), CATBLK,
     *   'REST', BUFF1, IRET)
      WVOL = SCRVOL(CNOSCR)
      WCNO = SCRCNO(CNOSCR)
      WSCI = CNOSCR
      CALL ZPHFIL ('SC', WVOL, WCNO, 1, NAME, IRET)
C                                       Open output file.
      CALL ZOPEN (LUN, FIND, WVOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      LENBU = 0
      CALL UVINIT ('WRIT', LUN, FIND, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFF2, BO, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT FILE'
         GO TO 990
         END IF
      IPTRO = BIND
      NIOUT = 0
      NIOLIM = LENBU
      XCOUNT = 0.0D0
      NCOPY = LREC - NRPARM
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', VPARM, BUFF1, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING VIS DATA'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            BASEN = VPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = VPARM(1+ILOCA1) + 0.1
            IA2 = VPARM(1+ILOCA2) + 0.1
            END IF
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IXANT, NXANT, IXBAS, NXBAS))
     *      GO TO 100
C                                       Copy to output.
         XCOUNT = XCOUNT + 1.0D0
         CALL RCOPY (NRPARM, VPARM, BUFF2(IPTRO))
         CALL RCOPY (NCOPY, BUFF1, BUFF2(IPTRO+NRPARM))
         IPTRO = IPTRO + LREC
         NIOUT = NIOUT + 1
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUN, FIND, BUFF2, NIOLIM, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING VIS DATA'
               GO TO 990
               END IF
            IPTRO = BIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUN, FIND, BUFF2, NIOUT, BIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISHING WRITING VIS DATA'
         GO TO 990
         END IF
C                                       tell
      NVIS = XCOUNT + 0.001D0
      WRITE (MSGTXT,1100) NVIS
      IF (NVIS.LE.0) THEN
         IRET = 10
         GO TO 990
      ELSE
         CALL MSGWRT (5)
         IRET = 0
         END IF
C                                       Compress output file.
      CALL UCMPRS (NVIS, WVOL, WCNO, LUN, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', VPARM, BUFF1, IRET)
      CALL ZCLOSE (LUN, FIND, IRET)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, WVOL, WCNO, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, WVOL, WCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'UVCOP: ERROR COPYING TABLES TO OUTPUT UV SCRATCH'
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCOP: ERROR',I4,' ON ',A)
 1100 FORMAT ('Copied',I12,' visibilities to the work file')
      END
      SUBROUTINE UVMDVS (DISKI, CNOSCI, DISK1, CNOSC1, DISK2, CNOSC2,
     *   MODEL, METHOD, DOMSG, CHANEL, NCHAN, JBUFSZ, FREQID,
     *   BUFF1, BUFF2, BUFF3, BUFF4, BUFF5, IRET)
C-----------------------------------------------------------------------
C   UVMDIV divides model visibilities derived from CLEAN components
C   or images into a uv data set.  The weights of the data returned
C   will be the input values multiplied by the model amplitude.
C      A variety of model computation methods are available; if a single
C   pass thru UVMDVS, the DFT routine, is not sufficient then the data
C   is copied to a scratch file which has space for a second copy of the
C   data, the model values are computed and summed in these locations
C   and finally the model is divided into the data and written to the
C   output file.
C      Extensive use is made of commons to communicate with UVMDIV, in
C   particular /MAPDES/ (include DGDS.INC) contains most of the
C   critical information about the CLEAN components files or images to
C   be used.  Common /UVHDR/ (filled in by UVPGET) is presumed to
C   describe the uv data files.
C      Also fills in frequency table (NCHANG, FREQG) in include
C   DGDS.INC
C   Inputs:
C      DISKI    I        Input disk number. if <= 0 then input is a
C                        scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISK1    I        Output disk number. if <= 0 then output is a
C                        scratch file.
C      CNOSC1   I        Output file catalog slot number or /CFILES/
C                        scratch file number.  If <= 0 then one of the
C                        internal scratch files will be used.
C      MODEL    I        1=> clean components, 2=>image.
C      METHOD   I        1=>gridded, -1=>DFT, 0=>chose.
C      DOMSG    L        If true give percent done messages for DFT.
C      CHANEL   I        First uv data channel to subtract.
C      NCHAN    I        Number of frequency channels to subtract.
C      CATBLK   I(256)   UV data catalog header record.
C      JBUFSZ   I        Size of BUFF1,2,3 in bytes, must be at least
C                        4096 words.
C      FREQID   I        Freq ID number, if it exists.
C   Inputs from COMMON /MAPDES/:
C      MFIELD   I        Number of fields
C      NSUBG    I(*)     Number of components already sub.
C      NCLNG    I(*)     Number of components per field.
C      CCDISK   I(*)     Disk numbers for CC files
C      CCCNO    I(*)     Catalog slot numbers for CC files.
C      CCVER    I(*)     CC file version number for each field.
C      FACGRD   R(2)     Value to multiply clean component fluxes by
C                        before subtraction (negative for sum).
C                        FACGRD(2) is for data and 0 or 1 only values
C                        used.  Model added not subtracted when data are
C                        ignored.
C      SCTYPE   C*2      Scratch file type to create. (eg. 'SC')
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop when flux < LIMFLX.
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF (DFT modeling ONLY)
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C   Input from COMMON /UVHDR/:
C      LREC     I        Length of visibility record.
C      NVIS     I        Number of visibility records.
C      NRPARM   I        "Random" parameters before data, can be used
C                        to skip observed values when computing model.
C   Output:
C      CNOSCO   I        Output file catalog slot number or /CFILES/
C                        scratch file number.  Value returned if not
C                        specified in call.
C      BUFF1    R(*)     Work buffers.
C      BUFF2    R(*)     Work buffers.
C      BUFF3    R(*)     Work buffers.
C      BUFF4    R(*)     Work buffers.
C      BUFF5    R(*)     Work buffers.
C      IRET     I        Return error code. 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOSCI, DISK1, CNOSC1, DISK2, CNOSC2, MODEL,
     *   METHOD, CHANEL, NCHAN, JBUFSZ, FREQID, BUFF4(*), IRET
      LOGICAL   DOMSG
      REAL      BUFF1(*), BUFF2(*), BUFF3(*), BUFF5(*)
C
      INTEGER   I, LENMOD, ISCR2, DISKX, SAVNRP, SAVLRC, DISK, CNO, LUN,
     *   LUN2, INMETH, OUMETH, CNOX, LBIF, LEIF, CATSCR(256), TEMP(256),
     *   LENBU, MXCMP, XNCC, APSIZ, SCFRW, IV, OV, NEED, MSGSAV, KAP,
     *   LCHANL, LCHAN
      LOGICAL   DOSUM, DODFT, F, LTEMP
      REAL      BUFSZ, TIMDFT, TIMFFT
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION SFOFF(MAXIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:DMOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE LUN, LUN2
      DATA LENMOD /7/, LUN/27/, LUN2/28/
      DATA F /.FALSE./
      DATA SFOFF /MAXIF*0.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      NGRDAT = F
C                                       Store CATBLK for later use
      CALL COPY (256, CATBLK, SCRCAT)
      SCLREC = LREC
      SCRPRM = NRPARM
      COMPDT = CATBLK(KINAX).EQ.1
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', SCRCAT(KIPCN), SCRHOL(KHPTP),
     *      WTLOC, IRET)
         IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IRET = 0
         END IF
C                                       Decide model computation method.
      INMETH = METHOD
      DODFT = (METHOD.LT.0) .AND. (MODEL.EQ.1)
C                                       DFT only for pt. model
      CALL UVMTYP (0, METHOD, NCHAN, LTEMP, TIMDFT, TIMFFT)
      IF (MODEL.EQ.1) DODFT = LTEMP
      DODFT = DODFT .OR. DOPTMD
      OUMETH = 1
      IF (DODFT) OUMETH = -1
C                                       If doing Direct Fourier Trans.
      IF (DODFT) THEN
C                                       Check point model
         IF (DOPTMD) THEN
            XNCC = 1
         ELSE
            XNCC = 0
            DO 20 I = 1,MFIELD
               XNCC = XNCC + NCLNG(I) - NSUBG(I)
 20            CONTINUE
            END IF
C                                       Make AP memory for this
         BUFSZ = JBUFSZ
         LENBU = ((BUFSZ-2*NBPS) / 2) / (LREC*2)
         NEED = 12 + (LENBU*LREC) + CATBLK(KINAX+JLOCF) + XNCC*LENMOD
     *      + 2*LENMOD
         NEED = NEED / 1024
         MSGSAV = MSGSUP
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = MSGSAV
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 12 + (LENBU*LREC) + CATBLK(KINAX+JLOCF) +
     *         XNCC*LENMOD/10 + 2*LENMOD
            NEED = NEED / 1024
            NEED = MIN (32+1024, NEED) + 2
            CALL QINIT (NEED, 0, KAP)
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               IRET = 8
               MSGTXT = 'UVMDIV CANNOT GET NEEDED MEMORY'
               CALL MSGWRT (8)
               END IF
            END IF
         APSIZ = PSAPNW * 1024
         CALL QRLSE
C                                       Decide if will fit.in AP
         MXCMP = (APSIZ-10.-(LENBU*LREC)-2.-CATBLK(KINAX+JLOCF))
     *    / LENMOD
         MXCMP = MXCMP - 2
C                                       If room for all components
         IF (XNCC.LE.MXCMP) THEN
C                                       IF using a scratch file
            IF (DISKI.LE.0) THEN
               DISK = SCRVOL(CNOSCI)
               CNO = SCRCNO(CNOSCI)
            ELSE
               DISK = DISKI
               CNO = CNOSCI
               END IF
C                                       Fill Frequency table.
            CALL FRQTAB (DISK, CNO, LUN, CATBLK, FREQID, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       Divide model.
            CALL EVADFT (CHANEL, NCHAN, DISKI, CNOSCI, DISK1, CNOSC1,
     *         DISK2, CNOSC2, 0, DOSUM, DOMSG, CATR, JBUFSZ, BUFF1,
     *         BUFF2, BUFF3, BUFF4, IRET)
C                                       Check for too many comps.
            IF (IRET.EQ.10) GO TO 100
            GO TO 999
            END IF
         END IF
C                                       Use UVMSUB for model.
C                                       Copy to padded scratch file.
 100  ISCR2 = 0
C                                       Message to about division
      MSGTXT = 'Divide data by model - first compute model by summing'
      IF (MSGSUP.NE.32000) CALL MSGWRT (3)
      CALL UVDPAD (DISKI, CNOSCI, ISCR2, JBUFSZ, BUFF1, BUFF2, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get scratch file header
      IF (DISKI.LE.0) THEN
         DISKX = SCRVOL(CNOSCI)
         CNOX = SCRCNO(CNOSCI)
      ELSE
         DISKX = DISKI
         CNOX = CNOSCI
         END IF
      CALL CATIO ('READ', DISKX, CNOX, CATSCR, 'REST', BUFF1, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.4)) GO TO 999
      IRET = 0
C                                       Determine LBIF, LEIF
      LBIF = 1
      LEIF = 1
C                                       If more than 1 IF
      IF (JLOCIF.GT.0) THEN
         LEIF = CATBLK(KINAX+JLOCIF)
C                                       Copy part portion of IF table
         IV = 1
         OV = 1
         CALL CHNCOP (IV, OV, LUN, LUN2, DISKX, SCRVOL(ISCR2), CNOX,
     *      SCRCNO(ISCR2), CATBLK, CATSCR, LBIF, LEIF, FREQID, SFOFF,
     *      BUFF1, BUFF2, BUFF3, BUFF5, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       End if more than 1 IF
         END IF
C                                       Compute model.
C                                       Set factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       Redefine record size in
C                                       /UVHDR/.
      DISKX = 0
      SAVNRP = NRPARM
      SAVLRC = LREC
      NRPARM = LREC
      LREC = 2 * SAVLRC - SAVNRP
      CALL UVMSUB (DISKX, ISCR2, DISKX, ISCR2, 0, MODEL, METHOD, CHANEL,
     *   NCHAN, F, DOMSG, CATBLK, JBUFSZ, FREQID, BUFF1, BUFF2, BUFF3,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Restore /UVHDR/
      NRPARM = SAVNRP
      LREC = SAVLRC
C                                       Reset factor for subtraction.
      FACGRD(1) = - FACGRD(1)
C                                       Divide/compress record to output
      LCHANL = CHANEL
      LCHAN = NCHAN
      IF (MODMAX.GT.0) THEN
         LCHAN = 0
         LCHANL = 1000000
         DO 110 I = 1,MODMAX
            IV = MODCHN(I) + MODNCH(I) - 1
            LCHAN = MAX (LCHAN, IV)
            LCHANL = MIN (LCHANL, MODCHN(I))
 110        CONTINUE
         END IF
      CALL UVDSOU (ISCR2, DISK1, CNOSC1, DISK2, CNOSC2, LCHANL,
     *   LCHAN, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete scratch file
      IF (ISCR2.GT.0) THEN
         CALL COPY (256, CATBLK, TEMP)
         SCFRW = 2
         IV = 1
         CALL MAPCLR (IV, SCRVOL(ISCR2), SCRCNO(ISCR2), SCFRW, BUFF1)
         IF (ISCR2.EQ.NSCR) NSCR = NSCR - 1
         CALL COPY (256, TEMP, CATBLK)
         END IF
C
 999  RETURN
      END
      SUBROUTINE UVDSOU (ISCR1, DISK1, CNOSC1, DISK2, CNOSC2, CHANEL,
     *   NCHAN, JBUFSZ, BUFF1, BUFF2, BUFF3, IRET)
C-----------------------------------------------------------------------
C   UVDSOU works on visibility data in which the lower half has
C   observed values and the upper half, in identical form, has model
C   values.  The model values are divided into the observed values and
C   only the ratios are written to the output.
C      The weight of the output record is multiplied by the amplitude
C   of the model visibility.
C      The contents of /UVHDR/ are assumed valid for the output.
C      If disk .LE. 0 then CNOSCR is assumed to be the /CFILES/ scratch
C   file number.  Uses LUNs 24 and 25.
C   Inputs:
C      ISCR1    I      /CFILES/ number of scratch file for input
C      DISK1    I      Disk number for catalogd output file. If .LE. 0
C                      then the output file is a /CFILES/ scratch file.
C      CNOSC1   I      Catalog slot number if catalogd output file;
C                      /CFILES/ scratch file number if a scratch file,
C      DISK2    I      Disk number for catalogd output file. If .LE. 0
C                      then the output file is a /CFILES/ scratch file.
C      CNOSC2   I      Catalog slot number if catalogd output file;
C                      /CFILES/ scratch file number if a scratch file,
C      CHANEL   I      First uv data channel to divide.
C      NCHAN    I      Number of model channels to divide.
C      JBUFSZ   I      The size of BUFF1 and BUFF2 in bytes.
C   Inputs from common /UVHDR/:
C      LREC     I      Length of visibility record in words.
C      NRPARM   I      Number of random parameters
C      NVIS     I      Number of visibilities
C   Output:
C      BUFF1    R(*)   Work buffers
C      BUFF2    R(*)   Work buffers
C      ISCR2    I      /CFILES/ number of output scratch file.
C      IRET     I      Return code, 0 => OK, otherwise failed.
C-----------------------------------------------------------------------
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
      INTEGER   ISCR1, DISK1, CNOSC1, DISK2, CNOSC2, CHANEL, NCHAN,
     *   JBUFSZ, IRET
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER NAME*48
      INTEGER   VOL, FINDI, FIND1, FIND2, KLOOP, LUNI, LUN1, LUN2,
     *   BINDI, BIND1, BIND2, LENBO, INIO, ONIO, LRECI, VVADD, OCNT,
     *   ILOOP, INCVIS, JLOOP, LENBI, IPM, IPO, OP1, OP2, IIPM, IIPO,
     *   OOP1, OOP2, BO, VO, LDATA, LRECUN, UNPARM
      LOGICAL   T, F
      REAL      AMP, AMPI, RATRE, RATIM, WT, TEMPBF(UVBFSS), WTSCL(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSCD.INC'
      DATA LUNI, LUN1, LUN2, BO, VO /24,25,26, 1,0/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set output file name.
      IF (DISK1.LE.0) THEN
         VOL = SCRVOL(CNOSC1)
         CALL ZPHFIL ('SC', SCRVOL(CNOSC1), SCRCNO(CNOSC1), 1, NAME,
     *      IRET)
      ELSE
         VOL = DISK1
         CALL ZPHFIL ('UV', VOL, CNOSC1, 1, NAME, IRET)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN1, FIND1, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Set output file name.
      IF (DISK2.LE.0) THEN
         VOL = SCRVOL(CNOSC2)
         CALL ZPHFIL ('SC', SCRVOL(CNOSC2), SCRCNO(CNOSC2), 1, NAME,
     *      IRET)
      ELSE
         VOL = DISK2
         CALL ZPHFIL ('UV', VOL, CNOSC2, 1, NAME, IRET)
         END IF
C                                       Open output file.
      CALL ZOPEN (LUN2, FIND2, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WRIT', IRET
         GO TO 990
         END IF
C                                       Setup to init I/O, determine
C                                       size.
      LENBO = 0
      CALL UVINIT ('WRIT', LUN1, FIND1, NVIS, VO, LREC, LENBO, JBUFSZ,
     *   BUFF1, BO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'WRIT', IRET
         GO TO 990
         END IF
      LENBO = 0
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, LENBO, JBUFSZ,
     *   BUFF2, BO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'WRIT', IRET
         GO TO 990
         END IF
      OCNT = 0
      ONIO = LENBO
C                                       Open input file.
      CALL ZPHFIL ('SC', SCRVOL(ISCR1), SCRCNO(ISCR1), 1, NAME, IRET)
      CALL ZOPEN (LUNI, FINDI, SCRVOL(ISCR1), NAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'READ', IRET
         GO TO 990
         END IF
C                                       Setup for init.
      LRECI = 2 * SCLREC - SCRPRM
      IF (COMPDT) LRECI = SCLREC
      LENBI = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LRECI, LENBI, JBUFSZ,
     *   BUFF3, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) 'READ', IRET
         GO TO 990
         END IF
C                                       Set up for reformatting.
      INCVIS = MIN (INCS, INCF)
      INCVIS = MAX (2, INCVIS)
      VVADD = (CHANEL - 1) * INCF + VOFF
      LRECUN = LREC
      UNPARM = NRPARM
      IF (COMPDT) THEN
         LRECUN = LREC - NRPARM
         LRECUN = LRECUN * 3
         LRECUN = LRECUN + NRPARM - 2
         UNPARM = NRPARM - 2
         END IF
      LDATA = (LRECUN - UNPARM) / INCS
      IF (LDATA.GT.UVBFSS) THEN
         MSGTXT = 'UVDSOU: TEMPORARY BUFFER TOO SMALL - MODIFY'
         IRET = 1
         GO TO 990
         END IF
C                                       Begin loop
 100     CALL UVDISK ('READ', LUNI, FINDI, BUFF3, INIO, BINDI, IRET)
         IF (INIO.LE.0) GO TO 250
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       Loop copying to output, zero
C                                       padding.
         DO 200 ILOOP = 1,INIO
C                                       Copy random parameters and obs.
            CALL RCOPY (LRECUN, BUFF3(BINDI), BUFF1(BIND1))
            CALL RCOPY (LRECUN, BUFF3(BINDI), BUFF2(BIND2))
            IIPO = BINDI + UNPARM + VVADD
            IIPM = BINDI + LRECUN + VVADD
            OOP1 = BIND1 + UNPARM + VVADD
            OOP2 = BIND2 + UNPARM + VVADD
C                                       Divide
C                                       Loop over channel.
            DO 150 JLOOP = 1,NCHAN
C                                       Set indices.
               IPO = IIPO
               IPM = IIPM
               OP1 = OOP1
               OP2 = OOP2
C                                       Loop over stokes'
               DO 130 KLOOP = 1,NSTOK
C                                       subtraction
                  BUFF1(OP1) = BUFF3(IPO) - BUFF3(IPM)
                  BUFF1(OP1+1) = BUFF3(IPO+1) - BUFF3(IPM+1)
                  BUFF1(OP1+2) = BUFF3(IPO+2)
C                                       division
                  AMP = BUFF3(IPM)*BUFF3(IPM) +
     *               BUFF3(IPM+1)*BUFF3(IPM+1)
                  RATRE = 0.0
                  RATIM = 0.0
                  WT = 0.0
C                                       Check for small values.
                  IF (AMP.GE.1.0E-20) THEN
                     AMPI = 1.0 / AMP
                     RATRE = AMPI * ( BUFF3(IPO)*BUFF3(IPM) +
     *                  BUFF3(IPO+1)*BUFF3(IPM+1))
                     RATIM = AMPI * ( BUFF3(IPM)*BUFF3(IPO+1) -
     *                  BUFF3(IPO)*BUFF3(IPM+1))
                     WT = AMP * BUFF3(IPO+2)
                     END IF
C                                       Fill in.
                  BUFF2(OP2) = RATRE
                  BUFF2(OP2+1) = RATIM
                  BUFF2(OP2+2) = WT
C                                       Update pointers for stokes'
                  IPO = IPO + INCS
                  IPM = IPM + INCS
                  OP1 = OP1 + INCS
                  OP2 = OP2 + INCS
 130              CONTINUE
C                                       Update pointers for freq.
               IIPO = IIPO + INCF
               IIPM = IIPM + INCF
               OOP1 = OOP1 + INCF
               OOP2 = OOP2 + INCF
 150           CONTINUE
C                                       Compress data if needed
            IF (COMPDT) THEN
               CALL ZUVPAK (LDATA, BUFF1(BIND1+UNPARM),
     *            WTSCL,  TEMPBF)
               CALL RCOPY (2, WTSCL, BUFF1(BIND1+UNPARM))
               CALL RCOPY (LDATA, TEMPBF, BUFF1(BIND1+UNPARM+2))
               CALL ZUVPAK (LDATA, BUFF2(BIND2+UNPARM),
     *            WTSCL,  TEMPBF)
               CALL RCOPY (2, WTSCL, BUFF2(BIND2+UNPARM))
               CALL RCOPY (LDATA, TEMPBF, BUFF2(BIND2+UNPARM+2))
               END IF
C                                       Update pointers.
            BINDI = BINDI + LRECI
            BIND1 = BIND1 + LREC
            BIND2 = BIND2 + LREC
C                                       Check if time for output.
            OCNT = OCNT + 1
C                                       Write
            IF (OCNT.GE.ONIO) THEN
               ONIO = OCNT
               CALL UVDISK ('WRIT', LUN1, FIND1, BUFF1, ONIO, BIND1,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT', IRET
                  GO TO 990
                  END IF
               ONIO = OCNT
               CALL UVDISK ('WRIT', LUN2, FIND2, BUFF2, ONIO, BIND2,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT', IRET
                  GO TO 990
                  END IF
               OCNT = 0
               END IF
 200        CONTINUE
C                                       Loop back for more.
         GO TO 100
C                                       Done - flush output buffer.
 250  ONIO = -OCNT
      CALL UVDISK ('FLSH', LUN1, FIND1, BUFF1, ONIO, BIND1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IRET
         GO TO 990
         END IF
      ONIO = -OCNT
      CALL UVDISK ('FLSH', LUN2, FIND2, BUFF2, ONIO, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IRET
         GO TO 990
         END IF
C                                       Close files.
      CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUN1, FIND1, IRET)
      CALL ZCLOSE (LUN2, FIND2, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDSOU: OPEN FOR ',A4,' ERROR ',I5)
 1010 FORMAT ('UVDSOU: INIT FOR ',A4,' ERROR ',I5)
 1100 FORMAT ('UVDSOU: ',A4,' ERROR ',I5)
      END
      SUBROUTINE EVADFT (CHANEL, NCHAN, DISKI, CNOSCI, DISK1, CNOSC1,
     *   DISK2, CNOSC2, IFIELD, DOSUM, DOMSG, CATR, JBUFSZ, BUFF1,
     *   BUFF2, BUFF3, IBUFF, IRET)
C-----------------------------------------------------------------------
C   EVADFT subtracts AND divides CLEAN components from/into ungridded
C   visibility data by a DFT model computation.  Only model components
C   of a single type are processed.  Point components will be taken as
C   Gaussians or Spheres as needed if some of the fields are extended
C   and some not.
C   All un subtracted data processed in one call.
C   Inputs:
C      CHANEL   I        Frequency channel: used if MODMAX=0
C      NCHAN    I        Number of frequency channels.: ditto
C      DISKI    I        Input file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.
C      CNOSCI   I        Input file catalog slot number or /CFILES/
C                        scratch file number.
C      DISK1    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.  subtraction
C      DISK2    I        Output file disk number for cataloged files,
C                        .LE. 0 => /CFILES/ scratch file.  division
C      IFIELD   I        Field to do (0 -> all): used if MODMAX=0
C      DOSUM    L        If true sum the flux in each field
C      DOMSG    L        If true give percent done messages.
C      CATR     R(256)   UV data catalog header record.
C      JBUFSZ   I        Size of BUFF1,2, IBUFF in AIPS bytes, each
C                        must be at least 4096 words.
C   Inputs: from commons
C      MODMAX   I        DMOD.INC - if set, this controls facets/chans
C      MFIELD   I        Number of fields
C      NCLNG    I(16)    Number of components per field. -
C                        changed if flux limit hit
C      NSUBG    I(16)    The next component to subtract.
C      CCDISK   I(16)    Disk numbers of the clean images.
C      CCCNO    I(16)    Catalog slot numbers of clean images.
C      CCVER    I(*)     CC file version number for each field.
C      NGRDAT   L        If FALSE get map size, scaling etc. parms
C                        from the model map cat. header. If TRUE
C                        then the values filled in by GRDAT must
C                        already be filled into the common.
C      LREC     I        Length in words of vis record.
C      NVIS     I        Number of vis. records
C      NONEG    L        Stop reading comps. from a file past the first
C                        negative component.
C      LIMFLX   R        Stop if abs(flux) < LIMFLX
C      DOPTMD   L        Use the point model specified by PTFLX, PTRAOF,
C                        PTDCOF
C      PTFLX    R        Point model flux density (Jy) (I pol. only)
C      PTRAOF   R        Point model RA offset from uv phase center
C                        (asec)
C      PTDCOF   R        Point model Dec. offset from uv phase center
C      PARMOD   R(6)     Model parameters for non point models; used
C                        only if DOPTMOD is true.
C                        1=> model type, 0=point, 1=gaussian, 3=sphere
C                        Gaussian: (2)=major axis(asec), (3)=minor axis
C                                  (4)=PA (degrees)
C                        Sphere: (2)=radius (asec).
C      KSTOK    I        (DGDS.INC) If a point model is specified a
C                        value of 2 indicates a Q pol model, 3 U and
C                        4 V pol.AC
C   In/out:
C      CNOSC1   I        IN: output file catalog slot number or /CFILES/
C                        scratch file number. Will create a scratch file
C                        if CNOSC1 and DISK1 .le. 0.
C                        Out: file /CFILES/ number if created.
C      CNOSC2   I        Output file 2 catalog slot number or /CFILES/
C                        scratch file number.
C   Output:
C      BUFF1    R(*)     I/O buffers.
C      BUFF2    R(*)     I/O buffers.
C      IBUFF    I(*)     I/O Buffer.
C      IRET     I        Return code, 0 => ok, otherwise not.
C                            8 => model types not compatible/illegal
C                            9 => Buffers too small to load AP.
C                           10 => Too many components for division.
C-----------------------------------------------------------------------
      INTEGER   CHANEL, NCHAN, DISKI, CNOSCI, DISK1, CNOSC1, DISK2,
     *   CNOSC2, IFIELD, JBUFSZ, IBUFF(*), IRET
      LOGICAL   DOSUM, DOMSG
      REAL      BUFF1(*), BUFF2(*), BUFF3(*), CATR(256)
C
      CHARACTER KEYS(7)*8, NAME*48, MDTYP(4)*8, ERRTXT*40, UMET*4
      INTEGER   JCOMP, JNCOMP, CCOUNT, XNCOMP, MXCMP, CURCMP, JT,
     *   NCOMP, J, MCOMP, VO, BO, ISIZE, INIO, MMCOMP, NNCOR, IDATA, UV,
     *   LLREC, IAPBUF, IAPCC0, IAPCT, LMCOMP, IAPTMP, VIS, WRK, LLNMOD,
     *   MCHAN, JNCS, JNCF, KAP, SFLAG, APSIZ, MXCC, INIO2, LUNC, VOL,
     *   INDEX, ITYPE, NIOUT, KBIND1, KBIND2, LENBU, LENMOD, JLREC,
     *   JNREC, FINDI, FIND1, FIND2, I, LUNI, LUN1, LUN2, ITIME(3),
     *   IBIND, LFIELD, LMOD(4), NKEY, KOLS(7), RAKOL, DECKOL, FLXKOL,
     *   TYPKOL, MAJKOL, MINKOL, PAKOL, IPCLST, IPCDNE, NTIMES, MODTYP,
     *   LRPARM, LF1, LF2, NEED, MSGSAV, LCHAN, I1, I2, FLD1, FLD2,
     *   CHN1, CHN2
      REAL      XXOFF, YYOFF, ZZOFF, RECORD(20), FACT2(4), CPA, SPA,
     *   XMAJ, XMIN, ABFACG, XYZ(3), XP(3), UMAT(3,3), PMAT(3,3), UUU,
     *   BTEMP
      DOUBLE PRECISION XTLST, PCTOT, PCLST, XRA, XDEC, XPR, YPR, CONST,
     *   CONST2
      LOGICAL   T, F, WESET
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMOD.INC'
      LOGICAL   DONE(MAXFLD), ONZE
      INTEGER   IAPCC, MFR, NFR, LFR, MMCMP(MAXMOD)
      DOUBLE PRECISION FFRAC, X, Y, PTFL0
      REAL      FBUFF(MAXCIF)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMPR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'DSPX.INC'
      PARAMETER (CONST = DG2RAD * TWOPI)
      SAVE ONZE
      EQUIVALENCE (KOLS(1), RAKOL),       (KOLS(2), DECKOL),
     *   (KOLS(3), FLXKOL),               (KOLS(4), TYPKOL),
     *   (KOLS(5) ,MAJKOL),               (KOLS(6), MINKOL),
     *   (KOLS(7), PAKOL)
      DATA KEYS /'DELTAX  ', 'DELTAY  ', 'FLUX    ',
     *   'TYPE OBJ', 'MAJOR AX', 'MINOR AX', 'POSANGLE'/
      DATA LMOD /4, 7, 7, 6/
      DATA MDTYP /'Point   ', 'Gaussian', 'Unknown ', 'Sphere  '/
      DATA VO, BO, MXCC /0, 1, 1024/
      DATA LUNI, LUN1, LUN2, LUNC /22,23,24,29/
      DATA T, F /.TRUE.,.FALSE./, ONZE/.FALSE./
C-----------------------------------------------------------------------
      UMET = 'DFT'
      CALL FILL (MAXMOD, 0, MMCMP)
C                                       CONST2 converts FWHM(deg) to
C                                       coefficients for u*u, v*v, u*v
      CONST2 = DG2RAD * (PI / 1.17741022D0) * SQRT (0.5D0)
C                                       Tell kind of operation.
      MSGTXT = 'EVADFT: Begin DFT component subtraction & division'
      CALL MSGWRT (2)
      NNCOR = 1
C                                       Get un-compressed UV increments
      CALL UVINCS (INCS, INCF, INCIF, NRPARM, LREC, JNCS, JNCF, LRPARM,
     *   LLREC)
      LFIELD = 0
      CCOUNT = 0
      IF (IFIELD.LE.0) THEN
         LF1 = 1
         LF2 = MFIELD
      ELSE
         LF1 = IFIELD
         LF2 = IFIELD
         END IF
      WESET = (MODMAX.LE.0) .AND. (.NOT.DOPTMD)
      MCHAN = NCHAN
      IF (WESET) THEN
         MODMAX = LF2 - LF1 + 1
         DO 5 I = 1,MODMAX
            MODFLD(I) = I + LF1 - 1
            MODCHN(I) = CHANEL
            MODNCH(I) = NCHAN
            MODCCV(I) = ABS(CCVER(MODFLD(I)))
            MODCCB(I) = NSUBG(MODFLD(I))
 5          CONTINUE
         FLD1 = LF1
         FLD2 = LF2
         CHN1 = CHANEL
         CHN2 = CHANEL + NCHAN - 1
      ELSE IF (MODMAX.GT.0) THEN
         MCHAN = 0
         FLD1 = 100000
         FLD2 = 0
         CHN1 = FLD1
         CHN2 = 0
         DO 6 I = 1,MODMAX
            MCHAN = MAX (MCHAN, MODNCH(I))
            FLD1 = MIN (FLD1, MODFLD(I))
            FLD2 = MAX (FLD2, MODFLD(I))
            CHN1 = MIN (CHN1, MODCHN(I))
            CHN2 = MAX (CHN2, MODCHN(I)+MODNCH(1)-1)
 6          CONTINUE
         END IF
      IF (MODMAX.GT.0) THEN
         WRITE (MSGTXT,1006) FLD1, FLD2, CHN1, CHN2, MODMAX
         CALL REFRMT (MSGTXT, ' ', I)
         CALL MSGWRT (2)
         END IF
C                                       Decide component type.
C                                       From model passed
      IF (DOPTMD) THEN
         MODTYP = PARMOD(1) + 0.5
         IF (MODTYP.EQ.2) MODTYP = 3
         XNCOMP = 1
         MODMAX = 1
         MODFLD(1) = LF1
         MODCHN(1) = CHANEL
         MODNCH(1) = NCHAN
         MODCCV(1) = 0
         MODCCB(1) = 1
         MMCMP(1) = 1
         WESET = .TRUE.
         IF (DOPTSP) MCHAN = 1
C                                       From CC table, field 1
      ELSE
         LFIELD = LF1
C                                       Get field info. if nec.
         IF (.NOT.NGRDAT) THEN
            CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       If NGRDAT read CLEAN CATBLK.
         IF (NGRDAT) THEN
            ERRTXT = 'READING CLEAN CATBLK'
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD), KLNBLK,
     *         'REST', IBUFF(2049), IRET)
            IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
            END IF
C                                       For point model
         MODTYP = 0
         XNCOMP = 0
C                                       check all CC files
         MFR = 0
         LFR = 0
         DO 10 I = 1,MODMAX
            IF (MODCHN(I).NE.LFR) THEN
               MFR = MFR + 1
               LFR = MODCHN(I)
               END IF
            LFIELD = MODFLD(I)
            JNREC = 1
            JLREC = 0
            NKEY = 0
            ERRTXT = 'OPENING CLEAN COMPS FILE'
            CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *         MODCCV(I), KLNBLK, LUNC, NKEY, JNREC, JLREC, BUFF1,
     *         IBUFF, IRET)
            IF (IRET.GT.1) GO TO 990
            IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
            DONE(I) = F
            XNCOMP = XNCOMP + NCLNG(LFIELD) - MODCCB(I) + 1
            MMCMP(MFR) = MMCMP(MFR) + NCLNG(LFIELD) - MODCCB(I) + 1
C                                       More complex models
C                                       Find columns (physical)
            IF ((JLREC.GT.3) .AND. (NCLNG(LFIELD).GE.MODCCB(I)))
     *         THEN
               NKEY = JLREC
               IF (JLREC.GT.7) NKEY = 7
               ERRTXT = 'FINDING REQUIRED CC COLUMNS'
               CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
               IF (IRET.NE.0) GO TO 990
C                                       Read 1st record
               JCOMP = MODCCB(I)
               CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1090) IRET, JCOMP
                  GO TO 995
                  END IF
C                                       Get model type.
               ITYPE = RECORD(TYPKOL) + 0.5
               IF (LFIELD.EQ.LF1) MODTYP = ITYPE
               IF (MODTYP.NE.ITYPE) THEN
                  IF ((MODTYP.EQ.0) .OR. (ITYPE.EQ.0)) THEN
                     MSGTXT = 'EVADFT: will treat points as extended'
                     CALL MSGWRT(6)
                     MODTYP = MAX (MODTYP, ITYPE)
                  ELSE
                     WRITE (MSGTXT,1000) MODTYP, ITYPE
                     IRET = 8
                     GO TO 995
                     END IF
                  END IF
               END IF
C                                       Close CLEAN components file.
            CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
 10         CONTINUE
         END IF
C                                       Bad model type.
      IF ((MODTYP.NE.0) .AND. (MODTYP.NE.1) .AND. (MODTYP.NE.3)) THEN
         IRET = 8
         WRITE (MSGTXT,1002) MODTYP
         GO TO 995
         END IF
C                                       Check for point model.
      LFIELD = LF1 - 1
C                                       Tell model type once
      IF (.NOT.ONZE) THEN
C                                       Tell model type
         MSGTXT = 'EVADFT: Model components of type '//MDTYP(MODTYP+1)
         CALL MSGWRT (2)
         ONZE = .NOT. ONZE
C                                       Check Buffer size
         IF ((XNCOMP.GT.10) .AND. (JBUFSZ/2.LT.4096)) THEN
            MSGTXT = 'EVADFT: SCRATCH BUFFER TOO SMALL FOR CCs!'
            CALL MSGWRT (8)
            IRET = 9
            GO TO 999
            END IF
         END IF
C                                       Set model length
      LENMOD = LMOD(MODTYP+1)
C                                       Determine size of uv I/O and
C                                       the number of CC that will fit.
      LENBU = ((JBUFSZ-2*NBPS) / 2) / (LLREC*2)
C                                       How much data fits in AP?
      JT = 15 + LENBU * LLREC
      IF (MXCC.GT.JT) JT = MXCC
      NEED = JT + MCHAN + (XNCOMP+2)*LENMOD
      NEED = NEED / 1024
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL QINIT (NEED, 0, KAP)
      MSGSUP = MSGSAV
      IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
         NEED = JT + MCHAN + (XNCOMP/10+2)*LENMOD
         NEED = NEED / 1024
         NEED = MIN (32*1024, NEED) + 2
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
             IRET = 8
             MSGTXT = 'EVADFT CANNOT GET NEEDED MEMORY'
             GO TO 995
             END IF
          END IF
      APSIZ = PSAPNW * 1024
      CALL QRLSE
      MXCMP = (APSIZ - JT - NCHAN)  / LENMOD
      MXCMP = MXCMP - 2
      CALL RFILL (20, 0.0, RECORD)
C                                       Set AP pointers.
C                                       UV=UV pointer, VIS=vis pointer
C                                       IAPCC0=CLEAN components pointer.
      IDATA = 12 + MCHAN
      UV = IDATA + ILOCU
      IAPCC0 = 15 + LENBU * LLREC + NCHAN
      IAPCC0 = MAX (IAPCC0, MXCC)

      LLNMOD = LENMOD
C                                       Compute number of passes.
      NTIMES = (1.0 * XNCOMP) / MXCMP + 0.99999
      NTIMES = MAX (NTIMES, 1)
C                                       Only one pass allowed for
C                                       division.  No can do.
      IF (NTIMES.GT.1) THEN
         ERRTXT = 'TOO MANY COMPONENTS FOR DIVISION'
         IRET = 10
         GO TO 990
         END IF
C                                       Fix for Division scaling
      ABFACG = ABS(FACGRD(1))
C                                       Open uv files.
C                                       Set input file name.
      IF (DISKI.LE.0) THEN
         VOL = SCRVOL(CNOSCI)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSCI), 1, NAME, IRET)
      ELSE
         VOL = DISKI
         CALL ZPHFIL ('UV', VOL, CNOSCI, 1, NAME, IRET)
         END IF
C                                       Open input file.
      ERRTXT = 'OPEN-FOR-READ VIS FILE'
      CALL ZOPEN (LUNI, FINDI, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create scratch file if necessary
      IF ((DISK1.LE.0) .AND. (CNOSC1.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         ERRTXT = 'CREATING SCRATCH 1 FILE'
         CALL SCREAT (ISIZE, BUFF2, IRET)
         CNOSC1 = NSCR
         IF (IRET.GT.0) THEN
            IF (IRET.EQ.1) ERRTXT = 'NO SPACE FOR SCRATCH 1 FILE'
            GO TO 990
            END IF
         END IF
      IF ((DISK2.LE.0) .AND. (CNOSC2.EQ.0)) THEN
         CALL UVSIZE (LREC, NVIS, ISIZE)
         ERRTXT = 'CREATING SCRATCH 2 FILE'
         CALL SCREAT (ISIZE, BUFF2, IRET)
         CNOSC1 = NSCR
         IF (IRET.GT.0) THEN
            IF (IRET.EQ.1) ERRTXT = 'NO SPACE FOR SCRATCH 2 FILE'
            GO TO 990
            END IF
         END IF
C                                       Open vis file for write.
      IF (DISK1.LE.0) THEN
         VOL = SCRVOL(CNOSC1)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSC1), 1, NAME, IRET)
      ELSE
         VOL = DISK1
         CALL ZPHFIL ('UV', VOL, CNOSC1, 1, NAME, IRET)
         END IF
      ERRTXT = 'OPEN-FOR-WRITE VIS 1 FILE'
      CALL ZOPEN (LUN1, FIND1, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
      IF (DISK2.LE.0) THEN
         VOL = SCRVOL(CNOSC2)
         CALL ZPHFIL ('SC', VOL, SCRCNO(CNOSC2), 1, NAME, IRET)
      ELSE
         VOL = DISK2
         CALL ZPHFIL ('UV', VOL, CNOSC2, 1, NAME, IRET)
         END IF
      ERRTXT = 'OPEN-FOR-WRITE VIS 2 FILE'
      CALL ZOPEN (LUN2, FIND2, VOL, NAME, T, F, T, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Loop, subtracting Max Component
C                                       In AP each pass
      I1 = 1
      I2 = I1 - 1
C                                       Setup for % done messages.
      PCTOT = NVIS
      PCLST = 0
      IPCLST = 0
C                                       Set AP loc for next CC load
      IAPCT = IAPCC0
C                                       Determine. no. this pass.
      MMCOMP = 0
      NCOMP = MIN (MXCMP, XNCOMP)
C                                       Grab AP.
      CALL QINIT (NEED, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         IRET = 10
         MSGTXT = 'EVADFT: BIZARRE FAILURE TO GET AP MEMORY'
         GO TO 995
         END IF
C                                       Initialize REAL time clock for
C                                       AP roller.
      CALL ZTIME (ITIME)
      XTLST = (ITIME(1) * 60.00) + ITIME(2) + (ITIME(3) / 60.0)
C                                       If Not single component model.
      IF (.NOT.DOPTMD) THEN
C                                       Find next FIELD.
C                                       Loop back here for next field.
 70      I2 = I2 + 1
C                                       See if done.
         IF (I2.GT.MODMAX) GO TO 150
         IF (DONE(I2)) GO TO 70
         LFIELD = MODFLD(I2)
C                                       See if there are CC's.
         IF ((MODCCB(I2).GT.NCLNG(LFIELD))) GO TO 70
C                                       Get field info. if nec.
         IF (.NOT.NGRDAT) THEN
            CALL GRDAT (F, LFIELD, CATR, IBUFF(2049), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       If NGRDAT read CLEAN CATBLK.
         IF (NGRDAT) THEN
            ERRTXT = 'READING CLEAN CATBLK'
            CALL CATIO ('READ', CCDISK(LFIELD), CCCNO(LFIELD),
     *         KLNBLK, 'REST', IBUFF(2049), IRET)
            IF ((IRET.GT.0) .AND. (IRET.LT.5)) GO TO 990
            END IF
C                                       Set field center offsets.
         XXOFF = DXCG(LFIELD) * CCROT + DYCG(LFIELD) * SSROT
         YYOFF = DYCG(LFIELD) * CCROT - DXCG(LFIELD) * SSROT
         ZZOFF = DZCG(LFIELD)
         CALL XYSHFT (RA, DEC, XSHIFT(LFIELD), YSHIFT(LFIELD),
     *      MAPROT, XRA, XDEC)
         IF (DO3DIM) THEN
            CALL PRJMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *         PMAT)
         ELSE
            CALL P2DMAT (RA, DEC, UVROT, XRA, XDEC, MAPROT, UMAT,
     *         PMAT)
            END IF
C                                       Load CLEAN components into AP.
C                                       Open components file.
         JNREC = 1
         JLREC = 0
         NKEY = 0
         ERRTXT = 'OPENING CLEAN COMPS FILE'
         CALL TABINI ('READ', 'CC', CCDISK(LFIELD), CCCNO(LFIELD),
     *      MODCCV(I2), KLNBLK, LUNC, NKEY, JNREC, JLREC, BUFF1, IBUFF,
     *      IRET)
         IF (IRET.GT.1) GO TO 990
         JCOMP = MODCCB(I2)
C                                       Find columns (physical)
         NKEY = JLREC
         TYPKOL = 7
         IF (JLREC.GT.7) NKEY = 7
         ERRTXT = 'FINDING REQUIRED CC COLUMNS'
         CALL FNDCOL (NKEY, KEYS, 8, F, IBUFF, KOLS, IRET)
         IF (IRET.GT.0) GO TO 990
C                                       Make sure that there are some
         IF (IBUFF(5).LE.0) GO TO 140
         IF (NCLNG(LFIELD).LE.0) NCLNG(LFIELD) = IBUFF(5)
C                                       Loop loading components.
         IAPBUF = 10
         CURCMP = MMCOMP + 1
C                                       Check next component
         IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 140
         DO 130 J = CURCMP,NCOMP,MXCC
            JT = J - 1
            JNCOMP = 0
            MCOMP = NCOMP - J + 1
            IF (MCOMP.GT.MXCC) MCOMP = MXCC
            IF (MCOMP.GT.(NCLNG(LFIELD)-MODCCB(I2)+1))
     *         MCOMP = NCLNG(LFIELD) - MODCCB(I2) + 1
            DO 110 I = 1,MCOMP
C                                       Check if finished field
               IF (JCOMP.GT.NCLNG(LFIELD)) GO TO 120
               CALL TABIO ('READ', 0, JCOMP, RECORD, IBUFF, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1090) IRET, JCOMP
                  GO TO 995
                  END IF
C                                        Check that point comp.
               JCOMP = JCOMP + 1
               JT = JT + 1
               IF (JLREC.LE.3) THEN
                  ITYPE = 0
               ELSE
                  ITYPE = RECORD(TYPKOL) + 0.5
                  END IF
               IF ((ITYPE.NE.MODTYP) .AND. (ITYPE.NE.0)) THEN
                  WRITE (MSGTXT,1070) LFIELD, JCOMP-1
                  CALL MSGWRT (6)
                  END IF
               IF (((ITYPE.EQ.MODTYP) .OR. (ITYPE.EQ.0)) .AND.
     *               (IRET.EQ.0)) THEN
C                                       Check negative component limit
                  DONE(I2) = (NONEG.AND.(RECORD(FLXKOL).LE.0.0))
     *               .OR. (ABS(RECORD(FLXKOL)).LT.LIMFLX)
                  IF (DONE(I2)) THEN
                     NCLNG(LFIELD) = JCOMP - 1
                     GO TO 120
                     END IF
C                                       If req. sum flux
                  IF (DOSUM) THEN
                     FLUXG(LFIELD) = FLUXG(LFIELD) + RECORD(FLXKOL)
                     TFLUXG = TFLUXG + RECORD(FLXKOL)
                     END IF
                  JNCOMP = JNCOMP + 1
                  CCOUNT = CCOUNT + 1
                  XP(1) = (RECORD(RAKOL) + XPOFF(LFIELD)) * CONST
                  XP(2) = (RECORD(DECKOL) + YPOFF(LFIELD)) * CONST
                  XP(3) = 0.0
                  CALL PRJMUL (2, XP, UMAT, XYZ)
                  BUFF1(JNCOMP) = XYZ(1) + XXOFF
                  BUFF1(1024+JNCOMP) = XYZ(2) + YYOFF
                  BUFF1(2048+JNCOMP) = XYZ(3) + ZZOFF
C                                       Handle scaling for division
                  BUFF1(3072+JNCOMP) = ABFACG * RECORD(FLXKOL)
C                                       Gaussian
                  IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
                     IF (ITYPE.EQ.0) THEN
                        BUFF2(JNCOMP) = 0.0
                        BUFF2(1024+JNCOMP) = 0.0
                        BUFF2(2048+JNCOMP) = 0.0
                     ELSE
                        CPA = COS (DG2RAD*RECORD(PAKOL))
                        SPA = SIN (DG2RAD*RECORD(PAKOL))
                        XMAJ = RECORD(MAJKOL) * CONST2
                        XMIN = RECORD(MINKOL) * CONST2
                        BUFF2(JNCOMP) = - (((CPA * XMAJ)**2) +
     *                     (SPA * XMIN)**2)
                        BUFF2(1024+JNCOMP) = - (((SPA * XMAJ)**2)
     *                     + (CPA * XMIN)**2)
                        BUFF2(2048+JNCOMP) = - 2.0 * CPA * SPA *
     *                     (XMAJ*XMAJ - XMIN*XMIN)
                        END IF
C                                       Sphere
                  ELSE IF (MODTYP.EQ.3) THEN
                     BUFF1(3072+JNCOMP) = 3.0 * BUFF1(3072+JNCOMP)
                     IF (ITYPE.EQ.0) THEN
                        BUFF2(JNCOMP) = 0.0
                     ELSE
                        BUFF2(JNCOMP) = RECORD(MAJKOL) * 0.109662271
                        END IF
                     BUFF2(1024+JNCOMP) = 0.1
                     END IF
                  END IF
 110           CONTINUE
C                                       Load components
 120        IF (JNCOMP.GT.0) THEN
               LMCOMP = JNCOMP
               MMCOMP = MMCOMP + LMCOMP
C                                       Load into AP
               IAPBUF = 10
C                                       x component
               CALL QPUT (BUFF1, IAPBUF, LMCOMP, 2)
               IAPTMP = IAPCT + 1
               CALL QWD
               CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
               CALL QWR
C                                       y component
               CALL QPUT (BUFF1(1025), IAPBUF, LMCOMP, 2)
               IAPTMP = IAPCT + 2
               CALL QWD
               CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
               CALL QWR
C                                       z component
               CALL QPUT (BUFF1(2049), IAPBUF, LMCOMP, 2)
               IAPTMP = IAPCT + 3
               CALL QWD
               CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
               CALL QWR
C                                       Flux density
               CALL QPUT (BUFF1(3073), IAPBUF, LMCOMP, 2)
               CALL QWD
               CALL QVMOV (IAPBUF, 1, IAPCT, LLNMOD, LMCOMP)
               CALL QWR
C                                       Gaussian
               IF (MODTYP.EQ.1) THEN
C                                       Coef 1.
                  CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 4
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Coef 2.
                  CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 5
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Coef 3.
                  CALL QPUT (BUFF2(2049), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 6
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
                  END IF
C                                       Sphere
               IF (MODTYP.EQ.3) THEN
C                                       Radius
                  CALL QPUT (BUFF2, IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 4
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
C                                       Minimum argument
                  CALL QPUT (BUFF2(1025), IAPBUF, LMCOMP, 2)
                  IAPTMP = IAPCT + 5
                  CALL QWD
                  CALL QVMOV (IAPBUF, 1, IAPTMP, LLNMOD, LMCOMP)
                  CALL QWR
                  END IF
               IAPCT = IAPCT + (LLNMOD * LMCOMP)
               END IF
C                                       Check if finished field.
            IF ((JCOMP.GT.NCLNG(LFIELD)) .OR. DONE(I2)) GO TO 140
 130        CONTINUE
C                                       Close CLEAN components file.
 140     CALL TABIO ('CLOS', 0, JCOMP, RECORD, IBUFF, IRET)
C                                       Update field sub. count.
         NSUBG(LFIELD) = JCOMP
C                                       Check if need another field.
         IF (JT.LT.NCOMP) GO TO 70
C                                      Check no. comps.
 150     IF (MMCOMP.EQ.0) THEN
            ERRTXT = 'NO POINT COMPONENTS FOUND'
            IRET = 1
            GO TO 990
            END IF
C                                       Load correlator factors
         CALL GETCTL (CATR, FACT2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Else, single component model.
      ELSE
         I2 = 1
         CALL RFILL (LENMOD, 0.0, BUFF1)
         BUFF1(2) = PTRAOF * CONST / 3600.D0
         BUFF1(3) = PTDCOF * CONST / 3600.D0
C                                       single comp uses W term
         XPR = PTRAOF / 206264.81D0
         YPR = PTDCOF / 206264.81D0
         BUFF1(4) = -(SQRT (1.0D0 - XPR*XPR - YPR*YPR) - 1.0D0) *
     *      206264.81D0 * CONST / 3600.D0
C                                       Point
         IF (MODTYP.EQ.0) BUFF1(1) = PTFLX
C                                       Gaussian
         IF (MODTYP.EQ.1) THEN
C                                       Convert to convenient
C                                       coefficients.
            CPA = COS (DG2RAD * PARMOD(4))
            SPA = SIN (DG2RAD * PARMOD(4))
            XMAJ = PARMOD(2) * CONST2 * 2.77777778E-4
            XMIN = PARMOD(3) * CONST2 * 2.77777778E-4
            BUFF1(5) = -(((CPA * XMAJ)**2) + (SPA * XMIN)**2)
            BUFF1(6) = -(((SPA * XMAJ)**2) + (CPA * XMIN)**2)
            BUFF1(7) = -2.0 *  CPA * SPA * (XMAJ*XMAJ - XMIN*XMIN)
            BUFF1(1) = PTFLX
            END IF
C                                       Uniform sphere
         IF (MODTYP.EQ.3) THEN
            BUFF1(5) = PARMOD(2) * 0.109662271 * 2.7777778E-4
            BUFF1(1) = PTFLX * 3.0
            BUFF1(6) = 0.1
            END IF
         PTFL0 = BUFF1(1)
         MMCOMP = 1
         IAPCC = IAPCC0
         CALL QPUT (BUFF1, IAPCC, LLNMOD, 2)
         IAPCT = IAPCC + LLNMOD
C                                       Set Stokes' for point model
         FACT2(1) = 1.0
         FACT2(2) = 1.0
         IF (ICOR0.LT.0) THEN
C                                       RR,LL etc.
C                                       Q?
            IF (KSTOK.EQ.2) THEN
               FACT2(1) = 1.0
               FACT2(2) = 1.0
               VOFF = (3 - ABS (ICOR0)) * INCS
            ELSE IF (KSTOK.EQ.3) THEN
               FACT2(1) = 1.0
               FACT2(2) = -1.0
               VOFF = (3 - ABS (ICOR0)) * INCS
            ELSE IF (KSTOK.EQ.4) THEN
               FACT2(1) = 1.0
               FACT2(2) = -1.0
               VOFF = 0
               END IF
         ELSE
C                                       True Stokes'
            IF ((KSTOK.GE.2) .AND. (KSTOK.LE.4)) THEN
               FACT2(1) = 1.0
               FACT2(2) = 0.0
               VOFF = (KSTOK - ICOR0) * INCS
               END IF
            END IF
         NSTOK = 1
         IF ((CATR(KRCIC+JLOCS).LT.0.0) .AND. (NCOR.GE.2)) NSTOK = 2
         IF ((NSTOK.EQ.2) .AND. (ABS (ICOR0).EQ.2)) NSTOK = 1
C                                       End if not single comp. model
         END IF
C                                       Correct for FACGRD
      NNCOR = NSTOK
      FACT2(1) = FACT2(1) * FACGRD(1)
      FACT2(2) = FACT2(2) * FACGRD(1)
      WRK = MCHAN + 2
      UUU = 1.0
      IF (FACGRD(2).EQ.0.0) UUU = 0.0
      CALL QPUT (UUU, WRK+2, 1, 2)
      CALL QPUT (FACT2, WRK, NNCOR, 2)
      SFLAG = 1
C                                       Set flag for UPOL and RL,LR data
      IF ((KSTOK.EQ.3) .AND. (ICOR0.LT.0)) SFLAG = -1
C                                       Set vis pointer
      VIS = IDATA + LRPARM + (CHANEL-1) * JNCF + (VOFF/INCS)*JNCS
      CALL QWAIT
C                                       Init for read & write
C                                       visibility file
C                                       Init vis file for write
      ERRTXT = 'INIT-FOR-READ VIS FILE'
      CALL UVINIT ('READ', LUNI, FINDI, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFF3, BO, IBIND, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Init vis file for read.
      ERRTXT = 'INIT-FOR-WRITE VIS 1 FILE'
      CALL UVINIT ('WRIT', LUN1, FIND1, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFF1, BO, KBIND1, IRET)
      IF (IRET.NE.0) GO TO 990
      ERRTXT = 'INIT-FOR-WRITE VIS 2 FILE'
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, LENBU, JBUFSZ,
     *   BUFF2, BO, KBIND2, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Subtract model from vis data.
C                                       Loop:  Read vis. record.
 200  CONTINUE
         ERRTXT = 'READING VIS FILE'
         CALL UVDISK ('READ', LUNI, FINDI, BUFF3, INIO2, IBIND, IRET)
         INIO = INIO2
         IF (IRET.NE.0) GO TO 990
C                                       Exit if no more data
         IF (INIO.LE.0) GO TO 300
C                                       Uncompress Vis and put in AP
         CALL BUFPUT (INIO, NRPARM, LREC, LLREC, IDATA, BUFF3(IBIND),
     *      IBUFF(1))
C                                       Loop over frequency groups
C                                       subtraction: normal
         LFR = 0
         NFR = 0
         IAPCC = IAPCC0
         IF (.NOT.DOPTSP) THEN
            DO 220 I = I1,I2
               IF (MODCHN(I).NE.LFR) THEN
                  NFR = NFR + 1
                  LFR = MODCHN(I)
                  IF (NFR.GT.1) IAPCC = IAPCC + MMCMP(NFR-1) * LENMOD
                  MMCOMP = MMCMP(NFR)
                  LCHAN = MODNCH(I)
                  VIS = IDATA + LRPARM + (LFR-1)*JNCF + (VOFF/INCS)*JNCS
C                                       Fill frequency table
C                                       Double array to force accuracy
                  FFRAC = (FREQG(LFR) / FREQ) - 1.0D0
                  FBUFF(1) = FFRAC
                  IF (LCHAN.GT.1) THEN
                     DO 210 J = 2,LCHAN
                        INDEX = LFR + J - 1
                        FFRAC = (FREQG(INDEX) / FREQ) - 1.0D0
                        FBUFF(J) = FFRAC
 210                    CONTINUE
                     END IF
                  CALL QWD
                  CALL QPUT (FBUFF, 2, MCHAN, 2)
                  CALL QWAIT
C                                       Do the arithmetic: subtract
C                                       Point
                  IF (MODTYP.EQ.0) CALL QXXPTS (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
C                                       Gaussian
                  IF (MODTYP.EQ.1) CALL QGASUB (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
C                                       Sphere
                  IF (MODTYP.EQ.3) CALL QSPSUB (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
                  END IF
 220           CONTINUE
         ELSE
            LCHAN = 1
            MMCOMP = 1
            DO 230 LFR = CHANEL,CHANEL+NCHAN-1
               X = FREQG(LFR) / FREQ
               X = LOG10 (X)
               Y = X*PTSPIX(1) + X*X*PTSPIX(2) + X*X*X*PTSPIX(3) +
     *            X*X*X*X*PTSPIX(4)
               BTEMP = PTFL0 * (10.0D0 ** Y)
               CALL QPUT (BTEMP, IAPCC, 1, 2)
               CALL QWD
               FFRAC = (FREQG(LFR) / FREQ) - 1.0D0
               FBUFF(1) = FFRAC
               CALL QPUT (FBUFF, 2, 1, 2)
               CALL QWAIT
               VIS = IDATA + LRPARM + (LFR-1)*JNCF + (VOFF/INCS)*JNCS
C                                       Do the arithmetic: subtract
C                                       Point
               IF (MODTYP.EQ.0) CALL QXXPTS (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
C                                       Gaussian
               IF (MODTYP.EQ.1) CALL QGASUB (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
C                                       Sphere
               IF (MODTYP.EQ.3) CALL QSPSUB (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR, SFLAG)
 230           CONTINUE
            END IF
         CALL QWR
C                                       Get UVs from AP (maybe pack UV)
         CALL BUFGET (INIO, NRPARM, LREC, LLREC, IDATA, BUFF1(KBIND1),
     *      IBUFF(1))
C                                       Write vis record.
         NIOUT = INIO
         ERRTXT = 'WRITING VIS FILE'
         CALL UVDISK ('WRIT', LUN1, FIND1, BUFF1, NIOUT, KBIND1, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Uncompress Vis and put in AP
         CALL BUFPUT (INIO, NRPARM, LREC, LLREC, IDATA, BUFF3(IBIND),
     *      IBUFF(1))
C                                       Loop over frequency groups
C                                       division
         LFR = 0
         NFR = 0
         IAPCC = IAPCC0
         IF (.NOT.DOPTSP) THEN
            DO 260 I = I1,I2
               IF (MODCHN(I).NE.LFR) THEN
                  NFR = NFR + 1
                  LFR = MODCHN(I)
                  IF (NFR.GT.1) IAPCC = IAPCC + MMCMP(NFR-1) * LENMOD
                  MMCOMP = MMCMP(NFR)
                  LCHAN = MODNCH(I)
                  VIS = IDATA + LRPARM + (LFR-1)*JNCF + (VOFF/INCS)*JNCS
C                                       Fill frequency table
C                                       Double array to force accuracy
                  FFRAC = (FREQG(LFR) / FREQ) - 1.0D0
                  FBUFF(1) = FFRAC
                  IF (LCHAN.GT.1) THEN
                     DO 250 J = 2,LCHAN
                        INDEX = LFR + J - 1
                        FFRAC = (FREQG(INDEX) / FREQ) - 1.0D0
                        FBUFF(J) = FFRAC
 250                    CONTINUE
                     END IF
                  CALL QWD
                  CALL QPUT (FBUFF, 2, MCHAN, 2)
                  CALL QWAIT
C                                       Do the arithmetic: divide
C                                       Point
                  IF (MODTYP.EQ.0) CALL QPTDIV (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Gaussian
                  IF (MODTYP.EQ.1) CALL QGADIV (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Sphere
                  IF (MODTYP.EQ.3) CALL QSPDIV (IAPCC, UV, VIS, LLREC,
     *               JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
                  END IF
 260           CONTINUE
         ELSE
            LCHAN = 1
            MMCOMP = 1
            DO 270 LFR = CHANEL,CHANEL+NCHAN-1
               X = FREQG(LFR) / FREQ
               X = LOG10 (X)
               Y = X*PTSPIX(1) + X*X*PTSPIX(2) + X*X*X*PTSPIX(3) +
     *            X*X*X*X*PTSPIX(4)
               BTEMP = PTFL0 * (10.0D0 ** Y)
               CALL QPUT (BTEMP, IAPCC, 1, 2)
               CALL QWD
               FFRAC = (FREQG(LFR) / FREQ) - 1.0D0
               FBUFF(1) = FFRAC
               CALL QPUT (FBUFF, 2, 1, 2)
               CALL QWAIT
               VIS = IDATA + LRPARM + (LFR-1)*JNCF + (VOFF/INCS)*JNCS
C                                       Do the arithmetic: subtract
C                                       Point
               IF (MODTYP.EQ.0) CALL QPTDIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Gaussian
               IF (MODTYP.EQ.1) CALL QGADIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
C                                       Sphere
               IF (MODTYP.EQ.3) CALL QSPDIV (IAPCC, UV, VIS, LLREC,
     *            JNCF, JNCS, MMCOMP, INIO, LCHAN, NNCOR)
 270           CONTINUE
            END IF
         CALL QWR
C                                       Get UVs from AP (maybe pack UV)
         CALL BUFGET (INIO, NRPARM, LREC, LLREC, IDATA, BUFF2(KBIND2),
     *      IBUFF(1))
C                                       Write vis record.
         NIOUT = INIO
         ERRTXT = 'WRITING VIS FILE'
         CALL UVDISK ('WRIT', LUN2, FIND2, BUFF2, NIOUT, KBIND2, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Check if time for % done
C                                       message.
         PCLST = PCLST + NIOUT
         IPCDNE = (100.0 / NTIMES) * (PCLST / PCTOT) + 0.5
         IPCDNE = IPCDNE - MOD (IPCDNE, 10)
C                                       Write % done message.
         IF ((IPCDNE.GT.IPCLST) .AND. (DOMSG)) THEN
            WRITE (MSGTXT,1240) IPCDNE
            IF (IPCDNE.LE.100) CALL MSGWRT (2)
            IPCLST = IPCDNE
            END IF
C                                       Check if time for AP roller
         CALL QROLL (IAPCT, IBUFF, JBUFSZ, IRET)
         IF (IRET.NE.0) GO TO 999
         GO TO 200
C                                       Finish write
 300  CALL QRLSE
      NIOUT = 0
      ERRTXT = 'FLUSHING VIS FILE'
      CALL UVDISK ('FLSH', LUN1, FIND1, BUFF1, NIOUT, KBIND1, IRET)
      IF (IRET.NE.0) GO TO 990
      NIOUT = 0
      CALL UVDISK ('FLSH', LUN2, FIND2, BUFF2, NIOUT, KBIND2, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Update no. comp. left.
      XNCOMP = XNCOMP - MXCMP
C                                       Close files
      CALL ZCLOSE (LUNI, FINDI, IRET)
      CALL ZCLOSE (LUN1, FIND1, IRET)
      CALL ZCLOSE (LUN2, FIND2, IRET)
      IF (WESET) MODMAX = 0
      IRET = 0
      GO TO 999
C                                       Error
 990  WRITE(MSGTXT,1990,ERR=999) IRET, ERRTXT
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVADFT: MODEL TYPES =',I2,I2,' INCOMPATIBLE')
 1002 FORMAT ('EVADFT: ILLEGAL MODEL TYPE =',I3)
 1006 FORMAT ('EVADFT: fields',I5,' -',I5,' chns',I5,' -',I5,' in',I5,
     *   ' CC models')
 1070 FORMAT ('EVADFT WRONG MODTYP AT FIELD, COMP',I5,I9)
 1090 FORMAT ('EVADFT: ERROR',I5,' READING CLEAN COMPS REC',I5)
 1240 FORMAT ('Model computation is ',I5,' percent complete')
 1990 FORMAT ('EVADFT: ERROR',I5,' ',A)
      END
      SUBROUTINE EVAUVS (IRET)
C-----------------------------------------------------------------------
C   EVAUVS reads through the UV data sets created by the model division
C   and subtraction and does various statistics on them.
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'EVAUV.INC'
      INTEGER   K, VOL, CNO, VO, BO, LUN, FIND, LENBU, INIO, BIND, IREC,
     *   IPTR, NCOPY, I, J, NP, HLIM, LVIS, IR, IRM, L, NOUT(2), NNO,
     *   NNT, NTOT(2), LMAX
      CHARACTER WHICH*1, PHNAME*48, UTYPE*2
      LOGICAL   T, F
      REAL      VIS(3,MAXCIF), R, AVREAL(2), AVIMAG(2), AVAMP(2),
     *   AVRRMS(2), AVIRMS(2), AVARMS(2), SUBTR, RMM(10), RM, AOUT(2),
     *   AAO, RMB, RM3, RM3S(2)
      DOUBLE PRECISION AS, ASS, RS, RSS, IS, ISS, WTS, HST(UVHIST),
     *   HSTS(UVHIST), HSTW(UVHIST), ASW, RMS, AV, AA, AAA, WT, A
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DSCD.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (VIS, BUFF3)
      DATA BO, VO, LUN /1, 0, 24/
      DATA T, F /.TRUE., .FALSE./
      DATA RMM / 5.0, 4.5, 4.0, 3.5, 6*3.0/
C-----------------------------------------------------------------------
      RM3S(1) = APARM(1)
      RM3S(2) = APARM(2)
      LMAX = 7
      RSCALE = MIN (CELLSG(1), CELLSG(2))
      IF (RSCALE.EQ.0.0) RSCALE = MIN (ABS(XCELL(1)), ABS(XCELL(2))) /
     *   3600.
      IF (RSCALE.EQ.0.0) RSCALE = 0.1 / 3600.0
      RSCALE = 1.0 / (DG2RAD * RSCALE) / (UVHIST - 2)
      IF (SC1FIL.GT.0) THEN
         VOL = SCRVOL(SC1FIL)
         CNO = SCRCNO(SC1FIL)
         UTYPE = 'SC'
      ELSE
         VOL = FVOL(-SC1FIL)
         CNO = FCNO(-SC1FIL)
         UTYPE = 'UV'
         END IF
      WHICH = '1'
      SUBTR = 0.0
      CALL COPY (256, UVBLK, CATBLK)
      CALL UVPGET (IRET)
      COMPDT = CATBLK(KINAX).EQ.1
      NCOPY = LREC - NRPARM
      IF (COMPDT) THEN
         LVIS = NCOPY
      ELSE
         LVIS = NCOPY / 3
         END IF
      NP = MIN (NCOR, 2)
      HLIM = UVHIST
C                                       sum histograms etc
      IRM = 0
      DO 100 K = 1,2
         REMXN(1,K) = 1.E10
         REMXN(2,K) = -1.E10
         IMMXN(1,K) = 1.E10
         IMMXN(2,K) = -1.E10
C                                       Open input file.
         CALL ZPHFIL (UTYPE, VOL, CNO, 1, PHNAME, IRET)
         CALL ZOPEN (LUN, FIND, VOL, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN SCRATCH FILE', WHICH
            GO TO 990
            END IF
C                                       Setup for init.
         RMS = 1.D5
         AV = 0.0D0
         DO 60 L = 1,LMAX
            LENBU = 0
            CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU,
     *         JBUFSZ, BUFF1, BO, BIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT SCRATCH IO', WHICH
               GO TO 990
               END IF
C                                       zero counters
            RM = RMM(L) * RMS
            RMB = 6.0 * RMS
            NNT = 0
            WTS = 0.0D0
            AA = 0.0D0
            AAA = 0.0D0
            IF (L.EQ.LMAX) THEN
               RM3 = RM3S(K) * RMS
               CALL DFILL (HLIM, 0.0D0, HST)
               CALL DFILL (HLIM, 0.0D0, HSTS)
               CALL DFILL (HLIM, 0.0D0, HSTW)
               RS = 0.0D0
               RSS = 0.0D0
               IS = 0.0D0
               ISS = 0.0D0
               NNO = 0
               AAO = 0
               END IF
C                                       read loop
 20         CALL UVDISK ('READ', LUN, FIND, BUFF1, INIO, BIND, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ SCRATCH DATA', WHICH
               GO TO 990
            ELSE IF (INIO.GT.0) THEN
               IPTR = BIND
               DO 50 IREC = 1,INIO
                  R = SQRT (BUFF1(IPTR+ILOCU)**2 + BUFF1(IPTR+ILOCV)**2)
                  IR = (R / RSCALE) + 2.0
                  IR = MAX (1, MIN (HLIM, IR))
                  IF (COMPDT) THEN
                     CALL ZUVXPN (NCOPY, BUFF1(IPTR+NRPARM),
     *                  BUFF1(IPTR+WTLOC), VIS)
                  ELSE
                     CALL RCOPY (NCOPY, BUFF1(IPTR+NRPARM), VIS)
                     END IF
                  DO 40 J = 1,NP
                     DO 30 I = J,LVIS,NCOR
                        WT = VIS(3,I)
                        IF (WT.GT.0.0) THEN
                           NNT = NNT + 1
                           VIS(1,I) = VIS(1,I) - SUBTR
                           A = SQRT (VIS(1,I)**2 + VIS(2,I)**2)
C                                       accumulate for averages
                           IF (ABS(A-AV).LE.RM) THEN
                              AA = AA + WT * A
                              AAA = AAA + WT * A * A
                              WTS = WTS + WT
                              END IF
C                                       get range values for Re/Im plots
                           IF (L.EQ.LMAX) THEN
                              IF (ABS(A-AV).LE.RM3) THEN
                                 REMXN(1,K) = MIN (REMXN(1,K), VIS(1,I))
                                 REMXN(2,K) = MAX (REMXN(2,K), VIS(1,I))
                                 IMMXN(1,K) = MIN (IMMXN(1,K), VIS(2,I))
                                 IMMXN(2,K) = MAX (IMMXN(2,K), VIS(2,I))
                                 END IF
C                                       accumulate histogram
                              IF (ABS(A-AV).LE.RM) THEN
                                 HST(IR) = HST(IR) + WT * A
                                 HSTS(IR) = HSTS(IR) + WT * A * A
                                 HSTW(IR) = HSTW(IR) + WT
                                 RS = RS + WT * VIS(1,I)
                                 RSS = RSS + WT * VIS(1,I)**2
                                 IS = IS + WT * VIS(2,I)
                                 ISS = ISS + WT * VIS(2,I)**2
C                                       count really bad ones
                              ELSE IF (ABS(A-AV).GT.RMB) THEN
                                 NNO = NNO + 1
                                 AAO = AAO + ABS (A-AV)
                                 END IF
                              END IF
                           END IF
 30                     CONTINUE
 40                  CONTINUE
                  IPTR = IPTR + LREC
 50               CONTINUE
               GO TO 20
               END IF
            IF (WTS.GT.0) THEN
               AV = AA / WTS
               AAA = AAA / WTS - AV * AV
               RMS = SQRT (MAX (0.0D0, AAA))
               END IF
 60         CONTINUE
C                                       close
         CALL ZCLOSE (LUN, FIND, IRET)
C                                       average counters
         AS = 0.0D0
         ASS = 0.0D0
         ASW = 0.0D0
         DO 90 IR = 1,HLIM
            IF (HSTW(IR).GT.0.0) THEN
               AS = AS + HST(IR)
               ASS = ASS + HSTS(IR)
               ASW = ASW + HSTW(IR)
               HST(IR) = HST(IR) / HSTW(IR)
               HSTS(IR) = HSTS(IR) / HSTW(IR) - HST(IR)**2
               HISTR(IR,K) = SQRT (MAX (0.0D0, HSTS(IR)))
               HISTA(IR,K) = HST(IR)
               IRM = MAX (IRM, IR)
            ELSE
               HISTA(IR,K) = 0.0
               HISTR(IR,K) = 0.0
               END IF
 90         CONTINUE
         IF (ASW.GT.0.0) THEN
            AS = AS / ASW
            ASS = ASS / ASW - AS * AS
            ASS = SQRT (MAX (0.0D0, ASS))
            END IF
         IF (WTS.GT.0.0) THEN
            RS = RS / WTS
            RSS = RSS / WTS - RS * RS
            RSS = SQRT (MAX (0.0D0, RSS))
            IS = IS / WTS
            ISS = ISS / WTS - IS * IS
            ISS = SQRT (MAX (0.0D0, ISS))
            END IF
         AVREAL(K) = RS
         AVRRMS(K) = RSS
         AVIMAG(K) = IS
         AVIRMS(K) = ISS
         AVAMP(K) = AS
         AVARMS(K) = ASS
         NOUT(K) = NNO
         AOUT(K) = AAO / MAX (1, NNO)
         NTOT(K) = NNT
C                                       point to second data set
         IF (SC2FIL.GT.0) THEN
            VOL = SCRVOL(SC2FIL)
            CNO = SCRCNO(SC2FIL)
            UTYPE = 'SC'
         ELSE
            VOL = FVOL(-SC2FIL)
            CNO = FCNO(-SC2FIL)
            UTYPE = 'UV'
             END IF
         WHICH = '2'
         SUBTR = 1.0
 100     CONTINUE
C                                       report results
      WRITE (MSGTXT,1100)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1101) 'subtract', AVREAL(1), AVRRMS(1), AVIMAG(1),
     *   AVIRMS(1), AVAMP(1), AVARMS(1)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1101) 'divide-1', AVREAL(2), AVRRMS(2), AVIMAG(2),
     *   AVIRMS(2), AVAMP(2), AVARMS(2)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1110)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1111) 'subtract', NOUT(1), NTOT(1), AOUT(1)
      CALL MSGWRT (5)
      WRITE (MSGTXT,1111) 'divide-1', NOUT(2), NTOT(2), AOUT(2)
      CALL MSGWRT (5)
      RPARM(1) = AVREAL(1)
      RPARM(2) = AVRRMS(1)
      RPARM(3) = AVIMAG(1)
      RPARM(4) = AVIRMS(1)
      RPARM(5) = AVAMP(1)
      RPARM(6) = AVARMS(1)
      RPARM(7) = FLOAT (NOUT(1)) / FLOAT (NTOT(1))
      RPARM(8) = AOUT(1)
      RPARM(9) = AVREAL(2)
      RPARM(10) = AVRRMS(2)
      RPARM(11) = AVIMAG(2)
      RPARM(12) = AVIRMS(2)
      RPARM(13) = AVAMP(2)
      RPARM(14) = AVARMS(2)
      RPARM(15) = FLOAT (NOUT(2)) / FLOAT (NTOT(2))
      RPARM(16) = AOUT(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVAUVS: ERROR',I4,' ON ',A,1X,A)
 1100 FORMAT ('method',4X,'real part',10X,'imaginary part',5X,
     *   'amplitude')
 1101 FORMAT (A8,3(F8.4,' +-',F7.3,1X))
 1110 FORMAT ('method',3X,'# bad samples',2X,'total samples',2X,
     *   'avg bad amp')
 1111 FORMAT (A8,I14,I15,F13.4)
      END
      SUBROUTINE EVAIMG (NGAUSS, MAHIST, PXRANG, MSCALE, MUNITS, HISTM,
     *   MFIT, RPARM, BUFF, IRET)
C-----------------------------------------------------------------------
C    Uses the CCCNO, CCDISK to report statistics on the model images
C    Inputs:
C       MAHIST   I        Size of histogram
C       NGAUSS   I        Number of Gaussians in MFIELD
C       PXRANG   R(2)     User specified range for 1st histogram
C    Outputs:
C       MSCALE   R(2,2)   Max, min of total, rms-sized histograms
C       MUNITS   C*8      Image units
C       HISTM    I(*,2)   Total range histogram, raange near avg
C       MFIT     R(2)     Robust average and rms
C       BUFF     R(*)     scratch buffer for image IO
C       RPARM    R(30)    returned adverb array
C       IRET     I        Error
C-----------------------------------------------------------------------
      INTEGER   NGAUSS, MAHIST, HISTM(MAHIST,2), IRET
      REAL      PXRANG(2), MSCALE(2,2), MFIT(2), BUFF(*), RPARM(30)
      CHARACTER MUNITS*8
C
      INTEGER   CATCLN(256), IFIELD, NFIELD, SCRTCH(256), NX, NY,
     *   NWORDS, MLUN, MIND, NSUM
      LONGINT   MPOFF
      REAL      CATCR(256), MP(2)
      DOUBLE PRECISION AV, RMS, RSUM, RSUMS, ASUM, ASUMS
      CHARACTER STAT*4, PHNAME*48
      LOGICAL   FIRST
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATCLN, CATCR)
      DATA MLUN /29/
C-----------------------------------------------------------------------
      NFIELD = MFIELD / MAX (1, NGAUSS)
      RSUM = 0.0
      RSUMS = 0.0
      ASUM = 0.0
      ASUMS = 0.0
      NSUM = 0
      CELLSG(1) = 1.E6
      CELLSG(2) = 1.E6
      FIRST = .TRUE.
      CALL FILL (2*MAHIST, 0, HISTM)
      DO 100 IFIELD = 1,NFIELD
         STAT = 'REST'
         CALL CATIO ('READ', CCDISK(IFIELD), CCCNO(IFIELD), CATCLN,
     *      STAT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IFIELD, IRET, 'READ CATBLK'
            GO TO 980
            END IF
         IF (CATCR(KRCIC).NE.0.0) CELLSG(1) = MIN (CELLSG(1),
     *      ABS(CATCR(KRCIC)))
         IF (CATCR(KRCIC+1).NE.0.0) CELLSG(2) = MIN (CELLSG(2),
     *      ABS(CATCR(KRCIC+1)))
         NX = CATCLN(KINAX)
         NY = CATCLN(KINAX+1)
         IF (IFIELD.EQ.1) CALL H2CHR (8, 1, CATCR(KHBUN), MUNITS)
C                                       get memory
         NWORDS = (NX * NY - 1) / 1024 + 3
         CALL ZMEMRY ('GET ', 'EVAIMG', NWORDS, MP, MPOFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IFIELD, IRET, 'GETTING MEMORY'
            GO TO 980
            END IF
C                                       opn map file
         CALL ZPHFIL ('MA', CCDISK(IFIELD), CCCNO(IFIELD), 1, PHNAME,
     *      IRET)
         CALL ZOPEN (MLUN, MIND, CCDISK(IFIELD), PHNAME, .TRUE., .TRUE.,
     *      .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IFIELD, IRET, 'OPENING MA FILE'
            GO TO 980
            END IF
         CALL EVARMS (MLUN, MIND, NX, NY, MP(1+MPOFF), AV, RMS, BUFF,
     *      IRET)
         IF (IRET.GT.0) GO TO 999
         IF (IRET.EQ.0) THEN
            ASUM = ASUM + AV
            ASUMS = ASUMS + AV * AV
            RSUM = RSUM + RMS
            RSUMS = RSUMS + RMS * RMS
            NSUM = NSUM + 1
            IF ((FIRST) .AND. (RMS.GT.0.0)) THEN
               IF (PXRANG(2).GT.PXRANG(1)) THEN
                  MSCALE(1,1) = PXRANG(1)
                  MSCALE(2,1) = PXRANG(2)
               ELSE
                  MSCALE(1,1) = CATCR(KRDMN)
                  MSCALE(2,1) = CATCR(KRDMX)
                  END IF
               MSCALE(1,2) = AV - 5.*RMS
               MSCALE(2,2) = AV + 5.*RMS
               FIRST = .FALSE.
               END IF
            CALL EVAHST (NX, NY, MP(1+MPOFF), MAHIST, MSCALE, HISTM)
            END IF
         CALL ZCLOSE (MLUN, MIND, IRET)
         CALL ZMEMRY ('FREE', 'EVAIMG', NWORDS, MP, MPOFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IFIELD, IRET, 'GETTING MEMORY'
            GO TO 980
            END IF
 100     CONTINUE
C                                       average the averages/rms's
      IF (NSUM.GT.0) THEN
         ASUM = ASUM / NSUM
         ASUMS = ASUMS / NSUM - ASUM * ASUM
         ASUMS = SQRT (MAX (0.0D0, ASUMS))
         RSUM = RSUM / NSUM
         RSUMS = RSUMS / NSUM - RSUM * RSUM
         RSUMS = SQRT (MAX (0.0D0, RSUMS))
         IF (NFIELD.GT.1) THEN
            WRITE (MSGTXT,1100) ASUM, ASUMS, NFIELD
            CALL MSGWRT (5)
            WRITE (MSGTXT,1101) RSUM, RSUMS, NFIELD
            CALL MSGWRT (5)
            RPARM(18) = ASUMS
            RPARM(20) = RSUMS
         ELSE
            WRITE (MSGTXT,1110) ASUM, RSUM
            CALL MSGWRT (5)
            END IF
         MFIT(1) = ASUM
         MFIT(2) = RSUM
         RPARM(17) = ASUM
         RPARM(19) = RSUM
      ELSE
         MSGTXT = 'EVAIMG DID NOT FIND ANY IMAGE RMS TO REPORT'
         CALL MSGWRT (8)
         MFIT(1) = 0.0
         MFIT(2) = 0.0
         END IF
      IRET = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVAIMG FIELD',I5,' ERROR',I3,' ON ',A)
 1100 FORMAT ('Average mean',1PE12.4,' +-',1PE11.4,' over',I5,' images')
 1101 FORMAT ('Average rms ',1PE12.4,' +-',1PE11.4,' over',I5,' images')
 1110 FORMAT ('Image mean',1PE12.4,'  rms',1PE11.4)
      END
      SUBROUTINE EVARMS (MLUN, MIND, NX, NY, MP, AV, RMS, BUFF, IRET)
C-----------------------------------------------------------------------
C   EVARMS does the robust rms computation on an image
C   Inputs:
C      MLUN   I      LUN of open image file
C      MIND   I      FTAB pointer for the file
C      NX     I      Number columns in image
C      NY     I      Number rows in image
C   Outputs:
C      MP     R(*)   Image-sized work area
C      AV     R      Robust mean of image
C      RMS    R      Robust average of image
C      BUFF   R(*)   IO buffer
C      IRET   I      Error code: -1 => no points found
C-----------------------------------------------------------------------
      INTEGER   MLUN, MIND, NX, NY, IRET
      REAL      MP(NX,NY), BUFF(*)
      DOUBLE PRECISION AV, RMS
C
      INTEGER   IX, IY, WIN(4), NBY, IBLKOF, L, BPOS, NSUM
      REAL      RM(7)
      DOUBLE PRECISION SUM, SUMS, RMM, V
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA RM /6., 5., 4., 4*3./
C-----------------------------------------------------------------------
C                                       init IO
      NBY = 2 * UVBFSS
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
      IBLKOF = 1
      CALL MINIT ('READ', MLUN, MIND, NX, NY, WIN, BUFF, NBY, IBLKOF,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT IMAGE IO'
         GO TO 990
         END IF
C                                       read in image
      DO 10 IY = 1,NY
         CALL MDISK ('READ', MLUN, MIND, BUFF, BPOS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ IMAGE ROWS'
            GO TO 990
            END IF
         CALL RCOPY (NX, BUFF(BPOS), MP(1,IY))
 10      CONTINUE
C                                       robust average
      RMS = 1.D6
      AV = 0
      DO 50 L = 1,7
         SUM = 0.0D0
         SUMS = 0.0D0
         NSUM = 0
         RMM = RM(L) * RMS
         DO 30 IY = 1,NY
            DO 20 IX = 1,NX
               IF (MP(IX,IY).NE.FBLANK) THEN
                  V = MP(IX,IY)
                  IF (ABS(V-AV).LE.RMM) THEN
                     SUM = SUM + V
                     SUMS = SUMS + V * V
                     NSUM = NSUM + 1
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
         IF (NSUM.GT.0.0D0) THEN
            SUM = SUM / NSUM
            SUMS = SUMS / NSUM - SUM * SUM
            SUMS = SQRT (MAX (0.0D0, SUMS))
            AV = SUM
            RMS = SUMS
         ELSE
            IRET = -1
            GO TO 999
            END IF
 50      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVARMS: ERROR',I4,' ON ',A)
      END
      SUBROUTINE EVAHST (NX, NY, MP, MAHIST, MSCALE, HISTM)
C-----------------------------------------------------------------------
C   EVAHST takes the image in ram and adds it to the histograms
C   Inputs:
C      NX       I        Number columns in image
C      NY       I        Number rows in image
C      MP       R(*,*)   Image data
C      MAHIST   I        Size of histogram + edge counters
C      MSCALE   R(2,2)   min/max for 2 histograms
C   In/out:
C      HISTM    I(*,2)   histogram
C-----------------------------------------------------------------------
      INTEGER   NX, NY, MAHIST, HISTM(MAHIST,2)
      REAL      MP(NX,NY), MSCALE(2,2)
C
      INTEGER   IX, IY, I
      REAL      DELTA1, DELTA2
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (MSCALE(2,1).GT.MSCALE(1,1)) THEN
         DELTA1 = (MSCALE(2,1) - MSCALE(1,1)) / (MAHIST-2)
      ELSE
         DELTA1 = 1.0
         END IF
      IF (MSCALE(2,2).GT.MSCALE(1,2)) THEN
         DELTA2 = (MSCALE(2,2) - MSCALE(1,2)) / (MAHIST-2)
      ELSE
         DELTA2 = 1.0
         END IF
      DO 20 IY = 1,NY
         DO 10 IX = 1,NX
            IF (MP(IX,IY).NE.FBLANK) THEN
               I = (MP(IX,IY) - MSCALE(1,1)) / DELTA1
               IF (MP(IX,IY).EQ.MSCALE(2,1)) I = MIN (I, MAHIST-3)
               I = MIN (MAHIST, MAX (1, I+2))
               HISTM(I,1) = HISTM(I,1) + 1
               I = (MP(IX,IY) - MSCALE(1,2)) / DELTA2
               IF (MP(IX,IY).EQ.MSCALE(2,2)) I = MIN (I, MAHIST-3)
               I = MIN (MAHIST, MAX (1, I+2))
               HISTM(I,2) = HISTM(I,2) + 1
               END IF
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EVAPLT (NP, MAP, SC1MAP, SC2MAP, IRET)
C-----------------------------------------------------------------------
C   EVAPLT plots the histograms
C   Input:
C      NP     I      Size of work space
C   Output:
C      MAP    D(*)   work space
C      IRET   I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, IRET
      REAL      SC1MAP(NP,*), SC2MAP(NP,*)
      DOUBLE PRECISION MAP(NP,*)
C
      INCLUDE 'EVAUV.INC'
      INTEGER   K, IM, IVER, GRCHN, TVCHN, TVCORN(2), PLUN, PIND, LABEL,
     *   IROUND, I, VOL, CNO, LUN, FIND, LENBU, VO, BO, BIND, INIO,
     *   IPTR, IREC, NCOPY, LVIS, NPP, WTLOC, J, IX, IY, NSM, XPRM
      REAL      YMIN, YMAX, XMIN, XMAX, MHIST(MAHIST), BLOG, TEMP, DX,
     *   DY, VIS(3,MAXCIF), X, Y, WT, SUBTR
      LOGICAL   DOTV, DOLOG, T, F, COMPDT
      DOUBLE PRECISION PARMS(3), NGOOD, NBAD
      CHARACTER PFILE*48, UUNITS*25, WHICH*1, PHNAME*48, UTYPE*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DGDS.INC'
      EQUIVALENCE (VIS, BUFF3)
      DATA T, F /.TRUE., .FALSE./
      DATA LUN /24/
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
      IF (DOHIST.LE.0.0) GO TO 999
      NSM = APARM(4) + 0.1
      DOTV = XDOTV.GT.0.0
      CALL FNDEXT ('PL', UVBLK, IVER)
      TVCHN = 1
      TVCORN(1) = 0
      TVCORN(2) = 0
      GRCHN = XGRCH + 0.5
      LABEL = IROUND (XLABEL)
      IF (MOD(LABEL,100).EQ.0) LABEL = (LABEL/100)*100 + 3
      DOLOG = .FALSE.
      PARMS(1) = 0.0D0
      XPRM = NPARM + 10
      IF (DOHIST.GE.1.5) THEN
         UUNITS = 'Baseline length (lambdas)'
         DO 20 K = 1,2
            IM = 0
            YMIN = 1.E10
            YMAX = -1.E10
            DO 10 I = 1,UVHIST
               IF (HISTA(I,K).GT.0.0) THEN
                  IM = I
                  YMIN = MIN (YMIN, HISTA(I,K)-HISTR(I,K))
                  YMAX = MAX (YMAX, HISTA(I,K)+HISTR(I,K))
                  END IF
 10            CONTINUE
            IF (IM.LT.4) THEN
               WRITE (MSGTXT,1010) IM, K
               CALL MSGWRT (6)
               GO TO 20
               END IF
            IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
            IVER = IVER + 1
            IF (.NOT.DOTV) THEN
               CALL MADDEX ('PL', DISKIN, OLDCNO, INCATB, BUFF2, .TRUE.,
     *            'UPDT', IVER, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                  CALL MSGWRT (7)
                  END IF
               END IF
            IM = IM + 1
            HISTA(IM,K) = HISTA(UVHIST,K)
            XMIN = 0.0
            XMAX = IM * RSCALE
            XTRPRM(1) = K
            XTRPRM(2) = XMIN
            XTRPRM(3) = XMAX
            XTRPRM(4) = YMIN
            XTRPRM(5) = YMAX
            CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
            CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 39, XPRM, XNAMEI,
     *         DOTV, TVCHN, GRCHN, TVCORN, INCATB, BUFF1, PLUN, PIND,
     *         IRET)
            IF (IRET.NE.0) GO TO 20
            CALL HISTOG (1+K, IM, HISTA(1,K), HISTR(1,K), XMIN, XMAX,
     *         YMIN, YMAX, DOLOG, LABEL, IVER, PARMS, UUNITS, IBUFF1,
     *         IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
               CALL MSGWRT (7)
               END IF
            GPHPAG = .TRUE.
            CALL GFINIS (BUFF1, IRET)
            IF (IRET.LT.0) GO TO 999
C                                       Successful plot file finished.
            IF (IRET.EQ.0) THEN
               IF (.NOT.DOTV) THEN
                  CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF2, IRET)
                  WRITE (MSGTXT,1015) IVER
                  CALL MSGWRT (5)
                  IRET = 0
                  END IF
               END IF
 20         CONTINUE
C                                       plot image histograms
         IF (.NOT.DOPTMD) THEN
            DOLOG = FUNCTY.EQ.'LG'
            BLOG = ALOG10 (0.5)
            PARMS(1) = 1.0D0
            PARMS(2) = MFIT(1)
            PARMS(3) = MFIT(2)
            DO 40 K = 1,2
               YMIN = 1.E10
               YMAX = -1.E10
               MHIST(1) = HISTM(1,K)
               MHIST(MAHIST) = HISTM(MAHIST,K)
               DO 30 I = 2,MAHIST-1
                  IF (DOLOG) THEN
                     IF (HISTM(I,K).GT.0) THEN
                        MHIST(I) = ALOG10 (HISTM(I,K)+0.0)
                     ELSE
                        MHIST(I) = BLOG
                        END IF
                  ELSE
                     MHIST(I) = HISTM(I,K)
                     END IF
                  YMIN = MIN (YMIN, MHIST(I))
                  YMAX = MAX (YMAX, MHIST(I))
 30               CONTINUE
               IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
               IVER = IVER + 1
                IF (.NOT.DOTV) THEN
                  CALL MADDEX ('PL', DISKIN, OLDCNO, INCATB, BUFF2,
     *               .TRUE., 'UPDT', IVER, IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'ERROR ADDING PLOT TO HEADER'
                     CALL MSGWRT (7)
                     END IF
                  END IF
               XMIN = MSCALE(1,K)
               XMAX = MSCALE(2,K)
               XTRPRM(1) = K + 2.0
               XTRPRM(2) = XMIN
               XTRPRM(3) = XMAX
                XTRPRM(4) = YMIN
               XTRPRM(5) = YMAX
               CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
               CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 39, XPRM, XNAMEI,
     *            DOTV, TVCHN, GRCHN, TVCORN, INCATB, BUFF1, PLUN, PIND,
     *            IRET)
               IF (IRET.NE.0) GO TO 40
               CALL HISTOG (1, MAHIST, MHIST, MHIST, XMIN, XMAX, YMIN,
     *            YMAX, DOLOG, LABEL, IVER, PARMS, MUNITS, IBUFF1, IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'ERROR. WILL TRY TO FINISH PARTIAL GRAPH.'
                  CALL MSGWRT (7)
                  END IF
               GPHPAG = .TRUE.
               CALL GFINIS (BUFF1, IRET)
               IF (IRET.LT.0) GO TO 999
C                                       Successful plot file finished.
               IF (IRET.EQ.0) THEN
                  IF (.NOT.DOTV) THEN
                     CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF2, IRET)
                     WRITE (MSGTXT,1015) IVER
                     CALL MSGWRT (5)
                     IRET = 0
                     END IF
                  END IF
 40            CONTINUE
            END IF
         END IF
C                                       plot Real vs Imag
      IF (SC1FIL.GT.0) THEN
         VOL = SCRVOL(SC1FIL)
         CNO = SCRCNO(SC1FIL)
         UTYPE = 'SC'
      ELSE
         VOL = FVOL(-SC1FIL)
         CNO = FCNO(-SC1FIL)
         UTYPE = 'UV'
         END IF
      WHICH = '1'
      CALL COPY (256, UVBLK, CATBLK)
      COMPDT = CATBLK(KINAX).EQ.1
      CALL UVPGET (IRET)
      NCOPY = LREC - NRPARM
      IF (COMPDT) THEN
         LVIS = NCOPY
      ELSE
         LVIS = NCOPY / 3
         END IF
      NPP = MIN (NCOR, 2)
      IF (COMPDT) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *      WTLOC, IRET)
         IF ((IRET.NE.0) .OR. (WTLOC.LT.0)) THEN
            IRET = 5
            MSGTXT = 'CANNOT FIND WEIGHT AND SCALE FOR COMPRESSED DATA'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         IRET = 0
         END IF
      SUBTR = 0.0
      DO 200 K = 1,2
         I = NP * NP
         CALL DFILL (I, 0.0D0, MAP)
         YMIN = IMMXN(1,K)
         YMAX = IMMXN(2,K)
         IF ((YMIN.GT.0.0) .AND. (YMIN.LT.0.33*YMAX)) YMIN = 0.0
         TEMP = YMAX - YMIN
         IF (ABS(YMAX+YMIN).LT.0.05*TEMP) THEN
            TEMP = MAX (YMAX, -YMIN)
            YMAX = TEMP
            YMIN = -TEMP
            TEMP = YMAX - YMIN
            END IF
         YMAX = YMAX + 0.08 * TEMP
         YMIN = YMIN - 0.08 * TEMP
         XMIN = REMXN(1,K)
         XMAX = REMXN(2,K)
         IF ((XMIN.GT.0.0) .AND. (XMIN.LT.0.33*XMAX)) XMIN = 0.0
         TEMP = XMAX - XMIN
         DX = TEMP / 400.0 * 1000.0
         DY = TEMP / 400.0 * 1000.0
         IF (ABS(XMAX+XMIN).LT.0.05*TEMP) THEN
            TEMP = MAX (XMAX, -XMIN)
            XMAX = TEMP
            XMIN = -TEMP
            TEMP = XMAX - XMIN
            END IF
         XMAX = XMAX + 0.08 * TEMP
         XMIN = XMIN - 0.08 * TEMP
         IF ((ABS(XMAX-YMAX).LT.0.05*TEMP) .AND.
     *      (ABS(XMIN-YMIN).LT.0.05*TEMP) .AND.
     *       (ABS(XMAX+XMIN).LT.0.05*TEMP)) THEN
            XMAX = MAX (XMAX, YMAX)
            XMAX = MAX (XMAX, -XMIN)
            XMAX = MAX (XMAX, -YMIN)
            YMAX = XMAX
            XMIN = -XMAX
            YMIN = -XMAX
            END IF
         IVER = IVER + 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, OLDCNO, INCATB, BUFF2, .TRUE.,
     *         'UPDT', IVER, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR ADDING PLOT TO HEADER'
               CALL MSGWRT (7)
               END IF
            END IF
         XTRPRM(1) = K + 4.0
         XTRPRM(2) = XMIN
         XTRPRM(3) = XMAX
         XTRPRM(4) = YMIN
         XTRPRM(5) = YMAX
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, IVER, PFILE, IRET)
         CALL GINIT (DISKIN, OLDCNO, PFILE, 0, 39, XPRM, XNAMEI, DOTV,
     *      TVCHN, GRCHN, TVCORN, INCATB, BUFF1, PLUN, PIND, IRET)
         IF (IRET.NE.0) GO TO 200
         CALL REIMPL (K, XMIN, XMAX, YMIN, YMAX, LABEL, IVER, BUFF1,
     *      IRET)
         IF (IRET.NE.0) GO TO 190
C                                       Open input file.
         CALL ZPHFIL (UTYPE, VOL, CNO, 1, PHNAME, IRET)
         CALL ZOPEN (LUN, FIND, VOL, PHNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN SCRATCH FILE', WHICH
            GO TO 990
            END IF
         LENBU = 0
         CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC, LENBU,
     *      JBUFSZ, BUFF2, BO, BIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT SCRATCH IO', WHICH
            GO TO 990
            END IF
         NGOOD = 0.0D0
         NBAD = 0.0D0
 100     CALL UVDISK ('READ', LUN, FIND, BUFF2, INIO, BIND, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ SCRATCH DATA', WHICH
            GO TO 990
         ELSE IF (INIO.GT.0) THEN
            IPTR = BIND
            DO 130 IREC = 1,INIO
               IF (COMPDT) THEN
                  CALL ZUVXPN (NCOPY, BUFF2(IPTR+NRPARM),
     *               BUFF2(IPTR+WTLOC), VIS)
               ELSE
                  CALL RCOPY (NCOPY, BUFF2(IPTR+NRPARM), VIS)
                  END IF
               DO 120 J = 1,NPP
                  DO 110 I = J,LVIS,NCOR
                     WT = VIS(3,I)
                     IF (WT.GT.0.0) THEN
                        X = VIS(1,I)  - SUBTR
                        Y = VIS(2,I)
                        IF ((X.LE.XMAX) .AND. (X.GE.XMIN) .AND.
     *                     (Y.LE.YMAX) .AND. (Y.GE.YMIN)) THEN
                           NGOOD = NGOOD + 1.0D0
                           IX = (X - XMIN) / (XMAX - XMIN) * (NP - 1.) +
     *                        1.5
                           IY = (Y - YMIN) / (YMAX - YMIN) * (NP - 1.) +
     *                        1.5
                           MAP(IX,IY) = MAP(IX,IY) + 1.0D0
                        ELSE
                           NBAD = NBAD + 1
                           END IF
                        END IF
 110                 CONTINUE
 120              CONTINUE
               IPTR = IPTR + LREC
 130           CONTINUE
            GO TO 100
            END IF
C                                       draw contours
 190     IF (NGOOD.GT.0.0D0) CALL IMGDRW (NP, MAP, NSM, SC1MAP, SC2MAP,
     *      XMIN, XMAX, YMIN, YMAX, IBUFF1, IRET)
         WRITE (MSGTXT,1190) NGOOD, NBAD, WHICH
         CALL MSGWRT (4)
         WRITE (MSGTXT,1191) NP, NSM
         CALL MSGWRT (4)
         GPHPAG = K.EQ.1
         CALL GFINIS (BUFF1, IRET)
         IF (IRET.LT.0) GO TO 999
C                                       Successful plot file finished.
         IF (IRET.EQ.0) THEN
            IF (.NOT.DOTV) THEN
               CALL HIPLOT (DISKIN, OLDCNO, IVER, BUFF2, IRET)
               WRITE (MSGTXT,1015) IVER
               CALL MSGWRT (5)
               IRET = 0
               END IF
            END IF
         IF (SC2FIL.GT.0) THEN
            VOL = SCRVOL(SC2FIL)
            CNO = SCRCNO(SC2FIL)
            UTYPE = 'SC'
         ELSE
            VOL = FVOL(-SC2FIL)
            CNO = FCNO(-SC2FIL)
            UTYPE = 'UV'
            END IF
         WHICH = '2'
         CALL ZCLOSE (LUN, FIND, IRET)
         SUBTR = 1.0
 200     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' ON ',A,' ',A)
 1010 FORMAT ('ONLY',I2,' BOXES SAMPLED IN UV HISTOGRAM',I2)
 1015 FORMAT ('Successful histogram plot file version',I7,' created')
 1190 FORMAT ('Plotted',F11.0,' omitted',F10.0,' vis of RE/IM plot ',A)
 1191 FORMAT ('Used image of',I5,' pixels on a side, smoothed by',I3,
     *   ' pixels')
      END
      SUBROUTINE HISTOG (ITY, NBOXES, HIST, HRMS, XMIN, XMAX, YMIN,
     *   YMAX, DOLOG, LABEL, IVER, PARMS, UNITS, BUFF1, IRET)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a histogram.
C   Inputs:
C      ITY      I      Plot type: 1 Flux on X axis, count on Y axis
C                        2 Radius on X axis, Avg flux +- rms on Y
C                        3 Radius on X axis, Avg gain - (1,0) +- rms Y
C      NBOXES   I      number of boxes for histogram.
C      HIST     R(*)   Histogram (underflow in (1) and overflow in
C                        (NBOXES))
C      XMIN     R      Min value in X
C      XMAX     R      Max value in X
C      YMIN     R      Min value in Y
C      XMAX     R      Max value in Y
C      DOLOG    L      T => use log(n) rather than linear scale
C      LABEL    I      Type of labeling
C      IVER     I      Plot file version number
C      PARMS    D(3)   Fit parameters (0) = 0 => none
C      UNITS    C*8    Units to use
C   In/out:
C      BUFF1    I(*)   I/O buffer for open, initialized pl file.
C   Output:
C      IRET     I      error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      INTEGER   ITY, NBOXES, LABEL, IVER, BUFF1(*), IRET
      REAL      XMIN, XMAX, YMIN, YMAX, HIST(*), HRMS(*)
      DOUBLE PRECISION PARMS(3)
      LOGICAL   DOLOG
      CHARACTER UNITS*(*)
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, XYRATO, LOCRAN(2),
     *   EXF, BFACT, AX(5), AY(5), RANGE(2), W, SF, SW, DCX, DCY, XDIST
      INTEGER   IDEPTH(5), I, LTYPE, JM
      CHARACTER TXTMSG*80
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set character offsets.
      LOCRAN(2) = YMAX
      LOCRAN(1) = YMIN
      CALL GTICNT (LABEL, LOCRAN, I)
      RANGE(1) = XMIN
      RANGE(2) = XMAX
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = CH(2) + 3 * 1.333
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF (LTYPE.GT.2) CH(4) = CH(4) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = -1.0
      TRC(1) = NBOXES + 1.0
      BLC(2) = YMIN - 0.05 * (YMAX - YMIN)
      TRC(2) = YMAX + 0.05 * (YMAX - YMIN)
      XYRATO = (TRC(1) - BLC(1)) / (TRC(2) - BLC(2))
C                                       Kludge to keep XYRATO in bounds
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      IF (XYRATO.GT.3.0) THEN
         DO 10 I = 1,10000
            IF (XYRATO.LT.2.0) GO TO 20
            FAC = FAC / 2.
            XYRATO = XYRATO / 2.
 10      CONTINUE
      ELSE IF (XYRATO.LT.0.333) THEN
         DO 15 I = 1,10000
            IF (XYRATO.GT.0.50) GO TO 20
            FAC = FAC * 2.
            XYRATO = XYRATO * 2.
 15         CONTINUE
         END IF
C
 20   TRC(2) = TRC(2) / FAC
      BLC(2) = BLC(2) / FAC
      XYRATO = 1.0 / XYRATO
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (1, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Labeling.
      I = NBOXES - 2
      CALL HLABX (ITY, BLC, TRC, FAC, I, RANGE, DOLOG, IVER, HIST(1),
     *   HIST(NBOXES), LABEL, UNITS, BUFF1, XDIST, IRET)
      IF (IRET.NE.0) GO TO 999
      TXTMSG = 'End labeling, draw histogram'
      CALL GCOMNT (-1, TXTMSG, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (2, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Type 1 plot
      IF (ITY.EQ.1) THEN
C                                       Position at first data point.
         X = 1
         Y = BLC(2)
         CALL GPOS (X, Y, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Loop for rest of data points.
         SW = 0.0
         JM = 0
         DO 30 I = 2,NBOXES-1
            IF (HIST(I).GT.SW) THEN
               SW = HIST(I)
               JM = I
               END IF
            Y = HIST(I) / FAC
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            X = I
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            Y = BLC(2)
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
 30         CONTINUE
C                                       Gaussian fit
         IF (PARMS(1).GT.0.0) THEN
            TXTMSG = 'Draw gaussian fit'
            CALL GCOMNT (2, TXTMSG, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GLTYPE (4, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            JM = JM - 2
            IF (JM.GE.0) THEN
               IF (JM.LT.NBOXES/3) THEN
                  WRITE (TXTMSG,1030) PARMS(2) * XDIST
                  X = TRC(1)
                  Y = TRC(2)
                  CALL GPOS (X, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  DCX = -29.0
                  DCY = -3.0
                  CALL GCHAR (12, 0, DCX, DCY, TXTMSG, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WRITE (TXTMSG,1031) PARMS(3) * XDIST
                  X = TRC(1)
                  Y = TRC(2)
                  CALL GPOS (X, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  DCX = -14.0
                  CALL GCHAR (11, 0, DCX, DCY, TXTMSG, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
               ELSE
                  WRITE (TXTMSG,1030) PARMS(2) * XDIST
                  X = BLC(1)
                  Y = TRC(2)
                  CALL GPOS (X, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  DCX = 3.0
                  DCY = -3.0
                  CALL GCHAR (12, 0, DCX, DCY, TXTMSG, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  WRITE (TXTMSG,1031) PARMS(3) * XDIST
                  X = TRC(1)
                  Y = TRC(2)
                  CALL GPOS (X, Y, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  DCX = -14.0
                  DCY = -3.0
                  CALL GCHAR (11, 0, DCX, DCY, TXTMSG, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
C                                       solve for peak
            SF = 0.0
            SW = 0.0
            IF (DOLOG) BFACT = 10.0 ** YMAX
            DO 35 I = 2,NBOXES-1
               X = RANGE(1) + (I * (RANGE(2)-RANGE(1))) / NBOXES
               X = (X - PARMS(2)) / PARMS(3)
               X = X * X / 2.0
               IF (X.LE.39.) THEN
                  W = EXP(-X)
                  IF ((DOLOG) .AND. (HIST(I).GE.0.0)) THEN
                     SF = SF + (10.0**HIST(I)) / BFACT
                     SW = SW + W
                  ELSE IF (.NOT.DOLOG) THEN
                     SF = SF + HIST(I) / YMAX
                     SW = SW + W
                     END IF
                  END IF
 35            CONTINUE
            IF (SW.GT.0.0) THEN
               IF (DOLOG) THEN
                  W = SF / SW * BFACT
                  W = LOG10 (W)
               ELSE
                  W = SF / SW * YMAX
                  END IF
            ELSE
               W = YMAX
               END IF
            EXF = NBOXES / 1000.0
            BFACT = W
            IF (DOLOG) BFACT = 10.0 ** W
            DO 40 I = 1,1001
               X = RANGE(1) + (I-1.0) * (RANGE(2)-RANGE(1)) / 1000.0
               X = (X - PARMS(2)) / PARMS(3)
               X = X * X / 2.0
               IF (X.LE.69.) THEN
                  Y = BFACT * EXP (-X)
               ELSE
                  Y = 0.
                  END IF
               IF (DOLOG) Y = ALOG10 (MAX (0.5, Y))
               X = (I - 1) * EXF
               Y = Y / FAC
               IF (I.EQ.1) THEN
                  CALL GPOS (X, Y, BUFF1, IRET)
               ELSE
                  CALL GVEC (X, Y, BUFF1, IRET)
                  END IF
               IF (IRET.NE.0) GO TO 999
 40            CONTINUE
            END IF
C                                       type 2 plot
      ELSE
         DO 50 I = 2,NBOXES-1
            X = I - 1
            Y = (HIST(I) - HRMS(I)) / FAC
            CALL GPOS (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            Y = (HIST(I) + HRMS(I)) / FAC
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            X = I
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            Y = (HIST(I) - HRMS(I)) / FAC
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            X = I - 1
            CALL GVEC (X, Y, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
 50         CONTINUE
         CALL GLTYPE (4, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         DO 60 I = 2,NBOXES-1
            AX(1) = I - 0.5
            AY(1) = HIST(I) / FAC
            AX(2) = AX(1)
            AX(3) = AX(1)
            AX(4) = AX(1) - 0.4
            AX(5) = AX(1) + 0.4
            AY(2) = AY(1) + 0.4
            AY(3) = AY(1) - 0.4
            AY(4) = AY(1)
            AY(5) = AY(1)
            CALL PNTPLT (3, AX, AY, BLC, TRC, .FALSE., .FALSE., BUFF1,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
 60         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('MEAN',F8.4)
 1031 FORMAT ('RMS',F8.4)
      END
      SUBROUTINE HLABX (ITY, BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER,
     *   UNDER, OVER, LABEL, UNITS, BUFF1, XDIST, IRET)
C-----------------------------------------------------------------------
C   Write labeling for histogram.
C   Inputs:
C      ITY     I        Type: 1 image, 2 vis, 3 gain
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C      UNITS   C*8      units
C   In/out:
C      BUFF1   I(256)   I/O buffer for plot file.
C   Output:
C      XDIST   R        Scaling of X axis
C      IRET    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC, RANGE(2), UNDER, OVER, XDIST
      INTEGER   ITY, NBOXES, IVER, LABEL, BUFF1(256), IRET
      LOGICAL   DOLOG
      CHARACTER UNITS*(*)
C
      CHARACTER PREFIX*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      REAL      XINTER(21), DCX, DCY, XNOINT, DIST, ODIST, XMAX, TICSCL,
     *   YTICEL, YTICER, XVAL, YPOS, TICLEN, XINT, X, FREQ, DCXM
      INTEGER   INOINT, INCHAR, I, IXO, M, ITRY, NXFR, NXST, NAX, INC,
     *   IANGL, JSTOK, IT(3), ID(3), ICPNT, ITMP
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TICSCL /70.0/
      DATA CSTOK /'????','Beam','Ipol','Qpol','Upol','Vpol','Ppol',
     *   'Fpol','Pang','Spix','Optd','    '/
      DATA XINTER /.1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200.,
     *   500., 1000., 2000., 5000., 10000., 20000., 50000., 100000.,
     *   200000., 500000./
C-----------------------------------------------------------------------
      IF (MOD(ABS(LABEL),100).LE.1) GO TO 999
C                                       Tic positions.
      TICLEN = (TRC(1) - BLC(1)) / TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(2) - BLC(2))
      XINT = 8.0
      DO 20 I = 1,21
         XNOINT = AINT (DIST/XINTER(I))
         IF (XNOINT.LE.XINT) GO TO 30
 20      CONTINUE
      GO TO 110
C                                       Interval and no of inter found.
 30   XINT = XINTER(I)
      INOINT = XNOINT + 2.5
      XVAL = AINT (FAC*BLC(2)/XINT) * XINT
      IF (XVAL.GT.FAC*BLC(2)) XVAL = XVAL - XINT
      IXO = I
      DCXM = -0.5
C                                       Loop for all tics.
      DO 100 I = 1,INOINT
         XVAL = XVAL + XINT
         YPOS = XVAL / FAC
         IF (YPOS.GT.TRC(2)) GO TO 110
C                                       TOP tic.
         CALL GPOS (TRC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (YTICER, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Left hand tic.
         CALL GPOS (YTICEL, YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write value.
         IF (MOD (ABS(LABEL),100).GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.3) INCHAR = INCHAR - 2
            DCX = - INCHAR - 1.0
            DCY = -0.5
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Number of pixels
 110  DCX = DCXM - 2.0
      YPOS = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), YPOS, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ITY.EQ.1) THEN
         IF (DOLOG) THEN
            MSGBUF = 'Log10 (number of pixels)'
            INCHAR = 24
         ELSE
            MSGBUF = 'Number of pixels'
            INCHAR = 16
            END IF
      ELSE IF (ITY.EQ.2) THEN
         MSGBUF = 'Flux residual (Jy)'
         INCHAR = 18
      ELSE
         MSGBUF = 'Gain - (1,0)'
         INCHAR = 12
         END IF
      DCY = INCHAR / 2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write bucket numbers on top
      IF (NBOXES.LE.8) THEN
         M = 1
      ELSE IF (NBOXES.LE.16) THEN
         M = 2
      ELSE IF (NBOXES.LE.40) THEN
         M = 5
      ELSE IF (NBOXES.LE.80) THEN
         M = 10
      ELSE IF (NBOXES.LE.160) THEN
         M = 20
      ELSE IF (NBOXES.LE.400) THEN
         M = 50
      ELSE IF (NBOXES.LE.800) THEN
         M = 100
      ELSE IF (NBOXES.LE.1600) THEN
         M = 200
      END IF
      TICLEN = (TRC(2) - BLC(2)) / TICSCL
      YTICEL = BLC(2) + TICLEN
      YTICER = TRC(2) - TICLEN
      DCY = 0.5
      DO 150 I = 0,NBOXES,M
         X = I + 0.5
         CALL GPOS (X, YTICER, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MOD (ABS(LABEL),100).GT.2) THEN
            WRITE (MSGBUF,1115) I
            CALL CHTRIM (MSGBUF, 4, MSGBUF, INCHAR)
            DCX = 0.5 - REAL(INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 150     CONTINUE
C                                       Label RHS bucket #
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = -5.0
      DCY = 0.5
      IF (MOD(ABS(LABEL),100).GT.2) DCY = DCY + 1.333
      MSGBUF = 'Box number'
      CALL GCHAR (10, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Range =
      IF (MOD(ABS(LABEL),100).LT.7) THEN
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         DCX = 0.0
         DCY = -2.833
         IF (MOD(ABS(LABEL),100).GT.2) DCY = DCY - 1.333
         WRITE (MSGBUF,1151) RANGE(1), RANGE(2), UNITS
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Interval =
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         X = (RANGE(2) - RANGE(1)) / NBOXES
         WRITE (MSGBUF,1152) X, UNITS
         DCY = DCY - 1.333
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
C                                       Underflow = overflow =
         CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         I = UNDER + 0.1
         M = OVER + 0.1
         WRITE (MSGBUF,1154) I, M
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY - 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         END IF
C                                       Determine label range
      DIST = RANGE(2) - RANGE(1)
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX, PFLAG)
      IF (PFLAG) GO TO 190
      XDIST = DIST / ODIST
      ODIST = XDIST * RANGE(1)
C                                       Get interval
      DO 160 ITRY = 1,21
         XNOINT = AINT (DIST/XINTER(ITRY))
         IF (XNOINT.LE.8.0) GO TO 170
 160     CONTINUE
      GO TO 190
C                                       Bottom (value) tics
 170  XINT = XINTER(ITRY)
      DCY = -1.5
      XMAX = MAX (ABS(RANGE(2)), ABS(RANGE(1))) * XDIST
      INOINT = XNOINT + 2.5
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GE.ODIST) XVAL = XVAL - XINT
      DO 175 I = 1,INOINT
         XVAL = XVAL + XINT
         X = ((XVAL-ODIST)/DIST) * NBOXES + 1.0
         IF (X.GT.TRC(1)) GO TO 180
         CALL GPOS (X, YTICEL, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MOD(ABS(LABEL),100).GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (ITRY.GT.3) INCHAR = INCHAR - 2
            DCX = 0.5 - INCHAR
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 175     CONTINUE
C                                       Label with prefix
 180  DCY = -1.5
      IF (MOD(ABS(LABEL),100).GT.2) DCY = -2.833
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGBUF,1175) PREFIX, UNITS
      CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
      DCX = 0.5 - INCHAR / 2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (MOD(ABS(LABEL),100).GE.7) GO TO 999
C                                       which axis is which?
 190  NXFR = 0
      NXST = 0
      NAX = CATBLK(KIDIM)
      INC = 2
      DO 200 I = 1,NAX
         ICPNT = KHCTP+(I-1)*INC
         CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
         IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
         IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 200     CONTINUE
C                                       Source name, stokes, freq.
      CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = 0.0
      DCY = 1.833
      IF (MOD(ABS(LABEL),100).GT.2) DCY = DCY + 1.333
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK),
     *   FREQ
      IF (NXFR.LE.2) WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '  _'
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.1)
 1115 FORMAT (I4)
 1151 FORMAT ('Range =',1PE12.4,' to',1PE12.4,1X,A)
 1152 FORMAT ('Interval =',1PE12.4,1X,A)
 1154 FORMAT ('Underflow =',I10,' _Overflow =',I10)
 1175 FORMAT (A5,1X,A)
 1200 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE REIMPL (K, XMIN, XMAX, YMIN, YMAX, LABEL, IVER, BUFF1,
     *   IRET)
C-----------------------------------------------------------------------
C   Does plot scale and other initialization, labels and returns
C   Inputs:
C      K       I      1 - subtraction, 2 - division
C      XMIN    R      X minimum
C      XMAX    R      X maximum
C      YMIN    R      Y minimum
C      YMAX    R      Y maximum
C      LABEL   I      Labeling type code
C      IVER    I      Plot file version number
C   In/Out:
C      BUFF1   R(*)   Plot buffer
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   K, LABEL, IVER, IRET
      REAL      XMIN, XMAX, YMIN, YMAX, BUFF1(*)
C
      REAL      BLC(7), CH(4), TRC(7), X, FAC, XYRATO, LOCRAN(2), FREQ,
     *   RANGE(2), DIST, ODIST, DCX, DCY, XINTER(21), XNOINT, TICLEN,
     *   YTICEL, YTICER, XINT, XVAL, YPOS, DCXM
      INTEGER   IDEPTH(5), I, LTYPE, INOINT, IXO, INCHAR, NXFR, NXST,
     *   NAX, INC, ICPNT, IANGL, JSTOK, IT(3), ID(3), ITRY, ITMP
      CHARACTER PREFIX*5, TIME*8, DATE*12, CTEMP*8, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA XINTER /.1, .2, .5, 1., 2., 5., 10., 20., 50., 100., 200.,
     *   500., 1000., 2000., 5000., 10000., 20000., 50000., 100000.,
     *   200000., 500000./
C-----------------------------------------------------------------------
C                                       Scaling
      LOCRAN(2) = YMAX
      LOCRAN(1) = YMIN
      CALL GTICNT (LABEL, LOCRAN, I)
      RANGE(1) = XMIN
      RANGE(2) = XMAX
      DIST = MAX (LOCRAN(2)-LOCRAN(1), RANGE(2)-RANGE(1))
      ODIST = DIST
      CALL METSCL (LABEL, DIST, PREFIX, PFLAG)
      FAC = DIST / ODIST
C                                       number characters around
      CALL RFILL (4, 0.5, CH)
      LTYPE = MOD (ABS (LABEL), 100)
      IF (LTYPE.EQ.2) CH(1) = 2.5
      IF (LTYPE.GT.2) CH(1) = I + 4.0
      IF (LTYPE.GT.1) CH(2) = 2.0
      IF (LTYPE.GT.2) CH(2) = CH(2) + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(2) = CH(2) + 1.333
      IF (LTYPE.EQ.2) CH(3) = 2.5
      IF (LTYPE.GT.1) CH(4) = CH(4) + 1.5
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CH(4) = CH(4) + 1.333
C                                       Set BLC, TRC, XYRATO.
      CALL RFILL (5, 1.0, BLC(3))
      CALL RFILL (5, 1.0, TRC(3))
      CALL FILL (5, 1, IDEPTH)
      BLC(1) = XMIN * 1000.0
      TRC(1) = XMAX * 1000.0
      BLC(2) = YMIN * 1000.0
      TRC(2) = YMAX * 1000.0
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (1, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       tick marks: Y axis
      IF (LTYPE.GT.1) THEN
         IF (PFLAG) GO TO 75
         TICLEN = (TRC(1) - BLC(1)) / 70.0
         YTICEL = BLC(1) + TICLEN
         YTICER = TRC(1) - TICLEN
C                                       Find interval value.
         DIST = FAC * (LOCRAN(2) - LOCRAN(1))
         XINT = 9.0
         DO 20 I = 1,21
            XNOINT = AINT (DIST/XINTER(I))
            IF (XNOINT.LE.XINT) GO TO 30
 20         CONTINUE
         I = 0
C                                       Interval and no of inter found.
 30      IF (I.GT.0) THEN
            XINT = XINTER(I) * 1000.0
            INOINT = XNOINT + 2.5
            XVAL = AINT (FAC*BLC(2)/XINT) * XINT
            IF (XVAL.GT.FAC*BLC(2)) XVAL = XVAL - XINT
            IXO = I
            DCXM = -0.5
C                                       Loop for all tics.
            DO 40 I = 1,INOINT
               XVAL = XVAL + XINT
               YPOS = XVAL / FAC
               IF (YPOS.LE.TRC(2)) THEN
C                                       TOP tic.
                  CALL GPOS (TRC(1), YPOS, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GVEC (YTICER, YPOS, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Left hand tic.
                  CALL GPOS (YTICEL, YPOS, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GVEC (BLC(1), YPOS, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
C                                       Write value.
                  IF (MOD(ABS(LABEL),100).GT.2) THEN
                     WRITE (MSGBUF,1030) XVAL/1000.0
                     CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
                     IF (IXO.GT.3) INCHAR = INCHAR - 2
                     DCX = - INCHAR - 1.0
                     DCY = -0.5
                     DCXM = MIN (DCXM, DCX)
                     CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1,
     *                  IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
                  END IF
 40            CONTINUE
            END IF
C                                       X axis
         YTICEL = BLC(2) + TICLEN
         YTICER = TRC(2) - TICLEN
C                                       Determine label range
         DIST = FAC * (RANGE(2) - RANGE(1))
         ODIST = FAC * RANGE(1)
C                                       Get interval
         DO 50 ITRY = 1,21
            XNOINT = AINT (DIST/XINTER(ITRY))
            IF (XNOINT.LE.9.0) GO TO 60
 50         CONTINUE
         ITRY = 0
C                                       Bottom (value) tics
 60      IF (ITRY.GT.0) THEN
            XINT = XINTER(ITRY) * 1000.0
            DCY = -1.5
            INOINT = XNOINT + 2.5
            XVAL = AINT (FAC*BLC(1)/XINT) * XINT
            IF (XVAL.GE.FAC*BLC(1)) XVAL = XVAL - XINT
            DO 70 I = 1,INOINT
               XVAL = XVAL + XINT
               X = XVAL / FAC
               IF (X.LE.TRC(1)) THEN
                  CALL GPOS (X, YTICER, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GVEC (X, TRC(2), BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GPOS (X, YTICEL, BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL GVEC (X, BLC(2), BUFF1, IRET)
                  IF (IRET.NE.0) GO TO 999
                  IF (MOD(ABS(LABEL),100).GT.2) THEN
                     WRITE (MSGBUF,1030) XVAL/1000.0
                     CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
                     IF (ITRY.GT.3) INCHAR = INCHAR - 2
                     DCX = 0.5 - INCHAR
                     CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1,
     *                  IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
                  END IF
 70            CONTINUE
            END IF
C                                       Y axis label
 75      DCX = DCXM - 2.0
         YPOS = (TRC(2) + BLC(2)) / 2.0
         CALL GPOS (BLC(1), YPOS, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (K.EQ.1) THEN
            MSGBUF = 'Imaginary part in ' // PREFIX // ' Jy'
         ELSE
            MSGBUF = 'Imaginary part in ' // PREFIX // ' gain'
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = INCHAR / 2.0 - 1.0
         CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       X axis label
         DCY = -1.5
         IF (MOD(ABS(LABEL),100).GT.2) DCY = -2.833
         X = (TRC(1) + BLC(1)) / 2.0
         CALL GPOS (X, BLC(2), BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (K.EQ.1) THEN
            MSGBUF = 'Real part in ' // PREFIX // ' Jy'
         ELSE
            MSGBUF = 'Real part in ' // PREFIX // ' gain - 1'
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCX = 0.5 - INCHAR / 2.0
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MOD(ABS(LABEL),100).LT.7) THEN
            NXFR = 0
            NXST = 0
            NAX = CATBLK(KIDIM)
            INC = 2
            DO 80 I = 1,NAX
               ICPNT = KHCTP+(I-1)*INC
               CALL H2CHR (8, 1, CATH(ICPNT), CTEMP)
               IF (CTEMP(1:4).EQ.'FREQ') NXFR  = I
               IF (CTEMP(1:4).EQ.'STOK') NXST  = I
 80            CONTINUE
C                                       Source name, stokes, freq.
            CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            DCX = 0.0
            DCY = 0.5
            IANGL = 0
            CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
            FREQ = 0.0
            JSTOK = 12
            IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) +
     *         CATR(KRCIC+NXFR-1) * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
            FREQ = FREQ / 1.E6
            IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + 2.5 +
     *         CATR(KRCIC+NXST-1)* (BLC(NXST) - CATR(KRCRP+NXST-1))
            IF (NXFR.GT.2) WRITE (MSGBUF,1080) CTEMP, CSTOK(JSTOK),
     *         FREQ
            IF (NXFR.LE.2) WRITE (MSGBUF,1080) CTEMP, CSTOK(JSTOK)
            CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
            INCHAR = INCHAR + 1
            IF (INCHAR.GT.1) THEN
               MSGBUF(INCHAR:INCHAR+2) = '  _'
               INCHAR = INCHAR + 3
               END IF
            CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
            CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
            CALL REFRMT (MSGBUF, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
C                                       time/date, version
            IF (LABEL.GT.0) THEN
               CALL GPOS (BLC(1), TRC(2), BUFF1, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL ZDATE (ID)
               CALL ZTIME (IT)
               CALL TIMDAT (IT, ID, TIME, DATE)
               WRITE (MSGBUF,1085) IVER, DATE, TIME
               CALL REFRMT (MSGBUF, '_', INCHAR)
               DCY = DCY + 1.333
               CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, BUFF1, IRET)
               END IF
            END IF
         END IF
      CALL GLTYPE (4, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (K.EQ.1) THEN
         IF ((XMIN.LT.0.) .AND. (XMAX.GT.0.0)) THEN
            CALL GPOS (0.0, BLC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GVEC (0.0, TRC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
      ELSE
         IF ((XMIN.LT.0.0) .AND. (XMAX.GT.0.0)) THEN
            CALL GPOS (0.0, BLC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            CALL GVEC (0.0, TRC(2), BUFF1, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         END IF
      IF ((YMIN.LT.0.0) .AND. (YMAX.GT.0.0)) THEN
         CALL GPOS (BLC(1), 0.0, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (TRC(1), 0.0, BUFF1, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      CALL GLTYPE (2, BUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GPOS (BLC(1), BLC(2), BUFF1, IRET)
      CALL GVEC (BLC(1), BLC(2), BUFF1, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.1)
 1080 FORMAT (A,'  _',A4,'_ ',F10.3,' MHz')
 1085 FORMAT ('Plot file version',I4,'__created ',A,A)
      END
      SUBROUTINE IMGDRW (NP, MAP, NSM, S1M, S2M, XMIN, XMAX, YMIN, YMAX,
     *   IGBLK, IERR)
C-----------------------------------------------------------------------
C   CONDRW will write commands to a plot file for the execution of
C   a contour plot.
C   Inputs:
C      NP       I         Number points on a side
C      MAP      D(*)      Image
C      NSM      I         Size of smoothing kernel
C      IGBLK    I(256)    I/O block for graph file.
C   Common:
C      CATBLK   I(256)    map header.
C      CNTRBU   R(8192)   buffers
C   Output:
C      IERR     I   error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   NP, NSM, IGBLK(256), IERR
      DOUBLE PRECISION MAP(NP,*)
      REAL      XMIN, XMAX, YMIN, YMAX, S1M(NP,*), S2M(NP,*)
C
      REAL      VAL(3), XPOS(3), YPOS(3), TEMP, VC, VL, VM, VS, XA, XB,
     *   XL, XLAST, XM, XS, YA, YB, YL, YLAST, YM, YS, DELTAX, DELTAY,
     *   TLEV, XLEV, ALEVS(30), XP, YP, XSCALE, YSCALE
      INTEGER   IPERM(3,6), IBLCX, IBLCY, IBLCY1, ICOL, II, INDEX,
     *   INLEVS, INPIXS, IPLUS, IPOS, IROW, ISKIP, ITRCX, ITRCXM,
     *   ITRCY, ITRI, I, MININT, LOCINT, IROUND, ISLEV, JJ, LASTC
      DOUBLE PRECISION DMAX
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DCNT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IPERM /1,3,2, 3,2,1, 3,1,2, 2,1,3, 1,2,3, 2,3,1/
C-----------------------------------------------------------------------
C                                       smooth input
      CALL IMGSMT (NSM, NP, MAP, S1M, S2M)
      XSCALE = (XMAX - XMIN) / (NP -1.0)
      YSCALE = (YMAX - YMIN) / (NP -1.0)
      ISKIP = 2
      XLAST = -1000.
      YLAST = -1000.
      TEMP = NP - 1
      TEMP = 10 - 3 * LOG10 (TEMP)
      LOCINT = IROUND (TEMP)
      IF (LOCINT.LT.2) LOCINT = 2
      IBLCY = 1
      ITRCY = NP
      ITRCX = NP
      IBLCX = 1
      INPIXS = ITRCX - IBLCX + 1
      LASTC = 0
C                                       magic parms for dashed lines
      XLEV = 256.0 / INPIXS
      TLEV = 256.0 / (ITRCY - IBLCY + 1.0)
      ISLEV = SQRT (1.0 / (XLEV * TLEV)) + 0.1
      IF (ISLEV.LT.1) ISLEV = 1
      IF (XLEV.LT.1.0) XLEV = (SQRT (XLEV) + 3.0*XLEV) / 4.0
C                                       Determine number of levels.
C                                       and convert to absolute levels.
      DMAX = 0.0D0
      DO 20 IROW = 1,NP
         DO 10 I = 1,NP
            DMAX = MAX (DMAX, MAP(I,IROW))
 10         CONTINUE
 20      CONTINUE
      INLEVS = 2.0D0 * LOG10 (DMAX)
      DO 45 I = 1,30
         ALEVS(I) = I/2.0
 45      CONTINUE
      WRITE (MSGTXT,1045) DMAX, ALEVS(INLEVS)
      CALL MSGWRT (3)
C                                       Read and save first row.
      DO 55 I = IBLCX,ITRCX
         IF (MAP(I,IBLCY).GT.0.0D0) THEN
            RLROW(I-IBLCX+1) = LOG10 (MAP(I,IBLCY))
         ELSE
            RLROW(I-IBLCX+1) = -0.3
            END IF
 55      CONTINUE
C                                       Loop over all rows.
      IBLCY1 = IBLCY + 1
      DO 300 IROW = IBLCY1,ITRCY
C                                       Read proper row.
         DO 110 I = IBLCX,ITRCX
            IF (MAP(I,IROW).GT.0.0D0) THEN
               BUFF(I) = LOG10 (MAP(I,IROW))
            ELSE
               BUFF(I) = -0.3
               END IF
 110        CONTINUE
         IPOS = IBLCX
C                                       Loop over all pixels in row.
         IPLUS = 0
         ITRCXM = ITRCX - 1
         DO 250 ICOL = IBLCX,ITRCXM
            IPLUS = IPLUS + 1
C                                       Init values
            VAL(1) = BUFF(IPOS+IPLUS-1)
            VAL(2) = BUFF(IPOS+IPLUS)
            VAL(3) = RLROW(IPLUS)
C                                       Init positions.
            XPOS(1) = ICOL
            XPOS(2) = ICOL + 1
            XPOS(3) = ICOL
            YPOS(1) = IROW
            YPOS(2) = IROW
            YPOS(3) = IROW - 1
C                                       Loop for both triangles.
            DO 200 ITRI = 1,2
C                                       Changes for 2nd triangle.
               IF (ITRI.EQ.2) THEN
                  VAL(1) = RLROW(IPLUS+1)
                  XPOS(1) = ICOL + 1
                  YPOS(1) = IROW - 1
                  END IF
C                                       Order points in triangle.
               DO 130 II = 1,3
                  IF (VAL(II).EQ.FBLANK) GO TO 200
 130              CONTINUE
               INDEX = 0
               IF (VAL(1).GT.VAL(2)) INDEX = 1
               IF (VAL(3).GE.VAL(1)) INDEX = INDEX + 2
               IF (VAL(2).GE.VAL(3)) INDEX = INDEX + 4
C                                       find large, med, small
C                                       values and X,Y positions.
               II = IPERM(1,INDEX)
               VL = VAL(II)
               XL = XPOS(II)
               YL = YPOS(II)
C
               II = IPERM(2,INDEX)
               VM = VAL(II)
               XM = XPOS(II)
               YM = YPOS(II)
C
               II = IPERM(3,INDEX)
               VS = VAL(II)
               XS = XPOS(II)
               YS = YPOS(II)
C                                       Loop for all levels.
               DO 190 II = 1,INLEVS
                  VC = ALEVS(II)
C                                       Cut down negatives
                  IF (VC.GE.0.0) GO TO 140
                     IF ((XLEV.LT.2.85) .AND. (ITRI.EQ.2)) GO TO 190
                     IF (XLEV.GE.1.0) GO TO 140
                        JJ = IROW + ICOL + II
                        IF (MOD(JJ, ISLEV).NE.0) GO TO 190
 140              IF ((VC.GT.VL) .OR. ((VL-VS).LE.0.0)) GO TO 200
C                                       If level not right, next lev.
                  IF (VC.LE.VS) GO TO 190
C                                       Interpolate btwn max two corns.
                  TEMP = (VC-VS) / (VL-VS)
                  XA = TEMP * (XL-XS) + XS
                  YA = TEMP * (YL-YS) + YS
C                                       See which corners 2nd pt. btwn.
                  IF (VC.GT.VM) GO TO 150
                  IF (VM.EQ.VS) GO TO 150
C                                       Level btwn med & small corners.
                     TEMP = (VC-VS) / (VM-VS)
                     XB = TEMP * (XM-XS) + XS
                     YB = TEMP * (YM-YS) + YS
                     GO TO 160
C                                       Level btwn large & med corners.
 150                 TEMP = (VC-VM) / (VL-VM)
                     XB = TEMP * (XL-XM) + XM
                     YB = TEMP * (YL-YM) + YM
C                                       Issue position & write commands
C                                       We can avoid position command
C                                       if we switch A and B.
 160              IF ((XLAST.EQ.XB) .AND. (YLAST.EQ.YB)) THEN
                     TEMP = XA
                     XA = XB
                     XB = TEMP
                     TEMP = YA
                     YA = YB
                     YB = TEMP
C                                       See if we need to position.
                  ELSE IF ((XLAST.NE.XA) .OR. (YLAST.NE.YA)) THEN
                     XP = (XMIN + (XA - 1.) * XSCALE) * 1000.0
                     YP = (YMIN + (YA - 1.) * YSCALE) * 1000.0
                     CALL GPOS (XP, YP, IGBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     END IF
C                                       Draw vector.
                  IF (VC.GE.0.0) THEN
                     XP = (XMIN + (XB - 1.) * XSCALE) * 1000.0
                     YP = (YMIN + (YB - 1.) * YSCALE) * 1000.0
                     CALL GVEC (XP, YP, IGBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     XLAST = XB
                     YLAST = YB
                     GO TO 190
C                                       Negative contours broken
                  ELSE
                     TEMP = LOCINT * SQRT (((XB-XA)**2 + (YB-YA)**2)
     *                  / 2.0)
                     MININT = IROUND (TEMP)
                     IF (MININT.LT.2) MININT = 2
                     DELTAX = (XB - XA) / MININT
                     DELTAY = (YB - YA) / MININT
                     DO 185 I = 1,MININT,2
                        XB = XA + DELTAX
                        YB = YA + DELTAY
                        XP = (XMIN + (XB - 1.) * XSCALE) * 1000.0
                        YP = (YMIN + (YB - 1.) * YSCALE) * 1000.0
                        CALL GVEC (XP, YP, IGBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        IF (I.LT.MININT-1) THEN
                           XA = XB + DELTAX
                           YA = YB + DELTAY
                           XP = (XMIN + (XA - 1.) * XSCALE) * 1000.0
                           YP = (YMIN + (YA - 1.) * YSCALE) * 1000.0
                           CALL GPOS (XP, YP, IGBLK, IERR)
                           IF (IERR.NE.0) GO TO 999
                           END IF
 185                    CONTINUE
                     XLAST = XB
                     YLAST = YB
                     END IF
 190              CONTINUE
 200           CONTINUE
 250        CONTINUE
         CALL RCOPY (INPIXS, BUFF(IPOS), RLROW)
 300     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1045 FORMAT ('Gridded image max=',1PE12.4,' counts; peak contour',
     *   0PF4.1,' in log10')
      END
      SUBROUTINE IMGSMT (NSM, NP, MAP, SUM, WGT)
C-----------------------------------------------------------------------
C   Inputs:
C      NP    I      size of image
C   In/out:
C      MAP   D(*)   image: in unsmoothed, out smoothed
C   Output:
C      SUM   R(*)   Sum
C      WGT   R(*)   Weight sum
C-----------------------------------------------------------------------
      INTEGER   NP, NSM
      DOUBLE PRECISION MAP(NP,*)
      REAL      SUM(NP,*), WGT(NP,*)
C
      INTEGER   NK, NC
      INTEGER   I, J, IX, IY, N
      REAL      KERNEL(33,33), W, R
C-----------------------------------------------------------------------
      IF (NSM.LE.2) GO TO 999
      N = NP * NP
      CALL RFILL (N, 0.0, SUM)
      CALL RFILL (N, 0.0, WGT)
      NK = MIN (33, NSM)
      NK = (NK / 2) * 2 + 1
      NC = NK / 2 + 1
      W = NK / 2
C                                       kernel
      DO 20 I = 1,NK
         DO 10 J = 1,NK
            R = (I-NC)*(I-NC) + (J-NC)*(J-NC)
            KERNEL(J,I) = EXP (-R/W)
 10         CONTINUE
 20      CONTINUE
C                                       sum
      DO 90 IY = 1,NP
         DO 80 IX = 1,NP
            DO 40 I = MAX(1,IY-NC+1),MIN(NP,IY+NC-1)
               DO 30 J = MAX(1,IX-NC+1),MIN(NP,IX+NC-1)
                  W = KERNEL(J-IX+NC,I-IY+NC)
                  SUM(J,I) = SUM(J,I) + W * MAP(IX,IY)
                  WGT(J,I) = WGT(J,I) + W
 30               CONTINUE
 40            CONTINUE
 80         CONTINUE
 90      CONTINUE
C                                       average
      DO 120 IY = 1,NP
         DO 110 IX = 1,NP
            IF (WGT(IX,IY).GT.0.0) THEN
               MAP(IX,IY) = SUM(IX,IY) / WGT(IX,IY)
            ELSE
               MAP(IX,IY) = 0.0D0
               END IF
 110        CONTINUE
 120     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE EVAUVH
C-----------------------------------------------------------------------
C   EVAUVH writes HI files and copies extension files for the
C   subtracted and the divided data sets when they are kept
C-----------------------------------------------------------------------
C
      INTEGER   I, J, IERR, DISK, CNO, CATB(256), LUN1, LUN2, NONOT
      CHARACTER NAMO*12, CLAO*6, AOPCOD(2)*4, HILINE*72, AMETH*4,
     *   AMODL*4, NOTTYP*2
      HOLLERITH CATBH(256)
      INCLUDE 'EVAUV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGDS.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CATB, CATBH)
      DATA LUN1, LUN2 /27,28/
      DATA AOPCOD /'SUB','DIV'/
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
      DISK = FVOL(-SC1FIL)
      CNO = FCNO(-SC1FIL)


      DO 100 I = 1,2
C                                       get image header
         CALL CATIO ('READ', DISK, CNO, CATB, 'REST', BUFF1, IERR)
         IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
            WRITE (MSGTXT,1000) IERR, 'READING HEADER', I
            GO TO 70
            END IF
         CALL HISCOP (LUN1, LUN2, DISKIN, DISK, OLDCNO, CNO, CATB,
     *      BUFF1, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR, 'COPYING HI FILE'
            GO TO 70
            END IF
C                                       calibration history
         CALL CALHIS (LUN2, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       New history
         CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 75
C                                       If point model, no model file
         IF (.NOT.DOPTMD) THEN
C                                       Model file(s)
            CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *         IERR)
            IF (IERR.NE.0) GO TO 75
            WRITE (HILINE,1020) TSKNAM, VER
            IF (MODEL.EQ.1) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 75
C                                       Number of input images
            WRITE (HILINE,1025) TSKNAM, MFIELD
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 75
C                                       Add no. clean comps.
            DO 25 J = 1,MFIELD
               NCOMP(J) = NSUBG(J) - 1
               WRITE (HILINE,1021) TSKNAM, J, BCOMP(J), J, NCOMP(J)
               IF (MODEL.EQ.1) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 75
 25            CONTINUE
            END IF
         CALL H2CHR (12, KHIMNO, CATBH(KHIMN), NAMO)
         CALL H2CHR (6, KHIMCO, CATBH(KHIMC), CLAO)
         CALL HENCOO (TSKNAM, NAMO, CLAO, CATB(KIIMS), DISK, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 75
C                                       Modeling method
         AMETH = 'DFT '
         IF (METHOD.EQ.1) AMETH = 'GRID'
         WRITE (HILINE,1027) TSKNAM, AMETH
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 75
C                                       OPCODE
         WRITE (HILINE,1029) TSKNAM, AOPCOD(I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 75
C                                       Model type
C                                       CC or image
         IF (.NOT.DOPTMD) THEN
            AMODL = 'COMP'
            IF (MODEL.EQ.2) AMODL = 'IMAG'
            IF (MODEL.EQ.3) AMODL = 'SUBI'
            WRITE (HILINE,1028) TSKNAM, AMODL
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 75
C                                       Point model
         ELSE
            WRITE (HILINE,1030) TSKNAM, SMODEL(1), SMODEL(2), SMODEL(3)
            CALL HIADD (LUN2, HILINE, BUFF3, IERR)
            IF (IERR.NE.0) GO TO 75
C                                       Other parameters
            WRITE (HILINE,1031) TSKNAM, SMODEL(4), SMODEL(5), SMODEL(6),
     *         SMODEL(7)
            IF (SMODEL(4).GT.0.01) CALL HIADD (LUN2, HILINE, BUFF3,
     *         IERR)
            IF (IERR.NE.0) GO TO 75
            END IF
         GO TO 75
 70      CALL MSGWRT (7)
C                                       Close history file.
 75      CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
C         CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISK, OLDCNO,
C     *      CNO, CATB, BUFF1, BUFF2, IERR)
         CALL COPTAB (DISKIN, OLDCNO, DISK, CNO, IERR)
         IF (IERR.GT.2) THEN
            WRITE (MSGTXT,1000) IERR, 'COPYING TABLES', I
            CALL MSGWRT (7)
            END IF
C                                        Update CATBLK.
         CALL CATIO ('UPDT', DISK, CNO, CATB, 'REST', BUFF1, IERR)
         DISK = FVOL(-SC2FIL)
         CNO = FCNO(-SC2FIL)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('EVAUVH: ERROR',I4,' ON ',A,' OUTPUT FILE',I2)
 1020 FORMAT (A6,'VER     = ',I6,' / CC file ver. no.')
 1021 FORMAT (A6,'BCOMP(',I2,')=',I6,', NCOMP(',I3,') =',I6,
     *   ' / First-last comp. no.')
 1025 FORMAT (A6,'NMAPS   = ',I6,' / Number of model images')
 1027 FORMAT (A6,'CMETHOD = ''',A4,'''  / Model method')
 1028 FORMAT (A6,'CMODEL  = ''',A4,'''  / Model type')
 1029 FORMAT (A6,'OPCODE  = ''',A4,'''  / Operation type')
 1030 FORMAT (A6,'SMODEL  = ',F12.5,2F10.5,' / Model flux,RA,Dec')
 1031 FORMAT (A6,'          ',F12.5,2F10.5,F8.5,' / Other parms')
      END
