LOCAL INCLUDE 'PRMFIT.INC'
      INTEGER   MAXCMP, MAXPRM, MXPAIR, MAXLIS, NPLIM, NMXIMG, NCOLRM
      PARAMETER (MAXCMP=4)
      PARAMETER (MAXPRM=4*MAXCMP)
      PARAMETER (MXPAIR=(((MAXCMP-1)*MAXCMP)/2))
      PARAMETER (MAXLIS = 1000)
      PARAMETER (NPLIM=8192)
      PARAMETER (NMXIMG=12)
      PARAMETER (NCOLRM=14)
LOCAL END
LOCAL INCLUDE 'RMFIT.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'PRMFIT.INC'
C                                       Local include for XRMFIT
      REAL      XSEQ1, XDISK1, XINVER, XSEQ2, XDISK2, XSEQ3, XDISK3,
     *   XSEQ4, XDISK4, XSEQ5, XDISK5, XSEQO, XDISKO, UBLC(7), UTRC(7),
     *   YINC, ZINC, PCUT, ICUT, XDOCAT, XNIT, XCOMPS, DOTV, DOSPIX(4),
     *   RMSLIM, DOWGT, BADD(10)
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XNAME3(3),
     *   XCLAS3(2), XNAME4(3), XCLAS4(2), XNAME5(3), XCLAS5(2),
     *   XNAMOU(3), XINFIL(12)
      CHARACTER NAMEIN(5)*12, CLASS(5)*6, NAMOUT*12, CLAOUT*6,
     *   FUNCTY(NMXIMG*MAXCMP)*2, INFILE*48
      DOUBLE PRECISION CATD(128), XVOFF
      REAL      CATR(256), BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS),
     *   BUFFS(MABFSS,5), BLC(7), TRC(7)
      HOLLERITH CATH(256)
      LOGICAL   RMNEW, ICUBE, ISIIMG
      INTEGER   CATBLK(256), SEQIN(5), SEQOUT, DISKIN(5), DISKO, NEWCNO,
     *   OLDCNO(5), JBUFSZ, ICODE, DOCOMP(MAXPRM), GCODE, SCRTCH(4096),
     *   RMVERS, IYINC, IZINC, IBLC(2), ITRC(2), LBLC(2), LTRC(2),
     *   RMBUFF(512), PSTART, RMROWS, IRMRNO, NCOMPS, RMKOLS(NCOLRM),
     *   RMNUMV(NCOLRM), TVSUP, DONROW, DOCAT, PIXLIS(2,MAXLIS), NLIST,
     *   IPL(2), SUBWIN(4)
      COMMON /INPARM/ XNAME1, XCLAS1, XSEQ1, XDISK1, XINVER, XNAME2,
     *   XCLAS2, XSEQ2, XDISK2, XNAME3, XCLAS3, XSEQ3, XDISK3, XNAME4,
     *   XCLAS4, XSEQ4, XDISK4, XNAME5, XCLAS5, XSEQ5, XDISK5, XNAMOU,
     *   XSEQO, XDISKO, UBLC, UTRC, YINC, ZINC, PCUT, ICUT, XDOCAT,
     *   XNIT, XCOMPS, DOTV, DOSPIX, RMSLIM, DOWGT, XINFIL, BADD
      COMMON /RMACHR/ NAMEIN, CLASS, NAMOUT, CLAOUT, FUNCTY, INFILE
      COMMON /TPARMS/ RMBUFF, XVOFF, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NEWCNO, OLDCNO, JBUFSZ, ICODE, DOCOMP, GCODE, RMVERS, IYINC,
     *   IZINC, IBLC, ITRC, LBLC, LTRC, RMNEW, PSTART, RMROWS, IRMRNO,
     *   RMKOLS, RMNUMV, TVSUP, DONROW, DOCAT, PIXLIS, NLIST, IPL,
     *   NCOMPS, ICUBE, ISIIMG, BLC, TRC, SUBWIN
      COMMON /BUFRS/ BUFFS, SCRTCH
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (BUFFS(1,1), BUFF1)
      EQUIVALENCE (BUFFS(1,2), BUFF2)
      EQUIVALENCE (BUFFS(1,3), BUFF3)
C                                                          End RMFIT
LOCAL END
LOCAL INCLUDE 'RMFITD.INC'
      INCLUDE 'PRMFIT.INC'
C
      DOUBLE PRECISION QDATA(NPLIM), UDATA(NPLIM), REDATA(NPLIM),
     *   IMDATA(NPLIM), LAMSQ(NPLIM), AMDATA(NPLIM), PHDATA(NPLIM),
     *   WEIGHT(2*NPLIM), LAMSQ1
      REAL      ORANGE(2,5), XBAR, XRANGE(2,2), THERMS(2,2)
      INTEGER   NITTER, ITTER, LLCOMP(MAXPRM), IGR1, IGR2, IGR3,
     *   IGR4, IGR5, TTYLUN, TTYIND, IGLUN, IGFIND, PLTBLK(256),
     *   PLPOS(7), NVAR, IVAR(MAXPRM), JVAR(MAXPRM), SPIXDO
      LOGICAL   DOEVEN, WASREI, NOWREI
      EQUIVALENCE (AMDATA, REDATA), (PHDATA, IMDATA)
      COMMON /GDATA/ QDATA, UDATA, REDATA, IMDATA, WEIGHT, LAMSQ,
     *   LAMSQ1, XBAR, NITTER, ITTER, LLCOMP, IGR1, IGR2, IGR3, IGR4,
     *   IGR5, DOEVEN, TTYLUN, TTYIND, ORANGE, XRANGE, IGLUN, IGFIND,
     *   PLTBLK, PLPOS, WASREI, NOWREI, NVAR, IVAR, JVAR, THERMS,
     *   SPIXDO
LOCAL END
LOCAL INCLUDE 'RMFITO.INC'
      INTEGER   CATOLD(256,5)
      REAL      OLDR(256,5)
      HOLLERITH OLDH(256,5)
      DOUBLE PRECISION OLDD(128,5)
      EQUIVALENCE (CATOLD, OLDR, OLDD, OLDH)
      COMMON /OLDHDR/ CATOLD
LOCAL END
      PROGRAM RMFIT
C-----------------------------------------------------------------------
C! Fits 1-D polarization curves to rows of an image.
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2016
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
C   RMFIT fits 1-dimensional polarization functions to Q/U cubes.
C   It fits up to 4 components and writes out an n-dim residual cube
C   (DOCAT > 0) and 6*Ngauss n-1 dimensional images of the results and
C   errors.  It will display the data, initial guess, model, and
C   residual for each row on the TV.  After each fit so displayed, it
C   asks for permission to keep the results.  If NCOMPS > 1, it asks
C   for permission to keep the initial guess and will accept TV cursor
C   input of a new initial guess on that.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of Q input image.
C      INCLASS        CLAIN         Class of Q input image.
C      INSEQ          SEQIN         Seq. of Q input image.
C      INDISK         DISKIN        Disk number of Q input image.
C      INVERS         RMVERS        XG table version number in use
C      IN2NAME        NAMEIN        Name of U input image.
C      IN2CLASS       CLAIN         Class of U input image.
C      IN2SEQ         SEQIN         Seq. of U input image.
C      IN2DISK        DISKIN        Disk number of U input image.
C      IN3NAME        NAMEIN        Name of I input image.
C      IN3CLASS       CLAIN         Class of I input image.
C      IN3SEQ         SEQIN         Seq. of I input image.
C      IN3DISK        DISKIN        Disk number of I input image.
C      IN4NAME        NAMEIN        Name FARS real input image.
C      IN4CLASS       CLAIN         Class FARS real input image.
C      IN4SEQ         SEQIN         Seq. FARS real input image.
C      IN4DISK        DISKIN        Disk FARS real input image.
C      IN5NAME        NAMEIN        Name FARS imag input image.
C      IN5CLASS       CLAIN         Class FARS imag input image.
C      IN5SEQ         SEQIN         Seq. FARS imag input image.
C      IN4DISK        DISKIN        Disk FARS imag input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      UBLC(7)        UBLC          Bottom left corner of subimage
C                                   of input image.
C      UTRC(7)        UTRC          Top right corner of subimage.
C      YINC           YINC          Pixel increment on 2nd axis
C      ZINC           ZINC          Pixel increment on 3rd axis
C      PCUT           PCUT          Flux cutoff: average P
C      ICUT           ICUT          Flux cutoff: average I
C                                   must > ICUT to fit
C      DOCAT          DOCAT         Catalog the residual map
C      NITER          XNIT          Limit on iterations in fit
C      NCOMPS         NCOMPS        Number of Components
C      BADD(10)       IBAD          Disk numbers to avoid.
C   Programmer Eric W. Greisen
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, DEVON, I
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'RMFIT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RMFIIN (PRGM, IRET)
      DEVON = 0
      IF (IRET.NE.0) GO TO 990
C                                       inits, open TV
      IF (ICODE.GE.1) THEN
         DEVON = 2
         TVSUP = 0
         CALL YINIT (BUFF2, IRET)
         IF (IRET.GT.0) GO TO 990
      ELSE
         TVSUP = 1
         END IF
C                                       routine that goes through
C                                       whole input cube
      IF ((IRET.EQ.0) .AND. (PSTART.LE.RMROWS)) THEN
         CALL RMFIDO (IRET)
         IF ((IRET.EQ.0) .AND. ((IYINC.GT.1) .OR. (IZINC.GT.1)))
     *      CALL RMFID1 (IRET)
         END IF
C                                       interactive routine to polish
      IF ((IRET.EQ.0) .AND. (DEVON.EQ.2)) CALL RMFITV (IRET)
C                                       close devices
      IF (DEVON.EQ.2) CALL TVCLOS (SCRTCH, IERR)
      DEVON = 0
C                                       Resume AIPS
      IF ((RQUICK) .AND. (ICODE.GT.0)) CALL RELPOP (IRET, SCRTCH, IERR)
C                                       write out images
      IF ((IRET.EQ.0) .AND. (DOCAT.GT.0)) CALL RMFIOU (IRET)
C                                       close RM table
      CALL TABKEY ('WRIT', 'PIX FIT ', 1, RMBUFF, 1, DONROW, 4, I)
      CALL TABIO ('CLOS', 0, IRMRNO, RMBUFF, RMBUFF, I)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE RMFIIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   RMFIIN gets input parameters for RMFIT and creates an output table
C   if needed for the fitting results, filling it with flux values.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, MTYPE*2, FCHARS(2)*4, CHTM12*12, PHNAME*48
      INTEGER   IERR, NPARM, IROUND, I, TVCORN(2), J, K, NUMSP, NUMTH
      LOGICAL   WASERR
      REAL      XFAC
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA FCHARS /'FREQ','FQID'/
      DATA TVCORN /2*0/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      DONROW = 0
      DOEVEN = .FALSE.
      TTYLUN = 5
      TTYIND = 0
C                                       Get input parameters.
      NPARM = 91
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAME1, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
         CALL MSGWRT (8)
         END IF
C                                       Using the TV?
      ICODE = 1
      IF (DOTV.GT.1.5) ICODE = 2
      IF ((NPOPS.GT.NINTRN) .AND. (NTVDEV.LE.0)) THEN
         ICODE = -1
         DOTV = -1.0
         END IF
C                                       Restart AIPS
      IF ((RQUICK) .AND. ((ICODE.LE.0) .OR. (IRET.NE.0))) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN(1) = IROUND (XSEQ1)
      SEQIN(2) = IROUND (XSEQ2)
      SEQIN(3) = IROUND (XSEQ3)
      SEQIN(4) = IROUND (XSEQ4)
      SEQIN(5) = IROUND (XSEQ5)
      SEQOUT = IROUND (XSEQO)
      DISKIN(1) = IROUND (XDISK1)
      DISKIN(2) = IROUND (XDISK2)
      DISKIN(3) = IROUND (XDISK3)
      DISKIN(4) = IROUND (XDISK4)
      DISKIN(5) = IROUND (XDISK5)
      DISKO = IROUND (XDISKO)
      DOCAT = IROUND (XDOCAT)
      DOCAT = MAX (0, MIN (3, DOCAT))
C                                       Characters
      CALL H2CHR (12, 1, XNAME1, NAMEIN(1))
      CALL H2CHR (12, 1, XNAME2, NAMEIN(2))
      CALL H2CHR (12, 1, XNAME3, NAMEIN(3))
      CALL H2CHR (12, 1, XNAME4, NAMEIN(4))
      CALL H2CHR (12, 1, XNAME5, NAMEIN(5))
      CALL H2CHR (6, 1, XCLAS1, CLASS(1))
      CALL H2CHR (6, 1, XCLAS2, CLASS(2))
      CALL H2CHR (6, 1, XCLAS3, CLASS(3))
      CALL H2CHR (6, 1, XCLAS4, CLASS(4))
      CALL H2CHR (6, 1, XCLAS5, CLASS(5))
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CLAOUT = ' '
      CALL H2CHR (48, 1, XINFIL, INFILE)
      IF (DOWGT.GE.1.5) INFILE = ' '
      DO 10 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 10      CONTINUE
      ISIIMG = (NAMEIN(3).NE.' ') .OR. (CLASS(3).NE.' ')
      IF (RMSLIM.LE.0.0) RMSLIM = 1.E6
C                                       Get CATBLK from old file.
      MTYPE = 'MA'
      DO 15 I = 1,5
         IF ((I.NE.3) .OR. (ISIIMG)) THEN
            OLDCNO(I) = 1
            CALL CATDIR ('SRCH', DISKIN(I), OLDCNO(I), NAMEIN(I),
     *         CLASS(I), SEQIN(I), MTYPE, NLUSER, STAT, SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, NAMEIN(I), CLASS(I), SEQIN(I),
     *            DISKIN(I), NLUSER
               GO TO 990
               END IF
C                                       Read CATBLK and mark 'READ'.
            STAT = 'READ'
            IF (I.EQ.1) STAT = 'WRIT'
            CALL CATIO ('READ', DISKIN(I), OLDCNO(I), CATOLD(1,I), STAT,
     *         SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, ' READING CATALOG HEADER'
               GO TO 990
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKIN(I)
            FCNO(NCFILE) = OLDCNO(I)
            FRW(NCFILE) = 0
            IF (I.EQ.1) FRW(NCFILE) = 1
            END IF
 15      CONTINUE
C                                       real/imag or amp/phas
      WASREI = (OLDH(KHBUN,4).EQ.OLDH(KHBUN,5)) .AND.
     *   (OLDH(KHBUN+1,4).EQ.OLDH(KHBUN+1,5))
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Set defaults on BLC,TRC
      CALL RFILL (7, 1.0, BLC)
      CALL RFILL (7, 0.0, TRC)
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), BLC, TRC, IERR)
C                                       Set defaults on UBLC,UTRC
      CALL WINDOW (CATOLD(KIDIM,1), CATOLD(KINAX,1), UBLC, UTRC, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check input axes
      CALL H2CHR (4, 1, CATH(KHCTP), CHTM12)
      DO 20 I = 1,2
         IF (FCHARS(I).EQ.CHTM12(:4)) GO TO 25
 20      CONTINUE
      MSGTXT = 'FIRST AXIS NOT FREQUENCY OR FQID'
      IERR = 8
      GO TO 990
C                                       get lambda squared
 25   CALL LAMSQD (I, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       is I a cube or plane
      ICUBE = ISIIMG .AND. (OLDH(KHCTP,1).EQ.OLDH(KHCTP,3)) .AND.
     *   (OLDH(KHCTP+1,1).EQ.OLDH(KHCTP+1,3))
      IF (.NOT.ISIIMG) ICUT = -1000.0
C                                       header check errors
      WASERR = .FALSE.
      DO 30 I = 2,5
         IF ((I.NE.3) .OR. (ICUBE)) THEN
            IF ((CATOLD(KINAX+1,I).NE.CATBLK(KINAX+1)) .OR.
     *         (CATOLD(KINAX+2,I).NE.CATBLK(KINAX+2))) THEN
               MSGTXT = 'AXES 2 AND 3 DO NOT MATCH IN SIZE'
               IERR = 8
               GO TO 990
               END IF
            IF ((CATOLD(KINAX,I).NE.CATBLK(KINAX)) .AND. (I.LE.3)) THEN
               MSGTXT = 'AXIS 1 DOES NOT MATCH IN SIZE FOR Q U I'
               IERR = 8
               GO TO 990
               END IF
            END IF
 30      CONTINUE
      IF (CATOLD(KINAX,4).NE.CATOLD(KINAX,5)) THEN
         MSGTXT = 'AXIS 1 DOES NOT MATCH IN SIZE FOR REAL/IMAG'
         IERR = 8
         GO TO 990
         END IF
      IF ((ISIIMG) .AND. (.NOT.ICUBE)) THEN
         IF ((CATOLD(KINAX,3).NE.CATBLK(KINAX+1)) .OR.
     *      (CATOLD(KINAX+1,3).NE.CATBLK(KINAX+2))) THEN
            MSGTXT = 'AXES 1/2 OF I PLANE DO NOT MATCH 2/3 OF Q IN SIZE'
            IERR = 8
            GO TO 990
            END IF
C                                       warnings
         IF ((CATOLD(KHCTP,3).NE.CATBLK(KHCTP+2)) .OR.
     *      (CATOLD(KHCTP+1,3).NE.CATBLK(KHCTP+3)) .OR.
     *      (CATOLD(KHCTP+2,3).NE.CATBLK(KHCTP+4)) .OR.
     *      (CATOLD(KHCTP+3,3).NE.CATBLK(KHCTP+5))) THEN
            MSGTXT = 'I PLANE AXIS 1/2 TYPES NOT MATCH Q AXIS 2/3 TYPES'
            CALL MSGWRT (7)
            WASERR = .TRUE.
         ELSE IF ((ABS(OLDD(KDCRV,3)-CATD(KDCRV+1)).GT.
     *      0.01*ABS(CATR(KRCIC+1))) .OR.
     *      (ABS(OLDD(KDCRV+1,3)-CATD(KDCRV+2)).GT.
     *      0.01*ABS(CATR(KRCIC+2)))) THEN
            MSGTXT = 'I PLANE AXIS 1/2 REF VALUE NOT MATCH Q AXIS 2/3'
     *         // ' TYPES'
            CALL MSGWRT (7)
            WASERR = .TRUE.
         ELSE IF ((ABS(OLDR(KRCIC,3)-CATR(KRCIC+1)).GT.
     *      0.001*ABS(CATR(KRCIC+1))) .OR.
     *      (ABS(OLDR(KRCIC+1,3)-CATR(KRCIC+2)).GT.
     *      0.01*ABS(CATR(KRCIC+2)))) THEN
            MSGTXT = 'I PLANE AXIS 1/2 INCREMENT NOT MATCH Q AXIS 2/3'
            CALL MSGWRT (7)
            WASERR = .TRUE.
         ELSE IF ((ABS(OLDR(KRCRP,3)-CATR(KRCRP+1)).GT.0.1) .OR.
     *      (ABS(OLDR(KRCRP+1,3)-CATR(KRCRP+2)).GT.0.1)) THEN
            MSGTXT = 'I PLANE AXIS 1/2 REF PIXEL NOT MATCH Q AXIS 2/3'
            CALL MSGWRT (7)
            WASERR = .TRUE.
         ELSE IF ((ABS(OLDR(KRCRT,3)-CATR(KRCRT+1)).GT.0.1) .OR.
     *      (ABS(OLDR(KRCRT+1,3)-CATR(KRCRT+2)).GT.0.1)) THEN
            MSGTXT = 'I PLANE AXIS 1/2 ROTATION NOT MATCH Q AXIS 2/3'
            CALL MSGWRT (7)
            WASERR = .TRUE.
            END IF
         END IF
C                                       more warnings
      DO 40 I = 2,5
         IF ((ICUBE) .OR. (I.NE.3)) THEN
            IF ((CATOLD(KHCTP+2,I).NE.CATBLK(KHCTP+2)) .OR.
     *         (CATOLD(KHCTP+3,I).NE.CATBLK(KHCTP+3)) .OR.
     *         (CATOLD(KHCTP+4,I).NE.CATBLK(KHCTP+4)) .OR.
     *         (CATOLD(KHCTP+5,I).NE.CATBLK(KHCTP+5))) THEN
               MSGTXT = 'AXIS TYPES NOT MATCH Q AXIS 2/3 TYPES'
               CALL MSGWRT (7)
               WASERR = .TRUE.
            ELSE IF ((ABS(OLDD(KDCRV+1,I)-CATD(KDCRV+1)).GT.
     *         0.01*ABS(CATR(KRCIC+1))) .OR.
     *         (ABS(OLDD(KDCRV+2,I)-CATD(KDCRV+2)).GT.
     *         0.01*ABS(CATR(KRCIC+2)))) THEN
               MSGTXT = 'AXIS REF VALUE NOT MATCH Q AXIS 2/3'
               CALL MSGWRT (7)
               WASERR = .TRUE.
            ELSE IF ((ABS(OLDR(KRCIC+1,I)-CATR(KRCIC+1)).GT.
     *         0.001*ABS(CATR(KRCIC+1))) .OR.
     *         (ABS(OLDR(KRCIC+2,I)-CATR(KRCIC+2)).GT.
     *         0.01*ABS(CATR(KRCIC+2)))) THEN
               MSGTXT = 'AXIS INCREMENT NOT MATCH Q AXIS 2/3'
               CALL MSGWRT (7)
               WASERR = .TRUE.
            ELSE IF ((ABS(OLDR(KRCRP+1,I)-CATR(KRCRP+1)).GT.0.1) .OR.
     *         (ABS(OLDR(KRCRP+2,I)-CATR(KRCRP+2)).GT.0.1)) THEN
               MSGTXT = 'AXIS REF PIXEL NOT MATCH Q AXIS 2/3'
               CALL MSGWRT (7)
               WASERR = .TRUE.
            ELSE IF ((ABS(OLDR(KRCRT+1,I)-CATR(KRCRT+1)).GT.0.1) .OR.
     *         (ABS(OLDR(KRCRT+2,I)-CATR(KRCRT+2)).GT.0.1)) THEN
               MSGTXT = 'AXIS ROTATION NOT MATCH Q AXIS 2/3'
               CALL MSGWRT (7)
               WASERR = .TRUE.
               END IF
            END IF
 40      CONTINUE
      IF (WASERR) THEN
         MSGTXT = 'CONTINUING - BUT RESULTS WILL BE RUBISH'
         CALL MSGWRT (7)
         END IF
C                                       RM table keywords
      I = YINC + 0.01
      IF (I.LE.0) I = 1
      YINC = I
      I = ZINC + 0.01
      IF (I.LE.0) I = 1
      ZINC = I
      IYINC = YINC + 0.1
      IZINC = ZINC + 0.1
      LBLC(1) = UBLC(2) + 0.1
      LBLC(2) = UBLC(3) + 0.1
      LTRC(1) = UTRC(2) + 0.1
      LTRC(2) = UTRC(3) + 0.1
      IF (PCUT.LE.0.0) PCUT = 0.0005
C                                       RM RANGE
      XRANGE(1,2) = OLDD(KDCRV,4) + (1.0 - OLDR(KRCRP,4)) *
     *   OLDR(KRCIC,4)
      XRANGE(2,2) = OLDD(KDCRV,4) + (CATOLD(KINAX,4) - OLDR(KRCRP,4)) *
     *   OLDR(KRCIC,4)
      XFAC = XRANGE(2,1) - XRANGE(1,1)
      XRANGE(2,1) = XRANGE(2,1) + 0.05 * XFAC
      XRANGE(1,1) = XRANGE(1,1) - 0.05 * XFAC
      XFAC = XRANGE(2,2) - XRANGE(1,2)
      XRANGE(2,2) = XRANGE(2,2) + 0.05 * XFAC
      XRANGE(1,2) = XRANGE(1,2) - 0.05 * XFAC

C                                       Component parms
      NCOMPS = XCOMPS + 0.01
      NCOMPS = MIN (4, MAX (1, NCOMPS))
      XCOMPS = NCOMPS
      CALL FILL (MAXPRM, -1, DOCOMP)
      J = 0
      NUMSP = 0
      NUMTH = 0
      SPIXDO = 0
      DO 45 I = 1,NCOMPS
         K = IROUND (DOSPIX(I))
         IF (K.EQ.1) THEN
            NUMSP = NUMSP + 1
         ELSE IF ((K.GE.2) .AND. (K.LE.4)) THEN
            NUMTH = NUMTH + 1
            IF (NUMTH.EQ.1) THEN
               SPIXDO = K
            ELSE IF (SPIXDO.NE.K) THEN
               MSGTXT = 'CANNOT MIX DIFFERENT THICKNESS MODEL TYPES'
               GO TO 990
               END IF
         ELSE
            K = 0
            END IF
         DOSPIX(I) = K
 45      CONTINUE
      IF ((NUMSP.GT.0) .AND. (NUMTH.GT.0)) THEN
         MSGTXT = 'CANNOT MIX SPECTRAL INDEX AND THICKNESS FITS'
         GO TO 990
         END IF
      IF (NUMSP.GT.0) SPIXDO = 1
      K = 1
      DO 50 I = 1,NCOMPS
         DOCOMP(J+1) = 1
         DOCOMP(J+2) = 1
         DOCOMP(J+3) = 1
         K = K + 3
         IF (DOSPIX(I).GE.0.5) DOCOMP(J+4) = 1
         IF (DOSPIX(I).GE.0.5) K = K + 1
         J = J + 4
 50      CONTINUE
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Check input size
      IRET = 0
      IF (XNIT.LT.10.) XNIT = 100 * XCOMPS
      IF (UTRC(1)-UBLC(1).GE.NPLIM) THEN
         IRET = 10
         WRITE (MSGTXT,1045) NPLIM
         GO TO 990
         END IF
      IF (UTRC(1)-UBLC(1)+1.LE.K) THEN
         IRET = 10
         WRITE (MSGTXT,1050) K
         GO TO 990
         END IF
C                                       set up plotting
      CALL GINIT (DISKIN, OLDCNO, PHNAME, 0, 0, NPARM, XNAME1, .TRUE.,
     *   0, 0, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING TV PLOT'
         GO TO 990
         END IF
C                                       RM version
      CALL FNDEXT ('RM', CATOLD, I)
      RMVERS = XINVER + 0.1
      IF (RMVERS.LE.0) THEN
         RMVERS = I + 1
      ELSE
         RMVERS = MIN (I+1, RMVERS)
         END IF
      RMNEW = RMVERS.GT.I
C                                       fill RM table
      CALL RMFILL (SPIXDO, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       get weights
      CALL WTFILL (IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C                                       Restart AIPS
 995  IF ((RQUICK) .AND. (IRET.NE.0) .AND. (ICODE.GT.0)) CALL RELPOP
     *   (IRET, SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFIIN: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1045 FORMAT ('WORKS ONLY ON (SUB)ROWS <=',I5,' PIXELS')
 1050 FORMAT ('REQUIRES AT LEAST',I4,' PIXELS TO DO FIT')
      END
      SUBROUTINE LAMSQD (ITYPE, IRET)
C-----------------------------------------------------------------------
C   Fills array of wavelength squared
C   Inputs:
C      ITYPE   I      1 => FREQ axis, 2 => FQID axis
C   Outputs:
C      IRET    I      Error code
C   Common out:
C      LAMSQ   D(*)   wavelength squared (meters ^2)
C-----------------------------------------------------------------------
      INTEGER   ITYPE, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INTEGER  FBLC, FTRC, I, FREQAX, ILUN, LUNTMP, FQVER, FQBUFF(512),
     *   IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), NUMIF, NUMREC, FQID,
     *   IFSIDE, IFQ
      REAL     IFCHW, IFTBW
      DOUBLE PRECISION FF, IFFREQ, LL
      CHARACTER BNDCOD*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      FBLC = UBLC(1) + 0.1
      FTRC = UTRC(1) + 0.1
      XRANGE(1,1) = 1.E10
      XRANGE(2,1) = -1.E10
      LAMSQ1 = (VELITE / 1.0D9) ** 2
C                                       freq axis
      IF (ITYPE.EQ.1) THEN
         DO 20 I = FBLC,FTRC
            FF = CATD(KDCRV) + (I - CATR(KRCRP)) * CATR(KRCIC)
            IF (FF.EQ.0.0D0) THEN
               MSGTXT = 'FREQUENCY IS ZERO'
               IRET = 10
               GO TO 990
            ELSE
               LL = (VELITE / FF) ** 2
               LAMSQ(I-FBLC+1) = LL
               IF (LL.LT.XRANGE(1,1)) XRANGE(1,1) = LL
               IF (LL.GT.XRANGE(2,1)) XRANGE(2,1) = LL
               END IF
 20         CONTINUE
C                                       FQ table
      ELSE IF (ITYPE.EQ.2) THEN
         CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATH(KHCTP), FREQAX,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'NO FREQUENCY AXIS FOUND: QUITTING'
            GO TO 990
            END IF
         FQVER = 0
         ILUN = LUNTMP (1)
         CALL FQINI ('READ', FQBUFF, DISKIN, OLDCNO, FQVER, CATBLK,
     *      ILUN, IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN FQ TABLE'
            GO TO 990
            END IF
C                                       Get number of records
         NUMREC = FQBUFF(5)
         IF (NUMREC.LE.0) GO TO 999
C                                       read FQ table
         DO 45 IFQRNO = 1,NUMREC
            IFQ = IFQRNO
            CALL TABFQ ('READ', FQBUFF, IFQ, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ FQ TABLE'
               GO TO 990
               END IF
            FF = (FQID - CATD(KDCRV)) / CATR(KRCIC) + CATR(KRCRP)
            I = FF + 1.1D0 - FBLC
            IF ((I.GE.1) .AND. (I.LE.FTRC-FBLC+1)) THEN
               FF = IFFREQ + CATD(KDCRV+FREQAX)
               LL = (VELITE / FF) ** 2
               LAMSQ(I) = LL
               IF (LL.LT.XRANGE(1,1)) XRANGE(1,1) = LL
               IF (LL.GT.XRANGE(2,1)) XRANGE(2,1) = LL
               END IF
 45         CONTINUE
C                                      Close table.
         CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'CLOSE FQ TABLE'
            GO TO 990
            END IF
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LAMSQD: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RMFILL (SPIXDO, IRET)
C-----------------------------------------------------------------------
C   RMFILL checks pre-existing RM files and builds new ones (filling
C   them with spectrum peak values
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   SPIXDO, IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER   IY, IZ, RMLUN, LUNI(3), INDI(3), NXI, NYI, WINI(4),
     *   IROUND, LIM3, LIM2, LIM1, I3, I2, I1, BOI, IPOS(7), IBIND(3),
     *   NIA, NPA, I, NFIL
      REAL      RESULT(MAXPRM,2), VAL, PICUT, PPCUT, IAVG, PAVG,
     *   THERMS(2,2)
      DOUBLE PRECISION DTEMP
      CHARACTER PHNAME*48
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA THERMS /4*0.0/
C-----------------------------------------------------------------------
C                                       pre-existing check
      RMLUN = 97
      IF (.NOT.RMNEW) THEN
         CALL RMINI ('READ', RMBUFF, DISKIN(1), OLDCNO(1), RMVERS,
     *      CATOLD, RMLUN, IRMRNO, RMKOLS, RMNUMV, IBLC, ITRC, IY, IZ,
     *      PICUT, PPCUT, PSTART, SPIXDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING PRE-EXISTING RM TABLE'
            GO TO 990
            END IF
         CALL TABIO ('CLOS', 0, IRMRNO, RMBUFF, RMBUFF, IRET)
         IF ((IBLC(1).GT.LBLC(1)) .OR. (IBLC(2).GT.LBLC(2)) .OR.
     *      (ITRC(1).LT.LTRC(1)) .OR. (ITRC(2).LT.LTRC(2))) THEN
            MSGTXT = 'OLD RM TABLE DOES NOT MATCH CURRENT ADVERBS'
            IRET = 10
            GO TO 990
            END IF
         RMROWS = RMBUFF(5)
C                                       reopen write
         CALL RMINI ('WRIT', RMBUFF, DISKIN(1), OLDCNO(1), RMVERS,
     *      CATOLD, RMLUN, IRMRNO, RMKOLS, RMNUMV, IBLC, ITRC, IY, IZ,
     *      PICUT, PPCUT, PSTART, SPIXDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-OPENING OLD RM TABLE'
            GO TO 990
            END IF
         PSTART = MAX (PSTART + 1, 1)
C                                       from beginning if lower cutoff
         IF (PICUT.GT.ICUT) THEN
            PSTART = 1
            DTEMP = ICUT
            CALL TABKEY ('WRIT', 'ICLIP   ', 1, RMBUFF, 1, DTEMP, 1, I1)
            END IF
         IF (PPCUT.GT.PCUT) THEN
            PSTART = 1
            DTEMP = PCUT
            CALL TABKEY ('WRIT', 'PCLIP   ', 1, RMBUFF, 1, DTEMP, 1, I1)
            END IF
C                                       new one
      ELSE
         IBLC(1) = BLC(2)
         IBLC(2) = BLC(3)
         ITRC(1) = TRC(2)
         ITRC(2) = TRC(3)
         PSTART = 0
         CALL RMINI ('WRIT', RMBUFF, DISKIN(1), OLDCNO(1), RMVERS,
     *      CATOLD, RMLUN, IRMRNO, RMKOLS, RMNUMV, IBLC, ITRC, IYINC,
     *      IZINC, ICUT, PCUT, PSTART, SPIXDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING NEW RM TABLE'
            GO TO 990
            END IF
         CALL RFILL (2*MAXPRM, FBLANK, RESULT)
         MSGTXT = 'Reading image cubes to find maxima in rows'
         CALL MSGWRT (2)
         MSGTXT = 'and initialize the RM table with these values'
         CALL MSGWRT (2)
C                                       Open and init for read
         DO 10 I = 1,3
            IF ((I.NE.3) .OR. (ISIIMG)) THEN
               LUNI(I) = 32 + I
               CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
               CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, .TRUE.,
     *            .FALSE., .TRUE., IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
                  GO TO 990
                  END IF
               END IF
 10         CONTINUE
C                                       I as plane
         NFIL = 3
         IF ((ISIIMG) .AND. (.NOT.ICUBE)) THEN
            NXI = CATOLD(KINAX,3)
            NYI = CATOLD(KINAX+1,3)
            WINI(1) = IROUND (BLC(2))
            WINI(2) = IROUND (BLC(3))
            WINI(3) = IROUND (TRC(2))
            WINI(4) = IROUND (TRC(3))
            CALL FILL (7, 1, IPOS)
            CALL COMOFF (CATOLD(KIDIM,3), CATOLD(KINAX,3), IPOS(3),
     *         BOI, IRET)
            BOI = BOI + 1
            CALL MINIT ('READ', LUNI(3), INDI(3), NXI, NYI, WINI,
     *         BUFFS(1,3), JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
               GO TO 990
               END IF
            NFIL = 2
            END IF
         IF (.NOT.ISIIMG) NFIL = 2
         NXI = CATOLD(KINAX,1)
         NYI = CATOLD(KINAX+1,1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
         LIM3 = TRC(3) - BLC(3) + 1.01
         LIM2 = TRC(2) - BLC(2) + 1.01
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         IAVG = 10000.
         DO 100 I3 = 1,LIM3
            IPOS(3) = BLC(3) + I3 - 0.9
            CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *         BOI, IRET)
            BOI = BOI + 1
            DO 20 I = 1,NFIL
               CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI, WINI,
     *            BUFFS(1,I), JBUFSZ, BOI, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
                  GO TO 990
                  END IF
 20            CONTINUE
            IF ((.NOT.ICUBE) .AND. (ISIIMG)) THEN
               CALL MDISK ('READ', LUNI(3), INDI(3), BUFFS(1,3),
     *            IBIND(3), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
                  GO TO 990
                  END IF
               END IF
            DO 90 I2 = 1,LIM2
               DO 30 I = 1,NFIL
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
                     GO TO 990
                     END IF
 30               CONTINUE
               IF (ISIIMG) IAVG = 0.0
               PAVG = 0.0
               NIA = 0
               NPA = 0
               IF ((ISIIMG) .AND. (.NOT.ICUBE)) THEN
                  IF (BUFF3(IBIND(3)+I2-1).NE.FBLANK) THEN
                     NIA = 1
                     IAVG = BUFF3(IBIND(3)+I2-1)
                     END IF
                  END IF
               DO 80 I1 = 1,LIM1
                  IF ((ICUBE) .AND. (BUFF3(IBIND(3)+I1-1).NE.FBLANK))
     *               THEN
                     IAVG = IAVG + BUFF3(IBIND(3)+I1-1)
                     NIA = NIA + 1
                     END IF
                  IF ((BUFF1(IBIND(1)+I1-1).NE.FBLANK) .AND.
     *               (BUFF2(IBIND(2)+I1-1).NE.FBLANK)) THEN
                     VAL = SQRT (BUFF1(IBIND(1)+I1-1) ** 2 +
     *                  BUFF2(IBIND(2)+I1-1) ** 2)
                     PAVG = PAVG + VAL
                     NPA = NPA + 1
                     END IF
 80               CONTINUE
               IPOS(2) = BLC(2) + I2 - 0.9
               IF (NIA.GT.1) IAVG = IAVG / NIA
               IF (NPA.GT.1) PAVG = PAVG / NPA
               CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *            IPOS(2), 0, IAVG, PAVG, RESULT, THERMS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE NEW RM TABLE'
                  GO TO 990
                  END IF
 90            CONTINUE
 100        CONTINUE
         CALL ZCLOSE (LUNI(1), INDI(1), IRET)
         CALL ZCLOSE (LUNI(2), INDI(2), IRET)
         IF (ISIIMG) CALL ZCLOSE (LUNI(3), INDI(3), IRET)
         RMROWS = RMBUFF(5)
C                                       close table for safety
         CALL TABIO ('CLOS', 0, IRMRNO, RMBUFF, RMBUFF, IRET)
C                                       and reopen
         CALL RMINI ('WRIT', RMBUFF, DISKIN(1), OLDCNO(1), RMVERS,
     *      CATOLD, RMLUN, IRMRNO, RMKOLS, RMNUMV, IBLC, ITRC, IYINC,
     *      IZINC, ICUT, PCUT, PSTART, SPIXDO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING NEW RM TABLE'
            GO TO 990
            END IF
         PSTART = 1
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFILL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE WTFILL (IRET)
C-----------------------------------------------------------------------
C   WTFILL fills the weight array with 1.0, values read from text file,
C   or values found by fitting rms.  Then it normalizes the array
C   Outputs:
C      IRET     I      Error code
C   Common
C      WEIGHT   D(*)   weights
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER   NX, NY, NWORDS, JTRIM, JT, LA, LF1, LF2, NF, LUN, FIND,
     *   IW, KW, I, KBP
      LONGINT   PIMAGE
      REAL      IMAGE(2)
      LOGICAL   T, F
      CHARACTER INLINE*132, PHNAME*48
      DOUBLE PRECISION XX, WSUM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      LF1 = UBLC(1) + 0.1
      LF2 = UTRC(1) + 0.1
      NF = LF2 - LF1 + 1
      LA = CATOLD(KINAX,1)
C                                       no weighting
      IF (DOWGT.LE.0.0) THEN
         CALL DFILL (2*NF, 1.0D0, WEIGHT)
C                                       text file
      ELSE IF (INFILE.NE.' ') THEN
         CALL DFILL (2*NF, 0.0D0, WEIGHT)
         LUN = 10
         CALL ZTXOPN ('READ', LUN, FIND, INFILE, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INFILE TEXT FILE'
            GO TO 990
            END IF
         NY = 132
         IW = 0
         KW = 0
 10      CALL ZTXIO ('READ', LUN, FIND, INLINE, IRET)
         IF (IRET.EQ.2) THEN
            IRET = 0
            CALL ZTXCLS (LUN, FIND, I)
         ELSE IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TEXT FILE'
            GO TO 990
         ELSE
            JT = JTRIM (INLINE)
            JT = INDEX (INLINE, ';')
            IF (JT.GT.0) INLINE(JT:) = ' '
            JT = JTRIM (INLINE)
            IF (JT.LE.0) GO TO 10
            KBP = 1
C                                       parse
 20         CALL GETNUM (INLINE, NY, KBP, XX)
C                                       ran off end this line
            IF (KBP.GT.NY) THEN
               GO TO 10
            ELSE
               IW = IW + 1
               IF (IW.GT.LA) IW = 1
               IF ((IW.GE.LF1) .AND. (IW.LE.LF2)) THEN
                  KW = KW + 1
                  WEIGHT(KW) = XX
                  END IF
               GO TO 20
               END IF
            END IF
C                                       fill U from Q ??
         WSUM = 0.0D0
         DO 30 I = 1,NF
            IF (WEIGHT(I).LE.0.0D0) WEIGHT(I) = 1.0D0
            IF (WEIGHT(I+NF).LE.0.0D0) WEIGHT(I+NF) = WEIGHT(I)
            WSUM = WSUM + WEIGHT(I) + WEIGHT(I+NF)
 30         CONTINUE
         WSUM = WSUM / (2.0D0 * NF)
         DO 40 I = 1,2*NF
            WEIGHT(I) = WEIGHT(I) / WSUM
 40         CONTINUE
C                                       fit the rms of each image plane
      ELSE
         NX = CATOLD(KINAX+1,1)
         NY = CATOLD(KINAX+2,1)
         NWORDS = (NX * NY * NF - 1) / 1024 + 2
         CALL ZMEMRY ('GET ', 'WTFILL', NWORDS, IMAGE, PIMAGE, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'GETTING DYNAMIC MEMORY'
            GO TO 990
            END IF
         LUN = 29
         DO 120 I = 1,2
            CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
            CALL ZOPEN (LUN, FIND, DISKIN(I), PHNAME, T, F, T,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
               GO TO 990
               END IF
            CALL RMSFIT (LUN, FIND, CATOLD, LF1, LF2, NX, NY,
     *         IMAGE(1+PIMAGE),  WEIGHT(1+(I-1)*NF), JBUFSZ, BUFFS,
     *         IRET)
            CALL ZCLOSE (LUN, FIND, JT)
            IF (IRET.NE.0) GO TO 999
 120        CONTINUE
         CALL ZMEMRY ('FREE', 'WTFILL', NWORDS, IMAGE, PIMAGE, IRET)
C                                       fill U from Q ??
         WSUM = 0.0D0
         DO 130 I = 1,NF
            WSUM = WSUM + WEIGHT(I) + WEIGHT(I+NF)
 130        CONTINUE
         WSUM = WSUM / (2.0D0 * NF)
         DO 140 I = 1,2*NF
            WEIGHT(I) = WEIGHT(I) / WSUM
 140        CONTINUE
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTFILL: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RMSFIT (LUN, IND, CATBLK, LF1, LF2, NX, NY, IMAGE,
     *   WEIGHT, JBUFSZ, BUFF, IRET)
C-----------------------------------------------------------------------
C   RMSFIT reads in the f,x,y image doing a transpose in memory
C   It then does a robust fit for the rms.
C   Inputs:
C      LUN      I      LUN of open map file
C      IND      I      FTAB pointer of open map file
C      CATBLK   I(*)   Header of map file
C      LF1      I      Initial freq to use
C      LF2      I      Last freq to use
C      NX       I      Number X pixels (celestial)
C      NY       I      Number Y pixels (celestial)
C      JBUFSZ   I      Buffer size in aips "bytes"
C   Output:
C      IMAGE    R(*)   Memory enough for full image
C      WEIGHT   D(*)   Weights to use
C      BUFF     R(*)   Work buffer for I/O
C      IRET     I      Error code from MDISK.
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, CATBLK(256), LF1, LF2, NX, NY, JBUFSZ, IRET
      REAL      IMAGE(NX,NY,*), BUFF(*)
      DOUBLE PRECISION WEIGHT(*)
C
      INTEGER   NITER
      PARAMETER (NITER=8)
C
      INTEGER   IWIN(4), BOI, IDEPTH(5), IX, IY, LF, NF, NA, IBIND, IT
      REAL      VP, VM, TEMP, WS(NITER)
      DOUBLE PRECISION SV, SSV, NV, DTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
C                                       read in full cube
C                                       doing transpose
      CALL FILL (5, 1, IDEPTH)
      NA = CATBLK(KINAX)
      NF = LF2 - LF1 + 1
      IWIN(1) = LF1
      IWIN(3) = LF2
      IWIN(2) = 1
      IWIN(4) = NX
      DTEMP = 0.0D0
      DO 50 IY = 1,NY
         IDEPTH(1) = IY
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, BOI, IRET)
         BOI = BOI + 1
         CALL MINIT ('READ', LUN, IND, NA, NX, IWIN, BUFF, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
            GO TO 990
            END IF
         DO 40 IX = 1,NX
            CALL MDISK ('READ', LUN, IND, BUFF, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
               GO TO 990
               END IF
            DO 30 LF = 1,NF
               IMAGE(IX,IY,LF) = BUFF(IBIND+LF-1)
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C                                       find rms 1 plane at a time
      DO 150 LF = 1,NF
         VP = 1.E5
         VM = -1.E5
         DO 140 IT = 1,NITER
            SV = 0.0D0
            SSV = 0.0D0
            NV = 0.0D0
            DO 130 IY = 1,NY
               DO 120 IX = 1,NX
                  TEMP = IMAGE(IX,IY,LF)
                  IF ((TEMP.NE.FBLANK) .AND. (TEMP.NE.0.0)) THEN
                     IF ((TEMP.GT.VM) .AND. (TEMP.LT.VP)) THEN
                        DTEMP = TEMP
                        SV = SV + DTEMP
                        SSV = SSV + DTEMP * DTEMP
                        NV = NV + 1.0D0
                        END IF
                     END IF
 120              CONTINUE
 130           CONTINUE
            IF (NV.GT.0.0D0) THEN
               SV = SV / NV
               SSV = SSV / NV - SV * SV
               SSV = SQRT (MAX (0.0D0, SSV))
               IF (IT.LT.NITER) THEN
                  VP = SV + WS(IT+1) * SSV
                  VM = SV - WS(IT+1) * SSV
                  END IF
            ELSE
               VP = 1.E4
               VM = -1.E4
               END IF
 140        CONTINUE
C                                       return answers
         WEIGHT(LF) = 0.0D0
         IF (SSV.GT.0.0D0) WEIGHT(LF) = 1.0D0 / (SSV * SSV)
 150     CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMSFIT: ERROR',I5,' ON ',A)
      END
      SUBROUTINE RMFIDO (IRET)
C-----------------------------------------------------------------------
C   RMFIDO goes through the table on a IYINC, IZINC stride, sends
C   data to fitting routine using the input initial guess.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'RMFITD.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI(5), NYI, NXI, WINI(4), BOI, I1, IPOS(7),
     *   BOTEMP, IBIND(5), INDI(5), LIM1, IG, NCMP, IY, IZ, LRMRNO,
     *   FIRSTY, XXPOS(2), I, WINF(4), NXF, NYF, MCMP
      REAL      RESULT(MAXPRM,2), IAVG, PAVG, IAVT, PAVT, TMPRMS(2,2)
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, FIRSTZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16,17,18,19,20/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      DO 10 I = 1,2
         CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
 10      CONTINUE
C                                       Setup for I/O
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (UBLC(1))
      WINI(2) = IROUND (UBLC(2))
      WINI(3) = IROUND (UTRC(1))
      WINI(4) = IROUND (UTRC(2))
C                                       the FARS images
      DO 15 I = 4,5
         CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
 15      CONTINUE
C                                       Setup for I/O
      NXF = CATOLD(KINAX,4)
      NYF = CATOLD(KINAX+1,4)
      WINF(1) = 1
      WINF(2) = IROUND (BLC(2))
      WINF(3) = NXF
      WINF(4) = IROUND (TRC(2))
C                                       Initial guess
      CALL DFILL (MAXPRM, 0.0D0, PARMS)
      CALL DFILL (MAXPRM, 0.0D0, UPARMS)
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
      IF (SPIXDO.GE.2) THEN
         DO 16 I = 1,MAXCMP
            UPARMS(4*I) = 10.0D0
 16         CONTINUE
         END IF
      IG = 4 * NCOMPS
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      IF ((IYINC.GT.1) .OR. (IZINC.GT.1)) THEN
         WRITE (MSGTXT,1055) IYINC, IZINC
      ELSE
         MSGTXT = 'RMFIDO: solving spectra at every pixel'
         END IF
      CALL MSGWRT (2)
C                                       Setup for looping
C                                       Loop
      LIM1 = UTRC(1) - UBLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
      IPOS(1) = UBLC(1) + 0.01
      FIRSTY = 0
      IRMRNO = 1
      DO 200 IZ = LBLC(2),LTRC(2),IZINC
         FIRSTZ = .TRUE.
         DO 190 IY = LBLC(1),LTRC(1),IYINC
            IRMRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LRMRNO = IRMRNO
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, IPOS(2),
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NCMP.LE.0) .AND.
     *         (IAVG.GE.ICUT) .AND. (PAVG.GE.PCUT)) THEN
C                                       find last nearby solution
               IF ((FIRSTZ) .AND. (FIRSTY.GT.0) .AND. (NCOMPS.GT.1))
     *            THEN
                  IRMRNO = (IZ-IBLC(2)-IZINC) * (ITRC(1)-IBLC(1)+1) +
     *               FIRSTY - IBLC(1) + 1
                  CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *               XXPOS, MCMP, IAVT, PAVT, RESULT, TMPRMS, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
                     GO TO 990
                     END IF
                  DO 20 I1 = 1,MAXPRM
                     IF (RESULT(I1,1).NE.FBLANK) XPARMS(I1) =
     *                  RESULT(I1,1)
 20                  CONTINUE
                  FIRSTY = 0
                  END IF
               FIRSTZ = .FALSE.
C                                       Init. files, first input.
               CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = IPOS(2)
               WINI(4) = IPOS(2)
               DO 30 I = 1,2
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI, WINI,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                     GO TO 990
                     END IF
 30               CONTINUE
C                                       Copy to buffer.
               DO 35 I1 = 1,LIM1
                  QDATA(I1) = BUFF1(IBIND(1)+I1-1)
                  UDATA(I1) = BUFF2(IBIND(2)+I1-1)
 35               CONTINUE
C                                       FARS
               CALL COMOFF (CATOLD(KIDIM,4), CATOLD(KINAX,4), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINF(2) = IPOS(2)
               WINF(4) = IPOS(2)
               DO 40 I = 4,5
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXF, NYF, WINF,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ FARS IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING FARS IMAGE'
                     GO TO 990
                     END IF
 40               CONTINUE
C                                       Copy to buffer.
               DO 45 I1 = 1,NXF
                  REDATA(I1) = BUFFS(IBIND(4)+I1-1,4)
                  IMDATA(I1) = BUFFS(IBIND(5)+I1-1,5)
 45               CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NCMP, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI(1), INDI(1), I1)
                  CALL ZCLOSE (LUNI(2), INDI(2), I1)
                  CALL ZCLOSE (LUNI(4), INDI(4), I1)
                  CALL ZCLOSE (LUNI(5), INDI(5), I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
               IF ((RESULT(1,1).NE.FBLANK) .AND. (FIRSTY.EQ.0))
     *            FIRSTY = IY
               IRMRNO = LRMRNO
               CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *            IPOS(2), NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
                  GO TO 990
                  END IF
               DO 50 I1 = 1,MAXPRM
                  IF (RESULT(I1,1).NE.FBLANK) UPARMS(I1) = RESULT(I1,1)
 50               CONTINUE
C                                       pick up good solution as last
            ELSE IF (IRET.EQ.0) THEN
               DO 120 I1 = 1,MAXPRM
                  IF (RESULT(I1,1).NE.FBLANK) PARMS(I1) = RESULT(I1,1)
 120              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI(1), INDI(1), IRET)
      CALL ZCLOSE (LUNI(2), INDI(2), IRET)
      CALL ZCLOSE (LUNI(4), INDI(4), IRET)
      CALL ZCLOSE (LUNI(5), INDI(5), IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFIDO: ERROR',I3,' ON ',A)
 1055 FORMAT ('RMFIDO: solving Components every',I3,' Y pixels and',I3,
     *   ' Z pixels')
 1180 FORMAT ('RMFIDO: DO1FIT ERROR',I3)
      END
      SUBROUTINE RMFID1 (IRET)
C-----------------------------------------------------------------------
C   RMFID1 goes through the table on a stride of 1, sends data to
C   fitting routine gets the initial guess from fit pixels.
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI(5), NYI, NXI, WINI(4), BOI, I1, IPOS(7),
     *   BOTEMP, IBIND(5), INDI(5), LIM1, IG, NCMP, LRMRNO, FIRSTY, IY,
     *   IZ, XXPOS(2), I, NXF, NYF, WINF(4), MCMP
      REAL      RESULT(MAXPRM,2), IAVG, PAVG, IAVT, PAVT, TMPRMS(2,2)
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F, FIRSTZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16,17,18,19,20/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (ICODE.GE.1) THEN
         MSGTXT = 'TV turned back on for this step'
         IF (TVSUP.EQ.1) CALL MSGWRT (2)
         TVSUP = 0
         END IF
C                                       Open and init for read
      DO 10 I = 1,2
         CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
 10      CONTINUE
C                                       Setup for I/O
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (UBLC(1))
      WINI(2) = IROUND (UBLC(2))
      WINI(3) = IROUND (UTRC(1))
      WINI(4) = IROUND (UTRC(2))
C                                       the FARS images
      DO 15 I = 4,5
         CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
            GO TO 990
            END IF
 15      CONTINUE
C                                       Setup for I/O
      NXF = CATOLD(KINAX,4)
      NYF = CATOLD(KINAX+1,4)
      WINF(1) = 1
      WINF(2) = IROUND (UBLC(2))
      WINF(3) = NXF
      WINF(4) = IROUND (UTRC(2))
C                                       Initial guess
      CALL DFILL (MAXPRM, 0.0D0, PARMS)
      CALL DFILL (MAXPRM, 0.0D0, UPARMS)
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
      IG = 4 * NCOMPS
      CALL COPY (MAXPRM, DOCOMP, LLCOMP)
      MSGTXT = 'RMFID1: solving Components at every pixel not yet done'
      CALL MSGWRT (2)
C                                       Setup for looping
C                                       Loop
      LIM1 = UTRC(1) - UBLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
      IPOS(1) = UBLC(1) + 0.01
      FIRSTY = 0
      IRMRNO = 1
      DO 200 IZ = LBLC(2),LTRC(2)
         FIRSTZ = .TRUE.
         DO 190 IY = LBLC(1),LTRC(1)
            IRMRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            LRMRNO = IRMRNO
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, IPOS(2),
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NCMP.LE.0) .AND.
     *         (IAVG.GE.ICUT) .AND. (PAVG.GE.PCUT)) THEN
C                                       Init. files, first input.
               IF ((FIRSTZ) .AND. (FIRSTY.GT.0) .AND. (NCOMPS.GT.1))
     *            THEN
                  IRMRNO = (IZ-IBLC(2)-1) * (ITRC(1)-IBLC(1)+1) +
     *               FIRSTY - IBLC(1) + 1
                  CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *               XXPOS, MCMP, IAVT, PAVT, RESULT, TMPRMS, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
                     GO TO 990
                     END IF
                  DO 20 I1 = 1,MAXPRM
                     IF (RESULT(I1,1).NE.FBLANK) XPARMS(I1) =
     *                  RESULT(I1,1)
 20                  CONTINUE
                  FIRSTY = 0
                  END IF
               FIRSTZ = .FALSE.
C                                       Init. files, first input.
               CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = IPOS(2)
               WINI(4) = IPOS(2)
               DO 30 I = 1,2
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI, WINI,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                     GO TO 990
                     END IF
 30               CONTINUE
C                                       Copy to buffer.
               DO 35 I1 = 1,LIM1
                  QDATA(I1) = BUFF1(IBIND(1)+I1-1)
                  UDATA(I1) = BUFF2(IBIND(2)+I1-1)
 35               CONTINUE
C                                       FARS
               CALL COMOFF (CATOLD(KIDIM,4), CATOLD(KINAX,4), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINF(2) = IPOS(2)
               WINF(4) = IPOS(2)
               DO 40 I = 4,5
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXF, NYF, WINF,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ FARS IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING FARS IMAGE'
                     GO TO 990
                     END IF
 40               CONTINUE
C                                       Copy to buffer.
               DO 45 I1 = 1,NXF
                  REDATA(I1) = BUFFS(IBIND(4)+I1-1,4)
                  IMDATA(I1) = BUFFS(IBIND(5)+I1-1,5)
 45               CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NCMP, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI(1), INDI(1), I1)
                  CALL ZCLOSE (LUNI(2), INDI(2), I1)
                  CALL ZCLOSE (LUNI(4), INDI(4), I1)
                  CALL ZCLOSE (LUNI(5), INDI(5), I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
               IF ((RESULT(3,1).NE.FBLANK) .AND. (FIRSTY.EQ.0))
     *            FIRSTY = IY
               IRMRNO = LRMRNO
               DONROW = LRMRNO
               CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *            IPOS(2), NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
                  GO TO 990
                  END IF
               DO 50 I1 = 1,MAXPRM
                  IF (RESULT(I1,1).NE.FBLANK) UPARMS(I1) = RESULT(I1,1)
 50               CONTINUE
            ELSE IF (IRET.EQ.0) THEN
               DONROW = LRMRNO
C                                       pick up good solution as last
               DO 120 I1 = 1,MAXPRM
                  IF (RESULT(I1,1).NE.FBLANK) PARMS(I1) = RESULT(I1,1)
 120              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI(1), INDI(1), IRET)
      CALL ZCLOSE (LUNI(2), INDI(2), IRET)
      CALL ZCLOSE (LUNI(4), INDI(4), IRET)
      CALL ZCLOSE (LUNI(5), INDI(5), IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFID1: ERROR',I3,' ON ',A)
 1180 FORMAT ('RMFID1: DO1FIT ERROR',I3)
      END
      SUBROUTINE DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NCMP, RESULT,
     *   IRET)
C-----------------------------------------------------------------------
C   DO1FIT fits Components to a row of an image and returns the
C   answers in RESULT.
C   Inputs:
C      IPOS     I(7)    BLC (input image) of first value in DATA
C      UPARMS   D(16)   Initial guess (input by user)
C      XPARMS   D(16)   Last fit in row below (0 -> do not use)
C   Values from commons:
C      DATA     D(*)    Input row, magic value blanked.
C      FBLANK   R       Value of blanked pixel.
C      CATBLK   I       Output catalog header (also CATR, CATD)
C      CATOLD   I       Input catalog header (also OLDR, OLDD)
C   In/out:
C      PARMS    D(16)   In: last answer, Out: Answer in fitting units
C   Output:
C      NCMP     I       Actual number components fit
C      RESULT   R(32)   Output row (parameter answers, errors).
C      IRET     I       Return code   0 => OK
C                               >0 => error, terminate.
C   Output in COMMON
C      CATBLK   I       Catalog header block - revised for slice header
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), NCMP, IRET
      REAL      RESULT(*)
      DOUBLE PRECISION UPARMS(*), PARMS(*), XPARMS(*)
C
      EXTERNAL  RMFUNC
C
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER   INFO, IPVT(MAXPRM), ING, INPARM, INPTS, LERR,
     *   TERR, LCODE, IERR, NTRY, ITRY, LCOMPS, I, J, K, LNPTS
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), TOL, FVEC(2*NPLIM),
     *   VALVAR(MAXPRM)
      INTEGER   JNPTS, JNPARM
      LOGICAL   REDO, SKIP1
      REAL      QURANG(2,2)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      NTRY = 0
C                                       real/imag switch
      NOWREI = WASREI
      CALL RFILL (4, 0.0, QURANG)
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         REDO = .FALSE.
         SKIP1 = .FALSE.
         LCOMPS = NCOMPS
C                                       Get the initial guess
 10      NTRY = NTRY + 1
         LCODE = ICODE
         IF (TVSUP.GT.0) LCODE = 0
         ING = LCOMPS
         NCMP = LCOMPS
         INPARM = 4 * LCOMPS
         INPTS = UTRC(1) - UBLC(1) + 1.01
         LNPTS = CATOLD(KINAX,4)
         ITTER = 0
         NITTER = XNIT + 1.01
         CALL RMFIGE (LCOMPS, INPTS, PCUT, UPARMS, XPARMS, DOCOMP,
     *      GCODE, PARMS, FVEC, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Plot it
         IF (LCODE.GE.1) THEN
            ITRY = 0
 20         ITRY = ITRY + 1
            CALL RTVINI (REDO, SKIP1, IPOS, INPTS, ING, QURANG, PARMS,
     *         TERR)
            REDO = .FALSE.
            SKIP1 = .FALSE.
            IF ((TERR.GT.0) .AND. (TERR.LT.100)) THEN
               IRET = TERR
               WRITE (MSGTXT,1000) IRET, 'TV ERROR IN RTVINI'
               GO TO 990
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 990
            ELSE IF (TERR.EQ.101) THEN
               GO TO 900
C                                       Redo guess
            ELSE IF (TERR.GE.100) THEN
               TERR = 0
               LNPTS = CATOLD(KINAX,4)
               CALL GTVGUS (ING, PARMS, TERR)
               IF (TERR.GT.0) THEN
                  IRET = TERR
                  WRITE (MSGTXT,1000) IRET, 'TV ERROR IN GTVGUS'
                  GO TO 990
                  END IF
               IF (ITRY.LT.5) THEN
                  SKIP1 = .TRUE.
                  GO TO 20
                  END IF
               END IF
            END IF
C                                       index
         NVAR = 0
         K = 0
         DO 50 I = 1,ING
            DO 40 J = 1,4
               K = K + 1
               IF (LLCOMP(K).GT.0) THEN
                  NVAR = NVAR + 1
                  IVAR(NVAR) = I
                  JVAR(NVAR) = J
                  IF ((J.EQ.4) .AND. (SPIXDO.GE.2)) THEN
                     IF (PARMS(K).EQ.0.0D0) PARMS(K) = 5.0D0
                     END IF
                  VALVAR(NVAR) = PARMS(K)
                  END IF
 40            CONTINUE
 50         CONTINUE
C                                       Fit Components
         TOL = 1.D-5
         JNPTS = 2 * INPTS
         JNPARM = NVAR
         CALL RMALMS (RMFUNC, JNPTS, JNPARM, VALVAR, FVEC, FJAC, MAXPRM,
     *      TOL, INFO, IPVT)
         IF (INFO.EQ.-1) THEN
            MSGTXT = 'NUMBER OF ITERATIONS EXCEEDED WHEN TRYING TO FIT'
         ELSE
            WRITE (MSGTXT,1020) INFO
            END IF
         IF ((INFO.LE.0) .OR. (INFO.GT.3)) CALL MSGWRT (6)
         IF ((INFO.EQ.0) .OR. (INFO.EQ.4)) GO TO 900
C                                       Get errors and nice units
         ITTER  = ITTER - 1
         JNPTS = 2 * INPTS
         INPARM = 4 * ING
         JNPARM = NVAR
         CALL REDOAN (JNPTS, JNPARM, VALVAR, PARMS, FVEC, FJAC, MAXPRM,
     *      RESULT)
         CALL RMFICH (ING, INPTS, FVEC, PARMS, LERR)
C                                       non-interactive: drop bad
         IF (LCODE.LT.1) THEN
C                                       restart TV
            IF (LERR.NE.0) THEN
               IF (ICODE.GE.1) THEN
                  TVSUP = 0
                  LCODE = ICODE
                  MSGTXT = 'Restart TV because of failure'
                  CALL MSGWRT (2)
                  WRITE (MSGTXT,1101) THERMS(1,1), THERMS(2,1)
                  CALL MSGWRT (3)
                  J = 1
                  DO 100 I = 1,ING
                     WRITE (MSGTXT,1100) I, PARMS(J), PARMS(J+1),
     *                  PARMS(J+2), PARMS(J+3)
                     CALL MSGWRT (3)
                     J = J + 4
 100                 CONTINUE
                  REDO = .TRUE.
                  GO TO 10
C                                       null solution
               ELSE
                  GO TO 900
                  END IF
               END IF
C                                       interactive
         ELSE
            CALL RTVMOD (DOTV, INPTS, ING, IPOS, NCOMPS, FVEC, QURANG,
     *         PARMS, LERR, TERR)
            IF (TERR.EQ.101) THEN
               GO TO 900
            ELSE IF (TERR.EQ.102) THEN
               IRET = 99
               GO TO 999
            ELSE IF (TERR.EQ.103) THEN
               REDO = .TRUE.
               LCOMPS = ING
               GO TO 10
            ELSE IF (TERR.EQ.105) THEN
               REDO = .TRUE.
               LCOMPS = ING
               GO TO 10
            ELSE IF (TERR.EQ.104) THEN
               TVSUP = 1
               MSGTXT = 'TV turned off until next step'
               CALL MSGWRT (2)
               END IF
C                                       PARMS may have changed
            ITTER  = ITTER - 1
            JNPTS = 2 * INPTS
            INPARM = 4 * ING
C                                       index
            NVAR = 0
            K = 0
            DO 150 I = 1,ING
               DO 140 J = 1,4
                  K = K + 1
                  IF (LLCOMP(K).GT.0) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = PARMS(K)
                     END IF
 140              CONTINUE
 150           CONTINUE
            JNPARM = NVAR
            CALL REDOAN (JNPTS, JNPARM, VALVAR, PARMS, FVEC, FJAC,
     *         MAXPRM, RESULT)
            END IF
         GO TO 999
C                                       Blank outputs
 900     CALL RFILL (2*MAXPRM, FBLANK, RESULT)
         NCMP = -1
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DO1FIT: ERROR',I4,' ON ',A)
 1020 FORMAT ('FIT ROUTINE RETURNS ERROR CODE',I3)
 1100 FORMAT ('Component',I2,F12.6,F10.2,F10.3,F9.3)
 1101 FORMAT ('RMS in Q',F11.6,'  in U',F11.6)
      END
      SUBROUTINE REDOAN (M, N, X, PARMS, FVEC, FJAC, LDFJAC, RESULT)
C-----------------------------------------------------------------------
C   REDOAN computes a new residual vector, a new covariance, and redoes
C   the answers to be stored in the table
C   Inputs:
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      X        D(N)     Vector of solutions
C      LDFJAC   I        First index of FJAC
C   Output:
C      PARMS    D(*)     all answers
C      FVEC     D(M)     Q, U residuals
C      FJAC     D(*,*)   work matrix
C      RESULT   R(*)     baseline, slope, amp, center, width...
C-----------------------------------------------------------------------
      INTEGER   M, N, LDFJAC
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), PARMS(*)
      REAL      RESULT(*)
C
      INCLUDE 'RMFITD.INC'
      INTEGER   I, J, IPVT(MAXPRM)
      DOUBLE PRECISION TEMP, WA1(MAXPRM), WA2(MAXPRM), WA3(MAXPRM),
     *   QTF(MAXPRM)
C-----------------------------------------------------------------------
C                                       get residual
      I = 1
      CALL RMFUNC (M, N, X, FVEC, FJAC, I)
C                                       determine IPVT, FJAC
      DO 20 J = 1,N
         IPVT(J) = J
         DO 10 I = 1,N
            FJAC(I,J) = 0.0D0
 10         CONTINUE
 20      CONTINUE
      J = 2
      DO 30 I = 1,M
         CALL RMFUNC (M, N, X, FVEC, WA3, J)
         TEMP = FVEC(I) / WEIGHT(I)
         CALL RWUPDT (N, FJAC, LDFJAC, WA3, QTF, TEMP, WA1, WA2)
         J = J + 1
 30      CONTINUE
C                                       fit array into actual PARMS
      CALL DFILL (MAXPRM, 0.0D0, PARMS)
      DO 40 I = 1,N
         J = 4 * (IVAR(I) - 1) + JVAR(I)
         PARMS(J) = X(I)
 40      CONTINUE
C                                       Get errors and squirrel away
      CALL RMFIFI (M, N, PARMS, IPVT, FJAC, FVEC, RESULT)
C
 999  RETURN
      END
      SUBROUTINE RMALMS (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO,
     *   IPVT)
C-----------------------------------------------------------------------
C   RMALMS provides an extra interface to the math routine LMSTR1
C   and holds the WORK array (for overlay purposes)
C   Inputs:
C      FCN      EXT      Function to evaluate the model
C      M        I        Number data points (adj. array dim.)
C      N        I        Number of unknowns (adj. array dim.)
C      LDFJAC   I        Number points on first axis of FJAC (adj.
C                           array dim.)
C      TOL      D        Tolerance desired
C   In/out:
C      X        D(N)     Initial guess/ answer
C      FVEC     D(M)     Function (Data - model) evaluation
C      FJAC     D(N,N)   Work matrix
C      INFO     I        Error code: 1 - 3 good, 0 bad input,
C                           4 orthogonal, 5 - 7 poor fit
C      IPVT     D(N)     Permutation matrix
C   See precursor remarks to LMSTR1 or LMSTR for details.
C-----------------------------------------------------------------------
      EXTERNAL  FCN
      INTEGER   M, N, LDFJAC, INFO, IPVT(N)
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N), TOL
C
      INTEGER   LWA
      DOUBLE PRECISION WA(10000)
      DATA LWA /10000/
C-----------------------------------------------------------------------
C                                       It's just a dummy routine
      CALL LMSTR1 (FCN, M, N, X, FVEC, FJAC, LDFJAC, TOL, INFO, IPVT,
     *   WA, LWA)
C
 999  RETURN
      END
      SUBROUTINE RMFIGE (NG, ND, FC, UPARMS, XPARMS, DOCOMP, GCODE,
     *   RPARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   RMFIGE obtains an initial guess for the parameters on the
C   Components.  For a single component, it uses moments.  For multiple
C   components, it gets a baseline guess, checks the data, and chooses
C   between the last solution and the user's initial guess.
C   Inputs:
C      NG       I        Number of Components
C      ND       I        Number of data samples in Q/U spectra
C      FC       R        Flux cutoff
C      UPARMS   D(16)    User's initial guess
C      DOCOMP   I(16)    > 0 -> do the parameter
C      GCODE    I        > 0 -> there is a user initial guess
C   In.out:
C      XPARMS   D(16)    another initial guess 0.0D0 -> none
C                           RETURNS 0.0 when XPARMS was considered
C      RPARMS   D(3NG)   In: previous solution
C                        Out:Guess to use
C   Output:
C      FVEC     D(*)     Buffer for computation
C      IERR     I        0 => ok, 1 => all data too low
C                        2 => input error
C   Common output:
C      LLCOMP   I(16)    copy DOCOMP unless previous guess is
C                                 used.  Then leave it alone.
C-----------------------------------------------------------------------
      INTEGER   NG, ND, DOCOMP(*), GCODE, IERR
      REAL      FC
      DOUBLE PRECISION UPARMS(*), XPARMS(*), RPARMS(*), FVEC(*)
C
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      DOUBLE PRECISION LPARMS(MAXPRM), XR, XI, FJC(MAXPRM),
     *   VALVAR(MAXPRM)
      INTEGER   JD, IJ, IM, I, J, K, INPTS
      REAL      X, XM, TS, BL, BLP, BLM, RMS(3)
      LOGICAL   DOUP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Test inputs
      IERR = 2
      IF ((ND.LT.3*NG+6) .OR. (ND.GT.NPLIM)) GO TO 999
      DOUP = .TRUE.
      IF (GCODE.GT.0) THEN
         DO 5 I = 1,MAXPRM
            LPARMS(I) = UPARMS(I)
 5          CONTINUE
      ELSE IF (XPARMS(1).GT.0.0D0) THEN
         DO 6 I = 1,MAXPRM,4
            LPARMS(I) = 0.0D0
            LPARMS(I+1) = 0.0D0
            LPARMS(I+2) = XPARMS(I+2)
            LPARMS(I+3) = XPARMS(I+3)
 6          CONTINUE
      ELSE
         DOUP = .FALSE.
         CALL DFILL (MAXPRM, 0.0D0, LPARMS)
         END IF
C                                       Real/Imag -> Amp/phase
      INPTS = CATOLD(KINAX,4)
      IF (NOWREI) THEN
         DO 10 I = 1,INPTS
            IF ((REDATA(I).EQ.FBLANK) .OR. (IMDATA(I).EQ.FBLANK)) THEN
               AMDATA(I) = FBLANK
               PHDATA(I) = FBLANK
            ELSE
               XR = REDATA(I)
               XI = IMDATA(I)
               AMDATA(I) = SQRT (XR*XR + XI*XI)
               PHDATA(I) = 0.0D0
               IF ((XR.NE.0.0D0) .OR. (XI.NE.0.0D0)) PHDATA(I) = RAD2DG
     *            * ATAN2 (XI, XR)
               END IF
 10         CONTINUE
         NOWREI = .FALSE.
         END IF
C                                       Clear max finding variables
      IM = 0
      XM = 0.
C                                       find max
      DO 40 I = 1,INPTS
         IF (AMDATA(I).NE.FBLANK) THEN
            BL = AMDATA(I)
            IF (BL.GE.FC) THEN
               IF ((I.NE.1) .AND. (I.NE.INPTS) .AND.
     *            (AMDATA(I-1).NE.FBLANK) .AND.
     *            (AMDATA(I+1).NE.FBLANK)) THEN
                  BLM = AMDATA(I-1)
                  BLP = AMDATA(I+1)
                  IF ((BLP.GE.FC) .AND. (BLM.GE.FC)) THEN
                     X = BLP + BL + BLM
                     IF (X.GE.XM) THEN
                        XM = X
                        IM = I
                        END IF
                     END IF
                  END IF
               END IF
            END IF
 40      CONTINUE
      IF (IM.LT.1) THEN
         DO 50 I = 1,INPTS
            IF (AMDATA(I).NE.FBLANK) THEN
               BL = AMDATA(I)
               IF ((BL.GE.FC) .AND. (BL.GT.XM)) THEN
                  XM = BL
                  IM = I
                  END IF
               END IF
 50         CONTINUE
         END IF
C                                       Find anything?
      IERR = 1
      IF (IM.LT.1) GO TO 999
C                                       Yes: make a guess
      IF (NG.LE.1) THEN
         RPARMS(1) = AMDATA(IM)
         RPARMS(2) = 0.5D0 * PHDATA(IM)
         RPARMS(3) = OLDD(KDCRV,4) + (IM-OLDR(KRCRP,4)) * OLDR(KRCIC,4)
         RPARMS(4) = 0.0D0
         LLCOMP(1) = DOCOMP(1)
         LLCOMP(2) = DOCOMP(2)
         LLCOMP(3) = DOCOMP(3)
         LLCOMP(4) = DOCOMP(4)
         TS = 0.
C                                       Fill in amplitudes
      ELSE IF (DOUP) THEN
         DO 55 I = 1,NG
            K = 4 * I - 3
            IF (LPARMS(K).LE.0.0D0) THEN
               J = (LPARMS(K+2) - OLDD(KDCRV,4)) / OLDR(KRCIC,4) +
     *            OLDR(KRCRP,4) + 0.5
               IF ((J.GT.0) .AND. (J.LE.INPTS)) THEN
                  LPARMS(K) = AMDATA(J)
                  LPARMS(K+1) = 0.5D0 * PHDATA(J)
                  IF ((LPARMS(K).LT.0.0D0) .OR. (LPARMS(K).EQ.FBLANK))
     *               THEN
                     LPARMS(K) = 0.0D0
                     LPARMS(K+1) = 0.0D0
                     END IF
                  END IF
               END IF
 55         CONTINUE
         END IF
C                                       Compute the RMSs
      IF (NG.NE.1) THEN
         TS = 0.0
         J = 4 * NG
C                                       user guess
         IF (LPARMS(1).LE.0.0) THEN
            RMS(1) = 1.E10
         ELSE
            ITTER = ITTER - 1
            JD = 2 * ND
            NVAR = 0
            K = 0
            DO 74 I = 1,NG
               DO 73 J = 1,4
                  K = K + 1
                  IF (LLCOMP(K).GT.0) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = LPARMS(K)
                     END IF
 73               CONTINUE
 74            CONTINUE
            IJ = NVAR
            I = 1
            CALL RMFUNC (JD, IJ, VALVAR, FVEC, FJC, I)
            FJC(1) = 0.0D0
            DO 75 I = 1,JD
               FJC(1) = FJC(1) + FVEC(I) * FVEC(I) / WEIGHT(I)
 75            CONTINUE
            RMS(1) = SQRT (FJC(1) / JD)
            END IF
C                                       last solution
         ITTER = ITTER - 1
         JD = 2 * ND
         NVAR = 0
         K = 0
         DO 79 I = 1,NG
            DO 78 J = 1,4
               K = K + 1
               IF (LLCOMP(K).GT.0) THEN
                  NVAR = NVAR + 1
                  IVAR(NVAR) = I
                  JVAR(NVAR) = J
                  VALVAR(NVAR) = RPARMS(K)
                  END IF
 78            CONTINUE
 79         CONTINUE
         IJ = NVAR
         I = 1
         CALL RMFUNC (JD, IJ, VALVAR, FVEC, FJC, I)
         FJC(1) = 0.0D0
         DO 80 I = 1,JD
            FJC(1) = FJC(1) + FVEC(I) * FVEC(I) / WEIGHT(I)
 80         CONTINUE
         RMS(2) = SQRT (FJC(1) / JD)
C                                       extra guess
         IF (XPARMS(1).EQ.0.0D0) THEN
            RMS(3) = 1.E10
         ELSE
            ITTER = ITTER - 1
            JD = 2 * ND
            NVAR = 0
            K = 0
            DO 84 I = 1,NG
               DO 83 J = 1,4
                  K = K + 1
                  IF (LLCOMP(K).GT.0) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = XPARMS(K)
                     END IF
 83               CONTINUE
 84            CONTINUE
            IJ = NVAR
            I = 1
            CALL RMFUNC (JD, IJ, VALVAR, FVEC, FJC, I)
            FJC(1) = 0.0D0
            DO 85 I = 1,JD
               FJC(1) = FJC(1) + FVEC(I) * FVEC(I) / WEIGHT(I)
 85            CONTINUE
            RMS(3) = SQRT (FJC(1) / JD)
            END IF
         IF ((RMS(3).LT.RMS(1)) .AND. (RMS(3).LT.RMS(2))) THEN
            DO 90 I = 1,MAXPRM
               RPARMS(I) = XPARMS(I)
               LLCOMP(I) = DOCOMP(I)
 90            CONTINUE
         ELSE IF (RMS(1).LT.RMS(2)) THEN
            DO 95 I = 1,MAXPRM
               RPARMS(I) = LPARMS(I)
               LLCOMP(I) = DOCOMP(I)
 95            CONTINUE
            END IF
         END IF
C                                       Return answers/guesses
C                                       test desire to plot
      IERR = 0
      IF (RMS(3).LT.1.E9) CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C
 999  RETURN
      END
      SUBROUTINE RMFIFI (INPTS, NP, PARMS, IPVT, FJAC, FVEC, RESULT)
C-----------------------------------------------------------------------
C   RMFIFI determines the errors in the fit and converts the results
C   to normal units for output.
C   Inputs:
C      INPTS    I          Number of data samples
C      NP       I          Number of parameters
C      PARMS    D(16)      Answers from LMSTR1
C      IPVT     I(16)      from LMSTR1
C      FJAC     D(16,16)   from LMSTR1
C      FVEC     D(*)       from LMSTR1
C   Output:
C      RESULT   R(36)      Answers then errors in PIXELS
C                             4 each of 4 Components
C                             Errors: 4 ea of 4 Components
C-----------------------------------------------------------------------
      INCLUDE 'RMFITD.INC'
C
      INTEGER   INPTS, NP, IPVT(*)
      DOUBLE PRECISION PARMS(*), FJAC(MAXPRM,MAXPRM), FVEC(*)
      REAL      RESULT(*)
C
      DOUBLE PRECISION EPARMS(MAXPRM), ENORM, FNORM, TOL, WORK(MAXPRM),
     *   SFJAC(MAXPRM,MAXPRM)
      INTEGER   I, NG, JC, JNPTS, JP, JJ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Get errors with LMSTR1 outputs
      TOL = 1.D-5
      JNPTS = 2 * INPTS
      FNORM = ENORM (JNPTS, FVEC)
      JP = MAXPRM * MAXPRM
      CALL DPCOPY (JP, FJAC, SFJAC)
      JP = NP
      CALL GETERR (IPVT, SFJAC, EPARMS, JP, JNPTS, MAXPRM, FNORM, WORK,
     *   TOL, IVAR, JVAR)
C                                       Convert to output
      NG = NP / 4
      JJ = 0
      JC = 0
      CALL RFILL (2*MAXPRM, FBLANK, RESULT)
      DO 20 I = 1,MAXCMP
         IF ((LLCOMP(1+JC).GT.0) .OR. (LLCOMP(2+JC).GT.0) .OR.
     *      (LLCOMP(3+JC).GT.0) .OR. (PARMS(1+JC).NE.0.0D0)) THEN
            IF (PARMS(1+JC).LT.0.0) THEN
               PARMS(1+JC) = -PARMS(1+JC)
               PARMS(2+JC) = PARMS(2+JC) + 90.0
               END IF
            RESULT(1+JJ) = PARMS(1+JC)
 10         IF (PARMS(2+JC).GT.90.0) THEN
               PARMS(2+JC) = PARMS(2+JC) - 180.0
               GO TO 10
            ELSE IF (PARMS(2+JC).LT.-90.0) THEN
               PARMS(2+JC) = PARMS(2+JC) + 180.0
               GO TO 10
               END IF
            RESULT(2+JJ) = PARMS(2+JC)
            RESULT(3+JJ) = PARMS(3+JC)
            IF (LLCOMP(4+JJ).GT.0) THEN
               RESULT(4+JJ) = PARMS(4+JC)
            ELSE
               RESULT(4+JJ) = 0.0
               END IF
            IF ((EPARMS(1+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+1+JJ) = RESULT(MAXPRM+1+JJ-4)
            ELSE
               RESULT(MAXPRM+1+JJ) = EPARMS(1+JC)
               END IF
            IF ((EPARMS(2+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+2+JJ) = RESULT(MAXPRM+2+JJ-4)
            ELSE
               RESULT(MAXPRM+2+JJ) = EPARMS(2+JC)
               END IF
            IF ((EPARMS(3+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
               RESULT(MAXPRM+3+JJ) = RESULT(MAXPRM+3+JJ-4)
            ELSE
               RESULT(MAXPRM+3+JJ) = EPARMS(3+JC)
               END IF
            IF (LLCOMP(4+JJ).GT.0) THEN
               IF ((EPARMS(4+JC).LE.0.0D0) .AND. (I.GT.1)) THEN
                  RESULT(MAXPRM+4+JJ) = RESULT(MAXPRM+4+JJ-4)
               ELSE
                  RESULT(MAXPRM+4+JJ) = EPARMS(4+JC)
                  END IF
            ELSE
               RESULT(MAXPRM+4+JJ) = 0.0
               END IF
            END IF
         JJ = JJ + 4
         JC = JC + 4
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE GETERR (IPVT, FJAC, PARERR, MP, NDATA, MD, FNORM, WA,
     *   TOL, IVAR, JVAR)
C-----------------------------------------------------------------------
C   This subroutine calculates the errors on the fitted parameters.
C   Inputs:
C      IPVT    I(MP)   Defines a permutation matrix P such that
C                      JAC*P = Q*R, where JAC is the final calculated
C                      Jacobian, Q is orthogonal (not stored), and R is
C                      upper triangular with diagonal elements of
C                      nonincreasing magnitude column J of P is column
C                      IPVT(J) of the identity matrix. (See FJAC below)
C      FJAC    D(MD,MP)   The upper MP by MP submatrix of FJAC contains
C                      an upper triangular matrix R with diagonal
C                      elements of nonincreasing magnitude such that
C                           T     T           T
C                          P *(JAC *JAC)*P = R *R,
C                      where P is a permutation matrix and JAC is the
C                      final calculated Jacobian. Column J of P is
C                      column IPVT(J) (see above) of the identity
C                      matrix.
C      MP      I       Number of parameters in fitted function.
C      NDATA   I       Number of data points fitted.
C      MD      I       Maximum no. of data points allowed for in FJAC
C      FNORM   D       Euclidian norm of solution vector.
C      WA      D(MP)   work array.
C  Output:
C      FJAC    D       modified by COVAR
C      PARERR  D(MP)   error in fitted parameters.
C      TOL     D       tolerance used in call to LMDER1.
C-----------------------------------------------------------------------
      INTEGER   MD, MP, IPVT(MP), NDATA, IVAR(*), JVAR(*)
      DOUBLE PRECISION FJAC(MD,MP), PARERR(MP), FNORM, WA(MP), TOL
C
      DOUBLE PRECISION EPSILN
      INTEGER   J, JC
C-----------------------------------------------------------------------
C                                       Calculate error following
C                                       Argonne write up
C      NPARMS = NCOMPS * 4
C                                       Is this right ??????
C                                       changed NPARMS to MP
      EPSILN = FNORM / SQRT (REAL(NDATA-MP))
      CALL COVAR (MP, FJAC, MD, IPVT, TOL, WA)
      CALL DFILL (MD, 0.0D0, PARERR)
      DO 100 J = 1,MP
         JC = 4 * (IVAR(J) - 1) + JVAR(J)
         PARERR(JC) = EPSILN * SQRT (FJAC(J,J))
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RMFUNC (M, N, VALVAR, FVEC, FJROW, IFLAG)
C-----------------------------------------------------------------------
C   This routine is called by the Argonne package to calculate the
C   difference between the current fit and the actual data OR the
C   Jacobian for this difference.
C   Inputs:
C      M        I      Number of data points in Q plus U
C      N        I      No. of parameters (adj. array dim.;
C                        NCOMPS * 4)
C      VALVAR   D(N)   parameters of components being fitted
C                        GMAX(1), GPOS(1), GWIDTH(1), GMAX(2), ...
C      IFLAG    I      1=calculate difference for current guess.
C                        2=calculate jacobian for current guess.
C    COMMON GDATA
C      QDATA    D(*)   Original Q slice data points.
C      UDATA    D(*)   Original Q slice data points.
C      ITTER    I      number of calls to evaluate FVEC.
C   Outputs:
C      FVEC1    D(M)   Q Slice data points minus data points
C                        evaluated for current guess.
C      FVEC2    D(M)   U Slice data points minus data points
C                        evaluated for current guess.
C      FJROW1   D(N)   Row (IFLAG - 1) of Jacobian.
C-----------------------------------------------------------------------
      INTEGER   N, M, IFLAG
      DOUBLE PRECISION VALVAR(N), FVEC(M), FJROW(N)
C
      INCLUDE 'RMFITD.INC'
      DOUBLE PRECISION AMP, THETA, RM, QQ, UU, TSIN, TCOS, ALPHA, AFACT,
     *   LPARMS(MAXPRM), DSINCS, XX
      INTEGER   ID, IAMP, ITHE, IRM, IALPH, M2, I, K
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LPARMS
C-----------------------------------------------------------------------
C                                       Determine difference between
C                                       data and current fit.
      M2 = M / 2
      IF (IFLAG.LE.1) THEN
         ITTER = ITTER + 1
         IF (ITTER.GT.NITTER) THEN
            IFLAG = -1
            GO TO 999
            END IF
C                                       get full parms array
         CALL DFILL (MAXPRM, 0.0D0, LPARMS)
         DO 10 I = 1,N
            K = 4 * (IVAR(I) - 1) + JVAR(I)
            LPARMS(K) = VALVAR(I)
 10         CONTINUE
C                                       Q residual
         DO 20 ID = 1,M2
            FVEC(ID) = QDATA(ID)
            IF (FVEC(ID).EQ.FBLANK) THEN
               FVEC(ID) = 0.0D0
            ELSE
               QQ = 0.0D0
               DO 15 I = 1,N
                  IF (JVAR(I).EQ.1) THEN
                     IAMP = (IVAR(I) - 1) * 4 + 1
                     AMP = LPARMS(IAMP)
                     IF (AMP.NE.0.0D0) THEN
                        IF (SPIXDO.EQ.1) THEN
                           AMP = AMP * ((LAMSQ1/LAMSQ(ID)) **
     *                        (0.5D0 * LPARMS(IAMP+3)))
                        ELSE IF (SPIXDO.GE.2) THEN
                           XX = LPARMS(IAMP+3) * LAMSQ(ID)
                           AMP = AMP * DSINCS (SPIXDO, 0, XX)
                           END IF
                        QQ = QQ + AMP * COS (2.0D0*DG2RAD*LPARMS(IAMP+1)
     *                     + 2.0D0 * LPARMS(IAMP+2) * LAMSQ(ID))
                        END IF
                     END IF
 15               CONTINUE
               FVEC(ID) = WEIGHT(ID) * (FVEC(ID) - QQ)
               END IF
 20         CONTINUE
C                                       U residual
         DO 40 ID = M2+1,M
            FVEC(ID) = UDATA(ID-M2)
            IF (FVEC(ID).EQ.FBLANK) THEN
               FVEC(ID) = 0.0D0
            ELSE
               UU = 0.0D0
               DO 35 I = 1,N
                  IF (JVAR(I).EQ.1) THEN
                     IAMP = 4 * (IVAR(I) - 1) + 1
                     AMP = LPARMS(IAMP)
                     IF (AMP.NE.0.0D0) THEN
                        IF (SPIXDO.EQ.1) THEN
                           AMP = AMP * ((LAMSQ1/LAMSQ(ID-M2)) **
     *                        (0.5D0 * LPARMS(IAMP+3)))
                        ELSE IF (SPIXDO.GE.2) THEN
                           XX = LPARMS(IAMP+3) * LAMSQ(ID-M2)
                           AMP = AMP * DSINCS (SPIXDO, 0, XX)
                           END IF
                        UU = UU + AMP * SIN (2.0D0*DG2RAD*LPARMS(IAMP+1)
     *                     + 2.0D0 * LPARMS(IAMP+2) * LAMSQ(ID-M2))
                        END IF
                     END IF
 35                CONTINUE
               FVEC(ID) = WEIGHT(ID) * (FVEC(ID) - UU)
               END IF
 40         CONTINUE
C                                       Calculate Jacobian.
      ELSE
         ID = IFLAG - 1
         AFACT = 1.0D0
         DO 110 I = 1,N
            IAMP = 4 * IVAR(I) - 3
            ITHE = IAMP + 1
            IRM  = IAMP + 2
            IALPH = IAMP + 3
            FJROW(I) = 0.0D0
            AMP   = LPARMS(IAMP)
            THETA = LPARMS(ITHE)
            RM    = LPARMS(IRM)
            ALPHA = LPARMS(IALPH)
            IF (ID.LE.M2) THEN
               IF (SPIXDO.EQ.1) THEN
                  AFACT = (LAMSQ1 / LAMSQ(ID)) ** (0.5D0 * ALPHA)
               ELSE IF (SPIXDO.GE.2) THEN
                  XX = ALPHA * LAMSQ(ID)
                  AFACT = DSINCS (SPIXDO, 0, XX)
                  END IF
               TCOS = COS (2.0D0 * DG2RAD * THETA +
     *            2.0D0 * LAMSQ(ID) * RM)
               TSIN = SIN (2.0D0 * DG2RAD * THETA +
     *            2.0D0 * LAMSQ(ID) * RM)
               IF (JVAR(I).EQ.1) THEN
                  FJROW(I) = -TCOS * AFACT
               ELSE IF (JVAR(I).EQ.2) THEN
                  FJROW(I) = AMP * 2.0D0 * DG2RAD * TSIN * AFACT
               ELSE IF (JVAR(I).EQ.3) THEN
                  FJROW(I) = AMP * 2.0D0 * LAMSQ(ID) * TSIN * AFACT
               ELSE IF (JVAR(I).EQ.4) THEN
                  IF (SPIXDO.EQ.1) THEN
                     FJROW(I) = -AMP * AFACT * TCOS * 0.5D0 *
     *                  LOG (LAMSQ1/LAMSQ(ID))
                  ELSE IF (SPIXDO.GE.2) THEN
                     XX = ALPHA * LAMSQ(ID)
                     FJROW(I) = -AMP * TCOS * LAMSQ(ID) *
     *                  DSINCS (SPIXDO, 1, XX)
                     END IF
                  END IF
            ELSE
               IF (SPIXDO.EQ.1) THEN
                  AFACT = (LAMSQ1 / LAMSQ(ID-M2)) ** (0.5D0 * ALPHA)
               ELSE IF (SPIXDO.GE.2) THEN
                  XX = ALPHA * LAMSQ(ID-M2)
                  AFACT = DSINCS (SPIXDO, 0, XX)
                  END IF
               TCOS = COS (2.0D0 * DG2RAD * THETA +
     *            2.0D0 * LAMSQ(ID-M2) * RM)
               TSIN = SIN (2.0D0 * DG2RAD * THETA +
     *            2.0D0 * LAMSQ(ID-M2) * RM)
               IF (JVAR(I).EQ.1) THEN
                  FJROW(I) = -TSIN * AFACT
               ELSE IF (JVAR(I).EQ.2) THEN
                  FJROW(I) = -AMP * 2.0D0 * DG2RAD * TCOS * AFACT
               ELSE IF (JVAR(I).EQ.3) THEN
                  FJROW(I) = -AMP * 2.0D0 * LAMSQ(ID-M2) * TCOS * AFACT
               ELSE IF (JVAR(I).EQ.4) THEN
                  IF (SPIXDO.EQ.1) THEN
                     FJROW(I) = -AMP * AFACT * TSIN * 0.5D0 *
     *                  LOG (LAMSQ1/LAMSQ(ID-M2))
                  ELSE IF (SPIXDO.GE.2) THEN
                     XX = ALPHA * LAMSQ(ID-M2)
                     FJROW(I) = -AMP * TSIN * LAMSQ(ID-M2) *
     *                  DSINCS (SPIXDO, 1, XX)
                     END IF
                  END IF
               END IF
 110        CONTINUE
         END IF
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION DSINCS (MODEL, TYPE, XX)
C-----------------------------------------------------------------------
C   returns sin(xx)/xx on type 0 and d (sin(xx)/xx) / dx on type 1
C   Inputs:
C      MODEL   I   2 - sin(x)/x, 3 Gauss, 4 exp
C      TYPE    I   0 - return function, 1 return derivative
C      XX      D   argument
C   Output
C     DSINCS   D   function value or derivative
C-----------------------------------------------------------------------
      INTEGER   MODEL, TYPE
      DOUBLE PRECISION XX
C
      DOUBLE PRECISION F
C                                       1.8954 radians is the half-power
C                                       of sin(x) / x
C-----------------------------------------------------------------------
      DSINCS = 0.0D0
C                                       SIN(X)/X function
      IF (MODEL.EQ.2) THEN
         IF (TYPE.EQ.0) THEN
            IF (ABS(XX).LE.1.D-5) THEN
               DSINCS = 1.0D0 - XX*XX / 6.0D0 + XX*XX*XX*XX / 120.0D0
            ELSE
               DSINCS = SIN (XX) / XX
               END IF
C                                       SIN(X)/X derivative
         ELSE IF (TYPE.EQ.1) THEN
            IF (ABS(XX).LE.1.D-5) THEN
               DSINCS = -XX/3.D0 + XX*XX*XX/30.D0 -
     *            XX*XX*XX*XX*XX/840.D0
            ELSE
               DSINCS = (XX * COS(XX) - SIN(XX)) / (XX*XX)
               END IF
            END IF
C                                       Gauss function
      ELSE IF (MODEL.EQ.3) THEN
         F = -LOG (2.0D0) / (1.8954 * 1.8954)
         IF (TYPE.EQ.0) THEN
            DSINCS = EXP (F * XX * XX)
C                                       Gauss derivative
         ELSE IF (TYPE.EQ.1) THEN
            DSINCS = 2.0 * XX * F * EXP (F * XX * XX)
            END IF
C                                       Exp function
      ELSE IF (MODEL.EQ.4) THEN
         F = -LOG (2.0D0) / 1.8954
         IF (TYPE.EQ.0) THEN
            DSINCS = EXP (F * ABS(XX))
C                                       Exp derivative
         ELSE IF (TYPE.EQ.1) THEN
            DSINCS = F * EXP (F * ABS(XX))
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE RTVINI (REDO, SKIP1, IPOS, INPTS, NG, QURANG, PARMS,
     *   IERR)
C-----------------------------------------------------------------------
C   RTVINI initializes the TV for a RMFIT plot, plots axis labels,
C   and, if requested, plots the data.
C   Inputs:
C      REDO     L        If true, will always want to make a new guess
C      SKIP1    L        If true, omit first plot and question
C      IPOS     I(7)     Position in cube first point in row.
C      INPTS    I        Number of points in row.
C      NG       I        Number of Components
C      PARMS    D(16)    Initial guess
C   Output:
C      IERR     I        > 0 => plot failed
C                        101 => bad initial guess
C                        102 => DIE
C   Output in COMMON:
C      ORANGE   R(6)     Actual plot range in plot units
C-----------------------------------------------------------------------
      LOGICAL   REDO, SKIP1
      INTEGER   IPOS(7), INPTS, NG, IERR
      DOUBLE PRECISION PARMS(*)
      REAL      QURANG(2,2)
C
      CHARACTER TEMP*1, MSGBUF*132, FIRSTC*1
      REAL      XFAC, PHS
      INTEGER   I, JERR, LNPTS, INBUF(256)
      LOGICAL   T, F, FIRST
      DOUBLE PRECISION XR, XI
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      SAVE FIRST
      DATA T, F, FIRST /.TRUE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      CALL YHOLD ('ONNN', IERR)
      IF (FIRST) THEN
         IGR1 = 1 + NGRAY
         IGR2 = 2 + NGRAY
         IGR3 = 3 + NGRAY
         IGR4 = 4 + NGRAY
         IGR5 = 5 + NGRAY
         DO 5 I = 1,NGRAY+NGRAPH
            CALL YSLECT ('OFFF', I, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 990
 5          CONTINUE
         FIRST = .FALSE.
         END IF
      CALL YSLECT ('ONNN', IGR1, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (IGR1.NE.IGR2) THEN
         CALL YSLECT ('ONNN', IGR2, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YSLECT ('ONNN', IGR3, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YSLECT ('ONNN', IGR4, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       Default: actual range
      ORANGE(1,1) = 1.0E10
      ORANGE(2,1) = -ORANGE(1,1)
      ORANGE(1,2) = 1.0E10
      ORANGE(2,2) = -ORANGE(1,2)
      ORANGE(1,3) = 1.0E10
      ORANGE(2,3) = -ORANGE(1,3)
      ORANGE(1,4) = 1.0E10
      ORANGE(2,4) = -ORANGE(1,4)
      ORANGE(1,5) = 1.0E10
      ORANGE(2,5) = -ORANGE(1,5)
      IF (QURANG(2,1).LE.QURANG(1,1)) THEN
         DO 10 I = 1,INPTS
            IF (QDATA(I).NE.FBLANK) THEN
               IF (QDATA(I).LT.ORANGE(1,1)) ORANGE(1,1) = QDATA(I)
               IF (QDATA(I).GT.ORANGE(2,1)) ORANGE(2,1) = QDATA(I)
               END IF
 10         CONTINUE
      ELSE
         ORANGE(1,1) = QURANG(1,1)
         ORANGE(2,1) = QURANG(2,1)
         END IF
      IF (QURANG(2,2).LE.QURANG(1,2)) THEN
         DO 15 I = 1,INPTS
            IF (UDATA(I).NE.FBLANK) THEN
               IF (UDATA(I).LT.ORANGE(1,2)) ORANGE(1,2) = UDATA(I)
               IF (UDATA(I).GT.ORANGE(2,2)) ORANGE(2,2) = UDATA(I)
               END IF
 15         CONTINUE
      ELSE
         ORANGE(1,2) = QURANG(1,2)
         ORANGE(2,2) = QURANG(2,2)
         END IF
      LNPTS = CATOLD(KINAX,4)
      IF (NOWREI) THEN
         DO 20 I = 1,LNPTS
            IF ((REDATA(I).NE.FBLANK) .AND. (IMDATA(I).NE.FBLANK)) THEN
               XR = REDATA(I)
               XI = IMDATA(I)
               AMDATA(I) = SQRT (XR*XR + XI*XI)
               PHDATA(I) = 0.0D0
               IF ((XR.NE.0.0D0) .OR. (XI.NE.0.0D0)) PHDATA(I) = RAD2DG
     *            * ATAN2 (XI, XR)
            ELSE
               AMDATA(I) = FBLANK
               PHDATA(I) = FBLANK
               END IF
 20         CONTINUE
         END IF
      DO 30 I = 1,LNPTS
         IF ((AMDATA(I).NE.FBLANK) .AND. (PHDATA(I).NE.FBLANK)) THEN
            IF (AMDATA(I).LT.ORANGE(1,3)) ORANGE(1,3) = AMDATA(I)
            IF (AMDATA(I).GT.ORANGE(2,3)) ORANGE(2,3) = AMDATA(I)
            PHS = PHDATA(I)
            IF (PHS.LT.ORANGE(1,4)) ORANGE(1,4) = PHS
            IF (PHS.GT.ORANGE(2,4)) ORANGE(2,4) = PHS
            IF (PHS.LT.0.0) PHS = PHS + 360.0
            IF (PHS.LT.ORANGE(1,5)) ORANGE(1,5) = PHS
            IF (PHS.GT.ORANGE(2,5)) ORANGE(2,5) = PHS
            END IF
 30      CONTINUE
      DO 40 I = 1,5
         XFAC = ORANGE(2,I) - ORANGE(1,I)
         ORANGE(2,I) = ORANGE(2,I) + 0.075 * XFAC
         ORANGE(1,I) = ORANGE(1,I) - 0.075 * XFAC
 40      CONTINUE
C                                       start with amp/phase of FARS
C                                       clear screen
      IF (SKIP1) THEN
         IERR = 0
      ELSE
         CALL YZERO (IGR1, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (IGR2.NE.IGR1) THEN
            CALL YZERO (IGR2, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YZERO (IGR3, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YZERO (IGR4, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         CALL COPY (256, CATOLD(1,4), CATBLK)
         CALL RTVPLT (.FALSE., LNPTS, NG, IPOS, PARMS, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YHOLD ('OFFF', IERR)
C                                       Talk to user
         IF (REDO) THEN
            IERR = 103
         ELSE
            IF (DOTV.GT.1.5) THEN
               CALL RMMEN1 (MSGBUF, SCRTCH, JERR)
            ELSE
               IF (TTYIND.LE.0) THEN
                  CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
                  IF (JERR.NE.0) THEN
                     WRITE (MSGTXT,1900) JERR
                     CALL MSGWRT (6)
                     GO TO 990
                     END IF
                  END IF
               WRITE (MSGBUF,1910)
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
               IF (JERR.NE.0) GO TO 920
               MSGBUF = '??? hit return or anything else to procede'
               CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
               IF (JERR.NE.0) GO TO 920
               CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
               END IF
            IF (JERR.NE.0) GO TO 920
            TEMP = FIRSTC (MSGBUF)
            IF (TEMP.EQ.'B') THEN
               IERR = 101
            ELSE IF (TEMP.EQ.'Q') THEN
               IERR = 102
            ELSE IF (TEMP.EQ.'E') THEN
               IERR = 103
               END IF
            END IF
         END IF
C                                       If not making a guess
C                                       plot Q, U
C                                       clear screen
      IF ((IERR.LT.101) .OR. (IERR.GT.103)) THEN
         JERR = IERR
         CALL YHOLD ('ONNN', IERR)
         CALL YZERO (IGR1, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (IGR2.NE.IGR1) THEN
            CALL YZERO (IGR2, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YZERO (IGR3, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YZERO (IGR4, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         CALL COPY (256, CATOLD(1,1), CATBLK)
         CALL RTVPLT (.TRUE., INPTS, NG, IPOS, PARMS, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YHOLD ('OFFF', IERR)
         IERR = JERR
         END IF
      GO TO 990
C                                       TTY error
 920  WRITE (MSGTXT,1920) JERR
      CALL MSGWRT (6)
 990  IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         TTYIND = 0
         END IF
      CALL YHOLD ('OFFF', I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('??? Type ''E'' to enter a guess, ''BAD'' to flag row,',
     *   ' ''Q'' to quit')
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE RMMEN1 (MSGBUF, SCRTCH, JERR)
C-----------------------------------------------------------------------
C   Does a TV menu for initial guess stage of user questions
C   Outputs:
C      MSGBUF   C*(*)   answer: E, B, Q, other
C      JERR     I       error
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), JERR
      CHARACTER MSGBUF*(*)
C
      INTEGER   MTYPE, NCOL, NROWS, GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE
      LOGICAL   LEAVE(6)
      CHARACTER CHOICS(6)*8, TITLE*8, ISHELP*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICS /'DO FIT', ' ', 'RE-GUESS', 'BAD', ' ', 'QUIT'/
      DATA LEAVE /.FALSE., .TRUE., 2*.FALSE., .TRUE., .FALSE./
      DATA ISHELP /'XGAUS'/
C-----------------------------------------------------------------------
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      NROWS = 6
      SIDSEP = 6 * CSIZTV(1)
      TOPSEP = 5 * CSIZTV(2)
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'RMMEN1: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
      ELSE IF (CHOICS(CHOICE).EQ.'RE-GUESS') THEN
         MSGBUF = 'E'
      ELSE IF (CHOICS(CHOICE).EQ.'BAD') THEN
         MSGBUF = 'B'
      ELSE IF (CHOICS(CHOICE).EQ.'QUIT') THEN
         MSGBUF = 'Q'
      ELSE IF (CHOICS(CHOICE).EQ.'DO FIT') THEN
         MSGBUF = 'D'
         END IF
C
 999  RETURN
      END
      SUBROUTINE RTVPLT (DOREIM, INPTS, NG, IPOS, PARMS, IERR)
C-----------------------------------------------------------------------
C   Does the data plot - either real and imaginary (Q,U) or Amp/phase
C   (FARS)
C   Inputs:
C      DOREIM   L        T => plot Q, U; F => convert tp amp/phase
C   Common: RMFITD.INC
C   Common in/out
C      CATBLK   I(*)     modified for TV catalog
C   Outputs:
C      IERR     I        error code
C-----------------------------------------------------------------------
      LOGICAL   DOREIM
      INTEGER   INPTS, NG, IPOS(7), IERR
      DOUBLE PRECISION PARMS(*)
C
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      CHARACTER PREF*8, XPREF*8, TEXT*132, CTEMP1*18
      INTEGER   TVWIND(4), TVSIZE(2), INCHAR, I, J, INP, JTRIM, K, L
      REAL      XYRATI, BLC(2), TRC(2), CHOUT(4), ATEMP, YMULT, XMULT,
     *   LINT, DX, DY, XBLC(2), XTRC(2), PADD, XP, YP, DP, QQ, UU, XX,
     *   XL, XH, AMP
      DOUBLE PRECISION X, DSINCS
      LOGICAL   PFLG
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      XYRATI = (WINDTV(3)-WINDTV(1)+1.0) / (WINDTV(4)-WINDTV(2)+1.0)
C                                       Initialize for plotting
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      CALL RFILL (4, 0.5, CHOUT)
C                                       labeling commons
      LOCNUM = 1
      CALL SETLOC (PLPOS(3), .FALSE.)
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      RPLOC(1,LOCNUM) = 0.5
      RPLOC(2,LOCNUM) = 0.5
C                                       Q and U
      IF (DOREIM) THEN
         LINT = 500.
         ATEMP = ORANGE(2,1) - ORANGE(1,1)
         YMULT = ATEMP
         CALL METSCA (ATEMP, PREF, PFLG)
         YMULT = ATEMP / YMULT
         CTYP(2,LOCNUM) = 'Q Jy/bem'
         CPREF(2,LOCNUM) = PREF
         RPVAL(2,LOCNUM) = ORANGE(1,1) * YMULT
         AXINC(2,LOCNUM) = (ORANGE(2,1) - ORANGE(1,1)) * YMULT /
     *      (LINT-1.0)
         ATEMP = XRANGE(2,1) - XRANGE(1,1)
         XMULT = ATEMP
         CALL METSCA (ATEMP, XPREF, PFLG)
         XMULT = ATEMP / XMULT
         RPVAL(1,LOCNUM) = XRANGE(1,1) * XMULT
         AXINC(1,LOCNUM) = (XRANGE(2,1) - XRANGE(1,1)) * XMULT / 999.0
         TEXT = 'Q and U spectrum'
         CTYP(1,LOCNUM) = 'Lambda^2'
         CPREF(1,LOCNUM) = XPREF
C                                       amp and phase
      ELSE
         LINT = 700.
         ATEMP = ORANGE(2,3) - ORANGE(1,3)
         YMULT = ATEMP
         CALL METSCA (ATEMP, PREF, PFLG)
         YMULT = ATEMP / YMULT
         CTYP(2,LOCNUM) = 'Amplitud'
         CPREF(2,LOCNUM) = PREF
         RPVAL(2,LOCNUM) = ORANGE(1,3) * YMULT
         AXINC(2,LOCNUM) = (ORANGE(2,3) - ORANGE(1,3)) * YMULT /
     *      (LINT-1.0)
         ATEMP = XRANGE(2,2) - XRANGE(1,2)
         XMULT = ATEMP
         CALL METSCA (ATEMP, XPREF, PFLG)
         XMULT = ATEMP / XMULT
         CTYP(1,LOCNUM) = 'Rot.Meas'
         CPREF(1,LOCNUM) = XPREF
         RPVAL(1,LOCNUM) = XRANGE(1,2) * XMULT
         AXINC(1,LOCNUM) = (XRANGE(2,2) - XRANGE(1,2)) * XMULT / 999.0
         TEXT = 'FARS output'
         END IF
C                                       lower plot
      XBLC(1) = BLC(1)
      XTRC(1) = TRC(1)
      XBLC(2) = BLC(2)
      XTRC(2) = LINT
C                                        Set text borders at L, B,
C                                        R & T in characters
      CALL CHNTIC (BLC, TRC, INP)
      INP = MAX (INP, 4)
      CHOUT(1) = INP + 4
      CHOUT(2) = 3.333
      CHOUT(4) = 2.0
C                                        Init. for line drawing
      CALL GINITL (BLC, TRC, XYRATI, CHOUT, PLPOS(3), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        Draw the box
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), LINT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (TRC(1), LINT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DX = 0.0
      DY = 0.333
      INCHAR = JTRIM (TEXT)
      TEXT(INCHAR+1:) = '____'
      INCHAR = INCHAR + 5
      CALL H2CHR (18, 1, CATH(KHIMN), CTEMP1)
      CALL NAMEST (CTEMP1, CATBLK(KIIMS), TEXT(INCHAR:), INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       label bottom plot
      CALL CLAB1 (XBLC, XTRC, CHOUT, 3, XYRATI, .FALSE., PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       pixel coordinates
      XP = TRC(1)
      YP = LINT
      CALL GPOS (XP, YP, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DX = -9.5
      DY = -2.5
      WRITE (CTEMP1,1060) IPOS(2)
      CALL GCHAR (7, 0, DX, DY, CTEMP1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      DY = DY - 1.5
      WRITE (CTEMP1,1061) IPOS(3)
      CALL GCHAR (7, 0, DX, DY, CTEMP1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       top plot
      XBLC(2) = LINT
      XTRC(2) = TRC(2)
      RPLOC(2,LOCNUM) = LINT
C                                       U
      IF (DOREIM) THEN
         CTYP(2,LOCNUM) = 'U Jy/bem'
         RPVAL(2,LOCNUM) = ORANGE(1,2) * YMULT
         AXINC(2,LOCNUM) = (ORANGE(2,2) - ORANGE(1,2)) * YMULT /
     *      (TRC(2)-LINT-1.0)
C                                       phase
      ELSE
         CTYP(2,LOCNUM) = 'Phase'
         CPREF(2,LOCNUM) = ' '
         IF (ORANGE(2,4)-ORANGE(1,4).LE.ORANGE(2,5)-ORANGE(1,5)) THEN
            PADD = 0.0
            RPVAL(2,LOCNUM) = ORANGE(1,4)
            AXINC(2,LOCNUM) = (ORANGE(2,4) - ORANGE(1,4)) /
     *         (TRC(2)-LINT-1.0)
         ELSE
            PADD = 360.0
            RPVAL(2,LOCNUM) = ORANGE(1,5)
            AXINC(2,LOCNUM) = (ORANGE(2,5) - ORANGE(1,5)) /
     *         (TRC(2)-LINT-1.0)
            END IF
         END IF
      CPREF(1,LOCNUM) = ' '
      CTYP(1,LOCNUM) = ' '
      CALL CLAB1 (XBLC, XTRC, CHOUT, 3, XYRATI, .FALSE., PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       now plot data
C                                       Q
      IF (DOREIM) THEN
         CALL GLTYPE (1, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         DX = 0.5
         DY = 0.5
         XL = 10000.
         XH = -1000.
         DO 110 I = 1,INPTS
            IF ((QDATA(I).NE.FBLANK) .AND. (QDATA(I).GE.ORANGE(1,1))
     *         .AND. (QDATA(I).LE.ORANGE(2,1))) THEN
               XP = 999.0 * (LAMSQ(I) - XRANGE(1,1)) /
     *            (XRANGE(2,1) - XRANGE(1,1)) + 0.5
               YP = (LINT - 1.0) * (QDATA(I) - ORANGE(1,1)) /
     *            (ORANGE(2,1) - ORANGE(1,1)) + 0.5
               CALL GPOS (XP+DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP-DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GPOS (XP-DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP+DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               XL = MIN (XL, XP)
               XH = MAX (XH, XP)
               END IF
 110        CONTINUE
C                                       U
         DO 120 I = 1,INPTS
            IF ((UDATA(I).NE.FBLANK) .AND. (UDATA(I).GE.ORANGE(1,2))
     *         .AND. (UDATA(I).LE.ORANGE(2,2))) THEN
               XP = 999.0 * (LAMSQ(I) - XRANGE(1,1)) /
     *            (XRANGE(2,1) - XRANGE(1,1)) + 0.5
               YP = (999.0 - LINT) * (UDATA(I) - ORANGE(1,2)) /
     *            (ORANGE(2,2) - ORANGE(1,2)) + LINT + 0.5
               CALL GPOS (XP+DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP-DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GPOS (XP-DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP+DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               XL = MIN (XL, XP)
               XH = MAX (XH, XP)
               END IF
 120        CONTINUE
C                                       now plot current guess
         CALL GLTYPE (2, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         DX = (XRANGE(2,1) - XRANGE(1,1)) / 999.0
         DP = -DX + XRANGE(1,1)
         PFLG = .TRUE.
         DO 130 I = 1,1000
            DP = DP + DX
            XP = 999.0 * (DP - XRANGE(1,1)) /
     *         (XRANGE(2,1) - XRANGE(1,1)) + 0.5
            IF ((XP.GE.XL) .AND. (XP.LE.XH)) THEN
               QQ = 0.0
               J = -4
               DO 125 K = 1,NG
                  J = J + 4
                  AMP = PARMS(J+1)
                  IF (SPIXDO.EQ.1) THEN
                     AMP = AMP * ((LAMSQ1/DP) ** (0.5D0 * PARMS(J+4)))
                  ELSE IF (SPIXDO.GE.2) THEN
                     X = PARMS(J+4) * DP
                     AMP = AMP * DSINCS (SPIXDO, 0, X)
                     END IF
                  QQ = QQ + AMP * COS (2.0D0*DG2RAD*PARMS(J+2) +
     *               2.0D0 * DP * PARMS(J+3))
 125              CONTINUE
               IF ((QQ.GE.ORANGE(1,1)) .AND. (QQ.LE.ORANGE(2,1))) THEN
                  YP = (LINT - 1.0) * (QQ - ORANGE(1,1)) /
     *               (ORANGE(2,1) - ORANGE(1,1)) + 0.5
                  IF (PFLG) THEN
                     CALL GPOS (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                  ELSE
                     CALL GVEC (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  PFLG = .FALSE.
               ELSE
                  PFLG = .TRUE.
                  END IF
            ELSE
               PFLG = .TRUE.
               END IF
 130        CONTINUE
C                                       U guess
         DP = -DX + XRANGE(1,1)
         PFLG = .TRUE.
         DO 140 I = 1,1000
            DP = DP + DX
            XP = 999.0 * (DP - XRANGE(1,1)) /
     *         (XRANGE(2,1) - XRANGE(1,1)) + 0.5
            IF ((XP.GE.XL) .AND. (XP.LE.XH)) THEN
               UU = 0.0
               J = -4
               DO 135 K = 1,NG
                  J = J + 4
                  AMP = PARMS(J+1)
                  IF (SPIXDO.EQ.1) THEN
                     AMP = AMP * ((LAMSQ1/DP) ** (0.5D0 * PARMS(J+4)))
                  ELSE IF (SPIXDO.GE.2) THEN
                     X = PARMS(J+4) * DP
                     AMP = AMP * DSINCS (SPIXDO, 0, X)
                     END IF
                  UU = UU + AMP * SIN (2.0D0*DG2RAD*PARMS(J+2) +
     *               2.0D0 * DP * PARMS(J+3))
 135              CONTINUE
               IF ((UU.GE.ORANGE(1,2)) .AND. (UU.LE.ORANGE(2,2))) THEN
                  YP = (999.0 - LINT) * (UU - ORANGE(1,2)) /
     *               (ORANGE(2,2) - ORANGE(1,2)) + LINT + 0.5
                  IF (PFLG) THEN
                     CALL GPOS (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                  ELSE
                     CALL GVEC (XP, YP, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 980
                     END IF
                  PFLG = .FALSE.
               ELSE
                 PFLG = .TRUE.
                 END IF
            ELSE
               PFLG = .TRUE.
               END IF
 140        CONTINUE

C                                       amplitude
      ELSE
         CALL GLTYPE (1, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         XX = OLDR(KRCIC,4) * 999.0 / (XRANGE(2,2) - XRANGE(1,2))
         DP = OLDD(KDCRV,4) + (1.0-OLDR(KRCRP,4))/OLDR(KRCIC,4)
         XP = 999.0 * (DP - XRANGE(1,2)) / (XRANGE(2,2) - XRANGE(1,2)) +
     *      0.5
         XP = XP - XX - XX/2.0
         PFLG = .TRUE.
         DO 210 I = 1,INPTS
            XP = XP + XX
            IF ((AMDATA(I).NE.FBLANK) .AND. (PHDATA(I).NE.FBLANK)) THEN
               DP = AMDATA(I)
               YP = (LINT - 1.0) * (DP - ORANGE(1,3)) /
     *            (ORANGE(2,3) - ORANGE(1,3)) + 0.5
               IF (PFLG) THEN
                  CALL GPOS (XP, YP, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 980
               ELSE
                  CALL GVEC (XP, YP, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 980
                  END IF
               CALL GVEC (XP+XX, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               PFLG = .FALSE.
            ELSE
               PFLG = .TRUE.
               END IF
 210        CONTINUE
C                                       phase
         DX = 1.5
         DY = 1.5
         J = 4
         IF (PADD.EQ.360.0) J = 5
         XX = OLDR(KRCIC,4) * 999.0 / (XRANGE(2,2) - XRANGE(1,2))
         DP = OLDD(KDCRV,4) + (1.0-OLDR(KRCRP,4))/OLDR(KRCIC,4)
         XP = 999.0 * (DP - XRANGE(1,2)) / (XRANGE(2,2) - XRANGE(1,2)) +
     *      0.5
         XP = XP - XX
         DO 220 I = 1,INPTS
            IF ((AMDATA(I).NE.FBLANK) .AND. (PHDATA(I).NE.FBLANK)) THEN
               XP = XP + XX
               DP = PHDATA(I)
               IF (DP.LT.0.0) DP = DP + PADD
               YP = (999.0-LINT) * (DP - ORANGE(1,J)) /
     *            (ORANGE(2,J) - ORANGE(1,J)) + LINT + 0.5
               CALL GPOS (XP+DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP-DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GPOS (XP-DX, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP+DX, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
 220        CONTINUE
C                                       plot current guess
         CALL GLTYPE (2, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         L = -4
         DX = 7.0
         DY = 7.0
         DO 230 K = 1,NG
            L = L + 4
            I = (PARMS(L+3) - OLDD(KDCRV,4)) / OLDR(KRCIC,4) +
     *         OLDR(KRCRP,4) + 0.5
            XP = 999.0 * (PARMS(L+3) - XRANGE(1,2)) /
     *         (XRANGE(2,2) - XRANGE(1,2)) + 0.5
            IF (LLCOMP(L+1).LT.0) XP = -9.0
            DP = AMDATA(I)
            IF (LLCOMP(L+1).LT.0) DP = 0.0
            YP = (LINT - 1.0) * (DP - ORANGE(1,3)) /
     *         (ORANGE(2,3) - ORANGE(1,3)) + 0.5
            CALL GPOS (XP+DX, YP+DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GVEC (XP-DX, YP-DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GPOS (XP-DX, YP+DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GVEC (XP+DX, YP-DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            DP = PHDATA(I)
            IF (DP.LT.0.0) DP = DP + PADD
            IF (LLCOMP(L+1).LT.0) DP = 0.0
            YP = (999.0-LINT) * (DP - ORANGE(1,J)) /
     *         (ORANGE(2,J) - ORANGE(1,J)) + LINT + 0.5
            CALL GPOS (XP+DX, YP+DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GVEC (XP-DX, YP-DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GPOS (XP-DX, YP+DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL GVEC (XP+DX, YP-DY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 980
 230        CONTINUE
         END IF
C
 980  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (8)
         END IF
      CALL YHOLD ('OFFF', K)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1980 FORMAT ('RTVPLT: ERROR',I4,' FROM TV PLOT ROUTINES')
      END
      SUBROUTINE GTVGUS (NG, PARMS, IERR)
C-----------------------------------------------------------------------
C   GTVGUS has the user point at the desired initial guess for each
C   Component with the TV cursor.  It will accept that there is no
C   component "n" at this row.  It sets the PARMS and plots the new
C   guess with + signs.
C   Inputs:
C      NG       I       Number of Components
C   Output:
C      PARMS    D(16)   Initial guess
C      FVEC     D(*)    Work buffer - size of data
C      IERR     I       error code: 0 -> ok
C-----------------------------------------------------------------------
      INTEGER   NG, IERR
      DOUBLE PRECISION PARMS(*)
C
      CHARACTER MSGBUF*80
      REAL      RPOS(2)
      INTEGER   I, J, IXT, IYT, IX, IY, JERR, IX0, IY0, QUAD, IBUT
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Set scales
      IXT = GPHCAT(IICOR+2)
      IYT = GPHCAT(IICOR+3)
      IX0 = GPHCAT(IICOR)
      IY0 = GPHCAT(IICOR+1)
C                                       Open terminal
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
C                                       Loop over components.
      J = -4
      DO 40 I = 1,NG
         J = J + 4
C                                       read peak and position
         IF ((DOCOMP(J+1).GT.0) .OR. (DOCOMP(J+2).GT.0)) THEN
            WRITE (MSGBUF,1020) I
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 900
            CALL TVWHER (QUAD, RPOS, IBUT, IERR)
            IF (IERR.NE.0) GO TO 990
            IX = RPOS(1) + 0.5
            IY = RPOS(2) + 0.5

C                                       Set peak and position
            IF ((IX.GE.IX0) .AND. (IY.GE.IY0) .AND. (IX.LE.IXT) .AND.
     *         (IY.LE.IYT)) THEN
               LLCOMP(J+1) = DOCOMP(J+1)
               LLCOMP(J+2) = DOCOMP(J+2)
               LLCOMP(J+3) = DOCOMP(J+3)
               LLCOMP(J+4) = DOCOMP(J+4)
               IF (LLCOMP(J+1).GT.0) THEN
                  PARMS(J+3) = ((RPOS(1) - IX0) / (IXT - IX0)) *
     *               (XRANGE(2,2) - XRANGE(1,2)) + XRANGE(1,2)
                  IX = (PARMS(J+3) - OLDD(KDCRV,4)) / OLDR(KRCIC,4) +
     *               OLDR(KRCRP,4) + 0.5
                  PARMS(J+1) = AMDATA(IX)
                  PARMS(J+2) = 0.5D0 * PHDATA(IX)
                  IF (SPIXDO.GE.2) THEN
                     PARMS(J+4) = 5.0D0
                  ELSE
                     PARMS(J+4) = 0.0D0
                     END IF
                  END IF
C                                       Blank this component
            ELSE
               PARMS(J+1) = 0.0D0
               PARMS(J+2) = 0.0D0
               PARMS(J+3) = 0.0D0
               PARMS(J+4) = 0.0D0
               LLCOMP(J+1) = -1
               LLCOMP(J+2) = -1
               LLCOMP(J+3) = -1
               LLCOMP(J+4) = -1
               END IF
            END IF
 40      CONTINUE
      GO TO 990
C                                       terminal error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (6)
C                                       Close hold
 990  CALL YHOLD ('OFFF', JERR)
      IF (TTYIND.GT.0) THEN
          CALL ZCLOSE (TTYLUN, TTYIND, JERR)
          TTYIND = 0
          END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' OPENING TERMINAL')
 1020 FORMAT ('Position cursor at center (RM) of component ',I2)
 1900 FORMAT ('ERROR',I5,' WRITING TO TERMINAL')
      END
      SUBROUTINE RTVMOD (DOTV, INPTS, NG, IPOS, NCOMPS, FVEC, QURANG,
     *   PARMS, PERR, IERR)
C-----------------------------------------------------------------------
C   RTVMOD plots the model functions on the TV.  It asks the user for
C   permission to proceed.
C   Inputs:
C      DOTV     R      > 1.5 use TV menu
C      INPTS    I      Number of data points
C      NCOMPS   I      Max number Components allowed
C      IPOS     I(*)   Position in cube
C      FVEC     D(*)   data - model
C      PERR     I      > 0 => probable parameter bad
C   In/Out:
C      NG       I      Number of Components: retry can ask for fewer
C      PARMS    D(*)   Model parameters
C   Output:
C      IERR     I      TV error code
C                         101 => blank this solution
C                         102 => User wants to quit
C                         103 => do a retry
C                         104 => TV off
C                         105 => Do fit with this guess
C-----------------------------------------------------------------------
      INTEGER   INPTS, NG, IPOS(*), NCOMPS, PERR, IERR
      DOUBLE PRECISION FVEC(*), PARMS(*)
      REAL      DOTV, QURANG(2,2)
C
      CHARACTER CTEMP*1, MSGBUF*80, FIRSTC*1, CHS*32
      INTEGER   I, J, JERR, JTRIM, NCHS, IGR, SCRTCH(4096)
      REAL      XP, YP, DX, DY, LINT, TEMP
      LOGICAL   T, F, WHAND
      DOUBLE PRECISION QRMS, URMS, QCNT, UCNT, QWRMS, UWRMS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'RMFITD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
 10   IERR = 0
      CHS = ' '
      IF (NCOMPS.EQ.2) THEN
         CHS = '''1'', ''2'''
      ELSE IF (NCOMPS.EQ.3) THEN
         CHS = '''1'', ''2'', ''3'''
      ELSE IF (NCOMPS.EQ.4) THEN
         CHS = '''1'', ''2'', ''3'', ''4'''
         END IF
      NCHS = JTRIM (CHS)
      WHAND = .FALSE.
C                                       Plot model
      IGR = IGR4
      CALL GLTYPE (4, PLTBLK, IERR)
      IF (IGR.NE.IGR1) THEN
         CALL YZERO (IGR, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      LINT = 500.0
      DX = 1.5
      DY = 1.5
      CALL YHOLD ('ONNN', IERR)
      QRMS = 0.0D0
      URMS = 0.0D0
      QWRMS = 0.0D0
      UWRMS = 0.0D0
      QCNT = 0.0D0
      UCNT = 0.0D0
      DO 60 I = 1,INPTS
C                                       Q
         IF (QDATA(I).NE.FBLANK) THEN
            TEMP = FVEC(I) / WEIGHT(I)
            QWRMS = QWRMS + FVEC(I)**2
            QRMS = QRMS + TEMP**2
            QCNT = QCNT + 1.0D0
            XP = 999.0 * (LAMSQ(I) - XRANGE(1,1)) /
     *         (XRANGE(2,1) - XRANGE(1,1)) + 0.5
            YP = QDATA(I) - TEMP
            IF ((YP.GE.ORANGE(1,1)) .AND. (YP.LE.ORANGE(2,1))) THEN
               YP = (LINT - 1.0) * (YP - ORANGE(1,1)) /
     *            (ORANGE(2,1) - ORANGE(1,1)) + 0.5
               CALL GPOS (XP, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GPOS (XP-DX, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP+DX, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            END IF
C                                       U
         IF (UDATA(I).NE.FBLANK) THEN
            TEMP = FVEC(I+INPTS) / WEIGHT(I+INPTS)
            URMS = URMS + TEMP**2
            UWRMS = UWRMS + FVEC(I+INPTS)**2
            UCNT = UCNT + 1.0D0
            XP = 999.0 * (LAMSQ(I) - XRANGE(1,1)) /
     *         (XRANGE(2,1) - XRANGE(1,1)) + 0.5
            YP = UDATA(I) - TEMP
            IF ((YP.GE.ORANGE(1,2)) .AND. (YP.LE.ORANGE(2,2))) THEN
               YP = (LINT - 1.0) * (YP - ORANGE(1,2)) /
     *            (ORANGE(2,2) - ORANGE(1,2)) + LINT + 0.5
               CALL GPOS (XP, YP+DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP, YP-DY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GPOS (XP-DX, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL GVEC (XP+DX, YP, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            END IF
 60      CONTINUE
      CALL YHOLD ('OFFF', IERR)
      IF (QCNT.GT.0.0D0) QRMS = SQRT (QRMS/QCNT)
      IF (UCNT.GT.0.0D0) URMS = SQRT (URMS/UCNT)
      IF (QCNT.GT.0.0D0) QWRMS = SQRT (QWRMS/QCNT)
      IF (UCNT.GT.0.0D0) UWRMS = SQRT (UWRMS/UCNT)
      THERMS(1,1) = QRMS
      THERMS(2,1) = URMS
      THERMS(1,2) = QWRMS
      THERMS(2,2) = UWRMS
C                                       Open TTY
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1900) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
         END IF
      IF (PERR.GT.0) THEN
         WRITE (MSGBUF,1910)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      WRITE (MSGBUF,1912) QWRMS, UWRMS, IPOS(2), IPOS(3)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      J = 1
      DO 905 I = 1,NG
         WRITE (MSGBUF,1913) I, PARMS(J), PARMS(J+1), PARMS(J+2),
     *      PARMS(J+3)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         J = J + 4
 905     CONTINUE
      IF (DOTV.GT.1.5) THEN
         CALL RMMEN2 (NCOMPS, WHAND, MSGBUF, SCRTCH, JERR)
      ELSE
         MSGBUF = '**** Enter HAND to enter new numbers by hand'
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         WRITE (MSGBUF,1915)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (NCHS.GT.0) THEN
            WRITE (MSGBUF,1916) CHS(:NCHS)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         IF (WHAND) THEN
            WRITE (MSGBUF,1919)
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            END IF
         WRITE (MSGBUF,1920)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         WRITE (MSGBUF,1921)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         WRITE (MSGBUF,1922)
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      WHAND = .FALSE.
      CTEMP = FIRSTC (MSGBUF)
      IF (CTEMP.EQ.'B') THEN
         IERR = 101
      ELSE IF (CTEMP.EQ.'Q') THEN
         IERR = 102
      ELSE IF (CTEMP.EQ.'T') THEN
         IERR = 104
      ELSE IF (CTEMP.EQ.'R') THEN
         IERR = 103
      ELSE IF (CTEMP.EQ.'D') THEN
         IERR = 105
      ELSE IF (CTEMP.EQ.'E') THEN
         IERR = 103
      ELSE IF (CTEMP.EQ.'1') THEN
         IERR = 103
         NG = 1
      ELSE IF ((CTEMP.EQ.'2') .AND. (NCOMPS.GE.2)) THEN
         IERR = 103
         NG = 2
      ELSE IF ((CTEMP.EQ.'3') .AND. (NCOMPS.GE.3)) THEN
         IERR = 103
         NG = 3
      ELSE IF ((CTEMP.EQ.'4') .AND. (NCOMPS.GE.4)) THEN
         IERR = 103
         NG = 4
      ELSE IF (CTEMP.EQ.'H') THEN
         WHAND = .TRUE.
         I = 2 * INPTS
         CALL RMHAND (MSGBUF, NG, NCOMPS, I, PARMS, FVEC, IERR)
         PERR = 0
         IF (IERR.EQ.0) GO TO 10
         IERR = MAX (IERR, 0)
      ELSE IF (CTEMP.EQ.'P') THEN
         CALL RMPIXR (INPTS, NG, IPOS, PARMS, MSGBUF, QURANG)
         GO TO 10
         END IF
      GO TO 990
C                                       TTY error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (6)
 990  IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, JERR)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('>>>> PARAMETERS SEEM OUT OF RANGE.  SOLUTION PROBABLY ',
     *   'BAD! <<<<')
 1912 FORMAT ('Weighted RMS residual Q',F11.6,' U',F11.6,'  at Y=',I5,
     *   ' Z=',I5)
 1913 FORMAT ('Component',I2,F12.6,F10.2,F10.3,F9.3)
 1915 FORMAT ('**** Enter RETR to repeat the row (must enter a guess',
     *   ' on the TV)')
 1916 FORMAT ('**** Enter one of ',A,' to RETR with that',
     *   ' number of Components')
 1919 FORMAT ('**** Enter DOFIT to do a new fit with this HAND guess')
 1920 FORMAT ('**** enter BAD to flag solution, QUIT to die, ',
     *   'TVOFF to stop TV display')
 1921 FORMAT ('**** enter PIXR to chose a different display range,')
 1922 FORMAT ('     or hit return to continue')
 1980 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE RMMEN2 (NG, WHAND, MSGBUF, SCRTCH, JERR)
C-----------------------------------------------------------------------
C   Does a TV menu for post-fit stage of user questions
C   Inputs:
C      NG       I       Number Gaussians max
C      WHAND    L       T => last command was HAND (add DO FIT to menu)
C   Outputs:
C      MSGBUF   C*(*)   answer: E, B, Q, other
C      JERR     I       error
C-----------------------------------------------------------------------
      INTEGER   NG, SCRTCH(*), JERR
      LOGICAL   WHAND
      CHARACTER MSGBUF*(*)
C
      INTEGER   NCHS
      PARAMETER (NCHS = 14)
C
      INTEGER   MTYPE, NCOL, NROWS, GRCHS(2), TOPSEP, SIDSEP, TIMLIM,
     *   NTITLE, TVBUTT, CHOICE, I, N
      LOGICAL   LEAVE(NCHS)
      CHARACTER CHOICS(NCHS)*8, TITLE*8, ISHELP*6, CHOICZ(NCHS)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA CHOICZ /'GOOD', ' ', 'RE-GUESS', 'TVOFF', 'HAND', 'BAD',
     *   'PIXRANGE', '1', '2', '3', '4', ' ', 'QUIT', 'DO FIT'/
      DATA LEAVE /.FALSE., .TRUE., 10*.FALSE., .TRUE., .FALSE./
      DATA ISHELP /'XGAUS'/
C-----------------------------------------------------------------------
      NROWS = 7 + NG
      CHOICS(1) = CHOICZ(1)
      CHOICS(2) = CHOICZ(2)
      N = 2
      IF (WHAND) THEN
         NROWS = NROWS + 1
         CHOICS(3) = 'DO FIT'
         N = 3
         END IF
      DO 10 I = 3,NROWS
         N = N + 1
         CHOICS(N) = CHOICZ(I)
 10      CONTINUE
      CHOICS(NROWS+1) = ' '
      CHOICS(NROWS+2) = 'QUIT'
      LEAVE(NROWS+1) = .TRUE.
      LEAVE(NROWS+2) = .FALSE.
      NROWS = NROWS + 2
      GRCHS(1) = 6
      GRCHS(2) = 5
      MTYPE = 1
      NCOL = 1
      SIDSEP = 6 * CSIZTV(1)
      TOPSEP = 5 * CSIZTV(2)
      NTITLE = 0
      TITLE = ' '
      TIMLIM = 0
      MSGBUF = ' '
C                                       menu
 20   CALL TVMENU (MTYPE, NCOL, NROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHOICE, TVBUTT, SCRTCH,
     *   JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'RMMEN2: ERROR FROM TV MENU'
         CALL MSGWRT (7)
      ELSE IF (CHOICS(CHOICE).EQ.' ') THEN
         GO TO 20
      ELSE
         MSGBUF = CHOICS(CHOICE)(:1)
         END IF
C
 999  RETURN
      END
      SUBROUTINE RMFICH (NG, INPTS, FVEC, PARMS, IERR)
C-----------------------------------------------------------------------
C   RMFICH checks the answers for being at least vaguely reasonable.
C   Inputs:
C      NG      I       Number of Components
C      INPTS   I      Number of data samples in Q/U
C      FVEC    D(*)   weighted residuals
C      PARMS   D      Answers
C   Output:
C      IERR    I      0 -> all in range, 1 -> some not
C-----------------------------------------------------------------------
      INTEGER   NG, INPTS, IERR
      DOUBLE PRECISION PARMS(*), FVEC(*)
C
      REAL      X, PMAX, RMAX, RMIN
      INTEGER   IAMP, I
      DOUBLE PRECISION QRMS, URMS, QCNT, UCNT, QWRMS, UWRMS, TEMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
C-----------------------------------------------------------------------
C                                       need rmses
      QRMS = 0.0D0
      URMS = 0.0D0
      QWRMS = 0.0D0
      UWRMS = 0.0D0
      QCNT = 0.0D0
      UCNT = 0.0D0
      DO 60 I = 1,INPTS
C                                       Q
         IF (QDATA(I).NE.FBLANK) THEN
            TEMP = FVEC(I) / WEIGHT(I)
            QWRMS = QWRMS + FVEC(I)**2
            QRMS = QRMS + TEMP**2
            QCNT = QCNT + 1.0D0
            END IF
C                                       U
         IF (UDATA(I).NE.FBLANK) THEN
            TEMP = FVEC(I+INPTS) / WEIGHT(I+INPTS)
            URMS = URMS + TEMP**2
            UWRMS = UWRMS + FVEC(I+INPTS)**2
            UCNT = UCNT + 1.0D0
            END IF
 60      CONTINUE
      IF (QCNT.GT.0.0D0) QRMS = SQRT (QRMS/QCNT)
      IF (UCNT.GT.0.0D0) URMS = SQRT (URMS/UCNT)
      IF (QCNT.GT.0.0D0) QWRMS = SQRT (QWRMS/QCNT)
      IF (UCNT.GT.0.0D0) UWRMS = SQRT (UWRMS/UCNT)
      THERMS(1,1) = QRMS
      THERMS(2,1) = URMS
      THERMS(1,2) = QWRMS
      THERMS(2,2) = UWRMS
C                                       Check answers
      PMAX = SQRT (OLDR(KRDMX,1)**2 + OLDR(KRDMX,2)**2)
      RMAX = OLDD(KDCRV,4) + (CATOLD(KINAX,4) - 1.0 - OLDR(KRCRP,4)) *
     *   OLDR(KRCIC,4)
      RMIN = OLDD(KDCRV,4) + (2.0 - OLDR(KRCRP,4)) * OLDR(KRCIC,4)
      IF (RMAX.LT.RMIN) THEN
         X = RMAX
         RMAX = RMIN
         RMIN = X
         END IF
      IERR = 1
      IF ((QRMS.GT.RMSLIM) .OR. (URMS.GT.RMSLIM)) GO TO 999
      DO 130 I = 1,NG
         IAMP = 4 * I - 3
         X = PARMS(IAMP)
         IF (((X.LT.0.0) .OR. (X.GT.PMAX)) .AND. (LLCOMP(IAMP).GT.0))
     *      GO TO 999
         X = ABS (PARMS(IAMP+2))
         IF (((X.LT.RMIN) .OR. (X.GT.RMAX)) .AND. (LLCOMP(IAMP+2).GT.0))
     *      GO TO 999
         IF (LLCOMP(IAMP+3).GT.0) THEN
            IF (SPIXDO.EQ.1) THEN
               IF (ABS(PARMS(IAMP+3)).GT.4.0) GO TO 999
            ELSE IF (SPIXDO.GE.2) THEN
               PARMS(IAMP+3) = ABS (PARMS(IAMP+3))
               IF (PARMS(IAMP+3).GT.200.) GO TO 999
               END IF
            END IF
 130     CONTINUE
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE RMHAND (MSGBUF, NG, NCOMPS, INPTS, PARMS, FVEC, IERR)
C-----------------------------------------------------------------------
C   Enter guesses as hand numbers
C   Inputs:
C      NG       I       Number Components: currently
C      NCOMPS   I       Max number of Components
C      INPTS    I       Number points in FVEC
C   In/out:
C      MSGBUF   C*(*)   Message buffer
C      PARMS    D(*)    Parameters
C      FVEC     D(*)    Residuals
C   Output:
C      IERR     I       0 => carry on
C-----------------------------------------------------------------------
      INTEGER   NG, NCOMPS, INPTS, IERR
      CHARACTER MSGBUF*(*)
      DOUBLE PRECISION PARMS(*), FVEC(*)
C
      INCLUDE 'RMFITD.INC'
      INTEGER   I, J, K, JTRIM, KBP, MP
      DOUBLE PRECISION XX, OPARMS(MAXPRM), FJAC(MAXPRM,MAXPRM),
     *   VALVAR(MAXPRM)
      CHARACTER TYPE4*5
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      J = 1
      CALL DPCOPY (MAXPRM, PARMS, OPARMS)
      IF (SPIXDO.EQ.1) THEN
         TYPE4 = 'spix'
      ELSE IF (SPIXDO.GE.2) THEN
         TYPE4 = 'thick'
      ELSE
         TYPE4 = ' '
         END IF
      DO 20 I = 1,NG
         IF (LLCOMP(J+3).LE.0) THEN
            WRITE (MSGBUF,1000) I
         ELSE
            WRITE (MSGBUF,1010) I, TYPE4
            END IF
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         K = JTRIM (MSGBUF)
         KBP = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J) = XX
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J+1) = XX
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         PARMS(J+2) = XX
         IF (LLCOMP(J+3).GT.0) THEN
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(J+3) = XX
            END IF
         J = J + 4
 20      CONTINUE
      DO 30 I = NG+1,NCOMPS
         IF (LLCOMP(J+3).LE.0) THEN
            WRITE (MSGBUF,1020) I
         ELSE
            WRITE (MSGBUF,1010) I, TYPE4
            CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            WRITE (MSGBUF,1011)
            END IF
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         K = JTRIM (MSGBUF)
         KBP = 1
         CALL GETNUM (MSGBUF, K, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 900
         IF (XX.LE.0.0D0) THEN
            GO TO 40
         ELSE
            NG = I
            PARMS(J) = XX
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(J+1) = XX
            CALL GETNUM (MSGBUF, K, KBP, XX)
            IF (XX.EQ.DBLANK) GO TO 900
            PARMS(J+2) = XX
            IF (LLCOMP(J+3).GT.0) THEN
               CALL GETNUM (MSGBUF, K, KBP, XX)
               IF (XX.EQ.DBLANK) GO TO 900
               PARMS(J+3) = XX
               END IF
            J = J + 4
            END IF
 30      CONTINUE
C                                       evaluate residual
 40   ITTER = 0
      NITTER = 100
      MP = 4 * NG
      NVAR = 0
      K = 0
      DO 84 I = 1,NG
         DO 83 J = 1,4
            K = K + 1
            IF (LLCOMP(K).GT.0) THEN
               NVAR = NVAR + 1
               IVAR(NVAR) = I
               JVAR(NVAR) = J
               VALVAR(NVAR) = PARMS(K)
               END IF
 83         CONTINUE
 84      CONTINUE
      I = 1
      MP = NVAR
      CALL RMFUNC (INPTS, MP, VALVAR, FVEC, FJAC, I)
      GO TO 999
C                                       bad value quiet exit
 900  IERR = -1
      CALL DPCOPY (MAXPRM, OPARMS, PARMS)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Enter Component',I2,' P, theta, RM (3 values)')
 1010 FORMAT ('Enter Component',I2,' P, theta, RM, ',A,' (4 values)')
 1011 FORMAT ('                  zero -> skip this one')
 1020 FORMAT ('Enter Component',I2,' P, theta, RM (3 values)',
     *   ', zero -> skip this one')
      END
      SUBROUTINE RMPIXR (INPTS, NG, IPOS, PARMS, MSGBUF, QURANG)
C-----------------------------------------------------------------------
C   gets a new QU pixel range
C   In/Out:
C      MSGBUF   C*(*)    message buffer
C   Outputs
C      QURANG   R(2,2)   Q range, U range
C-----------------------------------------------------------------------
      INTEGER   INPTS, NG, IPOS(*)
      DOUBLE PRECISION PARMS(*)
      CHARACTER MSGBUF*(*)
      REAL      QURANG(2,2)
C
      INTEGER   NCLIM, KBP, JTRIM, IERR, I
      REAL      TEMP
      DOUBLE PRECISION XX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
C-----------------------------------------------------------------------
      MSGBUF = 'Enter Q pixel range and U pixel range: 4 numbers'
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      MSGBUF = 'or 2 numbers for identical ranges or none for default'
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL RFILL (4, 0.0, QURANG)
      NCLIM = JTRIM (MSGBUF)
      IF (NCLIM.GT.0) THEN
         KBP = 1
         CALL GETNUM (MSGBUF, NCLIM, KBP, XX)
         IF (XX.EQ.DBLANK) GO TO 100
         QURANG(1,1) = XX
         CALL GETNUM (MSGBUF, NCLIM, KBP, XX)
         IF (XX.EQ.DBLANK) THEN
            QURANG(1,1) = 0.0
            GO TO 100
            END IF
         QURANG(2,1) = XX
         CALL GETNUM (MSGBUF, NCLIM, KBP, XX)
         IF (XX.EQ.DBLANK) THEN
            QURANG(1,2) = QURANG(1,1)
            QURANG(2,2) = QURANG(2,1)
            GO TO 100
            END IF
         QURANG(1,2) = XX
         CALL GETNUM (MSGBUF, NCLIM, KBP, XX)
         IF (XX.EQ.DBLANK) THEN
            QURANG(1,2) = 0.0
            GO TO 100
            END IF
         QURANG(2,2) = XX
         END IF
C                                       redo the max/min if needed
 100  IF (QURANG(2,1).LE.QURANG(1,1)) THEN
         DO 110 I = 1,INPTS
            IF (QDATA(I).NE.FBLANK) THEN
               IF (QDATA(I).LT.ORANGE(1,1)) ORANGE(1,1) = QDATA(I)
               IF (QDATA(I).GT.ORANGE(2,1)) ORANGE(2,1) = QDATA(I)
               END IF
 110        CONTINUE
      ELSE
         ORANGE(1,1) = QURANG(1,1)
         ORANGE(2,1) = QURANG(2,1)
         END IF
      IF (QURANG(2,2).LE.QURANG(1,2)) THEN
         DO 120 I = 1,INPTS
            IF (UDATA(I).NE.FBLANK) THEN
               IF (UDATA(I).LT.ORANGE(1,2)) ORANGE(1,2) = UDATA(I)
               IF (UDATA(I).GT.ORANGE(2,2)) ORANGE(2,2) = UDATA(I)
               END IF
 120        CONTINUE
      ELSE
         ORANGE(1,2) = QURANG(1,2)
         ORANGE(2,2) = QURANG(2,2)
         END IF
      TEMP = ORANGE(2,1) - ORANGE(1,1)
      ORANGE(2,1) = ORANGE(2,1) + 0.075 * TEMP
      ORANGE(1,1) = ORANGE(1,1) - 0.075 * TEMP
      TEMP = ORANGE(2,2) - ORANGE(1,2)
      ORANGE(2,2) = ORANGE(2,2) + 0.075 * TEMP
      ORANGE(1,2) = ORANGE(1,2) - 0.075 * TEMP
C                                       clear the screens
      CALL YZERO (IGR1, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (IGR2.NE.IGR1) THEN
         CALL YZERO (IGR2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YZERO (IGR3, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YZERO (IGR4, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       plot data
      CALL COPY (256, CATOLD(1,1), CATBLK)
      CALL RTVPLT (.TRUE., INPTS, NG, IPOS, PARMS, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YHOLD ('OFFF', I)
      GO TO 990
C
 980  WRITE (MSGTXT,1980) IERR, 'I/O TO TERMINAL'
      CALL MSGWRT (8)
      GO TO 999
C                                       TV
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR, 'WRITING TO THE TV'
         CALL MSGWRT (8)
         END IF
      CALL YHOLD ('OFFF', I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('RMPIXR: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RMFITV (IRET)
C-----------------------------------------------------------------------
C   RMFITV implements a TV menu driven method to manipulate the
C   Component fits results in order to re-try fits, rearrange fit
C   components, etc.
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER   NOPT1, NOPT2, NOPTS
      PARAMETER (NOPT1=19+MXPAIR)
      PARAMETER (NOPT2=NMXIMG*MAXCMP)
      PARAMETER (NOPTS=NOPT1+NOPT2)
C
      INTEGER   MTYPE, MCOL, MROWS(3), GRCHS(2), TIMLIM, CHS, TVBUT,
     *   NX, NY, NP, I, J, IPOS(2), NWORDS, TOPSEP, IP, NG, I1, I2,
     *   JTRIM, IC, ICOLOR, NLEVS, JJ, II, IBUFF1(MABFSS), IG, MAXCH,
     *   CATEMP(256), SIDSEP, WIND(4), MMXIMG, MP
      CHARACTER CHOIC1(NOPT1)*17, CHOIC2(NOPT2)*17, CHOICS(NOPTS)*17,
     *   ISHELP*6, TITLE(1+MAXCMP)*128, MSGBUF*72, SPINX*5
      LOGICAL   IMGOK, LEAVE1(NOPT1), LEAVE2(NOPT2), LEAVE(NOPTS), DOIT
      LONGINT   PIMAGE
      REAL      IMAGES(2), SNMIN(MAXCMP), MAXRES, MAXRM(2,MAXCMP),
     *   MAXDWD(MAXCMP), MAXSPX(2,MAXCMP), SLOPE, OFFS, POLMIN(MAXCMP)
      DOUBLE PRECISION XX(8)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IBUFF1, BUFF1)
      DATA TIMLIM, TOPSEP, SIDSEP, GRCHS /0, 3, 10, 2,1/
      DATA LEAVE1 /.FALSE., 7*.TRUE., 2*.FALSE., 7*.TRUE., 2*.FALSE.,
     *   MXPAIR*.TRUE./
      DATA CHOIC1 /'EXIT', ' ', 'SET MIN S/N', 'SET MIN P1',
     *   'SET MAX RESID', 'SET RM RANGE', 'SET MAX THETA ER',
     *   'SET SPIX RANGE', 'REDO ALL', 'FLAG ALL', ' ', 'OFF ZOOM',
     *   'OFF TRANSFER', 'SET DOOUTPUT', ' ', 'ADD TO LIST',
     *   'SHOW LIST', 'REDO LIST', 'FLAG LIST', 'SWAP LIST 1-2',
     *   'SWAP LIST 1-3', 'SWAP LIST 2-3', 'SWAP LIST 1-4',
     *   'SWAP LIST 2-4', 'SWAP LIST 3-4'/
C-----------------------------------------------------------------------
      MCOL = 2
      MMXIMG = NMXIMG
      IF ((SPIXDO.LT.1) .OR. (SPIXDO.GT.4)) MMXIMG = MMXIMG - 2
      IF (SPIXDO.GE.2) THEN
         CHOIC1(8) = 'SET THICKNESS MAX'
         SPINX = 'THICK'
      ELSE IF (SPIXDO.EQ.1) THEN
         SPINX = 'SPINX'
      ELSE
         CHOIC1(8) = ' '
         SPINX = ' '
         END IF
      IGR1 = 1
      IGR2 = 2
      IGR3 = 3
      IGR4 = 4
      IGR1 = IGR1 + NGRAY
      IGR2 = IGR2 + NGRAY
      IGR3 = IGR3 + NGRAY
      IGR4 = IGR4 + NGRAY
      DO 5 I = 1,NGRAY+NGRAPH
         CALL YSLECT ('OFFF', I, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
 5       CONTINUE
      CALL YSLECT ('ONNN', IGR1, 0, IBUFF1, IRET)
      IF (IRET.NE.0) GO TO 980
      IF (IGR1.NE.IGR2) THEN
         CALL YSLECT ('ONNN', IGR2, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL YSLECT ('ONNN', IGR3, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         CALL YSLECT ('ONNN', IGR4, 0, IBUFF1, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       set up image display
      DO 10 I = 1,NMXIMG*MAXCMP
         FUNCTY(I) = ' '
 10      CONTINUE
C                                       prepare menu
      J = 0
      DO 15 I = 1,MAXCMP
         LEAVE2(J+1) = .FALSE.
         LEAVE2(J+2) = .FALSE.
         LEAVE2(J+3) = .FALSE.
         LEAVE2(J+4) = .FALSE.
         LEAVE2(J+5) = .FALSE.
         LEAVE2(J+6) = .FALSE.
         LEAVE2(J+7) = .FALSE.
         LEAVE2(J+8) = .FALSE.
         LEAVE2(J+9) = .FALSE.
         LEAVE2(J+10) = .FALSE.
         IF (SPIXDO.GT.0) THEN
            LEAVE2(J+9) = .FALSE.
            LEAVE2(J+10) = .FALSE.
            END IF
         WRITE (CHOIC2(J+1),1015) 'P1_', I
         WRITE (CHOIC2(J+2),1015) 'TH_', I
         WRITE (CHOIC2(J+3),1015) 'RM_', I
         IF (SPIXDO.GE.2) THEN
            J = J + 1
            WRITE (CHOIC2(J+3),1015) 'BE_', I
         ELSE IF (SPIXDO.EQ.1) THEN
            J = J + 1
            WRITE (CHOIC2(J+3),1015) 'SP_', I
            END IF
         WRITE (CHOIC2(J+4),1015) 'Q0_', I
         WRITE (CHOIC2(J+5),1015) 'U0_', I
         WRITE (CHOIC2(J+6),1015) 'EP1', I
         WRITE (CHOIC2(J+7),1015) 'ETH', I
         WRITE (CHOIC2(J+8),1015) 'ERM', I
         IF (SPIXDO.GE.2) THEN
            J = J + 1
            WRITE (CHOIC2(J+8),1015) 'EBE_', I
         ELSE IF (SPIXDO.EQ.1) THEN
            J = J + 1
            WRITE (CHOIC2(J+8),1015) 'ESP_', I
            END IF
         WRITE (CHOIC2(J+9),1015) 'EQ0', I
         WRITE (CHOIC2(J+10),1015) 'EU0', I
         J = J + NMXIMG - 2
 15      CONTINUE
      ISHELP = TSKNAM
      IMGOK = .FALSE.
      NG = NCOMPS
      NP = MMXIMG * NG
      MP = NMXIMG * NG
      MROWS(1) = NOPT1 - MXPAIR + (((NCOMPS-1)*NCOMPS)/2)
      MROWS(2) = NP
      DO 20 I = 1,MROWS(1)
         LEAVE(I) = LEAVE1(I)
         CHOICS(I) = CHOIC1(I)
 20      CONTINUE
      J = MROWS(1)
      DO 25 I = 1,MROWS(2)
         J = J + 1
         LEAVE(J) = LEAVE2(I)
         CHOICS(J) = CHOIC2(I)
 25      CONTINUE
C                                       does menu fit?
      CALL YWINDO ('READ', WIND, IRET)
      IF (IRET.NE.0) THEN
         WIND(1) = 1
         WIND(3) = MAXXTV(1)
         END IF
      MAXCH = (WIND(3) - WIND(1)) / CSIZTV(1)
      IF (MAXCH.GT.71) THEN
         J = NG + 1
      ELSE
         J = 2 * NG + 1
         END IF
      MAXCH = (WIND(4) - WIND(2)) / CSIZTV(2) / 1.5 - 1.5 * J
      MCOL = 2
      MTYPE = -1
      IF ((MAXCH.LE.MROWS(1)) .OR. (MAXCH.LE.MROWS(2))) THEN
         MTYPE = 0
         J = MAXCH / MMXIMG
         IF (J.GE.2) THEN
            MROWS(2) = MMXIMG * J
            MROWS(3) = MMXIMG * (NG - J)
            MCOL = 3
            END IF
         END IF
      NX = ITRC(1) - IBLC(1) + 1
      NY = ITRC(2) - IBLC(2) + 1
      CALL RFILL (4, 0.0, SNMIN)
      CALL RFILL (4, 0.0, POLMIN)
      MAXRES = 0.0
      NLIST = 0
      CALL RFILL (8, 0.0, MAXRM)
      CALL RFILL (8, 0.0, MAXSPX)
      CALL RFILL (4, 0.0, MAXDWD)
      SUBWIN(1) = 1
      SUBWIN(2) = 1
      SUBWIN(3) = NX
      SUBWIN(4) = NY
C                                       Get image memory
      NWORDS = (NX * NY * (MP+1) - 1) / 1024 + 1 + 4
      PIMAGE = 0
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, IMAGES, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING DYNAMIC MEMORY FOR IMAGES'
         GO TO 990
         END IF
C                                       build images
      IF (.NOT.IMGOK) THEN
         CALL GETIMG (NX, NY, MP, IMAGES(PIMAGE+1), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'BUILDING IMAGES IN RAM'
            GO TO 990
            END IF
         IMGOK = .TRUE.
         END IF
C                                       clear graphics 3 and 4
      IF (IGR3.NE.IGR1) THEN
         IP = IGR3
         CALL YZERO (IP, IRET)
         IF (IRET.NE.0) GO TO 980
         IP = IGR4
         CALL YZERO (IP, IRET)
         IF (IRET.NE.0) GO TO 980
         END IF
C                                       menu selection
 50   CALL YWINDO ('READ', WIND, IRET)
      IF (IRET.NE.0) THEN
         WIND(1) = 1
         WIND(3) = MAXXTV(1)
         END IF
      MAXCH = (WIND(3) - WIND(1)) / CSIZTV(1)
      IF ((NG.EQ.1) .AND. (MAXCH.GT.125)) THEN
         WRITE (TITLE(1),1050) DOCAT, MAXRES, SNMIN(1), POLMIN(1),
     *      MAXRM(1,1), MAXRM(2,1), MAXDWD(1), MAXSPX(1,1), SPINX,
     *      MAXSPX(2,1)
         J = 1
      ELSE
         WRITE (TITLE(1),1051) DOCAT, MAXRES
         J = 1
         DO 51 I = 1,NG
            J = J + 1
            IF (MAXCH.GT.100) THEN
               WRITE (TITLE(J),1052) I, SNMIN(I), POLMIN(I), MAXRM(1,I),
     *            MAXRM(2,I), MAXDWD(I), MAXSPX(1,I), SPINX, MAXSPX(2,I)
            ELSE IF (MAXCH.GT.76) THEN
               WRITE (TITLE(J),1053) I, SNMIN(I), POLMIN(I), MAXRM(1,I),
     *            MAXRM(2,I), MAXDWD(I), MAXSPX(1,I), SPINX, MAXSPX(2,I)
            ELSE
               WRITE (TITLE(J),1054) I, SNMIN(I), POLMIN(I), MAXRM(1,I),
     *            MAXRM(2,I)
               J = J + 1
               WRITE (TITLE(J),1055) MAXDWD(I), MAXSPX(1,I), SPINX,
     *            MAXSPX(2,I)
               END IF
 51         CONTINUE
         END IF
      IF (TTYIND.LE.0) THEN
         CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN TTY FOR INPUTS'
            TTYIND = 0
            GO TO 990
            END IF
         END IF
      CALL TVMENU (MTYPE, MCOL, MROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *   CHOICS, TIMLIM, LEAVE, J, TITLE, CHS, TVBUT, BUFF2, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RETURNED FROM TVMENU'
         GO TO 990
         END IF
C                                       never start with TV surpressed
      TVSUP = 0
C                                       case statement
C                                       exit
      IF (CHOICS(CHS).EQ.'EXIT') THEN
         MSGTXT = 'Bye-bye'
         CALL MSGWRT (2)
         GO TO 990
C                                       blank
      ELSE IF (CHOICS(CHS).EQ.' ') THEN
C                                       min S/N
      ELSE IF (CHOICS(CHS).EQ.'SET MIN S/N') THEN
         MSGBUF = 'Enter minimum P1 signal to noise ratio' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 53 I = 1,NG
               SNMIN(I) = XX(I)
 53            CONTINUE
            END IF
C                                       min S/N
      ELSE IF (CHOICS(CHS).EQ.'SET MIN P') THEN
         MSGBUF = 'Enter minimum P for' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 54 I = 1,NG
               POLMIN(I) = XX(I)
 54            CONTINUE
            END IF
C                                       max residual
      ELSE IF (CHOICS(CHS).EQ.'SET MAX RESID') THEN
         MSGBUF = 'Enter maximum residual in image units'
         CALL INQFLT (TTYLUN, MSGBUF, 1, XX, IRET)
         IF (IRET.EQ.0) MAXRES = XX(1)
C                                       max freq offset
      ELSE IF (CHOICS(CHS).EQ.'SET RM RANGE') THEN
         MSGBUF = 'Enter min and max Rotation Measure' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 55 I = 1,NG
               MAXRM(1,I) = XX(2*I-1)
               MAXRM(2,I) = XX(2*I)
 55            CONTINUE
            END IF
C                                       max width error
      ELSE IF (CHOICS(CHS).EQ.'SET MAX THETA ER') THEN
         MSGBUF = 'Enter max theta error in degrees' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 56 I = 1,NG
               MAXDWD(I) = XX(I)
 56            CONTINUE
            END IF
C                                       max freq offset
      ELSE IF (CHOICS(CHS).EQ.'SET SPIX RANGE') THEN
         MSGBUF = 'Enter min and max spectral index' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, 2*NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 57 I = 1,NG
               MAXSPX(1,I) = XX(2*I-1)
               MAXSPX(2,I) = XX(2*I)
 57         CONTINUE
            END IF
C                                       max freq offset
      ELSE IF (CHOICS(CHS).EQ.'SET THICKNESS MAX') THEN
         MSGBUF = 'Enter max thickness' //
     *      ' each component'
         CALL INQFLT (TTYLUN, MSGBUF, NG, XX, IRET)
         IF (IRET.EQ.0) THEN
            DO 157 I = 1,NG
               MAXSPX(1,I) = 0.0
               MAXSPX(2,I) = XX(I)
 157        CONTINUE
            END IF
C                                       redo all
      ELSE IF (CHOICS(CHS).EQ.'REDO ALL') THEN
         DOIT = MAXRES.GT.0.0
         DO 58 IG = 1,NG
            IF (SNMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (POLMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXDWD(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXRM(1,IG).LT.MAXRM(2,IG)) DOIT = .TRUE.
            IF (MAXSPX(1,IG).LT.MAXSPX(2,IG)) DOIT = .TRUE.
 58         CONTINUE
         IF (.NOT.DOIT) THEN
            MSGTXT = 'Min S/N, min pol, max residual, width, offset,'
     *         // ' spix must be set'
            CALL MSGWRT (6)
         ELSE
            CALL YZERO (IGR5, IRET)
            CALL YZERO (1, IRET)
            CALL COPY (256, CATBLK, CATEMP)
            CALL COPY (256, CATOLD, CATBLK)
            CALL UPDALL ('REDO', SNMIN, POLMIN, MAXRES, MAXRM, MAXDWD,
     *         MAXSPX, NX, NY, IMAGES(PIMAGE+1), IRET)
            CALL COPY (256, CATEMP, CATBLK)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-DOING ALL'
               GO TO 990
               END IF
            END IF
C                                       flag all
      ELSE IF (CHOICS(CHS).EQ.'FLAG ALL') THEN
         DOIT = MAXRES.GT.0.0
         DO 59 IG = 1,NG
            IF (SNMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (POLMIN(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXDWD(IG).GT.0.0) DOIT = .TRUE.
            IF (MAXRM(1,IG).LT.MAXRM(2,IG)) DOIT = .TRUE.
            IF (MAXSPX(1,IG).LT.MAXSPX(2,IG)) DOIT = .TRUE.
 59         CONTINUE
        IF (.NOT.DOIT) THEN
            MSGTXT = 'Min S/N, min pol, max residual, width, offset,'
     *         // ' spix must be set'
            CALL MSGWRT (6)
         ELSE
            CALL UPDALL ('FLAG', SNMIN, POLMIN, MAXRES, MAXRM, MAXDWD,
     *         MAXSPX, NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FLAGGING ALL'
               GO TO 990
               END IF
            END IF
C                                       offzoom
      ELSE IF (CHOICS(CHS).EQ.'OFF ZOOM') THEN
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF ZOOM'
            GO TO 990
            END IF
C                                       offtr
      ELSE IF (CHOICS(CHS).EQ.'OFF TRANSFER') THEN
         IC = 2 ** NGRAY - 1
         ICOLOR = 7
         NLEVS = MAXINT + 1
         SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
         DO 67 I = 1,NLEVS
            IBUFF1(I) = I * SLOPE - 0.5
 67         CONTINUE
         CALL YLUT ('WRIT', IC, ICOLOR, .FALSE., IBUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF BLACK&WHITE TRANSFER'
            GO TO 990
            END IF
         I = OFMINP + 1
         ICOLOR = 7
         CALL RFILL (I, 0.0, BUFF1)
         NLEVS = LUTOUT + 1
         IF (NLEVS.GT.OFMINP) NLEVS = OFMINP + 1
         OFFS = 1.0 / REAL (OFMOUT)
         SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
         DO 167 I = 1,NLEVS
            BUFF1(I) = I * SLOPE - OFFS
 167        CONTINUE
         I = OFMINP + 1
         JJ = NLEVS
         I = I / NLEVS
         DO 267 II = 2,I
            CALL RCOPY (NLEVS, BUFF1, BUFF1(JJ+1))
            JJ = JJ + NLEVS
 267        CONTINUE
         CALL YOFM ('WRIT', ICOLOR, .FALSE., BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TUNING OFF PSEUDO-COLOR'
            GO TO 990
            END IF
C                                       do-output
      ELSE IF (CHOICS(CHS).EQ.'SET DOOUTPUT') THEN
         DOCAT = DOCAT + 1
         IF (DOCAT.GT.3) DOCAT = 0
C                                       add to list
      ELSE IF (CHOICS(CHS).EQ.'ADD TO LIST') THEN
         IF (NLIST.GE.MAXLIS) THEN
            MSGTXT = 'LIST IS FULL'
            CALL MSGWRT (6)
         ELSE
            MSGBUF = 'Enter X and Y pixels to add to list: 2 integers'
            CALL INQINT (TTYLUN, MSGBUF, 2, IPOS, IRET)
            IF (IRET.NE.0) GO TO 50
            IF ((IPOS(1).LE.-1) .AND. (IPOS(1).GE.-NX) .AND.
     *         (IPOS(2).LE.-1) .AND. (IPOS(2).GE.-NY)) THEN
               IPOS(1) = -IPOS(1)
               IPOS(2) = -IPOS(2)
               DO 65 I = 1,NLIST
                  IF ((IPOS(1).EQ.PIXLIS(1,I)) .AND.
     *               (IPOS(2).EQ.PIXLIS(2,I))) GO TO 165
 65               CONTINUE
 165           IF (I.GT.NLIST) THEN
                  WRITE (MSGTXT,1165) IPOS, NX, NY
                  CALL MSGWRT (6)
               ELSE
                  DO 265 I1 = I+1,NLIST
                     PIXLIS(1,I) = PIXLIS(1,I1)
                     PIXLIS(2,I) = PIXLIS(2,I1)
                     I = I + 1
 265                 CONTINUE
                  NLIST = NLIST - 1
                  END IF
            ELSE IF ((IPOS(1).LT.1) .OR. (IPOS(1).GT.NX) .OR.
     *         (IPOS(2).LT.1) .OR. (IPOS(2).GT.NY)) THEN
               WRITE (MSGTXT,1265) IPOS, NX, NY
               CALL MSGWRT (6)
            ELSE
               NLIST = NLIST + 1
               PIXLIS(1,NLIST) = IPOS(1)
               PIXLIS(2,NLIST) = IPOS(2)
               END IF
            END IF
C                                       list list
      ELSE IF (CHOICS(CHS).EQ.'SHOW LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            I1 = 1
 66         I2 = MIN (NLIST, I1+3)
            IF (I2.GE.I1) THEN
               WRITE (MSGTXT,1066) (PIXLIS(1,I), PIXLIS(2,I), I = I1,I2)
               IF (I2-I1.LT.3) THEN
                  J = JTRIM (MSGTXT)
                  IF (MSGTXT(J:J).EQ.'(') MSGTXT(J:) = ' '
                  END IF
               CALL MSGWRT (2)
               I1 = I2 + 1
               GO TO 66
               END IF
            END IF
C                                       redo list
      ELSE IF (CHOICS(CHS).EQ.'REDO LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            CALL YZERO (IGR5, IRET)
            CALL YZERO (1, IRET)
            CALL COPY (256, CATBLK, CATEMP)
            CALL COPY (256, CATOLD, CATBLK)
            CALL UPDLIS ('REDO', NX, NY, IMAGES(PIMAGE+1), IRET)
            CALL COPY (256, CATEMP, CATBLK)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'UPDATING FIT OF PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       flag list
      ELSE IF (CHOICS(CHS).EQ.'FLAG LIST') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            CALL UPDLIS ('FLAG', NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'UPDATING FIT OF PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       swap list
      ELSE IF (CHOICS(CHS)(:10).EQ.'SWAP LIST ') THEN
         IF (NLIST.LE.0) THEN
            MSGTXT = 'List is empty'
            CALL MSGWRT (6)
         ELSE
            IP = CHS - NOPT1 + (((MAXCMP-1)*MAXCMP)/2)
            CALL LISWAP (IP, NX, NY, IMAGES(PIMAGE+1), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'SWAPPING PIXEL LIST'
               GO TO 990
               END IF
            END IF
C                                       display image
      ELSE
         IP = CHS - MROWS(1)
C                                       deal with omitting SP, BE
         IF (NP.LT.MP) THEN
            I1 = MOD (IP-1, MMXIMG) + 1
            I2 = (IP-1) / MMXIMG
            IF (I1.GT.8) THEN
               I1 = I1 + 2
            ELSE IF (I1.GT.3) THEN
               I1 = I1 + 1
               END IF
            IP = I2 * NMXIMG + I1
            END IF
         CALL SHOIMG (MTYPE, IP, NX, NY, IGR5, SPIXDO, IMAGES(PIMAGE+1),
     *         IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'SHOWING AN IMAGE'
            GO TO 990
         ELSE IF (IRET.LT.0) THEN
            IMGOK = .FALSE.
            IRET = 0
            END IF
         END IF
      GO TO 50
C                                       TV function failure
 980  WRITE (MSGTXT,1000) IRET, 'TV INIT FUNCTIONS'
C
 990  IF ((IRET.NE.0) .AND. (IRET.NE.99)) CALL MSGWRT (8)
      IF (PIMAGE.NE.0) CALL ZMEMRY ('FRAL', TSKNAM, NWORDS, IMAGES,
     *   PIMAGE, I)
      IF (TTYIND.GT.0) THEN
         CALL ZCLOSE (TTYLUN, TTYIND, J)
         TTYIND = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFITV: ERROR',I4,' ON ',A)
 1015 FORMAT ('SHOW IMAGE ',A,I1)
 1050 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5,2X,'COMP 1: SNMIN=',F4.1,
     *   F7.1,'  P1_MIN',F8.5,' < RM <',F6.1,2X,'MAXDTHETA=',F5.1,F7.2,
     *   ' < ',A,' <',F7.2)
 1051 FORMAT ('DOOUT=',I1,2X,'MAXRES=',F8.5)
 1052 FORMAT ('COMP',I2,': SNMIN=',F4.1,'  P1_MIN=',
     *   F8.5,F7.1,' < RM <',F6.1,2X,'MAXDTHETA=',F5.1,F7.2,
     *   ' < ',A,' <',F7.2)
 1053 FORMAT ('C',I2,' SN=',F4.1,' P1_MIN=',F8.4,F6.1,'<RM<',F5.1,1X,
     *   'MXDT=',F5.1,F6.2,'<',A,'<',F6.2)
 1054 FORMAT ('COMP',I2,': SNMIN=',F4.1,'  P1_MIN=',F8.5,F7.1,
     *   ' < RM <',F6.1)
 1055 FORMAT (8X,'MAXDTHETA=',F5.1,F7.2,' < ',A,' <',F7.2)
 1165 FORMAT ('POSITION',2I6,' NOT FOUND IN THE PIXEL LIST')
 1265 FORMAT ('POSITION',2I6,' OUTSIDE 1-',I5,' 1-',I5)
 1066 FORMAT (4('(',I5,',',I5,')',3X))
      END
      SUBROUTINE LISWAP (IP, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   LISWAP swaps values at a list of pixels
C   Inputs:
C      IP       I      Index into array of comps to swap
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters
C   In/out (common):
C      NLIST    I      Number of pairs in list -> 0 if all are done
C   Output:
C      IRET     I      Error: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IP, NX, NY, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'RMFIT.INC'
      INTEGER   ILIST, IP1, IP2, IX, IY, I, LRMRNO, NCMP, YZPOS(2), LP1,
     *   LP2, LGSWAP(2,MXPAIR), INLIST
      REAL      TEMP, RESULT(MAXPRM,2), IAVG, PAVG, TMPRMS(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA LGSWAP /1, 2, 1, 3, 2, 3, 1, 4, 2, 4, 3, 4/
C-----------------------------------------------------------------------
      INLIST = NLIST
      IP1 = (LGSWAP(1,IP) - 1) * NMXIMG
      IP2 = (LGSWAP(2,IP) - 1) * NMXIMG
      LP1 = (LGSWAP(1,IP) - 1) * 4
      LP2 = (LGSWAP(2,IP) - 1) * 4
C                                       swap list
      DO 50 ILIST = 1,INLIST
         IX = PIXLIS(1,INLIST+1-ILIST)
         IY = PIXLIS(2,INLIST+1-ILIST)
         IF ((IX.GT.0) .AND. (IX.LE.NX) .AND. (IY.GT.0) .AND.
     *      (IY.LE.NY)) THEN
            LRMRNO = (IY-1) * NX + IX
            IRMRNO = LRMRNO
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, YZPOS,
     *         NCMP, IAVG, PAVG, RESULT, TMPRMS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
            ELSE IF (IRET.EQ.0) THEN
C                                       swap image values
               DO 20 I = 1,NMXIMG
                  TEMP = IMAGE(IX,IY,IP1+I)
                  IMAGE(IX,IY,IP1+I) = IMAGE(IX,IY,IP2+I)
                  IMAGE(IX,IY,IP2+I) = TEMP
 20               CONTINUE
C                                       swap table values too
               DO 30 I = 1,4
                  TEMP = RESULT(LP1+I,1)
                  RESULT(LP1+I,1) = RESULT(LP2+I,1)
                  RESULT(LP2+I,1) = TEMP
                  TEMP = RESULT(LP1+I,2)
                  RESULT(LP1+I,2) = RESULT(LP2+I,2)
                  RESULT(LP2+I,2) = TEMP
 30               CONTINUE
               IRMRNO = LRMRNO
               CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *            YZPOS, NCMP, IAVG, PAVG, RESULT, TMPRMS, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
                  GO TO 990
                  END IF
               END IF
            END IF
         NLIST = NLIST - 1
 50      CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LISWAP ERROR',I4,' ON ',A)
      END
      SUBROUTINE GETIMG (NX, NY, NP, IMAGE, IRET)
C-----------------------------------------------------------------------
C   GETIMG reads the RM file and makes images of the parameters
C   Inputs:
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C      NP      I      Number Z pixels in image (# gaussian parms + flux)
C   Output:
C      IMAGE   R(*)   Images of NP parameters
C      IRET    I      Error code
C      CATBLK  I(*)   in COMMON - output file header for images
C-----------------------------------------------------------------------
      INTEGER   NX, NY, NP, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'RMFITD.INC'
      INTEGER   I1, I2, IP, MP, YZPOS(2), NCMP, I, NAX, J, K
      REAL      IAVG, PAVG, RESULT(MAXPRM,2), XBLC(7), XTRC(7)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Reading RM table to fill images'
      CALL MSGWRT (2)
C                                       blank fill
      I1 = NX * NY * NP
      CALL RFILL (I1, FBLANK, IMAGE)
C                                       blotch plane
      I1 = NX * NY
      CALL RFILL (I1, 0.0, IMAGE(1,1,NP+1))
C                                       loop through table
      MP = NP / NMXIMG
      IRMRNO = 1
      DO 50 I2 = 1,NY
         DO 40 I1 = 1,NX
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, YZPOS,
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NCMP.GT.0)) THEN
               J = 0
               K = 0
               DO 20 IP = 1,MP
                  IMAGE(I1,I2,J+1) = RESULT(K+1,1)
                  IMAGE(I1,I2,J+2) = RESULT(K+2,1)
                  IMAGE(I1,I2,J+3) = RESULT(K+3,1)
                  IMAGE(I1,I2,J+4) = RESULT(K+4,1)
C                                       errors
                  IMAGE(I1,I2,J+7) = RESULT(K+1,2)
                  IMAGE(I1,I2,J+8) = RESULT(K+2,2)
                  IMAGE(I1,I2,J+9) = RESULT(K+3,2)
                  IMAGE(I1,I2,J+10) = RESULT(K+4,2)
C                                       Q0, U0 & error
                  IF ((RESULT(K+1,1).EQ.FBLANK) .OR.
     *                (RESULT(K+2,1).EQ.FBLANK)) THEN
                     IMAGE(I1,I2,J+5) = FBLANK
                     IMAGE(I1,I2,J+6) = FBLANK
                     IMAGE(I1,I2,J+11) = FBLANK
                     IMAGE(I1,I2,J+12) = FBLANK
                  ELSE
                     IMAGE(I1,I2,J+5) = RESULT(K+1,1) *
     *                  COS (2.0D0 * DG2RAD * RESULT(K+2,1))
                     IMAGE(I1,I2,J+6) = RESULT(K+1,1) *
     *                  SIN (2.0D0 * DG2RAD * RESULT(K+2,1))
                     IMAGE(I1,I2,J+11) = SQRT ((RESULT(K+1,2) *
     *                  COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *                  (RESULT(K+1,1) * 2.0D0 * DG2RAD * RESULT(K+2,2)
     *                  * SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                     IMAGE(I1,I2,J+12) = SQRT ((RESULT(K+1,2) *
     *                  SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *                  (RESULT(K+1,1) * 2.0D0 * DG2RAD * RESULT(K+2,2)
     *                  * COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                     END IF
                  J = J + NMXIMG
                  K = K + 4
 20               CONTINUE
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       make a header
      CALL COPY (256, CATOLD, CATBLK)
C                                       Get user modification to CATBLK
      CALL RCOPY (7, BLC, XBLC)
      CALL RCOPY (7, TRC, XTRC)
      XBLC(2) = IBLC(1)
      XBLC(3) = IBLC(2)
      XTRC(2) = ITRC(1)
      XTRC(3) = ITRC(2)
      CALL SUBHD3 (XBLC, XTRC, 1.0, 1.0, 1.0)
C                                       Basic output header: results
      CATBLK(KIDIM) = CATBLK(KIDIM) - 1
      NAX = CATBLK(KIDIM)
      DO 80 I = 1,NAX
         CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
         CATR(KRCRP+I-1) = CATR(KRCRP+I)
         CATR(KRCRT+I-1) = CATR(KRCRT+I)
         CATR(KRCIC+I-1) = CATR(KRCIC+I)
         CATD(KDCRV+I-1) = CATD(KDCRV+I)
         CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1,
     *      CATH(KHCTP+(I-1)*2))
 80      CONTINUE
      DO 85 I = NAX,6
         CATBLK(KINAX+I) = 1
 85      CONTINUE
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETIMG: ERROR',I4,' ON ',A)
      END
      SUBROUTINE SHOIMG (MTYP, IP, NX, NY, IGR5, SPIXDO, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SHOIMG displays an image plane on the TV screen and allows an
C   interactive transfer function, coloring, CURVALUE, and EXIT.
C   Inputs:
C      MTYP    I      -1 => menu split, else menu all on left
C      IP      I      Desired plane
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C      SPIXDO  I      2 +> thickness not SPIX
C      IMAGE   R(*)   Images of NP parameters
C   Output:
C      IRET    I      Error code
C-----------------------------------------------------------------------
      INTEGER   MTYP, IP, NX, NY, IGR5, SPIXDO, IRET
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER   NOPTS
      PARAMETER (NOPTS=11+MAXCMP)
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PTVC.INC'
C
      INTEGER   IX, IY, TVWIN(4), IWIN(4), NPIX, PLINC, IYTV, HORIZ,
     *   IBUFF(MABFSS), NLEVS, MCOL, MROWS, MTYPE, TIMLIM, TOPSEP, I,
     *   GRCHS(2), TVBUT, CHS, ITR, LUTBUF(TVMLUT), JJ, II, LG, LTY, J,
     *   LGSWAP(2,MAXCMP), JT, JTRIM, NW, EX(5), EY(5), NXFRAM, NYFRAM,
     *   CFRAME, TFRAME, PINC, LNX, LNY, IC(2), NPIXW, WXPOS, SIDSEP,
     *   MINC, MPIX, JBUFF(MABFSS), POFF, NEDGE, JNX, JNY, NBO, MBOX,
     *   IGR
      CHARACTER TRANFN*2, CHOICS(NOPTS+1)*12, ISHELP*8, TITLE*132,
     *   TVALS(NMXIMG)*16, CHTEMP*8, FUNCS(4)*2, BUNITS*8
      REAL      PMIN, PMAX, RPOS(2), SLOPE, OFFS, TEMP, BLCO(7), TRCO(7)
      LOGICAL   LEAVE(NOPTS+1), DOWEDG, DOEDGE
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (IBUFF, BUFF2, LUTBUF), (JBUFF, BUFF3)
      DATA MCOL, MTYPE, TIMLIM, TOPSEP, SIDSEP /1, -1, 0, 3, 10/
      DATA CHOICS /'RETURN', ' ','LOAD AS', 'SET WINDOW','RESET WINDOW',
     *   'OFF TRANSF', 'OFF COLOR', 'TVTRANSF', 'TVPSEUDO', 'TVPHLAME',
     *   'TVZOOM', 'CURVALUE', MAXCMP*' '/
      DATA LEAVE /3*.FALSE., .TRUE., .FALSE.,2*.TRUE., 4*.FALSE.,
     *   MAXCMP*.FALSE., .FALSE./
      DATA TVALS /'P-ONE', 'THETA', 'ROT.MEAS', 'SPIX', 'QZERO',
     *   'UZERO', 'ERR P1', 'ERR THETA', 'ERR ROT.MEAS', 'ERROR SP',
     *   'ERROR Q0', 'ERROR U0'/
      DATA FUNCS /'LN', 'SQ', 'LG', 'L2'/
C-----------------------------------------------------------------------
      IF (SPIXDO.GE.2) THEN
         TVALS(4) = 'THICKNESS'
         TVALS(10) = 'ERR THICK'
         END IF
C                                       compute swap lists
      LG = (IP-1) / NMXIMG + 1
      LTY = MOD (IP-1, NMXIMG) + 1
      JJ = 0
      II = NOPTS - MAXCMP + 1
      J = LG
      DO 5 I = 1,NCOMPS
         IF (I.NE.J) THEN
            JJ = JJ + 1
            II = II + 1
            LGSWAP(1,JJ) = J
            LGSWAP(2,JJ) = I
            WRITE (CHOICS(II),1005) J, I
            END IF
 5       CONTINUE
      MROWS = II
C                                       find max/min
 10   JNX = SUBWIN(3) - SUBWIN(1)
      JNY = SUBWIN(4) - SUBWIN(2)
      PMIN = 1.E15
      PMAX = -PMIN
      DO 20 IY = SUBWIN(2),SUBWIN(4)
         DO 15 IX = SUBWIN(1),SUBWIN(3)
            IF (IMAGE(IX,IY,IP).NE.FBLANK) THEN
               PMIN = MIN (PMIN, IMAGE(IX,IY,IP))
               PMAX = MAX (PMAX, IMAGE(IX,IY,IP))
               END IF
 15         CONTINUE
 20      CONTINUE
C                                       too big for TV?
      NXFRAM = (JNX - 1) / (MAXXTV(1)-33) + 1
      NYFRAM = (JNY - 1) / (MAXXTV(2)-33) + 1
      TFRAME = NXFRAM * NYFRAM
      CFRAME = 0
      PINC = MAX (NXFRAM, NYFRAM)
      LNX = JNX / PINC
      LNY = JNY / PINC
      MINC = 1
      IF (PINC.EQ.1) THEN
         JJ = 256
         IF ((MAXXTV(1).GT.650) .AND. (MAXXTV(2).GT.650)) JJ = 512
         IF ((2*JNX.LE.JJ) .AND. (2*JNY.LE.JJ)) THEN
            MINC = JJ / JNX
            IF (JJ/JNY.LT.MINC) MINC = JJ / JNY
            MINC = MIN (20, MINC)
            LNX = MINC * JNX
            LNY = MINC * JNY
            END IF
         END IF
      IC(1) = SUBWIN(1)
      IC(2) = SUBWIN(2)
C                                       menu list
      POFF = 0
      IF (TFRAME.GT.1) THEN
         MROWS = MROWS + 1
         CHOICS(MROWS) = 'NEXT WINDOW'
C                                       menu split
      ELSE IF (MTYP.NE.-1) THEN
         NEDGE = (CSIZTV(1) + 1) / 2
         POFF = 2 * (4 + 3*NEDGE) + 42 * CSIZTV(1)
         POFF = POFF + 7 + NEDGE
         IF (POFF+LNX.GT.WINDTV(3)-WINDTV(1)) POFF = 0
         END IF
C                                       width of wedge
      NW = MIN (JNY, 16)
C                                       no real image
      IF (PMAX.LT.PMIN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND'
         CALL MSGWRT (7)
         IRET = -1
C                                       okay do it
      ELSE
         TRANFN = FUNCTY(IP)
         ITR = 1
         DO 30 I = 2,4
            IF (TRANFN.EQ.FUNCS(I)) ITR = I
 30         CONTINUE
         ITR = MOD (ITR, 4) + 1
C                                       header adjust
         CALL H2CHR (8, 1, OLDH(KHBUN,1), BUNITS)
         IF (MOD(IP-1,6).EQ.1) THEN
            CALL CHR2H (8, 'DEGREES ', 1, CATH(KHBUN))
         ELSE IF (MOD(IP-1,6).EQ.2) THEN
            CALL CHR2H (8, 'RAD/M/M ', 1, CATH(KHBUN))
         ELSE IF (MOD(IP-1,6).EQ.3) THEN
            IF (SPIXDO.GE.2) THEN
               CALL CHR2H (8, 'RAD/M/M ', 1, CATH(KHBUN))
            ELSE IF (SPIXDO.EQ.1) THEN
               CALL CHR2H (8, 'SpecIndx', 1, CATH(KHBUN))
               END IF
         ELSE
            CALL CHR2H (8, BUNITS, 1, CATH(KHBUN))
            END IF
         CATR(KRDMX) = PMAX
         CATR(KRDMN) = PMIN
         CATR(IRRAN+1) = PMAX
         CATR(IRRAN) = PMIN
         TEMP = MAX (ABS(PMIN), ABS(PMAX))
         IF ((TEMP.LT.1000.) .AND. (TEMP.GT.0.001)) THEN
            WRITE (MSGTXT,1020) LG, PMIN, PMAX, TVALS(LTY)
         ELSE
            WRITE (MSGTXT,1021) LG, PMIN, PMAX, TVALS(LTY)
            END IF
         CALL MSGWRT (2)
C                                       window
 50      DOWEDG = .FALSE.
         DOEDGE = (LNX.LT.MAXXTV(1)-2) .AND. (LNY.LT.MAXXTV(2)-2)
         IF (LNX.LE.MAXXTV(1)) THEN
            IWIN(1) = IC(1)
            IWIN(3) = IC(1) - 1 + (LNX/MINC) * PINC
            TVWIN(1) = (MAXXTV(1) - POFF - LNX) / 2 + POFF
            TVWIN(3) = TVWIN(1) + LNX - 1
            END IF
         IF (LNY.LE.MAXXTV(2)) THEN
            IF (LNY.LE.MAXXTV(2)-24) THEN
               IY = (NW * 2) / 3
               TVWIN(2) = (MAXXTV(2)-IY - LNY) / 2 + 16
               TVWIN(4) = TVWIN(2) + LNY - 1
               DOWEDG = .TRUE.
            ELSE
               TVWIN(2) = (MAXXTV(2) - LNY) / 2
               TVWIN(4) = TVWIN(2) + LNY - 1
               END IF
            IWIN(2) = IC(2)
            IWIN(4) = IC(2) - 1 + (LNY/MINC) * PINC
            END IF
         CALL COPY (4, IWIN, CATBLK(IIWIN))
         CALL COPY (4, TVWIN, CATBLK(IICOR))
C                                       not from disk
         CATBLK(IIVOL) = 0
         CATBLK(IICNO) = 0
         IPL(1) = 1
         IPL(2) = 1
         CALL YZERO (IGR5, IRET)
         CALL YZERO (IPL, IRET)
         IF (TFRAME.GT.1) THEN
            IF (PINC.EQ.1) THEN
               WRITE (MSGTXT,1050) CFRAME
            ELSE
               WRITE (MSGTXT,1051) PINC
               END IF
            CALL MSGWRT (2)
            END IF
C                                       return here to reload
 60      CALL CHR2H (2, TRANFN, 1, CATH(IITRA))
         CHOICS(3)(9:10) = FUNCS(ITR)
         FUNCTY(IP) = TRANFN
         CALL YCWRIT (IPL(1), TVWIN, CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE IMAGE CATALOG'
            GO TO 990
            END IF
         CALL YHOLD ('ONNN', IRET)
         CALL YSLECT ('ONNN', IPL, 0, SCRTCH, IRET)
C                                       write TV image
         IYTV = TVWIN(2) - 1
         NPIX = (IWIN(3) - IWIN(1)) / PINC + 1
         MPIX = MINC * NPIX
         IF (MPIX.GT.17) THEN
            NPIXW = MPIX
            WXPOS = TVWIN(1)
         ELSE
            NPIXW = 17
            WXPOS = TVWIN(1) - (18-NPIX)/2
            WXPOS = MAX (1, WXPOS)
            END IF
         PLINC = 1
         HORIZ = 0
         DO 70 IY = IWIN(2),IWIN(4),PINC
            IYTV = IYTV + 1
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIX*PINC, PINC,
     *         IMAGE(IWIN(1),IY,IP), IBUFF)
            IF (MINC.EQ.1) THEN
               CALL YIMGIO ('WRIT', IPL(1), TVWIN(1), IYTV, HORIZ, NPIX,
     *            IBUFF, IRET)
            ELSE
               DO 64 I = 1,NPIX
                  CALL FILL (MINC, IBUFF(I), JBUFF(MINC*(I-1)+1))
 64               CONTINUE
               IYTV = IYTV - 1
               DO 65 I = 1,MINC
                  IYTV = IYTV + 1
                  IF (IRET.EQ.0) CALL YIMGIO ('WRIT', IPL(1), TVWIN(1),
     *               IYTV, HORIZ, MPIX, JBUFF, IRET)
 65               CONTINUE
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING IMAGE TO TV'
               GO TO 990
               END IF
 70         CONTINUE
         IF (DOWEDG) THEN
            IYTV = TVWIN(2) - (NW+1)/2
            SLOPE = (PMAX - PMIN) / (NPIXW - 1)
            DO 80 IY = 1,NPIXW
               BUFF1(IY) = (IY - 1.0) * SLOPE + PMIN
 80            CONTINUE
            CALL ISCALE (TRANFN, MAXINT, CATR(IRRAN), NPIXW, 1, BUFF1,
     *         IBUFF)
            DO 90 IY = 1,NW
               IYTV = IYTV - 1
               CALL YIMGIO ('WRIT', IPL(1), WXPOS, IYTV, HORIZ, NPIXW,
     *            IBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING WEDGE TO TV'
                  GO TO 990
                  END IF
 90            CONTINUE
            END IF
C                                       line around
         IF (DOEDGE) THEN
            EX(1) = TVWIN(1) - 1
            EY(1) = TVWIN(2) - 1
            EX(3) = TVWIN(3) + 1
            EY(3) = TVWIN(4) + 1
            EX(2) = EX(3)
            EY(2) = EY(1)
            EX(4) = EX(1)
            EY(4) = EY(3)
            EX(5) = EX(1)
            EY(5) = EY(1)
            IGR5 = NGRAY + 5
            CALL YSLECT ('ONNN', IGR5, 0, IBUFF, IRET)
            IF (IRET.EQ.0) CALL IMVECT ('ONNN', IGR5, 5, EX, EY, IBUFF,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'DRAWING EDGE LINE AROUND' //
     *            ' THE IMAGE'
               GO TO 990
               END IF
            END IF
         CALL YHOLD ('OFFF', IRET)
         NLEVS = LUTOUT + 1
         ISHELP = TSKNAM
C                                       simple menu
C                                       menu selection
         IX = MOD (IP - 1, NMXIMG) + 1
         IY = (IP - 1) / NMXIMG + 1
         CALL H2CHR (8, 1, CATH(KHBUN), CHTEMP)
         IF ((IX.EQ.2) .OR. (IX.EQ.8)) CHTEMP = 'DEGREES'
         IF ((IX.EQ.3) .OR. (IX.EQ.9)) CHTEMP = 'RAD/M/M'
         IF ((IX.EQ.4) .OR. (IX.EQ.10)) THEN
            IF (SPIXDO.EQ.1) THEN
               CHTEMP = 'SPIX'
            ELSE IF (SPIXDO.GE.2) THEN
               CHTEMP = 'RAD/M/M'
               END IF
            END IF
         JT = JTRIM (TVALS(IX))
         TEMP = MAX (ABS(PMIN), ABS(PMAX))
         IF ((TEMP.LT.10000.) .AND. (TEMP.GT.0.001)) THEN
            WRITE (TITLE,1090) IY, CFRAME, TVALS(IX)(:JT), PMIN, PMAX,
     *          CHTEMP(:JTRIM(CHTEMP))
         ELSE
            WRITE (TITLE,1091) IY, CFRAME, TVALS(IX)(:JT), PMIN, PMAX,
     *          CHTEMP(:JTRIM(CHTEMP))
            END IF
         IF (TFRAME.LE.1) TITLE(15:18) = ' '
         CALL REFRMT (TITLE, '_', I)
         GRCHS(1) = 1
         GRCHS(2) = 2
         RPOS(1) = MAXXTV(1) / 2
         RPOS(2) = MAXXTV(2) / 2
 100     CALL TVMENU (MTYPE, MCOL, MROWS, GRCHS, TOPSEP, SIDSEP, ISHELP,
     *      CHOICS, TIMLIM, LEAVE, 1, TITLE, CHS, TVBUT, BUFF2, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RETURNED FROM TVMENU'
            GO TO 990
            END IF
C                                       return to main menu
         IF (CHOICS(CHS).EQ.'RETURN') THEN
            MSGTXT = 'Returning to main menu'
            CALL MSGWRT (2)
            GO TO 999
C                                       blank
         ELSE IF (CHOICS(CHS).EQ.' ') THEN
C                                       set window
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            CALL YCINIT (IGR5, BUFF2)
            NBO = 0
            MBOX = 1
            CALL RCOPY (7, BLC, BLCO)
            CALL RCOPY (7, TRC, TRCO)
            IGR = IGR5 - NGRAY
            CALL GRBOXS (IGR, MBOX, NBO, BLCO, TRCO, BUFF2, IRET)
            IF (IRET.EQ.0) THEN
               SUBWIN(1) = BLCO(1) + 0.1
               SUBWIN(2) = BLCO(2) + 0.1
               SUBWIN(3) = TRCO(1) + 0.1
               SUBWIN(4) = TRCO(2) + 0.1
               WRITE (MSGTXT,1110) SUBWIN
               CALL MSGWRT (2)
               END IF
            GO TO 10
C                                       reset window
         ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
            SUBWIN(1) = 1
            SUBWIN(2) = 1
            SUBWIN(3) = NX
            SUBWIN(4) = NY
            GO TO 10
C                                       change transfer function
         ELSE IF (CHOICS(CHS)(:8).EQ.'LOAD AS ') THEN
            TRANFN = FUNCS(ITR)
            ITR = MOD (ITR, 4) + 1
            GO TO 60
C                                       TV transfer func OFF
         ELSE IF (CHOICS(CHS).EQ.'OFF TRANSF') THEN
            IYTV = MAXINT + 1
            SLOPE = (LUTOUT+1.0) / (MAXINT+1.0)
            DO 110 I = 1,IYTV
               LUTBUF(I) = I * SLOPE - 0.5
 110           CONTINUE
            I = 2 ** (IPL(1)-1)
            CALL YLUT ('WRIT', I, 7, .FALSE., LUTBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY OFF TRANSF'
               GO TO 990
               END IF
C                                       TV color func OFF
         ELSE IF (CHOICS(CHS).EQ.'OFF COLOR') THEN
            I = OFMINP + 1
            CALL RFILL (I, 0.0, BUFf1)
            NLEVS = LUTOUT + 1
            IF (I.LT.NLEVS) NLEVS = I
            OFFS = 1.0 / REAL (OFMOUT)
            SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
            DO 120 I = 1,NLEVS
               BUFF1(I) = I * SLOPE - OFFS
 120           CONTINUE
            I = (OFMINP + 1) / NLEVS
            JJ = NLEVS
            DO 130 II = 2,I
               CALL RCOPY (NLEVS, BUFF1, BUFF1(JJ+1))
               JJ = JJ + NLEVS
 130           CONTINUE
            CALL YOFM ('WRIT', 7, .FALSE., BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY OFF COLOR'
               GO TO 990
               END IF
C                                       TV transfer func
         ELSE IF (CHOICS(CHS).EQ.'TVTRANSF') THEN
            I = 2 ** (IPL(1)-1)
            IYTV = 1
            CALL IENHNS (I, 7, IYTV, RPOS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVTRANSF'
               GO TO 990
               END IF
C                                       TV pseudo colors
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL TVPSUD (NLEVS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVPSEUDO'
               GO TO 990
               END IF
C                                       TV flame colors
         ELSE IF (CHOICS(CHS).EQ.'TVPHLAME') THEN
            CALL TVFLAM (NLEVS, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVPHLAME'
               GO TO 990
               END IF
C                                       TV zoom
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL TVZOME (IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVZOOM'
               GO TO 990
               END IF
C                                       Cursor value: local version
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            CALL TVALUE (GRCHS(1), NX, NY, IMAGE(1,1,IP), NLIST, PIXLIS,
     *         BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RETURNED BY TVALUE'
               GO TO 990
               END IF
C                                       load next portion
         ELSE IF (CHOICS(CHS).EQ.'NEXT WINDOW') THEN
            CFRAME = CFRAME + 1
            IF (CFRAME.GT.TFRAME) THEN
               CFRAME = 0
               PINC = MAX (NXFRAM, NYFRAM)
               IC(1) = SUBWIN(1)
               IC(2) = SUBWIN(2)
               LNX = JNX / PINC
               LNY = JNY / PINC
            ELSE
               PINC = 1
               IF (NXFRAM.EQ.1) THEN
                  IC(1) = SUBWIN(1)
                  LNX = JNX
               ELSE
                  II = MOD (CFRAME-1, NXFRAM) + 1
                  IC(1) = (II - 1) * (MAXXTV(1) - 3) + SUBWIN(1)
                  IF (IC(1)+MAXXTV(1)-33.GT.NX) IC(1) = NX - MAXXTV(1)
     *               + 33
                  LNX = MAXXTV(1) - 33
                  END IF
               IF (NYFRAM.EQ.1) THEN
                  IC(2) = SUBWIN(2)
                  LNY = JNY
               ELSE
                  II = (CFRAME-1) / NXFRAM + 1
                  IC(2) = (II - 1) * (MAXXTV(2) - 3) + SUBWIN(2)
                  IF (IC(2)+MAXXTV(2)-33.GT.NY) IC(2) = NY - MAXXTV(2)
     *               + 33
                  LNY = MAXXTV(2) - 33
                  END IF
               END IF
            GO TO 50
C                                       blotch and swap
         ELSE IF (CHOICS(CHS)(:5).EQ.'SWAP ') THEN
            II = CHS - NOPTS + MAXCMP - 1
            CALL BLSWAP (LGSWAP(1,II), NX, NY, IMAGE, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TV BLOTCH AND SWAP COMPONENTS'
               GO TO 990
               END IF
            GO TO 10
            END IF
         GO TO 100
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SHOIMG: ERROR:',I4,' ON ',A)
 1005 FORMAT ('SWAP',I2,'-',I1)
 1020 FORMAT ('Loading component',I2,' image',F10.5,' to',F10.5,' of ',
     *   A)
 1021 FORMAT ('Loading comp #',I2,' image',1PE11.3,' to',1PE11.3,' of ',
     *   A)
 1050 FORMAT ('Loading every pixel in subimage',I3)
 1051 FORMAT ('Loading full image with only every',I3,
     *   ' pixels in X and Y')
 1090 FORMAT ('RM component',I2,'(',I2,') _',A,'_',F11.5,' TO',F11.5,
     *   '  (',A,')')
 1091 FORMAT ('RM component',I2,'(',I2,') _',A,'_',1PE11.3,' TO',
     *   1PE11.3,'  (',A,')')
 1110 FORMAT ('BLC/TRC=',4I7)
      END
      SUBROUTINE TVALUE (GR, NX, NY, IMAGE, NLIST, PIXLIS, BUFFER, IRET)
C-----------------------------------------------------------------------
C   TVALUE performs interactive displays of map image values:
C   Special version for RMFIT - allows picking pixels for list
C   Inputs:
C      GR        I        Graphics plane for lettering
C      NX        I        Number X pixels in image
C      NY        I        Number Y pixels in image
C      IMAGE     R(*)     Image values
C   In/out:
C      NLIST     I        Number entries in PIXLIS
C      PIXLIST   I(2,*)   List of pixels
C   Output:
C      BUFFER    I(*)     Scratch buffer
C      IRET      I        Basic TV error code
C-----------------------------------------------------------------------
      INTEGER   GR, NX, NY, NLIST, PIXLIS(2,*), IRET
      REAL      IMAGE(NX,NY), BUFFER(*)
C
      CHARACTER STRING*16, PREFIX*5, ITRTYP(8)*2, LMTYPS(2)*2, LMTYPE*2
      INTEGER   MIND, IG, IG1, IG2, ITW(3), NPIX, NROW, MAG, IX0, IY0,
     *   IX, IY, IP, ECOUNT, QUAD, IBUT, ITR, ICMASK, ZAND, ISCX,
     *   ISCY, I, INCNO, INVOL, LDEP(5), ITG1, ITG2, ITEMP, IX1, IY1,
     *   MSGSAV, LBUT
      REAL      PPOS(2), RPOS(2), PIXVAL, CORN(7)
      LOGICAL   T, F, EQUAL, DOIT, FROMTV, BLNKD, NOQUAD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ITRTYP /'LN','LG','L2','SQ','NE','NG','N2','NQ'/
      DATA LMTYPS /'WE','ZZ'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      MIND = 0
      QUAD = 0
      FROMTV = .TRUE.
      CALL ZTIME (ITW)
      MSGSAV = MSGSUP
      NOQUAD = .TRUE.
C                                       Turn on graphics
      IG1 = MIN (GR, NGRAPH)
      IF (IG1.LE.0) IG1 = MIN (2, NGRAPH)
      IG2 = 0
      IF (NGRAPH.GE.4) IG2 = NGRAPH
      ITG1 = NGRAY + IG1
      ITG2 = NGRAY + IG2
      CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      CALL YZERO (ITG1, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (IG2.NE.0) THEN
         CALL YSLECT ('ONNN', ITG2, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YZERO (ITG2, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
C                                       Display area: location, size
C                                       Approx corr for zoom
      NPIX = 13 * CSIZTV(1)
      NROW = 4 * CSIZTV(2)
      ISCX = 0
      ISCY = 0
      MAG = 1 + TVZOOM(1)
      IF (MXZOOM.GT.0) MAG = 2 ** TVZOOM(1)
      IX0 = WINDTV(1) - (MAG-1)/2
      IY0 = WINDTV(4) - MAG*NROW + 1 - (MAG-1)/2
      IF (MAG.GT.1) IY0 = IY0 + MAG
      IX0 = (IX0 - TVZOOM(2)) / MAG + TVZOOM(2) - TVSCGX
      IY0 = (IY0 - TVZOOM(3)) / MAG + TVZOOM(3) - TVSCGY
      IF (IX0+NPIX-1.GT.MAXXTV(1)) IX0 = 1
      IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = MAXXTV(2) - NROW + 1
      IX1 = IX0 + NPIX - 1
      IY1 = IY0 + NROW - 1
      CALL YHOLD ('ONNN', IRET)
      CALL YFILL (ITG1, IX0, IY0, IX1, IY1, 0, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 970
      IF (IG2.GT.0) THEN
         CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         END IF
      CALL YHOLD ('OFFF', IRET)
C                                       CURVALUE (from disk file)
C                                       no image yet
      CALL FILL (4, 0, CATBLK(IICOR))
      CATBLK(IICNO) = 0
      LDEP(1) = -10000
      WRITE (MSGTXT,1100)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1101)
      CALL MSGWRT (1)
      WRITE (MSGTXT,1102)
      CALL MSGWRT (1)
C                                       turn on cursor
      IP = 0
      ECOUNT = 0
      IG = IG1 + NGRAY
      PPOS(1) = 0.0
      PPOS(2) = 0.0
      RPOS(1) = (WINDTV(1) + WINDTV(3)) / 2
      RPOS(2) = (WINDTV(2) + WINDTV(4)) / 2
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IRET)
      IF (IRET.NE.0) GO TO 970
C                                       Cursor read loop point
 110  CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IRET)
      IF ((IBUT.GE.4) .OR. (IRET.NE.0)) GO TO 970
      LBUT = IBUT
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (.NOT.DOIT) GO TO 110
C                                       Find new image catalog block
         QUAD = 0
         CALL YCURSE ('FXIT', F, T, RPOS, QUAD, IBUT, IRET)
         IX = RPOS(1) + 0.51
         IY = RPOS(2) + 0.51
         INCNO = CATBLK(IICNO)
         INVOL = CATBLK(IIVOL)
         IF ((IX.LT.CATBLK(IICOR)) .OR. (IX.GT.CATBLK(IICOR+2)) .OR.
     *      (IY.LT.CATBLK(IICOR+1)) .OR. (IY.GT.CATBLK(IICOR+3))) THEN
            DO 115 IP = 1,NGRAY
               ITEMP = 2 ** (IP - 1)
               IF (ZAND(TVLIMG(QUAD),ITEMP).NE.0) THEN
                  CALL YCREAD (IP, IX, IY, CATBLK, IRET)
                  IF (IRET.EQ.0) GO TO 120
                  IF (IRET.NE.1) GO TO 960
                  END IF
 115           CONTINUE
C                                       No or invalid image here
 116        ECOUNT = ECOUNT + 1
            CALL FILL (4, 0, CATBLK(IICOR))
            CATBLK(IICNO) = 0
            IF (ECOUNT.LT.1) THEN
               WRITE (MSGTXT,1116) IX, IY
               CALL MSGWRT (1)
               END IF
            GO TO 110
C                                       Set up image reads
 120        CALL H2CHR (2, KHPTYO, CATH(KHPTY), LMTYPE)
            IF (LMTYPE.EQ.LMTYPS(2)) GO TO 116
            BLNKD = .FALSE.
C                                       Scaling parms for TV pixvals
            CALL COPY (5, CATBLK(IIDEP), LDEP)
            ICMASK = 2 ** (IP-1)
            ITR = 1
            CALL H2CHR (2, 1, CATH(IITRA), LMTYPE)
            DO 135 I = 1,8
               IF (LMTYPE.EQ.ITRTYP(I)) ITR = I
 135           CONTINUE
            ECOUNT = 0
            END IF
C                                       From TV for wedges
         CALL IMA2MP (RPOS, CORN)
         IX = CORN(1) + 0.51
         IY = CORN(2) + 0.51
         PIXVAL = IMAGE(IX,IY)
         BLNKD = IMAGE(IX,IY).EQ.FBLANK
C                                       Button A or B => add to lists
         IF (LBUT.GT.0) THEN
            NLIST = NLIST + 1
            PIXLIS(1,NLIST) = IX
            PIXLIS(2,NLIST) = IY
            WRITE (MSGTXT,1135) IX, IY
            CALL MSGWRT (2)
            END IF
C                                       Write text to TV
         IF ((IX.LE.9999) .AND. (IY.LE.9999)) THEN
            WRITE (STRING,1170) IX, IY
         ELSE
            WRITE (STRING,1171) IX, IY
            END IF
         IY = IY0 + 3*CSIZTV(2)
         CALL YHOLD ('ONNN', IRET)
         CALL YSLECT ('OFFF', ITG1, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:13), BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         IF (.NOT.BLNKD) THEN
            CALL METSCA (PIXVAL, PREFIX, EQUAL)
            WRITE (STRING,1172) PIXVAL
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            STRING = PREFIX
            CALL H2CHR (8, 1, CATH(KHBUN), STRING(6:))
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:13), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
         ELSE
            STRING = 'B  BLANKED'
            IY = IY - 1.5*CSIZTV(2)
            CALL IMCHAR (IG, IX0, IY, 0, 0, STRING(:10), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            STRING = ' '
            CALL IMCHAR (IG, IX0, IY0, 0, 0, STRING(:13), BUFFER, IRET)
            IF (IRET.NE.0) GO TO 970
            END IF
         IF (IG2.GT.0) CALL YFILL (ITG2, IX0, IY0, IX1, IY1, 1,
     *      BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YSLECT ('ONNN', ITG1, 0, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 970
         CALL YHOLD ('OFFF', IRET)
C                                       Button A or B => add to lists
         IF (IBUT.GT.0) THEN
            NLIST = NLIST + 1
            PIXLIS(1,NLIST) = IX
            PIXLIS(2,NLIST) = IY
            END IF
         GO TO 110
C-----------------------------------------------------------------------
C                                       Close downs
C                                       Img Catlg error
 960  WRITE (MSGTXT,1960) IRET
      CALL MSGWRT (6)
      GO TO 975
C                                       TV error possibly
 970  IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1970) IRET
         CALL MSGWRT (6)
         END IF
C                                       Close things
 975  CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IRET)
      ITEMP = 2 ** NGRAY
      IF ((ISCX.NE.0) .OR. (ISCY.NE.0)) CALL YSCROL (ITEMP, ISCX,
     *   ISCY, F, IRET)
      CALL YZERO (ITG1, I)
      IF (IG2.NE.0) CALL YZERO (ITG2, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Cursor selects which pixel is displayed')
 1101 FORMAT ('Hit button A or B to add the current pixel to list')
 1102 FORMAT ('Hit button C or D to exit')
 1116 FORMAT ('TVALUE: ',2I7,' NOT IN VALID IMAGE')
 1135 FORMAT ('Pixel',I5,',',I5,' added to list')
 1170 FORMAT ('X=',I4,' Y=',I4)
 1171 FORMAT (I6,I7)
 1172 FORMAT ('B=',F8.3)
 1960 FORMAT ('TVALUE: IMAGE CAT FILE IO ERROR',I7)
 1970 FORMAT ('TVALUE: TV ACTION IO ERROR',I7)
      END
      SUBROUTINE BLSWAP (LGSWAP, NX, NY, IMAGE, BUFF, IRET)
C-----------------------------------------------------------------------
C   BLSWAP prompts the user to mark a blotch region to swap the image
C   values and then does the swap.
C   Inputs:
C      LGSWAP   I(2)   Two Components to be swapped
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters
C   Output:
C      BUFF     I(*)   scratch buffer
C      IRET     I      Error: > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   LGSWAP(2), NX, NY, IRET
      REAL      IMAGE(NX,NY,*), BUFF(*)
C
      INTEGER   MPOLY
      PARAMETER (MPOLY = 100)
C
      INCLUDE 'RMFIT.INC'
      INTEGER   NVERT(MPOLY), XV(10*MPOLY), YV(10*MPOLY), LNX, IP1, IP2,
     *   IXL(MPOLY), IXU(MPOLY), IX, IY, IGR, NPOLY, I, LRMRNO, NCMP,
     *   YZPOS(2), LP1, LP2
      LOGICAL   DOIT
      REAL      TEMP, RESULT(MAXPRM,2), IAVG, PAVG, TMPRMS(2,2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IGR = MIN (3, NGRAPH)
C                                       interactive polygons
      NPOLY = 0
      CALL FILL (MPOLY, 0, NVERT)
      I = 10 * MPOLY
      CALL FILL (I, 0, XV)
      CALL FILL (I, 0, YV)
      CALL GRPOLY (MPOLY, IGR, NPOLY, NVERT, XV, YV, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SETTING POLYGONS ON THE TV'
         CALL MSGWRT (7)
         IRET = 0
         GO TO 999
      ELSE IF (NPOLY.LE.0) THEN
         MSGTXT = 'No polygons selected'
         CALL MSGWRT (2)
C                                       do the swap
      ELSE
         IP1 = (LGSWAP(1) - 1) * NMXIMG
         IP2 = (LGSWAP(2) - 1) * NMXIMG
         LP1 = (LGSWAP(1) - 1) * 4
         LP2 = (LGSWAP(2) - 1) * 4
C                                       loop over rows
         DO 50 IY = 1,NY
C                                       list X ranges in this row
            CALL BLTLIS (1, 1, NPOLY, NVERT, XV, YV, IY, LNX, IXL, IXU)
            IF (LNX.GT.0) THEN
C                                       loop over columns
               DO 40 IX = 1,NX
                  DOIT = .FALSE.
                  DO 20 I = 1,LNX
                     IF ((IX.GE.IXL(I)) .AND. (IX.LE.IXU(I))) DOIT =
     *                  .TRUE.
 20                  CONTINUE
                  IF (DOIT) THEN
C                                       swap image values
                     DO 30 I = 1,NMXIMG
                        TEMP = IMAGE(IX,IY,IP1+I)
                        IMAGE(IX,IY,IP1+I) = IMAGE(IX,IY,IP2+I)
                        IMAGE(IX,IY,IP2+I) = TEMP
 30                     CONTINUE
C                                       swap table values too
                     LRMRNO = (IY-1) * NX + IX
                     IRMRNO = LRMRNO
                     CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *                  YZPOS, NCMP, IAVG, PAVG, RESULT, TMPRMS, IRET)
                     IF (IRET.GT.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
                        GO TO 990
                     ELSE IF (IRET.EQ.0) THEN
                        DO 35 I = 1,4
                           TEMP = RESULT(LP1+I,1)
                           RESULT(LP1+I,1) = RESULT(LP2+I,1)
                           RESULT(LP2+I,1) = TEMP
                           TEMP = RESULT(LP1+I,2)
                           RESULT(LP1+I,2) = RESULT(LP2+I,2)
                           RESULT(LP2+I,2) = TEMP
 35                        CONTINUE
                        IRMRNO = LRMRNO
                        CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS,
     *                     RMNUMV, YZPOS, NCMP, IAVG, PAVG, RESULT,
     *                     TMPRMS, IRET)
                        IF (IRET.GT.0) THEN
                           WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
                           GO TO 990
                           END IF
                        END IF
                     END IF
 40               CONTINUE
               END IF
 50         CONTINUE
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
      IGR = IGR + NGRAY
      CALL YZERO (IGR, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLSWAP ERROR',I4,' ON ',A)
      END
      SUBROUTINE UPDLIS (OPER, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   UPDLIS flags or re-does fitting on a list of pixels
C   Inputs:
C      OPER    C*4    'REDO', 'FLAG'
C      NX      I      Number X pixels in image
C      NY      I      Number Y pixels in image
C   In/out:
C      IMAGE   R(*)   Images of NP parameters - updated on output
C   Output:
C      IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      CHARACTER OPER*4
      REAL      IMAGE(NX,NY,*)
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI(5), NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND(5), INDI(5), LIM1, IG, NCMP, I, IX, IY, LRMRNO,
     *   INLIST, YZPOS(2), LZOOM(3), K, NG, NXF, NYF, WINF(4), II
      REAL      RESULT(MAXPRM,2), IAVG, PAVG
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM)
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI /16,17,18,19,20/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       display parms
      INLIST = NLIST
      NG = NCOMPS
      DOEVEN = .TRUE.
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (OPER.EQ.'REDO') THEN
C                                       Open and init for read
         DO 10 I = 1,2
            CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
            CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
               GO TO 990
               END IF
 10         CONTINUE
C                                       Setup for I/O
         NXI = CATOLD(KINAX,1)
         NYI = CATOLD(KINAX+1,1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (BLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (TRC(2))
C                                       the FARS images
         DO 15 I = 4,5
            CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
            CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
               GO TO 990
               END IF
 15         CONTINUE
C                                       Setup for I/O
         NXF = CATOLD(KINAX,4)
         NYF = CATOLD(KINAX+1,4)
         WINF(1) = 1
         WINF(2) = IROUND (BLC(2))
         WINF(3) = NXF
         WINF(4) = IROUND (TRC(2))
C                                       Initial guess
         CALL DFILL (MAXPRM, 0.0D0, PARMS)
         CALL DFILL (MAXPRM, 0.0D0, UPARMS)
         CALL DFILL (MAXPRM, 0.0D0, XPARMS)
         IG = 4 * NCOMPS
         CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Setup for looping
C                                       Loop
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         IPOS(1) = UBLC(1) + 0.01
C                                       TV in good state
         CALL COPY (3, TVZOOM, LZOOM)
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV ZOOM'
            GO TO 990
            END IF
         IF (IPL(1).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(1), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         IF (IPL(2).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(2), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         END IF
C                                       do the list
      DO 300 II = 1,INLIST
         IX = PIXLIS(1,INLIST+1-II)
         IY = PIXLIS(2,INLIST+1-II)
         LRMRNO = (IY-1) * NX + IX
         IRMRNO = LRMRNO
         CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, YZPOS,
     *      NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       Init. files, first input.
            IF (OPER.EQ.'REDO') THEN
               IPOS(2) = YZPOS(1)
               IPOS(3) = YZPOS(2)
               CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINI(2) = YZPOS(1)
               WINI(4) = YZPOS(1)
               DO 30 I = 1,2
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI, WINI,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ INPUT IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                     GO TO 990
                     END IF
 30               CONTINUE
C                                       Copy to buffer.
               DO 35 I1 = 1,LIM1
                  QDATA(I1) = BUFF1(IBIND(1)+I1-1)
                  UDATA(I1) = BUFF2(IBIND(2)+I1-1)
 35               CONTINUE
C                                       FARS
               CALL COMOFF (CATOLD(KIDIM,4), CATOLD(KINAX,4), IPOS(3),
     *            BOTEMP, IRET)
               BOI = BOTEMP + 1
               WINF(2) = IPOS(2)
               WINF(4) = IPOS(2)
               DO 40 I = 4,5
                  CALL MINIT ('READ', LUNI(I), INDI(I), NXF, NYF, WINF,
     *               BUFFS(1,I), JBUFSZ, BOI, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'INIT READ FARS IMAGE'
                     GO TO 990
                     END IF
C                                       Read.
                  CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *               IBIND(I), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READING FARS IMAGE'
                     GO TO 990
                     END IF
 40               CONTINUE
C                                       Copy to buffer.
               DO 45 I1 = 1,NXF
                  REDATA(I1) = BUFFS(IBIND(4)+I1-1,4)
                  IMDATA(I1) = BUFFS(IBIND(5)+I1-1,5)
 45               CONTINUE
C                                       Call DO1FIT
               CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NCMP, RESULT,
     *            IRET)
               IF (IRET.EQ.99) THEN
                  MSGTXT = 'Quitting at user request'
                  CALL MSGWRT (5)
                  CALL ZCLOSE (LUNI(1), INDI(1), I1)
                  CALL ZCLOSE (LUNI(2), INDI(2), I1)
                  CALL ZCLOSE (LUNI(4), INDI(4), I1)
                  CALL ZCLOSE (LUNI(5), INDI(5), I1)
                  GO TO 999
               ELSE IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1180) IRET
                  GO TO 990
                  END IF
            ELSE IF (OPER.EQ.'FLAG') THEN
               CALL RFILL (2*MAXPRM, FBLANK, RESULT)
               NCMP = 0
               END IF
            IRMRNO = LRMRNO
            CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV, YZPOS,
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
               GO TO 990
               END IF
C                                       update the image value
            J = 0
            K = 0
            DO 120 I1 = 1,NG
               IMAGE(IX,IY,J+1) = RESULT(K+1,1)
               IMAGE(IX,IY,J+2) = RESULT(K+2,1)
               IMAGE(IX,IY,J+3) = RESULT(K+3,1)
               IMAGE(IX,IY,J+4) = RESULT(K+4,1)
               IMAGE(IX,IY,J+7) = RESULT(K+1,2)
               IMAGE(IX,IY,J+8) = RESULT(K+2,2)
               IMAGE(IX,IY,J+9) = RESULT(K+3,2)
               IMAGE(IX,IY,J+10) = RESULT(K+4,2)
C                                       Q0, U0 & error
               IF ((RESULT(K+1,1).EQ.FBLANK) .OR.
     *            (RESULT(K+3,1).EQ.FBLANK)) THEN
                  IMAGE(IX,IY,J+5) = FBLANK
                  IMAGE(IX,IY,J+6) = FBLANK
                  IMAGE(IX,IY,J+11) = FBLANK
                  IMAGE(IX,IY,J+12) = FBLANK
               ELSE
                  IMAGE(IX,IY,J+5) = RESULT(K+1,1) *
     *               COS (2.0D0 * DG2RAD * RESULT(K+2,1))
                  IMAGE(IX,IY,J+6) = RESULT(K+1,1) *
     *               SIN (2.0D0 * DG2RAD * RESULT(K+2,1))
                  IMAGE(IX,IY,J+11) = SQRT ((RESULT(K+1,2) *
     *               COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *               (RESULT(K+1,1) * 2.0D0 * DG2RAD * RESULT(K+2,2)
     *               * SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                  IMAGE(IX,IY,J+12) = SQRT ((RESULT(K+1,2) *
     *               SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *               (RESULT(K+1,1) * 2.0D0 * DG2RAD * RESULT(K+2,2)
     *               * COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                  END IF
               J = J + NMXIMG
               K = K + 4
 120           CONTINUE
            NLIST = NLIST - 1
            END IF
 300     CONTINUE
C                                       Close files
      IF (OPER.EQ.'REDO') THEN
         CALL ZCLOSE (LUNI(1), INDI(1), I)
         CALL ZCLOSE (LUNI(2), INDI(2), I)
         CALL ZCLOSE (LUNI(4), INDI(4), I)
         CALL ZCLOSE (LUNI(5), INDI(5), I)
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,4
            CALL YZERO (NGRAY+J, I)
 310        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDLIS: ERROR',I3,' ON ',A)
 1180 FORMAT ('UPDLIS: DO1FIT ERROR',I3)
      END
      SUBROUTINE UPDALL (OPER, SNMIN, POLMIN, MAXRES, MAXRM, MAXDWD,
     *   MAXSPX, NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   UPDALL flags or re-does fitting on all pixels matching test
C   conditions
C   Inputs:
C      OPER     C*4    'REDO', 'FLAG'
C      SNMIN    R(*)   Minimum amp S/N (< SNMIN => redo)
C      POLMIN   R(*)   Minimum amp (< POLMIN => redo)
C      MAXRES   R      Max allowed residual (> MAXRES => redo)
C      MAXRM    R(*)   Min/Max allowed rotation measure
C      MAXDWD   R(*)   Max error in Component width
C      MAXSPX   r(*)   Min/max allowed spectral index
C      NX       I      Number X pixels in image
C      NY       I      Number Y pixels in image
C   In/out:
C      IMAGE    R(*)   Images of NP parameters - updated on output
C   Output:
C      IRET     I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   NX, NY, IRET
      REAL      SNMIN(*), POLMIN(*), MAXRES, MAXRM(2,*), MAXDWD(*),
     *   MAXSPX(2,*), IMAGE(NX,NY,*)
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      CHARACTER PHNAME*48
      INTEGER   IROUND, LUNI(5), NYI, NXI, WINI(4), BOI, J, I1, IPOS(7),
     *   BOTEMP, IBIND(5), INDI(5), LIM1, IG, NCMP, I, IY, IZ, LRMRNO,
     *   INLIST, YZPOS(2), LZOOM(3), MP, IIZ, IIY, NG, K, NXF, NYF,
     *   WINF(4), JNPTS
      REAL      RESULT(MAXPRM,2), IAVG, PAVG
      DOUBLE PRECISION PARMS(MAXPRM), UPARMS(MAXPRM), XPARMS(MAXPRM),
     *   VALVAR(MAXPRM)
      LOGICAL   T, F, DOREAD, DOIT
      DOUBLE PRECISION  FJAC(MAXPRM,MAXPRM), FVEC(2*NPLIM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNI /16,17,18,19,20/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       display parms
      INLIST = NLIST
      DOREAD = (MAXRES.GT.0.0) .OR. (OPER.EQ.'REDO')
      NG = NCOMPS
      DOEVEN = .TRUE.
      CALL DFILL (MAXPRM, 0.0D0, XPARMS)
C                                       Open and init for read
      IF (DOREAD) THEN
C                                       Open and init for read
         DO 10 I = 1,2
            CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
            CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
               GO TO 990
               END IF
 10         CONTINUE
C                                       Setup for I/O
         NXI = CATOLD(KINAX,1)
         NYI = CATOLD(KINAX+1,1)
         WINI(1) = IROUND (UBLC(1))
         WINI(2) = IROUND (UBLC(2))
         WINI(3) = IROUND (UTRC(1))
         WINI(4) = IROUND (UTRC(2))
C                                       Setup for I/O
C                                       the FARS images
         DO 15 I = 4,5
            CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
            CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, T, F, T,
     *         IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FILE'
               GO TO 990
               END IF
 15         CONTINUE
         NXF = CATOLD(KINAX,4)
         NYF = CATOLD(KINAX+1,4)
         WINF(1) = 1
         WINF(2) = IROUND (UBLC(2))
         WINF(3) = NXF
         WINF(4) = IROUND (UTRC(2))
C                                       Initial guess
         CALL DFILL (MAXPRM, 0.0D0, PARMS)
         CALL DFILL (MAXPRM, 0.0D0, UPARMS)
         CALL DFILL (MAXPRM, 0.0D0, XPARMS)
         IG = 4 * NCOMPS
         CALL COPY (MAXPRM, DOCOMP, LLCOMP)
C                                       Setup for looping
C                                       Loop
         LIM1 = UTRC(1) - UBLC(1) + 1.01
         CALL FILL (7, 1, IPOS)
         IPOS(1) = UBLC(1) + 0.01
C                                       TV in good state
         CALL COPY (3, TVZOOM, LZOOM)
         TVZOOM(1) = 0
         TVZOOM(2) = MAXXTV(1) / 2
         TVZOOM(3) = MAXXTV(2) / 2
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV ZOOM'
            GO TO 990
            END IF
         IF (IPL(1).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(1), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         IF (IPL(2).GT.0) THEN
            CALL YSLECT ('OFFF', IPL(2), 0, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'TURNING OFF TV IMAGE DISPLAY'
               GO TO 990
               END IF
            END IF
         END IF
C                                       do the list
      DO 200 IZ = LBLC(2),LTRC(2)
         DO 190 IY = LBLC(1),LTRC(1)
            LRMRNO = (IZ-IBLC(2)) * (ITRC(1)-IBLC(1)+1) + IY - IBLC(1) +
     *         1
            IRMRNO = LRMRNO
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, YZPOS,
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
            ELSE IF ((IRET.EQ.0) .AND. (NCMP.GT.0)) THEN
C                                       Init. files, first input.
               IF (DOREAD) THEN
                  IPOS(2) = YZPOS(1)
                  IPOS(3) = YZPOS(2)
                  CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1),
     *               IPOS(3), BOTEMP, IRET)
                  BOI = BOTEMP + 1
                  WINI(2) = YZPOS(1)
                  WINI(4) = YZPOS(1)
                  DO 30 I = 1,2
                     CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI,
     *                  WINI, BUFFS(1,I), JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'INIT READ INPUT IMAGE'
                        GO TO 990
                        END IF
C                                       Read.
                     CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *                  IBIND(I), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'READING INPUT IMAGE'
                        GO TO 990
                        END IF
 30                  CONTINUE
C                                       Copy to buffer.
                  DO 35 I1 = 1,LIM1
                     QDATA(I1) = BUFF1(IBIND(1)+I1-1)
                     UDATA(I1) = BUFF2(IBIND(2)+I1-1)
 35                  CONTINUE
C                                       FARS
                  CALL COMOFF (CATOLD(KIDIM,4), CATOLD(KINAX,4),
     *               IPOS(3), BOTEMP, IRET)
                  BOI = BOTEMP + 1
                  WINF(2) = IPOS(2)
                  WINF(4) = IPOS(2)
                  DO 40 I = 4,5
                     CALL MINIT ('READ', LUNI(I), INDI(I), NXF, NYF,
     *                  WINF, BUFFS(1,I), JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'INIT READ FARS IMAGE'
                        GO TO 990
                        END IF
C                                       Read.
                     CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *                  IBIND(I), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'READING FARS IMAGE'
                        GO TO 990
                        END IF
 40                  CONTINUE
C                                       Copy to buffer.
                  DO 45 I1 = 1,NXF
                     REDATA(I1) = BUFFS(IBIND(4)+I1-1,4)
                     IMDATA(I1) = BUFFS(IBIND(5)+I1-1,5)
 45                  CONTINUE
                  END IF
C                                       do we do this one? check comps
               CALL COPY (MAXPRM, DOCOMP, LLCOMP)
               DOIT = .FALSE.
               J = -3
               DO 120 IG = 1,NCMP
                  J = J + 4
                  IF ((SNMIN(IG).GT.0) .AND. (RESULT(J,1).NE.FBLANK))
     *               THEN
                     IF (RESULT(J,1).LT.SNMIN(IG)*RESULT(J,2))
     *                  DOIT = .TRUE.
                     END IF
                  IF ((POLMIN(IG).GT.0) .AND. (RESULT(J,1).NE.FBLANK))
     *               THEN
                     IF (RESULT(J,1).LT.POLMIN(IG)) DOIT = .TRUE.
                     END IF
                  IF ((MAXRM(1,IG).LT.MAXRM(2,IG)) .AND.
     *               (RESULT(J+2,1).NE.FBLANK)) THEN
                     IF (RESULT(J+2,1).LT.MAXRM(1,IG)) DOIT=.TRUE.
                     IF (RESULT(J+2,1).GT.MAXRM(2,IG)) DOIT=.TRUE.
                     END IF
                  IF ((MAXDWD(IG).GT.0) .AND.
     *                  (RESULT(J+1,2).NE.FBLANK)) THEN
                     IF (RESULT(J+1,2).GT.MAXDWD(IG))
     *                  DOIT = .TRUE.
                     END IF
                  IF ((MAXSPX(1,IG).LT.MAXSPX(2,IG)) .AND.
     *               (RESULT(J+3,1).NE.FBLANK) .AND. (LLCOMP(J+3).GT.0))
     *               THEN
                     IF (RESULT(J+3,1).LT.MAXSPX(1,IG)) DOIT=.TRUE.
                     IF (RESULT(J+3,1).GT.MAXSPX(2,IG)) DOIT=.TRUE.
                     END IF
 120              CONTINUE
C                                       parameters
               CALL DFILL (MAXPRM, 0.0D0, PARMS)
               I1 = 1
               J = 1
               DO 130 I = 1,NCMP
                  IF (RESULT(J,1).NE.FBLANK) PARMS(I1) = RESULT(J,1)
                  IF (RESULT(J+1,1).NE.FBLANK) PARMS(I1+1)=RESULT(J+1,1)
                  IF (RESULT(J+2,1).NE.FBLANK) PARMS(I1+2)=RESULT(J+2,1)
                  IF ((RESULT(J+3,1).NE.FBLANK) .AND.
     *               (LLCOMP(J+3).GT.0)) PARMS(I1+3)=RESULT(J+3,1)
                  I1 = I1 + 4
                  J = J + 4
 130              CONTINUE
               MP = I1 - 1
C                                       check residuals
               IF ((.NOT.DOIT) .AND. (MAXRES.GT.0.0)) THEN
                  ITTER = 0
                  NITTER = MAX (XNIT, 100.0)
                  JNPTS = 2 * LIM1
                  NVAR = 0
                  K = 0
                  DO 135 I = 1,NG
                     DO 134 J = 1,4
                        K = K + 1
                        IF (LLCOMP(K).GT.0) THEN
                           NVAR = NVAR + 1
                           IVAR(NVAR) = I
                           JVAR(NVAR) = J
                           VALVAR(NVAR) = PARMS(K)
                           END IF
 134                    CONTINUE
 135                 CONTINUE
                  MP = NVAR
                  I = 1
                  CALL RMFUNC (JNPTS, MP, VALVAR, FVEC, FJAC, I)
                  DO 140 I1 = 1,LIM1
                     IF (BUFF1(IBIND(1)+I1-1).NE.FBLANK) THEN
                        IF (ABS(FVEC(I1)/WEIGHT(I1)).GT.MAXRES) DOIT =
     *                     .TRUE.
                        END IF
                     IF (BUFF2(IBIND(1)+I1-1).NE.FBLANK) THEN
                        IF (ABS(FVEC(I1+LIM1)/WEIGHT(I1+LIM1)).GT.
     *                     MAXRES) DOIT = .TRUE.
                        END IF
 140                 CONTINUE
                  END IF
C                                       Call DO1FIT
               IF (DOIT) THEN
                  IF (OPER.EQ.'REDO') THEN
                     CALL DO1FIT (IPOS, UPARMS, PARMS, XPARMS, NCMP,
     *                  RESULT, IRET)
                     IF (IRET.EQ.99) THEN
                        MSGTXT = 'Quitting at user request'
                        CALL MSGWRT (5)
                        CALL ZCLOSE (LUNI, INDI, I1)
                        GO TO 999
                     ELSE IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1180) IRET
                        GO TO 990
                        END IF
                     DO 145 I1 = 1,MAXPRM
                        IF (RESULT(I1,1).NE.FBLANK) XPARMS(I1) =
     *                     RESULT(I1,1)
 145                    CONTINUE
                  ELSE IF (OPER.EQ.'FLAG') THEN
                     CALL RFILL (2*MAXPRM, FBLANK, RESULT)
                     NCMP = 0
                     END IF
                  IRMRNO = LRMRNO
                  CALL TABRM ('WRIT', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *               YZPOS, NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITE RM TABLE'
                     GO TO 990
                     END IF
C                                       update the image value
                  IIY = IY - IBLC(1) + 1
                  IIZ = IZ - IBLC(2) + 1
                  J = 0
                  K = 0
                  DO 150 I1 = 1,NG
                     IMAGE(IIY,IIZ,J+1) = RESULT(K+1,1)
                     IMAGE(IIY,IIZ,J+2) = RESULT(K+2,1)
                     IMAGE(IIY,IIZ,J+3) = RESULT(K+3,1)
                     IMAGE(IIY,IIZ,J+4) = RESULT(K+4,1)
                     IMAGE(IIY,IIZ,J+7) = RESULT(K+1,2)
                     IMAGE(IIY,IIZ,J+8) = RESULT(K+2,2)
                     IMAGE(IIY,IIZ,J+9) = RESULT(K+3,2)
C                                       Q0, U0 & error
                     IF ((RESULT(K+1,1).EQ.FBLANK) .OR.
     *                  (RESULT(K+3,1).EQ.FBLANK)) THEN
                        IMAGE(IIY,IIZ,J+5) = FBLANK
                        IMAGE(IIY,IIZ,J+6) = FBLANK
                        IMAGE(IIY,IIZ,J+11) = FBLANK
                        IMAGE(IIY,IIZ,J+12) = FBLANK
                     ELSE
                        IMAGE(IIY,IIZ,J+5) = RESULT(K+1,1) *
     *                     COS (2.0D0 * DG2RAD * RESULT(K+2,1))
                        IMAGE(IIY,IIZ,J+6) = RESULT(K+1,1) *
     *                     SIN (2.0D0 * DG2RAD * RESULT(K+2,1))
                        IMAGE(IIY,IIZ,J+11) = SQRT ((RESULT(K+1,2) *
     *                     COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *                     (RESULT(K+1,1)*2.0D0*DG2RAD * RESULT(K+2,2)
     *                     * SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                        IMAGE(IIY,IIZ,J+12) = SQRT ((RESULT(K+1,2) *
     *                     SIN (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2 +
     *                     (RESULT(K+1,1)*2.0D0*DG2RAD * RESULT(K+2,2)
     *                     * COS (2.0D0 * DG2RAD * RESULT(K+2,1))) ** 2)
                        END IF
                     J = J + NMXIMG
                     K = K + 4
 150                 CONTINUE
                  END IF
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       Close files
      IF (DOREAD) THEN
         CALL ZCLOSE (LUNI(1), INDI(1), I)
         CALL ZCLOSE (LUNI(2), INDI(2), I)
         CALL ZCLOSE (LUNI(4), INDI(4), I)
         CALL ZCLOSE (LUNI(5), INDI(5), I)
         END IF
      IF (OPER.EQ.'REDO') THEN
         CALL COPY (3, LZOOM, TVZOOM)
         CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), .TRUE., I)
         IF (IPL(1).GT.0) CALL YSLECT ('ONNN', IPL(1), 0, SCRTCH, I)
         IF (IPL(2).GT.0) CALL YSLECT ('ONNN', IPL(2), 0, SCRTCH, I)
         DO 310 J = 1,4
            CALL YZERO (NGRAY+J, I)
 310        CONTINUE
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UPDALL: ERROR',I3,' ON ',A)
 1180 FORMAT ('UPDALL: DO1FIT ERROR',I3)
      END
      SUBROUTINE RMFIOU (IRET)
C-----------------------------------------------------------------------
C   RMFIOU creates the output residual image and the parameter images
C   and then computes the residual map (if any) and fills (via PSCALE)
C   the individual Component parameter images.  It calls RMFIHI for
C   history info for all images.
C   Output:
C      IRET    I   0 => ok,  4 => real trouble.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER SEQTYP*6
      INTEGER   NG, NP, NXO, NYO, WINO(4), IP, NCN, IG, IOFF
      LOGICAL   DORES, DOPARM
      REAL      RMIN(2), RMAX(2), RBLNK
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      DOPARM = DOCAT.GE.2
      DORES  = MOD(DOCAT-1,2).EQ.0
C                                       create output images
      CALL RMFICR (DORES, DOPARM, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING OUTPUT FILES'
         GO TO 990
         END IF
      NCN = 5
      IF (DORES) THEN
         NCN = NCN + 1
         IG = 0
         NEWCNO = FCNO(NCN)
         DISKO = FVOL(NCN)
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
            WRITE (MSGTXT,1005) IRET, NCN
            GO TO 990
            END IF
         CALL RMFIRE (RMIN, RMAX, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING THE RESIDUAL IMAGE'
            GO TO 990
            END IF
         RBLNK = CATR(KRBLK)
         CATR(KRDMX) = RMAX(1)
         CATR(KRDMN) = RMIN(1)
         CALL RMFIHI (IG, NCN)
C                                       get correct header for U
         NCN = NCN + 1
         NEWCNO = FCNO(NCN)
         DISKO = FVOL(NCN)
         CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH,
     *      IRET)
         IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
            WRITE (MSGTXT,1005) IRET, NCN
            GO TO 990
            END IF
         CATR(KRDMX) = RMAX(2)
         CATR(KRDMN) = RMIN(2)
         CATR(KRBLK) = RBLNK
         CALL RMFIHI (IG, NCN)
         END IF
C                                       loop limits etc.
      IF (DOPARM) THEN
         NG = NCOMPS
         NP = 4 * NG
         WINO(1) = 1
         WINO(2) = 1
C                                       Output Component parms
         DO 30 IG = 1,NG
            DO 20 IP = 1,8
               IOFF = MOD (IP-1, 4) + 1 + 4 * (IG-1)
               IF (LLCOMP(IOFF).GT.0) THEN
                  NCN = NCN + 1
                  NEWCNO = FCNO(NCN)
                  DISKO = FVOL(NCN)
                  CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST',
     *               SCRTCH, IRET)
                  IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
                     WRITE (MSGTXT,1005) IRET, NCN
                     GO TO 990
                     END IF
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
                  WRITE (MSGTXT,1010) SEQTYP
                  CALL MSGWRT (1)
                  SEQOUT = CATBLK(KIIMS)
                  CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
                  CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
                  NXO = CATBLK(KINAX)
                  NYO = CATBLK(KINAX+1)
                  WINO(3) = NXO
                  WINO(4) = NYO
C                                       Fill image
                  IF (IP.GE.5) IOFF = IOFF + MAXPRM
                  CALL PSCALE (IOFF, WINO, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1011) IRET, SEQTYP
                     GO TO 990
                     END IF
C                                       History, close
                  CALL RMFIHI (IOFF, NCN)
                  END IF
 20            CONTINUE
 30         CONTINUE
C                                       Q0, U0 maps
         DO 80 IG = 1,NG
            DO 70 IP = 1,4
               NCN = NCN + 1
               NEWCNO = FCNO(NCN)
               DISKO = FVOL(NCN)
               CALL CATIO ('READ', DISKO, NEWCNO, CATBLK, 'REST',
     *            SCRTCH, IRET)
               IF ((IRET.NE.0) .AND. (IRET.NE.6)) THEN
                  WRITE (MSGTXT,1005) IRET, NCN
                  GO TO 990
                  END IF
               CALL H2CHR (6, KHIMCO, CATH(KHIMC), SEQTYP)
               WRITE (MSGTXT,1010) SEQTYP
               CALL MSGWRT (1)
               SEQOUT = CATBLK(KIIMS)
               CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT(1:12))
               CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT(1:6))
               NXO = CATBLK(KINAX)
               NYO = CATBLK(KINAX+1)
               WINO(3) = NXO
               WINO(4) = NYO
C                                       Fill image
               IOFF = 2*MAXPRM + 4 * (IG - 1) + IP
               CALL PSCALE (IOFF, WINO, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1011) IRET, SEQTYP
                  GO TO 990
                  END IF
C                                       History, close
               CALL RMFIHI (IOFF, NCN)
 70            CONTINUE
 80         CONTINUE
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFIOU: ERROR',I4,' ON ',A)
 1005 FORMAT ('ERROR',I5,' RECOVERING FILE HEADER NUMBER',I5)
 1010 FORMAT ('Begin writing file of type ',A)
 1011 FORMAT ('ERROR',I5,' MOVING DATA TO FILE TYPE ',A4,A2)
      END
      SUBROUTINE RMFICR (DORES, DOPARM, IRET)
C-----------------------------------------------------------------------
C   RMFICR creates the output files.
C   Inputs:
C      DORES    L   Create residual?
C      DOPARM   L   Create parameter images?
C   Output:
C      IRET     I   Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C      /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      LOGICAL   DORES, DOPARM
      INTEGER   IRET
C
      CHARACTER BLANK*8, CUNITS(2)*8, BUNITS(9)*4, SEQTYP(8,4)*8,
     *   FLXTYP(4,4)*8, ASTS*4
      INTEGER   IERR, NG, NAX, I, ITYP, IG, IP, INPSEQ, J
      REAL      XBLC(7), XTRC(7)
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA CUNITS /'DEGREES', 'RAD/M/M'/
      DATA BUNITS /'/SEC', '/HZ ', '/M  ', '/M/S', '/M/S', '/PIX',
     *   '/PIX', '/DEG', '/UNK'/
      DATA SEQTYP /'PPOL1 ', 'THETA1', 'ROTME1', 'SPIX1 ',
     *             'DPPOL1', 'DTHET1', 'DROTM1', 'DSPIX1',
     *             'PPOL2 ', 'THETA2', 'ROTME2', 'SPIX2 ',
     *             'DPPOL2', 'DTHET2', 'DROTM2', 'DSPIX2',
     *             'PPOL3 ', 'THETA3', 'ROTME3', 'SPIX3 ',
     *             'DPPOL3', 'DTHET3', 'DROTM3', 'DSPIX3',
     *             'PPOL4 ', 'THETA4', 'ROTME4', 'SPIX4',
     *             'DPPOL4', 'DTHET4', 'DROTM4', 'DSPIX4'/
      DATA FLXTYP /'Q0_1  ', 'U0_1  ', 'DQ0_1 ', 'DU0_1',
     *             'Q0_2  ', 'U0_2  ', 'DQ0_2 ', 'DU0_2',
     *             'Q0_3  ', 'U0_3  ', 'DQ0_3 ', 'DU0_3',
     *             'Q0_4  ', 'U0_4  ', 'DQ0_4 ', 'DU0_4'/
      DATA ASTS /'****'/
C-----------------------------------------------------------------------
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLASS, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
      INPSEQ = SEQOUT
C                                       Get user modification to CATBLK
C                                       Get user modification to CATBLK
      CALL RCOPY (7, BLC, XBLC)
      CALL RCOPY (7, TRC, XTRC)
      XBLC(2) = IBLC(1)
      XBLC(3) = IBLC(2)
      XTRC(2) = ITRC(1)
      XTRC(3) = ITRC(2)
      CALL SUBHD3 (XBLC, XTRC, 1.0, 1.0, 1.0)
C                                       Create output files for residual
      NEWCNO = 0
      IRET = 4
C                                       Q residual
      IF (DORES) THEN
         CALL CHR2H (6, 'QRESID', KHIMCO, CATH(KHIMC))
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
C                                       U residual
         CALL CHR2H (6, 'URESID', KHIMCO, CATH(KHIMC))
         NEWCNO = 1
         CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO
         FCNO(NCFILE) = NEWCNO
         FRW(NCFILE) = 2
         END IF
C                                       Make names, classes, disks OK.
      IF (DOPARM) THEN
         SEQOUT = CATBLK(KIIMS)
C                                       save the residual header
         CALL COPY (256, CATBLK, BUFF2)
C                                       Basic output header: result
         CATBLK(KIDIM) = CATBLK(KIDIM) - 1
         NAX = CATBLK(KIDIM)
         DO 80 I = 1,NAX
            CATBLK(KINAX+I-1) = CATBLK(KINAX+I)
            CATR(KRCRP+I-1) = CATR(KRCRP+I)
            CATR(KRCRT+I-1) = CATR(KRCRT+I)
            CATR(KRCIC+I-1) = CATR(KRCIC+I)
            CATD(KDCRV+I-1) = CATD(KDCRV+I)
            CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1,
     *         CATH(KHCTP+(I-1)*2))
 80         CONTINUE
         DO 85 I = NAX,6
            CATBLK(KINAX+I) = 1
 85         CONTINUE
C                                       loop limits etc.
         NG = NCOMPS
C                                       Output Component parms
         J = -4
         DO 120 IG = 1,NG
            J = J + 4
            DO 110 IP = 1,8
               CALL CHR2H (6, SEQTYP(IP,IG), KHIMCO, CATH(KHIMC))
               IF ((IP.EQ.1) .OR. (IP.EQ.5)) THEN
                  IF (LLCOMP(J+1).LE.0) GO TO 110
                  CALL CHCOPY (8, 1, OLDH(KHBUN,1), 1, CATH(KHBUN))
               ELSE IF ((IP.EQ.2) .OR. (IP.EQ.6)) THEN
                  IF (LLCOMP(J+2).LE.0) GO TO 110
                  CALL CHR2H (8, CUNITS(1), 1, CATH(KHBUN))
               ELSE IF ((IP.EQ.3) .OR. (IP.EQ.7)) THEN
                  IF (LLCOMP(J+3).LE.0) GO TO 110
                  CALL CHR2H (8, CUNITS(2), 1, CATH(KHBUN))
               ELSE
                  IF (LLCOMP(J+4).LE.0) GO TO 110
                  CALL CHR2H (8, BLANK, 1, CATH(KHBUN))
                  END IF
C                                       Create
               DISKO = XDISKO + 0.01
               NEWCNO = 1
               CATBLK(KIIMS) = INPSEQ
               CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, SEQTYP(IP,IG)
                  GO TO 990
                  END IF
C                                       Record the creation
               NCFILE = NCFILE + 1
               FVOL(NCFILE) = DISKO
               FCNO(NCFILE) = NEWCNO
               FRW(NCFILE) = 2
 110           CONTINUE
 120        CONTINUE
C                                       Q0, U0 maps
         DO 170 IG = 1,NG
            DO 160 IP = 1,4
               CALL CHR2H (6, FLXTYP(IP,IG), KHIMCO, CATH(KHIMC))
               CALL CHCOPY (4, 1, OLDH(KHBUN,1), 1, CATH(KHBUN))
               CALL CHR2H (4, BUNITS(ITYP), 5, CATH(KHBUN))
               CALL CHR2H (1, ASTS(1:1), 5, CATH(KHBUN))
C                                       Create
               DISKO = XDISKO + 0.01
               NEWCNO = 1
               CATBLK(KIIMS) = INPSEQ
               CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1100) IERR, FLXTYP(IP,IG)
                  GO TO 990
                  END IF
C                                       Record the creation
               NCFILE = NCFILE + 1
               FVOL(NCFILE) = DISKO
               FCNO(NCFILE) = NEWCNO
               FRW(NCFILE) = 2
 160           CONTINUE
 170        CONTINUE
         END IF
      IRET = 0
      CALL COPY (256, BUFF2, CATBLK)
      DISKO = FVOL(2)
      NEWCNO = FCNO(2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT RESIDUALS FILE')
 1100 FORMAT ('ERROR',I5,' CREATING FILE TYPE ',A)
      END
      SUBROUTINE RMFIRE (RMIN, RMAX, IRET)
C-----------------------------------------------------------------------
C   RMFIRE reads the input data and the table to compute and write a
C   residual image.
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      RMIN(2), RMAX(2)
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'RMFITO.INC'
      INTEGER  LIM1, LIM2, LIM3, NXI, NYI, WINI(4), NXO, NYO, WINO(4),
     *   IPOS(7), BOI, IBIND(2), OBIND(2), LUNI(2), INDI(2), LUNO(2),
     *   INDO(2), NCMP, IROUND, I1, I2, I3, I, J, XYPOS(2), JNPTS, K
      REAL      RESULT(MAXPRM,2), IAVG, PAVG
      CHARACTER PHNAME*48
      LOGICAL   BLNKD
      DOUBLE PRECISION PARMS(MAXPRM), FJAC(MAXPRM,MAXPRM),
     *   FVEC(2*NPLIM), VALVAR(MAXPRM)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUNI, LUNO /16,17, 18,19/
C-----------------------------------------------------------------------
      MSGTXT = 'Begin writing Q and U residual image cubes'
      CALL MSGWRT (2)
      RMIN(1) = 1.E15
      RMAX(1) = -1.E15
      RMIN(2) = 1.E15
      RMAX(2) = -1.E15
C                                       Open and init for read
      DO 10 I = 1,2
         CALL ZPHFIL ('MA', DISKIN(I), OLDCNO(I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNI(I), INDI(I), DISKIN(I), PHNAME, .TRUE.,
     *      .FALSE., .TRUE., IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
            GO TO 990
            END IF
         CALL ZPHFIL ('MA', FVOL(5+I), FCNO(5+I), 1, PHNAME, IRET)
         CALL ZOPEN (LUNO(I), INDO(I), FVOL(5+I), PHNAME, .TRUE.,
     *      .FALSE., .TRUE., IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING INPUT IMAGE'
            GO TO 990
            END IF
 10      CONTINUE
C                                       pointers
      NXI = CATOLD(KINAX,1)
      NYI = CATOLD(KINAX+1,1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IBLC(1)
      WINI(3) = IROUND (TRC(1))
      WINI(4) = ITRC(1)
      LIM3 = ITRC(2) - IBLC(2) + 1.01
      LIM2 = ITRC(1) - IBLC(1) + 1.01
      LIM1 = TRC(1) - BLC(1) + 1.01
      CALL FILL (7, 1, IPOS)
C                                       output
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
C                                       table read
      IRMRNO = 1
      BLNKD = .FALSE.
      DO 100 I3 = 1,LIM3
C                                       input
         IPOS(3) = IBLC(2) + I3 - 0.9
         CALL COMOFF (CATOLD(KIDIM,1), CATOLD(KINAX,1), IPOS(3), BOI,
     *      IRET)
         BOI = BOI + 1
         DO 15 I = 1,2
            CALL MINIT ('READ', LUNI(I), INDI(I), NXI, NYI, WINI,
     *         BUFFS(1,I), JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
               GO TO 990
               END IF
 15         CONTINUE
C                                       output
         IPOS(3) = I3
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOI, IRET)
         BOI = BOI + 1
         DO 20 I = 1,2
            CALL MINIT ('WRIT', LUNO(I), INDO(I), NXO, NYO, WINO,
     *         BUFFS(1,2+I), JBUFSZ, BOI, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT READ OF INPUT IMAGE'
               GO TO 990
               END IF
 20         CONTINUE
         DO 90 I2 = 1,LIM2
            DO 25 I = 1,2
               CALL MDISK ('READ', LUNI(I), INDI(I), BUFFS(1,I),
     *            IBIND(I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ INPUT IMAGE'
                  GO TO 990
                  END IF
               CALL MDISK ('WRIT', LUNO(I), INDO(I), BUFFS(1,2+I),
     *            OBIND(I), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE RESIDUAL IMAGE'
                  GO TO 990
                  END IF
 25            CONTINUE
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV,
     *         XYPOS, NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ RM TABLE'
               GO TO 990
               END IF
C                                       Copy to buffer.
            DO 30 I1 = 1,LIM1
               QDATA(I1) = BUFFS(IBIND(1)+I1-1,1)
               UDATA(I1) = BUFFS(IBIND(2)+I1-1,2)
 30            CONTINUE
C                                       parameters
            CALL DFILL (MAXPRM, 0.0D0, PARMS)
            I1 = 1
            J = 1
            DO 35 I = 1,NCMP
               IF (RESULT(J,1).NE.FBLANK)   PARMS(I1) = RESULT(J,1)
               IF (RESULT(J+1,1).NE.FBLANK) PARMS(I1+1) = RESULT(J+1,1)
               IF (RESULT(J+2,1).NE.FBLANK) PARMS(I1+2) = RESULT(J+2,1)
               IF ((RESULT(J+3,1).NE.FBLANK) .AND. (LLCOMP(J+3).GT.0))
     *            PARMS(I1+3) = RESULT(J+3,1)
               I1 = I1 + 4
               J = J + 4
 35            CONTINUE
            I1 = I1 - 1
            ITTER = 0
            NITTER = MAX (XNIT, 100.0)
            JNPTS = 2 * LIM1
            NVAR = 0
            K = 0
            DO 39 I = 1,NCMP
               DO 38 J = 1,4
                  K = K + 1
                  IF (LLCOMP(K).GT.0) THEN
                     NVAR = NVAR + 1
                     IVAR(NVAR) = I
                     JVAR(NVAR) = J
                     VALVAR(NVAR) = PARMS(K)
                     END IF
 38               CONTINUE
 39            CONTINUE
            I1 = NVAR
            I = 1
            CALL RMFUNC (JNPTS, I1, VALVAR, FVEC, FJAC, I)
            J = 0
            DO 50 I = 1,2
               DO 40 I1 = 1,LIM1
                  J = J + 1
                  IF (BUFFS(IBIND(I)+I1-1,I).EQ.FBLANK) THEN
                     BUFFS(OBIND(I)+I1-1,2+I) = FBLANK
                     BLNKD = .TRUE.
                  ELSE
                     FVEC(J) = FVEC(J) / WEIGHT(J)
                     BUFFS(OBIND(I)+I1-1,2+I) = FVEC(J)
                     IF (FVEC(J).GT.RMAX(I)) RMAX(I) = FVEC(J)
                     IF (FVEC(J).LT.RMIN(I)) RMIN(I) = FVEC(J)
                     END IF
 40               CONTINUE
 50            CONTINUE
 90         CONTINUE
         DO 95 I = 1,2
            CALL MDISK ('FINI', LUNO(I), INDO(I), BUFFS(1,2+I),
     *         OBIND(I), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'FINISH RESIDUAL IMAGE'
               GO TO 990
               END IF
 95         CONTINUE
 100     CONTINUE
      IF (BLNKD) THEN
         CATR(KRBLK) = FBLANK
      ELSE
         CATR(KRBLK) = 0.0
         END IF
C
 990  IF (IRET.GT.0) CALL MSGWRT (8)
      IF (INDI(1).GT.0) CALL ZCLOSE (LUNI(1), INDI(1), I)
      IF (INDI(2).GT.0) CALL ZCLOSE (LUNI(2), INDI(2), I)
      IF (INDO(1).GT.0) CALL ZCLOSE (LUNO(1), INDO(1), I)
      IF (INDO(2).GT.0) CALL ZCLOSE (LUNO(2), INDO(2), I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFIRE: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PSCALE (IOFF, WINO, IRET)
C-----------------------------------------------------------------------
C   PSCALE reads the table and writes an image out.
C   Inputs:
C      IOFF     I       What to write: 1-4 comp 1 P1, theta, RM, spix
C                          5-8 comp 1 un certainty in P1, theta, RM, spx
C                          9-16 comp 2, 17-24 comp 3, 25-32 comp4
C                          33-36 comp 1 Q0, U0, err in Q0, U0
C                          37-40 comp 2 Q0, U0, err in Q0, U0
C                          41-44 comp 3 Q0, U0, err in Q0, U0
C                          45-48 comp 4 Q0, U0, err in Q0, U0
C      WINO     I(4)    Output Window
C      JBUFSZ   I       Buffer size in bytes
C   Output:
C      IRET     I       0 -> ok, else IO error
C      CATBLK in common: change max/min and scaling and blanking
C      Buffers in common
C-----------------------------------------------------------------------
      INTEGER   IOFF, WINO(4), IRET
C
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'RMFITD.INC'
      CHARACTER PHNAME*48
      LOGICAL   BLNKD, T, CHECK
      REAL      PMIN, PMAX, RESULT(MAXPRM,2), IAVG, PAVG
      INTEGER   NXO, NYO, I2, LUNO, INDO, IPOS(7), BOTEMP, OBIND, L,
     *   JERR, JOFF, KOFF, NCMP, I1, J, K
      DOUBLE PRECISION POFF, PMULT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T /.TRUE./
      DATA LUNO /17/
C-----------------------------------------------------------------------
C                                       loop limits
      NXO = WINO(3)
      NYO = WINO(4)
      CHECK = (IOFF.LT.2*MAXPRM) .AND. (MOD(IOFF-2,6).EQ.0)
C                                       Open files
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, T, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       loop
      CALL FILL (7, 1, IPOS)
      IRMRNO = 1
      BLNKD = .FALSE.
      PMIN = 1.E15
      PMAX = -PMIN
      POFF = 0.0D0
      PMULT = 1.0D0
      IF (OLDR(KRCIC,1).EQ.0.0) OLDR(KRCIC,1) = 1.0
      IF (IOFF.GT.2*MAXPRM) THEN
         L = IOFF - 2 * MAXPRM
         JOFF = MOD (L-1, 4) + 1
         KOFF = ((L - 1) / 4) * 4 + 1
      ELSE
         K = 1
         IF (IOFF.GT.MAXPRM) K = 2
         J = MOD (IOFF-1, MAXPRM) + 1
         END IF
C                                       Init output
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOTEMP = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *   BOTEMP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT I/O TO PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       Init a write
      DO 100 I2 = 1,NYO
         CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING PARAMETER IMAGE'
            GO TO 990
            END IF
C                                       Loop thru input plane
         L = OBIND - 1
         DO 90 I1 = 1,NXO
            CALL TABRM ('READ', RMBUFF, IRMRNO, RMKOLS, RMNUMV, IPOS(2),
     *         NCMP, IAVG, PAVG, RESULT, THERMS, IRET)
            L = L + 1
            IF (IOFF.GT.2*MAXPRM) THEN
               IF (RESULT(KOFF,1).EQ.FBLANK) THEN
                  BUFF2(L) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  IF (JOFF.EQ.1) THEN
                     BUFF2(L) = RESULT(KOFF,1) *
     *                  COS (2.0D0 * DG2RAD * RESULT(KOFF+1,1))
                  ELSE IF (JOFF.EQ.2) THEN
                     BUFF2(L) = RESULT(KOFF,1) *
     *                  SIN (2.0D0 * DG2RAD * RESULT(KOFF+1,1))
                  ELSE IF (JOFF.EQ.3) THEN
                     BUFF2(L) = SQRT ((RESULT(KOFF,2) *
     *                  COS (2.0D0*DG2RAD * RESULT(KOFF+1,1))) ** 2 +
     *                  (2.0D0*DG2RAD*RESULT(KOFF,1)*RESULT(KOFF+1,2) *
     *                  SIN (2.0D0*DG2RAD * RESULT(KOFF+1,1))) ** 2)
                  ELSE
                     BUFF2(L) = SQRT ((RESULT(KOFF,2) *
     *                  SIN (2.0D0*DG2RAD * RESULT(KOFF+1,1))) ** 2 +
     *                  (2.0D0*DG2RAD*RESULT(KOFF,1)*RESULT(KOFF+1,2) *
     *                  COS (2.0D0*DG2RAD * RESULT(KOFF+1,1))) ** 2)
                     END IF
                  PMIN = MIN (PMIN, BUFF2(L))
                  PMAX = MAX (PMAX, BUFF2(L))
                  END IF
            ELSE
               IF (RESULT(J,K).EQ.FBLANK) THEN
                  BUFF2(L) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  BUFF2(L) = RESULT(J,K)
                  IF (CHECK) THEN
 85                  IF (BUFF2(L).GT.90.0) THEN
                        BUFF2(L) = BUFF2(L) - 180.0
                        GO TO 85
                     ELSE IF (BUFF2(L).LT.-90.0) THEN
                        BUFF2(L) = BUFF2(L) + 180.0
                        GO TO 85
                        END IF
                     END IF
                  PMIN = MIN (PMIN, BUFF2(L))
                  PMAX = MAX (PMAX, BUFF2(L))
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C                                       Flush output plane
      CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISHING PARAMETER IMAGE'
         GO TO 990
         END IF
C                                       Set maxima, clear blanking
      CATR(KRDMX) = PMAX
      CATR(KRDMN) = PMIN
      CATR(KRBLK) = 0.0
C                                       Close down (error)
 990  IF (IRET.GT.0) CALL MSGWRT (8)
C                                       Close files
      IF (INDO.GT.0) CALL ZCLOSE (LUNO, INDO, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PSCALE: ERROR',I4,' ON ',A)
      END
      SUBROUTINE RMFIHI (ITYP, NCN)
C-----------------------------------------------------------------------
C   RMFIHI copies and updates history file.
C   Inputs:
C      ITYP   I   Output map type: 0 => residual
C                    1 => answers (get 1st axis info also)
C      NCN    I   Position in FILES common on catlgd file
C-----------------------------------------------------------------------
      INTEGER   ITYP, NCN
C
      CHARACTER HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, IG, JTRIM, KBLC(7), KTRC(7), I, NF
      LOGICAL   T
      INCLUDE 'RMFIT.INC'
      INCLUDE 'RMFITO.INC'
      INCLUDE 'RMFITD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       partial header keyword copy
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
C                                        Copy only the relevant table
      CALL TABCOP ('RM', RMVERS, RMVERS, LUN1, LUN2, DISKIN, DISKO,
     *   OLDCNO, NEWCNO, CATBLK, SCRTCH, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING RM TABLE'
         CALL MSGWRT (6)
         END IF
C                                       FQ table to residual images
      CALL FNDEXT ('FQ', CATOLD, I)
      IF ((I.GT.0) .AND. (ITYP.EQ.0)) THEN
         CALL TABCOP ('FQ', I, I, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *      NEWCNO, CATBLK, SCRTCH, BUFF2, IERR)
         IF (IERR.GT.2) THEN
            MSGTXT = 'ERROR COPYING FQ TABLE'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 50
         END IF
      DO 10 I = 1,7
         KBLC(I) = BLC(I) + 0.1
         KTRC(I) = TRC(I) + 0.1
 10      CONTINUE
      KBLC(2) = IBLC(1)
      KBLC(3) = IBLC(2)
      KTRC(2) = ITRC(1)
      KTRC(3) = ITRC(2)
      NF = KTRC(1) - KBLC(1) + 1
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN(1), CLASS(1), SEQIN(1), DISKIN(1),
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO2 (TSKNAM, NAMEIN(2), CLASS(2), SEQIN(2), DISKIN(2),
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO3 (TSKNAM, NAMEIN(3), CLASS(3), SEQIN(3), DISKIN(3),
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO4 (TSKNAM, NAMEIN(4), CLASS(5), SEQIN(4), DISKIN(4),
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL HENCO5 (TSKNAM, NAMEIN(5), CLASS(5), SEQIN(5), DISKIN(5),
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, KBLC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, KTRC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       other Parms
      WRITE (HILINE,2002) TSKNAM, IYINC, IZINC
      IF ((IYINC.GE.2) .OR. (IZINC.GE.2)) THEN
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 50
      CALL H2CHR (8, 1, OLDH(KHBUN,1), LABEL)
      WRITE (HILINE,2003) TSKNAM, ICUT, LABEL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2004) TSKNAM, PCUT, LABEL
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Component guesses
      IG = NCOMPS
      WRITE (HILINE,2005) TSKNAM, IG
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
      WRITE (HILINE,2006) TSKNAM, RMVERS
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 50
C                                       Old axis 1
      IF (ITYP.GT.0) THEN
         CALL H2CHR (8, 1, OLDH(KHCTP,1), LABEL)
         WRITE (HILINE,2020) TSKNAM, LABEL
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2021) TSKNAM, CATOLD(KINAX,1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2022) TSKNAM, OLDR(KRCRP,1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2023) TSKNAM, OLDR(KRCIC,1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         WRITE (HILINE,2024) TSKNAM, OLDD(KDCRV,1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         END IF
C                                       weights
      IF (DOWGT.GT.0.0) THEN
         IF (INFILE.EQ.' ') THEN
            HILINE = TSKNAM // '/ Weights found by robust rms'
         ELSE
            WRITE (HILINE,2030) TSKNAM, INFILE(1:JTRIM(INFILE))
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 50
         DO 20 I = 1,NF
            WRITE (HILINE,2031) TSKNAM, I, LAMSQ(I), WEIGHT(I),
     *         WEIGHT(I+NF)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 50
 20         CONTINUE
         END IF
C                                       Close HI file
 50   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Update CATBLK and close
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', SCRTCH, IERR)
      FRW(NCN) = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RMFIHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(I6,','),I6)
 2001 FORMAT (A6,'TRC =',6(I6,','),I6)
 2002 FORMAT (A6,'YINC =',I5,'  ZINC =',I5)
 2003 FORMAT (A6,'ICUT =',1PE12.4,14X,'/ I flux cutoff in ',A)
 2004 FORMAT (A6,'PCUT =',1PE12.4,14X,'/ P flux cutoff in ',A)
 2005 FORMAT (A6,'NCOMP =',I2,22X,'/ Maximum # components fit')
 2006 FORMAT (A6,'INVERS =',I4,19X,'/ RM table version number')
 2020 FORMAT (A6,'CTYPE1  = ''',A8,'''',12X,'/ Old axis 1')
 2021 FORMAT (A6,'NAXIS1  = ',I6,16X,'/ Old axis 1')
 2022 FORMAT (A6,'CRPIX1  = ',F9.3,13X,'/ Old axis 1')
 2023 FORMAT (A6,'CDELT1  = ',1PE13.5,9X,'/ Old axis 1')
 2024 FORMAT (A6,'CRVAL1  = ',1PE18.10,4X,'/ Old axis 1')
 2030 FORMAT (A6,'INFILE = ''',A,'''  / weights text file')
 2031 FORMAT (A6,'/ LAMSQ(',I4,') =',F9.6,' m^2   QWT=',F10.6,
     *   '  UWT=',F10.6)
      END
      SUBROUTINE SUBHD3 (BLC, TRC, XINC, YINC, ZINC)
C-----------------------------------------------------------------------
C   SUBHD3 corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first 3 axes for use
C   of pixel increments - namely the number of pixels, the reference
C   pixel and the axis increment.
C   Inputs:
C      BLC    R(7)   Bottom left corner to use
C      TRC    R(7)   Top right corner to use
C      XINC   R      Pixel increment on first axis
C      YINC   R      Pixel increment on second axis
C      ZINC   R      Pxel increment on third axis
C   Common /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC, ZINC
C
      CHARACTER FCHARS(3)*4, CHTM12*12
      REAL      AINC(7)
      INTEGER   IPL, IPH, NAX, I, J
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, AINC)
      IF (XINC.GT.0.0) AINC(1) = XINC
      IF (YINC.GT.0.0) AINC(2) = YINC
      IF (ZINC.GT.0.0) AINC(3) = ZINC
      DO 10 I = 1,NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL) / AINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) / AINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * AINC(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).NE.0) THEN
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1)*2
            DO 20 J = 1,3
               CALL H2CHR (4, 1, CATH(IPL), CHTM12)
C                                       Found one
               IF (FCHARS(J)(1:4).EQ.CHTM12(1:4)) THEN
                  IPL = BLC(I) + 0.01
                  CATR(KRARP) = (CATR(KRARP) - IPL) / AINC(I) + 1.0
                  GO TO 999
                  END IF
 20            CONTINUE
 25         CONTINUE
         END IF
C
 999  RETURN
      END
