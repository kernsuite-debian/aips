LOCAL INCLUDE 'SERCH.INC'
C                                       Local include for SERCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      CHARACTER NAMEIN*12, CLAIN*6, OUTFIL*48, FUNTYP*2, NAMOUT*12,
     *   CLAOUT*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMEO(3), XCLAOU(2), XFUNTP,
     *   XOUTFL(12)
      REAL      XSIN, XDISIN, XBLC(7), XTRC(7), XYINC, XZINC, BCHAN,
     *   ECHAN, CHINC, XCHSEL(3,10), PIXAVG, PIXSTD, XNBOXS, XBOX(2,20),
     *   REPORT, DOWGHT, DOOUT, XSEQO, XDISKO, DOHIST, XLABEL, XDOTV,
     *   XGRCH, XBAD(10)
      INTEGER   SEQIN, DISKIN, SLUN, SIND, BLC(7), TRC(7), YINC, ZINC,
     *   NCHSEL, CHSEL(3,10), CATOLD(256), SCRTCH(512), SELECT(MAXCHA),
     *   TXLUN, TXIND, IINC(7), NPARMS, CATNEW(256), SEQO, DISKO,
     *   CATSCR(256), OUTAX, SLOTO, LUNO, INDO, NBOXS, BOX(2,20)
      REAL      IBUFF(MABFSS), OBUFF(MABFSS), SIGMA(MAXCHA), CATNR(256),
     *   S2NMAX, S2NMIN, CATSR(256)
      DOUBLE PRECISION CATSD(128)
      LOGICAL   DOTV
      EQUIVALENCE (CATNEW, CATNR), (CATSCR, CATSD, CATSR)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XBLC, XTRC, XYINC,
     *   XZINC, BCHAN, ECHAN, CHINC, XCHSEL, PIXAVG, PIXSTD, XNBOXS,
     *   XBOX, REPORT, DOWGHT, DOOUT, XNAMEO, XCLAOU, XSEQO, XDISKO,
     *   DOHIST, XFUNTP, XLABEL, XOUTFL, XDOTV, XGRCH, XBAD
      COMMON /SERCOM/ CATSCR, CATOLD, CATNEW, SIGMA, SEQIN, DISKIN,
     *   SLUN, SIND, BLC, TRC, YINC, ZINC, NCHSEL, CHSEL, SELECT, TXLUN,
     *   TXIND, IINC, DOTV, NPARMS, SEQO, DISKO, OUTAX, SLOTO, LUNO,
     *   INDO, S2NMAX, S2NMIN, NBOXS, BOX
      COMMON /SERBUF/ IBUFF, OBUFF, SCRTCH
      COMMON /SERCHR / NAMEIN, CLAIN, NAMOUT, CLAOUT, OUTFIL, FUNTYP
LOCAL END
      PROGRAM SERCH
C-----------------------------------------------------------------------
C! searches transposed cube for line signals
C# UV Plot-appl Analysis Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2002, 2007-2008, 2010, 2012, 2014-2015
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
C   SERCH goes through a spectral-line cube looking for Gaussians above
C   the noise level.
C   Inputs:
C     INNAME         NAMEIN        Name of input UV data.
C     INCLASS        CLAIN         Class of input UV data.
C     INSEQ          SEQIN         Seq. of input UV data.
C     INDISK         DISKIN        Disk number of input UV data.
C     BLC            BLC           Bottom left corner of input
C     TRC            TRC           Top right corner of input
C     YINC           YINC          Pixel increment on Y axis
C                                  (usually right ascension)
C     ZINC           ZINC          Pixel increment on Z axis
C                                  (usually declination)
C     BCHAN          BCHAN         Lowest width in channels
C     ECHAN          ECHAN         Highest width in channels
C     CHINC          CHINC         Increment in widths
C     CHANSEL        CHNSEL(3,10)  Sets of begin, end increment channel
C                                  numbers for line search regions
C     PIXSTD         PIXSTD        Estimate of true noise rms
C     NBOXES         NBOXS         Number of spectral regions to be used
C                                  in fitting baseline 0 => no fit
C     BOX            BOX(2,20)     Pairs of begin-end channels for
C                                  spectral baseline regions
C     ICUT                         Report all "signals" with sig/noise
C                                  over ICUT.   0 => 8
C     BADDISK                      Disk to avoid for scratch
C   Based on algorithm by Juan Uson.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, IPL
      REAL      WIDTH
      LOGICAL   LAST
      INCLUDE 'SERCH.INC'
      DATA PRGM /'SERCH '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create scratch file
C                                       Subimage, baseline, rms's
      CALL SERCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C
      IPL = 0
      WIDTH = BCHAN - CHINC
 10   WIDTH = WIDTH + CHINC
      IF (WIDTH.LE.ECHAN) THEN
         LAST = WIDTH+CHINC.GT.ECHAN
         IPL = IPL + 1
         CALL SERCIT (WIDTH, IPL, LAST, IRET)
         IF (IRET.NE.0) GO TO 995
         GO TO 10
         END IF
C                                       close output with history
      IF (DOOUT.GT.0.0) CALL SERCHI (IERR)
C                                       Close down
 995  IF (TXLUN.NE.0) THEN
         TXLUN = ABS (TXLUN)
         CALL ZTXCLS (TXLUN, TXIND, IERR)
         END IF
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE SERCIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   SERCIN gets input parameters for SERCH, creates scratch
C   Removes baseline and solves for rms's
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INCLUDE 'SERCH.INC'
      CHARACTER TYPE*2, PHNAME*48, STAT*4
      INTEGER   CNO, IERR, IUSER, I, L, J, IROUND, SIZE, IWIN(4),
     *   OWIN(4), NPT(MAXCHA), IDEPTH(5), IBLKOF, NBY, IPOS, OPOS, I1,
     *   I2, I3, I4, I5, I6, I7, NCHAN, LUNI, INDI, JTRIM
      REAL      SUM(MAXCHA), SUMS(MAXCHA), AVE(MAXCHA), CUT(MAXCHA)
C-----------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NBY = MABFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARMS = 136
      CALL GTPARM (PRGM, NPARMS, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 10
         IRET = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       open input image
      IUSER = 0
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMEO, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (2, 1, XFUNTP, FUNTYP)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
      TYPE = 'MA'
      LUNI = 21
      DOTV = XDOTV.GT.0.0
      STAT = 'READ'
      IF ((DOHIST.GT.0.0) .AND. (.NOT.DOTV)) STAT = 'HDWR'
      CALL MAPOPN (STAT, DISKIN, NAMEIN, CLAIN, SEQIN, TYPE, IUSER,
     *   LUNI, INDI, CNO, CATBLK, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
      XSIN = SEQIN
      XDISIN = DISKIN
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNO
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATOLD)
C                                       other inputs
      DO 15 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 15      CONTINUE
C                                       window
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), XBLC, XTRC, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (REPORT.LE.0.01) REPORT = 8
      CALL FILL (7, 1, IINC)
      YINC = 1
      ZINC = 1
      IF (XYINC.GE.0.5) YINC = IROUND (XYINC)
      IF (XZINC.GE.0.5) ZINC = IROUND (XZINC)
      IINC(2) = YINC
      IINC(3) = ZINC
      DO 20 I = 1,7
         BLC(I) = IROUND (XBLC(I))
         TRC(I) = IROUND (XTRC(I))
         CATBLK(KINAX+I-1) = (TRC(I) - BLC(I)) / IINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - BLC(I)) / IINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * IINC(I)
 20      CONTINUE
C                                      Figure out how big, make, open
      CALL MAPSIZ (CATBLK(KIDIM), CATBLK(KINAX), SIZE)
      CALL SCREAT (SIZE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL CATIO ('UPDT', SCRVOL(NSCR), SCRCNO(NSCR), CATBLK, 'REST',
     *   SCRTCH, IERR)
      CALL ZPHFIL ('SC', SCRVOL(NSCR), SCRCNO(NSCR), 1, PHNAME, IERR)
      SLUN = 16
      CALL ZOPEN (SLUN, SIND, SCRVOL(NSCR), PHNAME, .TRUE., .TRUE.,
     *   .TRUE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Windows, ...
      CALL COPY  (256, CATBLK, CATSCR)
      NCHAN = CATSCR(KINAX)
      IWIN(1) = BLC(1)
      IWIN(2) = BLC(2)
      IWIN(3) = TRC(1)
      IWIN(4) = TRC(2)
      OWIN(1) = 1
      OWIN(2) = 1
      OWIN(3) = CATSCR(KINAX)
      OWIN(4) = CATSCR(KINAX+1)
      NCHSEL = 0
      CALL FILL (MAXCHA, 0, SELECT)
      DO 30 I = 1,10
         CHSEL(1,I) = IROUND (XCHSEL(1,I))
         CHSEL(2,I) = IROUND (XCHSEL(2,I))
         CHSEL(3,I) = IROUND (XCHSEL(3,I))
         CHSEL(1,I) = MAX (1, CHSEL(1,I))
         CHSEL(2,I) = MIN (NCHAN, CHSEL(2,I))
         CHSEL(3,I) = MAX (1, CHSEL(3,I))
         IF (CHSEL(2,I).GE.CHSEL(1,I)) THEN
            IF (I.EQ.NCHSEL+1) NCHSEL = NCHSEL + 1
            DO 25 J = CHSEL(1,I),CHSEL(2,I),CHSEL(3,I)
               SELECT(J) = 1
 25            CONTINUE
            END IF
 30      CONTINUE
      IF (NCHSEL.LE.0) THEN
         NCHSEL = 1
         CHSEL(1,1) = 1
         CHSEL(2,1) = NCHAN
         CHSEL(3,1) = 1
         CALL FILL (NCHAN, 1, SELECT)
         END IF
      NBOXS = IROUND (XNBOXS)
      IF (NBOXS.GT.0) THEN
         DO 40 I = 1,NBOXS
            BOX(1,I) = MAX (1, IROUND (XBOX(1,I)))
            BOX(2,I) = MIN (NCHAN, IROUND (XBOX(2,I)))
 40         CONTINUE
         END IF
C                                       Zero summing arrays
      CALL FILL (NCHAN, 0, NPT)
      CALL RFILL (NCHAN, 0.0, SUM)
      CALL RFILL (NCHAN, 0.0, SUMS)
      CALL RFILL (NCHAN, PIXAVG, AVE)
      IF (PIXSTD.LE.0) PIXSTD = 1.E10
      PIXSTD = 4.0 * PIXSTD
      CALL RFILL (NCHAN, PIXSTD, CUT)
      MSGTXT = 'Finding channel rms''s and copying to scratch file'
      CALL MSGWRT (2)
C                                       read subimage to scratch file
      DO 90 I7 = BLC(7),TRC(7)
      DO 90 I6 = BLC(6),TRC(6)
      DO 90 I5 = BLC(5),TRC(5)
      DO 90 I4 = BLC(4),TRC(4)
      DO 90 I3 = BLC(3),TRC(3),ZINC
C                                       Set corner selection.
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
C                                       Block offset for source file.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
C                                       Initialize for double buffering
         CALL MINIT ('READ', LUNI, INDI, CATOLD(KINAX), CATOLD(KINAX+1),
     *      IWIN, IBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT READ', IRET
            GO TO 990
            END IF
C                                       Find block offset for subimage.
         IDEPTH(1) = (I3 - BLC(3)) / ZINC + 1
         IDEPTH(2) = I4 - BLC(4) + 1
         IDEPTH(3) = I5 - BLC(5) + 1
         IDEPTH(4) = I6 - BLC(6) + 1
         IDEPTH(5) = I7 - BLC(7) + 1
         CALL COMOFF (CATSCR(KIDIM), CATSCR(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
C                                       init write
         CALL MINIT ('WRIT', SLUN, SIND, CATSCR(KINAX), CATSCR(KINAX+1),
     *      OWIN, OBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT WRITE', IRET
            GO TO 990
            END IF
C                                       Read row
         DO 80 I2 = BLC(2),TRC(2)
            CALL MDISK ('READ', LUNI, INDI, IBUFF, IPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'READ', IRET
               GO TO 990
               END IF
C                                       use only desired ones
            IF (MOD(I2-BLC(2),YINC).EQ.0) THEN
C                                       set output index
               CALL MDISK ('WRIT', SLUN, SIND, OBUFF, OPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRITE', IRET
                  GO TO 990
                  END IF
C                                       move data to output
               CALL RCOPY (NCHAN, IBUFF(IPOS), OBUFF(OPOS))
C                                       fit baseline
               IF (NBOXS.GT.0) CALL SERCBL (NBOXS, BOX, NCHAN,
     *            OBUFF(OPOS))
C                                       sum into arrays
               DO 50 I1 = 1,NCHAN
                  J = I1 + OPOS - 1
                  IF (OBUFF(J).NE.FBLANK) THEN
                     IF (ABS(OBUFF(J)-AVE(I1)).LT.CUT(I1)) THEN
                        NPT(I1) = NPT(I1) + 1
                        SUM(I1) = SUM(I1) + OBUFF(J)
                        SUMS(I1) = SUMS(I1) + OBUFF(J) * OBUFF(J)
                        END IF
                     END IF
 50               CONTINUE
               END IF
 80         CONTINUE
C                                       finish write
         CALL MDISK ('FINI', SLUN, SIND, OBUFF, OPOS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'FINISH WRITE', IRET
            GO TO 990
            END IF
 90      CONTINUE
      CALL ZCLOSE (LUNI, INDI, IRET)
      L = 0
C                                       are we done?
 100  L = L + 1
      J = 0
      DO 110 I = 1,NCHAN
         IF (NPT(I).GT.1) THEN
            AVE(I) = SUM(I) / NPT(I)
            SIGMA(I) = (SUMS(I) - SUM(I)*SUM(I)/NPT(I)) / (NPT(I) - 1)
            IF (SIGMA(I).GT.0.0) THEN
               SIGMA(I) = SQRT (SIGMA(I))
               IF (ABS(CUT(I)/4.0/SIGMA(I)-1.0).GT.0.01) THEN
                  CUT(I) = 4.0 * SIGMA(I)
                  J = J + 1
                  END IF
            ELSE
               AVE(I) = 0.0
               CUT(I) = 1.E10
               SIGMA(I) = 0.0
               J = J + 1
               END IF
            END IF
 110     CONTINUE
C                                       NO - reread
      NBY = MABFSS * 2
      IF ((J.GT.NCHAN/16) .AND. (L.LE.10)) THEN
         CALL FILL (NCHAN, 0, NPT)
         CALL RFILL (NCHAN, 0.0, SUM)
         CALL RFILL (NCHAN, 0.0, SUMS)
         DO 150 I7 = 1,CATSCR(KINAX+6)
         DO 150 I6 = 1,CATSCR(KINAX+5)
         DO 150 I5 = 1,CATSCR(KINAX+4)
         DO 150 I4 = 1,CATSCR(KINAX+3)
         DO 150 I3 = 1,CATSCR(KINAX+2)
            IDEPTH(1) = I3
            IDEPTH(2) = I4
            IDEPTH(3) = I5
            IDEPTH(4) = I6
            IDEPTH(5) = I7
            CALL COMOFF (CATSCR(KIDIM), CATSCR(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
C                                       init read
            CALL MINIT ('READ', SLUN, SIND, CATSCR(KINAX),
     *         CATSCR(KINAX+1), OWIN, IBUFF, NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'INIT SC READ', IRET
               GO TO 990
               END IF
C                                       Read row
            DO 130 I2 = 1,CATSCR(KINAX+1)
               CALL MDISK ('READ', SLUN, SIND, IBUFF, OPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'READ SC', IRET
                  GO TO 990
                  END IF
C                                       sum into arrays
               DO 120 I1 = 1,NCHAN
                  J = I1 + OPOS - 1
                  IF (IBUFF(J).NE.FBLANK) THEN
                     IF (ABS(IBUFF(J)-AVE(I1)).LT.CUT(I1)) THEN
                        NPT(I1) = NPT(I1) + 1
                        SUM(I1) = SUM(I1) + IBUFF(J)
                        SUMS(I1) = SUMS(I1) + IBUFF(J) * IBUFF(J)
                        END IF
                     END IF
 120              CONTINUE
 130           CONTINUE
 150        CONTINUE
         GO TO 100
         END IF
C                                       finished more or less
      IF (J.LE.0) THEN
         WRITE (MSGTXT,1150) L
      ELSE
         WRITE (MSGTXT,1151) J, L
         END IF
      CALL MSGWRT (4)
C                                       Create an output file
      IF (DOOUT.GT.0.0) THEN
C                                       find new axis
         J = CATBLK(KIDIM)
         L = 3
         DO 160 I = 1,J
            IF (CATBLK(KINAX+I-1).GT.1) L = I
 160        CONTINUE
         OUTAX = L + 1
         IF (OUTAX.GT.KICTPN) THEN
            MSGTXT = 'ALL AXES > 1 POINT, CANNOT CREATE OUTPUT'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
         J = MIN (J, KICTPN-1)
C                                       move 1-pt axes up one
         CATBLK(KIDIM) = J + 1
         IF (L.LT.J) THEN
            I = J - L
            CALL COPY (I, CATSCR(KINAX+L), CATBLK(KINAX+L+1))
            CALL RCOPY (I, CATSR(KRCRP+L), CATR(KRCRP+L+1))
            CALL RCOPY (I, CATSR(KRCIC+L), CATR(KRCIC+L+1))
            CALL RCOPY (I, CATSR(KRCRT+L), CATR(KRCRT+L+1))
            CALL DPCOPY (I, CATSD(KDCRV+L), CATD(KDCRV+L+1))
            I = 2 * I
            CALL RCOPY (I, CATSR(KHCTP+2*L), CATR(KHCTP+2*L+2))
            END IF
C                                       insert line width
         CATBLK(KINAX+L) = (ECHAN - BCHAN) / CHINC + 1.001
         CATR(KRCRP+L) = 1.0
         CATR(KRCRT+L) = 0.0
         CATR(KRCIC+L) = CHINC
         CATD(KDCRV+L) = BCHAN
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
         CALL CHR2H (8, 'SIG2NOIS', 1, CATH(KHBUN))
         CALL CHR2H (8, 'LINWIDTH', 1, CATH(KHCTP+2*L))
         DISKO = IROUND (XDISKO)
         SEQO = IROUND (XSEQO)
         CALL MAKOUT (NAMEIN, CLAIN, SEQIN, ' ', NAMOUT, CLAOUT, SEQO)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = SEQO
         CALL MCREAT (DISKO, SLOTO, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1160) IRET
            GO TO 990
            END IF
         SEQO = CATBLK(KIIMS)
         TYPE = 'MA'
         IUSER = NLUSER
         LUNO = 17
         CALL MAPOPN ('INIT', DISKO, NAMOUT, CLAOUT, SEQO, TYPE, IUSER,
     *      LUNO, INDO, SLOTO, CATBLK, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 999
         S2NMAX = -1.E10
         S2NMIN = 1.E10
         NCFILE = NCFILE + 1
         FCNO(NCFILE) = SLOTO
         FVOL(NCFILE) = DISKO
         FRW(NCFILE) = 2
C                                       copy some keywords
         CALL KEYPCP (DISKIN, CNO, DISKO, SLOTO, 0, ' ', IERR)
         END IF
      CALL COPY (256, CATBLK, CATNEW)
C                                       Open output text file
      J = JTRIM (OUTFIL)
      IF (J.LE.0) THEN
         TXLUN = 0
      ELSE
         TXLUN = 10
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTFIL(1:J), .TRUE., IRET)
         IF (IRET.NE.0) THEN
            TXLUN = 0
            WRITE (MSGTXT,1165) IRET
            GO TO 990
            END IF
         MSGTXT = 'Channel      Sigma     offset'
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) THEN
            TXLUN = -TXLUN
            GO TO 999
            END IF
         DO 170 I = 1,NCHAN
            WRITE (MSGTXT,1170) I, SIGMA(I), AVE(I)
            J = JTRIM (MSGTXT)
            CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
            IF (IERR.NE.0) THEN
               TXLUN = -TXLUN
               GO TO 999
               END IF
 170        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. ERR=',I5)
 1100 FORMAT ('SERCIN: DOING ',A,' ERROR',I4)
 1150 FORMAT ('All channel rms''s converged after',I3,' read passes')
 1151 FORMAT ('All but',I4,' rms''s converged after',I3,' read passes')
 1160 FORMAT ('ERROR',I5,' CREATING OUTPUT HYPER CUBE')
 1165 FORMAT ('ERROR ON OUTPUT TEXT FILE OPEN: ERR=',I5)
 1170 FORMAT (I7,2(1PE11.3))
      END
      SUBROUTINE SERCBL (NBOXS, BOX, NCHAN, ROW)
C-----------------------------------------------------------------------
C   fits and subtracts a linear baseline
C   Inputs:
C      NBOXS   I        Number of baseline regions
C      BOX     I(2,*)   Channel numbers
C      NCHAN   I        Total number of channels
C   In/out:
C      ROW     R(*)     Spectrum
C-----------------------------------------------------------------------
      INTEGER   NBOXS, BOX(2,*), NCHAN
      REAL      ROW(*)
C
      INTEGER   I, J, ND
      REAL      SX, SY, SXX, SXY, SYY, DELTA, A, B
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SX = 0.0
      SY = 0.0
      SXX = 0.0
      SXY = 0.0
      SYY = 0.0
      ND = 0
      DO 20 J = 1,NBOXS
         DO 10 I = BOX(1,J),BOX(2,J)
            IF (ROW(I).NE.FBLANK) THEN
               SX = SX + I
               SY = SY + ROW(I)
               SXX = SXX + I * I
               SXY = SXY + I * ROW(I)
               SYY = SYY + ROW(I) * ROW(I)
               END IF
 10         CONTINUE
 20      CONTINUE
      DELTA = ND * SXX - SX * SX
      IF ((ND.GT.2) .AND. (DELTA.GT.0.0)) THEN
         A = (SXX * SY - SX * SXY) / DELTA
         B = (SXY * ND - SX * SY) / DELTA
         DO 30 I = 1,NCHAN
            IF (ROW(I).NE.FBLANK) ROW(I) = ROW(I) - A - B * I
 30         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SERCIT (WIDTH, IPL, LAST, IRET)
C-----------------------------------------------------------------------
C   does the weighted or unweighted filtering plus histogram plotting
C   for a single filter width
C   Input:
C      WIDTH   R      Filter width in channels
C      IPL     I      Output plane
C      LAST    L      This is the last width to do
C   Output:
C      IRET    I      0 => continue, else quit.  > 0 -> error, <0 user
C                     request to quit.
C-----------------------------------------------------------------------
      REAL     WIDTH
      LOGICAL  LAST
      INTEGER  IPL, IRET
C
      INCLUDE 'SERCH.INC'
      INTEGER NHIST
      PARAMETER (NHIST=400)
      CHARACTER CTIME*8, CDATE*12
      INTEGER   I, I2, I3, I4, I5, I6, I7, IDEPTH(5), OWIN(4), IBLKOF,
     *   NBY, IPOS, NCHAN, NUMBER, II, JJ, HISTSN(NHIST+3), JTRIM, J, K,
     *   IERR, NGV, TIME(3), DATE(3), OPOS
      REAL     NORMS(MAXCHA), NORM, DOF, TOTSWT, Y, TEMP, SBW1, SBW2,
     *   AVESIG, AVES2, AINVS2, NOISE, SIGNAL, HSCALE, OUT, SIG, GAUSS,
     *   STON, GVS(MAXCHA), MAXSIG, GVSLIM
      DATA GVSLIM /1.0E-6/
C-----------------------------------------------------------------------
      IF (TXLUN.GT.0) THEN
         CALL ZDATE (DATE)
         CALL ZTIME (TIME)
         CALL TIMDAT (TIME, DATE, CTIME, CDATE)
         WRITE (MSGTXT,1000) TSKNAM, RLSNAM, CDATE, CTIME
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
      WRITE (MSGTXT,1001) WIDTH
      CALL MSGWRT (3)
      IF (TXLUN.GT.0) THEN
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
      CALL RFILL (NHIST, 0.0, HISTSN)
      HSCALE = 20.0 / NHIST
      AVESIG = 0.0
      AVES2 = 0.0
      AINVS2 = 0.0
      NUMBER = 0
      MAXSIG = -1.
      TOTSWT = 0
      DOF = 0.0
C                                       Filter values
      NCHAN = CATSCR(KINAX)
      NGV = 0
      DO 10 I = 1,NCHAN
         CALL FILTER (I, WIDTH, GVS(I))
         IF (ABS(GVS(I)).GT.GVSLIM) NGV = I
 10      CONTINUE
C                                       normalization factors
      CALL RFILL (NCHAN, 0.0, NORMS)
      DO 20 K = 1,NCHAN
         IF (SELECT(K).GT.0) THEN
            NORM = 0.0
            DO 15 J = 1,NCHAN
               I = ABS (J-K) + 1
               IF ((SELECT(J).GT.0) .AND. (I.LE.NGV)) THEN
                  GAUSS = GVS(I) * GVS(I)
                  NORM = NORM + GAUSS
                  IF (DOWGHT.GT.0.0) THEN
                     NORMS(K) = NORMS(K) + GAUSS / SIGMA(J)**2
                  ELSE
                     NORMS(K) = NORMS(K) + GAUSS
                     END IF
                  END IF
 15            CONTINUE
            DOF = DOF + NORM
            TOTSWT = TOTSWT + 1
            IF (TXLUN.GT.0) THEN
               WRITE (MSGTXT,1015) K, NORM, NORMS(K)
               J = JTRIM (MSGTXT)
               CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
               IF (IERR.NE.0) THEN
                  TXLUN = -TXLUN
                  GO TO 999
                  END IF
               END IF
            END IF
 20      CONTINUE
      IF ((CATNR(KRBMJ).GT.0.0) .AND. (CATNR(KRCIC+1).NE.0.0)) THEN
         SBW1 = CATNR(KRBMJ) / ABS (CATNR(KRCIC+1))
      ELSE
         SBW1 = 3.0
         END IF
      IF ((CATNR(KRBMN).GT.0.0) .AND. (CATNR(KRCIC+2).NE.0.0)) THEN
         SBW2 = CATNR(KRBMN) / ABS (CATNR(KRCIC+2))
      ELSE
         SBW2 = 3.0
         END IF
      DOF = TOTSWT * TOTSWT / DOF
      DOF = DOF * CATSCR(KINAX+1) * CATSCR(KINAX+2) / 0.5666 / SBW1 /
     *   SBW2
      WRITE (MSGTXT,1020) DOF
      CALL MSGWRT (3)
      IF (TXLUN.GT.0) THEN
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
      MSGTXT = 'Channel Xpix  Ypix     Signal      Noise'
     *   // '   Width     S/N'
      CALL MSGWRT (3)
      IF (TXLUN.GT.0) THEN
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
C                                       loop through cube
      NBY = MABFSS * 2
      DO 100 I7 = 1,CATSCR(KINAX+6)
      DO 100 I6 = 1,CATSCR(KINAX+5)
      DO 100 I5 = 1,CATSCR(KINAX+4)
      DO 100 I4 = 1,CATSCR(KINAX+3)
      DO 100 I3 = 1,CATSCR(KINAX+2)
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATSCR(KIDIM), CATSCR(KINAX), IDEPTH, IBLKOF,
     *      IRET)
         IBLKOF = IBLKOF + 1
C                                       init write
         CALL MINIT ('READ', SLUN, SIND, CATSCR(KINAX), CATSCR(KINAX+1),
     *      OWIN, IBUFF, NBY, IBLKOF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'INIT SC READ', IRET
            GO TO 990
            END IF
C                                       output file
         IF (DOOUT.GT.0.0) THEN
            IDEPTH(OUTAX-2) = IPL
            CALL COMOFF (CATNEW(KIDIM), CATNEW(KINAX), IDEPTH, IBLKOF,
     *         IRET)
            IBLKOF = IBLKOF + 1
C                                       init write
            CALL MINIT ('WRIT', LUNO, INDO, CATNEW(KINAX),
     *         CATNEW(KINAX+1), OWIN, OBUFF, NBY, IBLKOF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'INIT OUT WRITE', IRET
               GO TO 990
               END IF
            END IF
C                                       Read row
         DO 50 I2 = 1,CATSCR(KINAX+1)
            CALL MDISK ('READ', SLUN, SIND, IBUFF, IPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'READ SC', IRET
               GO TO 990
               END IF
            IF (DOOUT.GT.0.0) THEN
               CALL MDISK ('WRIT', LUNO, INDO, OBUFF, OPOS, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1100) 'WRIT OUT', IRET
                  GO TO 990
                  END IF
               CALL RFILL (NCHAN, 0.0, OBUFF(OPOS))
               END IF
C                                       filter
            DO 40 K = 1,NCHAN
               IF (SELECT(K).GT.0) THEN
                  OUT = 0.0
                  SIG = 0.0
                  DO 30 J = 1,NCHAN
                     I = ABS (J-K) + 1
                     IF ((SELECT(J).GT.0) .AND. (I.LE.NGV) .AND.
     *                  (IBUFF(IPOS+J-1).NE.FBLANK)) THEN
                        GAUSS = GVS(I)
                        IF (DOWGHT.GT.0.0) THEN
                           SIG = SIG + GAUSS * GAUSS / SIGMA(J) /
     *                        SIGMA(J)
                           OUT = OUT + GAUSS * IBUFF(IPOS+J-1) /
     *                        SIGMA(J) / SIGMA(J)
                        ELSE
                           SIG = SIG + GAUSS * GAUSS * SIGMA(J) *
     *                        SIGMA(J)
                           OUT = OUT + GAUSS * IBUFF(IPOS+J-1)
                           END IF
                        END IF
 30                  CONTINUE
                  IF ((NORMS(K).GT.0.0) .AND. (SIG.GT.0.0)) THEN
                     NOISE = SQRT (SIG) / NORMS(K)
                     SIGNAL = OUT / NORMS(K)
                     STON = SIGNAL / NOISE
                     IF (ABS(STON).GT.REPORT) THEN
                        IF (DOOUT.GT.0.0) THEN
                           OBUFF(OPOS+K-1) = STON
                           S2NMAX = MAX (STON, S2NMAX)
                           S2NMIN = MIN (STON, S2NMIN)
                           END IF
                        II = BLC(2) + (I2 - 1) * IINC(2)
                        JJ = BLC(3) + (I3 - 1) * IINC(3)
                        J = BLC(1) + K - 1
                        WRITE (MSGTXT,1030) J, II, JJ, SIGNAL, NOISE,
     *                     WIDTH, STON
                        CALL MSGWRT (5)
                        IF (TXLUN.GT.0) THEN
                           J = JTRIM (MSGTXT)
                           CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J),
     *                        IERR)
                           IF (IERR.NE.0) TXLUN = -TXLUN
                           END IF
                        END IF
                     MAXSIG = MAX (MAXSIG, ABS(STON))
                     AVESIG = AVESIG + NOISE
                     AVES2 = AVES2 + NOISE * NOISE
                     AINVS2 = AINVS2 + 1.0 / NOISE / NOISE
                     NUMBER = NUMBER + 1
                     J = NHIST/2 + 1.5 + STON/HSCALE
                     J = MAX (1, MIN (NHIST+2, J))
                     HISTSN(J) = HISTSN(J) + 1
                     END IF
                  END IF
 40            CONTINUE
 50         CONTINUE
         IF (DOOUT.GT.0.0) THEN
            CALL MDISK ('FINI', LUNO, INDO, OBUFF, OPOS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1100) 'FINISH OUT', IRET
               GO TO 990
               END IF
            CATNR(KRDMX) = S2NMAX
            CATNR(KRDMN) = S2NMIN
            END IF
 100     CONTINUE
      IF (NUMBER.LE.0) THEN
         MSGTXT = 'NO POINTS FOUND'
         IRET = 9
         GO TO 990
         END IF
      AVESIG = AVESIG / NUMBER
      AVES2 = SQRT (AVES2 / NUMBER)
      AINVS2 = SQRT (NUMBER / AINVS2)
      WRITE (MSGTXT,1100) MAXSIG, NUMBER
      CALL MSGWRT (4)
      IF (TXLUN.GT.0) THEN
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
      WRITE (MSGTXT,1101) AVESIG, AVES2, AINVS2
      CALL MSGWRT (4)
      IF (TXLUN.GT.0) THEN
         J = JTRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) TXLUN = -TXLUN
         END IF
C                                       print histogram
      IF ((DOHIST.GE.0.0) .AND. (TXLUN.GT.0)) THEN
         DO 120 I = 1,NHIST+2
            IF (TXLUN.GT.0) THEN
               Y = HISTSN(I)
               TEMP = MAX (0.1, Y)
               Y = SQRT (Y)
               TEMP = ALOG10 (TEMP)
               WRITE (MSGTXT,1110) I, HISTSN(I), Y, TEMP
               J = JTRIM (MSGTXT)
               CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
               IF (IERR.NE.0) TXLUN = -TXLUN
               END IF
 120        CONTINUE
         END IF
C                                       plot histogram
      IF (DOHIST.GT.0.0) THEN
         CALL COPY (256, CATOLD, CATBLK)
         CALL PLTHST (NHIST, HISTSN, WIDTH, LAST, IRET)
         CALL COPY (256, CATBLK, CATOLD)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1001 FORMAT ('**********  Starting width ',F8.3,' channels **********')
 1015 FORMAT ('Channel, norm norm**2',I6,2(1PE12.4))
 1020 FORMAT ('Degrees of freedom =',F12.2)
 1030 FORMAT (3I6,2(1PE11.3),2(0PF8.3))
 1100 FORMAT ('Max S/N',1PE11.3,' over',I14,' channels*pixels')
 1101 FORMAT ('Average noise',1PE11.3,' square',1PE11.3,' inverse',
     *   1PE11.3)
 1110 FORMAT ('Hist(',I3.3,')=',I12,'   sqrt',F8.2,'   log10',F8.2)
      END
      SUBROUTINE SERCHI (IRET)
C-----------------------------------------------------------------------
C   finishes output file header and copies a history
C   Output:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   HLUNI, HLUNO, I
      CHARACTER HILINE*72, NOTTYP*2
      INCLUDE 'SERCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA HLUNI, HLUNO /25, 26/
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
C                                       copy old
      CALL HISCOP (HLUNI, HLUNO, DISKIN, DISKO, FCNO(1), SLOTO, CATNEW,
     *   SCRTCH(257), SCRTCH, IRET)
      IF (IRET.GT.3) GO TO 100
      IF (IRET.EQ.3) GO TO 45
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, HLUNO, SCRTCH,
     *   IRET)
      IF (IRET.NE.0) GO TO 45
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQO, DISKO, HLUNO, SCRTCH,
     *   IRET)
      IF (IRET.NE.0) GO TO 45
C                                       corners
      WRITE (HILINE,1000) TSKNAM, BLC
      CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 45
      WRITE (HILINE,1001) TSKNAM, TRC
      CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 45
C                                       Yinc, Zinc
      IF ((YINC.GT.1) .OR. (ZINC.GT.1)) THEN
         WRITE (HILINE,1005) TSKNAM, YINC, ZINC
         CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 45
         END IF
C                                       Chansel
      DO 10 I = 1,NCHSEL
         WRITE (HILINE,1010) TSKNAM, I, CHSEL(1,I), CHSEL(2,I),
     *      CHSEL(3,I)
         CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 45
 10      CONTINUE
C                                       Nboxes, box
      IF (NBOXS.GT.0) THEN
         DO 20 I = 1,NBOXS
            WRITE (HILINE,1011) TSKNAM, I, BOX(1,I), BOX(2,I)
            CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 45
 20         CONTINUE
         END IF
C                                       Icut
      WRITE (HILINE,1020) TSKNAM, REPORT
      CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 45
C                                       Doweight
      IF (DOWGHT.GT.0.0) THEN
         WRITE (HILINE,1025) TSKNAM, 'T'
      ELSE
         WRITE (HILINE,1025) TSKNAM, 'F'
         END IF
      CALL HIADD (HLUNO, HILINE, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 45
C
 45   CALL HICLOS (HLUNO, .TRUE., SCRTCH, IRET)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, HLUNI, HLUNO, DISKIN, DISKO, FCNO(1),
     *   SLOTO, CATNEW, SCRTCH(257), SCRTCH, IRET)
      IF (IRET.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                       close output file
 100  CALL MAPCLS ('INIT', DISKO, SLOTO, LUNO, INDO, CATNEW, .TRUE.,
     *   SCRTCH, IRET)
      NCFILE = NCFILE - 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,'BLC=',6(I5,','),I5,' / bottom left corner')
 1001 FORMAT (A,'TRC=',6(I5,','),I5,' / top right corner')
 1005 FORMAT (A,'YINC=',I4,'  ZINC=',I4,
     *   ' / Y, Z pixel input increments')
 1010 FORMAT (A,'CHANSEL(*,',I2.2,')=',2(I5,','),I3,
     *   ' / channels searched for signal')
 1011 FORMAT (A,'BOX(*,',I2.2,')=',I5,',',I5,
     *   ' / channels used for baseline')
 1020 FORMAT (A,'ICUT =',1PE11.4,' / S/N recorded if greater than ICUT')
 1025 FORMAT (A,'DOWEIGHT = ',A,' / solutions weighted by channel rms?')
      END
      SUBROUTINE FILTER (I, S, GAUSS)
C-----------------------------------------------------------------------
C   returns filter function symmetric about 0
C   Input:
C      I       I   separation in cells + 1
C      S       R   width in cells
C   Output:
C      GAUSS   R   Value of Gaussian
C-----------------------------------------------------------------------
      INTEGER   I
      REAL      S, GAUSS
C-----------------------------------------------------------------------
      GAUSS = (I-1) / S
      GAUSS = GAUSS * GAUSS
      GAUSS = EXP ( 4.0 * ALOG(0.5) * GAUSS)
C
 999  RETURN
      END
      SUBROUTINE PLTHST (NHIST, HISTSN, WIDTH, LAST, IRET)
C-----------------------------------------------------------------------
C   Plots the histogram on the TV or a plot file
C   Inputs:
C      NHIST    I      Number values in histogram (1, NHIST+2 for
C                      overflows)
C      HISTSN   I(*)   Histogram
C      WIDTH    R      Filter width in channels (for labeling)
C      LAST     L      This is the last plot?
C   Output:
C      IRET     I      Return code: 0 nominal, < 0 please quit,
C                      > 0 error
C-----------------------------------------------------------------------
      INTEGER   NHIST, HISTSN(*), IRET
      REAL      WIDTH
      LOGICAL   LAST
C
      INCLUDE 'SERCH.INC'
      CHARACTER STAT*4, PFILE*48
      INTEGER   I, BMAX, PBLK(256), IVER, PLUN, PIND, GRCHN, TVCORN(2),
     *   LABEL, IROUND, LTYPE
      LOGICAL   DOLOG
      REAL      ARANGE(2)
      INCLUDE 'INCS:DGPH.INC'
      DATA TVCORN /2*0/
C-----------------------------------------------------------------------
      LABEL = IROUND (XLABEL)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.GE.0) THEN
         LABEL = (LABEL/100)*100 + LTYPE
      ELSE
         LABEL = (LABEL/100)*100 - LTYPE
         END IF
      DOLOG = FUNTYP.EQ.'LG'
      ARANGE(1) = -10. - 20.0/(NHIST-1)
      ARANGE(2) =  10. + 20.0/(NHIST-1)
C
      IVER = 0
      IF (.NOT.DOTV) THEN
         STAT = 'WRIT'
         IF (LAST) STAT = 'READ'
         CALL MADDEX ('PL', FVOL(1), FCNO(1), CATBLK, PBLK, .TRUE.,
     *      STAT, IVER, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'CANNOT CREATE PLOT FILE - TURN OFF PLOT'
            GO TO 990
            END IF
         END IF
C
      GRCHN = XGRCH + 0.5
      GRCHN = MAX (1, MIN (7, GRCHN))
      CALL ZPHFIL ('PL', FVOL(1), FCNO(1), IVER, PFILE, IRET)
      CALL GINIT (FVOL(1), FCNO(1), PFILE, 0, 38, NPARMS, XNAMEI, DOTV,
     *   1, GRCHN, TVCORN, CATOLD, PBLK, PLUN, PIND, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       move zero
      I = NHIST/2 + 1
      HISTSN(NHIST+3) = HISTSN(I)
      HISTSN(I) = HISTSN(I-1) + HISTSN(I+1) - (HISTSN(I-2) +
     *   HISTSN(I+2)) / 2
C                                       Get max
      BMAX = -10
      DO 10 I = 1,NHIST
         BMAX = MAX (BMAX, HISTSN(I+1))
 10      CONTINUE
      CALL HISTOX (BMAX, NHIST, HISTSN, ARANGE, DOLOG, LABEL, WIDTH,
     *   IVER, PBLK, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR PLOTTING HISTOGRAM'
         CALL MSGWRT (7)
         END IF
      GPHPAG = .NOT.LAST
      CALL GFINIS (PBLK, IRET)
C                                       Successful plot file finished.
      IF (IRET.EQ.0) THEN
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (FVOL(1), FCNO(1), IVER, PBLK, IRET)
            WRITE (MSGTXT,1010) IVER
            CALL MSGWRT (5)
            IRET = 0
            END IF
         END IF
       GO TO 999
C
 990   DOHIST = 0.0
       CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Successful plot file (histogram) version',I7)
      END
      SUBROUTINE HISTOX (BMAX, NBOXES, XNIB, RANGE, DOLOG, LABEL, WIDTH,
     *   IVER, PBLK, IRET)
C-----------------------------------------------------------------------
C   This routine will write commands to an open plot file for drawing
C   a histogram.  Flux on X axis, count on Y axis.
C   Inputs:
C      BMAX     I        the maximum value in any of the boxes.
C      XNIB     I(NBOXES+2)   The number of entries in each range.
C                        XNIB(1) is the underflow, XNIB(NBOXES+2) is
C                        the overflow. (NBOXES+3) = actual zero count
C      NBOXES   I        number of boxes or value ranges for histogram.
C      RANGE    R(2)     intensity range of histogram
C      DOLOG    L        T => use log(n) rather than linear scale
C      LABEL    I        Type of labeling
C      WIDTH    R        Smoothing width in channels
C      IVER     I        Plot file version number
C   In/out:
C      PBLK    I(256)   I/O buffer for open, initialized pl file.
C   Output:
C      IRET     I        error code. 0=ok, 1=write error to plot file.
C-----------------------------------------------------------------------
      REAL      RANGE(2), WIDTH
      INTEGER   BMAX, XNIB(*), NBOXES, LABEL, IVER, PBLK(256), IRET
      LOGICAL   DOLOG
C
      REAL      BLC(7), CH(4), TRC(7), X, Y, FAC, XYRATO, YMAX(2)
      INTEGER   IDEPTH(5), I, LTYPE
C-----------------------------------------------------------------------
C                                       Set character offsets.
      IF (DOLOG) THEN
         YMAX(2) = ALOG10 (BMAX+0.0001)
         YMAX(1) = ALOG10 (0.5)
      ELSE
         YMAX(2) = BMAX
         YMAX(1) = 0.0
         END IF
      CALL GTICNT (LTYPE, YMAX, I)
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
      BLC(2) = 0.0
      IF (DOLOG) BLC(2) = ALOG10 (0.5)
      BLC(1) = 0.0
      TRC(1) = NBOXES
      TRC(2) = YMAX(2) * 1.05
      XYRATO = (TRC(1) - BLC(1)) / (TRC(2) - BLC(2))
C                                       Kludge to keep XYRATO small
C                                       to prevent overflow in GINITL.
      FAC = 1.0
      DO 50 I = 1,10000
         IF (XYRATO.GT.0.50) GO TO 60
         FAC = FAC * 10.0
         XYRATO = XYRATO * 10.0
 50      CONTINUE
C
 60   TRC(2) = TRC(2) / FAC
      BLC(2) = BLC(2) / FAC
      XYRATO = 1.0 / XYRATO
C                                       Initialize plot file line drw.
      CALL GINITL (BLC, TRC, XYRATO, CH, IDEPTH, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (1, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Draw borders.
      CALL GPOS (BLC(1), BLC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), BLC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (TRC(1), TRC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), TRC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GVEC (BLC(1), BLC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Labeling.
      I = NBOXES + 2
      CALL HLABX (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER, XNIB(1),
     *   XNIB(I), XNIB(I+1), WIDTH, LABEL, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL GLTYPE (2, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Position at first data point.
      X = BLC(1)
      Y = BLC(2)
      CALL GPOS (X, Y, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Loop for rest of data points.
      DO 100 I = 1,NBOXES
         Y = XNIB(I+1)
         IF (DOLOG) THEN
            IF (XNIB(I+1).LE.0) THEN
               Y = ALOG10 (0.5)
            ELSE
               Y = ALOG10 (Y)
               END IF
            END IF
         Y = Y / FAC
         CALL GVEC (X, Y, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         X = I
         CALL GVEC (X, Y, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         Y = BLC(2)
         CALL GVEC (X, Y, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HLABX (BLC, TRC, FAC, NBOXES, RANGE, DOLOG, IVER,
     *   UNDER, OVER, ZERO, WIDTH, LABEL, PBLK, IRET)
C-----------------------------------------------------------------------
C   Write labeling for histogram.
C   Inputs:
C      BLC     R(2)     bottom left corner of plot.
C      TRC     R(2)     top right hand corner of plot.
C      FAC     R        FAC*XYRATO = real XYRATIO.
C      IVER    I        plot file version number
C      LABEL   I        labeling type
C      PBLK   I(256)   I/O buffer for plot file.
C   Output:
C      IRET    I        error code returned from GVEC.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), FAC, RANGE(2), WIDTH
      INTEGER   NBOXES, IVER, UNDER, OVER, ZERO, LABEL, PBLK(256), IRET
      LOGICAL   DOLOG
C
      CHARACTER PREFIX*5, TIME*8, DATE*12, CTEMP*12, CSTOK(12)*4,
     *   NAMSTR*18, MSGBUF*80
      LOGICAL   PFLAG
      REAL      XINTER(21), DCX, DCY, XNOINT, DIST, ODIST, XDIST, XMAX,
     *   TICSCL, YTICEL, YTICER, XVAL, YPOS, TICLEN, XINT, X, FREQ,
     *   DCXM
      INTEGER   INOINT, INCHAR, I, IXO, M, ITRY, NXRA, NXDEC, NXLL,
     *   NXMM, NXFR, NXST, NAX, INC, IANGL, JSTOK, IT(3), ID(3),
     *   ICPNT, ITMP, LTYPE
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
      LTYPE = MOD (ABS(LABEL), 100)
      IF (LTYPE.LE.1) GO TO 999
C                                       Tic positions.
      TICLEN = (TRC(1) - BLC(1)) / TICSCL
      YTICEL = BLC(1) + TICLEN
      YTICER = TRC(1) - TICLEN
C                                       Find interval value.
      DIST = FAC * (TRC(2) - BLC(2))
      XINT = 9.0
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
         CALL GPOS (TRC(1), YPOS, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (YTICER, YPOS, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Left hand tic.
         CALL GPOS (YTICEL, YPOS, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (BLC(1), YPOS, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Write value.
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (IXO.GT.3) INCHAR = INCHAR - 2
            DCX = - INCHAR - 1.0
            DCY = -0.5
            DCXM = MIN (DCXM, DCX)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       Number of pixels
 110  DCX = DCXM - 2.0
      YPOS = (TRC(2) + BLC(2)) / 2.0
      CALL GPOS (BLC(1), YPOS, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (DOLOG) THEN
         MSGBUF = 'Log10 (number of pixels)'
         INCHAR = 24
      ELSE
         MSGBUF = 'Number of pixels'
         INCHAR = 16
         END IF
      DCY = INCHAR / 2.0 - 1.0
      CALL GCHAR (INCHAR, 1, DCX, DCY, MSGBUF, PBLK, IRET)
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
         X = I - .5
         CALL GPOS (X, YTICER, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, TRC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1115) I
            CALL CHTRIM (MSGBUF, 4, MSGBUF, INCHAR)
            DCX = 0.5 - REAL(INCHAR)
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 150     CONTINUE
C                                       Label RHS bucket #
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, TRC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = -5.0
      DCY = 0.5
      IF (LTYPE.GT.2) DCY = 1.833
      MSGBUF = 'Box number'
      CALL GCHAR (10, 0, DCX, DCY, MSGBUF, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Range =
      CTEMP = 'Signal/Noise'
      IF (LTYPE.LT.7) THEN
         CALL GPOS (BLC(1), BLC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         DCX = 0.0
         DCY = -2.833
         IF (LTYPE.GT.2) DCY = DCY - 1.333
         WRITE (MSGBUF,1151) RANGE(1), RANGE(2), CTEMP
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Interval =
         CALL GPOS (BLC(1), BLC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         X = (RANGE(2) - RANGE(1)) / NBOXES
         WRITE (MSGBUF,1152) X, CTEMP, WIDTH
         DCY = DCY - 1.333
         CALL REFRMT (MSGBUF, '_', INCHAR)
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
C                                       Underflow = overflow =
         CALL GPOS (BLC(1), BLC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         IF ((UNDER.NE.0) .OR. (OVER.NE.0)) THEN
            WRITE (MSGBUF,1154) UNDER, OVER, ZERO
         ELSE
            WRITE (MSGBUF,1155) ZERO
            END IF
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY - 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
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
         IF (XNOINT.LE.9.0) GO TO 170
 160     CONTINUE
      GO TO 190
C                                       Bottom (value) tics
 170  XINT = XINTER(ITRY)
      DCY = -1.5
      XMAX = MAX (ABS(RANGE(2)), ABS(RANGE(1))) * XDIST
      INOINT = XNOINT + 2.5
      XVAL = AINT (ODIST/XINT) * XINT
      IF (XVAL.GT.ODIST) XVAL = XVAL - XINT
      DO 175 I = 1,INOINT
         XVAL = XVAL + XINT
         X = ((XVAL-ODIST)/DIST) * NBOXES
         IF (X.GT.TRC(1)) GO TO 180
         CALL GPOS (X, YTICEL, PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL GVEC (X, BLC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (LTYPE.GT.2) THEN
            WRITE (MSGBUF,1030) XVAL
            CALL CHTRIM (MSGBUF, 14, MSGBUF, INCHAR)
            IF (ITRY.GT.3) INCHAR = INCHAR - 2
            DCX = 0.5 - INCHAR
            CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 175     CONTINUE
C                                       Label with prefix
 180  DCY = -1.5
      IF (LTYPE.GT.2) DCY = -2.833
      X = (TRC(1) + BLC(1)) / 2.0
      CALL GPOS (X, BLC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (MSGBUF,1175) PREFIX, CTEMP
      CALL CHTRIM (MSGBUF, 18, MSGBUF, INCHAR)
      DCX = 0.5 - INCHAR / 2.0
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (LTYPE.GE.7) GO TO 999
C                                       which axis is which?
 190  NXRA = 0
      NXDEC = 0
      NXLL = 0
      NXMM = 0
      NXFR = 0
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
      CALL GPOS (BLC(1), TRC(2), PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
      DCX = 0.0
      DCY = 1.833
      IF (LTYPE.GT.2) DCY = DCY + 1.333
      IANGL = 0
      CALL H2CHR (8, 1, CATH(KHOBJ), CTEMP)
      FREQ = 0.0
      JSTOK = 12
      IF (NXFR.GT.2) FREQ = CATD(KDCRV+NXFR-1) + CATR(KRCIC+NXFR-1)
     *   * (BLC(NXFR) - CATR(KRCRP+NXFR-1))
      FREQ = FREQ / 1.E6
      IF (NXST.GT.2) JSTOK = CATD(KDCRV+NXST-1) + CATR(KRCIC+NXST-1)
     *   * (BLC(NXST) - CATR(KRCRP+NXST-1)) + 2.5
      IF (NXFR.GT.2) THEN
         WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK), FREQ
      ELSE
         WRITE (MSGBUF,1200) CTEMP, CSTOK(JSTOK)
         END IF
      CALL REFRMT (MSGBUF, '_', INCHAR)
C                                       image name
      INCHAR = INCHAR + 1
      IF (INCHAR.GT.1) THEN
         MSGBUF(INCHAR:INCHAR+2) = '   '
         INCHAR = INCHAR + 3
         END IF
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMSTR(1:12))
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), NAMSTR(13:18))
      CALL NAMEST (NAMSTR, CATBLK(KIIMS), MSGBUF(INCHAR:), ITMP)
      CALL REFRMT (MSGBUF, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       time/date, version
      IF (LABEL.GT.0) THEN
         CALL GPOS (BLC(1), TRC(2), PBLK, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, TIME, DATE)
         WRITE (MSGBUF,1210) IVER, DATE, TIME
         CALL REFRMT (MSGBUF, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GCHAR (INCHAR, 0, DCX, DCY, MSGBUF, PBLK, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F14.1)
 1115 FORMAT (I4)
 1151 FORMAT ('Range =',F6.1,' to',F5.1,1X,A)
 1152 FORMAT ('Interval =',F7.4,1X,A,3X,'Width',F5.1,' channels')
 1154 FORMAT ('Underflow=',I10,' _Overflow=',I10,' _Zero=',I10)
 1155 FORMAT ('Count at zero =',I12)
 1175 FORMAT (A5,1X,A)
 1200 FORMAT (A8,' _',A4,'_ ',F10.3,' MHz')
 1210 FORMAT ('Plot file version',I4,'__created ',A,A)
      END

