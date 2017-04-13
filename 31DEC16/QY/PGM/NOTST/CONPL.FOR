LOCAL INCLUDE 'CONPL.INC'
      CHARACTER NAMEIN*12, CLAIN*6, OUTFIL*48
      HOLLERITH XNAMIN(3), XCLAIN(2), XOUFIL(12)
      REAL      USERID, XSIN, XDISIN, XTYPE, XPARM(10), BPARM(10),
     *   DOCIRC, XDOTV, XGRCH
      INTEGER   SBUF(1024), CXTYPE, SEQIN, DISKIN, PVER, CNO, NPARM,
     *   GRCHN, TVCHN, TVCORN(4)
      LOGICAL   SCALEM(2,2), DOTV
C                                       Include for CONPL.
      COMMON /INPARM/ USERID, XNAMIN, XCLAIN, XSIN, XDISIN, XTYPE,
     *   XPARM, XOUFIL, BPARM, DOCIRC, XDOTV, XGRCH
      COMMON /CHPARM/ NAMEIN, CLAIN, OUTFIL
      COMMON /BUFRS/ SBUF
      COMMON /UVPCOM/ SCALEM, SEQIN, DISKIN, CNO, NPARM, CXTYPE,
     *   GRCHN, TVCHN, TVCORN, DOTV, PVER
LOCAL END
       PROGRAM CONPL
C-----------------------------------------------------------------------
C!  Task plots AIPS convolving functions
C#  Imaging AP Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2008-2009, 2015
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
C   CONPL plots an AIPS convolving function
C   Inputs:
C     USERID                       file owner # ignored
C     INNAME         NAMEIN        Name of main input file
C     INCLASS        CLAIN         Class of main input file
C     INSEQ          SEQIN         Seq. of main input file
C     INDISK         DISKIN        Disk number of main input file.
C     BPARM......Plot control parameters:
C        1 = Minimum of X-axis.
C        2 = Maximum of X-axis (if = BPARM(1) do self-scale in X).
C        3 = Minimum of Y-axis.
C        4 = Maximum of Y-axis (if = BPARM(3) do self-scale in Y).
C     DOTV........> 0 => plot directly on the TV device, otherwise
C                 make a plot file for later display on one or
C                 more devices (including the TV if desired).
C     GRCHAN......Graphics channel (1 - 3) to use for line drawing.
C                 0 => 1.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IERR, IRET
      INCLUDE 'CONPL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'CONPL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CONIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do plot
      CALL CONPLT (IRET)
C                                       Clear catlg on error
 990  IF ((IRET.EQ.0) .OR. (NCFILE.LT.1) .OR. (FRW(1).GT.0) .OR.
     *   (DOTV)) GO TO 995
         CALL DELEXT ('PL', FVOL(1), FCNO(1), 'READ', CATBLK, SBUF,
     *      PVER, IERR)
         NCFILE = NCFILE - 1
C                                       Close down
 995  CALL DIE (IRET, SBUF)
C
 999  STOP
      END
      SUBROUTINE CONIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   CONIN gets input parameters for CONPL .
C   Inputs:  PRGM   C*6)      Program name
C   Output:  JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER INTYPE*2, STAT*4, PRGM*6
      INTEGER  JERR, IUSER, IERR, IROUND
      LOGICAL   T
      INCLUDE 'CONPL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init IO et al.
      CALL ZDCHIN (.TRUE., SBUF)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      PVER = 10000
C                                       Get input parameters.
      NPARM = 44
      CALL GTPARM (PRGM, NPARM, RQUICK, USERID, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         JERR = 8
         RQUICK = .TRUE.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, SBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMIN, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XOUFIL, OUTFIL)
C                                       Crunch input parameters.
      USERID = NLUSER
      IUSER = NLUSER
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Get CATBLK from file.
      CNO = 1
      INTYPE = ' '
      CALL CATDIR ('SRCH', DISKIN, CNO, NAMEIN, CLAIN, SEQIN, INTYPE,
     *   IUSER, STAT, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, IUSER
         GO TO 990
 20   STAT = 'WRIT'
      IF (DOTV) STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNO, CATBLK, STAT, SBUF, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         GO TO 990
 30   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 1
      XSIN = SEQIN
      XDISIN = DISKIN
C                                       Set gridding function defaults.
      CXTYPE = IROUND (XTYPE)
      CXTYPE = ABS (CXTYPE)
      IF (CXTYPE.GT.10) CXTYPE = CXTYPE - 10
      CALL GRDFLT (CXTYPE, CXTYPE, XPARM, XPARM)
C                                       Update catalog header for PL
      FRW(NCFILE) = 0
      PVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISKIN, FCNO(1), CATBLK, SBUF, T, 'READ',
     *      PVER, IERR)
         IF (IERR.NE.0) THEN
            NCFILE = NCFILE - 1
            WRITE (MSGTXT,1060) IERR
            GO TO 990
            END IF
         END IF
C                                       Autoscale ?
      SCALEM(1,1) = BPARM(1).GE.BPARM(2)
      SCALEM(2,1) = BPARM(3).GE.BPARM(4)
      SCALEM(1,2) = BPARM(5).GE.BPARM(6)
      SCALEM(2,2) = BPARM(7).GE.BPARM(8)
      GO TO 999
C                                       Error message
 990  CALL MSGWRT (8)
      JERR = 5
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I4,' FINDING ',A12,'.',A6,'.',I4,' DISK',I2,
     *   ' USER',I5)
 1020 FORMAT ('ERROR',I4,' READING CATALOG HEADER')
 1060 FORMAT ('ERROR',I5,' ADDING PL FILE TO HEADER')
      END
      SUBROUTINE CONPLT (IRET)
C-----------------------------------------------------------------------
C   CONPLT actually plots table data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C                          1 => failed to add to catalog
C                          2 => failed to create
C                          3 => graph file write error
C                          4 => UV file IO error
C-----------------------------------------------------------------------
      CHARACTER TEXT*1024, PFILE*48, TIME*8, DATE*12, CHTYPS(6)*10
      INTEGER   BUFFER(256), IERR, ITYPE, IPSIZE, I, IRET, MM, LIM, NN,
     *   LUNPL, FINDPL, IAPARM(8), INCHAR, INP, J, K, IT(3), ID(3),
     *   NUMPRM(6), NGOOD(2), NNOFIT(2), KAP, XCEN, XSIZ(3), LIM2,
     *   XCEN2, FTSIZ, L, TLUN, TIND, ITRIM, IL
      REAL      BLC(2), TRC(2), CHOUT(4), XYRATO, DX, CBUF(8192),
     *   DY, XY(2), XYSCL(2), XYOFF(2), TEMP, DBUF(8192), EBUF(8192),
     *   XSCAL, FTFAC, FTMAX
      LOGICAL   DOGRID, DOWN
      INCLUDE 'CONPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNPL, TLUN /26, 3/
      DATA NUMPRM /1, 3, 2, 4, 2, 4/
      DATA CHTYPS /'PILL BOX', 'EXPONTIAL', 'SIN(X) / X', 'EXP * SINC',
     *   'SPHEROIDAL', 'EXP*BESSJ1'/
      DATA XSIZ /850, 510, 1100/
      DATA FTSIZ /2048/
C-----------------------------------------------------------------------
      NGOOD(1) = 0
      NNOFIT(1) = 0
      NGOOD(2) = 0
      NNOFIT(2) = 0
      IRET = 1
C                                       Open text file
      TIND = 0
      IF (OUTFIL.NE.' ') THEN
         CALL ZTXOPN ('WRIT', TLUN, TIND, OUTFIL, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            OUTFIL = ' '
            TIND = 0
            END IF
         END IF
C                                       Open AP
C                                       default will be enough
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         IRET = 10
         MSGTXT = 'CANNOT GET ANY AP MEMORY'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Compute function
      XSCAL = 0.01
      MM = MAX (XPARM(1), 1.0) + 0.001
      MM = 2 * MM + 1
      LIM = 100 * MM + 1
      XCEN = 50 * MM + 1
      CALL CONVFN (1, CXTYPE, XPARM, CBUF)
      IF (SCALEM(1,1)) THEN
         BPARM(1) = - 0.5 * MM
         BPARM(2) = 0.5 * MM
         END IF
      IF (SCALEM(2,1)) THEN
         BPARM(3) = 1.E6
         BPARM(4) = -BPARM(3)
         DO 10 I = 1,LIM
            BPARM(3) = MIN (BPARM(3), CBUF(I))
            BPARM(4) = MAX (BPARM(4), CBUF(I))
 10         CONTINUE
         END IF
C                                       Adjust the scales
      DO 20 I = 1,2
         XYSCL(I) = BPARM(2*I)
         XYOFF(I) = BPARM(2*I-1)
         TEMP = 0.035 * (XYSCL(I) - XYOFF(I))
         XYSCL(I) = XYSCL(I) + TEMP
         XYOFF(I) = XYOFF(I) - TEMP
         IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
         XYSCL(I) = XSIZ(I) / (XYSCL(I)-XYOFF(I))
 20      CONTINUE
C                                       Fill in last of actual parms
      BPARM(2) = XSIZ(1)/XYSCL(1) + XYOFF(1)
      BPARM(4) = XSIZ(2)/XYSCL(2) + XYOFF(2)
      BPARM(1) = XYOFF(1)
      BPARM(3) = XYOFF(2)
C                                       Create plot file
      CALL ZPHFIL ('PL', DISKIN, FCNO(1), PVER, PFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      IPSIZE = 0
      ITYPE = 15
      CALL GINIT (DISKIN, FCNO(1), PFILE, IPSIZE, ITYPE, NPARM, USERID,
     *   DOTV, TVCHN, GRCHN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IERR)
      IRET = 2
      IF (IERR.NE.0) GO TO 999
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = XSIZ(1)
      TRC(2) = XSIZ(3)
      XYRATO = 1.0
      CHOUT(1) = 9.0
      CHOUT(2) = 3.0
      CHOUT(3) = 0.5
      CHOUT(4) = 4.0
      IRET = 3
      CALL FILL (5, 1, IAPARM)
C                                       Set up location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      DO 30 I = 1,2
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = 1.0 / XYSCL(I)
 30      CONTINUE
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      CTYP(1,LOCNUM) = 'PIXELS'
      CTYP(2,LOCNUM) = 'CONVOLUTION FUNCTION'
      CALL CHNTIC (BLC, TRC, INP)
      IF (INP.GT.0) CHOUT(1) = 4.5 + INP
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IAPARM, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       Reset for half drawing
      BLC(2) = TRC(2) - XSIZ(2)
      RPLOC(2,LOCNUM) = BLC(2)
C                                       Draw border
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) PVER, DATE, TIME
      INCHAR = 51
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Text file header
      IF (OUTFIL.NE.' ') THEN
         IL = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
         IF (IERR.NE.0) OUTFIL = ' '
         END IF
      IF (OUTFIL.NE.' ') THEN
         TEXT = '******** Convolution function ********'
         IL = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
         IF (IERR.NE.0) OUTFIL = ' '
         END IF
C                                       Top labels: convolution type
      DY = DY - 1.6
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      WRITE (TEXT,1031) CHTYPS(CXTYPE), (XPARM(I), I = 1,NUMPRM(CXTYPE))
      INCHAR = 18 + 8 * NUMPRM(CXTYPE)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Text file parameters, type
      IF (OUTFIL.NE.' ') THEN
         IL = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
         IF (IERR.NE.0) OUTFIL = ' '
         END IF
      IF (OUTFIL.NE.' ') THEN
         TEXT = 'X cells    func'
         IL = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
         IF (IERR.NE.0) OUTFIL = ' '
         END IF
C                                       Put on labels and ticks
      DOGRID = DOCIRC.GT.0.0
      CALL CLAB1 (BLC, TRC, CHOUT, 3, XYRATO, DOGRID, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Read table file:
      DX = 0.005 * XYSCL(1)
      DOWN = .FALSE.
C                                       Loop
      DO 50 I = 1,LIM
         XY(1) = XSCAL * (I - XCEN)
         XY(2) = CBUF(I)
         IF (OUTFIL.NE.' ') THEN
            IF ((I.GE.XCEN) .AND. (MOD(I-XCEN,2).EQ.0)) THEN
               WRITE (TEXT,1035) XY
               IL = ITRIM (TEXT)
               CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
               IF (IERR.NE.0) OUTFIL = ' '
               END IF
            END IF
         DO 40 J = 1,2
            XY(J) = XYSCL(J) * (XY(J) - XYOFF(J))
            XY(J) = XY(J) + BLC(J)
            IF ((XY(J).LT.BLC(J)) .OR. (XY(J).GT.TRC(J))) THEN
               NNOFIT(1) = NNOFIT(1) + 1
               DOWN = .FALSE.
               GO TO 50
               END IF
 40         CONTINUE
         NGOOD(1) = NGOOD(1) + 1
         IF (DOWN) THEN
            CALL GVEC (XY(1), XY(2), BUFFER, IRET)
         ELSE
            CALL GPOS (XY(1), XY(2), BUFFER, IRET)
            END IF
         IF (IRET.NE.0) GO TO 970
         DOWN = .TRUE.
 50      CONTINUE
C                                       S/N computations
      IF (BPARM(9).LE.-1.0) THEN
         LIM2 = LIM * 2 - 1
         XCEN2 = LIM
C                                       autocorrelation of C.F.
         NN = 2 * MM - 1
         CALL RFILL (LIM2, 0.0, DBUF)
         DO 110 I = 1,NN
            L = (I - (NN+1)/2) * 100 + XCEN2
            DO 105 J = 1,LIM
               K = J + XCEN2 - L
               IF ((K.GE.1) .AND. (K.LE.LIM)) DBUF(L) = DBUF(L) +
     *            CBUF(J) * CBUF(K)
 105           CONTINUE
 110        CONTINUE
C                                       Fourier transform it
         FTFAC = TWOPI / (100.0 * FTSIZ)
         XCEN = FTSIZ/2 + 1
         FTMAX = -1.E10
         DO 120 I = 1,FTSIZ
            TEMP = FTFAC * (I - XCEN)
            EBUF(I) = 0.0
            DO 115 J = 1,NN
               K = (J - (NN+1)/2) * 100
               EBUF(I) = EBUF(I) + COS (TEMP*K) * DBUF(K+XCEN2)
 115           CONTINUE
            FTMAX = MAX (FTMAX, EBUF(I))
 120        CONTINUE
         IF (FTMAX.NE.0.0) THEN
            DO 125 I = 1,FTSIZ
               EBUF(I) = EBUF(I) / FTMAX
 125           CONTINUE
            END IF
         END IF
C                                       Plot the other half: FFT
      IF (BPARM(9).LE.0.0) THEN
         LIM = FTSIZ
         XCEN = LIM/2 + 1
         I = 4 * LIM
         CALL GRDTAB (LIM, XCEN, CXTYPE, XPARM, 0, I, DBUF)
         CALL QGET (CBUF, 0, LIM, 2)
         CALL QWD
         XSCAL = 1.0 / LIM
         IF (SCALEM(1,2)) THEN
            BPARM(5) = -0.5
            BPARM(6) = 0.5 - XSCAL
            END IF
C                                       Noise-to-signal
         IF (BPARM(9).LE.-1.0) THEN
            FTMAX = CBUF(XCEN) * SQRT (MAX (1.E-10, EBUF(XCEN)))
            DO 130 I = 1,FTSIZ
               CBUF(I) = CBUF(I) * SQRT (MAX (1.E-10, EBUF(I))) / FTMAX
 130           CONTINUE
            END IF
C                                       Log
         IF (BPARM(10).GT.1.5) THEN
            IF (BPARM(9).LE.-1.0) THEN
               CTYP(2,LOCNUM) = 'LOG (SIGNAL / NOISE)'
            ELSE
               CTYP(2,LOCNUM) = 'LOG (FFT (CONV FUNC))'
               END IF
            DO 140 I = 1,LIM
               IF (CBUF(I).NE.0.0) THEN
                  CBUF(I) = -LOG10 (ABS (CBUF(I)))
               ELSE
                  CBUF(I) = 0.0
                  END IF
 140           CONTINUE
         ELSE IF (BPARM(10).GT.0.0) THEN
            IF (BPARM(9).LE.-1.0) THEN
               CTYP(2,LOCNUM) = 'SIGNAL / NOISE'
            ELSE
               CTYP(2,LOCNUM) = 'FFT (CONV FUNC)'
               END IF
            DO 145 I = 1,LIM
               IF (CBUF(I).NE.0.0) THEN
                  CBUF(I) = 1.0 / CBUF(I)
               ELSE
                  CBUF(I) = 0.0
                  END IF
 145           CONTINUE
         ELSE
            IF (BPARM(9).LE.-1.0) THEN
               CTYP(2,LOCNUM) = 'NOISE / SIGNAL'
            ELSE
               CTYP(2,LOCNUM) = '1 / FFT (CONV FUNC)'
               END IF
            END IF
         CTYP(1,LOCNUM) = 'IMAGE DIAMETERS'
C                                       Convolve conv func w Gaussian
      ELSE
         CTYP(2,LOCNUM) = 'SMOOTHED CONV FUNC'
         CALL RCOPY (LIM, CBUF, DBUF)
         LIM2 = LIM
         TEMP = SQRT ((300. * BPARM(9))**2 + (LIM)**2)
         LIM = TEMP / 2
         LIM = 2 * LIM + 1
         XCEN2 = (LIM2+1)/2
         XCEN = (LIM+1)/2
         FTFAC = 4.0 * LOG (2.0) / BPARM(9) / BPARM(9) / 1.0E4
         FTMAX = -1.E10
         DO 160 I = 1,LIM
            CBUF(I) = 0.0
            DO 155 J = 1,LIM2
               TEMP = FTFAC * ((J - I - XCEN2 + XCEN) ** 2)
               IF (TEMP.LT.10.0) CBUF(I) = CBUF(I) + DBUF(J) *
     *            EXP (-TEMP)
 155           CONTINUE
            FTMAX = MAX (FTMAX, CBUF(I))
 160        CONTINUE
         IF (FTMAX.NE.0.0) THEN
            DO 165 I = 1,LIM
               CBUF(I) = CBUF(I) / FTMAX
 165           CONTINUE
            END IF
         IF (SCALEM(1,2)) THEN
            BPARM(5) = (1 - XCEN) * XSCAL
            BPARM(6) = (LIM - XCEN) * XSCAL
            END IF
         END IF
C                                       self-scale Y values
      IF (SCALEM(2,2)) THEN
         BPARM(7) = 1.E6
         BPARM(8) = -BPARM(7)
         DO 215 I = 1,LIM
            BPARM(7) = MIN (BPARM(7), CBUF(I))
            BPARM(8) = MAX (BPARM(8), CBUF(I))
 215        CONTINUE
         END IF
C                                       Adjust the scales
      DO 220 I = 1,2
         XYSCL(I) = BPARM(4+2*I)
         XYOFF(I) = BPARM(3+2*I)
         TEMP = 0.035 * (XYSCL(I) - XYOFF(I))
         XYSCL(I) = XYSCL(I) + TEMP
         XYOFF(I) = XYOFF(I) - TEMP
         IF (XYSCL(I).EQ.XYOFF(I)) GO TO 999
         XYSCL(I) = XSIZ(I) / (XYSCL(I)-XYOFF(I))
 220     CONTINUE
C                                       Fill in last of actual parms
      BPARM(6) = XSIZ(1)/XYSCL(1) + XYOFF(1)
      BPARM(8) = XSIZ(2)/XYSCL(2) + XYOFF(2)
      BPARM(5) = XYOFF(1)
      BPARM(7) = XYOFF(2)
C                                       bottom half coords
      BLC(2) = 0.0
      TRC(2) = XSIZ(2)
      DO 230 I = 1,2
         RPLOC(I,LOCNUM) = BLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I)
         AXINC(I,LOCNUM) = 1.0 / XYSCL(I)
 230     CONTINUE
C                                       Draw border
      CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Text file header
      IF (OUTFIL.NE.' ') THEN
         TEXT = '****** ' // CTYP(2,LOCNUM)(:ITRIM(CTYP(2,LOCNUM))) //
     *      ' vs ' // CTYP(1,LOCNUM)(:ITRIM(CTYP(1,LOCNUM)))  //
     *      ' ******'
         IL = ITRIM (TEXT)
         CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
         IF (IERR.NE.0) OUTFIL = ' '
         END IF
C                                       Put on labels and ticks
      CALL CLAB1 (BLC, TRC, CHOUT, 3, XYRATO, DOGRID, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Read table file:
      DOWN = .FALSE.
C                                       Loop
      DO 250 I = 1,LIM
         XY(1) = XSCAL * (I - XCEN)
         XY(2) = CBUF(I)
         IF (OUTFIL.NE.' ') THEN
            IF ((I.GE.XCEN) .AND. (MOD(I-XCEN,2).EQ.0)) THEN
               WRITE (TEXT,1235) XY
               IL = ITRIM (TEXT)
               CALL ZTXIO ('WRIT', TLUN, TIND, TEXT(:IL), IERR)
               IF (IERR.NE.0) OUTFIL = ' '
               END IF
            END IF
         DO 240 J = 1,2
            XY(J) = XYSCL(J) * (XY(J) - XYOFF(J))
            XY(J) = XY(J) + BLC(J)
            IF ((XY(J).LT.BLC(J)) .OR. (XY(J).GT.TRC(J))) THEN
               NNOFIT(2) = NNOFIT(2) + 1
               DOWN = .FALSE.
               GO TO 250
               END IF
 240        CONTINUE
         NGOOD(2) = NGOOD(2) + 1
         IF (DOWN) THEN
            CALL GVEC (XY(1), XY(2), BUFFER, IRET)
         ELSE
            CALL GPOS (XY(1), XY(2), BUFFER, IRET)
            END IF
         IF (IRET.NE.0) GO TO 970
         DOWN = .TRUE.
 250     CONTINUE
C                                       Done: finish plot
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IRET = 0
         GO TO 990
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 990
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         END IF
      IF (TIND.GT.0) CALL ZTXCLS (TLUN, TIND, IERR)
      GO TO 999
C                                       No catalog update
C                                       Messages
 990  WRITE (MSGTXT,1990) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT(1)+NNOFIT(2).GT.0) THEN
         WRITE (MSGTXT,1991) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISKIN, FCNO(1), PVER, BUFFER, IERR)
         WRITE (MSGTXT,1993) PVER
         CALL MSGWRT (2)
         END IF
      IF (TIND.GT.0) CALL ZTXCLS (TLUN, TIND, IERR)
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Plot file version',I4,'  created ',A12,A8)
 1031 FORMAT (A,' PARMS =',8F8.4)
 1035 FORMAT (F7.3,F8.3)
 1235 FORMAT (2F9.3)
 1970 FORMAT ('CONPLT: Error during graphing. will try to finish',
     *   ' partial graph')
 1990 FORMAT ('CONPLT: ',2I8,' points plotted')
 1991 FORMAT ('CONPLT: ',2I8,' points did not fit')
 1993 FORMAT ('CONPLT: Plot file version',I4,' created.')
      END
