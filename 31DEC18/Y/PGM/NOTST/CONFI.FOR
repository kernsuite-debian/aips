LOCAL INCLUDE 'FIT2A.INC'
C                                       Local include for FIT2A
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IBLNK
      INTEGER   CATOLD(256)
      INTEGER   CATBLK(256), FDVEC(50), TBIND, NBPIX,
     *   TAPEBP, TABLES, INBUFF(MABFSS), TAPBUF(29184)
      LOGICAL   ISBLNK, FUCKUP, STDEXT
      REAL      CATR(256)
      HOLLERITH CATH(256)
      CHARACTER HDRBUF*2880
      DOUBLE PRECISION CATD(128), POS11(2), SCALE, OFFSET, ISCALE,
     *   IZERO
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, TAPBUF,
     *   INBUFF, IBLNK, ISBLNK, FUCKUP, STDEXT,
     *   FDVEC, TBIND, NBPIX, TAPEBP, TABLES
      COMMON /MAPHDR/ CATBLK
      COMMON /MYCAT/ CATOLD
      COMMON /CHRCOM/ HDRBUF
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
C                                                          End FIT2A
LOCAL END
C
LOCAL INCLUDE 'CONFI.INC'
C                                                          Include CONFI
C                                       Local include for CONFI
      INTEGER MXPLT
      PARAMETER (MXPLT = 400)
C                                       array size for mask
      INTEGER MXMASK
      PARAMETER (MXMASK = 2000*2000)
C                                       array size for plot
      INTEGER MXNAR
      PARAMETER (MXNAR = 1000)
      INTEGER MXARPL
      PARAMETER (MXARPL = 100*100 + 400)
      INTEGER SEQIN, DISKIN, CNOIN
      INTEGER TVCHN, PLTBLK(256), GRCHN, TVCORN(4), LUNPR, PFIND,
     *   IGLUN, IGFIND, SLOT, NARRAY, NPARM, NITER, NBAD, NUMLEV
      LOGICAL DOTV, FA, DOCONF, DOPB, DOFITS
      REAL      XSIN, XDISIN, XNCOUN, XDOTV, APARM(10), BPARM(10),
     *   CPARM(10), LATIT, LONGT, ALT, ANTDIM, ANTEF, ANTSYS,
     *   CHOUT(4), FUNCMX, FUNCMN, ARGMX, ARGMN, XYSCL(2), GAIN,
     *   XYOFF(2), GMXX, GMNX, GXYSCL(2,MXPLT), GXYOFF(2,MXPLT),
     *   XCELL(2), XPARM(10), DPARM(10), XX(MXARPL),
     *   YY(MXARPL), XBAD(MXMASK), YBAD(MXMASK), XBLCC, YBLCC,
     *   XTRCC, YTRCC, XMAX, YMAX, XMIN, YMIN
      CHARACTER INFILE*48, I2FILE*48, OFILE*48, NAMEIN*12, CLAIN*6
      HOLLERITH XNAMEI(3), XCLAIN(2), XIFILE(12), X2FILE(12),
     *   XOFILE(12)
C                                       Buffers and file info
      INTEGER   BUFFER(1024)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XIFILE, X2FILE,
     *   XCELL, XPARM, XOFILE, XNCOUN, XDOTV, APARM, BPARM,
     *   CPARM, DPARM
      COMMON /CHAR/ INFILE, I2FILE, OFILE, NAMEIN, CLAIN
      COMMON /INTREA/ SEQIN, DISKIN, CNOIN, BUFFER, NARRAY,
     *   GMXX, GMNX, GXYSCL, GXYOFF, XYSCL, XYOFF, FUNCMX, FUNCMN,
     *   ARGMX, ARGMN, NPARM, GAIN, NITER, XX, YY, NBAD, XBAD,
     *   YBAD, XBLCC, YBLCC, XTRCC, YTRCC, LATIT, LONGT, ALT,
     *   ANTDIM, ANTEF, ANTSYS, NUMLEV, XMAX, YMAX, XMIN, YMIN,
     *   DOFITS
      COMMON /PLOT/ PLTBLK, CHOUT, DOTV, TVCHN, GRCHN, TVCORN,
     *   IGLUN, IGFIND, SLOT, LUNPR, PFIND, FA, DOCONF, DOPB
LOCAL END
      PROGRAM CONFI
C-----------------------------------------------------------------------
C! Optimize array's configuration minimizing sidelobes
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2000-2005, 2007, 2010, 2012, 2015
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
C   Optimize array's configuration minimizing sidelobes.
C   Inputs:
C     INNAME.....Input UV file name (name).      Standard defaults.
C     INCLASS....Input UV file name (class).     Standard defaults.
C     INSEQ......Input UV file name (seq. #).    0 => highest.
C     INDISK.....Disk drive # of input UV file.  0 => any.
C     INFILE.....The name of a file where the initial configuration
C                is stored
C     IN2FILE....The name of a file where the restricted positions
C                are stored
C     OFILE......The name of a file where the final configuration is
C                recorded
C     NCOUNT.....Number of plots at the page.
C     DOTV.......> 0 Do plot on the TV, else make a plot file
C     APARM......Parameters
C     BPARM......More parameters
C     DPARM......More parameters
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'CONFI.INC'
      DATA PRGM /'CONFI'/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL OPTI (PRGM, IRET)
      CALL DIE (IRET, BUFFER)
      STOP
      END
      SUBROUTINE OPTI (PRGN, JERR)
C-----------------------------------------------------------------------
C   OPTI gets input parameters for CONFI.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER MPOINT
      PARAMETER (MPOINT = 1000)
      INTEGER MXPOIN
      PARAMETER (MXPOIN = MPOINT*MPOINT)
      INCLUDE 'CONFI.INC'
      INTEGER IARRAY, KARRAY, NPLTOT, JERR, IERR, IROUND, IRET, JT,
     *   NLINE, ITER, NPLOTC, IPLTC, I, NCOUNT, NCOUNC, IPLOTC, NCH,
     *   JTRIM, K, IUV, NUV, NUVC, NPLOTS, KXTETA, IPLT, IPLOT, NUVTOT,
     *   NXTETA, NYTETA, NTOTX, NTOTY, IXTETA, IYTETA, IXY,
     *   MITER, KBAD,
     *   NBADMN, RADIUS, RADMAX, NINNER, NOUTER, NINTER, NTOT, NCEN,
     *   NROW, NXX, NINEST,  NX, NY, NROT, ADD1, NFIX, NXMIN, RADMIN
      REAL  REU, IMU, BEAZEN(MXPOIN), DPP, ANUMER,
     *   ELEV, EL, RDIRR(MPOINT), BEAM(MPOINT), X(MXNAR), Y(MXNAR),
     *   DX(MXNAR), DY(MXNAR), TOLER, TMAX, TMIN, TDIF, SIZEX, SIZEY,
     *   STEPAN, ANGLE, RMAX, XT, YT, RTEM, CELLX, CELLY, ARSIZE, XTEM,
     *   YTEM, XMASK(MXNAR), YMASK(MXNAR), XBEST(MXNAR), YBEST(MXNAR),
     *   XTEM1, YTEM1, XT1, YT1, DLIMIT, NEWDIS, XTETA, YTETA, ARG,
     *   SECZEN, WZENX, WZENY, DPPX, DPPY, DTETA,
     *   SCALE, MINZEN, MZENX, MZENY, XFOBAD(100), YFOBAD(100), RINNER,
     *   ROUTER, RT, SINNER, SOUTER, S1, S2, S3, RINTER, RINEST, RSUM,
     *   XSHIFT, YSHIFT, ROT, DROT, XSHFOU, YSHFOU, ROTFOU, HALFSE,
     *   SECTOR, RSHIFT, RTIN, RMIN, RMINAR, X0, DELTAX, DELTAY, XXX,
     *   YYY, BESS1, BESSJ1, ARGPB, PB, MAXVAL, DIST, ALPHA,
     *   SMINOR, SMAJOR, XELIPS, YELIPS, RELIPS, EPS
      INTEGER CONV1, CONV2, ZT, IMIN, KMIN, IND
      INTEGER INCOOR
      CHARACTER PRGN*6, LINE*80, STAT*4, UTYPE*2
      LOGICAL T, F, WREAD, BAD, TOPOGR,
     *   DONUTS, TWOCIR, TRECIR, DOSECT, OUSIDE, ELLIPS
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA EPS  /0.001/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFFER)
      CALL VHDRIN
      JERR = 0
C      NPARM = 77
C      NPARM = 91
C      NPARM = 101
      NPARM = 97
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      DOTV = XDOTV.GT.0.0
C                                       declination in degrees
      ELEV = 90
C                                       total number of plots
      NPLTOT = BPARM(2)
      IF (NPLTOT.EQ.0) NPLTOT = 20
C                                       elevation
      EL = ELEV
      ELEV = ELEV * PI / 180.0
C                                       number of iteration
      NITER = APARM(1)
      IF (NITER.EQ.0) NITER = 1
      GRCHN = 1
C                                       number of plots at page
      NCOUNT = IROUND(XNCOUN)
      IF (NCOUNT.EQ.0) NCOUNT = 20
C                                       shift of the configuration
C                                       relatively BLC of the topography
      XSHIFT = BPARM(4)
      YSHIFT = BPARM(5)
C                                       rotation of the configuration
      ROT = BPARM(6) * DG2RAD
C                                       number of shift of configuration
C                                       in X and Y
      NX = BPARM(7)
      IF (NX.EQ.0) NX = 50
      NY = BPARM(8)
      IF (NY.EQ.0) NY = 50
C                                       step in rotation
      DROT = BPARM(10)
      IF (DROT.EQ.0.0) DROT = 5.0
C                                       number of rotation
      NROT = BPARM(9)
      IF (NROT.EQ.0) NROT = 360.0 / DROT
C                                       Parameters for the output file
C                                       in the UVCON format
      DOCONF = CPARM(1).LT.0.1
      LATIT = CPARM(2)
      LONGT = CPARM(3)
      ANTDIM = CPARM(4)
      IF (ANTDIM.LT.0.01) ANTDIM = 12
      ANTEF = CPARM(5)
      IF (ANTEF.LT.0.01) ANTEF = 0.5
      ANTSYS = CPARM(6)
      IF (ANTSYS.LT.0.01) ANTSYS = 50.0
      NUMLEV = CPARM(7) + 0.01
      IF (NUMLEV.EQ.0) NUMLEV = 2
C                                       Make the beam correction by the
C                                       primary beam?
      DOPB = CPARM(8).GT.0.5
C                                       altitude of the array center
C                                       above the geoid
      ALT = CPARM(9)
C                                       cize of cells at the topography
C                                       step in the shift
      CELLX = XCELL(1)
      IF (CELLX.LT.0.1) CELLX = 10
      CELLY = XCELL(2)
      IF (CELLY.LT.0.1) CELLY = 10
      XBLCC = XPARM(1)
      YBLCC = XPARM(2)
      XTRCC = XPARM(3)
      YTRCC = XPARM(4)
      IF (XTRCC.LT.0.1) XTRCC = 1.E8
      IF (YTRCC.LT.0.1) YTRCC = 1.E8
C                                       degree to (1/DIST) at the
C                                       dependence of the gain on the
C                                       distance
      ALPHA = XPARM(5)
      IF (ALPHA .EQ. 0) ALPHA = 1
C                                       dimension of the array in meters
      ARSIZE = APARM(7) * 1000
      IF (ARSIZE.EQ.0) ARSIZE = 1
C                                       warning about very wide field
C                                       of view
      MSGTXT = ' ----------------------------------------------'
      CALL MSGWRT (8)
      MSGTXT = '| The task uses the -SIN projection of the sky |'
      CALL MSGWRT (8)
      MSGTXT = '| So the value of one corresponds to  THETA=90 |'
      CALL MSGWRT (8)
      MSGTXT = '|   The following unequality should be valid   |'
      CALL MSGWRT (8)
      MSGTXT = '|        to exclude ABS(SIN(THETA)) > 1        |'
      CALL MSGWRT (8)
      MSGTXT = '|        APARM(4) * LAMBDA/APARM(7) <1         |'
      CALL MSGWRT (8)
      MSGTXT = '|   (LAMBDA and APARM(7) at the same units)    |'
      CALL MSGWRT (8)
      MSGTXT = ' ----------------------------------------------'
      CALL MSGWRT (8)
C


C                                       gain of the loop
      GAIN = APARM(2)
      IF (GAIN.EQ.0) GAIN = 0.001
C                                       number of fixed antennas
      NFIX = DPARM(2) + 0.1
C                                       constraint in topography
      TOPOGR = ((DPARM(3).EQ.1) .OR. (DPARM(3).EQ.4)) .OR.
     *   (DPARM(3).EQ.5)
C                                       constraint in DONUTS
      DONUTS = (DPARM(3).EQ.3) .OR. (DPARM(3).EQ.4)

C                                       constraint: inside of the
C                                       elliptic area
      ELLIPS = DPARM(3).EQ.6

C                                       constraint in two circles
      TWOCIR = (DPARM(3).EQ.2) .OR. (DPARM(3).EQ.5)
C                                       initial model three circles
      TRECIR = DPARM(1).EQ.3
C                                       If constrain are two circles
C                                       then initial configuration
C                                       should be two circles also
      IF (TWOCIR) DPARM(1) = 2
C                                       outer radius of DONUTS
      ROUTER = 0.5
C                                       inner radius of DONUTS
      RINNER = ABS(DPARM(8))
C                                       minor and major axis of the
C                                       outer ellipse
      IF (ELLIPS) THEN
         SMAJOR = 0.5
         SMINOR = DPARM(8)*SMAJOR
         END IF

C                                       shift the inner circle if
C                                       DPARM(8) is negative
      RSHIFT = 0
      IF (DPARM(8).LT.0) RSHIFT = ROUTER - RINNER
C                                       Minimum distance between
C                                       elements in meters.
      DLIMIT = DPARM(7)
C      IF (DLIMIT.EQ.0) DLIMIT = 12.5
C                                       number of additional elements
C                                       at the third cirle
      ADD1 = DPARM(10)
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XIFILE, INFILE)
      CALL H2CHR (48, 1, X2FILE, I2FILE)
      CALL H2CHR (48, 1, XOFILE, OFILE)
C                                       Is the I2FILE in FITS format?
      CALL FILFIT (I2FILE, DOFITS)
C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1020) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
   20 CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1040) IERR
         GO TO 990
   40 NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       open input file if its name is
C                                       not blank
      WREAD = .FALSE.
      IF (INFILE(1:1).NE.' ') WREAD = .TRUE.
      IF (WREAD) THEN
C                                       initial configuration is in the
C                                       file
         LUNPR = 10
         FA = F
         CALL ZTXOPN ('READ', LUNPR, PFIND, INFILE, FA, IRET)
         NLINE = 1
C                                       read the file data
   50    CALL ZTXIO ('READ', LUNPR, PFIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 60
         JT = JTRIM (LINE)
         READ (LINE,1060) XT, YT
         IF (DPARM(4).EQ.0) THEN
            X(NLINE) = XT
            Y(NLINE) = YT
         ELSE
            IF (APARM(8).GT.0.001) THEN
C                                       original configuration is not
C                                       shifted
               X(NLINE) = XT / ARSIZE
               Y(NLINE) = YT / ARSIZE
            ELSE
C                                       original configuration had been
C                                       shifted
               X(NLINE) = (XT - XSHIFT) / ARSIZE
               Y(NLINE) = (YT - YSHIFT) / ARSIZE
               END IF
            END IF
C
         NLINE = NLINE + 1
         GO TO 50
   60    CONTINUE
         NARRAY = NLINE - 1
         CALL ZTXCLS (LUNPR, PFIND, IRET)
      ELSE
C                                       initial configuration is
C                                       calculated
         NARRAY = APARM(3) + 0.1
         IF (NARRAY.EQ.0) NARRAY = 64
C
         IF (TWOCIR .OR. (DPARM(1).EQ.2)) THEN
C                                       two circles.
C                                       take # elements on each of two
C                                       circle proportional to its R
            NINNER = IROUND(NARRAY * RINNER / (RINNER + ROUTER)) -
     *         ADD1
            NOUTER = NARRAY - NINNER
            SINNER = TWOPI / NINNER
            SOUTER = TWOPI / NOUTER
C
            I = 0
            IF (I2FILE(1:1).NE.' ') THEN
C                                       find positions in outer circle
C                                       with some of them fixed
               IF (DPARM(5).GT.0.1) THEN
                  JERR = 1
                  WRITE (MSGTXT, 1050)
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               CALL OUTER (I2FILE, NOUTER, ARSIZE, DLIMIT, X, Y, I,
     *            NFIX, JERR)
               IF (JERR.LT.0) GO TO 990
            ELSE
               DO 70 IARRAY = 1, NOUTER
                  I = I + 1
                  ANGLE = SOUTER * (IARRAY - 1)
                  X(I) = ROUTER * COS(ANGLE)
                  Y(I) = ROUTER * SIN(ANGLE)
   70             CONTINUE
               END IF
C                                       inner circle
            DO 80 IARRAY = 1, NINNER
               I = I + 1
               ANGLE = SINNER * (IARRAY - 1)
C                                       shift the inner circle to the
C                                       left
               X(I) = RINNER * COS(ANGLE) - RSHIFT
               Y(I) = RINNER * SIN(ANGLE)
   80          CONTINUE
         ELSE

C                                       three circle
            IF (TRECIR) THEN
               ROUTER = 0.5
               RINTER = RINNER
               RINEST = DPARM(9)
               RSUM = ROUTER + RINTER + RINEST
C                                       take N elements on each of two
C                                       circle proportional to its R
C
               NOUTER = IROUND(NARRAY * ROUTER / RSUM) + ADD1
               NINTER = IROUND(NARRAY * RINTER / RSUM) - ADD1 - 1
               NINEST = NARRAY - NOUTER - NINTER - 1
               S1 = TWOPI / NOUTER
               S2 = TWOPI / NINTER
               S3 = TWOPI / NINEST
               I = 0
C                                       outer circle
               IF (I2FILE(1:1).NE.' ') THEN
C                                       find positions in outer circle
C                                       with some of them fixed
                  IF (DPARM(5).GT.0.1) THEN
                     JERR = 1
                     WRITE (MSGTXT, 1050)
                     CALL MSGWRT (8)
                     GO TO 999
                     END IF
                  CALL OUTER (I2FILE, NOUTER, ARSIZE, DLIMIT, X, Y, I,
     *               NFIX, JERR)
                  IF (JERR.LT.0) GO TO 990
               ELSE
                  DO 180 IARRAY = 1, NOUTER
                     I = I + 1
                     ANGLE = S1 * (IARRAY - 1)
                     X(I) = ROUTER * COS(ANGLE)
                     Y(I) = ROUTER * SIN(ANGLE)
  180                CONTINUE
                  END IF
C                                       intermediate circle
               DO 220 IARRAY = 1, NINTER
                  I = I + 1
                  ANGLE = S2 * (IARRAY - 1)
                  X(I) = RINTER * COS(ANGLE)
                  Y(I) = RINTER * SIN(ANGLE)
  220             CONTINUE
C                                       inner circle
               DO 230 IARRAY = 1, NINEST
                  I = I + 1
                  ANGLE = S3 * (IARRAY - 1) + PI/6
                  X(I) = RINEST * COS(ANGLE)
                  Y(I) = RINEST * SIN(ANGLE)
  230             CONTINUE
C                                       add the element at zero
               X(I + 1) = 0.0
               Y(I + 1) = 0.0
C                                       end three circle
            ELSE
C                                       hexagon tiling
               IF (DPARM(1).EQ.4) THEN
C                                       The number of antennas at the
C                                       array should be equal 7,19,37
C                                       61..= 3*NC^2 + 3*NC + 1
C                                       NC = 1,2,3...
                  DO 234 I = 1, 20
                     NTOT = 3*(I*I + I) + 1
                     IF (NTOT.EQ.NARRAY) THEN
                        NCEN = I
                        GO TO 235
                        END IF
  234                CONTINUE
                  JERR = 1
                  WRITE (MSGTXT, 1051) NARRAY
                  CALL MSGWRT (8)
                  WRITE (MSGTXT, 1052)
                  CALL MSGWRT (8)
                  GO TO 999
C
  235             CONTINUE
C                                       number of rows at the upper
C                                       part not counting the central
                  NROW = NCEN
                  DELTAX = 1.0 / (2.0*NCEN)
                  DELTAY = DELTAX * SQRT(3.0) / 2
                  NXX = 2*NCEN + 2
                  YYY = -DELTAY
                  X0 = -DELTAX * NCEN - DELTAX/2
                  IND = 0
                  DO 238 K = 1, NROW+1
                     NXX = NXX -1
                     X0 = X0 + DELTAX/2
                     YYY = YYY + DELTAY
                     XXX = X0 - DELTAX
                     DO 236 I = 1, NXX
                        XXX = XXX + DELTAX
                        IND = IND + 1
                        Y(IND) = XXX
                        X(IND) = YYY
                        IF (K.NE.1) THEN
                           IND = IND + 1
                           Y(IND) = XXX
                           X(IND) = -YYY
                           END IF
  236                   CONTINUE
  238                CONTINUE
C
               ELSE
C                                       one gomogenios circle
                  STEPAN = TWOPI / NARRAY
                  DO 240 IARRAY = 1, NARRAY
                     ANGLE = STEPAN * (IARRAY - 1)
                     X(IARRAY) = 0.5 * COS(ANGLE)
                     Y(IARRAY) = 0.5 * SIN(ANGLE)
  240                CONTINUE
                  END IF
               END IF
            END IF
         END IF
C                                       Start calculating the beam
      NXTETA = APARM(4)
      IF (NXTETA.EQ.0) NXTETA = 20
      NYTETA = NXTETA
C                                       NXTETA, NYTETA - number of main
C                                       beams in half field in X and Y
C                                       direction
C                                       DTETA step in the beam
      DTETA = APARM(5)
      IF (DTETA.EQ.0) DTETA = 0.2
      NXTETA = NXTETA / DTETA
      NYTETA = NYTETA / DTETA
C                                       RADMAX is square of the circle
C                                       radius fitted to the squre
C                                       Square, becase it will compare
C                                       with square
      RADMAX = MAX (NXTETA *NXTETA, NYTETA *NYTETA)
C                                       Minimum radius of the
C                                       optimization area at the sky
      NXMIN = APARM(9) / DTETA
C                                       1.2 correspond to the main lobe
C                                       edge
      IF (NXMIN .EQ. 0) NXMIN = 1.2 / DTETA
      RADMIN = NXMIN * NXMIN
C                                       Half sector of optimization
      SECTOR = ABS(APARM(10)) * DG2RAD / 2
      IF (SECTOR.LT.0.001) SECTOR = PI
C
      NTOTX = 2 * NXTETA  + 1
      NTOTY = 2 * NYTETA  + 1
      DTETA = DTETA

C                                       fit the configuration with the
C                                       topography data - mask file if
C                                       its name is not blank
      IF (DPARM(5).EQ.0) THEN
C                                       Do not provide topography fit
         XSHFOU = 0
         YSHFOU = 0
         ROTFOU = 0
      ELSE
C                                       fource NX, NY, NROT to 1
C                                       if fitting to topography is
C                                       together with optimization
         IF (DPARM(5) .EQ. 2) THEN
            NX =1
            NY = 1
            NROT = 1
            END IF
         IF (I2FILE(1:1).NE.' ') THEN
            CALL MASK (NX, NY, NROT, CELLX, CELLY, DROT,
     *         XSHIFT, YSHIFT, ROT, ARSIZE, X, Y, XMASK, YMASK ,
     *         XSHFOU, YSHFOU, ROTFOU, NBADMN, XFOBAD, YFOBAD,
     *         IRET)
            IF (IRET .NE. 0) THEN
               JERR = 1
               GO TO 999
               END IF
C                                       Go to record configuration in
C                                       meters and that is all
            IF (DPARM(5).EQ.1) GO TO 820
            END IF
         END IF
C
      ITER = 1
      MINZEN = 10.0
C                                       Cycle by iterations
  250 CONTINUE
C
      IF ((TWOCIR.OR.DONUTS.OR.ELLIPS) .OR. (I2FILE(1:1).NE.' ')
     *   .OR. (NFIX.GT.0) .OR. (NITER.EQ.1)) THEN
C      IF ((TWOCIR .OR. TRECIR) .OR. (I2FILE(1:1).NE.' ') .OR.
C     *   (NFIX.GT.0) .OR. (NITER.EQ.1)) THEN
C                                       Normalize in the topography
C                                       or not. This is the question
C      IF ((TWOCIR .OR. TRECIR) .OR. (NITER.EQ.1))  THEN
C                                       The elements are on the outer
C                                       cirlce of diameter 1. So  no
C                                       need of normalization.
         RMAX = 1
C                                       check the minimum spacing
         RMIN = 1.0E10
C                                       normalize UV coverage in all
C                                       other cases
         DO 260 I = 1, NARRAY
            DO 255 K = 1, NARRAY
               IUV = IUV + 1
               XT = X(K) - X(I)
               YT = Y(K) - Y(I)
               RTEM = (XT * XT + YT *YT)
               IF ((K.NE.I) .AND. (RTEM.LT.RMIN)) THEN
                  RMIN = RTEM
                  IMIN = I
                  KMIN = K
                  END IF
  255       CONTINUE
  260       CONTINUE
C
         RMIN = SQRT(RMIN)
         RMINAR = RMIN * ARSIZE
         IF ((DLIMIT-RMINAR).GT.EPS .AND. ARSIZE.GT.1.01) THEN
            WRITE (MSGTXT, 1055) IMIN, KMIN, RMINAR, DLIMIT
            JERR = 1
            GO TO 990
            END IF
      ELSE
         RMAX = 0
C                                       normalize UV coverage in all
C                                       other cases
         DO 280 I = 1, NARRAY
            DO 270 K = 1, NARRAY
               IUV = IUV + 1
               XT = X(K) - X(I)
               YT = Y(K) - Y(I)
               RTEM = (XT * XT + YT *YT)
               RMAX = MAX(RMAX, RTEM)
  270          CONTINUE
  280       CONTINUE
         RMAX = SQRT(RMAX)
         END IF
C                                       normalise by RMAX
      DO 320 IARRAY = 1, NARRAY
         X(IARRAY) = X(IARRAY) / RMAX
         Y(IARRAY) = Y(IARRAY) / RMAX
  320    CONTINUE
C                                       Lets calculate beam at zenith
      DO 360 IXTETA = 1, NXTETA  + 1
         XTETA = (IXTETA - NXTETA - 1) * DTETA
         DO 350 IYTETA = 1, NTOTY
            IXY = IYTETA + (IXTETA - 1) * NTOTY
            YTETA = (IYTETA - NYTETA - 1) * DTETA
            REU = 0.0
            IMU = 0.0
            DO 340 IARRAY = 1, NARRAY
               ARG = TWOPI*(X(IARRAY) * XTETA + Y(IARRAY) * YTETA)
               REU = REU + COS(ARG)
               IMU = IMU + SIN(ARG)
  340          CONTINUE
            BEAZEN(IXY) = (REU*REU + IMU*IMU) / NARRAY / NARRAY
C                                       multiply by the primary beam
C                                       take the dish of diameter 'd'
C                                       iluminated nomogeniously
            IF (DOPB) THEN
               ARGPB = PI*SQRT(XTETA*XTETA+YTETA*YTETA)*ANTDIM/ARSIZE
               IF (ARGPB.EQ.0) THEN
                  PB = 1
               ELSE
                  BESS1 = BESSJ1(ARGPB)
                  PB = (2*BESS1/ARGPB)**2
                  END IF
               BEAZEN(IXY) = BEAZEN(IXY) * PB
               END IF
  350       CONTINUE
  360    CONTINUE
      SECZEN = 0
C
C                                       find the beam maximum outside
C                                       the RADMIN.
C                                       RADMIN should exclude the main
C                                       lobe
      IF (RADMIN .LT. 0.1) THEN
         JERR = 1
         WRITE (MSGTXT,2410)
         CALL MSGWRT (8)
         WRITE (MSGTXT,2420)
         GO TO 990
         END IF
C
      DO 380 IXTETA = 1, NXTETA
         DO 370 IYTETA = 1, NTOTY
            HALFSE = ATAN(ABS(IYTETA - NYTETA - 1 + 0.001)
     *         / ABS(IXTETA - NXTETA - 1 + 0.001))
            DOSECT = HALFSE.LT.SECTOR
            RADIUS = (IXTETA - NXTETA - 1) * (IXTETA - NXTETA - 1) +
     *         (IYTETA - NYTETA - 1) * (IYTETA - NYTETA - 1)
            IXY = IYTETA + (IXTETA - 1) * NTOTY
            IF ((RADIUS.LT.RADMAX) .AND.
     *         (RADIUS.GT.RADMIN) .AND. DOSECT)  THEN
C                                       find the biggest maximum
C                                       outside RADMIN
               MAXVAL = BEAZEN(IXY)
C
               IF (MAXVAL .GT. SECZEN) THEN
                  SECZEN = MAXVAL
                  WZENX = (IXTETA - NXTETA - 1) * DTETA
                  WZENY = (IYTETA - NYTETA - 1) * DTETA
                  END IF
               END IF
  370       CONTINUE
  380     CONTINUE
C                                       Location of the worst sidelobe
C                                       for zenith is found
      WRITE (MSGTXT,2450) ITER, SECZEN, WZENX, WZENY
      CALL MSGWRT (8)
C                                       Find the gain depending on the
C                                       distance of the maximum of the
C                                       center
      IF (APARM(2) .LT. -0.00001) THEN
         DIST = SQRT (WZENX*WZENX + WZENY*WZENY)
         GAIN = ABS(APARM(2)) * ((1 / DIST) ** ALPHA)
         END IF

C                                       store the iteration with the
C                                       smallest worst sidelobe SECZEN
      IF (SECZEN.LT.MINZEN) THEN
         MINZEN = SECZEN
         MITER = ITER
         MZENX = WZENX
         MZENY = WZENY
         DO 480 IARRAY = 1, NARRAY
C                                       store the best solution
            XBEST(IARRAY) = X(IARRAY)
            YBEST(IARRAY) = Y(IARRAY)
  480       CONTINUE
         END IF
      IF (ITER.NE.1 .AND. ITER.NE.NITER) GO TO 520
      SIZEX = 1000.0
      SIZEY = 1000.0 / NCOUNT
      GMXX = -1.0E20
      GMNX = 1.0E20
      NPLOTS = MIN(NXTETA + 1, NPLTOT)
C
      DO 510 IXTETA = 1, NPLOTS
         KXTETA = NXTETA + 2 - IXTETA
         XTETA = (KXTETA - NXTETA - 1) * DTETA
C
C                                       Prepare parameters for PLTEL
         IPLT = IXTETA
         IPLOT = IPLT - 1
         IPLOT = MOD (IPLOT, NCOUNT) + 1
         IF (IPLT.EQ.NPLOTS) IPLOT = -IPLOT
C                                       find max/min of the Y
         FUNCMX = -1.0E10
         FUNCMN =  1.0E10
         ARGMX = -1.0E10
         ARGMN =  1.0E10
         DO 490 IYTETA = 1, NTOTY
            IXY = IYTETA + (KXTETA - 1) * NTOTY
            YTETA = (IYTETA - NYTETA - 1) * DTETA
            RDIRR(IYTETA) = YTETA
C                                       Take beam for zenith
C                                       to plot it
            BEAM(IYTETA) = BEAZEN(IXY)
            IF (BPARM(3).EQ.0) THEN
               FUNCMX = MAX(FUNCMX, BEAM(IYTETA))
               FUNCMN = MIN(FUNCMN, BEAM(IYTETA))
            ELSE
               FUNCMX = BPARM(3)
               FUNCMN = 0
               END IF
            ARGMX = MAX(ARGMX, YTETA)
            ARGMN = MIN(ARGMN, YTETA)
  490       CONTINUE
         TOLER = 0.01
C
C                                       TMAX, TMIN new MAX&MIN of Y
         TMAX = FUNCMX + 0.1 * (FUNCMX - FUNCMN)
         TMIN = FUNCMN - 0.1 * (FUNCMX - FUNCMN)
         IF (ABS (TMAX-TMIN).LT.TOLER) THEN
            TMAX = TMAX + TOLER
            TMIN = TMIN - TOLER
            END IF
C                                       GMXX, GMNX MAX&MIN of Y for all
C                                       plots
         GMXX = MAX (GMXX, TMAX)
         GMNX = MIN (GMNX, TMIN)
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
         GXYOFF(2,IPLT) = TMIN
         GXYSCL(2,IPLT) = SIZEY / TDIF
C                                       now about X-axis
         TMAX = (ARGMX + 0.1 * (ARGMX - ARGMN))
         TMIN = (ARGMN - 0.1 * (ARGMX - ARGMN))
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.0.01) TDIF = 0.01
         GXYOFF(1,IPLT) = TMIN
         GXYSCL(1,IPLT) = SIZEX / TDIF
C                                       plot beam
         XYSCL(1) = GXYSCL(1,IPLT)
         XYSCL(2) = GXYSCL(2,IPLT)
         XYOFF(1) = GXYOFF(1,IPLT)
         XYOFF(2) = GXYOFF(2,IPLT)
         CALL PLTEL (IPLOT, NTOTY, NTOTY, NTOTY, XTETA, NCOUNT, EL,
     *      SECZEN, WZENY, WZENX, INFILE, RDIRR, BEAM, 1, ITER, IRET)
  510    CONTINUE
  520 CONTINUE
C                                       Plot the original and final
C                                       UV coverage of the array
C                                       together with configuration
C                                       Prepare parameters for PLTEL
      IF (ITER.NE.1 .AND. ITER.NE.NITER) GO TO 600
         IF (APARM(6).NE.0) GO TO 600
         NPLOTC = 1
         NCOUNC = 1
         IPLTC = 1
         IPLOTC = IPLTC - 1
         IPLOTC = MOD (IPLOTC, NCOUNC) + 1
         IF (IPLTC.EQ.NPLOTC) IPLOTC = -IPLOTC
         SIZEX = 1000.0
         SIZEY = 1000.0 / NCOUNC
         GMXX = -1.0E20
         GMNX = 1.0E20
C                                       find max/min of the Y
         FUNCMX = -1.0E10
         FUNCMN =  1.0E10
         ARGMX = -1.0E10
         ARGMN =  1.0E10
C                                       find UV coverage
C                                       Do not store auto convolution
C                                       if NARRAY.GT.100
C
         IF (NARRAY.LE.100) THEN
            NUVC = NARRAY * NARRAY
         ELSE
            NUVC = 0
            END IF
         IUV = 0
         DO 550 I = 1, NARRAY
            XX(I + NUVC) = X(I)
            YY(I + NUVC) = Y(I)
         IF (NARRAY.GT.100) GO TO 550
            DO 540 K = 1, NARRAY
               IUV = IUV + 1
               XX(IUV) = X(K) - X(I)
               YY(IUV) = (Y(K) - Y(I)) * SIN(ELEV)
  540          CONTINUE
  550       CONTINUE
         NUV = NUVC + NARRAY
         NUVTOT = NUV
C                                       Plot the circumferences
C                                       for donuts or two circle
         IF (DONUTS .OR. TWOCIR) THEN
C
            DO 560 I = 1, 200
               ARG = TWOPI * (I - 1) / 200.0
               XX(I + NUV) = RINNER * COS(ARG) - RSHIFT
               YY(I + NUV) = RINNER * SIN(ARG)
               XX(I + NUV + 200) = ROUTER * COS(ARG)
               YY(I + NUV + 200) = ROUTER * SIN(ARG)
  560          CONTINUE
            NUVTOT = NUV + 400
            END IF
C                                       Plot the ellipse outer border
C                                       Minor ases is horisontal
         IF (ELLIPS) THEN
C
            DO 565 I = 1, 200
               ARG = TWOPI * (I - 1) / 200.0
               XX(I + NUV) = SMINOR * COS(ARG)
               YY(I + NUV) = SMAJOR * SIN(ARG)
  565       CONTINUE
            NUVTOT = NUV + 200
            END IF
C
         DO 570 I = 1, NUV
            FUNCMX = MAX(FUNCMX, YY(I))
            FUNCMN = MIN(FUNCMN, YY(I))
            ARGMX = MAX(ARGMX, XX(I))
            ARGMN = MIN(ARGMN, XX(I))
  570       CONTINUE
C                                       fix the sizes to -1, 1
         FUNCMX = 1.0
         FUNCMN = -1.0
         ARGMX = 1.0
         ARGMN = -1.0
         TOLER = 0.01
C
C                                       TMAX, TMIN new MAX&MIN of Y
         TMAX = FUNCMX + 0.1 * (FUNCMX - FUNCMN)
         TMIN = FUNCMN - 0.1 * (FUNCMX - FUNCMN)
         IF (ABS (TMAX-TMIN).LT.TOLER) THEN
            TMAX = TMAX + TOLER
            TMIN = TMIN - TOLER
            END IF
C                                       GMXX, GMNX MAX&MIN of Y for all
C                                       plots
         GMXX = MAX (GMXX, TMAX)
         GMNX = MIN (GMNX, TMIN)
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
         GXYOFF(2,IPLTC) = TMIN
         GXYSCL(2,IPLTC) = SIZEY / TDIF
C                                       now about X-axis
         TMAX = (ARGMX + 0.1 * (ARGMX - ARGMN))
         TMIN = (ARGMN - 0.1 * (ARGMX - ARGMN))
         TDIF = TMAX - TMIN
         IF (ABS (TDIF).LE.0.01) TDIF = 0.01
         GXYOFF(1,IPLTC) = TMIN
         GXYSCL(1,IPLTC) = SIZEX / TDIF
C                                       plot Y/ARG and model
         XYSCL(1) = GXYSCL(1,IPLTC)
         XYSCL(2) = GXYSCL(2,IPLTC)
         XYOFF(1) = GXYOFF(1,IPLTC)
         XYOFF(2) = GXYOFF(2,IPLTC)
         CALL PLTEL (IPLOTC, NUVTOT, NUVC, NUV, XTETA, NCOUNC, EL,
     *      SECZEN, WZENY, WZENX, INFILE, XX, YY, 2, ITER, IRET)
C
  600    CONTINUE

C                                       estimate optimum of the
C                                       configuration, minimizing beam
C                                       at the worst direction WZENX,
C                                       WZENY of zenith beam
      IF (ITER.EQ.NITER) GO TO 800
C                                       do not correct the last
C                                       itteration
      DO 740 IARRAY = 1, NARRAY
         ANUMER = 0
         DO 720 KARRAY = 1, NARRAY
            ANUMER = ANUMER + SIN( TWOPI * (WZENX *
     *         (X(KARRAY) - X(IARRAY))  +  WZENY *
     *         (Y(KARRAY) - Y(IARRAY)) ))
  720       CONTINUE
         DPPX = 2 * TWOPI * WZENX * ANUMER / (NARRAY * NARRAY)
         DPPY = 2 * TWOPI * WZENY * ANUMER / (NARRAY * NARRAY)
         DPP = SQRT (DPPX*DPPX + DPPY*DPPY)
C
         DX(IARRAY) = - GAIN * DPPX
         DY(IARRAY) = - GAIN * DPPY
  740    CONTINUE
C                                       apply the found corrections
      DO 780 IARRAY = 1, NARRAY
C                                       do not move the fixed elements
C                                       of the outer circle
C         IF ((TWOCIR .OR. TRECIR) .AND. (IARRAY.LE.NFIX)) THEN
C         IF ((TWOCIR .OR. TRECIR) .OR. (IARRAY.LE.NFIX)) THEN
         IF (IARRAY.LE.NFIX) THEN
            XTEM = X(IARRAY)
            YTEM = Y(IARRAY)
         ELSE
            XTEM = X(IARRAY) + DX(IARRAY)
            YTEM = Y(IARRAY) + DY(IARRAY)
            END IF
C                                       RT is the distance of the given
C                                       point to the center of the
C                                       outer circle
         RT = SQRT (XTEM * XTEM + YTEM * YTEM)
C                                       RTIN is the distance of the
C                                       given point to the center
C                                       of the inner circle (shifted)
         RTIN = SQRT ((XTEM+RSHIFT) * (XTEM+RSHIFT) + YTEM * YTEM)
C                                       Constraint of two circles
         IF (TWOCIR) THEN
            IF (ABS(RTIN - RINNER).LT.ABS(RT - ROUTER)) THEN
C                                       scale to the inner circle
               SCALE = RINNER / RT
            ELSE
C                                       scale to the outer circle
               SCALE = ROUTER / RT
               END IF
            XTEM = XTEM * SCALE
            YTEM = YTEM * SCALE
            END IF
C                                       Scale the outer circle in
C                                       the case of three circles
         IF (TRECIR .AND. (IARRAY.LE.NOUTER)) THEN
            SCALE = ROUTER / RT
            XTEM = XTEM * SCALE
            YTEM = YTEM * SCALE
            END IF
C                                       convert normalized positions
C                                       to positions in meters
         XT = XTEM * ARSIZE + XSHFOU
         YT = YTEM * ARSIZE + YSHFOU

C
         IF (TOPOGR) THEN
C                                       Is the corrected antenna
C                                       outside of the topography mask?
            OUSIDE = ((XT.GT.XMAX) .OR. (XT.LT.XMIN))
     *         .OR. ((YT.GT.YMAX) .OR. (YT.LT.YMIN))
            IF (OUSIDE) GO TO 780
C                                       check topography
            DO 760 KBAD = 1, NBAD
               BAD = (ABS (XT - XBAD(KBAD)).LE.0.55*CELLX)
     *            .AND. (ABS (YT - YBAD(KBAD)).LE.0.55*CELLY)
               IF (BAD) GO TO 780
  760          CONTINUE
            END IF
         IF (DONUTS) THEN
C                                       check donuts
            BAD = (RT.GT.ROUTER) .OR. (RT.LT.RINNER)
            IF (BAD) GO TO 780
            END IF


         IF (ELLIPS) THEN
C                                       direction to point XTEM,YTEM
            ARG = ATAN2 (YTEM, XTEM)
C                                       coordinate of the ellips' point
C                                       at the direction XTEM, YTEM
            XELIPS = SMINOR * COS(ARG)
            YELIPS = SMAJOR * SIN(ARG)
C                                       distance to the ellips' point
            RELIPS = SQRT(XELIPS*XELIPS + YELIPS*YELIPS)
            BAD = RT .GT. RELIPS
            IF (BAD) THEN
               GO TO 780
               END IF
            END IF


C                                       Check minimum distance or not?
         IF (DLIMIT.EQ.0) GO TO 770
C                                       Is the given element too close
C                                       to any other element?
         DO 765 KARRAY = 1, NARRAY
            IF (KARRAY.EQ.IARRAY) GO TO 765
            XTEM1 = X(KARRAY)
            YTEM1 = Y(KARRAY)
            XT1 = XTEM1 * ARSIZE + XSHFOU
            YT1 = YTEM1 * ARSIZE + YSHFOU
            NEWDIS = (XT1 - XT)*(XT1 - XT) + (YT1 - YT)*(YT1 - YT)
            IF ((DLIMIT*DLIMIT-NEWDIS) .GT. EPS) GO TO 780
  765    CONTINUE
C
  770    X(IARRAY) = XTEM
         Y(IARRAY) = YTEM
C
  780    CONTINUE
C
  800 CONTINUE
C                                       Lets calculate beam at zenith
C                                       at the found worth direction
C                                       after the correction
      REU = 0.0
      IMU = 0.0
      DO 810 IARRAY = 1, NARRAY
         ARG = TWOPI*(X(IARRAY) * WZENX + Y(IARRAY) * WZENY)
         REU = REU + COS(ARG)
         IMU = IMU + SIN(ARG)
  810    CONTINUE
      SECZEN = (REU*REU + IMU*IMU) / NARRAY / NARRAY
C                                       multiply by the primary beam
C                                       take the dish of diameter 'd'
C                                       iluminated nomogeniously
      IF (DOPB) THEN
         ARGPB = PI*SQRT(WZENX*WZENX+WZENY*WZENY)*ANTDIM/ARSIZE
               IF (ARGPB.EQ.0) THEN
                  PB = 1
               ELSE
                  BESS1 = BESSJ1(ARGPB)
                  PB = (2*BESS1/ARGPB)**2
                  END IF
               SECZEN = SECZEN * PB
               END IF
C
      WRITE (MSGTXT,2450) ITER, SECZEN, WZENX, WZENY
      CALL MSGWRT (8)
C
      ITER = ITER + 1
      IF (ITER.LE.NITER) GO TO 250
C
      WRITE (MSGTXT,2470) MITER, MINZEN, MZENX, MZENY
      CALL MSGWRT (8)
C                                       record found configuration to
C                                       the output file
  820 IF (OFILE(1:1).NE.' ') THEN
         LUNPR = 10
         FA = F
         CALL ZTXOPN ('WRIT', LUNPR, PFIND, OFILE, FA, IRET)
C
         IF (I2FILE(1:1).NE.' ' .AND. DPARM(5).EQ.1) THEN
C
            IF (NBADMN.EQ.0) THEN
               WRITE (LINE, 1440)
               GO TO 840
            ELSE
               WRITE (LINE, 1450) NBADMN
               END IF
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            DO 830 KBAD = 1, NBADMN
               WRITE (LINE, 1500) KBAD,  XFOBAD(KBAD),
     *            YFOBAD(KBAD)
               NCH = JTRIM(LINE)
               CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
  830          CONTINUE
            END IF
C
         IF (.NOT. DOCONF) THEN
            WRITE (LINE, 1510) NARRAY
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Latitude, Longtitude
            WRITE (LINE, 1515) LATIT, LONGT, ALT
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
C                                       Conversion factors
            CONV1 = 1
            CONV2 = 1
            WRITE (LINE, 1520) CONV1, CONV2
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
            END IF
C
  840    DO 860 IARRAY = 1, NARRAY
            IF (DPARM(6).NE.0) THEN
C                                       write output file data in meters
C                                       as output of fit to topography
               IF (I2FILE(1:1).NE.' ' .AND. DPARM(5).EQ.1) THEN
C                                       Take output of the topography
C                                       fit if only that fit was done
                  XT = XMASK (IARRAY)
                  YT = YMASK (IARRAY)
               ELSE
C
                  XT = XBEST(IARRAY) * ARSIZE + XSHFOU
                  YT = YBEST(IARRAY) * ARSIZE + YSHFOU
                  END IF
               WRITE (MSGTXT, 1065) XT, YT
               CALL MSGWRT (8)
               IF (DOCONF) THEN
                  WRITE (LINE, 1065) XT, YT
               ELSE
                  INCOOR = 1
                  ZT = 0
                  WRITE (LINE, 1525) INCOOR, XT, YT, ZT, ANTDIM, ANTEF,
     *               ANTSYS, NUMLEV
                  END IF
            ELSE
               WRITE (MSGTXT,2500) IARRAY, XBEST(IARRAY), YBEST(IARRAY)
               CALL MSGWRT (8)
C                                       write output file data
               WRITE (LINE, 1060) XBEST(IARRAY), YBEST(IARRAY)
               END IF
            NCH = JTRIM(LINE)
            CALL ZTXIO ('WRIT', LUNPR, PFIND, LINE(1:NCH), IRET)
  860       CONTINUE
         CALL ZTXCLS (LUNPR, PFIND, IRET)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OPTI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('Put DPARM(5)=0 to use IN2FILE only for fixed antennas')
 1051 FORMAT ('You want the hexagonal configuration with NARRAY = ',
     *   I5)
 1052 FORMAT ('NARRAY should be equal 7,19,37..= 3*NC^2 + 3*NC + 1 ',
     *   'NC = 1,2,3...')
 1055 FORMAT ('Spacing between', I4, ' and', I4, ' ants', F10.6,
     *   '<allowed ', F10.6)
 1060 FORMAT (2F20.9)
 1065 FORMAT (2F20.4)
 1440 FORMAT ('All elements are fitted topography')
 1450 FORMAT (I3, ' elements are not fitted')
 1500 FORMAT (I6, ' XFIT= ', F7.1, ' YFIT= ', F7.1)
 1510 FORMAT (I4)
 1515 FORMAT (2F10.5, F10.1)
 1520 FORMAT (2I2)
 1525 FORMAT (I1, 2F20.4, I2, 1X,  F6.1, F5.2, F5.1, 1X, I1)
 2410 FORMAT ('You decided to find beam maximum as maximum outside ')
 2420 FORMAT ('of the circle of RADMIN. RADMIN can not be zero!!!')
 2450 FORMAT (I5, ' The worst at zenith equal ',F7.5,
     *   ' X = ', F6.2, ' Y = ', F6.2)
 2470 FORMAT (I5, ' The minimum worst equals ',F7.5,
     *   ' X = ', F6.2, ' Y = ', F6.2)
 2500 FORMAT (I3, 2F15.7)
      END

      SUBROUTINE EDGES (BEAM, NX, NY, XLEFT, XRIGHT, YLEFT, YRIGHT)
C-----------------------------------------------------------------------
C   EDGES fins the four edges of the main lobe of the beam
C   Input:
C      BEAM    R(*) The beam
C      NX      I    The half points at the X direction. The total
C                   number of the points at the X direction is 2*NX+1
C      NY      I    The half points at the Y direction. The total
C                   number of the points at the Y direction is 2*NY+1
C   Output:
C      XLEFT   I    The left point at X direction where main lobe ends
C      XRIGHT  I    The right point at X direction where main lobe ends
C      YLEFT   I    The left point at Y direction where main lobe ends
C      YRIGHT  I    The right point at Y direction where main lobe ends
C-----------------------------------------------------------------------
      REAL BEAM(*)
      INTEGER NX, NY, XLEFT, XRIGHT, YLEFT, YRIGHT, NTOTX, NTOTY,
     *   IX, IY, IXY1, IXY2
C----------------------------------------------------------------------
      NTOTX = 2 * NX  + 1
      NTOTY = 2 * NY  + 1
C                                       take the right edge at the beam
C                                       maximum, because later I
C                                       analyze only left side of the
C                                       beam using its symmetry
C
      XRIGHT = NX + 1
C                                       find left edge of the beam
C                                       at the X direction
      IX = NX + 1
  140 IXY1 = NY + 1 + (IX - 1) * NTOTY
         XLEFT = IX - 1
         IXY2 = NY + 1 + (XLEFT - 1) * NTOTY
         IF (BEAM(IXY2).GT.BEAM(IXY1)) GO TO 150
C
C                                       XLEFT the first point from
C                                       main maximum at X direction
C                                       to the left
C                                       where Beam start increasing
         IX = IX - 1
         GO TO 140
  150 CONTINUE
C                                       find right edge of the beam
C                                       at the Y direction
       IY = NY + 1
  160  IXY1 = IY + (NX + 1 - 1) * NTOTY
       YRIGHT = IY + 1
       IXY2 = YRIGHT + (NX + 1 - 1) * NTOTY
       IF (BEAM(IXY2).GT.BEAM(IXY1)) GO TO 170
C
C                                       YRIGHT the first point from
C                                       main maximum at Y direction
C                                       to the right
C                                       where Beam start increasing
       IY = IY + 1
       GO TO 160
  170 CONTINUE
C                                       find left edge of the beam
C                                       at the Y direction
       IY = NY + 1
  180  IXY1 = IY + (NX + 1 - 1) * NTOTY
       YLEFT = IY - 1
       IXY2 = YLEFT + (NX + 1 - 1) * NTOTY
       IF (BEAM(IXY2).GT.BEAM(IXY1)) GO TO 190
C
C                                       YLEFT the first point from
C                                       main maximum at Y direction
C                                       to the left
C                                       where Beam start increasing
       IY = IY - 1
       GO TO 180
 190  CONTINUE
 999  RETURN
      END
      SUBROUTINE PLTEL (IPLOT, ITIM, ITIMC, NUV, XTETA, NCOUNT, ELEV,
     *   SECM, WORSTY, WORSTX, KNFILE, X, Y, ICODE, ITER, IRET)
C-----------------------------------------------------------------------
C   PLTEL actually plots data and model.
C   Input:
C      IPLOT   I    Plot number on current page. If neg. then this is
C                   last plot.
C      ITIM    I    Total number of points at arrays X and Y
C      ITIMC   I    Number of UV points
C      NUV     I    Number of UV points plus number of antennas
C      XTETA   R    X position at the beam
C      NCOUNT  I    Number of plots at the page
C      ELEV    R    Elevation of the source in degrees
C      SECM    R    Maximum sidelobe's value
C      WORSTY  R    Y-Distance of the worst sidelobe
C      WORSTX  R    X-Distance of the worst sidelobe
C      IF (ICODE.EQ.3) THEN
C      SECM    R    XSHIFT in meters
C      WORSTY  R    YSHIFT in meters
C      WORSTX  R    Rotation in degrees
C      KNFILE  C    Infile
C      X       R(*) Array of data arguments
C      Y       R(*) Array of data function
C      ICODE   I    1 => beam partern; 2 => array configuration
C      ITER    I    Iteration number; used only for label
C   Inputs from Common:
C      GMNX    R    Max. value to plot
C      GMXX    R    Min. value to plot
C      XMX     R    Max. x value to plot
C      XMN     R    Min. x value to plot
C   Output:
C      IRET    I    Return code, 0 => OK, otherwise abort.
C                     -1 => user request termination
C                      1 => failed to add to catalog
C                      2 => failed to create
C                      3 => graph file write error
C                      4 => UV file IO error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, ITIM, ITIMC, NCOUNT, ICODE , ITER, IRET
      REAL   X(*), Y(*), ELEV, SECM, WORSTY, WORSTX, XTETA
      CHARACTER KNFILE*48
C
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, AUNITS(28)*8,
     *   XUNITS(3)*20
      INTEGER   POLBUF(256), VER, IERR, ITYPE, IPSIZE, LUNPL, LTYPE,
     *   FINDPL, DEPTH(5), INCHAR, INP, IT(3), ID(3), IAXLAB, IAPLOT,
     *   I, NGOOD, NNOFIT, LABEL, ITT, EL, ALIN, SHIFTX, SHIFTY,
     *   NUV, ROTA
      REAL      BLC(2), TRC(2), XYRATO, DX, DY, TR, VALUE, TI,
     *   XY(2), XTRC(2), XBLC(2), TLC(2), PLTINC, YYOFF(2), SIZE,
     *   XMULT(2), XVARIB, YPT, DBY, DXC, DYC
      LOGICAL   T, F, GOOD, CATUP, BLPLT, WREAD
      SAVE LABEL, LTYPE, POLBUF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CONFI.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA LUNPL /26/
      DATA DEPTH /5*1/
      DATA T, F /.TRUE.,.FALSE./
      DATA AUNITS /'Power', 'Y-unit', 'meters', 25*' '/
      DATA XUNITS / 'D/Lambda * Rteta', 'X-unit', 'meters'/
C----------------------------------------------------------------------
      EL = ELEV
      ALIN = WORSTX
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = T
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      XYRATO = 1.0
      PLTINC = 1000. / NCOUNT
C                                       Set window for current plot.
      XBLC(1) = BLC(1)
      XBLC(2) = 1000.0 - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
      TLC(1) = XBLC(1)
      TLC(2) = XTRC(2)
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
C                                       LABTYP(LOCNUM)=0 for xaxis
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (GMXX - GMNX)
      TI = TR
      CALL METSCA (TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 5 I = 1,2
         SIZE = 1000.0
         IF (I.EQ.2) SIZE = PLTINC
         TR = SIZE / XYSCL(I)
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 5      CONTINUE
      CTYP(1,LOCNUM) = XUNITS(ICODE)
      CTYP(2,LOCNUM) = AUNITS(ICODE)
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IRET = 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, CNOIN, CATBLK, POLBUF, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
         CALL ZPHFIL ('PL', DISKIN, CNOIN, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 17
C
         CALL GINIT (DISKIN, CNOIN, PFILE, IPSIZE, ITYPE, NPARM,
     *      XNAMEI, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, POLBUF, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
C                                       Number of characters on each
C                                       side of the plot
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
C                                       standard labeling
         LABEL = 3
         LTYPE = 3
         CHOUT(1) = INP + 4
         CHOUT(2) = 3.333
         CHOUT(4) = 4.666
C                                       Init for line drawing.
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, POLBUF, IERR)
         IRET = 3
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1000) VER
            CALL MSGWRT (3)
            END IF
         END IF
      IRET = 3
      CATUP = T
C                                       Draw border
      CALL GLTYPE (1, POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (XBLC(1), XTRC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XBLC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XBLC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XTRC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XTRC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      IF (ABS(IPLOT).EQ.1) THEN
         DX = 0.0
         DY = 1.833
C                                       The second line of the header
         CALL GPOS (BLC(1), TRC(2), POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         INP = 1
C                                       configuration in meters with
C                                       topography mask
         IF (ICODE.EQ.3) THEN
            SHIFTX = SECM
            SHIFTY = WORSTY
            ROTA = WORSTX
            WRITE (TEXT(INP:),1070) SHIFTX, SHIFTY, ROTA
         ELSE
            WRITE (TEXT(INP:),1040) SECM, WORSTX, WORSTY
            END IF
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       the third line of header
         DY = 0.5
         CALL GPOS (BLC(1), TRC(2), POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         INP = 1
         WREAD = .FALSE.
         IF (KNFILE(1:1).NE.' ') WREAD = .TRUE.
         IF (WREAD) THEN
            WRITE (TEXT(INP:),1055) KNFILE
         ELSE
            WRITE (TEXT(INP:),1056) NARRAY
            END IF
         INP = INP + 28
         IF (ICODE.EQ.3) THEN
            WRITE (TEXT(INP:),1080) I2FILE
         ELSE
            WRITE (TEXT(INP:),1045) ITER, EL
            END IF
C
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         DY = 0.5 + 2 * 1.333
C                                       the first line of the header
         CALL GPOS (BLC(1), TRC(2), POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (TEXT,1030) VER, ADATE, ATIME
         INCHAR = 51
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Set up location common
      CALL GPOS (XBLC(1), XTRC(2), POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
      DX =  1.5
      DY = -1.8
      INCHAR = 12
      INP = 1
C                                       label each plot
      IF (ICODE.EQ.1) THEN
         WRITE (TEXT(INP:),2040) XTETA
         INP = INP + 5
         CALL CHTRIM (TEXT, 80, TEXT, INCHAR)
         CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, POLBUF, IERR)
         IF (IERR.NE.0) GO TO 970
         END IF
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.NCOUNT)) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = NCOUNT / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, F, POLBUF, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       take data
C                                       Size of symbol.
      DX = 1.0
      DY = 1.0
      DBY = 0.5
      DXC = 10.0
      DYC = 10.0
C                                       Loop to plot the data
      DO 10 ITT = 1, ITIM
         XVARIB = X(ITT)
         VALUE = Y(ITT)
         BLPLT = .FALSE.
         IF (VALUE.EQ.FBLANK) THEN
            BLPLT = .TRUE.
            VALUE = 0.0
            END IF
C                                       Scale X, Y
         XY(1) = XVARIB
         XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
         IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1))) THEN
            NNOFIT = NNOFIT + 1
            GO TO 10
            END IF
         XY(2) = VALUE
         XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
         IF ((XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
            NNOFIT = NNOFIT + 1
            GO TO 10
            END IF
         NGOOD = NGOOD + 1
         DY = 1.0
         IF (BLPLT) DY = DBY
C                                       Draw crosses for UV points
         IF ((ITT.LE.ITIMC) .OR. (ITIMC.EQ.0)) THEN
            CALL GLTYPE (4, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GPOS (XY(1)+DX, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XY(1)-DX, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) + DY
            IF (YPT.GT.XTRC(2)) YPT = XTRC(2)
            CALL GPOS (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) - DY
            IF (YPT.LT.XBLC(2)) YPT = XBLC(2)
            CALL GVEC (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Draw diamonds for antennas
         ELSE IF ((ITT.LE.NUV) .AND. (ITIMC.GT.0)) THEN
            CALL GLTYPE (3, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GPOS (XY(1)-DXC, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) + DYC
            IF (YPT.GT.XTRC(2)) YPT = XTRC(2)
            CALL GVEC (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XY(1)+DXC, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) - DYC
            IF (YPT.LT.XBLC(2)) YPT = XBLC(2)
            CALL GVEC (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XY(1)-DXC, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
C                                       Draw crosses for circles
         ELSE
            CALL GLTYPE (2, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GPOS (XY(1)+DX, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GVEC (XY(1)-DX, XY(2), POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) + DY
            IF (YPT.GT.XTRC(2)) YPT = XTRC(2)
            CALL GPOS (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            YPT = XY(2) - DY
            IF (YPT.LT.XBLC(2)) YPT = XBLC(2)
            CALL GVEC (XY(1), YPT, POLBUF, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
 10     CONTINUE
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (3)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1201) NNOFIT
         CALL MSGWRT (3)
         END IF
      IF ((IPLOT.GT.0) .AND. (ABS(IPLOT).LT.NCOUNT)) GO TO 210
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (POLBUF, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, POLBUF, IERR)
            IERR = 0
            END IF
  210    IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
  960 CONTINUE
      WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, POLBUF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  CONTINUE
      WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1201) NNOFIT
         CALL MSGWRT (2)
         END IF
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (POLBUF, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, CNOIN, VER, POLBUF, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, CNOIN, 'WRIT', CATBLK, POLBUF,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1030 FORMAT ('Plot file version',I4,'  created ',A, A)
 1040 FORMAT (' The worst sidelobe = ', F7.5, '; X = ', F5.1,
     *   '; Y = ',F5.1)
 1045 FORMAT (' Iteration number', I4, '. Elev =',I3,'deg')
 2040 FORMAT (F5.1)
 1055 FORMAT ('Input file:', A17)
 1056 FORMAT ('Input:', I4, ' points in circle. ' )
 1070 FORMAT ('XSHIFT =', I5, ' m;', ' YSHIFT =', I5,
     *   ' m;', ' ROT = ',I4, ' deg.')
 1080 FORMAT ('Mask file:', A17)
 1200 FORMAT ('PLTEL: ',I9,' points plotted')
 1201 FORMAT ('PLTEL: ',I9,' points did not fit')
 1960 FORMAT ('PLTEL: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLTEL: ERROR DURING GRAPHING. ',
     *   ' WILL TRY TO FINISH PARTIAL GRAPH')
      END
      SUBROUTINE MASK ( NX, NY, NROT, CELLX, CELLY, DROT,
     *   XSHIFT, YSHIFT, ROT, ARSIZE, X, Y , XMASK, YMASK, XSHFOU,
     *   YSHFOU, ROTFOU, NBADMN, XFOBAD, YFOBAD, IRET)
C-----------------------------------------------------------------------
C   MASK find the best position of the array at the given topography.
C   Input:
C      NX      I    Number of shift at X
C      NY      I    Number of shift at Y
C      NROT    I    Number of rotations
C      CELLX   R    Step of shift at X, meter
C      CELLY   R    Step of shift at Y, meter
C      DROT    R    Step of rotation, degrees
C      ARSIZE  R    Desire size of the array, meter
C      X       R(*) Normolized X positions of the array's elements
C      Y       R(*) Normolized Y positions of the array's elements
C      XSHIFT  R    Gess shift of the configuration at X, meter
C      YSHIFT  R    Gess shift of the configuration at Y, meter
C      ROT     R    Gess rotation of the configuration, radians
C   Inputs from Common:
C      I2FILE  C(*) File of topography
C      NARRAY  I    Number of elements at the array
C      GMNX    R    Max. value to plot
C      GMXX    R    Min. value to plot
C      XBLCC   R    X of bottom low corner of the mask, in meters
C      YBLCC   R    Y of bottom left corner of the mask, in meters
C      XTRCC   R    X of top right corner of the mask, in meters
C      YTRCC   R    Y of top right corner of the mask, in meters
C   Output:
C      IRET    I    Return code, 0 => OK, otherwise abort.
C      XMASK   R(*) Fitted X positions of the array's elements in meters
C      YMASK   R(*) Fitted Y positions of the array's elements in meters
C      XSHFOU  R    Found shift of the configuration at X, meter
C      YSHFOU  R    Found shift of the configuration at Y, meter
C      ROTFOU  R    Found rotation of the configuration, radians
C   Output in Common:
C      NBAD    I    Number of bad points at the topgraphy
C      XBAD    R(*) Array of X bad points at the topography
C      YBAD    R(*) Array of Y bad points at the topography
C      NBADMN  I    Number of found bad points of the array
C      XFOBAD  R(*) X of bad points
C      YFOBAD  R(*) Y of bad points
C-----------------------------------------------------------------------
      INTEGER   ISCR(256)
      INTEGER NX, NY, NROT, IRET
      REAL CELLX, CELLY, DROT, XSHIFT, YSHIFT, ROT, ARSIZE, X(*), Y(*),
     *   XMASK(*), YMASK(*), XFOBAD(*), YFOBAD(*)
      INTEGER IX, IY, KX, KY, KBAD, NBADMN, IROT, NCBAD, NUMBAD(200),
     *   NUMBA(200), I, NPLOTC, NCOUNC, IPLTC, JT, JTRIM, IPLOTC, NUV,
     *   ITER, IND
      INTEGER NBYTES, KBP
      REAL   XSH, YSH, ROTA, XSHFOU, YSHFOU,
     *   ROTFOU, ROTFDG, XT, YT, SIZEX, SIZEY, TOLER, TMAX, TMIN,
     *   TDIF, XTETA, EL, DXBT, DYBT
      CHARACTER LINE*80
      LOGICAL BAD, INSIDE
      DOUBLE PRECISION VALUE
      INCLUDE 'CONFI.INC'
C
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'


      NBAD = 0
      LUNPR = 10
      FA = .FALSE.
      XMAX = -1.0E-10
      YMAX = -1.0E-10
      XMIN = 1.0E10
      YMIN = 1.0E10
      DXBT = XTRCC - XBLCC
      DYBT = YTRCC - YBLCC
C                                       FITS file
      IF (DOFITS) THEN
         CALL FITASC (ISCR, CELLX, CELLY, IRET)
         IF (IRET.NE.0) GO TO 999
      ELSE

C                                       open mask file in ASCII format
         CALL ZTXOPN ('READ', LUNPR, PFIND, I2FILE, FA, IRET)
         NBYTES = 80
C                                       read the file data
  320    CALL ZTXIO ('READ', LUNPR, PFIND, LINE, IRET)
         IF (IRET.EQ.2) GO TO 340
         JT = JTRIM (LINE)
C                                       Get values
         KBP = 1
C
         CALL GETNUM (LINE, NBYTES, KBP, VALUE)
         KX = VALUE + 0.1
         CALL GETNUM (LINE, NBYTES, KBP, VALUE)
         KY = VALUE + 0.1
         CALL GETNUM (LINE, NBYTES, KBP, VALUE)
         IND = VALUE + 0.1
C
         XT = (KX - 1) * CELLX
         YT = (KY - 1) * CELLY
C                                       take the point inside of BLC,
C                                       TRC
         IF (XT.LT.XBLCC) THEN
            GO TO 320
         ELSE
            IF (XT.LT.XTRCC) THEN
               IF (YT.LT.YBLCC .OR. YT.GT.YTRCC) GO TO 320
            ELSE
               GO TO 340
               END IF
            END IF

         XMAX = MAX(XMAX, XT)
         XMIN = MIN(XMIN, XT)
         YMAX = MAX(YMAX, YT)
         YMIN = MIN(YMIN, YT)
C                                       the bad (restricted) points
C                                       correspond to 1 (.NE. 0)
         IF (IND .NE. 0) THEN
            NBAD = NBAD + 1
            IF (NBAD .GT. MXMASK) THEN
               IRET = 5
               WRITE (MSGTXT,1040) MXMASK
               CALL MSGWRT (8)
               GO TO 999
               END IF
            XBAD(NBAD) = XT
            YBAD(NBAD) = YT
            END IF
C
         GO TO 320
  340    CONTINUE
         CALL ZTXCLS (LUNPR, PFIND, IRET)
         END IF
C
      WRITE (MSGTXT,1080)  NBAD
      CALL MSGWRT (8)
C-----shift and rotate configuration to avoid bad topography. start---
      DROT = DROT * DG2RAD
      NBADMN = 1000
      XSHFOU = XSHIFT
      YSHFOU = YSHIFT
      ROTFOU = ROT
      DO 390 IX = 1, NX
         XSH = XSHIFT + (IX - 1 - NX/2) * CELLX
         DO 380 IY = 1, NY
            YSH = YSHIFT + (IY - 1 - NY/2) *CELLY
            DO 370 IROT = 1, NROT
               ROTA = ROT + (IROT - 1 - NROT/2) * DROT
C                                       Check that all elements are
C                                       inside of the given region
               DO 345 I = 1, NARRAY
                  XT = (X(I) * COS(ROTA) + Y(I) * SIN(ROTA))
     *               * ARSIZE + XSH
                  YT = (Y(I) * COS(ROTA) - X(I) * SIN(ROTA))
     *               * ARSIZE + YSH
                  INSIDE = ((XT.LT.XMAX) .AND. (XT.GT.XMIN))
     *               .AND. ((YT.LT.YMAX) .AND. (YT.GT.YMIN))
                  IF (.NOT. INSIDE) GO TO 370
  345             CONTINUE


               NCBAD = 0
               DO 360 I = 1, NARRAY
                  XT = (X(I) * COS(ROTA) + Y(I) * SIN(ROTA))
     *               * ARSIZE + XSH
                  YT = (Y(I) * COS(ROTA) - X(I) * SIN(ROTA))
     *               * ARSIZE + YSH
C                                       Is the given element in good
C                                       location?
                  DO 350 KBAD = 1, NBAD
                     BAD = (ABS (XT - XBAD(KBAD)).LE.0.55*CELLX)
     *                  .AND. (ABS (YT - YBAD(KBAD)).LE.0.55*CELLY)
                     IF (BAD) THEN
                        NCBAD = NCBAD + 1
                        NUMBA(NCBAD) = I
                        GO TO 360
                        END IF
  350                CONTINUE
  360             CONTINUE
               IF (NCBAD.EQ.0) THEN
                  NBADMN = NCBAD
C                                       the configuration fitted the
C                                       topography has been found
                  XSHFOU = XSH
                  YSHFOU = YSH
                  ROTFOU = ROTA
                  WRITE (MSGTXT,1090)
                  CALL MSGWRT (8)
                  GO TO 400
               ELSE
C                                       find the minimum of unfitted
C                                       elements
                  IF (NCBAD.LT.NBADMN) THEN
                     NBADMN = NCBAD
                     DO 365 I = 1, NBADMN
                        NUMBAD(I) = NUMBA(I)
  365                   CONTINUE
                     XSHFOU = XSH
                     YSHFOU = YSH
                     ROTFOU = ROTA
                     END IF
                  END IF
  370          CONTINUE
  380       CONTINUE
  390    CONTINUE
C
      IF (NBADMN.EQ.1000) THEN
C                                       some antennas appeared outside
C                                       of the topography mask
         IRET = 10
         WRITE (MSGTXT,1050)
         CALL MSGWRT (8)
         GO TO 999
      ELSE
         WRITE (MSGTXT,1100) NBADMN
         CALL MSGWRT (8)
         END IF
  400 CONTINUE
      ROTFDG = ROTFOU*RAD2DG
C
      WRITE (MSGTXT,1110) XSHFOU, YSHFOU, ROTFDG
      CALL MSGWRT (8)
C                                       add the fitted configuration
      DO 410 I = 1, NARRAY
         XT = (X(I) * COS(ROTFOU) + Y(I) * SIN(ROTFOU))
     *      * ARSIZE + XSHFOU
         YT = (Y(I) * COS(ROTFOU) - X(I) * SIN(ROTFOU))
     *      * ARSIZE + YSHFOU
         XBAD(I + NBAD) = XT
         YBAD(I + NBAD) = YT
         XMASK(I) = XT
         YMASK(I) = YT
         DO 405 KBAD = 1, NBADMN
            IF (NUMBAD(KBAD).EQ.I) THEN
               XFOBAD(KBAD) = XMASK(I)
               YFOBAD(KBAD) = YMASK(I)
               GO TO 410
               END IF
  405          CONTINUE
  410    CONTINUE
      DO 415 KBAD = 1, NBADMN
         WRITE (MSGTXT,1120) KBAD, NUMBAD(KBAD), XFOBAD(KBAD),
     *      YFOBAD(KBAD)
         CALL MSGWRT (8)
  415    CONTINUE
C-----shift and rotate configuration to avoid bad topography. end-----
C                                       Plot restricted points
C                                       Prepare parameters for PLTEL
      NPLOTC = 1
      NCOUNC = 1
      IPLTC = 1
      IPLOTC = IPLTC - 1
      IPLOTC = MOD (IPLOTC, NCOUNC) + 1
      IF (IPLTC.EQ.NPLOTC) IPLOTC = -IPLOTC
C      SIZEY = 1000.0 / NCOUNC
      GMXX = -1.0E20
      GMNX = 1.0E20
C                                       find max/min of the Y
      FUNCMX = -1.0E10
      FUNCMN =  1.0E10
      ARGMX = -1.0E10
      ARGMN =  1.0E10
C
      NUV = NBAD + NARRAY
      DO 420 I = 1, NUV
         FUNCMX = MAX(FUNCMX, YBAD(I))
         FUNCMN = MIN(FUNCMN, YBAD(I))
         ARGMX = MAX(ARGMX, XBAD(I))
         ARGMN = MIN(ARGMN, XBAD(I))
  420    CONTINUE
C                                       scale the Xsize to have
C                                       identical scale at X and Y
      SIZEY = 1000.0
      SIZEX = SIZEY * (ARGMX - ARGMN) / (FUNCMX - FUNCMN)
      TOLER = 0.01
C
C                                       TMAX, TMIN new MAX&MIN of Y
      TMAX = FUNCMX + 0.1 * (FUNCMX - FUNCMN)
      TMIN = FUNCMN - 0.1 * (FUNCMX - FUNCMN)
      IF (ABS (TMAX-TMIN).LT.TOLER) THEN
         TMAX = TMAX + TOLER
         TMIN = TMIN - TOLER
         END IF
C                                       GMXX, GMNX MAX&MIN of Y for all
C                                       plots
      GMXX = MAX (GMXX, TMAX)
      GMNX = MIN (GMNX, TMIN)
      TDIF = TMAX - TMIN
      IF (ABS (TDIF).LE.1.0E-25) TDIF = 1.0E-25
      GXYOFF(2,IPLTC) = TMIN
      GXYSCL(2,IPLTC) = SIZEY / TDIF
C                                       now about X-axis
      TMAX = (ARGMX + 0.1 * (ARGMX - ARGMN))
      TMIN = (ARGMN - 0.1 * (ARGMX - ARGMN))
      TDIF = TMAX - TMIN
      IF (ABS (TDIF).LE.0.01) TDIF = 0.01
      GXYOFF(1,IPLTC) = TMIN
      GXYSCL(1,IPLTC) = SIZEX / TDIF
C                                       plot Y/ARG and model
      XYSCL(1) = GXYSCL(1,IPLTC)
      XYSCL(2) = GXYSCL(2,IPLTC)
      XYOFF(1) = GXYOFF(1,IPLTC)
      XYOFF(2) = GXYOFF(2,IPLTC)
      CALL PLTEL (IPLOTC, NUV, NBAD, NUV, XTETA, NCOUNC, EL, XSHFOU,
     *   YSHFOU, ROTFDG, INFILE, XBAD, YBAD, 3, ITER, IRET)
C
  999 RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('!Number of restricted pixels at the mask',
     *   ' exceeds limit', I8)
 1050 FORMAT ('Some antennas are outside the topography mask')
 1080 FORMAT ('NBAD = ', I8)
 1090 FORMAT ('Configuration fitted topography is found!!!')
 1100 FORMAT (I3, ' elements are not fitted')
 1110 FORMAT ('XSHFOU=', F6.0, ' m; YSHFOU=', F6.0,
     *   ' m;  ROTFOU =', F5.0, ' deg')
 1120 FORMAT (2I6, ' XFIT= ', F7.1, ' YFIT= ', F7.1)
      END
      SUBROUTINE OUTER (I2FILE, NOUTER, ARSIZE, DLIMIT, X, Y, I, NFIX,
     *   IRET)
C-----------------------------------------------------------------------
C   OUTER find element positions on the outer circle if some are fixed
C   Input:
C      I2FILE  C*48 Name of the file with fixed positions in
C                   normalized form
C      NOUTER  I    Number of elements at the outer circle
C      ARSIZE  R    Dimension of the array in meters
C      DLIMIT  R    Minimum distance between antennas, in meters
C   Output:
C      X,Y     R(*) Position of the elements
C      NFIX    I    Number of fixed elements
C      IRET    I    Return code, 0 => OK, otherwise abort.
C   In/Output:
C      I       I    The curent number of the element
C-----------------------------------------------------------------------
      INTEGER   NOUTER, NFIX, LUNPR, PFIND, NLINE, I, NADD, NIFIX,
     *   IFIX, IROUND, K, KK, KCAN(100), DROOM(100), IRET, JT, JTRIM
      REAL   ARSIZE, DLIMIT, X(*), Y(*), XT, YT, SL, ARG, DOTPRO,
     *   LENGTH, ARCINT, DLENGT, DISTM
      CHARACTER I2FILE*48, LINE*80
      LOGICAL   T, F, FA
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       outer circle
C                                       open in2file if its name is
C                                       not blank to read the position
C                                       and number of fixed elemets at
C                                       the outer circle
      LUNPR = 10
      FA = F
      CALL ZTXOPN ('READ', LUNPR, PFIND, I2FILE, FA, IRET)
      IF (IRET.NE.0) GO TO 999
      NLINE = 0
      I = 0
C                                       read the file data
 10   CALL ZTXIO ('READ', LUNPR, PFIND, LINE, IRET)
      IF (IRET.EQ.2) GO TO 50
      IF (IRET.GT.0) GO TO 999
      I = I + 1
      NLINE = NLINE + 1
      JT = JTRIM (LINE)
      READ (LINE,1000) XT, YT
      X(NLINE) = XT
      Y(NLINE) = YT
      GO TO 10
50    CONTINUE
      NFIX = NLINE
      CALL ZTXCLS (LUNPR, PFIND, IRET)
C                                       Now we have NFIX fixed elements
C                                       on the outer circle. Let's add
C                                       the rest NADD elements
      NADD = NOUTER - NFIX
C                                       insert number of elements
C                                       between i and i+1 fixed
C                                       elements proportionnaly to the
C                                       relevant distance between the
C                                       fixed elements
      DO 80 IFIX = 1, NFIX
         SL = ATAN2(Y(IFIX), X(IFIX))
         KK = 0
         IF (IFIX.LT.NFIX) THEN
            DOTPRO = X(IFIX) * X(IFIX+1) + Y(IFIX) * Y(IFIX+1)
         ELSE
            DOTPRO = X(1) * X(NFIX) + Y(1) * Y(NFIX)
            END IF
C                                       length of the arc in radians
C                                       between neibourgh elements on
C                                       the circle of radius 0.5
         LENGTH = ACOS(4 * DOTPRO)

C                                       How much elements can be spaced
C                                       in this arc with spacing DLIMIT
         ARCINT = 2 * ASIN(DLIMIT/ARSIZE)
C                                       ARCINT is arc of the interval
         KCAN(IFIX) = LENGTH / ARCINT - 1
C                                       The number of inserted elements
C                                       in the arc LENGTH homogeniously
         ARCINT = TWOPI / NADD
         NIFIX = IROUND(LENGTH / ARCINT)
C                                       Do the next operation only for
C                                       the smallest configuration
         IF (ARSIZE / DLIMIT.LT.10) THEN
            IF (KCAN(IFIX).GT.NIFIX) NIFIX = KCAN(IFIX)
            END IF
         IF (IFIX.EQ.NFIX) NIFIX = NOUTER - I
   55    DLENGT = LENGTH / (NIFIX + 1)
         DISTM = ARSIZE * SIN(DLENGT/2)
         IF (DISTM.LT.DLIMIT) THEN
            NIFIX = NIFIX - 1
            IF (NIFIX.LE.0) GO TO 70
            GO TO 55
            END IF
         DO 60 K = 1, NIFIX
            ARG = SL + K * DLENGT
            I = I + 1
            KK = KK + 1
            X(I) = 0.5 * COS(ARG)
            Y(I) = 0.5 * SIN(ARG)
            IF (I.EQ.NOUTER) GO TO 100
   60       CONTINUE
C                                       How much elements can be more
C                                       inserted into the given int.
  70    DROOM(IFIX) = KCAN(IFIX) - KK
  80     CONTINUE
  100 NIFIX = NOUTER - I
      IF (NIFIX.EQ.0) GO TO 999
      IF (NIFIX.GT.0) THEN
         WRITE (MSGTXT, 1100) NIFIX
         CALL MSGWRT (8)
         WRITE (MSGTXT, 1200)
         CALL MSGWRT (8)
         END IF
C
      DO 90 IFIX = 1, NFIX
         IF (DROOM(IFIX).GT.0) THEN
            WRITE (MSGTXT, 1300) DROOM(IFIX), IFIX, IFIX+1
            CALL MSGWRT (8)
            END IF
   90    CONTINUE

  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (2F20.9)
 1100 FORMAT ('There is no room for ', I2, ' elements in outer circle')
 1200 FORMAT ('Cut short number of the fixed elements')
 1300 FORMAT ('There is a room for', I2, ' elements at the interval ',
     *   I2,'-',I2)
      END
C
C----the soubroutines to read topography at FITS format-----------------
      SUBROUTINE FITASC (ISCR, CELLX, CELLY, IRET)
C-----------------------------------------------------------------------
C     FITASC reads the fits file and records it to ascii file
      INTEGER  IRET
      INTEGER  IOP, FITS, ISCR(256), IERR
      REAL CELLX, CELLY
      LOGICAL  NODATA
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       store the old CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Convert to internal format.
C
      CALL MLTAPE (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       See what kind of file.
      CALL TPIOHD (FDVEC, 128, FITS, TBIND, TAPBUF, ISCR, IRET)
      IF (IRET.NE.0) GO TO 950
      IF (FITS.EQ.1) THEN
         IOP = 1
         CALL FITHDR (IOP, NODATA, IRET)
C                                       Get the data and store in file
         CALL FITDAT (CELLX, CELLY, IRET)
         IF (IRET.NE.0) GO TO 950
      ELSE
         WRITE (MSGTXT,1000)
         IF (FITS.EQ.-1) WRITE (MSGTXT,1001)
         IF ((FITS.EQ.-1) .AND. (FDVEC(42).EQ.2560))
     *      WRITE (MSGTXT,1002)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 950
         END IF
C                                       Close output
 950  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
C                                       restore the old CATBLK
C                                       for PLTEL with dotv=-1
      CALL COPY (256, CATOLD, CATBLK)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPE IS UV-EXPORT FORMAT: USE UVLOD')
 1001 FORMAT ('TAPE FILE IS NON-STANDARD FITS WHICH IS NOT SUPPORTED')
 1002 FORMAT ('TAPE IS LIKELY TO BE ''RPFITS'' FORMAT: TRY ATLOD')
      END
C
C
      SUBROUTINE MLTAPE (IERR)
C-----------------------------------------------------------------------
C   MLTAPE sets up for TAPIO and opens the input for FIT2A.
C   Outputs: IERR        I    Error return
C                             0--> okay,  1--> error
C   Uses and build special common /MLTAPE/
C-----------------------------------------------------------------------
      INTEGER  IERR
      INCLUDE 'FIT2A.INC'
      INCLUDE 'CONFI.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
C                                       Disk output.
      CALL CHR2H (48, I2FILE, 1, FDVEC(7))
      WRITE (MSGTXT,1000) I2FILE
      CALL MSGWRT (2)
      FDVEC(1) = 25
C      NTAPE = 1
      FDVEC(5) = 1
C                                       Open tape
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 980
         END IF
C                                       Error returns
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Reading from disk file: ',A)
 1025 FORMAT ('MLTAPE: COULD NOT OPEN TAPE.  IER=',I7)
      END
      SUBROUTINE FITHDR (IOP, NODATA, IERR)
C-----------------------------------------------------------------------
C   FITHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     IOP    I     Operation code 1=> read tape, 2=>just create dummy
C                  file
C   Output:
C     NODATA L     True if tape contains no data section, else false.
C     IERR   I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      REAL      PIX11(2)
      INTEGER   IOP, ICARD, IE, IERR, IREC, I,
     *   IN, IS, IAX, IDEPTH(5), ICEND
      LOGICAL   END, F, T, ISHIST, NODATA
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      FUCKUP = .FALSE.
      CALL CATCLR (CATBLK)
C                                       See if tape read requested
      IF (IOP.EQ.2) GO TO 15
C                                       Initialize header values.
         CALL CATINI (CATBLK)
         SCALE = 1.0D0
         OFFSET = 0.0D0
         ISCALE = 1.0D0
         IZERO = 0.0D0
C                                       Record 1 already read
         CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       Decode required cards.
         CALL IMREQC (HDRBUF, ICEND, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (CATBLK(KIDIM).NE.0) GO TO 5
C                                       No data, must have tables.
            IF (.NOT.STDEXT) GO TO 980
C                                       Make a 2x2 map.
 15   NODATA = .TRUE.
      ISBLNK = .TRUE.
      CATBLK(KIDIM) = 2
            CATBLK(KINAX) = 2
            CATBLK(KINAX+1) = 2
C                                       More defaults.
 5    DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = CATBLK(KINAX+I-1) / 2
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, 'FIT2A       ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, 'TEMP  ', KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL CHR2H (12, '            ', KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, '      ', KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
C      ISLOT = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000)
         GO TO 990
         END IF
      NCFILE = 1
C      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
C      FVOL(NCFILE) = KVOL
C                                       See if need to parse rest of
C                                       header.
      IF (IOP.EQ.2) GO TO 210
C
         ICARD = ICEND + 1
C                                       Loop until END card found.
         DO 90 IREC = 1,1000000
C                                       Read next record.
            IF (ICARD.LE.36) GO TO 80
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
               ICARD = 1
C                                       Parse card, put value in hdr.
 80         CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
            IF (END) GO TO 100
            ICARD = ICARD + 1
 90         CONTINUE
C                                       End card found.
 100     CONTINUE
C                                       Make axis increments non zero
C                                       to help out dumb programs.
         IN = KINAX
         IS = KRCIC
         IE = IS + CATBLK(KIDIM) - 1
         DO 200 IAX = IS,IE
            IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1))
     *         CATR(IAX) = 1.0
            IN = IN + 1
 200        CONTINUE
  210    CONTINUE

C                                       Correct for PDP 11 values.
C                                       set common values
      IF (FUCKUP) THEN
         DO 310 I = 3,7
            IDEPTH(I-2) = 1
            IF (I.GT.CATBLK(KIDIM)) GO TO 310
               IDEPTH(I-2) = CATR(KRCRP+I-1) + 0.5
               IDEPTH(I-2) = MAX (1, MIN (IDEPTH(I-2),
     *            CATBLK(KINAX+I-1)))
 310        CONTINUE
         LOCNUM = 1
         CALL SETLOC (IDEPTH, F)
C                                       do conversion
         IF ((ABS(POS11(1)-CATD(KDCRV+KLOCL(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCL(LOCNUM)))) .OR.
     *      (ABS(POS11(2)-CATD(KDCRV+KLOCM(LOCNUM))).GE.
     *      ABS(0.01*CATR(KRCIC+KLOCM(LOCNUM))))) THEN
            CALL LMPIX (POS11(1), POS11(2), PIX11(1), PIX11(2))
            IF ((ABS(PIX11(1)-CATR(KRCRP+KLOCL(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCL(LOCNUM))/2) .OR.
     *         (ABS(PIX11(2)-CATR(KRCRP+KLOCM(LOCNUM))).GE.
     *         CATBLK(KINAX+KLOCM(LOCNUM))/2)) THEN
               WRITE (MSGTXT,1310) POS11
               CALL MSGWRT (6)
               WRITE (MSGTXT,1311) PIX11
               CALL MSGWRT (6)
            ELSE
               WRITE (MSGTXT,1320) CATR(KRCRP+KLOCL(LOCNUM)),
     *            CATR(KRCRP+KLOCM(LOCNUM)), PIX11
               CALL MSGWRT (3)
               CATR(KRCRP+KLOCL(LOCNUM)) = PIX11(1)
               CATD(KDCRV+KLOCL(LOCNUM)) = POS11(1)
               CATR(KRCRP+KLOCM(LOCNUM)) = PIX11(2)
               CATD(KDCRV+KLOCM(LOCNUM)) = POS11(2)
               END IF
            END IF
         END IF
      GO TO 999
C                                       No Data, no tables.
 980  WRITE (MSGTXT,1980)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP CREATE ERROR')
 1310 FORMAT ('PDP11/70 ERROR: PHASE REF. POS.',2E14.6)
 1311 FORMAT ('GIVES REF. PIXEL',2F9.2,' IGNORED')
 1320 FORMAT ('CORRECTING REF. PIXEL FROM',2F8.2,' TO',2F8.2)
 1980 FORMAT ('THIS FITS FILE HAS NEITHER DATA NOR TABLES')
      END
C
C
      SUBROUTINE IMREQC (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, EXTEND*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
      DATA EXTEND /'EXTEND  '/
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      I = NCT + NKT
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
      IF (NAXIS.LT.1) GO TO 40
C                                       Check NAXISm
         DO 30 I = 1,NAXIS
            ICARD = ICARD + 1
            IKEYWD = IKEYWD + 1
            NPNT = 1
            CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *         KL, SYMBOL, ITABNO, ISHIST, END, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            CATBLK(IAX) = X + .01
            IAX = IAX + 1
 30         CONTINUE
C                                       Look for EXTEND = T card.
 40   CONTINUE
      IF (CATBLK(KINAX).EQ.0) GO TO 930
      ICARD = ICARD + 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, EXTEND, FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, ITYP)
      IF (END.OR.ISHIST.OR.(ITYP.NE.0)) GO TO 50
          CALL GETLG (KL, 80, NPNT, ITYP)
          IF (ITYP.NE.1) GO TO 50
          STDEXT = .TRUE.
          GO TO 999
 50   CONTINUE
C                                       No extensions
         ICARD = ICARD - 1
         STDEXT = .FALSE.
      GO TO 999
C                                       Probably a UV tape.
 930  WRITE (MSGTXT,1930)
      CALL MSGWRT (8)
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'IMREQC: VALUE ERROR PARSING ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('THIS IS PROBABLY A UV DATA FILE THAT MUST BE READ WITH',
     *   ' UVLOD')
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
C
      SUBROUTINE FITDAT (CELLX, CELLY, IER)
C-----------------------------------------------------------------------
C   FITDAT reads the input data file and scales the data to disk.
C   Inputs:
C      KVOL  I     desired map disk
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
C
      INTEGER  IX, IY, IND
      INTEGER   IER
C
      CHARACTER MNAME*48, CDECLS(5)*4, CHTM12*12
      INTEGER   BLKS, IERR, IWIN(4), NBKOF1, IOFF, NX, NY, IDEPTH(5),
     *   NBYB, I, INX, INY, IBL, ITEMP, NXY, I3, I3B, I4, I4B, I5, I5B,
     *   I6, I6B, I7, I7B, DLUN, NTAPVL, J, NDECLS, III, L0, L1,
     *   L2, NXX, OUTIND
      INCLUDE 'FIT2A.INC'
      INCLUDE 'CONFI.INC'
      REAL      BUFF(MABFSS), MMAX, MMIN, INBUFR(MABFSS), XT, YT,
     *   CELLX, CELLY
      DOUBLE PRECISION    BSC, BZE, DPBUFR(MABFSS/2), DTEMP
      LOGICAL   T, BACK, WASBLK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUFR(1), INBUFF(1), DPBUFR(1))
      DATA T /.TRUE./
      DATA NDECLS, CDECLS /5, 'DEC ', 'DEC-', 'MM  ', 'GLAT', 'ELAT'/
C-----------------------------------------------------------------------
      DLUN = 16
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
      IF (IERR.EQ.0) GO TO 5
         WRITE (MSGTXT,1000) MNAME, IERR
         GO TO 980
C                                       Initialize
 5    IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
      NBYB = MABFSS * 2
C                                       second axis backwards?
      BACK = .FALSE.
      IF (CATR(KRCIC+1).GE.0.0) GO TO 10
         J = KHCTP + 2
         DO 6 I = 1,NDECLS
            CALL H2CHR (4, 1, CATH(J), CHTM12)
            BACK = CHTM12(1:4) .EQ. CDECLS(I)(1:4)
            IF (BACK) GO TO 8
  6         CONTINUE
         GO TO 10
  8      CONTINUE
            CATR(KRCIC+1) = -CATR(KRCIC+1)
            CATR(KRCRP+1) = CATBLK(KINAX+1) + 1 - CATR(KRCRP+1)
C                                       Set window parms
 10   I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      NY = IWIN(4)
      NX = IWIN(3)
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
      IF (.NOT.BACK) GO TO 20
         IWIN(2) = NY
         IWIN(4) = 1
C                                       Initialize tape
 20   NBPIX = TAPEBP
      BLKS = (ABS(NBPIX) / 8)
      NTAPVL = 2880 / BLKS
      IOFF = NTAPVL
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
C         CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, BUFF, NBYB,
C     *      NBKOF1, IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IERR
            GO TO 980
   30    CONTINUE
C
         NBAD = 0
C                                       Begin read/write loop
         DO 195 I = 1,NY
            NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-64) CALL ZR64RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.EQ.8) .OR. (NBPIX.EQ.16) .OR. (NBPIX.EQ.32))
     *            THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           BUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           BUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, BUFF(L1))
                           MMAX = MAX (MMAX, BUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        BUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 115                    CONTINUE
                     END IF
                  GO TO 190
C                                       IEEE 64-bit in
               ELSE IF (NBPIX.EQ.-64) THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 150 III = 1,NXX
                     L1 = L2 + III
                     DTEMP = DPBUFR(L0+III)
                     IF (DTEMP.EQ.DBLANK) THEN
                        WASBLK = .TRUE.
                        BUFF(L1) = FBLANK
                     ELSE
                        BUFF(L1) = BSC * DTEMP + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 150                 CONTINUE
C                                       IEEE 32-bit in
               ELSE
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     BUFF(L1) = INBUFR(L0+III)
                     IF (BUFF(L1).EQ.FBLANK) THEN
                        WASBLK = .TRUE.
                     ELSE
                        BUFF(L1) = BSC * BUFF(L1) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 160                 CONTINUE
                  END IF
C                                       Up the counters
 190           IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
C---------------------------------------------------------
C                                       new card to read the fits file
            IY = I
            DO 170 IX = 1, NX
               XT = (IX - 1) * CELLX
               YT = (IY - 1) * CELLY
C                                       take the point inside of BLC,
C                                       TRC
               IF (XT.LT.XBLCC) THEN
                  GO TO 170
               ELSE
                  IF (XT.LT.XTRCC) THEN
                     IF (YT.LT.YBLCC .OR. YT.GT.YTRCC) GO TO 170
                  ELSE
                     GO TO 180
                     END IF
                  END IF

               XMAX = MAX(XMAX, XT)
               XMIN = MIN(XMIN, XT)
               YMAX = MAX(YMAX, YT)
               YMIN = MIN(YMIN, YT)


               IND = BUFF (IX + OUTIND - 1) + 0.01
C                                       select bad (restricted) points
C                                       corresponded to 1 (.NE. 0)
               IF (IND .NE. 0) THEN
                  NBAD = NBAD + 1
                  IF (NBAD .GT. MXMASK) THEN
                     IERR = 5
                     WRITE (MSGTXT,1040) MXMASK
                     GO TO 980
                     END IF
                  XBAD(NBAD) = XT
                  YBAD(NBAD) = YT
                  END IF
  170          CONTINUE
  180 CONTINUE
C
 195         CONTINUE
C
 200     CONTINUE
C                                       close files
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1980)
         CALL MSGWRT(8)
         END IF
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITDAT: COULD NOT OPEN MAP ',6A4,' IER=',I4)
 1020 FORMAT ('FITDAT: COULD NOT INITIALIZE DISK FILE.  IER=',I4)
 1040 FORMAT ('!Number of restricted pixels at the mask',
     *   ' exceeds limit', I8)
 1970 FORMAT ('FITDAT: COULD NOT READ INPUT.  IER=',I4)
 1980 FORMAT ('FITDAT: - MAYBE PREMATURE END OF FILE?  CHECK FILE SIZE')
      END
C
      SUBROUTINE IMPARS (ICARD, FITBLK, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error, -1 => special header
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, NPNTS
      INCLUDE 'FIT2A.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CONTINUE
      CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF (IERR.NE.0) GO TO 999
      IF (FIRST) ISHIST = LHIST
C      IF ((ISHIST) .AND. (XERR4.GT.1.5)) GO TO 999
      FIRST = .FALSE.
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
      NPNTS = NPNT
      GO TO (100, 200, 300), KT
C                                       Logical value
 100     CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.GE.0) GO TO 110
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
C                                       Logical value special cases.
 110     CONTINUE
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
         GO TO 400
C                                       Number
 200     CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) GO TO 975
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).NE.'BLANK') GO TO 220
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
            GO TO 400
C                                       PDP 11 Stuff
 220     IF ((AWORD(TABNO).NE.'OPHRAE11') .AND.
     *      (AWORD(TABNO).NE.'OPHDCE11')) GO TO 230
            POS11(IPOFF) = X
            FUCKUP = .TRUE.
            GO TO 400
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
 230     IF (NBYT.NE.2) GO TO 240
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
            GO TO 400
C                                       4-byte real
 240     IF (NBYT.NE.4) GO TO 250
            IF ((AWORD(TABNO) .EQ. 'HISTORY') .AND. (X .GT. 1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
            GO TO 400
C                                       8-byte real
 250     IF (NBYT.NE.8) GO TO 400
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
            GO TO 400
C                                       String
 300     CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).NE.'IMCLASS') GO TO 320
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
            GO TO 400
C                                       Start string on real boundary.
 320     IPOFF = (NBYT / 4) * IPOFF
            CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
            CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
C
C                                       If this is a history card, look
C                                       for more values.
 400     IF (ISHIST) GO TO 10
         GO TO 999
C
 975  MSGTXT = 'IMPARS: NUMBER VALUE ERROR ON ' // SYMBOL
C                                       Error message
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (2A4,'LOGICAL VARIABLE HAS ILLEGAL VALUE')
      END
C
      SUBROUTINE FILFIT (I2FILE, DOFITS)
C-----------------------------------------------------------------------
C   FILFIT reads the input name I2FILE, and determines if the sequence of
C                                       the characters 'FITS' appears in
C                                       the name.
C
C   Inputs:  I2FILE      C*48 input name
C
C   Outputs: DOFITS      L  'FITS' appears in I2FILE => DOFITS= .TRUE.
C                           ELSE DOFITS = .FALSE.
C-----------------------------------------------------------------------
      CHARACTER   I2FILE*48
      LOGICAL     DOFITS
      INTEGER     I
C-----------------------------------------------------------------------
      DOFITS = .FALSE.
      I = 1
    5 CONTINUE
      IF (I2FILE(I:I+3).EQ.'FITS') THEN
         DOFITS = .TRUE.
         GO TO 999
      ELSE
         I = I + 1
         IF (I .LE. 45) GO TO 5
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
      END



