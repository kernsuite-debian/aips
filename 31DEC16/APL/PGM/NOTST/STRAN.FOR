      PROGRAM STRAN
C-----------------------------------------------------------------------
C! Compares two ST tables and calculate an image coordinate transform
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2009, 2015
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
C   STRAN reads in an ST table with the PIXELS of an image which contain
C   stars and a second ST table with COORDINATES.  The two tables
C   are compared, and a coordinate transformation is calculated.
C    INPUTS:   (from AIPS)
C              USERID   R      user 0 means use logon user number,
C                              32000 means any user can be accessed.
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R      sequence number of primary file.
C              INDISK   R      disk volume number. 0 means try all.
C              INVERS   R      ST table with pixel locations
C              IN2VERS  R      ST table with coordinates
C              BPARM    R(10)  Input Parameters
C              CPARM    R(10)  Input Parameters
C              DPARM    R(10)  Input Parameters
C-----------------------------------------------------------------------
C                                       Max Stars, Triangles and FITS
      INTEGER   MXSTAR, MXTRIA, MXFIT,MXFIT2
      PARAMETER (MXSTAR=2000, MXTRIA=4000, MXFIT=50, MXFIT2=100)
      DOUBLE PRECISION RADIAN, DEGRES
      PARAMETER (RADIAN=57.295779513,DEGRES=0.017453292519943295769)
      CHARACTER PRGNAM*6, HILINE*72, NAMIN*12, CLSIN*6, TYPIN*2,
     *   ATIME*8, ADATE*12, NAMES(MXSTAR)*20, SBUF*80
      HOLLERITH XNAMIN(3), XCLSIN(2), STLABL(6)
      REAL      DSKIN, SEQIN, DXPOS, DYPOS, POSANG, TRIA(3,MXTRIA),
     *   TRIB(3,MXTRIA), MINFIT, XINVER, MAXPIX, MINPIX, MINARC, MAXARC,
     *   MINDIF, MINRAT, RMSUM, RMSFIT(MXFIT2), BPARM(10), CPARM(10),
     *   DPARM(10), SA(2,MXSTAR), SB(2,MXSTAR), STRMIN(MXSTAR),
     *   ROKTRI(5,MXFIT2), RES(2,MXSTAR), MINRAD, XIN2VR, XY(2,MXSTAR),
     *   AD(2,MXSTAR), XY3(2,3), AD3(2,3), RMATCH, ESTXIN, ESTYIN,
     *   ESTINR, ESTORI, ESTORR, ESTRA, ESTRAR, ESTDEC, ESTDER, MINRA,
     *   MAXRA, MINDEC, MAXDEC
      DOUBLE PRECISION ANGLE, REFINC(2), REFVAL(2), REFPIX(2),
     *   MINANG, MAXANG, MINXIN, MAXXIN, MINYIN, MAXYIN, XPOS, YPOS
      INTEGER  IWBUFF(256), IMFIND, ITRIA(3,MXTRIA), ITRIB(3,MXTRIA),
     *   INVER, NSTA, NSTB, DEPTH(5), IFIT, ISAVE, IRMSTR(MXFIT2),
     *   IMLUN, IERR, IRETCD, ISEQ, INPRMS, ISLOT, IUSER, IVOL, ISTAR,
     *   IROUND, HLUN, IT(3), ID(3), IER, MINSTR, BSTFIT, NSTFIT,
     *   NOKFIT, IN2VER, NTRA, NTRB, IA, IB, NUMFIT, CATOLD(256), IDIST,
     *   JDIST, MAXDIS, MINDIS, MAXTRI, PLEV, ISTMIN(MXSTAR), DISINC
      LOGICAL   T, SAVE, QUICK, GOODFT, DOHEAD, DOPRT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XINVER, XIN2VR,
     *   BPARM, CPARM, DPARM
      COMMON /STRAND/ XPOS, YPOS, DXPOS, DYPOS, POSANG, STLABL
      DATA IMLUN, HLUN /16, 29/
      DATA PRGNAM /'STRAN '/
      DATA TYPIN /'  '/
      DATA T /.TRUE./, DOHEAD/.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SAVE = .TRUE.
      GOODFT = .FALSE.
      CALL FILL( 5, 1, DEPTH)
C                                       Get input values from AIPS.
      INPRMS = 39
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 0
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = NLUSER
      IN2VER = IROUND (XIN2VR)
      INVER  = MAX( 0, IROUND (XINVER))
C                                       If header should be updated
      DOHEAD = BPARM(1) .GT. 0.00001
C                                       User parameters
      MAXTRI = BPARM(2)
C                                       Print Level
      PLEV   = IROUND (BPARM(3))
C                                       set max triangles default
      IF (MAXTRI.LE.0) MAXTRI = MXTRIA
C                                       Tell user
      IF (DOHEAD) THEN
         MSGTXT = 'Image header WILL be updated'
      ELSE
         MSGTXT = 'Image header will NOT be updated'
         END IF
      CALL MSGWRT (3)
      WRITE (MSGTXT,1060) MAXTRI
      CALL MSGWRT (3)
C                                       Min and Max Trisides (pixels)
      MINPIX = ABS (CPARM(1))
      MAXPIX = ABS (CPARM(2))
      IF (MAXPIX.LE.MINPIX) MAXPIX = 1000.
C                                       Min and Max sides (arcseconds)
      MINARC = ABS (CPARM(3)) / 3600.
      MAXARC = ABS (CPARM(4)) / 3600.
      MINARC = MAX (MINARC, 1./3600.)
      IF (MAXARC.LE.MINARC) MAXARC = 10.
C                                       Minimum dif for match (Pix)
      MINRAD = ABS (CPARM(5))
C                                       Default match distance (pixels)
      IF (MINRAD.LE.0.0 .OR. MINRAD.GT.1.0E6) MINRAD = 4
C                                       Minimum Number of stars
      MINSTR = IROUND (CPARM(6))
      IF (MINSTR.LT.4) MINSTR = 7
C                                       Triangle Minimum difference
      MINDIF = ABS (CPARM(7))
      IF (MINDIF.LE.0.0) MINDIF = 0.025
C                                       Triangle Min side ratio
      MINRAT = CPARM(8)
      IF (MINRAT.LE.0.0) MINRAT = 0.8
C                                       Report inputs
      WRITE (MSGTXT,1070) MINPIX, MAXPIX, ' Pixels'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1070) MINARC*3600., MAXARC*3600., ' Arc-sec'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1080) INT(MINRAD)
      CALL MSGWRT(3)
      WRITE (MSGTXT,1090) MINDIF
      CALL MSGWRT(3)
      WRITE (MSGTXT,1100) MINRAT
      CALL MSGWRT(3)
C                                       Open map file & get header.
      CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save the header if a failure
      CALL COPY(256, CATBLK, CATOLD)
C                                       If optical probably Tangent
      CALL CHR2H (4, '-ARC', 1, CATH(KHCTP+1))
      CALL CHR2H (4, '-ARC', 1, CATH(KHCTP+3))
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
C                                       Set Match Range
      MAXDIS = MINRAD
      MINDIS = MAX ( 1, MAXDIS/2)
C                                       Try 2 different ranges
      DISINC = MAX ( MAXDIS-MINDIS, 1)
C                                       Estimated Pixel Increment +
C                                       error
      ESTXIN = DPARM(1)
      ESTINR = ABS( DPARM(2))
C                                       Default is header value
      IF (ABS(ESTXIN) .LT. 1.E-10) ESTXIN = CATR(KRCIC) * 3600.
C                                       Default accuracy is 20%
      IF (ABS(ESTINR) .LT. 1.E-10) ESTINR = ABS(ESTXIN)/5.
C                                       Min/Max inc degree/pixel
      MINXIN = (ESTXIN - ESTINR) / 3600.
      MAXXIN = (ESTXIN + ESTINR) / 3600.
C                                       Estimated Pixel Increment +
C                                       error
      ESTYIN = DPARM(3)
      ESTINR = ABS( DPARM(4))
C                                       Default is header value
      IF (ABS(ESTYIN) .LT. 1.E-10) ESTYIN = CATR(KRCIC+1)*3600.
C                                       Default accuracy is 20%
      IF (ABS(ESTINR) .LT. 1.E-10) ESTINR = ABS(ESTYIN)/5.
C                                       Min/Max inc degree/pixel
      MINYIN = (ESTYIN - ESTINR) / 3600.
      MAXYIN = (ESTYIN + ESTINR) / 3600.
C                                       Estimated Orientation
      ESTORI = DPARM(5)
      ESTORR = ABS( DPARM(6))
C                                       Default is 0. +/- 10
      IF (ESTORR.LE.0.0) ESTORR = 5
      MINANG = ESTORI - ESTORR
      MAXANG = ESTORI + ESTORR
C                                       Estimated RA
      ESTRA  = DPARM(7)
      ESTRAR = ABS( DPARM(8))
C                                       Default is header value
      IF (ESTRA  .LE. 0.0) ESTRA  = CATD(KDCRV)
      IF (ESTRAR .LE. 0.0) ESTRAR = 2.
C                                       Estimated Dec
      ESTDEC = DPARM(9)
      ESTDER = DPARM(10)
      IF (ESTDEC .EQ. 0.0) ESTDEC = CATD(KDCRV+1)
      IF (ESTDER .LE. 0.0) ESTDER = 2.
C                                       Convert to ranges
      MINRA  = ESTRA  - ESTRAR
      MAXRA  = ESTRA  + ESTRAR
      MINDEC = ESTDEC - ESTDER
      MAXDEC = ESTDEC + ESTDER
C                                       Tell User
      WRITE (MSGTXT,1110) MINXIN, MAXXIN, 'X-Increm'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1110) MINYIN, MAXYIN, 'Y-Increm'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1110) MINRA, MAXRA, 'RA'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1110) MINDEC, MAXDEC, 'DEC'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1110) MINANG, MAXANG, 'Rotation'
      CALL MSGWRT(3)
C                                       Read pixel list
      CALL STREAD (IVOL, ISLOT, INVER, MINPIX, 1.0, 1.0E5, 1.0, 1.0E5,
     *   NSTA, SA, IERR)
      IF (IERR.NE.0 .OR. NSTA.LE.2) THEN
         MSGTXT = 'ERROR READING ST TABLE WITH PIXEL INFORMATION'
         CALL MSGWRT(8)
         GO TO 980
         END IF
C                                       Read coordinate stars
      CALL STREAD (IVOL, ISLOT, IN2VER, MINARC, MINRA, MAXRA, MINDEC,
     *   MAXDEC, NSTB, SB, IERR)
      IF (IERR.NE.0 .OR. NSTB.LE.2) THEN
         MSGTXT = 'ERROR READING ST TABLE WITH COORDINATE INFO'
         CALL MSGWRT(8)
         GO TO 980
         END IF
C                                       If printing
      IF (PLEV.GT.2) THEN
C                                       Print pixel stars
         MSGTXT = 'Pixel location star list:'
         CALL MSGWRT(3)
C                                       for all pixel stars
         DO 50 IA = 1, NSTA
            WRITE (MSGTXT, 1040) IA, SA(1,IA), SA(2,IA)
            CALL MSGWRT(3)
 50         CONTINUE
         MSGTXT = 'Coordinate location star list:'
         CALL MSGWRT(3)
         DO 60 IB = 1, NSTB
            WRITE (MSGTXT, 1040) IB, SB(1,IB), SB(2,IB)
            CALL MSGWRT(3)
 60         CONTINUE
C                                       End if Printing
         END IF
C                                       Make a list of pixel triangles
      CALL TRILST( NSTA, MAXTRI, MINPIX, MAXPIX, MINRAT, SA, .FALSE.,
     *   NTRA, ITRIA, TRIA)
      WRITE (MSGTXT,1050) NTRA,'first '
      CALL MSGWRT(3)
C                                       Make a list of pixel triangles
      CALL TRILST( NSTB, MAXTRI, MINARC, MAXARC, MINRAT, SB, .TRUE.,
     *   NTRB, ITRIB, TRIB)
      WRITE (MSGTXT,1050) NTRB,'second'
      CALL MSGWRT(3)
C                                       Try to match Triangles
      REFPIX(1) = CATR(KRCRP)
      REFPIX(2) = CATR(KRCRP+1)
C                                       If Map header is non-zero`
      IF (CATD(KDCRV).NE.0.0) THEN
C                                       Use header as ref value
         REFVAL(1) = CATD(KDCRV)
         REFVAL(2) = CATD(KDCRV+1)
      ELSE
C                                       Use guess as field center
         REFVAL(1) = ESTRA
         REFVAL(2) = ESTDEC
         END IF
C                                       Init count of fits
      NOKFIT = 0
C                                       Init increment
      REFINC(1)  = CATR(KRCIC)
      REFINC(2)  = CATR(KRCIC+1)
C                                       If using header as a guess
      IF (BPARM(4).LT.1) THEN
C                                       Set default info
         NOKFIT = NOKFIT + 1
C                                       First try is image header
         ROKTRI(1,NOKFIT) = REFVAL(1)
         ROKTRI(2,NOKFIT) = REFVAL(2)
C                                       save original inc + value
         ROKTRI(3,NOKFIT) = CATR(KRCIC)
         ROKTRI(4,NOKFIT) = CATR(KRCIC+1)
         ROKTRI(5,NOKFIT) = CATR(KRCRT)
C
         MSGTXT = 'First trial fit uses image header'
         CALL MSGWRT(3)
         WRITE (MSGTXT,1190) NOKFIT, REFVAL(1), REFVAL(2)
         CALL MSGWRT(3)
         WRITE (MSGTXT,1195) NOKFIT,REFINC(1)*3600.,REFINC(2)*3600.,
     *      ANGLE
         CALL MSGWRT(3)
C                                       If estimate different
         IF (ROKTRI(1,1).NE.ESTRA .OR. ROKTRI(2,1).NE.ESTDEC .OR.
     *       ROKTRI(3,1).NE.ESTXIN/3600. .OR.
     *       ROKTRI(4,1).NE.ESTYIN/3600. .OR.
     *       ROKTRI(5,1).NE.ESTORI) THEN
            NOKFIT = NOKFIT + 1
C                                       First try is image header
            ROKTRI(1,NOKFIT) = ESTRA
            ROKTRI(2,NOKFIT) = ESTDEC
C                                       save original inc + value
            ROKTRI(3,NOKFIT) = ESTXIN /3600.
            ROKTRI(4,NOKFIT) = ESTYIN /3600.
            ROKTRI(5,NOKFIT) = ESTORI
            MSGTXT = 'Second Trial uses input estimate Parameters'
            CALL MSGWRT(3)
            WRITE (MSGTXT,1190) NOKFIT, ESTRA, ESTDEC
            CALL MSGWRT(3)
            WRITE (MSGTXT,1195) NOKFIT, ESTXIN, ESTYIN, ESTORI
            CALL MSGWRT(3)
C                                       End if estimate different
            END IF
C                                       End if not using header
         END IF
C                                       Init fit results, RMS, N stars
      CALL RFILL (MXFIT2, 1.0E20, RMSFIT)
      CALL FILL  (MXFIT2, 1,      IRMSTR)
C                                       Set Labels
      NAMES(1) = 'ONE'
      NAMES(2) = 'TWO'
      NAMES(3) = 'THR'
C                                       For all triangles in first list
      DO 200 IA = 1, NTRA
C                                       Transfer to work array
         XY3(1,1) = SA(1,ITRIA(1,IA))
         XY3(2,1) = SA(2,ITRIA(1,IA))
C                                       Pixels for star 2
         XY3(1,2) = SA(1,ITRIA(2,IA))
         XY3(2,2) = SA(2,ITRIA(2,IA))
C                                       Pixels for star 3
         XY3(1,3) = SA(1,ITRIA(3,IA))
         XY3(2,3) = SA(2,ITRIA(3,IA))
C                                       report progress
         IF ((IA/300)*300.EQ.IA) THEN
            WRITE (MSGTXT,1300) IA, NOKFIT
            CALL MSGWRT(3)
            END IF
C                                       For all triangles in second list
         DO 100 IB = 1, NTRB
C                                       IF printing
           IF (PLEV.GT.3 .AND. IA.EQ.IB) THEN
              WRITE (MSGTXT,1160) 'Pixel',IA, TRIA(1,IA),
     *               TRIA(2,IA), TRIA(3,IA)
              CALL MSGWRT(3)
              WRITE (MSGTXT,1160) 'Coord',IB, TRIB(1,IB),
     *            TRIB(2,IB), TRIB(3,IB)
              CALL MSGWRT(3)
              END IF
C                                       If triangles sides similar
           IF (ABS((TRIA(2,IA)/TRIB(2,IB))-1.) .LT. MINDIF) THEN
C                                       If other two sides similar
              IF (ABS((TRIA(3,IA)/TRIB(3,IB))-1.) .LT. MINDIF) THEN
C                                       We have a match! find transform
C                                       Transfer stars to work arrays
C                                       Coords for star 1
                 AD3(1,1) = SB(1,ITRIB(1,IB))
                 AD3(2,1) = SB(2,ITRIB(1,IB))
C                                       Coords for star 2
                 AD3(1,2) = SB(1,ITRIB(2,IB))
                 AD3(2,2) = SB(2,ITRIB(2,IB))
C                                       Coords for star 3
                 AD3(1,3) = SB(1,ITRIB(3,IB))
                 AD3(2,3) = SB(2,ITRIB(3,IB))
C                                       IF printing
                 IF (PLEV.GT.3) THEN
                    WRITE (MSGTXT,1168) IA, ITRIA(1,IA), ITRIA(2,IA),
     *                 ITRIA(3,IA)
                    CALL MSGWRT(3)
                    WRITE (MSGTXT,1168) IB, ITRIB(1,IB), ITRIB(2,IB),
     *                 ITRIB(3,IB)
                    CALL MSGWRT(3)
                    WRITE (MSGTXT,1170) XY3(1,1),XY3(2,1),
     *                  AD3(1,1), AD3(2,1)
                    CALL MSGWRT(3)
                    WRITE (MSGTXT,1170) XY3(1,2),XY3(2,2),
     *                  AD3(1,2), AD3(2,2)
                    CALL MSGWRT(3)
                    WRITE (MSGTXT,1170) XY3(1,3),XY3(2,3),
     *                  AD3(1,3), AD3(2,3)
                    CALL MSGWRT(3)
                    END IF
C                                       Refpix is center coord
                 CALL XTRFIT (3, 3, XY3, AD3, NAMES, .FALSE., REFPIX,
     *              REFVAL, REFINC, ANGLE, RES, IERR)
C                                       If angle is ok
                 IF ((ANGLE.GT.MINANG    .AND.ANGLE    .LT.MAXANG).OR.
     *               (ANGLE+180.GT.MINANG.AND.ANGLE+180.LT.MAXANG).OR.
     *               (ANGLE-180.GT.MINANG.AND.ANGLE-180.LT.MAXANG))
     *              THEN
C                                       If location is ok
                    IF (REFINC(1).GT.MINXIN .AND.
     *                  REFINC(1).LT.MAXXIN .AND.
     *                  REFINC(2).GT.MINYIN .AND.
     *                  REFINC(2).LT.MAXYIN .AND.
     *                  REFVAL(1).GT.MINRA  .AND.
     *                  REFVAL(1).LT.MAXRA  .AND.
     *                  REFVAL(2).GT.MINDEC .AND.
     *                  REFVAL(2).LT.MAXDEC) THEN
C                                       Looks like a good fit
                       NOKFIT = NOKFIT + 1
C                                       Save Transformation
                       ROKTRI(1,NOKFIT) = REFVAL(1)
                       ROKTRI(2,NOKFIT) = REFVAL(2)
                       ROKTRI(3,NOKFIT) = REFINC(1)
                       ROKTRI(4,NOKFIT) = REFINC(2)
                       ROKTRI(5,NOKFIT) = ANGLE
C                                       Tell user
                       IF (PLEV.GT.0) THEN
                          WRITE (MSGTXT,1180) IA, IB
                          CALL MSGWRT(3)
                          WRITE (MSGTXT,1190) NOKFIT,
     *                        REAL(REFVAL(1)), REAL(REFVAL(2))
                          CALL MSGWRT(3)
                          WRITE (MSGTXT,1195) NOKFIT,
     *                       REAL(REFINC(1)*3600.),
     *                       REAL(REFINC(2)*3600.), REAL(ANGLE)
                          CALL MSGWRT(3)
                          END IF
C                                       If enough good fits, exit
                       IF (NOKFIT.GE.MXFIT) GO TO 210
C                                       End if OK location
                       END IF
C                                       End if an OK angle
                    END IF
C                                       End if C sides similar
                 END IF
C                                       End if B sides similar
              END IF
C                                       End for all B triangles
 100       CONTINUE
C                                       End for all A triangles
 200     CONTINUE
C                                       Jump here on enough fits
 210  CONTINUE
C                                       Finished triangle search
      WRITE (MSGTXT,1200) NOKFIT
      CALL MSGWRT(3)
C                                       For all Distances to max
      DO 550 JDIST = MINDIS, MAXDIS, DISINC
C                                       Work from big to small distances
         IDIST = MAXDIS + MINDIS - JDIST
C                                       Tell User
         WRITE (MSGTXT,1400) IDIST
         CALL MSGWRT(3)
C                                       Test again fits found so far
         NUMFIT = NOKFIT
C                                       For all OK FITS
         DO 500 IFIT = 1, NUMFIT
C                                       Compare all fits
C                                       Set Match Distance
            RMATCH = MIN ( REAL(IDIST), MINRAD)
C                                       Put transformation in header
            REFVAL(1) = ROKTRI(1,IFIT)
            REFVAL(2) = ROKTRI(2,IFIT)
            REFINC(1) = ROKTRI(3,IFIT)
            REFINC(2) = ROKTRI(4,IFIT)
C                                       Set rotation angle
            ANGLE     = ROKTRI(5,IFIT)
            CALL STMAT (NSTA, SA, NSTB, SB, REFVAL, REFINC, REFPIX,
     *            ANGLE, STRMIN, ISTMIN)
C                                       Init count of stars in fit
            NSTFIT = 0
C                                       For All stars in coord list
            DO 400 IB = 1, NSTB
C                                       If star is close to Pixel
               IF (STRMIN(IB).LT.RMATCH) THEN
C                                       Found a star match
                  NSTFIT = NSTFIT + 1
                  IA = ISTMIN(IB)
C                                       Put pix, ra and Dec in list
                  XY(1,NSTFIT) = SA(1,IA)
                  XY(2,NSTFIT) = SA(2,IA)
                  AD(1,NSTFIT) = SB(1,IB)
                  AD(2,NSTFIT) = SB(2,IB)
                  WRITE(NAMES(NSTFIT),1350) IA
C                                       If enough stars, exit
                  IF (NSTFIT.GE.MXSTAR) GO TO 420
C                                       End if a match
                  END IF
C                                       End if a star matchs
 400           CONTINUE
C                                       Jump on enough matched stars
 420        CONTINUE
C                                       Need minimum number of stars
            IF (NSTFIT.GE.MINSTR) THEN
C                                       Watch out for repeated matchs
C                                       Reset estimates
               REFVAL(1) = ROKTRI(1,IFIT)
               REFVAL(2) = ROKTRI(2,IFIT)
C                                       Now do fit again.
               CALL XTRFIT (3, NSTFIT, XY, AD, NAMES, .FALSE.,
     *              REFPIX, REFVAL, REFINC, ANGLE, RES, IERR)
C                                       If location is still OK
               IF (REFINC(1).GT.MINXIN .AND.
     *             REFINC(1).LT.MAXXIN .AND.
     *             REFINC(2).GT.MINYIN .AND.
     *             REFINC(2).LT.MAXYIN .AND.
     *             REFVAL(1).GT.MINRA  .AND.
     *             REFVAL(1).LT.MAXRA  .AND.
     *             REFVAL(2).GT.MINDEC .AND.
     *             REFVAL(2).LT.MAXDEC) THEN
C                                       Now do fit again.
C                                       Assume few arc second RMS
                  RMSUM = 1.0E21
C                                       Best fit should have low RMS
                  IF (NSTFIT.GE.MINSTR) THEN
C                                       Else calc RMS
                     RMSUM = 0.
C                                       For all stars in fits
                     DO 430 ISTAR = 1, NSTFIT
C                                       Sum RMS for each STAR
                        RMSUM = RMSUM +
     *                     SQRT(RES(1,ISTAR)**2+RES(2,ISTAR)**2)
 430                    CONTINUE
C                                       Correct RMS for number of stars
                     RMSUM = RMSUM / (REAL(NSTFIT)-3.)
C                                       Save Transformation
                     GOODFT = .TRUE.
C                                       If enought stars to get rms
                     END IF
C                                       If fit is better
                  IF (RMSUM.LT.RMSFIT(IFIT)*0.999) THEN
C                                       Do not print again
                     DOPRT = .FALSE.
C                                       Do same fit again with printing
                     IF (PLEV.GT.0.OR.IDIST.LE.MINDIS) DOPRT=.TRUE.
C
                     CALL XTRFIT (3, NSTFIT, XY, AD, NAMES,
     *                      DOPRT, REFPIX, REFVAL, REFINC, ANGLE,
     *                      RES, IERR)
C                                       Save fit in old slot if last
                     IF (IDIST.EQ.MAXDIS .OR. NOKFIT.GE.MXFIT2) THEN
                        ISAVE = IFIT
                     ELSE
C                                       Else save fit in a new slot
                        NOKFIT = NOKFIT + 1
                        ISAVE = NOKFIT
                        END IF
                     ROKTRI(1,ISAVE) = REFVAL(1)
                     ROKTRI(2,ISAVE) = REFVAL(2)
                     ROKTRI(3,ISAVE) = REFINC(1)
                     ROKTRI(4,ISAVE) = REFINC(2)
                     ROKTRI(5,ISAVE) = ANGLE
C                                       Record RMS for fit
                     RMSFIT(ISAVE) = RMSUM
C                                       Keep track of total stars
                     IRMSTR(ISAVE) = NSTFIT
                     WRITE (MSGTXT,1500) 'Test', ISAVE, NSTFIT,
     *                     RMSUM*3600.
                     CALL MSGWRT(3)
C                                       End if Fit is better
                     END IF
C                                       End IF location still Good
                  END IF
C                                       End IF Enough Stars
               END IF
C                                       End Of for all fits loop
 500        CONTINUE
C                                       End of All distances loop
 550     CONTINUE
C                                       Init Best fit
      MINFIT = 1E20
      BSTFIT = 1
C                                       For ALL OK FITS
      DO 600 IFIT = 1, NOKFIT
C                                       IF A better fit
         IF (IRMSTR(IFIT).GE.MINSTR) THEN
            IF (MINFIT.GT.RMSFIT(IFIT)) THEN
C                                       Record RMS, FIT number
               MINFIT = RMSFIT(IFIT)
               BSTFIT = IFIT
               END IF
            END IF
C                                       End for all fits loop
 600     CONTINUE
C                                       If an OK fit
      IF (GOODFT .AND. IRMSTR(BSTFIT).GE.MINSTR) THEN
C                                       Tell User
         WRITE (MSGTXT,1500) 'Best',BSTFIT,IRMSTR(BSTFIT),
     *         RMSFIT(BSTFIT)*3600.
         CALL MSGWRT(3)
         WRITE (MSGTXT,1190) BSTFIT, ROKTRI(1,BSTFIT),
     *         ROKTRI(2,BSTFIT)
         CALL MSGWRT(3)
         WRITE (MSGTXT,1195) BSTFIT, ROKTRI(3,BSTFIT)*3600.,
     *         ROKTRI(4,BSTFIT)*3600., ROKTRI(5,BSTFIT)
         CALL MSGWRT(3)
C                                       Put results in header
         CATD(KDCRV)   = ROKTRI(1,BSTFIT)
         CATD(KDCRV+1) = ROKTRI(2,BSTFIT)
         CATR(KRCIC)   = ROKTRI(3,BSTFIT)
         CATR(KRCIC+1) = ROKTRI(4,BSTFIT)
C                                       Set rotation angle
         CATR(KRCRT)   = ROKTRI(5,BSTFIT)
         CATR(KRCRT+1) = ROKTRI(5,BSTFIT)
      ELSE
C                                       Tell user, no fit
         MSGTXT = 'No combination of stars gave a good FIT!'
         CALL MSGWRT(5)
         MSGTXT = 'Check that coordinate stars overlap with'
         CALL MSGWRT(5)
         MSGTXT = 'the pixel stars.  Also relax fit critera'
         CALL MSGWRT(5)
         END IF
C                                       If no good fit
      IF (.NOT. GOODFT .OR. .NOT. DOHEAD ) THEN
C                                       restore header
         CALL COPY ( 256, CATOLD, CATBLK)
         END IF
C                                       Add to history file
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, IVOL, ISLOT, IWBUFF, IER)
      IF (IER.NE.0) GO TO 980
C     Prepare text and add to file
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
      WRITE (SBUF,1211,ERR=215) TSKNAM, IN2VER, NSTA, ADATE, ATIME
 215  HILINE = SBUF
      CALL HIADD (HLUN, HILINE, IWBUFF, IER)
      CALL HICLOS (HLUN, T, IWBUFF, IER)
      GO TO 980
C                                       Close map file.
 980  CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, SAVE,
     *   IWBUFF, IERR)
C
 990  CALL DIETSK (IRETCD, QUICK, IWBUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
 1040 FORMAT ('Star',I5,' at ',F12.4,',',F12.4)
 1050 FORMAT ('Found',I5,' Triangles in ',A,' ST table ')
 1060 FORMAT ('Maximum number of triangles is',I6,' for the star lists')
 1080 FORMAT ('Match Stars within radius  of ',I6,' Pixels')
 1090 FORMAT ('Match Triangles with ratio differences     < ',F12.3)
 1070 FORMAT ('Triangle sides > ',F12.5,' and <',F13.5,A)
 1100 FORMAT ('Use only Triangles short/long length ratio > ',F12.3)
 1110 FORMAT ('Assuming Image > ',F12.5,' and <',F13.5,' Deg. ',A)
 1160 FORMAT (A,' Tri.',I5,' 1,2,3=',F10.4,',',F7.4,',',F7.4)
 1168 FORMAT ('Triangle ',I6,' is star numbers ',3I6)
 1170 FORMAT ('Match ',F10.4,',',F10.4,' with ',F10.4,',',F10.4)
 1180 FORMAT ('Triangles',I5,' and',I5,' Match.')
 1190 FORMAT ('Trial',I4,' RA, DEC:',1PE11.3,' ',E11.3,' degrees')
 1195 FORMAT ('Trial',I4,' Increm.:',1PE11.3,'"',E11.3,'"; Angle=',
     *         E8.1,' deg.')
 1200 FORMAT ('Finished Triangle Comparision:',I5,' Triangles Match')
 1211 FORMAT (A6,' Version=',I3,' Lines=',I5,
     *   '  / Image header updated ',A,A)
 1300 FORMAT ('Currently Comparing Triangle',I8,' (',I4,' Matches)')
 1350 FORMAT (I4)
 1400 FORMAT ('Testing Match Distance',I6,' Pixels ')
 1500 FORMAT (A,' Fit',I4,' has',I5,' Stars, RMS ',1PE9.2,'"')
      END
      SUBROUTINE TRILST( NSTARS, MXTRIA, MINSID, MAXSID, MINRAT, S,
     *   RADEC, NTRIAG, ITRI, TRI)
C-----------------------------------------------------------------------
C TRILST reads a list of stars and produces a list of triangles
C INPUT:
C    NSTARS    I   Number of stars in input list
C    MXTRIA    I   Maximum number of triangles to make
C    MINSID    R   Minimum Triangle side limit
C    MAXSID    R   Maximum Triangle side limit
C    MINRAT    R   Minimum Triangle side ratio (Reject narrow triangles)
C    S         R(2,NSTARS)  Star coordinates (pixels or degrees)
C    RADEC     L   If true, stars are RA and DEC coordinates (degrees)
C OUTPUT:
C    NTRIAG    I  Number of valid triangles found
C    ITRI      I(3, NTRIAG)  Index back into Star list
C    TRI       R(3, NTRIAG)  Lenghts of sides of triangles
C-----------------------------------------------------------------------
      INTEGER NSTARS, MXTRIA, NTRIAG, ITRI(3,*)
      LOGICAL RADEC
      REAL    MINSID, MAXSID, MINRAT, S(2,*), TRI(3,*)
C-----------------------------------------------------------------------
      INTEGER ISTAR, JSTAR, KSTAR, IORDER(3)
      REAL    SIDES(3), MXSID, MNSID, RANGE, AVE
      DOUBLE PRECISION  X0, Y0, X1, Y1, X2, Y2, DIST, SA
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Need three stars
      IF (NSTARS.LT.3 .OR. MXTRIA.LT.1) GO TO 999
C                                       Init upper/lower limit
      MXSID = MAXSID
      MNSID = MINSID
C                                       Tell User
      WRITE (MSGTXT,1000) NSTARS, MXTRIA
      CALL MSGWRT(3)
C                                       Loop here until not too many
C                                       triangles
 10   NTRIAG = 0
C                                       For all but two stars
      DO 300 ISTAR = 1, NSTARS-2
C                                       First star is the origine
C                                       If input is RA, DEC,
         X0 = S(1,ISTAR)
         Y0 = S(2,ISTAR)
C                                       for second of three stars
         DO 200 JSTAR = ISTAR+1, NSTARS-1
C                                       If input is RA, DEC,
            X1 = S(1,JSTAR)
            Y1 = S(2,JSTAR)
            IF (RADEC) THEN
               CALL ANGDIS( X0, Y0, X1, Y1, SA)
            ELSE
C                                       Else input is pixels
               SA = SQRT((X1-X0)**2  + (Y1-Y0)**2)
               END IF
C                                       If this pair is too extream
C                                       Go Get next pair
            IF (SA.LT.MNSID .OR. SA.GT.MXSID) GO TO 300
C                                       for third star
            DO 100 KSTAR = JSTAR+1, NSTARS
C                                       get third coordinates
               X2 = S(1,KSTAR)
               Y2 = S(2,KSTAR)
C                                       First side was set above
               SIDES(1) = SA
C                                       If input is RA, DEC,
               IF (RADEC) THEN
C                                       Calc other two sides
                  CALL ANGDIS( X0, Y0, X2, Y2, DIST)
                  SIDES(2) = DIST
                  CALL ANGDIS( X1, Y1, X2, Y2, DIST)
                  SIDES(3) = DIST
               ELSE
C                                       Else input is pixels
                  SIDES(2) = SQRT((X2-X0)**2  + (Y2-Y0)**2)
                  SIDES(3) = SQRT((X2-X1)**2  + (Y2-Y1)**2)
                  END IF
C                                       set side order
               IORDER(1) = ISTAR
               IORDER(2) = JSTAR
               IORDER(3) = KSTAR
C                                       sort side lengths
               IF (SIDES(1).LT.SIDES(3)) THEN
                  CALL RSWAP ( SIDES(1), SIDES(3))
                  CALL ISWAP ( IORDER(1), IORDER(3))
                  END IF
               IF (SIDES(2).LT.SIDES(3)) THEN
                  CALL RSWAP ( SIDES(2), SIDES(3))
                  CALL ISWAP ( IORDER(2), IORDER(3))
                  END IF
               IF (SIDES(1).LT.SIDES(2)) THEN
                  CALL RSWAP ( SIDES(1), SIDES(2))
                  CALL ISWAP ( IORDER(1), IORDER(2))
                  END IF
C                                       If an Not OK triangle
               IF (SIDES(1).GT.MXSID .OR. SIDES(3).LT.MNSID)
     *            GO TO 100

C                                       Now Triangle sorted A>B>C
C                                       Remove scale
               SIDES(3) = SIDES(3) / SIDES(1)
C                                       if two narrow a triangle
               IF (SIDES(3).LT.MINRAT) GO TO 100
C                                       Else an OK triangle
               SIDES(2) = SIDES(2) / SIDES(1)
               NTRIAG = NTRIAG + 1
C                                       Transfer to triangle list
               IF (NTRIAG.LT.MXTRIA) THEN
                  ITRI( 1, NTRIAG) = IORDER(1)
                  ITRI( 2, NTRIAG) = IORDER(2)
                  ITRI( 3, NTRIAG) = IORDER(3)
                  TRI ( 1, NTRIAG) = SIDES(1)
                  TRI ( 2, NTRIAG) = SIDES(2)
                  TRI ( 3, NTRIAG) = SIDES(3)
               ELSE
C                                       Else too many triangles
C                                       Limit max/min to find fewer
                  AVE   = (MXSID + MNSID)/2.
C                                       New range is half old
                  RANGE = (MXSID - MNSID)/4.
                  MNSID = AVE - RANGE
                  MXSID = AVE + RANGE
C                                       Exit when Min = Max
                  IF (MNSID.GE.MXSID*.99) THEN
                     GO TO 900
                  ELSE
C                                       Else reset count, start over
                     NTRIAG = 0
                     GO TO 10
                     END IF
                  END IF
C                                       End of all stars loops
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
 900  WRITE (MSGTXT,1100) MNSID, MXSID
      CALL MSGWRT(3)
C                                       Jump here on no more tris
 999  RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('TRILST: Using',I7,' Stars to make upto',I6,' Triangles')
1100  FORMAT ('TRILST: Triangle side length between',F12.3,' and',
     *   F12.3)
      END
      SUBROUTINE ISWAP ( I, J )
C-----------------------------------------------------------------------
C ISWAP swaps the values of two integers
C-----------------------------------------------------------------------
      INTEGER I, J
C
      INTEGER T
C-----------------------------------------------------------------------
      T = I
      I = J
      J = T
C
 999  RETURN
      END
      SUBROUTINE STREAD (IVOL, CNO, VERS, MINDIS, MINX, MAXX, MINY,
     *   MAXY, NST, S, IERR)
C-----------------------------------------------------------------------
C   STREAD Reads a ST table and returns a list of ST coordinates.
C   Table version VERS is examined.  The ST file contains the center
C   position (RA-DEC, GLON-GLAT, ELON-ELAT) of each star and the
C   "uncertainties" in those star positions.  The plotted plus signs
C   are scaled by these uncertainties and then further scaled by
C   multiplying by FACTOR.
C   Inputs:
C      VERS    I         Desired ST file version number: 0 => high
C      IVOL    I         File disk number
C      CNO     I         File catalog number
C      MINDIS  R         Minimum distance between pairs of stars
C      MINX    R         Minimum X coordinate
C      MAXX    R         Maximum X coordinate
C      MINY    R         Minimum Y coordinate
C      MAXY    R         Maximum Y coordinate
C   Output:
C      NST     I         Actual number of stars read
C      S       R(2,NST)  X, Y position of the star
C      IERR    I         Error code: 0 => okay
C                                -1 => there was no ST file
C                                +1 => logical error in ST file
C                                +2 => IO error in ST file
C   Common:
C      /MAPHDR/ CATBLK input   Image header having the ST file
C-----------------------------------------------------------------------
      INTEGER   VERS, IVOL, CNO, NST, IERR
      REAL      MINX, MAXX, MINY, MAXY, MINDIS, S(2,*)
C                                       Max Numb Columns, Label length
      INTEGER MXSTCL, MXSTLB
      REAL TWOPI
      PARAMETER (MXSTCL=7, MXSTLB=24, TWOPI=6.283185)
      CHARACTER KEYS(MXSTCL)*8
      INTEGER   IOBUF(512), IV, I, NKEY, NCOL, NREC, DATP(128,2), LRNO,
     *   TABLUN, KOLS(MXSTCL), IER, ITYPE, SCRTCH(50), IST, NSTOUT
      REAL DX, DY, DIST
      DOUBLE PRECISION XPOS, YPOS
      LOGICAL   LORDER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA TABLUN /27/
      DATA LORDER /.TRUE./
C-----------------------------------------------------------------------
C                                       Is there an ST file
      IERR = -1
      CALL FNDEXT ('ST', CATBLK, I)
C                                       If ST table not found
      IF (I.LE.0.OR.I.LT.VERS) GO TO 999
      IV = VERS
C                                       Open ST table file
      NKEY = 0
      NCOL = 0
      NREC = 0
      CALL TABINI ('READ', 'ST', IVOL, CNO, IV, CATBLK, TABLUN, NKEY,
     *   NREC, NCOL, DATP, IOBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, IV
         GO TO 990
         END IF
      NST = IOBUF(5)
C                                       Find needed columns
      CALL H2CHR (8, 1, CATH(KHCTP), KEYS(1))
      CALL H2CHR (8, 1, CATH(KHCTP+2), KEYS(2))
      KEYS(3) = 'DELT' // KEYS(1)(1:4)
      KEYS(4) = 'DELT' // KEYS(2)(1:4)
      KEYS(5) = 'POSANG'
      KEYS(6) = 'STARTYPE'
      KEYS(7) = 'LABEL'
      CALL FNDCOL (NCOL, KEYS, 8, LORDER, IOBUF, KOLS, IER)
      IF (IER.GT.0) THEN
         MSGTXT = 'STREAD: Assuming coordinates are First two '//
     *      'Columns of table'
         CALL MSGWRT(3)
         KOLS(1) = 1
         KOLS(2) = 2
         END IF

      WRITE (MSGTXT,1040) MINX, MAXX,' X coord'
      CALL MSGWRT(3)
      WRITE (MSGTXT,1040) MINY, MAXY,' Y coord'
      CALL MSGWRT(3)
C                                       Count
      NSTOUT = 0
C                                       I guess we can do it now
      LRNO = 0
      DO 100 IST = 1,NST
         CALL GETCOL (IST, KOLS(1), DATP, LRNO, IOBUF, ITYPE, XPOS,
     *      SCRTCH, IER)
         IF (IER.LT.0) GO TO 100
         IF (IER.NE.0) GO TO 975
         CALL GETCOL (IST, KOLS(2), DATP, LRNO, IOBUF, ITYPE, YPOS,
     *      SCRTCH, IER)
         IF (IER.NE.0) GO TO 975
C                                       Tell about first star
         IF (IST.EQ.1 .OR. IST.EQ.NST) THEN
            WRITE (MSGTXT, 1050) IST, XPOS, YPOS
            CALL MSGWRT(3)
            END IF
C                                       If outside range, get next
         IF (XPOS.LT.MINX .OR. XPOS.GT.MAXX .OR.
     *       YPOS.LT.MINY .OR. YPOS.GT.MAXY) GO TO 100
C                                       Get no more than needed stars
C                                       If not first star
         IF (NSTOUT.GT.0) THEN
C                                       Compare position with previous
            DX = S(1,NSTOUT) - XPOS
            DY = S(2,NSTOUT) - YPOS
            DIST = SQRT( DX**2 + DY**2)
C                                       If separation great enough
            IF (DIST.GT.MINDIS) THEN
C                                       Increase star count
               NSTOUT = NSTOUT + 1
C                                   Copy position to an array
               S(1,NSTOUT) = XPOS
               S(2,NSTOUT) = YPOS
               END IF
         ELSE
C                                   Else first star, save it
            NSTOUT = 1
            S(1,NSTOUT) = XPOS
            S(2,NSTOUT) = YPOS
            END IF
C                                   End for all stars loop
 100     CONTINUE
      IERR = 0
      NST = NSTOUT
      WRITE (MSGTXT,1100) NST, IV
      CALL MSGWRT (2)
      GO TO 985
C                                       Error print and close
 975  WRITE (MSGTXT,1975) IER
      IERR = 2
      CALL MSGWRT (8)
 985  CALL TABIO ('CLOS', 0, IST, SCRTCH, IOBUF, IER)
      GO TO 999
C                                       Error print
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('STREAD: ERROR',I5,' OPENING ST FILE VERSION',I4)
 1040 FORMAT ('STREAD: Selecting Stars >',F12.3,' and < ',F12.3,A)
 1050 FORMAT ('STREAD: Star',I8,' at position ',F13.4,',',F13.4)
 1100 FORMAT ('STREAD: Read',I8,' Positions from ST table version',I4)
 1975 FORMAT ('STREAD: I/O ERROR',I5,' IN STAR TABLE FILE')
      END
      SUBROUTINE ANGDIS (X0, Y0, X1, Y1, DIST)
C-----------------------------------------------------------------------
C Calculate the Angular Distance between two RA-Dec (GLAT-GLON) points
C INPUT:
C   X0,Y0  D   Coordinate of first  point (degrees)
C   X1,Y1  D   Coordinate of second point (degrees)
C Output:
C   DIST   D   Angular Distance between the two points (degrees)
C-----------------------------------------------------------------------
      DOUBLE PRECISION X0, Y0, X1, Y1, DIST
C-----------------------------------------------------------------------
      DOUBLE PRECISION DEGRES, COSDIS
      PARAMETER (DEGRES=0.017453292519943295769)
C-----------------------------------------------------------------------
C                                       Calc RA difference
      DIST   = (X0 - X1) * DEGRES
C
      COSDIS = ((SIN(Y0*DEGRES)*SIN(Y1*DEGRES)) +
     *          (COS(Y0*DEGRES)*COS(Y1*DEGRES)*COS(DIST)))
C                                       Distance is arc cos
      DIST   = ACOS( COSDIS) / DEGRES
C-----------------------------------------------------------------------
      RETURN
      END
      SUBROUTINE GETLOC( RA, DEC, REFVAL, REFINC, REFPIX, COSSIN, X, Y)
C-----------------------------------------------------------------------
C GETLOC get the pixel location for an input coordinate
C INPUT:
C   RA, DEC   R     RA and Dec of the star
C   REFVAL    D(2)  Reference Value of Image Header
C   REFINC    D(2)  Reference increment of image pixel
C   REFPIX    D(2)  Reference pixel of image
C   COSSIN    D(2)  Cos and Sin of image orientation
C OUTPUT:
C   X, Y      R     X and Y pixel location of star
C-----------------------------------------------------------------------
      REAL RA, DEC, X, Y
      DOUBLE PRECISION REFVAL(2), REFINC(2), REFPIX(2), COSSIN(2)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DRA, DDEC, DEGRES
      PARAMETER (DEGRES=0.017453292519943295769)
C-----------------------------------------------------------------------
C (X1 - X0) = (RA- RA0) * COS(A) * INCRA -  (DEC - DEC0) * SIN(A)*INCDEC
C (Y1 - Y0) = (RA- RA0) * SIN(A) * INCRA +  (DEC - DEC0) * SIN(A)*INCDEC
      DRA    = (RA  - REFVAL(1))*COS(REFVAL(2)*DEGRES)/REFINC(1)
      DDEC   = (DEC - REFVAL(2))/REFINC(2)
C
      X      = ( DRA*COSSIN(1)) - (DDEC*COSSIN(2))
      Y      = ( DRA*COSSIN(2)) + (DDEC*COSSIN(1))
      X      = X + REFPIX(1)
      Y      = Y + REFPIX(2)
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE STMAT (NSTA, SA, NSTB, SB, REFVAL, REFINC, REFPIX,
     *   ANGLE, STRMIN, ISTMIN)
C-----------------------------------------------------------------------
C STMAT get the pixel location for an input coordinate
C INPUT:
C   NSTA      I     Number of pixel stars
C   SA        R(2,NSTA) Pixel locations of  stars
C   NSTB      I     Number of coordinate stars
C   SB        R(2,NSTB) Coordiantes of Stars
C   REFVAL    D(2)  Reference Value of Image Header
C   REFINC    D(2)  Reference increment of image pixel
C   REFPIX    D(2)  Reference pixel of image
C   ANGLE     D(2)  orinentation angle image
C OUTPUT:
C   STRMIN    R(NSTB) Closest pixel star to coordinate star
C   ISTMIN    I(NSTB) Index to pixel star near coordiante star
C-----------------------------------------------------------------------
      INTEGER NSTA, NSTB, ISTMIN(*)
      REAL SA(2,*), SB(2,*), STRMIN(*)
      DOUBLE PRECISION REFVAL(2), REFINC(2), REFPIX(2), ANGLE
C-----------------------------------------------------------------------
      DOUBLE PRECISION DEGRES
      PARAMETER (DEGRES=0.017453292519943295769)
      INTEGER IA, IB, IC
      REAL    XPIX, YPIX, XDEL, YDEL, RDEL
      DOUBLE PRECISION COSSIN(2)
C-----------------------------------------------------------------------
C                                       Set orientation factors
      COSSIN(1) = COS(ANGLE*DEGRES)
      COSSIN(2) = SIN(ANGLE*DEGRES)
C                                       For All stars in coord list
      DO 200 IB = 1, NSTB
C                                       Calc pixel of Coords
         CALL GETLOC( SB(1,IB), SB(2,IB), REFVAL, REFINC,
     *      REFPIX, COSSIN, XPIX, YPIX)
C                                       Init Minimum distances
         STRMIN(IB) = 1.0E20
         ISTMIN(IB) = 0
C                                       Compare with pixel list
         DO 100 IA = 1, NSTA
C                                       Calc Pixel Difference
            XDEL = SA(1,IA) - XPIX
            YDEL = SA(2,IA) - YPIX
            RDEL = SQRT((XDEL*XDEL) + (YDEL*YDEL))
C                                       Is this the closest star?
            IF (RDEL.LT.STRMIN(IB)) THEN
C                                       Yes, Save distance, star
               STRMIN(IB) = RDEL
               ISTMIN(IB) = IA
               END IF
C                                       End For all stars with Coords
 100        CONTINUE
C                                       End For all Pixel Stars Loop
 200     CONTINUE
C                                       Now remove duplications
C                                       For All stars in coord list
      DO 400 IB = 1, NSTB-1
C                                       FOr all other stars c list
         DO 300 IC = IB+1, NSTB
C                                       IF the same star
            IF (ISTMIN(IB).EQ.ISTMIN(IC)) THEN
C                                       if first is closer
               IF (STRMIN(IB).LT.STRMIN(IC)) THEN
C                                       ignore second
                  STRMIN(IC) = 1.E20
               ELSE
C                                       else ignore first star
                  STRMIN(IB) = 1.E20
                  GO TO 400
                  END IF
C                                       End if Same star
               END IF
 300        CONTINUE
 400     CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
