      PROGRAM IMLHS
C-----------------------------------------------------------------------
C! Makes false color display on TV of up to 3 images
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-1999, 2002, 2009
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
C   IMLHS uses up to 3 maps to create a false (but not pseudo) color
C   image on the TV.  It uses the first map to modulate the brightness
C   of the image, the 2nd to modulate the hue, and the 3rd to modulate
C   the saturation.  If any of the INNAMEs are missing, the corres-
C   ponding parameter is set to a constant.
C   Inputs: adverbs from AIPS -
C   USERID             Input map user ID
C   INNAME(3)          Input map name
C   INCLASS(2)         Input map class
C   INSEQ              First input map seq no.
C   INDISK             Input map disk: 0 = any
C   IN2NAME(3)         IN2put map name default INNAME
C   IN2CLASS(2)        IN2put map class default INCLASS
C   IN2SEQ             IN2put map seq no. 0 => lowest unique
C   IN2DISK            IN2put map disk
C   IN3NAME(3)         Saturation map name
C   IN3CLASS(2)        Saturation map class
C   IN3SEQ             Saturation map seq. no.
C   IN3DISK            Saturation map disk
C   BLC(7)             Bottom left corner all input maps
C   TRC(7)             Top right corner all input maps
C   TVCORN(2)          Image corners on TV
C   FUNCTYPE           'LG'=logrithmic, otherwise linear
C   APARM(10)           ?
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMS(3)*36, ONCODE(2)*4, PRGNAM*6, MTYPE(3)*12,
     *   SUBS(10)*8, CHTM4*4
      HOLLERITH XMA
      REAL      RPARM(46), IN(MAXIMG,3), IHDR4(256),DMAX(3), DMIN(3),
     *   PIXR(2,3), BLC(7), TRC(7), TVCORN(2), TVC(2), SCALE(3), OFF(3),
     *   XRANGE(2,3), IMDEF(3), HUE0, HUEFUN, SATFUN, XIFUN
      LOGICAL   LLOG, LSQRT, ISMAP(3)
      INTEGER   ERROR, NSUB, IROUND, IHDR(256), NRET, EOM, LUNSL(3),
     *   IERR, IYTV, OUT(MAXIMG), IPL(3), NBYTES, NLINES, IM, IP, IX,
     *   ID, IXTV, NPIX, ICOLOR, NPARM
      DOUBLE PRECISION IHDR8(128)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DBUF.INC'
      EQUIVALENCE (IHDR, IHDR4, IHDR8)
      EQUIVALENCE (BLC, RPARM(23)),       (TRC, RPARM(30))
      EQUIVALENCE (TVCORN, RPARM(37)),    (PIXR, RPARM(40))
      EQUIVALENCE (HUE0, RPARM(46))
      EQUIVALENCE (OUT, RBUF(6145))
      DATA PRGNAM /'IMLHS '/
      DATA LUNSL /18,19,20/
      DATA ONCODE /'ONNN', 'OFFF'/
      DATA MTYPE /'INTENSITY   ', '         HUE', 'SATURATION  '/
      DATA IMDEF /1., 1., 1./
      DATA IPL /1, 2, 3/
      DATA EOM /9/
      DATA SUBS /'GTPARM  ', 'OPENCF  ',
     *   'GETHDR  ', 'MAPIO   ', 'WINDOW  ', 'TVOPEN  ', 'MAPWIN  ',
     *   'MINIT   ', 'MDISK   ', 'YIMGIO  '/
C-----------------------------------------------------------------------
C                                       Task init, get parms
      NRET = 0
C                                       Fixed PPM 1996.09.30: was 46
      NPARM = 50
      NSUB = 1
      CALL TSKBEG (PRGNAM, NPARM, RPARM(2), ERROR)
      IF (ERROR.NE.0) GO TO 980
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
      RPARM(1) = NLUSER
C                                       Set up file NAMEstrings
      CALL CHR2H (4, 'MA  ', 1, XMA)
      DO 10 IM = 1,3
         CALL H2WAWA (RPARM(7*IM-5), RPARM(7*IM-2), RPARM(7*IM),
     *      XMA,  RPARM(7*IM+1), RPARM(1), NAMS(IM))
C                                       Does this input map exist?
         ISMAP(IM) = NAMS(IM)(1:12) .EQ. '            '
         IF (ISMAP(IM)) GO TO 10
            NSUB = 2
            CALL OPENCF (LUNSL(IM), NAMS(IM), ERROR)
            IF (ERROR.NE.0) GO TO 980
            NSUB = 3
            CALL GETHDR (LUNSL(IM), IHDR, ERROR)
            IF (ERROR.NE.0) GO TO 980
            DMAX(IM) = IHDR4(KRDMX)
            DMIN(IM) = IHDR4(KRDMN)
 10      CONTINUE
C                                      Open TV
      NSUB = 6
      CALL TVOPEN (IN, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL YINIT (IN, ERROR)
C                                      Set up TV colors
      DO 20 IP = 1,NGRAY
         CALL YSLECT (ONCODE(2), IP, 0, OUT, ERROR)
 20      CONTINUE
      DO 30 IP = 1,3
         CALL YSLECT (ONCODE(1), IP, IP, OUT, ERROR)
 30      CONTINUE
      DO 40 IM = 1,3
         IF (ISMAP(IM)) GO TO 40
            WRITE (MSGTXT,1040) MTYPE(IM)
            CALL MSGWRT (3)
            CALL PRTNAM (NAMS(IM), 3)
 40      CONTINUE
C                                Rationalize window
      CALL WINDOW (IHDR(KIDIM), IHDR(KINAX), BLC, TRC, ERROR)
C                                Adjust window so image fits screen
      DO 50 ID = 1,2
         TVC(ID) = TVCORN(ID)
         IF ((TVC(ID).LE.0.) .OR. (TVC(ID).GT.MAXXTV(ID)))
     *      TVC(ID) = 1.
         BLC(ID) = MAX (BLC(ID), 0.5*(BLC(ID)+TRC(ID)-MAXXTV(ID)
     *      +TVC(ID)))
         TRC(ID) = MIN (TRC(ID), BLC(ID)+MAXXTV(ID)-TVC(ID))
C                                 If TVC=0 center image
         IF ((TVCORN(ID).GT.0.) .AND. (TVCORN(ID).LT.MAXXTV(ID)))
     *      GO TO 50
         TVC(ID) = 1. + 0.5 * (MAXXTV(ID) - 1. + BLC(ID) - TRC(ID))
 50      CONTINUE
      IYTV = TVC(2)
      IXTV = TVC(1)
C                                    Set windows internally
      NSUB = 7
      DO 70 IM = 1,3
         IF (ISMAP(IM)) GO TO 70
            CALL MAPWIN (LUNSL(IM), BLC, TRC, ERROR)
            IF (ERROR.NE.0) GO TO 980
 70      CONTINUE
      NPIX = IROUND (1.+TRC(1)-BLC(1))
      CALL H2CHR (4, 1, RPARM(39), CHTM4)
      LLOG = CHTM4(1:2) .EQ. 'LG'
      LSQRT = CHTM4(1:2) .EQ. 'SQ'
C                                    Set scale factors
      DO 80 IM = 1,3
         IF (ISMAP(IM)) GO TO 80
            CALL RNGSET (PIXR(1,IM), DMAX(IM), DMIN(IM), XRANGE(1,IM))
            SCALE(IM) = 1./(XRANGE(2,IM) - XRANGE(1,IM))
            OFF(IM)   = -XRANGE(1,IM)
 80      CONTINUE
C                                  Start TV output
      NLINES = 3 * IROUND (1. + TRC(2) - BLC(2))
      NBYTES = 8192
      NSUB = 8
C                                   Read loop for maps
 200  CONTINUE
         DO 250 IM = 1,3
            IF (ISMAP(IM)) GO TO 230
               NSUB = 4
               CALL MAPIO ('READ', LUNSL(IM), IN(1,IM), ERROR)
C                                   Test on end of map
               IF (ERROR.EQ.EOM) GO TO 290
               IF (ERROR.NE.0) GO TO 980
C                                    Scale Lum and Hue and Sat
               DO 210 IX = 1,NPIX
                  IN(IX,IM) = SCALE(IM) * (IN(IX,IM) + OFF(IM))
 210              CONTINUE
               GO TO 250
C                                      Fill in default if no map
 230        CONTINUE
               DO 240 IX = 1,NPIX
                  IN(IX,IM) = IMDEF(IM)
 240              CONTINUE
 250        CONTINUE
C                                     Offset hue if desired
         IF (HUE0.EQ.0.) GO TO 265
            DO 260 IX = 1,NPIX
               IN(IX,2) = IN(IX,2) + HUE0
 260           CONTINUE
 265     DO 280 ICOLOR = 1,3
            NSUB = 10
            DO 270 IX = 1,NPIX
               IN(IX,2) = IN(IX,2) - 0.33333
               HUEFUN = IN(IX,2)
               HUEFUN = HUEFUN - INT(HUEFUN)
               IF (HUEFUN.LT.0.) HUEFUN = 1. + HUEFUN
               HUEFUN = MIN (1., MAX (0., 1.-3.* ABS(0.33333-HUEFUN)))
               SATFUN = MIN (1., MAX (0., IN(IX,3)))
               HUEFUN = SQRT (SATFUN * HUEFUN + 0.33333 * (1. - SATFUN))
C                                   SQRT simulates gamma correction
C                                   should be **1/gamma = **0.4
               XIFUN = MAX (0., MIN (1., IN(IX, 1)))
               IF (LLOG) XIFUN = LOG10 (1. + 9. * XIFUN)
               IF (LSQRT) XIFUN = SQRT (XIFUN)
               OUT(IX) = MAXINT * HUEFUN * XIFUN
 270           CONTINUE
            CALL YIMGIO ('WRIT', IPL(ICOLOR), IXTV, IYTV, 0, NPIX,
     *         OUT, ERROR)
            IF (ERROR.NE.0) GO TO 980
 280        CONTINUE
         IYTV = IYTV + 1
         GO TO 200
C                                       Close files
 290  IF (ERROR.EQ.EOM) ERROR = 0
      DO 400 IM = 1,3
         CALL FILCLS (LUNSL(IM))
 400     CONTINUE
      CALL TVCLOS (IN, IERR)
C                                       Report any errors
      IF (ERROR.EQ.0) ERROR = IERR
      IF (ERROR.EQ.0) GO TO 990
C                                       Print error message
 980  WRITE (MSGTXT,1980) SUBS(NSUB), ERROR
      CALL MSGWRT (8)
C                                       Print offending file
      CALL PRTNAM (NAMS(IM), 8)
      NRET = 16
C                                       go home
 990  CALL TSKEND (NRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1040 FORMAT (A12, ' MAP')
 1980 FORMAT (A6,' ERROR NO ',I6)
 4000 FORMAT ('You are using a non-standard program')
         END
