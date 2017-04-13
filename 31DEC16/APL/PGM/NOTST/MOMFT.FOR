LOCAL INCLUDE 'MOMFT.INC'
      INTEGER           MAXMOM
      PARAMETER        (MAXMOM=3)
      DOUBLE PRECISION  PI180
      PARAMETER        (PI180 = 57.2957795D0)
      CHARACTER         ERRTXT*40, OUTFIL*48
      COMMON /INPARM/   OUTFIL, ERRTXT
LOCAL END
      PROGRAM MOMFT
C-----------------------------------------------------------------------
C! Determines components using moments
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009
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
C   MOMFT determines the parameters of a component by calculating the
C   zero, first and second moments associated with a part of an image.
C   The moments of the image are compared with that of the beam to
C   obtain the gaussian sizes of the component.
C   INPUTS:
C      INNAME   R(3)               Image name (name)
C      INCLASS  R(2)               Image name (class)
C      INSEQ    R                  Image name (seq. #)
C      INDISK   R                  Image disk unit #
C      IN2NAME  R(3)               Beam image name (name)
C      IN2CLASS R(2)               Beam image name (class)
C      IN2SEQ   R                  Beam image name (seq. #)
C      IN2DISK                     Beam image disk unit #
C      USERID   R                  User ID. 0 = current user
C                                      32000=any user
C      BLC      R(7)               Bottom left corner of image
C                                      0=>entire image
C      TRC      R(7)               Top right corner of images
C                                      0=>entire image
C      NOISE    R                  Noise level in the map to exclude
C-----------------------------------------------------------------------
      CHARACTER PREFIX*5, NAMS*36, MAPSRC*12, BEMNAM*12, DECNAM*12,
     *   PRGNAM*6, CHTEMP*8, ARRAY*20, LABEL*11, STAT*4, NAME*12,
     *   CLASS*12, NAME2*12, CLASS2*6, SAVRA*20, SAVDEC*20
      HOLLERITH MA
      INCLUDE 'MOMFT.INC'
      INTEGER  NPARM, ERROR, LUN, ISEQ, IVOL, SEQ, VOL, CNO, I,
     *   DEPTH(5), IROUND, IRET, ILEN, ICH, IMOM, CATREC(2), LOCS(2),
     *   ITIME(6), VALUES(5),K EYTYP(2), SCRTCH(256), IERR,
     *   TXLUN, TXFIND, LENOUT, ITRIM
      LOGICAL   LCH, LCOMP, TXOPEN, APPEND
      REAL      RPARM(41), X(MAXMOM), Y(MAXMOM), BLC(7), TRC(7), RX(2),
     *   CX(2), DXY(2), DXDY, UNFWHP, WERK(3), S, C, NOISE, EXTENT(3),
     *   EXTERR(3), ANGLE, POLI, POLA, POSERR, BLANK, USER
      DOUBLE PRECISION SKYPOS(3), MXY(MAXMOM,6), BXY(MAXMOM,6), FLUX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA PRGNAM /'MOMFT '/
      DATA DECNAM /'DECONVOLVED '/
      DATA MAPSRC /'MAP SOURCE  '/, BEMNAM /'BEAM        '/
      DATA DEPTH /5*0/, IRET/0/, LUN,TXLUN /17,10/, KEYTYP/2,2/
      DATA POLA,POLI /0.0,0.0/
      DATA TXOPEN, LCOMP, APPEND/.FALSE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
      UNFWHP = 1./(8.0 * LOG (2.0))
C                                          Get NPARM inputs
      NPARM = 41
      CALL TSKBEG (PRGNAM, NPARM, RPARM, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'TSKBEG: TASK PARAMTERS ERROR'
         GO TO 980
         END IF
C                                          Warn user
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
      CALL CHR2H (4, 'MA  ', 1, MA)
      USER = NLUSER
      CALL H2WAWA (RPARM(1), RPARM(4), RPARM(6), MA, RPARM(7),
     *   USER, NAMS)
C                                          Open up map file
      CALL PRTNAM (NAMS, 3)
      CALL OPENCF (LUN, NAMS, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'OPEN MAP FILE ERROR'
         GO TO 980
         END IF
      CALL GETHDR (LUN, CATBLK, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'GET MAP HEADER ERROR'
         GO TO 980
         END IF
C                                          Init magic blank value
      BLANK = CATR(KRBLK)
C                                          Get noise clip level
      NOISE = ABS (RPARM(29))
C                                       get output file name
      CALL H2CHR (48, 1, RPARM(30), OUTFIL)
C                                       Open output file
      LENOUT = ITRIM (OUTFIL)
C                                       If no file, do not write
      IF (LENOUT.LE.0) THEN
         TXLUN = 0
      ELSE
         CALL ZTXOPN ('WRIT', TXLUN, TXFIND, OUTFIL(1:LENOUT),
     *      APPEND, IERR)
C                                       Flag file open for later close
         IF (IERR.EQ.0) THEN
            TXOPEN = .TRUE.
C                                       Else report open error
         ELSE
            IRET = 4
            WRITE (MSGTXT,1128) IERR
            CALL MSGWRT (6)
C                                       Open error, no file write
            TXLUN = 0
            END IF
         END IF
C                                          Prepare to Get catalog num.
      CNO = 1
      CALL H2CHR (12, 1, RPARM(1), NAME)
      CALL H2CHR (6, 1, RPARM(4), CLASS)
      ISEQ = IROUND (RPARM(6))
      IVOL = IROUND (RPARM(7))
      SEQ  = ISEQ
      VOL  = IVOL
C                                          Get catalog number
      CALL CATDIR ('SRCH', VOL, CNO, NAME, CLASS,
     *   SEQ, MA, IROUND(RPARM(15)), STAT, WBUFF, ERROR)
C                                          Get coordinate increments
      DXY(1) = ABS (CATR(KRCIC)  ) * 3600.
      DXY(2) = ABS (CATR(KRCIC+1)) * 3600.
      DXDY   = DXY(1) * DXY(2)
C                                          Get BLC and TRC
      CALL RCOPY( 7, RPARM(15), BLC(1))
      CALL RCOPY( 7, RPARM(22), TRC(1))
C                                          If noise is not set
      IF (NOISE.LT.1.0E-9) THEN
C                                          Check if noise is in header
         CALL CATKEY ('REED', VOL, CNO, 'RMSNOISE', 1, LOCS,
     *      VALUES, KEYTYP, SCRTCH, IERR)
         IF (IERR.EQ.0) CALL COPY( 1, VALUES(LOCS(1)), NOISE)
         END IF

      WRITE (MSGTXT,1010) NOISE
      IF (NOISE.GT.0.0) CALL MSGWRT (3)
C                                          Take the moments
      CALL GETMOM (LUN, BLC, TRC, NOISE, BLANK, MXY, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'GET MOMENT ERROR'
         GO TO 980
         END IF
      CALL FILCLS (LUN)
C                                          Reduce moments to centroid
C                                          and normalize
      CALL REDMOM (MXY)
C                                          for all types of moments
      DO 700 IMOM = 1, MAXMOM
C                                           Multiply by increments
         MXY(IMOM,1) = MXY(IMOM,1) * DXDY
         MXY(IMOM,4) = MXY(IMOM,4) * DXY(1) ** 2
         MXY(IMOM,5) = MXY(IMOM,5) * DXY(2) ** 2
         MXY(IMOM,6) = MXY(IMOM,6) * DXDY
C                                       If "clean" map get clean beam
C                                      Use dirty beam for dirty or resid
         IF ((CATBLK(KINIT).GT.0) .AND. (CATBLK(KITYP).NE.3)) THEN
            LCOMP = (CATBLK(KITYP).EQ.2)
            IF (.NOT.LCOMP) THEN
               WERK(1) = CATR(KRBMJ) * 3600.
               WERK(2) = CATR(KRBMN) * 3600.
               WERK(3) = CATR(KRBPA) / PI180
C                                       Area under clean beam
C                                       1.1331=E^(1/8)
               BXY(IMOM,1) = 1.133148 * WERK(1) * WERK(2)
               S = SIN(WERK(3))
               C = COS(WERK(3))
C                                       Moments of clean beam
               BXY(IMOM,4) = C**2 * WERK(2)**2 + S**2 * WERK(1)**2
               BXY(IMOM,5) = S**2 * WERK(2)**2 + C**2 * WERK(1)**2
               BXY(IMOM,6) = (WERK(2)**2 - WERK(1)**2) * S * C
C                                       Convert to moments
C                                       instead of FWHP
               BXY(IMOM,4) = UNFWHP * BXY(IMOM,4)
               BXY(IMOM,5) = UNFWHP * BXY(IMOM,5)
               BXY(IMOM,6) = UNFWHP * BXY(IMOM,6)
               END IF
         ELSE
C                                       Find dirty beam & get moments
C                                       If no name2 use name1
            CALL H2CHR (12, 1, RPARM(8), NAME2)
            CALL H2CHR (6, 1, RPARM(11), CLASS2)
            IF (NAME2.EQ.'            ') NAME2 = NAME
C                                       If no class2 use IBEM
            IF (CLASS2.EQ.'      ') CLASS2 = 'IBEM  '
            IF (RPARM(13).NE.0.0) ISEQ = IROUND (RPARM(13))
            IF (RPARM(14).NE.0.0) IVOL = IROUND (RPARM(14))
            CALL A2WAWA (NAME2, CLASS2, ISEQ, 'MA', IVOL, NLUSER, NAMS)
            WRITE (MSGTXT,1001)
            CALL MSGWRT (3)
            CALL PRTNAM (NAMS, 3)
            CALL OPENCF (LUN, NAMS, ERROR)
            IF (ERROR.NE.0) THEN
               ERRTXT = 'OPEN BEAM FILE ERROR'
               GO TO 980
               END IF
            CALL GETHDR (LUN, CATBLK, ERROR)
            IF (ERROR.NE.0) THEN
               ERRTXT = 'GET BEAM HEADER ERROR'
               GO TO 980
               END IF
C                                       Find beam center, scale
            DO 510 I = 1,2
               CX(I) = CATR(KRCRP+I-1)
               RX(I) = DXY(I) / (3600. * ABS (CATR(KRCIC+I-1)))
               WERK(I) = TRC(I) - BLC(I)
               BLC(I) = CX(I) - 0.5 * WERK(I) * RX(I)
               BLC(I) = IROUND(BLC(I))
               TRC(I) = IROUND(BLC(I) + RX(I) * WERK(I))
 510           CONTINUE
            WRITE (MSGTXT,3301) BLC
            CALL MSGWRT (5)
            WRITE (MSGTXT,3301) TRC
            CALL MSGWRT (5)
            CALL GETMOM (LUN, BLC, TRC, NOISE, BLANK, BXY, ERROR)
            CALL FILCLS(LUN)
            WRITE (MSGTXT,1200) (BXY(IMOM,I),I=1,6)
C            CALL MSGWRT (5)
            CALL REDMOM (BXY)
            WRITE (MSGTXT,3000) (BXY(IMOM,I),I=1,6)
C            CALL MSGWRT (5)
            DXY(1) = DXY(1) / RX(1)
            DXY(2) = DXY(2) / RX(2)
            DXDY   = DXY(1) * DXY(2)
            BXY(IMOM,1) = BXY(IMOM,1) * DXDY
            BXY(IMOM,4) = BXY(IMOM,4) * DXY(1) ** 2
            BXY(IMOM,5) = BXY(IMOM,5) * DXY(2) ** 2
            BXY(IMOM,6) = BXY(IMOM,6) * DXDY
            END IF

C                                       If clean beam, Report once
         IF (.NOT.LCOMP.AND.IMOM.EQ.1) THEN
C                                       Report Beam
            CALL MOMGAU (IMOM, BXY, WERK)
            WRITE (MSGTXT,2003) BEMNAM, WERK
C                                       Write to screen and log file
            CALL PWRITE (5, TXLUN, TXFIND)
            END IF
C                                       Tell sum of pixels
         WRITE (MSGTXT,2000) IMOM, MXY(IMOM,1)
         CALL MSGWRT (5)
C                                       answers
         X(IMOM) = MXY(IMOM,2)
         Y(IMOM) = MXY(IMOM,3)
         WRITE (MSGTXT,2001) X(IMOM), Y(IMOM)
         CALL PWRITE (5, TXLUN, TXFIND)
C                                       set up common for coord. trans.
         DEPTH(1) = X(IMOM)
         DEPTH(2) = Y(IMOM)
C                                       print coords in nice format
         LOCNUM = 1
         CALL SETLOC (DEPTH, .TRUE.)
         CALL XYVAL (X(IMOM), Y(IMOM), SKYPOS(1), SKYPOS(2), SKYPOS(3),
     *      ERROR)
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) CALL AXSTRN
     *      (CTYP(3,LOCNUM), SKYPOS(3), KLOCA(LOCNUM), NCHLAB(1,LOCNUM),
     *      SAXLAB(1,LOCNUM))
         MSGTXT = 'Skypos: '
         ICH = 9
         DO 600 I = 1,2
            CALL AXSTRN (CTYP(I,LOCNUM), SKYPOS(I), I-1, ILEN, ARRAY)
            MSGTXT(ICH:ICH+ILEN-1) = ARRAY(1:ILEN)
C                                       save MOMENT 2 positions
            IF ((IMOM.EQ.2) .AND. (I.EQ.1)) SAVRA  = ARRAY
            IF ((IMOM.EQ.2) .AND.( I.EQ.2)) SAVDEC = ARRAY
            ICH = ICH + ILEN
            MSGTXT(ICH:ICH+1) = '  '
            ICH = ICH + 2
 600        CONTINUE
         CALL PWRITE (5, TXLUN, TXFIND)
C                                       Convert moments to gaussians
         CALL MOMGAU (IMOM, MXY, WERK)
         WRITE (MSGTXT,2003) MAPSRC, WERK
         CALL PWRITE (5, TXLUN, TXFIND)
C                                       Using a clean beam
         IF (.NOT.LCOMP) THEN
C                                       Deconvolve = subtract beam
            DO 610 I = 4,6
               MXY(IMOM,I) = MXY(IMOM,I) - BXY(IMOM,I)
 610           CONTINUE
C                                       convert to flux per beam
            MXY(IMOM,1) = MXY(IMOM,1) / BXY(IMOM,1)
C                                       if not first moment set
            IF (IMOM.GT.1) THEN
C                                       incorperate integral factor
               MXY(IMOM,1) = DBLE(IMOM) * MXY(IMOM,1)
               FLUX      = ABS(MXY(IMOM,1))
C                                       take square or cube root
               FLUX      = LOG(FLUX) / DBLE(IMOM)
               MXY(IMOM,1) = SIGN( EXP(FLUX), MXY(IMOM,1))
               END IF
C                                       scale jy to milli-jy etc.
            CALL METSCA (REAL(MXY(IMOM,1)), PREFIX, LCH)
            CALL H2CHR (8, 1, CATH(KHBUN), CHTEMP)
            CALL MOMGAU (IMOM, MXY, WERK)
            WRITE (MSGTXT,2003) DECNAM, WERK
            CALL PWRITE (5, TXLUN, TXFIND)
C                                       First moment is extended flux
            IF (IMOM.EQ.1) THEN
               WRITE (MSGTXT,2004) MXY(IMOM,1), PREFIX, CHTEMP
            ELSE
               WRITE (MSGTXT,2005) MXY(IMOM,1), PREFIX, CHTEMP
               END IF
            CALL PWRITE (5, TXLUN, TXFIND)
C                                       Save size of second moment
            IF (IMOM.EQ.2) CALL RCOPY(3, WERK, EXTENT)
            END IF
         MSGTXT = '---------------------------------------------'
         CALL MSGWRT(3)
C                                       End of for all moments loop
 700     CONTINUE
C                                       IF 2nd moment is 0, use 3rd
      IF (EXTENT(1) .EQ. 0.0) THEN
         CALL RCOPY(3, WERK, EXTENT)
C                                       reset for error calc.
         CALL RFILL(3, 0.0, WERK)
         END IF
C                                       Estimate errors
      EXTERR(1) = MAX(ABS(EXTENT(1) - WERK(1)),ABS(DXY(1))/4.0)
      EXTERR(2) = MAX(ABS(EXTENT(2) - WERK(2)),ABS(DXY(2))/4.0)
C                                       get angle difference mod pi/2
      ANGLE = MOD(EXTENT(3) + 180.0,180.0)
      WERK(3) = MOD(WERK(3) + 180.0,180.0)
      EXTERR(3) = ABS(ANGLE - WERK(3))
C                                       Get current date
      CALL CATIME(1, CATREC, ITIME)
      LABEL=SAVRA(4:14)
C                                       Make NAMS easier to read
      DO 810 I = 1,36
         IF (NAMS(I:I).EQ.' ') NAMS(I:I) = '.'
 810     CONTINUE
C                                       Write name, fluxes
      WRITE(MSGTXT,3400) LABEL,NAMS(1:29), ITIME(1), ITIME(2),
     *   ITIME(3)
      CALL PWRITE (-5, TXLUN, TXFIND)
C                                       Estimate error from difference
      POSERR = SQRT(((X(2)-X(3))*DXY(1))**2 + ((Y(2)-Y(3))*DXY(2))**2)
C                                       Error not smaller than pixel/4
      POSERR = MAX(POSERR, ABS(DXY(1))/4.0)
      POSERR = MAX(POSERR, ABS(DXY(2))/4.0)
C                                       Write Ra, Dec, Error
      WRITE(MSGTXT,3500) LABEL,SAVRA(4:16),SAVDEC(4:16),POSERR
      CALL PWRITE (-5, TXLUN, TXFIND)
      WRITE(MSGTXT,3550) LABEL,MXY(1,1), MXY(2,1), NOISE
      CALL PWRITE (-5, TXLUN, TXFIND)
      WRITE(MSGTXT,3600) LABEL,EXTENT
      CALL PWRITE (-5, TXLUN, TXFIND)
      WRITE(MSGTXT,3700) LABEL,EXTERR
      CALL PWRITE (-5, TXLUN, TXFIND)
C                                       Try to read Polarization info
C                                       Check if polIflux in header
      CALL CATKEY ('REED', VOL, CNO, 'POLIFLUX', 1, LOCS,
     *   VALUES, KEYTYP, SCRTCH, IERR)
      IF (IERR.EQ.0) CALL COPY( 1, VALUES(LOCS(1)), POLI)
C                                       Check if pol angle in header
      CALL CATKEY ('REED', VOL, CNO, 'POLANGLE', 1, LOCS,
     *   VALUES, KEYTYP, SCRTCH, IERR)
      IF (IERR.EQ.0) CALL COPY( 1, VALUES(LOCS(1)), POLA)
C                                       If read, print
      IF (IERR.EQ.0 .AND.(POLI.NE.0.0 .OR. POLA.NE.0.0)) THEN
         WRITE(MSGTXT,3800,ERR=999) LABEL, POLI, POLA
         CALL PWRITE (-5, TXLUN, TXFIND)
         END IF
C                                       Error
 980  IF (ERROR.NE.0) THEN
         WRITE (MSGTXT,1980) ERRTXT, ERROR
         CALL PWRITE (8, TXLUN, TXFIND)
         IRET = 16
         END IF
C                                       End the task
      IF (TXOPEN) CALL ZTXCLS( TXLUN, TXFIND, IERR)
      CALL TSKEND (IRET)
 999  STOP
C-----------------------------------------------------------------------
 1001 FORMAT ('Reading Beam file')
 1010 FORMAT ('Excluding pixels fainter than ABS(',1PE10.3,')')
 1128 FORMAT ('ERROR ON OUTPUT FILE OPEN: ERR=',I5)
 1200 FORMAT ('Moments:',6E9.2)
 2000 FORMAT ('Moment ',I4,', Sum of pixels: ',1PE11.3)
 2001 FORMAT ('Source centroid Pixel: ',2F11.2)
 2003 FORMAT (A12,' extent',F11.4,'" by',F11.4,'" at PA',F7.2,' deg')
 2004 FORMAT ('Integrated flux:',F10.3,1X,A5,1X,A8)
 2005 FORMAT ('Compact    flux:',F10.3,1X,A5,1X,A8)
 1980 FORMAT (A,', ERROR = ',I6)
 3000 FORMAT ('REDMOM :',6E9.2)
 3400 FORMAT (A,'1 ',A,'& ',I2,'-',I2,'-',I2,'  &%1')
 3500 FORMAT (A,'2 ',A,'  &',A,'&',F10.3,' &%2')
 3550 FORMAT (A,'3 ','FLUX',F11.3, '& ',F11.3,'&',F12.4,'&%3')
 3600 FORMAT (A,'4 ','SIZE',F10.2,' & ',F10.2,' & ',F9.2,'  &%4')
 3700 FORMAT (A,'5 ','+/- ',F10.2,' & ',F10.2,' & ',F9.2,'  &%5')
 3800 FORMAT (A,'6 ','POLI',F11.3, '& ',F10.2,' \\','\\ %6')
 3301 FORMAT ('B', 7E11.4)
 4000 FORMAT ('YOU ARE USING AN EXTREMELY NON-STANDARD PROGRAM')
      END
      SUBROUTINE GETMOM (LUN, BLC, TRC, NOISE, BLANK, MOMS, ERROR)
C-----------------------------------------------------------------------
C   Get all 0th - 2nd order moments in a rectangle in a given open map
C   relative to middle of rectangle
C   INPUTS:
C      LUN        I     Logical Unit No. of map
C      BLC(7)     R     Lower left corner of rectangle
C      TRC(7)     R     Upper right corner
C      NOISE      R     Noise level for pixel exclusion
C      BLANK      R     Blanking value for image
C   OUTPUTS:
C      MOMS(3,6)  R     Moments (1,x,xx,y,yy,xy)*Jy
C                       Moments (1,x,xx,y,yy,xy)*Jy**2
C                       Moments (1,x,xx,y,yy,xy)*Jy**3
C      ERROR      I     error from mapio, if any
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), NOISE, BLANK
      INCLUDE 'MOMFT.INC'
      DOUBLE PRECISION MOMS(MAXMOM,6)
      INTEGER   LUN, ERROR
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IX, IY, I, NYW, NXW, WIN(4)
      REAL      IN(MAXIMG), DX, DY, S, ABSS
      DOUBLE PRECISION XY(6), S2, COUNT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      CALL RFILL (6*MAXMOM, 0.0, MOMS)
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'WINDOW ERROR'
         GO TO 980
         END IF
C                                       Copy other BLCs to TRC
      CALL RCOPY ( 5, BLC(3), TRC(3))
      CALL MAPWIN (LUN, BLC, TRC, ERROR)
      IF (ERROR.NE.0) THEN
         ERRTXT = 'MAPWIN ERROR'
         GO TO 980
         END IF
C                                       Set window dimensions:
C                                       (as implimented in IMEAN)
      WIN(1) = BLC(1)
      WIN(2) = BLC(2)
      WIN(3) = TRC(1)
      WIN(4) = TRC(2)
      NXW = WIN(3) - WIN(1) + 1
      NYW = WIN(4) - WIN(2) + 1
C                                        Start pixel count
      COUNT = 0.
C                                        For all Rows
      DO 200 IY = 1, NYW
         CALL MAPIO ('READ', LUN, IN, ERROR)
         IF (ERROR.NE.0) THEN
            ERRTXT = 'MAPIO ERROR'
            GO TO 980
            END IF
         DY = WIN(2) + IY - 1
C                                       For all pixels in a row
         DO 100 IX = 1,NXW
            DX = WIN(1) + IX - 1
C                                      Get brightness
            S = IN (IX)
            ABSS = ABS(S)
C                                      If an OK value
            IF (S.NE.BLANK.AND.ABSS.GT.NOISE) THEN
C                                      Set brightness
               XY(1) = S
               S2    = S * S
               XY(2) = S * DX
               XY(3) = S * DY
               XY(4) = XY(2) * DX
               XY(5) = XY(3) * DY
               XY(6) = XY(3) * DX
               COUNT = COUNT + 1
C                                     Sum over all moments
               DO 90 I = 1,6
C                                     Flux Moment
                  MOMS(1,I) = MOMS(1,I) +  XY(I)
C                                     Flux*Flux Moment
                  MOMS(2,I) = MOMS(2,I) + (XY(I)*ABSS)
C                                     Flux*Flux*Flux Moment
                  MOMS(3,I) = MOMS(3,I) + (XY(I)*S2)
 90               CONTINUE
C                                       End if not blanked or below min
             END IF
 100         CONTINUE
 200     CONTINUE
C                                       Report pixels used
      WRITE(MSGTXT,1300,ERR=999) COUNT, NXW, NYW
      CALL MSGWRT(3)
C                                       If an error, print it
 980  IF (ERROR.NE.0) THEN
         WRITE(MSGTXT,1980,ERR=999) ERRTXT, ERROR
         CALL MSGWRT(6)
         END IF
 999  RETURN
C-----------------------------------------------------------------------
 1300 FORMAT('GETMOM: Used',F11.0,' Pixels in',I6,' by',I6,' Region')
 1980 FORMAT('GETMOM: ',A,', ERROR =',I9)
      END
      SUBROUTINE REDMOM (MOMS)
C-----------------------------------------------------------------------
C   Normalize a moment array by dividing by the zero moment and reducing
C   the 2nd moments to the centroid position
C   INPUTS:
C      MOMS(3,6)     R      Moment array:(1, x, y, xx, yy, xy)*S**IMOM
C-----------------------------------------------------------------------
      INCLUDE 'MOMFT.INC'
      DOUBLE PRECISION   MOMS(MAXMOM,6), FACTOR
      INTEGER   I, J
C-----------------------------------------------------------------------
C                                       For all powers of S
      DO 200 J = 1, MAXMOM
         IF (ABS(MOMS(J,1)) .GT. 1.0E-15) THEN
C                                       For all but first Moment
            DO 100 I = 2,6
               MOMS(J,I) = MOMS(J,I)/MOMS(J,1)
 100           CONTINUE
C                                       Normalize moment integrals
            FACTOR = DBLE(J)
            MOMS(J,4) = FACTOR*(MOMS(J,4) - MOMS(J,2) ** 2)
            MOMS(J,5) = FACTOR*(MOMS(J,5) - MOMS(J,3) ** 2)
            MOMS(J,6) = FACTOR*(MOMS(J,6) - (MOMS(J,2) * MOMS(J,3)))
            END IF
 200     CONTINUE
C
      RETURN
      END
      SUBROUTINE MOMGAU (IMOM, MOMS, GAUS)
C-----------------------------------------------------------------------
C   Convert 2nd moments to gaussian FWHP and position angle.
C   INPUTS:
C      IMOM       I      Index to moment
C      MOMS(3,6)  R      1, X, Y, XX, YY, XY moments
C   OUTPUTS:
C      GAUS(3)    R      FWHP major axis, minor axis, pos angle
C                        in degrees
C The major axis is W , the Minor axis is W , and the angle is T
C                    1                     2
C-----------------------------------------------------------------------
      INTEGER          IMOM
      INCLUDE 'MOMFT.INC'
      DOUBLE PRECISION MOMS(MAXMOM,6)
      REAL             GAUS(3)
C
      DOUBLE PRECISION SUM, DEL, DIF, FWHP, ANG, W2MW12, W2, W1, W22,
     *                 W12, TEMP, W2MW1
C-----------------------------------------------------------------------
      FWHP = 8.0 * LOG (2.0)
C                                     1     2    2
C                              SUM = --- [ W  + W  ]
C                                     2     2    1
      SUM = 0.5 * (MOMS(IMOM,4) + MOMS(IMOM,5))
C                                     1             2    2
C                              DIF = --- COS(2T) [ W  - W  ]
C                                     2             2    1
      DIF = 0.5 * (MOMS(IMOM,5) - MOMS(IMOM,4))
C                                     1             2    2
C                              DEL = --- SIN(2T) [ W  - W  ]
C                                     2             2    1
      DEL =  MOMS(IMOM,6)
      ANG = ATAN2 (DEL, DIF)
C                              Get T from 2T, and convert to degrees
C                              Change to North = 0 convention
      ANG = 270.0 - (PI180 * ANG / 2.0)
C                                     1    2    2  2       2      2
C                        W2MW12    = ---[ W  - W  ] = [ DEL  + DIF ]
C                                     4    2    1
      W2MW12 = DEL**2 + DIF**2
      W2MW1  = SQRT(W2MW12)
C                                2               (1/2)
C                               W  = SUM + W2MW12
C                                2
      W22 = SUM + W2MW1
C                                2               (1/2)
C                               W  = SUM - W2MW12
C                                1
      W12 = SUM - W2MW1
C                              fix angles
      IF (W22.LT.0.0D0) THEN
         ANG = 180. + ANG
         W22 = -W22
         END IF
      IF (W12.LT.0.0D0) THEN
         ANG = 180. + ANG
         W12 = -W12
         END IF
C                              if major is actually minor axis
      IF (W22.GT.W12) THEN
         TEMP = W22
         W22  = W12
         W12  = TEMP
         ANG = ANG + 90.
         END IF
C                             If SUM is less than zero
C                             Source is unresolved
      IF (SUM.LT.0.0D0) THEN
         W12 = 0.0
         W22 = 0.0
         ANG = 0.0
         END IF
C                             Check
      W1 = SQRT( W12)
      W2 = SQRT( W22)
      W1 = SQRT( FWHP) * W1
      W2 = SQRT( FWHP) * W2
C                              ANG ranges between -PI/2 an PI/2
      ANG = MOD( ANG, 180.0D0)
      IF (ANG.GT.90.0D0) ANG = ANG - 180.0D0
      GAUS(1) = W1
      GAUS(2) = W2
      GAUS(3) = ANG
      RETURN
      END
      SUBROUTINE PWRITE (MSGLEV, TXLUN, TXFIND)
C-----------------------------------------------------------------------
C! writes messages to log file and/or terminal and/or output file
C# Utility
C-----------------------------------------------------------------------
C   Inputs:
C      MSGLEV   I      Priority of Message
C      TXLUN    I      Logical unit number (0 => no file)
C      TXFIND   I      Index in FTAB for LUN
C   In/out (common)
C      MSGTXT   C*80   Message in DMSG.INC
C-----------------------------------------------------------------------
      INTEGER   MSGLEV, TXLUN, TXFIND
C
      INTEGER   IERR, TXLEN, ITRIM
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
      IF (MSGLEV.GT.0) THEN
         CALL MSGWRT (MSGLEV)
         END IF
C                                       If output file opened
C                                       Log Informative Messages
      IF (TXLUN.GT.0) THEN
         TXLEN = ITRIM (MSGTXT)
         CALL ZTXIO ('WRIT', TXLUN, TXFIND, MSGTXT(1:TXLEN), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) TXLUN, IERR
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OUTFILE WRITE ERROR ON LUN=',I5,', ERR=',I5)
      END
