      PROGRAM SETTVP
C-----------------------------------------------------------------------
C! stand-alone program to initialize the TV display parameter file
C# Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C-----------------------------------------------------------------------
C   SETTVP conducts a dialogue with the user to initialize and alter
C   The AIPS Television display parameter disk files
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48, MSGBUF*80, PRGNAM*6
      INTEGER   TTYLUN, TTYIND, JOPT, IOPT, IOBLK(256), LTVDEV, I, IERR,
     *   LUN, IND, SCRTCH(256), PASPER, PASCHG, ADDR, GOADDR, NWORDS,
     *   IREC
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      COMMON /TTYCOM/ TTYLUN, TTYIND
      DATA PRGNAM /'SETTVP'/
      DATA LUN /8/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       AIPS initialization
      TTYLUN = 5
      CALL AIPINI (TTYLUN, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 980
      PASPER = 0
      LTVDEV = NTVDEV
C                                       Read TV #, option number
 50   PASCHG = 0
      WRITE (MSGBUF,1050)
      CALL INQINT (TTYLUN, MSGBUF, 2, IOBLK, IERR)
      IF (IERR.LT.0) GO TO 50
      IF (IERR.GT.0) GO TO 980
      IOPT = IOBLK(1)
      I = IOBLK(2)
      IF ((I.GT.0) .AND. (I.LE.LTVDEV)) NTVDEV = I
      IF (IOPT.EQ.5) GO TO 995
      IF ((IOPT.GE.1) .OR. (IOPT.LE.4)) GO TO 100
C                                       Error from somewhere
 60      WRITE (MSGTXT,1060)
         CALL MSGWRT (7)
         GO TO 50
C                                       Read TV disk current parms
 100  CALL ZPHFIL ('ID', 1, 0, NTVDEV, PHNAME, IERR)
      CALL ZOPEN (LUN, IND, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) GO TO 60
      CALL ZFIO ('READ', LUN, IND, 1, IOBLK, IERR)
      IF (IERR.EQ.0) GO TO 110
         CALL ZCLOSE (LUN, IND, IERR)
         GO TO 60
C                                       Initialize from YTVCIN
 110  IF (IOPT.GT.1) THEN
         CALL COPY (29, IOBLK, NGRAY)
      ELSE
         CALL YTVCIN
         CALL COPY (29, NGRAY, IOBLK)
         PASCHG = 1
         END IF
C                                       ISU
 120  IF (IOPT.NE.4) GO TO 150
         IF (ISUNUM.GT.0) GO TO 125
            WRITE (MSGTXT,1120)
            CALL MSGWRT (7)
            GO TO 180
C                                       Require password
 125     IF (PASPER.GT.0) GO TO 130
            CALL PASWRD (SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 995
            PASPER = 1
 130     WRITE (MSGBUF,1130)
         CALL INQINT (TTYLUN, MSGBUF, 3, IOBLK, IERR)
         IF (IERR.LT.0) GO TO 130
         IF (IERR.GT.0) GO TO 980
         ADDR = IOBLK(1)
         NWORDS = IOBLK(2)
         GOADDR = IOBLK(3)
         CALL FILL (256, 0, SCRTCH)
         SCRTCH(1) = ADDR
         SCRTCH(2) = NWORDS
         SCRTCH(3) = GOADDR
         IREC = 2
         CALL ZFIO ('WRIT', LUN, IND, IREC, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 180
         TVLUN2 = LUN
         TVIND2 = IND
         CALL ZCLOSE (LUN, IND, IERR)
         CALL TVOPEN (SCRTCH, IERR)
         CALL YISLOD ('READ', SCRTCH, IERR)
         CALL TVCLOS (SCRTCH, IERR)
         GO TO 50
C                                       Change values: list first
 150  CONTINUE
         CALL KUESTV (IOPT, 0, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Enter option
 155     WRITE (MSGBUF,1155)
         CALL INQINT (TTYLUN, MSGBUF, 1, JOPT, IERR)
         IF (IERR.LT.0) GO TO 155
         IF (IERR.GT.0) GO TO 980
         IF (JOPT.LT.0) GO TO 160
            CALL KUESTV (IOPT, JOPT, IERR)
            IF (IERR.NE.0) GO TO 980
            IF (JOPT.GT.0) THEN
               PASCHG = 1
               CALL COPY (29, NGRAY, IOBLK)
               END IF
            GO TO 155
C                                       Write file and close
 160  IF (PASCHG.LE.0) GO TO 180
C                                       Require password
         IF (PASPER.GT.0) GO TO 170
            CALL PASWRD (SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 995
            PASPER = 1
C                                       do write
 170     CALL ZFIO ('WRIT', LUN, IND, 1, IOBLK, IERR)
 180  CALL ZCLOSE (LUN, IND, IERR)
      GO TO 50
C                                       Terminal IO error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close accounting
 995  CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1050 FORMAT ('Enter 1=Init, 2/3=Change parms, 4=ISU, 5=quit and the',
     *   ' TV # (2I2)')
 1060 FORMAT ('SOME ERROR: TRY AGAIN')
 1120 FORMAT ('THERE ARE NO ISU UNITS ON THIS TV')
 1130 FORMAT ('Enter: start ADDR, NWORDS, GOADDR (3I6)')
 1155 FORMAT ('Enter number to change, 0=print, -1=return (I2)')
 1980 FORMAT ('ERROR',I7,' IN TERMINAL IO')
      END
      SUBROUTINE KUESTV (IOPT, JOPT, IERR)
C-----------------------------------------------------------------------
C   KUESTV displays the TV parameters and reads a specified value
C   Inputs:
C      IOPT   I   2 do principal, 3 do all
C      JOPT   I   Which option, 0 => print all, read none
C   Output:
C      IERR   I   Error code from terminal IO
C   Common in/out:
C      /TVCHAR/ TV parms (29 words available)
C-----------------------------------------------------------------------
      INTEGER   IOPT, JOPT, IERR
C
      CHARACTER MSGBUF*80
      INTEGER   TTYLUN, TTYIND, K, KK(2)
      DOUBLE PRECISION DTEMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      COMMON /TTYCOM/ TTYLUN, TTYIND
C-----------------------------------------------------------------------
 10   IERR = 0
C                                       Branch to requested function
      IF (JOPT.EQ.0) GO TO 100
      IF (JOPT.LT.0) GO TO 90
      IF ((IOPT.NE.3) .AND. (JOPT.GT.13)) GO TO 90
      IF ((IOPT.EQ.3) .AND. (JOPT.GT.17)) GO TO 90
      GO TO (100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     *   210, 220, 230, 240, 250, 260), JOPT
C                                       Error
 90   CONTINUE
         WRITE (MSGTXT,1090)
         CALL MSGWRT (6)
         GO TO 999
C                                       # gray planes
 100  WRITE (MSGBUF,1100) NGRAY
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LE.0) GO TO 10
            NGRAY = K
            END IF
         GO TO 999
         END IF
C                                       # graphics overlay planes
 110  WRITE (MSGBUF,1110) NGRAPH
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LE.0) GO TO 10
            NGRAPH = K
            END IF
         GO TO 999
         END IF
C                                       # images / plane
 120  WRITE (MSGBUF,1120) NIMAGE
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LE.0) GO TO 10
            NIMAGE = K
            END IF
         GO TO 999
         END IF
C                                       Pixel size of TV, x,y
 130  WRITE (MSGBUF,1130) MAXXTV
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (KK(1).LE.0) GO TO 10
            IF (KK(2).LE.0) GO TO 10
            MAXXTV(1) = KK(1)
            MAXXTV(2) = KK(2)
            END IF
         GO TO 999
         END IF
C                                       Max TV intensity
 140  WRITE (MSGBUF,1140) MAXINT
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LE.0) GO TO 10
            MAXINT = K
            END IF
         GO TO 999
         END IF
C                                       LUT output intensity
 150  WRITE (MSGBUF,1150) LUTOUT
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LE.0) GO TO 10
            LUTOUT = K
            END IF
         GO TO 999
         END IF
C                                       OFM intensities
 160  WRITE (MSGBUF,1160) OFMINP, OFMOUT
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (KK(1).LE.0) GO TO 10
            IF (KK(2).LE.0) GO TO 10
            OFMINP = KK(1)
            OFMOUT = KK(2)
            END IF
         GO TO 999
         END IF
C                                       Scroll increments
 170  WRITE (MSGBUF,1170) SCXINC, SCYINC
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (KK(1).LE.0) GO TO 10
            IF (KK(2).LE.0) GO TO 10
            SCXINC = KK(1)
            SCYINC = KK(2)
            END IF
         GO TO 999
         END IF
C                                       Max zoom: power of 2, linear
 180  WRITE (MSGBUF,1180) MXZOOM
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1181)
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            MXZOOM = K
            END IF
         GO TO 999
         END IF
C                                       Type of splitting
 190  WRITE (MSGBUF,1190) TYPSPL
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1191)
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LT.0) GO TO 10
            TYPSPL = K
            END IF
         GO TO 999
         END IF
C                                       TV character size: x,y
 200  WRITE (MSGBUF,1200) CSIZTV
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 2, KK, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (KK(1).LE.0) GO TO 10
            IF (KK(2).LE.0) GO TO 10
            CSIZTV(1) = KK(1)
            CSIZTV(2) = KK(2)
            END IF
         GO TO 999
         END IF
C                                       TV x image write mode
C                                       0 no, 1 to right, 2 to left
 210  WRITE (MSGBUF,1210) TVXMOD
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1211)
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LT.0) GO TO 10
            TVXMOD = K
            END IF
         GO TO 999
         END IF
C                                       TV y image write mode
C                                       0 no, 1 up, 2 down
 220  WRITE (MSGBUF,1220) TVYMOD
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1221)
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LT.0) GO TO 10
            TVYMOD = K
            END IF
         GO TO 999
         END IF
C                                       More detailed ones
 230  IF (IOPT.NE.3) GO TO 999
C                                       Number of ALU subunits
      WRITE (MSGBUF,1230) TVALUS
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LT.0) GO TO 10
            TVALUS = K
            END IF
         GO TO 999
         END IF
C                                       Number ISU units
 240  WRITE (MSGBUF,1240) ISUNUM
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (K.LT.0) GO TO 10
            ISUNUM = K
            END IF
         GO TO 999
         END IF
C                                       Gamma correction
 250  WRITE (MSGBUF,1250) TVGAMA
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQFLT (TTYLUN, MSGBUF, 1, DTEMP, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF (DTEMP.LE.0.001) GO TO 10
            TVGAMA = DTEMP
            END IF
         GO TO 999
         END IF
C                                       Number channels / color
 260  WRITE (MSGBUF,1260) TVIMPC
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (MSGBUF,1261)
      IF (JOPT.EQ.0) THEN
         CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         CALL INQINT (TTYLUN, MSGBUF, 1, K, IERR)
         IF (IERR.LT.0) GO TO 10
         IF (IERR.EQ.0) THEN
            IF ((K.LT.0) .OR. (K.GT.16)) GO TO 10
            TVIMPC = K
            END IF
         GO TO 999
         END IF
C                                       Maybe more later
 270  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1090 FORMAT ('INVALID OPTION NUMBER')
 1100 FORMAT ('1  No. of gray-scale planes         (I)',I6)
 1110 FORMAT ('2  No. of graphics overlay planes   (I)',I6)
 1120 FORMAT ('3  No. of images / gray-plane       (I)',I6)
 1130 FORMAT ('4  X,Y size of TV planes (pixels) (2 I)',2I6)
 1140 FORMAT ('5  Maximum gray-scale intensity     (I)',I6)
 1150 FORMAT ('6  Peak intensity out of LUT        (I)',I6)
 1160 FORMAT ('7  Peak intensities in/out of OFM (2 I)',2I6)
 1170 FORMAT ('8  X,Y min. scroll increments     (2 I)',2I6)
 1180 FORMAT ('9  Maximum zoom: (>0) power of 2    (I)',I6)
 1181 FORMAT ('      (< 0) Max factor = 1 - MAXINT')
 1190 FORMAT ('10 Type of split-screen allowed     (I)',I6)
 1191 FORMAT ('      1=Vert, 2=Hori, 3=Either, 4=Both')
 1200 FORMAT ('11 # X,Y pixels in TV characters  (2 I)',2I6)
 1210 FORMAT ('12 X-axis image write mode(s)       (I)',I6)
 1211 FORMAT ('      0 - None, 1 -> Right, 2 -> Left')
 1220 FORMAT ('13 Y-axis image write mode(s)       (I)',I6)
 1221 FORMAT ('      0 - None, 1 -> Up, 2 -> Down')
 1230 FORMAT ('14 Number of ALU units in TV        (I)',I6)
 1240 FORMAT ('15 Number of image storage units    (I)',I6)
 1250 FORMAT ('16 Gamma correction for OFM         (R)',F8.3)
 1260 FORMAT ('17 No. simultaneous channels/color  (I)',I6)
 1261 FORMAT ('      0 -> one channel for all colors  ')
      END
