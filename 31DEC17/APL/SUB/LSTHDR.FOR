      SUBROUTINE LSTHDR (CATBLK, CATH, CATR, CATD, BSC, BZE)
C-----------------------------------------------------------------------
C! lists header contents in standard form with MSGWRT
C# Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2013, 2015
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
C   LSTHDR lists the contents of a standard header using the MSGWRT
C   facility.
C   Inputs:
C      CATBLK  I(256)      Header as integer
C      CATH    H(256)      Equivalenced header as HOLLERITH
C      CATR    R(256)      Equivalenced header as real
C      CATD    D(128)      Equivalenced header as double precision
C      BSC     D           Net scaling applied to image
C      BZE     D           Net offset applied to image - no disp if 1,0
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256)
      HOLLERITH CATH(128)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128), BSC, BZE
C
      CHARACTER ADATE*12, BDATE*12, VTYA(2)*8, BTEMP*8, CTYPE*2,
     *   VTYB(3)*4, LL(2)*4, MM(2)*4, RA*4, CDEC*4, PRODUC(5)*8,
     *   CHSIGN*1, CNAME*12, CCLAS*6, CTEMP*8, CHSLGN*1
      INTEGER   IBS, I, J, J1, J2, NAX, HM(2), INC, CLAXIS, CMAXIS,
     *   INP, DM(2), MSGLEV, ITRIM
      REAL      TEMP, SEC, DEC, DSEC, XTEST, YTEST
      LOGICAL   ISUV
      DOUBLE PRECISION BS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA MSGLEV /2/
      DATA VTYA, VTYB /'OPTICAL ','RADIO ', 'LSR ','SUN ','YOU '/
      DATA LL, MM /'LL  ','RA--', 'MM  ','DEC-'/
      DATA RA, CDEC /'RA  ','DEC '/
      DATA PRODUC /'NORMAL ','COMPNTS ','RESIDUAL','POINTS ','DIRTY '/
C-----------------------------------------------------------------------
C                                       Image name and file name
      CALL H2CHR (8, 1, CATH(KHOBJ), BTEMP)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CCLAS)
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTYPE)
      ISUV = CTYPE.EQ.'UV'
      WRITE (MSGTXT,1010) BTEMP, CTYPE, CNAME, CCLAS, CATBLK(KIIMS)
      CALL MSGWRT (MSGLEV)
C                                       Telescope and Receiver
      CALL H2CHR (8, 1, CATH(KHTEL), BTEMP)
      CALL H2CHR (8, 1, CATH(KHINS), CTEMP)
      WRITE (MSGTXT,1020) BTEMP, CTEMP
      CALL MSGWRT (MSGLEV)
C                                       Observer and user #
      CALL H2CHR (8, 1, CATH(KHOBS), BTEMP)
      WRITE (MSGTXT,1030) BTEMP, CATBLK(KIIMU)
      CALL MSGWRT (MSGLEV)
C                                       Observation and map date
      CALL H2CHR (8, 1, CATH(KHDOB), BTEMP)
      CALL H2CHR (8, 1, CATH(KHDMP), CTEMP)
      CALL DATDAT (BTEMP, ADATE)
      CALL DATDAT (CTEMP, BDATE)
      WRITE (MSGTXT,1040) ADATE, BDATE
      CALL MSGWRT (MSGLEV)
C                                       Maps
      IF (.NOT.ISUV) THEN
C                                       Blank information
         IF (CATR(KRBLK).EQ.FBLANK) THEN
            WRITE (MSGTXT,1053) 'FLOATING'
            CALL MSGWRT (MSGLEV)
            END IF
C                                       Real minimum and maximum
         CALL H2CHR (8,1, CATH(KHBUN), BTEMP)
         WRITE (MSGTXT,1060) CATR(KRDMN), CATR(KRDMX), BTEMP
         CALL MSGWRT (MSGLEV)
C                                       image scaling
         IF ((BSC.NE.1.0D0) .OR. (BZE.NE.0.0D0)) THEN
            WRITE (MSGTXT,1070) BSC, BZE
            CALL MSGWRT (MSGLEV)
            END IF
         END IF
C                                       Random axes
      IBS = CATBLK(KIGCN)
      J = CATBLK(KIPCN)
      IF ((IBS.GE.1) .AND. (J.GT.0)) THEN
         IF (ISUV) THEN
            CALL H2CHR (2, 1, CATBLK(KITYP), CTYPE)
            WRITE (MSGTXT,1080) IBS, CTYPE
         ELSE
            WRITE (MSGTXT,1081) IBS, J
            END IF
         CALL MSGWRT (MSGLEV)
         MSGTXT = 'Rand axes:'
         INC = 2
         INP = 12
         J = MIN (J, KIPTPN)
         DO 85 I = 1,J
            CALL H2CHR (8, 1, CATH(KHPTP+(I-1)*INC), MSGTXT(INP:))
            INP = ITRIM (MSGTXT) + 3
            IF (INP.GT.57) THEN
               CALL MSGWRT (MSGLEV)
               MSGTXT = ' '
               INP = 12
               END IF
 85         CONTINUE
         IF (INP.GT.12) CALL MSGWRT (MSGLEV)
         END IF
C                                       Set up loop for axes
      NAX = CATBLK(KIDIM)
      WRITE (MSGTXT,1090)
      CALL MSGWRT (MSGLEV)
      WRITE (MSGTXT,1091)
      CALL MSGWRT (MSGLEV)
C                                       Loop over axes
      INC = 2
      CLAXIS = -1
      CMAXIS = -1
      DO 150 I = 1,NAX
         CALL H2CHR (8, 1, CATH(KHCTP+(I-1)*INC), BTEMP)
C                                       Axis type RA
         IF ((BTEMP(1:4).EQ.LL(1)) .OR. (BTEMP(1:4).EQ.RA) .OR.
     *      (BTEMP(1:4).EQ.LL(2))) THEN
            CALL COORDD (1, CATD(KDCRV-1+I), CHSIGN, HM, SEC)
            DEC = CATR(KRCIC-1+I) * 3600.
            CLAXIS = I - 1
            IF (ABS(DEC).GE.1.0) THEN
               WRITE (MSGTXT,1100) BTEMP, CATBLK(KINAX-1+I), CHSIGN, HM,
     *            SEC, CATR(KRCRP-1+I), DEC, CATR(KRCRT-1+I)
            ELSE
               WRITE (MSGTXT,1101) BTEMP, CATBLK(KINAX-1+I), CHSIGN, HM,
     *            SEC, CATR(KRCRP-1+I), DEC, CATR(KRCRT-1+I)
               END IF
            IF (MSGTXT(25:25).EQ.' ') MSGTXT(25:25) = '0'
         ELSE IF ((BTEMP(1:4).EQ.MM(1)) .OR. (BTEMP(1:4).EQ.CDEC) .OR.
     *      (BTEMP(1:4).EQ.MM(2))) THEN
            CALL COORDD (2, CATD(KDCRV-1+I), CHSIGN, HM, SEC)
            DEC = CATR(KRCIC-1+I) * 3600.
            CMAXIS = I - 1
            IF (ABS(DEC).GE.1.0) THEN
               WRITE (MSGTXT,1130) BTEMP, CATBLK(KINAX-1+I), CHSIGN, HM,
     *            SEC, CATR(KRCRP-1+I), DEC, CATR(KRCRT-1+I)
            ELSE
               WRITE (MSGTXT,1131) BTEMP, CATBLK(KINAX-1+I), CHSIGN, HM,
     *           SEC, CATR(KRCRP-1+I), DEC, CATR(KRCRT-1+I)
               END IF
            IF (MSGTXT(25:25).EQ.' ') MSGTXT(25:25) = '0'
C                                       No RA or DEC axis
         ELSE
            WRITE (MSGTXT,1120) BTEMP, CATBLK(KINAX-1+I),
     *         CATD(KDCRV-1+I), CATR(KRCRP-1+I), CATR(KRCIC-1+I),
     *         CATR(KRCRT-1+I)
            END IF
         CALL MSGWRT (MSGLEV)
 150     CONTINUE
      WRITE (MSGTXT,1090)
      CALL MSGWRT (MSGLEV)
C                                       List coordinate equinox
      IF ((CLAXIS.GE.0) .OR. (CMAXIS.GE.0)) THEN
         WRITE (MSGTXT,1155) CATR(KREPO)
         CALL MSGWRT (MSGLEV)
         END IF
C                                       Maptype and iterations
      IF ((CATBLK(KINIT).GT.0) .OR. (CATR(KRBMJ).GT.0.) .OR.
     *   (CATR(KRBMN).GT.0.)) THEN
         IF (.NOT.ISUV) THEN
            I = MAX (1, CATBLK(KITYP))
            IF (I.GT.4) I = 1
            IF (CATBLK(KINIT).LE.0) I = 5
            WRITE (MSGTXT,1160) PRODUC(I), CATBLK(KINIT)
            CALL MSGWRT (MSGLEV)
            END IF
C                                       Beam Parameters
         TEMP = CATR(KRBMJ) * 3600.
         SEC = CATR(KRBMN) * 3600.
         DEC = CATR(KRBPA)
         WRITE (MSGTXT,1161) TEMP, SEC, DEC
         IF ((TEMP.LT.0.5) .AND. (SEC.LT.0.5)) WRITE (MSGTXT,1162)
     *      TEMP, SEC, DEC
         CALL MSGWRT (MSGLEV)
         END IF
C                                       Observed RA, Dec
      XTEST = 1.0E-8
      IF (CLAXIS.GE.0) XTEST = MAX (ABS(CATR(KRCIC+CLAXIS))/100., XTEST)
      YTEST = 1.0E-8
      IF (CMAXIS.GE.0) YTEST = MAX (ABS(CATR(KRCIC+CMAXIS))/100., YTEST)
      IF ((CATD(KDORA).EQ.0.0D0) .AND. (CATD(KDODE).EQ.0.0D0)) GO TO 180
         IF ((CLAXIS.GE.0) .AND. (CMAXIS.GE.0) .AND.
     *      (ABS(CATD(KDORA)-CATD(KDCRV+CLAXIS)).LT.XTEST) .AND.
     *      (ABS(CATD(KDODE)-CATD(KDCRV+CMAXIS)).LT.YTEST)) GO TO 180
            CALL COORDD (1, CATD(KDORA), CHSLGN, HM, SEC)
            CALL COORDD (2, CATD(KDODE), CHSIGN, DM, DSEC)
            WRITE (MSGTXT,1170) CHSLGN, HM, SEC, CHSIGN, DM, DSEC
            IF (MSGTXT(21:21).EQ.' ') MSGTXT(21:21) = '0'
            IF (MSGTXT(42:42).EQ.' ') MSGTXT(42:42) = '0'
            CALL MSGWRT (MSGLEV)
C                                       Shifts
 180  IF ((CATR(KRXSH).EQ.0.0) .AND. (CATR(KRYSH).EQ.0.0)) GO TO 190
         IF ((ABS(CATR(KRXSH)).LT.XTEST) .AND.
     *      (ABS(CATR(KRYSH)).LT.YTEST)) GO TO 190
         SEC = 3600. * CATR(KRXSH)
         DSEC = 3600. * CATR(KRYSH)
         WRITE (MSGTXT,1180) SEC, DSEC
         CALL MSGWRT (MSGLEV)
C                                       Alternative axis type
 190  IF (CATBLK(KIALT).NE.0) THEN
         I = CATBLK(KIALT)/256 + 1
         J = CATBLK(KIALT) - (I-1) * 256
         IF ((I.GE.1) .AND. (I.LE.2) .AND. (J.GE.1) .AND. (J.LE.3))
     *      THEN
            BS = CATD(KDRST) / 1.D6
            WRITE (MSGTXT,1190) BS, VTYA(I), VTYB(J)
            CALL MSGWRT (MSGLEV)
            WRITE (MSGTXT,1191) CATD(KDARV), CATR(KRARP)
            CALL MSGWRT (MSGLEV)
            END IF
         END IF
C                                       UV weight normalization
      IF ((CATR(KRWTN).NE.0.0) .AND. (CATR(KRWTN).NE.1.0)) THEN
         WRITE (MSGTXT,1195) CATR(KRWTN)
         CALL MSGWRT (MSGLEV)
         END IF
C                                       Extension files
      CALL FXHDEX (CATBLK)
      DO 205 I = 1,KIEXTN
         J1 = KIVER - 1 + I
         IF (CATBLK(J1).LE.0) GO TO 205
            J2 = KHEXT - 1 + I
            CALL H2CHR (2, 1, CATBLK(J2), CTYPE)
            WRITE (MSGTXT,1200) CTYPE, CATBLK(J1)
            CALL MSGWRT (MSGLEV)
 205     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Image=',A8,'  (',A2,')',9X,'Filename=',A12,'.',A6,'.',I4)
 1020 FORMAT ('Telescope=',A8,11X,'Receiver=',A8)
 1030 FORMAT ('Observer=',A8,12X,'User #=',I5)
 1040 FORMAT ('Observ. date=',A12,4X,'Map date=',A12)
 1053 FORMAT ('Pixel type: ',A8,9X,'Magic value blanking')
 1060 FORMAT ('Minimum=',1PE15.8,6X,'Maximum=',E15.8,1X,A8)
 1070 FORMAT ('Map scale=',1PE14.7,5X,'Map offset=',E14.7,' applied')
 1080 FORMAT ('# visibilities',I10,5X,'Sort order  ',A2)
 1081 FORMAT ('Group count',I10,8X,'# random axes',I2)
 1090 FORMAT (2('--------------------------------'))
 1091 FORMAT ('Type    Pixels   Coord value     at Pixel ',
     *   '    Coord incr   Rotat')
 1100 FORMAT (A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,F11.2,F15.3,F8.2)
 1101 FORMAT (A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,F11.2,F15.6,F8.2)
 1120 FORMAT (A8,I6,2X,1PE14.7,0PF11.2,1PE15.7,0PF8.2)
 1130 FORMAT (A8,I6,3X,A1,I2.2,I3.2,F7.3,F11.2,F15.3,F8.2)
 1131 FORMAT (A8,I6,3X,A1,I2.2,I3.2,F7.3,F11.2,F15.6,F8.2)
 1155 FORMAT ('Coordinate equinox',F8.2)
 1160 FORMAT ('Map type=',A8,12X,'Number of iterations=',I8)
 1161 FORMAT ('Conv size=',F7.2,' X',F7.2,3X,'Position angle=',F7.2)
 1162 FORMAT ('Conv size=',F8.5,' X',F8.5,3X,'Position angle=',F7.2)
 1170 FORMAT ('Observed RA ',A1,1X,I2.2,I3.2,F7.3,4X,'DEC ',A1,I2.2,
     *   I3.2,F6.2)
 1180 FORMAT ('Phase shifted in X',F13.5,2X,'in Y',F13.5)
 1190 FORMAT ('Rest freq',F11.3,9X,'Vel type: ',A7,' wrt ',A4)
 1191 FORMAT ('Alt ref. value',1PE13.5,2X,'wrt pixel',0PF8.2)
 1195 FORMAT ('UV weight normalization factor',1PE12.4)
 1200 FORMAT ('Maximum version number of extension files of type ',
     *   A2,' is',I4)
      END
