      SUBROUTINE KWIKHD
C-----------------------------------------------------------------------
C! list header contents in abbreviated, image centered form
C# Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2014
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
C   KWIKHD lists the contents of a standard header using MSGWRT.
C   Unlike LSTHDR, this routine does not try to list everything and
C   does convert the coordinates to the image numeric center.
C   Common input:
C      /MAPHDR/ image catalog block
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PHDR.INC'
      CHARACTER BTEMP*8, CPTYPE*2, CNAME*12, CCLASS*6, NPRT*1, CTEMP*8,
     *   CHSTOK(20)*4, PRODUC(5)*8, CHSIGN*1, TEXT(NIEXTN)*2
      INTEGER   I, J, J1, J2, NAX, HM(2), INC, INP, NEXT(NIEXTN), IN,
     *   IROUND, MSLEV, IDEPTH(7), NNOT, IERR, IBS, ITRIM
      REAL      TEMP, SEC, DEC, RDX, RDY
      DOUBLE PRECISION X(7), DX, DY, DT, COSR, SINR
      LOGICAL   ISUV, DONEIT, OKFLAG, NOSWAP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CHSTOK /'BEAM','IPOL','QPOL','UPOL','VPOL','PPOL','FPOL',
     *   'PANG','SPIX','OPTD','ROTM','????','RR','LL','RL','LR','XX',
     *   'YY','XY','YX'/
      DATA PRODUC /'NORMAL  ','COMPNTS ','RESIDUAL','POINTS  ',
     *   'DIRTY   '/
      DATA MSLEV /2/
      DATA NOSWAP /.FALSE./
C-----------------------------------------------------------------------
      NNOT = 0
C                                       Image name and file name
 10   CALL H2CHR (8, 1, CATH(KHOBJ), BTEMP)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), CNAME)
      CALL H2CHR (6, KHIMCO, CATH(KHIMN), CCLASS)
      CALL H2CHR (2, KHPTYO, CATH(KHIMN), CPTYPE)
      ISUV = CPTYPE.EQ.'UV'
      WRITE (MSGTXT,1010) BTEMP, CPTYPE, CNAME, CCLASS, CATBLK(KIIMS)
      CALL MSGWRT (MSLEV)
C                                       Blanking information
 20   IF (CATR(KRBLK).NE.0.0) THEN
         WRITE (MSGTXT,1020)
         IF (CATR(KRBLK).NE.FBLANK) WRITE (MSGTXT,1021) CATR(KRBLK)
         CALL MSGWRT (MSLEV)
         END IF
C                                       Real minimum and maximum
 30   IF (ISUV) GO TO 40
         CALL H2CHR (8, 1, CATH(KHBUN), BTEMP)
         SEC = CATR(KRDMX)
         DEC = CATR(KRDMN)
         TEMP = MAX (ABS(SEC), ABS(DEC))
         CTEMP = ' '
         IF (TEMP.EQ.0.0) GO TO 35
            CALL METSCA (TEMP, CTEMP, OKFLAG)
            IF (.NOT.OKFLAG) GO TO 34
               WRITE (MSGTXT,1030) DEC, SEC, BTEMP
               CALL MSGWRT (MSLEV)
               GO TO 40
 34         CONTINUE
               TEMP = TEMP / MAX (ABS(SEC), ABS(DEC))
               SEC = SEC * TEMP
               DEC = DEC * TEMP
 35      WRITE (MSGTXT,1035) DEC, SEC, CTEMP, BTEMP
         CALL MSGWRT (MSLEV)
C                                       Random axes
 40   IBS = CATBLK(KIGCN)
      J = CATBLK(KIPCN)
      IF ((IBS.LT.1) .OR. (J.LE.0)) GO TO 50
         IF (.NOT.ISUV) WRITE (MSGTXT,1040) IBS, J
         IF (ISUV) WRITE (MSGTXT,1041) IBS, CATBLK(KITYP)
         CALL MSGWRT (MSLEV)
         MSGTXT = 'Rand axes:'
         IF (ISUV) GO TO 50
            INC = 2
            INP = 12
            J = MIN (J, KIPTPN)
            DO 45 I = 1,J
               CALL H2CHR (8, 1, CATH(KHPTP+(I-1)*INC), MSGTXT(INP:))
               INP = ITRIM (MSGTXT) + 3
               IF (INP.GE.58) THEN
                  CALL MSGWRT (MSLEV)
                  INP = 12
                  MSGTXT = ' '
                  END IF
 45            CONTINUE
            IF (INP.GT.12) CALL MSGWRT (MSLEV)
C                                       Set up loop for axes
 50   NAX = CATBLK(KIDIM)
      WRITE (MSGTXT,1050)
      CALL MSGWRT (MSLEV)
      WRITE (MSGTXT,1051)
      CALL MSGWRT (MSLEV)
      INC = 2
C                                       Get values at center pixels
      IF (ISUV) THEN
         KLOCS(LOCNUM) = -1
         DO 55 I = 1,NAX
            J = I - 1
            X(I) = CATD(KDCRV+J) + CATR(KRCIC+J) * (1.0 - CATR(KRCRP+J))
            IDEPTH(I) = 1
            CALL H2CHR (8, 1, CATH(KHCTP+J*INC), CTEMP)
            IF (CTEMP.EQ.'STOKES') KLOCS(LOCNUM) = J
 55         CONTINUE
         GO TO 100
         END IF
C                                       Maps
 60   DO 65 I = 1,NAX
         IDEPTH(I) = (CATBLK(KINAX+I-1) + 1) / 2
 65      CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH(3), NOSWAP)
      RDX = IDEPTH(1)
      RDY = IDEPTH(2)
      CALL XYVAL (RDX, RDY, X(1), X(2), X(KLOCA(LOCNUM)+1), IERR)
      IF (NAX.LT.3) GO TO 100
         DONEIT = AXTYP(LOCNUM).NE.4
         DO 95 I = 3,NAX
            J = I - 1
            IF (((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) .AND.
     *         (KLOCA(LOCNUM).EQ.J)) GO TO 95
            IF (AXFUNC(I,LOCNUM).GT.0) GO TO 70
               X(I) = (IDEPTH(I)-CATR(KRCRP+J))*CATR(KRCIC+J) +
     *            CATD(KDCRV+J)
               GO TO 95
 70         IF (AXFUNC(I,LOCNUM).GT.1) GO TO 75
               X(I) = IDEPTH(I) - CATR(KRCRP+J)
               X(I) = CATD(KDCRV+J) + X(I) * CATR(KRCIC+J) /
     *            (1.D0 + AXDENU(LOCNUM)*X(I))
               GO TO 95
 75         IF (AXTYP(LOCNUM).EQ.4) GO TO 80
               WRITE (MSGTXT,1075) I
               CALL MSGWRT (8)
               X(I) = (IDEPTH(I)-CATR(KRCRP+J))*CATR(KRCIC+J) +
     *            CATD(KDCRV+J)
               GO TO 95
 80         IF (DONEIT) GO TO 95
               DONEIT = .TRUE.
               DX = (ZDEPTH(KLOCL(LOCNUM)-1,LOCNUM) - RPLOC(3,LOCNUM)) *
     *            AXINC(3,LOCNUM)
               DY = (ZDEPTH(KLOCM(LOCNUM)-1,LOCNUM) - RPLOC(4,LOCNUM)) *
     *            AXINC(4,LOCNUM)
               COSR = COS (ROT(LOCNUM) * COND2R)
               SINR = SGNROT(LOCNUM) * SIN (ROT(LOCNUM) * COND2R)
               DT = (DX * COSR - DY * SINR) * COND2R
               DY = (DY * COSR + DX * SINR) * COND2R
               DX = DT
               COSR = RPVAL(3,LOCNUM) * COND2R
               SINR = RPVAL(4,LOCNUM) * COND2R
               CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), COSR, SINR,
     *            DX, DY, X(KLOCL(LOCNUM)+1), X(KLOCM(LOCNUM)+1), IERR)
               X(KLOCL(LOCNUM)+1) = X(KLOCL(LOCNUM)+1) / COND2R
               X(KLOCM(LOCNUM)+1) = X(KLOCM(LOCNUM)+1) / COND2R
 95         CONTINUE
C                                       Loop to display axes
 100  DO 150 I = 1,NAX
         NPRT = ' '
         IF (ABS(IDEPTH(I)-CATR(KRCRP+I-1)).GT.0.05) THEN
            NPRT = '*'
            NNOT = NNOT + 1
            END IF
         J = I - 1
         CALL H2CHR (8, 1, CATH(KHCTP+J*INC), BTEMP)
C                                       Check axis type
         IF ((BTEMP(1:4).NE.'LL  ') .AND. (BTEMP(1:4).NE.'RA  ') .AND.
     *      (BTEMP(1:4).NE.'RA--')) GO TO 110
            CALL COORDD (1, X(I), CHSIGN, HM, SEC)
            DEC = CATR(KRCIC-1+I) * 3600.
            IF (ABS(DEC).GE.1.0) WRITE (MSGTXT,1100) BTEMP,
     *         CATBLK(KINAX-1+I), CHSIGN, HM, SEC, IDEPTH(I), NPRT, DEC,
     *         CATR(KRCRT-1+I)
            IF (ABS(DEC).LT.1.0) WRITE (MSGTXT,1101) BTEMP,
     *         CATBLK(KINAX-1+I), CHSIGN, HM, SEC, IDEPTH(I), NPRT, DEC,
     *         CATR(KRCRT-1+I)
            GO TO 145
 110     IF ((BTEMP(1:4).NE.'MM  ') .AND. (BTEMP(1:4).NE.'DEC ') .AND.
     *      (BTEMP(1:4).NE.'DEC-')) GO TO 120
            CALL COORDD (2, X(I), CHSIGN, HM, SEC)
            DEC = CATR(KRCIC-1+I) * 3600.
            GO TO 140
C                                       No RA or DEC axis
 120     IF (J.EQ.KLOCS(LOCNUM)) GO TO 130
            WRITE (MSGTXT,1120) BTEMP, CATBLK(KINAX-1+I), X(I),
     *         IDEPTH(I), NPRT, CATR(KRCIC-1+I), CATR(KRCRT-1+I)
            CALL MSGWRT (MSLEV)
            GO TO 150
C                                       Stokes axis
 130     CONTINUE
            TEMP = X(I)
            J1 = IROUND(TEMP) + 1
            J2 = J1
            IF ((J1.LE.0) .OR. (J1.GT.11)) J2 = 12
            IF ((J1.LE.0) .AND. (J1.GE.-7)) J2 = 13 - J1
            WRITE (MSGTXT,1130) BTEMP, CATBLK(KINAX-1+I), CHSTOK(J2),
     *         IDEPTH(I), NPRT, CATR(KRCIC-1+I), CATR(KRCRT-1+I)
            CALL MSGWRT (MSLEV)
            GO TO 150
C                                       RA or DEC axis
 140     IF (ABS(DEC).GE.1.0) WRITE (MSGTXT,1140) BTEMP,
     *      CATBLK(KINAX-1+I), CHSIGN, HM, SEC, IDEPTH(I), NPRT, DEC,
     *      CATR(KRCRT-1+I)
         IF (ABS(DEC).LT.1.0) WRITE (MSGTXT,1141) BTEMP,
     *      CATBLK(KINAX-1+I), CHSIGN, HM, SEC, IDEPTH(I), NPRT, DEC,
     *      CATR(KRCRT-1+I)
 145     IF (MSGTXT(25:25).EQ.' ') MSGTXT(25:25) = '0'
         CALL MSGWRT (MSLEV)
 150     CONTINUE
      WRITE (MSGTXT,1050)
      CALL MSGWRT (MSLEV)
C                                       Maptype and iterations
 160  IF ((CATBLK(KINIT).GT.0) .OR. (CATR(KRBMJ).GT.0.) .OR.
     *   (CATR(KRBMN).GT.0.)) THEN
         IF (.NOT.ISUV) THEN
            I = MAX (1, CATBLK(KITYP))
            IF (I.GT.4) I = 1
            IF (CATBLK(KINIT).LE.0) I = 5
            WRITE (MSGTXT,1160) PRODUC(I), CATBLK(KINIT)
            CALL MSGWRT (MSLEV)
            END IF
C                                       Beam Parameters
         TEMP = CATR(KRBMJ) * 3600.
         SEC = CATR(KRBMN) * 3600.
         DEC = CATR(KRBPA)
         WRITE (MSGTXT,1161) TEMP, SEC, DEC
         IF ((TEMP.LT.0.5) .AND. (SEC.LT.0.5)) WRITE (MSGTXT,1162)
     *      TEMP, SEC, DEC
         CALL MSGWRT (MSLEV)
         END IF
C                                       Extension files
 170  IN = 0
      CALL FXHDEX (CATBLK)
      DO 175 I = 1,KIEXTN
         J1 = KIVER - 1 + I
         IF ((CATBLK(J1).LE.0) .OR. (IN.EQ.50)) GO TO 175
            IN = IN + 1
            J2 = KHEXT - 1 + I
            CALL H2CHR (2, 1, CATBLK(J2), TEXT(IN))
            NEXT(IN) = CATBLK(J1)
 175     CONTINUE
      J1 = 1
 176  IF (J1.GT.IN) GO TO 180
         J2 = MIN (J1+4, IN)
         WRITE (MSGTXT,1175) (TEXT(I), NEXT(I), I = J1,J2)
         CALL MSGWRT (MSLEV)
         J1 = J2 + 1
         GO TO 176
C                                       Not reference pixel
 180  IF (NNOT.LE.0) GO TO 999
         WRITE (MSGTXT,1180)
         CALL MSGWRT (MSLEV)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Image=',A8,'  (',A2,')',9X,'Filename=',A12,'.',A6,'.',I4)
 1020 FORMAT ('Magic value blanking')
 1021 FORMAT ('Illegal blanking',1PE15.7)
 1030 FORMAT ('Minimum=',1PE13.5,8X,'Maximum=',1PE13.5,1X,A8)
 1035 FORMAT ('Minimum=',F9.4,12X,'Maximum=',F9.4,1X,A5,A8)
 1040 FORMAT ('Group count',I10,8X,'# Random axes',I2)
 1041 FORMAT ('# visibilities',I10,5X,'Sort order  ',A2)
 1050 FORMAT (2('-------------------------------'))
 1051 FORMAT ('Type    Pixels   Coord value  at Pixel',
     *   '    Coord incr   Rotat')
 1075 FORMAT ('ERROR IN TYPE POINTERS, AXIS NUMBER',I6)
 1100 FORMAT (A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,I8,A1,F13.3,F8.2)
 1101 FORMAT (A8,I6,2X,A1,1X,I2.2,I3.2,F7.3,I8,A1,F13.6,F8.2)
 1120 FORMAT (A8,I6,2X,1PE14.6,I8,A1,1PE13.4,0PF8.2)
 1130 FORMAT (A8,I6,4X,A4,8X,I8,A1,F13.4,F8.2)
 1140 FORMAT (A8,I6,3X,A1,I2.2,I3.2,F7.3,I8,A1,F13.3,F8.2)
 1141 FORMAT (A8,I6,3X,A1,I2.2,I3.2,F7.3,I8,A1,F13.6,F8.2)
 1160 FORMAT ('Map type=',A8,12X,'Number of iterations=',I8)
 1161 FORMAT ('Conv size=',F7.2,' X',F7.2,3X,'Position angle=',F7.2)
 1162 FORMAT ('Conv size=',F8.5,' X',F8.5,3X,'Position angle=',F7.2)
 1175 FORMAT ('Max ext file #:',5(1X,A2,I4,3X))
 1180 FORMAT ('* => NOT the formal reference pixel on this axis')
      END
