      SUBROUTINE SETLOC (DEPTH, SWAPOK)
C-----------------------------------------------------------------------
C! sets location common for coordinate computations and display
C# Coordinates Plot--util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2013
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
C   SETLOC uses the catalog header to build the values of the position
C   common /LOCATI/ for use by position finding and axis labeling
C   routines (at least).
C   Inputs:
C      DEPTH    I(5)   Position of map plane axes 3 - 7
C      SWAPOK   L      T => okay to swap axes if rotation near 90
C   Common:
C      /MAPHDR/ catalog block (not modified)
C      /LOCATI/ position parms - created here
C-----------------------------------------------------------------------
      INTEGER   DEPTH(5)
      LOGICAL   SWAPOK
C
      CHARACTER AXP(2,5)*4, STOKES*4, FRAXP(3)*4, GEOM(11)*4, TS*20,
     *   CTEST1*4, CTEST2*4
      INTEGER   NANG, KL, JX, I, J, NAX, NGEOM, II, JF, JY
      REAL      SWAP
      DOUBLE PRECISION DT, DX, DY, COSR, SINR, X, Y
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA NANG /5/
      DATA AXP /'ELON','ELAT', 'GLON','GLAT', 'RA--','DEC-',
     *   'RA  ','DEC ', 'LL  ','MM  '/
      DATA STOKES, FRAXP /'STOK', 'FREQ','VELO','FELO'/
      DATA NGEOM, GEOM /11, '-SIN', '-TAN', '-ARC', '-NCP', '-STG',
     *   '-GLS', '-MER', '-AIT', '-CAR', '-MOL', '-PAR'/
C-----------------------------------------------------------------------
C                                       Basic parms: assume VLA normal
      CALL CHECKL ('SETLOC')
      COND2R = 3.14159265358979D0 / 1.80D2
      REPOCH(LOCNUM) = CATR(KREPO)
      RPVAL(1,LOCNUM) = CATD(KDCRV)
      RPVAL(2,LOCNUM) = CATD(KDCRV+1)
      RPLOC(1,LOCNUM) = CATR(KRCRP)
      RPLOC(2,LOCNUM) = CATR(KRCRP+1)
      AXINC(1,LOCNUM) = CATR(KRCIC)
      IF (AXINC(1,LOCNUM).EQ.0.0) AXINC(1,LOCNUM) = 1.0
      AXINC(2,LOCNUM) = CATR(KRCIC+1)
      IF (AXINC(2,LOCNUM).EQ.0.0) AXINC(2,LOCNUM) = 1.0
      CALL H2CHR (8, 1, CATH(KHCTP), CTYP(1,LOCNUM))
      CALL H2CHR (8, 1, CATH(KHCTP+2), CTYP(2,LOCNUM))
      ROT(LOCNUM) = CATR(KRCRT+1)
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      CTYP(3,LOCNUM) = ' '
      CTYP(4,LOCNUM) = ' '
      CALL COPY (5, DEPTH, ZDEPTH(1,LOCNUM))
      ZAXIS(LOCNUM) = 0
      AXTYP(LOCNUM) = 1
      CORTYP(LOCNUM) = 1
      LABTYP(LOCNUM) = 65
      SGNROT(LOCNUM) = 1
      GEOMD1(LOCNUM) = 0.0D0
      GEOMD2(LOCNUM) = 0.0D0
      GEOMD3(LOCNUM) = 0.0D0
      GEOMD4(LOCNUM) = 0.0D0
C                                       Get pointers to axis types
      KLOCL(LOCNUM) = -1
      KLOCM(LOCNUM) = -1
      KLOCF(LOCNUM) = -1
      KLOCS(LOCNUM) = -1
      KLOCA(LOCNUM) = -1
      KLOCB(LOCNUM) = -1
      NAX = CATBLK(KIDIM)
      JX = 0
      JY = 0
      JF = 0
      DO 20 I = 1,NAX
         II = (I-1) * 2 + KHCTP
         CALL H2CHR (4, 1, CATR(II), CTEST1)
         DO 10 J = 1,NANG
            IF (AXP(1,J).EQ.CTEST1) THEN
               JX = J
               KLOCL(LOCNUM) = I - 1
               GO TO 20
            ELSE IF (AXP(2,J).EQ.CTEST1) THEN
               JY = J
               KLOCM(LOCNUM) = I - 1
               GO TO 20
               END IF
 10         CONTINUE
         DO 15 J = 1,3
            IF (FRAXP(J).EQ.CTEST1) THEN
               JF = J
               KLOCF(LOCNUM) = I - 1
               GO TO 20
               END IF
 15         CONTINUE
         IF (STOKES.EQ.CTEST1) KLOCS(LOCNUM) = I - 1
 20      CONTINUE
C                                       Non-linear axes?
      CALL FILL (KICTPN, -1, AXFUNC(1,LOCNUM))
      CALL FILL (NAX, 0, AXFUNC(1,LOCNUM))
C                                       velocity
      IF (JF.EQ.3) THEN
         AXFUNC(KLOCF(LOCNUM)+1,LOCNUM) = 1
         AXDENU(LOCNUM) = 0.0D0
         IF (CATD(KDRST).GT.0.0D0) AXDENU(LOCNUM) =
     *      -CATR(KRCIC+KLOCF(LOCNUM)) / (VELITE +
     *      CATD(KDCRV+KLOCF(LOCNUM)))
         END IF
C                                       Projective geometries
      IF ((KLOCL(LOCNUM).GE.0) .AND. (KLOCM(LOCNUM).GE.0)) THEN
         IF ((JX.EQ.5) .AND. (JY.EQ.5)) THEN
            AXFUNC(KLOCL(LOCNUM)+1,LOCNUM) = 2
            AXFUNC(KLOCM(LOCNUM)+1,LOCNUM) = 2
            END IF
         JX = MIN (JX, 3)
         JY = MIN (JY, 3)
         CALL H2CHR (4, 5, CATH(KHCTP+KLOCL(LOCNUM)*2), CTEST1)
         IF (CTEST1.EQ.'-SFL') CTEST1 = '-GLS'
         IF (CTEST1.NE.' ') AXFUNC(KLOCL(LOCNUM)+1,LOCNUM) = -100
         CALL H2CHR (4, 5, CATH(KHCTP+KLOCM(LOCNUM)*2), CTEST2)
         IF (CTEST2.EQ.'-SFL') CTEST2 = '-GLS'
         IF (CTEST2.NE.' ') AXFUNC(KLOCM(LOCNUM)+1,LOCNUM) = -101
         DO 30 I = 1,NGEOM
            IF (GEOM(I).EQ.CTEST1) AXFUNC(KLOCL(LOCNUM)+1,LOCNUM) =
     *         I + 1
            IF (GEOM(I).EQ.CTEST2) AXFUNC(KLOCM(LOCNUM)+1,LOCNUM) =
     *         I + 1
 30         CONTINUE
         END IF
C                                       Rational angle pair?
      IF ((JX.NE.JY) .OR. (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM).NE.
     *   AXFUNC(KLOCM(LOCNUM)+1,LOCNUM))) THEN
         IF ((JX.EQ.JY) .AND. (CTEST1.EQ.CTEST2)) THEN
            WRITE (MSGTXT,1041) CTEST1
         ELSE
            WRITE (MSGTXT,1040)
            KLOCL(LOCNUM) = -1
            KLOCM(LOCNUM) = -1
            JX = 0
            JY = 0
            END IF
         CALL MSGWRT (6)
         IF (KLOCL(LOCNUM).GE.0) AXFUNC(KLOCL(LOCNUM)+1,LOCNUM) = 0
         IF (KLOCM(LOCNUM).GE.0) AXFUNC(KLOCM(LOCNUM)+1,LOCNUM) = 0
C                                       Advance parms for geometries
      ELSE IF (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM).GT.6) THEN
C                                       oblique projection
         IF (CATD(KDCRV+KLOCM(LOCNUM)).NE.0.0D0) THEN
            MSGTXT = 'WARNING: OBLIQUE PROJECTION SELECTED DEC0 NOT 0'
            CALL MSGWRT (6)
            END IF
C                                       PHIP, ALPHAP, DELTAP
         IF (CATD(KDCRV+KLOCM(LOCNUM)).LT.0.0D0) THEN
            GEOMD1(LOCNUM) = PI
            GEOMD2(LOCNUM) = CATD(KDCRV+KLOCL(LOCNUM)) * DG2RAD
            GEOMD3(LOCNUM) = (90.0D0 + CATD(KDCRV+KLOCM(LOCNUM))) *
     *         DG2RAD
         ELSE
            GEOMD1(LOCNUM) = 0.0D0
            GEOMD2(LOCNUM) = (180.0D0 + CATD(KDCRV+KLOCL(LOCNUM)))
     *         * DG2RAD
            GEOMD3(LOCNUM) = (90.0D0 - CATD(KDCRV+KLOCM(LOCNUM))) *
     *         DG2RAD
            END IF
C                                       DELTAP
         END IF
C                                       Other parms
      ROT(LOCNUM) = CATR(KRCRT+KLOCM(LOCNUM))
C                                       No angle pair
      IF ((KLOCL(LOCNUM).LT.0) .OR. (KLOCM(LOCNUM).LT.0)) THEN
         ROT(LOCNUM) = 0.0
         AXTYP(LOCNUM) = 0
         CORTYP(LOCNUM) = 0
         LABTYP(LOCNUM) = 0
         IF (KLOCL(LOCNUM).GE.0) LABTYP(LOCNUM) = (2 * MIN (JX, 3) - 1)
     *      * (10 ** KLOCL(LOCNUM))
         IF (KLOCM(LOCNUM).GE.0) LABTYP(LOCNUM) = (2 * MIN (JX, 3)) *
     *      (10 ** KLOCM(LOCNUM))
C                                       normal order
      ELSE IF ((KLOCL(LOCNUM).EQ.0) .AND. (KLOCM(LOCNUM).EQ.1)) THEN
         AXTYP(LOCNUM) = 1
         CORTYP(LOCNUM) = 1
         LABTYP(LOCNUM) = 22 * JX - 1
         LMIN(LOCNUM) = (1.0 - CATR(KRCRP)) * CATR(KRCIC) * DG2RAD
         LMAX(LOCNUM) = (CATBLK(KINAX) - CATR(KRCRP)) * CATR(KRCIC)
     *      * DG2RAD
         MMIN(LOCNUM) = (1.0 - CATR(KRCRP+1)) * CATR(KRCIC+1) * DG2RAD
         MMAX(LOCNUM) = (CATBLK(KINAX+1) - CATR(KRCRP+1))*CATR(KRCIC+1)
     *      * DG2RAD
C                                       reverse order
      ELSE IF ((KLOCL(LOCNUM).EQ.1) .AND. (KLOCM(LOCNUM).EQ.0)) THEN
         AXTYP(LOCNUM) = 1
         CORTYP(LOCNUM) = 2
         SGNROT(LOCNUM) = -1
         LABTYP(LOCNUM) = 22 * JX - 10
         MMIN(LOCNUM) = (1.0 - CATR(KRCRP)) * CATR(KRCIC) * DG2RAD
         MMAX(LOCNUM) = (CATBLK(KINAX) - CATR(KRCRP)) * CATR(KRCIC)
     *      * DG2RAD
         LMIN(LOCNUM) = (1.0 - CATR(KRCRP+1)) * CATR(KRCIC+1) * DG2RAD
         LMAX(LOCNUM) = (CATBLK(KINAX+1) - CATR(KRCRP+1))*CATR(KRCIC+1)
     *      * DG2RAD
C                                       longitude with z
      ELSE IF (KLOCL(LOCNUM).LE.1) THEN
         AXTYP(LOCNUM) = 2 + KLOCL(LOCNUM)
         CORTYP(LOCNUM) = 3 + 2 * KLOCL(LOCNUM)
         LABTYP(LOCNUM) = (2 * JX - 1) * (10 ** KLOCL(LOCNUM))
         ZAXIS(LOCNUM) = KLOCM(LOCNUM) + 1
         KLOCA(LOCNUM) = KLOCM(LOCNUM)
         LMIN(LOCNUM) = (1.0 - CATR(KRCRP+KLOCL(LOCNUM))) *
     *      CATR(KRCIC+KLOCL(LOCNUM)) * DG2RAD
         LMAX(LOCNUM) = DG2RAD * (CATBLK(KINAX+KLOCL(LOCNUM)) -
     *      CATR(KRCRP+KLOCL(LOCNUM))) * CATR(KRCIC+KLOCL(LOCNUM))
         MMIN(LOCNUM) = DEPTH(KLOCM(LOCNUM)-1)
         MMIN(LOCNUM) = (MMIN(LOCNUM) - CATR(KRCRP+KLOCM(LOCNUM))) *
     *      CATR(KRCIC+KLOCM(LOCNUM)) * DG2RAD
         MMAX(LOCNUM) = MMIN(LOCNUM)
C                                       latitude with z
      ELSE IF (KLOCM(LOCNUM).LE.1) THEN
         AXTYP(LOCNUM) = 2 + KLOCM(LOCNUM)
         CORTYP(LOCNUM) = 4 + 2 * KLOCM(LOCNUM)
         LABTYP(LOCNUM) = (2 * JX) * (10 ** KLOCM(LOCNUM))
         ZAXIS(LOCNUM) = KLOCL(LOCNUM) + 1
         KLOCA(LOCNUM) = KLOCL(LOCNUM)
         SGNROT(LOCNUM) = -1
         MMIN(LOCNUM) = (1.0 - CATR(KRCRP+KLOCM(LOCNUM))) *
     *      CATR(KRCIC+KLOCM(LOCNUM)) * DG2RAD
         MMAX(LOCNUM) = DG2RAD * (CATBLK(KINAX+KLOCM(LOCNUM)) -
     *      CATR(KRCRP+KLOCM(LOCNUM))) * CATR(KRCIC+KLOCM(LOCNUM))
         LMIN(LOCNUM) = DEPTH(KLOCL(LOCNUM)-1)
         LMIN(LOCNUM) = (LMIN(LOCNUM) - CATR(KRCRP+KLOCL(LOCNUM))) *
     *      CATR(KRCIC+KLOCL(LOCNUM)) * DG2RAD
         LMAX(LOCNUM) = LMIN(LOCNUM)
C                                       2 "z" axes paired
      ELSE
         AXTYP(LOCNUM) = 4
         KLOCA(LOCNUM) = KLOCL(LOCNUM)
         KLOCB(LOCNUM) = KLOCM(LOCNUM)
         CORTYP(LOCNUM) = 1
         IF (KLOCL(LOCNUM).GT.KLOCM(LOCNUM)) CORTYP(LOCNUM) = 2
         IF (CORTYP(LOCNUM).EQ.2) SGNROT(LOCNUM) = -1
         LABTYP(LOCNUM) = 0
         END IF
C                                       Other axes to display?
      IF ((NAX.GT.2) .AND. ((KLOCA(LOCNUM).LT.2) .OR.
     *   (KLOCB(LOCNUM).LT.2))) THEN
         DO 105 I = 3,NAX
            IF (CATBLK(KINAX+I-1).GT.1) THEN
               IF ((KLOCA(LOCNUM).GE.2) .AND. (KLOCA(LOCNUM).NE.I-1))
     *            KLOCB(LOCNUM) = I - 1
               IF (KLOCB(LOCNUM).GE.2) GO TO 110
               KLOCA(LOCNUM) = I - 1
               END IF
 105        CONTINUE
         IF (KLOCA(LOCNUM).LT.2) KLOCA(LOCNUM) = KLOCL(LOCNUM)
         IF (KLOCA(LOCNUM).LT.2) KLOCA(LOCNUM) = KLOCM(LOCNUM)
         IF (KLOCA(LOCNUM).LT.2) KLOCA(LOCNUM) = KLOCS(LOCNUM)
         IF (KLOCA(LOCNUM).LT.2) KLOCA(LOCNUM) = KLOCF(LOCNUM)
         IF ((KLOCB(LOCNUM).LT.2) .OR. (KLOCB(LOCNUM).EQ.KLOCA(LOCNUM)))
     *      KLOCB(LOCNUM) = KLOCL(LOCNUM)
         IF ((KLOCB(LOCNUM).LT.2) .OR. (KLOCB(LOCNUM).EQ.KLOCA(LOCNUM)))
     *      KLOCB(LOCNUM) = KLOCM(LOCNUM)
         IF ((KLOCB(LOCNUM).LT.2) .OR. (KLOCB(LOCNUM).EQ.KLOCA(LOCNUM)))
     *      KLOCB(LOCNUM) = KLOCS(LOCNUM)
         IF ((KLOCB(LOCNUM).LT.2) .OR. (KLOCB(LOCNUM).EQ.KLOCA(LOCNUM)))
     *      KLOCB(LOCNUM) = KLOCF(LOCNUM)
         IF (KLOCB(LOCNUM).EQ.KLOCA(LOCNUM)) KLOCB(LOCNUM) = -1
         IF (KLOCA(LOCNUM).LT.2) KLOCA(LOCNUM) = -1
         END IF
C                                       get header values
 110  IF (KLOCA(LOCNUM).GE.2) THEN
         RPVAL(3,LOCNUM) = CATD(KDCRV+KLOCA(LOCNUM))
         RPLOC(3,LOCNUM) = CATR(KRCRP+KLOCA(LOCNUM))
         AXINC(3,LOCNUM) = CATR(KRCIC+KLOCA(LOCNUM))
         IF (AXINC(3,LOCNUM).EQ.0.0) AXINC(3,LOCNUM) = 1.0
         CALL H2CHR (8, 1, CATH(KHCTP+KLOCA(LOCNUM)*2), CTYP(3,LOCNUM))
         END IF
      IF (KLOCB(LOCNUM).GE.2) THEN
         RPVAL(4,LOCNUM) = CATD(KDCRV+KLOCB(LOCNUM))
         RPLOC(4,LOCNUM) = CATR(KRCRP+KLOCB(LOCNUM))
         AXINC(4,LOCNUM) = CATR(KRCIC+KLOCB(LOCNUM))
         IF (AXINC(4,LOCNUM).EQ.0.0) AXINC(4,LOCNUM) = 1.0
         CALL H2CHR (8, 1, CATH(KHCTP+KLOCB(LOCNUM)*2), CTYP(4,LOCNUM))
         END IF
C                                       Check ROT
      IF (ROT(LOCNUM).GT.180.0) ROT(LOCNUM) = ROT(LOCNUM) - 360.0
      IF (ROT(LOCNUM).LT.-180.0) ROT(LOCNUM) = ROT(LOCNUM) + 360.0
      IF (ABS(ROT(LOCNUM)).GT.90.0) THEN
         I = 1
         J = 2
         IF (AXTYP(LOCNUM).GT.1) J = 3
         IF (AXTYP(LOCNUM).EQ.3) I = 2
         IF (AXTYP(LOCNUM).EQ.4) J = 4
         IF (AXTYP(LOCNUM).EQ.4) I = 3
         AXINC(I,LOCNUM) = -AXINC(I,LOCNUM)
         AXINC(J,LOCNUM) = -AXINC(J,LOCNUM)
         ROT(LOCNUM) = ROT(LOCNUM) - SIGN(180.0, ROT(LOCNUM))
         END IF
C                                       Swap axes on ROT 90
      IF (.NOT.SWAPOK) GO TO 200
      IF ((AXTYP(LOCNUM).LT.1) .OR. (AXTYP(LOCNUM).GT.3)) GO TO 200
      IF ((ABS(ROT(LOCNUM)-90.0).GT.20.0) .AND.
     *   (ABS(ROT(LOCNUM)+90.0).GT.20.0)) GO TO 200
         SWAP = SIGN (1.0, ROT(LOCNUM))
         I = 1
         J = 2
         IF (AXTYP(LOCNUM).GT.1) J = 3
         IF (AXTYP(LOCNUM).EQ.3) I = 2
         IF (AXTYP(LOCNUM).EQ.4) J = 4
         IF (AXTYP(LOCNUM).EQ.4) I = 3
         DT = RPVAL(I,LOCNUM)
         RPVAL(I,LOCNUM) = RPVAL(J,LOCNUM)
         RPVAL(J,LOCNUM) = DT
         TS = CTYP(I,LOCNUM)
         CTYP(I,LOCNUM) = CTYP(J,LOCNUM)
         CTYP(J,LOCNUM) = TS
         ROT(LOCNUM) = ROT(LOCNUM) - 90.0 * SWAP
         AXINC(I,LOCNUM) = SWAP * AXINC(I,LOCNUM)
         AXINC(J,LOCNUM) = - SWAP * AXINC(J,LOCNUM)
         SGNROT(LOCNUM) = -SGNROT(LOCNUM)
         IF (CORTYP(LOCNUM).GT.0) CORTYP(LOCNUM) = CORTYP(LOCNUM) -
     *      ((-1)**CORTYP(LOCNUM))
         I = KLOCL(LOCNUM)
         KLOCL(LOCNUM) = KLOCM(LOCNUM)
         KLOCM(LOCNUM) = I
         IF (AXTYP(LOCNUM).EQ.1) LABTYP(LOCNUM) = 10 *
     *      MOD(LABTYP(LOCNUM),10) + LABTYP(LOCNUM)/10
         IF ((AXTYP(LOCNUM).LE.1) .OR. (AXTYP(LOCNUM).EQ.4)) GO TO 200
            IF (KLOCL(LOCNUM).LT.2) LABTYP(LOCNUM) = (2*JX-1) * (10 **
     *         KLOCL(LOCNUM))
            IF (KLOCM(LOCNUM).LT.2) LABTYP(LOCNUM) = (2*JX) * (10 **
     *         KLOCM(LOCNUM))
C                                       Set up fixed axis values
 200  SAXLAB(1,LOCNUM) = ' '
      SAXLAB(2,LOCNUM) = ' '
      NCHLAB(1,LOCNUM) = 0
      NCHLAB(2,LOCNUM) = 0
      KL = KLOCA(LOCNUM)
      J = 1
C                                       two extra labels loop
 210  IF (KL.GE.2) THEN
C                                       Linear axis
         IF (AXFUNC(KL+1,LOCNUM).LE.0) THEN
            DT = CATD(KDCRV+KL) + (ZDEPTH(KL-1,LOCNUM) - CATR(KRCRP+KL))
     *         * CATR(KRCIC+KL)
            CALL AXSTRN (CTYP(J+2,LOCNUM), DT, KL, NCHLAB(J,LOCNUM),
     *         SAXLAB(J,LOCNUM))
C                                       Felocity
         ELSE IF (AXFUNC(KL+1,LOCNUM).EQ.1) THEN
            DT = ZDEPTH(KL-1,LOCNUM) - CATR(KRCRP+KL)
            DT = CATD(KDCRV+KL) + CATR(KRCIC+KL)*DT / (1.0D0 +
     *         AXDENU(LOCNUM)*DT)
            CALL AXSTRN (CTYP(J+2,LOCNUM), DT, KL, NCHLAB(J,LOCNUM),
     *         SAXLAB(J,LOCNUM))
C                                       Positions: only 2 z axes now
         ELSE IF (AXTYP(LOCNUM).EQ.4) THEN
            DX = (ZDEPTH(KLOCL(LOCNUM)-1,LOCNUM) - RPLOC(3,LOCNUM)) *
     *         AXINC(3,LOCNUM)
            DY = (ZDEPTH(KLOCM(LOCNUM)-1,LOCNUM) - RPLOC(4,LOCNUM)) *
     *         AXINC(4,LOCNUM)
            COSR = COS (ROT(LOCNUM) * COND2R)
            SINR = SGNROT(LOCNUM) * SIN (ROT(LOCNUM) * COND2R)
            DT = (DX * COSR - DY * SINR) * COND2R
            DY = (DY * COSR + DX * SINR) * COND2R
            DX = DT
            COSR = RPVAL(3,LOCNUM) * COND2R
            SINR = RPVAL(4,LOCNUM) * COND2R
            CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), COSR, SINR, DX,
     *         DY, X, Y, II)
            IF (II.EQ.0) THEN
               X = X / COND2R
               Y = Y / COND2R
               CALL AXSTRN (CTYP(3,LOCNUM), X, KLOCL(LOCNUM),
     *            NCHLAB(1,LOCNUM), SAXLAB(1,LOCNUM))
               CALL AXSTRN (CTYP(4,LOCNUM), Y, KLOCM(LOCNUM),
     *            NCHLAB(2,LOCNUM), SAXLAB(2,LOCNUM))
               END IF
            GO TO 999
            END IF
C                                       loop for second label
         IF (J.NE.2) THEN
            J = 2
            KL = KLOCB(LOCNUM)
            GO TO 210
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('ANGLE-PAIR AXES INCONSISTENT OR PARTLY MISSING')
 1041 FORMAT ('ANGLE-PAIR AXES PROJECTION TYPE NOT RECOGNIZED ''',A,
     *   '''')
      END
