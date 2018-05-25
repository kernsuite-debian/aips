LOCAL INCLUDE 'WARP.INC'
C                                       Local include for WARP
      INTEGER   CATBLK(256), IT, VEL(32767), MVEL, NMAP
      HOLLERITH XNMI(3), XCLI(2), XNMJ(3), XCLJ(2), XTYPE
      CHARACTER NMI*12, CLI*6, NMJ*12, CLJ*6, TYPE*4, HED(6)*4, TY(2)*2
      REAL      SQI, DKI, SQJ, DKJ, BLC(7), TRC(7), TOL, APM(10),
     *   CPM(10), ERRV, WT, ASP(10), CSP(10), EPS
      COMMON /INPARM/ XNMI, XCLI, SQI, DKI, XNMJ, XCLJ, SQJ, DKJ, BLC,
     *   TRC, TOL, XTYPE, APM, CPM, ERRV
      COMMON /WRP/ IT, WT, ASP, CSP, VEL, MVEL, NMAP
      COMMON /MAPHDR/ CATBLK
      COMMON /MACHAC/ EPS
      COMMON /CHRCOM/ NMI, CLI, NMJ, CLJ, TYPE, HED, TY
LOCAL END
      PROGRAM WARP
C-----------------------------------------------------------------------
C! Fits model velocity field to spectral image
C# Map Modeling Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2009, 2015
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
C  Program WARP fits a model velocity field to an input frequency field,
C  and using a least squares' algorithm determines 5 parameters speci-
C  fying the change of position angle and inclination with varying
C  radius and the rotation velocity.
C      INNAME ... Image name(name).  blank=>any
C      INCLASS .. Image name(class). blank=>any
C      INSEQ .... Image name(seq).  0=>any
C      INDISK ... Disk drive # of image.  0=>any
C      IN2NAME
C      IN2CLASS
C      IN2SEQ
C      IN2DISK
C      BLC ...... Bottom Left hand pixel of subimage. 0,0=>1,1
C      TRC ...... Top Right hand pixel of subimage. 0,0=> maximum
C      FACTOR ... Criterion to stop fitting iterations. 0=>0.001
C      FUNCTYPE . Type of rotation curve. Blank : constant curve.
C      APARM .... (1),(2) central position;(3),(4) guesses a and c
C                 in pa = a + b * R, i = c + d * R. (6) guess Vmax.
C                 (9) 1:residual map, (10) 1:quick root find.
C                 In program: APM and ADP (single and double).
C      CPARM .... (4),(5),(6) Rmin,Rmax,Rinc. (7),(8) guesses b and d
C                 in pa = a + b * R, i = c + d * R. (9) 1,2 take smaller
C                 /larger in case of more than one root.
C                 In program: CPM, CSP.
C      PIXSTD ... Expected rms error in velocity at one pixel. 0=>10000
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, CL1*6, CL2*6, HEDINI(6)*4, CHEX*2
      INTEGER   M, N, NWA
      EXTERNAL  FCN
      INTEGER   NPAR, IRET, SCR(256), ISQ, JSQ, UID, MI, MJ, J, IPVT(8),
     *   IPVT2(8), INF, IVL, JVL, ICN, IER, IMIN, IMAX, UN, I
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION    CATD(64), ADP(10), FVEC(16384), FJAC(8, 8),
     *   WA(16440), WA1(8), WA2(8), WA3(8), DTL
      LOGICAL   T, RQK
      INCLUDE 'WARP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE (WA(1), WA1(1)), (WA(21), WA2(1)), (WA(41), WA3(1))
      DATA PRGM, NPAR /'WARP  ', 51/
      DATA CL1, CL2 /'RESID1','RESID2'/
      DATA T, NWA, UN /.TRUE., 16384, -32768/
      DATA HEDINI /'  p ','dpdr','  i ','didr','Vrot','Rmax'/
      DATA CHEX /'EX'/
C-----------------------------------------------------------------------
C                                       Initialize parts of /WRP/
      IT = 0
      WT = 1.0
      DO 5 I = 1,6
         HED(I) = HEDINI(I)
 5       CONTINUE
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL GTPARM (PRGM, NPAR, RQK, XNMI, SCR, IRET)
      IF (IRET.EQ.0) GO TO 10
         IF (IRET.EQ.1) GO TO 999
         IRET = 16
         WRITE (MSGTXT,1000)
         CALL MSGWRT (9)
 10   IF (RQK) CALL RELPOP (IRET, SCR, IER)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                         interpret input
      UID = NLUSER
      IF (TOL.EQ.0.0) TOL = 0.001
      IF (ERRV.EQ.0.0) ERRV = 10000.0
      IF (CPM(5).EQ.0.0) CPM(5) = 9999.0
      IF (CPM(9).EQ.0.0) CPM(9) = 1.0
      CALL H2CHR (12, 1, XNMI, NMI)
      CALL H2CHR (6, 1, XCLI, CLI)
      CALL H2CHR (12, 1, XNMJ, NMJ)
      CALL H2CHR (6, 1, XCLJ, CLJ)
      CALL H2CHR (4, 1, XTYPE, TYPE)
      N = 5
      DTL = TOL
      NMAP = 1
      TY(1) = TYPE(1:2)
C                                         machine accuracy
      CALL MACHIN (EPS)
C
      IVL = DKI + 0.01
      ISQ = SQI + 0.01
      CALL RDMAP (IVL, NMI, CLI, ISQ, UID, BLC, TRC, MVEL,
     *   VEL, UN, MI, SCR, IER)
      IF (IER.EQ.0) GO TO 20
         WRITE (MSGTXT,1060)
         CALL MSGWRT (8)
         GO TO 990
 20   WRITE (MSGTXT,1065) MI, NMI, CLI, ISQ, IVL
      CALL MSGWRT (5)
C
      MJ = 0
      IF (NMJ.EQ.'            ') GO TO 50
         JVL = DKJ + 0.01
         JSQ = SQJ + 0.01
         NMAP = NMAP + 1
         CALL COINC (IVL, NMI, CLI, ISQ, JVL, NMJ, CLJ, JSQ, UID, INF,
     *      SCR, IER)
         IF (IER.EQ.0) GO TO 30
            WRITE (MSGTXT,1010)
            CALL MSGWRT (8)
            GO TO 990
 30      IF (INF.EQ.0) GO TO 35
            WRITE (MSGTXT,1020) INF
            CALL MSGWRT (8)
            GO TO 990
 35      CALL RDMAP (JVL, NMJ, CLJ, JSQ, UID, BLC, TRC, MVEL,
     *      VEL(MVEL+1), UN, MJ, SCR, IER)
         IF (IER.EQ.0) GO TO 40
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            GO TO 990
C
 40      WRITE (MSGTXT,1065) MJ, NMJ, CLJ, JSQ, JVL
         CALL MSGWRT (5)
 50   M = MI + MJ
      IMIN = MAX (INT (CPM(4)), 0)
      IMAX = MIN (INT (CPM(5)), 9999)
      WRITE (MSGTXT,1070) IMIN, IMAX
      CALL MSGWRT (5)
C                                         units are km/s
      ERRV = ERRV / 1000.0
      APM(5) = APM(5) / 1000.0
      APM(6) = APM(6) / 1000.0
      IF (APM(7).EQ.0.0) APM(7) = 120.0
C                                         fill COMMON /WRP/
      DO 150 I = 1,10
         ASP(I) = APM(I)
         CSP(I) = CPM(I)
 150     CONTINUE
C
      ADP(1) = APM(3)
      ADP(2) = CPM(7)
      ADP(3) = APM(4)
      ADP(4) = CPM(8)
      ADP(5) = APM(6)
      IF (TY(1).NE.CHEX) GO TO 160
         N = N + 1
         ADP(6) = APM(7)
C                                         find least squares solution
 160  CALL LMSTR1 (FCN, M, N, ADP, FVEC, FJAC, 8, DTL, INF, IPVT,
     *   WA, NWA)
      WRITE (MSGTXT,1080) INF
      CALL MSGWRT (5)
      WRITE (MSGTXT,1090) IT
      CALL MSGWRT (5)
      WRITE (MSGTXT,1100) (ADP(I), I = 1,N)
      CALL MSGWRT (5)
C
      CALL FOUT (N, FJAC, 8, IPVT, IPVT2, WA1, WA2, WA3, ERRV)
      WRITE (MSGTXT,1100) (FJAC(J,J), J = 1,N)
      CALL MSGWRT (5)
C
      IF (APM(9).EQ.0.0) GO TO 330
C                                     first residual map
      CALL CHR2H (6, CL1, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = 0
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
      CALL MCREAT (IVL, ICN, SCR, IER)
      IF (IER.EQ.0) GO TO 180
         WRITE (MSGTXT,1130) IER
         CALL MSGWRT (8)
         GO TO 990
 180  BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 0.0
      TRC(2) = 0.0
      ISQ = CATBLK(KIIMS)
C                                      write first  residual map
      CALL WTMAP (IVL, NMI, CL1, ISQ, UID, BLC, TRC, MVEL, VEL,
     *   1, UN, FVEC, M, NMAP, SCR, IER)
      IF (IER.EQ.0) GO TO 190
         WRITE (MSGTXT,1140)
         CALL MSGWRT (5)
         GO TO 990
C                                      second residual map
 190  IF (NMJ.EQ.'            ') GO TO 330
         CALL CHR2H (6, CL2, KHIMCO, CATH(KHIMC))
         CATBLK(KIIMS) = 0
         CALL MCREAT (JVL, ICN, SCR, IER)
         IF (IER.EQ.0) GO TO 200
            WRITE (MSGTXT,1130) IER
            CALL MSGWRT (8)
            GO TO 990
 200     ISQ = CATBLK(KIIMS)
C                                      write second residual map
         CALL WTMAP (JVL, NMI, CL2, ISQ, UID, BLC, TRC, MVEL, VEL,
     *      2, UN, FVEC, M, NMAP, SCR, IER)
         IF (IER.EQ.0) GO TO 330
            WRITE (MSGTXT,1140)
            CALL MSGWRT (8)
            GO TO 990
C
 330  IRET = 0
 990  CALL DIETSK (IRET, RQK, SCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('GET PARAMETER ERROR ',I5)
 1010 FORMAT ('COINC ERROR')
 1020 FORMAT ('COINC: MAPS NOT COINCIDENT, INF = ',I1)
 1060 FORMAT ('RDMAP ERROR')
 1065 FORMAT (I5,' NONBLANKED IN ',A12,1X,A8,1X,I4,', vol. ',I2)
 1070 FORMAT ('RMIN=',I4,' RMAX=',I4,' ARCSEC')
 1080 FORMAT ('WARP : Information parameter # ',I2)
 1090 FORMAT (' After ',I3,' iterations the solution vector is ')
 1100 FORMAT (2(1X,F7.2,1X,F7.3),2(1X,F7.2))
 1130 FORMAT ('MCREAT ERROR: IER = ',I2)
 1140 FORMAT ('WTMAP ERROR')
      END
      SUBROUTINE FOUT (N, FJAC, LDIM, IPIV, JPIV, WA1, WA2, WA3, ERR)
C-----------------------------------------------------------------------
C   computation covariance matrix.
C-----------------------------------------------------------------------
      INTEGER   N, LDIM, I, IM1, J, IPIV(N), JPIV(N)
      REAL      ERR
      DOUBLE PRECISION    FJAC(LDIM,N), WA1(N), WA2(N), WA3(N), DTL
      LOGICAL   T
      DATA DTL, T /1.0D-6,.TRUE./
C-----------------------------------------------------------------------
C                                   zero lower triangle
      DO 120 I = 2,N
         IM1 = I - 1
         DO 100 J = 1, IM1
            FJAC(I,J) = 0.0D0
 100        CONTINUE
 120     CONTINUE
C                                         QR factorization of FJAC
      CALL QRFAC (N, N, FJAC, LDIM, T, JPIV, N, WA1, WA2, WA3)
C
      DO 140 J = 1,N
         FJAC(J,J) = WA1(J)
         I = JPIV(J)
         JPIV(J) = IPIV(I)
 140     CONTINUE
C
      CALL COVAR (N, FJAC, LDIM, JPIV, DTL, WA1)
      DO 160 I = 1,N
         FJAC(I,I) = ERR * SQRT (FJAC(I,I))
 160     CONTINUE
C
      RETURN
      END
      SUBROUTINE RDMAP (VL, NM, CL, IS, ID, BL, TR, MTOT, VEL, UN,
     *   M, SC, IER)
C-----------------------------------------------------------------------
C   Reads specified map into integer array VEL, substituting UN for
C   undefined values. The output integers MTOT and M are the total num-
C   ber of pixels, and the number of non blanked pixels, respectively.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER STAT*4, NM*12, CL*6, MTYPE*2
      INTEGER   VL, IS, VEL(32767), IERR, ID, L16, FND, ICN,
     *   CATBLK(256), SC(256), NA, NBUF, NX, NY, WIN(4), NXW, NYW, MX,
     *   MTOT, UN, BOF, J, JM, IY, IND, M, IX, IER
      REAL   BL(7), TR(7), CATR(256), DX, DY, RBF(MABFSS), VTEST, UNR
      DOUBLE PRECISION CATD(128)
      LOGICAL   F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /MAP/ WIN, NXW, NYW, DX, DY
      EQUIVALENCE (CATBLK, CATR, CATD)
      DATA F, STAT /.FALSE.,'READ'/
      DATA MX, BOF, L16 /32767,1,16/
C-----------------------------------------------------------------------
C                                         open input image
      MTYPE = 'MA'
      CALL MAPOPN (STAT, VL, NM, CL, IS, MTYPE, ID, L16, FND, ICN,
     *   CATBLK, SC, IERR)
      IF (IERR.NE.0) GO TO 990
C
      DX = CATR(KRCIC) * (-3600.0)
      DY = CATR(KRCIC+1) * 3600.0
      NA = CATBLK(KIDIM)
      NBUF = MABFSS * 2
C
      CALL WINDOW (NA, CATBLK(KINAX), BL ,TR, IERR)
      IF (IERR.NE.0) GO TO 990
C
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX + 1)
      WIN(1) = BL(1)
      WIN(2) = BL(2)
      WIN(3) = TR(1)
      WIN(4) = TR(2)
      NXW = WIN(3) - WIN(1) + 1
      NYW = WIN(4) - WIN(2) + 1
C
      IF ((REAL(NXW) * REAL(NYW)).GT.(REAL(MX) + 1.001)) GO TO 990
C                                          initialize reading
      MTOT = NXW * NYW
      CALL MINIT (STAT, L16, FND, NX, NY, WIN, RBF, NBUF, BOF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                         read image line by line
      J = 0
      JM = 0
      DO 130 IY = 1,NYW
C
         CALL MDISK (STAT, L16, FND, RBF, IND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                         undef. value is UN km/s.
         UNR = UN
         DO 120 IX = 1, NXW
            JM = JM + 1
C                                         real map
            IF (RBF(IND+IX-1).EQ.CATR(KRBLK)) THEN
               VTEST = UNR + SIGN (0.00001,UNR)
            ELSE
               VTEST = RBF(IND+IX-1) / 1000.0
               IF (VTEST.GT.32768) VTEST = UNR + SIGN (0.00001,UNR)
               END IF
            VEL(JM) = VTEST
            IF (VEL(JM).NE.UN)  J = J + 1
 120        CONTINUE
 130     CONTINUE
C                                         M (index J) # not blanked.
C                                         MTOT (index JM) total #.
      M = J
C                                         close input image
 990  IER = IERR
      CALL MAPCLS (STAT, VL, ICN, L16, FND, CATBLK, F, SC, IERR)
      IER = IER**2 + IERR**2
C
 999  RETURN
      END
      SUBROUTINE WTMAP (VL, NM, CL, IS, ID, BL, TR, MTOT, VEL, IVEL,
     *   UN, FVEC, M, NMAP, SC, IER)
C-----------------------------------------------------------------------
C  Writes part of array FVEC in a map. At each pixel in the map the
C  array VEL is incremented by 1 in the order : VEL(1),VEL(1+MTOT),
C  VEL(1+2*MTOT),..,VEL(1+(NMAP-1)*MTOT),VEL(2),VEL(2+MTOT),... etc.
C  Only if in VEL(J+(I-1)*MTOT) I equals IVEL an output pixel is
C  written.  This pixel is blanked if VEL(...) equals UN, otherwise the
C  current value of FVEC is written.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER  NM*12, CL*6, MTYPE*2
      INTEGER M
      INTEGER   VL, IS, VEL(32767), IERR, ID, L16, FND, ICN,
     *   CATBLK(256), SC(256), NA, NBUF, NX, NY, WIN(4), NXW, NYW,
     *   MTOT, UN, BOF, J, JM, IY, IND, I, IX, IVEL, IER, NMAP
      REAL   BL(7), TR(7), CATR(256), DX, DY, RBF(MABFSS), FMX, FMN
      DOUBLE PRECISION CATD(128), FVEC(M)
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      COMMON /MAP/ WIN, NXW, NYW, DX, DY
      EQUIVALENCE (CATBLK(1), CATR(1), CATD(1))
      DATA T /.TRUE./
      DATA BOF, L16 /1,16/
C-----------------------------------------------------------------------
C                                         open input image
      MTYPE = 'MA'
      CALL MAPOPN ('INIT', VL, NM, CL, IS, MTYPE, ID, L16, FND, ICN,
     *    CATBLK, SC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       initialize writing
      DX = CATR(KRCIC) * (-3600.0)
      DY = CATR(KRCIC+1) * 3600.0
      NA = CATBLK(KIDIM)
      NBUF = MABFSS * 2
C
      CALL WINDOW (NA, CATBLK(KINAX), BL ,TR, IERR)
      IF (IERR.NE.0) GO TO 990
C
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX + 1)
      WIN(1) = BL(1)
      WIN(2) = BL(2)
      WIN(3) = TR(1)
      WIN(4) = TR(2)
      NXW = WIN(3) - WIN(1) + 1
      NYW = WIN(4) - WIN(2) + 1
C
      CALL MINIT ('WRIT', L16, FND, NX, NY, WIN, RBF, NBUF, BOF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       new scaling for integer maps
      JM = 0
      J = 0
      FMN =  1.0E20
      FMX = -1.0E20
 210  IF (J.LT.M) THEN
         JM = JM + 1
         DO 230 I = 1, NMAP
            IF (VEL(JM+(I-1)*MTOT).NE.UN) THEN
               J = J + 1
               IF (I.EQ.IVEL) THEN
                  FVEC(J) = - FVEC(J) * 1000.0
                  IF (FMN.GT.FVEC(J))  FMN = FVEC(J)
                  IF (FMX.LT.FVEC(J))  FMX = FVEC(J)
                  END IF
               END IF
 230        CONTINUE
         GO TO 210
         END IF
      CATR(KRDMN) = FMN
      CATR(KRDMX) = FMX
C                                       write line by line
      J = 0
      I = 0
      JM = 1
      DO 320 IY = 1, NYW
         CALL MDISK ('WRIT', L16, FND, RBF, IND, IERR)
         IF (IERR.NE.0) GO TO 990
            IX = 0
 245        IF (IX.LT.NXW) THEN
               I = I + 1
               IF (I.GT.NMAP) THEN
               I = I - NMAP
               JM = JM + 1
               END IF
            IF (VEL(JM+(I-1)*MTOT).NE.UN) THEN
               J = J + 1
               IF (I.EQ.IVEL) THEN
                  IX = IX + 1
                  RBF(IND+IX-1) = FVEC(J)
                  END IF
            ELSE
               IF (I.EQ.IVEL) THEN
                  IX = IX + 1
                  RBF(IND+IX-1) = CATR(KRBLK)
                  END IF
               END IF
            GO TO 245
            END IF
 320     CONTINUE
C
      CALL MDISK  ('FINI', L16, FND, RBF, IND, IERR)
 990  IER = IERR
      CALL MAPCLS ('INIT', VL, ICN, L16, FND, CATBLK, T, SC, IERR)
      IER = IER**2 + IERR**2
C
 999  RETURN
      END
      SUBROUTINE FCN (M, N, X, FNC, DER, FLG)
C-----------------------------------------------------------------------
C   FCN is the subroutine required by subroutine LMSTR1; the latter per-
C   forms the least squares fit in task GAL.
C     M      I       input    # data points (adj. array. dim.).
C     N      I       input    # parameters (adj. array. dim.).
C     X      R(N)    input    current values of N parameters.
C     FNC    R(M)   output    M function values using current X.
C     DER    R(N)   output    derivatives w.r.t. X(1)...X(N) in
C                          point # FLG - 1.
C     FLG    I       input    1: FNC is returned, >1 : DER is
C                             returned.
C-----------------------------------------------------------------------
      CHARACTER CHEX*2
      INTEGER M, N
      INTEGER   FLG,I,J,NXW,NYW,VEL(32767),JM,IVEL,MVEL,NMAP,
     *   WIN(4),IT,JCOL,JROW,JRW,JTOT,NROOT,UNDEF,JC,IER
      CHARACTER NMI*12, CLI*6, NMJ*12, CLJ*6, TYPE*4, HED(6)*4, TY(2)*2
      REAL   COSP,SINP,CSH,SNH,SN2H,CS2H,CSI,SNI,SN2I,CST,
     *   RX,RY,R2,R,RR,RM,WT,GEW,DCTDCP,DRRDP,DCTDI,DRRDI,VRSNI,
     *   VR,VO,DX,DY,CSP(10),AUX1,AUX2,PHI,INC,DR,EPS,
     *   RTERM,DVRDRR,ASP(10),APM(10),ROOT(6),FACT,SFAC,TEK
      REAL   PROJ, DPDA,DPDB,DPDC,DPDD,DIDA,DIDB,DIDC,DIDD,DVDP,DVDI
      DOUBLE PRECISION X(N),FNC(M),DER(N),RAD,LN100
      LOGICAL   CALC,ALL
      EXTERNAL PROJ
      INCLUDE 'INCS:DMSG.INC'
      COMMON /MACHAC/ EPS
      COMMON /WRP/ IT,WT,ASP,CSP,VEL,MVEL,NMAP
      COMMON /CHRCOM/ NMI, CLI, NMJ, CLJ, TYPE, HED, TY
      COMMON /MAP/ WIN,NXW,NYW,DX,DY
      COMMON /ANG/ R,RX,RY,APM
      COMMON /FCNCOM/ NROOT,ROOT,IER,JM,IVEL
      DATA RAD /57.2957795130823208767/
      DATA LN100 /4.6051701859880914/
      DATA UNDEF /-32768/
      DATA CHEX /'EX'/
C-----------------------------------------------------------------------
      IF (FLG.NE.1) GO TO 80
C                                         X in degrees, APM in radians.
      DO 5 I = 1,4
         APM(I) = X(I) / RAD
 5       CONTINUE
C
      JTOT = 0
      JM = 0
      J = 0
 10   JM = JM + 1
      DO 60 IVEL = 1, NMAP
         JC = JM + (IVEL - 1) * MVEL
         IF (IVEL.NE.1)   GO TO 20
            CALC = .FALSE.
            ALL = .TRUE.
            DO 15 I = 1, NMAP
 15            ALL = ALL.AND.VEL(JM+(I-1)*MVEL).NE.UNDEF
 20      IF (VEL(JC).EQ.UNDEF) GO TO 60
         J = J + 1
         FNC(J) = 0.0
         IF (CALC)   GO TO 35
         CALC = .TRUE.
         JRW = (JM - 1) / NXW
         JCOL = JM - JRW * NXW + WIN(1) - 1
         JROW = JRW + WIN(2)
         RX = (JCOL - ASP(1)) * DX
         RY = (JROW - ASP(2)) * DY
         R2 = RY ** 2 + RX ** 2
         R = SQRT (R2)
         IF (R.GE.0.01) GO TO 30
            FNC(J) = (ASP(5) - VEL(JC)) * WT
            GO TO 60
 30      RX = RX / R
         RY = RY / R
         RM = MAX (CSP(4),R)
         RR = CSP(5)
         DR = CSP(6)
         CALL WORTEL (PROJ, RM, RR, DR, ROOT, NROOT, 4, IER)
 35      IF (IER.NE.0.OR.NROOT.EQ.0)  GO TO 60
         IF (ALL) GO TO 38
            RR = ROOT(1)
         GO TO 40
 38         IF (NROOT.EQ.1.AND.IVEL.EQ.1.AND.NMAP.GT.1) GO TO 60
            RR = ROOT(NMAP+1-IVEL)
C
 40      IF (RR.LT.CSP(4).OR.RR.GT.CSP(5))  GO TO 60
            JTOT = JTOT + 1
            INC = APM(3) + APM(4) * RR
            SNI = ABS (SIN (INC))
            CSI = ABS (COS (INC))
            SN2I = SNI * SNI
            PHI = APM(1) + APM(2) * RR
            CSH = COS (PHI) * RY - SIN (PHI) * RX
            CS2H = CSH * CSH
C                                       avoid singularity
            IF (CS2H.EQ.1.0.AND.SN2I.EQ.1.0) GO TO 60
               CST = CSH * CSI / SQRT (1.0 - CS2H * SN2I)
               IF (TY(1).EQ.CHEX)   AUX1 = X(6)
               IF (TY(1).NE.CHEX)   AUX1 = ASP(7)
               VR = X(5) * (1.0 - EXP (-LN100 * RR / AUX1))
               VO = ASP(5) + VR * SNI * CST
               FNC(J) = (VO - VEL(JC)) * WT
               GO TO 60
 60      CONTINUE
      IF (J.LT.M) GO TO 10
C
      IT = IT + 1
      IF (IT.NE.1) GO TO 70
         WRITE (MSGTXT,1000) (HED(I), I= 1, N)
         CALL MSGWRT (5)
 70   WRITE (MSGTXT,1010) IT,JTOT,(X(I), I = 1, N)
      CALL MSGWRT (5)
      JM = 1
      IVEL = 0
      GO TO 999
C                                       derivative in (JCOL,JROW)
 80   J = FLG - 1
 90   IVEL = IVEL + 1
      IF (IVEL.LE.NMAP) GO TO 100
         IVEL = IVEL - NMAP
         JM = JM + 1
 100  JC = JM + (IVEL - 1) * MVEL
      IF (IVEL.NE.1)   GO TO 110
         CALC = .FALSE.
         ALL = .TRUE.
         DO 105 I = 1, NMAP
 105         ALL = ALL.AND.VEL(JM+(I-1)*MVEL).NE.UNDEF
 110  IF (VEL(JC).EQ.UNDEF) GO TO 90
         DO 120 I = 1, N
            DER(I) = 0.0D0
 120        CONTINUE
         IF (CALC)   GO TO 140
         CALC = .TRUE.
         JRW = (JM - 1) / NXW
         JCOL = JM - JRW * NXW + WIN(1) - 1
         JROW = JRW + WIN(2)
         RX = (JCOL - ASP(1)) * DX
         RY = (JROW - ASP(2)) * DY
         IF (ABS(RX) .LT. 0.01)   RX = SIGN(0.01,RX)
         IF (ABS(RY) .LT. 0.01)   RY = SIGN(0.01,RY)
         R2 = RY ** 2 + RX ** 2
         R = SQRT (R2)
         RX = RX / R
         RY = RY / R
C
         RM = MAX (CSP(4), R)
         RR = CSP(5)
         DR = CSP(6)
         CALL WORTEL (PROJ, RM, RR, DR, ROOT, NROOT, 4, IER)
 140     IF (IER.NE.0.OR.NROOT.EQ.0)  GO TO 999
         IF (ALL) GO TO 150
            RR = ROOT(1)
         GO TO 160
 150        IF (NROOT.EQ.1.AND.IVEL.EQ.1.AND.NMAP.GT.1) GO TO 999
            RR = ROOT(NMAP+1-IVEL)
C
 160     IF (RR.LT.CSP(4).OR.RR.GT.CSP(5))  GO TO 999
            INC = APM(3) + APM(4) * RR
            SNI = SIN (INC)
            CSI = COS (INC)
            TEK = SIGN (1.0, SNI/CSI)
            SNI = ABS (SNI)
            CSI = ABS (CSI)
            SN2I = SNI * SNI
            PHI = APM(1) + APM(2) * RR
            COSP = COS (PHI)
            SINP = SIN (PHI)
            CSH =   COSP * RY - SINP * RX
            SNH = - COSP * RX - SINP * RY
            CS2H = CSH * CSH
            SN2H = SNH * SNH
C                                       avoid singularity
            IF (CS2H.EQ.1.0.AND.SN2I.EQ.1.0) GO TO 999
C
            FACT = 1.0 / (1.0 - CS2H * SN2I)
            SFAC = SQRT (FACT)
            CST = CSH * CSI * SFAC
            SFAC = SFAC ** 3
            IF (TY(1).EQ.CHEX)   AUX1 = X(6)
            IF (TY(1).NE.CHEX)   AUX1 = ASP(7)
            VR = X(5) * (1.0 - EXP (-LN100 * RR / AUX1))
            VRSNI = VR * SNI
C                                       some derivatives
         DCTDCP = CSI * SFAC
         DCTDI = - SNI * CSH * SN2H * SFAC * TEK
         DRRDP = - RR * SNH * CSH * SN2I * FACT
         DRRDI =   RR * SNI / CSI * SN2H * FACT * TEK
C                                       auxilliary expressions
         AUX1 = 1.0 - APM(2) * DRRDP
         AUX2 = 1.0 - APM(4) * DRRDI
         FACT = 1.0 / (AUX1 + AUX2 - 1.0)
C                                       derivatives
         DPDA = AUX2 * FACT
         DPDB = RR * DPDA
         DPDC = APM(2) * DRRDI * FACT
         DPDD = RR * DPDC
         DIDC = AUX1 * FACT
         DIDD = RR * DIDC
         DIDA = APM(4) * DRRDP * FACT
         DIDB = RR * DIDA
C
         DVDP = VRSNI * SNH * DCTDCP
         DVDI = X(5) * CST * CSI * TEK + VRSNI * DCTDI
         IF (TY(1).NE.CHEX)  GO TO 200
            DVRDRR = LN100 / X(6) * (X(5) - VR)
            RTERM = SNI * CST * DVRDRR
            DVDP = DVDP + RTERM * DRRDP
            DVDI = DVDI + RTERM * DRRDI
            DER(6) = - RR / X(6) * DVRDRR * WT
C                                       derivatives w.r.t. degrees
 200     GEW = WT / RAD
         DER(1) = (DVDP * DPDA + DVDI * DIDA) * GEW
         DER(2) = (DVDP * DPDB + DVDI * DIDB) * GEW
         DER(3) = (DVDI * DIDC + DVDP * DPDC) * GEW
         DER(4) = (DVDI * DIDD + DVDP * DPDD) * GEW
         DER(5) = SNI * CST * WT
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IT POINTS',2(2X,A4,4X,A4),3(2X,A4,1X),A4)
 1010 FORMAT (I2,1X,I5,1X,2(1X,F6.1,1X,F6.2),1X,F6.1,3X,F6.1)
      END
      SUBROUTINE WORTEL (FUNC, RMIN, RMAX, DRR, ROOT, NROOT, NS, IER)
C-----------------------------------------------------------------------
C     Searches for roots of the function F in the interval (RMIN,RMAX)
C     in steps of DRR.  The NROOT roots found are stored in ROOT.
C-----------------------------------------------------------------------
      EXTERNAL FUNC
      INTEGER   NROOT, NS, IER
      REAL      FUNC, RMIN, RMAX, DRR, ROOT(*)
C
      INTEGER   I, MEV
      REAL      FLO, FHI, RHI, RLO, EPS, WORT
      COMMON /MACHAC/ EPS
C-----------------------------------------------------------------------
      NROOT = 0
      I = 0
C                                        suffix Y: current, X: previous
      RHI = RMIN
      FHI = FUNC (RHI)
 100  RLO = RHI
      RHI = RLO + DRR
      IF (RHI.GT.RMAX) GO TO 999
         FLO = FHI
         FHI = FUNC (RHI)
         MEV = 50
C                                        has sign changed ?
         IF (FHI*FLO.GT.0.0) GO TO 100
            WORT = RHI
            CALL NULB (RLO, WORT, EPS, NS, FUNC, MEV, IER)
            IF (IER.NE.0) GO TO 999
               NROOT = NROOT + 1
               ROOT(NROOT) = WORT
               IF (NROOT.EQ.6) GO TO 999
            GO TO 100
 999  RETURN
      END
      REAL FUNCTION PROJ (X)
C-----------------------------------------------------------------------
C  X is the real radius of the specific point, R2 is
C  the squared projection of X onto the plane of the sky.
C-----------------------------------------------------------------------
      REAL   X, RX, RY, CSH, CS2H, CSI, CS2I, APM(10), PH, R
      COMMON /ANG/ R, RX, RY, APM
C-----------------------------------------------------------------------
C                                       position angle
      PH = APM(1) + APM(2) * X
C                                       azimuth angle in galaxy plane
      CSH =   COS (PH) * RY - SIN (PH) * RX
      CS2H =   CSH * CSH
C                                       avoid singularity
      IF (CS2H.NE.1.0) GO TO 10
         PROJ = R - X
         GO TO 999
C                                       inclination
 10   CSI = ABS (COS (APM(3) + APM(4) * X))
      CS2I = CSI * CSI
C
      PROJ = R - X * CSI / SQRT (1.0 - CS2H + CS2H * CS2I)
C
 999  RETURN
      END
