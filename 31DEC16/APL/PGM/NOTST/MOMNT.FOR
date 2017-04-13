LOCAL INCLUDE 'MOMNT.INC'
      REAL      BLC(7), TRC(7), ACUT, FCUT, PBPARM(7), CATOR(256), FI,
     *   RI, DI, FR, RR, DR, MROT
      DOUBLE PRECISION CATOD(128), FV, RV, DV, RA0, DE0
      HOLLERITH CATOH(256)
      INTEGER   CATOLD(256), FAX, RAX, DAX, IP(7)
      COMMON /PARAMS/ CATOLD, FV, RV, DV, RA0, DE0, BLC, TRC, ACUT,
     *   FCUT, PBPARM, FI, RI, DI, FR, RR, DR, MROT, FAX, RAX, DAX, IP
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
LOCAL END
      PROGRAM MOMNT
C-----------------------------------------------------------------------
C! Computes moments of a spectral cube using a cutoff
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2008-2009, 2015
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
C
C                +---------------------------------+
C                !                                 !
C                !            M O M N T            !
C                !                                 !
C                !  Version 2.3 - 6 December 1982  !
C                !                                 !
C                !           Arnold Rots           !
C                !                                 !
C                !  Version 2.4 - 26 October 1987  !
C                !                                 !
C                !  Scott Stevens and Neil Killeen !
C                !      University of Illinois      !
C                !                                 !
C                +---------------------------------+
C
C     This is a profile analysis task; it calculates moments
C     0 through 3 using the familiar cut-off/window mixed method:
C     if the smoothed value at a map point is below the cut-off,
C     the point is rejected.  The map may have 2 to 7 dimensions
C     the first of which has to be 'VE', 'FR', or 'CH'; a subimage
C     may be selected.  No smoothing is done over coords 4-7.
C     The smoothing functions may be Box (coords 1, 2, 3), Hanning
C     (coord 1), or Gaussian (coords 2, 3).
C     Desired moments can be selected by OUTCLASS (e.g., '0123',
C     '301', etc.; default = '012').  When the moment maps
C     are created they are given classes 'MOMn', n=0,1,2,3.
C     It probably will take a while to execute this task.
C
C     If possible, it will read the input map only once (into XBU).
C     The size of XBU is presently set to 4 Mbyte; this will fit
C     nicely if the swap and page slots are set to 1024 pages.  If
C     necessary the size of XBU can be reduced - at the expense of
C     a considerable increase of execution time for large data
C     cubes (so it should not be done lightly).  The program uses
C     a contiguous piece of XBU so as to minimize page swapping.
C     The number of words (4 bytes) used of XBU is the product of:
C        the number of profile points used + (velocity smoothing
C            kernel size) - 1       ; maximum: number of points
C                                     along first axis of cube
C        the equivalent number for the second axis of the cube
C        the size of the spatial smoothing kernel
C
C     Version 2.2: FASTIO set to .FALSE. if NXY=1  (ahr 821203)
C     Version 2.3: added REAL*4 MAX, MIN (ahr 821206)
C     Version 2.4: Modifications towards F77 Standard.  All expressions
C                  equating numeric variables to character strings have
C                  been eliminated. Single characters placed in
C                  LOGICAL*1 variables have been eliminated.
C                  CHARACTER variables and internal WRITE statements
C                  have been used in these changes.
C
C     INPUT ADVERBS :
C   USERID     -32000.0     32000.0    User ID.  0=>current user
C                                        32000=>all users
C   INNAME                             Input name(name). blank=>any
C   INCLASS                            Input name(class). blank=>any
C   INSEQ           0.0      9999.0    Input name(seq. #). 0=>any
C   INDISK          0.0         3.0    Input disk drive #. 0=>any
C   OUTNAME                            Output name(name).
C                                        blank=>INNAME
C   OUTCLASS                           Contains a string of numbers
C                                        (0-3) indicating which
C                                        moments are wanted.
C                                        blank=>'012'
C   OUTSEQ          0.0      9999.0    Output name(seq. #).
C                                        0=>lowest unique
C   OUTDISK         0.0         3.0    Output image disk drive #
C                                        0=>INDISK
C   BLC             0.0      8192.0    Bottom left corner of image
C                                        0=>entire image
C   TRC             0.0      8192.0    Top right corner of image
C                                        0=>entire image
C   FUNCTYPE                           Smoothing functions to be
C                                        used for blanking;
C                                        1st char refers to velocity
C                                        coord:  B:box, H:Hanning;
C                                        2nd char refers to spatial
C                                        coords:  B:box, G:Gausian.
C   CELLSIZE        0.0        11.0    Width of smoothing functions:
C                                        1st element: velocity coord
C                                        2nd element: spatial coords
C                                        Allowed ranges:
C                                        Vel. B/H: odd integer 1=>25
C                                        Spat. G: real 0.5=>6.0.
C                                              B: odd integer 1=>11
C   FLUX                               Threshold; if the smoothed
C                                        intensity at a map point
C                                        falls below this value, the
C                                        point is not used in the
C                                        moment calculations.
C      ICUT           ACUT          Abs(data) cutoff: if > 0, drop
C                                   data < ICUT in abs value.  If
C                                   < 0, drop data > ICUT in abs.
C     SPECIAL ERROR CODES :
C   ERROR = 11 :   Map too small for convolving functions
C           12 :   No moments 0=>3 specified
C           20 :   First coordinate is not velocity/frequency/channel#
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PRGNAM*6, NAME(5)*36, HILINE*72, UNIT(5)*8, MO(4)*6,
     *   SUBNOM(15)*6, IVE*2, IFE*2, IFR*2, ICH*2, CHTMP2*2, NAMEX*12,
     *   CLASSX*6, OPTYPE*4, OTYPE*8
      REAL   XBU(4000000)
      REAL   RP(41), MAXX, MINX, IN(4096,11), INCON(4096,11),
     *   OUT(MAXIMG,4), CATR(128), BLIN(7), TRIN(7), GSIG, KXY(11),
     *   KV(25), VI, BLPROF(4096), RTEMP
      HOLLERITH CATH(256), MAP, CLASO(2), PTYPE
      INTEGER   NMAX, ISEQ, IVOL, USID
      DOUBLE PRECISION CATD(128)
      INTEGER   IB(7), IT(7), NPARM,NSUB, JFIL, NV, NXY, ITEMP, ERROR,
     *   CATBLK(256), NV2, NXY2, NDIM, I, NRET, II, III, K, J, NK(7),
     *   ITYPE, L, K4, K5, K6, K7, M, IX, NXOUT, NYOUT, NP, NPC, LUN,
     *   LUN19, NN1, NN2, NN3, IROUND, INCNO, OUTCNO, INVOL, OUTVOL,
     *   OUTPTR, HLUN1, HLUN2, NAX
      CHARACTER*4 CRCTRN
      LOGICAL  GAUSS, HANN, MOM(4), NOTMOM(4), FASTIO, T
      DOUBLE PRECISION OFFS, V1, V2, VD, V0, VSIGN
      INCLUDE 'MOMNT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (CATBLK, CATR, CATD, CATH)
      DATA NMAX /4000000/
      DATA NPARM /40/,  PRGNAM /'MOMNT ' /
      DATA LUN19 /19/
      DATA UNIT /'M/S   ','HZ    ','CHAN  ',' M/S  ',' HZ   '/
      DATA MO /'MOM0  ', 'MOM1  ', 'MOM2  ', 'MOM3  '/
      DATA SUBNOM /'GTPARM', 'OPENCF', 'GETHDR', 'WRNGCO', 'KERNL3',
     *   'MP2SML', 'NO MOM', 'MAPCR ', 'LINEIN', 'BLANK ', 'MOMENT',
     *   'MAPIO ', 'MAPMAX', 'RDPLAN', 'MAPWIN'/
      DATA T /.TRUE./
      DATA HLUN1, HLUN2 /27,28/
      DATA IVE, IFE, IFR, ICH /'VE', 'FE', 'FR', 'CH'/
C-----------------------------------------------------------------------
C                                       Get inputs
      NSUB = 1
      JFIL = 1
      CALL TSKBEG (PRGNAM, NPARM, RP(2), ERROR)
      IF (ERROR.NE.0) GO TO 900
      RP(1) = NLUSER
C                                       open input, get header
      NSUB = 2
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RP(2), RP(5), RP(7), MAP, RP(8), RP(1), NAME(1))
      CALL OPENCF (LUN19, NAME(1), ERROR)
      IF (ERROR.NE.0) GO TO 900
      NSUB = 3
      CALL GETHDR (LUN19, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 900
      CALL FILNUM (LUN19, OUTPTR, ERROR)
      INVOL = FILTAB(POVOL,OUTPTR)
      INCNO = FILTAB(POCAT,OUTPTR)
C                                       get parameters
      CALL COPY (256, CATBLK, CATOLD)
      CALL RCOPY (7, RP(16), BLC)
      CALL RCOPY (7, RP(23), TRC)
      CALL RCOPY (7, RP(35), PBPARM)
      FCUT = RP(33)
      ACUT = RP(34)
C                                       Check first coordinate
      ERROR = 20
      NSUB = 4
      CALL H2CHR (2, 1, CATH(KHCTP), CHTMP2)
      ITYPE = 0
      IF (CHTMP2.EQ.IVE) ITYPE = 1
      IF (CHTMP2.EQ.IFE) ITYPE = 1
      IF (CHTMP2.EQ.IFR) ITYPE = 2
      IF (CHTMP2.EQ.ICH) ITYPE = 3
      IF (ITYPE.EQ.0) GO TO 900
C
      IF (CATBLK(KINAX).GT.4096) THEN
         MSGTXT = 'TOO MANY (> 4096) CHANNELS'
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       set up smoothing kernels
      ERROR = 0
      NSUB = 5
      GAUSS = .FALSE.
      HANN = .FALSE.
      CALL H2CHR (2, 1, RP(30), CRCTRN)
      IF (CRCTRN(1:1).EQ.'H') HANN = .TRUE.
      IF (CRCTRN(2:2).EQ.'G') GAUSS = .TRUE.
C
      ITEMP = MAX (1.01, RP(31)+0.1)
      NV = (MIN (25, ITEMP)/2)*2+1
      ITEMP = MAX (1.01, RP(32)+0.1)
      NXY = (MIN (11, ITEMP)/2)*2+1
      GSIG = MIN (6.0, MAX (0.5, RP(32)))
      IF (RP(31).LE.0.0) NV = 3
      IF (RP(32).LE.0.0) NXY = 5
      IF (RP(32).LE.0.0) GSIG = 2.0
      CALL KERNL3 (GAUSS, HANN, NXY, NV, GSIG, KXY, KV, ERROR)
      IF (ERROR.NE.0) GO TO 900
      NXY2 = NXY/2
      NV2 = NV/2
C                                       Image bounds
      ERROR = 11
      NSUB = 6
      NDIM = CATBLK(KIDIM)
      NK(1) = NV2
      NK(2) = NXY2
      NK(3) = NXY2
      DO 10 I = 4,7
         NK(I) = 0
 10      CONTINUE
      DO 20 I = 1,NDIM
         IB(I) = MAX (1+NK(I),IROUND(BLC(I)))
         IF (TRC(I).LE.0.0) TRC(I) = 9999.0
         IT(I) = MIN (CATBLK(KINAX+I-1)-NK(I), IROUND(TRC(I)))
         IF (IB(I).GT.IT(I)) GO TO 900
         BLC(I) = IB(I)-NK(I)
         TRC(I) = IT(I)+NK(I)
         BLIN(I) = BLC(I)
         TRIN(I) = TRC(I)
 20      CONTINUE
      DO 30 I = NDIM+1,7
         BLC(I) = 1.0
         TRC(I) = 1.0
         BLIN(I) = 1.0
         TRIN(I) = 1.0
         IB(I) = 1
         IT(I) = 1
 30      CONTINUE
C                                       Set some constants
      NP = IT(1)-IB(1)+1
      NXOUT = IT(2)-IB(2)+1
      NYOUT = IT(3)-IB(3)+1
      VI = CATR(KRCIC)
      V0 = CATD(KDCRV) - (CATR(KRCRP)-IB(1)+1.0)*VI
      V1 = CATD(KDCRV) + CATR(KRCIC) * (BLC(1) - CATR(KRCRP))
      V2 = CATD(KDCRV) + CATR(KRCIC) * (TRC(1) - CATR(KRCRP))
      VD = 2.0D0 * ABS ((V2 - V1) / (MAX (1.D-10, V1+V2)))
      IF (VD.GT.0.001) THEN
         OFFS = 0.0D0
      ELSE
         OFFS = (V1 + V2) / 2.0D0
         WRITE (MSGTXT,1030) OFFS
         CALL MSGWRT (7)
         END IF
      V0 = V0 - OFFS
C                                       Fast I/O
      FASTIO = .FALSE.
      NN1 = IROUND (TRC(1)-BLC(1))+1
      NN2 = NXY
      NN3 = IROUND (TRC(2)-BLC(2))+1
      IF (REAL (NN1)*REAL (NN2)*REAL (NN3).LE.FLOAT (NMAX))
     *   FASTIO = .TRUE.
      IF (NXY.LE.1) FASTIO = .FALSE.
C                                       Shift coord axes down in header
      K = KHCTP/2+1
      DO 40 I = 2,NDIM
         II = I-1
         III = I-2
         CATD(K+III) = CATD(K+II)
         CATD(KDCRV+III) = CATD(KDCRV+II)
         CATR(KRCIC+III) = CATR(KRCIC+II)
         CATR(KRCRP+III) = CATR(KRCRP+II) - REAL(IB(I)-1)
         CATR(KRCRT+III) = CATR(KRCRT+II)
         CATBLK(KINAX+III) = IT(I)-IB(I)+1
 40      CONTINUE
C                                       Do the other header stuff
      CATR(KRBLK) = FBLANK
      CATBLK(KIDIM) = NDIM-1
C                                       Which moments?
      ERROR = 12
      NSUB = 7
      DO 50 I = 1,4
         MOM(I) = .FALSE.
 50      CONTINUE
      CALL H2CHR (4, 1, RP(12), CRCTRN)
      DO 60 I = 1,4
         IF (CRCTRN(I:I).EQ.'0') MOM(1) = .TRUE.
         IF (CRCTRN(I:I).EQ.'1') MOM(2) = .TRUE.
         IF (CRCTRN(I:I).EQ.'2') MOM(3) = .TRUE.
         IF (CRCTRN(I:I).EQ.'3') MOM(4) = .TRUE.
 60      CONTINUE
C
      J = 0
      DO 70 I = 1,4
         NOTMOM(I) = .NOT.MOM(I)
         IF (MOM(I)) J = J + 1
 70      CONTINUE
      IF (J.LE.0) THEN
         MOM(1) = .TRUE.
         MOM(2) = .TRUE.
         MOM(3) = .TRUE.
         NOTMOM(1) = .FALSE.
         NOTMOM(2) = .FALSE.
         NOTMOM(3) = .FALSE.
         END IF
C                                       Construct moment maps names
      DO 80 I = 2,5
         CALL CHR2H (6, MO(I-1), 1, CLASO)
         CALL H2WAWA (RP(9), CLASO, RP(14), MAP, RP(15), RP(1), NAME(I))
 80      CONTINUE
C                                       Create and open moment maps
      IF (ITYPE.EQ.3) THEN
         CALL CHR2H (4, UNIT(ITYPE), 5, CATH(KHBUN))
         CALL CHR2H (1, '*', 4, CATH(KHBUN))
         END IF
C
      IF (ITYPE.LE.2) THEN
         CALL CHR2H (4, UNIT(ITYPE+3), 5, CATH(KHBUN))
         CALL CHR2H (1, '*', 5, CATH(KHBUN))
         END IF
C
      ERROR = 0
      LUN = LUN19
      DO 90 I = 1,4
         JFIL = JFIL + 1
         LUN = LUN + 1
         IF (MOM(I)) THEN
            NSUB = 8
            CALL MAPCR (NAME, NAME(JFIL), CATBLK, ERROR)
            IF (ERROR.NE.0) GO TO 900
            NSUB = 2
            CALL OPENCF (LUN, NAME(JFIL), ERROR)
            IF (ERROR.NE.0) GO TO 900
            END IF
         CALL CHR2H (8, UNIT(ITYPE), 1, CATH(KHBUN))
 90      CONTINUE
C                                       PBCORR parameters
      IF (PBPARM(1).GT.0.0) THEN
         FAX = 0
         RAX = 0
         DAX = 0
         NAX = CATOLD(KIDIM)
         DO 100 I = 1,NAX
            CALL H2CHR (8, 1, CATOH(KHCTP+2*(I-1)), OTYPE)
            IF (OTYPE(:4).EQ.'FREQ') THEN
               FAX = I
               FV = CATOD(KDCRV+I-1)
               FI = CATOR(KRCIC+I-1)
               FR = CATOR(KRCRP+I-1)
            ELSE IF ((OTYPE(:4).EQ.'FELO') .OR. (OTYPE(:4).EQ.'VELO'))
     *         THEN
               FAX = I
               VSIGN = 1.0D0
               IF (OPTYPE(:4).EQ.'VELO') VSIGN = -1.0D0
               FI = -CATOR(KRCIC+I-1) * CATOD(KDARV) /
     *            (VELITE + VSIGN * CATOD(KDCRV+I-1) + CATOR(KRCIC+I-1)
     *            * (CATOR(KRCRP+I-1) - CATOR(KRARP)))
               FV = CATOD(KDARV)
               FR = CATOR(KRARP)
            ELSE IF ((OTYPE(:2).EQ.'RA') .OR. (OPTYPE(2:4).EQ.'LON'))
     *         THEN
               RAX = I
               RV = CATOD(KDCRV+I-1) * DG2RAD
               RI = CATOR(KRCIC+I-1) * DG2RAD
               RR = CATOR(KRCRP+I-1)
            ELSE IF ((OTYPE(:3).EQ.'DEC') .OR. (OPTYPE(2:4).EQ.'LAT'))
     *         THEN
               DAX = I
               DV = CATOD(KDCRV+I-1) * DG2RAD
               DI = CATOR(KRCIC+I-1) * DG2RAD
               DR = CATOR(KRCRP+I-1)
               MROT = CATOR(KRCRT+I-1) * DG2RAD
               END IF
 100        CONTINUE
         IF (DAX*RAX*FAX.LE.0) THEN
            MSGTXT = 'FREQ/RA/DEC AXIS NOT FOUND: PBPARM TURNED OFF'
            CALL MSGWRT (7)
            PBPARM(1) = 0.0
         ELSE
            RA0 = CATOD(KDORA) * DG2RAD
            DE0 = CATOD(KDODE) * DG2RAD
            IF ((RA0.EQ.0.0D0) .AND. (DE0.EQ.0.0D0)) THEN
               RA0 = RV
               DE0 = DV
               END IF
            END IF
         END IF
C-----------------------------------------------------------------------
C                                       Now we start the real work
C                                       First loop on the useless axes
      DO 190 K7 = IB(7),IT(7)
      BLIN(7) = K7
      TRIN(7) = K7
      IP(7) = K7
      DO 180 K6 = IB(6),IT(6)
      BLIN(6) = K6
      TRIN(6) = K6
      IP(6) = K6
      DO 170 K5 = IB(5),IT(5)
      BLIN(5) = K5
      TRIN(5) = K5
      IP(5) = K5
      DO 160 K4 = IB(4),IT(4)
         BLIN(4) = K4
         TRIN(4) = K4
         IP(4) = K4
C                                       Prepare fast I/O
         IF (FASTIO) THEN
            JFIL = 1
            DO 102 I = 1,3
               BLIN(I) = BLC(I)
               TRIN(I) = TRC(I)
 102           CONTINUE
            NSUB = 15
            CALL MAPWIN (LUN19, BLIN, TRIN, ERROR)
            IF (ERROR.NE.0) GO TO 900
            NSUB = 14
            DO 104 J = 1,NXY-1
               CALL RDPLAN (LUN19, XBU, NN1, NN2, NN3, ERROR)
               IF (ERROR.NE.0) GO TO 900
 104           CONTINUE
            END IF
C                                       Start row loop
         BLIN(3) = BLC(3)
         TRIN(3) = BLC(3)+REAL(NXY-1)
         DO 150 M = 1,NYOUT
            IP(3) = IB(3) + M - 1
            IF (MOD(M-1,16).EQ.0) THEN
               WRITE (MSGTXT,1106) M
               CALL MSGWRT (1)
               END IF
            JFIL = 1
            IF (FASTIO) THEN
               NSUB = 14
               CALL RDPLAN (LUN19, XBU, NN1, NN2, NN3, ERROR)
               IF (ERROR.NE.0) GO TO 900
               END IF
            NSUB = 9
            BLIN(2) = BLC(2)
            TRIN(2) = BLIN(2)
C                                       prepare for first point of line
            IF (NXY.GT.1) THEN
               DO 110 J = 1,NXY-1
                  IX = IROUND(BLIN(2) - BLC(2)) + 1
                  CALL LINEIN (LUN19, BLIN, TRIN, NP, GAUSS, HANN, NXY,
     *               NV, KXY, KV, IX, IN, INCON, FASTIO, XBU, NN1, NN2,
     *               NN3, ERROR)
                  IF (ERROR.NE.0) GO TO 900
 110              CONTINUE
               END IF
C                                       column loop
            IP(1) = IB(1)
            DO 120 L = 1,NXOUT
               IP(2) = IB(2) + L - 1
               NSUB = 9
               IX = IROUND(BLIN(2) - BLC(2)) + 1
               CALL LINEIN (LUN19, BLIN, TRIN, NP, GAUSS, HANN, NXY, NV,
     *            KXY, KV, IX, IN, INCON, FASTIO, XBU, NN1, NN2, NN3,
     *            ERROR)
               IF (ERROR.NE.0) GO TO 900
               NSUB = 10
               CALL BLANK (FCUT, ACUT, NP, NPC, GAUSS, NXY, KXY,
     *            IN(1,NXY2+1), INCON, BLPROF, ERROR)
               IF (ERROR.NE.0) GO TO 900
               NSUB = 11
               CALL MOMENT (BLPROF, NP, NPC, FBLANK, V0, VI, OUT(L,1),
     *            OUT(L,2), OUT(L,3), OUT(L,4), ERROR)
               IF (ERROR.NE.0) GO TO 900
 120           CONTINUE
C                                       save an entire line
            NSUB = 12
            LUN = LUN19
            DO 140 L = 1,4
               JFIL = JFIL+1
               LUN = LUN+1
               IF (MOM(L)) THEN
                  CALL MAPIO ('WRIT', LUN, OUT(1,L), ERROR)
                  IF (ERROR.NE.0) GO TO 900
                  END IF
 140           CONTINUE
C                                       set window for next line
            BLIN(3) = BLIN(3)+1.0
            TRIN(3) = TRIN(3)+1.0
 150        CONTINUE
 160     CONTINUE
 170  CONTINUE
 180  CONTINUE
 190  CONTINUE
C                                       Put min/max in moment headers
C                                       Do history files
      LUN = LUN19
      CALL HIINIT (3)
      DO 210 L = 1,4
         LUN = LUN + 1
         JFIL = L + 1
         IF (MOM(L)) THEN
            WRITE (MSGTXT,1190) L
            CALL MSGWRT (1)
            CALL FILCLS (LUN)
            NSUB = 2
            CALL OPENCF (LUN, NAME(JFIL), ERROR)
            IF (ERROR.NE.0) GO TO 900
            NSUB = 13
            CALL MAPMAX (LUN, MAXX, MINX, ERROR)
            IF (ERROR.NE.0) GO TO 900
C                                       header for HI
            CALL GETHDR (LUN, CATBLK, ERROR)
            IF (ERROR.NE.0) GO TO 900
            CALL FILNUM (LUN, OUTPTR, ERROR)
            OUTVOL = FILTAB(POVOL,OUTPTR)
            OUTCNO = FILTAB(POCAT,OUTPTR)
            CALL FILCLS (LUN)
C                                       copy most header keywords
            CALL KEYPCP (INVOL, INCNO, OUTVOL, OUTCNO, 0, ' ', ERROR)
C                                       History: create and copy
            CALL HISCOP (HLUN1, HLUN2, INVOL, OUTVOL, INCNO, OUTCNO,
     *         CATBLK, WBUFF, IBUF, ERROR)
            IF (ERROR.GE.3) GO TO 200
C                                       add to history
            CALL WAWA2A (NAME(1), NAMEX, CLASSX, ISEQ, PTYPE, IVOL,
     *         USID)
            CALL HENCO1 (PRGNAM, NAMEX, CLASSX, ISEQ, IVOL, HLUN2,
     *         IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            CALL WAWA2A (NAME(L), NAMEX, CLASSX, ISEQ, PTYPE, IVOL,
     *         USID)
            CALL HENCOO (PRGNAM, NAMEX, CLASSX, ISEQ, IVOL, HLUN2,
     *         IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1191) TSKNAM, IB
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1192) TSKNAM
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1193) TSKNAM, IT
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1192) TSKNAM
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1194) TSKNAM, RP(30)
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1195) TSKNAM, FCUT
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            RTEMP = -ACUT
            WRITE (HILINE,1196) TSKNAM, ACUT
            IF (ACUT.LT.0.0) WRITE (HILINE,1197) TSKNAM, RTEMP
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF (ERROR.NE.0) GO TO 200
            WRITE (HILINE,1198) TSKNAM, NV, NXY
            CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
            IF ((ERROR.EQ.0) .AND. (GAUSS)) THEN
               WRITE (HILINE,1199) TSKNAM, GSIG
               CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
               END IF
C                                       end history
 200        CALL HICLOS (HLUN2, T, IBUF, I)
            IF (ERROR.NE.0) THEN
               WRITE (MSGTXT,1200) ERROR, L
               CALL MSGWRT (8)
               END IF
            END IF
 210     CONTINUE
      NRET = 0
      GO TO 990
C                                       Error exit
 900  WRITE (MSGTXT,1900) SUBNOM(NSUB), ERROR
      IF (NSUB.EQ.4) WRITE (MSGTXT,1901)
      IF (NSUB.EQ.6) WRITE (MSGTXT,1902) NV, NXY
      IF (NSUB.EQ.7) WRITE (MSGTXT,1903)
      CALL MSGWRT (8)
      CALL PRTNAM (NAME(JFIL), 8)
      NRET = 16
C                                       Exit
 990  CALL TSKEND (NRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING: FIRST MOMENT OFFSET BY',1PE13.5)
 1106 FORMAT ('Start output row',I5)
 1190 FORMAT ('Find maxima, do history: Moment',I2)
 1191 FORMAT (A6,'BLC=',6(I4,','),I4,'  / BOTTOM LEFT CORNER TO')
 1192 FORMAT (A6,40X,'/ OUTPUT')
 1193 FORMAT (A6,'TRC=',6(I4,','),I4,'  / TOP RIGHT CORNER TO')
 1194 FORMAT (A6,'FUNCTYPE=''',A2,'''  / SMOOTHING TYPE: X,Y & Z')
 1195 FORMAT (A6,'FLUX=',1PE12.4,' / USE ONLY T > FLUX')
 1196 FORMAT (A6,'ICUT =',1PE12.4,14X,'/ USE ONLY ABS(T) > ICUT ')
 1197 FORMAT (A6,'ICUT =',1PE12.4,14X,'/ USE ONLY ABS(T) < ICUT ')
 1198 FORMAT (A6,'WIDTH=',2I5,' / SMOOTHING SUPPORT WIDTH: X, Y&Z',
     *   ' (CELLS)')
 1199 FORMAT (A6,'GSIG=',F9.3,' / GAUSSIAN WIDTH IN Y&Z (CELLS)')
 1200 FORMAT ('WARNING: ERROR',I6,' WRITING HISTORY FILE',I3)
 1900 FORMAT (A6,'  ERROR NO',I6)
 1901 FORMAT ('FIRST AXIS MUST BE VELOCITY, FREQUENCY, OR CHANNEL')
 1902 FORMAT ('MAP TOO SMALL FOR CONVOLUTION SIZE:',2I5)
 1903 FORMAT ('NO OUTPUT IMAGES REQUESTED VIA OUTCLASS')
      END
      SUBROUTINE KERNL3 (GAUSS, HANN, NXY, NV, GSIG, KXY, KV, ERROR)
C-----------------------------------------------------------------------
C   KERNL3 calculates the smoothing kernels for blanking; they
C   are even calculated if GAUSS and/or HANN are FALSE, although
C   they are not used then.
C   Input parameters:
C      GAUSS   L       If true fill the spatial kernel with a Gaussian
C      HANN    L       If true fill the velocity kernel with a Hanning
C                      function
C      NXY     I       Width of spatial boxcar
C      NV      I       Width of velocity boxcar or Hanning
C      GSIG    R       FWHM of spatial Gaussian
C   Output parameters:
C      NXY     I       (IF GAUSS) width of Gauss kernel
C      KXY     R(11)   Spatial kernel
C      KV      R(25)   Velocity kernel
C      ERROR   I       Error indicator (inoperative)
C-----------------------------------------------------------------------
      REAL      KXY(*), KV(*), GSIG, X, S, Z
      INTEGER   NXY, NV, ERROR, I, NXY2, NV2, IROUND
      LOGICAL   GAUSS, HANN
C-----------------------------------------------------------------------
C                                       spatial gauss
      IF (GAUSS) THEN
         NXY = MIN(((IROUND(GSIG)*2)/2)*2+1, 11)
         NXY2 = NXY/2+1
         Z = -4.0*LOG(2.0)/(GSIG*GSIG)
         S = 0.0
         DO 10 I = 1,NXY
            X = NXY2-I
            KXY(I) = EXP (X*X*Z)
            S = S + KXY(I)
 10         CONTINUE
C                                       normalize
         S = 1.0 / S
         DO 20 I = 1,NXY
            KXY(I) = KXY(I)*S
 20         CONTINUE
C                                       spatial boxcar
      ELSE
         X = 1.0 / REAL(NXY)
         DO 30 I = 1,NXY
            KXY(I) = X
 30         CONTINUE
         END IF
C                                       velocity hanning
      IF ((HANN) .AND. (NV.GT.1)) THEN
         NV2 = NV/2+1
         Z = 1.0/REAL(NV2)
         X = Z*Z
         DO 40 I = 1,NV
            KV(I) = Z - ABS (REAL(NV2-I)) * X
 40         CONTINUE
C                                       velocity boxcar
      ELSE
         X = 1.0/REAL(NV)
         DO 50 I = 1,NV
            KV(I) = X
 50         CONTINUE
         END IF
      ERROR = 0
C
 999  RETURN
      END
      SUBROUTINE RDPLAN (LUN, XBU, LN1, LN2, LN3, ERROR)
C-----------------------------------------------------------------------
C   RDPLAN reads an entire (velocity,X) plane into array XBU.  Before it
C   does this the existing planes are shifted down in the third
C   coordinate.  The order of the coordinates in XBU is: Velocity, Y, X.
C   Input parameters:
C      LUN     I      LUN of the input map
C      LN1     I      Length of first dimension of XBU
C      LN2     I      Length of second dimension of XBU
C      LN3     I      Length of third dimension of XBU
C   In/out:
C      XBU     R(*)   Array containing a big chunk of the input map;
C                     structured as XBU(LN1,LN2,LN3)
C   Output
C      ERROR   I      Error indicator
C-----------------------------------------------------------------------
      INTEGER   LUN, ERROR, LN1, LN2, LN3
      REAL      XBU(LN1,LN2,LN3)
C
      INTEGER   I, J, K
      INCLUDE 'INCS:DITB.INC'
C-----------------------------------------------------------------------
C                                       line loop
      DO 30 J = 1,LN3
C                                       shift existing lines down in y
         IF (LN2.GT.1) THEN
            DO 20 K = 1,LN2-1
               DO 10 I = 1,LN1
                  XBU(I,K,J) = XBU(I,K+1,J)
 10               CONTINUE
 20            CONTINUE
            END IF
C                                       read in new line
         CALL MAPIO ('READ', LUN, XBU(1,LN2,J), ERROR)
         IF (ERROR.NE.0) GO TO 999
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE LINEIN (LUN, BLC, TRC, NP, G, H, NXY, NV, KXY, KV, IX,
     *   IN, INCON, FASTIO, XBU, LN1, LN2, LN3, ERROR)
C-----------------------------------------------------------------------
C   This subroutine does the following:
C       -Shift the contents of IN and INCON down by one line
C       -Set up the window for the input map
C       -Read NX2 lines, weight and stack
C       -Read one line, weight and stack, and store in IN
C       -Read NX2 lines, weight and stack
C       -Convolve stack in X and store at the bottom of INCON
C   If FASTIO=TRUE, it reads the input data from XBU, else from LUN.
C   Input parameters :
C      LUN    I        LUN of input map
C      BLC    R(7)     Window on input map
C      TRC    R(7)     Window on input map
C      NP     I        Number of points on output line in IN/INCON
C      G      L*1      If true, Gaussian convolution in X,Y
C                      if false, boxcar convolution
C      H      L*1      If true, Hanning convolution in velocity
C                      if false, boxcar convolution
C      NXY    I        Number of points in spatial kernel
C      NV     I        Number of points in velocity kernel
C      KXY    R(11)    Spatial kernel
C      KV     R(25)    Velocity kernel
C      IX     I        Index in buffer array
C      FASTIO L*1      If TRUE, use XBU for input
C      XBU    R(*)     A buffer containing a large piece of the
C                      input map; structured as XBU(LN1,LN2,LN3)
C      LN1    I        Length of first dimension in XBU
C      LN2    I        Length of second dimension in XBU
C      LN3    I        Length of third dimension in XBU
C   In/out:
C      IN     R(4096,11)   Measured spectra along the line MOMNT is
C                      currently working on
C      INCON  R(4096,11)   Convolved (in V and Y) spectra along the
C                      same line
C   Output parameters:
C      ERROR  I        Error indicator
C-----------------------------------------------------------------------
      INTEGER   LUN, NP, NXY, NV, IX, LN1, LN2, LN3, ERROR
      REAL      BLC(7), TRC(7), KXY(*), KV(*), IN(4096,*),INCON(4096,*),
     *   XBU(LN1,LN2,LN3)
      LOGICAL   G, H, FASTIO
C
      INTEGER   NPIN, NX2, NV2, I, J, K, N, I1, I2
      REAL      TEMP(4096), STACK(4096), X, Y, S, WSTACK(4096)
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       set some constants, set window
      NV2 = NV / 2
      NX2 = NXY / 2
      NPIN = NP + NV - 1
      IF (.NOT.FASTIO) THEN
         CALL MAPWIN (LUN, BLC, TRC, ERROR)
         IF (ERROR.NE.0) GO TO 999
         END IF
C                                       NXY = 1
      IF (NXY.LE.1) THEN
         IF (FASTIO) THEN
            DO 5 I = 1,NPIN
               STACK(I) = XBU(I,1,IX)
 5          CONTINUE
         ELSE
            CALL MAPIO ('READ', LUN, STACK, ERROR)
            IF (ERROR.NE.0) GO TO 999
            END IF
         DO 10 I = 1,NPIN
            IN(I,1) = STACK(I)
            IF (STACK(I).NE.FBLANK) THEN
               WSTACK(I) = 1.0
            ELSE
               WSTACK(I) = 0.0
               END IF
 10         CONTINUE
C                                       shift in and incon down; zero
C                                       stack
      ELSE
         DO 40 J = 1,NXY-1
            DO 30 I = 1,NP
               IN(I,J) = IN(I,J+1)
               INCON(I,J) = INCON(I,J+1)
 30            CONTINUE
 40         CONTINUE
         DO 50 I = 1,NPIN
            STACK(I) = 0.0
            WSTACK(I) = 0.0
 50         CONTINUE
C                                       read first nx2 lines
C                                       (what's being read is a subimage
C                                       in a plane along first (V) and
C                                       third (Y) axes)
         N = 0
         DO 180 K = 1,NX2
            N = N + 1
            IF (FASTIO) THEN
               DO 120 I = 1,NPIN
                  TEMP(I) = XBU(I,N,IX)
 120              CONTINUE
            ELSE
               CALL MAPIO ('READ', LUN, TEMP, ERROR)
               IF (ERROR.NE.0) GO TO 999
               END IF
C                                       spatial boxcar
            IF (.NOT.G) THEN
               DO 140 I = 1,NPIN
                  IF (TEMP(I).NE.FBLANK) THEN
                     STACK(I) = STACK(I) + TEMP(I)
                     WSTACK(I) = WSTACK(I) + 1.0
                     END IF
 140              CONTINUE
C                                       spatial gauss
            ELSE
               X = KXY(N)
               DO 160 I = 1,NPIN
                  IF (TEMP(I).NE.FBLANK) THEN
                     STACK(I) = STACK(I) + X*TEMP(I)
                     WSTACK(I) = WSTACK(I) + X
                     END IF
 160              CONTINUE
               END IF
 180        CONTINUE
C                                       center line; also has to be
C                                       filled into IN
         N = N+1
         IF (FASTIO) THEN
            DO 184 I=1,NPIN
               TEMP(I) = XBU(I,N,IX)
 184           CONTINUE
         ELSE
            CALL MAPIO ('READ', LUN, TEMP, ERROR)
            IF (ERROR.NE.0) GO TO 999
            END IF
         DO 190 I = 1,NP
            IN(I,NXY) = TEMP(I+NV2)
 190        CONTINUE
C                                       spatial gauss
         IF (G) THEN
            X = KXY(N)
            DO 200 I=1,NPIN
               IF (TEMP(I).NE.FBLANK) THEN
                  STACK(I) = STACK(I) + X*TEMP(I)
                  WSTACK(I) = WSTACK(I) + X
                  END IF
 200           CONTINUE
C                                       spatial boxcar
         ELSE
            DO 210 I = 1,NPIN
               IF (TEMP(I).NE.FBLANK) THEN
                  STACK(I) = STACK(I) + TEMP(I)
                  WSTACK(I) = WSTACK(I) + 1.0
                  END IF
 210           CONTINUE
            END IF
C                                       stack last NX2 lines
         DO 280 K = 1,NX2
            N = N + 1
            IF (FASTIO) THEN
               DO 234 I = 1,NPIN
                  TEMP(I) = XBU(I,N,IX)
 234              CONTINUE
            ELSE
               CALL MAPIO ('READ', LUN, TEMP, ERROR)
               IF (ERROR.NE.0) GO TO 999
               END IF
C                                       spatial gauss
            IF (G) THEN
               X = KXY(N)
               DO 240 I = 1,NPIN
                  IF (TEMP(I).NE.FBLANK) THEN
                     STACK(I) = STACK(I) + X*TEMP(I)
                     WSTACK(I) = WSTACK(I) + X
                     END IF
 240              CONTINUE
C                                       spatial boxcar
            ELSE
               DO 250 I = 1,NPIN
                  IF (TEMP(I).NE.FBLANK) THEN
                     STACK(I) = STACK(I) + TEMP(I)
                     WSTACK(I) = WSTACK(I) + 1.0
                     END IF
 250              CONTINUE
               END IF
 280        CONTINUE
C                                       normalize spatial boxcar
         IF (.NOT.G) THEN
            X = 1.0 / REAL(NXY)
            DO 290 I = 1,NPIN
               STACK(I) = X * STACK(I)
               WSTACK(I) = X * WSTACK(I)
 290           CONTINUE
            END IF
         END IF
C                                       velocity smooth
C                                       NV = 1
      IF (NV.EQ.1) THEN
         DO 310 I=1,NP
            IF (WSTACK(I).GT.0.0) THEN
               INCON(I,NXY) = STACK(I) / WSTACK(I)
            ELSE
               INCON(I,NXY) = FBLANK
               END IF
 310        CONTINUE
C                                       velocity boxcar
      ELSE IF (.NOT.H) THEN
         I1 = -1
         X = 1.0/REAL(NV)
         DO 340 J = 1,NP
            I1 = I1+1
            I2 = I1
            Y = 0.0
            S = 0.0
            DO 330 I = 1,NV
               I2 = I2+1
               IF (WSTACK(I2).GT.0.0) THEN
                  Y = Y + STACK(I2) / WSTACK(I2)
                  S = S + 1.0
                  END IF
 330           CONTINUE
            IF (S.GT.0.0) THEN
               INCON(J,NXY) = Y / S
            ELSE
               INCON(I,NXY) = FBLANK
               END IF
 340        CONTINUE
C                                       velocity Hanning
      ELSE
         I1 = -1
         DO 370 J = 1,NP
            I1 = I1+1
            I2 = I1
            Y = 0.0
            S = 0.0
            DO 360 I = 1,NV
               I2 = I2 + 1
               IF (WSTACK(I2).GT.0.0) THEN
                  Y = Y + KV(I) * STACK(I2) / WSTACK(I2)
                  S = S + KV(I)
                  END IF
 360           CONTINUE
            IF (S.GT.0.0) THEN
               INCON(J,NXY) = Y / S
            ELSE
               INCON(I,NXY) = FBLANK
               END IF
 370        CONTINUE
         END IF
C                                       update window and exit
      BLC(2) = BLC(2) + 1.0
      TRC(2) = TRC(2) + 1.0
      ERROR = 0
C
 999  RETURN
      END
      SUBROUTINE BLANK (FCO, ACO, NP, N, G, NX, KX, IN, INCON, OUT,
     *   ERROR)
C-----------------------------------------------------------------------
C   BLANK does the convolution in X direction on INCON, compares
C   with CO, and puts the blanked profile (from IN) into OUT.
C   Input parameters :
C      FCO     R            Flux cut-off value
C      ACO     R            Flux cut-off value on abs. value
C      NP      I            Number of points in profile
C      G       L            If true, Gaussian convolution;
C                           if false, boxcar
C      NX      I            Number of points in kernel
C      KX      R(11)        Convolution kernel
C      IN      R(4096)       Input profile
C      INCON   R(4096,11)    Input profiles smoothed in V and Y
C   Output parameters
C      N       I            Number of points used (unblanked) in prof
C      OUT     R(4096)      Blanked profile
C      ERROR   I            Error indicator (not yet used)
C-----------------------------------------------------------------------
      REAL      FCO, ACO, KX(*), IN(4096), INCON(4096,11), OUT(4096)
      INTEGER   NP, N, NX, ERROR
      LOGICAL   G
C
      REAL      TEMP(4096), X, SG, S, W, FC, AC
      INTEGER   I, J, IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'MOMNT.INC'
C-----------------------------------------------------------------------
C                                       Convolution in X
C                                       NX = 1
      IF (NX.LE.1) THEN
         DO 20 I = 1,NP
            TEMP(I) = INCON(I,1)
 20         CONTINUE
C                                       Boxcar
      ELSE IF (.NOT.G) THEN
         DO 50 I = 1,NP
            S = 0.0
            W = 0.0
            DO 40 J = 1,NX
               IF (INCON(I,J).NE.FBLANK) THEN
                  S = S + INCON(I,J)
                  W = W + 1.0
                  END IF
 40            CONTINUE
            IF (W.GT.0.0) THEN
               TEMP(I) = S / W
            ELSE
               TEMP(I) = FBLANK
               END IF
 50         CONTINUE
C                                       Gaussian
      ELSE
         DO 90 I = 1,NP
            S = 0.0
            W = 0.0
            DO 80 J = 1,NX
               IF (INCON(I,J).NE.FBLANK) THEN
                  X = KX(J)
                  S = S + X * INCON(I,J)
                  W = W + X
                  END IF
 80            CONTINUE
            IF (W.GT.0.0) THEN
               TEMP(I) = S / W
            ELSE
               TEMP(I) = FBLANK
               END IF
 90         CONTINUE
         END IF
C                                       Perform blanking
      N = 0
      SG = SIGN (1.0, ACO)
      FC = FCO
      AC = ACO
      IRET = 0
      DO 120 I = 1,NP
         OUT(I) = 0.0
         X = TEMP(I)
         IF (X.NE.FBLANK) THEN
            IF (PBPARM(1).GT.0.0) CALL MOMPB (I, FC, AC, IRET)
            IF ((X.GE.FC) .AND. (SG*ABS(X).GE.AC) .AND. (IRET.EQ.0))
     *         THEN
               OUT(I) = IN(I)
               N = N + 1
               END IF
            END IF
 120     CONTINUE
      ERROR = 0
C
 999  RETURN
      END
      SUBROUTINE MOMPB (IX, FCC, ICC, IRET)
C-----------------------------------------------------------------------
C   MOMPB computes the primary beam correction at the current pixel and
C   adjusts FCUT and ICUT for it.
C   Inputs:
C      IX     I      X pixel position
C   Output:
C      FCC    R      FCUT adjusted
C      ICC    R      ICUT adjusted
C      IRET   I      0 - okay, 1 outside the usable beam
C-----------------------------------------------------------------------
      INTEGER   IX, IRET
      REAL      FCC, ICC
C
      INTEGER   LPOS(7), LF, LR, LD, CATSAV(256), LY
      LOGICAL   OUTSID
      DOUBLE PRECISION DX, DY, DT, X, Y, LAMBDA, ANGLE
      REAL      PBCORF
      CHARACTER ARRAY*8
      INCLUDE 'MOMNT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE LF, LR, LD, LY, LAMBDA, ANGLE
      DATA LF, LR, LD, LY /3*0,300000/
C-----------------------------------------------------------------------
      CALL COPY (6, IP(2), LPOS(2))
      LPOS(1) = IX + IP(1) - 1
      IRET = 0
      CALL H2CHR (8, 1, CATH(KHTEL), ARRAY)
C                                       init common each plane
      IF (LPOS(2).LT.LY) THEN
        CALL COPY (256, CATBLK, CATSAV)
        LOCNUM = 1
        CALL COPY (256, CATOLD, CATBLK)
        CALL SETLOC (LPOS(3), .FALSE.)
        END IF
      LY = LPOS(2)
C                                       new frequency
      IF (LF.NE.LPOS(FAX)) THEN
         LF = LPOS(FAX)
         LAMBDA = FV + (LF - FR) * FI
         LAMBDA = VELITE / LAMBDA
         END IF
C                                       new coordinate
      IF ((LD.NE.LPOS(DAX)) .OR. (LR.NE.LPOS(RAX))) THEN
         DX = (LPOS(RAX) - RR) * RI
         DY = (LPOS(DAX) - DR) * DI
         DT = DX * COS (MROT) - DY * SIN (MROT)
         DY = DY * COS (MROT) + DX * SIN (MROT)
         DX = DT
         CALL NEWPOS (AXFUNC(KLOCL(LOCNUM)+1,LOCNUM), RA0, DE0, DX, DY,
     *      X, Y, IRET)
         IF (IRET.NE.0) THEN
            LD = -10
            GO TO 999
            END IF
         DT = SIN (DE0) * SIN (Y) + COS (DE0) * COS (Y) * COS (RA0-X)
         DT = MIN (1.0D0, DT)
         DT = MAX (-1.0D0, DT)
         ANGLE = RAD2DG * ACOS (DT)
         LD = LPOS(DAX)
         LR = LPOS(RAX)
         END IF
C                                       primary beam
      CALL PBCALC (ANGLE, LAMBDA, ARRAY, PBPARM(2), PBCORF, OUTSID)
      IF ((OUTSID) .OR. (PBCORF.LE.0.0) .OR. (PBCORF.LT.PBPARM(1))) THEN
         IRET = 1
      ELSE
         FCC = FCUT / PBCORF
         ICC = ACUT / PBCORF
         END IF
C
 999  RETURN
      END
      SUBROUTINE MOMENT (PROF, NP, NC, INDEF, V0, VI, M0, M1, M2, M3,
     *   ERROR)
C-----------------------------------------------------------------------
C   MOMENT calculates moments 0 through 3 for the profile PROF
C   Input parameters :
C      PROF      R(4096)       Profile
C      NP        I            Number of points in profile
C      NC        I            Number of unblanked points in profile
C      INDEF     R            Indefinite value
C      V0        D            Velocity of first point in profile
C      VI        R            Velocity increment
C   Output parameters:
C      M0        R            Value of zeroth moment
C      M1        R            Value of first moment
C      M2        R            Value of second moment
C      M3        R            Value of third moment
C      ERROR     I            Error indicator (presently unused)
C-----------------------------------------------------------------------
      DOUBLE PRECISION V0
      REAL      PROF(4096), INDEF, VI, M0, M1, M2, M3
      INTEGER   NP, NC, ERROR
C
      INTEGER   I
      REAL      SY, SYI, SYI2, SYI3, X, Y, Z, DDV
C-----------------------------------------------------------------------
C                                       All points blanked
      M0 = 0.0
      M1 = INDEF
      M2 = INDEF
      M3 = INDEF
      ERROR = 0
C                                       Check complete blank; zero sums
      IF (NC.GT.0) THEN
         DDV = ABS(VI)
         SY = 0.0
         SYI = 0.0
         SYI2 = 0.0
         SYI3 = 0.0
C                                       Form sums
         DO 20 I = 1,NP
            Y = PROF(I)
            IF ((Y.NE.0.0) .AND. (Y.NE.INDEF)) THEN
               X = I
               SY = SY + Y
               SYI = SYI + Y*X
               SYI2 = SYI2 + Y*X*X
               SYI3 = SYI3 + Y*X*X*X
               END IF
 20         CONTINUE
C                                       Calculate moments
         IF (ABS(SY).GT.1.0E-30) THEN
            M0 = DDV * SY
            SYI = SYI / SY
            IF ((SYI.GE.1) .AND. (SYI.LE.NP)) THEN
               M1 = VI * SYI + V0
               M2 = 0.0
               M3 = 0.0
               SYI2 = SYI2 / SY
               X = SYI2 - SYI * SYI
               IF (X.GT.0.0) M2 = DDV * SQRT (X)
               SYI3 = SYI3 / SY
               X = SYI3 - 3.0 * SYI2 * SYI + 2.0 * SYI * SYI * SYI
               Z = SIGN(1.0, X)
               X = ABS(X)
               IF (X.GT.1.0E-30) M3 = Z * DDV * EXP(0.3333333 * LOG(X))
               END IF
            END IF
         END IF
C
 999  RETURN
      END
