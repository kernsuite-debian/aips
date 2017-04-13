LOCAL INCLUDE 'SMOTH.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   CSIZE
      PARAMETER (CSIZE = 25)
LOCAL END
      PROGRAM SMOTH
C-----------------------------------------------------------------------
C! Smooths an image by brute force convolution
C# Map Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003, 2008-2010
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
C                ---------------------------------
C                !                               !
C                !           S M O T H           !
C                !                               !
C                ! Version 2.3 - 23 January 1984 !
C                !                               !
C                !          Arnold Rots          !
C                !                               !
C                ---------------------------------
C
C   This task smoothes a subimage of an up to 7-dimensional map.  BLC
C   and TRC set the boundaries of the subimage.  In addition it will
C   skip XINC-1 and YINC-1 points in the first two dimensions in between
C   the calculated grid points.  The first two axes have to be LL and
C   MM.  BMAJ, BMIN, and BPA give the beam of the input map (it uses the
C   catalogd header items if BMAJ=0.0); AMAJ, AMIN, and APA give the
C   beam of the output map.  This program represents a brute-force
C   approach and does not care about the amount of memory used.
C      Version 2.1: Fixed beam parameters for XINC > 1  -  821214 ahr
C      Version 2.2: Allowed RA--- and DEC___ coordinates  -  831114 ahr
C      Version 2.3: Fixed specified input beam p.a. and changed
C                   edge to indefinite  -  840123
C   INPUT ADVERBS :
C   USERID     -32000.0     32000.0    User ID.  0=>current user
C                                        32000=>all users
C   INNAME                             Input name(name). blank=>any
C   INCLASS                            Input name(class). blank=>any
C   INSEQ           0.0      9999.0    Input name(seq. #). 0=>any
C   INDISK          0.0         3.0    Input disk drive #. 0=>any
C   OUTNAME                            Output name(name).
C                                        blank=>INNAME
C   OUTCLASS                           Output name(class).
C                                        blank=>INCLASS
C   OUTSEQ          0.0      9999.0    Output name(seq. #).
C                                        0=>lowest unique
C   OUTDISK         0.0         3.0    Output image disk drive #
C                                        0=>INDISK
C   BLC             0.0      8192      Bottom left corner of image
C                                        0=>entire image
C   TRC             0.0      8192      Top right corner of image
C                                        0=>entire image
C   XINC            1.0        10.0    Select every XINC col. 0=>1
C   YINC            1.0        10.0    Select every YINC rows. 0=>1
C   BMAJ            0.0      9999.0    Beam major axis FWHP of
C                                      input map in arcsec. 0=>use
C                                      catalog header value
C   BMIN            0.0      9999.0    Same, minor axis FWHP
C   BPA          -180.0       180.0    Same, position angle in degr.
C   SCALR1          0.0      9999.0    Beam major axis FWHP of
C                                      output map in arcsec.
C   SCALR2          0.0      9999.0    Same, minor axis FWHP
C   SCALR3       -180.0       180.0    Same, position angle in degr.
C-----------------------------------------------------------------------
      INCLUDE 'SMOTH.INC'
C
      CHARACTER PRGNAM*6, NAME(2)*36, HILINE*72, SUBS(13)*6, CHTMP*2,
     *   MNAME*12, MCLASS*6, PTYPE*2
      HOLLERITH MAP
      INTEGER   NX, NY, MSEQ, MDISK, MUSID
      REAL      RP(40), IN(MAXIMG,25), OUT(MAXIMG), XK(CSIZE,CSIZE),
     *   XX(CSIZE), YY(CSIZE), BLC(7), TRC(7), XINC, YINC, BIN(7),
     *   TIN(7), XMAX, XMIN, AMAJ, AMIN, APA, BMAJ, BMIN, BPA, CMAJ,
     *   CMIN, CPA, DMAJ, DMIN, DPA, AREA, DX, DY, X, RADEG, ARSRAD, WS
      INTEGER   I, J, K, L, M, II, JJ, K3, K4, K5, K6, K7, I1, I2, J0,
     *   J1, J2, J3, I1IN, J1IN, NX2, NY2, NXSK, NYSK, IB(7), IT(7),
     *   NXIN, NYIN, NXOUT, NYOUT, OUTPTR, INVOL, INCNO, OUTVOL, OUTCNO,
     *   ERROR, NSUB, LUN18, LUN19, NRET, NPARM, JNAM, HLUN1, HLUN2,
     *   IROUND
      LOGICAL   SEPAR, T
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (BLC(1),RP(16)), (TRC(1),RP(23))
      EQUIVALENCE (XINC,RP(30)), (YINC,RP(31))
      EQUIVALENCE (BMAJ,RP(32)), (BMIN,RP(33)), (BPA,RP(34))
      EQUIVALENCE (AMAJ,RP(35)), (AMIN,RP(36)), (APA,RP(37))
      DATA NRET, LUN18, LUN19 /0,18,19/
      DATA PRGNAM /'SMOTH '/
      DATA HLUN1, HLUN2 /27,28/
      DATA T /.TRUE./
      DATA SUBS /'GTPARM', 'OPENCF', 'KERNEL',
     *           'MAPCR ', 'MAPIO ', 'MAPWIN',
     *           'MAPMAX', 'FILCLS', 'GETHDR',
     *           'NOT LM', 'GDECON', 'KER>MP',
     *           'INC>KR'/
      DATA RADEG, ARSRAD /57.2957795, 206264.8062/
C-----------------------------------------------------------------------
      NPARM = 36
      NSUB = 1
      CALL TSKBEG (PRGNAM, NPARM, RP(2), ERROR)
      IF (ERROR.NE.0) GO TO 900
      MSGTXT = 'YOU ARE USING A VERY NON-STANDARD TASK'
      CALL MSGWRT (2)
      RP(1) = NLUSER
C                                       Open input, get header
      JNAM = 1
      NSUB = 2
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RP(2), RP(5), RP(7), MAP, RP(8), RP(1), NAME(1))
      CALL OPENCF (LUN18, NAME(1), ERROR)
      IF (ERROR.NE.0) GO TO 905
      NSUB = 9
      CALL GETHDR (LUN18, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 905
      CALL FILNUM (LUN18, OUTPTR, ERROR)
      INVOL = FILTAB(POVOL,OUTPTR)
      INCNO = FILTAB(POVOL,OUTPTR)
C                                       set window, check coords
      DO 10 I = 1,CATBLK(KIDIM)
         BLC(I) = MAX (BLC(I), 1.0)
         IF (TRC(I).LE.0.0) TRC(I) = CATBLK(KINAX+I-1)
         TRC(I) = MAX (BLC(I), MIN (TRC(I),
     *      REAL(CATBLK(KINAX+I-1))))
 10      CONTINUE
      DO 20 I = CATBLK(KIDIM)+1,7
         BLC(I) = 1.0
         TRC(I) = 1.0
 20      CONTINUE
      DO 30 I = 1,7
         IB(I) = IROUND(BLC(I))
         IT(I) = IROUND(TRC(I))
 30      CONTINUE
      ERROR = 11
      NSUB = 10
      CALL H2CHR (2, 1, CATH(KHCTP), CHTMP)
      IF ((CHTMP.NE.'LL') .AND. (CHTMP.NE.'RA')) THEN
         MSGTXT = 'FIRST AXIS IS NOT RA'
         GO TO 900
         END IF
      CALL H2CHR (2, 1, CATH(KHCTP+2), CHTMP)
      IF ((CHTMP.NE.'MM') .AND. (CHTMP.NE.'DE')) THEN
         MSGTXT = 'SECOND AXIS IS NOT DEC'
         GO TO 900
         END IF
C                                       Set up kernel
      ERROR = 12
      NSUB = 11
      IF (XINC.LE.1.0) XINC = 1.0
      IF (YINC.LE.0.9) YINC = XINC
      NXSK = IROUND(XINC)
      NYSK = IROUND(YINC)
      XINC = NXSK
      YINC = NYSK
      DX = -CATR(KRCIC)/RADEG
      DY = CATR(KRCIC+1)/RADEG
      APA = APA/RADEG
      BPA = BPA/RADEG
      IF (BMAJ.LE.0.0) THEN
         BMAJ = CATR(KRBMJ)*ARSRAD/RADEG
         BMIN = CATR(KRBMN)*ARSRAD/RADEG
         BPA = CATR(KRBPA)/RADEG
         END IF
      CALL GDECON (AMAJ,AMIN,APA, BMAJ,BMIN,BPA, CMAJ,CMIN,CPA)
      IF ((CMAJ.LE.0.0) .OR. (CMIN.LE.0.0)) THEN
         MSGTXT = 'BEAM DECONVOLUTION FAILS'
         GO TO 900
         END IF
      NSUB = 3
      CALL GASTPX (BMAJ,BMIN,BPA, DMAJ,DMIN,DPA, DX,DY)
      AREA = DMAJ*DMIN
      IF (AREA.GT.0.0) THEN
         CALL GASTPX (AMAJ,AMIN,APA, DMAJ,DMIN,DPA, DX,DY)
         AREA = DMAJ*DMIN/AREA
         END IF
      IF (AREA.LE.0.0) THEN
         MSGTXT = 'BEAM AREA(S) EQUAL ZERO'
         GO TO 900
         END IF
      CALL GASTPX (CMAJ,CMIN,CPA, DMAJ,DMIN,DPA, DX,DY)
      IF (ABS (DMAJ/DMIN-1.0) .LE. 0.01) DPA = 0.0
      I1 = IROUND (DPA*RADEG*5.0)
      SEPAR = (MOD (I1, 450).EQ.0)
      CALL KERNEL (CSIZE, XK, NX, NY, DMAJ, DMIN, DPA, ERROR)
      IF (ERROR.NE.0) GO TO 905
      IF ((NXSK.GE.NY) .OR. (NYSK.GE.NX)) THEN
         NSUB = 13
         ERROR = 13
         WRITE (MSGTXT,1030) NXSK, NYSK, NX, NY
         GO TO 900
         END IF
      NX2 = NX / 2
      NY2 = NY / 2
      X = XK(NX2+1,NY2+1)
      DO 45 I = 1,MAX(NX,NY)
         XX(I) = XK(I,NY2+1)
         YY(I) = XK(NX2+1,I) / X
 45   CONTINUE
      NXIN = CATBLK(KINAX)
      NYIN = CATBLK(KINAX+1)
      NXOUT = (IT(1) - IB(1)) / NXSK + 1
      NYOUT = (IT(2) - IB(2)) / NYSK + 1
C                                       Output name
      IF (RP(15).LE.0.4) RP(15) = RP(8)
      CALL H2WAWA (RP(9), RP(12), RP(14), MAP, RP(15), RP(1), NAME(2))
C                                       Set header items for output map
      JNAM = 2
      CATR(KRBMJ) = AMAJ * RADEG / (ARSRAD*XINC)
      CATR(KRBMN) = AMIN * RADEG / (ARSRAD*XINC)
      CATR(KRBPA) = APA*RADEG
      CATBLK(KINAX) = NXOUT
      CATBLK(KINAX+1) = NYOUT
      IF (CATBLK(KIDIM).GE.3) THEN
         DO 50 I = 3,CATBLK(KIDIM)
            BIN(I) = IB(I)
            TIN(I) = IT(I)
            CATBLK(KINAX+I-1) = IT(I)-IB(I)+1
            CATR(KRCRP+I-1) = CATR(KRCRP+I-1)-BIN(I)+1.0
 50         CONTINUE
         END IF
      CATR(KRCRP) = (CATR(KRCRP)-BLC(1))/XINC + 1.0
      CATR(KRCRP+1) = (CATR(KRCRP+1)-BLC(2))/YINC + 1.0
      CATR(KRCIC) = CATR(KRCIC)*XINC
      CATR(KRCIC+1) = CATR(KRCIC+1)*YINC
      CATR(KRBLK) = FBLANK
C                                       create, open output map
      NSUB = 4
      CALL MAPCR (NAME(1), NAME(2), CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 905
      NSUB = 2
      CALL OPENCF (LUN19, NAME(2), ERROR)
      IF (ERROR.NE.0) GO TO 905
C                                       Set looping indices
      ERROR = 14
      NSUB = 12
      X = REAL (NX2+1-IB(1)) / XINC + 1.0
      I1 = INT (X)
      IF (X-I1.GT.0.0) I1 = I1 + 1
      I1 = MAX (1, I1)
      X = REAL (NY2+1-IB(2)) / YINC + 1.0
      J1 = INT (X)
      IF (X-J1.GT.0.0) J1 = J1 + 1
      J1 = MAX (1, J1)
      I2 = (NXIN-NX2-IB(1)) / NXSK + 1
      I2 = MIN(NXOUT,I2)
      J2 = (NYIN-NY2-IB(2)) / NYSK + 1
      J2 = MIN (NYOUT, J2)
      IF ((I1.GT.I2) .OR. (J1.GT.J2)) THEN
         MSGTXT = 'IMAGE TOO SMALL - NO PART FULLY CONVOLVED'
         GO TO 900
         END IF
      J0 = J1 - 1
      J3 = J2 + 1
      I1IN = (I1-1)*NXSK + IB(1)
      J1IN = (J1-1)*NYSK + IB(2)
C                                       Set window on input map
      NSUB = 6
      JNAM = 1
      BIN(1) = I1IN - NX2
      TIN(1) = (I2-1)*NXSK + IB(1) + NX2
      BIN(2) = J1IN - NY2
      TIN(2) = (J2-1)*NYSK + IB(2) + NY2
      NXIN = IROUND (TIN(1)-BIN(1)) + 1
      NYIN = IROUND (TIN(2)-BIN(2)) + 1
      CALL MAPWIN (LUN18, BIN, TIN, ERROR)
      IF (ERROR.NE.0) GO TO 905
C                                       Start convolution loop on
C                                       inactive axes
      NSUB = 5
      DO 250 K7 = IB(7),IT(7)
      DO 250 K6 = IB(6),IT(6)
      DO 250 K5 = IB(5),IT(5)
      DO 250 K4 = IB(4),IT(4)
      DO 250 K3 = IB(3),IT(3)
C                                       2-dim plane loop first
C                                       fill in indefs at the bottom
         JNAM = 1
         DO 110 I=1,NXOUT
            OUT(I) = FBLANK
 110        CONTINUE
         IF (J0.GT.0) THEN
            DO 120 J = 1,J0
               CALL MAPIO ('WRIT', LUN19, OUT, ERROR)
               IF (ERROR.NE.0) GO TO 900
 120           CONTINUE
            END IF
C                                       Read lines from input map in
C                                       preparation of first line of
C                                       plane
         DO 140 J = NYSK+1,NY
            CALL MAPIO ('READ', LUN18, IN(1,J), ERROR)
            IF (ERROR.NE.0) GO TO 900
            IF (SEPAR) CALL SM1 (IN(1,J), XX, NX, NXIN, FBLANK)
 140        CONTINUE
C                                       2-dim plane convolution loop
         DO 210 J = J1,J2
C                                       Shift down and read additional
C                                       lines
            DO 170 K = 1,NYSK
               DO 160 JJ = 2,NY
                  DO 150 I = 1,NXIN
                     IN(I,JJ-1) = IN(I,JJ)
 150                 CONTINUE
 160              CONTINUE
               CALL MAPIO ('READ', LUN18, IN(1,NY), ERROR)
               IF (ERROR.NE.0) GO TO 905
               IF (SEPAR) CALL SM1 (IN(1,NY), XX, NX, NXIN, FBLANK)
 170           CONTINUE
C                                       Line loop
            JNAM = 2
            IF (.NOT.SEPAR) THEN
               II = -NXSK
               DO 200 I = I1,I2
                  II = II + NXSK
                  X = 0.0
                  WS = 0.0
C                                       Convolution
                  DO 190 M = 1,NY
                     DO 180 L = 1,NX
                        IF (IN(L+II,M).NE.FBLANK) THEN
                           X = X + XK(L,M)*IN(L+II,M)
                           WS = WS + XK(L,M)
                           END IF
 180                    CONTINUE
 190                 CONTINUE
                  IF (WS.GT.0.0) THEN
                     OUT(I) = X * AREA / WS
                  ELSE
                     OUT(I) = FBLANK
                     END IF
 200              CONTINUE
C                                       Simple case convolution
            ELSE
               II = NX2 + 1 - NXSK
               DO 205 I = I1,I2
                  II = II + NXSK
                  X = 0.0
                  WS = 0.0
                  DO 202 M = 1,NY
                     IF (IN(II,M).NE.FBLANK) THEN
                        X = X + YY(M)*IN(II,M)
                        WS = WS + YY(M)
                        END IF
 202                 CONTINUE
                  IF (WS.GT.0.0) THEN
                     OUT(I) = X * AREA / WS
                  ELSE
                     OUT(I) = FBLANK
                     END IF
 205              CONTINUE
               END IF
            CALL MAPIO ('WRIT', LUN19, OUT, ERROR)
            IF (ERROR.NE.0) GO TO 905
 210        CONTINUE
C                                       fill indefs in remaining lines
         IF (J3.LE.NYOUT) THEN
            DO 220 I = 1,NXOUT
               OUT(I) = FBLANK
 220           CONTINUE
            DO 230 J = J3,NYOUT
               CALL MAPIO ('WRIT', LUN19, OUT, ERROR)
               IF (ERROR.NE.0) GO TO 905
 230           CONTINUE
            END IF
 250     CONTINUE
C                                       Quit
      NSUB = 2
      CALL FILCLS (LUN19)
      CALL OPENCF (LUN19, NAME(2), ERROR)
      IF (ERROR.NE.0) GO TO 905
      NSUB = 7
      CALL MAPMAX (LUN19, XMAX, XMIN, ERROR)
      IF (ERROR.NE.0) GO TO 905
      CALL GETHDR (LUN19, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 905
      CALL FILNUM (LUN19, OUTPTR, ERROR)
      OUTVOL = FILTAB(POVOL,OUTPTR)
      OUTCNO = FILTAB(POCAT,OUTPTR)
      CALL FILCLS (LUN19)
C                                       copy some keywords
      CALL KEYPCP (INVOL, INCNO, OUTVOL, OUTCNO, 0, ' ', ERROR)
C                                       History: create and copy
 300  CALL HIINIT (3)
      CALL HISCOP (HLUN1, HLUN2, INVOL, OUTVOL, INCNO, OUTCNO,
     *   CATBLK, WBUFF, IBUF, ERROR)
      IF (ERROR.GE.3) GO TO 320
C                                       add to history
      CALL WAWA2A (NAME(1), MNAME, MCLASS, MSEQ, PTYPE, MDISK, MUSID)
      CALL HENCO1 (PRGNAM, MNAME, MCLASS, MSEQ, MDISK, HLUN2, IBUF,
     *   ERROR)
      IF (ERROR.NE.0) GO TO 320
      CALL WAWA2A (NAME(1), MNAME, MCLASS, MSEQ, PTYPE, MDISK, MUSID)
      CALL HENCOO (PRGNAM, MNAME, MCLASS, MSEQ, MDISK, HLUN2, IBUF,
     *   ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1300) TSKNAM, IB
      CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1301) TSKNAM, IT
      CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1302) TSKNAM, NXSK, NYSK
      CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      BPA = BPA * RADEG
      WRITE (HILINE,1303) TSKNAM, BMAJ, BMIN, BPA
      CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      APA = APA * RADEG
      WRITE (HILINE,1304) TSKNAM, AMAJ, AMIN, APA
      CALL HIADD (HLUN2, HILINE, IBUF, ERROR)
C                                       end history
 320  CALL HICLOS (HLUN2, T, IBUF, I)
      IF (ERROR.NE.0) THEN
         WRITE (MSGTXT,1320) ERROR
         GO TO 900
         END IF
C                                        Copy tables
      CALL ALLTAB (1, '  ', HLUN1, HLUN2, INVOL, OUTVOL, INCNO,
     *   OUTCNO, CATBLK, WBUFF, IBUF, ERROR)
      IF (ERROR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      GO TO 990
C                                       Error exit
 900  CALL MSGWRT (8)
 905  WRITE (MSGTXT,1905) SUBS(NSUB), ERROR
      CALL MSGWRT (8)
      CALL PRTNAM (NAME(JNAM), 8)
      NRET = 16
C                                       Exit
 990  CALL TSKEND (NRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1030 FORMAT ('INCREMENTS',2I4,' GREATER THAN KERNEL',2I3)
 1300 FORMAT (A6,' BLC=',6(I4,','),I4,'  / Bottom left corner')
 1301 FORMAT (A6,' TRC=',6(I4,','),I4,'  / Top right corner')
 1302 FORMAT (A6,' XINC=',I4,'  YINC=',I4)
 1303 FORMAT (A6,' BMAJ=',F8.4,' BMIN=',F8.4,' BPA=',F6.1,
     *   ' / Input beam')
 1304 FORMAT (A6,' BMAJ=',F8.4,' BMIN=',F8.4,' BPA=',F6.1,
     *   ' / Output beam')
 1320 FORMAT ('WARNING: ERROR',I6,' WRITING HISTORY FILE')
 1905 FORMAT (A6,' ERROR NO',I6)
      END
      SUBROUTINE SM1 (X, S, NS, N, INDEF)
C-----------------------------------------------------------------------
C   SM1 does a one-dimensional convolution: convolving X with S.
C   Input:
C      X       R(*)   Array to be smoothed
C      S       R(*)   Smoothing kernel
C      NS      I      Size of kernel
C      N       I      Number of points in X
C      INDEF   R      Indefinite value
C   Output:
C      X       R(*)   Smoothed array
C-----------------------------------------------------------------------
      REAL      X(*), S(*), INDEF
      INTEGER   NS, N
C
      INCLUDE 'SMOTH.INC'
      REAL      Y(MAXIMG), Z, W
      INTEGER   I, I1, I2, II, J
C-----------------------------------------------------------------------
      DO 10 I = 1,N
         Y(I) = X(I)
 10      CONTINUE
      I1 = NS / 2 + 1
      I2 = N - NS / 2
      II = -1
      DO 50 I = I1,I2
         II = II + 1
         Z = 0.0
         W = 0.0
         DO 20 J = 1,NS
            IF (Y(II+J).NE.INDEF) THEN
               Z = Z + S(J)*Y(II+J)
               W = W + S(J)
               END IF
 20         CONTINUE
         IF (W.GT.0.0) THEN
            X(I) = Z / W
         ELSE
            X(I) = INDEF
            END IF
 50      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE KERNEL (CS, XK, NX, NY, DMAJ, DMIN, DPA, ERROR)
C-----------------------------------------------------------------------
C   KERNEL calculates the convolution kernel for SMOTH.
C   Input:
C      CS      I        Size of kernel
C      DMAJ    R        FWHM major axis of Gaussian
C      DMIN    R        FWHM minor axis of Gaussian
C      DPA     R        Position angle of Gaussian (radians)
C    Output:
C      XK      R(*,*)   Kernel
C      NX      I        Length of kernel in X
C      NY      I        Length of kernel in Y
C      ERROR   I        Error code:   0 => OK
C                              100 => Convolving function too big
C                                     for 25x25 kernel
C                             -100 => Convolving function too small
C   EPS contains the criterion for setting the size of the kernel, and
C   for deciding when the 25x25 kernel is too small.  The convolving
C   function is considered too small if the size of the kernel is
C   reduced to 1 in either direction.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, ERROR, CS
      REAL      XK(CS,CS), DMAJ, DMIN, DPA
C
      REAL      X, Y, Z, SP, CP, L1, L2, A, B, TC, BYS, TCY, EPS,
     *   TENEPS, XMI(25), XMA(25), YMI(25), YMA(25), SUM
      INTEGER   NXR, NYR, I, J, K, JJ, N2P, N2, NXMIN, NXMAX, NYMIN,
     *   NYMAX
      DATA EPS /0.01/
C-----------------------------------------------------------------------
C                                       Set constants
      NX = CS
      NY = CS
      N2P = (CS+1) / 2
      TENEPS = 10.0*EPS
      SP = SIN(DPA)
      CP = COS(DPA)
      L1 = 4.0*LOG(2.0)/(DMAJ*DMAJ)
      L2 = 4.0*LOG(2.0)/(DMIN*DMIN)
      TC = 2.0*SP*CP*(L2-L1)
      SP = SP*SP
      CP = CP*CP
      A = L1*SP+L2*CP
      B = L1*CP+L2*SP
C                                       Fill full kernel (it's
C                                       symmetric)
      DO 20 J = 1,N2P
         Y = J - N2P
         BYS = B * Y * Y
         TCY = TC * Y
         DO 10 I = 1,CS
            X = I - N2P
            Z = A*X*X + TCY*X + BYS
            XK(I,J) = 0.0
            IF (Z.LT.70.0) XK(I,J) = EXP (-Z)
            XK(CS+1-I,CS+1-J) = XK(I,J)
 10         CONTINUE
 20      CONTINUE
C                                       Determine size of kernel
      CALL KEREXT (XK, NX, NY, XMI, XMA, YMI, YMA)
      NXMAX = 0
      NXMIN = 0
      NXMAX = 0
      NYMIN = 0
      NYMAX = 0
      N2 = CS / 2
      DO 110 K = 1,N2
         IF (XMI(K).LT.TENEPS) NXMIN = K
         IF (XMA(K).LT.EPS) NXMAX = K
         IF (YMI(K).LT.TENEPS) NYMIN = K
         IF (YMA(K).LT.EPS) NYMAX = K
 110     CONTINUE
      NXR = MIN (NXMIN, NXMAX)
      NYR = MIN (NYMIN, NYMAX)
      ERROR = 0
      IF ((NXR.GE.N2).OR. (NYR.GE.N2)) ERROR = -100
      IF ((NXMIN.LE.0) .OR. (NYMIN.LE.0)) ERROR = 100
      IF (ERROR.NE.0) GO TO 990
      NX = NX - 2*NXR
      NY = NY - 2*NYR
C                                       Reduce size of kernel
      SUM = 0.0
      DO 130 J = 1,NY
         JJ = J + NYR
         DO 120 I = 1,NX
            XK(I,J) = XK(I+NXR,JJ)
            SUM = SUM + XK(I,J)
 120        CONTINUE
 130     CONTINUE
C                                       Scale kernel
      SUM = 1.0/SUM
      DO 150 J = 1,NY
         DO 140 I = 1,NX
            XK(I,J) = XK(I,J) * SUM
 140        CONTINUE
 150     CONTINUE
C                                       Return
 990  RETURN
      END
      SUBROUTINE KEREXT (XK, NX, NY, XMI, XMA, YMI, YMA)
C-----------------------------------------------------------------------
C   KEREXT finds the min and max along rows and columns in the
C   kernel for KERNEL.
C   Input:
C      XK(NX,NY) R Kernel
C      NX        I Size of kernel in X
C      NY        I Size of kernel in Y
C     Output:
C      XMI(*)   R  Minima along columns
C      XMA(*)   R  Maxima along columns
C      YMI(*)   R  Minima along rows
C      YMA(*)   R  Maxima along rows
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      XK(NX,NY), XMI(*), XMA(*), YMI(*), YMA(*)
C
      INTEGER   NX2, NY2, I, J
      REAL      X, Y, Z
C-----------------------------------------------------------------------
      NX2 = NX / 2
      NY2 = NY / 2
C                                       First, the columns
      DO 20 I = 1,NX2
         X = 1.0
         Y = 0.0
         DO 10 J = 1,NY
            Z = XK(I,J)
            IF (Z.LT.X) X = Z
            IF (Z.GT.Y) Y = Z
 10         CONTINUE
         XMI(I) = X
         XMA(I) = Y
 20      CONTINUE
C                                       Then, the rows
      DO 40 J = 1,NY2
         X = 1.0
         Y = 0.0
         DO 30 I = 1,NX
            Z = XK(I,J)
            IF (Z.LT.X) X = Z
            IF (Z.GT.Y) Y = Z
 30         CONTINUE
         YMI(J) = X
         YMA(J) = Y
 40      CONTINUE
C                                       Return
 999  RETURN
      END
      SUBROUTINE GPXTAS (BA1, BB1, BP1, BA2, BB2, BP2, DX, DY)
C-----------------------------------------------------------------------
C              VERSION 1.0  -  10 SEPTEMBER 1980
C     GPXTAS CONVERTS A GAUSSIAN WITH PARAMETERS IN PIXELS (CELLS)
C           - HALFWIDTHS BA1, BB1; POSITION ANGLE BP1 (RADIANS) -
C     TO ONE WITH PARAMETERS IN ARCSECONDS
C           - HALFWIDTHS BA2, BB2; POSITION ANGLE BP2 (RADIANS) -
C     DX AND DY ARE THE CELL SIZES IN RADIANS.
C     IT HAS BEEN CODED BY ARNOLD ROTS IN SEPTEMBER 1980.
C-----------------------------------------------------------------------
      REAL      DX,DY,T,L1,L2,M1,M2,SP,CP,C,U,V,W
      REAL      PI,BA1,BB1,BP1,BA2,BB2,BP2,ARSRAD
      INTEGER   IPA, I1000, I500
      DATA PI/3.141592654/, ARSRAD/206264.8062/
      DATA I1000/1000/
C-----------------------------------------------------------------------
      BP2=BP1
      I500=I1000/2
      IPA=NINT(BP1/PI*REAL(I1000))
      IF (MOD(IPA,I1000).EQ.0) GO TO 10
      IF (MOD(IPA,I500).EQ.0) GO TO 20
C
      T=DX/DY
      CP=COS(BP1)
      SP=SIN(BP1)
      L1=1.0/(BA1*BA1)
      L2=1.0/(BB1*BB1)
      C=T*SP*CP*(L2-L1)
      SP=SP*SP
      CP=CP*CP
      T=T*T
C
CCC                 THE REAL WORK
C
      U=(SP+T*CP)*L1
      V=(CP+T*SP)*L2
      W=SQRT((U-V)*(U-V)+4.0*SP*CP*(1.0-T)*(1.0-T)*L1*L2)
      M1=0.5*(U+V-W)
      M2=0.5*(U+V+W)
      BA2=SQRT(1.0/M1)*DX*ARSRAD
      BB2=SQRT(1.0/M2)*DX*ARSRAD
      BP2=BP1
      M1=SP*L1+CP*L2-M1
      IF (C.NE.0.0 .OR. M1.NE.0.0) BP2=ATAN2(C,M1)
      IF (BP2.LT.0.0) BP2=BP2+PI
      GO TO 30
C
   10 BA2=BA1*DY*ARSRAD
      BB2=BB1*DX*ARSRAD
      GO TO 30
C
   20 BA2=BA1*DX*ARSRAD
      BB2=BB1*DY*ARSRAD
C
   30 CONTINUE
      RETURN
      END
      SUBROUTINE GDECON (BA1, BB1, BP1, BA2, BB2, BP2, BA3, BB3, BP3)
C-----------------------------------------------------------------------
C              VERSION 1.0  -  10 SEPTEMBER 1980
C     GDECON DECONVOLVES ONE GAUSSIAN (E.G. OBSERVED; PARAMETERS
C     BA1,BB1,BP1) WITH ANOTHER (E.G. BEAM; PARAMETERS BA2,BB2,BP2).
C     THE RESULT IS A NEW GAUSSIAN (E.G. TRUE SOURCE; PARAMETERS
C     BA3,BB3,BP3).
C     IT HAS BEEN CODED BY ARNOLD ROTS IN SEPTEMBER 1980.
C-----------------------------------------------------------------------
      REAL   BA1,BB1,BP1,BA2,BB2,BP2,BA3,BB3,BP3,D0,D1,D2,A,B
C-----------------------------------------------------------------------
      D0 = BA1*BA1 - BB1*BB1
      D2 = BA2*BA2 - BB2*BB2
      D1 = MAX(0.0,D0*D0+D2*D2-2.0*D0*D2*COS(2.0*(BP1-BP2)))
      D1 = SQRT(D1)
      BP3 = 0.0
      A = D0*SIN(2.0*BP1) - D2*SIN(2.0*BP2)
      B = D0*COS(2.0*BP1) - D2*COS(2.0*BP2)
      IF (A.NE.0.0 .OR. B.NE.0.0) BP3=0.5*ATAN2(A,B)
      A=BA1*BA1+BB1*BB1-BA2*BA2-BB2*BB2
      BA3=MAX(0.0,0.5*(A+D1))
      BA3=SQRT(BA3)
      BB3=MAX(0.0,0.5*(A-D1))
      BB3=SQRT(BB3)
      IF (BP3.LT.0.0) BP3=BP3+3.141592654
      RETURN
      END
      SUBROUTINE GASTPX (BA1,BB1,BP1,BA2,BB2,BP2,DX,DY)
C
C***********************************************************************
C
C             G A S T P X
C
C             VERSION 1.0  -  10 SEPTEMBER 1980
C
C***********************************************************************
C
C     GASTPX DOES THE INVERSE OF GPXTAS :
C     THE PARAMETERS OF A GAUSSIAN : HALFWIDTHS BA1 AND BB1
C      IN ARCSECONDS,
C     POSITION ANGLE BP1 IN RADIANS,
C     ARE CONVERTED TO BA2, BB2, BP2, IN PIXELS (CELLS), PIXELS, AND
C      RADIANS.
C     DX AND DY ARE THE PIXEL DIMENSIONS IN RADIANS.
C
C     IT HAS BEEN CODED BY ARNOLD ROTS IN SEPTEMBER 1980.
C
CCC                 SPECIFICATION STATEMENTS
C
      REAL BA1,BB1,BP1,BA2,BB2,BP2,DX,DY,RASA
      DATA RASA /206264.8062/
C
C**********************************************************************
C             P R O G R A M   S T A R T
C**********************************************************************
C
      BA2=BA1/(RASA*DX)
      BB2=BB1/(RASA*DX)
      BP2=BP1
      CALL GPXTAS (BA2,BB2,BP2,BA2,BB2,BP2,DY,DX)
C                     NOTE : THE ARGUMENTS IN THIS PARAMETER LIST ARE
C                                    CORRECT!!
      BA2=BA2/(DY*RASA)
      BB2=BB2/(DY*RASA)
      RETURN
      END
