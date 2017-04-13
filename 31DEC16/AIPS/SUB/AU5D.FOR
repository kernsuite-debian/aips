      SUBROUTINE AU5D (BRANCH)
C-----------------------------------------------------------------------
C! verbs to load and run TV movie sequences
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-1999, 2003, 2008-2010, 2014-2015
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
C   AU5D performs movie functions with the TV
C   BRANCH = 1 : TVMOVIE  load images and run movie
C            2 : REMOVIE  resume running previously loaded movie
C            3 : TVCUBE   load images in order suited to display of the
C                         whole set, run movie anyway
C   Algorithm originally developed by Arnold Rots, installed and
C   modified by Eric W. Greisen.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER SNAME*12, SCLAS*6, STYPE*2, AXCHAR(13)*4, PRGNAM*6,
     *   LCHAR*1, CTEST*4, FORM2*16, STRING*12, CDUM*2
      INTEGER   POTERR, IERR, ICHAN, BUF(MABFSS), USID, IVOL, SEQNO,
     *   DLUN, DIND, LFMT, ICP, CNO, INC(3), NX, NY, NZ, I, J, IZP,
     *   LMAG, MAG, MP, MX, MY, JMAG, IX0, IY0, JJ, IWIN(4), ITVC(4),
     *   MMX, MMY, ITEMP, NP, IXP(5), IYP(5), IOFF(2), LABTYP, HNC,
     *   IBLC(2), ITRC(2), IBUFSZ, JX, JY, JR, LOFF(2), LWIN(4), SCROLX,
     *   SCROLY, SMAG, IDUM, IBUT, QUAD, LTYPE, LZP, NZ1, KBLC(7),
     *   KTRC(7), KINC(7), K3, K4, K5, K6, K7, KN(7)
      LOGICAL   T, F, EQUAL, ISFELO, TIMEIT
      REAL      EPS, LBLC(7), LTRC(7), X0, DX, RVA, RBUF(MABFSS),
     *   RDUM(2), FACT, DOALL
      DOUBLE PRECISION X, VV, VA, VB, ZREFP, ZZINC, VELITE, AXDENU
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ RBUF
      EQUIVALENCE (BUF, RBUF)
      DATA PRGNAM /'AU5D '/
      DATA T, F /.TRUE.,.FALSE./
      DATA EPS /0.01/
      DATA DLUN /16/
      DATA VELITE /2.997924562D8/
      DATA AXCHAR /'FELO','FREQ','VELO','LL  ','MM  ','RA  ','DEC ',
     *   'RA--','DEC-','GLON','GLAT','ELON','ELAT'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.3)) GO TO 999
      IBUFSZ = 2 * MABFSS
      CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 101
      CALL TVOPEN (BUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) ICHAN = 1
      TIMEIT = IDEBUG.GT.0
      GO TO (100, 200, 100), BRANCH
C-----------------------------------------------------------------------
C                                       TVMOVIE
C-----------------------------------------------------------------------
C                                       Open the map
 100  CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('INSEQ', 'I', 1, 0, SEQNO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      USID = NLUSER
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, SNAME)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, SCLAS)
      IF (ERRNUM.NE.0) GO TO 975
      STYPE = 'MA'
      POTERR = 33
      CALL MAPOPN ('READ', IVOL, SNAME, SCLAS, SEQNO, STYPE, USID, DLUN,
     *    DIND, CNO, CATBLK, BUF, IERR)
      IF (IERR.GT.1) GO TO 975
C                                       Basic window
      CALL ADVERB ('TBLC', 'R', 7, 0, IDUM, LBLC, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVERB ('TTRC', 'R', 7, 0, IDUM, LTRC, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      POTERR = 101
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), LBLC, LTRC, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ADVERB ('TXINC', 'I', 1, 0, INC(1), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('TYINC', 'I', 1, 0, INC(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('TZINC', 'I', 1, 0, INC(3), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('DOALL', 'R', 1, 0, IDUM, DOALL, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      INC(1) = MAX (1, INC(1))
      INC(2) = MAX (1, INC(2))
      INC(3) = MAX (1, INC(3))
      NX = (LTRC(1) - LBLC(1)) / INC(1) + 1.01
      NY = (LTRC(2) - LBLC(2)) / INC(2) + 1.01
      J = CATBLK(KIDIM)
      NZ = 1
      IZP = J
      JJ = INC(3)
      DO 105 I = 3,J
         IDUM = (LTRC(I) - LBLC(I)) / JJ + 1.01
         IF (IDUM.GT.1) THEN
            IZP = MIN (IZP, I)
            LZP = I
            IF (NZ.EQ.0) NZ1 = IDUM
            NZ = NZ * IDUM
            JJ = 1
            IF (DOALL.LE.0.0) GO TO 115
            END IF
 105     CONTINUE
      IF (NZ.LE.1) THEN
         MSGTXT = 'MOVIE ONLY WORKS WITH TRUE 3-DIMENSIONAL SUBIMAGES'
         CALL MSGWRT (7)
         GO TO 970
         END IF
C                                       Decide zoom scale, pix size
 115  J = ABS(MXZOOM) + 1
      JJ = 1 - MXZOOM
      IF (MXZOOM.GT.0) JJ = 2 ** MXZOOM
      LMAG = 0
      DO 120 I = 1,J
         JMAG = I - 1
         MAG = JMAG + 1
         IF (MXZOOM.GT.0) MAG = 2 ** JMAG
         MP = (NGRAY - ICHAN + 1) * MAG * MAG
         MMX = MAXXTV(1) - 2
         MMY = MAXXTV(2) - 2
         IF (MAG.NE.1) THEN
            MMX = MAXXTV(1) / (MAG * (MAG-1))
            MMX = MMX * (MAG-1)
            IF (MAG*MMX.GE.MAXXTV(1)) THEN
               MMX = (MAXXTV(1)-JJ) / (MAG * (MAG-1))
               MMX = MMX * (MAG-1)
               END IF
            MMY = MAXXTV(2) / (MAG * (MAG-1))
            MMY = MMY * (MAG-1)
            IF (MAG*MMY.GE.MAXXTV(2)) THEN
               MMY = (MAXXTV(2)-JJ) / (MAG * (MAG-1))
               MMY = MMY * (MAG-1)
               END IF
            END IF
C                                       all dimensions fit?
         IF (NZ.LE.MP) THEN
            IF (LMAG.EQ.0) LMAG = MAG
            MX = MMX * 0.58
            MY = MMY * 0.58
            IF ((NX.GE.MX) .OR. (NY.GE.MY)) GO TO 125
            END IF
 120     CONTINUE
C                                       Restrict number of planes
      IF (LMAG.LE.0) THEN
         LZP = IZP
         I = LBLC(IZP) + (LTRC(IZP) - LBLC(IZP) + 1 - MP*INC(3)) / 2
         LBLC(IZP) = I
         LTRC(IZP) = LBLC(IZP) + MP * INC(3)
         NZ = MP
         END IF
C                                       Check planes too
 125  DO 130 I = 1,2
         MX = (2-I) * MMX + (I-1) * MMY - 2
         MY = MX
         J = (MX-1) * INC(I)
         ITRC(I) = LTRC(I) + EPS
         IBLC(I) = LBLC(I) + EPS
         IF (ITRC(I)-IBLC(I).GT.J) THEN
            IBLC(I) = IBLC(I) + (ITRC(I) - IBLC(I) + 1 - J)/2
            ITRC(I) = IBLC(I) + J
            END IF
         IOFF(I) = (MY - 1 - (ITRC(I) - IBLC(I))/INC(I)) / 2.0
 130     CONTINUE
      KN(1) = 1
      KN(2) = 1
      DO 135 I = 3,7
         KINC(I) = 1
         KN(I) = KN(I-1) * MAX (1, CATBLK(KINAX+I-1))
         KBLC(I) = LBLC(I) + EPS
         IF ((I.LT.IZP) .OR. (I.GT.LZP)) THEN
            KTRC(I) = KBLC(I)
         ELSE IF (I.EQ.IZP) THEN
            KINC(I) = INC(3)
            J = (LTRC(I) - LBLC(I)+EPS) / INC(3)
            KTRC(I) = KBLC(I) + J * INC(3)
         ELSE
            KTRC(I) = LTRC(I) + EPS
            END IF
 135     CONTINUE
C                                       Zero planes to be used
      MP = MAG * MAG
      J = (NZ - 1) / MP + ICHAN
      CALL YHOLD ('ONNN', IERR)
      DO 140 I = ICHAN,J
         CALL YZERO (I, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL YCINIT (I, BUF)
         JJ = 2 ** (I-1)
         SCROLX = 0
         SCROLY = 0
         CALL YSCROL (JJ, SCROLX, SCROLY, F, IERR)
         IF (IERR.NE.0) GO TO 970
 140     CONTINUE
C                                       enter codes in common
      NY = MP
      IF (BRANCH.EQ.3) NY = -MP
      CALL MOVIST ('ONNN', ICHAN, NZ, NY, JMAG, IERR)
C                                       set range and other parms
      CALL ADVERB ('PIXRANGE', 'R', 2, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL RNGSET (RDUM, CATR(KRDMX), CATR(KRDMN), CATR(IRRAN))
      CATBLK(IIVOL) = IVOL
      CATBLK(IICNO) = CNO
      CALL ADVERB ('FUNCTYPE', 'C', 1, 2, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL CHR2H (2, CDUM, 1, CATH(IITRA))
      POTERR = 49
      IWIN(1) = IBLC(1)
      IWIN(2) = IBLC(2)
      IWIN(3) = ITRC(1)
      IWIN(4) = ITRC(2)
      IWIN(3) = IWIN(3) - MOD (IWIN(3)-IWIN(1), INC(1))
      IWIN(4) = IWIN(4) - MOD (IWIN(4)-IWIN(2), INC(2))
      CALL COPY (4, IWIN, LWIN)
      LOFF(1) = IOFF(1)
      LOFF(2) = IOFF(2)
C                                       Label fit wo overlap?
      NP = 7 - MAG + MAG/4 + MAG/8
      IF (NP.LT.2) NP = 2
      J = (ITRC(2)-IBLC(2))/INC(2) + 1
      IF (4*IOFF(2).GE.NP+CSIZTV(2)) THEN
         J = MMY - 2 - J
         MX = NP
         MY = CSIZTV(2)
         IF (J.LT.NP+CSIZTV(2)) MX = 0
         IF (J.LT.CSIZTV(2)) MY = J
         IF (J.GT.MX+CSIZTV(2)) J = MX + CSIZTV(2)
         IOFF(2) = IOFF(2) - (MY-MX)/2
         IF (IOFF(2).LT.0) IOFF(2) = 0
         LOFF(2) = IOFF(2) - MX
         LWIN(4) = LWIN(4) + J*INC(2)
         END IF
      DO 145 I = 1,5
         CATBLK(IIDEP+I-1) = LBLC(I+2) + EPS
 145     CONTINUE
      CATBLK(IIDEP+IZP-3) = CATBLK(IIDEP+IZP-3) - INC(3)
C                                       Off zoom
      TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), T, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Off graphics & gray
      NY = NGRAY + NGRAPH
      DO 150 I = 1,NY
         CALL YSLECT ('OFFF', I, 0, BUF, IERR)
         IF (IERR.NE.0) GO TO 970
 150     CONTINUE
C                                       Load planes finally
      NY = ICHAN
      JR = 0
      IF (MOD(ICHAN,2).EQ.0) JR = MAG
      CALL YSLECT ('ONNN', NY, 0, BUF, IERR)
      CALL YHOLD ('OFFF', IERR)
      MSGTXT = 'Hit button D to terminate load prematurely'
      CALL MSGWRT (1)
      RDUM(1) = MAXXTV(1) / 2
      RDUM(2) = MAXXTV(2) / 2
      CALL YCURSE ('ONNN', F, F, RDUM, QUAD, IBUT, IERR)
      I = 0
C     DO 160 I = 1,NZ
      DO 160 K7 = KBLC(7),KTRC(7),KINC(7)
         CATBLK(IIDEP+4) = K7
      DO 160 K6 = KBLC(6),KTRC(6),KINC(6)
         CATBLK(IIDEP+3) = K6
      DO 160 K5 = KBLC(5),KTRC(5),KINC(5)
         CATBLK(IIDEP+2) = K5
      DO 160 K4 = KBLC(4),KTRC(4),KINC(4)
         CATBLK(IIDEP+1) = K4
      DO 160 K3 = KBLC(3),KTRC(3),KINC(3)
         CATBLK(IIDEP) = K3
         I = I + 1
         NX = (I-1) / MP + ICHAN
         JX = MOD (I-1, MAG) + 1
         JY = (I-1)/MAG + 1 - ((I-1)/MP)*MAG
         IF (BRANCH.NE.3) THEN
            IF (JX.EQ.1) JR = JR + 1
            IF (MOD(JR,2).EQ.0) JX = MAG + 1 - JX
            JY = JR - ((JR-1)/MAG)*MAG
            IF (MOD(NX,2).EQ.0) JY = MAG + 1 - JY
            END IF
C                                       switch planes: select, set LUT
         IF (NX.NE.NY) THEN
            CALL YHOLD ('ONNN', IERR)
            CALL YSLECT ('OFFF', NY, 0, BUF, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL YSLECT ('ONNN', NX, 0, BUF, IERR)
            IF (IERR.NE.0) GO TO 970
            DO 154 MX = 1,3
               MY = 2 ** (MX-1)
               ITEMP = 2 ** (NY - 1)
               CALL YLUT ('READ', ITEMP, MY, F, BUF, IERR)
               IF (IERR.NE.0) GO TO 970
               ITEMP = 2 ** (NX - 1)
               CALL YLUT ('WRIT', ITEMP, MY, F, BUF, IERR)
               IF (IERR.NE.0) GO TO 970
 154           CONTINUE
            CALL YHOLD ('OFFF', IERR)
            NY = NX
            END IF
 155     ITVC(1) = IOFF(1) + 3 + MMX * (JX-1)
         ITVC(2) = IOFF(2) + MAXXTV(2) + 1 - MMY * JY
         ITVC(3) = ITVC(1) + (IWIN(3) - IWIN(1)) / INC(1)
         ITVC(4) = ITVC(2) + (IWIN(4) - IWIN(2)) / INC(2)
         CALL TVLOAD (DLUN, DIND, NX, INC, ITVC, IWIN, IBUFSZ, RBUF,
     *      IERR)
         IF (IERR.NE.0) GO TO 970
         CALL YCURSE ('READ', F, F, RDUM, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 970
         IF (IBUT.GE.8) THEN
            MSGTXT = 'Button D hit - quitting movie now'
            CALL MSGWRT (1)
            POTERR = 0
            GO TO 970
            END IF
 160     CONTINUE
C                                       Close map & 'MAP' type TV
      CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F, BUF, IERR)
C                                       Annotation section
C                                       Init labels
      CALL YHOLD ('ONNN', IERR)
      CALL ADVERB ('LTYPE', 'I', 1, 0, LTYPE, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      LABTYP = MOD (ABS (LTYPE), 100)
      IF (LABTYP.GT.6) LABTYP = LABTYP - 4
      IF (LABTYP.EQ.0) LABTYP = 3
      IF (LABTYP.LT.3) LABTYP = 1
      IF (LZP.GT.IZP) LABTYP = 2
      IF (LABTYP.GE.3) THEN
         HNC = (IZP - 1) * 2 + KHCTP
         CALL H2CHR (4, 1, CATH(HNC), CTEST)
         LCHAR = CTEST(1:1)
         X = 1.0D0
         VA = KBLC(IZP)
         VB = KTRC(IZP)
         ISFELO = CTEST.EQ.AXCHAR(1)
         IF (ISFELO) LCHAR = AXCHAR(3)(1:1)
         ZZINC = CATR(KRCIC+IZP-1)
         ZREFP = CATR(KRCRP+IZP-1)
         IF (ZZINC.EQ.0.0) LABTYP = 6
         IF (LABTYP.NE.6) THEN
            ICP = 3
            IF (ABS((VB-VA)*ZZINC).LT.2.0) ICP = 13
            DO 162 I = 1,ICP
               EQUAL = CTEST.EQ.AXCHAR(I)
               IF ((EQUAL) .AND. (I.GT.3)) X = 3600.D0
               IF ((EQUAL) .AND. (I.LE.3)) X = 0.001D0
 162           CONTINUE
C                                       Non-linear velocity
            ISFELO = (ISFELO) .AND. (CATD(KDRST).GT.0.0) .AND.
     *         (CATD(KDARV).GT.0.0)
            IF (ISFELO) THEN
               AXDENU = -ZZINC / (VELITE + CATD(KDCRV+IZP-1))
               IF (LABTYP.EQ.5) LABTYP = 4
               VA = X * ZZINC * (VA-ZREFP) / (1.D0 + AXDENU*(VA-ZREFP))
               VB = X * ZZINC * (VB-ZREFP) / (1.D0 + AXDENU*(VB-ZREFP))
               IF (LABTYP.EQ.3) THEN
                  VA = VA + X * CATD(KDCRV+IZP-1)
                  VB = VB + X * CATD(KDCRV+IZP-1)
                  END IF
            ELSE
               IF (LABTYP.EQ.5) ZREFP = (LBLC(IZP) + LTRC(IZP)) / 2.0
               VA = (VA - ZREFP) * ZZINC * X
               IF (LABTYP.EQ.3) VA = VA + CATD(KDCRV+IZP-1) * X
               VB = (VB - ZREFP) * ZZINC * X
               IF (LABTYP.EQ.3) VB = VB + CATD(KDCRV+IZP-1) * X
               END IF
            END IF
C                                       Test range
         VV = MAX (ABS(VA), ABS(VB))
         IF ((LABTYP.EQ.3) .AND. (ABS(VA-VB).LE.0.02*VV)) THEN
            LABTYP = 4
            VA = VA - X * CATD(KDCRV+IZP-1)
            VB = VB - X * CATD(KDCRV+IZP-1)
            VV = MAX (ABS(VA), ABS(VB))
            END IF
         LFMT = 5
         IF ((VA.LT.0.) .OR. (VB.LT.0.)) LFMT = 6
         I = 0
         VV = MAX (VV, ABS(VA-VB))
         RVA = VV
         IF ((VV.GT.9999.) .OR. (VV.LT.1.0)) CALL METSCA (RVA,
     *      SCLAS, EQUAL)
         IF (VV.NE.0.0) X = X * RVA / VV
         IF (RVA.LT.999.) LFMT = LFMT - 1
         IF ((RVA.LT.99.9) .AND. (LABTYP.LT.6)) I = 1
         IF ((RVA.LT.9.99) .AND. (LABTYP.LT.6)) I = 2
         IF ((RVA.LT.99.9) .AND. (LABTYP.EQ.6)) LFMT = LFMT - 1
         IF ((RVA.LT.9.99) .AND. (LABTYP.EQ.6)) LFMT = LFMT - 1
         WRITE (FORM2,1160) LFMT, I
         IF (I.EQ.0) LFMT = LFMT - 1
         LFMT = LFMT + 2
      ELSE IF (LABTYP.EQ.2) THEN
         VV = KN(7)
         I = LOG10 (VV) + 1.00001
         LFMT = I
         WRITE (FORM2,1161) I, I
         END IF
C                                       Init pointers
      NX = ITVC(3) - ITVC(1) + 1 - 2*NP
      X0 = NP + 1
      DX = REAL (NX) / REAL (NZ)
C                                       Off channel NY
      CALL YSLECT ('OFFF', NY, 0, BUF, IERR)
      IF (IERR.NE.0) GO TO 975
      NY = 0
      JR = 0
      IF (MOD(ICHAN,2).EQ.0) JR = MAG
      I = 0
      DO 190 K7 = KBLC(7),KTRC(7),KINC(7)
         CATBLK(IIDEP+4) = K7
      DO 190 K6 = KBLC(6),KTRC(6),KINC(6)
         CATBLK(IIDEP+3) = K6
      DO 190 K5 = KBLC(5),KTRC(5),KINC(5)
         CATBLK(IIDEP+2) = K5
      DO 190 K4 = KBLC(4),KTRC(4),KINC(4)
         CATBLK(IIDEP+1) = K4
      DO 190 K3 = KBLC(3),KTRC(3),KINC(3)
         CATBLK(IIDEP) = K3
         I = I + 1
C      DO 190 I = 1,NZ
         NX = (I-1) / MP + ICHAN
         JX = MOD (I-1, MAG) + 1
         JY = (I-1)/MAG + 1 - ((I-1)/MP)*MAG
         IF (BRANCH.NE.3) THEN
            IF (JX.EQ.1) JR = JR + 1
            IF (MOD(JR,2).EQ.0) JX = MAG + 1 - JX
            JY = JR - ((JR-1)/MAG)*MAG
            IF (MOD(NX,2).EQ.0) JY = MAG + 1 - JY
            END IF
C                                       switch planes
         IF (NX.NE.NY) THEN
            IF (NY.GT.0) CALL YSLECT ('OFFF', NY, 0, BUF, IERR)
            IF (IERR.NE.0) GO TO 975
            CALL YSLECT ('ONNN', NX, 0, BUF, IERR)
            IF (IERR.NE.0) GO TO 975
            NY = NX
            END IF
C                                       write border
         IX0 = LOFF(1) + 3 + MMX * (JX-1)
         IY0 = LOFF(2) + MAXXTV(2) + 1 - MMY * JY
         IXP(1) = IX0 - 1
         IXP(2) = IX0 + (LWIN(3)-LWIN(1))/INC(1) + 1
         IXP(3) = IXP(2)
         IXP(4) = IXP(1)
         IXP(5) = IXP(1)
         IYP(1) = IY0 - 1
         IYP(2) = IYP(1)
         IYP(3) = IY0 + (LWIN(4)-LWIN(2))/INC(2) + 1
         IYP(4) = IYP(3)
         IYP(5) = IYP(1)
         CALL IMVECT ('ONNN', NX, 5, IXP, IYP, BUF, IERR)
         IF (IERR.NE.0) GO TO 975
         IXP(1) = MAX (1, IXP(1)-1)
         IXP(2) = MIN (MAXXTV(1), IXP(2)+1)
         IXP(3) = IXP(2)
         IXP(4) = IXP(1)
         IXP(5) = IXP(1)
         IYP(1) = MAX (1, IYP(1)-1)
         IYP(2) = IYP(1)
         IYP(3) = MIN (MAXXTV(2), IYP(3)+1)
         IYP(4) = IYP(3)
         IYP(5) = IYP(1)
         CALL IMVECT ('ONNN', NX, 5, IXP, IYP, BUF, IERR)
         IF (IERR.NE.0) GO TO 975
C                                       Label
         IF (LABTYP.GE.3) THEN
            VV = CATBLK(IIDEP+IZP-3)
            IF (LABTYP.LE.5) THEN
               IF (ISFELO) THEN
                  VV = X * ZZINC * (VV-ZREFP) /
     *               (1.0D0 + AXDENU*(VV-ZREFP))
               ELSE
                  VV = (VV - ZREFP) * ZZINC * X
                  END IF
            ELSE IF (LABTYP.EQ.3) THEN
               VV = VV + CATD(KDCRV+IZP-1) * X
               END IF
            WRITE (STRING,FORM2) LCHAR, VV
            IYP(1) = IY0 + (LWIN(4)-LWIN(2))/INC(2) + 1 - CSIZTV(2)
            CALL IMCHAR (NY, IX0, IYP, 0, 0, STRING(1:LFMT), BUF, IERR)
            IF (IERR.NE.0) GO TO 975
         ELSE IF (LABTYP.EQ.2) THEN
            JJ = K3 + (K4-1)*KN(3) + (K5-1)*KN(4) + (K6-1)*KN(5) +
     *         (K7-1)*KN(6)
            WRITE (STRING,FORM2) JJ
            IYP(1) = IY0 + (LWIN(4)-LWIN(2))/INC(2) + 1 - CSIZTV(2)
            CALL IMCHAR (NY, IX0, IYP, 0, 0, STRING(1:LFMT), BUF, IERR)
            IF (IERR.NE.0) GO TO 975
            END IF
C                                       Pointer
         CALL ADVERB ('DOPOS', 'R', 8, 0, IDUM, RBUF, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         IF (RBUF(1).GT.0.0) THEN
            IXP(1) = IX0 + X0 + (I-1)*DX + 1.5
            IYP(1) = IY0 - 1
            DO 185 JJ = 1,NP
               IXP(1) = IXP(1) - 1
               IYP(1) = IYP(1) + 1
               IYP(2) = IYP(1)
               IXP(2) = IXP(1) + 2 * (JJ-1)
               CALL IMVECT ('ONNN', NX, 2, IXP, IYP, BUF, IERR)
               IF (IERR.NE.0) GO TO 975
 185           CONTINUE
            END IF
         CALL YCURSE ('READ', F, F, RDUM, QUAD, IBUT, IERR)
         IF (IERR.NE.0) GO TO 975
         IF (IBUT.GE.8) THEN
            MSGTXT = 'Button D hit - quitting movie now'
            CALL MSGWRT (1)
            POTERR = 0
            GO TO 975
            END IF
 190     CONTINUE
      IX0 = 1
      IY0 = NZ
      JX = 0
      IF (BRANCH.EQ.3) JX = 1
      CALL YHOLD ('OFFF', IERR)
      GO TO 230
C-----------------------------------------------------------------------
C                                       REMOVIE
C                                       rerun previously loaded movie
C-----------------------------------------------------------------------
C                                       Get old movie parms
 200  NZ = TYPMOV(ICHAN) / 64
      JMAG = TYPMOV(ICHAN) / 4 - 16 * NZ
      JX = MOD(TYPMOV(ICHAN), 2)
      IF (NZ.LE.0) THEN
         WRITE (MSGTXT,1200) ICHAN
         CALL MSGWRT (7)
         GO TO 975
         END IF
C                                       Get frame range desired
      CALL ADVERB ('BCHAN', 'I', 1, 0, IX0, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF (IX0.LE.0) IX0 = 1
      CALL ADVERB ('ECHAN', 'I', 1, 0, IY0, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IY0.LE.0) .OR. (IY0.GT.NZ)) IY0 = NZ
      IF (IX0.GE.IY0) THEN
         IX0 = 1
         IY0 = NZ
         END IF
C                                       Turn everything off
      JJ = NGRAY + NGRAPH
      CALL YHOLD ('ONNN', IERR)
      DO 215 I = 1,JJ
         CALL YSLECT ('OFFF', I, 0, BUF, IERR)
         IF (IERR.NE.0) GO TO 975
 215     CONTINUE
C                                       Shift LUTs
      MAG = JMAG + 1
      IF (MXZOOM.GT.0) MAG = 2 ** JMAG
      MAG = MAG * MAG
      NX = (IX0-1) / MAG + ICHAN
      NY = (IY0-1) / MAG + ICHAN
      IF (NX.LT.NY) THEN
         JJ = NX + 1
         DO 225 J = 1,3
            MY = 2 ** (J-1)
            ITEMP = 2 ** (NX - 1)
            CALL YLUT ('READ', ITEMP, MY, F, BUF, IERR)
            IF (IERR.NE.0) GO TO 975
            DO 220 I = JJ,NY
               ITEMP = 2 ** (I-1)
               CALL YLUT ('WRIT', ITEMP, MY, F, BUF, IERR)
               IF (IERR.NE.0) GO TO 975
 220           CONTINUE
 225        CONTINUE
         END IF
C                                       Run the movie
 230  CALL ADVERB ('DOCIRCLE', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      EQUAL = RDUM(1).LE.0.0
      CALL ADVERB ('FACTOR', 'R', 1, 0, IDUM, FACT, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('DOCENTER', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((RDUM(1).GT.0.0) .AND. (FACT.GT.0.9)) THEN
         IF (MXZOOM.LT.0) THEN
            SMAG = FACT - 0.9
         ELSE
            SMAG = -1
            I = FACT + 0.1
 231        SMAG = SMAG + 1
               I = I / 2
               IF (I.GT.0) GO TO 231
            END IF
      ELSE
         SMAG = JMAG
         END IF
      CALL YHOLD ('OFFF', IERR)
      CALL TVMOVI (ICHAN, JX, JMAG, SMAG, IX0, IY0, EQUAL, TIMEIT, BUF,
     *   IERR)
      POTERR = 49
      IF (IERR.EQ.0) POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       Close down
 970  CALL MAPCLS ('READ', IVOL, CNO, DLUN, DIND, CATBLK, F, BUF, IERR)
 975  CALL TVCLOS (BUF, IERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1160 FORMAT ('(A1,''='',F',I1,'.',I1,')')
 1161 FORMAT ('(I',I1,'.',I1,')')
 1200 FORMAT ('CHANNEL',I3,' DOES NOT CONTAIN A PRELOADED MOVIE',
     *   ' SEQUENCE')
      END
