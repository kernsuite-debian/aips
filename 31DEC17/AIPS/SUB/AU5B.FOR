      SUBROUTINE AU5B (BRANCH)
C-----------------------------------------------------------------------
C! verbs to anotate TV images
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2002-2006, 2008, 2010-2015
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
C   BRANCH = 1 does Verb TVLABEL (for MAps).
C            2           TVWLABEL (for WEdges).
C            3           TVANOT  (extra string on TV)
C            4           TVLINE  (draw line on TV graphics or grey ch)
C            5           TVSTAR  (plot stars atop images)
C            6           COSTAR  (plot symbol at coordinate atop image)
C            7           TVILINE (plot line between image pixels)
C            8           IM2TV   (return TVXY for PIXXY)
C            9           TVLAYOUT (overlay antenna layout pattern)
C   Draw axis and label on image on the the TV display device.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER IMTYPE*2, CTEST*2, PRGNAM*6, STRING*64, CDUM*1,
     *   INFILE*48
      REAL      RPOS(2), RDUM, BLC(2), TRC(2), XCOORD(6), AX(5), AY(5),
     *   STFACT, PIXXY(7)
      INTEGER   IERR, ILAB, INBUF(2560), IPL, IP, JERR, POTERR, IQ, NCH,
     *   IBUT, NXC, NYC, IX0, IY0, I, ITRIM, J, IX1, IY1, XP(2), YP(2),
     *   IDUM, IROUND, MVER, IVER, IG, MSGSAV, DOZERO
      LOGICAL   UNIQUE, DOGRID, DOBKND, DOVERT
      DOUBLE PRECISION XPOS, YPOS
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DGPH.INC'
      COMMON /AIPSCR/ INBUF
      EQUIVALENCE (XP(1), IX0),    (XP(2), IX1)
      EQUIVALENCE (YP(1), IY0),    (YP(2), IY1)
      DATA PRGNAM /'AU5B '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.9)) GO TO 999
      MSGSAV = MSGSUP
      POTERR = 101
      CALL TVOPEN (INBUF, JERR)
      IF (JERR.NE.0) GO TO 980
      GO  TO (100, 100, 300, 400, 500, 600, 700, 800, 900), BRANCH
C-----------------------------------------------------------------------
C                                       TVLABEL
C                                       label image on TV
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 100  IMTYPE = 'MA'
      LOCNUM = 1
      IF (BRANCH.EQ.2) IMTYPE = 'WE'
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IBUT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IBUT.LE.0) .OR. (IBUT.GT.7)) IBUT = 2
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
      POTERR = 49
C                                       write axis & labeling to TV.
 120  CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
      IF (CTEST.EQ.'ZZ') GO TO 960
C                                       Normal image labels
      CALL ADVERB ('LTYPE', 'I', 1, 0, ILAB, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      ILAB = ABS (ILAB)
      IF (CTEST.NE.'WE') THEN
         I = MOD (ILAB, 100)
         IF ((I.LE.0) .OR. (I.GT.10)) ILAB = (ILAB/100)*100 + 3
         CALL ADVERB ('DOCIRCLE', 'R', 1, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         DOGRID = RDUM.GT.0.0
C                                       Wedge type image
      ELSE
         DOGRID = .FALSE.
         ILAB = (ILAB/100)*100 + 7
         END IF
      LABTYP(LOCNUM) = 0
      DOZERO = -1
      IF (UNIQUE) DOZERO = 1
      CALL IAXIS1 (INBUF, ILAB, IBUT, DOZERO, DOGRID, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVANOT
C                                       string label on TV
C-----------------------------------------------------------------------
C                                       Do we have a string?
 300  CALL ADVERB ('COMMENT', 'C', 1, 64, IDUM, RDUM, STRING)
      IF (ERRNUM.NE.0) GO TO 975
      NCH = ITRIM (STRING)
      IF (NCH.LE.0) THEN
         POTERR = 13
         MSGTXT = 'STRING EMPTY: SET COMMENT'
         GO TO 970
         END IF
      CALL ADVERB ('DOINVERS', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      DOVERT = RDUM.GT.0.0
      IF (DOVERT) THEN
         NXC = CSIZTV(1)
         NYC = NCH * CSIZTV(2)
      ELSE
         NXC = NCH * CSIZTV(1)
         NYC = CSIZTV(2)
         END IF
      CALL ADVERB ('TVCORN', 'R', 2, 0, IDUM, RPOS, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IX0 = RPOS(1) + 0.5
      IY0 = RPOS(2) + 0.5
      IBUT = 0
      POTERR = 49
C                                       Read cursor
 305  IF ((IX0.LE.0) .OR. (IX0+NXC.GT.MAXXTV(1)) .OR. (IY0.LE.0) .OR.
     *   (IY0+NYC.GT.MAXXTV(2))) THEN
         IF (IBUT.GE.8) GO TO 975
         MSGTXT = 'Set cursor to string BLC, then push any button'
         CALL MSGWRT (1)
         CALL TVWHER (IQ, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 975
         IF ((IX0.LE.0) .OR. (IX0+NXC.GT.MAXXTV(1))) IX0 = RPOS(1) + 0.5
         IF ((IY0.LE.0) .OR. (IY0+NYC.GT.MAXXTV(2))) IY0 = RPOS(2) + 0.5
         GO TO 305
         END IF
C                                       Where to put and do it
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IBUT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('GR2CHAN', 'I', 1, 0, IPL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('TVCHAN', 'I', 1, 0, IP, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IBUT.LE.0) .AND. ((IP.LE.0) .OR. (IP.GT.NGRAY))) THEN
         IBUT = 2
         IPL = 8
         END IF
      IQ = 0
      IF (DOVERT) THEN
         IQ = 3
         IY0 = IY0 + (NCH-1)*CSIZTV(2)
         END IF
      CALL YHOLD ('ONNN', IERR)
      IF (IBUT.GT.0) THEN
         IP = IBUT
         DOBKND = (IPL.EQ.8) .OR. (IP.EQ.8)
         IF ((IPL.GT.0) .AND. (IP.GT.0)) THEN
            IPL = MIN (IP, IPL)
         ELSE IF (DOBKND) THEN
            IPL = 2
         ELSE
            IPL = MAX (IP, IPL)
            END IF
C                                       Graphics channels IPL & NGRAPH
         IF (DOBKND) THEN
            CALL IMANOT ('ONNN', IPL, IX0, IY0, IQ, 0, STRING(1:NCH),
     *         INBUF, IERR)
            IF (IERR.NE.0) GO TO 975
            CALL IMANOT ('WRIT', IPL, IX0, IY0, IQ, 0, STRING(1:NCH),
     *         INBUF, IERR)
            IF (IERR.EQ.0) POTERR = 0
         ELSE
            IPL = IPL + NGRAY
            CALL YSLECT ('ONNN', IPL, 0, INBUF, IERR)
            IF (IERR.NE.0) GO TO 975
            CALL IMCHAR (IPL, IX0, IY0, IQ, 0, STRING(1:NCH), INBUF,
     *         IERR)
            IF (IERR.EQ.0) POTERR = 0
            END IF
C                                       Grey plane
      ELSE
         CALL YSLECT ('ONNN', IP, 0, INBUF, IERR)
         IF (IERR.NE.0) GO TO 975
         CALL IMCHAR (IP, IX0, IY0, IQ, 0, STRING(1:NCH), INBUF, IERR)
         IF (IERR.NE.0) GO TO 975
C                                       Image catalog junk
         CALL FILL (256, 0, CATBLK)
         IF (DOVERT) IY0 = IY0 - (NCH-1)*CSIZTV(2)
         CALL RFILL (5, HBLANK, CATH(KHIMN))
         CALL CHR2H (2, 'ZZ', KHPTYO, CATH(KHPTY))
         CATBLK(IICOR  ) = IX0
         CATBLK(IICOR+1) = IY0
         CATBLK(IICOR+2) = IX0 + NXC - 1
         CATBLK(IICOR+3) = IY0 + NYC - 1
         CALL COPY (4, CATBLK(IICOR), CATBLK(IIWIN))
         CATBLK(KIDIM) = 2
         CATBLK(KINAX) = NXC
         CATBLK(KINAX+1) = NYC
         CATR(KRDMX) = 1.0
         CATR(KRDMN) = 0.0
         CATR(IRRAN) = CATR(KRDMN)
         CATR(IRRAN+1) = CATR(KRDMX)
         CALL RCOPY (2, HBLANK, CATH(KHBUN))
         CATR(KRCRP) = (CATBLK(IICOR) + CATBLK(IICOR+2)) / 2.0
         CATD(KDCRV) = (CATR(KRDMX) + CATR(KRDMN)) / 2.0
         CATR(KRCIC) = (CATR(KRDMX) - CATR(KRDMN)) / (CATBLK(IICOR+2) -
     *      CATBLK(IICOR))
         I = 2 * KICTPN
         CALL RFILL (I, HBLANK, CATH(KHCTP))
         CATR(KRCIC+1) = 0.0
         CATR(KRCRP+1) = CATBLK(IICOR+1) - 1
         CATD(KDCRV+1) = 0.0
         CALL YCWRIT (IP, CATBLK(IICOR), CATBLK, INBUF, IERR)
         POTERR = 0
         END IF
      CALL YHOLD ('OFFF', IERR)
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVLINE
C                                       draw line on TV
C-----------------------------------------------------------------------
C                                       Do we have a string?
 400  CALL ADVERB ('GRCHAN', 'I', 1, 0, J, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('TVCHAN', 'I', 1, 0, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((J.LE.0) .OR. (J.GT.NGRAPH)) J = MIN (2, NGRAPH)
C                                       set channel ID
      IF ((I.GT.0) .AND. (I.LE.NGRAY)) THEN
         NCH = I
      ELSE
         NCH = NGRAY + J
         END IF
      IF ((NCH.LE.0) .OR. (NCH.GT.NGRAY+NGRAPH)) NCH = NGRAY +
     *    MIN (NGRAPH, 2)
C                                       corners
      CALL ADVERB ('TVCORN', 'R', 2, 0, IDUM, RPOS, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IX0 = IROUND (RPOS(1))
      IY0 = IROUND (RPOS(2))
      CALL ADVERB ('TVXY', 'R', 2, 0, IDUM, RPOS, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IX1 = IROUND (RPOS(1))
      IY1 = IROUND (RPOS(2))
      IBUT = 0
      POTERR = 49
      IQ = 0
C                                       Read cursor for point 1
 410  IF ((IX0.GT.0) .AND. (IX0.LE.MAXXTV(1)) .AND. (IY0.GT.0) .AND.
     *   (IY0.LE.MAXXTV(2))) GO TO 420
         IF (IBUT.GE.8) GO TO 975
         WRITE (MSGTXT,1410) 'first'
         CALL MSGWRT (1)
         CALL TVWHER (IQ, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 975
         IF ((IX0.LE.0) .OR. (IX0.GT.MAXXTV(1))) IX0 = RPOS(1) + 0.5
         IF ((IY0.LE.0) .OR. (IY0.GT.MAXXTV(2))) IY0 = RPOS(2) + 0.5
         GO TO 410
C                                       Read cursor for point 2
 420  IF ((IX1.GT.0) .AND. (IX1.LE.MAXXTV(1)) .AND. (IY1.GT.0) .AND.
     *   (IY1.LE.MAXXTV(2))) GO TO 430
         IF (IBUT.GE.8) GO TO 975
         WRITE (MSGTXT,1410) 'second'
         CALL MSGWRT (1)
         CALL TVWHER (IQ, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 975
         IF ((IX1.LE.0) .OR. (IX1.GT.MAXXTV(1))) IX1 = RPOS(1) + 0.5
         IF ((IY1.LE.0) .OR. (IY1.GT.MAXXTV(2))) IY1 = RPOS(2) + 0.5
         GO TO 420
C                                       draw line
 430  CALL YHOLD ('ONNN', IERR)
      CALL YSLECT ('ONNN', NCH, 0, INBUF, IERR)
      IF (IERR.EQ.0) CALL IMVECT ('ONNN', NCH, 2, XP, YP, INBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      CALL YHOLD ('OFFF', IERR)
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVSTAR
C                                       plot image's stars on TV
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 500  IMTYPE = 'MA'
      LOCNUM = 1
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       test type
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
      IF (CTEST.NE.'MA') THEN
         POTERR = 101
         MSGTXT = 'TVSTAR WORKS ON IMAGES ONLY, NOT ' // CTEST
         GO TO 970
         END IF
C                                       set Graphics common
      CALL COPY (256, CATBLK, GPHCAT)
      GPHIX0 = GPHCAT(IICOR)
      GPHIY0 = GPHCAT(IICOR+1)
      GPHSCX = GPHCAT(IICOR+2) - GPHIX0
      GPHSCY = GPHCAT(IICOR+3) - GPHIY0
      GPHIX1 = GPHCAT(IIWIN)
      GPHIY1 = GPHCAT(IIWIN+1)
      GPHIX2 = GPHCAT(IIWIN+2)
      GPHIY2 = GPHCAT(IIWIN+3)
      GPHX1 = GPHIX1
      GPHX2 = GPHIX2
      GPHY1 = GPHIY1
      GPHY2 = GPHIY2
      GPHDOT = .TRUE.
      GPHDOD = .FALSE.
      GPHLTY = 1
      BLC(1) = GPHX1
      BLC(2) = GPHY1
      TRC(1) = GPHX2
      TRC(2) = GPHY2
C                                       get actual image header
      MSGSUP = 32000
      CALL CATIO ('READ', GPHCAT(IIVOL), GPHCAT(IICNO), CATBLK, 'REST',
     *   INBUF, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         POTERR = 33
         WRITE (MSGTXT,1500) IERR
         GO TO 970
         END IF
C                                       check: do conversions
C                                       like TVLOD/TVFIND
      CALL CATN2L ('L2N', CATBLK, INBUF)
      CALL CATN2L ('N2L', INBUF, CATBLK)
      DO 510 I = 1,112
         IF (GPHCAT(I).NE.CATBLK(I)) THEN
            POTERR = 33
            WRITE (MSGTXT,1505) I
            GO TO 970
            END IF
 510     CONTINUE
C                                       get adverbs
      CALL ADVERB ('INVERS', 'I', 1, 0, IVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IG, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('STFACTOR', 'R', 1, 0, IDUM, STFACT, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF (STFACT.EQ.0.0) STFACT = 1.0
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) IG = MIN (2, NGRAPH)
      CALL FNDEXT ('ST', CATBLK, MVER)
      IF (MVER.LE.0) THEN
         POTERR = 33
         MSGTXT = 'IMAGE MUST HAVE AN ST FILE'
         GO TO 970
         END IF
      IF ((IVER.LT.1) .OR. (IVER.GT.MVER)) IVER = MVER
      GPHTVG(1) = IG + NGRAY
      CALL YSLECT ('ONNN', GPHTVG(1), 0, INBUF, IERR)
      POTERR = 49
      IF (IERR.NE.0) GO TO 975
C                                       plot the things
      CALL SETLOC (GPHCAT(IIDEP), .FALSE.)
      CALL YHOLD ('ONNN', IERR)
      CALL STARPL (STFACT, GPHCAT(IIVOL), GPHCAT(IICNO), IVER, BLC, TRC,
     *   0, 0, GPHCAT, BLC, LOCNUM, INBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1510) IERR
         GO TO 970
      ELSE
         CALL YHOLD ('OFFF', IERR)
         POTERR = 0
         GO TO 975
         END IF
C-----------------------------------------------------------------------
C                                       COSTAR
C                                       plot cordinate pos on image
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 600  IMTYPE = 'MA'
      LOCNUM = 1
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       test type
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
      IF (CTEST.NE.'MA') THEN
         POTERR = 101
         MSGTXT = 'COSTAR WORKS ON IMAGES ONLY, NOT ' // CTEST
         GO TO 970
         END IF
C                                       set Graphics common
      CALL COPY (256, CATBLK, GPHCAT)
      GPHIX0 = GPHCAT(IICOR)
      GPHIY0 = GPHCAT(IICOR+1)
      GPHSCX = GPHCAT(IICOR+2) - GPHIX0
      GPHSCY = GPHCAT(IICOR+3) - GPHIY0
      GPHIX1 = GPHCAT(IIWIN)
      GPHIY1 = GPHCAT(IIWIN+1)
      GPHIX2 = GPHCAT(IIWIN+2)
      GPHIY2 = GPHCAT(IIWIN+3)
      GPHX1 = GPHIX1
      GPHX2 = GPHIX2
      GPHY1 = GPHIY1
      GPHY2 = GPHIY2
      GPHDOT = .TRUE.
      GPHDOD = .FALSE.
      GPHLTY = 1
      BLC(1) = GPHX1
      BLC(2) = GPHY1
      TRC(1) = GPHX2
      TRC(2) = GPHY2
C                                       get adverbs
      CALL ADVERB ('SYMBOL', 'I', 1, 0, NCH, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      NCH = MAX (1, MIN (23, NCH))
      CALL ADVERB ('COORDINA', 'R', 6, 0, IG, XCOORD, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IG, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) IG = MIN (2, NGRAPH)
      CALL ADVERB ('STFACTOR', 'R', 1, 0, IDUM, STFACT, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF (STFACT.EQ.0.0) STFACT = 1.0
      STFACT = ABS (STFACT)
C                                       on graphics
      GPHTVG(1) = IG + NGRAY
      CALL YHOLD ('ONNN', IERR)
      CALL YSLECT ('ONNN', GPHTVG(1), 0, INBUF, IERR)
      POTERR = 49
      IF (IERR.NE.0) GO TO 975
C                                       do coordinates
      CALL SETLOC (GPHCAT(IIDEP), .FALSE.)
      UNIQUE = (XCOORD(1).LT.0.0) .OR. (XCOORD(2).LT.0.0) .OR.
     *   (XCOORD(3).LT.0.0)
      XPOS = ABS (XCOORD(1)) + ABS (XCOORD(2))/60.0D0 +
     *   ABS (XCOORD(3))/3600.0D0
      IF (UNIQUE) XPOS = -XPOS
      IF ((CTYP(1,LOCNUM)(:4).EQ.'RA  ') .OR.
     *   (CTYP(1,LOCNUM)(:4).EQ.'RA--')) XPOS = XPOS * 15.0D0
      UNIQUE = (XCOORD(4).LT.0.0) .OR. (XCOORD(5).LT.0.0) .OR.
     *   (XCOORD(6).LT.0.0)
      YPOS = ABS(XCOORD(4)) + ABS(XCOORD(5))/60.0D0 +
     *   ABS(XCOORD(6))/3600.0D0
      IF (UNIQUE) YPOS = -YPOS
      IF ((CTYP(2,LOCNUM)(:4).EQ.'RA  ') .OR.
     *   (CTYP(2,LOCNUM)(:4).EQ.'RA--')) YPOS = YPOS * 15.0D0
C                                       Calc center of star pos
      CALL XYPIX (XPOS, YPOS, AX(1), AY(1), IERR)
C                                       If star not in plot, boo boo
      IF ((BLC(1).GT.AX(1)) .OR. (BLC(2).GT.AY(1)) .OR.
     *   (TRC(1).LT.AX(1)) .OR. (TRC(2).LT.AY(1))) IERR = 1
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1600) AX(1), AY(1)
         GO TO 970
         END IF
      AX(2) = AX(1)
      AX(3) = AX(1)
      AX(4) = AX(1) - 2.0 * STFACT
      AX(5) = AX(1) + 2.0 * STFACT
      AY(2) = AY(1) + 2.0 * STFACT
      AY(3) = AY(1) - 2.0 * STFACT
      AY(4) = AY(1)
      AY(5) = AY(1)
      CALL PNTPLT (NCH, AX, AY, BLC, TRC, .FALSE., .FALSE., INBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1510) IERR
         GO TO 970
      ELSE
         CALL YHOLD ('OFFF', IERR)
         POTERR = 0
         GO TO 975
         END IF
C-----------------------------------------------------------------------
C                                       TVILINE
C                                       plot line between pixels
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 700  IMTYPE = 'MA'
      LOCNUM = 1
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       test type
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
      IF (CTEST.NE.'MA') THEN
         POTERR = 101
         MSGTXT = 'TVILINE WORKS ON IMAGES ONLY, NOT ' // CTEST
         GO TO 970
         END IF
C                                       input adverbs
      CALL ADVERB ('GRCHAN', 'I', 1, 0, J, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('TVCHAN', 'I', 1, 0, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((J.LE.0) .OR. (J.GT.NGRAPH)) J = MIN (2, NGRAPH)
C                                       set channel ID
      IF ((I.GT.0) .AND. (I.LE.NGRAY)) THEN
         NCH = I
      ELSE
         NCH = NGRAY + J
         END IF
      IF ((NCH.LE.0) .OR. (NCH.GT.NGRAY+NGRAPH)) NCH = NGRAY +
     *    MIN (NGRAPH, 2)
C                                       corners
      CALL ADVERB ('PIXXY', 'R', 7, 0, IDUM, PIXXY, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      RPOS(1) = (CATBLK(IIWIN+2) - CATBLK(IIWIN))
      IF (RPOS(1).NE.0.0) RPOS(1) = (PIXXY(1) - CATBLK(IIWIN)) *
     *  (CATBLK(IICOR+2) - CATBLK(IICOR)) / RPOS(1) + CATBLK(IICOR)
      RPOS(2) = (CATBLK(IIWIN+3) - CATBLK(IIWIN+1))
      IF (RPOS(2).NE.0.0) RPOS(2) = (PIXXY(2) - CATBLK(IIWIN+1)) *
     *  (CATBLK(IICOR+3) - CATBLK(IICOR+1)) / RPOS(2) + CATBLK(IICOR+1)
      IX0 = IROUND (RPOS(1))
      IY0 = IROUND (RPOS(2))
      CALL ADVERB ('PIX2XY', 'R', 7, 0, IDUM, PIXXY, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      RPOS(1) = (CATBLK(IIWIN+2) - CATBLK(IIWIN))
      IF (RPOS(1).NE.0.0) RPOS(1) = (PIXXY(1) - CATBLK(IIWIN)) *
     *  (CATBLK(IICOR+2) - CATBLK(IICOR)) / RPOS(1) + CATBLK(IICOR)
      RPOS(2) = (CATBLK(IIWIN+3) - CATBLK(IIWIN+1))
      IF (RPOS(2).NE.0.0) RPOS(2) = (PIXXY(2) - CATBLK(IIWIN+1)) *
     *  (CATBLK(IICOR+3) - CATBLK(IICOR+1)) / RPOS(2) + CATBLK(IICOR+1)
      IX1 = IROUND (RPOS(1))
      IY1 = IROUND (RPOS(2))
C                                       draw line
      IF ((IX0.GE.1) .AND. (IX0.LE.MAXXTV(1)) .AND. (IX1.GE.1) .AND.
     *   (IX1.LE.MAXXTV(1)) .AND. (IY0.GE.1) .AND. (IY0.LE.MAXXTV(2))
     *   .AND. (IY1.GE.1) .AND. (IY1.LE.MAXXTV(2))) THEN
         CALL YHOLD ('ONNN', IERR)
         CALL YSLECT ('ONNN', NCH, 0, INBUF, IERR)
         IF (IERR.EQ.0) CALL IMVECT ('ONNN', NCH, 2, XP, YP, INBUF,
     *      IERR)
         IF (IERR.EQ.0) POTERR = 0
         CALL YHOLD ('OFFF', IERR)
      ELSE
         WRITE (MSGTXT,1700) IX0, IY0, IX1, IY1
         CALL MSGWRT (7)
         POTERR = 101
         END IF
      GO TO 975
C-----------------------------------------------------------------------
C                                       IM2TV
C                                       plot line between pixels
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 800  IMTYPE = 'MA'
      LOCNUM = 1
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
C                                       test type
      CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
      IF (CTEST.NE.'MA') THEN
         POTERR = 101
         MSGTXT = 'TVILINE WORKS ON IMAGES ONLY, NOT ' // CTEST
         GO TO 970
         END IF
C                                       corner
      CALL ADVERB ('PIXXY', 'R', 7, 0, IDUM, PIXXY, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      RPOS(1) = (CATBLK(IIWIN+2) - CATBLK(IIWIN))
      IF (RPOS(1).NE.0.0) RPOS(1) = (PIXXY(1) - CATBLK(IIWIN)) *
     *  (CATBLK(IICOR+2) - CATBLK(IICOR)) / RPOS(1) + CATBLK(IICOR)
      RPOS(2) = (CATBLK(IIWIN+3) - CATBLK(IIWIN+1))
      IF (RPOS(2).NE.0.0) RPOS(2) = (PIXXY(2) - CATBLK(IIWIN+1)) *
     *  (CATBLK(IICOR+3) - CATBLK(IICOR+1)) / RPOS(2) + CATBLK(IICOR+1)
      POTERR = 0
      CALL ADVRBS ('TVXY', 'R', 2, 0, IDUM, RPOS, CDUM)
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVLAYOUT
C                                       layout image on TV
C-----------------------------------------------------------------------
C                                       Which image? : get cat block
 900  IMTYPE = 'MA'
      LOCNUM = 1
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IBUT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IBUT.LE.0) .OR. (IBUT.GT.8)) IBUT = 2
      IBUT = IBUT + NGRAY
      CALL TVFIND (NGRAY, IMTYPE, IP, UNIQUE, CATBLK, INBUF, IERR)
      IF (IERR.NE.0) GO TO 975
      CALL ADVERB ('INFILE', 'C', 1, 48, IDUM, RDUM, INFILE)
      IF (ERRNUM.NE.0) GO TO 975
      POTERR = 49
      CALL YSLECT ('ONNN', IBUT, 0, INBUF, IERR)
      CALL LAYOUT (IBUT, INFILE, INBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       error handling and close
 960  MSGTXT = 'I DO NOT LABEL IMAGES WHICH ARE ALL ZEROES'
 970  CALL MSGWRT (6)
 975  CALL TVCLOS (INBUF, JERR)
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.GT.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1410 FORMAT ('Set cursor to ',A,' end point, push Button A, B, or C')
 1500 FORMAT ('ERROR',I5,' READING IMAGE HEADER FROM MAIN CATALOG')
 1505 FORMAT ('DISK HEADER DOES NOT MATCH IMAGE CATALOG WORD',I4)
 1510 FORMAT ('ERROR',I5,' FROM STARPL, PLOTTING THE STARS')
 1600 FORMAT ('PIXEL',2F10.2,' NOT IN IMAGE')
 1610 FORMAT ('ERROR',I5,' FROM PNTPLT, PLOTTING THE SYMBOL')
 1700 FORMAT ('ENDS NOT ON TV:',4I7)
      END
