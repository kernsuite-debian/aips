      SUBROUTINE AU6 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to manipulate TV scroll, zoom, color tables, and TVHUEINT
C# POPS=appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-1999, 2008-2011, 2015
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
C   AU6 performs TV image enhancement functions:
C   BRANCH = 1   OFFPSEUD  (OFM restet to linear)
C            2   OFFZOOM   (zoom set to zero)
C            3   OFFSCROL  (scrolls of IMCHAN to zero)
C            4   TVZOOM    (interative zooming)
C            5   TVSCROL   (interactive scrolling of IMCHAN)
C            6   TVPSEUDO  (interactive set of OFM to color contours)
C                          (interactive set of OFM to linear colors)
C                          (interactive set of OFM to circle in hue)
C            7   TVHUEINT  (interactive enhancement with 1 channel
C                           setting hue and another setting intensity)
C            8   TVPHLAME  (interactive set of OFM to "flame" colors)
C            9   TVHELIX   (interactive hue-int helix OFM)
C           10   HUEWEDGE  (2 step wedges for HUEINT image)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PTVC.INC'
      CHARACTER PRGNAM*6, CDUM*1, LINTYP*2
      INTEGER   POTERR, NLEVS, ICOLOR, BUFFER(TVMOFM), IERR, ITEMP,
     *   IC, QUAD, ITW(3), ZOR, ITYPE, JERR, I, ICHAN, HCHAN, NROW, J,
     *   IBUT, SCROLX, SCROLY, IDUM, II, JJ, IROUND, NPIX, IPLAN, IX0,
     *   IY0, IPL, IBUFF2(4096), LIMAGE, LABEL, GRCH, IX, IY, ICMS(12),
     *   ILAB, ZAND, DOZERO, ISIDE
      REAL      EPS, SLOPE, RPOS(2), A, RBUF(TVMOFM), OFFS, RDUM
      LOGICAL   T, F, ASKPOS, UNIQUE, DOGRID
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /AIPSCR/ BUFFER, IBUFF2
      EQUIVALENCE (BUFFER, RBUF)
      DATA EPS /0.05/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'AU6 '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C                                        general parameters
      QUAD = -1
      POTERR = 49
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      CALL ADVERB ('COLORS', 'I', 1, 0, ICOLOR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      CALL ADVERB ('TVCHAN', 'I', 1, 0, ICHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
      CALL ADVERB ('TVLEVS', 'I', 1, 0, NLEVS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 990
C                                        open TV device
      CALL TVOPEN (BUFFER, IERR)
      CALL ZTIME (ITW)
      IF (IERR.NE.0) THEN
         POTERR = 101
         GO TO 990
         END IF
C                                       check TV state
      IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         IF ((BRANCH.NE.1) .AND. (BRANCH.NE.6) .AND. (BRANCH.NE.8) .AND.
     *      (BRANCH.NE.9)) THEN
            POTERR = 77
            GO TO 980
            END IF
         END IF
C                                        interpret adverbs
      IF ((ICOLOR.LT.1) .OR. (ICOLOR.GT.3)) ICOLOR = 0
      IF (ICOLOR.GT.0) ICOLOR = 2 ** (3-ICOLOR)
      IF (ICOLOR.EQ.0) ICOLOR = 7
      IF ((ICHAN.GT.0) .AND. (ICHAN.LE.NGRAY)) THEN
         IC = 2 ** (ICHAN-1)
      ELSE
         ICHAN = 0
         I = 2**NGRAY
         IC = ZOR (TVLIMG(1), TVLIMG(2))
         IC = ZOR (IC, TVLIMG(3))
         IC = ZOR (IC, TVLIMG(4))
         IC = MOD (IC, I)
         END IF
      IF (IC.EQ.0) IC = 1
      IF (NLEVS.LT.10) NLEVS = LUTOUT + 1
      IF (NLEVS.LE.LUTOUT) NLEVS = LUTOUT + 1
C                                        branch to Operation
 10   GO TO (100, 200, 300, 400, 500, 600, 700, 800, 900, 910), BRANCH
C-----------------------------------------------------------------------
C                                        OFFCOLOR
C-----------------------------------------------------------------------
 100  I = OFMINP + 1
      CALL RFILL (I, 0.0, RBUF)
      NLEVS = LUTOUT + 1
      IF (I.LT.NLEVS) NLEVS = I
      OFFS = 1.0 / REAL (OFMOUT)
      SLOPE = OFFS * (OFMOUT + 1.0) / REAL(NLEVS)
      DO 110 I = 1,NLEVS
         RBUF(I) = I * SLOPE - OFFS
 110     CONTINUE
      I = (OFMINP + 1) / NLEVS
      JJ = NLEVS
      DO 111 II = 2,I
         CALL RCOPY (NLEVS, RBUF, RBUF(JJ+1))
         JJ = JJ + NLEVS
 111     CONTINUE
      CALL YOFM ('WRIT', ICOLOR, F, RBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        OFFZOOM
C-----------------------------------------------------------------------
 200  TVZOOM(1) = 0
      TVZOOM(2) = MAXXTV(1) / 2
      TVZOOM(3) = MAXXTV(2) / 2
      CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), F, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        OFFSCROL
C-----------------------------------------------------------------------
 300  CALL ADVERB ('GRCHAN', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IC = 0
      IF ((ICHAN.GT.0) .AND. (ICHAN.LE.NGRAY)) IC = 2 ** (ICHAN-1)
      ITEMP = 2 ** NGRAY
      IF (ABS(RDUM).GT.EPS) IC = ZOR (IC, ITEMP)
      IF (IC.EQ.0) THEN
         II = ZOR (TVLIMG(1), TVLIMG(2))
         II = ZOR (II, TVLIMG(3))
         II = ZOR (II, TVLIMG(4))
         IC = MOD (II, ITEMP)
         IF (IC.NE.II) IC = ZOR (IC, ITEMP)
         END IF
      SCROLX = 0
      SCROLY = 0
      CALL YSCROL (IC, SCROLX, SCROLY, F, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        TVZOOM
C-----------------------------------------------------------------------
 400  CALL TVZOME (IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        TVSCROL
C-----------------------------------------------------------------------
C                                        scroll graphics too ?
 500  IC = 0
      IF ((ICHAN.GT.0) .AND. (ICHAN.LE.NGRAY)) IC = 2 ** (ICHAN-1)
      ITEMP = 2 ** NGRAY
      CALL ADVERB ('GRCHAN', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (ABS(RDUM).GT.EPS) IC = ZOR (IC, ITEMP)
      IF (IC.EQ.0) THEN
         II = ZOR (TVLIMG(1), TVLIMG(2))
         II = ZOR (II, TVLIMG(3))
         II = ZOR (II, TVLIMG(4))
         IC = MOD (II, ITEMP)
         IF (IC.NE.II) IC = ZOR (IC, ITEMP)
         END IF
      CALL TVSCRL (IC, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        TVPSEUDO
C                                        Button A: RGB triangles
C                                        Button B: Loops in hue
C                                        Button C: color contours
C-----------------------------------------------------------------------
 600  CALL TVPSUD (NLEVS, BUFFER, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                       TVHUEINT
C                                       hue/intensity enhancement
C-----------------------------------------------------------------------
 700  POTERR = 31
      IF (NGRAY.LT.2) GO TO 970
      IF (OFMINP+1.LT.2*(LUTOUT+1)) THEN
         MSGTXT = 'HUE-INTENSITY WILL NOT WORK ON THIS TV DEVICE'
         CALL MSGWRT (7)
         GO TO 970
         END IF
C                                       Get 2 channel numbers
      CALL ADVERB ('TV2CHAN', 'I', 1, 0, HCHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      HCHAN = MOD (HCHAN, 10)
      IF ((ICHAN.LE.0) .OR. (ICHAN.GT.NGRAY) .OR. (HCHAN.LE.0) .OR.
     *   (HCHAN.GT.NGRAY) .OR. (HCHAN.EQ.ICHAN)) THEN
         ICHAN = 1
         HCHAN = 2
         END IF
C                                       Turn on the desired channels
      POTERR = 49
      CALL YHOLD ('ONNN', IERR)
      DO 705 I = 1,NGRAY
         CALL YSLECT ('OFFF', I, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 980
 705     CONTINUE
      CALL YSLECT ('ONNN', ICHAN, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL YSLECT ('ONNN', HCHAN, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       anti-log OFM
      A = LOG10 (REAL(LUTOUT)) / LUTOUT
      SLOPE = 1.0 / (REAL(LUTOUT) ** 2)
      ITYPE = OFMINP + 1
      CALL RFILL (OFMINP, 0.0, RBUF)
      ICOLOR = 2 * LUTOUT + 1
      DO 710 I = 1,ICOLOR
         RBUF(I) = SLOPE * (10.0 ** (A * (I-1)))
 710     CONTINUE
C                                       Inhibit the usual gamma corr.
      OFFS = TVGAMA
      TVGAMA = 1.0
      ICOLOR = 7
      CALL YOFM ('WRIT', ICOLOR, T, RBUF, IERR)
      TVGAMA = OFFS
      IF (IERR.NE.0) GO TO 980
C                                       Do it in separate routine
      CALL ADVERB ('DOCIRCLE', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      ITYPE = 0
      IF (RDUM.GT.0.0) ITYPE = 1
      CALL YHOLD ('OFFF', IERR)
      CALL HIENH (HCHAN, ICHAN, ITYPE, RPOS, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        TVPHLAME
C                                        Button A: RGB triangles
C                                        Button B: Loops in hue
C                                        Button C: color contours
C-----------------------------------------------------------------------
 800  CALL TVFLAM (NLEVS, BUFFER, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        TVHELIX
C                                        Button A: RGB triangles?
C                                        Button B: Loops in hue?
C                                        Button C: color contour?s
C-----------------------------------------------------------------------
 900  CALL ADVERB ('DOPRINT', 'I', 1, 0, HCHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL TVHELX (NLEVS, HCHAN, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        HUEWEDGE
C                                        Button A: RGB triangles?
C                                        Button B: Loops in hue?
C                                        Button C: color contour?s
C-----------------------------------------------------------------------
 910  NROW = 16
      ASKPOS = .FALSE.
      IF ((SP.GE.1) .AND. (STACK(SP).NE.2)) THEN
         I = IROUND (V(SP))
         J = ABS (I)
         IF ((J.GE.1) .AND. (J.LE.MAXXTV(2)/4)) THEN
            NROW = J
            ASKPOS = I.LT.0
            SP = SP - 1
            END IF
         END IF
C                                       Get 2 channel numbers
      CALL ADVERB ('TV2CHAN', 'I', 1, 0, HCHAN, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('GRCHAN', 'I', 1, 0, GRCH, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((GRCH.LT.1) .OR. (GRCH.GT.7)) GRCH = 2
      CALL ADVERB ('LABEL', 'I', 1, 0, LABEL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      HCHAN = MOD (HCHAN, 10)
      IF ((ICHAN.LE.0) .OR. (ICHAN.GT.NGRAY) .OR. (HCHAN.LE.0) .OR.
     *   (HCHAN.GT.NGRAY) .OR. (HCHAN.EQ.ICHAN)) THEN
         ICHAN = 1
         HCHAN = 2
         END IF
C                                       Turn on the desired channels
      LIMAGE = TVLIMG(1)
      POTERR = 49
      CALL YHOLD ('ONNN', IERR)
      CALL YSPLIT ('READ', IX, IY, ICMS(1), ICMS(5), ICMS(9), .FALSE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      DO 915 I = 1,NGRAY
         CALL YSLECT ('OFFF', I, 0, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 980
915      CONTINUE
      CALL YSLECT ('ONNN', ICHAN, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      LINTYP = 'MA'
      CALL TVFIND (NGRAY, LINTYP, IPLAN, UNIQUE, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (ASKPOS) THEN
         WRITE (MSGTXT,1910)
         CALL MSGWRT (1)
         CALL TVWHER (IPL, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 980
         IY0 = IROUND (RPOS(2))
         IF ((IY0.LT.CATBLK(IICOR+1)) .AND. (IY0.GE.CATBLK(IICOR+1)
     *      -NROW-2)) IY0 = MAX (1, CATBLK(IICOR+1)-NROW)
         IF ((IY0.GT.CATBLK(IICOR+3)) .AND. (IY0.LT.CATBLK(IICOR+3)
     *      +NROW)) IY0 = MIN (CATBLK(IICOR+3)+1, MAXXTV(2)+1-NROW)
      ELSE
C                                       leave room labels
         IY0 = CATBLK(IICOR+1) - 3.333*CSIZTV(2) - 4.5 - NROW
         IF (IY0-2.7*CSIZTV(2).LE.0.5) THEN
            IY0 = CATBLK(IICOR+3) + 5.2*CSIZTV(2) + 1.5
            IF (IY0+NROW.GT.MAXXTV(2)) THEN
C                                       put next to, or on, img
               IY0 = CATBLK(IICOR+1) - NROW
               IF (IY0.LT.1) THEN
                  IY0 = CATBLK(IICOR+3) + 1
                  IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = CATBLK(IICOR+1)
                  END IF
               END IF
            END IF
         END IF
      ISIDE = 1
      IF (IY0.GT.CATBLK(IICOR+3)) ISIDE = 3
      IX0 = CATBLK(IICOR)
      NPIX = CATBLK(IICOR+2) - IX0 + 1
      CALL ADVERB ('PIXRANGE', 'R', 2, 0, IDUM, RPOS, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOINVERS', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      LOCNUM = 1
      LABTYP(1) = 0
      IF (RDUM.GT.0.0) THEN
         CALL IWEDGE (2, ISIDE, ICHAN, IX0, IY0, NPIX, NROW, RPOS,
     *      BUFFER, IBUFF2, IERR)
      ELSE
         CALL IWEDGE (2, 0, ICHAN, IX0, IY0, NPIX, NROW, RPOS, BUFFER,
     *      IBUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      DOGRID = .FALSE.
      DOZERO = 1
      LABTYP(1) = 0
      IF (ZAND(LABEL,1).EQ.1) THEN
         ILAB = 7
         CALL IAXIS1 (BUFFER, ILAB, GRCH, DOZERO, DOGRID, IERR)
         IF (IERR.NE.0) GO TO 980
         DOZERO = 0
         END IF
      CALL YHOLD ('OFFF', IERR)
      CALL YHOLD ('ONNN', IERR)
      CALL YSLECT ('OFFF', ICHAN, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL YSLECT ('ONNN', HCHAN, 0, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVFIND (NGRAY, LINTYP, IPLAN, UNIQUE, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (ZAND(LABEL,4).EQ.4) THEN
         ILAB = 7
         CALL IAXIS1 (BUFFER, ILAB, GRCH, DOZERO, DOGRID, IERR)
         IF (IERR.NE.0) GO TO 980
         DOZERO = 0
         END IF
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      IF (RDUM.GT.0.0) THEN
         CALL IWEDGE (2, 0, HCHAN, IX0, IY0, NPIX, NROW, RPOS, BUFFER,
     *      IBUFF2, IERR)
      ELSE
         CALL IWEDGE (2, ISIDE, HCHAN, IX0, IY0, NPIX, NROW, RPOS,
     *      BUFFER, IBUFF2, IERR)
         END IF
      IF (IERR.NE.0) GO TO 980
      LABTYP(1) = 0
      IF (ZAND(LABEL,2).EQ.2) THEN
         ILAB = 7
         CALL IAXIS1 (BUFFER, ILAB, GRCH, DOZERO, DOGRID, IERR)
         IF (IERR.NE.0) GO TO 980
         DOZERO = 0
         END IF
      CALL YSPLIT ('WRIT', IX, IY, ICMS(1), ICMS(5), ICMS(9), .FALSE.,
     *   IERR)
      IF (IERR.NE.0) GO TO 999
      CALL YHOLD ('OFFF', IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        close down
C                                        cursor off, TV closed
 970  IF (BRANCH.GE.4) CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT,
     *   JERR)
 980  CALL TVCLOS (BUFFER, JERR)
C                                        messages on error
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR
         CALL MSGWRT (7)
         END IF
C
 990  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1910 FORMAT ('Please point out approx position for bottom edge',
     *   ' of wedge')
 1980 FORMAT ('TV ACTION ERROR CODE',I7)
      END
