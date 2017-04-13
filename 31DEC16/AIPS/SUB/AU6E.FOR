      SUBROUTINE AU6E (BRANCH)
C-----------------------------------------------------------------------
C! verbs to plot slices and models on graphics
C# POPS-appl Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2007-2009, 2012, 2014, 2016
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
C   Plot or replot slice file directly to the TV
C   Verbs:
C      BRANCH   I   6  TVSLICE  : plot slice file.
C                   7  TVASLICE : plot slice file but do not clear
C                                 screen or do labeling.
C                   8  TVMODEL  : plot gaussian model on TEK.
C                   9  TVAMODEL : plot model but do not label or clear.
C                  10  TVRESID  : plot residuals of data, model.
C                  11  TVARESID : plot residuals over current labeling.
C                  12  TVGUESS  : plot initial guess.
C                  13  TVAGUESS : plot initial guess on current graph.
C                  14  TVSET    : set guess parms for all components
C                  15  TV1SET   : set guess parms for 1 component
C                   1  TVCOMPS  : plot individual components
C                   2  TVACOMPS : plot components no label or clear
C   Common:  adverb values
C      INNAME   R(3)     name of primary file.
C      INCLASS  R(2)     class of primary file.
C      INSEQ    R        sequence number of primary file.
C      INDISK   R        disk volume number. 0 means try all.
C      INVER    R        version number of slice file, 0 means try
C                        latest version.
C      LTYPE    R        0=no labeling for plot, 1=standard.
C      PIXRANGE R(2)     the minimum and maximum values allowed for the
C                        plot All other values will be clipped If
C                        PIXRANGE(1) .GE. PIXRANGE(2) then the map max
C                        and min will be used.
C      BDROP    R        number of slice pts to drop from beginning
C                        of slice.
C      EDROP    R        number of slice pts to drop from end of slice.
C      NGAUSS   R        number of the gaussian model to print.
C                           (for TVMODEL, TVAMODEL only).
C      GPOS     R(2,4)   initial guess of position of gaussian
C                        components. (For TVGUESS, TVAGUESS only)
C      GMAX     R(4)     initial guess of max amplitude.
C                           (For TVGUESS, TVAGUESS only)
C      GWIDTH   R(3,4)   initial guess of half width.
C                           (For TVGUESS, TVAGUESS only)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER NAMEIN*12, CLASIN*6, PRGNAM*6, TYPEIN*2, TEXT(2)*80,
     *   CDUM*1, CHTEMP*2
      REAL      CHR(4), BLC(7), TRC(7), XBLC(7), XTRC(7), RANGE2(2),
     *   XFAC, XOFF, RANGE(2), YGAP, RDUM, GM(4), GP(2,4),  GW(3,4),
     *   VAL, ARC, FQFINC
      INTEGER   IMLUN, IVOL, IERR, ISFIND, LABEL, ISLUN, IMFIND, IFMOD,
     *   IMOD, ISLOT, IUSER, IDROP(2), INGAUS, INPTS, ISEQ, ISVER,
     *   NTEXT, IDUM, POTERR, IRXTRA, ISTART, IEND, I, K, ZAND, ITEMP,
     *   IX, IY, IDEPTH(5)
      LOGICAL   T, F, NOSAVE, EXCL, WAIT
      DOUBLE PRECISION FQFREQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (T, WAIT, EXCL),    (F, NOSAVE)
      DATA IMLUN, ISLUN /16,27/
      DATA F, T /.FALSE.,.TRUE./
      DATA PRGNAM /'AU9B  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.15)) GO TO 999
      IF ((BRANCH.GT.2) .AND. (BRANCH.LE.5)) GO TO 999
      POTERR = 101
      LOCNUM = 1
C                                       get adverbs
      CALL ADVERB ('GMAX', 'R', 4, 0, IDUM, GM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('GPOS', 'R', 8, 0, IDUM, GP, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('GWIDTH', 'R', 12, 0, IDUM, GW, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('NGAUSS', 'I', 1, 0, INGAUS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      INGAUS = MAX (1, MIN (4, INGAUS))
      CALL ADVERB ('GRCHAN', 'I', 1, 0, IGR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Plotting
      IF (BRANCH.GT.13) GO TO 100
      CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INVERS', 'I', 1, 0, ISVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IUSER = NLUSER
      CALL ADVERB ('LTYPE', 'I', 1, 0, LABEL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      LABEL = ABS (LABEL)
      I = MOD (LABEL, 100)
      IF ((I.LE.0) .OR. (I.GT.10)) I = 3
      LABEL = (LABEL/100) * 100 + I
      CALL ADVERB ('BDROP', 'I', 1, 0, IDROP(1), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('EDROP', 'I', 1, 0, IDROP(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IDROP(1) = MAX (0, IDROP(1))
      IDROP(2) = MAX (0, IDROP(2))
      CALL ADVERB ('PIXRANGE', 'R', 2, 0, IDUM, RANGE, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Open map file & get header.
      IMOD = 0
      IF (BRANCH.GT.2) THEN
         CALL ADVERB ('IN2VERS', 'I', 1, 0, IMOD, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, NAMEIN)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, CLASIN)
      IF (ERRNUM.NE.0) GO TO 980
      TYPEIN = ' '
      CALL MAPOPN ('READ', IVOL, NAMEIN, CLASIN, ISEQ, TYPEIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, ISBLK, IERR)
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Close map file.
      CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, NOSAVE,
     *   ISBLK, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'WARNING. ERROR CLOSING MAP FILE'
         CALL MSGWRT (6)
         END IF
C                                       Open TV device.
      POTERR = 49
      CALL TVOPEN (INBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       check TV state
      IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *   (TVLIMG(1).NE.TVLIMG(4))) THEN
         POTERR = 77
         GO TO 970
         END IF
C                                       Open SL extension file.
      CALL OPEXT ('SL', IVOL, ISLOT, ISVER, ISLUN, EXCL, WAIT, ISFIND,
     *   IERR)
      IF (IERR.NE.0) GO TO 970
      IGR = MAX (1, MIN (IGR, NGRAPH-1)) + NGRAY
      CALL YHOLD ('ONNN', IERR)
      CALL YSLECT ('ONNN', IGR, 0, INBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Read 1st rec of slice file,
C                                       do scaling, init variables.
      CALL TVSLIN (BRANCH, ISLUN, ISFIND, LABEL, IMOD, RANGE, IDROP,
     *   BLC, TRC, XBLC, XTRC, RANGE2, XFAC, XOFF, CHR, YGAP, TEXT,
     *   NTEXT, IFMOD, INPTS, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Clear screen, Draw borders,
C                                       tics and some annotation.
      IF ((BRANCH.EQ.1) .OR. ((BRANCH.GT.5) .AND. (MOD(BRANCH,2).EQ.0)))
     *   THEN
C                                       Init image catalog.
         CALL YCINIT (IGR, INBUF)
         CALL YZERO (IGR, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       Update image catalog
         CALL YCWRIT (IGR, CATBLK(IICOR), CATBLK, ISBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040)
            CALL MSGWRT (6)
            END IF
         CALL TVLAB (BLC, TRC, LABEL, YGAP, TEXT, NTEXT, CHR, F, IERR)
         IF (IERR.NE.0) GO TO 960
         END IF
C                                       Plot slice file.
      IF ((BRANCH.EQ.6) .OR. (BRANCH.EQ.7)) THEN
         CALL TVSLPL ('SLIC', ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *      IMOD, GM, GP, GW, IERR)
      ELSE IF ((BRANCH.EQ.8) .OR. (BRANCH.EQ.9)) THEN
         CALL TVSLPL ('MODL', ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *      IMOD, GM, GP, GW, IERR)
      ELSE IF ((BRANCH.EQ.10) .OR. (BRANCH.EQ.11)) THEN
         CALL TVSLPL ('RESI', ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *      IMOD, GM, GP, GW, IERR)
      ELSE IF ((BRANCH.EQ.12) .OR. (BRANCH.EQ.13)) THEN
         CALL TVSLPL ('GUES', ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *      INGAUS, GM, GP, GW, IERR)
      ELSE IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.2)) THEN
         CALL TVSLPL ('COMP', ISLUN, ISFIND, RANGE2, BLC, TRC, IFMOD,
     *      INGAUS, GM, GP, GW, IERR)
         END IF
      IF (IERR.EQ.0) POTERR = 0
      CALL YHOLD ('OFFF', IERR)
      GO TO 960
C-----------------------------------------------------------------------
C                                       TV1SET
C                                       TVSET
C                                       set guesses
C-----------------------------------------------------------------------
 100  IF (BRANCH.EQ.14) THEN
         ISTART = 1
         IEND = INGAUS
         CALL RFILL (8, 0.0, GP)
         CALL RFILL (12, 0.0, GW)
         CALL RFILL (4, 0.0, GM)
      ELSE
         IF ((SP.LT.1) .OR. (STACK(SP).EQ.2)) THEN
            ERRNUM = 8
            GO TO 980
            END IF
         ISTART = V(SP) + .01
         IF ((ISTART.GT.INGAUS) .OR. (ISTART.LT.1)) THEN
            WRITE (MSGTXT,1100) ISTART, INGAUS
            CALL MSGWRT (6)
            POTERR = 101
            GO TO 980
            END IF
         IEND = ISTART
         SP = SP - 1
         END IF
C                                       Open TV device.
      POTERR = 49
      CALL TVOPEN (INBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Find catblk
      DO 110 I = 1,NGRAPH-1
         IGR = I + NGRAY
         ITEMP = 2 ** (IGR-1)
         IF (ZAND(TVLIMG(1),ITEMP).NE.0) THEN
            IX = MAXXTV(1) / 2
            IY = MAXXTV(2) / 2
            CALL YCREAD (IGR, IX, IY, CATBLK, IERR)
            IF (IERR.EQ.0) THEN
               CALL H2CHR (2, KHPTYO, CATBLK(KHPTY), CHTEMP)
               IF (CHTEMP.EQ.'SL') GO TO 120
               IF ((CHTEMP.EQ.'MA') .AND. (CATBLK(IIPLT).EQ.5))
     *            GO TO 120
               END IF
            END IF
 110     CONTINUE
      MSGTXT = 'NO SLICE PLOT FOUND'
      CALL MSGWRT (6)
      ERRNUM = 101
      GO TO 970
C                                       SL found!
C                                       Reconstruct original values.
 120  LABEL = CATBLK(IIOTH)
      IDROP(1) = CATBLK(IIOTH+1)
      IDROP(2) = CATBLK(IIOTH+2)
      IRXTRA = IIOTH + 3
      XBLC(1) = CATR(IRXTRA)
      XBLC(2) = CATR(IRXTRA+1)
      XTRC(1) = CATR(IRXTRA+2)
      XTRC(2) = CATR(IRXTRA+3)
      XBLC(3) = CATR(IRXTRA+4)
      XTRC(3) = CATR(IRXTRA+5)
      IRXTRA = IRXTRA + 6
      IRXTRA = IRXTRA/2 + 1
      FQFREQ = CATD(IRXTRA)
      IRXTRA = 2*IRXTRA + 1
      FQFINC = CATR(IRXTRA)
      INPTS = CATBLK(IIWIN+2) - CATBLK(IIWIN) + IDROP(1) + IDROP(2) + 1
      RANGE(1) = CATR(IRRAN)
      RANGE(2) = CATR(IRRAN+1)
      BLC(2) = CATBLK(IIWIN+1)
      TRC(2) = CATBLK(IIWIN+3)
      K = 1
      DO 125 I = 4,7
         XBLC(I) = CATBLK(IIDEP+K)
         XTRC(I) = CATBLK(IIDEP+K)
         K = K + 1
 125     CONTINUE
      CALL COPY (5, CATBLK(IIDEP), IDEPTH)
C                                       Initialize location common.
      CALL SLBINI (IDROP, INPTS, RANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, IDEPTH, LABEL, YGAP, CHR, TEXT, NTEXT)
C                                       get the values
      DO 140 I = ISTART,IEND
C                                       Find arc sec of curs from cent.
         WRITE (MSGTXT,1130) I
         CALL MSGWRT (1)
         CALL TVSLAC (VAL, ARC, IERR)
         IF (IERR.NE.0) GO TO 970
         GP(1,I) = ARC
         GM(I) = VAL
C                                       Get halfwidth.
         WRITE (MSGTXT,1131) I
         CALL MSGWRT (1)
         CALL TVSLAC (VAL, ARC, IERR)
         IF (IERR.NE.0) GO TO 970
         GW(1,I) = 2.0 * ABS (GP(1,I) - ARC)
 140     CONTINUE
      CALL ADVRBS ('GPOS', 'R', 8, 0, IDUM, GP, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVRBS ('GWIDTH', 'R', 12, 0, IDUM, GW, CDUM)
      IF (ERRNUM.NE.0) GO TO 970
      CALL ADVRBS ('GMAX', 'R', 4, 0, IDUM, GM, CDUM)
      POTERR = 0
      GO TO 970
C                                       Close slice file.
 960  CALL ZCLOSE (ISLUN, ISFIND, IERR)
C                                       Close TV device.
 970  CALL TVCLOS (ISBLK, IERR)
C                                       Errors
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR OPENING MAP FILE. MAPOPN ERR =',I6)
 1040 FORMAT ('TV IMAGE CATALOG ERROR.')
 1100 FORMAT ('COMPONENT',I7,' OUT OF RANGE 1 -',I4)
 1130 FORMAT ('Put cursor at center & height of Gaussian comp',I2,
     *   ' press button')
 1131 FORMAT ('Put cursor at halfwidth of Gaussian component',I3,
     *   ' press button')
      END
