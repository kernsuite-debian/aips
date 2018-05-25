      SUBROUTINE AU9B (BRANCH)
C-----------------------------------------------------------------------
C! verbs to plot slices and models on graphics
C# POPS-appl Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2007, 2009, 2014
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
C   Plot or replot slice file directly to the TEKTRONIX 4012.
C   Verbs:
C      BRANCH   I   1  TKSLICE  : plot slice file.
C                   2  TKASLICE : plot slice file but do not clear
C                                 screen or do labeling.
C                   3  TKMODEL  : plot gaussian model on TEK.
C                   4  TKAMODEL : plot model but do not label or clear.
C                   5  TKRESID  : plot residuals of data, model.
C                   6  TKARESID : plot residuals over current labeling.
C                   7  TKGUESS  : plot initial guess.
C                   8  TKAGUESS : plot initial guess on current graph.
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
C                           (for TKMODEL, TKAMODEL only).
C      GPOS     R(2,4)   initial guess of position of gaussian
C                        components. (For TKGUESS, TKAGUESS only)
C      GMAX     R(4)     initial guess of max amplitude.
C                           (For TKGUESS, TKAGUESS only)
C      GWIDTH   R(3,4)   initial guess of half width.
C                           (For TKGUESS, TKAGUESS only)
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER NAMEIN*12, CLASIN*6, PRGNAM*6, TYPEIN*2, TEXT(2)*80,
     *   CDUM*1
      REAL      CHR(4), BLC(7), TRC(7), XBLC(7), XTRC(7), RANGE2(2),
     *   SBLK(1), XFAC, XOFF, RANGE(2), YGAP, RDUM, GM(4), GP(2,4),
     *   GW(3,4)
      INTEGER   ISBLK(256), IMLUN, IVOL, IERR, ISFIND, ILABEL, I,
     *   ISLUN, IMFIND, IFMOD, IMOD, ISLOT, IUSER, IDROP(2), IERR2,
     *   INGAUS, INPTS, ISEQ, ISVER, NTEXT, IDUM, POTERR
      LOGICAL   T, F, NOSAVE, EXCL, WAIT
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /WORK/ ISBLK
      EQUIVALENCE (T, WAIT, EXCL),    (F, NOSAVE)
      EQUIVALENCE (SBLK, ISBLK)
      DATA IMLUN, ISLUN /16,27/
      DATA F, T /.FALSE.,.TRUE./
      DATA PRGNAM /'AU9B  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.8)) GO TO 999
      POTERR = 37
      IF (NTKDEV.LE.0) GO TO 980
      POTERR = 101
      LOCNUM = 1
C                                       Set TKSIZE in common
      CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INVERS', 'I', 1, 0, ISVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IUSER = NLUSER
      CALL ADVERB ('LTYPE', 'I', 1, 0, ILABEL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      ILABEL = ABS (ILABEL)
      I = MOD (ILABEL, 100)
      IF ((I.LE.0) .OR. (I.GT.10)) I = 3
      ILABEL = (ILABEL/100) * 100 + I
      CALL ADVERB ('BDROP', 'I', 1, 0, IDROP(1), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('EDROP', 'I', 1, 0, IDROP(2), RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IDROP(1) = MAX (0, IDROP(1))
      IDROP(2) = MAX (0, IDROP(2))
      CALL ADVERB ('PIXRANGE', 'R', 2, 0, IDUM, RANGE, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('NGAUSS', 'I', 1, 0, INGAUS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (INGAUS.LT.1) INGAUS = 1
      IF (INGAUS.GT.4) INGAUS = 4
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
 10   CALL MAPCLS ('READ', IVOL, ISLOT, IMLUN, IMFIND, CATBLK,
     *   NOSAVE, ISBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         END IF
C                                       Open SL extension file.
 20   CALL OPEXT ('SL', IVOL, ISLOT, ISVER, ISLUN, EXCL, WAIT,
     *   ISFIND, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Open TEK device.
      CALL ZTKOPN (IERR)
      IF (IERR.NE.0) GO TO 910
C                                       Read 1st rec of slice file,
C                                       do scaling, init variables.
      CALL TKSLIN (BRANCH, ISLUN, ISFIND, ILABEL, IMOD, RANGE, IDROP,
     *   BLC, TRC, XBLC, XTRC, RANGE2, XFAC, XOFF, CHR, YGAP,
     *   TEXT, NTEXT, IFMOD, INPTS, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Clear screen, Draw borders,
C                                       tics and some annotation.
      IF (MOD(BRANCH,2).EQ.1) THEN
         CALL TKLAB (BLC, TRC, ILABEL, YGAP, TEXT, NTEXT, CHR,
     *      F, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Update image catalog
         CALL TKCATL ('WRIT', ISBLK, ISBLK, CATBLK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040)
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Plot slice file.
      IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.2)) THEN
         CALL TKSLPL (ISLUN, ISFIND, RANGE2, BLC, TRC, IERR)
      ELSE IF ((BRANCH.EQ.3) .OR. (BRANCH.EQ.4)) THEN
         CALL TKGMPL (INPTS, IFMOD, IMOD, ISLUN, ISFIND, RANGE2, BLC,
     *      TRC, IERR)
      ELSE IF ((BRANCH.EQ.5) .OR. (BRANCH.EQ.6)) THEN
         CALL TKRSPL (INPTS, IFMOD, IMOD, ISLUN, ISFIND, RANGE2, BLC,
     *      TRC, IERR)
      ELSE IF ((BRANCH.EQ.7) .OR. (BRANCH.EQ.8)) THEN
         CALL ADVERB ('GMAX', 'R', 4, 0, IDUM, GM, CDUM)
         IF (ERRNUM.NE.0) GO TO 900
         CALL ADVERB ('GPOS', 'R', 8, 0, IDUM, GP, CDUM)
         IF (ERRNUM.NE.0) GO TO 900
         CALL ADVERB ('GWIDTH', 'R', 12, 0, IDUM, GW, CDUM)
         IF (ERRNUM.NE.0) GO TO 900
         CALL TKGGPL (INGAUS, GP, GM, GW, RANGE2, BLC, TRC, IERR)
         END IF
      IF (IERR.NE.0) GO TO 900
      POTERR = 0
C                                       Close TEK device.
 900  CALL ZTKCLS (IERR2)
C                                       Close slice file.
 910  CALL ZCLOSE (ISLUN, ISFIND, IERR2)
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
 1010 FORMAT ('WARNING. ERROR CLOSING MAP FILE')
 1040 FORMAT ('TEK IMAGE CATALOG ERROR.')
      END
