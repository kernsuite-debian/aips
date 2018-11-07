LOCAL INCLUDE 'PTVHUI.INC'
      INTEGER   NOPT, MAXCHR
      PARAMETER (NOPT = 11)
      PARAMETER (MAXCHR = 19)
LOCAL END
LOCAL INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMIN*12, CLSIN*6, NAMIN2*12, CLSIN2*6, NAMIN3*12,
     *   CLSIN3*6, NAMOUT*12, CLSOUT*6, NAMOU2*12, CLSOU2*6, OPTYPE*4
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMI3(3),
     *   XCLSI3(2), XNAMO(3), XCLSO(2), XNAMO2(3), XCLSO2(2), XOPTYP
      REAL      SEQIN, DSKIN, SEQIN2, DSKIN2, SEQIN3, DSKIN3, DOOUT,
     *   XOUTS, XOUTD, XOUTS2, XOUTD2, XSIZE(2), BLC(7), TRC(7), XXINC,
     *   XYINC, DOGRID, XTVCH, DOWDGE, DOCIRC, RANGE(2), DPARM(10)
      INTEGER   CATBLK(256), CATBL2(256), CATBL3(256), CATBL4(256),
     *   CATBL5(256)
      REAL      CATR(256), CATR2(256), CATR3(256), CATR4(256),
     *   CATR5(256)
      DOUBLE PRECISION CATD(128), CATD2(128), CATD3(128), CATD4(128),
     *   CATD5(128)
      HOLLERITH CATH(256), CATH2(256), CATH3(256), CATH4(256),
     *   CATH5(256)
      LOGICAL   DO3CHN, DOSAT, FIRST, T, F, DOGRC, DOOPT
      REAL      PRANGE(2,3), ABLC(7,3), ATRC(7,3), SVGAMA, OBLC(7,3),
     *   OTRC(7,3), BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS),
     *   SC(2,3), DSC(2,3)
      INTEGER   LUN1, LUN2, LUN3, IND1, IND2, IND3, VOL1, VOL2, VOL3,
     *   SLOT1, SLOT2, SLOT3, SEQ1, SEQ2, SEQ3, IUSER, TVCH, JBUFSZ,
     *   VOLO, SLOTO, SEQO, IXWDGE, IYWDGE, NX, NY, INX, IDX, INY, IDY,
     *   NLI, NLH, NLS, XINC, YINC, LWINTV(4), IDWIN(4), GRCURS, GRX0,
     *   GRY0, GRMENU, GRWIND, DOLOGI, DOLOGH, SEQO2, VOLO2
      COMMON /MAPHDR/ CATBLK, CATBL2, CATBL3, CATBL4, CATBL5
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XNAMI2, XCLSI2,
     *   SEQIN2, DSKIN2, XNAMI3, XCLSI3, SEQIN3, DSKIN3, DOOUT, XNAMO,
     *   XCLSO, XOUTS, XOUTD, XNAMO2, XCLSO2, XOUTS2, XOUTD2, XSIZE,
     *   BLC, TRC, XXINC, XYINC, DOGRID, XTVCH, XOPTYP, DOWDGE, DOCIRC,
     *   RANGE, DPARM
      COMMON /CHARPM/ NAMIN, CLSIN, NAMIN2, CLSIN2, NAMIN3, CLSIN3,
     *   NAMOUT, CLSOUT, OPTYPE, NAMOU2, CLSOU2
      COMMON /BUFFRS/ BUFF1, BUFF2, BUFF3
      COMMON /TVHUIP/ PRANGE, ABLC, ATRC, OBLC, OTRC, SVGAMA, SC, DSC,
     *   DO3CHN, T, F, DOSAT, FIRST, DOLOGI, DOGRC, DOOPT, LUN1, LUN2,
     *   LUN3, IND1, IND2, IND3, VOL1, VOL2, VOL3, SLOT1, SLOT2, SLOT3,
     *   SEQ1, SEQ2, SEQ3, IUSER, TVCH, JBUFSZ, VOLO, SLOTO, SEQO,
     *   IXWDGE, IYWDGE, NX, NY, INX, IDX, INY, IDY, NLI, NLH, NLS,
     *   XINC, YINC, LWINTV, IDWIN, GRCURS, GRX0, GRY0, GRMENU, GRWIND,
     *   SEQO2, VOLO2, DOLOGH
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATBL2, CATR2, CATH2, CATD2)
      EQUIVALENCE (CATBL3, CATR3, CATH3, CATD3)
      EQUIVALENCE (CATBL4, CATR4, CATH4, CATD4)
      EQUIVALENCE (CATBL5, CATR5, CATH5, CATD5)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
LOCAL END
      PROGRAM TVHUI
C-----------------------------------------------------------------------
C! makes a TV image from images of hue and intensity
C# TV Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2002-2003, 2006, 2008-2009,
C;  Copyright (C) 2012-2015
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
C   TVHUI will display an image on the TV based on two input images, one
C   of hue and one of intensity.
C   INPUTS:   (from AIPS)
C      USERID   R      user number, 0 means use logon user number,
C                      32000 means any user can be accessed.
C      INNAME   R(3)   name of intensity image.
C      INCLASS  R(2)   class of intensity image.
C      INSEQ    R      sequence number of intensity image.
C      INDISK   R      disk volume number. 0 means try all.
C      IN2NAME  R(3)   name of hue image.
C      IN2CLASS R(2)   class of hue image.
C      IN2SEQ   R      sequence number of hue image.
C      IN2DISK  R      disk volume number. 0 means try all.
C      IN3NAME  R(3)   name of saturation image.
C      IN3CLASS R(2)   class of saturation image.
C      IN3SEQ   R      sequence number of saturation image.
C      IN3DISK  R      disk volume number. 0 means try all.
C      DOOUTPUT R      > 0 write output AIPS file w RGB axis
C      OUTNAME  H(3)   Output file name
C      OUTCLASS H(2)   Output file class
C      OUTSEQ   R      Output sequence number
C      OUTDISK  R      Output disk number
C      BLC      R(7)   the coordinate in the input file to become the
C                      left hand coordinate (1,1) of the displayed and
C                      output image.
C      TRC      R(7)   the coordinate in the input file to become the
C                      top right hand corner of the display/output
C      XINC     R      X pixel increment between input and display.
C                      Ignored when doing output.
C      YINC     R      Y pixel increment between input and display.
C                      Ignored when doing output.
C      DOALIGN  R      >= 0 => hue image must align w intensity
C      TVCHAN   R      Desired TV channel
C      OPTYPE   H      'S=1', 'LUT'
C      DOWEDGE  R      0 < DOWEDGE <= 1 -> grey wedge along top
C                      1.5 < DOWEDGE -> grey wedge along right edge
C                      2.5 < DOWEDGE both wedges
C      DOCIRCLE R      > 0 Plot coord grid rather than just ticks
C      PIXRANGE R(2)   the maximum and minimum values allowed for the
C                      map.  All other values will be clipped.  If
C                      IRANGE(1) .GE. IRANGE(2) then the map max and
C                      min will be used.
C      DPARM    R(10)  (1,2) pixrange of 2nd image
C                      (3,4) pixrange for saturation
C                      (5) # intensity / # hue levels used
C                      (6) saturation levels (0, 3, 4)
C                      (7) subimage total pixels <= 86000
C                      (8) force one-image mode even on IIS
C-----------------------------------------------------------------------
      INTEGER   MAXPIX
      PARAMETER (MAXPIX = 400000)
      INCLUDE 'INCS:PMAD.INC'
      REAL      INTENS(MAXPIX), HUE(MAXPIX), SATUR(MAXPIX)
      INTEGER   INPRMS, SCRTCH(MAXIMG), IRET, MAXP, JERR
      CHARACTER PRGNAM*6
C
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
      INPRMS = 71
      MAXP = MAXPIX
      PRGNAM = 'TVHUI'
      CALL TVHIIN (PRGNAM, INPRMS, MAXP, SCRTCH, IRET)
      IF (IRET.NE.0) GO TO 900
      CALL TVHINT (MAXP, INTENS, HUE, SATUR, SCRTCH, IRET)
C                                       close TV
 900  IF ((TVIND.GT.0) .AND. (TVIND2.GT.0)) THEN
         TVGAMA = SVGAMA
         CALL TVCLOS (SCRTCH, JERR)
         END IF
C                                       write output file
      IF ((IRET.EQ.0) .AND. (DOOUT.GT.0.0)) CALL TVHIOU (SCRTCH, IRET)
C                                       close files
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE TVHIIN (PRGNAM, INPRMS, MAXP, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   Routine to get parameters for TVHUI
C   Inputs:
C      PRGNAM   C*6      Program name
C      INPRMS   I        Number of data parameters from AIPS
C   In/out:
C      MAXP     I        Maximum number pixels
C   Outputs:
C       SCRTCH  I(256)   Scratch buffer
C       IRET    I        Return code 0=> OK, else just go to DIE
C   Task parameters are returned in common /INPARM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INPRMS, MAXP, SCRTCH(*), IRET
C
      INCLUDE 'PTVHUI.INC'
C
      INTEGER   IERR, IPOINT, IROUND, I, INC, J, II, JJ, IX, IY, MAXC,
     *   LWIN(4), MAGF, TVCODE, MX, MY, IM
      LOGICAL   REDUCE
      CHARACTER TYPE*2, CHTMP*8, CHTMP1*8, SUBR*6
      DOUBLE PRECISION   DX
      REAL      X, TRC2, TRC3, OFFS, SLOPE, DI(1536), DH(1536),
     *   DS(1536), RED(2048), GREEN(2048), BLUE(2048)
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (RED, BUFF2), (GREEN, BUFF2(2049)),
     *   (BLUE, BUFF2(4097)),
     *   (DI, BUFF3(1)), (DH, BUFF3(2049)), (DS, BUFF3(4097))
C-----------------------------------------------------------------------
      NSCR = 0
      NCFILE = 0
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
      JBUFSZ = 2 * MABFSS
      T = .TRUE.
      F = .FALSE.
      FIRST = T
C                                       Get input values from AIPS.
      CALL GTPARM (PRGNAM, INPRMS, RQUICK, XNAMIN, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = T
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
      ELSE
         RQUICK = F
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       AIPS Holleriths ->
C                                       characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (12, 1, XNAMI2, NAMIN2)
      CALL H2CHR (6, 1, XCLSI2, CLSIN2)
      CALL H2CHR (12, 1, XNAMI3, NAMIN3)
      CALL H2CHR (6, 1, XCLSI3, CLSIN3)
      CALL H2CHR (12, 1, XNAMO, NAMOUT)
      CALL H2CHR (6, 1, XCLSO, CLSOUT)
      CALL H2CHR (12, 1, XNAMO2, NAMOU2)
      CALL H2CHR (6, 1, XCLSO2, CLSOU2)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
C
      LUN1 = 16
      LUN2 = 17
      LUN3 = 18
      SEQ1 = IROUND (SEQIN)
      SEQ2 = IROUND (SEQIN2)
      SEQ3 = IROUND (SEQIN3)
      VOL1 = IROUND (DSKIN)
      VOL2 = IROUND (DSKIN2)
      VOL3 = IROUND (DSKIN3)
      IUSER = NLUSER
      TVCH = IROUND (XTVCH)
C                                       Open intensity map file
      TYPE = 'MA'
      SUBR = 'MAPOPN'
      CALL MAPOPN ('READ', VOL1, NAMIN, CLSIN, SEQ1, TYPE, IUSER,
     *   LUN1, IND1, SLOT1, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL1
      FCNO(NCFILE) = SLOT1
      FRW(NCFILE) = 0
C                                       Check windows
      TRC2 = TRC(3)
      TRC3 = TRC(4)
      CALL RCOPY (5, BLC(3), TRC(3))
      SUBR = 'WINDOW'
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL RFILL (7, 0.0, OBLC)
      CALL RFILL (7, 0.0, OTRC)
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), OBLC, OTRC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       save corners
      CALL RCOPY (5, BLC(3), TRC(3))
      CALL RCOPY (7, BLC, ABLC(1,1))
      CALL RCOPY (7, TRC, ATRC(1,1))
      CALL RCOPY (7, BLC, ABLC(1,2))
      CALL RCOPY (7, TRC, ATRC(1,2))
      CALL RCOPY (7, BLC, ABLC(1,3))
      CALL RCOPY (7, TRC, ATRC(1,3))
      ABLC(3,2) = TRC2
      ATRC(3,2) = TRC2
      ABLC(3,3) = TRC3
      ATRC(3,3) = TRC3
      CALL RCOPY (5, OBLC(3,1), OTRC(3,1))
      CALL RCOPY (7, OBLC, OBLC(1,2))
      CALL RCOPY (7, OTRC, OTRC(1,2))
      CALL RCOPY (7, OBLC, OBLC(1,3))
      CALL RCOPY (7, OTRC, OTRC(1,3))
      OBLC(3,2) = TRC2
      OTRC(3,2) = TRC2
      OBLC(3,3) = TRC3
      OTRC(3,3) = TRC3
C                                        Open HUE file
      REDUCE = F
      TYPE = 'MA'
      CALL MAPOPN ('READ', VOL2, NAMIN2, CLSIN2, SEQ2, TYPE, IUSER,
     *    LUN2, IND2, SLOT2, CATBL2, SCRTCH, IERR)
      IF (IERR.GE.2) THEN
         SUBR = 'MAPOPN'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL2
      FCNO(NCFILE) = SLOT2
      FRW(NCFILE) = 0
      SEQIN2 = SEQ2
      DSKIN2 = VOL2
C                                       Saturation image
      DOSAT = (NAMIN3.NE.' ') .OR. (CLSIN3.NE.' ') .OR. (SEQ3.NE.0)
      IF (DOSAT) THEN
         CALL MAPOPN ('READ', VOL3, NAMIN3, CLSIN3, SEQ3, TYPE, IUSER,
     *      LUN3, IND3, SLOT3, CATBL3, SCRTCH, IERR)
         IF (IERR.GE.2) THEN
            SUBR = 'MAPOPN'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL3
         FCNO(NCFILE) = SLOT3
         FRW(NCFILE) = 0
         SEQIN3 = SEQ3
         DSKIN3 = VOL3
      ELSE
         CALL COPY (256, CATBL2, CATBL3)
         CALL RCOPY (7, ABLC(1,2), ABLC(1,3))
         CALL RCOPY (7, ATRC(1,2), ATRC(1,3))
         CALL RCOPY (7, OBLC(1,2), OBLC(1,3))
         CALL RCOPY (7, OTRC(1,2), OTRC(1,3))
         END IF
C                                       Image 2,3 plane selection
      DO 20 I = 3,7
         J = KINAX + I - 1
         IM = 2
         IF ((CATBL2(J).LE.1) .OR. (CATBL2(KIDIM).LT.I)) THEN
            ABLC(I,2) = 1
            OBLC(I,2) = 1
         ELSE
            ABLC(I,2) = MAX (1.0, ABLC(I,2))
            IF (ABLC(I,2)-0.01.GT.CATBL2(J)) GO TO 45
            OBLC(I,2) = MAX (1.0, OBLC(I,2))
            END IF
         ATRC(I,2) = ABLC(I,2)
         OTRC(I,2) = OBLC(I,2)
         IM = 3
         IF ((CATBL3(J).LE.1) .OR. (CATBL3(KIDIM).LT.I)) THEN
            ABLC(I,3) = 1
            OBLC(I,3) = 1
         ELSE
            ABLC(I,3) = MAX (1.0, ABLC(I,3))
            IF (ABLC(I,3)-0.01.GT.CATBL3(J)) GO TO 45
            OBLC(I,3) = MAX (1.0, OBLC(I,3))
            END IF
         ATRC(I,3) = ABLC(I,3)
         OTRC(I,3) = OBLC(I,3)
 20      CONTINUE
C                                       Set Map 2 & 3 corners
      DO 35 I = 1,2
         J = KRCRP + I - 1
         IF (DOGRID.LT.-1.5) THEN
            ABLC(I,2) = ABLC(I,1)
            ATRC(I,2) = ATRC(I,1)
            ABLC(I,3) = ABLC(I,1)
            ATRC(I,3) = ATRC(I,1)
            OBLC(I,2) = OBLC(I,1)
            OTRC(I,2) = OTRC(I,1)
            OBLC(I,3) = OBLC(I,1)
            OTRC(I,3) = OTRC(I,1)
         ELSE
            ABLC(I,2) = CATR2(J) - CATR(J) + ABLC(I,1)
            ATRC(I,2) = CATR2(J) - CATR(J) + ATRC(I,1)
            ABLC(I,3) = CATR3(J) - CATR(J) + ABLC(I,1)
            ATRC(I,3) = CATR3(J) - CATR(J) + ATRC(I,1)
            OBLC(I,2) = CATR2(J) - CATR(J) + OBLC(I,1)
            OTRC(I,2) = CATR2(J) - CATR(J) + OTRC(I,1)
            OBLC(I,3) = CATR3(J) - CATR(J) + OBLC(I,1)
            OTRC(I,3) = CATR3(J) - CATR(J) + OTRC(I,1)
            END IF
         J = I - 1
         IF (DOGRID.GT.-0.1) THEN
            IF (CATR(KRCIC+J).EQ.0.0) GO TO 55
            IM = 2
            IF (CATR2(KRCIC+J).EQ.0.0) GO TO 45
            DX = CATD(KDCRV+J) + (ABLC(I,1) - CATR(KRCRP+J)) *
     *         CATR(KRCIC+J)
            X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *         CATR2(KRCRP+J)
            ABLC(I,2) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-ABLC(I,2)).GT.0.2))
     *         GO TO 45
            ATRC(I,2) = ABLC(I,2) + ATRC(I,1) - ABLC(I,1)
            IM = 3
            IF (CATR3(KRCIC+J).EQ.0.0) GO TO 45
            X = (DX - CATD3(KDCRV+J)) / CATR3(KRCIC+J) +
     *         CATR3(KRCRP+J)
            ABLC(I,3) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-ABLC(I,3)).GT.0.2))
     *         GO TO 45
            ATRC(I,3) = ABLC(I,3) + ATRC(I,1) - ABLC(I,1)
            IM = 2
            DX = CATD(KDCRV+J) + (OBLC(I,1) - CATR(KRCRP+J)) *
     *         CATR(KRCIC+J)
            X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *         CATR2(KRCRP+J)
            OBLC(I,2) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-OBLC(I,2)).GT.0.2))
     *         GO TO 45
            OTRC(I,2) = OBLC(I,2) + OTRC(I,1) - OBLC(I,1)
            IM = 3
            IF (CATR3(KRCIC+J).EQ.0.0) GO TO 45
            X = (DX - CATD3(KDCRV+J)) / CATR3(KRCIC+J) +
     *         CATR3(KRCRP+J)
            OBLC(I,3) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-OBLC(I,3)).GT.0.2))
     *         GO TO 45
            OTRC(I,3) = OBLC(I,3) + OTRC(I,1) - OBLC(I,1)
            END IF
C                                       smaller subimage needed?
         IF (ABLC(I,2).LT.1.0) THEN
            ABLC(I,1) = ABLC(I,1) + 1.0 - ABLC(I,2)
            ABLC(I,3) = ABLC(I,3) + 1.0 - ABLC(I,2)
            ABLC(I,2) = 1.0
            REDUCE = T
            END IF
         IF (ATRC(I,2).GT.CATBL2(KINAX+J)) THEN
            ATRC(I,1) = ATRC(I,1) + CATBL2(KINAX+J) - ATRC(I,2)
            ATRC(I,3) = ATRC(I,3) + CATBL2(KINAX+J) - ATRC(I,2)
            ATRC(I,2) = CATBL2(KINAX+J)
            REDUCE = T
            END IF
         IF (ABLC(I,3).LT.1.0) THEN
            ABLC(I,1) = ABLC(I,1) + 1.0 - ABLC(I,3)
            ABLC(I,2) = ABLC(I,2) + 1.0 - ABLC(I,3)
            ABLC(I,3) = 1.0
            REDUCE = T
            END IF
         IF (ATRC(I,3).GT.CATBL3(KINAX+J)) THEN
            ATRC(I,1) = ATRC(I,1) + CATBL3(KINAX+J) - ATRC(I,3)
            ATRC(I,2) = ATRC(I,2) + CATBL3(KINAX+J) - ATRC(I,3)
            ATRC(I,3) = CATBL3(KINAX+J)
            REDUCE = T
            END IF
         IF (ABLC(I,1).GE.ATRC(I,1)) GO TO 45
         IF (ABLC(I,2).GE.ATRC(I,2)) GO TO 45
         IF (ABLC(I,3).GE.ATRC(I,3)) GO TO 45
         IF (OBLC(I,2).LT.1.0) THEN
            OBLC(I,1) = OBLC(I,1) + 1.0 - OBLC(I,2)
            OBLC(I,3) = OBLC(I,3) + 1.0 - OBLC(I,2)
            OBLC(I,2) = 1.0
            REDUCE = T
            END IF
         IF (OTRC(I,2).GT.CATBL2(KINAX+J)) THEN
            OTRC(I,1) = OTRC(I,1) + CATBL2(KINAX+J) - OTRC(I,2)
            OTRC(I,3) = OTRC(I,3) + CATBL2(KINAX+J) - OTRC(I,2)
            OTRC(I,2) = CATBL2(KINAX+J)
            REDUCE = T
            END IF
         IF (OBLC(I,3).LT.1.0) THEN
            OBLC(I,1) = OBLC(I,1) + 1.0 - OBLC(I,3)
            OBLC(I,2) = OBLC(I,2) + 1.0 - OBLC(I,3)
            OBLC(I,3) = 1.0
            REDUCE = T
            END IF
         IF (OTRC(I,3).GT.CATBL3(KINAX+J)) THEN
            OTRC(I,1) = OTRC(I,1) + CATBL3(KINAX+J) - OTRC(I,3)
            OTRC(I,2) = OTRC(I,2) + CATBL3(KINAX+J) - OTRC(I,3)
            OTRC(I,3) = CATBL3(KINAX+J)
            REDUCE = T
            END IF
         IF (OBLC(I,1).GE.OTRC(I,1)) GO TO 45
         IF (OBLC(I,2).GE.OTRC(I,2)) GO TO 45
         IF (OBLC(I,3).GE.OTRC(I,3)) GO TO 45
 35      CONTINUE
C                                       Check true coincidence
      IF (DOGRID.GE.0.1) THEN
         INC = 2
         DO 40 I = 1,2
            J = I - 1
            X = 0.2 * 0.2 * ABS (CATR(KRCIC+J))
            IPOINT = KHCTP+J*INC
            IM = 2
            CALL H2CHR (8, 1, CATH(IPOINT), CHTMP)
            CALL H2CHR (8, 1, CATH2(IPOINT), CHTMP1)
            IF (CHTMP.NE.CHTMP1) GO TO 45
            IF (ABS(CATR(KRCIC+J)-CATR2(KRCIC+J)).GT.X) GO TO 45
            IF (ABS(CATR(KRCRT+J)-CATR2(KRCRT+J)).GT.1.) GO TO 45
            IM = 3
            CALL H2CHR (8, 1, CATH3(IPOINT), CHTMP1)
            IF (CHTMP.NE.CHTMP1) GO TO 45
            IF (ABS(CATR(KRCIC+J)-CATR3(KRCIC+J)).GT.X) GO TO 45
            IF (ABS(CATR(KRCRT+J)-CATR3(KRCRT+J)).GT.1.) GO TO 45
 40         CONTINUE
         END IF
C                                       Corners reduced a little
      IF (REDUCE) THEN
         MSGTXT = 'Input images coincident on reduced subimage only'
         CALL MSGWRT (4)
         END IF
      GO TO 55
C                                       Maps not coincident
 45   MSGTXT = 'INPUT IMAGES ARE NOT COINCIDENT.'
      CALL MSGWRT (7)
      WRITE (MSGTXT,1045) IM, I
      CALL MSGWRT (7)
      GO TO 999
C                                       pix ranges
 55   CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), PRANGE(1,1))
      CALL RNGSET (DPARM, CATR2(KRDMX), CATR2(KRDMN), PRANGE(1,2))
      CALL RNGSET (DPARM(3), CATR3(KRDMX), CATR3(KRDMN), PRANGE(1,3))
C                                       open the TV
      IRET = 8
      SUBR = 'TVOPEN'
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       init TV
      DO3CHN = (TVIMPC.GT.0) .AND. (DPARM(8).LE.0.0) .AND. (NGRAY.GE.3)
      DOOPT = (.NOT.DO3CHN) .AND. (NGRAY.GE.2)
      IF ((TVCH.LT.1) .OR. (TVCH.GT.NGRAY)) TVCH = 1
      IF ((DO3CHN) .AND. (TVCH+2.GT.NGRAY)) TVCH = NGRAY - 2
      IF ((DOOPT) .AND. (TVCH+1.GT.NGRAY)) TVCH = NGRAY - 1
      CALL COPY (4, WINDTV, LWINTV)
      SVGAMA = TVGAMA
      CALL YHOLD ('ONNN', IERR)
      SUBR = 'YCINIT'
      CALL YCINIT (TVCH, SCRTCH)
      IF (IERR.NE.0) GO TO 990
      IF ((DO3CHN) .OR. (DOOPT)) THEN
         CALL YCINIT (TVCH+1, SCRTCH)
         IF (IERR.NE.0) GO TO 990
         IF (DO3CHN) THEN
            CALL YCINIT (TVCH+2, SCRTCH)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      SUBR = 'YSLECT'
      II = NGRAY + NGRAPH
      DO 60 I = 1,II
         IF ((I.EQ.TVCH) .AND. (.NOT.DO3CHN)) THEN
            CALL YSLECT ('ONNN', I, 0, SCRTCH, IERR)
         ELSE
            CALL YSLECT ('OFFF', I, 0, SCRTCH, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
 60   CONTINUE
      SUBR = 'YZERO'
      CALL YZERO (TVCH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((DO3CHN) .OR. (DOOPT)) THEN
         CALL YZERO (TVCH+1, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (DO3CHN) THEN
         CALL YZERO (TVCH+2, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      TVCODE = 2 ** (TVCH - 1)
      IF (DO3CHN) TVCODE = TVCODE + 2 ** TVCH + 2 ** (TVCH + 1)
      IF (DOOPT) TVCODE = TVCODE + 2 ** TVCH
      CALL YSCROL (TVCODE, 0, 0, T, IERR)
      SUBR = 'YSCROL'
      IF (IERR.NE.0) GO TO 990
C                                       Init OFM
      II = OFMINP + 1
      CALL RFILL (II, 0.0, BUFF1)
      II = LUTOUT + 1
      OFFS = 1.0 / REAL(OFMOUT)
      SLOPE = OFFS * (OFMOUT + 1.0) / REAL (II)
      DO 65 I = 2,II
         BUFF1(I) = I * SLOPE - OFFS
 65      CONTINUE
      SUBR = 'YOFM'
      CALL YOFM ('WRIT', 7, T, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Number I and H levels
      MAXC = MAXINT
      IF (DPARM(5).LT.1.0001) DPARM(5) = 1.5
      IF (DPARM(5).GT.2.5001) DPARM(5) = 1.5
      IF (DOSAT) THEN
         NLS = IROUND (DPARM(6))
         NLS = MAX (3, MIN (4, NLS))
         NLI = (-1.0 + SQRT (1 + 4.*MAXC*(NLS-1.)/DPARM(5))) /
     *      (2. * (NLS-1.) / DPARM(5))
         NLH = (MAXC - NLI) / (NLI * (NLS - 1))
      ELSE
         NLI = SQRT (MAXC * DPARM(5))
         NLH = MAXC / NLI
         NLS = 0
         END IF
C                                       3-channel LUTs
      IF (DO3CHN) THEN
         II = MAXINT + 1
         SLOPE = (LUTOUT + 1.0) / (MAXINT+1.0)
         DO 70 I = 1,II
            SCRTCH(I+1) = I * SLOPE - 0.5
 70         CONTINUE
         SCRTCH(1) = 0
         CALL YLUT ('WRIT', TVCODE, 7, T, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, '3-CHANNEL'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       turn on in colors
         SUBR = 'YSLECT'
         DO 75 I = 1,3
            II = TVCH + I - 1
            CALL YSLECT ('ONNN', II, I, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
 75         CONTINUE
C                                       1-channel LUTs
      ELSE
C                                       Build I array
         IX = MAXC + 1
         CALL RFILL (IX, FBLANK, DI)
         CALL RFILL (IX, FBLANK, DH)
         CALL RFILL (IX, FBLANK, DS)
         DO 80 I = 1,NLI
            DI(I) = (I - 1.0) / (NLI - 1.0)
 80         CONTINUE
         J = 1
         II = NLH - 1
         IF (DOSAT) II = NLH * (NLS - 1)
         DO 85 I = 1,II
            J = J + NLI
            CALL RCOPY (NLI, DI(1), DI(J))
 85         CONTINUE
C                                       Build H array
         JJ = 0
         IF (DOSAT) THEN
            CALL RFILL (NLI, 1.0, DH(1))
            JJ = JJ + NLI
            END IF
         DO 95 I = 1,NLH
            JJ = JJ + 1
            DH(JJ) = (I - 1.0) / (NLH - 1.0)
            DO 90 J = 2,NLI
               JJ = JJ + 1
               DH(JJ) = DH(JJ-1)
 90            CONTINUE
 95         CONTINUE
         IF (DOSAT) THEN
            JJ = NLI + 1
            II = JJ
            IX = NLH * NLI
            DO 100 I = 3,NLS
               II = II + IX
               CALL RCOPY (IX, DH(JJ), DH(II))
 100           CONTINUE
C                                       Build S array
            CALL RFILL (NLI, 0.0, DS)
            JJ = NLI
            II = NLI * NLH
            DO 110 I = 2,NLS
               JJ = JJ + 1
               DS(JJ) = (I - 1.0) / (NLS - 1.0)
               DO 105 J = 2,II
                  JJ = JJ + 1
                  DS(JJ) = DS(JJ-1)
 105              CONTINUE
 110           CONTINUE
            END IF
C                                       get RGB
         JJ = NLI * NLH
         IF (DOSAT) JJ = NLI + (NLS-1) * NLI * NLH
         IF (OPTYPE.EQ.'LUT ') THEN
            CALL HILRGB (FIRST, T, DOSAT, JJ, DI, DH, DS, DOCIRC,
     *         RED, GREEN, BLUE)
         ELSE
            CALL HI2RGB (T, DOSAT, JJ, DI, DH, DS, DOCIRC, RED,
     *         GREEN, BLUE)
            END IF
C                                       Blue lut
         IX = MAXINT + 1
         CALL FILL (IX, 0, SCRTCH)
         DO 115 I = 1,JJ
            SCRTCH(I+1) = BLUE(I) * (LUTOUT - 1) + 1.5
 115        CONTINUE
         SCRTCH(1) = 0
         CALL YLUT ('WRIT', TVCODE, 1, T, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'BLUE'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Green lut
         CALL FILL (IX, 0, SCRTCH)
         DO 120 I = 1,JJ
            SCRTCH(I+1) = GREEN(I) * (LUTOUT - 1) + 1.5
 120        CONTINUE
         SCRTCH(1) = 0
         CALL YLUT ('WRIT', TVCODE, 2, T, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'GREEN'
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Red lut
         CALL FILL (IX, 0, SCRTCH)
         DO 125 I = 1,JJ
            SCRTCH(I+1) = RED(I) * (LUTOUT - 1) + 1.5
 125        CONTINUE
         SCRTCH(1) = 0
         CALL YLUT ('WRIT', TVCODE, 4, T, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'RED'
            CALL MSGWRT (6)
            GO TO 999
           END IF
         END IF
C                                       step wedge size
      IXWDGE = 0
      IYWDGE = 0
      XINC = MAX (1, IROUND (XXINC))
      YINC = MAX (1, IROUND (XYINC))
      IF (NLH.LE.0) NLH = 1
      IF (((DOWDGE.GT.0.0) .AND. (DOWDGE.LE.1.5)) .OR. (DOWDGE.GT.2.5))
     *   THEN
         IYWDGE = MAX (1.0, (ATRC(2,1)-ABLC(2,1))/(8.*YINC)) + 0.5
         IYWDGE = (1 + (IYWDGE - 1) / NLH) * NLH
         END IF
      IF (DOWDGE.GT.1.5) THEN
         IXWDGE = MAX (1.0, (ATRC(1,1)-ABLC(1,1))/(8.*XINC)) + 0.5
         IXWDGE = (1 + (IXWDGE - 1) / NLH) * NLH
         END IF
C                                       check window further
      NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01 + IXWDGE
      IF ((NX.GT.1.5*MAXXTV(1)) .AND. (XXINC.LT.0.5)) THEN
         XINC = IROUND (REAL (NX) / REAL (MAXXTV(1)))
         IF (IXWDGE.GT.0) IXWDGE = (1 + (IXWDGE - 1) / (XINC * NLH))
     *      * NLH
         NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01 + IXWDGE
         END IF
      IF (NX.GT.MAXXTV(1)) THEN
         ABLC(1,1) = ABLC(1,1) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ABLC(1,2) = ABLC(1,2) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ABLC(1,3) = ABLC(1,3) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ATRC(1,1) = ABLC(1,1) + XINC * (MAXXTV(1) - 1 - IXWDGE)
         ATRC(1,2) = ABLC(1,2) + XINC * (MAXXTV(1) - 1 - IXWDGE)
         ATRC(1,3) = ABLC(1,3) + XINC * (MAXXTV(1) - 1 - IXWDGE)
         NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01 + IXWDGE
         END IF
      NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01 + IYWDGE
      IF ((NY.GT.1.5*MAXXTV(2)) .AND. (XYINC.LT.0.5)) THEN
         YINC = IROUND (REAL (NY) / REAL (MAXXTV(2)))
         IF (IYWDGE.GT.0) IYWDGE = (1 + (IYWDGE - 1) / (YINC * NLH))
     *      * NLH
         NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01 + IYWDGE
         END IF
      IF (NY.GT.MAXXTV(2)) THEN
         ABLC(2,1) = ABLC(2,1) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ABLC(2,2) = ABLC(2,2) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ABLC(2,3) = ABLC(2,3) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ATRC(2,1) = ABLC(2,1) + YINC * (MAXXTV(2) - 1 - IYWDGE)
         ATRC(2,2) = ABLC(2,2) + YINC * (MAXXTV(2) - 1 - IYWDGE)
         ATRC(2,3) = ABLC(2,3) + YINC * (MAXXTV(2) - 1 - IYWDGE)
         NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01 + IYWDGE
         END IF
C                                       first subarray in core
      IF (DPARM(7).GT.MAXP) DPARM(7) = MAXP
      IF (DPARM(7).LT.MAXP/100.0) DPARM(7) = MAXP
      MAXP = DPARM(7)
      IDX = 1
      IDY = 1
 130  INX = (NX - 1) / IDX + 1
      INY = (NY - 1) / IDY + 1
      IF (INX*INY.GT.MAXP) THEN
         IDX = IDX + 1
         IDY = IDY + 1
         GO TO 130
         END IF
      IDWIN(1) = 1
      IDWIN(2) = 1
      IDWIN(3) = NX
      IDWIN(4) = NY
C                                       fix up image catalog somehow
      CALL COPY (256, CATBLK, CATBL4)
      CATBLK(IIVOL) = VOL1
      CATBLK(IICNO) = SLOT1
      CATBL2(IIVOL) = VOL2
      CATBL2(IICNO) = SLOT2
      CATBL3(IIVOL) = VOL3
      CATBL3(IICNO) = SLOT3
      CATR(IRRAN) = PRANGE(1,1)
      CATR(IRRAN+1) = PRANGE(2,1)
      CATR2(IRRAN) = PRANGE(1,2)
      CATR2(IRRAN+1) = PRANGE(2,2)
      CATR3(IRRAN) = PRANGE(1,3)
      CATR3(IRRAN+1) = PRANGE(2,3)
      CALL CHR2H (2, 'HI', 1, CATH(IITRA))
      CALL CHR2H (2, 'HI', 1, CATH2(IITRA))
      CALL CHR2H (2, 'HI', 1, CATH3(IITRA))
      DO 140 I = 1,2
         CATBLK(IIWIN+I-1) = IROUND (ABLC(I,1))
         CATBL2(IIWIN+I-1) = IROUND (ABLC(I,2))
         CATBL3(IIWIN+I-1) = IROUND (ABLC(I,3))
         CATBLK(IIWIN+I+1) = IROUND (ATRC(I,1))
         CATBL2(IIWIN+I+1) = IROUND (ATRC(I,2))
         CATBL3(IIWIN+I+1) = IROUND (ATRC(I,3))
 140     CONTINUE
      DO 145 I = 3,7
         CATBLK(IIDEP+I-3) = IROUND (ABLC(I,1))
         CATBL2(IIDEP+I-3) = IROUND (ABLC(I,2))
         CATBL3(IIDEP+I-3) = IROUND (ABLC(I,3))
 145     CONTINUE
C                                       location on TV
      CALL COPY (4, WINDTV, LWIN)
C                                       check zoom
      MX = CSIZTV(1) * (1 + MAXCHR)
      MY = CSIZTV(2) * 2 * (1 + NOPT)
      MX = MAX (MX, NX)
      MY = MAX (MY, NY)
 150  MAGF = TVZOOM(1) + 1
      IF (MXZOOM.GT.0) MAGF = 2 ** TVZOOM(1)
      IF (MAGF.GT.1) THEN
         IF ((MAGF*MX.GE.MAXXTV(1)) .OR. (MAGF*MY.GE.MAXXTV(2))) THEN
            TVZOOM(1) = TVZOOM(1) - 1
            TVZOOM(2) = MAXXTV(1) / 2
            TVZOOM(3) = MAXXTV(2) / 2
            CALL YZOOMC (TVZOOM(1), TVZOOM(2), TVZOOM(3), T, IERR)
            SUBR = 'YZOOMC'
            IF (IERR.NE.0) GO TO 990
            GO TO 150
            END IF
         END IF
C                                       adjust pixels for zoom
      IF (MAGF.GT.1) THEN
         X = MAGF
         II = (TVZOOM(2) - 1.0) * (X - 1.0) / X + 1.0
         IX = WINDTV(1) - (MAGF - 1) / 2
         LWIN(1) = (IX - 1) / X + II + 0.99
         IX = WINDTV(3) - (MAGF - 1) / 2
         LWIN(3) = (IX - 1) / X + II + 0.01
         II = (MAXXTV(2) - TVZOOM(3)) * (X - 1.0) / X
         OFFS = MAXXTV(2) - II - MAXXTV(2) / X + 1.0
         IY = WINDTV(2) - (MAGF - 1) / 2
         LWIN(2) = (IY - 1.0) / X + OFFS + 0.99
         IY = WINDTV(4) - (MAGF - 1) / 2
         LWIN(4) = (IY - 1.0) / X + OFFS + 0.01
         END IF
      IX = LWIN(1) + 10
      IF (IX+INX-1.GT.LWIN(3)) IX = IX - (IX + INX - 1 - LWIN(3)) / 2
      IF (IX+INX-1.GT.MAXXTV(1)) IX = MAXXTV(1) - INX + 1
      IY = LWIN(2) + 10
      IF (IY+INY-1.GT.LWIN(4)) IY = IY - (IY + INY - 1 - LWIN(4)) / 2
      IF (IY+INY-1.GT.MAXXTV(2)) IY = MAXXTV(2) - INY + 1
      CATBL2(IICOR) = IX
      CATBL2(IICOR+1) = IY
      CATBL2(IICOR+2) = IX + INX - 1 - IXWDGE / IDX
      CATBL2(IICOR+3) = IY + INY - 1 - IYWDGE / IDY
      CATBL3(IICOR) = IX
      CATBL3(IICOR+1) = IY
      CATBL3(IICOR+2) = IX + INX - 1 - IXWDGE / IDX
      CATBL3(IICOR+3) = IY + INY - 1 - IYWDGE / IDY
      IX = (LWIN(1) + LWIN(3) - NX) / 2
      IX = MAX (1, IX)
      IF (IX+NX-1.GT.MAXXTV(1)) IX = MAXXTV(1) - NX + 1
      IY = (LWIN(2) + LWIN(4) - NY) / 2
      IY = MAX (1, IY)
      IF (IY+NY-1.GT.MAXXTV(2)) IY = MAXXTV(2) - NY + 1
      CATBLK(IICOR) = IX
      CATBLK(IICOR+1) = IY
      CATBLK(IICOR+2) = IX + NX - 1 - IXWDGE
      CATBLK(IICOR+3) = IY + NY - 1 - IYWDGE
      SUBR = 'YCWRIT'
      CALL YCWRIT (TVCH, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF ((DO3CHN) .OR. (DOOPT)) THEN
         CALL YCWRIT (TVCH+1, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (DO3CHN) THEN
         CALL YCWRIT (TVCH+2, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
C                                       create output image
      IF (DOOUT.GT.0.0) THEN
C                                       header into common, fix subimage
         CALL COPY (256, CATBLK, BUFF1)
         CALL COPY (256, CATBL4, CATBLK)
         CALL SUBHDR (OBLC(1,1), OTRC(1,1), 1.0, 1.0)
C                                       Insert RGB axis
         DO 160 I = 1,4
            J = 7 - I
            CATBLK(KINAX+J) = CATBLK(KINAX+J-1)
            CATD(KDCRV+J) = CATD(KDCRV+J-1)
            CATR(KRCIC+J) = CATR(KRCIC+J-1)
            CATR(KRCRP+J) = CATR(KRCRP+J-1)
            CATR(KRCRT+J) = CATR(KRCRT+J-1)
            CATH(KHCTP+2*J+1) = CATH(KHCTP+2*J-1)
            CATH(KHCTP+2*J) = CATH(KHCTP+2*J-2)
 160        CONTINUE
         CATBLK(KINAX+2) = 3
         CATD(KDCRV+2) = 1.0D0
         CATR(KRCIC+2) = 1.0
         CATR(KRCRP+2) = 1.0
         CATR(KRCRT+2) = 0.0
         CALL CHR2H (8, 'RGB', 1, CATH(KHCTP+4))
         CATBLK(KIDIM) = MIN (7, CATBLK(KIDIM)+1)
C                                       Build new file cat name.
         SEQO = IROUND (XOUTS)
         VOLO = IROUND (XOUTD)
         CALL MAKOUT (NAMIN, CLSIN, SEQ1, ' ', NAMOUT, CLSOUT, SEQO)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLSOUT, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMS) = SEQO
C                                       Create new cataloged file.
         CALL MCREAT (VOLO, SLOTO, SCRTCH, IERR)
         SUBR = 'MCREAT'
         IF (IERR.NE.0) GO TO 990
         SEQO = CATBLK(KIIMS)
         NCFILE = NCFILE + 1
         FCNO(NCFILE) = SLOTO
         FVOL(NCFILE) = VOLO
         FRW(NCFILE) = 2
         CALL COPY (256, CATBLK, CATBL4)
         SEQO2 = IROUND (XOUTS2)
         VOLO2 = IROUND (XOUTD2)
         CALL COPY (256, BUFF1, CATBLK)
         END IF
      IRET = 0
C
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TVHIIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1045 FORMAT ('ERROR APPEARS TO BE ON IMAGE',I2,' AXIS',I2)
 1100 FORMAT ('ERROR',I5,' WRITING THE ',A,' LUT')
 1990 FORMAT ('TVHIIN: ERROR',I5,' RETURNED FROM ROUTINE ',A)
      END
      SUBROUTINE HI2RGB (CLIPI, DOSAT, NX, INT, HUE, SAT, DOCIRC, RED,
     *   GREEN, BLUE)
C-----------------------------------------------------------------------
C   scales intensity and hue and returns the corresponding red, green,
C   and blues.  Algorithm from Gonzales and Woods with S = 1
C   For HUE(i) <= 0.333: h = HUE(i) * 360 degrees
C                        b = (1 - S) / 3
C                        r = (1 + (S * cos(h)) / cos(60-h)) / 3
C                        g = 1 - r - b = 1 - r
C   Else for HUE(i) <= 0.667: h = HUE(i) * 360  - 120 degrees
C                        r = (1 - S) / 3
C                        g = (1 + (S * cos(h)) / cos(60-h)) / 3
C                        b = 1 - r - g = 1 - g
C   Else for HUE(i) > 0.667: h = HUE(i) * 360 - 240 degrees
C                        g = (1 - S) / 3
C                        b = (1 + (S * cos(h)) / cos(60-h)) / 3
C                        r = 1 - g - b = 1 - b
C   Then -> image row:   nb = I * b * (NLB - 1)
C                        ng = I * g * (NLG - 1)
C                        nr = I * r * (NLR - 1)
C                        img = (NLG * nr + ng) * NLB + nb + 1
C   Inputs:
C      CLIPI    L       Limit I to 0-1 or not
C      DOSAT    L       Do we include saturation image (T) or
C                       set S = 1 (F)
C      NX       I       Number points in row
C      INT      R(NX)   Intensity row (0 - 1)
C      HUE      R(NX)   Hue row (0 - 1)
C      SAT      R(NX)   Saturation row (0 - 1)
C      DOCIRC   R       > 0 => do all 360 degrees of color, else 240
C   Output:
C      RED      R(NX)   Red colors
C      GREEN    R(NX)   Green colors
C      BLUE     R(NX)   Blue colors
C-----------------------------------------------------------------------
      LOGICAL   CLIPI, DOSAT
      INTEGER   NX
      REAL      INT(*), HUE(*), SAT(*), DOCIRC, RED(*), BLUE(*),
     *   GREEN(*)
C
      INTEGER   I
      REAL      H, X, S, HM, XR, XG, XB, RADEG
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      RADEG = 3.14159 / 180.0
      IF (DOCIRC.GT.0.0) THEN
         HM = 360.0
      ELSE
         HM = 240.0
         END IF
      DO 100 I = 1,NX
         IF ((INT(I).EQ.FBLANK) .OR. (HUE(I).EQ.FBLANK)) THEN
            RED(I) = 0
            GREEN(I) = 0
            BLUE(I) = 0
         ELSE
            X = INT(I)
            IF (CLIPI) X = MAX (0.0, MIN (1.0, X))
            H = HUE(I)
            H = MAX (0.0, MIN (1.0, H)) * HM
            S = 1.0
            IF (DOSAT) S = MAX (0.0, MIN (1.0, SAT(I)))
            IF (H.LE.120.0) THEN
               XR = (1.0 - S) / 3.0
               XB = (1. + S * COS(RADEG*H) / COS((60-H)*RADEG)) / 3.0
               XG = 1. - XB - XR
            ELSE IF (H.LE.240.0) THEN
               H = H - 120.
               XB = (1.0 - S) / 3.0
               XG = (1. + S * COS(RADEG*H) / COS((60-H)*RADEG)) / 3.0
               XR = 1. - XG - XB
            ELSE
               H = H - 240.
               XG = (1.0 - S) / 3.0
               XR = (1. + S * COS(RADEG*H) / COS((60-H)*RADEG)) / 3.0
               XB = 1. - XR - XG
               END IF
            RED(I) = X * XR
            GREEN(I) = X * XG
            BLUE(I) = X * XB
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE HILRGB (FIRST, CLIPI, DOSAT, NX, INT, HUE, SAT, DOCIRC,
     *   RED, GREEN, BLUE)
C-----------------------------------------------------------------------
C   HILRGB implements the Torson/Rots (verb TVHUEINT) algorithm for
C   converting between hue-intensity and RGB.
C   Inputs:
C      FIRST    L       Set the tables?
C      CLIPI    L       Clip I between 0,1 else don't if possible
C      DOSAT    L       Include saturation image
C      NX       I       Number points in row
C      INT      R(NX)   Intensity row (0 - 1) or (?? - ??)
C      HUE      R(NX)   Hue row (0 - 1)
C      SAT      R(NX)   Saturation row (0 - 1)
C      DOCIRC   R       > 0 => do all 360 degrees of color, else 240
C   In/out:
C      FIRST    L       T => prepare tables
C   Output:
C      RED      R(NX)   Red colors
C      GREEN    R(NX)   Green colors
C      BLUE     R(NX)   Blue colors
C-----------------------------------------------------------------------
      LOGICAL   FIRST, CLIPI, DOSAT
      INTEGER   NX
      REAL      INT(*), HUE(*), SAT(*), DOCIRC, RED(*), BLUE(*),
     *   GREEN(*)
C
      INTEGER   I, JJ, IX, IX0, LUTR(1024), LUTG(1024), LUTB(1024)
      REAL      X, S, F
C                                       Above def'n of F is NOT same
C                                       as that in LOCAL INCLUDE.
C                                       Be careful...
      INCLUDE 'INCS:DDCH.INC'
      COMMON /LUTS/ LUTR, LUTG, LUTB
C-----------------------------------------------------------------------
C                                       init color filters
      IF (FIRST) THEN
         FIRST = .FALSE.
C                                       Red, non-circular filter
         JJ = 1023
         IF (DOCIRC.LE.0.0) THEN
            IX0 = 0.52 * JJ
            IX = 0.80 * JJ
            CALL STRLIN (0, 0, IX0, 0, LUTR)
            CALL STRLIN (IX0, 0, IX, JJ, LUTR)
            CALL STRLIN (IX, JJ, JJ, JJ, LUTR)
C                                       Green table
            IX0 = 0.06 * JJ
            CALL STRLIN (0, 0, IX0, 0, LUTG)
            IX = 0.37 * JJ
            CALL STRLIN (IX0, 0, IX, JJ, LUTG)
            IX0 = 0.63 * JJ
            CALL STRLIN (IX, JJ, IX0, JJ, LUTG)
            IX = 0.94 * JJ
            CALL STRLIN (IX0, JJ, IX, 0, LUTG)
            CALL STRLIN (IX, 0, JJ, 0, LUTG)
C                                       blue table
            IX0 = 0.20 * JJ
            IX = 0.48 * JJ
            CALL STRLIN (0, JJ, IX0, JJ, LUTB)
            CALL STRLIN (IX0, JJ, IX, 0, LUTB)
            CALL STRLIN (IX, 0, JJ, 0, LUTB)
C                                       Circular: red table
         ELSE
            IX0 = 0.35 * JJ
            IX = 0.48 * JJ
            CALL STRLIN (0, 0, IX0, 0, LUTR)
            CALL STRLIN (IX0, 0, IX, JJ, LUTR)
            IX0 = 0.85 * JJ
            CALL STRLIN (IX, JJ, IX0, JJ, LUTR)
            IX = .98 * JJ
            CALL STRLIN (IX0, JJ, IX, 0, LUTR)
C                                       Green table
            IX0 = 0.02 * JJ
            IX = 0.15 * JJ
            CALL STRLIN (0, 0, IX0, 0, LUTG)
            CALL STRLIN (IX0, 0, IX, JJ, LUTG)
            IX0 = 0.51 * JJ
            CALL STRLIN (IX, JJ, IX0, JJ, LUTG)
            IX = 0.65 * JJ
            CALL STRLIN (IX0, JJ, IX, 0, LUTG)
            CALL STRLIN (IX, 0, JJ, 0, LUTG)
C                                       Blue table
            IX0 = 0.18 * JJ
            IX = 0.32 * JJ
            CALL STRLIN (0, JJ, IX0, JJ, LUTB)
            CALL STRLIN (IX0, JJ, IX, 0, LUTB)
            IX0 = 0.67 * JJ
            CALL STRLIN (IX, 0, IX0, 0, LUTB)
            IX = 0.82 * JJ
            CALL STRLIN (IX0, 0, IX, JJ, LUTB)
            CALL STRLIN (IX, JJ, JJ, JJ, LUTB)
            END IF
         END IF
C                                       Now convert them all
      DO 100 I = 1,NX
         IF ((INT(I).EQ.FBLANK) .OR. (HUE(I).EQ.FBLANK) .OR.
     *      ((DOSAT) .AND. (SAT(I).EQ.FBLANK))) THEN
            RED(I) = 0
            GREEN(I) = 0
            BLUE(I) = 0
         ELSE
            JJ = MAX (0.0, MIN (1.0, HUE(I))) * 1022.0 + 1.5
            X = INT(I)
            IF (CLIPI) X = MAX (0.0, MIN (1.0, INT(I)))
            X = X
            S = 1.0
            IF (DOSAT) S = MAX (0.0, MIN (1.0, SAT(I)))
            F = X * (1.0 - S)
            X = X * S / 1023.0
            BLUE(I) = X * LUTB(JJ) + F
            GREEN(I) = X * LUTG(JJ) + F
            RED(I) = X * LUTR(JJ) + F
            END IF
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TVHINT (MAXP, INTENS, HUE, SATUR, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TVHINT implements the interactive selection and execution of the
C   various display and interaction options of TVHUI.
C   Inputs:
C      MAXP     I         dimensions of data subarrays
C   Outputs:
C      INTENS   R(MAXP)   Intensity subarray in core
C      HUE      R(MAXP)   Hue subarray in core
C      SATUR    R(MAXP)   Saturation subarray in core
C      SCRTCH   I(2050)   TV scratch
C      IRET     I         error return
C-----------------------------------------------------------------------
      INTEGER   MAXP, SCRTCH(*), IRET
      REAL      INTENS(MAXP), HUE(MAXP), SATUR(MAXP)
C
      INCLUDE 'PTVHUI.INC'
C
      INTEGER   FLIP(3), NOPTS, MAGF, LWIN(4), I, J, II, JJ, IX, IY,
     *   IERR, IBUT, GRCS(2), CHS, NTITLE, TOPSEP, SIDSEP, TIMLIM
      LOGICAL   MENUOK, WINDOK, DOIT, SUBIMG, TVCHON, LEAVE(NOPT),
     *   LEAVES(NOPT)
      REAL      X, Y, SCL, OFFS, EPOS(2,3), ARANGE(2,3), NDSC(2,3),
     *   ODSC(2,3)
      CHARACTER OPTION(NOPT)*20, SUBR*6, OFFCUR*20, UNITS*8, ONCUR*20,
     *   ONLOG(4)*20, ISHELP*8, CHOICS(NOPT)*20, TITLE*8, HULOG(4)*20
      INCLUDE 'TVHUI.INC'
      DATA OPTION /'ENHANCE INTENSITY', 'ENHANCE HUE',
     *   'ENHANCE SATURATION', 'USE LOG(INTENSITY)', 'USE LOG(HUE)',
     *   'DISPLAY CURSOR X,Y', 'SET WINDOW',
     *   'REDRAW FULL PICTURE', 'OPTIMIZE PICTURE', 'EXIT', 'ABORT'/
      DATA OFFCUR /'TURN OFF CURSOR X,Y'/
      DATA ONCUR /'DISPLAY CURSOR X,Y'/
      DATA ONLOG /'USE LOG(INTENSITY)','USE ELOG(INTENSITY)',
     *   'USE SQRT(INTENSITY)', 'USE LIN(INTENSITY)'/
      DATA HULOG /'USE LOG(HUE)','USE ELOG(HUE)',
     *   'USE SQRT(HUE)', 'USE LIN(HUE)'/
      DATA LEAVE /3*.FALSE., 3*.TRUE., .FALSE., 2*.TRUE., 2*.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize
      WINDOK = F
      MENUOK = F
      SUBIMG = F
      TVCHON = T
      DOGRC = F
      DOLOGI = 0
      DOLOGH = 0
      GRMENU = 1 + NGRAY
      GRCURS = MIN (2, NGRAPH) + NGRAY
      GRWIND = MIN (3, NGRAPH) + NGRAY
      SC(1,1) = 1.0
      SC(1,2) = 1.0
      SC(1,3) = 1.0
      SC(2,1) = 0.0
      SC(2,2) = 0.0
      SC(2,3) = 0.0
      FLIP(1) = 1
      FLIP(2) = 1
      FLIP(3) = 1
      CALL RFILL (6, 0.0, EPOS)
      CALL RFILL (6, 0.0, NDSC)
      CALL RFILL (6, 0.0, ODSC)
      NOPTS = 0
      DO 10 I = 1,NOPT
         IF ((I.NE.3) .OR. (DOSAT)) THEN
            NOPTS = NOPTS + 1
            CHOICS(NOPTS) = OPTION(I)
            LEAVES(NOPTS) = LEAVE(I)
            END IF
 10      CONTINUE
C                                       start by forcing a load
      CHS = NOPTS - 3
      GO TO 125
C                                       Menu interaction loop point
C                                       check window
 100  CALL YWINDO ('READ', WINDTV, IERR)
      SUBR = 'YWINDO'
      IF (IERR.NE.0) GO TO 990
      IF (WINDTV(1).NE.LWINTV(1)) MENUOK = F
      IF (WINDTV(2).NE.LWINTV(2)) MENUOK = F
      IF (WINDTV(3).NE.LWINTV(3)) MENUOK = F
      IF (WINDTV(4).NE.LWINTV(4)) MENUOK = F
      CALL COPY (4, WINDTV, LWINTV(1))
C                                       location on TV
      IF (.NOT.MENUOK) THEN
         CALL COPY (4, WINDTV, LWIN)
         MAGF = TVZOOM(1) + 1
         IF (MXZOOM.GT.0) MAGF = 2 ** TVZOOM(1)
C                                       adjust pixels for zoom
         IF (MAGF.GT.1) THEN
            X = MAGF
            II = (TVZOOM(2) - 1.0) * (X - 1.0) / X + 1.0
            IX = WINDTV(1) - (MAGF - 1) / 2
            LWIN(1) = (IX - 1) / X + II + 0.99
            IX = WINDTV(3) - (MAGF - 1) / 2
            LWIN(3) = (IX - 1) / X + II + 0.01
            II = (MAXXTV(2) - TVZOOM(3)) * (X - 1.0) / X
            OFFS = MAXXTV(2) - II - MAXXTV(2) / X + 1.0
            IY = WINDTV(2) - (MAGF - 1) / 2
            LWIN(2) = (IY - 1.0) / X + OFFS + 0.99
            IY = WINDTV(4) - (MAGF - 1) / 2
            LWIN(4) = (IY - 1.0) / X + OFFS + 0.01
            END IF
         IX = LWIN(1) + CSIZTV(1)
         IY = LWIN(4) - CSIZTV(2) * 2
         GRX0 = IX
         GRY0 = IY
         IF (GRCURS.NE.GRMENU) THEN
            SUBR = 'YZERO'
            CALL YZERO (GRCURS, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       turn on menu
      GRCS(1) = GRMENU - NGRAY
      IF (MENUOK) GRCS(1) = -GRCS(1)
      GRCS(2) = MIN (NGRAPH, 4)
      ISHELP = TSKNAM
      TOPSEP = 3 * CSIZTV(2) + 1
      TITLE = ' '
      NTITLE = 0
      TIMLIM = 0
      SIDSEP = 5
      CALL TVMENU (0, 1, NOPTS, GRCS, TOPSEP, SIDSEP, ISHELP, CHOICS,
     *   TIMLIM, LEAVES, NTITLE, TITLE, CHS, IBUT, SCRTCH, IERR)
      SUBR = 'TVMENU'
      IF (IERR.NE.0) GO TO 990
      MENUOK = T
C                                       Something to do
 125  CALL YHOLD ('ONNN', IERR)
C                                       Enhance subimage
      IF (CHOICS(CHS)(:8).EQ.'ENHANCE ') THEN
         IF (.NOT.TVCHON) THEN
            SUBR = 'YSLECT'
            CALL YSLECT ('OFFF', TVCH+1, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            TVCHON = T
            END IF
         JJ = 0
         IF (CHOICS(CHS).EQ.'ENHANCE INTENSITY') JJ = 1
         IF (CHOICS(CHS).EQ.'ENHANCE HUE') JJ = 2
         IF (CHOICS(CHS).EQ.'ENHANCE SATURATION') JJ = 3
         IF (JJ.GT.0) THEN
            CALL TVHISI (JJ, INX, INY, INTENS, HUE, SATUR, FLIP(JJ),
     *         EPOS(1,JJ), SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 999
            SUBIMG = T
            IF ((GRCURS.EQ.GRMENU) .AND. (DOGRC)) MENUOK = F
            END IF
C                                       reset cursor display option
      ELSE IF ((CHOICS(CHS).EQ.OFFCUR) .OR. (CHOICS(CHS).EQ.ONCUR)) THEN
         DOGRC = .NOT.DOGRC
         MENUOK = F
         SUBR = 'YSLECT'
         IF (DOGRC) THEN
            CHOICS(CHS) = OFFCUR
            CALL YSLECT ('ONNN', GRCURS, 0, SCRTCH, IERR)
         ELSE
            CHOICS(CHS) = ONCUR
            CALL YSLECT ('OFFF', GRCURS, 0, SCRTCH, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
C                                       abort
      ELSE IF (CHOICS(CHS).EQ.'ABORT') THEN
         IRET = 2
C                                       Reload possible
      ELSE
C                                       Change log/linear choice
         IF ((CHOICS(CHS).EQ.ONLOG(1)) .OR. (CHOICS(CHS).EQ.ONLOG(2))
     *      .OR. (CHOICS(CHS).EQ.ONLOG(3)).OR.
     *      (CHOICS(CHS).EQ.ONLOG(4))) THEN
            DOIT = T
            DOLOGI = MOD (DOLOGI+1,4)
            CHOICS(CHS) = ONLOG(DOLOGI+1)
            MENUOK = F
         ELSE IF ((CHOICS(CHS).EQ.HULOG(1)) .OR.
     *      (CHOICS(CHS).EQ.HULOG(2)) .OR. (CHOICS(CHS).EQ.HULOG(3)).OR.
     *      (CHOICS(CHS).EQ.HULOG(4))) THEN
            DOIT = T
            DOLOGH = MOD (DOLOGH+1,4)
            CHOICS(CHS) = HULOG(DOLOGH+1)
            MENUOK = F
C                                       Set window for subimage
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            IF (GRWIND.EQ.GRMENU) WINDOK = F
            CALL TVHIWI (WINDOK, MAXP, DOIT, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 999
            IF (GRWIND.EQ.GRMENU) MENUOK = F
C                                       Has scaling changed?
         ELSE
            IF (CHOICS(CHS).EQ.'REDRAW FULL PICTURE') THEN
               CALL RCOPY (6, NDSC, DSC)
            ELSE
               CALL RCOPY (6, ODSC, DSC)
               END IF
            X = 0
            DO 135 I = 1,2
               DO 130 J = 1,3
                  X = X + ABS (SC(I,J) - DSC(I,J))
 130              CONTINUE
 135           CONTINUE
            DOIT = (X.GT.0.003) .OR. ((CHOICS(CHS).EQ.'EXIT') .AND.
     *         (((TVCHON) .AND. (DOOPT)) .OR. ((SUBIMG) .AND.
     *         (.NOT.DOOPT))))
            END IF
C                                       do load function
         IF (DOIT) THEN
C                                       simple load
            IF ((CHOICS(CHS).EQ.'REDRAW FULL PICTURE') .OR.
     *         (((CHOICS(CHS).EQ.'SET WINDOW') .OR.
     *         (CHOICS(CHS).EQ.ONLOG(1)) .OR. (CHOICS(CHS).EQ.ONLOG(2))
     *         .OR. (CHOICS(CHS).EQ.ONLOG(3))) .AND. (TVCHON)) .OR.
     *         (.NOT.DOOPT)) THEN
               IF (.NOT.TVCHON) THEN
                  SUBR = 'YSLECT'
                  CALL YSLECT ('OFFF', TVCH+1, 0, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 990
                  TVCHON = T
                  END IF
               CALL TVHILD (SUBIMG, INX, INY, INTENS, HUE, SATUR,
     *            SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 999
               CALL RCOPY (6, SC, NDSC)
               SUBIMG = F
C                                       optimal load
            ELSE
               CALL TVHIOP (INX, INY, INTENS, HUE, SATUR, SCRTCH, IRET)
               IF (IRET.NE.0) GO TO 999
               TVCHON = F
               CALL RCOPY (6, SC, ODSC)
               END IF
C                                       turn on/off channels
         ELSE IF ((CHOICS(CHS).EQ.'REDRAW FULL PICTURE') .AND.
     *      (.NOT.TVCHON)) THEN
            SUBR = 'YSLECT'
            CALL YSLECT ('OFFF', TVCH+1, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            TVCHON = T
         ELSE IF ((CHOICS(CHS).EQ.'OPTIMIZE PICTURE') .AND. (TVCHON))
     *      THEN
            SUBR = 'YSLECT'
            CALL YSLECT ('OFFF', TVCH, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YSLECT ('ONNN', TVCH+1, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            TVCHON = F
            END IF
         END IF
      IF ((CHOICS(CHS).NE.'EXIT') .AND. (CHOICS(CHS).NE.'ABORT'))
     *   GO TO 100
         IF ((DOGRC) .AND. (GRCURS.NE.GRMENU)) CALL YSLECT ('OFFF',
     *      GRCURS, 0, SCRTCH, IERR)
         IERR = 0
C
 990  IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (8)
      ELSE
         DO 995 I = 1,3
            X = PRANGE(2,I)
            Y = PRANGE(1,I)
            SCL = SC(1,I)
            OFFS = SC(2,I)
            IF (SCL.EQ.0.0) SCL = 1.0
            ARANGE(1,I) = Y + OFFS * (X - Y) / SCL
            ARANGE(2,I) = Y + (1.0 + OFFS) * (X - Y) / SCL
 995        CONTINUE
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Intensity ', ARANGE(1,1), ARANGE(2,1),
     *      UNITS
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH2(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Hue       ', ARANGE(1,2), ARANGE(2,2),
     *      UNITS
         CALL MSGWRT (5)
         IF (DOSAT) THEN
            CALL H2CHR (8, 1, CATH3(KHBUN), UNITS)
            WRITE (MSGTXT,1995) 'Saturation', ARANGE(1,3), ARANGE(2,3),
     *         UNITS
            CALL MSGWRT (5)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TVHINT: ERROR',I5,' RETURNED BY ROUTINE ',A)
 1995 FORMAT ('Final ',A10,' pixrange =',1PE12.4,',',1PE12.4,1X,A8)
      END
      SUBROUTINE TVHIWI (WINDOK, MAXP, DOIT, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   implements interactive selection of the subimage area and resets the
C   resultant subimage size and increment.
C   Inputs:
C      MAXP     I      Maximum number of pixels in subimage
C   In/out:
C      WINDOK   L      Does it need erasing
C   Outputs:
C      DOIT     L      A new subimage was selected
C      SCRTCH   I(*)   TV scratch buffer
C      IRET     I      Error return
C-----------------------------------------------------------------------
      INTEGER   MAXP, SCRTCH(*), IRET
      LOGICAL   WINDOK, DOIT
C
      INTEGER   IERR, LWIN(4), ITW(3), QUAD, IBUT, IXP(5), IYP(5), IOFF
      REAL      RPOS(2), PPOS(2), R0, RT
      CHARACTER SUBR*6, STRING*16
      INCLUDE 'TVHUI.INC'
C-----------------------------------------------------------------------
      IRET = 0
      DOIT = F
      CALL ZTIME (ITW)
      CALL YHOLD ('ONNN', IERR)
C                                       erase
      IF (.NOT.WINDOK) THEN
         CALL YZERO (GRWIND, IERR)
         SUBR = 'YZERO'
         IF (IERR.NE.0) GO TO 990
         WINDOK = T
         END IF
C                                       turn on
      IF ((GRWIND.NE.GRCURS) .OR. (.NOT.DOGRC)) THEN
         CALL YSLECT ('ONNN', GRWIND, 0, SCRTCH, IERR)
         SUBR = 'YSLECT'
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                       initial window
      CALL COPY (4, IDWIN, LWIN)
      IF ((LWIN(1).LT.1) .OR. (LWIN(1).GE.NX)) LWIN(1) = 1
      IF ((LWIN(2).LT.1) .OR. (LWIN(2).GE.NY)) LWIN(2) = 1
      IF ((LWIN(3).LE.LWIN(1)) .OR. (LWIN(3).GT.NX)) LWIN(3) = NX
      IF ((LWIN(4).LE.LWIN(2)) .OR. (LWIN(4).GT.NY)) LWIN(4) = NY
C                                       cursor on + instruct
      IOFF = 1
      RPOS(1) = LWIN(1) - 1 + CATBLK(IICOR)
      RPOS(2) = LWIN(2) - 1 + CATBLK(IICOR+1)
      PPOS(1) = 0.
      PPOS(2) = 0.
      QUAD = -1
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
      MSGTXT = 'Hit button A or B to switch corners '
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C or D to exit'
      CALL MSGWRT (1)
C                                        erase drawing
 50   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF (DOIT) THEN
C                                       erase old plot
         IF (.NOT.WINDOK) THEN
            CALL IMVECT ('OFFF', GRWIND, 5, IXP, IYP, SCRTCH, IERR)
            SUBR = 'IMVECT'
            IF (IERR.NE.0) GO TO 990
            WINDOK = T
            END IF
C                                       new corner
         LWIN(IOFF) = RPOS(1) + 1.5 - CATBLK(IICOR)
         LWIN(IOFF+1) = RPOS(2) + 1.5 - CATBLK(IICOR+1)
         IF ((LWIN(1).LT.1) .OR. (LWIN(1).GE.NX)) LWIN(1) = 1
         IF ((LWIN(2).LT.1) .OR. (LWIN(2).GE.NY)) LWIN(2) = 1
         IF ((LWIN(3).LE.LWIN(1)) .OR. (LWIN(3).GT.NX)) LWIN(3) = NX
         IF ((LWIN(4).LE.LWIN(2)) .OR. (LWIN(4).GT.NY)) LWIN(4) = NY
         IXP(1) = LWIN(1) - 1 + CATBLK(IICOR)
         IXP(3) = LWIN(3) - 1 + CATBLK(IICOR)
         IYP(1) = LWIN(2) - 1 + CATBLK(IICOR+1)
         IYP(3) = LWIN(4) - 1 + CATBLK(IICOR+1)
         IXP(2) = IXP(3)
         IXP(4) = IXP(1)
         IXP(5) = IXP(1)
         IYP(2) = IYP(1)
         IYP(4) = IYP(3)
         IYP(5) = IYP(1)
         IF (IBUT.LT.4) THEN
            CALL IMVECT ('ONNN', GRWIND, 5, IXP, IYP, SCRTCH, IERR)
            SUBR = 'IMVECT'
            IF (IERR.NE.0) GO TO 990
            WINDOK = F
            END IF
         IF ((IBUT.GE.1) .AND. (IBUT.LT.4)) THEN
            IOFF = MOD (IOFF + 2, 4)
            RPOS(1) = IXP(IOFF)
            RPOS(2) = IYP(IOFF)
            CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
            SUBR = 'YCURSE'
            IF (IERR.NE.0) GO TO 990
            END IF
         IF (DOGRC) THEN
            WRITE (STRING,1050) LWIN(IOFF), LWIN(IOFF+1)
            SUBR = 'IMCHAR'
            CALL IMCHAR (GRCURS, GRX0, GRY0, 0, 0, STRING, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      IF (IBUT.LT.4) GO TO 50
C                                       reset windows
      DOIT = (IDWIN(1).NE.LWIN(1)) .OR. (IDWIN(2).NE.LWIN(2)) .OR.
     *   (IDWIN(3).NE.LWIN(3)) .OR. (IDWIN(4).NE.LWIN(4))
      IF (DOIT) THEN
         CALL COPY (4, LWIN, IDWIN)
         IDX = 1
         IDY = 1
         R0 = 1.5
 90      INX = (LWIN(3) - LWIN(1)) / IDX + 1
         INY = (LWIN(4) - LWIN(2)) / IDY + 1
         IF (INX*INY.GT.MAXP) THEN
            RT = REAL (INX * NY) / REAL (INY * NX)
            IF (RT.GT.R0) THEN
               IDX = IDX + 1
            ELSE IF (RT.LT.1.0/R0) THEN
               IDY = IDY + 1
            ELSE
               IDX = IDX + 1
               IDY = IDY + 1
               END IF
            GO TO 90
            END IF
         END IF
C                                       error
 990  IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (8)
         END IF
C                                       turn of cursor, graphics
      CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, IERR)
      IF ((GRWIND.NE.GRCURS) .OR. (.NOT.DOGRC)) THEN
         CALL YSLECT ('OFFF', GRWIND, 0, SCRTCH, IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('IM X=',I4,' Y=',I4)
 1990 FORMAT ('TVHIWI: ERROR',I5,' RETURNED BY ROUTINE ',A)
      END
      SUBROUTINE TVHISI (LJ, LX, LY, INTENS, HUE, SATUR, FLIP, PPOS,
     *   SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TVHI1S implements single-TV channel enhancement of one of the HSI
C   components or 3-channel.
C   Inputs:
C      LJ       I          Component number (I, H, S = 1,2,3)
C      LX       I          x dimension of in-core arrays
C      LY       I          y dimension of in-core arrays
C      INTENS   R(LX,LY)   in-core intensity array
C      HUE      R(LX,LY)   in-core hue array
C      SATUR    R(LX,LY)   in-core saturation array
C   In/out:
C      FLIP     I          Sign of slope of enhancement
C      PPOS     R(2)       Cursor position last used
C   Outputs:
C      SCRTCH   I(1536)    TV scratch array
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   LJ, FLIP, LX, LY, SCRTCH(*), IRET
      REAL      INTENS(LX,LY), HUE(LX,LY), SATUR(LX,LY), PPOS(2)
C
      INTEGER   I, IERR, IBUT, QUAD, ITW(3), IX, IY, II, JJ
      LOGICAL   DOIT
      REAL      RPOS(2), F0, X, SLOPE, OFFSET, POW, XSCL, POS0(2)
      CHARACTER SUBR*6, STRING*16
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA POW, XSCL /0.9, 1.3/
C     DATA POW, XSCL /1.33, 2.0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       init TV cursor
      CALL ZTIME (ITW)
      F0 = (WINDTV(4) - WINDTV(2)) / 3.0
      POS0(1) = (WINDTV(1) + WINDTV(3)) / 2.0
      POS0(2) = WINDTV(2) + F0
      IF ((PPOS(1).LT.WINDTV(1)) .OR. (PPOS(1).GT.WINDTV(3)) .OR.
     *   (PPOS(2).LT.WINDTV(2)) .OR. (PPOS(2).GT.WINDTV(4))) THEN
         RPOS(1) = POS0(1)
         RPOS(2) = POS0(2)
      ELSE
         RPOS(1) = PPOS(1)
         RPOS(2) = PPOS(2)
         END IF
C                                       turn on cursor
      QUAD = -1
      CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
C                                       instructions
      MSGTXT = 'Hit button A to reverse slope of enhancement'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button B to reset the enhancement to 1,0'
      CALL MSGWRT (1)
      MSGTXT = 'Hit button C or D to return to the menu'
      CALL MSGWRT (1)
      MSGTXT = 'Cursor X controls offset and Y slope of enhancement'
      CALL MSGWRT (1)
      IF (LJ.EQ.1) THEN
         MSGTXT = 'Enhancing the intensity image'
      ELSE IF (LJ.EQ.2) THEN
         MSGTXT = 'Enhancing the hue image'
      ELSE
         MSGTXT = 'Enhancing the saturation image'
         END IF
      CALL MSGWRT (1)
C                                       read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF ((DOIT) .AND. (IBUT.LT.8)) THEN
         IF (IBUT.EQ.1) FLIP = -FLIP
         IF ((IBUT.EQ.2) .OR. (IBUT.EQ.3)) THEN
            RPOS(1) = POS0(1)
            RPOS(2) = POS0(2)
            FLIP = 1
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            SUBR = 'YCURSE'
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       DEBUG
         XSCL = 1.75
         POW = 1.33
         X = (RPOS(1) - WINDTV(1) + 1.0) /
     *      (WINDTV(3) - WINDTV(1) + 1.0) - 0.5
         SLOPE = ((F0 / MAX (1., RPOS(2)-WINDTV(2))) ** POW) * FLIP
         OFFSET = 1.0 / REAL(LUTOUT)
         OFFSET = ((OFFSET + X) * SLOPE + (1 - FLIP) / 2) - OFFSET
C        OFFSET = (OFFSET - X) * ABS(SLOPE) - OFFSET
c          + (1 - FLIP) / 2
         OFFSET = XSCL * X * FLIP * SLOPE
         SC(1,LJ) = SLOPE
         SC(2,LJ) = OFFSET
C                                       subimage: display
         IX = CATBL2(IICOR)
         IY = CATBL2(IICOR+1)
         CALL YHOLD ('ONNN', IERR)
C                                       3-channel display
         SUBR = 'YIMGIO'
         IF (DO3CHN) THEN
            DO 60 I = 1,LY
C                                       protect stored data
               CALL HILOAD (LX, IX, IY, INTENS(1,I), HUE(1,I),
     *            SATUR(1,I), SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 990
               IY = IY + 1
 60            CONTINUE
C                                       1-channel display
         ELSE
            DO 70 I = 1,LY
               CALL HI2ROW (LX, DOLOGI, NLI, NLH, NLS, INTENS(1,I),
     *            HUE(1,I), SATUR(1,I), SC(1,1), SC(1,2), SC(1,3),
     *            SCRTCH)
               CALL YIMGIO ('WRIT', TVCH, IX, IY, 0, LX, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 990
               IY = IY + 1
 70            CONTINUE
            END IF
         IF (DOGRC) THEN
            II = RPOS(1) + 0.5
            JJ = RPOS(2) + 0.5
            WRITE (STRING,1070) II, JJ
            SUBR = 'IMCHAR'
            CALL IMCHAR (GRCURS, GRX0, GRY0, 0, 0, STRING, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         CALL YHOLD ('OFFF', IERR)
         END IF
      IF (IBUT.LT.4) GO TO 50
C                                       error message
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C                                       off cursor
      CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1070 FORMAT ('TV X=',I4,' Y=',I4)
 1990 FORMAT ('TVHISI: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE TVHILD (SUBIMG, LX, LY, INTENS, HUE, SATUR, SCRTCH,
     *   IRET)
C-----------------------------------------------------------------------
C   TVHILD reads in the intensity, hue, and saturation images, puts them
C   on the TV and returns subarrays suitable for interactive loads.
C   Inputs:
C      SUBIMG   L          Is there a subimage picture to erase?
C      LX       I          x dimension of in-core arrays
C      LY       I          y dimension of in-core arrays
C   Outputs:
C      INTENS   R(LX,LY)   in-core intensity array
C      HUE      R(LX,LY)   in-core hue array
C      SATUR    R(LX,LY)   in-core saturation array
C      SCRTCH   I(1536)    TV scratch array
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      LOGICAL   SUBIMG
      INTEGER   LX, LY, SCRTCH(*), IRET
      REAL      INTENS(LX,LY), HUE(LX,LY), SATUR(LX,LY)
C
      INTEGER   I, J, IBLK, HBLK, SBLK, IERR, JJ, JX, JY, BIND1, BIND2,
     *   BIND3, SY, SX, IX, IY, II, LL
      REAL      X, DI(1536), DH(1536), DS(1536)
      CHARACTER SUBR*6
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       erase subimage
      CALL YHOLD ('ONNN', IERR)
      IF (SUBIMG) THEN
         JJ = TVCH
         IF (DO3CHN) JJ = TVCH + 2
         SUBR = 'YZERO'
         DO 15 J = TVCH,JJ
            CALL YZERO (J, IERR)
            IF (IERR.NE.0) GO TO 990
 15         CONTINUE
         END IF
C                                       init image windows
      JX = CATBLK(IIWIN+2) - CATBLK(IIWIN+0) + 1
      JY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), IBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), HBLK,
     *   IERR)
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), SBLK,
     *   IERR)
      IBLK = IBLK + 1
      HBLK = HBLK + 1
      SBLK = SBLK + 1
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1)
      SX = 0
      SY = 0
C                                       init the reads
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, HBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOSAT) THEN
         CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *      CATBL3(IIWIN), BUFF3, JBUFSZ, SBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      SY = 0
      DO 80 J = 1,JY
C                                       read data rows
         SUBR = 'MDISK'
         CALL MDISK ('READ', LUN1, IND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOSAT) THEN
            CALL MDISK ('READ', LUN3, IND3, BUFF3, BIND3, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       scale data rows
            II = 0
            DO 65 I = 1,JX,XINC
               II = II + 1
               X = BUFF1(BIND1+I-1)
               IF (X.EQ.FBLANK) THEN
                  DI(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,1)) / (PRANGE(2,1) - PRANGE(1,1))
                  DI(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               X = BUFF2(BIND2+I-1)
               IF (X.EQ.FBLANK) THEN
                  DH(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,2)) / (PRANGE(2,2) - PRANGE(1,2))
                  DH(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               IF (DOSAT) THEN
                  X = BUFF3(BIND3+I-1)
                  IF (X.EQ.FBLANK) THEN
                     DS(II) = FBLANK
                  ELSE
                     X = (X - PRANGE(1,3)) / (PRANGE(2,3) - PRANGE(1,3))
                     DS(II) = MAX (0.0, MIN (1.0, X))
                     END IF
                  END IF
 65            CONTINUE
C                                       add step wedge to right
            JJ = (J - 1) / YINC + 1
            IF (IXWDGE.GT.1) THEN
               X = REAL (JJ - 1) / REAL ((JY - 1) / YINC)
               IF (DOSAT) THEN
                  CALL RFILL (IXWDGE, X, DS(II+1))
                  X = NLS * X
                  I = X
                  X = X - I
                  END IF
               CALL RFILL (IXWDGE, X, DI(II+1))
               DO 70 I = 1,IXWDGE
                  II = II + 1
                  DH(II) = (I - 1.0) / (IXWDGE - 1.0)
 70               CONTINUE
               END IF
C                                       copy to sub-image
            IF ((JJ.GE.IDWIN(2)) .AND. (JJ.LE.IDWIN(4)) .AND.
     *         (MOD(JJ-1, IDY).EQ.0)) THEN
               SY = SY + 1
               SX = 0
               II = IDWIN(1)
               DO 75 I = II,NX,IDX
                  IF (I.LE.IDWIN(3)) THEN
                     SX = SX + 1
                     INTENS(SX,SY) = DI(I)
                     HUE(SX,SY) = DH(I)
                     IF (DOSAT) SATUR(SX,SY) = DS(I)
                     END IF
 75               CONTINUE
               END IF
            SUBR = 'YIMGIO'
            IF (DO3CHN) THEN
               CALL HILOAD (NX, IX, IY, DI, DH, DS, SCRTCH, IERR)
            ELSE
               CALL HI2ROW (NX, DOLOGI, NLI, NLH, NLS, DI, DH, DS,
     *            SC(1,1), SC(1,2), SC(1,3), SCRTCH)
               CALL YIMGIO ('WRIT', TVCH, IX, IY, 0, NX, SCRTCH, IERR)
               END IF
            IF (IERR.NE.0) GO TO 990
            IY = IY + 1
            END IF
 80      CONTINUE
C                                       top step wedge
      IF (IYWDGE.GT.1) THEN
         II = NX
         IF (DOSAT) II = NX / NLS
         X = 1.0 / REAL (II - 1)
         DO 85 I = 1,NX
            LL = MOD (I - 1, II)
            DI(I) = LL* X
            IF (DOSAT) DS(I) = (I - 1.0) / (NX - 1.0)
 85         CONTINUE
         DO 95 J = 1,IYWDGE
            X = (J - 1.0) / (IYWDGE - 1.0)
            CALL RFILL (NX, X, DH)
            JJ = JJ + 1
C                                       copy to sub-image
            IF ((JJ.GE.IDWIN(2)) .AND. (JJ.LE.IDWIN(4)) .AND.
     *         (MOD(JJ-1, IDY).EQ.0)) THEN
               SY = SY + 1
               SX = 0
               II = IDWIN(1)
               DO 90 I = II,NX,IDX
                  IF (I.LE.IDWIN(3)) THEN
                     SX = SX + 1
                     INTENS(SX,SY) = DI(I)
                     HUE(SX,SY) = DH(I)
                     IF (DOSAT) SATUR(SX,SY) = DS(I)
                     END IF
 90               CONTINUE
               END IF
            SUBR = 'YIMGIO'
            IF (DO3CHN) THEN
               CALL HILOAD (NX, IX, IY, DI, DH, DS, SCRTCH, IERR)
            ELSE
               CALL HI2ROW (NX, DOLOGI, NLI, NLH, NLS, DI, DH, DS,
     *            SC(1,1), SC(1,2), SC(1,3), SCRTCH)
               CALL YIMGIO ('WRIT', TVCH, IX, IY, 0, NX, SCRTCH, IERR)
               END IF
            IF (IERR.NE.0) GO TO 990
            IY = IY + 1
 95         CONTINUE
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                        error message
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TVHILD: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE HI2ROW (NX, DOLOGI, NLI, NLH, NLS, INT, HUE, SAT, SCI,
     *   SCH, SCS, ROW)
C-----------------------------------------------------------------------
C   scales intensity and hue and returns encoded image row:
C   algorithm:
C   Scale and clip 0,1 : INT(i) * SCI(1) - SCI(2)
C                        HUE(i) * SCH(1) - SCH(2)
C                        SAT(i) * SCS(1) - SCS(2)
C   Then -> image row:   ni = I * NLI - 0.001
C                        nh = H * NLH - 0.001
C                        img = NLI * nh + ni + 1
C                        if (NLS > 0) then
C                           ns = S * NLS - 0.001
C                           img = ni (if ns = 0)
C                           img = NLI + ni + NLI*nh + NLI*NLH*(ns-1)
C   Inputs:
C      NX       I       Number points in row
C      DOLOGI   I       Use lin,sq,log,elog transfer for intensity ?
C      NLI      I       Number levels output in intensity
C      NLH      I       Number levels output in hue
C      NLS      I       Number levels output in saturation
C      INT      R(NX)   Intensity row (0 - 1)
C      HUE      R(NX)   Hue row (0 - 1)
C      SAT      R(NX)   Saturation row (0 - 1)
C      SCI      R(2)    Scaling parameters for intensity: slope, offset
C      SCH      R(2)    Scaling parameters for hue: slope, offset
C      SCS      R(2)    Scaling parameters for saturation: slope, offset
C   Output:
C      ROW      I(NX)   Image row to load to TV
C-----------------------------------------------------------------------
      INTEGER   NX, DOLOGI, NLI, NLH, NLS, ROW(*)
      REAL      INT(*), HUE(*), SAT(*), SCI(2), SCH(2), SCS(2)
C
      INTEGER   I, NI, NH, NS
      LOGICAL   DONEG
      REAL      H, X, S, ASCI
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       scaling parameters
      ASCI = SCI(1)
      ASCI = ABS (ASCI)
      DONEG = SCI(1).LT.0.0
C                                       No saturation
      IF (NLS.LE.1)  THEN
         DO 10 I = 1,NX
            IF ((INT(I).EQ.FBLANK) .OR. (HUE(I).EQ.FBLANK)) THEN
               ROW(I) = 0
            ELSE
               X = INT(I) * ASCI - SCI(2)
               X = MAX (0.0, MIN (1.0, X))
               IF (DONEG) X = 1.0 - X
               IF (DOLOGI.GT.0) THEN
                  IF (DOLOGI.EQ.2) THEN
                     X = LOG10 (9.0 * X + 1.0)
                  ELSE IF (DOLOGI.EQ.3) THEN
                     X = 0.5 * LOG10 (99.0 * X + 1.0)
                  ELSE
                     X = SQRT (X)
                     END IF
                  X = MAX (0.0, MIN (1.0, X))
                  END IF
               H = HUE(I) * SCH(1) - SCH(2)
               H = MAX (0.0, MIN (1.0, H))
               NI = X * NLI - 0.001
               NH = H * NLH - 0.001
               ROW(I) = NI + NH * NLI + 1
               END IF
 10         CONTINUE
C                                       Saturation
      ELSE
         DO 20 I = 1,NX
            IF ((INT(I).EQ.FBLANK) .OR. (HUE(I).EQ.FBLANK) .OR.
     *         (SAT(I).EQ.FBLANK)) THEN
               ROW(I) = 0
            ELSE
               X = INT(I) * ASCI - SCI(2)
               X = MAX (0.0, MIN (1.0, X))
               IF (DONEG) X = 1.0 - X
               IF (DOLOGI.GT.0) THEN
                  IF (DOLOGI.EQ.2) THEN
                     X = LOG (9.0 * X + 1.0)
                  ELSE IF (DOLOGI.EQ.3) THEN
                     X = 0.5 * LOG10 (99.0 * X + 1.0)
                  ELSE
                     X = SQRT (X)
                     END IF
                  X = MAX (0.0, MIN (1.0, X))
                  END IF
               H = HUE(I) * SCH(1) - SCH(2)
               H = MAX (0.0, MIN (1.0, H))
               S = SAT(I) * SCS(1) - SCS(2)
               S = MAX (0.0, MIN (1.0, S))
               NI = X * NLI - 0.001
               NH = H * NLH - 0.001
               NS = S * NLS - 0.001
               ROW(I) = NI + 1
               IF (NS.GT.0) ROW(I) = ROW(I) +
     *            NLI * (1 + NH + NLH * (NS-1))
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE HILOAD (NP, IX, IY, DI, DH, DS, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   HILOAD does the scaling, clipping of an INT/HUE pair for a row,
C   converts to RGB, scales and loads the row to 3 channels on the TV
C   Inputs:
C      NP       I       Number of points in the row
C      IX       I       X position on the TV
C      IY       I       Y position on the TV
C   In/out:
C      FIRST    L       First call (inits things)
C      DI(*)    R(NP)   Intensity row
C      DH(*)    R(NP)   Hue row
C      DS(*)    R(NP)   Saturation row
C   Output:
C      IERR     I       Return code from YIMGIO
C-----------------------------------------------------------------------
      INTEGER   NP, IX, IY, SCRTCH(*), IERR
      REAL      DI(NP), DH(NP), DS(NP)
C
      INTEGER   I, J
      REAL      RED(2048), GREEN(2048), BLUE(2048), COLS(2048,3), S,
     *   ASCI, XI(2048), XH(2048), XS(2048), ASCH
      LOGICAL   DONEG, DONEG2
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (COLS, RED), (COLS(1,2), GREEN), (COLS(1,3), BLUE)
C-----------------------------------------------------------------------
      ASCI = SC(1,1)
      DONEG = ASCI.LT.0.0
      ASCI = ABS(ASCI)
      ASCH = SC(1,2)
      DONEG2 = ASCH.LT.0.0
      ASCH = ABS(ASCH)
C                                       scale
      DO 10 I = 1,NP
         IF (DI(I).EQ.FBLANK) THEN
            XI(I) = DI(I)
         ELSE
            XI(I) = DI(I) * ASCI - SC(2,1)
            IF (DONEG) XI(I) = 1.0 - XI(I)
            XI(I) = MAX (0.0, MIN (1.0, XI(I)))
            IF (DOLOGI.GT.0) THEN
               IF (DOLOGI.EQ.1) THEN
                  XI(I) = LOG10 (9.0 * XI(I) + 1.0)
               ELSE IF (DOLOGI.EQ.2) THEN
                  XI(I) = 0.5 * LOG10 (99.0 * XI(I) + 1.0)
               ELSE
                  XI(I) = SQRT (XI(I))
                  END IF
               END IF
            END IF
         IF (DH(I).EQ.FBLANK) THEN
            XH(I) = DH(I)
         ELSE
            XH(I) = DH(I) * ASCH - SC(2,2)
            XH(I) = MAX (0.0, MIN (1.0, XH(I)))
            IF (DONEG2) XH(I) = 1.0 - XH(I)
            IF (DOLOGH.GT.0) THEN
               IF (DOLOGH.EQ.1) THEN
                  XH(I) = LOG10 (9.0 * XH(I) + 1.0)
               ELSE IF (DOLOGH.EQ.2) THEN
                  XH(I) = 0.5 * LOG10 (99.0 * XH(I) + 1.0)
               ELSE
                  XH(I) = SQRT (XH(I))
                  END IF
               END IF
            END IF
         IF (DOSAT) THEN
            IF (DS(I).EQ.FBLANK) THEN
               XS(I) = DS(I)
            ELSE
               XS(I) = DS(I) * SC(1,3) - SC(2,3)
               END IF
            END IF
 10      CONTINUE
C                                       convert
      IF (OPTYPE.EQ.'LUT') THEN
         CALL HILRGB (FIRST, T, DOSAT, NP, XI, XH, XS, DOCIRC, RED,
     *      GREEN, BLUE)
      ELSE
         CALL HI2RGB (T, DOSAT, NP, XI, XH, XS, DOCIRC, RED, GREEN,
     *      BLUE)
         END IF
C                                       scale to TV
      S = MAXINT - 1
      DO 40 J = 1,3
         DO 30 I = 1,NP
            IF ((DI(I).EQ.FBLANK) .OR. (DH(I).EQ.FBLANK) .OR.
     *         ((DOSAT) .AND. (DS(I).EQ.FBLANK))) THEN
               SCRTCH(I) = 0
            ELSE
               SCRTCH(I) = COLS(I,J) * S + 1.5
               END IF
 30         CONTINUE
         I = TVCH + J - 1
         CALL YIMGIO ('WRIT', I, IX, IY, 0, NP, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TVHIOP (LX, LY, INTENS, HUE, SATUR, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TVHIOP reads in the intensity, hue, and saturation images, converts
C   them to RGB, optimizes the use of the RGB for the available TV, puts
C   the result on the TV (reading the images again) and returns
C   subarrays suitable for interactive loads.
C   Inputs:
C      LX       I          x dimension of in-core arrays
C      LY       I          y dimension of in-core arrays
C   Outputs:
C      INTENS   R(LX,LY)   in-core intensity array
C      HUE      R(LX,LY)   in-core hue array
C      SATUR    R(LX,LY)   in-core saturation array
C      SCRTCH   I(1536)    TV scratch array
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   LX, LY, SCRTCH(*), IRET
      REAL      INTENS(LX,LY), HUE(LX,LY), SATUR(LX,LY)
C
      INTEGER   I, J, IBLK, HBLK, SBLK, IERR, JJ, JX, JY, BIND1, BIND2,
     *   BIND3, SY, SX, IX, IY, II, LL, LEVS, RLUT(8200), GLUT(8200),
     *   BLUT(8200), TVCODE, COLS, HS(5,32768)
      REAL      X, DI(1536), DH(1536), DS(1536)
      CHARACTER SUBR*6
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      CALL YHOLD ('ONNN', IERR)
C                                       init the histogram et al.
      LEVS = 32
      COLS = LEVS * LEVS * LEVS
      IX = LEVS * LEVS
      DO 20 I = 1,COLS
         HS(1,I) = 0
         HS(2,I) = (I - 1) / IX
         J = (I - 1) / LEVS
         HS(3,I) = MOD (J, LEVS)
         HS(4,I) = MOD (I-1, LEVS)
 20      CONTINUE
C                                       init image windows
      JX = CATBLK(IIWIN+2) - CATBLK(IIWIN+0) + 1
      JY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), IBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), HBLK,
     *   IERR)
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), SBLK,
     *   IERR)
      IBLK = IBLK + 1
      HBLK = HBLK + 1
      SBLK = SBLK + 1
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1)
      SX = 0
      SY = 0
C                                       init the reads
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, HBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOSAT) THEN
         CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *      CATBL3(IIWIN), BUFF3, JBUFSZ, SBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      SY = 0
      LL = (JX - 1) / XINC + 1
      IF (DPARM(9).GT.0.0) LL = NX
      DO 50 J = 1,JY
C                                       read data rows
         SUBR = 'MDISK'
         CALL MDISK ('READ', LUN1, IND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOSAT) THEN
            CALL MDISK ('READ', LUN3, IND3, BUFF3, BIND3, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       scale data rows
            II = 0
            DO 30 I = 1,JX,XINC
               II = II + 1
               X = BUFF1(BIND1+I-1)
               IF (X.EQ.FBLANK) THEN
                  DI(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,1)) / (PRANGE(2,1) - PRANGE(1,1))
                  DI(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               X = BUFF2(BIND2+I-1)
               IF (X.EQ.FBLANK) THEN
                  DH(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,2)) / (PRANGE(2,2) - PRANGE(1,2))
                  DH(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               IF (DOSAT) THEN
                  X = BUFF3(BIND3+I-1)
                  IF (X.EQ.FBLANK) THEN
                     DS(II) = FBLANK
                  ELSE
                     X = (X - PRANGE(1,3)) / (PRANGE(2,3) - PRANGE(1,3))
                     DS(II) = MAX (0.0, MIN (1.0, X))
                     END IF
                  END IF
 30            CONTINUE
C                                       add step wedge to right
            JJ = (J - 1) / YINC + 1
            IF ((IXWDGE.GT.1) .AND. (DPARM(9).GT.0.0)) THEN
               X = REAL (JJ - 1) / REAL ((JY - 1) / YINC)
               IF (DOSAT) THEN
                  CALL RFILL (IXWDGE, X, DS(II+1))
                  X = NLS * X
                  I = X
                  X = X - I
                  END IF
               CALL RFILL (IXWDGE, X, DI(II+1))
               DO 35 I = 1,IXWDGE
                  II = II + 1
                  DH(II) = (I - 1.0) / (IXWDGE - 1.0)
 35               CONTINUE
               END IF
            CALL HIHIST (T, LL, IX, IY, DI, DH, DS, HS, SCRTCH, IERR)
            IY = IY + 1
            END IF
 50      CONTINUE
C                                       top step wedge
      IF ((IYWDGE.GT.1) .AND. (DPARM(9).GT.0.0)) THEN
         II = NX
         IF (DOSAT) II = NX / NLS
         X = 1.0 / REAL (II - 1)
         DO 60 I = 1,NX
            LL = MOD (I - 1, II)
            DI(I) = LL* X
            IF (DOSAT) DS(I) = (I - 1.0) / (NX - 1.0)
 60         CONTINUE
         DO 70 J = 1,IYWDGE
            X = (J - 1.0) / (IYWDGE - 1.0)
            CALL RFILL (NX, X, BUFF2)
            JJ = JJ + 1
            CALL RCOPY (NX, DI, BUFF1)
            CALL RCOPY (NX, DS, BUFF3)
            CALL HIHIST (T, NX, IX, IY, BUFF1, BUFF2, BUFF3, HS, SCRTCH,
     *         IERR)
            IY = IY + 1
 70         CONTINUE
         END IF
C                                       optimize
      CALL OPTLUT (LEVS, COLS, HS, RLUT, GLUT, BLUT)
C                                       now read it all again
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1)
      SX = 0
      SY = 0
C                                       init the reads
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, IBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, HBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOSAT) THEN
         CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *      CATBL3(IIWIN), BUFF3, JBUFSZ, SBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      SY = 0
      DO 150 J = 1,JY
C                                       read data rows
         SUBR = 'MDISK'
         CALL MDISK ('READ', LUN1, IND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOSAT) THEN
            CALL MDISK ('READ', LUN3, IND3, BUFF3, BIND3, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       scale data rows
            II = 0
            DO 130 I = 1,JX,XINC
               II = II + 1
               X = BUFF1(BIND1+I-1)
               IF (X.EQ.FBLANK) THEN
                  DI(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,1)) / (PRANGE(2,1) - PRANGE(1,1))
                  DI(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               X = BUFF2(BIND2+I-1)
               IF (X.EQ.FBLANK) THEN
                  DH(II) = FBLANK
               ELSE
                  X = (X - PRANGE(1,2)) / (PRANGE(2,2) - PRANGE(1,2))
                  DH(II) = MAX (0.0, MIN (1.0, X))
                  END IF
               IF (DOSAT) THEN
                  X = BUFF3(BIND3+I-1)
                  IF (X.EQ.FBLANK) THEN
                     DS(II) = FBLANK
                  ELSE
                     X = (X - PRANGE(1,3)) / (PRANGE(2,3) - PRANGE(1,3))
                     DS(II) = MAX (0.0, MIN (1.0, X))
                     END IF
                  END IF
 130           CONTINUE
C                                       add step wedge to right
            JJ = (J - 1) / YINC + 1
            IF (IXWDGE.GT.1) THEN
               X = REAL (JJ - 1) / REAL ((JY - 1) / YINC)
               IF (DOSAT) THEN
                  CALL RFILL (IXWDGE, X, DS(II+1))
                  X = NLS * X
                  I = X
                  X = X - I
                  END IF
               CALL RFILL (IXWDGE, X, DI(II+1))
               DO 135 I = 1,IXWDGE
                  II = II + 1
                  DH(II) = (I - 1.0) / (IXWDGE - 1.0)
 135              CONTINUE
               END IF
C                                       copy to sub-image
            IF ((JJ.GE.IDWIN(2)) .AND. (JJ.LE.IDWIN(4)) .AND.
     *         (MOD(JJ-1, IDY).EQ.0)) THEN
               SY = SY + 1
               SX = 0
               II = IDWIN(1)
               DO 140 I = II,NX,IDX
                  IF (I.LE.IDWIN(3)) THEN
                     SX = SX + 1
                     INTENS(SX,SY) = DI(I)
                     HUE(SX,SY) = DH(I)
                     IF (DOSAT) SATUR(SX,SY) = DS(I)
                     END IF
 140              CONTINUE
               END IF
            SUBR = 'YIMGIO'
            CALL HIHIST (F, NX, IX, IY, DI, DH, DS, HS, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            IY = IY + 1
            END IF
 150     CONTINUE
C                                       top step wedge
      IF (IYWDGE.GT.1) THEN
         II = NX
         IF (DOSAT) II = NX / NLS
         X = 1.0 / REAL (II - 1)
         DO 160 I = 1,NX
            LL = MOD (I - 1, II)
            DI(I) = LL* X
            IF (DOSAT) DS(I) = (I - 1.0) / (NX - 1.0)
 160        CONTINUE
         DO 170 J = 1,IYWDGE
            X = (J - 1.0) / (IYWDGE - 1.0)
            CALL RFILL (NX, X, DH)
            JJ = JJ + 1
C                                       copy to sub-image
            IF ((JJ.GE.IDWIN(2)) .AND. (JJ.LE.IDWIN(4)) .AND.
     *         (MOD(JJ-1, IDY).EQ.0)) THEN
               SY = SY + 1
               SX = 0
               II = IDWIN(1)
               DO 165 I = II,NX,IDX
                  IF (I.LE.IDWIN(3)) THEN
                     SX = SX + 1
                     INTENS(SX,SY) = DI(I)
                     HUE(SX,SY) = DH(I)
                     IF (DOSAT) SATUR(SX,SY) = DS(I)
                     END IF
 165              CONTINUE
               END IF
            SUBR = 'YIMGIO'
            CALL RCOPY (NX, DI, BUFF1)
            CALL RCOPY (NX, DH, BUFF2)
            CALL RCOPY (NX, DS, BUFF3)
            CALL HIHIST (F, NX, IX, IY, BUFF1, BUFF2, BUFF3, HS, SCRTCH,
     *         IERR)
            IF (IERR.NE.0) GO TO 990
            IY = IY + 1
 170        CONTINUE
         END IF
      SUBR = 'YSLECT'
      CALL YSLECT ('OFFF', TVCH, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('ONNN', TVCH+1, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOGRC) CALL YHOLD ('OFFF', IERR)
      SUBR = 'YLUT'
      TVCODE = 2 ** TVCH
      CALL YLUT ('WRIT', TVCODE, 1, T, BLUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YLUT ('WRIT', TVCODE, 2, T, GLUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YLUT ('WRIT', TVCODE, 4, T, RLUT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YHOLD ('OFFF', IERR)
C                                        error message
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TVHIOP: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE HIHIST (DOSUM, NP, IX, IY, DI, DH, DS, HS, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C   HIHIST does the scaling, clipping of an INT/HUE pair for a row,
C   converts to RGB, scales and then either sums into a histogram or
C   looks up in the histogram and does a TVLOAD
C   Inputs:
C      DOSUM    L       T => sum, else apply histogram
C      NP       I       Number of points in the row
C      IX       I       X position on the TV
C      IY       I       Y position on the TV
C   In/out:
C      FIRST    L       First call (inits things)
C      DI       R(NP)   Intensity row
C      DH       R(NP)   Hue row
C      DS       R(NP)   Saturation row
C      HS       I(5,32768)  Histogram
C   Output:
C      IERR     I       Return code from YIMGIO
C-----------------------------------------------------------------------
      LOGICAL   DOSUM
      INTEGER   NP, IX, IY, HS(5,32768), SCRTCH(*), IERR
      REAL      DI(NP), DH(NP), DS(NP)
C
      INTEGER   I, J, IR, IG, IB, II, LEVS, LEV1, JJ
      REAL      RED(2048), GREEN(2048), BLUE(2048), COLS(2048,3), X,
     *   ASCI
      LOGICAL   DONEG
      INCLUDE 'TVHUI.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (COLS, RED), (COLS(1,2), GREEN), (COLS(1,3), BLUE)
C-----------------------------------------------------------------------
      LEVS = 32
      ASCI = SC(1,1)
      IF (DOLOGI.GT.0) THEN
         DONEG = ASCI.LT.0.0
         ASCI = ABS(ASCI)
         END IF
C                                       scale
      DO 10 I = 1,NP
         IF (DI(I).NE.FBLANK) THEN
            DI(I) = DI(I) * ASCI - SC(2,1)
            IF (DOLOGI.GT.0) THEN
               X = MAX (0.0, MIN (1.0, DI(I)))
               IF (DONEG) X = 1.0 - X
               IF (DOLOGI.EQ.2) THEN
                  DI(I) = LOG10 (9.0 * X + 1.0)
               ELSE IF (DOLOGI.EQ.3) THEN
                  DI(I) = 0.5 * LOG10 (99.0 * X + 1.0)
               ELSE
                  DI(I) = SQRT (X)
                  END IF
               END IF
            END IF
         IF (DH(I).NE.FBLANK) DH(I) = DH(I) * SC(1,2) - SC(2,2)
         IF (DOSAT) THEN
            IF (DS(I).NE.FBLANK) DS(I) = DS(I) * SC(1,3) - SC(2,3)
            END IF
 10      CONTINUE
C                                       convert
      IF (OPTYPE.EQ.'LUT') THEN
         CALL HILRGB (FIRST, T, DOSAT, NP, DI, DH, DS, DOCIRC, RED,
     *      GREEN, BLUE)
      ELSE
         CALL HI2RGB (T, DOSAT, NP, DI, DH, DS, DOCIRC, RED, GREEN,
     *      BLUE)
         END IF
C                                       count or scale to TV
      LEV1 = LEVS - 1
      JJ = LEVS * LEVS * LEVS
      DO 30 I = 1,NP
         IF ((DI(I).EQ.FBLANK) .OR. (DH(I).EQ.FBLANK) .OR.
     *      ((DOSAT) .AND. (DS(I).EQ.FBLANK))) THEN
            SCRTCH(I) = 0
         ELSE
            IR = RED(I) * LEVS - 0.001
            IG = GREEN(I) * LEVS - 0.001
            IB = BLUE(I) * LEVS - 0.001
            IR = MAX (0, MIN (IR, LEV1))
            IG = MAX (0, MIN (IG, LEV1))
            IB = MAX (0, MIN (IB, LEV1))
            II = IB + LEVS * (IG + LEVS * IR) + 1
            II = MAX (1, MIN (JJ, II))
            IF (DOSUM) THEN
               HS(1,II) = HS(1,II) + 1
            ELSE
               J = HS(5,II) + 0.0001
               SCRTCH(I) = MAX (1, MIN (MAXINT, J))
               END IF
            END IF
 30      CONTINUE
      IF (DOSUM) THEN
         IERR = 0
      ELSE
         I = TVCH + 1
         CALL YIMGIO ('WRIT', I, IX, IY, 0, NP, SCRTCH, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OPTLUT (LEVS, COLS, HC, RLUT, GLUT, BLUT)
C-----------------------------------------------------------------------
C   OPTLUT implements a color optimization scheme borrowed loosely from
C   ppmquant written by Jef Poskanzer.
C   Inputs:
C      LEVS    I           Number of sub levels for each base color
C      COLS    I           Total number of colors: LEVS**3
C   In/out:
C      HC      I(5,COLS)   in-core histogram array
C   Outputs:
C      RLUT    I(*)        Red LUT for optimal coloring
C      GLUT    I(*)        Green LUT for optimal coloring
C      BLUT    I(*)        Blue LUT for optimal coloring
C-----------------------------------------------------------------------
      INTEGER   LEVS, COLS, HC(5,COLS), RLUT(*), GLUT(*), BLUT(*)
C
      INTEGER   I, J, IERR, II, JJ, NNC, NNP, NBOX, HBOX(3,8200), MBOX,
     *   RN, RX, GN, GX, BN, BX, IC, IND, IP, CORD(2,3)
      REAL      RSUM, GSUM, BSUM, WSUM, W, XM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'Now optimize the colors'
      CALL MSGWRT (1)
C                                       compress histogram
C                                       count # pixels and colors
      NNC = 0
      NNP = 0
      DO 60 I = 1,COLS
         IF (HC(1,I).GT.0) THEN
            NNC = NNC + 1
            IF (NNC.LT.I) CALL COPY (5, HC(1,I), HC(1,NNC))
            NNP = NNP + HC(1,NNC)
            END IF
 60      CONTINUE
C                                       test bad scaling
      IF (NNC.LT.LEVS/2) THEN
         MSGTXT = 'Few values in histogram: reset scaling'
         CALL MSGWRT (6)
         END IF
C                                       make boxes in histogram
      MBOX = MIN (8200, MAXINT)
C                                       already have it!
      IF (NNC.LE.MBOX) THEN
         NBOX = NNC
         DO 65 I = 1,NBOX
            HBOX(1,I) = I
            HBOX(2,I) = 1
            HBOX(3,I) = HC(1,I)
 65         CONTINUE
C                                       make boxes by subdividing
      ELSE
C                                       start with all in 1
         NBOX = 1
         HBOX(1,1) = 1
         HBOX(2,1) = NNC
         HBOX(3,1) = NNP
C                                       loop point to make more
 70      IF (NBOX.GE.MBOX) GO TO 100
C                                       which box to split
            JJ = 0
            IP = 0
            DO 75 I = 1,NBOX
               IF (HBOX(2,I).GE.2) THEN
                  IF (HBOX(3,I).GT.IP) THEN
                     IP = HBOX(3,I)
                     JJ = I
                     END IF
                  END IF
 75            CONTINUE
C                                       all singletons => done
            IF (JJ.LE.0) GO TO 100
C                                       split box JJ
            IC = HBOX(2,JJ)
            IND = HBOX(1,JJ)
C                                       find range of colors
            RN = HC(2,IND)
            GN = HC(3,IND)
            BN = HC(4,IND)
            RX = RN
            GX = GN
            BX = BN
            DO 80 I = 2,IC
               J = IND + I - 1
               RN = MIN (RN, HC(2,J))
               GN = MIN (GN, HC(3,J))
               BN = MIN (BN, HC(4,J))
               RX = MAX (RX, HC(2,J))
               GX = MAX (GX, HC(3,J))
               BX = MAX (BX, HC(4,J))
 80            CONTINUE
            CORD(1,1) = RX - RN
            CORD(1,2) = GX - GN
            CORD(1,3) = BX - BN
            CORD(2,1) = 2
            CORD(2,2) = 3
            CORD(2,3) = 4
C                                       sort to get widest range
            CALL ISHSRT (CORD, 3, 1, 2, 2, IERR)
C                                       sort the histogram
            CALL ISHSRT (HC(1,IND), IC, CORD(2,3), CORD(2,2), 5, IERR)
C                                       now find median in pixels
            RN = HC(1,IND)
            RX = IP / 2
            DO 85 I = 2,IC
               J = IND + I - 1
               IF (RN.GE.RX) GO TO 90
               RN = RN + HC(1,J)
 85            CONTINUE
            RN = RN - HC(1,IND+IC-1)
            I = IC
C                                       make new box
 90         NBOX = NBOX + 1
            HBOX(1,NBOX) = J
            HBOX(2,NBOX) = IC - I + 1
            HBOX(3,NBOX) = IP - RN
            HBOX(2,JJ) = I - 1
            HBOX(3,JJ) = RN
            GO TO 70
         END IF
C                                       got the histogram boxed up
C                                       Blue LUT
 100  JJ = MAXINT + 1
      CALL FILL (JJ, 0, BLUT)
      CALL FILL (JJ, 0, RLUT)
      CALL FILL (JJ, 0, GLUT)
      XM = (LUTOUT - 1.0) / (LEVS - 1.0)
      DO 110 JJ = 1,NBOX
         IC = HBOX(2,JJ)
         IND = HBOX(1,JJ)
         RSUM = 0.0
         GSUM = 0.0
         BSUM = 0.0
         WSUM = 0.0
         DO 105 I = 1,IC
            J = IND + I - 1
            W = HC(1,J)
            WSUM = WSUM + W
            RSUM = RSUM + W * HC(2,J)
            GSUM = GSUM + W * HC(3,J)
            BSUM = BSUM + W * HC(4,J)
 105        CONTINUE
         IF (WSUM.GT.0.0) THEN
            I = XM * RSUM / WSUM + 1.5
            RLUT(JJ+1) = MAX (1, MIN (LUTOUT, I))
            I = XM * GSUM / WSUM + 1.5
            GLUT(JJ+1) = MAX (1, MIN (LUTOUT, I))
            I = XM * BSUM / WSUM + 1.5
            BLUT(JJ+1) = MAX (1, MIN (LUTOUT, I))
            END IF
 110     CONTINUE
C                                       reverse translation
      DO 115 I = 1,COLS
         HC(5,I) = 0
 115     CONTINUE
      DO 125 JJ = 1,NBOX
         IC = HBOX(2,JJ)
         IND = HBOX(1,JJ)
         DO 120 I = 1,IC
            J = IND + I - 1
            II = HC(4,J) + LEVS * (HC(3,J) + LEVS * HC(2,J)) + 1
            IF ((II.GT.0) .AND. (II.LE.COLS)) HC(5,II) = JJ
 120        CONTINUE
 125     CONTINUE
      MSGTXT = 'Now re-load image with optimized color scheme'
      CALL MSGWRT (1)
C
 999  RETURN
      END
      SUBROUTINE TVHIOU (SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TVHIOU writes out the image with an RGB axis developed and filled
C   using the hue scaling prevously established.
C   Outputs:
C      SCRTCH   I(*)   Scratch buffer
C      IRET     I      Return code
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IRET
C
      INTEGER   I, LUNO1, LUNO2, LUNO3, INDO1, INDO2, INDO3, IWIN(4),
     *   IDEP(5), IBLI, IBLH, IBLS, IBL1, IBL2, IBL3, IBINDI, IBINDH,
     *   IBINDS, IBIND1, IBIND2, IBIND3, IX, IY, IERR, IROUND, IDO, J
      CHARACTER SUBR*6, MTYPE*2, PHNAME*48, LINE*72
      LOGICAL   ISBLNK, DONEG2
      INCLUDE 'TVHUI.INC'
      REAL      BUFFI(MABFSS), BUFFH(MABFSS), BUFFS(MABFSS), SLH, OFH,
     *   RMX, RMN, SLS, OFS, X, ASCH, TRANGE(2,3)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = T
      ISBLNK = F
      RMX = -1.0E20
      RMN = -RMX
      CALL COPY (256, CATBLK, SCRTCH)
      CALL COPY (256, CATBL4, CATBLK)
      CALL COPY (256, SCRTCH, CATBL4)
      ASCH = SC(1,2)
      DONEG2 = ASCH.LT.0.0
      ASCH = ABS(ASCH)
C                                       Output windows
      CATBL4(IIWIN) = IROUND (OBLC(1,1))
      CATBL2(IIWIN) = IROUND (OBLC(1,2))
      CATBL3(IIWIN) = IROUND (OBLC(1,3))
      CATBL4(IIWIN+1) = IROUND (OBLC(2,1))
      CATBL2(IIWIN+1) = IROUND (OBLC(2,2))
      CATBL3(IIWIN+1) = IROUND (OBLC(2,3))
      CATBL4(IIWIN+2) = IROUND (OTRC(1,1))
      CATBL2(IIWIN+2) = IROUND (OTRC(1,2))
      CATBL3(IIWIN+2) = IROUND (OTRC(1,3))
      CATBL4(IIWIN+3) = IROUND (OTRC(2,1))
      CATBL2(IIWIN+3) = IROUND (OTRC(2,2))
      CATBL3(IIWIN+3) = IROUND (OTRC(2,3))
C                                       Copy any header keywords
C                                       Allow failure
      CALL KEYCOP (VOL1, SLOT1, VOLO, SLOTO, IERR)
C                                       scaling
      SLH = ASCH / (PRANGE(2,2) - PRANGE(1,2))
      OFH = SC(2,2) + SLH * PRANGE(1,2)
      IF (DOSAT) THEN
         SLS = SC(1,3) / (PRANGE(2,3) - PRANGE(1,3))
         OFS = SC(2,3) + SLS * PRANGE(1,3)
         END IF
C                                       Open new file.
      LUNO1 = LUN3 + 1
      LUNO2 = LUNO1 + 1
      LUNO3 = LUNO2 + 1
      CALL ZPHFIL ('MA', VOLO, SLOTO, 1, PHNAME, IERR)
      SUBR = 'ZOPEN'
      CALL ZOPEN (LUNO1, INDO1, VOLO, PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZOPEN (LUNO2, INDO2, VOLO, PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ZOPEN (LUNO3, INDO3, VOLO, PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Window for destination file.
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      CALL FILL (5, 1, IDEP)
      CALL COMOFF (CATBL4(KIDIM), CATBL4(KINAX), CATBL4(IIDEP), IBLI,
     *   IERR)
      IBLI = IBLI + 1
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), IBLH,
     *   IERR)
      IBLH = IBLH + 1
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), IBLS,
     *   IERR)
      IBLS = IBLS + 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL1, IERR)
      IDEP(1) = 2
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL2, IERR)
      IDEP(1) = 3
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL3, IERR)
      IBL1 = IBL1 + 1
      IBL2 = IBL2 + 1
      IBL3 = IBL3 + 1
C                                       Read/write time
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C                                       init the IOs
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBL4(KINAX), CATBL4(KINAX+1),
     *   CATBL4(IIWIN), BUFFI, JBUFSZ, IBLI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX),
     *   CATBL2(KINAX+1), CATBL2(IIWIN), BUFFH, JBUFSZ, IBLH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DOSAT) THEN
         CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX),
     *      CATBL3(KINAX+1), CATBL3(IIWIN), BUFFS, JBUFSZ, IBLS, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNO1, INDO1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF1, JBUFSZ, IBL1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('WRIT', LUNO2, INDO2, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF2, JBUFSZ, IBL2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('WRIT', LUNO3, INDO3, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF3, JBUFSZ, IBL3, IERR)
      IF (IERR.NE.0) GO TO 990
      SUBR = 'MDISK'
      DO 50 IY = 1,NY
C                                       do reads, point for writes
         CALL MDISK ('READ', LUN1, IND1, BUFFI, IBINDI, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFFH, IBINDH, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (DOSAT) THEN
             CALL MDISK ('READ', LUN3, IND3, BUFFS, IBINDS, IERR)
             IF (IERR.NE.0) GO TO 990
             END IF
         CALL MDISK ('WRIT', LUNO1, INDO1, BUFF1, IBIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('WRIT', LUNO2, INDO2, BUFF2, IBIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('WRIT', LUNO3, INDO3, BUFF3, IBIND3, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       scale data
         DO 20 IX = 1,NX
            I = IBINDH + IX - 1
            IF (BUFFH(I).NE.FBLANK) THEN
               BUFFH(I) = MAX (PRANGE(1,2), MIN (PRANGE(2,2), BUFFH(I)))
               BUFFH(I) = BUFFH(I) * SLH - OFH
               BUFFH(I) = MAX (0.0, MIN (1.0, BUFFH(I)))
               IF (DONEG2) BUFFH(I) = 1.0 - BUFFH(I)
               IF (DOLOGH.GT.0) THEN
                  IF (DOLOGH.EQ.1) THEN
                     BUFFH(I) = LOG10 (9.0 * BUFFH(I) + 1.0)
                  ELSE IF (DOLOGH.EQ.2) THEN
                     BUFFH(I) = 0.5 * LOG10 (99.0 * BUFFH(I) + 1.0)
                  ELSE
                     BUFFH(I) = SQRT (BUFFH(I))
                     END IF
                  END IF
               END IF
 20         CONTINUE
         IF (DOSAT) THEN
            DO 25 IX = 1,NX
               I = IBINDS + IX - 1
               IF (BUFFS(I).NE.FBLANK) THEN
                  BUFFS(I) = MAX (PRANGE(1,3), MIN (PRANGE(2,3),
     *               BUFFS(I)))
                  BUFFS(I) = BUFFS(I) * SLS - OFS
                  END IF
 25            CONTINUE
            END IF
C                                       convert to RGB
         IF (OPTYPE.EQ.'LUT ') THEN
            CALL HILRGB (FIRST, F, DOSAT, NX, BUFFI(IBINDI),
     *         BUFFH(IBINDH), BUFFS(IBINDS), DOCIRC, BUFF1(IBIND1),
     *         BUFF2(IBIND2), BUFF3(IBIND3))
         ELSE
            CALL HI2RGB (F, DOSAT, NX, BUFFI(IBINDI),
     *         BUFFH(IBINDH), BUFFS(IBINDS), DOCIRC, BUFF1(IBIND1),
     *         BUFF2(IBIND2), BUFF3(IBIND3))
            END IF
C                                       get scale and blanks
         DO 30 IX = 1,NX
            I = IX - 1
            IF ((BUFFH(IBINDH+I).EQ.FBLANK) .OR.
     *         (BUFFI(IBINDI+I).EQ.FBLANK) .OR.
     *         ((DOSAT) .AND. (BUFFS(IBINDS+I).EQ.FBLANK))) THEN
               BUFF1(IBIND1+I) = FBLANK
               BUFF2(IBIND2+I) = FBLANK
               BUFF3(IBIND3+I) = FBLANK
               ISBLNK = T
            ELSE
               RMN = MIN (RMN, BUFF1(IBIND1+I))
               RMX = MAX (RMX, BUFF1(IBIND1+I))
               RMN = MIN (RMN, BUFF2(IBIND2+I))
               RMX = MAX (RMX, BUFF2(IBIND2+I))
               RMN = MIN (RMN, BUFF3(IBIND3+I))
               RMX = MAX (RMX, BUFF3(IBIND3+I))
               END IF
 30         CONTINUE
 50      CONTINUE
C                                       finish the writes
      CALL MDISK ('FINI', LUNO1, INDO1, BUFF1, IBIND1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MDISK ('FINI', LUNO2, INDO2, BUFF2, IBIND2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MDISK ('FINI', LUNO3, INDO3, BUFF3, IBIND3, IERR)
      IF (IERR.NE.0) GO TO 990
      FRW(NCFILE) = 1
C                                       close the files
      CALL ZCLOSE (LUNO1, INDO1, IERR)
      CALL ZCLOSE (LUNO2, INDO2, IERR)
      CALL ZCLOSE (LUNO3, INDO3, IERR)
      CALL ZCLOSE (LUN1, IND1, IERR)
      CALL ZCLOSE (LUN2, IND2, IERR)
      IF (DOSAT) CALL ZCLOSE (LUN3, IND3, IERR)
C                                       update the header
      IF (RMX.LE.RMN) THEN
         MSGTXT = 'NO VALID PIXELS FOUND: QUITTING'
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
         END IF
      CATR(KRDMN) = RMN
      CATR(KRDMX) = RMX
      CATR(KRBLK) = 0.0
      IF (ISBLNK) CATR(KRBLK) = FBLANK
C                                       HI file
      CALL HIINIT (2)
      CALL HISCOP (LUNO3, LUNO2, VOL1, VOLO, SLOT1, SLOTO, CATBLK,
     *   BUFF1, SCRTCH, IERR)
      IF (IERR.GT.3) GO TO 100
C                                       TVHUI history: files
      CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQ1, VOL1, LUNO2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
      CALL HENCO2 (TSKNAM, NAMIN2, CLSIN2, SEQ2, VOL2, LUNO2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
      IF (DOSAT) THEN
         CALL HENCO3 (TSKNAM, NAMIN3, CLSIN3, SEQ3, VOL3, LUNO2, SCRTCH,
     *      IERR)
         IF (IERR.NE.0) GO TO 90
         END IF
C                                       TVHUI history: corners
      CALL COPY (4, CATBL4(IIWIN), IWIN)
      CALL COPY (5, CATBL4(IIDEP), IDEP)
      WRITE (LINE,1050) TSKNAM, 'BLC', IWIN(1), IWIN(2), IDEP,
     *   'Intensity'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      WRITE (LINE,1050) TSKNAM, 'TRC', IWIN(3), IWIN(4), IDEP,
     *   'Intensity'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      CALL COPY (4, CATBL2(IIWIN), IWIN)
      CALL COPY (5, CATBL2(IIDEP), IDEP)
      WRITE (LINE,1050) TSKNAM, 'BLC', IWIN(1), IWIN(2), IDEP, 'Hue'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      WRITE (LINE,1050) TSKNAM, 'TRC', IWIN(3), IWIN(4), IDEP, 'Hue'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      IF (DOSAT) THEN
         CALL COPY (4, CATBL3(IIWIN), IWIN)
         CALL COPY (5, CATBL3(IIDEP), IDEP)
         WRITE (LINE,1050) TSKNAM, 'BLC', IWIN(1), IWIN(2), IDEP,
     *      'Saturation'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1050) TSKNAM, 'TRC', IWIN(3), IWIN(4), IDEP,
     *      'Saturation'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         END IF
C                                       TVHUI history: scaling
      TRANGE(1,2) = OFH / SLH
      TRANGE(2,2) = 1.0 / SLH + TRANGE(1,2)
      WRITE (LINE,1060) TSKNAM, TRANGE(1,2), TRANGE(2,2), 'Hue'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      IF (DOSAT) THEN
         TRANGE(1,3) = OFS / SLS
         TRANGE(2,3) = 1.0 / SLS + TRANGE(1,3)
         WRITE (LINE,1060) TSKNAM, TRANGE(1,3), TRANGE(2,3),
     *      'Saturation'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         END IF
C                                       TVHUI history: operation
      IF (OPTYPE.NE.'LUT ') OPTYPE = 'S=1 '
      IF (DOCIRC.GT.0.0) THEN
         MTYPE = ' T'
      ELSE
         MTYPE = ' F'
         END IF
      LINE = TSKNAM // 'OPTYPE ''' // OPTYPE // '''  DOCIRCLE =' //
     *   MTYPE
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
C                                       close HI
 90   IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         END IF
      CALL HICLOS (LUNO2, T, SCRTCH, IERR)
C                                       Successful finish: image
 100  CALL CATIO ('UPDT', VOLO, SLOTO, CATBLK, 'REST', SCRTCH, IERR)
C                                       write a wedge
      IDO = IROUND (DOOUT)
      IF (IDO.GE.2) THEN
         FIRST = .TRUE.
C                                       first 2 axes
         IF (XSIZE(1).LT.25.) THEN
            XSIZE(1) = CATBLK(KINAX)
            IF (MAXXTV(1).LT.XSIZE(1)) XSIZE(1) = MAXXTV(1)
            END IF
         IF (XSIZE(2).LT.20.) XSIZE(2) = MAX (XSIZE(1), 100.0) * 0.2
         CATBLK(KINAX) = XSIZE(1) + 0.1
         CATBLK(KINAX+1) = XSIZE(2) + 0.1
C                                       intensity axis
         J = 0
         IF (IDO.GE.3) J = 1
C        CATD(KDCRV+J) = CATR4(KRDMN)
         CATD(KDCRV+J) = PRANGE(1,1)
         CATR(KRCRP+J) = 1.0
C        CATR(KRCIC+J) = (CATR4(KRDMX) - CATR4(KRDMN)) /
         CATR(KRCIC+J) = (PRANGE(2,1) - PRANGE(1,1)) /
     *      (CATBLK(KINAX+J) - 1.0)
         CATR(KRCRT+J) = 0.0
         CATH(KHCTP+2*J) = CATH4(KHBUN)
         CALL CHR2H (4, ' INT', 1, CATH(KHCTP+2*J+1))
C                                       hue axis
         J = 1
         IF (IDO.GE.3) J = 0
         CATD(KDCRV+J) = PRANGE(1,2)
         CATR(KRCRP+J) = 1.0
         CATR(KRCIC+J) = (PRANGE(2,2) - PRANGE(1,2)) /
     *      (CATBLK(KINAX+J) - 1.0)
         CATR(KRCRT+J) = 0.0
         CATH(KHCTP+2*J) = CATH2(KHBUN)
         CALL CHR2H (4, ' HUE', 1, CATH(KHCTP+2*J+1))
         CALL FILL (4, 1, CATBLK(KINAX+3))
C                                       create
         CALL MAKOUT (NAMIN, CLSIN, SEQ1, 'TVHWED', NAMOU2, CLSOU2,
     *      SEQO2)
         CALL CHR2H (12, NAMOU2, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, CLSOU2, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
         CATBLK(KIIMS) = SEQO2
C                                       Create new cataloged file.
         CALL MCREAT (VOLO2, SLOTO, SCRTCH, IERR)
         SUBR = 'MCREAT'
         IF (IERR.NE.0) GO TO 990
         SEQO2 = CATBLK(KIIMS)
         NCFILE = NCFILE + 1
         FCNO(NCFILE) = SLOTO
         FVOL(NCFILE) = VOLO2
         FRW(NCFILE) = 2
C                                       Open new file.
         LUNO1 = LUN3 + 1
         LUNO2 = LUNO1 + 1
         LUNO3 = LUNO2 + 1
         CALL ZPHFIL ('MA', VOLO2, SLOTO, 1, PHNAME, IERR)
         SUBR = 'ZOPEN'
         CALL ZOPEN (LUNO1, INDO1, VOLO2, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ZOPEN (LUNO2, INDO2, VOLO2, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ZOPEN (LUNO3, INDO3, VOLO2, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) GO TO 990
         IWIN(1) = 1
         IWIN(2) = 1
         IWIN(3) = CATBLK(KINAX)
         IWIN(4) = CATBLK(KINAX+1)
         CALL FILL (5, 1, IDEP)
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL1, IERR)
         IDEP(1) = 2
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL2, IERR)
         IDEP(1) = 3
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEP, IBL3, IERR)
         IBL1 = IBL1 + 1
         IBL2 = IBL2 + 1
         IBL3 = IBL3 + 1
C                                       Read/write time
         NX = CATBLK(KINAX)
         NY = CATBLK(KINAX+1)
         ISBLNK = F
         RMX = -1.0E20
         RMN = -RMX
C                                       init the IOs
         SUBR = 'MINIT'
         CALL MINIT ('WRIT', LUNO1, INDO1, CATBLK(KINAX),
     *      CATBLK(KINAX+1), IWIN, BUFF1, JBUFSZ, IBL1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MINIT ('WRIT', LUNO2, INDO2, CATBLK(KINAX),
     *      CATBLK(KINAX+1), IWIN, BUFF2, JBUFSZ, IBL2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MINIT ('WRIT', LUNO3, INDO3, CATBLK(KINAX),
     *      CATBLK(KINAX+1), IWIN, BUFF3, JBUFSZ, IBL3, IERR)
         IF (IERR.NE.0) GO TO 990
         SUBR = 'MDISK'
         DO 150 IY = 1,NY
C                                       point for writes
            CALL MDISK ('WRIT', LUNO1, INDO1, BUFF1, IBIND1, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL MDISK ('WRIT', LUNO2, INDO2, BUFF2, IBIND2, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL MDISK ('WRIT', LUNO3, INDO3, BUFF3, IBIND3, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       set row for I/H
            IF (IDO.LT.3) THEN
               X = CATD(KDCRV+1) + (IY-1) * CATR(KRCIC+1)
               X = X * SLH - OFH
               X = MAX (0.0, MIN (1.0, X))
               IF (DONEG2) X = 1.0 - X
               IF (DOLOGH.EQ.1) THEN
                  X = LOG10 (9.0 * X + 1.0)
               ELSE IF (DOLOGH.EQ.2) THEN
                  X = 0.5 * LOG10 (99.0 * X + 1.0)
               ELSE IF (DOLOGH.EQ.3) THEN
                  X = SQRT (X)
                  END IF
               CALL RFILL (NX, X, BUFFH)
               DO 110 I = 1,NX
                  BUFFI(I) = CATD(KDCRV) + (I-1.) * CATR(KRCIC)
 110              CONTINUE
C                                       set row for H/I
            ELSE
               DO 120 I = 1,NX
                  X = CATD(KDCRV) + (I-1) * CATR(KRCIC)
                  X = X * SLH - OFH
                  X = MAX (0.0, MIN (1.0, X))
                  IF (DONEG2) X = 1.0 - X
                  IF (DOLOGH.EQ.1) THEN
                     X = LOG10 (9.0 * X + 1.0)
                  ELSE IF (DOLOGH.EQ.2) THEN
                     X = 0.5 * LOG10 (99.0 * X + 1.0)
                  ELSE IF (DOLOGH.EQ.3) THEN
                     X = SQRT (X)
                     END IF
                  BUFFH(I) = X
 120              CONTINUE
               X = CATD(KDCRV+1) + (IY-1.) * CATR(KRCIC+1)
               CALL RFILL (NX, X, BUFFI)
               END IF

C                                       convert to RGB
            IF (OPTYPE.EQ.'LUT ') THEN
               CALL HILRGB (FIRST, F, DOSAT, NX, BUFFI, BUFFH, BUFFS,
     *            DOCIRC, BUFF1(IBIND1), BUFF2(IBIND2), BUFF3(IBIND3))
            ELSE
               CALL HI2RGB (F, DOSAT, NX, BUFFI, BUFFH, BUFFS, DOCIRC,
     *            BUFF1(IBIND1), BUFF2(IBIND2), BUFF3(IBIND3))
               END IF
C                                       get scale and blanks
            DO 130 IX = 1,NX
               I = IX - 1
               IF ((BUFFH(I).EQ.FBLANK) .OR.
     *            (BUFFI(I).EQ.FBLANK)) THEN
                  BUFF1(IBIND1+I) = FBLANK
                  BUFF2(IBIND2+I) = FBLANK
                  BUFF3(IBIND3+I) = FBLANK
                  ISBLNK = T
               ELSE
                  RMN = MIN (RMN, BUFF1(IBIND1+I))
                  RMX = MAX (RMX, BUFF1(IBIND1+I))
                  RMN = MIN (RMN, BUFF2(IBIND2+I))
                  RMX = MAX (RMX, BUFF2(IBIND2+I))
                  RMN = MIN (RMN, BUFF3(IBIND3+I))
                  RMX = MAX (RMX, BUFF3(IBIND3+I))
                  END IF
 130           CONTINUE
 150        CONTINUE
C                                       finish the writes
         CALL MDISK ('FINI', LUNO1, INDO1, BUFF1, IBIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('FINI', LUNO2, INDO2, BUFF2, IBIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('FINI', LUNO3, INDO3, BUFF3, IBIND3, IERR)
         IF (IERR.NE.0) GO TO 990
         FRW(NCFILE) = 1
C                                       close the files
         CALL ZCLOSE (LUNO1, INDO1, IERR)
         CALL ZCLOSE (LUNO2, INDO2, IERR)
         CALL ZCLOSE (LUNO3, INDO3, IERR)
C                                       history
C                                       update the header
         IF (RMX.LE.RMN) THEN
            MSGTXT = 'NO VALID PIXELS IN WEDGE FOUND: QUITTING'
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
            END IF
         CATR(KRDMN) = RMN
         CATR(KRDMX) = RMX
         CATR(KRBLK) = 0.0
         IF (ISBLNK) CATR(KRBLK) = FBLANK
C                                       HI file
         CALL HIINIT (2)
         CALL HISCOP (LUNO3, LUNO2, VOL1, VOLO2, SLOT1, SLOTO, CATBLK,
     *      BUFF1, SCRTCH, IERR)
         IF (IERR.GT.3) GO TO 200
C                                       TVHUI history: files
         CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQ1, VOL1, LUNO2, SCRTCH,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCO2 (TSKNAM, NAMIN2, CLSIN2, SEQ2, VOL2, LUNO2, SCRTCH,
     *   IERR)
         IF (IERR.NE.0) GO TO 190
C                                       TVHUI history: corners
         CALL COPY (4, CATBL4(IIWIN), IWIN)
         CALL COPY (5, CATBL4(IIDEP), IDEP)
         WRITE (LINE,1050) TSKNAM, 'BLC', IWIN(1), IWIN(2), IDEP,
     *      'Intensity'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (LINE,1050) TSKNAM, 'TRC', IWIN(3), IWIN(4), IDEP,
     *      'Intensity'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 190
         CALL COPY (4, CATBL2(IIWIN), IWIN)
         CALL COPY (5, CATBL2(IIDEP), IDEP)
         WRITE (LINE,1050) TSKNAM, 'BLC', IWIN(1), IWIN(2), IDEP, 'Hue'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 190
         WRITE (LINE,1050) TSKNAM, 'TRC', IWIN(3), IWIN(4), IDEP, 'Hue'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       TVHUI history: scaling
         PRANGE(1,2) = OFH / SLH
         PRANGE(2,2) = 1.0 / SLH + PRANGE(1,2)
         WRITE (LINE,1060) TSKNAM, PRANGE(1,2), PRANGE(2,2), 'Hue'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       TVHUI history: operation
         IF (OPTYPE.NE.'LUT ') OPTYPE = 'S=1 '
         IF (DOCIRC.GT.0.0) THEN
            MTYPE = ' T'
         ELSE
            MTYPE = ' F'
            END IF
         LINE = TSKNAM // 'OPTYPE ''' // OPTYPE // '''  DOCIRCLE =' //
     *      MTYPE
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
C                                       close HI
 190     IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1090) IERR
            CALL MSGWRT (6)
            END IF
         CALL HICLOS (LUNO2, T, SCRTCH, IERR)
C                                       Successful finish: image
 200     CALL CATIO ('UPDT', VOLO2, SLOTO, CATBLK, 'REST', SCRTCH, IERR)
         END IF
      GO TO 999
C
 990  IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT (A6,A,' = ',2(I5,','),4(I3,','),I3,5X,'/ For ',A,' image')
 1060 FORMAT (A6,'PIXRANGE = ',1PE12.4,',',1PE12.4,3X,'/ For ',A,
     *   ' image')
 1090 FORMAT ('TVHUOU WARNING: ERROR',I5,' FROM HISTORY ROUTINES')
 1990 FORMAT ('TVHIOU: ERROR',I5,' RETURNED BY ',A)
      END
