LOCAL INCLUDE 'PHUINT.INC'
      INTEGER   NOPT, MAXCHR
      PARAMETER (NOPT = 7)
      PARAMETER (MAXCHR = 19)
LOCAL END
LOCAL INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMIN*12, CLSIN*6, NAMIN2*12, CLSIN2*6, NAMOUT*12,
     *   CLSOUT*6, NAMOU2*12, CLSOU2*6
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMO(3),
     *   XCLSO(2), XNAMO2(3), XCLSO2(2)
      REAL      SEQIN, DSKIN, SEQIN2, DSKIN2, DOOUT, XOUTS, XOUTD,
     *   XOUTS2, XOUTD2, XSIZE(2), BLC(7), TRC(7), XXINC, XYINC, DOGRID,
     *   XTVCH, DOWDGE, DOCIRC, RANGE(2), DPARM(10)
      INTEGER   CATBLK(256), CATBL2(256), CATBL4(256), CATBL5(256)
      REAL      CATR(256), CATR2(256), CATR4(256), CATR5(256)
      DOUBLE PRECISION CATD(128), CATD2(128), CATD4(128), CATD5(128)
      HOLLERITH CATH(256), CATH2(256), CATH4(256), CATH5(256)
      LOGICAL   DO3CHN, FIRST, T, F, DOGRC, DOOPT
      REAL      PRANGE(2,2), ABLC(7,2), ATRC(7,2), SVGAMA, OBLC(7,2),
     *   OTRC(7,2), BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS),
     *   CURPOS(2,2)
      INTEGER   LUN1, LUN2, IND1, IND2, VOL1, VOL2, SLOT1, SLOT2, SEQ1,
     *   SEQ2, IUSER, ICHAN, HCHAN, JBUFSZ, VOLO, SLOTO, SEQO, IXWDGE,
     *   IYWDGE, NX, NY, INX, IDX, INY, IDY,NLI, NLH, NLS, XINC, YINC,
     *   LWINTV(4), IDWIN(4), GRX0,GRY0, GRMENU, DOLOGI, DOLOGH, SEQO2,
     *   VOLO2, FLIP(2)
      COMMON /MAPHDR/ CATBLK, CATBL2, CATBL4, CATBL5
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XNAMI2, XCLSI2,
     *   SEQIN2, DSKIN2, DOOUT, XNAMO, XCLSO, XOUTS, XOUTD, XNAMO2,
     *   XCLSO2, XOUTS2, XOUTD2, XSIZE, BLC, TRC, XXINC, XYINC, DOGRID,
     *   XTVCH, DOWDGE, DOCIRC, RANGE, DPARM
      COMMON /CHARPM/ NAMIN, CLSIN, NAMIN2, CLSIN2, NAMOUT, CLSOUT,
     *   NAMOU2, CLSOU2
      COMMON /BUFFRS/ BUFF1, BUFF2, BUFF3
      COMMON /HUINTP/ PRANGE, ABLC, ATRC, OBLC, OTRC, SVGAMA, DO3CHN,
     *   T, F, FIRST, DOLOGI, DOGRC, DOOPT, LUN1, LUN2, IND1, IND2,
     *   VOL1, VOL2, SLOT1, SLOT2, SEQ1, SEQ2, IUSER, JBUFSZ, VOLO,
     *   SLOTO, SEQO, IXWDGE, IYWDGE, NX, NY, INX, IDX, INY, IDY, NLI,
     *   NLH, NLS, XINC, YINC, LWINTV, IDWIN, GRX0, GRY0, GRMENU, SEQO2,
     *   VOLO2, DOLOGH, ICHAN, HCHAN, CURPOS, FLIP
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATBL2, CATR2, CATH2, CATD2)
      EQUIVALENCE (CATBL4, CATR4, CATH4, CATD4)
      EQUIVALENCE (CATBL5, CATR5, CATH5, CATD5)
C                                       OFM, LUTs, etc
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   WBUF(TVMLUT), LGLUT(TVMLUT), RED(TVMLUT), GREEN(TVMLUT),
     *   BLUE(TVMLUT), WLUT(TVMLUT)
      REAL      ALGOFM(TVMOFM), RWBUF(TVMLUT), RLGLUT(TVMLUT),
     *   RRED(TVMLUT), RGREEN(TVMLUT), RBLUE(TVMLUT), RWLUT(TVMLUT)
      COMMON /TVLUTS/ ALGOFM, RWBUF, RLGLUT, RRED, RGREEN, RBLUE, RWLUT,
     *   WBUF, LGLUT, RED, GREEN, BLUE, WLUT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
LOCAL END
      PROGRAM HUINT
C-----------------------------------------------------------------------
C! makes a TV image from images of hue and intensity
C# TV Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2014-2015
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
C   HUINT will display an image on the TV based on two input images, one
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
C      DOWEDGE  R      0 < DOWEDGE <= 1 -> grey wedge along top
C                      1.5 < DOWEDGE -> grey wedge along right edge
C                      2.5 < DOWEDGE both wedges
C      DOCIRCLE R      > 0 Plot coord grid rather than just ticks
C      PIXRANGE R(2)   the maximum and minimum values allowed for the
C                      map.  All other values will be clipped.  If
C                      IRANGE(1) .GE. IRANGE(2) then the map max and
C                      min will be used.
C      DPARM    R(10)  (1,2) pixrange of 2nd image
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   INPRMS, SCRTCH(MAXIMG), IRET, JERR
      CHARACTER PRGNAM*6
C
      INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
      INPRMS = 63
      PRGNAM = 'HUINT'
      CALL HUININ (PRGNAM, INPRMS, SCRTCH, IRET)
      IF (IRET.EQ.0) CALL HUEINT (SCRTCH, IRET)
C                                       close TV
      IF ((TVIND.GT.0) .AND. (TVIND2.GT.0)) THEN
         TVGAMA = SVGAMA
         CALL TVCLOS (SCRTCH, JERR)
         END IF
C                                       write output file
      IF ((IRET.EQ.0) .AND. (DOOUT.GT.0.0)) CALL HUINOU (SCRTCH, IRET)
C                                       close files
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE HUININ (PRGNAM, INPRMS, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   Routine to get parameters for HUINT
C   Inputs:
C      PRGNAM   C*6      Program name
C      INPRMS   I        Number of data parameters from AIPS
C   Outputs:
C       SCRTCH  I(256)   Scratch buffer
C       IRET    I        Return code 0=> OK, else just go to DIE
C   Task parameters are returned in common /INPARM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INPRMS, SCRTCH(*), IRET
C
      INCLUDE 'PHUINT.INC'
C
      INTEGER   IERR, IPOINT, IROUND, I, INC, J, II, IX, IY, LWIN(4),
     *   MAGF, TVCODE, MX, MY, IM, MXI, IX0, IY0, TYPE
      LOGICAL   REDUCE
      CHARACTER MTYPE*2, CHTMP*8, CHTMP1*8, SUBR*6
      DOUBLE PRECISION   DX
      REAL      X, A, TRC2, TRC3, OFFS, SLOPE
      INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
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
      CALL H2CHR (12, 1, XNAMO, NAMOUT)
      CALL H2CHR (6, 1, XCLSO, CLSOUT)
      CALL H2CHR (12, 1, XNAMO2, NAMOU2)
      CALL H2CHR (6, 1, XCLSO2, CLSOU2)
C
      LUN1 = 16
      LUN2 = 17
      SEQ1 = IROUND (SEQIN)
      SEQ2 = IROUND (SEQIN2)
      VOL1 = IROUND (DSKIN)
      VOL2 = IROUND (DSKIN2)
      IUSER = NLUSER
      ICHAN = IROUND (XTVCH)
C                                       Open intensity map file
      MTYPE = 'MA'
      SUBR = 'MAPOPN'
      CALL MAPOPN ('READ', VOL1, NAMIN, CLSIN, SEQ1, MTYPE, IUSER,
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
      ABLC(3,2) = TRC2
      ATRC(3,2) = TRC2
      CALL RCOPY (5, OBLC(3,1), OTRC(3,1))
      CALL RCOPY (7, OBLC, OBLC(1,2))
      CALL RCOPY (7, OTRC, OTRC(1,2))
      OBLC(3,2) = TRC2
      OTRC(3,2) = TRC2
C                                       Open HUE file
      REDUCE = F
      MTYPE = 'MA'
      CALL MAPOPN ('READ', VOL2, NAMIN2, CLSIN2, SEQ2, MTYPE, IUSER,
     *   LUN2, IND2, SLOT2, CATBL2, SCRTCH, IERR)
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
C                                       Image 2 plane selection
      DO 20 I = 3,7
         J = KINAX + I - 1
         IM = 2
         IF ((CATBL2(J).LE.1) .OR. (CATBL2(KIDIM).LT.I)) THEN
            ABLC(I,2) = 1
            OBLC(I,2) = 1
         ELSE
            ABLC(I,2) = MAX (1.0, ABLC(I,2))
            IF (ABLC(I,2)-0.01.GT.CATBL2(J)) GO TO 995
            OBLC(I,2) = MAX (1.0, OBLC(I,2))
         END IF
         ATRC(I,2) = ABLC(I,2)
         OTRC(I,2) = OBLC(I,2)
 20      CONTINUE
C                                       Set Map 2 corners
      DO 30 I = 1,2
         J = KRCRP + I - 1
         IF (DOGRID.LT.-1.5) THEN
            ABLC(I,2) = ABLC(I,1)
            ATRC(I,2) = ATRC(I,1)
            OBLC(I,2) = OBLC(I,1)
            OTRC(I,2) = OTRC(I,1)
         ELSE
            ABLC(I,2) = CATR2(J) - CATR(J) + ABLC(I,1)
            ATRC(I,2) = CATR2(J) - CATR(J) + ATRC(I,1)
            OBLC(I,2) = CATR2(J) - CATR(J) + OBLC(I,1)
            OTRC(I,2) = CATR2(J) - CATR(J) + OTRC(I,1)
            END IF
         J = I - 1
         IF (DOGRID.GT.-0.1) THEN
            IF (CATR(KRCIC+J).EQ.0.0) GO TO 45
            IM = 2
            IF (CATR2(KRCIC+J).EQ.0.0) GO TO 995
            DX = CATD(KDCRV+J) + (ABLC(I,1) - CATR(KRCRP+J)) *
     *         CATR(KRCIC+J)
            X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *         CATR2(KRCRP+J)
            ABLC(I,2) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-ABLC(I,2)).GT.0.2))
     *         GO TO 995
            ATRC(I,2) = ABLC(I,2) + ATRC(I,1) - ABLC(I,1)
            DX = CATD(KDCRV+J) + (OBLC(I,1) - CATR(KRCRP+J)) *
     *         CATR(KRCIC+J)
            X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *         CATR2(KRCRP+J)
            OBLC(I,2) = IROUND (X)
            IF ((DOGRID.GE.0.1) .AND. (ABS(X-OBLC(I,2)).GT.0.2))
     *         GO TO 995
            OTRC(I,2) = OBLC(I,2) + OTRC(I,1) - OBLC(I,1)
            END IF
C                                       smaller subimage needed?
         IF (ABLC(I,2).LT.1.0) THEN
            ABLC(I,1) = ABLC(I,1) + 1.0 - ABLC(I,2)
            ABLC(I,2) = 1.0
            REDUCE = T
            END IF
         IF (ATRC(I,2).GT.CATBL2(KINAX+J)) THEN
            ATRC(I,1) = ATRC(I,1) + CATBL2(KINAX+J) - ATRC(I,2)
            ATRC(I,2) = CATBL2(KINAX+J)
            REDUCE = T
            END IF
         IF (ABLC(I,1).GE.ATRC(I,1)) GO TO 995
         IF (ABLC(I,2).GE.ATRC(I,2)) GO TO 995
         IF (OBLC(I,2).LT.1.0) THEN
            OBLC(I,1) = OBLC(I,1) + 1.0 - OBLC(I,2)
            OBLC(I,2) = 1.0
            REDUCE = T
            END IF
         IF (OTRC(I,2).GT.CATBL2(KINAX+J)) THEN
            OTRC(I,1) = OTRC(I,1) + CATBL2(KINAX+J) - OTRC(I,2)
            OTRC(I,2) = CATBL2(KINAX+J)
            REDUCE = T
            END IF
         IF (OBLC(I,1).GE.OTRC(I,1)) GO TO 995
         IF (OBLC(I,2).GE.OTRC(I,2)) GO TO 995
 30      CONTINUE
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
            IF (CHTMP.NE.CHTMP1) GO TO 995
            IF (ABS(CATR(KRCIC+J)-CATR2(KRCIC+J)).GT.X) GO TO 995
            IF (ABS(CATR(KRCRT+J)-CATR2(KRCRT+J)).GT.1.) GO TO 995
 40         CONTINUE
         END IF
C                                       Corners reduced a little
 45   IF (REDUCE) THEN
         MSGTXT = 'Input images coincident on reduced subimage only'
         CALL MSGWRT (4)
         END IF
C                                       pix ranges
      CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), PRANGE(1,1))
      CALL RNGSET (DPARM, CATR2(KRDMX), CATR2(KRDMN), PRANGE(1,2))
C                                       open the TV
      IRET = 8
      SUBR = 'TVOPEN'
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       init TV
      DO3CHN = (TVIMPC.GT.0) .AND. (NGRAY.GE.2) .AND.
     *   (OFMINP+1.GE.2*(LUTOUT+1))
      IF (.NOT.DO3CHN) THEN
         MSGTXT = 'I DO NOT WORK ON NON-TRUECOLOR TVS'
         CALL MSGWRT (7)
         GO TO 999
         END IF
      IF ((ICHAN.LT.1) .OR. (ICHAN.GT.NGRAY)) ICHAN = 1
      IF (ICHAN+1.GT.NGRAY) ICHAN = NGRAY - 1
      HCHAN = ICHAN + 1
      CALL COPY (4, WINDTV, LWINTV)
      SVGAMA = TVGAMA
      CALL YHOLD ('ONNN', IERR)
      SUBR = 'YCINIT'
      CALL YCINIT (ICHAN, SCRTCH)
      IF (IERR.NE.0) GO TO 990
      CALL YCINIT (HCHAN, SCRTCH)
      IF (IERR.NE.0) GO TO 990
      SUBR = 'YSLECT'
      II = NGRAY + NGRAPH
      DO 50 I = 1,II
         CALL YSLECT ('OFFF', I, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
 50      CONTINUE
      CALL YSLECT ('ONNN', ICHAN, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YSLECT ('ONNN', HCHAN, 0, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      SUBR = 'YZERO'
      CALL YZERO (ICHAN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YZERO (HCHAN, IERR)
      IF (IERR.NE.0) GO TO 990
      TVCODE = 2 ** (ICHAN - 1) + 2 ** (HCHAN - 1)
      CALL YSCROL (TVCODE, 0, 0, T, IERR)
      SUBR = 'YSCROL'
      IF (IERR.NE.0) GO TO 990
C                                       anti-log OFM
      II = OFMINP + 1
      CALL RFILL (II, 0.0, ALGOFM)
      A = LOG10 (REAL(LUTOUT)) / LUTOUT
      SLOPE = 1.0 / (REAL(LUTOUT) ** 2)
      II = 2 * LUTOUT + 1
      DO 60 I = 1,II
         ALGOFM(I) = SLOPE * (10.0 ** (A * (I-1)))
60       CONTINUE
      SUBR = 'YOFM'
      TVGAMA = 1.0
      CALL YOFM ('WRIT', 7, T, ALGOFM, IERR)
      TVGAMA = SVGAMA
      IF (IERR.NE.0) GO TO 990
C                                       calculate log look up table
      A = LUTOUT / LOG10 (REAL(MAXINT))
      MXI = MAXINT + 1
      DO 65 I = 1,MAXINT
         RLGLUT(I+1) =  A * LOG10 (REAL(I))
         LGLUT(I+1) = RLGLUT(I+1) +  0.5
65       CONTINUE
      LGLUT(1) = 0
      RLGLUT(1) = 0.0
C                                       color filters
C                                       Red table: spectrum
      IF (DOCIRC.LE.0.0) THEN
         IX0 = 0.26 * MAXINT
         IX = 0.41 * MAXINT
         CALL STRLIN (0, 0, IX0, 0, RRED)
         CALL STRLIN (IX0, 0, IX, MAXINT, RRED)
         CALL STRLIN (IX, MAXINT, MAXINT, MAXINT, RRED)
C                                       Green table
         IY0 = 0.267 * MAXINT
         IX0 = 0.353 * MAXINT
         IX = 0.479 * MAXINT
         CALL STRLIN (0, IY0, IX0, MAXINT, RGREEN)
         CALL STRLIN (IX0, MAXINT, IX, MAXINT, RGREEN)
         IX0 = 0.981 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, 0, RGREEN)
         CALL STRLIN (IX0, 0, MAXINT, 0, RGREEN)
C                                       blue table
         IX0 = 0.063 * MAXINT
         IX = 0.334 * MAXINT
         CALL STRLIN (0, MAXINT, IX0, MAXINT, RBLUE)
         CALL STRLIN (IX0, MAXINT, IX, 0, RBLUE)
         CALL STRLIN (IX, 0, MAXINT, 0, RBLUE)
C                                       Circular: red table
      ELSE
         IX0 = 0.24 * MAXINT
         IX = 0.36 * MAXINT
         CALL STRLIN (0, 0, IX0, 0, RRED)
         CALL STRLIN (IX0, 0, IX, MAXINT, RRED)
         IX0 = 0.77 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, MAXINT, RRED)
         IY0 = 0.25 * MAXINT
         CALL STRLIN (IX0, MAXINT, MAXINT, IY0, RRED)
C                                       Green table
         IX0 = 0.16 * MAXINT
         IX = 0.37 * MAXINT
         CALL STRLIN (0, IY0, IX0, MAXINT, RGREEN)
         CALL STRLIN (IX0, MAXINT, IX, MAXINT, RGREEN)
         IX0 = 0.66 * MAXINT
         CALL STRLIN (IX, MAXINT, IX0, 0, RGREEN)
         CALL STRLIN (IX0, 0, MAXINT, 0, RGREEN)
C                                       Blue table
         IX0 = 0.13 * MAXINT
         IX = 0.286 * MAXINT
         CALL STRLIN (0, MAXINT, IX0, MAXINT, RBLUE)
         CALL STRLIN (IX0, MAXINT, IX, 0, RBLUE)
         IX0 = 0.57 * MAXINT
         CALL STRLIN (IX, 0, IX0, 0, RBLUE)
         IX = 0.8 * MAXINT
         CALL STRLIN (IX0, 0, IX, MAXINT, RBLUE)
         CALL STRLIN (IX, MAXINT, MAXINT, MAXINT, RBLUE)
         END IF
      RRED(1) = 0.0
      RGREEN(1) = 0.0
      RBLUE(1) = 0.0
C                                       Take log via lookup
      A = LUTOUT / LOG10 (REAL(MAXINT))
      DO 70 I = 2,MXI
         RED(I) = RRED(I) + 0.5
         GREEN(I) = RGREEN(I) + 0.5
         BLUE(I) = RBLUE(I) + 0.5
         RED(I) = LGLUT (RED(I) + 1)
         GREEN(I) = LGLUT (GREEN(I) + 1)
         BLUE(I) = LGLUT (BLUE(I) + 1)
 70      CONTINUE
      RED(1) = 0
      GREEN(1) = 0
      BLUE(1) = 0
C                                       init the LUTs
      CURPOS(1,1) = (WINDTV(3) + WINDTV(1) + 1) / 2
      CURPOS(2,1) = WINDTV(2) + (WINDTV(4) - WINDTV(2) + 1) / 3
      CURPOS(1,2) = CURPOS(1,1)
      CURPOS(2,2) = CURPOS(2,1)
      FLIP(1) = 1
      FLIP(2) = 1
      SUBR = 'HILUT'
      CALL HILUT (.FALSE., CURPOS(1,1), FLIP(1), IERR)
      IF (IERR.NE.0) GO TO 990
      CALL HILUT (.TRUE., CURPOS(1,2), FLIP(2), IERR)
      IF (IERR.NE.0) GO TO 990
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
         ATRC(1,1) = ABLC(1,1) + XINC * (MAXXTV(1) - 1 - IXWDGE)
         ATRC(1,2) = ABLC(1,2) + XINC * (MAXXTV(1) - 1 - IXWDGE)
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
         ATRC(2,1) = ABLC(2,1) + YINC * (MAXXTV(2) - 1 - IYWDGE)
         ATRC(2,2) = ABLC(2,2) + YINC * (MAXXTV(2) - 1 - IYWDGE)
         NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01 + IYWDGE
         END IF
      IDWIN(1) = 1
      IDWIN(2) = 1
      IDWIN(3) = NX
      IDWIN(4) = NY
      INX = NX
      INY = NY
      IDX = 1
      IDY = 1
C                                       fix up image catalog somehow
      CALL COPY (256, CATBLK, CATBL4)
      CATBLK(IIVOL) = VOL1
      CATBLK(IICNO) = SLOT1
      CATBL2(IIVOL) = VOL2
      CATBL2(IICNO) = SLOT2
      CATR(IRRAN) = PRANGE(1,1)
      CATR(IRRAN+1) = PRANGE(2,1)
      CATR2(IRRAN) = PRANGE(1,2)
      CATR2(IRRAN+1) = PRANGE(2,2)
      CALL CHR2H (2, 'HI', 1, CATH(IITRA))
      CALL CHR2H (2, 'HI', 1, CATH2(IITRA))
      DO 140 I = 1,2
         CATBLK(IIWIN+I-1) = IROUND (ABLC(I,1))
         CATBL2(IIWIN+I-1) = IROUND (ABLC(I,2))
         CATBLK(IIWIN+I+1) = IROUND (ATRC(I,1))
         CATBL2(IIWIN+I+1) = IROUND (ATRC(I,2))
 140     CONTINUE
      DO 145 I = 3,7
         CATBLK(IIDEP+I-3) = IROUND (ABLC(I,1))
         CATBL2(IIDEP+I-3) = IROUND (ABLC(I,2))
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
      CALL YCWRIT (ICHAN, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YCWRIT (HCHAN, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       load initial images
      DOLOGI = 0
      DOLOGH = 0
      TYPE = 0
      CALL HUINLD (TYPE, SCRTCH, IERR)
      SUBR = 'HUINLD'
      IF (IERR.NE.0) GO TO 990
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
      GO TO 999
C                                       Maps not coincident
 995  MSGTXT = 'INPUT IMAGES ARE NOT COINCIDENT.'
      CALL MSGWRT (7)
      WRITE (MSGTXT,1995) IM, I
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HUININ: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1990 FORMAT ('HUININ: ERROR',I5,' RETURNED FROM ROUTINE ',A)
 1995 FORMAT ('ERROR APPEARS TO BE ON IMAGE',I2,' AXIS',I2)
      END
      SUBROUTINE STRLIN (X1, Y1, X2, Y2, BUF)
C-----------------------------------------------------------------------
C   STRLIN interpolates a straight line from (X1,Y1) to (X2,Y2) into
C   BUF(X1+1) to BUF(X2+1).
C   Inputs:
C      X1    I          First X point (0-rel, i.e. >= 0)
C      Y1    I          First Y point
C      X2    I          Second X point (0-rel, >= X1)
C      Y2    I          Second Y point
C   Output:
C      BUF   R(>X2+1)   Output line in BUF(X1+1) to BUF(X2+1)
C-----------------------------------------------------------------------
      INTEGER   X1, Y1, X2, Y2
      REAL      BUF(*)
C
      INTEGER   NSTEPS, I
      REAL      INC
C-----------------------------------------------------------------------
C                                       Illegal direction
      IF (X2.GE.X1) THEN
C                                       First point
         BUF(X1+1) = Y1
         NSTEPS = X2 - X1
C                                       Interpolate
         IF (NSTEPS.GT.0) THEN
            INC = REAL(Y2-Y1) / REAL(NSTEPS)
            DO 10 I = 1,NSTEPS
               BUF(X1+I+1) = Y1 + I * INC
 10            CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE HILUT (HCHANG, RPOS, IFLIP, IERR)
C-----------------------------------------------------------------------
C   HILUT calculates new look up tables for hue/intensity display and
C   send them to the TV.
C   Inputs:
C      HCHANG  L       .TRUE. if hue LUTs are to be changed
C      RPOS    R(2)    Cursor position which determines the enhancement
C                      transformation
C      IFLIP   I       +1 or -1 for sign of transfer func. slope
C   Output:
C      IERR    I       Returns YLUT error code
C   Common: /AIPSCR/
C     Outputs:
C        BUFFER  I(TVMLUT)   Scratch buffer - returns the transfer
C                               function.
C        LUT     I(TVMLUT)   Scratch buffer
C   Inputs:
C      LOGLUT   I(TVMLUT)   Log look up table
C      RED      I(TVMLUT)   Look up table for red color gun func
C      GREEN    I(TVMLUT)   Look up table for green color gun func
C      BLUE     I(TVMLUT)   Look up table for blue color gun func
C-----------------------------------------------------------------------
      INTEGER   IFLIP, IERR
      LOGICAL   HCHANG
      REAL      RPOS(2)
C
      INCLUDE 'HUINT.INC'
      INTEGER   I, J, IROUND
      REAL      X, SLOPE, OFFSET, RVAL
      LOGICAL   WASYNC
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      X = ((RPOS(1) - WINDTV(1) + 1)/ (WINDTV(3) - WINDTV(1) + 1) - 0.5)
     *   * 2.0 * MAXINT
      SLOPE = (WINDTV(4)-WINDTV(2)) / (3. * MAX (1., RPOS(2)-WINDTV(2)))
     *   * IFLIP
      OFFSET = (1.0-X) * SLOPE + MAXINT * (1-IFLIP) / 2
      J = MAXINT + 1
      X = MAXINT
      DO 10 I = 1,J
         RVAL = (I-1)*SLOPE + OFFSET + 0.5
         RWBUF(I) = MAX (1.0, MIN (X, RVAL))
         WBUF(I) = IROUND (RWBUF(I))
 10      CONTINUE
      RWBUF(1) = 0.0
      WBUF(1) = 0
      WASYNC = ISYNCH.EQ.0
      IF (WASYNC) CALL YHOLD ('ONNN', I)
C                                       Change the intensity mapping
      IF (.NOT.HCHANG) THEN
         DO 20 I = 1,J
            WLUT(I) = LGLUT(WBUF(I)+1)
 20         CONTINUE
         CALL YLUT ('WRIT', ICHAN, 7, T, WLUT, IERR)
         GO TO 990
C                                       Change the hue mapping
C                                       Red
      ELSE
         DO 60 I = 1,J
            WLUT(I) = RED(WBUF(I)+1)
 60         CONTINUE
         CALL YLUT ('WRIT', HCHAN, 4, T, WLUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Green
         DO 70 I = 1,J
            WLUT(I) = GREEN(WBUF(I)+1)
 70         CONTINUE
         CALL YLUT ('WRIT', HCHAN, 2, T, WLUT, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Blue
         DO 80 I = 1,J
            WLUT(I) = BLUE(WBUF(I)+1)
 80         CONTINUE
         CALL YLUT ('WRIT', HCHAN, 1, T, WLUT, IERR)
         END IF
C
 990  IF (WASYNC) CALL YHOLD ('OFFF', I)
C
 999  RETURN
      END
      SUBROUTINE HUINLD (TYPE, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   HUINLD loads one or both images to the TV
C   Inputs:
C      TYPE     I          Type 1 intensity, 2 hue, else both
C      LX       I          x dimension of in-core arrays
C      LY       I          y dimension of in-core arrays
C   Outputs:
C      SCRTCH   I(1536)   TV scratch array
C      IRET     I         Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   TYPE, SCRTCH(*), IRET
C
      INTEGER   I, J, IBLK, HBLK, IERR, JJ, JX, JY, BIND1, BIND2, SY,
     *   SX, IX, IY, II, LL
      REAL      X, DI(4096), DH(4096)
      CHARACTER SUBR*6
      INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      CALL YHOLD ('ONNN', IERR)
C                                       init image windows
      JX = CATBLK(IIWIN+2) - CATBLK(IIWIN+0) + 1
      JY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), IBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), HBLK,
     *   IERR)
      IBLK = IBLK + 1
      HBLK = HBLK + 1
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1)
      SX = 0
      SY = 0
C                                       init the reads
      SUBR = 'MINIT'
      IF (TYPE.NE.2) THEN
         CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *      CATBLK(IIWIN), BUFF1, JBUFSZ, IBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (TYPE.NE.1) THEN
         CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *      CATBL2(IIWIN), BUFF2, JBUFSZ, HBLK, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      SY = 0
      DO 80 J = 1,JY
C                                       read data rows
         SUBR = 'MDISK'
         IF (TYPE.NE.2) THEN
            CALL MDISK ('READ', LUN1, IND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         IF (TYPE.NE.1) THEN
            CALL MDISK ('READ', LUN2, IND2, BUFF2, BIND2, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       scale data rows
            II = 0
            DO 65 I = 1,JX,XINC
               II = II + 1
               IF (TYPE.NE.2) THEN
                  X = BUFF1(BIND1+I-1)
                  IF (X.EQ.FBLANK) THEN
                     DI(II) = FBLANK
                  ELSE
                     X = (X - PRANGE(1,1)) / (PRANGE(2,1) - PRANGE(1,1))
                     DI(II) = MAX (0.0, MIN (1.0, X))
                     END IF
                  END IF
               IF (TYPE.NE.1) THEN
                  X = BUFF2(BIND2+I-1)
                  IF (X.EQ.FBLANK) THEN
                     DH(II) = FBLANK
                  ELSE
                     X = (X - PRANGE(1,2)) / (PRANGE(2,2) - PRANGE(1,2))
                     DH(II) = MAX (0.0, MIN (1.0, X))
                     END IF
                  END IF
 65            CONTINUE
C                                       add step wedge to right
            JJ = (J - 1) / YINC + 1
            IF (IXWDGE.GT.1) THEN
               X = REAL (JJ - 1) / REAL ((JY - 1) / YINC)
               CALL RFILL (IXWDGE, X, DI(II+1))
               DO 70 I = 1,IXWDGE
                  II = II + 1
                  DH(II) = (I - 1.0) / (IXWDGE - 1.0)
 70               CONTINUE
               END IF
            SUBR = 'YIMGIO'
            CALL HILOAD (TYPE, NX, IX, IY, DI, DH, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            IY = IY + 1
            END IF
 80      CONTINUE
C                                       top step wedge
      IF (IYWDGE.GT.1) THEN
         II = NX
         X = 1.0 / REAL (II - 1)
         DO 85 I = 1,NX
            LL = MOD (I - 1, II)
            DI(I) = LL* X
 85         CONTINUE
         DO 95 J = 1,IYWDGE
            X = (J - 1.0) / (IYWDGE - 1.0)
            CALL RFILL (NX, X, DH)
            JJ = JJ + 1
            SUBR = 'YIMGIO'
            CALL HILOAD (TYPE, NX, IX, IY, DI, DH, SCRTCH, IERR)
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
 1990 FORMAT ('HUINLD: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE HILOAD (TYPE, NP, IX, IY, DI, DH, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   HILOAD does the scaling, clipping of an INT/HUE pair for a row,
C   converts to RGB, scales and loads the row to 3 channels on the TV
C   Inputs:
C      TYPE     I       1 load ICHAN, 2 load HCHAN, else both
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
      INTEGER   TYPE, NP, IX, IY, SCRTCH(*), IERR
      REAL      DI(NP), DH(NP)
C
      INTEGER   I
      REAL      S, XI(4096), XH(4096)
      INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       scale
      DO 10 I = 1,NP
         IF (TYPE.NE.2) THEN
            XI(I) = DI(I)
            IF (XI(I).NE.FBLANK) THEN
               XI(I) = MAX (0.0, MIN (1.0, XI(I)))
               IF (DOLOGI.EQ.1) THEN
                  XI(I) = LOG10 (9.0 * XI(I) + 1.0)
               ELSE IF (DOLOGI.EQ.2) THEN
                  XI(I) = 0.5 * LOG10 (99.0 * XI(I) + 1.0)
               ELSE IF (DOLOGI.EQ.3) THEN
                  XI(I) = SQRT (XI(I))
                  END IF
               XI(I) = MAX (0.0, MIN (1.0, XI(I)))
               END IF
            END IF
         IF (TYPE.NE.1) THEN
            XH(I) = DH(I)
            IF (XH(I).NE.FBLANK) THEN
               IF (DOLOGH.EQ.1) THEN
                  XH(I) = LOG10 (9.0 * XH(I) + 1.0)
               ELSE IF (DOLOGH.EQ.2) THEN
                  XH(I) = 0.5 * LOG10 (99.0 * XH(I) + 1.0)
               ELSE IF (DOLOGH.EQ.3) THEN
                  XH(I) = SQRT (XH(I))
                  END IF
               XH(I) = MAX (0.0, MIN (1.0, XH(I)))
               END IF
            END IF
 10      CONTINUE
C                                       scale to TV
      S = MAXINT - 1
      IF (TYPE.NE.2) THEN
         DO 30 I = 1,NP
            IF (DI(I).EQ.FBLANK) THEN
               SCRTCH(I) = 0
            ELSE
               SCRTCH(I) = XI(I) * S + 1.5
               END IF
 30         CONTINUE
         I = ICHAN
         CALL YIMGIO ('WRIT', I, IX, IY, 0, NP, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      IF (TYPE.NE.1) THEN
         DO 40 I = 1,NP
            IF (DH(I).EQ.FBLANK) THEN
               SCRTCH(I) = 0
            ELSE
               SCRTCH(I) = XH(I) * S + 1.5
               END IF
 40         CONTINUE
         I = HCHAN
         CALL YIMGIO ('WRIT', I, IX, IY, 0, NP, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE HUEINT (SCRTCH, IRET)
C-----------------------------------------------------------------------
C   HUEINT implements the interactive selection and execution of the
C   various display and interaction options of HUINT.
C   Inputs:
C   Outputs:
C      IRET     I         error return
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IRET
C
      INCLUDE 'PHUINT.INC'
C
      INTEGER   NOPTS, MAGF, LWIN(4), I, II, JJ, IX, IY, TYPE, IERR,
     *   IBUT, GRCS(2), CHS, NTITLE, TOPSEP, SIDSEP, TIMLIM
      LOGICAL   MENUOK, WINDOK, DOIT, LEAVE(NOPT), LEAVES(NOPT)
      REAL      X, OFFS
      CHARACTER OPTION(NOPT)*20, SUBR*6, ONLOG(4)*20, ISHELP*8,
     *   CHOICS(NOPT)*20, TITLE*8, HULOG(4)*20
      INCLUDE 'HUINT.INC'
      DATA OPTION /'ENHANCE INTENSITY', 'ENHANCE HUE',
     *   'USE LOG(INTENSITY)', 'USE LOG(HUE)', ' ', 'EXIT', 'ABORT'/
      DATA ONLOG /'USE LOG(INTENSITY)','USE ELOG(INTENSITY)',
     *   'USE SQRT(INTENSITY)', 'USE LIN(INTENSITY)'/
      DATA HULOG /'USE LOG(HUE)','USE ELOG(HUE)',
     *   'USE SQRT(HUE)', 'USE LIN(HUE)'/
      DATA LEAVE /2*.FALSE., 2*.TRUE., 3*.FALSE./
C-----------------------------------------------------------------------
C                                        Initialize
      WINDOK = F
      MENUOK = F
      GRMENU = 1 + NGRAY
      NOPTS = 0
      DO 10 I = 1,NOPT
         NOPTS = NOPTS + 1
         CHOICS(NOPTS) = OPTION(I)
         LEAVES(NOPTS) = LEAVE(I)
 10      CONTINUE
C                                       all loads done
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
C                                       Enhance subimage
      IF (CHOICS(CHS)(:8).EQ.'ENHANCE ') THEN
         JJ = 0
         IF (CHOICS(CHS).EQ.'ENHANCE INTENSITY') JJ = 1
         IF (CHOICS(CHS).EQ.'ENHANCE HUE') JJ = 2
         IF (JJ.GT.0) THEN
            CALL HUINSI (JJ, FLIP(JJ), CURPOS(1,JJ), IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       abort
      ELSE IF (CHOICS(CHS).EQ.'ABORT') THEN
         IRET = 2
C                                       null
      ELSE IF (CHOICS(CHS).EQ.' ') THEN
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
            TYPE = 1
         ELSE IF ((CHOICS(CHS).EQ.HULOG(1)) .OR.
     *      (CHOICS(CHS).EQ.HULOG(2)) .OR. (CHOICS(CHS).EQ.HULOG(3)).OR.
     *      (CHOICS(CHS).EQ.HULOG(4))) THEN
            DOIT = T
            DOLOGH = MOD (DOLOGH+1,4)
            CHOICS(CHS) = HULOG(DOLOGH+1)
            MENUOK = F
            TYPE = 2
            END IF
C                                       do load function
         IF (DOIT) THEN
C                                       simple load
            CALL HUINLD (TYPE, SCRTCH, IERR)
            SUBR = 'HUINLD'
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
      IF ((CHOICS(CHS).NE.'EXIT') .AND. (CHOICS(CHS).NE.'ABORT'))
     *   GO TO 100
      IERR = 0
C
 990  IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('HUEINT: ERROR',I5,' RETURNED BY ROUTINE ',A)
      END
      SUBROUTINE HUINSI (LJ, IFLIP, PPOS, IRET)
C-----------------------------------------------------------------------
C   HUIN1S implements single-TV channel enhancement of one of the HSI
C   components or 3-channel.
C   Inputs:
C      LJ       I          Component number (I, H = 1,2)
C   In/out:
C      FLIP     I          Sign of slope of enhancement
C      PPOS     R(2)       Cursor position last used
C   Outputs:
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   LJ, IFLIP, IRET
      REAL      PPOS(2)
C
      INTEGER   IERR, IBUT, QUAD, ITW(3)
      LOGICAL   DOIT, HCHANG
      REAL      RPOS(2), F0, POS0(2)
      CHARACTER SUBR*6
      INCLUDE 'HUINT.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      HCHANG = LJ.EQ.2
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
         END IF
      CALL MSGWRT (1)
C                                       read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF ((DOIT) .AND. (IBUT.LT.8)) THEN
         IF (IBUT.EQ.1) IFLIP = -IFLIP
         IF ((IBUT.EQ.2) .OR. (IBUT.EQ.3)) THEN
            RPOS(1) = POS0(1)
            RPOS(2) = POS0(2)
            IFLIP = 1
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            SUBR = 'YCURSE'
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       change LUTs
         CALL HILUT (HCHANG, RPOS, IFLIP, IERR)
         SUBR = 'HILUT'
         IF (IERR.NE.0) GO TO 990
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
 1990 FORMAT ('HUINSI: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE HUINOU (SCRTCH, IRET)
C-----------------------------------------------------------------------
C   HUINOU writes out the image with an RGB axis developed and filled
C   using the hue scaling prevously established.
C   Outputs:
C      SCRTCH   I(*)   Scratch buffer
C      IRET     I      Return code
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IRET
C
      INTEGER   I, LUNO1, LUNO2, LUNO3, INDO1, INDO2, INDO3, IWIN(4),
     *   IDEP(5), IBLI, IBLH, IBL1, IBL2, IBL3, IBINDI, IBINDH, IBIND1,
     *   IBIND2, IBIND3, IX, IY, IERR, IROUND, IDO, J, ICNT
      CHARACTER SUBR*6, PHNAME*48, LINE*72, FUNCTY(4)*2
      LOGICAL   ISBLNK
      INCLUDE 'HUINT.INC'
      REAL      BUFFI(MABFSS), BUFFH(MABFSS), SLH, OFH, RMX, RMN, SLI,
     *   OFI, X, DI, DH, XI, XH, S, A, XR, XG, XB
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA FUNCTY /'LI','LG','L2','SQ'/
C-----------------------------------------------------------------------
      IRET = 0
      FIRST = T
      ISBLNK = F
      RMX = -1.0E20
      RMN = -RMX
      ICNT = ICNT + 1
C                                       transfer functions
      X = ((CURPOS(1,1)-WINDTV(1)+1) / (WINDTV(3)-WINDTV(1)+1.) - 0.5)
     *   * 2.0
      SLI = (WINDTV(4)-WINDTV(2)) / (3.*MAX(1.,CURPOS(2,1)-WINDTV(2)))
     *   * FLIP(1)
      OFI = (1.0/MAXINT - X) * SLI + (1-FLIP(1)) / 2
      X = ((CURPOS(1,2)-WINDTV(1)+1) / (WINDTV(3)-WINDTV(1)+1.) - 0.5)
     *   * 2.0
      SLH = (WINDTV(4)-WINDTV(2)) / (3.*MAX(1.,CURPOS(2,2)-WINDTV(2)))
     *   * FLIP(2)
      OFH = (1.0/MAXINT - X) * SLH + (1-FLIP(2)) / 2
C                                       re-arrange headers
      CALL COPY (256, CATBLK, SCRTCH)
      CALL COPY (256, CATBL4, CATBLK)
      CALL COPY (256, SCRTCH, CATBL4)
C                                       Output windows
      CATBL4(IIWIN) = IROUND (OBLC(1,1))
      CATBL2(IIWIN) = IROUND (OBLC(1,2))
      CATBL4(IIWIN+1) = IROUND (OBLC(2,1))
      CATBL2(IIWIN+1) = IROUND (OBLC(2,2))
      CATBL4(IIWIN+2) = IROUND (OTRC(1,1))
      CATBL2(IIWIN+2) = IROUND (OTRC(1,2))
      CATBL4(IIWIN+3) = IROUND (OTRC(2,1))
      CATBL2(IIWIN+3) = IROUND (OTRC(2,2))
C                                       Copy any header keywords
C                                       Allow failure
      CALL KEYCOP (VOL1, SLOT1, VOLO, SLOTO, IERR)
C                                       Open new file.
      LUNO1 = LUN2 + 1
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
      IDEP(1) = 1
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
      CALL MINIT ('WRIT', LUNO1, INDO1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF1, JBUFSZ, IBL1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('WRIT', LUNO2, INDO2, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF2, JBUFSZ, IBL2, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('WRIT', LUNO3, INDO3, CATBLK(KINAX), CATBLK(KINAX+1),
     *   IWIN, BUFF3, JBUFSZ, IBL3, IERR)
      IF (IERR.NE.0) GO TO 990
      A = LOG10 (REAL(LUTOUT)) / REAL (LUTOUT)
      S = 1.0 / (REAL(LUTOUT) ** 2)
      SUBR = 'MDISK'
      DO 50 IY = 1,NY
C                                       do reads, point for writes
         CALL MDISK ('READ', LUN1, IND1, BUFFI, IBINDI, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFFH, IBINDH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('WRIT', LUNO1, INDO1, BUFF1, IBIND1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('WRIT', LUNO2, INDO2, BUFF2, IBIND2, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('WRIT', LUNO3, INDO3, BUFF3, IBIND3, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       compute image row
         DO 30 IX = 1,NX
            I = IX - 1
C                                       blanked
            IF ((BUFFH(IBINDH+I).EQ.FBLANK) .OR.
     *         (BUFFI(IBINDI+I).EQ.FBLANK)) THEN
               BUFF1(IBIND1+I) = FBLANK
               BUFF2(IBIND2+I) = FBLANK
               BUFF3(IBIND3+I) = FBLANK
               ISBLNK = T
               ICNT = ICNT + 1
C                                       good
            ELSE
C                                       scale I
               DI = (BUFFI(IBINDI+I) - PRANGE(1,1)) /
     *            (PRANGE(2,1) - PRANGE(1,1))
               DI = MAX (0.0, MIN (1.0, DI))
               IF (DOLOGI.EQ.1) THEN
                  DI = LOG10 (9.0 * DI + 1.0)
               ELSE IF (DOLOGI.EQ.2) THEN
                  DI = 0.5 * LOG10 (99.0 * DI + 1.0)
               ELSE IF (DOLOGI.EQ.3) THEN
                  DI = SQRT (DI)
                  END IF
               DI = SLI * DI + OFI
               DI = MAX (0.0, MIN (1.0, DI))
               XI = (MAXINT - 1.0) * DI + 1.0
C                                       scale hue
               DH = (BUFFH(IBINDH+I) - PRANGE(1,2)) /
     *            (PRANGE(2,2) - PRANGE(1,2))
               DH = MAX (0.0, MIN (1.0, DH))
               IF (DOLOGH.EQ.1) THEN
                  DH = LOG10 (9.0 * DH + 1.0)
               ELSE IF (DOLOGH.EQ.2) THEN
                  DH = 0.5 * LOG10 (99.0 * DH + 1.0)
               ELSE IF (DOLOGH.EQ.3) THEN
                  DH = SQRT (DH)
                  END IF
               DH = SLH * DH + OFH
               DH = MAX (0.0, MIN (1.0, DH))
               XH = (MAXINT - 1.0) * DH + 1.0
C                                       log lookups
               IF (XI.GT.0.0) THEN
                  XI = LOG10 (XI) / A
               ELSE
                  XI = 0.0
                  END IF
               J = XH + 1.0
               XR = RRED(J) + (XH+1.0-J) * (RRED(J+1) - RRED(J))
               XG = RGREEN(J) + (XH+1.0-J) * (RGREEN(J+1) - RGREEN(J))
               XB = RBLUE(J) + (XH+1.0-J) * (RBLUE(J+1) - RBLUE(J))
               IF (XR.GT.0.0) THEN
                  XR = LOG10 (XR) / A
               ELSE
                  XR = 0.0
                  END IF
               IF (XG.GT.0.0) THEN
                  XG = LOG10 (XG) / A
               ELSE
                  XG = 0.0
                  END IF
               IF (XB.GT.0.0) THEN
                  XB = LOG10 (XB) / A
               ELSE
                  XB = 0.0
                  END IF
               BUFF1(IBIND1+I) = S * (10.0 ** (A * (XR+XI)))
               BUFF2(IBIND2+I) = S * (10.0 ** (A * (XG+XI)))
               BUFF3(IBIND3+I) = S * (10.0 ** (A * (XB+XI)))
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
C                                       HUINT history: files
      CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQ1, VOL1, LUNO2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
      CALL HENCO2 (TSKNAM, NAMIN2, CLSIN2, SEQ2, VOL2, LUNO2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 90
C                                       HUINT history: corners
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
C                                       HUINT history: scaling
      WRITE (LINE,1060) TSKNAM, PRANGE(1,1), PRANGE(2,1), 'Intensity'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      WRITE (LINE,1061) TSKNAM, FUNCTY(DOLOGI+1), 'Intensity'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      WRITE (LINE,1060) TSKNAM, PRANGE(1,2), PRANGE(2,2), 'Hue'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
      WRITE (LINE,1061) TSKNAM, FUNCTY(DOLOGH+1), 'Hue'
      CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 90
C                                       close HI
 90   IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         END IF
      CALL HICLOS (LUNO2, T, SCRTCH, IERR)
C                                       Successful finish: image
 100  CALL CATIO ('UPDT', VOLO, SLOTO, CATBLK, 'REST', SCRTCH, IERR)
      FRW(NCFILE) = 1
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
         CATD(KDCRV+J) = PRANGE(1,1)
         CATR(KRCRP+J) = 1.0
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
         LUNO1 = LUN2 + 1
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
C                                       scale hue
               DH = (IY - 1.0) / (CATBLK(KINAX+1) - 1.0)
               DH = MAX (0.0, MIN (1.0, DH))
               IF (DOLOGH.EQ.1) THEN
                  DH = LOG10 (9.0 * DH + 1.0)
               ELSE IF (DOLOGH.EQ.2) THEN
                  DH = 0.5 * LOG10 (99.0 * DH + 1.0)
               ELSE IF (DOLOGH.EQ.3) THEN
                  DH = SQRT (DH)
                  END IF
               DH = SLH * DH + OFH
               DH = MAX (0.0, MIN (1.0, DH))
               XH = (MAXINT - 1.0) * DH + 1.0
               J = XH + 1.0
               XR = RRED(J) + (XH+1.0-J) * (RRED(J+1) - RRED(J))
               XG = RGREEN(J) + (XH+1.0-J) * (RGREEN(J+1) - RGREEN(J))
               XB = RBLUE(J) + (XH+1.0-J) * (RBLUE(J+1) - RBLUE(J))
               IF (XR.GT.0.0) THEN
                  XR = LOG10 (XR) / A
               ELSE
                  XR = 0.0
                  END IF
               IF (XG.GT.0.0) THEN
                  XG = LOG10 (XG) / A
               ELSE
                  XG = 0.0
                  END IF
               IF (XB.GT.0.0) THEN
                  XB = LOG10 (XB) / A
               ELSE
                  XB = 0.0
                  END IF
               DO 110 IX = 1,NX
C                                       scale I
                  DI = (IX - 1.0) / (CATBLK(KINAX) - 1.0)
                  DI = MAX (0.0, MIN (1.0, DI))
                  IF (DOLOGI.EQ.1) THEN
                     DI = LOG10 (9.0 * DI + 1.0)
                  ELSE IF (DOLOGI.EQ.2) THEN
                     DI = 0.5 * LOG10 (99.0 * DI + 1.0)
                  ELSE IF (DOLOGI.EQ.3) THEN
                     DI = SQRT (DI)
                     END IF
                  DI = SLI * DI + OFI
                  DI = MAX (0.0, MIN (1.0, DI))
                  XI = (MAXINT - 1.0) * DI + 1.0
C                                       log lookups
                  IF (XI.GT.0.0) THEN
                     XI = LOG10 (XI) / A
                  ELSE
                     XI = 0.0
                     END IF
                  I = IX - 1
                  BUFF1(IBIND1+I) = S * (10.0 ** (A * (XR+XI)))
                  BUFF2(IBIND2+I) = S * (10.0 ** (A * (XG+XI)))
                  BUFF3(IBIND3+I) = S * (10.0 ** (A * (XB+XI)))
 110              CONTINUE
C                                       set row for H/I
            ELSE
C                                       scale I
               DI = (IY - 1.0) / (CATBLK(KINAX+1) - 1.0)
               DI = MAX (0.0, MIN (1.0, DI))
               IF (DOLOGI.EQ.1) THEN
                  DI = LOG10 (9.0 * DI + 1.0)
               ELSE IF (DOLOGI.EQ.2) THEN
                  DI = 0.5 * LOG10 (99.0 * DI + 1.0)
               ELSE IF (DOLOGI.EQ.3) THEN
                  DI = SQRT (DI)
                  END IF
               DI = SLI * DI + OFI
               DI = MAX (0.0, MIN (1.0, DI))
               XI = (MAXINT - 1.0) * DI + 1.0
C                                       log lookups
               IF (XI.GT.0.0) THEN
                  XI = LOG10 (XI) / A
               ELSE
                  XI = 0.0
                  END IF
C                                       hue
               DO 120 IX = 1,NX
                  DH = (IX - 1.0) / (CATBLK(KINAX) - 1.0)
                  DH = MAX (0.0, MIN (1.0, DH))
                  IF (DOLOGH.EQ.1) THEN
                     DH = LOG10 (9.0 * DH + 1.0)
                  ELSE IF (DOLOGH.EQ.2) THEN
                     DH = 0.5 * LOG10 (99.0 * DH + 1.0)
                  ELSE IF (DOLOGH.EQ.3) THEN
                     DH = SQRT (DH)
                     END IF
                  DH = SLH * DH + OFH
                  DH = MAX (0.0, MIN (1.0, DH))
                  XH = (MAXINT - 1.0) * DH + 1.0
                  J = XH + 1.0
                  XR = RRED(J) + (XH+1.0-J) * (RRED(J+1) - RRED(J))
                  XG = RGREEN(J) + (XH+1.0-J) * (RGREEN(J+1)-RGREEN(J))
                  XB = RBLUE(J) + (XH+1.0-J) * (RBLUE(J+1) - RBLUE(J))
                  IF (XR.GT.0.0) THEN
                     XR = LOG10 (XR) / A
                  ELSE
                     XR = 0.0
                     END IF
                  IF (XG.GT.0.0) THEN
                     XG = LOG10 (XG) / A
                  ELSE
                     XG = 0.0
                     END IF
                  IF (XB.GT.0.0) THEN
                     XB = LOG10 (XB) / A
                  ELSE
                     XB = 0.0
                     END IF
                  I = IX - 1
                  BUFF1(IBIND1+I) = S * (10.0 ** (A * (XR+XI)))
                  BUFF2(IBIND2+I) = S * (10.0 ** (A * (XG+XI)))
                  BUFF3(IBIND3+I) = S * (10.0 ** (A * (XB+XI)))
 120              CONTINUE
               END IF
C                                       get scale
            DO 130 IX = 1,NX
               I = IX - 1
               IF ((BUFF1(IBIND1+I).EQ.FBLANK) .OR.
     *            (BUFF2(IBIND2+I).EQ.FBLANK) .OR.
     *            (BUFF3(IBIND3+I).EQ.FBLANK)) THEN
                  MSGTXT = 'WE ARE HERE!'
                  ISBLNK = .TRUE.
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
C                                       HUINT history: files
         CALL HENCO1 (TSKNAM, NAMIN, CLSIN, SEQ1, VOL1, LUNO2, SCRTCH,
     *      IERR)
         IF (IERR.NE.0) GO TO 190
         CALL HENCO2 (TSKNAM, NAMIN2, CLSIN2, SEQ2, VOL2, LUNO2, SCRTCH,
     *   IERR)
         IF (IERR.NE.0) GO TO 190
C                                       HUINT history: corners
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
C                                       HUINT history: scaling
         WRITE (LINE,1060) TSKNAM, PRANGE(1,1), PRANGE(2,1), 'Intensity'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1061) TSKNAM, FUNCTY(DOLOGI+1), 'Intensity'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1060) TSKNAM, PRANGE(1,2), PRANGE(2,2), 'Hue'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1061) TSKNAM, FUNCTY(DOLOGH+1), 'Hue'
         CALL HIADD (LUNO2, LINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 90
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
 1061 FORMAT (A6,'FUNCTYPE = ''',A2,'''  /  For ',A)
 1090 FORMAT ('HUINOU WARNING: ERROR',I5,' FROM HISTORY ROUTINES')
 1990 FORMAT ('HUINOU: ERROR',I5,' RETURNED BY ',A)
      END
