LOCAL INCLUDE 'PTVRGB.INC'
      INTEGER   NOPT, MAXCHR
      PARAMETER (NOPT = 13)
      PARAMETER (MAXCHR = 19)
LOCAL END
LOCAL INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER NAMIN*12, CLSIN*6, NAMIN2*12, CLSIN2*6, NAMIN3*12,
     *   CLSIN3*6, OUTFIL*48, REASON*40, TVFUNC(3)*2
      HOLLERITH XNAMIN(3), XCLSIN(2), XNAMI2(3), XCLSI2(2), XNAMI3(3),
     *   XCLSI3(2), XOUFIL(12), XREASN(6)
      REAL      SEQIN, DSKIN, SEQIN2, DSKIN2, SEQIN3, DSKIN3, TBLC(7),
     *   TTRC(7), XXINC, XYINC, DOGRID, XTVCH, RANGE(2), DPARM(10),
     *   DOOUTP, BLC(7), TRC(7), XCOPYS, RGBGAM(3)
      INTEGER   CATBLK(256), CATBL2(256), CATBL3(256)
      REAL      CATR(256), CATR2(256), CATR3(256)
      DOUBLE PRECISION    CATD(128), CATD2(128), CATD3(256)
      HOLLERITH CATH(256), CATH2(256), CATH3(256)
      LOGICAL   DO3CHN, ISRGB, T, F, DOGRC
      REAL      PRANGE(2,3), ABLC(7,3), ATRC(7,3), OBLC(7,3), OTRC(7,3),
     *   SVGAMA, BUFF1(MABFSS), BUFF2(MABFSS), BUFF3(MABFSS), SC(2,4),
     *   DSC(2,4), FSC(2,4), ARANGE(2,3)
      INTEGER   LUN1, LUN2, LUN3, IND1, IND2, IND3, VOL1, VOL2, VOL3,
     *   SLOT1, SLOT2, SLOT3, SEQ1, SEQ2, SEQ3, IUSER, TVCH, JBUFSZ,
     *   NX, NY, NLB, NLG, NLR, XINC, YINC, IDX, IDY, IDWIN(4), INX,
     *   INY, LWINTV(4), ITVCH, GRCURS, GRX0, GRY0, GRMENU, GRWIND,
     *   COPIES, IFUNC(3)
      COMMON /MAPHDR/ CATBLK, CATBL2, CATBL3
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XNAMI2, XCLSI2,
     *   SEQIN2, DSKIN2, XNAMI3, XCLSI3, SEQIN3, DSKIN3, TBLC, TTRC,
     *   XXINC, XYINC, DOGRID, XTVCH, RANGE, DPARM, DOOUTP, BLC,TRC,
     *   XOUFIL, XCOPYS, RGBGAM, XREASN
      COMMON /CHARPM/ NAMIN, CLSIN, NAMIN2, CLSIN2, NAMIN3, CLSIN3,
     *   OUTFIL, REASON, TVFUNC
      COMMON /BUFFRS/ BUFF1, BUFF2, BUFF3
      COMMON /TVRGBP/ PRANGE, ABLC, ATRC, OBLC, OTRC, SC, DSC, FSC,
     *   SVGAMA, DO3CHN, ISRGB, T, F, DOGRC, LUN1, LUN2, LUN3, IND1,
     *   IND2, IND3, VOL1, VOL2, VOL3, SEQ1, SEQ2, SEQ3, SLOT1, SLOT2,
     *   SLOT3, TVCH, JBUFSZ, NX, NY, NLB, NLG, NLR, IUSER, XINC, YINC,
     *   IDX, IDY, IDWIN, INX, INY, LWINTV, ITVCH, GRCURS, GRX0, GRY0,
     *   GRMENU, GRWIND, ARANGE, COPIES, IFUNC
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATBL2, CATR2, CATH2, CATD2)
      EQUIVALENCE (CATBL3, CATR3, CATH3, CATD3)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
LOCAL END
      PROGRAM TVRGB
C-----------------------------------------------------------------------
C! makes a TV image from true-color (RGB) image
C# TV Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2002-2004, 2006, 2008-2009,
C;  Copyright (C) 2013-2015
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
C   TVRGB will display an image on the TV based on input images giving
C   separate red, green, and blue pictures.  It allows for interactive
C   scaling of the images.
C   INPUTS:   (from AIPS)
C      USERID   R      user number, 0 means use logon user number,
C                      32000 means any user can be accessed.
C      INNAME   R(3)   name of RGB/redimage.
C      INCLASS  R(2)   class of RGB/red image.
C      INSEQ    R      sequence number of RGB/red image.
C      INDISK   R      disk volume number. 0 means try all.
C      IN2NAME  R(3)   name of green image.
C      IN2CLASS R(2)   class of green image.
C      IN2SEQ   R      sequence number of green image.
C      IN2DISK  R      disk volume number. 0 means try all.
C      IN3NAME  R(3)   name of blue image.
C      IN3CLASS R(2)   class of blue image.
C      IN3SEQ   R      sequence number of blue image.
C      IN3DISK  R      disk volume number. 0 means try all.
C      TBLC      R(7)   the coordinate in the input file to become the
C                      left hand coordinate (1,1) of the contour plot.
C                      TBLC(1) is the X coordinate and TBLC(2) is the Y
C                      coordinate.  The first coordinate in the input
C                      image is (1,1).
C      TTRC      R(7)   the coordinate in the input file to become the
C                      top right hand corner of the plot.
C      DOALIGN  R      >= 0 => 2nd/3rd image must align with 1st
C      TVCHAN   R      Desired TV channel
C      PIXRANGE R(2)   the maximum and minimum values allowed for the
C                      map.  All other values will be clipped.  If
C                      IRANGE(1) .GE. IRANGE(2) then the map max and
C                      min will be used.
C      DPARM    R(10)  (1,2) pixrange of 2nd image, default = PIXRANGE
C                      (3,4) pixrange of 3rd image, default = PIXRANGE
C                      (5) # levels per R,G,B   < 8 or > 40 => 32.
C                      (6) # pixels in subimage <= 86000.  0 => 86000.
C                      (7) # levels/r,g,b during interaction <= (5)
C                      (8) > 0 => use 1-channel method on TVs capable of
C                          full RGB
C      DOOUTPUT R      > 0 => write RGB PostScript
C      OUTFILE  H(12)  Name of the output file. blank -> place in
C                      printer queue.
C      RGBGAMMA R(3)   RGB Gamma corrections
C-----------------------------------------------------------------------
      INTEGER   NLEVS, NCOLS, MAXPIX
      PARAMETER (NLEVS = 40)
      PARAMETER (NCOLS = NLEVS * NLEVS * NLEVS)
      PARAMETER (MAXPIX = 86000)
      INCLUDE 'INCS:PMAD.INC'
      REAL      RED(MAXPIX), GREEN(MAXPIX), BLUE(MAXPIX)
      INTEGER   HC(5,NCOLS)
C
      INTEGER   INPRMS, SCRTCH(MAXPIX), IRET, MAXC, JERR, MAXL, MAXP
      CHARACTER PRGNAM*6
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
C                                       init the task
      INPRMS = 88
      MAXL = NLEVS
      MAXP = MAXPIX
      PRGNAM = 'TVRGB'
      CALL TV3CIN (PRGNAM, INPRMS, MAXL, MAXP, SCRTCH, IRET)
      MAXC = MIN (NCOLS, MAXL*MAXL*MAXL)
C                                       do the function
      IF (IRET.EQ.0) CALL TV3CDO (MAXL, MAXC, HC, MAXP, RED, GREEN,
     *   BLUE, SCRTCH, IRET)
C                                       close TV
      IF ((TVIND.GT.0) .AND. (TVIND2.GT.0)) THEN
         TVGAMA = SVGAMA
         CALL TVCLOS (SCRTCH, JERR)
         END IF
C                                       do the PostScript
      IF ((IRET.EQ.0) .AND. (DOOUTP.GT.0.0)) CALL TV3COU (IRET)
C                                       close files
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE TV3CIN (PRGNAM, INPRMS, MAXL, MAXP, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   Routine to get parameters for TVRGB
C   Inputs:
C      PRGNAM   C*6      Program name
C      INPRMS   I        Number of data parameters from AIPS
C      MAXP     I        Number of pixels in subimage arrays
C   In/out:
C      MAXL     I        Maximum number levels: in gives limit, out
C                           gives user request
C   Outputs:
C       SCRTCH  I(2050)  Scratch buffer
C       IRET    I        Return code 0=> OK, else just go to DIE
C   Task parameters are returned in common /INPARM/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INPRMS, MAXL, MAXP, SCRTCH(*), IRET
C
      INCLUDE 'PTVRGB.INC'
C
      INTEGER   IERR, IPOINT, IROUND, I, INC, J, II, IX, IY, MAXC,
     *   LWIN(4), MAGF, TVCODE, RGBAX, MX, MY, IM
      LOGICAL   REDUCE
      CHARACTER FTYPE*2, CHTMP*8, CHTMP1*8, SUBR*6, PHNAME*48
      DOUBLE PRECISION   DX
      REAL      X, Y, OFFS, SLOPE, TRC2, TRC3
      INCLUDE 'TVRGB.INC'
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
      CALL H2CHR (48, 1, XOUFIL, OUTFIL)
      COPIES = XCOPYS + 0.01
      IF ((COPIES.LE.0) .OR. (OUTFIL.NE.' ')) COPIES = 1
C                                       Numeric user label, inverse
      IF (DPARM(9).GT.0.0) THEN
         CALL H2CHR (24, 1, XREASN, REASON)
      ELSE
         WRITE (REASON,1010) NLUSER
         REASON(15:) = ' '
         CALL H2CHR (24, 1, XREASN, REASON(17:))
         END IF
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
      MAXC = IROUND (DPARM(5))
      IF ((MAXC.LT.8) .OR. (MAXC.GT.MAXL)) MAXC = MIN (32, MAXL)
      DPARM(5) = MAXC
      IF (DPARM(7).GT.MAXC) DPARM(7) = MAXC
      IF (DPARM(7).LT.8) DPARM(7) = MAX (8, MAXC-1)
      MAXL = MAXC
C                                       Open RGB/red map file
      FTYPE = 'MA'
      SUBR = 'MAPOPN'
      CALL MAPOPN ('READ', VOL1, NAMIN, CLSIN, SEQ1, FTYPE, IUSER,
     *   LUN1, IND1, SLOT1, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = VOL1
      FCNO(NCFILE) = SLOT1
      FRW(NCFILE) = 0
C                                       Check windows
      TRC2 = TTRC(3)
      TRC3 = TTRC(4)
      CALL RCOPY (5, TBLC(3), TTRC(3))
      IF (DOOUTP.LE.0.0) THEN
         CALL RCOPY (7, TBLC, BLC)
         CALL RCOPY (7, TTRC, TRC)
      ELSE
         CALL RCOPY (5, TBLC(3), BLC(3))
         CALL RCOPY (5, TTRC(3), TRC(3))
         END IF
      SUBR = 'WINDOW'
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), TBLC, TTRC, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       save corners
      CALL RCOPY (5, TBLC(3), TTRC(3))
      CALL RCOPY (7, TBLC, ABLC(1,1))
      CALL RCOPY (7, TTRC, ATRC(1,1))
      CALL RCOPY (7, TBLC, ABLC(1,2))
      CALL RCOPY (7, TTRC, ATRC(1,2))
      CALL RCOPY (7, TBLC, ABLC(1,3))
      CALL RCOPY (7, TTRC, ATRC(1,3))
      ABLC(3,2) = TRC2
      ATRC(3,2) = TRC2
      ABLC(3,3) = TRC3
      ATRC(3,3) = TRC3
      CALL RCOPY (5, BLC(3), TRC(3))
      CALL RCOPY (7, BLC, OBLC(1,1))
      CALL RCOPY (7, TRC, OTRC(1,1))
      CALL RCOPY (7, BLC, OBLC(1,2))
      CALL RCOPY (7, TRC, OTRC(1,2))
      CALL RCOPY (7, BLC, OBLC(1,3))
      CALL RCOPY (7, TRC, OTRC(1,3))
      OBLC(3,2) = TRC2
      OTRC(3,2) = TRC2
      OBLC(3,3) = TRC3
      OTRC(3,3) = TRC3
C                                       RGB image?
      ISRGB = F
      CHTMP = 'RGB'
      CALL AXEFND (8, CHTMP, CATBLK(KIDIM), CATH(KHCTP), RGBAX, IERR)
      IF (IERR.EQ.0) THEN
         IF (RGBAX.LE.1) THEN
            MSGTXT = 'IS NOT FANCY ENOUGH TO HANDLE THIS IMAGE'
            CALL MSGWRT (8)
            MSGTXT = 'USE TRANS TO MOVE THE RGB AXIS TO 3 OR MORE'
            CALL MSGWRT (8)
            IRET = 8
            GO TO 999
            END IF
         ISRGB = (CATBLK(KINAX+RGBAX).GE.3) .AND.
     *      (CATR(KRCIC+RGBAX).NE.0.0)
         X = CATD(KDCRV+RGBAX) + (1.0 - CATR(KRCRP+RGBAX)) *
     *      CATR(KRCIC+RGBAX)
         Y = CATD(KDCRV+RGBAX) + CATR(KRCIC+RGBAX) *
     *      (CATBLK(KINAX+RGBAX) - CATR(KRCRP+RGBAX))
         ISRGB = ISRGB .AND. (MAX(X,Y).GE.2.999)
         ISRGB = ISRGB .AND. (MIN(X,Y).LE.1.001)
         END IF
C                                       set up headers etc
      REDUCE = F
      IF (ISRGB) THEN
         SEQ2 = SEQ1
         SLOT2 = SLOT1
         NAMIN2 = NAMIN
         CLSIN2 = CLSIN
         VOL2 = VOL1
         SEQ3 = SEQ1
         SLOT3 = SLOT1
         NAMIN3 = NAMIN
         CLSIN3 = CLSIN
         VOL3 = VOL1
         CALL RCOPY (7, ABLC, ABLC(1,2))
         CALL RCOPY (7, ABLC, ABLC(1,3))
         CALL RCOPY (7, ATRC, ATRC(1,2))
         CALL RCOPY (7, ATRC, ATRC(1,3))
         CALL RCOPY (7, OBLC, OBLC(1,2))
         CALL RCOPY (7, OBLC, OBLC(1,3))
         CALL RCOPY (7, OTRC, OTRC(1,2))
         CALL RCOPY (7, OTRC, OTRC(1,3))
         DO 10 I = 1,3
            ABLC(RGBAX+1,I) = CATR(KRCRP+RGBAX) +
     *         (I - CATD(KDCRV+RGBAX)) / CATR(KRCIC+RGBAX)
            ABLC(RGBAX+1,I) = IROUND (ABLC(RGBAX+1,I))
            ATRC(RGBAX+1,I) = ABLC(RGBAX+1,I)
            OBLC(RGBAX+1,I) = CATR(KRCRP+RGBAX) +
     *         (I - CATD(KDCRV+RGBAX)) / CATR(KRCIC+RGBAX)
            OBLC(RGBAX+1,I) = IROUND (OBLC(RGBAX+1,I))
            OTRC(RGBAX+1,I) = OBLC(RGBAX+1,I)
 10         CONTINUE
         CALL COPY (256, CATBLK, CATBL2)
         CALL COPY (256, CATBLK, CATBL3)
C                                        Open green, blue files
         SUBR = 'ZOPEN'
         CALL ZPHFIL (FTYPE, VOL1, SLOT1, 1, PHNAME, IERR)
         CALL ZOPEN (LUN2, IND2, VOL1, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL ZOPEN (LUN3, IND3, VOL1, PHNAME, T, F, T, IERR)
         IF (IERR.NE.0) GO TO 990
C                                        Open green, blue files
      ELSE
         CALL MAPOPN ('READ', VOL2, NAMIN2, CLSIN2, SEQ2, FTYPE, IUSER,
     *      LUN2, IND2, SLOT2, CATBL2, SCRTCH, IERR)
         IF (IERR.GE.2) THEN
            SUBR = 'MAPOPN'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL2
         FCNO(NCFILE) = SLOT2
         FRW(NCFILE) = 0
         CALL MAPOPN ('READ', VOL3, NAMIN3, CLSIN3, SEQ3, FTYPE, IUSER,
     *      LUN3, IND3, SLOT3, CATBL3, SCRTCH, IERR)
         IF (IERR.GE.2) THEN
            SUBR = 'MAPOPN'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = VOL3
         FCNO(NCFILE) = SLOT3
         FRW(NCFILE) = 0
C                                       Image 2,3 plane selection
         DO 20 I = 3,7
            J = KINAX + I - 1
            IM = 2
            IF ((CATBL2(J).LE.1) .OR. (CATBL2(KIDIM).LT.I)) THEN
               ABLC(I,2) = 1
            ELSE
               ABLC(I,2) = MAX (1.0, ABLC(I,2))
               IF (ABLC(I,2)-0.01.GT.CATBL2(J)) GO TO 45
               END IF
            ATRC(I,2) = ABLC(I,2)
            OBLC(I,2) = ABLC(I,2)
            OTRC(I,2) = OBLC(I,2)
            IM = 3
            IF ((CATBL3(J).LE.1) .OR. (CATBL3(KIDIM).LT.I)) THEN
               ABLC(I,3) = 1
            ELSE
               ABLC(I,3) = MAX (1.0, ABLC(I,3))
               IF (ABLC(I,3)-0.01.GT.CATBL3(J)) GO TO 45
               END IF
            ATRC(I,3) = ABLC(I,3)
            OBLC(I,3) = ABLC(I,3)
            OTRC(I,3) = ABLC(I,3)
 20         CONTINUE
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
     *            CATR(KRCIC+J)
               X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *            CATR2(KRCRP+J)
               ABLC(I,2) = IROUND (X)
               IF ((DOGRID.GE.0.1) .AND. (ABS(X-ABLC(I,2)).GT.0.2))
     *            GO TO 45
               ATRC(I,2) = ABLC(I,2) + ATRC(I,1) - ABLC(I,1)
               IM = 3
               IF (CATR3(KRCIC+J).EQ.0.0) GO TO 45
               X = (DX - CATD3(KDCRV+J)) / CATR3(KRCIC+J) +
     *            CATR3(KRCRP+J)
               ABLC(I,3) = IROUND (X)
               IF ((DOGRID.GE.0.1) .AND. (ABS(X-ABLC(I,3)).GT.0.2))
     *            GO TO 45
               ATRC(I,3) = ABLC(I,3) + ATRC(I,1) - ABLC(I,1)
               IM = 2
               IF (CATR2(KRCIC+J).EQ.0.0) GO TO 45
               DX = CATD(KDCRV+J) + (OBLC(I,1) - CATR(KRCRP+J)) *
     *            CATR(KRCIC+J)
               X = (DX - CATD2(KDCRV+J)) / CATR2(KRCIC+J) +
     *            CATR2(KRCRP+J)
               OBLC(I,2) = IROUND (X)
               IF ((DOGRID.GE.0.1) .AND. (ABS(X-OBLC(I,2)).GT.0.2))
     *            GO TO 45
               OTRC(I,2) = OBLC(I,2) + OTRC(I,1) - OBLC(I,1)
               IM = 3
               IF (CATR3(KRCIC+J).EQ.0.0) GO TO 45
               X = (DX - CATD3(KDCRV+J)) / CATR3(KRCIC+J) +
     *            CATR3(KRCRP+J)
               OBLC(I,3) = IROUND (X)
               IF ((DOGRID.GE.0.1) .AND. (ABS(X-OBLC(I,3)).GT.0.2))
     *            GO TO 45
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
            IM = 1
            IF (ABLC(I,1).GE.ATRC(I,1)) GO TO 45
            IM = 2
            IF (ABLC(I,2).GE.ATRC(I,2)) GO TO 45
            IM = 3
            IF (ABLC(I,3).GE.ATRC(I,3)) GO TO 45
C                                       output subimage
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
            IM = 1
            IF (OBLC(I,1).GE.OTRC(I,1)) GO TO 45
            IM = 2
            IF (OBLC(I,2).GE.OTRC(I,2)) GO TO 45
            IM = 3
            IF (OBLC(I,3).GE.OTRC(I,3)) GO TO 45
 35         CONTINUE
C                                       Check true coincidence
         IF (DOGRID.GE.0.1) THEN
            INC = 2
            DO 40 I = 1,2
               J = I - 1
               IPOINT = KHCTP+J*INC
               X = 0.2 * 0.2 * ABS (CATR(KRCIC+J))
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
 40            CONTINUE
            END IF
C                                       Corners reduced a little
         IF (REDUCE) THEN
            MSGTXT = 'Input images coincident on reduced subimage only'
            CALL MSGWRT (4)
            END IF
         GO TO 55
C                                       Maps not coincident
 45      MSGTXT = 'INPUT IMAGES ARE NOT COINCIDENT.'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1045) IM, I
         GO TO 999
         END IF
C                                       pix ranges
 55   CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), PRANGE(1,1))
      IF (DPARM(2).LE.DPARM(1)) THEN
         PRANGE(1,2) = PRANGE(1,1)
         PRANGE(2,2) = PRANGE(2,1)
      ELSE
         CALL RNGSET (DPARM, CATR2(KRDMX), CATR2(KRDMN), PRANGE(1,2))
         END IF
      IF (DPARM(4).LE.DPARM(3)) THEN
         PRANGE(1,3) = PRANGE(1,1)
         PRANGE(2,3) = PRANGE(2,1)
      ELSE
         CALL RNGSET (DPARM(3), CATR3(KRDMX), CATR3(KRDMN), PRANGE(1,3))
         END IF
C                                       open the TV
      IRET = 8
      SUBR = 'TVOPEN'
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL COPY (4, WINDTV, LWINTV)
C                                       init TV
      DO3CHN = (TVIMPC.GT.0) .AND. (DPARM(8).LE.0.0) .AND. (NGRAY.GE.3)
      IF ((TVCH.LT.1) .OR. (TVCH.GT.NGRAY)) TVCH = 1
      ITVCH = TVCH
      IF (DO3CHN) THEN
         IF (TVCH+2.GT.NGRAY) TVCH = NGRAY - 2
         ITVCH = TVCH
      ELSE IF (NGRAY.GE.2) THEN
         IF (TVCH+1.GT.NGRAY) TVCH = NGRAY - 1
         ITVCH = TVCH + 1
         END IF
      SVGAMA = TVGAMA
      IF (RGBGAM(1).LE.0.0) RGBGAM(1) = SVGAMA
      IF (RGBGAM(2).LE.0.0) RGBGAM(2) = RGBGAM(1)
      IF (RGBGAM(3).LE.0.0) RGBGAM(3) = RGBGAM(1)
      CALL YHOLD ('ONNN', IERR)
      SUBR = 'YCINIT'
      CALL YCINIT (TVCH, SCRTCH)
      IF (IERR.NE.0) GO TO 990
      IF (DO3CHN) THEN
         CALL YCINIT (TVCH+1, SCRTCH)
         IF (IERR.NE.0) GO TO 990
         CALL YCINIT (TVCH+2, SCRTCH)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (ITVCH.NE.TVCH) THEN
         CALL YCINIT (ITVCH, SCRTCH)
         IF (IERR.NE.0) GO TO 990
         END IF
      SUBR = 'YSLECT'
      II = NGRAY + NGRAPH
      DO 60 I = 1,II
         CALL YSLECT ('OFFF', I, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
 60      CONTINUE
      SUBR = 'YZERO'
      CALL YZERO (TVCH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DO3CHN) THEN
         CALL YZERO (TVCH+1, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YZERO (TVCH+2, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (ITVCH.NE.TVCH) THEN
         CALL YZERO (ITVCH, SCRTCH)
         IF (IERR.NE.0) GO TO 990
         END IF
      TVCODE = 2 ** (TVCH - 1)
      IF (DO3CHN) TVCODE = TVCODE + 2 ** TVCH + 2 ** (TVCH + 1)
      IF (ITVCH.NE.TVCH) TVCODE = TVCODE + 2 ** (ITVCH - 1)
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
      BUFF1(1) = 0.0
      SUBR = 'YOFM'
      CALL YOFM ('WRIT', 7, T, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       LUTs
      II = MAXINT + 1
      SLOPE = (LUTOUT + 1.0) / (MAXINT+1.0)
      DO 70 I = 1,II
         SCRTCH(I+1) = I * SLOPE - 0.5
 70      CONTINUE
      SCRTCH(1) = 0
      SUBR = 'YLUT'
      CALL YLUT ('WRIT', TVCODE, 7, T, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Colors, channels on
      SUBR = 'YSLECT'
C                                       3 channels on in colors
      IF (DO3CHN) THEN
         DO 75 I = 1,3
            II = TVCH + I - 1
            CALL YSLECT ('ONNN', II, I, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
 75         CONTINUE
C                                       dumb channel on
      ELSE
         CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Number dumb R, G, B levels
         MAXC = MAXINT
         NLB = REAL (MAXC) ** 0.33333 + 0.0001
         X = MAXC / NLB
         NLG = SQRT (X) + 0.0001
         NLR = MAXC / (NLB * NLG)
         END IF
C                                       check window further
      XINC = MAX (1, IROUND (XXINC))
      YINC = MAX (1, IROUND (XYINC))
      NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01
      IF ((NX.GT.1.5*MAXXTV(1)) .AND. (XXINC.LT.0.5)) THEN
         XINC = IROUND (REAL (NX) / REAL (MAXXTV(1)))
         NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01
         END IF
      IF (NX.GT.MAXXTV(1)) THEN
         ABLC(1,1) = ABLC(1,1) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ABLC(1,2) = ABLC(1,2) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ABLC(1,3) = ABLC(1,3) + XINC * (NX - MAXXTV(1)) / 2.0 + 0.5
         ATRC(1,1) = ABLC(1,1) + XINC * (MAXXTV(1) - 1)
         ATRC(1,2) = ABLC(1,2) + XINC * (MAXXTV(1) - 1)
         ATRC(1,3) = ABLC(1,3) + XINC * (MAXXTV(1) - 1)
         NX = (ATRC(1,1) - ABLC(1,1)) / XINC + 1.01
         END IF
      NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01
      IF ((NY.GT.1.5*MAXXTV(2)) .AND. (XYINC.LT.0.5)) THEN
         YINC = IROUND (REAL (NY) / REAL (MAXXTV(2)))
         NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01
         END IF
      IF (NY.GT.MAXXTV(2)) THEN
         ABLC(2,1) = ABLC(2,1) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ABLC(2,2) = ABLC(2,2) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ABLC(2,3) = ABLC(2,3) + YINC * (NY - MAXXTV(2)) / 2.0 + 0.5
         ATRC(2,1) = ABLC(2,1) + YINC * (MAXXTV(2) - 1)
         ATRC(2,2) = ABLC(2,2) + YINC * (MAXXTV(2) - 1)
         ATRC(2,3) = ABLC(2,3) + YINC * (MAXXTV(2) - 1)
         NY = (ATRC(2,1) - ABLC(2,1)) / YINC + 1.01
         END IF
C                                       first subarray in core
      IF (DPARM(6).GT.MAXP) DPARM(6) = MAXP
      IF (DPARM(6).LT.MAXP/100.0) DPARM(6) = MAXP
      MAXP = DPARM(6) + 0.1
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
      MX = NX
      MY = NY
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
      IX = (LWIN(1) + LWIN(3) - NX) / 2
      IX = MAX (1, IX)
      IF (IX+NX-1.GT.MAXXTV(1)) IX = MAXXTV(1) - NX + 1
      IY = (LWIN(2) + LWIN(4) - NY) / 2
      IY = MAX (1, IY)
      IF (IY+NY-1.GT.MAXXTV(2)) IY = MAXXTV(2) - NY + 1
      CATBLK(IICOR) = IX
      CATBLK(IICOR+1) = IY
      CATBLK(IICOR+2) = IX + NX - 1
      CATBLK(IICOR+3) = IY + NY - 1
      IF (DO3CHN) THEN
         CALL COPY (4, CATBLK(IICOR), CATBL2(IICOR))
         CALL COPY (4, CATBLK(IICOR), CATBL3(IICOR))
      ELSE
         IX = LWIN(1) + 10
         IF (IX+INX-1.GT.LWIN(3)) IX = IX - (IX + INX - 1 - LWIN(3)) / 2
         IF (IX+INX-1.GT.MAXXTV(1)) IX = MAXXTV(1) - INX + 1
         IY = LWIN(2) + 10
         IF (IY+INY-1.GT.LWIN(4)) IY = IY - (IY + INY - 1 - LWIN(4)) / 2
         IF (IY+INY-1.GT.MAXXTV(2)) IY = MAXXTV(2) - INY + 1
         CATBL2(IICOR) = IX
         CATBL2(IICOR+1) = IY
         CATBL2(IICOR+2) = IX + INX - 1
         CATBL2(IICOR+3) = IY + INY - 1
         CALL COPY (4, CATBL2(IICOR), CATBL3(IICOR))
         END IF
      SUBR = 'YCWRIT'
      CALL YCWRIT (TVCH, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (DO3CHN) THEN
         CALL YCWRIT (TVCH+1, CATBL2(IICOR), CATBL2, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YCWRIT (TVCH+2, CATBL3(IICOR), CATBL3, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
      ELSE IF (ITVCH.NE.TVCH) THEN
         CALL YCWRIT (TVCH+1, CATBL2(IICOR), CATBL2, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
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
 1000 FORMAT ('TV3CIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('AIPS User',I5)
 1045 FORMAT ('ERROR APPEARS TO BE ON IMAGE',I2,' AXIS',I2)
 1990 FORMAT ('TV3CIN: ERROR',I5,' RETURNED FROM ROUTINE ',A)
      END
      SUBROUTINE TV3CDO (LEVS, COLS, HC, MAXP, RED, GREEN, BLUE, SCRTCH,
     *   IRET)
C-----------------------------------------------------------------------
C   TV3CDO implements the interactive selection and execution of the
C   various display and interaction options of TVRGB.
C   Inputs:
C      LEVS     I           Number levels used / primary color in hist.
C      COLS     I           Total number colors in histogram area
C      MAXP     I           Size of RED, GREEN, BLUE subarray data areas
C   Output:
C      HC       I(5,COLS)   Work area for color optimization
C      RED      R(MAXP)     Red subarray data
C      GREEN    R(MAXP)     Green subarray data
C      BLUE     R(MAXP)     Blue subarray data
C      SCRTCH   I(2050)     Scratch for TV loads etc
C      IRET     I           Error code returned: 0 okay
C-----------------------------------------------------------------------
      INTEGER   LEVS, COLS, HC(5,COLS), MAXP, SCRTCH(*), IRET
      REAL      RED(MAXP), GREEN(MAXP), BLUE(MAXP)
C
      INCLUDE 'PTVRGB.INC'
C
      INTEGER   FLIP(4), I, II, J, IX, IY, IERR, IBUT, GRCS(2), CHS,
     *   LWIN(4), MAGF, LCHS, NTITLE, TOPSEP, SIDSEP, TIMLIM
      LOGICAL   MENUOK, WINDOK, DOIT, SUBIMG, LEAVE(NOPT)
      REAL      X, Y, XS, XO, EPOS(2,4), LFSC(2,4), LRANGE(2,3), OFFS
      CHARACTER OPTION(NOPT)*20, SUBR*6, UNITS*8, OFFCUR*20, ONCUR*20,
     *   ISHELP*8, TITLE*8, FUNCS(4)*2
      INCLUDE 'TVRGB.INC'
      DATA OPTION /'RED FUNC TO SQ', 'GREEN FUNC TO SQ',
     *   'BLUE FUNC TO SQ', 'ENHANCE RED', 'ENHANCE GREEN',
     *   'ENHANCE BLUE', 'ENHANCE TOGETHER', 'DISPLAY CURSOR X,Y',
     *   'SHOW PIXRANGES', 'SET WINDOW', 'REDRAW FULL PICTURE',
     *   'EXIT', 'ABORT'/
      DATA OFFCUR /'TURN OFF CURSOR X,Y'/
      DATA ONCUR /'DISPLAY CURSOR X,Y'/
      DATA LEAVE /7*.FALSE., 2*.TRUE., .FALSE., .TRUE., 2*.FALSE./
      DATA FUNCS /'LN', 'SQ', 'LG', 'L2'/
C-----------------------------------------------------------------------
C                                        Initialize
      TITLE = ' '
      NTITLE = 0
      WINDOK = F
      MENUOK = F
      SUBIMG = F
      GRMENU = 1 + NGRAY
      GRCURS = MIN (2, NGRAPH) + NGRAY
      GRWIND = MIN (3, NGRAPH) + NGRAY
      SC(1,1) = 1.0
      SC(1,2) = 1.0
      SC(1,3) = 1.0
      SC(1,4) = 1.0
      SC(2,1) = 0.0
      SC(2,2) = 0.0
      SC(2,3) = 0.0
      SC(2,4) = 0.0
      CALL FILL (4, 1, FLIP)
      CALL RFILL (8, 0.0, EPOS)
      CALL RFILL (8, 0.0, DSC)
      CALL RCOPY (6, PRANGE, ARANGE)
      CALL RCOPY (8, SC, FSC)
      DOGRC = F
      CALL FILL (3, 1, IFUNC)
C                                       start by forcing a load
      CHS = NOPT - 2
      GO TO 110
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
            CALL YZERO (GRCURS, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         END IF
C                                       build menu
      GRCS(1) = GRMENU - NGRAY
      IF (MENUOK) GRCS(1) = -GRCS(1)
      GRCS(2) = MIN (NGRAPH, 4)
      ISHELP = TSKNAM
      TOPSEP = 3 * CSIZTV(2) + 1
      SIDSEP = 5
      TIMLIM = 0
      CALL TVMENU (0, 1, NOPT, GRCS, TOPSEP, SIDSEP, ISHELP, OPTION,
     *   TIMLIM, LEAVE, NTITLE, TITLE, CHS, IBUT, SCRTCH, IERR)
      SUBR = 'TVMENU'
      IF (IERR.NE.0) GO TO 990
      MENUOK = T
C                                       Something to do
 110  CALL YHOLD ('ONNN', IERR)
C                                       Enhance subimage
      DOIT = F
      IF (OPTION(CHS)(:8).EQ.'ENHANCE ') THEN
         LCHS = CHS - 3
         IF (DO3CHN) THEN
            CALL TV3C3I (LCHS, FLIP(LCHS), EPOS(1,LCHS), SCRTCH, IRET)
         ELSE
            CALL TV3C1I (LCHS, INX, INY, RED, GREEN, BLUE, HC,
     *         FLIP(LCHS), EPOS(1,LCHS), SCRTCH, IRET)
            SUBIMG = T
            END IF
         IF (IRET.NE.0) GO TO 999
         IF ((GRCURS.EQ.GRMENU) .AND. (DOGRC)) MENUOK = F
C                                       transfer functions
      ELSE IF (OPTION(CHS)(:12).EQ.'RED FUNC TO ') THEN
         IFUNC(1) = MOD (IFUNC(1), 4) + 1
         I = MOD (IFUNC(1), 4) + 1
         OPTION(CHS)(13:14) = FUNCS(I)
         MENUOK = F
         DOIT = T
C                                       transfer functions
      ELSE IF (OPTION(CHS)(:14).EQ.'GREEN FUNC TO ') THEN
         IFUNC(2) = MOD (IFUNC(2), 4) + 1
         I = MOD (IFUNC(2), 4) + 1
         OPTION(CHS)(15:16) = FUNCS(I)
         MENUOK = F
         DOIT = T
C                                       transfer functions
      ELSE IF (OPTION(CHS)(:13).EQ.'BLUE FUNC TO ') THEN
         IFUNC(3) = MOD (IFUNC(3), 4) + 1
         I = MOD (IFUNC(3), 4) + 1
         OPTION(CHS)(14:15) = FUNCS(I)
         MENUOK = F
         DOIT = T
C                                       reset cursor display option
      ELSE IF ((OPTION(CHS).EQ.OFFCUR) .OR. (OPTION(CHS).EQ.ONCUR)) THEN
         DOGRC = .NOT.DOGRC
         MENUOK = F
         SUBR = 'YSLECT'
         IF (DOGRC) THEN
            OPTION(CHS) = OFFCUR
            CALL YSLECT ('ONNN', GRCURS, 0, SCRTCH, IERR)
         ELSE
            OPTION(CHS) = ONCUR
            CALL YSLECT ('OFFF', GRCURS, 0, SCRTCH, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
C                                       show pixranges
      ELSE IF (OPTION(CHS).EQ.'SHOW PIXRANGES') THEN
            DO 115 I = 1,4
               LFSC(1,I) = FSC(1,I) * SC(1,I)
               LFSC(2,I) = FSC(2,I) * SC(1,I) + SC(2,I)
 115        CONTINUE
            DO 120 I = 1,3
               X = PRANGE(2,I)
               Y = PRANGE(1,I)
               XS = LFSC(1,I) * LFSC(1,4)
               XO = LFSC(2,4) + LFSC(2,I) * LFSC(1,4)
               IF (XS.EQ.0.0) XS = 1.0
               LRANGE(1,I) = Y + XO * (X - Y) / XS
               LRANGE(2,I) = Y + (1.0 + XO) * (X - Y) / XS
 120           CONTINUE
         MSGTXT = 'PixRanges used for linear scaling:'
         CALL MSGWRT (2)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Current Red  ', LRANGE(1,1), LRANGE(2,1),
     *      UNITS
         CALL MSGWRT (2)
         CALL H2CHR (8, 1, CATH2(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Current Green', LRANGE(1,2), LRANGE(2,2),
     *      UNITS
         CALL MSGWRT (2)
         CALL H2CHR (8, 1, CATH3(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Current Blue ', LRANGE(1,3), LRANGE(2,3),
     *      UNITS
         CALL MSGWRT (2)
         MSGTXT = 'After being clipped by PixRanges:'
         CALL MSGWRT (1)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Red  ', PRANGE(1,1), PRANGE(2,1), UNITS
         CALL MSGWRT (1)
         CALL H2CHR (8, 1, CATH2(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Green', PRANGE(1,2), PRANGE(2,2), UNITS
         CALL MSGWRT (1)
         CALL H2CHR (8, 1, CATH3(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Blue ', PRANGE(1,3), PRANGE(2,3), UNITS
         CALL MSGWRT (1)
C                                       abort
      ELSE IF (OPTION(CHS).EQ.'ABORT') THEN
         IRET = 2
C                                       Reload possible
      ELSE
C                                       Set window for subimage
         IF (OPTION(CHS).EQ.'SET WINDOW') THEN
            IF (GRWIND.EQ.GRMENU) WINDOK = F
            CALL TV3CWI (WINDOK, MAXP, DOIT, SCRTCH, IRET)
            IF (IRET.NE.0) GO TO 999
            IF (GRWIND.EQ.GRMENU) MENUOK = F
C                                       Has scaling changed?
         ELSE
            X = 0
            DO 135 I = 1,2
               DO 130 J = 1,4
                  X = X + ABS (SC(I,J) - DSC(I,J))
 130              CONTINUE
 135           CONTINUE
            DOIT = X.GT.0.004
            DOIT = (DOIT) .OR. ((OPTION(CHS).EQ.'EXIT') .AND. (SUBIMG))
            END IF
         END IF
      IF (DOIT) THEN
         DO 140 I = 1,4
            FSC(1,I) = FSC(1,I) * SC(1,I)
            FSC(2,I) = FSC(2,I) * SC(1,I) + SC(2,I)
            SC(1,I) = 1.0
            SC(2,I) = 0.0
 140        CONTINUE
         DO 145 I = 1,3
            X = PRANGE(2,I)
            Y = PRANGE(1,I)
            XS = FSC(1,I) * FSC(1,4)
            XO = FSC(2,4) + FSC(2,I) * FSC(1,4)
            IF (XS.EQ.0.0) XS = 1.0
            ARANGE(1,I) = Y + XO * (X - Y) / XS
            ARANGE(2,I) = Y + (1.0 + XO) * (X - Y) / XS
 145        CONTINUE
         IF (DO3CHN) THEN
            CALL TV3C3L (SCRTCH, IRET)
         ELSE
            CALL TV3C1L (SUBIMG, INX, INY, LEVS, COLS, HC, RED,
     *         GREEN, BLUE, SCRTCH, IRET)
            END IF
         IF (IRET.NE.0) GO TO 999
         SUBIMG = F
         CALL FILL (4, 1, FLIP)
         CALL RFILL (8, 0.0, EPOS)
         CALL RCOPY (8, SC, DSC)
         END IF
      CALL YHOLD ('OFFF', IERR)
      IF ((OPTION(CHS).NE.'EXIT') .AND. (OPTION(CHS).NE.'ABORT'))
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
         MSGTXT = 'PixRanges used for linear scaling:'
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Final Red  ', ARANGE(1,1), ARANGE(2,1),
     *      UNITS
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH2(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Final Green', ARANGE(1,2), ARANGE(2,2),
     *      UNITS
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH3(KHBUN), UNITS)
         WRITE (MSGTXT,1995) 'Final Blue ', ARANGE(1,3), ARANGE(2,3),
     *      UNITS
         CALL MSGWRT (5)
         MSGTXT = 'After being clipped by PixRanges:'
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Red  ', PRANGE(1,1), PRANGE(2,1), UNITS
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH2(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Green', PRANGE(1,2), PRANGE(2,2), UNITS
         CALL MSGWRT (5)
         CALL H2CHR (8, 1, CATH3(KHBUN), UNITS)
         WRITE (MSGTXT,1996) 'Blue ', PRANGE(1,3), PRANGE(2,3), UNITS
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TV3CDO: ERROR',I5,' RETURNED BY ROUTINE ',A)
 1995 FORMAT (A,' pixrange =',1PE12.4,',',1PE12.4,1X,A8)
 1996 FORMAT (A,' pixrange =',1PE12.4,',',1PE12.4,1X,A8)
      END
      SUBROUTINE TV3CWI (WINDOK, MAXP, DOIT, SCRTCH, IRET)
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
      INTEGER   IERR, LWIN(4), ITW(3), QUAD, IBUT, IXP(5), IYP(5), IOFF,
     *   LTVL
      REAL      RPOS(2), PPOS(2), R0, RT
      CHARACTER SUBR*6, STRING*16
      INCLUDE 'TVRGB.INC'
C-----------------------------------------------------------------------
      IRET = 0
      DOIT = F
      CALL ZTIME (ITW)
      CALL YHOLD ('ONNN', IERR)
      LTVL = TVLIMG(1)
C                                       erase
      IF (.NOT.WINDOK) THEN
         CALL YZERO (GRWIND, IERR)
         SUBR = 'YZERO'
         IF (IERR.NE.0) GO TO 990
         WINDOK = T
         END IF
C                                       turn on
      SUBR = 'YSLECT'
      IF ((GRWIND.NE.GRCURS) .OR. (.NOT.DOGRC)) THEN
         CALL YSLECT ('ONNN', GRWIND, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      IF (ITVCH.NE.TVCH) THEN
         CALL YSLECT ('OFFF', ITVCH, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
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
         CALL YHOLD ('ONNN', IERR)
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
         CALL YHOLD ('OFFF', IERR)
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
      CALL YHOLD ('ONNN', IERR)
      IF ((GRWIND.NE.GRCURS) .OR. (.NOT.DOGRC)) THEN
         CALL YSLECT ('OFFF', GRWIND, 0, SCRTCH, IERR)
         END IF
      IF ((LTVL.NE.TVLIMG(1)) .AND. (.NOT.DO3CHN)) THEN
         CALL YSLECT ('OFFF', TVCH, 0, SCRTCH, IERR)
         CALL YSLECT ('ONNN', ITVCH, 0, SCRTCH, IERR)
         END IF
      CALL YHOLD ('OFFF', IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('IM X=',I4,' Y=',I4)
 1990 FORMAT ('TV3CWI: ERROR',I5,' RETURNED BY ROUTINE ',A)
      END
      SUBROUTINE TV3C3I (LJ, FLIP, PPOS, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   implements interactive enhancement of individual colors or all 3
C   colors in the case of 3 channels
C   Inputs:
C      LJ       I      color:1-4 => R, G, B, all
C   In/out:
C      FLIP     I      sign of slope for type LJ
C      PPOS     R(2)   previous cursor position for this type
C   Output:
C      SCRTCH   I(*)   TV buffer
C      IRET     I      Error return
C-----------------------------------------------------------------------
      INTEGER   LJ, FLIP, SCRTCH(*), IRET
      REAL      PPOS(2)
C
      INTEGER   I, IERR, IBUT, QUAD, ITW(3), IC1, IC2, IC, II, JJ
      LOGICAL   DOIT
      REAL      RPOS(2), F0, X, SLOPE, OFFSET, POW, XSCL, POS0(2)
      CHARACTER SUBR*6, STRING*20
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA POW, XSCL /0.9, 1.3/
C     DATA POW, XSCL /1.33, 2.0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       init TV cursor
      CALL YHOLD ('OFFF', IERR)
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
      IC1 = LJ
      IC2 = LJ
      IF (LJ.EQ.1) THEN
         MSGTXT = 'Enhancing the red image'
      ELSE IF (LJ.EQ.2) THEN
         MSGTXT = 'Enhancing the green image'
      ELSE IF (LJ.EQ.3) THEN
         MSGTXT = 'Enhancing the blue image'
      ELSE
         MSGTXT = 'Enhancing all three images together'
         IC1 = 1
         IC2 = 3
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
            SLOPE = 1.
            IF (FSC(1,LJ).NE.0.0) SLOPE = 1.0 / FSC(1,LJ)
            OFFSET = -FSC(2,LJ) * SLOPE
            FLIP = 1
            IF (SLOPE.LT.0.0) FLIP = -1
            RPOS(2) = WINDTV(2) + F0 / (ABS (SLOPE) ** (1.0/POW))
            X = 1.0 - (OFFSET + 1. - (1-FLIP)/2) / SLOPE
            RPOS(1) = WINDTV(1) - 1.0 +
     *         (WINDTV(3)-WINDTV(1)+1.) * (X/XSCL + 0.5)
            IF ((RPOS(1).LE.WINDTV(1)) .OR. (RPOS(1).GE.WINDTV(3)) .OR.
     *         (RPOS(2).LE.WINDTV(2)) .OR. (RPOS(2).GE.WINDTV(4))) THEN
               MSGTXT = 'Unable to represent this as cursor position' //
     *            ' ***********'
               CALL MSGWRT (6)
               MSGTXT = 'Reload full image NOW to avoid problems' //
     *            ' ***********'
               CALL MSGWRT (6)
               SC(1,LJ) = SLOPE
               SC(2,LJ) = OFFSET
               GO TO 990
               END IF
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            SUBR = 'YCURSE'
            IF (IERR.NE.0) GO TO 990
            END IF
         X = ((RPOS(1) - WINDTV(1) + 1.0) /
     *      (WINDTV(3) - WINDTV(1) + 1.0) - 0.5) * XSCL
         SLOPE = ((F0 / MAX (1., RPOS(2)-WINDTV(2))) ** POW) *  FLIP
         OFFSET = 1.0 / LUTOUT
         OFFSET = ((OFFSET+X) * SLOPE + (1 - FLIP) / 2) - OFFSET
         SC(1,LJ) = SLOPE
         SC(2,LJ) = OFFSET
         DO 70 IC = IC1,IC2
            II = MAXINT + 1
            SLOPE = SC(1,IC) * SC(1,4) / MAXINT
            OFFSET = SC(2,4) + SC(1,4) * SC(2,IC) + SLOPE
            DO 60 I = 2,II
               SCRTCH(I) = LUTOUT * (I * SLOPE - OFFSET) + 0.5
               SCRTCH(I) = MAX (0, MIN (LUTOUT, SCRTCH(I)))
 60            CONTINUE
            SCRTCH(1) = 0
            SUBR = 'YLUT'
            II = 2 ** (TVCH + IC - 2)
            JJ = 2 ** (3 - IC)
            CALL YLUT ('WRIT', II, JJ, T, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
 70         CONTINUE
         IF (DOGRC) THEN
            CALL YHOLD ('ONNN', IERR)
            II = RPOS(1) + 0.5
            JJ = RPOS(2) + 0.5
            WRITE (STRING,1050) II, JJ
            SUBR = 'IMCHAR'
            CALL IMCHAR (GRCURS, GRX0, GRY0, 0, 0, STRING, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YHOLD ('OFFF', IERR)
            END IF
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
 1050 FORMAT ('TV X=',I4,' Y=',I4)
 1990 FORMAT ('TV3C3I: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE TV3C3L (SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TV3C3L uses 3 channels on the TV, 1 for each color, to display 3
C   images
C   Outputs:
C      SCRTCH   I(1536)    TV scratch array
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   SCRTCH(*), IRET
C
      INTEGER   J, RBLK, GBLK, BBLK, IERR, RBIND, GBIND, BBIND, IX, IY,
     *   TVCODE, I, II, JX, JY
      REAL      SLOPE, OFFSET, X
      CHARACTER SUBR*6
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
      TVCODE = 2 ** (TVCH-1) + 2 ** TVCH + 2 ** (TVCH+1)
      CALL YHOLD ('ONNN', IERR)
C                                       reset the LUTs
      J = MAXINT + 1
      SLOPE = (LUTOUT + 1.0) / (MAXINT+1.0)
      DO 10 I = 1,J
         SCRTCH(I+1) = I * SLOPE - 0.5
 10      CONTINUE
      SCRTCH(1) = 0
      CALL YLUT ('WRIT', TVCODE, 7, T, SCRTCH, IERR)
      SUBR = 'YLUT'
      IF (IERR.NE.0) GO TO 990
C                                       reset the OFM
      II = OFMINP + 1
      CALL RFILL (II, 0.0, BUFF1)
      II = LUTOUT + 1
      OFFSET = 1.0 / REAL(OFMOUT)
      SLOPE = OFFSET * (OFMOUT + 1.0) / REAL (II)
      DO 15 I = 2,II
         X = I * SLOPE - OFFSET
         BUFF1(I) = MAX (0.0, MIN (1.0, X))
 15      CONTINUE
      BUFF1(1) = 0.0
      SUBR = 'YOFM'
      CALL YOFM ('WRIT', 7, T, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       init image windows
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), RBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), GBLK,
     *   IERR)
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), BBLK,
     *   IERR)
      RBLK = RBLK + 1
      GBLK = GBLK + 1
      BBLK = BBLK + 1
C                                       init disk read
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, RBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, GBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *   CATBL3(IIWIN), BUFF3, JBUFSZ, BBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1) - 1
      JX = CATBLK(IIWIN+2) - CATBLK(IIWIN) + 1
      JY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      SUBR = 'MDISK'
      DO 20 J = 1,JY
C                                       read data rows
         CALL MDISK ('READ', LUN1, IND1, BUFF1, RBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, GBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN3, IND3, BUFF3, BBIND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       scale data rows and tvlod
         IF (MOD(J-1,YINC).EQ.0) THEN
            IY = IY + 1
            CALL RGBLOD (JX, XINC, IX, IY, ARANGE, PRANGE, BUFF1(RBIND),
     *         BUFF2(GBIND), BUFF3(BBIND), SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               SUBR = 'YIMGIO'
               GO TO 990
               END IF
            END IF
 20      CONTINUE
      CALL YHOLD ('OFFF', IERR)
C                                        off graphics & cursor
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TV3C3L: ERROR',I5,' GENERATED BY ROUTINE',A)
      END
      SUBROUTINE RGBLOD (NP, INC, IX, IY, SCR, CLR, DR, DG, DB, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C   RGBLOD does the scaling, clipping of an RGB triple for a row,
C   and loads the row to 3 channels on the TV
C   Inputs:
C      NP       I        Number of points in the row
C      IX       I        X position on the TV
C      IY       I        Y position on the TV
C      SCR      R(2,3)   Min/max for red, g, b for scaling
C      CLR      R(2,3)   Min/max for brm gm b for clippin
C      DR       R(NP)    red row
C      DG       R(NP)    green row
C      DB       R(NP)    blue row
C   Output:
C      SCRTCH   I(*)     output row
C      IERR     I        Return code from YIMGIO
C-----------------------------------------------------------------------
      INTEGER   NP, INC, IX, IY, SCRTCH(*), IERR
      REAL      DR(NP), DG(NP), DB(NP), SCR(2,3), CLR(2,3)
C
      INTEGER   I, J
      REAL      S
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       red row
      J = 0
      DO 10 I = 1,NP
         IF (MOD(I-1, INC).EQ.0) THEN
            J = J + 1
            IF (DR(I).EQ.FBLANK) THEN
               SCRTCH(J) = 0
            ELSE
               S = MAX (CLR(1,1), MIN (CLR(2,1), DR(I)))
               S = (S - SCR(1,1)) / (SCR(2,1) - SCR(1,1))
               S = MAX (0.0, MIN (1.0, S))
               IF (IFUNC(1).EQ.2) THEN
                  S = SQRT (S)
               ELSE IF (IFUNC(1).EQ.3) THEN
                  S = LOG10 (9.0*S + 1.0)
               ELSE IF (IFUNC(1).EQ.4) THEN
                  S = LOG10 (99.0*S + 1.0) / 2.0
                  END IF
               SCRTCH(J) = (MAXINT - 1) * S + 1.5
               END IF
            END IF
 10      CONTINUE
      I = TVCH
      CALL YIMGIO ('WRIT', I, IX, IY, 0, J, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       green row
      J = 0
      DO 20 I = 1,NP
         IF (MOD(I-1, INC).EQ.0) THEN
            J = J + 1
            IF (DG(I).EQ.FBLANK) THEN
               SCRTCH(J) = 0
            ELSE
               S = MAX (CLR(1,2), MIN (CLR(2,2), DG(I)))
               S = (S - SCR(1,2)) / (SCR(2,2) - SCR(1,2))
               S = MAX (0.0, MIN (1.0, S))
               IF (IFUNC(2).EQ.2) THEN
                  S = SQRT (S)
               ELSE IF (IFUNC(2).EQ.3) THEN
                  S = LOG10 (9.0*S + 1.0)
               ELSE IF (IFUNC(1).EQ.4) THEN
                  S = LOG10 (99.0*S + 1.0) / 2.0
                  END IF
               SCRTCH(J) = (MAXINT - 1) * S + 1.5
               END IF
            END IF
 20      CONTINUE
      I = TVCH + 1
      CALL YIMGIO ('WRIT', I, IX, IY, 0, J, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       blue row
      J = 0
      DO 30 I = 1,NP
         IF (MOD(I-1, INC).EQ.0) THEN
            J = J + 1
            IF (DB(I).EQ.FBLANK) THEN
               SCRTCH(J) = 0
            ELSE
               S = MAX (CLR(1,3), MIN (CLR(2,3), DB(I)))
               S = (S - SCR(1,3)) / (SCR(2,3) - SCR(1,3))
               S = MAX (0.0, MIN (1.0, S))
               IF (IFUNC(3).EQ.2) THEN
                  S = SQRT (S)
               ELSE IF (IFUNC(3).EQ.3) THEN
                  S = LOG10 (9.0*S + 1.0)
               ELSE IF (IFUNC(1).EQ.4) THEN
                  S = LOG10 (99.0*S + 1.0) / 2.0
                  END IF
               SCRTCH(J) = (MAXINT - 1) * S + 1.5
               END IF
            END IF
 30      CONTINUE
      I = TVCH + 2
      CALL YIMGIO ('WRIT', I, IX, IY, 0, J, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE TV3C1I (LJ, LX, LY, RED, GREEN, BLUE, HC, FLIP, PPOS,
     *   SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TV3C1I implements single-TV channel enhancement of one of the RGB
C   components using a subimage or all 3 using OFM
C   Inputs:
C      LJ       I          Component number (RGB = 1,2,3; all = 4)
C      LX       I          x dimension of in-core arrays
C      LY       I          y dimension of in-core arrays
C      RED      R(LX,LY)   in-core red array
C      GREEN    R(LX,LY)   in-core green array
C      BLUE     R(LX,LY)   in-core blue array
C   In/out:
C      FLIP     I          Sign of slope of enhancement
C      PPOS     R(2)       Cursor position last used
C   Outputs:
C      HC       I(5,*)     histogram buffer
C      SCRTCH   I(1536)    TV scratch array
C      IRET     I          Return code for DIE
C-----------------------------------------------------------------------
      INTEGER   LJ, FLIP, LX, LY, HC(5,*), SCRTCH(*), IRET
      REAL      RED(LX,LY), GREEN(LX,LY), BLUE(LX,LY), PPOS(2)
C
      INTEGER   I, J, IERR, IBUT, QUAD, ITW(3), IX, IY, RLUT(8200),
     *   GLUT(8200), BLUT(8200), SRLUT(8200), SGLUT(8200), SBLUT(8200),
     *   LEVS, COLS, ITRY, LTRY, ZAND, MASK
      LOGICAL   DOIT, DUMB
      REAL      RPOS(2), F0, X, XS, XO, Y, SLOPE, OFFSET, POW, POS0(2),
     *   RSUM, GSUM, BSUM, LRANGE(2,3), XSCL
      CHARACTER SUBR*6, STRING*20
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RLUT, BUFF2), (GLUT, BUFF2(2051)),
     *   (BLUT, BUFF2(4101))
      EQUIVALENCE (SRLUT, BUFF3), (SGLUT, BUFF3(2051)),
     *   (SBLUT, BUFF3(4101))
      DATA POW, XSCL /0.9, 1.3/
C     DATA POW, XSCL /1.33, 2.0/
C-----------------------------------------------------------------------
      IRET = 0
      LEVS = DPARM(7) + 0.1
      LEVS = MAX (8, MIN (40, LEVS))
      COLS = LEVS * LEVS * LEVS
      ITRY = 0
      LTRY = 1
C                                       turn on correct image
      IF ((LJ.NE.4) .AND. (ITVCH.NE.TVCH)) THEN
         MASK = 2**(ITVCH-1)
         IF (ZAND (MASK, TVLIMG(1)).EQ.0) THEN
            SUBR = 'YSLECT'
            CALL YHOLD ('ONNN', IERR)
            CALL YSLECT ('OFFF', TVCH, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YSLECT ('ONNN', ITVCH, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
            CALL YHOLD ('OFFF', IERR)
            END IF
         END IF
C                                       set up dumb LUTs
      IX = MAXINT + 1
      CALL FILL (IX, 0, BLUT)
      CALL FILL (IX, 0, GLUT)
      CALL FILL (IX, 0, RLUT)
      IX = NLG * NLB
      RSUM = (LUTOUT - 1.0) / (NLR - 1.0)
      GSUM = (LUTOUT - 1.0) / (NLG - 1.0)
      BSUM = (LUTOUT - 1.0) / (NLB - 1.0)
      DO 10 I = 1,MAXINT
         J = MOD (I-1, NLB)
         BLUT(I+1) = J * BSUM + 1.5
         J = (I - 1) / NLB
         J = MOD (J, NLG)
         GLUT(I+1) = J * GSUM + 1.5
         J = (I - 1) / IX
         RLUT(I+1) = J * RSUM + 1.5
 10      CONTINUE
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
         MSGTXT = 'Enhancing the red image'
      ELSE IF (LJ.EQ.2) THEN
         MSGTXT = 'Enhancing the green image'
      ELSE IF (LJ.EQ.3) THEN
         MSGTXT = 'Enhancing the blue image'
      ELSE
         MSGTXT = 'Enhancing all three images together'
         END IF
      CALL MSGWRT (1)
      CALL RCOPY (6, ARANGE, LRANGE)
C                                       read loop
 50   CALL YCURSE ('READ', F, F, RPOS, QUAD, IBUT, IERR)
      SUBR = 'YCURSE'
      IF (IERR.NE.0) GO TO 990
      CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
      IF ((DOIT) .AND. (IBUT.LT.8)) THEN
         IF (IBUT.EQ.1) FLIP = -FLIP
         IF ((IBUT.EQ.2) .OR. (IBUT.EQ.3)) THEN
            SLOPE = 1.
            IF (FSC(1,LJ).NE.0.0) SLOPE = 1.0 / FSC(1,LJ)
            OFFSET = -FSC(2,LJ) * SLOPE
            FLIP = 1
            IF (SLOPE.LT.0.0) FLIP = -1
            RPOS(2) = WINDTV(2) + F0 / (ABS (SLOPE) ** (1.0/POW))
            X = 1.0 - (OFFSET + 1. - (1-FLIP)/2) / SLOPE
            RPOS(1) = WINDTV(1) - 1.0 +
     *         (WINDTV(3)-WINDTV(1)+1.) * (X/XSCL + 0.5)
            IF ((RPOS(1).LE.WINDTV(1)) .OR. (RPOS(1).GE.WINDTV(3)) .OR.
     *         (RPOS(2).LE.WINDTV(2)) .OR. (RPOS(2).GE.WINDTV(4))) THEN
               MSGTXT = 'Unable to represent this as cursor position' //
     *            ' ***********'
               CALL MSGWRT (6)
               MSGTXT = 'Reload full image NOW to avoid problems' //
     *            ' ***********'
               CALL MSGWRT (6)
               SC(1,LJ) = SLOPE
               SC(2,LJ) = OFFSET
               GO TO 990
               END IF
            CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
            SUBR = 'YCURSE'
            IF (IERR.NE.0) GO TO 990
            END IF
         X = ((RPOS(1) - WINDTV(1) + 1.0) /
     *      (WINDTV(3) - WINDTV(1) + 1.0) - 0.5) * XSCL
         SLOPE = ((F0 / MAX (1., RPOS(2)-WINDTV(2))) ** POW) *  FLIP
         OFFSET = 1.0 / REAL(LUTOUT)
         OFFSET = ((OFFSET + X) * SLOPE + (1 - FLIP) / 2) - OFFSET
         SC(1,LJ) = SLOPE
         SC(2,LJ) = OFFSET
C                                       subimage: display
         IF (LJ.LE.3) THEN
            DUMB = ITVCH.NE.TVCH
            IF (IBUT.NE.0) ITRY = 0
C                                       init the histogram et al.
            IF (DUMB) THEN
               IX = LEVS * LEVS
               DO 55 I = 1,COLS
                  HC(1,I) = 0
                  HC(2,I) = (I - 1) / IX
                  J = (I - 1) / LEVS
                  HC(3,I) = MOD (J, LEVS)
                  HC(4,I) = MOD (I-1, LEVS)
 55               CONTINUE
               END IF
            IX = CATBL2(IICOR)
            IY = CATBL2(IICOR+1)
            CALL YHOLD ('ONNN', IERR)
            DO 60 I = 1,3
               X = ARANGE(2,I)
               Y = ARANGE(1,I)
               XS = SC(1,I)
               XO = SC(2,I)
               IF (XS.EQ.0.0) XS = 1.0
               LRANGE(1,I) = Y + XO * (X - Y) / XS
               LRANGE(2,I) = Y + (1.0 + XO) * (X - Y) / XS
 60            CONTINUE
            MSGTXT = 'Load with simple colors'
            CALL MSGWRT (1)
            SUBR = 'YIMGIO'
            DO 70 I = 1,LY
               CALL OPTLOD (ITVCH, DUMB, DUMB, LX, 1, IX, IY, LRANGE,
     *            PRANGE, RED(1,I), GREEN(1,I), BLUE(1,I), LEVS, HC,
     *            SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 990
               IY = IY + 1
 70            CONTINUE
C                                       turn dumb to smart
            IF (DUMB) THEN
C                                       show the dumb
               IF (DOGRC) CALL YHOLD ('OFFF', IERR)
               IX = 2 ** (ITVCH - 1)
               SUBR = 'YLUT'
               CALL YLUT ('WRIT', IX, 1, T, BLUT, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL YLUT ('WRIT', IX, 2, T, GLUT, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL YLUT ('WRIT', IX, 4, T, RLUT, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL YHOLD ('OFFF', IERR)
C                                       not every time
               ITRY = MOD (ITRY, LTRY) + 1
               IF (ITRY.EQ.1) THEN
C                                       optimize
                  CALL OPTLUT (LEVS, COLS, HC, SRLUT, SGLUT, SBLUT)
C                                       reload
                  CALL YHOLD ('ONNN', IERR)
                  SUBR = 'YIMGIO'
                  IX = CATBL2(IICOR)
                  IY = CATBL2(IICOR+1)
                  DO 80 I = 1,LY
                     CALL OPTLOD (ITVCH, F, F, LX, 1, IX, IY, LRANGE,
     *                  PRANGE, RED(1,I), GREEN(1,I), BLUE(1,I), LEVS,
     *                  HC, SCRTCH, IERR)
                     IF (IERR.NE.0) GO TO 990
                     IY = IY + 1
 80                  CONTINUE
                  SUBR = 'YLUT'
                  IF (DOGRC) CALL YHOLD ('OFFF', IERR)
                  IX = 2 ** (ITVCH - 1)
                  CALL YLUT ('WRIT', IX, 1, T, SBLUT, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL YLUT ('WRIT', IX, 2, T, SGLUT, IERR)
                  IF (IERR.NE.0) GO TO 990
                  CALL YLUT ('WRIT', IX, 4, T, SRLUT, IERR)
                  IF (IERR.NE.0) GO TO 990
                  END IF
               END IF
C                                       OFM
         ELSE
            IX = OFMINP + 1
            CALL RFILL (IX, 0.0, BUFF1)
            IX = LUTOUT + 1
            OFFSET = 1.0 / REAL(OFMOUT)
            SLOPE = OFFSET * (OFMOUT + 1.0) / REAL (IX)
            SLOPE = SLOPE * SC(1,4)
            OFFSET = OFFSET * SC(1,4) + SC(2,4)
            DO 90 I = 2,IX
               X = I * SLOPE - OFFSET
               BUFF1(I) = MAX (0.0, MIN (1.0, X))
 90            CONTINUE
            SUBR = 'YOFM'
            CALL YOFM ('WRIT', 7, T, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         IF (DOGRC) THEN
            CALL YHOLD ('ONNN', IERR)
            IX = RPOS(1) + 0.5
            IY = RPOS(2) + 0.5
            WRITE (STRING,1050) IX, IY
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
 1050 FORMAT ('TV X=',I4,' Y=',I4)
 1990 FORMAT ('TV3C1I: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE TV3C1L (SUBIMG, LX, LY, LEVS, COLS, HC, RED, GREEN,
     *   BLUE, SCRTCH, IRET)
C-----------------------------------------------------------------------
C   TV3C1L implements a color optimization scheme borrowed loosely from
C   ppmquant written by Jef Poskanzer.  It loads the 3 colored images
C   to one TV memory using a dumb color scheme initially and the
C   previous optimization later.  Then it computes new LUTs that are
C   optimized for the current colors and reloads the image.
C   Inputs:
C      SUBIMG   L           Is there a subimage picture to erase?
C      LX       I           x dimension of in-core arrays
C      LY       I           y dimension of in-core arrays
C      LEVS     I           Number of sub levels for each base color
C      COLS     I           Total number of colors: LEVS**3
C   Outputs:
C      HC       I(5,COLS)   in-core histogram array
C      RED      R(LX,LY)    in-core red array
C      GREEN    R(LX,LY)    in-core green array
C      BLUE     R(LX,LY)    in-core blue array
C      SCRTCH   I(1536)     TV scratch array
C      IRET     I           Return code for DIE
C-----------------------------------------------------------------------
      LOGICAL   SUBIMG
      INTEGER   LX, LY, LEVS, COLS, HC(5,COLS), SCRTCH(*), IRET
      REAL      RED(LX,LY), GREEN(LX,LY), BLUE(LX,LY)
C
      INTEGER   I, J, IERR, II, JJ, IX, IY, TVCODE, RBLK, GBLK, BBLK,
     *   RBIND, GBIND, BBIND, IP, RLUT(8200), GLUT(8200), BLUT(8200),
     *   SX, SY, JX, JY
      REAL      RSUM, GSUM, BSUM, X, SLOPE, OFFSET
      CHARACTER SUBR*12
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       erase subimage
      IF (SUBIMG) THEN
         CALL YHOLD ('ONNN', IERR)
         SUBR = 'YZERO'
         CALL YZERO (ITVCH, IERR)
         IF (IERR.NE.0) GO TO 990
         SUBR = 'YSLECT'
         CALL YSLECT ('OFFF', ITVCH, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YSLECT ('ONNN', TVCH, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YHOLD ('OFFF', IERR)
         END IF
C                                       initial message
      CALL YHOLD ('ONNN', IERR)
      TVCODE = 2 ** (TVCH - 1)
      MSGTXT = 'Start by loading image from disk with simple ' //
     *   'color scheme'
      CALL MSGWRT (1)
C                                       init the LUTs for dumb colors
C                                       Blue lut
      IX = MAXINT + 1
      CALL FILL (IX, 0, BLUT)
      CALL FILL (IX, 0, GLUT)
      CALL FILL (IX, 0, RLUT)
      JJ = NLG * NLB
      RSUM = (LUTOUT - 1.0) / (NLR - 1.0)
      GSUM = (LUTOUT - 1.0) / (NLG - 1.0)
      BSUM = (LUTOUT - 1.0) / (NLB - 1.0)
      DO 10 I = 1,MAXINT
         J = MOD (I-1, NLB)
         BLUT(I+1) = J * BSUM + 1.5
         J = (I - 1) / NLB
         J = MOD (J, NLG)
         GLUT(I+1) = J * GSUM + 1.5
         J = (I - 1) / JJ
         RLUT(I+1) = J * RSUM + 1.5
 10      CONTINUE
C                                       init the histogram et al.
      IX = LEVS * LEVS
      DO 20 I = 1,COLS
         HC(1,I) = 0
         HC(2,I) = (I - 1) / IX
         J = (I - 1) / LEVS
         HC(3,I) = MOD (J, LEVS)
         HC(4,I) = MOD (I-1, LEVS)
 20      CONTINUE
C                                       init image windows
      JX = CATBLK(IIWIN+2) - CATBLK(IIWIN+0) + 1
      JY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), RBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), GBLK,
     *   IERR)
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), BBLK,
     *   IERR)
      RBLK = RBLK + 1
      GBLK = GBLK + 1
      BBLK = BBLK + 1
C                                       init disk read
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, RBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, GBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *   CATBL3(IIWIN), BUFF3, JBUFSZ, BBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1) - 1
      SUBR = 'MDISK'
      SY = 0
      DO 40 J = 1,JY
C                                       read data rows
         CALL MDISK ('READ', LUN1, IND1, BUFF1, RBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, GBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN3, IND3, BUFF3, BBIND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       copy to sub-image
            JJ = (J - 1) / YINC + 1
            IF ((JJ.GE.IDWIN(2)) .AND. (JJ.LE.IDWIN(4)) .AND.
     *         (MOD(JJ-1, IDY).EQ.0)) THEN
               SY = SY + 1
               SX = 0
               II = IDWIN(1)
               DO 35 I = II,NX,IDX
                  IF (I.LE.IDWIN(3)) THEN
                     IP = (I - 1) * XINC
                     SX = SX + 1
                     RED(SX,SY) = BUFF1(RBIND+IP)
                     GREEN(SX,SY) = BUFF2(GBIND+IP)
                     BLUE(SX,SY) = BUFF3(BBIND+IP)
                     END IF
 35               CONTINUE
               END IF
C                                       scale data rows and tvlod
            IY = IY + 1
            CALL OPTLOD (TVCH, T, T, NX, XINC, IX, IY, ARANGE, PRANGE,
     *         BUFF1(RBIND), BUFF2(GBIND), BUFF3(BBIND), LEVS, HC,
     *         SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               SUBR = 'YIMGIO'
               GO TO 990
               END IF
            END IF
 40      CONTINUE
C                                       reset the OFM
      II = OFMINP + 1
      CALL RFILL (II, 0.0, BUFF1)
      II = LUTOUT + 1
      OFFSET = 1.0 / REAL(OFMOUT)
      SLOPE = OFFSET * (OFMOUT + 1.0) / REAL (II)
      DO 50 I = 2,II
         X = I * SLOPE - OFFSET
         BUFF1(I) = MAX (0.0, MIN (1.0, X))
 50      CONTINUE
      IF (DOGRC) CALL YHOLD ('OFFF', IERR)
      SUBR = 'YOFM'
      CALL YOFM ('WRIT', 7, T, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL YLUT ('WRIT', TVCODE, 1, T, BLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (BLUE)'
         GO TO 990
         END IF
      CALL YLUT ('WRIT', TVCODE, 2, T, GLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (GREEN)'
         GO TO 990
         END IF
      CALL YLUT ('WRIT', TVCODE, 4, T, RLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (RED)'
         GO TO 990
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                       optimize the LUTs
C                                       optimize
      CALL OPTLUT (LEVS, COLS, HC, RLUT, GLUT, BLUT)
C                                       init disk read
      CALL YHOLD ('ONNN', IERR)
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, RBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, GBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *   CATBL3(IIWIN), BUFF3, JBUFSZ, BBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      IX = CATBLK(IICOR)
      IY = CATBLK(IICOR+1) - 1
      SUBR = 'MDISK'
      DO 130 J = 1,JY
C                                       read data rows
         CALL MDISK ('READ', LUN1, IND1, BUFF1, RBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, GBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN3, IND3, BUFF3, BBIND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       every YINC'th row
         IF (MOD(J-1,YINC).EQ.0) THEN
C                                       scale data rows and tvlod
            IY = IY + 1
            CALL OPTLOD (TVCH, F, F, NX, XINC, IX, IY, ARANGE, PRANGE,
     *         BUFF1(RBIND), BUFF2(GBIND), BUFF3(BBIND), LEVS, HC,
     *         SCRTCH, IERR)
            IF (IERR.NE.0) THEN
               SUBR = 'YIMGIO'
               GO TO 990
               END IF
            END IF
 130     CONTINUE
      IF (DOGRC) CALL YHOLD ('OFFF', IERR)
      CALL YLUT ('WRIT', TVCODE, 1, T, BLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (BLUE)'
         GO TO 990
         END IF
      CALL YLUT ('WRIT', TVCODE, 2, T, GLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (GREEN)'
         GO TO 990
         END IF
      CALL YLUT ('WRIT', TVCODE, 4, T, RLUT, IERR)
      IF (IERR.NE.0) THEN
         SUBR = 'YLUT (RED)'
         GO TO 990
         END IF
      CALL YHOLD ('OFFF', IERR)
C                                       error messages
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('TV3C1L: ERROR',I5,' GENERATED BY ROUTINE ',A)
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
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
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
      SUBROUTINE OPTLOD (CHAN, DOSUM, DUMB, NP, NINC, IX, IY, SCR, CLR,
     *   DR, DG, DB, LEVS, HC, SCRTCH, IERR)
C-----------------------------------------------------------------------
C   OPTLOD does the scaling, clipping of an RGB triple for a row,
C   looks up the color in a large table to translate, then loads the row
C   to 1 channel on the TV with "optimal" coloring.
C   Inputs:
C      CHAN     I        Channel to use
C      DOSUM    L        Sum new histogram
C      DUMB     L        Use simple colors not in table?
C      NP       I        Number of points TO BE PLOTTED
C      NINC     I        Increment in row between plotted points
C      IX       I        X position on the TV
C      IY       I        Y position on the TV
C      SCR      R(2,3)   Min/max for red, green, blue for scaling
C      CLR      R(2,3)   Min/max for RGB for clipping
C      DR       R(*)     red row  (* = (NP-1) * NINC + 1)
C      DG       R(*)     green row
C      DB       R(*)     blue row
C      LEVS     I        Number of levels per color in histogram
C      HC       I(5,*)   (5,..) translation table
C   Output:
C      SCRTCH   I(*)     output row
C      IERR     I        Return code from YIMGIO
C-----------------------------------------------------------------------
      LOGICAL   DOSUM, DUMB
      INTEGER   CHAN, NP, NINC, IX, IY, LEVS, HC(5,*), SCRTCH(*), IERR
      REAL      DR(NP), DG(NP), DB(NP), SCR(2,3), CLR(2,3)
C
      INTEGER   I, IR, IG, IB, II, JJ, L, J
      REAL      R, G, B
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      JJ = LEVS * LEVS * LEVS
C                                       loop over row
      DO 10 I = 1,NP
         J = (I - 1) * NINC + 1
         G = 0.
         B = 0.
         IF (DR(J).NE.FBLANK) THEN
            R = MAX (CLR(1,1), MIN (CLR(2,1), DR(J)))
            R = (R - SCR(1,1)) / (SCR(2,1) - SCR(1,1))
            R = MAX (0.0, MIN (1.0, R))
         ELSE
            R = 0.
            END IF
         IF (DG(J).NE.FBLANK) THEN
            G = MAX (CLR(1,2), MIN (CLR(2,2), DG(J)))
            G = (G - SCR(1,2)) / (SCR(2,2) - SCR(1,2))
            G = MAX (0.0, MIN (1.0, G))
         ELSE
            G = 0.
            END IF
         IF (DB(J).NE.FBLANK) THEN
            B = MAX (CLR(1,3), MIN (CLR(2,3), DB(J)))
            B = (B - SCR(1,3)) / (SCR(2,3) - SCR(1,3))
            B = MAX (0.0, MIN (1.0, B))
         ELSE
            B = 0.
            END IF
C                                       TV levels
         IF (DUMB) THEN
            IR = R * NLR - 0.0001
            IG = G * NLG - 0.0001
            IB = B * NLB - 0.0001
            L = IB + NLB * (IG + NLG * IR) + 1
            SCRTCH(I) = MAX (1, MIN (MAXINT, L))
C                                       histogram levels use
         ELSE
            IR = R * LEVS - 0.0001
            IG = G * LEVS - 0.0001
            IB = B * LEVS - 0.0001
            II = IB + LEVS * (IG + LEVS * IR) + 1
            II = MAX (1, MIN (JJ, II))
            L = HC(5,II)
            SCRTCH(I) = MAX (1, MIN (MAXINT, L))
            END IF
C                                       histogram levels count
         IF (DOSUM) THEN
            IF (DUMB) THEN
               IR = R * LEVS - 0.0001
               IG = G * LEVS - 0.0001
               IB = B * LEVS - 0.0001
               II = IB + LEVS * (IG + LEVS * IR) + 1
               II = MAX (1, MIN (JJ, II))
               END IF
            HC(1,II) = HC(1,II) + 1
            END IF
 10      CONTINUE
C                                       write line to TV
      CALL YIMGIO ('WRIT', CHAN, IX, IY, 0, NP, SCRTCH, IERR)
C
 999  RETURN
      END
      SUBROUTINE TV3COU (IRET)
C-----------------------------------------------------------------------
C   write the image as a 24-bit color PostScript file to the printer or
C   a text file.  The data are now (2003) read from disk with new
C   windows.
C   Outputs:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   RBLK, GBLK, BBLK, IERR, RBIND, GBIND, BBIND, IX, IY,
     *   I, II, PSLUN, BBOX(2,2), II0, IROUND
      REAL      V, HEIGHT, WIDTH, MARGIN
      LOGICAL   DOLAB, ROTATE
      CHARACTER SUBR*6, PSBUFF*72
      INCLUDE 'TVRGB.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 10 I = 1,2
         CATBLK(IIWIN+I-1) = IROUND (OBLC(I,1))
         CATBL2(IIWIN+I-1) = IROUND (OBLC(I,2))
         CATBL3(IIWIN+I-1) = IROUND (OBLC(I,3))
         CATBLK(IIWIN+I+1) = IROUND (OTRC(I,1))
         CATBL2(IIWIN+I+1) = IROUND (OTRC(I,2))
         CATBL3(IIWIN+I+1) = IROUND (OTRC(I,3))
 10      CONTINUE
      NY = CATBLK(IIWIN+3) - CATBLK(IIWIN+1) + 1
      NX = CATBLK(IIWIN+2) - CATBLK(IIWIN) + 1
      DO 15 I = 3,7
         CATBLK(IIDEP+I-3) = IROUND (OBLC(I,1))
         CATBL2(IIDEP+I-3) = IROUND (OBLC(I,2))
         CATBL3(IIDEP+I-3) = IROUND (OBLC(I,3))
 15      CONTINUE
      PSLUN = 3
      DOLAB = REASON.NE.' '
      WIDTH = 8.5 * 72.
      HEIGHT = 11.0 * 72.
      MARGIN = 0.5 * 72.
C                                       find window
      CALL CALCBB (CATBLK(IIWIN), WIDTH, HEIGHT, MARGIN, DOLAB, BBOX,
     *   ROTATE)
C                                       Open PostScript file and write
C                                       prologue:
      CALL PSPLOG (PSLUN, OUTFIL, BBOX, DOLAB, ROTATE, IRET)
      IF (IRET.NE.0) THEN
         IRET = 7
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                        Begin output page:
      CALL PSPSET (PSLUN, CATBLK(IIWIN), BBOX, DOLAB, REASON, ROTATE,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         CALL MSGWRT (8)
         IRET = 7
         GO TO 999
         END IF
C                                       init image windows
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CATBLK(IIDEP), RBLK,
     *   IERR)
      CALL COMOFF (CATBL2(KIDIM), CATBL2(KINAX), CATBL2(IIDEP), GBLK,
     *   IERR)
      CALL COMOFF (CATBL3(KIDIM), CATBL3(KINAX), CATBL3(IIDEP), BBLK,
     *   IERR)
      RBLK = RBLK + 1
      GBLK = GBLK + 1
      BBLK = BBLK + 1
C                                       init disk read
      SUBR = 'MINIT'
      CALL MINIT ('READ', LUN1, IND1, CATBLK(KINAX), CATBLK(KINAX+1),
     *   CATBLK(IIWIN), BUFF1, JBUFSZ, RBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN2, IND2, CATBL2(KINAX), CATBL2(KINAX+1),
     *   CATBL2(IIWIN), BUFF2, JBUFSZ, GBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL MINIT ('READ', LUN3, IND3, CATBL3(KINAX), CATBL3(KINAX+1),
     *   CATBL3(IIWIN), BUFF3, JBUFSZ, BBLK, IERR)
      IF (IERR.NE.0) GO TO 990
      SUBR = 'MDISK'
      II = 1
      DO 40 IY = 1,NY
C                                       read data rows
         CALL MDISK ('READ', LUN1, IND1, BUFF1, RBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN2, IND2, BUFF2, GBIND, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL MDISK ('READ', LUN3, IND3, BUFF3, BBIND, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       scale data rows, put in buffer
         DO 30 IX = 1,NX
            II0 = II
            V = BUFF1(RBIND+IX-1)
            IF (V.NE.FBLANK) THEN
               V = MAX (PRANGE(1,1), MIN (PRANGE(2,1), V))
               V = (V - ARANGE(1,1)) / (ARANGE(2,1) - ARANGE(1,1))
               V = MAX (0.0, MIN (1.0, V))
               IF (IFUNC(1).EQ.2) THEN
                  V = SQRT (V)
               ELSE IF (IFUNC(1).EQ.3) THEN
                  V = LOG10 (9.0*V + 1.0)
               ELSE IF (IFUNC(1).EQ.4) THEN
                  V = LOG10 (99.0*V + 1.0) / 2.0
                  END IF
               V = V ** (1.0 / RGBGAM(1))
               I = MAX (0.0, MIN (1.0, V)) * 255.0 + 0.4999
            ELSE
               I = 0
               END IF
            CALL ZHEX (I, 2, PSBUFF(II:II+1))
            II = II + 2
            V = BUFF2(GBIND+IX-1)
            IF (V.NE.FBLANK) THEN
               V = MAX (PRANGE(1,2), MIN (PRANGE(2,2), V))
               V = (V - ARANGE(1,2)) / (ARANGE(2,2) - ARANGE(1,2))
               V = MAX (0.0, MIN (1.0, V))
               IF (IFUNC(2).EQ.2) THEN
                  V = SQRT (V)
               ELSE IF (IFUNC(2).EQ.3) THEN
                  V = LOG10 (9.0*V + 1.0)
               ELSE IF (IFUNC(2).EQ.4) THEN
                  V = LOG10 (99.0*V + 1.0) / 2.0
                  END IF
               V = V ** (1.0 / RGBGAM(2))
               I = MAX (0.0, MIN (1.0, V)) * 255.0 + 0.4999
            ELSE
               I = 0
               END IF
            CALL ZHEX (I, 2, PSBUFF(II:II+1))
            II = II + 2
            V = BUFF3(BBIND+IX-1)
            IF (V.NE.FBLANK) THEN
               V = MAX (PRANGE(1,3), MIN (PRANGE(2,3), V))
               V = (V - ARANGE(1,3)) / (ARANGE(2,3) - ARANGE(1,3))
               V = MAX (0.0, MIN (1.0, V))
               IF (IFUNC(3).EQ.2) THEN
                  V = SQRT (V)
               ELSE IF (IFUNC(3).EQ.3) THEN
                  V = LOG10 (9.0*V + 1.0)
               ELSE IF (IFUNC(3).EQ.4) THEN
                  V = LOG10 (99.0*V + 1.0) / 2.0
                  END IF
               V = V ** (1.0 / RGBGAM(3))
               I = MAX (0.0, MIN (1.0, V)) * 255.0 + 0.4999
            ELSE
               I = 0
               END IF
            CALL ZHEX (I, 2, PSBUFF(II:II+1))
            II = II + 2
            IF ((DOOUTP.GT.1.5) .AND. (PSBUFF(II0:II0+5).EQ.'000000'))
     *         PSBUFF(II0:II0+5) = 'FFFFFF'
C                                        Flush buffer
            IF (II.EQ.73) THEN
               CALL ZLASIO ('WRIT', PSLUN, ' ', 72, PSBUFF, IRET)
               II = 1
               IF (IRET.NE.0) GO TO 999
               END IF
 30         CONTINUE
 40      CONTINUE
      II = II - 1
      IF (II.GT.0) THEN
         CALL ZLASIO ('WRIT', PSLUN, ' ', II, PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        finish off PostScript
      CALL PSFIN (PSLUN, OUTFIL, COPIES, IRET)
      GO TO 999
C                                       error messages
 990  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1990) IERR, SUBR
         CALL MSGWRT (7)
         IRET = 1
         IF (SUBR(1:1).EQ.'Y') IRET = 2
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT OPEN OUTPUT FILE: PSPLOG ERROR ',I2)
 1005 FORMAT ('CANNOT WRITE PAGE SETUP: PSPSET ERROR ',I2)
 1990 FORMAT ('TV3COU: ERROR',I5,' GENERATED BY ROUTINE ',A)
      END
      SUBROUTINE PSPLOG (PSLUN, OUTFIL, BBOX, DOLAB, ROTATE, IRET)
C-----------------------------------------------------------------------
C   Open a print file with name OUTFIL on logical unit number LUN and
C   write a PostScript prologue to it. The prologue conforms to version
C   3.0 of the PostScript document structuring conventions.
C   Inputs:
C      PSLUN     I           Logical unit number
C      OUTFIL    C*48        File name
C      BBOX      I(2,2)      TBLC and TRC or bounding box in points
C      DOLAB     L           labeling will be done
C      ROTATE    L
C   Outputs:
C      IRET      I           Status code: 0 => OK
C-----------------------------------------------------------------------
      INTEGER   PSLUN, BBOX(2,2), IRET
      CHARACTER OUTFIL*48
      LOGICAL   ROTATE, DOLAB
C                                        PSBUFF = line buffer
      CHARACTER PSBUFF*80
C                                        DATE = current date (year,
C                                               month and day)
C                                        TIME = current time (hours,
C                                               minutes and seconds)
      INTEGER   DATE(3), TIME(3)
C                                        NEWBOX = bounding box after
C                                                 allowing for a
C                                                 label.
      INTEGER   NEWBOX(2, 2)
C
      INTEGER   I, J
C
      INTEGER   ITRIM
      EXTERNAL  ITRIM
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Open the output file:
      CALL ZLASIO ('OPEN', PSLUN, OUTFIL, 0, PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write %!PS-Adobe comment:
      PSBUFF = '%!PS-Adobe-3.0 EPSF-3.0'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        If a label will be added
C                                        note that the Helvetica
C                                        font will be needed:
      IF (DOLAB) THEN
         PSBUFF = '%%DocumentNeededResources: font Helvetica'
         CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Write bounding box comment:
      DO 20 I = 1,2
         DO 10 J = 1,2
            NEWBOX(I,J) = BBOX(I,J) + 5 * (2*J-3)
   10       CONTINUE
   20    CONTINUE
      IF (DOLAB) THEN
         IF (ROTATE) THEN
            NEWBOX(1,2) = NEWBOX(1,2) + 25
         ELSE
            NEWBOX(2,1) = NEWBOX(2,1) - 25
            END IF
         END IF
      WRITE (PSBUFF, 1000) NEWBOX
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write creator comment:
      PSBUFF = '%%Creator: AIPS task TVCPS'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write creation date comment:
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      WRITE (PSBUFF, 1001) DATE, TIME
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write document data comment:
      PSBUFF = '%%DocumentData: Clean7Bit'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write extensions comment
      PSBUFF = '%%Extensions: CMYK'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '%%Requirements: color'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write for comment:
      WRITE (PSBUFF, 1002) NLUSER
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write pages comment:
      PSBUFF = '%%Pages: 1'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Finish header section:
      PSBUFF = '%%EndComments'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Write a null prologue:
      PSBUFF = '%%BeginProlog'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '%%EndProlog'
      CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        If a label is required then
C                                        the label font must be
C                                        included in the document
C                                        setup.
      IF (DOLAB) THEN
         PSBUFF = '%%BeginSetup'
         CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         PSBUFF = '%%IncludeResource: font Helvetica'
         CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         PSBUFF = '%%EndSetup'
         CALL ZLASIO ('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF,
     *      IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
      IRET = 0
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('%%BoundingBox: ', 4I5)
 1001 FORMAT ('%%CreationDate: ', I4.4, '-', I2.2, '-', I2.2, 'T',
     *   I2.2, ':', I2.2, ':', I2.2)
 1002 FORMAT ('%%For: AIPS user number ', I4)
      END
      SUBROUTINE PSPSET (PSLUN, IMGWIN, BBOX, DOLAB, REASON, ROTATE,
     *   IRET)
C-----------------------------------------------------------------------
C   Begin new page and write out everything except for the sctual
C   image.
C   Inputs:
C      PSLUN    I           LUN of open printer file
C      IMGWIN   I(4)        window into image
C      BBOX     I(2, 2)     Bounding box for image
C      DOLAB    L           True if a label should be added.
C      REASON   C*40        Character label to put on plot
C   Output
C      IERR     I           Status: 0 => OK
C                                   non-zero => ZLASIO error
C-----------------------------------------------------------------------
      INTEGER   PSLUN, IMGWIN(4), BBOX(2,2), IRET
      LOGICAL   DOLAB, ROTATE
      CHARACTER REASON*(*)
C
C                                        PSBUFF = character buffer
      CHARACTER PSBUFF*80
C                                        FSIZE = font size for label
C                                                in points
      INTEGER   FSIZE, ITRIM, I, WWIDTH, WHT, PIXELS, OLINES
C                                        LWIDTH = width of the label
C                                                 in points for 20pt
C                                                 Helvetica
      REAL      LWIDTH, XWIDTH
      PARAMETER (LWIDTH = 155.0)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Start page:
      PSBUFF = '%%Page 1 1'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Rotation
      IF (ROTATE) THEN
         PSBUFF = ' 90 rotate '
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (PSBUFF,1004) 0, -BBOX(1,2)-BBOX(1,1)
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       If outputting a label,
C                                       calculate the font size, and
C                                       draw the label in black
      IF (DOLAB) THEN
         I = ITRIM (REASON)
         XWIDTH = (I * LWIDTH) / 14
         FSIZE = INT (20.0 * (BBOX(1,2) - BBOX(1,1)) / XWIDTH)
         FSIZE = MIN (20, FSIZE)
         WRITE (PSBUFF,1000) FSIZE
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (ROTATE) THEN
            WRITE (PSBUFF,1001) BBOX(2,1) + 2, BBOX(1,1) - 23
         ELSE
            WRITE (PSBUFF,1001) BBOX(1,1) + 2, BBOX(2,1) - 23
            END IF
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         WRITE (PSBUFF,1002) REASON(1:I)
         CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                        Save state:
      PSBUFF = '/pgsave save def                       % save state'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Define a string capable of
C                                        holding one line of the image
      WRITE (PSBUFF,1003) 3 * (IMGWIN(3) - IMGWIN(1) + 1)
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                        Set up a transformation matrix
C                                        that maps the unit square to
C                                        the bounding box
      IF (ROTATE) THEN
         WRITE (PSBUFF,1004) BBOX(2,1), BBOX(1,1)
      ELSE
         WRITE (PSBUFF,1004) BBOX(1,1), BBOX(2,1)
         END IF
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (ROTATE) THEN
         WRITE (PSBUFF,1005) BBOX(2,2)-BBOX(2,1), BBOX(1,2)-BBOX(1,1)
      ELSE
         WRITE (PSBUFF,1005) BBOX(1,2)-BBOX(1,1), BBOX(2,2)-BBOX(2,1)
         END IF
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C
C                                       Calculate window width and
C                                       height:
      WWIDTH = IMGWIN(3) - IMGWIN(1) + 1
      WHT = IMGWIN(4) - IMGWIN(2) + 1
C                                       Calculate number of pixels:
      PIXELS = WWIDTH * WHT
C                                       Calculate number of lines of
C                                       output (if we tell the
C                                       PostScript interpreter the
C                                       number of lines of data it
C                                       can avoid parsing the data):
      OLINES = (PIXELS + 11) / 12 + 1
C                                       Write out PostScript
C                                       operators:
      WRITE (PSBUFF,1010) WWIDTH, WHT, WWIDTH, WHT
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '{currentfile picst readhexstring pop}'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = 'false 3'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      WRITE (PSBUFF,1011) OLINES
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = 'colorimage'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
C
  999 RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/Helvetica findfont ', I2, ' scalefont setfont')
 1001 FORMAT (I4, ' ', I4, ' moveto')
 1002 FORMAT ('(',A,') show')
 1003 FORMAT ('/picst',I7,' string def')
 1004 FORMAT (I8,I8,' translate')
 1005 FORMAT (I8,I8,' scale')
 1010 FORMAT (I5,1X,I5,' 8 [',I5,' 0 0 ',I5,' 0 0 ]')
 1011 FORMAT ('%%BeginData:',I8,' hex lines')
      END
      SUBROUTINE PSFIN (PSLUN, OUTFIL, COPIES, IRET)
C-----------------------------------------------------------------------
C   Close the output PostScript file
C   Input:
C      PSLUN    I       Lun in use
C      OUTFIL   C*(*)   File name
C      COPIES   I       Number of copies to print
C   Output:
C      IRET     I       Return status 0 -> success
C                          2 -> could not close output file
C-----------------------------------------------------------------------
      INTEGER   PSLUN, COPIES, IRET
      CHARACTER OUTFIL*(*)
C
      CHARACTER PSBUFF*80
      INTEGER   ITRIM, I
      EXTERNAL  ITRIM
C-----------------------------------------------------------------------
C                                       Finish off the PostScript file
      PSBUFF = '%%EndData'
      CALL ZLASIO ('WRIT', PSLUN, ' ', ITRIM(PSBUFF), PSBUFF, IRET)
      PSBUFF = 'pgsave restore'
      CALL ZLASIO('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (COPIES.GT.1) THEN
         PSBUFF = 'copypage'
         DO 20 I = 2,COPIES
            CALL ZLASIO('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
 20         CONTINUE
         END IF
      PSBUFF = 'showpage'
      CALL ZLASIO('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      PSBUFF = '%%EOF'
      CALL ZLASIO('WRIT', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZLASIO('CLOS', PSLUN, OUTFIL, ITRIM(PSBUFF), PSBUFF, IRET)
C
  999 RETURN
      END
      SUBROUTINE CALCBB (IMGWIN, WIDTH, HEIGHT, MARGIN, DOLAB, BBOX,
     *   ROTATE)
C-----------------------------------------------------------------------
C   Calculate the largest centered bounding box that will hold a scaled
C   image of the current TV window with a mimimum margin of MARGIN on
C   each side while preserving the TV aspect ratio.
C
C   Inputs:
C      IMGWIN   I(4)     Current TV window (from YWINDO)
C      WIDTH    R        Width of output media (points)
C      HEIGHT   R        Height of output media (points)
C      MARGIN   R        Minimum margin (points)
C      DOLAB    L        Should space be reserved for a label.
C   Outputs:
C      BBOX     I(2, 2)  TBLC and TRC of bounding box (points)
C      ROTATE   L        Use
C-----------------------------------------------------------------------
      INTEGER   IMGWIN(4), BBOX(2,2)
      REAL      WIDTH, HEIGHT, MARGIN
      LOGICAL   DOLAB, ROTATE
C                                        ASPECT = TV aspect ratio
C                                        MAXWID = maximum image width
C                                        MAXHT  = maximum image height
C                                        SCALE  = scaling factor
      REAL      MAXWID, MAXHT, PCH, XSZ, YSZ, ASPECT, SCALE
C-----------------------------------------------------------------------
      MAXWID = WIDTH - 2.0 * MARGIN
      MAXHT  = HEIGHT - 2.0 * MARGIN
C                                        Reserve space for a label, if
C                                        required:
      PCH = 0
      IF (DOLAB) PCH = 25
C                                       Scale to fit page:
      XSZ = IMGWIN(3) - IMGWIN(1) + 1
      YSZ = IMGWIN(4) - IMGWIN(2) + 1
C                                        Calculate scaling factor:
      SCALE = MAXWID / XSZ
      IF (YSZ*SCALE.GT.MAXHT-PCH) SCALE = (MAXHT-PCH) / YSZ
      ASPECT = (MAXWID-PCH) / YSZ
      IF (XSZ*ASPECT.GT.MAXHT) ASPECT = MAXHT / XSZ
C                                        Calculate TBLC of bounding box:
      IF (ASPECT.GT.SCALE) THEN
         ROTATE = .TRUE.
         BBOX(1,1) = INT ((WIDTH - ASPECT*YSZ) / 2.0)
         BBOX(2,1) = INT ((HEIGHT - ASPECT*XSZ) / 2.0)
      ELSE
         ROTATE = .FALSE.
         BBOX(1,1) = INT ((WIDTH - SCALE*XSZ) / 2.0)
         BBOX(2,1) = INT ((HEIGHT - SCALE*YSZ) / 2.0)
         END IF
C                                        Calculate TRC of bounding box:
      BBOX(1,2) = INT (WIDTH - BBOX(1,1))
      BBOX(2,2) = INT (HEIGHT - BBOX(2,1))
C                                        Adjust vertical positioning
C                                        for label.
      IF (DOLAB) THEN
         IF (ROTATE) THEN
            BBOX(1,1) = BBOX(1,1) - 12.5
            BBOX(1,2) = BBOX(1,2) - 12.5
         ELSE
            BBOX(2,1) = BBOX(2,1) + 12.5
            BBOX(2,2) = BBOX(2,2) + 12.5
            END IF
         END IF
C
  999 RETURN
      END
