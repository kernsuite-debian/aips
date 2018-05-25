LOCAL INCLUDE 'HGEOM.INC'
C                                       local variables for HGEOM
      REAL      XPARM(10), ZMX, ZMN, BLC1, BLC2, TRC1, TRC2,
     *   AA(3,3), AAI(3,3)
      INTEGER   BLNCNT(2)
      INTEGER   MORD, NPXI, NRWI, NPLANE, NPXO, NRWO, NPZI, IBLC1,
     *   IBLC2, ITRC1, ITRC2, ICALL, NLUN, ERROR
      LOGICAL   INVERT, AXONLY, ZEROFL, LBLANK
      COMMON /CXYG/ XPARM, ZMX,  ZMN, BLC1, BLC2, TRC1, TRC2, AA, AAI,
     *   BLNCNT, INVERT, AXONLY, ZEROFL, LBLANK, MORD, NPXI, NRWI,
     *   NPLANE, NPXO, NRWO, NPZI, IBLC1, IBLC2, ITRC1, ITRC2, ICALL,
     *   NLUN, ERROR
C                                                          End HGEOM
LOCAL END
LOCAL INCLUDE 'HGEOM2.INC'
C                             Everett Interpolation internal variables:
      REAL      BCOEF(49), SV(14), SAVWTS(8,61)
      INTEGER   MORD2, IS0, NVALS
      LOGICAL   LRECUR
      COMMON /CEVI/ BCOEF, SV, SAVWTS, LRECUR, MORD2, IS0, NVALS
C                                                          End GEOM2
LOCAL END
      PROGRAM HGEOM
C-----------------------------------------------------------------------
C! Geometric tranform of one image to geometry of the header of another.
C# Map Coordinates Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2002, 2008, 2010, 2012
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
C   AIPS task to transform an input image so that its geometry is
C   consistent with that of another input image (headers the same).
C   NOTE: the working array is dimensioned to be 700000 in this version
C   of HGEOM, enough to fit an 836-square image in memory.  The number
C   700000 appears in two places in the source:
C          declaration of R4(), and
C          DATA statement for variable NR4.
C   The memory required to link and execute HGEOM can be changed
C   easily by altering these two numbers.
C   Inputs (adverbs from AIPS):
C      USERID        Input image user ID
C      INNAME(3)     Input image name
C      INCLASS(2)    Input image class
C      INSEQ         First input image seq no.
C      INDISK        Input image disk: 0 = any
C      IN2NAME(3)    Second input image name
C      IN2CLASS(2)   Second input image class
C      IN2SEQ        Second input image seq no.
C      IN2DISK       Second input image disk: 0 = any
C      OUTNAME(3)    Output image name default INNAME
C      OUTCLASS(2)   Output image class default 'GEOM  '
C      OUTSEQ        Output image seq no. 0 => lowest unique
C      OUTDISK       Output image disk
C      BLC(7)        the coordinate in the source files to
C                    become the bottom left hand coordinate (1,1)
C                    of the subimages.  BLC(1) is the X value and
C                    BLC(2) is the Y value.  The first coordinate
C                    IN the source maps is (1,1). Any BLC(I) equal
C                    to zero defaults to 1.
C      TRC(7)        the coordinate in the source files to
C                    become the top right hand corner of the
C                    subimages.  The conventions used for BLC hold.
C      IMSIZE(2)     Output image size [1=columns, 2=rows]
C      APARM(10)     transformation parameters:
C                       (1)=interp.order, (2)=zero flag
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, INNAM*36, IN2NA*36, OTNAM*36, HILINE*72,
     *   TERPS(4)*8, SUBS(10)*6
      INTEGER  NPARM, IPXI
      HOLLERITH XINNAM(3), XINCLA(2), XIN2NA(3), XIN2CL(2), XOTNAM(3),
     *   XOTCLA(2), MA
      CHARACTER INNAME*12, INCLAS*6, IN2NAM*12, IN2CLA*6, OTNAME*12,
     *   OTCLAS*6, PTYPE*2
      REAL      RPARM(47), INSEQ, INDISK, IN2SEQ, IN2DIS, RUSER, OTSEQ,
     *   OTDISK, BLC(7), TRC(7), IMSIZE(2), APARM(10)
C                                       Work space and pointers:
C                                       NX = 1536 more or less
      INTEGER   R4DIM
      PARAMETER (R4DIM=2)
      REAL      R4(R4DIM)
      INTEGER   I2(R4DIM)
      LONGINT   NEXT, PR4, LXI, LYI, LZO, LPXT, LZI, LPZI
      INTEGER   NCLT, NPXT, NR4, I4TEMP
      DOUBLE PRECISION INCATD(128)
      REAL      INCATR(256)
      INTEGER   INCATI(256), ASEQ, BSEQ
      REAL      XFAC, YFAC
      INTEGER   INVOL, INCNO, OUTVOL, OUTCNO, OUTPTR, INLUN,
     *   IN2LUN, OTLUN, HLUN(2), LABEL, IRET, I, J, USID, I2SEQ, I2VOL,
     *   IERR, IN2VOL, IDEP(5), IDEPT(5)
      LOGICAL   T, LWWSUB(10), MAGIC
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'HGEOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'HGEOM2.INC'
      EQUIVALENCE (XINNAM,     RPARM( 1)),
     *            (XINCLA,     RPARM( 4)), (INSEQ,      RPARM( 6)),
     *            (INDISK,     RPARM( 7)), (XIN2NA,     RPARM( 8)),
     *            (XIN2CL,     RPARM(11)), (IN2SEQ,     RPARM(13)),
     *            (IN2DIS,     RPARM(14)), (XOTNAM,     RPARM(15)),
     *            (XOTCLA,     RPARM(18)), (OTSEQ,      RPARM(20)),
     *            (OTDISK,     RPARM(21)), (BLC(1),     RPARM(22)),
     *            (TRC(1),     RPARM(29)), (IMSIZE(1),  RPARM(36)),
     *            (APARM(1),   RPARM(38))
      EQUIVALENCE (INCATI, INCATR, INCATD)
      EQUIVALENCE (I2(1), R4(1))
      DATA PRGNAM, NPARM /'HGEOM ', 47/
      DATA NR4 /R4DIM/
      DATA XFAC, YFAC /4.0, 4.0/
      DATA TERPS /'Linear  ', 'Cubic   ', 'Quintic', 'Septic  '/
      DATA SUBS /'TSKBEG', 'OPENCF', 'GETHDR', 'MAPCR ', 'MAPIO ',
     *   'MAPWIN', 'GEOHDR', 'MAPFIX', 'SAVHDR', 'WINDOW'/
      DATA LWWSUB /  .TRUE.,         .FALSE.,        .TRUE.,
     *               .TRUE.,         .TRUE.,         .TRUE.,
     *               .FALSE.,        .TRUE.,         .FALSE.,
     *               .FALSE./
      DATA INLUN, IN2LUN, OTLUN, HLUN /17, 18, 19, 27, 28/
      DATA T, MAGIC /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Task init, get parms:
      IRET = 0
      LBLANK = .FALSE.
      ICALL = 1005
      CALL TSKBEG (PRGNAM, NPARM, RPARM, ERROR)
      IF (ERROR.NE.0) GO TO 980
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Convert characters
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6, 1, XINCLA, INCLAS)
      CALL H2CHR (12, 1, XIN2NA, IN2NAM)
      CALL H2CHR (6, 1, XIN2CL, IN2CLA)
      CALL H2CHR (12, 1, XOTNAM, OTNAME)
      CALL H2CHR (6, 1, XOTCLA, OTCLAS)
      NPXO = IMSIZE(1)
      NRWO = IMSIZE(2)
      DO 10 I = 1,10
         IBAD(I) = 0
 10      CONTINUE
      CALL RCOPY (10, APARM, XPARM)
C                                       Defaults and limits:
      XPARM(1) = MIN (MAX (0.0, XPARM(1)), 7.0)
      I = XPARM(1) + 0.1
      IF (I.LE.0) I = 3
      IF (I.GT.7) I = 7
      MORD = I / 2
      ZEROFL = (XPARM(2).GT.0.0)
C                                       Don's secret test flag:
      LRECUR = (XPARM(5).GT.0.0)
      IF (.NOT.LRECUR) GO TO 15
         WRITE (MSGTXT,1010)
         CALL MSGWRT (5)
C                                       Open input image:
 15   CALL CHR2H (4, 'MA  ', 1, MA)
      RUSER = NLUSER
      CALL H2WAWA (XINNAM, XINCLA, INSEQ, MA, INDISK, RUSER, INNAM)
      CALL H2WAWA (XIN2NA, XIN2CL, IN2SEQ, MA, IN2DIS, RUSER, IN2NA)
      ICALL = 2010
      NLUN = 1
      CALL OPENCF (INLUN, INNAM, ERROR)
      IF (ERROR.NE.0) GO TO 980
      ICALL = 3015
      CALL GETHDR (INLUN, INCATI, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL FILNUM (INLUN, OUTPTR, ERROR)
      INVOL = FILTAB(POVOL,OUTPTR)
      INCNO = FILTAB(POCAT,OUTPTR)
      ASEQ = INCATI(KIIMS)
C                                       Fix up BLC and TRC:
 20   ICALL = 10022
      CALL WINDOW (INCATI(KIDIM), INCATI(KINAX), BLC, TRC, ERROR)
      IF (ERROR.NE.0) GO TO 980
      BLC1  = BLC(1)
      IBLC1 = BLC1
      BLC2  = BLC(2)
      IBLC2 = BLC2
      TRC1  = TRC(1)
      ITRC1 = TRC1
      TRC2  = TRC(2)
      ITRC2 = TRC2
C                                       Get effective input size:
      NPXI = (ITRC1 - IBLC1 + 1)
      NRWI = (ITRC2 - IBLC2 + 1)
      NPLANE = 1
      DO 50 I = 3,7
         J = (TRC(I) - BLC(I) + 1.01)
         NPLANE = NPLANE * J
 50      CONTINUE
      WRITE (MSGTXT,1050) NPLANE
      IF (NPLANE.NE.1) CALL MSGWRT (1)
      IF (NPLANE.GE.1) GO TO 60
         ICALL = 0050
         ERROR = 1
         GO TO 980
C                                       Pass window to WaWa:
 60   ICALL = 6018
      NLUN = INLUN
      CALL MAPWIN (INLUN, BLC, TRC, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Set up output header:
      ICALL = 3015
      CALL OPENCF (IN2LUN, IN2NA, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL WAWA2A (IN2NA, IN2NAM, IN2CLA, I2SEQ, PTYPE, I2VOL, USID)
      IN2VOL = I2VOL
      ICALL = 3020
      CALL GETHDR (IN2LUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      BSEQ = CATBLK(KIIMS)
C                                       Default for output size:
      IF ((NPXO.EQ.0) .AND. (NRWO.EQ.0) .AND. (XPARM(3).EQ.0.0)
     *   .AND. (XPARM(4).EQ.0.0)) THEN
         XPARM(3) = CATR(KRCRP)
         XPARM(4) = CATR(KRCRP+1)
         END IF
      IF (NPXO.EQ.0) NPXO = CATBLK(KINAX)
      IF (NRWO.EQ.0) NRWO = CATBLK(KINAX+1)
C
      ICALL = 7025
      NLUN = 0
      CALL GEOHDR (BLC, TRC)
      IF (ERROR.NE.0) GO TO 980
C                                       Create & open output image:
      CALL H2WAWA (XOTNAM, XOTCLA, OTSEQ, MA, OTDISK, RUSER, OTNAM)
      ICALL = 4125
      NLUN = OTLUN
      CALL MAPCR (INNAM, OTNAM, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      ICALL = 2130
      CALL OPENCF (OTLUN, OTNAM, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       copy keywords
      CALL FILKCP (INLUN, OTLUN, ERROR)
C                                       Compute work space pointers:
 70   NCLT = INT((NPXO+2)/XFAC) + 1
      NPXT = 2 * NCLT
      NPXT = NPXT * (INT((NRWO+2)/YFAC)+1)
      NR4 = (NRWI * (NPXI+2) + NPXT + 6*NPXO) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NR4, R4, PR4, ERROR)
      IF (ERROR.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 980
         END IF
      NR4 = 1024 * NR4
      I4TEMP = (NR4 - (NPXO*3) - NPXT) / NPXI
      I4TEMP = (NR4 -(NPXO*3) -NPXT -(I4TEMP+1))/NPXI
      NPZI = MIN (I4TEMP, NRWI)
      NEXT = 1 + PR4
      LPZI = NEXT
      NEXT = NEXT + NPZI + 1
      LXI  = NEXT
      NEXT = NEXT + NPXO
      LYI  = NEXT
      NEXT = NEXT + NPXO
      LZO  = NEXT
      NEXT = NEXT + NPXO
      LPXT = NEXT
      NEXT = NEXT + NPXT
      LZI  = NEXT
      NEXT = LZI + (NPXI * NPZI) - 1
      WRITE (MSGTXT,4001) NEXT-PR4
      CALL MSGWRT (7)
      IF (NEXT-PR4.GT.NR4) THEN
         I = XFAC * 2.0 + 0.01
         XFAC = I
         YFAC = I
         WRITE (MSGTXT,1075) I
         CALL MSGWRT (7)
         CALL ZMEMRY ('FREE', TSKNAM, NR4, R4, PR4, ERROR)
         GO TO 70
         END IF
C                                       Do the work!
      IPXI = NPXI
      CALL GEOSUB (INLUN, OTLUN, IPXI, R4(LZI), I2(LPZI), R4(LZO),
     *   R4(LXI), R4(LYI), NCLT, R4(LPXT), MAGIC, XFAC, YFAC)
      IF (ERROR.NE.0) GO TO 980
C                                       Close the images:
      CALL FILCLS (INLUN)
      CALL FILCLS (IN2LUN)
      ICALL = 3150
      NLUN = OTLUN
      CALL GETHDR (OTLUN, INCATI, ERROR)
      IF (ERROR.NE.0) GO TO 980
      INCATR(KRDMX) = ZMX
      INCATR(KRDMN) = ZMN
      IF (MAGIC) INCATR(KRBLK) = INDEF
      ICALL = 9155
      CALL SAVHDR (OTLUN, INCATI, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL FILNUM (OTLUN, OUTPTR, ERROR)
      OUTVOL = FILTAB(POVOL,OUTPTR)
      OUTCNO = FILTAB(POCAT,OUTPTR)
      CALL FILCLS (OTLUN)
C                                       Create history file and
C                                       copy HI of input image:
 300  CALL HIINIT (3)
      CALL HISCOP (HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO, OUTCNO,
     *   INCATI, WBUFF, IBUF, ERROR)
      IF (ERROR.GE.3) GO TO 320
C                                       Add to history:
      CALL HENCO1 (PRGNAM, INNAME, INCLAS, ASEQ, INVOL, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      CALL HENCO2 (PRGNAM, IN2NAM, IN2CLA, BSEQ, IN2VOL, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      ASEQ = INCATI(KIIMS)
      CALL HENCOO (PRGNAM, OTNAME, OTCLAS, ASEQ, OUTVOL,
     *   HLUN(2), IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      DO 310 I = 1,5
         IDEP(I) = BLC(I+2) + 0.01
         IDEPT(I) = TRC(I+2) + 0.01
 310     CONTINUE
      WRITE (HILINE,1310) PRGNAM, IBLC1, IBLC2, IDEP
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1311) PRGNAM, ITRC1, ITRC2, IDEPT
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1312) PRGNAM, NPXO, NRWO
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1313) PRGNAM, TERPS(MORD+1)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1314) PRGNAM
      IF (ZEROFL) WRITE (HILINE,1315) PRGNAM
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1316) PRGNAM, BLNCNT(1)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1317) BLNCNT(1)
      IF (BLNCNT(1).GT.0) CALL MSGWRT (3)
      WRITE (HILINE,1318) PRGNAM, BLNCNT(2)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1319) BLNCNT(2)
      IF (BLNCNT(2).GT.0) CALL MSGWRT (3)
C                                       Close HI file
 320  CALL HICLOS (HLUN(2), T, IBUF, IERR)
C                                       Report any errors
      IF (ERROR.EQ.0) ERROR = IERR
      IF (ERROR.NE.0) THEN
         WRITE (MSGTXT,1330) ERROR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                        Copy tables
      CALL ALLTAB (1, '  ', HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO,
     *   OUTCNO, INCATI, WBUFF, IBUF, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      GO TO 990
C                                       Print error message
 980  LABEL = MOD (ICALL, 1000)
      ICALL = ICALL / 1000
      WRITE (MSGTXT,1980) SUBS(ICALL), ERROR, LABEL
      CALL MSGWRT (8)
      IF (LWWSUB(ICALL)) CALL PRTERR (ERROR, NLUN)
      IRET = 16
      CALL FILCLS (INLUN)
      CALL FILCLS (OTLUN)
C                                       go home
 990  CALL TSKEND (IRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('The secret recursion (slow) flag is enabled!')
 1050 FORMAT ('Note:', I4, ' planes will be processed.')
 1075 FORMAT ('Coordinate grid coarsened to every',I3,'th pixel')
 1310 FORMAT (A6,'BLC =',2(I5,','),4(I4,','),I4)
 1311 FORMAT (A6,'TRC =',2(I5,','),4(I4,','),I4)
 1312 FORMAT (A6,'IMSIZE =',I5,',',I5,10X,'/ Output image size')
 1313 FORMAT (A6,' / Interpolation order used was Bi',A8)
 1314 FORMAT (A6,' / Indeterminate pixels filled with magic values')
 1315 FORMAT (A6,' / Indeterminate pixels filled with zeros')
 1316 FORMAT (A6,' / ',I10,' Pixels blanked due to geometry')
 1317 FORMAT (I10,' Pixels blanked due to geometry')
 1318 FORMAT (A6,' / ',I10,' Pixels blanked due to input blanked',
     *   ' pixels')
 1319 FORMAT (I10,' Pixels blanked due to input blanked pixels')
 1330 FORMAT ('WARNING: HI FILE HAD PROBLEMS.  ERROR=', I7)
 1980 FORMAT ('SUBROUTINE ', A6, ' RETURNED ERROR', I6, ' AT', I4)
 2000 FORMAT (I2)
 4000 FORMAT ('You are using a non-standard program')
 4001 FORMAT ('Using',I12,' words of buffer')
      END
      SUBROUTINE GEOHDR (BLC,TRC)
C-----------------------------------------------------------------------
C   GEOHDR stores the /LOCATI/ commons of the two input maps and
C   sets certain default values for the output header.
C   Inputs:
C      BLC    R(7)     The bottom left hand corner of the input image
C      TRC    R(7)     The top right hand corner of the input image
C-----------------------------------------------------------------------
      REAL       BLC(7), TRC(7)
      DOUBLE PRECISION LCATD(128), X, Y, Z
      REAL       LCATR(256), CWINX, CWINY, XI, YI
      HOLLERITH  LCATH(256)
      INTEGER    I, II, IDEPTH(5), INLUN, IN2LUN, LCATI(256),
     *   CORD, ITI, ITO, ICX, ICY, COUTX, COUTY, IERR
      LOGICAL    F, CENTER, SWAP
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'HGEOM.INC'
      EQUIVALENCE (LCATI, LCATR, LCATD, LCATH)
      DATA F /.FALSE./
      DATA INLUN, IN2LUN /17, 18/
C-----------------------------------------------------------------------
C                                       Initialize and store /LOCATI/
C                                       second image
      CALL GETHDR (IN2LUN, LCATI, ERROR)
      IF (ERROR.NE.0) GO TO 980

      CALL COPY (256, LCATI, CATBLK)
      DO 10 I = 1, 5
         IDEPTH(I) = 1
 10      CONTINUE
      LOCNUM = 2
      CALL SETLOC (IDEPTH, F)
C                                       input image
      CALL GETHDR (INLUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      DO 15 I = 1, 5
         IDEPTH(I) = BLC(I+2) + 0.1
 15      CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH, F)
      CATR(KRBLK) = 0.0
      CATBLK(KINAX) = NPXO
      CATBLK(KINAX+1) = NRWO
      CENTER = (XPARM(3).EQ.0.0) .AND. (XPARM(4).EQ.0.0)
      DO 20 I = 1,2
         II = I - 1
         CATD(KDCRV+II) = LCATD(KDCRV+II)
         CATR(KRCRP+II) = XPARM(2+I)
         CATR(KRCIC+II) = LCATR(KRCIC+II)
         CATR(KRCRT+II) = LCATR(KRCRT+II)
         II = II * 2 + KHCTP
         CALL CHCOPY (8, 1, LCATH(II), 1, CATH(II))
 20      CONTINUE
      DO 30 I = 3,7
         II = I - 1
         CATBLK(KINAX+II) = TRC(I) - BLC(I) + 1.01
         CATR(KRCRP+II) = CATR(KRCRP+II) - BLC(I) + 1.0
 30      CONTINUE
C                                       If output image is smaller than
C                                       second input then shift
C                                       reference pixel to center the
C                                       output on the input subimage
      IF (CENTER) THEN
C                                       Center of input subimage
         ICX = (TRC(1) - BLC(1) + 1) / 2.0 - 0.9 + BLC(1)
         ICY = (TRC(2) - BLC(2) + 1) / 2.0 + 0.1 + BLC(2)
         CWINX = ICX
         CWINY = ICY
C                                       Find corresponding pixel in
C                                       second input image - verrryyy
C                                       painful
         LOCNUM = 1
         CORD = CORTYP(LOCNUM)
         ITI = 1
         IF (LABTYP(LOCNUM).LT.44) ITI = 2
         IF (LABTYP(LOCNUM).LT.22) ITI = 3
         LOCNUM = 2
         ITO = 1
         IF (LABTYP(LOCNUM).LT.44) ITO = 2
         IF (LABTYP(LOCNUM).LT.22) ITO = 3
         SWAP = (CORD.NE.CORTYP(LOCNUM))
         LOCNUM = 1
         CALL XYVAL (CWINX, CWINY, X, Y, Z, IERR)
         IF (IERR.NE.0) GO TO 990
         LOCNUM = 2
         IF (CORD.EQ.1) CALL COORDT (ITI, ITO, X, Y, CATR(KREPO),
     *      X, Y, IERR)
         IF (CORD.EQ.2) CALL COORDT (ITI, ITO, Y, X, CATR(KREPO),
     *      Y, X, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (SWAP) THEN
            CALL XYPIX (Y, X, YI, XI, IERR)
         ELSE
            CALL XYPIX (X, Y, XI, YI, IERR)
            END IF
         IF (IERR.NE.0) GO TO 990
C                                       Center of output
         COUTX = NPXO * 0.5 + 0.1
         COUTY = NRWO * 0.5 + 1.1
C                                       Shift reference pixel.
         CATR(KRCRP) = LCATR(KRCRP) + COUTX - XI
         CATR(KRCRP+1) = LCATR(KRCRP+1) + COUTY - YI
         END IF
C                                       Use output for geometry
      LOCNUM = 2
      CALL SETLOC (IDEPTH, F)
      GO TO 999
C                                       Print error message:
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
 990  MSGTXT = 'I WANT TO DIE BUT I CANNOT'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ERROR GETTING HEADER IN SUBROUTINE GEOHDR')
      END
      SUBROUTINE GEOSUB (INLUN, OTLUN, NPXID, ZI, PZI, ZO, XI, YI, NCL,
     *   PXT, MAGIC, XFAC, YFAC)
C-----------------------------------------------------------------------
C   GEOSUB uses the 'scrolling buffer' concept of how to do geometric
C   transformations. The main program gives it a working array,
C   ZI(NPXI,NPZI), to use.  As a transformation proceeds GEOSUB must
C   read new rows from the input image.  It also needs to forget old
C   rows which are no longer needed.  The solution adopted is to
C   maintain a table of subscripts in array PZI(NPZI) which points at
C   the rows in ZI(,).
C      GEOSUB moves the scroll along by just moving the pointer
C      values along in PZI, rather than moving the rows in memory. The
C      newest row reads in on top of the oldest row.
C   The value of NPZI is computed in the main program from the
C      dimension of ZI(,) and the length of rows in the input image.
C      For example, if ZI has
C      20000 cells and the rows are 512 pixels we can hold about 39
C      rows. The number of rows needed for a given transformation
C      is the sine of the angle times the row length. For example,
C      for 512 pixels and rotation of 4.0 degrees we need 35 rows.
C      If we try to rotate the 512 pixel image by 5.0 degrees we will
C      not have enough capacity in ZI. So, GEOSUB is useful for
C      small-rotation-angle problems. Smaller images can be rotated
C      through larger angles. Any problem which needs more than NPZI
C      rows will produce blanks in the output image where the missing
C      rows should have supplied values. A warning message will be
C      produced in such cases.
C   In the neighborhood of any edge of the data matrix in memory
C      GEOSUB 'reflects' the boundary. This causes first derivatives
C      to be zero at all boundaries.
C-----------------------------------------------------------------------
      INTEGER   NPXID
      INTEGER   INLUN, OTLUN, PZI(1), NCL
      REAL      ZI(NPXID,1), ZO(1), XI(1), YI(1), PXT(2,NCL,1)
      DOUBLE PRECISION  X, Y, Z
      REAL      DX, DY, SVX(14), SVY(14), XSUM,
     *   YSUM, ZIKP, GIANT, XFAC, YFAC
      INTEGER   I4T
      INTEGER    KO, JO, I, J, JFIRST, JLAST, JMIN, JMAX, IXI,
     *   IYI, MORD1, MORD21, MORD12, JPZI, JMN, JMX, JP, KP, K, PXMAX,
     *   RWMAX, CORD, IERR, ITI, ITO, IPL
      LOGICAL    LIVE, SWAP, MAGIC
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'HGEOM.INC'
      DATA GIANT /1.0E+30/
C-----------------------------------------------------------------------
      BLNCNT(1) = 0
      BLNCNT(2) = 0
      ERROR = 0
      MORD1 = MORD + 1
      MORD21 = MORD + MORD1
      MORD12 = MORD1 + MORD1
      CALL ISETWT (MORD)
C                                       Test if axes are manageable:
      LOCNUM = 1
      CORD = CORTYP(LOCNUM)
      ITI = 1
      IF (LABTYP(LOCNUM).LT.44) ITI = 2
      IF (LABTYP(LOCNUM).LT.22) ITI = 3
      LOCNUM = 2
      ITO = 1
      IF (LABTYP(LOCNUM).LT.44) ITO = 2
      IF (LABTYP(LOCNUM).LT.22) ITO = 3
      IF (CORD.GT.2) GO TO 950
      IF (CORTYP(LOCNUM).GT.2) GO TO 950
      IF (CORD.EQ.0) GO TO 960
      IF (CORTYP(LOCNUM).EQ.0) GO TO 960
      SWAP = (CORD.NE.CORTYP(LOCNUM))
C                                       Giant number for max/min loop:
      ZMN = GIANT
      ZMX = -ZMN
C                                       Compute table:
      IPL = 1024000. / NPXO + 0.5
      PXMAX = (NPXO+2)/XFAC + 1
      RWMAX = (NRWO+2)/YFAC + 1
      WRITE (MSGTXT,1000)
      CALL MSGWRT (1)
      DO 5 J = 1,RWMAX
         I = (J - 1) * YFAC + 1
         WRITE (MSGTXT,1001) I
         IF (MOD(J,IPL).EQ.0) CALL MSGWRT (1)
         DO 4 I = 1,PXMAX
            PXT(1,I,J) = -100000.
            PXT(2,I,J) = -100000.
            DX = XFAC * (I-1.0) + 1.0
            DY = YFAC * (J-1.0) + 1.0
            LOCNUM = 2
            CALL XYVAL (DX, DY, X, Y, Z, IERR)
            IF (IERR.NE.0) GO TO 4
            LOCNUM = 1
            IF (CORD.EQ.1) CALL COORDT (ITO, ITI, X, Y, CATR(KREPO),
     *         X, Y, IERR)
            IF (CORD.EQ.2) CALL COORDT (ITO, ITI, Y, X, CATR(KREPO),
     *         Y, X, IERR)
            IF (SWAP) CALL XYPIX (Y, X, PXT(1,I,J), PXT(2,I,J), IERR)
            IF (.NOT.SWAP) CALL XYPIX (X, Y, PXT(1,I,J), PXT(2,I,J),
     *         IERR)
            IF (IERR.NE.0) THEN
               PXT(1,I,J) = -100000.
               PXT(2,I,J) = -100000.
               END IF
 4          CONTINUE
 5       CONTINUE
      WRITE (MSGTXT,1005)
      CALL MSGWRT (1)
C                                       Loop over planes:
      DO 210 KO = 1,NPLANE
C                                       Main loop over output rows:
      DO 200 JO = 1,NRWO
C                                       Print row number every tenth
C                                       row:
      IF (MOD(JO,IPL).EQ.0) THEN
         WRITE (MSGTXT,1006) JO
         CALL MSGWRT (1)
         END IF
C                                       Compute warped coordinates:
      DY = JO
      DO 7 I = 1,NPXO
         DX = I
         CALL TABLE (PXMAX, PXT(1,1,1), DX, DY, SWAP, ITI, ITO, CORD,
     *      XI(I), YI(I), XFAC, YFAC)
C                                       round to interp accuracy
         I4T = 60.0 * XI(I) + 0.5
         XI(I) = I4T / 60.0
         I4T = 60.0 * YI(I) + 0.5
         YI(I) = I4T / 60.0
 7       CONTINUE
C                                       Get min/max of y coordinates:
      JMN = ITRC2
      JMX = IBLC2
      LIVE = .FALSE.
      DO 20 I = 1,NPXO
         IF ((XI(I).LT.BLC1) .OR. (XI(I).GT.TRC1)) GO TO 10
         IF ((YI(I).LT.BLC2) .OR. (YI(I).GT.TRC2)) GO TO 10
            J = YI(I)
            JMN = MIN (JMN, J)
            JMX = MAX (JMX, J)
            LIVE = .TRUE.
 10      CONTINUE
 20      CONTINUE
      IF (.NOT.LIVE) GO TO 22
         JMIN = MAX (IBLC2, MIN ((JMN - MORD), (ITRC2 - MORD21)) )
         JMAX = MIN (MAX ((JMIN + MORD21), (JMX + MORD1)), ITRC2)
         GO TO 24
 22   CONTINUE
         IF (JO.GT.1) GO TO 23
            JMIN = IBLC2
            JMAX = JMIN
 23      CONTINUE
 24   CONTINUE
C                                       Initialize scroll (first row):
      IF (JO.LE.1) THEN
         IF (KO.GE.2) THEN
            WRITE (MSGTXT,1024) KO
            CALL MSGWRT (1)
C                                       Finish reading previous plane:
            IF (JLAST.LT.ITRC2) THEN
               DO 25 I = JLAST+1,ITRC2
                  ICALL = 5025
                  NLUN = INLUN
                  CALL MAPIO ('READ', INLUN, ZI(1,1), ERROR)
                  IF (ERROR.NE.0) GO TO 980
 25               CONTINUE
               END IF
            END IF
C                                       Read first part of new plane:
         JFIRST = IBLC2
         JLAST  = JFIRST - 1
         DO 27 I = 1,NPZI
            PZI(I) = I
            JPZI = PZI(I)
            ICALL = 5026
            NLUN = INLUN
            CALL MAPIO ('READ', INLUN, ZI(1,JPZI), ERROR)
            IF (ERROR.NE.0) GO TO 980
            JLAST = JLAST + 1
 27         CONTINUE
         END IF
C                                       Logic to advance the scroll:
 60   CONTINUE
      IF ((JMIN.GE.JFIRST) .AND. (JMAX.LE.JLAST)) GO TO 100
      IF (JMIN.LE.JFIRST) GO TO 100
C                                       Delete oldest row (JFIRST):
         JPZI = PZI(1)
         DO 80 J = 2, NPZI
            PZI(J-1) = PZI(J)
 80         CONTINUE
         PZI(NPZI) = JPZI
         JFIRST = JFIRST + 1
C                                       Read new row (JLAST):
         ICALL = 5085
         NLUN = INLUN
         CALL MAPIO ('READ', INLUN, ZI(1,JPZI), ERROR)
         IF (ERROR.NE.0) GO TO 980
         JLAST = JLAST + 1
         GO TO 60
C                                       Check for memory overflow:
 100  CONTINUE
      IF (((JFIRST.LE.JMIN) .AND. (JMAX.LE.JLAST)) .OR.LBLANK) GO TO 105
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         LBLANK = .TRUE.
 105  CONTINUE
C                                       Now to compute the output row:
      DO 160 I = 1,NPXO
         IF ((XI(I).LT.BLC1) .OR. (XI(I).GT.TRC1) .OR.
     *      (YI(I).LT.BLC2) .OR. (YI(I).GT.TRC2)) GO TO 140
            YSUM = 0.0
C                                       Compute Everett coefficients:
            CALL IGETWT (SVX, XI(I), IXI)
            CALL IGETWT (SVY, YI(I), IYI)
C                                       Loop on y-interpolation:
            DO 130 J = 1,MORD12
               JP = J + IYI
               IF ((JP.GE.JFIRST) .AND. (JP.LE.JLAST)) GO TO 110
                  IF ((JP.LT.JFIRST).AND.(JFIRST.GT.IBLC2))    GO TO 140
                  IF ((JP.GT.JLAST) .AND. (JLAST.LT.ITRC2))    GO TO 140
                  IF ((JP.LT.(IBLC2-MORD1)).OR.(JP.GT.(ITRC2+MORD1)))
     *                                                         GO TO 140
                  IF (JP.LT.IBLC2) JP = IBLC2 + IBLC2 - JP - 1
                  IF (JP.GT.ITRC2) JP = ITRC2 + ITRC2 - JP + 1
 110           CONTINUE
               JPZI = PZI(JP - JFIRST + 1)
               XSUM = 0.0
C                                       Do an x-interpolation:
               DO 120 K = 1,MORD12
                  KP = K + IXI
                  IF ((KP.GE.IBLC1).AND.(KP.LE.ITRC1)) GO TO 115
                     IF ((KP.LT.IBLC1-MORD1) .OR. (KP.GT.ITRC1+MORD1))
     *                  GO TO 140
                     IF (KP.LT.IBLC1) KP = IBLC1 + IBLC1 - KP - 1
                     IF (KP.GT.ITRC1) KP = ITRC1 + ITRC1 - KP + 1
 115              CONTINUE
                  ZIKP = ZI((KP-IBLC1+1),JPZI)
                  IF (ZIKP.EQ.INDEF) GO TO 140
                  XSUM = XSUM + (ZIKP * SVX(K))
 120              CONTINUE
               YSUM = YSUM + (XSUM * SVY(J))
 130           CONTINUE
            ZO(I) = YSUM
            ZMX = MAX (ZMX, YSUM)
            ZMN = MIN (ZMN, YSUM)
            GO TO 150
 140     CONTINUE
            IF (ZIKP.NE.INDEF) BLNCNT(1) = BLNCNT(1) + 1
            IF (ZIKP.EQ.INDEF) BLNCNT(2) = BLNCNT(2) + 1
            IF (ZEROFL) GO TO 145
               ZO(I) = INDEF
               MAGIC = .TRUE.
               GO TO 150
 145        CONTINUE
               ZO(I) = 0.0
               ZMX = MAX (ZMX, 0.0)
               ZMN = MIN (ZMN, 0.0)
 150     CONTINUE
 160     CONTINUE
C
      ICALL = 5165
      NLUN = OTLUN
      CALL MAPIO ('WRIT', OTLUN, ZO, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Bottom of the main loop:
 200  CONTINUE
C                                       Bottom of plane loop:
 210  CONTINUE
C                                       If output all blanks:
      IF (ZMN.NE.GIANT) GO TO 999
         ZMX = 1.0
         ZMN = -ZMX
         WRITE (MSGTXT,1210)
         ERROR = 16
         GO TO 990
C                                       header errors
 950  WRITE (MSGTXT,1950)
      GO TO 990
 960  WRITE (MSGTXT,1960)
      GO TO 990
 970  WRITE (MSGTXT,1970)
      GO TO 990
C                                       I/O errors print here:
 980  WRITE (MSGTXT,1980) ICALL, NLUN, ERROR
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Begin computing coordinate tables')
 1001 FORMAT ('Currently on table row number',I6)
 1005 FORMAT ('Begin computing output image')
 1006 FORMAT ('Currently on output row number',I6)
 1024 FORMAT ('Begin plane', I4)
 1100 FORMAT ('Memory limit. some pixels probably blanked.')
 1210 FORMAT ('*****  ALL OUTPUT PIXELS BLANKED  *****')
 1950 FORMAT ('CANNOT HANDLE CURRENT TRANSPOSITION: USE TRANS')
 1960 FORMAT ('ONLY HANDLES IMAGES WITH POSITION AXES')
 1970 FORMAT ('NOT SMART ENOUGH TO CONVERT BETWEEN COORDINATE TYPES')
 1980 FORMAT ('ERROR EXIT AT 980 IN GEOSUB:', 3I6)
      END
      SUBROUTINE TABLE (NCLTD, TABL, XO, YO, SWAP, ITI, ITO, CORD,
     *   XI, YI, XFAC, YFAC)
C-----------------------------------------------------------------------
C     This subroutine determines the input pixel which corresponds to
C     the specified output pixel.  By default, it computes the pixel
C     by interpolating data in a table.  It can also do the computation
C     explicitly.
C-----------------------------------------------------------------------
      INTEGER   NCLTD, IERR, ITI, ITO, CORD
      REAL      TABL(2,NCLTD,1), XO, YO, XI, YI
      LOGICAL   SWAP
      DOUBLE PRECISION  X, Y, Z
      REAL      XTAB, YTAB, LEFWT, DOWNWT, XDOWN, XUP, YLEFT, YRIGHT,
     *   XFAC, YFAC
      INTEGER   LEFT, RIGHT, UP, DOWN
      INCLUDE 'HGEOM2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Fast table lookup:
      IF (.NOT.LRECUR) THEN
         XTAB  = (XO-1.0)/XFAC + 1.0
         YTAB  = (YO-1.0)/YFAC + 1.0
         LEFT  = INT((XO - 0.5)/XFAC) + 1
         RIGHT = INT((XO - 0.5)/XFAC) + 2
         DOWN  = INT((YO - 0.5)/YFAC) + 1
         UP    = INT((YO - 0.5)/YFAC) + 2
         LEFWT = RIGHT - XTAB
         DOWNWT= UP - YTAB
         XDOWN = LEFWT*TABL(1,LEFT,DOWN) +
     *           (1-LEFWT)*TABL(1,RIGHT,DOWN)
         XUP   = LEFWT*TABL(1,LEFT,UP) +
     *           (1-LEFWT)*TABL(1,RIGHT,UP)
         XI    = DOWNWT*XDOWN + (1-DOWNWT)*XUP
         YLEFT = DOWNWT*TABL(2,LEFT,DOWN) +
     *           (1-DOWNWT)*TABL(2,LEFT,UP)
         YRIGHT= DOWNWT*TABL(2,RIGHT,DOWN) +
     *           (1-DOWNWT)*TABL(2,RIGHT,UP)
         YI    = LEFWT*YLEFT + (1-LEFWT)*YRIGHT
C                                       Slow recomputation:
      ELSE
         LOCNUM = 2
         CALL XYVAL (XO, YO, X, Y, Z, IERR)
         LOCNUM = 1
         IF (IERR.EQ.0) THEN
            IF (CORD.EQ.1) CALL COORDT (ITO, ITI, X, Y, CATR(KREPO),
     *         X, Y, IERR)
            IF (CORD.EQ.2) CALL COORDT (ITO, ITI, Y, X, CATR(KREPO),
     *         Y, X, IERR)
            IF (.NOT.SWAP) CALL XYPIX (X, Y, XI, YI, IERR)
            IF (SWAP) CALL XYPIX (Y, X, XI, YI, IERR)
            END IF
         IF (IERR.NE.0) THEN
            XI = -100000.
            YI = -100000.
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE ISETWT (MORD)
C-----------------------------------------------------------------------
C     This routine substitutes for SETCOF in the interpolation package.
C     It's purpose is to compute a table of interpolation weights which
C     can be used without recomputation. The table has a subdivision
C     of 60 parts of the pixel interval. This provides EXACT weights for
C     scale factors of 2, 3, 4, 5, 6, and 10 (60=3*4*5), commonly used
C     integer blowup factors. Cell 61 is identical to cell 1 for ease of
C     programming. If experimental trials were to demonstrate that this
C     is not a fine enough subdivision, then 120 parts would be a good
C     choice to try.
C     Inputs: MORD = 0,1,2,3 = "order" of interpolation
C        0=bilinear, 1=bicubic, 2=biquintic, 3=biseptic
C-----------------------------------------------------------------------
      INTEGER     I, MORD, J
      REAL        DXI
      INCLUDE 'HGEOM2.INC'
C-----------------------------------------------------------------------
      CALL SETCOF (MORD)
C                                       Initialize weight array:
      DO 10 I = 1, 61
         DXI = MIN ((I - 1.0) / 60.0 + 1.0, 1.99999)
         CALL IEVERT (SAVWTS(1,I), DXI, J)
 10      CONTINUE
      RETURN
      END
      SUBROUTINE IGETWT (SVECT, DX, IWT)
C-----------------------------------------------------------------------
C     Fetch weights from the table computed by ISETWT.
C     The point of this scheme is to avoid the recursive computation of
C     the weights.
C-----------------------------------------------------------------------
      INTEGER     IWT
      REAL        SVECT(1), DX
      INTEGER     I, J
      INCLUDE 'HGEOM2.INC'
C-----------------------------------------------------------------------
C                                       Fast table lookup:
         IWT = DX
         J = (DX - IWT) * 60.0 + 1.5
         IWT = IWT - IS0
         DO 10 I = 1, NVALS
            SVECT(I) = SAVWTS(I,J)
 10         CONTINUE
C
 999  RETURN
      END
      SUBROUTINE IEVERT (SVECT, DX, L)
C-----------------------------------------------------------------------
C   Compute subscript offset and weights for interpolating at a
C      specified position in a vector. The position enters as floating
C      point argument DX. The offset returns as the integer value
C      function result of IEVERT. The weights return in SVECT().
C      Interpolation is done using a group of pixels centered on the
C      specified position. The order of interpolation is specified by
C      integer MORD in subroutine SETCOF. We do linear interpolation
C      for MORD=0, cubic for 1, and quintic for 2. If MORD=1 (i.e.,
C      cubic interpolation) we will be using four pixels in the
C      interpolation.
C      From the Everett interpolation package originally coded by
C      Larry Goad at KPNO.
C-----------------------------------------------------------------------
      INTEGER   L
      REAL      SVECT(1), DX
      INTEGER   N, NR, IR0, IV0, MC, IR
      REAL      U, W,  CW,  CU,  W2, U2
      INCLUDE 'HGEOM2.INC'
C-----------------------------------------------------------------------
      L = DX
      U = DX - L
      L = L - IS0
C
      DO 5 N = 1,NVALS
         SVECT(N) = 0.0
 5       CONTINUE
C
      W            = 1. - U
      CW           = W
      SVECT(IS0)   = W
      CU           = U
      SVECT(IS0+1) = U
      W2           = W * W
      U2           = U * U
      IF ((U.EQ.0) .OR. (MORD2.EQ.0)) GO TO 999
C
      NR  = 1
      IR0 = 0
      IV0 = IS0 - 1
      DO 20 MC = 1,MORD2
C                                       (IR0=MC*MC):
         IR0 = IR0 + NR
C                                       (NR=2*MC+1):
         NR  = NR  + 2
C                                       (IV0=IS0-MC-1):
         IV0 = IV0 - 1
         CU  = CU * (U2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         CW  = CW * (W2 - IR0) / ((IR0 + IR0 + MC) + (IR0 + IR0 + MC))
         DO 10 IR = 1,NR
            SVECT(IV0+IR)   = SVECT(IV0+IR)   + CW * BCOEF(IR0+IR)
            SVECT(IV0+IR+1) = SVECT(IV0+IR+1) + CU * BCOEF(IR0+IR)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETCOF (IORD)
C-----------------------------------------------------------------------
C   SETCOF computes certain quantities which are needed by IEVERT
C      when it computes the actual weights for an interpolation. The
C      result produced by SETCOF is in the BCOEF array in the COMMON
C      block, and is based on binomial coefficients computed by BINOM.
C      From the Everett interpolation package originally coded by
C      Larry Goad at KPNO.
C-----------------------------------------------------------------------
      INTEGER   IORD
      INTEGER   IN, NT, M, N
      REAL      XT
      INCLUDE 'HGEOM2.INC'
C-----------------------------------------------------------------------
      MORD2 = MIN (IORD, 6)
      IN = 1
      NT = 0
      BCOEF(1) = 1.
C                                   Compute the BCOEF array:
      DO 50 M = 1,MORD2
         IN = IN + NT + 1
         NT = M + M
         XT = NT
         CALL BINOM (XT, NT, BCOEF(IN))
C
         DO 40 N = 1,NT,2
            BCOEF(IN+N) = -BCOEF(IN+N)
 40         CONTINUE
 50      CONTINUE
C                                    Set up pointer constants:
      IS0   = MORD2 + 1
      NVALS = IS0  + IS0
C
 999  RETURN
      END
      SUBROUTINE BINOM (X, M, VAL)
C-----------------------------------------------------------------------
C   BINOM generates binomial coefficients for use in the Everett
C      interpolation routines. It is called only by SETCOF.
C      From the Everett interpolation package originally coded by
C      Larry Goad at KPNO.
C-----------------------------------------------------------------------
      INTEGER    M
      REAL       X, VAL(1)
      INTEGER    I
      REAL       R, XL
C-----------------------------------------------------------------------
      VAL(1) = 1.
      R = 0.
      XL = X + 1.
C
      DO 5 I = 1,M
         XL = XL - 1.
         R = R + 1.
         VAL(I+1) = VAL(I) * XL / R
 5       CONTINUE
C
      RETURN
      END
