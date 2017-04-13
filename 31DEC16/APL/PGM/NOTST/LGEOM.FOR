LOCAL INCLUDE 'LGEOM.INC'
C                                       local variables for LGEOM
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
C                                                          End LGEOM
LOCAL END
LOCAL INCLUDE 'LGEOM2.INC'
C                             Everett Interpolation internal variables:
      REAL      BCOEF(49), SV(14), SAVWTS(8,61)
      INTEGER   MORD2, IS0, NVALS
      LOGICAL   LRECUR
      COMMON /CEVI/ BCOEF, SV, SAVWTS, LRECUR, MORD2, IS0, NVALS
C                                                          End LGEOM2
LOCAL END
      PROGRAM LGEOM
C-----------------------------------------------------------------------
C! Geometric transformation of image (large memory version)
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002, 2009-2010, 2012
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
C   AIPS task to compute a linear geometric transformation (translate,
C   scale, & rotate) of an input image.
C   NOTE: the working array is dimensioned to be 700000 in this version
C   of LGEOM, enough to fit an 836-square image in memory.  The number
C   700000 appears in two places in the source:
C          declaration of R4(), and
C          DATA statement for variable NR4.
C   The memory required to link and execute LGEOM can be changed
C   easily by altering these two numbers.
C   Inputs (adverbs from AIPS):
C      INNAME(3)     Input image name
C      INCLASS(2)    Input image class
C      INSEQ         First input image seq no.
C      INDISK        Input image disk: 0 = any
C      OUTNAME(3)    Output image name default INNAME
C      OUTCLASS(2)   Output image class default 'GEOM  '
C      OUTSEQ        Output image seq no. 0 => lowest unique
C      OUTDISK       Output image disk
C      BLC(7)        the coordinate in the source file to
C                    become the bottom left hand coordinate (1,1)
C                    of the subimage.  BLC(1) is the X value and
C                    BLC(2) is the Y value.  The first coordinate
C                    IN the source map is (1,1). Any BLC(I) equal
C                    to zero defaults to 1.
C      TRC(7)        the coordinate in the source file to
C                    become the top right hand corner of the
C                    subimage.  The conventions used for BLC hold.
C      IMSIZE(2)     Output image size [1=columns, 2=rows]
C      APARM(10)     transformation parameters:
C                       (1)=x-shift,    (2)=y-shift (pixels)
C                       (3)=rotate(deg),(4)=scale
C                       (5)=diff.scale, (6)=interp.order
C                       (7)=invert.flag,(8)=axes-only.flag,
C                       (9)=zero.flag
C      BADDISK(10)   Disks to avoid for the scratch file.
C   Wishlist notes:
C   1. test of arbitrary slice capability.
C   2. check Butcher's wish list.
C   3. what to do about CC and other extension files?
C   4. what type is DUM in call to FILDES? (see Vol. III)
C   5. need integer output reserved at start for OUTDISK
C-----------------------------------------------------------------------
      CHARACTER OTNAM*36, PRGNAM*6, INNAM*36, HILINE*72,
     *   TERPS(4)*8, SUBS(9)*6
      INTEGER   IPXI, NPARM
      REAL      RPARM(41), USERID, INSEQ, INDISK, OTSEQ, OTDISK, BLC(7),
     *   TRC(7), IMSIZE(2), APARM(10)
      HOLLERITH XINNAM(3), XINCLA(2), XOTNAM(3), XOTCLA(2), XMA
      CHARACTER INNAME*12, INCLAS*6, OTNAME*12, OTCLAS*6
C                                       Work space and pointers:
C                                       NX = dynamic memory
      INTEGER   R4DIM
      PARAMETER (R4DIM=2)
      REAL      R4(R4DIM)
      INTEGER   I2(R4DIM), NR4
      LONGINT   NEXT, LXI, LYI, LZO, LZI, LPZI, PR4
C                                       Miscellaneous variables:
      REAL      TEMP, RTEMP
      INTEGER   INVOL, INCNO, OUTVOL, OUTCNO, OUTPTR, INLUN, OTLUN,
     *   HLUN(2), LABEL, IRET, IDEP(5), IDEPT(5), I, J, IERR, ASEQ
      LOGICAL   T, LWWSUB(10), MAGIC
      INCLUDE 'LGEOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'LGEOM2.INC'
      EQUIVALENCE (USERID,    RPARM( 1)), (XINNAM,    RPARM( 2)),
     *            (XINCLA,    RPARM( 5)), (INSEQ,     RPARM( 7)),
     *            (INDISK,    RPARM( 8)), (XOTNAM,    RPARM( 9)),
     *            (XOTCLA,    RPARM(12)), (OTSEQ,     RPARM(14)),
     *            (OTDISK,    RPARM(15)), (BLC(1),    RPARM(16)),
     *            (TRC(1),    RPARM(23)), (IMSIZE(1), RPARM(30)),
     *            (APARM(1),  RPARM(32))
      EQUIVALENCE (I2(1), R4(1))
      DATA NPARM /40/
      DATA PRGNAM /'LGEOM '/
      DATA NR4 /R4DIM/
      DATA TERPS /'Linear  ', 'Cubic   ', 'Quintic', 'Septic  '/
      DATA SUBS /'TSKBE2', 'OPENCF', 'GETHDR', 'MAPCR ', 'MAPIO ',
     *   'MAPWIN', 'GEOHDR', 'SAVHDR', 'WINDOW'/
      DATA LWWSUB /  .TRUE.,         .FALSE.,        .TRUE.,
     *               .TRUE.,         .TRUE.,         .TRUE.,
     *               .FALSE.,        .TRUE.,         .FALSE.,
     *               .FALSE./
      DATA INLUN, OTLUN, HLUN /17, 18, 27, 28/
      DATA T, MAGIC /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Task init, get parms:
      IRET = 0
      LBLANK = .FALSE.
      ICALL = 1005
      CALL TSKBEG (PRGNAM, NPARM, RPARM(2), ERROR)
      IF (ERROR.NE.0) GO TO 980
      RPARM(1) = NLUSER
      CALL H2CHR (12, 1, XINNAM, INNAME)
      CALL H2CHR (6, 1, XINCLA, INCLAS)
      CALL H2CHR (12, 1, XOTNAM, OTNAME)
      CALL H2CHR (6, 1, XOTCLA, OTCLAS)
      NPXO = IMSIZE(1) + 0.1
      NRWO = IMSIZE(2) + 0.1
      DO 10 I = 1,10
         IBAD(I) = 0
 10      CONTINUE
      CALL RCOPY (10, APARM, XPARM)
C                                       Defaults and limits:
      IF (XPARM(4).EQ.0.0) XPARM(4) = 1.0
      IF (XPARM(5).EQ.0.0) XPARM(5) = 1.0
      XPARM(6) = MIN (MAX (0.0, XPARM(6)), 7.0)
      I = XPARM(6) + 0.1
      IF (I.LE.0) I = 3
      IF (I.GT.7) I = 7
      MORD = I / 2
      INVERT = (XPARM(7).GT.0.0)
      AXONLY = (XPARM(8).GT.0.0)
      ZEROFL = (XPARM(9).GT.0.0)
C                                       Don's secret test flag:
      LRECUR = (XPARM(10).GT.0.0)
      IF (.NOT.LRECUR) GO TO 15
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
C                                       Open input image:
 15   CALL CHR2H (4, 'MA  ', 1, XMA)
      CALL H2WAWA (XINNAM, XINCLA, INSEQ, XMA, INDISK, USERID, INNAM)
      ICALL = 2010
      NLUN = 1
      CALL OPENCF (INLUN, INNAM, ERROR)
      IF (ERROR.NE.0) GO TO 980
      ICALL = 3015
      CALL GETHDR (INLUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL FILNUM (INLUN, OUTPTR, ERROR)
      INVOL = FILTAB(POVOL,OUTPTR)
      INCNO = FILTAB(POCAT,OUTPTR)
      ASEQ = CATBLK(KIIMS)
      IF (ABS(XPARM(3)).LE.2.0) GO TO 20
         TEMP = (ABS(CATR(KRCIC)) + ABS(CATR(KRCIC+1))) / 2.0
         RTEMP = ABS(CATR(KRCIC)) - ABS(CATR(KRCIC+1))
         IF (ABS(RTEMP).GE.0.1*TEMP) GO TO 19
         IF ((XPARM(5).GT.0.9) .AND. (XPARM(5).LE.1.1)) GO TO 20
 19      CONTINUE
            WRITE (MSGTXT,1019)
            GO TO 985
C                                       Fix up BLC and TRC:
 20   ICALL = 10022
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, ERROR)
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
C                                       Default for IMSIZE:
 60   IF (NPXO.EQ.0) NPXO = NPXI
      IF (NRWO.EQ.0) NRWO = NRWI
C                                       Pass window to WaWa:
      ICALL = 6018
      NLUN = INLUN
      CALL MAPWIN (INLUN, BLC, TRC, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Set up output header:
      ICALL = 3020
      CALL GETHDR (INLUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      ICALL = 7025
      NLUN = 0
      CALL GEOHDR (BLC, TRC)
      IF (ERROR.NE.0) GO TO 980
C                                       Create & open output image:
      CALL CHR2H (4, 'MA  ', 1, XMA)
      CALL H2WAWA (XOTNAM, XOTCLA, OTSEQ, XMA, OTDISK, USERID, OTNAM)
 120  ICALL = 4125
      NLUN = OTLUN
      CALL MAPCR (INNAM, OTNAM, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      ICALL = 2130
      CALL OPENCF (OTLUN, OTNAM, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       dynamic memory
      NR4 = (NPXI * (NPXI * NRWI + 3.0 * NPXO + 1.0)) / (NPXI - 1.0)
      NR4 = (NR4 + 3 * NPXO) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NR4, R4, PR4, ERROR)
      IF (ERROR.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY'
         CALL MSGWRT (8)
         GO TO 980
         END IF
      NR4 = 1024 * NR4
C                                       Compute work space pointers:
      NPZI = (NR4 - (NPXO * 3)) / NPXI
      NPZI = (NR4 - (NPXO * 3) - (NPZI+1)) / NPXI
      NPZI = MIN (NPZI, NRWI)
      NEXT = 1 + PR4
      LPZI = NEXT
      NEXT = NEXT + NPZI + 1
      LXI  = NEXT
      NEXT = NEXT + NPXO
      LYI  = NEXT
      NEXT = NEXT + NPXO
      LZO  = NEXT
      NEXT = NEXT + NPXO
      LZI  = NEXT
      NEXT = NEXT + (NPXI * NPZI)
      WRITE (MSGTXT,4001) NEXT-PR4
      CALL MSGWRT (7)
C                                       Do the work!
      IPXI = NPXI
      CALL GEOSUB (INLUN, OTLUN, IPXI, R4(LZI), I2(LPZI), R4(LZO),
     *   R4(LXI), R4(LYI), MAGIC)
      IF (ERROR.NE.0) GO TO 980
C                                       Close the images:
      CALL FILCLS (INLUN)
      ICALL = 3150
      NLUN = OTLUN
      CALL GETHDR (OTLUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CATR(KRDMX) = ZMX
      CATR(KRDMN) = ZMN
      IF (MAGIC) CATR(KRBLK) = INDEF
      ICALL = 9155
      CALL SAVHDR (OTLUN, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      CALL FILNUM (OTLUN, OUTPTR, ERROR)
      OUTVOL = FILTAB(POVOL,OUTPTR)
      OUTCNO = FILTAB(POCAT,OUTPTR)
 220  CALL FILCLS (OTLUN)
C                                       Copy keywords
      CALL KEYCOP (INVOL, INCNO, OUTVOL, OUTCNO, ERROR)
C                                       Create history file and
C                                       copy HI of input image:
      CALL HIINIT (3)
      CALL HISCOP (HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO, OUTCNO,
     *   CATBLK, WBUFF, IBUF, ERROR)
      IF (ERROR.GE.3) GO TO 320
C                                       Add to history:
      CALL HENCO1 (PRGNAM, INNAME, INCLAS, ASEQ, INVOL, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      ASEQ = CATBLK(KIIMS)
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
      WRITE (HILINE,1314) PRGNAM, (XPARM(I), I = 1,3)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      TEMP = XPARM(4) * XPARM(5)
      WRITE (HILINE,1315) PRGNAM, XPARM(4), TEMP
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      IF (INVERT) THEN
         WRITE (HILINE,1316) PRGNAM
         CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
         END IF
      IF (ERROR.NE.0) GO TO 320
      IF (AXONLY) THEN
         WRITE (HILINE,1317) PRGNAM
         CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
         END IF
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1318) PRGNAM
      IF (ZEROFL) WRITE (HILINE,1319) PRGNAM
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1320) PRGNAM, BLNCNT(1)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1321) BLNCNT(1)
      IF (BLNCNT(1).GT.0) CALL MSGWRT (3)
      WRITE (HILINE,1322) PRGNAM, BLNCNT(2)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1323) BLNCNT(2)
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
     *   OUTCNO, CATBLK, WBUFF, IBUF, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      GO TO 990
C                                       Print error message
 980  CONTINUE
      LABEL = MOD (ICALL, 1000)
      ICALL = ICALL / 1000
      WRITE (MSGTXT,1980) SUBS(ICALL), ERROR, LABEL
 985  CALL MSGWRT (8)
      IF (LWWSUB(ICALL)) CALL PRTERR (ERROR, NLUN)
      IRET = 16
      CALL FILCLS (INLUN)
      CALL FILCLS (OTLUN)
C=====      CALL FILDES (OTNAM, F, DUM, DUM, ERROR)
C                                       go home
 990  CALL TSKEND (IRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('The secret recursion (slow) flag is enabled!')
 1019 FORMAT ('DOES NOT ROTATE RECTANGULAR PIXELS: USE HGEOM')
 1050 FORMAT ('Note:', I4, ' planes will be processed.')
 1310 FORMAT (A6,'BLC =',2(I5,','),4(I4,','),I4)
 1311 FORMAT (A6,'TRC =',2(I5,','),4(I4,','),I4)
 1312 FORMAT (A6,'IMSIZE =',I5,',',I5,10X,'/ Output image size')
 1313 FORMAT (A6,' / Interpolation order used was Bi',A8)
 1314 FORMAT (A6,'XSHIFT=',1PE13.5,' YSHIFT=',1PE13.5,' ROTATE=',
     *   0PF9.3)
 1315 FORMAT (A6,'XSCALE=',1PE13.5,' YSCALE=',1PE13.5)
 1316 FORMAT (A6,' / Inverse of above operation actually performed')
 1317 FORMAT (A6,' / Header not altered for shift of data')
 1318 FORMAT (A6,' / Indeterminate pixels filled with magic values')
 1319 FORMAT (A6,' / Indeterminate pixels filled with zeros')
 1320 FORMAT (A6,' / ',I10,' pixels blanked due to geometry')
 1321 FORMAT (I10,' Pixels blanked due to geometry')
 1322 FORMAT (A6,' / ',I10,' Pixels blanked due to input blanked',
     *   ' pixels')
 1323 FORMAT (I10,' Pixels blanked due to input blanked pixels')
 1330 FORMAT ('WARNING: HI FILE HAD PROBLEMS.  ERROR=', I7)
 1980 FORMAT ('SUBROUTINE ', A, ' RETURNED ERROR', I6, ' AT', I4)
 4001 FORMAT ('Using',I12,' words of buffer')
      END
      SUBROUTINE GEOHDR (BLC, TRC)
C-----------------------------------------------------------------------
C   Generate header for output image. The output header starts out as
C      a copy (in CATBLK/CATR/CATD) of the input header, and is
C      transformed to the new header.
C   APARM(10), the transformation params:
C      (1)=x-shift,    (2)=y-shift (pixels)
C      (3)=rotate(deg),(4)=scale
C      (5)=diff.scale, (6)=interp.order
C      (7)=invert.flag,(8)=axes-only.flag,
C      (9)=zero.flag
C   Transformation matrix scheme allows concatenation and inversion
C      of transformations:
C                          | a  d  0 |
C      [xp,yp,1] = [x,y,1] | b  e  0 |
C                          | c  f  1 |
C               or:
C         xp = a*x + b*y + c
C         yp = d*x + e*y + f
C   This is discussed in Ch.6, 'Two-dimensional Transformations', of
C      the book 'Principles of Interactive Computer Graphics' (1973)
C      by Newman and Sproul.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CHTM12*12
      REAL       BLC(7), TRC(7), XMARKI, YMARKI, XMARKO, YMARKO, DTHETA,
     *   DET(2), WORK(3), XCRP, YCRP, TEMP
      INTEGER    I, NDIM, J, IPVT(3), JOB, II
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'LGEOM.INC'
      INCLUDE 'LGEOM2.INC'
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
      CATR(KRBLK) = 0.0
C                                       Compute 'reference marks':
      XMARKI = (BLC1 + TRC1) * 0.5
      YMARKI = (BLC2 + TRC2) * 0.5
      XMARKO = (   1 + NPXO) * 0.5 + XPARM(1)
      YMARKO = (   1 + NRWO) * 0.5 + XPARM(2)
C                                       Compute transformation matrix:
      DTHETA  = XPARM(3) * 3.14159265 / 180.0
      AA(1,1) = COS (DTHETA) / XPARM(4)
      AA(2,1) = SIN (DTHETA) / XPARM(4)
      AA(3,1) = XMARKI
C
      AA(1,2) = -AA(2,1)
      AA(2,2) =  AA(1,1)
      AA(3,2) = YMARKI
C
      AA(2,2) = AA(2,2) / XPARM(5)
      AA(2,1) = AA(2,1) / XPARM(5)
C
      AA(3,1) = AA(3,1) - XMARKO * AA(1,1) - YMARKO * AA(2,1)
      AA(3,2) = AA(3,2) - XMARKO * AA(1,2) - YMARKO * AA(2,2)
C
      AA(1,3) = 0.0
      AA(2,3) = 0.0
      AA(3,3) = 1.0
C                                       Invert the transformation:
      DO 5 I = 1,3
         DO 5 J = 1,3
            AAI(I,J) = AA(I,J)
 5       CONTINUE
      CALL SGEFA (AAI, 3, 3, IPVT, ERROR)
      IF (ERROR.NE.0) GO TO 980
      JOB = 1
      CALL SGEDI (AAI, 3, 3, IPVT, DET, WORK, JOB)
C                                       Check INVERT flag:
      IF (XPARM(7).LE.0.0) GO TO 8
         DO 7 I = 1,3
            DO 7 J = 1,3
               TEMP     = AA(I,J)
               AA(I,J)  = AAI(I,J)
               AAI(I,J) = TEMP
 7             CONTINUE
         WRITE (MSGTXT,1007)
         CALL MSGWRT (4)
C                                       Set output dimensions:
 8    CATBLK(KINAX+0) = NPXO
      CATBLK(KINAX+1) = NRWO
      DO 10 I = 3,7
         CATBLK(KINAX+I-1) = (TRC(I) - BLC(I) + 1.01)
 10      CONTINUE
C                                       Set reference pixels:
      IF (AXONLY) GO TO 100
         XCRP = CATR(KRCRP+0)
         YCRP = CATR(KRCRP+1)
         CATR(KRCRP+0) = (XCRP * AAI(1,1) + YCRP * AAI(2,1)) + AAI(3,1)
         CATR(KRCRP+1) = (XCRP * AAI(1,2) + YCRP * AAI(2,2)) + AAI(3,2)
         NDIM = CATBLK(KIDIM)
         DO 20 I = 3,7
            CATR(KRCRP+I-1) = CATR(KRCRP+I-1) - BLC(I) + 1.00
            IF (I.GT.NDIM) CATR(KRCRP+I-1) = 1.0
 20         CONTINUE
C                                       Coordinate increments:
         CATR(KRCIC+0) = CATR(KRCIC+0) / XPARM(4)
         CATR(KRCIC+1) = CATR(KRCIC+1) / XPARM(4) / XPARM(5)
C                                       Rotation angle:
         CATR(KRCRT+1) = CATR(KRCRT+1)
     *      - XPARM(3) * SIGN (1.0, (CATR(KRCIC+0) * CATR(KRCIC+1)))
C                                       Alternate axis parameter
         IF (CATBLK(KIALT).EQ.0) GO TO 200
            DO 30 I = 1,KICTPN
               II = KHCTP + (I-1) * 2
               DO 25 J = 1,3
                  CALL H2CHR (4, 1, CATH(II), CHTM12)
                  IF (FCHARS(J)(1:4).EQ. CHTM12(1:4)) GO TO 40
 25               CONTINUE
 30            CONTINUE
            GO TO 200
C                                       Fix alternate axis description
 40         CONTINUE
               IF (I.GE.3) CATR(KRARP) = CATR(KRARP) - BLC(I) + 1.0
               IF (I.EQ.1) CATR(KRARP) = (CATR(KRARP) * AAI(1,1) +
     *               YCRP * AAI(2,1)) + AAI(3,1)
               IF (I.EQ.2) CATR(KRARP) = (XCRP * AAI(1,2) +
     *               CATR(KRARP) * AAI(2,2)) + AAI(3,2)
               GO TO 200
C                                       AXES-ONLY case:
 100  CONTINUE
         WRITE (MSGTXT,1100)
         CALL MSGWRT (4)
         IF ((XPARM(3).EQ.0.0) .AND. (XPARM(4).EQ.1.0) .AND.
     *      (XPARM(5).EQ.1.0)) GO TO 200
            WRITE (MSGTXT,1101)
            CALL MSGWRT (8)
            ERROR = 13
            GO TO 980
C
 200  CONTINUE
         GO TO 999
C                                       Error exit:
 980  CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1007 FORMAT ('Inverse transformation will be done (XPARM(7)=TRUE)')
 1100 FORMAT ('Axes-only case will be done (XPARM(8)=TRUE)')
 1101 FORMAT ('Transformation params illegal for axes-only case!')
      END
      SUBROUTINE GEOSUB (INLUN, OTLUN, NPXID, ZI, PZI, ZO, XI, YI,
     *   MAGIC)
C-----------------------------------------------------------------------
C   GEOSUB uses the 'scrolling buffer' concept of how to do
C      geometric transformations. The main program gives it a
C      working array, ZI(NPXI,NPZI), to use. As a transformation
C      proceeds GEOSUB must read new rows from the input image. It
C      also needs to forget old rows which are no longer needed.
C      The solution adopted is to maintain a table of subscripts in
C      array PZI(NPZI) which point at the rows in ZI(,).
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
      INTEGER   INLUN, OTLUN, PZI(1),  I4T
      REAL      ZI(NPXID,*), ZO(*), XI(*), YI(*), DX,
     *   DY, SVX(14), SVY(14), XSUM, YSUM, ZIKP, GIANT
      INTEGER   KO, JO, I, J, JFIRST, JLAST, JMIN, JMAX, IXI,
     *   IYI, MORD1, MORD21, MORD12, JPZI, JMN, JMX, JP, KP, K, IPL
      LOGICAL    LIVE, MAGIC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'LGEOM.INC'
      INCLUDE 'LGEOM2.INC'
      DATA GIANT /1.0E+30/
C-----------------------------------------------------------------------
      BLNCNT(1) = 0
      BLNCNT(2) = 0
      ERROR = 0
      MORD1 = MORD + 1
      MORD21 = MORD + MORD1
      MORD12 = MORD1 + MORD1
      CALL ISETWT (MORD)
C                                       Giant number for max/min loop:
      ZMN = GIANT
      ZMX = -ZMN
      IPL = 1024000. / NPXO + 0.5
C                                       Loop over planes:
      DO 210 KO = 1,NPLANE
C                                       Main loop over output rows:
      DO 200 JO = 1,NRWO
C                                       Print row number every tenth
C                                       row:
      IF (MOD(JO,IPL).EQ.0) THEN
         WRITE (MSGTXT,1000) JO
         CALL MSGWRT (1)
         END IF
C                                       Compute warped coordinates:
      DY = JO
      DO 5 I = 1,NPXO
         DX = I
         XI(I) = (DX * AA(1,1) + DY * AA(2,1)) + AA(3,1)
         YI(I) = (DX * AA(1,2) + DY * AA(2,2)) + AA(3,2)
C                                       round to interp accuracy
         I4T = 60.0 * XI(I) + 0.5
         XI(I) = I4T / 60.0
         I4T = 60.0 * YI(I) + 0.5
         YI(I) = I4T / 60.0
 5       CONTINUE
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
         DO 80 J = 2,NPZI
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
      IF ( ((JFIRST.LE.JMIN).AND.(JMAX.LE.JLAST)) .OR.LBLANK) GO TO 105
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
                  IF ((JP.LT.JFIRST) .AND. (JFIRST.GT.IBLC2)) GO TO 140
                  IF ((JP.GT.JLAST) .AND. (JLAST.LT.ITRC2)) GO TO 140
                  IF ((JP.LT.(IBLC2-MORD1)) .OR. (JP.GT.(ITRC2+MORD1)))
     *               GO TO 140
                  IF (JP.LT.IBLC2) JP = IBLC2 + IBLC2 - JP - 1
                  IF (JP.GT.ITRC2) JP = ITRC2 + ITRC2 - JP + 1
 110           CONTINUE
               JPZI = PZI(JP - JFIRST + 1)
               XSUM = 0.0
C                                       Do an x-interpolation:
               DO 120 K = 1,MORD12
                  KP = K + IXI
                  IF ((KP.GE.IBLC1) .AND. (KP.LE.ITRC1)) GO TO 115
                     IF ((KP.LT.IBLC1-MORD1) .OR. (KP.GT.ITRC1+MORD1))
     *                  GO TO 140
                     IF (KP.LT.IBLC1) KP = IBLC1 + IBLC1 - KP - 1
                     IF (KP.GT.ITRC1) KP = ITRC1 + ITRC1 - KP + 1
 115              ZIKP = ZI((KP-IBLC1+1),JPZI)
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
C                                       I/O errors print here:
 980  WRITE (MSGTXT,1980) ICALL, NLUN, ERROR
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Currently on output row number',I6)
 1024 FORMAT ('Begin plane', I4)
 1100 FORMAT ('Memory limit. Some pixels probably blanked.')
 1210 FORMAT ('*****  ALL OUTPUT PIXELS BLANKED  *****')
 1980 FORMAT ('Error exit at 980 in GEOSUB:', 3I6)
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
      INCLUDE 'LGEOM2.INC'
C-----------------------------------------------------------------------
      CALL SETCOF ( MORD )
C                                       Initialize weight array:
      DO 10 I = 1,61
         DXI = MIN ((I - 1.0) / 60.0 + 1.0, 1.99999)
         CALL IEVERT (SAVWTS(1,I), DXI, J)
 10      CONTINUE
      RETURN
      END
      SUBROUTINE IGETWT (SVECT, DX, IWT)
C-----------------------------------------------------------------------
C     Fetch weights from the table computed by ISETWT.
C     The point of this scheme is to avoid the recursive computation of
C     the weights. The old slow scheme is still supported as a secret
C     option for test purposes.
C-----------------------------------------------------------------------
      INTEGER     IWT
      REAL        SVECT(*), DX
      INTEGER     I, J
      INCLUDE 'LGEOM2.INC'
C-----------------------------------------------------------------------
C                                       Fast table lookup:
      IF (LRECUR) GO TO 20
         IWT = DX
         J = (DX - IWT) * 60.0 + 1.5
         IWT = IWT - IS0
         DO 10 I = 1,NVALS
            SVECT(I) = SAVWTS(I,J)
 10         CONTINUE
         GO TO 30
C                                       Slow recomputation:
 20   CONTINUE
         CALL IEVERT (SVECT, DX, IWT)
 30   CONTINUE
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
      REAL      SVECT(*), DX
      INTEGER   N, NR, IR0, IV0, MC, IR
      REAL      U, W,  CW,  CU,  W2, U2
      INCLUDE 'LGEOM2.INC'
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
      INCLUDE 'LGEOM2.INC'
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
      REAL       X, VAL(*)
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
      SUBROUTINE SGEFA (A, LDA, N, IPVT, INFO)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C     SGEFA FACTORS A REAL MATRIX BY GAUSSIAN ELIMINATION.
C
C     SGEFA IS USUALLY CALLED BY SGECO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR SGECO) = (1 + 9/N)*(TIME FOR SGEFA) .
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT SGESL OR SGEDI WILL DIVIDE BY ZERO
C                     IF CALLED.  USE  RCOND  IN SGECO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS SAXPY,SSCAL,ISAMAX
C
C     INTERNAL VARIABLES
C-----------------------------------------------------------------------
      INTEGER   LDA, N, IPVT(*), INFO
      REAL      A(LDA,*), T
      INTEGER   J, K, KP1, L, NM1, M
C-----------------------------------------------------------------------
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      INFO = 0
      NM1 = N - 1
      IF (NM1.LT.1) GO TO 70
      DO 60 K = 1,NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         M = N - K + 1
         CALL ISAMAX (M, A(K,K), 1, L)
         L = L + K  - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0E0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0E0/A(K,K)
            M = N - K
            CALL SSCAL(M,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1,N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL SAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0E0) INFO = N
      RETURN
      END
      SUBROUTINE SGEDI(A,LDA,N,IPVT,DET,WORK,JOB)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   LDA,N,IPVT(*),JOB
      REAL   A(LDA,*),DET(2),WORK(*)
C
C     SGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
C     USING THE FACTORS COMPUTED BY SGECO OR SGEFA.
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE OUTPUT FROM SGECO OR SGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM SGECO OR SGEFA.
C
C        WORK    REAL(N)
C                WORK VECTOR.  CONTENTS DESTROYED.
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
C                OTHERWISE UNCHANGED.
C
C        DET     REAL(2)
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. ABS(DET(1)) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF SGECO HAS SET RCOND .GT. 0.0 OR SGEFA HAS SET
C        INFO .EQ. 0 .
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS SAXPY,SSCAL,SSWAP
C     FORTRAN ABS,MOD
C-----------------------------------------------------------------------
      REAL   T
      REAL   TEN
      INTEGER   I,J,K,KB,KP1,L,NM1,M
C-----------------------------------------------------------------------
C                                       COMPUTE DETERMINANT
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         TEN = 10.0E0
         DO 50 I = 1, N
            IF (IPVT(I) .NE. I) DET(1) = -DET(1)
            DET(1) = A(I,I)*DET(1)
C        ...EXIT
            IF (DET(1) .EQ. 0.0E0) GO TO 60
   10       IF (ABS(DET(1)) .GE. 1.0E0) GO TO 20
               DET(1) = TEN*DET(1)
               DET(2) = DET(2) - 1.0E0
            GO TO 10
   20       CONTINUE
   30       IF (ABS(DET(1)) .LT. 10) GO TO 40
               DET(1) = DET(1)/TEN
               DET(2) = DET(2) + 1.0E0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(U)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 150
         DO 100 K = 1, N
            A(K,K) = 1.0E0/A(K,K)
            T = -A(K,K)
            M = K - 1
            CALL SSCAL(M,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0E0
               CALL SAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM INVERSE(U)*INVERSE(L)
C
         NM1 = N - 1
         IF (NM1 .LT. 1) GO TO 140
         DO 130 KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO 110 I = KP1, N
               WORK(I) = A(I,K)
               A(I,K) = 0.0E0
  110       CONTINUE
            DO 120 J = KP1, N
               T = WORK(J)
               CALL SAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
            L = IPVT(K)
            IF (L .NE. K) CALL SSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END
      SUBROUTINE ISAMAX (N, SX, INCX, L)
C-----------------------------------------------------------------------
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C-----------------------------------------------------------------------
      REAL      SX(*), SMAX
      INTEGER   I, INCX, IX, N, L
C-----------------------------------------------------------------------
      L = 0
      IF( N .LT. 1 ) RETURN
      L = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMAX = ABS(SX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF(ABS(SX(IX)).LE.SMAX) GO TO 5
         L = I
         SMAX = ABS(SX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         IF(ABS(SX(I)).LE.SMAX) GO TO 30
         L = I
         SMAX = ABS(SX(I))
   30 CONTINUE
      RETURN
      END
      SUBROUTINE SAXPY(N,SA,SX,INCX,SY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      REAL   SX(*),SY(*),SA
      INTEGER   I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (SA .EQ. 0.0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SY(IY) + SA*SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SY(I) + SA*SX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
        SY(I) = SY(I) + SA*SX(I)
        SY(I + 1) = SY(I + 1) + SA*SX(I + 1)
        SY(I + 2) = SY(I + 2) + SA*SX(I + 2)
        SY(I + 3) = SY(I + 3) + SA*SX(I + 3)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SSCAL(N,SA,SX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      REAL   SA,SX(*)
      INTEGER   I,INCX,M,MP1,N,NINCX
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        SX(I) = SA*SX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE SSWAP (N,SX,INCX,SY,INCY)
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      REAL   SX(*),SY(*),STEMP
      INTEGER   I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        STEMP = SX(IX)
        SX(IX) = SY(IY)
        SY(IY) = STEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
        STEMP = SX(I + 1)
        SX(I + 1) = SY(I + 1)
        SY(I + 1) = STEMP
        STEMP = SX(I + 2)
        SX(I + 2) = SY(I + 2)
        SY(I + 2) = STEMP
   50 CONTINUE
      RETURN
      END
