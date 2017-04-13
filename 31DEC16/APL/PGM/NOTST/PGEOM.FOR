LOCAL INCLUDE 'PGEOM.INC'
C                                       local variables for PGEOM
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
C                                                          End PGEOM
LOCAL END
LOCAL INCLUDE 'PGEOM2.INC'
C                             Everett Interpolation internal variables:
      REAL      BCOEF(49), SV(14), SAVWTS(8,61)
      INTEGER   MORD2, IS0, NVALS
      LOGICAL   LRECUR
      COMMON /CEVI/ BCOEF, SV, SAVWTS, LRECUR, MORD2, IS0, NVALS
C                                                          End PGEOM2
LOCAL END
      PROGRAM PGEOM
C-----------------------------------------------------------------------
C! Transforms images into polar coordinates
C# Map Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2002, 2008-2010, 2012, 2015
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
C     PGEOM is a special version of GEOM devised by TAP to transform
C     images into polar coordinates.
C     The working array is dimensioned 650000 and the variable NR4
C     is declared I.
C     This should permit PGEOM to do large angle problems on 512-square.
C   Inputs (adverbs from AIPS):
C      USERID        Input image user ID
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
C                       (1)=x-reference,(2)=y-reference (pixels),
C                       (3)=inclin.ang.,(4)=position angle,
C                       (5)=bound.size, (6)=interp.order,
C                       (7)=invert.flag,(8)=spiral flag,
C                       (9)=spir.width,(10)=zero flag
C      BADDISK(10)   Disks to avoid for the scratch file.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, INNAM*36, OTNAM*36, HILINE*72,
     *   SUBS(10)*6, NAME*12, CLASS*6, PTYPE*2, TERPS(4)*8
      HOLLERITH MAP, INNAME(3), INCLAS(2), OTNAME(3), OTCLAS(2)
      INTEGER   IPXI, NPARM, SEQ, VOL, USID
      REAL      RPARM(41), USERID, INSEQ, INDISK, OTSEQ, OTDISK, BLC(7),
     *   TRC(7), IMSIZE(2), APARM(10)
C                                       Work space and pointers:
C                                       NX = 1536 more or less
      INTEGER   R4DIM
      PARAMETER (R4DIM=2)
      REAL      R4(R4DIM)
      INTEGER   I2(R4DIM), NR4, ASEQ
      LONGINT   NEXT, LXI, LYI, LZO, LZI, LPZI, PR4
C                                       Miscellaneous variables:
      REAL      TEMP, BOUND
      INTEGER   INVOL, INCNO, OUTVOL, OUTCNO, OUTPTR, INLUN,
     *   OTLUN, HLUN(2), LABEL, IRET, I, J, IERR, IDEP(5), IDEPT(5)
      LOGICAL   T, LWWSUB(10), MAGIC
      INCLUDE 'PGEOM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'PGEOM2.INC'
      EQUIVALENCE (USERID,    RPARM( 1)), (INNAME(1), RPARM( 2)),
     *            (INCLAS(1), RPARM( 5)), (INSEQ,     RPARM( 7)),
     *            (INDISK,    RPARM( 8)), (OTNAME(1), RPARM( 9)),
     *            (OTCLAS,    RPARM(12)), (OTSEQ,     RPARM(14)),
     *            (OTDISK,    RPARM(15)), (BLC(1),    RPARM(16)),
     *            (TRC(1),    RPARM(23)), (IMSIZE(1), RPARM(30)),
     *            (APARM(1),  RPARM(32))
      EQUIVALENCE (I2(1), R4(1))
      DATA PRGNAM, NPARM /'PGEOM ', 40/
      DATA TERPS /'Linear  ', 'Cubic   ', 'Quintic',
     *   'Septic  '/
      DATA NR4 /R4DIM/
      DATA SUBS /'TSKBE2', 'OPENCF', 'GETHDR',
     *           'MAPCR ', 'MAPIO ', 'MAPWIN',
     *           'GEOHDR', 'MAPFIX', 'SAVHDR',
     *           'WINDOW'/
      DATA LWWSUB /  .TRUE.,         .FALSE.,        .TRUE.,
     *               .TRUE.,         .TRUE.,         .TRUE.,
     *               .FALSE.,        .TRUE.,         .FALSE.,
     *               .FALSE./
      DATA INLUN, OTLUN,  HLUN /17, 18, 27, 28/
      DATA T, MAGIC /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C                                       Task init, get parms:
      IRET = 0
      LBLANK = .FALSE.
      ICALL = 1005
      CALL TSKBEG (PRGNAM, NPARM, RPARM(2), ERROR)
      IF (ERROR.NE.0) GO TO 980
      RPARM(1) = NLUSER
      DO 10 I = 1,10
         IBAD(I) = 0
 10      CONTINUE
      CALL RCOPY (10, APARM, XPARM)
C                                       Default & Limit:
      XPARM(6) = MIN (MAX (0.0, XPARM(6)), 7.0)
      XPARM(5) = MAX (0.0, XPARM(5))
C
      IF (XPARM(8).GT.0.0) THEN
         XPARM(9) = ABS(XPARM(9))
         IF (XPARM(9).EQ.0.0) XPARM(9) = 1.0
         END IF
      I = XPARM(6) + 0.1
      IF (I.LE.0) I = 3
      IF (I.GT.7) I = 7
      MORD = I / 2
      IF (XPARM(5).EQ.0.0) XPARM(5) = MORD + 1.0
      INVERT = (XPARM(7).GT.0.0)
      ZEROFL = (XPARM(10).GT.0.0)
C                                       Open input image:
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (INNAME, INCLAS, INSEQ, MAP, INDISK, USERID, INNAM)
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
C                                       Fix up BLC and TRC:
      ICALL = 10022
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
      IF (NPLANE.LT.1) THEN
         ICALL = 0050
         ERROR = 1
         GO TO 980
         END IF
C                                       Default for IMSIZE:
      NPXO = IMSIZE(1)
      NRWO = IMSIZE(2)
      IF (NRWO.EQ.0) NRWO = NRWI
C                                       Polar => Rectangular
      IF (XPARM(7).GT.0.0) THEN
         BOUND = 2 * XPARM(5)
         IF (NRWO.LE.BOUND) XPARM(5) = INT((NRWO-1)/2)
         NRWO = NRWO - 2*XPARM(5)
         IF (NPXO.LE.0) THEN
            IF (XPARM(5).GE.NPXI) XPARM(5) = NPXI - 1.0
            NPXO = 2 * (NPXI - XPARM(5))
            END IF
C                                       Rectangular => Polar
      ELSE
         NRWO = NRWO + 2*XPARM(5)
         TEMP = SQRT ((TRC1-BLC1)**2 + (TRC2-BLC2)**2)
         TEMP = MAX (TEMP, ABS(BLC1-XPARM(1)))
         TEMP = MAX (TEMP, ABS(BLC2-XPARM(2)))
         TEMP = MAX (TEMP, ABS(TRC1-XPARM(1)))
         TEMP = MAX (TEMP, ABS(TRC2-XPARM(2)))
         IF (IMSIZE(1).GT.16.) TEMP = TEMP / 1.414
         NPXO = XPARM(5) + MAX (TEMP, IMSIZE(1))
         END IF
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
      CALL GEOHDR (BLC, TRC, NRWI, ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Create & open output image:
      CALL H2WAWA (OTNAME, OTCLAS, OTSEQ, MAP, OTDISK, USERID, OTNAM)
      ICALL = 4125
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
      NR4 = NR4 * 1024
C                                       Compute work space pointers:
      NPZI = (NR4 - (NPXO * 3)) / NPXI
      NPZI = (NR4 - (NPXO * 3) - ((NPZI+1))) / NPXI
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
C                                       copy most header keywords
      CALL KEYCOP (INVOL, INCNO, OUTVOL, OUTCNO, IERR)
C                                       Create history file and
C                                       copy HI of input image:
      CALL HIINIT (3)
      CALL HISCOP (HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO, OUTCNO,
     *   CATBLK, WBUFF, IBUF, ERROR)
      IF (ERROR.GE.3) GO TO 320
C                                       Add to history:
      CALL WAWA2A (INNAM, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCO1 (PRGNAM, NAME, CLASS, SEQ, VOL, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      CALL WAWA2A (OTNAM, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCOO (PRGNAM, NAME, CLASS, SEQ, VOL,
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
      WRITE (HILINE,1314) PRGNAM, XPARM(1), XPARM(2)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1315) PRGNAM, XPARM(3), XPARM(4)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      IF (XPARM(8).GT.0.0) THEN
         WRITE (HILINE,1316) PRGNAM, XPARM(9)
         CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
         IF (ERROR.NE.0) GO TO 320
         END IF
      IF (INVERT) THEN
         WRITE (HILINE,1317) PRGNAM
         CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
         IF (ERROR.NE.0) GO TO 320
         END IF
      WRITE (HILINE,1318) PRGNAM
      IF (ZEROFL) WRITE (HILINE,1319) PRGNAM
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (HILINE,1320) PRGNAM, BLNCNT(1)
      CALL HIADD (HLUN(2), HILINE, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1321) BLNCNT(1)
      IF (BLNCNT(1).GT.0) CALL MSGWRT (3)
      WRITE (MSGTXT,1322) PRGNAM, BLNCNT(2)
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
      CALL ALLTAB (1, 'CC', HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO,
     *   OUTCNO, CATBLK, WBUFF, IBUF, IERR)
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
 1050 FORMAT ('Note:', I4, ' planes will be processed.')
 1310 FORMAT (A6,'BLC =',2(I5,','),4(I4,','),I4)
 1311 FORMAT (A6,'TRC =',2(I5,','),4(I4,','),I4)
 1312 FORMAT (A6,'IMSIZE =',I5,',',I5,10X,'/ output image size')
 1313 FORMAT (A6,' / Interpolation order used was BI',A8)
 1314 FORMAT (A6,'XREF=',1PE13.5,' YREF=',1PE13.5)
 1315 FORMAT (A6,'INCLINATION=',F9.3,'  ROTATION=',F9.3)
 1316 FORMAT (A6,'RADIUS=',1PE13.5,' / Spiral unwrap radius')
 1317 FORMAT (A6,' / INVERSE OF ABOVE OPERATION ACTUALLY PERFORMED')
 1318 FORMAT (A6,' / Indeterminate pixels filled with magic values')
 1319 FORMAT (A6,' / Indeterminate pixels filled with zeros')
 1320 FORMAT (A6,' / ',I10,' Pixels blanked due to geometry')
 1321 FORMAT (I10,' Pixels blanked due to geometry')
 1322 FORMAT (A6,' / ',I10,' Pixels blanked due to input blanked',
     *   ' pixels')
 1323 FORMAT (I10,' Pixels blanked due to input blanked pixels')
 1330 FORMAT ('WARNING: HI FILE HAD PROBLEMS.  ERROR=', I7)
 1980 FORMAT ('SUBROUTINE ', A, ' RETURNED ERROR', I6, ' AT', I4)
 4001 FORMAT ('Using',I12,' words of buffer')
      END
      SUBROUTINE GEOHDR (BLC, TRC, NRWID, IERR)
C-----------------------------------------------------------------------
C   Generate header for output image.
C-----------------------------------------------------------------------
      REAL       BLC(7), TRC(7)
      INTEGER    NRWID, IERR
      CHARACTER FCHARS(3)*4, XLAB*4, YLAB*8, PIX*4, CHTM12*12
      REAL XCRP, YCRP, XCIC, YCIC, TEMP, PHI, ROTA, COSP, RAD, ALPHA,
     *   XCRPO, YCRPO, THETA, TEMPX, TEMPY, RHO, SINT, COST, PI, TPI,
     *   YE, THESCL, XSGN, YSGN, AROT, TBIAS
      INTEGER    I, II, NDIM, J, IDEPTH(5), LIM
      LOGICAL    EQUAL, NOSWAP
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PGEOM.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /PGEPRM/ PI, TPI, YE, THESCL, XSGN, YSGN, AROT, TBIAS
      DATA FCHARS /'FREQ','VELO','FELO'/
      DATA XLAB, YLAB, PIX /'RAD-','ANGL    ','-PIX'/
      DATA NOSWAP /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Call SETLOC to obtain useful
C                                       header information
      DO 10 I = 1,5
         IDEPTH(I) = BLC(I+2) + 0.1
 10      CONTINUE
      LOCNUM = 1
      CALL SETLOC (IDEPTH, NOSWAP)
C                                       Set some constants
      PI = 3.14159265
      TPI = 2.0 * PI
      YE = 1.0 + XPARM(5)
      IF (XPARM(7).LE.0.0) THESCL = TPI / (NRWO - 2.0*XPARM(5))
      IF (XPARM(7).GT.0.0) THESCL = TPI / (NRWID - 2.0*XPARM(5))
      XSGN = 1.
      YSGN = 1.
      AROT = 0.
      TBIAS = 0.
C                                       Convert angles to radians
      PHI  = XPARM(3) * PI / 180.0
      ROTA = XPARM(4) * PI / 180.0
      COSP = COS(PHI)
      CATR(KRBLK) = 0.0
C                                       Move axes appropriately:
C                                       Rectangular => Polar
      IF (XPARM(7).LE.0.0) THEN
C                                       Set geometry parms
         IF (CATR(KRCIC).LT.0.0) XSGN = -1.
         IF (CATR(KRCIC+1).LT.0.0) YSGN = -1.
         TEMP = SQRT (ABS (CATR(KRCIC) * CATR(KRCIC+1)))
         IF (TEMP.LE.0.0) TEMP = 1.0
         IF ((AXTYP(LOCNUM).EQ.1) .AND.
     *      (AXFUNC(1,LOCNUM).EQ.AXFUNC(2,LOCNUM))) THEN
            XSGN = CATR(KRCIC) / TEMP
            YSGN = CATR(KRCIC+1) / TEMP
            END IF
         IF (CORTYP(LOCNUM).EQ.2) THEN
            YSGN = -YSGN
            TBIAS = PI / 2.
            AROT = CATR(KRCRT)
            CATR(KRCRT) = CATR(KRCRT) + XPARM(4)
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            AROT = CATR(KRCRT+1)
            CATR(KRCRT+1) = CATR(KRCRT+1) + XPARM(4)
            END IF
         IF (CATBLK(KIDIM).GT.5) THEN
            WRITE (MSGTXT,1010)
            CALL MSGWRT (5)
         ELSE IF ((CATBLK(KINAX+5).GT.1) .OR. (CATBLK(KINAX+6).GT.1))
     *      THEN
            WRITE (MSGTXT,1011)
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
            END IF
         NDIM = CATBLK(KIDIM) + 2
         NDIM = MIN(NDIM,KICTPN)
         LIM  = NDIM - 2
         DO 30 I = 1,LIM
            CATD(KDCRV + NDIM - I) = CATD(KDCRV + NDIM - I - 2)
            CATR(KRCIC + NDIM - I) = CATR(KRCIC + NDIM - I - 2)
            CATR(KRCRP + NDIM - I) = CATR(KRCRP + NDIM - I - 2)
            CATR(KRCRT + NDIM - I) = CATR(KRCRT + NDIM - I - 2)
            CATBLK(KINAX + NDIM - I) = CATBLK(KINAX + NDIM - I - 2)
            CALL CHCOPY (8, 1, CATH(KHCTP+(NDIM-I-2)*2), 1,
     *         CATH(KHCTP+(NDIM-I)*2))
 30         CONTINUE
C                                       Polar => Rectangular
      ELSE
C                                       Set geometry parms
         CORTYP(LOCNUM) = 0
         IF ((KLOCL(LOCNUM).EQ.3) .AND. (KLOCM(LOCNUM).EQ.4))
     *      CORTYP(LOCNUM) = 1
         IF ((KLOCL(LOCNUM).EQ.4) .AND. (KLOCM(LOCNUM).EQ.3))
     *      CORTYP(LOCNUM) = 2
         IF (CATR(KRCIC+2).LT.0.0) XSGN = -1.
         IF (CATR(KRCIC+3).LT.0.0) YSGN = -1.
         TEMP = ABS (CATR(KRCIC))
         IF (TEMP.LE.0.0) TEMP = 1.0
         IF ((CORTYP(LOCNUM).NE.0) .AND.
     *      (AXFUNC(3,LOCNUM).EQ.AXFUNC(4,LOCNUM))) THEN
            XSGN = CATR(KRCIC+2) / TEMP
            YSGN = CATR(KRCIC+3) / TEMP
            END IF
         IF (CORTYP(LOCNUM).EQ.2) THEN
            YSGN = -YSGN
            TBIAS = PI / 2.
            AROT = CATR(KRCRT+2)
            CATR(KRCRT+2) = CATR(KRCRT+2) - XPARM(4)
         ELSE IF (CORTYP(LOCNUM).EQ.1) THEN
            AROT = CATR(KRCRT+3)
            CATR(KRCRT+3) = CATR(KRCRT+3) - XPARM(4)
            END IF
         NDIM = CATBLK(KIDIM) - 2
         IF (NDIM.GE.5) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (5)
            END IF
         DO 70 I = 1,NDIM
            CATD(KDCRV + I - 1) = CATD(KDCRV + I + 1)
            CATR(KRCIC + I - 1) = CATR(KRCIC + I + 1)
            CATR(KRCRP + I - 1) = CATR(KRCRP + I + 1)
            CATR(KRCRT + I - 1) = CATR(KRCRT + I + 1)
            CATBLK(KINAX + I - 1) = CATBLK(KINAX + I + 1)
            CALL CHCOPY (8, 1, CATH(KHCTP+(I+1)*2), 1,
     *         CATH(KHCTP+(I-1)*2))
 70         CONTINUE
         END IF
C
C                                       Output dimensions
C
      CATBLK(KINAX+0) = NPXO
      CATBLK(KINAX+1) = NRWO
C                                       Rectangular => Polar
      IF (XPARM(7).LE.0.0) THEN
         CATBLK(KINAX+2) = 1
         CATBLK(KINAX+3) = 1
         DO 93 I = 5,NDIM
            CATBLK(KINAX+I-1) = (TRC(I-2) - BLC(I-2) + 1.01)
 93         CONTINUE
C                                       Polar => Rectangular
      ELSE
         DO 97 I = 3,NDIM
            CATBLK(KINAX+I-1) = (TRC(I+2) - BLC(I+2) + 1.01)
 97         CONTINUE
         END IF
C                                       Set reference pixels
C                                       Rectangular => Polar
      IF (XPARM(7).LE.0.0) THEN
         XCRP = CATR(KRCRP+2)
         YCRP = CATR(KRCRP+3)
         TEMPX = (XCRP - XPARM(1)) / XSGN
         TEMPY = (YCRP - XPARM(2)) / YSGN
         RAD = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
         ALPHA = 0.0
         IF (RAD.NE.0.0) ALPHA = ATAN2 (TEMPX, TEMPY) - ROTA - AROT -
     *      TBIAS
         TEMPX = COS(ALPHA) * RAD
         TEMPY = 0.0
         IF (COSP.NE.0.0) TEMPY = SIN(ALPHA) * RAD / COSP
         RHO = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
         THETA = 0.0
         IF (RHO.NE.0.0) THETA = ATAN2 (TEMPY, TEMPX)
         IF (THETA.LT.0.0) THETA = TPI + THETA
         IF (XPARM(8).GT.0.0) RHO = RHO + XPARM(9) -
     *      XPARM(9) * THETA / TPI
         XCRPO = RHO + YE
         YCRPO = THETA / THESCL + YE
         CATR(KRCRP+0) = YE
         CATR(KRCRP+1) = YE
         CATR(KRCRP+2) = XCRPO
         CATR(KRCRP+3) = YCRPO
         DO 110 I = 3,LIM
            CATR(KRCRP+I+1) = CATR(KRCRP+I+1) - BLC(I) + 1.00
 110        CONTINUE
C                                       Polar => Rectangular
      ELSE
         XCRP = CATR(KRCRP)
         YCRP = CATR(KRCRP+1)
         THETA = THESCL * (YCRP - YE)
         SINT = SIN(THETA)
         COST = COS(THETA)
         RHO = XCRP - YE
         IF (XPARM(8).GT.0.0) THEN
            TEMP = RHO - XPARM(9) + XPARM(9) * THETA / TPI
            RHO = MAX(0.0, TEMP)
            END IF
         TEMPX = RHO * COST
         TEMPY = RHO * SINT * COSP
         RAD = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
         ALPHA = 0.0
         IF (RAD.NE.0.0) ALPHA = ATAN2 (TEMPY, TEMPX)
         XCRPO = XSGN * RAD * COS(ALPHA+ROTA+AROT+TBIAS) + XPARM(1)
         YCRPO = YSGN * RAD * SIN(ALPHA+ROTA+AROT+TBIAS) + XPARM(2)
         CATR(KRCRP) = XCRPO
         CATR(KRCRP+1) = YCRPO
         END IF
C                                       Reference pixel value
C                                       Rectangular => Polar
      IF (XPARM(7).LE.0.0) THEN
         CATD(KDCRV) = 0.0D0
         CATD(KDCRV+1) = 0.0D0
C                                       Polar => Rectangular
C                                          nothing to do
C                                       Coordinate increments
C                                       Rectangular => Polar
         XCIC = CATR(KRCIC+2)
         YCIC = CATR(KRCIC+3)
         CATR(KRCIC) = SQRT (ABS (XCIC * YCIC))
         CATR(KRCIC+1) = 360.0 / (NRWO - 2.0*XPARM(5))
         IF ((AXTYP(LOCNUM).NE.1) .OR. (CATR(KRCIC).LE.0.0) .OR.
     *      (AXFUNC(1,LOCNUM).NE.AXFUNC(2,LOCNUM))) CATR(KRCIC) = 1.0
C                                       Polar => Rectangular
C                                       nothing to do
C                                       Axis types
C                                       Rectangular => Polar
         CALL CHR2H (4, XLAB, 1, CATH(KHCTP))
         CALL CHR2H (8, YLAB, 1, CATH(KHCTP+2))
         IF ((AXTYP(LOCNUM).NE.1) .OR.
     *      (AXFUNC(1,LOCNUM).NE.AXFUNC(2,LOCNUM)))
     *      CALL CHR2H (4, PIX, 5, CATH(KHCTP))
C                                       Polar => Rectangular
C                                       nothing to do
C                                       Rotation
C                                       Rectangular => Polar
         CATR(KRCRT) = 0.0
         CATR(KRCRT+1) = 0.0
         END IF
C                                       Polar => Rectangular
C                                          nothing to do
C                                       Number of Axes
      CATBLK(KIDIM) = NDIM
C                                       Alternate axis parameter
      IF (CATBLK(KIALT).NE.0) THEN
         DO 230 I = 1,KICTPN
            II = KHCTP + (I-1) * 2
            DO 220 J = 1,3
               CALL H2CHR (4, 1, CATH(II), CHTM12)
               EQUAL = FCHARS(J)(1:4) .EQ. CHTM12(1:4)
C                                       Fix alternate axis description
               IF (EQUAL) THEN
                  CATR(KRARP) = CATR(KRARP) - BLC(I) + 1.0
                  GO TO 999
                  END IF
 220           CONTINUE
 230        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('Too many axes, losing valuable information')
 1011 FORMAT ('Lost axis with more than one pixel on it!')
 1050 FORMAT ('May not have been able to recover complete header.')
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
C   Note: May want to introduce tests to avoid making redundant calls
C      to IEVERT. Example: simple shift needs only two calls to
C      compute SVX and SVY once for whole image.
C-----------------------------------------------------------------------
      INTEGER   NPXID
      INTEGER   INLUN, OTLUN, PZI(*)
      REAL      ZI(NPXID,*), ZO(*), XI(*), YI(*)
      REAL       DX, DY, SVX(14), SVY(14), XSUM,
     *   YSUM, ZIKP, GIANT, THETA, SINT, COST, RHO, PHI, ROTA, RAD,
     *   TEMPX, TEMPY, ALPHA, TEMP, COSP,
     *   PI, TPI, YE, THESCL, XSGN, YSGN, AROT, TBIAS
      INTEGER   KO, JO, I, J, JFIRST, JLAST, JMIN, JMAX, IXI,
     *   IYI, MORD1, MORD21, MORD12, JPZI, JMN, JMX, JP, KP, K, IPL
      LOGICAL    LIVE, MAGIC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'PGEOM.INC'
      COMMON /PGEPRM/ PI, TPI, YE, THESCL, XSGN, YSGN, AROT, TBIAS
      DATA GIANT /1.0E+30/
C-----------------------------------------------------------------------
      BLNCNT(1) = 0
      BLNCNT(2) = 0
      ERROR = 0
      MORD1 = MORD + 1
      MORD21 = MORD + MORD1
      MORD12 = MORD1 + MORD1
      CALL SETCOF (MORD)
C                                       Giant number for max/min loop:
      ZMN = GIANT
      ZMX = -ZMN
C                                       Convert angles to radians:
      PHI = XPARM(3) * PI / 180
      ROTA = (XPARM(4) + AROT) * PI / 180 + TBIAS
      COSP = COS(PHI)
      IPL = 102400. / NPXO + 0.5
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
      DO 10 I = 1,NPXO
         DX = I
C                                       Rectangular => Polar
         IF (XPARM(7).LE.0.0) THEN
            THETA = THESCL * (DY - YE)
            SINT = SIN(THETA)
            COST = COS(THETA)
            RHO = DX - YE
            IF (XPARM(8).GT.0.0) THEN
               TEMP = RHO - XPARM(9) + XPARM(9) * THETA / TPI
               RHO = MAX (0.0, TEMP)
               END IF
            TEMPX = RHO * COST
            TEMPY = RHO * SINT * COSP
            RAD = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
            ALPHA = 0.0
            IF (RAD.NE.0.0) ALPHA = ATAN2 (TEMPY, TEMPX)
            XI(I) = XSGN * RAD * SIN(ALPHA + ROTA) + XPARM(1)
            YI(I) = YSGN * RAD * COS(ALPHA + ROTA) + XPARM(2)
C                                       Polar => Rectangular
         ELSE
            TEMPX = (DX - XPARM(1)) / XSGN
            TEMPY = (DY - XPARM(2)) / YSGN
            RAD = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
            ALPHA = 0.0
            IF (RAD.NE.0.0) ALPHA = ATAN2 (TEMPX, TEMPY) - ROTA
            TEMPX = COS(ALPHA) * RAD
            TEMPY = 0.0
            IF (COSP.NE.0.0) TEMPY = SIN(ALPHA) * RAD / COSP
            RHO = SQRT (TEMPX*TEMPX + TEMPY*TEMPY)
            THETA = 0.0
            IF (RHO.NE.0.0) THETA = ATAN2 (TEMPY, TEMPX)
            IF (THETA.LT.0.0) THETA = TPI + THETA
            IF (XPARM(8).GT.0.0) RHO = RHO + XPARM(9)
     *         - XPARM(9) * THETA / TPI
            XI(I) = RHO + YE
            YI(I) = THETA / THESCL + YE
            END IF
 10      CONTINUE
C                                       Get min/max of y coordinates:
      JMN = ITRC2
      JMX = IBLC2
      LIVE = .FALSE.
      DO 20 I = 1 , NPXO
         IF ((XI(I).GE.BLC1) .AND. (XI(I).LE.TRC1) .AND. (YI(I).GE.BLC2)
     *      .AND. (YI(I).LE.TRC2)) THEN
            J = YI(I)
            JMN = MIN (JMN, J)
            JMX = MAX (JMX, J)
            LIVE = .TRUE.
            END IF
 20      CONTINUE
      IF (LIVE) THEN
         JMIN = MAX (IBLC2, MIN ((JMN - MORD), (ITRC2 - MORD21)) )
         JMAX = MIN (MAX ((JMIN + MORD21), (JMX + MORD1)), ITRC2)
      ELSE IF (JO.LE.1) THEN
         JMIN = IBLC2
         JMAX = JMIN
         END IF
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
 60   IF ((JMIN.GE.JFIRST) .AND. (JMAX.LE.JLAST)) GO TO 100
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
 100  IF (((JFIRST.GT.JMIN) .OR. (JMAX.GT.JLAST)) .AND. .NOT.LBLANK)
     *   THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (6)
         LBLANK = .TRUE.
         END IF
C                                       Now to compute the output row:
      DO 160 I = 1,NPXO
         IF ((XI(I).GE.BLC1) .AND. (XI(I).LE.TRC1) .AND.
     *      (YI(I).GE.BLC2) .AND. (YI(I).LE.TRC2) ) THEN
            YSUM = 0.0
C                                       Compute Everett coefficients:
            CALL IEVERT (SVX, XI(I), IXI)
            CALL IEVERT (SVY, YI(I), IYI)
C                                       Loop on y-interpolation:
            DO 130 J = 1,MORD12
               JP = J + IYI
               IF ((JP.GE.JFIRST) .AND. (JP.LE.JLAST)) GO TO 110
                  IF ((JP.LT.JFIRST) .AND. (JFIRST.GT.IBLC2)) GO TO 140
                  IF ((JP.GT.JLAST) .AND. (JLAST.LT.ITRC2)) GO TO 140
                  IF ((JP.LT.(IBLC2-MORD1)).OR.(JP.GT.(ITRC2+MORD1)))
     *                                                         GO TO 140
                  IF (JP.LT.IBLC2) JP = IBLC2 + IBLC2 - JP - 1
                  IF (JP.GT.ITRC2) JP = ITRC2 + ITRC2 - JP + 1
 110           CONTINUE
               JPZI = PZI(JP - JFIRST + 1)
               XSUM = 0.0
C                                       Do an x-interpolation:
               DO 120 K = 1, MORD12
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
            GO TO 160
            END IF
 140     CONTINUE
            IF (ZIKP.NE.INDEF) BLNCNT(1) = BLNCNT(1) + 1
            IF (ZIKP.EQ.INDEF) BLNCNT(2) = BLNCNT(2) + 1
            IF (.NOT.ZEROFL) THEN
               ZO(I) = INDEF
               MAGIC = .TRUE.
            ELSE
               ZO(I) = 0.0
               ZMX = MAX (ZMX, 0.0)
               ZMN = MIN (ZMN, 0.0)
               END IF
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
C
 980  WRITE (MSGTXT,1980) ICALL, NLUN, ERROR
 990  CALL MSGWRT (8)
C                                       I/O errors print here:
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Currently on output row number',I6)
 1024 FORMAT ('Begin plane', I4)
 1100 FORMAT ('Memory limit. Some pixels probably blanked.')
 1210 FORMAT ('*****  ALL OUTPUT PIXELS BLANKED  *****')
 1980 FORMAT ('Error exit at 980 in GEOSUB:', 3I6)
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
      INCLUDE 'PGEOM2.INC'
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
      INCLUDE 'PGEOM2.INC'
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
