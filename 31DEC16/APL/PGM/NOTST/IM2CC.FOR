LOCAL INCLUDE 'IM2CC.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), JBUFSZ, NX, NY, IBLC(7), ITRC(7), INX, INY, JNX,
     *   JNY
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), ICUT,
     *   FLUX, XNX, XNY, BUFF1(MABFSS), BUFF2(MABFSS), BMAREA
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XSEQO,
     *   XDISKO, BLC, TRC, ICUT, FLUX, XNX, XNY
      COMMON /CHPARM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO,
     *   OLDCNO, JBUFSZ, NX, NY, IBLC, ITRC, INX, INY, BMAREA, JNX, JNY
      COMMON /BUFRS/ BUFF1, BUFF2
LOCAL END
      PROGRAM IM2CC
C-----------------------------------------------------------------------
C! Task to convert a model image to multi-facet files with CC tables
C# Map-util Calibration Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2015-2016
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
C   IM2CC converts model image to multi-facet images with CC files
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      ICUT           ICUT          CC only if abs value > ICUT
C      FLUX           FLUX          CC only if value > FLUX
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'IM2CC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'IM2CC '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL IM2CCI (PRGM, IRET)
C                                       do the work
      IF (IRET.EQ.0) CALL IM2CCD (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE IM2CCI (PRGN, IRET)
C-----------------------------------------------------------------------
C   IM2CCI starts up the task, sets windows
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                               4 => user routine detected error.
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, MTYPE*2, UNITS*8, TUNITS*8
      INTEGER   IERR, NPARM, IROUND, I
      REAL      RTEMP, OLDR(256)
      INCLUDE 'IM2CC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (CATOLD, OLDR)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 30
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Hollerith -> char.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
      NX = IROUND (XNX)
      NY = IROUND (XNY)
      IF (NX.LT.1) NX = 5
      IF (NY.LT.1) NY = 5
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Set integer windows
      DO 10 I = 1,7
         IBLC(I) = IROUND (BLC(I))
         ITRC(I) = IROUND (TRC(I))
 10      CONTINUE
C                                       round up for facet size
      INX = (ITRC(1) - IBLC(1)) / NX + 1
      INY = (ITRC(2) - IBLC(2)) / NY + 1
C                                       init location common
      LOCNUM = 2
      CALL SETLOC (IBLC(3), .FALSE.)
      IF (AXTYP(LOCNUM).NE.1) THEN
         IRET = 10
         MSGTXT = 'AXIS TYPES NOT AS EXPECTED'
         GO TO 990
         END IF
C                                       units conversion
      CALL H2CHR (8, 1, CATH(KHBUN), UNITS)
      TUNITS = UNITS
      CALL CHLTOU (8, TUNITS)
      IF (TUNITS.EQ.'JY/PIXEL') THEN
         BMAREA = 1.0
      ELSE IF (TUNITS.EQ.'JY/BEAM') THEN
         BMAREA = 1.1331 * CATR(KRBMJ) * CATR(KRBMN)
         RTEMP = ABS (CATR(KRCIC) * CATR(KRCIC+1))
         IF ((BMAREA.GT.0.0) .AND. (RTEMP.GT.0.0)) THEN
            BMAREA = BMAREA / RTEMP
            MSGTXT = 'CONVOLVED IMAGES DO NOT MAKE GOOD MODELS'
            CALL MSGWRT (6)
         ELSE
            MSGTXT = 'BEAM AREA IS INDETERMINATE, USING 1.0'
            CALL MSGWRT (7)
            BMAREA = 1.0
            END IF
      ELSE
         BMAREA = 1.0
         MSGTXT = 'UNITS TAKEN AS JY/PIXEL, BUT ARE ACTUALLY ' // UNITS
         CALL MSGWRT (7)
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IM2CCI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE IM2CCD (IRET)
C-----------------------------------------------------------------------
C   IM2CCD loops over the subimages, creating the files, making the CC
C   files and images and updating the headers.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, LUNO, NYI, NXI, WINI(4), NXO, NYO, JPAN,
     *   WINO(4), BOI, BOO, I1, I2, I3, I4, I5, I6, I7, IPOS(5), JPLAN,
     *   CORN(5), BOTEMP, CCBUFF(512), CCLUN, CCRNO, CCKOLS(7), IXP,
     *   CCNUMV(7), NCOL, IBIND, OBIND, INDI, INDO, NCC, NCCM, NCCT,
     *   IYP, ITYPE, KEY(2,2), KEYSUB(2,2), VER, XOFF, YOFF, XXTRA,
     *   YXTRA, I
      REAL      OUTMAX, OUTMIN, OLD4(256), XPIX, YPIX, VAL, XX, YY,
     *   CCP(4), FKEY(2,2)
      DOUBLE PRECISION    OLD8(128), RA, DEC, XRA, XDEC, Z
      LOGICAL   T, F, BLNKD
      INCLUDE 'IM2CC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      EQUIVALENCE (CATOLD, OLD4, OLD8)
      EQUIVALENCE (I3, IPOS(1)), (I4, IPOS(2)), (I5, IPOS(3)),
     *   (I6, IPOS(4)), (I7, IPOS(5))
      DATA LUNI, LUNO /66,67/
      DATA T, F /.TRUE.,.FALSE./
      DATA KEYSUB /1,1,1,1/
      DATA FKEY /-1.0, 0.0, 1.0, 0.0/
      DATA KEY / 1, 0, 2, 0/
C-----------------------------------------------------------------------
C                                       panel loop
      JPAN = 0
C                                       Open the data files
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, JPAN, 'OPEN INPUT IMAGE FILE'
         GO TO 990
         END IF
      NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
      ITYPE = 1
      DO 900 IYP = 1,NY
         DO 800 IXP = 1,NY
            JPAN = JPAN + 1
            WINI(1) = IBLC(1) + (IXP-1) * INX
            WINI(2) = IBLC(2) + (IYP-1) * INY
            WINI(3) = WINI(1) + INX - 1
            WINI(4) = WINI(2) + INY - 1
            IF (WINI(3).GT.ITRC(1)) WINI(3) = ITRC(1)
            IF (WINI(4).GT.ITRC(2)) WINI(4) = ITRC(2)
            NXO = WINI(3) - WINI(1) + 1
            NYO = WINI(4) - WINI(2) + 1
C                                       power 2 above that
            CALL POWER2 (NXO, JNX)
            IF (JNX.LT.NXO) JNX = JNX * 2
            CALL POWER2 (NYO, JNY)
            IF (JNY.LT.NYO) JNY = JNY * 2
            XOFF = (JNX - NXO) / 2
            XXTRA = JNX - NXO - XOFF
            YOFF = (JNY - NYO) / 2
            YXTRA = JNY - NYO - YOFF
C                                       output size
            CATBLK(KINAX) = JNX
            CATBLK(KINAX+1) = JNY
C                                       coordinate info
            XPIX = WINI(1) + NXO/2 - 1
            YPIX = WINI(2) + NYO/2
            IF (CORTYP(LOCNUM).EQ.1) THEN
               CALL XYVAL (XPIX, YPIX, XRA, XDEC, Z, IRET)
               RA = CATD(KDCRV)
               DEC = CATD(KDCRV+1)
            ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
               CALL XYVAL (XPIX, YPIX, XDEC, XRA, Z, IRET)
               RA = CATD(KDCRV+1)
               DEC = CATD(KDCRV)
               END IF
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, JPAN, 'FIND REF COORDINATE'
               GO TO 990
               END IF
            CATR(KRCRP) = OLD4(KRCRP) - WINI(1) + 1 + XOFF
            CATR(KRCRP+1) = OLD4(KRCRP+1) - WINI(2) + 1 + YOFF
            CATR(KRXSH) = XRA - RA
            CATR(KRYSH) = XDEC - DEC
C                                       create output image
            WRITE (CLAOUT,1010) JPAN
            CALL CHR2H (6, CLAOUT, KHIMCO, CATR(KHIMC))
            SEQOUT = IROUND (XSEQO)
            CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
            NEWCNO = 1
            CALL MCREAT (DISKO, NEWCNO, BUFF1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, JPAN, 'CREATE OUTPUT IMAGE'
               GO TO 990
               END IF
            NCFILE = NCFILE + 1
            FVOL(NCFILE) = DISKO
            FCNO(NCFILE) = NEWCNO
            FRW(NCFILE) = 2
C                                       open output
            CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
            CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, JPAN, 'OPEN OUTPUT IMAGE'
               GO TO 990
               END IF
C                                       Setup for I/O
            WINO(1) = 1
            WINO(2) = 1
            WINO(3) = CATBLK(KINAX)
            WINO(4) = CATBLK(KINAX+1)
            OUTMAX = -1.0E20
            OUTMIN = 1.0E20
            BLNKD = F
C                                       Loop
            JPLAN = 0
            NCCM = 0
            NCCT = 0
            DO 700 I7 = IBLC(7),ITRC(7)
               CORN(5) = I7 - IBLC(7) + 1
               DO 600 I6 = IBLC(6),ITRC(6)
                  CORN(4) = I6 - IBLC(6) + 1
                  DO 500 I5 = IBLC(5),ITRC(5)
                     CORN(3) = I5 - IBLC(5) + 1
                     DO 400 I4 = IBLC(4),ITRC(4)
                        CORN(2) = I4 - IBLC(4) + 1
                        DO 300 I3 = IBLC(3),ITRC(3)
                           CORN(1) = I3 - IBLC(3) + 1
                           JPLAN = JPLAN + 1
C                                       Create CC table
                           NCOL = 3
                           CCLUN = 18
                           NCC = 0
                           CALL CCMINI ('WRIT', CCBUFF, DISKO, NEWCNO,
     *                        JPLAN, CATBLK, CCLUN, CCRNO, CCKOLS,
     *                        CCNUMV, NCOL, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, JPAN,
     *                           'CREATE CC TABLE'
                              GO TO 990
                              END IF
C                                       Init. files, first input.
                           CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX),
     *                        IPOS, BOTEMP, IRET)
                           BOI = BOTEMP + 1
                           CALL MINIT ('READ', LUNI, INDI, NXI, NYI,
     *                        WINI, BUFF1, JBUFSZ, BOI, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, JPAN,
     *                           'INIT READ INPUT IMAGE'
                              GO TO 990
                              END IF
C                                       Init output file.
                           CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX),
     *                        CORN, BOTEMP, IRET)
                           BOO = BOTEMP + 1
                           CALL MINIT ('WRIT', LUNO, INDO, JNX, JNY,
     *                        WINO, BUFF2, JBUFSZ, BOO, IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, JPAN,
     *                           'INIT WRIT OUTPUT IMAGE'
                              GO TO 990
                              END IF
                           DO 200 I2 = 1,YOFF
                              CALL MDISK ('WRIT', LUNO, INDO, BUFF2,
     *                           OBIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET, JPAN,
     *                              'WRITE OUTPUT IMAGE'
                                 GO TO 990
                                 END IF
                              CALL RFILL (JNX, 0.0, BUFF2(OBIND))
 200                          CONTINUE
                           DO 220 I2 = WINI(2),WINI(4)
C                                       Read.
                              CALL MDISK ('READ', LUNI, INDI, BUFF1,
     *                           IBIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET, JPAN,
     *                              'READ INPUT IMAGE'
                                 GO TO 990
                                 END IF
                              CALL MDISK ('WRIT', LUNO, INDO, BUFF2,
     *                           OBIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET, JPAN,
     *                              'WRITE OUTPUT IMAGE'
                                 GO TO 990
                                 END IF
C                                       copy values
                              CALL RFILL (XOFF, 0.0, BUFF2(OBIND))
                              CALL RFILL (XXTRA, 0.0,
     *                           BUFF2(OBIND+JNX-XXTRA))
                              DO 210 I1 = 1,NXO
                                 VAL = BUFF1(IBIND+I1-1)
                                 BUFF2(OBIND+I1-1+XOFF) = VAL
                                 IF (VAL.NE.FBLANK) THEN
                                    OUTMAX = MAX (OUTMAX, VAL)
                                    OUTMIN = MIN (OUTMIN, VAL)
C                                       output a CC comp
                                    IF ((VAL.GT.FLUX) .AND.
     *                                 (ABS(VAL).GT.ICUT)) THEN
                                       VAL = VAL / BMAREA
                                       XX = (I1+WINI(1)-1-OLD4(KRCRP)) *
     *                                    OLD4(KRCIC)
                                       YY= (I2 - OLD4(KRCRP+1)) *
     *                                    OLD4(KRCIC+1)
                                       CALL TABCCM ('WRIT', CCBUFF,
     *                                    CCRNO, CCKOLS, CCNUMV, NCOL,
     *                                    XX, YY, VAL, ITYPE, CCP, IRET)
                                       IF (IRET.NE.0) THEN
                                          WRITE (MSGTXT,1000) IRET,
     *                                       JPAN, 'WRITE CC TABLE'
                                          GO TO 990
                                          END IF
                                       NCC = NCC + 1
                                       END IF
                                 ELSE
                                    BLNKD = .TRUE.
                                    END IF
 210                             CONTINUE
 220                          CONTINUE
                           DO 230 I2 = 1,YXTRA
                              CALL MDISK ('WRIT', LUNO, INDO, BUFF2,
     *                           OBIND, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET, JPAN,
     *                              'WRITE OUTPUT IMAGE'
                                 GO TO 990
                                 END IF
                              CALL RFILL (JNX, 0.0, BUFF2(OBIND))
 230                          CONTINUE
C                                       Flush buffer.
                           CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, JPAN,
     *                           'FINI WRITE OUTPUT IMAGE'
                              GO TO 990
                              END IF
C                                       close CC
                           CALL TABCCM ('CLOS', CCBUFF, CCRNO, CCKOLS,
     *                        CCNUMV, NCOL, XX, YY, VAL, ITYPE, CCP,
     *                        IRET)
                           IF (IRET.NE.0) THEN
                              WRITE (MSGTXT,1000) IRET, JPAN,
     *                           'CLOSE CC TABLE'
                              GO TO 990
                              END IF
                           NCCM = MAX (NCCM, NCC)
                           NCCT = NCCT + NCC
 300                       CONTINUE
 400                    CONTINUE
 500                 CONTINUE
 600              CONTINUE
 700           CONTINUE
C                                       close output map
            CALL ZCLOSE (LUNO, INDO, IRET)
C                                       did we do anything?
            IF (NCCT.GT.0) THEN
C                                       Update CATBLK.
               CATR(KRBLK) = 0.0
               IF (BLNKD) CATR(KRBLK) = FBLANK
               CATR(KRDMX) = OUTMAX
               CATR(KRDMN) = OUTMIN
               CATBLK(KINIT) = NCCM
C                                       history
               CALL IM2CCH (NCCM, NCCT)
C                                       sort
               VER = 1
               CALL TABSRT (DISKO, NEWCNO, 'CC', VER, VER, KEY, KEYSUB,
     *            FKEY, CCBUFF, CATBLK, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, JPAN, 'SORT THE CC TABLE'
                  GO TO 990
                  END IF
C                                       update catalog
               CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', BUFF1,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, JPAN,
     *               'UPDATE CATALOG HEADER'
                  GO TO 990
                  END IF
               NCFILE = NCFILE - 1
C                                       destroy the file
            ELSE
C                                       update catalog
               CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'CLWR', BUFF1,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, JPAN,
     *               'UPDATE CATALOG HEADER'
                  GO TO 990
                  END IF
               NCFILE = NCFILE - 1
C                                       now destroy
               CALL MDESTR (DISKO, NEWCNO, CATBLK, BUFF1, I, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, JPAN,
     *               'DESTROY EMPTY OUTPUT FACET'
                  GO TO 990
                  END IF
               JPAN = JPAN - 1
               END IF
 800        CONTINUE
 900     CONTINUE
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IM2CCD: ERROR',I3,' PANEL',I3,' ON ',A)
 1010 FORMAT ('IMC',I3.3)
      END
      SUBROUTINE IM2CCH (NCCM, NCCT)
C-----------------------------------------------------------------------
C   AHEHIS copies and updates history file.
C-----------------------------------------------------------------------
      INTEGER   NCCM, NCCT
C
      CHARACTER LINE*80, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'IM2CC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CC'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BLC
      WRITE (LINE,2000) TSKNAM, IBLC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       TRC
      WRITE (LINE,2001) TSKNAM, ITRC
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       abs value cutoff
      WRITE (LINE,2002) TSKNAM, ICUT
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       cutoff
      WRITE (LINE,2003) TSKNAM, FLUX
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       panel numbers
      WRITE (LINE,2004) TSKNAM, NX, NY
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       CCs written
      IF (NCCT.GT.NCCM) THEN
         WRITE (LINE,2005) TSKNAM, NCCM
         CALL HIADD (LUN2, LINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Total CCs written
      WRITE (LINE,2006) TSKNAM, NCCT
      CALL HIADD (LUN2, LINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('im2cch: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(I6,','),I6)
 2001 FORMAT (A6,'TRC =',6(I6,','),I6)
 2002 FORMAT (A6,'ICUT=',F10.4,'  / abs(pixel) > ICUT to include')
 2003 FORMAT (A6,'FLUX=',F10.4,'  / pixel value > FLUX to include')
 2004 FORMAT (A6,'NX=',I3,' NY=',I3,'  / number panels in X and Y')
 2005 FORMAT (A6,'NCCMAX=',I12,'  / max number CCs in any table')
 2006 FORMAT (A6,'NCCTOT=',I12,'  / total number CCs in all tables')
      END
