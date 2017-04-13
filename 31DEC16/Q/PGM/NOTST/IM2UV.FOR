LOCAL INCLUDE 'IM2UV.INC'
C                                       Local include for IM2UV
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   LUNWK1, LUNWK2, WK1VOL, WK2VOL, JBUFSZ, SEQIN, SEQOUT,
     *   DISKI, DISKO, CNOIN, CNOUT, CATIM(256), CATUV(256), BLC(7),
     *   TRC(7), NFFTX, NFFTY
      REAL   SCALEU, SCALEV, SCLVIS, XOFF, YOFF, XSI, XDI, XBLC(7),
     *   XTRC(7), XSO, XDO, WTZERO, XUVTAP(2), XUVRA(2), FLUX0,
     *   XXSHFT(2), XBADD(10), BUFF1(UVBFSS), BUFF2(UVBFSS),
     *   BUFF3(UVBFSS), UVTAP(2)
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMOU(3), XCLAOU(2)
      CHARACTER WK1FIL*48, WK2FIL*48, NAMEIN*12, CLASIN*6, NAMOUT*12,
     *   CLAOUT*6
      LOGICAL   DOTAP
      COMMON /I2UCOM/ CATIM, CATUV, SCALEU, SCALEV, SCLVIS, XOFF, YOFF,
     *   LUNWK1, LUNWK2, WK1VOL, WK2VOL,
     *   JBUFSZ, CNOIN, CNOUT, NFFTX, NFFTY
      COMMON /I2UCHR/ WK1FIL, WK2FIL, NAMEIN, CLASIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3
      COMMON /INPUTS/ XNAMEI, XCLASI, XSI, XDI, XBLC, XTRC, XNAMOU,
     *   XCLAOU, XSO, XDO, WTZERO, XUVTAP, XUVRA, FLUX0, XXSHFT, XBADD,
     *   SEQIN, SEQOUT, DISKI, DISKO, BLC, TRC, UVTAP, DOTAP
C                                                          End IM2UV
LOCAL END
      PROGRAM IM2UV
C-----------------------------------------------------------------------
C! FFTs an image and converts it to UV data form
C# UV Map AP-fft
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2006, 2008-2009, 2015
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
C   IM2UV reads an image, FFTs it and writes a selected portion
C   as a UV data file.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN       Name of input image.
C      INCLASS        CLASIN        Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKI         Disk number of input.
C      BLC            BLC           Botton right corner of input
C      TRC            TRC           Top right corner of input
C      OUTNAME        NAMOUT        Name of the output uv data file.
C      OUTCLASS       CLAOUT        Class of output file.
C      OUTSEQ         SEQOUT        Seq. number of output file.
C      OUTDISK        DISKO         Disk number of the output file.
C      UVRANGE        XUVRA          UV range of output.
C      BADDISK(10)    IBAD(10)      Disks to avoid for scratch files
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, CATBLK(256)
      INCLUDE 'IM2UV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /MAPHDR/ CATBLK
      DATA PRGM /'IM2UV '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file .
      CALL I2UINI (PRGM, IRET)
C                                       Transform image.
      IF (IRET.EQ.0) CALL DOI2U (IRET)
C                                       Write history
      IF (IRET.EQ.0) CALL I2UHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE I2UINI (PRGN, JERR)
C-----------------------------------------------------------------------
C   I2UINI gets input parameters for IM2UV and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                6 => axes in wrong order
C                                7 => Too many axes
C                                8 => can't start
C   Commons: /INPUTS/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in I2U for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, BLANK*6, JY*8, JYPBM*8,
     *   COMPLX*8, RTYPES(9)*8, CHTM12*12, MTYPE*2, CTEMP*8
      HOLLERITH CATH(256), CATMH(256)
      INTEGER   JERR
      INTEGER   CATBLK(256), INDEX, NPARM, I, J, IROUND, IERR, NAX,
     *   NP(2), JNDEX, NUMVIS, ISIZE
      LOGICAL   T, EQUAL, NOSWAP
      REAL      CATR(256), CATMR(256), ABOX, AANN, RATIO, XPOS, YPOS
      DOUBLE PRECISION CATD(128), CATMD(128), XRA, XDEC
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IM2UV.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PMAD.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR, CATD, CATH, CATBLK)
      EQUIVALENCE (CATIM, CATMR, CATMD, CATMH)
      DATA BLANK /' '/
      DATA  JY,         JYPBM,     COMPLX
     *    /'JY ','JY/BEAM ', 'COMPLEX '/
      DATA T, NOSWAP /.TRUE.,.FALSE./
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L    ', 'VV-L    ', 'WW-L    ',
     *   'TIME1   ', 'SUBARRAY', 'ANTENNA1', 'ANTENNA2', 2*' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2  * UVBFSS
      LUNWK1 = 16
      LUNWK2 = 18
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 46
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = T
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQOUT = IROUND (XSO)
      DISKI = IROUND (XDI)
      DISKO = IROUND (XDO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Get CATBLK from old file.
      CNOIN = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKI, CNOIN, NAMEIN, CLASIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLASIN, SEQIN, DISKI,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI, CNOIN, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKI
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       Save input header
      CALL COPY (256, CATBLK, CATIM)
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), XBLC, XTRC, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       BLC, TRC
      DO 20 I = 1,7
         BLC(I) = IROUND (XBLC(I))
         TRC(I) = IROUND (XTRC(I))
 20      CONTINUE
C                                       Baddisk
      DO 30 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 30      CONTINUE
C                                       UVRANGE default
      IF (XUVRA(2).LE.1.0E-20) XUVRA(2) = 1.0E12
      IF (WTZERO.LE.0.0) WTZERO = 1.0
      DOTAP = (XUVTAP(1).GT.0.0) .AND. (XUVTAP(2).GT.0.0)
      IF (DOTAP) THEN
         UVTAP(1) = LOG(0.3) / (XUVTAP(1) * 1000.0)**2
         UVTAP(2) = LOG(0.3) / (XUVTAP(2) * 1000.0)**2
         END IF
C                                       Blanked image - barf
      IF (ABS (CATR(KRBLK)).GT.0.001) THEN
         IERR = 6
         MSGTXT = 'ERROR: I CANNOT COPE WITH BLANKED IMAGES - USE' //
     *      ' ZERO BLANKING'
         GO TO 990
         END IF
C                                       Default output name etc.
      CALL MAKOUT (NAMEIN, CLASIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Decide size of FFT
      NFFTX = (LOG(1.0*(TRC(1)-BLC(1)+1)) / LOG (2.0)) + 0.999
      NFFTX = 2 ** NFFTX
      NFFTY = (LOG (1.0*(TRC(2)-BLC(2)+1)) / LOG (2.0)) + 0.999
      NFFTY = 2 ** NFFTY
C                                       Blew max buffer size. ?
      IF ((NFFTY.GT.MAXIMG) .OR. (NFFTX.GT.MAXIMG)) THEN
         MSGTXT = 'ERROR: CANNOT HANDLE LARGER THAN 16384 IMAGES'
         JERR = 9
         GO TO 990
         END IF
C                                       U and V scaling factors, cells
C                                       to wavelengths
      SCALEU = 2.06265E5 / (NFFTX * ABS (CATR(KRCIC) * 3600.0))
      SCALEV = 2.06265E5 / (NFFTY * ABS (CATR(KRCIC+1) * 3600.0))
C                                       Visibility scaling
      SCLVIS = 1.1331 * CATR(KRBMJ) * CATR(KRBMN)
      IF (SCLVIS.GT.0.0) THEN
         SCLVIS = ABS(CATR(KRCIC)*CATR(KRCIC+1)) / SCLVIS
      ELSE
         SCLVIS = 1.0
         END IF
      SCLVIS = SCLVIS * NFFTX
      SCLVIS = SCLVIS * NFFTY
C                                       Redo headers
      LOCNUM = 1
      CALL SETLOC (BLC(3), NOSWAP)
C                                       new phase ref point
      XDEC = CATD(KDCRV+1) + XXSHFT(2) / 3600.0
      XRA = CATD(KDCRV) + (XXSHFT(1) / 3600.) / COS (DG2RAD * XDEC)
      CALL XYPIX (XRA, XDEC, XPOS, YPOS, IERR)
C                                       Output file size
      NUMVIS = (NFFTX / 2) + 1
      NUMVIS = NUMVIS * NFFTY
C                                       Approx. correction for UVRANGE
      ABOX = ((NFFTX/2) * SCALEU) * ((NFFTY-1) * SCALEV)
      AANN = 3.1415926 * ((XUVRA(2)*XUVRA(2)*1.0E6) -
     *   (XUVRA(1)*XUVRA(1)*1.0E6))
C                                       Add a bit for margin
      RATIO = (1.10 * AANN / ABOX)
      IF (RATIO.LT.1.0) NUMVIS = NUMVIS * RATIO
      CATBLK(KIGCN) = NUMVIS
C                                       Random parameters
      CATBLK(KIPCN) = 7
C                                       Random axis names
      DO 50 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * 2
         IF (I.LE.7) THEN
            CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
         ELSE
            CALL CHR2H (8, RTYPES(8), 1, CATH(INDEX))
            END IF
 50      CONTINUE
C                                       Save projection type on u,v,w
      CATH(KHPTP+1) = CATH(KHCTP+1)
      CATH(KHPTP+3) = CATH(KHCTP+1)
      CATH(KHPTP+5) = CATH(KHCTP+1)
C                                       Uniform axes
C                                       Axes in wrong order
      IF ((KLOCL(LOCNUM).NE.0) .OR. (KLOCM(LOCNUM).NE.1)) THEN
         JERR = 6
         WRITE (MSGTXT,1050)
         GO TO 990
         END IF
      CATBLK(KIDIM) = CATBLK(KIDIM) + 1
C                                       Too many axes - can't cope
      IF (CATBLK(KIDIM).GT.KICTPN) THEN
         JERR = 7
         WRITE (MSGTXT,1051)
         GO TO 990
         END IF
      J = CATBLK(KIDIM)
C                                       First axis is complex
      CATBLK(KINAX) = 3
      CATR(KRCRP) = 1.0
      CATR(KRCIC) = 1.0
      CATR(KRCRT) = 0.0
      CATD(KDCRV) = 1.0D0
      CALL CHR2H (8, COMPLX, 1, CATH(KHCTP))
C                                       Slide everything down 1
      DO 60 I = 1,J-3
C                                       Subimaging
         CATBLK(KINAX+I) = TRC(I+2) - BLC(I+2) + 1
         CATR(KRCRP+I) = CATR(KRCRP+I+1) - BLC(I+2) + 1
         CATR(KRCIC+I) = CATR(KRCIC+I+1)
         CATR(KRCRT+I) = CATR(KRCRT+I+1)
         CATD(KDCRV+I) = CATD(KDCRV+I+1)
         INDEX = KHCTP + (I + 1) * 2
         JNDEX = KHCTP + I * 2
         CATH(JNDEX) = CATMH(INDEX)
         CATH(JNDEX+1) = CATMH(INDEX+1)
 60      CONTINUE
C                                       Add RA and Dec to end
      DO 70 I = J-1,J
         CATBLK(KINAX+I-1) = 1
         CATR(KRCRP+I-1) = 1.0
         CATR(KRCIC+I-1) = 1.0
         CATR(KRCRT+I-1) = 0.0
         CATD(KDCRV+I-1) = XRA
         IF (I.EQ.J) CATD(KDCRV+I-1) = XDEC
         INDEX = KHCTP + (I - J + 1) * 2
         JNDEX = KHCTP + (I - 1) * 2
         CALL H2CHR (8, 1, CATMH(INDEX), CTEMP)
         IF (CTEMP(:5).EQ.'RA---') CTEMP = 'RA'
         IF (CTEMP(:5).EQ.'DEC--') CTEMP = 'DEC'
         CALL CHR2H (8, CTEMP, 1, CATH(JNDEX))
 70      CONTINUE
C                                       Misc.
C                                       Max. and min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Convolving beam.
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
      CATBLK(KITYP) = 0
C                                       Units, if JY/BEAM use JY.
      CALL H2CHR (8, 1, CATH(KHBUN), CHTM12)
      CALL CHLTOU (8, CHTM12)
      EQUAL = CHTM12(1:8) .EQ. JYPBM(1:8)
      IF (EQUAL) CALL CHR2H (8, JY, 1, CATH(KHBUN))
C                                       Sort Order
      CALL CHR2H (2, 'X*', 1, CATH(KITYP))
      CALL UVPGET (IERR)
C                                       Can correctly do only 1 freq
      IF (JLOCF.GT.0) THEN
C                                       Correct frequency
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) +
     *      (CATR(KRCRP+JLOCF)-1.0D0) * CATR(KRCIC+JLOCF)
C                                       Reset reference pixel
         CATR(KRCRP+JLOCF) = 1.0
C                                       Give warning
         IF (CATBLK(KINAX+JLOCF).GT.1) THEN
            WRITE (MSGTXT,1052)
            CALL MSGWRT (6)
            CATBLK(KINAX+JLOCF) = 1
            TRC(2+JLOCF) = BLC(2+JLOCF)
            END IF
         END IF
C                                       Create output uv data file.
      CNOUT = 1
      JERR = 4
      CALL UVCREA (DISKO, CNOUT, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 85
         WRITE (MSGTXT,1080) IERR
         GO TO 990
 85   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOUT
      FRW(NCFILE) = 2
      SEQOUT = CATBLK(KIIMS)
C                                       copy some keywords
      CALL KEYPCP (DISKI, CNOIN, DISKO, CNOUT, 0, ' ', IERR)
C                                       UV header common
      CALL UVPGET (IERR)
C                                       Save catalog header
      CALL COPY (256, CATBLK, CATUV)
C                                       Phase center offsets
      XOFF = (XPOS - ((BLC(1) + TRC(1)) / 2 + 1)) *
     *   CATMR(KRCIC) * DG2RAD
      YOFF = (YPOS - ((BLC(2) + TRC(2)) / 2 + 1)) *
     *   CATMR(KRCIC+1) * DG2RAD
C                                       Dummy AN table
      CALL ANTFIL (JERR)
C                                       Create scratch files
C                                       Work file 1
      NAX = 2
      NP(1) =  NFFTX + 4
      NP(2) = NFFTY + 4
      CALL MAPSIZ (NAX, NP, ISIZE)
      CALL SCREAT (ISIZE, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1081) IERR
         GO TO 990
         END IF
C                                       Get name
      WK1VOL = SCRVOL(NSCR)
      CALL ZPHFIL ('SC', WK1VOL, SCRCNO(NSCR), 1, WK1FIL, IERR)
C                                       Work file 2
      CALL SCREAT (ISIZE, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1081) IERR
         GO TO 990
         END IF
C                                       Get name
      WK2VOL = SCRVOL(NSCR)
      CALL ZPHFIL ('SC', WK2VOL, SCRCNO(NSCR), 1, WK2FIL, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('I2UINI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR: AXES IN WRONG ORDER, USE TASK TRANS')
 1051 FORMAT ('ERROR: ALL AXES ARE USED AND I NEED ONE MORE.')
 1052 FORMAT ('WARNING: I CAN ONLY DO 1 FREQUENCY AT A TIME, CHANGING',
     *   ' TRC')
 1080 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1081 FORMAT ('I2UINI: ERROR',I3,' CREATING SCRATCH FILE')
      END
      SUBROUTINE DOI2U (IRET)
C-----------------------------------------------------------------------
C   DOI2U selects and transforms each plane and then writes the
C   "visibility" in the uv data file.
C   Output:
C   IRET   I    Return code, 0 => OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LIM3, LIM4, LIM5, LIM6, LIM7, WIN(4), IPOINT, CORN(7),
     *   I3, I4, I5, I6, I7, IXOFF, IYOFF, IDIR, KAP, NEED
      LOGICAL   T, F, ISFRST
      REAL      DUM1, DUM2, SCALE
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IM2UV.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Setup for looping
      ISFRST = T
      IPOINT = NRPARM
      WIN(1) = BLC(1)
      WIN(2) = BLC(2)
      WIN(3) = TRC(1)
      WIN(4) = TRC(2)
      LIM3 = TRC(3) - BLC(3) + 1
      LIM4 = TRC(4) - BLC(4) + 1
      LIM5 = TRC(5) - BLC(5) + 1
      LIM6 = TRC(6) - BLC(6) + 1
      LIM7 = TRC(7) - BLC(7) + 1
      SCALE = -1.0
C                                       Loop over planes
      DO 700 I7 = 1,LIM7
         CORN(7) = BLC(7) + I7 - 1
         DO 600 I6 = 1,LIM6
            CORN(6) = BLC(6) + I6 - 1
            DO 500 I5 = 1,LIM5
               CORN(5) = BLC(5) + I5 - 1
               DO 400 I4 = 1,LIM4
                  CORN(4) = BLC(4) + I4 - 1
                  DO 300 I3 = 1,LIM3
                     CORN(3) = BLC(3) + I3 - 1
C                                       Get plane with zero padding.
         IXOFF = 0
         IYOFF = 0
         CALL PLNGET (DISKI, CNOIN, CORN, WIN, IXOFF, IYOFF, 1, NFFTX,
     *      NFFTY, BUFF1, BUFF2, JBUFSZ, JBUFSZ, LUNWK1, LUNWK2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Do FFT.
C                                       should be enough
         NEED = 4 * NFFTX * NFFTY + 4 * (NFFTX + NFFTY)
         NEED = NEED / 1024
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = 0
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 2 * NFFTX * NFFTY + 2 * (NFFTX + NFFTY)
            NEED = NEED / 1024
            NEED = MIN (32 * 1024, NEED)
            MSGSUP = 32000
            CALL QINIT (NEED, 0, KAP)
            MSGSUP = 0
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               NEED = 5120
               CALL QINIT (NEED, 0, KAP)
               IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
                  MSGTXT = 'DOI2U: FAILED TO GET ANY AP MEMORY'
                  CALL MSGWRT (8)
                  IRET = 10
                  GO TO 999
                  END IF
               END IF
            END IF
         IDIR = 3
         CALL DSKFFT (NFFTX, NFFTY, IDIR, T, 1, 2, 1, JBUFSZ,
     *      BUFF1, BUFF2, DUM1, DUM2, IRET)
         CALL QRLSE
         IF (IRET.NE.0) GO TO 999
         IF (SCALE.LT.0.0) THEN
            IF (FLUX0.GT.0.0) THEN
               SCALE = 0.0
            ELSE IF (FLUX0.LT.0) THEN
               IF (DUM1.LE.0.0) DUM1 = 1.0
               SCALE = -FLUX0 / (DUM1 * SCLVIS)
            ELSE
               SCALE = 1.0
               END IF
           END IF
C                                       Copy to uv data file.
         CALL I2U (ISFRST, IPOINT, SCALE, IRET)
         IF (IRET.NE.0) GO TO 999
         ISFRST = F
         IPOINT = IPOINT + 3
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE I2U (ISFRST, IPOINT, SCALE, IRET)
C-----------------------------------------------------------------------
C   I2U takes a complex rotated (center at corners) image WK1
C   and puts the real and imaginary parts in the output uv data file.
C   The data in the work file is half plane complex, rotated by 90 deg.
C   and with the center at the edges.
C        The phase of the visibilities are rotated to correct the
C   position offset due to subimaging.
C       On the first call (ISFRST=true) a count of the visibilities is
C   made and the output file is compressed if necessary.
C   Inputs:
C     ISFRST    L    If true UV data records are initialized, else
C                    the previous version is read and updated.
C     IPOINT    I    0-rel visibility offset pointer, increment by 3
C                    each call.
C   In/out:
C      SCALE    R    Peak flux found so far
C   Inputs from common:
C     XOFF      R    Offset to correct in x direction (radians)
C     YOFF      R    Offset to correct in y direction (radians)
C     SCALEU    R    U Cells to wavelengths scaling factor
C     SCALEV    R    V Cells to wavelengths scaling factor
C     SCLVIS    R    Visibility scaling factor
C     XUVRA(2)  R    Min. and max. "baseline" length (kilowavelengths)
C   Output:
C     IRET      I    Return error code., 0 =>OK otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IPOINT, IRET
      LOGICAL   ISFRST
      REAL      SCALE
C
      CHARACTER NAM(2)*4, FNAME*48
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO, WIN(4),
     *   MX, MY, NY2, IERR, II, I, IROW, LUNUVI, LUNUVO, NIOIN, NIOUT,
     *   LOOP, VO, BOUV, NUMVIS
      LOGICAL   T, F, DOSHFT
      REAL      U, V, W, BLMAX2, BLMIN2, BL2, XFAC, YFAC, TR, TI, WT,
     *   PHASE, CP, SP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'IM2UV.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO /1/
      DATA NAM /'WORK','UV  '/
      DATA LUNUVI, LUNUVO /17,18/
      DATA VO, BOUV /0,1/
C-----------------------------------------------------------------------
      NY2 = NFFTY / 2
C                                       Baseline limits
      BLMIN2 = XUVRA(1) * XUVRA(1) * 1.0E6
      BLMAX2 = XUVRA(2) * XUVRA(2) * 1.0E6
C                                       Shift
      XFAC = - XOFF * 6.283185308
      YFAC = - YOFF * 6.283185308
      DOSHFT = (ABS (XOFF).GT.1.0E-20) .OR. (ABS (YOFF).GT.1.0E-20)
C                                       Open work file.
      CALL ZOPEN (LUNWK1, FIND1, WK1VOL, WK1FIL, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(1)
         GO TO 990
         END IF
C                                       Init input file.
      MY = NFFTY * 2
      MX = NFFTX / 2 + 1
C                                       find scaling
      IF (SCALE.LE.0.0) THEN
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = MY
         WIN(4) = 1
C                                       Read row with 0,0
         CALL MINIT ('READ', LUNWK1, FIND1, MY, MX, WIN, BUFF1, JBUFSZ,
     *      BO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT', NAM(1)
            GO TO 990
            END IF
C                                       Read row.
         CALL MDISK ('READ', LUNWK1, FIND1, BUFF1, BIND1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', NAM(1)
            GO TO 990
            END IF
         SCALE = SQRT (BUFF1(BIND1)**2 + BUFF1(BIND1+1)**2) * SCLVIS
         IF (SCALE.LE.0.0) THEN
            WRITE (MSGTXT,1005) SCALE
            CALL MSGWRT (6)
            SCALE = FLUX0
            END IF
         IF (FLUX0.GT.0.0) THEN
            SCALE = FLUX0 / SCALE
         ELSE
            SCALE = 1.0
            END IF
         END IF
C                                       init full I/O (backwards)
      WIN(1) = 1
      WIN(2) = MX
      WIN(3) = MY
      WIN(4) = 1
C                                       Read backward for sort order.
      CALL MINIT ('READ', LUNWK1, FIND1, MY, MX, WIN, BUFF1, JBUFSZ, BO,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', NAM(1)
         GO TO 990
         END IF
C                                       Read file this pass.
      IF (.NOT.ISFRST) THEN
         CALL ZPHFIL ('UV', DISKO, CNOUT, 1, FNAME, IRET)
         CALL ZOPEN (LUNUVI, FIND2, DISKO, FNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(2)
            GO TO 990
            END IF
C                                        Init UV file 'READ'.
         NIOIN = 1
         CALL UVINIT ('READ', LUNUVI, FIND2, NVIS, VO, LREC, NIOIN,
     *      JBUFSZ, BUFF2, BOUV, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT', NAM(2)
            GO TO 990
            END IF
C                                       Initialize BUFF2 with zeroes
      ELSE
         DO 50 LOOP = 1,LREC
            BUFF2(LOOP) = 0.0
 50         CONTINUE
         BIND2 = 1
         NUMVIS = 0
         END IF
C                                        Open UV file for write.
      CALL ZPHFIL ('UV', DISKO, CNOUT, 1, FNAME, IRET)
      CALL ZOPEN (LUNUVO, FIND3, DISKO, FNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(2)
         GO TO 990
         END IF
C                                        Init UV file  WRITE.
      NIOUT = 1
      CALL UVINIT ('WRIT', LUNUVO, FIND3, NVIS, VO, LREC, NIOUT, JBUFSZ,
     *   BUFF3, BOUV, BIND3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', NAM(2)
         GO TO 990
         END IF
C                                       Loop over rows.
      DO 200 IROW = 1,MX
C                                       Set u, w
         U = (MX - IROW) * SCALEU
         W = 0.0
C                                       Read row.
         CALL MDISK ('READ', LUNWK1, FIND1, BUFF1, BIND1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', NAM(1)
            GO TO 990
            END IF
C                                       Process row
         DO 150 I = 1,MY,2
C                                       Compute v
            II = (I - 1) / 2
            IF ((II+1).GT.NY2) II = II - NFFTY
            V = - II * SCALEV
C                                       Check if in range
            IF (DOTAP) THEN
               WT = UVTAP(1) * U * U + UVTAP(2) *  V * V
               WT = EXP (WT)
            ELSE
               WT = 1.0
               END IF
            BL2 = U*U + V*V
            IF ((BL2.GE.BLMIN2) .AND. (BL2.LE.BLMAX2) .AND.
     *         (WT.GT.0.0001)) THEN
C                                       Read old value
               IF (.NOT.ISFRST) THEN
                  NIOIN = 1
                  CALL UVDISK ('READ', LUNUVI, FIND2, BUFF2, NIOIN,
     *               BIND2, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'READ', NAM(2)
                     GO TO 990
                     END IF
C                                       Verify correct record
                  IF ((U.NE.BUFF2(BIND2+ILOCU)) .OR.
     *                (V.NE.BUFF2(BIND2+ILOCV))) THEN
                     IRET = 10
                     WRITE (MSGTXT,1050)
                     GO TO 990
                     END IF
C                                       Initialize record
               ELSE
                  BUFF2(BIND2+ILOCU) = U
                  BUFF2(BIND2+ILOCV) = V
                  BUFF2(BIND2+ILOCW) = W
                  IF (ILOCB.GE.0) THEN
                     BUFF2(BIND2+ILOCB) = 258.0
                  ELSE
                     BUFF2(BIND2+ILOCA1) = 1.0
                     BUFF2(BIND2+ILOCA2) = 2.0
                     BUFF2(BIND2+ILOCSA) = 1.0
                     END IF
                  NUMVIS = NUMVIS + 1
                  END IF
C                                       Enter vis.
               BUFF2(BIND2+IPOINT) = BUFF1(BIND1+I-1) * SCLVIS * SCALE
               BUFF2(BIND2+IPOINT+1) = BUFF1(BIND1+I) * SCLVIS * SCALE
               BUFF2(BIND2+IPOINT+2) = WTZERO * WT
C                                       Phase shift
               IF (DOSHFT) THEN
                  PHASE = XFAC * U + YFAC * V
                  CP = COS (PHASE)
                  SP = SIN (PHASE)
                  TR = BUFF2(BIND2+IPOINT)
                  TI = BUFF2(BIND2+IPOINT+1)
                  BUFF2(BIND2+IPOINT) = TR * CP - TI * SP
                  BUFF2(BIND2+IPOINT+1) = TR * SP + TI * CP
                  END IF
C                                       Copy to output
               DO 100 LOOP = 1,LREC
                  BUFF3(BIND3+LOOP-1) = BUFF2(BIND2+LOOP-1)
 100              CONTINUE
C                                       (Re)write record
               NIOUT = 1
               CALL UVDISK ('WRIT', LUNUVO, FIND3, BUFF3, NIOUT, BIND3,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT', NAM(2)
                  GO TO 990
                  END IF
C                                       End processing of vis.
               END IF
 150        CONTINUE
 200     CONTINUE
C                                         Flush buffer.
      NIOUT = 0
      CALL UVDISK ('FLSH', LUNUVO, FIND3, BUFF3, NIOUT, BIND3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLSH', NAM(2)
         GO TO 990
         END IF
C                                       Compress output file
      IF (ISFRST) THEN
         NVIS = NUMVIS
         CALL UCMPRS (NVIS, DISKO, CNOUT, LUNUVO, CATUV, IERR)
         END IF
C                                         Close files.
      CALL ZCLOSE (LUNWK1, FIND1, IERR)
      IF (.NOT.ISFRST) CALL ZCLOSE (LUNUVI, FIND2, IERR)
      CALL ZCLOSE (LUNUVO, FIND3, IERR)
      IRET = 0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('I2U: ERROR',I3,1X,A4,'ING ',A4,' FILE')
 1005 FORMAT ('I2U: PEAK FLUX',1PE10.3,' USING NULL SCALING')
 1050 FORMAT ('I2U: I HAVE MESSED UP THE OUTPUT FILE - QUITING')
      END
      SUBROUTINE I2UHIS
C-----------------------------------------------------------------------
C   I2UHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'IM2UV.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKI, DISKO, CNOIN, CNOUT, CATUV,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, DISKI, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       UVRANGE
      WRITE (HILINE,2002) TSKNAM, XUVRA
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       WTZERO
      WRITE (HILINE,2003) TSKNAM, WTZERO
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       UVTAPER
      WRITE (HILINE,2004) TSKNAM, XUVTAP
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       FLUX
      IF (FLUX0.GT.0.0) THEN
         WRITE (HILINE,2005) TSKNAM, FLUX0
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       SHIFT
      WRITE (HILINE,2006) TSKNAM, XXSHFT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables.
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKI, DISKO,
     *   CNOIN, CNOUT, CATUV, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 30
         WRITE (MSGTXT,1020)
         CALL MSGWRT (6)
C                                       Update CATBLK.
 30   CALL CATIO ('UPDT', DISKO, CNOUT, CATUV, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('I2UHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1020 FORMAT ('I2UHIS: ERROR COPYING TABLES')
 2000 FORMAT (A6,'BLC = ',7I6)
 2001 FORMAT (A6,'TRC = ',7I6)
 2002 FORMAT (A6,'UVRANGE = ',1PE12.5,1X,E12.5,' / kilo lambda')
 2003 FORMAT (A6,'WTZERO  = ',F9.4,16X,' / data weight at 0,0')
 2004 FORMAT (A6,'UVTAPER = ',1PE12.5,1X,E12.5,' / kilo lambda')
 2005 FORMAT (A6,'FLUX    = ',1PE12.5,13X,' / data value at 0,0')
 2006 FORMAT (A6,'SHIFT   = ',1PE12.5,1X,E12.5,' / arc seconds')
      END
      SUBROUTINE ANTFIL (IERR)
C-----------------------------------------------------------------------
C   ANTFIL creates and fills a dummy 2 antenna AN table.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER ANTNAM(2)*8, DUMMY*8
      INTEGER   NANT, VER, LUN, I
      REAL      CATRUV(256)
      HOLLERITH CATHUV(256)
      DOUBLE PRECISION ANTLOC(3,2), GST0, XUT1, XIAT, JD, GMSTM, GASTM,
     *   RATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'IM2UV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATUV, CATRUV, CATHUV)
      DATA NANT /2/
      DATA ANTNAM /'DUMMY 1 ', 'DUMMY 2 '/
      DATA ANTLOC /3*0.0D0, 3*1.0D0/
      DATA GST0, XIAT, XUT1 /3*0.0D0/
      DATA DUMMY /'DUMMY   '/
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (NANT.LE.0) GO TO 999
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = XUT1
         DATUTC = XIAT
         TIMSYS = 'IAT '
C                                       Array name
         ANAME = DUMMY
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) =  0.0D0
         ARRAYC(3) =  0.0D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATHUV(KHDOB), RDATE)
         GSTIA0 = GST0
         IF (ABS (GST0).LT.1.0E-20) THEN
            CALL JULDAY (RDATE, JD)
            CALL GSTROT (JD, GMSTM, GASTM, RATE)
            GSTIA0 = GMSTM
            END IF
         DEGPDY = RATE
         SAFREQ = FREQ
         ANFQID = -1
         VER = 1
         ANTNIF = 1
         XYZHAN = ' '
         TFRAME = ' '
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFF1, DISKO, CNOUT, VER, CATUV, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB,NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = ' '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         FWHMAN(1) = 0.0
C                                       AN records
         DO 20 I = 1,NANT
            STAXYZ(1) = ANTLOC(1,I)
            STAXYZ(2) = ANTLOC(2,I)
            STAXYZ(3) = ANTLOC(3,I)
            NOSTA = I
            ANNAME = ANTNAM(I)
            IANRNO = I
            CALL TABAN ('WRIT', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 990
 20         CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 1, IANRNO, ANNAME, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING DUMMY ANTENNA TABLE')
      END
