LOCAL INCLUDE 'FFT.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for FFT
      INTEGER   NX, NY, LUNMPR, LUNMPI, LUNWK1, WK1VOL, WK2VOL, JBUFSZ,
     *   SEQINR, SEQINI, SEQOUT, DISKR, DISKI, DISKO, CNOINR, CNOINI,
     *   CNOUTR, CNOUTI, IDIR, CATRE(256), CATIM(256), NINOUT, NWRK,
     *   ICENX, ICENY, NZ, ONEPLN
      LOGICAL   DOCMPX
      HOLLERITH XNAMER(3), XCLASR(2), XNAMEI(3), XCLASI(2), XNAMOU(3),
     *   XOPCOD
      CHARACTER NAMER*12, CLASSR*6, NAMEI*12, CLASSI*6, NAMOUT*12,
     *   OPCODE*4
      REAL      XSR, XSI, XSO, XDR, XDI, XDO, BADD(10), XCEN, YCEN,
     *   BUFF1(MABFSL), BUFF2(MABFSL), BUFF3(MABFSL), BMUL(2*MAXIMG+20)
      CHARACTER WK1FIL*48, WK2FIL*48
      COMMON /FFTCOM/ CATRE, CATIM, XCEN, YCEN, DOCMPX, NX, NY, LUNMPR,
     *   LUNMPI, LUNWK1, WK1VOL, WK2VOL, JBUFSZ, ICENX, ICENY, CNOINR,
     *   CNOINI, CNOUTR, CNOUTI, NINOUT, NWRK, IDIR, NZ, ONEPLN
      COMMON /FFTCHR/ WK1FIL, WK2FIL, NAMER, CLASSR, NAMEI, CLASSI,
     *   NAMOUT, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, BMUL
      COMMON /INPUTS/ XNAMER, XCLASR, XSR, XDR, XNAMEI, XCLASI, XSI,
     *   XDI, XNAMOU, XSO, XDO, XOPCOD, BADD, SEQINR, SEQINI, SEQOUT,
     *   DISKR, DISKI, DISKO
C                                                          End FFT
LOCAL END
      PROGRAM FFT
C-----------------------------------------------------------------------
C! Fourier transforms an image
C# Map AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2006, 2008, 2015
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
C   FFT fourier transforms a complex image and returns a complex image.
C   The imaginary part of the input may be set to zero using OPCODE
C   but if both parts are given they are in different files.  The
C   output is in two separate files whose CLASS indicates whether
C   they are the real or imaginary part and whether they are in the
C   UV or map plane.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMER        Name of input image.
C      INCLASS        CLASSR        Class of real part of input image.
C      INSEQ          SEQINR        Seq. of real part of input image.
C      INDISK         DISKR         Disk number of real part.
C      IN2NAME        NAMEI         Name of imag. part of input image.
C      IN2CLASS       CLASSI        Class of imag. part of input image.
C      IN2SEQ         SEQINI        Seq. no. of imag. part.
C      IN2DISK        DISKI         Vol. no. of imag. part.
C      OUTNAME        NAMOUT        Name of the output image.
C                                   Default is INNAME
C                                   CLASS='UVREAL','UVIMAG' or
C                                   'MAREAL','MAIMAG'
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      OPCODE         IDIR,DOCMPX   OPCODE:
C                                   'UVRE'  Real uv => CMPLX map
C                                   'UVCX'  CMPLX uv => CMPLX map
C                                   'MARE'  Real map => CMPLX uv
C                                   'MACX'  CMPLX map => CMPLX uv.
C                                   Default = 'MARE'
C      BADDISK(10)    IBAD(10)      Disks to avoid for scratch files
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, CATBLK(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FFT.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /MAPHDR/ CATBLK
      DATA PRGM /'FFT   '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FFTINI (PRGM, IRET)
C                                       Float input file(s).
      IF (IRET.EQ.0) CALL FILES (IRET)
C                                       Transform image.
      IF (IRET.EQ.0) CALL DOFFT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write history
      CALL FFTHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FFTINI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FFTINI gets input parameters for FFT and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPUTS/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in FFT for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, CLASS*6, MTYPE*2, OPS(4)*4, CLASR(2)*6,
     *   CLASI(2)*6, UU*4, VV*4, XX*4, YY*4, JY*8, JYPBM*8, CHTM12*12
      HOLLERITH CATH(256), CATREH(256), CATIMH(256)
      INTEGER   JERR
      INTEGER   CATBLK(256), JDIR(4), II, NPARM, IDEPTH(5), I, J,
     *   IROUND, IERR
      LOGICAL   T, EQUAL, DCPX(4), NOSWAP
      REAL      CATR(256), CATRER(256), CATIMR(256), XREF1, XREF2, TEMP
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'FFT.INC'
      INCLUDE 'INCS:DLOC.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATR, CATD, CATH, CATBLK)
      EQUIVALENCE (CATRE, CATRER, CATREH), (CATIM, CATIMR, CATIMH)
      DATA JDIR /-1, -1, 1, 1/
      DATA OPS /'UVRE','UVCX','MARE','MACX'/
      DATA CLASR /'MAREAL', 'UVREAL'/
      DATA CLASI /'MAIMAG', 'UVIMAG'/
      DATA DCPX /.FALSE.,.TRUE.,.FALSE.,.TRUE./
      DATA UU, VV, XX, YY /'UU--','VV--','XX--','YY--'/
      DATA JY, JYPBM /'JY      ', 'JY/BEAM '/
      DATA IDEPTH /5 * 1/
      DATA T, NOSWAP /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE., BUFF1)
      CALL VHDRIN
      JBUFSZ = 2  * MABFSL
      LUNMPR = 16
      LUNMPI = 17
      LUNWK1 = 18
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 30
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMER, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = T
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQINR = IROUND (XSR)
      SEQOUT = IROUND (XSO)
      SEQINI = IROUND (XSI)
      DISKR = IROUND (XDR)
      DISKI = IROUND (XDI)
      DISKO = IROUND (XDO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMER, NAMER)
      CALL H2CHR (6, 1, XCLASR, CLASSR)
      CALL H2CHR (12, 1, XNAMEI, NAMEI)
      CALL H2CHR (6, 1, XCLASI, CLASSI)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Determine transform type.
      II = -1
      IF (OPCODE.EQ.'    ') II = 3
      DO 20 I = 1,4
         IF (OPCODE(1:4).EQ.OPS(I)(1:4)) II = I
 20      CONTINUE
C                                        Check OPCODE
      IF (II.LE.0) THEN
         WRITE (MSGTXT,1020) OPCODE
         GO TO 990
         END IF
      IDIR = JDIR(II)
      DOCMPX = DCPX(II)
      OPCODE = OPS(II)
      DO 35 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 35      CONTINUE
C                                       Get CATBLK from old file.
      CNOINR = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKR, CNOINR, NAMER, CLASSR, SEQINR, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMER, CLASSR, SEQINR, DISKR,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKR, CNOINR, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKR
      FCNO(NCFILE) = CNOINR
      FRW(NCFILE) = 0
C                                       Blanked image - barf
      IF (ABS (CATR(KRBLK)).GT.0.001) THEN
         IERR = 6
         WRITE (MSGTXT,1046)
         GO TO 990
         END IF
C                                       Save coor. ref pixels.
      XREF1 = CATR(KRCRP)
      XREF2 = CATR(KRCRP+1)
C                                       Find Imag. file.
      CNOINI = 1
      MTYPE = 'MA'
      IF (DOCMPX) THEN
         CALL CATDIR ('SRCH', DISKI, CNOINI, NAMEI, CLASSI, SEQINI,
     *      MTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, NAMEI, CLASSI, SEQINI, DISKI,
     *         NLUSER
            JERR = 1
            GO TO 990
            END IF
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMER, CLASSR, SEQINR, '      ', NAMOUT, CLASS,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CATBLK(KIIMS) = SEQOUT
C                                       Redo headers
      LOCNUM = 1
      CALL SETLOC (IDEPTH, NOSWAP)
C                                       MAP to UV
      IF (IDIR.NE.1) GO TO 70
         IF (CATBLK(KIDIM).GT.KICTPN-2) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (6)
            END IF
         IF (AXTYP(LOCNUM).NE.1) THEN
            WRITE (MSGTXT,1051)
            CALL MSGWRT (6)
            END IF
C                                       Save old X,Y axis parms
         J = 0
 55      I = CATBLK(KIDIM)
         IF (I.LE.KICTPN-1) THEN
            CATBLK(KIDIM) = CATBLK(KIDIM) + 1
            CATD(KDCRV+I) = CATD(KDCRV+J)
            CATR(KRCRP+I) = CATR(KRCRP+J)
            CATR(KRCRT+I) = CATR(KRCRT+J)
            CATR(KRCIC+I) = CATR(KRCIC+J)
            CATBLK(KINAX+I) = 1
            CALL CHCOPY (8, 1, CATH(KHCTP+J*2), 1, CATH(KHCTP+I*2))
            J = J + 1
            IF (J.LE.1) GO TO 55
            END IF
C                                       Make new X,Y transposed
         CALL H2CHR (4, 1, CATH(KHBUN), CHTM12)
         CALL CHLTOU (8, CHTM12)
         EQUAL = JYPBM(1:8) .EQ. CHTM12(1:8)
         IF (EQUAL) CALL CHR2H (8, JY, 1, CATH(KHBUN))
         CALL H2CHR (8, 1, CATH(KHCTP), CHTM12(1:8))
         CALL CHCOPY (8, 1, CATH(KHCTP+2), 1, CATH(KHCTP))
         CALL CHR2H (8, CHTM12(1:8), 1, CATH(KHCTP+2))
         CALL CHR2H (1, UU, 1, CATH(KHCTP+2))
         CALL CHR2H (1, VV, 1, CATH(KHCTP))
         IF ((AXTYP(LOCNUM).GT.0) .AND. (AXTYP(LOCNUM).LT.4)) THEN
            IF (KLOCL(LOCNUM).LE.1) CALL CHR2H (4, UU, 1,
     *         CATH(KHCTP+(1-KLOCL(LOCNUM))*2))
            IF (KLOCM(LOCNUM).LE.1) CALL CHR2H (4, VV, 1,
     *         CATH(KHCTP+(1-KLOCM(LOCNUM))*2))
            IF ((KLOCL(LOCNUM).LE.1) .AND. (KLOCM(LOCNUM).GT.1)) CALL
     *         CHR2H (1, VV(1:1), 1, CATH(KHCTP+KLOCL(LOCNUM)*2))
            IF ((KLOCM(LOCNUM).LE.1) .AND. (KLOCL(LOCNUM).GT.1)) CALL
     *         CHR2H (1, UU(1:1), 1, CATH(KHCTP+KLOCM(LOCNUM)*2))
            END IF
 65      CATD(KDCRV) = 0.0D0
         CATD(KDCRV+1) = 0.0D0
         CATR(KRCRP) = (CATBLK(KINAX+1) / 2.0) + 1.0
         CATR(KRCRP+1) = (CATBLK(KINAX) / 2.0) + 1.0
         TEMP = CATR(KRCIC)
         IF (CATR(KRCIC+1).NE.0.0) CATR(KRCIC) = 1.0 / (CATBLK(KINAX+1)
     *      * CATR(KRCIC+1))
         IF (CATR(KRCIC+1).EQ.0.0) CATR(KRCIC) = 0.0
         IF (TEMP.NE.0.0) CATR(KRCIC+1) = 1.0 / (CATBLK(KINAX) * TEMP)
         IF (TEMP.EQ.0.0) CATR(KRCIC+1) = 0.0
         I = CATBLK(KINAX)
         CATBLK(KINAX) = CATBLK(KINAX+1)
         CATBLK(KINAX+1) = I
         CATR(KRCRT) = 0.0
         CATR(KRCRT+1) = 0.0
         IF (IDIR.NE.1) GO TO 80
            IF (KLOCM(LOCNUM).LE.1) CATR(KRCIC+1-KLOCM(LOCNUM)) =
     *         CATR(KRCIC+1-KLOCM(LOCNUM)) / COND2R
            IF (KLOCL(LOCNUM).LE.1) CATR(KRCIC+1-KLOCL(LOCNUM)) =
     *         CATR(KRCIC+1-KLOCL(LOCNUM)) / COND2R
            GO TO 80
C                                       UV to MAP
 70      CALL H2CHR (4, 1, CATH(KHBUN), CHTM12)
         EQUAL = 'JY  ' .EQ. CHTM12(1:4)
         IF (EQUAL) CALL CHR2H (8, JYPBM, 1, CATH(KHBUN))
         J = CATBLK(KIDIM)
         CALL CHCOMP (4, 5, CATH(KHCTP+(J-1)*2), 5, CATH(KHCTP+2),
     *      EQUAL)
         IF (EQUAL) CALL CHCOMP (4, 5, CATH(KHCTP+(J-2)*2), 5,
     *      CATH(KHCTP+2), EQUAL)
         IF ((J.GE.4) .AND. (EQUAL) .AND. (CATBLK(KINAX+J-1).EQ.1)
     *      .AND. (CATBLK(KINAX+J-2).EQ.1)) GO TO 75
            WRITE (MSGTXT,1070)
            CALL MSGWRT (6)
            CALL H2CHR (8, 1, CATH(KHCTP), CHTM12(1:1+8-1))
            CALL CHCOPY (8, 1, CATH(KHCTP+2), 1, CATH(KHCTP))
            CALL CHR2H (8, CHTM12(1:8), 1, CATH(KHCTP+2))
            CALL CHR2H (1, YY, 1, CATH(KHCTP+2))
            CALL CHR2H (1, XX, 1, CATH(KHCTP))
            GO TO 65
C                                       Get from previous FFT
 75      CONTINUE
            CATBLK(KIDIM) = CATBLK(KIDIM) - 2
            I = CATBLK(KIDIM)
            CATD(KDCRV) = CATD(KDCRV+I)
            CATD(KDCRV+1) = CATD(KDCRV+I+1)
            CATR(KRCRP) = CATR(KRCRP+I)
            CATR(KRCRP+1) = CATR(KRCRP+I+1)
            CATR(KRCRT) = CATR(KRCRT+I)
            CATR(KRCRT+1) = CATR(KRCRT+I+1)
            CATR(KRCIC) = CATR(KRCIC+I)
            CATR(KRCIC+1) = CATR(KRCIC+I+1)
            CALL CHCOPY (8, 1, CATH(KHCTP+I*2), 1, CATH(KHCTP))
            CALL CHCOPY (8, 1, CATH(KHCTP+(I+1)*2), 1, CATH(KHCTP+2))
            I = CATBLK(KINAX)
            CATBLK(KINAX) = CATBLK(KINAX+1)
            CATBLK(KINAX+1) = I
C                                       Copy CATBLKs.
 80   CALL COPY (256, CATBLK, CATRE)
      CALL COPY (256, CATBLK, CATIM)
C                                       Put class in CATBLKs.
      IF (IDIR.EQ.-1) THEN
         CALL CHR2H (6, CLASR(1), KHIMCO, CATREH(KHIMC))
         CALL CHR2H (6, CLASI(1), KHIMCO, CATIMH(KHIMC))
      ELSE
         CALL CHR2H (6, CLASR(2), KHIMCO, CATREH(KHIMC))
         CALL CHR2H (6, CLASI(2), KHIMCO, CATIMH(KHIMC))
         END IF
C                                       Copy real header to CATBLK.
      CALL COPY (256, CATRE, CATBLK)
C                                       Create real output file.
      CNOUTR = 1
      JERR = 4
      CALL MCREAT (DISKO, CNOUTR, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOUTR
      FRW(NCFILE) = 2
      SEQOUT = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, CATRE)
C                                       copy keywords
      CALL KEYPCP (DISKR, CNOINR, DISKO, CNOUTR, 0, ' ', IERR)
C                                       Create Imag. output file.
      CALL COPY (256, CATIM, CATBLK)
      CNOUTI = 1
      CALL MCREAT (DISKO, CNOUTI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOUTI
      FRW(NCFILE) = 2
      CALL COPY (256, CATBLK, CATIM)
C                                       copy keywords
      CALL KEYPCP (DISKR, CNOINR, DISKO, CNOUTI, 0, ' ', IERR)
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FFTINI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('UNKNOWN OPCODE =',A4)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1046 FORMAT ('I CANNOT COPE WITH BLANKED IMAGES - USE ZERO BLANKING')
 1050 FORMAT ('WARNING: CAN''T SAVE OLD X-Y AXES IN HEADER')
 1051 FORMAT ('WARNING: NON-ANGLE X OR Y AXIS - OUTPUT UNITS',
     *   ' UNCERTAIN')
 1070 FORMAT ('WARNING: CAN''T RECOVER PREVIOUS X-Y AXES - OUTPUT',
     *   ' UNCERTAIN')
 1080 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE FILES (IERR)
C-----------------------------------------------------------------------
C  FILES reads and floats the input image and if an imaginary image
C  is to be used it is floated.  Both parts are rotated to the center
C  at the edges convention for the FFT.
C   Output:
C   IERR        I    Return error code. 0 => OK, error otherwise.
C-----------------------------------------------------------------------
      INTEGER    CATBLK(256), IERR, CAT2(256), MX, MY, NAX, NP(3),
     *   ISIZE, MZ, DEP(5), IX, IY
      DOUBLE PRECISION CATD(128), CAT2D(128)
      REAL      CATR(256), CAT2R(256), CATRER(256), XSPA, YSPA
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FFT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD), (CAT2, CAT2R, CAT2D, BUFF2)
      EQUIVALENCE (CATRE, CATRER)
      DATA DEP /2,1,1,1,1/
C-----------------------------------------------------------------------
      MX = 0
      MY = 0
      MZ = 1
C                                      Check if imag. image to
C                                      be used.
      IF (DOCMPX) THEN
C                                       Read imag. header.
C                                       Copy CATBLK and mark READ.
         CALL CATIO ('READ', DISKI, CNOINI, CAT2, 'READ', BUFF3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI
         FCNO(NCFILE) = CNOINI
         FRW(NCFILE) = 0
C                                       Determine imag. NX and NY
         MX = CAT2(KINAX)
         MY = CAT2(KINAX+1)
         IF (CAT2(KIDIM).GE.3) MZ = MZ * MAX (1, CAT2(KINAX+2))
         IF (CAT2(KIDIM).GE.4) MZ = MZ * MAX (1, CAT2(KINAX+3))
         IF (CAT2(KIDIM).GE.5) MZ = MZ * MAX (1, CAT2(KINAX+4))
         IF (CAT2(KIDIM).GE.6) MZ = MZ * MAX (1, CAT2(KINAX+5))
         IF (CAT2(KIDIM).GE.7) MZ = MZ * MAX (1, CAT2(KINAX+6))
         XSPA = CAT2R(KRCIC)
         YSPA = CAT2R(KRCIC+1)
C                                       Blanked image - barf
         IF (ABS (CAT2R(KRBLK)).GT.0.001) THEN
            IERR = 6
            WRITE (MSGTXT,1018)
            GO TO 990
            END IF
         END IF
C                                       Recover REAL CATBLK
      CALL CATIO ('READ', DISKR, CNOINR, CATBLK, 'REST', BUFF3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Determine map rotation.
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
      NZ = 1
      IF (CATBLK(KIDIM).GE.3) NZ = NZ * MAX (1, CATBLK(KINAX+2))
      IF (CATBLK(KIDIM).GE.4) NZ = NZ * MAX (1, CATBLK(KINAX+3))
      IF (CATBLK(KIDIM).GE.5) NZ = NZ * MAX (1, CATBLK(KINAX+4))
      IF (CATBLK(KIDIM).GE.6) NZ = NZ * MAX (1, CATBLK(KINAX+5))
      IF (CATBLK(KIDIM).GE.7) NZ = NZ * MAX (1, CATBLK(KINAX+6))
      XCEN = CATR(KRCRP) - CATRER(KRCRP+1)
      YCEN = CATR(KRCRP+1) - CATRER(KRCRP)
      ICENX = 0
      ICENY = 0
C                                       Make sure NX,NY same as
C                                       for Imag. part.
      IF (((NX.NE.MX) .OR. (NY.NE.MY) .OR. (CATR(KRCIC).NE.XSPA) .OR.
     *   (CATR(KRCIC+1).NE.YSPA) .OR. (NZ.NE.MZ)) .AND.
     *   (DOCMPX)) THEN
         IERR = 1
         WRITE (MSGTXT,1030)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1031) NX, NY, NZ, MX, MY, MZ
         GO TO 990
         END IF
C                                       Check NX, NY
      CALL POWER2 (NX, IX)
      CALL POWER2 (NY, IY)
      IF ((NX.NE.IX) .OR. (NX.LT.32) .OR. (NX.GT.MAXIMG) .OR. (NY.NE.IY)
     *   .OR. (NY.LT.32) .OR. (NY.GT.MAXIMG)) THEN
         WRITE (MSGTXT,1035) NX, NY
         IERR = 1
         GO TO 990
         END IF
      NAX = 3
      NP(1) = NX
      NP(2) = NY
      NP(3) = NZ
      IF (NZ.GT.1) THEN
         CALL COMOFF (NAX, NP, DEP, ONEPLN, IERR)
      ELSE
         ONEPLN = 0
         END IF
C                                       Create scratch files.
C                                       Work file 1
      NAX = 2
      NP(1) = NX * 2
      NP(2) = NY
      CALL MAPSIZ (NAX, NP, ISIZE)
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      WK1VOL = SCRVOL(NSCR)
      CALL ZPHFIL ('SC', WK1VOL, SCRCNO(NSCR), 1, WK1FIL, IERR)
      NINOUT = NSCR
      LUNS(NINOUT) = LUNMPR
C                                       Work file No. 2.
      CALL SCREAT (ISIZE, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Update from COMMON /FILES/
      WK2VOL = SCRVOL(NSCR)
      CALL ZPHFIL ('SC', WK2VOL, SCRCNO(NSCR), 1, WK2FIL, IERR)
      NWRK = NSCR
      LUNS(NWRK) = LUNMPI
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FILES: CANNOT READ IMAG. CATBLK, ERROR',I3)
 1018 FORMAT ('I CANNOT COPE WITH BLANKED IMAGES - USE ZERO BLANKING')
 1020 FORMAT ('FILES: CANNOT REREAD REAL CATBLK, ERROR',I3)
 1030 FORMAT ('FILES: UNEQUAL DIMENSIONS IN TWO MAPS')
 1031 FORMAT ('       REAL=',3I5,' IMAG=',3I5)
 1035 FORMAT ('ILLEGAL NX, NY=',2I6)
 1060 FORMAT ('FILES: ERROR',I3,' CREATING WORK SCRATCH FILE')
      END
      SUBROUTINE DOFFT (IRET)
C-----------------------------------------------------------------------
C   DOFFT merges two input maps, does the FFT and then splits output
C   into real and imaginary parts.  Inputs assumed in input catalogd
C   files and the output will be to catalogd output files.
C   Output:
C   IRET   I    Return code, 0 => OK, otherwise error.
C   Max. and min. are also put in the map header.
C-----------------------------------------------------------------------
      INTEGER   IRET, KAP, IZ, BO, NEED
      LOGICAL   F
      REAL      DUM1, DUM2
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FFT.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Merge files.
      DO 100 IZ = 1,NZ
         BO = 1 + ONEPLN * (IZ - 1)
         CALL FMERG (BO, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Do FFT.
C                                       should be enough
         NEED = 4 * NX * NY + 4 * (NX + NY)
         NEED = NEED / 1024
         MSGSUP = 32000
         CALL QINIT (NEED, 0, KAP)
         MSGSUP = 0
         IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
            NEED = 2 * NX * NY + 2 * (NX + NY)
            NEED = NEED / 1024
            NEED = MIN (32 * 1024, NEED)
            MSGSUP = 32000
            CALL QINIT (NEED, 0, KAP)
            MSGSUP = 0
            IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
               NEED = 5120
               CALL QINIT (NEED, 0, KAP)
               IF ((KAP.EQ.0) .OR. (PSAPNW.EQ.0)) THEN
                  MSGTXT = 'DOFFT: FAILED TO GET ANY AP MEMORY'
                  CALL MSGWRT (8)
                  IRET = 10
                  GO TO 999
                  END IF
               END IF
            END IF
         CALL DSKFFT (NY, NX, IDIR, F, NINOUT, NWRK, NINOUT, JBUFSZ,
     *      BUFF1, BUFF2, DUM1, DUM2, IRET)
         CALL QRLSE
         IF (IRET.NE.0) GO TO 999
C                                       Split and rotate output.
         CALL SPLIT (BO, IRET)
         IF (IRET.NE.0) GO TO 999
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE FMERG (BI, IRET)
C-----------------------------------------------------------------------
C   FMERG merges real and imaginary files MPR and MPI and converts
C   them into a complex file in WK1.
C   Output:
C   IRET   I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'FFT.INC'
C
      CHARACTER NAM(3)*4, FNAME*48
      INTEGER   BI, FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO,
     *   WIN(4), MX, MY, JJ, IRET, J, I, NX2, NY2, IH, I1, I2, ILIM
      LOGICAL   T, F, DOMUL
      REAL      TWOPI, TEMP
      COMPLEX   XMUL(MAXIMG+10), YMUL, TMUL, CONJG, CMPLX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (XMUL, BMUL)
      DATA T, F /.TRUE.,.FALSE./
      DATA NAM /'REAL','IMAG','WORK'/
      DATA TWOPI /6.283185308/
C-----------------------------------------------------------------------
      BO = 1
      CALL FILL (4, 0, WIN)
C                                        Open and init REAL
      CALL ZPHFIL ('MA', DISKR, CNOINR, 1, FNAME, IRET)
      CALL ZOPEN (LUNMPR, FIND1, DISKR, FNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ', NAM(1)
         GO TO 990
         END IF
      MX = NX * 2
      MY = NY
      NX2 = NX / 2
      NY2 = NY / 2
      CALL MINIT ('READ', LUNMPR, FIND1, NX, NY, WIN, BUFF1, JBUFSZ, BI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'READ', NAM(1)
         GO TO 990
         END IF
C                                        Zero Imag. buffer.
      CALL RFILL (MX, 0.0, BUFF2)
      BIND2 = 1
C                                        Check if read imag.
      IF (DOCMPX) THEN
C                                        Open and init MPI
         CALL ZPHFIL ('MA', DISKI, CNOINI, 1, FNAME, IRET)
         CALL ZOPEN (LUNMPI, FIND2, DISKI, FNAME, T, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ', NAM(2)
            GO TO 990
            END IF
         CALL MINIT ('READ', LUNMPI, FIND2, NX, NY, WIN, BUFF2, JBUFSZ,
     *      BI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ', NAM(2)
            GO TO 990
            END IF
         END IF
C                                        Phase shift terms
      DOMUL = (IDIR.NE.1) .AND. ((XCEN.NE.0.0) .OR. (YCEN.NE.0.0))
      IF (DOMUL) THEN
         TEMP = TWOPI * XCEN / NX
         ILIM = NX / 2 + 1
C                                       Note: following loop uses
C                                       XMUL(NX+1)
         DO 30 I = 1,ILIM
            XMUL(I) = CMPLX (COS((I-1)*TEMP), SIN((I-1)*TEMP))
            XMUL(NX+2-I) = CONJG (XMUL(I))
 30         CONTINUE
C                                       Oops, one too many
         XMUL(ILIM) = CONJG (XMUL(ILIM))
         TEMP = TWOPI * YCEN / NY
         END IF
C                                        Open and init WK1
      CALL ZOPEN (LUNWK1, FIND3, WK1VOL, WK1FIL, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRIT', NAM(3)
         GO TO 990
         END IF
      WIN(1) = 1
      WIN(3) = MX
      WIN(2) = NY2 + 1
      WIN(4) = NY
C                                        Loop over file half.
      DO 150 IH = 1,2
         CALL MINIT ('WRIT', LUNWK1, FIND3, MX, MY, WIN, BUFF3, JBUFSZ,
     *      BO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRIT', NAM(3)
            GO TO 990
            END IF
C                                        Loop, forming complex.
         DO 140 I = 1, NY2
C                                        Read real
            CALL MDISK ('READ', LUNMPR, FIND1, BUFF1, BIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1060) IRET, 'READ', NAM(1)
               GO TO 990
               END IF
C                                        Read imag if needed
            IF (DOCMPX) THEN
               CALL MDISK ('READ', LUNMPI, FIND2, BUFF2, BIND2, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1060) IRET, 'READ', NAM(2)
                  GO TO 990
                  END IF
               END IF
C                                        Write WK1
            CALL MDISK ('WRIT', LUNWK1, FIND3, BUFF3, BIND3, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1060) IRET, 'WRIT', NAM(3)
               GO TO 990
               END IF
C                                        Merge 1st half row.
            IF (.NOT.DOMUL) THEN
               DO 100 J = 1, NX2
                  JJ = BIND3 + 2 * J - 2 + NX
                  I1 = BIND1 + J - 1
                  I2 = BIND2 + J - 1
                  BUFF3(JJ) = BUFF1(I1)
                  BUFF3(JJ+1) = BUFF2(I2)
 100              CONTINUE
C                                        Do second half.
               DO 110 J = 1, NX2
                  JJ = BIND3 + 2 * J - 2
                  I1 = BIND1 + J - 1 + NX2
                  I2 = BIND2 + J - 1 + NX2
                  BUFF3(JJ) = BUFF1(I1)
                  BUFF3(JJ+1) = BUFF2(I2)
 110              CONTINUE
            ELSE
C                                        With shift
               J = WIN(2) + I - 2
               IF (IH.EQ.1) J = J - NY
               YMUL = CMPLX (COS(J*TEMP), SIN(J*TEMP))
               DO 130 J = 1,NX2
                  JJ = BIND3 + 2 * J - 2 + NX
                  I1 = BIND1 + J - 1
                  I2 = BIND2 + J - 1
                  TMUL = YMUL * XMUL(J+NX2) * CMPLX (BUFF1(I1),
     *               BUFF2(I2))
                  BUFF3(JJ) = REAL(TMUL)
                  BUFF3(JJ+1) = AIMAG(TMUL)
 130              CONTINUE
C                                        Do second half.
               DO 135 J = 1,NX2
                  JJ = BIND3 + 2 * J - 2
                  I1 = BIND1 + J - 1 + NX2
                  I2 = BIND2 + J - 1 + NX2
                  TMUL = YMUL * XMUL(J) * CMPLX (BUFF1(I1),
     *               BUFF2(I2))
                  BUFF3(JJ) = REAL(TMUL)
                  BUFF3(JJ+1) = AIMAG(TMUL)
 135              CONTINUE
               END IF
 140        CONTINUE
C                                        Flush buffer.
         CALL MDISK ('FINI', LUNWK1, FIND3, BUFF3, BIND3, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1060) IRET, 'FINI', NAM(3)
            GO TO 990
            END IF
C                                        Prepare for second half.
         WIN(2) = 1
         WIN(4) = NY2
 150     CONTINUE
C                                        Close files.
      CALL ZCLOSE (LUNMPR, FIND1, IRET)
      IF (DOCMPX) CALL ZCLOSE (LUNMPI, FIND2, IRET)
      CALL ZCLOSE (LUNWK1, FIND3, IRET)
      IRET = 0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FMERG: ERROR',I3,' OPEN FOR ',A4,1X,A4,' FILE')
 1010 FORMAT ('FMERG: ERROR',I3,' INIT FOR ',A4,1X,A4,' FILE')
 1060 FORMAT ('FMERG: ERROR',I3,1X,A4,'ING ',A4,' FILE')
      END
      SUBROUTINE SPLIT (BI, IRET)
C-----------------------------------------------------------------------
C   SPLIT takes a complex rotated (center at corners) image WK1
C   and puts the real and imaginary parts in the correct order in
C   output files. The values of MAX and MIN are placed in the headers.
C   Output:
C   IRET    I     Return error code., 0 =>OK otherwise error.
C-----------------------------------------------------------------------
      INCLUDE 'FFT.INC'
C
      CHARACTER NAM(3)*4, FNAME*48
      INTEGER   BI, FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO,
     *   WIN(4), MX, MY, NX2, NY2, IRET, IERR, J, II, I, I2, I3, IROW,
     *   IH, ILIM
      LOGICAL   T, F, DOMUL
      REAL      RMAX, RMIN, IMAX, IMIN, CATRER(256), CATIMR(256), TEMP,
     *   TWOPI
      COMPLEX   XMUL(MAXIMG+10), YMUL, TMUL, CONJG, CMPLX
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (XMUL,BMUL),  (CATIMR,CATIM), (CATRER,CATRE)
      DATA T, F /.TRUE.,.FALSE./
      DATA NAM /'WORK','REAL','IMAG'/
      DATA TWOPI /6.283185308/
C-----------------------------------------------------------------------
      CALL FILL (4, 0, WIN)
      BO = 1
C                                        Initialize extrema.
      RMAX = -1.0E6
      RMIN = 1.0E6
      IMAX = -1.0E6
      IMIN = 1.0E6
C                                        Open work file.
      CALL ZOPEN (LUNWK1, FIND1, WK1VOL, WK1FIL, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(1)
         GO TO 990
         END IF
C                                        Open real file.
      CALL ZPHFIL ('MA', DISKO, CNOUTR, 1, FNAME, IRET)
      CALL ZOPEN (LUNMPR, FIND2, DISKO, FNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(2)
         GO TO 990
         END IF
C                                        Init real file.
      CALL MINIT ('WRIT', LUNMPR, FIND2, NY, NX, WIN, BUFF2, JBUFSZ, BI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', NAM(2)
         GO TO 990
         END IF
C                                        Open imag. file
      CALL ZPHFIL ('MA', DISKO, CNOUTI, 1, FNAME, IRET)
      CALL ZOPEN (LUNMPI, FIND3, DISKO, FNAME, T, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', NAM(3)
         GO TO 990
         END IF
C                                        Init imag. file.
      CALL MINIT ('WRIT', LUNMPI, FIND3, NY, NX, WIN, BUFF3, JBUFSZ, BI,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT', NAM(3)
         GO TO 990
         END IF
C                                        Phase shift
      DOMUL = (IDIR.EQ.1) .AND. ((XCEN.NE.0.0) .OR. (YCEN.NE.0.))
      IF (DOMUL) THEN
         TEMP = TWOPI * YCEN / NY
         ILIM = NY / 2 + 1
C                                       Note: following loop uses
C                                       XMUL(NY+1)
         DO 60 I = 1,ILIM
            XMUL(I) = CMPLX (COS((I-1)*TEMP), SIN((I-1)*TEMP))
            XMUL(NY+2-I) = CONJG (XMUL(I))
 60         CONTINUE
C                                       Oops, one too many
         XMUL(ILIM) = CONJG (XMUL(ILIM))
         TEMP = TWOPI * XCEN / NX
         END IF
C                                        Prepare to init input.
      MX = NX
      MY = NY * 2
      NX2 = NX / 2
      NY2 = NY / 2
      WIN(1) = 1
      WIN(3) = MY
      WIN(2) = NX2 + 1
      WIN(4) = NX
C                                       Begin loop over two halves.
      DO 250 IH = 1,2
C                                       Inint input file.
         CALL MINIT ('READ', LUNWK1, FIND1, MY, MX, WIN, BUFF1, JBUFSZ,
     *      BO, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT', NAM(1)
            GO TO 990
            END IF
C                                       Loop over rows in this half.
         DO 200 IROW = 1,NX2
C                                       Read row.
            CALL MDISK ('READ', LUNWK1, FIND1, BUFF1, BIND1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ', NAM(1)
               GO TO 990
               END IF
C                                       Write real.
            CALL MDISK ('WRIT', LUNMPR, FIND2, BUFF2, BIND2, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', NAM(2)
               GO TO 990
               END IF
C                                       Write imag.
            CALL MDISK ('WRIT', LUNMPI, FIND3, BUFF3, BIND3, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRIT', NAM(3)
               GO TO 990
               END IF
C                                       Split row - first half.
            IF (.NOT.DOMUL) THEN
               DO 140 I = 1,NY2
                  II = BIND1 + I * 2 - 2 + NY
                  I2 = BIND2 + I - 1
                  I3 = BIND3 + I - 1
                  BUFF2(I2) = BUFF1(II)
                  BUFF3(I3) = BUFF1(II+1)
C                                         Find extrema.
                  RMAX = MAX (RMAX, BUFF2(I2))
                  RMIN = MIN (RMIN, BUFF2(I2))
                  IMAX = MAX (IMAX, BUFF3(I3))
                  IMIN = MIN (IMIN, BUFF3(I3))
 140              CONTINUE
C                                         Second half.
               DO 150 I = 1,NY2
                  II = BIND1 + I * 2 - 2
                  I2 = BIND2 + I - 1 + NY2
                  I3 = BIND3 + I - 1 + NY2
                  BUFF2(I2) = BUFF1(II)
                  BUFF3(I3) = BUFF1(II+1)
C                                         Find extrema.
                  RMAX = MAX (RMAX, BUFF2(I2))
                  RMIN = MIN (RMIN, BUFF2(I2))
                  IMAX = MAX (IMAX, BUFF3(I3))
                  IMIN = MIN (IMIN, BUFF3(I3))
 150              CONTINUE
            ELSE
C                                        With shift
               J = WIN(2) + IROW - 2
               IF (IH.EQ.1) J = J - NX
               YMUL = CMPLX (COS(J*TEMP), SIN(J*TEMP))
               DO 170 I = 1,NY2
                  II = BIND1 + I * 2 - 2 + NY
                  I2 = BIND2 + I - 1
                  I3 = BIND3 + I - 1
                  TMUL = YMUL * XMUL(I+NY2) * CMPLX (BUFF1(II),
     *               BUFF1(II+1))
                  BUFF2(I2) = REAL(TMUL)
                  BUFF3(I3) = AIMAG(TMUL)
C                                         Find extrema.
                  RMAX = MAX (RMAX, BUFF2(I2))
                  RMIN = MIN (RMIN, BUFF2(I2))
                  IMAX = MAX (IMAX, BUFF3(I3))
                  IMIN = MIN (IMIN, BUFF3(I3))
 170              CONTINUE
C                                         Second half.
               DO 180 I = 1,NY2
                  II = BIND1 + I * 2 - 2
                  I2 = BIND2 + I - 1 + NY2
                  I3 = BIND3 + I - 1 + NY2
                  TMUL = YMUL * XMUL(I) * CMPLX (BUFF1(II),
     *               BUFF1(II+1))
                  BUFF2(I2) = REAL(TMUL)
                  BUFF3(I3) = AIMAG(TMUL)
C                                         Find extrema.
                  RMAX = MAX (RMAX, BUFF2(I2))
                  RMIN = MIN (RMIN, BUFF2(I2))
                  IMAX = MAX (IMAX, BUFF3(I3))
                  IMIN = MIN (IMIN, BUFF3(I3))
 180              CONTINUE
               END IF
 200        CONTINUE
C                                         Prepare for 2nd half plane.
         WIN(2) = 1
         WIN(4) = NX2
 250     CONTINUE
C                                         Flush buffers.
      CALL MDISK ('FINI', LUNMPR, FIND2, BUFF2, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINI', NAM(2)
         GO TO 990
         END IF
      CALL MDISK ('FINI', LUNMPI, FIND3, BUFF3, BIND3, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINI', NAM(3)
         GO TO 990
         END IF
C                                         Close files.
      CALL ZCLOSE (LUNWK1, FIND1, IERR)
      CALL ZCLOSE (LUNMPR, FIND2, IERR)
      CALL ZCLOSE (LUNMPI, FIND3, IERR)
      IRET = 0
C                                         Fill extrema in headers.
      CATRER(KRDMX) = RMAX
      CATRER(KRDMN) = RMIN
      CATIMR(KRDMX) = IMAX
      CATIMR(KRDMN) = IMIN
      GO TO 999
C                                        Error.
 990  CALL MSGWRT(8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPLIT: ERROR',I3,1X,A4,'ING ',A4,' FILE')
      END
      SUBROUTINE FFTHIS
C-----------------------------------------------------------------------
C   FFTHIS copies and updates history file.
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), LUN1, LUN2, IERR, IPTR, ITEMP, IER
      CHARACTER HILINE*72
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'FFT.INC'
      COMMON /MAPHDR/ CATBLK
      DATA LUN1, LUN2 /27,28/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKR, DISKO, CNOINR, CNOUTR, CATRE,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       Copy second input history.
      IF (DOCMPX) THEN
         WRITE (HILINE,1010) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         CALL HIOPEN (LUN1, DISKI, CNOINI, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            CALL MSGWRT (6)
            GO TO 10
            END IF
         CALL HILOCT ('SRCH', LUN2, IPTR, IERR)
         ITEMP = HITAB(IPTR+2)
         CALL HICOPY (LUN1, BUFF1, LUN2, BUFF2, IERR)
         CALL HICLOS (LUN1, F, BUFF1, IER)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1011) IERR
            CALL MSGWRT (6)
            IF (IERR.LT.100) GO TO 20
            HITAB(IPTR+2) = ITEMP
            END IF
         END IF
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMER, CLASSR, SEQINR, DISKR, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF (DOCMPX) CALL HENCO2 (TSKNAM, NAMEI, CLASSI, SEQINI, DISKI,
     *   LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, '      ', SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Add OPCODE
      WRITE (HILINE,2000) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOUTR, CATRE, 'REST', BUFF1,
     *   IERR)
C                                        Copy history to imag. file.
      CALL HISCOP (LUN1, LUN2, DISKO, DISKO, CNOUTR, CNOUTI, CATIM,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         END IF
C                                        Close history file.
      CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Update imag CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOUTI, CATIM, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FFTHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1005 FORMAT ('FFTHIS UNABLE TO OPEN IMAG. HISTORY FILE')
 1010 FORMAT (A6,'/************** Imaginary image history')
 1011 FORMAT ('FFTHIS: ERROR',I3,' COPYING IMAG. HISTORY')
 2000 FORMAT (A6,' OPCODE =',A4,' /Requested operation')
      END
