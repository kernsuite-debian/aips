LOCAL INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Local include for CONVL
      INTEGER   NX, NY, LUNMP1, LUNMP2, LUNWRK, MP1VOL, MP2VOL, WRKVOL,
     *   BUFSZ1, BUFSZ2, BUFSZ3, SEQIN, SEQ2, SEQOUT, DISKIN, DISK2,
     *   DISKO, CNOIN, CNOIN2, CNOOUT, CATOLD(256), NFFTX, NFFTY,
     *   MP1SCR, MP2SCR, WRKSCR, START3, START4, START5, START6, START7,
     *   END3, END4, END5, END6, END7, WININ(4), WINOUT(4), BOX(2), NMX,
     *   NMY, LXMAX, LYMAX
      LOGICAL   OLD, PIXEL, NOTSEC, LGAUS, LDGAU, LIMAG, LIMAC, LDCON,
     *   WBLANK, DOMSG, USECG, LGMOD
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2), XOPCOD,
     *   XNAMOU(3), XCLAOU(2), CATOH(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, OPCODE*4,
     *   NAMOUT*12, CLAOUT*6
      REAL      BMAJ, BMIN, BPA, MAPROT, XSPACE, YSPACE, DOBLNK,
     *   XSI, XS2, XSO, XDI, XD2, XDO, BADD(10), CVBMAJ, CVBMIN, CVBPA,
     *   FACTOR, TRC(7), BLC(7), FACT2, COCNV, OVFACT, BUFF1(MABFSL),
     *   BUFF2(MABFSL), BUFF3(MABFSL), WASFCT, IMSIZE(2), CVBMAP,
     *   CVBMIP, CVBPAP, CVBHI(3,MAXIMG), CATOR(256)
      CHARACTER MP1FIL*48, MP2FIL*48, WRKFIL*48
      DOUBLE PRECISION CATOD(128)
      EQUIVALENCE (CATOLD, CATOR, CATOH, CATOD)
      COMMON /CVLCOM/ CATOLD, MAPROT, XSPACE, YSPACE, COCNV, OVFACT,
     *   CVBMAJ, CVBMIN, CVBPA, FACT2, OLD, PIXEL, NOTSEC, LGAUS, LDGAU,
     *   LIMAG, LIMAC, LDCON, NX, NY, LUNMP1, LUNMP2, LUNWRK, MP1VOL,
     *   MP2VOL, WRKVOL, WININ, WINOUT, NFFTX, NFFTY, MP1SCR, MP2SCR,
     *   WRKSCR, START3, START4, START5, START6, START7, NMX, NMY,
     *   LXMAX, LYMAX, END3, END4, END5, END6, END7, WASFCT, WBLANK,
     *   DOMSG, SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO, CNOIN,
     *   CNOIN2, CNOOUT, BOX, CVBMAP, CVBMIP, CVBPAP, USECG, CVBHI,
     *   LGMOD
      COMMON /CVLCHR/ MP1FIL, MP2FIL, WRKFIL,
     *   NAMEIN, CLAIN, NAME2, CLAS2, OPCODE, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, BUFSZ1, BUFSZ2, BUFSZ3
      COMMON /INPUTS/ XNAMEI, XCLAIN, XSI, XDI, XOPCOD, BLC, TRC,
     *   XNAME2, XCLAS2, XS2, XD2, XNAMOU, XCLAOU, XSO, XDO, IMSIZE,
     *   BMAJ, BMIN, BPA, FACTOR, DOBLNK, BADD
C                                                          End CONVL
LOCAL END
      PROGRAM CONVL
C-----------------------------------------------------------------------
C! Convolves an image with a Gaussian or another image
C# Map AP-fft Spectral
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2003-2013, 2015-2016, 2018
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
C   CONVL convolves the input image with either an eliptical gaussian
C   or another image.  In the case of convolution with a gaussian
C   the units are the same as the input map.  In the case of
C   convolution with an input image the output units are uncertain.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OPCODE         OPCODE        OPCODE desired GAUS, DGAU, IMAG
C      BLC            BLC           Bottom left corner of subimage
C      TRC            TRC           Top right corner of subimage
C      IN2NAME        NAME2         Name of convolving image.
C      IN2CLASS       CLAS2         Class of convolving image.
C      IN2SEQ         SEQ2          Seq. no. of convolving image.
C      IN2DISK        DISK2         Vol. no. of convolving image.
C      OUTNAME        NAMOUT        Name of the output image.
C      OUTCLASS       CLAOUT        Class of the output image.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BMAJ           BMAJ          Major axis of output gaussian (sec)
C                                   If .le. 0.0 use second input image.
C      BMIN           BMIN          Minor axis of output gaussian.
C                                   Default = BMAJ
C      BPA            BPA           Position angle (deg) of gaussian.
C      FACTOR         FACTOR        The conversion factor for the map
C                             units. If 0.0 then it will be computed
C                             if the input map has a clean beam and
C                             it is being convolved with a gaussian.
C      BADDISK(10)    IBAD(10)      Disks to avoid for scratch files
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   ICOUNT, IRET, IXOFF, IYOFF, ICORN(7), OCORN(7), LOOP3,
     *   LOOP4, LOOP5, LOOP6, LOOP7, NNX, NNY, PCORN(5), MAXS, NXX, NYY
      REAL      X0, Y0, MAXP
      DOUBLE PRECISION DFACT
      LOGICAL   BLANKD
C
      INTEGER   MAXAR
      PARAMETER (MAXAR = 33*33)
      REAL   FUNC(MAXAR), XARG(MAXAR), YARG(MAXAR)
C
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (PCORN, ICORN(3))
      DATA PRGM /'CONVL '/, IXOFF, IYOFF /0, 0/
C-----------------------------------------------------------------------
C                                       Release AP memory
      CALL QRLSE
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CONINI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      WBLANK = .FALSE.
C                                       Prepare scratch file and
C                                       convolving image.
      CALL CONFIL (IRET)
      IF (IRET.NE.0) GO TO 990
      NNX = NFFTX / 2
      NNY = NFFTY / 2
C                                        Loop over planes parallel
C                                        to first.
      CATR(KRDMX) = -1.E10
      CATR(KRDMN) = 1.E10
      ICOUNT = 0
      DO 700 LOOP7 = START7,END7
         DO 600 LOOP6 = START6,END6
            DO 500 LOOP5 = START5,END5
               DO 400 LOOP4 = START4,END4
                  DO 300 LOOP3 = START3,END3
C                                        Set corner.
                     ICORN(1) = BLC(1)
                     ICORN(2) = BLC(2)
                     ICORN(3) = LOOP3
                     ICORN(4) = LOOP4
                     ICORN(5) = LOOP5
                     ICORN(6) = LOOP6
                     ICORN(7) = LOOP7
                     ICOUNT = ICOUNT + 1
                     WRITE (MSGTXT,1000) PCORN
                     IF (DOMSG) CALL MSGWRT (2)
C                                       Get plane.
                     CALL PLNGBL (DISKIN, CNOIN, ICORN, WININ, IXOFF,
     *                  IYOFF, 1, NNX, NNY, BUFF1, BUFF2, BUFSZ1,
     *                  BUFSZ2, LUNMP1, LUNWRK, BLANKD, IRET)
                     IF (IRET.NE.0) GO TO 990
                     IF ((BLANKD) .AND. (.NOT.WBLANK)) THEN
                        WBLANK = .TRUE.
                        MSGTXT = 'WARNING: BLANKS REPLACED BY 0 FOR' //
     *                     ' CONVOLUTION'
                        CALL MSGWRT (7)
                        END IF
C                                      Convolve.
                     MSGTXT = 'Do convolution'
                     IF (DOMSG) CALL MSGWRT (2)
                     CALL CONVOL (ICORN, ICOUNT, IRET)
                     IF (IRET.GT.0) GO TO 990
C                                      Replace plane in output.
                     OCORN(3) = LOOP3 - START3 + 1
                     OCORN(4) = LOOP4 - START4 + 1
                     OCORN(5) = LOOP5 - START5 + 1
                     OCORN(6) = LOOP6 - START6 + 1
                     OCORN(7) = LOOP7 - START7 + 1
                     IF (IRET.LT.0) THEN
                        WRITE (MSGTXT,1001) PCORN
                        CALL MSGWRT (7)
                        CALL PLNBLK (DISKO, CNOOUT, OCORN, WINOUT,
     *                     BUFF2, BUFSZ2, LUNWRK, IRET)
                     ELSE
                        MSGTXT = 'Store resulting plane'
                        IF (DOMSG) CALL MSGWRT (2)
                        IF ((.NOT.BLANKD) .OR. (DOBLNK.LT.0.0)) THEN
                           CALL PLNPUT (DISKO, CNOOUT, OCORN, WINOUT, 1,
     *                        NNX, NNY, BUFF1, BUFF2, BUFSZ1, BUFSZ2,
     *                        LUNMP1, LUNWRK, IRET)
                        ELSE
                           CALL PLNPBL (DISKIN, CNOIN, ICORN, DISKO,
     *                        CNOOUT, OCORN, WINOUT, 1, NNX, NNY, BUFF1,
     *                        BUFF2, BUFF3, BUFSZ1, BUFSZ2, LUNMP1,
     *                        LUNWRK, LUNMP2, IRET)
                           END IF
                       END IF
                     IF (IRET.NE.0) GO TO 990
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       If necessary apply FACTOR(s)
      DFACT = FACTOR
      DFACT = DFACT / OVFACT
C                                       Rescale image
      IF (ABS (DFACT-1.0D0).GT.1.0D-6) THEN
         CALL RESCAL (DFACT, 0.0D0, DISKO, CNOOUT, LUNMP1, LUNMP2,
     *      BUFF1, BUFSZ1, BUFF2, BUFSZ2, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       create the data array at the
C                                       vicinity of the maximum position
C                                       at the output image
      IF (LIMAC) THEN
         MAXS = MAXAR
         NXX = BOX(1)
         NYY = BOX(2)
         CALL ARRAY (NXX, NYY, MAXS, XARG, YARG, FUNC, IRET)
         IF (IRET.NE.0) GO TO 990
C                                       The following subroutine
C                                       fits the polinomial to the data
C                                       and calculates position (X0,Y0)
C                                       and value of the maximum (MAXP)
         CALL POL2DI (XARG, YARG, FUNC, NXX, NYY, X0, Y0, MAXP, IRET)
C
         MSGTXT = '---------------------------------------------------'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1010) LXMAX, LYMAX
         CALL MSGWRT (7)
         WRITE (MSGTXT,1100) X0, Y0
         CALL MSGWRT (7)
         MSGTXT = '|  So the more precise maximum position is:        |'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1200) LXMAX+X0, LYMAX+Y0
         CALL MSGWRT (7)
         MSGTXT = '|  The auto correlation max is at:                 |'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1210) NMX/2+1, NMY/2
         CALL MSGWRT (7)
         MSGTXT = '|  The difference between positions of maximuma    |'
         CALL MSGWRT (7)
         MSGTXT = '|  at cross & auto correlation is:                 |'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1220) LXMAX+X0-(NMX/2+1), LYMAX+Y0 - NMY/2
         CALL MSGWRT (7)
         MSGTXT = '----------------------------------------------------'
         CALL MSGWRT (7)
         END IF
C                                       Write history
      CALL CONHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('Fetching image plane:',I5,4I3)
 1001 FORMAT ('Blanking image plane:',I5,4I3)
 1010 FORMAT ('|', 2X, 'The map maximum is at: X= ',
     *   I5, ';  Y=',I5, ' pixels', '|')
 1100 FORMAT ('|', 2X, 'DeltaX= ', F8.2, ';  DeltaY= ', F8.2,
     *   '  pixels', 5X, '|')
 1200 FORMAT ('|', 2X, 'X= ', F8.2, ';  Y= ', F8.2, '  pixels',
     *   15X, '|')
 1210 FORMAT ('|', 2X, 'X= ', I5, ';  Y= ', I5, '  pixels', 21X, '|')
 1220 FORMAT ('|', 2X, 'DX= ', F8.2, ';  DY= ', F8.2, '  pixels',
     *   13X, '|')
      END
      SUBROUTINE CONINI (PRGN, JERR)
C-----------------------------------------------------------------------
C   CONINI gets input parameters for CONVL and creates an output file
C   if necessary.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok
C                      4 => BMAJ, BMIN not fully specified.
C                      5 => catalog troubles
C                      8 => can't start
C   Commons:
C      /INPUTS/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in CONVL for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, BLANK*6, UNDEF*8, JYPBM*8, CHTM12*12,
     *   MTYPE*2
      INTEGER   JERR, NAXIS, NPARM, IERR, IROUND, I
      LOGICAL   T, F, DECON
      REAL      CBMAJ, CBMIN, CBPA, XMAJ, XMIN, XPA
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
      DATA UNDEF, JYPBM /'UNDEFINE','JY/BEAM '/
      DATA CBMAJ, CBMIN, CBPA /3*0.0/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
      BUFSZ1 = MABFSL  * 2
      BUFSZ2 = BUFSZ1
      BUFSZ3 = BUFSZ1
      LUNMP1 = 16
      LUNMP2 = 17
      LUNWRK = 18
      OLD = F
C                                       Init. overflow prevention fact.
      OVFACT = 1.0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 53
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       default for IMSIZE
      BOX(1) = IMSIZE(1)
      BOX(2) = IMSIZE(2)
      IF (BOX(1).EQ.0 .AND. BOX(2).EQ.0) THEN
         BOX(1) = 5
         BOX(2) = 5
         END IF
      IF (DOBLNK.GE.0.0) THEN
         DOBLNK = 1.0
      ELSE
         DOBLNK = -1.0
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      SEQOUT = IROUND (XSO)
      SEQ2 = IROUND (XS2)
      DISKIN = IROUND (XDI)
      DISK2 = IROUND (XD2)
      DISKO = IROUND (XDO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
C                                       Decode OPCODE.
      IF (OPCODE.EQ.' ') OPCODE = 'GAUS'
      LGAUS = OPCODE .EQ. 'GAUS'
      LGMOD = OPCODE .EQ. 'GMOD'
      LIMAG = OPCODE .EQ. 'IMAG'
      LIMAC = OPCODE .EQ. 'IMAC'
      LDCON = OPCODE .EQ. 'DCON'
      LDGAU = OPCODE .EQ. 'DGAU'
      LGAUS = .NOT.(LIMAG.OR.LIMAC)
      IF (BMIN.LE.0.0) BMIN = BMAJ
      DO 25 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 25      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      CNOIN = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                        Check defaults on BLC TRC
      NAXIS = CATBLK(KIDIM)
      CALL WINDOW (NAXIS, CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((TRC(1)-BLC(1)+1.GT.MAXIMG/2) .OR.
     *   (TRC(2)-BLC(2)+1.GT.MAXIMG/2)) THEN
         WRITE (MSGTXT,1041) MAXIMG/2
         CALL MSGWRT (8)
         IERR = 3
         GO TO 999
         END IF
C                                        Set first and last dim.
      START3 = IROUND (BLC(3))
      START4 = IROUND (BLC(4))
      START5 = IROUND (BLC(5))
      START6 = IROUND (BLC(6))
      START7 = IROUND (BLC(7))
      END3 = IROUND (TRC(3))
      END4 = IROUND (TRC(4))
      END5 = IROUND (TRC(5))
      END6 = IROUND (TRC(6))
      END7 = IROUND (TRC(7))
C                                        Save old CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                      Set input window.
      WININ(1) = IROUND (BLC(1))
      WININ(2) = IROUND (BLC(2))
      WININ(3) = IROUND (TRC(1))
      WININ(4) = IROUND (TRC(2))
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Check if units Jy/beam
      CALL H2CHR (8, 1, CATH(KHBUN), CHTM12)
      CALL CHLTOU (8, CHTM12)
      PIXEL = JYPBM.NE.CHTM12(:8)
C                                        Map rotation.
      MAPROT = 0.0
      CALL ROTFND (CATR, MAPROT, IERR)
      NOTSEC = IERR.NE.0
      IERR = 0
C                                       Set coordinate factor
      COCNV = 3600.
      IF (NOTSEC) COCNV = 1.0
      USECG = .FALSE.
C                                       Compute convolving gaussian
C                                       Check if gaussian specified.
      IF (.NOT.(LIMAG.OR.LIMAC)) THEN
C                                       Beam not fully specified.
         IF ((BMAJ.LE.0.0) .OR. (BMIN.LE.0.0)) THEN
            IF (LDCON) THEN
               BMAJ = CATR(KRBMJ) * COCNV
               BMIN = CATR(KRBMN) * COCNV
               BPA = CATR(KRBPA)
            ELSE IF (OPCODE.EQ.'GAUS') THEN
               CALL FNDEXT ('CG', CATBLK, I)
               USECG = (I.GT.0)
               BMAJ = CATR(KRBMJ) * COCNV
               BMIN = CATR(KRBMN) * COCNV
               BPA = CATR(KRBPA)
               CALL GETGAU (BMAJ, BMIN, BPA, COCNV)
               END IF
            IF ((BMAJ.LE.0.0) .OR. (BMIN.LE.0.0)) THEN
               IERR = 4
               WRITE (MSGTXT,1045) BMAJ, BMIN
               GO TO 990
               END IF
            END IF
C                                       Setup.
         DECON = F
C                                       Initialize conv. to input.
         CVBMAJ = BMAJ
         CVBMIN = BMIN
         CVBPA = BPA
C                                       Set axis increment.
         XSPACE = CATR(KRCIC)
         YSPACE = CATR(KRCIC+1)
         IF (ABS (XSPACE).LT.1.0E-15) XSPACE = 1.0 / COCNV
         IF (ABS (YSPACE).LT.1.0E-15) YSPACE = 1.0 / COCNV
C                                      Initialize convolving beam.
         CBMAJ = 0.0
         CBMIN = 0.0
         CBPA = 0.0
C                                       Get CLEAN beam, first check
C                                       if CLEAN map.
         IF (.NOT.LGMOD) THEN
            IF ((CATBLK(KITYP).EQ.1) .OR. (CATBLK(KITYP).EQ.2)) THEN
               IF ((.NOT.LDGAU) .AND. (.NOT.LDCON)) THEN
                  CBMAJ = CATR(KRBMJ) * COCNV
                  CBMIN = CATR(KRBMN) * COCNV
                  CBPA = CATR(KRBPA)
                  END IF
               END IF
C                                       Print CLEAN beam size
            WRITE (MSGTXT,1060) CBMAJ, CBMIN, CBPA
            CALL MSGWRT (4)
C                                       CG file for beams
            CALL FNDEXT ('CG', CATBLK, I)
            USECG = (I.GT.0)
            END IF
C                                       Deconvolve.
         CALL DCHECK (CBMAJ, CBMIN, CBPA, XMAJ, XMIN, XPA, IERR)
C                                       Check if worked.
         IF (IERR.EQ.0) THEN
            CVBMAJ = XMAJ
            CVBMIN = XMIN
            CVBPA = XPA
            DECON = T
         ELSE
            WRITE (MSGTXT,1070)
            JERR = 4
            GO TO 990
            END IF
         CVBMAP = CVBMAJ
         CVBMIP = CVBMIN
         CVBPAP = CVBPA
C                                     Print convolving beam size
         IF (.NOT.USECG) THEN
            WRITE (MSGTXT,1072) CVBMAJ, CVBMIN, CVBPA
            CALL MSGWRT (4)
            END IF
C                                     Put new beam in header.
         IF (LDCON) THEN
            CATR(KRBMJ) = 0.0
            CATR(KRBMN) = 0.0
            CATR(KRBPA) = 0.0
            CATBLK(KITYP) = 4
         ELSE
            CATR(KRBMJ) = BMAJ / COCNV
            CATR(KRBMN) = BMIN / COCNV
            CATR(KRBPA) = BPA
            END IF
         END IF
C                                     Determine FACTOR
      IF (FACTOR.LE.0.0) THEN
C                                     If gauss and have CLEAN beam
C                                     use them
         IF (.NOT.LDCON) THEN
            IF ((CBMAJ*CBMIN.GT.0.0) .AND. (BMAJ*BMIN.GT.0.0))
     *         FACTOR = (BMAJ / CBMAJ) * (BMIN / CBMIN)
C                                       If NOTSEC and PIXEL use
C                                       default FACTOR = 1.0
            IF ((NOTSEC.AND.PIXEL) .AND. (LGAUS)) FACTOR = 1.0
C                                       Jy/pixel -> Jy/beam
            IF (PIXEL .AND. (LGAUS) .AND. (FACTOR.LE.0.0)) THEN
               FACTOR = 1.1331 * (BMAJ / (COCNV * ABS (XSPACE))) *
     *            (BMIN / (COCNV * ABS (YSPACE)))
               CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
               END IF
            END IF
C                                     No FACTOR USE 1.0
         IF (FACTOR.LE.0.0) THEN
            FACTOR = 1.0
            CALL CHR2H (8, UNDEF, 1, CATH(KHBUN))
            END IF
         END IF
      IF ((LDCON) .AND. (FACTOR.LE.1.01)) FACTOR = 1000.
C                                       Get axis dimensions right
      CALL SUBHDR (BLC, TRC, 1.0, 1.0)
C                                       Tell user FACTOR
      WRITE (MSGTXT,1080) FACTOR, 'scale image'
      IF (LDCON) WRITE (MSGTXT,1080) FACTOR, 'clip inv Gaussian'
      CALL MSGWRT (4)
      WASFCT = FACTOR
C                                       Create output file.
      CATR(KRDMX) = -1.E10
      CATR(KRDMN) = 1.E10
      CNOOUT = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CATR(KRBLK) = 0.0
      CALL MCREAT (DISKO, CNOOUT, BUFF1, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1200) IERR
         GO TO 990
         END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
C                                       Also refuse if types differ.
      IF (IERR.EQ.2) THEN
         IF ((CNOOUT.NE.CNOIN) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1210)
            GO TO 990
         ELSE
C                                       Recover existing CATBLK
            OLD = T
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CNOOUT, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1220) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOOUT
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy some keywords
      CALL KEYPCP (DISKIN, CNOIN, DISKO, CNOOUT, 0, ' ', IERR)
C                                        Put input file in READ
      MTYPE = 'MA'
      CALL CATDIR ('CSTA', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONINI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1041 FORMAT ('CANNOT HANDLE SUBIMAGES LARGER THAN',I6,' ON A SIDE')
 1045 FORMAT ('BEAM NOT FULLY SPECIFIED, BMAJ=',1PE12.5,',BMIN=',E12.5)
 1060 FORMAT ('CLEAN beam   : MAJ: ',F8.4,' MIN: ',F8.4,' PA: ',F6.1)
 1070 FORMAT ('** WARNING ** Cannot convolve to achieve BMAJ, BMIN',
     *   ' values')
 1072 FORMAT ('Convolve all planes with Major',F9.4,' Minor',F9.4,
     *   ' PA ',F6.1)
 1080 FORMAT ('Using factor =',1PE12.5,' to ',A)
 1200 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1210 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1220 FORMAT ('CONINI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE CONFIL (IERR)
C-----------------------------------------------------------------------
C  CONFIL finds the input image and if a convolving image
C  is to be used it is rotated so that the center cell
C  is at NFFTX/2+1, NFFTY/2+1 for the FFT.
C   Output:
C   IERR        I    Return error code. 0 => OK, error otherwise.
C-----------------------------------------------------------------------
      CHARACTER CHSTOK*8, STAT*4, MTYPE*2
      HOLLERITH CAT2H(256)
      INTEGER   IERR, CATBLK(256), ISTOK, JERR, CORN(7), IDEP(5), ICENX,
     *   ICENY, WINCIN(4), KMX, KMY, ITEMP, IROUND,CAT2(256), MX, MY,
     *   ISIZE, JSIZE
      DOUBLE PRECISION CATD(128), CAT2D(128)
      REAL      CATR(256), CAT2R(256), DELX, DELY, XCEN, YCEN
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD), (CAT2, CAT2R, CAT2D)
      DATA CORN, IDEP /7*1, 5*1/, CHSTOK /'STOKES  '/
C-----------------------------------------------------------------------
      MX = 0
      MY = 0
      XSPACE = CATR(KRCIC)
C                                       Dec spacing.
      YSPACE = CATR(KRCIC+1)
C                                       Determine subimage size.
      NX = WININ(3) - WININ(1) + 1
      NY = WININ(4) - WININ(2) + 1
C                                       Determine size of FFT's
      NFFTX = (LOG(1.0*(WININ(3)-WININ(1)+1)) / LOG (2.0)) + 0.999
      NFFTX = 2 ** NFFTX
      ITEMP = 2 * NFFTX
      NFFTX = MIN (MAXIMG, ITEMP)
      NFFTY = (LOG (1.0*(WININ(4)-WININ(2)+1)) / LOG (2.0)) + 0.999
      NFFTY = 2 ** NFFTY
      ITEMP = 2 * NFFTY
      NFFTY = MIN (MAXIMG, ITEMP)
      DOMSG = (NFFTY.GE.4096) .OR. (NFFTX.GE.4096)
C                                       Check if convolving map to
C                                       be used.
      IF (LDGAU .OR. (LIMAG.OR.LIMAC)) THEN
C                                       Read convolving image.
C                                       Get catalog slot.
         CNOIN2 = 1
         MTYPE = 'MA'
         CALL CATDIR ('SRCH', DISK2, CNOIN2, NAME2, CLAS2, SEQ2,
     *      MTYPE, NLUSER, STAT, BUFF3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME2, CLAS2, SEQ2, DISK2,
     *         NLUSER
            GO TO 990
            END IF
C                                       Copy CATBLK and mark READ.
         CALL CATIO ('READ', DISK2, CNOIN2, CAT2, 'READ', BUFF3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK2
         FCNO(NCFILE) = CNOIN2
         FRW(NCFILE) = 0
C                                      Determine input map size.
         MX = CAT2(KINAX)
         MY = CAT2(KINAX+1)
C
         NMX = MX
         NMY = MY
C                                       Determine convl. fn. center.
         XCEN = MX / 2
         YCEN = MY / 2 + 1
C                                       If a beam find the center.
         ISTOK = 3
         CALL AXEFND (8, CHSTOK, CAT2(KIDIM), CAT2H(KHCTP), ISTOK,
     *      JERR)
         IF ((JERR.EQ.0) .AND. CAT2D(KDCRV+ISTOK).EQ.0.0D0)
     *      CALL PEAKFN (LUNMP2, DISK2, CNOIN2, IDEP, CAT2, BUFF1,
     *         BUFF1, BUFSZ1, XCEN, YCEN, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Set beam window
         WINCIN(1) = XCEN - NFFTX / 2
         WINCIN(3) = XCEN + NFFTX / 2 - 1
         WINCIN(2) = YCEN - NFFTY / 2
         WINCIN(4) = YCEN + NFFTY / 2 - 1
         WINCIN(1) = MAX (1, WINCIN(1))
         WINCIN(2) = MAX (1, WINCIN(2))
         WINCIN(3) = MIN (MX, WINCIN(3))
         WINCIN(4) = MIN (MY, WINCIN(4))
         XCEN = XCEN - MX/2 - 1
         YCEN = YCEN - MY/2 - 1
C                                       If using small subimage there
C                                       is no need to rotate beam
         KMX = WINCIN(3) - WINCIN(1) + 1
         KMY = WINCIN(4) - WINCIN(2) + 1
         IF ((MX - KMX) .GT. ABS (XCEN)) XCEN = 0.0
         IF ((MY - KMY) .GT. ABS (YCEN)) YCEN = 0.0
         ICENX = IROUND (XCEN)
         ICENY = IROUND (YCEN)
         END IF
C                                       Set output window.
      WINOUT(1) = (NFFTX/2 - NX) / 2.0 + 1.6
      WINOUT(2) = (NFFTY/2 - NY) / 2.0 + 1.6
      WINOUT(3) = WINOUT(1) + NX - 1
      WINOUT(4) = WINOUT(2) + NY - 1
C                                       Compute FACT2 for the effects
C                                       of changing map size.
      FACT2 = 1.0
      IF (.NOT.LGAUS) FACT2 = REAL (NFFTX) * REAL (NFFTY)
      FACTOR = FACTOR * FACT2
C                                       Make sure spacing the same as
C                                       for the beam.
      IF (.NOT.LGAUS) THEN
         IF ((XSPACE.NE.0.0) .AND. (YSPACE.NE.0.0)) THEN
            DELX = (XSPACE-CAT2R(KRCIC)) / XSPACE
            DELY = (YSPACE-CAT2R(KRCIC+1)) / YSPACE
            IF ((ABS (DELX).LT.0.05) .AND. (ABS (DELY).LT.0.05))
     *         GO TO 40
            END IF
         IERR = 1
         WRITE (MSGTXT,1030)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1031) XSPACE, YSPACE, CAT2R(KRCIC),
     *      CAT2R(KRCIC+1)
         GO TO 990
         END IF
 40   IF (ABS (XSPACE).LT.1.0E-15) XSPACE = 1.0
      IF (ABS (YSPACE).LT.1.0E-15) YSPACE = 1.0
C                                       Create scratch files.
      MSGTXT = 'Begin making 3 scratch files'
      IF (DOMSG) CALL MSGWRT (2)
C                                       Image file.
      ISIZE = MAX (NFFTX, 64)
C                                       Factor 4 to make CONVL work on
C                                       512*1024 maps.   GvM Jan-07-93
C                                       Th is must be < 2 Gbytes
      ISIZE = (ISIZE * (NFFTY + 2)) + 2048 * 2
      ISIZE = (ISIZE - 1) / 256 + 1
      JSIZE = 2 * NFFTY * (MAX(NFFTX,64)/2 + 1) + 2 * (2048 + NFFTX)
      JSIZE = (JSIZE - 1) / 256 + 1
      ISIZE = MAX (ISIZE, JSIZE)
      CALL SCREAT (ISIZE, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Get name
      MP1VOL = SCRVOL(NSCR)
      MP1SCR = NSCR
      CALL ZPHFIL ('SC', MP1VOL, SCRCNO(NSCR), 1, MP1FIL, IERR)
C                                       Convolving image file.
      CALL SCREAT (ISIZE, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Get name
      MP2VOL = SCRVOL(NSCR)
      MP2SCR = NSCR
      CALL ZPHFIL ('SC', MP2VOL, SCRCNO(NSCR), 1, MP2FIL, IERR)
C                                       Work file
      CALL SCREAT (ISIZE, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Get name
      WRKVOL = SCRVOL(NSCR)
      WRKSCR = NSCR
      CALL ZPHFIL ('SC', WRKVOL, SCRCNO(NSCR), 1, WRKFIL, IERR)
C                                       Save CATBLK in BUFF3
      CALL COPY (256, CATBLK, BUFF3)
C                                      Rotate convolving image if
C                                      necessary.
      IF ((LIMAG.OR.LIMAC) .OR. LDGAU) THEN
         MSGTXT = 'Fetching convolution image'
         IF (DOMSG) CALL MSGWRT (2)
         CALL PLNGET (DISK2, CNOIN2, CORN, WINCIN, ICENX, ICENY, MP2SCR,
     *      NFFTX, NFFTY, BUFF1, BUFF2, BUFSZ1,BUFSZ2, LUNMP2, LUNWRK,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1070) IERR
            GO TO 990
            END IF
         END IF
C                                      Restore CATBLK
      CALL COPY (256, BUFF3, CATBLK)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONFIL: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I4)
 1010 FORMAT ('CONFIL: CANNOT COPY CONV. FN. CATBLK, ERROR',I3)
 1040 FORMAT ('CONFIL: ERROR',I3,' CREATING MAP1 SCRATCH FILE')
 1050 FORMAT ('CONFIL: ERROR',I3,' CREATING MAP2 SCRATCH FILE')
 1060 FORMAT ('CONFIL: ERROR',I3,' CREATING WORK SCRATCH FILE')
 1070 FORMAT ('CONFIL: CANNOT COPY IMAGE, ERROR ',I3)
 1030 FORMAT ('CONFIL: UNEQUAL SPACINGS IN TWO MAPS')
 1031 FORMAT ('       IMAGE=',2F8.4,' CONV. FN=',2F8.4)
      END
      SUBROUTINE CONVOL (ICORN, ICOUNT, IERR)
C-----------------------------------------------------------------------
C   CONVOL convolves an image with either a Gaussian or another image.
C   Input:
C      ICORN    I(7)   Corners of input image
C      ICOUNT   I      Number of times this routine has been called.
C                      Used for initialization on the first call.
C      BMAJ     R      Major axis of convolving Gaussian.  If .le. 0.0
C                      the secondary input image is used as the
C                      convolving fn.
C   Output:
C      IERR     I   Return code, 0 => OK, otherwise error.
C   Max. and min. are also put in the map header.
C-----------------------------------------------------------------------
      INTEGER   ICORN(7), ICOUNT, IERR, BO(3), VOL(3), LUN(3), IDIR,
     *   KAP, CATBLK(256), NNX, NNY, NEED
      LOGICAL   FULL
      CHARACTER FIL(3)*48
      REAL   CATR(256), XSCALE, SMAX, SMIN
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DAPM.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR)
      DATA BO /1, 1, 1/
      DATA FULL /.FALSE./
C-----------------------------------------------------------------------
      LUN(1) = LUNMP1
      LUN(2) = LUNMP2
      LUN(3) = LUNWRK
C                                      Read output CATBLK.
      CALL CATIO ('READ', DISKO, CNOOUT, CATBLK, 'REST', BUFF1, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Reset IERR (CATIO will normally
C                                       return IERR=6 if ICOUNT is not
C                                       1)
      IERR = 0
C                                       FFT convolving image
      NEED = (2 * NFFTX * NFFTY) / 1024
      IF ((LDGAU .OR. (LIMAG.OR.LIMAC)) .AND. (ICOUNT.EQ.1)) THEN
         MSGTXT = 'FFT convolving image'
         IF (DOMSG) CALL MSGWRT (2)
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CONVOL(1): DID NOT GET REQUESTED AP MEMORY'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 999
            END IF
         VOL(1) = MP2VOL
         VOL(2) = WRKVOL
         VOL(3) = MP2VOL
         FIL(1) = MP2FIL
         FIL(2) = WRKFIL
         FIL(3) = MP2FIL
         IDIR = 3
C                                       I do not want to add a new input
C                                       parameter, so put IERR=11 (LK)
         IF (LIMAC) IERR = 11
         CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, SMAX, SMIN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL QRLSE
         END IF
C                                       Scale to prevent overflows.
      XSCALE = 1.0
      IF (ICOUNT.EQ.1) XSCALE = MAX (ABS (SMAX), ABS(SMIN))
      IF (ABS (XSCALE).GE.1.0E6) OVFACT = OVFACT / XSCALE
C                                       Convolutions
      IF (LDGAU) THEN
         MSGTXT = 'FFT imput image plane'
         IF (DOMSG) CALL MSGWRT (2)
C                                       FFT input image.
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CONVOL(2): DID NOT GET REQUESTED AP MEMORY'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 999
            END IF
         VOL(1) = MP1VOL
         VOL(2) = WRKVOL
         VOL(3) = MP1VOL
         FIL(1) = MP1FIL
         FIL(2) = WRKFIL
         FIL(3) = MP1FIL
         IDIR = 3
         CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, SMAX, SMIN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL QRLSE
C                                       Scale to prevent overflows.
         XSCALE = 1.0
         IF (ICOUNT.EQ.1) XSCALE = MAX (ABS (SMAX), ABS(SMIN))
         IF (ABS (XSCALE).GE.1.0E6) OVFACT = OVFACT / XSCALE
C                                       Compute XFER fn. first time
C                                       only.
         IF ((ICOUNT.EQ.1) .OR. (USECG)) THEN
            MSGTXT = 'Make FFT of Gaussian'
            IF ((DOMSG) .AND. (ICOUNT.EQ.1)) CALL MSGWRT (2)
            CALL CONGAU (ICORN, ICOUNT, WRKSCR, IERR)
C                                       CONGAU applies FACTOR
            FACTOR = 1.0
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1100) IERR
               GO TO 990
            ELSE IF (IERR.LT.0) THEN
               GO TO 999
               END IF
            CALL CONDIV (IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1160) IERR
               GO TO 990
               END IF
            END IF
C                                     Convolve/Multiply.
         MSGTXT = 'Multiply FFTed images'
         IF (DOMSG) CALL MSGWRT (2)
         CALL CONMUL (IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) IERR
            GO TO 990
            END IF
C                                     FFT back.
         MSGTXT = 'FFT back convolved image'
         IF (DOMSG) CALL MSGWRT (2)
         CALL QINIT (0, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CONVOL(4): DID NOT GET REQUESTED AP MEMORY'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 999
            END IF
         VOL(1) = WRKVOL
         VOL(2) = MP1VOL
         VOL(3) = WRKVOL
         FIL(1) = WRKFIL
         FIL(2) = MP1FIL
         FIL(3) = WRKFIL
         IDIR = -1
         CALL PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PASS2 (IDIR, FULL, LUN, VOL, FIL, BO, BUFF1, BUFSZ1,
     *      BUFF2, BUFSZ2, NFFTX, NFFTY, SMAX, SMIN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL QRLSE
      ELSE
C                                       OPCODEs GAUS and  IMAG
         NEED = (NFFTX * NFFTY) / 1024
         CALL QINIT (NEED, 0, KAP)
         IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
            MSGTXT = 'CONVOL(4): DID NOT GET REQUESTED AP MEMORY'
            CALL MSGWRT (8)
            IERR = 10
            GO TO 999
            END IF
         IF (((ICOUNT.EQ.1) .OR. (USECG)) .AND. LGAUS) THEN
            MSGTXT = 'Make FFT of Gaussian'
            IF ((DOMSG) .AND. (ICOUNT.EQ.1)) CALL MSGWRT (2)
            CALL CONGAU (ICORN, ICOUNT, MP2SCR, IERR)
C                                       CONGAU applies FACTOR
            FACTOR = 1.0
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1100) IERR
               GO TO 990
            ELSE IF (IERR.LT.0) THEN
               GO TO 999
               END IF
            END IF
C                                       Convolve
         MSGTXT = 'Convolve with APCONV'
         IF (DOMSG) CALL MSGWRT (2)
         NNX = NFFTX/2
         NNY = NFFTY/2
C
         CALL APCONV (NNX, NNY, MP1SCR, WRKSCR, MP1SCR, MP1SCR, MP2SCR,
     *      OVFACT, BUFSZ1, BUFF1, BUFF2, BUFF3, SMAX, SMIN, IERR)
C
         CALL QRLSE
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1301) IERR
            GO TO 990
            END IF
         END IF
C                                        Get max, min.
      IF (ICOUNT.EQ.1) THEN
         CATR(KRDMX)= SMAX
         CATR(KRDMN) = SMIN
      ELSE
         CATR(KRDMX) = MAX (CATR(KRDMX), SMAX)
         CATR(KRDMN) = MIN (CATR(KRDMN), SMIN)
         END IF
C                                        Rewrite output CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1300) IERR
C                                       Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONVOL: ERROR',I3,' READING OUTPUT CAT. HEADER')
 1100 FORMAT ('CONVOL: ERROR',I3,' COMPUTING GAUSSIAN FUNCTION')
 1160 FORMAT ('CONVOL: ERROR',I3,' DIVIDING TRANSFORMS')
 1200 FORMAT ('CONVOL: ERROR',I3,' MULTIPLYING TRANSFORMS')
 1300 FORMAT ('CONVOL: ERROR',I3,' UPDATING OUTPUT CAT. HEADER')
 1301 FORMAT ('CONVOL: APCONV ERROR',I3,' CONVOLVING IMAGE')
      END
      SUBROUTINE CONMUL (IERR)
C-----------------------------------------------------------------------
C   CONMUL multiplies the complex values in files MP1 and MP2 and
C   writes the complex results into file WRK.
C   Output:
C   IERR   I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IERR
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO,
     *   WIN(4), MX, MY, I, J, J1
      LOGICAL   T,F
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./, BO /1/, WIN /4*0/
C-----------------------------------------------------------------------
C                                        Open and init MP1
      CALL ZOPEN (LUNMP1, FIND1, MP1VOL, MP1FIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 1
         GO TO 990
         END IF
      MX = NFFTX / 2 + 1
      MY = NFFTY * 2
      CALL MINIT ('READ', LUNMP1, FIND1, MY, MX, WIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ', 1
         GO TO 990
         END IF
C                                        Open and init MP2
      CALL ZOPEN (LUNMP2, FIND2, MP2VOL, MP2FIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 2
         GO TO 990
         END IF
      CALL MINIT ('READ', LUNMP2, FIND2, MY, MX, WIN, BUFF2, BUFSZ2, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ', 2
         GO TO 990
         END IF
C                                        Open and init WRK
      CALL ZOPEN (LUNWRK, FIND3, WRKVOL, WRKFIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNWRK, FIND3, MY, MX, WIN, BUFF3, BUFSZ3, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'WRIT'
         GO TO 990
         END IF
C                                        Loop, multiplying.
      DO 110 I = 1,MX
C                                        Read MP1
         CALL MDISK ('READ', LUNMP1, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'READ', 1
            GO TO 990
            END IF
C                                        Read MP2
         CALL MDISK ('READ', LUNMP2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'READ', 2
            GO TO 990
            END IF
C                                        Write WRK
         CALL MDISK ('WRIT', LUNWRK, FIND3, BUFF3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'WRIT'
            GO TO 990
            END IF
C                                        Multiply row.
            DO 100 J = 1,MY,2
               J1 = J - 1
               BUFF3(BIND3+J1) =
     *            BUFF1(BIND1+J1)*OVFACT*BUFF2(BIND2+J1) -
     *            BUFF1(BIND1+J)*OVFACT*BUFF2(BIND2+J)
               BUFF3(BIND3+J) =
     *            BUFF1(BIND1+J1)*OVFACT*BUFF2(BIND2+J) +
     *            BUFF1(BIND1+J)*OVFACT*BUFF2(BIND2+J1)
 100           CONTINUE
 110     CONTINUE
C                                        Flush buffer.
      CALL MDISK ('FINI', LUNWRK, FIND3, BUFF3, BIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR, 'FINI'
         GO TO 990
         END IF
C                                        Close files.
      CALL ZCLOSE (LUNMP1, FIND1, IERR)
      CALL ZCLOSE (LUNMP2, FIND2, IERR)
      CALL ZCLOSE (LUNWRK, FIND3, IERR)
      IERR = 0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONMUL: ERROR',I3,' OPEN FOR ',A4,' FILE',I2)
 1010 FORMAT ('CONMUL: ERROR',I3,' INIT FOR ',A4,' FILE',I2)
 1060 FORMAT ('CONMUL: ERROR',I3,1X,A4,'ING FILE',I2)
      END
      SUBROUTINE CONGAU (ICORN, ICOUNT, OUTSCR, IERR)
C-----------------------------------------------------------------------
C   CONGAU puts gaussian transfer fn in specified file.
C   Input:
C     OUTSCR        I    Output /CFILES/ slot number
C   Input from common:
C     NFFTX,NFFTY   I    Number of map grid cells in X and Y.
C     BMAJ      R    Major Axis (FWHP in cells)
C     BMIN      R    Minor axis size (FWHP in cells).
C     BPA       R    Position angle of restoring beam (deg ).
C     XSPACE    R    RA grid spacing (deg).
C     YSPACE    R    Dec grid spacing (deg).
C     MAPROT    R    Coordinate rotation (deg).
C   Output:
C     IERR   I      Return error code. 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   ICORN(7), ICOUNT, OUTSCR, IERR
C
      REAL      COMP(128), WT, XNX2, XNY2, XNXNY, TEMP, TA, TB, AM, AN,
     *   GAUSAA, GAUSCC, GAUSBB, AK
      INTEGER    I, K, LIM, FLIST(40), NAPRES, NAPGAU, NAPEXP, NAPEX1,
     *   APSAV, ONENY, TWONY, WRK1, WRK2, KAP
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DAPM.INC'
      EQUIVALENCE (BUFF2(1), COMP(1))
C-----------------------------------------------------------------------
      FACTOR = WASFCT
      IF (USECG) THEN
         CALL GETCB (ICORN, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1000) ICOUNT, CVBMAP, CVBMIP, CVBPAP
         CALL MSGWRT (4)
         END IF
      IF (ICOUNT.EQ.1) THEN
         CALL RFILL (3*MAXIMG, 0.0, CVBHI)
         CVBHI(1,ICOUNT) = CVBMAP
         CVBHI(2,ICOUNT) = CVBMIP
         CVBHI(3,ICOUNT) = CVBPAP
      ELSE IF (USECG) THEN
         CVBHI(1,ICOUNT) = CVBMAP
         CVBHI(2,ICOUNT) = CVBMIP
         CVBHI(3,ICOUNT) = CVBPAP
         END IF
      IERR = 1
C                                       Set AP assignments.
      ONENY = NFFTY
      TWONY = 2*NFFTY
      NAPRES = 100
      WRK1 = ONENY + 1
      WRK2 = TWONY + 1
      NAPEXP = NAPRES + WRK2
      NAPGAU = NAPEXP + WRK1
      NAPEX1 = NAPGAU + WRK1
      APSAV = NAPEX1 + WRK1
C                                       Do a little arithmetic to put
C                                       the beam parameters into useable
C                                       form. Convert to sigma, then to
C                                       coeficients of U**2, U*V, and
C                                       V**2 the last of which is return
C                                       for later use.
      TA = CVBMAP * 3.1415927 / 1.1774
      TB = CVBMIP * 3.1415927 / 1.1774
      AM = COS ((CVBPAP+MAPROT)*3.1415927/180.)
      AN = SIN ((CVBPAP+MAPROT)*3.1415927/180.)
      XNX2 = NFFTX * XSPACE * COCNV
      XNY2 = NFFTY * YSPACE * COCNV
      XNXNY = ABS (XNX2 * XNY2)
      XNX2 = XNX2 ** 2
      XNY2 = XNY2 ** 2
      GAUSAA = (TA*TA*AM*AM + TB*TB*AN*AN) / (XNY2)
      GAUSCC = (TA*TA*AN*AN + TB*TB*AM*AM) / (XNX2)
      GAUSBB = ((TB*TB-TA*TA) * AN*AM) / (XNXNY )
C                                       Create array NAPEX1.
      DO 10 I = 1,NFFTY
         K = I - 1
         IF (I.GT.NFFTY/2) K = K - NFFTY
         COMP(I) = (-GAUSBB * K)
 10      CONTINUE
C                                       Load NAPEX1 into AP.
      CALL QINIT (0, 0, KAP)
      IF ((KAP.EQ.0) .OR. (PSAPNW.LE.0)) THEN
         MSGTXT = 'CONGAU: DID NOT GET REQUESTED AP MEMORY'
         IERR = 10
         GO TO 990
         END IF
      CALL QPUT (COMP, NAPEX1, ONENY, 2)
      CALL QWD
C                                       Create array NAPGAU.
      DO 20 I = 1,NFFTY
         K = I - 1
         IF (I.GT.NFFTY/2) K = K - NFFTY
         AK = K
         COMP(I) = -0.5 * GAUSAA * AK * AK
 20      CONTINUE
C                                       Load NAPGAU into AP.
      CALL QPUT (COMP, NAPGAU, ONENY, 2)
C                                       Put in scaling factor.
      WT = LOG (FACTOR)
      IF (LDCON) WT = 0.0
      CALL QPUT (WT, 0, 1, 2)
      IF (LDCON) THEN
         COMP(1) = 1.0
         COMP(2) = 1.0 / FACTOR
         CALL QPUT (COMP, 2, 2, 2)
         END IF
      CALL QWD
C                                       Compute transfer fn.
C                                       Setup for APIO
      CALL FILL (40, 0, FLIST)
      FLIST(1) = LUNMP1
      FLIST(2) = OUTSCR
      FLIST(3) = 0
      FLIST(5) = NFFTY * 2
      FLIST(6) = NFFTX / 2 + 1
      FLIST(13) = BUFSZ1
C                                        Prepare NAPEXP using weight
C                                        left in AP loc 0.
      CALL QVCLR (NAPEXP, 1, ONENY)
      CALL QVSADD (NAPEXP, 1, 0, NAPEXP, 1, ONENY)
C                                        Zero NAPRES.
      CALL QVCLR (NAPRES, 1, TWONY)
      CALL QWR
C                                      Loop thru grid
      LIM = NFFTX / 2 + 1
      DO 100 I = 1,LIM
         TEMP = (-0.5 * GAUSCC * (I-1.0)**2)
         CALL QPUT (TEMP, 1, 1, 2)
         CALL QWD
C                                      Compute XFER fn.
         CALL QVSADD (NAPGAU, 1, 1, NAPRES, 2, ONENY)
         CALL QVADD (NAPRES, 2, NAPEXP, 1, NAPRES, 2, ONENY)
         CALL QVEXP (NAPRES, 2, NAPRES, 2, ONENY)
C                                       Prepare NAPEXP for next.
         CALL QVADD (NAPEXP, 1, NAPEX1, 1, NAPEXP, 1, ONENY)
C                                       Invert the Gaussian in DCON
         IF (LDCON) THEN
            CALL QVCLIP (NAPRES, 2, 3, 2, NAPRES, 2, ONENY)
            CALL QVDIV (NAPRES, 2, 2, 0, NAPRES, 2, ONENY)
            END IF
         CALL QWR
C                                       Write row
         CALL APIO ('WRIT', FLIST, NAPRES, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, I
            GO TO 990
            END IF
C                                       Check AP roller.
         IF ((MOD (I,128).NE.0) .OR. (I.EQ.LIM)) GO TO 100
C                                       Call AP roller.
            CALL QROLL (APSAV, BUFF2, BUFSZ2, IERR)
            IF (IERR.NE.0) GO TO 999
 100     CONTINUE
      CALL QRLSE
C                                       Flush buffer / close output
      CALL APIO ('CLOS', FLIST, NAPRES, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR, LIM
         GO TO 990
         END IF
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONGAU: plane',I6,' convolve with',F10.5,' x',F10.5,
     *   ' at',F6.1)
 1060 FORMAT ('CONGAU: ERROR',I3,' WRITING TRANSFER FILE ROW',I5)
      END
      SUBROUTINE CONHIS
C-----------------------------------------------------------------------
C   CONHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP(2)*2
      INTEGER   CATBLK(256), LUN1, LUN2, ITEMP, IPTR, IERR, I
      LOGICAL   T
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      COMMON /MAPHDR/ CATBLK
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'CG','FQ'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, CNOIN, CNOOUT, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       Copy second input history.
      IF (.NOT.LGAUS) THEN
         WRITE (HILINE,1010) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         CALL HIOPEN (LUN1, DISK2, CNOIN2, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            CALL MSGWRT (6)
            GO TO 10
            END IF
         CALL HILOCT ('SRCH', LUN2, IPTR, IERR)
         ITEMP = HITAB(IPTR+2)
         CALL HICOPY (LUN1, BUFF1, LUN2, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1011) IERR
            CALL MSGWRT (6)
            IF (IERR.LT.100) GO TO 20
            HITAB(IPTR+2) = ITEMP
            END IF
         END IF
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      IF ((LIMAG.OR.LIMAC).OR.LDGAU) CALL HENCO2 (TSKNAM, NAME2, CLAS2,
     *   SEQ2, DISK2, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                        BLC
      WRITE (HILINE,1014) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                        TRC
      WRITE (HILINE,1015) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                        FACTOR
      IF (LDCON) THEN
         WRITE (HILINE,1016) TSKNAM, WASFCT, 'Upper limit on inverse'
      ELSE
         WRITE (HILINE,1016) TSKNAM, WASFCT, 'Units scaling factor'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                        If used convl. gauss. write.
      IF (.NOT.(LIMAG.OR.LIMAC)) THEN
         WRITE (MSGTXT,1012) BMAJ, BMIN, BPA
         CALL MSGWRT (4)
         HILINE = TSKNAM // MSGTXT
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 20
         DO 15 I = 1,MAXIMG
            IF (CVBHI(1,I).GT.0.0) THEN
               WRITE (HILINE,1013) TSKNAM, I, CVBHI(1,I), CVBHI(2,I),
     *            CVBHI(3,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 20
               END IF
 15         CONTINUE
         END IF
C                                       OPCODE
      WRITE (HILINE,1017) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       DOBLANK
      IF (DOBLNK.GE.0.0) THEN
         HILINE = TSKNAM // 'DOBLANK = 1   / Blanks restored after FFT'
      ELSE
         HILINE = TSKNAM // 'DOBLANK = -1  / Blanks not restored' //
     *      ' after FFT'
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, CNOIN, CNOOUT,
     *   CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOOUT, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1005 FORMAT ('UNABLE TO OPEN SECOND INPUT HISTORY FILE: ERR',I7)
 1010 FORMAT (A6,'/************** Convolving function history')
 1011 FORMAT ('CONHIS: ERROR',I3,' COPYING CONVL. FN. HISTORY')
 1012 FORMAT ('BMAJ=',F8.4,' BMIN=',F8.4,' BPA=',F6.1,
     *   '/Output beam')
 1013 FORMAT (A6,'/ plane',I6,' conv with',F10.5,' x',F10.5,' at',F6.1)
 1014 FORMAT (A6,'BLC=',7F6.0,'/BLC')
 1015 FORMAT (A6,'TRC=',7F6.0,'/TRC')
 1016 FORMAT (A6,'FACTOR=',1PE12.5,' / ',A)
 1017 FORMAT (A6,'OPCODE=''',A4,''' /Operation requested')
      END
      SUBROUTINE CONDIV (IERR)
C-----------------------------------------------------------------------
C   CONDIV divides the complex values in file WRK by MP2 and
C   writes the complex results into file MP2.
C   Output:
C   IERR   I    Return code, 0=>OK, otherwise error.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   FIND1, FIND2, FIND3, BIND1, BIND2, BIND3, BO,
     *   WIN(4), MX, MY, I, J, J1
      LOGICAL   T, F
      REAL   WT
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, WIN /1, 4*0/
C-----------------------------------------------------------------------
C                                        Open and init MP2
      CALL ZOPEN (LUNMP2, FIND1, MP2VOL, MP2FIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 1
         GO TO 990
         END IF
      MX = NFFTX / 2 + 1
      MY = NFFTY * 2
      CALL MINIT ('READ', LUNMP2, FIND1, MY, MX, WIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ', 1
         GO TO 990
         END IF
C                                        Open and init WRK
      CALL ZOPEN (LUNWRK, FIND2, WRKVOL, WRKFIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ', 2
         GO TO 990
         END IF
      CALL MINIT ('READ', LUNWRK, FIND2, MY, MX, WIN, BUFF2, BUFSZ2, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'READ', 2
         GO TO 990
         END IF
C                                        Open and init MP2
      CALL ZOPEN (LUNMP1, FIND3, MP2VOL, MP2FIL, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
      CALL MINIT ('WRIT', LUNMP1, FIND3, MY, MX, WIN, BUFF3, BUFSZ3, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, 'WRIT'
         GO TO 990
         END IF
C                                        Loop, dividing.
      DO 110 I = 1,MX
C                                        Read MP2
         CALL MDISK ('READ', LUNMP2, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'READ', 1
            GO TO 990
            END IF
C                                        Read WRK
         CALL MDISK ('READ', LUNWRK, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'READ', 2
            GO TO 990
            END IF
C                                        Write MP2
         CALL MDISK ('WRIT', LUNMP1, FIND3, BUFF3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR, 'WRIT'
            GO TO 990
            END IF
C                                        Divide row.
         DO 100 J = 1,MY,2
            J1 = J - 1
            WT = (BUFF1(BIND1+J1) * BUFF1(BIND1+J1) +
     *         BUFF1(BIND1+J) * BUFF1(BIND1+J))
            WT = 1.0 / MAX (1.0E-20,WT)
            BUFF3(BIND3+J1) = BUFF1(BIND1+J1) * BUFF2(BIND2+J1) +
     *         BUFF1(BIND1+J) * BUFF2(BIND2+J)
            BUFF3(BIND3+J1) = BUFF3(BIND3+J1) * WT
            BUFF3(BIND3+J) = BUFF1(BIND1+J1) * BUFF2(BIND2+J) -
     *         BUFF1(BIND1+J) * BUFF2(BIND2+J1)
            BUFF3(BIND3+J) = BUFF3(BIND3+J) * WT
 100        CONTINUE
 110     CONTINUE
C                                        Flush buffer.
      CALL MDISK ('FINI', LUNMP1, FIND3, BUFF3, BIND3, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR, 'FINI'
         GO TO 990
         END IF
C                                        Close files.
      CALL ZCLOSE (LUNMP2, FIND1, IERR)
      CALL ZCLOSE (LUNWRK, FIND2, IERR)
      CALL ZCLOSE (LUNMP1, FIND3, IERR)
      IERR = 0
      GO TO 999
C                                        Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONDIV: ERROR',I3,' OPEN FOR ',A4,' FILE',I2)
 1010 FORMAT ('CONDIV: ERROR',I3,' INIT FOR ',A4,' FILE',I2)
 1060 FORMAT ('CONDIV: ERROR',I3,1X,A4,'ING FILE',I2)
      END
      SUBROUTINE PLNGBL (IDISK, ICNO, CORN, JWIN, XOFF, YOFF, NOSCR,
     *   NX, NY, BUFF1, BUFF2, BUFSZ1, BUFSZ2, LUN1, LUN2, BLANKD, IRET)
C-----------------------------------------------------------------------
C   PLNGBL reads a selected portion of a selected plane parallel to the
C   front and writes it into a specified scratch file.  The output file
C   will be zero padded and a shift of the center may be specified.  If
C   the input window is unspecified (0's) and the output file is smaller
C   than the input file, the NX x NY region about position (MX/2+1-OFFX,
C   MY/2+1-OFFY) in the input map will be used where MX,MY is the size
C   of the input map.  NOTE: If both XOFF and/or YOFF and a window
C   (JWIN) which does not contain the whole map, XOFF and YOFF will
C   still be used to end-around rotate the region inside the window.
C     The image header is taken from the disk catalog *** Local version
C     handles  blanks
C   Inputs:
C      IDISK    I      Input image disk number.
C      ICNO     I      Input image catalog slot number.
C      CORN     I(7)   BLC in input image (1 & 2 ignored)
C      JWIN     I(4)   Window in plane.
C      XOFF     I      offset in cells in first dimension of the center
C                      from MX/2+1 (MX 1st dim. of input win.)
C      YOFF     I      offset in cells in second dimension of the center
C                      from MY/2+1 (MY 2nd dim. of input win.)
C      NOSCR    I      Scratch file number in common /CFILES/ for outpu.
C      NX       I      Dimension of output file in X
C      NY       I      Dimension of output file in Y
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer.
C      BUFSZ1   I      Size in AIPS bytes of BUFF1
C      BUFSZ2   I      Size in AIPS bytes of BUFF2
C      LUN1     I      Logical unit number for input file
C      LUN2     I      Logical unit number to use for output
C   Output:
C      BLANKD   L      There were blanks
C      IRET     I      Return error code, 0 => OK,
C                       1 = couldn't copy input CATBLK
C                       2 = wrong number of bits/pixel in input map.
C                       3 = input map has inhibit bits.
C                       4 = couldn't open output map file.
C                       5 = couldn't init input map.
C                       6 = couldn't init output map.
C                       7 = read error input map.
C                       8 = write error output map.
C                       9 = error computing block offset
C                       10 = output file too small.
C   Common:
C      /MAPHDR/ CATBLK  is set to the input file CATBLK.
C   Programmer: W. D. Cotton, May 1982.
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, CORN(7), JWIN(4), XOFF, YOFF, NOSCR, NX,
     *   NY, BUFSZ1, BUFSZ2, LUN1, LUN2, IRET
      REAL      BUFF1(*), BUFF2(*)
      LOGICAL   BLANKD
C
      CHARACTER PHNAME*48, IFILE*48
      INTEGER   IERR, WIN(4), FIND1, FIND2, BIND1, BIND2, BO, RBO, I4,
     *   IFIRST, ILAST, IOUT, KORN(7), IADD, INDEX, LIM, IOFF, LIM1, MX,
     *   MY, JOFF1, JOFF2, LIMIT, NUM, OFFX, OFFY, IWIN(4), MMX, MMY,
     *   I, I1, I2
      LOGICAL   T, F
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA RBO /1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      BLANKD = .FALSE.
      OFFX = XOFF
      OFFY = YOFF
      FIND1 = 0
      FIND2 = 0
C                                       Read input CATBLK
      CALL CATIO ('READ', IDISK, ICNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Determine mapsize
      MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Check defaults on IWIN
      CALL COPY (4, JWIN, IWIN)
      IF ((MX.GT.NX) .AND. ((IWIN(1).EQ.0) .OR. (IWIN(3).EQ.0))) THEN
         IWIN(1) = (MX/2+1) - (NX/2) - OFFX
         IWIN(3) = (MX/2+1) + (NX/2-1) - OFFX
         OFFX = 0
         END IF
      IF ((MY.GT.NY) .AND. ((IWIN(2).EQ.0).OR. (IWIN(4).EQ.0))) THEN
         IWIN(2) = (MY/2+1) - (NY/2) - OFFY
         IWIN(4) = (MY/2+1) + (NY/2-1) - OFFY
         OFFY = 0
          END IF
      IF (IWIN(1).LE.0) IWIN(1) = 1
      IF (IWIN(2).LE.0) IWIN(2) = 1
      IF ((IWIN(3).LE.0) .OR. (IWIN(3).GT.MX)) IWIN(3) = MX
      IF ((IWIN(4).LE.0) .OR. (IWIN(4).GT.MY)) IWIN(4) = MY
C                                        Determine input window size.
      MMX = IWIN(3) - IWIN(1) + 1
      MMY = IWIN(4) - IWIN(2) + 1
C                                        Determine first and last
C                                        output rows for read.
      IFIRST = ((NY - MMY) / 2.0) + 1.6
      ILAST = IFIRST + (IWIN(4) - IWIN(2))
C                                        Check defaults on CORN
      IERR = 0
      DO 45 I4 = 1,KICTPN
         KORN(I4) = 1
         IF (I4.LE.CATBLK(KIDIM)) THEN
            KORN(I4) = MAX (CORN(I4), 1)
            IF (CATBLK(KINAX+I4-1).LE.1) KORN(I4) = 1
            IF (CATBLK(KINAX+I4-1).LT.KORN(I4)) IERR = 2
            END IF
 45      CONTINUE
C                                       Set input BLOCK offset.
      IF (IERR.EQ.0) THEN
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BO, IERR)
         BO = BO + 1
         END IF
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1045) IERR
         GO TO 990
         END IF
C                                       Set window for output.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = NX
      WIN(4) = NY
C                                        Make sure input window .le.
C                                        output file size.
      IF ((NX.LT.MMX) .OR. (NY.LT.MMY)) THEN
         IRET = 10
         WRITE (MSGTXT,1050) NX, NY, MMX, MMY
         GO TO 990
         END IF
      INDEX = - OFFY
      IF (INDEX.LT.0) INDEX = MMY - OFFY
      WIN(2) = INDEX + 1
C                                       If OFFX positive
      LIM = OFFX
      JOFF1 = MMX - OFFX - 1
      JOFF2 = - OFFX - 1
      IADD = (NX - MMX) / 2.0 + 0.6
C                                       If OFFX .LE. 0
      IF ((OFFX.LE.0)) THEN
         LIM = MMX + OFFX
         JOFF1 = - OFFX - 1
         JOFF2 = - MMX - OFFX - 1
         END IF
      JOFF1 = JOFF1 + IADD
      JOFF2 = JOFF2 + IADD
      LIMIT = MMX
      LIM = MIN (LIM, LIMIT)
      LIM1 = LIM + 1
C                                       Open output map file.
      CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, FIND2, SCRVOL(NOSCR), PHNAME, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       Open input file.
      CALL ZPHFIL ('MA', IDISK, ICNO, 1, IFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, IDISK, IFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1065) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       Init files.
      CALL MINIT ('READ', LUN1, FIND1, MX, MY, IWIN, BUFF1, BUFSZ1, BO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1070) IERR
         GO TO 970
         END IF
      CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2, RBO,
     *   IERR)
      IOUT = WIN(2) - 1
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1080) IERR
         GO TO 970
         END IF
C                                       Finally do what you're here for
      DO 200 I4 = 1,NY
         IOUT = IOUT + 1
C                                       Restart at first of output file
C                                       Finish write.
         IF (IOUT.GT.NY) THEN
            CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
            IF (IERR.NE.0) THEN
               IRET = 8
               WRITE (MSGTXT,1090) IERR, NY
               GO TO 970
               END IF
            INDEX = 1
            WIN(4) = WIN(2) - 1
            WIN(2) = 1
            CALL MINIT ('WRIT', LUN2, FIND2, NX, NY, WIN, BUFF2, BUFSZ2,
     *         RBO, IERR)
            IOUT = WIN(2)
            IF (IERR.NE.0) THEN
               IRET = 6
               WRITE (MSGTXT,1080) IERR
               GO TO 970
               END IF
            END IF
C                                       Write real map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT,1090) IERR, I4
            GO TO 970
            END IF
C                                       Zero fill output row.
         CALL RFILL (NX, 0.0, BUFF2(BIND2))
C                                       Check if data for this row.
         IF ((IOUT.GE.IFIRST) .AND. (IOUT.LE.ILAST)) THEN
C                                       Read map row.
            CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               IRET = 7
               WRITE (MSGTXT,1105) IERR, I4
               GO TO 970
               END IF
C                                       Move to output buffer.
            IOFF = BIND2 + JOFF1
            DO 120 I = 1,LIM
               I1 = BIND1 + I - 1
               I2 = IOFF + I
               IF (BUFF1(I1).EQ.FBLANK) THEN
                  BLANKD = .TRUE.
                  BUFF2(I2) = 0.0
               ELSE
                  BUFF2(I2) = BUFF1(I1)
                  END IF
 120           CONTINUE
            IF (LIM.LT.LIMIT) THEN
               IOFF = BIND2 + JOFF2
               NUM = LIMIT - LIM1 + 1
               DO 140 I = 1,NUM
                  I1 = BIND1 + LIM1 - 1 + I - 1
                  I2 = IOFF + LIM1 + I - 1
                  IF (BUFF1(I1).EQ.FBLANK) THEN
                     BLANKD = .TRUE.
                     BUFF2(I2) = 0.0
                  ELSE
                     BUFF2(I2) = BUFF1(I1)
                     END IF
 140              CONTINUE
               END IF
            END IF
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1090) IERR, NY
         END IF
C                                       Close real map file.
 970  IF (IRET.NE.0) CALL MSGWRT (8)
      IF (FIND1.GT.0) CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       Close integer map file.
 980  IF (FIND2.GT.0) CALL ZCLOSE (LUN2, FIND2, IERR)
      GO TO 999
C                                       message only
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLNGBL: ERROR',I3,' READING INPUT CATBLK')
 1045 FORMAT ('PLNGBL: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1050 FORMAT ('PLNGBL: OUTPUT MAP TOO SMALL',2I6,' .LT. ',2I6)
 1060 FORMAT ('PLNGBL: ERROR',I3,' OPENING OUTPUT FILE')
 1065 FORMAT ('PLNGBL: ERROR',I3,' OPENING INPUT FILE')
 1070 FORMAT ('PLNGBL: CANNOT INIT INPUT MAP, ERROR',I3)
 1080 FORMAT ('PLNGBL: CANNOT INIT OUTPUT MAP, ERROR',I3)
 1090 FORMAT ('PLNGBL: WRITE ERROR',I3,' ROW ',I5)
 1105 FORMAT ('PLNGBL: READ ERROR',I3,' ROW ',I5)
      END
      SUBROUTINE PLNPBL (IDISK, ICNO, ICORN, ODISK, OCNO, OCORN, JWIN,
     *   NOSCR, NX, NY, BUFF1, BUFF2, BUFF3, BUFSZ1, BUFSZ2, LUN1, LUN2,
     *   LUN3, IRET)
C-----------------------------------------------------------------------
C   PLNPBL writes a subregion of a scratch file image into a cataloged
C   image.  It differs from PLNPUT in that it also reads a catalogued
C   image and, where it is blanked, blanks the output.
C   Input:
C      IDISK    I      Blanking image disk number
C      ICNO     I      Blanking image catalog slot number
C      ICORN    I(7)   BLC in blanking image
C      ODISK    I      Output image disk number.
C      OCNO     I      Output image catalog slot number.
C      OCORN    I(7)   BLC in Output image (1 & 2 ignored)
C      JWIN     I(4)   Window in plane in input image.
C      NOSCR    I      Scratch file number in common /CFILES/ for
C                      input scratch file.
C      NX       I      X-dimension of input file.
C      NY       I      Y-dimension of input file.
C      BUFF1    R(*)   Work buffer
C      BUFF2    R(*)   Work buffer.
C      BUFSZ1   I      Size in bytes of BUFF1.
C      BUFSZ2   I      Size in bytes of BUFF2
C      LUN1     I      Logical unit number to use.
C      LUN2     I      Second loical unit number to use.
C   Output:
C      IRET     I      Return error code: 0 => OK
C                         1 = couldn't read output CATBLK.
C                         2 = Output bits/pixel not allowed.
C                         3 = Output and input windows not same.
C                         4 = couldn't open input map file.
C                         5 = couldn't init output map.
C                         6 = couldn't init input map.
C                         7 = read error input map.
C                         8 = write error output map.
C                         9 = error writing header to catalog
C                        10 = error computing block offset.
C   Commons:
C      CATBLK in /MAPHDR/ is used as the map header.
C             Of particular importance is the data max/min values
C             which must apply to the map.  As this is read from the
C             catalog it must be updated by a call to CATIO etc.
C             before calling this routine.
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, ICORN(7), ODISK, OCNO, OCORN(7), JWIN(4),
     *   NOSCR, NX, NY, BUFSZ1, BUFSZ2, LUN1, LUN2, LUN3, IRET
      REAL      BUFF1(*), BUFF2(*), BUFF3(*)
C
      CHARACTER OFILE(6)*48
      INTEGER   IERR, WIN(4), OBO, I4, OWIN(4), KORN(7), FIND1, FIND2,
     *   BIND1, BIND2, MX, MY, NNX, NNY, RBO, I, BOTEMP, IBO, IWIN(4),
     *   IX, IY, J, FIND3, BIND3
      LOGICAL   T, F, OPEN1, OPEN2, OPEN3, BLNKD
      REAL      XMAX, XMIN
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA RBO /1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      OPEN1 = F
      OPEN2 = F
      OPEN3 = F
C                                       Read input CATBLK.
      CALL CATIO ('READ', IDISK, ICNO, CATBLK, 'REST', BUFF3, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) 'INPUT', IERR
         GO TO 980
         END IF
      IX = CATBLK(KINAX)
      IY = CATBLK(KINAX+1)
      IWIN(1) = ICORN(1)
      IWIN(2) = ICORN(2)
      BLNKD = .FALSE.
C                                       Set input BLOCK offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), ICORN(3), BOTEMP, IERR)
      IBO = BOTEMP + 1
C                                       Read output CATBLK.
      CALL CATIO ('READ', ODISK, OCNO, CATBLK, 'REST', BUFF2, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) 'OUTPUT', IERR
         GO TO 980
         END IF
C                                       Determine max, min and mapsize.
      XMAX = CATR(KRDMX)
      XMIN = CATR(KRDMN)
      MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Set window for maps.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = MX
      WIN(4) = MY
      IWIN(3) = IWIN(1) + MX - 1
      IWIN(4) = IWIN(2) + MY - 1
C                                        Check defaults on OWIN
      CALL COPY (4, JWIN, OWIN)
      IF (OWIN(1).LE.0) OWIN(1) = 1
      IF (OWIN(2).LE.0) OWIN(2) = 1
      IF ((OWIN(3).LE.0) .OR. (OWIN(3).GT.(MX+OWIN(1)-1)))
     *   OWIN(3) = MX + OWIN(1) - 1
      IF ((OWIN(4).LE.0) .OR. (OWIN(4).GT.(MY+OWIN(2)-1)))
     *   OWIN(4) = MY + OWIN(2) - 1
C                                        Check defaults on OCORN
      DO 30 I4 = 1,KICTPN
         KORN(I4) = MAX (OCORN(I4), 1)
 30      CONTINUE
C                                       Set output BLOCK offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BOTEMP, IERR)
      OBO = BOTEMP + 1
      IF (IERR.NE.0) THEN
         IRET = 10
         WRITE (MSGTXT,1030) IERR
         GO TO 980
         END IF
C                                        Check that input and output
C                                        windows are the same size.
      NNX = OWIN(3) - OWIN(1) + 1
      NNY = OWIN(4) - OWIN(2) + 1
      IF ((NNX.NE.MX) .OR. (NNY.NE.MY)) THEN
         IRET = 3
         WRITE (MSGTXT,1035) NNX, NNY, MX, MY
         GO TO 980
         END IF
C                                       Open input file.
      CALL ZPHFIL ('MA', IDISK, ICNO, 1, OFILE, IERR)
      CALL ZOPEN (LUN3, FIND3, IDISK, OFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1040) IERR, 'OPEN INPUT'
         GO TO 980
         END IF
      OPEN3 = T
C                                       Open output file.
      CALL ZPHFIL ('MA', ODISK, OCNO, 1, OFILE, IERR)
      CALL ZOPEN (LUN2, FIND2, ODISK, OFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1040) IERR, 'OPEN OUTPUT'
         GO TO 980
         END IF
      OPEN2 = T
C                                       Open input map file.
      CALL ZPHFIL ('SC', SCRVOL(NOSCR), SCRCNO(NOSCR), 1, OFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, SCRVOL(NOSCR), OFILE, T, F, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1040) IERR, 'OPEN SCRATCH'
         GO TO 980
         END IF
      OPEN1 = T
C                                       Init files.
      CALL MINIT ('READ', LUN1, FIND1, NX, NY, OWIN, BUFF1, BUFSZ1, RBO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 5
         WRITE (MSGTXT,1040) IERR, 'INIT SCRATCH'
         GO TO 980
         END IF
      CALL MINIT ('WRIT', LUN2, FIND2, MX, MY, WIN, BUFF2, BUFSZ2, OBO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1040) IERR, 'INIT OUTPUT'
         GO TO 980
         END IF
      CALL MINIT ('READ', LUN3, FIND3, IX, IY, IWIN, BUFF3, BUFSZ2, IBO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1040) IERR, 'INIT INPUT'
         GO TO 980
         END IF
C                                       Finally do what you're here for
      DO 200 I = 1,MY
C                                       Read map row.
         CALL MDISK ('READ', LUN3, FIND3, BUFF3, BIND3, IERR)
         IF (IERR.NE.0) THEN
            IRET = 7
            WRITE (MSGTXT,1100) IERR, I, 'READ INPUT'
            GO TO 980
            END IF
C                                       Read map row.
         CALL MDISK ('READ', LUN1, FIND1, BUFF1, BIND1, IERR)
         IF (IERR.NE.0) THEN
            IRET = 7
            WRITE (MSGTXT,1100) IERR, I, 'READ SCRATCH'
            GO TO 980
            END IF
C                                       Write output map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT,1100) IERR, I, 'WRITE OUTPUT'
            GO TO 980
            END IF
C                                       Move to output buffer.
         DO 120 J = 1,MX
            IF (BUFF3(BIND3+J-1).EQ.FBLANK) THEN
               BUFF2(BIND2+J-1) = FBLANK
               BLNKD = .TRUE.
            ELSE
               BUFF2(BIND2+J-1) = BUFF1(BIND1+J-1)
               XMAX = MAX (XMAX, BUFF2(BIND2+J-1))
               XMIN = MIN (XMIN, BUFF2(BIND2+J-1))
               END IF
 120        CONTINUE
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1100) IERR, MY, 'WRITE OUTPUT'
         END IF
C                                       Update catlg of output map
 980  IF (IRET.EQ.0) THEN
         CATR(KRDMX) = XMAX
         CATR(KRDMN) = XMIN
         IF (BLNKD) CATR(KRBLK) = FBLANK
         CALL CATIO ('UPDT', ODISK, OCNO, CATBLK, 'REST', BUFF2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 9
            WRITE (MSGTXT,1980) IERR
            END IF
         END IF
C                                       error message if any
      IF (IRET.NE.0) CALL MSGWRT (8)
C                                       close files
      IF (OPEN1) CALL ZCLOSE (LUN1, FIND1, IERR)
      IF (OPEN2) CALL ZCLOSE (LUN2, FIND2, IERR)
      IF (OPEN3) CALL ZCLOSE (LUN3, FIND3, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLNPBL: ERROR',I3,' READING ',A,' CATBLK')
 1030 FORMAT ('PLNPBL: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1035 FORMAT ('PLNPBL: INPUT WINDOW SIZE=',2I5,' OUTPUT=',2I5)
 1040 FORMAT ('PLNPBL: ERROR',I4,1X,A,' FILE')
 1100 FORMAT ('PLNPBL: ERROR',I4,' ROW ',I5,1X,A,' FILE')
 1980 FORMAT ('PLNPBL: ERROR',I3,' UPDATING CATALOGED MAP HEADER')
      END
      SUBROUTINE PASS1 (IDIR, FULL, LUN, VOL, FIL, BO, XBUFF1, BUFSZ1,
     *   XBUFF2, BUFSZ2, NX, NY, IERR)
C-----------------------------------------------------------------------
C  This is a private version of the system PASS1.
C  This version includes some changes required for adding
C       cross correlation option additionally to convolution.
C   PASS1 = first pass of a 2-dimensional disk-based AP FFT  Several
C   rows are loaded into the AP, FFTed and then partially transposed
C   and written on the work file.  If the entire map will fit into the
C   AP the intermediate results are not written to the WORK file.
C   Note: for Complex to Real transforms, NX/2+1 rows are expected.
C   Input:
C     IDIR        I    -1 = reverse transform
C                       1 = forward transform, keep real part only.
C                       2 = forward transform, keep amplitudes only,
C                       3 = forward transform, keep full complex.
C                       (In this step, no difference between 1,2,3)
C     FULL        L    If .TRUE. then COMPLEX to COMPLEX transform,
C                      otherwise, half plane complex to real or reverse
C     LUN(3)      I    LUNs for files
C     VOL(3)      I    Volume numbers for the files.
C     FIL(3)      C*48 Physical names for the files.
C     BO(3)       I    Block offsets for the files.
C     XBUFF1(),XBUFF2()  R    Work buffers for I/O
C     BUFSZ1,BUFSZ2  I    Size in bytes for XBUFF1 and XBUFF2
C     NX,NY          I    Number of grid cells in X and Y of maps.
C   Output:
C     IERR          I    Return error code, 0=>OK, otherwise failed.
C                        3=>image too small to FFT
C     Partially transformed and transposed file left in the AP or
C     on the WORK file if necessary.
C-----------------------------------------------------------------------
      CHARACTER    FIL(3)*48, BADOP*4
      INTEGER   IDIR, LUN(3), VOL(3), BO(3), BUFSZ1, BUFSZ2,
     *   NX, NY, IHALF, IERR, IWIN(4), APSIZ
      INTEGER   IXOLD, IXNEW
      REAL      XBUFF1(*), XBUFF2(*)
      INTEGER   FIND1, FIND2, BIND1, BIND2, BADFIL, WIN(4), IER,
     *   J, K, MPASS, NCOL, NROW, JAPWRD, ITEMP, I, ILIM, IT,
     *   HALFNX, INDEX, ONENX, TWONY, ONEROW, JNDEX, ONENY, NWORD, JDIR,
     *   KNDEX, LNDEX
      INTEGER WINC(4)
      DOUBLE PRECISION XNDEX
      LOGICAL   FULL, MAP, WAIT, EXCL
      LOGICAL   LIMAC
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
      INCLUDE 'INCS:PMAD.INC'
      REAL      XTEMP(MAXIMG)
      DATA MAP, WAIT, EXCL /2*.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Use the artificial IERR=11
C                                       to introduce LIMAC = TRUE
      IF (IERR .EQ. 11) THEN
         LIMAC = .TRUE.
      ELSE
         LIMAC = .FALSE.
         END IF
      IERR = 0
C                                       Get power of 2 size of AP.
      JAPWRD = PSAPNW
      CALL AP2SIZ (JAPWRD, APSIZ)
C                                       Make sure min. dim .ge. 4
      IF ((NX.LT.4) .OR. (NY.LT.4)) THEN
         WRITE (MSGTXT,1000) NX,NY
         CALL MSGWRT (8)
         IERR = 3
         GO TO 999
         END IF
C                                       Determine number of AP loads
C                                       required
      XNDEX = APSIZ
      XNDEX = NX / XNDEX
      IF (FULL) XNDEX = XNDEX * 2.0D0
      MPASS = NY * XNDEX + 0.1
      MPASS = MAX (MPASS, 1)
      HALFNX = NX / 2
      ONENX = NX
      ONENY = NY
      NWORD = 0
C                                       Open input and work file.
      BADFIL = 1
      BADOP = 'OPEN'
      CALL ZOPEN (LUN(1), FIND1, VOL(1), FIL(1), MAP, EXCL, WAIT,
     *   IERR)
      IF (IERR.GT.0) GO TO 900
C                                        Writeonly on multiple AP loads
      IF (MPASS.GT.1) THEN
C                                        DEBUG WARNING MESSAGE
         MSGTXT = '***********************************************'
         CALL MSGWRT (9)
         MSGTXT = 'PASS1: USING MORE THAN ONE PASS!'
         CALL MSGWRT (9)
         MSGTXT = '***********************************************'
         CALL MSGWRT (9)
         BADFIL = 2
         CALL ZOPEN (LUN(2), FIND2, VOL(2), FIL(2), MAP, EXCL, WAIT,
     *      IERR)
         IF (IERR.GT.0) GO TO 900
         END IF
      BADOP = 'INIT'
C                                       REAL TO COMPLEX, scramble data.
      IF ((IDIR.GE.0) .AND. (.NOT.FULL)) THEN
C                                       INIT output file.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
C                                        Write only for multiple AP
C                                        loads.
         IF (MPASS.GT.1) THEN
            CALL MINIT ('WRIT', LUN(2), FIND2, NX, NY, WIN, XBUFF2,
     *         BUFSZ2, BO(2), IERR)
            IF (IERR.NE.0) GO TO 900
            WIN(2) = NY / 2 + 1
            END IF
C                                       NROW = no. rows loaded at once.
         NROW = APSIZ / NX
         NROW = MIN (NROW, NY)
         ONEROW = NROW
C                                       Load second half first.
C                                       Jump to here for first half.
 40      CONTINUE
C                                       INIT Input file.
C                                       Inverse WIN (Y only) for LIMAC
            IF (LIMAC) THEN
               WINC(1) = WIN(1)
               WINC(2) = NY - WIN(2) + 1
               WINC(3) = WIN(3)
               WINC(4) = NY - WIN(4) + 1
            ELSE
               WINC(1) = WIN(1)
               WINC(2) = WIN(2)
               WINC(3) = WIN(3)
               WINC(4) = WIN(4)
               END IF
            CALL MINIT ('READ', LUN(1), FIND1, NX, NY, WINC, XBUFF1,
     *         BUFSZ1, BO(1), IERR)
            IF (IERR.NE.0) THEN
               BADOP = 'INIT'
               BADFIL = 1
               GO TO 900
               END IF
            ILIM = MPASS / 2
            ILIM = MAX (ILIM, 1)
            DO 120 I = 1,ILIM
               DO 80 J = 1,NROW
                  CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     IT = (I - 1) * NROW + WIN(3) + J - 1
                     BADOP = 'READ'
                     BADFIL = 1
                     GO TO 900
                     END IF
C                                       mirror the columns for cros
C                                       correlation (LIMAC)
                  IF (LIMAC) THEN
C                                       store the row XBUFF1
                     DO 55 IXOLD = 1,NX
                        XTEMP(IXOLD) = XBUFF1(IXOLD+BIND1)
 55                     CONTINUE
C                                       mirror XBUFF1
                     DO 60 IXOLD = 1,NX
                        IXNEW = NX - IXOLD + 1
                        XBUFF1(IXNEW+BIND1) = XTEMP(IXOLD)
 60                     CONTINUE
                     END IF
C
                  K = J
C                                       Following to scramble input
C                                       if MPASS = 1.
                  IF (MPASS.LE.1) THEN
                     K = J - NY / 2
                     IF (J.LE.NY/2) K = J + NY / 2
                     END IF
                  INDEX = (K - 1) * ONENX
C                                       Load data in AP, scramble.
                  CALL QWR
                  CALL QPUT (XBUFF1(BIND1+NX/2), INDEX, HALFNX, 2)
                  JNDEX = INDEX + HALFNX
                  CALL QPUT (XBUFF1(BIND1), JNDEX, HALFNX, 2)
                  CALL QWD
C                                       Row loaded, do FFT.
                  CALL QRFFT (INDEX, ONENX, 1)
 80               CONTINUE
               CALL QWR
C                                       Transpose.
               IHALF = HALFNX
               CALL APXPOS (NROW, IHALF, 0, IERR)
               IF (IERR.NE.0) GO TO 990
C                                       Write out from AP to work file.
C                                       Writeonly on multiple AP loads.
               IF (MPASS.GT.1) THEN
                  DO 110 J = 1,NROW
                     CALL MDISK ('WRIT', LUN(2), FIND2, XBUFF2, BIND2,
     *                  IERR)
                     IF (IERR.NE.0) THEN
                        IT = WIN(2) + I + J - 1
                        BADOP = 'WRIT'
                        BADFIL = 2
                        GO TO 900
                        END IF
                     INDEX = (J - 1) * ONENX
                     CALL QGET (XBUFF2(BIND2), INDEX, ONENX, 2)
                     CALL QWD
 110                 CONTINUE
C                                       Roll AP if necessary.
C                                       Roll whole AP memory if
C                                       MPASS=1 otherwise none.
                  NWORD = 0
                  IF (MPASS.EQ.1) NWORD = APSIZ
                  CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       Reset NWORD
                  NWORD = 0
                  END IF
 120           CONTINUE
C                                       Check to see if finished.
            IF (WIN(2).NE.1) THEN
               WIN(2) = 1
               GO TO 40
               END IF
C                                       Writeonly on multiple AP loads
         IF (MPASS.GT.1) THEN
            BADFIL = 2
            BADOP = 'FINI'
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
C                                       COMPLEX to REAL or FULL
C                                       COMPLEX transform.
      ELSE
C                                       INIT read and write files.
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NY * 2
         WIN(4) = NX / 2
         IF (FULL) WIN(4) = NX
         BADFIL = 1
         NCOL = APSIZ / (NY * 2)
         ITEMP = NX / 2
         IF (FULL) ITEMP = NX
         NCOL = MIN (NCOL, ITEMP)
         TWONY = 2 * NY
         JDIR = -1
         IF (FULL) JDIR = IDIR
C                                       If Complex=>real then do last
C                                       row (NX/2+1) to pack with first.
         IF (.NOT.FULL) THEN
            BADOP = 'INIT'
            IWIN(1) = 1
            IWIN(3) = NY * 2
            IWIN(2) = (NX / 2) + 1
            IWIN(4) = (NX / 2) + 1
            CALL MINIT ('READ', LUN(1), FIND1, IWIN(3), IWIN(4), IWIN,
     *         XBUFF1, BUFSZ1, BO(1), IERR)
            IF (IERR.NE.0) GO TO 900
            BADOP = 'READ'
            INDEX = ONENY * 2
            JNDEX = INDEX
            CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1, IERR)
            IF (IERR.NE.0) THEN
               IT = NCOL
               GO TO 900
               END IF
            CALL QWR
            CALL QPUT (XBUFF1(BIND1), JNDEX, TWONY, 2)
            CALL QWD
C                                       FFT row
            CALL QCFFT (INDEX, ONENY, JDIR)
            END IF
         BADOP = 'INIT'
         CALL MINIT ('READ', LUN(1), FIND1, WIN(3), WIN(4), WIN, XBUFF1,
     *      BUFSZ1, BO(1), IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Writeonly on multiple AP loads
         IF (MPASS.GT.1) THEN
            BADFIL = 2
            CALL MINIT ('WRIT', LUN(2), FIND2, WIN(3), WIN(4), WIN,
     *         XBUFF2, BUFSZ2, BO(2), IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         DO 600 I = 1,MPASS
            DO 560 J = 1,NCOL
               INDEX = (J-1) * 2 * ONENY
               JNDEX = INDEX
               CALL MDISK ('READ', LUN(1), FIND1, XBUFF1, BIND1, IERR)
               IF (IERR.NE.0) THEN
                  IT = I + J - 1
                  BADFIL = 1
                  BADOP = 'READ'
                  GO TO 900
                  END IF
               CALL QWR
               CALL QPUT (XBUFF1(BIND1), JNDEX, TWONY, 2)
               CALL QWD
C                                       Row loaded, do FFT.
               CALL QCFFT (INDEX, ONENY, JDIR)
               IF ((I.GT.1) .OR. (J.GT.1) .OR. FULL) GO TO 560
C                                       Pack first (real) and
C                                       last (imag.)
                  LNDEX = 1
                  KNDEX = TWONY
                  CALL QVMOV (KNDEX, 2, LNDEX, 2, ONENY)
 560           CONTINUE
C                                       Transpose.
            CALL APXPOS (NCOL, NY, 0, IERR)
            IF (IERR.NE.0) GO TO 990
C                                       Writeonly on multiple AP loads
            IF (MPASS.GT.1) THEN
C                                       Write out AP to work file.
               DO 590 J = 1,NCOL
                  INDEX = (J - 1) * ONENY * 2
                  JNDEX = INDEX
                  CALL MDISK ('WRIT', LUN(2), FIND2, XBUFF2, BIND2,
     *               IERR)
                  IF (IERR.NE.0) THEN
                     IT = I + J - 1
                     BADOP = 'WRIT'
                     BADFIL = 2
                     GO TO 900
                     END IF
                  CALL QGET (XBUFF2(BIND2), JNDEX, TWONY, 2)
                  CALL QWD
 590              CONTINUE
               END IF
C                                       Roll AP if necessary.
C                                       Roll whole AP memory if
C                                       MPASS=1 otherwise none.
            NWORD = 0
            IF (MPASS.EQ.1) NWORD = APSIZ
            CALL QROLL (NWORD, XBUFF1, BUFSZ1, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Reset NWORD
            NWORD = 0
 600        CONTINUE
C                                       Finish write.
C                                       Writeonly on multiple AP loads
         IF (MPASS.GT.1) THEN
            CALL MDISK ('FINI', LUN(2), FIND2, XBUFF2, BIND2, IERR)
            IF (IERR.NE.0) THEN
               BADFIL = 2
               BADOP = 'FINI'
               GO TO 900
               END IF
            END IF
         END IF
      GO TO 990
C                                       Errors
 900  WRITE (MSGTXT,1900) BADOP, IERR, FIL(BADFIL)
      IF ((BADOP.EQ.'READ') .OR. (BADOP.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1901) BADOP, IERR, IT
         CALL MSGWRT (8)
         WRITE (MSGTXT,1902) FIL(BADFIL)
         END IF
      CALL MSGWRT (8)
C                                       Close files.
 990  IF ((MPASS.GT.1) .AND. (BADOP.NE.'OPEN')) CALL ZCLOSE (LUN(2),
     *   FIND2, IER)
      IF ((BADOP.NE.'OPEN') .OR. (BADFIL.EQ.2)) CALL ZCLOSE (LUN(1),
     *   FIND1, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PASS1: NX OR NY (',I5,',',I5,') LESS THAN 4')
 1900 FORMAT ('PASS1: ',A4,' ERROR',I7,' FILE ',A)
 1901 FORMAT ('PASS1: ',A4,' ERROR',I7,' ROW',I5)
 1902 FORMAT ('   FILE = ',A)
      END
C
      SUBROUTINE POL2DI(XARG, YARG, FUNC, NX, NY, X0, Y0, MAXP, IRET)
C---------------------------------------------------------------------
C     Routine to fit a polinomial
C     C1*X^2 +C2*Y^2 + C3*X*Y + C4*X + C5*Y + C6
C     to the data by Least Square method.
C     Routine estimates position (X0, Y0) and value MAXP of the
C     fitted two dimensional elliptical poraballoid
C   Input:
C      XARG    R(*)  Array of data argument at X
C      YARG    R(*)  Array of data argument at Y
C      FUNC    R(*)  Array of data function
C                    Argument of ARG, FUNC is
C                    INDEX = (IX-1)*NY + IY; IX=1,2..NX  IY=1,2..NY
C      NX      I     Number of points at arrays ARG, FUNC along X
C      NY      I     Number of points at arrays ARG, FUNC along Y
C   Output:
C      X0      R     X position of maximum
C      Y0      R     Y position of maximum
C      MAXP    R     Maximum value
C      IRET    I     Error; 0 => OK
C-----------------------------------------------------------------------
      INTEGER NX, NY, IRET
      REAL    XARG(*), YARG(*), FUNC(*), X0, Y0, MAXP
      INTEGER NFIT, NEQUAT, INDEX
      INTEGER IX, IY, IFIT, KFIT, IKFIT
      REAL      R(20), MATR(400), COEFF(20), NOBS, SUM, SSQ,
     *   VX(20), SSQRES, VARY, FIT, ARGX, ARGY, VARRES, FITPAR(6),
     *   C1, C2, C3, C4, C5, C6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       number of fit parameters:
C                                       COEFFs near X^2,Y^2,XY,X,Y,C
      NFIT = 6
      NEQUAT = NX*NY
C
      IRET = 0
C                                       Force result vector R(NFIT),
C                                       matrix M(NFIT*NFIT) to zero
      DO 20 IFIT = 1, NFIT
         R(IFIT) = 0.0
         DO 10 KFIT = 1, NFIT
            IKFIT = IFIT + (KFIT - 1)*NFIT
            MATR (IKFIT) = 0.0
 10         CONTINUE
 20      CONTINUE
      SUM = 0.0
      SSQ = 0.0
      NOBS = 0.0
C                                       Prepare result vector R(NFIT)
C                                       and matrix MATR(NFIT*NFIT)
C                                       for routine LEASQR
      DO 80 IX = 1, NX
         DO 70 IY = 1, NY
            INDEX = (IX-1)*NY + IY
            ARGX = XARG(INDEX)
            ARGY = YARG(INDEX)
            NOBS = NOBS + 1
            SUM = SUM + FUNC(INDEX)
            SSQ = SSQ + FUNC(INDEX)*FUNC(INDEX)
C
            DO 60 IFIT = 1, NFIT
               IF (IFIT .EQ. 1) COEFF(IFIT) = ARGX*ARGX
               IF (IFIT .EQ. 2) COEFF(IFIT) = ARGY*ARGY
               IF (IFIT .EQ. 3) COEFF(IFIT) = ARGX*ARGY
               IF (IFIT .EQ. 4) COEFF(IFIT) = ARGX
               IF (IFIT .EQ. 5) COEFF(IFIT) = ARGY
               IF (IFIT .EQ. 6) COEFF(IFIT) = 1

               R(IFIT) = R(IFIT) + FUNC(INDEX) * COEFF(IFIT)
C                                       calculate upper/right
C                                       triangle of MATR
               DO 40 KFIT = IFIT, NFIT
                  IF (KFIT .EQ. 1) COEFF(KFIT) = ARGX*ARGX
                  IF (KFIT .EQ. 2) COEFF(KFIT) = ARGY*ARGY
                  IF (KFIT .EQ. 3) COEFF(KFIT) = ARGX*ARGY
                  IF (KFIT .EQ. 4) COEFF(KFIT) = ARGX
                  IF (KFIT .EQ. 5) COEFF(KFIT) = ARGY
                  IF (KFIT .EQ. 6) COEFF(KFIT) = 1
                  IKFIT = IFIT + (KFIT-1)*NFIT
                  MATR(IKFIT) = MATR(IKFIT) +
     *               COEFF(IFIT) * COEFF(KFIT)
 40               CONTINUE
 60            CONTINUE
 70         CONTINUE
 80      CONTINUE
C
      CALL LEASQR (NFIT, NOBS, SUM, SSQ, R, MATR, FITPAR, VX, SSQRES,
     *   VARRES, VARY, FIT, IRET)
      VARRES = SQRT(VARRES)
C                                       find solution for X0, Y0 as
C                                       a function of the found polinom
C                                       coefficients C1,..C5
C                                       see LK's  analysis
      C1 = FITPAR(1)
      C2 = FITPAR(2)
      C3 = FITPAR(3)
      C4 = FITPAR(4)
      C5 = FITPAR(5)
      C6 = FITPAR(6)
C                                       see LK's analysis
      X0 = (C3*C5 - 2*C2*C4) / (4*C1*C2 - C3*C3)
      Y0 = (C3*C4 - 2*C1*C5) / (4*C1*C2 - C3*C3)
C                                       Solution for the maximum
      MAXP = C6 - C1*X0*X0 - C2*Y0*Y0 - C3*X0*Y0
C
 999  RETURN
      END
      SUBROUTINE ARRAY (NXX, NYY, MAXS, XARG, YARG, ARR, JERR)
C-----------------------------------------------------------------------
C  ARRAY reads the determined rectangular portion of the given image,
C  and store it at the output one dimensional array
C   Inputs:
C   NXX         I       Number of points at the arrays along X (BOX(1)
C   NYY         I       Number of points at the arrays along Y (BOX(2)
C   MAXS        I       Maximum size of the arrays
C
C   Output:
C   XARG        R(*)    Array of Xs of the data
C   YARG        R(*)    Array of Ys of the data
C   ARR         R(*)    Array of data at the subimage

C   JERR        I    Return error code. 0 => OK, error otherwise.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      REAL  ARR(*), XARG(*), YARG(*)
      INTEGER   NXX, NYY, MAXS
      CHARACTER PTYPE*2
      LOGICAL   F
      INTEGER   JERR, DLUN, DIND, CNO, IX, IY, INDEX, BUFF(MABFSS),
     *   WIN(4), HALFX, HALFY, JBUFSZ, OFBLK, ININD
      REAL  ARGX, ARGY, RBUFF(MABFSS), VALM, VAL
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ RBUFF
      EQUIVALENCE  (BUFF, RBUFF)
C
      DATA DLUN, OFBLK /25, 1/
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      JBUFSZ = 2 * MABFSS
C                                       Read output data to determine
C                                       the position and value of the
C                                       maximum
      PTYPE = 'MA'
C                                       Clear the status of the data
C                                       file
      CNO = CNOOUT
      CALL CATDIR ('CSTA', DISKO, CNO, NAMOUT, CLAOUT, SEQOUT, PTYPE,
     *   NLUSER, 'CLWR', BUFF, JERR)


      CALL MAPOPN ('READ', DISKO, NAMOUT, CLAOUT, SEQOUT, PTYPE, NLUSER,
     *    DLUN, DIND, CNO, CATBLK, BUFF, JERR)
C                                       NXX, NYY must be odd!
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = CATBLK(KINAX)
      WIN(4) = CATBLK(KINAX+1)
      NX = CATBLK(KINAX)
      NY = CATBLK(KINAX+1)
C
      CALL MINIT ('READ', DLUN, DIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   WIN, BUFF, JBUFSZ, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970
C                                       Find the maximum position and
C                                       its value
      VALM = -1.E12
      DO 20 IY = 1,NY
         CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, JERR)
         IF (JERR.NE.0) GO TO 970
         DO 10 IX = 1,NX
            VAL = RBUFF(IX+ININD-1)
            IF (VAL .GT. VALM) THEN
               VALM = VAL
               LXMAX = IX
               LYMAX = IY
               END IF
 10         CONTINUE
 20      CONTINUE
C-----------------------------------------------------------------------
C                                       close up map
      CALL MAPCLS ('READ', DISKO, CNO, DLUN, DIND, CATBLK, F,
     *      BUFF, JERR)
C                                       Read the box near the found
C                                       maximum position
      CALL MAPOPN ('READ', DISKO, NAMOUT, CLAOUT, SEQOUT, PTYPE, NLUSER,
     *    DLUN, DIND, CNO, CATBLK, BUFF, JERR)
C                                       NXX, NYY must be odd!
      HALFX = NXX / 2
      HALFY = NYY / 2
      WIN(1) = LXMAX - HALFX
      WIN(1) = MAX (1, WIN(1))
      WIN(2) = LYMAX - HALFY
      WIN(2) = MAX (1, WIN(2))
      WIN(3) = WIN(1) + NXX - 1
      WIN(3) = MIN (WIN(3), CATBLK(KINAX))
      WIN(4) = WIN(2) + NYY - 1
      WIN(4) = MIN (WIN(4), CATBLK(KINAX+1))
      NXX = WIN(3) - WIN(1) + 1
      NYY = WIN(4) - WIN(2) + 1
C
      CALL MINIT ('READ', DLUN, DIND, CATBLK(KINAX), CATBLK(KINAX+1),
     *   WIN, BUFF, JBUFSZ, OFBLK, JERR)
      IF (JERR.NE.0) GO TO 970


C                                       read the data and record into
C                                       array
      DO 120 IY = 1,NYY
         ARGY = IY - HALFY - 1
         CALL MDISK ('READ', DLUN, DIND, BUFF, ININD, JERR)
         IF (JERR.NE.0) GO TO 970
         DO 110 IX = 1,NXX
            ARGX = IX - HALFX - 1
            INDEX = (IX-1)*NYY + IY
            XARG(INDEX) = ARGX
            YARG(INDEX) = ARGY
            IF (INDEX .GT. MAXS) THEN
               WRITE (MSGTXT,1000) INDEX, MAXS
               CALL MSGWRT (8)
               JERR = 1
               GO TO 970
               END IF
            ARR(INDEX) = RBUFF(IX+ININD-1)
 110        CONTINUE
 120     CONTINUE
C-----------------------------------------------------------------------
C                                       close up map
 970  CALL MAPCLS ('READ', DISKO, CNO, DLUN, DIND, CATBLK, F,
     *      BUFF, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ARRAY: The box size ',I10, ' exceeds maximum', I10)
      END
      SUBROUTINE GETCB (ICORN, IRET)
C-----------------------------------------------------------------------
C   GETCB returns in common the convolution beam to use.  It gets this
C   from the CG file having first to find the frequency for the current
C   plane.
C   Inputs:
C      ICORN   I(7)   Current image corner
C   Output
C      IRET    I      Error code from tables if > 0
C                     -10 => deconvolution failed
C                     -11 => could not find frequency in CG table
C-----------------------------------------------------------------------
      INTEGER   ICORN(7), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CONVL.INC'
      INTEGER   FQA, IFQ, BUFF(512), VER, LUN, IRNO, NUMIF,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), NRNO, FQID, IFSIDE, I
      REAL      IFCHW, IFTBW, BMA, BMN, BP, XMAJ, XMIN, XPA
      DOUBLE PRECISION FREQ, IFFREQ, AFREQ, BFREQ, CFREQ
      CHARACTER CTYPE(2)*8, BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CTYPE /'FQID', 'FREQ'/
C-----------------------------------------------------------------------
C                                       FQID axis?
      LUN = 51
      VER = 1
      FREQ = 0.0D0
      CALL AXEFND (4, CTYPE(1), CATOLD(KIDIM), CATOLD(KHCTP), FQA, IRET)
      IF (IRET.EQ.0) THEN
         IFQ = ICORN(FQA+1)
         NUMIF = 1
         CALL FQINI ('READ', BUFF, DISKIN, CNOIN, VER, CATOLD, LUN,
     *      IRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN FQ TABLE'
            GO TO 980
            END IF
         NRNO = BUFF(5)
         DO 10 I = 1,NRNO
            CALL TABFQ ('READ', BUFF, IRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ FQ TABLE'
               GO TO 980
               END IF
            IF (FQID.EQ.IFQ) THEN
               FREQ = IFFREQ
               GO TO 15
               END IF
 10         CONTINUE
         WRITE (MSGTXT,1010) IFQ
         CALL MSGWRT (6)
         CALL TABFQ ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         GO TO 999
C                                       found it
 15      CALL TABFQ ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, NUMIF,
     *      FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
         CALL AXEFND (4, CTYPE(2), CATOLD(KIDIM), CATOLD(KHCTP), FQA,
     *      IRET)
         IF (IRET.EQ.0) FREQ = FREQ + CATOD(KDCRV+FQA)
      ELSE
         CALL AXEFND (4, CTYPE(2), CATOLD(KIDIM), CATOLD(KHCTP), FQA,
     *      IRET)
         IF (IRET.EQ.0) THEN
            BFREQ = CATOR(KRCIC+FQA)
            CFREQ = CATOR(KRCRP+FQA)
            IFQ = ICORN(FQA+1)
            AFREQ = (IFQ - CFREQ)
            FREQ = CATOD(KDCRV+FQA) + AFREQ * BFREQ
            END IF
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'FREQ AXIS MISSING IN INPUT'
         GO TO 980
         END IF
C                                       read CG file to find FREQ
      CALL CGINI ('READ', BUFF, DISKIN, CNOIN, VER, CATOLD, LUN, IRNO,
     *   FQKOLS, FQNUMV, NUMIF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN CG TABLE'
         GO TO 980
         END IF
      NRNO = BUFF(5)
      DO 20 I = 1,NRNO
         CALL TABCG ('READ', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *      BMN, BP, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ CG TABLE'
            GO TO 980
         ELSE IF (IRET.EQ.0) THEN
            IF (ABS(FREQ-IFFREQ).LT.1.0D0) GO TO 25
            END IF
 20      CONTINUE
C                                       try a second time
      IRNO = 1
      DO 22 I = 1,NRNO
         CALL TABCG ('READ', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *      BMN, BP, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ CG TABLE'
            GO TO 980
         ELSE IF (IRET.EQ.0) THEN
         IF (ABS(FREQ-IFFREQ).LT.500.0D0) GO TO 25
            END IF
 22      CONTINUE
      WRITE (MSGTXT,1020) FREQ
      CALL MSGWRT (6)
      CALL TABCG ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA, BMN,
     *   BP, IRET)
      IRET = -11
      GO TO 999
C                                       found it
 25   CALL TABCG ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA, BMN,
     *   BP, IRET)
C                                     Deconvolve.
      BMA = BMA * 3600.
      BMN = BMN * 3600.
      CALL DECONV (BMAJ, BMIN, BPA, BMA, BMN, BP, XMAJ, XMIN, XPA, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1025) FREQ
         CALL MSGWRT (6)
         IRET = -10
      ELSE
         CVBMAP = XMAJ
         CVBMIP = XMIN
         CVBPAP = XPA
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETCB: ERROR',I4,' ON ',A)
 1010 FORMAT ('GETCB: COULD NOT FIND FREQID',I6,' IN FQ TABLE')
 1020 FORMAT ('GETCB: COULD NOT FIND FREQ',1PE14.6,' IN CG TABLE')
 1025 FORMAT ('GETCB: DID NOT FIND CONV BEAM FOR FREQ',1PE14.6)
      END
      SUBROUTINE GETGAU (CBMAJ, CBMIN, CBPA, CONV)
C-----------------------------------------------------------------------
C   GETGAU returns the largest clean beam in the CG table
C   Input/output
C      CBMAJ   R   Clean beam major axis in header
C      CBMIN   R   Clean beam minor axis in header
C      CBPA    R   Clean beam position angle in header
C-----------------------------------------------------------------------
      REAL      CBMAJ, CBMIN, CBPA, CONV
C
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRET, BUFF(512), LUN, VER, NUMIF, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NRNO, I, IRNO
      REAL      BMA, BMN, BP, AREA, AMA, AMN, APA
      DOUBLE PRECISION IFFREQ
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       check the CG table
      IF (USECG) THEN
         LUN = 51
         VER = 1
C                                       read CG file to find FREQ
         CALL CGINI ('READ', BUFF, DISKIN, CNOIN, VER, CATOLD, LUN,
     *      IRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN CG TABLE'
            GO TO 980
            END IF
         NRNO = BUFF(5)
C         AREA = CBMAJ * CBMIN
C         AMA = CBMAJ
C         AMN = CBMIN
C         APA = BPA
         AREA = 0.0
         DO 20 I = 1,NRNO
            CALL TABCG ('READ', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *         BMN, BP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ CG TABLE'
               GO TO 980
            ELSE IF (IRET.EQ.0) THEN
               BMA = BMA * CONV
               BMN = BMN * CONV
               IF (BMA*BMN.GT.AREA) THEN
                  AREA = BMA * BMN
                  AMA = BMA
                  AMN = BMN
                  APA = BPA
                  END IF
               END IF
 20         CONTINUE
         CALL TABCG ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *      BMN, BP, IRET)
         CBMAJ = AMA
         CBMIN = AMN
         CBPA = APA
         WRITE (MSGTXT,1030) AMA, AMN, APA
         CALL MSGWRT (3)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      CBMAJ = 0.0
      CBMIN = 0.0
      CBPA = 0.0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETGAU: ERROR',I4,' ON ',A)
 1030 FORMAT ('GETGAU finds BMAJ, BMIN, BPA',2F11.4,F7.1)
      END
      SUBROUTINE DCHECK (CBMAJ, CBMIN, CBPA, XMAJ, XMIN, XPA, IERR)
C-----------------------------------------------------------------------
C   DCHECK checks if the requested deconvolution will work
C   Inputs
C      CBMAJ   R   Clean beam major axis in header
C      CBMIN   R   Clean beam minor axis in header
C      CBPA    R   Clean beam position angle in header
C   Outputs:
C      XMAJ    R   Major axis of convolution kernal
C      XMIN    R   Minor axis of convolution kernal
C      XPA     R   Position angle of convolution kernal
C      IERR    I   > 0 => quit
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      CBMAJ, CBMIN, CBPA, XMAJ, XMIN, XPA
C
      INCLUDE 'CONVL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRET, BUFF(512), LUN, VER, NUMIF, FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), NRNO, I, IRNO
      REAL      BMA, BMN, BP
      DOUBLE PRECISION IFFREQ
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       try header beam
      CALL DECONV (BMAJ, BMIN, BPA, CBMAJ, CBMIN, CBPA, XMAJ, XMIN, XPA,
     *   IERR)
C                                       check the CG table
      IF ((IERR.NE.0) .AND. (USECG)) THEN
         LUN = 51
         VER = 1
C                                       read CG file to find FREQ
         CALL CGINI ('READ', BUFF, DISKIN, CNOIN, VER, CATOLD, LUN,
     *      IRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN CG TABLE'
            GO TO 980
            END IF
         NRNO = BUFF(5)
         DO 20 I = 1,NRNO
            CALL TABCG ('READ', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *         BMN, BP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ CG TABLE'
               GO TO 980
            ELSE IF (IRET.EQ.0) THEN
               BMA = BMA * 3600.0
               BMN = BMN * 3600.0
               CALL DECONV (BMAJ, BMIN, BPA, BMA, BMN, BP, XMAJ, XMIN,
     *            XPA, IERR)
               IF (IERR.EQ.0) GO TO 30
               END IF
 20         CONTINUE
 30      CALL TABCG ('CLOS', BUFF, IRNO, FQKOLS, FQNUMV, IFFREQ, BMA,
     *      BMN, BP, IRET)
         END IF
      GO TO 999
C
 980  CALL MSGWRT (8)
      IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DCHECK: ERROR',I4,' ON ',A)
      END
      SUBROUTINE PLNBLK (ODISK, OCNO, OCORN, JWIN, BUFF2, BUFSZ2, LUN2,
     *   IRET)
C-----------------------------------------------------------------------
C   PLNBLK writes a subregion of a cataloged image with blanks.
C   Input:
C      ODISK    I      Output image disk number.
C      OCNO     I      Output image catalog slot number.
C      OCORN    I(7)   BLC in Output image (1 & 2 ignored)
C      JWIN     I(4)   Window in plane in input image.
C      BUFF2    R(*)   Work buffer.
C      BUFSZ2   I      Size in bytes of BUFF2
C      LUN2     I      Second logical unit number to use.
C   Output:
C      IRET     I      Return error code: 0 => OK
C                         1 = couldn't read output CATBLK.
C                         2 = Output bits/pixel not allowed.
C                         3 = Output and input windows not same.
C                         4 = couldn't open input map file.
C                         5 = couldn't init output map.
C                         6 = couldn't init input map.
C                         7 = read error input map.
C                         8 = write error output map.
C                         9 = error writing header to catalog
C                        10 = error computing block offset.
C   Commons:
C      CATBLK in /MAPHDR/ is used as the map header.
C             Of particular importance is the data max/min values
C             which must apply to the map.  As this is read from the
C             catalog it must be updated by a call to CATIO etc.
C             before calling this routine.
C-----------------------------------------------------------------------
      INTEGER   ODISK, OCNO, OCORN(7), JWIN(4), BUFSZ2, LUN2, IRET
      REAL      BUFF2(*)
C
      CHARACTER OFILE(6)*48
      INTEGER   IERR, WIN(4), OBO, I4, OWIN(4), KORN(7), FIND2, BIND2,
     *   MX, MY, NNX, NNY, I, BOTEMP, IWIN(4)
      LOGICAL   T, F, OPEN2
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
      OPEN2 = F
C                                       Read output CATBLK.
      CALL CATIO ('READ', ODISK, OCNO, CATBLK, 'REST', BUFF2, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.3)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) 'OUTPUT', IERR
         GO TO 980
         END IF
C                                       Determine max, min and mapsize.
      MX = CATBLK(KINAX)
      MY = CATBLK(KINAX+1)
C                                       Set window for maps.
      WIN(1) = 1
      WIN(2) = 1
      WIN(3) = MX
      WIN(4) = MY
      IWIN(3) = IWIN(1) + MX - 1
      IWIN(4) = IWIN(2) + MY - 1
C                                        Check defaults on OWIN
      CALL COPY (4, JWIN, OWIN)
      IF (OWIN(1).LE.0) OWIN(1) = 1
      IF (OWIN(2).LE.0) OWIN(2) = 1
      IF ((OWIN(3).LE.0) .OR. (OWIN(3).GT.(MX+OWIN(1)-1)))
     *   OWIN(3) = MX + OWIN(1) - 1
      IF ((OWIN(4).LE.0) .OR. (OWIN(4).GT.(MY+OWIN(2)-1)))
     *   OWIN(4) = MY + OWIN(2) - 1
C                                        Check defaults on OCORN
      DO 30 I4 = 1,KICTPN
         KORN(I4) = MAX (OCORN(I4), 1)
 30      CONTINUE
C                                       Set output BLOCK offset.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), KORN(3), BOTEMP, IERR)
      OBO = BOTEMP + 1
      IF (IERR.NE.0) THEN
         IRET = 10
         WRITE (MSGTXT,1030) IERR
         GO TO 980
         END IF
C                                        Check that input and output
C                                        windows are the same size.
      NNX = OWIN(3) - OWIN(1) + 1
      NNY = OWIN(4) - OWIN(2) + 1
      IF ((NNX.NE.MX) .OR. (NNY.NE.MY)) THEN
         IRET = 3
         WRITE (MSGTXT,1035) NNX, NNY, MX, MY
         GO TO 980
         END IF
C                                       Open output file.
      CALL ZPHFIL ('MA', ODISK, OCNO, 1, OFILE, IERR)
      CALL ZOPEN (LUN2, FIND2, ODISK, OFILE, T, T, F, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1040) IERR, 'OPEN OUTPUT'
         GO TO 980
         END IF
      OPEN2 = T
C                                       Init files.
      CALL MINIT ('WRIT', LUN2, FIND2, MX, MY, WIN, BUFF2, BUFSZ2, OBO,
     *   IERR)
      IF (IERR.NE.0) THEN
         IRET = 6
         WRITE (MSGTXT,1040) IERR, 'INIT OUTPUT'
         GO TO 980
         END IF
C                                       Finally do what you're here for
      DO 200 I = 1,MY
C                                       Write output map.
         CALL MDISK ('WRIT', LUN2, FIND2, BUFF2, BIND2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 8
            WRITE (MSGTXT,1100) IERR, I, 'WRITE OUTPUT'
            GO TO 980
            END IF
C                                       Move to output buffer.
         CALL RFILL (MX, FBLANK, BUFF2(BIND2))
 200     CONTINUE
C                                       Finish write.
      CALL MDISK ('FINI', LUN2, FIND2, BUFF2, BIND2, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         WRITE (MSGTXT,1100) IERR, MY, 'WRITE OUTPUT'
         END IF
C                                       Update catlg of output map
 980  IF (IRET.EQ.0) THEN
         CATR(KRBLK) = FBLANK
         CALL CATIO ('UPDT', ODISK, OCNO, CATBLK, 'REST', BUFF2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 9
            WRITE (MSGTXT,1980) IERR
            END IF
         END IF
C                                       error message if any
      IF (IRET.NE.0) CALL MSGWRT (8)
C                                       close files
      IF (OPEN2) CALL ZCLOSE (LUN2, FIND2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLNBLK: ERROR',I3,' READING ',A,' CATBLK')
 1030 FORMAT ('PLNBLK: ERROR',I3,' COMPUTING BLOCK OFFSET')
 1035 FORMAT ('PLNBLK: INPUT WINDOW SIZE=',2I5,' OUTPUT=',2I5)
 1040 FORMAT ('PLNBLK: ERROR',I4,1X,A,' FILE')
 1100 FORMAT ('PLNBLK: ERROR',I4,' ROW ',I5,1X,A,' FILE')
 1980 FORMAT ('PLNBLK: ERROR',I3,' UPDATING CATALOGED MAP HEADER')
      END
