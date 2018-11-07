LOCAL INCLUDE 'LWPLA.INC'
C                                       Local INCLUDE for LWPLA
      INCLUDE 'INCS:PTVC.INC'
      HOLLERITH XNAMIN(3), XCLSIN(2), XFUNTY, XOUTFL(12), XOMFIL(12)
      CHARACTER NAMIN*12, CLSIN*6, FUNTYP*2, LWBUFF*512, OUTFIL*48,
     *   GPHFUN*2, OFMFIL*48
      INTEGER   FX0, FY0, GOPR, ITRN, IXL, IYL, LWPOS, LWSIZE, NPAGE,
     *   PAGEX, PAGEY, COPIES, IVER1, IVER2, ISEQ, SLOT, IVOL, USER,
     *   EXTBOX(4), LTYPE, LASTM(2), LASTR(6), NOFM
      REAL   SEQIN, DSKIN, VERSN, VERSN2, XASPMM, XXPEN, GAMMA(3),
     *   DPARM(10), SCALEX, SCALEY, XCOPYS, DODARK, DOCOLR,
     *   RGBCOL(3,10), RANGE(2,3), ROFM(TVMLOU), GOFM(TVMLOU),
     *   BOFM(TVMLOU), CURCOL(3), THREEC(3)
      LOGICAL   COLRIT, DOOFM, DO3C, DO4C, FIRST
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, VERSN2,
     *   XASPMM, XXPEN, GAMMA, XFUNTY, DPARM, XOUTFL, XCOPYS, DODARK,
     *   XOMFIL, DOCOLR, RGBCOL, LASTM, LASTR
      COMMON /CHRCOM/ NAMIN, CLSIN, FUNTYP, LWBUFF, OUTFIL, GPHFUN,
     *   OFMFIL
      COMMON /LWSPCL/ LWPOS, LWSIZE, IXL, IYL, FX0, FY0, SCALEX, SCALEY,
     *   PAGEX, PAGEY, COPIES, GOPR, ITRN, IVER1, IVER2, ISEQ, SLOT,
     *   IVOL, USER, NPAGE, EXTBOX, LTYPE, RANGE, COLRIT, DOOFM, DO3C,
     *   DO4C, ROFM, GOFM, BOFM, FIRST, CURCOL, THREEC, NOFM
LOCAL END
      PROGRAM LWPLA
C-----------------------------------------------------------------------
C! Translates plot file for LaserWriter printer
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2002-2003, 2006-2012, 2014-2015, 2018
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
C   LWPLA is a program for the AIPS graphics package.  The program runs
C   as a detached task initiated from AIPS.  First a cataloged file is
C   found using data passed from AIPS.  The list of associated files is
C   searched for a PLot file corresponding to the version number.  The
C   graphics commands are converted to character strings appropriate to
C   the LaserWriter and written to a file which "prints" at the end.
C   Inputs:  (from AIPS)
C      USERID  R      User ID: 0 -> logon number, 32000 -> any.
C      INNAME  R(3)   Name of primary file.
C      INCLASS R(2)   Class of primary file.
C      INSEQ   R      Sequence number of primary file.
C      INDISK  R      Disk volume number. 0 means try all.
C      INVERS  R      Extension file version number. 0 ->
C                     use the highest version number.
C      ASPMM   R      Arc sec per millimeter. 0 -> self scale.
C      LPEN    R      Pen width in dots.  0 => 1.
C      RGBGAMMA  R(3) Gamma correction values
C      FUNCTYPE  R    Convert after clipping by: 'NE' negative linear,
C                     'LG' log, 'NG' negative log.
C      DPARM  R(10)   (1,2) Min, max clipping range 0 <= min < max <= 1.
C                           before FUNCTYPE, then
C                     (3,4) rescale g = DPARM(3)*g + DPARM(4) and
C                           clip again to 0 to 1.
C                     (5) Page orientation
C                         0: fill page
C                         1: portrait
C                         2: landscape
C                     (6) Paper type (for centering)
C                         0: American quarto
C                         1: American legal
C                         2: 4x5 Slide Maker
C                         3: A3  4: A4  5: Slide Maker
C                     (7) Font type
C                         0: Helvetica-Bold
C                         1: Courier-Bold
C                     (8) Font size (points, default 13).
C                     (9) 0.0 -> black contours,
C                         0.5 -> grey,
C                         1.0 -> white
C
C      OUTFILE R(12)  Named file for Postscript output.
C
C   Submitted by Bruce Cogan at Mt. Stromlo Observatory, revised freely
C   thereafter.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   BUFFER(256), LUN, FIND, IRET, RETCOD
      LOGICAL   QUICK
      INCLUDE 'LWPLA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'LWPLA '/
      DATA LUN /26/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL LWINI (PRGNAM, QUICK, BUFFER, IRET)
C                                       Write to Postscript file
      IF (IRET.EQ.0) THEN
         CALL LWDRW (LUN, FIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000)
            CALL MSGWRT (8)
            RETCOD = IRET
         ELSE
            RETCOD = 0
            END IF
         END IF
C                                       Close graph file.
      CALL DIE (RETCOD, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR WRITING TO LASERWRITER')
      END
      SUBROUTINE LWINI (PRGNAM, QUICK, BUFFER, IRET)
C-----------------------------------------------------------------------
C   LWINI does the initialization for LWPLA, getting parameters,
C   locating the Plot file, and opening it.
C   Inputs: PRGNAM   C*6     Task name
C   Output: QUICK    L        Quick return desired or not.
C           BUFFER   I(256)   Plot buffer
C           IRET     I        Error code: not 0 => quit.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   BUFFER(256), IRET
      LOGICAL   QUICK
C
      CHARACTER TYPIN*2, STATUS*4, OKTRN(7)*2
      INTEGER   CATERR, IERR, NPARMS, IVER, I, NTRN
      LOGICAL   EQUAL
      INCLUDE 'LWPLA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA NTRN, OKTRN /7, 'NE', 'LG', 'NG', 'L2', 'N2', 'SQ', 'NQ'/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
      COLRIT = .FALSE.
C                                       Get input values from AIPS.
      NPARMS = 82
      CALL GTPARM (PRGNAM, NPARMS, QUICK, XNAMIN, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
      IF (QUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (2, 1, XFUNTY, FUNTYP)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
      CALL H2CHR (48, 1, XOMFIL, OFMFIL)
      COPIES = XCOPYS
      IF (COPIES.LE.0) COPIES = 1
      IF (OUTFIL.NE.' ') COPIES = 1
C                                       Transfer function: type, clips
      ITRN = 0
      DO 20 I = 1,NTRN
         IF (OKTRN(I)(:2).EQ.FUNTYP(:2)) ITRN = I
 20      CONTINUE
      ITRN = ITRN + 1
      IF (GAMMA(1).LE.0.0) GAMMA(1) = 2.5
      IF (GAMMA(2).LE.0.0) GAMMA(2) = GAMMA(1)
      IF (GAMMA(3).LE.0.0) GAMMA(3) = GAMMA(2)
      GAMMA(1) = 1.0/GAMMA(1)
      GAMMA(2) = 1.0/GAMMA(2)
      GAMMA(3) = 1.0/GAMMA(3)
      IF ((DPARM(1).LE.0.0) .OR. (DPARM(1).GE.0.999)) DPARM(1) = 0.0
      IF ((DPARM(2).GT.1.0) .OR. (DPARM(2).LE.DPARM(1))) DPARM(2) =
     *   1.0
      IF ((ABS(DPARM(3)).LE.0.01) .OR. (ABS(DPARM(3)).GE.100.))
     *   DPARM(3) = 1.0
      IF (ABS(DPARM(4)).GT.100.0) DPARM(4) = 0.0
      IF ((DPARM(1).NE.0.0) .OR. (DPARM(2).NE.1.0) .OR.
     *   (DPARM(3).NE.1.0) .OR. (DPARM(4).NE.0.0)) THEN
         WRITE (MSGTXT,1020) DPARM(1), DPARM(2), DPARM(3), DPARM(4)
      ELSE
         MSGTXT = 'No clipping or scaling applied to grey scale pixels'
         END IF
      CALL MSGWRT (3)
      IF (DPARM(9).LT.0.0) DPARM(9) = 0.0
      IF (DPARM(9).GT.1.0) DPARM(9) = 1.0
C                                       Get map header.
      ISEQ = SEQIN + .01
      IVOL = DSKIN + .01
      USER = NLUSER
      SLOT = 1
      TYPIN = '  '
      CALL CATDIR ('SRCH', IVOL, SLOT, NAMIN, CLSIN, ISEQ, TYPIN, USER,
     *   STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1025) IERR
         GO TO 990
         END IF
C                                       Read catalog header.
      CALL CATIO ('READ', IVOL, SLOT, CATBLK, 'REST', BUFFER, CATERR)
      IF ((CATERR.NE.0) .AND. (CATERR.NE.6)) THEN
         WRITE (MSGTXT,1040) CATERR
         GO TO 990
         END IF
C                                        Find plot file:
C                                        if PL & IVER=0, then main file
      EQUAL = TYPIN.EQ.'PL'
      IF ((EQUAL) .AND. (CATERR.EQ.6)) THEN
         WRITE (MSGTXT,1040) CATERR
         GO TO 990
         END IF
C
      IVER1 = VERSN + .01
      IVER2 = VERSN2 + .01
      IF (EQUAL) THEN
         IF (IVER1.EQ.0) IVER1 = CATBLK(KIIMS)
         IVER2 = IVER1
         IVER = IVER1
      ELSE
         CALL FNDEXT ('PL', CATBLK, IVER)
         IF (IVER1.LE.0) IVER1 = IVER
         IVER2 = MIN (IVER2, IVER)
         IF (IVER2.LT.IVER1) IVER2 = IVER1
         END IF
C                                       PLot file not found.
      IF (IVER.LE.0) THEN
         MSGTXT = 'PLOT FILE NOT IN CATALOG'
         GO TO 990
         END IF
C                                       Save parms
      SEQIN = ISEQ
      DSKIN = IVOL
      VERSN = IVER1
      VERSN2 = IVER2
C                                       Set line colors?
      IF (DOCOLR.GT.0.0) THEN
         DO 75 I = 1,10
            IF ((RGBCOL(1,I).LT.0.0) .OR. (RGBCOL(1,I).GT.1.0))
     *          DOCOLR = -1.0
            IF ((RGBCOL(2,I).LT.0.0) .OR. (RGBCOL(2,I).GT.1.0))
     *         DOCOLR = -1.0
            IF ((RGBCOL(3,I).LT.0.0) .OR. (RGBCOL(3,I).GT.1.0))
     *         DOCOLR = -1.0
            IF (ABS(RGBCOL(1,I)-RGBCOL(2,I)).GT.0.001) COLRIT = .TRUE.
            IF (ABS(RGBCOL(1,I)-RGBCOL(3,I)).GT.0.001) COLRIT = .TRUE.
 75         CONTINUE
         END IF
C                                       Init color scale
      IF (DOCOLR.LE.0.0) THEN
         CALL RFILL (12, 0.0, RGBCOL(1,1))
         CALL RFILL (18, 1.0, RGBCOL(1,5))
         CALL RFILL ( 3, 0.0, RGBCOL(1,9))
         COLRIT = .FALSE.
         END IF
C                                       ofm file
      CALL GETOFM (OFMFIL, .FALSE., I, DOOFM, DO3C, ROFM, GOFM, BOFM,
     *   NOFM, IRET)
      IF (DO3C) COLRIT = .TRUE.
      CURCOL(1) = 0.0
      CURCOL(2) = 0.0
      CURCOL(3) = 0.0
      THREEC(1) = 0.0
      THREEC(2) = 0.0
      THREEC(3) = 0.0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('Clip greys by',2F7.3,' then scale by',2F9.3)
 1025 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1040 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
      END
      SUBROUTINE LWDRW (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the
C   LaserWriter.
C   INPUTS:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      CHARACTER CHBUF*256, TMPBUF*256, GREYS*72, PAPTYP(8)*10
      INTEGER   CHBSIZ
C                                       Max size string to print
      PARAMETER (CHBSIZ=256)
      REAL      DELTA, DX, DY, EPSLOG, GROFF, GRSCAL, LNEPS, NOWPIX,
     *   NXPIX, NYPIX, PPD, RCALEX, RCALEY, RDLNEP, RXPIX, RX1, RX2,
     *   RYPIX, RY1, RY2, SCALEF, TOTPIX, UASPMM, V, XLWDMM, XPXSEP,
     *   XYDIFF, XYRATO, YASPMM, YLWDMM, YPXSEP, BRDR, XPG, YPG, ASPMM,
     *   ROBLK(256), RV(3), TEMP, BGAMMA, COLSCL, ACTCHR
      INTEGER   BNDBOX(4), CSIZLW(2), CV, FIND, FX1, FY1, GRYERR,
     *   HAFPIX, HAFPIY, I, IANGL, ICHB, ICHBUF, ICHL, ICHR, NN,
     *   ICHT, IERR, IERRC, IERRCH, IFONT, IGRHI, IGRLO, INCRRN,
     *   IOBLK(CHBSIZ), IOPOS, IORRN, IPAPER, IPER, IPER2, IROUND,
     *   IT(6), ITRIM, ITYPE, IX, IX1, IX2, IY, IY1, IY2, J, K,
     *   LASTNP, LLPEN, LUN, LWLUN, MAXXLW(2), N, NCH, NCHAR, NFLX,
     *   NFPX, NFLY, NFPY, NFX, NFY, NPIX, NPTSX, NPTSY, NXA, NYA,
     *   OPCODE, ORIENT, PAGESZ(2), PSCALX, PSCALY, CLWSIZ, JNN,
     *   IVER, IRIENT, INPAGE, MPIX, MCOLR, ICOLR, NCP, VCP, MCP, NOMIT,
     *   BNDSCL(4), JTRIM, INCHAR
      LOGICAL   DOGREY, YES, PRTOK, DONEG, DO3COL, DOBG, INVRT, OMIT,
     *   DOOPEN
      CHARACTER ATIME*8, ADATE*12, CLOCK*20, FONT*40, XCHBUF(CHBSIZ)*2,
     *   GFILE*48, CLTYPE(4)*13, COMNT*80, LCHBUF*256
      HOLLERITH HOBLK(256)
      EQUIVALENCE (IOBLK, HOBLK, ROBLK)
      INCLUDE 'LWPLA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA PAPTYP /'legal' , '4x5 slide', 'a3', 'a4', '35mm slide',
     *   'ledger', 'letter',' '/
      DATA EPSLOG /1.0E-6/
      DATA LWLUN /3/
      DATA YES /.TRUE./
      DATA CLTYPE /'labeling','contours','pol. lines','stars & marks'/
      DATA BGAMMA /12.0/
C-----------------------------------------------------------------------
      DOOPEN = .TRUE.
      NOMIT = 0
      LASTM(1) = -10000
      LASTM(2) = -10000
      LASTR(1) = -10000
      LASTR(2) = -10000
      LASTR(3) = -10000
      LASTR(4) = -10000
      LASTR(5) = -10000
      LASTR(6) = -10000
C                                       Nunber of points per dot.
      PPD = 72.0/300.0
      BRDR = 5.0 / PPD + 0.75
      DOBG = (RGBCOL(1,10).NE.1.0) .OR. (RGBCOL(2,10).NE.1.0) .OR.
     *   (RGBCOL(3,10).NE.1.0)
      COLSCL = 0.8
      IF (DOBG) COLSCL = 1.0
C                                       Page orientation.
C                                       change DPARM(5), not ORIENT
      ORIENT = 0
      J = IROUND (DPARM(5))
      IF (J.EQ.0) ORIENT = 1
      IF ((J.EQ.2) .OR. (J.EQ.3)) ORIENT = J
      IRIENT = ORIENT
C                                       Paper and usable page sizes in
C                                       dots (1/300 inch).
      IPAPER = IROUND (DPARM(6))
C                                       User set
      IF (DPARM(6).GT.1001.) THEN
         IPAPER = 8
         YPG = MOD (DPARM(6), 100.0)
         XPG = (DPARM(6) - YPG) / 1000.0
         WRITE (PAPTYP(8),1000) XPG, YPG
         PAGESZ(1) = XPG * 300.0 + 0.5
         PAGESZ(2) = YPG * 300.0 + 0.5
         MAXXLW(1) = MAX (PAGESZ(1)-300, PAGESZ(1)/2)
         MAXXLW(2) = MAX (PAGESZ(2)-300, PAGESZ(2)/2)
C                                       American legal.
      ELSE IF (IPAPER.EQ.1) THEN
         PAGESZ(1) =  8.5*300.0
         PAGESZ(2) = 14.0*300.0
         MAXXLW(1) =  7.5*300.0
         MAXXLW(2) = 13.5*300.0
C                                       Freedom Slide 4x5 in Slide
      ELSE IF (IPAPER.EQ.2) THEN
         PAGESZ(1) =  7.5*300.0
         PAGESZ(2) = 10.0*300.0
         MAXXLW(1) =  7.0*300.0
         MAXXLW(2) =  9.5*300.0
C                                       Metric A3.
      ELSE IF (IPAPER.EQ.3) THEN
         PAGESZ(1) = 297.0*(300.0/25.4)
         PAGESZ(2) = 420.0*(300.0/25.4)
         MAXXLW(1) = 277.0*(300.0/25.4)
         MAXXLW(2) = 400.0*(300.0/25.4)
C                                       Metric A4.
      ELSE IF (IPAPER.EQ.4) THEN
         PAGESZ(1) = 210.0*(300.0/25.4)
         PAGESZ(2) = 297.0*(300.0/25.4)
         MAXXLW(1) = 190.0*(300.0/25.4)
         MAXXLW(2) = 277.0*(300.0/25.4)
C                                       Freedom Slide 35 mm Slide
      ELSE IF (IPAPER.EQ.5) THEN
         PAGESZ(1) =  7.333*300.0
         PAGESZ(2) = 11.0*300.0
         MAXXLW(1) =  6.9*300.0
         MAXXLW(2) = 10.5*300.0
C                                       Double American paper
      ELSE IF (IPAPER.EQ.6) THEN
         PAGESZ(1) = 11.0*300.0
         PAGESZ(2) = 17.0*300.0
         MAXXLW(1) = 10.0*300.0
         MAXXLW(2) = 16.0*300.0
C                                       American quarto.
      ELSE
         IPAPER = 7
         PAGESZ(1) =  8.5*300.0
         PAGESZ(2) = 11.0*300.0
         MAXXLW(1) =  7.5*300.0
         MAXXLW(2) = 10.0*300.0
         END IF
C                                       Font size.
      CLWSIZ = IROUND (DPARM(8)) / PPD
      IF (CLWSIZ.LE.0) CLWSIZ = 13 / PPD
      IF (CLWSIZ.GT.600) THEN
         CLWSIZ = 600
         WRITE (MSGTXT,1001) INT (CLWSIZ*PPD)
         CALL MSGWRT (7)
         END IF
      CSIZLW(1) = 0.51 * CLWSIZ
      CSIZLW(2) = 0.77 * CLWSIZ
C                                       Font type.
      IFONT = IROUND (DPARM(7))
      FONT  = 'Helvetica-Bold'
      IF (IFONT.LT.10) THEN
         IF (IFONT.EQ.01) FONT = 'AvantGarde-Book'
         IF (IFONT.EQ.02) FONT = 'AvantGarde-BookOblique'
         IF (IFONT.EQ.03) FONT = 'AvantGarde-Demi'
         IF (IFONT.EQ.04) FONT = 'AvantGarde-DemiOblique'
      ELSE IF (IFONT.LT.20) THEN
         IF (IFONT.EQ.11) FONT = 'Bookman-Demi'
         IF (IFONT.EQ.12) FONT = 'Bookman-DemiItalic'
         IF (IFONT.EQ.13) FONT = 'Bookman-Light'
         IF (IFONT.EQ.14) FONT = 'Bookman-LightItalic'
      ELSE IF (IFONT.LT.30) THEN
         CSIZLW(1) = 0.6 * CLWSIZ
         CSIZLW(2) = 0.65 * CLWSIZ
         IF (IFONT.EQ.21) FONT = 'Courier'
         IF (IFONT.EQ.22) FONT = 'Courier-Bold'
         IF (IFONT.EQ.23) FONT = 'Courier-Oblique'
         IF (IFONT.EQ.24) FONT = 'Courier-BoldOblique'
      ELSE IF (IFONT.LT.40) THEN
         IF (IFONT.EQ.31) FONT = 'Helvetica'
         IF (IFONT.EQ.32) FONT = 'Helvetica-Bold'
         IF (IFONT.EQ.33) FONT = 'Helvetica-Oblique'
         IF (IFONT.EQ.34) FONT = 'Helvetica-BoldOblique'
      ELSE IF (IFONT.LT.50) THEN
         IF (IFONT.EQ.41) FONT = 'Helvetica-Narrow'
         IF (IFONT.EQ.42) FONT = 'Helvetica-Narrow-Bold'
         IF (IFONT.EQ.43) FONT = 'Helvetica-Narrow-Oblique'
         IF (IFONT.EQ.44) FONT = 'Helvetica-Narrow-BoldOblique'
      ELSE IF (IFONT.LT.60) THEN
         IF (IFONT.EQ.51) FONT = 'NewCenturySchlbk-Roman'
         IF (IFONT.EQ.52) FONT = 'NewCenturySchlbk-Bold'
         IF (IFONT.EQ.53) FONT = 'NewCenturySchlbk-Italic'
         IF (IFONT.EQ.54) FONT = 'NewCenturySchlbk-BoldItalic'
      ELSE IF (IFONT.LT.70) THEN
         IF (IFONT.EQ.61) FONT = 'Palatino-Roman'
         IF (IFONT.EQ.62) FONT = 'Palatino-Bold'
         IF (IFONT.EQ.63) FONT = 'Palatino-Italic'
         IF (IFONT.EQ.64) FONT = 'Palatino-BoldItalic'
      ELSE IF (IFONT.LT.80) THEN
         IF (IFONT.EQ.71) FONT = 'Times-Roman'
         IF (IFONT.EQ.72) FONT = 'Times-Bold'
         IF (IFONT.EQ.73) FONT = 'Times-Italic'
         IF (IFONT.EQ.74) FONT = 'Times-BoldItalic'
      ELSE IF (IFONT.LT.90) THEN
         IF (IFONT.EQ.81) FONT = 'ZapfChancery-MediumItalic'
         END IF

C                                       Dots per mm.
      XLWDMM = 300.0/25.4
      YLWDMM = XLWDMM
      IERR = 0
      IERRC = 0
      IERRCH = 0
      GRYERR = 0
      LNEPS = LOG(EPSLOG)
      RDLNEP = 1.0/(LOG(1.0+EPSLOG) - LNEPS)
      DOGREY = .FALSE.
      DO3COL = .FALSE.
C                                       Pen size.
      LLPEN = IROUND (XXPEN)
      IF (LLPEN.LT.1) LLPEN = 3
      LLPEN = MIN (LLPEN, 12)
C                                       Set buffer size
      LWSIZE = LEN (LWBUFF)
C                                       Loop over versions
      IVER = IVER1
C                                       Determine where plot starts
C                                       Build file name.
 10   CALL ZPHFIL ('PL', IVOL, SLOT, IVER, GFILE, IERR)
      IF (IERR.NE.0) GO TO 920
C                                       Open graphics file.
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, .FALSE., .FALSE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IVER
         CALL MSGWRT (6)
         GO TO 925
         END IF
      CALL ZFIO ('READ', LUN, FIND, 1, IOBLK, IERR)
      IF (IERR.NE.0) GO TO 920
C                                       Open Print file
C                                       deferred until 1st file found
      IF (DOOPEN) THEN
         NPAGE = 0
         CALL LWIO ('OPEN', LWLUN, LWBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1005) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         FIRST = .TRUE.
         INPAGE = NPAGE
         DOOPEN = .FALSE.
         END IF
      ORIENT = IRIENT
      IOPOS = 9999
      IORRN = (IOBLK(10) + 9) / 256 + 1
      ASPMM = XASPMM
C                                       Read record.
 50   IF (IOPOS.LE.256) GO TO 60
 55      IORRN = IORRN + 1
         CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
         IF (IERR.NE.0) GO TO 980
         IOPOS = 1
C                                       Transfer based on opcode.
 60   OPCODE = IOBLK(IOPOS)
      IF ((OPCODE.GT.0) .AND. (OPCODE.LE.18)) GO TO (100, 200,
     *   300, 400, 500, 600, 700, 800, 150, 500, 700, 300, 300, 600,
     *   600, 160, 500, 810), OPCODE
      IF (OPCODE.EQ.0) GO TO 55
      IF (OPCODE.NE.32767) GO TO 900
C                                       End of file.
         GO TO 910
C-----------------------------------------------------------------------
C                                       Opcode 1, initialize.
C                                       Put map type in image header.
 100  ITYPE = IOBLK(IOPOS+5)
      IOPOS = IOPOS + 6
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 9, line type
 150  IF (IOBLK(IOPOS+1).NE.LTYPE) THEN
         LTYPE = IOBLK(IOPOS+1)
         CHBUF = '% following vectors are of type ' // CLTYPE(LTYPE)
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
      IOPOS = IOPOS + 2
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 16, 3-color vector color
 160  THREEC(1) = (ROBLK(IOPOS+1) * COLSCL) ** GAMMA(1)
      THREEC(2) = (ROBLK(IOPOS+2) * COLSCL) ** GAMMA(2)
      THREEC(3) = (ROBLK(IOPOS+3) * COLSCL) ** GAMMA(3)
      IOPOS = IOPOS + 4
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 2, line drawing init.
 200  XYRATO = IOBLK(IOPOS+1) / 100.0
      SCALEF = IOBLK(IOPOS+2)
      IX1    = IOBLK(IOPOS+3)
      IY1    = IOBLK(IOPOS+4)
      IX2    = IOBLK(IOPOS+5)
      IY2    = IOBLK(IOPOS+6)
      RX1    = IOBLK(IOPOS+7)/1000.0 + IX1
      RY1    = IOBLK(IOPOS+8)/1000.0 + IY1
      RX2    = IOBLK(IOPOS+9)/1000.0 + IX2
      RY2    = IOBLK(IOPOS+10)/1000.0 + IY2
      ICHL   = (IOBLK(IOPOS+11)/10.0 + 0.8) * CSIZLW(1) + BRDR
      ICHB   = (IOBLK(IOPOS+12)/10.0) * CSIZLW(2) + BRDR
      ICHR   = (IOBLK(IOPOS+13)/10.0 + 0.3) * CSIZLW(1) + BRDR
      ICHT   = (IOBLK(IOPOS+14)/10.0) * CSIZLW(2) + BRDR
      IOPOS = IOPOS + 15
C                                       Frame size in pixels.
      NXPIX = ABS(IX2-IX1)
      NYPIX = ABS(IY2-IY1)
      RXPIX = ABS(RX2-RX1)
      RYPIX = ABS(RY2-RY1)
      IF ((RXPIX.LE.0) .OR. (RYPIX.LE.0)) THEN
         WRITE (MSGTXT,1200) RXPIX, RYPIX
         CALL MSGWRT (7)
         GO TO 920
         END IF
C                                       Dots available for frame.
      NFPX = MAXXLW(1) - ICHL - ICHR
      NFPY = MAXXLW(2) - ICHT - ICHB
      NFLX = MAXXLW(2) - ICHL - ICHR
      NFLY = MAXXLW(1) - ICHT - ICHB
      IF ((NFPX.LE.0) .OR. (NFPY.LE.0)) THEN
         IF ((NFLX.LE.0) .OR. (NFLY.LE.0)) THEN
            WRITE (MSGTXT,1205) NFPX, NFPY
            CALL MSGWRT (7)
            GO TO 920
         ELSE
            ORIENT = 2
            END IF
         END IF
C                                       Scale factors, portrait.
      IF (NFPX/(RXPIX*XYRATO).LT.NFPY/RYPIX) THEN
         SCALEX = (NFPX/SCALEF)*(NXPIX/RXPIX)
         SCALEY = (NFPX/SCALEF)*(NYPIX/(RXPIX*XYRATO))
      ELSE
         SCALEX = (NFPY/SCALEF)*(NXPIX*XYRATO/RYPIX)
         SCALEY = (NFPY/SCALEF)*(NYPIX/RYPIX)
         END IF
C                                       Scale factors, landscape.
      IF (NFLX/(RXPIX*XYRATO).LT.NFLY/RYPIX) THEN
         RCALEX = (NFLX/SCALEF)*(NXPIX/RXPIX)
         RCALEY = (NFLX/SCALEF)*(NYPIX/(RXPIX*XYRATO))
      ELSE
         RCALEX = (NFLY/SCALEF)*(NXPIX*XYRATO/RYPIX)
         RCALEY = (NFLY/SCALEF)*(NYPIX/RYPIX)
         END IF
C                                       Self-orient if required.
      IF (ORIENT.EQ.0) THEN
         ORIENT = 1
         IF (SCALEX*SCALEY.LT.RCALEX*RCALEY) ORIENT = 2
         END IF
C                                       Page parameters.
C                                       Portrait.
      IF (ORIENT.EQ.1) THEN
         PAGEX = PAGESZ(1)
         PAGEY = PAGESZ(2)
         NFX = MAXXLW(1) - ICHL - ICHR
         NFY = MAXXLW(2) - ICHT - ICHB
C                                       Swap dimensions for landscape.
      ELSE
         SCALEX = RCALEX
         SCALEY = RCALEY
         PAGEX = PAGESZ(2)
         PAGEY = PAGESZ(1)
         NFX = MAXXLW(2) - ICHL - ICHR
         NFY = MAXXLW(1) - ICHT - ICHB
         END IF
C                                       Arcsec per mm scaling.
      LOCNUM = 1
      AXTYP(LOCNUM) = 0
      UASPMM = ASPMM
      IF ((ITYPE.EQ.2) .OR. (ITYPE.EQ.3) .OR. (ITYPE.EQ.6) .OR.
     *   (ITYPE.EQ.18) .OR. (ITYPE.EQ.27) .OR. (ITYPE.EQ.28) .OR.
     *   (ITYPE.EQ.29)) THEN
         CALL SETLOC (IOBLK(IOPOS), YES)
C                                       Determine arcsec per mm.
         IF (AXTYP(LOCNUM).EQ.1) THEN
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
            YPXSEP = XPXSEP
            CALL SCALMM (NXPIX*XYRATO, NYPIX, XPXSEP, YPXSEP, SCALEF,
     *         XLWDMM, YLWDMM, ASPMM, SCALEX, SCALEY)
            GO TO 240
            END IF
         END IF
C                                       Scale it down?
      IF ((ASPMM.GT.0.02) .AND. (ASPMM.LE.0.98)) THEN
         AXTYP(LOCNUM) = -10
         WRITE (MSGTXT,1220) ASPMM
         CALL MSGWRT (4)
         SCALEX = SCALEX * ASPMM
         SCALEY = SCALEY * ASPMM
         END IF
C                                       Total plot size.
 240  NXA = SCALEX*SCALEF*RXPIX/NXPIX + ICHL + ICHR
      NYA = SCALEY*SCALEF*RYPIX/NYPIX + ICHB + ICHT
C                                       Frame coordinates, BLC & TRC.
      FX0 = ICHL + MAX (0, PAGEX-NXA)/2 + 1
      FY0 = ICHB + MAX (0, PAGEY-NYA)/2 + 1
      FX1 = FX0 + (SCALEX*SCALEF*RXPIX/NXPIX)
      FY1 = FY0 + (SCALEY*SCALEF*RYPIX/NYPIX)
C                                       Determine the bounding box.
C                                       Portrait.
      IF (ORIENT.EQ.1) THEN
         BNDBOX(1) = (FX0-ICHL)*PPD
         BNDBOX(2) = (FY0-ICHB)*PPD
         BNDBOX(3) = (FX1+ICHR)*PPD
         BNDBOX(4) = (FY1+ICHT)*PPD
C                                       Landscape.
      ELSE
         BNDBOX(1) = (FY0-ICHB)*PPD
         BNDBOX(2) = (FX0-ICHL)*PPD
         BNDBOX(3) = (FY1+ICHT)*PPD
         BNDBOX(4) = (FX1+ICHR)*PPD
         END IF
C                                       Begin PostScript header.
      NPAGE = NPAGE + 1
      IF (NPAGE.EQ.1) THEN
         CHBUF = '%!PS-Adobe-3.0 EPSF-3.0'
         IF (IVER2.GT.IVER1) CHBUF = '%!PS-Adobe-3.0'
         FIRST = .FALSE.
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Creator:
         CHBUF = '%%Creator: AIPS task LWPLA'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       CreationDate:
         CALL ZDATE (IT(1))
         CALL ZTIME (IT(4))
         CALL TIMDAT (IT(4), IT(1), ATIME, ADATE)
         CLOCK = ADATE(10:11) // '/' // ADATE(4:6) // '/' // ADATE(1:2)
     *      // ' ' // ATIME
         CHBUF = '%%CreationDate: ' // CLOCK
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Title:
         CHBUF = '%%Title: AIPS plot'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Pages:
         CHBUF = '%%Pages:(atend)'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       DocumentFonts:
         CHBUF = '%%DocumentFonts: ' // FONT(1:ITRIM(FONT))
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       BoundingBox:
         CHBUF = '%%BoundingBox:(atend)'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Paper size
         CHBUF = '%%DocumentPaperSizes: ' // PAPTYP(IPAPER)
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       For:
         WRITE (CHBUF,1240) '%%For: AIPS user number', NLUSER
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       EndComments:
         CHBUF = '%%EndComments'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       BeginProcSet:
         CHBUF = '%%BeginProcSet: lwpla.pro'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Move pen command.
         CHBUF = '   /m {moveto} def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Draw line command.
         CHBUF = '   /v {lineto currentpoint stroke moveto} def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Write text command.
         CHBUF = '   /c {rmoveto gsave currentpoint translate rotate'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C        CHBUF = ' 0 setgray'
C        CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C        IF (IERR.NE.0) GO TO 985
C                                       Fixed spaced font (Courier).
C        IF (IFONT.GE.20 .AND. IFONT.LE.29) THEN
            CHBUF = '       show grestore} def'
C                                       Proportionally spaced font.
C                                       Puts in too much space
C         ELSE
C           CHBUF = '       14 0 32 4 -1 roll widthshow grestore} def'
C           END IF
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Grey scale.
         CHBUF = '%                                   Grey scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '   /g {save hafpix hafpiy rmoveto currentpoint'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       translate pscalx pscaly scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       npix 1 8 [npix 0 0 1 0 0] ' //
     *      '{currentfile npix string readhexstring pop} image'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       restore} def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       RGB scale.
         CHBUF = '%                                    RGB scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '   /G {save hafpix hafpiy rmoveto currentpoint'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       translate pscalx pscaly scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       npix 1 8 [npix 0 0 1 0 0] ' //
     *      '{currentfile 3 npix mul string readhexstring pop}' //
     *      ' false 3 colorimage'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       restore} def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       CMYK scale.
         CHBUF = '%                                   CMYK scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '   /H {save hafpix hafpiy rmoveto currentpoint'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       translate pscalx pscaly scale'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       npix 1 8 [npix 0 0 1 0 0] ' //
     *      '{currentfile 4 npix mul string readhexstring pop}' //
     *      ' false 4 colorimage'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '       restore} def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Draw X command.
C        CHBUF = '   /x {-50 -50 rmoveto 100 100 rlineto' //
C     *     ' 0 -100 rmoveto -100 100 rlineto} def '
C        CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C        IF (IERR.NE.0) GO TO 985
C                                       EndProcSet.
         CHBUF = '%%EndProcSet'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Report frame coordinates.
         CHBUF = '%                Dots       BoundingBox'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = '%            (1/300 inch)   (1/72 inch)'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       EndProlog.
         CHBUF = '%%EndProlog'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       BeginSetup
C      IF (IPAPER.NE.8) THEN
C         CHBUF = '%%BeginSetup'
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C                                      PaperSize
C         CHBUF = '%%PaperSize: ' // PAPTYP(IPAPER)
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C                                       BeginPaperSize
C         CHBUF = '%%BeginPaperSize: ' // PAPTYP(IPAPER)
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C                                       paper size command
C         CHBUF = PAPTYP(IPAPER)
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C                                       EndPaperSize
C         CHBUF = '%%EndPaperSize'
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C                                       EndSetup
C         CHBUF = '%%EndSetup'
C         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C         IF (IERR.NE.0) GO TO 985
C         END IF
C                                       User start-hook.
         CHBUF = 'userdict /start-hook known {start-hook} if'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
C                                       Page IVER of NPAGE.
      FIRST = .FALSE.
      CALL LWIO ('FLUS', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
      WRITE (CHBUF,1240) '%%Page: ', IVER, NPAGE
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
      WRITE (MSGTXT,1235) IVER, NPAGE
      CALL MSGWRT (2)
C                                        Write extensions comment (if
C:                                       required)
      IF (IOBLK(IOPOS+5).EQ.12) COLRIT = .TRUE.
      IF (COLRIT) THEN
         CHBUF = '%%Extensions: CMYK'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CHBUF = '%%Requirements: color'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Attend: BoundingBox
      WRITE (CHBUF,1240) '%%PageBoundingBox: ', BNDBOX
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      EXTBOX(1) = MIN (EXTBOX(1), BNDBOX(1))
      EXTBOX(2) = MIN (EXTBOX(2), BNDBOX(2))
      EXTBOX(3) = MAX (EXTBOX(3), BNDBOX(3))
      EXTBOX(4) = MAX (EXTBOX(4), BNDBOX(4))
C                                       BLC.
      IF (ORIENT.EQ.1) THEN
         WRITE (CHBUF,1240) '%Frame BLC:', FX0, FY0, INT(FX0*PPD),
     *      INT(FY0*PPD)
      ELSE
         WRITE (CHBUF,1240) '%Frame BLC:', FX0, FY0, INT(FY0*PPD),
     *      INT(FX0*PPD)
         END IF
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       TRC.
      WRITE (CHBUF,1240) '%TRC:', FX1, FY1
      IF (ORIENT.EQ.1) THEN
         WRITE (CHBUF,1240) '%Frame TRC:', FX1, FY1, INT(FX1*PPD),
     *      INT(FY1*PPD)
      ELSE
         WRITE (CHBUF,1240) '%Frame TRC:', FX1, FY1, INT(FY1*PPD),
     *      INT(FX1*PPD)
         END IF
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       User beginning-of-page hook.
      CHBUF = 'userdict /bop-hook known {bop-hook} if'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Save state.
      CHBUF = '/vmsave save def'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Background color
      DO4C = (COLRIT) .AND. (DPARM(9).GT.0.0)
      IF (DOBG) THEN
         CALL SETRGB (10, 'FRIT', LWLUN, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1152) BNDBOX(1), BNDBOX(2), 'moveto'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1152) BNDBOX(3), BNDBOX(2), 'lineto'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1152) BNDBOX(3), BNDBOX(4), 'lineto'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1152) BNDBOX(1), BNDBOX(4), 'lineto'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1152) BNDBOX(1), BNDBOX(2), 'lineto'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         CHBUF = ' fill stroke'
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
C                                       Scale to points.
      CHBUF = '72 300 div dup scale'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Landscape mode?
      IF (ORIENT.EQ.2) THEN
         WRITE (CHBUF,1250) PAGESZ(1), ' 0 translate 90 rotate'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
      IF (ORIENT.EQ.3) THEN
         WRITE (CHBUF,1251) ' 0 ', (PAGESZ(1)+750),
     *      ' translate -90 rotate'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
C                                       Rounded joins and ends.
      CHBUF = '1 setlinejoin 1 setlinecap'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Line thickness.
      WRITE (CHBUF,1252) LLPEN, ' setlinewidth'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Load font.
      WRITE (CHBUF,1253) '/', FONT(1:ITRIM(FONT)),
     *   ' findfont', CLWSIZ, ' scalefont setfont'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Initialize.
      CHBUF = 'newpath gsave'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
C
      IOPOS = IOPOS + 5
C                                       force line type and its color
      LTYPE = 1
      CHBUF = '% following vectors are of type ' // CLTYPE(LTYPE)
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
      CALL SETRGB (LTYPE, 'FRIT', LWLUN, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       set header if not grey next
      GO TO 350
C-----------------------------------------------------------------------
C                                       Opcode 3, grey scale init.
C                                       parms pick up
 300  IGRLO = IOBLK(IOPOS+1)
      IGRHI = IOBLK(IOPOS+2)
      NPTSX = IOBLK(IOPOS+3)
      NPTSY = IOBLK(IOPOS+4)
      IF (OPCODE.EQ.13) THEN
         CALL H2CHR (2, 1, HOBLK(IOPOS+5), GPHFUN)
         CALL RCOPY (2, ROBLK(IOPOS+6), RANGE)
         IOPOS = IOPOS + 8
      ELSE IF (OPCODE.EQ.3) THEN
         GPHFUN = 'LN'
         RANGE(1,1) = IGRLO
         RANGE(2,1) = IGRHI
         IOPOS = IOPOS + 5
      ELSE
C                                       Re-default color scale
         IF (DOCOLR.LE.0.0) THEN
            CALL RFILL (12, 1.0, RGBCOL(1,1))
            CALL RFILL (9, 0.0, RGBCOL(1,6))
            COLSCL = 1.0
            IF (.NOT.DOBG) THEN
               CALL SETRGB (10, 'FRIT', LWLUN, IERR)
               IF (IERR.NE.0) GO TO 985
               TEMP = 300.0 / 72.0
               BNDSCL(1) = BNDBOX(1) * TEMP + 0.5
               BNDSCL(2) = BNDBOX(2) * TEMP + 0.5
               BNDSCL(3) = BNDBOX(3) * TEMP + 0.5
               BNDSCL(4) = BNDBOX(4) * TEMP + 0.5
               WRITE (CHBUF,1152) BNDSCL(1), BNDSCL(2), 'moveto'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               WRITE (CHBUF,1152) BNDSCL(3), BNDSCL(2), 'lineto'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               WRITE (CHBUF,1152) BNDSCL(3), BNDSCL(4), 'lineto'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               WRITE (CHBUF,1152) BNDSCL(1), BNDSCL(4), 'lineto'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               WRITE (CHBUF,1152) BNDSCL(1), BNDSCL(2), 'lineto'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               CHBUF = ' fill stroke'
               CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
               IF (IERR.NE.0) GO TO 985
               END IF
            CALL SETRGB (LTYPE, 'FRIT', LWLUN, IERR)
            IF (IERR.NE.0) GO TO 985
            END IF
         CALL H2CHR (2, 1, HOBLK(IOPOS+5), GPHFUN)
         CALL RCOPY (6, ROBLK(IOPOS+6), RANGE)
         IOPOS = IOPOS + 12
C                                        Write extensions comment (if
C:                                       required)
         IF (.NOT.COLRIT) THEN
            COLRIT = .TRUE.
            CHBUF = '%%Extensions: CMYK'
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            CHBUF = '%%Requirements: color'
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       check parameters
      IF ((IGRLO.NE.IGRHI) .AND. (NPTSX.GT.0) .AND. (NPTSY.GT.0)) THEN
         TOTPIX = REAL(NPTSX) * REAL(NPTSY)
         NOWPIX = 0
         IPER = 25
         LASTNP = 0
         IF (OPCODE.NE.12) DOGREY = .TRUE.
         IF (OPCODE.EQ.12) DO3COL = .TRUE.
         GROFF = - REAL(IGRLO)
         GRSCAL = 1.0 / (REAL(IGRHI) - REAL(IGRLO))
      ELSE
         WRITE (MSGTXT,1305) IGRLO, IGRHI, NPTSX, NPTSY
         CALL MSGWRT (7)
         END IF
C                                       Offset for negative coords
 350  FX0 = FX0 - (RX1-IX1) * SCALEX*SCALEF/NXPIX
      FY0 = FY0 - (RY1-IY1) * SCALEY*SCALEF/NYPIX
C                                       Display arcsec / mm info
      IF (AXTYP(LOCNUM).EQ.1) THEN
         ASPMM  = ABS (NXPIX*AXINC(1,LOCNUM)*3600.0*XLWDMM) /
     *      (SCALEX*SCALEF)
         YASPMM = ABS (NYPIX*AXINC(2,LOCNUM)*3600.0*YLWDMM) /
     *      (SCALEY*SCALEF)
         XYDIFF = ABS (ASPMM - YASPMM)
         DELTA = .025 * ASPMM
C                                       X and Y ASPMM about the same.
         IF (XYDIFF.LE.DELTA) THEN
            WRITE (MSGTXT,1350) ASPMM
C                                       Using different X and Y ASPMM
         ELSE
            WRITE (MSGTXT,1355) ASPMM
            CALL MSGWRT (4)
            WRITE (MSGTXT,1356) YASPMM
            END IF
         CALL MSGWRT (4)
C                                       Changed user's ASPMM.
         IF ((UASPMM.GT.0.0) .AND. (UASPMM.LT.MIN(ASPMM,YASPMM))) THEN
            MSGTXT ='Overriding user ASPMM in order to fit plot on page'
            CALL MSGWRT (4)
            END IF
         END IF
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 4, position cursor.
 400  IX = IOBLK(IOPOS+1)
      IY = IOBLK(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL LWIO ('FLUS', LWLUN, LWBUFF, IERR)
      CALL LWVEC (IX, IY, 1, LWLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 980
         IERRC = IERRC + 1
         GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 5, draw vector.
C                                       Opcode 10, draw dark vector.
C                                       Opcode 17, draw colored vector
 500  IX = IOBLK(IOPOS+1)
      IY = IOBLK(IOPOS+2)
      IOPOS = IOPOS + 3
C                                       draw dark
      IF ((DODARK.GT.0.0) .AND. (OPCODE.EQ.10)) THEN
         IF (LTYPE.LE.4) LTYPE = LTYPE + 4
         K = LTYPE
C                                       draw colored
      ELSE IF (OPCODE.EQ.17) THEN
         K = -1
C                                        Write extensions comment (if
C:                                       required)
         IF (.NOT.COLRIT) THEN
            COLRIT = .TRUE.
            CHBUF = '%%Extensions: CMYK'
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            CHBUF = '%%Requirements: color'
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
C                                       draw LTYPE bright line
      ELSE
         IF (LTYPE.GT.4) LTYPE = LTYPE - 4
         K = LTYPE
         END IF
      CALL SETRGB (K, 'WRIT', LWLUN, IERR)
      IF (IERR.NE.0) GO TO 985
      CALL LWVEC (IX, IY, 2, LWLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 980
         IERRC = IERRC + 1
         GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 6, write characters.
C                                       Opcode 14, inside characters
C                                       Opcode 15, dark characters
 600  INCHAR = IOBLK(IOPOS+1)
      IANGL = IOBLK(IOPOS+2)
      DX    = IOBLK(IOPOS+3)/100.0
      DY    = IOBLK(IOPOS+4)/100.0
C                                       Are all characters printable ?
      CALL H2CHR (INCHAR, 1, HOBLK(IOPOS+5), LCHBUF)
      NCHAR = ITRIM (LCHBUF)
      CHBUF = LCHBUF
      IX = JTRIM (LCHBUF)
      PRTOK = IX.EQ.NCHAR
      CALL ALLPRT (NCHAR, CHBUF, ACTCHR)
C                                       Fixed spaced font (Courier).
      IF ((IFONT.GE.20) .AND. (IFONT.LE.29)) ACTCHR = NCHAR
      IF (.NOT.PRTOK) ACTCHR = NCHAR
C                                       offsets
      IF (IANGL.EQ.1) THEN
         IANGL = 90
         IX = IROUND ((DX+1.0) * CSIZLW(1))
         IY = IROUND ((DY - ACTCHR + 1.) * CSIZLW(1))
      ELSE
         IANGL = 0
         IF ((DX.LT.0) .AND. (NCHAR-ACTCHR.GT.0.01)) THEN
            ACTCHR = (DX + NCHAR - ACTCHR) * CSIZLW(1)
            IX = IROUND (ACTCHR)
         ELSE
            IX = IROUND (DX * CSIZLW(1))
            END IF
         IY = IROUND (DY * CSIZLW(2))
         END IF
C                                       Overlap the last string
      IF ((LASTM(2).EQ.LASTR(2)) .AND. (IY.EQ.LASTR(4))) THEN
         OMIT = (LASTM(1)+IX).LE.LASTR(5)
      ELSE
         OMIT = .FALSE.
         END IF
      IF (OMIT) THEN
         NOMIT = NOMIT + 1
      ELSE
         LASTR(1) = LASTM(1)
         LASTR(2) = LASTM(2)
         LASTR(3) = IX
         LASTR(4) = IY
         LASTR(5) = LASTM(1) + IX + NCHAR * CSIZLW(1)
         END IF
C                                       change colors if needed
C                                       dark characters
      IF ((DODARK.GT.0.0) .AND. (OPCODE.EQ.15)) THEN
         IF (LTYPE.LE.4) LTYPE = LTYPE + 4
         K = LTYPE
      ELSE IF (OPCODE.EQ.14) THEN
         IF (LTYPE.GT.4) LTYPE = LTYPE - 4
         K = LTYPE
      ELSE IF (OPCODE.EQ.6) THEN
         K = 9
         END IF
      CALL SETRGB (K, 'WRIT', LWLUN, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       If all are printable,
      IF (PRTOK) THEN
C                                       Transfer to temporary array
         TMPBUF = CHBUF(1:NCHAR)
C                                       Add "(" and ")" around string
C                                       and escape any ( and )
         IF (OMIT) THEN
            J = 3
            CHBUF = '%('
         ELSE
            J = 2
            CHBUF = '('
            END IF
         DO 605 I = 1,NCHAR
            IF ((TMPBUF(I:I).EQ.'(') .OR. (TMPBUF(I:I).EQ.')')) THEN
               CHBUF(J:J) = '\\'
               J= J + 1
               END IF
            CHBUF(J:J) = TMPBUF(I:I)
            J = J + 1
 605        CONTINUE
         CHBUF(J:) = ')'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       Write string in Hex
C                                       Not all printable, add comment
      ELSE
         NCH = MIN (72, NCHAR)
         TMPBUF = '% ' // CHBUF(1:NCH)
         CALL LWIO ('FRIT', LWLUN, TMPBUF, IERR)
         IF (IERR.NE.0) GO TO 985
C                                       Encode Characters in HEX
         DO 610 I = 1,NCHAR
            ICHBUF = ICHAR (CHBUF(I:I))
            CALL ZHEX (ICHBUF, 2, XCHBUF(I))
 610        CONTINUE
C                                       Put Hex into output string
         IF (OMIT) THEN
            CHBUF = '%<'
            N = 3
         ELSE
            CHBUF = '<'
            N = 2
            END IF
         DO 620 J = 1,NCHAR,36
            K = MIN (J+35,NCHAR)
            WRITE (CHBUF(N:),1610) (XCHBUF(I), I=J,K)
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
            IF (K.EQ.NCHAR) THEN
               CALL LWIO ('WRIT', LWLUN, '>', IERR)
            ELSE
               CALL LWIO ('FLUS', LWLUN, CHBUF, IERR)
               END IF
            CHBUF(N-1:) = ' '
 620        CONTINUE
         END IF
C                                       Write command.
      IF (OMIT) THEN
         N = 2
         CHBUF = '%'
      ELSE
         N = 1
         END IF
      WRITE (CHBUF(N:),1620) IANGL, IX, IY, ' c'
      CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IOPOS = IOPOS + 5 + ((INCHAR - 1) / 4) + 1
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 7, write grey scale.
 700  IF (((OPCODE.EQ.7) .AND. (.NOT.DOGREY)) .OR. ((OPCODE.EQ.11) .AND.
     *   (.NOT.DO3COL))) THEN
         GRYERR = GRYERR + 1
C                                       Update IO position. Read
C                                       proper RRN if necessary.
         IF (OPCODE.EQ.7) THEN
            IOPOS = IOPOS + 3 + IOBLK(IOPOS+1)
         ELSE
            IOPOS = IOPOS + 3 + 3 * IOBLK(IOPOS+1)
            END IF
         IF (IOPOS.LE.256) GO TO 60
            INCRRN = (IOPOS-1)/256
            IORRN = IORRN + INCRRN
            IOPOS = IOPOS - 256*INCRRN
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
            IF (IERR.EQ.0) GO TO 60
            GO TO 985
         END IF
C                                       Get grey values from record(s).
      NPIX = IOBLK(IOPOS+1)
      MPIX = NPIX
      IF (OPCODE.EQ.11) MPIX = 3*NPIX
      IANGL = IOBLK(IOPOS+2)
      IOPOS = IOPOS + 3
      IF (NPIX.LE.0) GO TO 50
      IF (NPIX.NE.LASTNP) THEN
         LASTNP = NPIX
         IF (IANGL.EQ.1) THEN
            PSCALX = INT (SCALEX*SCALEF/NXPIX + 1.0)
            PSCALY = INT (SCALEY*SCALEF/NYPIX*REAL(NPIX) + 1.0)
         ELSE
            PSCALX = INT (SCALEX*SCALEF/NXPIX*REAL(NPIX) + 1.0)
            PSCALY = INT (SCALEY*SCALEF/NYPIX + 1.0)
            END IF
         HAFPIX = -NINT (SCALEX*SCALEF/NXPIX/2.0)
         HAFPIY = -NINT (SCALEY*SCALEF/NYPIX/2.0)
C                                       Greyscale parameters.
         WRITE (CHBUF,1700) '/hafpix', HAFPIX, ' def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1700) '/hafpiy', HAFPIY, ' def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1700) '/pscalx', PSCALX, ' def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1700) '/pscaly', PSCALY, ' def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         WRITE (CHBUF,1700) '/npix  ', NPIX, ' def'
         CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
C                                       Set bits for each pixel.
      GREYS = ' '
      NN = 1
      DONEG = .FALSE.
      MCOLR = 1
      MCP = 3
      IF (OPCODE.EQ.7) THEN
         IF ((DOOFM) .AND. (DO3C)) THEN
            MCOLR = 3
         ELSE
            MCP = 1
            END IF
         END IF
C                                       For all pixels in row
      NCP = 0
      VCP = 255
      INVRT = ((OPCODE.EQ.11) .OR. (MCOLR.EQ.3)) .AND. (DO4C)
      DO 750 N = 1,MPIX
C                                       Need another buffer of G values
C                                       Read more graphics
         IF (IOPOS.GT.256) THEN
            IORRN = IORRN + 1
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBLK, IERR)
            IF (IERR.NE.0) GO TO 985
            IOPOS = 1
            END IF
C
         V = MAX (0.0, MIN ((REAL(IOBLK(IOPOS))+GROFF)*GRSCAL, 1.0))
         V = MAX (DPARM(1), MIN (DPARM(2), V))
         V = MAX (0.0, MIN (DPARM(3) * V + DPARM(4), 1.0))
         IF (OPCODE.EQ.11) V = 1.000 - V
         IF (MOD(ITRN,2).EQ.0) V = 1.000 - V
         IF ((ITRN.EQ.3) .OR. (ITRN.EQ.4)) THEN
            V = LOG (9.0*V + 1.0) / LOG (10.0)
         ELSE IF ((ITRN.EQ.5) .OR. (ITRN.EQ.6)) THEN
            V = LOG (99.0*V + 1.0) / LOG (100.0)
         ELSE IF ((ITRN.EQ.6) .OR. (ITRN.EQ.7)) THEN
            V = SQRT (V)
            END IF
         RV(1) = V
C                                       Do OFM
         IF ((DOOFM) .AND. (OPCODE.EQ.7)) THEN
            CV = V * (NOFM-1.0) + 1.5
            RV(1) = ROFM(CV)
            IF (MCOLR.EQ.3) RV(1) = 1.0 - RV(1)
            RV(2) = 1.0 - GOFM(CV)
            RV(3) = 1.0 - BOFM(CV)
            END IF
C                                       Loop over OFM colors
         DO 725 ICOLR = 1,MCOLR
            NCP = NCP + 1
            IF (NCP.GT.MCP) NCP = 1
C                                       Scale values to be represented
C                                       by characters of ASCII set
            CV = 256 - NINT (RV(ICOLR) * 255.9998 + 0.5001)
            IF (GAMMA(NCP).NE.1.0) THEN
               TEMP = (CV/255.0) ** GAMMA(NCP)
               CV = 255.0 * TEMP + 0.5
               END IF
            IF (INVRT) CV = 255 - CV
            VCP = MIN (VCP, CV)
C                                       Record for printing
            CALL ZHEX (CV, 2, GREYS(NN:NN+1))
            NN = NN + 2
            IF (NN.GT.72) THEN
               IF (.NOT.DONEG) THEN
C                                       Grey scale macro.
                  IF ((OPCODE.EQ.11) .OR. (MCOLR.EQ.3)) THEN
                     TMPBUF = ' G'
                     IF (DO4C) TMPBUF = ' H'
                  ELSE
                     TMPBUF = ' g'
                     END IF
                  CALL LWIO ('FRIT', LWLUN, TMPBUF, IERR)
                  IF (IERR.NE.0) GO TO 985
                  END IF
C                                       Write out greyscale.
               JNN = NN - 1
               CALL LWIO ('FRIT', LWLUN, GREYS(:JNN), IERR)
               IF (IERR.NE.0) GO TO 985
               DONEG = .TRUE.
               NN = 1
               GREYS = ' '
               END IF
 725        CONTINUE
C                                       Update index to graphics buff
         IOPOS = IOPOS + 1
C                                       4-color black
         IF ((DO4C) .AND. (NCP.GE.3)) THEN
            NCP = 0
C                                       DEBUG WITH MIN
            TEMP = (VCP/255.0) ** BGAMMA
            VCP = 255.0 * TEMP + 0.5
            CALL ZHEX (VCP, 2, GREYS(NN:NN+1))
            VCP = 255
            NN = NN + 2
            IF (NN.GT.72) THEN
               IF (.NOT.DONEG) THEN
C                                       Grey scale macro.
                  IF ((OPCODE.EQ.11) .OR. (MCOLR.EQ.3)) THEN
                     TMPBUF = ' G'
                     IF (DO4C) TMPBUF = ' H'
                  ELSE
                     TMPBUF = ' g'
                     END IF
                  CALL LWIO ('FRIT', LWLUN, TMPBUF, IERR)
                  IF (IERR.NE.0) GO TO 985
                  END IF
C                                       Write out greyscale.
               JNN = NN - 1
               CALL LWIO ('FRIT', LWLUN, GREYS(:JNN), IERR)
               IF (IERR.NE.0) GO TO 985
               DONEG = .TRUE.
               NN = 1
               GREYS = ' '
               END IF
            END IF
 750     CONTINUE
      IF (NN.GT.2) THEN
         IF (.NOT.DONEG) THEN
            IF ((OPCODE.EQ.11) .OR. (MCOLR.EQ.3)) THEN
               TMPBUF = ' G'
               IF (DO4C) TMPBUF = ' H'
            ELSE
               TMPBUF = ' g'
               END IF
            CALL LWIO ('FRIT', LWLUN, TMPBUF, IERR)
            IF (IERR.NE.0) GO TO 985
            END IF
C                                       Write out greyscale.
         JNN = NN - 1
         CALL LWIO ('FRIT', LWLUN, GREYS(:JNN), IERR)
         IF (IERR.NE.0) GO TO 985
         END IF
C                                       Measure progress
      NOWPIX = NOWPIX + NPIX
      IPER2 = INT (100.0 * NOWPIX / TOTPIX)
C                                       Write progress report.
      IF (IPER2.LT.IPER) GO TO 50
         WRITE (MSGTXT,1790) IPER
         CALL MSGWRT (1)
         IPER = IPER + 25
C                         suppress last message
         IF (IPER.GE.100) IPER = 200
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 8, put misc info in
C                                       image catalog.
 800  IOPOS = IOPOS + 2 + IOBLK(IOPOS+1)
      GO TO 50
C-----------------------------------------------------------------------
C                                       Opcode 18, user comment
 810  NCHAR = IOBLK(IOPOS+1)
      CALL H2CHR (NCHAR, 1, IOBLK(IOPOS+2), COMNT)
      CHBUF = '% ' // COMNT
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 985
      IOPOS = IOPOS + 2 + (NCHAR+3)/4
      GO TO 50
C-----------------------------------------------------------------------
C                                       Invalid opcode.
 900  WRITE (MSGTXT,1900) OPCODE
      CALL MSGWRT (8)
C                                       Draw it in the paper.
 910  CALL LWIO ('FLUS', LWLUN, CHBUF, IERR)
      CHBUF = 'stroke'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       Restore state.
      CHBUF = 'vmsave restore'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       Copies
      IF (COPIES.GT.1) THEN
         CHBUF = 'copypage'
         DO 915 I = 2,COPIES
            CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
 915        CONTINUE
         END IF
C                                       Print it.
      CHBUF = 'showpage'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       User end-of-page hook.
      CHBUF = 'userdict /eop-hook known {eop-hook} if'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       omitted strings
      IF (NOMIT.GT.0) THEN
         WRITE (MSGTXT,1915) NOMIT
         CALL MSGWRT (6)
         END IF
C                                       close PL file
 920  CALL ZCLOSE (LUN, FIND, IERR)
 925  IVER = IVER + 1
      IF (IVER.LE.IVER2) GO TO 10
C                                       Trailer.
      IF (NPAGE.LE.INPAGE) THEN
         MSGTXT = 'NO OUTPUT PRODUCED'
         CALL MSGWRT (8)
         GO TO 993
         END IF
      CHBUF = '%%Trailer'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       Attend: Pages
      WRITE (CHBUF,1240) '%%Pages: ', NPAGE
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       Attend: BoundingBox
      WRITE (CHBUF,1240) '%%BoundingBox: ', EXTBOX
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       User end-hook.
      CHBUF = 'userdict /end-hook known {end-hook} if'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
C                                       PostScript end-of-file.
      CHBUF = '%%EOF'
      CALL LWIO ('FRIT', LWLUN, CHBUF, IERR)
      GO TO 990
C                                       Write error.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
      GO TO 990
C                                       Disk error.
 985  WRITE (MSGTXT,1985) IERR
      CALL MSGWRT (8)
C                                       Close files.
 990  CALL LWIO ('CLOS', LWLUN, CHBUF, IERR)
 993  IF (GRYERR.EQ.0) GO TO 995
         WRITE (MSGTXT,1993) GRYERR
         CALL MSGWRT (3)
 995  IF ((IERRC.LE.0) .AND. (IERRCH.LE.0)) GO TO 999
         WRITE (MSGTXT,1995) IERRC
         IF (IERRC.GT.0) CALL MSGWRT (3)
         WRITE (MSGTXT,1996) IERRCH
         IF (IERRCH.GT.0) CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('U',F4.1,'x',F4.1)
 1001 FORMAT ('Font exceeds maximum (',I3,'), maximum used')
 1005 FORMAT ('LWDRW: ERROR',I4,' OPENING LASERWRITER FILE/DEVICE')
 1010 FORMAT ('LWDRW: PLOT FILE VERSION',I6,' NOT FOUND')
 1152 FORMAT (2I6,1X,A)
 1200 FORMAT ('LWDRW: ERROR X,Y=',2E10.3,' PLOT AREA')
 1205 FORMAT ('LWDRW: ERROR NFPX,NFPY=',2I6,' DOTS AVAILABLE')
 1220 FORMAT ('Plot reduced in size by factor',F6.3)
 1235 FORMAT ('Writing plot file version',I6,' to page',I5)
 1240 FORMAT (A,4I7)
 1250 FORMAT (I6,A)
 1251 FORMAT (A,I6,A)
 1252 FORMAT (I3,A)
 1253 FORMAT (3A,I5,A)
 1305 FORMAT ('LWDRW: BAD PARMS FROM GINITG:',4I7)
 1350 FORMAT ('Using',1PE12.5,' arcsec per mm')
 1355 FORMAT ('Using',1PE12.5,' arcsec per mm for x axis')
 1356 FORMAT ('Using',1PE12.5,' arcsec per mm for y axis')
 1610 FORMAT (36A2)
 1620 FORMAT (I4,2I6,A)
 1700 FORMAT (A,I6,A)
 1790 FORMAT (I3,'% of grey scale pixels plotted')
 1980 FORMAT ('LWDRW: LASERWRITER WRITE ERROR')
 1900 FORMAT ('LWDRW: INVALID OPCODE',I6,' IN GRAPH FILE')
 1915 FORMAT ('LWDRW: OMITTED',I3,' STRINGS DUE TO OVERLAP')
 1985 FORMAT ('LWDRW: DISK IO RELATED ERROR',I3)
 1993 FORMAT ('LWDRW: ',I5,' GREY SCALE OPCODES IGNORED')
 1995 FORMAT (I8,' VECTORS TRUNCATED AT EDGES')
 1996 FORMAT (I4,' CHARACTER STRINGS OMITTED AT EDGES')
      END
      SUBROUTINE LWVEC (IX, IY, IN, LWLUN, IERR)
C-----------------------------------------------------------------------
C   This routine will put control characters, and X and Y coordinates
C   into the LaserWriter output file.
C   INPUTS:   IX    I   X coordinate value.
C             IY    I   Y coordinate value.
C             IN    I   control value:
C                           1 = Scale IX and IY and move to position
C                           2 = Scale IX and IY, and draw line.
C                           3 = IX and IY are not scaled, move to
C                               position
C                           4 = no scaling, draw line
C             LWLUN   I   LUN of LaserWriter file
C   Output:   IERR    I   Error code: 0 => ok
C                                    10 => vector truncated
C                                    else I/O error
C-----------------------------------------------------------------------
      CHARACTER  COMAND(2)*2, CHBUF*40
      INTEGER   IX, IY, IN, LWLUN, IERR
      INTEGER   IXN, IYN, X, Y, INC
      LOGICAL   ISERR
      REAL      ALPHA, BETA
      INCLUDE 'LWPLA.INC'
      DATA COMAND /' m', ' v'/
C-----------------------------------------------------------------------
      ISERR = .FALSE.
C                                       Set last X and Y.
      IXN = IX
      IYN = IY
C                                       If IN = 1 or 2 do scaling.
      IF (IN.LE.2) THEN
         IXN = IX*SCALEX + FX0 + 0.5
         IYN = IY*SCALEY + FY0 + 0.5
         END IF
C                                       Draw line
      INC = 1
C                                       If IN = 1 or 3 'dark vector'.
      IF ((IN.NE.1) .AND. (IN.NE.3)) INC = 2
      IF (INC.EQ.2) GO TO 30
C                                       Check start point
         X = MAX (1, MIN (PAGEX, IXL))
         Y = MAX (1, MIN (PAGEY, IYL))
         IF ((X.NE.IXL) .OR. (Y.NE.IYL)) THEN
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X-IXN)/REAL(IXL-IXN)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y-IYN)/REAL(IYL-IYN)
            ALPHA = MIN (ALPHA, BETA)
            X = IXN + ALPHA * (IXL-IXN) + 0.5
            Y = IYN + ALPHA * (IYL-IYN) + 0.5
            WRITE (CHBUF, 2000) X, Y, COMAND(INC)
            CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 90
            END IF
C                                       Check end point
 30      X = MAX (1, MIN (PAGEX, IXN))
         Y = MAX (1, MIN (PAGEY, IYN))
         IF ((X.NE.IXN) .OR. (Y.NE.IYN)) THEN
            IF ((IXN.LT.1) .AND. (IXL.LT.1)) GO TO 90
            IF ((IYN.LT.1) .AND. (IYL.LT.1)) GO TO 90
            IF ((IXN.GT.PAGEX) .AND. (IXL.GT.PAGEX)) GO TO 90
            IF ((IYN.GT.PAGEY) .AND. (IYL.GT.PAGEY)) GO TO 90
            ISERR = .TRUE.
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X = IXL + ALPHA * (IXN-IXL) + 0.5
            Y = IYL + ALPHA * (IYN-IYL) + 0.5
            END IF
C
         WRITE (CHBUF, 2000) X, Y, COMAND(INC)
         CALL LWIO ('WRIT', LWLUN, CHBUF, IERR)
         IF (INC.EQ.1) THEN
            LASTM(1) = X
            LASTM(2) = Y
            END IF
C                                         Save new pos
 90   IXL = IXN
      IYL = IYN
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT (2I6,A2)
      END
      SUBROUTINE LWIO (OP, LWLUN, BUF, IERR)
C-----------------------------------------------------------------------
C   LWIO moves characters to the buffer and if needed causes an I/O
C   to occur to the LaserWriter print file.
C   Inputs:
C      LWLUN   I       LUN of file
C      NCH     I       Number of new characters
C      BUF     C*(*)   Character string
C   Output:
C      IERR    I       Error code: 0 -> ok, else I/O error
C-----------------------------------------------------------------------
      CHARACTER OP*4, BUF*(*)
      INTEGER   LWLUN, IERR
C
      INTEGER   NT, LTOT, NCH, JTRIM, XX(5)
      INCLUDE 'LWPLA.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       execute the opcode
      IF (OP.EQ.'OPEN') THEN
         XX(1) = 0
         XX(2) = 1000000
         XX(3) = 1000000
         XX(4) = -1000000
         XX(5) = -1000000
         CALL ZLWIO ('OPEN', LWLUN, OUTFIL, XX, LWBUFF, IERR)
C                                       Init count of chars in buffer
         LWPOS = 0
         NPAGE = XX(1)
         CALL COPY (4, XX(2), EXTBOX)
      ELSE IF ((FIRST) .AND. ((OP.EQ.'WRIT') .OR. (OP.EQ.'FRIT') .OR.
     *   (OP.EQ.'FLUS'))) THEN
         MSGTXT = 'Warning: output requested before first correct line'
         CALL MSGWRT (6)
         MSGTXT = 'Line=' // BUF
         CALL MSGWRT (6)
C                                       If writing a command
      ELSE IF ((OP.EQ.'WRIT') .OR. (OP.EQ.'FRIT')) THEN
         NCH = JTRIM (BUF)
         IF (NCH.GT.0) THEN
C                                       Limit to 80 characters per line.
            IF ((LWPOS.GT.0) .AND. ((LWPOS+NCH.GT.80) .OR.
     *         (OP.EQ.'FRIT'))) THEN
               CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF, IERR)
               IF (IERR.NE.0) GO TO 999
               LWPOS = 0
               END IF
C                                       Begin 1 char at a time write
            NT = 0
 110        IF (NT.LT.NCH) THEN
C                                       Limit to 80 characters per line.
               IF (LWPOS.GE.80) THEN
                  CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF,
     *               IERR)
                  IF (IERR.NE.0) GO TO 999
                  LWPOS = 0
                  END IF
C                                       Just move characters
               LTOT = NCH - NT
               IF ((LWSIZE - LWPOS).LT.LTOT) LTOT = LWSIZE - LWPOS
               LWBUFF(LWPOS+1:) = BUF(NT+1:LTOT)
               LWPOS = LWPOS + LTOT
               NT = NT + LTOT
               GO TO 110
               END IF
C                                       flush
            IF ((LWPOS.GT.0) .AND. (OP.EQ.'FRIT')) THEN
               CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF, IERR)
               IF (IERR.NE.0) GO TO 999
               LWPOS = 0
               END IF
            END IF
C                                       If flushing remaining output
      ELSE IF (OP.EQ.'FLUS') THEN
C                                       If characters to flush
         IF (LWPOS.GT.0) THEN
            CALL ZLWIO ('WRIT', LWLUN, OUTFIL, LWPOS, LWBUFF, IERR)
            IF (IERR.NE.0) GO TO 999
            LWPOS = 0
            END IF
      ELSE IF (OP.EQ.'CLOS') THEN
         CALL ZLWIO ('CLOS', LWLUN, OUTFIL, 1, LWBUFF, IERR)
C                                       Else un-recognised opcode
      ELSE
         IERR = 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE ALLPRT (NCHAR, CHBUF, ACTCHR)
C-----------------------------------------------------------------------
C   ALLPRT determines whether all characters in the input string are OK
C   to print as ascii text on a postscript printer.  Parenthesis are
C   sometimes confused with PostScript control characters.
C   Inputs: NCHAR    I        Number of characters in the string
C           CHBUF    C*256    Character string
C   Output: ACTCHR   R        reduction for small characters
C-----------------------------------------------------------------------
      INTEGER   NCHAR
      CHARACTER CHBUF*(*)
      REAL      ACTCHR
C
      INTEGER   ICH, IOK, OKSMAL
      PARAMETER (OKSMAL=10)
      CHARACTER  OKSMAC*10, ONECHR*1
C                        12345678901234567890123456
      DATA      OKSMAC /'.-, :;()"'''/
C-----------------------------------------------------------------------
      ACTCHR = NCHAR
C                                       For all Chars in string
      DO 100 ICH = 1,NCHAR
C                                       Work with ONE character
         ONECHR = CHBUF(ICH:ICH)
C                                       small characters
         DO 10 IOK = 1,OKSMAL
            IF (ONECHR.EQ.OKSMAC(IOK:IOK)) THEN
               ACTCHR = ACTCHR - 0.33
               GO TO 100
               END IF
 10         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETRGB (IC, OP, LUN, IERR)
C-----------------------------------------------------------------------
C   Prepares the set color command
C   Inputs:
C      IC      I       Color number 1-10, -1 => 3-color
C      OP      C*4     'FRIT', 'WRIT'
C      LUN     I       LUN to use
C   Outputs:
C      IERR    I       Error code
C-----------------------------------------------------------------------
      INTEGER   IC, LUN, IERR
      CHARACTER OP*4
C
      REAL      CC, CM, CY, CK, R, G, B
      CHARACTER LSTRNG*36, CSTRNG*36
      INCLUDE 'LWPLA.INC'
      SAVE LSTRNG
      DATA LSTRNG /' '/
C-----------------------------------------------------------------------
      IF ((IC.GE.1) .AND. (IC.LE.10)) THEN
         R = RGBCOL(1,IC)
         G = RGBCOL(2,IC)
         B = RGBCOL(3,IC)
      ELSE
         R = THREEC(1)
         G = THREEC(2)
         B = THREEC(3)
         END IF
C                                       changed?
      IF ((ABS(R-CURCOL(1)).LT.0.001) .AND. (ABS(G-CURCOL(2)).LT.0.001)
     *   .AND. (ABS(B-CURCOL(3)).LT.0.001)) THEN
         IERR = 0
      ELSE
         CURCOL(1) = R
         CURCOL(2) = G
         CURCOL(3) = B
C                                       CMYK
         IF (DO4C) THEN
            CC = 1.0 - R
            CM = 1.0 - G
            CY = 1.0 - B
            IF ((CC.EQ.1.0) .AND. (CM.EQ.1.0) .AND. (CY.EQ.1.0)) THEN
               CK = 1.0
            ELSE
               CK = 0.0
               END IF
            WRITE (CSTRNG,1150) CC, CM, CY, CK
C                                       RGB
         ELSE IF (COLRIT) THEN
            WRITE (CSTRNG,1151) R, G, B
C                                       B & W
         ELSE
            WRITE (CSTRNG,1152) R
            END IF
         IF (CSTRNG.NE.LSTRNG) THEN
            LSTRNG = CSTRNG
            CALL LWIO (OP, LUN, CSTRNG, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT (3F6.3,F4.1,' setcmykcolor')
 1151 FORMAT (3F6.3,' setrgbcolor')
 1152 FORMAT (F6.3,' setgray')
      END
