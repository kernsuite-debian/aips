LOCAL INCLUDE 'CANPL.INC'
      REAL      SEQIN, DSKIN, VERSN, ASPMM, COPIES, XXPEN, DPARM(10),
     *   TRANFN(6), SCALEX, SCALEY, AMASK(256), RNRAND
      HOLLERITH XNAMIN(3), XCLSIN(2), XOUTFL(12), XOPTYP, XTRANF(6)
      CHARACTER NAMIN*12, CLSIN*6, OUTFIL*48, OPTYPE*4, CTRANF*2,
     *   CANBUF*80
      INTEGER GOPR, ITRN, CANPOS, CANSIZ, IXL, IYL, IX0, IY0, MAXCAN(2),
     *   MXD, MYD, MRAND(100), NRAND, PRAND, QRAND, JRAND
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, ASPMM,
     *   COPIES, XXPEN, XOUTFL, XOPTYP, XTRANF, DPARM, TRANFN, GOPR,
     *   ITRN
      COMMON /CHPARM/ NAMIN, CLSIN, OUTFIL, OPTYPE, CTRANF
C                                       IMPORTANT NOTE:
C                                       the variables in canpl are not
C                                       what they appear: DPARM here
C                                       starts with AIPS DPARM(3);
C                                       TRANFN includes several adverbs
C                                       plus the first two words of
C                                       DPARM.
      COMMON /CANPCL/ CANPOS, CANSIZ, IXL, IYL, IX0, IY0, SCALEX,
     *   SCALEY, MAXCAN, MXD, MYD, AMASK
      COMMON /CANPCH/ CANBUF
      COMMON /RANDMM/ RNRAND, MRAND, NRAND, PRAND, QRAND, JRAND
LOCAL END
      PROGRAM CANPL
C-----------------------------------------------------------------------
C! Task to pass plot (PL) files to a Canon LBP-8 laser printer
C# Plot-appl Plot-util Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2009, 2015
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
C   CANPL is a program for the AIPS graphics package.  The program runs
C   as a detached task initiated from AIPS.  First a cataloged file is
C   found using data passed from AIPS.  The list of associated files is
C   searched for a plot file corresponding to the version number.  The
C   graphics commands are converted to character strings appropriate to
C   the Canon device and written to a file which "prints" at the end.
C
C   Inputs:  (from AIPS)
C      INNAME  R(3)   Name of primary file.
C      INCLASS R(2)   Class of primary file
C      INSEQ   R      Sequence number of primary file.
C      INDISK  R      Disk volume number. 0 means try all.
C      INVERS  R      Extension file version number. 0 ->
C                     use the highest version number.
C      ASPMM   R      Arc sec per millimeter. 0 -> self scale.
C      COPIES  R      Number of copies desired.
C      LPEN    R      Pen width in dots.  0 => 3.
C      OUTFILE R(12)  Output file name
C      OPTYPE  R      'NONE' ignore grey scale commands
C                     'MBIT' use sine pattern to build a font
C                     'BITS' use sine pattern to determine each dot
C                     'QRAN' use random numbers to build character font
C                     'MRAN' as 'QRAN' but with 78 levels not 26
C                     'RAND' use random numbers to determine each dot
C      FUNCTYPE  R    Convert after clipping by 'NE' negative linear,
C                     'LG' log, 'NG' negative log, 'PL' power law,
C                     'NL' negative power law.
C      FACTOR  R      Power law index for use with 'PL' or 'NL'
C                     FUNCTYPE's.
C      AXVAL   R(2)   Inflection point for transition from 1/FACTOR
C                     to FACTOR power law (0,0) => only use FACTOR
C      DPARM  R(10)   (1,2) Min, max clipping range 0 <= min<max <= 1.
C                           before FUNCTYPE, then
C                     (3,4) rescale G = DPARM(3)*G + DPARM(4) and
C                           clip again to 0 to 1.
C                  Parms for bit opcodes only:
C                     (5) <=0 -> Ignore rest of DPARM, use Gaus
C                         1 -> use DPARM for gaussian 'BIT ' mask
C                         2 -> ignore rest of DPARM, use sine
C                         3 -> use DPARM for sine wave 'BIT ' mask
C                     (6) width of mask (dots)
C                     (7) width of Gaussian, sine 0 => 1
C                     (8) x-shift of Gaus/sin in mask
C                     (9) y-shift of Gaus/sin in mask
C                     (10) number of grey-scale levels
C   Note: the adverbs are declared internally somewhat differently.
C   FACTOR, AXVAL -> TRANFN(2-4)
C   DPARM(1,2) -> TRANFN(5,6) and DPARM(3,10) -> DPARM(1,8).  the other
C   two positions in DPARM are declared however and used internally.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   BUFFER(256), LUN, FIND, IRET, IERR
      LOGICAL   QUICK
      INCLUDE 'CANPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGNAM /'CANPL '/
      DATA LUN /26/
C-----------------------------------------------------------------------
C                                       Initialize the io parameters.
      CALL CANINI (PRGNAM, LUN, FIND, QUICK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Write to graph file.
      CALL CANDRW (LUN, FIND, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 990
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         GO TO 990
C                                       Close graph file.
 990  CALL ZCLOSE (LUN, FIND, IERR)
 995  CALL DIETSK (IRET, QUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR WRITING TO CANON DEVICE')
      END
      SUBROUTINE CANINI (PRGNAM, LUN, FIND, QUICK, BUFFER, IRET)
C-----------------------------------------------------------------------
C   CANINI does the initialization for CANPL, getting parameters,
C   locating the plot file, and opening it.
C   Inputs: PRGNAM   C*6      Task name
C           LUN      I        LUN to use for plot file
C   Output: FIND     I        FTAB pointer for plot file
C           QUICK    L        Quick return desired or not.
C           BUFFER   I(256)   Plot buffer
C           IRET     I        Error code: not 0 => quit.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   LUN, FIND
      LOGICAL   QUICK
      INTEGER   BUFFER(256), IRET
C
      CHARACTER TYPIN*2, STATUS*4, GFILE*48, OKOPS(6)*4, OKTRN(5)*2
      INTEGER  CATERR, IERR, NPARMS, SLOT, IVER, I, IVOL, ISEQ, USER,
     *   IROUND, NOPS, NTRN
      LOGICAL   NOMAP, NOEXCL, WAIT, EQUAL
      INCLUDE 'CANPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NOPS, OKOPS /6, 'NONE','MBIT','BITS','QRAN','MRAN','RAND'/
      DATA NTRN, OKTRN /5, 'NE','LG','NG','PL','NL'/
      DATA NOMAP, NOEXCL, WAIT /.FALSE.,.FALSE.,.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
C                                       Get input values from AIPS.
      NPARMS = 38
      CALL GTPARM (PRGNAM, NPARMS, QUICK, XNAMIN, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         END IF
      IF (QUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Force number of grey-scale
C                                       levels to be between 2 and 78
C                                       inclusive:
      IF ((DPARM(8) .LT. 2.0) .OR. (DPARM(8) .GT .78.0)) DPARM(8) = 78.0
C                                       Set DPARMs not set by GTPARM
      DPARM(9) = 1.
      DPARM(10) = 1.
C                                       Hollerith -> character
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      CALL H2CHR (2, 1, XTRANF(1), CTRANF)
      DO 12 I = 1, 6
         TRANFN(I) = XTRANF(I)
 12      CONTINUE
C                                       Clip power law, inflec. point
      IF ((TRANFN(2).LT.0.001) .OR. (TRANFN(2).GT.999.)) TRANFN(2) = 1.0
      IF ((TRANFN(3).LT.0.001) .OR. (TRANFN(3).GT.0.999) .OR.
     *   (TRANFN(4).LT.0.001) .OR. (TRANFN(4).GT.0.999)) THEN
         TRANFN(3) = 0.5
         TRANFN(4) = 0.5
         END IF
C                                       Get OPcode for grey conversion
      GOPR = 0
      DO 20 I = 1,NOPS
         IF (OPTYPE.EQ.OKOPS(I)) GOPR = I
 20      CONTINUE
      IF (GOPR.EQ.0) GOPR = 2
C                                       Fix number of grey-scale levels
C                                       for OPcode QRAN:
      IF ((GOPR .EQ. 4) .AND. (DPARM(8) .GT. 26.0)) DPARM(8) = 26.0
C                                       Transfer function: type, clips
      ITRN = 0
      DO 25 I = 1,NTRN
         EQUAL = OKTRN(I) .EQ. CTRANF
         IF (EQUAL) ITRN = I
 25      CONTINUE
      ITRN = ITRN + 1
      IF ((TRANFN(5).LE.0.0) .OR. (TRANFN(5).GE.0.999)) TRANFN(5) = 0.0
      IF ((TRANFN(6).GT.1.0) .OR. (TRANFN(6).LE.TRANFN(5))) TRANFN(6) =
     *   1.0
      IF ((DPARM(1).LE.0.01) .OR. (DPARM(1).GE.100.)) DPARM(1) = 1.0
      IF (ABS(DPARM(2)).GT.0.9) DPARM(2) = 0.0
C                                       Get map header.
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      USER = NLUSER
      IVER = IROUND (VERSN)
      SLOT = 1
      TYPIN = ' '
      CALL CATDIR ('SRCH', IVOL, SLOT, NAMIN, CLSIN, ISEQ, TYPIN, USER,
     *   STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 995
         END IF
C                                       Read catalog header.
      CALL CATIO ('READ', IVOL, SLOT, CATBLK, 'REST', BUFFER, CATERR)
      IF ((CATERR.EQ.0) .OR. (CATERR.EQ.6)) GO TO 50
 45      WRITE (MSGTXT,1045) CATERR
         GO TO 995
C                                        Find plot file:
C                                        if PL & IVER=0, then main file
 50   EQUAL = TYPIN .EQ. 'PL'
      IF ((EQUAL) .AND. (CATERR.EQ.6)) GO TO 45
      IF ((EQUAL) .AND. (IVER.EQ.0)) IVER = CATBLK(KIIMS)
C                                        else take IVER as given or
C                                        as max version #
      IF (IVER.LE.0) CALL FNDEXT ('PL', CATBLK, IVER)
C                                       PLot file not found.
      IF (IVER.LE.0) THEN
         WRITE (MSGTXT,1060)
         GO TO 995
         END IF
C                                       Save parms
      SEQIN = ISEQ
      DSKIN = IVOL
      VERSN = IVER
C                                       Build file name.
      CALL ZPHFIL ('PL', IVOL, SLOT, IVER, GFILE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open graphics file.
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, NOMAP, NOEXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070)
         GO TO 995
         END IF
      GO TO 999
C                                       Error
 995  CALL MSGWRT (8)
      IF (IRET.EQ.0) IRET = 4
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1045 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
 1060 FORMAT ('PLOT FILE NOT IN CATALOG')
 1070 FORMAT ('PLOT FILE NOT FOUND')
      END
      SUBROUTINE CANDRW (LUN, FIND, BUFFER, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the
C   Canon.
C   INPUTS:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            BUFFER  I(256)   buffer to use for I/O.
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER LUN, FIND, BUFFER(256), IERR
C
      CHARACTER CHBUF*200
      REAL   DX, DY, SCALEF, X, XYRATO, Y, RX1, RX2, TOTPIX, VALUE, RY1,
     *   RY2, AX, AY, XPXSEP, YPXSEP, YCADMM, UASPMM, XYDIFF, YASPMM,
     *   DELTA, XCADMM, RCALEX, RCALEY, GRSCAL, GROFF, NOWPIX, TEMPV
      INTEGER   ITYPE, QY, LXA, GRYERR, CSIZCA(2), NNCHAR, LYA, IANGL,
     *   ICHB, ICHL, ICHR, ICHT, INCRRN, IOPOS, IX, IX1, IY, IY1, NXA,
     *   NYA, OPCODE, CANLUN, I, IERRC, IX2, IY2, NCHAR, IROUND, LLPEN,
     *   IGRLO, IGRHI, NPTSX, NPTSY, NXDPP, NYDPP, IPER, IPER2, N, NPIX,
     *   IERRCH, ILAND, IORRN, CHLAND(2), IXCUR, IYCUR
      CHARACTER*1 CHAR, CSI, ESC, GRAVE, IS1, IS2, X23, X31
      LOGICAL   DOLAND, DOGREY, T
      INCLUDE 'CANPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CANLUN, T /7, .TRUE./
      DATA CHLAND /0,1/
C
C                                       The following character
C                                       variables are used for some
C                                       non-printing control characters.
C                                       The intrinsic Fortran77 function
C                                       CHAR returns the ASCII code for
C                                       the integer argument
C
      CSI =    CHAR (155)
      GRAVE =  CHAR (96)
      ESC =    CHAR (27)
      IS1 =    CHAR (31)
      IS2 =    CHAR (30)
      X23 =    CHAR (35)
      X31 =    CHAR (49)
C
C-----------------------------------------------------------------------
C                                       Initialize all values (units
C                                       are 1/300 inches).
C                                       MAXCAN is the paper size (10 x
C                                       7.5 inches; CSIZCA contains the
C                                       conversion factor from plot file
C                                       units for the frame and XCADMM
C                                       is
C                                       1 mm in 1/300 inches.
C
C                                       **** CSIZCA must be changed if
C                                       **** if the font is changed.
      MAXCAN(1) = 10. * 300
      MAXCAN(2) = 7.5 * 300
      CSIZCA(1) = 30
      CSIZCA(2) = 45
      XCADMM = 11.81
C                                       Initially, assume same scaling
C                                       for
C                                       both axes.
      YCADMM = XCADMM
      IERR = 0
      IERRC = 0
      IERRCH = 0
      GRYERR = 0
      DOGREY = .FALSE.
      IOPOS = 9999
      IORRN = 1
      CANPOS = 0
      CANSIZ = 80
      CANBUF = ' '
C                                       Open CANON file
      CALL ZLASIO ('OPEN', CANLUN, OUTFIL, NNCHAR, CANBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       determine where plot starts
      CALL ZFIO ('READ', LUN, FIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      IORRN = (BUFFER(10) + 9) / 256 + 1
C                                       next plot command
C                                       Read record.
 50   IF (IOPOS.LE.256) GO TO 60
 55      IORRN = IORRN + 1
         CALL ZFIO ('READ', LUN, FIND, IORRN, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 980
         IOPOS = 1
C                                       Transfer based on opcode.
 60   OPCODE = BUFFER(IOPOS)
      IF ((OPCODE.GT.0) .AND. (OPCODE.LE.8)) GO TO (100, 200,
     *   300, 400, 500, 600, 700, 800), OPCODE
      IF (OPCODE.EQ.0) GO TO 55
C                                       End of file.
      IF (OPCODE.NE.32767) GO TO 970
         GO TO 990
C                                       Opcode 1, initialize.
C                                       Put map type in image header.
 100  ITYPE = BUFFER(IOPOS+5)
      IOPOS = IOPOS + 6
      GO TO 50
C                                       Opcode 2, line drawing init.
 200  XYRATO = BUFFER(IOPOS+1) / 100.0
      SCALEF = BUFFER(IOPOS+2)
      IX1    = BUFFER(IOPOS+3)
      IY1    = BUFFER(IOPOS+4)
      IX2    = BUFFER(IOPOS+5)
      IY2    = BUFFER(IOPOS+6)
      RX1    = BUFFER(IOPOS+7)/1000.0 + IX1
      RY1    = BUFFER(IOPOS+8)/1000.0 + IY1
      RX2    = BUFFER(IOPOS+9)/1000.0 + IX2
      RY2    = BUFFER(IOPOS+10)/1000.0 + IY2
C                                       Left, bottom, right and top of
C                                       frame
      ICHL   = (BUFFER(IOPOS+11)/10.0) * CSIZCA(1)  +  0.5
      ICHB   = (BUFFER(IOPOS+12)/10.0) * CSIZCA(2)  +  0.5
      ICHR   = (BUFFER(IOPOS+13)/10.0) * CSIZCA(1)  +  0.5
      ICHT   = (BUFFER(IOPOS+14)/10.0) * CSIZCA(2) +  0.5
      IOPOS = IOPOS + 15
C                                       Find number of pixels inside
C                                       border
      NYA = MAXCAN(2) - ICHT - ICHB - 1
      NXA = MAXCAN(1) - ICHL - ICHR - 1
      LYA = MAXCAN(1) - ICHT - ICHB - 1
      LXA = MAXCAN(2) - ICHL - ICHR - 1
      IF ((NXA.GT.0) .AND. (NYA.GT.0)) GO TO 205
         WRITE (MSGTXT,1200) NXA, NYA
         CALL MSGWRT (7)
         GO TO 990
C                                       Compute scaling: sizes
 205  AX = ABS(IX2 - IX1) * XYRATO
      AY = ABS(IY2 - IY1)
      X = ABS(RX2-RX1) * XYRATO
      Y = ABS(RY2-RY1)
      IF ((X.GT.0) .AND. (Y.GT.0)) GO TO 210
         WRITE (MSGTXT,1205) X, Y
         CALL MSGWRT (7)
         GO TO 990
C                                       Factors: landscape mode
C                                       (short axis y)
 210  IF ((X/Y).GT.FLOAT(NXA)/FLOAT(NYA)) GO TO 215
         SCALEY = NYA/SCALEF * AY/Y
         SCALEX = SCALEY * AX/AY
         GO TO 220
 215  CONTINUE
         SCALEX = NXA/SCALEF * AX/X
         SCALEY = SCALEX * AY/AX
C                                       Factors: portrait mode
 220  IF ((X/Y).GT.FLOAT(LXA)/FLOAT(LYA)) GO TO 223
         RCALEY = LYA/SCALEF * AY/Y
         RCALEX = RCALEY * AX/AY
         GO TO 225
 223  CONTINUE
         RCALEX = LXA/SCALEF * AX/X
         RCALEY = RCALEX * AY/AX
C                                       Use portrait
 225  DOLAND =  SCALEX*SCALEY.GE.RCALEX*RCALEY
      ILAND = 2
      IF (DOLAND) GO TO 230
         ILAND = 1
         SCALEX = RCALEX
         SCALEY = RCALEY
         I = MAXCAN(2)
         MAXCAN(2) = MAXCAN(1)
         MAXCAN(1) = I
C                                       Mm / arc sec scaling
 230  LOCNUM = 1
      AXTYP(LOCNUM) = 0
      UASPMM = ASPMM
      IF ((ITYPE.NE.2) .AND. (ITYPE.NE.3) .AND. (ITYPE.NE.6)) GO TO 235
         CALL SETLOC (BUFFER(IOPOS), T)
         IF (AXTYP(LOCNUM).NE.1) GO TO 235
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
            YPXSEP = XPXSEP
            CALL SCALMM (AX, AY, XPXSEP, YPXSEP, SCALEF, XCADMM,
     *         YCADMM, ASPMM, SCALEX, SCALEY)
            GO TO 240
C                                       Scale it down ?
 235  IF ((ASPMM.LE.0.02) .OR. (ASPMM.GT.0.98)) GO TO 240
         AXTYP(LOCNUM) = -10
         WRITE (MSGTXT,1235) ASPMM
         CALL MSGWRT (4)
         SCALEX = SCALEX * ASPMM
         SCALEY = SCALEY * ASPMM
C                                        Center
 240  NXA = SCALEX*SCALEF*X/AX + ICHL + ICHR
      IX0 = ICHL + MAX(0,MAXCAN(1)-NXA)/2 + 1
      NYA = SCALEY*SCALEF*Y/AY + ICHB + ICHT
      IY0 = ICHB + MAX(0,MAXCAN(2)-NYA)/2 + 1
      IOPOS = IOPOS + 5
      IF (BUFFER(IOPOS).NE.3) GO TO 350
      DOGREY = .TRUE.
C                                       Opcode 3, grey scale init.
 300  IF ((GOPR.GT.1) .AND. (DOGREY)) GO TO 305
         IOPOS = IOPOS + 5
         WRITE (MSGTXT,1300)
         IF (.NOT.DOGREY) CALL MSGWRT (7)
         DOGREY = .FALSE.
         GO TO 350
C                                       parms pick up
 305  IGRLO = BUFFER(IOPOS+1)
      IGRHI = BUFFER(IOPOS+2)
      NPTSX = BUFFER(IOPOS+3)
      NPTSY = BUFFER(IOPOS+4)
      IOPOS = IOPOS + 5
      IF ((IGRLO.LT.IGRHI) .AND. (NPTSX.GT.0) .AND. (NPTSY.GT.0)
     *   .AND. (NPTSX.LT.MAXCAN(1)-ICHL-ICHR)) GO TO 310
         WRITE (MSGTXT,1305) IGRLO, IGRHI, NPTSX, NPTSY
         CALL MSGWRT (7)
         DOGREY = .FALSE.
         GO TO 350
C                                       Change to RAND if small
 310  TOTPIX = REAL(NPTSX) * REAL(NPTSY)
      IF ((GOPR.EQ.3) .OR. (GOPR.EQ.6) .OR. (TOTPIX.GT.100)) GO TO 320
         WRITE (MSGTXT,1310)
         CALL MSGWRT (3)
         GOPR = 1
         DOGREY = .FALSE.
C                                       dots per pixel
 320  NXDPP = (NXA - ICHL - ICHR) * XYRATO / X  + 0.05
      NYDPP = (NYA - ICHT - ICHB) / Y  + 0.05
      IF (NXDPP*X/XYRATO.GT.MAXCAN(1)-ICHL-ICHR) NXDPP = NXDPP - 1
      IF (NYDPP*Y.GT.MAXCAN(2)-ICHB-ICHT) NYDPP = NYDPP - 1
      CALL FIXDPP (GOPR, NXDPP, NYDPP, DPARM)
      NXDPP = MAX (1, NXDPP)
      NYDPP = MAX (1, NYDPP)
      IF ((NXDPP.GE.4) .AND. (NYDPP.GE.4)) GO TO 325
         IF (NXDPP*NYDPP.GE.10) GO TO 325
         IF ((GOPR.NE.2) .AND. (GOPR.NE.4) .AND. (GOPR.NE.5)) GO TO 325
            WRITE (MSGTXT,1320)
            IF (GOPR.EQ.2) WRITE (MSGTXT,1321)
            CALL MSGWRT (7)
            DOGREY = .FALSE.
            GOPR = 1
C                                       reset scales
 325  SCALEX = NXDPP * (AX / XYRATO) / SCALEF
      SCALEY = NYDPP * AY / SCALEF
      NXA = ICHL + ICHR + NXDPP * X / XYRATO
      NYA = ICHB + ICHT + NYDPP * Y
      IX0 = ICHL + MAX (0, MAXCAN(1) - NXA) / 2
      IY0 = ICHB + MAX (0, MAXCAN(2) - NYA) / 2
      GRSCAL = 0.998 / (REAL(IGRHI) - REAL(IGRLO))
      GROFF = IGRLO - 0.001 / GRSCAL + TRANFN(5) / GRSCAL
      GRSCAL = GRSCAL / (TRANFN(6) - TRANFN(5))
C                                       Offset for negative coords
 350  IX0 = IX0 - (RX1-IX1) * XYRATO * SCALEX * SCALEF / AX
      IY0 = IY0 - (RY1-IY1) * SCALEY * SCALEF / AY
C                                       Display arcsec / mm info
      IF (AXTYP(LOCNUM).NE.1) GO TO 370
         ASPMM = ABS (AX * AXINC(1,LOCNUM) * 3600.0 * XCADMM)
     *      / (SCALEX * SCALEF * XYRATO)
         YASPMM = ABS (AY * AXINC(2,LOCNUM) * 3600.0 * YCADMM)
     *      / (SCALEY * SCALEF)
         XYDIFF = ABS (ASPMM - YASPMM)
         DELTA = .025 * ASPMM
C                                       X and Y ASPMM about the same.
         IF (XYDIFF.GT.DELTA) GO TO 355
            WRITE (MSGTXT,1350) ASPMM
            CALL MSGWRT (4)
            GO TO 360
C                                       Using different X and Y ASPMM
 355     CONTINUE
            WRITE (MSGTXT,1355) ASPMM
            CALL MSGWRT (4)
            WRITE (MSGTXT,1356) YASPMM
            CALL MSGWRT (4)
C
 360     IF ((UASPMM.LE.0.0) .OR. (UASPMM.GE.ASPMM)) GO TO 370
            WRITE (MSGTXT,1360)
            CALL MSGWRT (2)
C                                       Switch to ISO mode and reset
 370  WRITE (CHBUF,2370) ESC, ESC, ESC, CSI
      NNCHAR = 10
      CALL ZLASIO ('WRIT', CANLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       page format = cassette size
C                                       landscape/portrait
      WRITE (CHBUF,2371) CSI, CHLAND(ILAND)
      NNCHAR = 3
      CALL ZLASIO ('WRIT', CANLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Copies
      NNCHAR = IROUND (COPIES)
      IF (NNCHAR.LE.0) NNCHAR = 1
      WRITE (CHBUF,2373) CSI, NNCHAR
      NNCHAR = 4
      CALL ZLASIO ('WRIT', CANLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       And more stuff, as done by DVI!
      WRITE (CHBUF,2374) CSI, CSI, CSI, CSI
      NNCHAR = 26
      CALL ZLASIO ('WRIT', CANLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR. NE. 0) GO TO 960
C                                       Init grey scale generation
      IF (.NOT.DOGREY) GO TO 380
         CALL CANGRY (1, CANLUN, NXDPP, NYDPP, IX, IY, DOLAND,
     *      VALUE, CHBUF, IERR)
         IF (IERR.EQ.0) GO TO 375
            WRITE (MSGTXT,1370) IERR
            CALL MSGWRT (7)
            DOGREY = .FALSE.
            GO TO 50
C                                       Init variables for progress
C                                       report.
 375     TOTPIX = REAL(NPTSX) * REAL(NPTSY)
         NOWPIX = 0.0
         IPER = 0
         WRITE (MSGTXT,1375)
         CALL MSGWRT (1)
C                                       on vector graphics, pen width
 380  LLPEN = IROUND (XXPEN)
      IF (LLPEN.LT.1) LLPEN = 2
      IF (LLPEN.GT.4) LLPEN = 4
      WRITE (CHBUF,2375) CSI, IS2, X23, X31, IS2, IS2,
     *                             CHAR(LLPEN+32), IS2
      NNCHAR = 22
      CALL FILZCH (NNCHAR, 1, CHBUF)
      CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Check that scalefactors are OK
      CALL CNTPLT (LUN, FIND, IOPOS, IORRN, IX0, SCALEX, CSIZCA(1),
     *   ICHL, NXA, MAXCAN(1), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 50
C                                       Opcode 4, position cursor.
 400  IX = BUFFER(IOPOS+1)
      IY = BUFFER(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL CANVEC (IX, IY, IXCUR, IYCUR, 1, CANLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 5, draw vector.
 500  IX = BUFFER(IOPOS+1)
      IY = BUFFER(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL CANVEC (IX, IY, IXCUR, IYCUR, 2, CANLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 6, write characters.
 600  NCHAR = BUFFER(IOPOS+1)
      IANGL = BUFFER(IOPOS+2)
      DX    = BUFFER(IOPOS+3)/100.0
      DY    = BUFFER(IOPOS+4)/100.0
      IX = IXL + DX * CSIZCA(1)
      IY = IYL + DY * CSIZCA(2)
      QY = IY + 0.25 * CSIZCA(2)
C                                       horizontal
      IF (IANGL.EQ.1) GO TO 610
C                                       Move cursor
         CALL CANVEC (IX, QY, IXCUR, IYCUR, 3, CANLUN, IERR)
         IF (IERR.EQ.10) IERRCH = IERRCH + 1
         IF (IERR.EQ.10) GO TO 630
         IF (IERR.NE.0) GO TO 960
C                                       Switch to ISO text mode
         CALL CANTXT (IXCUR, IYCUR, CANLUN, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       select text font
         WRITE (CHBUF,2376) ESC
         NNCHAR = 3
         CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
         IF (IERR .NE. 0) GO TO 960
C                                       Copy characters to buffer
         CALL H2CHR (NCHAR, 1, BUFFER(IOPOS+5), CHBUF)
         NNCHAR = NCHAR
         CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       Switch to vector graphics
         WRITE (CHBUF,2375) CSI, IS2, X23, X31, IS2, IS2,
     *                          CHAR(LLPEN+32), IS2
         NNCHAR = 22
         CALL FILZCH (NNCHAR, 1, CHBUF)
         CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
         GO TO 630
C                                       vertical
 610  CONTINUE
         DO 620 I = 1,NCHAR
C                               Move cursor
            CALL CANVEC (IX, QY, IXCUR, IYCUR, 3, CANLUN, IERR)
            IF (IERR.EQ.10) IERRCH = IERRCH + 1
            IF (IERR.EQ.10) GO TO 630
            IF (IERR.NE.0) GO TO 960
C                                       Switch to ISO text mode
            CALL CANTXT (IXCUR, IYCUR, CANLUN, IERR)
            IF (IERR.NE.0) GO TO 960
C                                       select text font
            WRITE (CHBUF,2376) ESC
            NNCHAR = 3
            CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
            IF (IERR .NE. 0) GO TO 960
C                                       Copy characters one at a time
            CALL H2CHR (1, I, BUFFER(IOPOS+5), CHBUF)
            NNCHAR=1
            CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
C                                       Change vertical position
C                                       for next character
            QY = QY - CSIZCA(2)
C                                       Switch to vector graphics
            WRITE (CHBUF,2375) CSI, IS2, X23, X31, IS2, IS2,
     *                             CHAR(LLPEN+32), IS2
            NNCHAR = 22
            CALL FILZCH (NNCHAR, 1, CHBUF)
            CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
 620        CONTINUE
 630  IOPOS = IOPOS + 5 + ((NCHAR+3)/4)
      GO TO 50
C                                       Opcode 7, write grey scale.
 700  IF (DOGREY) GO TO 710
         GRYERR = GRYERR + 1
C                                       Update IO position. Read
C                                       proper RRN if necessary.
         IOPOS = IOPOS + 3 + BUFFER(IOPOS+1)
         IF (IOPOS.LE.256) GO TO 60
            INCRRN = (IOPOS-1)/256
            IORRN = IORRN + INCRRN
            IOPOS = IOPOS - 256*INCRRN
            CALL ZFIO ('READ', LUN, FIND, IORRN, BUFFER, IERR)
            IF (IERR.EQ.0) GO TO 60
            GO TO 980
C                                       Get grey values from record(s).
 710  NPIX = BUFFER(IOPOS+1)
      IANGL = BUFFER(IOPOS+2)
      IOPOS = IOPOS + 3
      IF (NPIX.LE.0) GO TO 50
      IX = IXL - NXDPP/2
      IY = IYL + NYDPP/2
C                                       Set bits for each pixel.
      DO 790 N = 1,NPIX
C                                       Need another buffer of G values
         IF (IOPOS.LE.256) GO TO 720
            IORRN = IORRN + 1
            CALL ZFIO ('READ', LUN, FIND, IORRN, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 980
            IOPOS = 1
C
 720     VALUE = (BUFFER(IOPOS) - GROFF) * GRSCAL
         VALUE = MAX (0.001, MIN (0.999, VALUE))
         IF ((ITRN.EQ.2) .OR. (ITRN.EQ.4) .OR. (ITRN.EQ.6))
     *      VALUE = 1.000 - VALUE
         IF ((ITRN.EQ.3) .OR. (ITRN.EQ.4))
     *      VALUE = LOG (30.0*VALUE + 1.0) / LOG (30.42) - .007
         IF ((ITRN.NE.5) .AND. (ITRN.NE.6)) GO TO 722
            TEMPV = VALUE
            IF (TEMPV.LT.TRANFN(3))
     *         VALUE = TRANFN(4) * (VALUE / TRANFN(3)) ** (1./TRANFN(2))
            IF (TEMPV.GE.TRANFN(3))
     *         VALUE = TRANFN(4) + (1.-TRANFN(4)) *
     *                  ((VALUE-TRANFN(3))/(1.-TRANFN(3))) ** TRANFN(2)
 722     CONTINUE
         VALUE = DPARM(1) * VALUE + DPARM(2)
         VALUE = MAX (0.0, MIN (1.0, VALUE))
C                                       position for pixel
         IF ((IANGL.EQ.0) .AND. (N.GT.1)) GO TO 725
            CALL CANVEC (IX, IY, IXCUR, IYCUR, 3, CANLUN, IERR)
            IF (IERR.EQ.10) GO TO 730
            IF (IERR.NE.0) GO TO 960
C                                       do it
 725     CALL CANGRY (2, CANLUN, NXDPP, NYDPP, IX, IY, DOLAND,
     *      VALUE, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       reset position
 730     IF (IANGL.EQ.0) IX = IX + NXDPP
         IF (IANGL.EQ.1) IY = IY + NYDPP
         IF (IANGL.EQ.2) IX = IX - NXDPP
         IF (IANGL.EQ.3) IY = IY - NYDPP
         IOPOS = IOPOS + 1
C                                       on graphics
         IF ((IANGL.EQ.0) .AND. (N.LT.NPIX)) GO TO 790
            WRITE (CHBUF,2375) CSI, IS2, X23, X31, IS2, IS2,
     *                             CHAR(LLPEN+32), IS2
            NNCHAR = 22
            CALL FILZCH (NNCHAR, 1, CHBUF)
            CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
 790     CONTINUE
      NOWPIX = NOWPIX + NPIX
      IPER2 = INT(5.0 * NOWPIX / TOTPIX)
C                                       Write progress report.
      IF (IPER2.EQ.IPER) GO TO 50
         IPER = 20*IPER2
         WRITE (MSGTXT,1790) IPER
         IPER = IPER2
         CALL MSGWRT (1)
         GO TO 50
C                                       Opcode 8, put misc info in
C                                       image catalog.
 800  IOPOS = IOPOS + 2 + BUFFER(IOPOS+1)
      GO TO 50
C                                       Write error.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      GO TO 990
C                                       Invalid opcode.
 970  WRITE (MSGTXT,1970) OPCODE
      CALL MSGWRT (8)
      GO TO 990
C                                       Disk error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       End picture
 990  WRITE (CHBUF,2990) IS2, IS2
      NNCHAR = 5
      CALL FILZCH (NNCHAR, 1, CHBUF)
      CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 993
      IF (CANPOS.GT.0) CALL ZLASIO ('WRIT', CANLUN, OUTFIL, CANPOS,
     *    CANBUF, IERR)
C                                       and quit
      IF (IERR.EQ.0) CALL ZLASIO ('CLOS', CANLUN, OUTFIL, 0, CANBUF,
     *   IERR)
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
 1000 FORMAT ('CANDRW: ERROR',I4,' OPENING CANON FILE/DEVICE')
 1200 FORMAT ('CANDRW: ERR FROM GINITL ARGS. NXA, NYA=',2I6)
 1205 FORMAT ('CANDRW: ERR FROM GINITL ARGS. X,Y=',2E10.3)
 1235 FORMAT ('Plot reduced in size by factor',F6.3)
 1300 FORMAT ('CANDRW: init grey scale out of order, ignored')
 1305 FORMAT ('CANDRW: BAD PARMS FROM GINITG:',4I7)
 1310 FORMAT ('FEWER THAN 157 PIXELS, USE BITS OR RAND')
 1320 FORMAT ('PIXELS TOO SMALL FOR QRAN OR MRAN, USE RAND')
 1321 FORMAT ('PIXELS TOO SMALL FOR MBIT, USE BITS')
 1350 FORMAT ('Using',1PE12.5,' arc sec per mm')
 1355 FORMAT ('Using',1PE12.5,' arc sec per mm for x axis')
 1356 FORMAT ('Using',1PE12.5,' arc sec per mm for y axis')
 1360 FORMAT ('Which overrides user ASPMM to fit plot on screen')
 1370 FORMAT ('CANDRW: ERROR',I5,' INITIALIZING GREY SCALE')
 1375 FORMAT ('Grey scale initialization complete')
 1790 FORMAT (I3,' %  of grey scale pixels plotted')
 1960 FORMAT ('CANDRW: CANON WRITE ERROR')
 1970 FORMAT ('CANDRW: INVALID OPCODE',I5,' IN GRAPH FILE')
 1980 FORMAT ('CANDRW: DISK IO RELATED ERROR',I3)
 1993 FORMAT ('CANDRW: ',I5,' GREY SCALE OPCODES IGNORED')
 1995 FORMAT (I8,' vectors truncated at edges')
 1996 FORMAT (I4,' character strings omitted at edges')
C                               select ISO mode (ESC ;)
C                               hard reset (ESC c)
C                               select ISO mode again (ESC ;)
C                               Full paint memory mode (CSI 2 & z)
 2370 FORMAT (A1,';',A1,'c',A1,';',A1,'2&z')
C                               Page format is cassette size
C                               CSI 0 p for portrait,
C                               CSI 1 p for landscape
 2371 FORMAT (A1,I1,'p')
C                               Number of copies (CSI no. v)
 2373 FORMAT (A1,I2,'v')
C                               Some stuff that dvicn does
 2374 FORMAT (A1,'11h',A1,'7 I',A1,'?2h',A1,'?1;3;4;5;6;7l')
C                               Enter vector mode (CSI 0 & })
C                               Begin picture (# name IS2)
C                               Scaling in 1/300" (! 0 X23 X31 IS2)
C                               Begin picture body ($ IS2)
C                               Set pen width (F 1 number IS2)
C                               number = -1,-2,-3,-4
 2375 FORMAT (A1,'0&}#CANPL',A1,'!0',3A1,'$',A1,'F1',2A1)
C                               Select ISO/ASCII character set
 2376 FORMAT( A1, '(B')
C                               End picture (% IS2)
C                               Return to text (} p IS2)
 2990 FORMAT ('%',A1,'}p',A1)
      END
      SUBROUTINE CANGRY (IOP, CANLUN, NX, NY, IX, IY, DOLAND, VALUE,
     *   CHBUF, IERR)
C-----------------------------------------------------------------------
C   CANGRY performs (IOP=1) the grey scale initialization needed for the
C   given operation: GOPR = 4,5 compute random character set and
C   download to the Canon, GOPR = 6 initialize random number generator,
C   GOPR = 2,3 prepare sine/Gaussian array for masking and on 2 download
C   a character set.  On IOP=2, it writes the pixel to the device as a
C   character (GOPR = 2,4,5) or as a dot matrix.
C   Inputs: IOP     I        = 1 -> INIT, = 2 -> do it.
C           CANLUN  I        LUN for open CANON print file.
C           NX      I        Number dots per pixel in X
C           NY      I        Number dots per pixel in Y
C           IX      I        Current dot position of pixel in X.
C           IY      I        Current dot position of pixel in Y.
C           DOLAND  L        T => landscape mode, else portrait
C           VALUE   R        Grey scale pixel value (0. to 1.)
C   Output: CHBUF   C*80     Scratch buffer
C           IERR    I        Error code: 0 -> ok, 1 -> input error,
C                               2 -> I/O error
C   Inputs in COMMON:
C           DPARM   R(10)    DPARM(8) = number of grey-scale levels
C           GOPR    I        Grey-scale type: 2 MBIT, 3 BITS, 4 QRAN,
C                            5 MRAN, 6 RAND
C-----------------------------------------------------------------------
      INTEGER   IOP, CANLUN, NX, NY, IX, IY
      LOGICAL   DOLAND
      REAL      VALUE
      CHARACTER CHBUF*80
      INTEGER   IERR
C
      INTEGER   ITJ(8), I, J, NNCHAR, INX, INY, NC, I1, I2, IT, IL, IC,
     *   JJ, IOFF, ILY, LLLY, WIDTH, QUANT, YFIX, FPITCH, FHT,
     *   CWIDTH, CHT, NLAND, NLEVEL, NBYTES
      REAL     TV, SX, SY, AM, ADX, ADY, CNX, CNY, CAX, CAY, CBX, CBY
      CHARACTER*1 CSI, ESC, X31, IS2, NXTCHR
      CHARACTER*26 ALPHA(3)
      INCLUDE 'CANPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ITJ /128, 64, 32, 16, 8, 4, 2, 1/
      DATA ALPHA /'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     *            'abcdefghijklmnopqrstuvwxyz',
     *            '1234567890!@#$%&*()-+=<>,.'/
C-----------------------------------------------------------------------
      CSI = CHAR(155)
      ESC = CHAR(27)
      X31 = CHAR(49)
      IS2 = CHAR(30)
C                                       Test inputs
      IERR = 1
      IF ((IOP.NE.1) .AND. (IOP.NE.2)) GO TO 999
      IF ((GOPR.LT.2) .OR. (GOPR.GT.6)) GO TO 999
      IERR = 0
C                                       Initialization
      NLEVEL = NINT(DPARM(8))
      IF (IOP.EQ.2) GO TO 500
         MXD = DPARM(9) + 0.01
         MYD = DPARM(10) + 0.01
         IF (MXD.LE.0) MXD = 1
         IF (MYD.LE.0) MYD = 1
C                                       'QRAN', 'MRAN', 'MBIT', 'BITS'
         IF (GOPR.EQ.6) GO TO 100
C                                       Get BIT mask
            IF ((GOPR.EQ.4) .OR. (GOPR.EQ.5)) GO TO 20
               INX = MXD / 2 + 1
               SX = 6.283185 * DPARM(5) / MXD
               INY = MYD / 2 + 1
               SY = 6.283185 * DPARM(5) / MYD
               AM = 0.0
               ADX = DPARM(5) / (MXD/4.0) ** 2
               ADY = DPARM(5) / (MYD/4.0) ** 2
               CNX = INX - DPARM(6)
               CNY = INY - DPARM(7)
               CAX = MXD + 1 - DPARM(6)
               CAY = MYD + 1 - DPARM(7)
               CBX = 1 - DPARM(6)
               CBY = 1 - DPARM(7)
               I1 = 0
               DO 15 I = 1,MYD
                  TV = COS (SY * (I-CNY))
                  ILY = I1 + 1
                  DO 10 J = 1,MXD
                     I1 = I1 + 1
                     AMASK(I1) = 0.50 * (TV * COS (SX*(J-CNX))
     *                  + 1.0)
                     IF (DPARM(3).GT.1.5) GO TO 10
                        AMASK(I1) =
     *                     EXP (-ADX*((J-CNX)**2) - ADY*((I-CBY)**2))
     *                   + EXP (-ADX*((J-CAX)**2) - ADY*((I-CNY)**2))
     *                   + EXP (-ADX*((J-CNX)**2) - ADY*((I-CAY)**2))
     *                   + EXP (-ADX*((J-CBX)**2) - ADY*((I-CNY)**2))
                        AM = MAX (AM, AMASK(I1))
 10                  CONTINUE
 15               CONTINUE
               IF (DPARM(3).GT.1.5) GO TO 19
                  I1 = 0
                  DO 18 I = 1,MYD
                     ILY = I1 + 1
                     DO 17 J = 1,MXD
                        I1 = I1 + 1
                        AMASK(I1) = 1.0 - AMASK(I1) / AM
 17                     CONTINUE
 18                  CONTINUE
 19            CONTINUE
               I1 = 0
               DO 192 I = 1,MYD
                  DO 191 J = 1,MXD
                     I1 = I1 + 1
                     AMASK(I1) = 0.998 * AMASK(I1) + 0.001
 191              CONTINUE
 192           CONTINUE
               IF (GOPR.EQ.3) GO TO 999
C                                       get sizes
 20         INX = NX
            INY = NY
            IF (.NOT.DOLAND) GO TO 25
               INX = NY
               INY = NX
 25         NC = ((INX+7) / 8)
C                                       memory requirements
C                                       (Assume text font is less
C                                        than 100k)
            I2 = ((NC * INY + 4) * 78. + 1023.) / 1024.
            IF (I2.LE.1000) GO TO 30
               GOPR = 1
               WRITE (MSGTXT,1025)
               CALL MSGWRT (6)
               GO TO 100
C                                       init random numbers
 30         CALL RANDIN (IERR)
            IF (IERR.NE.0) GO TO 999
C                                       compute a character set
C                                       first, do the rather complicated
C                                       coding to set up font
C                                       number bytes per char, * chars
            NBYTES = (NC * INY + 4)
            NBYTES = NBYTES * 78
C                                       landscape or portrait
            IF (DOLAND) THEN
               NLAND = 1
            ELSE
               NLAND = 0
            END IF
C                                       font pitch and height
            FPITCH = INT(3750.0 / REAL(NC)) + 1
            FHT = 100 * INY
C                                       cell width and height
            CWIDTH = NC * 8
            CHT = INY
C                                       send it to the file
            WRITE (CHBUF,1030) CSI, NBYTES, NLAND, FPITCH,
     *                               FHT, CWIDTH, CHT
            NNCHAR = 58
            CALL FILZCH(NNCHAR, 1, CHBUF)
            CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       define characters
            DO 75 I = 1,26
               DO 70 J = 1,3
C                                       character, pitch, width
C                                       and offset
                  NXTCHR = ALPHA(J)(I:I)
                  WRITE (CHBUF,1031) NXTCHR, CHAR(CWIDTH),
     *                                     CHAR(CWIDTH), CHAR(0)
                  NNCHAR = 4
                  CALL FILZCH (NNCHAR, 1, CHBUF)
                  CALL CANIO (CANLUN, NNCHAR, CHBUF, IERR)
                  IF (IERR.NE.0) GO TO 995
C                                       Value of char
                  VALUE = 0.001 + 0.998 * (I-1) / (NLEVEL - 1)
                  IF ((GOPR.EQ.2) .OR. (GOPR.EQ.5)) VALUE = 0.001
     *               + 0.998 * ((J-1)*26+I-1) / (NLEVEL - 1)
                  IF (VALUE .GT. 0.999) VALUE = 0.999
                  IOFF = -1
                  DO 65 IL = 1,INY
                     I1 = 0
                     ILY = MOD (IL+IOFF, MYD) * MXD + 1
                     DO 60 IC = 1,NC
                        CANPOS = CANPOS + 1
                        IF (CANPOS.LE.CANSIZ) GO TO 35
                           CALL ZLASIO ('WRIT', CANLUN, OUTFIL, CANSIZ,
     *                        CANBUF, IERR)
                           IF (IERR.NE.0) GO TO 995
                           CANPOS = 1
 35                     IT = 0
C                                       Random functions
                        IF (GOPR.EQ.2) GO TO 45
                           DO 40 JJ = 1,8
                              I1 = I1 + 1
                              IF (I1.GT.INX) GO TO 55
                                 CALL RANDUM (TV)
                                 IF (VALUE.GT.TV) IT = IT + ITJ(JJ)
 40                           CONTINUE
                           GO TO 55
C                                       Starlink function
 45                     CONTINUE
                           DO 50 JJ = 1,8
                              I1 = I1 + 1
                              IF (I1.GT.INX) GO TO 55
                                 I2 = ILY + MOD (I1+IOFF, MXD)
                                 IF (VALUE.GT.AMASK(I2)) IT = IT +
     *                              ITJ(JJ)
 50                           CONTINUE
C ???                   ENCODE(80,1040,CHBUF) CHAR(IT)
 55                     WRITE (CHBUF,1040) CHAR(IT)
                        CALL FILZCH(1, 1, CHBUF)
                        CANBUF(CANPOS:CANPOS+1-1) = CHBUF(1:1+1-1)
 60                     CONTINUE
 65                  CONTINUE
 70               CONTINUE
 75            CONTINUE
            GO TO 999
C                                       'RAND'
 100     IF (GOPR.NE.6) GO TO 999
C                                       init random numbers
            CALL RANDIN (IERR)
            GO TO 999
C-----------------------------------------------------------------------
C                                       Do it: grey to Canon
 500  CONTINUE
C                                       All character forms
         IF ((GOPR.EQ.3) .OR. (GOPR.EQ.6)) GO TO 600
C                                       random case
            IF (GOPR.NE.4) GO TO 510
               CALL RANDUM (TV)
               J = 1
               IF (TV.GT.0.3333) J = 2
               IF (TV.GT.0.6666) J = 3
C                                       get character
               I = 25.0 * VALUE + 1.50
               IF (I.LE.0) I = 1
               IF (I.GT.26) I = 26
               GO TO 520
C                                       Finer levels
 510        CONTINUE
               I = (NLEVEL - 1) * VALUE + 1.50
               IF (I.LE.0) I = 1
               IF (I.GT.78) I = 78
               J = (I-1) / 26 + 1
               I = I - (J-1)*26
C                                       Switch to Text mode at start
C                                       of current scan line
 520        WRITE (CHBUF,1040) '}'
            CALL FILZCH(1, 1, CHBUF)
            CALL CANIO(CANLUN, 1, CHBUF, IERR)
            WRITE (CHBUF,1040) 'p'
            CALL FILZCH(1, 1, CHBUF)
            CALL CANIO(CANLUN, 1, CHBUF, IERR)
            CALL CANINT(CANLUN,CHBUF,IX,IERR)
            CALL CANINT(CANLUN,CHBUF,(MAXCAN(2)-(IY-INY)),IERR)
            WRITE (CHBUF,1040) IS2
            CALL FILZCH(1, 1, CHBUF)
            CALL CANIO(CANLUN, 1, CHBUF, IERR)
C                                        select grey-scale font
            WRITE (CHBUF,1038) ESC
            NNCHAR = 5
            CALL FILZCH(NNCHAR, 1, CHBUF)
            CALL CANIO(CANLUN, NNCHAR, CHBUF, IERR)
C                                         and send character
            NXTCHR = ALPHA(J)(I:I)
            WRITE (CHBUF,1040) NXTCHR
            CALL FILZCH(1, 1, CHBUF)
            CALL CANIO(CANLUN, 1, CHBUF, IERR)
C                                          return to vector graphics
            WRITE (CHBUF,1044) CSI
            CALL FILZCH(4,1,CHBUF)
            CALL CANIO(CANLUN,4,CHBUF,IERR)
            GO TO 999
C-----------------------------------------------------------------------
C                                       'RAND', 'SIN ' : each dot
 600     IF ((GOPR.NE.3) .AND. (GOPR.NE.6)) GO TO 999
C         I = INT( (VALUE - 0.001) * (NLEVEL-1) / 0.998)
C         VALUE = 0.001 + (REAL(I) / (NLEVEL-1) * 0.998)
C                                       This may not be correct for
C                                       landscape mode - try it
            INX = NX
            INY = NY
            IF (.NOT.DOLAND) GO TO 610
C               INX = NY
C               INY = NX
 610        NC = ((INX + 7) / 8)
            LLLY = MYD * 1000 - IY
C                                       Send random bits until full
            DO 650 I = 1,INY
               YFIX = MAXCAN(2) - (IY + I - INY)
C                                       Switch to Text mode at start
C                                       of current scan line
               WRITE (CHBUF,1040) '}'
               CALL FILZCH(1, 1, CHBUF)
               CALL CANIO(CANLUN, 1, CHBUF, IERR)
               WRITE (CHBUF,1040) 'p'
               CALL FILZCH(1, 1, CHBUF)
               CALL CANIO(CANLUN, 1, CHBUF, IERR)
               CALL CANINT(CANLUN,CHBUF,IX,IERR)
               CALL CANINT(CANLUN,CHBUF,YFIX,IERR)
               WRITE (CHBUF,1040) IS2
               CALL FILZCH(1, 1, CHBUF)
               CALL CANIO(CANLUN, 1, CHBUF, IERR)
C                                        Send image draw command
C                                        using binary data
C                                        <CSI><quant>;<width>.r
               WIDTH = NC
               QUANT = NC
               IF (WIDTH .LE. 9) THEN
                  WRITE (CHBUF,1041) CSI,QUANT,WIDTH
                  NNCHAR = 6
               ELSE
                  WRITE (CHBUF,1043) CSI,QUANT,WIDTH
                  NNCHAR = 8
               END IF
               CALL FILZCH(NNCHAR,1,CHBUF)
               CALL CANIO(CANLUN, NNCHAR, CHBUF, IERR)
               I1 = 0
               ILY = LLLY + I - 2
               ILY = MOD (ILY, MYD) * MXD + 1
               IL = IX - 2
               DO 645 J = 1,NC
                  CANPOS = CANPOS + 1
                  IF (CANPOS.LE.CANSIZ) GO TO 620
                     CALL ZLASIO ('WRIT', CANLUN, OUTFIL, CANSIZ,
     *                  CANBUF, IERR)
                     IF (IERR.NE.0) GO TO 995
                     CANPOS = 1
 620              IT = 0
C                                       Random case
                  IF (GOPR.EQ.3) GO TO 630
                     DO 625 I2 = 1,8
                        I1 = I1 + 1
                        IF (I1.GT.INX) GO TO 640
                           CALL RANDUM (TV)
                           IF (VALUE.GE.TV) IT = IT + ITJ(I2)
 625                    CONTINUE
                     GO TO 640
C                                       Starlink case
 630              CONTINUE
                     DO 635 I2 = 1,8
                        I1 = I1 + 1
                        IF (I1.GT.INX) GO TO 640
                           IC = ILY + MOD (IL+I1, MXD)
                           IF (VALUE.GT.AMASK(IC)) IT = IT + ITJ(I2)
 635                    CONTINUE
 640              WRITE (CHBUF,1040) CHAR(IT)
                  CALL FILZCH(1, 1, CHBUF)
                  CANBUF(CANPOS:CANPOS+1-1) = CHBUF(1:1+1-1)
 645              CONTINUE
C                                       return to vector graphics
               WRITE (CHBUF,1044) CSI
               CALL FILZCH(4,1,CHBUF)
               CALL CANIO(CANLUN,4,CHBUF,IERR)
 650        CONTINUE
            GO TO 999
C                                       I/O error
 995  WRITE (MSGTXT,1995) IERR
      IERR = 2
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('CANGRY: SWITCH TO DOT-BY-DOT - TOO MUCH CORE NEEDED IN',
     *   ' QUICK MODES')
 1030 FORMAT (A1,I5.5,';78;0;',I1,';3998;2;',I5.5,';',
     *        I5.5,';0;0;254;',I2.2,';',I2.2,';1;1;1;0;0.p')
 1031 FORMAT (4A1)
 1038 FORMAT (A1,'(/)>')
 1040 FORMAT (A1)
 1041 FORMAT (A1,I1,';',I1,'.r')
 1043 FORMAT (A1,I2,';',I2,'.r')
 1044 FORMAT (A1,'0&}')
 1995 FORMAT ('CANGRY: CANON WRITE ERROR',I5)
      END
      SUBROUTINE CANTXT (IXCUR, IYCUR, CANLUN, IERR)
C-----------------------------------------------------------------------
C   This routine exits from vector graphics leaving the cursor
C   at position (IXCUR, IYCUR)
C
C   Inputs:   IXCUR    I   X coordinate value.
C             IYCUR    I   Y coordinate value.
C             CANLUN   I   Unit for CANON print file
C   Output:   IERR     I   Error code: 0 => ok
C                                    10 => vector truncated
C                                    else I/O error
C-----------------------------------------------------------------------
      INTEGER   IXCUR, IYCUR, CANLUN, IERR
C
      CHARACTER CANCOM*16
      CHARACTER*1 CHAR, IS2, CSI, ESC
C-----------------------------------------------------------------------
C                                       Get character values of
C                                       hex codes used by the CANON
      IS2 = CHAR(30)
      CSI = CHAR(155)
      ESC = CHAR(27)
C
      WRITE (CANCOM,1005) IS2
      CALL FILZCH (4, 1, CANCOM)
      CALL CANIO (CANLUN, 4, CANCOM, IERR)
      CALL CANINT(CANLUN,CANCOM,IXCUR,IERR)
      CALL CANINT(CANLUN,CANCOM,IYCUR,IERR)
      WRITE (CANCOM,1010) IS2
      CALL FILZCH (1, 1, CANCOM)
      CALL CANIO (CANLUN, 1, CANCOM, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
C                               End picture (% IS2)
C                               Return to text (} p point IS2)
C                               point is the position at which
C                               the text should be placed. This
C                               position is an integer parameter
C                               which must be encoded by routine
C                               CANINT
 1005 FORMAT ('%',A1,'}p')
 1010 FORMAT (A1)
      END
      SUBROUTINE CANINT (CANLUN, CANCOM, INT, IERR)
C-----------------------------------------------------------------------
C  CANINT outputs an integer parameter to the Canon in vector graphics
C  mode. An integer with an absolute value of less than 16 is output as
C  a single byte, an integer with an absolute value greater than 16 but
C  less than 1024 is output as two bytes and an integer with an absolute
C  value greater than 1024 is output as three bytes. The encoding rules
C  are given on pp 117-120 of volume II of the Canon LBP-8 Programmer's
C  Manual.
C
C  Inputs:  CANLUN    I       LUN for Canon printer
C           CANCOM    C*16    Output buffer
C           INT       I       The number to be output
C  Outputs: IERR      I       CANIO status
C  Common:  CANBUF
C           CANPOS
C-----------------------------------------------------------------------
      INTEGER   CANLUN
      CHARACTER CANCOM*16
      INTEGER   INT, IERR
C
      INTEGER   REM, BIT, SIGN
C-----------------------------------------------------------------------
C                                   Get sign and absolute value of input
C                                   number
      SIGN=ISIGN(1,INT)
      INT=ABS(INT)
      IF(INT.GE.1024) THEN
         BIT=INT/1024
         REM=INT-1024*BIT
         BIT=BIT+64
         WRITE (CANCOM,1010) CHAR(BIT)
         CALL CANIO (CANLUN, 1, CANCOM, IERR)
         IF(IERR.NE.0) GO TO 999
      ELSE
         REM=INT
         END IF
      IF(INT.GE.16) THEN
         BIT=REM/16
         REM=REM-16*BIT
         BIT=BIT+64
         WRITE (CANCOM,1010) CHAR(BIT)
         CALL CANIO (CANLUN, 1, CANCOM, IERR)
         IF(IERR.NE.0) GO TO 999
      ELSE
         REM=INT
         END IF
      IF(SIGN.LT.0) THEN
         BIT=REM+32
      ELSE
         BIT=REM+48
         END IF
      WRITE (CANCOM,1010) CHAR(BIT)
      CALL CANIO (CANLUN, 1, CANCOM, IERR)
      IF(IERR.NE.0) GO TO 999
999   RETURN
C-----------------------------------------------------------------------
1010  FORMAT(A1)
      END
      SUBROUTINE CANIO (CANLUN, N, QBUF, IERR)
C-----------------------------------------------------------------------
C   CANIO moves characters to the buffer and if needed causes an I/O
C   to occur to the CANON "print" file.
C   Inputs: CANLUN   I        LUN of file
C           N        I        Number of new characters
C           QBUF     C*(*)    Character string
C   Output: IERR     I        Error code: 0 -> ok, else I/O error
C-----------------------------------------------------------------------
      INTEGER   CANLUN, N
      CHARACTER QBUF*(*)
      INTEGER   IERR
C
      INTEGER   J, J0, J1
      INCLUDE 'CANPL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      J0 = N
      J1 = 1
C                                       Do an I/O (or two or three...)
 10   IF (CANPOS+J0.LE.CANSIZ) GO TO 30
         J = CANSIZ - CANPOS
         CANPOS = CANPOS + 1
         CANBUF(CANPOS:CANPOS+J-1) = QBUF(J1:J1+J-1)
         CANPOS = CANPOS + J - 1
         J0 = J0 - J
         J1 = J1 + J
         CALL ZLASIO ('WRIT', CANLUN, OUTFIL, CANPOS, CANBUF, IERR)
         IF (IERR.NE.0) GO TO 999
         CANPOS = 0
         GO TO 10
C                                       Just move characters
 30   CONTINUE
         CANPOS = CANPOS + 1
         CANBUF(CANPOS:CANPOS+J0-1) = QBUF(J1:J1+J0-1)
         CANPOS = CANPOS + J0 - 1
C
 999  RETURN
      END
      SUBROUTINE FIXDPP (GOPR, NXDPP, NYDPP, DPARM)
C-----------------------------------------------------------------------
C   FIXDPP attempts to fix up the dots/pixel and mask size to be
C   compatible
C   Inputs: GOPR    I        Type of grey scale
C   In/out: NXDPP   I        Dots/pixel in X: (in) request (out) used
C           NYDPP   I        Dots/pixel in Y: (in) request (out) used
C           DPARM   R(10)    (3) default mask size (in)
C                            (6) use this X mask size (out)
C                            (7) use this Y mask size (out)
C-----------------------------------------------------------------------
      INTEGER   GOPR, NXDPP, NYDPP
      REAL      DPARM(10)
C
      INTEGER   N(2), M, I, J, IROUND, LM, LJ, L, MD(2)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       BITS Parameters
      M = 8
      IF ((GOPR.NE.2) .AND. (GOPR.NE.3)) GO TO 100
         I = IROUND (DPARM(3))
         IF (I.LE.0) I = 0
         IF (I.GT.3) I = 0
         DPARM(3) = I
         IF ((I.EQ.1) .OR. (I.EQ.3)) GO TO 20
            DPARM(5) = 1.0
            DPARM(4) = 8.0
            DPARM(6) = 10.0
            DPARM(7) = 10.0
            M = 8
            GO TO 100
 20      CONTINUE
            IF (DPARM(5).LT.0.01) DPARM(5) = 1.0
            M = ABS (DPARM(4)) + 0.01
            IF (M.LE.4) M = 8
            IF (M.GT.16) M = 16
            IF (DPARM(4).GT.-0.5) DPARM(4) = M
            IF (DPARM(4).LE.-0.5) DPARM(4) = -M
C                                       Not pixel masked
 100  IF (GOPR.EQ.2) GO TO 110
         IF (NYDPP.EQ.NXDPP+1) NYDPP = NXDPP
         IF (NXDPP.EQ.NYDPP+1) NXDPP = NYDPP
         DPARM(9) = M
         DPARM(10) = M
         IF (GOPR.EQ.3) GO TO 150
         GO TO 999
C                                       Pixel masked
 110  CONTINUE
         N(1) = NXDPP
         N(2) = NYDPP
         DO 140 I = 1,2
            DPARM(8+I) = M
            IF ((DPARM(4).LT.0) .AND. (N(I).LT.M)) GO TO 140
            LJ = M
C                                       Program chooses mask
            IF (DPARM(4).LT.0) GO TO 135
               IF (N(I).LT.7) GO TO 140
               IF (N(I).GT.13) GO TO 120
                  DPARM(8+I) = N(I)
                  GO TO 140
 120           LM = N(I)
               LJ = 1
               DO 130 J = 7,13
                  L = N(I) - (N(I)/J) * J
                  IF (L.GE.LM) GO TO 130
                     LM = L
                     LJ = J
 130              CONTINUE
 135        DPARM(8+I) = LJ
            N(I) = (N(I)/LJ) * LJ
 140        CONTINUE
         NXDPP = N(1)
         NYDPP = N(2)
C                                       Default shifts
 150  IF (ABS(DPARM(6)).GT.0.9) DPARM(6) = 0.25 * DPARM(9) / 8.0
      IF (ABS(DPARM(7)).GT.0.9) DPARM(7) = 0.133 * DPARM(10) / 8.0
      MD(1) = DPARM(9) + 0.01
      MD(2) = DPARM(10) + 0.01
      WRITE (MSGTXT,1150) NXDPP, NYDPP, MD
      CALL MSGWRT (2)
      IF (DPARM(3).LT.1.5) WRITE (MSGTXT,1151) DPARM(5), DPARM(6),
     *   DPARM(7)
      IF (DPARM(3).GE.1.5) WRITE (MSGTXT,1152) DPARM(5), DPARM(6),
     *   DPARM(7)
      CALL MSGWRT (2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1150 FORMAT ('Using x,y dots/pix, dot/mask',4I5)
 1151 FORMAT ('      Scale, x-, y-shift  =',3F9.5,' Gaussian')
 1152 FORMAT ('      Scale, x-, y-shift  =',3F9.5,' sine')
      END
      SUBROUTINE CNTPLT (LUN, FIND, IOPOS, IRRN, IX0, SCALEX, CSIZEX,
     *   ICHL, NXA, MAXX, BUFFER, IERR)
C-----------------------------------------------------------------------
C   CNTPLT reads a few records from the plot file to see if some
C   character string or other command will cause the plot to overflow
C   in the X direction (on the right).  It will correct IX0 if possible.
C   Inputs:
C      LUN     I        LUN of open plot file
C      FIND    I        FTAB pointer for plot file
C      IOPOS   I        Position in BUFFER of next command
C      IRRN    I        Record number now in BUFFER.
C      SCALEX  R        X scaling factor
C      CSIZEX  I        Size of character in X
C      ICHL    I        Number chars to left of plot (dots)
C      NXA     I        Number dots in X plot expected
C      MAXX    I        Max X value allowed (1 is min)
C      BUFFER  I(256)   I/O buffer with plot file record IRRN
C   In/out:
C      IX0     I        X bias position
C   Output:
C      IERR    I        Error code from ZFIO
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IOPOS, IRRN, IX0, CSIZEX, MAXX, BUFFER(256),
     *   NXA, ICHL, IERR
      REAL      SCALEX
C
      REAL      DX
      INTEGER   XMAX, XMIN, X, LOPOS, I, IX, NCHAR, IANGL, LX, LRRN
C-----------------------------------------------------------------------
C                                       Init
      X = 0
      XMAX = IX0 + NXA - ICHL
      XMIN = IX0 - ICHL
      LOPOS = IOPOS
      LRRN = IRRN
      IERR = 0
C                                       Read record from disk
 50   IF (LOPOS.LE.256) GO TO 60
 55      LRRN = LRRN + 1
         IF (LRRN.GT.IRRN+4) GO TO 900
         CALL ZFIO ('READ', LUN, FIND, LRRN, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         LOPOS = 1
C                                       Branch to type log record
 60   I = BUFFER(LOPOS)
      IF (I.EQ.0) GO TO 55
      IF ((I.LT.0) .OR. (I.GT.8)) GO TO 900
      GO TO (100, 200, 300, 400, 500, 600, 700, 800), I
C                                       Init
 100  LOPOS = LOPOS + 6
      GO TO 50
C                                       Init line drawing
 200  LOPOS = LOPOS + 20
      GO TO 50
C                                       Init grey scale
 300  LOPOS = LOPOS + 5
      GO TO 50
C                                       Vector position
 400  CONTINUE
C                                       Vector draw
 500  IX = BUFFER(LOPOS+1)
      LOPOS = LOPOS + 3
      X = IX * SCALEX + IX0 + 0.5
      XMIN = MIN (XMIN, X)
      XMAX = MAX (XMAX, X)
      GO TO 50
C                                       Characters
 600  NCHAR = BUFFER(LOPOS+1)
      IANGL = BUFFER(LOPOS+2)
      DX = BUFFER(LOPOS+3) / 100.0
      LX = X + DX * CSIZEX
      IF (IANGL.EQ.0) IX = LX + NCHAR * CSIZEX
      IF (IANGL.NE.0) IX = LX + CSIZEX
      IF (IANGL.EQ.2) LX = LX - (NCHAR-1) * CSIZEX
      XMIN = MIN (XMIN, IX, LX)
      XMAX = MAX (XMAX, LX, IX)
      LOPOS = LOPOS + 5 + ((NCHAR+3)/4)
      GO TO 50
C                                       Do grey scale (quit here)
 700  GO TO 900
C                                       Misc info
 800  LOPOS = LOPOS + 2 + BUFFER(LOPOS+1)
      GO TO 50
C                                       Recover record
 900  IF (LRRN.EQ.IRRN) GO TO 910
         LRRN = IRRN
         CALL ZFIO ('READ', LUN, FIND, LRRN, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Reset origin
 910  I = XMAX - MAXX
      IF (I.LE.0) GO TO 999
         X = XMIN - IX0
         IX0 = IX0 - I - (XMIN-I-1)/2
         IX0 = MAX (2, IX0)
         X = X + IX0
         IF (X.LT.2) IX0 = IX0 + 2 - X
C
 999  RETURN
      END
      SUBROUTINE CANVEC (IX, IY, IXCUR, IYCUR, IN, CANLUN, IERR)
C-----------------------------------------------------------------------
C   This routine moves the cursor position from (IXL,IYL) to (IX,IY).
C   If requested, a vector will be drawn between these points.
C   The input coordinates will be scaled or not according to the input
C   value of IN. The properly scaled cursor coordinates are returned
C   as IXCUR, IYCUR
C
C   INPUTS:   IX    I   X coordinate value.
C             IY    I   Y coordinate value.
C             IN    I   control value:
C                           1 = Scale IX and IY. (IX,IY) are the new
C                               cursor coordinates.
C                           2 = Scale IX and IY. (IX,IY) are the new
C                               cursor coordinates. Draw a vector
C                               between the previous coordinates, which
C                               have been stored as (IXL,IYL), and the
C                               new coordinates.
C                           3 = Do not scale IX and IY. (IX,IY) are
C                               the new cursor coordinates.
C                           4 = Do not scale IX and IY. (IX,IY) are
C                               the new cursor coordinates.Draw a vector
C                               between the previous coordinates, which
C                               have been stored as (IXL,IYL), and the
C                               new coordinates.
C   Output:   IXCUR   I   Scaled cursor x-coordinate
C             IYCUR   I   Scaled cursor y-coordinate
C             IERR    I   Error code: 0 => ok
C                                    10 => vector truncated
C                                    else I/O error
C   Common:   CANBUF
C             CANPOS
C             IXL
C             IYL
C-----------------------------------------------------------------------
      INTEGER   IX, IY, IN, IXCUR, IYCUR, IERR
C
      CHARACTER CANCOM*16
      INTEGER   CANLUN, IXN, IYN, X1, Y1, X2, Y2, QY1, QY2, INC
      LOGICAL   ISERR
      REAL      ALPHA, BETA
      CHARACTER*1 CHAR, X31, IS2
      INCLUDE 'CANPL.INC'
C-----------------------------------------------------------------------
C                                       Get character values of
C                                       hex codes used by the CANON
      X31 = CHAR(49)
      IS2 = CHAR(30)
C
      ISERR = .FALSE.
C                                       Set last X and Y.
      IXN = IX
      IYN = IY
C                                       If IN = 1 or 2 do scaling.
      IF (IN.GT.2) GO TO 10
         IXN = IX*SCALEX + IX0 + .5
         IYN = IY*SCALEY + IY0 + .5
C                                       Draw vector
 10   INC = 1
C                                       If IN = 1 or 3, do not draw
C                                       vector
      IF ((IN.NE.1) .AND. (IN.NE.3)) INC = 2
C                                       Check start point
         X1 = MAX (1, MIN (MAXCAN(1), IXL))
         Y1 = MAX (1, MIN (MAXCAN(2), IYL))
         IF ((X1.EQ.IXL) .AND. (Y1.EQ.IYL)) GO TO 30
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL (X1-IXN) / REAL (IXL-IXN)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL (Y1-IYN) / REAL (IYL-IYN)
            ALPHA = MIN (ALPHA, BETA)
            X1 = IXN + ALPHA * (IXL-IXN) + 0.5
            Y1 = IYN + ALPHA * (IYL-IYN) + 0.5
C                                       Check end point
 30      X2 = MAX (1, MIN (MAXCAN(1), IXN))
         Y2 = MAX (1, MIN (MAXCAN(2), IYN))
         IF ((X2.EQ.IXN) .AND. (Y2.EQ.IYN)) GO TO 40
            IF ((IXN.LT.1) .AND. (IXL.LT.1)) GO TO 90
            IF ((IYN.LT.1) .AND. (IYL.LT.1)) GO TO 90
            IF ((IXN.GT.MAXCAN(1)) .AND. (IXL.GT.MAXCAN(1))) GO TO 90
            IF ((IYN.GT.MAXCAN(2)) .AND. (IYL.GT.MAXCAN(2))) GO TO 90
            ISERR = .TRUE.
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X2-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y2-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X2 = IXL + ALPHA * (IXN-IXL) + 0.5
            Y2 = IYL + ALPHA * (IYN-IYL) + 0.5
 40      CONTINUE
C                               If requested, draw vector from
C                               (X1, QY1) to (X2,QY2)
         QY1 = MAXCAN(2) - Y1
         QY2 = MAXCAN(2) - Y2
C                               Send polyline to CANON
C                               (<3/1> X1 QY1 (X2-X1) (QY2-QY1) IS2)
C                               The 4 integer coordinates are encoded
C                               in an extremely odd CANON-specific
C                               manner in a variable number of ASCII
C                               characters. The routine CANINT does
C                               the encoding and sends the result to
C                               the output buffer
         IF (INC.EQ.2) THEN
            WRITE (CANCOM,1010) X31
            CALL FILZCH (1, 1, CANCOM)
            CALL CANIO (CANLUN, 1, CANCOM, IERR)
            CALL CANINT (CANLUN,CANCOM,X1,IERR)
            CALL CANINT (CANLUN,CANCOM,QY1,IERR)
            CALL CANINT (CANLUN,CANCOM,X2-X1,IERR)
            CALL CANINT (CANLUN,CANCOM,QY2-QY1,IERR)
            WRITE (CANCOM,1010) IS2
            CALL FILZCH (1, 1, CANCOM)
            CALL CANIO (CANLUN, 1, CANCOM, IERR)
            END IF
C                                         Save new position
 90   IXL = IXN
      IYL = IYN
      IXCUR = X2
      IYCUR = QY2
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A1)
      END
