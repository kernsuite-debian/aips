LOCAL INCLUDE 'QMSPL.INC'
      REAL   SEQIN, DSKIN, VERSN, ASPMM, COPIES, XXPEN, DPARM(10),
     *   TRANFN(6), SCALEX, SCALEY, AMASK(256), RNRAND
      HOLLERITH XNAMIN(3), XCLSIN(2), XOUTFL(12), XOPTYP, XTRANF(6)
      CHARACTER NAMIN*12, CLSIN*6, OUTFIL*48, OPTYPE*4, CTRANF*2,
     *   QMBUFF*80
      INTEGER GOPR, ITRN, QMPOS, QMSIZE, IXL, IYL, IX0, IY0, MAXXQM(2),
     *   MXD, MYD, MRAND(100), NRAND, PRAND, QRAND, JRAND
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, ASPMM,
     *   COPIES, XXPEN, XOUTFL, XOPTYP, XTRANF, DPARM, TRANFN, GOPR,
     *   ITRN
      COMMON /CHPARM/ NAMIN, CLSIN, OUTFIL, OPTYPE, CTRANF
C                                       Important NOTE:
C                                       The variables in QMSPL are NOT
C                                       what they appear, DPARM here
C                                       starts with AIPS DPARM(3);
C                                       TRANFN includes several adverbs
C                                       plus the first two words of
C                                       DPARM.
      COMMON /QMSPCL/ QMPOS, QMSIZE, IXL, IYL, IX0, IY0,
     *   SCALEX, SCALEY, MAXXQM, MXD, MYD, AMASK
      COMMON /QMSPCH/ QMBUFF
      COMMON /RANDMM/ RNRAND, MRAND, NRAND, PRAND, QRAND, JRAND
LOCAL END
      PROGRAM QMSPL
C-----------------------------------------------------------------------
C! Task to pass plot (PL) files to a QMS printer
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
C   QMSPL is a program for the AIPS graphics package.  The program runs
C   as a detached task initiated from AIPS.  First a cataloged file is
C   found using data passed from AIPS.  The list of associated files is
C   searched for a PLot file corresponding to the version number.  The
C   graphics commands are converted to character strings appropriate to
C   the QMS device and written to a file which "prints" at the end.
C   Inputs:  (from AIPS)
C      INNAME  R(3)   Name of primary file.
C      INCLASS R(2)   Class of primary file.
C      INSEQ   R      Sequence number of primary file.
C      INDISK  R      Disk volume number. 0 means try all.
C      INVERS  R      Extension file version number. 0 ->
C                     use the highest version number.
C      ASPMM   R      Arc sec per millimeter. 0 -> self scale.
C      COPIES  R      Number of copies desired.
C      LPEN    R      Pen width in dots.  0 => 3.
C      OUTFILE R(12)   output file name
C      OPTYPE  R      'NONE' ignore grey scale commands
C                     'MBIT' use sine pattern to build a font
C                     'BITS' use sine pattern to determine each dot
C                     'QRAN' use random numbers to build character font
C                     'MRAN' use 78 levels nots 26
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
C                     (3,4) rescale g = DPARM(3)*g + DPARM(4) and
C                           clip again to 0 to 1.
C                  Parms for BIT opcodes only:
C                     (5) <=0 -> ignore rest of DPARM, use Gaus
C                         1 -> Use DPARM for Gaussian 'BIT ' mask
C                         2 -> ignore rest of DPARM, use Sine
C                         3 -> Use DPARM for Sine wave 'BIT ' mask
C                     (6) width of mask (dots)
C                     (7) width of Gaussian, sine 0 => 1
C                     (8) x-shift of Gaus/sin in mask
C                     (9) y-shift of Gaus/sin in mask
C   Note: the adverbs are declared internally somewhat differently.
C   FACTOR, AXVAL -> TRANFN(2-4)
C   DPARM(1,2) -> TRANFN(5,6) and DPARM(3,10) -> DPARM(1,8).  The other
C   two positions in DPARM are declared however and used internally.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   BUFFER(256), LUN, FIND, IRET, IERR
      LOGICAL   QUICK
      INCLUDE 'QMSPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGNAM /'QMSPL '/
      DATA LUN /26/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL QMSINI (PRGNAM, LUN, FIND, QUICK, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Write to Tektronix 4012.
      CALL QMSDRW (LUN, FIND, IRET)
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
 1000 FORMAT ('ERROR WRITING TO QMS DEVICE')
      END
      SUBROUTINE QMSINI (PRGNAM, LUN, FIND, QUICK, BUFFER, IRET)
C-----------------------------------------------------------------------
C   QMSINI does the initialization for QMSPL, getting parameters,
C   locating the Plot file, and opening it.
C   Inputs: PRGNAM   C*6)     Task name
C           LUN      I        LUN to use for plot file
C   Output: FIND     I        FTAB pointer for plot file
C           QUICK    L        Quick return desired or not.
C           BUFFER   I(256)   Plot buffer
C           IRET     I        Error code: not 0 => quit.
C-----------------------------------------------------------------------
      CHARACTER  TYPIN*2, STATUS*4, PRGNAM*6, GFILE*48, OKOPS(6)*4,
     *    OKTRN(5)*2
      INTEGER  LUN, FIND, IRET, BUFFER(256), CATERR, IERR, NPARMS,
     *   SLOT, IVER, I, IVOL, ISEQ, USER, NOPS, NTRN, IROUND
      LOGICAL   NOMAP, QUICK, NOEXCL, WAIT, EQUAL
      INCLUDE 'QMSPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C      INCLUDE 'INCS:DLOC.INC'
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
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
 10   IF (QUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Set DPARMs not set by GTPARM
      DPARM(9) = 1.
      DPARM(10) = 1.
C                                       Hollerith -> char.
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
      IF ((TRANFN(3).GE.0.001) .AND. (TRANFN(3).LE.0.999) .AND.
     *   (TRANFN(4).GE.0.001) .AND. (TRANFN(4).LE.0.999)) GO TO 15
         TRANFN(3) = 0.5
         TRANFN(4) = 0.5
C                                       Get OPcode for grey conversion
 15   GOPR = 0
      DO 20 I = 1,NOPS
         IF (OPTYPE.EQ.OKOPS(I)) GOPR = I
 20      CONTINUE
      IF (GOPR.EQ.0) GOPR = 2
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
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1070)
         GO TO 995
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
      SUBROUTINE QMSDRW (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the
C   QMS.
C   INPUTS:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            BUFFER   I(256)   buffer to use for I/O.
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      CHARACTER CHBUF*200, OPTELL*4, CHLAND(2)*1
      REAL   DX, DY, SCALEF, X, XYRATO, Y, RX1, RX2, TOTPIX, VALUE, RY1,
     *   RY2, AX, AY, XPXSEP, YPXSEP, YQMDMM, UASPMM, XYDIFF, YASPMM,
     *   DELTA, XQMDMM, RCALEX, RCALEY, GRSCAL, GROFF, NOWPIX, TEMPV,
     *   DUMMY
      INTEGER   ITYPE, BUFFER(256), QY, LXA, FIND, GRYERR, CSIZQM(2),
     *   NNCHAR, LYA, IANGL, ICHB, ICHL, ICHR, ICHT, IERR, INCRRN,
     *   IOPOS, IX, IX1, IY, IY1, LUN, NXA, NYA, OPCODE, QMSLUN, I,
     *   IERRC, IX2, IY2, NCHAR, IROUND, LLPEN, IGRLO, IGRHI, NPTSX,
     *   NPTSY, NXDPP, NYDPP, IPER, IPER2, N, ICNT, NPIX, IERRCH, ILAND,
     *   SCRTCH(256), IORRN
      LOGICAL   DOLAND, DOGREY, T
      HOLLERITH HBUFFR(256)
      INCLUDE 'QMSPL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (HBUFFR, BUFFER)
      DATA QMSLUN, T /7, .TRUE./
      DATA CHLAND /'P','L'/
C-----------------------------------------------------------------------
C                                       Initialize all values.
      MAXXQM(1) = 10. * 300
      MAXXQM(2) = 7.5 * 300
      CSIZQM(1) = 33
      CSIZQM(2) = 50
      XQMDMM = 11.81
C                                       Assume symetric device for now.
      YQMDMM = XQMDMM
      IERR = 0
      IERRC = 0
      IERRCH = 0
      GRYERR = 0
      DOGREY = .FALSE.
      IOPOS = 9999
      ICNT = 0
      QMPOS = 0
      QMSIZE = 80
      QMBUFF = ' '
C                                       Open QMS file
      CALL ZLASIO ('OPEN', QMSLUN, OUTFIL, NNCHAR, QMBUFF, IERR)
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
 60   IF (ICNT.GT.0) GO TO 65
         CALL GTTELL (0, OPTELL, DUMMY, SCRTCH, IERR)
         IF (IERR.LE.1) GO TO 65
            WRITE (MSGTXT,1060) OPTELL
            CALL MSGWRT (4)
            IF (IERR.EQ.2) GO TO 990
            GO TO 993
 65   ICNT = ICNT + 1
      IF (ICNT.GE.200) ICNT = 0
      OPCODE = BUFFER(IOPOS)
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
      ICHL   = (BUFFER(IOPOS+11)/10.0) * CSIZQM(1)  +  0.5
      ICHB   = (BUFFER(IOPOS+12)/10.0) * CSIZQM(2)  +  0.5
      ICHR   = (BUFFER(IOPOS+13)/10.0) * CSIZQM(1)  +  0.5
      ICHT   = (BUFFER(IOPOS+14)/10.0) * CSIZQM(2) +  0.5
      IOPOS = IOPOS + 15
C                                       Find no. pixels inside border.
      NYA = MAXXQM(2) - ICHT - ICHB - 1
      NXA = MAXXQM(1) - ICHL - ICHR - 1
      LYA = MAXXQM(1) - ICHT - ICHB - 1
      LXA = MAXXQM(2) - ICHL - ICHR - 1
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
         I = MAXXQM(2)
         MAXXQM(2) = MAXXQM(1)
         MAXXQM(1) = I
C                                       Mm / arc sec scaling
 230  LOCNUM = 1
      AXTYP(LOCNUM) = 0
      UASPMM = ASPMM
      IF ((ITYPE.NE.2) .AND. (ITYPE.NE.3) .AND. (ITYPE.NE.6)) GO TO 235
         CALL SETLOC (BUFFER(IOPOS), T)
         IF (AXTYP(LOCNUM).NE.1) GO TO 235
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
C                                       odd: but XYRATO has AXINC(1)/(2)
            YPXSEP = XPXSEP
            CALL SCALMM (AX, AY, XPXSEP, YPXSEP, SCALEF, XQMDMM,
     *         YQMDMM, ASPMM, SCALEX, SCALEY)
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
      IX0 = ICHL + MAX(0,MAXXQM(1)-NXA)/2 + 1
      NYA = SCALEY*SCALEF*Y/AY + ICHB + ICHT
      IY0 = ICHB + MAX(0,MAXXQM(2)-NYA)/2 + 1
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
     *   .AND. (NPTSX.LT.MAXXQM(1)-ICHL-ICHR)) GO TO 310
         WRITE (MSGTXT,1305) IGRLO, IGRHI, NPTSX, NPTSY
         CALL MSGWRT (7)
         DOGREY = .FALSE.
         GO TO 350
C                                       Change to RAND if small
 310  TOTPIX = REAL(NPTSX) * REAL(NPTSY)
      IF ((GOPR.EQ.3) .OR. (GOPR.EQ.6) .OR. (TOTPIX.GT.156)) GO TO 320
         WRITE (MSGTXT,1310)
         CALL MSGWRT (3)
         GOPR = 1
         DOGREY = .FALSE.
C                                       dots per pixel
 320  NXDPP = (NXA - ICHL - ICHR) * XYRATO / X  + 0.05
      NYDPP = (NYA - ICHT - ICHB) / Y  + 0.05
      IF (NXDPP*X/XYRATO.GT.MAXXQM(1)-ICHL-ICHR) NXDPP = NXDPP - 1
      IF (NYDPP*Y.GT.MAXXQM(2)-ICHB-ICHT) NYDPP = NYDPP - 1
C                                       Check for aspmm type
      IF ((AXTYP(LOCNUM).EQ.1) .AND. (AXINC(1,LOCNUM).NE.0.0)) THEN
         IF (ABS(ABS(XYRATO*AXINC(2,LOCNUM) /
     *      AXINC(1,LOCNUM))-1.0).LT.0.05) THEN
            IF (NXDPP.EQ.NYDPP-1) NYDPP = NXDPP
            IF (NXDPP.EQ.NXDPP-1) NXDPP = NYDPP
            END IF
         END IF
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
      IX0 = ICHL + MAX (0, MAXXQM(1) - NXA) / 2
      IY0 = ICHB + MAX (0, MAXXQM(2) - NYA) / 2
      GRSCAL = 0.998 / (REAL(IGRHI) - REAL(IGRLO))
      GROFF = IGRLO - 0.001 / GRSCAL + TRANFN(5) / GRSCAL
      GRSCAL = GRSCAL / (TRANFN(6) - TRANFN(5))
C                                       Offset for negative coords
 350  IX0 = IX0 - (RX1-IX1) * XYRATO * SCALEX * SCALEF / AX
      IY0 = IY0 - (RY1-IY1) * SCALEY * SCALEF / AY
C                                       Display arcsec / mm info
      IF (AXTYP(LOCNUM).NE.1) GO TO 370
         ASPMM = ABS (AX * AXINC(1,LOCNUM) * 3600.0 * XQMDMM)
     *      / (SCALEX * SCALEF * XYRATO)
         YASPMM = ABS (AY * AXINC(2,LOCNUM) * 3600.0 * YQMDMM)
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
 360     IF ((UASPMM.LE.0.0) .OR. (UASPMM.GE.ASPMM-DELTA)) GO TO 370
            WRITE (MSGTXT,1360)
            CALL MSGWRT (2)
C                                       Inits: on QUIC
 370  WRITE (CHBUF,2370)
      NNCHAR = 5
      CALL ZLASIO ('WRIT', QMSLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       on freeform,landscape/portrait
      WRITE (CHBUF,2371) CHLAND(ILAND)
      NNCHAR = 8
      CALL ZLASIO ('WRIT', QMSLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Copies
      NNCHAR = IROUND (COPIES)
      IF (NNCHAR.LE.0) NNCHAR = 1
      WRITE (CHBUF,2373) NNCHAR
      NNCHAR = 31
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       set top,left borders
      WRITE (CHBUF,2374)
      NNCHAR = 22
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       on graphics, pen width
      LLPEN = IROUND (XXPEN)
      IF (LLPEN.LT.1) LLPEN = 3
      IF (LLPEN.GT.99) LLPEN = 99
      WRITE (CHBUF,2375) LLPEN
      NNCHAR = 11
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
C                                       Init grey scale generation
      IF (.NOT.DOGREY) GO TO 380
         CALL QMSGRY (1, QMSLUN, GOPR, NXDPP, NYDPP, IX, IY, DOLAND,
     *      LLPEN, VALUE, DPARM, CHBUF, IERR)
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
C                                       Send a dummy character line
C                                       avoids a problem hopefully
 380  CALL QMSVEC (0, 0, 1, QMSLUN, IERR)
      WRITE (CHBUF,2600)
      NNCHAR = 33
      CHBUF(33:36) = '    '
      NNCHAR = 36
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      WRITE (CHBUF,2375) LLPEN
      NNCHAR = 11
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL CNTPLT (LUN, FIND, IOPOS, IORRN, IX0, SCALEX, CSIZQM(1),
     *   ICHL, NXA, MAXXQM(1), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 50
C                                       Opcode 4, position cursor.
 400  IX = BUFFER(IOPOS+1)
      IY = BUFFER(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL QMSVEC (IX, IY, 1, QMSLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 5, draw vector.
 500  IX = BUFFER(IOPOS+1)
      IY = BUFFER(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL QMSVEC (IX, IY, 2, QMSLUN, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.10) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 6, write characters.
 600  NCHAR = BUFFER(IOPOS+1)
      IANGL = BUFFER(IOPOS+2)
      DX    = BUFFER(IOPOS+3)/100.0
      DY    = BUFFER(IOPOS+4)/100.0
      IX = IXL + DX * CSIZQM(1)
      IY = IYL + DY * CSIZQM(2)
      QY = IY + 0.25 * CSIZQM(2)
C                                       horizontal
      IF (IANGL.NE.1) THEN
         CALL QMSVEC (IX, QY, 3, QMSLUN, IERR)
         IF (IERR.EQ.10) IERRCH = IERRCH + 1
         IF (IERR.EQ.10) GO TO 630
         IF (IERR.NE.0) GO TO 960
         WRITE (CHBUF,2600)
         NNCHAR = 33
         CALL H2CHR (NCHAR, 1, HBUFFR(IOPOS+5), CHBUF(33:33+NCHAR-1))
         NNCHAR = NCHAR + NNCHAR - 1
         CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       on graphics
         WRITE (CHBUF,2375) LLPEN
         NNCHAR = 11
         CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       vertical
      ELSE
         DO 620 I = 1,NCHAR
            CALL QMSVEC (IX, QY, 3, QMSLUN, IERR)
            IF (IERR.EQ.10) IERRCH = IERRCH + 1
            IF (IERR.EQ.10) GO TO 630
            IF (IERR.NE.0) GO TO 960
            WRITE (CHBUF,2600)
            NNCHAR = 33
            CALL H2CHR (1, I, HBUFFR(IOPOS+5), CHBUF(33:))
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
C                                       Reposition vertical
            QY = QY - CSIZQM(2)
C                                       on graphics
            WRITE (CHBUF,2375) LLPEN
            NNCHAR = 11
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
 620        CONTINUE
         END IF
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
     *      VALUE = LOG10 (30.0*VALUE + 1.0) / LOG10 (30.42) - .007
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
            CALL QMSVEC (IX, IY, 3, QMSLUN, IERR)
            IF (IERR.EQ.10) GO TO 730
            IF (IERR.NE.0) GO TO 960
            WRITE (CHBUF,2720)
            NNCHAR = 32
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 960
C                                       do it
 725     CALL QMSGRY (2, QMSLUN, GOPR, NXDPP, NYDPP, IX, IY, DOLAND,
     *      LLPEN, VALUE, DPARM, CHBUF, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       reset position
 730     IF (IANGL.EQ.0) IX = IX + NXDPP
         IF (IANGL.EQ.1) IY = IY + NYDPP
         IF (IANGL.EQ.2) IX = IX - NXDPP
         IF (IANGL.EQ.3) IY = IY - NYDPP
         IOPOS = IOPOS + 1
C                                       on graphics
         IF ((IANGL.EQ.0) .AND. (N.LT.NPIX)) GO TO 790
            WRITE (CHBUF,2375) LLPEN
            NNCHAR = 11
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
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
C                                       Close files.
C                                       delete font
C                                       normal syntax
 990  WRITE (CHBUF,2990) CHLAND(ILAND)
      NNCHAR = 38
      CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 993
      IF (QMPOS.GT.0) CALL ZLASIO ('WRIT', QMSLUN, OUTFIL, QMPOS,
     *   QMBUFF,
     *   IERR)
      IF (IERR.NE.0) GO TO 993
C                                       and quit (off free form)
      WRITE (CHBUF,2992)
      NNCHAR = 5
      CALL ZLASIO ('WRIT', QMSLUN, OUTFIL, NNCHAR, CHBUF, IERR)
      IF (IERR.NE.0) GO TO 993
         CALL ZLASIO ('CLOS', QMSLUN, OUTFIL, 0, CHBUF, IERR)
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
C                                       Some changes made 870806
C                                       Pat Murphy
C                                       QUIC programming errors and the
C                                       infamous ^PN^- documentation
C                                       slipup by QMS, inc. (it has to
C                                       be on a line by itself).
 1000 FORMAT ('QMSDRW: ERROR',I4,' OPENING QMS FILE/DEVICE')
 1060 FORMAT (A4,' Operation received from tell')
 1200 FORMAT ('QMSDRW: ERR FROM GINITL ARGS. NXA, NYA=',2I6)
 1205 FORMAT ('QMSDRW: ERR FROM GINITL ARGS. X,Y=',2E10.3)
 1235 FORMAT ('Plot reduced in size by factor',F6.3)
 1300 FORMAT ('QMSDRW: Init grey scale out of order, ignored')
 1305 FORMAT ('QMSDRW: BAD PARMS FROM GINITG:',4I7)
 1310 FORMAT ('FEWER THAN 157 PIXELS, USE BITS OR RAND')
 1320 FORMAT ('PIXELS TOO SMALL FOR QRAN OR MRAN, USE RAND')
 1321 FORMAT ('PIXELS TOO SMALL FOR MBIT, USE BITS')
 1350 FORMAT ('Using',1PE12.5,' arc sec per mm')
 1355 FORMAT ('Using',1PE12.5,' arc sec per mm for x axis')
 1356 FORMAT ('Using',1PE12.5,' arc sec per mm for y axis')
 1360 FORMAT ('Which overrides user ASPMM in order to fit plot on',
     *   ' screen')
 1370 FORMAT ('QMSDRW: ERROR',I5,' INITIALIZING GREY SCALE')
 1375 FORMAT ('Grey scale initialization complete')
 1790 FORMAT (I3,' %  of grey scale pixels plotted')
 1960 FORMAT ('QMSDRW: QMS WRITE ERROR')
 1970 FORMAT ('QMSDRW: INVALID OPCODE',I5,' IN GRAPH FILE')
 1980 FORMAT ('QMSDRW: DISK IO RELATED ERROR',I3)
 1993 FORMAT ('QMSDRW: ',I5,' GREY SCALE OPCODES IGNORED')
 1995 FORMAT (I8,' vectors truncated at edges')
 1996 FORMAT (I4,' character strings omitted at edges')
 2370 FORMAT ('^PY^-')
 2371 FORMAT ('^IO',A1,'^G^-')
 2373 FORMAT ('^DC',I4.4,'^ISYNTAX00010^G^ISTFX0^G')
 2374 FORMAT ('^IT0150^G^IJ0150^G^F^-')
 2375 FORMAT ('^IGV^G^PW',I2.2)
 2600 FORMAT ('^IGE^G^SM00204^IL0600^G^IC0900^G')
 2720 FORMAT ('^IGE^G^SM09001^IL0000^G^IC0000^G')
C                                       Drop "^," at end for test
 2990 FORMAT ('^IGE^G^DF09001',A1,'^G^ISYNTAX00000^O^G^-')
 2992 FORMAT ('^PN^-')
      END
      SUBROUTINE QMSVEC (IX, IY, IN, QMSLUN, IERR)
C-----------------------------------------------------------------------
C   This routine will put control characters, and X and Y coordinates
C   into the QMS output file.
C   INPUTS:   IX    I   X coordinate value.
C             IY    I   Y coordinate value.
C             IN    I   control value:
C                           1 = Scale IX and IY and preceed coordinates
C                               by 'write dark vector' control charactr
C                           2 = Scale IX and IY, put in buffer.
C                           3 = IX and IY are not scaled, 'write dark
C                               vector' control character is put into
C                               the buffer.
C                           4 = no scale, write vector
C             QMSLUN  I   LUN of QMS file
C   Output:   IERR    I   Error code: 0 => ok
C                                    10 => vector truncated
C                                    else I/O error
C   Common:   QMBUFF
C             QMPOS
C             IXL
C             IYL
C-----------------------------------------------------------------------
      CHARACTER QMSCOM*16, COMAND(2)*2
      INTEGER   IX, IY, IN, QMSLUN, IERR
      REAL      ALPHA, BETA
      INTEGER   IXN, IYN, X, Y, QY, INC
      LOGICAL   ISERR
      INCLUDE 'QMSPL.INC'
      DATA COMAND /'^U','^D'/
C-----------------------------------------------------------------------
      ISERR = .FALSE.
C                                       Set last X and Y.
      IXN = IX
      IYN = IY
C                                       If IN = 1 or 2 do scaling.
      IF (IN.GT.2) GO TO 10
         IXN = IX*SCALEX + IX0 + .5
         IYN = IY*SCALEY + IY0 + .5
C                                       Bright vector
 10   INC = 1
C                                       If IN = 1 or 3 'dark vector'.
      IF ((IN.NE.1) .AND. (IN.NE.3)) INC = 2
      IF (INC.EQ.2) GO TO 30
C                                       Check start point
         X = MAX (1, MIN (MAXXQM(1), IXL))
         Y = MAX (1, MIN (MAXXQM(2), IYL))
         IF ((X.EQ.IXL) .AND. (Y.EQ.IYL)) GO TO 30
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL (X-IXN) / REAL (IXL-IXN)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL (Y-IYN) / REAL (IYL-IYN)
            ALPHA = MIN (ALPHA, BETA)
            X = IXN + ALPHA * (IXL-IXN) + 0.5
            Y = IYN + ALPHA * (IYL-IYN) + 0.5
            QY = MAXXQM(2) - Y
            WRITE (QMSCOM,1010) COMAND(1), X, QY
            CALL QMSIO (QMSLUN, 12, QMSCOM, IERR)
            IF (IERR.NE.0) GO TO 90
C                                       Check end point
 30      X = MAX (1, MIN (MAXXQM(1), IXN))
         Y = MAX (1, MIN (MAXXQM(2), IYN))
         IF ((X.EQ.IXN) .AND. (Y.EQ.IYN)) GO TO 40
            IF ((IXN.LT.1) .AND. (IXL.LT.1)) GO TO 90
            IF ((IYN.LT.1) .AND. (IYL.LT.1)) GO TO 90
            IF ((IXN.GT.MAXXQM(1)) .AND. (IXL.GT.MAXXQM(1))) GO TO 90
            IF ((IYN.GT.MAXXQM(2)) .AND. (IYL.GT.MAXXQM(2))) GO TO 90
            ISERR = .TRUE.
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X = IXL + ALPHA * (IXN-IXL) + 0.5
            Y = IYL + ALPHA * (IYN-IYL) + 0.5
 40      QY = MAXXQM(2) - Y
         WRITE (QMSCOM,1010) COMAND(INC), X, QY
         CALL QMSIO (QMSLUN, 12, QMSCOM, IERR)
C                                         Save new pos
 90   IXL = IXN
      IYL = IYN
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 10
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (A2,I4.4,':',I4.4,';')
      END
      SUBROUTINE QMSIO (QMSLUN, N, QBUF, IERR)
C-----------------------------------------------------------------------
C   QMSIO moves characters to the buffer and if needed causes an I/O
C   to occur to the QMS "print" file.
C   Inputs: QMSLUN   I        LUN of file
C           N        I        Number of new characters
C           QBUF     C(*)     Character string (packed)
C   Output: IERR     I        Error code: 0 -> ok, else I/O error
C-----------------------------------------------------------------------
      CHARACTER  QBUF*(*)
      INTEGER   QMSLUN, N, IERR
      INTEGER   J, J0, J1
      INCLUDE 'QMSPL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      J0 = N
      J1 = 1
C                                       Do an I/O (or two or three...)
 10   IF (QMPOS+J0.LE.QMSIZE) GO TO 30
         J = QMSIZE - QMPOS
         QMPOS = QMPOS + 1
         QMBUFF(QMPOS:QMPOS+J-1) = QBUF(J1:J1+J-1)
         QMPOS = QMPOS + J - 1
         J0 = J0 - J
         J1 = J1 + J
         CALL ZLASIO ('WRIT', QMSLUN, OUTFIL, QMPOS, QMBUFF, IERR)
         IF (IERR.NE.0) GO TO 999
         QMPOS = 0
         GO TO 10
C                                       Just move characters
 30   CONTINUE
         QMPOS = QMPOS + 1
         QMBUFF(QMPOS:QMPOS+J0-1) = QBUF(J1:J1+J0-1)
         QMPOS = QMPOS + J0 - 1
C
 999  RETURN
      END
      SUBROUTINE QMSGRY (IOP, QMSLUN, GGOPR, NX, NY, IX, IY, DOLAND,
     *   LLPEN, VALUE, DDPARM, CHBUF, IERR)
C-----------------------------------------------------------------------
C   QMSGRY performs (IOP=1) the grey scale initialization needed for the
C   given operation: GGOPR = 4,5 compute random character set and
C   download to the QMS, GGOPR = 6 initialize random number generator,
C   GGOPR = 2,3 prepare sine/Gaussian array for masking and on 2
C   download a character set.  On IOP=2, it writes the pixel to the
C   device as a character (GGOPR = 2,4,5) or as a dot matrix.
C   Inputs:
C      IOP      I      = 1 -> INIT, = 2 -> do it.
C      QMSLUN   I      LUN for open QMS print file.
C      GGOPR    I      Type operation: 2 MBIT, 3 BITS, 4 QRAN, 5 MRAN,
C                         6 RAND - On IOP=1, can be changed to 3 or 6.
C      NX       I      Number dots per pixel in X
C      NY       I      Number dots per pixel in Y
C      IX       I      Current dot position of pixel in X.
C      IY       I      Current dot position of pixel in Y.
C      DOLAND   I      T => landscape mode, else portrait
C      LLPEN    I      Pen width for graphics
C      VALUE    R      Grey scale pixel value (0. to 1.)
C   Output:
C      CHBUF    C*80   Scratch buffer
C      IERR     I      Error code: 0 -> ok, 1 -> input error,
C                         2 -> I/O error
C-----------------------------------------------------------------------
      CHARACTER CHBUF*80, XOUFIL*48, CHLAND(2)*1, CHHEX(16)*1,
     *   ALPHA(3)*26, CTEMP*2
      INTEGER   IOP, QMSLUN, GGOPR, NX, NY, IX, IY, LLPEN, IERR
      LOGICAL   DOLAND
      REAL      VALUE, DDPARM(10)
      INTEGER   ITJ(4), I, J, NNCHAR, INX, INY, NC, I1, I2, IT, IL, IC,
     *   JJ, IOFF, ILY, LLLY
      REAL      TV, SX, SY, AM, ADX, ADY, CNX, CNY, CAX, CAY, CBX, CBY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'QMSPL.INC'
      DATA CHLAND, XOUFIL /'P','L',' '/
      DATA CHHEX /'0','1','2','3','4','5','6','7','8','9','A','B','C',
     *   'D','E','F'/
      DATA ITJ /8, 4, 2, 1/
      DATA ALPHA /'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     *            'abcdefghijklmnopqrstuvwxyz',
     *            '1234567890!@#$%&*()-+=<>,.'/
C-----------------------------------------------------------------------
C                                       Test inputs
      IERR = 1
      IF ((IOP.NE.1) .AND. (IOP.NE.2)) GO TO 999
      IF ((GGOPR.LT.2) .OR. (GGOPR.GT.6)) GO TO 999
      IERR = 0
C                                       Initialization
      IF (IOP.EQ.2) GO TO 500
         MXD = DDPARM(9) + 0.01
         MYD = DDPARM(10) + 0.01
         IF (MXD.LE.0) MXD = 1
         IF (MYD.LE.0) MYD = 1
C                                       'QRAN', 'MRAN', 'MBIT', 'BITS'
         IF (GGOPR.EQ.6) GO TO 100
C                                       Get BIT mask
            IF ((GGOPR.EQ.4) .OR. (GGOPR.EQ.5)) GO TO 20
               INX = MXD / 2 + 1
               SX = 6.283185 * DDPARM(5) / MXD
               INY = MYD / 2 + 1
               SY = 6.283185 * DDPARM(5) / MYD
               AM = 0.0
               ADX = DDPARM(5) / (MXD/4.0) ** 2
               ADY = DDPARM(5) / (MYD/4.0) ** 2
               CNX = INX - DDPARM(6)
               CNY = INY - DDPARM(7)
               CAX = MXD + 1 - DDPARM(6)
               CAY = MYD + 1 - DDPARM(7)
               CBX = 1 - DDPARM(6)
               CBY = 1 - DDPARM(7)
               I1 = 0
               DO 15 I = 1,MYD
                  TV = COS (SY * (I-CNY))
                  ILY = I1 + 1
                  DO 10 J = 1,MXD
                     I1 = I1 + 1
                     AMASK(I1) = 0.4995 * (TV * COS (SX*(J-CNX))
     *                  + 1.0)
                     IF (DDPARM(3).GT.1.5) GO TO 10
                        AMASK(I1) =
     *                     EXP (-ADX*((J-CNX)**2) - ADY*((I-CBY)**2))
     *                   + EXP (-ADX*((J-CAX)**2) - ADY*((I-CNY)**2))
     *                   + EXP (-ADX*((J-CNX)**2) - ADY*((I-CAY)**2))
     *                   + EXP (-ADX*((J-CBX)**2) - ADY*((I-CNY)**2))
                        AM = MAX (AM, AMASK(I1))
 10                  CONTINUE
 15               CONTINUE
               IF (DDPARM(3).GT.1.5) GO TO 19
                  AM = 1.001 * AM
                  I1 = 0
                  DO 18 I = 1,MYD
                     ILY = I1 + 1
                     DO 17 J = 1,MXD
                        I1 = I1 + 1
                        AMASK(I1) = 1.0 - AMASK(I1) / AM
 17                     CONTINUE
 18                  CONTINUE
 19            IF (GGOPR.EQ.3) GO TO 999
C                                       get sizes
 20         INX = NX
            INY = NY
            IF (.NOT.DOLAND) GO TO 25
               INX = NY
               INY = NX
 25         NC = ((INX+15) / 16) * 4
C                                       memory requirements
            I2 = (((NC/2.) * INY + 10.) * 78. + 1023.) / 1024.
            IF (I2.LE.100) GO TO 30
               GGOPR = 1
               WRITE (MSGTXT,1025)
               CALL MSGWRT (6)
               GO TO 100
C                                       init random numbers
 30         CALL RANDIN (IERR)
            IF (IERR.NE.0) GO TO 999
C                                       compute a character set
            I = 1
            IF (DOLAND) I = 2
            WRITE (CHBUF,1030) CHLAND(I), NY
            NNCHAR = 23
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 995
C                                       define characters
            DO 75 I = 1,26
               DO 70 J = 1,3
C                                       character in hex
                  IT = ICHAR (ALPHA(J)(I:I))
                  CALL ZHEX (IT, 2, CTEMP)
                  WRITE (CHBUF,1031) CTEMP, NX, INY, INX
                  NNCHAR = 20
                  CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
                  IF (IERR.NE.0) GO TO 995
C                                       Value of char
                  VALUE = 0.001 + 0.998 * (I-1) / 25.0
                  IF ((GGOPR.EQ.2) .OR. (GGOPR.EQ.5)) VALUE = 0.001
     *               + 0.998 * ((J-1)*26+I-1) / 77.
                  IOFF = -1
                  DO 65 IL = 1,INY
                     I1 = 0
                     ILY = MOD (IL+IOFF, MYD) * MXD + 1
                     DO 60 IC = 1,NC
                        QMPOS = QMPOS + 1
                        IF (QMPOS.LE.QMSIZE) GO TO 35
                           CALL ZLASIO ('WRIT', QMSLUN, XOUFIL, QMSIZE,
     *                        QMBUFF, IERR)
                           IF (IERR.NE.0) GO TO 995
                           QMPOS = 1
 35                     IT = 1
C                                       Random functions
                        IF (GGOPR.EQ.2) GO TO 45
                           DO 40 JJ = 1,4
                              I1 = I1 + 1
                              IF (I1.GT.INX) GO TO 55
                                 CALL RANDUM (TV)
                                 IF (VALUE.GT.TV) IT = IT + ITJ(JJ)
 40                           CONTINUE
                           GO TO 55
C                                       Starlink function
 45                     CONTINUE
                           DO 50 JJ = 1,4
                              I1 = I1 + 1
                              IF (I1.GT.INX) GO TO 55
                                 I2 = ILY + MOD (I1+IOFF, MXD)
                                 IF (VALUE.GT.AMASK(I2)) IT = IT +
     *                              ITJ(JJ)
 50                           CONTINUE
 55                     QMBUFF(QMPOS:) = CHHEX(IT)
 60                     CONTINUE
 65                  CONTINUE
 70               CONTINUE
 75            CONTINUE
            WRITE (CHBUF,1075) LLPEN
            NNCHAR = 13
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 995
            GO TO 999
C                                       'RAND'
 100     IF (GGOPR.NE.6) GO TO 999
C                                       init random numbers
            CALL RANDIN (IERR)
            GO TO 999
C                                       Do it: grey to QMS
 500  CONTINUE
C                                       All character forms
         IF ((GGOPR.EQ.3) .OR. (GGOPR.EQ.6)) GO TO 600
C                                       random case
            IF (GGOPR.NE.4) GO TO 510
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
               I = 77.0 * VALUE + 1.50
               IF (I.LE.0) I = 1
               IF (I.GT.78) I = 78
               J = (I-1) / 26 + 1
               I = I - (J-1)*26
C                                       Position in ALPHA
 520        QMPOS = QMPOS + 1
C                                       Do I/O when needed
            IF (QMPOS.LE.QMSIZE) GO TO 530
               CALL ZLASIO ('WRIT', QMSLUN, XOUFIL, QMSIZE, QMBUFF,
     *            IERR)
               IF (IERR.NE.0) GO TO 995
               QMPOS = 1
 530        QMBUFF(QMPOS:QMPOS) = ALPHA(J)(I:I)
            GO TO 999
C                                       'RAND', 'SIN ' : each dot
 600     IF ((GGOPR.NE.3) .AND. (GGOPR.NE.6)) GO TO 999
C                                       This may not be correct for
C                                       landscape mode - try it
            INX = NX
            INY = NY
            IF (.NOT.DOLAND) GO TO 610
C               INX = NY
C               INY = NX
 610        NC = ((INX + 15) / 16) * 4
            WRITE (CHBUF,1610) INX
            NNCHAR = 6
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 995
            LLLY = MYD * 1000 - IY
C                                       Send random bits until full
            DO 650 I = 1,INY
               I1 = 0
               ILY = LLLY + I - 2
               ILY = MOD (ILY, MYD) * MXD + 1
               IL = IX - 2
               DO 645 J = 1,NC
                  QMPOS = QMPOS + 1
                  IF (QMPOS.LE.QMSIZE) GO TO 620
                     CALL ZLASIO ('WRIT', QMSLUN, XOUFIL, QMSIZE,
     *                  QMBUFF,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 995
                     QMPOS = 1
 620              IT = 1
C                                       Random case
                  IF (GGOPR.EQ.3) GO TO 630
                     DO 625 I2 = 1,4
                        I1 = I1 + 1
                        IF (I1.GT.INX) GO TO 640
                           CALL RANDUM (TV)
                           IF (VALUE.GE.TV) IT = IT + ITJ(I2)
 625                    CONTINUE
                     GO TO 640
C                                       Starlink case
 630              CONTINUE
                     DO 635 I2 = 1,4
                        I1 = I1 + 1
                        IF (I1.GT.INX) GO TO 640
                           IC = ILY + MOD (IL+I1, MXD)
                           IF (VALUE.GT.AMASK(IC)) IT = IT + ITJ(I2)
 635                    CONTINUE
 640              QMBUFF(QMPOS:) = CHHEX(IT)
 645              CONTINUE
 650           CONTINUE
C                                       End dots
            WRITE (CHBUF,1650)
            NNCHAR = 2
            CALL QMSIO (QMSLUN, NNCHAR, CHBUF, IERR)
            IF (IERR.NE.0) GO TO 995
            GO TO 999
C                                       I/O error
 995  WRITE (MSGTXT,1995) IERR
      IERR = 2
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('QMSGRY: SWITCH TO DOT-BY-DOT - TOO MUCH CORE NEEDED IN',
     *   ' QUICK MODES')
 1030 FORMAT ('^IGE^G^DF09001',A1,'0AIPS',I3.3)
 1031 FORMAT (',',A2,I3.3,'^M',I3.3,I3.3,'000000')
 1075 FORMAT ('^G^IGV^G^PW',I2.2)
 1610 FORMAT ('^P',I4.4)
 1650 FORMAT ('^G')
 1995 FORMAT ('QMSGRY: QMS WRITE ERROR',I5)
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
C                                       now done before this call
C         IF (NYDPP.EQ.NXDPP+1) NYDPP = NXDPP
C         IF (NXDPP.EQ.NYDPP+1) NXDPP = NYDPP
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
 1151 FORMAT ('      Scale, x-, y-shift  =',3F9.5,' gaussian')
 1152 FORMAT ('      Scale, x-, y-shift  =',3F9.5,' sin')
      END
      SUBROUTINE CNTPLT (LUN, FIND, IOPOS, IRRN, IX0, SCALEX, CSIZEX,
     *   ICHL, NXA, MAXX, BUFFER, IERR)
C-----------------------------------------------------------------------
C   CNTPLT reads a few records from the plot file to see if some
C   character string or other command will cause the plot to overflow
C   in the X direction (on the right).  It will correct IX0 if possible.
C   Inputs:
C      LUN     I         LUN of open plot file
C      FIND    I         FTAB pointer for plot file
C      IOPOS   I         Position in BUFFER of next command
C      SCALEX  R         X scaling factor
C      CSIZEX  I         Size of character in X
C      ICHL    I         Number chars to left of plot (dots)
C      NXA     I         Number dots in X plot expected
C      IRRN    I   !!    Record number now in BUFFER.
C      MAXX    I         Max X value allowed (1 is min)
C      BUFFER  I(256)    I/O buffer with plot file record IRRN
C   In/out:
C      IX0     I         X bias position
C   Output:
C      IERR    I         Error code from ZFIO
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IOPOS, IRRN, IX0, CSIZEX, MAXX, BUFFER(256),
     *   NXA, ICHL, IERR
      REAL      SCALEX
C
      REAL      DX
      INTEGER   LRRN, XMAX, XMIN, X, LOPOS, I, IX, NCHAR, IANGL, LX
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
         IX0 = IX0 - I - (XMIN/2-I-1)/2
         IX0 = MAX (2, IX0)
         X = X + IX0
         IF (X.LT.2) IX0 = IX0 + 2 - X
C
 999  RETURN
      END
