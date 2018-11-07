LOCAL INCLUDE 'TVPL.INC'
      REAL      SCALEX, SCALEY, RANGE(2,3)
      INTEGER   IMCHAN(3), IMCORN(2), GRCHAN(8), IXL, IYL, IX0, IY0,
     *   NGRAYS, GPHCON(32), LTYPE, GPHSCR(2560), THREEC(3)
      LOGICAL   DODARK
      COMMON /TVSPCL/ SCALEX, SCALEY, IMCHAN, IMCORN, GRCHAN, IXL, IYL,
     *   IX0, IY0, NGRAYS, GPHCON, LTYPE, GPHSCR, DODARK, RANGE, THREEC
LOCAL END
      PROGRAM TVPL
C-----------------------------------------------------------------------
C! Plots an AIPS plot (PL) file on a television device.
C# EXT-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002-2003, 2006, 2008-2009, 2015
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
C   TVPL is a program for the graphics package. The program runs as a
C   detached task initiated from AIPS.  First a cataloged file is found
C   using data passed from AIPS.  The list of associated files is
C   searched for a PLot file corresponding to the version number.  The
C   graphics commands in this file are executed for the TV display.
C   INPUTS:  (from AIPS)
C            INNAME  R(3)   Name of primary file.
C            INCLASS R(2)   Class of primary file.
C            INSEQ   R      Sequence number of primary file.
C            INDISK  R      Disk volume number. 0 means try all.
C            INVERS  R      Extension file version number. 0 means.
C                           use the highest version number.
C            GRCHAN  R      Graphics channel to use (1 - 3, 0 => 1)
C            TVCHAN  R      channel number to use for displaying gray
C                           scale commands in plot file
C            TVCORN  R(2)   lower left corner to use if graphic only.
C                           Forces pixel scaling.  If values not
C                           reasonable (i.e. 0) use self-scaling
C-----------------------------------------------------------------------
      CHARACTER  NAMIN*12, CLSIN*6, TYPIN*2, STATUS*4, NAME*6, GFILE*48
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL      SEQIN, DSKIN, VERSN, GRPHCH, GRAYCH, GRAYCR(2), XDODRK
      INTEGER   BUFFER(256), LUN, FIND, IERR, NPARMS, RETCOD, SLOT,
     *   IVER, ISEQ, IERR2, USER, CATERR, IVOL, IROUND, IWIN(4), I
      LOGICAL   NOMAP, QUICK, NOEXCL, WAIT, EQUAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'TVPL.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, GRPHCH,
     *   GRAYCH, GRAYCR, XDODRK
      DATA TYPIN /'  '/
      DATA NAME /'TVPL  '/
      DATA NOMAP, WAIT, NOEXCL /.FALSE., .TRUE., .FALSE./
      DATA LUN /26/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
C                                       Get input values from AIPS.
      NPARMS = 13
      CALL GTPARM (NAME, NPARMS, QUICK, XNAMIN, BUFFER, IERR)
      RETCOD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
      ELSE IF (NTVDEV.LE.0) THEN
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         RETCOD = 8
         END IF
      IF (QUICK) CALL RELPOP (RETCOD, BUFFER, IERR2)
      IF (RETCOD.NE.0) GO TO 999
      RETCOD = 8
C                                       Get map header.
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      USER = NLUSER
      IVER = VERSN
      DODARK = XDODRK.GT.0.0
C                                       Character inputs
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      SLOT = 1
      CALL CATDIR ('SRCH', IVOL, SLOT, NAMIN, CLSIN, ISEQ, TYPIN, USER,
     *   STATUS, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Read catalog header.
      CALL CATIO ('READ', IVOL, SLOT, CATBLK, 'REST', BUFFER, CATERR)
      IF ((CATERR.NE.0) .AND. (CATERR.NE.6)) THEN
         WRITE (MSGTXT,1045) CATERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                        Find plot file:
C                                        if PL & IVER=0, then main file
      EQUAL = TYPIN(:2).EQ.'PL'
      IF ((EQUAL) .AND. (CATERR.EQ.6)) THEN
         WRITE (MSGTXT,1045) CATERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
      IF ((EQUAL) .AND. (IVER.EQ.0)) IVER = CATBLK(KIIMS)
C                                        else take IVER as given or
C                                        as max version #
      IF (IVER.LE.0) THEN
         CALL FNDEXT ('PL', CATBLK, IVER)
C                                       PLot file not found.
         IF (IVER.LE.0) THEN
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            GO TO 995
            END IF
         END IF
C                                       Open graphics file
      CALL ZPHFIL ('PL', IVOL, SLOT, IVER, GFILE, IERR)
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, NOMAP, NOEXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070) IVER
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Open TV device.
      CALL TVOPEN (BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                        init image catalog
      GRCHAN(1) = IROUND (GRPHCH)
      IF ((GRCHAN(1).LT.1) .OR. (GRCHAN(1).GT.NGRAPH)) THEN
         DO 95 I = 1,8
            GRCHAN(I) = I + NGRAY
 95         CONTINUE
      ELSE
         I = GRCHAN(1) + NGRAY
         CALL FILL (7, I, GRCHAN)
         END IF
      GRCHAN(8) = 8 + NGRAY
      CATBLK(IIVOL) = IVOL
      CATBLK(IICNO) = SLOT
C                                        init common
      IMCHAN(1) = IROUND (GRAYCH)
      IMCORN(1) = IROUND (GRAYCR(1))
      IMCORN(2) = IROUND (GRAYCR(2))
      NGRAYS = 0
      CALL FILL (32, 0, GPHCON)
C                                       Write to TV.
      CALL TVDRAW (LUN, FIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090)
         CALL MSGWRT (8)
         GO TO 990
         END IF
      RETCOD = 0
C                                        update image catalog
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = MAXXTV(1)
      IWIN(4) = MAXXTV(2)
      CALL YCWRIT (GRCHAN, IWIN, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         END IF
C                                       Close graph file.
 990  CALL ZCLOSE (LUN, FIND, IERR)
      CALL TVCLOS (BUFFER, IERR)
C
 995  CALL DIETSK (RETCOD, QUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1010 FORMAT ('TV DEVICES ALL IN USE')
 1020 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1045 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
 1060 FORMAT ('PLOT FILE NOT IN CATALOG')
 1070 FORMAT ('PLOT FILE ',I4,' NOT FOUND')
 1080 FORMAT ('UNABLE TO OPEN TV, IERR =',I7)
 1090 FORMAT ('ERROR WRITING TO IIS DEVICE')
 1100 FORMAT ('ERROR UPDATING IMAGE CATALOG # = ',I6)
      END
      SUBROUTINE TVDRAW (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the TV.
C   Inputs:
C      LUN     I        logical unit number of an open graph file.
C      FIND    I        pointer to the FTAB info of the graph file.
C   Output:
C      IERR    I        error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      CHARACTER LINE*132, GPHFUN*2
      REAL      DX, DY, SCALEF, X, XYRATO, Y, GROFF, GRSCAL, RX1, RX2,
     *   RY1, RY2, AX, AY, ROBUF(256)
      INTEGER   GRYERR, IANGL, IGRLO, IGRHI, NPIX, NPTSX, NPTSY, INO,
     *   ICHB, ICHL, ICHR, ICHT, INCRRN, IOPOS, IX, IX1, IY, IY1, NXA,
     *   NYA, OPCODE, IMAWIN(4), IERRC, IERRLM, IX2, IY2, N, NCHAR,
     *   IORRN, LX, LY, IERRV, IERRG, IERRO, PLX(2), PLY(2), CMAXTV(2),
     *   TVWIND(4), LCHAR, LPIX, GPHCNT, IER, IOBUF(256), ICH,
     *   GSCR(2560), BSCR(2560)
      LOGICAL   DOGRAY, DO3COL, ERR3C
      HOLLERITH HOBUF(256)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'TVPL.INC'
      EQUIVALENCE (IOBUF, HOBUF, ROBUF)
C-----------------------------------------------------------------------
C                                       Initialize all values.
      IERR = 0
      ERR3C = .FALSE.
      IERRV = 0
      IERRG = 0
      IERRC = 0
      IERRO = 0
      IERRLM = 20
      GRYERR = 0
      IOPOS = 9999
      IGRLO = 0
      IGRHI = 0
      PLX(1) = 0
      PLX(2) = 0
      PLY(1) = 0
      PLY(2) = 0
      GPHCNT = 0
      LTYPE = 1
      CALL YHOLD ('ONNN', IERR)
C                                        Check inputs
      IMCHAN(1) = MAX (1, MIN (IMCHAN(1), NGRAY))
      IMCHAN(2) = MIN (IMCHAN(1)+1, NGRAY)
      IMCHAN(3) = MIN (IMCHAN(2)+1, NGRAY)
      IF ((IMCORN(1).LT.1) .OR. (IMCORN(1).GT.MAXXTV(1))) IMCORN(1) = 0
      IF ((IMCORN(2).LT.1) .OR. (IMCORN(2).GT.MAXXTV(2))) IMCORN(2) = 0
      DOGRAY = .FALSE.
      DO3COL = .FALSE.
C                                       determine where plot starts
      CALL ZFIO ('READ', LUN, FIND, 1, IOBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IORRN = (IOBUF(10) + 9) / 256 + 1
C                                       next plot command
 50   IF (IOPOS.LE.256) GO TO 60
C                                       Read record.
 55      IORRN = IORRN + 1
         CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IOPOS = 1
C                                       Transfer based on opcode.
 60   OPCODE = IOBUF(IOPOS)
      IF (OPCODE.EQ.0) GO TO 55
      IF (OPCODE.EQ.32767) GO TO 65
      IF ((OPCODE.LT.1) .OR. (OPCODE.GT.18)) GO TO 970
      GO TO (100, 200, 300, 400, 500, 600, 700, 800, 150, 500, 700, 300,
     *   300, 600, 600, 160, 500, 810), OPCODE
C                                       End of file.
C                                       Update image catalog if gray
 65      DO 70 ICH = 1,32
            IF (GPHCON(ICH).GT.0) THEN
               IF (ICH.LE.NGRAY) THEN
                  IMAWIN(1) = IX0
                  IMAWIN(2) = IY0
                  IMAWIN(3) = IX0 + NPTSX - 1
                  IMAWIN(4) = IY0 + NPTSY - 1
                  IF (DO3COL) THEN
                     IF (ICH.EQ.IMCHAN(2)) CALL RCOPY (2, RANGE(1,2),
     *                  CATR(IRRAN))
                     IF (ICH.EQ.IMCHAN(3)) CALL RCOPY (2, RANGE(1,3),
     *                  CATR(IRRAN))
                     END IF
               ELSE
                  IMAWIN(1) = 1
                  IMAWIN(2) = 1
                  IMAWIN(3) = MAXXTV(1)
                  IMAWIN(4) = MAXXTV(2)
                  END IF
               CALL YCWRIT (ICH, IMAWIN, CATBLK, GPHSCR, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1060) IERR,ICH
                  CALL MSGWRT (6)
                  END IF
               END IF
 70         CONTINUE
         GO TO 990
C                                       Opcode 1, initialize.(NOOP)
 100  CATBLK(IIPLT) = IOBUF(IOPOS+5)
      IOPOS = IOPOS + 6
      GO TO 50
C                                       Opcode 9, line type
 150  LTYPE = IOBUF(IOPOS+1)
      IOPOS = IOPOS + 2
      GO TO 50
C                                       Opcode 16, 3-color vector color
 160  THREEC(1) = ROBUF(IOPOS+1) * MAXINT + 0.5
      THREEC(2) = ROBUF(IOPOS+2) * MAXINT + 0.5
      THREEC(3) = ROBUF(IOPOS+3) * MAXINT + 0.5
      IOPOS = IOPOS + 4
      GO TO 50
C                                       Opcode 2, line drawing init.
 200  XYRATO = IOBUF(IOPOS+1) / 100.0
      SCALEF = IOBUF(IOPOS+2)
      IX1    = IOBUF(IOPOS+3)
      IY1    = IOBUF(IOPOS+4)
      IX2    = IOBUF(IOPOS+5)
      IY2    = IOBUF(IOPOS+6)
      RX1    = IOBUF(IOPOS+7) / 1000.0
      RY1    = IOBUF(IOPOS+8) / 1000.0
      RX2    = IOBUF(IOPOS+9) / 1000.0
      RY2    = IOBUF(IOPOS+10) / 1000.0
      ICHL   = (IOBUF(IOPOS+11)/10.0) * CSIZTV(1) + 0.9
      ICHB   = (IOBUF(IOPOS+12)/10.0) * CSIZTV(2) + 0.9
      ICHR   = (IOBUF(IOPOS+13)/10.0) * CSIZTV(1) + 0.9
      ICHT   = (IOBUF(IOPOS+14)/10.0) * CSIZTV(2)+ 0.9
      IOPOS = IOPOS + 20
      CALL COPY (5, IOBUF(IOPOS-5), CATBLK(IIDEP))
C                                       Find no. pixels inside border.
      CALL YWINDO ('READ', TVWIND, IERR)
      IF ((IERR.NE.0) .OR. ((IMCORN(1).NE.0) .AND. (IMCORN(2).NE.0)))
     *   THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      CMAXTV(1) = TVWIND(3) - TVWIND(1) + 1
      CMAXTV(2) = TVWIND(4) - TVWIND(2) + 1
      NXA = CMAXTV(1) - ICHL - ICHR - 1
      IF (NXA.LE.0) THEN
         TVWIND(1) = 1
         TVWIND(3) = MAXXTV(1)
         CMAXTV(1) = MAXXTV(1)
         NXA = MAXXTV(1) - ICHL - ICHR - 1
         END IF
      NYA = CMAXTV(2) - ICHT - ICHB - 1
      IF (NYA.LE.0) THEN
         TVWIND(2) = 1
         TVWIND(4) = MAXXTV(2)
         CMAXTV(2) = MAXXTV(2)
         NYA = MAXXTV(2) - ICHT - ICHB - 1
         END IF
      IF ((NXA.LE.0) .OR. (NYA.LE.0)) THEN
         WRITE (MSGTXT,1200) NXA, NYA
         CALL MSGWRT (7)
         GO TO 990
         END IF
C                                        self scale
      AX = ABS(IX2 - IX1) * XYRATO
      AY = ABS(IY2 - IY1)
      IF ((IMCORN(1).EQ.0) .OR. (IMCORN(2).EQ.0)) THEN
         X = ABS(IX2+RX2-IX1-RX1) * XYRATO
         Y = ABS(IY2+RY2-IY1-RY1)
         IF ((X.GT.0) .AND. (Y.GT.0)) GO TO 220
            WRITE (MSGTXT,1210) X, Y
            CALL MSGWRT (7)
            GO TO 990
C
 220     IF ((X/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
            SCALEY = NYA/SCALEF * AY/Y
            SCALEX = SCALEY * AX/AY
         ELSE
            SCALEX = NXA/SCALEF * AX/X
            SCALEY = SCALEX * AY/AX
            END IF
C                                       Center
         NXA = SCALEX * SCALEF * X/AX + ICHL + ICHR
         IF (NXA.GT.CMAXTV(1)) THEN
            TVWIND(1) = 1
            TVWIND(3) = MAXXTV(1)
            CMAXTV(1) = MAXXTV(1)
            END IF
         IX0 = TVWIND(1) + ICHL + MAX (0, CMAXTV(1)-NXA)/2 + 0.9 -
     *      RX1 * XYRATO * SCALEX * SCALEF / AX
         NYA = SCALEY * SCALEF * Y/AY + ICHB + ICHT
         IF (NYA.GT.CMAXTV(2)) THEN
            TVWIND(2) = 1
            TVWIND(4) = MAXXTV(2)
            CMAXTV(2) = MAXXTV(2)
            END IF
         IY0 = TVWIND(2) + ICHB + MAX (0, CMAXTV(2)-NYA)/2 + 0.9 -
     *      RY1 * SCALEY * SCALEF / AY
C                                        pixel scaling
      ELSE
         IX0 = IMCORN(1)
         IY0 = IMCORN(2)
         SCALEX = (IX2-IX1) / SCALEF
         SCALEY = (IY2-IY1) / SCALEF
         END IF
      GO TO 350
C                                       Opcode 3, grey scale init.
 300  IF (IMCHAN(1).GT.0) GO TO 310
C                                        ignore gray scales
 305     WRITE (MSGTXT,1300)
         CALL MSGWRT (2)
         IOPOS = IOPOS + 5
         IF (OPCODE.EQ.12) IOPOS = IOPOS + 7
         IF (OPCODE.EQ.13) IOPOS = IOPOS + 3
         GO TO 50
C                                        Use gray scale: pixel scaling
 310  NPTSX = IOBUF(IOPOS+3)
      NPTSY = IOBUF(IOPOS+4)
      IGRLO = IOBUF(IOPOS+1)
      IGRHI = IOBUF(IOPOS+2)
      IF (OPCODE.EQ.13) THEN
         CALL H2CHR (2, 1, IOBUF(IOPOS+5), GPHFUN)
         CALL RCOPY (2, ROBUF(IOPOS+6), RANGE)
         IOPOS = IOPOS + 8
      ELSE IF (OPCODE.EQ.3) THEN
         GPHFUN = 'LN'
         RANGE(1,1) = IGRLO
         RANGE(2,1) = IGRHI
         IOPOS = IOPOS + 5
      ELSE
         CALL H2CHR (2, 1, IOBUF(IOPOS+5), GPHFUN)
         CALL RCOPY (6, ROBUF(IOPOS+6), RANGE)
         IOPOS = IOPOS + 12
         END IF
      IF (IGRHI.LE.IGRLO) GO TO 305
      IF ((NPTSX.LE.0) .OR. (NPTSY.LE.0)) GO TO 305
      IF ((NPTSX.GT.MAXXTV(1)) .OR. (NPTSY.GT.MAXXTV(2))) THEN
         WRITE (MSGTXT,1310) NPTSX, NPTSY
         CALL MSGWRT (6)
         END IF
C                                       set origin
      IX0 = IMCORN(1)
      IY0 = IMCORN(2)
C                                        center
      IF ((IX0.LE.0) .OR. (IY0.LE.0)) THEN
         NXA = IX2 - IX1 + ICHL + ICHR + RX2 - RX1
         NYA = IY2 - IY1 + ICHB + ICHT + RY2 - RY1
         IF (NXA.GT.CMAXTV(1)) THEN
            TVWIND(1) = 1
            TVWIND(3) = MAXXTV(1)
            CMAXTV(1) = MAXXTV(1)
            END IF
         IF (NXA.GT.MAXXTV(1)) THEN
            IX0 = (MAXXTV(1) - NPTSX - RX2 + RX1) / 2 + 1
         ELSE
            IX0 = ICHL + (CMAXTV(1) - NXA) / 2 + TVWIND(1)
            END IF
         IF (NYA.GT.CMAXTV(2)) THEN
            TVWIND(2) = 1
            TVWIND(4) = MAXXTV(2)
            CMAXTV(2) = MAXXTV(2)
            END IF
         IF (NYA.GT.MAXXTV(2)) THEN
            IY0 = (MAXXTV(2) - NPTSY - RY2 + RY1) / 2 + 1
         ELSE
            IY0 = ICHB + (CMAXTV(2) - NYA) / 2 + TVWIND(2)
            END IF
         END IF
      SCALEX = (IX2-IX1) / SCALEF
      SCALEY = (IY2-IY1) / SCALEF
      GROFF = 0.0
      GRSCAL = 1.0
      IF ((IGRLO.LT.0) .OR. (IGRHI.GT.MAXINT)) THEN
         GROFF = IGRLO
         GRSCAL = REAL (MAXINT) / (REAL (IGRHI) - REAL (IGRLO))
         END IF
      CALL CNTVPL (LUN, FIND, IOPOS, IORRN, IX0, SCALEX, CSIZTV(1),
     *   ICHL, NXA, TVWIND, MAXXTV(1), IOBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (OPCODE.EQ.3) DOGRAY = .TRUE.
      IF (OPCODE.EQ.13) DOGRAY = .TRUE.
      IF (OPCODE.EQ.12) THEN
         DO3COL = .TRUE.
         IF ((IMCHAN(1).GT.NGRAY-2) .AND. (NGRAY.GE.3)) THEN
            IMCHAN(1) = NGRAY - 2
            IMCHAN(2) = NGRAY - 1
            IMCHAN(3) = NGRAY
            END IF
         END IF
C                                        update catalog header for img
 350  CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = IX0
      CATBLK(IICOR+1) = IY0
      CATBLK(IICOR+2) = IX0 + SCALEX*SCALEF
      CATBLK(IICOR+3) = IY0 + SCALEY*SCALEF
      CALL CHR2H (2, GPHFUN, 1, CATH(IITRA))
      CATR(IRRAN) = RANGE(1,1)
      CATR(IRRAN+1) = RANGE(2,1)
      GO TO 50
C                                       Opcode 4, position cursor.
 400  IX = IOBUF(IOPOS+1)
      IY = IOBUF(IOPOS+2)
      IOPOS = IOPOS + 3
      CALL TVVEC (IX, IY, 1, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
      IERRV = IERRV + 1
      GO TO 50
C                                       Opcode 5, draw vector.
C                                       Opcode 10, draw dark vector.
C                                       Opcode 17, draw colored vector
 500  IX = IOBUF(IOPOS+1)
      IY = IOBUF(IOPOS+2)
      IOPOS = IOPOS + 3
      ICH = 2
      IF ((OPCODE.EQ.10) .AND. (DODARK)) ICH = 3
      IF (OPCODE.EQ.17) THEN
         IF (TVIMPC.LE.0) THEN
            MSGTXT = 'TV IS NOT CAPABLE OF 3-COLOR VECTORS'
            IF (.NOT.ERR3C) CALL MSGWRT (7)
            ERR3C = .TRUE.
         ELSE
            ICH = 4
            END IF
         END IF
      CALL TVVEC (IX, IY, ICH, IERR)
      IF (IERR.EQ.0) THEN
         GPHCNT = GPHCNT + 1
         IF (GPHCNT.GT.5000) THEN
            CALL YHOLD ('OFFF', IER)
            CALL YHOLD ('ONNN', IER)
            GPHCNT = 0
            END IF
      ELSE IF (IERR.NE.2) THEN
         GO TO 960
      ELSE
         IERRV = IERRV + 1
         END IF
      GO TO 50
C                                       Opcode 6, write characters.
 600  NCHAR = IOBUF(IOPOS+1)
      IANGL = IOBUF(IOPOS+2)
C                                       NOTE: plot file angle 1 => 3
      IF (IANGL.EQ.1) IANGL = 3
      DX    = IOBUF(IOPOS+3)/100.0
      DY    = IOBUF(IOPOS+4)/100.0
      IX = IXL + DX * CSIZTV(1)
      IY = IYL + DY * CSIZTV(2)
      CALL H2CHR (NCHAR, 1, HOBUF(IOPOS+5), LINE)
      IOPOS = IOPOS + 5 + ((NCHAR-1) / 4) + 1
      LCHAR = 1
      IF (IANGL.EQ.0) THEN
         LY = IY
         LX = IX + NCHAR * CSIZTV(1) - 1
         IF ((IY.LE.0) .OR. (IY+CSIZTV(2)-1.GT.MAXXTV(2))) THEN
            IERRC = IERRC + 1
            GO TO 50
            END IF
         IF ((PLY(2).EQ.LY) .AND. (PLX(2).GT.IX) .AND. (PLX(1).LE.LX))
     *      THEN
            IERRO = IERRO + 1
            GO TO 50
            END IF
         IF (LX.GT.MAXXTV(1)) THEN
            IERRC = IERRC + 1
            NCHAR = (MAXXTV(1) + 1 - IX) / CSIZTV(1)
            IF (NCHAR.LE.0) GO TO 50
            END IF
         IF (IX.LT.1) THEN
            IERRC = IERRC + 1
            LCHAR = (-IX + CSIZTV(1) - 1) / CSIZTV(1) + 1
            IX = IX + (LCHAR - 1) * CSIZTV(1)
            END IF
         PLY(1) = LY + CSIZTV(2) - 1
         PLY(2) = LY
         PLX(1) = IX
         PLX(2) = IX + (NCHAR-LCHAR+1) * CSIZTV(1) - 1
      ELSE
         LX = IX
         LY = IY - (NCHAR-1) * CSIZTV(2)
         IF ((IX.LE.0) .OR. (IX+CSIZTV(1)-1.GT.MAXXTV(1))) THEN
            IERRC = IERRC + 1
            GO TO 50
            END IF
         IF ((PLX(1).EQ.LX) .AND. (PLY(2).LT.IY) .AND. (PLY(1).GE.LY))
     *      THEN
            IERRO = IERRO + 1
            GO TO 50
            END IF
         IF (LY.LT.1) THEN
            IERRC = IERRC + 1
            NCHAR = (IY - 1) / CSIZTV(2) + 1
            IF (NCHAR.LE.0) GO TO 50
            END IF
         IF (IY.GT.MAXXTV(2)) THEN
            IERRC = IERRC + 1
            LCHAR = (IY - MAXXTV(2) + CSIZTV(2) -1) / CSIZTV(2) + 1
            IY = IY - (LCHAR-1) * CSIZTV(2)
            END IF
         PLY(1) = IY + CSIZTV(2) - 1
         PLY(2) = IY - (NCHAR-LCHAR) * CSIZTV(2)
         PLX(1) = IX
         PLX(2) = IX + CSIZTV(1) - 1
         END IF
      ICH = GRCHAN(LTYPE)
      IF ((OPCODE.EQ.15) .AND. (DODARK)) ICH = GRCHAN(8)
      CALL TVPINI (ICH, 0, IERR)
      IF (IERR.EQ.0) CALL IMCHAR (ICH, IX, IY, IANGL, 0,
     *   LINE(LCHAR:NCHAR), GPHSCR, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 960
      GO TO 50
C                                       Opcode 7, write grey scale.
 700  IF (((OPCODE.EQ.7) .AND. (.NOT.DOGRAY)) .OR. ((OPCODE.EQ.11) .AND.
     *   (.NOT.DO3COL))) THEN
C                                        ignore gray scale
         GRYERR = GRYERR + 1
C                                       Update IO position. Read
C                                       proper RRN if necessary.
         IF (OPCODE.EQ.7) THEN
            IOPOS = IOPOS + 3 + IOBUF(IOPOS+1)
         ELSE
            IOPOS = IOPOS + 3 + 3*IOBUF(IOPOS+1)
            END IF
         IF (IOPOS.LE.256) GO TO 60
            INCRRN = (IOPOS-1)/256
            IORRN = IORRN + INCRRN
            IOPOS = IOPOS - 256*INCRRN
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
            IF (IERR.EQ.0) GO TO 60
            GO TO 980
         END IF
C                                        get gray values from record(s)
      NPIX = IOBUF(IOPOS+1)
      IANGL = IOBUF(IOPOS+2)
C                                       NOTE: plot file angle 1 => 3
      IF (IANGL.EQ.1) IANGL = 3
      IOPOS = IOPOS + 3
      IF (NPIX.LE.0) GO TO 50
      NGRAYS = NGRAYS + 1
C                                        update catalog, screen
      IF (OPCODE.EQ.7) THEN
         CALL TVPINI (IMCHAN(1), 0, IERR)
      ELSE
         CALL TVPINI (IMCHAN(1), 1, IERR)
         CALL TVPINI (IMCHAN(2), 2, IERR)
         CALL TVPINI (IMCHAN(3), 3, IERR)
         END IF
      LPIX = 1
      DO 725 N = 1,NPIX
         IF (IOPOS.GT.256) THEN
            IORRN = IORRN + 1
            CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            IOPOS = 1
            END IF
         GPHSCR(N) = (IOBUF(IOPOS) - GROFF) * GRSCAL + 0.5
         IOPOS = IOPOS + 1
         IF (OPCODE.EQ.11) THEN
            IF (IOPOS.GT.256) THEN
               IORRN = IORRN + 1
               CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               IOPOS = 1
               END IF
            GSCR(N) = (IOBUF(IOPOS) - GROFF) * GRSCAL + 0.5
            IOPOS = IOPOS + 1
            IF (IOPOS.GT.256) THEN
               IORRN = IORRN + 1
               CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
               IF (IERR.NE.0) GO TO 980
               IOPOS = 1
               END IF
            BSCR(N) = (IOBUF(IOPOS) - GROFF) * GRSCAL + 0.5
            IOPOS = IOPOS + 1
            END IF
 725     CONTINUE
      IF (IANGL.EQ.0) THEN
         IF ((IYL.LT.1) .OR. (IYL.GT.MAXXTV(2))) THEN
            GRYERR = GRYERR + 1
            GO TO 50
            END IF
         IF (IXL+NPIX-1.GT.MAXXTV(1)) THEN
            IERRG = IERRG + 1
            NPIX = MAXXTV(1) + 1 - IXL
            END IF
         IF (IXL.LE.0) THEN
            LPIX = 2 - IXL
            NPIX = NPIX - LPIX + 1
            IERRG = IERRG + 1
            END IF
      ELSE IF (IANGL.EQ.3) THEN
         IF ((IXL.LT.1) .OR. (IXL.GT.MAXXTV(1))) THEN
            GRYERR = GRYERR + 1
            GO TO 50
            END IF
         IF (IYL-NPIX+1.LT.1) THEN
            IERRG = IERRG + 1
            NPIX = IYL
            END IF
         IF (IYL.GT.MAXXTV(2)) THEN
            LPIX = IYL - MAXXTV(2) + 1
            NPIX = NPIX - LPIX + 1
            IERRG = IERRG + 1
            END IF
         END IF
C                                        send to TV
      CALL YIMGIO ('WRIT', IMCHAN(1), IXL, IYL, IANGL, NPIX,
     *   GPHSCR(LPIX), IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 960
      IF (OPCODE.EQ.11) THEN
         CALL YIMGIO ('WRIT', IMCHAN(2), IXL, IYL, IANGL, NPIX,
     *      GSCR(LPIX), IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 960
         CALL YIMGIO ('WRIT', IMCHAN(3), IXL, IYL, IANGL, NPIX,
     *      BSCR(LPIX), IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 960
         END IF
      GPHCNT = GPHCNT + 6
      IF (GPHCNT.GT.5000) THEN
         CALL YHOLD ('OFFF', IER)
         CALL YHOLD ('ONNN', IER)
         GPHCNT = 0
         END IF
      GO TO 50
C                                       Opcode 8, put misc info in
C                                       image catalog.
 800  INO = IOBUF(IOPOS+1)
      IOPOS = IOPOS + 2
      CALL COPY (INO, IOBUF(IOPOS), CATBLK(IIOTH))
      IOPOS = IOPOS + INO
      GO TO 50
C                                       Opcode 18, comment
 810  INO = IOBUF(IOPOS+1)
      IOPOS = IOPOS + (INO+3)/4 + 2
      GO TO 50
C                                       Write error.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      GO TO 990
C                                       Invalid opcode.
 970  WRITE (MSGTXT,1970) OPCODE
      CALL MSGWRT (8)
      IERR = 1
      GO TO 990
C                                       Disk error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close files.
 990  IF (GRYERR.GT.0) THEN
         WRITE (MSGTXT,1990) GRYERR
         CALL MSGWRT (3)
         END IF
      IF (IERRV.GT.0) THEN
         WRITE (MSGTXT,1991) IERRV
         CALL MSGWRT (3)
         END IF
      IF (IERRC.GT.0) THEN
         WRITE (MSGTXT,1992) IERRC
         CALL MSGWRT (3)
         END IF
      IF (IERRG.GT.0) THEN
         WRITE (MSGTXT,1993) IERRG
         CALL MSGWRT (3)
         END IF
      IF (IERRO.GT.0) THEN
         WRITE (MSGTXT,1994) IERRO
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('IMAGE CATALOG UPDATE ERROR #',I6,' CHAN',I4)
 1200 FORMAT ('TVDRAW: ERROR FROM GINITL ARGS. NXA, NYA=',2I6)
 1210 FORMAT ('TVDRAW: ERROR FROM GINITL ARGS. X,Y=',2E10.3)
 1300 FORMAT ('TVDRAW: GREY SCALE INIT OPCODE IGNORED')
 1310 FORMAT ('TVDRAW: ARRAY DIMENSIONS',2I7,' TOO LARGE FOR TV')
 1960 FORMAT ('TVDRAW: TV WRITE ERROR LIMIT EXCEEDED')
 1970 FORMAT ('TVDRAW: INVALID OPCODE',I5,' IN GRAPH FILE')
 1980 FORMAT ('TVDRAW: DISK IO RELATED ERROR',I3)
 1990 FORMAT (I8,' grey scale opcodes ignored')
 1991 FORMAT (I8,' vectors truncated at edges')
 1992 FORMAT (I8,' character strings truncated at edges')
 1993 FORMAT (I8,' grey scale lines truncated at edges')
 1994 FORMAT (I8,' character strings omitted due to overlaps')
      END
      SUBROUTINE TVVEC (IX, IY, IN, IERR)
C-----------------------------------------------------------------------
C   TVVEC scales coordinates, writes bright vectors on screen, and
C   saves latest coordinate values
C   Inputs:
C      IX      I      X position
C      IY      I      Y position
C      IN      I      1 => scale IX,IY but no vector
C                     2 => scale IX,IY write vector
C                     3 => scale IX,IY write dark vector
C                     4 => scale IX,IY write 3-color vector
C   Output:
C      IERR    I      error code of Z...XF
C-----------------------------------------------------------------------
      INTEGER   IX, IY, IN, IERR
C
      REAL      ALPHA, BETA
      INTEGER   BUFFX(2), BUFFY(2), IXN, IYN, X1, X2, Y1, Y2, IC, I
      LOGICAL   ISERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'TVPL.INC'
C-----------------------------------------------------------------------
C                                        scale
      IERR = 0
      ISERR = .FALSE.
      IXN = IX * SCALEX  +  IX0 + 0.5
      IYN = IY * SCALEY  +  IY0 + 0.5
C                                        Actually write a vector
      IF (IN.GT.1) THEN
         X1 = MAX (1, MIN (MAXXTV(1), IXL))
         Y1 = MAX (1, MIN (MAXXTV(2), IYL))
         X2 = MAX (1, MIN (MAXXTV(1), IXN))
         Y2 = MAX (1, MIN (MAXXTV(2), IYN))
         IF ((X1.NE.IXL) .OR. (Y1.NE.IYL)) THEN
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X1-IXN)/REAL(IXL-IXN)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y1-IYN)/REAL(IYL-IYN)
            ALPHA = MIN (ALPHA, BETA)
            X1 = IXN + ALPHA * (IXL-IXN)
            Y1 = IYN + ALPHA * (IYL-IYN)
            ISERR = .TRUE.
            END IF
         IF ((X2.NE.IXN) .OR. (Y2.NE.IYN)) THEN
            ALPHA = 1.0
            IF (IXN.NE.IXL) ALPHA = REAL(X2-IXL)/REAL(IXN-IXL)
            BETA = 1.0
            IF (IYN.NE.IYL) BETA = REAL(Y2-IYL)/REAL(IYN-IYL)
            ALPHA = MIN (ALPHA, BETA)
            X2 = IXL + ALPHA * (IXN-IXL)
            Y2 = IYL + ALPHA * (IYN-IYL)
            ISERR = .TRUE.
            END IF
C                                        write bright vectors
         BUFFX(1) = X1
         BUFFY(1) = Y1
         BUFFX(2) = X2
         BUFFY(2) = Y2
         IF (IN.NE.4) THEN
            IC = GRCHAN(LTYPE)
            IF (IN.EQ.3) IC = GRCHAN(8)
            CALL TVPINI (IC, 0, IERR)
            IF (IERR.EQ.0) CALL IMVECT ('ONNN', IC, 2, BUFFX, BUFFY,
     *         GPHSCR, IERR)
C                                       3-color vetor
         ELSE
            DO 40 I = 1,3
               IC = IMCHAN(I)
               CALL TVPINI (IC, I, IERR)
               IF (IERR.NE.0) GO TO 999
               CALL TVVECT (THREEC(I), IC, 2, BUFFX, BUFFY, GPHSCR,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
 40            CONTINUE
            END IF
         END IF
C                                        set last position
      IXL = IXN
      IYL = IYN
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE TVPINI (CHN, COL, IERR)
C-----------------------------------------------------------------------
C   Inits and displays channel, inits image catalog
C   Inputs:
C      CHN    I   Channel number 1 - ngray+NGRAPH
C      COL    I   Color: 0 all, 1 r, 2 G, 3 B
C   Output
C      IERR   I   Error return
C-----------------------------------------------------------------------
      INTEGER   CHN, COL, IERR
C
      INTEGER   LC
      INCLUDE 'TVPL.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF (GPHCON(CHN).LE.0) THEN
         LC = COL
         IF (CHN.GT.NGRAY) LC = 0
         CALL YCINIT (CHN, GPHSCR)
         CALL YZERO (CHN, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (LC.NE.0) THEN
            CALL YSLECT ('OFFF', CHN, 0, GPHSCR, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         CALL YSLECT ('ONNN', CHN, LC, GPHSCR, IERR)
         GPHCON(CHN) = 1
         END IF
C
 999  RETURN
      END
      SUBROUTINE CNTVPL (LUN, FIND, IOPOS, IRRN, IX0, SCALEX, CSIZEX,
     *   ICHL, NXA, TVWIND, MAXX, BUFFER, IERR)
C-----------------------------------------------------------------------
C   CNTVPL reads a few records from the plot file to see if some
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
C      IRRN    I         Record number now in BUFFER.
C      MAXX    I         Max X value allowed (1 is min)
C      BUFFER  I(256)    I/O buffer with plot file record IRRN
C   In/out:
C      IX0     I         X bias position
C      TVWIND   I(4)      TV window desired
C   Output:
C      IERR    I         Error code from ZFIO
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IOPOS, IRRN, IX0, CSIZEX, MAXX, BUFFER(256),
     *   NXA, ICHL, IERR, TVWIND(4)
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
      IF ((I.LT.0) .OR. (I.GT.17)) GO TO 900
      GO TO (100, 200, 300, 400, 500, 600, 700, 800, 150, 500, 700, 300,
     *   300, 600, 600, 160, 500), I
C                                       Init
 100  LOPOS = LOPOS + 6
      GO TO 50
C                                       Line type
 150  LOPOS = LOPOS + 2
      GO TO 50
C                                       Opcode 16 3-color vector
 160  LOPOS = LOPOS + 4
      GO TO 50
C                                       Init line drawing
 200  LOPOS = LOPOS + 20
      GO TO 50
C                                       Init grey scale
 300  LOPOS = LOPOS + 5
      IOPOS = IOPOS + 5
      IF (I.EQ.12) IOPOS = IOPOS + 7
      IF (I.EQ.13) IOPOS = IOPOS + 3
      GO TO 50
C                                       Vector position
 400  CONTINUE
C                                       Opcode 5, draw vector.
C                                       Opcode 10, draw dark vector.
C                                       Opcode 17, draw colored vector
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
 900  IF (LRRN.NE.IRRN) THEN
         LRRN = IRRN
         CALL ZFIO ('READ', LUN, FIND, LRRN, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Reset window
      IF (XMAX-XMIN.GT.TVWIND(3)-TVWIND(1)) THEN
         TVWIND(1) = 1
         TVWIND(3) = MAXX
         END IF
C                                       Reset origin
      IF ((XMAX.GT.TVWIND(3)) .OR. (XMIN.LT.TVWIND(1))) THEN
         X = XMIN - IX0
         IX0 = IX0 + (TVWIND(3) + TVWIND(1) - XMAX - XMIN) / 2
         IX0 = MAX (2, IX0)
         X = X + IX0
         IF (X.LT.2) IX0 = IX0 + 2 - X
         END IF
C
 999  RETURN
      END
