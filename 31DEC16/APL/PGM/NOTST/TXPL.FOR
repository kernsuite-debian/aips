LOCAL INCLUDE 'TXPL.INC'
C                                       Local include for TXPL
      CHARACTER NAMIN*12, CLSIN*6, LPNAME*48
      HOLLERITH XNAMIN(3), XCLSIN(2), XLNAME(12)
      REAL      DSKIN, SEQIN, VERSN, DOCRT
C                                          common for screen parms
      CHARACTER SCREEN(60)*132
      INTEGER   BITSCR(400,200), IDOCRT, LASTX, LASTY, MINX, MAXX,
     *   MINY, MAXY, CHARSI, MAXC
      LOGICAL   DOPRNT
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, DOCRT, XLNAME
      COMMON /CHRCOM/ NAMIN, CLSIN, LPNAME
      COMMON /SCRN/ BITSCR, CHARSI, LASTX, LASTY, MINX, MAXX, MINY,
     *   MAXY, MAXC, IDOCRT, DOPRNT
      COMMON /SCRCHR/ SCREEN
LOCAL END
      PROGRAM TXPL
C-----------------------------------------------------------------------
C! Displays a plot (PL) file on a terminal or line printer
C# Plot-appl Terminal Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1999, 2002-2004, 2006-2007, 2009
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
C   TXPL is a program for the graphics package.  First a cataloged
C   file is found using data passed from AIPS.  The list of
C   associated files is searched for a PLot file corresponding
C   to the version number.  The graphics commands in this file
C   are executed for a dump line printing CRT or Lineprinter.
C   INPUTS:  (from AIPS)
C      INNAME  R(3)   Name of primary file.
C      INCLASS R(2)   Class of primary file.
C      INSEQ   R      Sequence number of primary file.
C      INDISK  R      Disk volume number. 0 means try all.
C      INTYPE  R      Primary file type.
C      INVERS  R      Extension file version number. 0 means.
C                     use the highest version number.
C      DOCRT   R      > 0 means CRT, else LINEPRINTER
C                        2 = write to history file
C                        3 = msgfil, no wait
C   Glen Langston MPIFR and MIT, November 1987
C-----------------------------------------------------------------------
      CHARACTER TYPIN*2, STATUS*4, PRGM*6, GFILE*48
      INTEGER   BUFFER(256), CATERR, LUN, FIND, IERR, NPARMS, RETCOD,
     *   CNO, IVER, I, IVOL, ISEQ, IERR2, USER, IROUND
      LOGICAL   NOMAP, QUICK, NOEXCL, WAIT
      INCLUDE 'TXPL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTKS.INC'
      DATA PRGM /'TXPL  '/
      DATA NOMAP, NOEXCL, WAIT /.FALSE.,.FALSE.,.TRUE./
      DATA LUN /26/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input values from AIPS.
      NPARMS = 21
      CALL GTPARM (PRGM, NPARMS, QUICK, XNAMIN, BUFFER, IERR)
      RETCOD = IERR
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
C                                       If not CRT Release POPs
 10   I = IROUND (DOCRT)
      IF ((DOCRT.GT.0.0001) .AND. (I.EQ.0)) I = 1
      IDOCRT = MIN (I, 132)
      QUICK = QUICK .AND. ((I.LE.0) .OR. (I.EQ.2) .OR. (I.EQ.3))
      IF (QUICK) CALL RELPOP (RETCOD, BUFFER, IERR2)
      IF (RETCOD.NE.0) GO TO 995
      DOPRNT = (IDOCRT.LE.1) .OR. (IDOCRT.GE.4)
      DOCRT = IDOCRT
C                                       Get map header.
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      USER = NLUSER
      IVER = IROUND (VERSN)
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CNO = 1
      TYPIN = '  '
      CALL CATDIR ('SRCH', IVOL, CNO, NAMIN, CLSIN, ISEQ, TYPIN, USER,
     *   STATUS, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (8)
         GO TO 995
C                                       Read catalog header.
 40   CALL CATIO ('READ', IVOL, CNO, CATBLK, 'REST', BUFFER, CATERR)
      IF (CATERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 995
C                                       Find version to plot
 50   IF (IVER.LE.0) CALL FNDEXT ('PL', CATBLK, IVER)
C                                       PLot file not found.
      IF (IVER.LE.0) THEN
         MSGTXT = 'PLOT FILE NOT IN CATALOG'
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Build file name.
 70   CALL ZPHFIL ('PL', IVOL, CNO, IVER, GFILE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open graphics file.
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, NOMAP, NOEXCL, WAIT, IERR)
      IF (IERR.EQ.0) GO TO 80
         WRITE (MSGTXT,1070)
         CALL MSGWRT (8)
         GO TO 995
 80   CALL H2CHR (48, 1, XLNAME, LPNAME)
C                                       Write to terminal or Printer.
      CALL TEXDRW (LUN, FIND, IVOL, CNO,  IERR)
      IF (IERR.EQ.0) GO TO 90
         WRITE (MSGTXT,1080)
         CALL MSGWRT (8)
         GO TO 990
C                                        update image catalog
 90   CONTINUE
C                                       Close graph file.
 990  CALL ZCLOSE (LUN, FIND, IERR)
 995  CALL DIETSK (RETCOD, QUICK, BUFFER)
C
      STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1020 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1040 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
 1070 FORMAT ('PLOT FILE NOT FOUND')
 1080 FORMAT ('ERROR WRITING TO TEXT DEVICE')
      END
      SUBROUTINE TEXDRW (LUN, FIND, IVOL, CNO, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the
C   TEXT oriented dumb terminal or printer
C   Inputs:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            IVOL    I   DISK containing MAP
C            CNO     I   Catalog number of file
C   Output:
C            IOBUF   I(256)   buffer to use for I/O.
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      REAL   DX, DY, SCALEF, X, XYRATO, RX1, RX2, RY1, RY2, AX, AY,
     *   ASPMM, XPXSEP, YPXSEP, YTKDMM, UASPMM, XYDIFF, YASPMM, DELTA,
     *   Y, GDX, GDY
      LOGICAL   T
      INTEGER   ITYPE, IOBUF(256), FIND, GRYERR, LOOP, LOOP2, IANGL,
     *   ICHB, ICHL, ICHR, ICHT, IERR, IERR2, INCRRN, IOPOS, IORRN, IT,
     *   IX1, IY1, LUN, NXA, NYA, OPCODE, I, IERRC, IERRLM, IX2, IY2,
     *   NCHAR, IARG, INO, CNO, IVOL, NPIXL, IGLO, IGHI, IGDX, IGDY,
     *   NCOL
      HOLLERITH HOBUF(256)
      INCLUDE 'TXPL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTKS.INC'
      EQUIVALENCE (IOBUF, HOBUF)
      DATA T /.TRUE./
      DATA IGLO, IGHI/0,0/
C-----------------------------------------------------------------------
C                                       Initialize all values.
C                                       Assume symetric device for now.
      YTKDMM = XTKDMM
      IERR = 0
      IERRLM = 20
      IERRC = 0
      GRYERR = 0
      IOPOS = 9999
      TKPOS = 0
      DO 10 LOOP = 1,60
         SCREEN(LOOP) = '          '
 10      CONTINUE
C                                       initialize screen parameters,
C                                       set character size to 1 by 1
      CHARSI = 3
      CSIZTK(1) = CHARSI
      CSIZTK(2) = CHARSI
      MINX = 1
      MINY = 1
C                                       If writing on dumb crt,
      IF (IDOCRT.GT.0) THEN
C                                       if plot written to history
         IF ((IDOCRT.EQ.2) .OR. (IDOCRT.EQ.3)) THEN
            MAXC = 64
            MAXY = 26 * CHARSI
C                                       else plot to terminal
         ELSE
            CALL ZWINC (NCOL)
            IF ((IDOCRT.LE.72) .OR. (IDOCRT.GT.NCOL)) IDOCRT = NCOL
            MAXC = IDOCRT
            DOCRT = IDOCRT
C                                       CRTMAX characters high
            MAXY = MAX((ABS(CRTMAX)-1),22) * CHARSI
            END IF
C                                       else plotting on printer
      ELSE
         MAXC = NCHPRT - 1
         MAXY = (PRTMAX-5) * CHARSI
         END IF
      MAXX = MAXC * CHARSI
C                                       zero bit array
      DO 20 LOOP = 1,MAXY
         DO 20 LOOP2 = 1,MAXX
            BITSCR(LOOP2,LOOP) = 0
 20      CONTINUE
C                                       Set plot region to number of
C                                       chars times CHARSI
      MAXXTK(1) = MAXX
      MAXXTK(2) = MAXY
      CALL RFILL (TKSIZE, 0.0, TKBUFF)
C                                       determine where plot starts
      CALL ZFIO ('READ', LUN, FIND, 1, IOBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IORRN = (IOBUF(10) + 9) / 256 + 1
C                                       next plot command
C                                       Read record.
 50   IF (IOPOS.LE.256) GO TO 60
 55      IORRN = IORRN + 1
         CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IOPOS = 1
C                                       Transfer based on opcode.
 60   OPCODE = IOBUF(IOPOS)
      IF ((OPCODE.GT.0) .AND. (OPCODE.LE.18)) GO TO (100, 200,
     *   300, 400, 500, 600, 700, 800, 150, 500, 750, 300, 300, 600,
     *   600, 160, 500, 810), OPCODE
      IF (OPCODE.EQ.0) GO TO 55
C                                       End of file.
      IF (OPCODE.NE.32767) GO TO 970
         GO TO 990
C                                       Opcode 1, initialize.
 100  ITYPE = IOBUF(IOPOS+5)
      IOPOS = IOPOS + 6
      GO TO 50
C                                       Opcode 9, line type (ignored)
 150  IOPOS = IOPOS + 2
      GO TO 50
C                                       Opcode 16, 3-color color
C                                       (ignored)
 160  IOPOS = IOPOS + 4
      GO TO 50
C                                       Opcode 2, line drawing init.
 200  XYRATO = IOBUF(IOPOS+1) / 100.0
      SCALEF = IOBUF(IOPOS+2)
      IX1    = IOBUF(IOPOS+3)
      IY1    = IOBUF(IOPOS+4)
      IX2    = IOBUF(IOPOS+5)
      IY2    = IOBUF(IOPOS+6)
      RX1    = IOBUF(IOPOS+7)/1000.0
      RY1    = IOBUF(IOPOS+8)/1000.0
      RX2    = IOBUF(IOPOS+9)/1000.0
      RY2    = IOBUF(IOPOS+10)/1000.0
      ICHL   = (IOBUF(IOPOS+11)/10.0) * CSIZTK(1)  + 0.5
      ICHB   = (IOBUF(IOPOS+12)/10.0) * CSIZTK(2)  + 0.5
      ICHR   = (IOBUF(IOPOS+13)/10.0) * CSIZTK(1)  + 0.5
      ICHT   = (IOBUF(IOPOS+14)/10.0) * CSIZTK(2)  + 0.5
      IOPOS = IOPOS + 15
C                                      Expand scale to use all space
      XYRATO = XYRATO * REAL (MAXX) / REAL (MAXY)
C                                       Find no. pIXels inside border.
      NYA = MAXXTK(2) - ICHT - ICHB
      NXA = MAXXTK(1) - ICHL - ICHR
      IF ((NXA.GT.0) .AND. (NYA.GT.0)) GO TO 210
         WRITE (MSGTXT,1200) NXA, NYA
         CALL MSGWRT (7)
         GO TO 990
C                                       Compute scaling: sizes
 210  AX = ABS (IX2 - IX1) * XYRATO
      AY = ABS (IY2 - IY1)
      X = ABS (IX2+RX2-IX1-RX1) * XYRATO
      Y = ABS (IY2+RY2-IY1-RY1)
      IF ((X.GT.0) .AND. (Y.GT.0)) GO TO 220
         WRITE (MSGTXT,1210) X, Y
         CALL MSGWRT (7)
         GO TO 990
C                                       Factors
 220  IF ((X/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA/SCALEF * AY/Y
         SCALEX = SCALEY * AX/AY
      ELSE
         SCALEX = NXA/SCALEF * AX/X
         SCALEY = SCALEX * AY/AX
         END IF
C                                       Mm / arc sec scaling
 230  LOCNUM = 1
      IF ((ITYPE.EQ.2) .OR. (ITYPE.EQ.3) .OR. (ITYPE.EQ.6) .OR.
     *   (ITYPE.EQ.18) .OR. (ITYPE.EQ.27) .OR. (ITYPE.EQ.28) .OR.
     *   (ITYPE.EQ.29)) THEN
         CALL SETLOC (IOBUF(IOPOS), T)
         IF (AXTYP(LOCNUM).EQ.1) THEN
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
            YPXSEP = XPXSEP
            UASPMM = ASPMM
            CALL SCALMM (AX, AY, XPXSEP, YPXSEP, SCALEF, XTKDMM,
     *         YTKDMM, ASPMM, SCALEX, SCALEY)
            YASPMM = ABS (AY * AXINC(2,LOCNUM) * 3600.0 * YTKDMM)
     *         / (SCALEY * SCALEF)
            XYDIFF = ABS (ASPMM - YASPMM)
            DELTA = .05 * ASPMM
            END IF
         END IF
C                                        Center
 235  NXA = SCALEX*SCALEF*X/AX + ICHL + ICHR
      IARG = MAXXTK(1) - NXA
      RX0 = ICHL + MAX (0, IARG)/2 + 1
      NYA = SCALEY*SCALEF*Y/AY + ICHB + ICHT
      IARG = MAXXTK(2) - NYA
      RY0 = ICHB + MAX (0, IARG)/2 + 1
      IOPOS = IOPOS + 5
      RX0 = RX0 - RX1 * XYRATO * SCALEX * SCALEF / AX
      RY0 = RY0 - RY1 * SCALEY * SCALEF / AY
C                                       ScaleX*ScaleF is the x size
C                                       inside the plot border in
C                                       device pixels
      GO TO 50
C                                       Opcode 3, grey scale init.
C                                       Max and Min Grey values
 300  IGLO = IOBUF(IOPOS+1) / 8
      IGHI = IOBUF(IOPOS+2) / 8
C                                       Size of a Grey scale unit
      IGDX = IOBUF(IOPOS+3)
      IGDY = IOBUF(IOPOS+4)
      GDX = 1
      GDY = 1
      IF (IGDX.NE.0) GDX  = SCALEX * SCALEF / IGDX
      IF (IGDY.NE.0) GDY  = SCALEY * SCALEF / IGDY
      IF ((GDX.LT..01) .OR. (GDX.GT.30)) GDX = 0.4
      IOPOS = IOPOS + 5
      IF (OPCODE.EQ.13) IOPOS = IOPOS + 3
      IF (OPCODE.EQ.12) IOPOS = IOPOS + 7
      GO TO 50
C                                       Opcode 4, position cursor.
 400  X = IOBUF(IOPOS+1)
      Y = IOBUF(IOPOS+2)
      IOPOS = IOPOS+3
      CALL TEXVEC (X, Y, 1,  IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 5, draw vector.
C                                       Opcode 10, dark vector
C                                       Opcode 17, color vector
 500  X = IOBUF(IOPOS+1)
      Y = IOBUF(IOPOS+2)
      IOPOS = IOPOS+3
      CALL TEXVEC (X, Y, 2, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 6, write characters.
 600  NCHAR = IOBUF(IOPOS+1)
      IANGL = IOBUF(IOPOS+2)
      DX    = IOBUF(IOPOS+3)/100.0
      DY    = IOBUF(IOPOS+4)/100.0
      CALL TXCHAR (NCHAR, IANGL, DX, DY, HOBUF(IOPOS+5), IERR)
      IF (IERR.NE.0) GO TO 960
      IOPOS = IOPOS + 5 + ((NCHAR + 3) / 4)
      GO TO 50
C                                       Do not process past end of buf
 700  IARG = 256 - IOPOS - 2
      NPIXL = MIN (IARG, IOBUF(IOPOS+1))
      IANGL = IOBUF(IOPOS+2)
C                                       PROCESS NPIXLs at once
      CALL TXGREY (NPIXL, IANGL, IGLO, IGHI, GDX, GDY,
     *   IOBUF(IOPOS+3), IERR)
      IF (IERR.NE.0) GO TO 960
      IOPOS = IOPOS + 3 + IOBUF(IOPOS+1)
C                                       Update IO position. Read
C                                       proper RRN if necessary.
      IF (IOPOS.LE.256) GO TO 60
C                                       Grey scale spans a buffer
C                                       determine number of blocks left
         INCRRN = (IOPOS-1)/256
C                                       process each block
         DO 720 I = 1, INCRRN
           IORRN = IORRN + 1
C                                       Get the rest
           CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
           IF (IERR.NE.0) GO TO 960
C                                       if not last block process all
           IF (I.LT.INCRRN) NPIXL = 256
           IF (I.EQ.INCRRN) NPIXL = IOPOS - 256*INCRRN - 1
C                                       process the block
           CALL TXGREY (NPIXL, IANGL, IGLO, IGHI, GDX, GDY, IOBUF(1),
     *        IERR)
           IF (IERR.NE.0) GO TO 960
 720       CONTINUE
C                                       point to next opcode
         IOPOS = IOPOS - 256*INCRRN
         GO TO 60
C                                       Opcode 11, write color scale
 750  GRYERR = GRYERR + 1
      IOPOS = IOPOS + 3 + 3*IOBUF(IOPOS+1)
      IF (IOPOS.GT.256) THEN
         INCRRN = (IOPOS-1)/256
         IORRN = IORRN + INCRRN
         IOPOS = IOPOS - 256*INCRRN
         CALL ZFIO ('READ', LUN, FIND, IORRN, IOBUF, IERR)
         IF (IERR.NE.0) GO TO 960
         END IF
      GO TO 60
C                                       Opcode 8, put misc info in
C                                       image catalog.
 800  INO = IOBUF(IOPOS+1)
      IOPOS = IOPOS + INO + 2
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
      GO TO 990
C                                       Disk error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close files.
C                                       force alpha
 990  IT = 31
C                                       Write out screen of data to
C                                       the terminal
      CALL TEXFLS (IVOL, CNO, IGLO, IGHI, IERR2)
      IF (IERRC.GT.0) THEN
         WRITE (MSGTXT,1990) IERRC
         CALL MSGWRT (3)
         END IF
      IF (GRYERR.GT.0) THEN
         WRITE (MSGTXT,1995) GRYERR
         CALL MSGWRT (3)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('TEXDRW: ERR FROM GINITL ARGS. NXA, NYA=',2I6)
 1210 FORMAT ('TEXDRW: ERR FROM GINITL ARGS. X,Y=',2E10.3)
 1960 FORMAT ('TEXDRW: TEXTRONIX WRITE ERROR')
 1970 FORMAT ('TEXDRW: INVALID OPCODE',I5,' IN GRAPH FILE')
 1980 FORMAT ('TEXDRW: DISK IO RELATED ERROR',I3)
 1990 FORMAT (I8,' VECTORS TRUNCATED AT EDGES')
 1995 FORMAT (I8,' COLOR COMMANDS IGNORED')
      END
      SUBROUTINE TXCHAR (INCHAR, IANGL, DCX, DCY, TEXT, IERR)
C-----------------------------------------------------------------------
C  TXCHAR will write characters to a Dumb Terminal
C  Inputs:
C     INCHAR  I      number of characters.
C     IANGL   I      0=horizontal, other = vertical.
C     DCX     R      X distance in characters from current position.
C     DCY     R      Y distance in characters from current position.
C     TEXT    H(*)   Hollerith characters.
C  Outputs:
C     IERR    I      error indicator. 0 = ok.
C-----------------------------------------------------------------------
      REAL      DCX, DCY, X, Y
      HOLLERITH TEXT(*)
      CHARACTER CTEXT*512
      INTEGER   IANGL, IERR, IERRC, IERRL, INCHAR, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      DATA IERRL /50/
C-----------------------------------------------------------------------
      IERR = 0
      IF (INCHAR.LE.0) GO TO 999
      IERRC = 0
C                                       Convert to character
      CALL H2CHR (INCHAR, 1, TEXT, CTEXT)
C                                       Calc starting position, CSIZTK
C                                       is the character size
      X = RXL + DCX * CSIZTK(1)
      Y = RYL + DCY * CSIZTK(2)
C                                       Move to the starting position
C                                       for the string
      CALL TEXVEC (X, Y, 3, IERR)
      IERRC = IERRC + IERR
      IF (IERRC.GT.IERRL) GO TO 980
C                                       Alpha mode.
      IF (IERR.NE.0) GO TO 980
C                                       Loop for each character.
      DO 10 I = 1,INCHAR
C                                       Put the character at the screen
C                                       coordinate
         CALL TEXPUT (CTEXT(I:I))
C                                       Vertical, reposition.
         IF (IANGL.EQ.0) GO TO 10
            X = RXL
            Y = RYL - CSIZTK(2)
            CALL TEXVEC (X, Y, 3, IERR)
            IERRC = IERRC + IERR
            IF (IERRC.GT.IERRL) GO TO 980
 10      CONTINUE
      IERR = 0
      GO TO 999
C                                       Error limit exceeded.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('TXCHAR: ERROR LIMIT EXCEEDED.')
      END
      SUBROUTINE TXGREY (NPIXL, IANGL, IGLO, IGHI, GDX, GDY, THEGRE,
     *   IERR)
C-----------------------------------------------------------------------
C  TXGREY will write GREY Scales to a Dumb Terminal
C  Inputs:
C     NPIXL   I      number of greyscale values.
C     IANGL   I      0=horizontal, other = vertical.
C     IGLO    I      Minimum Grey scale value in counts
C     IGHI    I      Maximum Grey scale value in counts
C     GDX     R      X size of a grey scale unit in pIXels
C     GDY     R      Y size of a grey scale unit in pIXels
C     THEGRE  I(*)   greyscales.
C In/Out COMMON
C      RXL,RYL R   Last X and Y coordinates
C Output:
C      IERR   I      Error code, IERR=0 is good
C-----------------------------------------------------------------------
      INTEGER   NPIXL, IANGL, IGLO, IGHI, THEGRE(*)
      REAL      GDX, GDY
      INTEGER   IERR, I, PIXL, IX, IY
      INCLUDE 'TXPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF ((NPIXL.LE.0) .OR. (NPIXL.GT.256)) GO TO 999
      I = IGHI
C                                       Loop for every other grey scale
      DO 100 I = 1,NPIXL,2
C                                       Convert Grey to negative range
         PIXL = IGLO - (THEGRE(I)/8)
C                                       if screen does not have a line
C                                       here and new pixel is brighter
C                                       than old, put grey at the
C                                       screen coordinate.
         IF ((BITSCR(LASTX,LASTY).LE.0) .AND. (BITSCR(LASTX,LASTY).GT.
     *      PIXL)) BITSCR(LASTX,LASTY) = PIXL
C                                       Move to next grey start
         IF (IANGL.EQ.0) THEN
C                                       Horizontal, move two units
            RXL = RXL + GDX + GDX
         ELSE
C                                       Vertical, reposition.
            RYL = RYL - GDY - GDY
            ENDIF
C                                       Do actual move
         IX = RXL
         IY = RYL
C                                       Side effect:changes LASTX,LASTY
         CALL TXDVEC (1, IX, IY, IERR)
 100  CONTINUE
C
 999  RETURN
      END
      SUBROUTINE TEXVEC (XX, YY, IN, IERR)
C-----------------------------------------------------------------------
C   This routine will put lines and move to X and Y coordinates
C   into the TEXT output buffer.
C   Inputs:
C      XX     R   X coordinate value.
C      YY     R   Y coordinate value.
C      IN     I   control value:
C                 1 = Scale XX and YY and preceed coordinates
C                     by 'write dark vector' control charactr
C                 2 = Scale XX and YY, put in buffer.
C                 3 = XX and YY are not scaled, 'write dark vector'
C                     control character is put into
C                     the buffer.
C                 4 = no scale, write vector
C   Output:
C      IERR   I   error code, 0=ok, 1=write error.
C   COMMON:
C      TKBUFF
C      TKPOS
C      RXL, RYL
C-----------------------------------------------------------------------
      REAL      XX, YY
      INTEGER   IN, IERR
C
      INTEGER   X, Y
      LOGICAL   ISERR
      REAL      ALPHA, BETA, RZ(2), RXN, RYN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
      ISERR = .FALSE.
C                                       Set last X and Y.
      RXN = XX
      RYN = YY
      RZ(1) = MAXXTK(1)
      RZ(2) = MAXXTK(2)
C                                       If IN = 1 or 2 do scaling.
      IF (IN.LE.2) THEN
         RXN = XX*SCALEX + RX0
         RYN = YY*SCALEY + RY0
         END IF
C                                       If IN = 1 or 3 'dark vector'.
      IF ((IN.NE.1) .AND. (IN.NE.3)) GO TO 20
         X = MAX (1.0, MIN (RZ(1), RXN)) + 0.5
         Y = MAX (1.0, MIN (RZ(2), RYN)) + 0.5
         CALL TXDVEC (1, X, Y, IERR)
         GO TO 90
C                                       Bright vector
 20   CONTINUE
C                                       Check start point
         IF ((RXL.GT.0.5) .AND. (RXL.LE.RZ(1)) .AND. (RYL.GT.0.5)
     *      .AND. (RYL.LE.RZ(2))) GO TO 30
            X = MAX (1.0, MIN (RZ(1), RXL)) + 0.5
            Y = MAX (1.0, MIN (RZ(2), RYL)) + 0.5
            ALPHA = 1.0
            IF (RXN.NE.RXL) ALPHA = (X - RXN) / (RXL - RXN)
            BETA = 1.0
            IF (RYN.NE.RYL) BETA = (Y - RYN) / (RYL - RYN)
            ALPHA = MIN (ALPHA, BETA)
            X = RXN + ALPHA * (RXL-RXN) + 0.5
            Y = RYN + ALPHA * (RYL-RYN) + 0.5
            CALL TXDVEC (1, X, Y, IERR)
            IF (IERR.NE.0) GO TO 90
C                                       Check end point
 30      X = MAX (1.0, MIN (RZ(1), RXN)) + 0.5
         Y = MAX (1.0, MIN (RZ(2), RYN)) + 0.5
         IF ((RXN.GT.0.5) .AND. (RYN.GT.0.5) .AND. (RXN.LE.RZ(1))
     *      .AND. (RYN.LE.RZ(2))) GO TO 40
            ISERR = .TRUE.
            ALPHA = 1.0
            IF (RXN.NE.RXL) ALPHA = (X - RXL) / (RXN - RXL)
            BETA = 1.0
            IF (RYN.NE.RYL) BETA = (Y - RYL) / (RYN - RYL)
            ALPHA = MIN (ALPHA, BETA)
            X = RXL + ALPHA * (RXN-RXL) + 0.5
            Y = RYL + ALPHA * (RYN-RYL) + 0.5
 40      CONTINUE
         CALL TXDVEC (2, X, Y, IERR)
C                                         Save new pos
 90   RXL = RXN
      RYL = RYN
      IF ((IERR.EQ.0) .AND. (ISERR)) IERR = 2
C
      RETURN
      END
      SUBROUTINE TXDVEC (IOP, X, Y, IERR)
C-----------------------------------------------------------------------
C   TXDVEC draws a line in the array of characters to be displayed on a
C   dumb terminal.
C   Input:
C      IOP    I   OPcode, 1=Move, 2=Draw
C      X      I   Screen coordinate in characters
C      Y      I   Screen coordinate in characters
C Output:
C      IERR   I   Error code, 0=good
C-----------------------------------------------------------------------
      INTEGER   IOP, X, Y, IERR
C
      INTEGER   BITCOU, DELTAX, DELTAY, IX, IY, I, J
      REAL      DX, DY
      INCLUDE 'TXPL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       do not draw if not a draw
C                                       command.
      IF (IOP.NE.2) GO TO 900
C                                       calculate the absolute movement
C                                       in x and y
      DX = X - LASTX
      DY = Y - LASTY
      DELTAX = ABS (DX)
      DELTAY = ABS (DY)
C                                       turn on as many pixels as the
C                                       max movement
      BITCOU = DELTAX
      IF (DELTAY.GT.BITCOU) BITCOU = DELTAY
C                                       convert pixel increment to line
C                                       increment .
      IF (BITCOU.GT.0) THEN
         DX = DX / BITCOU
         DY = DY / BITCOU
         END IF
C                                       fill in the bits.
      J = BITCOU + 1
      DO 100 I = 1,J
        IX = (I-1)*DX + LASTX
        IY = (I-1)*DY + LASTY
C                                       limit values to within the
C                                       screen.
        IX = MIN (MAXX, MAX (MINX, IX))
        IY = MIN (MAXY, MAX (MINY, IY))
        BITSCR(IX,IY) = 1
 100    CONTINUE
C                               store the last coordinate filled
 900  LASTX = MIN (MAXX, MAX (MINX, X))
      LASTY = MIN (MAXY, MAX (MINY, Y))
C
 999  RETURN
      END
      SUBROUTINE TEXPUT (THECHS)
C-----------------------------------------------------------------------
C   TEXPUT puts one character at the current screen location on the
C   terminal.
C   Input:
C      THECHS  C*1   Input character
C-----------------------------------------------------------------------
      CHARACTER   THECHS*1
C
      INTEGER   IX,IY, IARG
      INCLUDE 'TXPL.INC'
C-----------------------------------------------------------------------
C                                       convert bit coord to Character
C                                       coord.
      IX = (LASTX + CHARSI - 1) / CHARSI
      IARG = LASTY / CHARSI
      IY =  MAX (1,IARG)
      SCREEN(IY)(IX:IX) = THECHS
C                                       Move current position to next
C                                       character
      LASTX = LASTX + CHARSI
      IF (LASTX.GT.MAXX) LASTX = MAXX
C
 999  RETURN
      END
      SUBROUTINE TEXFLS (IVOL, CNO, IGLO, IGHI, IERR)
C-----------------------------------------------------------------------
C   TEXFLS flushes the tex screen to printer or user teminal
C   Input:
C      IVOL    I     Disk containing map
C      CNO     I     Catalog number of file
C      IGLO    I     Low gray scale value
C      IGHI    I     High gray scale value
C   Output:
C      IERR    I     Error code, 0=good
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, WORK*160, OUTCHA*1, TITL1*160, TITL2*160,
     *   WORK2*160
      REAL      GREYVA
      INTEGER   IERR, IVOL, CNO, IGLO, IGHI, OUTLUN, OUTIND, NC, I, J,
     *   K, NLINE, PIXEL(3,3), IX, IY, CRTX, CRTY, NUMGRE, HLUN, II,
     *   IBUF(256), IPAGE
      LOGICAL   T
      INCLUDE 'TXPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA HLUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      IERR = 0
      IPAGE = 0
      NLINE = 999
C                                       Calculate max CRT X and Y
      CRTX = MAXX/CHARSI
      CRTY = MAXY/CHARSI
      NC   = CRTX
C                                       Open output device
C                                       if not writing to msg file.
      IF (DOPRNT) THEN
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, OUTLUN, OUTIND, II, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       Initialize titles for print
         TITL1 = ' '
         TITL2 = ' '
         END IF
C                                       write out one line at a time,
      II = CRTY - 1
      DO 400 I = 1,II
C                                       getting y axis positive
         IY = CRTY - I
C                                       copy Screen line to scratch
         WORK(1:NC) = SCREEN(IY)(1:NC)
C                                       convert PIXEL(3*3) into one
C                                       character.
         DO 300 IX = 1,CRTX
C                                       init count of greyscales values
            NUMGRE = 0
            GREYVA = 0
C                                       transfer the bits to a work
C                                       array
            DO 200 J = 1,3
               DO 100 K = 1,3
                  PIXEL(J,K) = BITSCR((IX-1)*CHARSI+J,IY*CHARSI+K)
C                                       greyscale values are negative
                  IF (PIXEL(J,K).LT.0) THEN
C                                       count number of greyscale value
                     NUMGRE = NUMGRE + 1
C                                       let darkest grey represent pixel
                     IF (GREYVA.LT.-PIXEL(J,K)) GREYVA = -PIXEL(J,K)
                     END IF
 100              CONTINUE
 200           CONTINUE
C                                       convert the 3*3 PIXEL to 1 char
            CALL BIT2CH (PIXEL, OUTCHA)
C                                       does screen have a blank here?
            IF (WORK(IX:IX).EQ.' ') WORK(IX:IX) = OUTCHA
C                                       now insert greyscale value if
C                                       any, and is grey scale correctly
C                                       initialized?
            IF ((NUMGRE.GT.0) .AND. (IGHI-IGLO.GT.0)) THEN
C                                       grey scale ranges from 0 to 9
               GREYVA = 10. * GREYVA / (IGHI-IGLO)
C                                       values of 0. to .999 map to 0,
C                                       etc 9. to 9.999 maps to 9, 10
C                                       is special.
               NUMGRE   = GREYVA
C                                       convert intensity to char
               CALL SETDCH (NUMGRE, OUTCHA)
C                                       does screen have a blank here?
               IF (WORK(IX:IX).EQ.' ') WORK(IX:IX) = OUTCHA
               END IF
 300        CONTINUE
C                                       copy scratch back to screen
         SCREEN(IY) = WORK(:NC)
C                                       CRT (no header), Printer
         IF (DOPRNT) THEN
            IF (IDOCRT.GT.0) NLINE = 1
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, TITL1, TITL2,
     *         WORK, NLINE, IPAGE, WORK2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1900,ERR=999) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       Message Write prints 72 chars
C                                       per line and adds a 8 char label
C                                       leaving only 64 chars of info.
         ELSE IF (IDOCRT.EQ.3) THEN
            MSGTXT = SCREEN(IY)(1:MIN(NC,64))
            CALL MSGWRT (4)
            END IF
 400     CONTINUE
C                                       Close down output
C                                       CRT: wait for user input
      IF (DOPRNT) THEN
         IF (IDOCRT.GT.0) THEN
            NLINE = 1001
            IPAGE = 1
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, TITL1, TITL2,
     *         WORK, NLINE, IPAGE, WORK2, IERR)
            END IF
         CALL LPCLOS (OUTLUN, OUTIND, NLINE, IERR)
         END IF
C                                       If plot added to history
      IF (IDOCRT.EQ.2) THEN
C                                       Prepare history
         CALL HIINIT (3)
         CALL HIOPEN (HLUN, IVOL, CNO, IBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Write out one line at a time,
C                                       getting y axis positive
         II = CRTY - 1
         DO 900 I = 1,II
            IY = CRTY - I
            WRITE (HILINE,1400,ERR=900) TSKNAM(1:5), SCREEN(IY)(1:64)
            CALL HIADD (HLUN, HILINE, IBUF, IERR)
 900        CONTINUE
         CALL HICLOS (HLUN, T, IBUF , IERR)
         END IF
C
 999  RETURN
C----------------------------------------------------------------------
 1040 FORMAT ('ERROR',I5,' OPENING OUTPUT DEVICE')
 1100 FORMAT ('ERROR:',I7,' OPENING HISTORY FILE')
 1400 FORMAT (A5,1X,A)
 1900 FORMAT ('PRTLIN ERROR:',I7)
      END
      SUBROUTINE BIT2CH (PIXEL, OUTCHA)
C-----------------------------------------------------------------------
C   Convert the 3*3 pixel to one character
C   Input:
C      PIXEL  I(3,3)   Array of bits to be converted to a character
C                      organized as follows
C                                  COL
C                     (COL,ROW)
C                        1,3    2,3   3,3
C               ROW      1,2    2,2   3,2
C                        1,1    2,1   3,1
C              SO "T" IS (1,3) (2,3) (3,3) (2,2) AND (2,1)
C   Output:
C      OUTCHA  C*1     One character representing the pixel
C-----------------------------------------------------------------------
      INTEGER   PIXEL(3,3)
      CHARACTER OUTCHA*1
C
      CHARACTER TICK*1, MINUS*1, QUOTE*1, PIPE*1, SLASH*1, BACKSL*2,
     *   PLUS*1, EQUAL*1, XX*1, HH*1, II*1, OO*1, UU*1
      INTEGER   I, J, NUMON, ROWON(3), COLON(3)
      DATA TICK, MINUS, QUOTE, PIPE
     *   / '`',   '-',   '"',   '|'/
      DATA SLASH, BACKSL, PLUS, EQUAL
     *   / '/',    '\\',   '+',  '='/
      DATA XX, HH,  II,  OO,  UU
     *   /'X', 'H', 'I', 'O', 'U'/
C-----------------------------------------------------------------------
C                                       INIT COUNTS OF NUMB ON IN
C                                       ROWS
      NUMON = 0
      DO 10 I = 1,3
         ROWON(I) = 0
         COLON(I) = 0
 10      CONTINUE
C                                       Count the number of bits on
C                                       will range from 0 to 9
C                                       I = col, J = row
      DO 30 I = 1, 3
         DO 20 J = 1, 3
            IF (PIXEL(I,J).GE.1) THEN
               NUMON = NUMON + 1
               ROWON(J) = ROWON(J) + 1
               COLON(I) = COLON(I) + 1
               END IF
 20         CONTINUE
 30      CONTINUE
C                                       If none on, blank, etc...
C                                       set default for all cases
      CALL SETDCH (NUMON, OUTCHA)
C                                       Now correct for special cases
      IF ((NUMON.EQ.1) .AND. (ROWON(3).EQ.1)) OUTCHA = TICK
C                                       Special processing for 2,3 on
      IF (NUMON.LE.3) THEN
C                                       Use mostly vertical or horiz
         IF ((ROWON(1).GE.2) .OR. (ROWON(2).GE.2) .OR. (ROWON(3).GE.2))
     *      OUTCHA = MINUS
         IF ((PIXEL(2,3).EQ.0) .AND. (ROWON(3).EQ.2)) OUTCHA = QUOTE
         IF ((COLON(1).GE.2) .OR. (COLON(2).GE.2) .OR. (COLON(3).GE.2))
     *      OUTCHA = PIPE
C                                       One and Two finished.
         IF (NUMON.LT.3) GO TO 999
C
         IF ((ROWON(1).NE.3) .AND. (ROWON(2).NE.3) .AND.
     *      (ROWON(3).NE.3) .AND. (COLON(1).NE.3) .AND.
     *      (COLON(2).NE.3) .AND. (COLON(3).NE.3)) OUTCHA = PLUS
         END IF
C                                       Take care of diagonal cases,
C                                       if center on
      IF ((NUMON.LE.5) .AND. (PIXEL(2,2).NE.0)) THEN
         IF ((PIXEL(1,1).GE.1) .AND. (PIXEL(3,3).GE.1)) OUTCHA = SLASH
         IF ((PIXEL(3,1).GE.1) .AND. (PIXEL(1,3).GE.1))
     *      OUTCHA = BACKSL(1:1)
         IF ((PIXEL(3,1).GE.1) .AND. (PIXEL(1,3).GE.1) .AND.
     *      (PIXEL(1,1).GE.1) .AND. (PIXEL(3,3).GE.1)) OUTCHA = XX
         END IF
C                                       To simplify follow tests,
C                                       exit if only a few pixels
C                                       are on or almost all on.
      IF ((NUMON.LE.3) .OR. (NUMON.GE.8)) GO TO 900
C                                       Try to make lines work
      IF ((COLON(2).EQ.3) .OR. (COLON(3).EQ.3) .OR. (ROWON(1).EQ.3) .OR.
     *   (ROWON(2).EQ.3) .OR. (ROWON(3).EQ.3)) OUTCHA = PLUS
C                                        Cases for which there are
C                                        special characters
      IF ((COLON(1).EQ.3) .AND. (ROWON(1).EQ.3) .AND. (COLON(3).EQ.3))
     *   OUTCHA = UU
      IF ((COLON(2).EQ.3) .AND. (ROWON(1).EQ.3) .AND. (ROWON(3).EQ.3))
     *   OUTCHA = II
      IF ((ROWON(2).EQ.3) .AND. (COLON(1).EQ.3) .AND. (COLON(3).EQ.3))
     *   OUTCHA = HH
      IF ((COLON(1).EQ.2) .AND. (COLON(2).EQ.2) .AND. (COLON(3).EQ.2))
     *   OUTCHA = EQUAL
C                                    IS ALL BUT CENTER ON ?
 900  IF ((NUMON.EQ.8) .AND. (PIXEL(2,2).LE.0)) OUTCHA = OO
C
 999  RETURN
      END
      SUBROUTINE SETDCH (INTENS, OUTCHA)
C-----------------------------------------------------------------------
C   SETDCH sets the character which corresponds to the input intensity
C   Input:
C      INTENS  I    Value to be turned into a character, allowed range
C                   0 to 9, values outside this range are set to blank
C   Output:
C      OUTCHA  C*1  Character representation of the INTENS
C-----------------------------------------------------------------------
      CHARACTER OUTCHA*1
      INTEGER   INTENS
C
      CHARACTER DEFCHA(11)*1
      INTEGER   INRANG
      DATA DEFCHA/' ','.',':','~','+','*','&','%','#','@','M'/
C-----------------------------------------------------------------------
      INRANG = MIN (10, MAX (0, INTENS))
      OUTCHA = DEFCHA(INRANG+1)
C
 999  RETURN
      END
