      PROGRAM TKPL
C-----------------------------------------------------------------------
C! Plots an AIPS plot (PL) file on a graphics device.
C# EXT-appl Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2002-2003, 2006, 2009
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
C   TKPL is a program for the graphics package. The program
C   runs as a detached task initiated from AIPS. First a cataloged
C   file is found using data passed from AIPS.  The list of
C   associated files is searched for a PLot file corresponding
C   to the version number.  The graphics commands in this file
C   are executed for the Tektronix 4012.
C   Inputs:  (from AIPS)
C      INNAME   H*12  Name of primary file.
C      INCLASS  H*6   Class of primary file.
C      INSEQ    R     Sequence number of primary file.
C      INDISK   R     Disk volume number. 0 means try all.
C      INVERS   R     Extension file version number. 0 means.
C                       use the highest version number.
C      ASPMM    R      arc sec per millimeters. 0 means self scale.
C-----------------------------------------------------------------------
      CHARACTER NAMIN*12, CLSIN*6, TYPIN*2, STATUS*4, NAME*6, GFILE*48,
     *   SCRTCH*132
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL      DSKIN, SEQIN, VERSN, ASPMM
      INTEGER   BUFFER(256), CATERR, LUN, FIND, IERR, NPARMS, RETCOD,
     *   SLOT, IVER, IPLANE, IVOL, ISEQ, IERR2, USER, IROUND, INLUN, NC
      LOGICAL   NOMAP, QUICK, NOEXCL, WAIT, EQUAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, VERSN, ASPMM
      DATA NAME /'TKPL  '/
      DATA NOMAP, NOEXCL, WAIT /.FALSE.,.FALSE.,.TRUE./
      DATA INLUN, LUN /5, 26/
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE., BUFFER)
      CALL VHDRIN
C                                       Get input values from AIPS.
      NPARMS = 9
      CALL GTPARM (NAME, NPARMS, QUICK, XNAMIN, BUFFER, IERR)
      RETCOD = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 20
         END IF
      IF (NPOPS.LE.NTKACC) GO TO 20
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         RETCOD = 8
 20   IF (QUICK) THEN
         CALL RELPOP (RETCOD, BUFFER, IERR2)
      ELSE
         MSGTXT = 'Type Return after Plot is finished to continue'
         CALL MSGWRT(3)
         END IF
      IF (RETCOD.NE.0) GO TO 995
C                                       Get map header.
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      USER = NLUSER
      IVER = IROUND (VERSN)
C                                       Character inputs
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      TYPIN = ' '
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
      IF ((CATERR.EQ.0) .OR. (CATERR.EQ.6)) GO TO 50
 40      WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 995
C                                        Find plot file:
C                                        if PL & IVER=0, then main file
 50   EQUAL = TYPIN(1:2) .EQ. 'PL'
      IF ((EQUAL) .AND. (CATERR.EQ.6)) GO TO 40
      IF ((EQUAL) .AND. (IVER.EQ.0)) IVER = CATBLK(KIIMS)
C                                        else take IVER as given or
C                                        as max version #
      IF (IVER.LE.0) CALL FNDEXT ('PL', CATBLK, IVER)
C                                       PLot file not found.
      IF (IVER.LE.0) THEN
         WRITE (MSGTXT,1060)
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Build file name.
 70   CALL ZPHFIL ('PL', IVOL, SLOT, IVER, GFILE, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open graphics file.
      CALL ZOPEN (LUN, FIND, IVOL, GFILE, NOMAP, NOEXCL, WAIT, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070)
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Open TEKTRONICS device.
      CALL ZTKOPN (IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TEKTRONIX OPEN ERROR'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       init TK catalog
 80   CALL TKCATL ('INIT', IPLANE, IPLANE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 980
      CATBLK(IIVOL) = IVOL
      CATBLK(IICNO) = SLOT
C                                       Write to Tektronix 4012.
      CALL TEKDRW (LUN, FIND, ASPMM, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080)
         CALL MSGWRT (8)
         GO TO 980
         END IF
C                                       update TK catalog
 90   CALL TKCATL ('WRIT', IPLANE, IPLANE, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1090) IERR
         CALL MSGWRT (6)
         END IF
C                                       Close TEK
 980  CALL ZTKCLS (IERR)
C                                       Close graph file.
 990  CALL ZCLOSE (LUN, FIND, IERR)
C                                       Get user input before quiting
      IF (.NOT.QUICK .AND. ISBTCH.NE.1) THEN
         CALL ZTTOPN (INLUN, FIND, IERR)
         NC = 1
         CALL ZTTYIO ('READ', INLUN, FIND, NC, SCRTCH, IERR)
         CALL ZTTCLS (INLUN, FIND, IERR)
         END IF

 995  CALL DIETSK (RETCOD, QUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT GET INPUTS FROM AIPS. GTPARM ERR =',I5)
 1010 FORMAT ('TEK DEVICES ALL IN USE')
 1020 FORMAT ('CATALOG ENTRY NOT FOUND. CATDIR ERR =',I5)
 1040 FORMAT ('ERROR READING CATALOG HEADER. CATIO ERR =',I5)
 1060 FORMAT ('PLOT FILE NOT IN CATALOG')
 1070 FORMAT ('PLOT FILE NOT FOUND')
 1080 FORMAT ('ERROR WRITING TO TEKTRONIX DEVICE')
 1090 FORMAT ('ERROR ',I3,' UPDATING TEK CATALOG')
      END
      SUBROUTINE TEKDRW (LUN, FIND, ASPMM, IERR)
C-----------------------------------------------------------------------
C   This routine will execute the commands in a graph file for the
C   TEKTRONIX 4012.
C   Inputs:  LUN     I   logical unit number of an open graph file.
C            FIND    I   pointer to the FTAB info of the graph file.
C            ASPMM   R   user requested arc sec / mm
C            IERR    I   error code. 0 = ok.
C-----------------------------------------------------------------------
      CHARACTER LINE*132
      REAL   DX, DY, SCALEF, X, XYRATO, RX1, RX2, RY1, RY2, AX, AY, Y,
     *   ASPMM, XPXSEP, YPXSEP, YTKDMM, UASPMM, XYDIFF, YASPMM, DELTA
      LOGICAL   T
      INTEGER   ITYPE, FIND, GRYERR, INO, IANGL, ICHB, ICHL, ICHR, ICHT,
     *   IERR, IERR2, INCRRN, IOPOS, IORRN, IT, IX1, IY1, LUN, NXA, NYA,
     *   OPCODE, IOBUF(256), IERRC, IERRLM, IX2, IY2, NCHAR
      HOLLERITH HOBUF(256)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (HOBUF, IOBUF)
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize all values.
C                                       Assume symetric device for now.
      YTKDMM = XTKDMM
      IERR = 0
      IERRLM = 20
      IERRC = 0
      GRYERR = 0
      IOPOS = 9999
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
     *   300, 400, 500, 600, 700, 800, 150, 500, 700, 300, 300, 600,
     *   600, 160, 500, 810), OPCODE
      IF (OPCODE.EQ.0) GO TO 55
C                                       End of file.
      IF (OPCODE.NE.32767) GO TO 970
         GO TO 990
C                                       Opcode 1, initialize.
C                                       Put map type in image header.
 100  ITYPE = IOBUF(IOPOS+5)
      CATBLK(IIPLT) = ITYPE
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
      ICHL   = (IOBUF(IOPOS+11)/10.0) * CSIZTK(1)  +  0.5
      ICHB   = (IOBUF(IOPOS+12)/10.0) * CSIZTK(2)  +  0.5
      ICHR   = (IOBUF(IOPOS+13)/10.0) * CSIZTK(1)  +  0.5
      ICHT   = (IOBUF(IOPOS+14)/10.0) * CSIZTK(2) +  0.5
      IOPOS = IOPOS + 15
C                                       Find no. pixels inside border.
      NYA = MAXXTK(2) - ICHT - ICHB - 1
      NXA = MAXXTK(1) - ICHL - ICHR - 1
      IF ((NXA.GT.0) .AND. (NYA.GT.0)) GO TO 210
         WRITE (MSGTXT,1200) NXA, NYA
         CALL MSGWRT (7)
         GO TO 990
C                                       Compute scaling: sizes
 210  AX = ABS(IX2 - IX1) * XYRATO
      AY = ABS(IY2 - IY1)
      X = ABS(IX2+RX2-IX1-RX1) * XYRATO
      Y = ABS(IY2+RY2-IY1-RY1)
      IF ((X.GT.0) .AND. (Y.GT.0)) GO TO 220
         WRITE (MSGTXT,1210) X, Y
         CALL MSGWRT (7)
         GO TO 990
C                                       Factors
 220  IF ((X/Y).GT.FLOAT(NXA)/FLOAT(NYA)) GO TO 225
         SCALEY = NYA/SCALEF * AY/Y
         SCALEX = SCALEY * AX/AY
         GO TO 230
 225     SCALEX = NXA/SCALEF * AX/X
         SCALEY = SCALEX * AY/AX
C                                       Mm / arc sec scaling
 230  LOCNUM = 1
      IF ((ITYPE.EQ.2) .OR. (ITYPE.EQ.3) .OR. (ITYPE.EQ.6) .OR.
     *   (ITYPE.EQ.18) .OR. (ITYPE.EQ.27) .OR. (ITYPE.EQ.28) .OR.
     *   (ITYPE.EQ.29)) THEN
         CALL SETLOC (IOBUF(IOPOS), T)
         IF (AXTYP(LOCNUM).EQ.1) THEN
            XPXSEP = AXINC(1,LOCNUM) * 3600.0 / XYRATO
            YPXSEP = XPXSEP
C                                       odd: but XYRATO has AXINC(1)/(2)
            UASPMM = ASPMM
            CALL SCALMM (AX, AY, XPXSEP, YPXSEP, SCALEF, XTKDMM,
     *         YTKDMM, ASPMM, SCALEX, SCALEY)
            YASPMM = ABS (AY * AXINC(2,LOCNUM) * 3600.0 * YTKDMM)
     *         / (SCALEY * SCALEF)
            XYDIFF = ABS (ASPMM - YASPMM)
            DELTA = .05 * ASPMM
C                                       X and Y ASPMM about the same.
            IF (XYDIFF.LE.DELTA) THEN
               WRITE (MSGTXT,1230) ASPMM
               CALL MSGWRT (4)
C                                       Using different X and Y ASPMM
            ELSE
               WRITE (MSGTXT,1232) ASPMM
               CALL MSGWRT (4)
               WRITE (MSGTXT,1233) YASPMM
               CALL MSGWRT (4)
               END IF
C
            IF ((UASPMM.GT.0.0) .AND. (UASPMM.LT.ASPMM)) THEN
               WRITE (MSGTXT,1234)
               CALL MSGWRT (2)
               END IF
            END IF
         END IF
C                                        Center
 235  NXA = SCALEX*SCALEF*X/AX + ICHL + ICHR
      RX0 = ICHL + MAX(0,MAXXTK(1)-NXA)/2 + 1
      NYA = SCALEY*SCALEF*Y/AY + ICHB + ICHT
      RY0 = ICHB + MAX(0,MAXXTK(2)-NYA)/2 + 1
C                                        upgrade catalog header
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CALL COPY (5, IOBUF(IOPOS), CATBLK(IIDEP))
      IOPOS = IOPOS + 5
      RX0 = RX0 - RX1 * XYRATO * SCALEX * SCALEF / AX
      RY0 = RY0 - RY1 * SCALEY * SCALEF / AY
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + SCALEX*SCALEF + 0.5
      CATBLK(IICOR+3) = RY0 + SCALEY*SCALEF + 0.5
C                                       Clear screen.
      CALL TKCLR (IERR)
      GO TO 50
C                                       Opcode 3, grey scale init.
 300  IOPOS = IOPOS + 5
      IF (OPCODE.EQ.13) IOPOS = IOPOS + 3
      IF (OPCODE.EQ.12) IOPOS = IOPOS + 7
      GO TO 50
C                                       Opcode 4, position cursor.
 400  X = IOBUF(IOPOS+1)
      Y = IOBUF(IOPOS+2)
      IOPOS = IOPOS+3
      CALL TEKVEC (X, Y, 1,  IERR)
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
      CALL TEKVEC (X, Y, 2, IERR)
      IF (IERR.EQ.0) GO TO 50
      IF (IERR.NE.2) GO TO 960
         IERRC = IERRC + 1
         GO TO 50
C                                       Opcode 6, write characters.
 600  NCHAR = IOBUF(IOPOS+1)
      IANGL = IOBUF(IOPOS+2)
      DX    = IOBUF(IOPOS+3)/100.0
      DY    = IOBUF(IOPOS+4)/100.0
      CALL H2CHR (NCHAR, 1, HOBUF(IOPOS+5), LINE)
      CALL TKCHAR (NCHAR, IANGL, DX, DY, LINE, IERR)
      IF (IERR.NE.0) GO TO 960
      IOPOS = IOPOS + 5 + ((NCHAR-1) / 4) + 1
      GO TO 50
C                                       Opcode 7, write grey scale.
C                                       Opcode 11, write 3color scale.
 700  GRYERR = GRYERR + 1
C                                       Update IO position. Read
C                                       proper RRN if necessary
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
      GO TO 990
C                                       Disk error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (8)
C                                       Close files.
C                                       force alpha
 990  CONTINUE
C                       Move to bottom of the plot
      CALL TEKVEC (1.0, 1.0, 1,  IERR)
      IT = 31
      CALL ZTKBUF (IT, 1, IERR2)
      CALL TEKFLS (IERR2)
      IF (IERR.EQ.2) IERR = 2
      IF (GRYERR.EQ.0) GO TO 995
         WRITE (MSGTXT,1990) GRYERR
         CALL MSGWRT (3)
 995  IF (IERRC.LE.0) GO TO 999
         WRITE (MSGTXT,1995) IERRC
         CALL MSGWRT (3)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('TEKDRW: ERR FROM GINITL ARGS. NXA, NYA=',2I6)
 1210 FORMAT ('TEKDRW: ERR FROM GINITL ARGS. X,Y=',2E10.3)
 1230 FORMAT ('Using',1PE12.5,' arc sec per mm')
 1232 FORMAT ('Using',1PE12.5,' arc sec per mm for x axis')
 1233 FORMAT ('Using',1PE12.5,' arc sec per mm for y axis')
 1234 FORMAT ('Which overrides user aspmm in order to fit plot on',
     *   ' screen')
 1960 FORMAT ('TEKDRW: TEKTRONIX WRITE ERROR')
 1970 FORMAT ('TEKDRW: INVALID OPCODE',I5,' IN GRAPH FILE')
 1980 FORMAT ('TEKDRW: DISK IO RELATED ERROR',I3)
 1990 FORMAT ('TEKDRW: ',I5,' GREY SCALE OPCODES IGNORED')
 1995 FORMAT (I8,' Vectors truncated at edges')
      END

