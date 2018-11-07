LOCAL INCLUDE 'RGBMP.INC'
      CHARACTER OPTYPE*4, FUNCTY*2
      REAL      CATR(256), BLC(7), TRC(7), RANGE(2), TFACT
      INTEGER   CATBLK(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   FORWRD
      EQUIVALENCE (CATR, CATBLK, CATD, CATH)
      COMMON /RGBMPV/ CATBLK, BLC, TRC, RANGE, TFACT, FORWRD
      COMMON /RGBMPC/ OPTYPE, FUNCTY
LOCAL END
      PROGRAM RGBMP
C-----------------------------------------------------------------------
C! Makes red-green-blue image from input image.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2006, 2008-2009, 2015
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
C  Program RGBMP will produce a cubed image with 3 planes, each
C  representing one of the colors red, green, and blue.  Each plane of
C  the input image image makes a weighted contribution to each of the
C  planes of the output image according to the input image planes
C  position along the 3rd axis.  Planes near the front of the
C  cube make their major contribution to the blue plane, middle planes
C  make their major contribution to the green plane, and planes near
C  the back of the cube make their major contribution to the red plane.
C  INPUTS:  Adverbs from AIPS
C     USERID    R    Input & output image user ID
C     INNAME    R(3)   Input image name.
C     INCLASS   R(2)   Input image class.
C     INSEQ     R   Input image sequence.
C     INDISK    R   Disk number for input image.
C     OUTNAME   R(3)   Name of output image.
C     OUTCLASS  R(2)   Class of the output image.
C     OUTSEQ    R   Sequence number of the output image.
C     OUTDISK   R   Disk number for output image.
C     BLC       R(7)   Bottom left corner of input cube. 0=> 1,1,1
C     TRC       R(7)   Top right corner of input cube. 0=> max.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PRGNAM*6
      REAL      DATA1(MAXIMG), DATA2(MAXIMG)
      INTEGER   IERR, INLUN, IOTLUN, IOTLU2, IHOTL, IHINL,
     *   IDATA1(MAXIMG), IDATA2(MAXIMG)
      EQUIVALENCE (DATA1, IDATA1), (DATA2, IDATA2)
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'RGBMP.INC'
      DATA PRGNAM /'RGBMP '/
      DATA INLUN, IOTLUN, IOTLU2, IHOTL, IHINL /16,17,18, 27,28/
C-----------------------------------------------------------------------
C                                       Set up, get parms, open, create
C                                       all files.
      CALL SETUP (PRGNAM, INLUN, IOTLUN, IOTLU2, DATA1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Add weighted planes to proper
C                                       output plane.
      CALL ADDWT (INLUN, IOTLUN, IOTLU2, DATA1, DATA2, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Create, copy, update hist file.
      CALL HISTRY (PRGNAM, INLUN, IOTLUN, IHINL, IHOTL, IDATA1, IDATA2)
C                                       Release AIPS if necessary,
C                                       close, clear files, delete any
C                                       new files if error.
 980  CALL TSKEND (IERR)
C
 999  STOP
      END
      SUBROUTINE SETUP (PRGNAM, INLUN, IOTLUN, IOTLU2, DATA, IERR)
C-----------------------------------------------------------------------
C  SETUP will initialize the WAWA IO package, open the input image,
C  create the output image , determine the name strings of the
C  output image and initialize the output image.
C  INPUTS:  PRGNAM  C*6 Program name.
C           INLUN   I   Input image logical unit number.
C           IOTLUN  I   New image input logical unit number.
C           IOTLU2  I   Output LUN for new image.
C           DATA    R(?)   Work buffer big enough for 1 map row.
C   Outputs:
C      IERR     I      Standard WAWA IO error message.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INLUN, IOTLUN, IOTLU2, IERR
      REAL      DATA(*)
C
      CHARACTER INSTR*36, OUTSTR*36, AXTYP*8
      REAL      ZBLC(7), ZTRC(7), PARMS(34)
      HOLLERITH MAP
      INTEGER   NOPARM
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RGBMP.INC'
C-----------------------------------------------------------------------
      NOPARM = 33
      CALL RFILL (7, 0.0, ZBLC)
      CALL RFILL (7, 0.0, ZTRC)
      CALL TSKBEG (PRGNAM, NOPARM, PARMS(2), IERR)
      IF (IERR.NE.0) GO TO 990
      PARMS(1) = NLUSER
C                                       Other parameters
      CALL H2CHR (4, 1, PARMS(30), OPTYPE)
      CALL H2CHR (2, 1, PARMS(31), FUNCTY)
      RANGE(1) = PARMS(32)
      RANGE(2) = PARMS(33)
      TFACT    = PARMS(34)
      IF (TFACT.LE.0.0) TFACT = 1.0
C                                       Fix the input image name string
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (PARMS(2), PARMS(5), PARMS(7), MAP, PARMS(8),
     *   PARMS(1), INSTR)
C                                       Open the old file. Get header.
      CALL OPENCF (INLUN, INSTR, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETHDR (INLUN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Fix BLC, TRC.
      CALL RCOPY (7, PARMS(16), BLC)
      CALL RCOPY (7, PARMS(23), TRC)
      CALL HDRWIN (BLC, TRC, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check for enough planes on
C                                       3rd axis.
      IF (CATBLK(KINAX+2).LT.3) GO TO 960
      FORWRD = CATR(KRCIC+2).GT.0.0
      CALL H2CHR (8, 1, CATH(KHCTP+4), AXTYP)
      IF (AXTYP(:4).EQ.'FREQ') FORWRD = .NOT.FORWRD
      CALL RNGSET (RANGE, CATR(KRDMX), CATR(KRDMN), RANGE)
C                                       Set up output name string.
      CALL H2WAWA (PARMS(9), PARMS(12), PARMS(14), MAP, PARMS(15),
     *   PARMS(1), OUTSTR)
      CATBLK(KINAX+2) = 3
      CATR(KRBLK) = FBLANK
      CALL MAPCR (INSTR, OUTSTR, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL OPENCF (IOTLUN, OUTSTR, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL OPENCF (IOTLU2, OUTSTR, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Get new improved header.
      CALL GETHDR (IOTLUN, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Init output image to blanks
      ZBLC(3) = 1.0
      ZTRC(3) = 3.0
      CALL MAPWIN (IOTLUN, ZBLC, ZTRC, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL MFILL (IOTLUN, FBLANK, DATA, IERR)
      IF (IERR.NE.0) GO TO 970
      GO TO 999
C                                       error handling.
C                                       Not enough planes in 3rd dim.
 960  IERR = 10
      WRITE (MSGTXT,1960) CATBLK(KINAX+2)
      CALL MSGWRT (7)
      GO TO 999
 970  CALL PRTERR (IERR, IOTLUN)
      GO TO 999
 980  CALL PRTERR (IERR, INLUN)
      GO TO 999
 990  CALL PRTERR (IERR, 0)
C
 999  RETURN
C-----------------------------------------------------------------------
 1960 FORMAT ('NOT ENOUGH POINTS ON 3RD AXIS.  INPTS =',I7)
      END
      SUBROUTINE ADDWT (INLUN, IOTLUN, IOTLU2, DATA1, DATA2, IERR)
C-----------------------------------------------------------------------
C   Program ADDWT will add the weighted planes of the input image to
C   the proper plane of the output image.
C   INPUTS:  INLUN    I   The logical unit number of the old image.
C            IOTLUN   I   The input logical unit no. of the new image.
C            IOTLU2   I   The output logical unit no. of the new image.
C            BLC      R(7)   bottom left corner of input image.
C            TRC      R(7)   top right corner of input image.
C            DATA1    R(?)   I/O buffer big enough for one map row.
C            DATA2    R(?)   I/O buffer big enough for one map row.
C   OUTPUTS: IERR     I   error code. 0 = ok.
C-----------------------------------------------------------------------
      REAL      DATA1(*), DATA2(*)
      INTEGER   INLUN, IOTLUN, IOTLU2, IERR
C
      REAL      FACTOR(3), ZBLC(7), ZTRC(7), RFAC(3), RSUM(3), XMIN,
     *   XMAX, XXMIN, XXMAX
      INTEGER   IN3AX, IPLAN0, IPLANE, IEND, ICOLOR, IBEG, IND
      LOGICAL   LBLK
      CHARACTER BLANKS*8
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'RGBMP.INC'
      DATA BLANKS /' '/
C-----------------------------------------------------------------------
C                                       Initialize ZBLC & ZTRC to zero.
      CALL RFILL (7, 0.0, ZBLC)
      CALL RFILL (7, 0.0, ZTRC)
C                                       Recover initial max, min.
      XMAX = -1.0E20
      XMIN = +1.0E20
C                                       Add weighted maps.
      IEND = TRC(3) + 0.5
      IBEG = BLC(3) + 0.5
      IN3AX = IEND - IBEG + 1
      RSUM(1) = 0.0
      RSUM(2) = 0.0
      RSUM(3) = 0.0
      CALL RFILL (3, 1.0, RFAC)
      DO 20 IPLANE = IBEG,IEND
         IPLAN0 = IPLANE - IBEG + 1
         CALL FIXFAC (IPLAN0, IN3AX, RFAC, FACTOR)
         RSUM(1) = RSUM(1) + FACTOR(1)
         RSUM(2) = RSUM(2) + FACTOR(2)
         RSUM(3) = RSUM(3) + FACTOR(3)
 20      CONTINUE
      IF (RSUM(1).GT.0.0) RFAC(1) = 1.0 / RSUM(1)
      IF (RSUM(2).GT.0.0) RFAC(2) = 1.0 / RSUM(2)
      IF (RSUM(3).GT.0.0) RFAC(3) = 1.0 / RSUM(3)
      DO 400 IPLANE = IBEG, IEND
C                                       a forward axis goes from blue to
C                                       red to swap since red is 1st out
         IPLAN0 = IPLANE - IBEG + 1
         IF (FORWRD) IPLAN0 = IN3AX + 1 - IPLAN0
         CALL FIXFAC (IPLAN0, IN3AX, RFAC, FACTOR)
         DO 100 ICOLOR = 1,3
C                                       Add weighted plane to out map.
            IF (FACTOR(ICOLOR).GT.0.0) THEN
               BLC(3) = IPLANE
               TRC(3) = IPLANE
               CALL MAPWIN (INLUN, BLC, TRC, IERR)
               IF (IERR.NE.0) GO TO 970
               ZBLC(3) = ICOLOR
               ZTRC(3) = ICOLOR
               CALL MAPWIN (IOTLUN, ZBLC, ZTRC, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL MAPWIN (IOTLU2, ZBLC, ZTRC, IERR)
               IF (IERR.NE.0) GO TO 970
               CALL MADDX (INLUN, IOTLUN, IOTLU2, FACTOR(ICOLOR), DATA1,
     *            DATA2, DATA2, XXMIN, XXMAX, LBLK, IERR)
               IF (IERR.NE.0) GO TO 970
               IF (LBLK) CATR(KRBLK) = FBLANK
               XMIN = MIN (XMIN, XXMIN)
               XMAX = MAX (XMAX, XXMAX)
               END IF
 100        CONTINUE
 400     CONTINUE
C                                       Update max, min, save header.
      CATR(KRDMN) = XMIN
      CATR(KRDMX) = XMAX
      IND = KHCTP + 2 * 2
      CALL CHR2H (8, 'RGB     ', 1, CATH(IND))
      CATR(KRCIC+2) = 1.0
      CATR(KRCRP+2) = 1.0
      CATD(KDCRV+2) = 1.0D0
C                                       units trashed sometimes
      IF ((OPTYPE.EQ.'CLIS') .OR. (OPTYPE.EQ.'OPTD') .OR.
     *   (OPTYPE.EQ.'POPT')) CALL CHR2H (8, BLANKS, 1, CATH(KHBUN))
      CALL SAVHDR (IOTLUN, CATBLK, IERR)
      BLC(3) = IBEG - 1
      TRC(3) = IEND + 1
      GO TO 999
C                                       error handling.
 970  CONTINUE
      CALL PRTERR (IERR, INLUN)
 999  RETURN
      END
      SUBROUTINE FIXFAC (IPLANE, INODIM, SFACT, FACTOR)
C-----------------------------------------------------------------------
C   Given the plane and number of dimensions, this routine will calc
C   the weighting factors for the red, green, and blue contribution.
C   Inputs:
C      IPLANE   I      The plane of the input image.
C      INODIM   I      The number of planes in the 3rd dimension
C      SFACT    R(3)   scale all weights by SFACT
C   Output:
C      FACTOR   R(3)   the weighting factors for the blue, green, and
C                      red contribution (in that order) of this plane.
C-----------------------------------------------------------------------
      INTEGER   IPLANE, INODIM
      REAL      FACTOR(3), SFACT(3)
C
      REAL      XMID
C-----------------------------------------------------------------------
      XMID = (INODIM - 1.0) / 2.0
C                                       Red and green contribution.
      IF (IPLANE*2.LE.INODIM) THEN
         FACTOR(1) = (XMID - IPLANE + 1.0) / XMID
         FACTOR(2) = SFACT(2) * (1.0 - FACTOR(1))
         FACTOR(1) = FACTOR(1) * SFACT(1)
         FACTOR(3) = 0.0
C                                       Green and blue contribution.
      ELSE
         FACTOR(1) = 0.0
         FACTOR(3) = (IPLANE - XMID - 1.0) / XMID
         FACTOR(2) = SFACT(2) * (1.0 - FACTOR(3))
         FACTOR(3) = SFACT(3) * FACTOR(3)
         END IF
C
 999  RETURN
      END
      SUBROUTINE HISTRY (PRGNAM, INLUN, IOTLUN, IHINL, IHOTL, IDATA,
     *   IWORK)
C-----------------------------------------------------------------------
C  HISTRY will create and write the history file for the new image.
C   Inputs:
C      PRGNAM   C*6      program name.
C      INLUN    I        the LUN for the input map.
C      IOTLUN   I        the LUN for the output map.
C      IHINL    I        the LUN to use for the input history file.
C      IHOTL    I        the LUN to use for the new output history file.
C      IDATA    I(256)   History I/O buffer.
C      IWORK    I(256)   work buffer.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INLUN, IOTLUN, IHINL, IHOTL, IDATA(*), IWORK(*)
C
      CHARACTER NAMSTR*36, HILINE*72, NAME*12, CLASS*6, PTYPE*2
      INTEGER   SEQ, VOL, USID, IBLC(7), ITRC(7), INPTR, IOPTR, IPTR,
     *   IERR, I, IERR2
      LOGICAL   UPDATE
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'RGBMP.INC'
      DATA UPDATE /.TRUE./
C-----------------------------------------------------------------------
C                                       Get additional data from commo
      INPTR = 0
      IOPTR = 0
      DO 20 IPTR = 1,EFIL
         IF (INLUN.EQ.FILTAB(POLUN,IPTR)) INPTR = IPTR
         IF (IOTLUN.EQ.FILTAB(POLUN,IPTR)) IOPTR = IPTR
 20      CONTINUE
C                                       Check for file not open error.
      IF ((INPTR.EQ.0) .OR. (IOPTR.EQ.0)) GO TO 330
C                                       copy some header keywords
      CALL KEYPCP (FILTAB(POVOL,INPTR), FILTAB(POCAT,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,IOPTR), 0, ' ', IERR)
C                                       Create history file and
C                                       copy HI of INSEQ
      CALL HIINIT (2)
      CALL HISCOP (IHINL, IHOTL, FILTAB(POVOL,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,INPTR), FILTAB(POCAT,IOPTR),
     *   CATBLK, IWORK, IDATA, IERR)
      IF (IERR.GT.2) GO TO 320
C                                       Add RGBMP history.
C                                       Input map.
      CALL GTNAME (INLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 320
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCO1 (PRGNAM, NAME, CLASS, SEQ, VOL, IHOTL, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
C                                       Output map.
      CALL GTNAME (IOTLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 320
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCOO (PRGNAM, NAME, CLASS, SEQ, VOL, IHOTL, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
C                                       BLC, TRC
      DO 40 I = 1,7
         IBLC(I) = BLC(I) + .5
         ITRC(I) = TRC(I) + .5
 40      CONTINUE
      WRITE (HILINE,1040) PRGNAM, 'BLC', IBLC
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
      WRITE (HILINE,1040) PRGNAM, 'TRC', ITRC
      CALL HIADD (IHOTL, HILINE, IDATA, IERR)
      IF (IERR.NE.0) GO TO 320
C                                       Experimental algorithms
      IF ((OPTYPE.EQ.'CLIP') .OR. (OPTYPE.EQ.'CLIS') .OR.
     *   (OPTYPE.EQ.'OPTD') .OR. (OPTYPE.EQ.'OPT2') .OR.
     *   (OPTYPE.EQ.'POPT') .OR. (OPTYPE.EQ.'POP2')) THEN
         WRITE (HILINE,1050) PRGNAM, OPTYPE
         CALL HIADD (IHOTL, HILINE, IDATA, IERR)
         IF (IERR.NE.0) GO TO 320
         WRITE (HILINE,1051) PRGNAM, RANGE
         CALL HIADD (IHOTL, HILINE, IDATA, IERR)
         IF (IERR.NE.0) GO TO 320
         IF (OPTYPE.NE.'CLIP') THEN
            IF ((FUNCTY.NE.'SQ') .AND. (FUNCTY.NE.'LG') .AND.
     *         (FUNCTY.NE.'L2')) FUNCTY = 'LN'
            WRITE (HILINE,1052) PRGNAM, FUNCTY
            CALL HIADD (IHOTL, HILINE, IDATA, IERR)
            IF (IERR.NE.0) GO TO 320
            IF (OPTYPE.NE.'CLIS') THEN
               WRITE (HILINE,1053) PRGNAM, TFACT
               CALL HIADD (IHOTL, HILINE, IDATA, IERR)
               IF (IERR.NE.0) GO TO 320
               END IF
            END IF
         END IF
C                                       Close HI file
 320  CALL HICLOS (IHOTL, UPDATE, IDATA, IERR2)
      IF ((IERR.EQ.0) .AND. (IERR2.EQ.0)) GO TO 999
C                                       Report any IERRs
 330     WRITE (MSGTXT,1330)
         CALL MSGWRT (7)
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT (A6,A3,' =',7I6)
 1050 FORMAT (A6,'OPTYPE = ''',A4,'''   / Summing operation type')
 1051 FORMAT (A6,'PIXRANGE =',2(1PE12.4),'  / clip limits')
 1052 FORMAT (A6,'FUNCTYPE =''',A2,'''   / transfer function')
 1053 FORMAT (A6,'FACTOR =',F6.2,'   / optical depth scale factor')
 1330 FORMAT ('WARNING. ERROR WRITING HISTORY FILE.')
      END
      SUBROUTINE MADDX (INLUN1, INLUN2, OUTLUN, FACT1, DATA1, DATA2,
     *   DATA3, XMIN, XMAX, BLANK, IERR)
C-----------------------------------------------------------------------
C   MADDX (for use with WAWA IO only) will add one windowed section of
C   an open map file to another windowed section of an open map file and
C   put the results into another open windowed file.  The second input
C   map may be the same as the output map (opened under a different LUN)
C   if the windows do not overlap or overlap exactly.  MADDX differs
C   from MADD in that blanks are treated as 0.0, not as output blanks.
C   THIS PROGRAM USES COMMON MAPHDR AS A WORK BUFFER.
C   Inputs:
C      INLUN1   I      input map 1 LUN.
C      INLUN2   I      input map 2 LUN.
C      OUTLUN   I      output map LUN.
C      FACT1    R      multiplication factor for map 1.
C      DATA1    R(?)   IO buffer big enough for 1 row.
C      DATA2    R(?)   IO buffer big enough for 1 row.
C      DATA3    R(?)   IO buffer big enough for 1 row.  If input map two
C                      is the same as the output map, and the windows
C                      overlap exactly, this buffer can be the same as
C                      DATA2.
C   Outputs:
C      XMIN     R      map min after scaling of windowed output map.
C      XMAX     R      map max after scaling of windowed output map.
C      BLANK    L      True if magic value blank pixel found else false
C      IERR     I      standard WAWA IO error code.
C-----------------------------------------------------------------------
      REAL      FACT1, DATA1(*), DATA2(*), DATA3(*), XMIN, XMAX
      INTEGER   INLUN1, INLUN2, OUTLUN, IERR
      LOGICAL   BLANK
C
      CHARACTER DCH*12
      REAL      D0
      INTEGER   IBLC1(7), ITRC1(7), IBLC2(7), ITRC2(7), IBLC3(7),
     *   ITRC3(7), INODI1, INODI2, INODI3, INVOL2, I7, I6, I5, I4, I3,
     *   I2, I1, IPIXS1, IPIXS2, IPIXS3, IPIXS4, IPIXS5, IPIXS6, IPIXS7,
     *   IDMAX, IDIM, INSLT2, OUTSLT, OUTVOL, IPTR
      LOGICAL   DOCLR, T, F
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./, F /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize XMIN and XMAX
      XMIN = 1.0E30
      XMAX = - XMIN
      BLANK = F
C                                       Get window information
      CALL GETWIN (INLUN1, INODI1, IBLC1, ITRC1, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETWIN (INLUN2, INODI2, IBLC2, ITRC2, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETWIN (OUTLUN, INODI3, IBLC3, ITRC3, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Verify windows match.
      IDMAX = MAX (INODI1, INODI2)
      IDMAX = MAX (IDMAX, INODI3)
      DO 20 IDIM = 1,IDMAX
         IF (((ITRC1(IDIM) - IBLC1(IDIM)).NE.(ITRC2(IDIM) - IBLC2(IDIM))
     *      .AND. ((ITRC1(IDIM) - IBLC1(IDIM))).NE.(ITRC3(IDIM) -
     *      IBLC3(IDIM))))
     *      GO TO 980
 20      CONTINUE
C                                       Figure out if IN/OUT file.
      DO 70 IPTR = 1,EFIL
         IF (FILTAB(POLUN,IPTR).EQ.INLUN2) THEN
            INSLT2 = FILTAB(POCAT,IPTR)
            INVOL2 = FILTAB(POVOL,IPTR)
            END IF
         IF (FILTAB(POLUN,IPTR).EQ.OUTLUN) THEN
            OUTSLT = FILTAB(POCAT,IPTR)
            OUTVOL = FILTAB(POVOL,IPTR)
            END IF
 70      CONTINUE
C                                       If in equals out we must clear
C                                       READ later on.
      DOCLR = F
      IF ((OUTVOL.EQ.INVOL2) .AND. (OUTSLT.EQ.INSLT2)) DOCLR = T
C                                       Do add.
      IPIXS1 = ITRC1(1) - IBLC1(1) + 1
      IPIXS2 = ITRC1(2) - IBLC1(2) + 1
      IPIXS3 = ITRC1(3) - IBLC1(3) + 1
      IPIXS4 = ITRC1(4) - IBLC1(4) + 1
      IPIXS5 = ITRC1(5) - IBLC1(5) + 1
      IPIXS6 = ITRC1(6) - IBLC1(6) + 1
      IPIXS7 = ITRC1(7) - IBLC1(7) + 1
      DO 700 I7 = 1,IPIXS7
      DO 600 I6 = 1,IPIXS6
      DO 500 I5 = 1,IPIXS5
      DO 400 I4 = 1,IPIXS4
      DO 300 I3 = 1,IPIXS3
      DO 200 I2 = 1,IPIXS2
         CALL MAPIO ('READ', INLUN1, DATA1, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL MAPIO ('READ', INLUN2, DATA2, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       If IN2 = OUT we must clear this
C                                       READ so we can set WRIT status.
         IF (DOCLR) THEN
            CALL CATDIR ('CSTA', INVOL2, INSLT2, DCH, DCH, D0, DCH, D0,
     *         'CLRD', CATBLK, IERR)
            DOCLR = F
            END IF
         CALL RGBSET (IPIXS1, FACT1, DATA1, DATA2, DATA3)
         DO 100 I1 = 1,IPIXS1
            IF (DATA3(I1).EQ.FBLANK) THEN
               BLANK = T
            ELSE
               XMIN = MIN (XMIN, DATA3(I1))
               XMAX = MAX (XMAX, DATA3(I1))
               END IF
 100        CONTINUE
         CALL MAPIO ('WRIT', OUTLUN, DATA3, IERR)
         IF (IERR.NE.0) GO TO 999
 200     CONTINUE
 300     CONTINUE
 400     CONTINUE
 500     CONTINUE
 600     CONTINUE
 700     CONTINUE
      GO TO 999
C                                       Window mis-match error.
 980  IERR = 16
C
 999  RETURN
      END
      SUBROUTINE RGBSET (NX, FACT1, DATA1, DATA2, DATA3)
C-----------------------------------------------------------------------
C   Computes the new sum row from the new and current sum rows
C   Inputs:
C      NX      I      Number points in row
C      FACT1   R      Factor applied to 1
C      DATA1   R(*)   New row in
C      DATA2   R(*)   Current sum row
C   Output:
C      DATA3   R(*)   New sum row
C-----------------------------------------------------------------------
      INTEGER   NX
      REAL      FACT1, DATA1(*), DATA2(*), DATA3(*)
C
      INTEGER   I
      REAL      X, T
      INCLUDE 'RGBMP.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (OPTYPE.EQ.'CLIP') THEN
         DO 10 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               IF ((RANGE(1).GT.0.0) .AND. (DATA1(I).LT.RANGE(1))) THEN
                  T = 0.0
               ELSE
                  T = MAX (RANGE(1), MIN (RANGE(2), DATA1(I)))
                  END IF
               X = FACT1 * T
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)
               DATA3(I) = X
               END IF
 10         CONTINUE
      ELSE IF (OPTYPE.EQ.'OPTD') THEN
         DO 100 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               T = (DATA1(I) - RANGE(1)) / (RANGE(2) - RANGE(1))
               T = MAX (0.0, MIN (1.0, T))
               IF (FUNCTY.EQ.'SQ') THEN
                  T = SQRT (T)
               ELSE IF (FUNCTY.EQ.'LG') THEN
                  T = LOG10 (9.0 * T + 1.0)
               ELSE IF (FUNCTY.EQ.'L2') THEN
                  T = 0.5 * LOG10 (99.0 * T + 1.0)
                  END IF
               X = FACT1 * T * (1.0 - EXP (-TFACT * T))
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)*EXP (-TFACT * T)
               DATA3(I) = X
               END IF
 100        CONTINUE
      ELSE IF (OPTYPE.EQ.'CLIS') THEN
         DO 20 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               T = (DATA1(I) - RANGE(1)) / (RANGE(2) - RANGE(1))
               T = MAX (0.0, MIN (1.0, T))
               IF (FUNCTY.EQ.'SQ') THEN
                  T = SQRT (T)
               ELSE IF (FUNCTY.EQ.'LG') THEN
                  T = LOG10 (9.0 * T + 1.0)
               ELSE IF (FUNCTY.EQ.'L2') THEN
                  T = 0.5 * LOG10 (99.0 * T + 1.0)
                  END IF
               X = FACT1 * T
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)
               DATA3(I) = X
               END IF
 20         CONTINUE
      ELSE IF (OPTYPE.EQ.'OPT2') THEN
         DO 110 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               T = (DATA1(I) - RANGE(1)) / (RANGE(2) - RANGE(1))
               T = MAX (0.0, MIN (1.0, T))
               IF (FUNCTY.EQ.'SQ') THEN
                  T = SQRT (T)
               ELSE IF (FUNCTY.EQ.'LG') THEN
                  T = LOG10 (9.0 * T + 1.0)
               ELSE IF (FUNCTY.EQ.'L2') THEN
                  T = 0.5 * LOG10 (99.0 * T + 1.0)
                  END IF
               X = FACT1 * DATA1(I) * (1.0 - EXP (-TFACT * T))
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)*EXP (-TFACT * T)
               DATA3(I) = X
               END IF
 110        CONTINUE
      ELSE IF (OPTYPE.EQ.'POPT') THEN
         DO 200 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               T = (DATA1(I) - RANGE(1)) / (RANGE(2) - RANGE(1))
               T = MAX (0.0, MIN (1.0, T))
               IF (FUNCTY.EQ.'SQ') THEN
                  T = SQRT (T)
               ELSE IF (FUNCTY.EQ.'LG') THEN
                  T = LOG10 (9.0 * T + 1.0)
               ELSE IF (FUNCTY.EQ.'L2') THEN
                  T = 0.5 * LOG10 (99.0 * T + 1.0)
                  END IF
               X = FACT1 * T * (1.0 - EXP (-TFACT * T))
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)
               DATA3(I) = X
               END IF
 200        CONTINUE
      ELSE IF (OPTYPE.EQ.'POP2') THEN
         DO 210 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               T = (DATA1(I) - RANGE(1)) / (RANGE(2) - RANGE(1))
               T = MAX (0.0, MIN (1.0, T))
               IF (FUNCTY.EQ.'SQ') THEN
                  T = SQRT (T)
               ELSE IF (FUNCTY.EQ.'LG') THEN
                  T = LOG10 (9.0 * T + 1.0)
               ELSE IF (FUNCTY.EQ.'L2') THEN
                  T = 0.5 * LOG10 (99.0 * T + 1.0)
                  END IF
               X = FACT1 * DATA1(I) * (1.0 - EXP (-TFACT * T))
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)
               DATA3(I) = X
               END IF
 210        CONTINUE
      ELSE
         DO 300 I = 1,NX
            IF (DATA1(I).EQ.FBLANK) THEN
               DATA3(I) = DATA2(I)
            ELSE
               X = FACT1 * DATA1(I)
               IF (DATA2(I).NE.FBLANK) X = X + DATA2(I)
               DATA3(I) = X
               END IF
 300        CONTINUE
         END IF
C
 999  RETURN
      END
