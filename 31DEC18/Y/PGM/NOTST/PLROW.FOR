LOCAL INCLUDE 'PLROW.INC'
      INTEGER   NTEXT, NTEXT2, PCODES(5)
      HOLLERITH XNAMIN(3), XCLSIN(2)
      CHARACTER NAMIN*12, CLSIN*6, TEXT(2)*80, TEXT2(2)*80, TITLE*80,
     *   XUNIT*8, YUNIT*8
      REAL      PRUSER, SEQIN, DSKIN, BLC(7), TRC(7), PIXRNG(2), YINC,
     *    ZXRATO, XLTYPE, CHOUT(4), YGAP, XDOTV, XGRCH
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, SEQIN, DSKIN, BLC, TRC,
     *   PIXRNG, YINC, ZXRATO, XLTYPE, XDOTV, XGRCH
      COMMON /PLTLAB/ CHOUT, YGAP, NTEXT, PCODES, NTEXT2
      COMMON /CHRCOM/ NAMIN, CLSIN, TEXT, TEXT2, TITLE, XUNIT, YUNIT
LOCAL END
      PROGRAM PLROW
C-----------------------------------------------------------------------
C! Ruled surface plot of an image.
C# Plot-appl Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2009, 2014-2016
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
C   This program will plot all the rows of a map subimage.  The X axis
C   will be the X axis of the map, the Y axis will be the intensity
C   of the map pixels. Each plot of a row will be offset from the
C   previous row by a constant intensity value.
C   INPUTS:   (from AIPS)
C       USERID   R      user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C       INNAME   R(3)   name of primary file.
C       INCLASS  R(2)   class of primary file.
C       INSEQ    R      sequence number of primary file.
C       NDISK    R      disk volume number. 0 means try all.
C       BLC      R(7)   the starting coordinates for the slice
C                       BLC(1) is the X coordinate and
C                       BLC(2) is the Y coordinate.  The first
C                       coordinate in the input image is (1,1).
C       TRC      R(7)   the ending coordinate for the slice.
C       PIXRANGE R(2)   Sets the upper and lower limit on intensity of
C                       values to appear in the slice.  0, 0
C                       defaults to range in map header.
C       YINC     R      Plot every YINCth row.
C       OFFSET   R      The separation of the rows as a fraction of the
C                       maximum intensity range.  That is, a 0 value
C                       will plot all rows on the same Y axis scale (no
C                       separation between rows). A very large value
C                       will print widely spaced straight lines.
C                       0 => 0.25 PIXRANGE.
C       LTYPE    R      Label type of X axis.
C       DOTV     R      > 0 => TV, else plot file
C       GRCHAN   R      graphics channel to use
C-----------------------------------------------------------------------
      INTEGER   IROUND, IMLUN, NPARMS, IERR, IYINC, IDEBUG, IPTYPE, I
      CHARACTER PRGNAM*6
      INCLUDE 'PLROW.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      DATA IMLUN /16/
      DATA PRGNAM /'PLROW '/
C-----------------------------------------------------------------------
      IBLKSZ = MABFSS
      GPHIND = 0
C                                       Delete plot file on error
      IDEBUG = 0
      NPARMS = 29
      IPTYPE = 51
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PLRINI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Label type from user input.
      PCODES(1) = IROUND (XLTYPE)
      I = MOD (ABS(PCODES(1)), 100)
      IF ((I.EQ.0) .OR. (I.GT.10)) THEN
         IF (PCODES(1).GE.0) THEN
            PCODES(1) = (PCODES(1)/100)*100 + 3
         ELSE
            PCODES(1) = (PCODES(1)/100)*100 - 3
            END IF
         END IF
      XLTYPE = PCODES(1)
C                                       Use "standard" labeling for
C                                       X axis based on map header.
      PCODES(2) = 0
C                                       Use intensity value in header
C                                       for Y axis label.
      PCODES(3) = 0
C                                       Use "standard" title line
      PCODES(4) = 0
C                                       Not now used
      PCODES(5) = 0
C                                       Do plotting.
      IYINC = YINC + .5
      IYINC = MAX (1, IYINC)
      YINC = IYINC
      CALL PLRTOR (BLC, TRC, IYINC, ZXRATO, IERR)
C                                       Shutdown.
 900  CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE PLRINI (PRGNAM, NPARMS, IMLUN, IPTYPE, IERR)
C-----------------------------------------------------------------------
C   This routine does all the intial set up.  Get parms from AIPS,
C   open the map file, create the plot file and write the plot file
C   records to do the plot labeling.
C   Inputs:
C      PRGNAM C*6    Name of this program.
C      NPARMS I      Number of R   words to get from AIPS.
C      IMLUN  I      The logical unit number to use for the map file.
C      IPTYPE I      Plot file type: 1 misc., 2 CNTR, 3 GREYS, 4 PROFL,
C                    5 SL2PL, 6 PCNTR, 7 IMEAN (hist), 8 UVPLT,
C                    9 GNPLT, 10 VBPLT  Use 1 unless your inputs match
C                    those of these tasks - or take a new number, but
C                    AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, NAME*36, PTYPE*2, STAT*4
      HOLLERITH AMAP
      REAL      TEMP
      INTEGER   IWORK(256), NPARMS, IERR, IMLUN, IPTYPE, I, SEQ, VOL,
     *   USID
      INCLUDE 'PLROW.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Get parms, set IO & hdr vals.
C                                       Get parameters from AIPS, init
C                                       AIPS I/O, other startup things.
      CALL SETUP (PRGNAM, NPARMS, PRUSER, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
      CALL CHR2H (4, 'MA  ', 1, AMAP)
      DOTV = XDOTV.GT.0.0
      PRUSER = NLUSER
C                                       Open the map file.
      CALL MAKNAM  (XNAMIN, XCLSIN, SEQIN, DSKIN, AMAP, PRUSER, NAME)
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL INTMIO (IMLUN, STAT, NAME, BLC, TRC, IBLKSZ, CATBLK,
     *   IMSTUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Force just one plane
      DO 20 I = 3,7
         TRC(I) = BLC(I)
         IMSTUF(16+2*I) = IMSTUF(15+2*I)
 20      CONTINUE
      IMSTUF(32) = BLC(3) + .0001
      IMSTUF(32) = IMSTUF(32) - 1
C                                       Backwards not allowed!!!
C                                       X axis: I/O already fixed
      IF (BLC(1).GE.TRC(1)) THEN
         TEMP = BLC(1)
         BLC(1) = TRC(1)
         TRC(1) = TEMP
         END IF
      IF (BLC(2).GE.TRC(2)) THEN
         TEMP = BLC(2)
         BLC(2) = TRC(2)
         TRC(2) = TEMP
         IMSTUF(19) = BLC(2) + .0001
         IMSTUF(20) = TRC(2) + .9999
         IMSTUF(37) = 1
         IMSTUF(31) = IMSTUF(20)
         END IF
C                                       Find actual range of map values
      CALL RNGSET (PIXRNG, CATR(KRDMX), CATR(KRDMN), RANGE)
C                                       Fill in defaults
      CALL WAWA2A (NAME, NAMIN, CLSIN, SEQ, PTYPE, VOL, USID)
      CALL CHR2H (12, NAMIN, 1, XNAMIN)
      CALL CHR2H (6, CLSIN, 1, XCLSIN)
      SEQIN = SEQ
      DSKIN = VOL
      PIXRNG(1) = RANGE(1)
      PIXRNG(2) = RANGE(2)
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
C                                       Create and open plot file.
      CALL PLMAKE (NPARMS, PRUSER, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLRTOR (BBLC, TTRC, IYINC, ZXRAT, IERR)
C-----------------------------------------------------------------------
C   This routine writes all of the plot commands (including the axis
C   drawing and labeling commands) into the plot file.
C   Output: IERR    I    Error code.  0=ok.
C-----------------------------------------------------------------------
      REAL      BBLC(7), TTRC(7), ZXRAT
      INTEGER   IYINC, IERR
C
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PREFIX*5, CHTEMP*8
      REAL      ROW(MAXIMG), XVAL, YVAL, DXINC, RNGMAX, RNGMIN, XMIN,
     *   XMAX, DXINC2, AROW(256), BROW(MAXIMG), DSCAL(2)
      INTEGER   INOSL, I, II, INOXS, J1, J2, JJ
      LOGICAL   EOF, WORKED, DOWN
      INCLUDE 'PLROW.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DPLT.INC'
C-----------------------------------------------------------------------
C                                       Figure out increment btwn rows.
      DXINC = (RANGE(2) - RANGE(1)) * 0.25
      IF ((ZXRAT.GT.0.0). AND. (ZXRAT.LE.10.*DXINC)) DXINC = ZXRAT
      ZXRAT = DXINC
C                                       Put it on plot.
      NTEXT = 1
      DXINC2 = DXINC
      CALL METSCA (DXINC2, PREFIX, WORKED)
      CALL H2CHR (8, 1, CATH(KHBUN), CHTEMP)
      DXINC2 = DXINC2 * IYINC
      WRITE (TEXT(1),1000) DXINC2, PREFIX, CHTEMP
      CALL REFRMT (TEXT(1), ' ', I)
C                                       Find subimage min and max.
      INOXS = IMSTUF(9)
      RNGMAX = -10000.
      RNGMIN = 10000.
      DO 20 I = 1,32000
         CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (EOF) GO TO 30
         IF (MOD(I-1,IYINC).EQ.0) THEN
            XMIN = RANGE(2)
            XMAX = RANGE(1)
            JJ = 0
            DO 10 II = 1,INOXS
               YVAL = ROW(II)
               IF (YVAL.NE.FBLANK) THEN
                  XMIN = MIN (XMIN, YVAL)
                  XMAX = MAX (XMAX, YVAL)
                  JJ = JJ + 1
                  END IF
 10            CONTINUE
C                                       rescale: set Y pix max,min
            IF (JJ.GT.0) THEN
               XMIN = MAX (XMIN, RANGE(1))
               XMAX = MIN (XMAX, RANGE(2))
               XMIN = IMSTUF(31) + XMIN / DXINC
               XMAX = IMSTUF(31) + XMAX / DXINC
               RNGMAX = MAX (RNGMAX, XMAX)
               RNGMIN = MIN (RNGMIN, XMIN)
               END IF
            END IF
 20      CONTINUE
C
 30   PBLC(2) = MIN (BBLC(2), RNGMIN) - 0.2
      PTRC(2) = MAX (TTRC(2), RNGMAX) + 0.2
      PBLC(1) = BBLC(1) - 0.2
      PTRC(1) = TTRC(1) + 0.2
C                                       Re-init for 2nd read.
      CALL REIMIO (BBLC, TTRC, IBLKSZ, CATBLK, IMSTUF)
C                                       Number of points in slice
C                                       same as the number of pixels
C                                       along X axis.
      INOSL = TTRC(1) - BBLC(1) + 1
C                                       Set up commons for plotting.
      CALL PLINIR (BBLC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLRLAB (IERR)
      IF (IERR.NE.0) GO TO 999
      DSCAL(1) = 1.0 / DXINC
      DSCAL(2) = BBLC(2)
      RNGMIN = RANGE(1) / DXINC + BBLC(2)
      RNGMAX = RANGE(2) / DXINC + BBLC(2)
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Second pass thru the data,
C                                       this time to plot.
C                                       Do it straight
      IF (INOSL.GE.256) THEN
         CALL RFILL (INOSL, PBLC(2), BROW)
         DO 60 I = 1,32000
            CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (EOF) GO TO 999
            IF (MOD(I-1, IYINC).EQ.0) THEN
               DOWN = .FALSE.
C                                       Scale
               DO 34 II = 1,INOXS
                  IF (ROW(II).NE.FBLANK) ROW(II) = ROW(II) * DSCAL(1) +
     *               DSCAL(2)
 34               CONTINUE
               DO 45 II = 1,INOXS
                  XVAL = II - 1.0 + BBLC(1)
                  YVAL = ROW(II)
                  IF (YVAL.EQ.FBLANK) THEN
                     DOWN = .FALSE.
                  ELSE
                     YVAL = MIN (RNGMAX, MAX (RNGMIN, YVAL))
                     IF (YVAL.LT.BROW(II)) THEN
                        DOWN = .FALSE.
                     ELSE
                        BROW(II) = YVAL
                        IF (DOWN) THEN
                           CALL PLVEC (XVAL, YVAL, IERR)
                        ELSE
                           CALL PLPOS (XVAL, YVAL, IERR)
                           ENDIF
                        IF (IERR.NE.0) GO TO 999
                        DOWN = .TRUE.
                        END IF
                     END IF
 45               CONTINUE
               END IF
C                                       Play with DSCAL(2) to get offset
C                                       values we will actually use in
C                                       plot.
            DSCAL(2) = DSCAL(2) + 1.0
            RNGMAX = RNGMAX + 1.0
            RNGMIN = RNGMIN + 1.0
 60         CONTINUE
C                                       Do it with interpolation to 256
      ELSE
         DXINC2 = (TTRC(1) - BBLC(1)) / 255.0
         CALL RFILL (256, PBLC(2), BROW)
         DO 100 I = 1,32000
            CALL GETROW (IMSTUF, IOBLK, ROW, EOF, IERR)
            IF (IERR.NE.0) GO TO 999
            IF (EOF) GO TO 999
            IF (MOD(I-1, IYINC).EQ.0) THEN
C                                       Scale
               DO 74 II = 1,INOXS
                  IF (ROW(II).NE.FBLANK) ROW(II) = ROW(II) * DSCAL(1) +
     *               DSCAL(2)
 74               CONTINUE
C                                       Interpolate
               DO 75 II = 1,256
                  AROW(II) = FBLANK
                  XVAL = (II - 1.0) * DXINC2 + 1.0
                  J1 = XVAL + 0.03
                  J2 = XVAL + 0.97
                  J2 = MIN (INOXS, J2)
                  IF (ROW(J1).NE.FBLANK) THEN
                     IF (J1.EQ.J2) AROW(II) = ROW(J1)
                     IF ((J1.NE.J2) .AND. (ROW(J2).NE.FBLANK)) THEN
                        AROW(II) = (XVAL - J1) * (ROW(J2) - ROW(J1)) +
     *                     ROW(J1)
                        END IF
                     END IF
 75               CONTINUE
               DOWN = .FALSE.
               DO 90 II = 1,256
                  XVAL = (II - 1.0) * DXINC2 + BBLC(1)
                  YVAL = AROW(II)
                  IF (YVAL.EQ.FBLANK) THEN
                     DOWN = .FALSE.
                  ELSE
                     YVAL = MIN (RNGMAX, MAX (RNGMIN, YVAL))
                     IF (YVAL.LT.BROW(II)) THEN
                        DOWN = .FALSE.
                     ELSE
                        BROW(II) = YVAL
                        IF (DOWN) THEN
                           CALL PLVEC (XVAL, YVAL, IERR)
                        ELSE
                           CALL PLPOS (XVAL, YVAL, IERR)
                           END IF
                        IF (IERR.NE.0) GO TO 999
                        DOWN = .TRUE.
                        END IF
                     END IF
 90               CONTINUE
               END IF
C                                       Play with DSCAL(2) to get offset
C                                       values we will actually use in
C                                       plot.
            DSCAL(2) = DSCAL(2) + 1.0
            RNGMAX = RNGMAX + 1.0
            RNGMIN = RNGMIN + 1.0
 100        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Each plotted row is offset by',F8.2,1X,A5,1X,A8)
      END
      SUBROUTINE PLINIR (BBLC, IERR)
C-----------------------------------------------------------------------
C   This routine will set up the location commons for the plot file.
C   Inputs:
C      BBLC    R(7)    Bottom left corner of plane
C   Output:
C      IERR    I       Error code. 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
      REAL      BBLC(7)
      INTEGER   IDEPT(5), I, LABEL, LTYPE
      LOGICAL   SLICE
      INCLUDE 'PLROW.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
      LOCNUM = 1
C                                       Text borders at left, bottom,
C                                       right and top in characters.
      CALL RFILL (4, 0.0, CHOUT)
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(2) = NTEXT *
     *   1.333
C                                       Set up default scaling parms.
      XSCAL = 1.0
      YSCAL = 1.0
      XOFF = 0.0
      YOFF = 0.0
      DO 10 I = 1,5
         IDEPT(I) = BBLC(I+2) + 0.5
 10      CONTINUE
C                                       standard labeling init
      SLICE = .FALSE.
      CALL LABINI (PBLC, PTRC, IDEPT, CHOUT, LABEL, SLICE, YGAP, TEXT2,
     *   NTEXT2)
      IF ((XY.EQ.0.0) .AND. (CATR(KRCIC+1).NE.0.0) .AND.
     *   (AXTYP(LOCNUM).EQ.1)) XY = ABS (CATR(KRCIC) / CATR(KRCIC+1))
      IF (XY.EQ.0.0) XY = 1.0
C                                       Change labeling to XUNIT if
C                                       programmer wants.
      IF (PCODES(2).GT.0) CTYP(1,LOCNUM) = XUNIT
C                                       Fill in units for YUNIT.
      IF (PCODES(3).GT.0) CTYP(2,LOCNUM) = YUNIT
C                                       Write intialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (PBLC, PTRC, XY, CHOUT, IDEPT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLRLAB (IERR)
C-----------------------------------------------------------------------
C   This subroutine uses the values set in PLINIR and passed through
C   commons LOCATI and PLTCOM to do the axes labeling.
C   Inputs: COMMON /LOCATI/
C           COMMON /PLTCOM/
C   Output: IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, CHTM18*18
      INTEGER   IERR
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), ITCHAR, LABEL, LTYPE
      LOGICAL   F
      INCLUDE 'PLROW.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
C                                        Tics and tic labels
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CLAB1 (PBLC, PTRC, CHOUT, LABEL, XY, F, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        draw rectangle
      CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PTRC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PTRC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PBLC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GVEC (PBLC(1), PBLC(2), PLTBLK, IERR)
C                                       title line.
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Standard title
         IF (PCODES(4).EQ.0) THEN
            CALL H2CHR (8, 1, CATH(KHOBJ), TITLE)
            INCHAR = 12
            IF (TITLE.EQ.' ') INCHAR = 1
            IF (NCHLAB(1,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
               TITLE(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
               END IF
            IF (NCHLAB(2,LOCNUM).GT.0) THEN
               IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
               TITLE(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
               INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
               END IF
            IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
            CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTM18(1:12))
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTM18(13:18))
            CALL NAMEST (CHTM18, CATBLK(KIIMS), TITLE(INCHAR:), ITCHAR)
            CALL REFRMT (TITLE, '_', INCHAR)
            END IF
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = .5
         IANGLE = 0
         CALL CHTRIM (TITLE, 80, TITLE, INCHAR)
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, TITLE, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Date/time version
      IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (WRKTXT,1020) IVER, ADATE, ATIME
         CALL REFRMT (WRKTXT, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Label X axis.
      IF ((NTEXT2.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 30 I = 1,NTEXT2
            WRKTXT = TEXT2(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 30         CONTINUE
         END IF
C                                       Text at bottom.
      IF ((NTEXT.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 40 I = 1,NTEXT
            WRKTXT = TEXT(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 40         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLot file version',I4,'__created ',A,A)
      END
