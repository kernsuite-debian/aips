      SUBROUTINE COMLAB (BLC, TRC, LTYPE, IVER, YGAP, CH, XMULT, XLEVS,
     *   XYR, IBUFF, IERR)
C-----------------------------------------------------------------------
C! initializes line drawing and labels plot with text, contour levels
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2002, 2014
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
C   COMLAB is an axis drawing and labelling routine for use with the
C   common labeling for contour plots and pol vector plots.  It calls
C   GINITL and puts subsidiary labels (source, frequency, Stokes, image
C   name), (peak flux), (contour levels) in file.
C   Inputs:
C      BLC      R(7)   bottom left hand corner of the map.
C      TRC      R(7)   top right hand corner of the map.
C      LTYPE    I      label type: 1 none, 2 no ticks, 3 Ra/dec
C                      4 center relative, 5 subimg center-rel,
C                      6 pixels, 7-9 as 3-5 with only tick labels
C                      < 0 => no date/time, else as positive
C      IVER     I      plot file version number
C      XMULT    R      the multiplier for the LEVS to find the
C                      actual contour levels.
C      XLEVS    R(30)  the contour levels (when used with XMULT)
C   In/out:
C      CH       R(4)   Number of character outside plot on left, bot,
C                      top, right.  Input - extra (NOT INCLUDING LEVS)
C                      from task alone, output - including standard
C      IBUFF    I(256) the updated graphics output buffer.
C      YGAP     R      On input: # lines at bottom to skip before
C                      peak flux line in addition to standard
C                      On output: includes standard
C   Output:
C      IERR     I      error indicator: 0 = No error.
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), YGAP, CH(4), XMULT, XLEVS(30), XYR
      INTEGER   LTYPE, IVER, IBUFF(256), IERR
C
      INTEGER   I, IANGL, INCHAR, IDEPTH(5), NTEXT, IT(3), ID(3), ITEMP,
     *   NL, IXL, IROUND, I2, LLTYPE
      REAL      DCX, DCY, RGAP, TEMP, Y0, Y1, X0, X1
      LOGICAL   SLICE
      CHARACTER SPRTXT*80, ATIME*8, ADATE*12, CHTEMP*20, EXTTXT(2)*80,
     *   SUBR*6, LTEXT(5)*80
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (SPRTXT, EXTTXT(1))
      DATA SLICE /.FALSE./
C-----------------------------------------------------------------------
      CALL CHECKL ('COMLAB')
      LLTYPE = MOD (ABS(LTYPE), 100)
C                                       Initial values.
      X0 = BLC(1)
      X1 = TRC(1)
      Y0 = BLC(2)
      Y1 = TRC(2)
      IDEPTH(1) = BLC(3) + .01
      IDEPTH(2) = BLC(4) + .01
      IDEPTH(3) = BLC(5) + .01
      IDEPTH(4) = BLC(6) + .01
      IDEPTH(5) = BLC(7) + .01
      RGAP = YGAP
      CALL LABINI (BLC, TRC, IDEPTH, CH, LTYPE, SLICE, YGAP, EXTTXT,
     *   NTEXT)
C                                       Prepare LEVS lines
      NL = 0
      IF (LLTYPE.LT.7) THEN
         NL = NL + 1
         WRITE (LTEXT(NL),1040) XMULT
         INCHAR = 22
         DO 10 I = 1,30
            I2 = 11
            IXL = IROUND (XLEVS(I))
            IF (ABS(IXL-XLEVS(I)).GT.0.0001) THEN
               IF ((XLEVS(I).GE.-99.90) .AND. (XLEVS(I).LE.999.90))
     *            I2 = 13
               IF ((XLEVS(I).GE.-9.990) .AND. (XLEVS(I).LE.99.990))
     *            I2 = 14
               IF ((XLEVS(I).GE.-0.9990) .AND. (XLEVS(I).LE.9.9990))
     *            I2 = 15
               DCX = 10.0 ** (I2-12)
               IXL = IROUND (XLEVS(I) * DCX)
               TEMP = IXL / DCX
            ELSE
               TEMP = IXL
               END IF
            WRITE (CHTEMP,1041,ERR=5) TEMP
 5          IF (CHTEMP(10:11).EQ. ' -') CHTEMP(10:11) = '-0'
            IF (CHTEMP(10:11).EQ. '  ') CHTEMP(10:11) = ' 0'
            CHTEMP(I2+1:) = ' '
            CALL CHTRIM (CHTEMP, 20, CHTEMP, IXL)
            LTEXT(NL)(INCHAR:) = CHTEMP(:IXL) // ', '
            INCHAR = INCHAR + 2 + IXL
C                                       Print out this line.
            IF (I.EQ.30) GO TO 15
            IF (XLEVS(I+1).LE.XLEVS(I)) GO TO 15
            IF (INCHAR.GE.50) THEN
               INCHAR = 1
               NL = NL + 1
               END IF
 10         CONTINUE
 15      INCHAR = INCHAR - 2
         LTEXT(NL)(INCHAR:INCHAR) = ')'
         CH(2) = CH(2) + (NL + 1) * 1.333
         END IF
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYR, CH, IDEPTH, IBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Set line type
      CALL GLTYPE (1, IBUFF, IERR)
      SUBR = 'GLTYPE'
      IF (IERR.NE.0) GO TO 980
C                                       Draw borders.
      CALL GPOS (X0, Y0, IBUFF, IERR)
      SUBR = 'GPOS'
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (X1, Y0, IBUFF, IERR)
      SUBR = 'GVEC'
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (X1, Y1, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (X0, Y1, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GVEC (X0, Y0, IBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (LLTYPE.EQ.1) GO TO 999
C                                       Center-rel: true center pos
      IF ((NTEXT.GT.0) .AND. (LLTYPE.LE.6)) THEN
         DCX = 0.0
         IANGL = 0
         DO 20 I = 1,NTEXT
            CALL GPOS (X0, Y0, IBUFF, IERR)
            SUBR = 'GPOS'
            IF (IERR.NE.0) GO TO 980
            DCY = -YGAP
            CALL CHTRIM (EXTTXT(I), 80, EXTTXT(I), INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, EXTTXT(I), IBUFF, IERR)
            SUBR = 'GCHAR'
            IF (IERR.NE.0) GO TO 980
            YGAP = YGAP + 1.333
 20         CONTINUE
         END IF
      YGAP = YGAP + RGAP
C                                       Source name, stokes, freq.
      IF (LLTYPE.LE.6) THEN
         CALL GPOS (X0, Y1, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         DCX = 0.0
         DCY = 0.5
         IANGL = 0
         CALL H2CHR (8, 1, CATH(KHOBJ), SPRTXT)
         INCHAR = 11
         IF (SPRTXT.EQ.' ') INCHAR = 1
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            SPRTXT(INCHAR-1:INCHAR-1) = '_'
            SPRTXT(INCHAR:) = SAXLAB(1,LOCNUM)(:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            SPRTXT(INCHAR-1:INCHAR-1) = '_'
            SPRTXT(INCHAR:) = SAXLAB(2,LOCNUM)(:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
C                                       image name
         CHTEMP = ' '
         IF (INCHAR.GT.1) SPRTXT(INCHAR-1:INCHAR-1) = '_'
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), CHTEMP(1:12))
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTEMP(13:18))
         CALL NAMEST (CHTEMP, CATBLK(KIIMS), SPRTXT(INCHAR:), ITEMP)
         CALL REFRMT (SPRTXT, '_', INCHAR)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       Date/time version
      IF ((LTYPE.GT.1) .AND. (LLTYPE.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (SPRTXT,1020) IVER, ADATE, ATIME
         CALL REFRMT (SPRTXT, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GPOS (X0, Y1, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       Peak flux.
      IF (LLTYPE.LT.7) THEN
         DCX = 0.0
         DCY = -YGAP
         IANGL = 0
         INCHAR = 32
         CALL GPOS (X0, Y0, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
         CALL H2CHR (8, 1, CATH(KHBUN), CHTEMP)
         TEMP = CATR(KRDMX)
         IF (ABS(CATR(KRDMX)).LT.ABS(CATR(KRDMN))) TEMP = CATR(KRDMN)
         WRITE (SPRTXT,1030) TEMP, CHTEMP(1:8)
         CALL GCHAR (INCHAR, IANGL, DCX, DCY, SPRTXT, IBUFF, IERR)
         SUBR = 'GCHAR'
         IF (IERR.NE.0) GO TO 980
C                                       Write levels.
         DCX = 0.0
         DCY = - YGAP - 1.333
         IANGL = 0
         DO 50 I = 1,NL
            SUBR = 'GPOS'
            CALL GPOS (X0, Y0, IBUFF, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL REFRMT (LTEXT(I), ' ', INCHAR)
            CALL GCHAR (INCHAR, IANGL, DCX, DCY, LTEXT(I), IBUFF, IERR)
            SUBR = 'GCHAR'
            IF (IERR.NE.0) GO TO 980
            DCY = DCY - 1.333
 50         CONTINUE
         END IF
      IF (IERR.EQ.0) GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR, SUBR
      CALL MSGWRT (7)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GRAPH FILE INITIALIZATION ERROR. GINITL ERR =',I5)
 1020 FORMAT ('PLot file version',I4,'__created ',A12,A8)
 1030 FORMAT ('Peak flux =',1PE12.4,1X,A8)
 1040 FORMAT ('Levs =',1PE11.3,' * (')
 1041 FORMAT (F15.3)
 1980 FORMAT ('COMLAB: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
      END
