      SUBROUTINE TVSLIN (IBR, ISLUN, ISFIND, LABEL, IMOD, RANGE, IDROP,
     *   BLC, TRC, XBLC, XTRC, RANGE2, XFAC, XOFF, CH, YGAP, TEXT,
     *   NTEXT, IFMOD, INPTS, IERR)
C-----------------------------------------------------------------------
C! initialize parameters for plotting a slice directly on a TK graphics
C# Slice Graphics Plot-util TV
C-----------------------------------------------------------------------
C;  Copyright (C) 2000, 2007, 2009, 2012, 2014, 2016
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
C   Given an open slice file TVSLIN will read the first record, do the
C   necessary scaling and initialization of variables, and update the
C   image catalog header. (Based on TKSLIN)
C   Inputs:
C      IBR     I      Type code: here only use is 8 - 11 plotting model,
C                     else data and 1-2
C      ISLUN   I      logical unit no. of open slice file.
C      ISFIND  I      FTAB index for slice file.
C      LABEL   I      type of labeling: see adverb LTYPE
C      IDROP   I(2)   the number of slice points to omit from the
C                     beginning/end of the plot.
C   In/out:
C      IMOD    I      model no. to plot (IBR = 1-2, 8-11  only). 0 means
C                     latest.
C      RANGE   R(2)   the minimum and maximum values allowed for
C                     the plot.  If RANGE(1) .GE. RANGE(2) then the map
C                     min and max will be used.
C   Output:
C      BLC     R(2)   bottom left corners of slice plot.
C      TRC     R(2)   top right corner of slice plot.
C      XBLC    R(7)   starting pixel coordinate of slice vector.
C      XTRC    R(7)   ending pixel coordinate of slice vector.
C      RANGE2  R(2)   min and max pixel values.
C      XFAC    R      XFAC * real Y value  +  XOFF = rescaled Y value
C                     between 1 and 4000.
C      XOFF    R      see XFAC.
C      CH      R(4)   Number chars to left,bot,right,top of plot
C      YGAP    R      Place to put first line of TEXT
C      TEXT    C(2)*80  Text to add at bot of plot
C      NTEXT   I      Numbers lines used in TEXT (0, 1, 2)
C      IFMOD   I      Block number of 1st gaussian model data.
C      INPTS   I      No. of points for this slice.
C      IERR    I      error code 0= none.
C  Common:
C      /TKSPCL/  SCALEX     R   X scale factor
C                SCALEY     R   Y scale factor.
C      /WORK/    ISBLK(256)     scratch
C      /MAPHDR/  CATBLK in/out  header - converted for image catlg
C-----------------------------------------------------------------------
      CHARACTER TEXT(2)*80
      INTEGER   IBR, ISLUN, ISFIND, LABEL, IMOD, IDROP(2), NTEXT,
     *   IFMOD, INPTS, IERR
      REAL      RANGE(2), BLC(2), TRC(2), XBLC(7), XTRC(7), RANGE2(2),
     *   XFAC, XOFF, CH(4), YGAP
C
      REAL      X, Y, PIXMAX, PIXMIN, RMAX, XBLC1, XBLC2, XTRC1, XTRC2,
     *   XBLC3, XTRC3, XX, XYRATO, XFLUX(2), DX, FQFINC
      INTEGER   I4XTRA, INMOD, ICHB, ICHL, ICHR, ICHT, IX1, IX2, IY1,
     *   IY2, NXA, NYA, IROUND, TVSIZE(2)
      CHARACTER JY*8, OPTYPE*4
      DOUBLE PRECISION DSBLK(128), FQFREQ
      LOGICAL   ISOLD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ISBLK, DSBLK)
      DATA JY /'JY'/
C-----------------------------------------------------------------------
C                                       Read first slice file record
      CALL ZFIO ('READ', ISLUN, ISFIND, 1, ISBLK, IERR)
      IF (IERR.NE.0) GO TO 980
      INPTS = ISBLK(57)
C                                       Get, check g model data.
      IF (((IBR.GE.8) .AND. (IBR.LE.11)) .OR. ((IBR.GE.1) .AND.
     *   (IBR.LE.2))) THEN
         IFMOD = ISBLK(59)
         INMOD = ISBLK(58)
         IF (INMOD.EQ.0) GO TO 950
         IF (IMOD.GT.INMOD) GO TO 950
         IF (IMOD.LE.0) IMOD = INMOD
         END IF
C                                       Read 2nd slice rec (inputs).
      CALL ZFIO ('READ', ISLUN, ISFIND, 2, ISBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       pick up image Blc, Trc; Sl size
      CALL RCOPY (7, RSBLK(19), XBLC)
      CALL RCOPY (7, RSBLK(26), XTRC)
      CALL H2CHR (4, 1, RSBLK(33), OPTYPE)
      IF ((OPTYPE.EQ.'AVER') .OR. (OPTYPE.EQ.'FLUX') .OR.
     *   (OPTYPE.EQ.'LGFL') .OR. (OPTYPE.EQ.'LNFL') .OR.
     *   (OPTYPE.EQ.'ADER') .OR. (OPTYPE.EQ.'FDER') .OR.
     *   (OPTYPE.EQ.'LGAV') .OR. (OPTYPE.EQ.'LNAV')) THEN
         ISOLD = .FALSE.
      ELSE
         ISOLD = .TRUE.
         END IF
      CALL RCOPY (2, RSBLK(34), XFLUX)
      IF (OPTYPE.EQ.'FLUX') CALL CHR2H (8, JY, 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LGFL') CALL CHR2H (8, 'LOG10 JY', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'LNFL') CALL CHR2H (8, 'LN (JY)', 1, CATH(KHBUN))
      IF (OPTYPE.EQ.'FDER') CALL CHR2H (8, 'dJY / dx', 1, CATH(KHBUN))
      IF (ISOLD) THEN
         FQFINC = 0.0
         FQFREQ = 0.0D0
      ELSE
         FQFINC = RSBLK(36)
         FQFREQ = DSBLK(19)
         END IF
C                                       Figure out range of pixel val.
C                                       Default to slice max min.
      IF (RANGE(1).GE.RANGE(2)) THEN
         RANGE(1) = XFLUX(1)
         RANGE(2) = XFLUX(2)
         END IF
C                                       header max/min if needed
C                                       Range in pixel values.
 20   DX = (RANGE(2) - RANGE(1)) * 0.03
      RANGE2(2) = RANGE(2) + DX
      RANGE2(1) = RANGE(1) - DX
C                                       Calc fac & offset to keep BLC
C                                       TRC within range to prevent
C                                       overflow in graphics routines.
      PIXMAX = RANGE2(2)
      PIXMIN = RANGE2(1)
      XFAC = 39999.0 / (PIXMAX - PIXMIN)
      XOFF = 40000.0 - XFAC * PIXMAX
      RANGE2(1) = XFAC * RANGE2(1) + XOFF
      RANGE2(2) = XFAC * RANGE2(2) + XOFF
      RMAX = 2.0 ** (NBITWD-1) - 1
C                                       Must reduce users max value.
      IF (RANGE2(2).GT.RMAX) THEN
         MSGTXT = '** WARNING ** REDUCING Y MAX INPUT BY USER'
         CALL MSGWRT (6)
         RANGE2(2) = RMAX
         END IF
C                                       Must increase users min.
      IF (RANGE2(1).LT.-RMAX) THEN
         MSGTXT = '** WARNING ** INCREASING Y MIN INPUT BY USER'
         CALL MSGWRT (6)
         RANGE2(1) = -RMAX
         END IF
C                                       Round and back calc range.
      RANGE2(1) = IROUND (RANGE2(1))
      RANGE2(2) = IROUND (RANGE2(2))
      IF (RANGE2(1).GE.RANGE2(2)) THEN
         RANGE(1) = CATR(KRDMN)
         RANGE(2) = CATR(KRDMX)
         GO TO 20
         END IF
      RANGE(1) = (RANGE2(1) - XOFF) / XFAC
      RANGE(2) = (RANGE2(2) - XOFF) / XFAC
      BLC(2) = RANGE2(1)
      TRC(2) = RANGE2(2)
C                                       Save original slice pts.
      XBLC1 = XBLC(1)
      XBLC2 = XBLC(2)
      XBLC3 = XBLC(3)
      XTRC1 = XTRC(1)
      XTRC2 = XTRC(2)
      XTRC3 = XTRC(3)
C                                       Initialize plot file line drw.
      CALL SLBINI (IDROP, INPTS, RANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      XYRATO = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
      IX1 = BLC(1) + .5
      IY1 = BLC(2) + .5
      IX2 = TRC(1) + .5
      IY2 = TRC(2) + .5
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      TVSIZE(1) = WINDTV(3) - WINDTV(1) + 1
      TVSIZE(2) = WINDTV(4) - WINDTV(2) + 1
      NYA = WINDTV(4) - WINDTV(2) - ICHT - ICHB
      NXA = WINDTV(3) - WINDTV(1) - ICHL - ICHR
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         MSGTXT = 'SCALING ERROR'
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X * FLOAT(TVSIZE(1)) / FLOAT(TVSIZE(2))
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
      NXA = SCALEX * X + ICHL + ICHR
      IF (NXA.GE.TVSIZE(1)) THEN
         SCALEX = SCALEX * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
         SCALEY = SCALEY * (FLOAT(TVSIZE(1)) / (NXA + 5.0))
         NXA = SCALEX * X + ICHL + ICHR
         END IF
      NYA = SCALEY * Y + ICHB + ICHT
      IF (NXA.GE.TVSIZE(1)) THEN
         SCALEX = SCALEX * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
         SCALEY = SCALEY * (FLOAT(TVSIZE(2)) / (NYA + 5.0))
         NXA = SCALEX * X + ICHL + ICHR
         NYA = SCALEY * Y + ICHB + ICHT
         END IF
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + WINDTV(1)
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + WINDTV(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + .5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + .5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC1
      CATR(I4XTRA+1) = XBLC2
      CATR(I4XTRA+2) = XTRC1
      CATR(I4XTRA+3) = XTRC2
      CATR(I4XTRA+4) = XBLC3
      CATR(I4XTRA+5) = XTRC3
      I4XTRA = I4XTRA + 6
      I4XTRA = I4XTRA/2 + 1
      CATD(I4XTRA) = FQFREQ
      I4XTRA = 2*I4XTRA + 1
      CATR(I4XTRA) = FQFINC
      CALL CHR2H (2, 'SL', KHPTYO, CATBLK(KHPTY))
      CATR(IRRAN) = RANGE(1)
      CATR(IRRAN+1) = RANGE(2)
C
      RX0 = RX0 - BLC(1) * SCALEX + .5
      RY0 = RY0 - BLC(2) * SCALEY + .5
      GO TO 999
C                                       Model requested not avail.
 950  WRITE (MSGTXT,1950) IMOD, INMOD
      CALL MSGWRT (6)
      IERR = 1
      GO TO 999
C                                       Error reading slice file.
 980  MSGTXT = 'ERROR READING SLICE FILE HEADER'
      CALL MSGWRT (8)
C                                       Return values.
 999  RETURN
C-----------------------------------------------------------------------
 1950 FORMAT ('GAUSSIAN MODEL',I6,' REQUESTED. ONLY',I6,' AVAILABLE.')
      END
