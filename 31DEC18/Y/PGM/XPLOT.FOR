LOCAL INCLUDE 'XPLBUFRS'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SCRTCH(256)
      REAL      XBAR, XDATA(MABFSS), BUFF1(MABFSS)
      COMMON /BUFRS/ BUFF1, XDATA, SCRTCH, XBAR
LOCAL END
LOCAL INCLUDE 'XPLOT.INC'
C                                       Local include for XPLOT
      INCLUDE 'XPLBUFRS'
      INTEGER   SEQIN, DISKIN, OLDCNO, JBUFSZ
      REAL      XSEQIN, XDISKI, BLC(7), TRC(7), FCUT, YINC, ZINC,
     *   PIXVAL, PLTYPE, RANGE(2), DOTV, GRCHAN
      HOLLERITH XNAMEI(3), XCLAIN(2)
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, BLC, TRC, YINC,
     *   ZINC, FCUT, RANGE, PLTYPE, PIXVAL, DOTV, GRCHAN
      COMMON /XPLCHR/ NAMEIN, CLAIN
      COMMON /PARMS/ SEQIN, DISKIN, OLDCNO, JBUFSZ
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM XPLOT
C-----------------------------------------------------------------------
C! Plots rows of an image on a graphics device.
C# Map Spectral Graphics TV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2005, 2007, 2012, 2014-2015
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
C   XPLOT plots rows of an image on the TEK graphics screen.
C   AIPS adverbs:
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      YINC           YINC          Pixel increment on 2nd axis
C      ZINC           ZINC          Pixel increment on 3rd axis
C      FLUX           FCUT          Flux cutoff: > 2 consecutive
C                                   points must > FLUX to plot
C      LTYPE                        Type of labeling: 1 border,
C                                   2 no ticks, 3 standard, 4 rel
C                                   to center, 5 rel to subim cen
C                                   6 map pixels
C      PIXRANGE                     Min,Max of image intensity
C                                   Max <= Min => entire range
C      PIXVAL         PIXVAL        Display only if peak < PIXVAL
C   Programmer Eric W. Greisen based on Cotton's Taffy and Fickling's
C   TKSLICE.   July 1983
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'XPLOT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'XPLOT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL XPLTIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL XPLTDO (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE XPLTIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   XPLTIN gets input parameters for XPLOT
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C            /MAPHDR/ file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET
C
      CHARACTER STAT*4, MTYPE*2
      INTEGER   NPARM, IERR, I
      REAL      EPS
      INCLUDE 'XPLOT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 30
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Using the TEK?
      IF ((NPOPS.GT.NINTRN) .OR. ((NTKDEV.LE.0) .AND. (DOTV.LE.0.0))
     *   .OR. ((NTVDEV.LE.0) .AND. (DOTV.GT.0.0))) THEN
         IRET = 8
         WRITE (MSGTXT,1010)
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (IRET.EQ.0) RQUICK = .FALSE.
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      EPS = 0.1
      SEQIN = XSEQIN + EPS
      DISKIN = XDISKI + EPS
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK and mark 'READ'.
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'READ', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       check parms -> defaults
      IRET = 4
      I = YINC + 0.01
      IF (I.LE.0) I = 1
      YINC = I
      I = ZINC + 0.01
      IF (I.LE.0) I = 1
      ZINC = I
      IF (FCUT.LE.0.0) FCUT = 0.0005
      IF (PIXVAL.LE.FCUT) PIXVAL = 100. * FCUT
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XPLTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('NO TV OR TEK OR NOT ALLOWED IN BATCH')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I2,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE XPLTDO (IRET)
C-----------------------------------------------------------------------
C   XPLTDO sends image one row at a time to the plotting routine
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   IROUND, LUNI, NYI, NXI, WINI(4), BOI, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), BOTEMP,
     *   IBIND, INDI, LIM1, IINC2, IINC3
      REAL      PLTODO, PLDONE
      LOGICAL   T, F
      INCLUDE 'XPLOT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXI = CATBLK(KINAX)
      NYI = CATBLK(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      IINC2 = YINC + 0.01
      IINC3 = ZINC + 0.01
      PLDONE = 0.0
      PLTODO = REAL(LIM7) * REAL(LIM4) * REAL(LIM5) * REAL(LIM6)
      PLTODO = PLTODO * ((LIM3 - 1) / IINC3 + 1)
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  DO 300 I3 = 1,LIM3,IINC3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     PLDONE = PLDONE + 1
                     WRITE (MSGTXT,1090) PLDONE, PLTODO
                     CALL MSGWRT (1)
C                                       Init. files, first input.
                     CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3),
     *                  BOTEMP, IRET)
                     IF (IRET.GT.0) THEN
                        WRITE (MSGTXT,1099) IRET
                        GO TO 990
                        END IF
                     BOI = BOTEMP + 1
                     CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI,
     *                  BUFF1, JBUFSZ, BOI, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1100) 'READ', IRET
                        GO TO 990
                        END IF
                     DO 250 I2 = 1,LIM2
                        IPOS(2) = BLC(2) + I2 - 0.9
                        IPOS(1) = IROUND (BLC(1))
C                                       Read.
                        CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1120) 'READ', IRET
                           GO TO 990
                           END IF
C                                       Want this row?
                        IF (MOD(I2-1,IINC2).EQ.0) THEN
C                                       Copy to buffer.
                           DO 160 I1 = 1,LIM1
                              XDATA(I1) = BUFF1(IBIND+I1-1)
 160                          CONTINUE
C                                       Call DO1PLT
                           CALL DO1PLT (IPOS, IRET)
                           IF (IRET.LT.0) THEN
                              GO TO 710
                           ELSE IF (IRET.GT.0) THEN
                              WRITE (MSGTXT,1180) IRET
                              GO TO 990
                              END IF
                           END IF
 250                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
 710  CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('XPLTDO: ERROR',I3,' OPENING INPUT FILE')
 1090 FORMAT ('Begin plane',F8.0,' of',F8.0)
 1099 FORMAT ('XPLTDO: COMOFF ERROR',I3)
 1100 FORMAT ('XPLTDO: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('XPLTDO: ',A4,' ERROR',I3)
 1180 FORMAT ('XPLTDO: DO1PLT ERROR',I3)
      END
      SUBROUTINE DO1PLT (IPOS, IRET)
C-----------------------------------------------------------------------
C   DO1PLT calls the TEK plot routines
C   Inputs:
C      IPOS(7)   I    BLC (input image) of first value in XDATA
C   Values from commons:
C      XDATA(*)  R    Input row, magic value blanked.
C      FBLANK    R    Value of blanked pixel.
C      CATBLK    I    Input catalog header (also CATR, CATD)
C   Output:
C      IRET      I    Return code   0 => OK
C                               <0 => user requests end
C                               >0 => error, terminate.
C   Output in COMMON
C     CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
C
      INTEGER   INPTS, LABEL, TKERR, FVEC(512), IERR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'XPLOT.INC'
      INCLUDE 'INCS:DTKS.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Not last call
      IF (IPOS(1).GE.0) THEN
         INPTS = TRC(1) - BLC(1) + 1.01
         XBAR = IPOS(1) - 1 - CATR(KRCRP)
         CALL XPLTGE (INPTS, FCUT, PIXVAL, IERR)
C                                       Plot it
         IF (IERR.EQ.0) THEN
            LABEL = ABS (PLTYPE)
            IF (MOD(LABEL,100).LE.0) LABEL = (LABEL/100)*100 + 3
            IF (DOTV.LE.0.0) THEN
               CALL PTKINI (IPOS, INPTS, LABEL, RANGE, FVEC, TKERR)
            ELSE
               CALL PTVINI (IPOS, INPTS, LABEL, RANGE, GRCHAN, TKERR)
               END IF
            IRET = TKERR
            IF (TKERR.EQ.102) IRET = -1
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE XPLTGE (ND, FC, PIXVAL, IERR)
C-----------------------------------------------------------------------
C   XPLTGE decides if the data are intense enough to plot.
C   Inputs:
C      ND       I   Number of data samples
C      FC       R   Flux cutoff
C      PIXVAL   R   No plot if peak > PIXVAL
C   Output:
C      IERR     I   0 => ok, 1 => all data too low
C-----------------------------------------------------------------------
      INTEGER   ND, IERR
      REAL      FC, PIXVAL
C
      INTEGER   IM, I
      REAL      X, XM, BLP, BLM, BL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'XPLBUFRS'
C-----------------------------------------------------------------------
      IM = 0
      XM = 0.
      DO 40 I = 2,ND-1
         BL = XDATA(I)
         IF ((BL.NE.FBLANK) .AND. (BL.GE.FC)) THEN
            BLM = XDATA(I-1)
            BLP = XDATA(I+1)
            IF ((BLP.NE.FBLANK) .AND. (BLM.NE.FBLANK) .AND. (BLP.GE.FC)
     *         .AND. (BLM.GE.FC)) THEN
               X = BLP + BL + BLM
               IF (X.GE.XM) THEN
                  XM = X
                  IM = I
                  END IF
               END IF
            END IF
 40      CONTINUE
C                                       Find anything?
      IERR = 1
C                                       test desire to plot
      IF (IM.GE.1) THEN
         IF (XM/3.0.LE.PIXVAL) IERR = 0
         END IF
C
 999  RETURN
      END
      SUBROUTINE PTKINI (IPOS, INPTS, LABEL, PIXR, FVEC, IERR)
C-----------------------------------------------------------------------
C   PTKINI initializes the TEK for a XPLOT plot, plots axis labels,
C   and, plots the data.
C   Inputs:
C      IPOS    I(7)     Position in cube first point in row.
C      INPTS   I        Number of points in row.
C      LABEL   I        Requested label type
C      PIXR    R(2)     Requested plot value range
C   Output:
C      FVEC    I(512)   Scratch buffer
C      IERR    I        > 0 => plot failed
C                             102 => DIE
C   Common:
C      TKLUN   I        LUN for open TEK
C      TKIND   I        FTAB pointer for TEK
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), INPTS, LABEL, FVEC(512), IERR
      REAL      PIXR(2)
C
      CHARACTER TEMP*4, TEXT(2)*80, MSGBUF*132, NTEXT*80
      REAL      ORANGE(2), XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP,
     *   CH(4), XYRATO, X, XX, Y, YFAC, YOFF, DX, DY, FQFINC
      INTEGER   IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR, ICHT,
     *   NXA, NYA, I, J, JERR, TTYLUN, TTYIND, IPLANE, I4XTRA
      LOGICAL   T, F, BLAST, BNEXT
      DOUBLE PRECISION FQFREQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTKS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'XPLBUFRS'
      DATA T, F /.TRUE.,.FALSE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
C                                       inits, open TEK
      CALL ZTKOPN (IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TKCATL ('INIT', I, I, FVEC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 10 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 10      CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
C                                       Set PIX ranges
      ORANGE(1) = PIXR(1)
      ORANGE(2) = PIXR(2)
C                                       Default: actual range
      IF (PIXR(2).LE.PIXR(1)) THEN
         ORANGE(1) = 1.0E10
         ORANGE(2) = -ORANGE(1)
         DO 15 I = 1,INPTS
            IF (XDATA(I).NE.FBLANK) THEN
               ORANGE(1) = MIN (ORANGE(1), XDATA(I))
               ORANGE(2) = MAX (ORANGE(2), XDATA(I))
               END IF
 15         CONTINUE
         YFAC = ORANGE(2) - ORANGE(1)
         ORANGE(2) = ORANGE(2) + 0.25  * YFAC
         ORANGE(1) = ORANGE(1) - 0.25  * YFAC
         END IF
      CATR(IRRAN) = ORANGE(1)
      CATR(IRRAN+1) = ORANGE(2)
      YFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      YOFF = 40000.0 - YFAC * CATR(IRRAN)
      PBLC(2) = ORANGE(1) * YFAC + YOFF
      PTRC(2) = ORANGE(2) * YFAC + YOFF
C                                       Label inits
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, ORANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      ORANGE(1) = YFAC*ORANGE(1) + YOFF
      ORANGE(2) = YFAC*ORANGE(2) + YOFF
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTK(1) + .5
      ICHB = CH(2) * CSIZTK(2) + .5
      ICHR = CH(3) * CSIZTK(1) + .5
      ICHT = CH(4) * CSIZTK(2) + .5
      NYA = MAXXTK(2) - ICHT -ICHB -1
      NXA = MAXXTK(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
      NXA = SCALEX * X + ICHL + ICHR
      RX0 = ICHL + MAX (0, MAXXTK(1)-NXA) / 2 + 1
      NYA = SCALEY * Y + ICHB + ICHT
      RY0 = ICHB + MAX (0, MAXXTK(2)-NYA) / 2 + 1
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
      CALL TKCATL ('WRIT', IPLANE, IPLANE, CATBLK, IERR)
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + .5
      RY0 = RY0 - PBLC(2) * SCALEY + .5
C                                       label the plot
      CALL TKLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       add pixel coordinates
      DX = -8.5
      DY = -2.0
      CALL TEKVEC (PTRC(1), PTRC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1060) IPOS(2)
      J = 7
      CALL TKCHAR (J, 0, DX, DY, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      DY = DY - 1.5
      CALL TEKVEC (PTRC(1), PTRC(2), 1, IERR)
      IF (IERR.NE.0) GO TO 990
      WRITE (MSGBUF,1061) IPOS(3)
      CALL TKCHAR (J, 0, DX, DY, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       plot data
      BLAST = .TRUE.
      DO 70 I = 1,INPTS
         BNEXT = XDATA(I).EQ.FBLANK
         IF (.NOT.BNEXT) THEN
            X = I - 0.5
            Y = XDATA(I) * YFAC + YOFF
            Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
            J = 2
            IF (BLAST) J = 1
            CALL TEKVEC (X, Y, J, IERR)
            IF (IERR.NE.0) GO TO 990
            X = I + 0.5
            J = 2
            CALL TEKVEC (X, Y, J, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         BLAST = BNEXT
 70      CONTINUE
C                                       alpha mode
      I = 31
      CALL ZTKBUF (I, 1, IERR)
      CALL TEKFLS (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Talk to user
      CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1900) JERR
         CALL MSGWRT (6)
         GO TO 990
         END IF
      WRITE (MSGBUF,1910)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
      IF (JERR.NE.0) GO TO 920
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
      IF (JERR.NE.0) GO TO 920
      READ (MSGBUF,1911) TEMP
      IF ((TEMP(1:1).EQ.'Q') .OR. (TEMP(1:1).EQ.'q')) IERR = 102
      GO TO 930
C                                       TTY error
 920  WRITE (MSGTXT,1920) JERR
      CALL MSGWRT (6)
 930  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
C                                       Close all the time
 990  CALL ZTKCLS (JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SCALING ERROR.')
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('**** Type QUIT to end task, hit return to proceed')
 1911 FORMAT (A4)
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
      SUBROUTINE PTVINI (IPOS, INPTS, LABEL, PIXR, GRCHAN, IERR)
C-----------------------------------------------------------------------
C   PTVINI initializes the TV for a XPLOT plot, plots axis labels,
C   and, plots the data.
C   Inputs:
C      IPOS     I(7)     Position in cube first point in row.
C      INPTS    I        Number of points in row.
C      LABEL    I        Requested label type
C      PIXR     R(2)     Requested plot value range
C      GRCHAN   R        User adverb selects graphics channel.
C   Output:
C      IERR     I        > 0 => plot failed
C                             102 => DIE
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), INPTS, LABEL, IERR
      REAL      PIXR(2), GRCHAN
C
      CHARACTER TEMP*4, TEXT(2)*80, MSGBUF*132, NTEXT*80
      REAL      ORANGE(2), XBLC(7), XTRC(7), PBLC(2), PTRC(2), YGAP,
     *   CH(4), XYRATO, X, XX, Y, YFAC, YOFF, DX, DY, FQFINC
      INTEGER   IDROP(2), IX1, IX2, IY1, IY2, ICHL, ICHB, ICHR, ICHT,
     *   NXA, NYA, I, J, JERR, TTYLUN, TTYIND, I4XTRA, IDX, IDY,
     *   TVWIND(4), TVSIZE(2)
      LOGICAL   T, F, BLAST, BNEXT, FIRST
      DOUBLE PRECISION FQFREQ
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DTVS.INC'
      INCLUDE 'XPLBUFRS'
      SAVE FIRST
      DATA T, F /.TRUE.,.FALSE./
      DATA FIRST /.TRUE./
      DATA TTYLUN /5/
C-----------------------------------------------------------------------
C                                       inits, open TEK
      CALL TVOPEN (SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
      IGR = GRCHAN + 0.01
      IF (IGR.LE.0) IGR = 1
      IF (IGR.GT.7) IGR = 1
      IGR = IGR + NGRAY
      CALL YHOLD ('ONNN', IERR)
      IF (FIRST) THEN
         DO 10 I = 1,NGRAY+NGRAPH
            CALL YSLECT ('OFFF', I, 0, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 990
 10         CONTINUE
         CALL YSLECT ('ONNN', IGR, 0, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 990
         FIRST = .FALSE.
         END IF
C                                       "Slice" corners
      IDROP(1) = 0
      IDROP(2) = 0
      DO 15 I = 1,7
         XBLC(I) = IPOS(I)
         XTRC(I) = XBLC(I)
 15   CONTINUE
      XTRC(1) = XBLC(1) + INPTS - 1
C                                       Set PIX ranges
      ORANGE(1) = PIXR(1)
      ORANGE(2) = PIXR(2)
C                                       Default: actual range
      IF (PIXR(2).LE.PIXR(1)) THEN
         ORANGE(1) = 1.0E10
         ORANGE(2) = -ORANGE(1)
         DO 20 I = 1,INPTS
            IF (XDATA(I).NE.FBLANK) THEN
               ORANGE(1) = MIN (ORANGE(1), XDATA(I))
               ORANGE(2) = MAX (ORANGE(2), XDATA(I))
               END IF
 20            CONTINUE
         YFAC = ORANGE(2) - ORANGE(1)
         ORANGE(2) = ORANGE(2) + 0.25  * YFAC
         ORANGE(1) = ORANGE(1) - 0.25  * YFAC
         END IF
      CATR(IRRAN) = ORANGE(1)
      CATR(IRRAN+1) = ORANGE(2)
      YFAC = 39999.0 / (CATR(IRRAN+1) - CATR(IRRAN))
      YOFF = 40000.0 - YFAC * CATR(IRRAN)
      PBLC(2) = ORANGE(1) * YFAC + YOFF
      PTRC(2) = ORANGE(2) * YFAC + YOFF
C                                       Label inits
      LOCNUM = 1
      FQFINC = 0.0
      FQFREQ = 0.0D0
      CALL SLBINI (IDROP, INPTS, ORANGE, PBLC, PTRC, XBLC, XTRC,
     *   FQFREQ, FQFINC, CATBLK(IIDEP), LABEL, YGAP, CH, TEXT, NTEXT)
      ORANGE(1) = YFAC*ORANGE(1) + YOFF
      ORANGE(2) = YFAC*ORANGE(2) + YOFF
      XYRATO = (PTRC(2) - PBLC(2)) / (PTRC(1) - PBLC(1))
      IX1 = PBLC(1) + .5
      IY1 = PBLC(2) + .5
      IX2 = PTRC(1) + .5
      IY2 = PTRC(2) + .5
      ICHL = CH(1) * CSIZTV(1) + .5
      ICHB = CH(2) * CSIZTV(2) + .5
      ICHR = CH(3) * CSIZTV(1) + .5
      ICHT = CH(4) * CSIZTV(2) + .5
      CALL YWINDO ('READ', TVWIND, IERR)
      IF (IERR.NE.0) THEN
         TVWIND(1) = 1
         TVWIND(2) = 1
         TVWIND(3) = MAXXTV(1)
         TVWIND(4) = MAXXTV(2)
         END IF
      TVSIZE(1) = TVWIND(3) - TVWIND(1) + 1
      TVSIZE(2) = TVWIND(4) - TVWIND(2) + 1
      NYA = TVSIZE(2) - ICHT -ICHB -1
      NXA = TVSIZE(1) - ICHL - ICHR - 1
C                                       compute scaling
      X = IX2
      X = ABS (X - IX1)
      XX = X * XYRATO
      Y = IY2
      Y = ABS (Y - IY1)
      IF ((XX.LE.0.0) .OR. (Y.LE.0.0)) THEN
         WRITE (MSGTXT,1020)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 990
         END IF
      IF ((XX/Y).LE.FLOAT(NXA)/FLOAT(NYA)) THEN
         SCALEY = NYA / Y
         SCALEX = SCALEY * Y / X
      ELSE
         SCALEX = NXA / X
         SCALEY = SCALEX * X / Y
         END IF
C
      NXA = SCALEX * X + ICHL + ICHR
      RX0 = ICHL + MAX (0, TVSIZE(1)-NXA) / 2 + TVWIND(1)
      NYA = SCALEY * Y + ICHB + ICHT
      RY0 = ICHB + MAX (0, TVSIZE(2)-NYA) / 2 + TVWIND(2)
C                                       Put stuff in image catalog.
      CATBLK(IIWIN  ) = IX1
      CATBLK(IIWIN+1) = IY1
      CATBLK(IIWIN+2) = IX2
      CATBLK(IIWIN+3) = IY2
      CATBLK(IICOR  ) = RX0 + 0.5
      CATBLK(IICOR+1) = RY0 + 0.5
      CATBLK(IICOR+2) = RX0 + X * SCALEX + 0.5
      CATBLK(IICOR+3) = RY0 + Y * SCALEY + 0.5
      CATBLK(IIPLT) = 5
      CATBLK(IIOTH) = LABEL
      CATBLK(IIOTH+1) = IDROP(1)
      CATBLK(IIOTH+2) = IDROP(2)
      I4XTRA = IIOTH + 3
      CATR(I4XTRA  ) = XBLC(1)
      CATR(I4XTRA+1) = XBLC(2)
      CATR(I4XTRA+2) = XTRC(1)
      CATR(I4XTRA+3) = XTRC(2)
C                                       Update image catalog
      CALL YCINIT (IGR, SCRTCH)
      CALL YCWRIT (IGR, CATBLK(IICOR), CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040)
         CALL MSGWRT (6)
         END IF
C                                       clear screen
      CALL YZERO (IGR, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       set scale for plot routines
      RX0 = RX0 - PBLC(1) * SCALEX + .5
      RY0 = RY0 - PBLC(2) * SCALEY + .5
C                                       label the plot
      CALL TVLAB (PBLC, PTRC, LABEL, YGAP, TEXT, NTEXT, CH, F, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       add pixel coordinates
      DX = PTRC(1)*SCALEX + RX0 - 8.5 * CSIZTV(1)
      DY = PTRC(2)*SCALEY + RY0 - 2.0 * CSIZTV(2)
      WRITE (MSGBUF,1060) IPOS(2)
      IDX = DX + 0.5
      IDY = DY + 0.5
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
      DY = DY - 1.5 * CSIZTV(2)
      WRITE (MSGBUF,1061) IPOS(3)
      IDY = DY + 0.5
      CALL IMCHAR (IGR, IDX, IDY, 0, 0, MSGBUF(:7), INBUF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       plot data
      BLAST = .TRUE.
      DO 70 I = 1,INPTS
         BNEXT = XDATA(I).EQ.FBLANK
         IF (.NOT.BNEXT) THEN
            X = I - 0.5
            Y = XDATA(I) * YFAC + YOFF
            Y = MIN (ORANGE(2), MAX (ORANGE(1), Y))
            J = 2
            IF (BLAST) J = 1
            CALL TVVEC (X, Y, J, IERR)
            IF (IERR.NE.0) GO TO 990
            X = I + 0.5
            J = 2
            CALL TVVEC (X, Y, J, IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
         BLAST = BNEXT
 70      CONTINUE
C                                       Talk to user
      CALL YHOLD ('OFFF', JERR)
      CALL ZOPEN (TTYLUN, TTYIND, 1, MSGBUF, F, T, T, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1900) JERR
         CALL MSGWRT (6)
         GO TO 990
         END IF
      WRITE (MSGBUF,1910)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYIND, 72, MSGBUF, JERR)
      IF (JERR.NE.0) GO TO 920
      CALL ZTTYIO ('READ', TTYLUN, TTYIND, 72, MSGBUF, JERR)
      IF (JERR.NE.0) GO TO 920
      READ (MSGBUF,1911) TEMP
      IF ((TEMP(1:1).EQ.'Q') .OR. (TEMP(1:1).EQ.'q')) IERR = 102
      GO TO 930
C                                       TTY error
 920  WRITE (MSGTXT,1920) JERR
      CALL MSGWRT (6)
 930  CALL ZCLOSE (TTYLUN, TTYIND, JERR)
C                                       Close all the time
 990  CALL YHOLD ('OFFF', JERR)
      CALL TVCLOS (SCRTCH, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SCALING ERROR.')
 1040 FORMAT ('TV IMAGE CATALOG ERROR.')
 1060 FORMAT ('Y=',I5)
 1061 FORMAT ('Z=',I5)
 1900 FORMAT ('OPEN TERMINAL ERROR',I7)
 1910 FORMAT ('**** Type QUIT to end task, hit return to proceed')
 1911 FORMAT (A4)
 1920 FORMAT ('TERMINAL I/O ERROR',I7)
      END
