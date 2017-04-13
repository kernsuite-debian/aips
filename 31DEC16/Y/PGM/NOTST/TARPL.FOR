LOCAL INCLUDE 'TARPL.INC'
C                                       PLTLAB declarations.
      INTEGER   MAXPNT, MAXTYP
      PARAMETER (MAXTYP = 30)
      PARAMETER (MAXPNT = 2000)
C
      REAL       CHOUT(4), YGAP, XP(MAXPNT,MAXTYP), YP1(MAXPNT,MAXTYP),
     *   YP2(MAXPNT,MAXTYP), XMAX, XMIN, Y1MAX, Y1MIN, Y2MAX, Y2MIN,
     *   Y3MIN, Y3MAX, BLC(7), TRC(7), YPS(MAXPNT,MAXTYP,2)
      INTEGER    PCODES(5), NP, STTYPE(MAXTYP,2), LITYPE(MAXTYP,2),
     *   NSTYPE(MAXTYP), NCOL, ICOL(MAXTYP)
      CHARACTER  XUNIT*24, YUNIT(2)*24, TITLE*80
      LOGICAL    DOCOLR
      EQUIVALENCE (YP1, YPS(1,1,1)), (YP2, YPS(1,1,2))
C                                       Plot labeling parameters.
      COMMON /PLTLAB/ CHOUT, YGAP, PCODES, XP, YP1, YP2, XMAX,
     *   XMIN, Y1MAX, Y1MIN, Y2MAX, Y2MIN, Y3MIN, Y3MAX, NP, BLC, TRC,
     *   STTYPE, LITYPE, NSTYPE, DOCOLR, NCOL, ICOL
      COMMON /PLCLAB/ XUNIT, YUNIT, TITLE
C                                       INPARM declarations.
      HOLLERITH XNAMIN(3), XCLSIN(2), XINFIL(12)
      REAL      PRUSER, XSEQ, XDISK, APARM(10), RPARM(30), VPARM(30),
     *   FACTOR, DO3COL, LTYPE, XYRATO, XDOTV, XGRCH, PARMS(96)
      EQUIVALENCE (PRUSER, PARMS)
C                                       Parameters from AIPS.
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, XSEQ, XDISK, XINFIL,
     *   APARM, RPARM, VPARM, FACTOR, DO3COL, LTYPE, XYRATO, XDOTV,
     *   XGRCH
C
LOCAL END
LOCAL INCLUDE 'TARPL.PLT'
      INTEGER   PLTBLK(256), GRCHN, TVCHN, TVCORN(4), IPLOT1, IPLOT2,
     *   LINT, IMSTUF(40), IBLKSZ, IVER, IPLOT, IOFFPL
      LOGICAL   DOTV
      REAL      XY, PBLC(2,2), PTRC(2,2), XSCAL, XOFF, YSCAL(2),
     *   YOFF(2), XRANGE, YRANGE(2), XLAST, YLAST
      COMMON /PLTARS/ PLTBLK, GRCHN, TVCHN, TVCORN, IPLOT1, IPLOT2,
     *   LINT, DOTV, XY, PBLC, PTRC, XSCAL, XOFF, YSCAL, YOFF, XRANGE,
     *   YRANGE, IMSTUF, IBLKSZ, IVER, XLAST, YLAST, IPLOT, IOFFPL
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGPH.INC'
LOCAL END
      PROGRAM TARPL
C-----------------------------------------------------------------------
C! General plot task for TARS output files
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2012, 2014-2015
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
C   TARPL is a task to read data rom a TARS output file and make plots
C   of RM spectra
C   Inputs:   (from AIPS)
C       USERID    R      user number, 0 means use logon user
C                        number, 32000 means any user can be accessed.
C       INNAME    R(3)   name of primary file. to attach plot file
C       INCLASS   R(2)   class of primary file.
C       INSEQ     R      sequence number of primary file.
C       NDISK     R      disk volume number. 0 means try all.
C       INFILE    H(12)  Input text file
C       APARM     R(10)  Plot controls
C       LTYPE     R      Label type.
C       DOTV      R      > 0 => TV, else plot file
C       GRCHAN    R      graphics channel to use (0 => 1)
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IERR, IDEBUG, IPTYPE, IROUND, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'TARPL.PLT'
      INCLUDE 'TARPL.INC'
      DATA PRGNAM /'TARPL '/
C-----------------------------------------------------------------------
C                                       Copy from DATA to Commons
      IBLKSZ = MABFSS
      GPHIND = 0
C                                       Don't delete plot file on error
      IDEBUG = 1
C                                       This is the number of REAL*4
C                                       words to get from AIPS.
      NPARMS = 96
C                                       Plot type TARPL paraform
      IPTYPE = 26
      CALL FILL (5, 0, PCODES)
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL TARPLI (PRGNAM, NPARMS, IERR)
C                                       Labeling type:
      PCODES(1) = IROUND (LTYPE)
      I = MOD (ABS(PCODES(1)), 100)
      IF ((I.EQ.0) .OR. (I.GT.10)) I = 3
      IF (PCODES(1).LT.0) THEN
         PCODES(1) = (PCODES(1)/100)*100 - I
      ELSE
         PCODES(1) = (PCODES(1)/100)*100 - I
         END IF
C                                       Do plotting.
      IF (IERR.EQ.0) CALL PLOTER (NPARMS, IPTYPE, IERR)
C                                       Shutdown.
      CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE TARPLI (PRGNAM, NPARMS, IERR)
C-----------------------------------------------------------------------
C   This routine does all the intial set up.  Get parms from AIPS,
C   open the map file, create the plot file and write the plot file
C   records to do the plot labeling.
C   Inputs:
C      PRGNAM C*6    Name of this program.
C      NPARMS I      Number of R words to get from AIPS.
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IERR
C
      CHARACTER NAME*36, FNAME*12, FCLASS*6, FPTYPE*2, STAT*4, LINE*80,
     *   INFILE*48
      INTEGER   IWORK(256), FSEQ, FVOL, FUSID, TLUN, TIND, KBP, IMLUN,
     *   I, IROUND, JT, JTRIM, IGRP, J
      DOUBLE PRECISION X
      REAL      SCALE(2), TEMP
      LOGICAL   ISAMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'TARPL.INC'
      INCLUDE 'TARPL.PLT'
      DATA TLUN, IMLUN /3, 16/
C-----------------------------------------------------------------------
C                                       Get parameters from AIPS, init
C                                       AIPS I/O, other startup things.
      CALL SETUP (PRGNAM, NPARMS, PARMS, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
      DOCOLR = DO3COL.GT.0.0
C                                       Open the map file.
      CALL H2CHR (12, 1, XNAMIN, FNAME)
      CALL H2CHR (6, 1, XCLSIN, FCLASS)
      FSEQ = XSEQ + 0.01
      FVOL = XDISK + 0.01
      PRUSER = NLUSER
      FUSID = NLUSER
      DOTV = XDOTV.GT.0.0
      GRCHN = XGRCH + 0.01
      TVCHN = 1
      CALL FILL (4, 0, TVCORN)
      STAT = 'HDWR'
      IF (DOTV) STAT = 'READ'
      CALL A2WAWA (FNAME, FCLASS, FSEQ, '  ', FVOL, FUSID, NAME)
      CALL INTMIO (IMLUN, STAT, NAME, BLC, TRC, IBLKSZ, CATBLK,
     *   IMSTUF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       fill in adverbs w actual values
      CALL WAWA2A (NAME, FNAME, FCLASS, FSEQ, FPTYPE, FVOL, FUSID)
      CALL CHR2H (12, FNAME, 1, XNAMIN)
      CALL CHR2H (6, FCLASS, 1, XCLSIN)
      XSEQ = FSEQ
      XDISK = FVOL
      CALL FILL (MAXTYP, 0, NSTYPE)
C                                       gather parameters
      DO 10 I = 1,MAXTYP
         STTYPE(I,1) = IROUND (RPARM(I))
         IF ((STTYPE(I,1).LT.0) .AND. (APARM(7).LE.0.0)) THEN
            STTYPE(I,2) = -STTYPE(I,1)
            STTYPE(I,1) = 0
         ELSE
            STTYPE(I,1) = ABS (STTYPE(I,1))
            STTYPE(I,2) = STTYPE(I,1)
            END IF
         IF (STTYPE(I,1).GT.23) STTYPE(I,1) = 1
         IF (STTYPE(I,2).GT.23) STTYPE(I,2) = 1
         LITYPE(I,1) = IROUND (VPARM(I))
         IF ((LITYPE(I,1).LT.0) .AND. (APARM(7).LE.0.0)) THEN
            LITYPE(I,2) = 0
            LITYPE(I,1) = -LITYPE(I,1)
         ELSE
            LITYPE(I,1) = ABS (LITYPE(I,1))
            LITYPE(I,2) = LITYPE(I,1)
            END IF
 10      CONTINUE
      XMAX = -1.E10
      Y1MAX = XMAX
      Y2MAX = XMAX
      Y3MAX = XMAX
      Y1MIN = -Y1MAX
      Y2MIN = -Y2MAX
      Y3MIN = -Y3MAX
      XMIN = Y1MIN
      J = IROUND (APARM(7))
      IF (J.EQ.0) J = -1
      IF (J.LT.-3) J = -1
      IF (J.GT.3) J = 1
      APARM(7) = J
      XUNIT = 'Rotation measure'
      IF (J.EQ.1) THEN
         YUNIT(1) = 'Real part'
         YUNIT(2) = 'Imaginary part'
      ELSE
         YUNIT(1) = 'Amplitude'
         YUNIT(2) = 'Phase'
         END IF
C                                       OPEN text file to get info
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL ZTXOPN ('READ', TLUN, TIND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ TEXT FILE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IGRP = 0
C                                       Read for start of first group
 15   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF (JT.LE.0) GO TO 15
      IF (LINE(:5).NE.';   K') GO TO 15
C                                       parse a start-group card
 20   IGRP = IGRP + 1
      NP = 0
      I = INDEX (LINE, '10^')
      I = I + 3
      READ (LINE(I:I), 1005) J
      SCALE(1) = 10.0 ** (-J)
      I = INDEX (LINE, 'PHASE')
      IF (I.GT.0) THEN
         ISAMP = .TRUE.
         SCALE(2) = 1.0
      ELSE
         ISAMP = .FALSE.
         SCALE(2) = SCALE(1)
         END IF
C                                       read loop
 50   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.EQ.0) THEN
         JT = JTRIM (LINE)
         IF (LINE(:5).EQ.';   K') GO TO 20
         IF (JT.LE.0) GO TO 50
         IF ((LINE(:1).NE.' ') .AND. ((LINE(:1).LT.'0') .OR.
     *      (LINE(:1).GT.'9'))) GO TO 50
         KBP = 1
         CALL GETNUM (LINE, 80, KBP, X)
         IF (X.NE.DBLANK) THEN
            CALL GETNUM (LINE, 80, KBP, X)
            IF (X.NE.DBLANK) THEN
               XP(NP+1,IGRP) = X
               CALL GETNUM (LINE, 80, KBP, X)
               IF (X.NE.DBLANK) THEN
                  YP1(NP+1,IGRP) = X * SCALE(1)
                  CALL GETNUM (LINE, 80, KBP, X)
                  IF (X.NE.DBLANK) THEN
                     YP2(NP+1,IGRP) = X * SCALE(2)
                     NP = NP + 1
                     NSTYPE(IGRP) = NSTYPE(IGRP) + 1
C                                       convert
                     IF (ISAMP) THEN
                        IF (APARM(7).GT.0.0) THEN
                           TEMP = YP1(NP,IGRP) *
     *                        COS (DG2RAD * YP2(NP,IGRP))
                           YP2(NP,IGRP) = YP1(NP,IGRP) *
     *                        SIN (DG2RAD * YP2(NP,IGRP))
                           YP1(NP,IGRP) = TEMP
                           END IF
                     ELSE IF (APARM(7).LE.0.0) THEN
                        TEMP = SQRT (YP1(NP,IGRP)**2 + YP2(NP,IGRP)**2)
                        YP2(NP,IGRP) = RAD2DG * ATAN2 (YP2(NP,IGRP),
     *                     YP1(NP,IGRP))
                        YP1(NP,IGRP) = TEMP
                        END IF
C                                       max/min
                     XMAX = MAX (XMAX, XP(NP,IGRP))
                     Y1MAX = MAX (Y1MAX, YP1(NP,IGRP))
                     Y2MAX = MAX (Y2MAX, YP2(NP,IGRP))
                     XMIN = MIN (XMIN, XP(NP,IGRP))
                     Y1MIN = MIN (Y1MIN, YP1(NP,IGRP))
                     Y2MIN = MIN (Y2MIN, YP2(NP,IGRP))
                     IF (APARM(7).LE.0.0) THEN
                        TEMP = YP2(NP,IGRP)
                        IF (TEMP.LT.0.0) TEMP = TEMP + 360.
                        Y3MAX = MAX (Y3MAX, TEMP)
                        Y3MIN = MIN (Y3MIN, TEMP)
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         GO TO 50
      ELSE IF (IERR.EQ.2) THEN
         CALL ZTXCLS (TLUN, TIND, IERR)
      ELSE
         WRITE (MSGTXT,1000) IERR, 'READ TEXT FILE'
         CALL MSGWRT (8)
         END IF
C                                       color pattern
      CALL FILL (MAXTYP, -1, ICOL)
      NCOL = 0
      DO 30 I = 1,MAXTYP
         IF ((NSTYPE(I).GT.0) .AND. ((STTYPE(I,1).GT.0) .OR.
     *      (LITYPE(I,1).GT.0) .OR. (STTYPE(I,2).GT.0) .OR.
     *      (LITYPE(I,2).GT.0))) THEN
            NCOL = NCOL + 1
            ICOL(I) = NCOL
            END IF
 30      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' ON ',A)
 1005 FORMAT (I1)
      END
      SUBROUTINE PLOTER (NPARMS, IPTYPE, IERR)
C-----------------------------------------------------------------------
C   This routine writes all of the plot commands (including the axis
C   drawing and labeling commands) into the plot file.
C      NPARMS I      Number of R words to get from AIPS.
C      IPTYPE I      Plot file type: 1 misc., 2 CNTR, 3 GREYS, 4 PROFL,
C                    5 SL2PL, 6 PCNTR, 7 IMEAN (hist), 8 UVPLT,
C                    9 GNPLT, 10 VBPLT, 11 PFPLn, 12 GAPLT, 13 PLCUB,
C                    14 IMVIM, 15 TAPLT.  Use 1 unless your inputs
C                    match those of these tasks - or take a new number,
C                    but AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR   I   Error code.  0=ok.
C-----------------------------------------------------------------------
      INTEGER   NPARMS, IPTYPE, IERR
C
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'TARPL.INC'
      REAL      X, Y, AX(5), AY(5), DX, DY, CP(MAXTYP)
      INTEGER   I, IPL, DOC
      LOGICAL   DOIT, FIRST
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'TARPL.PLT'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL RFILL (MAXTYP, -1.0, CP)
      IF (NCOL.LE.1) DOCOLR = .FALSE.
      IF (DOCOLR) THEN
         DO 10 I = 1,MAXTYP
            IF (ICOL(I).GT.0) THEN
               IF ((NCOL.LE.4) .AND. (DO3COL.LT.1.5)) THEN
                  CP(I) = 6.0 - ICOL(I)
               ELSE
                  CP(I) = (ICOL(I) - 1.0) / (NCOL - 1.0)
                  END IF
               END IF
 10         CONTINUE
         END IF
C                                       phase wrap
      IF (APARM(7).LE.0.0) THEN
         IF (Y3MAX-Y3MIN.LT.Y2MAX-Y2MIN) THEN
            Y2MAX = Y3MAX
            Y2MIN = Y3MIN
            DO 15 IPL = 1,MAXTYP
               DO 14 I = 1,NSTYPE(IPL)
                  IF (YP2(I,IPL).LT.0.0) YP2(I,IPL) = YP2(I,IPL) + 360.0
 14               CONTINUE
 15            CONTINUE
            END IF
         END IF
C                                       Set corner values.
      IF (APARM(2).LE.APARM(1)) THEN
         APARM(1) = XMIN
         APARM(2) = XMAX
         END IF
      IF (APARM(4).LE.APARM(3)) THEN
         APARM(3) = Y1MIN
         APARM(4) = Y1MAX
         END IF
      IF (APARM(6).LE.APARM(5)) THEN
         APARM(5) = Y2MIN
         APARM(6) = Y2MAX
         END IF
      BLC(1) = 0.0
      TRC(1) = 0.0
      BLC(2) = 1000.0
      TRC(2) = 1000.0
      IF (FACTOR.LE.0.1) FACTOR = 1.0
      IF (XYRATO.LE.0.0) XYRATO = 1.35
      XY = XYRATO
C                                       plot ranges
      IF (ABS(APARM(7)).GT.2.5) THEN
         IPLOT1 = 2
         IPLOT2 = 2
         LINT = 0
      ELSE IF (ABS(APARM(7)).GT.1.5) THEN
         IPLOT1 = 1
         IPLOT2 = 1
         LINT = 1000
      ELSE
         IPLOT1 = 1
         IPLOT2 = 2
         LINT = 500
         IF (APARM(7).LE.0.0) LINT = 700
         END IF
C                                       Create and open plot file.
      CALL PLMAKE (NPARMS, PARMS, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up commons for plotting.
      CALL PLINI2 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL2 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       two plots possibly
      DO 100 IPLOT = IPLOT1,IPLOT2
         DX = 80.0 * FACTOR / XSCAL
         DY = 80.0 * FACTOR / YSCAL(IPLOT)
         DX = DX / SQRT (XY)
         DY = DY * SQRT (XY)
C                                       Line drawing:
         DO 30 IPL = 1,MAXTYP
            IF (ICOL(IPL).GT.0) THEN
               NP = NSTYPE(IPL)
               FIRST = .TRUE.
C                                       Point plot
               IF (STTYPE(IPL,IPLOT).GT.0) THEN
                  CALL GLTYPE (4, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 20 I = 1,NP
                     AX(1) = XP(I,IPL) * XSCAL + XOFF
                     AX(2) = AX(1)
                     AX(3) = AX(1)
                     AX(4) = AX(1) - DX * XSCAL
                     AX(5) = AX(1) + DX * XSCAL
                     AY(1) = YPS(I,IPL,IPLOT)*YSCAL(IPLOT) + YOFF(IPLOT)
                     AY(2) = AY(1) + DY * YSCAL(IPLOT)
                     AY(3) = AY(1) - DY * YSCAL(IPLOT)
                     AY(4) = AY(1)
                     AY(5) = AY(1)
                     IF ((DOCOLR) .AND. (FIRST)) THEN
                        DOC = -1
                        CALL COLVEC (DOC, X, Y, CP(IPL), PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        FIRST = .FALSE.
                        END IF
                     DOIT = DOCOLR .AND. (NCOL.GT.4)
                     CALL PNTPLT (STTYPE(IPL,IPLOT), AX, AY,
     *                  PBLC(1,IPLOT), PTRC(1,IPLOT), .FALSE., DOIT,
     *                  PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 20                  CONTINUE
                  END IF
               END IF
 30         CONTINUE
C                                       Line drawing:
         DO 60 IPL = 1,MAXTYP
            IF (ICOL(IPL).GT.0) THEN
               NP = NSTYPE(IPL)
               FIRST = .TRUE.
               IF (LITYPE(IPL,IPLOT).GE.2) THEN
                  CALL GLTYPE (3, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = XP(1,IPL)
                  Y = YPS(1,IPL,IPLOT)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 40 I = 1,NP-1
                     DOC = 1
                     FIRST = .FALSE.
                     CALL DLINE (LITYPE(IPL,IPLOT)-2, DOC, XP(I,IPL),
     *                  YPS(I,IPL,IPLOT), CP(IPL), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 40                  CONTINUE
                  X = XP(NP,IPL)
                  Y = YPS(NP,IPL,IPLOT)
                  CALL COLVEC (DOC, X, Y, CP(IPL), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       step line
               ELSE IF (LITYPE(IPL,IPLOT).EQ.1) THEN
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = XP(1,IPL) + (XP(1,IPL) - XP(MIN(2,NP),IPL)) / 2.0
                  X = MIN (XMAX, MAX (XMIN, X))
                  Y = YPS(1,IPL,IPLOT)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 50 I = 1,NP
                     IF (FIRST) THEN
                        DOC = 1
                        FIRST = .FALSE.
                     ELSE
                        DOC = 0
                        END IF
                     Y = YPS(I,IPL,IPLOT)
                     IF (I.GT.1) THEN
                        CALL COLVEC (DOC, X, Y, CP(IPL), PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     X = (XP(I,IPL) + XP(MIN(I+1,NP),IPL)) / 2.0
                     CALL COLVEC (DOC, X, Y, CP(IPL), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     Y = (YPS(I,IPL,IPLOT) +
     *                  YPS(MIN(I+1,NP),IPL,IPLOT)) / 2.0
                     CALL COLVEC (DOC, X, Y, CP(IPL), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 50                  CONTINUE
                  END IF
               END IF
 60         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLINI2 (IERR)
C-----------------------------------------------------------------------
C   This routine will set up the location commons for the plot file.
C   Inputs from common:
C      BLC     R(2)    Bottom left corner of plot.
C      TRC     R(2)    Top right corner of plot.
C   Output:
C      IERR    I       Error code. 0=OK.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      REAL      XR, YR
      INTEGER   DEPT(5), I, LABEL, J, K
      LOGICAL   PFLG, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'TARPL.INC'
      INCLUDE 'TARPL.PLT'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
C                                       X and Y plot axis not related
C                                       to map axis.
      IF (XY.EQ.0.0) XY = 1.0
      DO 10 I = 1,5
         DEPT(I) = 1
 10      CONTINUE
C                                       Set up default scaling parms.
      XSCAL = 1.0
      XOFF = 0.0
      XRANGE = APARM(2) - APARM(1)
      APARM(1) = APARM(1) - 0.03 * XRANGE
      APARM(2) = APARM(2) + 0.03 * XRANGE
      XRANGE = APARM(2) - APARM(1)
      XSCAL = 16000.0 / XRANGE
      XOFF = - APARM(1) * XSCAL
      TRC(1) = 16000.0
      BLC(1) = 0.0
      K = 0
      TRC(2) = 16000.0
      BLC(2) = 0.0
      PBLC(1,1) = BLC(1)
      PTRC(1,1) = TRC(1)
      PBLC(1,2) = BLC(1)
      PTRC(1,2) = TRC(1)
      PBLC(2,1) = BLC(2)
      PBLC(2,2) = BLC(2) + (LINT / 1000.0) * (TRC(2) - BLC(2))
      PTRC(2,1) = PBLC(2,2)
      PTRC(2,2) = TRC(2)
C                                       two plot types
      DO 100 IPLOT = IPLOT1,IPLOT2
         LOCNUM = IPLOT
         YOFF(IPLOT) = 0.0
         YSCAL(IPLOT) = 0.0
         CALL SETLOC (DEPT, F)
         AXFUNC(1,LOCNUM) = 0
         AXFUNC(2,LOCNUM) = 0
         AXTYP(LOCNUM) = 0
         CORTYP(LOCNUM) = 0
         LABTYP(LOCNUM) = 0
C                                        proper scaling labels
         J = 1 + 2 * IPLOT
         YRANGE(IPLOT) = APARM(J+1) - APARM(J)
         APARM(J) = APARM(J) - 0.03 * YRANGE(IPLOT)
         APARM(J+1) = APARM(J+1) + 0.03 * YRANGE(IPLOT)
         YRANGE(IPLOT) = 1.06 * YRANGE(IPLOT)
C                                       Some kind of error
         IF ((XRANGE.LE.0.0) .OR. (YRANGE(IPLOT).LE.0.0)) THEN
            MSGTXT = 'PLOT RANGE <= 0'
            CALL MSGWRT (8)
            IERR = 5
            GO TO 999
            END IF
         YR = YRANGE(IPLOT)
         CALL METSCL (LABEL, YR, CPREF(2,LOCNUM), PFLG)
C                                       reset scaling parms
         IF (IPLOT.EQ.1) THEN
            YSCAL(IPLOT) = 16000.0 * (LINT/1000.) / YRANGE(IPLOT)
            YOFF(IPLOT) = - APARM(J) * YSCAL(IPLOT)
            RPLOC(2,LOCNUM) = PBLC(2,IPLOT)
         ELSE
            YSCAL(IPLOT) = 16000.0 * (1. - LINT/1000.) / YRANGE(IPLOT)
            YOFF(IPLOT) = - APARM(J) * YSCAL(IPLOT) + 16.0*LINT
            RPLOC(2,LOCNUM) = PBLC(2,IPLOT)
            END IF
C                                       labeling coordinates
         XR = XRANGE
         CALL METSCL (LABEL, XR, CPREF(1,LOCNUM), PFLG)
         RPLOC(1,LOCNUM) = PBLC(1,1)
         RPVAL(1,LOCNUM) = APARM(1) * XR / XRANGE
         RPVAL(2,LOCNUM) = APARM(J) * YR / YRANGE(IPLOT)
         AXINC(1,LOCNUM) = XR / XRANGE / XSCAL
         AXINC(2,LOCNUM) = YR / YRANGE(IPLOT) / YSCAL(IPLOT)
         CTYP(1,LOCNUM) = XUNIT
         CTYP(2,LOCNUM) = YUNIT(IPLOT)
         IF ((IPLOT.EQ.2) .AND. (IPLOT1.LT.IPLOT2)) THEN
            CPREF(1,LOCNUM) = ' '
            CTYP(1,LOCNUM) = ' '
            END IF
C                                       Left border in characters
         IF (LTYPE.GT.2) THEN
            CALL CHNTIC (PBLC(1,IPLOT), PTRC(1,IPLOT), I)
            K = MAX (K, I)
            END IF
 100     CONTINUE
C                                       Left border in characters
      CHOUT(1) = 0.5
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) THEN
         CHOUT(1) = 3.0
         IF (K.GT.0) CHOUT(1) = 4 + K
         END IF
C                                       Bottom border in characters
      CHOUT(2) = 0.5
      IF (LTYPE.GT.1) CHOUT(2) = CHOUT(2) + 1.5
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      YGAP = CHOUT(2) - 0.5 + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(2) =
     *    CHOUT(2)
C                                       Right border in characters
      CHOUT(3) = 0.5
C                                       Top border in characters
      CHOUT(4) = 0.5
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) + 1.5
      IF ((LABEL.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   CHOUT(4) = CHOUT(4) + 1.333
C                                       Write initialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (BLC, TRC, XY, CHOUT, DEPT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLABL2 (IERR)
C-----------------------------------------------------------------------
C   This program uses the values set in PLINI2 and passed through
C   commons LOCATI and PLTCOM to do the axes labeling.
C   Inputs from common:
C      /LOCATI/  (from incs:DLOC.INC)
C      /LOCATC/  (from incs:DLOC.INC)
C      /PLTARS/  (from TARPL.PLT)
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER  IERR
C
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), LABEL, J, LLTYPE
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, ANAME*18
      LOGICAL   F
      INCLUDE 'TARPL.PLT'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'TARPL.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LLTYPE = MOD (ABS(PCODES(1)), 100)
C                                        Tics and tic labels
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 100 IPLOT = IPLOT1,IPLOT2
         LOCNUM = IPLOT
         CALL CLAB1 (PBLC(1,IPLOT), PTRC(1,IPLOT), CHOUT, LABEL, XY, F,
     *      PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                        draw rectangle
         CALL GPOS (PBLC(1,IPLOT), PBLC(2,IPLOT), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (PTRC(1,IPLOT), PBLC(2,IPLOT), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (PTRC(1,IPLOT), PTRC(2,IPLOT), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (PBLC(1,IPLOT), PTRC(2,IPLOT), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GVEC (PBLC(1,IPLOT), PBLC(2,IPLOT), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       draw line at zero
         J = IPLOT * 2 + 1
         IF ((APARM(J).LT.0.0) .AND. (APARM(J+1).GT.0.0)) THEN
            DCY = -APARM(J) / (APARM(J+1)-APARM(J)) * YSCAL(IPLOT) +
     *         YOFF(IPLOT)
            CALL GPOS (PBLC(1,IPLOT), DCY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL GVEC (PTRC(1,IPLOT), DCY, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 100     CONTINUE
C                                       title line.
      IF ((LLTYPE.GT.1) .AND. (LLTYPE.LT.7)) THEN
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
            CALL H2CHR (18, 1, CATH(KHIMN), ANAME)
            CALL NAMEST (ANAME, CATBLK(KIIMS), TITLE(INCHAR:), I)
            CALL REFRMT (TITLE, '_', INCHAR)
            END IF
         CALL GPOS (PBLC(1,IPLOT2), PTRC(2,IPLOT2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DCX = 0.0
         DCY = .5
         IANGLE = 0
         CALL CHTRIM (TITLE, 80, TITLE, INCHAR)
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, TITLE, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Date/time version
      IF ((LABEL.GT.0) .AND. (LLTYPE.GT.1) .AND. (LLTYPE.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (WRKTXT,1020) IVER, ADATE, ATIME
         CALL REFRMT (WRKTXT, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GPOS (PBLC(1,IPLOT2), PTRC(2,IPLOT2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLOT FILE VERSION',I4,'__CREATED ',A,A)
      END
      SUBROUTINE COLVEC (DOC, X, Y, CVAL, PLTBLK, IERR)
C-----------------------------------------------------------------------
C   Plot a color vector
C   Input:
C      X        R        Draw to (X,Y)
C      Y        R        Draw to (X,Y)
C      CVAL     R        Color value 0 - 1 or 2-5 line type
C   In/Out
C      DOC      I        Init the color 1,-1; plot line 1,0
C                        set to 0 on return
C      PLTBLK   I(256)   plot block
C   Output
C      IERR     I        error code
C-----------------------------------------------------------------------
      REAL      X, Y, CVAL
      INTEGER   DOC, PLTBLK(256), IERR
C
      INTEGER   ITY, IROUND
      REAL      RR, GG, BB, CP, GAMMA, V
      PARAMETER (CP = 0.5)
      PARAMETER (GAMMA = 1.0 / 3.0)
C-----------------------------------------------------------------------
C                                       init the color
      IF (DOC.NE.0) THEN
C                                       simple line type
         IF (CVAL.GT.1.5) THEN
            ITY = IROUND (CVAL) - 1
            IF ((ITY.GE.1) .AND. (ITY.LE.4)) THEN
               CALL GLTYPE (ITY, PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
C                                       3-color
         ELSE IF (CVAL.GE.0.0) THEN
            V = MAX (0.0, MIN (1.0, CVAL))
            BB = 1.0 - V / CP
            RR = (V - CP) / (1.0 - CP)
            BB = MAX (0.0, MIN (1.0, BB))
            RR = MAX (0.0, MIN (1.0, RR))
            GG = 1.0 - RR - BB
            BB = BB ** GAMMA
            GG = GG ** GAMMA
            RR = RR ** GAMMA
            CALL G3VCOL (RR, GG, BB, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       plot vector: 3 color
      IF (DOC.GE.0) THEN
         IF ((CVAL.GE.0.0) .AND. (CVAL.LE.1.5)) THEN
            CALL PL3VEC (X, Y, IERR)
C                                       "no" color line type
         ELSE
            CALL PLVEC (X, Y, IERR)
            END IF
         END IF
      DOC = 0
C
 999  RETURN
      END
      SUBROUTINE DLINE (IT, DOC, XX, YY, CC, PLTBLK, IERR)
C-----------------------------------------------------------------------
C   Draws a dashed line
C   Inputs:
C      IT       I        Type: IT = # breaks between point 1 and 2
C      XX       R(2)     X value of points
C      YY       R(2)     Y value of points
C      CC       R        color
C   Outputs:
C      DOC      I        1 => change color on next line
C      PLTBLK   I(256)   Plot buffer
C      IERR     I        Error code
C-----------------------------------------------------------------------
      INTEGER   IT, DOC, PLTBLK(256), IERR
      REAL      XX(2), YY(2), CC
C
      REAL      DX, DY, X, Y, S
      INTEGER   I
C-----------------------------------------------------------------------
      S = 0.5
      IF (IT.GT.0) S = S / (2.0 * IT)
      DX = XX(2) - XX(1)
      DY = YY(2) - YY(1)
C                                       move to point first
      CALL COLVEC (DOC, XX(1), YY(1), CC, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       loop
      X = XX(1) + S * DX
      Y = YY(1) + S * DY
      DOC = 0
      DO 20 I = 1,MAX(1,IT)
         CALL COLVEC (DOC, X, Y, CC, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         DOC = 0
         X = X + 2.0 * S * DX
         Y = Y + 2.0 * S * DY
         IF (IT.GT.0) THEN
            CALL PLPOS (X, Y, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         X = X + 2.0 * S * DX
         Y = Y + 2.0 * S * DY
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLMAKE (NP, RPARM, IGTYPE, IERR)
C-----------------------------------------------------------------------
C   This routine will create and open a plot file, put it in the map
C   header and write the first record into the plot file.
C   PLMAKE assumes that the map/uv file has been marked write and will
C   change it to a read flag.  It would be nice if the defaults had
C   been filled into RPARM before this routine is called.
C   Inputs:
C      NP     I      Number of floating point words in parameter list
C      RPARM  R(NP)  AIPS parameters
C      IGTYPE I      Plot file type: 1 => misc.,  2 => CNTR,
C                     3 => GREYS,  4 => PROFL,  5 => SL2PL,  6 => PCNTR,
C                     7 => IMEAN,  8 => UVPLT,  9 => GNPLT, 10 => VBPLT,
C                    11 => PFPLn, 12 => GAPLT, 13 => PLCUB, 14 => IMVIM,
C                    15 => TAPLT, 16 => POSSM, 17 => SNPLT, 18 => KNTR,
C                    19 => UVHGM, 20 => ISPEC
C                    Use 1 unless your inputs match those of these
C                    tasks - or take a new number, but
C                    AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR   I      Error code. two digit, first digit indicates
C                    subroutine: 1: MAPOPN, 2: MADDEX, 3: ZPHFIL,
C                    4: GINIT, second digit indicates error code of
C                    that subroutine.
C   Version for TARPL only - not match public version
C-----------------------------------------------------------------------
      INTEGER   NP, IERR, IGTYPE
      REAL      RPARM(*)
C
      INTEGER   IWBLK(256), IGSIZE, IGLUN, IGFIND, I
      CHARACTER PHNAME*48
      LOGICAL   SAVE
      INCLUDE 'TARPL.PLT'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IGSIZE /0/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
      IF (IGTYPE.LT.1) IGTYPE = 1
C                                       Add plot file to header
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IMSTUF(7), IMSTUF(5), CATBLK, IWBLK, SAVE,
     *      'READ', IVER, IERR)
         IF (IERR.NE.0) THEN
            IERR = IERR + 20
            GO TO 999
            END IF
         END IF
C                                       Change file common
      IF (NCFILE.GT.0) THEN
         DO 15 I = 1,NCFILE
            IF ((FVOL(I).EQ.IMSTUF(7)) .AND. (FCNO(I).EQ.IMSTUF(5)))
     *         FRW(I) = 0
 15         CONTINUE
         END IF
C                                       Make physical filename
      CALL ZPHFIL ('PL', IMSTUF(7), IMSTUF(5), IVER, PHNAME, IERR)
      IF (IERR.NE.0) THEN
         IERR = IERR + 30
         GO TO 999
         END IF
C                                       Open plot file
      CALL GINIT (IMSTUF(7), IMSTUF(5), PHNAME, IGSIZE, IGTYPE, NP,
     *   RPARM, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN,
     *   IGFIND, IERR)
      IF (IERR.NE.0) THEN
         GPHIND = 0
         IERR = IERR + 40
         END IF
C
 999  RETURN
      END
      SUBROUTINE PLEND (ISTAT, IDEBUG)
C-----------------------------------------------------------------------
C   Do some plotting cleanup functions.  Write "end of plot" record,
C   close plot file, check for vectors that were off the plot.
C   Then terminates the task with a call to DIE.
C   Inputs:
C     ISTAT  I         0=successful completion, other=dies unnaturally.
C     IDEBUG I         > 0 => don't delete PL file despite errors
C   Version for TARPL
C-----------------------------------------------------------------------
      INTEGER   ISTAT, IDEBUG
C
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'TARPL.PLT'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVD.INC'
C-----------------------------------------------------------------------
C                                       Was plot even opened?
      IF ((GPHIND.GT.0) .OR. (TVIND.GT.0)) THEN
C                                       vectors off plot message
         IF (IOFFPL.GT.0) THEN
            WRITE (MSGTXT,1000) IOFFPL
            CALL MSGWRT (6)
            END IF
C                                       Attempt close partial graph
         IF (ISTAT.GT.0) THEN
            MSGTXT = 'ERROR DURING GRAPHING WILL TRY TO FINISH' //
     *         ' PARTIAL GRAPH'
            CALL MSGWRT (6)
            END IF
C                                       Close plot file
         CALL GFINIS (PLTBLK, IERR)
C                                       OK finish - do HI file
         IF (.NOT.DOTV) THEN
            IF ((IERR.EQ.0) .OR. (IDEBUG.GT.0)) THEN
               CALL HIPLOT (IMSTUF(7), IMSTUF(5), IVER, PLTBLK, IERR)
               WRITE (MSGTXT,1020) IVER
               CALL MSGWRT (2)
C                                       Finish not sucessful. Destroy.
            ELSE
               CALL ZCLOSE (GPHLUN, GPHIND, IERR)
               CALL ZDESTR (GPHVOL, GPHNAM, IERR)
               CALL DELEXT ('PL', IMSTUF(7), IMSTUF(5), 'RDRD', CATBLK,
     *            PLTBLK, IVER, IERR)
               END IF
            END IF
         END IF
C                                       Close down task
      CALL DIE (ISTAT, PLTBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I7,' VECTORS TRUNCATED AT EDGES OF PLOT.')
 1020 FORMAT ('Plot file version',I4,' created')
      END
      SUBROUTINE PLVEC (X, Y, IERR)
C-----------------------------------------------------------------------
C   This routine will put a 'draw vector' command in a plot file.
C   Vectors that are too big are interpolated.
C   Inputs:
C      X       R    X value.
C      Y       R    Y value.
C   Common:
C      /PLTCOM/
C   Output:
C      IERR    I    Error code.  0 means OK.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   IERR
C
      REAL      XX, YY, XT(2), YT(2)
      INTEGER   NO
      INCLUDE 'TARPL.PLT'
C-----------------------------------------------------------------------
      XX = XSCAL * X + XOFF
      YY = YSCAL(IPLOT) * Y + YOFF(IPLOT)
C                                       Test edge intersections
      CALL INTEDG (PBLC(1,IPLOT), PTRC(1,IPLOT), XX, YY, XLAST, YLAST,
     *   NO, XT, YT)
C                                       Vector all in plot
      IF (NO.LT.0) THEN
         CALL GVEC (XX, YY, PLTBLK, IERR)
C                                       Both points off plot
      ELSE IF (NO.EQ.0) THEN
         IOFFPL = IOFFPL + 1
      ELSE IF (NO.NE.1) THEN
         CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
         IF (IERR.EQ.0) CALL GVEC (XT(2), YT(2), PLTBLK, IERR)
C                                       This point off plot
      ELSE IF ((XX.LT.PBLC(1,IPLOT)) .OR. (XX.GT.PTRC(1,IPLOT)) .OR.
     *   (YY.LT.PBLC(2,IPLOT)) .OR. (YY.GT.PTRC(2,IPLOT))) THEN
         IOFFPL = IOFFPL + 1
         CALL GVEC (XT(1), YT(1), PLTBLK, IERR)
C                                       Last point off plot
      ELSE
         CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
         IF (IERR.EQ.0) CALL GVEC (XX, YY, PLTBLK, IERR)
         END IF
C                                       Save last desired position
      XLAST = XX
      YLAST = YY
C
 999  RETURN
      END
      SUBROUTINE PLPOS (X, Y, IERR)
C-----------------------------------------------------------------------
C   This routine will put a 'position vector' command in a plot file.
C   Inputs:
C      X       R    X value.
C      Y       R    Y value.
C      COMMON /PLTCOM/
C   Output:
C      IERR    I    Error code.  0 means OK.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   IERR
C
      REAL      XX, YY
      INCLUDE 'TARPL.PLT'
C-----------------------------------------------------------------------
      XX = XSCAL * X + XOFF
      YY = YSCAL(IPLOT) * Y + YOFF(IPLOT)
C                                       Position off plot: flag
      IF ((XX.LT.PBLC(1,IPLOT)) .OR. (XX.GT.PTRC(1,IPLOT)) .OR.
     *   (YY.LT.PBLC(2,IPLOT)) .OR. (YY.GT.PTRC(2,IPLOT))) THEN
         IOFFPL = IOFFPL + 1
C                                       Put position in plot file
      ELSE
         CALL GPOS (XX, YY, PLTBLK, IERR)
         END IF
C                                       Save last position
      XLAST = XX
      YLAST = YY
C
 999  RETURN
      END
      SUBROUTINE PL3VEC (X, Y, IERR)
C-----------------------------------------------------------------------
C   This routine will put a 'draw color vector' command in a plot file.
C   Vectors that are too big are interpolated.
C   Inputs:
C      X       R    X value.
C      Y       R    Y value.
C   Common:
C      /PLTCOM/
C   Output:
C      IERR    I    Error code.  0 means OK.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   IERR
C
      REAL      XX, YY, XT(2), YT(2)
      INTEGER   NO
      INCLUDE 'TARPL.PLT'
C-----------------------------------------------------------------------
      XX = XSCAL * X + XOFF
      YY = YSCAL(IPLOT) * Y + YOFF(IPLOT)
C                                       Test edge intersections
      CALL INTEDG (PBLC(1,IPLOT), PTRC(1,IPLOT), XX, YY, XLAST, YLAST,
     *   NO, XT, YT)
C                                       Vector all in plot
      IF (NO.LT.0) THEN
         CALL G3VEC (XX, YY, PLTBLK, IERR)
C                                       No points hit plot
      ELSE IF (NO.EQ.0) THEN
         IOFFPL = IOFFPL + 1
C                                       Both points off plot
      ELSE IF (NO.NE.1) THEN
         CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
         IF (IERR.EQ.0) CALL G3VEC (XT(2), YT(2), PLTBLK, IERR)
C                                       This point off plot
      ELSE IF ((XX.LT.PBLC(1,IPLOT)) .OR. (XX.GT.PTRC(1,IPLOT)) .OR.
     *   (YY.LT.PBLC(2,IPLOT)) .OR. (YY.GT.PTRC(2,IPLOT))) THEN
         IOFFPL = IOFFPL + 1
         CALL G3VEC (XT(1), YT(1), PLTBLK, IERR)
C                                       Last point off plot
      ELSE
         CALL GPOS (XT(1), YT(1), PLTBLK, IERR)
         IF (IERR.EQ.0) CALL G3VEC (XX, YY, PLTBLK, IERR)
         END IF
C                                       Save last desired position
      XLAST = XX
      YLAST = YY
C
 999  RETURN
      END
