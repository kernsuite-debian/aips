LOCAL INCLUDE 'PCHIS.INC'
C                                       PLTLAB declarations.
      INTEGER   MAXPNT
      PARAMETER (MAXPNT = 100000)
C
      REAL       CHOUT(4), YGAP, YP(MAXPNT), XMAX, XMIN, YMAX, YMIN,
     *   BLC(7), TRC(7), HMAX, HMIN, HIST(1026), AVG, RMS, HAVG, HRMS
      INTEGER    NTEXT, PCODES(5), NP, XF, YF, NBOXES, NLOW, NHIGH
      CHARACTER  TEXT(2)*80, XUNIT*24, YUNIT*24, TITLE*80, FUNC*2
C                                       Plot labeling parameters.
      COMMON /PLTLAB/ CHOUT, YGAP, NTEXT, PCODES, YP, XMAX, XMIN, YMAX,
     *   YMIN, NP, BLC, TRC, XF, YF, HIST, NBOXES, HMAX, HMIN, AVG, RMS,
     *   HAVG, HRMS, NLOW, NHIGH
      COMMON /PLCLAB/ TEXT, XUNIT, YUNIT, TITLE, FUNC
C                                       INPARM declarations.
      HOLLERITH XNAMIN(3), XCLSIN(2), XINFIL(12), XFUNC
      REAL      XSEQ, XDISK, APARM(10), XBOXES, XLTYPE, XYRATO, XDOTV,
     *   XGRCH
C                                       Parameters from AIPS.
      COMMON /INPARM/ XNAMIN, XCLSIN, XSEQ, XDISK, XINFIL, APARM, XFUNC,
     *   XBOXES, XLTYPE, XYRATO, XDOTV, XGRCH
C
LOCAL END
      PROGRAM PCHIS
C-----------------------------------------------------------------------
C! General plot task for histograms of data from text file
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2017
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
C   PCHIS is a basic task to read X, Y labels and data from a text file
C    Inputs:   (from AIPS)
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
C                                       local declarations
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IERR, IDEBUG, IPTYPE, IROUND, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'PCHIS.INC'
      DATA PRGNAM /'PCHIS '/
C-----------------------------------------------------------------------
C                                       Copy from DATA to Commons
      IBLKSZ = MABFSS
      GPHIND = 0
C                                       Don't delete plot file on error
      IDEBUG = 1
C                                       This is the number of REAL*4
C                                       words to get from AIPS.
      NPARMS = 35
C                                       Plot type PCHIS paraform
      IPTYPE = 26
      CALL FILL (5, 0, PCODES)
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PCHISI (PRGNAM, NPARMS, IERR)
C                                       Labeling type:
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
C                                       Do plotting.
      IF (IERR.EQ.0) CALL PLOTER (NPARMS, IPTYPE, IERR)
C                                       Shutdown.
      CALL PLEND (IERR, IDEBUG)
C
 999  STOP
      END
      SUBROUTINE PCHISI (PRGNAM, NPARMS, IERR)
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
      CHARACTER NAME*36, FNAME*12, FCLASS*6, FPTYPE*2, STAT*4,
     *   LINE*1000, INFILE*48, RTEXT(2)*80
      INTEGER   IWORK(256), FSEQ, FVOL, FUSID, TLUN, TIND, KBP, IMLUN,
     *   I, JTRIM, IP, J, KBLIM
      REAL      CUTL, CUTH
      DOUBLE PRECISION X(100), D
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PCHIS.INC'
      DATA TLUN, IMLUN /3, 16/
C-----------------------------------------------------------------------
C                                       Get parameters from AIPS, init
C                                       AIPS I/O, other startup things.
      CALL SETUP (PRGNAM, NPARMS, XNAMIN, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open the map file.

      CALL H2CHR (12, 1, XNAMIN, FNAME)
      CALL H2CHR (6, 1, XCLSIN, FCLASS)
      CALL H2CHR (2, 1, XFUNC, FUNC)
      FSEQ = XSEQ + 0.01
      FVOL = XDISK + 0.01
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
      IF (APARM(5).LE.0.0) APARM(5) = 1.0
      RTEXT(1) = ' '
      RTEXT(2) = ' '
C                                       OPEN text file to get info
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL ZTXOPN ('READ', TLUN, TIND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read data
      NP = 0
      XMAX = -1.E10
      YMAX = XMAX
      YMIN = -YMAX
      XMIN = YMIN
      AVG = 0.0
      RMS = 0.0
      IP = APARM(5) + 0.1
      IP = MAX (1, MIN (100, IP))
C                                       read loop
 50   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.EQ.0) THEN
         KBLIM = JTRIM (LINE)
         IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 50
         KBP = 1
         DO 55 I = 1,100
            CALL GETNUM (LINE, KBLIM, KBP, X(I))
            IF (X(IP).EQ.DBLANK) GO TO 60
 55         CONTINUE
 60      IF ((I.GT.IP) .AND. (X(IP).NE.DBLANK)) THEN
            NP = NP + 1
            YP(NP) = X(IP)
            AVG = AVG + YP(NP)
            RMS = RMS + YP(NP)*YP(NP)
            YMIN = MIN (YMIN, YP(NP))
            YMAX = MAX (YMAX, YP(NP))
            END IF
         GO TO 50
      ELSE IF (IERR.EQ.2) THEN
         CALL ZTXCLS (TLUN, TIND, IERR)
      ELSE
         WRITE (MSGTXT,1000) IERR, 'READING THE TEXT FILE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       average
      IF (NP.GT.0) THEN
         AVG = AVG / NP
         RMS = RMS / NP - AVG * AVG
         RMS = SQRT (MAX (0.0, RMS))
      ELSE
         MSGTXT = 'NO VALID SAMPLES FOUND'
         CALL MSGWRT (8)
         IERR = 10
         GO TO 999
         END IF
C                                       number extreme
      NLOW = 0
      NHIGH = 0
      I = APARM(6) + 0.5
      IF (I.LE.1) I = 4
      APARM(6) = I
      CUTL = AVG - I * RMS
      CUTH = AVG + I * RMS
      DO 90 I = 1,NP
         IF (YP(I).LT.CUTL) NLOW = NLOW + 1
         IF (YP(I).GT.CUTH) NHIGH = NHIGH + 1
 90      CONTINUE
C                                       compute histogram
      IF (APARM(4).GT.APARM(3)) THEN
         YMIN = APARM(3)
         YMAX = APARM(4)
         END IF
      CALL RFILL (1026, 0.0, HIST)
      NBOXES = XBOXES + 0.1
      NBOXES = MAX (128, MIN (1024, NBOXES))
      D = (YMAX - YMIN) / NBOXES
      DO 100 I = 1,NP
         J = 2 + (YP(I) - YMIN) / D
         J = MAX (1, MIN (NBOXES+2, J))
         HIST(J) = HIST(J) + 1.0
 100     CONTINUE
      HAVG = (AVG - YMIN) / D + 2.0
      HRMS = RMS / D
      HMAX = -1000.
      HMIN = 1.E8
      DO 110 J = 2,NBOXES+1
         IF (FUNC.EQ.'LG') THEN
            IF (HIST(J).GT.0.0) THEN
               HIST(J) = LOG10 (HIST(J))
            ELSE
               HIST(J) = -1.0
               END IF
            END IF
         HMAX = MAX (HMAX, HIST(J))
         HMIN = MIN (HMIN, HIST(J))
 110     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PCHISI: ERROR',I4,' ON ',A)
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
      INCLUDE 'PCHIS.INC'
      REAL      X, AX(5), AY(5), DX, DY
      INTEGER   I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set corner values.
      APARM(1) = 1
      APARM(2) = NBOXES + 1
      BLC(1) = APARM(1)
      TRC(1) = APARM(2)
      X = TRC(1) - BLC(1)
      BLC(1) = BLC(1) - 0.05*X
      TRC(1) = TRC(1) + 0.05*X
      BLC(2) = HMIN
      TRC(2) = HMAX
      X = TRC(2) - BLC(2)
      BLC(2) = BLC(2) - 0.05*X
      TRC(2) = TRC(2) + 0.05*X
C                                       Draw a square plot.
      IF (XYRATO.LE.0.0) XYRATO = 1
      XY = XYRATO * (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       ** End plot specific statements.
C                                       Create and open plot file.
      CALL PLMAKE (NPARMS, XNAMIN, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up commons for plotting.
      CALL PLINI3 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL3 (IERR)
      IF (IERR.NE.0) GO TO 999
      DX = 0.005 * (TRC(1) - BLC(1))
      DY = 0.005 * (TRC(2) - BLC(2))
      DX = DX / SQRT (XY)
      DY = DY * SQRT (XY)
C                                       Line drawing:
      DO 100 I = 1,NBOXES
         AX(1) = I
         AX(2) = AX(1)
         AX(3) = I + 1
         AX(4) = AX(3)
         AY(1) = HMIN
         AY(2) = HIST(I+1)
         AY(3) = AY(2)
         AY(4) = AY(1)
         CALL PLPOS (AX(1), AY(1), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(2), AY(2), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(3), AY(3), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(4), AY(4), IERR)
         IF (IERR.NE.0) GO TO 999
 100     CONTINUE
C                                       mark mean and rms
      CALL GLTYPE (4, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      AY(1) = HMAX + DY
      AY(2) = HMAX - DY
      AX(1) = HAVG
      CALL PLPOS (AX(1), AY(1), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLVEC (AX(1), AY(2), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLPOS (HAVG-DX, HMAX, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLVEC (HAVG+DX, HMAX, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 110 I = 1,5
         AX(1) = HAVG - I * HRMS
         CALL PLPOS (AX(1), AY(1), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(1), AY(2), IERR)
         IF (IERR.NE.0) GO TO 999
         AX(1) = HAVG + I * HRMS
         CALL PLPOS (AX(1), AY(1), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(1), AY(2), IERR)
         IF (IERR.NE.0) GO TO 999
 110     CONTINUE
      AY(1) = HMIN
      AY(2) = HMIN - 2 * DY
      AX(1) = HAVG
      CALL PLPOS (AX(1), AY(1), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLVEC (AX(1), AY(2), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLPOS (HAVG-DX, HMIN-DY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL PLVEC (HAVG+DX, HMIN-DY, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 120 I = 1,5
         AX(1) = HAVG - I * HRMS
         CALL PLPOS (AX(1), AY(1), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(1), AY(2), IERR)
         IF (IERR.NE.0) GO TO 999
         AX(1) = HAVG + I * HRMS
         CALL PLPOS (AX(1), AY(1), IERR)
         IF (IERR.NE.0) GO TO 999
         CALL PLVEC (AX(1), AY(2), IERR)
         IF (IERR.NE.0) GO TO 999
 120     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE PLINI3 (IERR)
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
      REAL      XRANGE, YRANGE, XR, YR
      INTEGER   DEPT(5), I, LABEL, LTYPE, J, K
      LOGICAL   PFLG, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PCHIS.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      LABEL = PCODES(1)
      LTYPE = MOD (ABS(LABEL), 100)
C                                       Set up default scaling parms.
      XSCAL = 1.0
      YSCAL = 1.0
      XOFF = 0.0
      YOFF = 0.0
C                                       X and Y plot axis not related
C                                       to map axis.
      IF (XY.EQ.0.0) XY = 1.0
      DO 320 I = 1,5
         DEPT(I) = 1
 320     CONTINUE
      LOCNUM = 1
      CALL SETLOC (DEPT, F)
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
C                                        proper scaling labels
      XRANGE = TRC(1) - BLC(1)
      YRANGE = TRC(2) - BLC(2)
C                                       Some kind of error
      IF ((XRANGE.GT.0.0) .AND. (YRANGE.GT.0.0)) GO TO 325
         IERR = 5
         GO TO 999
 325  XR = XRANGE
      CALL METSCL (LABEL, XR, CPREF(1,LOCNUM), PFLG)
      YR = YRANGE
      CALL METSCL (LABEL, YR, CPREF(2,LOCNUM), PFLG)
C                                        proceed filling /LOCATI/
      CALL RCOPY (2, BLC, PBLC)
      CALL RCOPY (2, TRC, PTRC)
      NTEXT = 2
      J = HIST(1) + 0.1
      K = HIST(NBOXES+2) + 0.1
      I = APARM(6) + 0.1
      WRITE (TEXT(1),1325) J, K, NP, I, NLOW, NHIGH
      WRITE (TEXT(2),1326) YMIN, YMAX, AVG, RMS
C                                       reset scaling parms
      XSCAL = 16000.0 / XRANGE
      XOFF = - BLC(1) * XSCAL
      PTRC(1) = 16000.0
      PBLC(1) = 0.0
      YSCAL = 16000.0 / YRANGE
      YOFF = - BLC(2) * YSCAL
      PTRC(2) = 16000.0
      PBLC(2) = 0.0
      XY = XY * (YSCAL / XSCAL)
C                                       labeling coordinates
      XUNIT = 'amplitude'
      YUNIT = 'counts'
      IF (FUNC.EQ.'LG') YUNIT = 'log10(count)'
      RPLOC(1,LOCNUM) = PBLC(1)
      RPLOC(2,LOCNUM) = PBLC(2)
      RPVAL(1,LOCNUM) = BLC(1) * XR / XRANGE
      RPVAL(2,LOCNUM) = BLC(2) * YR / YRANGE
      AXINC(1,LOCNUM) = XR / XRANGE / XSCAL
      AXINC(2,LOCNUM) = YR / YRANGE / YSCAL
      CTYP(1,LOCNUM) = XUNIT
      CTYP(2,LOCNUM) = YUNIT
C                                       Left border in characters
      CHOUT(1) = 0.5
      IF (LTYPE.EQ.2) CHOUT(1) = 2.5
      IF (LTYPE.GT.2) THEN
         CHOUT(1) = 3.0
         CALL CHNTIC (PBLC, PTRC, I)
         IF (I.GT.0) CHOUT(1) = 4 + I
         END IF
C                                       Bottom border in characters
      CHOUT(2) = 0.5
      IF (LTYPE.GT.1) CHOUT(2) = CHOUT(2) + 1.5
      IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      YGAP = CHOUT(2) - 0.5 + 1.333
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(2) =
     *    CHOUT(2) + NTEXT * 1.333
C                                       Right border in characters
      CHOUT(3) = 0.5
C                                       Top border in characters
      CHOUT(4) = 0.5
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) + 1.5
      IF ((PCODES(1).GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) =
     *   CHOUT(4) + 1.333
C                                       Write initialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (PBLC, PTRC, XY, CHOUT, DEPT, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1325 FORMAT ('Samples below',I6,' above',I6,' total',I10,I3,
     *   ' sigma out',2I4)
 1326 FORMAT ('PixRange=',2F8.5,'  Avg, rms',2F8.5)
      END
      SUBROUTINE PLABL3 (IERR)
C-----------------------------------------------------------------------
C   This program uses the values set in PLINI3 and passed through
C   commons LOCATI and PLTCOM to do the axes labeling.
C   Inputs from common:
C      /LOCATI/  (from incs:DLOC.INC)
C      /LOCATC/  (from incs:DLOC.INC)
C      /PLTCOM/  (from incs:DPLT.INC)
C   Output:
C      IERR   I      Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER  IERR
C
      REAL      DCX, DCY
      INTEGER   I, IANGLE, INCHAR, IT(3), ID(3), LABEL, LTYPE
      CHARACTER WRKTXT*80, ATIME*8, ADATE*12, ANAME*18
      LOGICAL   F
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PCHIS.INC'
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
      IF (IERR.NE.0) GO TO 999
C                                       title line.
      IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
C                                       Standard title
         TITLE = 'Histogram:'
         IF (PCODES(4).EQ.0) THEN
            CALL H2CHR (8, 1, CATH(KHOBJ), TITLE)
            INCHAR = 12
            IF (TITLE.EQ.' ') INCHAR = 1
            IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
            CALL H2CHR (18, 1, CATH(KHIMN), ANAME)
            CALL NAMEST (ANAME, CATBLK(KIIMS), TITLE(INCHAR:), I)
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
      IF ((LABEL.GT.1) .AND. (LABEL.LT.7)) THEN
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
C                                       Text at bottom.
      IF ((NTEXT.GT.0) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7))
     *   THEN
         DCX = 0.0
         IANGLE = 0
         DO 50 I = 1,NTEXT
            WRKTXT = TEXT(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
 50         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLOT FILE VERSION',I4,'__CREATED ',A,A)
      END
