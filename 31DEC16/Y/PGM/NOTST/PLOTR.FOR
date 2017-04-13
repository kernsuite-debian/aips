LOCAL INCLUDE 'PLOTR.INC'
C                                       PLTLAB declarations.
      INTEGER   MAXPNT, MAXTYP
      PARAMETER (MAXTYP = 30)
      PARAMETER (MAXPNT = 100000)
C
      REAL       CHOUT(4), YGAP, XP(MAXPNT), YP(MAXPNT), CP(MAXPNT),
     *   XMAX, XMIN, YMAX, YMIN, BLC(7), TRC(7)
      INTEGER    NTEXT, PCODES(5), NP, SP(MAXPNT), SMIN, SMAX, XF, YF,
     *   STTYPE(MAXTYP), LITYPE(MAXTYP), NFTYPE(MAXTYP)
      CHARACTER  TEXT(2)*80, XUNIT*24, YUNIT*24, TITLE*80,
     *   STEXT(MAXTYP)*60
C                                       Plot labeling parameters.
      COMMON /PLTLAB/ CHOUT, YGAP, NTEXT, PCODES, XP, YP, CP, XMAX,
     *   XMIN, YMAX, YMIN, NP, BLC, TRC, SP, SMIN, SMAX, XF, YF, STTYPE,
     *   LITYPE, NFTYPE
      COMMON /PLCLAB/ TEXT, XUNIT, YUNIT, TITLE, STEXT
C                                       INPARM declarations.
      HOLLERITH XNAMIN(3), XCLSIN(2), XINFIL(12)
      REAL      PRUSER, XSEQ, XDISK, APARM(10), RPARM(30), VPARM(30),
     *   FPARM(30), FACTOR, DOCOL, XLTYPE, XYRATO, XDOTV, XGRCH
C                                       Parameters from AIPS.
      COMMON /INPARM/ PRUSER, XNAMIN, XCLSIN, XSEQ, XDISK, XINFIL,
     *   APARM, RPARM, VPARM, FPARM, FACTOR, DOCOL, XLTYPE, XYRATO,
     *   XDOTV, XGRCH
C
LOCAL END
LOCAL INCLUDE 'PLOTR2.INC'
      INTEGER   MAXPP
      PARAMETER (MAXPP = 100000)
      INTEGER   MAXORD
      PARAMETER (MAXORD = 12)
C
      INTEGER   JJC, NPTS
      REAL      XVAL(MAXPP), YVAL(MAXPP), AARRAY(MAXORD,MAXORD),
     *   CARRAY(MAXORD,MAXORD), GAMMA(MAXORD), MOMENT(2*MAXORD),
     *   POLYFN(MAXORD), POLAVG(MAXORD), POLXFN(MAXPP,MAXORD),
     *   XBAR
      COMMON /GDATA/ POLXFN, XVAL, YVAL, AARRAY, CARRAY, GAMMA, MOMENT,
     *   XBAR, POLYFN, POLAVG, JJC, NPTS
LOCAL END
      PROGRAM PLOTR
C-----------------------------------------------------------------------
C! General plot task labels, data from file
C# Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1999, 2001-2005, 2009-2010, 2012, 2014-2015
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
C   PLOTR is a basic task to read X, Y labels and data from a text file
C    Inputs:   (from AIPS)
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
C                                       local declarations
      CHARACTER PRGNAM*6
      INTEGER   NPARMS, IERR, IDEBUG, IPTYPE, IROUND, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'PLOTR.INC'
      DATA PRGNAM /'PLOTR '/
C-----------------------------------------------------------------------
C                                       Copy from DATA to Commons
      IBLKSZ = MABFSS
      GPHIND = 0
C                                       Don't delete plot file on error
      IDEBUG = 1
C                                       This is the number of REAL*4
C                                       words to get from AIPS.
      NPARMS = 126
C                                       Plot type PLOTR paraform
      IPTYPE = 26
      CALL FILL (5, 0, PCODES)
C                                       Get parms from AIPS, open map
C                                       file, create plot file,
      CALL PLOTRI (PRGNAM, NPARMS, IERR)
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
      SUBROUTINE PLOTRI (PRGNAM, NPARMS, IERR)
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
     *   INFILE*48, RTEXT(2)*80
      INTEGER   IWORK(256), FSEQ, FVOL, FUSID, TLUN, TIND, KBP, IMLUN,
     *   I, IROUND, JJ, DROUND, JT, JTRIM
      DOUBLE PRECISION X, PFUNC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PLOTR.INC'
      DATA TLUN, IMLUN /3, 16/
C-----------------------------------------------------------------------
C                                       Get parameters from AIPS, init
C                                       AIPS I/O, other startup things.
      CALL SETUP (PRGNAM, NPARMS, PRUSER, IWORK, IERR)
      IF (IERR.NE.0) GO TO 999
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
      IF (APARM(5).EQ.0.0) APARM(5) = 1.0
      IF (APARM(8).EQ.0.0) APARM(8) = 1.0
      XF = APARM(7) + 1.1
      YF = APARM(10) + 1.1
      IF ((XF.LT.1) .OR. (XF.GT.10)) XF = 1
      IF ((YF.LT.1) .OR. (YF.GT.10)) YF = 1
      APARM(7) = XF - 1
      APARM(10) = YF - 1
      RTEXT(1) = ' '
      CALL FSTRNG (XF, APARM(5), 1, RTEXT(1))
      RTEXT(2) = ' '
      CALL FSTRNG (YF, APARM(8), 2, RTEXT(2))
C                                       OPEN text file to get info
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL ZTXOPN ('READ', TLUN, TIND, INFILE, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read title
 10   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 10
      TITLE = LINE
      IF (TITLE.NE.' ') PCODES(4) = 1
C                                       Read Line 1
 15   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 15
      TEXT(1) = LINE
C                                       Read Line 2
 20   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 20
      TEXT(2) = LINE
      NTEXT = 0
      IF ((RTEXT(1).NE.' ') .AND. (RTEXT(2).NE.' ')) THEN
         TEXT(1) = RTEXT(1)
         TEXT(2) = RTEXT(2)
      ELSE IF (RTEXT(1).NE.' ') THEN
         IF (TEXT(1).EQ.' ') THEN
            TEXT(1) = RTEXT(1)
         ELSE
            TEXT(2) = RTEXT(1)
            END IF
      ELSE IF (RTEXT(2).NE.' ') THEN
         IF (TEXT(1).EQ.' ') THEN
            TEXT(1) = RTEXT(2)
         ELSE
            TEXT(2) = RTEXT(2)
            END IF
         END IF
      IF (TEXT(1).EQ.' ') THEN
         TEXT(1) = TEXT(2)
         TEXT(2) = ' '
         IF (TEXT(1).NE.' ') NTEXT = 1
      ELSE
         NTEXT = 1
         IF (TEXT(2).NE.' ') NTEXT = 2
         END IF
C                                       Read Xunits
 25   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 25
      XUNIT = LINE(:24)
C                                       Read Yunits
 30   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.NE.0) GO TO 999
      JT = JTRIM (LINE)
      IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 30
      YUNIT = LINE(:24)
C                                       gather parameters
      DO 40 I = 1,MAXTYP
         STTYPE(I) = IROUND (RPARM(I))
         IF (STTYPE(I).GT.23) STTYPE(I) = 1
         LITYPE(I) = IROUND (VPARM(I))
         NFTYPE(I) = IROUND (FPARM(I))
         NFTYPE(I) = MAX (-8, MIN (8, NFTYPE(I)))
         IF ((NFTYPE(I).NE.0) .AND. (LITYPE(I).LE.0)) LITYPE(I) = 2
         STEXT(I) = ' '
 40      CONTINUE
C                                       Read data
      NP = 0
      XMAX = -1.E10
      YMAX = XMAX
      YMIN = -YMAX
      XMIN = YMIN
      SMIN = 11
      SMAX = 0
 50   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF (IERR.EQ.0) THEN
         JT = JTRIM (LINE)
         IF ((LINE(:1).EQ.'$') .OR. (LINE(:1).EQ.'#')) GO TO 50
C                                       Read symbol labels
         IF ((LINE(:2).EQ.'SL') .OR. (LINE(:2).EQ.'sl')) THEN
            KBP = 3
            CALL GETNUM (LINE, 80, KBP, X)
            IF (X.NE.DBLANK) THEN
               I = DROUND (X)
               I = MAX (1, MIN (MAXTYP, ABS(I)))
               CALL CHTRIM (LINE(KBP:), 81-KBP, STEXT(I), JJ)
               END IF
            GO TO 50
            END IF
         KBP = 1
         CALL GETNUM (LINE, 80, KBP, X)
         X = PFUNC (XF, APARM(5), X)
         IF (X.NE.DBLANK) THEN
            XP(NP+1) = X
            CALL GETNUM (LINE, 80, KBP, X)
            X = PFUNC (YF, APARM(8), X)
            IF (X.NE.DBLANK) THEN
               YP(NP+1) = X
               CALL GETNUM (LINE, 80, KBP, X)
               IF (X.NE.DBLANK) THEN
                  IF (X.LT.0.0) THEN
                     JJ = X - 0.1
                  ELSE
                     JJ = X + 0.1
                     END IF
                  IF (JJ.EQ.0) JJ = 1
                  CALL GETNUM (LINE, 80, KBP, X)
                  CP(NP+1) = -1.0
                  IF ((X.NE.DBLANK) .AND. (DOCOL.GT.0.0)) THEN
                     CP(NP+1) = X
                     IF (X.GT.1.0D0) CP(NP+1) = IROUND (CP(NP+1))
                     END IF
                  IF ((ABS(JJ).LE.MAXTYP) .AND. ((STTYPE(ABS(JJ)).GT.0)
     *               .OR. (LITYPE(ABS(JJ)).GT.0))) THEN
                     NP = NP + 1
                     SP(NP) = JJ
                     XMAX = MAX (XMAX, XP(NP))
                     YMAX = MAX (YMAX, YP(NP))
                     SMAX = MAX (SMAX, ABS(SP(NP)))
                     XMIN = MIN (XMIN, XP(NP))
                     YMIN = MIN (YMIN, YP(NP))
                     SMIN = MIN (SMIN, ABS(SP(NP)))
                     END IF
                  END IF
               END IF
           END IF
         GO TO 50
      ELSE IF (IERR.EQ.2) THEN
         CALL ZTXCLS (TLUN, TIND, IERR)
      ELSE
         GO TO 999
         END IF
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION PFUNC (FT, FP, X)
C-----------------------------------------------------------------------
C   Function of parameters
C   Input:
C      FT   I      Function type code: 1 - 10
C      FP   R(2)   Scale X first by x = FP(1)*X + FP(2)
C      X    D      Input value
C   Output:
C      PFUNC   D      Function of scaled X
C-----------------------------------------------------------------------
      INTEGER   FT
      REAL      FP(2)
      DOUBLE PRECISION X
C
      DOUBLE PRECISION Y
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      PFUNC = DBLANK
      IF (X.NE.DBLANK) THEN
         Y = FP(1) * X + FP(2)
         IF (FT.EQ.1) THEN
            PFUNC = Y
         ELSE IF (FT.EQ.2) THEN
            IF (Y.GT.0.0D0) PFUNC = LOG (Y)
         ELSE IF (FT.EQ.3) THEN
            IF (Y.GT.0.0D0) PFUNC = LOG10 (Y)
         ELSE IF (FT.EQ.4) THEN
            PFUNC = EXP (Y)
         ELSE IF (FT.EQ.5) THEN
            PFUNC = Y * Y
         ELSE IF (FT.EQ.6) THEN
            PFUNC = Y * Y * Y
         ELSE IF (FT.EQ.7) THEN
            IF (Y.GE.0.0D0) PFUNC = SQRT (Y)
            IF (Y.LT.0.0D0) PFUNC = -SQRT (-Y)
         ELSE IF (FT.EQ.8) THEN
            IF (Y.GE.0.0D0) PFUNC = (Y) ** 0.333
            IF (Y.LT.0.0D0) PFUNC = (-Y) ** 0.333
         ELSE IF (FT.EQ.9) THEN
            IF (Y.NE.0.0D0) PFUNC = 1.0D0 / Y
         ELSE IF (FT.EQ.10) THEN
            IF (Y.GT.0.0D0) PFUNC = 1.0D0 / SQRT (Y)
            IF (Y.GE.0.0D0) PFUNC = SQRT (Y)
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE FSTRNG (FT, FP, COOR, STRING)
C-----------------------------------------------------------------------
C   Makes a descriptive string
C   Input:
C      FT       I       Function type
C      FP       R(2)    Function parameters
C      COOR     I       1 = X, 2 = Y
C   Output
C      String   C*80    String - not changed if FT=1, FP=1,0
C-----------------------------------------------------------------------
      INTEGER   FT, COOR
      REAL      FP(2)
      CHARACTER STRING*(*)
C
      INTEGER   NP
      REAL      R
C-----------------------------------------------------------------------
      IF ((FT.NE.1) .OR. (FP(1).NE.1.0) .OR. (FP(2).NE.0.0)) THEN
         STRING = 'Xplot = '
         NP = 9
         IF (COOR.EQ.2) STRING(1:1) = 'Y'
         IF (FT.EQ.2) THEN
            STRING(NP:) = 'LOG'
            NP = NP + 4
         ELSE IF (FT.EQ.3) THEN
            STRING(NP:) = 'LOG10'
            NP = NP + 6
         ELSE IF (FT.EQ.4) THEN
            STRING(NP:) = 'EXP'
            NP = NP + 4
         ELSE IF (FT.EQ.7) THEN
            STRING(NP:) = 'SQRT'
            NP = NP + 5
         ELSE IF (FT.EQ.9) THEN
            STRING(NP:) = '1 /'
            NP = NP + 4
         ELSE IF (FT.EQ.10) THEN
            STRING(NP:) = '1 / SQRT'
            NP = NP + 9
            END IF
         STRING(NP:) = '['
         NP = NP + 1
         IF (FP(1).NE.1.0) THEN
            IF (ABS(FP(1)).GT.1000.) THEN
               WRITE (STRING(NP:),1000) FP(1)
            ELSE IF (ABS(FP(1)).GT.10.) THEN
               WRITE (STRING(NP:),1001) FP(1)
            ELSE
               WRITE (STRING(NP:),1002) FP(1)
               END IF
            NP = NP + 8
            STRING(NP:) = '*'
            NP = NP + 2
            END IF
         STRING(NP:) = 'X'
         IF (COOR.EQ.2) STRING(NP:) = 'Y'
         NP = NP + 1
         IF (FP(2).NE.0.0) THEN
            IF (FP(2).LT.0) THEN
               STRING(NP:) = ' -'
            ELSE
               STRING(NP:) = ' +'
               END IF
            NP = NP + 2
            R = ABS (FP(2))
            IF (R.GT.1000.) THEN
               WRITE (STRING(NP:),1000) R
            ELSE IF (R.GT.10.) THEN
               WRITE (STRING(NP:),1001) R
            ELSE
               WRITE (STRING(NP:),1002) R
               END IF
            NP = NP + 7
            END IF
         STRING(NP:) = ']'
         NP = NP + 2
         IF (FT.EQ.5) THEN
            STRING(NP:) = '** 2'
         ELSE IF (FT.EQ.6) THEN
            STRING(NP:) = '** 3'
         ELSE IF (FT.EQ.8) THEN
            STRING(NP:) = '** 1/3'
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (F7.0)
 1001 FORMAT (F7.2)
 1002 FORMAT (F7.4)
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
      INCLUDE 'PLOTR.INC'
      INCLUDE 'PLOTR2.INC'
      REAL      X, Y, AX(5), AY(5), DX, DY, XPP(MAXPNT), YPP(MAXPNT),
     *   CPP(MAXPNT), PARMS(MAXORD+1)
      INTEGER   I, IPL, JPL, J, ITRIM, NPS, DOC, NN, JERR, NFIT
      LOGICAL   DOIT, FIRST
      DOUBLE PRECISION DXVAL(MAXPNT), DYVAL(MAXPNT), DWT(MAXPNT), DRMS,
     *   DANS(10), DVAL
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Set corner values.
      IF (APARM(2).LE.APARM(1)) THEN
         APARM(1) = XMIN
         APARM(2) = XMAX
         END IF
      BLC(1) = APARM(1)
      TRC(1) = APARM(2)
      X = TRC(1) - BLC(1)
      BLC(1) = BLC(1) - 0.05*X
      TRC(1) = TRC(1) + 0.05*X
      IF (APARM(4).LE.APARM(3)) THEN
         APARM(3) = YMIN
         APARM(4) = YMAX
         END IF
      BLC(2) = APARM(3)
      TRC(2) = APARM(4)
      X = TRC(2) - BLC(2)
      BLC(2) = BLC(2) - 0.05*X
      TRC(2) = TRC(2) + 0.05*X
      IF (FACTOR.LE.0.1) FACTOR = 1.0
C                                       Draw a square plot.
      IF (XYRATO.LE.0.0) XYRATO = 1
      XY = XYRATO * (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       ** End plot specific statements.
C                                       Create and open plot file.
      CALL PLMAKE (NPARMS, PRUSER, IPTYPE, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Set up commons for plotting.
      CALL PLINI3 (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Do axis labeling.
      CALL PLABL3 (IERR)
      IF (IERR.NE.0) GO TO 999
      DX = 0.005 * (TRC(1) - BLC(1)) * FACTOR
      DY = 0.005 * (TRC(2) - BLC(2)) * FACTOR
      DX = DX / SQRT (XY)
      DY = DY * SQRT (XY)
C                                       Line drawing:
      DO 20 IPL = SMIN,SMAX
         JPL = IPL - SMIN + 1
         FIRST = .TRUE.
C                                       Point plot
         IF (STTYPE(IPL).GT.0) THEN
            CALL GLTYPE (4, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            DO 10 I = 1,NP
               IF (ABS(SP(I)).EQ.IPL) THEN
                  AX(1) = XP(I) * XSCAL + XOFF
                  AX(2) = AX(1)
                  AX(3) = AX(1)
                  AX(4) = AX(1) - DX * XSCAL
                  AX(5) = AX(1) + DX * XSCAL
                  AY(1) = YP(I) * YSCAL + YOFF
                  AY(2) = AY(1) + DY * YSCAL
                  AY(3) = AY(1) - DY * YSCAL
                  AY(4) = AY(1)
                  AY(5) = AY(1)
                  DOC = -1
                  IF (CP(I).LT.0.0) CP(I) = 5.0
                  IF ((FIRST) .OR. (CP(I).NE.CP(MAX(I-1,1)))) THEN
                     CALL COLVEC (DOC, X, Y, CP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     FIRST = .FALSE.
                     END IF
                  DOIT = (CP(I).GE.0.0) .AND. (CP(I).LE.1.0)
                  CALL PNTPLT (STTYPE(IPL), AX, AY, PBLC, PTRC, .FALSE.,
     *               DOIT, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  IF (SP(I).LT.0) THEN
                     IF ((XP(I).GT.BLC(1)) .AND. (XP(I).LT.TRC(1)) .AND.
     *                  (YP(I).GT.BLC(2)) .AND. (YP(I).LT.TRC(2))) THEN
                        CALL GLTYPE (1, PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        J = ITRIM (STEXT(-SP(I)))
                        X = XP(I) + DX + DX
                        Y = YP(I) - DY
                        CALL PLPOS (X, Y, IERR)
                        IF (IERR.NE.0) GO TO 999
                        CALL GCHAR (J, 0, 0.5, 0.0, STEXT(-SP(I)),
     *                     PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        CALL GLTYPE (4, PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     END IF
                  END IF
 10            CONTINUE
            END IF
 20      CONTINUE
C                                       Line drawing:
      DO 100 IPL = SMIN,SMAX
         JPL = IPL - SMIN + 1
         FIRST = .TRUE.
         IF ((LITYPE(IPL).GE.2) .AND. (NFTYPE(IPL).EQ.0)) THEN
            CALL GLTYPE (3, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            NPS = 0
            DO 30 I = 1,NP
               IF ((SP(I).EQ.-IPL) .AND. (STTYPE(IPL).LE.0)) THEN
                  IF ((XP(I).GT.BLC(1)) .AND. (XP(I).LT.TRC(1)) .AND.
     *               (YP(I).GT.BLC(2)) .AND. (YP(I).LT.TRC(2))) THEN
                     X = XP(I) - DX
                     Y = YP(I)
                     CALL PLPOS (X, Y, IERR)
                     IF (IERR.NE.0) GO TO 999
                     X = XP(I) + DX
                     DOC = 1
                     IF ((CP(I).LT.0.0) .OR. (CP(I).GT.1.0)) CP(I) = 4.0
                     CALL COLVEC (DOC, X, Y, CP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GLTYPE (1, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     J = ITRIM (STEXT(-SP(I)))
                     X = XP(I) + DX + DX
                     Y = YP(I) - DY
                     CALL PLPOS (X, Y, IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GCHAR (J, 0, 0.5, 0.0, STEXT(-SP(I)),
     *                  PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GLTYPE (3, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     END IF
               ELSE IF (SP(I).EQ.IPL) THEN
                  IF ((CP(I).LT.0.0) .OR. (CP(I).GT.1.0)) CP(I) = 4.0
                  NPS = NPS + 1
                  XPP(NPS) = XP(I)
                  YPP(NPS) = YP(I)
                  CPP(NPS) = CP(I)
                  END IF
 30            CONTINUE
            X = XPP(1)
            Y = YPP(1)
            CALL PLPOS (X, Y, IERR)
            IF (IERR.NE.0) GO TO 999
            DO 40 I = 1,NPS-1
               DOC = 1
               FIRST = .FALSE.
               CALL DLINE (LITYPE(IPL)-2, DOC, XPP(I), YPP(I), CPP(I),
     *            PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 999
 40            CONTINUE
            X = XPP(NPS)
            Y = YPP(NPS)
            CALL COLVEC (DOC, X, Y, CPP(NPS), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       step line
         ELSE IF ((LITYPE(IPL).EQ.1) .AND. (NFTYPE(IPL).EQ.0)) THEN
            CALL GLTYPE (2, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 999
            NPS = 0
            DO 80 I = 1,NP
               IF ((SP(I).EQ.-IPL) .AND. (STTYPE(I).LE.0)) THEN
                  IF ((XP(I).GT.BLC(1)) .AND. (XP(I).LT.TRC(1)) .AND.
     *               (YP(I).GT.BLC(2)) .AND. (YP(I).LT.TRC(2))) THEN
                     X = XP(I) - DX
                     Y = YP(I)
                     CALL PLPOS (X, Y, IERR)
                     IF (IERR.NE.0) GO TO 999
                     X = XP(I) + DX
                     DOC = 1
                     IF ((CP(I).LT.0.0) .OR. (CP(I).GT.1.0)) CP(I) = 3.0
                     CALL COLVEC (DOC, X, Y, CP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GLTYPE (1, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     J = ITRIM (STEXT(-SP(I)))
                     X = XP(I) + DX + DX
                     Y = YP(I) - DY
                     CALL PLPOS (X, Y, IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GCHAR (J, 0, 0.5, 0.0, STEXT(-SP(I)), PLTBLK,
     *                  IERR)
                     IF (IERR.NE.0) GO TO 999
                     CALL GLTYPE (2, PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     END IF
               ELSE IF (SP(I).EQ.IPL) THEN
                  IF ((CP(I).LT.0.0) .OR. (CP(I).GT.1.0)) CP(I) = 3.0
                  NPS = NPS + 1
                  XPP(NPS) = XP(I)
                  YPP(NPS) = YP(I)
                  CPP(NPS) = CP(I)
                  END IF
 80            CONTINUE
            X = XPP(1) + (XPP(1) - XPP(MIN(2,NPS))) / 2.0
            X = MIN (XMAX, MAX (XMIN, X))
            Y = YPP(1)
            CALL PLPOS (X, Y, IERR)
            IF (IERR.NE.0) GO TO 999
            DO 90 I = 1,NPS
               IF ((FIRST) .OR. (CPP(I).NE.CPP(MAX(I-1,1)))) THEN
                  DOC = 1
                  FIRST = .FALSE.
               ELSE
                  DOC = 0
                  END IF
               Y = YPP(I)
               IF (I.GT.1) THEN
                  CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  END IF
               X = (XPP(I) + XPP(MIN(I+1,NPS))) / 2.0
               CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               Y = (YPP(I) + YPP(MIN(I+1,NPS))) / 2.0
               CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
               IF (IERR.NE.0) GO TO 999
 90            CONTINUE
            END IF
 100     CONTINUE
C                                       Do fitting instead
      DO 200 IPL = SMIN,SMAX
         JPL = IPL - SMIN + 1
         FIRST = .TRUE.
         NN = 0
C                                       find data for fit
         IF (NFTYPE(IPL).NE.0) THEN
            DO 110 I = 1,NP
               IF (SP(I).EQ.IPL) THEN
                  NN = NN + 1
                  XVAL(NN) = XP(I)
                  YVAL(NN) = YP(I)
                  DXVAL(NN) = XP(I)
                  DYVAL(NN) = YP(I)
                  DWT(NN) = 1.0D0
                  END IF
 110           CONTINUE
            END IF
C                                       found enough - do fit
         JERR = 0
C                                       orthogonal polynomials
         IF (NFTYPE(IPL).LT.0) THEN
            IF (NN.GT.-NFTYPE(IPL)) THEN
               JJC = -NFTYPE(IPL) + 1
               JJC = MIN (JJC, MAXORD)
               XBAR = 0.0
               CALL POLYIN (NN, JERR)
               END IF
            IF ((NN.GT.-NFTYPE(IPL)) .AND. (JERR.EQ.0)) THEN
               CALL XBALMS (NN, PARMS)
               CALL XBFUNC (NN, PARMS)
               NPS = 0
               DVAL = 0.0
               DO 120 I = 1,NN
                  NPS = NPS + 1
                  XPP(NPS) = XVAL(I)
                  YPP(NPS) = YVAL(I)
                  CPP(NPS) = 4.0
                  DVAL = DVAL + (DYVAL(I) - YVAL(I))**2
 120              CONTINUE
               DVAL = SQRT (DVAL / MAX (1, NN-JJC))
               WRITE (MSGTXT,1110) IPL, DVAL, JJC-1
               CALL MSGWRT (5)
               IF (LITYPE(IPL).GE.2) THEN
                  CALL GLTYPE (3, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = XPP(1)
                  Y = YPP(1)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 130 I = 1,NPS-1
                     DOC = 1
                     FIRST = .FALSE.
                     CALL DLINE (LITYPE(IPL)-2, DOC, XPP(I), YPP(I),
     *                  CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 130                 CONTINUE
                  X = XPP(NPS)
                  Y = YPP(NPS)
                  CALL COLVEC (DOC, X, Y, CPP(NPS), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       step line
               ELSE IF (LITYPE(IPL).EQ.1) THEN
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  NPS = 0
                  DO 140 I = 1,NN
                     NPS = NPS + 1
                     XPP(NPS) = XVAL(I)
                     YPP(NPS) = YVAL(I)
                     CPP(NPS) = 3.0
 140                 CONTINUE
                  X = XPP(1) + (XPP(1) - XPP(MIN(2,NPS))) / 2.0
                  X = MIN (XMAX, MAX (XMIN, X))
                  Y = YPP(1)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 150 I = 1,NPS
                     IF ((FIRST) .OR. (CPP(I).NE.CPP(MAX(I-1,1)))) THEN
                        DOC = 1
                        FIRST = .FALSE.
                     ELSE
                        DOC = 0
                        END IF
                     Y = YPP(I)
                     IF (I.GT.1) THEN
                        CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     X = (XPP(I) + XPP(MIN(I+1,NPS))) / 2.0
                     CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     Y = (YPP(I) + YPP(MIN(I+1,NPS))) / 2.0
                     CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 150                 CONTINUE
                  END IF
               END IF
C                                       ordinary polynomials
         ELSE
            IF (NN.GT.NFTYPE(IPL)+1) THEN
               NFIT = NFTYPE(IPL) + 1
               CALL DFITPN (DXVAL, DYVAL, DWT, NFIT, NN, DANS, DRMS,
     *            JERR)
               END IF
            IF ((NN.GT.NFTYPE(IPL)+1) .AND. (JERR.EQ.0)) THEN
               NPS = 0
               DO 160 I = 1,NN
                  DVAL = DANS(1)
                  DO 155 J = 1,NFTYPE(IPL)
                     DVAL = DVAL +  DANS(J+1) * (DXVAL(I) ** J)
 155                 CONTINUE
                  DYVAL(I) = DVAL
                  NPS = NPS + 1
                  XPP(NPS) = DXVAL(I)
                  YPP(NPS) = DYVAL(I)
                  CPP(NPS) = 4.0
                  IF (LITYPE(IPL).EQ.1) CPP(NPS) = 3.0
 160              CONTINUE
               WRITE (MSGTXT,1160) IPL, DRMS, STEXT(IPL)
               CALL MSGWRT (5)
               DO 170 J = 1,NFIT
                  WRITE (MSGTXT,1165) J-1, DANS(J)
                  CALL MSGWRT (5)
 170              CONTINUE
               IF (LITYPE(IPL).GE.2) THEN
                  CALL GLTYPE (3, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = XPP(1)
                  Y = YPP(1)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 180 I = 1,NPS-1
                     DOC = 1
                     FIRST = .FALSE.
                     CALL DLINE (LITYPE(IPL)-2, DOC, XPP(I), YPP(I),
     *                  CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 180                 CONTINUE
                  X = XPP(NPS)
                  Y = YPP(NPS)
                  CALL COLVEC (DOC, X, Y, CPP(NPS), PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
C                                       step line
               ELSE IF (LITYPE(IPL).EQ.1) THEN
                  CALL GLTYPE (2, PLTBLK, IERR)
                  IF (IERR.NE.0) GO TO 999
                  X = XPP(1) + (XPP(1) - XPP(MIN(2,NPS))) / 2.0
                  X = MIN (XMAX, MAX (XMIN, X))
                  Y = YPP(1)
                  CALL PLPOS (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 999
                  DO 190 I = 1,NPS
                     IF ((FIRST) .OR. (CPP(I).NE.CPP(MAX(I-1,1)))) THEN
                        DOC = 1
                        FIRST = .FALSE.
                     ELSE
                        DOC = 0
                        END IF
                     Y = YPP(I)
                     IF (I.GT.1) THEN
                        CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                        IF (IERR.NE.0) GO TO 999
                        END IF
                     X = (XPP(I) + XPP(MIN(I+1,NPS))) / 2.0
                     CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
                     Y = (YPP(I) + YPP(MIN(I+1,NPS))) / 2.0
                     CALL COLVEC (DOC, X, Y, CPP(I), PLTBLK, IERR)
                     IF (IERR.NE.0) GO TO 999
 190                 CONTINUE
                  END IF
               END IF
            END IF
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1190) IPL
            CALL MSGWRT (6)
            END IF
 200     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('Type',I3,' fit rms',F9.4,' fit order',I2)
 1160 FORMAT ('Type',I3,' fit rms',F10.4,2X,A37)
 1165 FORMAT (7X,' fit order',I2,F14.7)
 1190 FORMAT ('TYPE',I3,'  PROBLEM FINDING ORTHOGONAL POLYNOMIALS')
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
      INTEGER   DEPT(5), I, LABEL, LTYPE
      LOGICAL   PFLG, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'PLOTR.INC'
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
      INCLUDE 'PLOTR.INC'
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
C      CC       R(2)     Color of points
C   Outputs:
C      DOC      I        1 => change color on next line
C      PLTBLK   I(256)   Plot buffer
C      IERR     I        Error code
C-----------------------------------------------------------------------
      INTEGER   IT, DOC, PLTBLK(256), IERR
      REAL      XX(2), YY(2), CC(2)
C
      REAL      DX, DY, DC, X, Y, C, S
      INTEGER   I
C-----------------------------------------------------------------------
      S = 0.5
      IF (IT.GT.0) S = S / (2.0 * IT)
      DX = XX(2) - XX(1)
      DY = YY(2) - YY(1)
      DC = CC(2) - CC(1)
C                                       move to point first
      CALL COLVEC (DOC, XX(1), YY(1), CC(1), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       loop
      X = XX(1) + S * DX
      Y = YY(1) + S * DY
      C = CC(1)
      DOC = 0
      DO 20 I = 1,MAX(1,IT)
         CALL COLVEC (DOC, X, Y, C, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         IF (DC.NE.0.0) THEN
            DOC = 1
         ELSE
            DOC = 0
            END IF
         X = X + 2.0 * S * DX
         Y = Y + 2.0 * S * DY
         C = C + 2.0 * S * DC
         IF (IT.GT.0) THEN
            CALL PLPOS (X, Y, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         X = X + 2.0 * S * DX
         Y = Y + 2.0 * S * DY
         C = C + 2.0 * S * DC
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE POLYIN (NVALS, IERR)
C-----------------------------------------------------------------------
C   POLYIN prepares the parameters of a set of orthogonal polynomials.
C   All are carried in COMMON /GDATA/.
C   Input:
C      NVALS   I   Number of points in data array
C   Output:
C      IERR    I   0 ok, 1 no good data, 2 other singularity
C-----------------------------------------------------------------------
      INTEGER   NVALS, IERR
C
      REAL      PP, AL, SUM, TEMP
      INTEGER   MMAX, I, J, K, JJ, KK, N, MM
      INCLUDE 'PLOTR2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       calculate moments
      MMAX = 2 * JJC - 1
      CALL RFILL (MMAX, 0.0, MOMENT)
      NPTS = 0
      DO 20 I = 1,NVALS
         PP = 1
         AL = XVAL(I) + XBAR
         NPTS = NPTS + 1
         DO 15 J = 2,MMAX
            PP = PP * AL
            MOMENT(J) = MOMENT(J) + PP
 15         CONTINUE
 20      CONTINUE
      IERR = 1
      IF (NPTS.LE.0) GO TO 999
      IERR = 2
      DO 25 J = 2,MMAX
         MOMENT(J) = MOMENT(J) / NPTS
 25      CONTINUE
      MOMENT(1) = 1.0
C                                       Matrix: P(K) = G(K) * (X**K
C                                        - SUM (A(K,J)*P(J)))
      MMAX = MAXORD * MAXORD
      CALL RFILL (MMAX, 0.0, AARRAY)
      CALL RFILL (MMAX, 0.0, CARRAY)
      GAMMA(1) = 1.0
      DO 50 K = 2,JJC
         SUM = 0.0
         KK = K - 1
         DO 40 J = 1,KK
            AARRAY(K,J) = MOMENT(J+K-1)
            IF (J.GT.1) THEN
               JJ = J - 1
               DO 30 MM = 1,JJ
                  AARRAY(K,J) = AARRAY(K,J) - AARRAY(J,MM)*AARRAY(K,MM)
 30               CONTINUE
               END IF
            AARRAY(K,J) = AARRAY(K,J) * GAMMA(J)
            SUM = SUM + AARRAY(K,J) ** 2
 40         CONTINUE
         TEMP = MOMENT(2*K-1) - SUM
         IF (TEMP.LE.0.0) GO TO 999
         GAMMA(K) = 1.0 / SQRT (TEMP)
 50      CONTINUE
C                                       Matrix: P(K) = SUM (C(K,J) *
C                                                      X**J)
      CARRAY(1,1) = GAMMA(1)
      DO 65 K = 2,JJC
         CARRAY(K,K) = GAMMA(K)
         KK = K - 1
         DO 60 MM = 1,KK
            DO 55 N = MM,KK
               CARRAY(K,MM) = CARRAY(K,MM) - GAMMA(K) * AARRAY(K,N)
     *            * CARRAY(N,MM)
 55            CONTINUE
 60         CONTINUE
 65      CONTINUE
C                                       average of polynomials
      MMAX = MAXORD
      CALL RFILL (MMAX, 0.0, POLAVG)
      DO 75 I = 1,NVALS
         CALL POLYEV (XVAL(I))
         DO 70 J = 1,MMAX
            POLAVG(J) = POLAVG(J) + POLYFN(J)
            POLXFN(I,J) = POLYFN(J)
 70         CONTINUE
 75      CONTINUE
      DO 80 J = 1,MMAX
         POLAVG(J) = POLAVG(J) / NVALS
 80      CONTINUE
      IERR = 0
C
 999  RETURN
      END
      SUBROUTINE POLYEV (IX)
C-----------------------------------------------------------------------
C   POLYEV evaluates the orthogonal polynomials at the X value given.
C   Inputs:
C      IX   R   X position
C-----------------------------------------------------------------------
      REAL      IX
C
      REAL      AX
      INTEGER   J, K, KK
      INCLUDE 'PLOTR2.INC'
C-----------------------------------------------------------------------
      POLYFN(1) = 1.0
      AX = IX + XBAR
      DO 20 K = 2,JJC
         POLYFN(K) = AX**(K-1)
         KK = K-1
         DO 10 J = 1,KK
            POLYFN(K) = POLYFN(K) - AARRAY(K,J) * POLYFN(J)
 10         CONTINUE
         POLYFN(K) = POLYFN(K) * GAMMA(K)
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XBALMS (INPTS, PARMS)
C-----------------------------------------------------------------------
C   XBALMS computes the answers
C   Inputs:
C      INPTS   I      Number data points
C   Output:
C      PARMS   R(*)   Answers (1 - 8), sigma ** 2 (MAXORD+1)
C-----------------------------------------------------------------------
      INTEGER   INPTS
      REAL      PARMS(*)
C
      INTEGER   I, J
      REAL      YBAR, YBAR2
      INCLUDE 'PLOTR2.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       clear sum variables
      DO 10 I = 1,MAXORD+1
         PARMS(I) = 0.0
 10      CONTINUE
      YBAR = 0.0
      YBAR2 = 0.0
C                                       sum: data, data**2
C                                       data*polyfunc(j)
      DO 30 I = 1,INPTS
         YBAR = YBAR + YVAL(I)
         YBAR2 = YBAR2 + YVAL(I)**2
         DO 20 J = 1,JJC
            PARMS(J) = PARMS(J) + YVAL(I) * POLXFN(I,J)
 20         CONTINUE
 30      CONTINUE
C                                       average
C                                       sigma**2=ybar2-sum(parms**2)
      YBAR = YBAR / NPTS
      YBAR2 = YBAR2 / NPTS
      PARMS(MAXORD+1) = YBAR2
      DO 40 J = 1,JJC
         PARMS(J) = PARMS(J) / NPTS
         PARMS(MAXORD+1) = PARMS(MAXORD+1) - PARMS(J)**2
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE XBFUNC (NVALS, PARMS)
C-----------------------------------------------------------------------
C   XBFUNC computes the model
C   Inputs:
C      NVALS   I      Number of data points in row
C      PARMS   R(9)   factors of orthogonal polynomials
C   Common: /GDATA/
C      YVAL    R(?)   Original slice data pointsreplaced by model
C-----------------------------------------------------------------------
      INTEGER   NVALS
      REAL      PARMS(*)
C
      INTEGER   I, J
      REAL      SUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'PLOTR2.INC'
C-----------------------------------------------------------------------
      DO 20 I = 1,NVALS
         SUM = 0.0
         DO 10 J = 1,JJC
            SUM = SUM + PARMS(J) * POLXFN(I,J)
 10         CONTINUE
         YVAL(I) = SUM
 20      CONTINUE
C
 999  RETURN
      END
