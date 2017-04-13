LOCAL INCLUDE 'BPERR.INC'
      REAL      XSEQ, XDISK, DOALL, PIXR(2), XLAB, XDOTV, XGRCH
      HOLLERITH XNAME(3), XCLASS(2), XINFIL(12), XOUPRT(12)
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER INFIL*48, NAME*12, CLASS*6, STAT*4, TYPE*2, TEXT(2)*80,
     *   TITLE*80
      INTEGER   NCH, MAN, SEQ, DISK, CNO, NPARMS, IGR, MIF, MCR, NTEXT,
     *   LABEL
      REAL      CHNSA(1024,MAXIF,2), CHNSP(1024,MAXIF,2), CHOUT(4), YGAP
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XINFIL, XOUPRT,
     *   DOALL, PIXR, XLAB, XDOTV, XGRCH
      COMMON /BPCH/ INFIL, NAME, CLASS, STAT, TYPE, TEXT, TITLE
      COMMON /BPPARM/ CHNSA, CHNSP, NCH, MAN, SEQ, DISK, CNO, NPARMS,
     *   IGR, MIF, MCR, NTEXT, YGAP, CHOUT, LABEL
LOCAL END
      PROGRAM BPERR
C-----------------------------------------------------------------------
C! Extracts excess closure errors from BPASS output
C# Utility task
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2002, 2012, 2014
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
C
      INCLUDE 'BPERR.INC'
      CHARACTER INLINE*136, OUTLIN*132, OUTFIL*48, PRGNAM*6
      INTEGER   IOERR, I, LUNO, FINDO, LUNL, FINDL, JTRIM, IERR, IROUND,
     *   ANTS(MAXANT,MAXIF,2), CHNSN(1024,MAXIF,2), ICH, LIF, LCR,
     *   SCRTCH(256), NFAIL, LAN
      LOGICAL  RQUICK
      REAL     AMP, PHS
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNO, LUNL /17, 18/
      DATA PRGNAM /'BPERR'/
C-----------------------------------------------------------------------
C                                       AIPS system start up stuff
      CALL ZDCHIN (.TRUE.)
      DEVTAB(LUNO) = 3
      DEVTAB(LUNL) = 3
      CALL VHDRIN
      NPARMS = 37
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XNAME, SCRTCH, IERR)
      IF (RQUICK) CALL RELPOP (0, SCRTCH, IERR)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      CALL H2CHR (48, 1, XINFIL, INFIL)
      CALL H2CHR (48, 1, XOUPRT, OUTFIL)
      SEQ = XSEQ + 0.1
      DISK = XDISK + 0.1
      IGR = XGRCH + 0.1
      IF (IGR.LE.0) IGR  = 1
      IOERR = 0
      LABEL = IROUND (XLAB)
C                                       Find plot file to attach
      IF (XDOTV.NE.0.0) THEN
         CNO = 1
         TYPE = '  '
         CALL CATDIR ('SRCH', DISK, CNO, NAME, CLASS, SEQ, TYPE, NLUSER,
     *      STAT, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, NAME, CLASS, SEQ, DISK, NLUSER
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       Save name class etc.
         CALL CHR2H (12, NAME, 1, XNAME)
         CALL CHR2H (6, CLASS, 1, XCLASS)
         XDISK = DISK
         XSEQ = SEQ
C                                       Read catalog header
         STAT = 'WRIT'
         IF (XDOTV.GT.0.0) STAT = 'READ'
         CALL CATIO ('READ', DISK, CNO, CATBLK, STAT, SCRTCH, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1001) IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         END IF
C                                       Get list of routines
      CALL ZTXOPN ('READ', LUNL, FINDL, INFIL, .FALSE., IOERR)
      IF (IOERR.NE.0) GO TO 990
C                                       Open output file
      IF (OUTFIL.NE.' ') THEN
         CALL ZTXOPN ('WRIT', LUNO, FINDO, OUTFIL, .TRUE., IOERR)
         IF (IOERR.NE.0) GO TO 980
         END IF
C                                       Read for start job
 100  CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
      IF (IOERR.NE.0) GO TO 900
         I = JTRIM (INLINE)
         IF (INLINE(39:48).NE.'BPASS ') GO TO 100
         IF (INLINE(49:60).NE.'Task BPASS  ') GO TO 100
C                                       new job
         I = 2 * 1024 * MAXIF
         CALL FILL (I, 0, CHNSN)
         CALL RFILL (I, 0.0, CHNSA)
         CALL RFILL (I, 0.0, CHNSP)
         I = MAXANT * MAXIF * 2
         CALL FILL (I, 0, ANTS)
         NCH = 0
         MIF = 0
         MAN = 0
C                                       Read for stats
 110     CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
         IF (IOERR.NE.0) GO TO 200
            I = JTRIM (INLINE)
            IF (INLINE(39:48).NE.'BPASS ') GO TO 110
            IF (INLINE(49:72).NE.'Closure error statistics') GO TO 110
            READ (INLINE,1110) LIF, LCR
            MIF = MAX (MIF, LIF)
C                                       skip line
 115        CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
            IF (IOERR.NE.0) GO TO 200
            I = JTRIM (INLINE)
            IF (INLINE(39:48).NE.'BPASS ') GO TO 115
C                                       read channel data
 120        CALL ZTXIO ('READ', LUNL, FINDL, INLINE, IOERR)
            IF (IOERR.NE.0) GO TO 200
            I = JTRIM (INLINE)
            IF (INLINE(39:48).NE.'BPASS ') GO TO 120
C                                       new time
            IF (INLINE(49:55).EQ.' Time= ') THEN
               GO TO 120
C                                       failure
            ELSE IF (INLINE(49:55).EQ.'GCALC: ') THEN
               GO TO 120
C                                       numeric
            ELSE IF (INLINE(49:49).EQ.' ') THEN
               READ (INLINE,1120) ICH, AMP, PHS
               NCH = MAX (NCH, ICH)
               CHNSA(ICH,LIF,LCR) = CHNSA(ICH,LIF,LCR) + AMP
               CHNSP(ICH,LIF,LCR) = CHNSP(ICH,LIF,LCR) + PHS
               CHNSN(ICH,LIF,LCR) = CHNSN(ICH,LIF,LCR) + 1
               GO TO 120
C                                       closures
            ELSE IF (INLINE(49:55).EQ.'Antenna') THEN
               READ (INLINE,1121) LAN, LIF, LCR, NFAIL
               ANTS(LAN,LIF,LCR) = ANTS(LAN,LIF,LCR) + NFAIL
               MAN = MAX (MAN, LAN)
               MIF = MAX (MIF, LIF)
               GO TO 120
            ELSE IF (INLINE(49:72).EQ.'Closure error statistics') THEN
               READ (INLINE,1110) LIF, LCR
               MIF = MAX (MIF, LIF)
               GO TO 115
               END IF
C                                       Done with this task's output
C                                       Average IFs etc
 200     MCR = 2
         IF (DOALL.LE.0.0) THEN
            MCR = 1
            DO 225 LIF = 1,MIF
               DO 220 LCR = 1,2
C                                       channel-level failure
                  IF ((LIF.GT.1) .OR. (LCR.GT.1)) THEN
                     IF (NCH.GT.0) THEN
                        DO 210 ICH = 1,NCH
                           CHNSA(ICH,1,1) = CHNSA(ICH,1,1) +
     *                        CHNSA(ICH,LIF,LCR)
                           CHNSP(ICH,1,1) = CHNSP(ICH,1,1) +
     *                        CHNSP(ICH,LIF,LCR)
                           CHNSN(ICH,1,1) = CHNSN(ICH,1,1) +
     *                        CHNSN(ICH,LIF,LCR)
 210                       CONTINUE
                        END IF
C                                       count by antenna
                     IF (MAN.GT.0) THEN
                        DO 215 LAN = 1,MAN
                           ANTS(LAN,1,1) = ANTS(LAN,1,1) +
     *                        ANTS(LAN,LIF,LCR)
 215                       CONTINUE
                        END IF
                     END IF
 220              CONTINUE
 225           CONTINUE
            MIF = 1
            END IF
C                                       Now do some output
         DO 300 LIF = 1,MIF
            DO 300 LCR = 1,MCR
               AMP = 0.0
               PHS = 0.0
               IF (NCH.GT.0) THEN
                  DO 230 ICH = 1,NCH
                     IF (CHNSN(ICH,LIF,LCR).GT.0) THEN
                        CHNSA(ICH,LIF,LCR) = CHNSA(ICH,LIF,LCR) /
     *                     CHNSN(ICH,LIF,LCR)
                        CHNSP(ICH,LIF,LCR) = CHNSP(ICH,LIF,LCR) /
     *                     CHNSN(ICH,LIF,LCR)
                        AMP = MAX (AMP, CHNSA(ICH,LIF,LCR))
                        PHS = MAX (PHS, CHNSP(ICH,LIF,LCR))
                        END IF
 230                 CONTINUE
                  END IF
C                                       output channel stuff
               IF ((AMP.GT.0.0) .AND. (PHS.GT.0.0) .AND.
     *            (OUTFIL.NE.' ')) THEN
                  WRITE (OUTLIN,1230) TSKNAM, LIF, LCR
                  I = JTRIM (OUTLIN)
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:I), IERR)
                  WRITE (OUTLIN,1231) TSKNAM
                  IF (IERR.NE.0) GO TO 900
                  I = JTRIM (OUTLIN)
                  CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:I), IERR)
                  IF (IERR.NE.0) GO TO 900
                  DO 240 ICH = 1,NCH
                     WRITE (OUTLIN,1232) TSKNAM, ICH,
     *                  CHNSA(ICH,LIF,LCR), CHNSP(ICH,LIF,LCR)
                     I = JTRIM (OUTLIN)
                     CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:I), IERR)
                     IF (IERR.NE.0) GO TO 900
 240                 CONTINUE
                  END IF
               IF ((OUTFIL.NE.' ') .AND. (MAN.GT.0)) THEN
                  DO 250 LAN = 1,MAN
                     WRITE (OUTLIN,1240) TSKNAM, LAN, LIF, LCR,
     *                  ANTS(LAN,LIF,LCR)
                     I = JTRIM (OUTLIN)
                     CALL ZTXIO ('WRIT', LUNO, FINDO, OUTLIN(:I), IERR)
                     IF (IERR.NE.0) GO TO 900
 250                 CONTINUE
                  END IF
 290           CONTINUE
 300        CONTINUE
         IERR = 0
         IF ((MIF.GT.0) .AND. (NCH.GT.0)) THEN
            IF (XDOTV.NE.0.0) CALL BPLOTR (IERR)
            IF ((IOERR.EQ.0) .AND. (IERR.EQ.0)) GO TO 100
            END IF
C
 900  IF (XDOTV.LT.0.0) CALL CATDIR ('CSTA', DISK, CNO, NAME, CLASS,
     *   SEQ, TYPE, NLUSER, 'CLWR', SCRTCH, I)
      IF (XDOTV.GT.0.0) CALL CATDIR ('CSTA', DISK, CNO, NAME, CLASS,
     *   SEQ, TYPE, NLUSER, 'CLRD', SCRTCH, I)
C
      IF (OUTFIL.NE.' ') CALL ZTXCLS (LUNO, FINDO, I)
C
 980  IF (IOERR.EQ.2) IOERR = 0
      CALL ZTXCLS (LUNL, FINDL, I)
C
 990  IF (IOERR.EQ.0) IOERR = IERR
      IOERR = MAX (0, IOERR)
      CALL DIETSK (IOERR, RQUICK, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE= '' '' USER=',I4)
 1001 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER')
 1110 FORMAT (76X,I3,11X,I3)
 1120 FORMAT (48X,I5,F12.2,12X,F12.2)
 1121 FORMAT (48X,7X,I3,4X,I3,6X,I2,5X,I10)
 1230 FORMAT (A6,32X,'BPASS',5X,'Closure error statistics: IF',I3,
     *   ' correlator',I3)
 1231 FORMAT (A6,32X,'BPASS',5X,'Channel   Mean amp &  amp**2   ',
     *   ' Mean phase & phase**2')
 1232 FORMAT (A6,32X,'BPASS',5X,I5,F12.2,12X,F12.2)
 1240 FORMAT (A6,32X,'BPASS',5X,'Antenna',I3,'  IF',I3,'  corr',I2,
     *   '  had',I10,' excess closure errors')
      END
      SUBROUTINE BPLOTR (IERR)
C-----------------------------------------------------------------------
C   BPLOTR plots the current data array
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'BPERR.INC'
      REAL      X, Y, XMAX, YRNG(2), XRANGE, YRANGE, XR, YR, BLC(2),
     *   TRC(2), DCX, DCY, ID(3), IT(3), IANGLE
      INTEGER   I, LIF, LCR, LCH, DEPTH(5), LLABEL, INCHAR, IGLUN,
     *   IGFIND
      LOGICAL   PFLG
      CHARACTER ANAME*18, ADATE*12, ATIME*8, WRKTXT*80, PHNAME*48
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      DOTV = XDOTV.GT.0.0
C                                       Find maximum value.
      XMAX = 0.0
      DO 20 LIF = 1,MIF
         DO 15 LCR = 1,MCR
            DO 10 LCH = 1,NCH
               XMAX = MAX (XMAX, CHNSA(LCH,LIF,LCR))
 10            CONTINUE
 15         CONTINUE
 20      CONTINUE
      IF (PIXR(2).LE.PIXR(1)) THEN
         YRNG(2) = XMAX
         YRNG(1) = 0.0
      ELSE
         YRNG(1) = PIXR(1)
         YRNG(2) = PIXR(2)
         END IF
      XMAX = 0.05 * (YRNG(2) - YRNG(1))
      YRNG(2) = YRNG(2) + XMAX
      YRNG(1) = YRNG(1) - XMAX
C                                       Create the plot file
C                                       Add plot file to header
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', DISK, CNO, CATBLK, IOBLK, .TRUE., 'WRIT',
     *      IVER, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Make physical filename
      CALL ZPHFIL ('PL', DISK, CNO, IVER, PHNAME, IERR)
C                                       Open plot file
      TVCHN = 0
      GRCHN = IGR
      TVCORN(1) = 0
      TVCORN(2) = 0
      CALL GINIT (DISK, CNO, PHNAME, 0, 1, NPARMS, XNAME, DOTV, TVCHN,
     *   GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Set corner values. Constant 1.1
C                                       chosen for esthetics.
      BLC(1) = 0.0
      TRC(1) = NCH + 1.0
      BLC(2) = YRNG(1)
      TRC(2) = YRNG(2)
      NTEXT = 0
C                                       Draw a square plot.
      XY = (TRC(2) - BLC(2)) / (TRC(1) - BLC(1))
C                                       Set up commons for plotting.
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
C                                        proper scaling labels
      XRANGE = TRC(1) - BLC(1)
      YRANGE = TRC(2) - BLC(2)
      XR = XRANGE
      CALL METSCL (LABEL, XR, CPREF(1,LOCNUM), PFLG)
      YR = YRANGE
      CALL METSCL (LABEL, YR, CPREF(2,LOCNUM), PFLG)
C                                        proceed filling /LOCATI/
      CALL RCOPY (2, BLC, PBLC)
      CALL RCOPY (2, TRC, PTRC)
C                                       See if we need to rescale to
C                                       prevent integer overflow in
C                                       plot routines (plot file values
C                                       are integers).
C                                       Rescale X values
      XSCAL = 1.0
      XOFF = 0.0
      IF ((XRANGE.GE.16000.0) .OR. (XRANGE.LE.10.0)) THEN
         XSCAL = 16000.0 / XRANGE
         XOFF = - BLC(1) * XSCAL
         PTRC(1) = 16000.0
         PBLC(1) = 0.0
         END IF
C                                       Rescale Y values
      YSCAL = 1.0
      YOFF = 0.0
      IF ((YRANGE.GT.16000.0) .OR. (YRANGE.LE.10.0)) THEN
         YSCAL = 16000.0 / YRANGE
         YOFF = - BLC(2) * YSCAL
         PTRC(2) = 16000.0
         PBLC(2) = 0.0
         END IF
      XY = XY * (YSCAL / XSCAL)
      RPLOC(1,LOCNUM) = PBLC(1)
      RPLOC(2,LOCNUM) = PBLC(2)
      RPVAL(1,LOCNUM) = BLC(1) * XR / XRANGE
      RPVAL(2,LOCNUM) = BLC(2) * YR / YRANGE
      AXINC(1,LOCNUM) = XR / XRANGE / XSCAL
      AXINC(2,LOCNUM) = YR / YRANGE / YSCAL
      CTYP(1,LOCNUM) = 'Channels'
      CTYP(2,LOCNUM) = 'Per cent'
C                                       Left border in characters
      LLABEL = MOD (ABS (LABEL), 100)
      CHOUT(1) = 0.5
      IF (LLABEL.EQ.2) CHOUT(1) = 3.0
      IF (LLABEL.GT.2) THEN
         CHOUT(1) = 2.5
         CALL CHNTIC (PBLC, PTRC, I)
         IF (I.GT.0) CHOUT(1) = 4.0 + I
         END IF
C                                       Bottom border in characters
      CHOUT(2) = 0.5
      IF (LLABEL.GT.1) CHOUT(2) = CHOUT(2) + 1.5
      IF (LLABEL.GT.2) CHOUT(2) = CHOUT(2) + 1.333
      YGAP = CHOUT(2) - 0.5 + 1.333
      IF ((LLABEL.GT.1) .AND. (LLABEL.LT.7)) CHOUT(2) =
     *    CHOUT(2) + NTEXT * 1.333
C                                       Right border in characters
      CHOUT(3) = 0.5
C                                       Top border in characters
      CHOUT(4) = 0.5
      IF ((LLABEL.GT.1) .AND. (LLABEL.LT.7)) CHOUT(4) = CHOUT(4) + 1.5
      IF ((LABEL.GT.1) .AND. (LLABEL.LT.7)) CHOUT(4) =
     *   CHOUT(4) + 1.333
C                                       Write intialization records
C                                       into plot file.
C                                       initialize line drawing
      CALL GINITL (PBLC, PTRC, XY, CHOUT, DEPTH, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Do axis labeling.
      CALL CLAB1 (PBLC, PTRC, CHOUT, LABEL, XY, .FALSE., PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                        draw rectangle
      CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL GVEC (PTRC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL GVEC (PTRC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL GVEC (PBLC(1), PTRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      CALL GVEC (PBLC(1), PBLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       title line.
      IF ((ABS(LLABEL).GT.1) .AND. (ABS(LLABEL).LT.7)) THEN
C                                       Standard title
         CALL H2CHR (8, 1, CATH(KHOBJ), TITLE)
         INCHAR = 12
         IF (TITLE.EQ.' ') INCHAR = 1
         IF (NCHLAB(1,LOCNUM).GT.0) THEN
            TITLE(INCHAR-1:INCHAR-1) = '_'
            TITLE(INCHAR:) = SAXLAB(1,LOCNUM)(1:NCHLAB(1,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(1,LOCNUM)
            END IF
         IF (NCHLAB(2,LOCNUM).GT.0) THEN
            TITLE(INCHAR-1:INCHAR-1) = '_'
            TITLE(INCHAR:) = SAXLAB(2,LOCNUM)(1:NCHLAB(2,LOCNUM))
            INCHAR = INCHAR + 3 + NCHLAB(2,LOCNUM)
            END IF
         IF (INCHAR.GT.1) TITLE(INCHAR-1:INCHAR-1) = '_'
         CALL H2CHR (18, 1, CATH(KHIMN), ANAME)
         CALL NAMEST (ANAME, CATBLK(KIIMS), TITLE(INCHAR:), I)
         CALL REFRMT (TITLE, '_', INCHAR)
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 900
         DCX = 0.0
         DCY = .5
         IANGLE = 0
         CALL CHTRIM (TITLE, 80, TITLE, INCHAR)
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, TITLE, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Date/time version
      IF ((LABEL.GT.1) .AND. (LLABEL.LT.7)) THEN
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, ATIME, ADATE)
         WRITE (WRKTXT,1020) IVER, ADATE, ATIME
         CALL REFRMT (TITLE, '_', INCHAR)
         DCY = DCY + 1.333
         CALL GPOS (PBLC(1), PTRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Text at bottom.
      IF ((NTEXT.GT.0) .AND. (ABS(LLABEL).GT.1) .AND.
     *   (ABS(LLABEL).LT.7)) THEN
         DCX = 0.0
         IANGLE = 0
         DO 30 I = 1,NTEXT
            WRKTXT = TEXT(I)
            CALL CHTRIM (WRKTXT, 80, WRKTXT, INCHAR)
            DCY = -YGAP
            YGAP = YGAP + 1.333
            CALL GPOS (PBLC(1), PBLC(2), PLTBLK, IERR)
            CALL GCHAR (INCHAR, IANGLE, DCX, DCY, WRKTXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 900
 30         CONTINUE
         END IF
C                                       ** Plot specific statements.
C                                       Draw data
      CALL GLTYPE (2, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
      DO 50 LIF = 1,MIF
         DO 45 LCR = 1,MCR
            IF (MIF*MCR.LE.1) THEN
               X = 0.5
               Y = CHNSA(1,LIF,LCR)
               CALL PLPOS (X, Y, IERR)
               IF (IERR.NE.0) GO TO 900
               X = 1.5
               CALL PLVEC (X, Y, IERR)
               IF (IERR.NE.0) GO TO 900
               DO 35 LCH = 2,NCH
                  Y = CHNSA(LCH,LIF,LCR)
                  X = LCH - 0.5
                  CALL PLVEC (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 900
                  X = LCH + 0.5
                  CALL PLVEC (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 900
 35               CONTINUE
            ELSE
               X = 1.0
               Y = CHNSA(1,LIF,LCR)
               CALL PLPOS (X, Y, IERR)
               IF (IERR.NE.0) GO TO 900
               DO 40 LCH = 2,NCH
                  X = LCH
                  Y = CHNSA(LCH,LIF,LCR)
                  CALL PLVEC (X, Y, IERR)
                  IF (IERR.NE.0) GO TO 900
 40               CONTINUE
               END IF
 45         CONTINUE
 50      CONTINUE
C                                       Close plot file
      GPHPAG = DOTV
      CALL GFINIS (PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       OK finish - do HI file
      IF (.NOT.DOTV) THEN
         CALL HIPLOT (DISK, CNO, IVER, PLTBLK, IERR)
         WRITE (MSGTXT,1050) IVER
         CALL MSGWRT (2)
         END IF
      IERR = 0
      GO TO 999
C                                       not sucessful. Destroy.
 900  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (GPHLUN, GPHIND, IERR)
         CALL ZDESTR (GPHVOL, GPHNAM, IERR)
         CALL DELEXT ('PL', DISK, CNO, 'WRIT', CATBLK, IOBLK, IVER,
     *      IERR)
         IERR = 8
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('Plot file version',I4,'__created ',A,A)
 1050 FORMAT ('Plot file version',I4,' created')
      END
