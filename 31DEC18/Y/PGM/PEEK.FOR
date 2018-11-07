LOCAL INCLUDE 'PEEK.INC'
      HOLLERITH XINAME(3), XCLASS(2), XBAND, XASDMF(16,2)
      REAL      XSEQ, XDISK, XANT(50), TIMER(8), APARM(10), FPARM(30),
     *   PRTLEV, DOPLOT, XDOTV, XYRATO, DPARM(10)
      COMMON /INPARM/ XINAME, XCLASS, XSEQ, XDISK, XANT, TIMER, APARM,
     *   XBAND, FPARM, PRTLEV, XASDMF, DOPLOT, XDOTV, XYRATO, DPARM
      CHARACTER INNAME*12, INCLAS*6, REFBND*1, ASDMF(2)*64, OUTFIL*128,
     *   PGDATE*24, FILNAM*128, MYNAME*12
      COMMON /INCHAR/ ASDMF, OUTFIL, INNAME, INCLAS, REFBND, PGDATE,
     *   FILNAM, MYNAME
      INTEGER   INSEQ, INDISK, CNO, IANT(50), GRCHAN, NANTS, NPARMS,
     *   SCRTCH(256)
      REAL      DATE1, DATE2, TIME1, TIME2, EMIN, EMAX, AMIN, BMIN,
     *   BMAX, WMAX, F0, F1
      LOGICAL   VERBOS, TILT, AZZERO, ELZERO, COLIMA, SAG, AZ3, AZCNTR,
     *    ELCNTR, AXSPRP, AZ2, REFRAC, DOTV
      COMMON /XTPARM/ SCRTCH, INSEQ, INDISK, IANT, GRCHAN, NANTS, CNO,
     *   DATE1, DATE2, TIME1, TIME2, EMIN, EMAX, AMIN, BMIN, BMAX, WMAX,
     *   VERBOS, TILT, AZZERO, ELZERO, COLIMA, SAG, AZ3, AZCNTR,
     *   ELCNTR, AXSPRP, AZ2, REFRAC, DOTV, NPARMS, F0, F1
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
LOCAL INCLUDE 'PEEKLUN.INC'
      INTEGER   PNTLUN, PTRLUN, PLTLUN, GPLLUN, CHGLUN, FCHLUN,
     *          PNTIND, PTRIND, PLTIND, GPLIND, CHGIND, FCHIND
      COMMON /PKLUN/ PNTLUN, PTRLUN, PLTLUN, GPLLUN, CHGLUN, FCHLUN,
     *   PNTIND, PTRIND, PLTIND, GPLIND, CHGIND, FCHIND
LOCAL END
LOCAL INCLUDE 'PEEKPLT.INC'
      INTEGER   ISYM1, ISYM2, IGR1, IGR2
      REAL      FACT1, FACT2, XYRAT
      COMMON /PKPLOT/ FACT1, FACT2, XYRAT, ISYM1, ISYM2, IGR1, IGR2
LOCAL END
      PROGRAM PEEK
C-----------------------------------------------------------------------
C! Fits pointing model to output from VLA
C# Calibration VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1987-2018
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
C     Program to fit to the standard pointing parameters.
C
C     National Radio Astronomy Observatory, Socorro, NM 87801
C      VLA - Software development
C
C     PROGRAMMER           DATE            Version
C
C     Bruno Garagnon       August 1987       1.0
C     Gareth Hunt          March 1988        1.1
C     Gareth Hunt          July 1989         2.0
C     Bob Greschke         August 1989       2.1  (See PEEKGO.FOR)
C     Bob Greschke         October 1989      3.0  (See PEEKGO.FOR)
C     Bob Greschke         October 1989      3.1  (See PEEKGO.FOR)
C     Bob Greschke         November 1989     3.2  (See PEEKGO.FOR)
C     Bob Greschke         November 1989     4.0  (See PEEKGO.FOR) uc
C     Phillip Hicks        November 1989     4.1  (See PEEKGO.FOR)
C     Rick Perley          December 1991     5.0  Added More Terms
C     Rick Perley          January 1992      5.1  Added Band Command
C     Rick Perley          February 1992     5.2  Add Wind, Print Cmds
C     Rick Perley          February 1992     5.3  New Collimation Code
C     Rick Perley          April 1992        7.0  New Format w WX info
C     Phillip Hicks        June 1994         7.1  The length of the
C                                                 string with the old
C                                                 pointing files renamed
C     Min Yun              December 1996     8.0  Q-band included, A1/E1
C                                                 separated.
C     Min Yun              June 1999         8.1  Modified MYMODL to
C                                                 to allow "FULL" option.
C     Min Yun              January 2000      8.2  ANT 29 added
C     Ken Sowinski         September 2005    9.0  Many changes ...
C     Eric Greisen         July 2018         AIPS version
C-----------------------------------------------------------------------
      INCLUDE 'PEEK.INC'
      INTEGER   IRET
      CHARACTER PRGNAM*6, GOTXT*3
      DATA PRGNAM /'PEEK'/
C-----------------------------------------------------------------------
      PGDATE = 'Wed Feb 22 14:16:36 2017'
      CALL PEEKIN (PRGNAM, IRET)
      IF (IRET.NE.0) GO TO 900
      IF (FPARM(12).GT.0.0) THEN
         GOTXT = 'ANT'
      ELSE
         GOTXT = 'PAD'
         END IF
      CALL PEEKGO (GOTXT, IRET)
C
 900  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE PEEKIN (PRGNAM, IRET)
C-----------------------------------------------------------------------
C   Does initialization for PEEK
C   Inputs
C      PRGNAM   C*6   Task name
C   Outputs:
C      IRET     I     Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGNAM*(*)
C
      INCLUDE 'PEEK.INC'
      INCLUDE 'PEEKPLT.INC'
      INTEGER   IERR, I, J, JTRIM
      CHARACTER INTYP*2, STAT*4
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Initialize the I/O parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Get input values from AIPS.
      NPARMS = 152
      CALL GTPARM (PRGNAM, NPARMS, RQUICK, XINAME, SCRTCH, IERR)
      IRET = IERR
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         END IF
C
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 8
C                                       interpret things
      CALL H2CHR (12, 1, XINAME, INNAME)
      CALL H2CHR (6, 1, XCLASS, INCLAS)
      CALL H2CHR (1, 1, XBAND, REFBND)
      CALL H2CHR (64, 1, XASDMF(1,1), ASDMF(1))
      CALL H2CHR (64, 1, XASDMF(1,2), ASDMF(2))
      IF (REFBND.EQ.' ') REFBND = 'X'
      J = JTRIM (ASDMF(1))
      FILNAM = ASDMF(1)(:J) // ASDMF(2)
      CALL FIXOUT (OUTFIL, MYNAME, FILNAM)
      INSEQ = XSEQ + 0.1
      INDISK = XDISK + 0.1
      NANTS = 0
      CALL FILL (50, 0, IANT)
      DO 10 I = 1,50
         J = XANT(I) + 0.1
         IF (J.GT.0) THEN
            NANTS = NANTS + 1
            IANT(NANTS) = J
            END IF
 10      CONTINUE
      IF (NANTS.LE.0) THEN
         DO 15 I = 1,28
            IANT(I) = I
 15         CONTINUE
         NANTS = 28
         END IF
      TIME1 = TIMER(1)*24.0 + TIMER(2)*3600.0 + TIMER(3)*60.0 + TIMER(4)
      TIME2 = TIMER(5)*24.0 + TIMER(6)*3600.0 + TIMER(7)*60.0 + TIMER(8)
      IF (TIME2.LE.TIME1) TIME2 = 24. * 3600.
      DATE1 = APARM(1)
      IF (DATE1.LE.0) CALL SCTIMX ('FMJAD=Y,M,D', DATE1, 100, 1, 1)
      APARM(1) = DATE1
      DATE2 = APARM(2)
      IF (DATE2.LE.0) CALL SCTIMX ('FMJAD=Y,M,D', DATE2, 150, 12, 31)
      APARM(2) = DATE2
      CALL RFILL (8, 0.0, TIMER)
      TIMER(1) = TIME1
      TIMER(5) = TIME2
      EMIN = 7.0
      EMAX = 120.0
      IF (APARM(3).NE.0.0) EMIN = APARM(3)
      IF (APARM(4).NE.0.0) EMAX = APARM(4)
      APARM(3) = EMIN
      APARM(4) = EMAX
      EMIN = EMIN * DG2RAD
      EMAX = EMAX * DG2RAD
      AMIN = 0.01
      IF (APARM(5).NE.0.0) AMIN = APARM(5)
      APARM(5) = AMIN
      WMAX = 5
      IF (APARM(6).NE.0.0) WMAX = APARM(6)
      APARM(6) = WMAX
      BMIN = 0.95
      BMAX = 1.15
      IF (APARM(7).NE.0.0) BMIN = APARM(7)
      IF (APARM(8).NE.0.0) BMAX = APARM(8)
      APARM(7) = BMIN
      APARM(8) = BMAX
      F0 = 0.1
      IF (APARM(9).GT.0.0) F0 = APARM(9)
      APARM(9) = F0
      F1 = 4.0
      IF (APARM(10).GT.0.0) F1 = APARM(10)
      APARM(10) = F1
      VERBOS = .FALSE.
      IF (PRTLEV.GT.1.5) VERBOS = .TRUE.
      J = 0
      DO 20 I = 1,11
         IF (FPARM(I).GT.0.0) J = J + 1
 20      CONTINUE
      IF (J.EQ.0) THEN
         FPARM(1) = 1.0
         FPARM(2) = 1.0
         FPARM(3) = 1.0
         FPARM(4) = 1.0
         FPARM(11) = 1.0
         END IF
      TILT =   FPARM(1).GT.0.0
      AZZERO = FPARM(2).GT.0.0
      ELZERO = FPARM(3).GT.0.0
      COLIMA = FPARM(4).GT.0.0
      SAG =    FPARM(5).GT.0.0
      AZ3 =    FPARM(6).GT.0.0
      AZCNTR = FPARM(7).GT.0.0
      ELCNTR = FPARM(8).GT.0.0
      AXSPRP = FPARM(9).GT.0.0
      AZ2 =    FPARM(10).GT.0.0
      REFRAC = FPARM(11).GT.0.0
      IF (FPARM(16).LE.0) FPARM(17) = -1.0
      DOTV = XDOTV.GT.0.0
      ISYM1 = DPARM(1) + 0.1
      ISYM2 = DPARM(2) + 0.1
      IGR1 = DPARM(5) + 0.1
      IGR2 = DPARM(6) + 0.1
      FACT1 = DPARM(3)
      FACT2 = DPARM(4)
      IF (FACT1.LE.0.0) FACT1 = 1.0
      FACT1 = MAX (0.33, MIN (30.0, FACT1))
      IF (FACT2.LE.0.0) FACT2 = 1.0
      FACT2 = MAX (0.33, MIN (30.0, FACT2))
      IF ((ISYM1.LE.0) .OR. (ISYM1.GT.24)) ISYM1 = 2
      IF ((ISYM2.LE.0) .OR. (ISYM2.GT.24)) ISYM2 = 3
      IF ((IGR1.LE.0) .OR. (IGR1.GT.4)) IGR1 = 2
      IF ((IGR2.LE.0) .OR. (IGR2.GT.4)) IGR2 = 3
C                                       find catalog file if needed
      IF ((DOPLOT.GT.0.0) .AND. (XDOTV.LE.0.0)) THEN
         CNO = 1
         INTYP = ' '
         CALL CATDIR ('SRCH', INDISK, CNO, INNAME, INCLAS, INSEQ, INTYP,
     *      NLUSER, STAT, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, INNAME, INCLAS, INSEQ, INTYP,
     *         INDISK, NLUSER
            GO TO 990
            END IF
C                                       Get catblk, mark file read
         CALL CATIO ('READ', INDISK, CNO, CATBLK, 'REST', SCRTCH, IRET)
         IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
            WRITE (MSGTXT,1021) IRET
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = INDISK
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 1
         IF (DOTV) FRW(NCFILE) = 0
      ELSE
         INDISK = MAX (1, INDISK)
         CNO = 1
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1021 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
      END
      SUBROUTINE FIXOUT (OUTFIL, MYNAME, FILNAM)
C-----------------------------------------------------------------------
C   Parses FILNAM to set OUTFIL and MYNAME
C   Input:
C      FILNAM   C*(*)   ASDMFILE set input data name
C   Output:
C      OUTFIL   C*(*)   Directory for ANTnn.dat files
C      MYNAME   C*(*)   Nicjname for data
C-----------------------------------------------------------------------
      CHARACTER OUTFIL*(*), MYNAME*(*), FILNAM*(*)
C
      INTEGER   J, JT, JTRIM
C-----------------------------------------------------------------------
      JT = JTRIM (FILNAM)
      DO 20 J = JT,1,-1
         IF ((FILNAM(J:J).EQ.'/') .OR. (FILNAM(J:J).EQ.':')) GO TO 50
 20      CONTINUE
      OUTFIL = 'HOME:'
      MYNAME = FILNAM(JT-7:JT)
      GO TO 999
C                                       fond a break
 50   OUTFIL = FILNAM(:J)
      MYNAME = FILNAM(J+1:JT)
C
 999  RETURN
      END
      SUBROUTINE PEEKGO (GOTXT, IRET)
C-----------------------------------------------------------------------
C   Subroutine to execute PEEK
C   Inputs:
C      GOTXT   C*(*)   ANT or PAD
C   Outputs
C      IRET    I       error code
C   History:
C     PROGRAMMER           DATE            Version
C     Bruno Garagnon       August 1987       1.0
C     Gareth Hunt          March 1988        1.1
C     Gareth Hunt          July 1989         2.0
C     Bob Greschke         August 1989       2.1  Output format changes
C                                                 and the re-creation
C                                                 of the .PTR file.
C     Bob Greschke         October 1989      3.0  Converted the Inhale portions
C                                                 of PEEKGO to read just the
C                                                 .PNT file to get its data
C                                                 (Just like the old DEC-10
C                                                 PEEK method).
C     Bob Greschke         October 1989      3.1  Added in the collimation
C                                                 offset new calculations,
C                                                 and station ID.
C     Bob Greschke         November 1989     3.2  Had to adjust the //PAD
C                                                 column's for the latest vers.
C                                                 of PTG on the ModComps.
C     Bob Greschke         November 1989  uc 4.0  Additions to the .PTR print-
C                                                 outs. Generates an averages
C                                                 page of various parameters.
C     Rick Perley          December 1991     5.0  Added more terms to model.
C                                                 Using Numerical Recipies
C                                                 routines. Minor changes to
C                                                 run under UNIX
C     Rick Perley          January 1992      5.1  Added the BAND command, and
C                                                 made program band generic.
C     Rick Perley          February 1992     5.2  Added wind, print cmds.
C     Rick Perley          February 1992     5.3  Added EXCEL stuff back in,
C                                                 changed Collimation calc.
C     Rick Perley          April 1992        7.0  Changed data entry to comply
C                                                 with new on-line output.
C     Phillip Hicks        June 1994         7.1  The length of the string
C                                                 with the old pointing
C                                                 parameters increased.
C                                                 The Excel files renamed.
C     Min Yun              December 1996     8.0  Q-band included.  A1/E1
C						  separated.
C     Min Yun              June 1999         8.1  Direct pointing/ROT output
C						  added.
C     Min Yun              September 1999    8.2  report of changes added.
C     Min Yun              January 2000      8.3  ANT 29 added.
C     Ken Sowinski         September 2005    9.0  Many EVLA changes ...
C     Ken Sowinski         October 2008      9.1  S,A bands; no Modcomps
C     eric Greisen         July 2018         AIPS version
C-----------------------------------------------------------------------
      CHARACTER GOTXT*(*)
      INTEGER   IRET
C
      INTEGER   JTRIM, LFN, LUNTMP, JJJ, I, JANT, NOBS, ID
      CHARACTER MYFIL*128, OUTLIN*128, TIMEST*14, IATIME*12, PNTFIL*128,
     *   STANT*2, INLINE*132, PCONST(29)*128, FF*1
      INCLUDE 'PEEK.INC'
      INCLUDE 'PEEKLUN.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'GOtext = ' // GOTXT
      CALL MSGWRT (2)
C                                       This is the form feed character
      FF = CHAR(12)
C                                       open some files
      LFN = JTRIM (FILNAM)
C                                       Open the Input Data
      PNTLUN = 3
      PNTFIL = FILNAM(1:LFN) // '.PNT'
      CALL ZTXOPN ('READ', PNTLUN, PNTIND, PNTFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT DATA FILE'
         GO TO 980
         END IF
C   Open the file <FILNAM>.PTR and leave it open until all of the
C   requested antennas have been processed.  This file will contain all
C   of the fit parameters, and is to be printed after execution.
      MYFIL = FILNAM(1:LFN) // '.PTR'
      PTRLUN = 10
      CALL ZTXOPN ('REWR', PTRLUN, PTRIND, MYFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C   Open the file <FILNAM>.gp and leave it open until all of the
c   requested antennas have been processed.  This file will contain
c   gnuplot commands.
      IF (FPARM(16).LE.0.0) THEN
         MYFIL = FILNAM(1:LFN) // '.gp'
         GPLLUN = LUNTMP (3)
         CALL ZTXOPN ('REWR', GPLLUN, GPLIND, MYFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set term postscript portrait'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set nokey'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set output "plot.ps"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set pointsize 0.5'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         END IF
C   Open the file <FILNAM>.changes and leave it open until all of the
c   requested antennas have been processed.  This file lists antennas
c   with pointing or collimation changes.
      IF (FPARM(18).LE.0.0) THEN
         MYFIL = FILNAM(1:LFN) // '.changes'
         CHGLUN = 11
         CALL ZTXOPN ('REWR', CHGLUN, CHGIND, MYFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT CHANGES FILE'
            GO TO 980
            END IF
         END IF
      IF (FPARM(19).LE.0.0) THEN
         MYFIL = FILNAM(1:LFN) // '.filtchg'
         FCHLUN = LUNTMP (3)
         CALL ZTXOPN ('REWR', FCHLUN, FCHIND, MYFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT FILTERED FILE'
            GO TO 980
            END IF
         END IF
C
 100  CALL ZTXIO ('READ', PNTLUN, PNTIND, INLINE, IRET)
      IF (IRET.EQ.0) THEN
C                                       set up the .change report
C                                       file headings)
         IF (INLINE(3:5).EQ.'RFN') IATIME = INLINE(26:37)
         IF (INLINE(16:19).EQ.'ANT ') THEN
            READ (INLINE(21:22),1100) ID
            PCONST(ID)(1:2) = INLINE(21:22)
            PCONST(ID)(3:3) = ','
            PCONST(ID)(4:) = INLINE(23:100)
            END IF
         IF (INLINE(1:6).EQ.'//OFS2') TIMEST = INLINE(9:21)
         GO TO 100
      ELSE IF (IRET.NE.2) THEN
         WRITE (MSGTXT,1000) IRET, 'READING INPUT DATA FILE'
         GO TO 980
         END IF
C                                       close down 1st pass
      OUTLIN = '#Pointing and Collimation Changes Report'
      JJJ = JTRIM (OUTLIN)
      IF (FPARM(18).LE.0.0) THEN
         CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
            GO TO 980
            END IF
         END IF
      IF (FPARM(19).LE.0.0) THEN
         CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILTERED FILE'
            GO TO 980
            END IF
         END IF
      OUTLIN = '#Run Name:  ' // FILNAM(1:LFN)
      JJJ = JTRIM (OUTLIN)
      IF (FPARM(18).LE.0.0) THEN
         CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
            GO TO 980
            END IF
         END IF
      IF (FPARM(19).LE.0.0) THEN
         CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILTERED FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1110) F0, F1
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILTERED FILE'
            GO TO 980
            END IF
         END IF
      IF (FPARM(18).LE.0.0) THEN
         OUTLIN = '#IAT Time:  ' // TIMEST
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
            GO TO 980
            END IF
         OUTLIN = '#ID AntName Pad  DB Parameter' //
     *      '       VLA name     Old   Change  New   Error'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
            GO TO 980
            END IF
         END IF
C                                       Now we collect data for each
C                                       antenna, one at a time
      CALL ZTXCLS (PNTLUN, PNTIND, IRET)
      DO 200 I = 1,NANTS
         CALL ZTXOPN ('QRED', PNTLUN, PNTIND, PNTFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-OPEN INPUT DATA FILE'
            GO TO 980
            END IF
         JANT = IANT(I)
         WRITE (STANT,1200) JANT
C                                       Open the file that contains
C                                       data summary for plotting
         IF (FPARM(17).LE.0.0) THEN
            PLTLUN = LUNTMP (3)
            JJJ = JTRIM (OUTFIL)
            MYFIL = OUTFIL(:JJJ) // 'ant' // STANT // '.dat'
            CALL ZTXOPN ('REWR', PLTLUN, PLTIND, MYFIL, .FALSE., IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'OPEN SUMMARY DATA PLOT FILE'
               GO TO 980
               END IF
            END IF
C                                       Tell which antenna
         MSGTXT = 'Working on antenna ' // STANT
         CALL MSGWRT (2)
C                                       Collect data for a
C                                       specified antenna
         IF (I.GT.1) THEN
            OUTLIN = FF(1:1)
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
               GO TO 980
               END IF
            END IF
C                                       do it
         CALL SOLVE (JANT, GOTXT, NOBS, PCONST(JANT), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RETURNED BY SOLVE'
            GO TO 980
            END IF
C                                       If there are fewer than 2
C                                       data points, delete the
C                                       summary data file.
         IF (FPARM(17).LE.0.0) THEN
            CALL ZTXCLS (PLTLUN, PLTIND, IRET)
            IF (NOBS.LT.2) THEN
               CALL ZTXZAP (PLTLUN, MYFIL, IRET)
               END IF
            END IF
         CALL ZTXCLS (PNTLUN, PNTIND, IRET)
 200     CONTINUE
      IRET = 0
      GO TO 990
C
 980  CALL MSGWRT (8)
C                                       close the files and exit
 990  MSGSUP = 32000
      CALL ZTXCLS (PTRLUN, PTRIND, I)
      CALL ZTXCLS (PNTLUN, PNTIND, I)
      IF (FPARM(17).LE.0.0) CALL ZTXCLS (PLTLUN, PTRIND, I)
      IF (FPARM(16).LE.0.0) CALL ZTXCLS (GPLLUN, GPLIND, I)
      IF (FPARM(18).LE.0.0) CALL ZTXCLS (CHGLUN, CHGIND, I)
      IF (FPARM(19).LE.0.0) CALL ZTXCLS (FCHLUN, FCHIND, I)
      MSGSUP = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PEEKGO ERROR',I4,' ON ',A)
 1100 FORMAT (I2)
 1110 FORMAT ('# FILTER: change >',F7.2,' and >',F5.1,' * sigma')
 1200 FORMAT (I2.2)
      END
      SUBROUTINE SOLVE (JANT, GOTXT, NROBS, PSTRIN, IRET)
C-----------------------------------------------------------------------
C   Displays the pointing data and provides analysis, e.g. rms etc.,
C   and do a least squares fit to the standard pointing parameters.
C   Inputs:
C      JANT     I       Desired ntenna
C      GOTXT    C*(*)   Determines setting of TILTSW (ANT, PAD)
C   Outputs:
C      NROBS    I       Number unique offsets at reference band
C      PSTRIN   C*(*)
C      IRET     I       error code
C-----------------------------------------------------------------------
      INTEGER   JANT, NROBS, IRET
      CHARACTER GOTXT*(*), PSTRIN*(*)
C
      INTEGER   NOBS, NBND
      PARAMETER ( NBND = 8 )
      INTEGER   LBND, CBND, UBND, KBND, XBND, QBND, SBND, ABND
      PARAMETER (XBND = 1, LBND=2, CBND=3, UBND=4, KBND=5, QBND=6,
     +           SBND=7, ABND=8)
      INTEGER   AZ, EL, LEFT, RIGHT
      PARAMETER (AZ = 1, EL = 2)
      PARAMETER (LEFT = 1, RIGHT = 2)
C
      INTEGER   M, MA, MA0, MX
      PARAMETER (M = 400)
      PARAMETER (MA0 = 12)
      PARAMETER (MA = 17)
      PARAMETER (MX = 800)
C
      INTEGER   ADONE, LDONE, XDONE, UDONE, CDONE, KDONE, PDONE, QDONE,
     *   PADID
      REAL      OLDCOL(2,NBND), DUM
      CHARACTER INLINE*132, OUTLIN*264

C
C       Rickcode Follows  - BE WARNED!
C
C       NBND - The total number of BANDs
C       NOBS - The total number of measured offsets
C       NUOBS - The number of unique measured offsets
C       NROBS - The number of unique offsets at the reference band
C       MA0 = Number of Parameters in the old Pointing Model
C       MA = Number of Parameters in Pointing Model
C       MFIT = Number of Param. being fitted in this trial
C       M = maximum number of observations of az,el offsets
C       MX = 2*M (to allow a single call to the fitting routine)
C       LISTA(M) is the fitting mask, to describe which terms are fitted
C       FTPARM(MA) The best-fit coefficients
C
      INTEGER   MFIT, NCVM, TNR, LISTA(MA)
      REAL      AZM(M), ELV(M), AZERR(M), ELERR(M), SIG(M), CHISQ, AZ1,
     *   EL1, FTPARM(MA), COVAR(MA,MA), DAT(MX), DSIG(MX), DAZM(MX),
     *   DELV(MX), DATFT(MX), DATDF(MX), DATERR(MX), RCOVAR(MA,MA),
     *   SIZE(2,NBND), SCAT(2,NBND), BMINT, BMAXT
      DATA SIG / M*1. /
      DATA NCVM /MA/
C
C     Enter the beamwidths in units defined by the on-line program, in
C     (Az, El) pairs, for X, L, C, U, K, Q, S and A bands, followed by
C     the measured rms scatter in the beamwidths
C
      DATA SIZE/1.04, 1.04, 1.024, 1.024, 1.075, 1.05, 1.069, 1.069,
     +  1.056, 1.056, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/
      DATA SCAT/1.0,1.0,1.0,1.0,0.6,0.6,2.5,2.5,3.4,3.4,4.0,4.0,
     +          1.0, 1.0, 3.7, 3.7/
C
C       End of Rickcode
C
C     K = the number of unique observation times (increments when the time
C         changes
C     C(1) = the number of RHP observations at the reference band
C     C(2) = the number of LHP observations at the reference band
C     F(K) contains the band code for the Kth observation (X = 1, L = 2,
C          C = 3, U = 4, K = 5, Q = 6)
C     LOLIMIT(NBND) defines the min. elevation limit for calculating collimation
C              for each band
C     HILIMIT(NBND) the upper elevation limit, per band, for collimation calc.
C     H(K) = Elevation of the Kth observation
C     A(K) = Azimuth of the Kth observation
C     N = the band indicator (X = 1, L = 2, C = 3, U = 4, K = 5, Q = 6)
C     L(K), R(K) = TRUE if the Kth observation contains a Right or Left Pol'n.
C     J = The Polarization indicator: R = 1, L = 2
C     S(NBND) The # of observations at each band which have both pol'ns.
C     Z(NBND) The min. # of points for a squint solution at each band.
C     O(NBND) The number of observations at each band
C
      REAL      DUMMY, PARAM(MA0), H(M), A(M), OBSTIM(M)
      REAL      DELTA(2,2,M), WIDTH(2,2,M), ON(2,M)
      REAL      ERROR(2,M), BEAM(2,M), AMPL(M), FIT(2,M)
      REAL      SQUINT(2,M), ESTIM(2), OFFSET(2,M)
      REAL      ERRAVE(2,2), ERRSIG(2,2), ERRRMS(2,2)
      REAL      BEAAVE(2,2), BEASIG(2,2), BEARMS(2,2)
      REAL      SQUAVE(2,NBND), SQUSIG(2,NBND), SQURMS(2,NBND),
     +   SQUERR(2,NBND)
      REAL      OFFAVE(2,NBND), OFFSIG(2,NBND), OFFRMS(2,NBND),
     +   OFFERR(2,NBND)
      REAL      DEFAUL(2,NBND), CORREC(2,NBND), LOLIMT(NBND),
     +   HILIMT(NBND)
      REAL      AZNOMR, AZNOMT, AZPOST, AZPRE, BANDS, CA1, CA2, CE1,
     *   CE2, COLAZ, COLEL, DELAZ, DELEL, DELTIM, ELNOMR, ELNOMT,
     *   ELPOST, ELPRE, OLDCOS, PADEW, PADNS, RAZERR, RELERR, RSSR,
     *   RSST, SA1, SA2, SE1, SE2, SUMAIR, SUMAMP, SUAZER, SUELER,
     *   SUMCSA, SUMCSE, SUMDEW, SUMDIF, SUMDIR, SUMMAX, SUPRES, SUMRMS,
     *   SUMSNA, SUMSNE, SUTIME, SUMWHT, SUWIND, ZCHANG, OLDSIN, SUMBLK
      INTEGER   F(M), C(2), Z(NBND), S(NBND), O(NBND), II, IJ, JJJ,
     *   JTRIM, NB1, NB2, NREF, NRNOMR, NRNOMT, NUOBS
      INTEGER   YY, DDD, HRS, MIN
      INTEGER   MONDAY, LSTHR, LSTMIN
      INTEGER   CWIND, CSHAD, CAMPL, CELEV, CBEAM, CTIME
      INTEGER   ID, I, J, K, N
      INTEGER   IND, LST(3,M), KBPLIM, KBP, III
      DOUBLE PRECISION XX
      LOGICAL   LR(M), L(M), R(M), FIRSTR, NEWVER, KNOVER, NEWCOL
      LOGICAL   EVLA, TILTSW
      LOGICAL   PCHANG, CCHANG, COLLIM
      REAL      LSTRAD, LSTSEC, ANWND1, ANWND2, WNDSPD, WNDDIR, WNDRMS
      REAL      TEMP, BAROM, DEWPT, BLCKTM, WHTTMP, DIFFTM, WNDMAX
      REAL      YDJD, DATE, TIME, CLOCK, REFDAY, REFTIME
      REAL      SINEL, COSEL, SINAZ, COSAZ, AM, HK, HA
      REAL      DEL, HPEL, DAZ, HPAZ
      REAL      WINDIR(M), WINDSP(M), AIRTMP(M), DEWTMP(M)
      REAL      BLACKB(M), WHITEB(M), PRESUR(M), BLCKWH(M)
      REAL      WINRMS(M), WINDMX(M)
      REAL      AZCH, AZER, ELCH, ELER
      CHARACTER PAD*3, SOURCE*5, BAND*1, POLAR*1, OTT*1, SHADOW*1
      CHARACTER MODEM*1, PADSTR*8, ANTSTR*4, DBNAME(20)*18
      CHARACTER ANTENT*6, PADENT*6
      CHARACTER ANTWRP*1, MONTH*3, CORMOD*18
      CHARACTER SRCNAM(M)*8, SOURCE1*8, SOURC2*8, REFSRC*8
      CHARACTER LNGSRC*8, VERSON*4
      CHARACTER P(2)*1, B(NBND)*1, BF(NBND)*6, AXIS(2)*2
C
c      INCLUDE 'peekcm.dcl'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'PEEKLUN.INC'
      INCLUDE 'PEEK.INC'

      DATA P / 'L', 'R'/
      DATA B / 'X', 'L', 'C', 'U', 'K', 'Q', 'S', 'A'/
      DATA BF /'10GHz','1.5GHz','6GHz','15GHz','22GHz','45GHz',
     +         '3GHz','33GHz'/
      DATA AXIS / 'Az', 'El' /
      DATA DBNAME /'AZ_EWTILT', 'AZ_NSTILT', 'AZCENTERINGCOS',
     $             'AZCENTERINGSIN', 'PERPENDICULARITY',
     $             'AZCOLIMATION', 'AZENCZERO', 'EWTILT',
     $             'NSTILT', 'ELCENTERINGCOS', 'ELCENTERINGSIN',
     $             'ELCOLIMATION',
     $             '', '', '', '', '', '', '', ''/
      DATA Z / 20, 4, 4, 4, 4, 4, 4, 4/
C
C     We set the limits for collimation calculations at 15 and 75 degrees.
C
      DATA LOLIMT / NBND*.262/
      DATA HILIMT /NBND*1.309/

C   Enter Best Squint Values (Az,El) (R.P. Entered New Values 31/12/91)
C    Band Order is X, L, C, U, K, Q. S, A. Units are arcminutes
C
      DATA DEFAUL  /.045, .297, -1.132, -1.188, .481, .263,
     +              .151, -.088, .107, .011, -0.017, 0.048,
     +              0.77, -0.16, 0.0, 0.0/
C-----------------------------------------------------------------------
CCC DEBUG
C      do 3172 i = 1,nbnd
C 3172    write(6,*) i, oldcol(1,i), oldcol(2,i)
C      write(6,*) (param(i), i=1,12)

      CCHANG = .FALSE.
      PCHANG = .FALSE.
      NOBS = 0
      K = 0
      FIRSTR = .TRUE.
      CALL FILL (2, 0, C)
      CALL FILL (NBND, 0, S)
      CALL FILL (NBND, 0, O)
      CALL FILL (MA, 0, LISTA)
      CALL RFILL (MA*MA, 0.0, COVAR)
      CALL RFILL (MA, 0.0, FTPARM)
      CALL RFILL (4, 0.0, ERRAVE)
      CALL RFILL (4, 0.0, ERRSIG)
      CALL RFILL (4, 0.0, BEAAVE)
      CALL RFILL (4, 0.0, BEASIG)
      I = 2*NBND
      CALL RFILL (I, 0.0, SQUAVE)
      CALL RFILL (I, 0.0, SQUSIG)
      CALL RFILL (I, 0.0, SQUERR)
      CALL RFILL (I, 0.0, OFFAVE)
      CALL RFILL (I, 0.0, OFFSIG)
      CALL RFILL (I, 0.0, OFFERR)
      ADONE = 0
      PDONE = 0
      LDONE = 0
      XDONE = 0
      UDONE = 0
      CDONE = 0
      KDONE = 0
      QDONE = 0
      CWIND = 0
      CSHAD = 0
      CAMPL = 0
      CELEV = 0
      CBEAM = 0
      CTIME = 0
      PAD='XXX'
      KNOVER = .FALSE.
      NEWVER  = .FALSE.
      NEWCOL = .FALSE.
C                                       Read in a line from .PNT
 10   CALL ZTXIO ('READ', PNTLUN, PNTIND, INLINE, IRET)
      IF (IRET.EQ.2) THEN
         GO TO 199
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING PNT FILE'
         GO TO 980
         END IF
C    Check to find the version of the data.  New format data include a
C    '//VER' card at the beginning of the file.  If found, set the
C    appropriate logicals.  If missing, we assume the file is of the
C    old (prior to April 1992) format.  (The Version referred to here is
C    the 'on-line' version, not the PEEK program version number).
      IF (.NOT.KNOVER) THEN
         IF (INLINE(1:5).EQ.'//VER') THEN
            VERSON = INLINE(20:23)
            NEWVER = .TRUE.
            MSGTXT = 'Reading a new format file, Ver. ' // VERSON
            CALL MSGWRT (2)
            IF (VERSON.NE.' ') THEN
               OUTLIN = MSGTXT
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'WRITE OUTPUT PARAMETER FILE'
                  GO TO 980
                  END IF
               END IF
            END IF
         KNOVER = .TRUE.
         END IF
C    We ignore the '//RFN' card.  We do check if the date of the
C    observation is before or after the reference pointing is used
C    for collimation measurements (as of November 1st, 1996).
      IF (INLINE(1:5).EQ.'//RFN') THEN
	 READ (INLINE(26:27),1010) YY
	 READ (INLINE(29:31),1011) DDD
	 IF ((YY.GE.96) .AND. (DDD.GE.300)) NEWCOL = .TRUE.
	 IF (YY.LT.78) NEWCOL = .TRUE.
	 GO TO 10
         END IF
C    Now we begin to find what kind of card we have read.  If the 14th
C    character is a '/', the card is an 'ANT', 'PAD', or 'ROT' card.
C    Information from these cards must be saved.

C Each of the following modules works the same way. It has been determined that
C INLINE contains a line from a MODCOMP system file (ANTENNAS, POINTING or
C SYSxROT). Figure out which one, and check to see if data for the current
C antenna has been found (using the xDONE variable). If not, check to see if
C the current line of data is for the current antenna. If it is get the data
C and set the xDONE variable.
C
C The POINTING file portion:  Identify the card, and read in the old pointing
C parameters if the antenna is the one wanted, and we don't already have that
C information.
C
      IF (INLINE(14:14).EQ.'/') THEN
         IF (INLINE(16:19).EQ.'ANT ') THEN
            IF (PDONE.EQ.1) GO TO 10
            READ (INLINE(21:22),1010) ID
            IF (ID.NE.JANT) GO TO 10
            KBP = 23
            KBPLIM = 132
            DO 15 III = 1,MA0
               CALL GETNUM (INLINE, KBPLIM, KBP, XX)
               PARAM(III) = XX
 15            CONTINUE
            PDONE = 1
            GO TO 10
            END IF

C  The ANTENNAS file portion:  Same idea, if we don't already have the
C  information, and the antenna is the one wanted, read in the antenna
C  location.  If the PAD is "OUT", the antenna is not in use, and the
C  existing pointing constants are simply passed along.
         IF (INLINE(16:19).EQ.'PAD ') THEN
            IF (ADONE.EQ.1) GO TO 10
            READ (INLINE(24:25),1010) ID
            IF (ID.NE.JANT) GO TO 10
            MODEM = INLINE(45:45)
            IF (INLINE(32:32).EQ.'/') THEN
               PAD = INLINE(29:31)
               PADSTR = PAD // '/' // PAD
            ELSE
               PAD = INLINE(33:35)
               IF (PAD.EQ.'OUT') THEN
                  WRITE (OUTLIN,1015) JANT
                  JJJ = JTRIM (OUTLIN)
                  CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ),
     *               IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET,
     *                  'WRITE OUTPUT PARAMETER FILE'
                     GO TO 980
                     END IF
                  GO TO 900
                  END IF
C                                       Crunch out the station ID
C                                       Convert the Old naming system
C                                       to the new
               IF(PAD.EQ.'MAS') THEN
                  PADSTR = 'MAS/MAS'
               ELSE
                  READ (PAD(3:3),1012) PADID
                  IF (PAD(1:1).EQ.'A') PADID = PADID * 8
                  IF (PAD(1:1).EQ.'B') PADID = PADID * 4
                  IF (PAD(1:1).EQ.'C') PADID = PADID * 2
                  WRITE (PADSTR,1016) PAD, '/', PAD(2:2), PADID
                  END IF
               END IF
            EVLA = .FALSE.
            IF (MODEM.EQ.' ') EVLA = .TRUE.
            IF (PAD.EQ.'OUT') THEN
               WRITE (OUTLIN,1015) JANT
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'WRITE OUTPUT PARAMETER FILE'
                  GO TO 980
                  END IF
               GO TO 900
               END IF
C                                       Canonical antenna name string
            TILTSW = .FALSE.
            IF (EVLA) THEN
               ANTSTR(1:2) = 'EA'
            ELSE
               ANTSTR(1:2) = 'VA'
               END IF
            WRITE (ANTSTR(3:4),1013) ID
            IF (GOTXT.EQ.'PAD') TILTSW = .FALSE.
            IF (GOTXT.EQ.'ANT') TILTSW = .TRUE.
C                                       Write out the antenna number
C                                       and location.
            WRITE (OUTLIN,1020) 'Antenna ', ANTSTR, JANT,
     *         ' on pad/station ', PADSTR, ' for runname ', MYNAME(:10)
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
            IF (FPARM(17).LE.0.0) THEN
               WRITE (OUTLIN,1020) '# Antenna ', ANTSTR, JANT,
     *            ' on pad/station ', PADSTR, ' for runname ',
     *            MYNAME(:10)
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PLTLUN, PLTIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'WRITE SUMMARY DATA PLOT FILE'
                  GO TO 980
                  END IF
               END IF
            ADONE = 1
            GO TO 10
            END IF
C                                       ROT card is ignored
C                                       should save collimation values
         IF (INLINE(16:18).EQ.'ROT') THEN
            GO TO 10
            END IF
         END IF
C  If we have gotten here, INLINE must contain pointing or weather  data.
C  We put these data into the appropriate arrays and variables.  The
C  Commented cards write out all the read values, and are used to ensure
C  all data have been properly read.
      IF (NEWVER) THEN
         IF (INLINE(1:6).EQ.'//OFS1') THEN
            READ (INLINE,1050) YY, DDD, HRS, MIN, LNGSRC, BAND, CORMOD
C           WRITE (6,'(8X,I2,X,I3,X,I2,X,I2,3X,A,2X,A,2X,A)')
C     +        YY, DDD, HRS, MIN, LNGSRC, BAND, CORMOD
            GO TO 10
        ELSE IF (INLINE(1:6).EQ.'//OFS2') THEN
           READ (INLINE,1051) YY, MONTH, MONDAY, HRS, MIN, LSTRAD,
     *        LSTHR, LSTMIN, LSTSEC
C          WRITE (6,'(8X,I2,A,I2,X,I2,X,I2,2X,F12.6,2X,I3,X,I2,X,
C    +        F5.2,X)') YY, MONTH, MONDAY, HRS, MIN, LSTRAD, LSTHR,
c    *        LSTMIN,LSTSEC
           GO TO 10
        ELSE IF(INLINE(1:5).EQ.'//WX1') THEN
           READ (INLINE,1052) YY, DDD, HRS, MIN, WNDSPD, WNDDIR, TEMP,
     *        BAROM, DEWPT
C          WRITE(6,'(7X,I2,X,I3,X,I2,X,I2,X,5(2X,F7.2))')
C     +       YY,DDD,HRS,MIN,WNDSPD,WNDDIR,TEMP,BAROM,DEWPT
           GO TO 10
        ELSE IF (INLINE(1:5).EQ.'//WX2') THEN
           READ (INLINE,1053) YY, DDD, HRS, MIN, BLCKTM, WHTTMP, DIFFTM,
     *        WNDMAX, WNDRMS
C          WRITE(6,'(7X,I2,X,I3,X,I2,X,I2,X,4(2X,F7.2),2X,F7.4)')
C     +       YY,DDD,HRS,MIN,BLCKTM,WHTTMP,DIFFTM,WNDMAX,WNDRMS
           GO TO 10
        ELSE IF ((INLINE(4:4).EQ.'R') .OR. (INLINE(4:4).EQ.'L')) THEN
           READ (INLINE,1055) ID, POLAR, SINEL, COSEL, SINAZ, COSAZ,
     *        DEL, HPEL, DAZ, HPAZ, AM, OTT, SHADOW, ANTWRP, ANWND1,
     *        ANWND2
C          WRITE(6,'(I2,X,A,4(X,F8.4),5(X,F6.3),3(X,A),2(X,F7.2))')
C     +       ID,POLAR,SINEL,COSEL,SINAZ,COSAZ,DEL,HPEL,DAZ,HPAZ,AM,OTT,
C     +       SHADOW,ANTWRP,ANWND1,ANWND2
        ELSE IF (INLINE(1:2).EQ.'//') THEN
           GO TO 10
        ELSE
           MSGTXT = 'UNRECOGNIZED INPUT CARD -- Printed Below'
           CALL MSGWRT (7)
           MSGTXT = INLINE(:80)
           CALL MSGWRT (7)
           GO TO 10
           END IF
      ELSE
         READ (INLINE,1060) YY, DDD, HRS, MIN, SOURCE, ID, POLAR, SINEL,
     *      COSEL, SINAZ, COSAZ, DEL, HPEL, DAZ, HPAZ, AM, BAND
         LNGSRC(1:5) = SOURCE(1:5)
         LNGSRC(6:8) = '   '
         END IF
C                                       Compute true antenna elevation
      HK = ATAN2 (SINEL, COSEL)
      HA = ATAN2 (SINAZ, COSAZ)
      IF (HA.LT.0) HA = HA + 2.*PI
C                                       normalize the beamwidths by the
C                                       band-dependent values, adjust
C                                       beamwidth range by known scatter
C
      IF (BAND.EQ.'X') THEN
         HPAZ = HPAZ / SIZE(AZ,XBND)
         HPEL = HPEL / SIZE(EL,XBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,XBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,XBND)
      ELSE IF (BAND.EQ.'L') THEN
         HPAZ = HPAZ / SIZE(AZ,LBND)
         HPEL = HPEL / SIZE(EL,LBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,LBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,LBND)
      ELSE IF (BAND.EQ.'C') THEN
         HPAZ = HPAZ / SIZE(AZ,CBND)
         HPEL = HPEL / SIZE(EL,CBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,CBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,CBND)
      ELSE IF (BAND.EQ.'U') THEN
         HPAZ = HPAZ / SIZE(AZ,UBND)
         HPEL = HPEL / SIZE(EL,UBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,UBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,UBND)
      ELSE IF (BAND.EQ.'K') THEN
         HPAZ = HPAZ / SIZE(AZ,KBND)
         HPEL = HPEL / SIZE(EL,KBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,KBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,KBND)
      ELSE IF (BAND.EQ.'Q') THEN
         HPAZ = HPAZ / SIZE(AZ,QBND)
         HPEL = HPEL / SIZE(EL,QBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,QBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,QBND)
      ELSE IF (BAND.EQ.'S') THEN
         HPAZ = HPAZ / SIZE(AZ,SBND)
         HPEL = HPEL / SIZE(EL,SBND)
         BMINT = 1 - (1-BMIN) * SCAT(1,SBND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,SBND)
      ELSE IF (BAND.EQ.'A') THEN
         HPAZ = HPAZ / SIZE(AZ,ABND)
         HPEL = HPEL / SIZE(EL,ABND)
         BMINT = 1 - (1-BMIN) * SCAT(1,ABND)
         BMAXT = 1 + (BMAX-1) * SCAT(2,ABND)
         END IF
C                                       Check antenna-id, elevation, and
C                                       beamwidth checks
      IF (ID.NE.JANT) GO TO 10
C     EVLA over-the-top Kludge.  PTGSOL in the Modcomps reverses
C     the signs to account for the fact that the Modcomps have
C     the collimations backwards if OTT.  EVLA antennas are more
C     correct, so we have to undo here what PTGSOL does.
C                                       kludge for modcompless system
CCC   IF(EVLA.AND.(OTT.EQ.'T')) THEN
      IF (OTT.EQ.'T') THEN
         DEL = -DEL
         DAZ = -DAZ
         END IF
C     First check whether we are reading a 'new format' file, and if so,
C     whether the data passes the extra checks allowed by the extra
C     information.  First, wind speed and shadowing is checked.
      IF (NEWVER) THEN
         IF (WNDSPD.GT.WMAX) THEN
            CWIND = CWIND + 1
            GO TO 10
            END IF
         IF (SHADOW.EQ.'T') THEN
            CSHAD = CSHAD + 1
            GO TO 10
            END IF
         END IF
      IF (10.*AM.LT.AMIN) THEN
         CAMPL = CAMPL + 1
         GO TO 10
         END IF
      IF ((HK.LT.EMIN) .OR. (HK.GT.EMAX)) THEN
         CELEV = CELEV + 1
         GO TO 10
         END IF
C???? TEMPORARY KLUDGE TO DELETE SOME AZIMUTHS
      IF (HA.GT.3.*PI/2.) THEN
C         CELEV = CELEV+1
C         GO TO 10
         END IF
      IF ((HPEL.LT.BMINT) .OR. (HPEL.GT.BMAXT) .OR. (HPAZ.LT.BMINT) .OR.
     *   (HPAZ.GT.BMAXT)) THEN
         CBEAM = CBEAM + 1
         GO TO 10
         END IF
      DATE = YDJD (YY * 1000. + DDD)
      TIME = HRS * 3600. + MIN * 60.
C                                       timerange check
      IF (((DATE.LT.DATE1) .OR.
     +    ((DATE.EQ.DATE1) .AND. (TIME.LT.TIME1))) .OR.
     +    ((DATE.GT.DATE2) .OR.
     +    ((DATE.EQ.DATE2) .AND. (TIME.GT.TIME2)))) THEN
         CTIME = CTIME + 1
         GO TO 10
         END IF
C                                       convert time to minutes
C                                       set `First Record' Flag
C
      TIME = TIME / 60.
      IF (FIRSTR) THEN
         REFDAY = DATE
         REFTIME = TIME
         FIRSTR = .FALSE.
         END IF
C   If the current time is different than last, increment the data counter,
C   compute the antenna Az. and El., and set the band counter.  Then save
C   the Az., El, source name, time (in minutes since the first observation),
C   and all the other potentially useful information
      IF (TIME.NE.CLOCK) THEN
         CLOCK = TIME
         K = K + 1
         H(K) = ATAN2 (SINEL, COSEL)
         A(K) = ATAN2 (SINAZ, COSAZ)
         IF(A(K).LT.0.) A(K) = A(K) + 2.*PI
         SRCNAM(K) = LNGSRC
         BLACKB(K) = BLCKTM
         WINDSP(K) = WNDSPD
         AIRTMP(K) = TEMP
         DEWTMP(K) = DEWPT
         WHITEB(K) = WHTTMP
         WINDIR(K) = WNDDIR
         PRESUR(K) = BAROM
         BLCKWH(K) = DIFFTM
         WINRMS(K) = WNDRMS
         WINDMX(K) = WNDMAX
         LST(1,K) = LSTHR
         LST(2,K) = LSTMIN
         LST(3,K) = LSTSEC
         OBSTIM(K) = (DATE-REFDAY)*1440. + TIME - REFTIME
         N = IND (BAND, B, NBND)
         F(K) = N
         L(K) = .FALSE.
         R(K) = .FALSE.
         END IF
C                                       Set the Polarization integer for
C                                       the current observations.
      J = IND (POLAR, P, 2)
C    Read the data into the arrays for the appropriate polarization,
C    elevation, azimuth, and data point number

      DELTA(EL,J,K) = DEL
      WIDTH(EL,J,K) = HPEL
      DELTA(AZ,J,K) = DAZ
      WIDTH(AZ,J,K) = HPAZ
      ON(J,K) = 10.*AM
C    Accumulate the sum and sum_square of the offsets and beamwidths
C    (reference band only)
      IF (BAND.EQ.REFBND) THEN
         C(J) = C(J) + 1
         DO 100 I = AZ,EL
            CALL ACCU (ERRAVE(I,J), ERRSIG(I,J), DELTA(I,J,K))
            CALL ACCU (BEAAVE(I,J), BEASIG(I,J), WIDTH(I,J,K))
 100        CONTINUE
         END IF
      R(K) = R(K) .OR. (POLAR.EQ.'R')
      L(K) = L(K) .OR. (POLAR.EQ.'L')
      LR(K) = L(K) .AND. R(K)
C    Check to see if both polarizations are present -- if so, calculate
C     and accumulate the offset differences (i.e., the beam squint)
      IF (LR(K)) THEN
         S(N) = S(N) + 1
         DO 110 I = AZ, EL
            DUMMY = DELTA(I,RIGHT,K) - DELTA(I,LEFT,K)
            CALL ACCU (SQUAVE(I,N), SQUSIG(I,N), DUMMY)
 110        CONTINUE
         END IF
C                                       Now go back to read another line.
      GO TO 10

199   CONTINUE

C    We have reached the end of the input file, so thus have accumulated
C    all of the useable data for the requested antenna.
C    Now let's go and PROCESS IT!
C                                       Explain about rejected trials
      WRITE (OUTLIN,1200) CTIME, CWIND, CAMPL, CELEV, CBEAM, CSHAD
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (MSGTXT,1201) CTIME, 'timerange', CWIND, 'wind'
      CALL MSGWRT (3)
      WRITE (MSGTXT,1201) CAMPL, 'amplitude', CELEV, 'elevation'
      CALL MSGWRT (3)
      WRITE (MSGTXT,1201) CBEAM, 'beamwidth', CSHAD, 'shadowing'
      CALL MSGWRT (3)
C                                       If there are no data for either
C                                       polarization at ref. band, QUIT
      IF ((C(LEFT).EQ.0) .AND. (C(RIGHT).EQ.0)) THEN
         MSGTXT = 'I have NO DATA'
         CALL MSGWRT (7)
         OUTLIN = 'I have NO DATA'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
            GO TO 980
            END IF
         GO TO 900
         END IF
C                                       compute mean offset, error,
C                                       beamwidth and error
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1210) 'BEAM', JANT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = 'Poln. Axis Points   ' //
     + ' Error avg.  Error rms. Error sigma.  Beam avg.  Beam sigma.'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      DO 220 J = LEFT,RIGHT
         IF (C(J).GT.0) THEN
            DO 215 I = AZ,EL
               CALL STATS (C(J), ERRAVE(I,J), ERRSIG(I,J), ERRRMS(I,J),
     *            DUM)
               CALL STATS (C(J), BEAAVE(I,J), BEASIG(I,J), BEARMS(I,J),
     *            DUM)
               WRITE (OUTLIN,1215) P(J), AXIS(I), C(J), ERRAVE(I,J),
     *            ERRRMS(I,J), ERRSIG(I,J), BEAAVE(I,J), BEASIG(I,J)
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,'WRITE OUTPUT PARAMETER FILE'
                  GO TO 980
                  END IF
 215           CONTINUE
         ELSE
            OUTLIN = 'No data for ' // P(J) // P(J)
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
            END IF
 220     CONTINUE
C                                       Estimate error in offset using
C                                       measured error in beam widths
      DO 225 I = AZ,EL
         ESTIM(I) = SQRT (1.667 * (C(1) * BEASIG(I,1)**2 +
     +      C(2) * BEASIG(I,2)**2) / (C(1) + C(2)))
 225     CONTINUE
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1225) 'Estimated mean error in measured offset =',
     +   60 * ESTIM(AZ), 60 * ESTIM(EL), ' arc sec'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C     We now compute the average beam squint, and its statistics.
C     If there are not enough data to determine the squints reliably,
C     we use defaults to determine correction factor.
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1210) 'SQUINT', JANT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      DO 250 N = 1,NBND
         IF (S(N).GT.0) THEN
            DO 230 I = AZ,EL
               CALL STATS (S(N), SQUAVE(I,N), SQUSIG(I,N), SQURMS(I,N),
     +            SQUERR(I,N))
 230           CONTINUE
            WRITE (OUTLIN,1230) B(N), ' band (R-L) squint:', S(N),
     *         ' points  Squint: ', SQUAVE(AZ,N), ' ', SQUAVE(EL,N),
     *         '  error: ', SQUERR(AZ,N), ' ', SQUERR(EL,N)
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
            END IF
         IF (S(N).LT.Z(N)) THEN
            DO 235 I = AZ, EL
               CORREC(I,N) = DEFAUL(I,N)
 235           CONTINUE
            WRITE (OUTLIN,1235) B(N), ' band: default squint of',
     +         CORREC(AZ,N), CORREC(EL,N), ' used to correct data.'
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
         ELSE
            DO 240 I = 1, 2
               CORREC(I,N) = SQUAVE(I,N) / 2
 240           CONTINUE
            OUTLIN = B(N) // ' band: squint used to correct data.'
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
            END IF
 250     CONTINUE
C                                       First correct for beam squints.
      NOBS = K
      DO 300 K = 1,NOBS
         N = F(K)
C                                       If both polarizations present,
C                                       no correction needed.
C                                       compute mean offset directly.
         IF (LR(K)) THEN
            DO 260 I = AZ,EL
               BEAM(I,K) =  (WIDTH(I,RIGHT,K) + WIDTH(I,LEFT,K))/ 2
               ERROR(I,K) = (DELTA(I,RIGHT,K) + DELTA(I,LEFT,K))/ 2
               SQUINT(I,K) = DELTA(I,RIGHT,K) - DELTA(I,LEFT,K)
 260           CONTINUE
            AMPL(K) = (ON(RIGHT,K) + ON(LEFT,K)) / 2
C                                       If the Kth data point is missing
C                                       either the RH or LH value,
C                                       adjust the point by half the
C                                       squint offset
         ELSE IF (R(K)) THEN
            AMPL(K) = ON(RIGHT,K)
            DO 270 I = AZ,EL
               BEAM(I,K) =  WIDTH(I,RIGHT,K)
               ERROR(I,K) = DELTA(I,RIGHT,K) - CORREC(I,N)
 270           CONTINUE
         ELSE IF (L(K)) THEN
            AMPL(K) = ON(LEFT,K)
            DO 280 I = AZ,EL
               BEAM(I,K) =  WIDTH(I,LEFT,K)
               ERROR(I,K) = DELTA(I,LEFT,K) + CORREC(I,N)
 280           CONTINUE
            END IF
 300     CONTINUE
C     Having completed filling the arrays with data of pointing offsets,
C     we can now proceed with the pointing analysis.
C       DANGER!  Rickcode Begins Here.  Be Warned!
C     First we load the data into the arrays we need for Numer. Recip.
      NREF = IND(REFBND,B,NBND)
C                                       print out every raw number
C                                       (used for debugging)
C
      IF (VERBOS) THEN
         DO 310 K = 1,NOBS
            WRITE (MSGTXT,1300) K, OBSTIM(K), SRCNAM(K), B(F(K)),
     +         AMPL(K), ERROR(AZ,K), ERROR(EL,K), F(K),
     *         A(K)*RAD2DG, H(K)*RAD2DG
            CALL MSGWRT (2)
            WRITE (MSGTXT,1301) AIRTMP(K), DEWTMP(K), WHITEB(K),
     *         BLACKB(K), BLCKWH(K), WINDSP(K), WINDIR(K), WINRMS(K),
     *         WINDMX(K), PRESUR(K)
            CALL MSGWRT (2)
 310        CONTINUE
         END IF
C                                       Count how much raw data we've
C                                       got at each band.
      CALL FILL (NBND, 0, O)
      DO 315 K = 1,NOBS
         IF (F(K).EQ.XBND) O(XBND) = O(XBND) + 1
         IF (F(K).EQ.LBND) O(LBND) = O(LBND) + 1
         IF (F(K).EQ.CBND) O(CBND) = O(CBND) + 1
         IF (F(K).EQ.UBND) O(UBND) = O(UBND) + 1
         IF (F(K).EQ.KBND) O(KBND) = O(KBND) + 1
         IF (F(K).EQ.QBND) O(QBND) = O(QBND) + 1
         IF (F(K).EQ.SBND) O(SBND) = O(SBND) + 1
         IF (F(K).EQ.ABND) O(ABND) = O(ABND) + 1
 315     CONTINUE
      MSGTXT = 'DATA SUMMARY'
      CALL MSGWRT (3)
      WRITE (MSGTXT,1315) 'Total:  ', NOBS, 'L', O(LBND), 'S', O(SBND),
     *   'C', O(CBND), 'X', O(XBND)
      CALL MSGWRT (3)
      WRITE (MSGTXT,1316) 'U', O(UBND), 'K', O(KBND), 'A', O(ABND), 'Q',
     *   O(QBND)
      CALL MSGWRT (3)
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = 'DATA SUMMARY'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1317) 'Total:  ', NOBS, 'L', O(LBND), 'S', O(SBND),
     *   'C', O(CBND), 'X', O(XBND), 'U', O(UBND), 'K', O(KBND), 'A',
     *   O(ABND), 'Q', O(QBND)
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C    Temporary code to find dispersions of repeated observations of the
C    same source at the same band.
C      I = 1
C 881  SAZ = 0.
C      SAZS = 0.
C      SEL = 0.
C      SELS = 0.
C      CALL ACCU(SAZ,SAZS,ERROR(AZ,I)*60.)
C      CALL ACCU(SEL,SELS,ERROR(EL,I)*60.)
C      J = 1
C 880  IF((F(I+J).EQ.F(I)) .AND. (SRCNAM(I+J).EQ.SRCNAM(I))) THEN
C         CALL ACCU (SAZ, SAZS, ERROR(AZ,I+J)*60.)
C         CALL ACCU (SEL, SELS, ERROR(EL,I+J)*60.)
C         J=J+1
C         GO TO 880
C      ELSE
C         CALL STATS (J, SAZ, SAZS, RMSAZ, RMSAZE)
C         CALL STATS (J, SEL, SELS, RMSEL, RMSELE)
C         WRITE (LUNTTO,'(I2,1X,A,1X,2F8.1)') J,SRCNAM(I),SAZS,SELS
C         WRITE (PTRLUN,'(I2,1X,A,1X,2F8.1)') J,SRCNAM(I),SAZS,SELS
C         END IF
C      I = I + J
C      IF(I.LT.NOBS) GO TO 881
C   Oftentimes, the same object is observed repeatedly at the same band.
C   These repeated observations should be averaged, as they do not give
C   independent information.  Thus, we now average data when the same
C   source is observed at the same band when separated by < 5 degrees.
      N = 0
      I = 1
 320  AZ1 = A(I)
      EL1 = H(I)
      NB1 = F(I)
      CA1 = COS (A(I))
      SA1 = SIN (A(I))
      CE1 = COS (H(I))
      SE1 = SIN (H(I))
      SOURCE1 = SRCNAM(I)
      J = 1
 325  NB2 = F(I+J)
      CA2 = COS (A(I+J))
      SA2 = SIN (A(I+J))
      CE2 = COS (H(I+J))
      SE2 = SIN (H(I+J))
      SOURC2 = SRCNAM(I+J)
      DELAZ = ABS (ATAN2 ((SA2*CA1-CA2*SA1), (CA2*CA1+SA2*SA1))*RAD2DG)
      DELEL = ABS (ATAN2 ((SE2*CE1-CE2*SE1), (CE2*CE1+SE2*SE1))*RAD2DG)
      IF ((DELAZ.LT..005) .AND. (DELEL.LT..005) .AND. (NB1.EQ.NB2) .AND.
     +   (SOURCE1.EQ.SOURC2)) THEN
         J = J + 1
         GO TO 325
      ELSE
         N = N + 1
         SRCNAM(N) = SOURCE1
         F(N) = F(I)
C                                       no average
         IF(J.EQ.1) THEN
            A(N) = AZ1
            H(N) = EL1
            AMPL(N) = AMPL(I)
            OBSTIM(N) = OBSTIM(I)
            ERROR(AZ,N) = ERROR(AZ,I)
            ERROR(EL,N) = ERROR(EL,I)
            WINDSP(N) = WINDSP(I)
            AIRTMP(N) = AIRTMP(I)
            DEWTMP(N) = DEWTMP(I)
            WHITEB(N) = WHITEB(I)
            BLACKB(N) = BLACKB(I)
            WINDIR(N) = WINDIR(I)
            PRESUR(N) = PRESUR(I)
            BLCKWH(N) = BLCKWH(I)
            WINRMS(N) = WINRMS(I)
            WINDMX(N) = WINDMX(I)
C                                       average
         ELSE
            SUMSNA = 0.
            SUMCSA = 0.
            SUMSNE = 0.
            SUMCSE = 0.
            SUAZER = 0.
            SUELER = 0.
            SUMMAX = 0.
            DO 330 K = 0,J-1
               SUMSNA = SUMSNA + SIN(A(I+K))
               SUMSNE = SUMSNE + SIN(H(I+K))
               SUMCSA = SUMCSA + COS(A(I+K))
               SUMCSE = SUMCSE + COS(H(I+K))
               SUAZER = SUAZER + ERROR(AZ,I+K)
               SUELER = SUELER + ERROR(EL,I+K)
               SUMMAX = MAX (SUMMAX, WINDMX(I+K))
 330           CONTINUE
            IJ = I + J - 1
            CALL SUMIT (SUWIND, I, IJ, WINDSP)
            CALL SUMIT (SUMAMP, I, IJ, AMPL)
            CALL SUMIT (SUTIME, I, IJ, OBSTIM)
            CALL SUMIT (SUMAIR, I, IJ, AIRTMP)
            CALL SUMIT (SUMDEW, I, IJ, DEWTMP)
            CALL SUMIT (SUMWHT, I, IJ, WHITEB)
            CALL SUMIT (SUMBLK, I, IJ, BLACKB)
            CALL SUMIT (SUMDIR, I, IJ, WINDIR)
            CALL SUMIT (SUPRES, I, IJ, PRESUR)
            CALL SUMIT (SUMDIF, I, IJ, BLCKWH)
            CALL SUMIT (SUMRMS, I, IJ, WINRMS)
            WINDSP(N) = SUWIND / J
            AMPL(N)   = SUMAMP / J
            OBSTIM(N) = SUTIME / J
            AIRTMP(N) = SUMAIR / J
            DEWTMP(N) = SUMDEW / J
            WHITEB(N) = SUMWHT / J
            BLACKB(N) = SUMBLK / J
            WINDIR(N) = SUMDIR / J
            PRESUR(N) = SUPRES / J
            BLCKWH(N) = SUMDIF / J
            WINRMS(N) = SUMRMS / J
            WINDMX(N) = SUMMAX
            SUMSNA = SUMSNA / J
            SUMSNE = SUMSNE / J
            SUMCSA = SUMCSA / J
            SUMCSE = SUMCSE / J
            A(N) = ATAN2(SUMSNA,SUMCSA)
            IF(A(N).LT.0.) A(N) = A(N) + 2.*PI
            H(N) = ATAN2(SUMSNE,SUMCSE)
            ERROR(AZ,N) = SUAZER / J
            ERROR(EL,N) = SUELER / J
            END IF
         END IF
      I = I + J
      IF (I.LE.NOBS) GO TO 320
C                                       count unique points each band
      NUOBS = N
      CALL FILL (NBND, 0, O)
      DO 340 K = 1,NUOBS
         IF (F(K).EQ.XBND) O(XBND) = O(XBND) + 1
         IF (F(K).EQ.LBND) O(LBND) = O(LBND) + 1
         IF (F(K).EQ.CBND) O(CBND) = O(CBND) + 1
         IF (F(K).EQ.UBND) O(UBND) = O(UBND) + 1
         IF (F(K).EQ.KBND) O(KBND) = O(KBND) + 1
         IF (F(K).EQ.QBND) O(QBND) = O(QBND) + 1
         IF (F(K).EQ.SBND) O(SBND) = O(SBND) + 1
         IF (F(K).EQ.ABND) O(ABND) = O(ABND) + 1
 340     CONTINUE
      WRITE (MSGTXT,1315) 'Unique: ', NOBS, 'L', O(LBND), 'S', O(SBND),
     *   'C', O(CBND), 'X', O(XBND)
      CALL MSGWRT (3)
      WRITE (MSGTXT,1316) 'U', O(UBND), 'K', O(KBND), 'A', O(ABND), 'Q',
     *   O(QBND)
      CALL MSGWRT (3)
      WRITE (OUTLIN,1317) 'Unique: ', NOBS, 'L', O(LBND), 'S', O(SBND),
     *   'C', O(CBND), 'X', O(XBND), 'U', O(UBND), 'K', O(KBND), 'A',
     *   O(ABND), 'Q', O(QBND)
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C                                       Is this be a collimation run?
      BANDS = 0
      DO 345 K = 1,NBND
         IF (O(K).GT.0) BANDS = BANDS + 1
 345     continue
      IF ((BANDS.GT.1) .AND. (O(NREF).LT.30)) THEN
         COLLIM = .TRUE.
         MSGTXT = 'This is a collimation run.'
      ELSE
         COLLIM = .FALSE.
         MSGTXT = 'This is NOT a collimation run.'
         END IF
      CALL MSGWRT (2)
C                                       collect reference band data.
      NROBS = 0
      DO 350 K = 1,NUOBS
         IF (F(K).EQ.NREF) THEN
            NROBS = NROBS+1
            AZM(NROBS) = A(K)
            ELV(NROBS) = H(K)
            AZERR(NROBS) = ERROR(AZ,K)
            ELERR(NROBS) = ERROR(EL,K)
            SIG(NROBS) = 1.
            END IF
 350     CONTINUE
      WRITE (MSGTXT,1350) NROBS, REFBND
      CALL MSGWRT (3)
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1350) NROBS, REFBND
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C   Combine the Az and El offsets into one vector. (This is to allow for
C   the tilt terms (which affect both Az and El offsets) to be solved for
C   simultaneously using both offsets.
      TNR = 2*NROBS
      DO 360 I = 1,TNR
         IF (I.LE.NROBS) THEN
            DAZM(I) = AZM(I)
            DELV(I) = ELV(I)
            DAT(I) = AZERR(I)
            DSIG(I) = SIG(I)
         ELSE
            II = I - NROBS
            DAZM(I) = AZM(II)
            DELV(I) = ELV(II)
            DAT(I) = ELERR(II)
            DSIG(I) = SIG(II)
            END IF
 360     CONTINUE
C   Now fill the Fitting Mask, LISTA (which is needed to tell the least-
C   square program which coefficients to solve for.
      I = 1
      IF (TILT) THEN
         LISTA(I) = 1
         LISTA(I+1) = 2
         I = I + 2
         END IF
      IF (AZCNTR) THEN
         LISTA(I) = 3
         LISTA(I+1) = 4
         I = I + 2
         END IF
      IF (AXSPRP) THEN
         LISTA(I) = 5
         I = I + 1
         END IF
      IF (COLIMA) THEN
         LISTA(I) = 6
         I = I + 1
         END IF
      IF (AZZERO) THEN
         LISTA(I) = 7
         I = I + 1
         END IF
      IF (TILT.AND.TILTSW) THEN
         LISTA(I) = 8
         LISTA(I+1) = 9
         I = I + 2
         END IF
      IF (SAG) THEN
         LISTA(I) = 10
         I = I + 1
         END IF
      IF (ELCNTR) THEN
         LISTA(I) = 11
         I = I + 1
         END IF
      IF (ELZERO) THEN
         LISTA(I) = 12
         I = I + 1
         END IF
      IF (AZ3) THEN
         LISTA(I) = 13
         LISTA(I+1) = 14
         I = I + 2
         END IF
      IF (AZ2) THEN
         LISTA(I) = 15
         LISTA(I+1) = 16
         I = I + 2
         END IF
      IF (REFRAC) THEN
         LISTA(I) = 17
         I = I + 1
         END IF
C                                       number of terms to be fit
      MFIT = I - 1
C   Check to see if we have enough points to determine parameters
C   If not, return with the original pointing constants and
C   collimation offsets.
      IF (NROBS.LE.MFIT) THEN
         MSGTXT = 'There are more coefficients than data points, I quit'
         CALL MSGWRT (7)
         OUTLIN = MSGTXT
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
            GO TO 980
            END IF
         GO TO 900
         END IF
C                                       Now call the Fitting Routine
      CALL LFAZEL (DAZM, DELV, DAT, DSIG, TNR, FTPARM, MA, LISTA,
     +   MFIT, TILTSW, COVAR, NCVM, CHISQ, DATFT, DATDF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RETURNED BY LFAZEL'
         GO TO 980
         END IF
C                                       compute errors, write results.
      DO 400 I = 1,MA
         DATERR(I) = SQRT (COVAR(I,I) * CHISQ / (NROBS-MFIT-1))
 400     CONTINUE
      IF ((TILT) .AND. (.NOT.TILTSW)) THEN
         FTPARM(8) = -FTPARM(1)
         FTPARM(9) = FTPARM(2)
         DATERR(1) = DATERR(1) / 1.4142
         DATERR(2) = DATERR(2) / 1.4142
         DATERR(8) = DATERR(1)
         DATERR(9) = DATERR(2)
         END IF
      OUTLIN = 'Normalized Covariance Matrix:'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      DO 410 I = 1,MA
         DO 405 J = 1,MA
            IF (ABS(COVAR(I,J)).GT.1E-4) THEN
               RCOVAR(I,J) = COVAR(I,J) / SQRT (COVAR(I,I)*COVAR(J,J))
               END IF
 405        CONTINUE
 410     CONTINUE
      IF (VERBOS) THEN
         DO 415 I = 1,MA
            WRITE (OUTLIN,1410) (RCOVAR(I,J), J = 1,MA)
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
               GO TO 980
               END IF
 415        CONTINUE
         END IF
C                                       Chi squared
      WRITE (MSGTXT,1415) CHISQ
      CALL MSGWRT (4)
      OUTLIN = MSGTXT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C                                       Pointing solution header
      WRITE (MSGTXT,1420) JANT
      CALL MSGWRT (4)
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = MSGTXT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = '         EWTLT NSTLT AZENC AZENC AXPRP HCOLL AZERO ' //
     *   'EWTLT NSTLT  SAG  ELENC ELZRO   3A    3B'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = 'Parameter: A1    A2    A3    A4    A5    A6    A7    '
     *   // 'E1    E2    E3    E4    E5    3A    3B    2A    2B   REFR'
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1425) 'Old     ', (PARAM(I), I = 1,12), 0.0, 0.0,
     *   0.0, 0.0, 0.0
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1425) 'Change  ', (FTPARM(I), I = 1,MA)
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1425) 'Error   ', (DATERR(I), I = 1,MA)
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (OUTLIN,1425) 'New     ', (PARAM(I)+FTPARM(I), I = 1,12),
     *   0.0+FTPARM(13), 0.0+FTPARM(14), FTPARM(15), FTPARM(16),
     *   FTPARM(17)
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C                                       message file 9/go to fit
      MSGTXT = '         EWTLT NSTLT AZENC AZENC ' //
     *   'AXPRP HCOLL AZERO EWTLT NSTLT'
      CALL MSGWRT (4)
      MSGTXT = 'Parameter: A1    A2    A3    A4    ' //
     *   'A5    A6    A7    E1    E2'
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Old     ', (PARAM(I), I = 1,9)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Change  ', (FTPARM(I), I = 1,9)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Error   ', (DATERR(I), I = 1,9)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'New     ', (PARAM(I)*FTPARM(I), I = 1,9)
      CALL MSGWRT (4)
      MSGTXT = '           SAG  ELENC ELZRO   3A    3B'
      CALL MSGWRT (4)
      MSGTXT = 'Parameter: E3    E4    E5    3A    3B    ' //
     +   '2A    2B   REFR'
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Old     ', (PARAM(I), I = 10,12), 0.0, 0.0,
     *   0.0, 0.0, 0.0
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Change  ', (FTPARM(I), I = 10,MA)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'Error   ', (DATERR(I), I = 10,MA)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1425) 'New     ', (PARAM(I)*FTPARM(I), I = 10,12),
     *   0.0, 0.0, FTPARM(15), FTPARM(16), FTPARM(17)
      CALL MSGWRT (4)
C                                       write out new pointing constants
C                                       if change is larger than 3 sigma
c
      DO 430 I = 1,MA
        ZCHANG = ABS (FTPARM(I) / DATERR(I) / 3.0)
        IF ((ZCHANG.GE.1.0) .AND. (I.NE.17)) THEN
            PCHANG = .TRUE.
C           WRITE(CHGLUN,'(3X,I2,A4,X,A,X,A7,I2,A1,X,3(X,F6.2))')
c     +        JANT, ANTSTR, PADSTR, 'FTPARM(', I, ')', PARAM(I),
c     +        FTPARM(I), PARAM(I)+FTPARM(I), DATERR(I)
            END IF
 430     CONTINUE

      IF ((PCHANG) .AND. (.NOT.COLLIM)) THEN
         WRITE (PSTRIN,1430) JANT, (',', PARAM(I)+FTPARM(I), I = 1,12)
         IF (FPARM(18).LE.0.0) THEN
            DO 435 I = 1,12
               IF (DATERR(I).GT.0.0) THEN
                  WRITE (OUTLIN,1435) '#', JANT, ANTSTR, PADSTR,
     *               DBNAME(I), 'APARM(', I, ')', PARAM(I), FTPARM(I),
     *               PARAM(I)+FTPARM(I), DATERR(I)
                  JJJ = JTRIM (OUTLIN)
                  CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET,
     *                  'WRITE OUTPUT CHANGES FILE'
                     GO TO 980
                     END IF
                  END IF
 435           CONTINUE
            END IF
         PADEW = FTPARM(8)
         PADNS = FTPARM(9)
         CALL ROTATE (PADSTR(5:7), PADEW, PADNS, FTPARM(8), FTPARM(9))
         OLDCOS = FTPARM(3)
         OLDSIN = FTPARM(4)
         CALL ROTATE (PADSTR(5:7), OLDCOS, OLDSIN, FTPARM(3), FTPARM(4))
         IF ((EVLA) .AND. (PADSTR(5:7).NE.'MAS')) THEN
            ANTENT = '#'//ANTSTR(1:4)
            PADENT = PADSTR(5:7)
         ELSE
            ANTENT = ANTSTR(1:4)
            PADENT = '#'//PADSTR(5:7)
            END IF
         DO 450 I = 3,12
            IF (ABS(FTPARM(I)).GT.0.005) THEN
               IF ((I.EQ.8) .OR. (I.EQ.9)) THEN
                  WRITE (OUTLIN,1440) ANTENT, ',,', DBNAME(I), ',$',
     *               FTPARM(I)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  IF ((ABS(FTPARM(I)).GT.F0) .AND. (ANTENT(:1).NE.'#')
     *               .AND. (ABS(FTPARM(I)).GT.F1*DATERR(I)) .AND.
     *               (FPARM(19).LE.0.0)) THEN
                     CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT FILTERED FILE'
                        GO TO 980
                        END IF
                     END IF
               ELSE IF ((I.EQ.6) .OR. (I.EQ.12)) THEN
C*   The idea here is to update only L or S band if it was an
C*   L or S band observation, else to update all the bands but
C*   L or S bands.
                  IF (REFBND.EQ.'L') THEN
                     WRITE (OUTLIN,1441) ANTSTR, ',', BF(LBND), ',',
     *                  DBNAME(I), ',$', FTPARM(I)
                     JJJ = JTRIM (OUTLIN)
                     IF (FPARM(18).LE.0.0) THEN
                        CALL ZTXIO ('WRIT', CHGLUN, CHGIND,
     *                     OUTLIN(:JJJ), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITE OUTPUT CHANGES FILE'
                           GO TO 980
                           END IF
                        END IF
                     IF ((ABS(FTPARM(I)).GT.F0) .AND.
     *                  (ABS(FTPARM(I)).GT.F1*DATERR(I)) .AND.
     *                  (FPARM(19).LE.0.0)) THEN
                        CALL ZTXIO ('WRIT', FCHLUN, FCHIND,
     *                     OUTLIN(:JJJ), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITE OUTPUT FILTERED FILE'
                           GO TO 980
                           END IF
                        END IF
                  ELSE IF (REFBND.EQ.'S') THEN
                     WRITE (OUTLIN,1441) ANTSTR, ',', BF(SBND), ',',
     *                  DBNAME(I), ',$', FTPARM(I)
                     JJJ = JTRIM (OUTLIN)
                     IF (FPARM(18).LE.0.0) THEN
                        CALL ZTXIO ('WRIT', CHGLUN, CHGIND,
     *                     OUTLIN(:JJJ), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITE OUTPUT CHANGES FILE'
                           GO TO 980
                           END IF
                        END IF
                     IF ((ABS(FTPARM(I)).GT.F0) .AND.
     *                  (ABS(FTPARM(I)).GT.F1*DATERR(I)) .AND.
     *                  (FPARM(19).LE.0.0)) THEN
                        CALL ZTXIO ('WRIT', FCHLUN, FCHIND,
     *                     OUTLIN(:JJJ), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITE OUTPUT FILTERED FILE'
                           GO TO 980
                           END IF
                        END IF
                  ELSE
                     DO 440 II = 1,NBND
                        IF ((II.NE.LBND) .AND. (II.NE.SBND)) THEN
                           WRITE (OUTLIN,1441) ANTSTR, ',', BF(II), ',',
     *                        DBNAME(I), ',$', FTPARM(I)
                           JJJ = JTRIM (OUTLIN)
                           IF (FPARM(18).LE.0.0) THEN
                              CALL ZTXIO ('WRIT', CHGLUN, CHGIND,
     *                           OUTLIN(:JJJ), IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'WRITE OUTPUT CHANGES FILE'
                                 GO TO 980
                                 END IF
                              END IF
                           IF ((ABS(FTPARM(I)).GT.F0) .AND.
     *                        (ABS(FTPARM(I)).GT.F1*DATERR(I)) .AND.
     *                        (FPARM(19).LE.0.0)) THEN
                              CALL ZTXIO ('WRIT', FCHLUN, FCHIND,
     *                           OUTLIN(:JJJ), IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1000) IRET,
     *                              'WRITE OUTPUT FILTERED FILE'
                                 GO TO 980
                                 END IF
                              END IF
                           END IF
 440                    CONTINUE
                     END IF
               ELSE
                  WRITE (OUTLIN,1440) ANTSTR, ',,', DBNAME(I), ',$',
     *               FTPARM(I)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  IF ((ABS(FTPARM(I)).GT.F0) .AND.
     *               (ABS(FTPARM(I)).GT.F1*DATERR(I)) .AND.
     *               (FPARM(19).LE.0.0)) THEN
                     CALL ZTXIO ('WRIT', FCHLUN, FCHIND,
     *                  OUTLIN(:JJJ), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT FILTERED FILE'
                        GO TO 980
                        END IF
                     END IF
                  END IF
               END IF
 450        CONTINUE
         IF (ABS(PADEW).GT.0.005) THEN
            WRITE (OUTLIN,1450) PADENT, ',,EWTILT', ',$', PADEW
            JJJ = JTRIM (OUTLIN)
            IF (FPARM(18).LE.0.0) THEN
               CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
                  GO TO 980
                  END IF
               END IF
            IF ((ABS(PADEW).GT.F0) .AND. (ABS(PADEW).GT.F1*DATERR(8))
     *         .AND. (PADENT(:1).NE.'#') .AND. (FPARM(19).LE.0.0)) THEN
               CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILTERED FILE'
                  GO TO 980
                  END IF
               END IF
            END IF
         IF (ABS(PADNS).GT.0.005) THEN
            WRITE (OUTLIN,1450) PADENT, ',,NSTILT', ',$', PADNS
            JJJ = JTRIM (OUTLIN)
            IF (FPARM(18).LE.0.0) THEN
               CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT CHANGES FILE'
                  GO TO 980
                  END IF
               END IF
            IF ((ABS(PADNS).GT.F0) .AND. (ABS(PADNS).GT.F1*DATERR(9))
     *         .AND. (PADENT(:1).NE.'#') .AND. (FPARM(19).LE.0.0)) THEN
               CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT FILTERED FILE'
                  GO TO 980
                  END IF
               END IF
            END IF
C        WRITE(CHGLUN,'(I2,6(A1,A,F5.2),A,F6.2,4(A,F5.2),A,F6.2)')
C     +     JANT, (',', PARAM(I)+FTPARM(I), I = 1,12)
C      ELSE
C         WRITE (PSTRING,'(I2,6(A,F5.2),A,F6.2,A,1X,F5.2,
C     +     3(A,F5.2),A,F6.2)') JANT, (',',PARAM(i), I = 1,12)
         END IF
C                                       Compute and Write out the Pre-
C                                       and Post-Fit r.m.s. error
      AZPRE = 0.
      AZPOST = 0.
      ELPRE = 0.
      ELPOST = 0.
      AZNOMR = 0.
      ELNOMR = 0.
      NRNOMR = 0
      AZNOMT = 0.
      ELNOMT = 0.
      NRNOMT = 0
      DO 460 I = 1,NROBS
         AZPRE = AZPRE + DAT(I)**2.
         AZPOST = AZPOST + DATDF(I)**2.
         II = NROBS + I
         ELPRE = ELPRE + DAT(II)**2.
         ELPOST = ELPOST + DATDF(II)**2.
         IF ((H(I).GT.0.349) .AND. (H(I).LT.1.396)) THEN
            AZNOMT = AZNOMT + DATDF(I)**2
            ELNOMT = ELNOMT + DATDF(II)**2
            NRNOMT = NRNOMT + 1
            END IF
         IF ((H(I).GT.0.524) .AND. (H(I).LT.1.222)) THEN
            AZNOMR = AZNOMR + DATDF(I)**2
            ELNOMR = ELNOMR + DATDF(II)**2
            NRNOMR = NRNOMR + 1
            END IF
 460     CONTINUE
      AZPRE = SQRT (AZPRE / NROBS) * 60.
      AZPOST = SQRT (AZPOST / NROBS) * 60.
      ELPRE = SQRT (ELPRE / NROBS) * 60.
      ELPOST = SQRT (ELPOST / NROBS) * 60.
      AZNOMR = SQRT (AZNOMR / NRNOMR) * 60.
      ELNOMR = SQRT (ELNOMR / NRNOMR) * 60.
      AZNOMT = SQRT (AZNOMT / NRNOMT) * 60.
      ELNOMT = SQRT (ELNOMT / NRNOMT) * 60.
      RSST = SQRT (AZNOMT*AZNOMT + ELNOMT*ELNOMT)
      RSSR = SQRT (AZNOMR*AZNOMR + ELNOMR*ELNOMR)
      WRITE (MSGTXT,1460) 'Prefit rms: ', AZPRE, ELPRE
      CALL MSGWRT (4)
      OUTLIN = ' '
      JJJ = 1
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      OUTLIN = MSGTXT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (MSGTXT,1460) 'Postfit rms:', AZPOST, ELPOST
      CALL MSGWRT (4)
      OUTLIN = MSGTXT
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (MSGTXT,1465) NRNOMR, AZNOMR, ELNOMR, RSSR
      CALL MSGWRT (4)
      WRITE (OUTLIN,1466) NRNOMR, AZNOMR, ELNOMR, RSSR
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
      WRITE (MSGTXT,1465) NRNOMT, AZNOMT, ELNOMT, RSST
      CALL MSGWRT (4)
      WRITE (OUTLIN,1466) NRNOMT, AZNOMT, ELNOMT, RSST
      JJJ = JTRIM (OUTLIN)
      CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
         GO TO 980
         END IF
C   Now we compute the collimation offsets with respect to the standard
C   band.  First, we subtract the model from the data if old format
C   (before Nov. 1996).
      DO 470 K = 1,NUOBS
         N = F(K)
         SINAZ = SIN (A(K))
         COSAZ = COS (A(K))
         SINEL = SIN (H(K))
         COSEL = COS (H(K))
         FIT(AZ,K) = SINEL * (FTPARM(1)*COSAZ + FTPARM(2)*SINAZ) +
     +      COSEL * (FTPARM(3)*COSAZ + FTPARM(4)*SINAZ) +
     +      FTPARM(5)*SINEL + FTPARM(6) + FTPARM(7)*COSEL
c         FIT(EL,K) = -FTPARM(1)*SINAZ + FTPARM(2)*COSAZ +  ! A1=-E1
         FIT(EL,K) = FTPARM(8)*SINAZ + FTPARM(9)*COSAZ +
     +      FTPARM(10)*COSEL + FTPARM(11)*SINEL + FTPARM(12) +
     +      FTPARM(13)*COS(3*A(K)) + FTPARM(14)*SIN(3*A(K)) +
     +      FTPARM(15)*COS(2*A(K)) + FTPARM(16)*SIN(2*A(K)) +
     +      FTPARM(17)*TAN(ABS(PI/2 - H(K)))
         IF ((NEWCOL).AND.(N.NE.NREF)) THEN
            OFFSET(AZ,K) = ERROR(AZ,K)
            OFFSET(EL,K) = ERROR(EL,K)
         ELSE IF (N.NE.NREF) THEN
            OFFSET(AZ,K) = ERROR(AZ,K) - FIT(AZ,K)
            OFFSET(EL,K) = ERROR(EL,K) - FIT(EL,K)
            END IF
 470     CONTINUE
C   Find and remember the source name, obs. time, and offsets for
C   each non-reference band observation which lies within the allowed
C   elevation range.
      IF (COLLIM) THEN
         I = 2 * NBND
         CALL RFILL (I, 0.0, OFFAVE)
         CALL RFILL (I, 0.0, OFFSIG)
         CALL RFILL (I, 0.0, OFFERR)
         CALL FILL (NBND, 0, O)

         DO 490 I = 1,NUOBS
            N = F(I)
            IF ((N.NE.NREF) .AND. (H(I).GT.LOLIMT(N)).AND.
     +         (H(I).LT.HILIMT(N))) THEN
               REFSRC = SRCNAM(I)
               REFTIME = OBSTIM(I)
               RAZERR = OFFSET(AZ,I)
               RELERR = OFFSET(EL,I)
C   Then find the reference band observation of the same source
C   and subtract its offset if observed within 20 minutes.
C   Accumulate these differences by band.
               DO 480 J = 1,NUOBS
                  K = F(J)
                  SOURCE1 = SRCNAM(J)
                  TIME = OBSTIM(J)
                  DELTIM = ABS (REFTIME - TIME)
                  IF ((SOURCE1.EQ.REFSRC) .AND. (DELTIM.LT.20.).AND.
     +               (K.EQ.NREF)) THEN
                     O(N) = O(N) + 1
                     IF (NEWCOL) THEN
                        COLAZ = RAZERR
                        COLEL = RELERR
                     ELSE
                        COLAZ = RAZERR - OFFSET(AZ,J)
                        COLEL = RELERR - OFFSET(EL,J)
                        END IF
                     CALL ACCU (OFFAVE(AZ,N), OFFSIG(AZ,N), COLAZ)
                     CALL ACCU (OFFAVE(EL,N), OFFSIG(EL,N), COLEL)
                     END IF
 480              CONTINUE
               END IF
 490        CONTINUE
         WRITE (MSGTXT,1490) 'COLLIMATION OFFSETS-ANTENNA', JANT
         CALL MSGWRT (4)
         OUTLIN = ' '
         JJJ = 1
         CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
            GO TO 980
            END IF
         OUTLIN = MSGTXT
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
            GO TO 980
            END IF
         MSGTXT = 'Band  Points       Old    Change       New       Err'
         CALL MSGWRT (4)
         OUTLIN = MSGTXT
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT PARAMETER FILE'
            GO TO 980
            END IF
C                                       Find the average offsets
C                                       and write them out.
         DO 520 N = 1,NBND
            DO 510 I = AZ,EL
               IF (O(N).GT.0) THEN
                  CALL STATS (O(N), OFFAVE(I,N), OFFSIG(I,N),
     +               OFFRMS(I,N), OFFERR(I,N))
                  END IF
 510           CONTINUE
            IF (O(N).NE.0) THEN
               WRITE (MSGTXT,1510) B(N), O(N), OLDCOL(AZ,N),
     *            OFFAVE(AZ,N), OLDCOL(AZ,N)+OFFAVE(AZ,N), OFFERR(AZ,N),
     *            'Az'
               CALL MSGWRT (4)
               OUTLIN = MSGTXT
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'WRITE OUTPUT PARAMETER FILE'
                  GO TO 980
                  END IF
               WRITE (MSGTXT,1511) OLDCOL(EL,N), OFFAVE(EL,N),
     *            OLDCOL(EL,N)+OFFAVE(EL,N), OFFERR(EL,N), 'El'
               CALL MSGWRT (4)
               OUTLIN = MSGTXT
               JJJ = JTRIM (OUTLIN)
               CALL ZTXIO ('WRIT', PTRLUN, PTRIND, OUTLIN(:JJJ), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET,
     *               'WRITE OUTPUT PARAMETER FILE'
                  GO TO 980
                  END IF
C                                       If change > 3 sigma update
               AZCH = OFFAVE(AZ,N)
               AZER = OFFERR(AZ,N)*3.
               ELCH = OFFAVE(EL,N)
               ELER = OFFERR(EL,N)*3.
               IF ((ABS(AZCH).GE.AZER) .OR. (ABS(ELCH).GE.ELER)) THEN
                  CCHANG = .TRUE.
                  WRITE (OUTLIN,1515) '#', JANT, 'AZ COLL(', B(N), ')',
     *               OLDCOL(AZ,N), OFFAVE(AZ,N),
     *               OLDCOL(AZ,N)+OFFAVE(AZ,N), OFFERR(AZ,N)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  WRITE (OUTLIN,1515) '#', JANT, 'EL COLL(', B(N), ')',
     *               OLDCOL(EL,N), OFFAVE(EL,N),
     *               OLDCOL(EL,N)+OFFAVE(EL,N), OFFERR(EL,N)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  WRITE (OUTLIN,1516) ANTSTR, ',', BF(N),
     *               ',AZCOLIMATION,$', OFFAVE(AZ,N)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  IF ((ABS(OFFAVE(AZ,N)).GT.F0) .AND.
     *               (ABS(OFFAVE(AZ,N)).GT.F1*OFFERR(AZ,N)) .AND.
     *               (FPARM(19).LE.0.0)) THEN
                     CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT FILTERED FILE'
                        GO TO 980
                        END IF
                     END IF
                  WRITE (OUTLIN,1516) ANTSTR, ',', BF(N),
     +               ',ELCOLIMATION,$', OFFAVE(EL,N)
                  JJJ = JTRIM (OUTLIN)
                  IF (FPARM(18).LE.0.0) THEN
                     CALL ZTXIO ('WRIT', CHGLUN, CHGIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT CHANGES FILE'
                        GO TO 980
                        END IF
                     END IF
                  IF ((ABS(OFFAVE(EL,N)).GT.F0) .AND.
     *               (ABS(OFFAVE(EL,N)).GT.F1*OFFERR(EL,N)) .AND.
     *               (FPARM(19).LE.0.0)) THEN
                     CALL ZTXIO ('WRIT', FCHLUN, FCHIND, OUTLIN(:JJJ),
     *                  IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET,
     *                     'WRITE OUTPUT FILTERED FILE'
                        GO TO 980
                        END IF
                     END IF
                  END IF
            ELSE
               MSGTXT = B(N) // '     No solution'
               CALL MSGWRT (7)
               END IF
 520        CONTINUE
         END IF
C                                       Write out all the data
      IF (FPARM(17).LE.0.0) THEN
         IF (.NOT.NEWVER) THEN
            OUTLIN = '#PNT TIME SOURCE   B  FLUX  #   AZ    EL  AZERR'
     *         // ' AZFIT AZDIF ELERR ELFIT ELDIF'
         ELSE
            OUTLIN = '#PNT TIME SOURCE   B  FLUX  #   AZ    EL   TEMP'
     *         // '  DWPT WIND WNAZ WRMS WMAX BAROM AZERR' //
     *         ' AZFIT AZDIF ELERR ELFIT ELDIF  AZSQU   ELSQU'
            END IF
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', PLTLUN, PLTIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE SUMMARY DATA PLOT FILE'
            GO TO 980
            END IF
         CALL FILL (NBND, 0, O)
         DO 530 K = 1,NUOBS
            N = F(K)
            O(N) = O(N) + 1
            FIT(AZ,K) = FIT(AZ,K) - OFFAVE(AZ,N)
            FIT(EL,K) = FIT(EL,K) - OFFAVE(EL,N)
            OFFSET(AZ,K) = OFFSET(AZ,K) - OFFAVE(AZ,N)
            OFFSET(EL,K) = OFFSET(EL,K) - OFFAVE(EL,N)
            IF (.NOT.NEWVER) THEN
               WRITE (OUTLIN,1520) K, OBSTIM(K), SRCNAM(K), B(N),
     *            AMPL(K),O(N), A(K)*RAD2DG, H(K)*RAD2DG, ERROR(AZ,K),
     *            FIT(AZ,K), OFFSET(AZ,K), ERROR(EL,K), FIT(EL,K),
     *            OFFSET(EL,K)
            ELSE
               WRITE (OUTLIN,1521) K, OBSTIM(K), SRCNAM(K), B(N),
     *            AMPL(K),O(N), A(K)*RAD2DG, H(K)*RAD2DG, AIRTMP(K),
     *            DEWTMP(K), WINDSP(K),  WINDIR(K), WINRMS(K),
     *            WINDMX(K), PRESUR(K), DAT(K), DATFT(K), DATDF(K),
     *            DAT(NUOBS+K), DATFT(NUOBS+K), DATDF(NUOBS+K),
     *            SQUINT(1,K), SQUINT(2,K)
C     +        ERROR(AZ,K),FIT(AZ,K),OFFSET(AZ,K),
C     +        ERROR(EL,K),FIT(EL,K),OFFSET(EL,K)
               END IF
            JJJ = JTRIM (OUTLIN)
            CALL ZTXIO ('WRIT', PLTLUN, PLTIND, OUTLIN(:JJJ), IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE SUMMARY DATA PLOT FILE'
               GO TO 980
               END IF
 530        CONTINUE
         END IF
      IF (DOPLOT.GT.0.0) THEN
         CALL AIPLOT (JANT, PADSTR, MYNAME, NUOBS, OBSTIM, A, H, DAT,
     *      DATDF, DAT(NUOBS+1), DATDF(NUOBS+1), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'MAKING AN AIPS PLOT'
            CALL MSGWRT (7)
            END IF
         IF (IRET.NE.0) DOPLOT = 0.0
         END IF
C                                       write plotting instructions
      IF (FPARM(16).LE.0.0) THEN
         OUTLIN = 'set nolabel'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set label "' // MYNAME(1:8) // '" at scr 0.0,.99'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1490) 'set label "Antenna', JANT,
     *         '/' // PADSTR(:3) // '" at scr 0.5,.99 cent'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set multiplot'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set size 0.5,0.333'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.0,0.667'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "Az Error vs Az"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set xrange[0:360]'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 7:($16-$17) w p pt 7, "" u 7:16'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.5,0.667'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "El Error vs Az"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 7:($19-$20) w p pt 7, "" u 7:19'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.0,0.333'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "Az Error vs El"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set xrange[0:125]'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 8:($16-$17) w p pt 7, "" u 8:16'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.5,0.333'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "El Error vs El"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 8:($19-$20) w p pt 7, "" u 8:19'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.0,0.0'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "Az Error vs Time"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set autoscale'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 2:($16-$17) w p pt 7, "" u 2:16'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set origin 0.5,0.0'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set title "El Error vs Time"'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         WRITE (OUTLIN,1491) 'plot "ant', JANT,
     *         '.dat" u 2:($19-$20) w p pt 7, "" u 2:19'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         OUTLIN = 'set nomultiplot'
         JJJ = JTRIM (OUTLIN)
         CALL ZTXIO ('WRIT', GPLLUN, GPLIND, OUTLIN(:JJJ), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT GNUPLOT FILE'
            GO TO 980
            END IF
         END IF
C                                       Time to Go Home!
 900  GO TO 999
C                                       error
 980  CALL MSGWRT (8)
C
999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOLVE ERROR',I4,' ON ',A)
 1010 FORMAT (I2)
 1011 FORMAT (I3)
 1012 FORMAT (I1)
 1013 FORMAT (I2.2)
 1015 FORMAT ('Antenna ',I2.2,' is out of the array.')
 1016 FORMAT (3A,I2.2)
 1020 FORMAT (A,A,I3.2,4A)
 1050 FORMAT (8X,I2,1X,I3,1X,I2,1X,I2,3X,A,2X,A,2X,A)
 1051 FORMAT (8X,I2,A,I2,1X,I2,1X,I2,2X,F12.6,2X,I3,1X,I2,1X,F5.2,X)
 1052 FORMAT (7X,I2,1X,I3,1X,I2,1X,I2,1X,5(2X,F7.2))
 1053 FORMAT (7X,I2,1X,I3,1X,I2,1X,I2,1X,4(2X,F7.2),2X,F7.4)
 1055 FORMAT (I2,1X,A,4(1X,F8.4),5(1X,F6.3),3(1X,A),2(1X,F7.2))
 1060 FORMAT (I2,1X,I3,2(1X,I2),1X,A,2X,I2,1X,A,4F8.4,2(2X,2F6.3,X),
     +   F6.3,3X,A)
 1200 FORMAT ('Rejected trials:',I3,' for timerange,',I3,' for wind,',
     *   I3,' for amplitude,',I3,' for elevation,',I3,' for beamwidth,',
     *   I3,' for shadowing.')
 1201 FORMAT ('Rejected',I3,' for ',A,I4,' for ',A)
 1210 FORMAT (A,' SOLUTIONS FOR ANTENNA ',I2.2,':')
 1215 FORMAT (2X,A,4X,A,I7,5F12.3)
 1225 FORMAT (A,2F5.1,A)
 1230 FORMAT (2A,I5,A,F8.3,A,F8.3,A,F8.3,A,F8.3)
 1235 FORMAT (A,A,2F7.3,A)
 1300 FORMAT (I3,1X,F5.0,1X,A8,1X,A1,1X,F5.2,2F6.2,1X,I2,2F6.1)
 1301 FORMAT (6X,4F6.1,2F5.1,F5.0,2F5.1,F5.0)
 1315 FORMAT (A8,I4,4('; ',A1,'-band:',I4))
 1316 FORMAT (12X,4('; ',A1,'-band:',I4))
 1317 FORMAT (A8,I4,8('; ',A1,'-band:',I4))
 1350 FORMAT ('There are',I4,' measurements at ',A1,'-band ',
     +   'being used for basic fit')
 1410 FORMAT (17F6.2)
 1415 FORMAT ('Chi-Square =',F8.3)
 1420 FORMAT ('Pointing solution for antenna',I3.2,':')
 1425 FORMAT (A8,17F6.2)
 1430 FORMAT (I2,6(A,F5.2),A,F6.2,A,1X,F5.2,3(A,F5.2),A,F6.2)
 1435 FORMAT (A1,I2,1X,A4,1X,A,A,A7,I2,A1,1X,4(1X,F6.2))
 1440 FORMAT (A,A2,A,A2,F7.2)
 1441 FORMAT (A,A1,A,A1,A,A2,F7.2)
 1450 FORMAT (A,A,A2,F6.2)
 1460 FORMAT (A,8X,'Az =',F8.1,' Arcsec',5X,'El =',
     *   F8.1,' Arcsec')
 1465 FORMAT ('Restricted rms:(',I2,') Az=',F7.1,' Arcsec',
     +   ' El=',F7.1,' Asec, RSS=',F5.1)
 1466 FORMAT ('Restricted rms:(',I2,') Az =',F8.1,' Arcsec',5X,
     +   'El =',F8.1,' Arcsec,    RSS=',F5.1)
 1490 FORMAT (A,I3.2,A)
 1491 FORMAT (A,I2.2,A)
 1510 FORMAT (A,5X,I4,2X,4(2X,F8.2),2X,A)
 1511 FORMAT (12X,4(2X,F8.2),2X,A)
 1515 FORMAT (A,I2,2X,A8,A1,A1,4(X,F5.2))
 1516 FORMAT (4A,F7.2)
 1520 FORMAT (I3,1X,F5.0,1X,A8,1X,A1,1X,F5.2,1X,I2,2F6.1,6F6.2)
 1521 FORMAT (I3,1X,F5.0,1X,A8,1X,A1,1X,F5.2,1X,I2,2F6.1,2F6.1,
     +   1F5.1,F5.0,2F5.1,F5.0,6F6.2,2F8.3)
      END
      SUBROUTINE LFAZEL (AZ, EL, Y, SIG, NDATA, A, MA, LISTA, MFIT,
     *   TILTSW, COVAR, NCVM, CHISQ, YFIT, YDIF, IRET)
C-----------------------------------------------------------------------
C   fits the data
C   Inputs:
C      AZ       R(*)   Azimuth (NDATA)
C      EL       R(*)   Elevation (NDATA)
C      Y        R(*)   data (NDATA)
C      SIG      R(*)   data uncertainty (NDATA)
C      NDATA    I      Number of data points
C      MA       I      Dimension of A
C      LISTA    I(*)   Pointers (MA)
C      MFIT     I      Number parameters fit
C      TILTSW   L      TRUE if fitting tilts separately; else FALSE
C      NCVM     I      Dimension of covariance matrix
C   Outputs
C      A        R(*)   ?? (MA)
C      COVAR    r(*)   Covariance matrix (NCVM,NCVM)
C      CHISQ    R      Chi-suqared of fit
C      YFIT     R(*)   Fit values (NDATA)
C      YDIF     R(*)   Data - fit (NDATA)
C      IRET     I      Error code: 8 was pause, 0 okay
C-----------------------------------------------------------------------
      INTEGER   NDATA, MA, LISTA(*), MFIT, NCVM, IRET
      REAL      AZ(*), EL(*), Y(*), SIG(*), A(*), COVAR(NCVM,NCVM),
     *   CHISQ, YFIT(*), YDIF(*)
      LOGICAL   TILTSW
C
      INTEGER   MMAX
      PARAMETER (MMAX=20)
      REAL      BETA(MMAX), AFUNC(MMAX), SIG2I, SUM, WT, YM
      INTEGER   I, J, K, IHIT, IAZEL, KK
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Sort, Check Coefficients List
C     WRITE (MSGTXT,1100) MA, MFIT, NDATA, LISTA
C     CALL MSGWRT (2)
C     WRITE(6,1101) (A(J), J = 1,5)
C     CALL MSGWRT (2)
C     WRITE(6,1102) (A(J), J = 6,10)
C     CALL MSGWRT (2)
      IRET = 0
      KK = MFIT + 1
      DO 20 J = 1,MA
         IHIT = 0
         DO 10 K = 1,MFIT
            IF (LISTA(K).EQ.J) IHIT = IHIT + 1
 10         CONTINUE
         IF (IHIT.EQ.0) THEN
            LISTA(KK) = J
            KK = KK + 1
          ELSE IF (IHIT.GT.1) THEN
             MSGTXT = 'Improper set in LISTA -- 1'
             GO TO 990
             END IF
20       CONTINUE
      IF (KK.NE.(MA+1)) THEN
         MSGTXT = 'Improper set in LISTA --2'
         GO TO 990
         END IF
C                                       Clear Design Matrix and
C                                       Data Vector
      DO 40 J = 1,MFIT
         DO 30 K = 1,MFIT
            COVAR(J,K) = 0.
 30         CONTINUE
         BETA(J) = 0.
 40      CONTINUE
C                                       Evaluate Basis Functions
      DO 80 I = 1,NDATA
C                                       Azimuth or Elevation Point?
         IF (I.LE.NDATA/2) THEN
            IAZEL = 1
         ELSE
            IAZEL = 2
            END IF
         CALL FUAZEL (AZ(I), EL(I), AFUNC, MA, IAZEL, TILTSW)
C        WRITE (MSGTXT,1105) I, Y(I), (AFUNC(II), II = 1,9)
C        CALL MSGWRT (2)
         YM = Y(I)
C   Adjust input data by known, non-fitted coefficients
C   (commented out by msy on 11/26/96 because A(LISTA(J))
C   is zero and the subtraction below does nothing)
         IF (MFIT.LT.MA) THEN
            DO 50 J = MFIT+1,MA
C              WRITE(6,*) YM, LISTA(J), A(LISTA(J)), AFUNC(LISTA(J))
               YM = YM - A(LISTA(J)) * AFUNC(LISTA(J))
 50            CONTINUE
            END IF
C                                       Weight data by known errors
C                                       fill in Design Matrix
         SIG2I = 1. / SIG(I)**2
         DO 70 J = 1,MFIT
            WT = AFUNC(LISTA(J)) * SIG2I
            DO 60 K = 1,J
               COVAR(J,K) = COVAR(J,K) + WT * AFUNC(LISTA(K))
 60            CONTINUE
C                                       And the Data Vector
            BETA(J) = BETA(J) + YM * WT
 70         CONTINUE
 80      CONTINUE
C                                       Fill in Opposite Side of
C                                       Design Matrix
      IF (MFIT.GT.1) THEN
         DO 110 J = 2,MFIT
            DO 90 K = 1,J-1
               COVAR(K,J) = COVAR(J,K)
 90            CONTINUE
 110        CONTINUE
         END IF
C                                       Solve for Parameters
      CALL GAUSSJ (COVAR, MFIT, NCVM, BETA, 1, 1, IRET)
      IF (IRET.NE.0) GO TO 999
      DO 120 J = 1,MFIT
         A(LISTA(J)) = BETA(J)
 120     CONTINUE
      CHISQ = 0.
      DO 140 I = 1,NDATA
         IF (I.LE.NDATA/2) THEN
            IAZEL = 1
         ELSE
            IAZEL = 2
            END IF
         CALL FUAZEL (AZ(I), EL(I), AFUNC, MA, IAZEL, TILTSW)
         SUM = 0.
C                                       Calculate Fit, Residuals,
C                                       and Errors
         DO 130 J = 1,MA
            SUM = SUM + A(J) * AFUNC(J)
 130        CONTINUE
         YFIT(I) = SUM
         YDIF(I) = Y(I) - SUM
         CHISQ = CHISQ + (YDIF(I)/SIG(I))**2
 140     CONTINUE
      CALL COVSRT (COVAR, NCVM, MA, LISTA, MFIT)
      GO TO 999
C                                       error (was PAUSE!)
 990  CALL MSGWRT (8)
      IRET = 8
C
 999  RETURN
C-----------------------------------------------------------------------
C1100 FORMAT (20I3)
C1101 FORMAT ('A',5F10.3)
C1102 FORMAT (' ',5F10.3)
C1105 FORMAT (I3,1X,10F6.4)
      END
      SUBROUTINE GAUSSJ (A, N, NP, B, M, MP, IRET)
C-----------------------------------------------------------------------
C
C   Inputs:
C      N      I       Portion of A actually used
C      NP     I       Dimension of A and B
C      M      I       Portion of B actually used
C   In/Outputs:
C      A      R(*)    Array(NP,NP) examined up to N
C      B      R(*)    Array(NP,NP) examined up to M
C   Outputs:
C      IRET   I       error code: 8 -> singular matrix, 0 okay
C-----------------------------------------------------------------------
      INTEGER   N, NP, M, MP, IRET
      REAL      A(NP,NP), B(NP,MP)
C
      INTEGER   NMAX
      PARAMETER (NMAX=50)
      INTEGER   I, J, K, IPIV(NMAX), INDXR(NMAX), INDXC(NMAX), IROW,
     *   ICOL, L, LL
      REAL      BIG, DUM, PIVINV
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DO 10 J = 1,N
         IPIV(J)=0
 10      CONTINUE
      IRET = 0
C                                       main loop
      DO 120 I = 1,N
         BIG = 0.
         DO 30 J = 1,N
C                                       find location of maximum
            IF (IPIV(J).NE.1) THEN
               DO 20 K = 1,N
                  IF (IPIV(K).EQ.0) THEN
                     IF (ABS(A(J,K)).GE.BIG) THEN
                        BIG = ABS (A(J,K))
                        IROW = J
                        ICOL = K
                        END IF
                  ELSE IF (IPIV(K).GT.1) THEN
                     GO TO 900
                     END IF
 20               CONTINUE
               END IF
 30         CONTINUE
C
         IPIV(ICOL) = IPIV(ICOL) + 1
         IF (IROW.NE.ICOL) THEN
            DO 40 L = 1,N
               DUM = A(IROW,L)
               A(IROW,L) = A(ICOL,L)
               A(ICOL,L) = DUM
 40            CONTINUE
            DO 50 L=1,M
               DUM = B(IROW,L)
               B(IROW,L) = B(ICOL,L)
               B(ICOL,L) = DUM
 50            CONTINUE
            END IF
C
         INDXR(I) = IROW
         INDXC(I) = ICOL
         IF (A(ICOL,ICOL).EQ.0.) GO TO 900
         PIVINV = 1. / A(ICOL,ICOL)
         A(ICOL,ICOL) = 1.
         DO 60 L = 1,N
            A(ICOL,L) = A(ICOL,L) * PIVINV
 60        CONTINUE
         DO 70 L = 1,M
            B(ICOL,L)=B(ICOL,L)*PIVINV
 70         CONTINUE
         DO 110 LL = 1,N
            IF (LL.NE.ICOL) THEN
               DUM = A(LL,ICOL)
               A(LL,ICOL) = 0.
               DO 80 L = 1,N
                  A(LL,L) = A(LL,L) - A(ICOL,L) * DUM
 80               CONTINUE
               DO 90 L = 1,M
                  B(LL,L) = B(LL,L) - B(ICOL,L) * DUM
 90               CONTINUE
               END IF
 110        CONTINUE
 120     CONTINUE
C
      DO 140 L = N,1,-1
         IF (INDXR(L).NE.INDXC(L)) THEN
            DO 130 K = 1,N
               DUM = A(K,INDXR(L))
               A(K,INDXR(L)) = A(K,INDXC(L))
               A(K,INDXC(L)) = DUM
 130           CONTINUE
            END IF
 140     CONTINUE
      GO TO 999
C
 900  MSGTXT = 'GAUSSJ: SINGULAR MATRIX'
      CALL MSGWRT (8)
      IRET = 8
C
 999  RETURN
      END
      SUBROUTINE FUAZEL (AZ, EL, AFUNC, MA, IAZEL, TILTSW)
C-----------------------------------------------------------------------
C   Computes a function array
C   Inputs:
C      AZ       R      Azimuth
C      EL       R      Elevation
C      MA       I
C      IAZEL    I
C      TILTSW   L      TRUE if fitting tilts separately; else FALSE
C   Outputs:
C      AFUNC    R(*)   see below
C      AFUNC   A    E      Meaning
C         1    1    (-)1   E-W Tilt, positive to W
C         2    2    2      N-S Tilt, positive to N
C         3    3    -      Az. encoder center error, along E-W line
C         4    4    -      Az. encoder center error, along N-S  line
C         5    5    -      Axis perpendicularity error
C         6    6    -      Horizontal Collimation Error
C         7    7    -      Az. encoder zero offset
C         8    -    1      E-W tilt, positive to E if TILTSW
C         9    -    2      N-S tilt, postivie to N if TILTSW
C        10    -    3      Sag and el. encoder center error in vertical
C        11    -    4      El. encoder center error in horizontal plane
C        12    -    5      El. encoder zero offset and Vert. Collimation
C        13    -    -      First component of 3-theta term
C        14    -    -      Second component of 3-theta term
C        15    -    -      First component of 2-theta term
C        16    -    -      Second component of 2-theta term
C        17    -    -      simple refraction
C-----------------------------------------------------------------------
      INTEGER   MA, IAZEL
      LOGICAL   TILTSW
      REAL      AZ, EL, AFUNC(*)
C
      INTEGER   I
C-----------------------------------------------------------------------
      DO 10 I = 1,MA
         AFUNC(I) = 0.
 10      CONTINUE
C
      IF (IAZEL.EQ.1) THEN
         AFUNC(1) = COS(AZ)*SIN(EL)
         AFUNC(2) = SIN(AZ)*SIN(EL)
         AFUNC(3) = COS(AZ)*COS(EL)
         AFUNC(4) = SIN(AZ)*COS(EL)
         AFUNC(5) = SIN(EL)
         AFUNC(6) = 1.
         AFUNC(7) = COS(EL)
         AFUNC(13) = COS(3*AZ)*SIN(EL)
         AFUNC(14) = SIN(3*AZ)*SIN(EL)
         AFUNC(15) = COS(2*AZ)*SIN(EL)
         AFUNC(16) = SIN(2*AZ)*SIN(EL)
C
      ELSE
         IF (TILTSW) THEN
            AFUNC(8) =  SIN(AZ)
            AFUNC(9) =  COS(AZ)
         ELSE
            AFUNC(1)  =-SIN(AZ)
            AFUNC(2)  = COS(AZ)
            END IF
         AFUNC(10)  = COS(EL)
         AFUNC(11)  = SIN(EL)
         AFUNC(12) = 1.
         AFUNC(13) = -SIN(3*AZ)
         AFUNC(14) = COS(3*AZ)
         AFUNC(15) = -SIN(2*AZ)
         AFUNC(16) = COS(2*AZ)
         AFUNC(17) = TAN(ABS(3.14159265/2-EL))
         END IF
C
 999  RETURN
      END
      SUBROUTINE COVSRT (COVAR, NCVM, MA, LISTA, MFIT)
C-----------------------------------------------------------------------
C   sorts the covariance matrix
C   Inputs:
C      NCVM    I      Dimension of covriance matrix
C      MA      I      used portion of matrix
C      MFIT    I      dimension of sort pointers
C      LISTA   I(*)   Sort pointers (MFIT)
C   In/Out
C      COVAR   R(*)   Covariance matrix
C-----------------------------------------------------------------------
      INTEGER   NCVM, MA, MFIT, LISTA(*)
      REAL      COVAR(NCVM,NCVM)
C
      INTEGER   I, J
      REAL      SWAP
C-----------------------------------------------------------------------
C                                       zero lower left matrix
      DO 20 J = 1,MA-1
         DO 10 I = J+1,MA
            COVAR(I,J)=0.
 10         CONTINUE
 20      CONTINUE
C                                       move matrix into lower left
      DO 40 I = 1,MFIT-1
         DO 30 J = I+1,MFIT
            IF (LISTA(J).GT.LISTA(I)) THEN
               COVAR(LISTA(J),LISTA(I)) = COVAR(I,J)
            ELSE
               COVAR(LISTA(I),LISTA(J)) = COVAR(I,J)
               END IF
 30         CONTINUE
 40      CONTINUE
C
      SWAP = COVAR(1,1)
      DO 50 J = 1,MA
         COVAR(1,J)=COVAR(J,J)
         COVAR(J,J)=0.
50       CONTINUE
      COVAR(LISTA(1),LISTA(1)) = SWAP
C
      DO 60 J = 2,MFIT
         COVAR(LISTA(J),LISTA(J)) = COVAR(1,J)
60       CONTINUE
      DO 80 J = 2,MA
         DO 70 I = 1,J-1
            COVAR(I,J) = COVAR(J,I)
 70         CONTINUE
 80      CONTINUE
c
 999  RETURN
      END
      INTEGER FUNCTION IND (ITEM, VECTOR, DIM)
C-----------------------------------------------------------------------
C   determines if a string is in an array of strings
C   Inputs:
C      ITEM     C*(*)      test strings
C      VECTOR   C(*)*(*)   array of strings
C      DIM      I          size of array
C   Outputs:
C      IND      I          position in array or zero
C-----------------------------------------------------------------------
      INTEGER   DIM
      CHARACTER ITEM*(*), VECTOR(*)*(*)
C-----------------------------------------------------------------------
      DO 10 IND = 1,DIM
         IF (ITEM.EQ.VECTOR(IND)) GO TO 999
 10      CONTINUE
      IND = 0
C
 999  RETURN
      END
      SUBROUTINE ACCU (AVE, SIG, SAMPLE)
C-----------------------------------------------------------------------
C   accumulates statistical sums
C   Inputs:
C      SAMPLE   R   new sample value
C   In/Out
C      AVE      R   sum of samples
C      SIG      R   sum of samples squared
C-----------------------------------------------------------------------
      REAL      AVE, SIG, SAMPLE
C-----------------------------------------------------------------------
      AVE = AVE + SAMPLE
      SIG = SIG + SAMPLE * SAMPLE
c
 999  RETURN
      END
      SUBROUTINE STATS (N, AVE, SIG, RMS, ERR)
C-----------------------------------------------------------------------
C   returns the the average, sigma, error from pre-prepared sums
C   Inputs:
C      N     I   Number samples in sums
C      AVE   R   Sum of data
C      SIG   R   Sum of data squared
C   Outputs:
C      AVE   R   Average of data
C      SIG   R   proper RMS of sum
C      RMS   R   Mean data squared
C      ERR   R   proper RMS / sqrt (n-1)
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      AVE, SIG, RMS, ERR
C-----------------------------------------------------------------------
      AVE = AVE / N
      SIG = SIG / N
      RMS = SQRT (MAX (0.0, SIG))
      SIG = SIG - AVE * AVE
      SIG = SQRT (MAX (0.0, SIG))
      IF (N.LE.1) THEN
         ERR = 0.
      ELSE
         ERR = SIG / SQRT (MAX (1.0, N-1.0))
         END IF
C
 999  RETURN
      END
      SUBROUTINE SUMIT (SUM, START, STOP, VECTOR)
C-----------------------------------------------------------------------
C   sums VECTOR from subscript START through STOP
C   Inputs
C      START    I      start subscript
C      STOP     I      end subscript
C      VECTOR   R(*)   vector to sum
C   Outputs:
C      SUM      R      sum
C-----------------------------------------------------------------------
      REAL      SUM, VECTOR(*)
      INTEGER   START, STOP
C
      INTEGER   I
C-----------------------------------------------------------------------
      SUM = 0.
      DO 10 I = START,STOP
         SUM = SUM + VECTOR(I)
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ROTATE (PAD, OLDEW, OLDNS, NEWEW, NEWNS)
C-----------------------------------------------------------------------
C   Rotates an EW/NS pair depending on the pAD code
C   Inputs:
C      PAD     C*(*)   Pad code: 'MAS' and "W" -> -124, 'N' -> -5,
C                         'E' -> -65, 'x01' -> -95
C      OLDEW   R       Input EW value
C      OLDNS   R       Input NS value
C   Outputs
C      NEWEW   R       Rotated EW
C      NEWNS   R       Rotated NS
C-----------------------------------------------------------------------
      CHARACTER PAD*(*)
      REAL      OLDEW, OLDNS, NEWEW, NEWNS
C
      REAL      ANG, COSANG, SINANG
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IF (PAD(1:3).EQ.'MAS') ANG = -124.
      IF (PAD(1:1).EQ.'W') ANG = -124.
      IF (PAD(1:1).EQ.'N') ANG = -5.
      IF (PAD(1:1).EQ.'E') ANG = -65.
      IF (PAD(2:3).EQ.'01') ANG = -95.
      ANG = ANG * DG2RAD
      COSANG = COS (ANG)
      SINANG = SIN (ANG)
      NEWEW = COSANG * OLDEW + SINANG * OLDNS
      NEWNS = -SINANG * OLDEW + COSANG * OLDNS
C
 999  RETURN
      END
      SUBROUTINE SCTIMX (STR1, R1, I1, I2, I3)
C-----------------------------------------------------------------------
C   Return MJAD
C   Inputs:
C      STR1   C*(*)   Function 'FMJAD=Y,M,D' only one supported
C      I1     I       Year since 1900
C      I2     I       Month
C      I3     I       Day
C   Output:
C      R1     R       MJAD
C-----------------------------------------------------------------------
       CHARACTER STR1*(*)
       REAL      R1
       INTEGER   I1, I2, I3
C
       CHARACTER*3  MONTHS(13)
       INTEGER      DAYSIN(12), DAYSBF(13), IYY, Y, M, D, NDAYS
       LOGICAL      LPYEAR
       REAL         MJAD0, D400, D100, D4, D1
C                                       MJAD FOR YEAR 0, DAY 0
       DATA MJAD0   /-678576.0/
       DATA DAYSIN  /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
       DATA DAYSBF  /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304,
     *    334, 365/
       DATA D400,D100, D4, D1 /146097.0, 36524.0, 1461.0, 365.0/
       DATA MONTHS /'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     *    'AUG','SEP','OCT','NOV','DEC','***'/
C-----------------------------------------------------------------------
C
      IYY = 11
      IF (STR1.EQ.'FMJAD=Y,M,D') THEN
         Y = I1 + 1900
         M = I2
         D = I3
         LPYEAR = ((MOD(Y,4) .EQ. 0) .AND. (MOD(Y,100) .NE. 0))
     *      .OR.  (MOD(Y,400) .EQ. 0)
         IF (((M.LE.12) .AND. (D.LE.DAYSIN(M))) .OR.
     *      ((LPYEAR) .AND. (M.EQ.2) .AND. (D.LE.29))) THEN
            Y = Y - 1
            NDAYS = DAYSBF(M) + D
            IF ((LPYEAR) .AND. (M.GT.2)) NDAYS = NDAYS + 1
            R1 = MJAD0 + 365.0*FLOAT(Y) + FLOAT(Y/4)
     *         - FLOAT(Y/100) + FLOAT(Y/400) + FLOAT(NDAYS)
         ELSE
            R1 = 0.0
            END IF
      ELSE
         R1 = 0.0
         END IF
C
 999  RETURN
      END
      REAL FUNCTION YDJD (DATE)
C-----------------------------------------------------------------------
C   This routine converts dates of the form YYDDD to modified Julian
c   atomic dates.  CALLED BY:
C
C           MJAD = YDJD(YYDDD)
C
C   The date to be converted is a real number whose decimal
c   representation is of the form YYDDD (for example 76186).  This
C   number represents the 186th day of the year 1976 or July 4, 1976.
c   When the year is expressed with two digits, the twentieth century
C   is assumed.  For other centuries, use four digits as in 1776186.
C   The modified Julian atomic date as of 0 hours on the given date is
C   returned.
C   Inputs
C      DATE   R   Date in YYYYDDD form
C   Outputs
C      YDJD   R   Modified Julian atomic date at 0 hours
C-----------------------------------------------------------------------
      REAL      DATE
C
      REAL      AMOD, FLOAT, MJD, MJAD0
      INTEGER   IFIX, YEAR, DYEAR, DAY
C                                       MJAD FOR YEAR 0, DAY 0
      DATA MJAD0/-678576.0/
C-----------------------------------------------------------------------
      YEAR = IFIX (DATE/1000.0)
      IF (YEAR.LT.78) YEAR = YEAR + 2000
      IF (YEAR.LT.100) YEAR = YEAR + 1900
      DAY = IFIX (AMOD (DATE, 1000.0))
      DYEAR = YEAR - 1
      MJD = MJAD0 + (365.0*FLOAT(DYEAR)) + FLOAT(DYEAR/4)
     *   - FLOAT(DYEAR/100) + FLOAT(DYEAR/400)
C
      YDJD = MJD + FLOAT(DAY)
C
 999  RETURN
      END
      SUBROUTINE AIPLOT (JANT, PADSTR, DNAME, NUOBS, OBSTIM, A, H,
     *   ADAT, ADATFT, EDAT, EDATFT, IRET)
C-----------------------------------------------------------------------
C   makes an aips plot on the TV or in a file
C   Inputs:
C      JANT     I      Antenna number
C      PADSTR   C*3    Pad ID
C      DNAME    C*(*)  Input data set nick name
C      NUOBS    I      Number data points
C      OBSTIM   R(*)   Relative times
C      A        R(*)   Azimuth
C      H        R(*)   Elevation
C      ADAT     R(*)   Azimuth error
C      ADATFT   R(*)   Azimuth fit
C      EDAT     R(*)   Elevation error
C      EDATFT   R(*)   Elevation fit
C   Outputs:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      INTEGER   JANT, NUOBS, IRET
      CHARACTER PADSTR*3, DNAME*(*)
      REAL      OBSTIM(*), A(*), H(*), ADAT(*), ADATFT(*), EDAT(*),
     *   EDATFT(*)
C
      INCLUDE 'PEEK.INC'
      INCLUDE 'PEEKPLT.INC'
      INTEGER   I, ITYPE, IPSIZE, VER, TVCHAN, TVCORN(2), BUFFER(256),
     *   LUNPL, FINDPL, LTYPE, IDEPTH(5), JTRIM, ID(3), IT(3), INCHAR
      REAL      ADRANG(2), EDRANG(2), TRANG(2), ARANG(2), HRANG(2),
     *   CHOUT(4), DX, DY, BLC(2), TRC(2), ABLC(2), ATRC(2), X, Y
      CHARACTER PFILE*48, TIME*8, DATE*12, TEXT*80
      REAL DBG(50), DBG2(50)
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA IDEPTH /5*1/
C-----------------------------------------------------------------------
      IRET = 0
      LOCNUM = 1
      CALL SETLOC (IDEPTH, .FALSE.)
C                                       find data ranges
      ADRANG(1) = 1.E10
      EDRANG(1) = 1.E10
      TRANG(1) = 1.E10
      ARANG(1) = 1.E10
      HRANG(1) = 1.E10
      ADRANG(2) = -1.E10
      EDRANG(2) = -1.E10
      TRANG(2) = -1.E10
      ARANG(2) = -1.E10
      HRANG(2) = -1.E10
      CALL RCOPY (NUOBS, ADAT, DBG)
      CALL RCOPY (NUOBS, EDAT, DBG2)
      DO 20 I = 1,NUOBS
         A(I) = A(I) * RAD2DG
         H(I) = H(I) * RAD2DG
         ADRANG(1) = MIN (ADRANG(1), ADAT(I))
         ADRANG(1) = MIN (ADRANG(1), ADATFT(I))
         EDRANG(1) = MIN (EDRANG(1), EDAT(I))
         EDRANG(1) = MIN (EDRANG(1), EDATFT(I))
         TRANG(1) = MIN (TRANG(1), OBSTIM(I))
         ADRANG(2) = MAX (ADRANG(2), ADAT(I))
         ADRANG(2) = MAX (ADRANG(2), ADATFT(I))
         EDRANG(2) = MAX (EDRANG(2), EDAT(I))
         EDRANG(2) = MAX (EDRANG(2), EDATFT(I))
         TRANG(2) = MAX (TRANG(2), OBSTIM(I))
 20      CONTINUE
C                                       fixed az, el scales
      ARANG(1) = 0.0
      ARANG(2) = 360.0
      HRANG(1) = 0.0
      HRANG(2) = 125.0
C                                       start on plot
      VER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', INDISK, CNO, CATBLK, SCRTCH, .TRUE., 'WRIT',
     *      VER, IRET)
         FRW(NCFILE) = 0
         IF (IRET.NE.0) NCFILE = NCFILE - 1
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       create plot file
      CALL ZPHFIL ('PL', INDISK, CNO, VER, PFILE, IRET)
      IF (IRET.NE.0) GO TO 999
      ITYPE = 55
      IPSIZE = 0
      TVCHAN = 1
      TVCORN(1) = 0
      TVCORN(2) = 0
      DPARM(10) = JANT
      LUNPL = 28
      CALL GINIT (INDISK, CNO, PFILE, IPSIZE, ITYPE, NPARMS, XINAME,
     *   DOTV, TVCHAN, GRCHAN, TVCORN, CATBLK, BUFFER, LUNPL, FINDPL,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GINIT'
         GO TO 980
         END IF
C                                       character surrounding
      CALL RFILL (4, 0.5, CHOUT)
      LTYPE = 3
      CHOUT(4) = 3.3
C                                       default XYRATIO
      IF (XYRATO.LT.0.01) THEN
         IF (DOTV) THEN
            DX = WINDTV(3) - WINDTV(1) + 1 - CSIZTV(1) * (CHOUT(1)
     *         + CHOUT(3))
            DY = WINDTV(4) - WINDTV(2) + 1 - CSIZTV(2) * (CHOUT(2)
     *         + CHOUT(4))
            XYRATO = 1.0
            IF (DY.GT.0.0) XYRATO = DX / DY
         ELSE
            XYRATO = 1.0
            END IF
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      TRC(1) = 1000.0
      TRC(2) = 1000.0
      IF ((.NOT.DOTV) .AND. (XYRATO.NE.1.0)) THEN
         IF (XYRATO.GT.1.0) THEN
            I = 1000.0 / XYRATO + 0.5
            TRC(2) = I
         ELSE
            I = 1000.0 * XYRATO + 0.5
            TRC(2) = I
            END IF
         END IF
      XYRAT = XYRATO
C                                       Init for line drawing.
      CALL GINITL (BLC, TRC, XYRATO, CHOUT, IDEPTH, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GINITL'
         GO TO 980
         END IF
C                                       top labels
      Y = TRC(2)
      X = BLC(1) + 0.2 * (TRC(1)/2.0 - BLC(1))
      DX = 0.0
      DY = CHOUT(4) - 1.5
      CALL GPOS (X, Y, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GPOS TOP LABELS'
         GO TO 980
         END IF
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, TIME, DATE)
      WRITE (TEXT,1030) VER, DATE, TIME
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCHAR ON DATE STRING'
         GO TO 980
         END IF
      DY = DY - 1.333
      WRITE (TEXT,1035) DNAME, JANT, PADSTR
      INCHAR = JTRIM (TEXT)
      CALL GPOS (X, Y, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GPOS TOP LABELS'
         GO TO 980
         END IF
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCHAR ON FILE/ANTENNA STRING'
         GO TO 980
         END IF
      X = X + TRC(1) / 2.0
      CALL GPOS (X, Y, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GPOS TOP LABELS'
         GO TO 980
         END IF
      CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCHAR ON FILE/ANTENNA STRING'
         GO TO 980
         END IF
C                                       do 6 plots
      DX = TRC(1) / 2.0
      DY = TRC(2) / 3.0
      ABLC(1) = 1
      ATRC(1) = DX
      ABLC(2) = 2 * DY + 1.0
      ATRC(2) = TRC(2)
      CALL PLOT1 (NUOBS, A, ARANG, ADAT, ADATFT, ADRANG, 'Az','Az',
     *   ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 Az error vs Az'
         GO TO 980
         END IF
      ABLC(1) = DX + 1
      ATRC(1) = TRC(1)
      CALL PLOT1 (NUOBS, A, ARANG, EDAT, EDATFT, EDRANG, 'El', 'Az',
     *   ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 El error vs Az'
         GO TO 980
         END IF
      ABLC(1) = 1
      ATRC(1) = DX
      ABLC(2) = DY + 1.0
      ATRC(2) = 2 * DY
      CALL PLOT1 (NUOBS, H, HRANG, ADAT, ADATFT, ADRANG, 'Az', 'El',
     *   ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 Az error vs El'
         GO TO 980
         END IF
      ABLC(1) = DX + 1
      ATRC(1) = TRC(1)
      CALL PLOT1 (NUOBS, H, HRANG, EDAT, EDATFT, EDRANG, 'El', 'El',
     *   ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 El error vs El'
         GO TO 980
         END IF
      ABLC(1) = 1
      ATRC(1) = DX
      ABLC(2) = 0
      ATRC(2) = DY
      CALL PLOT1 (NUOBS, OBSTIM, TRANG, ADAT, ADATFT, ADRANG, 'Az',
     *   'Time', ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 Az error vs Time'
         GO TO 980
         END IF
      ABLC(1) = DX + 1
      ATRC(1) = TRC(1)
      CALL PLOT1 (NUOBS, OBSTIM, TRANG, EDAT, EDATFT, EDRANG, 'El',
     *   'Time', ABLC, ATRC, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'PLOT1 El error vs Time'
         GO TO 980
         END IF
      GPHPAG = JANT.NE.IANT(NANTS)
      CALL GFINIS (BUFFER, IRET)
      I = IRET
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH THE PLOT'
         CALL MSGWRT (8)
         GO TO 990
      ELSE IF (IRET.LT.0) THEN
         MSGTXT = 'Turning off plots as requested'
         CALL MSGWRT (6)
         DOPLOT = 0.0
         END IF
      IRET = 0
      GO TO 999
C
 980  CALL MSGWRT (8)
      MSGTXT = 'WILL TRY TO FINISH PARTIAL PLOT'
      CALL MSGWRT (6)
      GPHPAG = .FALSE.
      CALL GFINIS (BUFFER, I)
C                                       destroy the plot file
 990  IF ((I.NE.0) .AND. (.NOT.DOTV)) THEN
         CALL ZCLOSE (LUNPL, FINDPL, I)
         CALL ZDESTR (INDISK, PFILE, I)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AIPLOT ERROR',I4,' RETURNED BY ',A)
 1030 FORMAT ('Plot file version',I4,' _created ',A12,A8)
 1035 FORMAT (A,10X,'Antenna',I3.2,'/',A)
      END
      SUBROUTINE PLOT1 (NOBS, XVAL, XRANG, Y1VAL, Y2VAL, YRANG, YTYPE,
     *   XTYPE, BLC, TRC, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Plots one panel
C   Inputs:
C      NOBS     I      Number of samples
C      XVAL     R(*)   X values
C      XRANG    R(2)   Range of X values
C      Y1VAL    R(*)   Input data values
C      Y2VAL    R(*)   Afetr fit values
C      YRANG    R(2)   Range of Y values
C      YTYPE    C*(2)  Type of Y axis
C      XTYPE    C*(*)  Type of X axis
C      BLC      R(2)   BLC plot coordinates
C      TRC      R(2)   TRC plot coordinates
C   In/Out
C      BUFFER   I(*)   Plot buffer
C   Outputs
C      IRET     I      Plot error code
C-----------------------------------------------------------------------
      INTEGER   NOBS, BUFFER(*), IRET
      REAL      XVAL(*), XRANG(2), Y1VAL(*), Y2VAL(*), YRANG(2), BLC(2),
     *   TRC(2)
      CHARACTER YTYPE*2, XTYPE*(*)
C
      INTEGER   I
      REAL      XBLC(2), XTRC(2), DX, DY, CH(4), X, Y, XRANGE(2),
     *   YRANGE(2), AX(5), AY(5)
      CHARACTER TEXT*80
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'PEEKPLT.INC'
C-----------------------------------------------------------------------
      DX = TRC(1) - BLC(1)
      XBLC(1) = BLC(1) + 0.2*DX
      XTRC(1) = TRC(1) - 0.01*DX
      DY = TRC(2) - BLC(2)
      XBLC(2) = BLC(2) + 0.2*DY
      XTRC(2) = TRC(2) - 0.01*DY
      DX = (XRANG(2) - XRANG(1)) * 0.04
      DY = (YRANG(2) - YRANG(1)) * 0.04
      XRANGE(1) = XRANG(1) - DX
      XRANGE(2) = XRANG(2) + DX
      YRANGE(1) = YRANG(1) - DY
      YRANGE(2) = YRANG(2) + DY
      TEXT = 'Plotting ' // YTYPE // ' error vs ' // XTYPE
      CALL GCOMNT (-1, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCOMNT'
         GO TO 980
         END IF
C                                       plot border
      CALL GLTYPE (1, BUFFER, IRET)
      IF (IRET.EQ.0) CALL GPOS (XBLC(1), XTRC(2), BUFFER, IRET)
      IF (IRET.EQ.0) CALL GVEC (XBLC(1), XBLC(2), BUFFER, IRET)
      IF (IRET.EQ.0) CALL GVEC (XTRC(1), XBLC(2), BUFFER, IRET)
      IF (IRET.EQ.0) CALL GVEC (XTRC(1), XTRC(2), BUFFER, IRET)
      IF (IRET.EQ.0) CALL GVEC (XBLC(1), XTRC(2), BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GLTYPE/GVEC BORDER LINE'
         GO TO 980
         END IF
C                                       location common
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      CTYP(1,LOCNUM) = XTYPE
      CTYP(2,LOCNUM) = YTYPE // ' error'
      RPLOC(1,LOCNUM) = XBLC(1)
      RPLOC(2,LOCNUM) = XBLC(2)
      RPVAL(1,LOCNUM) = XRANGE(1)
      RPVAL(2,LOCNUM) = YRANGE(1)
      AXINC(1,LOCNUM) = (XRANGE(2) - XRANGE(1)) / (XTRC(1) - XBLC(1))
      AXINC(2,LOCNUM) = (YRANGE(2) - YRANGE(1)) / (XTRC(2) - XBLC(2))
      CALL CHNTIC (XBLC, XTRC, I)
      CH(1) = I + 4.0
      CH(2) = 3.333
      CH(3) = 0.5
      CH(4) = 0.5
      CALL CLAB1 (XBLC, XTRC, CH, 3, 1.0, .FALSE., BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET,  'CLAB1 LABELING AXES'
         GO TO 980
         END IF
C                                       plot input data
      TEXT = 'Plotting input data ' // YTYPE // ' error vs ' // XTYPE
      CALL GCOMNT (-1, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCOMNT'
         GO TO 980
         END IF
      CALL GLTYPE (IGR1, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GLTYPE'
         GO TO 980
         END IF
      DX = 2 * FACT1
      DY = 2 * FACT1 * XYRAT
      DO 20 I = 1,NOBS
         X = XBLC(1) + (XVAL(I) - XRANGE(1)) / AXINC(1,LOCNUM)
         Y = XBLC(2) + (Y1VAL(I) - YRANGE(1)) / AXINC(2,LOCNUM)
         AX(1) = X
         AY(1) = Y
         AX(2) = AX(1)
         AX(3) = AX(1)
         AX(4) = AX(1) - DX
         AX(5) = AX(1) + DX
         AY(2) = AY(1) + DY
         AY(3) = AY(1) - DY
         AY(4) = AY(1)
         AY(5) = AY(1)
         CALL PNTPLT (ISYM1, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *      BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'PNTPLT PLOTTING INPUT DATA'
            GO TO 980
            END IF
 20      CONTINUE
C                                       plot input data
      TEXT = 'Plotting post-fit data ' // YTYPE // ' error vs ' // XTYPE
      CALL GCOMNT (-1, TEXT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GCOMNT'
         GO TO 980
         END IF
      CALL GLTYPE (IGR2, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GLTYPE'
         GO TO 980
         END IF
      DX = 2 * FACT2
      DY = 2 * FACT2 * XYRAT
      DO 30 I = 1,NOBS
         X = XBLC(1) + (XVAL(I) - XRANGE(1)) / AXINC(1,LOCNUM)
         Y = XBLC(2) + (Y2VAL(I) - YRANGE(1)) / AXINC(2,LOCNUM)
         AX(1) = X
         AY(1) = Y
         AX(2) = AX(1)
         AX(3) = AX(1)
         AX(4) = AX(1) - DX
         AX(5) = AX(1) + DX
         AY(2) = AY(1) + DY
         AY(3) = AY(1) - DY
         AY(4) = AY(1)
         AY(5) = AY(1)
         CALL PNTPLT (ISYM2, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *      BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'PNTPLT PLOTTING POST-FIT DATA'
            GO TO 980
            END IF
 30      CONTINUE
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PLOT1 ERROR',I4,' FROM ',A)
      END
