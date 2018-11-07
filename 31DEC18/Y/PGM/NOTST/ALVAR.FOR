LOCAL INCLUDE 'ALVAR.INC'
C                                       Local include for ALVAR
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK, XFUNC
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XANT(50), XBASE(50), XDOCAL,
     *   XGUSE, XDOPOL,XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3),
     *   SOLINT, DOPLOT, XNPLOT, DOEBAR, FACTOR, SYMBOL, XLTYPE,
     *   PIXR(2), XDOTV, XGRCH, XYRATO, BADD(10),
     *   SCRBUF(256), BUFF(UVBFSS), TLAST, TINIT, PMIN(4,2),PMAX(4,2),
     *   XMIN, XMAX
      INTEGER   SEQIN, DISKIN, JBUFSZ, ILOCWT, CATOLD(256), INCSI,
     *   INCFI, INCIFI, NRPRMI, OLDCNO, NSTK, NIF, NFREQ, NBASL, CTIME,
     *   NTIMES, NANT, NBAS, IANT(50), IBAS(50), MTIMES, NPLOTS, LTYPE,
     *   BLPTR(MAXANT,MAXANT), GRCHAN, NPARMS
      LOGICAL   DESEL, DOTV, DOLOG(2)
      CHARACTER NAMEIN*12, CLAIN*6, FUNCTY*2
      DOUBLE PRECISION AVRAGE(4,2,1000)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XANT, XBASE, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XDOBND, XBPVER, XSMOTH, SOLINT, DOPLOT, XNPLOT, XFUNC,
     *   DOEBAR, FACTOR, SYMBOL, XLTYPE, PIXR, XDOTV, XGRCH, XYRATO,
     *   BADD
      COMMON /ALVARM/ CATOLD, AVRAGE, SEQIN, DISKIN, ILOCWT, INCSI,
     *   INCFI, INCIFI, NRPRMI, OLDCNO, NSTK, NIF, NFREQ, NBASL, TLAST,
     *   CTIME, NTIMES, NANT, NBAS, IANT, IBAS, DESEL, MTIMES, BLPTR,
     *   TINIT, NPLOTS, LTYPE, DOTV, GRCHAN, PMIN, PMAX, XMIN, XMAX,
     *   DOLOG, NPARMS
      COMMON /CHARPM/ NAMEIN, CLAIN, FUNCTY
      COMMON /BUFRS/ SCRBUF, BUFF, JBUFSZ
C                                       End local include for ALVAR
LOCAL END
      PROGRAM ALVAR
C-----------------------------------------------------------------------
C! Plot the Allan-variance of a UV data set
C# Utility UV Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2015-2016, 2018
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
C   ALVAR grids the data at SOLINT second intervals and computes the
C   Allan variance of the data, ploting that variance at the end.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      SOLINT         SOLINT        pre-average time in Sec (-> days)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NWORDS, NTAU
      REAL      SUMS(2), VALS(2), ANSW(2), WORK(2)
C                                       for debug
C      REAL      SUMS(100), VALS(2400), ANSW(10000), WORK(1000)
      LONGINT   PSUMS, PVALS, PANSW, PWORK
      INCLUDE 'ALVAR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'ALVAR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL ALVARI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       count times
      MSGTXT = 'Count the number of SOLINTs in the data'
      CALL MSGWRT (2)
      NTIMES = 0
      CALL ALVARC (IRET)
      IF (IRET.NE.0) GO TO 999
      NTAU = MTIMES / 3
      MTIMES = MTIMES
C                                       get memory
      NWORDS = (3 * NBASL - 1) / 1024 + 10
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SUMS, PSUMS, IRET)
      NWORDS = (3 * NBASL * MTIMES - 1) / 1024 + 10
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VALS, PVALS,
     *   IRET)
      NWORDS = (4 * MTIMES - 1) / 1024 + 10
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, WORK, PWORK,
     *   IRET)
      NWORDS = (8 * NTAU * NBASL - 1) / 1024 + 10
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ANSW, PANSW,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET NEEDED MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Sum up data, make big array
      MSGTXT = 'Read the data into big arrays'
      CALL MSGWRT (2)
      CALL ALVARU (NBASL, SUMS(1+PSUMS), VALS(1+PVALS), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Compute on big array
      MSGTXT = 'Now find Allan variance functions'
      CALL MSGWRT (2)
      CALL ALVARK (NTAU, NBASL, VALS(1+PVALS), WORK(1+PWORK),
     *   ANSW(1+PANSW), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Now do plots
      MSGTXT = 'Now plot Allan variance functions'
      CALL MSGWRT (2)
      CALL ALVARP (NTAU, NBASL, ANSW(1+PANSW), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE ALVARI (PRGN, JERR)
C-----------------------------------------------------------------------
C   ALVARI gets input parameters for ALVAR
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in ALVAR for more details.
CC-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2, STOKV(12)*4
      INTEGER   IROUND, IERR, INCX, I, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'ALVAR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA STOKV /'I','Q','U','V', 'RR','LL','RL','LR',
     *   'XX','YY','XY','YX'/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARMS = 280
      CALL GTPARM (PRGN, NPARMS, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      CALL H2CHR (2, 1, XFUNC, FUNCTY)
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      I = IROUND (DOPLOT)
      IF ((I.LE.0) .OR. (I.GT.4)) DOPLOT = 3.0
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       limit to 1 Stokes
      DO 20 I = 1,12
         IF (STOKES.EQ.STOKV(I)) GO TO 25
 20      CONTINUE
C                                       default Stokes
      IF (ICOR0.GT.0) THEN
         STOKES = STOKV(ICOR0)
      ELSE
         STOKES = STOKV(4-ICOR0)
         END IF
      MSGTXT = 'SET STOKES VALUE TO ' // STOKES
      CALL MSGWRT (6)
C                                       Channel selection?
 25   IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       fill in defaults
      CALL RCOPY (8, TIMRNG, XTIME)
      XFLAG = FGVER
      XSUBA = SUBARR
      XBIF = BIF
      XEIF = EIF
      XBCHAN = BCHAN
      XECHAN = ECHAN
      XFQID = FRQSEL
      CALL CHR2H (4, STOKES, 1, XXSTOK)
C                                       Save input file info
      NSTK = 1
      NFREQ = 1
      NIF = 1
      IF (JLOCS.GE.0) NSTK = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NFREQ = CATBLK(KINAX+JLOCF)
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Initialize baseline selection.
      CALL SETANT (50, XANT, XBASE, NANT, NBAS, IANT, IBAS, DESEL)
C                                       get antenna info
      CALL COPY (256, CATOLD, CATBLK)
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, SCRBUF, IERR)
      IF (SOLINT.LE.0.0) SOLINT = 9.99
      SOLINT = SOLINT / (24.0 * 3600.0)
      JERR = 0
      NPLOTS = IROUND (XNPLOT)
      NPLOTS = MAX (-1, NPLOTS)
      IF (NPLOTS.EQ.0) NPLOTS = 2
      XNPLOT = NPLOTS
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVARI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE ALVARC (IRET)
C-----------------------------------------------------------------------
C   ALVARC counts time intervals
C   Output:
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'ALVAR.INC'
      INTEGER   CATMP(256), I, J
      LOGICAL   REQBAS
      REAL      T, VIS(UVBFSS), RPARM(20), TFIN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       buffer pointers
      TLAST = -1.E6
      CTIME = 0
      TINIT = 99999.
      TFIN = -100.0
      NBASL = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, BLPTR)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
C                                       Do we need this baseline?
         IF (ILOCB.GE.0) THEN
            I = INT (RPARM(ILOCB+1)) / 256
            J = MOD (INT (RPARM(ILOCB+1)), 256)
         ELSE
            I = RPARM(ILOCA1+1) + 0.1
            J = RPARM(ILOCA2+1) + 0.1
            END IF
         IF (REQBAS (I, J, DESEL, IANT, NANT, IBAS, NBAS)) THEN
            IF (BLPTR(J,I).LE.0) THEN
               NBASL = NBASL + 1
               BLPTR(I,J) = NBASL
               END IF
            BLPTR(J,I) = BLPTR(J,I) + 1
            T = RPARM(1+ILOCT)
            TINIT = MIN (T, TINIT)
            TFIN = MAX (T, TFIN)
            IF (T.GT.TLAST) THEN
               IF (TLAST.GT.0.0) NTIMES = NTIMES + 1
               TLAST = T + SOLINT
               END IF
            END IF
         GO TO 100
         END IF
      MTIMES = (TFIN - TINIT) / SOLINT + 1.01
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVARC: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1100 FORMAT ('ALVARC: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE ALVARU (NB, SUMS, VALS, IRET)
C-----------------------------------------------------------------------
C   ALVARU sends uv data one point at a time to a routine that grids
C   the data, averaging over channel and IF
C   Input
C      NB     I      Number baselines
C   Output:
C      SUMS   R(*)   Summing variables
C      VALS   R(*)   Time sequence of real and imaginary averages
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NB, IRET
      REAL      SUMS(3,*), VALS(3,NB,*)
C
      INCLUDE 'ALVAR.INC'
      INTEGER   IA1, IA2, NUMVIS, CATMP(256), I
      REAL      DUM, BASEN, VIS(UVBFSS), RPARM(20)
      LOGICAL REQBAS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
      I = 3 * NB * MTIMES
      CALL RFILL (I, FBLANK, VALS)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       buffer pointers
      TLAST = -1.E6
      CTIME = 0
      NUMVIS = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         IF (REQBAS (IA1, IA2, DESEL, IANT, NANT, IBAS, NBAS)) THEN
            NUMVIS = NUMVIS + 1
C                                       call user routine
            CALL ALVARR (NUMVIS, RPARM(1+ILOCT), IA1, IA2, VIS, NIF, NB,
     *         SUMS, VALS)
            END IF
         GO TO 100
         END IF
C                                       last average
      NUMVIS = -1
      CALL ALVARR (NUMVIS, DUM, IA1, IA2, SCRBUF, NIF, NB, SUMS, VALS)
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVARU: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1100 FORMAT ('ALVARU: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE ALVARR (NUMVIS, T, IA1, IA2, VIS, NI, NB, SUMS, VALS)
C-----------------------------------------------------------------------
C   ALVARR averages the data into the SOLINT bins
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C      NS     I      Number stokes
C      NI     I      Number IFs
C      NB     I      Number baselines
C   In/Output:
C      SUMS   R(*)   Summing variables
C      VALS   R(*)   Time sequence of real and imaginary averages
C   Inputs from COMMON:
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C   Output:
C      T          R    Time in same units as input.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, NI, NB, IRET
      REAL      T, VIS(3,*), SUMS(3,*), VALS(3,NB,*)
C
      INTEGER   JI, JF, JB, INDEXI, INDI, IT
      REAL      VR, VI, VW, VA, TT
      INCLUDE 'ALVAR.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LT.0) T = 1.E6
      IT = (T - TINIT) / SOLINT + 1.00001
C                                       new integration
      IF (IT.NE.CTIME) THEN
C                                       average this lot, normalize
         IF (CTIME.GT.0) THEN
            DO 40 JB = 1,NB
               VW = SUMS(3,JB)
               IF ((VW.GT.0.0) .AND. (VW.NE.FBLANK)) THEN
                  VA = SQRT (SUMS(1,JB)*SUMS(1,JB) +
     *               SUMS(2,JB)*SUMS(2,JB))
                  IF (VA.LE.0.0) VA = 1.0
                  VALS(1,JB,CTIME) = SUMS(1,JB)/VA
                  VALS(2,JB,CTIME) = SUMS(2,JB)/VA
                  VALS(3,JB,CTIME) = VW
               ELSE
                  VALS(1,JB,CTIME) = FBLANK
                  VALS(2,JB,CTIME) = FBLANK
                  VALS(3,JB,CTIME) = FBLANK
                  END IF
 40            CONTINUE
            END IF
         JF = 3 * NB
         CALL RFILL (JF, 0.0, SUMS)
         TLAST = T + SOLINT
         CTIME = IT
         END IF
C                                       time average
      IF (NUMVIS.GT.0) THEN
         JB = BLPTR(IA1,IA2)
         VR = 0.0
         VI = 0.0
         VW = 0.0
C                                       average over IF
         DO 130 JI = 1,NI
            INDI = (JI-1) * INCIFI + 1
            INDEXI = INDI
C                                       average over frequency
            DO 110 JF = 1,NFREQ
               TT = VIS(3,INDEXI)
               IF ((TT.GT.0.0) .AND. (TT.NE.FBLANK)) THEN
                  VR = VR + VIS(1,INDEXI) * TT
                  VI = VI + VIS(2,INDEXI) * TT
                  VW = VW + TT
                  END IF
               INDEXI = INDEXI + INCFI
 110           CONTINUE
 130        CONTINUE
         SUMS(1,JB) = SUMS(1,JB) + VR
         SUMS(2,JB) = SUMS(2,JB) + VI
         SUMS(3,JB) = SUMS(3,JB) + VW
         END IF
C
 999  RETURN
      END
      SUBROUTINE ALVARK (NTAU, NB, VALS, WORK, ANSW, IRET)
C-----------------------------------------------------------------------
C   ALVARK computes various Allan Variance functions from large data buf
C   Input
C      NB     I      Number baselines
C      VALS   R(*)   Time sequence of real and imaginary averages
C   Output:
C      ANSW   R(*)   Allan variance functions (4,NTau,NB)
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NTAU, NB, IRET
      REAL      VALS(3,NB,*), WORK(4,*), ANSW(4,2,NTAU,*)
C
      INTEGER   NITER
      PARAMETER (NITER = 8)
C
      INCLUDE 'ALVAR.INC'
      INTEGER   IA1, IA2, JB, JT, JP, IT, J, KT, L, I
      REAL      SR, SI, WS(NITER), VP, VM, P1, P2,
     *   P3, TT(3,3), WT
      DOUBLE PRECISION S1, S2, Q1, Q2, NSQ, SV, SSV, NV, AVRWTS(4,1000),
     *   RAW(2,2), ROB(2,2)
      LOGICAL   OKAY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
      NTAU = MTIMES / 3
      I = 8 * MIN (NTAU, 1000)
      CALL DFILL (I, 0.0D0, AVRAGE)
      I = I / 2
      CALL DFILL (I, 0.0D0, AVRWTS)
C                                       over baselines
      DO 200 IA1 = 1,NSTNS-1
         DO 190 IA2 = IA1+1,NSTNS
            JB = BLPTR(IA1,IA2)
            IF ((JB.GT.0) .AND. (BLPTR(IA2,IA1).GT.0)) THEN
C                                       loop over integration time
               DO 180 IT = 1,NTAU
C                                       do brute rms/mean
                  S1 = 0.0D0
                  S2 = 0.0D0
                  Q1 = 0.0D0
                  Q2 = 0.0D0
                  NSQ = 0.0D0
                  DO 110 JT = 1,MTIMES-3*IT+1,IT
                     CALL RFILL (9, 0.0, TT)
                     DO 20 KT = JT,JT+IT-1
                        L = KT
                        DO 10 J = 1,3
                           IF ((VALS(3,JB,L).GT.0.0) .AND.
     *                        (VALS(3,JB,L).NE.FBLANK))  THEN
                              TT(1,J) = TT(1,J) +
     *                           VALS(3,JB,L)*VALS(1,JB,L)
                              TT(2,J) = TT(2,J) +
     *                           VALS(3,JB,L)*VALS(2,JB,L)
                              TT(3,J) = TT(3,J) + VALS(3,JB,L)
                              END IF
                           L = L + IT
 10                        CONTINUE
 20                     CONTINUE
                     OKAY = .TRUE.
                     DO 25 J = 1,3
                        IF (TT(3,J).GT.0.0) THEN
                           TT(1,J) = TT(1,J) / TT(3,J)
                           TT(2,J) = TT(2,J) / TT(3,J)
                        ELSE
                           OKAY = .FALSE.
                           END IF
 25                     CONTINUE
                     IF (OKAY) THEN
                        SR = TT(1,1) + TT(1,3) - 2 * TT(1,2)
                        SI = TT(2,1) + TT(2,3) - 2 * TT(2,2)
                        WORK(1,JT) = SR * SR + SI * SI
                        P1 = ATAN2 (TT(2,1), TT(1,1))
                        P2 = ATAN2 (TT(2,2), TT(1,2))
                        P3 = ATAN2 (TT(2,3), TT(1,3))
                        CALL PHMINI (P1, P2, P3, WORK(2,JT))
                        Q1 = Q1 + WORK(1,JT)
                        S1 = S1 + WORK(1,JT) ** 2
                        Q2 = Q2 + WORK(2,JT)
                        S2 = S2 + WORK(2,JT) ** 2
                        NSQ = NSQ + 1.0D0
                     ELSE
                        WORK(1,JT) = FBLANK
                        WORK(2,JT) = FBLANK
                        END IF
 110                 CONTINUE
                  IF (NSQ.GT.0.0D0) THEN
                     RAW(1,1) = Q1 / NSQ
                     RAW(1,2) = Q2 / NSQ
                     RAW(2,1) = S1 / NSQ - RAW(1,1)*RAW(1,1)
                     RAW(2,2) = S2 / NSQ - RAW(1,2)*RAW(1,2)
                     RAW(2,1) = SQRT (MAX (0.0D0, RAW(2,1)))
                     RAW(2,2) = SQRT (MAX (0.0D0, RAW(2,2)))
C                                       loop over 2 parameters
                     DO 140 JP = 1,2
                        VP = RAW(1,JP) + WS(1) * RAW(2,JP)
                        VM = RAW(1,JP) - WS(1) * RAW(2,JP)
                        DO 130 KT = 1,NITER
                           SV = 0.0D0
                           SSV = 0.0D0
                           NV = 0.0D0
                           DO 120 JT = 1,MTIMES-3*IT+1,IT
                              IF (WORK(JP,JT).NE.FBLANK) THEN
                                 IF ((WORK(JP,JT).GT.VM) .AND.
     *                              (WORK(JP,JT).LT.VP)) THEN
                                    SV = SV + WORK(JP,JT)
                                    SSV = SSV + WORK(JP,JT)**2
                                    NV = NV + 1.0D0
                                    END IF
                                 END IF
 120                          CONTINUE
                           IF (NV.GT.0.0D0) THEN
                              SV = SV / NV
                              SSV = SSV / NV - SV * SV
                              SSV = SQRT (MAX (0.0D0, SSV))
                              IF (KT.LT.NITER) THEN
                                 VP = SV + WS(KT+1) * SSV
                                 VM = SV - WS(KT+1) * SSV
                                 END IF
                           ELSE
                              VP = 1.E4
                              VM = -1.E4
                              END IF
 130                       CONTINUE
                        ROB(1,JP) = SV
                        ROB(2,JP) = SSV
 140                    CONTINUE
C                                       send answers to plot package
                     ANSW(1,1,IT,JB) = RAW(1,1) / IT
                     ANSW(2,1,IT,JB) = RAW(1,2) / IT
                     ANSW(1,2,IT,JB) = RAW(2,1) / IT
                     ANSW(2,2,IT,JB) = RAW(2,2) / IT
                     ANSW(3,1,IT,JB) = ROB(1,1) / IT
                     ANSW(4,1,IT,JB) = ROB(1,2) / IT
                     ANSW(3,2,IT,JB) = ROB(2,1) / IT
                     ANSW(4,2,IT,JB) = ROB(2,2) / IT
                     IF (NPLOTS.LT.0) THEN
                        DO 150 J = 1,4
                           IF ((ANSW(J,2,IT,JB).NE.FBLANK) .AND.
     *                        (ANSW(J,1,IT,JB).NE.FBLANK)) THEN
                              WT = 0.0
                              IF (ANSW(J,2,IT,JB).GT.0.0) WT = 1.0 /
     *                           (ANSW(J,2,IT,JB)**2)
                              AVRAGE(J,1,IT) = AVRAGE(J,1,IT) +
     *                           WT * ANSW(J,1,IT,JB)
                              AVRAGE(J,2,IT) = AVRAGE(J,2,IT) +
     *                           WT * ANSW(J,1,IT,JB) * ANSW(J,1,IT,JB)
                              AVRWTS(J,IT) = AVRWTS(J,IT) + WT
                              END IF
 150                       CONTINUE
                        END IF
                  ELSE
                     CALL RFILL (8, FBLANK, ANSW(1,1,IT,JB))
                     END IF
 180              CONTINUE
               END IF
 190        CONTINUE
 200     CONTINUE
C                                       average
      IF (NPLOTS.LT.0) THEN
         DO 220 JT = 1,NTAU
            DO 210 J = 1,4
               WT = AVRWTS(J,JT)
               IF (WT.GT.0.0) THEN
                  AVRAGE(J,1,JT) = AVRAGE(J,1,JT) / WT
                  AVRAGE(J,2,JT) = AVRAGE(J,2,JT) / WT -
     *               AVRAGE(J,1,JT) * AVRAGE(J,1,JT)
                  AVRAGE(J,2,JT) = SQRT (MAX (0.0D0, AVRAGE(J,2,JT)))
               ELSE
                  AVRAGE(J,1,JT) = FBLANK
                  AVRAGE(J,2,JT) = FBLANK
                  END IF
 210           CONTINUE
 220        CONTINUE
         END IF

C
 999  RETURN
      END
      SUBROUTINE PHMINI (P1, P2, P3, PDMIN)
C-----------------------------------------------------------------------
C   Find minimum (P3 - 2*P2 + P1)**2
C   Inputs:
C      P1      R   Phase at data point
C      P2      R   Phase at data point+1
C      P3      R   Phase at data point+2
C   Outputs
C      PDMIN   R   Minimum Allan variance
C-----------------------------------------------------------------------
      REAL      P1, P2, P3, PDMIN
C
      INTEGER   I, J
      REAL      P
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      PDMIN = 1.E10
      DO 20 I = 1,5
         DO 10 J = 1,3
            P = ((P3 + (I-3)*TWOPI) - 2.0 * (P2 + (J-2)*TWOPI) + P1) **2
            PDMIN = MIN (P, PDMIN)
 10         CONTINUE
 20      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ALVARP (NTAU, NB, ANSW, IRET)
C-----------------------------------------------------------------------
C   ALVARP plots various Allan Variance functions from answers
C   Input
C      NB     I      Number baselines
C      ANSW   R(*)   Allan variance functions (4,2,NTau,NB)
C   Output:
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NTAU, NB, IRET
      REAL      ANSW(4,2,NTAU,*)
C
      INCLUDE 'ALVAR.INC'
      INTEGER   JT, JB, J, NPL, IA1, IA2, IPLOT, JPL, IROUND, I,
     *   SCRTCH(256)
      CHARACTER CHTM12*12, CHTM6*6, CTEMP*2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Plot adverbs
      LTYPE = IROUND (XLTYPE)
      IF (LTYPE.EQ.0) LTYPE = 3
      XLTYPE = LTYPE
      DOTV = XDOTV.GT.0.0
      GRCHAN = XGRCH + 0.01
C                                       status
      IF (.NOT.DOTV) THEN
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CHTM12, CHTM6, 0, CTEMP,
     *      0, 'CLRD', SCRTCH, IRET)
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, CHTM12, CHTM6, 0, CTEMP,
     *      0, 'WRIT', SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RESETTING HEADER STATUS'
            CALL MSGWRT (7)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
      IF (DOTV) FRW(NCFILE) = 0
C                                       plot range
      CALL RFILL (8, 1.E8, PMIN)
      CALL RFILL (8, 0.0, PMAX)
      NPL = 0
      IF (NPLOTS.GT.0) THEN
         DO 20 JB = 1,NB
            IF (ANSW(1,1,1,JB).NE.FBLANK) NPL = NPL + 1
            DO 15 JT = 1,NTAU
               DO 10 J = 1,4
                  IF (ANSW(J,1,JT,JB).NE.FBLANK) THEN
                     PMIN(J,1) = MIN (PMIN(J,1), ANSW(J,1,JT,JB))
                     PMAX(J,1) = MAX (PMAX(J,1), ANSW(J,1,JT,JB))
                     PMIN(J,2) = MIN (PMIN(J,2),
     *                  ANSW(J,1,JT,JB)-ANSW(J,2,JT,JB))
                     PMAX(J,2) = MAX (PMAX(J,2),
     *                  ANSW(J,1,JT,JB)+ANSW(J,2,JT,JB))
                     END IF
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
      ELSE
         IF (AVRAGE(1,1,1).NE.FBLANK) NPL = NPL + 1
         DO 30 JT = 1,NTAU
            DO 25 J = 1,4
               IF (AVRAGE(J,1,JT).NE.FBLANK) THEN
                  IF (PMIN(J,1).GT.AVRAGE(J,1,JT)) PMIN(J,1) =
     *               AVRAGE(J,1,JT)
                  IF (PMAX(J,1).LT.AVRAGE(J,1,JT)) PMAX(J,1) =
     *               AVRAGE(J,1,JT)
                  IF (PMIN(J,2).GT.AVRAGE(J,1,JT)-AVRAGE(J,2,JT))
     *               PMIN(J,2) = AVRAGE(J,1,JT) - AVRAGE(J,2,JT)
                  IF (PMAX(J,2).LT.AVRAGE(J,1,JT)+AVRAGE(J,2,JT))
     *               PMAX(J,2) = AVRAGE(J,1,JT) + AVRAGE(J,2,JT)
                  END IF
 25            CONTINUE
 30         CONTINUE
         END IF
      XMIN = 1
      XMAX = NTAU
C                                       log plot
      DOLOG(1) = FUNCTY.EQ.'L2'
      DOLOG(2) = DOLOG(1) .OR. FUNCTY.EQ.'LG'
      IF (DOLOG(2)) THEN
         DO 50 J = 1,4
            DO 45 I = 1,2
               PMIN(J,I) = LOG10 (MAX (1.E-10, PMIN(J,I)))
               PMAX(J,I) = LOG10 (PMAX(J,I))
 45            CONTINUE
 50         CONTINUE
         END IF
      IF (DOLOG(1)) THEN
         XMIN = 0.0
         XMAX = LOG10 (XMAX)
         END IF
C                                       loop to do plots
      IPLOT = 0
      JPL = 0
      IF (NPLOTS.GT.0) THEN
         DO 70 IA1 = 1,NSTNS-1
            DO 60 IA2 = IA1+1,NSTNS
               JB = BLPTR(IA1,IA2)
               IF ((JB.GT.0) .AND. (BLPTR(IA2,IA1).GT.0) .AND.
     *            (ANSW(1,1,1,JB).NE.FBLANK)) THEN
                  IPLOT = IPLOT + 1
                  JPL = MOD (JPL, NPLOTS) + 1
                  IF (IPLOT.GE.NPL) JPL = -JPL
                  CALL PLOTAV (JPL, IA1, IA2, JB, NTAU, ANSW, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 60            CONTINUE
 70        CONTINUE
      ELSE
         JPL = -1
C                                       move to real*4
         DO 80 IA2 = 1,NTAU
            DO 75 IA1 = 1,4
               ANSW(IA1,1,IA2,1) = AVRAGE(IA1,1,IA2)
               ANSW(IA1,2,IA2,1) = AVRAGE(IA1,2,IA2)
 75            CONTINUE
 80         CONTINUE
         CALL PLOTAV (JPL, 0, 0, 1, NTAU, ANSW, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AVALRP ERROR',I4,' ON ',A)
      END
      SUBROUTINE PLOTAV (IPLOT, IA1, IA2, JB, NTAU, ANSW, IRET)
C-----------------------------------------------------------------------
C   Plot a panel for ALVAR
C   Inputs:
C      IPLOT   I      Panel number on current page: < 0 -> last one
C      IA1     I      Lower ant number in baseline
C      IA2     I      Higher ant number in baseline
C      JB      I      Pointer into data array for baseline
C      NTAU    I      Number of averaging value
C      ANSW    R(*)   data to plot, 4 type, value/error, times,
C                     baselines
C   Outputs:
C      IRET    I      Error code: 0 none, -1 quit, > 0 real error
C-----------------------------------------------------------------------
      INTEGER   IPLOT, IA1, IA2, JB, NTAU, IRET
      REAL      ANSW(4,2,NTAU,*)
C
      INCLUDE 'ALVAR.INC'
      INTEGER   BUFFER(256), LUNPL, DEPTH(5), NGOOD, NNOFIT, IERR, VER,
     *   IPSIZE, ITYPE, TVCHN, TVCORN(2), FINDPL, PTYP, INP, ISYM,
     *   IROUND, I, IAPLOT, IAXLAB, ID(3), LABEL, IT(3), ILITY, INCHAR,
     *   JTRIM, JT, JTYP
      LOGICAL   CATUP, GOOD, UP
      REAL      BLC(2), TRC(2), XBLC(2), XTRC(2), YYOFF(2), XMULT(2),
     *   SIZE, TR, TI, PLMAX(2), PLMIN(2), CHOUT(4), DX, DY, AX(5),
     *   AY(5), XY(2), XYOFF(2), XYSCL(2), PLTINC
      CHARACTER TEXT*132, PFILE*48, ATIME*8, ADATE*12, CHTMP*18,
     *   PLTYPE(4)*16
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      SAVE BUFFER, CHOUT
      DATA TVCORN /2*0/
      DATA DEPTH /5*1/
      DATA PLTYPE /'Mean complex', 'Mean phase', 'Robust complex',
     *   'Robust phase'/
C-----------------------------------------------------------------------
      PTYP = IROUND (DOPLOT)
      JTYP = 1
      IF (DOEBAR.GT.0.0) JTYP = 2
C                                       pixrange enforced
      IF (PIXR(2).GT.PIXR(1)) THEN
         PMAX(PTYP,JTYP) = PIXR(2)
         PMIN(PTYP,JTYP) = PIXR(1)
         END IF
      NGOOD = 0
      NNOFIT = 0
      IRET = 3
      CATUP = .TRUE.
      IF (FACTOR.LE.0.001) FACTOR = 1.0
      IF ((SYMBOL.LE.0.0) .OR. (SYMBOL.GT.24.0)) SYMBOL = 3.0
      ISYM = SYMBOL + 0.5
      TVCHN = 0
      GRCHAN = IROUND (XGRCH)
      LABEL = IROUND (XLTYPE)
      LTYPE = MOD (ABS(LABEL), 100)
      IF ((LTYPE.EQ.0) .OR. (LTYPE.GT.10)) LTYPE = 3
      IF (LTYPE.GT.7) LTYPE = 7
      IF ((LTYPE.GE.4) .AND. (LTYPE.LE.6)) LTYPE = 3
      IF (LABEL.LT.0) THEN
         LABEL = (LABEL/100)*100 - LTYPE
      ELSE
         LABEL = (LABEL/100)*100 + LTYPE
         END IF
      XLTYPE = LABEL
C
C                                       Create plot file
      IF (ABS (IPLOT).EQ.1) THEN
C                                       Update catalog header.
         VER = 0
         IRET = 1
         IF (.NOT.DOTV) THEN
            CALL MADDEX ('PL', DISKIN, OLDCNO, CATBLK, BUFFER, CATUP,
     *         'WRIT', VER, IERR)
            IF (IERR.NE.0) THEN
               NCFILE = NCFILE - 1
               GO TO 999
               END IF
            END IF
         CALL ZPHFIL ('PL', DISKIN, OLDCNO, VER, PFILE, IERR)
         IF (IERR.NE.0) GO TO 960
         IPSIZE = 0
         ITYPE = 49
         SOLINT = SOLINT * 24.0 * 3600.0
         CALL GINIT (DISKIN, OLDCNO, PFILE, IPSIZE, ITYPE, NPARMS,
     *      XNAMEI, DOTV, TVCHN, GRCHAN, TVCORN, CATBLK, BUFFER, LUNPL,
     *      FINDPL, IERR)
         IRET = 2
         IF (IERR.NE.0) GO TO 960
         SOLINT = SOLINT / 24.0 / 3600.0
         END IF
C                                       Graph drawing parameters.
      BLC(1) = 0.0
      BLC(2) = 0.0
      IF (DOTV) THEN
         TRC(1) = WINDTV(3) - WINDTV(1)
         TRC(2) = WINDTV(4) - WINDTV(2)
      ELSE
         TRC(1) = 1000.0
         TRC(2) = 1000.0
         END IF
      IF (XYRATO.LE.0.0) XYRATO = TRC(1) / TRC(2)
C                                       Set window for current plot.
      PLTINC = TRC(2) / ABS(NPLOTS)
      XBLC(1) = BLC(1)
      XBLC(2) = TRC(2) - ABS (IPLOT) * PLTINC
      XTRC(1) = TRC(1)
      XTRC(2) = XBLC(2) + PLTINC - 1.0
C                                       Offsets for current plot.
      YYOFF(1) = XBLC(1)
      YYOFF(2) = XBLC(2)
C                                       plot range
      TR = 0.1 * (XMAX - XMIN)
      PLMAX(1) = XMAX + TR
      PLMIN(1) = XMIN - TR
      TR = 0.1 * (PMAX(PTYP,JTYP) - PMIN(PTYP,JTYP))
      PLMAX(2) = PMAX(PTYP,JTYP) + TR
      PLMIN(2) = PMIN(PTYP,JTYP) - TR
C                                       fool with location common
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      TR = 1.2 * (PMAX(PTYP,JTYP)-PMIN(PTYP,JTYP))
      IF (TR.LE.0.0) TR = 1.0
      TI = TR
      CALL METSCL (LABEL, TR, CPREF(2,LOCNUM), GOOD)
      XMULT(2) = TR / TI
      CPREF(1,LOCNUM) = ' '
      XMULT(1) = 1.0
      DO 20 I = 1,2
         SIZE = XTRC(I) - XBLC(I) + 1
         TR = PLMAX(I) - PLMIN(I)
         XYOFF(I) = PLMIN(I)
         XYSCL(I) = (XTRC(I) - XBLC(I)) / TR
         RPLOC(I,LOCNUM) = XBLC(I)
         RPVAL(I,LOCNUM) = XYOFF(I) * XMULT(I)
         AXINC(I,LOCNUM) = TR * XMULT(I) / (XTRC(I) - XBLC(I))
 20      CONTINUE
      IF (DOLOG(1)) THEN
         CTYP(1,LOCNUM) = 'LOG(INT)'
      ELSE
         CTYP(1,LOCNUM) = 'INTERVAL'
         END IF
      IF (DOLOG(2)) THEN
         CTYP(2,LOCNUM) = 'LOG (AV)'
      ELSE
         CTYP(2,LOCNUM) = 'AL VAR'
         END IF
C                                       Init plot calls again
C                                       Number of characters on each
C                                       side of the plot
      IF (ABS (IPLOT).EQ.1) THEN
         CALL RFILL (4, 0.5, CHOUT)
C                                       Not fully initialized, may make
C                                       INP too large which is okay.
         CALL CHNTIC (XBLC, XTRC, INP)
         INP = MAX (INP, 3)
         IF (LTYPE.EQ.2) CHOUT(1) = 2.5
         IF (LTYPE.GT.2) CHOUT(1) = INP + 4
         IF (LTYPE.GT.1) CHOUT(2) = 2.0
         IF (LTYPE.GT.2) CHOUT(2) = CHOUT(2) + 1.333
         IF ((LTYPE.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = 3.333
         IF ((LABEL.GT.1) .AND. (LTYPE.LT.7)) CHOUT(4) = CHOUT(4) +
     *      1.333
C                                       Init for line drawing.
         CALL GINITL (BLC, TRC, XYRATO, CHOUT, DEPTH, BUFFER, IERR)
         IRET = 3
         IF (IERR.NE.0) GO TO 970
         IF (.NOT.DOTV) THEN
            WRITE (MSGTXT,1000) VER
            CALL MSGWRT (2)
            END IF
         END IF
      IRET = 3
      CATUP = .TRUE.
C                                       Draw border
      CALL GLTYPE (1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XBLC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XTRC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Top labels: type & name
      IF ((ABS(IPLOT).EQ.1) .AND. (LTYPE.GT.1) .AND. (LTYPE.LT.7)) THEN
         DX = 0.0
         DY = 0.5
C                                       The first line header
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       File name
         CALL H2CHR (18, KHIMNO, CATH(KHIMN), CHTMP)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CHTMP(13:18))
         CALL NAMEST (CHTMP, CATBLK(KIIMS), TEXT, INCHAR)
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       second line
         DY = DY + 1.333
         CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
         TEXT = PLTYPE(PTYP)
         INCHAR = JTRIM (TEXT) + 3
         TR = SOLINT * 24.0 * 3600.0
         WRITE (TEXT(INCHAR:),1020) TR
         INP = JTRIM (TEXT) + 2
         TEXT(INP:) = 'sec'
         CALL REFRMT (TEXT, ' ', INCHAR)
         CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 970
C                                       Date/time/version
         IF (LABEL.GT.1) THEN
            DY = DY + 1.333
            CALL GPOS (BLC(1), TRC(2), BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            CALL ZDATE (ID)
            CALL ZTIME (IT)
            CALL TIMDAT (IT, ID, ATIME, ADATE)
            WRITE (TEXT,1030) VER, ADATE, ATIME
            CALL REFRMT (TEXT, '_', INCHAR)
            CALL GCHAR (INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 970
            END IF
         END IF
C                                       baseline
      CALL GPOS (XBLC(1), XTRC(2), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      IF (NPLOTS.GT.1) THEN
         DX =  3.5
         DY = -3.5
      ELSE
         DX =  4.5
         DY = -4.5
         END IF
      WRITE (TEXT,1040) IA1, IA2
      IF ((IA1.LE.0) .AND. (IA2.LE.0)) TEXT = 'average'
      CALL CHTRIM (TEXT, 132, TEXT, INCHAR)
      CALL REFRMT (TEXT, '_', INCHAR)
      CALL GICHAR (1, INCHAR, 0, DX, DY, TEXT, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Set up location common
C                                       Blank bottom label.
      IF ((IPLOT.GE.0) .AND. (ABS (IPLOT).NE.ABS(NPLOTS))) THEN
         CPREF(1,LOCNUM) = ' '
         CTYP(1,LOCNUM) = ' '
         END IF
C                                       Only label Y axis once.
      IAXLAB = ABS(NPLOTS) / 2 + 1
      IAPLOT = ABS (IPLOT)
      IF ((IAPLOT.NE.IAXLAB) .AND. ((IPLOT.GE.0) .OR.
     *   (IAPLOT.GT.IAXLAB))) CPREF(2,LOCNUM) = '-1'
C                                       Put on labels and ticks
      CALL CLAB1 (XBLC, XTRC, CHOUT, LABEL, XYRATO, .FALSE., BUFFER,
     *   IERR)
      IF (IERR.NE.0) GO TO 970
      ILITY = 2
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       connected lines
      UP = .TRUE.
      DO 30 JT = 1,NTAU
         IF (ANSW(PTYP,1,JT,JB).EQ.FBLANK) THEN
            UP = .TRUE.
         ELSE
            XY(1) = JT
            IF (DOLOG(1)) XY(1) = LOG10 (XY(1))
            XY(2) = ANSW(PTYP,1,JT,JB)
            IF (DOLOG(2)) XY(2) = LOG10 (XY(2))
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(1).LT.XBLC(1)) .OR. (XY(1).GT.XTRC(1)) .OR.
     *         (XY(2).LT.XBLC(2)) .OR. (XY(2).GT.XTRC(2))) THEN
               NNOFIT = NNOFIT + 1
               UP = .TRUE.
            ELSE
               NGOOD = NGOOD + 1
               IF (UP) THEN
                  CALL GPOS (XY(1), XY(2), BUFFER, IERR)
               ELSE
                  CALL GVEC (XY(1), XY(2), BUFFER, IERR)
                  END IF
               UP = .FALSE.
               IF (IERR.NE.0) GO TO 970
               END IF
            END IF
 30      CONTINUE
C                                       pts w error bars
      ILITY = 4
      CALL GLTYPE (ILITY, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      DX = 5.0 * FACTOR
      DY = 5.0 * FACTOR
      IF (XYRATO.GT.1.0) THEN
         DY = DY * XYRATO
      ELSE
         DX = DX / XYRATO
         END IF
      DO 50 JT = 1,NTAU
         IF (ANSW(PTYP,1,JT,JB).EQ.FBLANK) THEN
            UP = .TRUE.
         ELSE
            XY(1) = JT
            IF (DOLOG(1)) XY(1) = LOG10 (XY(1))
            XY(2) = ANSW(PTYP,1,JT,JB)
            IF (DOLOG(2)) XY(2) = LOG10 (XY(2))
            XY(1) = XYSCL(1) * (XY(1) - XYOFF(1)) + YYOFF(1)
            XY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
            IF ((XY(1).GE.XBLC(1)) .AND. (XY(1).LE.XTRC(1)) .AND.
     *         (XY(2).GE.XBLC(2)) .AND. (XY(2).LE.XTRC(2))) THEN
               AX(1) = XY(1)
               AY(1) = XY(2)
               AX(2) = AX(1)
               AX(3) = AX(1)
               AX(4) = AX(1) - DX
               AX(5) = AX(1) + DX
               AY(2) = AY(1) + DY
               AY(3) = AY(1) - DY
               AY(4) = AY(1)
               AY(5) = AY(1)
               CALL PNTPLT (ISYM, AX, AY, XBLC, XTRC, .FALSE., .FALSE.,
     *            BUFFER, IERR)
               IF (IERR.NE.0) GO TO 970
               IF (DOEBAR.GT.0.0) THEN
                  XY(2) = ANSW(PTYP,1,JT,JB) + ANSW(PTYP,2,JT,JB)
                  IF (DOLOG(2)) XY(2) = LOG10 (XY(2))
                  AY(1) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                  AY(1) = MIN (AY(1), XTRC(2))
                  XY(2) = ANSW(PTYP,1,JT,JB) - ANSW(PTYP,2,JT,JB)
                  IF (DOLOG(2)) XY(2) = LOG10 (MAX (1.E-10, XY(2)))
                  AY(2) = XYSCL(2) * (XY(2) - XYOFF(2)) + YYOFF(2)
                  AY(2) = MAX (AY(2), XBLC(2))
                  CALL GPOS (XY(1), AY(1), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  CALL GVEC (XY(1), AY(2), BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 970
                  END IF
               END IF
            END IF
 50      CONTINUE
C                                       Done: finish plot
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      IF ((IPLOT.LE.0) .OR. (ABS(IPLOT).GE.ABS(NPLOTS))) THEN
         GPHPAG = IPLOT.GT.0
         CALL GFINIS (BUFFER, IERR)
         IF (IERR.GT.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
         END IF
      IF (IERR.GT.0) GO TO 975
         IRET = MIN (IERR, 0)
         GO TO 999
C                                       ZPHFIL or GINIT failure.
 960  WRITE (MSGTXT,1960)
      CALL MSGWRT (8)
      IF (.NOT.DOTV) THEN
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
      GO TO 999
C                                       Try to finish partial graph
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1200) NGOOD
      CALL MSGWRT (2)
      IF (NNOFIT.GE.1) THEN
         WRITE (MSGTXT,1202) NNOFIT
         CALL MSGWRT (2)
         END IF
      GPHPAG = IPLOT.GT.0
      CALL GFINIS (BUFFER, IERR)
      IF (IERR.NE.0) GO TO 975
         IF (.NOT.DOTV) THEN
            CALL HIPLOT (DISKIN, OLDCNO, VER, BUFFER, IERR)
            IERR = 0
            END IF
         GO TO 999
C                                       Destroy the plot file
 975  IF (.NOT.DOTV) THEN
         CALL ZCLOSE (LUNPL, FINDPL, IERR)
         CALL ZDESTR (DISKIN, PFILE, IERR)
         CALL DELEXT ('PL', DISKIN, OLDCNO, 'WRIT', CATBLK, BUFFER,
     *      VER, IERR)
         NCFILE = NCFILE - 1
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Plot file version',I4,'  created.')
 1020 FORMAT ('Allan Variance with SOLINT =',F7.2)
 1030 FORMAT ('Plot file version',I4,'__created ',A, A)
 1040 FORMAT (I2.2,' - ',I2.2)
 1200 FORMAT ('PLOTAV:',I9,' points plotted')
 1202 FORMAT ('PLOTAV:',I9,' points did not fit')
 1960 FORMAT ('PLOTAV: ERROR DURING GRAPH FILE CREATION')
 1970 FORMAT ('PLOTAV: ERROR DURING GRAPHING. WILL TRY TO FINISH ',
     *   'PARTIAL GRAPH')
      END
