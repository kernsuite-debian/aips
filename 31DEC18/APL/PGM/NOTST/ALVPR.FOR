LOCAL INCLUDE 'ALVPR.INC'
C                                       Local include for ALVPR
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK,
     *   XLPNAM(12)
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC,
     *   SOLINT, DOCRT, BADD(10),
     *   SCRBUF(256), BUFF(UVBFSS), TLAST
      INTEGER   SEQIN, DISKIN, JBUFSZ, ILOCWT, CATOLD(256), INCSI,
     *   INCFI, INCIFI, NRPRMI, OLDCNO, NSTK, NIF, NFREQ, NBASL, CTIME,
     *   NTIMES
      CHARACTER NAMEIN*12, CLAIN*6, LPNAME*48
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XDOAC, SOLINT, DOCRT, XLPNAM, BADD
      COMMON /ALVPRM/ CATOLD, SEQIN, DISKIN, ILOCWT, INCSI, INCFI,
     *   INCIFI, NRPRMI, OLDCNO, NSTK, NIF, NFREQ, NBASL, TLAST, CTIME,
     *   NTIMES
      COMMON /CHARPM/ NAMEIN, CLAIN, LPNAME
      COMMON /BUFRS/ SCRBUF, BUFF, JBUFSZ
C                                       End local include for ALVPR
LOCAL END
      PROGRAM ALVPR
C-----------------------------------------------------------------------
C! Allan-variance of a UV data set
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2013, 2015-2016, 2018
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
C   ALVPR pre-averages data to SOLINT seconds and computes the Allan
C   variance of the data, printing statistics of that variance at the
C   end.
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
      INTEGER   IRET, NWORDS
      REAL      SUMS(2), VALS(2), ANSW(2)
      LONGINT   PSUMS, PVALS, PANSW
      INCLUDE 'ALVPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'ALVPR '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL ALVPRI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       count times
      MSGTXT = 'Count the number of SOLINTs in the data'
      CALL MSGWRT (2)
      NTIMES = 0
      CALL ALVPRC (IRET)
      IF (IRET.NE.0) GO TO 999
      NTIMES = NTIMES + 10
C                                       get memory
      NBASL = (NSTNS * (NSTNS+1)) / 2
      NWORDS = (3 * NSTK * NIF * NBASL - 1) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, SUMS, PSUMS, IRET)
      NWORDS = (3 * NSTK * NIF * NBASL * NTIMES - 1) / 1024 + 2
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VALS, PVALS,
     *   IRET)
      NWORDS = (4 * NTIMES - 1) / 1024 + 10
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ANSW, PANSW,
     *   IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'CANNOT GET NEEDED MEMORY'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Sum up data, make big array
      MSGTXT = 'Sum up the data into big arrays'
      CALL MSGWRT (2)
      CALL ALVPRU (NSTK, NIF, NBASL, SUMS(1+PSUMS), VALS(1+PVALS),
     *   IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Display big array
      CALL ALVPCH (NSTK, NIF, NBASL, IRET)
      IF (IRET.NE.0) GO TO 990
      MSGTXT = 'Now find Allan variance and display'
      CALL MSGWRT (2)
      CALL ALVPRP (NSTK, NIF, NBASL, VALS(1+PVALS), ANSW(1+PANSW), IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  IRET = MAX (0, IRET)
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE ALVPRI (PRGN, JERR)
C-----------------------------------------------------------------------
C   ALVPRI gets input parameters for ALVPR
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
C   See prologue comments in ALVPR for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file.
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R, INNAME etc. are 12 char. 3 words;
C       INCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not INNAME (NAMEIN) then replace
C       NAMEIN in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R words desired.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'ALVPR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
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
      NPARM = 182
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
      IF ((IERR.NE.0) .OR. (NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000))
     *   DOCRT = MIN (-1.0, DOCRT)
      RQUICK = (RQUICK) .AND. (DOCRT.LE.0.0)
      IF (RQUICK) RQUICK = LPNAME.NE.' '
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
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
      DOACOR = XDOAC.GT.0.0
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
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
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
C                                       get antenna info
      CALL COPY (256, CATOLD, CATBLK)
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, SCRBUF, IERR)
      IF (SOLINT.LE.0.0) SOLINT = 9.95
      SOLINT = SOLINT / (24.0 * 3600.0)
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVPRI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE ALVPRC (IRET)
C-----------------------------------------------------------------------
C   ALVPRC counts time intervals
C   Output:
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'ALVPR.INC'
      INTEGER   NUMVIS, XCOUNT, CATMP(256)
      REAL      T, VIS(UVBFSS), RPARM(20)
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
      NUMVIS = 0
      XCOUNT = 0
C                                       buffer pointers
      TLAST = -1.E6
      CTIME = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         T = RPARM(1+ILOCT)
         IF (T.GT.TLAST) THEN
            IF (TLAST.GT.0.0) NTIMES = NTIMES + 1
            TLAST = T + SOLINT
            END IF
         GO TO 100
         END IF
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVPRC: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1100 FORMAT ('ALVPRC: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE ALVPRU (NS, NI, NB, SUMS, VALS, IRET)
C-----------------------------------------------------------------------
C   ALVPRU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input
C      NS     I      Number stokes
C      NI     I      Number IFs
C      NB     I      Number baselines
C   Output:
C      SUMS   R(*)   Summing variables
C      VALS   R(*)   Time sequence of real and imaginary averages
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NS, NI, NB, IRET
      REAL      SUMS(3,NS,NI,*), VALS(3,NS,NI,NB,*)
C
      INCLUDE 'ALVPR.INC'
      INTEGER   IA1, IA2, NUMVIS, XCOUNT, CATMP(256)
      REAL      DUM, BASEN, VIS(UVBFSS), RPARM(20)
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
      NUMVIS = 0
      XCOUNT = 0
C                                       buffer pointers
      TLAST = -1.E6
      CTIME = 0
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
         NUMVIS = NUMVIS + 1
C                                       call user routine
         CALL DIDDLE (NUMVIS, RPARM(1+ILOCT), IA1, IA2, VIS, NS, NI, NB,
     *      SUMS, VALS)
         GO TO 100
         END IF
C                                       last average
      NUMVIS = -1
      CALL DIDDLE (NUMVIS, DUM, IA1, IA2, SCRBUF, NS, NI, NB, SUMS,
     *   VALS)
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVPRU: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1100 FORMAT ('ALVPRU: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE DIDDLE (NUMVIS, T, IA1, IA2, VIS, NS, NI, NB, SUMS,
     *   VALS)
C-----------------------------------------------------------------------
C
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
      INTEGER   NUMVIS, IA1, IA2, NS, NI, NB, IRET
      REAL      T, VIS(3,*), SUMS(3,NS,NI,*), VALS(3,NS,NI,NB,*)
C
      INTEGER   JI, JF, JS, JB, INDEXI, INDI
      REAL      VR, VI, VW
      INCLUDE 'ALVPR.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LT.0) T = 1.E6
C                                       new integration
      IF (T.GT.TLAST) THEN
C                                       average this lot
         IF (TLAST.GT.0.0) THEN
            CTIME = CTIME + 1
            DO 40 JB = 1,NB
               DO 30 JI = 1,NI
                  DO 20 JS = 1,NS
                     VW = SUMS(3,JS,JI,JB)
                     IF ((VW.GT.0.0) .AND. (VW.NE.FBLANK)) THEN
                        VALS(1,JS,JI,JB,CTIME) = SUMS(1,JS,JI,JB)/VW
                        VALS(2,JS,JI,JB,CTIME) = SUMS(2,JS,JI,JB)/VW
                        VALS(3,JS,JI,JB,CTIME) = VW
                     ELSE
                        VALS(1,JS,JI,JB,CTIME) = FBLANK
                        VALS(2,JS,JI,JB,CTIME) = FBLANK
                        VALS(3,JS,JI,JB,CTIME) = FBLANK
                        END IF
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
            END IF
         JF = 3 * NS * NI * NB
         CALL RFILL (JF, 0.0, SUMS)
         TLAST = T + SOLINT
         END IF
C                                       time average
      IF (NUMVIS.GT.0) THEN
         JB = ((IA1-1)*NSTNS) - (((IA1-1)*IA1)/2) + IA2
         DO 130 JI = 1,NI
            DO 120 JS = 1,NS
               INDI = (JI-1) * INCIFI + (JS-1) * INCSI + 1
               VR = 0.0
               VI = 0.0
               VW = 0.0
               INDEXI = INDI
C                                       average over frequency
               DO 110 JF = 1,NFREQ
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     VR = VR + VIS(1,INDEXI) * VIS(3,INDEXI)
                     VI = VI + VIS(2,INDEXI) * VIS(3,INDEXI)
                     VW = VW + VIS(3,INDEXI)
                     END IF
                  INDEXI = INDEXI + INCFI
 110              CONTINUE
               SUMS(1,JS,JI,JB) = SUMS(1,JS,JI,JB) + VR
               SUMS(2,JS,JI,JB) = SUMS(2,JS,JI,JB) + VI
               SUMS(3,JS,JI,JB) = SUMS(3,JS,JI,JB) + VW
 120           CONTINUE
 130        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE ALVPCH (NS, NI, NB, IRET)
C-----------------------------------------------------------------------
C   ALVPCH checks the printer line count
C   Input
C      NS     I      Number stokes
C      NI     I      Number IFs
C      NB     I      Number baselines
C   Output:
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NS, NI, NB, IRET
C
      INCLUDE 'ALVPR.INC'
      CHARACTER CTEMP*4, SCRTCH*132
      INTEGER   NCOUNT, TTY(2), I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       do we need to do this
      IRET = 0
      IF ((DOCRT.GT.0.0) .OR. (LPNAME.NE.' ')) GO TO 999
      MSGTXT = 'Checking count of lines for direct output to printer'
      CALL MSGWRT (2)
      NCOUNT = 0
C                                       header
      IF (DOCRT.GT.-2.5) NCOUNT = NCOUNT + 4
      IF (DOCRT.GT.-1.5) NCOUNT = NCOUNT + 1
C                                       over baselines
      NCOUNT = NCOUNT + NB * NI * NS
C                                       ask if needed
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         IF (NCOUNT.GT.1000) IRET = -1
      ELSE IF (NCOUNT.GT.500) THEN
         TTY(1) = 5
         CALL ZOPEN (TTY(1), TTY(2), 1, SCRTCH, .FALSE., .FALSE.,
     *      .TRUE., IRET)
         MSGTXT = 'PROBLEM OPENING TERMINAL'
         IF (IRET.GT.0) GO TO 990
         WRITE (SCRTCH,1000) NCOUNT
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, IRET)
         MSGTXT = 'PROBLEM DOING IO TO TERMINAL'
         IF (IRET.GT.0) GO TO 990
         SCRTCH = 'Do you really want to print this much??' //
     *      ' Enter Y or y if so'
         CALL INQSTR (TTY, SCRTCH, 1, CTEMP, IRET)
         IF (IRET.GT.0) GO TO 990
         IF ((CTEMP(:1).NE.'y') .AND. (CTEMP(:1).NE.'Y')) THEN
            IRET = -1
            SCRTCH = 'Good choice - save trees'
         ELSE
            SCRTCH = 'OKAY, printing anyway'
            END IF
         CALL ZTTYIO ('WRIT', TTY(1), TTY(2), 72, SCRTCH, I)
         CALL ZCLOSE (TTY(1), TTY(2), I)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Requested print job is',I10,' lines long!')
      END
      SUBROUTINE ALVPRP (NS, NI, NB, VALS, ANSW, IRET)
C-----------------------------------------------------------------------
C   ALVPRU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input
C      NS     I      Number stokes
C      NI     I      Number IFs
C      NB     I      Number baselines
C   Output:
C      VALS   R(*)   Time sequence of real and imaginary averages
C      IRET   I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NS, NI, NB, IRET
      REAL      VALS(3,NS,NI,NB,*), ANSW(4,*)
C
      INTEGER   NITER
      PARAMETER (NITER = 8)
C
      INCLUDE 'ALVPR.INC'
      CHARACTER TITL1*132, TITL2*132, LINE*132, SCRTCH*132
      INTEGER   LUNP, FINDP, NACROS, IA1, IA2, JB, JI, JS, JT, NG, JP,
     *   PAGE, IPCNT, IT
      REAL      TR1, TI1, TR2, TI2, TR3, TI3, TA1, TA2, TA3, SR, SI,
     *   RAW(2,4), ROB(2,4), WS(NITER), VP, VM, P1, P2, P3
      DOUBLE PRECISION S1, S2, S3, S4, Q1, Q2, Q3, Q4, NSQ, SV, SSV, NV
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA WS /5.0, 4.0, 3.5, 3.0, 2.7, 2.6, 2.5, 3.5/
C-----------------------------------------------------------------------
C                                       Open output device
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING LINE PRINTER'
         GO TO 990
         END IF
C                                       header
      PAGE = 0
      IPCNT = 998
      TITL1 = ' '
      TITL2 = ' '
      IF (DOCRT.GT.-2.5) THEN
         WRITE (LINE,2000) NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER, BCHAN,
     *      ECHAN
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      WRITE (TITL1,2010)
      WRITE (TITL2,2011)
      IF (DOCRT.GT.-2.5) THEN
         LINE = TITL1
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         LINE = TITL2
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
      IF (DOCRT.GT.-1.5) THEN
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      IPCNT, PAGE, SCRTCH, IRET)
         IF (IRET.NE.0) GO TO 960
         END IF
C                                       over baselines
      IA1 = 1
      IA2 = 0
      DO 200 JB = 1,NB
         IA2 = IA2 + 1
         IF (IA2.GT.NSTNS) THEN
            IA1 = IA1 + 1
            IA2 = IA1
             END IF
         DO 190 JI = 1,NI
            DO 180 JS = 1,NS
C                                       find amplitudes
               NG = 0
               DO 100 JT = 1,NTIMES
                  IF ((VALS(1,JS,JI,JB,JT).NE.FBLANK) .AND.
     *               (VALS(2,JS,JI,JB,JT).NE.FBLANK)) THEN
                     VALS(3,JS,JI,JB,JT) = SQRT (VALS(1,JS,JI,JB,JT)**2
     *                  + VALS(2,JS,JI,JB,JT)**2)
                     IF (VALS(3,JS,JI,JB,JT).LE.0.0) THEN
                        VALS(1,JS,JI,JB,JT) = FBLANK
                        VALS(2,JS,JI,JB,JT) = FBLANK
                        VALS(3,JS,JI,JB,JT) = FBLANK
                     ELSE
                        NG = NG + 1
                        END IF
                  ELSE
                     VALS(3,JS,JI,JB,JT) = FBLANK
                     END IF
 100              CONTINUE
C                                       none
              IF (NG.LE.0) THEN
                 CALL RFILL (6, 0.0, RAW)
C                                       find differences
              ELSE
C                                       do brute rms/mean
                 S1 = 0.0D0
                 S2 = 0.0D0
                 S3 = 0.0D0
                 S4 = 0.0D0
                 Q1 = 0.0D0
                 Q2 = 0.0D0
                 Q3 = 0.0D0
                 Q4 = 0.0D0
                 NSQ = 0.0D0
                 DO 110 JT = 1,NTIMES-2
                    TR1 = VALS(1,JS,JI,JB,JT)
                    TI1 = VALS(2,JS,JI,JB,JT)
                    TA1 = VALS(3,JS,JI,JB,JT)
                    TR2 = VALS(1,JS,JI,JB,JT+1)
                    TI2 = VALS(2,JS,JI,JB,JT+1)
                    TA2 = VALS(3,JS,JI,JB,JT+1)
                    TR3 = VALS(1,JS,JI,JB,JT+2)
                    TI3 = VALS(2,JS,JI,JB,JT+2)
                    TA3 = VALS(3,JS,JI,JB,JT+2)
                    IF ((TA1.NE.FBLANK) .AND. (TA2.NE.FBLANK) .AND.
     *                 (TA3.NE.FBLANK)) THEN
                       ANSW(3,JT) = (TA3 + TA1 - 2.*TA2) ** 2
                       SR = (TR3 + TR1 - 2.*TR2)
                       SI = (TI3 + TI1 - 2.*TI2)
                       ANSW(2,JT) = SR * SR + SI * SI
                       SR = (TR3/TA3 + TR1/TA1 - 2.*TR2/TA2)
                       SI = (TI3/TA3 + TI1/TA1 - 2.*TI2/TA2)
                       ANSW(1,JT) = SR * SR + SI * SI
                       P1 = ATAN2 (TI1, TR1)
                       P2 = ATAN2 (TI2, TR2)
                       P3 = ATAN2 (TI3, TR3)
                       CALL PHMINI (P1, P2, P3, ANSW(4,JT))
                       Q1 = Q1 + ANSW(1,JT)
                       S1 = S1 + ANSW(1,JT) ** 2
                       Q2 = Q2 + ANSW(2,JT)
                       S2 = S2 + ANSW(2,JT) ** 2
                       Q3 = Q3 + ANSW(3,JT)
                       S3 = S3 + ANSW(3,JT) ** 2
                       Q4 = Q4 + ANSW(4,JT)
                       S4 = S4 + ANSW(4,JT) ** 2
                       NSQ = NSQ + 1.0D0
                    ELSE
                       ANSW(1,JT) = FBLANK
                       ANSW(2,JT) = FBLANK
                       ANSW(3,JT) = FBLANK
                       ANSW(4,JT) = FBLANK
                       END IF
 110                CONTINUE
                 IF (NSQ.GT.0.0D0) THEN
                    RAW(1,1) = Q1 / NSQ
                    RAW(1,2) = Q2 / NSQ
                    RAW(1,3) = Q3 / NSQ
                    RAW(1,4) = Q4 / NSQ
                    RAW(2,1) = S1 / NSQ - RAW(1,1)*RAW(1,1)
                    RAW(2,2) = S2 / NSQ - RAW(1,2)*RAW(1,2)
                    RAW(2,3) = S3 / NSQ - RAW(1,3)*RAW(1,3)
                    RAW(2,4) = S4 / NSQ - RAW(1,4)*RAW(1,4)
                    RAW(2,1) = SQRT (MAX (0.0, RAW(2,1)))
                    RAW(2,2) = SQRT (MAX (0.0, RAW(2,2)))
                    RAW(2,3) = SQRT (MAX (0.0, RAW(2,3)))
                    RAW(2,4) = SQRT (MAX (0.0, RAW(2,4)))
                    END IF
C                                       loop over 3 parameters
                 DO 140 JP = 1,4
                    VP = RAW(1,JP) + WS(1) * RAW(2,JP)
                    VM = RAW(1,JP) - WS(1) * RAW(2,JP)
                    DO 130 IT = 1,NITER
                       SV = 0.0D0
                       SSV = 0.0D0
                       NV = 0.0D0
                       DO 120 JT = 1,NTIMES-2
                          IF (ANSW(JP,JT).NE.FBLANK) THEN
                             IF ((ANSW(JP,JT).GT.VM) .AND.
     *                          (ANSW(JP,JT).LT.VP)) THEN
                                SV = SV + ANSW(JP,JT)
                                SSV = SSV + ANSW(JP,JT)**2
                                NV = NV + 1.0D0
                                END IF
                             END IF
 120                      CONTINUE
                       IF (NV.GT.0.0D0) THEN
                          SV = SV / NV
                          SSV = SSV / NV - SV * SV
                          SSV = SQRT (MAX (0.0D0, SSV))
                          IF (IT.LT.NITER) THEN
                             VP = SV + WS(IT+1) * SSV
                             VM = SV - WS(IT+1) * SSV
                             END IF
                       ELSE
                          VP = 1.E4
                          VM = -1.E4
                          END IF
 130                   CONTINUE
                    ROB(1,JP) = SV
                    ROB(2,JP) = SSV
 140                CONTINUE
                  WRITE (LINE,2140) IA1, IA2, JS, JI, RAW(1,1),
     *               RAW(2,1), RAW(1,4), RAW(2,4), ROB(1,1), ROB(2,1),
     *               ROB(1,4), ROB(2,4)
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, IPCNT, PAGE, SCRTCH, IRET)
                  IF (IRET.NE.0) GO TO 960
                  END IF
 180           CONTINUE
 190        CONTINUE
 200     CONTINUE
C
 960  IF (IRET.LE.0) IRET = 0
      CALL LPCLOS (LUNP, FINDP, IPCNT, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ALVPRP: ERROR',I4,' ON ',A)
 2000 FORMAT (A12,'.',A6,'.',I4,'  Vol=',I2,'  User=',I5,'  Channels=',
     *   I4,' to ',I4)
 2010 FORMAT ('An1 An2 S IF',16X,'Mean square',14X,2X,10X,
     *   'Robust mean square')
 2011 FORMAT (12X,'  Normal Vector Amp ','   Phase (radians^2)',3X,
     *   '  Normal Vector Amp ','   Phase (radians^2)')
 2140 FORMAT (I3,I4,I2,I3,2F10.6,1x,2F10.6,2X,2F10.6,1X,2F10.6)
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
