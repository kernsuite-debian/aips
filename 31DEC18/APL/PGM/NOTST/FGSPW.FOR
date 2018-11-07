LOCAL INCLUDE 'FGSPW.INC'
C                                       Local include for FGSPW
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XOUTFG,
     *   SOLINT, APARM(10), BADD(10),    SCRBUF(256), BUFF1(UVBFSS),
     *   BUFF2(UVBFSS)
      INTEGER   SEQIN, DISKIN, NUMHIS, JBUFSZ, ILOCWT, CATOLD(256),
     *   INCSI, INCFI, INCIFI, NRPRMI, OLDCNO, FGVERO, NANT, NIF, NPOL,
     *   NCHAN, IDSOUR, NFLAGS, MFLAGS(2,MAXIF), INSOU,
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      CHARACTER NAMEIN*12, CLAIN*6, TTIME(2)*12
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND, XBPVER,
     *   XSMOTH, XOUTFG, SOLINT, APARM, BADD
      COMMON /FGSPWP/ CATOLD, SEQIN, DISKIN, NUMHIS, ILOCWT, INCSI,
     *   INCFI, INCIFI, NRPRMI, OLDCNO, FGVERO, NANT, NIF, NPOL, NCHAN,
     *   IDSOUR, NFLAGS, MFLAGS, INSOU
      COMMON /CHARPM/ NAMEIN, CLAIN, TTIME
      COMMON /BUFRS/ SCRBUF, BUFF1, BUFF2, JBUFSZ
C                                       End local include for FGSPW
LOCAL END
      PROGRAM FGSPW
C-----------------------------------------------------------------------
C! Flags bad spectral windows
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2017-2018
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
C   FGSPW flags bad spectral wondows ("IFs")
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      OUTFGVER
C      APARM(10)      APARM         User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      DOUBLE PRECISION CATD(128)
      INCLUDE 'FGSPW.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATD)
      DATA PRGM /'FGSPW '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FGSPIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL FGSPUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FGSPHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE FGSPIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   FGSPIN gets input parameters for FGSPW
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
C   See prologue comments in FGSPW for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN, TIME(3),
     *   DATE(3)
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'FGSPW.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 178
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      STOKES = 'FULL'
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      FGVERO = XOUTFG + 0.1
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      DATE(1) = -DATE(1)
      CALL TIMDAT (TIME, DATE, TTIME(2), TTIME)
      SOLINT = SOLINT / 60.0 / 24.0
      IF (SOLINT.LE.0.0) SOLINT = 10.
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
C                                       flag versions
      CALL FNDEXT ('FG', CATBLK, I)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.I)) FGVERO = I + 1
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
      INSOU = 0
      IF ((DOSWNT) .AND. (NSOUWD.EQ.1)) INSOU = SOUWAN(1)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
c      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
c     *   PTYPE, NLUSER, 'WRIT', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGSPIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE FGSPUV (IRET)
C-----------------------------------------------------------------------
C   FGSPUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'FGSPW.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      INTEGER   IA1, IA2, IIF, IP, LUN, LFGRNO, LIF, LRET, LSOU
      LOGICAL   T, F
      REAL      VIS(UVBFSS), RPARM(20), SCANV(MAXANT,MAXANT,MAXIF,4),
     *   TIME(2), FLUX(MAXIF)
      LOGICAL   PFLAGS(4), ISFLAG
      CHARACTER REASON*24
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /48/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT IO TO INPUT UV DATA SET'
         GO TO 990
         END IF
      NPOL = CATBLK(KINAX+JLOCS)
      NIF = CATBLK(KINAX+JLOCIF)
      NCHAN = CATBLK(KINAX+JLOCF)
      NANT = 0
      NFLAGS = 0
      CALL RFILL (2*MAXIF, 0, MFLAGS)
      WRITE (REASON,1010) TSKNAM, TTIME
      RPARM(1) = FBLANK
      CALL RFILL (MAXIF, 1.0, FLUX)
C                                       Loop
C                                       Read vis. record.
 100  CALL SCANAV (RPARM, VIS, TIME, SCANV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'AVERAGING SCAN DATA'
         GO TO 990
C                                       check values
      ELSE
C                                       scaling
         IF ((IDSOUR.NE.LSOU) .AND. (APARM(4).GT.0.0))
     *      CALL GTFLUX (IDSOUR, DISKIN, OLDCNO, CATOLD, FLUX)
         LSOU = IDSOUR
         LRET = IRET
         DO 150 IA1 = 1,NANT-1
            DO 140 IA2 = IA1+1,NANT
               DO 130 IIF = 1,NIF
                  LIF = IIF + BIF - 1
                  ISFLAG = .FALSE.
                  DO 120 IP = 1,NPOL
                     PFLAGS(IP) = SCANV(IA1,IA2,IIF,IP).GT.
     *                  APARM((IP+1)/2) * FLUX(LIF)
                     ISFLAG = ISFLAG .OR. PFLAGS(IP)
 120                 CONTINUE
                  IF (ISFLAG) THEN
                     IF (APARM(3).GT.0) CALL LFILL (4, .TRUE., PFLAGS)
                     CALL FLAGIT ('FLAG', LUN, DISKIN, OLDCNO, FGVER,
     *                  FGVERO, LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR,
     *                  FRQSEL, IA1, IA2, TIME(1), TIME(2), LIF, LIF,
     *                  1, 0, PFLAGS, REASON, CATOLD, IBUFF1, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITE FG TABLE'
                        GO TO 990
                        END IF
                     NFLAGS = NFLAGS + 1
                     IF (PFLAGS(1)) MFLAGS(1,IIF) = MFLAGS(1,IIF) + 1
                     IF (PFLAGS(2)) MFLAGS(2,IIF) = MFLAGS(2,IIF) + 1
                     END IF
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
         IF (LRET.EQ.0) THEN
            GO TO 100
         ELSE
            WRITE (MSGTXT,1100) NFLAGS
            CALL MSGWRT (5)
            DO 170 IIF = 1,NIF
               DO 165 IP = 1,2
                  IF (MFLAGS(IP,IIF).GT.0) THEN
                     LIF = IIF + BIF - 1
                     WRITE (MSGTXT,1150) MFLAGS(IP,IIF), IP, LIF
                     CALL MSGWRT (4)
                     END IF
 165              CONTINUE
 170           CONTINUE
            CALL FLAGIT ('CLOS', LUN, DISKIN, OLDCNO, FGVER, FGVERO,
     *         LFGRNO, FGKOLS, FGNUMV, IDSOUR, SUBARR, FRQSEL, IA1, IA2,
     *         TIME(1), TIME(2), IIF, IIF, 1, 0, PFLAGS, REASON, CATOLD,
     *         IBUFF1, IRET)
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGSPUV: ERROR',I3,' ON ',A)
 1010 FORMAT (A6,A9,1X,A8)
 1100 FORMAT ('Wrote',I10,' flag commands')
 1150 FORMAT ('Wrote',I6,' for pol',I2,' IF',I3)
      END
      SUBROUTINE SCANAV (RPARM, VIS, TIME, SCANV, IRET)
C-----------------------------------------------------------------------
C   Computes scan average scalar amplitude averaging over time and
C   spectral channel within each IF
C   Input
C      SOLINT   R        Max integration time (days)
C   Input/output:
C      RPARM    R(*)     Random parameters
C      VIS      R(3,*)   Visibility data
C      ISOU     I        source number: -1 => ignore source
C   Outputs:
C      TIME     R(2)     Start and stop times of data
C      SCANV    R(*)     Average data
C      IRET     I        Error code: -1 => end of data
C-----------------------------------------------------------------------
      INCLUDE 'FGSPW.INC'
      INTEGER   IRET
      REAL      RPARM(*), VIS(3,*), TIME(2),
     *   SCANV(MAXANT,MAXANT,MAXIF,*)
C
      INTEGER   IA1, IA2, I, COUNT(MAXANT,MAXANT,MAXIF,4), SCANUM,
     *   INDEXI, JF, JIF, JS
      LOGICAL   DONE1, GOTDAT
      REAL      BASEN, TT, AMP, WT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      DONE1 = RPARM(1).NE.FBLANK
      GOTDAT = .FALSE.
C                                       clear arrays
 10   I = MAXANT * MAXANT * MAXIF * 4
      CALL RFILL (I, 0.0, SCANV)
      CALL FILL (I, 0, COUNT)
      TIME(1) = 1.E10
      TIME(2) = -1.E10
C                                       Save scan number (0= no index)
      SCANUM = INXRNO
C                                       Loop reading data
 100  IF (.NOT.DONE1) THEN
         CALL UVGET ('READ', RPARM, VIS, IRET)
         IF (IRET.GT.0) GO TO 999
         END IF
      DONE1 = .FALSE.
C                                       Check if scan done
      TT = RPARM(ILOCT+1)
      IF ((INXRNO.GT.SCANUM) .OR. (IRET.LT.0) .OR.
     *   (TT.GT.TIME(1)+SOLINT)) GO TO 200
C                                       antenna numbers
      IF (ILOCB.GE.0) THEN
         BASEN = RPARM(1+ILOCB)
         IA1 = BASEN / 256. + 0.1
         IA2 = BASEN - IA1*256. + 0.1
      ELSE
         IA1 = RPARM(1+ILOCA1) + 0.1
         IA2 = RPARM(1+ILOCA2) + 0.1
         END IF
C                                       parameters needed later
      NANT = MAX (NANT, IA1)
      NANT = MAX (NANT, IA2)
      IDSOUR = INSOU
      IF (ILOCSU.GE.0) IDSOUR = RPARM(1+ILOCSU) + 0.1
      TIME(1) = MIN (TIME(1), TT)
      TIME(2) = MAX (TIME(2), TT)
      GOTDAT = .TRUE.
      DO 140 JIF = 1,NIF
         DO 130 JF = 1,NCHAN
            DO 120 JS = 1,NPOL
               INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *            (JS-1) * INCSI + 1
               IF (VIS(3,INDEXI).GT.0.0) THEN
                  WT = VIS(3,INDEXI)
                  COUNT(IA1,IA2,JIF,JS) = COUNT(IA1,IA2,JIF,JS) + WT
                  AMP = SQRT (VIS(1,INDEXI)**2 + VIS(2,INDEXI)**2)
                  SCANV(IA1,IA2,JIF,JS) = SCANV(IA1,IA2,JIF,JS) + WT*AMP
                  END IF
 120           CONTINUE
 130        CONTINUE
 140     CONTINUE
      GO TO 100
C                                       scan done
 200  IF ((.NOT.GOTDAT) .AND. (IRET.EQ.0)) GO TO 10
      IF (GOTDAT) THEN
         DO 260 IA1 = 1,NANT-1
            DO 250 IA2 = IA1+1,NANT
               DO 240 JIF = 1,NIF
                  DO 230 JS = 1,NPOL
                     IF (COUNT(IA1,IA2,JIF,JS).GT.0.0) THEN
                        WT = COUNT(IA1,IA2,JIF,JS)
                        SCANV(IA1,IA2,JIF,JS) = SCANV(IA1,IA2,JIF,JS) /
     *                     WT
                        END IF
 230                CONTINUE
 240              CONTINUE
 250           CONTINUE
 260        CONTINUE
         END IF
      IF (IRET.LT.0) CALL UVGET ('CLOS', RPARM, VIS, I)
      IF ((IRET.LT.0) .AND. (.NOT.GOTDAT)) THEN
         IRET = 1
         MSGTXT = 'NO DATA FOUND'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE FGSPHI
C-----------------------------------------------------------------------
C   FGSPHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, IERR
      INCLUDE 'FGSPW.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
C-----------------------------------------------------------------------
      IF (NFLAGS.LE.0) GO TO 999
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, OLDCNO, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 999
      WRITE (HILINE,1000) TSKNAM, RLSNAM, TTIME
      CALL HIADD (LUN1, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       calibration history
      CALL CALHIS (LUN1, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       parameters
      WRITE (HILINE,1010) TSKNAM, APARM(1)
      CALL HIADD (LUN1, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, APARM(2)
      CALL HIADD (LUN1, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (APARM(3).GT.0.0) THEN
         WRITE (HILINE,1012) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1015) TSKNAM, NFLAGS
      CALL HIADD (LUN1, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1016) TSKNAM, FGVERO
      CALL HIADD (LUN1, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN1, .TRUE., BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A9,2X,A8)
 1010 FORMAT (A6,'APARM(1) =',F7.2,'   / clip level parallel hands')
 1011 FORMAT (A6,'APARM(2) =',F7.2,'   / clip level cross hands')
 1012 FORMAT (A6,'/ All polarizations flagged when one is bad')
 1015 FORMAT (A6,'/ Wrote',I10,' flag commands to')
 1016 FORMAT (A6,'OUTFGVER =',I4,'   / flag table version')
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, ID, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, BIF,
     *   EIF, BCHAN, ECHAN, PFLAGS, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      ID       I(NID)   List of source ID as defined in SOURCE table
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      BIF      I        First IF number to flag. 0=>all
C      EIF      I        Last IF number to flag. 0=>all higher than IFS(1)
C      BCHAN    I        First channel number to flag. 0=>all
C      ECHAN    I        Last channel number to flag. 0=>all higher.
C      PFLAGS   L(4)     Correlator flags
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   Output:
C      IRET     I        Error code, 0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   ID, SUBA, FQID, ANT1, ANT2, BIF, EIF, BCHAN, ECHAN, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               WRITE (MSGTXT,1002) NROW, VERI, VER
               CALL MSGWRT (2)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 20               CONTINUE
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IFS(1) = BIF
         IFS(2) = EIF
         CHANS(1) = BCHAN
         CHANS(2) = ECHAN
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
C                                       Reset status to read
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'READ', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END
      SUBROUTINE GTFLUX (SUID, DISK, CNO, CATBLK, FLUXES)
C-----------------------------------------------------------------------
C   fill in FLUX array for current source
C   Input:
C      SUID     I       source number
C      DISK     I       disk number
C      CNO      I       catalog number
C      CATBLK   I(*)    header
C   Output:
C      FLUXES   R(*)    flux in Jy from SU or 1.0
C-----------------------------------------------------------------------
      INTEGER   SUID, DISK, CNO, CATBLK(256)
      REAL      FLUXES(*)
C
      INTEGER   IERR, I, LUN, LUNTMP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
C-----------------------------------------------------------------------
      LUN = LUNTMP (1)
      CALL GETSOU (SUID, DISK, CNO, CATBLK, LUN, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FINDING SOURCE FLUX'
         CALL MSGWRT (8)
         CALL RFILL (MAXIF, 1.0, FLUXES)
      ELSE
         DO 20 I = 1,MAXIF
            FLUXES(I) = FLUX(1,I)
            IF (FLUXES(I).LE.0.0) FLUXES(I) = 1.0
 20         CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GTFLUX: ERROR',I4,' ON ',A)
      END
