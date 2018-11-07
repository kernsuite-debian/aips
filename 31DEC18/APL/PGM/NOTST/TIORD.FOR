LOCAL INCLUDE 'TIORD.INC'
C                                       Local include for TIORD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XPRT(10),
     *   BADD(10),  SCRBUF(256), BUFF2(UVBFSS), TLAST, BLAST
      INTEGER   SEQIN, DISKIN, JBUFSZ, ILOCWT, CATOLD(256), INCSI,
     *   INCFI, INCIFI, NRPRMI, OLDCNO, NERROR, NBERR, NAERR, PRTLIM(3)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XPRT, BADD
      COMMON /TIORDP/ CATOLD, SEQIN, DISKIN, ILOCWT, INCSI, INCFI,
     *   INCIFI, NRPRMI, ISCOMP, OLDCNO, NERROR, TLAST, BLAST, NBERR,
     *   NAERR, PRTLIM
      COMMON /CHARPM/ NAMEIN, CLAIN
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for TIORD
LOCAL END
      PROGRAM TIORD
C-----------------------------------------------------------------------
C! Check time ordering
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2010-2011, 2013-2015, 2018
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
C   TIORD checks the data set for time ordering including after the
C   application of the cal adverbs.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      APARM         User specified array.
C      BPARM(10)      BPARM         User specified array.
C      BOX(4,10)      BOX           User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'TIORD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TIORD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TIORIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL TIORUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE TIORIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TIORIN gets input parameters for TIORD.
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
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'TIORD.INC'
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
      NPARM = 177
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
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      PRTLIM(1) = IROUND (XPRT(1))
      PRTLIM(2) = IROUND (XPRT(2))
      PRTLIM(3) = IROUND (XPRT(3))
      IF (PRTLIM(1).LE.0) PRTLIM(1) = 100
      IF (PRTLIM(2).LE.0) PRTLIM(2) = 100
      IF (PRTLIM(3).LE.0) PRTLIM(3) = 10
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = .TRUE.
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
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
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
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      NERROR = 0
      NBERR = 0
      NAERR = 0
      TLAST = -1.E10
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIORIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE TIORUV (IRET)
C-----------------------------------------------------------------------
C   TIORUV sends uv data one point at a time to the time chacke routine.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   ILENBU, NIOLIM, NUMVIS, XCOUNT, CATMP(256), IA1, IA2
      INCLUDE 'TIORD.INC'
      REAL      DUM, VIS(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
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
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
C                                       call user routine
         IF (ILOCB.GE.0) THEN
            IA2 = RPARM(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - 256*IA1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         CALL TORDER (NUMVIS, RPARM(1+ILOCT), IA1, IA2, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1
            END IF
         GO TO 100
         END IF
C                                       Final call to TORDER.
      NUMVIS = -1
      CALL TORDER (NUMVIS, DUM, IA1, IA2, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       close NX table
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TIORUV: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1100 FORMAT ('TIORUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('TIORUV: TORDER ERROR',I3)
      END
      SUBROUTINE TORDER (NUMVIS, T, A1, A2, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      A1      I    Antenna 1
C      A2      I    Antenna 1
C   Output:
C      T          R    Time in same units as input.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, A1, A2, IRET
      REAL      T
C
      INTEGER   MAXBLE
      PARAMETER (MAXBLE=1000)
      INTEGER   NUMPR1, NUMB,  T1(4), T2(4), T31(3), T32(3), B1, B2, I,
     *   J, NUMPR2
      REAL      B, TS1, TS2, TBLERR(5,MAXBLE)
      INTEGER   BLERR(5,MAXBLE)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'TIORD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (T1, T31), (T2, T32), (BLERR, TBLERR)
      SAVE NUMPR1, NUMPR2, NUMB, BLERR
      DATA NUMPR1, NUMPR2, NUMB /0, 0, 0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       call with data
      IF (NUMVIS.GT.0) THEN
         IF (T.GT.TLAST) BLAST = -1.E3
C                                       time order
         IF (T.LT.TLAST) THEN
            IF (NUMPR1.LT.PRTLIM(1)) THEN
               NUMPR1 = NUMPR1 + 1
               IF (NUMPR1.EQ.1) THEN
                  WRITE (MSGTXT,1000)
                  CALL MSGWRT (5)
                  END IF
               CALL T2DHMS (3, T, T1, TS1)
               CALL T2DHMS (3, TLAST, T2, TS2)
               WRITE (MSGTXT,1001) NUMVIS, T31, TS1, T32, TS2
               IF (MSGTXT(22:22).EQ.' ') MSGTXT(22:22) = '0'
               IF (MSGTXT(41:41).EQ.' ') MSGTXT(41:41) = '0'
               CALL MSGWRT (5)
               END IF
            NERROR = NERROR + 1
            END IF
         TLAST = T
C                                       antenna values
         IF ((A1.GT.A2) .OR. (A1.LE.0) .OR. (A2.LE.0) .OR.
     *      (A1.GT.MAXANT) .OR. (A2.GT.MAXANT)) THEN
            IF (NUMPR2.LT.PRTLIM(2)) THEN
               NUMPR2 = NUMPR2 + 1
               CALL T2DHMS (3, T, T1, TS1)
               WRITE (MSGTXT,1002) T31, TS1, A1, A2
               IF (MSGTXT(22:22).EQ.' ') MSGTXT(22:22) = '0'
               CALL MSGWRT (5)
               END IF
            NAERR = NAERR + 1
            END IF
C                                       baseline order
         B = 4096.0 * A1 + A2
         IF (B.LT.BLAST) THEN
            IF (NUMB.LT.MAXBLE) THEN
               NUMB = NUMB + 1
               B1 = BLAST / 4096.0 + 0.01
               B2 = BLAST - B1 * 4096 + 0.01
               TBLERR(1,NUMB) = T
               BLERR(2,NUMB) = A1
               BLERR(3,NUMB) = A2
               BLERR(4,NUMB) = B1
               BLERR(5,NUMB) = B2
               END IF
            NBERR = NBERR + 1
            END IF
         BLAST = B
C                                       last call - no vis
      ELSE
         NUMB = MIN (NUMB, PRTLIM(3))
         WRITE (MSGTXT,1100)
         IF (NUMB.GT.0) CALL MSGWRT (5)
         DO 100 I = 1,NUMB
            CALL T2DHMS (3, TBLERR(1,I), T1, TS1)
            WRITE (MSGTXT,1101) T31, TS1, (BLERR(J,I), J = 2,5)
            IF (MSGTXT(11:11).EQ.' ') MSGTXT(11:11) = '0'
            CALL MSGWRT (5)
 100        CONTINUE
C                                       summary
         WRITE (MSGTXT,1110) NERROR
         CALL MSGWRT (5)
         WRITE (MSGTXT,1111) NAERR
         CALL MSGWRT (5)
         WRITE (MSGTXT,1112) NBERR
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('   VIS NUM','    TIME',6X,7X,'  T_LAST')
 1001 FORMAT (I10,I4,'/',2(I2.2,':'),F6.3,3X,I3,'/',2(I2.2,':'),F6.3)
 1002 FORMAT (10X,I4,'/',2(I2.2,':'),F6.3,3X,'ANTENNAS',2I4)
 1100 FORMAT ('  TIME',8X,'BASELINE  <  PREVIOUS BL')
 1101 FORMAT (I3,'/',2(I2.2,':'),F6.3,3X,I2.2,' - ',I2.2,5X,I2.2,
     *   ' - ',I2.2)
 1110 FORMAT ('Total number time errors    ',I9)
 1111 FORMAT ('Total number antenna errors ',I9)
 1112 FORMAT ('Total number baseline errors',I9)
      END
      SUBROUTINE T2DHMS (NDIG, TIMEIN, TIME, TIMS)
C-----------------------------------------------------------------------
C   Convert from Time to Days Hours Minutes Seconds format
C   Input:
C      NDIG     I       Number of digits in display, determines rounding
C                       for TIMS.
C      TIMEIN   R       Time in days
C   Output:
C      TIME     I*(4)   Output Time in Days Hours Minutes Seconds
C      TIMS     R       Output Time in Seconds
C-----------------------------------------------------------------------
      REAL     TIMEIN, TIMS
      INTEGER  NDIG, TIME(4)
C
      REAL     T
      INTEGER  MUL
C-----------------------------------------------------------------------
      MUL = 10 ** (MAX (0, NDIG))
      T = TIMEIN
      IF (TIMEIN.LT.0.0) T = -T
C
      TIME(1) = T
      T = (T - TIME(1)) * 24.0
      TIME(2) = T
      T = (T - TIME(2)) * 60.0
      TIME(3) = T
      T = (T - TIME(3)) * 60.0
      TIMS = T
      TIME(4) = T*MUL + 0.5
C                                       Now Remove 60 seconds
      IF (TIME(4).GE.60*MUL) THEN
         TIME(4) = TIME(4) - 60*MUL
         TIME(3) = TIME(3) + 1
         END IF
C                                       Now Remove 60 minutes
      IF (TIME(3).GE.60) THEN
         TIME(3) = TIME(3) - 60
         TIME(2) = TIME(2) + 1
         END IF
C                                       Now Remove 24 hours
      IF (TIME(2).GE.24) THEN
         TIME(2) = TIME(2) - 24
         TIME(1) = TIME(1) + 1
         END IF
C                                       Sign
      IF (TIMEIN.LT.0.0) TIME(1) = -TIME(1)
C                                       Seconds
      TIMS = REAL (TIME(4)) / REAL (MUL)
      TIME(4) = TIMS + 0.5
C
 999  RETURN
      END
