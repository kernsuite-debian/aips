LOCAL INCLUDE 'FGCNT.INC'
C                                       Local include for FGCNT
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XXSTOK, XOPTYP
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XANT(50), XBASE(50),  XSUBA, XBIF, XEIF, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XFLAG2, XDOBND,
     *   XBPVER, XSMOTH(3), XDOAC,     SCRBUF(256), BUFF2(UVBFSS)
      INTEGER   SEQIN, DISKIN, JBUFSZ, CATOLD(256), INCSI, INCFI,
     *   INCIFI, INCSO, INCFO, INCIFO,LRECO, NRPRMI, OLDCNO, IXANT(50),
     *   IXBAS(50), NXANT, NXBAS, DESEL, NUMIFS, INSNUM, POLTYP
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XANT, XBASE, XSUBA, XBIF,
     *   XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XFLAG, XFLAG2, XDOBND, XBPVER, XSMOTH, XDOAC, XOPTYP
      COMMON /FGCNTP/ CATOLD, SEQIN, DISKIN, INCSI, INCFI, INCIFI,
     *   INCSO, INCFO, INCIFO, LRECO, NRPRMI, OLDCNO, IXANT, IXBAS,
     *   NXANT, NXBAS, DESEL, NUMIFS, INSNUM, POLTYP
      COMMON /CHARPM/ NAMEIN, CLAIN, OPTYPE
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       End local include for FGCNT
LOCAL END
LOCAL INCLUDE 'FGCNTSOU.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NUMSOU, SUNUMS(1000)
      CHARACTER SUNAMS(1000)*16
      COMMON /SUCOMM/ NUMSOU, SUNUMS
      COMMON /SUCOMC/ SUNAMS
LOCAL END
      PROGRAM FGCNT
C-----------------------------------------------------------------------
C! counts samples with different sampling
C# Utility UV editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2016
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
C   FGCNT counts sampling with flag versions F:AGVER and IN2VERS
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, VISCNT(2), ANTCNT(2), NWORDS
      LONGINT   VISPTR, ANTPTR
      INCLUDE 'FGCNT.INC'
      INCLUDE 'FGCNTSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FGCNT'/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FGCNTI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       counting memories
      NWORDS = (4 * NUMIFS * NUMSOU * 2 - 1) / 1024 + 4
      CALL ZMEMRY ('GET ', TSKNAM, NWORDS, VISCNT, VISPTR, IRET)
      IF ((IRET.EQ.0) .AND. (OPTYPE(:2).EQ.'AN')) THEN
         NWORDS = (4 * NUMIFS * MAXANT * NUMSOU * 2 - 1) / 1024 + 4
         CALL ZMEMRY ('GET ', TSKNAM, NWORDS, ANTCNT, ANTPTR, IRET)
         END IF
      IF (IRET.NE.0) THEN
         MSGTXT = 'FAILED TO GET DYNAMIC MEMORY NEEDED'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Call routine that sends data
C                                       to the user routine.
      CALL FGCNTU (NUMIFS, MAXANT, NUMSOU, VISCNT(1+VISPTR),
     *   ANTCNT(1+ANTPTR), IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FGCNTH (OPTYPE, NUMIFS, MAXANT, NUMSOU, VISCNT(1+VISPTR),
     *   ANTCNT(1+ANTPTR), POLTYP)
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE FGCNTI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FGCNTI gets input parameters for FGCNT and creates an output file
C   if necessary.
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
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, NFREQ, LUN
      LOGICAL   MATCH
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'FGCNT.INC'
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
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 260
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
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
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
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
      IF (FGVER.LE.0) FGVER = -1
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
         WRITE (MSGTXT,1000) IERR, 'READING HEADR'
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
      NUMIFS = EIF - BIF + 1
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
C                                       Find baselines to copy
      CALL SETANT (50, XANT, XBASE, NXANT, NXBAS, IXANT, IXBAS, DESEL)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
      INSNUM = 1
      IF ((NSOUWD.EQ.1) .AND. (DOSWNT)) INSNUM = SOUWAN(1)
      POLTYP = 4
      IF (ICOR0.EQ.-1) THEN
         IF (CATR(KRCIC+JLOCS).EQ.-1.0) POLTYP = 1
      ELSE IF (ICOR0.EQ.-5) THEN
         IF (CATR(KRCIC+JLOCS).EQ.-1.0) POLTYP = 2
      ELSE IF (ICOR0.EQ.1) THEN
         IF (CATR(KRCIC+JLOCS).EQ.1.0) POLTYP = 3
         END IF

C                                       Save input file info
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
C                                       set up source and count common
      CALL SOUSET (DISKIN, OLDCNO, CATOLD, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1000) JERR, 'READING SOURCE TABLE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGCNTI: ERROR',I3,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
      END
      SUBROUTINE SOUSET (DISK, CNO, CATBLK, IRET)
C-----------------------------------------------------------------------
C   reads source numbers and names from source file
C   Inputs:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   input header
C   Output:
C      IRET     I      error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), IRET
C
      INTEGER   NREC, IREC, VER, LUN, NKEY, NCOL, DATP(128,2), I,
     *   BUFFER(512), KOLS(2), SUNUM, LRNO, SCRTCH(1024), ITYPE
      CHARACTER KOLTYP(2)*24
      HOLLERITH HNAME(4)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FGCNTSOU.INC'
      DATA KOLTYP /'ID. NO.', 'SOURCE'/
C-----------------------------------------------------------------------
C                                       single source?
      CALL FNDEXT ('SU', CATBLK, I)
      IF (I.LE.0) THEN
         NUMSOU = 1
         SUNAMS(1) = ' '
         CALL H2CHR (8, 1, CATBLK(KHOBJ), SUNAMS(1))
         GO TO 999
         END IF
C                                       open source table
      VER = 1
      LUN = 86
      CALL TABINI ('READ', 'SU', DISK, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN SOURCE TABLE'
         GO TO 980
         END IF
C                                       locate columns
      CALL FNDCOL (2, KOLTYP, 24, .TRUE., BUFFER, KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FIND SOURCE NAME AND NUMBER COLS'
         GO TO 980
         END IF
C                                       read it
      NREC = BUFFER(5)
      LRNO = 0
      DO 100 IREC = 1,NREC
         CALL GETCOL (IREC, KOLS(1), DATP, LRNO, BUFFER, ITYPE, SUNUM,
     *      SCRTCH, IRET)
         IF (IRET.EQ.0) CALL GETCOL (IREC, KOLS(2), DATP, LRNO, BUFFER,
     *      ITYPE, HNAME, SCRTCH, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ SU TABLE ROW'
            GO TO 980
            END IF
         CALL H2CHR (16, 1, HNAME, SUNAMS(IREC))
         SUNUMS(IREC) = SUNUM
 100     CONTINUE
      NUMSOU = NREC
      CALL TABIO ('CLOS', 0, IREC, BUFFER, BUFFER, IRET)
      GO TO 999
C
 980  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SOUSET ERROR',I4,' ON ',A)
      END
      SUBROUTINE FGCNTU (NI, NA, NS, VISCNT, ANTCNT, IRET)
C-----------------------------------------------------------------------
C   FGCNTU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NI, NA, NS, VISCNT(4,NI,NS,2), ANTCNT(4,NI,NA,NS,2),
     *   IRET
C
      INCLUDE 'FGCNT.INC'
      INTEGER   IA1, IA2, NUMVS1, NUMVS2, CATMP(256), I
      LOGICAL   REQBAS
      REAL      BASEN, VIS(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
      WRITE (MSGTXT,1010) FGVER
      CALL MSGWRT (2)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
      NUMVS1 = 0
      NUMVS2 = 0
      I = 4 * NI * NS * 2
      CALL FILL (I, 0, VISCNT)
      I = I * NA
      IF (OPTYPE(:2).EQ.'AN') CALL FILL (I, 0, ANTCNT)
C                                       Loop for before
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
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IXANT, NXANT, IXBAS, NXBAS))
     *      GO TO 100
         NUMVS1 = NUMVS1 + 1
C                                       call counting routine
         CALL FGCNTC (1, VIS, RPARM, IA1, IA2, NI, NA, NS, VISCNT,
     *      ANTCNT)
C                                       read next
         GO TO 100
         END IF
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       now after
      FGVER = XFLAG2 + 0.1
      WRITE (MSGTXT,1010) FGVER
      CALL MSGWRT (2)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Loop for before
C                                       Read vis. record.
 200  CALL UVGET ('READ', RPARM, VIS, IRET)
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
         IF (.NOT.REQBAS (IA1, IA2, DESEL, IXANT, NXANT, IXBAS, NXBAS))
     *      GO TO 200
         NUMVS2 = NUMVS2 + 1
C                                       call counting routine
         CALL FGCNTC (2, VIS, RPARM, IA1, IA2, NI, NA, NS, VISCNT,
     *      ANTCNT)
C                                       read next
         GO TO 200
         END IF
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGCNTU: ERROR',I3,' OPEN/INIT INPUT VIS FILE')
 1010 FORMAT ('Start reading with flag table version',I5)
 1100 FORMAT ('FGCNTU: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE FGCNTH (OPTYPE, NI, NA, NS, VISCNT, ANTCNT, POLTYP)
C-----------------------------------------------------------------------
C   FGCNTH prints the results
C-----------------------------------------------------------------------
      CHARACTER OPTYPE*4
      INTEGER   NI, NA, NS, VISCNT(4,NI,NS,2), ANTCNT(4,NI,NA,NS,2),
     *   POLTYP
C
      INTEGER   LP, LS, LI, JI, LA, N1, N2, J, JTRIM
      REAL      FRAC
      LOGICAL   FIRST
      CHARACTER POLLAB(4,4)*2
      INCLUDE 'FGCNTSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA POLLAB /'RR','LL','RL','LR', 'XX','YY','XY','YX',
     *   'I','Q','U','V', '1','2','3','4'/
C-----------------------------------------------------------------------
C                                       sum over antennas
      IF (OPTYPE(:2).NE.'AN') THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (5)
         DO 40 LS = 1,NUMSOU
            J = JTRIM (SUNAMS(LS))
            DO 30 LP = 1,4
               FIRST = .TRUE.
               DO 20 LI = BIF,EIF
                  JI = LI - BIF + 1
                  IF ((VISCNT(LP,JI,LS,1).GT.0) .OR.
     *               (VISCNT(LP,JI,LS,2).GT.0)) THEN
                     FRAC = (VISCNT(LP,JI,LS,1) - VISCNT(LP,JI,LS,2))
                     FRAC = (100.0 * FRAC) / VISCNT(LP,JI,LS,1)
                     IF (FIRST) THEN
                        WRITE (MSGTXT,1010) SUNUMS(LS), SUNAMS(LS)(:J),
     *                     POLLAB(LP,POLTYP)
                        CALL MSGWRT (5)
                        FIRST = .FALSE.
                        END IF
                     WRITE (MSGTXT,1020) LI, VISCNT(LP,JI,LS,1),
     *                  VISCNT(LP,JI,LS,2), FRAC
                     CALL MSGWRT (5)
                     END IF
 20               CONTINUE
 30            CONTINUE
 40        CONTINUE
C                                       sum over IFS
      ELSE IF (OPTYPE.EQ.'ANTE') THEN
         WRITE (MSGTXT,1100)
         CALL MSGWRT (5)
         DO 140 LS = 1,NUMSOU
            J = JTRIM (SUNAMS(LS))
            DO 130 LA = 1,NA
               FIRST = .TRUE.
               DO 120 LP = 1,4
                  N1 = 0
                  N2 = 0
                  DO 110 JI = 1,NI
                     N1 = N1 + ANTCNT(LP,JI,LA,LS,1)
                     N2 = N2 + ANTCNT(LP,JI,LA,LS,2)
 110                 CONTINUE
                  IF (N1.GT.0) THEN
                     FRAC = (100.0 * (N1 - N2)) / N1
                     IF (FIRST) THEN
                        WRITE (MSGTXT,1110) SUNUMS(LS), SUNAMS(LS)(:J),
     *                     LA, BIF, EIF
                        CALL MSGWRT (5)
                        FIRST = .FALSE.
                        END IF
                     WRITE (MSGTXT,1120) POLLAB(LP,POLTYP), N1, N2, FRAC
                     CALL MSGWRT (5)
                     END IF
 120              CONTINUE
 130           CONTINUE
 140       CONTINUE
      ELSE IF (OPTYPE.EQ.'ANIF') THEN
         WRITE (MSGTXT,1000)
         CALL MSGWRT (5)
         DO 250 LS = 1,NUMSOU
            J = JTRIM (SUNAMS(LS))
            DO 240 LA = 1,NA
               DO 230 LP = 1,4
                  FIRST = .TRUE.
                  DO 220 LI = BIF,EIF
                     JI = LI - BIF + 1
                     IF (ANTCNT(LP,JI,LA,LS,1).GT.0) THEN
                        FRAC = ANTCNT(LP,JI,LA,LS,1) -
     *                     ANTCNT(LP,JI,LA,LS,2)
                        FRAC = (100.0 * FRAC) / ANTCNT(LP,JI,LA,LS,1)
                        IF (FIRST) THEN
                           WRITE (MSGTXT,1210) SUNUMS(LS),
     *                        SUNAMS(LS)(:J), LA, POLLAB(LP,POLTYP)
                           CALL MSGWRT (5)
                           FIRST = .FALSE.
                           END IF
                        WRITE (MSGTXT,1020) LI, ANTCNT(LP,JI,LA,LS,1),
     *                     ANTCNT(LP,JI,LA,LS,2), FRAC
                        CALL MSGWRT (5)
                        END IF
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
 250        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (21X,'IF   Samp before    Samp after  Percent')
 1010 FORMAT ('Source',I4,'  ''',A,'''  polarization ',A2)
 1020 FORMAT (20X,I3,2I14,F9.2)
 1100 FORMAT (21X,'Pol   Samp before    Samp after  Percent')
 1110 FORMAT ('Source',I4,'  ''',A,'''  antenna',I3,'  IFs',I3,' -',I3)
 1120 FORMAT (21X,A2,2I14,F9.2)
 1210 FORMAT ('Source',I4,'  ''',A,'''  antenna',I3,'  polarization ',
     *   A2)
      END
      SUBROUTINE FGCNTC (ITYP, VIS, RPARM, IA1, IA2, NI, NA, NS, VISCNT,
     *   ANTCNT)
C-----------------------------------------------------------------------
C   counts visibilities by source, IF, polarization
C   Inputs:
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C   Output:
C      counting common update
C-----------------------------------------------------------------------
      INTEGER   ITYP, IA1, IA2, NI, NA, NS, VISCNT(4,NI,NS,2),
     *   ANTCNT(4,NI,NA,NS,2)
      REAL      VIS(3,*), RPARM(*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NST, I, JSRC, INDEXI
      INCLUDE 'FGCNT.INC'
      INCLUDE 'FGCNTSOU.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       which source
      JSRC = INSNUM
      IF (ILOCSU.GE.0) THEN
         JS = RPARM(1+ILOCSU) + 0.01
         DO 10 I = 1,NUMSOU
            IF (JS.EQ.SUNUMS(I)) JSRC = I
 10         CONTINUE
         END IF
C                                       pointers to traverse the data
      NST = 1
      NIF = 1
      NF = 1
      IF (JLOCS.GE.0) NST = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
      DO 40 JIF = 1,NIF
         DO 30 JF = 1,NF
            DO 20 JS = 1,NST
               INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *            (JS-1) * INCSI + 1
               IF (VIS(3,INDEXI).GT.0.0) THEN
                  VISCNT(JS,JIF,JSRC,ITYP) = VISCNT(JS,JIF,JSRC,ITYP)+1
                  IF (OPTYPE(:2).EQ.'AN') THEN
                     ANTCNT(JS,JIF,IA1,JSRC,ITYP) =
     *                  ANTCNT(JS,JIF,IA1,JSRC,ITYP) + 1
                     IF (IA1.NE.IA2) ANTCNT(JS,JIF,IA2,JSRC,ITYP) =
     *                  ANTCNT(JS,JIF,IA2,JSRC,ITYP) + 1
                     END IF
                  END IF
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
