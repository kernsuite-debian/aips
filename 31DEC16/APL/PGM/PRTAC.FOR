      PROGRAM PRTAC
C-----------------------------------------------------------------------
C! prints lists and summaries of AIPS ACcount file
C# History Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2003-2004, 2007, 2009, 2011-2012
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
C   PRTAC prints lists and summaries of the AIPS ACcount file as task or
C   as stand alone program.  In the latter, the aips manager may list 1
C   or more computers.
C-----------------------------------------------------------------------
      INTEGER   MAXE, MAXH
      PARAMETER (MAXE=600)
      PARAMETER (MAXH=200)
C
      CHARACTER MSGBUF*80, PRGNAM*6, TITL1*132, TITL2*132, CLINE*132,
     *   SCRTCH*132, CTIME*8, CDATE*12, BTIME*8, BDATE*12, PHNAME*48,
     *   CHTMP*4, CHARS(3)*4, ITASK*6, TTASK*6, ICHAR*2, OPTYPE*4,
     *   OPCODE(6)*4, ITATAB(MAXE)*6, LPNAME*48, LVCH(5)*3, LVERSN*48,
     *   AVERSN*48, MEMBER*16, JBUF*80, HOST*16, SITE*8, TSITE*8,
     *   THOST*16, HOSTS(MAXH)*16, LFILE*24, ALINE*80, FNAME*48
      HOLLERITH XXTASK(2), XOPTYP, XLPNAM(12)
      INTEGER  BUFFER(256), IPOPS(2), IUSER(2), IT0(6), IERR, ALUN,
     *   AIND, PLUN, PIND, NOP, NWPL, NUSER, NTASK, LINE, IPAGE, NLPR,
     *   NP, IP, IU, IT(6), JA, I, J, NPMAX, TTYLUN, TTYFND, IOP, JJ,
     *   NPARM, IRET, OTYPE, NACROS, INACT, NPOTAB(4,5,16), I4CPU,
     *   IUSTAB(MAXE), NUSTAB(4,5,MAXE), NTATAB(2,5,MAXE), IJOB, LJOB,
     *   LREC, NREC, IOCNT, NABORT(5), I4TR, I4TC, I4AR, I4AC, I4IOC,
     *   TTY(2), LV, NJOB(5), JTRIM, IROUND, ITRE, LMUSER, LUSER(100),
     *   LVM, LLV, RUNLUN, RUNIND, STUSER(100), SMUSER, ISUSER(2), JT1,
     *   JT2, ITRIM, SLNB, NHOST, LHOST, KBP, NHOSTT(MAXH), OBUF(256),
     *   OREC, LOREC, OJOB, ONP, JT
      REAL     POPTAB(4,5,16), USETAB(4,5,MAXE), TASTAB(3,5,MAXE), TCP,
     *   CTAST(5,MAXE), RBUFF(256), CPULIM, TOTCPU(5), TOTREA(5),
     *   TOTCPR(5), TRE, XID, DOCRT, PRTIME, DOALL
      DOUBLE PRECISION   CUSET(5,MAXE), TOTCNT(5), JD, JD0, DPRTIM, DX,
     *   JDE, JDMIN
      LOGICAL   TASK, T, F, APRINT, UPRINT, PPRINT, TPRINT, WASABO,
     *   ISTASK, RQUICK, LPOPN, DOVERS, AOPEN, OPENLP, DOHOST, DOALLH,
     *   ASKHST
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /INPARM/ XID, XXTASK, PRTIME, XOPTYP, DOCRT, DOALL, XLPNAM
      EQUIVALENCE (RBUFF, BUFFER), (TTY(1), TTYLUN), (TTY(2), TTYFND)
      DATA TTYLUN, RUNLUN /5, 10/
      DATA PRGNAM /'PRTAC '/
      DATA T, F /.TRUE.,.FALSE./
      DATA ALUN, NOP /13, 5/
      DATA CHARS /'AIPS','TASK','USER'/
      DATA OPCODE /'ROPT','SOPT','GO  ','INIT','QUIT', 'HOST'/
      DATA LVCH /'???','OLD','NEW','TST','CVX'/
C-----------------------------------------------------------------------
      AOPEN = .FALSE.
      OPENLP = .FALSE.
      DOHOST = .FALSE.
      DOALLH = .FALSE.
C                                       AIPS init
      NPOPS = 1
      MSGCNT = -1
      NLUSER = 1
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      NPMAX = NINTRN
      IF (NBATQS.GT.0) NPMAX = NINTRN + 1 + NBATQS
      NPMAX = MIN (15, NPMAX)
      NPARM = 19
      IRET = 0
      ISTASK = .FALSE.
      LPOPN = .FALSE.
C                                       Are we a task?
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XID, BUFFER, IERR)
      IF (IERR.EQ.0) THEN
         CALL H2CHR (8, 1, XXTASK, ITASK)
         CALL H2CHR (4, 1, XOPTYP, OPTYPE)
         CALL H2CHR (48, 1, XLPNAM, LPNAME)
         ISTASK = .TRUE.
         IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) DOCRT =
     *      MIN (-1.0, DOCRT)
         IF (DOCRT.GT.0.0) RQUICK = .FALSE.
         IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
         IRET = 8
      ELSE
         NPOPS = 1
         NLUSER = 1
         RQUICK = .TRUE.
         DOCRT = -1.0
         LPNAME = ' '
         OPTYPE = ' '
         END IF
C                                       Open terminal
 20   IF ((ISTASK) .AND. (DOCRT.LE.0.0)) GO TO 25
         CALL ZOPEN (TTYLUN, TTYFND, 1, FNAME, F, F, T, IERR)
         IF (IERR.EQ.0) GO TO 25
            WRITE (MSGTXT,1020) IERR
            CALL MSGWRT (8)
            GO TO 990
C                                       Open AC file
 25   CALL ZPHFIL ('AC', 1, 0, 0, PHNAME, IERR)
      CALL ZOPEN (ALUN, AIND, 1, PHNAME, F, T, F, IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (8)
         GO TO 990
 30   AOPEN = .TRUE.
      PLUN = TTYLUN
      PIND = TTYFND
C                                       read in STAFF user numbers
      ISUSER(1) = 1
      ISUSER(2) = 46655
      SMUSER = 0
      IF (.NOT.ISTASK) THEN
         MEMBER = 'STAFF'
         LVERSN = VERNAM(:3)
         CALL ZPHFIL ('RU', 1, 0, 0, FNAME, IERR)
         CALL VERMAT (1, FNAME, MEMBER, LVERSN, I, AVERSN, IERR)
         IF (IERR.NE.0) GO TO 45
         CALL ZTOPEN (RUNLUN, RUNIND, 1, FNAME, MEMBER, LVERSN, T,
     *      IERR)
         IF (IERR.NE.0) GO TO 45
         CALL ZTREAD (RUNLUN, RUNIND, JBUF, IERR)
         IF (IERR.NE.0) GO TO 40
 35      CALL ZTREAD (RUNLUN, RUNIND, JBUF, IERR)
            IF (IERR.NE.0) GO TO 40
            IF ((JBUF(:1).EQ.';') .OR. (JBUF(:1).EQ.'*')) GO TO 35
            I = 1
            CALL GETNUM (JBUF, 72, I, DX)
            IF (DX.EQ.DBLANK) GO TO 40
            I = DX + 0.5D0
            IF (I.GT.0) THEN
               SMUSER = SMUSER + 1
               STUSER(SMUSER) = I
               IF (SMUSER.EQ.1) THEN
                  ISUSER(1) = I
                  ISUSER(2) = I
               ELSE
                  ISUSER(1) = MIN (ISUSER(1), I)
                  ISUSER(2) = MAX (ISUSER(2), I)
                  END IF
               END IF
            GO TO 35
 40      CALL ZTCLOS (RUNLUN, RUNIND, IERR)
         END IF
C-----------------------------------------------------------------------
C                                       Options init
 45   IPOPS(1) = 1
      IPOPS(2) = NPMAX
      IUSER(1) = 1
      IUSER(2) = 46655
      CALL FILL (6, 0, IT0)
      CPULIM = -1.E5
      APRINT = F
      UPRINT = T
      PPRINT = T
      TPRINT = T
      DOVERS = T
C                                       Task: use parms, skip options
      IF (ISTASK) THEN
         DOVERS = DOALL.GT.0.0
         I = ABS(XID) + 0.01
         APRINT = I.NE.32000
         IF (APRINT) THEN
            IF (I.EQ.0) I = NLUSER
            IUSER(1) = I
            IUSER(2) = I
            END IF
         GO TO 200
         END IF
C                                       Get opcode from user
 50   WRITE (MSGBUF,1050) OPCODE
      IF (.NOT.DOHOST) MSGBUF(30:33) = ' '
      IF (DOALLH) WRITE (MSGBUF,1051) OPCODE(3), OPCODE(4), OPCODE(5)
      CALL INQSTR (TTY, MSGBUF, 4, CHTMP, IERR)
      IF (IERR.NE.0) GO TO 890
      CALL CHLTOU (4, CHTMP)
      DO 55 IOP = 1,NOP
         IF (CHTMP.EQ.OPCODE(IOP)) GO TO 60
 55      CONTINUE
C                                       New host name
      IF ((DOHOST) .AND. (.NOT.DOALLH) .AND. (CHTMP.EQ.'HOST')) THEN
 56      WRITE (MSGBUF,1165)
         CALL INQSTR (TTY, MSGBUF, 16, HOST, IERR)
         IF (IERR.NE.0) GO TO 890
         CALL CHLTOU (16, HOST)
         IF ((HOST.EQ.' ') .OR. (HOST.EQ.'ALL ')) GO TO 56
         END IF
      GO TO 50
C                                       Branch to OP
 60   GO TO (45, 100, 200, 400, 900), IOP
C-----------------------------------------------------------------------
C                                       Set options
 100  WRITE (MSGBUF,1100)
      CALL INQSTR (TTY, MSGBUF, 6, ITASK, IERR)
      IF (IERR.NE.0) GO TO 890
      CALL CHLTOU (6, ITASK)
      WRITE (MSGBUF,1110)
      CALL INQINT (TTY, MSGBUF, 2, IPOPS, IERR)
      IF (IERR.NE.0) GO TO 890
      IUSER(1) = 1
      IUSER(2) = 46655
      DO 125 I = 1,99
         IF (I.EQ.1) THEN
            WRITE (MSGBUF,1120) I
         ELSE
            WRITE (MSGBUF,1121) I
            END IF
         CALL INQINT (TTY, MSGBUF, 1, LUSER(I), IERR)
         IF (IERR.NE.0) GO TO 890
         IF (LUSER(I).LE.0) GO TO 130
         LMUSER = I
         IF (I.EQ.1) THEN
            IUSER(1) = LUSER(I)
            IUSER(2) = LUSER(I)
         ELSE
            IUSER(1) = MIN (IUSER(1), LUSER(I))
            IUSER(2) = MAX (IUSER(2), LUSER(I))
            END IF
 125     CONTINUE
 130  IF ((LUSER(I).EQ.-999) .AND. (SMUSER.GT.0)) THEN
         LMUSER = SMUSER
         CALL COPY (2, ISUSER, IUSER)
         CALL COPY (LMUSER, STUSER, LUSER)
         END IF
      WRITE (MSGBUF,1130)
      CALL INQINT (TTY, MSGBUF, 6, IT0, IERR)
      IF (IERR.NE.0) GO TO 890
      WRITE (MSGBUF,1140)
      CALL INQFLT (TTY, MSGBUF, 1, DX, IERR)
      IF (IERR.NE.0) GO TO 890
      CPULIM = DX
      WRITE (MSGBUF,1150)
      CALL ZTTYIO ('WRIT', TTYLUN, TTYFND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 890
      CALL ZTTYIO ('READ', TTYLUN, TTYFND, 72, MSGBUF, IERR)
      IF (IERR.NE.0) GO TO 890
      CALL CHLTOU (8, MSGBUF)
      READ (MSGBUF,1151) APRINT, UPRINT, PPRINT, TPRINT, DOVERS
      IF (IPOPS(1).LT.1) IPOPS(1) = 1
      IF (IPOPS(2).LT.IPOPS(1)) IPOPS(2) = NPMAX
      IF (IUSER(1).LT.1) IUSER(1) = 1
      IF (IUSER(2).LT.IUSER(1)) IUSER(2) = 46655
      IPOPS(2) = MIN (NPMAX, IPOPS(2))
      IF (.NOT.LPOPN) THEN
         WRITE (MSGBUF,1160)
         CALL INQSTR (TTY, MSGBUF, 48, LPNAME, IERR)
         IF (IERR.NE.0) GO TO 890
         CALL CHLTOU (48, LPNAME)
         END IF
C                                       Other hosts?
      WRITE (MSGBUF,1165)
      CALL INQSTR (TTY, MSGBUF, 16, HOST, IERR)
      IF (IERR.NE.0) GO TO 890
      CALL CHLTOU (16, HOST)
      DOHOST = HOST.NE.' '
      IF (DOHOST) THEN
         CALL PASWRD (BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         DOALLH = HOST.EQ.'ALL '
C                                       all hosts of a site (summed)
         IF (DOALLH) THEN
            CALL ZTRLOG (4, 'SITE', 8, SITE, SLNB, IERR)
            IF (IERR.NE.0) GO TO 900
C                                       Read hosts into list
            LFILE = 'AIPS_ROOT:HOSTS.LIST'
            CALL ZTXOPN ('READ', RUNLUN, RUNIND, LFILE, .FALSE., IERR)
            IF (IERR.NE.0) GO TO 999
            NHOST = 0
C                                       Read loop
 150        CALL ZTXIO ('READ', RUNLUN, RUNIND, ALINE, IERR)
            IF (IERR.EQ.0) THEN
               JT = JTRIM (ALINE)
C                                       get Host name and site
               IF (ALINE(1:1).EQ.'+') THEN
                  KBP = 4
                  CALL NXTSTR (ALINE, 80, KBP, THOST, I)
                  CALL NXTSTR (ALINE, 80, KBP, TSITE, I)
                  CALL NXTSTR (ALINE, 80, KBP, TSITE, I)
                  CALL CHLTOU (16, THOST)
                  CALL CHLTOU (8, TSITE)
                  IF (TSITE.EQ.SITE) THEN
                     NHOST = NHOST + 1
                     HOSTS(NHOST) = THOST
                     END IF
                  END IF
               GO TO 150
C                                       I/O error
            ELSE IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1030) LFILE, IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       end reading HOSTS.LIST
            CALL ZTXCLS (RUNLUN, RUNIND, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
      GO TO 50
C-----------------------------------------------------------------------
C                                       GO: init counters
 200  NUSER = 0
      CALL ZTIME (IT(4))
      CALL ZDATE (IT(1))
      CALL DAT2JD (IT, JD)
      DPRTIM = PRTIME
      JDE = JD + 100.0D0
      JDMIN = JDE
      ASKHST = .TRUE.
      IF (ISTASK) THEN
         IF (DPRTIM.LT.0.00069) DPRTIM = 3650.
         IF ((OPTYPE.EQ.'DDT ') .OR. (OPTYPE(:3).EQ.'Y2K') .OR.
     *      (OPTYPE.EQ.'CYG ')) THEN
            CALL FNDDDT (OPTYPE, ALUN, AIND, JD, DPRTIM, JDE, IPOPS,
     *         IUSER, IERR)
            IF (IERR.NE.0) GO TO 910
            END IF
         JD0 = JD - DPRTIM
         CALL JD2DAT (JD0, IT0)
      ELSE
         IF (IT0(1).LE.0) IT0(1) = 1900
         CALL DAT2JD (IT0, JD0)
         END IF
      INACT = JTRIM (ITASK)
      TASK = INACT.GT.0
C                                       Open line printer
      IF (.NOT.LPOPN) THEN
         LPOPN = .TRUE.
         IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
         CALL LPOPEN (LPNAME, DOCRT, PLUN, PIND, NACROS, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            CALL MSGWRT (8)
            GO TO 910
            END IF
         OPENLP = .TRUE.
         END IF
      NTASK = 0
      CALL FILL (5, 0, NJOB)
      CALL FILL (5, 0, NABORT)
      CALL RFILL (5, 0.0, TOTCPU)
      CALL RFILL (5, 0.0, TOTCPR)
      CALL RFILL (5, 0.0, TOTCNT)
      TOTCNT(1) = 0.0D0
      TOTCNT(2) = 0.0D0
      TOTCNT(3) = 0.0D0
      TOTCNT(4) = 0.0D0
      TOTCNT(5) = 0.0D0
      CALL RFILL (320, 0.0, POPTAB)
      CALL FILL (320, 0, NPOTAB)
      LINE = 900
      IPAGE = 0
      LREC = 1
      OTYPE = 1
      IF (NACROS.GE.80) OTYPE = 2
      IF (NACROS.GE.90) OTYPE = 3
      LHOST = 1
      IF (DOALLH) LHOST = 0
C                                       open desired host
 210  IF (DOHOST) THEN
         IF (AOPEN) THEN
            CALL ZCLOSE (ALUN, AIND, IERR)
            AOPEN = .FALSE.
            END IF
         IF (DOALLH) THEN
            LHOST = LHOST + 1
            IF (LHOST.GT.NHOST) GO TO 275
            HOST = HOSTS(LHOST)
            NHOSTT(LHOST) = 0
            IF (ASKHST) THEN
               MSGBUF = 'Do host ' // HOST(:ITRIM(HOST)) // ' ? [y]'
               CALL INQSTR (TTY, MSGBUF, 16, THOST, IERR)
               IF (IERR.NE.0) GO TO 990
            ELSE
               MSGTXT = 'Doing host ' // HOST(:ITRIM(HOST))
               CALL MSGWRT (1)
               END IF
            CALL CHLTOU (16, THOST)
            IF (THOST(:1).EQ.'A') ASKHST = .FALSE.
            IF (THOST(:1).EQ.'N') GO TO 210
            END IF
         PHNAME = 'NET0:' // HOST(:ITRIM(HOST)) // '/ACD000000;'
         CALL ZOPEN (ALUN, AIND, 1, PHNAME, F, T, F, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1025) IERR
            CALL MSGWRT (8)
            IF (DOALLH) GO TO 210
            GO TO 990
            END IF
         AOPEN = .TRUE.
      ELSE
         HOST = HSTNAM
         END IF
      HSTNAM = HOST
C                                       Read rec 1 of control parms
      CALL ZFIO ('READ', ALUN, AIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      LREC = 1
      LJOB = BUFFER(1)
      NLPR = BUFFER(4)
      NWPL = BUFFER(3)
      IF (NACROS.GE.80) THEN
         WRITE (TITL1,1210)
         WRITE (TITL2,1212)
      ELSE
         WRITE (TITL1,1211)
         WRITE (TITL2,1213)
         END IF
      IF ((APRINT) .AND. (DOCRT.LE.-2.5)) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      LINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      LINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
C                                       Loop for all jobs
      DO 270 IJOB = 1,LJOB
         NREC = IJOB / NLPR + 1
         IF (NREC.NE.LREC) THEN
            LREC = NREC
            CALL ZFIO ('READ', ALUN, AIND, NREC, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1200) IERR
               CALL MSGWRT (8)
               IF (DOALLH) GO TO 210
               GO TO 275
               END IF
            END IF
         NP = (IJOB - (NREC-1)*NLPR) * NWPL + 1
         CALL CATIME (2, BUFFER(NP+4), IT)
C                                       Do we want it?
         CALL H2CHR (6, 1, BUFFER(NP), TTASK)
         IF (TASK) THEN
            IF (ITASK(1:INACT).NE.TTASK(1:INACT)) GO TO 270
            END IF
         IP = MOD (BUFFER(NP+2), 100)
         IF ((IP.LT.IPOPS(1)) .OR. (IP.GT.IPOPS(2))) GO TO 270
         IU = BUFFER(NP+3)
         IF ((IU.LT.IUSER(1)) .OR. (IU.GT.IUSER(2))) GO TO 270
         IF (LMUSER.GT.0) THEN
            DO 215 I = 1,LMUSER
               IF (IU.EQ.LUSER(I)) GO TO 220
 215           CONTINUE
            GO TO 270
            END IF
 220     IF (IT(1).LE.0) IT(1) = 1900
         CALL DAT2JD (IT, JD)
         IF ((JD.LT.JD0) .OR. (JD.GT.JDE)) GO TO 270
         TRE = RBUFF(NP+7)
         TCP = RBUFF(NP+8)
         IOCNT = BUFFER(NP+6)
         IF (ABS(TCP).LT.CPULIM) GO TO 270
C                                       version code
         LV = BUFFER(NP+2) / 100 + 1
         IF ((LV.LT.1) .OR. (LV.GT.5)) LV = 1
         LLV = LV
         IF (.NOT.DOVERS) LV = 1
C                                       We want it: global sums
         JDMIN = MIN (JDMIN, JD)
         NHOSTT(LHOST) = NHOSTT(LHOST) + 1
         IF ((TRE.EQ.0.0) .AND. (ABS(TCP).LE.1.)) TRE = TCP * 1.1
         IF (TRE.EQ.0.0) TCP = 0.0
         WASABO = (TRE.LE.0.0) .OR. (TCP.LE.0.0)
         TCP = ABS(TCP)
         TRE = ABS(TRE)
         IF ((TRE.GT.1.E7) .OR. (TCP.GT.1.E7)) THEN
            IF (TRE.GT.1.E7) TRE = TCP
            IF (TCP.GT.1.E7) TCP = TRE
            IF (TRE.GT.1.E6) TRE = 1.0
            IF (TCP.GT.1.E6) TCP = 1.0
            END IF
         NJOB(LV) = NJOB(LV) + 1
         TOTCPU(LV) = TOTCPU(LV) + TCP
         TOTREA(LV) = TOTREA(LV) + TRE
         TOTCNT(LV) = TOTCNT(LV) + IOCNT
         IF (WASABO) THEN
            NABORT(LV) = NABORT(LV) + 1
         ELSE
            TOTCPR(LV) = TOTCPR(LV) + TCP/TRE
            END IF
C                                       Print each entry
         IF (APRINT) THEN
            ICHAR = ' '
            IF (WASABO) ICHAR = '**'
            ITRE = IROUND (TRE)
            CALL TIMDAT (IT(4), IT(1), BTIME, BDATE)
            IF (NACROS.GE.80) THEN
               WRITE (CLINE,1220) TTASK, IP, IU, BDATE, BTIME, ITRE,
     *            TCP, IOCNT, LVCH(LLV), ICHAR
            ELSE
               WRITE (CLINE,1221) TTASK, IP, IU, BDATE, BTIME, ITRE,
     *            TCP, IOCNT, LVCH(LLV), ICHAR
               END IF
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE,
     *         LINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
C                                       Accumulate by POPS number
         JA = 0
         IF (TTASK(1:4).EQ.'AIPS') JA = 2
         IF (TTASK(1:6).EQ.'QMNGR ') JA = 2
         TRE = TRE / 60.0
         TCP = TCP / 60.0
         IF ((.NOT.PPRINT) .OR. (IP.GT.15)) GO TO 230
            POPTAB(JA+1,LV,IP) = POPTAB(JA+1,LV,IP) + TRE
            POPTAB(JA+2,LV,IP) = POPTAB(JA+2,LV,IP) + TCP
            J = JA/2 + 1
            IF (WASABO) NPOTAB(J+2,LV,IP) = NPOTAB(J+2,LV,IP) + 1
            NPOTAB(J,LV,IP) = NPOTAB(J,LV,IP) + 1
C                                       Accumulate by user #
 230     IF (.NOT.UPRINT) GO TO 250
C                                       find in current table
            IF (NUSER.EQ.0) GO TO 240
               DO 235 J = 1,NUSER
                  IF (IU.EQ.IUSTAB(J)) GO TO 245
 235              CONTINUE
C                                       add one to table
 240        IF (NUSER.GE.MAXE) GO TO 250
               NUSER = NUSER + 1
               J = NUSER
               IUSTAB(J) = IU
               CALL FILL (16, 0, NUSTAB(1,1,J))
               CALL RFILL (16, 0.0, USETAB(1,1,J))
               CUSET(1,J) = 0.0D0
               CUSET(2,J) = 0.0D0
               CUSET(3,J) = 0.0D0
               CUSET(4,J) = 0.0D0
C                                       add it in
 245        USETAB(1+JA,LV,J) = USETAB(1+JA,LV,J) + TRE
            USETAB(2+JA,LV,J) = USETAB(2+JA,LV,J) + TCP
            CUSET(LV,J) = CUSET(LV,J) + IOCNT
            JA = JA/2 + 1
            IF (WASABO) NUSTAB(JA+2,LV,J) = NUSTAB(JA+2,LV,J) + 1
            NUSTAB(JA,LV,J) = NUSTAB(JA,LV,J) + 1
C                                       Accumulate by task name
 250     IF (.NOT.TPRINT) GO TO 270
            IF (NTASK.GT.0) THEN
               DO 255 J = 1,NTASK
                  IF (ITATAB(J).EQ.TTASK) GO TO 265
 255              CONTINUE
               END IF
C                                       add to table
            IF (NTASK.GE.MAXE) GO TO 270
               NTASK = NTASK + 1
               ITATAB(NTASK) = TTASK
               CALL FILL (8, 0, NTATAB(1,1,NTASK))
               CALL RFILL (12, 0.0, TASTAB(1,1,NTASK))
               CALL RFILL (4, 0.0, CTAST(1,NTASK))
               J = NTASK
 265        NTATAB(1,LV,J) = NTATAB(1,LV,J) + 1
            TASTAB(1,LV,J) = TASTAB(1,LV,J) + TRE
            TASTAB(2,LV,J) = TASTAB(2,LV,J) + TCP
            IF (WASABO) THEN
               NTATAB(2,LV,J) = NTATAB(2,LV,J) + 1
            ELSE
               TASTAB(3,LV,J) = TASTAB(3,LV,J) + TCP/TRE
               CTAST(LV,J) = CTAST(LV,J) + IOCNT/TCP/60.0
               END IF
 270     CONTINUE
      IF ((DOALLH) .AND. (LHOST.LT.NHOST)) GO TO 210
C                                       Global summary
 275  LINE = 900
      CLINE = '  Version:'
      IF (DOVERS) THEN
         LVM = 5
      ELSE
         LVM = 1
         CLINE(20:22) = 'ALL'
         END IF
      DO 276 LV = 1,LVM
         IF (NJOB(LV).GT.NABORT(LV)) THEN
            TOTCPR(LV) = TOTCPR(LV) / (NJOB(LV) - NABORT(LV))
         ELSE
            TOTCPR(LV) = 10.0
            END IF
         TOTREA(LV) = TOTREA(LV) / 60.0
         TOTCPU(LV) = TOTCPU(LV) / 60.0
         IF (LVM.GT.1) CLINE(12*LV+8:12*LV+10) = LVCH(LV)
 276     CONTINUE
      CALL JD2DAT (JDMIN, IT)
      CALL TIMDAT (IT(4), IT(1), CTIME, CDATE)
      IF (DOALLH) THEN
         JT2 = JTRIM (SITE)
         WRITE (TITL1,1275) CDATE, CTIME, 'All hosts at', SITE(:JT2)
      ELSE
         JT1 = JTRIM (HOST)
         JT2 = JTRIM (SYSNAM)
         WRITE (TITL1,1275) CDATE, CTIME, HOST(:JT1), SYSNAM(:JT2)
         END IF
      TITL2 = ' '
      IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      LINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1276) (NJOB(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1277) (TOTREA(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1278) (TOTCPU(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1279) (TOTCNT(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1280) (TOTCPR(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1281) (NABORT(LV), LV = 1,LVM)
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1282) NUSER
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1283) NTASK
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (DOCRT.GT.-2.5) THEN
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *      LINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (DOCRT.LE.0.0) THEN
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, TITL2,
     *         LINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       Summary of limits
      WRITE (CLINE,1284) IPOPS
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (LMUSER.GT.0) THEN
         JA = 1
 285     JJ = MIN (JA+7, LMUSER)
            WRITE (CLINE,1285) (LUSER(J), J = JA,JJ)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE,
     *         LINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            JA = JJ + 1
            IF (JA.LE.LMUSER) GO TO 285
         END IF
      WRITE (CLINE,1286) IT0
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      WRITE (CLINE,1287) CPULIM
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (TASK) THEN
         WRITE (CLINE,1288) ITASK
      ELSE
         WRITE (CLINE,1289)
         END IF
      CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE, LINE,
     *   IPAGE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       ALL hosts list
      IF (DOALLH) THEN
         LINE = 900
         CLINE = 'List of total tasks/AIPS by host'
         CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE,
     *      LINE, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         DO 290 LV = 1,NHOST,3
            JA = MIN (LV+2, NHOST)
            WRITE (CLINE,1290) (HOSTS(I), NHOSTT(I), I = LV,JA)
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE,
     *         LINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
 290        CONTINUE
         END IF
C                                       Print by POPS number
 300  IF (PPRINT) THEN
         DO 320 LV = 1,LVM
            IF (NJOB(LV).LE.0) GO TO 320
            LINE = 900
            CHTMP = LVCH(LV)
            IF (LVM.LE.1) CHTMP = 'ALL'
            WRITE (TITL1,1300) CHARS(1), CHARS(2), CHARS(1)
            WRITE (TITL2,1301)
            IF (NACROS.GT.72) TITL1(78:80) = CHTMP(1:3)
            IF (NACROS.GE.88) TITL1(82:88) = 'VERSION'
            IF (DOCRT.LE.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL1, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            DO 310 I = 1,NPMAX
               I4CPU = POPTAB(2,LV,I) + POPTAB(4,LV,I) + 0.5
               I4TR = POPTAB(1,LV,I) + 0.5
               I4TC = POPTAB(2,LV,I) + 0.5
               I4AR = POPTAB(3,LV,I) + 0.5
               I4AC = POPTAB(4,LV,I) + 0.5
               WRITE (CLINE,1305) I, NPOTAB(1,LV,I), I4TR, I4TC,
     *            NPOTAB(3,LV,I), NPOTAB(2,LV,I), I4AR, I4AC,
     *            NPOTAB(4,LV,I), I4CPU
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            CLINE, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               DO 310 J = 1,4
                  NPOTAB(J,LV,16) = NPOTAB(J,LV,16) + NPOTAB(J,LV,I)
                  POPTAB(J,LV,16) = POPTAB(J,LV,16) + POPTAB(J,LV,I)
 310           CONTINUE
            I = 16
            I4CPU = POPTAB(2,LV,I) + POPTAB(4,LV,I) + 0.5
            I4TR = POPTAB(1,LV,I) + 0.5
            I4TC = POPTAB(2,LV,I) + 0.5
            I4AR = POPTAB(3,LV,I) + 0.5
            I4AC = POPTAB(4,LV,I) + 0.5
            WRITE (CLINE,1310) NPOTAB(1,LV,I), I4TR, I4TC,
     *          NPOTAB(3,LV,I), NPOTAB(2,LV,I), I4AR, I4AC,
     *          NPOTAB(4,LV,I), I4CPU
            CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2, CLINE,
     *         LINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
 320        CONTINUE
         END IF
C                                       Print by user number
C                                       Sort in decreasing CPU use
      IF ((UPRINT) .AND. (NUSER.GT.0)) THEN
         DO 340 LV = 1,LVM
            IF (NJOB(LV).LE.0) GO TO 340
            LINE = 900
            CHTMP = LVCH(LV)
            IF (LVM.LE.1) CHTMP = 'ALL'
            IF (OTYPE.EQ.3) THEN
               WRITE (TITL1,1320) CHTMP, CHARS(3), CHARS(2), CHARS(1),
     *            ' '
               WRITE (TITL2,1322) ' '
            ELSE IF (OTYPE.EQ.2) THEN
               WRITE (TITL1,1320) CHTMP, CHARS(3), CHARS(2), CHARS(1)
               WRITE (TITL2,1322)
            ELSE
               WRITE (TITL1,1321) CHTMP, CHARS(3), CHARS(2), CHARS(1)
               WRITE (TITL2,1323)
               END IF
            IF (DOCRT.LE.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL1, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            DO 335 I = 1,NUSER
               TRE = -100000.0
               JJ = 0
               DO 325 J = 1,NUSER
                  IF (NUSTAB(1,LV,J)+NUSTAB(2,LV,J).LE.0) GO TO 325
                     TCP = USETAB(2,LV,J) + USETAB(4,LV,J)
                     IF (TCP.LE.TRE) GO TO 325
                        TRE = TCP
                        JJ = J
 325              CONTINUE
               IF (JJ.LE.0) GO TO 340
               I4CPU = TRE + 0.5
               I4TR = USETAB(1,LV,JJ) + 0.5
               I4TC = USETAB(2,LV,JJ) + 0.5
               I4AR = USETAB(3,LV,JJ) + 0.5
               I4AC = USETAB(4,LV,JJ) + 0.5
               I4IOC = CUSET(LV,JJ)
               IF (OTYPE.EQ.3) THEN
                  WRITE (CLINE,1325) I, IUSTAB(JJ), NUSTAB(1,LV,JJ),
     *               I4TR, USETAB(2,LV,JJ), NUSTAB(3,LV,JJ),
     *               NUSTAB(2,LV,JJ), I4AR, USETAB(4,LV,JJ),
     *               NUSTAB(4,LV,JJ), I4CPU, I4IOC
               ELSE IF (OTYPE.EQ.2) THEN
                  WRITE (CLINE,1325) I, IUSTAB(JJ), NUSTAB(1,LV,JJ),
     *               I4TR, USETAB(2,LV,JJ), NUSTAB(3,LV,JJ),
     *               NUSTAB(2,LV,JJ), I4AR, USETAB(4,LV,JJ),
     *               NUSTAB(4,LV,JJ), I4CPU
               ELSE
                  WRITE (CLINE,1326) I, IUSTAB(JJ), NUSTAB(1,LV,JJ),
     *               I4TR, I4TC, NUSTAB(3,LV,JJ), NUSTAB(2,LV,JJ),
     *               I4AR, I4AC, NUSTAB(4,LV,JJ), I4CPU
                  END IF
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            CLINE, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               NUSTAB(1,LV,JJ) = 0
               NUSTAB(2,LV,JJ) = 0
 335           CONTINUE
 340        CONTINUE
         END IF
C                                       Print by task name
C                                       Sort decreasing CPU
      IF ((TPRINT) .AND. (NTASK.GT.0)) THEN
         DO 390 LV = 1,LVM
            IF (NJOB(LV).LE.0) GO TO 390
            CHTMP = LVCH(LV)
            IF (LVM.LE.1) CHTMP = 'ALL'
            LINE = 900
            WRITE (TITL1,1340) CHTMP
            WRITE (TITL2,1341)
            CALL COPY (4800, NTATAB, NUSTAB)
            IF (DOCRT.LE.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL1, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            DO 355 I = 1,NTASK
               TRE = -10000.0
               JJ = 0
               DO 345 J = 1,NTASK
                  IF ((NTATAB(1,LV,J).LE.0) .OR.
     *               (TASTAB(2,LV,J).LE.TRE)) GO TO 345
                     JJ = J
                     TRE = TASTAB(2,LV,J)
 345              CONTINUE
               IF (JJ.LE.0) GO TO 360
               TCP = 10.0
               IOCNT = 0
               IF (NTATAB(1,LV,JJ).GT.NTATAB(2,LV,JJ)) TCP =
     *            TASTAB(3,LV,JJ) / (NTATAB(1,LV,JJ) - NTATAB(2,LV,JJ))
               IF (NTATAB(1,LV,JJ).GT.NTATAB(2,LV,JJ)) IOCNT =
     *            CTAST(LV,JJ) / (NTATAB(1,LV,JJ) - NTATAB(2,LV,JJ))
               WRITE (CLINE,1350) I, ITATAB(JJ), NTATAB(1,LV,JJ),
     *            TASTAB(1,LV,JJ), TRE, NTATAB(2,LV,JJ), TCP, IOCNT
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            CLINE, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               NTATAB(1,LV,JJ) = 0
 355           CONTINUE
C                                       Print by task name
C                                       Sort decreasing number
 360        LINE = 900
            CALL COPY (4800, NUSTAB, NTATAB)
            IF (DOCRT.LE.-2.5) THEN
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL1, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            TITL2, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
            DO 375 I = 1,NTASK
               TRE = 0.001
               JJ = 0
               DO 365 J = 1,NTASK
                  IF (NTATAB(1,LV,J).LE.TRE) GO TO 365
                     JJ = J
                     TRE = NTATAB(1,LV,J)
 365              CONTINUE
               IF (JJ.LE.0) GO TO 390
               TCP = 10.0
               IOCNT = 0
               IF (NTATAB(1,LV,JJ).GT.NTATAB(2,LV,JJ)) TCP =
     *            TASTAB(3,LV,JJ) / (NTATAB(1,LV,JJ) - NTATAB(2,LV,JJ))
               IF (NTATAB(1,LV,JJ).GT.NTATAB(2,LV,JJ)) IOCNT =
     *            CTAST(LV,JJ) / (NTATAB(1,LV,JJ) - NTATAB(2,LV,JJ))
               WRITE (CLINE,1350) I, ITATAB(JJ), NTATAB(1,LV,JJ),
     *            TASTAB(1,LV,JJ), TASTAB(2,LV,JJ), NTATAB(2,LV,JJ),
     *            TCP, IOCNT
               CALL PRTLIN (PLUN, PIND, DOCRT, NACROS, TITL1, TITL2,
     *            CLINE, LINE, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               NTATAB(1,LV,JJ) = 0
 375           CONTINUE
 390        CONTINUE
         END IF
C                                       loop or quit or init ?
      IF (.NOT.ISTASK) GO TO 50
      IF ((NLUSER.NE.1) .OR. (OPTYPE.NE.OPCODE(4))) GO TO 900
C-----------------------------------------------------------------------
C                                       INIT: clear file
C                                       Require password
 400  IF (.NOT.ISTASK) THEN
         CALL PASWRD (BUFFER, IERR)
         IF (IERR.NE.0) GO TO 900
         END IF
      LHOST = 0
      ASKHST = .TRUE.
      CALL ZTIME (IT(4))
      CALL ZDATE (IT(1))
      CALL DAT2JD (IT, JD)
      DPRTIM = PRTIME
      JDE = JD + 100.0D0
      IF (ISTASK) THEN
         IF (DPRTIM.LT.0.00069) DPRTIM = 3650.
         JD0 = JD - DPRTIM
      ELSE
         WRITE (MSGBUF,1400)
         CALL INQINT (TTY, MSGBUF, 6, IT0, IERR)
         IF (IERR.NE.0) GO TO 890
         IF (IT0(1).EQ.0) IT0(1) = 2050
         CALL DAT2JD (IT0, JD0)
         END IF
C                                       open desired host
 410  IF (DOHOST) THEN
         IF (AOPEN) THEN
            CALL ZCLOSE (ALUN, AIND, IERR)
            AOPEN = .FALSE.
            END IF
         IF (DOALLH) THEN
            LHOST = LHOST + 1
            IF (LHOST.GT.NHOST) GO TO 900
            HOST = HOSTS(LHOST)
            IF (ASKHST) THEN
               MSGBUF = 'Do host ' // HOST(:ITRIM(HOST)) // ' ? [y]'
               CALL INQSTR (TTY, MSGBUF, 16, THOST, IERR)
               IF (IERR.NE.0) GO TO 990
            ELSE
               MSGTXT = 'Doing host ' // HOST(:ITRIM(HOST))
               CALL MSGWRT (1)
               END IF
            CALL CHLTOU (16, THOST)
            IF (THOST(:1).EQ.'A') ASKHST = .FALSE.
            IF (THOST(:1).EQ.'N') GO TO 410
            END IF
         PHNAME = 'NET0:' // HOST(:ITRIM(HOST)) // '/ACD000000;'
         CALL ZOPEN (ALUN, AIND, 1, PHNAME, F, T, F, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1025) IERR
            CALL MSGWRT (8)
            IF (DOALLH) GO TO 410
            GO TO 990
            END IF
         AOPEN = .TRUE.
      ELSE
         HOST = HSTNAM
         END IF
      HSTNAM = HOST
C                                       do it
      CALL ZFIO ('READ', ALUN, AIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      LJOB = BUFFER(1)
      OJOB = 0
      LOREC = 1
      LREC = 1
      NLPR = BUFFER(4)
      NWPL = BUFFER(3)
      CALL COPY (256, BUFFER, OBUF)
      DO 430 IJOB = 1,LJOB
         NREC = IJOB / NLPR + 1
         IF (NREC.NE.LREC) THEN
            LREC = NREC
            CALL ZFIO ('READ', ALUN, AIND, NREC, BUFFER, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1200) IERR
               CALL MSGWRT (8)
               GO TO 900
               END IF
            END IF
         NP = (IJOB - (NREC-1)*NLPR) * NWPL + 1
         CALL CATIME (2, BUFFER(NP+4), IT)
         CALL DAT2JD (IT, JD)
C                                       keep this record
         IF (JD.GT.JD0) THEN
            OJOB = OJOB + 1
            OREC = OJOB / NLPR + 1
            IF (OREC.NE.LOREC) THEN
               CALL ZFIO ('WRIT', ALUN, AIND, LOREC, OBUF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1410) IERR
                  CALL MSGWRT (8)
                  GO TO 900
                  END IF
               LOREC = OREC
               END IF
            ONP = (OJOB - (OREC-1)*NLPR) * NWPL + 1
            CALL COPY (NWPL, BUFFER(NP), OBUF(ONP))
            END IF
 430     CONTINUE
      CALL ZFIO ('WRIT', ALUN, AIND, LOREC, OBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1410) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       read back control buf
      CALL ZFIO ('READ', ALUN, AIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         CALL MSGWRT (8)
         IF (DOALLH) GO TO 410
         GO TO 900
         END IF
      BUFFER(1) = OJOB
      BUFFER(3) = 9
      BUFFER(4) = 256 / BUFFER(3)
      LREC = (BUFFER(2) + 1) / BUFFER(4)
      NREC = (OJOB + 1) / BUFFER(4) + 1
      NREC = MAX (100, NREC)
      IF (LREC.LE.NREC) NREC = MAX (LREC, 2)
      CALL ZFIO ('WRIT', ALUN, AIND, 1, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1410) IERR
         CALL MSGWRT (8)
         GO TO 900
         END IF
      WRITE (MSGTXT,1420) HSTNAM, LJOB, OJOB
      CALL MSGWRT (4)
C                                       Compress it
      IF (NREC.LT.LREC) THEN
         CALL ZCMPRS (1, PHNAME, ALUN, NREC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1430) IERR
            CALL MSGWRT (7)
            GO TO 900
            END IF
         NREC = NREC * BUFFER(4) - 1
         BUFFER(2) = NREC
         CALL ZFIO ('WRIT', ALUN, AIND, 1, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1410) IERR
            CALL MSGWRT (8)
            GO TO 900
            END IF
         END IF
      IF ((DOALLH) .AND. (LHOST.LT.NHOST)) GO TO 410
      GO TO 900
C-----------------------------------------------------------------------
C                                       Close down
 890  WRITE (MSGTXT,1890) IERR
      CALL MSGWRT (7)
C
 900  IF (IERR.LE.0) IRET = 0
C
 910  IF (AOPEN) CALL ZCLOSE (ALUN, AIND, IERR)
      IF (OPENLP) CALL LPCLOS (PLUN, PIND, LINE, IERR)
 990  IF (ISTASK) CALL DIETSK (IRET, RQUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR',I7,' OPENING TERMINAL')
 1025 FORMAT ('CAN''T OPEN FILE',I7)
 1030 FORMAT ('CAN''T OPEN PRINTER',I7)
 1050 FORMAT ('Do:',6(1X,A4),'?  (A4 at start of line)')
 1051 FORMAT ('Do:',3(1X,A4),'?  (A4 at start of line)')
 1100 FORMAT ('Restrict to task name (A6 at start line) ',
     *   'BLANK => all')
 1110 FORMAT ('Range of pops numbers (2 I) 0 => all')
 1120 FORMAT (I2,' user number to include (I) 0 => all, -999 => staff')
 1121 FORMAT (I2,' user number to include (I) 0 => end list')
 1130 FORMAT ('Begin time YYYY MM DD HH MM SS (6 I) 0 => all')
 1140 FORMAT ('Only tasks with cpu time > x (F6.2)')
 1150 FORMAT ('Print: LIST, USERS, POPS, TASKS, versions (5L1)')
 1151 FORMAT (5L1)
 1160 FORMAT ('File name to keep printer output: '' '' => scratch')
 1165 FORMAT ('Name of computer: '' '' => local, ''ALL'' => ',
     *   'HOSTS.LIST')
C
 1200 FORMAT ('IO ERROR ON DISK READ',I7)
 1210 FORMAT ('Task',3X,'#',3X,'User',7X,'Start time',9X,'Real',6X,
     *   'Cpu',8X,'Iocnt  Ver  Abort')
 1211 FORMAT ('Task',3X,'#',2X,'User',6X,'Start time',8X,'Real',5X,
     *   'Cpu',6X,'Iocnt',2X,'Ver Abort')
 1212 FORMAT ('Sequential list',26X,'secs',6X,'secs')
 1213 FORMAT ('Sequential list',23X,'secs',5X,'secs')
 1220 FORMAT (A5,I3,I6,2X,A,A8,I9,F10.2,I12,2X,A3,5X,A2)
 1221 FORMAT (A5,I3,I5,1X,A,A8,I8,F9.2,I10,2X,A3,4X,A2)
 1275 FORMAT ('Totals since ',A12,2X,A8,5X,'on ',A,' (',A,')')
 1276 FORMAT ('  # tasks ',5I12)
 1277 FORMAT ('  Tot real',5F12.1)
 1278 FORMAT ('  Tot cpu ',5F12.3)
 1279 FORMAT ('  Tot ioc ',5F12.0)
 1280 FORMAT ('  Av cp/re',5F12.5)
 1281 FORMAT ('  # abort ',5I12)
 1282 FORMAT ('  # users ',I12)
 1283 FORMAT ('  # tasks ',I12)
 1284 FORMAT (' Limited to pops #    ',2I6)
 1285 FORMAT (' Limited to user #(s) ',8I6)
 1286 FORMAT (' Limited to start time',I6,5I3)
 1287 FORMAT (' Limited to cpu min   ',F12.1)
 1288 FORMAT (' Limited to task name ',A6)
 1289 FORMAT (' List all task names')
 1290 FORMAT (3(A16,I10,6X))
 1300 FORMAT (A4,2(4X,A4,4X,'Real',4X,'Cpu',1X,'Abort'),5X,'Total')
 1301 FORMAT (3X,'#',2(4X,'jobs',4X,'mins',4X,'mins',4X,'#'),7X,'cpu')
 1305 FORMAT (I4,2(3I8,I5),I10)
 1310 FORMAT ('Tot ',2(3I8,I5),I10)
 1320 FORMAT (A4,1X,A4,2(4X,A4,4X,'Real',5X,'Cpu',1X,'Abort'),5X,
     *   'Total',A1,5X,'Total')
 1321 FORMAT (A4,A4,2(3X,A4,4X,'Real',3X,'Cpu',1X,'Abort'),5X,'Total')
 1322 FORMAT ('Ver',5X,'#',2(4X,'jobs',4X,'mins',4X,'mins',5X,'#'),6X,
     *   'cpu',A1,5X,'iocnt')
 1323 FORMAT ('Ver',4X,'#',2(3X,'jobs',4X,'mins',3X,'mins',4X,'#'),7X,
     *   'cpu')
 1325 FORMAT (I3,I6,2(2I8,F9.1,I5),I10,I11)
 1326 FORMAT (I3,I5,2(I7,I8,I7,I5),I10)
 1340 FORMAT (A,3X,'Task',6X,'Jobs',6X,'Real',9X,'Cpu',6X,'Abort',3X,
     *   'Cpu to',2X,'Iocnt/')
 1341 FORMAT ('Vers',3X,'name',9X,'#',6X,'mins',9X,'mins',9X,'#',4X,
     *   'real',3X,'Cpusec')
 1350 FORMAT (I4,3X,A6,I8,F12.2,F13.3,I8,F9.3,I8)
 1400 FORMAT ('Keep times > YYYY MM DD HH MM SS (6 I) 0 => none')
 1410 FORMAT ('IO ERROR ON DISK READ',I7)
 1420 FORMAT ('Host ',A,' compress',I10,' entries to',I10)
 1430 FORMAT ('ERROR',I7,' COMPRESSING THE FILE')
 1890 FORMAT ('ERROR',I7,' DOING IO TO TERMINAL')
      END
      SUBROUTINE FNDDDT (OPTYPE, ALUN, AIND, JD, DPRTIM, JDE, IPOPS,
     *   IUSER, IERR)
C-----------------------------------------------------------------------
C   Finds when the last DDT sequence started
C   Inputs:
C      OPTYPE   C*4   Optype of user: 'DDT ', 'Y2K ', 'Y2KH', 'CYG'
C      ALUN     I     Open lun
C      AIND     I     FTAB pointer
C      JD       D     Reference time of PRTAC run
C   Output:
C      DPRTIM   D     Start time before JD for run
C      JDE      D     End time for DDT run
C      IPOPS    I(2)  Restrict to POPS number
C      IUSER    I(2)  Restrict to user number
C      IERR     I     Error code
C-----------------------------------------------------------------------
      CHARACTER OPTYPE*4
      INTEGER   ALUN, AIND, IPOPS(2), IUSER(2), IERR
      DOUBLE PRECISION JD, DPRTIM, JDE
C
      CHARACTER TTASK*6, TASKD(23)*6, TASKY(23)*6, TASKH(23)*6,
     *   TASKC(23)*6, TASKS(23,4)*6
      INTEGER   BUFFER(256), LREC, LJOB, IJOB, NLPR, NWPL, JJOB, TP, LU,
     *   LP, IU, IP, NREC, NP, IT(6), NTASK(4), JJ
      DOUBLE PRECISION JDX
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (TASKS(1,1), TASKD), (TASKS(1,2), TASKY),
     *   (TASKS(1,3), TASKH), (TASKS(1,4), TASKC)
      DATA NTASK /22, 20, 23, 16/
      DATA TASKD /'COMB', 'VTESS', 'SUBIM', 'COMB', 'MX', 'COMB',
     *   'COMB', 'MX', 'UVDIF', 'UVSRT', 'CALIB', 'CCMRG', 'COMB',
     *   'APCLN', 'SUBIM', 'COMB', 'APCLN', 'COMB', 'COMB', 'UVMAP',
     *   'UVDIF', 'UVSRT', ' '/
      DATA TASKY /'COMB', 'COMB', 'VTESS', 'SUBIM', 'IMAGR', 'COMB',
     *   'IMAGR', 'UVDIF', 'CALIB', 'CCMRG', 'COMB', 'APCLN', 'SUBIM',
     *   'COMB', 'APCLN', 'COMB', 'COMB', 'IMAGR', 'UVDIF', 'UVSRT',
     *   ' ', ' ', ' '/
      DATA TASKH /'COMB', 'COMB', 'VTESS', 'REMAG', 'OGEOM', 'IMAGR',
     *   'COMB', 'COMB', 'COMB', 'COMB', 'IMAGR', 'UVDIF', 'CALIB',
     *   'COMB', 'COMB', 'COMB', 'COMB', 'IMAGR', 'COMB', 'COMB',
     *   'IMAGR', 'UVDIF', 'UVSRT'/
      DATA TASKC /'COMB', 'IMAGR', 'UVDIF', 'CALIB', 'COMB', 'IMAGR',
     *   'UVDIF', 'CALIB', 'COMB', 'COMB', 'COMB', 'COMB', 'IMAGR',
     *   'COMB', 'COMB', 'IMAGR', 7*' '/
C-----------------------------------------------------------------------
      JJ = 0
      IERR = 9
      IF (OPTYPE.EQ.'DDT ') JJ = 1
      IF (OPTYPE.EQ.'Y2K ') JJ = 2
      IF (OPTYPE.EQ.'Y2KH') JJ = 3
      IF (OPTYPE.EQ.'CYG ') JJ = 4
      IF (JJ.EQ.0) GO TO 999
C                                       Read rec 1 of control parms
      LREC = 1
      CALL ZFIO ('READ', ALUN, AIND, LREC, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
      LJOB = BUFFER(1)
      NLPR = BUFFER(4)
      NWPL = BUFFER(3)
C                                       Loop for all jobs
      TP = 1
      LU = 0
      LP = 0
      DO 50 JJOB = 1,LJOB
         IJOB = LJOB - JJOB + 1
         NREC = IJOB / NLPR + 1
         IF (NREC.NE.LREC) THEN
            LREC = NREC
            CALL ZFIO ('READ', ALUN, AIND, NREC, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         NP = (IJOB - (NREC-1)*NLPR) * NWPL + 1
         CALL CATIME (2, BUFFER(NP+4), IT)
         CALL DAT2JD (IT, JDX)
         IP = MOD (BUFFER(NP+2), 100)
         IU = BUFFER(NP+3)
         IF ((LU.EQ.0) .OR. ((IU.EQ.LU) .AND. (IP.EQ.LP))) THEN
 20         CALL H2CHR (6, 1, BUFFER(NP), TTASK)
C                                       continues to match
            IF (TTASK.EQ.TASKS(TP,JJ)) THEN
               TP = TP + 1
               DPRTIM = JD - JDX + 1.5D-5
               LU = IU
               LP = IP
               IF (TP.EQ.NTASK(JJ)+1) THEN
                  IPOPS(1) = IP
                  IUSER(1) = IU
                  IPOPS(2) = IP
                  IUSER(2) = IU
                  GO TO 999
                  END IF
C                                       not match
            ELSE
               JDE = JDX - 0.5D-5
               LU = 0
               LP = 0
               IF (TP.GT.1) THEN
                  TP = 1
                  GO TO 20
                  END IF
               END IF
            END IF
 50      CONTINUE
      IERR = 6
      MSGTXT = OPTYPE // ' JOB NOT FOUND'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE NXTSTR (KB, KBPLIM, KBP, ISTR, NCHAR)
C-----------------------------------------------------------------------
C   NXTSTR obtains a character value from a buffer - blank separators
C   Inputs:
C      KB      C*80     character buffer
C      KBPLIM  I        size of buffer
C      KBP     I        start position in KB
C   Outputs:
C      KBP     I        start position in KB next field
C      ISTR    C*(*)    packed string, blank filled
C      NCHAR   I        # characters (0 => no string found)
C-----------------------------------------------------------------------
      CHARACTER KB*80, ISTR*(*)
      INTEGER   KBPLIM, KBP, NCHAR
C
      INTEGER   JB, JJ, LMAX
C-----------------------------------------------------------------------
      NCHAR = 0
      LMAX = LEN (ISTR)
      ISTR = ' '
      IF (KBP.GT.KBPLIM) GO TO 999
C                                        skip leading blanks
 10   IF (KB(KBP:KBP).EQ.' ') THEN
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
         END IF
C                                        find end
 15   JB = KBP
 20   KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 25
         IF (KB(KBP:KBP).EQ.' ') GO TO 25
         GO TO 20
C                                        got it
 25   NCHAR = KBP - JB
      KBP = KBP + 1
      JJ = MIN (NCHAR, LMAX)
      NCHAR = MIN (NCHAR, LMAX)
      IF (NCHAR.NE.0) ISTR(1:JJ) = KB(JB:JB+JJ-1)
C                                        make null string ok
      IF (NCHAR.EQ.0) NCHAR = 1
C
 999  RETURN
      END
