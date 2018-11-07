LOCAL INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:POBV.INC'
C
      INTEGER MXNXL
      PARAMETER (MXNXL = 1000)
      INTEGER IDNX(MXNXL), NIDNX
      REAL DTIM(MXNXL), RTIM(MXNXL)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XOPTY
      REAL      XSIN, XDISIN, XINV, XTIME(8), XSUBA,  APARM(10), XFLAG,
     *   BADD(10),  TIMBEG, TIMEND
      DOUBLE PRECISION  DT, STARNX
      INTEGER   MXORB
      PARAMETER (MXORB = 5)
      INTEGER   SEQIN, CNOIN, DISKIN, SUBA, BUFFER(1024),
     *   INVER, NUMHIS, ICODE, KFGRNO, NTIME, FGVER
      LOGICAL  NOOB
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4, HISCRD(1000)*64,
     *   REASON*24
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XINV, XTIME, XSUBA,
     *   XOPTY, APARM, XFLAG, BADD
      COMMON /SEC/ SEQIN, DISKIN, CNOIN, TIMBEG, TIMEND, NUMHIS,
     *   SUBA, NOOB, BUFFER, INVER, ICODE, DT, STARNX,
     *   KFGRNO, NTIME, IDNX, DTIM, RTIM, NIDNX, FGVER
      COMMON /CHPARM/ NAMEIN, CLAIN, HISCRD, REASON, OPTYPE
LOCAL END
      PROGRAM OBEDT
C-----------------------------------------------------------------------
C! OBEDT fill out the OB table by additional data (Sun angle, eclipce,..)
C# TASK Coordinates Table
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2007, 2009, 2018
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
C  flag data under control of information read from  OB table
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INVER......The input OB table version number
C      TIMERANG...Time range of the data to be calibrateded. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      SUBARRAY...Subarray number.
C      OPCODE.....Parameter used to control the flaging
C      APARM......Control information
C      FLAGVER....Version of the FG table.
C   Outputs:
C     Programmer L. Kogan, April 1997
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'OBEDT'/
C-----------------------------------------------------------------------
      CALL ORBIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       provide flagging
      CALL FLAG (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL OBHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE ORBIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ORBIN gets input parameters for OBEDT.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => input parameters problem
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      REAL TTIME, DTIME, RTIME, STOPNX
      CHARACTER CODE(5)*4, UTYPE*2, STAT*4, BLANK*4
      INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INTEGER IRET, NPARMS, JERR, IROUND, OBTOT, I, NCODE, OUTVER,
     *   LUNX, BUFFNX(512), INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC),
     *   NUMBNX, ISUB, IDSOU, VSTART, VEND, FRID
C     CODE =                 1       2       3       4       5
      DATA NCODE, CODE /5, 'DIST', 'VEL ', 'SUNS', 'ECLB', 'ECLE'/
      DATA BLANK /'    '/
      DATA LUNX /16/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NUMHIS = 0
      NCFILE = 0
      JERR = 0
      NPARMS = 39
      CALL GTPARM (PRGN, NPARMS, RQUICK, XNAMEI, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IRET.EQ.1) THEN
            JERR = 1
            WRITE (MSGTXT,1000) JERR
            GO TO 990
            END IF
         END IF
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IRET)
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XOPTY, OPTYPE)
C                                       OPTYPE ?
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      ICODE = 1
      DO 10 I = 1,NCODE
         IF ((OPTYPE.EQ.CODE(I)) .AND. (OPTYPE.NE.BLANK))
     *      ICODE = I
 10      CONTINUE
C
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1200) IRET, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         JERR = 5
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IRET)
      IF (IRET.NE.0) THEN
         JERR = 5
         WRITE (MSGTXT,1300) JERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Input OB table
      INVER = IROUND (XINV)
C                                       determine number of OB tables
      CALL FNDEXT ('OB', CATBLK, OBTOT)
      NOOB = (OBTOT.EQ.0)
      IF (INVER.EQ.0  .OR.  INVER.GT.OBTOT) INVER = OBTOT
C                                       Subarray
      SUBA = IROUND (XSUBA)
      IF (SUBA.EQ.0) SUBA = 1
      WRITE (REASON, 1350) CODE(ICODE)
C                                       flagver
      FGVER = IROUND (XFLAG)
      CALL FNDEXT ('FG', CATBLK, OUTVER)
      IF (FGVER.EQ.0) FGVER = OUTVER + 1
      WRITE (MSGTXT,1370) FGVER
      CALL MSGWRT (8)
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMBEG.LT.1.0E-5)) TIMBEG = 0.0
C                                       step in time
      DT = APARM(9)
      IF (DT.EQ.0) DT = 10
      DT = DT / 86400.0
C                                       look NX table to determine the
C                                       source observed at the time
C                                       and maximum time observed
C
C                                       Initialize the NX table
      CALL NDXINI ('READ', BUFFNX, DISKIN, CNOIN, 1, CATBLK,
     *   LUNX, INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1250)
         GO TO 990
         END IF
C                                       How many entries at the NX table
      NUMBNX = BUFFNX(5)
C                                       read NX table to know the source
C                                       observed
      DO 180 I = 1, NUMBNX
         CALL TABNDX ('READ', BUFFNX, INXRNO, NXKOLS, NXNUMV, RTIME,
     *      DTIME, IDSOU, ISUB, VSTART, VEND, FRID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1325) IRET
            GO TO 990
            END IF
C                                       store times, intervals and
C                                       source IDs
         RTIM(I) = RTIME
         DTIM(I) = DTIME
         IDNX(I) = IDSOU
  180    CONTINUE
      NIDNX = NUMBNX
C                                       close NX table
      CALL TABIO ('CLOS', 0, INXRNO, BUFFNX, BUFFNX, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1425) IRET
         GO TO 990
         END IF
C                                       Actual timerang of the data
C                                       and number of the points
      STARNX = RTIM(1) - DTIM(1) / 2
      STOPNX = RTIM(NUMBNX) + (DTIM(NUMBNX) / 2)
      IF (TIMBEG.GT.STOPNX  .OR.  TIMEND.LT.STARNX) THEN
         IRET = 1
         WRITE (MSGTXT,1800) TIMBEG, TIMEND, STARNX, STOPNX
         GO TO 990
         END IF
      IF (TIMBEG.GT.STARNX) STARNX = TIMBEG
      IF (TIMEND.LT.STOPNX) STOPNX = TIMEND
      NTIME = (STOPNX - STARNX) / DT + 0.5
C                                       history file
      NUMHIS = NUMHIS + 1
      TTIME = 0
      DO 20 I = 1, 8
         TTIME = TTIME + XTIME(I)
   20    CONTINUE
      IF (TTIME.EQ.0.0) THEN
         WRITE (HISCRD(NUMHIS),1400)
      ELSE
         WRITE (HISCRD(NUMHIS),1500) (XTIME(I),I=1,8)
         END IF
C                                       OB table version
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1600) INVER
C                                       FG table version
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1650) FGVER
C                                       subarray
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1700) SUBA
C                                       opcode
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),1750) CODE(ICODE)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ORBIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1200 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1250 FORMAT ('There is no NX table. Run INDXR to get it')
 1300 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1325 FORMAT ('ERROR in reading of NX table ',I3)
 1350 FORMAT ('Bad ', A4, ' at the orb ant.')
 1370 FORMAT ('FG table ' ,I3, ' is used')
 1400 FORMAT ('TIMERANG = beginning to end')
 1425 FORMAT ('ERROR ',I3,' CLOSING NX FILE')
 1500 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' - ',F3.0,1X,F3.0,F3.0,F4.1,
     *   '/ Time range')
 1600 FORMAT ('Input OB table =', I3)
 1650 FORMAT ('FG table =', I3)
 1700 FORMAT ('Subarray =', I3)
 1750 FORMAT ('Opcode = ', A4)
 1800 FORMAT ('Your timerange ', F7.4, '-', F7.4, 'is outside ',
     *   'of the data range ', F7.4, '-', F7.4)
      END
      SUBROUTINE FLAG (IRET)
C-----------------------------------------------------------------------
C   Routine to flag data under control of the selected parameter and
C   threshold
C   Input from common:
C       CNOIN        I         Cat. # of file
C       DISKIN       I         Vol containing input file
C       TIMBEG       D         Start time
C       TIMEND       D         Stop time
C       INVER        I         Input OB table version
C       SUBA         I         Subarray
C       ICODE        I         Opcode
C   Output:
C       IRET         I         Error code, 0 => OK
C-----------------------------------------------------------------------
      INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER LUNI, IFS(2), CHANS(2), SID, FQID, MORB, KANT, K, ITIME,
     *   NUMBAN(MXORB), IORB, ANTSS(2), FGKOLS(MAXFGC), FGNUMV(MAXFGC),
     *   BUFF(512), IRET
      REAL  TIMER(2), LOWLIM, TOPLIM, LTIME, TTIME
      DOUBLE PRECISION TIME, VAL
      LOGICAL PFLAGS(4), IN, FT, INSIDE, DOFL
C
      DATA LUNI /27/
C-----------------------------------------------------------------------
C
      LOWLIM = APARM(1)
      TOPLIM = APARM(2)
      DO 20 K = 1, 2
         IFS(K) = 2 - K
         CHANS(K) = 2 - K
         PFLAGS(K) = .TRUE.
         PFLAGS(K + 2) = .TRUE.
   20    CONTINUE
      SID = 0
      FQID = 0
      CALL FLGINI ('WRIT', BUFF, DISKIN, CNOIN, FGVER, CATBLK,
     *   LUNI, KFGRNO, FGKOLS, FGNUMV, IRET)
C                                       mark FG table as unsorted
      BUFFER(43) = 0
      BUFFER(44) = 0
      MORB = MXORB
      KANT = 1
      DO 60 IORB = 1, MORB
         IN = .FALSE.
         FT = .FALSE.
         DOFL = .FALSE.
C                                       find the time intervals when VAL
C                                       is inside of the given range and
C                                       flag them
         IF (IORB.GT.KANT) GO TO 80
         DO 40 ITIME = 1, NTIME
            TIME = STARNX + DT * (ITIME - 1)
            CALL VALFG (TIME, ITIME, IORB, IN, KANT, NUMBAN, VAL, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000)
               GO TO 990
               END IF
            IF (VAL.EQ.FBLANK) GO TO 40
            INSIDE = (VAL.GT.LOWLIM)  .AND.  (VAL.LT.TOPLIM)
            IF (INSIDE .AND. .NOT. FT) THEN
               LTIME = TIME
               FT = .TRUE.
               END IF
            IF (.NOT. INSIDE .AND. FT) THEN
               DOFL = .TRUE.
               TTIME = TIME
               FT = .FALSE.
               END IF
            IF (DOFL) THEN
               TIMER(1) = LTIME
               TIMER(2) = TTIME
               ANTSS(1) = NUMBAN(IORB)
               ANTSS(2) = 0
               CALL TABFLG ('WRIT', BUFF, KFGRNO, FGKOLS, FGNUMV,
     *            SID, SUBA, FQID, ANTSS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IRET)
               DOFL = .FALSE.
               END IF
   40       CONTINUE
   60    CONTINUE
   80 CONTINUE
      CALL TABFLG ('CLOS', BUFF, KFGRNO, FGKOLS, FGNUMV, SID, SUBA,
     *   FQID, ANTSS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VALFG: Error in finding VAL')
      END
      SUBROUTINE VALFG (TIME, ITIME, IORB, IN, KANT, NUMBAN, VAL, IRET)
C-----------------------------------------------------------------------
C   Routine to calculate the selected parameter of the orbiting antenna
C   for the given time
C   threshold
C   Input:
C       TIME         D         Time relatively to the reference
C                              date, days
C                              available
C       ITIME        I         sequence number of the time
C       IORB         I         sequence number of the orbiting antenna
C   Input from common:
C       CNOIN        I         Cat. # of file
C       DISKIN       I         Vol containing input file
C       TIMBEG       D         Start time
C       TIMEND       D         Stop time
C       NOOB         L         True if there is no OB table
C       SUBA         I         Subarray
C       INVER        I         Input OB table version
C       ICODE        I         Code of the output value
C   Output:
C       KANT         I         Number of found orbiting antennas
C       NUMBAN       I(*)      Array of orbiting antennas numbers
C       VAL          D         The value of the selected parameter
C       IRET         I         Error code, 0 => OK
C   Input/Output
C       IN           L         Given antenna is in eclipce now?

C-----------------------------------------------------------------------
      INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER   ITIME, IORB, KANT, NUMBAN(*), FIRSTN, MANT, IRET, LUNI,
     *   NUMREC, IOBRNI, IANTOB, ISUBOB, IDF, K, YEAR, IDY, IMN, MXOR,
     *   DAY, ILOYR, IANT, OBKOLS(MAXOBC), OBNUMV(MAXOBC), IANRNI, I
      REAL      ANGLOB(3), ECLPOB(4), ORIOB, TST, TEN
      LOGICAL   GOOD, OURS, ECLIPS, ANFIL, FIRST, IN
      DOUBLE PRECISION   DTIMOB, DXYZOB(3), DVELOB(3), RA, DEC, MANCOS,
     *   MANSIN, PERCOS, PERSIN, OMECOS, OMESIN, AMETER, ECCEN, INCLIN,
     *   SUND, PA, TRANOM, PROJA, PROJB, VA, VB, X, Y, Z, VX, VY, VZ,
     *   TIMES, TIME, VAL
      CHARACTER LREF*8
C
      DOUBLE PRECISION SEMIAX(MXORB), ECC(MXORB), INCL(MXORB),
     *   MANCOR(MXORB), MANSIR(MXORB), PERCOR(MXORB), PERSIR(MXORB),
     *   OMECOR(MXORB), OMESIR(MXORB), MANOMR(MXORB), PERIGR(MXORB),
     *   OMEGAR(MXORB), START, STOP
      INTEGER KPOINT(MXORB)
      INCLUDE 'INCS:DANT.INC'
C
      DATA LUNI /28/
C-----------------------------------------------------------------------
      IF (IORB.GT.1  .OR. ITIME.GT.1) GO TO 170
C                                       take the orbit parameters of
C                                       orbiting antenna from AN table
C                                       initialize the AN table to read
      CALL ANTINI ('READ', BUFFER, DISKIN, CNOIN, SUBA, CATBLK, LUNI,
     *   IANRNI, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANNAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1800) SUBA
         GO TO 990
         END IF
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      KANT = 0
      ANFIL = .FALSE.
      FIRST = .TRUE.
      DO 20 I = 1,NUMREC
C                                       read the AN table
         CALL TABAN ('READ', BUFFER, IANRNI, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
C                                       store the first orbiting antenna
C                                       number
         IF ( MNTSTA.EQ.2  .AND. FIRST) THEN
            FIRSTN = NOSTA
            FIRST = .FALSE.
            END IF
C
         IF ( MNTSTA.EQ.2  .AND.  ORBPRM(1).NE.0) THEN
C                                       Yes. AN table is filled out by
C                                       the orbiting parameters
            ANFIL = .TRUE.
            KANT = KANT + 1
            NUMBAN(KANT) = NOSTA
            SEMIAX(KANT) = ORBPRM(1)
            ECC(KANT) = ORBPRM(2)
            INCL(KANT) = ORBPRM(3) * DG2RAD
            OMEGAR(KANT) = ORBPRM(4) * DG2RAD
            PERIGR(KANT) = ORBPRM(5) * DG2RAD
            MANOMR(KANT) = ORBPRM(6) * DG2RAD
            END IF
 20      CONTINUE
C                                       close the AN table
      CALL TABIO ('CLOS', 0, IANRNI, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1900) SUBA
         GO TO 990
         END IF
      MANT = KANT
      CALL H2CHR (8, 1, CATH(KHDOB), LREF)
      READ (LREF, 1000) IDY, IMN, YEAR
C                                       find day number startin Jan 1
C                                       DAY and number of days in the
C                                       year - ILOYR
      IF (YEAR.LT.20) THEN
         YEAR = 2000 + YEAR
      ELSE
         YEAR = 1900 + YEAR
         END IF
      CALL DAYYR (YEAR, IMN, IDY, DAY, ILOYR)
C                                       if AN table is not filled by
C                                       orbiting parameters derive them
C                                       from OB table
      IF (ANFIL) GO TO 160
C                                       if there is no an OB table
C                                       derive the orbiting parameters
C                                       from APARMs
      IF (NOOB) GO TO 140
C                                       initialize the input OB table
      CALL OBINI ('READ', BUFFER, DISKIN, CNOIN, INVER, CATBLK, LUNI,
     *   IOBRNI, OBKOLS, OBNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) INVER
         GO TO 990
         END IF
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       initialize the orbit parameters
      MXOR = MXORB
      CALL DFILL (MXOR, 0.0D0, SEMIAX)
      CALL DFILL (MXOR, 0.0D0, ECC)
      CALL DFILL (MXOR, 0.0D0, INCL)
      CALL DFILL (MXOR, 0.0D0, MANCOR)
      CALL DFILL (MXOR, 0.0D0, MANSIR)
      CALL DFILL (MXOR, 0.0D0, PERCOR)
      CALL DFILL (MXOR, 0.0D0, PERSIR)
      CALL DFILL (MXOR, 0.0D0, OMECOR)
      CALL DFILL (MXOR, 0.0D0, OMESIR)
      CALL FILL  (MXOR, 0,     KPOINT)
      CALL FILL  (MXOR, 0,     NUMBAN)
      CALL LFILL (MXOR, .FALSE.,     IN)
C
      IANT = 0
      DO 80  I = 1, NUMREC
C                                       read the input table
         CALL TABOB ('READ', BUFFER, IOBRNI, OBKOLS, OBNUMV,
     *      IANTOB, ISUBOB, DTIMOB, DXYZOB, DVELOB, ANGLOB, ECLPOB,
     *      ORIOB, IRET)
C                                       Identify the sequence number of
C                                       the orbiting antenna
         DO 40 KANT = 1, IANT
            IF (IANTOB.EQ.NUMBAN(KANT)) GO TO 60
  40       CONTINUE
         IANT = IANT + 1
         KANT = IANT
         NUMBAN(KANT) = IANTOB
  60     CONTINUE
C                                       restore the orbit
C                                       The source position is not
C                                       important now so take them zero
         RA = 0
         DEC = 0
C                                       calculate the new columns
         CALL ORTAB (YEAR, DAY, ILOYR, DTIMOB, DXYZOB, DVELOB,
     *      RA, DEC, GOOD, SUND, ECLIPS, PA, AMETER, ECCEN, MANCOS,
     *      MANSIN, PERCOS, PERSIN,  INCLIN, OMECOS, OMESIN)
C                                       average orbit parameters
         IF (GOOD) THEN
            SEMIAX(KANT) = SEMIAX(KANT) + AMETER
            ECC(KANT) = ECC(KANT) + ECCEN
            INCL(KANT) = INCL(KANT) + INCLIN
            MANCOR(KANT) = MANCOR(KANT) + MANCOS
            MANSIR(KANT) = MANSIR(KANT) + MANSIN
            PERCOR(KANT) = PERCOR(KANT) + PERCOS
            PERSIR(KANT) = PERSIR(KANT) + PERSIN
            OMECOR(KANT) = OMECOR(KANT) + OMECOS
            OMESIR(KANT) = OMESIR(KANT) + OMESIN
            KPOINT(KANT) = KPOINT(KANT) + 1
            END IF
  80     CONTINUE
      WRITE (MSGTXT,1600) INVER
      CALL MSGWRT (8)
C                                       calculate mean orbits
      DO 120 K = 1, KANT
         SEMIAX(K) = SEMIAX(K) / KPOINT(K)
         ECC(K) = ECC(K) / KPOINT(K)
         INCL(K) = (INCL(K) / KPOINT(K))
         MANOMR(K) = ATAN2 (MANSIR(K), MANCOR(K))
         PERIGR(K) = ATAN2 (PERSIR(K), PERCOR(K))
         OMEGAR(K) = ATAN2 (OMESIR(K), OMECOR(K))
  120    CONTINUE
C                                       Close input OB table
      CALL TABIO ('CLOS', 0, IOBRNI, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1700) INVER
         GO TO 990
         END IF
      MANT = KANT
      GO TO 160
C
  140 CONTINUE
C                                       Derive the only orbiting antenna
C                                       from APARMs if AN table is not
C                                       filled out by the orbiting
C                                       parameters and OB table is
C                                       absent
      MANT = 1
      NUMBAN(1) = FIRSTN
      SEMIAX(1) = APARM(3)
      ECC(1) = APARM(4)
      INCL(1) = APARM(5) * DG2RAD
      OMEGAR(1) = APARM(6) * DG2RAD
      PERIGR(1) = APARM(7) * DG2RAD
      MANOMR(1) = APARM(8) * DG2RAD
  160 CONTINUE
C                                       Now the calculation of the six
C                                       parameters of all orbiting
C                                       antennas have been consamated
      KANT = MANT
  170 CONTINUE
C                                       look NX table to determine the
C                                       source observed at the time
C
      OURS = .TRUE.
      DO 180 I = 1, NIDNX
         TST = RTIM(I) - DTIM(I)*0.5
         TEN = RTIM(I) + DTIM(I)*0.5
         IF (TIME.GE.TST  .AND.  TIME.LE.TEN) THEN
            IDF = IDNX(I)
            GO TO 220
            END IF
  180       CONTINUE
      OURS = .FALSE.
  220 CONTINUE
      IF (TIME.GT.TIMEND .OR. TIME.LT.TIMBEG)
     *   OURS = .FALSE.
C                                       find the RA, DEC of the source
C                                       observed
      IF (OURS) THEN
         CALL GETSOU (IDF, DISKIN, CNOIN, CATBLK, LUNI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1500) IRET
            GO TO 990
            END IF
         RA = RAAPP
         DEC = DECAPP
      ELSE
         RA = 0.0
         DEC = 0.0
         END IF
      IF (.NOT. OURS .AND. ICODE.EQ.3) THEN
         VAL = FBLANK
         GO TO 340
         END IF
C                                       Coordinates of the source have
C                                       been determined
C
C                                       calculate VAL for each ICODE
C                                       for the given antenna
C
C                                       position of the given antenna
      TIMES = TIME * 86400.0D0
      CALL ORBIT (SEMIAX(IORB), ECC(IORB), INCL(IORB), OMEGAR(IORB),
     *   PERIGR(IORB), MANOMR(IORB), TIMES, TRANOM, PROJA, PROJB, VA,
     *   VB, X, Y, Z, VX, VY, VZ)
C
C                                       OPCODE = 'DIST'
      IF (ICODE.EQ.1) THEN
C                                       Distance from the Earth center
C                                       in km
         VAL = SQRT (PROJA * PROJA + PROJB * PROJB) / 1000
         GO TO 340
         END IF
C                                       OPCODE = 'VEL '
      IF (ICODE.EQ.2) THEN
C                                       Speed in km/s
         VAL = SQRT (VA * VA + VB * VB) / 1000
         GO TO 340
         END IF
C
      DXYZOB(1) = X
      DXYZOB(2) = Y
      DXYZOB(3) = Z
      DVELOB(1) = VX
      DVELOB(2) = VY
      DVELOB(3) = VZ
      CALL ORTAB (YEAR, DAY, ILOYR, TIME, DXYZOB, DVELOB,
     *   RA, DEC, GOOD, SUND, ECLIPS, PA, AMETER, ECCEN, MANCOS,
     *   MANSIN, PERCOS, PERSIN,  INCLIN, OMECOS, OMESIN)
C                                       OPCODE = 'SUNS'
      IF (ICODE.EQ.3) THEN
C                                       Angle Sun-source in degrees
         VAL = SUND
         GO TO 340
         END IF
C                                       find the begin and end of the
C                                       previous eclipse of the Sun
      IF (ECLIPS .AND. .NOT. IN) THEN
         START = TIME
         IN = .TRUE.
         END IF
      IF (.NOT. ECLIPS .AND. IN) THEN
         STOP = TIME
         IN = .FALSE.
         END IF
C                                       OPCODE = 'ECLB'
      IF (ICODE.EQ.4) THEN
C                                       Time interval from the beginning
C                                       of the previos eclipce, hours
         VAL = (TIME - START) * 24
         GO TO 340
         END IF
C                                       OPCODE = 'ECLE'
      IF (ICODE.EQ.5) THEN
C                                       Time interval from the end
C                                       of the previos eclipce, hours
         VAL = (TIME - STOP) * 24
         GO TO 340
         END IF
  340 CONTINUE
      GO TO 999
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2, 1X, I2, 1X, I2)
 1100 FORMAT ('ERROR in initialization of OB table ',I3)
 1500 FORMAT ('ERROR in GETSOU')
 1600 FORMAT ( ' OB table ', I3 , ' is used')
 1700 FORMAT ('ERROR closing OB table', I3)
 1800 FORMAT ('ERROR in initialization of AN table ',I3)
 1900 FORMAT ('ERROR closing AN table', I3)
      END
      SUBROUTINE OBHIS
C-----------------------------------------------------------------------
C   OBHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'OBEDT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIM)
      CALL TIMDAT (TIM, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OBHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END
      SUBROUTINE DAYYR (IYR, IMONTH, IDAY, IDAYNO, ILOYR)
C-----------------------------------------------------------------------
C   Subroutine to compute day number (Jan 1 = 1) from Gregorian date
C   Inputs:
C      IYR     I   Year number (eg. 1994)
C      IMONTH  I   Month number (Jan=1, Feb=2,.., Dec=12)
C      IDAY    I   Day of month
C   Outputs:
C      IDAYNO  I   Day number (Jan. 1 = 1)
C      ILOYR   I   No. of days in year (365 or 366)
C----------------------------------------------------------------------
      INTEGER IYR, IMONTH, IDAY, IDAYNO, ILOYR
C
      LOGICAL WLEAP
      INTEGER IDSUM(12)
      DATA IDSUM /0,31,59,90,120,151,181,212,243,273,304,334/
C----------------------------------------------------------------------
      IDAYNO = IDSUM(IMONTH) + IDAY
      ILOYR = 365
C                                       Leap year ?
      WLEAP = (MOD(IYR,4).EQ.0)
C                                       Centurial leap years ?
      IF (WLEAP.AND.(MOD(IYR,100).EQ.0)) WLEAP = (MOD(IYR,400).EQ.0)
C
      IF (WLEAP.AND.(IMONTH.GT.2)) IDAYNO = IDAYNO + 1
      IF (WLEAP) ILOYR = 366
      RETURN
      END
      SUBROUTINE ORTAB (YEAR, DAY, NDAY, DTIMOB, DXYZOB, DVELOB,
     *   RA, DEC, GOOD, SUND, ECLIPS, PA, AMETER, ECCEN, MANCOS, MANSIN,
     *   PERCOS, PERSIN, INCLIN, OMECOS, OMESIN)
C-----------------------------------------------------------------------
C   Routine to derive orbital properties for a spacecraft for
C   inclusion in the orbit table.
C
C   Input:
C      YEAR            I     Year of the reference point
C      DAY             I     The day number of the year starting
C                            with Jan 1.
C      NDAY            I     Number of days in the year
C      DTIMOB          D     Time, days
C      DXYZOB          D(3)  Equatorial coordinates - X,Y,Z, m
C      DVELOB          D(3)  Velocities - Vx, Vy, Vz,  m/sec
C      RA              D     Right assension of the source, radians
C      DEC             D     Declination of the source, radians
C   Output:
C      GOOD            L     Use the point at the orbit restoration?
C      SUND            D     Angular distance between Sun and source,
C                            degrees
C      ECLIPS          L     TRUE if sun is eclipsed by Earth,
C                            seeng from the orbital antenna
C      PA              D     Position angle of major axis of the
C                            polarisation ellipce of the VSOP
C                            antenna feed, measured from north to
C                            east, degrees
C      AMETER          D     Semi-major axis (m)
C      ECCEN           D     Eccentricity
C      INCLIN          D     Inclination of orbit plane to equator (radians)
C      OMECOS          D     Cosine of RA of the ascending node
C      OMESIN          D     Sine of RA of the ascending node
C      PERCOS          D     Cosine of angle of perigee to the ascending
C                            node
C      PERSIN          D     Sine of angle of perigee to the ascending
C                            node
C      MANCOS          D     Cosine of mean anomaly at the reference time
C      MANSIN          D     Sine of mean anomaly at the reference time
C-----------------------------------------------------------------------
      INTEGER DAY, YEAR
      LOGICAL ECLIPS, GOOD
      DOUBLE PRECISION  DTIMOB, DXYZOB(3), DVELOB(3), RA, DEC,
     *   SUND, PA, INCLIN, MANCOS, MANSIN, PERCOS, PERSIN, OMECOS,
     *   OMESIN,
     *   VELX, VELY, VELZ, VEL, R, AMETER, COSFI, FI, SIN2FI,
     *   ECCEN, COSE, E, COSF, TRANOM, MANOM, EN, SIN2I,
     *   ESQ, ASQ, DMDT, DWDT, DODT, DT, COSPF, SINPF, OMEGF,
     *   PERIG, COSOKS, SINOKS, COSKSI, SINKSI, OMEGA
C
      INTEGER  IDAY, IYEAR, NDAY
      REAL FDAY, VSUN(6), ASAO, MAXANG
      DOUBLE PRECISION  X, Y, Z, RAS, DECS
      DOUBLE PRECISION RE, GM, HJ2
      INCLUDE 'INCS:PSTD.INC'
      DATA RE, GM, HJ2 /6378140.0D0, 3.986012D14, 1.08263D-3/
C-----------------------------------------------------------------------
      X = DXYZOB(1)
      Y = DXYZOB(2)
      Z = DXYZOB(3)
      R = SQRT (X * X + Y * Y + Z * Z)
C                                       Start working with Sun
      IDAY = DTIMOB
      FDAY = DTIMOB - IDAY
      IDAY = IDAY + DAY
C                                       If the day number more number
C                                       of days in the year
      IF (IDAY.GT.NDAY) THEN
         IYEAR = YEAR +1
         IDAY = IDAY - NDAY
      ELSE
         IYEAR = YEAR
         END IF
      CALL SUN(IYEAR, IDAY, FDAY, VSUN)
      RAS = ATAN2 (VSUN(2), VSUN(1))
      DECS = ATAN2 (VSUN(3), SQRT (VSUN(1) ** 2 +
     *   VSUN(2) ** 2))
C                                       Angle source - sun
      SUND = COS (DEC) * COS (DECS) * COS (RAS - RA) + SIN (DEC) *
     *   SIN (DECS)
      SUND = ACOS (SUND) * RAD2DG
C                                       Angle between the vectors
C                                       (orbital antenna-sun) and
C                                       (orbital antenna-geocenter)
      ASAO = ACOS(-(X * COS (DECS) * COS (RAS) + Y * COS (DECS) *
     *   SIN (RAS) + Z * SIN (DECS)) / R)
C                                       Angular size of Earth's half as
C                                       seen from the orbital antenna
      MAXANG = ATAN (6370000.0 / R)
      ECLIPS = .FALSE.
      IF(ASAO.LE.MAXANG) ECLIPS = .TRUE.
C                                       Position angle of the feed
C                                       ellipse. Correct only for VSOP.
      PA = ATAN2 ( COS (DECS) * SIN (RAS - RA), (-SIN (DEC) *
     *   COS (DECS) * COS (RAS - RA) + COS (DEC) * SIN (DECS)) )
      PA = PA * RAD2DG
C----------------------------------------------------------------------
C     THE SECOND PART OF THE ROUTINE: RESTORE THE MEAN ORBIT
C----------------------------------------------------------------------
      GOOD = .TRUE.
      VELX = DVELOB(1)
      VELY = DVELOB(2)
      VELZ = DVELOB(3)
      VEL = SQRT (VELX*VELX + VELY*VELY + VELZ*VELZ)
C                                       The first orbit parameter:
C                                       major semiaxis
      AMETER = 1.0D0 / ((2.0D0 /R) - (VEL*VEL) / GM)
C                                       find the angle from radius
C                                       vector to the vector of velocity
      COSFI = (X*VELX + Y*VELY + Z*VELZ) / (VEL * R)
      IF (ABS (COSFI).GT.1) THEN
         GOOD = .FALSE.
         GO TO 999
         END IF
      FI = ACOS (COSFI)
      SIN2FI = 1 - COSFI * COSFI
      ECCEN = SIN2FI * R * (2*AMETER - R) / (AMETER*AMETER)
      IF (ECCEN.GT.1) THEN
         GOOD = .FALSE.
         GO TO 999
         END IF
C                                       The second orbit parameter:
C                                       eccentricity
      ECCEN = SQRT (1 - ECCEN)
C
      COSPF = ( VELZ/VEL - Z/R * COS(FI)) / SIN(FI)
      SINPF = Z / R
      OMEGF = ATAN2 (SINPF, COSPF)
C                                       The third orbit parameter:
C                                       the orbit plane inclination
      INCLIN = SINPF / SIN(OMEGF)
      IF (ABS (INCLIN).GT.1) THEN
         GOOD = .FALSE.
         GO TO 999
         END IF
      INCLIN = ASIN (INCLIN)
C
C                                       Find eccentric anomaly
      COSE = (1 - R / AMETER) / ECCEN
      IF (ABS (COSE).GT.1) THEN
         GOOD = .FALSE.
         GO TO 999
         END IF
      E = ACOS(COSE)
C                                       determine the sign of E using
C                                       the property SIN(E)*COSFI >0
      IF (COSFI.LT.0.0) E = TWOPI - E
C                                       find true anomaly from the two
C                                       equation and compare them
      COSF = ( AMETER * (1 - ECCEN*ECCEN) / R - 1 ) / ECCEN
      IF (ABS (COSF).GT.1) THEN
         GOOD = .FALSE.
         GO TO 999
         END IF
      TRANOM = ACOS (COSF)
      IF (COSFI.LT.0.0) TRANOM = TWOPI - TRANOM
C                                       Find mean anomaly
      MANOM = E - ECCEN * SIN(E)
      EN = DSQRT(GM/AMETER) / AMETER
      SIN2I = SIN(INCLIN)**2
      ESQ = 1.0D0 - ECCEN*ECCEN
      ASQ  = (AMETER / RE)**2
C                                       angular speed of mean anomaly
      DMDT = EN * (1.D0 + (0.75D0*HJ2*(2.D0-3.D0*SIN2I))
     *   / (ASQ*ESQ**1.5D0))
C                                       angular speed of perigee at the
C                                       orbit plane
      DWDT = 3.D0*EN*HJ2 * (4.D0 - 5.D0*SIN2I) / (4.*ASQ*ESQ*ESQ)
C                                       angular speed of ascending node
      DODT = -1.5D0 * HJ2 * COS(INCLIN) *EN / (ASQ*ESQ*ESQ)
C                                       The fourth orbit parameter:
C                                       mean anomaly at the reference
C                                       time
      DT = DTIMOB * 86400.0D0
      MANOM = MANOM - DMDT * DT
      MANCOS = COS(MANOM)
      MANSIN = SIN(MANOM)
C                                       The fifth orbit parameter:
C                                       perihelion at the reference time
C
      PERIG = OMEGF - TRANOM - DWDT * DT
      PERCOS = COS(PERIG)
      PERSIN = SIN(PERIG)
C                                       start calculating angle of the
C                                       ascending node
      COSOKS = X / R
      SINOKS = Y / R
      COSKSI = COS(OMEGF)
      SINKSI = COS(INCLIN) * SIN(OMEGF)
C                                       The sixth orbit parameter:
C                                       angle of ascending node
      OMEGA = ATAN2 (SINOKS, COSOKS) - ATAN2 (SINKSI, COSKSI)
      OMEGA = OMEGA - DODT *DT
      OMECOS = COS(OMEGA)
      OMESIN = SIN(OMEGA)
C
 999  RETURN
      END
      SUBROUTINE SUN (YEAR, DAY, FDAY, SUNCV)
C-----------------------------------------------------------------------
C   Calculate three coordinates of the Sun and its velocity at
C   the equatorial system of coordinates originating at the Earth
C   center.
C   Acuracy for the time interval 1950-2050:
C                      10000 km in position
C                      0.004 km/s in speed
C
C   Inputs:
C      YEAR      I       Year, for example 1996
C      DAY       I       Day in year (1 = Jan 1st)
C      FDAY      R       Fraction of day
C
C   Outputs:
C      SUNCV      R(6)    Sun position & velocity vector
C                        Position part, SUNCV(1-3), is in km;
C                        Velocity part, SUNCV(4-6), is in km/sec.
C-----------------------------------------------------------------------
C
      INCLUDE 'INCS:PSTD.INC'
      INTEGER YEAR, DAY, YEAR4
      REAL SPEED, REMB, SEMB, Y1900, YF, T, ELM, GAMMA, EM, ELT, EPS0,
     *   E, ESQ, V, R, ELMM, COSELT, SINEPS, COSEPS, W1, W2, SELMM,
     *   CELMM, AU, FDAY, SUNCV(6)
C                                       Astronomical union, km
      DATA AU /1.495979E8/
      REMB = 3.12E-5
      SEMB = 8.31E-11
C                                       Mean orbital speed of Earth,
C                                       AU/s
      SPEED = 1.9913E-7
      Y1900 = YEAR-1900.0
      YEAR4 = MOD(MOD(YEAR,4)+4,4)
      YF=(FLOAT( 4*(DAY-1/(YEAR4+1)) - YEAR4 - 2) + 4.0*FDAY) / 1461.0
      T=Y1900 + YF
C                                       Geometric mean longitude of Sun
      ELM = DMOD(4.881628 + TWOPI*YF + 0.00013420*T, TWOPI)
C                                       Mean longitude of perihelion
      GAMMA = 4.908230 + 3.0005E - 4*T
C                                       Mean anomaly
      EM = ELM - GAMMA
C                                       Mean obliquity
      EPS0 = 0.40931975 - 2.27E - 6*T
C                                       Eccentricity
      E = 0.016751 - 4.2E - 7*T
      ESQ = E*E
C                                       True anomaly
      V = EM + 2.0*E*SIN(EM) + 1.25*ESQ*SIN(2.0*EM)

C                                       True ecliptic longitude
      ELT = V + GAMMA
C                                       True distance
      R = (1.0 - ESQ)/(1.0 + E*COS(V))

C                                       Moon's mean longitude
      ELMM = DMOD(4.72D0 + 83.9971D0*T, TWOPI)
C
      COSELT = COS(ELT)
      SINEPS = SIN(EPS0)
      COSEPS = COS(EPS0)
      W1 = -R*SIN(ELT)
      W2 = -SPEED*(COSELT + E*COS(GAMMA))
      SELMM = SIN(ELMM)
      CELMM = COS(ELMM)
C                                       Sun position and velocity
      SUNCV(1) = (R*COSELT + REMB*CELMM) * AU
      SUNCV(2) = -(W1 - REMB*SELMM) * COSEPS *AU
      SUNCV(3) = -W1*SINEPS *AU
      SUNCV(4) = -(SPEED*(SIN(ELT) + E*SIN(GAMMA)) + SEMB*SELMM) * AU
      SUNCV(5) = -(W2 - SEMB*CELMM)*COSEPS * AU
      SUNCV(6) = -W2 * SINEPS * AU
C
 999  RETURN
      END
