      SUBROUTINE AU2 (BRANCH)
C-----------------------------------------------------------------------
C! handles task-related activities: GO, TELL, WAIT, ABORT, SPY, TPUT
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2005, 2007-2008, 2010-2011, 2013-2016
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
C   AU2 handles tasking activities
C   Input:
C      BRANCH   I   1 activate a task (named TASK in /CORE/)
C                   2 print list of active tasks
C                   3 delay until task completes (named TASK in /CORE/)
C                   4 kill a task                (named TASK in /CORE/)
C                   5 Save task adverbs for later TGET
C                   6 TELL running task some parameters
C                   7 STQUEUE queue of pending TELLs
C                   8 SETDEBUG sets debugging global parameter DBGAIP
C                   9 PARALLEL sets degree of parallelism for tasks
C                  10 Save task adverbs for later VGET
C   Inputs file function codes are in column 10 as:
C      ' '   GO
C      '*'   GO  TELL
C      '?'       TELL
C      '&'   GO  TELL  GET
C      '%'       TELL  GET
C      '$'   GO        GET
C      '@'             GET
C      other -> error message
C   INPUTS, TPUT, TGET and GO only do the GO adverbs, OUTPUTS only does
C   the GET adverbs, SHOW and TELL only do the TELL adverbs
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INTEGER   NSYSAD, NSYSTA, NSYSDA, NSYSCH, NSYSIM
      PARAMETER (NSYSAD=36, NSYSTA=2, NSYSDA=9, NSYSCH=4, NSYSIM=11)
C
      CHARACTER ITASK*8, ANAME*6, ATIME*8, CTEST*8, CVER(4)*4, ADATE*12,
     *   PHNAME*48, LVERSN*48, AVERSN*48, JBUF*80, SYSADV(NSYSAD)*8,
     *   CTEMP*4, PRGNAM*6, CDUM*1, OPTELL*4, POPSNO*1, CODE*1,
     *   HHNAME*48
      INTEGER   POTERR, FINDIN, FINDTD, FINDTT, LUNTTY, IPRBUF(256),
     *   IUSER, IVER, NREC, I, JTRIM, IERR, LUNTG, FINDTG, IPOPS,
     *   ITIME(6), NTASK, JTASK, COLLBN, COLUBN, COLCOM, IEOF, ILOC,
     *   IPTR, IRETCD, ITAG, ITYPE, LUNIN, LUNTD, NBLOCK, VERTYP, NWPL,
     *   NLPR, ITEMP, PID(4), ISIZE, LPTR, LPTRMX, COLSHO, IX, IXL, IXH,
     *   IY, IYL, IYH, IREC, LREC, TSBLK, JOFF, NSIZE, LSIZE, NMATCC,
     *   IROUND, IDUM, LPID(4), NPROC, IPRGUF(256), JPTR, ADTYPE
      REAL      PARBUF(256), LMIN, LMAX, LIM(2), TSKDLY, SYSLIM(2), TDL,
     *   RDUM, DOWAIT, PARGUF(256)
      LOGICAL   T, F, ACTIVE, WASDEF, LDOWT, OK(16), ADVGET, TGNEW
      HOLLERITH HARBUF(256), HARGUF(256), HNAME(3)
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCRE.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:PUVD.INC'
      COMMON /AIPSCR/ IPRBUF
      COMMON /AIPCCR/ PHNAME, LVERSN, AVERSN, JBUF
      EQUIVALENCE (IPRBUF(1), PARBUF(1), HARBUF(1))
      EQUIVALENCE (IPRGUF(1), PARGUF(1), HARGUF(1))
      EQUIVALENCE (ADTYPE, HNAME(3))
      DATA PRGNAM /'AU2 '/
      DATA LMIN, LMAX /-1.E16, 1.E16/
      DATA COLSHO, COLLBN, COLUBN, COLCOM /10, 11, 23, 35/
      DATA LUNIN, LUNTD, LUNTTY, LUNTG /11, 13, 5, 14/
      DATA IEOF /2/
      DATA T, F /.TRUE.,.FALSE./
      DATA CVER /'OLD:','NEW:','TST:','CVX:'/
      DATA SYSADV /'INTAPE  ', 'OUTTAPE ', 'INDISK  ', 'IN2DISK ',
     *   'IN3DISK ', 'IN4DISK ', 'DDISK', 'MDISK', 'TDISK', 'OUTDISK ',
     *   'OUT2DISK', 'BCHAN', 'ECHAN', 'CHANSEL', 'CHANNEL',
     *   'BLC', 'TRC', 'TBLC', 'TTRC', 'PIXXY', 'IMSIZE', 'BOX',
     *   'CLBOX', 'FLDSIZE', 'PIX2XY', 'UVSIZE',
     *   'BATQUE', 'NUMTELL', 'PRNUMBER', 'TVCHAN', 'TV2CHAN', 'GRCHAN',
     *   'GR2CHAN', 'TVCORN', 'TVXY', 'TVLEVS'/
C-----------------------------------------------------------------------
      ADVGET = .FALSE.
      NBLOCK = 8
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
      FINDTD = 0
      FINDTG = 0
      FINDIN = 0
      IPOPS = NPOPS
      CALL ADVERB ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOWAIT', 'R', 1, 0, IDUM, DOWAIT, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       get member name from stack
      IF ((BRANCH.EQ.2) .OR. ((BRANCH.GE.7) .AND. (BRANCH.LE.9)))
     *   GO TO 90
         POTERR = 7
         IF (SP.LT.4) GO TO 980
         POTERR = 8
         IF ((STACK(SP).NE.2) .OR. (STACK(SP-3).NE.14) .OR.
     *      (STACK(SP-2).NE.8)) GO TO 980
         WASDEF = CH(STACK(SP-1)).EQ.HBLANK
         IF (.NOT.WASDEF) CALL H2CHR (8, 1, C(STACK(SP-1)), ITASK)
         SP = SP - 4
C                                       open inputs file
         CALL ZPHFIL ('HE', 1, 0, 0, HHNAME, IERR)
         POTERR = 101
C                                       Allow min match
C                                       Return true value
         CALL ADVERB ('VERSION', 'C', 1, 48, IDUM, RDUM, LVERSN)
         IF (ERRNUM.NE.0) GO TO 980
         CALL VERMAT (1, HHNAME, ITASK, LVERSN, VERTYP, AVERSN, IERROR)
         IF (IERROR(1).NE.0) GO TO 980
         IF (ITASK(:4).EQ.'AIPS') THEN
            IF ((BRANCH.NE.3) .OR. (ITASK(:5).NE.'AIPSC')) THEN
               WRITE (MSGTXT,1000)
               CALL MSGWRT (8)
               POTERR = 101
               GO TO 980
               END IF
            END IF
C                                       Branch to operation
 90   GO TO (100, 300, 400, 500, 100, 600, 700, 800, 900, 100), BRANCH
C-----------------------------------------------------------------------
C                                       GO
C                                       start a task, save adverbs
C                                       TPUT
C                                       Just save the adverbs
C                                       VPUT
C                                       Just save the adverbs, versioned
C-----------------------------------------------------------------------
 100  IF (WASDEF) THEN
         CALL ADVRBS ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      CALL ZTOPEN (LUNIN, FINDIN, 1, HHNAME, ITASK, AVERSN, T, IERROR)
      POTERR = 31
      IF (IERROR(1).NE.0) THEN
         FINDIN = 0
         GO TO 980
         END IF
C                                       Name Task adverb save file
      IF (BRANCH.EQ.10) THEN
         CALL ADVERB ('VNUMBER', 'I', 1, 1, IVER, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         IF ((IVER.LE.0) .OR. (IVER.GT.35)) IVER = 1
      ELSE
         IVER = 0
         END IF
      IUSER = 100 * NBLOCK
      IF ((IPOPS.GT.NINTRN) .OR. (TSKNAM(1:5).EQ.'AIPSC'))
     *   IUSER = IUSER + IPOPS
C                                       prime TV parameters
      IF (NGRAPH.LE.0) THEN
         IF (ISBTCH.EQ.32000) THEN
            NGRAY = 16
            NGRAPH = 8
            OFMINP = 16 * 2046
            MAXXTV(1) = 1024
            MAXXTV(2) = 1024
         ELSE
            MSGSUP = 32000
            CALL TVOPEN (IPRBUF, IERR)
            IF (IERR.EQ.0) CALL TVCLOS (IPRBUF, IERR)
            MSGSUP = 0
            END IF
         END IF
C                                       Open Task adverb save file
      CALL ZPHFIL ('TG', 1, IUSER, IVER, PHNAME, IERR)
      CALL ZEXIST (1, PHNAME, NSIZE, IERR)
      POTERR = 101
      TGNEW = .FALSE.
      IF (IERR.EQ.1) THEN
         NSIZE = 64
         CALL ZCREAT (1, PHNAME, NSIZE, F, LSIZE, IERR)
         IF (IERR.EQ.0) NSIZE = LSIZE
         TGNEW = .TRUE.
         END IF
      IF (IERR.GE.2) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (6)
         GO TO 980
         END IF
      CALL ZOPEN (LUNTG, FINDTG, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         FINDTG = 0
         GO TO 980
         END IF
C                                       Init file record 1
      IPRBUF(1) = NSIZE
      IPRBUF(2) = 0
      IPRBUF(3) = 0
      IPRBUF(4) = 0
      IPRBUF(5) = NBLOCK
      POTERR = 50
      NWPL = 5
      NLPR = 51
C                                       Read old record 1
      IF (.NOT.TGNEW) THEN
         CALL ZFIO ('READ', LUNTG, FINDTG, 1, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         NBLOCK = MAX (8, IPRBUF(5))
         END IF
C                                       Search for task name
 115  LREC = 1
      NTASK = IPRBUF(2)
C                                       Self protection
      IF ((NTASK.GE.0) .AND. (NTASK.LE.508) .AND. (IPRBUF(5).EQ.NBLOCK)
     *   .AND. (IPRBUF(1).GT.0) .AND. (IPRBUF(1).LE.4090)) GO TO 120
         WRITE (MSGTXT,1115) IPRBUF(1), IPRBUF(2)
         CALL MSGWRT (8)
         TSBLK = -1143
         GO TO 160
 120  DO 130 JTASK = 1,NTASK
         IREC = JTASK / NLPR + 1
C                                       Read next rec
         IF (IREC.NE.LREC) THEN
            CALL ZFIO ('READ', LUNTG, FINDTG, IREC, IPRBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            LREC = IREC
            END IF
         ITEMP = MOD (JTASK, NLPR) * NWPL + 1
         CALL H2CHR (8, 1, IPRBUF(ITEMP), CTEST)
         IF (CTEST.EQ.ITASK) GO TO 140
 130     CONTINUE
C                                       Add task to directory
      JTASK = NTASK + 1
 140  IREC = JTASK / NLPR + 1
      I = MOD (JTASK, NLPR) * NWPL  +  1
      IF ((IREC.NE.LREC) .AND. (I.GT.1)) THEN
         CALL ZFIO ('READ', LUNTG, FINDTG, IREC, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      CALL CHR2H (8, ITASK, 1, IPRBUF(I))
      CALL CATIME (1, IPRBUF(I+2), ITIME)
      IPRBUF(I+4) = VERTYP
C                                       Save & get rec 1 back
      IF (IREC.GT.1) THEN
         CALL ZFIO ('WRIT', LUNTG, FINDTG, IREC, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         CALL ZFIO ('READ', LUNTG, FINDTG, 1, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       Save task count, expand file
      IPRBUF(2) = MAX (NTASK, JTASK)
      TSBLK = NBLOCK * (JTASK-1) + 11
      CALL CATIME (1, IPRBUF(3), ITIME)
      IF (TSBLK+NBLOCK-1.GT.IPRBUF(1)) THEN
         NREC = 16
         CALL ZEXPND (LUNTG, 1, PHNAME, NREC, IERR)
         IF (IERR.NE.0) GO TO 980
         IPRBUF(1) = IPRBUF(1) + NREC
         END IF
C                                       Return Record 1
      CALL ZFIO ('WRIT', LUNTG, FINDTG, 1, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       open output file
 160  IF (BRANCH.EQ.1) THEN
         CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
         CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
         POTERR = 101
         IF (IERR.NE.0) THEN
            FINDTD = 0
            GO TO 980
            END IF
         END IF
C                                       More self protection
      IF ((TSBLK.LT.11) .AND. (TSBLK.NE.-1143)) THEN
         WRITE (MSGTXT,1160) TSBLK
         CALL MSGWRT (8)
         TSBLK = -1143
         END IF
C                                      initial values
      JOFF = (IPOPS-1)*4 + 2
      CALL FILL (256, 0, IPRBUF)
      IPTR = 11
      JPTR = IPTR
      LPTR = IPTR
      LPTRMX = 256 * 4
      IPRBUF(1) = NLUSER
      IPRBUF(2) = NTVDEV
      IPRBUF(3) = NTKDEV
      IF ((ISBTCH.EQ.32000) .OR. (IPOPS.GT.NINTRN+1)) IPRBUF(2) = 0
      IF ((ISBTCH.EQ.32000) .OR. (IPOPS.GT.NINTRN+1)) IPRBUF(3) = 0
      IPRBUF(4) = MSGKIL
      IPRBUF(5) = ISBTCH
      IPRBUF(6) = DBGAIP
      CALL ADVERB ('QCREATE', 'I', 1, 1, I, RDUM, CDUM)
      IF (ERRNUM.NE.0) THEN
         MSGTXT = 'DO THE COMPRESS VERB TO ENABLE THE QCREATE ADVERB'
         CALL MSGWRT (6)
         ERRNUM = 0
      ELSE
         QCREAT = I
         END IF
      IPRBUF(7) = QCREAT
      IPRBUF(8) = 0
      PARBUF(IPTR-2) = DOWAIT
      CALL CHR2H (4, VERNAM, 1, PARBUF(IPTR-1))
      IF ((VERTYP.GE.1) .AND. (VERTYP.LE.4)) CALL CHR2H (4,
     *   CVER(VERTYP), 1, PARBUF(IPTR-1))
      IF ((VERTYP.GE.5) .AND. (VERTYP.LE.8)) CALL CHR2H (4,
     *   CVER(VERTYP-3), 1, PARBUF(IPTR-1))
      CALL COPY (256, IPRBUF, IPRGUF)
C                                       skip first 2 records
 180  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      POTERR = 59
      IF (IERR.NE.0) GO TO 980
      IF (JBUF(1:1).EQ.';') GO TO 180
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 980
         END IF
 185  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (JBUF(1:1).EQ.';') GO TO 185
      IF (JBUF(1:8).EQ.'--------') THEN
         POTERR = 54
         GO TO 980
         END IF
C                                       loop for adverbs
 210  CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.EQ.IEOF) GO TO 250
         POTERR = 59
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 210
         IF (JBUF(1:8).EQ.'--------') GO TO 250
         I = JTRIM (JBUF)
C                                       Decode adverb usage
         CODE = JBUF(COLSHO:COLSHO)
C                                       go tell get
         IF (CODE.EQ.'&') THEN
            ADVGET = .TRUE.
C                                       -- tell get
         ELSE IF (CODE.EQ.'%') THEN
            ADVGET = .TRUE.
            IF (BRANCH.NE.6) GO TO 210
C                                       go ---- get
         ELSE IF (CODE.EQ.'$') THEN
            ADVGET = .TRUE.
            IF (BRANCH.EQ.6) GO TO 210
C                                       -- ---- get
         ELSE IF (CODE.EQ.'@') THEN
            ADVGET = .TRUE.
            GO TO 210
C                                       -- tell ---
         ELSE IF (CODE.EQ.'?') THEN
            IF (BRANCH.NE.6) GO TO 210
C                                       go ---- ---
         ELSE IF (CODE.EQ.' ') THEN
            IF (BRANCH.EQ.6) GO TO 210
C                                       go tell --- code '*'
         ELSE IF (CODE.NE.'*') THEN
            MSGTXT = 'WRONG ADVERB CODE IN INPUTS'
            CALL MSGWRT (6)
            GO TO 210
            END IF
         JBUF(COLSHO:COLSHO) = ' '
         KARBUF = JBUF
         KBPTR = NMATCC (1, ' ', KARBUF)
C                                       comment
         IF (KBPTR.GE.COLLBN) GO TO 210
C                                       adverb: is it system value?
         SYSLIM(1) = -1.0
         SYSLIM(2) = -2.0
         DO 212 I = 1,NSYSAD
            IF (KARBUF(1:8).EQ.SYSADV(I)) THEN
               IF (I.LE.NSYSTA) THEN
                  SYSLIM(2) = NTAPED
               ELSE IF (I.LE.NSYSTA+NSYSDA) THEN
                  SYSLIM(2) = NVOL
               ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH) THEN
                  SYSLIM(2) = MAXCHA
               ELSE IF (I.LE.NSYSTA+NSYSDA+NSYSCH+NSYSIM) THEN
                  SYSLIM(2) = MAXIMG
               ELSE
                  SYSLIM(2) = NINTRN + 1 + NBATQS
                  IF (SYSADV(I).EQ.'BATQUE') SYSLIM(2) = NBATQS
                  IF (SYSADV(I).EQ.'TVCHAN') SYSLIM(2) = NGRAY
                  IF (SYSADV(I).EQ.'TV2CHAN') SYSLIM(2) = NGRAY
                  IF (SYSADV(I).EQ.'GRCHAN') SYSLIM(2) = NGRAPH
                  IF (SYSADV(I).EQ.'GR2CHAN') SYSLIM(2) = NGRAPH
                  IF (SYSADV(I).EQ.'TVLEVS') SYSLIM(2) = OFMINP
                  IF (SYSADV(I).EQ.'TVXY') SYSLIM(2) =
     *               MAX (MAXXTV(1), MAXXTV(2))
                  IF (SYSADV(I).EQ.'TVCORN') SYSLIM(2) =
     *               MAX (MAXXTV(1), MAXXTV(2))
                  END IF
               GO TO 213
               END IF
 212        CONTINUE
C                                       get the adverb
 213     CALL GETFLD
         IF (ERRNUM.EQ.2) THEN
            MSGTXT = 'MISSING ADVERB: TRY A ''COMPRESS'' OR '
     *         // '''HELP NOADVERB'' FOR INFO'
            CALL MSGWRT (8)
            END IF
         IF (ERRNUM.NE.0) GO TO 980
         ITAG = TAG
         ILOC = LOCSYM
         ITYPE = SYTYPE
         LIM(1) = LMIN
         LIM(2) = LMAX
C                                       Type 7 is a string.
         IF (ITYPE.EQ.7) GO TO 225
         POTERR = 8
C                                       Type 1 is a scalar, 2=array.
         IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2)) GO TO 980
C                                       find limits: reals, arrays
 215     KBPTR = NMATCC (KBPTR, ' ', KARBUF)
         IF (KBPTR.LT.COLCOM) THEN
            I = 2
            IF (KBPTR.LT.COLUBN) I = 1
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
C                                       negative
            IF (TAG.EQ.6) THEN
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
               LIM(I) = -C(TAG)
            ELSE
               LIM(I) = C(TAG)
               END IF
            GO TO 215
            END IF
C                                       values to buffer
 225     ISIZE = 1
         IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
         IF (SYSLIM(2).GE.SYSLIM(1)) LIM(2) = SYSLIM(2)
C                                       Check character strings
         IF (ITYPE.EQ.7) THEN
            CALL VERSTR (ILOC)
            IF ((ERRNUM.NE.0).AND.(BRANCH.EQ.1)) GO TO 980
            IF ((ERRNUM.NE.0).AND.(BRANCH.GT.1)) GO TO 980
            END IF
C                                       output TGET adverb name, type
         ADTYPE = 10 * ISIZE + ITYPE
         CALL CHR2H (8, KARBUF, 1, HNAME)
         DO 230 I = 1,3
            HARGUF(JPTR) = HNAME(I)
            JPTR = JPTR + 1
            IF (JPTR.GT.256) THEN
               IF ((TSBLK.GE.4) .AND. (BRANCH.NE.6)) CALL ZFIO ('WRIT',
     *            LUNTG, FINDTG, TSBLK, IPRGUF, IERR)
               IF (IERR.NE.0) GO TO 980
               TSBLK = TSBLK + 1
               CALL FILL (256, 0, IPRGUF)
               JPTR = 1
               END IF
 230        CONTINUE
         DO 245 I = 1,ISIZE
            IF (ITYPE.EQ.7) THEN
               HARBUF(IPTR) = CH(ITAG)
               HARGUF(JPTR) = CH(ITAG)
            ELSE
               PARBUF(IPTR) = C(ITAG)
               PARGUF(JPTR) = C(ITAG)
C                                       Check value range
               CALL BOUNDS (C(ITAG), C(ITAG), LIM, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1235) JBUF(1:10)
                  CALL MSGWRT (6)
                  POTERR = 32
                  GO TO 980
                  END IF
               END IF
 240        ITAG = ITAG + 1
            IPTR = IPTR + 1
            JPTR = JPTR + 1
            LPTR = LPTR + 1
            IF (LPTR.GT.LPTRMX) THEN
               WRITE (MSGTXT,1240)
               CALL MSGWRT (8)
               POTERR = 32
               GO TO 980
               END IF
            IF (IPTR.GT.256) THEN
               IPTR = 1
               IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.6)) CALL ZFIO ('WRIT',
     *            LUNTD, FINDTD, JOFF, IPRBUF, IERR)
               POTERR = 50
               IF (IERR.NE.0) GO TO 980
               JOFF = JOFF + 1
               CALL FILL (256, 0, IPRBUF)
               END IF
            IF (JPTR.GT.256) THEN
               IF ((TSBLK.GE.4) .AND. (BRANCH.NE.6)) CALL ZFIO ('WRIT',
     *            LUNTG, FINDTG, TSBLK, IPRGUF, IERR)
               IF (IERR.NE.0) GO TO 980
               TSBLK = TSBLK + 1
               CALL FILL (256, 0, IPRGUF)
               JPTR = 1
               END IF
 245        CONTINUE
         GO TO 210
C
C                                       clear last block
 250  IF (IPTR.GT.1) THEN
         IERR = 0
         IF ((BRANCH.EQ.1) .OR. (BRANCH.EQ.6)) CALL ZFIO ('WRIT', LUNTD,
     *      FINDTD, JOFF, IPRBUF, IERR)
         POTERR = 50
         IF (IERR.NE.0) GO TO 980
         IF ((TSBLK.GE.4) .AND. (BRANCH.NE.6)) CALL ZFIO ('WRIT', LUNTG,
     *      FINDTG, TSBLK, IPRGUF, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       Close files
      CALL ZTCLOS (LUNIN, FINDIN, IERR)
      IF (BRANCH.NE.6) THEN
         CALL ZCLOSE (LUNTG, FINDTG, IERR)
         FINDTG = 0
      ELSE
         CALL ZCLOSE (LUNTD, FINDTD, IERR)
         FINDTD = 0
         END IF
      FINDIN = 0
      IF (BRANCH.NE.1) GO TO 999
C                                       Only GO from here on
C                                       DOWAIT changes ?
C      IF ((DOWAIT.LE.0.0) .AND. (ADVGET)) THEN
C         JOFF = (IPOPS-1)*NBLOCK + 2
C         CALL ZFIO ('READ', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
C         POTERR = 50
C         IF (IERR.NE.0) GO TO 980
C         DOWAIT = 1.0
C         PARBUF(9) = DOWAIT
C         CALL ZFIO ('WRIT', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
C         POTERR = 50
C         IF (IERR.NE.0) GO TO 980
C         END IF
C                                        pointer block (#1)
      CALL ZFIO ('READ', LUNTD, FINDTD, 1, IPRBUF, IERR)
      POTERR = 50
      IF (IERR.NE.0) GO TO 980
      ITEMP = 5*(IPOPS-1)
      CALL CHR2H (8, ITASK, 1, IPRBUF(1+ITEMP))
      IPRBUF(3+ITEMP) = -999
      IPRBUF(4+ITEMP) = 0
      IPRBUF(5+ITEMP) = 0
      IPRBUF(256) = 4
      CALL ZFIO ('WRIT', LUNTD, FINDTD, 1, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       close task data file
      CALL ZCLOSE (LUNTD, FINDTD, IERR)
      FINDTD = 0
C                                       task already running
      CALL BLDTNM (ITASK, IPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
      CALL FILL (4, 0, PID)
      PID(1) = NLUSER
      CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
      IF (IERR.NE.0) GO TO 980
      POTERR = 42
      IF (ACTIVE) GO TO 980
C                                        close message file
      CALL MSGWRT (-1)
C                                        close input terminal
      IF ((NPOPS.LE.NINTRN+1) .AND. (ISBTCH.NE.32000)) THEN
         CALL LSERCH ('SRCH', LUNTTY, FINDTT, F, IERR)
         POTERR = 58
         IF (IERR.NE.0) GO TO 980
         CALL ZCLOSE (LUNTTY, FINDTT, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                        activate the task
      CALL ZACTV8 (ITASK, IPOPS, AVERSN, PID, IERR)
      POTERR = 101
      IF (IERR.NE.0) GO TO 950
      IF (ADVGET) THEN
         MSGTXT = 'Waiting for returned adverbs'
         CALL MSGWRT (2)
         END IF
C                                       Wait for resumption through
C                                       TD file or abort of task
      TDL = 1.25
      IF (DOWAIT.GT.0.0) TDL = 2.01
      IF (NPOPS.GT.NINTRN+1) TDL = 15.
      IF (ISBTCH.EQ.32000) TDL = 15.
      LDOWT = TDL.GT.2.0
      IF (AIPSMK.GT.0.1) TDL = TDL / AIPSMK
      CALL TASKWT (ANAME, IPOPS, PID, LDOWT, TDL, IPRBUF, IRETCD, IERR)
C                                       ABORT not resume via TD
      IF (IRETCD.EQ.-999) CALL DESCR (IPRBUF)
C                                        reopen terminal
      IF ((NPOPS.LE.NINTRN+1) .AND. (ISBTCH.NE.32000))
     *   CALL ZOPEN (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
C                                       resumption message
      MSGTXT = 'Resumes'
      CALL MSGWRT (2)
      IF (IRETCD.NE.0) THEN
         POTERR = 101
C                                       abort batch
         IF ((NPOPS.GT.NINTRN+1) .OR. (ISBTCH.EQ.32000)) POTERR = 100
         WRITE (MSGTXT,1285) IRETCD
         IF (IRETCD.NE.-999) CALL MSGWRT (9)
         GO TO 980
         END IF
C                                       get returned adverbs
      IF (ADVGET) THEN
         CALL ZTOPEN (LUNIN, FINDIN, 1, HHNAME, ITASK, AVERSN, T,
     *      IERROR)
         POTERR = 31
         IF (IERROR(1).NE.0) THEN
            FINDIN = 0
            GO TO 980
            END IF
         CALL ZPHFIL ('TD', 1, 0, 4, PHNAME, IERR)
         CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
         POTERR = 101
         IF (IERR.NE.0) THEN
            FINDTD = 0
            GO TO 980
            END IF
         JOFF = (IPOPS-1)*4 + 2
         CALL ZFIO ('READ', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 50
            GO TO 980
            END IF
         IPTR = 11
         LPTR = IPTR
         LPTRMX = 256 * 4
C                                       skip first 2 records
 255     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         POTERR = 59
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 255
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
 260     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
         IF (IERR.NE.0) GO TO 980
         IF (JBUF(1:1).EQ.';') GO TO 260
         IF (JBUF(1:8).EQ.'--------') THEN
            POTERR = 54
            GO TO 980
            END IF
C                                       loop for adverbs
 265     CALL ZTREAD (LUNIN, FINDIN, JBUF, IERR)
            IF (IERR.EQ.IEOF) GO TO 290
            POTERR = 59
            IF (IERR.NE.0) GO TO 980
            IF (JBUF(1:1).EQ.';') GO TO 265
            IF (JBUF(1:8).EQ.'--------') GO TO 290
            I = JTRIM (JBUF)
C                                       Decode adverb usage
            CODE = JBUF(COLSHO:COLSHO)
C                                       get codes only
            IF ((CODE.NE.'&') .AND. (CODE.NE.'%') .AND.(CODE.NE.'$')
     *         .AND. (CODE.NE.'@')) GO TO 265
            JBUF(COLSHO:COLSHO) = ' '
            KARBUF = JBUF
            KBPTR = NMATCC (1, ' ', KARBUF)
C                                       comment
            IF (KBPTR.GE.COLLBN) GO TO 265
C                                       adverb
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            ITAG = TAG
            ILOC = LOCSYM
            ITYPE = SYTYPE
C                                       Type 7 is a string.
            POTERR = 8
C                                       Type 1 is a scalar, 2=array.
            IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2) .AND. (SYTYPE.NE.7))
     *         GO TO 980
            ISIZE = 1
            IF (ITYPE.NE.1) ISIZE = K(K(ILOC+3))
            DO 270 I = 1,ISIZE
               IF (ITYPE.EQ.7) THEN
                  CH(ITAG) = HARBUF(IPTR)
               ELSE
                  C(ITAG) = PARBUF(IPTR)
                  END IF
               ITAG = ITAG + 1
               IPTR = IPTR + 1
               IF (IPTR.GT.256) THEN
                  IPTR = 1
                  JOFF = JOFF + 1
                  CALL ZFIO ('READ', LUNTD, FINDTD, JOFF, IPRBUF, IERR)
                  IF (IERR.NE.0) THEN
                     POTERR = 50
                     GO TO 980
                     END IF
                  END IF
 270           CONTINUE
C                                       Check character strings
            IF (ITYPE.EQ.7) THEN
               CALL VERSTR (ILOC)
               IF (ERRNUM.NE.0) GO TO 980
               END IF
            GO TO 265
C                                       Done !
 290     POTERR = 0
         CALL ZTCLOS (LUNIN, FINDIN, IERR)
         CALL ZCLOSE (LUNTD, FINDTD, IERR)
         FINDIN = 0
         FINDTD = 0
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                        SPY
C                                        list running tasks
C                                        Z routines do actual lists now
C-----------------------------------------------------------------------
 300  CALL ADVERB ('DOALL', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ZTQSPY (RDUM, IPRBUF)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                        WAITTASK
C                                        wait for task to finish
C-----------------------------------------------------------------------
 400  CALL BLDTNM (ITASK, IPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
      I = 0
      CALL FILL (4, 0, PID)
      PID(1) = NLUSER
 405  CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        delay & try again
      IF (.NOT.ACTIVE) GO TO 410
         TSKDLY = 2.0
         IF (AIPSMK.GT.0.1) TSKDLY = 2.0 / SQRT (AIPSMK)
         CALL ZDELAY (TSKDLY, IERR)
         POTERR = 44
         IF (IERR.NE.0) GO TO 980
         I = 1
         GO TO 405
C                                        done
 410  CONTINUE
         IF (I.EQ.1) WRITE (MSGTXT,1410) ITASK
         IF (I.NE.1) WRITE (MSGTXT,1411) ITASK
         CALL MSGWRT (3)
         GO TO 999
C-----------------------------------------------------------------------
C                                        ABORTASK
C                                        abort specified task
C-----------------------------------------------------------------------
 500  CALL BLDTNM (ITASK, IPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
      CALL FILL (4, 0, PID)
      PID(1) = NLUSER
      CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
      IF (IERR.NE.0) GO TO 980
C                                        already gone
      IF (ACTIVE) GO TO 510
         WRITE (MSGTXT,1500) ITASK
         CALL MSGWRT (7)
         GO TO 999
C                                        kill it!
 510  CALL ZTABOR (ANAME, PID, IERR)
      IF (IERR.EQ.0) GO TO 520
         WRITE (MSGTXT,1510) ITASK, IERR
         CALL MSGWRT (7)
         GO TO 999
C                                       Clear scratch if possible.
 520  TDL = 1.0
      IF (AIPSMK.GT.0.5) TDL = TDL / SQRT (AIPSMK)
      I = 0
 525  CALL ZDELAY (TDL, IERR)
         CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
         IF (IERR.NE.0) GO TO 980
         I = I + 1
         IF ((ACTIVE) .AND. (I.LT.11)) GO TO 525
      CALL DESCR (IPRBUF)
      GO TO 999
C-----------------------------------------------------------------------
C                                       TELL
C                                       running task some parameters
C-----------------------------------------------------------------------
 600  IF (WASDEF) THEN
         CALL ADVRBS ('TASK', 'C', 1, 8, IDUM, RDUM, ITASK)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      CALL ADVERB ('NUMTELL', 'I', 1, 0, IPOPS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF (IPOPS.LT.1) IPOPS = NPOPS
      CALL ZTOPEN (LUNIN, FINDIN, 1, HHNAME, ITASK, AVERSN, T, IERROR)
      POTERR = 31
      IF (IERROR(1).NE.0) THEN
         FINDIN = 0
         GO TO 980
         END IF
C                                       Is the task active?
      CALL BLDTNM (ITASK, IPOPS, ANAME, IERR)
      POTERR = 43
      IF (IERR.NE.0) GO TO 980
      CALL FILL (4, 0, LPID)
      LPID(1) = NLUSER
      CALL ZTACTQ (ANAME, LPID, ACTIVE, IERR)
      IF (IERR.NE.0) GO TO 980
      POTERR = 101
      IF (ACTIVE) GO TO 610
         WRITE (MSGTXT,1500) ITASK
         CALL MSGWRT (7)
         GO TO 980
C                                       Open data file
 610  CALL ZPHFIL ('TC', 1, 0, 1, PHNAME, IERR)
      CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         FINDTG = 0
         GO TO 980
         END IF
      IREC = (IPOPS - 1) / 2 + 1
      IXL = (IPOPS + 1 - 2 * IREC) * 128 + 1
      CALL ZFIO ('READ', LUNTD, FINDTD, IREC, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Check/clear slots, find 1
      ITEMP = 0
 611  ITEMP = ITEMP + 1
      IX = IXL - 8
      IXH = 0
      DO 620 I = 1,16
         IX = IX + 8
         OK(I) = .FALSE.
         ACTIVE = .FALSE.
         IF (IPRBUF(IX).EQ.0) GO TO 615
         CALL H2CHR (8, 1, IPRBUF(IX), CTEST)
         IF (CTEST.EQ.' ') GO TO 615
         IF ((IPRBUF(IX+4).GT.0) .AND. (IPRBUF(IX+4).NE.NLUSER) .AND.
     *      (ITEMP.EQ.1)) GO TO 620
         CALL BLDTNM (CTEST, IPOPS, ANAME, IERR)
         IF (IERR.EQ.0) THEN
            CALL FILL (4, 0, PID)
            PID(1) = IPRBUF(IX+4)
            PID(2) = IPRBUF(IX+6)
            IF (PID(1).LE.0) PID(1) = NLUSER
            CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
            END IF
 615     IF ((.NOT.ACTIVE) .OR. (IERR.NE.0)) THEN
            CALL FILL (8, 0, IPRBUF(IX))
            OK(I) = .TRUE.
            IF ((I.EQ.1) .OR. (.NOT.OK(I-1))) IXH = IX
            END IF
 620     CONTINUE
      IF (IXH.GT.0) GO TO 630
         IF (ITEMP.LT.2) GO TO 611
            WRITE (MSGTXT,1620)
            CALL MSGWRT (7)
            GO TO 980
C                                       Install task
 630  CALL CHR2H (8, ITASK, 1, IPRBUF(IXH))
      CALL CATIME (1, IPRBUF(IXH+2), ITIME)
      CALL ADVERB ('OPTELL', 'C', 1, 4, IDUM, RDUM, OPTELL)
      IPRBUF(IXH+4) = NLUSER
      CALL CHR2H (4, OPTELL, 1, PARBUF(IXH+5))
      IPRBUF(IXH+6) = LPID(2)
      IPRBUF(IXH+7) = 0
      CALL ZFIO ('WRIT', LUNTD, FINDTD, IREC, IPRBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       data parms
      JOFF = 19 + (IXH-IXL)/8 + 16 * (IPOPS-1)
      TSBLK = -1143
      IPTR = 2
      LPTR = 2
      CALL FILL (256, 0, IPRBUF)
      CALL CHR2H (4, OPTELL, 1, PARBUF(1))
      LPTRMX = 256
      GO TO 180
C-----------------------------------------------------------------------
C                                       STQUEUE
C                                       show queue of waiting TELLS
C-----------------------------------------------------------------------
 700  CALL ADVERB ('NUMTELL', 'I', 1, 0, IPOPS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOALL', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      IF ((RDUM.GT.0.0) .OR. (IPOPS.LE.0) .OR.
     *   (IPOPS.GT.NINTRN+1+NBATQS)) THEN
         IYL = 1
         IYH = NINTRN
         IF (NBATQS.GT.0) IYH = NINTRN + 1 + NBATQS
      ELSE
         IYL = IPOPS
         IYH = IPOPS
         END IF
 710  POTERR = 50
      CALL ZPHFIL ('TC', 1, 0, 1, PHNAME, IERR)
      CALL ZOPEN (LUNTG, FINDTG, 1, PHNAME, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         FINDTG = 0
         GO TO 980
         END IF
      LREC = 0
      CALL ZTIME (ITIME(4))
      CALL ZDATE (ITIME(1))
      CALL TIMDAT (ITIME(4), ITIME(1), ATIME, ADATE)
      WRITE (MSGTXT,1710) IYL, IYH, ADATE, ATIME
      CALL REFRMT (MSGTXT, '_', I)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1711)
      CALL MSGWRT (2)
      DO 730 IY = IYL,IYH
         IREC = (IY - 1) / 2 + 1
         IF (IREC.EQ.LREC) GO TO 715
            CALL ZFIO ('READ', LUNTG, FINDTG, IREC, IPRBUF, IERR)
            IF (IERR.NE.0) GO TO 980
            LREC = IREC
 715     IXL = (IY + 1 - 2 * LREC) * 128 + 1
         IX = IXL - 8
         DO 725 I = 1,16
            IX = IX + 8
            IF (IPRBUF(IX).EQ.0) GO TO 725
            CALL H2CHR (8, 1, IPRBUF(IX), ITASK)
            IF (ITASK.EQ.' ') GO TO 725
            IF (IPRBUF(IX+4).LE.0) GO TO 725
               CALL CATIME (2, IPRBUF(IX+2), ITIME)
               CALL TIMDAT (ITIME(4), ITIME(1), ATIME, ADATE)
               CTEST = ' '
               CALL BLDTNM (ITASK, IY, ANAME, IERR)
               IF (IERR.NE.0) GO TO 720
                  CALL FILL (4, 0, PID)
                  PID(1) = IPRBUF(IX+4)
                  PID(2) = IPRBUF(IX+6)
                  IF (PID(1).LE.0) PID(1) = NLUSER
                  CALL ZTACTQ (ANAME, PID, ACTIVE, IERR)
 720           CALL H2CHR (4, 1, IPRBUF(IX+5), CTEMP)
               IF ((.NOT.ACTIVE) .OR. (IERR.NE.0)) CTEST = '****'
               CALL ZEHEX (IY, 1, POPSNO)
               WRITE (MSGTXT,1720) ITASK, POPSNO, ADATE, ATIME,
     *            IPRBUF(IX+4), CTEMP, CTEST
               CALL MSGWRT (2)
               IF (CTEST.EQ.'****') CALL FILL (8, 0, IPRBUF(IX))
 725        CONTINUE
         CALL ZFIO ('WRIT', LUNTG, FINDTG, LREC, IPRBUF, IERR)
         IF (IERR.NE.0) GO TO 980
 730     CONTINUE
      CALL ZCLOSE (LUNTG, FINDTG, IERR)
      FINDTG = 0
      POTERR = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       SETDEBUG
C                                       set the parameter DBGAIP
C-----------------------------------------------------------------------
 800  IF (SP.GT.0) THEN
         POTERR = 8
         IF ((SP.GE.4) .AND. (STACK(SP).EQ.2)) GO TO 980
         DBGAIP = IROUND (V(SP))
         SP = SP - 1
      ELSE
         DBGAIP = 0
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                       PARALLEL
C                                       Set number of threads or
C                                       processors for subsequent
C                                       parallel tasks
C-----------------------------------------------------------------------
C                                       Get number
  900 IF (SP.GT.0) THEN
C                                       Syntax error - argument is array
         IF ((SP.GE.4) .AND. (STACK(SP).EQ.2)) THEN
            POTERR = 8
            GO TO 980
         ELSE
            NPROC = NINT (V(SP))
            SP = SP - 1
            END IF
      ELSE
         NPROC = 0
         END IF
      CALL ZPARAL (NPROC)
      WRITE (MSGTXT,1900) NPROC
      CALL MSGWRT (5)
      GO TO 999
C-----------------------------------------------------------------------
C                                       errors: close files
 950  IF ((NPOPS.LE.NINTRN+1) .AND. (ISBTCH.NE.32000))
     *   CALL ZOPEN (LUNTTY, FINDTT, 1, PHNAME, F, T, T, IERR)
C                                       POPS error management
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
      IF (FINDTD.GT.0) CALL ZCLOSE (LUNTD, FINDTD, IERR)
      IF (FINDTG.GT.0) CALL ZCLOSE (LUNTG, FINDTG, IERR)
      IF (FINDIN.GT.0) CALL ZTCLOS (LUNIN, FINDIN, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANNOT DO THIS WITH AIPS, AIPSC, OR AIPSB')
 1100 FORMAT ('UNABLE TO CREATE ADVERB SAVE FILE.  IER=',I7)
 1115 FORMAT ('WARNING: TGET FILE HAS BEEN DAMAGED:',2I8)
 1160 FORMAT ('SOMEHOW TASK SAVE REC #',I7,' WAS REQUESTED & IGNORED')
 1235 FORMAT ('ERROR IS IN VALUE OF ADVERB ',A10)
 1240 FORMAT ('TOO MANY PARAMETERS FOR COMM FILE --- SEE THE',
     *   ' PROGRAMMER')
 1285 FORMAT ('RETURN CODE',I7,' RECEIVED: STOPPING')
 1410 FORMAT ('Task ',A8,'has finished')
 1411 FORMAT (A8,' was not found to be running')
 1500 FORMAT ('TASK ',A8,'WAS NOT RUNNING')
 1510 FORMAT ('TASK ',A8,'WAS NOT KILLED: ERR #',I5)
 1620 FORMAT ('NO ROOM: 16 TELLS ALREADY WAITING')
 1710 FORMAT ('Pending TELL operations on POPS',I3,' -',I3,' at ',A12,
     *   A8)
 1711 FORMAT ('Task',13X,'Queued',11X,'User',4X,'OP',6X,'Error')
 1720 FORMAT (A5,A1,4X,A12,A8,4X,I4,4X,A4,5X,A4)
 1900 FORMAT ('Subsequent parallel tasks will use ', I3, ' threads')
      END
