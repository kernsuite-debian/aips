LOCAL INCLUDE 'OFLAG.INC'
      INCLUDE 'INCS:DOFV.INC'
      HOLLERITH XINNAM(3), XINCLS(2)
      REAL      XSEQ, XDISK, XVER, XFLAG, XFGOUT, DETIME, DOFLAG,
     *   FPARM(30), BADD(10)
      CHARACTER INNAM*12, INCLS*6, INEXT*2, ANNAME*8, OBSCOD*8, OBSDAT*8
      INTEGER   SCRBUF(256), OFBUFF(512), INSEQ, INDISK, INVERS, CNO,
     *   IUSER, IOFRNO, OFKOLS(MAXOFC), OFNUMV(MAXOFC), TABVER, NPOL,
     *   NIF, FGVERI, FGVERO, FGBUFF(512), IDOFLG, NFLBIT(32), NFLLEV,
     *   NFLAG, NANT(513), NBDBIT(32)
      COMMON /INPARM/ XINNAM, XINCLS, XSEQ, XDISK, XVER, XFLAG, XFGOUT,
     *   DETIME, DOFLAG, FPARM, BADD
      COMMON /CHPARM/ INNAM, INCLS, INEXT, ANNAME, OBSCOD, OBSDAT
      COMMON /OFLAGC/ SCRBUF, FGBUFF, OFBUFF, INSEQ, INDISK, INVERS,
     *   CNO, IUSER, IOFRNO, OFKOLS, OFNUMV, TABVER, NPOL, NIF, FGVERI,
     *   FGVERO, IDOFLG, NFLBIT, NFLLEV, NFLAG, NANT, NBDBIT
LOCAL END
      PROGRAM OFLAG
C-----------------------------------------------------------------------
C! Task to convert OF table flag info into FG table flags
C# Calibration EXT-appl EXT-util Editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2015-2016, 2018
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
C   OFLAG is an AIPS task to convert flagging info in an on-line flag
C   table (OF) to a standard aips flag table (FG)
C   AIPS adverbs:                     Use:
C     INNAME     INNAM(3)    Image name: standard defaults, wildcards
C     INCLASS    INCLS(2)    Image class: ditto
C     INSEQ      INSEQ       Image sequence number: ditto
C     INDISK     INDISK      Image disk number: 0 -> any
C     INEXT      INEXT       Extension type: '  ' => 'TA'
C     INVERS     INVERS      Extension version number
C     FLAGVER    FGVERI      Input FG table copied
C     OUTFGVER   FGVERO      Output FG table
C     DOFLAG     DOFLAG      Allowed status cutoff
C     FPARM      FPARM(30)   Flag if bit(i) set and FPARM(i) > 0
C-----------------------------------------------------------------------
      INTEGER   IRET
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'OFLAG.INC'
C-----------------------------------------------------------------------
C                                       Get parms, open things
      CALL OFLGIN (IRET)
C                                       do printing
      IF (IRET.EQ.0) CALL OFLGDO (IRET)
C                                       HI writing
      IF (IRET.EQ.0) CALL OFLGHI
C                                       close down
      CALL DIE (IRET, OFBUFF)
C
 999  STOP
      END
      SUBROUTINE OFLGIN (IRET)
C-----------------------------------------------------------------------
C   OFLGIN performs initialization for AIPS task OFLAG.  It gets the
C   adverbs, opens the catalog file for 'READ', opens the table
C   extension
C   file, and opens the output device.
C   Output: IRET    I      Error code: 0 => keep going, else quit.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER INTYP*2, STAT*4, PRGN*6
      INTEGER   NPARM, IROUND, OFLUN, IERR, IFG, FGLUN, I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'OFLAG.INC'
      DATA OFLUN, FGLUN, INTYP /27, 28, 'UV'/
      DATA PRGN /'OFLAG '/
C-----------------------------------------------------------------------
C                                       AIPS init
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       get adverbs
      NPARM = 52
      IRET = 0
      CALL GTPARM (PRGN, NPARM, RQUICK, XINNAM, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XINNAM, INNAM)
      CALL H2CHR (6, 1, XINCLS, INCLS)
      INEXT = 'OF'
C                                       find image file
      INSEQ = IROUND (XSEQ)
      INDISK = IROUND (XDISK)
      INVERS = IROUND (XVER)
      IUSER = NLUSER
      FGVERI = IROUND (XFLAG)
      FGVERO = IROUND (XFGOUT)
      IDOFLG = IROUND (DOFLAG)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       find data set
      CNO = 1
      CALL CATDIR ('SRCH', INDISK, CNO, INNAM, INCLS, INSEQ, INTYP,
     *   IUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, INNAM, INCLS, INSEQ, INTYP,
     *      INDISK, IUSER
         GO TO 990
         END IF
C                                       Get catblk, mark file read
      CALL CATIO ('READ', INDISK, CNO, CATBLK, 'REST', SCRBUF, IERR)
      IF ((IERR.GT.0) .AND. (IERR.LT.5)) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       flag versions
      CALL FNDEXT ('FG', CATBLK, IFG)
      IF (FGVERI.GT.IFG) FGVERI = -1
      IF (FGVERI.EQ.0) FGVERI = IFG
      IF ((FGVERO.LE.0) .OR. (FGVERO.GT.IFG)) FGVERO = IFG + 1
C                                       copy old FG table
      IF ((FGVERI.GT.0) .AND. (FGVERO.GT.IFG)) THEN
         CALL TABCOP ('FG', FGVERI, FGVERO, OFLUN, FGLUN, INDISK,
     *      INDISK, CNO, CNO, CATBLK, OFBUFF, FGBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1030) IRET, FGVERI, FGVERO
            GO TO 990
            END IF
         END IF
C                                       number antennas
      CALL GETNAN (INDISK, CNO, CATBLK, OFLUN, OFBUFF, NANT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1035) IRET
         GO TO 990
         END IF
C                                       Open table file
      CALL OFINI ('READ', OFBUFF, INDISK, CNO, INVERS, CATBLK, OFLUN,
     *   IOFRNO, OFKOLS, OFNUMV, NIF, NPOL, ANNAME, OBSCOD, OBSDAT,
     *   TABVER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET, INEXT, INVERS
      ELSE IF (TABVER.NE.2) THEN
         WRITE (MSGTXT,1045) TABVER
         IRET = 10
         END IF
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I5,' FINDING INPUT ADVERBS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   'DISK=',I2,' USER=',I5)
 1020 FORMAT ('ERROR',I5,' READING CATBLK FROM CATALOG FILE')
 1030 FORMAT ('ERROR',I5,' COPYING OLD FLAG TABLE')
 1035 FORMAT ('ERROR',I5,' CHECKING ANTENNA TABLES')
 1040 FORMAT ('ERROR',I5,' OPENING ',A2,' TABLE VERS=',I6)
 1045 FORMAT ('OF TABLE FORMAT VERSION',I4,' NOT RECOGNIZED')
      END
      SUBROUTINE OFLGDO (IRET)
C-----------------------------------------------------------------------
C   OFLGDO reads, formats, and prints an OF table extension file
C   Output:
C      IRET   I   Error code: 0 => OK or user terminates,
C                    2 => error writing, 3 => error reading
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'OFLAG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IRNO, SOURCE, IANT, SUBID, FREQID, ANFLAG, NS, ECOUNT,
     *   BCHAN, ECHAN, IFGRNO, STATUS(2,MAXIF), LP, LIF, BITS(32), I,
     *   FGKOLS(MAXFGC), FGNUMV(MAXFGC), JANT, FGLUN, KANT, IA1, IA2,
     *   NFG
      REAL      TIME, BTIME, ETIME
      CHARACTER VSYM(32)*8, REASON*24, STREAS*24, STOKES*4, STPAT(2,3)*4
      LOGICAL   DOFL(2,MAXIF), DOIT, ALL
      INCLUDE 'INCS:DMSG.INC'
      DATA VSYM /'RefPoint', 'Shadowed', 'OffSourc', '1 LOLock',
     *   'TsysFluc', 'Operator', 'BETotPow', 'BEFilter', 'L8Module',
     *   'L6Module', 'SubRefle', 'SouChang', 'PhSwitch', 'RndTrip',
     *   18*'????????'/
      DATA FGLUN /28/
      DATA STPAT /'1001','0110','1011','0111','1010','0101'/
C-----------------------------------------------------------------------
      NFLLEV = 0
      NFLAG = 0
      CALL FILL (32, 0, NFLBIT)
      CALL FILL (32, 0, NBDBIT)
      WRITE (STREAS,1000) IDOFLG
      ECOUNT = OFBUFF(5)
      BCHAN = 0
      ECHAN = 0
      JANT = 0
      IF (DETIME.LE.0.02) DETIME = 0.02
      DETIME = DETIME / (3600.0 * 24.0)
C                                       loop over file
      DO 100 IRNO = 1,ECOUNT
         IOFRNO = IRNO
         CALL TABOF ('READ', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME,
     *      SOURCE, IANT, SUBID, FREQID, ANFLAG, STATUS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, IRNO
            CALL MSGWRT (8)
            GO TO 970
         ELSE IF (IRET.EQ.0) THEN
            I = 2 * LIF
            CALL LFILL (I, .FALSE., DOFL)
            CALL ZGTBIT (32, ANFLAG, BITS)
            NS = 1
            REASON = ' '
            DO 10 I = 1,30
               IF ((FPARM(I).GT.0.0) .AND. (BITS(I).GT.0)) THEN
                  NFLBIT(I) = NFLBIT(I) + 1
                  IF (ANNAME.NE.'VLA') THEN
                     IF (NS.LE.22) THEN
                        WRITE (REASON(NS:),1001) I
                        NS = NS + 3
                        END IF
                  ELSE
                     IF (NS.LE.17) THEN
                        REASON(NS:) = VSYM(I)
                        NS = NS + 8
                        END IF
                     END IF
                  END IF
 10            CONTINUE
C                                       antenna fully flagged
            DOIT = .FALSE.
            IF (REASON.NE.' ') THEN
               I = 2 * LIF
               CALL FILL (I, .TRUE., DOFL)
               DOIT = .TRUE.
               ALL = .TRUE.
C                                       check status
            ELSE IF (IDOFLG.GT.0) THEN
               ALL = .TRUE.
               DO 20 LIF = 1,NIF
                  DO 15 LP = 1,NPOL
                     DOFL(LP,LIF) = STATUS(LP,LIF).GE.IDOFLG
                     IF (DOFL(LP,LIF)) THEN
                        NFLLEV = NFLLEV + 1
                        REASON = STREAS
                        DOIT = .TRUE.
                     ELSE
                        ALL = .FALSE.
                        END IF
 15                  CONTINUE
 20               CONTINUE
               END IF
C                                       write some flags
            IF (DOIT) THEN
C                                       count bad bits even if ignored
               IF (IDOFLG.GT.0) THEN
                  DO 25 I = 1,30
                     IF (BITS(I).GT.0) NBDBIT(I) = NBDBIT(I) + 1
 25                  CONTINUE
                  END IF
               BTIME = TIME - DETIME
               ETIME = TIME + DETIME
C                                       all
               IF (ALL) THEN
                  CALL FLAGUP ('FLAG', FGLUN, INDISK, CNO, FGVERO,
     *               FGBUFF, IFGRNO, FGKOLS, FGNUMV, SOURCE, 1, SUBID,
     *               FREQID, 1, IANT, JANT, BTIME, ETIME, 1, NIF, BCHAN,
     *               ECHAN, '1111', REASON, NFG, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1020) IRET, IFGRNO
                     CALL MSGWRT (8)
                     GO TO 970
                     END IF
                  NFLAG = NFLAG + NFG
C                                       by pieces
               ELSE
                  DO 40 LP = 1,NPOL
                     DO 35 LIF = 1,NIF
                        IF (DOFL(LP,LIF)) THEN
                           IF ((LP.EQ.1) .AND. (NPOL.EQ.2) .AND.
     *                        (DOFL(2,LIF))) THEN
                              CALL FLAGUP ('FLAG', FGLUN, INDISK,
     *                           CNO, FGVERO, FGBUFF, IFGRNO, FGKOLS,
     *                           FGNUMV, SOURCE, 1, SUBID, FREQID, 1,
     *                           IANT, JANT, BTIME, ETIME, LIF, LIF,
     *                           BCHAN, ECHAN, '1111', REASON, NFG,
     *                           IRET)
                              DOFL(2,LIF) = .FALSE.
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT,1020) IRET, IFGRNO
                                 CALL MSGWRT (8)
                                 GO TO 970
                                 END IF
                              NFLAG = NFLAG + NFG
                           ELSE
                              DO 30 KANT = 1,NANT(SUBID+1)
                                 IF (KANT.LT.IANT) THEN
                                    STOKES = STPAT(LP,1)
                                    IA1 = KANT
                                    IA2 = IANT
                                 ELSE IF (KANT.EQ.IANT) THEN
                                    STOKES = STPAT(LP,2)
                                    IA1 = KANT
                                    IA2 = KANT
                                 ELSE
                                    STOKES = STPAT(LP,3)
                                    IA1 = IANT
                                    IA2 = KANT
                                    END IF
                                 CALL FLAGUP ('FLAG', FGLUN, INDISK,
     *                              CNO, FGVERO, FGBUFF, IFGRNO, FGKOLS,
     *                              FGNUMV, SOURCE, 1, SUBID, FREQID, 1,
     *                              IA1, IA2, BTIME, ETIME, LIF, LIF,
     *                              BCHAN, ECHAN, STOKES, REASON, NFG,
     *                              IRET)
                                 IF (IRET.NE.0) THEN
                                    WRITE (MSGTXT,1020) IRET, IFGRNO
                                    CALL MSGWRT (8)
                                    GO TO 970
                                    END IF
                                 NFLAG = NFLAG + NFG
 30                              CONTINUE
                              END IF
                           END IF
 35                     CONTINUE
 40                  CONTINUE
                  END IF
               END IF
            END IF
 100     CONTINUE
C                                       close down
 970  CALL TABOF ('CLOS', OFBUFF, IOFRNO, OFKOLS, OFNUMV, TIME, SOURCE,
     *   IANT, SUBID, FREQID, ANFLAG, STATUS, I)
      CALL FLAGUP ('CLOS', FGLUN, INDISK, CNO, FGVERO, FGBUFF, IFGRNO,
     *   FGKOLS, FGNUMV, SOURCE, 1, SUBID, FREQID, 1, IANT, JANT, BTIME,
     *   ETIME, 1, NIF, BCHAN, ECHAN, '1111', REASON, NFG, I)
      IRET = MAX (0, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Status code >',I4)
 1001 FORMAT ('B',I2.2)
 1010 FORMAT ('ERROR',I5,' READING OF TABLE ROW',I10)
 1020 FORMAT ('ERROR',I5,' WRITING FG TABLE ROW',I10)
      END
      SUBROUTINE OFLGHI
C-----------------------------------------------------------------------
C   Add info in history file
C-----------------------------------------------------------------------
C
      INTEGER   LUNH, IRET, DATE(3), TIME(3), I
      CHARACTER HILINE*72, CTIME*8, CDATE*12, VSYM(32)*8
      INCLUDE 'OFLAG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNH /27/
      DATA VSYM /'RefPoint', 'Shadowed', 'OffSourc', '1 LOLock',
     *   'TsysFluc', 'Operator', 'BETotPow', 'BEFilter', 'L8Module',
     *   'L6Module', 'SubRefle', 'SouChang', 'PhSwitch', 'RndTrip',
     *   18*'????????'/
C-----------------------------------------------------------------------
C                                       Open history file
      CALL HIINIT (2)
      CALL HIOPEN (LUNH, INDISK, CNO, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Write time and date on old file
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       versions
      WRITE (HILINE,1010) TSKNAM, INVERS
      CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 100
      IF (FGVERI.GT.0) THEN
         WRITE (HILINE,1011) TSKNAM, FGVERI
         CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
         IF (IRET.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1012) TSKNAM, FGVERO
      CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       time expansion
      DETIME = DETIME * (3600.0 * 24.0)
      WRITE (HILINE,1015) TSKNAM, DETIME
      CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 100
C                                       total flags
      WRITE (HILINE,1020) TSKNAM, NFLAG
      CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
      IF (IRET.NE.0) GO TO 100
      MSGTXT = HILINE(9:)
      CALL MSGWRT (4)
C                                       status clip
      IF (IDOFLG.GT.0) THEN
         WRITE (HILINE,1025) TSKNAM, IDOFLG
         CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
         IF (IRET.NE.0) GO TO 100
         WRITE (HILINE,1026) TSKNAM, NFLLEV
         CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
         IF (IRET.NE.0) GO TO 100
         MSGTXT = HILINE(9:)
         CALL MSGWRT (4)
         END IF
      DO 20 I = 1,30
         IF ((FPARM(I).GT.0.0) .AND. (NFLBIT(I).GT.0)) THEN
            IF (ANNAME.EQ.'VLA') THEN
               WRITE (HILINE,1030) TSKNAM, I, VSYM(I)
            ELSE
               WRITE (HILINE,1031) TSKNAM, I
               END IF
            CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
            IF (IRET.NE.0) GO TO 100
            WRITE (HILINE,1031) TSKNAM, NFLBIT(I)
            CALL HIADD (LUNH, HILINE, SCRBUF, IRET)
            IF (IRET.NE.0) GO TO 100
            IF (ANNAME.EQ.'VLA') THEN
               WRITE (MSGTXT,1035) NFLBIT(I), I, VSYM(I)
            ELSE
               WRITE (MSGTXT,1036) NFLBIT(I), I
               END IF
            CALL MSGWRT (4)
            END IF
         IF ((NBDBIT(I).GT.0) .AND. (NBDBIT(I).NE.NFLBIT(I))) THEN
            IF (ANNAME.EQ.'VLA') THEN
               WRITE (MSGTXT,1037) NBDBIT(I), I, VSYM(I)
            ELSE
               WRITE (MSGTXT,1038) NBDBIT(I), I
               END IF
            CALL MSGWRT (4)
            END IF
 20      CONTINUE
C
 100  CALL HICLOS (LUNH, .TRUE., SCRBUF, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1010 FORMAT (A6,'INVERS  =',I5,5X,'/ OF table version input')
 1011 FORMAT (A6,'FLAGVER =',I5,5X,'/ FG version copied to output FG')
 1012 FORMAT (A6,'OUTFGVER=',I5,5X,'/ FG version written')
 1015 FORMAT (A6,'DETIME=',F7.2,5X,'/ sec expand time of OF record')
 1020 FORMAT (A6,'/ Wrote  ',I10,' flag commands to FG table')
 1025 FORMAT (A6,'DOFLAG  =',I5,5X,'/ flag status >= DOFLAG')
 1026 FORMAT (A6,'/ Flagged',I10,' correlators solely because of',
     *   ' status')
 1030 FORMAT (A6,'FPARM(',I2,') = 1',3X,'/ Flag if bit for ',A,' set')
 1031 FORMAT (A6,'FPARM(',I2,') = 1',3X,'/ Flag if bit set')
 1035 FORMAT ('Flagged',I10,' samples for bit',I3,' =''',A,'''')
 1036 FORMAT ('Flagged',I10,' samples for bit',I3)
 1037 FORMAT ('Found  ',I10,' samples for bit',I3,' =''',A,'''')
 1038 FORMAT ('Found  ',I10,' samples for bit',I3)
      END
