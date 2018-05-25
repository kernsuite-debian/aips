LOCAL INCLUDE 'REFLG.INC'
C                                       Local include for REFLG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXTIM, MAXSOU
      PARAMETER (MAXTIM=100000)
      PARAMETER (MAXSOU=300)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALC
      REAL      XSIN, XDISIN, XFLAG, CPARM(10)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XCALC,
     *   XFLAG, CPARM
C
      INTEGER   SEQIN, DISKIN, JBUFSZ, OLDCNO, NUMAN(513), NANT, NSUB,
     *   NTIMES, NCHAN, NIF, IFGVER, OFGVER, NUMSU, NUMFQ, FGBUF1(512),
     *   FGBUF2(512), FGKOLS(MAXFGC), FGNUMV(MAXFGC), NFGSCR, NFGOUT,
     *   NOUTR, SUNUMS(MAXSOU), FCHAN, FTIME, FBL, FANTS, FEXT, OFGRNO,
     *   BLEXIS(MXBASE)
      CHARACTER NAMEIN*12, CLAIN*6, SNMS(MAXSOU)*16, XSOUR(30)*16,
     *   CLCODE*4
      REAL      BUFF1(UVBFSS), TIMES(2,MAXTIM)
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /REFLGC/ FGBUF1, FGBUF2, TIMES, NTIMES, SEQIN, DISKIN,
     *   OLDCNO, NUMAN, NANT, NSUB, NCHAN, NIF, IFGVER, OFGVER, NUMSU,
     *   NUMFQ, FGKOLS, FGNUMV, SUNUMS, NFGSCR, NFGOUT, NOUTR, FCHAN,
     *   FTIME, FBL, FANTS, FEXT, OFGRNO, BLEXIS
      COMMON /CHRCOM/ NAMEIN, CLAIN, SNMS, XSOUR, CLCODE
LOCAL END
      PROGRAM REFLG
C-----------------------------------------------------------------------
C! Compresses an FG table
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2015-2016
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
C   Compress an FG table
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      CPARM......1=max. gap
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, PHNAME*48
      INTEGER   ISUB, IRET, IFLAG(2), NWORDS, NBL, ISU, IFQ, IIF, IERR,
     *   JSU
      LONGINT   PIFLAG
      INCLUDE 'REFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'REFLG '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL REFLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      NBL = (NANT * (NANT+1)) / 2
C                                       loop over subarrays, sources,
C                                       FQs
      DO 100 IFQ = 1,NUMFQ
         DO 90 ISUB = 1,NSUB
            DO 80 JSU = 1,NUMSU
               ISU = SUNUMS(JSU)
C                                       Get list of times
               CALL REFLTI (JSU, ISUB, IFQ, IRET)
               IF (IRET.NE.0) GO TO 980
               IF (NTIMES.GT.0) THEN
                  NWORDS = (NCHAN * NBL * NTIMES - 1) / 1024 + 2
                  CALL ZMEMRY ('GET ', PRGM, NWORDS, IFLAG, PIFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO GET REQUIRED MEMORY'
                     CALL MSGWRT (8)
                     GO TO 980
                     END IF
C                                       redo flags
                  DO 70 IIF = 1,NIF
                     CALL REFLFG (IIF, ISU, ISUB, IFQ, NCHAN, NBL,
     *                  IFLAG(1+PIFLAG), IRET)
                     IF (IRET.NE.0) GO TO 980
 70                  CONTINUE
                  CALL ZMEMRY ('FREE', PRGM, NWORDS, IFLAG, PIFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO FREE DYNAMIC MEMORY'
                     CALL MSGWRT (8)
                     GO TO 980
                     END IF
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       summary
      WRITE (MSGTXT,1100) NFGSCR, NOUTR
      CALL MSGWRT (5)
      NFGSCR = NFGSCR + NFGOUT
      NFGOUT = NFGOUT + NOUTR
C                                       try for more global
      CALL REFLGL (IRET)
C                                       HI file
      CALL REFLHI
      GO TO 985
C                                       delete on failure
 980  CALL ZPHFIL ('FG', DISKIN, OLDCNO, OFGVER, PHNAME, IERR)
      CALL ZDESTR (DISKIN, PHNAME, IERR)
      CALL DELEXT ('FG', DISKIN, OLDCNO, 'RDRD', BUFF1, FGBUF1, OFGVER,
     *   IERR)
C                                       delete scratch
 985  IIF = OFGVER + 1
      CALL ZPHFIL ('FG', DISKIN, OLDCNO, IIF, PHNAME, IERR)
      CALL ZDESTR (DISKIN, PHNAME, IERR)
      CALL DELEXT ('FG', DISKIN, OLDCNO, 'RDRD', BUFF1, FGBUF1, IIF,
     *   IERR)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1100 FORMAT ('T-F in baseline process of',I10,' FG records into',I10)
      END
      SUBROUTINE REFLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   REFLIN gets input parameters for REFLG and finds input file.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok
C                               3 => Wrong sort order
C                               4 => No source table
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      INCLUDE 'REFLG.INC'
      CHARACTER STAT*4, UTYPE*2, VELTYP*8, VELDEF*8, SOUNAM*16,
     *   CALCOD*4, BNDCOD(MAXIF)*8
      INTEGER   NPARM, IROUND, IERR, ALUN, ISUB, I, BUFFER(512), NUMIF,
     *   RNOFQ, KOLS(MAXFQC), NUMV(MAXFQC), NREC, FQID, ISURNO, QUAL,
     *   SIDFQ(MAXIF), SUKOLS(MAXSUC), SUNUMV(MAXSUC), IDSOU, LUN, VER,
     *   LUNTMP, J, NS
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC, RAOBS,
     *   DECOBS
      REAL      TBWFQ(MAXIF), CHBWFQ(MAXIF), FLUX(4,MAXIF)
      LOGICAL   T, DESEL, FAIL
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ALUN /29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NFGSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 138
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check sort order
      IF (ISORT(:1).NE.'T') THEN
         JERR = 3
         WRITE (MSGTXT,1050) ISORT
         GO TO 990
         END IF
C                                       flag ver
      CALL FNDEXT ('FG', CATBLK, I)
      IFGVER = IROUND (XFLAG)
      IF ((IFGVER.LE.0) .OR. (IFGVER.GT.I)) IFGVER = I
      OFGVER = I + 1
      IF (I.LE.0) THEN
         MSGTXT = 'NO FG TABLE TO COMPRESS'
         JERR = 3
         GO TO 990
         END IF
      NCHAN = CATBLK(KINAX+JLOCF)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
C                                       Get number of antennas
      CALL GETNAN (DISKIN, OLDCNO, CATBLK, ALUN, BUFF1, NUMAN, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1070) JERR
         CALL MSGWRT (7)
      ELSE
         NSUB = NUMAN(1)
         NANT = 0
         DO 100 ISUB = 1,NSUB
            NANT = MAX (NANT, NUMAN(ISUB+1))
 100        CONTINUE
         END IF
C                                       source list
      NS = 0
      DESEL = .FALSE.
      DO 110 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOUNAM)
         IF (SOUNAM.NE.' ') THEN
            NS = NS + 1
            IF (SOUNAM(1:1).EQ.'-') THEN
               DESEL = .TRUE.
               XSOUR(NS) = SOUNAM(2:)
            ELSE
               XSOUR(NS) = SOUNAM
               END IF
            END IF
 110     CONTINUE
      CALL H2CHR (4, 1, XCALC, CLCODE)
C                                       get max source number
      LUN = LUNTMP (1)
      CALL FNDEXT ('SU', CATBLK, I)
      IF (I.LE.0) THEN
         NUMSU = 1
         SUNUMS(1) = 1
         SNMS(1) = ' '
      ELSE
         NUMSU = 0
         VER = 1
         CALL SOUINI ('READ', BUFFER, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *      NUMIF, VELTYP, VELDEF, FQID, ISURNO, SUKOLS, SUNUMV, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR, 'OPENING SU TABLE'
            GO TO 990
            END IF
         NREC = BUFFER(5)
         DO 130 I = 1,NREC
            CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *         PMRA, PMDEC, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1100) JERR, 'READING SU TABLE'
               GO TO 990
               END IF
            FAIL = .FALSE.
            IF ((NS.GT.0) .OR. (CLCODE.NE.' ')) THEN
               IF (CLCODE.EQ.'*') THEN
                  FAIL = CALCOD.EQ.' '
               ELSE IF (CLCODE.EQ.'-CAL') THEN
                  FAIL = CALCOD.NE.' '
               ELSE IF (CLCODE.NE.' ') THEN
                  FAIL = CLCODE.NE.CALCOD
                  END IF
               IF ((.NOT.FAIL) .AND. (NS.GT.0)) THEN
                  DO 120 J = 1,NS
                     IF (XSOUR(J).EQ.SOUNAM) THEN
                        FAIL = DESEL
                        GO TO 125
                        END IF
 120                 CONTINUE
                  FAIL = .NOT.DESEL
                  END IF
               END IF
 125        IF (.NOT.FAIL) THEN
               NUMSU = NUMSU + 1
               SUNUMS(NUMSU) = IDSOU
               SNMS(NUMSU) = SOUNAM
               END IF
 130        CONTINUE
         CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR, 'CLOSING SU TABLE'
            GO TO 990
            END IF
         END IF
C                                       getn max FQ number
      CALL FNDEXT ('FQ', CATBLK, I)
      IF (I.LE.0) THEN
         NUMFQ = 1
      ELSE
         NUMFQ = 0
         VER = 1
         CALL FQINI ('READ', BUFFER, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *      RNOFQ, KOLS, NUMV, NUMIF, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR, 'OPENING FQ TABLE'
            GO TO 990
            END IF
         NREC = BUFFER(5)
         RNOFQ = 1
         DO 140 I = 1,NREC
            CALL TABFQ ('READ', BUFFER, RNOFQ, KOLS, NUMV, NUMIF,
     *         FQID, FREQO, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, JERR)
            IF (JERR.NE.0) THEN
               WRITE (MSGTXT,1100) JERR, 'READING FQ TABLE'
               GO TO 990
               END IF
            NUMFQ = MAX (NUMFQ, FQID)
 140        CONTINUE
         CALL TABFQ ('CLOS', BUFFER, RNOFQ, KOLS, NUMV, NUMIF,
     *      FQID, FREQO, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR, 'CLOSING FQ TABLE'
            GO TO 990
            END IF
         END IF
C                                       prepare FG files for work
      CALL FGPREP (BUFFER, JERR)
C                                       counters
      FCHAN = 0
      FTIME = 0
      FBL = 0
      FANTS = 0
      FEXT = 0
      CALL FILL (MXBASE, 0, BLEXIS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REFLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('WRONG SORT ORDER(',A2,'), USE UVSRT TO SORT TO ''TB''')
 1070 FORMAT ('REFLIN: ERROR ',I3,' DETERMINING NUMBER OF ANTENNAS')
 1100 FORMAT ('REFLIN ERROR:',I5,' ON ',A)
      END
      SUBROUTINE FGPREP (BUFFER, IRET)
C-----------------------------------------------------------------------
C   FGPREP separates the flags into two FG files - the channel and IF
C   dependent ones (OFGVER+1) and the rest (OFGVER)
C   Outputs:
C      BUFFER   I(512)   Input FG work buffer
C      IRET     I        Error code
C-----------------------------------------------------------------------
      INTEGER   BUFFER(512), IRET
C
      INCLUDE 'REFLG.INC'
      INTEGER   LUN1, LUN2, LUN3, LUNTMP, VER, NREC, IREC, IFGRNO, I,
     *   FGRNO1, FGRNO2, SOURID, SUBA, FREQID, ANTS(2), IFS(2), NST,
     *   CHANS(2), ISU
      REAL      TIMER(2)
      LOGICAL   PFLAGS(4), COPY
      CHARACTER REASON*24
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      NST = CATBLK(KINAX+JLOCS)
C                                       open input FG
      LUN1 = LUNTMP (1)
      CALL FLGINI ('READ', BUFFER, DISKIN, OLDCNO, IFGVER, CATBLK, LUN1,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FG TABLE'
         GO TO 990
         END IF
C                                       open output FG
      LUN2 = LUNTMP (1)
      CALL FLGINI ('WRIT', FGBUF1, DISKIN, OLDCNO, OFGVER, CATBLK, LUN2,
     *   FGRNO1, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FG TABLE'
         GO TO 990
         END IF
C                                       open scratch FG
      LUN3 = LUNTMP (1)
      VER = OFGVER + 1
      CALL FLGINI ('WRIT', FGBUF2, DISKIN, OLDCNO, VER, CATBLK, LUN3,
     *   FGRNO2, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SCRATCH FG TABLE'
         GO TO 990
         END IF
      NREC = BUFFER(5)
      NFGSCR = 0
      NFGOUT = 0
C                                       read loop
      DO 100 IREC = 1,NREC
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT FG TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
            COPY = ((ANTS(1).GT.0) .AND. (ANTS(2).GT.0) .AND.
     *         (IFS(1).GT.0) .AND. (IFS(2).GT.0) .AND.
     *         (CHANS(1).GT.0) .AND. (CHANS(2).GT.0) .AND.
     *         ((TIMER(1).GT.0.0) .OR. (TIMER(2).LT.1000.)))
C                                       flagging all?
            DO 10 I = 1,NST
               COPY = (COPY) .AND. (PFLAGS(I))
 10            CONTINUE
C                                       check source
            IF ((NUMSU.GT.0) .AND. (SOURID.GT.0) .AND. (COPY)) THEN
               DO 20 ISU = 1,NUMSU
                  IF (SOURID.EQ.SUNUMS(ISU)) GO TO 30
 20               CONTINUE
               COPY = .FALSE.
               END IF
C                                       for scratch
 30         IF (COPY) THEN
               CALL TABFLG ('WRIT', FGBUF2, FGRNO2, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING SCRATCH FG TABLE'
                  GO TO 990
                  END IF
               NFGSCR = NFGSCR + 1
            ELSE
               CALL TABFLG ('WRIT', FGBUF1, FGRNO1, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *            REASON, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING SCRATCH FG TABLE'
                  GO TO 990
                  END IF
               NFGOUT = NFGOUT + 1
               END IF
            END IF
 100     CONTINUE
      CALL TABFLG ('CLOS', BUFFER, IFGRNO, FGKOLS, FGNUMV, SOURID, SUBA,
     *   FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING INPUT FG TABLE'
         GO TO 990
         END IF
      CALL TABFLG ('CLOS', FGBUF2, FGRNO2, FGKOLS, FGNUMV, SOURID, SUBA,
     *   FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING SCRATCH FG TABLE'
         GO TO 990
         END IF
      CALL TABFLG ('CLOS', FGBUF1, FGRNO1, FGKOLS, FGNUMV, SOURID, SUBA,
     *    FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'WRITING SCRATCH FG TABLE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1100) NFGOUT, 'output flag table'
      CALL MSGWRT (4)
      WRITE (MSGTXT,1100) NFGSCR, 'temp FG table for processing'
      CALL MSGWRT (4)
      IF (NFGSCR.EQ.0) THEN
         MSGTXT = 'No flags in scratch: skip reading sources'
         CALL MSGWRT (4)
         NUMSU = 0
         END IF
      NOUTR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGPREP ERROR:',I4,' ON ',A)
 1100 FORMAT ('FGPREP: wrote',I10,' records to ',A)
      END
      SUBROUTINE REFLTI (ISU, ISUBA, IFQ, IRET)
C-----------------------------------------------------------------------
C   REFLTI finds the list of times
C   Input:
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   ISU, ISUBA, IFQ, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, IA1, IA2, KBASE, NBL, ISDAT(MXBASE), JBL, CATSAV(256)
      LOGICAL   GETNEW
      REAL      CURTIM, TLIMIT, TINT, TB, RPARM(20)
      INCLUDE 'REFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
C-----------------------------------------------------------------------
C                                       init DSEL parms
      CALL SELINI
      FGVER = -1
      SUBARR = ISUBA
      FRQSEL = IFQ
      SOURCS(1) = SNMS(ISU)
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL COPY (256, CATBLK, CATSAV)
C                                       rflag parameters
      IF (CPARM(1).LE.0.0) CPARM(1) = 10.
      TLIMIT = 2.01 * CPARM(1)
      TLIMIT = TLIMIT / (24. * 3600.)
      TINT = CPARM(1) / (24. * 3600.)
      TB = -1000.
      NBL = (NANT * (NANT+1)) / 2
      CALL FILL (NBL, 0, ISDAT)
      NTIMES = 0
C                                       init I/O
      CALL UVGET ('INIT', RPARM, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INIT READ ON ' // SOURCS(1)
            GO TO 990
            END IF
         IRET = 0
         GO TO 980
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, BUFF1, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ VIS FOR ' // SOURCS(1)
         GO TO 990
      ELSE IF (IRET.EQ.0) THEN
         CURTIM = RPARM(1+ILOCT)
         IF (ILOCB.GE.0) THEN
            KBASE = RPARM(1+ILOCB) + 0.1
            IA1 = KBASE / 256
            IA2 = KBASE - IA1 * 256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         JBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
         BLEXIS(JBL) = 1
C                                       usable in this interval?
         IF (ABS(CURTIM-TB).LT.TINT) THEN
            GETNEW = ISDAT(JBL).GT.0
            ISDAT(JBL) = 1
C                                       definitely need new
         ELSE
            GETNEW = .TRUE.
            END IF
         IF (GETNEW) THEN
            NTIMES = NTIMES + 1
            TIMES(1,NTIMES) = CURTIM
            TIMES(2,NTIMES) = CURTIM
            TB = TIMES(1,NTIMES)
            CALL FILL (NBL, 0, ISDAT)
         ELSE
            TIMES(1,NTIMES) = MIN (CURTIM, TIMES(1,NTIMES))
            TIMES(2,NTIMES) = MAX (CURTIM, TIMES(2,NTIMES))
            TB = TIMES(1,NTIMES)
            END IF
         GO TO 100
      ELSE
         IRET = 0
         END IF
C                                       Close files
 980  CALL UVGET ('CLOS', RPARM, BUFF1, I)
      CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REFLTI: ERROR',I3,' ON ',A)
      END
      SUBROUTINE REFLFG (IIF, ISU, ISUB, IFQ, NC, NBL, IFLAG, IRET)
C-----------------------------------------------------------------------
C   Does the heavy lifting for REFLG
C   Inputs:
C      IIF      I      IF number this pass
C      ISU      I      Source number
C      ISUB     I      Subarray number
C      IFQ      I      Frequency ID
C      NC       I      Number channels
C      NBL      I      Number baselines
C   Output:
C      IFLAG    I(*)   Work memory (NC,NBL,number of times)
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IIF, ISU, ISUB, IFQ, NC, NBL, IFLAG(NC,NBL,*), IRET
C
      INCLUDE 'REFLG.INC'
      INTEGER   LUN, LUNTMP, VER, NREC, IREC, IFGRNO, SOURID, SUBA,
     *   FREQID, ANTS(2), IFS(2), CHANS(2), NFGIN, NFGOU, IFLAGS, ITB,
     *   ITE, IT0, IA1, IA2, ZOR, ZAND, IC, IT, IBL, MM, J, I, JC, JT,
     *   DATE(3), TIME(3), NN, NODD, FFCHAN, FFTIME, FFBL, IROUND,
     *   MASK, FFANTS, NEXT, NX, JXX, FFEXT
      REAL      TIMER(2), FRACT, TEPS
      LOGICAL   PFLAGS(4), INONE
      CHARACTER REASON*24, ATIME*8, ADATE*12
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      NFGIN = 0
      NFGOU = 0
      IC = NC * NBL * NTIMES
      CALL FILL (IC, 0, IFLAG)
      TEPS = 0.1 / (24.0 * 3600.0)
C                                       open scratch FG
      VER = OFGVER + 1
      LUN = LUNTMP (1)
      CALL FLGINI ('READ', FGBUF1, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SCRATCH FG TABLE'
         GO TO 990
         END IF
      NREC = FGBUF1(5)
      IT0 = 1
      DO 50 IREC = 1,NREC
         CALL TABFLG ('READ', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING SCRATCH FG TABLE'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       applies to this data subset
            IF ((IFS(1).LE.IIF) .AND. (IFS(2).GE.IIF) .AND.
     *         ((SUBA.LE.0) .OR. (SUBA.EQ.ISUB)) .AND.
     *         ((FREQID.LE.0) .OR. (FREQID.EQ.IFQ)) .AND.
     *         ((SOURID.LE.0) .OR. (SOURID.EQ.ISU))) THEN
               NFGIN = NFGIN + 1
               IFLAGS = 0
               IF (PFLAGS(1)) IFLAGS = IFLAGS + 1
               IF (PFLAGS(2)) IFLAGS = IFLAGS + 2
               IF (PFLAGS(3)) IFLAGS = IFLAGS + 4
               IF (PFLAGS(4)) IFLAGS = IFLAGS + 8
C                                       find start time
               IT0 = 1
               DO 10 IT = IT0,NTIMES
                  IF (TIMER(1).GT.TIMES(2,IT)) THEN
                     IT0 = IT0 + 1
                  ELSE
                     ITB = IT0
                     GO TO 15
                     END IF
 10               CONTINUE
C                                       done
               GO TO 60
C                                       find end time
 15            DO 20 IT = ITB,NTIMES
                  IF (TIMER(2).LT.TIMES(1,IT)) THEN
                     ITE = IT-1
                     GO TO 25
                     END IF
 20               CONTINUE
               ITE = NTIMES
C                                       mark array
 25            IA1 = MIN (ANTS(1), ANTS(2))
               IA2 = MAX (ANTS(1), ANTS(2))
               IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
               DO 40 IT = ITB,ITE
                  DO 30 IC = CHANS(1),CHANS(2)
                     IFLAG(IC,IBL,IT) = ZOR (IFLAG(IC,IBL,IT), IFLAGS)
 30                  CONTINUE
 40               CONTINUE
               END IF
            END IF
 50      CONTINUE
 60   CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *   SUBA,FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING SCRATCH FG TABLE'
         GO TO 990
         END IF
      WRITE (MSGTXT,1050) NFGIN, IIF, ISU, ISUB, IFQ
      CALL MSGWRT (2)
      MM = 0
      DO 75 IT = 1,NTIMES
         DO 70 IBL = 1,NBL
            DO 65 IC = 1,NC
               IF (IFLAG(IC,IBL,IT).GT.0) MM = MM + 1
 65            CONTINUE
 70         CONTINUE
 75      CONTINUE
      WRITE (MSGTXT,1075) MM
      CALL MSGWRT (2)
C                                       set reason for this call
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ATIME(:5)
C                                       channels extend
      NEXT = CPARM(2) + 0.5
      IF (NEXT.GT.0) THEN
         FFEXT = 0
         DO 150 IT = 1,NTIMES
            DO 140 IBL = 1,NBL
C                                       extend at beginning?
               IF (IFLAG(1,IBL,IT).EQ.0) THEN
                  DO 110 NX = 1,NEXT
                     IF (IFLAG(1+NX,IBL,IT).GT.0) THEN
                        CALL FILL (NX, 15, IFLAG(1,IBL,IT))
                        FFEXT = FFEXT + NX
                        GO TO 115
                        END IF
 110                 CONTINUE
                  END IF
C                                       look through channels
 115           INONE = .FALSE.
               DO 130 IC = 1,NC
C                                       extend
                  IF ((INONE) .AND. (IFLAG(IC,IBL,IT).EQ.0)) THEN
                     JXX = MIN (NC-IC, NEXT)
                     DO 120 NX = 1,JXX
                        IF (IFLAG(IC+NX,IBL,IT).GT.0) THEN
                           CALL FILL (NX, 15, IFLAG(IC,IBL,IT))
                           FFEXT = FFEXT + NX
                           GO TO 125
                           END IF
 120                    CONTINUE
                     IF (JXX.LT.NEXT) THEN
                        CALL FILL (JXX+1, 15, IFLAG(IC,IBL,IT))
                        FFEXT = FFEXT + JXX + 1
                        END IF
                     END IF
C                                       mark if in one
 125              IF (IFLAG(IC,IBL,IT).GT.0) THEN
                     INONE = .TRUE.
                  ELSE IF (IFLAG(IC,IBL,IT).EQ.0) THEN
                     INONE = .FALSE.
                     END IF
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
         IF (FFEXT.GT.0) THEN
            WRITE (MSGTXT,1150) FFEXT
            CALL MSGWRT (4)
            FEXT = FEXT + FFEXT
            END IF
         END IF
C                                       channels excess
      FFCHAN = 0
      IF ((CPARM(3).GT.0.0) .AND. (CPARM(3).LT.1.0)) THEN
         DO 240 IT = 1,NTIMES
            DO 230 IBL = 1,NBL
               FRACT = 0.0
               DO 210 IC = 1,NC
                  IF (IFLAG(IC,IBL,IT).NE.0) FRACT = FRACT + 1.0
 210              CONTINUE
               FRACT = FRACT / NC
               IF ((FRACT.GT.CPARM(3)) .AND. (FRACT.LT.1.0)) THEN
                  FFCHAN = FFCHAN + 1
                  DO 220 IC = 1,NC
                     IF (IFLAG(IC,IBL,IT).EQ.0) IFLAG(IC,IBL,IT) = 15
 220                 CONTINUE
                  END IF
 230           CONTINUE
 240        CONTINUE
         END IF
      IF (FFCHAN.GT.0) THEN
         WRITE (MSGTXT,1240) FFCHAN
         CALL MSGWRT (4)
         FCHAN = FCHAN + FFCHAN
         END IF
C                                       times excess
      FFTIME = 0
      IF ((CPARM(4).GT.0.0) .AND. (CPARM(4).LT.1.0)) THEN
         DO 290 IC = 1,NC
            DO 280 IBL = 1,NBL
               FRACT = 0.0
               DO 260 IT = 1,NTIMES
                  IF (IFLAG(IC,IBL,IT).NE.0) FRACT = FRACT + 1.0
 260              CONTINUE
               FRACT = FRACT / NTIMES
               IF ((FRACT.GT.CPARM(4)) .AND. (FRACT.LT.1.0)) THEN
                  FFTIME = FFTIME + 1
                  DO 270 IT = 1,NTIMES
                     IF (IFLAG(IC,IBL,IT).EQ.0) IFLAG(IC,IBL,IT) = 15
 270                 CONTINUE
                  END IF
 280           CONTINUE
 290        CONTINUE
         END IF
      IF (FFTIME.GT.0) THEN
         WRITE (MSGTXT,1290) FFTIME
         CALL MSGWRT (4)
         FTIME = FTIME + FFTIME
         END IF
C                                       baselines excess
      FFBL = 0
      IF ((CPARM(5).GT.0.0) .AND. (CPARM(5).LT.1.0)) THEN
         DO 340 IT = 1,NTIMES
            DO 330 IC = 1,NC
               FRACT = 0.0
               DO 310 IBL = 1,NBL
                  IF (IFLAG(IC,IBL,IT).NE.0) FRACT = FRACT + 1.0
 310              CONTINUE
               FRACT = FRACT / NBL
               IF ((FRACT.GT.CPARM(5)) .AND. (FRACT.LT.1.0)) THEN
                  FFBL = FFBL + 1
                  DO 320 IBL = 1,NBL
                     IF (IFLAG(IC,IBL,IT).EQ.0) IFLAG(IC,IBL,IT) = 15
 320                 CONTINUE
                  END IF
 330           CONTINUE
 340        CONTINUE
         END IF
      IF (FFBL.GT.0) THEN
         WRITE (MSGTXT,1340) FFBL
         CALL MSGWRT (4)
         FBL = FBL + FFBL
         END IF
C                                       antennas excess
      FFANTS = 0
      IF ((CPARM(6).GT.0.0) .AND. (CPARM(6).LT.1.0)) THEN
         DO 450 IT = 1,NTIMES
            DO 440 IC = 1,NC
               DO 430 IA1 = 1,NANT
                  FRACT = 0.0
                  DO 410 IA2 = 1,NANT
                     IF (IA2.LT.IA1) THEN
                        IBL = NANT * (IA2-1) - ((IA2*(IA2-1))/2) + IA1
                     ELSE
                        IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
                        END IF
                     IF (IFLAG(IC,IBL,IT).NE.0) FRACT = FRACT + 1.0
 410                 CONTINUE
                  FRACT = FRACT / NANT
                  IF ((FRACT.GT.CPARM(6)) .AND. (FRACT.LT.1.0)) THEN
                     FFANTS = FFANTS + 1
                     DO 420 IA2 = 1,NANT
                        IF (IA2.LT.IA1) THEN
                           IBL = NANT * (IA2-1) - ((IA2*(IA2-1))/2) +
     *                        IA1
                        ELSE
                           IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) +
     *                        IA2
                           END IF
                        IF (IFLAG(IC,IBL,IT).EQ.0) IFLAG(IC,IBL,IT) = 15
 420                    CONTINUE
                     END IF
 430              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
      IF (FFANTS.GT.0) THEN
         WRITE (MSGTXT,1450) FFANTS
         CALL MSGWRT (4)
         FANTS = FANTS + FFANTS
         END IF
C                                       flag more polarizations if some
      MASK = IROUND (CPARM(7))
      IF (MASK.EQ.1) THEN
         MASK = 12
      ELSE IF (MASK.GE.2) THEN
         MASK = 15
      ELSE
         MASK = 0
         END IF
      IF (MASK.GT.0) THEN
         DO 530 IT = 1,NTIMES
            DO 520 IBL = 1,NBL
               DO 510 IC = 1,NC
                  IF (IFLAG(IC,IBL,IT).GT.0) IFLAG(IC,IBL,IT) =
     *               ZOR (IFLAG(IC,IBL,IT), MASK)
 510              CONTINUE
 520           CONTINUE
 530        CONTINUE
         END IF
C
C                                       reserved for analysis and
C                                       spreading algorithms
C
C                                       open output FG
      VER = OFGVER
      LUN = LUNTMP (1)
      CALL FLGINI ('WRIT', FGBUF1, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT FG TABLE'
         GO TO 990
         END IF
      NODD = 0
      DO 800 IT = 1,NTIMES
         IA1 = 1
         IA2 = 0
         DO 790 IBL = 1,NBL
            IA2 = IA2 + 1
            IF (IA2.GT.NANT) THEN
               IA1 = IA1 + 1
               IA2 = IA1
               END IF
            IC = 0
 700        IC = IC + 1
            IF (IC.LE.NC) THEN
               IF (IFLAG(IC,IBL,IT).LE.0) GO TO 700
               JT = IT
 710           JT = JT + 1
               IF (JT.LE.NTIMES) THEN
                  IF (IFLAG(IC,IBL,JT).NE.0) GO TO 710
                  END IF
               JT = JT - 1
               JC = IC
 720           JC = JC + 1
               IF (JC.LE.NC) THEN
                  DO 725 I = IT,JT
                     IF (IFLAG(JC,IBL,I).EQ.0) GO TO 730
 725                 CONTINUE
                  GO TO 720
                  END IF
 730           JC = JC - 1
C                                       flag IC-JC, IT-JT
               IFLAGS = 0
               DO 740 I = IT,JT
                  DO 735 J = IC,JC
                     MM = IFLAG(J,IBL,I)
                     IF (MM.GT.0) IFLAGS = ZOR (IFLAGS, MM)
                     IF (MM.EQ.0) THEN
                        NODD = NODD + 1
                        MM = 100
                        END IF
                     IFLAG(J,IBL,I) = -ABS (MM)
 735                 CONTINUE
 740              CONTINUE
               IF (IFLAGS.GT.0) THEN
                  PFLAGS(1) = ZAND (IFLAGS,1).EQ.1
                  PFLAGS(2) = ZAND (IFLAGS,2).EQ.2
                  PFLAGS(3) = ZAND (IFLAGS,4).EQ.4
                  PFLAGS(4) = ZAND (IFLAGS,8).EQ.8
                  IFS(1) = IIF
                  IFS(2) = IIF
                  CHANS(1) = IC
                  CHANS(2) = JC
                  TIMER(1) = TIMES(1,IT) - TEPS
                  TIMER(2) = TIMES(2,JT) + TEPS
                  ANTS(1) = IA1
                  ANTS(2) = IA2
                  CALL TABFLG ('WRIT', FGBUF1, IFGRNO, FGKOLS, FGNUMV,
     *               ISU, ISUB, IFQ, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *               REASON, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FG TABLE'
                     GO TO 990
                     END IF
                  NFGOU = NFGOU + 1
                  END IF
               IC = JC
               GO TO 700
               END IF
 790        CONTINUE
 800     CONTINUE
C                                       what we did
      WRITE (MSGTXT,1800) NFGOU, IIF, ISU, ISUB, IFQ
      CALL MSGWRT (2)
      NOUTR = NOUTR + NFGOU
C                                       close
      CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, ISU, ISUB,
     *   IFQ, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING OUTPUT FG TABLE'
         GO TO 990
         END IF
C                                       double check
      MM = 0
      NN = 0
      DO 820 IT = 1,NTIMES
         DO 815 IBL = 1,NBL
            DO 810 IC = 1,NC
               IF (IFLAG(IC,IBL,IT).GT.0) MM = MM + 1
               IF (IFLAG(IC,IBL,IT).LT.0) NN = NN + 1
 810           CONTINUE
 815        CONTINUE
 820     CONTINUE
      WRITE (MSGTXT,1820) MM, NN
      IF (MM.GT.0) CALL MSGWRT (2)
      WRITE (MSGTXT,1825) NODD, NN
      IF (NODD.GT.0) CALL MSGWRT (2)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REFLFG ERROR:',I4,' ON ',A)
 1050 FORMAT ('REFLFG: read ',I9,' flag records IF/Source/Sub/FQ',I3,I4,
     *   I3,I2)
 1075 FORMAT ('REFLFG:',I15,' cells marked')
 1150 FORMAT ('Flagged channels between flags  ',I12,' channels')
 1240 FORMAT ('Flagged for excess over channel ',I12,' times')
 1290 FORMAT ('Flagged for excess over time    ',I12,' times')
 1340 FORMAT ('Flagged for excess over baseline',I12,' times')
 1450 FORMAT ('Flagged for excess over antenna',I12,' times')
 1800 FORMAT ('REFLFG: wrote',I9,' flag records IF/Source/Sub/FQ',I3,I4,
     *   I3,I2)
 1820 FORMAT ('REFLFG:',I15,' cells still marked',I12,' flagged')
 1825 FORMAT ('REFLFG:',I5,' 0-value cells flagged',I12,' flagged')
      END
      SUBROUTINE REFLHI
C-----------------------------------------------------------------------
C   REFLHI adds to input file history
C-----------------------------------------------------------------------
C
      INCLUDE 'REFLG.INC'
      INTEGER   HLUN, IERR, DATE(3), TIME(3), I, IROUND
      CHARACTER HILINE*72, CTIME*8, CDATE*12
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA HLUN /28/
C-----------------------------------------------------------------------
      CALL HIINIT (3)
      CALL HIOPEN (HLUN, DISKIN, OLDCNO, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Write time and date on new file
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME, CDATE)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CDATE, CTIME
      CALL HIADD (HLUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1010) TSKNAM, IFGVER
      CALL HIADD (HLUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1015) TSKNAM, OFGVER
      CALL HIADD (HLUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1020) TSKNAM, CPARM(1)
      CALL HIADD (HLUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (FEXT.GT.0) THEN
         WRITE (HILINE,1024) TSKNAM, CPARM(2), FEXT
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         MSGTXT = HILINE(7:)
         CALL MSGWRT (4)
         END IF
      IF (FCHAN.GT.0) THEN
         WRITE (HILINE,1025) TSKNAM, CPARM(3), FCHAN
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         MSGTXT = HILINE(7:)
         CALL MSGWRT (4)
         END IF
      IF (FTIME.GT.0) THEN
         WRITE (HILINE,1030) TSKNAM, CPARM(4), FTIME
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         MSGTXT = HILINE(7:)
         CALL MSGWRT (4)
         END IF
      IF (FBL.GT.0) THEN
         WRITE (HILINE,1035) TSKNAM, CPARM(5), FBL
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         MSGTXT = HILINE(7:)
         CALL MSGWRT (4)
         END IF
      IF (FANTS.GT.0) THEN
         WRITE (HILINE,1040) TSKNAM, CPARM(6), FANTS
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         MSGTXT = HILINE(7:)
         CALL MSGWRT (4)
         END IF
      I = IROUND (CPARM(7))
      IF (I.GT.0) THEN
         IF (I.EQ.1) THEN
            WRITE (HILINE,1045) TSKNAM, I
         ELSE
            WRITE (HILINE,1046) TSKNAM, I
            END IF
         CALL HIADD (HLUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      WRITE (HILINE,1050) TSKNAM, NFGSCR, NFGOUT
      CALL HIADD (HLUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (HLUN, .TRUE., BUFF1, I)
C
 900  IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO WRITE HISTORY FILE'
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',A12,2X,A8)
 1010 FORMAT (A6,'FLAGVER=',I6,'  / input FG table version')
 1015 FORMAT (A6,'OUTFGVER=',I5,'  / output FG table version')
 1020 FORMAT (A6,'CPARM(1)=',F5.1,'  / time interval in sec')
 1024 FORMAT (A6,'CPARM(2)=',F3.0,'  / flagged',I11,' chan between',
     *   ' flagged chan')
 1025 FORMAT (A6,'CPARM(3)=',F5.3,'  / flagged',I9,
     *   ' for excess over channels')
 1030 FORMAT (A6,'CPARM(4)=',F5.3,'  / flagged',I9,
     *   ' for excess over time')
 1035 FORMAT (A6,'CPARM(5)=',F5.3,'  / flagged',I9,
     *   ' for excess over baselines')
 1040 FORMAT (A6,'CPARM(6)=',F5.3,'  / flagged',I9,
     *   ' for excess over antennas')
 1045 FORMAT (A6,'CPARM(7)=',I2,'  / flag cross-hands if parallel',
     *   ' flagged')
 1046 FORMAT (A6,'CPARM(7)=',I2,'  / flag all polarizations if any',
     *   ' flagged')
 1050 FORMAT (A6,'/ input records',I12,' output records',I12)
      END
      SUBROUTINE REFLGL (IRET)
C-----------------------------------------------------------------------
C   REFLGL looks over the output of the baseline-dependent time-freq
C   FG records and finds more global flags if possible
C   Output:
C      IRET   I   > 0 something bad happened
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   MAXFG
      PARAMETER (MAXFG=100000)
      INCLUDE 'REFLG.INC'
C
      CHARACTER PHNAME*48, REASON*24, TREAS*24, ADATE*12, ATIME*8
      INTEGER   VER, IERR, KEY(2,2), LUN1, LUN2, LUNTMP, IFGRNO, ISU,
     *   ISUB, IFQ, IANTS(2), IFS(2), ICHANS(2), JSU(MAXFG), JFQ(MAXFG),
     *   JSUB(MAXFG), JANTS(2,MAXFG), JFS(2,MAXFG), JCHANS(2,MAXFG),
     *   IFLS, JFLS(MAXFG), DATE(3), TIME(3), NLIST, JLIST, IREC, NREC,
     *   ZAND, KLIST, MATCH1(MAXFG), NMATCH, IFMAT(MAXIF), NG, IIF, I,
     *   JIF, NDELIF, ANTMAT(2,MAXANT), MMATCH, NBLANT(MAXANT), IA1,
     *   IA2, MATCH2(MAXFG), NDELAN, JBL, KIF, KEYSUB(2,2), ICP8
      REAL      FKEY(2,2), TIMER(2), JTIMER(2,MAXFG), CTIME, TEPS
      LOGICAL   PFLAGS(4), INONE, DOIF, DOBL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA KEY  /5,0, 1,0/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      ICP8 = CPARM(8) + 0.1
      DOIF = ICP8.LE.1
      DOBL = (ICP8.LE.0) .OR. ((ICP8/2)*2.EQ.ICP8)
      TEPS = 0.05 / (24.0 * 3600.0)
      NDELIF = 0
      NDELAN = 0
      CALL FILL (MAXANT, 0, NBLANT)
      DO 20 IA1 = 1,NANT
         DO 10 IA2 = IA1,NANT
            JBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
            IF (BLEXIS(JBL).GT.0) THEN
               NBLANT(IA1) = NBLANT(IA1) + 1
               NBLANT(IA2) = NBLANT(IA2) + 1
               END IF
 10         CONTINUE
 20      CONTINUE
      DO 21 IA1 = 1,NANT
         IF (NBLANT(IA1).LE.0) NBLANT(IA1) = 1000
 21      CONTINUE
C                                       delete the scratch FG table
C                                       ignore errors here
      VER = OFGVER + 1
      CALL ZPHFIL ('FG', DISKIN, OLDCNO, VER, PHNAME, IERR)
      CALL ZDESTR (DISKIN, PHNAME, IERR)
C      CALL DELEXT ('FG', DISKIN, OLDCNO, 'WRWR', BUFF1, FGBUF1, VER,
C     *   IERR)
C                                       sort the current output to SC
C                                       does not upgrade the header!
C                                       so DELEXT is omitted.
      MSGTXT = 'Sorting FG table'
      CALL MSGWRT (2)
C                                       get size of input
      LUN1 = LUNTMP (1)
      CALL FLGINI ('READ', FGBUF1, DISKIN, OLDCNO, OFGVER, CATBLK, LUN1,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT FG TABLE'
         GO TO 990
         END IF
      CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, ISU, ISUB,
     *   IFQ, IANTS, TIMER, IFS, ICHANS, PFLAGS, TREAS, IERR)
      CALL TABSRT (DISKIN, OLDCNO, 'FG', OFGVER, VER, KEY, KEYSUB, FKEY,
     *   FGBUF1, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SORTING OUTPUT FG TABLE TO SC'
         GO TO 990
         END IF
C                                       set reason for this call
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
      REASON = TSKNAM // ADATE // ATIME(:5)
C                                       open the sorted file read
      CALL FLGINI ('READ', FGBUF1, DISKIN, OLDCNO, VER, CATBLK, LUN1,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SORTED FG TABLE'
         GO TO 990
         END IF
      LUN2 = LUNTMP (1)
      CALL FLGINI ('WRIT', FGBUF2, DISKIN, OLDCNO, OFGVER, CATBLK, LUN2,
     *   OFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING SORTED FG TABLE'
         GO TO 990
         END IF
      NREC = FGBUF1(5)
      FGBUF2(5) = 0
      OFGRNO = 1
      CTIME = -1.E6
      NLIST = 0
      DO 800 IREC = 1,NREC
         CALL TABFLG ('READ', FGBUF1, IFGRNO, FGKOLS, FGNUMV, ISU, ISUB,
     *      IFQ, IANTS, TIMER, IFS, ICHANS, PFLAGS, TREAS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING SORTED FG SC TABLE'
            GO TO 990
            END IF
         IF (ICHANS(1).LE.0) ICHANS(1) = 1
         IF (ICHANS(2).LE.0) ICHANS(2) = NCHAN
         IF (IFS(1).LE.0) IFS(1) = 1
         IF (IFS(2).LE.0) IFS(2) = NIF
C                                       add to list
         IF ((TIMER(1).LE.CTIME) .AND. (NLIST.LE.MAXFG)) THEN
            NLIST = NLIST + 1
            JSU(NLIST) = ISU
            JSUB(NLIST) = ISUB
            JFQ(NLIST) = IFQ
            JANTS(1,NLIST) = IANTS(1)
            JANTS(2,NLIST) = IANTS(2)
            JTIMER(1,NLIST) = TIMER(1)
            JTIMER(2,NLIST) = TIMER(2)
            JFS(1,NLIST) = IFS(1)
            JFS(2,NLIST) = IFS(2)
            JCHANS(1,NLIST) = ICHANS(1)
            JCHANS(2,NLIST) = ICHANS(2)
            IFLS = 0
            IF (PFLAGS(1)) IFLS = IFLS + 1
            IF (PFLAGS(2)) IFLS = IFLS + 2
            IF (PFLAGS(3)) IFLS = IFLS + 4
            IF (PFLAGS(4)) IFLS = IFLS + 8
            JFLS(NLIST) = IFLS
            END IF
C                                       process/output list
         IF (((TIMER(1).GT.CTIME) .OR. (IREC.EQ.NREC)) .AND.
     *      (NLIST.GT.0)) THEN
C                                       combine IFs?
            JLIST = 0
 110        JLIST = JLIST + 1
            IF ((JLIST.LT.NLIST) .AND. (DOIF)) THEN
               IF (JSU(JLIST).LT.0) GO TO 110
               NMATCH = 1
               MATCH1(1) = JLIST
               KLIST = JLIST
               CALL FILL (NIF, 0, IFMAT)
               I = JFS(2,KLIST) - JFS(1,KLIST) + 1
               CALL FILL (I, 1, IFMAT(JFS(1,KLIST)))
 115           KLIST = KLIST + 1
               IF (KLIST.LE.NLIST) THEN
                  IF ((JSU(JLIST).EQ.JSU(KLIST)) .AND.
     *               (JSUB(JLIST).EQ.JSUB(KLIST)) .AND.
     *               (JFQ(JLIST).EQ.JFQ(KLIST)) .AND.
     *               (JFLS(JLIST).EQ.JFLS(KLIST)) .AND.
     *               (JANTS(1,JLIST).EQ.JANTS(1,KLIST)) .AND.
     *               (JANTS(2,JLIST).EQ.JANTS(2,KLIST)) .AND.
     *               (JCHANS(1,JLIST).EQ.JCHANS(1,KLIST)) .AND.
     *               (JCHANS(2,JLIST).EQ.JCHANS(2,KLIST)) .AND.
C     *               (ABS(JTIMER(1,JLIST)-JTIMER(1,KLIST)).LE.TEPS)
C     *               .AND.
     *               (ABS(JTIMER(2,JLIST)-JTIMER(2,KLIST)).LE.TEPS))
     *               THEN
                     NMATCH = NMATCH + 1
                     MATCH1(NMATCH) = KLIST
                     I = JFS(2,KLIST) - JFS(1,KLIST) + 1
                     CALL FILL (I, 1, IFMAT(JFS(1,KLIST)))
                     END IF
                  GO TO 115
                  END IF
C                                       matches?
               IF (NMATCH.GT.1) THEN
                  NG = 0
                  INONE = .FALSE.
                  DO 120 IIF = 1,NIF
                     IF ((.NOT.INONE) .AND. (IFMAT(IIF).GT.0)) THEN
                        NG = NG + 1
                        INONE = .TRUE.
                     ELSE IF ((INONE) .AND. (IFMAT(IIF).LE.0)) THEN
                        INONE = .FALSE.
                        END IF
 120                 CONTINUE
                  IF (NG.LT.NMATCH) THEN
                     INONE = .FALSE.
                     KLIST = 0
                     DO 125 IIF = 1,NIF
                        IF (IFMAT(IIF).GT.0) THEN
                           IF (.NOT.INONE) THEN
                              JIF = IIF
                              INONE = .TRUE.
                              END IF
                           KIF = IIF
                        ELSE IF (INONE) THEN
                           KLIST = KLIST + 1
                           I = MATCH1(KLIST)
                           JFS(1,I) = JIF
                           JFS(2,I) = KIF
                           INONE = .FALSE.
                           END IF
 125                    CONTINUE
                     IF (INONE) THEN
                        KLIST = KLIST + 1
                        I = MATCH1(KLIST)
                        JFS(1,I) = JIF
                        JFS(2,I) = KIF
                        END IF
 130                 KLIST = KLIST + 1
                     IF (KLIST.LE.NMATCH) THEN
                        I = MATCH1(KLIST)
                        JSU(I) = -999999
                        NDELIF = NDELIF + 1
                        GO TO 130
                        END IF
                     END IF
                  END IF
               GO TO 110
               END IF
C                                       combine baselines
            JLIST = 0
 210        JLIST = JLIST + 1
            IF ((JLIST.LT.NLIST) .AND. (DOBL)) THEN
               IF (JSU(JLIST).LT.0) GO TO 210
               NMATCH = 1
               MMATCH = 1
               MATCH1(1) = JLIST
               MATCH2(1) = JLIST
               KLIST = JLIST
               CALL FILL (2*MAXANT, 0, ANTMAT)
               IA1 = JANTS(1,JLIST)
               IA2 = JANTS(2,JLIST)
               IF ((IA1.LE.0) .OR. (IA2.LE.0)) GO TO 210
               ANTMAT(1,IA1) = 1
               ANTMAT(2,IA2) = 1
 215           KLIST = KLIST + 1
               IF (KLIST.LE.NLIST) THEN
                  IF ((JSU(JLIST).EQ.JSU(KLIST)) .AND.
     *               (JSUB(JLIST).EQ.JSUB(KLIST)) .AND.
     *               (JFQ(JLIST).EQ.JFQ(KLIST)) .AND.
     *               (JFLS(JLIST).EQ.JFLS(KLIST)) .AND.
     *               (JFS(1,JLIST).EQ.JFS(1,KLIST)) .AND.
     *               (JFS(2,JLIST).EQ.JFS(2,KLIST)) .AND.
     *               (JCHANS(1,JLIST).EQ.JCHANS(1,KLIST)) .AND.
     *               (JCHANS(2,JLIST).EQ.JCHANS(2,KLIST)) .AND.
C    *               (ABS(JTIMER(1,JLIST)-JTIMER(1,KLIST)).LE.TEPS)
C    *               .AND.
     *               (ABS(JTIMER(2,JLIST)-JTIMER(2,KLIST)).LE.TEPS))
     *               THEN
                     IF (JANTS(1,KLIST).EQ.IA1) THEN
                        NMATCH = NMATCH + 1
                        MATCH1(NMATCH) = KLIST
                        ANTMAT(1,JANTS(1,KLIST)) = 1
                     ELSE IF (JANTS(2,KLIST).EQ.IA1) THEN
                        NMATCH = NMATCH + 1
                        MATCH1(NMATCH) = KLIST
                        ANTMAT(1,JANTS(2,KLIST)) = 1
                        END IF
                     IF (JANTS(1,KLIST).EQ.IA2) THEN
                        MMATCH = MMATCH + 1
                        MATCH2(MMATCH) = KLIST
                        ANTMAT(2,JANTS(1,KLIST)) = 1
                     ELSE IF (JANTS(2,KLIST).EQ.IA2) THEN
                        MMATCH = MMATCH + 1
                        MATCH2(MMATCH) = KLIST
                        ANTMAT(2,JANTS(2,KLIST)) = 1
                        END IF
                     END IF
                  GO TO 215
                  END IF
C                                       matches?
               IF ((NMATCH.GE.NBLANT(IA1)) .OR. (MMATCH.GE.NBLANT(IA2)))
     *            THEN
C                                       both antennas
                  IF ((NMATCH.GE.NBLANT(IA1)) .AND.
     *               (MMATCH.GE.NBLANT(IA2))) THEN
                     I = MATCH1(1)
                     JANTS(1,I) = IA1
                     JANTS(2,I) = 0
                     I = MATCH1(2)
                     JANTS(1,I) = IA2
                     JANTS(2,I) = 0
                     DO 220 KLIST = 3,NMATCH
                        I = MATCH1(KLIST)
                        JSU(I) = -999999
                        NDELAN = NDELAN + 1
 220                    CONTINUE
                     DO 225 KLIST = 1,MMATCH
                        I = MATCH2(KLIST)
                        JSU(I) = -999999
                        NDELAN = NDELAN + 1
 225                    CONTINUE
C                                       antenna 1 only
                  ELSE IF (NMATCH.GE.NBLANT(IA1)) THEN
                     I = MATCH1(2)
                     JANTS(1,I) = IA1
                     JANTS(2,I) = 0
                     DO 230 KLIST = 3,NMATCH
                        I = MATCH1(KLIST)
                        JSU(I) = -999999
                        NDELAN = NDELAN + 1
 230                    CONTINUE
C                                       antenna 2 only
                  ELSE
                     I = MATCH2(2)
                     JANTS(1,I) = IA2
                     JANTS(2,I) = 0
                     DO 235 KLIST = 3,NMATCH
                        I = MATCH2(KLIST)
                        JSU(I) = -999999
                        NDELAN = NDELAN + 1
 235                    CONTINUE
                     END IF
                  END IF
               GO TO 210
               END IF
C                                       dump what's left
            DO 700 JLIST = 1,NLIST
               IF (JSU(JLIST).GE.0) THEN
                  PFLAGS(1) = ZAND(JFLS(JLIST),1).NE.0
                  PFLAGS(2) = ZAND(JFLS(JLIST),1).NE.0
                  PFLAGS(3) = ZAND(JFLS(JLIST),1).NE.0
                  PFLAGS(4) = ZAND(JFLS(JLIST),1).NE.0
                  CALL TABFLG ('WRIT', FGBUF2, OFGRNO, FGKOLS, FGNUMV,
     *               JSU(JLIST), JSUB(JLIST), JFQ(JLIST),
     *               JANTS(1,JLIST), JTIMER(1,JLIST), JFS(1,JLIST),
     *               JCHANS(1,JLIST), PFLAGS, REASON, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT FG TABLE'
                     GO TO 990
                     END IF
                  END IF
 700           CONTINUE
            END IF
C                                       start new list
         IF ((TIMER(1).GT.CTIME) .AND. (IREC.LT.NREC)) THEN
            NLIST = 1
            JSU(NLIST) = ISU
            JSUB(NLIST) = ISUB
            JFQ(NLIST) = IFQ
            JANTS(1,NLIST) = IANTS(1)
            JANTS(2,NLIST) = IANTS(2)
            JTIMER(1,NLIST) = TIMER(1)
            JTIMER(2,NLIST) = TIMER(2)
            JFS(1,NLIST) = IFS(1)
            JFS(2,NLIST) = IFS(2)
            JCHANS(1,NLIST) = ICHANS(1)
            JCHANS(2,NLIST) = ICHANS(2)
            IFLS = 0
            IF (PFLAGS(1)) IFLS = IFLS + 1
            IF (PFLAGS(2)) IFLS = IFLS + 2
            IF (PFLAGS(3)) IFLS = IFLS + 4
            IF (PFLAGS(4)) IFLS = IFLS + 8
            JFLS(NLIST) = IFLS
            CTIME = TIMER(1) + TEPS
            END IF
 800     CONTINUE
C                                       close FG tables
      CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, ISU, ISUB,
     *   IFQ, IANTS, TIMER, IFS, ICHANS, PFLAGS, TREAS, IERR)
      CALL TABFLG ('CLOS', FGBUF2, OFGRNO, FGKOLS, FGNUMV, ISU, ISUB,
     *   IFQ, IANTS, TIMER, IFS, ICHANS, PFLAGS, TREAS, IERR)
      OFGRNO = OFGRNO - 1
C                                       selection algorithms
      WRITE (MSGTXT,1800) NDELIF
      CALL MSGWRT (5)
      WRITE (MSGTXT,1801) NDELAN
      CALL MSGWRT (5)
C                                       new summary
      WRITE (MSGTXT,1810) IFGVER, NFGSCR
      CALL MSGWRT (5)
      WRITE (MSGTXT,1811) NFGOUT
      CALL MSGWRT (5)
      WRITE (MSGTXT,1812) OFGVER, OFGRNO
      CALL MSGWRT (5)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REFLGL: ERROR',I5,' ON ',A)
 1800 FORMAT ('REFLGL: deleted',I8,' records combining IFs')
 1801 FORMAT ('REFLGL: deleted',I8,' records combining antennas')
 1810 FORMAT ('Input FG version',I5,' has ',I10,' flag records')
 1811 FORMAT ('After Time-freq step  has ',I10,' flag records')
 1812 FORMAT ('Output FG version',I4,' has ',I10,' flag records')
       END
