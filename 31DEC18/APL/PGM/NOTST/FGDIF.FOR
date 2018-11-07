LOCAL INCLUDE 'FGDIF.INC'
C                                       Local include for FGDIF
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXTIM, MAXSOU
      PARAMETER (MAXTIM=100000)
      PARAMETER (MAXSOU=300)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XCALC
      REAL      XSIN, XDISIN, XFLAG, XFLAG2, CPARM(10), BADD(10)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, XCALC,
     *   XFLAG, XFLAG2, CPARM, BADD
C
      INTEGER   SEQIN, DISKIN, JBUFSZ, OLDCNO, NUMAN(513), NANT, NSUB,
     *   NTIMES, NCHAN, NIF, FGVER1, FGVER2, NUMSU, NUMFQ, FGBUF1(512),
     *   FGBUF2(512), FGKOLS(MAXFGC), FGNUMV(MAXFGC), SUNUMS(MAXSOU),
     *   N1ONLY, N2ONLY, NBOTH, MAXMSG, NMSG
      CHARACTER NAMEIN*12, CLAIN*6, SNMS(MAXSOU)*16, XSOUR(30)*16,
     *   CLCODE*4
      REAL      BUFF1(UVBFSS), TIMES(2,MAXTIM)
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /FGDIFC/ FGBUF1, FGBUF2, TIMES, NTIMES, SEQIN, DISKIN,
     *   OLDCNO, NUMAN, NANT, NSUB, NCHAN, NIF, FGVER1, FGVER2, NUMSU,
     *   NUMFQ, FGKOLS, FGNUMV, SUNUMS, N1ONLY, N2ONLY, NBOTH, MAXMSG,
     *   NMSG
      COMMON /CHRCOM/ NAMEIN, CLAIN, SNMS, XSOUR, CLCODE
LOCAL END
      PROGRAM FGDIF
C-----------------------------------------------------------------------
C! Compares 2 FG tables
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2011-2012, 2015-2016, 2018
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
C      CPARM......1=max. gap, 2=max scan, 3=CL/CS entry interval,
C                 4=recalc CL group delays from IM table (VLBA only).
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   ISUB, IRET, IFLAG(2), NWORDS, NBL, ISU, IFQ, IIF,
     *   JFLAG(2), JSU
      LONGINT   PIFLAG, PJFLAG
      INCLUDE 'FGDIF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'FGDIF '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL FGDIFI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      NBL = (NANT * (NANT+1)) / 2
      N1ONLY = 0
      N2ONLY = 0
      NBOTH = 0
C                                       loop over subarrays, sources,
C                                       FQs
      DO 100 IFQ = 1,NUMFQ
         DO 90 ISUB = 1,NSUB
            DO 80 JSU = 1,NUMSU
               ISU = SUNUMS(JSU)
C                                       Get list of times
               CALL FGDIFT (JSU, ISUB, IFQ, IRET)
               IF (IRET.NE.0) GO TO 990
               IF (NTIMES.GT.0) THEN
                  NWORDS = (NCHAN * NBL * NTIMES - 1) / 1024 + 2
                  CALL ZMEMRY ('GET ', PRGM, NWORDS, IFLAG, PIFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO GET REQUIRED MEMORY'
                     CALL MSGWRT (8)
                     GO TO 990
                     END IF
                  CALL ZMEMRY ('GET ', PRGM, NWORDS, JFLAG, PJFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO GET REQUIRED MEMORY'
                     CALL MSGWRT (8)
                     GO TO 990
                     END IF
C                                       redo flags
                  DO 70 IIF = 1,NIF
                     CALL FGDIFD (IIF, ISU, ISUB, IFQ, NCHAN, NBL,
     *                  IFLAG(1+PIFLAG), JFLAG(1+PJFLAG), IRET)
                     IF (IRET.NE.0) GO TO 990
 70                  CONTINUE
                  CALL ZMEMRY ('FREE', PRGM, NWORDS, IFLAG, PIFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO FREE DYNAMIC MEMORY'
                     CALL MSGWRT (8)
                     GO TO 990
                     END IF
                  CALL ZMEMRY ('FREE', PRGM, NWORDS, JFLAG, PJFLAG,
     *               IRET)
                  IF (IRET.NE.0) THEN
                     MSGTXT = 'FAILED TO FREE DYNAMIC MEMORY'
                     CALL MSGWRT (8)
                     GO TO 990
                     END IF
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C                                       summary
      IF (N1ONLY+N2ONLY+NBOTH.LE.0) THEN
         MSGTXT = 'No differences found'
         CALL MSGWRT (5)
      ELSE
         WRITE (MSGTXT,1100) N1ONLY, 'flag in 1 not in 2'
         CALL MSGWRT (5)
         WRITE (MSGTXT,1100) N2ONLY, 'flag in 2 not in 1'
         CALL MSGWRT (5)
         WRITE (MSGTXT,1100) NBOTH, 'flag in 1 and 2 differ'
         CALL MSGWRT (5)
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1100 FORMAT ('Total differences',I12,' cells ',A)
      END
      SUBROUTINE FGDIFI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FGDIFI gets input parameters for FGDIF and finds input file.
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
      INCLUDE 'FGDIF.INC'
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
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 150
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
      MAXMSG = CPARM(4) + 0.1
      IF (MAXMSG.LE.0) MAXMSG = 100
      NMSG = 0
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
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
      FRW(NCFILE) = 1
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
      IF (I.LE.1) THEN
         MSGTXT = 'NOT ENOUGH FG TABLES TO COMPARE'
         JERR = 3
         GO TO 990
         END IF
      FGVER1 = IROUND (XFLAG)
      IF ((FGVER1.LE.0) .OR. (FGVER1.GT.I)) FGVER1 = I
      FGVER2 = IROUND (XFLAG2)
      IF ((FGVER2.LE.0) .OR. (FGVER2.GT.I)) FGVER2 = I
      IF ((FGVER1.EQ.I) .AND. (FGVER2.EQ.I)) FGVER1 = I - 1
      IF ((FGVER1.EQ.FGVER2) .AND. (FGVER2.LT.I)) FGVER1 = I
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
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGDIFI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('WRONG SORT ORDER(',A2,'), USE UVSRT TO SORT TO ''TB''')
 1070 FORMAT ('FGDIFI: ERROR ',I3,' DETERMINING NUMBER OF ANTENNAS')
 1100 FORMAT ('FGDIFI ERROR:',I5,' ON ',A)
      END
      SUBROUTINE FGDIFT (ISU, ISUBA, IFQ, IRET)
C-----------------------------------------------------------------------
C   FGDIFT finds the list of times
C   Input:
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   ISU, ISUBA, IFQ, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, IA1, IA2, KBASE, NBL, ISDAT(MXBASE), JBL, CATSAV(256)
      LOGICAL   GETNEW
      REAL      CURTIM, TLIMIT, TINT, TB, RPARM(20)
      INCLUDE 'FGDIF.INC'
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
      CALL UVGET ('CLOS', RPARM, BUFF1, I)
 980  CALL COPY (256, CATSAV, CATBLK)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGDIFT: ERROR',I3,' ON ',A)
      END
      SUBROUTINE FGDIFD (IIF, ISU, ISUB, IFQ, NC, NBL, IFLAG, JFLAG,
     *   IRET)
C-----------------------------------------------------------------------
C   Does the heavy lifting for FGDIF
C   Inputs:
C      IIF      I      IF number this pass
C      ISU      I      Source number
C      ISUB     I      Subarray number
C      IFQ      I      Frequency ID
C      NC       I      Number channels
C      NBL      I      Number baselines
C   Output:
C      IFLAG    I(*)   Work memory (NC,NBL,number of times)
C      JFLAG    I(*)   Work memory (NC,NBL,number of times)
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   IIF, ISU, ISUB, IFQ, NC, NBL, IFLAG(NC,NBL,*),
     *   JFLAG(NC,NBL,*), IRET
C
      INCLUDE 'FGDIF.INC'
      INTEGER   LUN, LUNTMP, VER, NREC, IREC, IFGRNO, SOURID, SUBA,
     *   FREQID, ANTS(2), IFS(2), CHANS(2), NFG1, NFG2, IFLAGS, ITB,
     *   ITE, IT0, IA1, IA2, ZOR, IC, IT, IBL, MM, NN, II, JJ, KK, ID,
     *   ITT(3), IST, NST
      REAL      TIMER(2), TTEMP, RTT
      LOGICAL   PFLAGS(4), DOIT
      CHARACTER REASON*24
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      NST = CATBLK(KINAX+JLOCS)
      NFG1 = 0
      NFG2 = 0
      IC = NC * NBL * NTIMES
      CALL FILL (IC, 0, IFLAG)
      CALL FILL (IC, 0, JFLAG)
C                                       open scratch FG
      WRITE (MSGTXT,1010) IIF, ISU, ISUB, IFQ
      CALL MSGWRT (2)
      VER = FGVER1
      LUN = LUNTMP (1)
      CALL FLGINI ('READ', FGBUF1, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING FG TABLE 1'
         GO TO 990
         END IF
      NREC = FGBUF1(5)
      IT0 = 1
      DO 70 IREC = 1,NREC
         CALL TABFLG ('READ', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FG TABLE 1'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       applies to this data subset
            DOIT = .TRUE.
            IF (CPARM(2).GT.0.0) THEN
               IF ((ANTS(1).LE.0) .OR. (ANTS(2).LE.0)) DOIT = .FALSE.
               IF ((IFS(1).LE.0) .OR. (IFS(2).LE.0)) DOIT = .FALSE.
               IF ((CHANS(1).LE.0) .OR. (CHANS(2).LE.0)) DOIT = .FALSE.
               IF ((TIMER(1).LE.0.0) .AND. (TIMER(2).GE.1000.))
     *            DOIT = .FALSE.
               END IF
            IF (IFS(2).EQ.0) IFS(2) = MAX (IFS(1), IIF)
            IF ((((IFS(1).LE.IIF) .AND. (IFS(2).GE.IIF)) .OR.
     *         (IFS(1).EQ.0)) .AND. ((SUBA.LE.0) .OR. (SUBA.EQ.ISUB))
     *         .AND. ((FREQID.LE.0) .OR. (FREQID.EQ.IFQ)) .AND.
     *         ((SOURID.LE.0) .OR. (SOURID.EQ.ISU)) .AND. (DOIT)) THEN
               NFG1 = NFG1 + 1
               IFLAGS = 0
               DO 5 IST = 1,NST
                  IF (PFLAGS(1)) IFLAGS = IFLAGS + 2 ** (IST-1)
 5                CONTINUE
               IF ((CHANS(1).LE.0) .OR. (CHANS(2).LE.0)) THEN
                  CHANS(1) = 1
                  CHANS(2) = NC
                  END IF
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
               GO TO 75
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
               IF ((IA1.GT.0) .AND. (IA2.GT.0)) THEN
                  IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
                  DO 35 IT = ITB,ITE
                     DO 30 IC = CHANS(1),CHANS(2)
                        IFLAG(IC,IBL,IT) = ZOR (IFLAG(IC,IBL,IT),IFLAGS)
 30                     CONTINUE
 35                  CONTINUE
               ELSE IF ((IA1.LE.0) .AND. (IA2.GT.0)) THEN
                  IA1 = IA2
                  DO 50 IA2 = 1,NANT
                     IF (IA1.LE.IA2) THEN
                        IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
                     ELSE
                        IBL = NANT * (IA2-1) - ((IA2*(IA2-1))/2) + IA1
                        END IF
                     DO 45 IT = ITB,ITE
                        DO 40 IC = CHANS(1),CHANS(2)
                           IFLAG(IC,IBL,IT) = ZOR (IFLAG(IC,IBL,IT),
     *                        IFLAGS)
 40                        CONTINUE
 45                     CONTINUE
 50                  CONTINUE
               ELSE
                  DO 65 IBL = 1,NBL
                     DO 60 IT = ITB,ITE
                        DO 55 IC = CHANS(1),CHANS(2)
                           IFLAG(IC,IBL,IT) = ZOR (IFLAG(IC,IBL,IT),
     *                        IFLAGS)
 55                        CONTINUE
 60                     CONTINUE
 65                  CONTINUE
                  END IF
               END IF
            END IF
 70      CONTINUE
 75   CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *   SUBA,FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING SCRATCH FG TABLE'
         GO TO 990
         END IF
C                                       open scratch FG
      VER = FGVER2
      LUN = LUNTMP (1)
      CALL FLGINI ('READ', FGBUF1, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING FG TABLE 2'
         GO TO 990
         END IF
      NREC = FGBUF1(5)
      IT0 = 1
      DO 170 IREC = 1,NREC
         CALL TABFLG ('READ', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *      SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON,
     *      IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FG TABLE 2'
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       applies to this data subset
            DOIT = .TRUE.
            IF (CPARM(2).GT.0.0) THEN
               IF ((ANTS(1).LE.0) .OR. (ANTS(2).LE.0)) DOIT = .FALSE.
               IF ((IFS(1).LE.0) .OR. (IFS(2).LE.0)) DOIT = .FALSE.
               IF ((CHANS(1).LE.0) .OR. (CHANS(2).LE.0)) DOIT = .FALSE.
               IF ((TIMER(1).LE.0.0) .AND. (TIMER(2).GE.1000.))
     *            DOIT = .FALSE.
               END IF
            IF (IFS(2).EQ.0) IFS(2) = MAX (IFS(1), IIF)
            IF ((((IFS(1).LE.IIF) .AND. (IFS(2).GE.IIF)) .OR.
     *         (IFS(1).EQ.0)) .AND. ((SUBA.LE.0) .OR. (SUBA.EQ.ISUB))
     *         .AND. ((FREQID.LE.0) .OR. (FREQID.EQ.IFQ)) .AND.
     *         ((SOURID.LE.0) .OR. (SOURID.EQ.ISU)) .AND. (DOIT)) THEN
               NFG2 = NFG2 + 1
               IFLAGS = 0
               DO 105 IST = 1,NST
                  IF (PFLAGS(1)) IFLAGS = IFLAGS + 2 ** (IST-1)
 105              CONTINUE
C                                       find start time
               IT0 = 1
               DO 110 IT = IT0,NTIMES
                  IF (TIMER(1).GT.TIMES(2,IT)) THEN
                     IT0 = IT0 + 1
                  ELSE
                     ITB = IT0
                     GO TO 115
                     END IF
 110              CONTINUE
C                                       done
               GO TO 175
C                                       find end time
 115           DO 120 IT = ITB,NTIMES
                  IF (TIMER(2).LT.TIMES(1,IT)) THEN
                     ITE = IT-1
                     GO TO 125
                     END IF
 120              CONTINUE
               ITE = NTIMES
C                                       mark array
 125           IA1 = MIN (ANTS(1), ANTS(2))
               IA2 = MAX (ANTS(1), ANTS(2))
               IF ((IA1.GT.0) .AND. (IA2.GT.0)) THEN
                  IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
                  DO 135 IT = ITB,ITE
                     DO 130 IC = CHANS(1),CHANS(2)
                        JFLAG(IC,IBL,IT) = ZOR (JFLAG(IC,IBL,IT),IFLAGS)
 130                    CONTINUE
 135                 CONTINUE
               ELSE IF ((IA1.LE.0) .AND. (IA2.GT.0)) THEN
                  IA1 = IA2
                  DO 150 IA2 = 1,NANT
                     IF (IA1.LE.IA2) THEN
                        IBL = NANT * (IA1-1) - ((IA1*(IA1-1))/2) + IA2
                     ELSE
                        IBL = NANT * (IA2-1) - ((IA2*(IA2-1))/2) + IA1
                        END IF
                     DO 145 IT = ITB,ITE
                        DO 140 IC = CHANS(1),CHANS(2)
                           JFLAG(IC,IBL,IT) = ZOR (JFLAG(IC,IBL,IT),
     *                        IFLAGS)
 140                       CONTINUE
 145                    CONTINUE
 150                 CONTINUE
               ELSE
                  DO 165 IBL = 1,NBL
                     DO 160 IT = ITB,ITE
                        DO 155 IC = CHANS(1),CHANS(2)
                           JFLAG(IC,IBL,IT) = ZOR (JFLAG(IC,IBL,IT),
     *                        IFLAGS)
 155                       CONTINUE
 160                    CONTINUE
 165                 CONTINUE
                  END IF
               END IF
            END IF
 170     CONTINUE
 175  CALL TABFLG ('CLOS', FGBUF1, IFGRNO, FGKOLS, FGNUMV, SOURID,
     *   SUBA,FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING SCRATCH FG TABLE'
         GO TO 990
         END IF
C                                       double check
      MM = 0
      NN = 0
      JJ = 0
      II = 0
      KK = 0
      DO 190 IT = 1,NTIMES
         IA1 = 1
         IA2 = 0
         DO 185 IBL = 1,NBL
            IA2 = IA2 + 1
            IF (IA2.GT.NANT) THEN
               IA1 = IA1 + 1
               IA2 = IA1
               END IF
            DO 180 IC = 1,NC
               ID = 0
               IF (IFLAG(IC,IBL,IT).GT.0) THEN
                  MM = MM + 1
                  ID = 1
                  IF (JFLAG(IC,IBL,IT).EQ.0) II = II + 1
                  END IF
               IF (JFLAG(IC,IBL,IT).GT.0) THEN
                  NN = NN + 1
                  ID = ID + 2
                  IF (IFLAG(IC,IBL,IT).EQ.0) JJ = JJ + 1
                  END IF
               IF (JFLAG(IC,IBL,IT).NE.IFLAG(IC,IBL,IT)) THEN
                  KK = KK + 1
                  IF ((ABS(CPARM(3)-IIF).LT.0.1) .AND. (NMSG.LT.MAXMSG))
     *               THEN
                     NMSG = NMSG + 1
                     TTEMP = (TIMES(1,IT) + TIMES(2,IT)) / 2.0
                     CALL TIDHMS (TTEMP, 1, ITT, RTT)
                     WRITE (MSGTXT,1175) IC, IA1, IA2, ITT, RTT, ID
                     CALL MSGWRT (2)
                     END IF
                  END IF
 180           CONTINUE
 185        CONTINUE
 190     CONTINUE
      WRITE (MSGTXT,1190) NFG1, FGVER1, MM
      CALL MSGWRT (4)
      WRITE (MSGTXT,1190) NFG2, FGVER2, NN
      CALL MSGWRT (4)
      IF (KK.GT.0) THEN
         WRITE (MSGTXT,1195) II, ' flag', NFG1, ' not', NFG2
         CALL MSGWRT (4)
         N1ONLY = N1ONLY + II
         WRITE (MSGTXT,1195) JJ, ' flag', NFG2, ' not', NFG1
         CALL MSGWRT (4)
         N2ONLY = N2ONLY + JJ
         KK = KK - II - JJ
         WRITE (MSGTXT,1195) KK, ' flag', NFG1, ' not equal', NFG2
         IF (KK.GT.0) CALL MSGWRT (4)
         NBOTH = NBOTH + KK
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGDIFD ERROR:',I4,' ON ',A)
 1010 FORMAT ('FGDIFD: processing IF/Source/Sub/FQ',I6,I4,I3,I2)
 1175 FORMAT ('Two differ at chan/A1-A2/T/in',I7,2I4,I3,'/',
     *   2(I2.2,':'),F4.1,I3)
 1190 FORMAT ('FGDIFD: read',I10,' records from vers',I3,' to',I12,
     *   ' cells')
 1195 FORMAT ('FGDIFD  differ',I12,' cells by',A,I3,A,I3)
      END
