LOCAL INCLUDE 'ACSCL.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC
      REAL      XSIN, XDISIN, XFLAG, TIME(8), SOLINT, XQUAL, XBAND,
     *   XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER,
     *   XDOBND, XBPVER, XSMOTH(3), XCHNS(4,20), BADD(10)
      REAL      BUFF1(256), SOLIN, TIME1, TIME2, GNMOD, STARTD, STOPD,
     *   TACCUM(MAXANT)
      INTEGER SNKOLS(MAXSNC), SNNUMV(MAXSNC), ISNRNO, NUMNOD,
     *   SNLUN, SNBUFF(1024), NPOLZN, NUMBIF, NUMTEL, SNV
      INTEGER   SEQIN, DISKIN, NUMHIS, ILOCWT, CATOLD(256),
     *   INCSI, INCFI, INCIFI, LRECI, NRPRMI, NANT, ANTNO(MAXANT),
     *   NNIF, NNF, NCORI, OLDCNO, CHNSEL(3,20,MAXIF)
      LOGICAL   ISCOMP, ISAPPL, DOSCAN
      CHARACTER NAMEIN*12, CLAIN*6, HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XFLAG, TIME, SOLINT,
     *   XXSOUR, XQUAL, XXCALC, XBAND, XFREQ, XFQID, xsuba, XDOCAL,
     *   XGUSE, XDOPOL, XPDVER, XBLVER, XDOBND, XBPVER, XSMOTH, XCHNS,
     *   BADD
      COMMON /MYPARM/ CATOLD, BUFF1, SEQIN, DISKIN, NUMHIS, ILOCWT,
     *   INCSI, SOLIN, INCFI, INCIFI, LRECI, NRPRMI, ISCOMP, NNIF, NNF,
     *   NCORI, TIME1, TIME2, NANT, NUMTEL, NPOLZN, NUMBIF, STARTD,
     *   STOPD, OLDCNO, SNV, CHNSEL
      COMMON /CHARPM/ NAMEIN, CLAIN, HISCRD
      COMMON /CANIN/ ANTNO, TACCUM
      COMMON /SNPARM/ SNBUFF, SNKOLS, SNNUMV, ISNRNO, NUMNOD,
     *   SNLUN, GNMOD, ISAPPL, DOSCAN
LOCAL END
      PROGRAM ACSCL
C-----------------------------------------------------------------------
C! Correct amplitudes using auto correlation measurements
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2014-2016, 2018
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
C   ACSCL correct amplitudes' errors causing due to sampler errors
C   using auto correlation measurements. The correction is recorded
C   in SN table.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      TIMERANG(8)    TIME          Timerange to be copied
C                                   1-4 = start Day, Hour, Min, Sec.
C                                   5-8 =  end  Day, Hour, Min, Sec.
C      SOLINT         SOLINT        Solution interval in minutes
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, SUB1, SUB2
      LOGICAL   LAST
      INCLUDE 'ACSCL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
C
      DATA PRGM /'ACSCL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL ACCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Calculate corrections and
C                                       record them into SN table
      CALL FNDEXT ('BP', CATOLD, SUB2)
      IF (SUB2.LE.0) THEN
         MSGTXT = 'WARNING: NO BP TABLE FOUND, DOBAND SET FALSE'
         CALL MSGWRT (6)
         DOBAND = -1
         END IF
      SUB1 = 1
      CALL FNDEXT ('AN', CATOLD, SUB2)
      IF ((SUBARR.GT.0) .AND. (SUBARR.LE.SUB2)) THEN
         SUB1 = SUBARR
         SUB2 = SUBARR
         END IF
      DO 10 SUBARR = SUB1,SUB2
         LAST = SUBARR.EQ.SUB2
         CALL ACRCOR (LAST, IRET)
         IF (IRET.NE.0) GO TO 990
 10      CONTINUE
      CALL ACCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE ACCIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ACCIN gets input parameters for ACSCL and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'ACSCL.INC'
      CHARACTER STAT*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, VER, KEYLOC, KEYTYP,
     *   ORIGIN, NUMKEY, NW(MAXIF), K, K1, K2, J, NCHAN
      REAL      DECNOD(25), RANOD(25), TTIME, CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T, F, CHSTAT, MULTI
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 243
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
C                                       SOLINT
      IF (SOLINT.EQ.0.0) SOLINT = -2
C                                       to pay attention on scan
C                                       if SOLINT is negative
      DOSCAN = .FALSE.
      IF (SOLINT.LT.-0.1) THEN
         SOLINT = ABS(SOLINT)
         DOSCAN = .TRUE.
         END IF
C                                       SOLIN is in minutes
      SOLIN = SOLINT
C                                       SOLINT is in days
      SOLINT = SOLINT / (24.0*60.0)
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBARR = IROUND (XSUBA)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, STAT, BUFF1, IERR)
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
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
      CALL COPY (256, CATBLK, CATUV)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOXCOR = .FALSE.
      DOACOR = .TRUE.
      FGVER = IROUND (XFLAG)
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (4, 1, XXCALC, SELCOD)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), SOURCS(I))
 10      CONTINUE
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      BLVER = IROUND (XBLVER)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 35 K = K1,K2
               NW(K) = NW(K) + 1
               DO 30 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 30               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 35            CONTINUE
            END IF
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      NCHAN = CATUV(KINAX+JLOCF)
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (NCHAN+1)/8 + 1
            CHNSEL(2,1,K) = NCHAN - ((NCHAN+1)/8)
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NCHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = NCHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NCHAN))
 45         CONTINUE
 50      CONTINUE
C                                       Determine stop time of data
      CALL UVTIME (DISKIN, OLDCNO, CATBLK, STARTD, STOPD, JERR)
      IF (JERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
      TIME1 = TIME(1)+TIME(2)/24.+TIME(3)/(24.*60.)+TIME(4)/(24.*3600.)
      TIME2 = TIME(5)+TIME(6)/24.+TIME(7)/(24.*60.)+TIME(8)/(24.*3600.)
      IF (TIME1.EQ.0.0) TIME1 = -1.0E6
      IF ((TIME1.GE.TIME2) .OR. (TIME2.EQ.0.0)) TIME2  = 1.0E6
      IF (TIME1.LT.STARTD) TIME1 = STARTD
      IF (TIME2.GT.STOPD) TIME2 = STOPD
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
      NNIF = 1
      NNF = 1
C      IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NNIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NNF = CATBLK(KINAX+JLOCF)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   UTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Prepare antennas data
      VER = 1
      CALL ANTDAT (VER, DISKIN, OLDCNO, IERR)
      IF (IERR .NE. 0) THEN
         JERR = 1
         WRITE (MSGTXT, 1120)
         GO TO 990
         END IF
C                                       Open SN table
C                                       Change status to 'writ'
C                                       Determine status of file
      UTYPE = 'UV'
      CHSTAT = .FALSE.
      CALL CATDIR ('INFO', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *   SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
C                                       Change status
      IF (STAT.EQ.'READ') THEN
         STAT = 'CLRD'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1 , IERR)
         IF (IERR .NE. 0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         STAT = 'WRIT'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR .NE. 0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         CHSTAT = .TRUE.
         END IF
C
      ISAPPL = F
      GNMOD = 1.0
      DO 70 I = 1, 25
         RANOD(I) = 0.0
         DECNOD(I) = 0.0
 70      CONTINUE
      SNLUN = 48
      NUMBIF = NNIF
      NUMTEL = NANT
      NPOLZN = 2
      IF (NCOR.EQ.1) NPOLZN = 1
      NUMNOD = 0
      SNV = 0
      CALL SNINI ('WRIT', SNBUFF, DISKIN, OLDCNO, SNV, CATOLD, SNLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMTEL, NPOLZN, NUMBIF, NUMNOD,
     *   GNMOD, RANOD, DECNOD, ISAPPL, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1010) SNV
         GO TO 990
         END IF
C                                       history file
      NUMHIS = NUMHIS + 1
      TTIME = 0
      DO 80 I = 1, 8
         TTIME = TTIME + TIME(I)
 80      CONTINUE
      IF (TTIME.EQ.0.0) THEN
         WRITE (HISCRD(NUMHIS),2100)
      ELSE
         WRITE (HISCRD(NUMHIS),2200) (TIME(I), I =1,8)
         END IF
      NUMHIS = NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2300) SNV
      NUMHIS = NUMHIS + 1
C                                       averaged by scans or by the
C                                       given SOLINT?
      WRITE (HISCRD(NUMHIS),2500) SOLIN
C
      WRITE (MSGTXT,1060) SNV
      CALL MSGWRT (8)
C                                       Add the ORIGIN keyword
      CALL MULSDB (CATOLD, MULTI)
      KEYLOC = 1
      KEYTYP = 4
      ORIGIN = 0
      NUMKEY = 1
      IF (.NOT.MULTI) ORIGIN = 1
      CALL TABKEY ('WRIT', 'SNORIGIN', NUMKEY, SNBUFF, KEYLOC,
     *   ORIGIN, KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       Check if changed status
      IF (CHSTAT) THEN
         STAT = 'CLWR'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR .NE. 0) THEN
            JERR = 1
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         STAT = 'READ'
         CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN,
     *      SEQIN, UTYPE, NLUSER, STAT, BUFF1, IERR)
         IF (IERR .NE. 0) THEN
            WRITE (MSGTXT,1140) IERR, STAT
            GO TO 990
            END IF
         END IF
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ACCIN: ERROR in initialization of SN table ',I3)
 1015 FORMAT ('ACCIN: ERROR',I3,' READING KEYWORDS FROM SN TABLE')
 1020 FORMAT ('ACCIN: UNABLE TO DETERMINE DATA STOP TIME')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('Writing to SN table ',I3)
 1120 FORMAT ('ACCIN: PROBREM OF READING DATA FROM ANTENNA TABLE')
 1140 FORMAT ('ACCIN: ERROR ',I3,' CHANGING ',A4,' STATUS')
 2100 FORMAT ('TIMERANG = beginning to end')
 2200 FORMAT (F3.0,1X,F3.0,F3.0,F4.1,' - ',F3.0,1X,F3.0,F3.0,F4.1,
     *   '/ Time range')
 2300 FORMAT ('SN table =',I3)
 2500 FORMAT ('SOLINT = ', F5.1, ' minutes')
      END
      SUBROUTINE ACRCOR (LAST, IRET)
C-----------------------------------------------------------------------
C   ACRCOR calculates deviation of auto correlation from unit and record
C   it into SN table.
C   Input:
C      LAST    L  True => close SN table
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      LOGICAL   LAST
      INTEGER   IRET
c
      INTEGER   I, L, IA1, IA2, INCX, JIF, JF, JS, IND, IANT, INDEXI,
     *   SUBA, SOUN, FREQN, MODENO, NVAL
      DOUBLE PRECISION CTIM
      INCLUDE 'ACSCL.INC'
      INTEGER   MAXM
      PARAMETER (MAXM = 2 * MAXANT * MAXIF)
      REAL      BASEN, DMEAN, MEAN(MAXM), TMAX, TMIN, REAL(2,MAXIF),
     *   IMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   MBDELY(2), TIMINT, EPS, SWT(MAXM), WT, WTT, IFRM, RPARM(20),
     *   VIS(3,UVBFSS/3), CHFLGS(MAXCIF), DISP(2), DDISP(2)
      INTEGER   SCNSUB, SCNSOU, IDUM1, IDUM2, FREQID, IERR
      REAL      SCNTIM, SCNDT, SCNEND, CURTIM, SCNDTM
      LOGICAL   DONDX, GOTDAT, ISEOF
      INTEGER   REFA(2,MAXIF), LNXBUF(512), LNXKOL(MAXNXC),
     *   LNXNUM(MAXNXC), LNXRNO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      SAVE GOTDAT
      DATA GOTDAT /.FALSE./
C-----------------------------------------------------------------------

C                                       Dimension of complex axis
      INCX = CATOLD(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Initials for a new SN table
      DO 20 L = 1, 2
         MBDELY(L) = 0.0
         DISP(L) = 0.0
         DDISP(L) = 0.0
         DO 10 I = 1, NNIF
            IMAG(L, I) = 0.0
            REAL(L,I) = 1.0
            DELAY(L, I) = 0.0
            RATE(L, I) = 0.0
            WEIGHT(L, I) = 1.0
            REFA(L, I) = ANTNO(1)
 10         CONTINUE
 20      CONTINUE
C                                       Force the auto correlation
C                                       mean to zero
      DO 50 IANT = 1, NANT
         TACCUM(IANT) = 0
         DO 40 JIF = 1,NNIF
            DO 30 JS = 1,NPOLZN
               IND= JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNIF
               MEAN(IND) = 0.0
               SWT(IND) = 0.0
  30           CONTINUE
  40        CONTINUE
  50     CONTINUE
C                                       ICHANSEL
      CALL CHWANT (NNF, NNIF, CHNSEL, CHFLGS)
C                                       begin of buisness1 with NX table
C                                       Initialize I/O to INDEX file
      MSGSUP = 32000
      IXLUN = 98
      CALL NDXINI ('READ', LNXBUF, DISKIN, OLDCNO, 1, CATOLD, IXLUN,
     *   LNXRNO, LNXKOL, LNXNUM, IERR)
      MSGSUP = 0
      DONDX = IERR.EQ.0
C
      LNXRNO = 1
C                                       Dummy if no scans
      SCNTIM = -1.0E10
      SCNEND =  1.0E10
      EPS = 1.0/(60.0*60.0*24.0)
C                                   Read first scan info
      IF (DONDX) THEN
 60      CALL TABNDX ('READ', LNXBUF, LNXRNO, LNXKOL, LNXNUM, SCNTIM,
     *      SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2, FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (SCNSUB.NE.SUBARR) GO TO 60
         IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0)) GO TO 60
C                                       Add 0.5 sec to each end
         SCNDT = SCNDT + EPS
         SCNTIM = SCNTIM - 0.5 * SCNDT
         SCNEND = SCNTIM + SCNDT
         END IF
C
      TMIN = TIME1
      TMAX = TMIN + SOLINT
      IF (TMAX.GT.TIME2) TMAX = TIME2
C
C   *******        This is where the routine loops back to after
C   *******        finishing a solution interval
C
C                                       end of buisness1 with NX table
C                                       READ loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       got data maybe
      ELSE
C                                       Out of data?
         ISEOF = IRET.LT.0
C                                       Begin Loop in time.
         IF (ISEOF) THEN
            CURTIM = 10000.0
         ELSE
            CURTIM = RPARM(1+ILOCT)
            IF (CURTIM.LT.TIME1) GO TO 100
            END IF
C                                       Determine subarray, source,
C                                       FREQID of the previous scan
         SUBA = SCNSUB
C                                       If averaging through costant
C                                       time (not scan) record the same
C                                       SOUN = 0 and FREQN = 0
         IF (DOSCAN) THEN
            SOUN = SCNSOU
            FREQN = FREQID
         ELSE
            SOUN = 0
            FREQN = 0
            END IF
C                                       next scan?
C                                       pay attention on scans only if
C                                       SOLINT=-1
         IF ((CURTIM.GT.SCNEND) .AND. (DOSCAN)) THEN
            TMAX = SCNEND
C
            IF (DONDX) THEN
C                                       Find next index record
 105           IF (LNXRNO.LE.LNXBUF(5)) THEN
                  CALL TABNDX ('READ', LNXBUF, LNXRNO, LNXKOL, LNXNUM,
     *               SCNTIM, SCNDT, SCNSOU, SCNSUB, IDUM1, IDUM2,
     *               FREQID, IERR)
                  IF (IERR.NE.0) GO TO 999
                  IF (SCNSUB.NE.SUBARR) GO TO 105
                  IF ((FREQID.NE.FRQSEL) .AND. (FRQSEL.GT.0)) GO TO 105
C                                       Add 0.5 sec to each end
                  SCNDT = SCNDT + EPS
                  SCNTIM = SCNTIM - 0.5 * SCNDT
                  SCNEND = SCNTIM + SCNDT
                  IF (CURTIM.GT.SCNEND) GO TO 105
               ELSE
                  SCNEND = CURTIM + EPS
                  END IF
               END IF
         ELSE
            IF (TMAX.EQ.STOPD) THEN
               IF (CURTIM.LE.(TMAX-EPS)) GO TO 200
            ELSE
               IF (CURTIM.LE.TMAX) GO TO 200
               END IF
            END IF
C                                       Do a record into SN table
C
C                                       Length and central of time
C                                       interval for SN table
         CTIM = 0.5D0 * (TMIN + TMAX)
         TIMINT = TMAX - TMIN
C
         IFRM = 0.0
         MODENO = 0
         DO 130 IANT = 1, NANT
            DO 120 JIF = 1,NNIF
               DO 110 JS = 1,NPOLZN
                  IND = JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNIF
                  IF (SWT(IND).GT.0.0) THEN
                     MEAN(IND) = MEAN(IND) / SWT(IND)
                  ELSE
                     MEAN(IND) = 0.0
                     END IF
                  IF (MEAN(IND).GT.0.0) THEN
                     REAL(JS,JIF) = 1.0 / SQRT(MEAN(IND))
                     IMAG(JS,JIF) = 0.0
                     GOTDAT = .TRUE.
                  ELSE
                     REAL(JS,JIF) = FBLANK
                     IMAG(JS,JIF) = FBLANK
                     END IF
C                 WRITE (MSGTXT,1900) IANT, JIF, MEAN(IND), SWT(IND)
C                 CALL MSGWRT (2)
 110              CONTINUE
 120           CONTINUE
C                                       record only if the accumulated
C                                       time for the given antenna is
C                                       big enough
C
C                                       SCNDTM scan length in minutes
            SCNDTM = 1000
            IF (DOSCAN) SCNDTM = SCNDT*24*60
            IF ((TACCUM(IANT).GT.0.1*MIN(SOLIN,SCNDTM))
     *         .OR. (SOLIN.GT.60)) THEN
C                                       write SN table
               CALL TABSN ('WRIT', SNBUFF, ISNRNO, SNKOLS, SNNUMV,
     *            NPOLZN, CTIM, TIMINT, SOUN, ANTNO(IANT), SUBA,
     *            FREQN, IFRM, MODENO, MBDELY, DISP, DDISP, REAL, IMAG,
     *            DELAY, RATE, WEIGHT, REFA, IRET)
               END IF
 130        CONTINUE
C                                       quit if done with data
         IF (ISEOF) GO TO 500
C                                       Force the auto mean to
C                                       zero for next SN record
         DO 180 IANT = 1, NANT
            TACCUM(IANT) = 0
            DO 160 JIF = 1,NNIF
               DO 140 JS = 1,NPOLZN
                  IND = JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNIF
                  MEAN(IND) = 0.0
                  SWT(IND) = 0.0
 140              CONTINUE
 160           CONTINUE
 180        CONTINUE
C                                       TMIN, TMAX for next SN row
 190     TMIN = CURTIM
         TMAX = TMIN + SOLINT
         IF (TMAX.GT.TIME2) TMAX = TIME2
         IF (TMAX.EQ.STOPD) THEN
            IF (RPARM(1+ILOCT).GT.(TIME2-EPS) ) GO TO 500
         ELSE
            IF (RPARM(1+ILOCT).GT.TIME2 ) GO TO 500
            END IF
C
         IF (RPARM(1+ILOCT).GT.TMAX ) GO TO 190
C                                       accumulate auto correlation
C                                       data
 200     IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       take only auto correlation
         IF (IA1.NE.IA2) GO TO 100
         DO 220 IANT = 1, NANT
            IF (IA1.EQ.ANTNO(IANT)) GO TO 230
 220        CONTINUE
         GO TO 100
C                                       accumulated time for the given
C                                       antenna, in minutes
 230     IF (ILOCIT.GE.0) THEN
            TACCUM(IANT) = TACCUM(IANT) + RPARM(1+ILOCIT)/60
         ELSE
            TACCUM(IANT) = TACCUM(IANT) + 0.166
            END IF
         DO 260 JS = 1,NPOLZN
            I = 0
            DO 250 JIF = 1,NNIF
               IND = JS + (JIF-1)*NPOLZN + (IANT-1)*NPOLZN*NNIF
               DMEAN = 0.0
               WT = 0.0
               NVAL = 0
               DO 240 JF = 1,NNF
                  I = I + 1
                  INDEXI = (JIF-1) * INCIFI + (JF-1) *
     *               INCFI + (JS-1) * INCSI + 1
                  WTT = VIS(3,INDEXI) * CHFLGS(I)
                  IF (WTT.GT.0.0) THEN
                     DMEAN = DMEAN + VIS(1,INDEXI)
                     WT = WT + WTT
                     NVAL = NVAL + 1
                     END IF
 240              CONTINUE
               IF (NVAL.GT.0) THEN
                  DMEAN = DMEAN / NVAL
                  WT = WT / NVAL
C                                       accumulate MEAN with weights
                  SWT(IND) = SWT(IND) + WT
                  MEAN(IND) = MEAN(IND) + DMEAN *WT
                  END IF
 250           CONTINUE
 260        CONTINUE
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Close files
 500  CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       Close SN table
      IF (LAST) CALL TABIO ('CLOS', 0, ISNRNO, SNBUFF, SNBUFF, IRET)
C                                       Close index file
      IF (DONDX) CALL TABIO ('CLOS', 0, LNXRNO, LNXBUF, LNXBUF, IRET)
      IF ((GOTDAT) .OR. (.NOT.LAST)) THEN
         IRET = 0
         GO TO 999
      ELSE
         MSGTXT = 'ACRCOR: NO VALID AC DATA FOUND'
         IRET = 10
         END IF
C                                       Error
 990  CALL MSGWRT (8)
C                                       delete SN table on error
      IF (IRET.GT.0) THEN
         CALL DELEXT ('SN', DISKIN, OLDCNO, 'READ', CATOLD, SNBUFF, SNV,
     *      I)
         WRITE (MSGTXT,1990) SNV
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACRCOR: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1100 FORMAT ('ACRCOR: ERROR',I3,' READING VIS FILE')
C1900 FORMAT ('ANT=', I3, '  IF=', I3, ' MEAN =',F7.4, ' WEIGHT',F7.4)
 1990 FORMAT ('ACRCOR: DELETED SN VERSION',I5,' DUE TO ERROR')
      END
      SUBROUTINE ANTDAT (VER, DISKI, CNOIN, IERR)
C-----------------------------------------------------------------------
C   Selects station information
C   Inputs:
C      VER           I    Antenna array number (AN file ver.)
C      DISKI         I    Vol number
C      CNOIN         I    CNO
C   Outputs in common:
C      NANT          I    Number of antennas
C      ANTNO(*)      I    Array of antennas' numbers
C-----------------------------------------------------------------------
      INTEGER   IERR, IA, LUNA, CNOIN, IABUF(512), VER,  DISKI
      REAL      UT1XXX, IATUTC
      CHARACTER STNAME*8
      INCLUDE 'ACSCL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                     open antenna file
      LUNA = 28
      CALL ANTINI ('READ', IABUF, DISKI, CNOIN, VER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1XXX, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR .NE. 0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                     Get antenna info.
      NANT = 0
      DO 30 IA = 1,IABUF(5)
         IANRNO = IA
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, STNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IA, IERR
            CALL MSGWRT (8)
            END IF
         ANTNO(IA) = NOSTA
         NANT = MAX (NANT, NOSTA)
 30      CONTINUE
C                                     close antenna file
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANTDAT: ERROR IN OPEN AN-FILE IERR = ',I6)
 1010 FORMAT ('ANTDAT: ERROR IN FINDING STATION',I3,' IERR=',I3)
 1030 FORMAT ('ANTDAT: ERROR IN CLOSING AN-FILE IERR = ',I6)
      END
      SUBROUTINE ACCHIS
C-----------------------------------------------------------------------
C   ACCHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12,  HILINE*72, LABEL*8
      INTEGER   LUN, IERR, TIM(3), DATE(3), I
      LOGICAL   T
      INCLUDE 'ACSCL.INC'
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
      CALL HIOPEN (LUN, DISKIN, FCNO(NCFILE), BUFF1, IERR)
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
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1020) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ACCHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6, 'RELEASE =''',A7,' ''  /********* Start ',
     *   A12, 2X, A8)
 1020 FORMAT (A6)
      END
      SUBROUTINE CHWANT (NCH, NIF, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NCH      I            Number spectral chans
C      NIF      I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   R(*,*)       1.0 => use, 0.0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, CHNSEL(3,20,*)
      REAL      CHFLGS(NCH,NIF)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NCH * NIF
      CALL RFILL (J, 0.0, CHFLGS)
      DO 30 K = 1,NIF
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1.0
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
