LOCAL INCLUDE 'UV2TB.INC'
C                                       Local include for UV2TB
C                                       Needs PUVD.INC
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXCALC, XXSTOK
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XOVER,
     *   BADD(10),  BUFFER(512)
      INTEGER   SEQIN, DISKIN, CNOIN, OUTVER, BSTOK, ESTOK, NUMHIS,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, LRECI, NRPRMI,
     *   IACRNO, ACKOLS(4+4*MAXIF), ACNUMV(4+4*MAXIF)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XCALCO*4, XSTOK*4,
     *   HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XXSOUR, XQUAL, XXCALC, XTIME, XXSTOK, XBAND, XFREQ, XFQID,
     *   XBIF, XEIF, XBCHAN, XECHAN, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XOVER, BADD,
     *   SEQIN, DISKIN, CNOIN, OUTVER, BSTOK, ESTOK,
     *   NUMHIS, ILOCWT, CATOLD, INCSI, INCIFI, INCFI, LRECI, NRPRMI,
     *   IACRNO, ACKOLS, ACNUMV,
     *   ISCOMP
      COMMON /CHARPM/ NAMEIN, CLAIN, XSOUR, XCALCO, XSTOK, HISCRD
      COMMON /BUFRS/ BUFFER
LOCAL END
      PROGRAM UV2TB
C-----------------------------------------------------------------------
C! Measures the decorrelation between channels and IF of uv data
C# Task UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2006-2007, 2010, 2015, 2018
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
C   Determines decorrelation between channels and IFs
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      STOKES         BSTOK,ESTOK   Stokes' type(s) to test.
C      BIF            BIF           First IF to test
C      EIF            EIF           Highest IF to test
C      BCHAN          BCHAN         First Channel to test
C      ECHAN          ECHAN         Highest Channel to test
C      OUTVER         OUTVER        Output AC table version number.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UV2TB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'UV2TB '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UV2TIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Process.
      CALL UV2TUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UV2TIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UV2TIN gets input parameters for UV2TB.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      BSTOK   I  First Stokes' parameter to use (number in data base)
C      ESTOK   I  Highest Stokes' parameter to use.
C      BIF     I  First IF to test
C      EIF     I  Highest IF to test
C      BCHAN   I  First channel to test
C      ECHAN   I  Highest channel to test
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in UV2TB for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, I, LUN, NSTOK
      REAL      CATR(256)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      LOGICAL   T, F, ISIQUV, MATCH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UV2TB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /'      '/
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
      NPARM = 168
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXCALC, XCALCO)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      OUTVER = IROUND (XOVER)
C                                       Get CATBLK from input file.
      CNOIN = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Set stokes' parameter
C                                       limits.
      NSTOK = CATBLK(KINAX+JLOCS)
      ISIQUV = CATD(KDCRV+JLOCS) .GT. 0.0D0
      BSTOK = 1
      IF ((XSTOK.EQ.'I   ') .OR . (XSTOK.EQ.'    ') .OR.
     *   (XSTOK.EQ.'HALF')) THEN
         ESTOK = 2
      ELSE
         ESTOK = 1
         END IF
      ESTOK = MIN (ESTOK, NSTOK)
      IF (ISIQUV) ESTOK = 1
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 70 I = 1,30
         SOURCS(I) = XSOUR(I)
 70      CONTINUE
      SELQUA = IROUND (XQUAL)
      SELCOD = XCALCO
      CALL RCOPY (8, XTIME, TIMRNG)
      STOKES = XSTOK
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
         EIF = IROUND (XEIF)
         IF (BIF.GT.EIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MAX (1, MIN (EIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         EIF = 1
         END IF
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOPOL = IROUND (XDOPOL)
      IF ((DOPOL.EQ.0) .AND. (XDOPOL.GT.0.0)) DOPOL = 1
      PDVER = IROUND (XPDVER)
      CALL FILL (50, 0, ANTENS)
      SUBARR = IROUND (XSUBA)
      FGVER = IROUND (XFLAG)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      DOACOR = T
C                                       Only Autocorrelations
      DOXCOR = F
C                                       Spectral smoothing
      CALL RCOPY (3, XSMOTH, SMOOTH)
C                                       Rate error amplitude correction
C                                       assuming 2 s integration
      DXTIME = 2.0 / 86400.0
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       Open and close UVGET to get
C                                       output CATBLK.  Can't just leave
C                                       it open due to status conflicts.
      CALL UVGET ('INIT', BUFFER, BUFFER, JERR)
      IF (JERR.GT.1) GO TO 999
C                                       Close file
      CALL UVGET ('CLOS', BUFFER, BUFFER, JERR)
      IF (JERR.GT.1) GO TO 999
C                                       Init Table
      CALL INTACT (JERR)
      IF (JERR.NE.0) GO TO 999
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV2TIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE INTACT (IRET)
C-----------------------------------------------------------------------
C   Subroutine to initialize the AC table.
C   Output:
C      IRET   I   Return code 0=OK, else failed
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LOOPIF, LOOPS, NUMCHN, DATP(128,2), NKEY, NREC,  NCOL,
     *   NUMKEY, LOC, KEYVAL, KEYTYP, LUN, I, ISOFF
      HOLLERITH HOLTMP(6)
      DOUBLE PRECISION CATD(128)
      CHARACTER TTITLE*56, TITLE(4+4*MAXIF)*24, UNITS(4+4*MAXIF)*8,
     *   KEYWRD*8, POLNAM(8)*2, NUMBER(15)*2
      INCLUDE 'UV2TB.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATD, CATBLK)
      DATA TTITLE /'AIPS AUTOCORRELATION SPECTRUM TABLE          '/
      DATA POLNAM /'RR','LL','RL','LR','I ','Q ','U ','V '/
      DATA NUMBER /'01','02','03','04','05','06','07','08','09',
     *   '10','11','12','13','14','15'/
C-----------------------------------------------------------------------
C                                       Init AC Table
C                                       Col 1 = Time
         TITLE(1) = 'TIME'
         UNITS(1) = 'DAYS'
         DATP(1,2) = 12
C                                       Col 2 = ant
         TITLE(2) = 'ANTENNA NO.'
         UNITS(2) = '  '
         DATP(2,2) = 14
C                                       Col 3 = source number
         TITLE(3) = 'SOURCE NO.'
         UNITS(3) = '  '
         DATP(3,2) = 14
C                                       Fiddle with Stokes' labeling
         IF (CATD(KDCRV+JLOCS).GT.0.0D0) THEN
            ISOFF = CATD(KDCRV+JLOCS) + 0.1
            ISOFF = ISOFF + 3
         ELSE
            ISOFF = -CATD(KDCRV+JLOCS) + 0.1
            ISOFF = ISOFF - 1
            END IF
C                                       Set up spectrum columns
         NCOL = 3
         NUMCHN = ECHAN - BCHAN + 1
         NUMCHN = MIN (NUMCHN, CATBLK(KINAX+JLOCF))
         DO 20 LOOPIF = BIF,EIF
            DO 10 LOOPS = BSTOK,ESTOK
               NCOL = NCOL + 1
               TITLE(NCOL) = POLNAM(LOOPS+ISOFF) // NUMBER(LOOPIF)
               UNITS(NCOL) = 'JY'
               DATP(NCOL,2) = NUMCHN * 10 + 2
 10            CONTINUE
 20         CONTINUE
         NREC = 1000
         NKEY = 10
         LUN = 40
C                                       Create/open file
         CALL TABINI ('WRIT', 'AS', DISKIN, CNOIN, OUTVER, CATOLD,
     *      LUN,  NKEY, NREC, NCOL, DATP, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       File created, initialize
         DO 40 I = 1,NCOL
C                                       Col. labels.
            CALL CHR2H (24, TITLE(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 3, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1021) IRET
               GO TO 990
               END IF
C                                       Units
            CALL CHR2H (8, UNITS(I), 1, HOLTMP)
            CALL TABIO ('WRIT', 4, I, HOLTMP, BUFFER, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1022) IRET
               GO TO 990
               END IF
 40         CONTINUE
C                                       Fill in Table title
         CALL CHR2H (54, TTITLE, 1, BUFFER(101))
C                                       Write keywords
         LOC = 1
         KEYTYP = 4
         NUMKEY = 1
C                                       Number of IFs
         KEYWRD = 'NO_IF'
         KEYVAL = EIF - BIF + 1
         CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER,  LOC, KEYVAL,
     *      KEYTYP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
C                                       Number of channels
         KEYWRD = 'NO_CHAN'
         KEYVAL = NUMCHN
         CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER,  LOC, KEYVAL,
     *      KEYTYP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
C                                       Number of Stokes'
         KEYWRD = 'NO_POL'
         KEYVAL = ESTOK - BSTOK + 1
         KEYVAL = MIN (KEYVAL, CATBLK(KINAX+JLOCS))
         CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER,  LOC, KEYVAL,
     *      KEYTYP, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1040) IRET
               GO TO 990
               END IF
C                                       Get array indices
         DO 60 I = 1,NCOL
            ACKOLS(I) = DATP(I,1)
            ACNUMV(I) = DATP(I,2) / 10
 60         CONTINUE
         GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('TABINI ERROR ',I4,' CREATING/OPENING TABLE')
 1021 FORMAT ('TABIO ERROR ',I4,' WRITING COL. LABELS')
 1022 FORMAT ('TABIO ERROR ',I4,' WRITING COL. UNITS')
 1040 FORMAT ('TABIO ERROR ',I4,' WRITING KEYWORD')
      END
      SUBROUTINE UV2TUV (IRET)
C-----------------------------------------------------------------------
C   UV2TUV sends uv data one point at a time to the table writing
C   routine.
C   Input in common:
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   IA1, IA2, INCX, NUMVIS
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      REAL      DUM, RPARM(50), VIS(3,MAXCIF)
      INCLUDE 'UV2TB.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open input file
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Save input file info (as
C                                       returned by UVGET).
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVGET ('READ', RPARM, VIS, IRET)
C                                       Out of data?
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IF (ILOCB.GE.0) THEN
            IA2 = RPARM(1+ILOCB) + 0.1
            IA1 = IA2 / 256
            IA2 = IA2 - IA1*256
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                      Call table writing routine.
         CALL UV2TBR (NUMVIS, RPARM(1+ILOCT), IA1, VIS, RPARM, IRET)
         IF (IRET.GT.0) THEN
C                                       Error (fatal)
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Read next vis.
         GO TO 100
C                                       Final call to UV2TBR.
 200     NUMVIS = -1
         CALL UV2TBR (NUMVIS, DUM, IA1, VIS, RPARM, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Close file
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV2TUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1100 FORMAT ('UV2TUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UV2TUV: UV2TBR ERROR',I3)
      END
      SUBROUTINE UV2TBR (NUMVIS, T, IA1, VIS, RPARM, IRET)
C-----------------------------------------------------------------------
C   Determine decorrelation of selected data.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      T       R    Time in days since 0 IAT on the reference day.
C      IA1     I    First antenna number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      NRPARM     I       # random parameters.
C      BSTOK      I  First Stokes' parameter to use (number in data
C                    base)
C      ESTOK      I  Highest Stokes' parameter to use.
C      LRECI      I  Input file record length
C      ILOCSU     I  RP pointer for source number.
C      INCSI      I  Input Stokes' increment in vis.
C      INCFI      I  Input frequency increment in vis.
C      INCIFI     I  Input IF increment in vis.
C      IRET       I    Return code   0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IRET
      REAL      T, VIS(3,*), RPARM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  LOOPIF, LOOPF, LOOPS, INDEX, NUMCHN, NUMBIF,
     *   REC(MAXCIF+3), ICOL, IPNT
      REAL      RECR(MAXCIF+3)
      INCLUDE 'UV2TB.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (REC, RECR)
      SAVE NUMCHN, NUMBIF
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LE.0) GO TO 800
C                                       Setup on first call
      IF (NUMVIS.EQ.1) THEN
         NUMCHN = CATBLK(KINAX+JLOCF)
         NUMBIF = 1
         IF (JLOCIF.GT.0) NUMBIF = CATBLK(KINAX+JLOCIF)
         IACRNO = 0
         END IF
C                                       Fill table entry
C                                       Time
      RECR(ACKOLS(1)) = T
C                                       Antenna
      REC(ACKOLS(2)) = IA1
C                                       Source number
      IF (ILOCSU.GE.0) THEN
         REC(ACKOLS(3)) = RPARM(1+ILOCSU) + 0.5
      ELSE
         REC(ACKOLS(3)) = 0
         END IF
C                                       Fill spectra
      ICOL = 3
      DO 120 LOOPIF = BIF,EIF
            DO 110 LOOPS = BSTOK,ESTOK
               ICOL = ICOL + 1
               INDEX = ACKOLS(ICOL) - 1
               DO 100 LOOPF = 1,NUMCHN
                  IPNT = 1 + (LOOPF-1) * INCFI + (LOOPIF-1) * INCIFI +
     *               (LOOPS-1) * INCSI
                  IF (VIS(3,IPNT).GT.0.0) THEN
                     RECR(INDEX+LOOPF) = VIS(1,IPNT)
                  ELSE
                     RECR(INDEX+LOOPF) = FBLANK
                     END IF
 100              CONTINUE
 110           CONTINUE
 120         CONTINUE
C                                       Write entry
      CALL TABIO ('WRIT', 0, IACRNO, REC, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Close table
 800  CALL TABIO ('CLOS', 0, IACRNO, REC, BUFFER, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1121) IRET
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 1120 FORMAT ('TABIO ERROR ',I4,' WRITING TABLE')
 1121 FORMAT ('TABIO ERROR ',I4,' CLOSING TABLE')
      END
