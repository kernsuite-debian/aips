LOCAL INCLUDE 'CLSMO.INC'
C                                                          Include CLSMO
C                                       Local include for CLSMO
C                                       Needs parameter from PUVD.INC
C                                       Inputs and general info
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, NUMHIS, CLVER, NSOUWD,
     *   SOUWAN(30), NANTSL, ANTENS(50), BIF, EIF, ISTOK, FREQID, INVER
      LOGICAL   DOSWNT, DOAWNT
      REAL      XSIN, XDISIN, DOBTWN, XFQID, XBAND, XFREQ, XBIF, XEIF,
     *   XTIME(8), XANT(50), XSUBA, XIPARM(10), CUTOFF, XDOBLK, XNORM,
     *   XGVER, XBAD(10), SELBAN, TSTART, TEND
      CHARACTER  HISCRD(30)*64, NAMEIN*12, CLAIN*6, XSOUR(30)*16,
     *   XSTOK*4, XINTP*4, XSMO*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XXINTP,
     *   XXSMO
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ
C                                       Buffers and file info
      INTEGER   BUFFER(512)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, DOBTWN,
     *   XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT,  XSUBA,
     *   XXINTP, XIPARM, CUTOFF, XDOBLK, XXSMO, XNORM, XGVER, XBAD
     *
      COMMON /CINFO/ FRQOFF, SELFRQ, SELBAN, TSTART, TEND, SEQIN,
     *   DISKIN, CNOIN, SUBA, CLVER, DOSWNT, DOAWNT, NSOUWD, SOUWAN,
     *   NANTSL, ANTENS, BIF, EIF, ISTOK, FREQID, NUMHIS, INVER
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XSTOK, XINTP, XSMO
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER
C                                                          End CLSMO
LOCAL END
      PROGRAM CLSMO
C-----------------------------------------------------------------------
C! Smooths a CL table
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2003-2004, 2010-2012
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
C   Task CLSMO smooths a CL tables.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CLSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'CLSMO '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL CLSMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Apply corrections
      CALL CLSUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL CLSMHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
 999  STOP
      END
      SUBROUTINE CLSMIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   CLSMIN gets input parameters for CLSMO.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                1 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, DESEL, MATCH
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN, LUN2,
     *   IIVER, ICLRNO, NUMANT, NUMPOL, NUMIF, IRET, NTERM, BUFF2(512)
      REAL      GMMOD
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DUMMY(MAXIF), CLKOLS(MAXCLC), CLNUMV(MAXCLC)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'CLSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN, LUN2 /28,29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 218
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
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      INVER = IROUND (XGVER)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XXINTP, XINTP)
      CALL H2CHR (4, 1, XXSMO, XSMO)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C                                       Read CATBLK, mark "WRIT"
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Register in DFIL.INC
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Check that CL table present
      CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, INVER, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, DUMMY, BUFFER, IERR)
      IF ((IRET.NE.0) .OR. (IERR.NE.0)) GO TO 999
C                                       Create new CL table
      CLVER = 0
      CALL TABCOP ('CL', INVER, CLVER, LUN, LUN2, DISKIN, DISKIN, CNOIN,
     *   CNOIN, CATBLK, BUFFER, BUFF2, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'TROUBLE COPYING CL TABLE TO NEW VERSION'
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FREQID, JERR)
      IF (.NOT.MATCH) THEN
C                                       No match
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       IF range
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATBLK(KINAX+JLOCIF)
      IF (EIF.LE.0) EIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF)))
     *   BIF = CATBLK(KINAX+JLOCIF)
      IF ((JLOCIF.GT.0) .AND. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *   EIF = CATBLK(KINAX+JLOCIF)
C                                       Stokes' type.
      ISTOK = 0
      IF (XSTOK.EQ.'R ') ISTOK = 1
      IF (XSTOK.EQ.'L ') ISTOK = 2
C                                       If none selected take what you
C                                       have.
      IF (ISTOK.EQ.0) THEN
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +1.0D0) .LE. 0.5D0)) ISTOK = 1
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +2.0D0) .LE. 0.5D0)) ISTOK = 2
C                                       Is selected Stokes' available?
      ELSE
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *      (ABS (CATD(KDCRV+JLOCS)+ISTOK).GT.0.5D0)) THEN
            JERR = 1
            MSGTXT = 'STOKES ' // XSTOK // ' UNAVAILABLE IN DATA'
            GO TO 990
            END IF
         END IF
C                                       If Only LCP available set ISTOK
C                                       to 1
      IF (ABS (CATD(KDCRV+JLOCS)+2.0D0).LE.0.1) ISTOK = 1
      JERR = 0
C                                       Antenna list
      ALLANT = T
      NANTSL = 0
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
C                                       Not all selected - make list
C                                       ANTENNAS array.
      IF (.NOT.ALLANT) THEN
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.GT.0) THEN
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.GE.1) THEN
                  DO 130 J = 1,LIMIT
                     IF (IARG.EQ.ANTENS(J)) GO TO 150
 130                 CONTINUE
                  END IF
C                                       New antenna
               ANTENS(NEXT) = IARG
               NEXT = NEXT + 1
               END IF
 150        CONTINUE
         END IF
      DOAWNT = .NOT. DESEL
      NANTSL = NEXT - 1
C                                       Get source numbers
      CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFER, NSOUWD, DOSWNT,
     *   SOUWAN, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Timerange
      TSTART = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TEND.LT.TSTART) .OR. (TEND.LT.1.0E-5)) TEND = 1.0E20
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLSMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
      END
      SUBROUTINE CLSUV (IERR)
C-----------------------------------------------------------------------
C   Smooths selected portions of CL tables.
C   Leaves the output table sorted in time-antenna order.
C   Inputs from common:
C      CLVER        I    Cal (CL) file version number.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C   Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXTIME
C                                       MXTIME = dim work arrays
      PARAMETER (MXTIME = 25000)
      CHARACTER COLHED(2)*24, SMOOS(5)*4
      INTEGER   IRET, KEY(2,2), ICLUN, CLANT, CLTIM, ICLRNO, NKEY,
     *   KOLS(2), MAXTIM, CLNUMV(MAXCLC), CLKOLS(MAXCLC), NUMANT,
     *   NUMPOL, NUMIF, NTERM, SMOTYP, NSMOOS, I, KEYSUB(2,2),
     *   WRKSRC(MXTIME)
      LOGICAL   T
      REAL      FKEY(2,2), WRKTIM(MXTIME), WORK1(MXTIME), WORK2(MXTIME),
     *   WORK3(MXTIME),  WORK4(MXTIME), WORK5(MXTIME), SMOTIM(3,4),
     *   GMMOD
      DOUBLE PRECISION TIMRA(2)
      INCLUDE 'CLSMO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C                                       Align WRK* in memory
      COMMON /XXYYZZ/ WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5, WRKSRC
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA T /.TRUE./
      DATA ICLUN /30/
      DATA NSMOOS /5/
      DATA SMOOS /'AMPL','PHAS','BOTH','FULL','VLBI'/
C-----------------------------------------------------------------------
C                                       MAXTIM = size of work arrays;
C                                       determined by EQUIVALENCE above
      MAXTIM = MXTIME
      IERR = 0
      SMOTIM(1,1) = XIPARM(1) / 24.0
      SMOTIM(1,2) = XIPARM(2) / 24.0
      SMOTIM(1,3) = XIPARM(3) / 24.0
      SMOTIM(1,4) = XIPARM(4) / 24.0
      SMOTIM(2,1) = XIPARM(6) / 24.0
      SMOTIM(2,2) = XIPARM(7) / 24.0
      SMOTIM(2,3) = XIPARM(8) / 24.0
      SMOTIM(2,4) = XIPARM(9) / 24.0
      SMOTIM(3,1) = CUTOFF
      SMOTIM(3,2) = CUTOFF
      SMOTIM(3,3) = CUTOFF
      SMOTIM(3,4) = CUTOFF
      TIMRA(1) = TSTART
      TIMRA(2) = TEND
C                                       Type of data to smooth
      SMOTYP = 1
      DO 10 I = 1,NSMOOS
         IF (XSMO.EQ.SMOOS(I)) SMOTYP = I
 10      CONTINUE
C                                       Sort CL table to antenna-time.
C                                       Need col. pointers, sort order.
C                                       Reformat?
      CALL CLREFM (DISKIN, CNOIN, CLVER, CATBLK, ICLUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CALINI ('READ', BUFFER, DISKIN, CNOIN, CLVER, CATBLK, ICLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
C                                       Reopen write
      CALL CALINI ('WRIT', BUFFER, DISKIN, CNOIN, CLVER, CATBLK, ICLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Get column pointers
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) GO TO 995
      IRET = 0
      CLTIM = KOLS(1)
      CLANT = KOLS(2)
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
      KEY(1,1) = CLANT
      KEY(1,2) = CLTIM
C                                       Sort to antenna time order.
      IF (((BUFFER(43).NE.CLANT) .OR. (BUFFER(44).NE.CLTIM)))
     *   CALL TABSRT (DISKIN, CNOIN, 'CL', CLVER, CLVER, KEY, KEYSUB,
     *   FKEY, BUFFER, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Smooth.
      IERR = 0
      CALL CLSMTH (DISKIN, CNOIN, CLVER, CATBLK, BUFFER, XINTP, SMOTIM,
     *   SMOTYP, TIMRA, SUBA, DOSWNT, NSOUWD, SOUWAN, DOAWNT, NANTSL,
     *   ANTENS, BIF, EIF, ISTOK, FREQID, XDOBLK, DOBTWN, XNORM, MAXTIM,
     *   WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5, WRKSRC, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  IF ((IERR.EQ.0) .AND. (IRET.NE.0)) IERR = IRET
C
 999  RETURN
      END
      SUBROUTINE CLSMTH (DISK, CNO, VER, CATBLK, BUFFER, SMMETH, SMOTIM,
     *   SMOTYP, TIMRA, SUBA, DOSWNT, NSOUWD, SOUWAN, DOAWNT, NANTSL,
     *   ANTENS, BIF, EIF, ISTOK, FREQID, XDOBLK, DOBTWN, XNORM, MAXTIM,
     *   WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5, WRKSRC, IRET)
C-----------------------------------------------------------------------
C   Subroutine to smooth and/or copy an antenna-time sorted CL table
C   to a specified output file associated with the same uv data set.
C   Several work arrays are passed which are used for storing and
C   smoothing data.
C   Input:
C      DISK     I      Disk to use.
C      CNO      I      Catalog slot number
C      VER      I      CL file version must be antenna-time sorted for
C                         both input and output
C      CATBLK   I(256) Catalog header block.
C      SMMETH   C*4    Smoothing method.
C      SMOTIM   R(3,4) Function times (support, FWHM, cutoff) (days)
C                         1=ampl, 2=phase 3=rate 4 = delay
C      SMOTYP   I      Smoothing type
C                         1 = amplitude
C                         2 = phase
C                         3 = amp and phase
C                         4 = amp, phase, delay, rate
C                         5 = delay, rate
C      TIMRA    D(2)   First and last times to be considered. (days)
C      SUBA     I      Subarray number, .le. 0 => all
C      DOSWNT   L      If true sources in SOUWAN are selected else
C                      deselected.
C      NSOUWD   I      Number of entries in SOUWAN, 0=all selected
C      SOUWAN   I(*)   Source list
C      DOAWNT   L      If true antennas in ANTENS selected, else
C                      deselected.
C      NANTSL   I      Number of entries in ANTENS, 0=all selected.
C      ANTENS   I(*)   Antenna list
C      BIF      I      Start IF
C      EIF      I      Highest IF
C      ISTOK    I      Stokes wanted, 0=both, 1=1st, 2=2nd.
C      FREQID   I      Frequency group ID. 0 => all.
C      XDOBLK   R      What values are changed: <= 0 change good values,
C                         >= 0 change blanked values
C      MAXTIM   I      Maximum number of times (dim of WRKTIM etc)
C   Input/Output:
C      BUFFER   I(*)   Buffer for TABIO use.
C      WRKTIM   R(*)   Large work array.
C      WORK1    R(*)   Large work array, same size as WRKTIM
C      WORK2    R(*)   Large work array, same size as WRKTIM
C      WORK3    R(*)   Large work array, same size as WRKTIM
C      WORK4    R(*)   Large work array, same size as WRKTIM
C      WORK5    R(*)   Large work array, same size as WRKTIM
C   Output:
C      IRET     I    Return code 0=OK, else failed.
C   Note: uses LUNs 28, 29 and 30
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, VER, CATBLK(256), BUFFER(*), SMOTYP, SUBA,
     *   NSOUWD, SOUWAN(*),  NANTSL, ANTENS(*), BIF, EIF, ISTOK, FREQID,
     *   MAXTIM, WRKSRC(*), IRET
      LOGICAL   DOSWNT, DOAWNT
      REAL      SMOTIM(3,4), XNORM, WORK1(*), WORK2(*), WORK3(*),
     *   WORK4(*), WORK5(*), WRKTIM(*), XDOBLK, DOBTWN
      DOUBLE PRECISION TIMRA(2)
      CHARACTER SMMETH*4
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NHCOL
C                                       NHCOL = no. column labels known.
      PARAMETER (NHCOL = 17)
      CHARACTER KEYWRD*8, COLHED(NHCOL)*24
      INTEGER   IRCODE, RECORD(1024), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   LUN, NUMANT, NUMPOL, NUMIF, DATP(128,2), ANT, IERR, SUB,
     *   NUMSUB, I, NUMTIM, THSOU, KOLS(NHCOL), NKEY, NREC, NCOL,
     *   TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL,
     *   RE1KOL, IM1KOL, DL1KOL, RA1KOL, TS1KOL, WT1KOL,
     *   RE2KOL, IM2KOL, DL2KOL, RA2KOL, TS2KOL, WT2KOL,
     *   WTKOL, REKOL, IMKOL, DELKOL, RATKOL, TSKOL
      LOGICAL   BAD, MAT
      INTEGER   NUMREC, LOOPS, LOOPIF, LOOPA, ICLRNO, LOOPR, FSTREC,
     *   SAVE, GMCNT, NLEFT, J, ITIME, NRECDO, CNTDO, NTERM
      REAL      RECR(1024), AMPL, FAC, GMMOD, PHSCHK, TWOPI, LASTPH,
     *   PHASE
      DOUBLE PRECISION RECD(512), TIMOFF, GMSUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), SOUKOL),
     *   (KOLS(3), ANTKOL), (KOLS(4), SUBKOL),  (KOLS(5), FRQKOL),
     *   (KOLS(6), RE1KOL), (KOLS(7),IM1KOL),
     *   (KOLS(8), RA1KOL), (KOLS(9), DL1KOL),
     *   (KOLS(10), TS1KOL), (KOLS(11), WT1KOL),
     *   (KOLS(12), RE2KOL), (KOLS(13),IM2KOL),
     *   (KOLS(14), RA2KOL), (KOLS(15), DL2KOL),
     *   (KOLS(16), TS2KOL), (KOLS(17), WT2KOL)
      DATA KEYWRD /'MGMOD   '/
      DATA COLHED /'TIME                    ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'TSYS 1                  ', 'WEIGHT 1                ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'TSYS 2                  ', 'WEIGHT 2                '/
      DATA LUN /28/
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      TWOPI = 8.0 * ATAN (1.0)
      PHSCHK = 4.0 * ATAN (1.0)
      GMCNT = 0
      GMSUM = 0.0D0
      CNTDO = 0
C                                       Open CL table
C                                       Dummy open to get keywords
      CALL CALINI ('READ', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close
      CALL TABIO ('CLOS', IRCODE, LOOPR, RECORD, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Dummy open to get pointers
      NKEY = 0
      NREC = 0
      NCOL = 0
      CALL TABINI ('READ', 'CL', DISK, CNO, VER, CATBLK, LUN,
     *   NKEY, NREC, NCOL, DATP, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close
      CALL TABIO ('CLOS', IRCODE, LOOPR, RECORD, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Reopen WRITE
      CALL CALINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, ICLRNO,
     *   CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get column pointers
      CALL FNDCOL (NHCOL, COLHED, 24, .TRUE., BUFFER, KOLS, IRET)
C                                       Convert from logical to physical
C                                       pointers.
      DO 10 I = 1,NHCOL
         IF (KOLS(I).GT.0) KOLS(I) = DATP(KOLS(I),1)
 10      CONTINUE
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
C                                       Inform user of smoothing:
      WRITE (MSGTXT,1010)
      CALL MSGWRT (3)
C                                       Loop over subarrays
      DO 750 SUB = 1,NUMSUB
C                                       Want this subarray?
         IF ((SUBA.GT.0) .AND. (SUB.NE.SUBA)) GO TO 750
C                                       Set pointers
         WTKOL = WT1KOL
         REKOL = RE1KOL
         IMKOL = IM1KOL
         DELKOL = DL1KOL
         RATKOL = RA1KOL
         TSKOL = TS1KOL
C                                       Loop over Stokes
         DO 700 LOOPS = 1,NUMPOL
C                                       Want this Stokes?
            IF (((ISTOK.EQ.1).AND.(LOOPS.NE.1)) .OR.
     *         ((ISTOK.EQ.2).AND.(LOOPS.NE.2))) GO TO 660
C                                       Loop over IF
            DO 650 LOOPIF = 1,NUMIF
C                                       Want this IF
               IF ((LOOPIF.LT.BIF) .OR. (LOOPIF.GT.EIF)) GO TO 640
               FSTREC = 0
C                                       Loop over antenna
               DO 600 LOOPA = 1,NUMANT
                  ANT = LOOPA
C                                       Want this antenna?
                  IF (NANTSL.GT.0) THEN
                     MAT = .FALSE.
                     DO 25 J = 1,NANTSL
                        MAT = MAT .OR. (ANT.EQ.ANTENS(J))
 25                     CONTINUE
C                                       Check for no match, selected.
                     IF (DOAWNT .AND. .NOT.MAT) GO TO 600
C                                       Check for match excluded
                     IF (.NOT.DOAWNT .AND. MAT) GO TO 600
                     END IF
C                                       Set pointers, counters
      NUMTIM = 0
      NLEFT = NUMREC - FSTREC
      NRECDO = 0
      LASTPH = FBLANK
C                                       Loop in time, reading
      DO 100 LOOPR = 1,NLEFT
         ICLRNO = FSTREC + LOOPR
         CALL TABIO ('READ', IRCODE, ICLRNO, RECORD, BUFFER, IRET)
         NRECDO = NRECDO + 1
         IF (IRET.LT.0) GO TO 100
         IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
         IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       Want this FQ id?
         IF ((RECORD(FRQKOL).NE.FREQID) .AND. (RECORD(FRQKOL).GT.0)
     *      .AND. (FREQID.GT.0)) GO TO 100
C                                       Want this source?
C                                       Check list
         THSOU = RECORD(SOUKOL)
         IF (NSOUWD.GT.0) THEN
            DO 30 J = 1,NSOUWD
               IF ((THSOU.EQ.SOUWAN(J)) .AND. DOSWNT) GO TO 35
               IF ((THSOU.EQ.SOUWAN(J)) .AND. (.NOT.DOSWNT)) GO TO 100
 30            CONTINUE
            IF (DOSWNT) GO TO 100
            END IF
C                                       Check subarray
 35      IF ((RECORD(SUBKOL).NE.SUB) .AND. (RECORD(SUBKOL).GT.0))
     *      GO TO 100
C                                       Not all antennas wanted
         IF (RECORD(ANTKOL).LT.ANT) GO TO 100
C                                       Check time
         IF ((RECD(TIMKOL).LT.TIMRA(1)) .OR. (RECD(TIMKOL).GT.TIMRA(2)))
     *      GO TO 100
C                                       See if bad (all weights OK.)
         BAD = (RECR(REKOL).EQ.FBLANK). OR.
     *      ((ABS (RECR(REKOL)).LT.1.0E-20) .AND.
     *      (ABS(RECR(IMKOL)).LT.1.0E-20))
         IF (NUMTIM.GE.MAXTIM) GO TO 100
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
            IF (DOBTWN.LE.0.0) THEN
               WRKSRC(NUMTIM) = THSOU
            ELSE
               WRKSRC(NUMTIM) = -1
               END IF
            IF (BAD) THEN
               WORK2(NUMTIM) = FBLANK
               WORK3(NUMTIM) = FBLANK
               WORK4(NUMTIM) = FBLANK
               WORK5(NUMTIM) = FBLANK
C                                       Select by type
C                                       Amplitude
            ELSE IF (SMOTYP.EQ.1) THEN
               WORK2(NUMTIM) = SQRT
     *            (RECR(REKOL)*RECR(REKOL) + RECR(IMKOL)*RECR(IMKOL))
C                                       Need phase for interpolating
C                                       blanked values.
               PHASE = ATAN2 (RECR(IMKOL), RECR(REKOL)+1.0E-20)
C                                       Make phase continious
               IF ((LASTPH.NE.FBLANK) .AND.
     *            ( ABS (PHASE-LASTPH).GT.PHSCHK))
     *            PHASE = PHASE - TWOPI * NINT ((PHASE-LASTPH) / TWOPI)
               LASTPH = PHASE
               WORK3(NUMTIM) = PHASE
C                                       Phase
            ELSE IF (SMOTYP.EQ.2) THEN
               PHASE = ATAN2 (RECR(IMKOL), RECR(REKOL)+1.0E-20)
C                                       Make phase continious
               IF ((LASTPH.NE.FBLANK) .AND.
     *            ( ABS (PHASE-LASTPH).GT.PHSCHK))
     *            PHASE = PHASE - TWOPI * NINT ((PHASE-LASTPH) / TWOPI)
               LASTPH = PHASE
               WORK2(NUMTIM) = PHASE
C                                       Need amplitudes for
C                                       interpolating blanked values.
               WORK3(NUMTIM) = SQRT
     *            (RECR(REKOL)*RECR(REKOL) + RECR(IMKOL)*RECR(IMKOL))
C                                       Amplitude and phase
            ELSE IF (SMOTYP.EQ.3) THEN
               WORK2(NUMTIM) = SQRT
     *            (RECR(REKOL)*RECR(REKOL) + RECR(IMKOL)*RECR(IMKOL))
               PHASE = ATAN2 (RECR(IMKOL), RECR(REKOL)+1.0E-20)
C                                       Make phase continious
               IF ((LASTPH.NE.FBLANK) .AND.
     *            ( ABS (PHASE-LASTPH).GT.PHSCHK))
     *            PHASE = PHASE - TWOPI * NINT ((PHASE-LASTPH) / TWOPI)
               LASTPH = PHASE
               WORK3(NUMTIM) = PHASE
C                                       Amp, phase, delay, rate
            ELSE IF (SMOTYP.EQ.4) THEN
               WORK2(NUMTIM) = SQRT
     *            (RECR(REKOL)*RECR(REKOL) + RECR(IMKOL)*RECR(IMKOL))
               PHASE = ATAN2 (RECR(IMKOL), RECR(REKOL)+1.0E-20)
C                                       Make phase continuous
               IF ((LASTPH.NE.FBLANK) .AND.
     *            ( ABS (PHASE-LASTPH).GT.PHSCHK))
     *            PHASE = PHASE - TWOPI * NINT ((PHASE-LASTPH) / TWOPI)
               LASTPH = PHASE
               WORK3(NUMTIM) = PHASE
               WORK4(NUMTIM) = RECR(RATKOL)
               WORK5(NUMTIM) = RECR(DELKOL)
C                                       Delay, rate
            ELSE IF (SMOTYP.EQ.5) THEN
               WORK2(NUMTIM) = RECR(RATKOL)
               WORK3(NUMTIM) = RECR(DELKOL)
               END IF
 100     CONTINUE
 110  SAVE = ICLRNO - 1
      IF (NUMTIM.LE.0) GO TO 590
C                                       Smooth as requested
C                                       Amplitude
      IF (SMOTYP.EQ.1) THEN
         CALL CLSMSM (SMMETH, SMOTIM(1,1), WRKTIM, WORK2, WRKSRC,
     *      FBLANK, NUMTIM, WORK1)
         CALL CLSMSM (SMMETH, SMOTIM(1,2), WRKTIM, WORK3, WRKSRC,
     *      FBLANK, NUMTIM, WORK2)
      ELSE IF (SMOTYP.EQ.2) THEN
C                                       Phase
         CALL CLSMSM (SMMETH, SMOTIM(1,2), WRKTIM, WORK2, WRKSRC,
     *      FBLANK, NUMTIM, WORK1)
         CALL CLSMSM (SMMETH, SMOTIM(1,1), WRKTIM, WORK3, WRKSRC,
     *      FBLANK, NUMTIM, WORK2)
C                                       Amplitude and phase
      ELSE IF (SMOTYP.EQ.3) THEN
         CALL CLSMSM (SMMETH, SMOTIM(1,1), WRKTIM, WORK2, WRKSRC,
     *      FBLANK, NUMTIM, WORK1)
         CALL CLSMSM (SMMETH, SMOTIM(1,2), WRKTIM, WORK3, WRKSRC,
     *      FBLANK, NUMTIM, WORK2)
C                                       All
      ELSE IF (SMOTYP.EQ.4) THEN
         CALL CLSMSM (SMMETH, SMOTIM(1,1), WRKTIM, WORK2, WRKSRC,
     *      FBLANK, NUMTIM, WORK1)
         CALL CLSMSM (SMMETH, SMOTIM(1,2), WRKTIM, WORK3, WRKSRC,
     *      FBLANK, NUMTIM, WORK2)
         CALL CLSMSM (SMMETH, SMOTIM(1,3), WRKTIM, WORK4, WRKSRC,
     *      FBLANK, NUMTIM, WORK3)
         CALL CLSMSM (SMMETH, SMOTIM(1,4), WRKTIM, WORK5, WRKSRC,
     *      FBLANK, NUMTIM, WORK4)
C                                       rate, delay
      ELSE IF (SMOTYP.EQ.5) THEN
         CALL CLSMSM (SMMETH, SMOTIM(1,3), WRKTIM, WORK2, WRKSRC,
     *      FBLANK, NUMTIM, WORK1)
         CALL CLSMSM (SMMETH, SMOTIM(1,4), WRKTIM, WORK3, WRKSRC,
     *      FBLANK, NUMTIM, WORK2)
         END IF
C                                       Replace with smoothed values
      ITIME = 1
      DO 200 LOOPR = 1,NRECDO
         IF (ITIME.GT.NUMTIM) GO TO 200
         ICLRNO = FSTREC + LOOPR
         CALL TABIO ('READ', IRCODE, ICLRNO, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) GO TO 900
C                                       Not all antennas wanted
         IF (RECORD(ANTKOL).LT.ANT) GO TO 200
C                                       Did this FQ id?
         IF ((RECORD(FRQKOL).NE.FREQID) .AND. (RECORD(FRQKOL).GT.0)
     *      .AND. (FREQID.GT.0)) GO TO 200
C                                       Did this source?
C                                       Check list
         IF (NSOUWD.GT.0) THEN
            THSOU = RECORD(SOUKOL)
            DO 150 J = 1,NSOUWD
               IF ((THSOU.EQ.SOUWAN(J)) .AND. DOSWNT) GO TO 170
               IF ((THSOU.EQ.SOUWAN(J)) .AND. (.NOT.DOSWNT)) GO TO 200
 150           CONTINUE
            IF (DOSWNT) GO TO 200
            END IF
C                                       Check subarray
 170     IF ((RECORD(SUBKOL).NE.SUB) .AND. (RECORD(SUBKOL).GT.0))
     *      GO TO 200
C                                       Check time (within 1 sec)
         IF (ABS (RECD(TIMKOL)-WRKTIM(ITIME)-TIMOFF) .GT. 1.574D-5)
     *      GO TO 200
C                                       Update
C                                       Amplitude
         IF (SMOTYP.EQ.1) THEN
            IF (WORK1(ITIME).EQ.FBLANK) GO TO 190
C                                       Use interpolated phase if input
C                                       blanked.
            IF ((RECR(REKOL).EQ.FBLANK) .OR. (RECR(IMKOL).EQ.FBLANK))
     *         THEN
               IF (XDOBLK.GE.0.0) THEN
                  RECR(REKOL) = WORK1(ITIME) * COS (WORK2(ITIME))
                  RECR(IMKOL) = WORK1(ITIME) * SIN (WORK2(ITIME))
                  CNTDO = CNTDO + 1
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               AMPL = SQRT (RECR(REKOL)*RECR(REKOL) +
     *            RECR(IMKOL)*RECR(IMKOL))
               IF (AMPL.LE.1.0E-20) AMPL = WORK1(ITIME)
               FAC = WORK1(ITIME) / AMPL
               RECR(REKOL) = RECR(REKOL) * FAC
               RECR(IMKOL) = RECR(IMKOL) * FAC
               CNTDO = CNTDO + 1
               END IF
C                                       Mean gain modulus
            IF (RECR(REKOL).NE.FBLANK) THEN
               GMCNT = GMCNT + 1
               GMSUM = GMSUM + SQRT (RECR(REKOL)*RECR(REKOL) +
     *            RECR(IMKOL)*RECR(IMKOL))
               END IF
C                                       Phase
         ELSE IF (SMOTYP.EQ.2) THEN
            IF (WORK1(ITIME).EQ.FBLANK) GO TO 190
C                                       Use interpolated amplitude for
C                                       blanked values.
            IF ((RECR(REKOL).EQ.FBLANK) .OR. (RECR(IMKOL).EQ.FBLANK))
     *         THEN
               IF (XDOBLK.GE.0.0) THEN
                  AMPL = WORK2(ITIME)
                  RECR(REKOL) = AMPL * COS (WORK1(ITIME))
                  RECR(IMKOL) = AMPL * SIN (WORK1(ITIME))
                  CNTDO = CNTDO + 1
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               AMPL = SQRT (RECR(REKOL)*RECR(REKOL) +
     *            RECR(IMKOL)*RECR(IMKOL))
               RECR(REKOL) = AMPL * COS (WORK1(ITIME))
               RECR(IMKOL) = AMPL * SIN (WORK1(ITIME))
               CNTDO = CNTDO + 1
               END IF
C                                       Amplitude and phase
         ELSE IF (SMOTYP.EQ.3) THEN
            IF ((WORK1(ITIME).EQ.FBLANK) .OR. (WORK2(ITIME).EQ.FBLANK))
     *         GO TO 190
            IF ((RECR(REKOL).EQ.FBLANK) .OR. (RECR(IMKOL).EQ.FBLANK))
     *         THEN
               IF (XDOBLK.GE.0.0) THEN
                  RECR(REKOL) = WORK1(ITIME) * COS (WORK2(ITIME))
                  RECR(IMKOL) = WORK1(ITIME) * SIN (WORK2(ITIME))
                  CNTDO = CNTDO + 1
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               RECR(REKOL) = WORK1(ITIME) * COS (WORK2(ITIME))
               RECR(IMKOL) = WORK1(ITIME) * SIN (WORK2(ITIME))
               CNTDO = CNTDO + 1
               END IF
C                                       Mean gain modulus
            IF (RECR(REKOL).NE.FBLANK) THEN
               GMCNT = GMCNT + 1
               GMSUM = GMSUM + SQRT (RECR(REKOL)*RECR(REKOL) +
     *            RECR(IMKOL)*RECR(IMKOL))
               END IF
C                                       amp,phase,delay,rate
         ELSE IF (SMOTYP.EQ.4) THEN
            IF ((WORK1(ITIME).EQ.FBLANK) .OR. (WORK2(ITIME).EQ.FBLANK)
     *         .OR. (WORK3(ITIME).EQ.FBLANK)
     *         .OR. (WORK4(ITIME).EQ.FBLANK)) GO TO 190
            IF ((RECR(REKOL).EQ.FBLANK) .OR. (RECR(IMKOL).EQ.FBLANK))
     *         THEN
               IF (XDOBLK.GE.0.0) THEN
                  RECR(REKOL) = WORK1(ITIME) * COS (WORK2(ITIME))
                  RECR(IMKOL) = WORK1(ITIME) * SIN (WORK2(ITIME))
                  RECR(RATKOL) = WORK3(ITIME)
                  RECR(DELKOL) = WORK4(ITIME)
                  CNTDO = CNTDO + 1
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               RECR(REKOL) = WORK1(ITIME) * COS (WORK2(ITIME))
               RECR(IMKOL) = WORK1(ITIME) * SIN (WORK2(ITIME))
               RECR(RATKOL) = WORK3(ITIME)
               RECR(DELKOL) = WORK4(ITIME)
               CNTDO = CNTDO + 1
               END IF
C                                       Mean gain modulus
            IF (RECR(REKOL).NE.FBLANK) THEN
               GMCNT = GMCNT + 1
               GMSUM = GMSUM + SQRT (RECR(REKOL)*RECR(REKOL) +
     *            RECR(IMKOL)*RECR(IMKOL))
               END IF
C                                       delay,rate
         ELSE IF (SMOTYP.EQ.5) THEN
            IF ((WORK1(ITIME).EQ.FBLANK) .OR. (WORK2(ITIME).EQ.FBLANK))
     *         GO TO 190
            IF ((RECR(DELKOL).EQ.FBLANK) .OR. (RECR(RATKOL).EQ.FBLANK))
     *         THEN
               IF (XDOBLK.GE.0.0) THEN
                  RECR(RATKOL) = WORK1(ITIME)
                  RECR(DELKOL) = WORK2(ITIME)
                  CNTDO = CNTDO + 1
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               RECR(RATKOL) = WORK1(ITIME)
               RECR(DELKOL) = WORK2(ITIME)
               CNTDO = CNTDO + 1
               END IF
            END IF
C                                       Count entries changed
 190     ITIME = ITIME + 1
C                                       Rewrite record
        CALL TABIO ('WRIT', IRCODE, ICLRNO, RECORD, BUFFER, IRET)
        IF (IRET.NE.0) GO TO 900
 200    CONTINUE
 590               FSTREC = SAVE
C                                       End of antenna loop
 600               CONTINUE
C                                       End of IF loop, update pointers
 640            WTKOL = WTKOL + 1
                REKOL = REKOL + 1
                IMKOL = IMKOL + 1
                DELKOL = DELKOL + 1
                RATKOL = RATKOL + 1
                TSKOL = TSKOL + 1
 650            CONTINUE
C                                       End of Stokes loop, update
 660         WTKOL = WT2KOL
             REKOL = RE2KOL
             IMKOL = IM2KOL
             DELKOL = DL2KOL
             RATKOL = RA2KOL
             TSKOL = TS2KOL
 700         CONTINUE
C                                       End subarray loop
 750      CONTINUE
C                                       Update GMMOD
      IF (((ABS (GMMOD-1.0).GT.1.0E-5) .AND. (XNORM.EQ.0.0)) .OR.
     *   (XNORM.GT.0.0)) THEN
         IF (GMCNT.GT.0) THEN
            GMMOD = GMSUM / GMCNT
            CALL TABKEY ('WRIT', KEYWRD, 1, BUFFER, 1, GMMOD, 2, IERR)
            END IF
         END IF
C                                       Close table
 890  CALL TABIO ('CLOS', IRCODE, ITIME, RECORD, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Number changed
      WRITE (MSGTXT,1890) CNTDO
      CALL MSGWRT (4)
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLSMTH: Smoothing CL table')
 1890 FORMAT ('Changed ',I6,' entries')
 1900 FORMAT ('CLSMTH: TABIO ERROR',I3,' SMOOTHING VALUES')
      END
      SUBROUTINE CLSMSM (SMMETH, SMOTIM, TIME, IN, SRC, BLANK, NUMTIM,
     *   OUT)
C-----------------------------------------------------------------------
C   Routine to call appropriate smoothing routine.  Magic value blanking
C   is supported.
C   Inputs:
C      SMMETH   C*4    Method 'BOX','MWF', unknown = 'BOX'
C      SMOTIM   R(3)   Smoothing time (days): support, FWHM, cutoff
C      TIME     R(*)   Times (days)
C      IN       R(*)   Input values.
C      SRC      I(*)   Source number list
C      BLANK    R      Magic blank value.
C      NUMTIM   I      Number of time/values
C   Output:
C      OUT      R(*) Output array
C-----------------------------------------------------------------------
      CHARACTER SMMETH*4
      REAL      SMOTIM(3), TIME(*), IN(*), BLANK, OUT(*)
      INTEGER   SRC(*), NUMTIM
C-----------------------------------------------------------------------
C                                       Median window filter
      IF (SMMETH.EQ.'MWF ') THEN
         CALL MWFBSM (SMOTIM, TIME, IN, SRC, BLANK, NUMTIM, OUT)
C                                       function types
      ELSE IF (SMMETH.EQ.'GAUS') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, SRC, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'EXP ') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, SRC, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'LINE') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, SRC, BLANK,
     *      NUMTIM, OUT)
C                                       2-point
      ELSE IF (SMMETH.EQ.'2PT ') THEN
         CALL TPTBSM (SMOTIM, TIME, IN, SRC, BLANK, NUMTIM, .FALSE.,
     *      OUT)
C                                       2-point - hanning
      ELSE IF (SMMETH.EQ.'2PTH') THEN
         CALL TPTBSM (SMOTIM, TIME, IN, SRC, BLANK, NUMTIM, .TRUE., OUT)
C                                       Default = Boxcar
      ELSE
         CALL BOXBSM (SMOTIM, TIME, IN, SRC, BLANK, NUMTIM, OUT)
         END IF
C
 999  RETURN
      END
      SUBROUTINE CLSMHI
C-----------------------------------------------------------------------
C   CLSMHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, LABEL*8
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'CLSMO.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 2 and label.
         WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Rest of sources
         DO 20 I = 3,NSOUWD,2
            WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 20         CONTINUE
         END IF
C                                       Antennas
 25   IF (NANTSL.LE.0) THEN
         WRITE (HILINE,3005) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      ELSE
C                                       Included or excluded?
         WRITE (HILINE,3006) TSKNAM
         IF (DOAWNT) WRITE (HILINE,3007) TSKNAM
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
         LIMIT = MIN (12, NANTSL)
         WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
C                                       Rest of antennas
         DO 30 I = 13,NANTSL,12
            LIMIT = I
            LIMIT2 = I + 11
            LIMIT2 = MIN (NANTSL, LIMIT2)
            WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 30         CONTINUE
         END IF
C                                       Timerange
      TIMBEG = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TIMEND = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, GAINVER
      WRITE (HILINE,2002) TSKNAM, SUBA, CLVER
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation fn
      WRITE (HILINE,2006) TSKNAM, XINTP
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation parms
      WRITE (HILINE,2007) TSKNAM, XIPARM(1), XIPARM(2), XIPARM(3)
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,4007) TSKNAM, XIPARM(4), 'support'
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       extra parms
      IF ((XINTP.EQ.'GAUS') .OR. (XINTP.EQ.'EXP ') .OR.
     *   (XINTP.EQ.'LINE')) THEN
         WRITE (HILINE,2007) TSKNAM, XIPARM(6), XIPARM(7), XIPARM(8)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4007) TSKNAM, XIPARM(9), 'FWHM'
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4008) TSKNAM, CUTOFF
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       smoothing type
      WRITE (HILINE,2008) TSKNAM, XSMO
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any other history.
         IF (NUMHIS.LE.0) GO TO 100
         WRITE (LABEL,1011) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 90 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 90         CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLSMHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,'  ')
 2002 FORMAT (A6,'SUBARRAY =',I3,' GAINVER = ',I4,' /CL table')
 2004 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2005 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 2006 FORMAT (A6,'INTERPOL = ''',A4,''' / Interpolation type')
 2007 FORMAT (A6,'INTPARM = ',3F10.5,' / Interpolation parms')
 4007 FORMAT (A6,'          ',F10.5,' / Interpolation ',A)
 4008 FORMAT (A6,'CUTOFF =',F8.5,'  / sum of weights cutoff')
 2008 FORMAT (A6,'SMOTYPE = ''',A4,''' / Data to be smoothed')
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded:')
 3002 FORMAT (A6,'/Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'ANTENNAS = 0     /All antennas selected')
 3006 FORMAT (A6,'/Antennas excluded:')
 3007 FORMAT (A6,'/Antennas included:')
 3008 FORMAT (A6,'ANTENNAS = ',12(I3,' '))
 3009 FORMAT (A6,'           ',12(I3,' '))
      END
