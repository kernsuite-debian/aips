LOCAL INCLUDE 'SNSMO.INC'
C                                                          Include SNSMO
C                                       Local include for SNSMO
C                                       Needs parameter from PUVD.INC
C                                       Inputs and general info
      INTEGER   SEQIN, SUBA, DISKIN, CNOIN, SNVER, ISNIN, ISNOUT,
     *   NSOUWD, SOUWAN(30), NANTSL, ANTENS(50), BIF, EIF, FREQID,
     *   REFANT, NPIECE
      LOGICAL   DOSWNT, DOAWNT
      REAL      XSIN, XDISIN, XFQID, XBAND, XFREQ, XBIF, XEIF, XTIME(8),
     *   XANT(50), XSUBA, XIPARM(10), CUTOFF, XDOBLK, XCPARM(10), XDELC,
     *   XPIECE, XNORM, XINVER, XOUTVR, XREFA, XBAD(10), SELBAN, DOBTWN
      CHARACTER  NAMEIN*12, CLAIN*6, XSOUR(30)*16, XINTP*4, XSMO*4
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXINTP,
     *   XXSMO
      DOUBLE PRECISION FRQOFF(MAXIF), SELFRQ, TSTART, TEND,
     *   FREQS(MAXIF), FREQD(MAXIF)
C                                       Buffers and file info
      INTEGER   BUFFER(512), BUFF2(512), RECORD(XCLRSZ)
      REAL      RECR(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2)
      EQUIVALENCE (RECORD, RECR, RECD)
      INTEGER   MXTIME
C                                       MXTIME = dim work arrays
      PARAMETER (MXTIME = 25000)
      REAL      WRKTIM(MXTIME), WORK1(MXTIME), WORK2(MXTIME),
     *   WORK3(MXTIME),  WORK4(MXTIME), WORK5(MXTIME),
     *   WORK6(MXTIME), WORK7(MXTIME), WORK8(MXTIME)
      INTEGER   WRKREC(MXTIME), WRKSRC(MXTIME)
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XXSOUR, DOBTWN,
     *   XBAND, XFREQ, XFQID, XBIF, XEIF, XTIME, XANT,  XSUBA, XXINTP,
     *   XIPARM, CUTOFF, XDOBLK, XXSMO, XCPARM, XDELC, XPIECE, XNORM,
     *   XINVER, XOUTVR, XREFA, XBAD,SELBAN, SEQIN, DISKIN, CNOIN, SUBA,
     *   SNVER, ISNIN, ISNOUT,REFANT
      COMMON /CINFO/ FRQOFF, FREQS, FREQD, SELFRQ, TSTART, TEND, DOSWNT,
     *   DOAWNT, NSOUWD, SOUWAN, NANTSL, ANTENS, BIF, EIF, FREQID,
     *   NPIECE
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XINTP, XSMO
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER, BUFF2, RECORD
C                                       Align WRK* in memory
      COMMON /XXYYZZ/ WRKTIM, WORK1, WORK2, WORK3, WORK4, WORK5, WORK6,
     *   WORK7, WORK8, WRKREC, WRKSRC
C                                                          End SNSMO
LOCAL END
      PROGRAM SNSMO
C-----------------------------------------------------------------------
C! Smooths a SN table
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2003-2008, 2010-2012, 2014-2015
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
C   Task SNSMO smooths a SN table.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'SNSMO '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL SNSMIN (PRGM, IRET)
C                                       Clip
      IF (IRET.EQ.0) CALL SNCLIP (IRET)
C                                       Rereference
      IF (IRET.EQ.0) CALL SNREF (IRET)
C                                       Smooth
      IF (IRET.EQ.0) CALL SNSMOO (IRET)
C                                       Copy and update HI file.
      IF (IRET.EQ.0) CALL SNSMHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE SNSMIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SNSMIN gets input parameters for SNSMO.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok
C                               1 => Invalid request
C                               5 => catalog troubles
C                               8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, DESEL, MATCH, WTABLE, WEXIST, WFITS
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN,
     *   LUN2, IIVER, NUMIF, NPIF
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      FINC(MAXIF), DPIX
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/, LUN2 /30/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 232
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
      ISNIN = MAX (IROUND (XINVER), 0)
      ISNOUT = MAX (IROUND (XOUTVR), 0)
      REFANT = IROUND (XREFA)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXINTP, XINTP)
      CALL H2CHR (4, 1, XXSMO, XSMO)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
C                                       Check smoothing code
      IF ((XSMO.NE.'AMPL') .AND. (XSMO.NE.'PHAS') .AND. (XSMO.NE.'BOTH')
     *   .AND. (XSMO.NE.'FULL') .AND. (XSMO.NE.'VLBI') .AND.
     *   (XSMO.NE.'VLRI') .AND. (XSMO.NE.'VLMB') .AND. (XSMO.NE.'DELA')
     *   .AND. (XSMO.NE.'VLDE'))  XSMO = 'AMPL'
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
C                                       Reformat? This also assures that
C                                       it is present.
C      CALL SNREFM (DISKIN, CNOIN, SNVER, CATBLK, LUN, JERR)
C      IF (JERR.NE.0) GO TO 999
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FREQID = IROUND (XFQID)
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ,
     *   MATCH, FREQID, JERR)
C                                       No match
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       IF range
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
C                                       all for pieced bits
      ELSE IF ((XSMO.EQ.'VLMB') .OR. (XSMO.EQ.'VLDE')) THEN
         BIF = 1
         EIF = CATBLK(KINAX+JLOCIF)
C                                       let user decide
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         IF (BIF.LE.0) BIF = 1
         IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATBLK(KINAX+JLOCIF)
         IF (EIF.LE.0) EIF = 1
         IF (BIF.GT.CATBLK(KINAX+JLOCIF)) BIF = CATBLK(KINAX+JLOCIF)
         IF (EIF.GT.CATBLK(KINAX+JLOCIF)) EIF = CATBLK(KINAX+JLOCIF)
         END IF
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
      IF (.NOT.ALLANT) THEN
C                                       Not all selected - make list
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.NE.0) THEN
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
C                                       Get source numbers.
C                                       Check if single-source file.
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUN, BUFFER, WTABLE, WEXIST,
     *   WFITS, JERR)
      IF ((JERR.EQ.0) .AND. WEXIST .AND. WTABLE) THEN
         CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFER, NSOUWD, DOSWNT,
     *      SOUWAN, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
         NSOUWD = 0
         END IF
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, FINC, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                        IF frequencies
      NPIECE = XPIECE + 0.5
      IF (NPIECE.LE.0) NPIECE = NUMIF
      IF ((XSMO.NE.'VLMB') .AND. (XSMO.NE.'VLDE')) NPIECE = NUMIF
      NPIECE = MIN (NPIECE, NUMIF)
      NPIF = NUMIF / NPIECE
      IF (NPIF*NPIECE.NE.NUMIF) THEN
         MSGTXT = 'NUMIF AND NPIECE NOT COMPATIBLE'
         IERR = 10
         GO TO 990
         END IF
      DPIX = (CATBLK(KINAX+JLOCF) + 1.0) / 2.0 - CATR(KRCRP+JLOCF)
      DO 160 I = 1,NUMIF
         FREQS(I) = FREQ + FRQOFF(I)
         J = I - MOD (I-1, NPIF)
         FREQD(I) = DPIX * FINC(I) + FRQOFF(I) - FRQOFF(J)
 160     CONTINUE
C                                       Timerange
      TSTART = XTIME(1) + XTIME(2) / 24.0D0 + XTIME(3) / (24.0D0*60.0D0)
     *   + (XTIME(4) / (24.0D0*60.0D0*60.0D0))
      TEND = XTIME(5) + XTIME(6) / 24.0D0 + XTIME(7) / (24.0D0*60.0D0) +
     *   (XTIME(8) / (24.0D0*60.0D0*60.0D0))
      IF ((TEND.LT.TSTART) .OR. (TEND.LT.1.0D-5)) TEND = 1.0D20
C                                       Copy input SN table version
C                                       to output version. Smoothing
C                                       is carried out on the output
C                                       table in place.
      IF (ISNIN.EQ.0) CALL FNDEXT ('SN', CATBLK, ISNIN)
      SNVER = ISNOUT
      IF (ISNIN.NE.ISNOUT) THEN
         CALL TABCOP ('SN', ISNIN, ISNOUT, LUN, LUN2, DISKIN, DISKIN,
     *      CNOIN, CNOIN, CATBLK, BUFFER, BUFF2, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1200) JERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNSMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1200 FORMAT ('SNSMIN: ERROR',I4,' RETURNED BY TABCOP')
      END
      SUBROUTINE SNCLIP (IRET)
C-----------------------------------------------------------------------
C   Clips selected portions of SN tables.
C   Leaves the output table sorted in antenna-time order.
C    Inputs from common:
C      SNVER        I    Cal (SN) file version number.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C    Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  COLHED(2)*24
      INTEGER   KEY(2,2), ICLUN, SNANT, SNTIM, ISNRNO,  NKEY, KOLS(2),
     *   SNNUMV(MAXSNC), SNKOLS(MAXSNC), NUMANT, KEYSUB(2,2),
     *   NUMPOL, NUMIF, I, NUMNOD, SUB, NUMSUB, TIMKOL, SOUKOL, ANTKOL,
     *   SUBKOL, FRQKOL, MB1KOL, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL,
     *   MB2KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, DI1KOL, DI2KOL
      LOGICAL   T, ISAPPL, DODELA, DORATE, DOAMP, DOPH, DOMBDE, DODISP
      REAL      FKEY(2,2), GMMOD, RANOD(25), DECNOD(25), STDELA(3),
     *   STRATE(3), STAMP(3), STPH(3), STMBDE(3), MXDELA, MXMBDE,
     *   MXRATE, MXAMP, MXPH, MXDISP
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA T /.TRUE./
      DATA ICLUN /30/
C-----------------------------------------------------------------------
C                                       Sort SN table to antenna-time.
C                                       Need col. pointers, sort order.
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,  NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNCLIP: SNINI FOR READ FAILS'
         GO TO 990
         END IF
      IF (BUFFER(5).LE.0) THEN
         MSGTXT = 'SNCLIP: sn TABLE IS EMPTY'
         IRET = 8
         GO TO 990
         END IF
C                                       Get column pointers
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
         MSGTXT = 'SNCLIP: DOES NOT FIND NEEDED COLUMNS'
         GO TO 990
         END IF
      IRET = 0
      SNTIM = KOLS(1)
      SNANT = KOLS(2)
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      SOUKOL = SNKOLS(SNISID)
      FRQKOL = SNKOLS(SNIFQI)
      MB1KOL = SNKOLS(SNRMD1)
      DI1KOL = SNKOLS(SNRDS1)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      MB2KOL = SNKOLS(SNRMD2)
      DI2KOL = SNKOLS(SNRDS2)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) GO TO 999
      KEY(1,1) = SNANT
      KEY(1,2) = SNTIM
C                                       Sort to antenna time order.
      IF (((BUFFER(43).NE.SNANT) .OR. (BUFFER(44).NE.SNTIM))) THEN
         CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SNCLIP: SORT FAILS'
            GO TO 990
            END IF
         END IF
C                                       Reopen write
      CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNCLIP: SNINI FOR WRITE FAILS'
         GO TO 990
         END IF
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
C                                       Determine smoothing parameters.
      STDELA(1) = XCPARM(4) / 24.0
      STMBDE(1) = XCPARM(5) / 24.0
      STRATE(1) = XCPARM(3) / 24.0
      STAMP(1) = XCPARM(1) / 24.0
      STPH(1) = XCPARM(2) / 24.0
C
      MXDELA = ABS (XCPARM(9)) * 1.0E-9
      MXMBDE = ABS (XCPARM(10)) * 1.0E-9
      MXRATE = ABS (XCPARM(8))  / 1.0E3
      MXAMP = ABS (XCPARM(6))
      MXPH = ABS (XCPARM(7)) / 57.296
      MXDISP = ABS (XDELC) * 1.E-9
      DODELA = MXDELA .GE. 1.0E-10
      DOMBDE = MXMBDE .GE. 1.0E-10
      DODISP = MXDISP .GE. 1.0E-10
      DORATE =  MXRATE .GE. 1.0E-18
      DOAMP =  MXAMP .GE. 1.0E-10
      DOPH =  MXPH .GE. 1.0E-10
      IF (MXDELA.LT.1.0E-10) MXDELA = 1.0E20
      IF (MXMBDE.LT.1.0E-10) MXMBDE = 1.0E20
      IF (MXDISP.LT.1.0E-10) MXDISP = 1.0E20
      IF (MXRATE.LT.1.0E-18) MXRATE = 1.0E20
      IF (MXAMP.LT.1.0E-10) MXAMP = 1.0E20
      IF (MXPH.LT.1.0E-10) MXPH = 1.0E20
C                                       Inform user of smoothing:
      IF (DODELA.OR.DOMBDE.OR.DORATE.OR.DOAMP.OR.DOPH.OR.DODISP) THEN
         MSGTXT ='SNSMTH: Clipping SN table'
         CALL MSGWRT (4)
         END IF
C                                       Loop over subarrays
      DO 200 SUB = 1,NUMSUB
C                                       Want this subarray?
         IF ((SUBA.GT.0) .AND. (SUB.NE.SUBA)) GO TO 200
C                                       Clip Multiband delay
         IF (DOMBDE) THEN
            CALL CLPDLY (STMBDE, MXMBDE, SUB, NUMANT, NUMPOL, TIMKOL,
     *         SUBKOL, ANTKOL, SOUKOL, FRQKOL, MB1KOL, MB2KOL, 0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Clip Dispersion
         IF (DODISP) THEN
            CALL CLPDLY (STMBDE, MXDISP, SUB, NUMANT, NUMPOL, TIMKOL,
     *         SUBKOL, ANTKOL, SOUKOL, FRQKOL, DI1KOL, DI2KOL, 0, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                        Clip Single band delay
         IF (DODELA) THEN
            DO 60 I = BIF,EIF
               CALL CLPDLY (STDELA, MXDELA, SUB, NUMANT, NUMPOL, TIMKOL,
     *            SUBKOL, ANTKOL, SOUKOL, FRQKOL, DL1KOL+I-1,
     *            DL2KOL+I-1, I, IRET)
               IF (IRET.NE.0) GO TO 999
 60            CONTINUE
            END IF
C                                        Clip rates
         IF (DORATE) THEN
            IF ((XSMO.EQ.'VLRI') .OR. (XSMO.EQ.'VLMB') .OR.
     *         (XSMO.EQ.'VLDE')) THEN
               CALL CLPRAT (STRATE, MXRATE, SUB, NUMANT, NUMPOL, NUMIF,
     *            FREQS, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, RA1KOL,
     *            WT1KOL, RA2KOL, WT2KOL, IRET)
               IF (IRET.NE.0) GO TO 999
            ELSE
               DO 70 I = BIF,EIF
                  CALL CLPRAT (STRATE, MXRATE, SUB, NUMANT, NUMPOL, 1,
     *               FREQS(I), TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *               RA1KOL+I-1, WT1KOL+I-1, RA2KOL+I-1, WT2KOL+I-1,
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
 70               CONTINUE
               END IF
            END IF
C                                       Clip amp/phase
         IF (DOAMP.OR.DOPH) THEN
            DO 100 I = BIF,EIF
               CALL CLPAPH (STAMP, STPH, MXAMP, MXPH, DOAMP, DOPH, SUB,
     *            NUMANT, I, I, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *            RA1KOL, RE1KOL, IM1KOL, WT1KOL, RA2KOL, RE2KOL,
     *            IM2KOL, WT2KOL, IRET)
               IF (IRET.NE.0) GO TO 999
 100           CONTINUE
            END IF
 200     CONTINUE
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNCLIP: CLOSE TABLE FOR WRITE FAILS'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE CLPDLY (STIME, MXDLY, SUB, NUMANT, NUMPOL, TIMKOL,
     *   SUBKOL, ANTKOL, SOUKOL, FRQKOL, DL1KOL, DL2KOL, BEGIF, IRET)
C-----------------------------------------------------------------------
C   Routine to Clip delays using a comparison with a running Median
C   window.
C   Inputs:
C      STIME    R  Smoothing time (days)
C      MXDLY    R  Max delay residual (sec)
C      SUB      I  Desired subarray
C      NUMANT   I  Number of antennas
C      NUMPOL   I  Number of polarizations (1 or 2)
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      FRQKOL   I  FQ id column pointer
C      SOUKOL   I  Source ID column pointer
C      DL1KOL   I  Delay  column pointer poln. 1
C      DL2KOL   I  Delay  column pointer poln. 2
C      BEGIF    I  IF for display purposes only
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STIME(*), MXDLY
      INTEGER   SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, DL1KOL, DL2KOL, BEGIF, IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, ITIME, NRECS, NDEL
      LOGICAL   SLCTD, SNWANT, BAD, BAD2, WANT
      REAL       DIFF
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      IRET = 8
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
      NRECS = 0
      NDEL = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND.
     *         (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND.
     *         (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       See if flagged value
               BAD = (RECR(DL1KOL).EQ.FBLANK)
               BAD2 = (NUMPOL.LE.1) .OR. (RECR(DL2KOL).EQ.FBLANK)
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                  ELSE
                     WORK2(NUMTIM) = RECR(DL1KOL)
                     END IF
                  IF (BAD2) THEN
                     WORK3(NUMTIM) = FBLANK
                  ELSE
                     WORK3(NUMTIM) = RECR(DL2KOL)
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
         IF (NUMTIM.LE.0) GO TO 590
C                                       Smooth as requested
         CALL SNSMSM ('MWF ', STIME, WRKTIM, WORK2, FBLANK, NUMTIM,
     *      WRKSRC, WORK1)
C                                       Second Poln?
         IF (NUMPOL.GT.1) THEN
            CALL SNSMSM ('MWF ', STIME, WRKTIM, WORK3, FBLANK,
     *         NUMTIM, WRKSRC, WORK2)
            END IF
C                                       Clip
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.GT.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            BAD = .FALSE.
            DIFF = ABS (RECR(DL1KOL) - WORK1(ITIME))
            IF (DIFF.GT.MXDLY) THEN
               RECR(DL1KOL) = FBLANK
               BAD = .TRUE.
               NDEL = NDEL + 1
               END IF
C                                       Second polarization?
            IF (NUMPOL.GE.2) THEN
               DIFF = ABS (RECR(DL2KOL) - WORK2(ITIME))
               IF (DIFF.GT.MXDLY) THEN
                  RECR(DL2KOL) = FBLANK
                  BAD = .TRUE.
                  NDEL = NDEL + 1
                  END IF
               END IF
C                                       Rewrite record
            IF (BAD) THEN
               CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 900
               NRECS = NRECS + 1
               END IF
 200        CONTINUE
 590     FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      IF (NRECS.GT.0) THEN
         WRITE (MSGTXT,1600) BEGIF, NDEL
         CALL MSGWRT (4)
         WRITE (MSGTXT,1602) BEGIF, NRECS
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('IF',I3,' clipped',I10,' gains due to delays')
 1602 FORMAT ('IF',I3,' changed',I10,' table records due to clipping')
 1900 FORMAT ('CLPDLY: TABIO ERROR',I3,' CLIPPING DELAYS')
      END
      SUBROUTINE CLPRAT (STIME, MXRATE, SUB, NUMANT, NUMPOL, NUMIF,
     *   FQS, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, RA1KOL, WT1KOL,
     *   RA2KOL, WT2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to clip fringe rates using a comparison with a median window
C   filter.
C   Inputs:
C      STIME    R  Smoothing time (days)
C      MXRATE   R  Max. rate residual (Hz)
C      SUB      I  Desired subarray
C      NUMANT   I  Number of antennas
C      NUMPOL   I  Number of polarizations
C      NUMIF    I  The number of IFs
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      FRQKOL   I  FQ id column pointer
C      SOUKOL   I  Source ID column pointer
C      RA1KOL   I  Rate pol 1  column pointer
C      WT1KOL   I  Weight pol 1 column pointer.
C      RA2KOL   I  Rate pol 2  column pointer <1 => not present
C      WT2KOL   I  Weight pol 2 column pointer <1 => not present
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STIME(*), MXRATE
      INTEGER   SUB, NUMANT, NUMPOL, NUMIF, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RA1KOL, WT1KOL, RA2KOL, WT2KOL, IRET
      DOUBLE PRECISION FQS(*)
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, I, ITIME, NRECS, NRAT
      REAL      SUM, COUNT, DIFF
      LOGICAL   SNWANT, SLCTD, BAD, WANT
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      IRET = 8
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
      NRECS = 0
      NRAT = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND. (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND. (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       Average rate
               SUM = 0.0
               COUNT = 0.0
               DO 20 I = 1,NUMIF
                  IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *               (RECR(RA1KOL+I-1).NE.FBLANK)) THEN
                     SUM = SUM + FQS(I) * RECR(RA1KOL+I-1)
                     COUNT = COUNT + 1
                     END IF
                  IF ((NUMPOL.GT.1) .AND. (RECR(WT2KOL+I-1).GT.0.0)
     *                  .AND. (RECR(RA2KOL+I-1).NE.FBLANK)) THEN
                     SUM = SUM + FQS(I) * RECR(RA2KOL+I-1)
                     COUNT = COUNT + 1
                     END IF
 20               CONTINUE
C                                       See if flagged value
               BAD = COUNT.LE.0.1
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                  ELSE
                     WORK2(NUMTIM) = SUM / COUNT
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
C                                       Smooth as requested
         CALL SNSMSM ('MWF ', STIME, WRKTIM, WORK2, FBLANK, NUMTIM,
     *      WRKSRC, WORK1)
C                                       Clip
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            IF (IRET.NE.0) GO TO 900
            BAD = .FALSE.
            IF (WORK1(ITIME).NE.FBLANK) THEN
               DO 120 I = 1,NUMIF
                  DIFF = ABS (FQS(I)*RECR(RA1KOL+I-1) - WORK1(ITIME))
                  IF (DIFF.GT.MXRATE) THEN
                     RECR(RA1KOL+I-1) = FBLANK
                     BAD = .TRUE.
                     NRAT = NRAT + 1
                     END IF
C                                       Second polarization present?
                  IF (NUMPOL.GT.1) THEN
                     DIFF = ABS (FQS(I)*RECR(RA2KOL+I-1) - WORK1(ITIME))
                     IF (DIFF.GT.MXRATE) THEN
                        RECR(RA2KOL+I-1) = FBLANK
                        BAD = .TRUE.
                        NRAT = NRAT + 1
                        END IF
                     END IF
 120              CONTINUE
C                                       Rewrite record
               IF (BAD) THEN
                  CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER,
     *               IRET)
                  IF (IRET.NE.0) GO TO 900
                  NRECS = NRECS + 1
                  END IF
               END IF
 200        CONTINUE
C                                       First SN number of next antenna
         FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      IF (NRECS.GT.0) THEN
         WRITE (MSGTXT,1600) NRAT
         CALL MSGWRT (4)
         WRITE (MSGTXT,1602) NRECS
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('CLPRAT: clipped',I10,' gains due to rates')
 1602 FORMAT ('CLPRAT: changed',I10,' table records due to clipping')
 1900 FORMAT ('CLPRAT: TABIO ERROR',I3,' CLIPPING RATES')
      END
      SUBROUTINE CLPAPH (STAMP, STPH, MXAMP, MXPH, DOAMP, DOPH, SUB,
     *   NUMANT, IFBEG, IFEND, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *   RA1KOL, RE1KOL, IM1KOL, WT1KOL, RA2KOL, RE2KOL, IM2KOL,
     *   WT2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to clip amplitudes and/or phases rates by comparison with a
C   median window filter.
C   All poln present and the range of IF specified by IFBEG and IFEND
C   are clipped.  The values in a single polarization are
C   averaged.
C      The phases are corrected by the integral of the rate functions
C   from the first time before smoothing.   All selected phases in each
C   polarization are averaged and corrected using the integrated phase
C   function for the first IF selected.
C   Input table must be in antenna-time order.
C   Inputs:
C      STAMP    R    Amplitude smoothing time (days)
C      STPH     R    Phase smoothing time (days)
C      MXAMP    R    Max. amp residual
C      MXPH     R    Max. phase residual (rad)
C      DOAMP    L    Smooth amplitudes?
C      DOPH     L    Smooth phases?
C      SUB      I    Desired subarray
C      NUMANT   I    Number of antennas
C      IFBEG    I    First IF
C      IFEND    I    Highest IF
C      TIMKOL   I    Time column pointer.
C      SUBKOL   I    Subarray column pointer
C      ANTKOL   I    Antenna column pointer
C      FRQKOL   I    FQ id column pointer
C      SOUKOL   I    Source ID column pointer
C      RA1KOL   I    Rate pol 1  column pointer
C      RE1KOL   I    Real 1  column pointer
C      IM1KOL   I    Imaginary 1  column pointer
C      WT1KOL   I    Weight 1 column pointer.
C      RA2KOL   I    Rate pol 2 pointer <1 => not present
C      RE2KOL   I    Real 2  column pointer <1 => not present
C      IM2KOL   I    Imaginary 2  column pointer
C      WT2KOL   I    Weight 2 column pointer <1 => not present
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STAMP(*), STPH(*), MXAMP, MXPH
      LOGICAL   DOAMP, DOPH
      INTEGER   SUB, NUMANT, IFBEG, IFEND, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RA1KOL, RE1KOL, IM1KOL, WT1KOL, RA2KOL, RE2KOL,
     *   IM2KOL, WT2KOL, IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, I, ITIME, NUGOOD, NAMP, NPHASE, NRECS
      REAL      SUMRE1, SUMIM1, SUMRE2, SUMIM2, COUNT1, COUNT2,
     *   IPHASE, CPH, RATE, IPRE, IPIM, AMP, PHASE, TWOPIR, DIFF
      LOGICAL   SLCTD, SNWANT, BAD, BAD2, WANT
      DOUBLE PRECISION TIMOFF, INTFAZ, PHADD, LSTIME, TWOPI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN (1.0D0)
      TWOPIR = TWOPI
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
      NAMP = 0
      NPHASE = 0
      NRECS = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
         NUGOOD = 0
C                                       Integrated phase function
         INTFAZ = 0.0D0
         LSTIME = 0.0
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND.
     *         (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND.
     *         (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       Average phases
               SUMRE1 = 0.0
               SUMIM1 = 0.0
               COUNT1 = 0.0
               SUMRE2 = 0.0
               SUMIM2 = 0.0
               COUNT2 = 0.0
               RATE = FBLANK
               DO 20 I = IFBEG,IFEND
                  IF ((RATE.EQ.FBLANK) .AND.
     *               (RECR(RA1KOL+I-1).NE.FBLANK)) RATE = FREQS(I) *
     *               RECR(RA1KOL+I-1)
                  IF (RECR(RE1KOL+I-1).NE.FBLANK) THEN
                     IF ((RATE.EQ.FBLANK) .AND.
     *                  (RECR(RA1KOL+I-1).NE.FBLANK)) RATE = FREQS(I) *
     *                  RECR(RA1KOL+I-1)
                     SUMRE1 = SUMRE1 + RECR(RE1KOL+I-1)
                     SUMIM1 = SUMIM1 + RECR(IM1KOL+I-1)
                     COUNT1 = COUNT1 + 1.0
                     END IF
                  IF ((RA2KOL.GT.0) .AND. (RATE.EQ.FBLANK) .AND.
     *               (RECR(RA2KOL+I-1).NE.FBLANK)) RATE = FREQS(I) *
     *               RECR(RA2KOL+I-1)
                  IF ((RA2KOL.GT.0) .AND.
     *               (RECR(RE2KOL+I-1).NE.FBLANK)) THEN
                     SUMRE2 = SUMRE2 + RECR(RE2KOL+I-1)
                     SUMIM2 = SUMIM2 + RECR(IM2KOL+I-1)
                     COUNT2 = COUNT2 + 1.0
                     END IF
 20               CONTINUE
C                                       See if flagged value
               BAD = COUNT1 .LE. 0.1
               BAD2 = COUNT2 .LE. 0.1
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) THEN
                     TIMOFF = RECD(TIMKOL)
                     LSTIME = 0.0D0
                     END IF
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
C                                       Compute integrated phase
C                                       function; use current rate since
C                                       last time
                  INTFAZ = 0.0
                  IF (RATE.NE.FBLANK) THEN
                     PHADD = TWOPI * RATE * (WRKTIM(NUMTIM)-LSTIME) *
     *                  86400.0D0
                     INTFAZ = INTFAZ + PHADD
                     LSTIME = WRKTIM(NUMTIM)
                     END IF
                  IPRE = COS (INTFAZ)
                  IPIM = SIN (INTFAZ)
                  CPH = ATAN2 (IPIM, IPRE)
                  WORK8(NUMTIM) = CPH
C                                       Accumulate by real, imaginary
C                                       and amplitude.
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                     WORK3(NUMTIM) = FBLANK
                     WORK4(NUMTIM) = FBLANK
                  ELSE
                     NUGOOD = NUGOOD + 1
                     SUMRE1 = SUMRE1 / COUNT1
                     SUMIM1 = SUMIM1 / COUNT1
C                                        Subtract integrated phase
                     WORK2(NUMTIM) = SUMRE1*IPRE + SUMIM1*IPIM
                     WORK3(NUMTIM) = SUMIM1*IPRE - SUMRE1*IPIM
                     WORK4(NUMTIM) = SQRT (WORK2(NUMTIM)*WORK2(NUMTIM) +
     *                  WORK3(NUMTIM)*WORK3(NUMTIM)) + 1.0E-20
C                                       Normalize real and imag.
                     WORK2(NUMTIM) = WORK2(NUMTIM) / WORK4(NUMTIM)
                     WORK3(NUMTIM) = WORK3(NUMTIM) / WORK4(NUMTIM)
                     END IF
                  IF (BAD2) THEN
                     WORK5(NUMTIM) = FBLANK
                     WORK6(NUMTIM) = FBLANK
                     WORK7(NUMTIM) = FBLANK
                  ELSE
                     NUGOOD = NUGOOD + 1
                     SUMRE2 = SUMRE2 / COUNT2
                     SUMIM2 = SUMIM2 / COUNT2
C                                        Subtract integrated phase
                     WORK5(NUMTIM) = SUMRE2*IPRE + SUMIM2*IPIM
                     WORK6(NUMTIM) = SUMIM2*IPRE - SUMRE2*IPIM
                     WORK7(NUMTIM) = SQRT (WORK5(NUMTIM)*WORK5(NUMTIM) +
     *                  WORK6(NUMTIM)*WORK6(NUMTIM)) + 1.0E-20
C                                       Normalize real and imag.
                     WORK5(NUMTIM) = WORK5(NUMTIM) / WORK7(NUMTIM)
                     WORK6(NUMTIM) = WORK6(NUMTIM) / WORK7(NUMTIM)
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
C                                       Smooth as requested
         CALL SNSMSM ('MWF ', STPH, WRKTIM, WORK2, FBLANK,
     *      NUMTIM, WRKSRC, WORK1)
         CALL SNSMSM ('MWF ', STPH, WRKTIM, WORK3, FBLANK,
     *      NUMTIM, WRKSRC, WORK2)
         CALL SNSMSM ('MWF ', STAMP, WRKTIM, WORK4, FBLANK,
     *      NUMTIM, WRKSRC, WORK3)
C                                       Second polarization if present
         IF (RE2KOL.GT.0) THEN
            CALL SNSMSM ('MWF ', STPH, WRKTIM, WORK5, FBLANK,
     *         NUMTIM, WRKSRC, WORK4)
            CALL SNSMSM ('MWF ', STPH, WRKTIM, WORK6, FBLANK,
     *         NUMTIM, WRKSRC, WORK5)
            CALL SNSMSM ('MWF ', STAMP, WRKTIM, WORK7, FBLANK,
     *         NUMTIM, WRKSRC, WORK6)
            END IF
C                                       Clip
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            IF (IRET.NE.0) GO TO 900
            BAD = .FALSE.
C                                       Update
            IF ((WORK1(ITIME).NE.FBLANK) .AND. (WORK2(ITIME).NE.FBLANK)
     *         .AND. (WORK3(ITIME).NE.FBLANK)) THEN
C                                       Smoothed phase same for all IFs;
C                                       add integrated phase function.
               IF ((WORK2(ITIME).NE.FBLANK) .AND.
     *            (WORK1(ITIME).NE.FBLANK)) THEN
                  IPHASE = ATAN2(WORK2(ITIME), WORK1(ITIME)+1.0E-20) +
     *               WORK8(ITIME)
               ELSE
                  IPHASE = FBLANK
                  END IF
               DO 130 I = IFBEG,IFEND
C                                       Clip amplitude
                  IF (DOAMP .AND. (RECR(RE1KOL+I-1).NE.FBLANK) .AND.
     *               (WORK3(ITIME).NE.FBLANK)) THEN
                     AMP = SQRT (RECR(RE1KOL+I-1)*RECR(RE1KOL+I-1)
     *                  + RECR(IM1KOL+I-1)*RECR(IM1KOL+I-1))
                     DIFF = ABS (AMP - WORK3(ITIME))
                     IF (DIFF.GT.MXAMP) THEN
                        RECR(RE1KOL+I-1) = FBLANK
                        RECR(IM1KOL+I-1) = FBLANK
                        RECR(WT1KOL+I-1) = 0.0
                        BAD = .TRUE.
                        NAMP = NAMP + 1
                        END IF
                     END IF
C                                       Clip phase
                  IF (DOPH .AND. (RECR(RE1KOL+I-1).NE.FBLANK) .AND.
     *               (IPHASE.NE.FBLANK)) THEN
                     PHASE = ATAN2(RECR(IM1KOL+I-1),
     *                  RECR(RE1KOL+I-1)+1.0E-20)
                     DIFF = ABS (PHASE - IPHASE)
                     DIFF = MOD (DIFF, TWOPIR)
                     IF (DIFF.GT.MXPH) THEN
                        RECR(RE1KOL+I-1) = FBLANK
                        RECR(IM1KOL+I-1) = FBLANK
                        RECR(WT1KOL+I-1) = 0.0
                        BAD = .TRUE.
                        NPHASE = NPHASE + 1
                        END IF
                     END IF
 130              CONTINUE
               END IF
C                                       Second polarization present?
            IF ((RA2KOL.GT.0) .AND. (WORK4(ITIME).NE.FBLANK) .AND.
     *         (WORK5(ITIME).NE.FBLANK) .AND. (WORK6(ITIME).NE.FBLANK))
     *         THEN
C                                       Smoothed phase same for all IFs.
               IF ((WORK5(ITIME).NE.FBLANK) .AND.
     *            (WORK4(ITIME).NE.FBLANK)) THEN
                  IPHASE = ATAN2(WORK5(ITIME), WORK4(ITIME)+1.0E-20) +
     *               WORK8(ITIME)
                  ELSE
                     IPHASE = FBLANK
                     END IF
               DO 140 I = IFBEG,IFEND
C                                       Clip amplitude
                  IF (DOAMP .AND. (RECR(RE2KOL+I-1).NE.FBLANK) .AND.
     *               (WORK6(ITIME).NE.FBLANK)) THEN
                     AMP = SQRT (RECR(RE2KOL+I-1)*RECR(RE2KOL+I-1)
     *                  + RECR(IM2KOL+I-1)*RECR(IM2KOL+I-1))
                     DIFF = ABS (AMP - WORK6(ITIME))
                     IF (DIFF.GT.MXAMP) THEN
                        RECR(RE2KOL+I-1) = FBLANK
                        RECR(IM2KOL+I-1) = FBLANK
                        RECR(WT2KOL+I-1) = 0.0
                        BAD = .TRUE.
                        NAMP = NAMP + 1
                        END IF
                     END IF
C                                       Clip phase
                  IF (DOPH .AND. (RECR(RE2KOL+I-1).NE.FBLANK) .AND.
     *               (IPHASE.NE.FBLANK)) THEN
                     PHASE = ATAN2(RECR(IM2KOL+I-1),
     *                  RECR(RE2KOL+I-1)+1.0E-20)
                     DIFF = ABS (PHASE - IPHASE)
                     DIFF = MOD (DIFF, TWOPIR)
                     IF (DIFF.GT.MXPH) THEN
                        RECR(RE2KOL+I-1) = FBLANK
                        RECR(IM2KOL+I-1) = FBLANK
                        RECR(WT2KOL+I-1) = 0.0
                        BAD = .TRUE.
                        NPHASE = NPHASE + 1
                        END IF
                     END IF
 140              CONTINUE
               END IF
C                                       Rewrite record
            IF (BAD) THEN
               CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 900
               NRECS = NRECS + 1
               END IF
 200        CONTINUE
C                                       First SN number of next antenna
         FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      IF (NRECS.GT.0) THEN
         WRITE (MSGTXT,1600) IFBEG, NAMP
         IF (NAMP.GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1601) IFBEG, NPHASE
         IF (NPHASE.GT.0) CALL MSGWRT (4)
         WRITE (MSGTXT,1602) IFBEG, NRECS
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1600 FORMAT ('IF',I3,' clipped',I10,' gains due to amplitude')
 1601 FORMAT ('IF',I3,' clipped',I10,' gains due to phase')
 1602 FORMAT ('IF',I3,' changed',I10,' table records due to clipping')
 1900 FORMAT ('CLPAPH: TABIO ERROR',I3,' CLIPPING AMP AND/OR PHASES')
      END
      SUBROUTINE SNREF (IRET)
C-----------------------------------------------------------------------
C   References the phase, delays and rates.
C   For 'VL??' smoothing only delays and rates are smoothed.
C   Leaves the output table sorted in time-antenna order.
C    Inputs from common:
C    Output:
C      IRET         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      CHARACTER COLHED(2)*24
      INTEGER   IRCODE, NUMREC, LOOPR, ANT, IIF, IREF, ANTUSE(MAXANT),
     *   REFCNT, KEY(2,2), NKEY, KOLS(2), ICLUN, ISNRNO,
     *   TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, MB1KOL, MB2KOL, RE1KOL,
     *   RE2KOL, IM1KOL, IM2KOL, RA1KOL, RA2KOL, DL1KOL, DL2KOL, WT1KOL,
     *   WT2KOL, RF1KOL, RF2KOL, SNANT, SNTIM, DI1KOL, DI2KOL,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   MIDIF, J, NREF, NSUM, KEYSUB(2,2)
      REAL      FKEY(2,2), GMMOD, RANOD(25), DECNOD(25), PERC
      LOGICAL   T, ISAPPL, WANT, SNWANT, DOREF(3)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
      DATA IRCODE /0/
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
C-----------------------------------------------------------------------
      IRET = 0
C                                        Open table
      ICLUN = 30
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,  NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNREF: SNINI FOR READ FAILS'
         GO TO 990
         END IF
C                                       Get column pointers
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      SOUKOL = SNKOLS(SNISID)
      FRQKOL = SNKOLS(SNIFQI)
      MB1KOL = SNKOLS(SNRMD1)
      DI1KOL = SNKOLS(SNRDS1)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      RF1KOL = SNKOLS(SNIRF1)
      MB2KOL = SNKOLS(SNRMD2)
      DI2KOL = SNKOLS(SNRDS2)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
      RF2KOL = SNKOLS(SNIRF2)
C                                       Get column pointers for sort
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
         MSGTXT = 'SNREF: FNDCOL FOR READ FAILS'
         GO TO 990
         END IF
      IRET = 0
      SNTIM = KOLS(1)
      SNANT = KOLS(2)
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNREF: CLOSE TABLE FOR READ FAILS'
         GO TO 990
         END IF
C                                       Sort to time-antenna order.
      KEY(1,1) = SNTIM
      KEY(2,1) = SNTIM
      KEY(1,2) = SNANT
      KEY(2,2) = SNANT
      IF (((BUFFER(43).NE.TIMKOL) .OR. (BUFFER(44).NE.ANTKOL))) THEN
         CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SNREF: SORT FAILS'
            GO TO 990
            END IF
         END IF
C                                       Reopen write
      CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNREF: SNINI FOR WRITE FAILS'
         GO TO 990
         END IF
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
C                                       Fix data flagging in table
      CALL RFFXFG (NUMIF, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL,
     *   RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, DOREF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Determine which antennas used as
C                                       reference antennas.
      CALL FILL (NUMANT, 0, ANTUSE)
      DO 20 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 20
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (WANT) THEN
C                                       Loop over IFs
            DO 10 IIF = 1,NUMIF
               IF (RECR(RE1KOL+IIF-1).NE.FBLANK) THEN
                  IREF = RECORD(RF1KOL+IIF-1)
                  IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT))
     *               ANTUSE(IREF) = ANTUSE(IREF) + 1
                  END IF
               IF ((RE2KOL.GT.0) .AND. (RECR(RE2KOL+IIF-1).NE.FBLANK))
     *            THEN
                  IREF = RECORD(RF2KOL+IIF-1)
                  IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT))
     *               ANTUSE(IREF) = ANTUSE(IREF) + 1
                  END IF
 10            CONTINUE
            END IF
 20      CONTINUE
C                                       Compute percentage of table
C                                       entries that need to be
C                                       re-referenced.
      NREF = 0
      NSUM = 0
      DO 100 J = 1, NUMANT
         IF ((ANTUSE(J).GT.0).AND.(J.NE.REFANT)) NREF = NREF + ANTUSE(J)
         NSUM = NSUM + ANTUSE(J)
100      CONTINUE
      PERC = FLOAT (NREF) / MAX (1, NSUM) * 100.0
C                                       Determine reference antenna if
C                                       necessary.
      IF (REFANT.LE.0) THEN
         REFANT = 1
         REFCNT = ANTUSE(REFANT)
         DO 30 ANT = 2,NUMANT
            IF (ANTUSE(ANT).GT.REFCNT) THEN
               REFANT = ANT
               REFCNT = ANTUSE(REFANT)
               END IF
 30         CONTINUE
         END IF
C                                       Message about rereferencing.
      WRITE (MSGTXT,1030) PERC, REFANT
      CALL MSGWRT (4)
C                                       Loop through antennas used as
C                                       secondary reference antennas.
      DO 500 ANT = 1,NUMANT
         IF ((ANTUSE(ANT).LE.0) .OR. (ANT.EQ.REFANT)) GO TO 500
C                                       Reference delay and rate.
C                                       Rates
         IF (DOREF(3)) THEN
            CALL REFRAT (REFANT, ANT, NUMIF, TIMKOL, SUBKOL, ANTKOL,
     *         SOUKOL, FRQKOL, RF1KOL, RA1KOL, WT1KOL, RF2KOL, RA2KOL,
     *         WT2KOL, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
C                                       Multiband delays, use middle IF
         MIDIF = (EIF+BIF) / 2
         CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *      FRQKOL, RF1KOL+MIDIF-1, MB1KOL, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (MB2KOL.GT.0)
     *      CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *      FRQKOL, RF2KOL+MIDIF-1, MB2KOL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       dispersions
         CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *      FRQKOL, RF1KOL+MIDIF-1, DI1KOL, IRET)
         IF (IRET.NE.0) GO TO 999
         IF (DI2KOL.GT.0)
     *      CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *      FRQKOL, RF2KOL+MIDIF-1, DI2KOL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Loop through single band
C                                       delays.
         IF (DOREF(2)) THEN
            DO 200 IIF = BIF,EIF
C                                        Delays
               CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *            FRQKOL, RF1KOL+IIF-1, DL1KOL+IIF-1, IRET)
               IF (IRET.NE.0) GO TO 999
               IF (DL2KOL.GT.0)
     *            CALL REFDLY (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL,
     *               SOUKOL, FRQKOL, RF2KOL+IIF-1, DL2KOL+IIF-1, IRET)
               IF (IRET.NE.0) GO TO 999
 200           CONTINUE
            END IF
C                                       Reference phase
         IF (DOREF(1)) THEN
            DO 300 IIF = BIF,EIF
               CALL REFFAZ (REFANT, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *            FRQKOL, RF1KOL, RE1KOL, IM1KOL, WT1KOL, RF2KOL,
     *            RE2KOL, IM2KOL, WT2KOL, IIF, IRET)
               IF (IRET.NE.0) GO TO 999
 300           CONTINUE
            END IF
C                                       End of antenna loop
 500     CONTINUE
C                                       Blank any entries that could
C                                       not be re-referenced
      CALL RFFIX (REFANT, BIF, EIF, RE1KOL, IM1KOL, DL1KOL, RA1KOL,
     *   WT1KOL, RF1KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL,
     *   RF2KOL, DOREF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNREF: CLOSE TABLE FOR WRITE FAILS'
         GO TO 990
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFANT
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('SNREF: re-referencing',F7.2,'% of table entries',
     *   ' to antenna ',I3)
 1900 FORMAT ('SNREF: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE RFFXFG (NUMIF, RE1KOL, IM1KOL, DE1KOL, RA1KOL, WT1KOL,
     *   RE2KOL, IM2KOL, DE2KOL, RA2KOL, WT2KOL, DOREF, IRET)
C-----------------------------------------------------------------------
C   Routine to assure that all entries for bad IF/poln are flagged.  If
C   any of real, imag, delay, rate or weight indicate a bad solution
C   then all are flagged.
C   Inputs:
C      NUMIF    I  The number of IFs
C      RE1KOL   I  Real pol 1  column pointer
C      IM1KOL   I  Imaginary pol 1  column pointer
C      DE1KOL   I  SB Delay pol 1  column pointer
C      RA1KOL   I  Rate pol 1  column pointer
C      WT1KOL   I  Weight pol 1  column pointer
C      RE2KOL   I  Real pol 2  column pointer, >0 => not present.
C      IM2KOL   I  Imaginary pol 2  column pointer
C      DE2KOL   I  SB Delay pol 2  column pointer
C      RA2KOL   I  Rate pol 2  column pointer
C      WT2KOL   I  Weight pol 2  column pointer
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      DOREF    L(3)  There are phases, rates, delays
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   NUMIF, RE1KOL, IM1KOL, DE1KOL, RA1KOL, WT1KOL, RE2KOL,
     *   IM2KOL, DE2KOL, RA2KOL, WT2KOL, IRET
      LOGICAL   DOREF(3)
C
      INTEGER   LOOPR, IRCODE, NUMREC, ISNRNO, I
      REAL      XIM, XDE, XRA
      LOGICAL   BAD, BAD2, WRIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      DOREF(1) = .FALSE.
      DOREF(2) = .FALSE.
      DOREF(3) = .FALSE.
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      XIM = 0.0
      XDE = 0.0
      XRA = 0.0
C                                       Loop through table
      DO 100 LOOPR = 1,NUMREC
         ISNRNO = LOOPR
         CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 100
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       Check if flagged
         WRIT = .FALSE.
         DO 20 I = 1,NUMIF
            BAD = (RECR(WT1KOL+I-1).LE.0.0)  .OR.
     *         (RECR(RE1KOL+I-1).EQ.FBLANK)  .OR.
     *         (RECR(IM1KOL+I-1).EQ.FBLANK)  .OR.
     *         (RECR(DE1KOL+I-1).EQ.FBLANK)  .OR.
     *         (RECR(RA1KOL+I-1).EQ.FBLANK)
            IF (BAD) THEN
               RECR(WT1KOL+I-1) = 0.0
               RECR(RE1KOL+I-1) = FBLANK
               RECR(IM1KOL+I-1) = FBLANK
               RECR(DE1KOL+I-1) = FBLANK
               RECR(RA1KOL+I-1) = FBLANK
               WRIT = .TRUE.
            ELSE
               XIM = MAX (XIM, ABS (RECR(IM1KOL+I-1)))
               XDE = MAX (XDE, ABS (RECR(DE1KOL+I-1)))
               XRA = MAX (XRA, ABS (RECR(RA1KOL+I-1)))
               END IF
C                                       Second poln?
            IF (RE2KOL.GT.0) THEN
               BAD2 = (RECR(WT2KOL+I-1).LE.0.0) .OR.
     *            (RECR(RE2KOL+I-1).EQ.FBLANK)  .OR.
     *            (RECR(IM2KOL+I-1).EQ.FBLANK)  .OR.
     *            (RECR(DE2KOL+I-1).EQ.FBLANK)  .OR.
     *            (RECR(RA2KOL+I-1).EQ.FBLANK)
               IF (BAD2) THEN
                  RECR(WT2KOL+I-1) = 0.0
                  RECR(RE2KOL+I-1) = FBLANK
                  RECR(IM2KOL+I-1) = FBLANK
                  RECR(DE2KOL+I-1) = FBLANK
                  RECR(RA2KOL+I-1) = FBLANK
                  WRIT = .TRUE.
               ELSE
                  XIM = MAX (XIM, ABS (RECR(IM2KOL+I-1)))
                  XDE = MAX (XDE, ABS (RECR(DE2KOL+I-1)))
                  XRA = MAX (XRA, ABS (RECR(RA2KOL+I-1)))
                  END IF
               END IF
 20         CONTINUE
C                                       Rewrite record
         IF (WRIT) THEN
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 100     CONTINUE
      DOREF(1) = XIM.GT.0.0
      DOREF(2) = XDE.GT.0.0
      DOREF(3) = XRA.GT.0.0
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('RFFXFG: TABIO ERROR',I3,' SMOOTHING DELAYS')
      END
      SUBROUTINE RFFIX (REFA, IFB, IFE, RE1KOL, IM1KOL, DE1KOL, RA1KOL,
     *   WT1KOL, RF1KOL, RE2KOL, IM2KOL, DE2KOL, RA2KOL, WT2KOL,
     *   RF2KOL, DOREF, IRET)
C-----------------------------------------------------------------------
C   Flag any remaining entries that could not be re-referenced to
C   prevent their inclusion in subsequent smoothing.
C   If there was no phase, just reset the ref ants.
C   Inputs:
C      REFA     I  Primary reference antenna
C      IFB      I  Start IF
C      IFE      I  End IF
C      RE1KOL   I  Real pol 1  column pointer
C      IM1KOL   I  Imaginary pol 1  column pointer
C      DE1KOL   I  SB Delay pol 1  column pointer
C      RA1KOL   I  Rate pol 1  column pointer
C      WT1KOL   I  Weight pol 1  column pointer
C      RF1KOL   I  Reference antenna pol 1 column pointer
C      RE2KOL   I  Real pol 2  column pointer, >0 => not present.
C      IM2KOL   I  Imaginary pol 2  column pointer
C      DE2KOL   I  SB Delay pol 2  column pointer
C      RA2KOL   I  Rate pol 2  column pointer
C      WT2KOL   I  Weight pol 2  column pointer
C      RF2KOL   I  Reference antenna pol 2 column pointer
C      DOREF    L(3) Rereference needed in phase, delay, rate
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER  REFA, IFB, IFE, RE1KOL, IM1KOL, DE1KOL, RA1KOL, WT1KOL,
     *   RF1KOL, RE2KOL, IM2KOL, DE2KOL, RA2KOL, WT2KOL, RF2KOL, IRET
      LOGICAL   DOREF(3)
C
      INTEGER   LOOPR, IRCODE, NUMREC, ISNRNO, I, NBLNK
      LOGICAL   BAD, BAD2, WRIT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      NBLNK = 0
C                                       Loop through table
      DO 100 LOOPR = 1,NUMREC
         ISNRNO = LOOPR
         CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 100
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       Blank if not re-referenced.
         WRIT = .FALSE.
         DO 20 I = IFB,IFE
            BAD = (RECR(WT1KOL+I-1).LE.0.0).OR.
     *         (RECR(RE1KOL+I-1).EQ.FBLANK).OR.
     *         (RECR(IM1KOL+I-1).EQ.FBLANK)
            IF (RECORD(RF1KOL+I-1).NE.REFA) THEN
               IF (DOREF(1)) THEN
                  IF (.NOT.BAD) NBLNK = NBLNK + 1
                  RECR(WT1KOL+I-1) = 0.0
                  RECR(RE1KOL+I-1) = FBLANK
                  RECR(IM1KOL+I-1) = FBLANK
                  RECR(DE1KOL+I-1) = FBLANK
                  RECR(RA1KOL+I-1) = FBLANK
                  END IF
               RECORD(RF1KOL+I-1) = REFA
               WRIT = .TRUE.
               END IF
C                                       Second poln?
            IF (RE2KOL.GT.0) THEN
               BAD2 = (RECR(WT2KOL+I-1).LE.0.0).OR.
     *            (RECR(RE2KOL+I-1).EQ.FBLANK).OR.
     *            (RECR(IM2KOL+I-1).EQ.FBLANK)

               IF (RECORD(RF2KOL+I-1).NE.REFA) THEN
                  IF (DOREF(1)) THEN
                     IF (.NOT.BAD2) NBLNK = NBLNK + 1
                     RECR(WT2KOL+I-1) = 0.0
                     RECR(RE2KOL+I-1) = FBLANK
                     RECR(IM2KOL+I-1) = FBLANK
                     RECR(DE2KOL+I-1) = FBLANK
                     RECR(RA2KOL+I-1) = FBLANK
                     END IF
                  RECORD(RF2KOL+I-1) = REFA
                  WRIT = .TRUE.
                  END IF
               END IF
 20         CONTINUE
C                                       Rewrite record
         IF (WRIT) THEN
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 100     CONTINUE
C                                       Report blanked entries
      IF (NBLNK.GT.0) THEN
         WRITE (MSGTXT,1100) NBLNK
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('RFFIX:',I8,' entries blanked in re-referencing')
 1900 FORMAT ('RFFIX: TABIO ERROR',I3,' SMOOTHING DELAYS')
      END
      SUBROUTINE REFDLY (REFA, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, REFKOL, DLYKOL, IRET)
C-----------------------------------------------------------------------
C   Routine to rereference a delay like column in an open table.
C   Inputs:
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      SOUKOL   I  Source ID column pointer
C      FRQKOL   I  FQ id column pointer
C      REFKOL   I  Reference antenna pointer
C      DLYKOL   I  Delay  column pointer
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   REFA, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *   REFKOL, DLYKOL, IRET
C
      INTEGER   LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, NUMREC
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      REAL      WT1, WT2, SMOTIM, DLY1, DLY2
      LOGICAL   WANT, SNWANT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Loop thru table referring ANT
C                                       to REFA.
      NUMTIM = 0
      NUMREC = BUFFER(5)
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 100
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 100
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFA))
     *      GO TO 100
         IF ((RECORD(REFKOL).NE.ANT) .AND. (RECORD(REFKOL).NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.RECORD(REFKOL)) GO TO 100
         IF (RECR(DLYKOL).EQ.FBLANK) GO TO 100
         IF (NUMTIM.LT.MXTIME) THEN
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
C                                       REFA is reference ant
            IF (RECORD(REFKOL).NE.ANT) THEN
               WORK3(NUMTIM) = RECR(DLYKOL)
C                                       ANT is reference ant
            ELSE
               WORK3(NUMTIM) = -RECR(DLYKOL)
               END IF
            IF (DOBTWN.LE.0.0) THEN
               WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
            ELSE
               WRKSRC(NUMTIM) = -1
               END IF
            END IF
 100     CONTINUE
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth (2 sec to extrapolate)
C                                       This doesn't really do anything
      SMOTIM = 2.0 / 86400.0
      CALL BOXBSM (SMOTIM, WRKTIM, WORK3, WRKSRC, FBLANK, NUMTIM, WORK2)
C                                       Set up for interpolation
C                                       Note - there are no blanks
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
      IF (NUMTIM.EQ.1) THEN
         IPNT2 = 1
         TIME2 = TIME1
         END IF
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFA
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 200
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 200
         IF (RECORD(REFKOL).NE.ANT) GO TO 200
         IF (RECR(DLYKOL).EQ.FBLANK) GO TO 190
C                                       Interpolate
         TIME = RECD(TIMKOL) - TIMOFF
 140     CONTINUE
C                                       Between entries
            IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
               IF (TIME2.NE.TIME1) THEN
                  WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
               ELSE
                  WT1 = 1.0
                  END IF
C                                       Before first time
            ELSE IF (TIME.LT.TIME1) THEN
               WT1 = 1.0
C                                       After last time
            ELSE IF (IPNT2.GE.NUMTIM) THEN
               WT1 = 0.0
C                                       Shift in interpolation arrays
            ELSE
               IPNT1 = IPNT1 + 1
               TIME1 = WRKTIM(IPNT1)
               IPNT2 = IPNT2 + 1
               TIME2 = WRKTIM(IPNT2)
               GO TO 140
               END IF
C                                       Interpolate
         WT2 = 1.0 - WT1
         DLY1 = WORK2(IPNT1)
         DLY2 = WORK2(IPNT2)
         RECR(DLYKOL) = RECR(DLYKOL) + WORK2(IPNT1) * WT1 +
     *      WORK2(IPNT2) * WT2
C                                       Rewrite record
 190     CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFDLY: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE REFRAT (REFA, ANT, NUMIF, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RF1KOL, RA1KOL, WT1KOL, RF2KOL, RA2KOL, WT2KOL,
     *   IRET)
C-----------------------------------------------------------------------
C   Routine to rereference rates in an open table. Rates are averaged
C   over IF and polarization.  If possible only table entries with valid
C   entries in both polarizations are used to rereference data.
C   Inputs:
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      NUMIF    I  The number of IFs
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      SOUKOL   I  Source ID column pointer
C      FRQKOL   I  FQ id column pointer
C      RF1KOL   I  Reference antenna 1 pointer
C      RA1KOL   I  Rate 1  column pointer
C      WT1KOL   I  Weight 1 column pointer.
C      RF2KOL   I  Reference antenna 2 pointer <1 => not present
C      RA2KOL   I  Rate 2  column pointer <1 => not present
C      WT2KOL   I  Weight 2 column pointer <1 => not present
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   REFA, ANT, NUMIF, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, RF1KOL, RA1KOL, WT1KOL, RF2KOL, RA2KOL, WT2KOL, IRET
C
      INTEGER    LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, REFA1, REFA2, I,
     *   NUMREC
      LOGICAL   NEED2, WANT, SNWANT
      REAL    WT1, WT2, SUM, COUNT, RATE, RATE1, RATE2, SMOTIM
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Initially require both
C                                       polarizations if present
      NEED2 = RA2KOL.GT.0
C                                       Loop thru table referring ANT
C                                       to REFA.
      NUMREC = BUFFER(5)
 10   NUMTIM = 0
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 100
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 100
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFA))
     *      GO TO 100
C                                        Find and check reference
C                                        antennas.  Must all be the
C                                        same.
         REFA1 = -1
         REFA2 = -1
C                                        Also accumulate rate.
         SUM = 0.0
         COUNT = 0.0
         DO 20 I = 1,NUMIF
            IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *         (RECR(RA1KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF1KOL+I-1).GT.0)) THEN
               IF (REFA1.LE.0) REFA1 = RECORD(RF1KOL+I-1)
               IF (REFA1.NE.RECORD(RF1KOL+I-1)) GO TO 100
               SUM = SUM + RECR(RA1KOL+I-1) * FREQS(I)
               COUNT = COUNT + 1
               END IF
            IF ((RA2KOL.GT.0) .AND. (RECR(WT2KOL+I-1).GT.0.0) .AND.
     *         (RECR(RA2KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF2KOL+I-1).GT.0)) THEN
               IF (REFA2.LE.0) REFA2 = RECORD(RF2KOL+I-1)
               IF (REFA2.NE.RECORD(RF2KOL+I-1)) GO TO 100
               SUM = SUM + RECR(RA2KOL+I-1) * FREQS(I)
               COUNT = COUNT + 1
               END IF
 20            CONTINUE
C                                       Desired antenna combination?
         IF (NEED2 .AND. (REFA1.GT.0) .AND. (REFA2.GT.0) .AND.
     *      (REFA1.NE.REFA2)) GO TO 100
         IF (REFA1.LT.0) REFA1 = REFA2
         IF ((REFA1.NE.ANT) .AND. (REFA1.NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.REFA1) GO TO 100
         IF (COUNT.LE.0) GO TO 100
         IF (NUMTIM.LT.MXTIME) THEN
            RATE = SUM / COUNT
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
C                                       REFA is reference ant
            IF (REFA1.NE.ANT) THEN
               WORK3(NUMTIM) = RATE
C                                       ANT is reference ant
            ELSE
               WORK3(NUMTIM) = -RATE
               END IF
            IF (DOBTWN.LE.0.0) THEN
               WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
            ELSE
               WRKSRC(NUMTIM) = -1
               END IF
            END IF
 100     CONTINUE
C                                       Try again with only one poln.
      IF (NEED2 .AND. (NUMTIM.LE.0)) THEN
         NEED2 = .FALSE.
         GO TO 10
         END IF
C                                       Find any?
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth (2 sec to extrapolate)
      SMOTIM = 2.0 / 86400.0
      CALL BOXBSM (SMOTIM, WRKTIM, WORK3, WRKSRC, FBLANK, NUMTIM, WORK2)
C                                       Set up for interpolation
C                                       Note - there are no blanks
      IPNT1 = 1
      IPNT2 = 2
      IF (NUMTIM.EQ.1) IPNT2 = 1
      TIME1 = WRKTIM(IPNT1)
      TIME2 = WRKTIM(IPNT2)
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFA
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 200
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 200
         WANT = .FALSE.
         DO 130 I = 1,NUMIF
            WANT = WANT .OR. ((RECORD(RF1KOL).EQ.ANT)
     *         .OR. ((RF2KOL.GT.0) .AND. (RECORD(RF2KOL).EQ.ANT)))
 130        CONTINUE
         IF (.NOT.WANT) GO TO 200
C                                       Interpolate
         TIME = RECD(TIMKOL) - TIMOFF
 140     CONTINUE
C                                       Between entries
            IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
               IF (TIME2.NE.TIME1) THEN
                  WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
               ELSE
                  WT1 = 1.0
                  END IF
C                                       Before first time
            ELSE IF (TIME.LT.TIME1) THEN
               WT1 = 1.0
C                                       After last time
            ELSE IF (IPNT2.GE.NUMTIM) THEN
               WT1 = 0.0
C                                       Shift in interpolation arrays
            ELSE
               IPNT1 = IPNT1 + 1
               IPNT2 = IPNT2 + 1
               TIME1 = WRKTIM(IPNT1)
               TIME2 = WRKTIM(IPNT2)
               GO TO 140
               END IF
C                                       Interpolate
         WT2 = 1.0 - WT1
         RATE1 = WORK2(IPNT1)
         RATE2 = WORK2(IPNT2)
         RATE = WT1*RATE1 + WT2*RATE2
         DO 120 I = 1,NUMIF
            IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *         (RECR(RA1KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF1KOL+I-1).EQ.ANT))
     *         RECR(RA1KOL+I-1) = RECR(RA1KOL+I-1) + RATE / FREQS(I)
            IF ((RA2KOL.GT.0) .AND. (RECR(WT2KOL+I-1).GT.0.0)
     *         .AND.  (RECR(RA2KOL+I-1).NE.FBLANK) .AND.
     *         (RECORD(RF2KOL+I-1).EQ.ANT))
     *         RECR(RA2KOL+I-1) = RECR(RA2KOL+I-1) + RATE / FREQS(I)
 120        CONTINUE
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFRAT: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE REFFAZ (REFA, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, RF1KOL, RE1KOL, IM1KOL, WT1KOL, RF2KOL, RE2KOL, IM2KOL,
     *   WT2KOL, IFNO, IRET)
C-----------------------------------------------------------------------
C   Routine to phases in an a polarization coherent fashion; i.e. both
C   polarizations must be present (if possible) in data used to
C   determine the relative phase between the primary and secondary
C   reference antennas.  This routine does one IF at a time but both
C   polarizations (if present) are done.
C   Inputs:
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      SOUKOL   I  Source ID column pointer
C      FRQKOL   I  FQid column pointer
C      RF1KOL   I  Reference antenna 1 pointer
C      RE1KOL   I  Real 1  column pointer
C      IM1KOL   I  Imaginary 1  column pointer
C      WT1KOL   I  Weight 1 column pointer.
C      RF2KOL   I  Reference antenna 2 pointer <1 => not present
C      RE2KOL   I  Real 2  column pointer <1 => not present
C      IM2KOL   I  Imaginary 2  column pointer
C      WT2KOL   I  Weight 2 column pointer <1 => not present
C      IFNO     I  Current IF number
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   REFA, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *   RF1KOL, RE1KOL, IM1KOL, WT1KOL, RF2KOL, RE2KOL, IM2KOL, WT2KOL,
     *   IFNO, IRET
C
      INTEGER    LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, REFA1, REFA2,
     *   NUMREC, RF1COL, RE1COL, IM1COL, WT1COL, RF2COL, RE2COL,
     *   IM2COL, WT2COL
      LOGICAL   NEED2, WANT, SNWANT
      REAL    WT1, WT2, RE1, RE2, IM1, IM2, AMP, TRE, TIM, SMOTIM
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Initially require both
C                                       polarizations if present
      NEED2 = RE2KOL.GT.0
C                                       Correct pointers for IF offsets
      RF1COL = RF1KOL + IFNO - 1
      RE1COL = RE1KOL + IFNO - 1
      IM1COL = IM1KOL + IFNO - 1
      WT1COL = WT1KOL + IFNO - 1
      RF2COL = RF2KOL + IFNO - 1
      RE2COL = RE2KOL + IFNO - 1
      IM2COL = IM2KOL + IFNO - 1
      WT2COL = WT2KOL + IFNO - 1
C                                       Reset pointers for 2nd polzn.
C                                       to those of the first polzn. if
C                                       they are not needed.
      IF (.NOT.NEED2) THEN
         RF2COL = RF1COL
         RE2COL = RE1COL
         IM2COL = IM1COL
         WT2COL = WT1COL
         END IF
C                                       Loop thru table referring ANT
C                                       to REFA.
      NUMREC = BUFFER(5)
 10   NUMTIM = 0
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 100
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 100
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFA))
     *      GO TO 100
C                                        Find and check reference
C                                        antennas.  Must all be the
C                                        same.
         REFA1 = RECORD(RF1COL)
         REFA2 = RECORD(RF2COL)
C                                        Bad solution?
         IF (((RECR(WT1COL).LE.0.0) .OR. (RECR(RE1COL).EQ.FBLANK) .OR.
     *      (RECORD(RF1COL).LE.0))) GO TO 100
         IF (NEED2 .AND. ((RE2COL.GT.0) .AND. ((RECR(WT2COL).LE.0.0)
     *      .OR. (RECR(RE2COL).EQ.FBLANK) .OR.
     *      (RECORD(RF2COL).LE.0)))) GO TO 100
C                                       Desired antenna combination?
         IF (NEED2 .AND. (REFA1.GT.0) .AND. (REFA2.GT.0) .AND.
     *      (REFA1.NE.REFA2)) GO TO 100
         IF (REFA1.LT.0) REFA1 = REFA2
         IF ((REFA1.NE.ANT) .AND. (REFA1.NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.REFA1) GO TO 100
         IF (NUMTIM.LT.MXTIME) THEN
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
C                                       REFA is reference ant
            IF (REFA1.NE.ANT) THEN
               WORK2(NUMTIM) = RECR(RE1COL)
               WORK3(NUMTIM) = RECR(IM1COL)
               IF (NEED2) THEN
                  WORK4(NUMTIM) = RECR(RE2COL)
                  WORK5(NUMTIM) = RECR(IM2COL)
                  END IF
C                                       ANT is reference ant
            ELSE
               WORK2(NUMTIM) = RECR(RE1COL)
               WORK3(NUMTIM) = -RECR(IM1COL)
               IF (NEED2) THEN
                  WORK4(NUMTIM) = RECR(RE2COL)
                  WORK5(NUMTIM) = -RECR(IM2COL)
                  END IF
               END IF
            IF (DOBTWN.LE.0.0) THEN
               WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
            ELSE
               WRKSRC(NUMTIM) = -1
               END IF
            END IF
 100     CONTINUE
C                                       Try again with only one poln.
      IF (NEED2 .AND. (NUMTIM.LE.0)) THEN
         NEED2 = .FALSE.
         GO TO 10
         END IF
C                                       Find any?
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth (2 sec to extrapolate)
      SMOTIM = 2.0 / 86400.0
      CALL BOXBSM (SMOTIM, WRKTIM, WORK2, WRKSRC, FBLANK, NUMTIM, WORK1)
      CALL BOXBSM (SMOTIM, WRKTIM, WORK3, WRKSRC, FBLANK, NUMTIM, WORK2)
      IF (NEED2) THEN
         CALL BOXBSM (SMOTIM, WRKTIM, WORK4, WRKSRC, FBLANK, NUMTIM,
     *      WORK3)
         CALL BOXBSM (SMOTIM, WRKTIM, WORK5, WRKSRC, FBLANK, NUMTIM,
     *      WORK4)
         END IF
C                                       Set up for interpolation
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
      IF (NUMTIM.EQ.1) THEN
         IPNT2 = 1
         TIME2 = TIME1
         END IF
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFA
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) THEN
            IRET = 0
            GO TO 200
            END IF
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL), RECORD(ANTKOL),
     *      RECORD(SUBKOL), RECD(TIMKOL))
         IF (.NOT.WANT) GO TO 200
         IF ((RECORD(RF1COL).NE.ANT) .AND. (NEED2 .AND.
     *      (RECORD(RF2COL).NE.ANT))) GO TO 200
C                                       Interpolate
         TIME = RECD(TIMKOL) - TIMOFF
 140     CONTINUE
C                                       Between entries
            IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
               IF (TIME2.NE.TIME1) THEN
                  WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
               ELSE
                  WT1 = 1.0
                  END IF
C                                       Before first time
            ELSE IF (TIME.LT.TIME1) THEN
               WT1 = 1.0
C                                       After last time
            ELSE IF (IPNT2.GE.NUMTIM) THEN
               WT1 = 0.0
C                                       Shift in interpolation arrays
            ELSE
               IPNT1 = IPNT1 + 1
               TIME1 = WRKTIM(IPNT1)
               IPNT2 = IPNT2 + 1
               TIME2 = WRKTIM(IPNT2)
               GO TO 140
               END IF
C                                       Interpolate
         WT2 = 1.0 - WT1
         RE1 = WT1 * WORK1(IPNT1) + WT2 * WORK1(IPNT2)
         IM1 = WT1 * WORK2(IPNT1) + WT2 * WORK2(IPNT2)
         IF (NEED2) THEN
            RE2 = WT1 * WORK3(IPNT1) + WT2 * WORK3(IPNT2)
            IM2 = WT1 * WORK4(IPNT1) + WT2 * WORK4(IPNT2)
            END IF
C                                       Normalize amplitude to 1.0
         AMP = MAX (SQRT (RE1*RE1 + IM1*IM1), 1.0E-20)
         RE1 = RE1 / AMP
         IM1 = IM1 / AMP
         IF (NEED2) THEN
            AMP = MAX (SQRT (RE2*RE2 + IM2*IM2), 1.0E-20)
            RE2 = RE2 / AMP
            IM2 = IM2 / AMP
            END IF
C                                       Correct phase.
         IF ((RECR(WT1COL).GT.0.0) .AND. (RECR(RE1COL).NE.FBLANK) .AND.
     *         (RECORD(RF1COL).EQ.ANT)) THEN
            TRE = RECR(RE1COL)
            TIM = RECR(IM1COL)
            RECR(RE1COL) = TRE*RE1 - TIM*IM1
            RECR(IM1COL) = TRE*IM1 + TIM*RE1
C                                       Relabel reference antenna
            RECORD(RF1COL) = REFA
            END IF
C                                       Correct phase.
         IF (NEED2 .AND. (RECR(WT2COL).GT.0.0) .AND.
     *      (RECR(RE2COL).NE.FBLANK) .AND. (RECORD(RF2COL).EQ.ANT)) THEN
            TRE = RECR(RE2COL)
            TIM = RECR(IM2COL)
            RECR(RE2COL) = TRE*RE2 - TIM*IM2
            RECR(IM2COL) = TRE*IM2 + TIM*RE2
C                                       Relabel reference antenna
            RECORD(RF2COL) = REFA
            END IF
C                                       Rewrite record
         CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFFAZ: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
      SUBROUTINE SNSMOO (IRET)
C-----------------------------------------------------------------------
C   Smooths selected portions of SN tables.
C   Leaves the output table sorted in antenna-time order.
C    Inputs from common:
C      SNVER        I    Cal (SN) file version number.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C    Output:
C      IRET         I    Return code, 0=>OK, otherwise failed.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  COLHED(2)*24, KEYWRD*8
      INTEGER   KEY(2,2), ICLUN, SNANT, SNTIM, ISNRNO, NKEY, KOLS(2),
     *   SNNUMV(MAXSNC), SNKOLS(MAXSNC), NUMANT, NUMPOL, NUMIF, NPIF,
     *   I, NUMNOD, SUB, NUMSUB, TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL,
     *   MB1KOL, RE1KOL, IM1KOL, DL1KOL, RA1KOL, WT1KOL, MB2KOL, RE2KOL,
     *   IM2KOL, DL2KOL, RA2KOL, WT2KOL, DI1KOL, DI2KOL, IERR,
     *   KEYSUB(2,2), DELCOR
      LOGICAL   T, F, ISAPPL, DODELA, DORATE, DOAMP, DOPH, DOALIF,
     *   AVRATE, DONORM
      REAL      FKEY(2,2), GMMOD, RANOD(25), DECNOD(25), STDELA(3),
     *   STRATE(3), STAMP(3), STPH(3), STMBDE(3), CNTGN, TCNTGN, SUMGN,
     *   TSUMGN
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME ', 'ANTENNA NO. '/
      DATA T, F /.TRUE.,.FALSE./
      DATA ICLUN /30/
      DATA KEYWRD /'MGMOD   '/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Sort SN table to antenna-time.
C                                       Need col. pointers, sort order.
      CALL SNINI ('READ', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,  NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNSMOO: SNINI FOR READ ERROR'
         GO TO 990
         END IF
C                                       Get column pointers
      NKEY = 2
      CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.10)) THEN
         MSGTXT = 'SNSMOO: FNDCOL ERROR'
         GO TO 990
         END IF
      IRET = 0
      SNTIM = KOLS(1)
      SNANT = KOLS(2)
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      SOUKOL = SNKOLS(SNISID)
      FRQKOL = SNKOLS(SNIFQI)
      MB1KOL = SNKOLS(SNRMD1)
      DI1KOL = SNKOLS(SNRDS1)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      MB2KOL = SNKOLS(SNRMD2)
      DI2KOL = SNKOLS(SNRDS2)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNSMOO: CLOSE FOR READ ERROR'
         GO TO 990
         END IF
      KEY(1,1) = SNANT
      KEY(1,2) = SNTIM
C                                       Sort to antenna time order.
      IF (((BUFFER(43).NE.SNANT) .OR. (BUFFER(44).NE.SNTIM))) THEN
         CALL TABSRT (DISKIN, CNOIN, 'SN', SNVER, SNVER, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'SNSMOO: SORT ERROR'
            GO TO 990
            END IF
         END IF
C                                       Reopen write
      CALL SNINI ('WRIT', BUFFER, DISKIN, CNOIN, SNVER, CATBLK, ICLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNSMOO: SNINI FOR WRITE ERROR'
         GO TO 990
         END IF
      IF (IRET.NE.0) GO TO 999
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
C                                       Determine smoothing parameters.
      STAMP(1)  = XIPARM(1) / 24.0
      STPH(1)   = XIPARM(2) / 24.0
      STRATE(1) = XIPARM(3) / 24.0
      STDELA(1) = XIPARM(4) / 24.0
      STMBDE(1) = XIPARM(5) / 24.0
      STAMP(2)  = XIPARM(6) / 24.0
      STPH(2)   = XIPARM(7) / 24.0
      STRATE(2) = XIPARM(8) / 24.0
      STDELA(2) = XIPARM(9) / 24.0
      STMBDE(2) = XIPARM(10) / 24.0
      STAMP(3)  = CUTOFF
      STPH(3)   = CUTOFF
      STRATE(3) = CUTOFF
      STDELA(3) = CUTOFF
      STMBDE(3) = CUTOFF
      DODELA = F
      DORATE = F
      DOAMP = F
      DOPH = F
      DOALIF = F
      AVRATE = F
      DELCOR = -1
      IF (XSMO.EQ.'AMPL') THEN
         DOAMP = T
         CALL RCOPY (3, STAMP, STPH)
      ELSE IF (XSMO.EQ.'PHAS') THEN
         DOPH = T
         CALL RCOPY (3, STPH, STAMP)
      ELSE IF (XSMO.EQ.'BOTH') THEN
         DOAMP = T
         DOPH = T
      ELSE IF (XSMO.EQ.'FULL') THEN
         DOAMP = T
         DOPH = T
         DODELA = T
         DORATE = T
      ELSE IF (XSMO.EQ.'VLBI') THEN
         DOAMP = T
         DOPH = T
         DODELA = T
         DORATE = T
      ELSE IF (XSMO.EQ.'VLRI') THEN
         DOAMP = T
         DOPH = T
         DODELA = T
         DORATE = T
         AVRATE = T
      ELSE IF (XSMO.EQ.'VLMB') THEN
         DOAMP = T
         DOPH = T
         DODELA = T
         DORATE = T
         DOALIF = T
         AVRATE = T
         DELCOR = 1
      ELSE IF (XSMO.EQ.'VLDE') THEN
         DOAMP = T
         DOPH = T
         DODELA = T
         DORATE = T
         AVRATE = T
         DELCOR = 0
      ELSE IF (XSMO.EQ.'DELA') THEN
         DODELA = T
C                                        Unknown
      ELSE
         MSGTXT = 'SNSMOO: UNKNOWN SMOOTHING CODE = ' // XSMO
         IRET = 5
         GO TO 990
         END IF
C                                       Check DOBLANK
      IF ((XDOBLK.GE.0.0) .AND. (.NOT.(DOAMP .AND. DOPH))) THEN
         MSGTXT = 'DOBLANK >=0 INCOMPATIBLE WITH SMOTYPE = ' // XSMO
         IRET = 5
         GO TO 990
         END IF
C                                       Inform user of smoothing:
      MSGTXT ='SNSMTH: Smoothing SN table'
      CALL MSGWRT (4)
C                                       Gain normalization info
      SUMGN = 0.0
      CNTGN = 0.0
C                                       Loop over subarrays
      DO 200 SUB = 1,NUMSUB
C                                       Want this subarray?
         IF ((SUBA.GT.0) .AND. (SUB.NE.SUBA)) GO TO 200
C                                       Smooth delays
         IF (DODELA) THEN
C                                        Multiband delay
            CALL SMOMBD (STMBDE, SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL,
     *         ANTKOL, SOUKOL, FRQKOL, MB1KOL, MB2KOL, IRET)
            IF (IRET.NE.0) GO TO 999
C                                        dispersion
            CALL SMOMBD (STMBDE, SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL,
     *         ANTKOL, SOUKOL, FRQKOL, DI1KOL, DI2KOL, IRET)
            IF (IRET.NE.0) GO TO 999
C                                        Single band delay
            DO 60 I = BIF,EIF
               CALL SMODLY (STDELA, SUB, NUMANT, NUMPOL, FREQD(I),
     *            TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, DL1KOL+I-1,
     *            RE1KOL+I-1, IM1KOL+I-1, DL2KOL+I-1, RE2KOL+I-1,
     *            IM2KOL+I-1, IRET)
               IF (IRET.NE.0) GO TO 999
 60            CONTINUE
            END IF
C                                        Smooth rates
         IF (DORATE) THEN
            IF (AVRATE) THEN
               CALL SMORAT (STRATE, SUB, NUMANT, NUMPOL, NUMIF, FREQS,
     *            TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, RA1KOL,
     *            WT1KOL, RA2KOL, WT2KOL, IRET)
               IF (IRET.NE.0) GO TO 999
            ELSE
               DO 70 I = BIF,EIF
                  CALL SMORAT (STRATE, SUB, NUMANT, NUMPOL, 1, FREQS(I),
     *               TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, RA1KOL+I-1,
     *               WT1KOL+I-1, RA2KOL+I-1, WT2KOL+I-1, IRET)
                  IF (IRET.NE.0) GO TO 999
 70               CONTINUE
               END IF
            END IF
C                                       Smooth amp/phase
         IF (DOAMP.OR.DOPH) THEN
C                                        Average IFs or separately?
            IF (DOALIF) THEN
               NPIF = NUMIF
            ELSE IF (DELCOR.LT.0) THEN
               NPIF = 1
            ELSE
               NPIF = NUMIF / NPIECE
               END IF
            DO 100 I = BIF,EIF,NPIF
               CALL SMOAPH (STAMP, STPH, DELCOR,  DOAMP, DOPH, SUB,
     *            NUMANT, I, I+NPIF-1, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *            FRQKOL, MB1KOL, DL1KOL, RA1KOL, RE1KOL, IM1KOL,WT1KOL,
     *            MB2KOL, DL2KOL, RA2KOL, RE2KOL, IM2KOL,WT2KOL, TCNTGN,
     *            TSUMGN, IRET)
               IF (IRET.NE.0) GO TO 999
               SUMGN = SUMGN + TSUMGN
               CNTGN = CNTGN + TCNTGN
 100           CONTINUE
            END IF
 200     CONTINUE
C                                       Update GMMOD
      DONORM = ((XNORM.EQ.0.0) .AND. (ABS (GMMOD-1.0).GT.1.0E-5)) .OR.
     *   (XNORM.GT.0.0)
      IF ((DONORM) .AND. (CNTGN.GT.0.1)) THEN
         GMMOD = SUMGN / CNTGN
         CALL TABKEY ('WRIT', KEYWRD, 1, BUFFER, 1, GMMOD, 2, IERR)
         IF (IERR.NE.0) THEN
             MSGTXT = 'SNSMOO: UPDATE GMMDO KEYWORD ERROR'
             CALL MSGWRT (6)
             END IF
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ISNRNO, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'SNSMOO: CLOSE WRITE ERROR'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE SMOMBD (STIME, SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL,
     *   ANTKOL, SOUKOL, FRQKOL, DL1KOL, DL2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to smooth a multi-band delay column in an open table.
C   Inputs:
C      STIME    R(3)   Smoothing time (days)
C      SUB      I      Desired subarray
C      NUMANT   I      Number of antennas
C      NUMPOL   I      Number of polarizations (1 or 2)
C      TIMKOL   I      Time column pointer.
C      SUBKOL   I      Subarray column pointer
C      ANTKOL   I      Antenna column pointer
C      FRQKOL   I      FQ id column pointer
C      SOUKOL   I      Source ID column pointer
C      DL1KOL   I      Delay  column pointer poln. 1
C      DL2KOL   I      Delay  column pointer poln. 2
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I      Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STIME(*)
      INTEGER   SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, DL1KOL, DL2KOL, IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, ITIME
      LOGICAL   SLCTD, SNWANT, BAD, BAD2, WANT
      REAL      COUNT, SUMDIF
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
         COUNT = 0.0
         SUMDIF = 0.0
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND.
     *         (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND.
     *         (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       See if flagged value
               BAD = (RECR(DL1KOL).EQ.FBLANK)
               BAD2 = (NUMPOL.LE.1) .OR. (RECR(DL2KOL).EQ.FBLANK)
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                  ELSE
                     WORK2(NUMTIM) = RECR(DL1KOL)
                     END IF
                  IF (BAD2) THEN
                     WORK3(NUMTIM) = FBLANK
                  ELSE
                     WORK3(NUMTIM) = RECR(DL2KOL)
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               IF (.NOT. (BAD .OR. BAD2)) THEN
                  COUNT = COUNT + 1.0
                  SUMDIF = SUMDIF + WORK2(NUMTIM) - WORK3(NUMTIM)
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
         IF (NUMTIM.LE.0) GO TO 590
C                                       Smooth as requested
         CALL SNSMSM (XINTP, STIME, WRKTIM, WORK2, FBLANK,
     *      NUMTIM, WRKSRC, WORK1)
C                                       Second Poln?
         IF (NUMPOL.GT.1) THEN
            CALL SNSMSM (XINTP, STIME, WRKTIM, WORK3, FBLANK,
     *         NUMTIM, WRKSRC, WORK2)
            END IF
C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
C                                       Update if desired
            IF (RECR(DL1KOL).EQ.FBLANK) THEN
               IF (XDOBLK.GE.0.0) RECR(DL1KOL) = WORK1(ITIME)
            ELSE
               IF (XDOBLK.LE.0.0) RECR(DL1KOL) = WORK1(ITIME)
               END IF
C                                       Second polarization?
            IF (NUMPOL.GE.2) THEN
               IF (RECR(DL2KOL).EQ.FBLANK) THEN
                  IF (XDOBLK.GE.0.0) RECR(DL2KOL) = WORK2(ITIME)
               ELSE
                  IF (XDOBLK.LE.0.0) RECR(DL2KOL) = WORK2(ITIME)
                  END IF
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200        CONTINUE
 590     FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SMOMBD: TABIO ERROR',I3,' SMOOTHING DELAYS')
      END
      SUBROUTINE SMODLY (STIME, SUB, NUMANT, NUMPOL, FRQD, TIMKOL,
     *   SUBKOL, ANTKOL, SOUKOL, FRQKOL, DL1KOL, RE1KOL, IM1KOL, DL2KOL,
     *   RE2KOL, IM2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to smooth a delay like column in an open table.
C   Inputs:
C      STIME    R(3)   Smoothing time (days)
C      SUB      I      Desired subarray
C      NUMANT   I      Number of antennas
C      NUMPOL   I      Number of polarizations (1 or 2)
C      TIMKOL   I      Time column pointer.
C      SUBKOL   I      Subarray column pointer
C      ANTKOL   I      Antenna column pointer
C      FRQKOL   I      FQ id column pointer
C      SOUKOL   I      Source ID column pointer
C      DL1KOL   I      Delay  column pointer poln. 1
C      DL2KOL   I      Delay  column pointer poln. 2
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STIME(*)
      INTEGER   SUB, NUMANT, NUMPOL, TIMKOL, SUBKOL, ANTKOL, SOUKOL,
     *   FRQKOL, DL1KOL, RE1KOL, IM1KOL, DL2KOL, RE2KOL, IM2KOL, IRET
      DOUBLE PRECISION FRQD
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, ITIME
      LOGICAL   SLCTD, SNWANT, BAD, BAD2, WANT
      REAL      COUNT, SUMDIF, PHASE, REAL, IMAG
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
         COUNT = 0.0
         SUMDIF = 0.0
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND.
     *         (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND.
     *         (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       See if flagged value
               BAD = (RECR(DL1KOL).EQ.FBLANK)
               BAD2 = (NUMPOL.LE.1) .OR. (RECR(DL2KOL).EQ.FBLANK)
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                  ELSE
                     WORK2(NUMTIM) = RECR(DL1KOL)
                     END IF
                  IF (BAD2) THEN
                     WORK3(NUMTIM) = FBLANK
                  ELSE
                     WORK3(NUMTIM) = RECR(DL2KOL)
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               IF (.NOT. (BAD .OR. BAD2)) THEN
                  COUNT = COUNT + 1.0
                  SUMDIF = SUMDIF + WORK2(NUMTIM) - WORK3(NUMTIM)
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
         IF (NUMTIM.LE.0) GO TO 590
C                                       Smooth as requested
         CALL SNSMSM (XINTP, STIME, WRKTIM, WORK2, FBLANK,
     *      NUMTIM, WRKSRC, WORK1)
C                                       Second Poln?
         IF (NUMPOL.GT.1) THEN
            CALL SNSMSM (XINTP, STIME, WRKTIM, WORK3, FBLANK,
     *         NUMTIM, WRKSRC, WORK2)
            END IF
C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
C                                       Update if desired
C                                       and correct delay phase
            IF (RECR(DL1KOL).EQ.FBLANK) THEN
               IF (XDOBLK.GE.0.0) THEN
                  PHASE = TWOPI * WORK1(ITIME) * FRQD
                  RECR(RE1KOL) = COS(PHASE)
                  RECR(IM1KOL) = SIN(PHASE)
                  RECR(DL1KOL) = WORK1(ITIME)
                  END IF
            ELSE IF (XDOBLK.LE.0.0) THEN
               PHASE = TWOPI * (WORK1(ITIME) - RECR(DL1KOL)) * FRQD
               REAL = RECR(RE1KOL)
               IMAG = RECR(IM1KOL)
               RECR(RE1KOL) = REAL * COS(PHASE) - IMAG * SIN(PHASE)
               RECR(IM1KOL) = IMAG * COS(PHASE) + REAL * SIN(PHASE)
               RECR(DL1KOL) = WORK1(ITIME)
               END IF
C                                       Second polarization?
            IF (NUMPOL.GE.2) THEN
               IF (RECR(DL2KOL).EQ.FBLANK) THEN
                  IF (XDOBLK.GE.0.0) THEN
                     PHASE = TWOPI * WORK2(ITIME) * FRQD
                     RECR(RE2KOL) = COS(PHASE)
                     RECR(IM2KOL) = SIN(PHASE)
                     RECR(DL2KOL) = WORK2(ITIME)
                     END IF
               ELSE IF (XDOBLK.LE.0.0) THEN
                  PHASE = TWOPI * (WORK2(ITIME) - RECR(DL2KOL)) * FRQD
                  REAL = RECR(RE2KOL)
                  IMAG = RECR(IM2KOL)
                  RECR(RE2KOL) = REAL * COS(PHASE) - IMAG * SIN(PHASE)
                  RECR(IM2KOL) = IMAG * COS(PHASE) + REAL * SIN(PHASE)
                  RECR(DL2KOL) = WORK2(ITIME)
                  END IF
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200        CONTINUE
 590     FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SMODLY: TABIO ERROR',I3,' SMOOTHING DELAYS')
      END
      SUBROUTINE SMORAT (STIME, SUB, NUMANT, NUMPOL, NUMIF, FRQS,
     *   TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, RA1KOL, WT1KOL, RA2KOL,
     *   WT2KOL, IRET)
C-----------------------------------------------------------------------
C   Routine to smooth fringe rates in an open table.  All poln and IF
C   are averaged and smoothed together.
C   Input table must be in antenna-time order.
C   Inputs:
C      STIME    R(3)   Smoothing time (days)
C      SUB      I      Desired subarray
C      NUMANT   I      Number of antennas
C      NUMPOL   I      Number of polarizations
C      NUMIF    I      The number of IFs
C      TIMKOL   I      Time column pointer.
C      SUBKOL   I      Subarray column pointer
C      ANTKOL   I      Antenna column pointer
C      FRQKOL   I      FQ id column pointer
C      SOUKOL   I      Source ID column pointer
C      RA1KOL   I      Rate pol 1  column pointer
C      WT1KOL   I      Weight pol 1 column pointer.
C      RA2KOL   I      Rate pol 2  column pointer <1 => not present
C      WT2KOL   I      Weight pol 2 column pointer <1 => not present
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      IRET     I      Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STIME(*)
      INTEGER   SUB, NUMANT, NUMPOL, NUMIF, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, RA1KOL, WT1KOL, RA2KOL, WT2KOL, IRET
      DOUBLE PRECISION FRQS(*)
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, I, ITIME
      REAL      SUM, COUNT
      LOGICAL   SNWANT, SLCTD, BAD, WANT
      DOUBLE PRECISION TIMOFF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND.
     *         (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND.
     *         (RECORD(ANTKOL).EQ.ANT)
            IF (WANT) THEN
C                                       Average rate
               SUM = 0.0
               COUNT = 0.0
               DO 20 I = 1,NUMIF
                  IF ((RECR(WT1KOL+I-1).GT.0.0) .AND.
     *               (RECR(RA1KOL+I-1).NE.FBLANK)) THEN
                     SUM = SUM + RECR(RA1KOL+I-1) * FRQS(I)
                     COUNT = COUNT + 1
                     END IF
                  IF ((NUMPOL.GT.1) .AND. (RECR(WT2KOL+I-1).GT.0.0)
     *               .AND. (RECR(RA2KOL+I-1).NE.FBLANK)) THEN
                     SUM = SUM + RECR(RA2KOL+I-1) * FRQS(I)
                     COUNT = COUNT + 1
                     END IF
 20               CONTINUE
C                                       See if flagged value
               BAD = COUNT .LE. 0.1
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                  ELSE
                     WORK2(NUMTIM) = SUM / COUNT
                     END IF
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
C                                       Smooth as requested
         CALL SNSMSM (XINTP, STIME, WRKTIM, WORK2, FBLANK, NUMTIM,
     *      WRKSRC, WORK1)
C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
C                                       Update
            IF (WORK1(ITIME).NE.FBLANK) THEN
               DO 120 I = 1,NUMIF
C                                       Desired?
                  IF (RECR(RA1KOL+I-1).EQ.FBLANK) THEN
                     IF (XDOBLK.GE.0.0) THEN
                        RECR(RA1KOL+I-1) = WORK1(ITIME) / FRQS(I)
                        RECR(WT1KOL+I-1) = MAX (0.01, RECR(WT1KOL+I-1))
                        END IF
                  ELSE
                     IF (XDOBLK.LE.0.0) RECR(RA1KOL+I-1) = WORK1(ITIME)
     *                  / FRQS(I)
                     END IF
C                                       Second polarization present?
                  IF (NUMPOL.GT.1) THEN
                     IF (RECR(RA2KOL+I-1).EQ.FBLANK) THEN
                        IF (XDOBLK.GE.0.0) THEN
                           RECR(RA2KOL+I-1) = WORK1(ITIME) / FRQS(I)
                           RECR(WT2KOL+I-1) = MAX (0.01,
     *                        RECR(WT2KOL+I-1))
                           END IF
                     ELSE
                        IF (XDOBLK.LE.0.0) RECR(RA2KOL+I-1) =
     *                     WORK1(ITIME) / FRQS(I)
                        END IF
                     END IF
 120              CONTINUE
C                                       Rewrite record
               CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 900
               END IF
 200        CONTINUE
C                                       First SN number of next antenna
         FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SMORAT: TABIO ERROR',I3,' SMOOTHING RATES')
      END
      SUBROUTINE SMOAPH (STAMP, STPH, DELCOR, DOAMP, DOPH, SUB, NUMANT,
     *   IFBEG, IFEND, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL, MB1KOL,
     *   DE1KOL, RA1KOL, RE1KOL, IM1KOL, WT1KOL, MB2KOL, DE2KOL, RA2KOL,
     *   RE2KOL, IM2KOL, WT2KOL, GNCNT, GNSUM, IRET)
C-----------------------------------------------------------------------
C   Routine to smooth amplitudes and/or phases rates in an open table.
C   All poln present and the range of IF specified by IFBEG and IFEND
C   are smoothed jointly.  The values in a single polarization are
C   averaged after correcting for multiband delay.
C      Any delay and rate smoothing should be done before amplitude and
C   phase smoothing.  Any blanked delay and rate values will be set
C   to 0.0.
C      The phases are corrected by the integral of the rate functions
C   from the first time before smoothing.   All selected phases in each
C   polarization are averaged and corrected using the integrated phase
C   function for the first IF selected.
C   Input table must be in antenna-time order.
C   Inputs:
C      STAMP    R(3)   Amplitude smoothing time (days)
C      STPH     R(3)   Phase smoothing time (days)
C      DELCOR   I      Correct phase by MD (1) or SB (0) delay or no (-1)
C      DOAMP    L      Smooth amplitudes?
C      DOPH     L      Smooth phases?
C      SUB      I      Desired subarray
C      NUMANT   I      Number of antennas
C      IFBEG    I      First IF
C      IFEND    I      Highest IF
C      TIMKOL   I      Time column pointer.
C      SUBKOL   I      Subarray column pointer
C      ANTKOL   I      Antenna column pointer
C      FRQKOL   I      FQ id column pointer
C      SOUKOL   I      Source ID column pointer
C      MB1KOL   I      Multiband delay 1 column pointer
C      DE1KOL   I      Singleband delay 1 column pointer
C      RA1KOL   I      Rate pol 1  column pointer
C      RE1KOL   I      Real 1  column pointer
C      IM1KOL   I      Imaginary 1  column pointer
C      WT1KOL   I      Weight 1 column pointer.
C      MB2KOL   I      Multiband delay 2 column pointer <1 => not present
C      DE2KOL   I      Singleband delay 2 column pointer <1 => not present
C      RA2KOL   I      Rate pol 2 pointer <1 => not present
C      RE2KOL   I      Real 2  column pointer <1 => not present
C      IM2KOL   I      Imaginary 2  column pointer
C      WT2KOL   I      Weight 2 column pointer <1 => not present
C   Inputs from common:
C      I/O buffer etc.
C      Other data selection criteria.
C   Output:
C      GNCNT    R      Count for gain normalization
C      GNSUM    R      Sum of gain modulii
C      IRET     I      Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      REAL      STAMP(*), STPH(*), GNCNT, GNSUM
      LOGICAL   DOAMP, DOPH
      INTEGER   DELCOR, SUB, NUMANT, IFBEG, IFEND, TIMKOL, SUBKOL,
     *   ANTKOL, SOUKOL, FRQKOL, MB1KOL, DE1KOL, RA1KOL, RE1KOL, IM1KOL,
     *   WT1KOL, MB2KOL, DE2KOL, RA2KOL, RE2KOL, IM2KOL, WT2KOL, IRET
C
      INTEGER   LOOPR, LOOPA, IRCODE, NUMTIM, ANT, NUMREC, FSTREC,
     *   NLEFT, SAVE, ISNRNO, I, ITIME, NUGOOD
      REAL      SUMRE1, SUMIM1, SUMRE2, SUMIM2, COUNT1, COUNT2,
     *   IPHASE, CPH, RATE, IPRE, IPIM, AMP, PHASE, MBPHAS, MBRE,
     *   MBIM, SUMAM1, SUMAM2
      LOGICAL   SLCTD, SNWANT, BAD, BAD2, WANT, SMOPHS
      DOUBLE PRECISION TIMOFF, INTFAZ, PHADD, LSTIME, TWOPI
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      TWOPI = 8.0D0 * ATAN (1.0D0)
      GNCNT = 0.0
      GNSUM = 0.0
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IRET = 8
      IF (NUMREC.LE.0) GO TO 999
      FSTREC = 0
C                                       Loop over antenna
      DO 600 LOOPA = 1,NUMANT
         ANT = LOOPA
C                                       Want this antenna?
         IF (.NOT.SLCTD (ANT, ANTENS, NANTSL, DOAWNT)) GO TO 600
C                                       Set pointers, counters
         NUMTIM = 0
         NLEFT = NUMREC - FSTREC
         NUGOOD = 0
C                                       Integrated phase function
         INTFAZ = 0.0D0
         LSTIME = 0.0
C                                       Loop in time, reading
         DO 100 LOOPR = 1,NLEFT
            ISNRNO = FSTREC + LOOPR
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 100
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Finished antenna?
            IF (RECORD(ANTKOL).GT.ANT) GO TO 110
C                                       See if wanted.
            WANT = SNWANT (RECORD(SOUKOL), RECORD(FRQKOL),
     *         RECORD(ANTKOL), RECORD(SUBKOL), RECD(TIMKOL))
C                                       Check subarray
            WANT = WANT .AND. (RECORD(SUBKOL).EQ.SUB)
C                                       Not all antennas wanted
            WANT = WANT .AND. (RECORD(ANTKOL).EQ.ANT)
C                                       Average phases
            IF (WANT) THEN
               SUMRE1 = 0.0
               SUMIM1 = 0.0
               COUNT1 = 0.0
               SUMRE2 = 0.0
               SUMIM2 = 0.0
               COUNT2 = 0.0
               SUMAM1 = 0.0
               SUMAM2 = 0.0
               RATE = FBLANK
               DO 20 I = IFBEG,IFEND
                  IF ((RATE.EQ.FBLANK) .AND.
     *               (RECR(RA1KOL+I-1).NE.FBLANK))
     *               RATE = RECR(RA1KOL+I-1) * FREQS(I)
                  IF ((RECR(RE1KOL+I-1).NE.FBLANK) .AND.
     *               ((COUNT1.EQ.0.) .OR. (DELCOR.EQ.1))) THEN
C                                       Multiband delay for multiple IFs
                     MBPHAS = 0.0
                     IF (DELCOR.GE.0) THEN
                        IF ((DELCOR.EQ.1) .AND.
     *                     (RECR(MB1KOL).NE.FBLANK)) THEN
                           MBPHAS = -TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(MB1KOL)
                        ELSE IF ((DELCOR.EQ.0) .AND.
     *                     (RECR(DE1KOL+I-1).NE.FBLANK)) THEN
                           MBPHAS = -TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(DE1KOL+I-1)
                           END IF
                        END IF
                     MBRE = COS (MBPHAS)
                     MBIM = SIN (MBPHAS)
                     SUMRE1 = SUMRE1 + RECR(RE1KOL+I-1)*MBRE -
     *                  RECR(IM1KOL+I-1)*MBIM
                     SUMIM1 = SUMIM1 + RECR(IM1KOL+I-1)*MBRE +
     *                  RECR(RE1KOL+I-1)*MBIM
C                                       scalar average of amplitude
                     SUMAM1 = SUMAM1 + SQRT(RECR(RE1KOL+I-1)**2 +
     *                  RECR(IM1KOL+I-1)**2)
                     COUNT1 = COUNT1 + 1.0
                     END IF
                  IF ((RA2KOL.GT.0) .AND. (RATE.EQ.FBLANK) .AND.
     *               (RECR(RA1KOL+I-1).NE.FBLANK))
     *               RATE = RECR(RA1KOL+I-1) * FREQS(I)
                  IF ((RA2KOL.GT.0) .AND. ((COUNT2.EQ.0.0) .OR.
     *               (DELCOR.EQ.1)) .AND. (RECR(RE2KOL+I-1).NE.FBLANK))
     *               THEN
                     MBPHAS = 0.0
                     IF (DELCOR.GE.0) THEN
                        IF ((DELCOR.EQ.1) .AND.
     *                     (RECR(MB2KOL).NE.FBLANK)) THEN
                           MBPHAS = -TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(MB2KOL)
                        ELSE IF ((DELCOR.EQ.0) .AND.
     *                     (RECR(DE2KOL+I-1).NE.FBLANK)) THEN
                           MBPHAS = -TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(DE2KOL+I-1)
                           END IF
                        END IF
                     MBRE = COS (MBPHAS)
                     MBIM = SIN (MBPHAS)
                     SUMRE2 = SUMRE2 + RECR(RE2KOL+I-1)*MBRE -
     *                  RECR(IM2KOL+I-1)*MBIM
                     SUMIM2 = SUMIM2 + RECR(IM2KOL+I-1)*MBRE +
     *                  RECR(RE2KOL+I-1)*MBIM
C                                       scalar average of amplitude
                     SUMAM2 = SUMAM2 + SQRT(RECR(RE2KOL+I-1)**2 +
     *                  RECR(IM2KOL+I-1)**2)
                     COUNT2 = COUNT2 + 1.0
                     END IF
 20               CONTINUE
C                                       See if flagged value
               BAD = COUNT1 .LE. 0.1
               BAD2 = COUNT2 .LE. 0.1
               IF (NUMTIM.LT.MXTIME) THEN
                  NUMTIM = NUMTIM + 1
                  IF (NUMTIM.EQ.1) THEN
                     TIMOFF = RECD(TIMKOL)
                     LSTIME = 0.0D0
                     END IF
                  WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                  WRKREC(NUMTIM) = ISNRNO
                  IF (DOBTWN.LE.0.0) THEN
                     WRKSRC(NUMTIM) = RECORD(SOUKOL) + 0.5
                  ELSE
                     WRKSRC(NUMTIM) = -1
                     END IF
C                                       Compute integrated phase
C                                       function; use current rate since
C                                       last time
                  IF (RATE.NE.FBLANK) THEN
                     PHADD = TWOPI * RATE * (WRKTIM(NUMTIM)-LSTIME) *
     *                  86400.0D0
                     INTFAZ = INTFAZ + PHADD
                     LSTIME = WRKTIM(NUMTIM)
                     END IF
                  IPRE = COS (INTFAZ)
                  IPIM = SIN (INTFAZ)
                  CPH = ATAN2 (IPIM, IPRE)
                  WORK8(NUMTIM) = CPH
C                                       Accumulate by real, imaginary
C                                       and amplitude.
                  IF (BAD) THEN
                     WORK2(NUMTIM) = FBLANK
                     WORK3(NUMTIM) = FBLANK
                     WORK4(NUMTIM) = FBLANK
                  ELSE
                     NUGOOD = NUGOOD + 1
                     SUMRE1 = SUMRE1 / COUNT1
                     SUMIM1 = SUMIM1 / COUNT1
                     SUMAM1 = SUMAM1 / COUNT1
C                                        Subtract integrated phase
                     WORK2(NUMTIM) = SUMRE1*IPRE + SUMIM1*IPIM
                     WORK3(NUMTIM) = SUMIM1*IPRE - SUMRE1*IPIM
                     WORK4(NUMTIM) = SQRT (WORK2(NUMTIM)*WORK2(NUMTIM) +
     *                  WORK3(NUMTIM)*WORK3(NUMTIM)) + 1.0E-20
C                                       Normalize real and imag.
                     WORK2(NUMTIM) = WORK2(NUMTIM) / WORK4(NUMTIM)
                     WORK3(NUMTIM) = WORK3(NUMTIM) / WORK4(NUMTIM)
                     IF (IFEND.GT.IFBEG) WORK4(NUMTIM) = SUMAM1
                     END IF
                  IF (BAD2) THEN
                     WORK5(NUMTIM) = FBLANK
                     WORK6(NUMTIM) = FBLANK
                     WORK7(NUMTIM) = FBLANK
                  ELSE
                     NUGOOD = NUGOOD + 1
                     SUMRE2 = SUMRE2 / COUNT2
                     SUMIM2 = SUMIM2 / COUNT2
                     SUMAM2 = SUMAM2 / COUNT2
C                                        Subtract integrated phase
                     WORK5(NUMTIM) = SUMRE2*IPRE + SUMIM2*IPIM
                     WORK6(NUMTIM) = SUMIM2*IPRE - SUMRE2*IPIM
                     WORK7(NUMTIM) = SQRT (WORK5(NUMTIM)*WORK5(NUMTIM) +
     *                  WORK6(NUMTIM)*WORK6(NUMTIM)) + 1.0E-20
C                                       Normalize real and imag.
                     WORK5(NUMTIM) = WORK5(NUMTIM) / WORK7(NUMTIM)
                     WORK6(NUMTIM) = WORK6(NUMTIM) / WORK7(NUMTIM)
                     IF (IFEND.GT.IFBEG) WORK7(NUMTIM) = SUMAM2
                     END IF
                  END IF
               END IF
 100        CONTINUE
 110     SAVE = ISNRNO - 1
C                                       Smooth as requested
         CALL SNSMSM (XINTP, STPH, WRKTIM, WORK2, FBLANK, NUMTIM,
     *      WRKSRC, WORK1)
         CALL SNSMSM (XINTP, STPH, WRKTIM, WORK3, FBLANK, NUMTIM,
     *      WRKSRC, WORK2)
         CALL SNSMSM (XINTP, STAMP, WRKTIM, WORK4, FBLANK, NUMTIM,
     *      WRKSRC, WORK3)
C                                       Second polarization if present
         IF (RE2KOL.GT.0) THEN
            CALL SNSMSM (XINTP, STPH, WRKTIM, WORK5, FBLANK,
     *         NUMTIM, WRKSRC, WORK4)
            CALL SNSMSM (XINTP, STPH, WRKTIM, WORK6, FBLANK,
     *         NUMTIM, WRKSRC, WORK5)
            CALL SNSMSM (XINTP, STAMP, WRKTIM, WORK7, FBLANK,
     *         NUMTIM, WRKSRC, WORK6)
            END IF
C                                       Replace with smoothed values
         DO 200 ITIME = 1,NUMTIM
            ISNRNO = WRKREC(ITIME)
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.LT.0) THEN
               IRET = 0
               GO TO 200
               END IF
            IF (IRET.NE.0) GO TO 900
C                                       Update
            IF ((WORK1(ITIME).NE.FBLANK) .AND. (WORK2(ITIME).NE.FBLANK)
     *         .AND. (WORK3(ITIME).NE.FBLANK)) THEN
C                                       Smoothed phase same for all IFs;
C                                       add integrated phase function.
               IPHASE = ATAN2 (WORK2(ITIME), WORK1(ITIME)) +
     *            WORK8(ITIME)
               DO 130 I = IFBEG,IFEND
C                                       Set AMP and PHASE by smoothing
C                                       selected.
                  IF (DOAMP) THEN
                     AMP = WORK3(ITIME)
C                                       Use smoothed value if blanked.
                  ELSE
                     IF (RECR(RE1KOL+I-1).EQ.FBLANK) THEN
                        AMP = WORK3(ITIME)
                     ELSE
                        AMP = SQRT (RECR(RE1KOL+I-1)*RECR(RE1KOL+I-1)
     *                     + RECR(IM1KOL+I-1)*RECR(IM1KOL+I-1))
                        END IF
                     END IF
C                                       Phase
                  IF (DOPH) THEN
                     PHASE = IPHASE
                     SMOPHS = IPHASE.NE.FBLANK
C                                       Use smoothed value if blanked.
                  ELSE
                     IF (RECR(RE1KOL+I-1).EQ.FBLANK) THEN
                        PHASE = IPHASE
                        SMOPHS = IPHASE.NE.FBLANK
                     ELSE
                        PHASE = ATAN2(RECR(IM1KOL+I-1),
     *                     RECR(RE1KOL+I-1)+1.0E-20)
                        SMOPHS = .FALSE.
                        END IF
                     END IF
C                                       Save smoothed values
                  IF ((IPHASE.NE.FBLANK) .AND. (AMP.NE.FBLANK)) THEN
C                                       delay correction
                     IF ((SMOPHS) .AND. (DELCOR.GE.0)) THEN
C                                       multi-band, incl phase
                        IF ((DELCOR.EQ.1) .AND.
     *                     (RECR(MB1KOL).NE.FBLANK)) THEN
                           PHASE = PHASE + TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(MB1KOL)
C                                       single-band, incl phase
                        ELSE IF (RECR(DE1KOL+I-1).NE.FBLANK) THEN
                           PHASE = PHASE + TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(DE1KOL+I-1)
                           END IF
                        END IF
C                                       Check if flagged entries are
C                                       to be overwritten
                     IF ((RECR(RE1KOL+I-1).EQ.FBLANK) .AND.
     *                  (XDOBLK.GE.0.0)) THEN
                        RECR(RE1KOL+I-1) = AMP * COS (PHASE)
                        RECR(IM1KOL+I-1) = AMP * SIN (PHASE)
                        RECR(WT1KOL+I-1) = MAX (0.01, RECR(WT1KOL+I-1))
C                                       Unflag delay and rate
                        IF (RECR(DE1KOL+I-1).EQ.FBLANK)
     *                     RECR(DE1KOL+I-1) = 0.0
                        IF (RECR(RA1KOL+I-1).EQ.FBLANK)
     *                     RECR(RA1KOL+I-1) = 0.0
                     ELSE IF ((RECR(RE1KOL+I-1).NE.FBLANK) .AND.
     *                  (XDOBLK.LE.0.0)) THEN
                        RECR(RE1KOL+I-1) = AMP * COS (PHASE)
                        RECR(IM1KOL+I-1) = AMP * SIN (PHASE)
                        END IF
C                                        Keep track of mean gain
C                                        modulus
                     IF (RECR(RE1KOL+I-1).NE.FBLANK) THEN
                        GNCNT = GNCNT + 1.0
                        GNSUM = GNSUM + SQRT (RECR(RE1KOL+I-1)**2 +
     *                     RECR(IM1KOL+I-1)**2)
                        END IF
                  ELSE
                     RECR(RE1KOL+I-1) = FBLANK
                     RECR(IM1KOL+I-1) = FBLANK
                     RECR(WT1KOL+I-1) = 0.0
                     END IF
 130              CONTINUE
               END IF
C                                       Second polarization present?
            IF ((RA2KOL.GT.0) .AND. (WORK4(ITIME).NE.FBLANK) .AND.
     *         (WORK5(ITIME).NE.FBLANK) .AND. (WORK6(ITIME).NE.FBLANK))
     *         THEN
C                                       Smoothed phase same for all IFs.
               IPHASE = ATAN2 (WORK5(ITIME),WORK4(ITIME)) + WORK8(ITIME)
               DO 140 I = IFBEG,IFEND
C                                       Set AMP and PHASE by smoothing
C                                       selected.
                  IF (DOAMP) THEN
                     AMP = WORK6(ITIME)
C                                       Use smoothed value if blanked.
                  ELSE
                     IF (RECR(RE2KOL+I-1).EQ.FBLANK) THEN
                        AMP = WORK6(ITIME)
                     ELSE
                        AMP = SQRT (RECR(RE2KOL+I-1)*RECR(RE2KOL+I-1)
     *                     + RECR(IM2KOL+I-1)*RECR(IM2KOL+I-1))
                        END IF
                     END IF
C                                       Phase
                  IF (DOPH) THEN
                     PHASE = IPHASE
                     SMOPHS = IPHASE.NE.FBLANK
C                                       Use smoothed value if blanked.
                  ELSE
                     IF (RECR(RE2KOL+I-1).EQ.FBLANK) THEN
                        PHASE = IPHASE
                        SMOPHS = IPHASE.NE.FBLANK
                     ELSE
                        PHASE = ATAN2(RECR(IM2KOL+I-1),
     *                     RECR(RE2KOL+I-1)+1.0E-20)
                        SMOPHS = .FALSE.
                        END IF
                     END IF
C                                        Save corrected data
                  IF ((IPHASE.NE.FBLANK) .AND. (AMP.NE.FBLANK)) THEN
C                                       delay correction
                     IF ((SMOPHS) .AND. (DELCOR.GE.0)) THEN
C                                       multi-band incl phase
                        IF ((DELCOR.EQ.1) .AND.
     *                     (RECR(MB2KOL).NE.FBLANK)) THEN
                           PHASE = PHASE + TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(MB2KOL)
C                                       single-band, incl input phase
                        ELSE IF (RECR(DE2KOL+I-1).NE.FBLANK) THEN
                           PHASE = PHASE + TWOPI * (FREQS(I)-FREQS(1)) *
     *                        RECR(DE2KOL+I-1)
                           END IF
                        END IF
C                                       Check if flagged entries
C                                       are to be overwritten
                     IF ((XDOBLK.GE.0.0) .AND.
     *                  (RECR(RE2KOL+I-1).EQ.FBLANK)) THEN
                        RECR(RE2KOL+I-1) = AMP * COS (PHASE)
                        RECR(IM2KOL+I-1) = AMP * SIN (PHASE)
                        RECR(WT2KOL+I-1) = MAX (1.0, RECR(WT2KOL+I-1))
                        IF (RECR(DE2KOL+I-1).EQ.FBLANK)
     *                     RECR(DE2KOL+I-1) = 0.0
                        IF (RECR(RA2KOL+I-1).EQ.FBLANK)
     *                     RECR(RA2KOL+I-1) = 0.0
                     ELSE IF ((XDOBLK.LE.0.0) .AND.
     *                  (RECR(RE2KOL+I-1).NE.FBLANK)) THEN
                        RECR(RE2KOL+I-1) = AMP * COS (PHASE)
                        RECR(IM2KOL+I-1) = AMP * SIN (PHASE)
                        END IF
C                                        Keep track of mean gain
C                                        modulus
                     IF (RECR(RE2KOL+I-1).NE.FBLANK) THEN
                        GNCNT = GNCNT + 1.0
                        GNSUM = GNSUM + SQRT (RECR(RE2KOL+I-1)**2 +
     *                     RECR(IM2KOL+I-1)**2)
                        END IF
                  ELSE
                     RECR(RE2KOL+I-1) = FBLANK
                     RECR(IM2KOL+I-1) = FBLANK
                     RECR(WT2KOL+I-1) = 0.0
                     END IF
 140              CONTINUE
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, ISNRNO, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200        CONTINUE
C                                       First SN number of next antenna
         FSTREC = SAVE
C                                       End of antenna loop
 600     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('SMOAPH: TABIO ERROR',I3,' SMOOTHING AMP AND/OR PHASES')
      END
      SUBROUTINE SNSMSM (SMMETH, SMOTIM, TIME, IN, BLANK, NUMTIM, S,
     *   OUT)
C-----------------------------------------------------------------------
C   Routine to call appropriate smoothing routine.  Magic value blanking
C   is supported.
C   Inputs:
C      SMMETH   C*4    Method 'BOX','MWF', unknown = 'BOX'
C      SMOTIM   R(*)   Smoothing time (days)
C      TIME     R(*)   Times (days)
C      IN       R(*)   Input values.
C      BLANK    R      Magic blank value.
C      NUMTIM   I      Number of time/values
C      S        I(*)   Source number list
C   Output:
C      OUT      R(*)   Output array
C-----------------------------------------------------------------------
      CHARACTER SMMETH*4
      REAL      SMOTIM(*), TIME(*), IN(*), BLANK, OUT(*)
      INTEGER   NUMTIM, S(*)
C-----------------------------------------------------------------------
C                                       Any work to do?
      IF (NUMTIM.LE.0) GO TO 999
C                                       Median window filter
      IF (SMMETH.EQ.'MWF') THEN
         CALL MWFBSM (SMOTIM(1), TIME, IN, S, BLANK, NUMTIM, OUT)
C                                       function types
      ELSE IF (SMMETH.EQ.'GAUS') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'EXP ') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
      ELSE IF (SMMETH.EQ.'LINE') THEN
         CALL FUNBSM (SMMETH, SMOTIM(3), SMOTIM, TIME, IN, S, BLANK,
     *      NUMTIM, OUT)
C                                       2-point
      ELSE IF (SMMETH.EQ.'2PT ') THEN
         CALL TPTBSM (SMOTIM(1), TIME, IN, S, BLANK, NUMTIM, .FALSE.,
     *      OUT)
C                                       2-point - hanning
      ELSE IF (SMMETH.EQ.'2PTH') THEN
         CALL TPTBSM (SMOTIM(1), TIME, IN, S, BLANK, NUMTIM, .TRUE.,
     *      OUT)
C                                       Default = Boxcar
      ELSE
         CALL BOXBSM (SMOTIM(1), TIME, IN, S, BLANK, NUMTIM, OUT)
         END IF
C
 999  RETURN
      END
      SUBROUTINE SNSMHI
C-----------------------------------------------------------------------
C   SNSMHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
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
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       Task message
      CALL ZDATE (DATE)
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
      IF (NANTSL.LE.0) THEN
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
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       SUBARRAY, SNVER
      WRITE (HILINE,2002) TSKNAM, SUBA, SNVER
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation fn
      WRITE (HILINE,2006) TSKNAM, XINTP
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       what was changed
      IF (XDOBLK.GT.0.0) THEN
         HILINE = TSKNAM // '/ Only blanked solutions changed'
      ELSE IF (XDOBLK.EQ.0.0) THEN
         HILINE = TSKNAM // '/ Blanked and good solutions changed'
      ELSE
         HILINE = TSKNAM // '/ Only good solutions changed'
         END IF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Interpolation parms
      WRITE (HILINE,2007) TSKNAM, XIPARM(1), XIPARM(2), XIPARM(3)
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,4007) TSKNAM, XIPARM(4), XIPARM(5), 'support'
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       extra parms
      IF ((XINTP.EQ.'GAUS') .OR. (XINTP.EQ.'EXP ') .OR.
     *   (XINTP.EQ.'LINE')) THEN
         WRITE (HILINE,2007) TSKNAM, XIPARM(6), XIPARM(7), XIPARM(8)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4007) TSKNAM, XIPARM(9), XIPARM(10), 'FWHM'
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,4008) TSKNAM, CUTOFF
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
C                                       Clipping parameters
      WRITE (HILINE,4009) TSKNAM, XCPARM(1), XCPARM(2), XCPARM(3)
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,4010) TSKNAM, XCPARM(4), XCPARM(5), XCPARM(6),
     *   XCPARM(7)
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,4010) TSKNAM, XCPARM(8), XCPARM(9), XCPARM(10)
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       smoothing type
      WRITE (HILINE,2008) TSKNAM, XSMO
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNSMHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2002 FORMAT (A6,'SUBARRAY =',I3,' SNVER = ',I4,' /SN table')
 2004 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2006 FORMAT (A6,'INTERPOL = ''',A4,''' / Interpolation type')
 2007 FORMAT (A6,'INTPARM = ',3F10.5)
 4007 FORMAT (A6,'          ',2F10.5,' / Interpolation',A)
 4008 FORMAT (A6,'CUTOFF =',F8.5,'  / sum of weights cutoff')
 4009 FORMAT (A6,'CPARM     ',3F10.5,' / Clipping parms')
 4010 FORMAT (A6,'          ',4F10.5)
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
      LOGICAL FUNCTION SNWANT (SOUR, FQID, ANT, SUB, TIME)
C-----------------------------------------------------------------------
C   Function to determine if a source, FQid, antenna, subarray and time
C   have been selected.  Returns .TRUE. if task selection criteria are
C   met else .FALSE.
C   Inputs
C      SOUR     I    Source Id
C      FQID     I    FQ id
C      ANT      I    Antenna number
C      SUB      I    Subarray number
C      TIME     D    Time
C   Inputs from common:
C      SOUWAN   I(*)  List of selected source IDs.
C      NSOUWD   I     Number of values in SOUWAN, 0=any source
C      DOSWNT   L     If .TRUE. values in SOUWAN are selected else
C                     deselected.
C      FREQID   I     Selected FQ id, .le. 0 => any
C      SUBA     I     Selected subarray, .le. 0 => any
C      ANTENS   I(*)  List of selected antennas
C      NANTSL   I     Number of values in ANTENS, 0=any antenna
C      DOAWNT   L     If .TRUE. values in ANTENS are selected else
C                     deselected.
C      TSTART   D     Start time
C      TEND     D     End time
C-----------------------------------------------------------------------
      INTEGER   SOUR, FQID, ANT, SUB
      DOUBLE PRECISION TIME
C
      LOGICAL WANT, SLCTD
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SNSMO.INC'
C-----------------------------------------------------------------------
C                                       Want this source?
      WANT = SLCTD (SOUR, SOUWAN, NSOUWD, DOSWNT)
C                                       Want this FQ id?
      WANT = WANT .AND.
     *   ((FQID.EQ.FREQID) .OR. (FREQID.LE.0) .OR. (FQID.LE.0))
C                                       Check subarray
      WANT = WANT .AND.
     *   ((SUB.EQ.SUBA) .OR. (SUBA.LE.0) .OR. (SUB.LE.0))
C                                       Want this antenna?
      WANT = WANT .AND.
     *   SLCTD (ANT, ANTENS, NANTSL, DOAWNT)
C                                       Check time
      WANT = WANT .AND.
     *   ((TIME.GE.TSTART) .AND. (TIME.LE.TEND))
      SNWANT = WANT
C
 999  RETURN
      END
