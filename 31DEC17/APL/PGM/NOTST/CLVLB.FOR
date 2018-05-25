LOCAL INCLUDE 'CLVLB.INC'
C                                       Local include for CLVLB
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XCALIN(12)
      REAL      XSIN, XDISIN, XFQID, XSUBA, XGAINV, XGAINU, XCUT,
     *   DOENNO, XBIF, XANT(50),
     *   SCRBUF(256), BUFF2(UVBFSS), CURTIM, FINC(MAXIF)
      INTEGER   SEQIN, DISKIN, JBUFSZ, OLDCNO, NANT, ICLVER, OCLVER,
     *   NIF, NPOL, CURSOU, SUBARR, FRQSEL, ISBAND(MAXIF), LSTSOU,
     *   DBGIF, DBGANT
      CHARACTER NAMEIN*12, CLAIN*6, CALIN*48
      DOUBLE PRECISION FOFF(MAXIF), RAPOBS, DAPOBS
      INTEGER   ICLBUF(512), OCLBUF(512), ICLRNO, OCLRNO,
     *   CLKOLS(MAXCLC), CLNUMV(MAXCLC)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XFQID, XSUBA,
     *   XGAINV, XGAINU, XCUT, XCALIN, DOENNO, XBIF, XANT
      COMMON /CLVLBP/ RAPOBS, DAPOBS, FOFF, ICLBUF, OCLBUF, FINC,
     *   ISBAND, SEQIN, DISKIN, OLDCNO, CURTIM, NIF, NPOL, ICLVER,
     *   OCLVER, CURSOU, SUBARR, FRQSEL, CLKOLS, CLNUMV, ICLRNO, OCLRNO,
     *   NANT, LSTSOU, DBGIF, DBGANT
      COMMON /CHARPM/ NAMEIN, CLAIN, CALIN
      COMMON /BUFRS/ SCRBUF, BUFF2, JBUFSZ
C                                       antenna squint
C                                       SQAZ, SQEL radians
C                                       BMFREQ, EFFREQ MHz
C                                       EFFD meters
C                                       EFFRAT m/MHz
      DOUBLE PRECISION SQAZ(MAXANT), SQEL(MAXANT), BMFREQ(MAXANT),
     *   EFFD(MAXANT), EFFREQ(MAXANT), EFFRAT(MAXANT)
      COMMON /SQUINT/ SQAZ, SQEL, BMFREQ, EFFD, EFFREQ, EFFRAT
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       End local include for CLVLB
LOCAL END
      PROGRAM CLVLB
C-----------------------------------------------------------------------
C! correct VLBI CL tables for offset between the pointing and phase pos.
C# Utility UV Calibration VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 2013, 2015
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
C   CLVLB corrects a CL table amplitudes for the single-dish beam
C   pattern at the phase-stopping position
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      SUBARRAY       SUBARR        Subarray to do
C      FREQID         FRQSEL        FREQID to do
C      GAINVER        ICLVER        CL table version in
C      GAINUSE        OCLVER        CL table version out
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'CLVLB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'CLVLB '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CLVLBI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL CLVLBD (IRET)
      IF (IRET.EQ.0) CALL CLVLBH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE CLVLBI (PRGN, JERR)
C-----------------------------------------------------------------------
C   CLVLBI gets input parameters for CLVLB and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'CLVLB.INC'
      CHARACTER STAT*4, PTYPE*2, BNDCOD(MAXIF)*8
      INTEGER   IROUND, NPARM, IERR, J, LUN(3), KEY(2,2), IP, IIF,
     *   IANT, VER, KEYSUB(2,2), NTERM
      REAL      FKEY(2,2), GMMOD, NCH2
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA LUN /59,60,61/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      CURTIM = -1.E5
      JBUFSZ = UVBFSS * 2
      LSTSOU = -999
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 76
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'GETTING INPUT ADVERB VALUES'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XCALIN, CALIN)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      DBGIF = IROUND (XBIF)
      DBGANT = IROUND (XANT(1))
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       number antennas
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, BUFF2, JERR)
      IF (JERR.NE.0) GO TO 999
      NANT = NSTNS
      NPOL = CATBLK(KINAX+JLOCS)
      NPOL = MIN (2, NPOL)
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      NCH2 = CATBLK(KINAX+JLOCF) / 2.0
C                                       CL
      ICLVER = IROUND (XGAINV)
      CALL FNDEXT ('CL', CATBLK, J)
      IF (J.LE.0) THEN
         MSGTXT = 'NO CL TABLES: I QUIT'
         JERR = 10
         GO TO 990
         END IF
      IF ((ICLVER.LE.0) .OR. (ICLVER.GT.J)) ICLVER = J
      OCLVER = IROUND (XGAINU)
      IF ((OCLVER.LE.0) .OR. (OCLVER.GT.J+1)) OCLVER = J + 1
C                                       Freq id
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.LE.0) FRQSEL = 1
C                                       get frequencies
      VER = 1
      CALL CHNDAT ('READ', ICLBUF, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FRQSEL, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1000) JERR, 'FINDING FREQUENCIES'
         GO TO 990
         END IF
C                                       freqs at center channel
      DO 30 J = 1,NIF
         FOFF(J) = (FOFF(J) + CATD(KDCRV+JLOCF) +
     *      (NCH2 - CATR(KRCRP+JLOCF)) * FINC(J)) / 1.D6
 30      CONTINUE
C                                       get beam parameters
      CALL BMPARM (JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1000) JERR, 'FINDING BEAM PARAMETERS'
         GO TO 990
         END IF
C                                       open and sort CL table
      CALL CALINI ('READ', ICLBUF, DISKIN, OLDCNO, ICLVER, CATBLK,
     *   LUN(2), ICLRNO, CLKOLS, CLNUMV, IANT, IP, IIF, NTERM, GMMOD,
     *   JERR)
      IF (JERR.NE.0) GO TO 999
      KEY(1,2) = 4
      KEY(1,1) = 1
      IF ((ICLBUF(43).NE.KEY(1,1)) .OR. (ICLBUF(44).NE.KEY(1,2)))
     *   THEN
         CALL TABIO ('CLOS', 0, ICLRNO, ICLBUF, ICLBUF, JERR)
         CALL TABSRT (DISKIN, OLDCNO, 'CL', ICLVER, ICLVER, KEY,
     *      KEYSUB, FKEY, ICLBUF, CATBLK, JERR)
         IF (JERR.NE.0) GO TO 999
         CALL CALINI ('READ', ICLBUF, DISKIN, OLDCNO, ICLVER, CATBLK,
     *      LUN(2), ICLRNO, CLKOLS, CLNUMV, IANT, IP, IIF, NTERM, GMMOD,
     *      JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      IF ((IP.NE.NPOL) .OR. (IIF.NE.NIF)) THEN
         MSGTXT = 'CL TABLE NO MATCH FOR POL AND/OR IFS'
         JERR = 10
         GO TO 999
         END IF
C                                       create open new CL table
      CALL CALINI ('WRIT', OCLBUF, DISKIN, OLDCNO, OCLVER, CATBLK,
     *   LUN(1), OCLRNO, CLKOLS, CLNUMV, IANT, IP, IIF, NTERM, GMMOD,
     *   JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Put input file in READ
      PTYPE = 'UV'
      STAT = 'WRIT'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
      JERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLVLBI: ERROR',I3,' ON',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE BMPARM (IRET)
C-----------------------------------------------------------------------
C   BMPARM attempts to fill in the antenna squint parameters
C   Outputs:
C      IRET   I   > 0 error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'CLVLB.INC'
      CHARACTER VLBANM(30)*8, INLINE*132
      INTEGER   IA, I, LUN, FIND, JT, JTRIM, KBP, ISVLBA, DROUND
      DOUBLE PRECISION X, TEMP, DEFALT(6), VALS(6), VLBPRM(4)
      LOGICAL   WASDEF
      REAL      VLAZEL(2,10)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA VLBANM /'SC','VLBA_SC','SC-VLBA' ,'HN','VLBA_HN','HN-VLBA',
     *   'NL','VLBA_NL','NL-VLBA', 'FD','VLBA_FD','FD-VLBA',
     *   'LA','VLBA_LA','LA-VLBA', 'PT','VLBA_PT','PT-VLBA',
     *   'KP','VLBA_KP','KP-VLBA', 'OV','VLBA_OV','OV-VLBA',
     *   'BR','VLBA_BR','BR-VLBA', 'MK','VLBA_MK','MK-VLBA'/
      DATA VLBPRM /1438.D0, 24.926527565D0, 1500.D0, -1.64665969D-3 /
C      DATA VLBPRM /1438.D0, 24.8276714D0, 1500.D0, 0.0D0 /
      DATA VLAZEL /-1.53, -0.56, -1.35, -0.67,
     *   -1.56, -0.62, -1.56, -0.58,
     *   -1.62, -0.67, -1.59, -0.63,
     *   -1.61, -0.59, -1.76, -0.95,
     *   -1.44, -0.50, -1.56, -0.60/
C-----------------------------------------------------------------------
C                                       what did we get from AN file
      CALL RFILL (6*MAXANT, 0.0, SQAZ)
      DO 20 IA = 1,NSTNS
         IF ((ANFWHM(1,IA).GT.0.0) .AND. (ANFWHM(1,IA).NE.FBLANK)) THEN
            EFFD(IA) = 4.43 * 180.0 / PI / PI / ANFWHM(1,IA)
            EFFREQ(IA) = FOFF(1)
            IF ((ANFWHM(NIF,IA).GT.0) .AND. (ANFWHM(NIF,IA).NE.FBLANK))
     *         THEN
               TEMP =  4.43 * 180.0 / PI / PI / ANFWHM(NIF,IA)
               EFFRAT(IA) = (TEMP - EFFD(IA)) / (FOFF(NIF) - FOFF(1))
               END IF
            END IF
C                                       is it a VLBA antenna?
         ISVLBA = 0
         DO 10 I = 1,30
            IF (STNNAM(IA).EQ.VLBANM(I)) ISVLBA = (I+2)/3
 10         CONTINUE
C                                       VLBA antennas
C                                       note these are for L band!
         IF (ISVLBA.GT.0) THEN
            SQAZ(IA) = VLAZEL(1,ISVLBA) * PI / 180.0 / 60.0
            SQEL(IA) = VLAZEL(2,ISVLBA) * PI / 180.0 / 60.0
            BMFREQ(IA) = VLBPRM(1)
            IF (EFFD(IA).EQ.0.0) THEN
               EFFD(IA) =  VLBPRM(2)
               EFFREQ(IA) = VLBPRM(3)
               EFFRAT(IA) = VLBPRM(4)
               END IF
            END IF
 20      CONTINUE
C                                       user text file now
      IF (CALIN.NE.' ') THEN
         LUN = 3
         CALL ZTXOPN ('READ', LUN, FIND, CALIN, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN TEXT FILE'
            GO TO 990
            END IF
         WASDEF = .FALSE.
 100     CALL ZTXIO ('READ', LUN, FIND, INLINE, IRET)
         IF (IRET.EQ.0) THEN
            JT = JTRIM (INLINE)
            IF ((INLINE(1:1).EQ.'!') .OR. (INLINE(1:1).EQ.'#'))
     *         GO TO 100
            KBP = 1
            CALL GETNUM (INLINE, JT, KBP, X)
            IF (X.EQ.DBLANK) GO TO 980
            IA = DROUND (X)
            IF ((IA.LT.0) .OR. (IA.GT.NSTNS)) THEN
               WRITE (MSGTXT,1100) IA
               CALL MSGWRT (7)
               GO TO 100
               END IF
            DO 110 I = 1,6
               CALL GETNUM (INLINE, JT, KBP, X)
               IF (X.EQ.DBLANK) GO TO 980
               VALS(I) = X
 110           CONTINUE
            IF (IA.EQ.0) THEN
               WASDEF = .TRUE.
               CALL RCOPY (6, VALS, DEFALT)
               DEFALT(1) = DEFALT(1) * PI / 180.0 / 60.0
               DEFALT(2) = DEFALT(2) * PI / 180.0 / 60.0
            ELSE
               SQAZ(IA) = VALS(1) * PI / 180.0 / 60.0
               SQEL(IA) = VALS(2) * PI / 180.0 / 60.0
               BMFREQ(IA) = VALS(3)
               EFFD(IA) = VALS(4)
               EFFREQ(IA) = VALS(5)
               EFFRAT(IA) = VALS(6)
               END IF
            GO TO 100
         ELSE IF (IRET.NE.2) THEN
            WRITE (MSGTXT,1000) IRET, 'READ TEXT FILE'
            GO TO 990
         ELSE
            IRET = 0
            END IF
         CALL ZTXCLS (LUN, FIND, I)
C                                       fill in default
         IF (WASDEF) THEN
            DO 120 IA = 1,NSTNS
               IF ((SQAZ(IA).EQ.0.0) .AND. (SQEL(IA).EQ.0.0) .AND.
     *            (BMFREQ(IA).EQ.0.0) .AND. (EFFD(IA).EQ.0.0) .AND.
     *            (EFFREQ(IA).EQ.0.0) .AND. (EFFRAT(IA).EQ.0.0)) THEN
                  SQAZ(IA) = DEFALT(1)
                  SQEL(IA) = DEFALT(2)
                  BMFREQ(IA) = DEFALT(3)
                  EFFD(IA) = DEFALT(4)
                  EFFREQ(IA) = DEFALT(5)
                  EFFRAT(IA) = DEFALT(6)
                  END IF
 120           CONTINUE
            END IF
         END IF
      GO TO 999
C
 980  IRET = 10
      MSGTXT = 'ILLEGAL VALUE IN TEXT FILE INPUT'
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BMPARM: ERROR',I4,' ON ',A)
 1100 FORMAT ('BMPARM IGNORING BAD ANTENNA NUMBER =',I10)
      END
      SUBROUTINE CLVLBD (IRET)
C-----------------------------------------------------------------------
C   CLVLBD reads through the CL table and calls the correction routines
C   Outputs:
C      IRET   I   > 0 => failure
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'CLVLB.INC'
      INTEGER   IREC, SOURID, ANTNO, SUBA, FREQID, I, REFA(2,MAXIF),
     *   LUN, LUNTMP, LSTBAD, CLMAX, DIR, NBAD
      REAL      TIMEI, IFR, DOPOFF(MAXIF), ATMOS(2), DATMOS(2),
     *   MBDELY(2), CLOCK(2), DCLOCK(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), DELAY(2,MAXIF), RATE(2,MAXIF),
     *   WEIGHT(2,MAXIF), POLAR(2)
      DOUBLE PRECISION DELDAT, TIME, GEODLY(10), JD, OBSPOS(3)
      LOGICAL   GR
      CHARACTER OBSDAT*8
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LSTBAD
      DATA LSTBAD /0/
      DATA DELDAT, OBSPOS, POLAR /1.D-6, 0.D0, 0.D0, 0.D0, 0., 0./
C-----------------------------------------------------------------------
      CLMAX = ICLBUF(5)
      NBAD = 0
      DO 100 IREC = 1,CLMAX
         ICLRNO = IREC
         CALL TABCAL ('READ', ICLBUF, ICLRNO, CLKOLS, CLNUMV, NPOL,
     *      NIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OLD CL TABLE'
            GO TO 990
            END IF
C                                       match ?
         IF ((SUBARR.GT.0) .AND. (SUBA.GT.0) .AND. (SUBARR.NE.SUBA))
     *      GO TO 100
         IF ((FRQSEL.GT.0) .AND. (FREQID.GT.0) .AND. (FRQSEL.NE.FREQID))
     *      GO TO 100
C                                       source
         CURSOU = SOURID
         IF (CURSOU.NE.LSTSOU) THEN
            LUN = LUNTMP (1)
            CALL GETSOU (CURSOU, DISKIN, OLDCNO, CATBLK, LUN, IRET)
            IF (IRET.EQ.11) THEN
               WRITE (MSGTXT,1010) CURSOU
               IF (CURSOU.NE.LSTBAD) CALL MSGWRT (6)
               LSTBAD = CURSOU
               GO TO 100
            ELSE
               LSTSOU = CURSOU
C                                       Find time of observation
               CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
C                                       Find Julian day, JD
               CALL JULDAY (OBSDAT, JD)
               EPOCH = CATR(KREPO)
C                                       Do not trust apparent coords
C                                       recompute for pointing
               GR = .TRUE.
               DIR = 1
               CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR,
     *            RAOBS, DECOBS, RAPOBS, DAPOBS)
               END IF
            END IF
C                                       let's do it
         CALL DOWORK (TIME, ANTNO, CREAL, CIMAG, IRET)
         IF (IRET.NE.0) NBAD = NBAD + 1
C                                       write it
         CALL TABCAL ('WRIT', OCLBUF, OCLRNO, CLKOLS, CLNUMV, NPOL,
     *      NIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR,
     *      GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY, CLOCK, DCLOCK, DISP,
     *      DDISP, CREAL, CIMAG, DELAY, RATE, WEIGHT, REFA, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING NEW CL TABLE'
            GO TO 990
            END IF
 100     CONTINUE
      CALL TABIO ('CLOS', 0, OCLRNO, BUFF2, OCLBUF, I)
      CALL TABIO ('CLOS', 0, ICLRNO, BUFF2, ICLBUF, I)
      WRITE (MSGTXT,1100) NBAD, CLMAX
      CALL MSGWRT (5)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLVLBD: ERROR',I4,' ON ',A)
 1010 FORMAT ('CLVLBD: SOURCE ID',I8,' NOT RECOGNIZED')
 1100 FORMAT ('CLVLBD:',I8,' RECORDS BELOW HORIZON OF',I8)
      END
      SUBROUTINE DOWORK (TIME, ANTNO, CREAL, CIMAG, IRET)
C-----------------------------------------------------------------------
C   Determines the corrections and applies them
C   Inputs:
C      TIME    D        sample time
C      ANTNO   I        antenna number
C   In/out:
C      CREAL   R(2,*)   real part of gain
C      CIMAG   R(2,*)   imag part of gain
C   Outputs:
C      IRET    I        error code: 1 => all blanked (below horizon)
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIME
      INTEGER   ANTNO, IRET
      REAL      CREAL(2,*), CIMAG(2,*)
C
      INCLUDE 'CLVLB.INC'
      INTEGER   LF, PRTS(MAXANT)
      DOUBLE PRECISION PEL, PAZ, TEL, TAZ, LAMBDA, DAZ, DEL, REL, RAZ,
     *   LAZ, LEL, RSEP, LSEP, D, RFAC, LFAC, ANGSEP, BEAM, R, THETA,
     *   SEPAZ, SEPEL, HAVSIN, AZBEAR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE PRTS
      DATA PRTS /MAXANT*0/
C-----------------------------------------------------------------------
C                                       get two positions in az/el
      CALL ELVS (ANTNO, TIME, RAPOBS, DAPOBS, PEL, PAZ, TEL, TAZ)
C                                       below horizon
      IF ((PEL.LE.0.0D0) .OR. (TEL.LE.0.0D0)) THEN
         IRET = 1
         LF = 2 * NIF
         CALL RFILL (LF, FBLANK, CREAL)
         CALL RFILL (LF, FBLANK, CIMAG)
C                                       observable
      ELSE
         DO 100 LF = 1,NIF
            LAMBDA = VELITE / (1.D6 * FOFF(LF))
            DAZ = SQAZ(ANTNO) * (BMFREQ(ANTNO) / FOFF(LF))
            DEL = SQEL(ANTNO) * (BMFREQ(ANTNO) / FOFF(LF))
C                                       Eric method:
C                                       add delta EL/AZ to pointing
C                                       position, get separations,
C                                       original signs
            IF (DOENNO.LE.0.0) THEN
               REL = PEL - DEL / 2.0D0
               LEL = PEL + DEL / 2.0D0
               RAZ = PAZ - DAZ / 2.0D0
               LAZ = PAZ + DAZ / 2.0D0
               RSEP = ANGSEP (TEL, TAZ, REL, RAZ)
               LSEP = ANGSEP (TEL, TAZ, LEL, LAZ)
C                                       Enno's method
C                                       compute separation beam to
C                                       target, add delta AZ/EL
            ELSE
               R = HAVSIN (TAZ, TEL, PAZ, PEL)
               THETA = AZBEAR (TAZ, TEL, PAZ, PEL)
               SEPAZ = R * SIN (THETA)
               SEPEL = R * COS (THETA)
C                                       swap R and L
               RAZ = SEPAZ + DAZ / 2.0D0
               LAZ = SEPAZ - DAZ / 2.0D0
               REL = SEPEL + DEL / 2.0D0
               LEL = SEPEL - DEL / 2.0D0
               RSEP = SQRT (RAZ*RAZ + REL*REL)
               LSEP = SQRT (LAZ*LAZ + LEL*LEL)
               END IF
C                                       beam diameter effective
            D = EFFD(ANTNO) + (FOFF(LF) - EFFREQ(ANTNO)) * EFFRAT(ANTNO)
C                                       apply beam factor
            IF (D.GT.0.0D0) THEN
               RFAC = BEAM (RSEP, D, LAMBDA)
               LFAC = BEAM (LSEP, D, LAMBDA)
C                                       DEBUG
               IF ((LF.EQ.DBGIF) .AND. (ANTNO.EQ.DBGANT)) THEN
                  IF (PRTS(ANTNO).LT.CURSOU) THEN
                     WRITE (MSGTXT,1000) ANTNO, 'R', CURSOU, RAD2DG*REL,
     *                  RAD2DG*RAZ, RAD2DG*RSEP, RFAC*RFAC
                     CALL MSGWRT (5)
                     WRITE (MSGTXT,1000) ANTNO, 'L', CURSOU, RAD2DG*LEL,
     *                  RAD2DG*LAZ, RAD2DG*LSEP, LFAC*LFAC
                     CALL MSGWRT (5)
                     PRTS(ANTNO) = PRTS(ANTNO) + 1
                     END IF
                  END IF
               IF (RFAC.LT.XCUT) THEN
                  CREAL(1,LF) = FBLANK
                  CIMAG(1,LF) = FBLANK
               ELSE
                  CREAL(1,LF) = CREAL(1,LF) / (ABS(RFAC))
                  CIMAG(1,LF) = CIMAG(1,LF) / (ABS(RFAC))
                  END IF
               IF (LFAC.LT.XCUT) THEN
                  CREAL(2,LF) = FBLANK
                  CIMAG(2,LF) = FBLANK
               ELSE
                  CREAL(2,LF) = CREAL(2,LF) / (ABS(LFAC))
                  CIMAG(2,LF) = CIMAG(2,LF) / (ABS(LFAC))
                  END IF
               END IF
 100        CONTINUE
         IRET = 0
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I2,1X,A,I3,F12.7,F13.7,F11.7,F10.6)
      END
      DOUBLE PRECISION FUNCTION ANGSEP (EL1, AZ1, EL2, AZ2)
C-----------------------------------------------------------------------
C   ANGSEP returns the sine of angular separation between 2 points
C   Inputs:
C      EL1      D   Point 1: elevation
C      AZ1      D            azimuth
C      EL2      D   Point 2: elevation
C      AZ2      D            azimuth
C   Output:
C      ANGSEP   D   sine of the great circle distance between
c                   the 2 points
C-----------------------------------------------------------------------
      DOUBLE PRECISION EL1, AZ1, EL2, AZ2
C
      DOUBLE PRECISION DD
C-----------------------------------------------------------------------
      DD = SIN(EL1) * SIN(EL2) + COS(EL1) * COS(EL2) * COS(AZ1-AZ2)
      DD = MAX (-1.0D0, MIN (1.0D0, DD))
      DD = ACOS (DD)
      ANGSEP = SIN (DD)
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION BEAM (SEP, DIAM, LAMBDA)
C-----------------------------------------------------------------------
C   BEAM returns the value of the single-dish VOLTAGE beam of specified
C   effective diameter at specified wavelength and angular separation.
C   Inputs:
C      SEP      D   sine of the angle away from the center of the beam
C      DIAM     D   dish effective diameter (meters)
C      LAMBDA   D   Observation wavelength (meters)
C   Output:
C      BEAM     D   Beam power level - set to 0.01 if less than this
C-----------------------------------------------------------------------
      DOUBLE PRECISION SEP, DIAM, LAMBDA
C
      DOUBLE PRECISION X, BESSJ1
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      X = PI * SEP * DIAM / LAMBDA
C                                       BESSJ1 starts at x/2
      BEAM = 2.0D0 * BESSJ1 (X) / X
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION BESSJ1 (X)
C-----------------------------------------------------------------------
C   Compute the J1 Bessel function for a given argument
C   Inputs:
C      X        D    The argument of the J1 Bessel function desired
C   Output:
C      BESSJ1   D   Returns the resultant J1 Bessel function
C   Method
C      from "Numerical Recipes" by Press et al.
C-----------------------------------------------------------------------
      DOUBLE PRECISION X
C
      DOUBLE PRECISION P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2,
     *   R3, R4, R5, R6, S1, S2, S3, S4, S5, S6, Y, AX, XX, Z
      SAVE P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2, R3, R4, R5,
     *   R6, S1, S2, S3, S4, S5, S6
      DATA R1, R2, R3, R4, R5, R6 /72362614232.D0, -7895059235.D0,
     *   242396853.1D0, -2972611.439D0, 15704.48260D0, -30.16036606D0/
      DATA S1, S2, S3, S4, S5, S6 /144725228442.D0, 2300535178.D0,
     *   18583304.74D0, 99447.43394D0, 376.9991397D0, 1.D0/
      DATA P1, P2, P3, P4, P5 /1.D0, .183105D-2, -.3516396496D-4,
     *   .2457520174D-5, -.240337019D-6/
      DATA Q1, Q2, Q3, Q4, Q5 /.04687499995D0, -.2002690873D-3,
     *   .8449199096D-5, -.88228987D-6, .105787412D-6/
C-----------------------------------------------------------------------
      IF (ABS(X).LT.8.) THEN
         Y = X**2
         BESSJ1 = X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /
     *      (S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
         AX = ABS (X)
         Z = 8. / AX
         Y = Z**2
         XX = AX - 2.356194491D0
         BESSJ1 = SIGN (1.0D0, X) * SQRT (.636619772D0/AX) *
     *      (COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*P5)))) -
     *       Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
         END IF
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION HAVSIN (AZ1, EL1, AZ2, EL2)
C-----------------------------------------------------------------------
C   Returns the distance between two coordinates
C   Inputs:
C      AZ1      D   Position 1: azimuth
C      EL1      D   Position 1: elevation
C      AZ2      D   Position 2: azimuth
C      EL2      D   Position 2: elevation
C   Outputs:
C      HAVSIN   D   Bearing in radians
C-----------------------------------------------------------------------
      DOUBLE PRECISION AZ1, EL1, AZ2, EL2
C
      DOUBLE PRECISION A
C-----------------------------------------------------------------------
      A = SIN((EL1-EL2)/2.D0) ** 2 +
     *   (SIN((AZ1-AZ2)/2.D0) ** 2) * COS (EL1) * COS(EL2)
      HAVSIN = 2.0D0 * ATAN2 (SQRT(A), SQRT(1.0D0-A))
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION AZBEAR (AZ1, EL1, AZ2, EL2)
C-----------------------------------------------------------------------
C   Returns the angle between two coordinates
C   Inputs:
C      AZ1      D   Position 1: azimuth
C      EL1      D   Position 1: elevation
C      AZ2      D   Position 2: azimuth
C      EL2      D   Position 2: elevation
C   Outputs:
C      AZBEAR   D   Bearing in radians
C-----------------------------------------------------------------------
      DOUBLE PRECISION AZ1, EL1, AZ2, EL2
C
C-----------------------------------------------------------------------
      AZBEAR = -ATAN2 (SIN(AZ2-AZ1) * COS(EL1),
     *   COS(EL2) * SIN(EL1) - SIN(EL2) * COS(EL1) * COS(AZ2-AZ1))
C
 999  RETURN
      END
      SUBROUTINE CLVLBH
C-----------------------------------------------------------------------
C   CLVLBH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN, IERR, I
      INCLUDE 'CLVLB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN /27/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       output file written
      CALL HIOPEN (LUN, DISKIN, OLDCNO, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN OLD HISTORY FILE'
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       versions applied
      WRITE (HILINE,1010) TSKNAM, ICLVER
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1011) TSKNAM, OCLVER
      CALL HIADD (LUN, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       parameters used
      WRITE (MSGTXT,1020)
      CALL MSGWRT (4)
      DO 20 I = 1,NSTNS
         SQAZ(I) = SQAZ(I) * 180.0 * 60.0 / PI
         SQEL(I) = SQEL(I) * 180.0 * 60.0 / PI
         WRITE (MSGTXT,1021) I, SQAZ(I), SQEL(I), BMFREQ(I)
         CALL MSGWRT (4)
 20      CONTINUE
      WRITE (MSGTXT,1030)
      CALL MSGWRT (4)
      DO 30 I = 1,NSTNS
         WRITE (MSGTXT,1031) I, EFFD(I), EFFREQ(I), EFFRAT(I)
         CALL MSGWRT (4)
 30      CONTINUE
      MSGTXT = ' IF   FREQ MHz'
      CALL MSGWRT (4)
      DO 40 I = 1,NIF
         WRITE (MSGTXT,1032) I, FOFF(I)
         CALL MSGWRT (4)
 40      CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL CATIO ('UPDT', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLVLBH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'GAINVER =',I5,5X,'/ CL file version in')
 1011 FORMAT (A6,'GAINUSE =',I5,5X,'/ CL file version out')
 1020 FORMAT ('ANT  SQAZ    SQEL    SQFREQ')
 1021 FORMAT (I3,2F8.4,F9.2)
 1030 FORMAT ('ANT    BMDIAM    BMFREQ   DBMDIAM')
 1031 FORMAT (I3,F10.5,F9.2,1PE13.5)
 1032 FORMAT (I3,F10.3)
      END
      SUBROUTINE ELVS (ANTNO, TIME, RAPOBS, DAPOBS, PEL, PAZ, TEL, TAZ)
C-----------------------------------------------------------------------
C   Subroutine to compute the apparent source elevations based on source
C   and antenna coordinates in common.  The routines GETANT and GETSOU
C   should be called before this routine to but the correct values in
C   the relevant commons.
C   Inputs:
C      ANTNO      I    Antenna number
C      TIME       D    Current data time (days).
C      RAPOBS     D    Apparent RA of pointing (radians)
C      DAPOBS     D    Apparent Dec of pointing (radians)
C   Input from common:
C      RAAPP      D    Apparent RA of source
C      DECAPP     D    Apparent Declination of source.
C      STNLAT     D(*) Antenna latitude (rad).
C      STNLON     D(*) Antenna east longitudes (rad).
C      GSTIAT     D    GST at IAT=0 of reference day (rad).
C      ROTIAT     D    Rotation of the earth rate in IAT.
C   Output:
C      PEL        D    Pointing elevation (rad)
C      PAZ        D    Pointing azimuth (rad)
C      TEL        D    Target source elevation (rad)
C      TAZ        D    Target source azimuth (rad)
C-----------------------------------------------------------------------
      INTEGER   ANTNO
      DOUBLE PRECISION TIME, RAPOBS, DAPOBS, PEL, PAZ, TEL, TAZ
C
      DOUBLE PRECISION HRANG, ANTLST, DARG, DARG2, DRA, DDEC, DAZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
C                                       Antenna LST
      ANTLST = GSTIAT + STNLON(ANTNO) + TIME * ROTIAT
C                                       Source position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAAPP
      DDEC = DECAPP
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAEPO
         DDEC = DECEPO
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
C                                       Limit to between 0 and 2pi
      HRANG = DMOD (HRANG, TWOPI)
C                                       Elevation angle
      DARG = SIN (STNLAT(ANTNO)) * SIN (DDEC) + COS (STNLAT(ANTNO))
     *   * COS (DDEC) * COS (HRANG)
      TEL = PI/2.0D0 - ACOS (DARG)
C                                       AZ = ATAN2(SD*CL - CD*SL*CH,
C                                       CD*SH)
      DARG = SIN (DDEC) * COS (STNLAT(ANTNO)) -
     *       COS (DDEC) * SIN (STNLAT(ANTNO)) * COS(HRANG)
      DARG2 = COS (DDEC) * SIN (HRANG)
      DAZ = ATAN2 (DARG, DARG2)
      DAZ = MOD (DAZ - PI/2.0D0, TWOPI)
      IF (DAZ.LT.0.0D0) DAZ = DAZ + TWOPI
      TAZ = DAZ
C                                       pointing position; if apparent
C                                       position missing use mean
C                                       position.
      DRA = RAPOBS
      DDEC = DAPOBS
      IF ((DRA.LE.1.0D-5) .AND. (DDEC.LE.1.0D-5)) THEN
         DRA = RAOBS
         DDEC = DECOBS
         END IF
C                                       Hour angle
      HRANG = ANTLST - DRA
C                                       Limit to between 0 and 2pi
      HRANG = DMOD (HRANG, TWOPI)
C                                       Elevation angle
      DARG = SIN (STNLAT(ANTNO)) * SIN (DDEC) + COS (STNLAT(ANTNO))
     *   * COS (DDEC) * COS (HRANG)
      PEL = PI/2.0D0 - ACOS (DARG)
C                                       AZ = ATAN2(SD*CL - CD*SL*CH,
C                                       CD*SH)
      DARG = SIN (DDEC) * COS (STNLAT(ANTNO)) -
     *       COS (DDEC) * SIN (STNLAT(ANTNO)) * COS(HRANG)
      DARG2 = COS (DDEC) * SIN (HRANG)
      DAZ = ATAN2 (DARG, DARG2)
      DAZ = MOD (DAZ - PI/2.0D0, TWOPI)
      IF (DAZ.LT.0.0D0) DAZ = DAZ + TWOPI
      PAZ = DAZ
C
 999  RETURN
      END
