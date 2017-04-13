LOCAL INCLUDE 'EVASN.INC'
C                                       Local include for EVASN
      INCLUDE 'INCS:PUVD.INC'
      REAL      XSIN, XDISIN, XNVER, XQUAL, XTIME(8), XBAND, XFREQ,
     *   XFQID, XBIF, XEIF, XSUBA, XANT(50), CUTOFF, DPARM(10)
      HOLLERITH XNAMEI(3), XCLAIN(2), XTYPE, XXSOUR(4,30), XXSTOK
      CHARACTER NAMEIN*12, CLAIN*6, TYPE*2, XSOUR(30)*16, XSTOK*4,
     *   OUTFIL*48
C                                       Program info
      REAL      TSTART, TSTOP, SELBAN
      INTEGER   SEQIN, DISKIN, CNOIN, IVER, BIF, ANTS(50), ISTOK,
     *   NPARMS, NID, SID(500), NANTSL, NPLOTS, SUMSTK, SUMIF, FRQSEL,
     *   SUMAMP, GRCHN, TVCHN, TVCORN(4), XVAR, ISOU, OSOU, IANT,
     *   SUBARR, EIF, EPOL, BPOL, NR, NA, NS
      LOGICAL   DOAWNT, DOERRB, NNODAT
      DOUBLE PRECISION SELFRQ, GNRECD(XCLRSZ/2)
C                                       SN/CL table info
      INTEGER CLBUFF(512), NCLINR, NUMANT, NUMPOL, NUMIF, ICLRNO,
     *   KOLS(40), KOLTYP(40), KOLDIM(40), ICLUN, GNRECI(XCLRSZ),
     *   TIMKOL, SOUKOL, ANTKOL, SUBKOL, FRQKOL, RE1KOL, IM1KOL, WT1KOL,
     *   RE2KOL, IM2KOL, WT2KOL, REKOL1, REKOL2, IMKOL1, IMKOL2, WTKOL1,
     *   WTKOL2, TIMECL
      REAL GNREC(XCLRSZ)
C                                       Constants
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XTYPE, XNVER,
     *   XXSOUR, XQUAL, XTIME, XXSTOK, XBAND, XFREQ, XFQID, XBIF, XEIF,
     *   XSUBA, XANT, CUTOFF, DPARM
      COMMON /VGNCOM/ SELFRQ, TSTART, TSTOP, SELBAN, NID, SID, NANTSL,
     *   NPLOTS, DOAWNT, SUMAMP, SUMSTK, SUMIF, FRQSEL, SEQIN, DISKIN,
     *   CNOIN, IVER, BIF, SUBARR, ANTS, ISTOK, NPARMS, GRCHN, TVCHN,
     *   TVCORN, XVAR, ISOU, OSOU, IANT, DOERRB, NNODAT, EIF, EPOL,
     *   BPOL, NR, NA, NS
      COMMON /VGNCHR/ NAMEIN, CLAIN, TYPE, XSOUR, XSTOK, OUTFIL
      COMMON /TABCOM/ GNREC, CLBUFF, NCLINR, NUMANT, NUMPOL, NUMIF,
     *   ICLRNO, KOLS, KOLTYP, KOLDIM, ICLUN,
     *   REKOL1, IMKOL1, WTKOL1, REKOL2, IMKOL2, WTKOL2, TIMECL
      EQUIVALENCE (GNREC, GNRECD, GNRECI)
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(3), SOUKOL),
     *   (KOLS(4), ANTKOL), (KOLS(5), SUBKOL), (KOLS(6), FRQKOL),
     *   (KOLS(13), RE1KOL), (KOLS(14), IM1KOL), (KOLS(17), WT1KOL),
     *   (KOLS(26), RE2KOL), (KOLS(27), IM2KOL), (KOLS(30), WT2KOL)
C                                                          End EVASN
LOCAL END
      PROGRAM EVASN
C-----------------------------------------------------------------------
C! Statistics in SN/CL table to evaluate quality
C# UV Calibration Editing
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2011-2012
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
C   EVASN reads an SN extension file and write entries into a flag
C   table.
C   which can be read into UVFLG.
C   Inputs:
C      INNAME.....UV file name (name).       Standard defaults.
C      INCLASS....UV file name (class).      Standard defaults.
C      INSEQ......UV file name (seq. #).     0 => highest.
C      INDISK.....Disk unit #.               0 => any.
C      INEXT......'SN' or 'CL' table to be examined
C      INVERS.....Version number of table to plot, 0=>highest no.
C      SOURCES....Source list.  '*' = all; a "-" before a source
C                 name means all except ANY source named.
C      TIMERANG...Time range of the data to be plotted. In order:
C                 Start day, hour, min. sec,
C                 end day, hour, min. sec. Days relative to ref.
C                 date.
C      STOKES.....The desired Stokes type of the output data:
C                 'R' = RCP, 'L' = LCP, 'DIFF' = difference
C      BIF........IF to plot
C      ANTENNAS...A list of the antennas to be plotted. All 0 => all.
C                 If any number is negative then all antennas listed
C                 are NOT to be plotted and all others are.
C      DPARM......(1) Maximum okay deviation from mean gain amp in
C                     sigmas   0 -> 6
C                 (2) Min okay cos (delta phase)  0 -> 0.5
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   IRET, CWORDS, IERR, NIF, NP, VWORDS
      LONGINT   VOFF, COFF
      REAL      VDATA(2), CDATA(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGN /'EVASN '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL EVASNI (PRGN, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       allocate memory
      NIF = EIF - BIF + 1
      NP = EPOL - BPOL + 1
      IF (SUMSTK.EQ.3) NP = 1
      CWORDS = (3 * NR + 1024) / 1024 + 1
      VWORDS = (3 * NP * NIF * NR + 1024) / 1024 + 1
      CALL ZMEMRY ('GET ', TSKNAM, CWORDS, CDATA, COFF, IRET)
      IF (IRET.NE.0) GO TO 995
      CALL ZMEMRY ('GET ', TSKNAM, VWORDS, VDATA, VOFF, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       get table data
      CALL EVASNG (NP, NIF, CDATA(1+COFF), VDATA(1+VOFF), IRET)

C                                       write flags...
      IF (IRET.EQ.0) CALL EVASND (NP, NIF, CDATA(1+COFF), VDATA(1+VOFF),
     *   IRET)
C                                       free up memory
      CALL ZMEMRY ('FREE', TSKNAM, CWORDS, CDATA, COFF, IERR)
      CALL ZMEMRY ('FREE', TSKNAM, VWORDS, VDATA, VOFF, IERR)
C                                       close down
 995  CALL DIE (IRET, CLBUFF)
C
 999  STOP
      END
      SUBROUTINE EVASNI (PRGN, IERR)
C-----------------------------------------------------------------------
C   Gets the input parameters for EVASN.
C   Inputs:
C      PRGN    C*6  Program name
C   Output in common:
C      SUMSTK  I    Selected Stokes 0=both, 1=R, 2=L, 4=difference
C      SUMIF   I    Selected IF, 0=>all
C      ISTOK   I    1 = R, 2 = L
C      NS      I    Number of sources
C      NR      I    Number records included
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, TYPTMP*2
      INTEGER   IRET, BUFF(256), I, J, K, JERR, QUAL(30), NSOUR, ISUBA,
     *   BUFFER(512), IROUND, LUN, NSTOK, NSOU(500)
      LOGICAL   T, F, MATCH
      REAL      VTIME
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OSOU = -1
      NPARMS = 206
C                                        Get input parameters.
      CALL SETUP (PRGN, NPARMS, XNAMEI, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         RQUICK = .FALSE.
         GO TO 990
         END IF
C                                       Decode inputs.
C                                       characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (2, 1, XTYPE, TYPE)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
         QUAL(I) = IROUND (XQUAL)
 20      CONTINUE
      IF (TYPE.EQ.' ') TYPE ='SN'
      XTYPE = HBLANK
      CALL CHR2H (2, TYPE, 1, XTYPE)
      IF (DPARM(1).LE.0.0) DPARM(1) = 6.0
      IF ((DPARM(2).GT.0.999) .OR. (DPARM(2).LT.-0.999) .OR.
     *   (DPARM(2).EQ.0.0)) DPARM(2) = 0.5
C                                       Integers
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      IVER = IROUND (XNVER)
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*3600.0))
      TSTOP = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*3600.0))
C                                        Defaults to plot all.
      IF (TSTART.GE.TSTOP) THEN
         TSTART = 0.0
         TSTOP = 999.0
         END IF
C                                       Find input catalog
      CNOIN = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      'UV', NLUSER
         GO TO 990
         END IF
C                                       Save name class etc.
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
C                                       Read catalog header
      STAT = 'READ'
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, STAT, BUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FCNO(NCFILE) = CNOIN
      FVOL(NCFILE) = DISKIN
      FRW(NCFILE) = 0
      XDISIN = DISKIN
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 990
      SEQIN = CATBLK(KIIMS)
      XSIN = SEQIN
C                                       IF'S
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         IF (BIF.LE.0) BIF = 1
         BIF = MIN (BIF, CATBLK(KINAX+JLOCIF))
         EIF = IROUND (XEIF)
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         EIF = MIN (EIF, CATBLK(KINAX+JLOCIF))
         END IF
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.GE.0) THEN
         LUN = 25
         CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *      FRQSEL, IERR)
         IF (.NOT.MATCH) THEN
            WRITE (MSGTXT,1070)
            IERR = 1
            GO TO 990
            END IF
         IF (IERR.GT.0) GO TO 999
         END IF
C                                       Look up sources
      NID = 500
      NSOUR = 30
      MSGSUP = 32000
      CALL SOURNO (XSOUR, QUAL, NSOUR, DISKIN, CNOIN, NID, BUFFER,
     *   SID, JERR)
      MSGSUP = 0
C                                       Trap problem with SU table.
      IF (JERR.NE.0) NID = 0
      IF ((NID.LE.0) .AND. (XSOUR(1).NE.' ')) NID = 1
C                                       Check if none specified
      IF ((SID(1).EQ.0) .AND. (XSOUR(1).EQ.' ')) NID = 0
C                                       Check antennas desired.
      NANTSL = 0
      DOAWNT = T
      DO 40 J = 1,50
         ANTS(J) = IROUND (XANT(J))
         IF (ANTS(J).LT.0) DOAWNT = F
C                                       Make positive
         ANTS(J) = ABS (ANTS(J))
         IF (NANTSL.GE.1) THEN
            DO 30 K = 1,NANTSL
               IF (ANTS(J).EQ.ANTS(K)) ANTS(J) = 0
 30            CONTINUE
C                                       Check for multiple entries
            IF (ABS (ANTS(J)).GE.1) NANTSL = J
            END IF
 40      CONTINUE
C                                       Make sure not too many
      IF (NANTSL.GT.MAXANT) NANTSL = MAXANT
C                                       Get antenna names
      CALL GETANT (DISKIN, CNOIN, 1, CATBLK, BUFFER, JERR)
C                                       Check and set Stokes
      NSTOK = CATBLK(KINAX+JLOCS)
      IF ((NSTOK.EQ.1) .AND. (ICOR0.EQ.-1)) XSTOK = 'R'
      IF (ICOR0.EQ.-2) XSTOK = 'L'
      IF (ICOR0.GT.0) XSTOK = 'I'
      BPOL = 1
      EPOL = MIN (2, NSTOK)
      IF ((XSTOK(1:1).EQ.'V') .OR. (XSTOK.EQ.'DIFF')) THEN
         ISTOK = 1
         SUMSTK = 3
      ELSE IF (XSTOK(1:1).EQ.'L') THEN
         IF (ICOR0.EQ.-2) THEN
            EPOL = 1
         ELSE
            BPOL = 2
            END IF
         ISTOK = 2
         SUMSTK = 2
      ELSE
         ISTOK = 1
         SUMSTK = 0
         IF (XSTOK.EQ.'R') THEN
            EPOL = 1
            SUMSTK = 1
            END IF
         END IF
C                                       Open SN or CL table
      IF ((TYPE.EQ.'SN') .OR. (TYPE.EQ.'CL')) THEN
         CALL SNPOPN (IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Invalid table type
      ELSE
         IERR = 5
         MSGTXT = 'INVALID TABLE TYPE =' // TYPE
         GO TO 990
         END IF
C                                       count up times
      NR = 0
C                                       Loop thru data
      DO 100 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1090) IERR
            GO TO 990
C                                       Record within specified
C                                       time range ?
         ELSE IF (IERR.EQ.0) THEN
            IF (KOLTYP(TIMECL).EQ.1) THEN
               VTIME = GNRECD(TIMKOL)
            ELSE
               VTIME = GNREC(TIMKOL)
               END IF
            IF ((VTIME.LT.TSTART) .OR. (VTIME.GT.TSTOP)) GO TO 100
C                                       Freq id
            IF (GNRECI(FRQKOL).NE.-1) THEN
               IF ((GNRECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *            GO TO 100
               END IF
C                                       Antenna?
            IANT = GNRECI(ANTKOL)
            IF (NANTSL.GT.0) THEN
               DO 50 I = 1,NANTSL
                  IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 60
                  IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 100
 50               CONTINUE
               IF (DOAWNT) GO TO 100
               END IF
C                                       Check source
 60         ISOU = 1
            IF (NID.GT.0) THEN
               ISOU = GNRECI(SOUKOL)
               DO 70 I = 1,NID
                  IF (ISOU.EQ.SID(I)) GO TO 80
 70               CONTINUE
               GO TO 100
               END IF
C                                       Subarray check
 80         ISUBA = GNRECI(SUBKOL)
            IF ((ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR)) GO TO 100
            NR = NR + 1
            NSOU(ISOU) = 1
            END IF
 100     CONTINUE
      IF (XSOUR(1).EQ.'ALL') THEN
         NS = 1
      ELSE
         NS = 0
         DO 110 J = 1,500
            IF (NSOU(J).GT.0) NS = J
 110        CONTINUE
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR;',I7,'GETTING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',I3,
     *   ' TYPE=',A2,' USER=',I4)
 1040 FORMAT ('ERROR',I3,' COPYING CATALOG HEADER')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1090 FORMAT ('EVASNI: ERROR =',I3,' FROM TABIO')
 1100 FORMAT ('REQUESTED STOKES IS',I3,', BUT ICOR0=',I3,
     *   ' AND NSTOK =',I3)
      END
      SUBROUTINE EVASNG (NP, NI, CDATA, VDATA, IERR)
C-----------------------------------------------------------------------
C   EVASNG gets the SN/CL data for analysis
C   Input/Output in common:
C      TSTART   R      Start time
C      TSTOP    R      Stop time
C   Input:
C      NP       I      Number polarizations in PDATA
C      NI       I      Number IFs in PDATA
C   Output:
C      CDATA    R(*)   Control data (T/Sou/Ant, row)
C      VDATA    R(*)   Visibility data (RE/IM/A, NP, NI, row)
C      IERR     I      Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NP, NI, IERR
      REAL      VDATA(3,NP,NI,*), CDATA(3,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   I, J, ISUBA, LSOU, NRIN, INS
      REAL      TB, TE, GTIME
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Loop thru data
      NRIN = NR
      INS = NS
      NR = 0
      NA = 0
      NS = 0
      LSOU = 1
      DO 50 J = 1,NCLINR
         ICLRNO = J
         CALL TABIO ('READ', 0, ICLRNO, GNREC, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 50
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Record within specified
C                                       time range ?
         IF (KOLTYP(TIMECL).EQ.1) THEN
            GTIME = GNRECD(TIMKOL)
         ELSE
            GTIME = GNREC(TIMKOL)
            END IF
         IF ((GTIME.LT.TSTART) .OR. (GTIME.GT.TSTOP)) GO TO 50
C                                       Freq id
         IF (GNRECI(FRQKOL).NE.-1) THEN
            IF ((GNRECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0))
     *         GO TO 50
            END IF
C                                       Antenna?
         IANT = GNRECI(ANTKOL)
         IF (NANTSL.GT.0) THEN
            DO 10 I = 1,NANTSL
               IF ((IANT.EQ.ANTS(I)).AND.DOAWNT) GO TO 20
               IF ((IANT.EQ.ANTS(I)).AND.(.NOT.DOAWNT)) GO TO 50
 10            CONTINUE
            IF (DOAWNT) GO TO 50
            END IF
C                                       Check source
 20      ISOU = GNRECI(SOUKOL)
         IF (NID.GT.0) THEN
            DO 30 I = 1,NID
               IF (ISOU.EQ.SID(I)) GO TO 40
 30            CONTINUE
            GO TO 50
            END IF
         IF (INS.EQ.1) ISOU = 1
C                                       Subarray check
 40      ISUBA = GNRECI(SUBKOL)
         IF ((ISUBA.GT.0) .AND. (ISUBA.NE.SUBARR)) GO TO 50
         IF (XSOUR(1).NE.'ALL') LSOU = ISOU
         TB = MIN (TB, GTIME)
         TE = MAX (TE, GTIME)
C                                       Get value - checking weight
         NR = NR + 1
         CALL SNDAT (VDATA(1,1,1,NR), CDATA(1,NR))
         CDATA(2,NR) = LSOU
         CDATA(3,NR) = IANT
         NS = MAX (NS, LSOU)
         NA = MAX (NA, IANT)
 50      CONTINUE
      CALL TABIO ('CLOS', 0, ICLRNO, GNREC, CLBUFF, I)
      IF (NR.NE.NRIN) THEN
         WRITE (MSGTXT,1050) NR, NRIN
         CALL MSGWRT (7)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('EVASNG: ERROR =',I3,' FROM TABIO')
 1050 FORMAT ('EVASNG: READ',I8,' RECORDS, EXPECTED',I8)
      END
      SUBROUTINE SNPOPN (IERR)
C-----------------------------------------------------------------------
C   Routine to open SN, CL, PC or TY table and get necessary information
C   Input from Common:
C      TYPE     C*2  'SN', 'CL'
C      DISKIN   I     Disk number
C      CNOIN    I     Catalog slot number
C      CATBLK   I(*)  Catalog header
C      SUMSTK   I     Stokes type requested 0=both, 1=R, 2=L,
C                     3=difference
C   Output:
C      IERR     I     Error code, 0=OK else failed.
C   Output in common:
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of gain records in file.
C      NUMANT       I    Number of antennas
C      NUMPOL       I    Number of polarizations
C      NUMIF        I    Number of IFs.
C      KOLS         I(*) Column pointers
C      KOLTYP       I(*) Column data types
C      KOLDIM       I(*) Column dimension
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER KEYW(4)*8, COLHD1(11)*24, COLHD2(13)*24, COLHD3(13)*24,
     *   COLTAB(40)*24, COLHED(37)*24, KEYSN(4)*8
      INTEGER   NKEY, NREC, NCOL, DATP(128,2), IPOINT, KEYTYP(4),
     *   KLOCS(4), KEYVAL(5), I, KP, MSGSAV
      LOGICAL   T
      REAL      KEYVR(5)
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DANS.INC'
      EQUIVALENCE (KEYVAL, KEYVR)
      EQUIVALENCE (COLHED(1), COLHD1), (COLHED(12), COLHD2),
     *   (COLHED(25), COLHD3)
      DATA COLHD1 /'TIME                    ',
     *   'TIME INTERVAL           ',
     *   'SOURCE ID               ', 'ANTENNA NO.             ',
     *   'SUBARRAY                ', 'FREQ ID                 ',
     *   'I.FAR.ROT               ',
     *   'GEODELAY                ', 'DOPPOFF                 ',
     *   'ATMOS                   ', 'DATMOS                  '/
      DATA COLHD2 /'MBDELAY1      ',
     *   'REAL1                   ', 'IMAG1                   ',
     *   'RATE 1                  ', 'DELAY 1                 ',
     *   'WEIGHT 1                ', 'REFANT 1                ',
     *   'TSYS 1                  ', 'TANT 1                  ',
     *   'CLOCK 1                 ', 'DCLOCK 1                ',
     *   'DISP 1                  ', 'DDISP 1                 '/
      DATA COLHD3 /'MBDELAY2      ',
     *   'REAL2                   ', 'IMAG2                   ',
     *   'RATE 2                  ', 'DELAY 2                 ',
     *   'WEIGHT 2                ', 'REFANT 2                ',
     *   'TSYS 2                  ', 'TANT 2                  ',
     *   'CLOCK 2                 ', 'DCLOCK 2                ',
     *   'DISP 2                  ', 'DDISP 2                 '/
      DATA KEYSN /'NO_ANT  ', 'NO_POL  ', 'NO_IF   ','MGMOD   '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Open table
      ICLUN = 28
      NKEY = 0
      NREC = 0
      NCOL = 0
      ICLRNO = 1
      CALL TABINI ('READ', TYPE, DISKIN, CNOIN, IVER, CATBLK, ICLUN,
     *   NKEY, NREC, NCOL, DATP, CLBUFF, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1100) IERR, TYPE, IVER
         GO TO 980
         END IF
C                                       Get number of scans
      NCLINR = CLBUFF(5)
C                                       Check if empty
      IF (NCLINR.LE.0) THEN
         IERR = 6
         MSGTXT = 'ERROR: SELECTED TABLE IS EMPTY'
         GO TO 980
         END IF
C                                       Get column pointers
      NKEY = MAXCLC
      DO 10 I = 1,NKEY
         COLTAB(I) = COLHED(I)
   10    CONTINUE
      CALL FNDCOL (NKEY, COLTAB, 24, T, CLBUFF, KOLS, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.10)) GO TO 999
      IERR = 0
C                                       Time column logical number
      TIMECL = KOLS(1)
C                                       Convert to pointers, types
      DO 100 I = 1,NKEY
         KP = KOLS(I)
         IF (KP.GT.0) THEN
            KOLS(I) = DATP(KP,1)
            KOLTYP(I) = MOD (DATP(KP,2), 10)
            KOLDIM(I) = DATP(KP,2) / 10
         ELSE
            KOLS(I) = -1
            KOLTYP(I) = -1
            KOLDIM(I) = 0
            END IF
 100     CONTINUE
C                                       Table keywords
      NKEY = 4
      DO 20 I = 1,NKEY
         KEYW(I) = KEYSN(I)
   20    CONTINUE
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', KEYW, NKEY, CLBUFF, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      MSGSUP = MSGSAV
      IF ((IERR.GE.1) .AND. (IERR.LE.20)) GO TO 999
      IERR = 0
C                                       No. antennas.
      NUMANT = NSTNS
      IPOINT = KLOCS(1)
      IF (IPOINT.GT.0) NUMANT = KEYVAL(IPOINT)
C                                       No. poln.
      NUMPOL = 1
      IPOINT = KLOCS(2)
      IF (IPOINT.GT.0) NUMPOL = KEYVAL(IPOINT)
C                                       No. IF
      NUMIF = 1
      IPOINT = KLOCS(3)
      IF (IPOINT.GT.0) NUMIF = KEYVAL(IPOINT)
C                                       Set pointers
C                                       1st poln
      IF (BPOL.EQ.1) THEN
         REKOL1 = RE1KOL - 1
         IMKOL1 = IM1KOL - 1
         WTKOL1 = WT1KOL - 1
C                                       2nd poln
      ELSE
         REKOL1 = RE2KOL - 1
         IMKOL1 = IM2KOL - 1
         WTKOL1 = WT2KOL - 1
         END IF
C                                       2nd Poln
      REKOL2 = RE2KOL - 1
      IMKOL2 = IM2KOL - 1
      WTKOL2 = WT2KOL - 1
C                                       Requested data not in table
      IF ((REKOL1.LT.0) .OR. (IMKOL1.LT.0) .OR. ((EPOL.NE.BPOL) .AND.
     *   ((REKOL2.LT.0) .OR. (IMKOL2.LT.0)))) THEN
         WRITE(MSGTXT,1500) TYPE
         IERR = 10
         END IF
C                                       Error
 980  IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR ',I3,' OPENING ',A,' TABLE NO. ',I3)
 1500 FORMAT (' REQUESTED DATA NOT IN ',A,' TABLE ')
      END
      SUBROUTINE SNDAT (VALUE, XVARIB)
C-----------------------------------------------------------------------
C   Routine to return amplitude and time from gain record
C   Input from common:
C      GNREC    R(*)  Table record
C   Also uses pointers etc. set in SNPOPN
C   Output:
C      VALUE    R     Phase value, magic value blanked
C      XVARIB   R     Time
C-----------------------------------------------------------------------
      REAL     VALUE(3,2,*), XVARIB
C
      INTEGER   LIF
      REAL      V1, V2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Time
      IF (KOLTYP(TIMECL).EQ.1) THEN
         XVARIB = GNRECD(TIMKOL)
      ELSE
         XVARIB = GNREC(TIMKOL)
         END IF
C                                       In case the data are bad
      DO 10 LIF = BIF,EIF
         VALUE(1,1,LIF-BIF+1) = FBLANK
         VALUE(2,1,LIF-BIF+1) = FBLANK
         VALUE(3,1,LIF-BIF+1) = FBLANK
         VALUE(1,2,LIF-BIF+1) = FBLANK
         VALUE(2,2,LIF-BIF+1) = FBLANK
         VALUE(3,2,LIF-BIF+1) = FBLANK
C                                       amplitude
         IF ((GNREC(REKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL1+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL1+LIF).GE.CUTOFF)) THEN
            V1 = FBLANK
            IF (SUMSTK.EQ.3) THEN
               IF ((GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *            (GNREC(WTKOL2+LIF).GE.CUTOFF)) THEN
                  V2 = GNREC(IMKOL2+LIF)**2 + GNREC(REKOL2+LIF)**2
                  IF (V2.GT.0.0) THEN
                     V1 = (GNREC(REKOL1+LIF)*GNREC(REKOL2+LIF) +
     *                  GNREC(IMKOL1+LIF)*GNREC(IMKOL2+LIF)) / V2
                     V2 = (GNREC(IMKOL1+LIF)*GNREC(REKOL2+LIF) -
     *                  GNREC(IMKOL1+LIF)*GNREC(REKOL2+LIF)) / V2
                  ELSE
                     V1 = FBLANK
                     END IF
                  END IF
            ELSE
               V1 = GNREC(IMKOL1+LIF)
               V2 = GNREC(REKOL1+LIF)
               END IF
            IF (V1.NE.FBLANK) THEN
               VALUE(1,1,LIF-BIF+1) = V1
               VALUE(2,1,LIF-BIF+1) = V2
               VALUE(3,1,LIF-BIF+1) = SQRT (V1*V1 + V2*V2)
               END IF
            END IF
         IF ((SUMSTK.EQ.0) .AND.(GNREC(REKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(IMKOL2+LIF).NE.FBLANK) .AND.
     *      (GNREC(WTKOL2+LIF).GE.CUTOFF)) THEN
            V1 = GNREC(IMKOL2+LIF)
            V2 = GNREC(REKOL2+LIF)
            VALUE(1,2,LIF-BIF+1) = V1
            VALUE(2,2,LIF-BIF+1) = V2
            VALUE(3,2,LIF-BIF+1) = SQRT (V1*V1 + V2*V2)
            END IF
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SOURNO (SOURCE, QUAL, NSOUR, DISK, CNO, NID, BUFFER,
     *   ID, IRET)
C-----------------------------------------------------------------------
C   Determines the source numbers of a list of source names from the
C   source table associated with a specified catalog entry.
C   LOCAL VERSION: REQUIRES CALCODE NOT BLANK AND NOT NONE.
C   Inputs:
C      SOURCE   C*16(*)  List of source names.
C                        If the first character of any source names
C                        begins with a "-", all sources EXCEPT those
C                        named will be returned ( the "-" will be
C                        ignored in determining the source name).
C                        Blank source names are ignored.  Names should
C                        be left justified, blank filled
C      QUAL      I(*)    SOURCE qualifiers, .lt. 0 => any.
C      NSOUR     I       Number of entries in SOURCE, may include
C                        blank names.
C      DISK      I       Disk number of the data set.
C      CNO       I       Catalog slot number of data set.
C   Input/Output:
C      NID       I       On input the maximum number of elements to be
C                        filled in ID; on output, the number of elements
C                        in ID. 0 => all selected.
C      BUFFER    I(512)  Work buffer, used for I/O and manipulating
C                        source lists, should be at least min (512,NID)
C   Output:
C      ID        I(*)    Source ID numbers of selected sources,
C                        If ID(1)=0 then all sources are selected.
C      IRET      I       Return code. 0 => OK; else failed.
C                           -1 => source/qual specified and not found
C                                 no message is generated
C   Usage notes:
C       This routine uses AIPS LUN 27 which will be closed on normal
C   return.
C       Version 1 of the source table is assumed.
C-----------------------------------------------------------------------
      CHARACTER SOURCE(*)*16
      INTEGER   QUAL(*), NSOUR, DISK, CNO, NID, BUFFER(*), ID(*), IRET,Q
C
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4
      INTEGER   CAT(256), VER, LUN, IDKOL, SUKOL, I, IDSOU, SQUAL, J,
     *   MAXID, NUMIF, ISURNO, NUMREC, I4, SUFQID
      LOGICAL   EQUAL, DESEL, ALLSEL, GOTIT, ALLBLN
      DOUBLE PRECISION    BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP,
     *   PMRA, PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IDKOL, SUKOLS(1)),   (SUKOL, SUKOLS(2))
      DATA VER, LUN /1, 27/
C-----------------------------------------------------------------------
C                                       Setup
      MAXID = NID
      NID = 0
      IRET = 0
C                                       Check if sources deselected or
C                                       if all sources are selected.
      DESEL = .FALSE.
      ALLSEL = .TRUE.
      ALLBLN = .TRUE.
      DO 10 I = 1,NSOUR
C                                       Check deselection
         DESEL = DESEL .OR. SOURCE(I)(1:1).EQ.'-'
C                                       Check if all blank (GvM, 1/93)
         ALLBLN = ALLBLN .AND. (SOURCE(I).EQ.' ')
C                                       Check if all blank, no qual
         ALLSEL = ALLSEL .AND. (SOURCE(I).EQ.' ') .AND. (QUAL(I).LT.0)
 10      CONTINUE
      ALLSEL = (ALLSEL) .OR. (SOURCE(1).EQ.'ALL')
C                                       Check all selected case.
      ID(1) = 0
C                                       Get catalog header.
      CALL CATIO ('READ', DISK, CNO, CAT, 'REST', BUFFER, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Initialize SOURCE table.
      CALL SOUINI ('READ', BUFFER, DISK, CNO, VER, CAT, LUN, NUMIF,
     *   VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
         END IF
C                                       Get number of entries
      NUMREC = BUFFER(5)
C                                       Loop through source records.
      DO 50 ISURNO = 1,NUMREC
C                                       Read record
         I4 = ISURNO
         CALL TABSOU ('READ', BUFFER, I4, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, SQUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
C                                       See is source record turned off
         IF (IRET.LT.0) GO TO 50
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Require CALCODE
         IF ((CALCOD.EQ.' ') .OR. (CALCOD.EQ.'NONE')) GO TO 50
C                                       all selected if calcode ok
         IF (ALLSEL) THEN
            IF ((NID+1).GT.MAXID) GO TO 980
            NID = NID + 1
            ID(NID) = IDSOU
C                                       Check if in list SOURCE.
         ELSE
            GOTIT = .FALSE.
            DO 30 J = 1,NSOUR
C                                       Sources selected.
               IF (.NOT.DESEL) THEN
                  EQUAL = SOURCE(J).EQ.SOUNAM
C                                       Check QUAL
                  Q = QUAL(J)
C                                       is qualifier OK, too?
                  EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *               (QUAL(J).EQ.SQUAL))
C                                       cover blank source and
C                                       given qualifier
                  EQUAL = EQUAL.OR.(QUAL(J).EQ.SQUAL.AND.ALLBLN)
                  IF (EQUAL) THEN
                     IF ((NID+1).GT.MAXID) GO TO 980
                     NID = NID + 1
                     ID(NID) = IDSOU
                     GO TO 50
                     END IF
C                                       Deselected
               ELSE
C                                       Check for leading "-"
                  IF (SOURCE(J)(1:1).EQ.'-') THEN
                     EQUAL = SOURCE(J)(2:16).EQ.SOUNAM(1:15)
                  ELSE
                     EQUAL = SOURCE(J).EQ.SOUNAM
                     END IF
C                                       Check QUAL
                  EQUAL = EQUAL .AND. ((QUAL(J).LT.0) .OR.
     *               (QUAL(J).EQ.SQUAL))
                  GOTIT = GOTIT .OR. EQUAL
                  END IF
 30            CONTINUE
C                                       Source not deselected
            IF (DESEL .AND. (.NOT.GOTIT)) THEN
               IF ((NID+1).GT.MAXID) GO TO 980
               NID = NID + 1
               ID(NID) = IDSOU
               END IF
            END IF
 50      CONTINUE
C                                       Close Source table
      CALL TABIO ('CLOS', 0, I4, SOUNAM, BUFFER, IRET)
      IF (NID.EQ.0) IRET = -1
      GO TO 999
C                                       Too many sources selected
 980  WRITE (MSGTXT,1980) MAXID
      IRET = 5
C                                       Error
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('SOURNO: ERROR ',I3,' READING CATBLK FOR SOURCE TABLE')
 1100 FORMAT ('SOURNO: ERROR ',I3,' INITIALIZING SOURCE TABLE')
 1120 FORMAT ('SOURNO: ERROR ',I3,' READING SOURCE TABLE')
 1980 FORMAT ('SOURNO: MORE SOURCES SELECTED THAN MAX (',I5,')')
      END
      SUBROUTINE EVASND (NP, NI, CDATA, VDATA, IERR)
C-----------------------------------------------------------------------
C   EVASND computes statistics on the vis gain data
C   Input/Output in common:
C      TSTART   R      Start time
C      TSTOP    R      Stop time
C   Input:
C      NP       I      Number polarizations in VDATA
C      NI       I      Number IFs in VDATA
C      CDATA    R(*)   Control data (T/Sou/Ant, row)
C      VDATA    R(*)   Visibility data (RE/IM/A, NP, NI, row)
C   Output:
C      IERR     I      Error code, 0=OK else failed
C-----------------------------------------------------------------------
      INTEGER   NP, NI, IERR
      REAL      VDATA(3,NP,NI,*), CDATA(3,*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NC, LS, LA, J, LR, COUNT, IROUND, NSUM(2,MAXIF), IS,
     *   NAVRMS(2,MAXIF), NDVRMS(2,MAXIF), LIF, LP, NTVRMS(2,MAXIF),
     *   NAVJMP(2,MAXIF), NDVJMP(2,MAXIF), NTVJMP(2,MAXIF)
      DOUBLE PRECISION V, RMS(2,MAXIF), AVG(2,MAXIF), SUMA(2,MAXIF),
     *   SUMS(2,MAXIF), AVRMS(2,MAXIF), DAVRMS(2,MAXIF), AVJMP(2,MAXIF),
     *   AVAMP(2,MAXIF), DVJMP(2,MAXIF), AVJRMS(2,MAXIF), RM(10),
     *   PHASE(2,MAXIF), PHOLD(2,MAXIF)
      CHARACTER STARS(2)*1
      INCLUDE 'EVASN.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA RM /6.D0,5.D0,4.D0,3.D0,3.D0,3.D0,3.D0,3.D0,3.D0,3.D0/
      DATA STARS /' ','*'/
C-----------------------------------------------------------------------
C                                       examine amplitudes
C                                       outer loop
      NC = NP * NI
      CALL FILL (NC, 0, NAVRMS)
      CALL FILL (NC, 0, NDVRMS)
      CALL FILL (NC, 0, NTVRMS)
      CALL DFILL (NC, 0.0D0, AVAMP)
      CALL DFILL (NC, 0.0D0, AVRMS)
      CALL DFILL (NC, 0.0D0, DAVRMS)
      DO 70 LS = 1,NS
         DO 65 LA = 1,NA
            CALL DFILL (NC, 1000.D0, RMS)
            CALL DFILL (NC, 0.D0, AVG)
            J = 0
C                                       robust RMS loop
 10         CALL DFILL (NC, 0.0D0, SUMA)
            CALL DFILL (NC, 0.0D0, SUMS)
            CALL FILL (NC, 0, NSUM)
            J = J + 1
            COUNT = 0
            DO 25 LR = 1,NR
               IANT = IROUND (CDATA(3,LR))
               ISOU = IROUND (CDATA(2,LR))
               IF ((IANT.EQ.LA) .AND. (ISOU.EQ.LS)) THEN
                  DO 20 LIF = 1,NI
                     DO 15 LP = 1,NP
                        IF (VDATA(3,LP,LIF,LR).NE.FBLANK) THEN
                           V = VDATA(3,LP,LIF,LR)
                           IF (ABS(V-AVG(LP,LIF)).LE.RM(J)*RMS(LP,LIF))
     *                        THEN
                              SUMA(LP,LIF) = SUMA(LP,LIF) + V
                              SUMS(LP,LIF) = SUMS(LP,LIF) + V * V
                              NSUM(LP,LIF) = NSUM(LP,LIF) + 1.0
                              COUNT = COUNT + 1
                              END IF
                           END IF
 15                     CONTINUE
 20                  CONTINUE
                  END IF
 25            CONTINUE
C                                       found something
            IF (COUNT.GT.0) THEN
               DO 35 LIF = 1,NI
                  DO 30 LP = 1,NP
                     IF (NSUM(LP,LIF).GT.0) THEN
                        AVG(LP,LIF) = SUMA(LP,LIF) / NSUM(LP,LIF)
                        RMS(LP,LIF) = SUMS(LP,LIF) / NSUM(LP,LIF) -
     *                     AVG(LP,LIF)**2
                        IF (RMS(LP,LIF).GE.0.0D0) THEN
                           RMS(LP,LIF) = SQRT (RMS(LP,LIF))
                        ELSE IF (J.EQ.7) THEN
                           RMS(LP,LIF) = -1.0D0
                        ELSE
                           RMS(LP,LIF) = 0.0D0
                           END IF
                     ELSE IF (J.EQ.7) THEN
                        RMS(LP,LIF) = -1.0D0
                        END IF
 30                  CONTINUE
 35               CONTINUE
               IF (J.LT.7) GO TO 10
C                                       robust rms found
               DO 45 LP = 1,NP
                  DO 40 LIF = 1,NI
                     IF (RMS(LP,LIF).GE.0.0D0) THEN
                        NAVRMS(LP,LIF) = NAVRMS(LP,LIF) + 1
                        AVRMS(LP,LIF) = AVRMS(LP,LIF) + RMS(LP,LIF)
                        AVAMP(LP,LIF) = AVAMP(LP,LIF) + AVG(LP,LIF)
                        END IF
 40                  CONTINUE
 45               CONTINUE
C                                       count outliers
               DO 60 LR = 1,NR
                  IANT = IROUND (CDATA(3,LR))
                  ISOU = IROUND (CDATA(2,LR))
                  IF ((IANT.EQ.LA) .AND. (ISOU.EQ.LS)) THEN
                     DO 55 LP = 1,NP
                        DO 50 LIF = 1,NI
                           IF ((VDATA(3,LP,LIF,LR).NE.FBLANK) .AND.
     *                        (RMS(LP,LIF).GT.0.0)) THEN
                              V = ABS (VDATA(3,LP,LIF,LR) - AVG(LP,LIF))
     *                           / RMS(LP,LIF)
                              NTVRMS(LP,LIF) = NTVRMS(LP,LIF) + 1
                              IF (V.GT.DPARM(1)) THEN
                                 NDVRMS(LP,LIF) = NDVRMS(LP,LIF) + 1
                                 DAVRMS(LP,LIF) = DAVRMS(LP,LIF) + V
                                 END IF
                              END IF
 50                        CONTINUE
 55                     CONTINUE
                     END IF
 60               CONTINUE
               END IF
 65         CONTINUE
 70      CONTINUE
C                                       write amp summary
      MSGTXT = '*** Average gain amplitudes ***'
      CALL MSGWRT (4)
      MSGTXT = '  IF     Avg R gain    Rms R gain' //
     *   '     Avg L gain    Rms L gain'
      CALL MSGWRT (4)
      DO 80 LIF = 1,NI
         IS = 1
         DO 75 LP = 1,NP
            IF (NAVRMS(LP,LIF).GT.0) THEN
               AVRMS(LP,LIF) = AVRMS(LP,LIF) / NAVRMS(LP,LIF)
               AVAMP(LP,LIF) = AVAMP(LP,LIF) / NAVRMS(LP,LIF)
               IF (AVRMS(LP,LIF).GT.0.01*AVAMP(LP,LIF)) IS = 2
               END IF
 75         CONTINUE
         WRITE (MSGTXT,1075) STARS(IS), LIF, (AVAMP(LP,LIF),
     *      AVRMS(LP,LIF), LP = 1,NP)
         CALL MSGWRT (4)
 80      CONTINUE
      WRITE (MSGTXT,1080) DPARM(1)
      CALL MSGWRT (4)
      MSGTXT = '  IF  R frac excess  Avg R excess' //
     *      '  L frac excess  Avg L excess'
      CALL MSGWRT (4)
      DO 90 LIF = 1,NI
         IS = 1
         DO 85 LP = 1,NP
            SUMA(LP,LIF) = NDVRMS(LP,LIF)
            IF (NTVRMS(LP,LIF).GT.0) SUMA(LP,LIF) = SUMA(LP,LIF) /
     *         NTVRMS(LP,LIF)
            IF (NDVRMS(LP,LIF).GT.0) THEN
               DAVRMS(LP,LIF) = DAVRMS(LP,LIF) / NDVRMS(LP,LIF)
               IS = 2
               END IF
 85         CONTINUE
         WRITE (MSGTXT,1075) STARS(IS), LIF, (SUMA(LP,LIF),
     *      DAVRMS(LP,LIF), LP = 1,NP)
         CALL MSGWRT (4)
 90      CONTINUE
C                                       now look at phase jumps
      CALL FILL (NC, 0, NAVJMP)
      CALL FILL (NC, 0, NDVJMP)
      CALL FILL (NC, 0, NTVJMP)
      CALL DFILL (NC, 0.0D0, AVJMP)
      CALL DFILL (NC, 0.0D0, DVJMP)
      CALL DFILL (NC, 0.0D0, AVJRMS)
      DO 150 LS = 1,NS
         DO 145 LA = 1,NA
            COUNT = 0
            CALL DFILL (NC, DBLANK, PHOLD)
            CALL DFILL (NC, 0.0D0, SUMA)
            CALL DFILL (NC, 0.0D0, SUMS)
            CALL FILL (NC, 0, NSUM)
            DO 125 LR = 1,NR
               IANT = IROUND (CDATA(3,LR))
               ISOU = IROUND (CDATA(2,LR))
               IF ((IANT.EQ.LA) .AND. (ISOU.EQ.LS)) THEN
                  DO 120 LIF = 1,NI
                     DO 115 LP = 1,NP
                        IF (VDATA(3,LP,LIF,LR).NE.FBLANK) THEN
                           PHASE(LP,LIF) = ATAN2 (VDATA(2,LP,LIF,LR),
     *                        VDATA(1,LP,LIF,LR))
                           IF (PHOLD(LP,LIF).NE.DBLANK) THEN
                              V = COS (PHASE(LP,LIF) - PHOLD(LP,LIF))
                              NSUM(LP,LIF) = NSUM(LP,LIF) + 1
                              SUMA(LP,LIF) = SUMA(LP,LIF) + V
                              SUMS(LP,LIF) = SUMS(LP,LIF) + V * V
                              COUNT = COUNT + 1
                              IF (V.LT.DPARM(2)) THEN
                                 NDVJMP(LP,LIF) = NDVJMP(LP,LIF) + 1
                                 DVJMP(LP,LIF) = DVJMP(LP,LIF) + V
                                 END IF
                              NTVJMP(LP,LIF) = NTVJMP(LP,LIF) + 1
                              END IF
                        ELSE
                           PHASE(LP,LIF) = DBLANK
                           END IF
 115                    CONTINUE
 120                 CONTINUE
                  CALL DPCOPY (NC, PHASE, PHOLD)
                  END IF
 125           CONTINUE
            IF (COUNT.GT.0) THEN
               DO 135 LIF = 1,NI
                  DO 130 LP = 1,NP
                     IF (NSUM(LP,LIF).GT.0) THEN
                        SUMA(LP,LIF) = SUMA(LP,LIF) / NSUM(LP,LIF)
                        SUMS(LP,LIF) = SUMS(LP,LIF) / NSUM(LP,LIF) -
     *                     SUMA(LP,LIF) * SUMA(LP,LIF)
                        SUMS(LP,LIF) = SQRT (MAX (0.0D0, SUMS(LP,LIF)))
                        NAVJMP(LP,LIF) = NAVJMP(LP,LIF) + 1
                        AVJMP(LP,LIF) = AVJMP(LP,LIF) + SUMA(LP,LIF)
                        AVJRMS(LP,LIF) = AVJRMS(LP,LIF) + SUMS(LP,LIF)
                        END IF
 130                 CONTINUE
 135              CONTINUE
               END IF
 145        CONTINUE
 150     CONTINUE
      MSGTXT = '*** Correlation judged by cos (Delta phase) ***'
      CALL MSGWRT (4)
C                                       write phase summary
      MSGTXT = '  IF     Avg R corr    Rms R corr' //
     *   '     Avg L corr    Rms L corr'
      CALL MSGWRT (4)
      DO 160 LIF = 1,NI
         IS = 1
         DO 155 LP = 1,NP
            IF (NAVJMP(LP,LIF).GT.0) THEN
               AVJMP(LP,LIF) = AVJMP(LP,LIF) / NAVJMP(LP,LIF)
               AVJRMS(LP,LIF) = AVJRMS(LP,LIF) / NAVJMP(LP,LIF)
               IF (AVJRMS(LP,LIF).GT.0.1) IS = 2
               END IF
 155        CONTINUE
         WRITE (MSGTXT,1075) STARS(IS), LIF, (AVJMP(LP,LIF),
     *      AVJRMS(LP,LIF), LP = 1,NP)
         CALL MSGWRT (4)
 160     CONTINUE
      WRITE (MSGTXT,1160) DPARM(2)
      CALL MSGWRT (4)
      MSGTXT = '  IF     R frac low     Avg R low' //
     *      '     L frac low     Avg L low'
      CALL MSGWRT (4)
      DO 170 LIF = 1,NI
         IS = 1
         DO 165 LP = 1,NP
            SUMA(LP,LIF) = NDVJMP(LP,LIF)
            IF (NTVJMP(LP,LIF).GT.0) SUMA(LP,LIF) = SUMA(LP,LIF) /
     *         NTVJMP(LP,LIF)
            IF (NDVJMP(LP,LIF).GT.0) THEN
               DVJMP(LP,LIF) = DVJMP(LP,LIF) / NDVJMP(LP,LIF)
               IS = 2
               END IF
 165        CONTINUE
         WRITE (MSGTXT,1075) STARS(IS), LIF, (SUMA(LP,LIF),
     *      DVJMP(LP,LIF), LP = 1,NP)
         CALL MSGWRT (4)
 170     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1075 FORMAT (A1,I3,F15.5,F14.5,F15.5,F14.5)
 1080 FORMAT ('    Excess amps are >',F5.1,' sigma from mean')
 1160 FORMAT ('    Low correlations have cos (Delta phase) <',F6.2)
      END
