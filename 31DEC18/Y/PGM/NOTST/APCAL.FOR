LOCAL INCLUDE 'APCAL.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       Local include for APCAL
      INTEGER MXAVIF
      PARAMETER (MXAVIF = 2)
      HOLLERITH XNAMEI(3), XCLAIN(2), XINFIL(12), XOPCOD,
     *   XSOURC(4,30), XSTOK
      CHARACTER LNAME*12, LCLASS*6, LINFIL*48, LOPCOD*4, LSOURC(30)*16,
     *   LSTOK*4
      LOGICAL   WFQALL, ANTNEG
      DOUBLE PRECISION DTIM1, DTIM2, FREQS(MAXIF)
      REAL      XINSEQ, XINDIS, XANT(50), XSUBA, XBIF, XEIF, XFRQID,
     *   XTIME(8), XTYVER, XGCVER, XSNVER, XAPARM(10), XSOLIN,
     *   XTREC(60), XTAU0(30), XDOFIT(30), XPRTLV, XDOTV, XLTYPE,
     *   TREC(60), TAU0(30), BDEF, ZALIM, SOLINT, XINVER, XGRCH,
     *   NTAU0(60), TSMAX(60)
      INTEGER   INSEQ, INDISK, IANTWT(50), NANTWT, ITYVER, IGCVER,
     *   ISNVER, IFRQID, ISUBA, IDOFIT(30), ISPILL, NSOUWT, JBIF,
     *   JEIF, IPOL1, IPOL2, IPRTLV, JLTYPE, NPARM, INVERS, GRCHAN,
     *   ISUB1, ISUB2, WDOTV
C                                       Input parameters
      COMMON /INPARM/ XNAMEI, XCLAIN, XINSEQ, XINDIS, XANT, XSUBA,
     *   XSTOK, XBIF, XEIF, XFRQID, XSOURC, XTIME, XTYVER, XGCVER,
     *   XSNVER, XOPCOD, XAPARM, XSOLIN, XINVER, XINFIL, XTREC, XTAU0,
     *   XDOFIT, XPRTLV, XDOTV, XLTYPE, XGRCH
      COMMON /INVAL/ DTIM1, DTIM2, FREQS, BDEF, ZALIM, SOLINT, TREC,
     *   TAU0, IDOFIT, JBIF, JEIF, IPOL1, IPOL2, IANTWT, NANTWT, INSEQ,
     *   INDISK, ISUBA, IFRQID, ITYVER, IGCVER, ISNVER, ISPILL, NSOUWT,
     *   IPRTLV, JLTYPE, NPARM, WFQALL, WDOTV, INVERS, GRCHAN, NTAU0,
     *   TSMAX, ANTNEG, ISUB1, ISUB2
      COMMON /CHVAL/ LNAME, LCLASS, LINFIL, LOPCOD, LSOURC, LSTOK
C                                       Buffers
      INTEGER   NBUF1, NBUF2, NBUF3
      PARAMETER (NBUF1 = 512, NBUF2 = 512, NBUF3 = 512)
      INTEGER   BUFF1(NBUF1), BUFF2(NBUF2), BUFF3(NBUF3)
      COMMON /WRKBUF/ BUFF1, BUFF2, BUFF3
C                                       General global variables
      LOGICAL   WTANY
      INTEGER   ILUNF, IFINDF, ILUN1, ILUN2, ILUN3, ICNO, NPOLUV, NIFUV,
     *   IPOLUV, IPLROW
      COMMON /GLBLVR/ ILUNF, IFINDF, ILUN1, ILUN2, ILUN3, ICNO, NPOLUV,
     *   NIFUV, IPOLUV, IPLROW, WTANY
C
LOCAL END
LOCAL INCLUDE 'APCAL2.INC'
      INTEGER   MXBUFF, MXVAL
      PARAMETER (MXBUFF = 3000000, MXVAL = 32768)
      LOGICAL   WTATS
      REAL  TA(MXBUFF), TS(MXBUFF), GAIN(MXBUFF), SFLUX(MXBUFF),
     *   FOPAC(MXBUFF), TATIME(MXVAL), TATINT(MXVAL), TSELEV(MXVAL),
     *   TSPILL(MXVAL), TGAVG(MAXANT), TGPEAK(MAXANT), TSAVG(MXVAL,2),
     *   TSFIT(MXVAL,2), FAVG(MXVAL,2), TSECZ(MXVAL,2), TATM, TIMAVG,
     *   TSTIME(MXVAL,2), GAVG(MXVAL,2)
      INTEGER ITASOU(MXVAL), ITAFQD(MXVAL), ITSPTR(MXVAL),
     *   IPLWRK(MXVAL), NTSPTR, NTA, NPOL, NIF, ITSTRT, ITEND,
     *   IOPTYP, JFTPOL, NTEMP(MAXANT)
C
      COMMON /OPADAT/ TA, TS, GAIN, SFLUX, FOPAC, TATIME, TATINT,
     *   TSELEV, TSPILL, TGAVG, TGPEAK, TSAVG, TSFIT, FAVG, GAVG,
     *   TSECZ, TATM, TIMAVG, TSTIME, NTEMP,
     *   ITASOU, ITAFQD, ITSPTR, IPLWRK, NTSPTR, NTA, NPOL, NIF,
     *   ITSTRT, ITEND, IOPTYP, JFTPOL, WTATS
LOCAL END
LOCAL INCLUDE 'DGCV.INC'
C                                       Include for gain curve table
C                                       (used by GETGC and GCVAL)
C                                       Requires INCS:PUVD.INC and
C                                       INCS:PGCV.INC
      INTEGER MAXVAL
      PARAMETER (MAXVAL = 10)
      REAL XVALGC(2,MAXIF,MAXVAL), YVALGC(2,MAXIF,MXTBGC,MAXVAL),
     *   GAINGC(2,MAXIF,MXTBGC,MAXVAL), SENSGC(2,MAXIF,MAXVAL)
      INTEGER ITPGC(2,MAXIF,MAXVAL), NTGC(2,MAXIF,MAXVAL),
     *   IXTGC(2,MAXIF,MAXVAL), IYTGC(2,MAXIF,MAXVAL), IANTGC,
     *   ISUBGC, IFQDGC, NXVAL
      COMMON /GCCOM/ XVALGC, YVALGC, GAINGC, SENSGC,
     *   ITPGC, NTGC, IXTGC, IYTGC, IANTGC, ISUBGC, IFQDGC, NXVAL
C                                       End Include
LOCAL END
      PROGRAM APCAL
C-----------------------------------------------------------------------
C! Apply TY and GC tables to generate an amplitude calibration SN table
C# UV Calibration EXT-util VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2002-2017
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
C   Task APCAL applies a selected gain table (GC) and system temperature
C   table (TY) to produce a solution table (SN) containing amplitude
C   calibration information.
C   Inputs:
C      AIPS adverb       Local var.       Description
C      INNAME            LNAME            Input uv-file name.
C      INCLASS           LCLASS           Class of input uv-file
C      INSEQ             INSEQ            Seq. no. of input uv-file
C      INDISK            INDISK           Disk no. of input file
C      ANTENNAS          XANT             Antennas to calibrate
C      SUBARRAY          ISUBA            Subarray to calibrate
C      STOKES            LSTOK            Polarization to calibrate
C      BIF               JBIF             Start IF to calibrate
C      EIF               JEIF             End IF to calibrate
C      FREQID            IFRQID           Freq. ID to calibrate
C      SOURCES           XSOURC           Sources to calibrate
C      TIMERNG           XTIME            Time range to calibrate
C      TYVER             ITYVER           Input TY table version
C      GCVER             IGCVER           Input GC table version
C      OPCODE            LOPCOD           Opcode for calibration method
C      APARM             XAPARM           Input calibration parameters
C      SOLINT            SOLINT           Soln. interval for opacity
C      INFILE            LINFIL           File containing weather info.
C      INVERS            INVERS           WX table number with weather info.
C      TRECVR            TREC             Recvr. temp. for opacity soln.
C      TAU0              TAU0             Zenith opacity
C      DOFIT             IDOFIT           Opacity fit control parameter.
C      PRTLEV            IPRTLV           Print level reading WX file
C      DOTV              WDOTV            Use TV dev or PL file ?
C      LTYPE             JLTYPE           Plot labelling style
C-----------------------------------------------------------------------
      INCLUDE 'APCAL.INC'
      CHARACTER LPGM*6
      INTEGER   IRET
      INCLUDE 'INCS:DCAT.INC'
      DATA LPGM /'APCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and perform
C                                       general initialisation
      CALL APCIN (LPGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Perform the amplitude
C                                       calibration
      IF (ISUBA.LE.0) THEN
         CALL FNDEXT ('AN', CATBLK, ISUB2)
         ISUB1 = 1
      ELSE
         ISUB1 = ISUBA
         ISUB2 = ISUBA
         END IF
      DO 20 ISUBA = ISUB1,ISUB2
         CALL AMPCAL (IRET)
         IF (IRET.NE.0) GO TO 990
 20      CONTINUE
      IF (ISUB1.EQ.ISUB2) THEN
         ISUBA = ISUB1
      ELSE
         ISUBA = 0
         END IF
C                                       Update history file
      CALL APCHI (IRET)
C                                       Close down files/exit
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE APCIN (LPGM, IRET)
C------------------------------------------------------------------------
C   Read input parameters for APCAL and perform general initialisation
C   Inputs:
C      LPGM    C*6      Task name
C   Outputs:
C      IRET    I        Return code (0 => ok)
C------------------------------------------------------------------------
      CHARACTER LPGM*6
      INTEGER IRET
C
      INCLUDE 'APCAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTRUE
      CHARACTER LSTAT*4, LTYPE*2, LSTR*16
      INTEGER IERR, I, NSTR, IROUND, J
      DATA WTRUE /.TRUE./
C------------------------------------------------------------------------
      IRET = 0
C                                       LUN for reading text file
      ILUNF = 10
C                                       General LUNs for table I/O
      ILUN1 = 27
      ILUN2 = 28
      ILUN3 = 29
C                                       Initialise AIPS from disk
      CALL ZDCHIN (WTRUE)
C                                       Compute catalog rec. pointers
      CALL VHDRIN
C                                       Initialise /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input adverbs
      NPARM = 342
      CALL GTPARM (LPGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = WTRUE
         IRET = 1
C                                       Check if initiator (AIPS)
C                                       not found
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IERR.NE.0) IRET = 1
C                                       Abort if error obtaining
C                                       input parameters
      IF (IRET.NE.0) GO TO 999
C                                       Convert input parameters
      CALL H2CHR (12, 1, XNAMEI, LNAME)
      CALL H2CHR (6, 1, XCLAIN, LCLASS)
      INSEQ = XINSEQ
      INDISK = XINDIS
      GRCHAN = XGRCH + 0.01
      GRCHAN = MAX (0, MIN (7, GRCHAN))
C                                       Antennas wanted
      NANTWT = 0
      ANTNEG = .FALSE.
      DO 150 I = 1, 50
         J = IROUND (XANT(I))
         IF (J.GT.0) THEN
            NANTWT = NANTWT + 1
            IANTWT(NANTWT) = J
         ELSE IF (J.LT.0) THEN
            NANTWT = NANTWT + 1
            IANTWT(NANTWT) = -J
            ANTNEG = .TRUE.
            END IF
 150     CONTINUE
C                                       Subarray
      ISUBA = XSUBA
      ISUBA = MAX (ISUBA, 0)
C                                       Freqid
      IFRQID = XFRQID
C                                       Sources selected
      NSOUWT = 0
      DO 190 I = 1, 30
         CALL H2CHR (16, 1, XSOURC(1,I), LSTR)
C                                       Blank entry ?
         CALL CHBLNK (16, 1, LSTR, NSTR)
         IF (NSTR.NE.0) THEN
            NSOUWT = NSOUWT + 1
            LSOURC(NSOUWT) = LSTR
            IF (LSOURC(NSOUWT)(1:1).EQ.'-') THEN
               LSOURC(1) = LSOURC(NSOUWT)
               NSOUWT = 1
               GO TO 200
               END IF
            END IF
 190     CONTINUE
C                                       Timerang
 200  DTIM1 = XTIME(1) + XTIME(2) / 24.0D0 + XTIME(3) / 1440.0D0 +
     *   XTIME(4) / 86400.0D0
      DTIM2 = XTIME(5) + XTIME(6) / 24.0D0 + XTIME(7) / 1440.0D0 +
     *   XTIME(8) / 86400.0D0
      IF (DTIM1.GE.DTIM2) THEN
         CALL RFILL (8, 0.0, XTIME)
         XTIME(1) = -10.0
         XTIME(5) = 999.0
         DTIM1 = -10.0
         DTIM2 = 999.
         END IF
C                                       Table version numbers
      ITYVER = XTYVER
      ITYVER = MAX (0, ITYVER)
      IGCVER = XGCVER
      IGCVER = MAX (0, IGCVER)
      ISNVER = XSNVER
C                                       Opcode
      CALL H2CHR (4, 1, XOPCOD, LOPCOD)
      IF (LOPCOD.EQ.' ') LOPCOD = 'CALI'
      IF ((LOPCOD.NE.'CALI') .AND. (LOPCOD.NE.'LESQ') .AND.
     *    (LOPCOD.NE.'GRID') .AND. (LOPCOD.NE.'OPAC') .AND.
     *    (LOPCOD.NE.'GRDR') .AND. (LOPCOD.NE.'OPCR')) THEN
         MSGTXT = 'OPCODE = ''' // LOPCOD // ''' NOT KNOWN' //
     *      ' SET TO CALI'
         CALL MSGWRT (6)
         LOPCOD = 'CALI'
         END IF
      CALL CHR2H (4, LOPCOD, 1, XOPCOD)
C                                       APARM parameters
C                                       Default b-factor
      IF (XAPARM(1).LE.0.0) XAPARM(1) = 1.0
      BDEF = XAPARM(1)
C                                       Opacity parameters follow:
C                                       Zenith angle cutoff
      IF (XAPARM(2).LE.0.0) XAPARM(2) = 75.0
      ZALIM = ABS (XAPARM(2))
C                                       Spillover correction
      ISPILL = XAPARM(3)
      IF (ISPILL.EQ.0) ISPILL = 1
      ISPILL = MAX (ISPILL, 0)
      ISPILL = MIN (ISPILL, 2)
      XAPARM(3) = ISPILL
C                                       Use all FQIDs ?
      WFQALL = (XAPARM(4).GT.0.0)
C                                       Rec. temp. / zenith opacity
      CALL RCOPY (60, XTREC, TREC)
      CALL RCOPY (30, XTAU0, TAU0)
C                                       Solution type
      DO 220 I = 1, 30
         IDOFIT(I) = XDOFIT(I)
         IDOFIT(I) = MAX (IDOFIT(I), -1)
         IDOFIT(I) = MIN (IDOFIT(I), 1)
 220     CONTINUE
C                                       Solution interval (in days)
      SOLINT = MAX (XSOLIN, 0.0) / 1440.0
C                                       WX table number
      INVERS = XINVER
      INVERS = MAX (0, INVERS)
C                                       End of opacity parameters
C                                       Infile
      CALL H2CHR (48, 1, XINFIL, LINFIL)
C                                       Print level
      IPRTLV = XPRTLV
      IPRTLV = MAX (IPRTLV, 0)
      IPRTLV = MIN (IPRTLV, 1)
C                                       TV parameters
      IF (XDOTV.GT.0.0) THEN
         WDOTV = 1
      ELSE
         WDOTV = -1
         END IF
      JLTYPE = IROUND (XLTYPE)
C                                       Find uv-file in catalog directory
      LSTAT = 'SRCH'
      LTYPE = 'UV'
      ICNO = 1
      CALL CATDIR ('SRCH', INDISK, ICNO, LNAME, LCLASS, INSEQ, LTYPE,
     *   NLUSER, LSTAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 2
         GO TO 990
         END IF
C                                       Read catalog header; mark file
C                                       status as 'WRITE'
      CALL CATIO ('READ', INDISK, ICNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR, LNAME, LCLASS, INSEQ, INDISK
         IRET = 3
         GO TO 990
         END IF
C                                       Add to /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = INDISK
      FCNO(NCFILE) = ICNO
      FRW(NCFILE) = 1
C                                       SNver for multiple subarrays
      IF (ISNVER.LE.0) THEN
         CALL FNDEXT ('SN', CATBLK, ISNVER)
         ISNVER = ISNVER + 1
         END IF
C                                       Convert input parameters
      CALL CHR2H (12, LNAME, 1, XNAMEI)
      CALL CHR2H (6, LCLASS, 1, XCLAIN)
      XINSEQ = INSEQ
      XINDIS = INDISK
C                                       Get uv-header information
      CALL UVPGET (IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
C                                       No. of IFs/polzns. in header
      IF (JLOCIF.GT.0) THEN
         NIFUV = CATBLK(KINAX+JLOCIF)
      ELSE
         NIFUV = 1
         END IF
      IF (JLOCS.GT.0) THEN
         NPOLUV = MIN (2, CATBLK(KINAX+JLOCS))
      ELSE
         NPOLUV = 1
         END IF
      IPOLUV = 1
C                                       First polzn (RR,XX) = 1
      IF ((ICOR0.EQ.-1).OR.(ICOR0.EQ.-5)) IPOLUV = 1
C                                       (LL,YY) = 2
      IF ((ICOR0.EQ.-2).OR.(ICOR0.EQ.-6)) IPOLUV = 2
C                                       For all multi-Stokes datasets
C                                       expect both R and L in the
C                                       tables
      IF (NPOLUV.GT.1) IPOLUV = 1
C                                       Stokes type/ IF range
      CALL H2CHR (4, 1, XSTOK, LSTOK)
      IPOL1 = 1
      IPOL2 = NPOLUV
      IF ((LSTOK.EQ.'L').AND.(NPOLUV.EQ.2)) THEN
         IPOL1 = 2
         IPOL2 = 2
         END IF
      IF ((LSTOK.EQ.'R').AND.(NPOLUV.EQ.2)) THEN
         IPOL1 = 1
         IPOL2 = 1
         END IF
C                                       IF range
      JBIF = XBIF
      JEIF = XEIF
      IF (JEIF.LE.0) JEIF = NIFUV
      JBIF = MAX (1, JBIF)
      JBIF = MIN (NIFUV, JBIF)
      JEIF = MAX (1, JEIF)
      JEIF = MIN (NIFUV, JEIF)
      GO TO 999
C
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
1000  FORMAT ('APCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
1020  FORMAT ('APCIN: ERR',I3,' FINDING ',A12,'.',A6,'.',I4,'.',I3)
1040  FORMAT ('APCIN: ERR',I3,' READING HEADER ',A12,'.',A6,'.',I4,'.',
     *   I3)
1060  FORMAT ('APCIN: ERROR',I3,' DECODING HEADER')
      END
      SUBROUTINE AMPCAL (IRET)
C----------------------------------------------------------------------
C   Write an SN table from information in the TY and GC tables
C   Outputs:
C      IRET    I   Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PTYTAB.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'DGCV.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER BNDCOD(MAXIF)*8
      LOGICAL   WTANT(2,MAXIF), WAPPL, WTSYS, WSELEC, WMATCH, WSTART,
     *   WFLUX, WTABLE, WEXIST, WFITS, WFONE, NOTREC, NOTAU0, NODOFT
      DOUBLE PRECISION DTIME, DSTIME
      REAL FKEY(2,2), GMMOD, RANOD(25), RDCNOD(25), TIMETY, TINTTY,
     *   TSYST(2,MAXIF), TANT(2,MAXIF), TAVAL, TSVAL, HA, EL, AZ,
     *   RDEC, SNMBD(2), SNREAL(2,MAXIF), SNIMAG(2,MAXIF), SNDISP(2),
     *   SNDEL(2,MAXIF), SNRAT(2,MAXIF), SNWGT(2,MAXIF), SNIFR,
     *   TAV, AMPSN, ELDEG, HADEG, TBWFQ(MAXIF), CHBWFQ(MAXIF),
     *   SNDDIS(2)
      INTEGER ISNREF(2,MAXIF), LSNREF, SIDFQ(MAXIF)
      INTEGER KEY(2,2), IERR, ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC),
     *   NPOLTY, NIFTY, NANTSN, NPOLSN, NIFSN, NUMNOD, ISNRNO, IER,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), JREC, ITYSOU, ITYANT,
     *   ITYSUB, ITYFQD, ICRANT, ICRFQD, ICRSUB, I, JPOL, JIF, NROW,
     *   JSTART, K, ISNNOD, NDIM, MPOL, MIF, KEYSUB(2,2)
      INTEGER JTRIM, IANT2, II, NSN
      DATA KEYSUB /4*1/
      DATA SNMBD, SNDISP, SNDDIS /6*0.0/
C----------------------------------------------------------------------
      WFONE = .FALSE.
      NOTREC = .TRUE.
      NOTAU0 = .TRUE.
      NODOFT = .TRUE.
      NSN = 0
      IRET = 0
      LSNREF = -1
C                                       Get antenna table information
      CALL GETANT (INDISK, ICNO, ISUBA, CATBLK, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Strip null characters from
C                                       antenna names returned by
C                                       GETANT and check if DOFIT,
C                                       TREC and TAU0 are set
      DO 10 I = 1, NSTNS
         K = JTRIM (STNNAM(I))
         IF (IDOFIT(I).EQ.1) NODOFT = .FALSE.
         IF (TAU0(I).NE.0) NOTAU0 = .FALSE.
 10      CONTINUE
      DO 15 I = 1, NSTNS*2
         IF (TREC(I).NE.0) NOTREC = .FALSE.
 15      CONTINUE
C                                       get frequency
      CALL GETFQ (IFRQID, INDISK, ICNO, CATBLK, ILUN3, FREQS, TBWFQ,
     *   CHBWFQ, SIDFQ, BNDCOD, IRET)
C                                       Read weather information
C                                       and write some warnings and
C                                       set initial TAU0 and TRECVR
C                                       if DOFIT = 1
      IF (LOPCOD.NE.'CALI') THEN
         CALL TXTWX (IERR)
         IF ((IERR.NE.0).AND.(IERR.NE.1)) THEN
            IRET = 2
            GO TO 995
            END IF
         IF (NODOFT) THEN
            WRITE (MSGTXT, 1400)
            CALL MSGWRT (8)
            END IF
         IF (NOTAU0 .AND. (LOPCOD .NE. 'LESQ')) THEN
            CALL DFTAU(IERR)
            END IF
         CALL DFTRC(NOTREC,IERR)
         END IF
C                                       Sort TY table into (Ant, Suba,
C                                       Fqid, Time) order.
      KEY(1,1) = TYIANT
      FKEY(1,1) = 1000.0
      KEY(2,1) = TYISUB
      FKEY(2,1) = 1.0
      KEY(1,2) = TYIFQI
      FKEY(1,2) = 100.0
      KEY(2,2) = TYRTIM
      FKEY(2,2) = 1.0
      CALL TABSRT (INDISK, ICNO, 'TY', ITYVER, ITYVER, KEY, KEYSUB,
     *   FKEY, BUFF2, CATBLK, IERR)
      IF (IERR.NE.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Open input TY table
      CALL TYINI ('READ', BUFF1, INDISK, ICNO, ITYVER, CATBLK, ILUN1,
     *   ITYRNO, TYKOLS, TYNUMV, NPOLTY, NIFTY, IERR)
      IF (IERR.NE.0) THEN
         IRET = 4
         WRITE (MSGTXT,1050) IERR
         GO TO 990
         END IF
C                                       Delete SN table if it already exists
      IF ((ISNVER.GT.0) .AND. (ISUBA.EQ.ISUB1)) THEN
         CALL ISTAB ('SN', INDISK, ICNO, ISNVER, ILUN2, BUFF2, WTABLE,
     *      WEXIST, WFITS, IERR)
         IF (WTABLE.AND.WEXIST) THEN
            CALL RMEXT (INDISK, ICNO, 'SN', ISNVER, CATBLK, BUFF2,IERR)
            IF (IERR.NE.0) THEN
               IRET = 5
               WRITE (MSGTXT,1060)
               GO TO 990
               END IF
            END IF
         END IF
C                                       Open output SN table
      NANTSN = NSTNS
      NPOLSN = NPOLUV
      NIFSN = NIFUV
      NUMNOD = 0
      CALL RFILL (25, 0.0, RANOD)
      CALL RFILL (25, 0.0, RDCNOD)
      GMMOD = 1.0
      WAPPL = .FALSE.
      CALL SNINI ('WRIT', BUFF2, INDISK, ICNO, ISNVER, CATBLK, ILUN2,
     *   ISNRNO, SNKOLS, SNNUMV, NANTSN, NPOLSN, NIFSN, NUMNOD, GMMOD,
     *   RANOD, RDCNOD, WAPPL, IERR)
      IF (IERR.GT.0) THEN
         IRET = 6
         WRITE (MSGTXT,1070) IERR
         GO TO 990
      ELSE
         WRITE (MSGTXT,1080) ISNVER, ISUBA
         CALL MSGWRT (3)
         END IF
C                                       Always rewrite the SN table
      IF (ISUBA.EQ.ISUB1) ISNRNO = 1
C
      NROW = BUFF1(5)
      JREC = 1
C                                       Reset IDSOUR in DSOU.INC
      IDSOUR = 0
C                                       Reset IANTGC, ISUBGC and IFQDGC
C                                       in DGCV.INC
      IANTGC = 0
      ISUBGC = 0
      IFQDGC = 0
C                                       Reset plot row number (used
C                                       if opacity solution determined)
      IPLROW = 1
C                                       While (NOT EOF (TY_table)) do:
 80   IF (JREC.GT.NROW) GO TO 650
C                                       Initialisation for start of
C                                       each scan.
         WSTART = .TRUE.
         WMATCH = .TRUE.
         DO 90 I = 1, MAXIF
            WTANT(1,I) = .FALSE.
            WTANT(2,I) = .FALSE.
 90         CONTINUE
         WTANY = .FALSE.
         NTSPTR = 1
         ITSPTR(1) = 1
         NTA = 0
C                                       Scan loop:
C                                       While (NOT EOF (TY_table) and
C                                       (within current antenna)) do:
 100     IF ((JREC.GT.NROW).OR.(.NOT.WMATCH)) GO TO 400
C                                       Read TY record
         ITYRNO = JREC
         CALL TABTY ('READ', BUFF1, ITYRNO, TYKOLS, TYNUMV, NPOLTY,
     *      NIFTY, TIMETY, TINTTY, ITYSOU, ITYANT, ITYSUB, ITYFQD,
     *      TSYST, TANT, IERR)
         IF (IERR.LT.0) GO TO 350
         IF (IERR.NE.0) THEN
            IRET = 7
            WRITE (MSGTXT,1090) IERR
            GO TO 990
            END IF
C                                       Check if record is selected
C                                       Antenna:
         IF (NANTWT.GT.0) THEN
            DO 150 I = 1,NANTWT
               IF (IANTWT(I).EQ.ITYANT) THEN
                  IF (ANTNEG) GO TO 350
                  GO TO 155
                  END IF
 150           CONTINUE
            IF (.NOT.ANTNEG) GO TO 350
            END IF
C                                       Subarray:
  155    IF ((ITYSUB.GT.0) .AND. (ITYSUB.NE.ISUBA) .AND. (ISUBA.GT.0))
     *      GO TO 350
C                                       Freqid:
         IF ((IFRQID.GT.0) .AND. (ITYFQD.NE.IFRQID) .AND. (ITYFQD.GT.0))
     *      GO TO 350
C                                       Time range
         IF ((TIMETY.LT.DTIM1) .OR. (TIMETY.GT.DTIM2)) GO TO 350
C                                       Sources:
         IF (IDSOUR.NE.ITYSOU) THEN
            CALL GETSOU (ITYSOU, INDISK, ICNO, CATBLK, ILUN3, IERR)
            IF (IERR.NE.0) GO TO 350
C                                       the source listed at the TY
C                                       table absents at the SU table.
C                                       So read the next line of the
C                                       TY table
            END IF
C
         WSELEC = (NSOUWT.EQ.0)
         DO 200 I = 1, NSOUWT
            IF (LSOURC(I)(1:1).EQ.'-') THEN
               IF (LSOURC(I)(2:16).NE.SNAME(1:15)) WSELEC = .TRUE.
            ELSE
               IF (LSOURC(I).EQ.SNAME) WSELEC = .TRUE.
               END IF
 200        CONTINUE
         IF (.NOT.WSELEC) GO TO 350
C                                       Set current antenna information
         DTIME = TIMETY
         IF (WSTART) THEN
            ICRANT = ITYANT
            ICRSUB = ITYSUB
            ICRFQD = ITYFQD
            JSTART = JREC
            DSTIME = DTIME
            END IF
C                                       Process if first record or
C                                       still in current antenna.
         WMATCH = ((ICRANT.EQ.ITYANT).AND.(ICRSUB.EQ.ITYSUB))
         IF (WMATCH) THEN
C                                       No longer first record
            WSTART = .FALSE.
C                                       Accumulate data for this
C                                       antenna
            NTA = NTA + 1
            IF (NTA.GT.MXVAL) THEN
               IRET = 9
               WRITE (MSGTXT,1220)
               GO TO 990
               END IF
C
            ITASOU(NTA) = ITYSOU
            ITAFQD(NTA) = ITYFQD
            TATIME(NTA) = TIMETY
            TATINT(NTA) = TINTTY
C                                       force EL= 90 for orbiting
C                                       antenna
            IF (MNTYP(ITYANT).EQ.2) THEN
               EL = 90.0
C                                       Calculate source elevation
            ELSE
               CALL SOUELV (ITYANT, DTIME, HA, EL, AZ)
               END IF
            TSELEV(NTA) = EL / DG2RAD
C
            NPOL = IPOL2 - IPOL1 + 1
            NIF = JEIF - JBIF + 1
C
            DO 300 JPOL = IPOL1, IPOL2
               DO 250 JIF = JBIF, JEIF
                  K = (NTA - 1) * NPOL * NIF + (JIF - JBIF) * NPOL +
     *               (JPOL-IPOL1+1)
                  IF (K.GT.MXBUFF) THEN
                     IRET = 10
                     WRITE (MSGTXT,1240)
                     GO TO 990
                     END IF
C                                       Blank if source is below
C                                       horizon
                  IF (TSELEV(NTA).LE.0.0) THEN
                     TS(K) = FBLANK
                     TA(K) = FBLANK
                     SFLUX(K) = FBLANK
                     GAIN(K) = FBLANK
                     GO TO 250
                     END IF
C
                  TA(K) = TANT(JPOL,JIF)
                  IF ((TA(K).NE.FBLANK) .AND.
     *               (ABS(TA(K)-999.0).LE.0.01)) TA(K) = FBLANK
                  WTANT(JPOL,JIF) = WTANT(JPOL,JIF).OR.
     *               ((TA(K).NE.FBLANK).AND.(TA(K).GT.0))
                  WTANY = (WTANY.OR.WTANT(JPOL,JIF))
                  TS(K) = TSYST(JPOL,JIF)
                  IF ((TS(K).NE.FBLANK) .AND.
     *               (ABS(TS(K)-999.0).LE.0.01)) TS(K) = FBLANK
C                                       Calculate gain curve value

                  RDEC = DECAPP / DG2RAD
                  ELDEG = EL / DG2RAD
                  HADEG = HA / DG2RAD
                  CALL GCVAL (BUFF3, INDISK, ICNO, IGCVER, CATBLK,
     *               ILUN3, ITYANT, ITYSUB, ITYFQD, JPOL, JIF,
     *               HADEG, ELDEG, RDEC, GAIN(K), IERR)
C                                       End of solution interval ?
                  IF ((((DTIME-DSTIME).GT.SOLINT).AND.(SOLINT.GT.0.0))
     *               .OR.((ITYFQD.NE.ICRFQD).AND.(.NOT.WFQALL))) THEN
                     NTSPTR = NTSPTR + 1
                     ITSPTR(NTSPTR) = NTA - 1
                     DSTIME = DTIME
                     END IF
C                                       Tabulate flux density
                  IF (FLUX(1,JIF).EQ.FBLANK) FLUX(1,JIF) = 0.0
                  IF (FLUX(4,JIF).EQ.FBLANK) FLUX(4,JIF) = 0.0
                  WFLUX = ((FLUX(1,JIF).NE.0.0) .OR.
     *               (FLUX(4,JIF).NE.0.0))
                  SFLUX(K) = FBLANK
C                                       RCP
                  IF ((IPOLUV+JPOL-1).EQ.1) SFLUX(K) = FLUX(1,JIF) -
     *               FLUX(4,JIF)
C                                       LCP
                  IF ((IPOLUV+JPOL-1).EQ.2) SFLUX(K) = FLUX(1,JIF) +
     *               FLUX(4,JIF)
                  IF (.NOT.WFLUX) WFONE = .TRUE.
 250              CONTINUE
 300           CONTINUE
            ICRFQD = ITYFQD
            END IF
C                                       Loop back for next record
         IF (.NOT.WMATCH) GO TO 100
 350     JREC = JREC + 1
         GO TO 100
C                                       Endwhile
 400  CONTINUE
C
      NTSPTR = NTSPTR + 1
      ITSPTR(NTSPTR) = NTA
C                                       Solve for opacity if required
      IANT2 = ICRANT
      IF ((NANTWT.GT.0) .AND. (.NOT.ANTNEG)) THEN
         DO 410 II = 1,NANTWT
            IF (IANTWT(II).EQ.ICRANT) IANT2 = II
 410        CONTINUE
         ENDIF
      CALL RFILL (MXBUFF, 1.0, FOPAC)
      IF (XAPARM(6).GT.0.0) CALL BUGRTS (ICRANT)
      IF ((LOPCOD.NE.'CALI') .AND. (.NOT.WTANY) .AND.
     *   (IDOFIT(IANT2).GE.0)) THEN
         CALL OPACOR (ICRANT, IERR)
         IF (IERR.NE.0) THEN
            IRET = 11
            GO TO 995
            END IF
         END IF
C                                       Write SN records
      DO 500 I = 1,NTA
C                                       Initialise SN record
         WTSYS = .FALSE.
         NDIM = 2 * MAXIF
         CALL RFILL (NDIM, FBLANK, SNREAL)
         CALL RFILL (NDIM, FBLANK, SNIMAG)
         CALL RFILL (NDIM, FBLANK, SNDEL)
         CALL RFILL (NDIM, FBLANK, SNRAT)
         CALL RFILL (NDIM, FBLANK, SNWGT)
         CALL FILL (NDIM, 0, ISNREF)
C                                       Fill SN record
         DO 460 JPOL = IPOL1, IPOL2
            DO 450 JIF = JBIF, JEIF
C
               K = (I - 1) * NPOL * NIF + (JIF - JBIF) * NPOL +
     *            (JPOL - IPOL1+1)
               TSVAL = TS(K)
               TAVAL = TA(K)
               AMPSN = FBLANK
               IF ((TSVAL.NE.FBLANK).AND.(TSVAL.GT.0.0)) THEN
                  IF (TAVAL.LT.0.0) THEN
C                                       Ta/Tsys
                     IF (SFLUX(K).EQ.FBLANK) THEN
                        IRET = 12
                        WRITE (MSGTXT,1350) ITASOU(I)
                        GO TO 990
                        END IF
C                                       Amplitude cal. factor
                     AMPSN = SQRT (BDEF * SFLUX(K) * FOPAC(I) / TSVAL)
                  ELSE
C                                       Standard Tsys
C                                       Ta or gain curve ?
                     IF (WTANT(JPOL,JIF)) THEN
                        MPOL = JPOL - IPOL1 + 1
                        MIF = JIF - JBIF + 1
                        CALL TAINT (ITASOU(I), ITAFQD(I), TATIME(I),
     *                     MPOL, MIF, TAV)
                        IF ((TAV.EQ.FBLANK).OR.(SFLUX(K).EQ.FBLANK))
     *                     THEN
                           IRET = 13
                           WRITE (MSGTXT,1350) ITASOU(I)
                           GO TO 990
                           END IF
                        IF (FOPAC(K).GT.0) AMPSN = SQRT (BDEF * TSVAL
     *                     * SFLUX(K) * FOPAC(K) / TAV)
                     ELSE
                        IF (GAIN(K).EQ.FBLANK) THEN
                           IRET = 14
                           WRITE (MSGTXT,1355) ICRANT, JPOL, JIF
                           GO TO 990
                           END IF
                        IF ((GAIN(K).GT.0.0).AND.(FOPAC(K).GT.0))
     *                     AMPSN = SQRT (BDEF * TSVAL * FOPAC(K) /
     *                     GAIN(K))
                        END IF
                     END IF
                  END IF
C                                       Fill SN record
               IF ((AMPSN.NE.FBLANK).AND.(AMPSN.GT.0.0)) THEN
                  SNREAL(JPOL,JIF) = AMPSN
                  SNIMAG(JPOL,JIF) = 0.0
                  SNDEL(JPOL,JIF) = 0.0
                  SNRAT(JPOL,JIF) = 0.0
                  SNWGT(JPOL,JIF) = 1.0
                  IF (LSNREF.LE.0) LSNREF = ICRANT
                  ISNREF(JPOL,JIF) = LSNREF
                  WTSYS = .TRUE.
                  NSN = NSN + 1
                  END IF
 450           CONTINUE
 460        CONTINUE
C                                       Write SN record
         IF (WTSYS) THEN
            DTIME = TATIME(I)
            TINTTY = TATINT(I)
            ITYSOU = ITASOU(I)
            ITYANT = ICRANT
            ITYSUB = ICRSUB
            ITYFQD = ITAFQD(I)
            CALL TABSN ('WRIT', BUFF2, ISNRNO, SNKOLS, SNNUMV, NPOLSN,
     *         DTIME, TINTTY, ITYSOU, ITYANT, ITYSUB, ITYFQD, SNIFR,
     *         ISNNOD, SNMBD, SNDISP, SNDDIS, SNREAL, SNIMAG, SNDEL,
     *         SNRAT, SNWGT, ISNREF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 15
               WRITE (MSGTXT,1360) IERR
               GO TO 990
               END IF
            END IF
C
 500     CONTINUE
C
         GO TO 80
C                                       Endwhile
 650  CONTINUE
      IF (WFONE .AND. (LOPCOD.NE.'CALI')) THEN
         MSGTXT = '*** At least one source had identically zero flux'
         CALL MSGWRT (8)
         MSGTXT = '***  Usually this means you forgot to run SETJY.'
         CALL MSGWRT (8)
         MSGTXT = '***  This may be important if you have a very'
         CALL MSGWRT (8)
         MSGTXT = '***  bright source (masers?).'
         CALL MSGWRT (8)
         END IF
C                                       Are there any SN entries?
      IF(NSN.EQ.0) THEN
         WRITE (MSGTXT,1410)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1420)
         GO TO 990
         END IF

C                                       Close tables
      CALL TABIO ('CLOS', 0, ITYRNO, BUFF1, BUFF1, IER)
      CALL TABIO ('CLOS', 0, ISNRNO, BUFF2, BUFF2, IER)
      GO TO 995
C                                       Error
  990 CALL MSGWRT (8)
C                                       Exit
C                                       Close any plot files or dev.
 995  CALL PLCLOS (INDISK, ICNO, CATBLK, BUFF3, I)
      IF (IRET.LE.0) THEN
         CALL CATIO ('UPDT', INDISK, ICNO, CATBLK, 'REST', BUFF1, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1650) IER
            CALL MSGWRT (7)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('AMPCAL: ERROR',I3,' READING ANTENNA TABLE')
1020  FORMAT ('AMPCAL: ERROR',I3,' SORTING TY TABLE')
1050  FORMAT ('AMPCAL: ERROR',I3,' OPENING TY TABLE')
1060  FORMAT ('AMPCAL: ERROR DELETING OLD SN TABLE')
1070  FORMAT ('AMPCAL: ERROR',I3,' OPENING SN TABLE')
1080  FORMAT ('Writing SN table',I4,' for subarray',I3)
1090  FORMAT ('AMPCAL: ERROR',I3,' READING TY TABLE')
1220  FORMAT ('AMPCAL: PARAMETER MXVAL TOO SMALL: CONTACT AIPS ADMIN')
1240  FORMAT ('AMPCAL: PARAMETER MXBUFF TOO SMALL: CONTACT AIPS ADMIN')
1350  FORMAT ('AMPCAL: NEED FLUX DENSITY IN SU TABLE FOR SOURCE NO: ',
     *   I4)
1355  FORMAT ('AMPCAL: NO GAIN FOR ANT:',I4,' POL:',I2,' IF:',I5)
1360  FORMAT ('AMPCAL: ERROR',I3,' WRITING SN RECORD')
1400  FORMAT ('WARNING: DOFIT = 0, OPACITY FIT NOT DONE')
1410  FORMAT ('AMPCAL: ERROR: NO ENTRIES TO PUT IN SN TABLE')
1420  FORMAT ('        Something wrong with your TY/GC table(s)?')
 1650 FORMAT ('AMPCAL: ERROR',I4,' UPDATING HEADER ON DISK')
      END
      SUBROUTINE TAINT (ISOU, IFQID, TIME, JPOL, JIF, TAV)
C-----------------------------------------------------------------------
C   Interpolate the measured antenna temp. values
C   Inputs:
C      ISOU    I    Source number
C      IFQID   I    Freq. id. number
C      TIME    R    Time value
C      JPOL    I    Polarization number
C      JIF     I    IF number
C   Outputs:
C      TAV     R    Interpolated Tant value
C-----------------------------------------------------------------------
      REAL TIME, TAV
      INTEGER ISOU, IFQID, JPOL, JIF
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INTEGER I, J, K, INDX, IND, IND1, IND2, N, IFIRST, ILAST
C-----------------------------------------------------------------------
      TAV = FBLANK
      IF (NTA.LE.0) GO TO 999
C
      I = 0
      J = 0
      IFIRST = 0
      ILAST = 0
      INDX = (JIF - 1) * NPOL + JPOL
      N = NPOL * NIF
C                                       Find points on either side
C                                       of the desired time
      DO 200 K = 1, NTA
         IND = (K - 1) * N + INDX
         IF ((ITASOU(K).NE.ISOU).OR.(ITAFQD(K).NE.IFQID).OR.
     *      (TA(IND).EQ.FBLANK)) GO TO 200
         IF (IFIRST.EQ.0) IFIRST = K
         IF (TIME.GT.TATIME(K)) I = K
         IF ((TIME.LT.TATIME(K)).AND.(J.EQ.0)) J = K
         ILAST = K
 200     CONTINUE
C
      IF ((I.EQ.0).AND.(J.EQ.0)) GO TO 999
C                                       Before first point
      IF (I.EQ.0) THEN
         IND = (IFIRST - 1) * N + INDX
         TAV = TA(IND)
         GO TO 999
         END IF
C                                       After last point
      IF (J.EQ.0) THEN
         IND = (ILAST - 1) * N + INDX
         TAV = TA(IND)
         GO TO 999
         END IF
C                                       Linear interpolation
      IND1 = (I - 1) * N + INDX
      IND2 = (J - 1) * N + INDX
      TAV = (TA(IND2) - TA(IND1)) / (TATIME(J) - TATIME(I)) *
     *   (TIME - TATIME(I)) + TA(IND1)
C                                       Exit
 999  RETURN
      END
      SUBROUTINE APCHI (IRET)
C-----------------------------------------------------------------------
C   Subroutine to update the history file
C   Outputs:
C      IRET    I      Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      LOGICAL WUPDAT
      CHARACTER LHIREC*72, LTIME*20
      INTEGER IERR, IDATE(3), ITIME(3), I, K, J, PTIME(8)
C-----------------------------------------------------------------------
      IRET = 0
C
      CALL HIINIT (3)
C                                       Open history table
      CALL HIOPEN (ILUN1, INDISK, ICNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
C                                       Task name and time
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, LTIME(13:20), LTIME(1:12))
      WRITE (LHIREC,1010) TSKNAM, RLSNAM, LTIME(1:12), LTIME(13:20)
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       UV-file name
      WRITE (LHIREC,1020) TSKNAM, LNAME, LCLASS, INDISK, INSEQ
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Antennas
      IF (ANTNEG) THEN
         LHIREC = TSKNAM // '/ antennas excluded from fit'
      ELSE
         LHIREC = TSKNAM // '/ antennas included in fit'
         END IF
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
      DO 50 I = 1,NANTWT,10
         WRITE (LHIREC,1040) TSKNAM, (IANTWT(I+K-1),
     *      K = 1, MIN (10, NANTWT-I+1))
         CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
 50      CONTINUE
C                                       Subarray, Stokes, IF range
      WRITE (LHIREC,1060) TSKNAM, ISUBA, LSTOK, JBIF, JEIF
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Freqid, time range
      IF((XTIME(1).EQ.-10.0).AND.(XTIME(5).EQ.999.0))THEN
         DO 60 I=1, 8
            PTIME(I)=0
 60         CONTINUE
      ELSE
         DO 70 I=1, 8
            PTIME(I)=INT(XTIME(I))
 70         CONTINUE
         ENDIF
      WRITE (LHIREC,1080) TSKNAM, IFRQID, PTIME
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Sources
      DO 100 I = 1, NSOUWT, 2
         WRITE (LHIREC,1100) TSKNAM, (LSOURC(I+K-1),
     *      K = 1, MIN (2, NSOUWT-I+1))
         CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
 100     CONTINUE
C                                       TYver, GCver, SNver
      WRITE (LHIREC,1120) TSKNAM, ITYVER, IGCVER, ISNVER
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Opcode, solint
      WRITE (LHIREC,1140) TSKNAM, LOPCOD, SOLINT
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       APARM
      WRITE (LHIREC,1160) TSKNAM, BDEF, ZALIM, ISPILL, WFQALL
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (XAPARM(6).GT.0.0) THEN
         IF ((I.GE.JBIF) .AND. (I.LE.JEIF)) THEN
            WRITE (LHIREC,1171) TSKNAM, I
         ELSE
            WRITE (LHIREC,1170) TSKNAM
            END IF
         CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       WX file name
      WRITE (LHIREC,1180) TSKNAM, LINFIL
      CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Trec, tau0, fit type
      DO 120 I = 1, NSTNS
         J = 2 * (I - 1) + 1
         WRITE (LHIREC,1200) TSKNAM, STNNAM(I), TREC(J), TREC(J+1),
     *      NTAU0(J), NTAU0(J+1), IDOFIT(I)
         CALL HIADD (ILUN1, LHIREC, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
 120     CONTINUE
C                                       Close HI file
      WUPDAT = .TRUE.
      CALL HICLOS (ILUN1, WUPDAT, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) IERR
C
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('ANTBHI: ERROR',I3,' OPENING HI TABLE')
 1010 FORMAT (A6,'RELEASE: ',A8,' START TIME: ',A12,2X,A8)
 1020 FORMAT (A6,'INNAME= ',A12,'.',A6,'.',I3,'.',I4)
 1040 FORMAT (A6,'ANTENNAS= ',10I3)
 1060 FORMAT (A6,'SUBA=',I3,' STOKES= ',A4,' IF=',I2,' - ',I2)
 1080 FORMAT (A6,'FQID=',I3,' TIMERNG= ',8I3)
 1100 FORMAT (A6,'SRC= ',2(A16,5X))
 1120 FORMAT (A6,'TYVER=',I4,' GCVER=',I4,' SNVER=',I4)
 1140 FORMAT (A6,'OPCODE= ',A4,' SOLINT=',F6.1)
 1160 FORMAT (A6,'BDEF= ',F6.3,' ZALIM=',F5.1,' SPILL=',I2,
     *   ' ALL FQ=',L4)
 1170 FORMAT (A6,'APARM(6) = 1   / Tsys adjusted to mean Tsys over IF')
 1171 FORMAT (A6,'APARM(6) = 1   / Tsys adjusted to Tsys of IF',I3)
 1180 FORMAT (A6,'WEATHER FILE= ',A48)
 1200 FORMAT (A6,A8,'TREC(R,L)=',2F7.2,' TAU0(R,L)=',2F6.3,
     *   ' DOFIT=',I1)
 1980 FORMAT ('ANTBHI: ERROR',I3,' PROCESSING HISTORY FILE')
      END
      SUBROUTINE GETGC (BUFFER, IDISK, ICNO, IVER, CATBLK, ILUN, IANT,
     *   ISUB, IFQID, IRET)
C-----------------------------------------------------------------------
C   Load all gain curve info. for a specific antenna, sub. and fqid
C   Inputs
C      IDISK   I   Disk volume of uv-file
C      ICNO    I   Catalog number of uv-file
C      CATBLK  I   Catalog header for uv-file
C      ILUN    I   LUN to use for table I/O
C      IANT    I   Antenna number
C      ISUB    I   Subarray
C      IFQID   I   Frequency ID
C   Input/output:
C      BUFFER  I(512)   I/O buffer
C      IVER    I        Version number of GC table
C   Output in common:
C      Data output in DGCV.INC
C   Output:
C      IRET    I        Return code (0 => ok)
C-----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, IVER, ILUN, IANT, ISUB, IFQID, IRET
      INTEGER   BUFFER(512), CATBLK(256)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'DGCV.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      INTEGER   IGCRNO, GCKOLS(MAXGCC), GCNUMV(MAXGCC), NPOLGC, NTABGC,
     *   NOBAND, I, NROW, IERR, N, J, K, L
      LOGICAL   SAME
C-----------------------------------------------------------------------
      IRET = 0
C                                       Open GC table
      CALL GCINI ('READ', BUFFER, IDISK, ICNO, IVER, CATBLK, ILUN,
     *   IGCRNO, GCKOLS, GCNUMV, NPOLGC, NOBAND, NTABGC, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C
      NXVAL = 1
      NROW = BUFFER(5)
C                                       While (NOT EOF(GC_table)) do
C                                          (read record)
      DO 300 I = 1, NROW
         IGCRNO = I
         CALL TABGC ('READ', BUFFER, IGCRNO, GCKOLS, GCNUMV, NPOLGC,
     *      NTABGC, IANTGC, ISUBGC, IFQDGC, ITPGC(1,1,NXVAL),
     *      NTGC(1,1,NXVAL), IXTGC(1,1,NXVAL), IYTGC(1,1,NXVAL),
     *      XVALGC(1,1,NXVAL), YVALGC(1,1,1,NXVAL),
     *      GAINGC(1,1,1,NXVAL), SENSGC(1,1,NXVAL), IERR)
         IF (IERR.GT.0) THEN
            IRET = 2
            WRITE (MSGTXT,1040) IERR
            GO TO 990
C                                       Matching ant, subarray, fqid ?
         ELSE IF (IERR.EQ.0) THEN
            IF ((IANT.EQ.IANTGC) .AND. ((ISUB.EQ.ISUBGC) .OR.
     *         (ISUBGC.LE.0)) .AND. ((IFQID.EQ.IFQDGC) .OR.
     *         (IFQDGC.LE.0))) THEN
C                                       is this a duplicate?
               IF (NXVAL.GT.1) THEN
                  SAME = .TRUE.
                  N = NXVAL - 1
                  DO 100 J = 1,NOBAND
                     DO 90 K = 1,2
                        IF (ITPGC(K,J,NXVAL).NE.ITPGC(K,J,N))
     *                     SAME = .FALSE.
                        IF (NTGC(K,J,NXVAL).NE.NTGC(K,J,N))
     *                     SAME = .FALSE.
                        IF (IXTGC(K,J,NXVAL).NE.IXTGC(K,J,N))
     *                     SAME = .FALSE.
                        IF (IYTGC(K,J,NXVAL).NE.IYTGC(K,J,N))
     *                     SAME = .FALSE.
                        IF (XVALGC(K,J,NXVAL).NE.XVALGC(K,J,N))
     *                     SAME = .FALSE.
                        IF (SENSGC(K,J,NXVAL).NE.SENSGC(K,J,N))
     *                     SAME = .FALSE.
                        DO 80 L = 1,NTGC(K,J,NXVAL)
                           IF (YVALGC(K,J,L,NXVAL).NE.YVALGC(K,J,L,N))
     *                        SAME = .FALSE.
                           IF (GAINGC(K,J,L,NXVAL).NE.GAINGC(K,J,L,N))
     *                        SAME = .FALSE.
  80                       CONTINUE
  90                    CONTINUE
  100                CONTINUE
               ELSE
                  SAME = .FALSE.
                  END IF
               IF (.NOT.SAME) THEN
                  NXVAL = NXVAL + 1
                  IF (NXVAL.GT.MAXVAL) THEN
                     IRET = 3
                     WRITE (MSGTXT,1060)
                     GO TO 990
                     END IF
                  END IF
               END IF
            END IF
 300     CONTINUE
C                                       Close GC table
      IANTGC = IANT
      ISUBGC = ISUB
      IFQDGC = IFQID
      NXVAL = NXVAL - 1
      CALL TABIO ('CLOS', 0, IGCRNO, BUFFER, BUFFER, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IF (IRET.NE.1) CALL TABIO ('CLOS', 0, IGCRNO, BUFFER, BUFFER,
     *   IERR)
C                                       Exit
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('GETGC: ERROR',I3,' OPENING GC TABLE')
 1040 FORMAT ('GETGC: ERROR',I3,' READING GC TABLE')
 1060 FORMAT ('GETGC: PARAMETER MAXVAL TOO SMALL: CONTACT AIPS ADMIN')
      END
      SUBROUTINE GCVAL (BUFFER, IDISK, ICNO, IVER, CATBLK, ILUN, IANT,
     *   ISUB, IFQID, JPOL, JIF, HA, ELEV, RDEC, GAIN, IRET)
C-----------------------------------------------------------------------
C   Calculate the gain of an antenna at a specified sky position.
C   Inputs:
C      IDISK   I    Disk volume of uv-data file
C      ICNO    I    Catalog number
C      CATBLK  I(*) Catalog header of u-v file
C      ILUN    I    LUN to use for table I/O
C      IANT    I    Antenna number
C      ISUB    I    Subarray number
C      IFQID   I    Freq. ID
C      JPOL    I    Polarization number in GC table
C      JIF     I    IF number in GC table
C      HA      R    Hour angle (degrees)
C      ELEV    R    Elevation (degrees)
C      RDEC    R    Declination of date (degrees)
C   Input/outputs:
C      IVER    I    GC table version number
C      BUFFER  I(*) Buffer for table I/O
C   Outputs:
C      GAIN    R    Gain value (in K/Jy)
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, IVER, ILUN, IANT, ISUB, IFQID, JPOL,
     *   JIF, IRET
      INTEGER   BUFFER(512), CATBLK(256)
      REAL HA, ELEV, RDEC, GAIN
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PGCV.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DGCV.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WPOL2D, WTYP1D, WTYP2D
      DOUBLE PRECISION DSUM, DEXPN, DXRAD, DYRAD, DCOSM, DSINM, DLGN,
     *   DGNRAO
      REAL GVAL(MXTBGC), YVAL(MXTBGC), XMIN, XDIFF, XORD, YORD, PVAL
      INTEGER NVALID, I, J, IERR, JX, L, M, IXTYPE
C----------------------------------------------------------------------
      IRET = 0
      GAIN = FBLANK
C                                       Does GETGC need to be called ?
      IF ((IANT.NE.IANTGC).OR.(ISUB.NE.ISUBGC).OR.(IFQID.NE.IFQDGC))
     *   THEN
         CALL GETGC (BUFFER, IDISK, ICNO, IVER, CATBLK, ILUN, IANT,
     *      ISUB, IFQID, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
         END IF
C                                       Check gain table consistency
      WPOL2D = .FALSE.
      WTYP1D = .FALSE.
      WTYP2D = .FALSE.
      NVALID = 0
      DO 50 I = 1, NXVAL
C                                       Skip null entries
         IF (ITPGC(JPOL,JIF,I).LE.0) GO TO 50
C
            NVALID = NVALID + 1
            J = I
C                                       2-d polynomial
            IF (ITPGC(JPOL,JIF,I).EQ.3) WPOL2D = .TRUE.
            IF (IXTGC(JPOL,JIF,I).GT.0) THEN
C                                       2-d gain curve
               IXTYPE = IXTGC(JPOL,JIF,I)
               WTYP2D = .TRUE.
            ELSE
C                                       1-d gain curve
               WTYP1D = .TRUE.
               END IF
 50      CONTINUE
C                                       Look for incompatible modes
      IF ((NVALID.LE.0) .OR. (WTYP2D.AND.WTYP1D) .OR.
     *   (WPOL2D.AND.WTYP1D) .OR. (WTYP1D.AND.(NVALID.GT.1)) .OR.
     *   (WPOL2D.AND.(NVALID.GT.1))) THEN
         GO TO 999
         END IF
C                                       Determine X-ordinate
      IF (IXTYPE.EQ.1) XORD = ELEV
      IF (IXTYPE.EQ.2) XORD = 90.0 - ELEV
      IF (IXTYPE.EQ.3) XORD = HA
      IF (IXTYPE.EQ.4) XORD = RDEC
      IF (IXTYPE.EQ.5) XORD = 90.0 - RDEC
C                                       Interpolate in X
      IF (WPOL2D.OR.WTYP1D) THEN
C                                       No interpolation necessary
         JX = J
C                                       Only allow IOPX=1 (nearest X-val)
      ELSE
         XMIN = 360.0
         JX = 0
         DO 100 I = 1, NXVAL
            IF (ITPGC(JPOL,JIF,I).GT.0) THEN
               XDIFF = ABS (XVALGC(JPOL,JIF,I) - XORD)
               IF (XDIFF.LT.XMIN) THEN
                  JX = I
                  XMIN = XDIFF
                  END IF
               END IF
 100        CONTINUE
         END IF
C                                       Interpolate in Y
C                                       Determine Y ordinate
      IF (IYTGC(JPOL,JIF,JX).EQ.1) YORD = ELEV
      IF (IYTGC(JPOL,JIF,JX).EQ.2) YORD = 90.0 - ELEV
      IF (IYTGC(JPOL,JIF,JX).EQ.3) YORD = HA
      IF (IYTGC(JPOL,JIF,JX).EQ.4) YORD = RDEC
      IF (IYTGC(JPOL,JIF,JX).EQ.5) YORD = 90.0 - RDEC
C                                       Copy polynomial coeff. or
C                                       tabulated gains to local array
      DO 150 I = 1,NTGC(JPOL,JIF,JX)
         GVAL(I) = GAINGC(JPOL,JIF,I,JX)
 150     CONTINUE
C
      PVAL = 1.0
C                                       Skip if no relative gain specified.
      IF (NTGC(JPOL,JIF,JX).LE.0) GO TO 300
C                                       Case (Gain_curve_type) of:
C                                       1: tabulated values
      IF (ITPGC(JPOL,JIF,JX).EQ.1) THEN
C                                       Copy Y-values to local array
         DO 175 I = 1, NTGC(JPOL,JIF,JX)
            YVAL(I) = YVALGC(JPOL,JIF,I,JX)
 175        CONTINUE
C                                       Interpolate
         CALL APCINT (YORD, YVAL, GVAL, NTGC(JPOL,JIF,JX), PVAL)
         END IF
C                                       2: 1-d polynomial
      IF (ITPGC(JPOL,JIF,JX).EQ.2) THEN
         DSUM = GAINGC(JPOL,JIF,1,JX)
         DO 200 I = 2, NTGC(JPOL,JIF,JX)
            DEXPN = YORD ** (I-1)
            DSUM = DSUM + GAINGC(JPOL,JIF,I,JX) * DEXPN
 200        CONTINUE
         PVAL = DSUM
         END IF
C                                       3: 2-d spherical harmonic
      IF (ITPGC(JPOL,JIF,JX).EQ.3) THEN
         DSUM = 0.0D0
         L = 0
         M = 0
         I = 1
         DXRAD = XORD * DG2RAD
         DYRAD = YORD * DG2RAD
C                                       While (I <= Nterms) do
 220     IF (I.LE.NTGC(JPOL,JIF,JX)) THEN
            DCOSM = COS (M * DYRAD)
            DLGN = DGNRAO (L, M, DXRAD)
            DSUM = DSUM + DLGN * DCOSM * GAINGC(JPOL,JIF,I,JX)
            IF (M.NE.0) THEN
               I = I + 1
               DSINM = SIN (M * DYRAD)
               DSUM = DSUM + DLGN * DSINM * GAINGC(JPOL,JIF,I,JX)
               END IF
            I = I + 1
            M = M + 1
            IF (M.GT.L) THEN
               M = 0
               L = L + 1
               END IF
            GO TO 220
            END IF
C                                       Endwhile
         PVAL = DSUM
         END IF
C                                       Endcase
C                                       Return relative gain
 300  IF (SENSGC(JPOL,JIF,JX).NE.FBLANK) THEN
         GAIN = SENSGC(JPOL,JIF,JX) * PVAL
      ELSE
         GAIN = FBLANK
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1020 FORMAT ('GCVAL: ERROR',I3,' LOADING GC TABLE')
      END
      SUBROUTINE APCINT (X, XVAL, YVAL, N, VALUE)
C-----------------------------------------------------------------------
C   One-dimensional interpolation routine
C   Inputs:
C      X       R    Abscissa value
C      XVAL    R(*) Tabulated X values
C      YVAL    R(*) Tabulated Y values
C      N       I    Dimension of XVAL and YVAL
C   Outputs:
C      VALUE   R    Interpolated value
C----------------------------------------------------------------------
      INTEGER N
      REAL X, XVAL(N), YVAL(N), VALUE
C
      REAL RTEMP, SLOPE
      INTEGER I, J
C----------------------------------------------------------------------
C                                       Sort array
      DO 100 I = 1, N-1
         DO 50 J = (I+1), N
            IF (XVAL(J).LT.XVAL(I)) THEN
               RTEMP = XVAL(J)
               XVAL(J) = XVAL(I)
               XVAL(I) = RTEMP
               RTEMP = YVAL(J)
               YVAL(J) = YVAL(I)
               YVAL(I) = RTEMP
               END IF
 50         CONTINUE
 100     CONTINUE
C                                       Special cases
      IF (X.LE.XVAL(1)) THEN
         VALUE = YVAL(1)
         GO TO 999
         END IF
C
      IF (X.GE.XVAL(N)) THEN
         VALUE = YVAL(N)
         GO TO 999
         END IF
C                                       Find values on either side of
C                                       the desired x-ordinate and
C                                       interpolate.
      VALUE = 0.0
      DO 200 I = 1, N-1
         IF ((XVAL(I).LE.X).AND.(XVAL(I+1).GE.X)) THEN
            SLOPE = (YVAL(I+1) - YVAL(I)) / (XVAL(I+1) - XVAL(I))
            VALUE = (X - XVAL(I)) * SLOPE + YVAL(I)
            END IF
 200     CONTINUE
C                                       Exit
 999  RETURN
      END
      FUNCTION DGNRAO (L, M, DTHETA)
C-----------------------------------------------------------------------
C   Compute the Legendre-type functions used in the GB 140 ft gain curve
C   Inputs:
C      L,M     I     Order of the Legendre type polynomial
C      DTHETA  D     Theta (in radians)
C   Output:
C      DGNRAO  D     Value of the associated Legendre polynomial with
C                    sin(theta)=sqrt(1-x**2) and the (-1)**m term
C                    removed.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DGNRAO, DTHETA
      INTEGER L, M
C
      DOUBLE PRECISION DP0, DP1, DSINT, DCOST, DFACT, DFM1, DSINTM,
     *   DGTMP
      INTEGER I, J
C-----------------------------------------------------------------------
      DGTMP = 0.0D0
C                                       Check validity of the input
C                                       parameters
      IF ((M.LT.0).OR.(L.LT.0).OR.(M.GT.L)) GO TO 990
C
      DCOST = COS (DTHETA)
      DSINT = SIN (DTHETA)
C                                       Compute P(m,m)
      DFACT = 1.0D0
      DO 20 J = 2, M
         DFACT = DFACT * (2*J-1)
 20      CONTINUE
      DFM1 = 1.0D0
C                                       Avoid raising zero to
C                                       a power directly
      IF (DSINT.EQ.0.0D0) THEN
         DP0 = 0.0D0
         IF (M.EQ.0.0D0) DP0 = 1.0D0
      ELSE
         DSINTM = DSINT ** M
         DP0 = DFM1 * DFACT * DSINTM
         END IF
C
      I = M + 2
      IF (M.EQ.L) THEN
         DGTMP = DP0
         GO TO 990
         END IF
C                                       Recurrence loop
      DP1 = DCOST * (2.0D0 * M + 1.0D0) * DP0
      IF (L.EQ.(M+1)) THEN
         DGTMP = DP1
         GO TO 990
         END IF
C                                       Compute P(m,l)
C                                       While (I <= L) do:
 50   IF (I.LE.L) THEN
         DGTMP = (DCOST * (2.0D0 * I - 1.0D0) * DP1 -
     *      (I + M - 1) * DP0) / DBLE (I - M)
         DP0 = DP1
         DP1 = DGTMP
         I=I+1
C                                       Endwhile
         GO TO 50
         END IF
C                                       Exit
 990  DGNRAO = DGTMP
C
 999  RETURN
      END
      SUBROUTINE BUGRTS (IANT)
C-----------------------------------------------------------------------
C   BUGRTS modifies the TS to produce more similar TS's
C   Inputs:
C      IANT   I   Antenna number
C-----------------------------------------------------------------------
      INTEGER   IANT
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INTEGER   JPOL, NASUM(MAXIF), NTSUM, I, JIF, K, J, IA
      REAL      ASUM(MAXIF), TSUM
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IA = XAPARM(7) + 0.01
C                                       outer loop over polarization
      DO 100 JPOL = 1,NPOL
         CALL RFILL (NIF, 0.0, ASUM)
         CALL FILL (NIF, 0, NASUM)
C                                       average factor
         DO 20 I = 1,NTA
C                                       average TS at time I
            TSUM = 0.0
            NTSUM = 0
            DO 10 JIF = 1,NIF
               K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JPOL
               IF ((TS(K).NE.FBLANK) .AND. (TS(K).GT.0.0)) THEN
                  TSUM = TSUM + TS(K)
                  NTSUM = NTSUM + 1
                  END IF
 10            CONTINUE
C                                       sum into factors
            IF (NTSUM.GT.0) THEN
               TSUM = TSUM / NTSUM
C                                       substitute 1 IF for average
               IF ((IA.GE.JBIF) .AND. (IA.LE.JEIF)) THEN
                  K = (I - 1) * NPOL * NIF + (IA - 1) * NPOL + JPOL
                  IF ((TS(K).NE.FBLANK) .AND. (TS(K).GT.0.0))
     *               TSUM = TS(K)
                  END IF
               DO 15 JIF = 1,NIF
                  K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JPOL
                  IF ((TS(K).NE.FBLANK) .AND. (TS(K).GT.0.0)) THEN
                     ASUM(JIF) = ASUM(JIF) + LOG10 (TS(K)/TSUM)
                     NASUM(JIF) = NASUM(JIF) + 1
                     END IF
 15               CONTINUE
               END IF
 20         CONTINUE
C                                       scale values
         DO 40 JIF = 1,NIF
            IF (NASUM(JIF).GT.0) THEN
               ASUM(JIF) = ASUM(JIF) / NASUM(JIF)
               ASUM(JIF) = 10.0 ** (-ASUM(JIF))
               DO 30 I = 1,NTA
                  K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JPOL
                  IF (TS(K).NE.FBLANK) TS(K) = TS(K) * ASUM(JIF)
 30               CONTINUE
               END IF
 40         CONTINUE
C                                       fess up
         WRITE (MSGTXT,1040) IANT, JPOL
         CALL MSGWRT (4)
         DO 50 JIF = 1,NIF,9
            J = MIN (JIF+8, NIF)
            WRITE (MSGTXT,1045) JIF, J, (ASUM(I), I = JIF,J)
            CALL MSGWRT (4)
 50         CONTINUE
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Tsys correction factors for antenna',I3,' polarization',
     *   I2)
 1045 FORMAT ('IFs',I3.2,'-',I2.2,1X,9(F6.3))
      END
      SUBROUTINE OPACOR (IANT, IRET)
C----------------------------------------------------------------------
C   Solve for atmospheric opacity
C   Inputs:
C      IANT    I   Antenna number
C   Outputs:
C      IRET    I   Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IANT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER LPOLZN(2)*3
      REAL      ZTREC(2), ZTAU0(2), TAVG, FAMOD, TSMOD
      INTEGER   I, II, K, JPOL, JIF, J, N(2), IERR, NCNT, IDAY, IHR,
     *   IMN, KPOL, IANT2, JJ, JANT
      DOUBLE PRECISION CFREQ
      DATA LPOLZN / 'RCP', 'LCP'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Compute spillover and check
C                                       that all source temp. values
C                                       are known
      WTATS = .FALSE.
      CALL RFILL (MXVAL, 0.0, TSPILL)
      I = (NIFUV + 1) / 2
      CFREQ = FREQS(I)/1.D9
C
      DO 100 I = 1, NTA
C                                       Compute spill-over
         IF (ISPILL.GT.0) CALL SPILL (ISPILL, TSELEV(I), CFREQ,
     *      TSPILL(I))
         DO 50 JPOL = 1, NPOL
            DO 40 JIF = 1, NIF
               K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JPOL
               IF (TS(K).EQ.FBLANK) GO TO 40
C                                       Check for Ta/Tsys
               IF (TA(K).LT.0.0) WTATS = .TRUE.
C                                       Can source temp. be calc. ?
               IF ((GAIN(K).EQ.FBLANK).OR.(SFLUX(K).EQ.FBLANK)) THEN
                  IRET = 1
                  WRITE (MSGTXT,1040) IANT, ITASOU(I)
                  GO TO 990
                  END IF
 40            CONTINUE
 50         CONTINUE
 100     CONTINUE
C                                       Compute atmospheric temp.
C                                       (VLBA Sci. Memo. 1)
      IF (TGPEAK(IANT).NE.FBLANK) THEN
         TATM = (TGPEAK(IANT) + 273.15) * 0.652 + 84.6
         IF (NTEMP(IANT).EQ.0.AND.INVERS.GT.0) THEN
            WRITE (MSGTXT,1000) IANT
            CALL MSGWRT (8)
            END IF
      ELSE
         TATM = 0.0
         END IF
C                                       Solve for each soln. interval
      DO 800 I = 1, (NTSPTR-1)
         ITSTRT = ITSPTR(I)
         ITEND = ITSPTR(I+1)
C                                       Decode start and end times
         TAVG = (TATIME(ITEND) + TATIME(ITSTRT)) / 2.0
         IDAY = TAVG
         IHR = (TAVG - IDAY) * 24.0
         IMN = (TAVG - IDAY - IHR / 24.0) * 60.0
C                                       Solve R, L separately
         DO 425 JPOL = 1, NPOL
C                                       Abs. polzn. value (1=R;2=L)
            KPOL = IPOLUV + (JPOL - 1) + (IPOL1 - 1)
C
            IANT2 = IANT
            IF ((NANTWT.GT.0) .AND. (.NOT.ANTNEG)) THEN
               DO 410 II = 1, NANTWT
                  IF (IANTWT(II).EQ.IANT) IANT2 = II
 410              CONTINUE
               END IF
            J = 2 * (IANT2 - 1) + KPOL
            JANT = 2 * (IANT - 1) + KPOL
            JJ = 2 * (IANT2 - 1) + JPOL
            ZTREC(JPOL) = TREC(J)
            ZTAU0(JPOL) = TAU0(IANT2)
            JFTPOL = JPOL
C                                       Case optype of:
C                                       'GRID': simple grid search
C
C                                        no robust fit:
            IF (LOPCOD.EQ.'GRID') THEN
               IF (IDOFIT(IANT2).EQ.1) THEN
                  CALL GRD2P (ZTREC(JPOL), ZTAU0(JPOL), .FALSE.)
                  WRITE (MSGTXT,1110) STNNAM(IANT), LPOLZN(KPOL),
     *               IDAY, IHR, IMN, ZTREC(JPOL), ZTAU0(JPOL)
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                        robust fit:
            IF (LOPCOD.EQ.'GRDR') THEN
               IF (IDOFIT(IANT2).EQ.1) THEN
                  CALL GRD2P (ZTREC(JPOL), ZTAU0(JPOL), .TRUE.)
                  WRITE (MSGTXT,1110) STNNAM(IANT), LPOLZN(KPOL),
     *               IDAY, IHR, IMN, ZTREC(JPOL), ZTAU0(JPOL)
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                       'OPAC': simplex method
C
C                                        no robust fit:
            IF (LOPCOD.EQ.'OPAC') THEN
               IF (IDOFIT(IANT2).EQ.1) THEN
                  CALL SIM2P (ZTREC(JPOL), ZTAU0(JPOL),.FALSE.)
                  WRITE (MSGTXT,1110) STNNAM(IANT), LPOLZN(KPOL),
     *               IDAY, IHR, IMN, ZTREC(JPOL), ZTAU0(JPOL)
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                        robust fit:
            IF (LOPCOD.EQ.'OPCR') THEN
               IF (IDOFIT(IANT2).EQ.1) THEN
                  CALL SIM2P (ZTREC(JPOL), ZTAU0(JPOL),.TRUE.)
                  WRITE (MSGTXT,1110) STNNAM(IANT), LPOLZN(KPOL),
     *               IDAY, IHR, IMN, ZTREC(JPOL), ZTAU0(JPOL)
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                       'LESQ': least squares method
            IF (LOPCOD.EQ.'LESQ' .OR. LOPCOD.EQ.'LSQR') THEN
               IF (IDOFIT(IANT2).EQ.1) THEN
                  CALL LESQR (ZTREC(JPOL), ZTAU0(JPOL))
                  WRITE (MSGTXT,1110) STNNAM(IANT), LPOLZN(KPOL),
     *               IDAY, IHR, IMN, ZTREC(JPOL), ZTAU0(JPOL)
                  CALL MSGWRT (4)
                  END IF
               END IF
C                                       Test fit TREC and TAU0 for
C                                       impossibilities:
C                                           0<Tau0<100
C                                           0<TREC<Tsys(max)
C                                       Set TREC and TAU0 to fit
C                                       values for history
            IF(ZTREC(JPOL) .LT. 0.0 .OR. ZTREC(JPOL)
     *        .GT. TSMAX(JANT)) THEN
               WRITE (MSGTXT,1120) STNNAM(IANT), LPOLZN(KPOL),
     *            ZTREC(JPOL)
               CALL MSGWRT (8)
               WRITE (MSGTXT, 1140)
               CALL MSGWRT (8)
               WRITE (MSGTXT, 1150)
               IRET = 3
               GO TO 990
               ENDIF
            IF(ZTAU0(JPOL) .LT. 0.0 .OR. ZTAU0(JPOL)
     *        .GT. 100.0) THEN
               WRITE (MSGTXT,1130) STNNAM(IANT), LPOLZN(KPOL),
     *            ZTAU0(JPOL)
               CALL MSGWRT (8)
               WRITE (MSGTXT, 1140)
               CALL MSGWRT (8)
               WRITE (MSGTXT, 1150)
               IRET = 3
               GO TO 990
               ENDIF
            TREC(J) = ZTREC(JPOL)
            NTAU0(J) = ZTAU0(JPOL)
 425        CONTINUE
C                                       Generate the solutions and
C                                       fill plot arrays
         N(1) = 1
         N(2) = 1
         TIMAVG = 0.0
         DO 480 II = ITSTRT, ITEND
C                                       Determine mid time of
C                                       solution interval
            TIMAVG = TIMAVG + TATIME(II)
C
            DO 460 JPOL = 1, NPOL
C                                       Initialise plot arrays
               J = N(JPOL)
               TSAVG(J,JPOL) = 0.0
               TSFIT(J,JPOL) = 0.0
               FAVG(J,JPOL) = 0.0
               GAVG(J,JPOL) = 0.0
               NCNT = 0
               DO 450 JIF = 1, NIF
                  K = (II - 1) * NPOL * NIF + (JIF - 1) * NPOL + JPOL
                  IF (TS(K).EQ.FBLANK) GO TO 450
C                                       Compute opacity correction
                  FOPAC(K) = FAMOD (ZTREC(JPOL), II, K)
C                                       Fill plot arrays
                  FOPAC(K) = MAX (0.0001, MIN (10000.0, FOPAC(K)))
                  TSAVG(J,JPOL) = TSAVG(J,JPOL) + TS(K)
                  TSFIT(J,JPOL) = TSFIT(J,JPOL) +
     *               TSMOD (ZTREC(JPOL), ZTAU0(JPOL), II, K)
                  FAVG(J,JPOL) = FAVG(J,JPOL) + FOPAC(K)
                  GAVG(J,JPOL) = GAVG(J,JPOL) + LOG(FOPAC(K))
                  NCNT = NCNT + 1
 450              CONTINUE
C                                       Average plot quantities
C                                       over IF
               IF (NCNT.GT.0) THEN
                  TSAVG(J,JPOL) = TSAVG(J,JPOL) / NCNT
                  TSFIT(J,JPOL) = TSFIT(J,JPOL) / NCNT
                  FAVG(J,JPOL) = FAVG(J,JPOL) / NCNT
C                                       Compute sec z.
                  TSECZ(J,JPOL) = 1.0 /
     *               SIN (TSELEV(II) * DG2RAD)
                  GAVG(J,JPOL) = GAVG(J,JPOL) / NCNT / TSECZ(J,JPOL)
                  TSTIME(J,JPOL) = TATIME(II)
                  N(JPOL) = N(JPOL) + 1
                  END IF
 460           CONTINUE
 480        CONTINUE
C                                       Plot
         TIMAVG = TIMAVG / (ITEND - ITSTRT + 1)
         N(1) = MAX (0, N(1) - 1)
         N(2) = MAX (0, N(2) - 1)
         IF (WDOTV.NE.0) THEN
            CALL PLOTOP (IANT, N, ZTREC, ZTAU0, IERR)
            IF (IERR.NE.0) THEN
               IRET = 2
               IF (IERR.LT.0) GO TO 999
               WRITE (MSGTXT,1480) IERR
               GO TO 990
               END IF
            END IF
C
 800     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('REAWX: *** NO TEMPS FOUND IN WX TABLE FOR ANT ',I3,
     *     ', LEFT AT 0 ***')
 1040 FORMAT ('OPACOR: MISSING GAIN OR FLUX DENSITY FOR ANT: ',I3,
     *   ' SRC:',I6)
 1110 FORMAT (A8,A4,I2,'/',I2,'h',I2,'m',' Trec (K):',F9.2,
     *   ' Zen. opac.:',F7.3)
 1120 FORMAT ('OPACOR: ERROR:',A3,A4,' Trec (K),',F7.2,
     *   ', unreasonable value.')
 1130 FORMAT ('OPACOR: ERROR:',A3,A4,' Tau0 (K),',F7.2,
     *   ', unreasonable value.')
 1140 FORMAT ('      --This is probably due to bad data in TY table,')
 1150 FORMAT ('        do some editing and try again.')
 1480 FORMAT ('OPACOR: ERROR',I3,' RETURNED BY PLOTOP')
      END
      SUBROUTINE GRD2P (PTREC, PTAU0, ROBUST)
C----------------------------------------------------------------------
C   Fit receiver temp. and zenith atmospheric opacity using grid search
C   Input/output:
C      PTREC   R   Receiver temperature (K)
C      PTAU0   R   Zenith atmospheric opacity (nepers)
C----------------------------------------------------------------------
      REAL PTREC, PTAU0
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER NITER
      PARAMETER (NITER = 100)
      DOUBLE PRECISION DP(2), DXINC1, DXINC2, DXM(2), DXMVAL, DXVAL
      DOUBLE PRECISION DF1
      INTEGER I, J, NROB
      LOGICAL ROBUST
C----------------------------------------------------------------------
C                                       Initialisation
      DXM(1) = 0.0D0
      DXM(2) = 0.0D0
      DXMVAL = 1.0D30
      NROB = 0
C                                       Search from zero to twice
C                                       the initial value.
      DXINC1 = (2.0 * PTREC) / (NITER - 1)
      DXINC2 = (2.0 * PTAU0) / (NITER - 1)
C                                       Simple 2-D grid search
      DO 200 I = 1, NITER
         DP(1) = (I - 1) * DXINC1
         DO 100 J = 1, NITER
            DP(2) = (J - 1) * DXINC2
            DXVAL = DF1 (DP, 2, NROB)
            IF (DXVAL.LT.DXMVAL) THEN
               DXM(1) = DP(1)
               DXM(2) = DP(2)
               DXMVAL = DXVAL
               END IF
100         CONTINUE
200      CONTINUE
C                                       Do robust soln if wanted
      IF (ROBUST) THEN
         DO 500 NROB = 1, 10
            DXINC1 = 2*DXM(1) / (NITER - 1)
            DXINC2 = 2*DXM(2) / (NITER - 1)
            DO 400 I = NITER/4, NITER*3/4
               DP(1) = (I -1) * DXINC1
               DO 300 J = NITER/4, NITER*3/4
                  DP(2) = (J - 1) * DXINC2
                  DXVAL = DF1 (DP, 2, NROB)
                  IF (DXVAL.LT.DXMVAL) THEN
                     DXM(1) = DP(1)
                     DXM(2) = DP(2)
                     DXMVAL = DXVAL
                     END IF
300               CONTINUE
400            CONTINUE
500         CONTINUE
         ENDIF
C                                       Return values
      PTREC = DXM(1)
      PTAU0 = DXM(2)
      RETURN
      END
      SUBROUTINE SIM2P (PTREC, PTAU0, ROBUST)
C-----------------------------------------------------------------------
C   Fit for Trec and zenith opacity using a simplex method
C   Input/output:
C      PTREC   R    Initial value for rec. temp. on input;
C                   fitted value on output.
C      PTAU0   R    Initial value for zenith opacity on input;
C                   fitted value on output.
C-----------------------------------------------------------------------
      REAL PTREC, PTAU0
C
      DOUBLE PRECISION DF1
      EXTERNAL DF1
      DOUBLE PRECISION DXC(2), DBUFF(24), DSCALE(2), DTOL
      INTEGER NITER, M, IERR, NROB
      LOGICAL ROBUST
C-----------------------------------------------------------------------
C                                       Set up call to SIMPLX
      DXC(1) = PTREC
      DXC(2) = PTAU0
      DSCALE(1) = ABS (PTREC) * 0.01
      DSCALE(2) = ABS (PTAU0) * 0.01
      DTOL = 1.0D-8
      NITER = 1000
      M = 2
      NROB = 0
      CALL SIMPLX (DF1, DXC, DSCALE, M, DTOL, NITER, DBUFF, NROB, IERR)
C                                       Do robust soln' if wanted
      IF (ROBUST) THEN
         DO 100 NROB = 1, 10
            CALL SIMPLX (DF1, DXC, DSCALE, M, DTOL, NITER,
     *         DBUFF, NROB, IERR)
100         CONTINUE
         ENDIF
C                                       Return fitted parameters
      PTREC = DXC(1)
      PTAU0 = DXC(2)
C                                       Exit
      RETURN
      END
      FUNCTION DF1 (DP, M, NROB)
C----------------------------------------------------------------------
C   Compute function to be minimised in opacity fit
C   Inputs:
C      DP       D(M)   Input parameters (1=Trec; 2= Zenoth opacity)
C      M        I      Dimension of DP
C----------------------------------------------------------------------
      DOUBLE PRECISION DF1
      INTEGER M, NROB
      DOUBLE PRECISION DP(M)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DOUBLE PRECISION DSUM, DVAL
      REAL PTREC, PTAU0
      REAL TSMOD, TSEXP, FF(10), CHI2, RMS, SUMWT
      INTEGER JIF, K, I, TSWT(MXBUFF)
      DATA FF /15.0, 8.0, 6.0, 4.5, 3.5, 3.0, 2.8, 2.6, 2.4, 3.0/
C----------------------------------------------------------------------
      PTREC = DP(1)
      PTAU0 = DP(2)
C                                            If robust fit, adjust wts
      IF (NROB .GT. 0) THEN
         CALL RFILL(MXBUFF, 1.0, TSWT)
         CHI2 = 0.0
         SUMWT = 0.0
         DO 20 I = ITSTRT, ITEND
            DO 10 JIF = 1, NIF
               K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JFTPOL
               IF (TS(K).EQ.FBLANK) GO TO 10
               IF ((90.0-TSELEV(I)).GT.ZALIM) GO TO 10
               TSEXP = TSMOD(PTREC, PTAU0, I, K)
               CHI2 = ((TS(K) - TSEXP)*(TS(K) - TSEXP))/TSEXP + CHI2
               SUMWT = SUMWT + TSWT(K)
 10            CONTINUE
 20         CONTINUE
         RMS = CHI2/SUMWT
         IF (ABS(TS(K)-TSEXP)/RMS .GT. FF(NROB))
     *      TSWT(K) = 0.0
         ENDIF
      DSUM = 0.0D0
      DO 200 I = ITSTRT, ITEND
         DO 100 JIF = 1, NIF
            K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JFTPOL
            IF (TS(K).EQ.FBLANK .OR. (TSWT(K).LT.0.01 .AND. NROB.GT.0))
     *         GO TO 100
C                                       Check zenith angle limit
            IF ((90.0-TSELEV(I)).GT.ZALIM) GO TO 100
            DVAL = TS(K) - TSMOD(PTREC, PTAU0, I, K)
C                                       Assume Cauchy err. distribution
            DSUM = DSUM + LOG (1.0 + 0.5 * DVAL * DVAL)
100      CONTINUE
200   CONTINUE
C                                       Exit
      DF1 = DSUM
      RETURN
      END
      FUNCTION TSMOD (PTREC, PTAU0, I, K)
C----------------------------------------------------------------------
C   Function to compute model Tsys or Ta/Tsys
C   Inputs:
C      PTREC    R    Receiver temperature (K)
C      PTAU0    R    Zenith atmospheric opacity (nepers)
C      I,K      I    Indices in system temp. arrays
C----------------------------------------------------------------------
      REAL TSMOD, PTREC, PTAU0
      INTEGER I, K
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      DOUBLE PRECISION DOPAC
      REAL TASOU
C----------------------------------------------------------------------
      TSMOD = FBLANK
      IF (TS(K).EQ.FBLANK) GO TO 999
C                                       Source temperature
      TASOU = SFLUX(K) * GAIN(K)
C                                       Opacity factor
      DOPAC = EXP (-PTAU0 / SIN (TSELEV(I) * DG2RAD))
C                                       Ta/Tsys
      IF (WTATS) THEN
         TSMOD = TASOU * DOPAC / (TATM + PTREC + TSPILL(I) +
     *      (TASOU - TATM) * DOPAC)
      ELSE
C                                       Standard Tsys
         TSMOD = TATM + PTREC + TSPILL(I) + (TASOU - TATM) * DOPAC
         END IF
C
999   RETURN
      END
      SUBROUTINE LESQR (PTREC, PTAU0)
C-----------------------------------------------------------------------
C   Fit for Trec and zenith opacity using a least square fit, with
C     interitive weighting to downweight high points.
C   Output:
C      PTREC   R    output fitted value
C      PTAU0   R    output fitted value
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL PTREC, PTAU0, XSECZ(MXBUFF), YTSYS(MXBUFF), SIGMA(MXBUFF),
     *   ZTATM, ATREC, CHI2, LTAU0(MXBUFF), COSZ(MXBUFF), DIFF, A, B,
     *   SIG
C
      INTEGER NFIT, I, K, JIF, NITER, ITER, NFIT2
      PARAMETER (NITER = 30)
C-----------------------------------------------------------------------
C                                       Set up call to LSFIT
      NFIT = 0
      DO 200 I = ITSTRT, ITEND
         DO 100 JIF = 1, NIF
            K = (I - 1) * NPOL * NIF + (JIF - 1) * NPOL + JFTPOL
            IF (TS(K).EQ.FBLANK) GO TO 100
C                                       Check zenith angle limit
            IF ((90.0-TSELEV(I)).GT.ZALIM) GO TO 100
            NFIT = NFIT + 1
            XSECZ(NFIT) = 1.0 / SIN (TSELEV(I) * DG2RAD)
            YTSYS(NFIT) = TS(K) - TSPILL(I)
100      CONTINUE
200   CONTINUE
C                                       Do fit to a straight line
C                                       while forcing fit to lower
C                                       bound by adjusting errors
      IF (NFIT .GT. 0) THEN
         DO 500 ITER = 1, NITER
            IF (ITER .EQ. 1) THEN
               DO 300 I = 1, NFIT
                  SIGMA(I) = 0.01
300               CONTINUE
            ELSE
               DO 400 I = 1, NFIT
                  SIGMA(I) = YTSYS(I) - ATREC - XSECZ(I) * ZTATM
                  IF (SIGMA(I) .LT. -1.0)
     *               SIGMA(I) = -0.25 * SIGMA(I)
                  IF (ABS(SIGMA(I)) .LE. 1.0) SIGMA(I) = 1.0
400               CONTINUE
               ENDIF
            CALL LSFIT(XSECZ, YTSYS, NFIT, SIGMA, ATREC, ZTATM, CHI2)
500         CONTINUE
         PTREC = ATREC
C                                        Assuming Trec is correct get
C                                        Tau0 by fitting cos(z) vs Tau
C                                        Tau(cos(z)=1)=TAU0
C
C                                        Calculate TAU0 for straight
C                                        line.
         NFIT2=0
         PTAU0 = -1.0 * LOG (1.0 - ZTATM/TATM)
         DO 600 I = 1, NFIT
            SIG = ABS(YTSYS(I) - ATREC - XSECZ(I) * ZTATM)
C                                        Remove large outlyers
            IF (SIG.LT.0.25*YTSYS(I)) THEN
               NFIT2=NFIT2+1
               LTAU0(NFIT2) = -1.0 * (1.0 /XSECZ(I)) *
     *            LOG(1.0 - (YTSYS(I) - PTREC) / TATM)
               COSZ(NFIT2) = 1.0/XSECZ(I)
               SIGMA(NFIT2) = 0.1 * PTAU0
               END IF
600         CONTINUE
         CALL LSFIT(COSZ, LTAU0, NFIT2, SIGMA, A, B, CHI2)
         DIFF = ABS((A + B) - PTAU0) / PTAU0
C                                        IF difference with PTAU0 is
C                                        large there is a problem,
C                                        stick with 'safe' first
C                                        estimate.
         IF ((A+B).GT.0.0.AND.DIFF.LT.0.5) PTAU0 = A + B
         END IF
C                                       Exit
      RETURN
      END
      SUBROUTINE LSFIT(X,Y,NFIT,SIGMA,A,B,CHI2)
C-----------------------------------------------------------------------
C   Least squares fitting routine  Y = A + B * X
C   Input:
C      X       R(MXBUFF)  array of x-axis values
C      Y       R(MXBUFF)  array of Y-axis values
C      NFIT    I          number of values to fit
C      SIGMA   R(MXBUFF)  array of errors in Y
C   Output:
C      A       R          y-intercept
C      B       R          slope
C      CHI2    R          goodness of fit
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL2.INC'
      REAL X(MXBUFF), Y(MXBUFF), SIGMA(MXBUFF), A, B, CHI2
      INTEGER NFIT, I
      REAL SUMX, SUMY, SUMSIG, SUMX2, SUMY2, SUMXY, WEIGHT
      SUMX=0.
      SUMY=0.
      SUMX2=0.
      SUMY2=0.
      SUMXY=0.
      SUMSIG=0.
      CHI2=0.
      DO 100 I=1,NFIT
        WEIGHT=1./(SIGMA(I)**2)
        SUMSIG=SUMSIG+WEIGHT
        SUMX=SUMX+X(I)*WEIGHT
        SUMY=SUMY+Y(I)*WEIGHT
        SUMX2=SUMX2+X(I)*X(I)*WEIGHT
        SUMY2=SUMY2+Y(I)*Y(I)*WEIGHT
        SUMXY=SUMXY+X(I)*Y(I)*WEIGHT
100      CONTINUE
      A=(SUMX2 * SUMY - SUMX * SUMXY) / (SUMSIG * SUMX2 - SUMX * SUMX)
      B=(SUMSIG * SUMXY - SUMX * SUMY) / (SUMSIG * SUMX2 - SUMX * SUMX)
      DO 200 I=1,NFIT
        CHI2=CHI2+((Y(I)-A-B*X(I))/SIGMA(I))**2
200   CONTINUE
      RETURN
      END
      FUNCTION FAMOD (PTREC, I, K)
C----------------------------------------------------------------------
C   Function to compute opacity correction factor
C   Inputs:
C      PTREC    R    Receiver temperature (K)
C      I, K     I    Indices in system temp. arrays
C---------------------------------------------------------------------
      REAL FAMOD, PTREC
      INTEGER I, K
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DDCH.INC'
      REAL TASOU, FATMP
C---------------------------------------------------------------------
      FAMOD = 1.0
      IF (TS(K).EQ.FBLANK) GO TO 999
C                                       Source temperature
      TASOU = GAIN(K) * SFLUX(K)
C                                       Ta/Tsys
      IF (WTATS) THEN
         FATMP = (TATM + PTREC + TSPILL(I)) / (TASOU * (1.0 / TS(K)
     *      - 1.0) + TATM)
      ELSE
C                                       Standard Tsys
         FATMP = (TS(K) - TATM - PTREC - TSPILL(I)) / (TASOU - TATM)
         END IF
C                                       Get factor into correct
C                                       form for antenna based
C                                       gain correction
      IF (FATMP.NE.0.0) FAMOD = 1.0 / FATMP
C                                       Exit
999   RETURN
      END
      SUBROUTINE SPILL (ISPILL, EL, FREQ, TVAL)
C----------------------------------------------------------------------
C   Compute antenna spill-over (Dhawan & Walker, VLBA Sci. Mem. 1)
C   Function to calculate spillover.
C   A full blown version should integrate the spillover pattern
C   over the measured horizon and over the atmospheric emission.
C   For now, just use a simple curve.
C
C   These curves will have minima of 0.  This ignores the fact
C   that there is some spillover at all elevations.  Just treat
C   the minimum spillover contribution as part of the receiver
C   temperature.
C   Inputs:
C      ISPILL  I    2 => use 7mm for all, 1 normal, 0 do not do
C      EL      R    Elevation (degrees)
C      FREQ    D    Frequency (GHz)
C   OutputL
C      TVAL    R    Spill-over temperature (K)
C----------------------------------------------------------------------
      INTEGER   ISPILL
      REAL      EL, TVAL
      DOUBLE PRECISION FREQ
C
      INTEGER   MEL, MELD, MEL7, MEL13, MEL90, I, M
      PARAMETER (MEL=10, MELD=5, MEL13=7)
      PARAMETER (MEL90=2, MEL7=9)
C
      REAL      SELEV(MEL), ASPILL(MEL), SELEVD(MELD), SPILLD(MELD),
     *   SELEV13(MEL13), SPILL13(MEL13), SELEV90(MEL90), SPILL90(MEL90),
     *   SELEV7(MEL7), SPILL7(MEL7)
C
C     The default spillover is based on a Sept 11-12 1989 6cm Ts vs el
C     curve that was especially clean so it seemed apparent what was
C     spillover.  It will not be quite right for other bands or
C     other azimuths, but it should be better than nothing, I hope.
C
      DATA  SELEVD  / 2., 14., 19., 30., 90. /
      DATA  SPILLD  / 8.,  7.,  5.,  0.,  0. /
C
C     The 13 cm spillover curve is based on early test data from PT
C     with and without the dichroic.  The plots are with the Jodrell
C     meeting viewgraphs.  A 2.7 deg. atmosphere was assumed and
C     the 0 level was set by the "without" data (which match the
C     default spillover reasonably well.
      DATA  SELEV13 /  0., 11., 14., 19., 30.,  46., 90. /
      DATA  SPILL13 / 18., 19., 17., 15., 11.,   7.,  0. /
C
C     Treat 90 and 50 cm as if there is no spillover (or rather
C     that it is constant.  Certainly the ground pickup around the
C     subreflector at low elevation is not a problem at these
C     bands.
C
      DATA  SELEV90 / 0., 90. /
      DATA  SPILL90 / 0.,  0. /
C
C     7mm spillover from Vivek.
C
      DATA  SELEV7  / 2.,  15.,  20., 25., 30., 40., 50., 70., 90. /
      DATA  SPILL7 / 12.0, 11.0, 9.0, 6.5, 5.0, 2.0, 1.0, 0.0, 0.0 /
C--------------------------------------------------------------------
C                                       don't do
      IF ((ISPILL.LE.0) .OR. (ISPILL.GT.2)) THEN
         TVAL = 0.0
C                                       do
      ELSE
C                                       7mm
         IF (((FREQ.GT.30.0D0) .AND. (FREQ.LT.50.0D0)) .OR.
     *      (ISPILL.EQ.2)) THEN
            M = MEL7
            DO 10 I = 1,M
               SELEV(I) = SELEV7(I)
               ASPILL(I) = SPILL7(I)
 10            CONTINUE
C                                       13cm
         ELSE IF ((FREQ.GT.2.0D0) .AND. (FREQ.LT.3000.0)) THEN
            M = MEL13
            DO 20 I = 1,M
               SELEV(I) = SELEV13(I)
               ASPILL(I) = SPILL13(I)
 20            CONTINUE
C                                       50 or 90 cm
         ELSE IF (FREQ.LT.1.0D0) THEN
            M = MEL90
            DO 30 I = 1,M
               SELEV(I) = SELEV90(I)
               ASPILL(I) = SPILL90(I)
 30            CONTINUE
C                                       Some other band.
         ELSE
            M = MELD
            DO 40 I = 1,M
               SELEV(I) = SELEVD(I)
               ASPILL(I) = SPILLD(I)
 40            CONTINUE
            END IF
C                                       Interpolate:
         TVAL = 0.0
         DO 50 I = 1,M-1
            IF ((EL.GT.SELEV(I)) .AND. (EL.LE.SELEV(I+1))) THEN
               TVAL = (ASPILL(I) * (SELEV(I+1) - EL) +
     *            ASPILL(I+1) * (EL - SELEV(I))) /
     *            (SELEV(I+1) - SELEV(I))
               GO TO 999
               END IF
 50         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE TXTWX (IRET)
C----------------------------------------------------------------------
C   Read the auxillary weather file
C   Output:
C      IRET    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER NPARS, NKEYW, MEXTRA, MXDUM
      PARAMETER (NKEYW = 1, MXDUM = 100000)
      PARAMETER (MEXTRA = MAXANT)
      PARAMETER (NPARS = NKEYW + MEXTRA)
      LOGICAL WEOF, WERROR
      CHARACTER LPARS(NPARS)*8, LMARK*8,
     *   LVALS(NPARS)*8, LDUMMY*8
      DOUBLE PRECISION DUMMY(MXDUM), DVALS(NPARS), DAVG, DPEAK
      INTEGER I, J, IOFF, KMODE, ITYP, ISTN, NSKIP, NDIM, IERR,
     *   NVALS, N
C                                       Recognized KEYIN keywords
      DATA LPARS /'WEATHER ', MEXTRA * '        '/
      DATA LMARK /'/       '/
C-----------------------------------------------------------------------
C                                       Initialize commons
      NDIM = MAXANT
      CALL RFILL (NDIM, 0.0, TGPEAK)
      CALL RFILL (NDIM, 0.0, TGAVG)
      IF (LINFIL.NE.' ') THEN
C                                       Open external text file
         CALL ZTXOPN ('READ', ILUNF, IFINDF, LINFIL, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1020) IERR, LINFIL
            GO TO 990
            END IF
      ELSE IF(INVERS .GT. 0) THEN
C                                     If WX table version specified, read
C                                     in weather data from WX table
         CALL REAWX(IERR)
         IF(IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT, 1030) IERR, INVERS
         ELSE
            WRITE (MSGTXT, 1040) INVERS
            END IF
         GO TO 990
      ELSE
C                                     If no text file or WX table version
C                                     specified, leave now.
C                                     just put in a default value of
C                                     20 C for the ground temperature
            DO 10 I = 1, NDIM
               TGPEAK(I) = 20.0
 10            CONTINUE
            MSGTXT = '*** WX text file or WX table version not'
            CALL MSGWRT (8)
            MSGTXT = '*** specified, using default ground'
            CALL MSGWRT (8)
            MSGTXT = '*** temperature of 20 C'
            IRET = 1
            GO TO 990
         END IF
C                                       Add antenna names as keywords
      IOFF = NKEYW
      DO 70 I = 1, MAXANT
         IF (I.LE.NSTNS) LPARS(I+IOFF) = STNNAM(I)
70       CONTINUE
C
      NSKIP = 0
C                                       While (NOT EOF and NOT ERROR) do
C                                         Read record;
80    CONTINUE
C                                       Set defaults
      DO 100 J = 1, NPARS
         DVALS(J) = DBLANK
         LVALS(J) = '        '
100      CONTINUE
C                                       Echo KEYIN input if verbose
C                                       print level selected.
      KMODE = 0
      IF (IPRTLV.GT.0) KMODE = 1
C
      CALL KEYIN (LPARS, DVALS, LVALS, NPARS, LMARK, KMODE, ILUNF,
     *   IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
      IF (WERROR) THEN
         WRITE (MSGTXT,1100) IERR
         GO TO 900
         END IF
C                                       EOF or ERROR encountered
      IF (WEOF.OR.WERROR) GO TO 900
C                                       Determine record type
      ITYP = 0
      IF (DVALS(1).NE.DBLANK) ITYP = 1
C                                       Extract station name in this
C                                       record
      IOFF = NKEYW
      ISTN = 0
      DO 130 I = 1, NSTNS
         IF ((DVALS(I+IOFF).NE.DBLANK).AND.(ISTN.EQ.0)) ISTN = I
130      CONTINUE
C                                       Case (record_type) of:
C                                       0: Unidentified
      IF (ITYP.NE.0) GO TO 200
         NSKIP = NSKIP + 1
         GO TO 850
C                                       1: Weather record
200   IF (ITYP.NE.1) GO TO 850
C                                       Is a station specified ?
         IF (ISTN.EQ.0) THEN
            WRITE (MSGTXT,1150)
            WERROR = .TRUE.
            GO TO 850
            END IF
C                                       Read weather values
         KMODE = 3
         IF (IPRTLV.GT.0) KMODE = 4
         NVALS = MXDUM
         CALL KEYIN (LDUMMY, DUMMY, LVALS, NVALS, LMARK, KMODE,
     *      ILUNF, IFINDF, IERR)
         WEOF = (IERR.EQ.1)
         WERROR = ((IERR.NE.0).AND.(.NOT.WEOF))
         IF (WERROR.OR.WEOF) THEN
            WRITE (MSGTXT,1520) IERR, LPARS(ITYP)
            GO TO 850
            END IF
C                                       Extract ground temperatures
         DAVG = 0.0
         DPEAK = -1.0E30
         N = 0
         DO 250 I = 3, (NVALS-6), 9
            DAVG = DAVG + DUMMY(I)
            DPEAK = MAX (DPEAK, DUMMY(I))
            N = N + 1
250         CONTINUE
         I = TELNO(ISTN)
         IF (N.GT.0) THEN
            TGAVG(I) = DAVG / N
         ELSE
            TGAVG(I) = 0.0
            ENDIF
         TGPEAK(I) = DPEAK
         GO TO 850
C                                       Endcase (record_type)
850   IF (.NOT.(WERROR.OR.WEOF)) GO TO 80
C                                       Endwhile
900   IF (WERROR) THEN
         IRET = 9
         GO TO 990
         END IF
C                                       Message about records skipped
      WRITE (MSGTXT,1900) NSKIP
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Hard close before exit
999   CONTINUE
      IF (LINFIL.NE.' ') CALL ZTXCLS (ILUNF, IFINDF, IERR)
      RETURN
C-----------------------------------------------------------------------
1020  FORMAT ('TXTWX: ERR',I3,' OPENING ',A48)
1030  FORMAT ('TXTWX: ERR',I3,' OPENING WX TABLE ',I3)
1040  FORMAT ('TXTWX: Weather data read from WX table ',I3)
1100  FORMAT ('TXTWX: ERROR',I3,' READING WEATHER FILE')
1150  FORMAT ('TXTWX: WEATHER RECORD WITHOUT ANTENNA NAME')
1520  FORMAT ('TXTWX: ERROR',I3,' READING WEATHER DATA')
1900  FORMAT (I6,' unidentified WX records skipped on input')
      END
      SUBROUTINE PLOTOP (IANT, N, PTREC, PTAU0, IRET)
C-----------------------------------------------------------------------
C   This routine will plot the opacity solutions, averaged over IF.
C   Input:
C      IANT    I        Antenna number
C      N       I(2)     No. of data points for each polzn.
C      PTREC   R(2)     Receiver temp. soln. for each polzn.
C      PTAU0   R(2)     Zenith opacity soln. for each polzn.
C   Output:
C      IRET    I        Termination code (0 => ok)
C   Input from common:
C      TSAVG   R(*,2)   Measured Ts or Ta/Tsys, avg. over IF.
C      TSFIT   R(*,2)   Fitted Tsys or Ta/Tsys, avg. over IF.
C      FAVG    R(*,2)   Opacity correction factor, avg. over IF.
C      WDOTV   I        If true then plot to TV dev., else PL file.
C      JLTYPE  I        Label type from plot (follows AIPS convention).
C-----------------------------------------------------------------------
      REAL PTREC(2), PTAU0(2)
      INTEGER N(2), IANT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DPLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      CHARACTER LABOVE*80, LBELOW*80, LXUNIT*20, LYUNIT*20, LPOLZN(2)*1,
     *   CTEMP*4
      REAL BLC(2), TRC(2), PGBLC(2), PGTRC(2), XWIDTH, YWIDTH, XDIV,
     *   YDIV, XMIN, XMAX, YMIN, YMAX, XPFRAC, YPFRAC, ZMAX, ZMIN
      INTEGER NX, NY, IGTYPE, ITVORG(2), ITVCHN, IERR, NABOVE, NBELOW,
     *   JPOL, KPOL, I
      LOGICAL   LDOTV
      DATA NX /2/, NY /4/, XPFRAC / 0.35/, YPFRAC / 0.9/
      DATA LPOLZN /'R', 'L'/
C----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      CALL FILL (2, 0, ITVORG)
      ITVCHN = 1
      LBELOW = ' '
      LDOTV = WDOTV.EQ.1
C                                       New page ?
      IF (MOD (IPLROW,NY).EQ.1) THEN
C                                       put some parameters into header
         CALL RFILL (50, 0.0, XANT)
         CTEMP = ' '
         DO 20 JPOL = 1,NPOL
            KPOL = IPOLUV + (JPOL - 1) + (IPOL1 - 1)
            CTEMP(JPOL:JPOL) = LPOLZN(KPOL)
            XANT(JPOL) = IANT
            XTAU0(JPOL) = PTAU0(JPOL)
            XTREC(JPOL) = PTREC(JPOL)
            XDOFIT(JPOL) = IDOFIT(IANT)
 20         CONTINUE
         CALL CHR2H (4, CTEMP, 1, XSTOK)
         IGTYPE = 31
         CALL PLINIT (IGTYPE, INDISK, ICNO, CATBLK, NPARM, XNAMEI,
     *      LDOTV, GRCHAN, ITVCHN, ITVORG, BUFF3, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
C                                       Define plotting page
         PGBLC(1) = 0.0
         PGBLC(2) = 0.0
         PGTRC(1) = 1000.0
         PGTRC(2) = 1000.0
         NABOVE = 1
         WRITE (LABOVE,1030) LNAME, LCLASS, INSEQ, INDISK, JBIF, JEIF
         NBELOW = 0
         LXUNIT = ' '
         LYUNIT = ' '
         CALL PLAXES (PGBLC, PGTRC, 0.0, 0.0, 0.0, 0.0, JLTYPE, LXUNIT,
     *      LYUNIT, NABOVE, NBELOW, LABOVE, LBELOW, 1, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
         END IF
C                                       Compute subplot widths (in
C                                       pixels)
      XDIV = NX + (NX + 0.5) * XPFRAC
      YDIV = NY + (NY + 1.0) * YPFRAC
      XWIDTH = (PGTRC(1) - PGBLC(1)) / XDIV
      YWIDTH = (PGTRC(2) - PGBLC(2)) / YDIV
C                                       Plot for each polzn.
      DO 200 JPOL = 1,NPOL
C                                       Left column subplot:
C                                       Tsys or Ta/Tsys vs. sec z
         BLC(1) = PGBLC(1) + XWIDTH * XPFRAC
         I = MOD (NY - MOD (IPLROW, NY), NY)
         BLC(2) = PGBLC(2) + YWIDTH * YPFRAC +
     *      YWIDTH * (1.0 + YPFRAC) * I
         TRC(1) = BLC(1) + XWIDTH
         TRC(2) = BLC(2) + YWIDTH
C                                       Text header
         NABOVE = 1
         KPOL = IPOLUV + (JPOL - 1) + (IPOL1 - 1)
         WRITE (LABOVE,1200) STNNAM(IANT), LPOLZN(KPOL), PTREC(JPOL),
     *      PTAU0(JPOL)
         CALL REFRMT (LABOVE, '_', IERR)
         NBELOW = 0
         LXUNIT = 'SEC Z'
         IF (WTATS) THEN
            LYUNIT = 'TA/TSYS'
         ELSE
            LYUNIT = 'TSYS'
            END IF
C                                       Determine axis range
         CALL PLRANG (TSECZ(1,JPOL), N(JPOL), 1.1, 0.2, XMIN, XMAX)
         CALL PLRANG (TSAVG(1,JPOL), N(JPOL), 1.2, 5.0, YMIN, YMAX)
         IF (IDOFIT(IANT).EQ.1) THEN
            CALL PLRANG (TSFIT(1,JPOL), N(JPOL), 1.2, 5.0, ZMIN, ZMAX)
            YMIN = MIN (YMIN, ZMIN)
            YMAX = MAX (YMAX, ZMAX)
            END IF
         IF ((XAPARM(5).GT.0.0) .AND. (XMAX/1.05.GT.XAPARM(5))) XMAX =
     *      XAPARM(5) * 1.05
C                                       Draw axes
         CALL PLAXES (BLC, TRC, XMIN, XMAX, YMIN, YMAX, JLTYPE, LXUNIT,
     *      LYUNIT, NABOVE, NBELOW, LABOVE, LBELOW, 1, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Plot points
         CALL PLPNT (TSECZ(1,JPOL), TSAVG(1,JPOL), N(JPOL), 1, 0.03,
     *      IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1220) IERR
            GO TO 990
            END IF
C                                       Plot fitted function
         IF (IDOFIT(IANT).EQ.1) THEN
            CALL PLLINE (TSECZ(1,JPOL), TSFIT(1,JPOL), IPLWRK,
     *         N(JPOL), IERR)
            IF (IERR.NE.0) THEN
               IRET = 4
               WRITE (MSGTXT,1230) IERR
               GO TO 990
               END IF
            END IF
C                                       Right column subplot:
C                                       Opacity factor vs sec z.
         BLC(1) = BLC(1) + XWIDTH * (1.0 + XPFRAC)
         TRC(1) = BLC(1) + XWIDTH
C                                       Plot header
         NABOVE = 0
         LABOVE = ' '
         NBELOW = 0
         LXUNIT = 'TIME (HOURS)'
         LYUNIT = 'OPACITY'
C                                       Determine axis range
         CALL PLRANG (TSTIME(1,JPOL), N(JPOL), 1.1, 0.2, XMIN, XMAX)
         CALL PLRANG (GAVG(1,JPOL), N(JPOL), 1.2, 0.012, YMIN, YMAX)
C                                       Draw axes
         CALL PLAXES (BLC, TRC, XMIN, XMAX, YMIN, YMAX, JLTYPE, LXUNIT,
     *      LYUNIT, NABOVE, NBELOW, LABOVE, LBELOW, 1, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 5
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
C                                       Plot the opacities
         CALL PLPNT (TSTIME(1,JPOL), GAVG(1,JPOL), N(JPOL), 1, 0.03,
     *      IERR)
         IF (IERR.NE.0) THEN
            IRET = 6
            WRITE (MSGTXT,1220) IERR
            GO TO 990
            END IF
C                                       Increment plot row count
         IPLROW = IPLROW + 1
C
 200     CONTINUE
C                                       End of page ?
      IF (MOD (IPLROW,NY).EQ.1) THEN
         GPHPAG = .TRUE.
         CALL PLCLOS (INDISK, ICNO, CATBLK, BUFF3, IRET)
         IF (IRET.LT.0) THEN
            WDOTV = 0
            IRET = 0
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      GPHPAG = .FALSE.
      CALL PLCLOS (INDISK, ICNO, CATBLK, BUFF3, IERR)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1020 FORMAT ('PLOTOP: ERROR',I5,' RETURNED BY PLINIT')
 1030 FORMAT ('Opacity plot for ',A12,'.',A6,'.',I4,'.',I4,5X,
     *   'IF=',I2,' - ',I2)
 1040 FORMAT ('PLOTOP: ERROR',I5,' RETURNED BY PLAXES')
 1200 FORMAT (A8,'_',A1,' _TR=',F8.2,' _TAU=',F7.3)
 1220 FORMAT ('PLOTOP: ERROR',I5,' RETURNED BY PLPNT')
 1230 FORMAT ('PLOTOP: ERROR',I5,' RETURNED BY PLLINE')
      END
      SUBROUTINE SIMPLX (DF, DB, DSCAL, M, DTOL, NITER, DBUFF, NR, IRET)
C----------------------------------------------------------------------
C   Nelder Mead minimisation of a function by direct simplex method
C   Inputs:
C      DF      D     External function to be minimised; will be called
C                    as DF(DB,M) where DB is an array of M parameters.
C      M       I     Number of free parameters
C      DSCAL   D(M)  Intial steps to take in each parameter.
C      DTOL    D     Tolerance to be used in convergence test.
C   Input/output:
C      NITER   I     Maximum number of iterations allowed; actual
C                    values returned on output.
C      DB      D(M)  Intial guess at parameters on input; values at
C                    minimum on output.
C      DBUFF   D(*)  Work buffer (at least (M+1)*(M+6) elements)
C   Output:
C      IRET    I     Return code (0 => converged; 1=> iteration limit
C                    exceeded)
C----------------------------------------------------------------------
      DOUBLE PRECISION DF
      EXTERNAL DF
      INTEGER M, NITER, IRET, NR
      DOUBLE PRECISION DB(M), DSCAL(M), DBUFF(M+1,M+6), DTOL
C
      DOUBLE PRECISION DRNG, DALPHA, DGAMMA, DBETA, DFR, DFE, DFS
      INTEGER I, J, IHIGH, IHIGH2, ILOW, NLOOP
      DATA DALPHA, DGAMMA, DBETA /1.0D0, 2.0D0, 0.5D0/
C----------------------------------------------------------------------
      IRET = 0
C                                       Set up the initial simplex
      DO 100 I = 1, (M+1)
         DO 80 J = 1, M
            DBUFF(J,I) = DB(J)
80          CONTINUE
         IF (I.NE.1) DBUFF(I-1,I) = DBUFF(I-1,I) + DSCAL(I-1)
100      CONTINUE
C
      NLOOP = 1
C                                       Start of iteration loop
110   IF (NLOOP.GT.NITER) GO TO 890
C                                       Store function val. at simplex
C                                       points in row (M+2) of DBUFF
      DO 120 I = 1, (M+1)
         DBUFF(I,M+2) = DF (DBUFF(1,I), M, NR)
120      CONTINUE
C                                       Find highest, lowest points
      IHIGH = 1
      ILOW = 1
      DO 140 I = 2, (M+1)
         IF (DBUFF(I,M+2).GT.DBUFF(IHIGH,M+2)) IHIGH = I
         IF (DBUFF(I,M+2).LT.DBUFF(ILOW,M+2)) ILOW = I
140      CONTINUE
C                                       Check for convergence
      DRNG = 2.0D0 * ABS (DBUFF(IHIGH,M+2) - DBUFF(ILOW,M+2)) /
     *   (ABS (DBUFF(IHIGH,M+2)) + ABS (DBUFF(ILOW,M+2)))
      IF (DRNG.LT.DTOL) GO TO 900
C                                       Find centroid, excluding
C                                       highest point; simultaneously
C                                       determine 2nd highest point;
C                                       Store centroid in row (M+3)
      DO 150 I = 1, M
         DBUFF(I,M+3) = 0.0D0
150      CONTINUE
C
      IHIGH2 = 1
      DO 160 I = 1, (M+1)
         IF (I.EQ.IHIGH) GO TO 160
         IF (DBUFF(I,M+2).GT.DBUFF(IHIGH2,M+2)) IHIGH2 = I
         DO 155 J = 1, M
            DBUFF(J,M+3) = DBUFF(J,M+3) + DBUFF(J,I)
155         CONTINUE
160      CONTINUE
C
      DO 170 I = 1, M
         DBUFF(I,M+3) = DBUFF(I,M+3) / M
170      CONTINUE
C                                       Reflect highest point through
C                                       centroid by factor alpha; store
C                                       in row (M+4)
      DO 180 I = 1, M
         DBUFF(I,M+4) = (1.0D0 + DALPHA) * DBUFF(I,M+3) -
     *      DALPHA * DBUFF(I,IHIGH)
180      CONTINUE
C                                       Compute function value at
C                                       reflected point.
      DFR = DF (DBUFF(1,M+4), M, NR)
C                                       Is this less than the lowest
C                                       point in the simplex ?
      IF (DFR.LT.DBUFF(ILOW,M+2)) THEN
C                                       Extend further by factor gamma
C                                       Store new point in row (M+5)
         DO 190 I = 1, M
            DBUFF(I,M+5) = (1.0D0 + DALPHA * DGAMMA) * DBUFF(I,M+4) -
     *         DALPHA * DGAMMA * DBUFF(I,IHIGH)
190         CONTINUE
C                                       Compute function value at
C                                       extended point.
         DFE = DF (DBUFF(1,M+5), M, NR)
C                                       Replace highest point with
C                                       the lower of the reflected or
C                                       extended points; end of
C                                       iteration.
            DO 200 I = 1, M
               IF (DFE.LT.DFR) THEN
                  DBUFF(I,IHIGH) = DBUFF(I,M+5)
               ELSE
                  DBUFF(I,IHIGH) = DBUFF(I,M+4)
                  END IF
200            CONTINUE
C                                       Reflected point was not
C                                       lower than lowest point
      ELSE
C                                       Is the reflected point less
C                                       than the 2nd highest point ?
C                                       If so, then replace highest
C                                       point and end this iteration
         IF (DFR.LT.DBUFF(IHIGH2,M+2)) THEN
            DO 210 I = 1, M
               DBUFF(I,IHIGH) = DBUFF(I,M+4)
210            CONTINUE
         ELSE
C                                       Is the reflected point between
C                                       the 2nd highest and highest
C                                       point ?
            IF ((DFR.GE.DBUFF(IHIGH2,M+2)).AND.
     *         (DFR.LT.DBUFF(IHIGH,M+2))) THEN
C                                       Replace highest point
               DO 220 I = 1, M
                  DBUFF(I,IHIGH) = DBUFF(I,M+4)
220               CONTINUE
               DBUFF(IHIGH,M+2) = DF (DBUFF(1,IHIGH), M, NR)
               END IF
C                                       Now find new vertex by
C                                       contracting the simplex; store
C                                       contracted point in row (M+6);
C                                       Use factor beta to contract
            DO 230 I = 1, M
               DBUFF(I,M+6) = DBUFF(I,IHIGH) * DBETA +
     *            (1.0D0 - DBETA) * DBUFF(I,M+3)
230            CONTINUE
C                                       Compute func. val. at new point
            DFS = DF (DBUFF(1,M+6), M, NR)
C                                       Is this better than the worst
C                                       vertex ?
            IF (DFS.LE.MIN(DBUFF(IHIGH,M+2), DFR)) THEN
C                                       Replace highest point
               DO 240 I = 1, M
                  DBUFF(I,IHIGH) = DBUFF(I,M+6)
240               CONTINUE
            ELSE
C                                       Nothing has worked; contract
C                                       simplex about lowest point.
               DO 260 I = 1, (M+1)
                  IF (I.EQ.ILOW) GO TO 260
                  DO 250 J = 1, M
                     DBUFF(J,I) = DBETA * DBUFF(J,I) +
     *                  (1.0D0 - DBETA) * DBUFF(J,ILOW)
250                  CONTINUE
260               CONTINUE
               END IF
            END IF
         END IF
C                                       Next iteration
      NLOOP = NLOOP + 1
      GO TO 110
C                                       Iteration limit exceeded
 890  IRET = 1
C                                       Copy solution to output
 900  DO 910 I = 1, M
         DB(I) = DBUFF(I,ILOW)
 910     CONTINUE
C                                       Exit
      NITER = NLOOP
C
 999  RETURN
      END
      SUBROUTINE REAWX (IERR)
C----------------------------------------------------------------------
C   Read the weather (WX) table
C   Output:
C      IERR    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DWXV.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   WXBUFF(512), LUNWX, IWXRNO, I, NROW,
     *   WXKOLS(MAXWXC), WXNUMV(MAXWXC), TABVER, ANT, JSUB, IRET
      CHARACTER OBSCOD*8, OBSDAT*8
      DOUBLE PRECISION TIMEWX
      REAL     PRESS, DTIME, TEMP, DEWPT, WVEL, WDIR, WGUST, PRECIP,
     *   H2OCOL, IONCOL
      DATA LUNWX /30/
C-----------------------------------------------------------------------
C                                               Open WX table
      CALL WXINI ('READ', WXBUFF, INDISK, ICNO, INVERS, CATBLK, LUNWX,
     *   IWXRNO, WXKOLS, WXNUMV, OBSCOD, OBSDAT, TABVER, IERR)
      NROW=WXBUFF(5)
      IF (NROW.LT.1) IERR = 6
      IF (IERR.EQ.0) THEN
C                                               Fill in peak temperature
C                                               (TGPEAK) for each antenna
         DO 100 I = 1,NROW
            IWXRNO = I
            CALL TABWX ('READ', WXBUFF, IWXRNO, WXKOLS, WXNUMV, TIMEWX,
     *         DTIME, ANT, JSUB, TEMP, PRESS, DEWPT, WVEL, WDIR, WGUST,
     *         PRECIP, H2OCOL, IONCOL, IERR)
            IF (IERR.GT.0) THEN
               GO TO 900
            ELSE IF (IERR.EQ.0) THEN
C                                       test fot FITLD error
               IF (ABS(ANT).GT.100) THEN
                  IERR = 6
                  GO TO 900
                  END IF
               IF ((JSUB.EQ.ISUBA) .OR. (JSUB.LE.0) .OR. (ISUBA.LE.0))
     *            THEN
                  TGPEAK(ANT) = MAX (TGPEAK(ANT), TEMP)
                  TGAVG(ANT) = TGAVG(ANT) + TEMP
                  NTEMP(ANT) = NTEMP(ANT) + 1
                  END IF
               END IF
100         CONTINUE
         DO 110 I = 1,MAXANT
            IF (NTEMP(I).GT.0) TGAVG(I) = TGAVG(I) / NTEMP(I)
110         CONTINUE
         END IF
C
 900  IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR
         CALL MSGWRT (8)
         IF (IERR.EQ.6) THEN
            WRITE (MSGTXT,1200) ANT
            CALL MSGWRT (8)
            END IF
         END IF
      CALL TABIO ('CLOS', 0, 1, IWXRNO, WXBUFF, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('REAWX: ERROR',I3,' SOMETHING WRONG WITH WX TABLE')
 1200 FORMAT ('REAWX: TABWX FINDS ANTENNA #',I11,', WX TABLE CORRUPT?')
      END
      SUBROUTINE DFTAU (IERR)
C----------------------------------------------------------------------
C   Set default zenith opacity (TAU0)
C   Output:
C      IERR    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IERR, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   I, NANT
C-----------------------------------------------------------------------
C                                       if freq. OK then estimate
C                                       TAU0 assiming a linear
C                                       relationship btw the opacity
C                                       and the freq.: 0 at 0 GHz,
C                                       0.1 at 43 GHz.
      IF (IRET.EQ.0) THEN
         NANT = NSTNS
         IF ((NANTWT.GT.0) .AND. (.NOT.ANTNEG)) NANT = NANTWT
         DO 10 I = 1,NANT
            IF (IDOFIT(I).GE.0) TAU0(I) = 0.1/43.0E9 * FREQS(NIFUV/2)
10          CONTINUE
        WRITE (MSGTXT,1000) TAU0(1)
        CALL MSGWRT (8)
      ELSE
        WRITE (MSGTXT, 1010)
        CALL MSGWRT (8)
        END IF
      IERR = IRET
      RETURN
C--------------------------------------------------------------------
 1000 FORMAT ('DFTAU: INITIAL TAU0 SET TO', F6.3)
 1010 FORMAT ('DFTAU: CANNOT SET DEFAULT TAU0, TAU0 FOR FIT REMAINS 0')
      END
      SUBROUTINE DFTRC (NOTREC, IERR)
C----------------------------------------------------------------------
C   Set default receiver temperature (TREC) and/or set TSMAX
C   Input:
C      NOTREC  L    If T, set default TREC and TSMAX
C                   If F, just set TSMAX
C   Output:
C      IERR    I    Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IERR, IRET
      LOGICAL NOTREC
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'APCAL.INC'
      INCLUDE 'APCAL2.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL TIMETY, TSYST(2,MAXIF), TANT(2,MAXIF), TSLOW(MAXANT,2,10),
     *   TINTTY, TTEMP
      INTEGER ITYRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), NPOLTY, NIFTY,
     *   ITYSOU, ITYANT, ITYSUB, ITYFQD, NROW, JROW, IP, IIF, J, JJ, I,
     *   KP
      CHARACTER LPOL(2)*3
      DATA LPOL / 'RCP', 'LCP'/
C                                       Open TY table
      CALL TYINI ('READ', BUFF1, INDISK, ICNO, ITYVER, CATBLK, ILUN1,
     *   ITYRNO, TYKOLS, TYNUMV, NPOLTY, NIFTY, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1010)
         GO TO 990
         END IF
      NROW = BUFF1(5)
      JROW = 1
      DO 30 JJ = 1, NSTNS
         DO 20 IP = 1, NPOLTY
            KP = IPOLUV + (IP - 1) + (IPOL1 - 1)
            TSMAX(2*(JJ-1)+KP)=0.0
            DO 10 J = 1, 10
               TSLOW(JJ, KP, J) = 10000.0
10            CONTINUE
20         CONTINUE
30      CONTINUE
40    IF (JROW.LE.NROW) THEN
         CALL TABTY ('READ', BUFF1, ITYRNO, TYKOLS,
     *      TYNUMV, NPOLTY, NIFTY, TIMETY, TINTTY, ITYSOU, ITYANT,
     *      ITYSUB, ITYFQD, TSYST, TANT, IRET)
         JROW = ITYRNO
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT, 1010)
            GO TO 990
         ELSE IF (IRET.EQ.0) THEN
C                                       Check FREQID
            IF ((ITYFQD.NE.IFRQID) .AND. (IFRQID.GT.0) .AND.
     *         (ITYFQD.GT.0)) GO TO 90
C                                       Check subarray
            IF ((ITYSUB.NE.ISUBA) .AND. (ITYSUB.GT.0) .AND.
     *         (ISUBA.GT.0)) GO TO 90
C                                       Now find highest and 10 lowest Tsys
C                                       for each antenna and polzn.
            DO 80 IP = 1,NPOLTY
               KP = IPOLUV + (IP - 1) + (IPOL1 - 1)
               DO 70 IIF = 1, NIFTY
                  IF (TSYST(IP,IIF).GT.TSMAX(2*(ITYANT-1)+KP))
     *               TSMAX(2*(ITYANT-1)+KP) = TSYST(IP,IIF)
                  DO 60 J = 1,10
                     IF (TSYST(IP,IIF).LT.TSLOW(ITYANT,KP,J)) THEN
                        DO 50 JJ = 1,10-J
                           TSLOW(ITYANT,KP,J+JJ) =
     *                        TSLOW(ITYANT,KP,J+JJ-1)
50                         CONTINUE
                        TSLOW(ITYANT,KP,J) = TSYST(IP,IIF)
                        END IF
60                   CONTINUE
70                CONTINUE
80             CONTINUE
            END IF
90       GO TO 40
         END IF
C                                       Now assign 10th lowest Tsys
C                                       to selected antenna
      IF (NOTREC) THEN
         IF ((NANTWT.EQ.0) .OR. (ANTNEG)) THEN
            DO 150 I = 1,NSTNS
               TTEMP = (TGPEAK(I) + 273.15) * 0.652 + 84.6
               DO 140 IP = 1, NPOLTY
                  IF (IDOFIT(I).GE.0) THEN
                     KP = IPOLUV + (IP - 1) + (IPOL1 - 1)
                     TREC(2*(I-1)+KP) = TSLOW(I,KP,10) - TTEMP*TAU0(I)
                     IF (LOPCOD.NE.'LESQ') THEN
                        WRITE (MSGTXT,1000) STNNAM(I), LPOL(KP),
     *                  TREC(2*(I-1)+KP)
                        CALL MSGWRT (8)
                        ENDIF
                     ENDIF
 140           CONTINUE
 150        CONTINUE
         ELSE
            DO 170 I = 1,NANTWT
               TTEMP=(TGPEAK(I) + 273.15) * 0.652 + 84.6
               DO 160 IP = 1, NPOLTY
                  KP = IPOLUV + (IP - 1) + (IPOL1 - 1)
                  IF (IDOFIT(I).GE.0) THEN
                     TREC(2*(I-1)+KP) = TSLOW(IANTWT(I),KP,10)
     *                  - TTEMP*TAU0(I)
                     IF(LOPCOD .NE. 'LESQ') THEN
                        WRITE (MSGTXT,1000) STNNAM(IANTWT(I)), LPOL(KP),
     *                     TREC(2*(I-1)+KP)
                        CALL MSGWRT (8)
                        ENDIF
                    ENDIF
 160             CONTINUE
 170           CONTINUE
            END IF
         GO TO 180
         ENDIF
 990  CALL MSGWRT (8)
C
 180  IERR = IRET
      CALL TABIO ('CLOS', 0, ITYRNO, BUFF1, BUFF1, IERR)
C
 999  RETURN
C--------------------------------------------------------------------
 1000 FORMAT ('DFTRC:',A8,A3,' Initial Trec set to =',F7.2)
 1010 FORMAT ('DFTRC: CANNOT SET DEFAULT TREC, TREC FOR FIT REMAINS 0')
      END
