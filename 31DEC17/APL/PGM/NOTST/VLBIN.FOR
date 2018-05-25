LOCAL INCLUDE 'VLBIN.INC'
C                                       Local include for VLBIN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER INFILE*48, IN2FIL*48, NAMOUT*12, CLAOUT*6, XSOUR(30)*16
      HOLLERITH XINFIL(12), XIN2FI(12), XNAMOU(3), XCLAOU(2),
     *   XXSOUR(4,30)
      INTEGER   SEQOUT, DISKO, KLOCWT, CCNOUT, NSW, KSW, ASW, NSWPHR
      LOGICAL   DOUVCM
      REAL      XSOUT, XDISO, XBCHAN, XECHAN, XBDROP, XEDROP, XNPTS,
     *   XDOUV, APARM(20), KEYVAL(2), TIMRAN(8), ACCBW, ACCFQL, ACCFQH,
     *   SWTIME(3600,2)
      DOUBLE PRECISION ARGD, TSW
C                                       Antenna Info
      INTEGER   NANT, ANTSYM(MAXANT), NACSCN(MAXANT), KKREFS
      CHARACTER ANTNAM(MAXANT)*8, REFNAM*8
      REAL      XIAT, XUT1, ANTOFF(MAXANT), ACBIAS(MAXANT)
      DOUBLE PRECISION  ANTLOC(3,MAXANT), GST0
C                                       Source Info
      CHARACTER SORNAM*16, CALCOD*4, OBSDAT*8
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), SUBUFF(512), IDSOU,
     *   IQUAL, CURSOU
      LOGICAL   GOTSOU(300)
      INTEGER   ISURNO
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION  FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, LSRVEL(MAXIF), PMRA, PMDEC, SRCFRQ(1000),
     *   FRQOFF(1000), RAOBS, DECOBS
C                                       Calibration Info
      INTEGER   CLKOLS(MAXCLC), CLNUMV(MAXCLC), CLBUFF(1024),
     *   NUMANT, NUMPOL, NUMIF, ANTNO(2), SUBA, ISBAND(MAXIF)
      INTEGER   ICLRNO, FREQID, DATASB
      REAL      TIMEI, CALINT, CURINT
      DOUBLE PRECISION CTIME
C                                       FQ info
      INTEGER   MXFQE
      PARAMETER (MXFQE=100)
      INTEGER  IFQRNO, CURFQI, FQBUFF(512), FQKOLS(MAXFQC),
     *   FQNUMV(MAXFQC), SIDBND(MXFQE)
      REAL     CHBW(MXFQE), TOTBW(MXFQE)
      DOUBLE PRECISION IFFREQ(MXFQE), FRQDEL
C                                       Commons
      COMMON /INPARM/ XINFIL, XIN2FI, XNAMOU, XCLAOU, XSOUT, XDISO,
     *   XXSOUR, XBCHAN, XECHAN, XBDROP, XEDROP, XNPTS, XDOUV, APARM,
     *   KEYVAL, TIMRAN, SEQOUT, DISKO, CCNOUT, KLOCWT, DOUVCM
      COMMON /CHPARM/ INFILE, IN2FIL, NAMOUT, CLAOUT, XSOUR
      COMMON /ACCPRM/ ARGD, TSW, ACCBW, ACCFQL, ACCFQH, SWTIME,
     *   NSW, KSW, ASW, NSWPHR
      COMMON /ANTS/ ANTLOC, GST0, XIAT, XUT1, ANTOFF, ACBIAS, NANT,
     *   KKREFS, NACSCN, ANTSYM
      COMMON /SOUINF/ FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, PMRA, PMDEC, SRCFRQ, FRQOFF,
     *   FLUX, ISURNO, DATASB,
     *   GOTSOU,
     *   SUKOLS, SUNUMV, SUBUFF, IDSOU, IQUAL, CURSOU
      COMMON /SOUCHR/ SORNAM, CALCOD, ANTNAM, OBSDAT, REFNAM
      COMMON /CALINF/ CTIME,
     *   TIMEI, CALINT, CURINT, ICLRNO, FREQID,
     *   CLKOLS, CLNUMV, CLBUFF, NUMANT, NUMPOL, NUMIF, ANTNO, SUBA,
     *   ISBAND
      COMMON /FQINF/ IFFREQ, FRQDEL, CHBW, TOTBW, SIDBND, CURFQI,
     *   IFQRNO, FQBUFF, FQKOLS, FQNUMV
LOCAL END
LOCAL INCLUDE 'VLBBUF.INC'
C                                        Local buffer include
      INTEGER   JBUFSZ, IABUF(2048)
      REAL      BUFFER(UVBFSS), BUFF3(UVBFSS), A4BUF(2048)
      DOUBLE PRECISION A8BUF(1024)
      EQUIVALENCE (BUFFER, A8BUF, IABUF, A4BUF)
      COMMON /BUFRS/ BUFFER, BUFF3, JBUFSZ
LOCAL END
LOCAL INCLUDE 'DKIN.INC'
      INTEGER    NKSTNS,NKSRCS
      CHARACTER  KEYSTN(50)*8, KEYSRC(100)*8
      COMMON /KEYCHR/ KEYSTN, KEYSRC
      COMMON /KEYCOM/ NKSRCS, NKSTNS
LOCAL END
LOCAL INCLUDE 'DSET.INC'
      INTEGER    ITFLAG, JISOC, INSOC, NFRQS2, TSIDB
      INTEGER    NRPRM, NAXIS, NRATE, NDLY, NDEC, NRA, NFREQ,
     *           NSTOKE, NCMPLX, IPOLAR, CHNLO, CHNHI, NFOUT,
     *           LOCHN, HICHN, NCHAN, DAYN, REFDAY, IA, IB, ILAG,
     *           NLAG
      LOGICAL    AEQPA, XFORM, MACHDL, LFBS, FBSAMP, VLECK, LINED,
     *           NOAC, NOUVC, LAFBS, BUGGER, ACSWIT
      REAL       PHSSET, CP(32,32), SP(32,32), AVGX, AVGAC
      DOUBLE PRECISION
     *       OMEGA, AR(6), BR(6), ABR(6), SFACT, BXM, BYM, BZM, BM,
     *           SBXM, SBYM, SBZM, CBXM, CBYM, CBZM, UVCON, IATUT1,
     *           SIDER, CLIGHT, GAST0, PACLK, ACLK, BCLK,
     *           ADOP, BDOP, AFREQ, TSTART, TSTOP, TSRC(50)
      COMMON /CONST/  SIDER, CLIGHT
      COMMON /SETCON/ SFACT, BXM, BYM, BZM, BM, SBXM, SBYM, SBZM,
     *                CBXM, CBYM, CBZM, UVCON, GAST0, IATUT1,
     *                PACLK, ACLK, BCLK, ADOP, BDOP, AFREQ, AVGX,
     *                CP, SP, PHSSET, AVGAC, DAYN, REFDAY, NFRQS2,
     *                TSIDB, IA, IB
      COMMON /SETFBS/ OMEGA, AR, BR, ABR, AEQPA, ITFLAG, JISOC, INSOC
      COMMON /CONTRL/ TSRC, TSTART, TSTOP, LOCHN, HICHN, CHNLO, CHNHI,
     *                NFOUT, ILAG, NLAG, XFORM, MACHDL, LFBS, FBSAMP,
     *                VLECK, BUGGER, ACSWIT, LINED, NOAC, NOUVC, LAFBS,
     *                NCHAN
      COMMON /NRAND/  NRPRM, NAXIS, NRATE, NDLY, NDEC, NRA, NFREQ,
     *                NSTOKE, NCMPLX, IPOLAR
LOCAL END
LOCAL INCLUDE 'DREC.INC'
      INTEGER    TYPE, NREC, NHD, NDD, SCN1, SCN2, IND,
     *           SCNHDR(256), DATHDR(128)
      REAL       RECDAT(1024), SCN4(128), DAT4(64)
      DOUBLE PRECISION AT, SCN8(64), DAT8(32)
      COMMON /VLBREC/ AT, SCN8, DAT8, IND, TYPE, NREC, NHD, NDD, SCN1,
     *                SCN2, SCNHDR, DATHDR, RECDAT, SCN4, DAT4
LOCAL END
      PROGRAM VLBIN
C-----------------------------------------------------------------------
C! Task to read VLBI uv data from a local file in DECODE format.
C# VLB Spectral UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002, 2006, 2009, 2012, 2014-2015
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
C   VLBIN reads VLBI uv data in the NRAO DECODE format from a file on
C   the host computer. VLBIN may transform delay channels into frequency
C   channels for VBFIT, or pass VLBI spectral line records directly
C   into AIPS.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFILE        Input file name.
C      IN2FILE        IN2FIL        Name of ant. and source file
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is source name
C      OUTCLASS       CLAOUT        Class of the output uv file.
C                                   Default 'UVDATA'
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      SOURCES                      Source name(s)
C      BCHAN                        Begin channel (delay or freq)
C      ECHAN                        End channel, use 4 and 10 dly
C      BDROP                        Drop first BDROP and last
C      EDROP                        EDROP freq chans on output.
C      NPOINTS                      Max # 1000's of visibilities
C      DOUVCOMP                     1 (T) => compress output data
C      APARM                        APARM(1): =1 machine dly corr
C                                   APARM(2): =1 trans dly to frq
C                                   APARM(3): Num. pol. axis pix.
C                                   APARM(4): Ith pol. axis pix.
C                                   APARM(5): Reference day number
C                                      (first day of entire VLB run)
C                                   APARM(6): =1 FBS corrections.
C                                   APARM(7): =1 Do not recalc u,v,w
C                                   APARM(8): Cal table interval (min).
C                                   APARM(9): =1 van-vleck correction
C                                   APARM(10): additional corrections
C                                   APARM(11):=1 drop AC records
C                                   APARM(12):=FQ tolerance (kHz)
C      TIMERANG                     Timerange:DOY,H,M,S,DOY,H,M,S
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'VLBIN '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VLBINN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read data.
      CALL VLBGET (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL VLBHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE VLBINN (PRGN, IERR)
C-----------------------------------------------------------------------
C   VLBINN gets input parameters for VLBIN and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                4 => error creating output file.
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in VLBIN for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER  IERR
C
      INTEGER   JERR, IROUND, NPARM, LOOP, IND2, FIND, I, IK
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA IND2 /10/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      IERR = 0
      IND = 21
C                                       Get input parameters.
      NPARM = 187
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Hollerith -> char
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XIN2FI, IN2FIL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (12, 1, XCLAOU, CLAOUT)
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSOUT)
      DISKO = IROUND (XDISO)
      DOUVCM = XDOUV .GT. 0.0
C                                       Give message if compressed UV
      IF (DOUVCM) THEN
         MSGTXT = 'UV data will be written in compressed format'
         CALL MSGWRT (4)
         END IF
C                                       Ref. freq in catblk?
      CALL POPSRD ('R2D', KEYVAL, ARGD)
C                                       Zero out new catalog header
      CALL FILL (256,0,CATBLK)
C                                       Max number of vis.
      CATBLK(KIGCN) = 1200 * XNPTS
C                                       Set time interval for CAL
      IF (APARM(8).LE.0.0) APARM(8) = 1.0
      TIMEI  = APARM(8)*60.0D0/86400.0D0
C                                       True sideband?
      TSIDB = 0
      IF (APARM(16).NE.0.0) THEN
         IF (APARM(16).GT.0.0) TSIDB = 1
         IF (APARM(16).LT.0.0) TSIDB = -1
         WRITE (MSGTXT,1010) TSIDB
         CALL MSGWRT (6)
         END IF
C                                       Switch autocorrelations?
      ACSWIT = .TRUE.
      IF (APARM(17).GT.0.0) ACSWIT = .FALSE.
C                                       Switching cycles
C                                       Way to do this is TSW defines
C                                       the switching cycle time.
C                                       NSW defines number of switch
C                                       cycles.
C                                       ASW defines which cycle to
C                                       accept, if TSW = 30.0 & ASW = 3
C                                       we would take every 3rd 30sec
C                                       block
      TSW = APARM(18) / 86400.D0
      NSW = IROUND (APARM(19))
      ASW = IROUND (APARM(20))
      KSW = 1
      IF (ASW.GT.0) THEN
         IF (APARM(18).LT.1.0) THEN
            WRITE (MSGTXT,1040)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 999
            END IF
         NSWPHR = IROUND (3600/APARM(18))
         IF (NSW.LT.ASW) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 999
            END IF
         IF (NSW.LT.2) THEN
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            IERR = 1
            GO TO 999
            END IF
         WRITE (MSGTXT,1020) APARM(18)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1030) ASW, NSW
         CALL MSGWRT (6)
C                                       Set up switch time array
         SWTIME(1,1) = 0.0
         SWTIME(1,2) = TSW
         DO 50 I = 2, NSWPHR
            IF (SWTIME(I,2) .GT. (1.0/24.0)) GO TO 50
            SWTIME(I,1) = SWTIME(I-1,2)
            SWTIME(I,2) = SWTIME(I,1) + TSW
 50         CONTINUE
         IK = 1
         DO 60 I = 1, NSWPHR
            IF (IK.GT.NSW) IK = 1
            IF (IK.NE.ASW) THEN
               SWTIME(I,1) = 999.0
               SWTIME(I,2) = 999.0
               END IF
            IK = IK + 1
 60         CONTINUE
         END IF
C                                       Set up the control parameters
      CALL SETPRM (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open input files
      CALL FLOPEN (INFILE,IND,F,FIND,IERR)
      IF (IERR.NE.0) GO TO 999
C
      CALL FLOPEN (IN2FIL,IND2,T,FIND,IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read KEYIN text file
      CALL VLBKEY (IND2,FIND,IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTXCLS (IND2, FIND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Initialize SOURCE table
      DO 100 LOOP = 1,300
         GOTSOU(LOOP) = F
 100     CONTINUE
      GO TO 999
C                                       Error.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLBINN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('VLBINN: True sideband of data set to ',I2)
 1020 FORMAT ('Switch cycle = ',F5.2,' seconds')
 1030 FORMAT ('Selecting cycle ',I2,' from total of ',I2)
 1040 FORMAT ('VLBINN: SWITCH CYCLES < 1 SECOND NOT ALLOWED')
 1050 FORMAT ('VLBINN: NO. SWITCHES < # SELECTED - CHECK PARMS')
 1060 FORMAT ('VLBINN: NO POINT IN < 2 SWITCHES - CHECK PARMS')
      END
      SUBROUTINE VLBGET (IRET)
C-----------------------------------------------------------------------
C   VLBGET reads uv data one point at a time from the
C   routine VLBDAT and then writes the data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET, IPTRO, LUNO, IA1, IA2, NIOUT, INDO, LRECO, KBIND,
     *   NPIO, TYPE, NFREQ, NSTRT, NA1, NA2, I, LOOP, NUMVIS, XCOUNT,
     *   VISLIM, NCORR, LREFST
      REAL      DUM, BUFF1(3,1024), U, V, W, TIMEF, TEMP(20)
      LOGICAL   MK3
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNO /16/
C-----------------------------------------------------------------------
      NIOUT = 0
      NUMVIS = 0
C                                       Initialize output file. Load
C                                       CATBLK with axis dimensions.
      CALL VLBCRE (LUNO, INDO, KBIND, NPIO, IRET)
      IF (IRET.NE.0) GO TO 999
      IPTRO = KBIND
      IF (DOUVCM) IPTRO = 1
      LRECO = LREC
C                                       Max. number of vis.
      VISLIM = CATBLK(KIGCN)
      XCOUNT = 0
C                                       Loop thru input data file
 100  CONTINUE
         NUMVIS = NUMVIS + 1
         LOOP = 0
C                                      Call main routine
         CALL VLBDAT (NUMVIS, BUFFER(IPTRO+ILOCU),
     *      BUFFER(IPTRO+ILOCV), BUFFER(IPTRO+ILOCW),
     *      BUFFER(IPTRO+ILOCT), IA1, IA2, BUFFER(IPTRO+NRPARM),
     *      TYPE, NFREQ, MK3, LREFST, IRET)
C                                       Branch on his return
C                                       IRET=-1, end of data
C                                             0, normal return
C                                             1, fatal error
C                                             2, do not accept this
C                                                data point
         IF (IRET.EQ.1) THEN
C                                       Error (fatal)
            WRITE (MSGTXT,1130) IRET
            GO TO 990
            END IF
C                                       Refuse data
         IF (IRET.EQ.2) THEN
            IRET = 0
            GO TO 100
            END IF
C                                       Accept data
         IF (IRET.EQ.0) THEN
C                                       Special section for auto-
C                                       correlation data.
            IF (TYPE.EQ.3 .OR. TYPE.EQ.5) THEN
               U = BUFFER(IPTRO+ILOCU)
               V = BUFFER(IPTRO+ILOCV)
               W = BUFFER(IPTRO+ILOCW)
               TIMEF = BUFFER(IPTRO+ILOCT)
               DO 125 I = 1, NRPARM
                  TEMP(I) = BUFFER(IPTRO+I-1)
 125              CONTINUE
               NSTRT = IPTRO + NRPARM - 1
               NA1 = IA1
               NA2 = IA2
C                                       Store AC data
               DO 110 I = 1, NFREQ
                  BUFF1(1,I) = BUFFER(NSTRT+1)
                  BUFF1(2,I) = BUFFER(NSTRT+2)
                  BUFF1(3,I) = BUFFER(NSTRT+3)
                  NSTRT = NSTRT + 3
 110              CONTINUE
               END IF
C                                       Select needed AC spectrum
 105        IF (TYPE .EQ. 3 .OR. TYPE .EQ. 5) THEN
               LOOP = LOOP + 1
               IF (LOOP .EQ. 1) THEN
                  NSTRT = IPTRO + NRPARM - 1
                  DO 115 I = 1, NFREQ
                     BUFFER(NSTRT+1) = BUFF1(1,I)
                     BUFFER(NSTRT+2) = 0.0
                     BUFFER(NSTRT+3) = BUFF1(3,I)
                     NSTRT = NSTRT + 3
 115                 CONTINUE
                  IA1 = NA1
                  IA2 = NA1
               ELSE IF (LOOP .EQ. 2) THEN
                  NSTRT = IPTRO + NRPARM - 1
                  NUMVIS = NUMVIS + 1
                  DO 120 I = 1, NFREQ
                     BUFFER(NSTRT+1) = BUFF1(2,I)
                     BUFFER(NSTRT+2) = 0.0
                     BUFFER(NSTRT+3) = BUFF1(3,I)
                     NSTRT = NSTRT + 3
 120                 CONTINUE
                  IA1 = NA2
                  IA2 = NA2
                  END IF
               BUFFER(IPTRO+ILOCU) = U
               BUFFER(IPTRO+ILOCV) = V
               BUFFER(IPTRO+ILOCW) = W
               BUFFER(IPTRO+ILOCT) = TIMEF
               DO 135 I = 1, NRPARM
                  BUFFER(IPTRO+I-1) = TEMP(I)
 135              CONTINUE
               END IF
C                                       Got datum
            XCOUNT = XCOUNT + 1
C                                       Fill in baseline.
C                                       Encode ref. station as 1/1000
C                                       of baseline number (i.e. below
C                                       the subarray number)
            IF (ILOCB.GE.0) THEN
               BUFFER(IPTRO+ILOCB)  = IA2*256 + IA1 + (0.001*(LREFST-1))
            ELSE
               BUFFER(IPTRO+ILOCA1)  = IA2
               BUFFER(IPTRO+ILOCA2)  = IA1
               BUFFER(IPTRO+ILOCSA)  = 1.0
               END IF
C                                       Fill in source ID.
            BUFFER(IPTRO+ILOCSU) = IDSOU
C                                       Fill in FQ ID
            BUFFER(IPTRO+ILOCFQ) = CURFQI
C                                       Fill in nominal integration time
            BUFFER(IPTRO+ILOCIT) = CURINT
C                                       Message to user
            IF (MOD (XCOUNT,1000) .EQ. 0) THEN
               WRITE (MSGTXT,1140) XCOUNT
               CALL MSGWRT (6)
               END IF
C                                       See if full
            IF (XCOUNT.GT.VISLIM) THEN
               XCOUNT = XCOUNT - 1
               GO TO 200
               END IF
            IF (.NOT. DOUVCM) IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            IF (NIOUT.LT.NPIO) THEN
               IF (MK3) LOOP = 0
               IF (LOOP.EQ.0 .OR. LOOP.GE.2) GO TO 100
               IF (LOOP.LT.2) GO TO 105
               END IF
C                                       Write vis record.
            IF (DOUVCM) THEN
               CALL COPY ((NRPARM-2), BUFFER(IPTRO), BUFF3(KBIND))
               NCORR = LRECO-NRPARM
               CALL ZUVPAK (NCORR, BUFFER(IPTRO+NRPARM),
     *            BUFF3(KBIND+KLOCWT), BUFF3(KBIND+NRPARM))
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF3, NIOUT, KBIND,
     *            IRET)
               IPTRO = 1
            ELSE
               CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND,
     *            IRET)
               END IF
            NPIO = NIOUT
C                                       Check for end.
            IF (NIOUT.LE.0) GO TO 200
            IF (IRET.EQ.0) GO TO 160
            WRITE (MSGTXT,1150) IRET
            GO TO 990
 160        IF (.NOT. DOUVCM) IPTRO = KBIND
            NIOUT = 0
            IF (MK3) LOOP = 0
            IF (LOOP.EQ.0 .OR. LOOP.GE.2) GO TO 100
            IF (LOOP .LT. 2) GO TO 105
            END IF
C                                       Final call to VLBDAT.
 200     IF (IRET.EQ.-1) THEN
            NUMVIS = -1
            CALL VLBDAT (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, BUFFER,
     *         0, 0, MK3, LREFST, IRET)
            IF (IRET.LE.0) GO TO 205
            WRITE (MSGTXT,1130) IRET
            GO TO 990
            END IF
C                                       Close tables
 205  CALL TABIO ('CLOS', 0, I, CLBUFF, CLBUFF, IRET)
      CALL TABIO ('CLOS', 0, I, SUBUFF, SUBUFF, IRET)
C                                       Finish write
      NIOUT = - NIOUT
      IF (DOUVCM) THEN
         CALL UVDISK ('FLSH', LUNO, INDO, BUFF3, NIOUT, KBIND, IRET)
      ELSE
         CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
         END IF
      IF (IRET.EQ.0) GO TO 210
         WRITE (MSGTXT,1150) IRET
         GO TO 990
C                                       Compress output file.
 210  NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNOUT, LUNO, CATBLK, IRET)
      IF (NVIS.EQ.VISLIM) THEN
         WRITE (MSGTXT,1160) NVIS
         CALL MSGWRT (6)
      ELSE
         WRITE (MSGTXT,1170) NVIS
         CALL MSGWRT (6)
         END IF
C                                       Close file
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('VLBGET: VLBDAT ERROR',I3)
 1140 FORMAT ('VLBGET: Translated ',I8,' records')
 1150 FORMAT ('VLBGET: ERROR',I3,' WRITING VIS FILE')
 1160 FORMAT ('VLBGET: ',I8,' vis. written - there may be more')
 1170 FORMAT ('VLBGET: ',I8,' visibilities written')
      END
      SUBROUTINE VLBCRE (LUNO, INDO, KBIND, NPIO, IRET)
C-----------------------------------------------------------------------
C   VLBCRE fills in the output catalog header record, creates the
C   output file and initializes the I/O.
C    Input:
C     LUNO    I    LUN for I/O
C    Output:
C     INDO    I    FTAB pointer for I/O
C     KBIND   I    buffer pointer
C     NPIO    I    Number of vis per call
C     IRET    I    Return error code, 0=>OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   LUNO, INDO, KBIND, IRET, JERR, LUN
C
      CHARACTER  OLDNAM*12, DEFCLS*6, VELTYP*8, VELDEF*8, OFILE*48
      INTEGER    NPIO, NOGRP, BO, VO, SUVER, CLVER, SUFQID, NTERM
      REAL      GMMOD
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VO, BO /0, 1/
      DATA T /.TRUE./
      DATA VELTYP, VELDEF /'LSR','OPTICAL '/
C-----------------------------------------------------------------------
C                                       Call VLBHED to load random parm
C                                       and axis info into CATBLK.
      CALL VLBHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Get uv header info and
C                                       verify header structure.
      CALL UVPGET (JERR)
C                                       Put new values in CATBLK.
C                                       Get naming defaults
      OLDNAM = 'VLBIN'
      DEFCLS = 'UVDATA'
      CALL MAKOUT (OLDNAM, '      ', 0, DEFCLS, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
C                                       Image type ='UV'
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNOUT = 1
      CALL UVCREA (DISKO, CCNOUT, BUFFER, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Mark in /CFILES/
 40   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNOUT
      FRW(NCFILE) = 2
C                                       Get SEQ. no. used.
      SEQOUT = CATBLK(KIIMS)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNOUT, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, T, T, IRET)
      IF (IRET.LE.0) GO TO 60
         WRITE (MSGTXT,1040) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LREC = length of output rec.
 60   NPIO = 0
      IF (DOUVCM) THEN
         NPIO = 1
         CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, NPIO, JBUFSZ,
     *      BUFF3, BO, KBIND, IRET)
      ELSE
         CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, NPIO, JBUFSZ,
     *      BUFFER, BO, KBIND, IRET)
         END IF
      IF (IRET.EQ.0) GO TO 80
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 80   CONTINUE
      IRET = 0
      NUMIF = 1
C                                       Create SOURCE and CAL tables
C                                       SOURCE table
      LUN = 28
      NOGRP = NUMIF
      SUVER = 1
      SUFQID = -1
      CALL SOUINI ('WRIT', SUBUFF, DISKO, CCNOUT, SUVER, CATBLK, LUN,
     *   NOGRP, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       CAL table
      LUN = 29
      GMMOD = 1.0
      CLVER = 1
      NTERM = 3
      CALL CALINI ('WRIT', CLBUFF, DISKO, CCNOUT, CLVER, CATBLK, LUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM, GMMOD,
     *   IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1040 FORMAT ('ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('ERROR',I3,' INIT-FOR-WRITE VIS FILE')
      END
      SUBROUTINE VLBSOU (IRET)
C-----------------------------------------------------------------------
C   Routine to keep track of sources in the SOURCE file.
C   Puts source info into commons.
C    Input from COMMON
C      ISURNO        I     Next source record in table
C      SUBUFF        I     Buffer  etc for source file.
C      GOTSOU(300)   L     If true have written source info for source.
C      SORNAM        C     Source name packed. (16 char.)
C      qual                not yet implemented
C      calcode             not yet implemented
C      FREQ          D     Frequency
C      BANDW         D     Bandwidth
C      RAEPO         D     Right Ascension (deg) at mean epoch
C      DECEPO        D     Declination (deg) at mean epoch
C      EPOCH         D     Epoch
C      RAAPP         D     Right ascension (deg) apparent
C      DECAPP        D     Declination (deg) apparent
C    Output in common:
C      CURSOU        I     Current source ID number.
C      ISURNO        I     Next source record in table
C      NSOUR         I     Number of sources in source list
C      SULIST(300)   C    Packed names of sources in list
C      IDSOUR(300)   I     Ids of sources on list.
C    Output:
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PSOU*8
      REAL      CVEL, VELINC, NUX, VELFPX, ALTRFP
      DOUBLE PRECISION    FRQGHZ, RESTF, CHNSEP, FRQDIF
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
C                                       Get source info.
C                                       See if already have SOURCE
      IF (GOTSOU(IDSOU)) GO TO 999
C                                       New source
      GOTSOU(IDSOU) = .TRUE.
C                                       Send user a message.
      PSOU(1:8) = SORNAM(1:8)
      FRQGHZ = FREQO * 1.0D-9
      WRITE (MSGTXT,1021) PSOU, FRQGHZ
      CALL MSGWRT (6)
C                                       Store source ref. frq
      SRCFRQ(IDSOU) = FREQO
      IF (CATD(KDCRV+2) .NE. 1.0D0) THEN
         FRQDIF = SRCFRQ(IDSOU) - CATD(KDCRV+2)
         FRQOFF(IDSOU) = FRQDIF
         END IF
C                                       Determine line rest freq.
      CALL REST (FREQO, RESTF)
C                                       Determine velocity of ref.
C                                       pixel
      ALTRFP = NFREQ/2 + 1
      CVEL = SCN4(60)
      CVEL = CVEL * 1.0D3
      CHNSEP = BANDW / NFREQ
      NUX = FREQO + CHNSEP * (ALTRFP - 1.0)
      VELINC = - (CHNSEP * (VELITE + CVEL)) / NUX
      VELFPX = CVEL + (VELINC * (1.0 - ALTRFP))
      LSRVEL(1) = VELFPX
      IQUAL  = 1
      CURSOU = IDSOU
      CALCOD = '    '
      SORNAM(9:16) = ' '
C
      CALL TABSOU ('WRIT', SUBUFF, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *   SORNAM,
     *   IQUAL, CALCOD, FLUX, FRQDIF, CHNSEP, RAEPO, DECEPO, EPOCH,
     *   RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTF, PMRA, PMDEC, IRET)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1021 FORMAT ('Found ',A8,' observed at ',F10.5,' GHz')
      END
      SUBROUTINE VLBANT
C-----------------------------------------------------------------------
C   VLBANT creates and fills the antenna file.
C-----------------------------------------------------------------------
      CHARACTER NULL*1
      INTEGER   IERR, VER, LUN, I, MXANT, NIF
      DOUBLE PRECISION JD, GMSTM, GASTM, GRATE
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA MXANT /MAXANT/
C-----------------------------------------------------------------------
C                                       Make sure there is antenna info
      IF (NANT.LE.0) GO TO 999
         NIF = 1
         IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
C                                       Setup for AN table initization
         NULL = CHAR(0)
         NUMORB = 0
         NOPCAL = 2
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
C                                       Array name
         ANAME = 'VLBI'
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) =  0.0D0
         ARRAYC(3) =  0.0D0
C                                       Get GST0 and Earth rotation rate
         DATUTC = XIAT
         UT1UTC = XUT1
         TIMSYS = 'UTC'
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         IF (ABS (GST0).LT.1.0E-20)
     *      CALL GSTROT (JD, GMSTM, GASTM, GRATE)
         GSTIA0 = GST0
         IF (ABS (GST0).LT.1.0E-20) GSTIA0 = GMSTM
         DEGPDY = (SIDER + 0.589D-10*(JD-2415020D0)) * 360.0D0
         SAFREQ = FREQO
         ANFQID = -1
         ANTNIF = NIF
         VER = 1
         XYZHAN = ' '
         TFRAME = ' '
C                                       Create/init file
         CALL ANTINI ('WRIT', BUFFER, DISKO, CCNOUT, VER, CATBLK, LUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = ' '
         STAXOF = 0.0
         STAXYZ(1) = DBLANK
         STAXYZ(2) = DBLANK
         STAXYZ(3) = DBLANK
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 0.0
         CALL RFILL (NIF, 0.0, FWHMAN)
C                                       AN records
         DO 20 I = 1,MXANT
            IF ((ANTNAM(I).NE.' ') .AND. (ANTNAM(I)(:1).NE.NULL)) THEN
               STAXYZ(1) = ANTLOC(1,I)
               STAXYZ(2) = ANTLOC(2,I)
               STAXYZ(3) = ANTLOC(3,I)
               STAXOF    = ANTOFF(I)
               NOSTA = I
               ANNAME = ANTNAM(I)
               CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IERR)
               IF (IERR.NE.0) GO TO 990
               END IF
 20         CONTINUE
C                                       Fill in header and close
         CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 990
         GO TO 999
C                                       Error
 990  WRITE (MSGTXT,1020) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('ERROR ',I3,' OCCURED WRITING ANTENNA FILE')
      END
      SUBROUTINE VLBHED (IRET)
C-----------------------------------------------------------------------
C   VLBHED is a routine in which the catalog header is constructed.
C   NOTE: the AIPS convention for the coordinate reference value
C   for the STOKES axis is that 1,2,3,4 represent I, Q, U, V
C   stokes' parameters and -1,-2,-3,-4 represent RR, LL, RL and
C   LR correlator values.  Currently set for R and L polarization
C   ie Ref. value = -1 and increment = -1.
C
C   The MINIMUM information required here is that
C   required to define the size of the output file; ie.
C      CATBLK(KIGCN)   = I   number of visibility records
C      CATBLK(KIPCN) = Number of random parameters.
C      CATBLK(KIDIM)= the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis.
C   Other changes can be made either here or in VLBDAT; the
C   catalog block will be updated when the history file is
C   written.
C      The antenna information can also be entered in this
C   routine.  It is possible to put much more information in the
C   ANtenna file, see the AIPS manual vol. 2 for details.
C
C    Input:
C     CATBLK(256)    I     Output catalog header, also CATR, CATD
C                          The OUTNAME, OUTCLASS, OUTSEQ are entered
C                          elsewhere.
C    Output:
C     CATBLK(256)    I     Modified output catalog header.
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C    Also the antenna informtion can be filled into a common.
C-----------------------------------------------------------------------
      CHARACTER RTYPES(12)*8, TYPES(7)*8, UNITS*8, TELE*8, INSTR*8
      INTEGER   I, NDIM(7), INDEX, INC, IRET
      LOGICAL   FIRST
      REAL      CRPIX(7), CRINC(7)
      DOUBLE PRECISION    CRVAL(7)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA FIRST/.TRUE./
C                                       User definable values
C                                       Random parameters.
C                                         Rand. parm. names.
      DATA RTYPES /'UU-L    ','VV-L    ','WW-L    ',
     *   'TIME1   ','SUBARRAY', 'SOURCE  ', 'FREQSEL ',
     *   'INTTIM  ','ANTENNA1','ANTENNA2','WEIGHT  ','SCALE   '/
C                                       Uniform axes.
C                                         Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ', 'RA      ','DEC     ',' '/
C                                         Axis dimensions
      DATA NDIM /3,1,4,1,1,1,0/
C                                         Reference values
      DATA CRVAL /1.0D0, -1.0D0, 0.0D0, 1.0D0, 3*0.0D0/
C                                         Reference pixel.
      DATA CRPIX /7*1.0/
C                                         Coordinate increment.
      DATA CRINC /1.0, -1.0, 0.0, 1.0, 0.0, 2*0.0/
C                                       Units
      DATA UNITS /'CORREL'/
C                                       Telescope/instuument
      DATA TELE, INSTR /'VLBI    ', 'NRAOMKII'/
C-----------------------------------------------------------------------
      IRET = 0
C                                       No. random parameters.
      NRPRM = 10
C                                       No. axes
      NAXIS = 6
      IF (.NOT.FIRST) GO TO 200
C                                       Fill axis arrays.
      INC = 2
C                                       Random axis names
      IF (DOUVCM) NRPRM = NRPRM + 2
      KLOCWT = 8
      DO 10 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * INC
         IF (I.LE.NRPRM) THEN
            CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
         ELSE
            CALL CHR2H (8, '        ', 1, CATH(INDEX))
            END IF
 10      CONTINUE
C                                       Uniform axes
      DO 30 I = 1,KICTPN
C                                       Init dimension
         CATBLK(KINAX+I-1) = NDIM(I)
C                                       Init. increment.
         CATR(KRCIC+I-1) = CRINC(I)
C                                       Init. rotation.
         CATR(KRCRT+I-1) = 0.0
C                                       Init. ref pixel.
         CATR(KRCRP+I-1) = CRPIX(I)
C                                       Init. ref value.
         CATD(KDCRV+I-1) = CRVAL(I)
C                                       Fill axis type from
C                                       TYPES
         INDEX = KHCTP + (I-1) * INC
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C
      CATBLK(KINAX+2) = NFREQ
C                                       Compressed ?
      IF (DOUVCM) CATBLK(KINAX) = 1
C
      LREC = NRPRM  + CATBLK(KINAX ) * CATBLK(KINAX+1)
     *              * CATBLK(KINAX+2) * CATBLK(KINAX+3)
     *              * CATBLK(KINAX+4) * CATBLK(KINAX+5)
     *              * CATBLK(KINAX+6)
C                                       Fill in values.
C                                       Fill other character strings.
C                                       Observation date.
      OBSDAT = ' '
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRPRM
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 1950.0
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
      CATBLK(KINIT) = 0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Shift
      CATR(KRXSH) = 0.0
      CATR(KRYSH) = 0.0
C                                       "Old" (observed) position.
      CATD(KDORA) = 0.0D0
      CATD(KDODE) = 0.0D0
C                                       Rest Frequency
      CATD(KDRST) = 0.0D0
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('**'=>unsorted)
      CALL CHR2H (2, '**', 1, CATH(KITYP))
C                                       No magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       OBSDAT = Reference date of time
C                                          tags for data as "dd/mm/yy"
C                                       SOURCE = source name (8 char)
C                                       TELE = telescope name (8 char)
C                                       INSTR = Receiver name (8char)
C                                       OBSR = Observers name (8 char)
C                                       Observing date.
      CALL CHR2H (8, OBSDAT, 1, CATH(KHDOB))
C                                       Telescope.
      CALL CHR2H (8, TELE, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, INSTR, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, TELE, 1, CATH(KHOBS))
      FIRST = .FALSE.
      GO TO 999
C-----------------------------------------------------------------------
C                                       Enter values for data:
C                                       RA = Right ascension (1950)
C                                          in degrees.
C                                       DEC = Declination in degrees.
C                                       FREQ = frequency of obs in Hz.
C                                       BANDW = bandwidth or channel
C                                           separation.
C                                       NCHAN = Number of freq chan.
C                                       NPOLN = number of polarization
C                                            correlators.
C                                       Just in case you forget.
C                                       Insert values in header.
 200  CONTINUE
C                                       Object.
      CALL CHR2H (8, 'MULTI   ', 1, CATR(KHOBJ))
C                                       Position.
      CATD(KDCRV+4) = 0.D0
      CATD(KDCRV+5) = 0.D0
C                                       Frequency
      CATD(KDCRV+2) = FREQO
      IF (ARGD.GT.0.D0) THEN
         CATD(KDCRV+2) = ARGD
         END IF
C                                       Bandwidth.
      CATR(KRCIC+2) = BANDW / NFREQ
      IF (DATASB.LT.0) THEN
         CATR(KRCRP+2) = NFREQ
         END IF
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NFREQ
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NSTOKE
C                                       Finished.
      IRET = 0
      GO TO 999
C
 999  RETURN
      END
      SUBROUTINE VLBDAT (NUMVIS, U, V, W, T, IA1, IA2, VIS, DECTYP,
     *   KFREQ, MK3, LREFST, IRET)
C-----------------------------------------------------------------------
C
C  This is a skeleton version of subroutine VLBDAT which allows the
C  user to create a UV data base.  Visibilities are returned one at
C  a time and are written on the output file.
C
C       If IRET .GT. 0 then the output file will be destroyed.
C       A value of IRET .lt. 0 indicates the end of the data.
C
C  Inputs:
C  NUMVIS     I    Visibility number, -1 => final call, no data
C                  passed but allows any operations to be completed.
C
C  Inputs from COMMON
C  IN2FIL(12) R    Name of the aux. file (48 char)
C  APARM(10)  R    User array.
C  RA         D    Right ascension (1950) of phase center. (deg)
C  DEC        D    Declination (1950) of phase center. (deg)
C  FREQ       D    Frequency of observation (Hz)
C  NRPARM     I    # random parameters.
C  NCOR       I    # correlators
C  CATBLK(256)I    Catalog header record. See [DOC]HEADER for details
C
C  Output:
C  U          R    U in wavelengths at the reference frequency.
C  V          R    V in wavelengths
C  W          R    W in wavelengths
C  T          R    Time in days since the midnight at the start of
C                  the reference date.
C  IA1        I    Antenna number of the first antenna.
C  IA2        I    Antenna number of the second antenna.
C  VIS(3,*)   R    Visibilities.  The first dimension is the COMPLEX
C                  axis in the order Real part, Imaginary part, weight.
C                  The order of the following visibilities is defined
C                  by variables in COMMOM /UVHDR/ (originally
C                  specified in NEWHDR).  The order number for Stokes
C                  parameters is JLOCS and the order number for
C                  frequency is given by JLOCF.  The lower order number
C                  increases faster in the array.
C                  See precursor comments in UVPGET for more details.
C  DECTYP     I    The data type found in the DECODE format data
C  KFREQ      I    No. of frequency channels to be written.
C  MK3        L    Logical flag specifying if input data comes from a
C                  MkIII correlator, this is used for AC data since the
C                  format is slightly different in that only one
C                  station's AC spectrum(/function) is held in each vis
C                  record, as opposed to both stations for MkII data.
C  LREFST     I    Reference station in the correlator for this baseline
C  IRET       I    Return code  -1 => End of data.
C                                0 => OK
C                               >0 => error, terminate.
C
C  Output in COMMON
C  CATBLK     I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   IA1, IA2, IRET, K, FBSDN, DBRDN, FFTDN, DECTYP, KFREQ,
     *   LREFST
      LOGICAL   FIRST, LSEL, NEWSRC, MK3
      INTEGER   NUMVIS
      REAL      U, V, W, T, VIS(3,1024)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      MK3 = .FALSE.
      IRET = 0
      IF (NUMVIS.EQ.-1) GO TO 999
C                                       Read the next pair of VLBI recs.
 10   CALL VLBIO (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Test to see if we want this rec.
      CALL SELECT (LSEL,IRET)
      IF (IRET.NE.0) GO TO 999
      IF (.NOT.LSEL) GO TO 10
      IF (TYPE .EQ. 8) GO TO 10
C                                       Did we just read a data record ?
      IF (TYPE.EQ.2.OR.TYPE.EQ.7.OR.TYPE.EQ.3.OR.
     *    TYPE.EQ.5) GO TO 100
C                                       Scan header records ?
C                                       FIRST = true for first scan in
C                                       input file.
      IF (TYPE.NE.1) GO TO 10
      IF (.NOT.FIRST) GO TO 50
C
C                                       This is the first scan in file
C                                       History cards never used
C                                       Initialize average bias arrays
         DO 220 K = 1, MAXANT
            ACBIAS(K) = 0.0
            NACSCN(K) = 0
 220        CONTINUE
C                                       Check that the channel config.
C                                       specified by the user is
C                                       consistant with the data recs.
C                                       Trap mismatch errors here.
         NCHAN = ABS(SCNHDR(4))
         IF (LOCHN.LE.NCHAN.AND.HICHN.LE.NCHAN) GO TO 30
            WRITE (MSGTXT,1030) LOCHN,HICHN,NCHAN
            CALL MSGWRT (8)
            IRET = 1
            GO TO 999
 30      CONTINUE
C                                       Check to see whether FBS
C                                       correction has been done.
         FFTDN = SCNHDR(144)
         FBSDN = SCNHDR(145)
         DBRDN = SCNHDR(146)
         IF (LFBS .OR. LAFBS) THEN
C                                       Amp & phase FBS done ?
            IF (FBSDN.EQ.1 .AND. FFTDN.EQ.1 .AND. DBRDN.EQ.1) THEN
               WRITE (MSGTXT,1040)
               CALL MSGWRT (8)
               LFBS = .FALSE.
               LAFBS = .FALSE.
               END IF
C                                       Only Amp FBS done ?
            IF (FBSDN.EQ.1 .AND. FFTDN.EQ.0 .AND. DBRDN.EQ.0) THEN
               WRITE (MSGTXT,1050)
               CALL MSGWRT (8)
               LFBS = .TRUE.
               FBSAMP = .FALSE.
               LAFBS = .FALSE.
               END IF
            END IF
C                                       Trap for type 7 data with no
C                                       transform specified.
         IF (TYPE.EQ.7 .AND. (.NOT.XFORM)) THEN
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            IRET = 2
            END IF
C        *****  MORE TRAPS HERE *********
C
 50   CONTINUE
C                                       Load station info into the
C                                       AN table COMMON (CVLB.INC).
C                                       The AN table is written at end
C                                       of task in call to VLBHIS.
         CALL SETSTN (IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       Load source info into the SOUINF
C                                       COMMON.
         CALL SETSRC (NEWSRC,IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       Second call to VLBHED. First
C                                       call is in VLBCRE. Setup CATBLK
C                                       values using VLB scan header
C                                       record.
         IF (FIRST) CALL VLBHED (IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       Write new entry in source table.
         IF (NEWSRC) CALL VLBSOU (IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       Determine FQ number
         CALL VLBNFQ (IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Set up multipliers for the DFT,
C                                       u,v,w calcs and the FBS correct.

         CALL SETDFT (IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       Calculate and write entries into
C                                       CAL table. Write CAL records for

C                                       time range encompassed by the
C                                       current scan.
         CALL SETCAL (IRET)
         IF (IRET.EQ.1) GO TO 999
C                                       if .not. first scan then
C                                       print out the acbias info.
         IF (.NOT.FIRST) THEN
            DO 230 K = 1, NANT
               IF (NACSCN(K).GT.0) THEN
                  ACBIAS(K) = ACBIAS(K) / NACSCN(K)
                  WRITE (MSGTXT,1070) K, ACBIAS(K)
                  CALL MSGWRT (2)
                  END IF
               ACBIAS(K) = 0.0
               NACSCN(K) = 0
 230           CONTINUE
            END IF
         FIRST = .FALSE.
         GO TO 10
C
C                                       The VLB record read must be a
C                                       data record. Process it and
C                                       return.
 100   IF (TYPE.NE.2.AND.TYPE.NE.7.AND.TYPE.NE.3
     *     .AND.TYPE.NE.5) GO TO 10
C                                       Reject AC data ?
       IF (TYPE.EQ.3 .OR. TYPE.EQ.5) THEN
          IF (NOAC) GO TO 10
          END IF
       DECTYP = TYPE
       KFREQ = NFREQ
       CALL PROCES (U, V, W, T, IA1, IA2, VIS, LREFST, IRET)
       MK3 = IA1 .EQ. IA2
       IF (IRET.EQ.1) GO TO 999
C
       NUMVIS = NUMVIS + 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('ILLEGAL BCHAN, ECHAN = ',I4,',',I4,' NCHAN = ',I5)
 1040 FORMAT ('VLBDAT: FBS already done will ignore APARM(6)')
 1050 FORMAT ('VLBDAT: FBS amp corr. done - will just do phase corr')
 1060 FORMAT ('VLBDAT: CROSS-CORR FUNCTIONS NOT ALLOWED - USE XFORM')
 1070 FORMAT ('Antenna ',I2,', avg. clipper bias = ',F8.4)
      END
      SUBROUTINE VLBHIS
C-----------------------------------------------------------------------
C   VLBHIS creates and fills a history file and creates and fills
C   the ANtenna file if any given.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, TELE*8, OBSR*8, HILINE*72
      INTEGER   LUN, IERR, DATE(3), TIME(3)
      REAL      TIME1, TIME2
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, CCNOUT, CATBLK, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       New history
      WRITE (HILINE,1011) TSKNAM, INFILE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
      WRITE (HILINE,1012) TSKNAM, IN2FIL
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Number of visibilities
      WRITE (HILINE,2001) TSKNAM, CATBLK(KIGCN)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Telescope, observer name.
      CALL H2CHR (8, 1, CATH(KHTEL), TELE)
      CALL H2CHR (8, 1, CATH(KHOBS), OBSR)
      WRITE (HILINE,2002) TSKNAM, TELE, OBSR
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       AIPS release
      WRITE (HILINE,2003) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Machine delay
      IF (MACHDL) THEN
         WRITE (HILINE,2004) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Fourier Transformed
      IF (XFORM) THEN
         IF (LINED) THEN
            WRITE (HILINE,2005) TSKNAM
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
            END IF
         IF (.NOT. LINED) THEN
            WRITE (HILINE,2006) TSKNAM
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 20
            END IF
         END IF
C                                      Fractional bit shift
      IF (LFBS) THEN
         WRITE (HILINE,2007) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
      IF (LAFBS) THEN
         WRITE (HILINE,2016) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Van-Vleck correction
      IF (VLECK) THEN
         WRITE (HILINE,2008) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      AC data passed ?
      IF (NOAC) THEN
         WRITE (HILINE,2009) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Reference day
      WRITE (HILINE,2010) TSKNAM,REFDAY
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Cal table interval
      WRITE (HILINE,2011) TSKNAM,APARM(8)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Time range
      TIME1 = TSTART / TWOPI
      TIME2 = 1.0E5
      IF (ABS(TSTOP-366.0D0*TWOPI).LT.1.0D0) TIME2 = TSTOP / TWOPI
      CALL HITIME (TIME1, TIME2, LUN, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Number freq channels
      WRITE (HILINE,2014) TSKNAM,NFREQ
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      U & V calculation
      IF (NOUVC) THEN
         WRITE (HILINE,2015) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      FQ tolerance
      WRITE (HILINE,2017) TSKNAM, APARM(12)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Quantization corrections
      IF (BUGGER) THEN
         WRITE (HILINE,2018) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Autocorrelation switch?
      IF (ACSWIT) THEN
         WRITE (HILINE,2019) TSKNAM
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                      Switch cycles
      IF (ASW.GT.0) THEN
         WRITE (HILINE,2020) TSKNAM, APARM(18)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         WRITE (HILINE,2021) TSKNAM, ASW, NSW
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF


C                                       Close HI file
 20   CALL HICLOS (LUN, T, BUFFER, IERR)
C                                       Write ANtenna file.
      CALL VLBANT
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CCNOUT, CATBLK, 'REST', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLBHIS: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,' Infile =',A48)
 1012 FORMAT (A6,' In2file=',A48)
 2000 FORMAT (A6,' Source = ',A8)
 2001 FORMAT (A6,' / Number of visibilities copied=',I9)
 2002 FORMAT (A6,' / Telescope = ',A8,' Observer = ',A8)
 2003 FORMAT (A6,' Release = ''',A7,' ''')
 2004 FORMAT (A6,' Machine delay corrections done')
 2005 FORMAT (A6,' Data transformed with an FFT')
 2006 FORMAT (A6,' Data transformed with a DFT')
 2007 FORMAT (A6,' Fractional bit shift correction applied')
 2008 FORMAT (A6,' Van-Vleck clipping correction applied')
 2009 FORMAT (A6,' No autocorrelation data accepted')
 2010 FORMAT (A6,' Ref. day = ',I4,'/ First day in data?')
 2011 FORMAT (A6,' Cal table entry interval = ',F4.0,'/ mins')
 2014 FORMAT (A6,' No. freq channels = ',I4)
 2015 FORMAT (A6,' U and V not recalculated by VLBIN')
 2016 FORMAT (A6,' Amp portion of fbs correction only')
 2017 FORMAT (A6,' APARM(11) =', F10.3,' / FQ entry tolerance')
 2018 FORMAT (A6,' Fringe-rotator and factor of 2 (0.64) applied to ',
     *   'XC data')
 2019 FORMAT (A6,' Autocorrelation arrays switched')
 2020 FORMAT (A6,' Switch cycle = ',F5.2,' seconds')
 2021 FORMAT (A6,' Selecting cycle ',I2,' from total of ',I2)
      END
      SUBROUTINE VLBKEY (IUNIT, FIND, IRET)
C-----------------------------------------------------------------------
C     VLBKEY calls KEYIN which reads a native text file. VLBKEY is
C     called from VLBIN.
C
C-----------------------------------------------------------------------
      INTEGER   IUNIT, FIND, IRET
C
      CHARACTER PARS(150)*8, VALCH(150)*8, ENDMRK*8, NULL*1
      INTEGER   KMODE, K, I, NSTRT, NSTOP, NPARS
      DOUBLE PRECISION VALS(150)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
C
      DATA    PARS/50*'STAT????',100*'SOURCE??'/
      DATA    VALS/150*-1.0D0/
      DATA    ENDMRK/'/'/, NPARS/150/
C-----------------------------------------------------------------------
      NULL = CHAR(0)
C
      KMODE = 0
      CALL KEYIN (PARS, VALS, VALCH, NPARS, ENDMRK, KMODE, IUNIT, FIND,
     *   IRET)
      IF (IRET.EQ.0) GO TO 10
         WRITE (MSGTXT,1000)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
C
 10   NKSTNS = 0
      DO 100 I = 1, 50
         IF ((VALCH(I).EQ.' ') .OR. (VALCH(I)(1:1).EQ.NULL)) GO TO 100
            NKSTNS = NKSTNS + 1
            KEYSTN(NKSTNS) = VALCH(I)
C                                       Initialize antenna common info
            ANTNAM(NKSTNS) = VALCH(I)
            ANTLOC(1,NKSTNS) = DBLANK
            ANTLOC(2,NKSTNS) = DBLANK
            ANTLOC(3,NKSTNS) = DBLANK
 100     CONTINUE
C
      NUMANT = NKSTNS
      IF (NKSTNS.GT.0) GO TO 150
         WRITE (MSGTXT,1100)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
C
 150  NKSRCS = 0
      DO 200 I = 51, 150
         IF ((VALCH(I).EQ.' ') .OR. (VALCH(I)(1:1).EQ.NULL)) GO TO 200
            NKSRCS = NKSRCS + 1
            KEYSRC(NKSRCS) = VALCH(I)
 200     CONTINUE
C
      IF (NKSRCS.GT.0) GO TO 250
         WRITE (MSGTXT,1200)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
C
 250  NSTRT = -4
 300  NSTRT = NSTRT + 5
      NSTOP = NSTRT + 4
      WRITE (MSGTXT,1300) (K,KEYSTN(K),K=NSTRT,NSTOP)
      CALL MSGWRT (8)
      IF (NSTOP.LT.NKSTNS) GO TO 300
C
      NSTRT = -4
 400  NSTRT = NSTRT + 5
      NSTOP = NSTRT + 4
      WRITE (MSGTXT,1300) (K,KEYSRC(K),K=NSTRT,NSTOP)
      CALL MSGWRT (8)
      IF (NSTOP.LT.NKSRCS) GO TO 400
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('KEYIN: ERROR READING IN2FILE TEXT FILE')
 1100 FORMAT ('ERROR: NO STATIONS FOUND IN IN2FILE')
 1200 FORMAT ('ERROR: NO SOURCES FOUND IN IN2FILE')
 1300 FORMAT (5(I2,':',A8))
      END
      SUBROUTINE VLBIO (IRET)
C-----------------------------------------------------------------------
C  VLBIO reads one VLB NRAO/SAO format record and returns the contents
C  in the VLBREC Common. VLBIO uses unformatted Fortran READ's.
C
C   Input:
C    IND      I    Input data file unit number (Fortran I/O).
C
C   Output:
C    IRET     I   Return code  -2 => end of data.
C                               0 => OK
C                               1 => non-recoverable error
C
C-----------------------------------------------------------------------
      INTEGER  IRET
C
      INTEGER  MHD, MDD, MDH, IERR
      LOGICAL  EEOF, SKIP
      INTEGER  I4NHD, I4NDD, NHD2, ARRSML(1024)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
C
      DATA MHD, MDH, MDD/256, 128, 1024/
C-----------------------------------------------------------------------
C                                       Read VLB pre-records here
 100  CONTINUE
      CALL PREREC (IND, IERR, TYPE, NREC, NHD, NDD, AT, SCN1, SCN2,
     *             MHD, MDH, MDD, EEOF, SKIP)
      IF (IERR.EQ.-2) GO TO 100
      IF (IERR.EQ.+1) GO TO 999
      IF (EEOF)       GO TO 999
C
      I4NHD = NHD
      I4NDD = NDD
C                                       Data record, go to 200
      IF(TYPE.NE.1.AND.TYPE.NE.10) GO TO 200
C
C                                       Read scan header record
      NHD2 = NHD / 2
      CALL READH (IND, NHD, SCNHDR, NHD2, ARRSML, SCN8, SCN4, EEOF,
     *   IERR)
      IF (EEOF)        GO TO 999
      IF (IERR.EQ.-2)  GO TO 100
C                                       Reset sideband
      IF (TSIDB.NE.0) THEN
         SCNHDR(131) = TSIDB
         END IF
      GO TO 999
C
C                                       Read a VLB data record here
 200  CONTINUE
      NHD2 = NHD / 2
      CALL RFILL (1024, 0.0, RECDAT)
      CALL READD (IND, NHD, DATHDR, NHD2, ARRSML, DAT8, DAT4,
     *   NDD, RECDAT, EEOF, IERR)
      IF (EEOF)        GO TO 999
      IF (IERR.EQ.-2)  GO TO 100
C
 999  IRET = IERR
      IF (EEOF) IRET = -2
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE FLOPEN (FNAME, LUN, FORM, FIND, IRET)
C-----------------------------------------------------------------------
C     FLOPEN opens native input files for VLBIN.
C     Inputs : FNAME(12) R    File name to open
C              LUN       I    Logical unit of file
C              FORM      L    If true, formatted data file.
C     Output : FIND      I    Index in FTAB for LUN
C              IRET      I    Return error
C
C-----------------------------------------------------------------------
      CHARACTER FNAME*48
      INTEGER   LUN, FIND, IRET
      LOGICAL   FORM
C
      INTEGER    JERR
      LOGICAL    MAP
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA MAP /.FALSE./
C
C-----------------------------------------------------------------------
C
      IRET = 0
C                                       Determine full filename
C                                       and open it
      CALL VLBNAM (LUN, FNAME, FORM, IRET)
      IF (IRET .NE. 0) THEN
         WRITE (MSGTXT,2001) IRET, FNAME
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Open text file in FTAB
      IF (FORM) THEN
C                                       Valid LUN?
         IF ((LUN.GT.0) .AND. (LUN.LE.50)) GO TO 10
            WRITE (MSGTXT,1000) LUN
            CALL MSGWRT (6)
            GO TO 999
C                                       Valid device type?
 10      CONTINUE
         IF (DEVTAB(LUN).EQ.3) GO TO 20
            WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
            CALL MSGWRT (6)
            GO TO 999
C                                       Allocate area in FTAB for LUN.
 20      CALL LSERCH ('OPEN', LUN, FIND, MAP, JERR)
         IF (JERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) JERR, 'OPEN', LUN
            CALL MSGWRT (6)
            IF (JERR.NE.2) GO TO 23
C                                       LUN already in use.
               IRET = 1
               WRITE (MSGTXT,1022) LUN
               CALL MSGWRT (6)
               GO TO 999
 23         CONTINUE
            IF (JERR.NE.3) GO TO 999
C                                       No room in FTAB for LUN.
               IRET = 5
               WRITE (MSGTXT,1023) LUN
               CALL MSGWRT (6)
               GO TO 999
         END IF
C                                       Inform user
 30   WRITE (MSGTXT,2002) FNAME
      CALL MSGWRT(8)
      GO TO 999
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLOPEN: INVALID LUN = ',I2)
 1010 FORMAT ('FLOPEN: INVALID DEVTAB(',I2,') = ',I1)
 1020 FORMAT ('FLOPEN: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2)
 1022 FORMAT ('FLOPEN: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1023 FORMAT ('FLOPEN: NO ROOM IN FTAB FOR LUN = ',I2)
 2001 FORMAT ('FLOPEN: VLBNAM ERROR ',I3,' ON FILE ',A24)
 2002 FORMAT ('Opened file : ',A48)
C-----------------------------------------------------------------------
      END
      SUBROUTINE SETSRC (NEWSRC,IRET)
C-----------------------------------------------------------------------
C  SETSRC is called each time a VLB scan header record is read in
C  subroutine VLBDAT. SETSRC down loads source information from the
C  VLB header and passes it to VLBSOU through CVLB.INC. SETSRC is
C  used in AIPS task VLBIN.
C
C  Outputs:  NEWSRC  L    Set true when a new source is found.
C            IRET    I    0, => normal
C                         1, => fatal error.
C
C ------------------------------------------------------------------
      LOGICAL   NEWSRC
      INTEGER   IRET
C
      CHARACTER CSRC*8, SOURCS(300)*8
      INTEGER   NSRCS, I
      REAL      IBAND
      DOUBLE PRECISION    IFREQO, IFREQA, IRAEPO, IDECEP, IRAAPP,
     *   IDECAP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA      NSRCS/0/
C-----------------------------------------------------------------------
C
      IRET = 0
      NEWSRC = .TRUE.
      CALL H2CHR (8, 1, SCN8(3), CSRC)
C
      IF (NSRCS.EQ.0) GO TO 100
      DO 20 I = 1, NSRCS
         IF (CSRC.EQ.SOURCS(I)) NEWSRC = .FALSE.
 20      CONTINUE
C
 100  IDSOU = 0
      DO 200 I = 1, NKSRCS
         IF (CSRC.EQ.KEYSRC(I)) IDSOU = I
 200     CONTINUE
      IF (IDSOU.EQ.0) GO TO 999
C
      SORNAM = CSRC
      IRAEPO = SCN8(4)
      IDECEP = SCN8(5)
      IRAAPP = SCN8(6)
      IDECAP = SCN8(7)
      IBAND  = SCN4(65)
      DATASB = SCNHDR(131)
      IFREQO = SCN8(43)
      IFREQA = SCN8(15)
      IF (IFREQO.GT.0.0D0) FREQO = IFREQO * 1.0D06
      IF (IFREQO.LE.0.0D0) FREQO = IFREQA * 1.0D06
      BANDW  = IBAND * 1.0D03
      RAEPO  = IRAEPO * 360.0D0 / TWOPI
      DECEPO = IDECEP * 360.0D0 / TWOPI
      RAOBS = RAEPO
      DECOBS = DECEPO
      EPOCH  = 1950.0
      RAAPP  = IRAAPP * 360.0D0 / TWOPI
      DECAPP = IDECAP * 360.0D0 / TWOPI
CCC      NEWSRC = .TRUE.
      IF (NEWSRC) THEN
         NSRCS  = NSRCS + 1
         SOURCS(NSRCS) = CSRC
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE SETSTN(IRET)
C-----------------------------------------------------------------------
C Routine which sets up various quantities needed for antenna tables
C Output:
C   IRET    I   Error return code.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER CHTM8*8, AHDRNM*8, BHDRNM*8
      INTEGER   HDR(256), DAY, YEAR, I
      LOGICAL   FOUND(50), FIRST, T
      REAL      AAXOFF, BAXOFF, HDR4(128)
      REAL      HDRPLX, HDRPLY, HDRUT1
      DOUBLE PRECISION  AHDRNH, BHDRNH, AX, AY, AZ, BX, BY, BZ,
     *          HDRFRQ, GASTM, IATOFF, JD, TU, XFREQ, UT1UTC,
     *          IATUTC, DEGPDY, GSTIA0, HDR8(64), TJD, REFNH
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C                                       Values from VLBI scan
C                                       header record.
C                                       Integer equiv.
      EQUIVALENCE (YEAR,  HDR(1)),   (DAY   ,HDR(2))
C                                       Real equiv.
      EQUIVALENCE (AAXOFF,HDR4(33)), (BAXOFF,HDR4(51)),
     *            (HDRUT1,HDR4(57)), (HDRPLX,HDR4(58)),
     *            (HDRPLY,HDR4(59))
C                                       Double prec. equiv
      EQUIVALENCE (AHDRNH,HDR8(10)), (AX    ,HDR8(11)),
     *            (AY    ,HDR8(12)), (AZ    ,HDR8(13)),
     *            (BHDRNH,HDR8(19)), (BX    ,HDR8(20)),
     *            (BY    ,HDR8(21)), (BZ    ,HDR8(22)),
     *            (GASTM ,HDR8(28)), (HDRFRQ,HDR8(15)),
     *            (REFNH ,HDR8(41))
C
      DATA      FOUND/50*.FALSE./,FIRST/.FALSE./
      DATA      T /.TRUE./
C-----------------------------------------------------------------------
      IA = 0
      IB = 0
      IRET = 0
C                                       Copy arrays for equivalencing
      DO 10 I = 1, 256
        HDR(I) = SCNHDR(I)
 10     CONTINUE
      DO 20 I = 1, 128
        HDR4(I) = SCN4(I)
 20     CONTINUE
      DO 30 I = 1, 64
        HDR8(I) = SCN8(I)
 30     CONTINUE
      CALL H2CHR (8, 1, AHDRNH, AHDRNM)
      CALL H2CHR (8, 1, BHDRNH, BHDRNM)
      CALL H2CHR (8, 1, REFNH, REFNAM)
      DO 100 I = 1, NKSTNS
         IF(AHDRNM.EQ.KEYSTN(I)) IA = I
         IF(BHDRNM.EQ.KEYSTN(I)) IB = I
 100     CONTINUE
      IF (REFNAM.EQ.AHDRNM) KKREFS = 1
      IF (REFNAM.EQ.BHDRNM) KKREFS = 2
C                                       Are both station names from
C                                       the scan hdr in STNNAM ?
      IF (IA.GT.0) GO TO 110
         WRITE (MSGTXT,1005) AHDRNM
         CALL MSGWRT(8)
         IRET = -1
         GO TO 999
 110  IF (IB.GT.0) GO TO 200
         WRITE (MSGTXT,1005) BHDRNM
         CALL MSGWRT(8)
         IRET = -1
         GO TO 999
C                                       Load from first scan read to get

C                                       something in case there is no
C                                       ref day scan.
 200  FIRST = .FALSE.
C
      CALL JULIAN (YEAR, REFDAY, 0.D0, JD)
      TJD = JD - 2400000.5D0
      IATOFF = 0.0
      GSTIA0 = (GASTM + IATOFF * PI / 43200.D0) * RAD2DG
      GST0   = GSTIA0
      XFREQ  = HDRFRQ * 1.0D+06
C
      UT1UTC    = HDRUT1
      XUT1      = UT1UTC
      IATUTC    = IATOFF
      XIAT      = IATUTC
      IATUT1    = IATUTC
C                                       Load AN file header.
      TU     = (JD - 2415020.0D0) / 36525.0D0
      DEGPDY = (1.00273790265D0 + 0.589D-10 * TU) * 360.0D0
      CALL GREG (JD, CHTM8)
      CALL CHR2H (8, CHTM8, 1, CATR(KHDOB))
      IF (.NOT.FOUND(IA)) THEN
C                                       Load ANT file array.
         FOUND(IA)    = T
         NANT         = NANT + 1
         ANTNAM(IA) = AHDRNM
         ANTLOC(1,IA) = AX
         ANTLOC(2,IA) = AY
         ANTLOC(3,IA) = AZ
         ANTOFF(IA)   = AAXOFF
      END IF
C
      IF (.NOT.FOUND(IB)) THEN
C                                       Load ANT file array.
         FOUND(IB)    = T
         NANT         = NANT + 1
         ANTNAM(IB) = BHDRNM
         ANTLOC(1,IB) = BX
         ANTLOC(2,IB) = BY
         ANTLOC(3,IB) = BZ
         ANTOFF(IB)   = BAXOFF
      END IF
C
 999  CONTINUE
      RETURN
C-----------------------------------------------------------------------
 1005 FORMAT ('SETSTN: STATION IN DATA, ',A8,', NOT IN STATION LIST')
      END
      SUBROUTINE SELECT (LSEL,IRET)
C-----------------------------------------------------------------------
C     SELECT is called from the VLBI AIPS routine VBLIN.
C     SELECT tests VLBI header and data records against user
C     specified source name and start/stop time boundries.
C     OUTPUT: LSEL   L   If true, pass data reocrd into AIPS.
C     J. M. Benson   16 March, 1983
C-----------------------------------------------------------------------
      LOGICAL   LSEL
      INTEGER   IRET
C
      CHARACTER  SRC*8
      INTEGER    I, NSOUR
      LOGICAL    HSEL, FIRST
      REAL       START, STOP, DSTART, DSTOP, BANDI
      DOUBLE PRECISION     RAT, RDAY, FREQA
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
C
      DATA       FIRST/.TRUE./, NSOUR/0/
C-----------------------------------------------------------------------
C
      IRET = 0
      LSEL = .TRUE.
C                                               On the first call to
C                                               SELECT, load and count
C                                               the INPUTS sources.
      IF (.NOT.FIRST) GO TO 50
         DO 10 I = 1, 30
            XSOUR(I) = ' '
            CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
            IF (XSOUR(I).EQ.' ') GO TO 10
               NSOUR = NSOUR + 1
 10         CONTINUE
         FIRST = .FALSE.
C                                       If data record, go to 200
C                                       Otherwise, must be a scan hdr
 50   IF (TYPE.NE.1 .AND. TYPE.NE.10) GO TO 200
C
         RDAY  = TWOPI * SCNHDR(2)
         CALL H2CHR (8, 1, SCN8(3), SRC)
         START = SCN4(3)
         STOP  = SCN4(4)
         HSEL  = .TRUE.
C                                       Was the current source specified
C                                       in the INPUTS ?
         IF (NSOUR.EQ.0) GO TO 120
         DO 105 I = 1, NSOUR
 105        IF (XSOUR(I).EQ.SRC) GO TO 120
         HSEL = .FALSE.
         GO TO 300
C                                       Is the current source in the
C                                       KEYIN source list ?
 120     CONTINUE
         DO 130 I = 1, NKSRCS
 130        IF (KEYSRC(I).EQ.SRC) GO TO 150
         HSEL = .FALSE.
         GO TO 300
C                                       Compare the scan time range
C                                       with that time range in INPUTS.
 150     DSTART = RDAY + START
         DSTOP  = RDAY + STOP
         IF (DSTOP.GT.TSTART.AND.DSTART.LT.TSTOP) GO TO 160
            HSEL = .FALSE.
            GO TO 300
C                                       Check bandwidth & freqs
C                                       against user supplied values.
 160     IF (ACCBW.GT.0.0) THEN
            BANDI = SCN4(65) * 1.0E3
            IF (ACCBW.NE.BANDI) THEN
               HSEL = .FALSE.
               GO TO 300
               END IF
            END IF
         IF (ACCFQL.NE.ACCFQH) THEN
            FREQA = SCN8(15) * 1.0D6
            IF ((FREQA.LT.ACCFQL) .OR. (FREQA.GT.ACCFQH)) THEN
               HSEL = .FALSE.
               GO TO 300
               END IF
            END IF
         GO TO 300
C                                       Test data record time against
C                                       INPUTS time range here
 200  RAT = RDAY + AT
      IF (RAT.GE.TSTART.AND.RAT.LE.TSTOP) GO TO 300
         LSEL = .FALSE.
C
 300  IF (.NOT.HSEL) LSEL = .FALSE.
      RETURN
      END
      SUBROUTINE SETPRM (IERR)
C-----------------------------------------------------------------------
C  SETPRM constructs a set of program control parameters from the
C  inputs variables in CVLB.INC. The control parameters are carried
C  in the commons in CSET.INC.
C-----------------------------------------------------------------------
      INTEGER  IERR
C
      INTEGER    IBDROP, IEDROP, I, IROUND
      REAL       BSTART, BSTOP, T1, T2
      LOGICAL    T, F
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA  T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
C
       NUMIF  = 0
C
       XFORM = F
       MACHDL = F
       LFBS = F
       LAFBS = F
       VLECK = F
       BUGGER = F
       LINED = F
       NOAC = F
       NOUVC = F
       FBSAMP = T
       ACSWIT = T
       SIDER = 1.002737923D0
       CLIGHT = 2.997924562D08
       LOCHN  = IROUND (XBCHAN)
       HICHN  = IROUND (XECHAN)
       IBDROP = IROUND (XBDROP)
       IEDROP = IROUND (XEDROP)
C
       IF (APARM(2).EQ.1.D0) XFORM  = .TRUE.
       IF (APARM(1).EQ.1.D0) MACHDL = .TRUE.
       IF (APARM(6).EQ.1.D0) LFBS   = .TRUE.
       IF (APARM(6).EQ.2.D0) LAFBS  = .TRUE.
       IF (APARM(7).EQ.1.D0) NOUVC  = .TRUE.
       IF (APARM(9).EQ.1.D0) VLECK  = .TRUE.
       IF (APARM(10).EQ.0.D0) BUGGER = .TRUE.
       IF (APARM(11).EQ.1.D0) NOAC  = .TRUE.
       IF (APARM(17).GT.0.D0) ACSWIT = .FALSE.
C                                       Set up freq. acceptance parms
       ACCBW = APARM(13) * 1.0E3
       ACCFQL = (APARM(14)-APARM(15)/2) * 1.0E6
       ACCFQH = (APARM(14)+APARM(15)/2) * 1.0E6
C
       NSTOKE = APARM(3)
       IF (NSTOKE.LT.0) NSTOKE = 1
       NUMPOL = NSTOKE
       IF (NSTOKE.GE.1.AND.NSTOKE.LE.4) GO TO 100
          WRITE (MSGTXT,1100) NSTOKE
          CALL MSGWRT (8)
          IERR = 1
          GO TO 999
 100   IPOLAR = APARM(4)
       IF (IPOLAR.LE.0) IPOLAR = 1
       IF (IPOLAR.GE.1.AND.IPOLAR.LE.4.AND.IPOLAR.LE.NSTOKE) GO TO 110
          WRITE (MSGTXT,1120) IPOLAR
          CALL MSGWRT (8)
          IERR = 1
          GO TO 999
C
 110   REFDAY = APARM(5)
       IF (REFDAY.GT.0.AND.REFDAY.LT.366) GO TO 120
          WRITE (MSGTXT,1110)
          CALL MSGWRT (8)
          IERR = 1
          GO TO 999
C                                       Set up channel ranges :
C                                       NFREQ is number of freq chans
C                                       from transform; NFOUT is the
C                                       number of freq chans written
C                                       into AIPS vis. records.
C                                       CHNLO, CHNHI specify freq chan
C                                       range written to AIPS.
 120  CONTINUE
      NFREQ = HICHN - LOCHN + 1
      NFOUT = NFREQ
      IF (NFREQ.GT.16) LINED = .TRUE.
      BUGGER = BUGGER .AND. LINED
      ACSWIT = ACSWIT .AND. LINED
      IF (NFREQ.LT.33) BUGGER = .FALSE.
      IF (LINED) THEN
         IF (XFORM) THEN
             NLAG = HICHN - LOCHN + 1
C             IP2 = ALOG (FLOAT(NLAG)) / ALOG(2) + 0.9999
C             ILAG = EXP (IP2 * ALOG(2))
C             NFREQ = ILAG / 2
             NFREQ = NLAG / 2
             NFOUT = NFREQ - IBDROP - IEDROP
             END IF
      ELSE IF (.NOT. LINED) THEN
         IF (.NOT.XFORM) THEN
            IBDROP = 0
            IEDROP = 0
            END IF
         IF (XFORM) THEN
            NFREQ = (HICHN - LOCHN + 2) / 2
            NFOUT = NFREQ - IBDROP - IEDROP
            END IF
         IF (XFORM) CHNHI = (HICHN - LOCHN + 2) / 2 - IEDROP
         END IF
      CHNLO = IBDROP + 1
      CHNHI = HICHN - LOCHN + 1
      IF (NFREQ.GT.128) THEN
         LFBS = .FALSE.
         LAFBS = .FALSE.
         END IF
C
C                                       Get user's start/stop times
C
      BSTART = TIMRAN(1) + TIMRAN(2)/24.0D0 + TIMRAN(3)/(24.D0*60.D0)
     *       + TIMRAN(4)/(24.D0*60.D0*60.D0)
      TSTART = TWOPI * BSTART
      BSTOP  = TIMRAN(5) + TIMRAN(6)/24.0D0 + TIMRAN(7)/(24.D0*60.D0)
     *       + TIMRAN(8)/(24.D0*60.D0*60.D0)
      TSTOP  = 366.0D0 * TWOPI
      IF (BSTOP.GT.0.0D0) TSTOP = TWOPI * BSTOP
C
      IF (XSOUR(1).NE.' ') GO TO 210
         DO 205 I = 1, 30
            XSOUR(I) = KEYSRC(I)
 205        CONTINUE
C
C                                       Write out back to user what
C                                       he is about to do to himself.
 210  NCHAN = HICHN - LOCHN + 1
      IF (LOCHN.GT.0.AND.HICHN.GT.0.AND.NCHAN.GT.0) GO TO 220
         WRITE (MSGTXT,2100) LOCHN,HICHN
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
C
 220  CONTINUE
      IF (TSTART.LT.TSTOP) GO TO 230
         WRITE (MSGTXT,2200)
         CALL MSGWRT (8)
         IERR = 1
         GO TO 999
C
 230  CONTINUE
      IF (.NOT.MACHDL) GO TO 250
         WRITE (MSGTXT,2400)
         CALL MSGWRT (8)
C
 250  CONTINUE
      IF (.NOT.XFORM)  GO TO 260
         NCHAN = HICHN - LOCHN + 1
         WRITE (MSGTXT,2500) NCHAN, NFREQ
         CALL MSGWRT (8)
      IF (BUGGER) THEN
         WRITE (MSGTXT,2550)
         CALL MSGWRT (8)
         WRITE (MSGTXT,2570)
         CALL MSGWRT (8)
         END IF
      IF (ACSWIT) THEN
         WRITE (MSGTXT,2580)
      ELSE
         WRITE (MSGTXT,2590)
         END IF
      CALL MSGWRT (8)
C
 260  CONTINUE
      WRITE (MSGTXT,2600) NFOUT
      CALL MSGWRT (8)
      IF (.NOT.LFBS) GO TO 261
         WRITE (MSGTXT,2700)
         CALL MSGWRT (8)
 261  IF (.NOT.LAFBS) GO TO 270
         WRITE (MSGTXT,2701)
         CALL MSGWRT (8)
C
 270  CONTINUE
      IF (VLECK .AND. XFORM) THEN
         WRITE (MSGTXT,2800)
         CALL MSGWRT (8)
         END IF
      IF (VLECK .AND. (.NOT. XFORM)) THEN
         VLECK = .FALSE.
         WRITE (MSGTXT,2900)
         CALL MSGWRT (8)
         WRITE (MSGTXT,2910)
         CALL MSGWRT (8)
         IERR = 1
         END IF
C
      IF (NOAC) THEN
         WRITE (MSGTXT,3000)
         CALL MSGWRT (8)
         END IF
C
      IF (NOUVC) THEN
         WRITE (MSGTXT,3010)
         CALL MSGWRT (8)
         END IF
C
      IF (ACCBW.NE.0.0) THEN
         WRITE (MSGTXT,3020) APARM(13)
         CALL MSGWRT (8)
         END IF
C
      IF (ACCFQL.NE.ACCFQH) THEN
         T1 = ACCFQL / 1.0E6
         T2 = ACCFQH / 1.0E6
         WRITE (MSGTXT,3030) T1, T2
         CALL MSGWRT (8)
         END IF
C
      IF (ARGD.GT.0.D0) THEN
         T1 = ARGD / 1.0E6
         WRITE (MSGTXT,3040) T1
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR, ILLEGAL APARM(3) =',I4,' TRY 1,2,3 OR 4')
 1110 FORMAT ('ERROR: SPECIFY REFERENCE DAY NUMBER, APARM(5)')
 1120 FORMAT ('ERROR, ILLEGAL APARM(4) =',I4)
 2100 FORMAT ('ILLEGAL BCHAN,ECHAN = ',I4,',',I4)
 2200 FORMAT ('START TIME IS LATER THAN STOP TIME IN INPUTS.')
 2400 FORMAT ('VLBIN will apply processor machine delay corrections.')
 2500 FORMAT ('Transforming ',I4,' delay chans to',I4,' freq chans.')
 2550 FORMAT ('Spectral line data - will use FFT not DFT')
 2570 FORMAT ('Will apply the fringe-rotator and factor of 2 (0.64) to',
     *   ' XC data')
 2580 FORMAT ('Will switch the autocorrelation arrays')
 2590 FORMAT ('Will NOT switch the autocorrelation arrays')
 2600 FORMAT ('VLBIN will write ',I4,' frequency channels.')
 2700 FORMAT ('VLBIN will apply fractional-bit-shift corrections.')
 2701 FORMAT ('VLBIN will apply amp. portion of fbs corrections.')
 2800 FORMAT ('VLBIN will apply the Van-Vleck correction to line data',
     *   ' only')
 2900 FORMAT ('Van-Vleck correction can only be applied to lag data')
 2910 FORMAT ('CHECK YOUR INPUT PARAMETERS')
 3000 FORMAT ('VLBIN will not pass autocorrelation data')
 3010 FORMAT ('VLBIN will not recalculate u and v')
 3020 FORMAT ('VLBIN will accept data with bandwidth ',F7.2,' kHz')
 3030 FORMAT ('VLBIN will accept data from ',F7.2,' - ',F7.2,' MHz')
 3040 FORMAT ('VLBIN will force ref. freq to be ',F15.7,' MHz')
      END
      SUBROUTINE PROCES (U, V, W, T, IOUT1, IOUT2, VIS, LREFST, IERR)
C-----------------------------------------------------------------------
C     VLBI data record is received in array DATA, transformed,
C     corrected for machine delay error, fractional-bit-shift error
C     or not, and loaded into the AIPS output buffer.
C-----------------------------------------------------------------------
      REAL      U, V, W, T, VIS(3,*)
      INTEGER   IOUT1, IOUT2, IERR, LREFST
C
      INTEGER   ISIGN, ISB2, NCOR, NC, NS, JERR, II, NA, NB, IPS, KFRQ,
     *   K, J, I,I1, I0, IPIX, NTRNF, TIT1(4)
      LOGICAL   LOBFLG
      REAL      TRANS(2,128), TINT, BUFF2(8192),
     *          PDERR, PDFIX, DLYRAT, DLYROT, FBSERR(128,2),
     *          LOBROT, TSPANR, AMPFBS, FBSCOS, FBSSIN, PRATE,
     *          PHSCOR, PHSSIN, PHSCOS, X, Y, FROTCR, TDERR,
     *          BW, PIBY2, DECOR(1024), OLDU, OLDV, RAT, RHR, RDAY
      DOUBLE PRECISION     PFR, SHA, PDLY, PRTIME, GST, DLYBIT, DELY,
     *          ADLY0, ADLY1, ADLY2, ART1, ART2, ADRT, DRATE,
     *          BDLY0, BDLY1, BDLY2, BRT1, BRT2, BDRT,
     *          MICRAD, MACDEL, MACDAL, STM, SSHA, CSHA, DBW,
     *          DLYWID, FRQFCT, SWT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (OLDU,  DAT4(3)),    (OLDV,  DAT4(4)),
     *            (TINT,  DAT4(5)),    (TSPANR,DAT4(6)),
     *            (TDERR, DAT4(16))
      EQUIVALENCE (PRTIME, DAT8(4)),    (PDLY,  DAT8(5)),
     *            (PFR,   DAT8(6))
      EQUIVALENCE (BW,    SCN4(65)),   (ISB2,  SCNHDR(131))
      DATA       MICRAD/7.272205218D-11/
C-----------------------------------------------------------------------
C
      PIBY2 = 2.0 * ATAN (1.0)
C                                       Data integration time
      CURINT = TSPANR
      IF (CURINT.LE.0.0) CURINT = 1.0
C
      IF (BUGGER) THEN
         FROTCR = 1.28 / 2.0
      ELSE
C                                       these corrections absorbed
C                                       in b-factor for continuum
         FROTCR = 1.0
         END IF
C
      JERR = 0
      LOBFLG = .FALSE.
      IF (BW .GT. 1500.) BW = 2000.0
      DBW = DBLE (BW)
      FRQFCT = 2000.D0 / DBW
      DLYWID = 0.25D0 * FRQFCT
C                                       Frequency switching
      SWT = (AT/TWOPI)+IATUT1/86400.0+DAYN
      RAT = SWT
      CALL TODHMS (RAT, TIT1)
      RDAY = TIT1(1)
      RHR = TIT1(2)
      RAT = SWT - (RHR/24.0) - RDAY
      IF (ASW.GT.0) THEN
         DO 10 I = 1, NSWPHR
            IF ((RAT.GE.SWTIME(I,1)) .AND. (RAT.LT.SWTIME(I,2)))
     *         GO TO 50
 10      CONTINUE
         IERR = 2
         GO TO 1000
         END IF
C
 50   IF ((TYPE.EQ.3) .OR. (TYPE.EQ.5)) GO TO 700
C                                       Calculate u and v here
      STM = AT * SIDER
      SHA = SFACT + STM
      CSHA = COS(SHA)
      SSHA = SIN(SHA)
      IF (.NOT. NOUVC) THEN
         U   = BXM*SSHA - BYM*CSHA
         V   = CBZM - SBXM*CSHA - SBYM*SSHA
         W   = CBXM*CSHA + CBYM*SSHA + SBZM
         U   = UVCON * U
         V   = UVCON * V
         W   = UVCON * W
         END IF
      IF (NOUVC) THEN
         U = OLDU
         V = OLDV
         W = 0.
         END IF
C
      IF (.NOT.XFORM) GO TO 200
C                                       If line data do Van-Vleck
C                                       clipping correction then FFT
      IF (LINED) THEN
         NTRNF = NDD/2
C                                       Van-Vleck
         IF (VLECK) THEN
            DO 100 I = 1, NDD
               RECDAT(I) = SIN(PIBY2*RECDAT(I))
 100           CONTINUE
            END IF
C
         CALL DTRANS (RECDAT, BUFF2, NTRNF, ISB2)
         J = 0
         DO 110 I = 1, NFREQ
            NC   = 2 * I - 1
            NS   = 2 * I
            J    = J + 1
            TRANS(1,J) = RECDAT(NC)
            TRANS(2,J) = RECDAT(NS)
 110        CONTINUE
         GO TO 300
         END IF
C                                       DFT data record.
      DO 120 K = 1, NFREQ
          TRANS(1,K) = 0.0
          TRANS(2,K) = 0.0
 120      CONTINUE
C
      DO 140 K = 1, NFREQ
         J = 0
         DO 140 I = LOCHN, HICHN
             J    = J + 1
             NC   = 2 * I - 1
             NS   = 2 * I
             TRANS(1,K) = RECDAT(NC)*CP(K,J) - RECDAT(NS)*SP(K,J)
     *                                       + TRANS(1,K)
             TRANS(2,K) = RECDAT(NC)*SP(K,J) + RECDAT(NS)*CP(K,J)
     *                                       + TRANS(2,K)
 140         CONTINUE
C
 200  CONTINUE
      IF (XFORM) GO TO 300
      J = 0
      DO 210 I = LOCHN, HICHN
         NC   = 2 * I - 1
         NS   = 2 * I
         J    = J + 1
         TRANS(1,J) = RECDAT(NC)
         TRANS(2,J) = RECDAT(NS)
 210     CONTINUE
C
C
 300  IF (.NOT.MACHDL) PHSSET = 0.0
      PHSCOR = PFR * PHSSET
      PHSCOS = COS(PHSCOR)
      PHSSIN = SIN(PHSCOR)
      II = 0
      DO 310 I = CHNLO, CHNHI
         II   = II + 1
         NC   = 2 * II - 1
         NS   = 2 * II
         RECDAT(NC) = TRANS(1,I)*PHSCOS - TRANS(2,I)*PHSSIN
         RECDAT(NS) = TRANS(1,I)*PHSSIN + TRANS(2,I)*PHSCOS
 310  CONTINUE
C
      IF (.NOT.LFBS) GO TO 400
C                                       Calculate the FBS errors
C                                       here. First get the wave
C                                       arrival time at the earth
C                                       center. Use the processor
C                                       stn 'A' time and delay to
C                                       the earth center.
      CALL ECTIME (PRTIME, MACDEL, MACDAL, ITFLAG, GAST0, SIDER, PACLK,
     *   MICRAD, ABR)
      GST = (PRTIME + MACDEL) * SIDER + GAST0
      GST = MOD (GST,TWOPI)
C                                       Calculate the center earth
C                                       delay for each station in
C                                       current baseline.
      CALL AVGDLY (GST,AR,ADLY0,ADLY1,ADLY2,ART1,ART2,ADRT)
      CALL AVGDLY (GST,BR,BDLY0,BDLY1,BDLY2,BRT1,BRT2,BDRT)
C                                       Put together the various
C                                       order terms depending on what
C                                       has previously been done to
C                                       the data.
      IF (AEQPA .OR. JISOC.NE.0) GO TO 350
      DELY  = BDLY1 - ADLY1
      DRATE = BRT1  - ART1
      GO TO 370
 350  IF (JISOC.NE.0) GO TO 360
      DELY  = BDLY2 - ADLY2
      DRATE = BRT1  - ART2
      GO TO 370
 360  DELY  = BDLY2 - ADLY2
      DRATE = BRT2  - ART2
 370  CONTINUE
      DELY   = DELY - (ACLK - BCLK)
      DLYBIT = DELY / 0.25D0
C                                       Truncate to integer
      IPS = DLYBIT
      DLYBIT = IPS
C                                       Processor delay error, PDERR.
      PDERR  = DELY - DLYBIT * 0.25D0
      DLYRAT = DRATE * 1.0E+06
C                                       Throw out records where fewer
C                                       than three lobe rotator turns
C                                       in average interval.
      PRATE  = - PFR
      IF (INSOC.EQ.0) PRATE = - (PFR - (ADOP - BDOP))
      IF (ABS (PRATE) .LT. ABS (PFR)) PRATE = -PFR
      LOBROT = ABS (PRATE*AVGX)
      IF (LOBROT.LE.3.0) LOBFLG = .TRUE.
C                                       Delay error at start of record.
      PDFIX  = PDERR - DLYRAT * 0.10
      IF (PDFIX.LT. (-0.125)) PDFIX = PDFIX + (0.250)
      IF (PDFIX.GT. (+0.125)) PDFIX = PDFIX - (0.250)
C                                       Delay change during record.
      DLYROT = DLYRAT * AVGX
C                                       Calc FBS errors.
      CALL FBSCOR (NFREQ, PDFIX, DLYROT, AVGX, FBSERR, IERR)
C                                       Flip baseline direction
C                                       to be consistant with
C                                       self-cal requirements.
 400  ISIGN = 1
      IOUT1 = IA
      IOUT2 = IB
      LREFST = KKREFS
      IF(IB.LT.IA) GO TO 410
         ISIGN = -1
         IOUT1 = IB
         IOUT2 = IA
 410  CONTINUE
C                                       Load random parmeters
      U = ISIGN * U
      V = ISIGN * V
      W = ISIGN * W
      T = (AT/TWOPI)+IATUT1/86400.0+DAYN
C                                       Load decorrelation array
C                                       to restore amplitudes to state
C                                       existing before MORASS
      DECOR(1) = 1.
      DO 420 I = 2, NFREQ
         X = PIBY2 * (I-1)/NFREQ
         DECOR(I) = SIN(X)/X
 420     CONTINUE
C
      NCOR = 0
      IPIX = 0
      DO 610 I1 = 1, NFREQ
      DO 610 I0 = 1, NSTOKE
         IPIX  = IPIX + 1
         NCOR  = NCOR + 1
         IF (IPOLAR.NE.I0) VIS(3,IPIX) = -99.0
         IF (IPOLAR.EQ.I0) VIS(3,IPIX) = TINT / 0.2
         IF (LOBFLG)       VIS(3,IPIX) = -99.0
C
         NC = 2 * NCOR - 1
         NS = 2 * NCOR
         X  = RECDAT(NC)
         Y  = RECDAT(NS)
         IF (X.LT.-1.0E06 .OR. X.GT.1.0E06 .OR. X.EQ.0.0) X = 1.0E-5
         IF (Y.LT.-1.0E06 .OR. Y.GT.1.0E06 .OR. Y.EQ.0.0) Y = 1.0E-5
         IF (.NOT.LFBS) GO TO 600
            KFRQ   = I1 + CHNLO - 1
            AMPFBS = SQRT (FBSERR(KFRQ,1)*FBSERR(KFRQ,1)
     *             +       FBSERR(KFRQ,2)*FBSERR(KFRQ,2))
            FBSCOS =                  FBSERR(KFRQ,1) / AMPFBS
            FBSSIN =           ISB2 * FBSERR(KFRQ,2) / AMPFBS
C                                            Check if ampl. correction
C                                            done in Morass
            IF (.NOT. FBSAMP) THEN
               X = X * DECOR(I1)
               Y = Y * DECOR(I1)
            END IF
            VIS(1,IPIX) =           (X*FBSCOS - Y*FBSSIN) / AMPFBS
            VIS(2,IPIX) = - ISIGN * (X*FBSSIN + Y*FBSCOS) / AMPFBS
            VIS(1,IPIX) = VIS(1,IPIX) * FROTCR
            VIS(2,IPIX) = VIS(2,IPIX) * FROTCR
            GO TO 610
 600     IF (LAFBS) THEN
            X = X / DECOR(I1)
            Y = Y / DECOR(I1)
            END IF
         VIS(1,IPIX) =           X
         VIS(2,IPIX) = - ISIGN * Y
         VIS(1,IPIX) = VIS(1,IPIX) * FROTCR
         VIS(2,IPIX) = VIS(2,IPIX) * FROTCR
 610     CONTINUE
C                                            Autocorrelation data
 700  IF (TYPE .NE. 3 .AND. TYPE .NE. 5) GO TO 1000
      U = 0.
      V = 0.
      W = 0.
      IOUT1 = IA
      IOUT2 = IB
      LREFST = KKREFS
      T = (AT/TWOPI)+IATUT1/86400.0+DAYN
      IF (TYPE .EQ. 3) THEN
         NTRNF = NDD/2
         CALL ACTRAN (RECDAT,BUFF2,NTRNF,NFREQ,DATASB,ACBIAS,
     *      NACSCN,IA,IB)
         END IF
      NCOR = 0
      IPIX = 0
      DO 710 I1 = 1, NFREQ
         DO 710 I0 = 1, NSTOKE
            IPIX  = IPIX + 1
            NCOR  = NCOR + 1
            IF (IPOLAR.NE.I0) VIS(3,IPIX) = -99.0
            IF (IPOLAR.EQ.I0) VIS(3,IPIX) = TINT / 0.2
            NA = 2 * NCOR - 1
            NB = 2 * NCOR
            IF (.NOT.ACSWIT) THEN
C                                             Don't switch AC arrays
               VIS(1,IPIX) = RECDAT(NA)
               VIS(2,IPIX) = RECDAT(NB)
            ELSE
C                                             Switch AC arrays
               VIS(1,IPIX) = RECDAT(NB)
               VIS(2,IPIX) = RECDAT(NA)
               END IF
 710        CONTINUE
C
 1000 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE SETDFT (IRET)
C-----------------------------------------------------------------------
C     Sets up sin and cos table for DFT, and other constants for
C     calculating u and v. Called from VBLIN.
C-----------------------------------------------------------------------
      INTEGER ISBN, KMID, NFRQS, NLAGS, SOC, PMODE, PBAS,
     *  NCHANS, K, J, IRET, DAY
      LOGICAL FIRST
      REAL    DLY1, DELDLY, DELF, PHS, BW, PHSCON, FREQ, DELAY
      DOUBLE PRECISION  DEC, CDEC, SDEC,
     *           RA, AX, AY, AZ, BX, BY, BZ, PAX, PAY, PAZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
      OMEGA  = TWOPI * SIDER * 1.0D-06 / 86400.0D0
C
      DAY    = SCNHDR(2)
      RA     = SCN8(6)
      DEC    = SCN8(7)
      AX     = SCN8(11)
      AY     = SCN8(12)
      AZ     = SCN8(13)
      ACLK   = SCN8(14)
      AFREQ  = SCN8(15)
      ADOP   = SCN8(16)
      BX     = SCN8(20)
      BY     = SCN8(21)
      BZ     = SCN8(22)
      BCLK   = SCN8(23)
      BDOP   = SCN8(25)
      GAST0  = SCN8(28)
      PACLK  = SCN8(42)
      SOC    = SCNHDR(148)
      PMODE  = SCNHDR(152)
      PBAS   = SCNHDR(143)
      NCHANS = SCNHDR(4)
      ISBN   = SCNHDR(131)
      AVGX   = SCN4(63)
      AVGAC  = SCN4(64)
      BW     = SCN4(65)
      IF (BW.LT.62.5) BW = 2000.0
C
      DAYN = DAY - REFDAY
      IF (REFDAY.GE.0) GO TO 100
         WRITE (MSGTXT,1100)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1101)
         CALL MSGWRT (8)
         IRET = 1
         GO TO 999
C
 100  IF (.NOT.FIRST.OR..NOT.XFORM) GO TO 200
         FIRST = .FALSE.
         NFRQS =  HICHN - LOCHN + 2
         NFRQS2 = NFRQS/2
         NLAGS  = HICHN - LOCHN + 1
         KMID   = NCHANS / 2 + 1
         DELDLY = 1.0E-06 * 500.0 / BW
         DLY1   = DELDLY * (LOCHN - KMID)
         DELF   = 2.0E03 * BW / NFRQS
         FREQ   = -DELF / 2.0
C                                        Note, the DFT is normalized
C                                        by 2.0 (in CP and SP).
         IF (LINED) GO TO 200
         DO 110 K = 1, NFRQS2
            FREQ  = FREQ + DELF
            DELAY = - DLY1 + DELDLY
            DO 110 J = 1, NLAGS
               DELAY = DELAY - DELDLY
               PHS   = -1.0*ISBN*TWOPI*FREQ*DELAY
               CP(K,J) = COS(PHS) / 2.0
 110           SP(K,J) = SIN(PHS) / 2.0
C
 200     CONTINUE
         UVCON = (SCN8(15)*1.D6 + ISBN*(BW/2.D0)*1.D3)/CLIGHT
         IF (.NOT.XFORM) NFRQS2 = HICHN - LOCHN + 1
         SFACT = SCN8(28) - RA
         BXM   = BX - AX
         BYM   = BY - AY
         BZM   = BZ - AZ
         BM    = SQRT(BXM*BXM + BYM*BYM + BZM*BZM)
         CDEC  = COS(DEC)
         SDEC  = SIN(DEC)
         SBXM  = BXM * SDEC
         CBXM  = BXM * CDEC
         SBYM  = BYM * SDEC
         CBYM  = BYM * CDEC
         SBZM  = BZM * SDEC
         CBZM  = BZM * CDEC
C                                       Machine delay correction
C                                       for C'ville processor.
      PHSSET = 0.0
      IF (.NOT.MACHDL.OR.SOC.NE.0) GO TO 210
         PHSCON= TWOPI / (2.0E03*BW)
         IF (PMODE.LT.0             ) PHSSET =   0.0
         IF (PMODE.EQ.0             ) PHSSET =  96.0*PHSCON
         IF (PMODE.EQ.4             ) PHSSET = 144.0*PHSCON
         IF (PMODE.EQ.6.AND.PBAS.EQ.1) PHSSET =  48.0*PHSCON
         IF (PMODE.EQ.6.AND.PBAS.EQ.2) PHSSET =  96.0*PHSCON
         IF (PMODE.EQ.6.AND.PBAS.EQ.3) PHSSET =  96.0*PHSCON
 210     CONTINUE
C                                        Fractional-bit-shift
C                                        corrections for C'ville
C                                        processor.
      IF (.NOT.LFBS) GO TO 220
         INSOC  = SOC
         AEQPA  = PBAS.EQ.1 .OR. PBAS.EQ.2
         ITFLAG = INSOC / 10
         JISOC  = INSOC - ITFLAG * 10
         CALL AVGSET (RA,DEC,AX,AY,AZ,CLIGHT,OMEGA,AR)
         CALL AVGSET (RA,DEC,BX,BY,BZ,CLIGHT,OMEGA,BR)
C                                        Save x,y,z of processor
C                                        station 'A'.
         IF (.NOT.AEQPA) GO TO 215
            PAX  = AX
            PAY  = AY
            PAZ  = AZ
 215        CONTINUE
         CALL AVGSET (RA,DEC,PAX,PAY,PAZ,CLIGHT,OMEGA,ABR)
 220     CONTINUE
 999  CONTINUE
C
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR: NEGATIVE RELATIVE DAY NUMBER CALCULATED.')
 1101 FORMAT ('REFERENCE DAY, APARM(5), MUST < OR = FIRST ',
     *   'DAY IN EXP.')
      RETURN
      END
      SUBROUTINE FBSCOR (NFRQS, DFIRST, DLYROT, AVGXC, FBSERR, IERR)
C-----------------------------------------------------------------------
C     Calculate fractional-bit-shift amp and phase errors for NFRQS
C     frequency channels. Load into array FBSERR. FBSCOR is called
C     from VBLIN only.
C       NFRQS      I      Number of frequency channels in data record.
C       DFIRST     R      Processor delay error at the beginning of
C                         the data record (in microsecs).
C       DLYROT     R      Change in delay during this record interval
C                         (in microsecs).
C       AVGXC      R      Record time interval (in seconds).
C       FBSERR     R      Amp and phase errors in this record due to
C                         the fractional-bit-shift error. Calculated
C                         for each of NFRQS frequency channels.
C       IERR       I      Usual AIPS error return code.
C     J.M. Benson,  15 February 1984
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INTEGER    NTURNS, N60HZ, NT, NFRQS, IERR, KFRQ, DBYREC, N5HZ
      REAL       PIBY4N, PIBY2N, THETA0(128), THETA1(128),
     *           THETAN(128), FBSERR(128,2), DLYWND, ISGN, DFIRST,
     *           DLYROT, AVGXC, CORDLY(50), CORPHS(128,50), TCROSS,
     *           X60HZ, DELTIM, DENOM(128), DLAST, X5HZ
      DOUBLE PRECISION     SIDER, CLIGHT
      COMMON     /CONST/ SIDER, CLIGHT
C-----------------------------------------------------------------------
C
      IERR   = 0
      DLYWND = 0.125
      PIBY2N = PI / REAL(2*NFRQS)
      PIBY4N = PI / REAL(4*NFRQS)
C
      DBYREC = SCNHDR(146)
C                                    Calculate how many delay bit shifts
C                                    occurred and the delay error at the
C                                    end of the record.
      DLAST  = DFIRST + DLYROT
      ISGN   = 1.0
      IF (DLYROT.LT.0.0) ISGN = -1.0
      NTURNS = (ABS(DLAST) + DLYWND) / (2.0 * DLYWND)
      DLAST  = DLAST - ISGN*NTURNS*2.0*DLYWND
C                                    Calculate the phase errors at the
C                                    beginning and end of the record in
C                                    each frequency channel.
      DO 10 KFRQ = 1, NFRQS
         THETA0(KFRQ) =  (PIBY4N+REAL(KFRQ-1)*PIBY2N)*DFIRST/DLYWND
         THETA1(KFRQ) =  (PIBY4N+REAL(KFRQ-1)*PIBY2N)*DLAST/DLYWND
 10      THETAN(KFRQ) =  (PIBY4N+REAL(KFRQ-1)*PIBY2N)
C                                       Calculate the FBS errors for the
C                                       simple case of no bit-switch.
      IF (NTURNS.GT.0) GO TO 100
         DO 50 KFRQ = 1, NFRQS
            FBSERR(KFRQ,1) = (SIN(THETA1(KFRQ))-SIN(THETA0(KFRQ)))
     *                     / (THETA1(KFRQ)-THETA0(KFRQ))
            FBSERR(KFRQ,2) = (COS(THETA0(KFRQ))-COS(THETA1(KFRQ)))
     *                     / (THETA1(KFRQ)-THETA0(KFRQ))
 50      CONTINUE
         GO TO 999
C                                       Calculate the FBS error for
C                                       NTURNS of bit-shifting in the
C                                       record time interval.
 100  CONTINUE
      DO 110 NT = 1, NTURNS
         IF (NT.EQ.1)TCROSS = (ISGN*DLYWND-DFIRST)*AVGXC/DLYROT
         IF (NT.GT.1)TCROSS = (ISGN*DLYWND-DFIRST
     *                      + REAL(NT-1)*ISGN*2.0*DLYWND)*AVGXC/DLYROT
C                                       The processor actually updates
C                                       the delay on 60 Hz intervals
C                                       or 5 Hz intervals.
         X60HZ  = TCROSS*60.0
         N60HZ  = X60HZ
         X5HZ = TCROSS * 5.0
         N5HZ = X5HZ
         IF (DBYREC.NE.1)DELTIM = (REAL(N60HZ) + 1.0 - X60HZ) / 60.0
         IF (DBYREC.EQ.1)DELTIM = (REAL(N5HZ)  + 1.0 - X5HZ)  / 5.0
         CORDLY(NT) = ABS(DELTIM * DLYROT / AVGXC)
C                                       Calculate the FBS error in
C                                       this thing.
      DO 110 KFRQ = 1, NFRQS
         CORPHS(KFRQ,NT)
     *      = (PIBY4N+REAL(KFRQ-1)*PIBY2N)*(CORDLY(NT)/DLYWND)
C
         IF (NT.EQ.1) FBSERR(KFRQ,1)
     *      = (SIN(ISGN*(THETAN(KFRQ)+CORPHS(KFRQ,NT)))
     *      - SIN(THETA0(KFRQ)))
         IF (NT.EQ.NTURNS) FBSERR(KFRQ,1) = FBSERR(KFRQ,1)

     *      + (SIN(THETA1(KFRQ))
     *      - SIN(-1.*ISGN*(THETAN(KFRQ)-CORPHS(KFRQ,NT))))
         IF (NT.GT.1) FBSERR(KFRQ,1) = FBSERR(KFRQ,1)
     *      + (SIN(ISGN*(THETAN(KFRQ)+CORPHS(KFRQ,NT)))
     *      - SIN(-1.*ISGN*(THETAN(KFRQ)-CORPHS(KFRQ,NT-1))))
C
         IF (NT.EQ.1) FBSERR(KFRQ,2) = (COS(THETA0(KFRQ))
     *      - COS(ISGN*(THETAN(KFRQ)+CORPHS(KFRQ,NT))))
         IF (NT.EQ.NTURNS) FBSERR(KFRQ,2) = FBSERR(KFRQ,2)
     *      + (COS(-1.*ISGN*(THETAN(KFRQ)-CORPHS(KFRQ,NT)))
     *      -  COS(THETA1(KFRQ)))
         IF (NT.GT.1) FBSERR(KFRQ,2) = FBSERR(KFRQ,2)
     *      + (COS(-1.*ISGN*(THETAN(KFRQ)-CORPHS(KFRQ,NT-1)))
     *      -  COS(ISGN*(THETAN(KFRQ)+CORPHS(KFRQ,NT))))
C
         IF (NT.EQ.1) DENOM(KFRQ)=ISGN*(THETAN(KFRQ)+CORPHS(KFRQ,NT))
     *      - THETA0(KFRQ)
         IF (NT.EQ.NTURNS) DENOM(KFRQ) = DENOM(KFRQ)
     *      + THETA1(KFRQ)+ISGN*(THETAN(KFRQ)-CORPHS(KFRQ,NT))
         IF (NT.GT.1) DENOM(KFRQ) = DENOM(KFRQ)
     *      + ISGN*2.0*THETAN(KFRQ)
C
 110     CONTINUE
C
         DO 150 KFRQ = 1, NFRQS
         FBSERR(KFRQ,1) = FBSERR(KFRQ,1) / DENOM(KFRQ)
         FBSERR(KFRQ,2) = FBSERR(KFRQ,2) / DENOM(KFRQ)
         IF (FBSERR(KFRQ,1).NE.0.0.AND.FBSERR(KFRQ,2).NE.0.0) GO TO 150
            FBSERR(KFRQ,1) = 1.0E+00
            FBSERR(KFRQ,2) = 1.0E-06
 150     CONTINUE
C
 999  CONTINUE
C-----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE PREREC (INUNIT, IRET, TYPE, NREC, NHD, NDD, AT,
     *   SCN1, SCN2, MHD, MDH, MDD, EEOF, SKIP)
C-----------------------------------------------------------------------
C Routine that read and check NRAO/SAO DECODE format prerecords
C for spectral line data.
C
C   Inputs:
C      INUNIT       I      Input unit for data
C      MHD          I      Maximum allowed header size from program
C      MDH          I      Maximum allowed data header size from program
C      MDD          I      Maximum allowed data length from program
C   Outputs:
C      IRET         I      Error flag
C      TYPE         I      Data type  from prerecord
C      NREC         I      Record or channel number from prerecord
C      NHD          I      Length of header section from prerecord
C      NDD          I      Length of data  section  from prerecord
C      AT           D      Time from prerecord of data record
C      SCN1,SCN2    I      Scan number from header prerecord
C      EEOF         L      End of file found during read attempt
C      SKIP         L      Skip to next header
C-----------------------------------------------------------------------
      INTEGER TYPE, NREC, NHD, NDD, SCN1, SCN2, IRET, INUNIT,
     *  MHD, MDH, MDD, IN(4), SC1, SC2, NERR1, NERR2
      LOGICAL    EEOF, SKIP ,EOF
      DOUBLE PRECISION     AT, ATIN
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (SC1,IN(1)), (SC2,IN(2))
      DATA  NERR1, NERR2 /0, 0/
C-----------------------------------------------------------------------
        EEOF = .FALSE.
C
    1   CONTINUE
        CALL READP (INUNIT, TYPE, NREC, NHD, NDD, IN, ATIN, EOF, IRET)
        IF (IRET.EQ.1) GO TO 999
        IF (EOF) THEN
           EEOF = EOF
           GO TO 999
           END IF
C                                       If skip, space forward
        IF(.NOT.SKIP .OR. (TYPE.EQ.10.OR.TYPE.EQ.1)) GO TO 5
        CALL READP (INUNIT, TYPE, NREC, NHD, NDD, IN, ATIN, EOF, IRET)
        IF (IRET.EQ.1) GO TO 999
        IF (EOF) THEN
           EEOF = EOF
           GO TO 999
           END IF
        GO TO 1
    5   CONTINUE
C
        IF(TYPE.EQ.10.OR.TYPE.EQ.1) SCN1 = SC1
        IF(TYPE.EQ.10.OR.TYPE.EQ.1) SCN2 = SC2
        IF(TYPE.NE.10.AND.TYPE.NE.1) AT   = ATIN
C                                       Check record sizes
        IF ((TYPE.NE.10) .AND. (TYPE.NE.1)) GO TO 20
        IF (NDD.NE.0) THEN
           NERR1 = NERR1 + 1
           NDD = 0
           END IF
        IF((NHD.GT.0) .AND. (NHD.LE.MHD)) GO TO 18
        NERR2 = NERR2 + 1
        NHD = MHD
C                                       Reset skip
 18     SKIP = .FALSE.
        GO TO 999
 20     IF (TYPE.GE.1 .AND. TYPE.LE.111)  GO TO 25
C                                       Invalid data type
        WRITE (MSGTXT,2020) TYPE
        IRET = -1
        GO TO 990
 25     CONTINUE
C
        IF(NHD.GE.0 .AND. NHD.LE.MDH) GO TO 30
        WRITE (MSGTXT,2025) NHD
        IRET = -1
        GO TO 990
30      IF(NDD.GE.0 .AND. NDD.LE.MDD) GO TO 999
        WRITE (MSGTXT,2035) NDD
        IRET = -1
        GO TO 990
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 2020 FORMAT ('PREREC: INVALID DATA TYPE # ',I5,' ENCOUNTERED')
 2025 FORMAT ('PREREC: ILLEGAL DATA HEADER SIZE: ',I5)
 2035 FORMAT ('PREREC: ILLEGAL DATA RECORD SIZE: ',I5)
      END
      SUBROUTINE READP (INUNIT, TYPE, NREC, NHD, NDD, IN, TIME, EOF,
     *   IRET)
C-----------------------------------------------------------------------
C  Routine to read the NRAO/SAO format pre-records.
C  All integers are held on disc as local small integers
C
C  Input:
C   INUNIT      I       LUN to read
C  Output:
C   TYPE        I       Record type to be read next
C   NREC        I       Record #
C   NHD         I       # small integers in header part of record
C   NDD         I       # small integers in data part of record
C   IN(4)       I       Small array containing scan numbers for headers
C                       & time for data
C   TIME        D       If type is data record set time
C   EOF         L       .TRUE. if eof found
C   IRET        I       Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   TYPE, NREC, NHD, NDD, IN(4), IRET, INUNIT
      LOGICAL   EOF
      DOUBLE PRECISION TIME
C
      INTEGER   I, INDAT(4), OUTDAT(8), IARR(8), IDF
      REAL      RDUM(4)
      DOUBLE PRECISION DUM(2)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      EOF  = .FALSE.
      READ (INUNIT, ERR=10, END=20) INDAT
      CALL ZVLBIN (8, INDAT, RDUM, DUM, IARR, IRET)
      IF (IRET.NE.0) GO TO 999
      TIME = DUM(2)
      IDF = DPFRMT
      IF (IDF.NE.3) THEN
         DPFRMT = 3
         CALL ZRLR64 (1, 1, TIME, TIME)
         DPFRMT = IDF
         CALL ZR64RL (1, 1, TIME, TIME)
         END IF
      IF (BYTFLP.NE.3) THEN
         CALL ZI16IL (8, 1, INDAT, IARR)
         END IF
      CALL COPY (8, IARR, OUTDAT)
      TYPE = OUTDAT(1)
      NREC = OUTDAT(2)
      NHD = OUTDAT(3)
      NDD = OUTDAT(4)
      DO 100 I = 1, 4
         IN(I) = OUTDAT(I+4)
 100     CONTINUE
      GO TO 999
C
 10   IRET = 1
      WRITE (MSGTXT,1000)
      GO TO 990
C
 20   EOF=.TRUE.
      GO TO 999
C                                      Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READP: ERROR READING DECODE FORMAT PRE-RECORD')
      END
      SUBROUTINE READH (IN, NI2, ARRAY, NSMALL, ARRSML, DARR, RARR,
     *   EOF, IRET)
C-----------------------------------------------------------------------
C Routine to read a DECODE style header record and perform all the
C necessary translations.
C
C Inputs:
C    IN          I         lun to read
C    NI2         I         # small integers in the array to be read
C    NSMALL      I         NI2 / 2 for array decleration
C Outputs:
C    ARRAY(*)    I         Header record
C    ARRSML(*)   I         Dummy array contining small integers
C    DARR(*)     D         Double precision version of integer array
C    RARR(*)     R         Real version of integer array
C    EOF         L         .TRUE. if eof found
C    IRET        I         Error code, 0 => OK
C-----------------------------------------------------------------------
      INTEGER  NI2, ARRAY(NI2), IRET, IN, NSMALL, ARRSML(NSMALL),
     *   IARR(1024), ISF, IDF
      LOGICAL  EOF
      REAL     RARR(*)
      DOUBLE PRECISION DARR(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      EOF  = .FALSE.
      READ (IN, ERR=10, END=20) ARRSML
      CALL ZVLBIN (NI2, ARRSML, RARR, DARR, IARR, IRET)
      IF (IRET.NE.0) GO TO 999
CC      CALL ZIPACK ('STOL', NI2, ARRAY, 1, ARRSML)
      CALL COPY (NI2, IARR, ARRAY)
C                                       Am I on a Convex or a VAX?
      ISF = SPFRMT
      IDF = DPFRMT
      IF (IDF.NE.3) THEN
         DPFRMT = 3
C                                       RA, Dec etc
         CALL ZRLR64 (5, 1, DARR(4), DARR(4))
C                                       Stat. A parms
         CALL ZRLR64 (6, 1, DARR(11), DARR(11))
C                                       Stat. B parms
         CALL ZRLR64 (6, 1, DARR(20), DARR(20))
C                                       GASTM
         CALL ZRLR64 (1, 1, DARR(28), DARR(28))
C                                       Hdrfrq
         CALL ZRLR64 (1, 1, DARR(30), DARR(30))
C                                       ifreqo
         CALL ZRLR64 (1, 1, DARR(43), DARR(43))
         DPFRMT = IDF
         CALL ZR64RL (5, 1, DARR(4), DARR(4))
         CALL ZR64RL (6, 1, DARR(11), DARR(11))
         CALL ZR64RL (6, 1, DARR(20), DARR(20))
         CALL ZR64RL (1, 1, DARR(28), DARR(28))
         CALL ZR64RL (1, 1, DARR(30), DARR(30))
         CALL ZR64RL (1, 1, DARR(43), DARR(43))
         END IF
      IF (ISF.NE.2) THEN
         SPFRMT = 2
         CALL ZRLR32 (1, 1, RARR(33), RARR(33))
         CALL ZRLR32 (1, 1, RARR(51), RARR(51))
         CALL ZRLR32 (3, 1, RARR(57), RARR(57))
C                                       Start & stop
         CALL ZRLR32 (2, 1, RARR(3), RARR(3))
C                                       Cvel
         CALL ZRLR32 (1, 1, RARR(60), RARR(60))
C                                       Avg. times & BW
         CALL ZRLR32 (3, 1, RARR(63), RARR(63))
         SPFRMT = ISF
         CALL ZR32RL (1, 1, RARR(33), RARR(33))
         CALL ZR32RL (1, 1, RARR(51), RARR(51))
         CALL ZR32RL (3, 1, RARR(57), RARR(57))
         CALL ZR32RL (2, 1, RARR(3), RARR(3))
         CALL ZR32RL (1, 1, RARR(60), RARR(60))
         CALL ZR32RL (3, 1, RARR(63), RARR(63))
         END IF
      GO TO 999
C
 10   CONTINUE
      IRET = 1
      WRITE (MSGTXT,1000)
      GO TO 990
C
 20   EOF=.TRUE.
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READH: ERROR READING DECODE FORMAT HEADER')
      END
      SUBROUTINE READD (IN, NI2, ARRAY1, NSMALL, ARRSML, DARR, RARR,
     *   NR4, ARRAY2, EOF, IRET)
C-----------------------------------------------------------------------
C  Routine to read a data record from an NRAO/SAO DECODE format file.
C
C  Inputs:
C    IN          I         LUN of file
C    NI2         I         # I*2 words in header portion of record
C    NSMALL      I         NI2/2 for array decleration
C    NR4         I         # real words in data portion of record
C  Outputs:
C    ARRAY1(*)   I         Integer header portion of data record
C    ARRSML(*)   I         Array contining small integers
C    DARR(*)     D         Double precision version of integer*2 array
C    RARR(*)     R         Real version of integer*2 array
C    ARRAY2(*)   R         Real data portion of record
C    EOF         L         .TRUE. if eof during read
C    IRET        I         Error flag
C-----------------------------------------------------------------------
      INTEGER   NI2, NR4, ARRAY1(NI2), IRET, IN, NSMALL,
     *   ARRSML(NSMALL), ISF, IDF
      REAL      ARRAY2(NR4), RARR(*)
      DOUBLE PRECISION DARR(*)
      LOGICAL   EOF
C
      INTEGER  IARR(1024)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IRET = 0
      EOF  = .FALSE.
      READ (IN, ERR=10, END=20) ARRSML, ARRAY2
      CALL ZVLBIN (NI2, ARRSML, RARR, DARR, IARR, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL ZIPACK ('STOL', NI2, ARRAY1, 1, ARRSML)
C                                       Am I on a Convex or a VAX?
      ISF = SPFRMT
      IDF = DPFRMT
C                                       Delay etc
      IF (IDF.NE.3) THEN
         DPFRMT = 3
         CALL ZRLR64 (3, 1, DARR(4), DARR(4))
         DPFRMT = IDF
         CALL ZR64RL (3, 1, DARR(4), DARR(4))
         END IF
C                                       U,V,times
C                                       Data
      IF (ISF.NE.2) THEN
         SPFRMT = 2
         CALL ZRLR32 (4, 1, RARR(3), RARR(3))
         CALL ZRLR32 (NR4, 1, ARRAY2, ARRAY2)
         SPFRMT = ISF
         CALL ZR32RL (4, 1, RARR(3), RARR(3))
         CALL ZR32RL (NR4, 1, ARRAY2, ARRAY2)
         END IF
      GO TO 999
C
 10   IRET = 1
      WRITE (MSGTXT, 1000)
      GO TO 990
C
 20   EOF=.TRUE.
      GO TO 999
C                                          Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READD: ERROR READING DECODE FORMAT DATA RECORD')
      END
      SUBROUTINE AVGSET (RA,DEC,X,Y,Z,C,W,R)
C-----------------------------------------------------------------------
C     THIS ROUTINE PREPARES CONSTANTS FOR DELAY CALCULATIONS
C     ALL DELAYS WILL BE IN MICROSECONDS. FIXED UP A BIT FOR AIPS.
C     R.C. WALKER, J.M. BENSON
C-----------------------------------------------------------------------
      DOUBLE PRECISION  R(6),RA,DEC,X,Y,Z,C,W,CM,BLONG,BEQ,COSRA,SINRA,
     *        COSDEC,SINDEC,COSLON,SINLON
C
      CM = C * 1.D-6
      BLONG = ATAN2 (Y,X)
      BEQ   = SQRT  (X*X + Y*Y)   / CM
      COSRA = COS (RA)
      SINRA = SIN (RA)
      COSDEC = COS (DEC)
      SINDEC = SIN (DEC)
      COSLON = COS (BLONG)
      SINLON = SIN (BLONG)
C
      R(1) = BEQ * COSDEC * (COSRA*COSLON - SINRA*SINLON)
      R(2) = BEQ * COSDEC * (COSRA*SINLON + SINRA*COSLON)
      R(3) = Z * SINDEC / CM
      R(4) = W * R(1)
      R(5) = W * R(2)
      R(6) = W * W
C
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE AVGDLY (GST,R,DLY0,DLY1,DLY2,RATE1,RATE2,DRATE)
C-----------------------------------------------------------------------
C     THIS ROUTINE CALCULATES DELAY AND DELAY RATE TO THE CENTER
C     OF THE EARTH TO ZEROTH, FIRST (SHOULD DUPLICATE PROCESSOR),
C     AND SECOND ORDER.  IT ALSO DELIVERS THE RATE OF CHANGE OF DELAY
C     RATE.  DLY0 AND RATE1 ARE THE EXACT VALUES IF GST IS THE
C     WAVEFRONT ARRIVAL TIME AT THE STATION.
C     ALL TIME AND DELAY UNITS ARE MICROSECONDS. R.C. WALKER
C     FIXED UP FOR AIPS, 20 FEB 1984. J.M. BENSON
C-----------------------------------------------------------------------
      DOUBLE PRECISION
     *   R(6),GST,DLY0,DLY1,DLY2,RATE1,RATE2,DRATE,COSGST,SINGST,
     *         A1,A2,A3,T1,T2
C
      COSGST = COS(GST)
      SINGST = SIN(GST)
C
      A1 = R(1)*COSGST + R(2)*SINGST + R(3)
      A2 = R(5)*COSGST - R(4)*SINGST
      A3 = R(6) * (R(3) - A1)
C
C                                        SAVE MULTIPLE MULTIPLIES LATER.

      T1 = A1*A3
      T2 = A2*A2
      DLY0 = A1
      DLY1 = A1 - A2*A1
      RATE1 = A2
C
      DLY2 = DLY1 + T2*A1 + 0.5D0*A1*T1
      RATE2 = RATE1 - T2 - T1
      DRATE = A3
C
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE ECTIME (AT, MACDEL, MACDAL, ITFLAG, IGAST0, SIDR,
     *   PACLK, MICRAD,  PABR)
C-----------------------------------------------------------------------
C   THIS ROUTINE TAKES THE INPUT TIME, THE MACHINE DELAY, AND THE
C   FLAGS FROM THE SOC PARAMETER AND FINDS THE CENTER EARTH TIME AND
C   THE MACHINE DELAY TO ADD TO IT.
C   THE NATURE OF THE INPUT TIME IS DETERMINED
C   BY ITFLAG:
C      =0  =>  PROCESSOR STATION A TIME
C      =1  =>  CENTER EARTH TIME
C      =2  =>  BASELINE STATION A TIME
C
C   PABR CONTAINS THE APPROPRIATE NUMBERS FOR
C   CORRECTING EITHER OF THE STATION A TIMES. R.C. WALKER
C
C   ECTIME IS CALLED ZTIME IN THE NRAO VLBI SOFTWARE. J.M. BENSON
C-----------------------------------------------------------------------
      DOUBLE PRECISION     AT,MACDEL,MACDAL,IGAST0,SIDR,PACLK,MICRAD,
     *           PABR(6),PGST,
     *           IQDLY0,IQDLY1,IQDLY2,IQRT1,IQRT2,IQDRT
      INTEGER    ITFLAG
C-----------------------------------------------------------------------
           MACDAL = 0.D0
           IF(MACDEL.EQ.0.D0.AND.ITFLAG.EQ.1)  GO TO 512
                PGST = IGAST0 + SIDR * (AT +  PACLK*MICRAD)
                CALL AVGDLY(PGST,PABR,IQDLY0,IQDLY1,IQDLY2,IQRT1,IQRT2,
     *                      IQDRT)
                IF (ITFLAG.EQ.0  .OR.  ITFLAG.EQ.2)
     *                   AT = AT  +  (IQDLY0 + PACLK)* MICRAD
C                            FIND CENTER EARTH MACHINE DELAY TO ADD
C                            IN AVGND
                MACDAL = MACDEL + IQDRT*1.D-6*MACDEL
  512      CONTINUE
C
      RETURN
      END
      SUBROUTINE SETCAL (IRET)
C-----------------------------------------------------------------------
C Routine to set up and write the CL table
C Output:
C   IRET    I    0, => normal
C                1, => fatal error.
C-----------------------------------------------------------------------
      CHARACTER CSRC*8, CANAME*8, CBNAME*8
      INTEGER   IERR, HDR(256), I, IYEAR, IDAY, ISWX, J, IRET, JSTART,
     *   JSTOP, ISTNA, ISTNB, REFA(1,2)
      REAL      IBAND, IEPOCH, IAXOFF, IBXOFF,
     *          ISTART,  ISTOP,  ACLKGD,  ACLKPD,  BCLKGD, BCLKPD,
     *          ADOPOF, BDOPOF, HDR4(128)
      REAL      ATMOS, DATMOS, MBDELY(2), CLOCK(2), IFR,
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(1,2), CIMAG(1,2),
     *   DELAY(1,2), RATE(1,2), WEIGHT(1,2), DOPOFF(2)
      DOUBLE PRECISION    GEODLY(3)
      DOUBLE PRECISION IFREQO, IFREQA, IFREQB, IAPPAR, ISRC,
     *          IRAEPO, IDECEP, IRAAPP, IDECAP, IANAME, IBNAME, MACDEL,
     *          IAX, IAY, IAZ, IBX, IBY, IBZ, ICLKA, ICLKB, IDOPA,
     *          IDOPB, ACALR(6), BCALR(6),
     *          TIME, TIMRAD, DAYS, GST,  ADLY, ARATE,
     *          ADLY0, ADLY1, ADLY2, ART1, ART2, ADRT, BDLY, BRATE,
     *          BDLY0, BDLY1, BDLY2, BRT1, BRT2, BDRT,
     *          ATIME, BTIME, FRQDIF, HDR8(64)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DREC.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'VLBIN.INC'
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'DKIN.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Values from VLBI scan
C                                       header record.
C                                       Integer equiv
      EQUIVALENCE
     *         (IYEAR, HDR(1)),  (IDAY,  HDR(2)),  (ISWX,  HDR(3))
C                                       Real equiv.
      EQUIVALENCE
     *         (ISTART,HDR4(3)),  (ISTOP, HDR4(4)),
     *         (IEPOCH,HDR4(17)), (IAXOFF,HDR4(33)),
     *         (IBXOFF,HDR4(51)), (IBAND, HDR4(65))
C                                       D.P. equiv
      EQUIVALENCE
     *         (ISRC,  HDR8(3)),  (IRAEPO,HDR8(4)), (IDECEP,HDR8(5)),
     *         (IRAAPP,HDR8(6)),  (IDECAP,HDR8(7)), (IAPPAR,HDR8(8)),
     *         (IANAME,HDR8(10)), (IAX,   HDR8(11)),(IAY,   HDR8(12)),
     *         (IAZ,   HDR8(13)), (ICLKA, HDR8(14)),(IFREQA,HDR8(15)),
     *         (IDOPA, HDR8(16)),
     *         (IBNAME,HDR8(19)), (IBX,   HDR8(20)),(IBY,   HDR8(21)),
     *         (IBZ,   HDR8(22)), (ICLKB, HDR8(23)),(IFREQB,HDR8(24)),
     *         (IDOPB, HDR8(25)),
     *         (IFREQO,HDR8(43))
C
      DATA      MACDEL/0.0D0/
      DATA GEODLY /3*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /8*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, DELAY, RATE, WEIGHT /8*0.0/
      DATA CREAL /2*1.0/
      DATA IFR /0.0/
      DATA REFA /0,0/
C-----------------------------------------------------------------------
C
      IERR   = 0
C
      DO 10 I = 1, 256
         HDR(I) = SCNHDR(I)
 10      CONTINUE
      DO 11 I = 1, 128
         HDR4(I) = SCN4(I)
 11      CONTINUE
      DO 12 I = 1, 64
         HDR8(I) = SCN8(I)
 12      CONTINUE
      DAYS   = IDAY - REFDAY
      CALL H2CHR (8, 1, ISRC, CSRC)
      CALL H2CHR (8, 1, IANAME, CANAME)
      CALL H2CHR (8, 1, IBNAME, CBNAME)
C
      IDSOU = 1
      DO 20 J = 1, NKSRCS
         IF (CSRC.EQ.KEYSRC(J)) IDSOU = J
 20      CONTINUE
C                                       Determine any additional
C                                       freq. offsets
      FRQDIF = 0.D0
      FRQDIF = (IFREQO*1.0D6) - SRCFRQ(IDSOU)
C
      ISTNA = 1
      ISTNB = 1
      DO 30 J = 1, NUMANT
         IF (CANAME.EQ.KEYSTN(J)) ISTNA = J
         IF (CBNAME.EQ.KEYSTN(J)) ISTNB = J
 30      CONTINUE
C
      CALL AVGSET (IRAAPP,IDECAP,IAX,IAY,IAZ,CLIGHT,OMEGA,ACALR)
      CALL AVGSET (IRAAPP,IDECAP,IBX,IBY,IBZ,CLIGHT,OMEGA,BCALR)
C
      JSTART = (ISTART / (TWOPI * TIMEI))
      JSTOP  = (ISTOP  / (TWOPI * TIMEI)) + 1
C
      DO 100 J = JSTART, JSTOP, 1
         TIME  = J * TIMEI
         TIMRAD = TIME * TWOPI
         GST   = (TIMRAD + MACDEL) * SIDER + GAST0
         GST   = MOD (GST,TWOPI)
C
         CALL AVGDLY (GST,ACALR,ADLY0,ADLY1,ADLY2,ART1,ART2,ADRT)
         CALL AVGDLY (GST,BCALR,BDLY0,BDLY1,BDLY2,BRT1,BRT2,BDRT)
C
         IF (AEQPA .OR. JISOC.NE.0) GO TO 40
            ADLY  = ADLY1
            BDLY  = BDLY1
            ARATE = ART1
            BRATE = BRT1
            GO TO 60
 40      IF (JISOC.NE.0) GO TO 50
            ADLY  = ADLY2
            BDLY  = BDLY2
            ARATE = ART2
            BRATE = BRT1
            GO TO 60
 50      ADLY  = ADLY2
         BDLY  = BDLY2
         ARATE = ART2
         BRATE = BRT2
C
 60      CONTINUE
         CTIME  = DAYS + TIME
C                                       Beginning and end
         IF (J.EQ.JSTART) CTIME = DAYS + (ISTART / TWOPI)
         IF (J.EQ.JSTOP) CTIME = DAYS + (ISTOP / TWOPI)
C                                       IAT correction
         CTIME = CTIME + IATUT1 / 86400.0D0
         ADLY   = ADLY / 1.0D06
         BDLY   = BDLY / 1.0D06
         ATIME  = GST / TWOPI + ADLY / (86400.0*SIDER)
         BTIME  = GST / TWOPI + BDLY / (86400.0*SIDER)
         ACLKGD = ICLKA / 1.0D06
         BCLKGD = ICLKB / 1.0D06
         ACLKPD = ACLKGD
         BCLKPD = BCLKGD
         ADOPOF  = IDOPA + FRQDIF
         BDOPOF  = IDOPB + FRQDIF
         SUBA = 1
         IFR = 0.0
         GEODLY(1) = ADLY
         GEODLY(2) = ARATE
         GEODLY(3) = ADRT
         DOPOFF(1) = ADOPOF
         CLOCK(1) = ACLKGD
C
         CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *      NUMPOL, NUMIF, CTIME, TIMEI, IDSOU, ISTNA, SUBA, CURFQI,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *      CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *      WEIGHT, REFA, IRET)
         IF (IRET.NE.0) GO TO 999
C
         GEODLY(1) = BDLY
         GEODLY(2) = BRATE
         GEODLY(3) = BDRT
         DOPOFF(1) = BDOPOF
         CLOCK(1) = BCLKGD
         CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *      NUMPOL, NUMIF, CTIME, TIMEI, IDSOU, ISTNB, SUBA, CURFQI,
     *      IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *      CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *      WEIGHT, REFA, IRET)
         IF (IRET.NE.0) GO TO 999
C
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE JULIAN (YEAR,DAY,UTC, JD)
C-----------------------------------------------------------------------
C   JULIAN calcualtes the Julian day number from the year, day of the
C   year and the UTC in decimal days.
C   Inputs:
C       YEAR    I   Year
C       DAY     I   Day of year.
C       UTC     D   UTC in decimal days.
C   Output:
C       JD      D   Julian date.
C   J. Benson   20 June, 1986
C-----------------------------------------------------------------------
      INTEGER   YEAR, DAY, NYRM1,NYRM14,ICXX4,ICXX
      DOUBLE PRECISION    JD,UTC
C-----------------------------------------------------------------------
C
      NYRM1  = YEAR - 1
      ICXX   = NYRM1 / 100.0
      NYRM14 = NYRM1 / 4
      ICXX4  = ICXX  / 4
      JD     = 1721425.0 + 365.0 * NYRM1 + NYRM14 - ICXX + ICXX4
      JD     = JD + DAY + MOD(UTC,1.0D0)
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE DTRANS(DATA, WORK, NCORR, ISB)
C-----------------------------------------------------------------------
C  This routine transforms a complex correlation function DATA of
C  length NCORR to a single sideband, complex spectrum of length
C  NSPEC=NCORR/2. If NSPEC is a power of two a fast, vectorized FFT is
C  used; if not a smoewhat slower routine is used.
C  Is done this way in order to avoid grabbing the AP on AP machines.
C  Inputs:
C      DATA(2,NCORR)    R     array containing the delay lag function
C      WORK(2,NCORR)    R     work array
C      NCORR            I     length of complex correlation function
C      ISB              I     sideband of output data (1=upper,-1=lower)
C  Outputs:
C      DATA(2,NSPEC)    R     array containing the transformed data
C  P. Diamond   18 March 1987
C-----------------------------------------------------------------------
      INTEGER   NCORR,ISB,K,I,GAMMA,NSPEC
      INTEGER   ISIGN
      REAL      DATA(2,*),WORK(*),TEMP1,TEMP2
      LOGICAL POWTWO
C-----------------------------------------------------------------------
C                            Determine if NSPEC is power of two
      NSPEC = NCORR / 2
      POWTWO = .FALSE.
      DO 5 GAMMA = 2,15
 5       IF ((2**GAMMA) .EQ. NSPEC) POWTWO = .TRUE.
C
C                            Rearrange the correlation function so that
C                            the center channel (ncorr/2 + 1) winds up
C                            in the first element of the array to be
C                            transformed.  the correlation function
C                            to be transformed should be (nspec * 2)
C                            points long.  use the work array for
C                            temporary storrage for convenience.
      DO 10 I = 1,NSPEC
         K = I + NSPEC
         TEMP1 = DATA(1,I)
         TEMP2 = DATA(2,I)
         DATA(1,I) = DATA(1,K)
         DATA(2,I) = DATA(2,K)
         DATA(1,K) = TEMP1
   10    DATA(2,K) = TEMP2
C
C                            transform
C                            isign determines which sideband will end
C                            up in first half of data
      ISIGN = 1
      IF(ISB.EQ.-1) ISIGN = -1
      CALL FOURG(DATA, NCORR, ISIGN, WORK)
      RETURN
      END
      SUBROUTINE ACTRAN (DATA, WORK, NAUTO, NFREQ, ISB, ACBIAS,
     *   NACSCN, IA1, IA2)
C-----------------------------------------------------------------------
C  Routine to FFT the autocorrelation functions generated by the
C  NRAO MkII correlator.
C  Inputs:
C     DATA(2,NAUTO)     R     array containing the two AC functions
C     WORK(2,NAUTO)     R     work array
C     NFREQ             I     length of output spectra required
C     NAUTO             I     length of each AC function
C     ISB               I     Sideband, 1 => upper
C                                      -1 => lower
C     ACBIAS            R(*)  Average ac bias
C     NACSCN            I     # ac bias points
C     IA1               I     First antenna to be FFT'd
C     IA2               I     Second antenna to be FFT'd
C  Ouput:
C     DATA(2,NFREQ)     R     array containing the spectra
C  P.Diamond  23 March 1987
C-----------------------------------------------------------------------
      REAL   DATA(2,*),WORK(2,*),PIE,XXX,ANORM,AC,ACBIAS(*)
      INTEGER   NAUTO,NFREQ,NPTS,NSTP,M,I,NDUMMY,NBIAS,
     *   NBIAS1, NACL, K, NMEAN, ISB, NACSCN(*), IA1, IA2
      INTEGER   I4SGN,I4N4
C-----------------------------------------------------------------------
C                                       Set variables
      PIE = 4.0 * ATAN(1.0)
      NPTS = NFREQ * 2
C                                       Where does the padding
C                                       start?
      NACL = 0
      DO 50 I = NAUTO,1,-1
         IF ((DATA(1,I).EQ.0.0) .AND.
     *      (DATA(2,I).EQ.0.0)) NACL = I
 50   CONTINUE
      NACL = NACL - 1
      IF (NACL.LE.0) NACL = NAUTO
C
      K = NACL + 1
      NBIAS = (NAUTO/2) + 1
      NBIAS1 = NBIAS + 1
C
      DO 300 M = 1,2
C                                       Zero pad array
         NSTP = NPTS - NACL + 1
         DO 100 I=K,NSTP
            WORK(M,I) = 0.
 100        CONTINUE
C                                       Determine bias
         XXX = 0.
         DO 150 I=NBIAS1,NACL
            XXX = XXX + DATA(M,I)
 150        CONTINUE
         NMEAN = NACL - NBIAS1 + 1
         IF (NMEAN.GT.0) THEN
            XXX = XXX / NMEAN
            END IF
         ANORM = PIE/2. / ( 1. - XXX)
         IF (M.EQ.1) THEN
            ACBIAS(IA1) = ACBIAS(IA1) + XXX
            NACSCN(IA1) = NACSCN(IA1) + 1
         ELSE IF (M.EQ.2) THEN
            ACBIAS(IA2) = ACBIAS(IA2) + XXX
            NACSCN(IA2) = NACSCN(IA2) + 1
            END IF
C                                       Set first lag to 1.0
         WORK(M,1) = 1.0
C                                       Correct for clipping
C                                       bias, normalize and
C                                       mirror the data
         NDUMMY = NPTS + 2
         DO 200 I=2,NAUTO
            AC = SIN( (DATA(M,I-1) - XXX) * ANORM )
            IF (I.GT.NACL) AC = 0.0
            WORK(M,I) = AC
            WORK(M,NDUMMY-I) = AC
 200        CONTINUE
 300     CONTINUE
C
      DO 400 I = 1, NPTS
         DATA (1,I) = WORK(1,I)
         DATA (2,I) = WORK(2,I)
 400     CONTINUE
C                                            FFT both functions
C                                            simultaneously.
      I4SGN = ISB
      I4N4 = NPTS
      CALL FOURG (DATA, I4N4, I4SGN, WORK)
C
      RETURN
      END
      SUBROUTINE FOURG (DATA, N, ISIGN, WORK)
C-----------------------------------------------------------------------
C     Cooley-Tukey fast fourier transform.
C     One-dimensional transform of complex data, arbitrary number of
C     points.  N points can be transformed in time proportional to
C     N*log(N) (for N non-prime), whereas other methods take N**2 time.
C     furthermore, because fewer arithmetic operations are performed,
C     less error is built up.
C     The transform done is--
C           Real*4 data(n),transform(n),work(n)
C           Complex data,transform,work
C       transform(k) = sum(data(j)*exp(isign*2*pi*i*(j-1)*(k-1)/n)),
C     summed from j = 1 to n for all k from 1 to N.  The transform
C     values are returned to data, replacing the input.  N may be any
C     positive number, but it should be non-prime for speed.
C     ISIGN = +1 or -1.
C     A -1 transform followed by a +1 one (or vice versa)
C     returns n times the original data.  work is a one-dimensional
C     complex array of length n used for working storage.
C     running time is proportional to n * (sum of the prime factors of
C     n).  for example, n = 1960, time is to * 1960 * (2+2+2+5+7+7).
C     naive methods directly implementing the summation run in time
C     proportional to n**2.  an upper bound for the rms relative error
C     is 3 * 2**(-b) * sum(f**1.5), where b is the number of bits in
C     the floating point fraction and the sum is over the prime
C     factors of n.
C     Written by Norman Brenner, Mit, August 1968.
C-----------------------------------------------------------------------
      REAL    DATA(*), WORK(*), TWOPI, THETA, SINTH, ROOTR,
     *   ROOTI, WSTPR, WSTPI, WMINR, WMINI, TEMPR, TEMPI,
     *   WR, WI, SUMR, SUMI, TEMP
      INTEGER   IFACT(32), N, ISIGN, IF, NPART, ID, IDIV, IQUOT,
     *   NFACT, IP0, IP3, IWORK, I3REV, I3, IP2, IP1,
     *   IFCUR, I1, J0, J1, IWMAX, I2MAX, I2
C-----------------------------------------------------------------------

      TWOPI = 6.2831853072 * REAL(ISIGN)
C                                           factor n into its prime
C                                           factors, nfact in number.
C                                           for example, for n = 1960,
C                                           nfact = 6 and ifact(if) = 2,
C                                           2, 2, 5, 7 and 7.
      IF = 0
      NPART = N
      DO 50 ID = 1, N, 2
      IDIV = ID
      IF (ID-1) 10,10,20
 10   IDIV = 2
 20   IQUOT = NPART / IDIV
      IF (NPART - IDIV * IQUOT) 40,30,40
 30   IF = IF + 1
      IFACT(IF) = IDIV
      NPART = IQUOT
      GO TO 20
 40   IF (IQUOT-IDIV) 60,60,50
 50   CONTINUE
 60   IF (NPART-1) 80,80,70
 70   IF = IF + 1
      IFACT(IF) = NPART
 80   NFACT = IF
C                                       shuffle the data array by
C                                       reversing the digits of the
C                                       index. Replace data(i) by
C                                       data(irev) for all i from 1
C                                       to n.  irev-1 is the integer
C                                       whose digit representation
C                                       in the multi-radix notation
C                                       of factors ifact(if) is the
C                                       reverse of the representatn
C                                       of i-1. For example, if all
C                                       ifact(if) = 2, then for i-1
C                                       = 11001, irev-1 = 10011. A
C                                       work array of length n is
C                                       needed.
      IP0 = 2
      IP3 = IP0 * N
      IWORK = 1
      I3REV = 1
      DO 110 I3 = 1, IP3, IP0
      WORK(IWORK) = DATA(I3REV)
      WORK(IWORK+1) = DATA(I3REV+1)
      IP2 = IP3
      DO 100 IF = 1, NFACT
      IP1 = IP2 / IFACT(IF)
      I3REV = I3REV + IP1
      IF (I3REV-IP2) 110,110,90
 90   I3REV = I3REV - IP2
 100  IP2 = IP1
 110  IWORK = IWORK + IP0
      IWORK = 1
      DO 120 I3 = 1, IP3, IP0
      DATA(I3) = WORK(IWORK)
      DATA(I3+1) = WORK(IWORK+1)
 120  IWORK = IWORK + IP0
C                                       phase-shifted fourier transform
C                                       of length ifact(if).
C                                       iprod=ip1/ip0
C                                       irem=n/(ifact(if)*iprod)
C                                       real data(iprod,ifact(if),irem),
C                                       work(ifact(if))
C                                       complex data,work
C                                       data(i1,j2,i3) =
C                                         sum(data(i1,i2,i3)
C                                         * w**(i2-1)), summed over
C                                       i2 = 1 to ifact(if) for all i1
C                                       from 1 to iprod, j2 from 1 to
C                                       ifact(if) and i3 from 1 to irem.
C                                       w = exp(isign*2*pi*i*
C                                         (i1-1+iprod*(j2-1))/
C                                         (iprod*ifact(if))).
      IF = 0
      IP1 = IP0
 130  IF (IP1-IP3) 140,240,240
 140  IF = IF + 1
      IFCUR = IFACT(IF)
      IP2 = IP1 * IFCUR
      THETA = TWOPI / REAL(IFCUR)
      SINTH = SIN(THETA/2.)
      ROOTR = -2. * SINTH * SINTH
      ROOTI = SIN(THETA)
      THETA = TWOPI / REAL(IP2/IP0)
      SINTH = SIN(THETA / 2.)
      WSTPR = -2. * SINTH * SINTH
      WSTPI = SIN(THETA)
      WMINR = 1.
      WMINI = 0.
      DO 230 I1 = 1, IP1, IP0
      IF (IFCUR-2) 150,150,170
 150  DO 160 I3 = I1, IP3, IP2
      J0 = I3
      J1 = I3 + IP1
      TEMPR = WMINR * DATA(J1) - WMINI * DATA(J1+1)
      TEMPI = WMINR * DATA(J1+1) + WMINI * DATA(J1)
      DATA(J1) = DATA(J0) - TEMPR
      DATA(J1+1) = DATA(J0+1) - TEMPI
      DATA(J0) = DATA(J0) + TEMPR
 160  DATA(J0+1) = DATA(J0+1) + TEMPI
      GO TO 220
 170  IWMAX = IP0 * IFCUR
      DO 210 I3 = I1, IP3, IP2
      I2MAX = I3 + IP2 - IP1
      WR = WMINR
      WI = WMINI
      DO 200 IWORK = 1, IWMAX, IP0
      I2 = I2MAX
      SUMR = DATA(I2)
      SUMI = DATA(I2+1)
 180  I2 = I2 - IP1
      TEMPR = SUMR
      SUMR = WR * SUMR - WI * SUMI + DATA(I2)
      SUMI = WR * SUMI + WI * TEMPR + DATA(I2+1)
      IF (I2-I3) 190, 190, 180
 190  WORK(IWORK) = SUMR
      WORK(IWORK+1) = SUMI
      TEMP = WR
      WR = WR * ROOTR - WI * ROOTI + WR
 200  WI = TEMP * ROOTI + WI * ROOTR + WI
      IWORK = 1
      DO 210 I2 = I3, I2MAX, IP1
      DATA(I2) = WORK(IWORK)
      DATA(I2+1) = WORK(IWORK+1)
 210  IWORK = IWORK + IP0
 220  TEMPR = WMINR
      WMINR = WMINR * WSTPR - WMINI * WSTPI + WMINR
 230  WMINI = TEMPR * WSTPI + WMINI * WSTPR + WMINI
      IP1 = IP2
      GO TO 130
 240  RETURN
      END
      SUBROUTINE REST (INFREQ, RESTFQ)
C-----------------------------------------------------------------------
C  Routine to determine the rest frequency from the observed frequency.
C  List will obviously need extending, and possibly modifed to attempt
C  to redo the calculation that derived the observing freq. originally.
C  INPUT:
C    INFREQ       D    Observed frequency (Hz)
C  OUTPUT:
C    RESTFQ       D    Line rest frequency (Hz)
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQ, INFREQ, RESTFQ
C-----------------------------------------------------------------------
      RESTFQ = 0.0D0
      FREQ = INFREQ / 1.0D6
C                                             H freq.
      IF (FREQ .GE. 100.0 .AND. FREQ .LT. 1500.0) RESTFQ=1420.40575D0
C                                             OH freq.
      IF (FREQ .GE. 1600.0.AND. FREQ .LE. 1620.0) RESTFQ=1612.2310D0
      IF (FREQ .GE. 1660.0.AND. FREQ .LE.1666.4D0) RESTFQ=1665.4018D0
      IF (FREQ .GT.1666.4D0.AND. FREQ .LE. 1670.0) RESTFQ=1667.3590D0
      IF (FREQ .GT. 1710.0.AND. FREQ .LE. 1730.0) RESTFQ=1720.5300D0
C                                             Excited state OH
      IF (FREQ .GT. 3500.0.AND. FREQ .LE. 5000.0) RESTFQ=4765.56200D0
      IF (FREQ .GE. 6027.0.AND. FREQ .LE. 6033.0) RESTFQ=6030.7470D0
      IF (FREQ .GT. 6033.0.AND. FREQ .LE. 6038.0) RESTFQ=6035.0930D0
C                                             H2O freq.
      IF (FREQ.GE.22200.0.AND.FREQ.LE.22270.0) RESTFQ=22235.080D0
C                                             SiO freq
      IF (FREQ.GE.42000.0.AND.FREQ.LE.45000.0) RESTFQ=43122.027D0
      IF (RESTFQ .GT. 0.0) GO TO 999
C
      RESTFQ = 0.0D0
C
  999 RESTFQ = RESTFQ * 1.0D6
      RETURN
      END
      SUBROUTINE VLBNAM (LUN, INFILE, FORM, IERR)
C-----------------------------------------------------------------------
C   Open a formatted or unformatted binary file for Fortran I/O.
C
C   Inputs:
C      LUN      I           Logical unit number
C      INFILE   C*48        User provided file name
C      FORM     L           TRUE => formatted
C   Outputs:
C      IERR     I           Error return code:
C                              0 => no error
C                              1 => error
C
C   Generic UNIX version.
C-----------------------------------------------------------------------
      INTEGER   LUN
      CHARACTER INFILE*48
      LOGICAL   FORM
      INTEGER   IERR
C
      INTEGER   XLNB, ITRIM, IOSVAL
      LOGICAL   EXISTS
      CHARACTER FILSPC*256
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Assumme success.
      IERR = 0
      EXISTS = .FALSE.
C                                       Determine full name
      CALL ZFULLN (INFILE, ' ', ' ', FILSPC, IERR)
      IF (IERR.NE.0) THEN
         IERR = 4
         GO TO 999
         END IF
      XLNB = ITRIM (FILSPC)
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
      WRITE (MSGTXT,1001) FILSPC(1:MIN (56, XLNB))
      CALL MSGWRT (2)
      IF (XLNB.GT.56) THEN
         WRITE (MSGTXT,1001) FILSPC(57:XLNB)
         CALL MSGWRT (2)
         END IF
C                                       Check for existence.
      INQUIRE (FILE=FILSPC(1:XLNB), EXIST=EXISTS)
C                                       Found it?
      IF (.NOT.EXISTS) THEN
C                                       Can't find it.
         IERR = 1
         WRITE (MSGTXT,1030)
         CALL MSGWRT (6)
      ELSE
C                                       Found it, now open it.
         IF (FORM) THEN
            OPEN (UNIT=LUN, FILE=FILSPC(1:XLNB), FORM='FORMATTED',
     *         STATUS='OLD', IOSTAT=IOSVAL)
         ELSE
            OPEN (UNIT=LUN, FILE=FILSPC(1:XLNB), FORM='UNFORMATTED',
     *         STATUS='OLD', IOSTAT=IOSVAL)
            END IF
C                                       OPEN error?
         IF (IOSVAL.NE.0) THEN
            IERR = 1
            WRITE (MSGTXT,1040) IOSVAL
            CALL MSGWRT (6)
         ELSE
C                                       Rewind.
            REWIND (LUN)
            END IF
         END IF
C                                       Exit.
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLBNAM: using translated file name =')
 1001 FORMAT ('VLBNAM: ',A)
 1030 FORMAT ('VLBNAM: CAN''T FIND FILE = ')
 1040 FORMAT ('VLBNAM: FORTRAN OPEN ERROR = ',I6,' FOR FILE =')
      END
      SUBROUTINE GEOPAR (BXM,BYM,BZM,RA,DEC,GAST0,AFREQ,
     *                  UT,U,V,HA,FR,DEL)
C-----------------------------------------------------------------------
C  Routine to determine fringe rate and delay from the geometric
C  parameters of the observations.
C
C  Inputs:
C    BXM         D      projected distance between antennas
C                       along the x-plane (metres)
C    BYM         D      projected distance between antennas
C                       along the y-plane (metres)
C    BZM         D      projected distance between antennas
C                       along the z-plane (metres)
C    RA          D      Source ra (rads)
C    DEC         D      Source dec (rads)
C    GAST0       D      GAST at 0 hr UT (rads)
C    AFREQ       D      Observing freq. (Hz)
C    UT          D      UTC of the observation (rads)
C
C  Outputs:
C    FR          D      Natural fringe rate (Hz)
C    DEL         D      Natural delay (Hz)
C-----------------------------------------------------------------------
      DOUBLE PRECISION BXM, BYM, BZM, RA, DEC, GAST0, AFREQ, UT, FR,
     *   DEL,
     *       C, PIE, TWOPIE, R, SECDAY, OMEG, STM, SHA, BHA,
     *       HA, CDEC, SDEC, CSHA, SSHA, SZ, SX, SY, U, V
C-----------------------------------------------------------------------
C                                       Constants
      C = 299792500.D0
      PIE = 3.14159265359D0
      TWOPIE = PIE * 2.00
      R = PIE/180.
      SECDAY = 86400.D0
      OMEG = 2.*PIE*1.002737923D0/SECDAY
C                                       Sidereal time of observation
      STM = UT * 1.002737923D0
C                                       HA of source wrt x axis
      SHA = GAST0 + STM - RA
C                                       HA of baseline wrt x axis
      BHA = 0.0
      IF (BXM .NE. 0.0) BHA = ATAN2 (BYM, BXM)
C                                       HA of source wrt normal to
C                                       baseline
      HA = SHA - BHA - PIE/2.D0
      HA = MOD (HA, TWOPIE)
C                                       Needed cos & sin values
      CDEC = COS(DEC)
      SDEC = SIN(DEC)
      CSHA = COS(SHA)
      SSHA = SIN(SHA)
C                                       Source vector coordinates
      SZ = SDEC
      SX = CDEC*CSHA
      SY = CDEC*SSHA
C                                       Calculate U & V
      U = ((BXM*SY - BYM*SX)/CDEC)
      V = (BZM*CDEC - (SX*BXM + SY*BYM)*SDEC/CDEC)
C                                       Calculate delay (sec) and
C                                       fringe rate (Hz)
      DEL = -(BXM*SX + BYM*SY + BZM*SZ) / C
      FR = U * OMEG * CDEC * AFREQ / C
C
      RETURN
      END
      SUBROUTINE VLBNFQ (IRET)
C-----------------------------------------------------------------------
C   Creates and writes the FQ table, only writes a new entry when
C   needed.
C   Input from COMMON
C      IFQRNO  I     Next FQ record in table
C      FQBUFF  I     Buffer  etc for FQ file.
C    Input from DVLB include:
C      FREQO   D     Sky frequency (Hz)
C      BANDW   R     Bandwidth Hz.
C      NFREQ   I     # spectral channels
C   Output in common:
C      CURFQI   I(4) Current FQ id number per stream
C      IFFREQ   D(*,2,4)  FQ Frequency Hz, (FQ id, IF,stream)
C      TOTBW    R(*,2,4)  FQ total bandwidth Hz.
C      CHBW     R(*,2,4)  FQ channel seperation Hz.
C   Output:
C      IRET     I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'VLBIN.INC'
      INTEGER   VER, IQLUN, NUMFQE, FQSID(MAXIF), FQID, I, NIF, IRTEMP
      REAL      FQCHB(MAXIF), FQTBW(MAXIF), CHNSEP
      DOUBLE PRECISION FQFRQ(MAXIF), SKYFHZ, VELDEL, FTEMP, FTEMP1
      LOGICAL   FRQOK, GIVMES
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'VLBBUF.INC'
      INCLUDE 'DSET.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (IRTEMP, FTEMP)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialize
      DO 10 I = 1, MAXIF
         FQSID(I) = 1
         FQCHB(I) = 0.0
         FQTBW(I) = 0.0
         FQFRQ(I) = 0.D0
         BNDCOD(I) = ' '
 10      CONTINUE
      VELDEL = 30000.D0/CLIGHT
C
      NIF = 1
C                                       See if need to open
      VER = 1
      IQLUN = 40
      CALL FQINI ('WRIT', FQBUFF, DISKO, CCNOUT, VER, CATBLK, IQLUN,
     *   IFQRNO, FQKOLS, FQNUMV, NIF, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMFQE = FQBUFF(5)
C                                       Init. comparison arrays
C                                       if necessary
      IF (NUMFQE.EQ.0) THEN
         IFFREQ(1) = 0.D0
         TOTBW(1) = 0.0
         CHBW(1) = 0.0
         SIDBND(1) = 1
         END IF
      IF (NUMFQE.EQ.0) NUMFQE = 1
      IF (NUMFQE.GT.MXFQE) THEN
         IRET = 1
         WRITE (MSGTXT,1000) NUMFQE, MXFQE
         GO TO 990
         END IF
C                                       See if need new entry
C                                       Get reference frequency from
C                                       header using EQUIVALENCE.
CC      IIPT = ((KDCRV+JLOCF-1) * NWDPDP) + 1
CC      CALL COPY (NWDPDP, CATBLK(IIPT), IRTEMP)
      FTEMP = CATD(KDCRV+JLOCF)
      SKYFHZ = FREQO - FTEMP - FRQOFF(IDSOU)
      GIVMES = FRQDEL.EQ.0.D0
C                                       Set FQ tolerance
      IF (APARM(11).EQ.0.0) THEN
         FRQDEL = 1.0D20
      ELSE IF (APARM(11).LT.0.0) THEN
         FRQDEL = FREQO * VELDEL
      ELSE IF (APARM(11).GT.0.0) THEN
         FRQDEL = APARM(11) * 1.0D3
         END IF
      IF (GIVMES) THEN
         FTEMP1 = FRQDEL / 1.0D3
         WRITE (MSGTXT,1010) FTEMP1
         IF (APARM(11).NE.0.0) CALL MSGWRT (6)
         END IF
C
      FRQOK = .FALSE.
      CHNSEP = BANDW / NFREQ
C                                       Loop and compare
      DO 100 I = 1,NUMFQE
         FTEMP = ABS (SKYFHZ - IFFREQ(I))
         IF (FTEMP.LT.FRQDEL) FRQOK = .TRUE.
CC         IF ((SKYFHZ .LT. (IFFREQ(I)+FRQDEL)) .AND.
CC     *      (SKYFHZ .GT. (IFFREQ(1)-FRQDEL))) FRQOK = .TRUE.
C                                       Check channel sep
C                                       for line data
         IF (FRQOK                         .AND.
     *      (BANDW .EQ. TOTBW(I))  .AND.
     *      (CHNSEP .EQ. CHBW(I))  .AND.
     *      (DATASB .EQ. SIDBND(I))) THEN
            CURFQI = I
            GO TO 300
            END IF
 100     CONTINUE
C                                       Write a new entry
C                                       Copy necessary parms into
C                                       FQ arrays
      FQFRQ(1) = SKYFHZ
      FQCHB(1) = CHNSEP
      FQTBW(1) = BANDW
      FQSID(1) = DATASB
      FQID  = IFQRNO
      CALL TABFQ ('WRIT', FQBUFF, IFQRNO, FQKOLS, FQNUMV, NIF,
     *   FQID, FQFRQ, FQCHB, FQTBW, FQSID, BNDCOD, IRET)
      IF (IRET.NE.0) GO TO 999
      CURFQI = FQID
C                                       Fill in the comparison arrays
      IFFREQ(FQID) = FQFRQ(1)
      TOTBW(FQID)  = FQTBW(1)
      CHBW(FQID)   = FQCHB(1)
      SIDBND(FQID) = FQSID(1)
C
C                                       Close the table
 300  CALL TABIO ('CLOS', 0, IFQRNO, FQBUFF, FQBUFF, IRET)
      GO TO 999
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VLBNFQ: ',I4,'ROWS IN FQ TABLE, > MAX OF ',I4)
 1010 FORMAT ('VLBNFQ: FQ entry tolerance = ',E15.5,' kHz')
      END
