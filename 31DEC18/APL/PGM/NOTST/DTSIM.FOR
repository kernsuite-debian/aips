LOCAL INCLUDE 'DTSIM.INC'
C                                       Local include for DTSIM
      INCLUDE 'INCS:ZPBUFSZ.INC'
C                                       Input adverbs
      HOLLERITH XNAME(3), XCLASS(2), XINFIL(12)
      CHARACTER LNAME*12, LCLASS*6, LINFIL*48
      REAL      XSEQ, XDISK, XPRTLV, DPARMS(70)
      INTEGER   ISEQ, IDISK, IPRTLV
      COMMON /DTCHR/ LNAME, LCLASS, LINFIL
      COMMON /INPARM/ XNAME, XCLASS, XSEQ, XDISK, XINFIL,
     *   XPRTLV, DPARMS, ISEQ, IDISK, IPRTLV
C                                       General global variables
      INTEGER   ICNOUT
      COMMON /DTGEN/ ICNOUT
C                                       Buffers
      INTEGER   NBUFSZ, NBUFTB
      PARAMETER (NBUFSZ = UVBFSL, NBUFTB = 512)
      REAL      BUFF(UVBFSL)
      INTEGER   JBUFF(NBUFTB)
      COMMON /DTBUF/ BUFF, JBUFF
C
LOCAL END
      PROGRAM DTSIM
C----------------------------------------------------------------------
C! Generate fake uv-data
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2004, 2007-2009, 2012, 2015, 2017-2018
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
      CHARACTER LPGM*6
      INTEGER   IRET
C
      INTEGER   JLUNF, ILUNO, ILUNTB
      INCLUDE 'DTSIM.INC'
      DATA LPGM /'DTSIM'/
      DATA JLUNF /10/, ILUNO /18/, ILUNTB /28/
C----------------------------------------------------------------------
C                                       Read input adverbs/init.
      CALL DTSINP (LPGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read simulation parameters
      CALL DTPARM (LINFIL, JLUNF, IPRTLV, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write the fake uv-data
      CALL DTFILL (ILUNO, ILUNTB, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Create a history file
      CALL DTHIS (ILUNTB, IRET)
C                                       Exit
990   CALL DIE (IRET, BUFF)
C
999   STOP
      END
      SUBROUTINE DTSINP (LPGM, IRET)
C----------------------------------------------------------------------
C   Read input adverbs and perform general initialization
C   Inputs:
C      LPGM      C*6      Task name.
C   Output:
C      IRET      I        Return code (0=> ok; else error)
C----------------------------------------------------------------------
      CHARACTER LPGM*6
      INTEGER   IRET
C
      INTEGER   NPARM, IERR
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DTPM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C
      NSCR = 0
      NCFILE = 0
C                                       Get input adverbs
      NPARM = 90
      CALL GTPARM (LPGM, NPARM, RQUICK, XNAME, BUFF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 1
C                                       Check if initiator (AIPS)
C                                       not found.
         IF (IERR.EQ.1) GO TO 999
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IERR.NE.0) IRET = 1
C                                       Abort if error obtaining
C                                       input parameters.
      IF (IRET.NE.0) GO TO 999
C                                       Convert input parameters
      ISEQ = XSEQ + 0.1
      IDISK = XDISK + 0.1
      IPRTLV = XPRTLV + 0.1
C
      CALL H2CHR (12, 1, XNAME, LNAME)
      CALL H2CHR (6, 1, XCLASS, LCLASS)
      CALL H2CHR (48, 1, XINFIL, LINFIL)
C                                       defaults to DTPM.INC
      DYEAR  = DPARMS(1) + 0.1
      DMONTH = DPARMS(2) + 0.1
      DDAY   = DPARMS(3) + 0.1
      CALL RFILL (MAXIF, DPARMS(40), DEFGAN)
      CALL RCOPY (30, DPARMS(11), DEFGAN)
      CALL RFILL (MAXIF, DPARMS(70), DEFSYS)
      CALL RCOPY (30, DPARMS(41), DEFSYS)
C
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('DTSINP: ERROR',I4,' READING INPUT ADVERBS')
      END
      SUBROUTINE DTFILL (ILUNO, ILUNTB, IRET)
C----------------------------------------------------------------------
C   Generate and write the fake data file.
C   Input:
C      ILUNO   I     LUN for output uv-data file.
C      ILUNTB  I     LUN for general table I/O
C   Outputs:
C      IRET    I     Return code; 0 => ok, else error
C----------------------------------------------------------------------
      INTEGER   ILUNO, ILUNTB, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   WMASK(4*MAXCIF), WERROR, WTRUE, WFALSE, WEND
      CHARACTER LNAM*12, LCLS*6, LOUTFL*48
      INTEGER   INDO, NVOUT, NFIRST, NPIO, NBO, IPTRO, NVCNT, NIOUT,
     *   KBIND, NFLSIZ, NVBUFF, N256, IENTRY, IANT, JANT, RNXRET,
     *   NFLAGD
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DRNX.INC'
      DATA WTRUE, WFALSE /.TRUE., .FALSE./
      DATA NFIRST / 0/, NBO / 1/, NVOUT / 10000000/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NFLAGD = 0
C                                       Construct the AIPS catalog hdr
      CALL DTCAT (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create the output uv-file
C                                       Fix output name
      LNAM = 'DTSIM'
      LCLS = 'UVDATA'
      CALL MAKOUT (LNAM, LCLS, 0, LCLS, LNAME, LCLASS, ISEQ)
C                                       Update in catalog header
      CALL CHR2H (12, LNAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, LCLASS, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = ISEQ
C                                       Create UV file
      ICNOUT = 1
      CALL UVCREA (IDISK, ICNOUT, BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Mark in /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = IDISK
      FCNO(NCFILE) = ICNOUT
      FRW(NCFILE) = 1
C                                       Read uv-data pointers in
C                                       catalog header
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1110) IRET
         GO TO 990
         END IF
C                                       Construct physical file name
      CALL ZPHFIL ('UV', IDISK, ICNOUT, 1, LOUTFL, IRET)
C                                       Open for write
      CALL ZOPEN (ILUNO, INDO, IDISK, LOUTFL, WTRUE, WFALSE, WFALSE,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1130) IRET
         GO TO 990
         END IF
C                                       Set file size
      NFLSIZ = CATBLK(KIGCN)
C                                       Initialize for write
      NPIO = 0
      CALL UVINIT ('WRIT', ILUNO, INDO, NVOUT, NFIRST, LREC, NPIO,
     *   NBUFSZ, BUFF, NBO, IPTRO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1140) IRET
         GO TO 990
         END IF
C                                       No records per buffer
      NVBUFF = NPIO
C                                       Initialize
      NVCNT = 0
      NIOUT = 0
C                                       NX table
      CALL FILL (MAXSUB, 0, RNXNOS)
      RNXRET = 0
      CALL RNXINI (IDISK, ICNOUT, CATBLK, RNXRET)
C                                       Get first schedule entry
      WERROR = .FALSE.
      CALL DTFRST (IENTRY, IANT, JANT, WMASK, WEND)
C
C                                       While (still schedule data)
C                                          and (not error)
C                                       do (write uv-record)
100   IF (WERROR.OR.WEND) GO TO 800
C                                       Get next data point.
         CALL DTUVDT (IENTRY, IANT, JANT, BUFF(IPTRO), IRET)
         WERROR = (IRET.GT.0)
         IF (IRET.LT.0) NFLAGD = NFLAGD + 1
C                                       Write uv-record
         IF (IRET.EQ.0) THEN
C                                       update NX table
            CALL RNXUPD (BUFF(IPTRO), RNXRET)
            NIOUT = NIOUT + 1
            NVCNT = NVCNT + 1
            IPTRO = IPTRO + LREC
            IF (MOD(NVCNT,50000).EQ.1) THEN
               WRITE (MSGTXT,1100) NVCNT
               CALL MSGWRT (2)
               END IF
C                                       Is buffer full ?
            IF (NIOUT.GE.NVBUFF) THEN
C                                       Expand uv-file ?
               IF (NVCNT.GT.NFLSIZ) THEN
C                                       Add space for an additional
C                                       10 I/O buffers
                  N256 = ((10 * NVBUFF * LREC) / 256) + 1
                  CALL ZEXPND (ILUNO, IDISK, LOUTFL, N256, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1200) IRET
                     GO TO 990
                     END IF
C                                       Update file size
                  NFLSIZ = NFLSIZ + 10 * NVBUFF
                  END IF
C                                       Write buffer
               CALL UVDISK ('WRIT', ILUNO, INDO, BUFF, NIOUT, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1220) IRET
                  GO TO 990
                  END IF
C                                       Update pointers
               IPTRO = KBIND
               NVBUFF = NIOUT
               NIOUT = 0
               END IF
            END IF
C                                       Get next schedule record
         IF (IRET.LE.0) CALL DTNEXT (IENTRY, IANT, JANT, WMASK, WEND)
C                                       Endwhile
         GO TO 100
C                                       Close uv-file
C                                       First, finish last write.
800   IF (NVCNT.GT.NFLSIZ) THEN
C                                       Expand output file if necessary
         N256 = (((NVCNT-NFLSIZ) * LREC) / 256) + 1
         CALL ZEXPND (ILUNO, IDISK, LOUTFL, N256, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1200) IRET
            GO TO 990
            END IF
         END IF
C                                       Flush output buffer
      NIOUT = -ABS (NIOUT)
      CALL UVDISK ('FLSH', ILUNO, INDO, BUFF, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1220) IRET
         GO TO 990
         END IF
C                                       Compress output file
      CALL UCMPRS (NVCNT, IDISK, ICNOUT, ILUNO, CATBLK, BUFF, IRET)
C                                       Close output file
      CALL ZCLOSE (ILUNO, INDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1400) IRET
         GO TO 990
         END IF
C                                       close NX table
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
C                                       Update visibility count in
C                                       catalog header
      CATBLK(KIGCN) = NVCNT
      CALL CATIO ('UPDT', IDISK, ICNOUT, CATBLK, 'REST', BUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1500) IRET
         GO TO 990
         END IF
      IF (NFLAGD.GT.0) THEN
         WRITE (MSGTXT,1800) NFLAGD
         CALL MSGWRT (4)
         END IF
C                                       Write FQ table
      CALL DTFQTB (ILUNTB, IRET)
C                                       Write AN table(s)
      CALL DTANTB (ILUNTB, IRET)
C                                       Write SU table
      CALL DTSUTB (ILUNTB, IRET)
C                                       Write BP table
      CALL DTBPTB (ILUNTB, IRET)
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
      GO TO 999
C
999   RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('DTFILL: ERROR',I4,' CREATING OUTPUT FILE')
 1100 FORMAT ('Writing record',I12)
 1110 FORMAT ('DTFILL: ERROR',I4,' READING CATALOG HEADER')
 1130 FORMAT ('DTFILL: ERROR',I4,' OPENING OUTPUT FILE')
 1140 FORMAT ('DTFILL: ERROR',I4,' INITIALIZING OUTPUT FILE')
 1200 FORMAT ('DTFILL: ERROR',I4,' ALLOCATING NEW DISK SPACE')
 1220 FORMAT ('DTFILL: ERROR',I4,' WRITING OUTPUT RECORD')
 1400 FORMAT ('DTFILL: ERROR',I4,' CLOSING OUTPUT FILE')
 1500 FORMAT ('DTFILL: ERROR',I4,' UPDATING CATALOG HEADER')
 1800 FORMAT ('DTFILL: omitted',I8,' fully flagged records')
      END
      SUBROUTINE DTUVDT (IENTRY, IANT, JANT, VBUFF, IRET)
C----------------------------------------------------------------------
C   Construct a simulated uv-data record
C   Input:
C      IENTRY  I     Entry number in schedule
C      IANT    I     Antenna 1
C      JANT    I     Antenna 2
C   Output:
C      VBUFF   R(*)  Output uv-record in AIPS format
C      IRET    I     Return code (0 => ok, -1 => fully flagged)
C-----------------------------------------------------------------------
      REAL      VBUFF(*)
      INTEGER   IENTRY, IANT, JANT, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DTPM.INC'
      CHARACTER LANT1*8, LANT2*8
      COMPLEX   GAINR1(MAXCHA), GAINL1(MAXCHA), GAINR2(MAXCHA),
     *   GAINL2(MAXCHA), CDAT(4)
      DOUBLE PRECISION TIME, DATE, DSHFT1, DSHFT2
      INTEGER   JFQ
      REAL      TINT, FLUXN(4), BWIDTH, SEED, WEIGHT(4), RTIME, PANG(2)
      DOUBLE PRECISION DAMPL1(2), DAMPL2(2), DFEED1(2,2),
     *   DFEED2(2,2), DRAP, DDECP, U, V, W, GAST, GAST0, GASTR,
     *   DX(2), DY(2), DZ(2), DRA, DDEC, DFR
      COMPLEX   FLUX(4), FLXOUT(4), GAIN(4)
      INTEGER   ISUBA, JSUBA, IFQID, K, JIF, ISTOKE, ICHAN, IOFF, M,
     *   JSOU, MODTYP, IMODEL, JNDXC1, JNDXC2, IDUM, NGOOD
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NGOOD = 0
C                                       Schedule entry descriptors
      ISUBA = IDTSSB(IENTRY)
      IFQID = IDTSFQ(IENTRY)
C                                       Match src, suba and freqid
C                                       in simulated data tables
      CALL DTMATC (LDTSCH(IENTRY), IDUM, ISUBA, IFQID, JSOU, IDUM,
     *   JSUBA, JFQ)
C                                       Check for valid subarray
      IF (JSUBA.EQ.0) THEN
         IRET = 1
         WRITE (MSGTXT,1050) ISUBA
         GO TO 990
         END IF
C                                       Check for valid source
      IF (JSOU.EQ.0) THEN
         IRET = 2
         WRITE (MSGTXT,1100) LDTSCH(IENTRY)
         GO TO 990
         END IF
C                                       Antenna names
      LANT1 = LDTANT(IANT,JSUBA)
      LANT2 = LDTANT(JANT,JSUBA)
C                                       Seed for random par. for noise
C                                       calcs.
      SEED = 0.0123456789
C                                       Integration for noise calcs.
      TINT = RDTINT(IENTRY) * 86400.0
C                                       First, get the proper Julian Date
      DATE = DTJDAT
C                                       Get UT time
      TIME = RDTSTK(IENTRY)
      RTIME = TIME
C                                       Get mean RA, DEC
      RA = DTSRA(JSOU)
      DEC = DTSDEC(JSOU)
      DRA = RA * DG2RAD
      DDEC = DEC * DG2RAD
C                                       Get GAST and GAST rate at UT0
      CALL GETGST (DATE, GAST0, GASTR)
      GAST = GAST0 + TIME * GASTR
C                                       Get baseline separation
      CALL GETBAS (TIME, GAST, RA, DEC, U, V, W,
     *     IDTMNT(IANT,JSUBA), IDTMNT(JANT,JSUBA),
     *     DTCPOS(1,IANT,JSUBA), DTCPOS(1,JANT,JSUBA),
     *     DTORB(1,IANT,JSUBA), DTORB(1,JANT,JSUBA), IRET)
C                                       If source is not mutually
C                                       visible, then set weight -ve
      CALL RFILL (4, 1.0, WEIGHT)
      IF (IRET.NE.0) THEN
         U = 1.0
         V = 1.0
         W = 1.0
         CALL RFILL (4, -1.0, WEIGHT)
         ENDIF
C                                       Convert meters to wavelengths
      U = U * DTRFRQ / VELITE
      V = V * DTRFRQ / VELITE
      W = W * DTRFRQ / VELITE
C                                       Calculate U,V,W coordinates
C                                       [send apparent sidereal time]
C                                       although Local should be used,
C                                       for now, send in Greenwich
      CALL CALCUV (DATE, TIME, GAST, RA, DEC, U, V, W, DRAP, DDECP)
C                                       Station coordinates reqd.
C                                       for natural fringe rate
C                                       calculation.
      CALL DFILL (2, 0.0D0, DX)
      CALL DFILL (2, 0.0D0, DY)
      CALL DFILL (2, 0.0D0, DZ)
C                                       All but space orbiting mount
      IF (IDTMNT(IANT,JSUBA).NE.2) THEN
         DX(1) = DTCPOS(1,IANT,JSUBA)
         DY(1) = DTCPOS(2,IANT,JSUBA)
         DZ(1) = DTCPOS(3,IANT,JSUBA)
         END IF
      IF (IDTMNT(JANT,JSUBA).NE.2) THEN
         DX(2) = DTCPOS(1,JANT,JSUBA)
         DY(2) = DTCPOS(2,JANT,JSUBA)
         DZ(2) = DTCPOS(3,JANT,JSUBA)
         END IF
C                                       Fill random parameters
C                                       (u,v,w)
      VBUFF(1+ILOCU) = U
      VBUFF(1+ILOCV) = V
      VBUFF(1+ILOCW) = W
C                                       Time, baseline
      VBUFF(1+ILOCT) = TIME
      VBUFF(1+ILOCA1) = IANT
      VBUFF(1+ILOCA2) = JANT
      VBUFF(1+ILOCSA) = ISUBA
C                                       Freq. id.
      VBUFF(1+ILOCFQ) = IFQID
C                                       Source ID.
      VBUFF(1+ILOCSU) = JSOU
      VBUFF(1+ILOCSU) = IDTSID(JSOU)
C                                       Integration time.
      VBUFF(1+ILOCIT) = TINT
C                                       This call just initializes
C                                       the random number generator
C                                       - it ignores FLUX...CDAT
      IRET = 0
      CALL ADNOIZ (FLUX, FLUXN, GAIN, CDAT, SEED, IRET)
      IF (IRET.NE.0) THEN
C        IRET=0 means a portable number generator was being used.
         IRET = 0
         END IF
C                                       Compute parallactic angle
C                                       for each antenna
      CALL ORIENT (IANT, JANT, JSUBA, GAST, DRAP, DDECP, PANG)
C
C                                       Fill uv_record:
C                                       Loop over IF
      DO 500 JIF = 1, NDTIF
C                                       Compute antenna-based gain
C                                       factors. First antenna_1.
C
C                                       Compute channel shift if
C                                       VLBA data
         DSHFT1 = 0.0D0
         GSTIAT = GAST0
         IF (LDTARR(JSUBA).EQ.'VLBA') THEN
            CALL DETRAT (RTIME, DRA, DDEC, DX(1), DY(1), DZ(1),
     *         DTFREQ(JIF,JFQ), DFR)
            IF (RDTCHW(JIF,JFQ).NE.0.0) DSHFT1 = DFR / RDTCHW(JIF,JFQ)
            END IF
C
         CALL DTINDX (LANT1, IFQID, JNDXC1)
         CALL DTGAIN (JSOU, IANT, JSUBA, JFQ, JIF, JNDXC1, RTIME,
     *      DSHFT1, GAINR1, GAINL1, DAMPL1, DFEED1, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Antenna_2.
         DSHFT2 = 0.0D0
         IF (LDTARR(JSUBA).EQ.'VLBA') THEN
            CALL DETRAT (RTIME, DRA, DDEC, DX(2), DY(2), DZ(2),
     *         DTFREQ(JIF,JFQ), DFR)
            IF (RDTCHW(JIF,JFQ).NE.0.0) DSHFT2 = DFR / RDTCHW(JIF,JFQ)
            END IF
C
         CALL DTINDX (LANT2, IFQID, JNDXC2)
         CALL DTGAIN (JSOU, JANT, JSUBA, JFQ, JIF, JNDXC2, RTIME,
     *      DSHFT2, GAINR2, GAINL2, DAMPL2, DFEED2, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Now, we have channel bandwidth
         BWIDTH = RDTCHW(JIF,JFQ)
C
         FLUXN(1) = DAMPL1(1) * DAMPL2(1) / SQRT(2.0*BWIDTH*TINT)
         FLUXN(2) = DAMPL1(2) * DAMPL2(2) / SQRT(2.0*BWIDTH*TINT)
         FLUXN(3) = DAMPL1(1) * DAMPL2(2) / SQRT(2.0*BWIDTH*TINT)
         FLUXN(4) = DAMPL1(2) * DAMPL2(1) / SQRT(2.0*BWIDTH*TINT)
C                                       Compute inverse variance
C                                       weights
         DO 375 K = 1, 4
C                                       Compute weight if not already
C                                       flagged on elevation; unit
C                                       weight if no noise specified.
            IF ((WEIGHT(K).GT.0).AND.(FLUXN(K).GT.0)) WEIGHT(K) =
     *         1.0 / (FLUXN(K) ** 2)
375         CONTINUE
C                                       Loop over freq. channel
         DO 400 ICHAN = 1, NDTCHN
C                                       Baseline gain factors
            GAIN(1) = GAINR1(ICHAN) * CONJG (GAINR2(ICHAN))
            GAIN(2) = GAINL1(ICHAN) * CONJG (GAINL2(ICHAN))
            GAIN(3) = GAINR1(ICHAN) * CONJG (GAINL2(ICHAN))
            GAIN(4) = GAINL1(ICHAN) * CONJG (GAINR2(ICHAN))
C                                       Form polzn. corr. pairs
            MODTYP = 0
            CALL ADDMOD (MODTYP, RDTSPM(1,1), U, V, FLUX)
C                                       If MODTYP = 0, vis is ZEROed!
            DO 200 IMODEL = 1,NDTSMD
               IF (LDTSMD(IMODEL).EQ.LDTSCH(IENTRY)) THEN
                  MODTYP = RDTSPM(1,IMODEL)
                  CALL ADDMOD (MODTYP, RDTSPM(2,IMODEL), U, V, FLUX)
                  END IF
 200           CONTINUE
C                                       Convert IQUV into RR,LL,RL,LR
C                                       Flux = (I,Q,U,V)
            FLUX(3) = (0.0,1.0)*FLUX(3)
C                                       Flux = (I,Q,iU,V)
            FLUX(2) = FLUX(2) - FLUX(4)
            FLUX(4) = FLUX(2) + FLUX(4)
            FLUX(2) = FLUX(4) - FLUX(2)
C                                       Flux = (I,V,iU,Q)
            FLUX(1) =     FLUX(1) - FLUX(2)
            FLUX(2) = 2.0*FLUX(2) + FLUX(1)
            FLUX(3) =     FLUX(4) + FLUX(3)
            FLUX(4) = 2.0*FLUX(4) - FLUX(3)
C                                       Flux = (I-V,I+V,Q+iU,Q-iU)
C
C                                       Correct for instrumental
C                                       polarization
            CALL DTERMS (FLUX, FLXOUT, DFEED1, DFEED2, PANG, WDTLIN)
C                                       Add noise
            CALL ADNOIZ (FLXOUT, FLUXN, GAIN, CDAT, SEED, IRET)
C                                       Index in buffer
            IOFF = NRPARM + 1 + (JIF - 1) * INCIF + (ICHAN - 1) * INCF
C                                       Loop over Stokes
            DO 300 ISTOKE = 1, NDTSTK
C                                       Update index
               K = IOFF + (ICOR0 - IDTSTK(ISTOKE)) * INCS
C                                       Fill (Re, Im, Wgt)
               M = MIN (-IDTSTK(ISTOKE), 4)
               M = MAX (M, 1)
               VBUFF(K) = REAL (CDAT(M))
               VBUFF(K+1) = AIMAG (CDAT(M))
               VBUFF(K+2) = WEIGHT(M)
               IF (WEIGHT(M).GT.0.0) NGOOD = NGOOD + 1
300            CONTINUE
400         CONTINUE
500      CONTINUE
      IF ((NGOOD.EQ.0) .AND. (IRET.EQ.0)) IRET = -1
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1050  FORMAT ('DTUVDT: UNIDENTIFIED SUBARRAY',I4,' IN SCHEDULE')
1100  FORMAT ('DTUVDT: UNIDENTIFIED SOURCE ',A16,' IN SCHEDULE')
      END
      SUBROUTINE DTGAIN (JSOU, JANT, JSUBA, JFQ, JIF, JNDXCE, TIME,
     *   DSHIFT, GAINR, GAINL, DAMPL, DFEED, IRET)
C----------------------------------------------------------------------
C   Compute antenna-based calibration factors.
C   Input:
C      JSOU     I       Source index
C      JANT     I       Antenna index
C      JSUBA    I       Subarray index
C      JFQ      I       Freqid. index
C      JIF      I       IF number
C      JNDXCE   I       Cal. error index
C      TIME     R       Time in days since reference day.
C      DSHIFT   D       Frequency shift (channels)
C   Output:
C      GAINR    CX(*)   Complex gain spectrum (RCP)
C      GAINL    CX(*)   Complex gain spectrum (LCP)
C      DAMPL    R(2)    Noise power level in Janskys (RCP,LCP)
C      DFEED    D(2,2)  Feed D-terms (Re,Im; RCP,LCP)
C      IRET     I       Return code (0=> ok; else error)
C----------------------------------------------------------------------
      DOUBLE PRECISION DFEED(2,2), DSHIFT
      COMPLEX GAINR(*), GAINL(*)
      REAL TIME
      INTEGER JSOU, JANT, JSUBA, JFQ, JIF, JNDXCE, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DTPM.INC'
      DOUBLE PRECISION DELAY(2), DPHASE(2), DRATE(2), DACCEL(2),
     *   DGAIN(2), DTSYS(2), DAMPL(2), DGERR(2), DARG, DOMEGA
      COMPLEX BAND(2,MAXCHA)
      INTEGER J
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Set to unit gain.
      DO 10 J = 1, NDTCHN
         GAINR(J) = CMPLX (1.0, 0.0)
         GAINL(J) = CMPLX (1.0, 0.0)
10       CONTINUE
C                                       Compute calibration error
C                                       quantities.
      CALL DTCALS (JSOU, JANT, JSUBA, JFQ, JIF, JNDXCE, TIME, DELAY,
     *   DPHASE, DRATE, DACCEL, DFEED, DGAIN, DTSYS, DAMPL, DGERR)
C                                       Get complex bandpass
C                                       response function in both
C                                       polarizations.
      CALL DTBAND (JNDXCE, JIF, TIME, DSHIFT, BAND)
C                                       Freq. factor for delay slope
      DOMEGA = TWOPI * RDTCHW(JIF,JFQ)
C                                       Fill spectrum
      DO 200 J = 1, NDTCHN
         DARG = DPHASE(1) + (J - 1) * DOMEGA * DELAY(1)
         GAINR(J) = DGERR(1) * BAND(1,J) *
     *      CMPLX (COS (DARG), SIN (DARG))
         DARG = DPHASE(2) + (J - 1) * DOMEGA * DELAY(2)
         GAINL(J) = DGERR(2) * BAND(2,J) *
     *      CMPLX (COS (DARG), SIN (DARG))
200      CONTINUE
C                                       Exit
 999  RETURN
      END
      SUBROUTINE DTCAT (IRET)
C----------------------------------------------------------------------
C   Construct the AIPS catalog header.
C   Output:
C      IRET    I     Return code; 0=> ok, else error.
C----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER RTYPES(12)*8, TYPES(7)*8, UNITS*8
      INTEGER   I, NAXIS, NRAN, NDIM(7), INDEX, JLOW
      REAL      CRINC(7), CRPIX(7)
      DOUBLE PRECISION CRVAL(7)
C
      INCLUDE 'INCS:DTPM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                        No. random parameters.
      DATA NRAN /10/
C                                        Rand. parm. names.
      DATA RTYPES /'UU-L-SIN','VV-L-SIN','WW-L-SIN',
     *   'TIME1   ','SUBARRAY','SOURCE  ','FREQSEL',
     *   'INTTIM  ','ANTENNA1','ANTENNA2','WEIGHT','SCALE'/
C                                       Uniform axes.
C                                        No. axes.
      DATA NAXIS /6/
C                                        Axes names.
      DATA TYPES /'COMPLEX ','STOKES  ','FREQ    ',
     *   'IF      ','RA      ','DEC     ','        '/
C                                        Axis dimensions
      DATA NDIM /3,1,1,1,1,1,0/
C                                        Reference values
      DATA CRVAL /1.0D0, -1.0D0, 0.0D0, 1.0D0, 3*0.0D0/
C                                        Reference pixel.
      DATA CRPIX /7*1.0/
C                                        Coordinate increment.
      DATA CRINC /1.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0/
C                                       Units
      DATA UNITS /'CORREL. '/
C-----------------------------------------------------------------------
C                                       Zero fill CATBLK
      CALL FILL (256, 0, CATBLK)
C                                       Fill axis arrays.
C                                       Random axis names
      DO 10 I = 1,KIPTPN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, '        ', 1, CATH(INDEX))
 10      CONTINUE
C                                       Compressed data?
      IF (IDTCMP.GT.0) THEN
         NRAN = 12
         NDIM(1) = 1
      ELSE
         NRAN = 10
         NDIM(1) = 3
         END IF
C                                       Actual random parameters.
      DO 20 I = 1,NRAN
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, RTYPES(I), 1, CATH(INDEX))
 20      CONTINUE
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
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, TYPES(I), 1, CATH(INDEX))
 30      CONTINUE
C                                       Fill in values.
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = NRAN
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 2000.0
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
      CATD(KDRST) = DTLRFQ
C                                       Alternate ref. value & pixel
      CATD(KDARV) = 0.0D0
      CATR(KRARP) = 0.0
      CATBLK(KIALT) = 0
C                                       Sort order ('TB'=>time-baseline
C                                       order)
      CALL CHR2H (2, 'TB', 1, CATH(KITYP))
C                                       Magic value blanking.
      CATR(KRBLK) = 0.0
C                                       Units
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C-----------------------------------------------------------------------
C                                       Number of vis. (Init. 1000)
      CATBLK(KIGCN) = 1000
C                                       Position (RA, DEC).
      CATD(KDCRV+4) = 0.0D0
      CATD(KDCRV+5) = 0.0D0
C                                       Number of polarizations.
      CATBLK(KINAX+1) = NDTSTK
C                                       Lowest Stokes type.
      CATD(KDCRV+1) = IDTSTK(1)
C                                       Reference frequency.
      CATD(KDCRV+2) = DTRFRQ
C                                       Find channel bandwidth of
C                                       IF.1 of lowest FQ_ID.
      JLOW = IDTFQD(1)
      CATR(KRCIC+2) = RDTCHW(1,1)
       DO 50 I = 1, NDTFQD
          IF (IDTFQD(I).LT.JLOW) THEN
             JLOW = IDTFQD(I)
             CATR(KRCIC+2) = RDTCHW(1,I)
             END IF
50       CONTINUE
C                                       Number of frequencies.
      CATBLK(KINAX+2) = NDTCHN
C                                       Number of IFS.
      CATBLK(KINAX+3) = NDTIF
C                                       Observing date.
      CALL CHR2H (8, LDTDAT, 1, CATH(KHDOB))
C                                       Object.
      CALL CHR2H (8, 'MULTI   ', 1, CATH(KHOBJ))
C                                       Telescope.
      CALL CHR2H (8, 'DTSIM   ', 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, '        ', 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, '        ', 1, CATH(KHOBS))
C                                       UV data
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE DTFLAG (TIME, LANT1, LANT2, ISUBA, IFQID, LSOUR,
     *   WMASK, WFLAG)
C----------------------------------------------------------------------
C   Determine flagging on a given time-baseline record
C   Inputs:
C      TIME    R      Time (in days since reference day).
C      LANT1   C*8    Antenna 1.
C      LANT2   C*8    Antenna 2.
C      ISUBA   I      Subarray number.
C      IFQID   I      Frequency id.
C      LSOUR   C*16   Source name.
C   Outputs:
C      WMASK   L(*)   Mask defining valid data (per IF,chan,Stokes)
C      WFLAG   L      True if all data flagged.
C----------------------------------------------------------------------
      LOGICAL WMASK(*), WFLAG
      CHARACTER LANT1*8, LANT2*8, LSOUR*16
      REAL TIME
      INTEGER ISUBA, IFQID
C
      INTEGER NFILL, J, K, M, JJ, I
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Initialization
      NFILL = 4 * NDTIF * NDTCHN
      DO 20 J = 1, NFILL
         WMASK(J) = .TRUE.
20       CONTINUE
C                                       Loop through flag records.
      DO 500 J = 1, NDTFG
C                                       Check time
         IF ((TIME.LT.RDTSFG(J)).OR.(TIME.GT.RDTEFG(J))) GO TO 500
C                                       Check antennas
         IF ((LDTFGA(J).NE.'ALL').AND.(LANT1.NE.LDTFGA(J))) GO TO 500
         IF ((LDTFGB(J).NE.'ALL').AND.(LANT2.NE.LDTFGB(J))) GO TO 500
C                                       Check fqid.
         IF ((IDTFFQ(J).GT.0).AND.(IFQID.NE.IDTFFQ(J))) GO TO 500
C                                       Check subarray.
         IF ((IDTSBF(J).GT.0).AND.(ISUBA.NE.IDTSBF(J))) GO TO 500
C                                       Check source
         IF ((LDTFGS(J).NE.'ALL').AND.(LSOUR.NE.LDTFGS(J))) GO TO 500
C                                       Update mask of flagged data
C                                       for each Stokes, IF and chan.
         DO 300 M = 1, 4
            DO 280 K = IDTBIF(J), IDTEIF(J)
               DO 260 I = IDTBCH(J), IDTECH(J)
C                                       Compute index in WMASK
                  JJ = (M - 1) * NDTIF * NDTCHN +
     *               (K - 1) * NDTCHN + I
                  WMASK(JJ) = .FALSE.
260               CONTINUE
280            CONTINUE
300         CONTINUE
C
500      CONTINUE
C                                       Are all data flagged ?
      WFLAG = .TRUE.
      DO 600 J = 1, NFILL
         IF (WMASK(J)) WFLAG = .FALSE.
600      CONTINUE
C                                       Exit
999   RETURN
      END
      SUBROUTINE DTFRST (IENTRY, IANT, JANT, WMASK, WEND)
C----------------------------------------------------------------------
C   Initialize the schedule pipe; get the first entry in TB order
C   Outputs:
C      IENTRY     I      Schedule entry number.
C      IANT       I      Antenna 1.
C      JANT       I      Antenna 2.
C      WMASK      L(*)   Mask of valid data (per IF,Stokes,channel).
C      WEND       L      True if end of schedule reached.
C----------------------------------------------------------------------
      LOGICAL WMASK(*), WEND
      INTEGER IENTRY, IANT, JANT
C
      INTEGER K
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IENTRY = 0
C                                       Loop over schedule entries.
      DO 200 K = 1, NDTSCH
C                                       Generate stack containing
C                                       current time of each schedule
C                                       entry.
         RDTSTK(K) = RDTSTT(K) + RDTINT(K) / 2.0
C                                       Check for end.
         IF (RDTSTK(K).GT.RDTEND(K)) RDTSTK(K) = -1.0
C                                       Roll each schedule entry until
C                                       record containing unflagged
C                                       data is found.
         IANT = 1
         JANT = 0
         CALL DTROLL (K, IANT, JANT, WMASK)
C                                       Find lowest time
         IF (RDTSTK(K).LT.0) GO TO 200
         IF (IENTRY.EQ.0) THEN
            IENTRY = K
         ELSE
            IF (RDTSTK(K).LT.RDTSTK(IENTRY)) IENTRY = K
            END IF
C
200      CONTINUE
C                                       End of schedule ?
      WEND = (IENTRY.EQ.0)
C                                       Exit
999   RETURN
      END
      SUBROUTINE DTNEXT (IENTRY, IANT, JANT, WMASK, WEND)
C----------------------------------------------------------------------
C   Get next schedule point in time-baseline order
C   Input/Output:
C      IENTRY     I      Current schedule entry number.
C      IANT       I      Current antenna 1.
C      JANT       I      Current antenna 2.
C   Output:
C      WMASK      L(*)   Mask for valid data (per IF,Stokes,channel)
C      WEND       L      True if end of schedule.
C----------------------------------------------------------------------
      LOGICAL WMASK(*), WEND
      INTEGER IENTRY, IANT, JANT
C
      REAL TOLD
      INTEGER K
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Get next baseline on current
C                                       schedule entry.
      TOLD = RDTSTK(IENTRY)
      CALL DTROLL (IENTRY, IANT, JANT, WMASK)
C                                       Search all entries if new
C                                       time returned.
      WEND = (RDTSTK(IENTRY).LT.0)
      IF ((RDTSTK(IENTRY).NE.TOLD).OR.WEND) THEN
C                                       Find lowest time.
         IENTRY = 0
         DO 200 K = 1, NDTSCH
            IF (RDTSTK(K).LT.0) GO TO 200
            IF (IENTRY.EQ.0) THEN
               IENTRY = K
            ELSE
               IF (RDTSTK(K).LT.RDTSTK(IENTRY)) IENTRY = K
               END IF
200         CONTINUE
C                                       End of schedule ?
         WEND = (IENTRY.EQ.0)
C                                       Get first entry at lowest
C                                       time found.
         IF (.NOT.WEND) THEN
            IANT = 1
            JANT = 0
            CALL DTROLL (IENTRY, IANT, JANT, WMASK)
            END IF
         END IF
C                                       Exit
      RETURN
      END
      SUBROUTINE DTROLL (IENTRY, IANT, JANT, WMASK)
C----------------------------------------------------------------------
C   Roll the stack for a given schedule entry.
C   Input:
C      IENTRY    I     Schedule entry number.
C   Input/Output:
C      IANT      I     Antenna 1.
C      JANT      I     Antenna 2.
C      WMASK     L(*)  Mask for valid data (per IF,Stokes,channel)
C----------------------------------------------------------------------
      LOGICAL WMASK(*)
      INTEGER IENTRY, IANT, JANT
C
      LOGICAL WEND, WFLAG, WSELCT
      CHARACTER LSOUR*16, LANT1*8, LANT2*8, LSEL*8, LPRE*1
      INTEGER IFQID, ISUBA
      INCLUDE 'INCS:DTPM.INC'
C----------------------------------------------------------------------
C                                       Initialization
      ISUBA = IDTSSB(IENTRY)
      IFQID = IDTSFQ(IENTRY)
      LSOUR = LDTSCH(IENTRY)
      LSEL = LDTSEL(IENTRY)(2:8)
      LPRE = LDTSEL(IENTRY)(1:1)
      WEND = .FALSE.
      WSELCT = .FALSE.
C                                       Repeat
C                                       Increment baseline
30    JANT = JANT + 1
      IF (JANT.GT.NDTANT(ISUBA)) THEN
         IANT = IANT + 1
         JANT = IANT
         IF (IANT.GT.NDTANT(ISUBA)) THEN
            IANT = 1
            JANT = 1
            RDTSTK(IENTRY) = RDTSTK(IENTRY) + RDTINT(IENTRY)
C                                       End of this schedule entry ?
            IF (RDTSTK(IENTRY).GT.RDTEND(IENTRY)) THEN
               RDTSTK(IENTRY) = -1.0
               WEND = .TRUE.
               END IF
            END IF
         END IF
C                                       Is this baseline selected ?
      LANT1 = LDTANT(IANT,ISUBA)
      LANT2 = LDTANT(JANT,ISUBA)
      IF (LPRE.EQ.'*') THEN
         WSELCT = (LSEL.EQ.LANT1).OR.(LSEL.EQ.LANT2)
      ELSE IF (LPRE.EQ.'-') THEN
         WSELCT = (LSEL.NE.LANT1).AND.(LSEL.NE.LANT2)
      ELSE IF (LPRE.EQ.' ') THEN
         WSELCT = .TRUE.
         END IF
C                                       Until (end of schedule entry)
C                                       or (selected baseline found)
      IF ((.NOT.WSELCT).AND.(.NOT.WEND)) GO TO 30
C                                       Check flagging information
C                                       for this record.
      WFLAG = .FALSE.
      IF (.NOT.WEND) THEN
         CALL DTFLAG (RDTSTK(IENTRY), LANT1, LANT2, ISUBA, IFQID,
     *      LSOUR, WMASK, WFLAG)
         END IF
C                                       Until (unflagged data found)
C                                       or (end of schedule entry)
      IF ((WFLAG).AND.(.NOT.WEND)) GO TO 30
C                                       Exit
      RETURN
      END
      SUBROUTINE DTFQTB (ILUN, IRET)
C----------------------------------------------------------------------
C   Create an FQ table for a simulated dataset
C   Input:
C      ILUN     I      Logical unit number.
C   Output:
C      IRET     I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER ILUN, IRET
C
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DTPM.INC'
      DOUBLE PRECISION DFOFF(MAXIF)
      INTEGER FQKOLS(MAXFQC), FQNUMV(MAXFQC), IVER, IFQRNO, J, I
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Create FQ table; open for
C                                       write
      CALL FQINI ('WRIT', JBUFF, IDISK, ICNOUT, IVER, CATBLK, ILUN,
     *   IFQRNO, FQKOLS, FQNUMV, NDTFID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Write FQ table records
      DO 200 J = 1, NDTFQD
         IFQRNO = J
C                                       Subtract ref. frequency
         DO 150 I = 1, NDTIF
            DFOFF(I) = DTFREQ(I,J) - DTRFRQ
            BNDCOD(I) = ' '
150         CONTINUE
C
         CALL TABFQ ('WRIT', JBUFF, IFQRNO, FQKOLS, FQNUMV, NDTFID,
     *      IDTFQD(J), DFOFF, RDTCHW(1,J), RDTTBW(1,J),
     *      IDTSDB(1,J), BNDCOD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
200      CONTINUE
C                                       Close FQ table
      IFQRNO = 0
      CALL TABIO ('CLOS', 0, IFQRNO, JBUFF, JBUFF, IRET)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1010  FORMAT ('DTFQTB: ERROR',I4,' OPENING FQ TABLE')
1020  FORMAT ('DTFQTB: ERROR',I4,' WRITING TO FQ TABLE')
      END
      SUBROUTINE DTANTB (ILUN, IRET)
C----------------------------------------------------------------------
C   Create AN tables for a simulated dataset
C   Input:
C      ILUN      I      LUN for table I/O
C   Output:
C      IRET      I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER ILUN, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DTPM.INC'
      CHARACTER LPOLTP*8, LSOURC*16
      HOLLERITH HSOLTY(2)
      DOUBLE PRECISION DELAY(2), DPHASE(2), DRATE(2), DACCEL(2),
     *   DFEED(2,2), DGAIN(2), DTSYS(2), DAMPL(2), DGERR(2)
      REAL TIME
      INTEGER   ISUBA, IVER, NFILL, J, K, M, I, INDXCE, ILOC, IKEYTP,
     *   NN, JSOU, JANT, JSUBA, JFQ
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LPOLTP /'POLTYPE '/, LSOURC /' '/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Loop over subarrays
      DO 500 J = 1, NDTSUB
         ISUBA = IDTSUB(J)
C                                       Compute GST at UT 0 and
C                                       GST rate
         CALL GETGST (DTJDAT, GSTIA0, DEGPDY)
         GSTIA0 = GSTIA0 * RAD2DG
         DEGPDY = DEGPDY * RAD2DG
C                                       Create AN table; open for
C                                       write. First initialize.
         CALL DFILL (3, 0.0D0, ARRAYC)
         CALL RFILL (2, 0.0, POLRXY)
         UT1UTC = 0.0
         DATUTC = 0.0
         TIMSYS = 'UTC'
         NUMORB = 6
         NOPCAL = 2
         ANFQID = 1
         XYZHAN = 'RIGHT'
         TFRAME = 'ITRF'
C                                       Open AN table
         IANRNO = 0
         IVER = ISUBA
         CALL ANTINI ('WRIT', JBUFF, IDISK, ICNOUT, IVER, CATBLK, ILUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, DTRFRQ,
     *      LDTDAT, POLRXY, UT1UTC, DATUTC, TIMSYS, LDTARR(J), XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, NDTIF, ANFQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, ISUBA
            GO TO 990
            END IF
C                                       Write AN records
         DO 250 K = 1, NDTANT(J)
            IANRNO = K
C                                       Initialize AN record
C                                       Get Antenna Position
            IF (IDTMNT(K,J).NE.2) THEN
               STAXYZ(1) = DTCPOS(1,K,J)
               STAXYZ(2) = DTCPOS(2,K,J)
               STAXYZ(3) = DTCPOS(3,K,J)
            ELSE
               CALL DFILL (3, 0.0D0, STAXYZ)
               END IF
            STAXOF = 0.0
            POLTYA = 'R '
            POLTYB = 'L '
            POLAA = 0.0
            POLAB = 0.0
C                                       Transfer polzn. solutions
C                                       to AN record if requested
            IF (WDTPOL(J)) THEN
C                                       Loop over IF
               DO 150 M = 1, NDTIF
C                                       Compute D-terms for (t=0)
C                                       and (FQ_ID = 1)
                  CALL DTINDX (LDTANT(K,J), ANFQID, INDXCE)
                  CALL DTMATC (LSOURC, IDTANO(K,J), ISUBA, ANFQID,
     *               JSOU, JANT, JSUBA, JFQ)
                  TIME = 0.0
                  CALL DTCALS (JSOU, JANT, JSUBA, JFQ, M, INDXCE,
     *               TIME, DELAY, DPHASE, DRATE, DACCEL, DFEED, DGAIN,
     *               DTSYS, DAMPL, DGERR)
C                                       Copy D-terms to AN record
                  DO 140 I = 1, 2
                     POLCA(I) = DFEED(I,1)
                     POLCB(I) = DFEED(I,2)
140                  CONTINUE
150               CONTINUE
C                                       Zero polzn. cal.
            ELSE
               NFILL = 2 * MAXIF
               CALL RFILL (NFILL, 0.0, POLCA)
               CALL RFILL (NFILL, 0.0, POLCB)
               END IF
C                                       Write AN record
            CALL TABAN ('WRIT', JBUFF, IANRNO, ANKOLS, ANNUMV,
     *         LDTANT(K,J), STAXYZ, DTORB(1,K,J), IDTANO(K,J),
     *         IDTMNT(K,J), STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *         POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1250) IRET, ISUBA
               GO TO 990
               END IF
250         CONTINUE
C                                       Add polarization solution
C                                       type keyword if requested
         CALL CHBLNK (8, 1, LDTPOL(J), NN)
         IF (NN.NE.0) THEN
            ILOC = 1
            IKEYTP = 3
            CALL CHR2H (8, LDTPOL(J), 1, HSOLTY)
            CALL TABKEY ('WRIT', LPOLTP, 1, JBUFF, ILOC, HSOLTY,
     *         IKEYTP, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1300) IRET, ISUBA
               GO TO 990
               END IF
            END IF
C                                       Close AN table
         CALL TABIO ('CLOS', 0, IANRNO, JBUFF, JBUFF, IRET)
500      CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1010  FORMAT ('DTANTB: ERROR',I4,' CREATING AN TABLE',I4)
1250  FORMAT ('DTANTB: ERROR',I4,' WRITING AN TABLE',I4)
1300  FORMAT ('DTANTB: ERR',I4,' WRITING POLZN. KEYWORD FOR AN',I4)
      END
      SUBROUTINE DTSUTB (ILUN, IRET)
C----------------------------------------------------------------------
C   Create an SU table for a simulated dataset
C   Input:
C      ILUN      I       LUN for table I/O.
C   Output:
C      IRET      I       Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER ILUN, IRET
C
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DTPM.INC'
      DOUBLE PRECISION DSFREQ(MAXIF)
      INTEGER SUKOLS(MAXSUC), SUNUMV(MAXSUC), NFILL, IVER, ISURNO, J
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Create SU table; open
C                                       for write.
      IVER = 1
C                                       Initialise SU table.
      VELTYP = '        '
      VELDEF = '        '
      SUFQID = 1
      ISURNO = 0
C
      CALL SOUINI ('WRIT', JBUFF, IDISK, ICNOUT, IVER, CATBLK, ILUN,
     *   NDTIF, VELTYP, VELDEF, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Write SU records
      DO 250 J = 1, NDTSID
C                                       Initialize SU record
         QUAL = 0
         CALCOD = '    '
         NFILL = 4 * MAXIF
         CALL RFILL (NFILL, 0.0, FLUX)
         CALL DFILL (MAXIF, 0.0D0, DSFREQ)
         BANDW = RDTTBW(1,1)
         CALL DCOPY (MAXIF, DTSUFQ(1,J), 1, RESTFQ, 1)
         CALL DCOPY (MAXIF, DTSLSR(1,J), 1, LSRVEL, 1)
         PMRA = 0.0D0
         PMDEC = 0.0D0
C
         ISURNO = J
         RAOBS = DTSRA(J)
         DECOBS = DTSDEC(J)
         IF (DTEPOC(J).LE.0.0) DTEPOC(J) = 2000.
         CALL TABSOU ('WRIT', JBUFF, ISURNO, SUKOLS, SUNUMV, IDTSID(J),
     *      LDTSOU(J), QUAL, CALCOD, FLUX, DSFREQ, BANDW, DTSRA(J),
     *      DTSDEC(J), DTEPOC(J), DTSRA(J), DTSDEC(J), RAOBS, DECOBS,
     *      LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1250) IRET
            GO TO 990
            END IF
250      CONTINUE
C                                       Close SU table
      CALL TABIO ('CLOS', 0, ISURNO, JBUFF, JBUFF, IRET)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1010  FORMAT ('DTSUTB: ERROR',I4,' CREATING SU TABLE')
1250  FORMAT ('DTSUTB: ERROR',I4,' WRITING TO SU TABLE')
      END
      SUBROUTINE DTHIS (ILUN, IRET)
C----------------------------------------------------------------------
C   Create history (HI) file
C   Input:
C      ILUN     I     LUN for HI table I/O.
C   Output:
C      IRET     I     Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER   ILUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER LHIST*72, LDATE*12, LTIME*8, INLINE*128
      INTEGER   IDATE(3), ITIME(3), IERR, FPHI, VPHI, I, JTRIM, LUN,
     *   IND, J
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Initialize /HICOM/
      CALL HIINIT (3)
C                                       Create/open history file.
      CALL HICREA (ILUN, IDISK, ICNOUT, CATBLK, BUFF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 900
         END IF
C                                       Write HI records
C                                       Get current date/time
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, LTIME, LDATE)
      WRITE (LHIST,1010) TSKNAM, NLUSER, LDATE, LTIME
      CALL HIADD (ILUN, LHIST, BUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       default values
      IDATE(1) = DPARMS(1) + 0.1
      IDATE(2) = DPARMS(2) + 0.1
      IDATE(3) = DPARMS(3) + 0.1
      IF ((IDATE(1).GT.0) .OR. (IDATE(2).GT.0) .OR. (IDATE(3).GT.0))
     *   THEN
         WRITE (LHIST,1020) TSKNAM, IDATE
         CALL HIADD (ILUN, LHIST, BUFF, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      FPHI = 0
      VPHI = 0
      DO 30 I = 1,30
         IF (DPARMS(10+I).GT.0.0) FPHI = I
         IF (DPARMS(40+I).GT.0.0) VPHI = I
 30      CONTINUE
      DO 40 I = 1,FPHI
         WRITE (LHIST,1030) TSKNAM, I, DPARMS(10+I), I
         CALL HIADD (ILUN, LHIST, BUFF, IERR)
         IF (IERR.NE.0) GO TO 100
 40      CONTINUE
      DO 50 I = 1,FPHI
         WRITE (LHIST,1040) TSKNAM, I, DPARMS(40+I), I
         CALL HIADD (ILUN, LHIST, BUFF, IERR)
         IF (IERR.NE.0) GO TO 100
 50      CONTINUE
C                                       INFILE
      I = JTRIM (LINFIL)
      WRITE (LHIST,1050) TSKNAM, LINFIL(:I)
      CALL HIADD (ILUN, LHIST, BUFF, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       open
      LUN = 3
      CALL ZTXOPN ('READ', LUN, IND, LINFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'HISTORY ROUTINE FAILS TO OPEN INFILE'
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       read loop
 60   CALL ZTXIO ('READ', LUN, IND, INLINE, IERR)
      IF (IERR.EQ.0) THEN
         I = JTRIM (INLINE)
         J = MIN (I, 64)
         LHIST = TSKNAM // '/ ' // INLINE(1:J)
         CALL HIADD (ILUN, LHIST, BUFF, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (I.GT.64) THEN
            J = MIN (I, 128)
            LHIST =  '      / ' // INLINE(65:J)
            CALL HIADD (ILUN, LHIST, BUFF, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         GO TO 60
      ELSE IF (IERR.NE.2) THEN
         MSGTXT = 'HISTORY ROUTINE FAILS WHILE READING INFILE'
         CALL MSGWRT (6)
         END IF
      CALL ZTXCLS (LUN, IND, IERR)
C
 100  CALL HICLOS (ILUN, .TRUE., BUFF, IERR)
C                                       Update CATBLK
 900  CALL CATIO ('UPDT', IDISK, ICNOUT, CATBLK, 'REST', BUFF, IERR)
C                                       Exit
 999  RETURN
C----------------------------------------------------------------------
 1000 FORMAT ('DTHIS: ERROR ',I4,' CREATING HI FILE')
 1010 FORMAT (A6,'/created by user ',I5,' at ',A12,2X,A8)
 1020 FORMAT (A6,'DPARM=',I5,',',I3,',',I3,
     *   '  / default year, month, day')
 1030 FORMAT (A6,'FPARM(',I2.2,')=',F8.3,'  / default gain (Jy/K), IF',
     *   I3)
 1040 FORMAT (A6,'VPARM(',I2.2,')=',F8.2,'  / default Tsys (K), IF',I3)
 1050 FORMAT (A6,'INFILE=''',A,''' / KEYIN')
      END
      SUBROUTINE GETGST (DATE, GAST0, GASTR)
C----------------------------------------------------------------------
C   Compute GAST0 and GASTR(radians), given DATE(julian days)
C   Input:
C      DATE   D  Julian date (Julian days)
C   Output:
C      GAST0  D  GAST on DATE at UT0 (radians)
C      GASTR  D  GAST rate on DATE at UT0 (radians/day)
C                (both computed only if DATE has changed since
C                 last call)
C----------------------------------------------------------------------
      DOUBLE PRECISION DATE, GAST0, GASTR
C
      DOUBLE PRECISION DELPSI, DELEPS, SPSI, SEPS, TC, EPS, EQEQ,
     *   GMSTM, LASDAT, GST0, B1950, BESCEN, J1900, JULCEN, GSTR
      SAVE LASDAT, GST0, GSTR, EQEQ
      INCLUDE 'INCS:PSTD.INC'
      DATA LASDAT, GST0, GSTR, EQEQ /0.0D0, 0.0D0, 0.0D0, 0.0D0/
C----------------------------------------------------------------------
C                                       This date already processed ?
      IF (DATE.NE.LASDAT) THEN
C                                       Set some constants
C                                       [all dates and intervals are
C                                        measured in Julian day units]
         B1950 = 2433282.423D0
         BESCEN = 36524.21988D0
         J1900 = 2415020.0D0
         JULCEN = 36525.0D0
C
         LASDAT = DATE
C                                       Get GST(rad) at UT=0
         TC = (DATE - J1900) / JULCEN
         GMSTM = (8640184.542D0 * TC) / 86400.0D0
         GMSTM = MOD (GMSTM, 1.0D0) +
     *        (6.0D0 + (38.0D0 + 45.836D0/60.0D0) / 60.0D0) / 24.0D0 +
     *        (0.0929D0 * TC * TC) / 86400.0D0
         GST0 = GMSTM * TWOPI
C                                       Compute Advance of Equinoxes(rad)
         CALL NUT4 (DATE, DELPSI, DELEPS, SPSI, SEPS)
         TC = (DATE - B1950) / BESCEN
         EPS = (-46.850D0 + (-0.0034D0 + 0.0018D0 * TC) * TC) * TC
         EPS = (84404.84D0 + EPS) * AS2RAD
         EQEQ = DELPSI * COS (EPS)
C                                       Get GSTdot(rad)
         GSTR = (1.00273790265D0 + 0.589D-10 * TC) * TWOPI
         ENDIF
      GAST0 = GST0 + EQEQ
      GASTR = GSTR
      RETURN
      END
      SUBROUTINE GETBAS (TIME, GAST, RA , DEC, DX, DY, DZ,
     *   MNTYPA, MNTYPB, CPOSA, CPOSB, ORBA, ORBB, IRET)
C----------------------------------------------------------------------
C   Compute antenna separation in meters, also check source elevation
C   Inputs:
C      TIME       D     UT  (hours)
C      GAST       D     Greenwich Apparent Sidereal Time (radians)
C      RA         D     J2000 Right ascension (decimal degrees)
C      DEC        D     J2000 Declination (decimal degrees)
C      MNTYP{A,B} I     mount type of antennas
C      LONG{A,B}  D     west longitude of antennas (degrees)
C      LAT{A,B}   D     north latitude of antennas  (degrees)
C      ELEV{A,B}  D(*)  elevation of antennas above earth (meters)
C      ORB{A,B}   D(*)  orbital parameters for antennas [if applicable]
C   Output:
C      DX,DY,DZ   D     separation of antennas in meters
C      IRET       I     return code, 0=>okay,
C                                    1=> not visible at both antennas
C----------------------------------------------------------------------
      DOUBLE PRECISION TIME, GAST, RA, DEC, DX, DY, DZ,
     *   CPOSA(3), CPOSB(3), ORBA(*), ORBB(*)
      INTEGER MNTYPA, MNTYPB, IRET
C
      DOUBLE PRECISION X, Y, Z
C----------------------------------------------------------------------
      IRET = 0
C                                       Get first antenna's position
      IF (MNTYPA.NE.2) THEN
         X = CPOSA(1)
         Y = CPOSA(2)
         Z = CPOSA(3)
C                                       Check source elevation
         CALL CHKELE (X, Y, Z, RA, DEC, GAST, IRET)
         IF (IRET.GT.0) GO TO 999
      ELSE
         CALL ORBPOS (ORBA, TIME, GAST, X, Y, Z)
         ENDIF
      DX = X
      DY = Y
      DZ = Z
C                                       Get second antenna's position
      IF (MNTYPB.NE.2) THEN
         X = CPOSB(1)
         Y = CPOSB(2)
         Z = CPOSB(3)
         CALL CHKELE (X, Y, Z, RA, DEC, GAST, IRET)
         IF (IRET.GT.0) GO TO 999
      ELSE
         CALL ORBPOS (ORBB, TIME, GAST, X, Y, Z)
         ENDIF
      DX = DX - X
      DY = DY - Y
      DZ = DZ - Z
 999  RETURN
      END
      SUBROUTINE GEOGEO (X, Y, Z, LON, LAT, ELE)
C----------------------------------------------------------------------
C   Returns geodetic (surface of the earth) coordinates, given
C   cartesian coordinates.
C   Output:
C      LON    D   geodetic west longitude (degrees)
C      LAT    D   geodetic north latitude (degrees)
C      ELE    D   geodetic elevation (meters)
C   Input:
C      X      D   x coord, X-axis lies along Greenwich Longitude
C      Y      D   y coord, Y-axis lies along N.American Longitude
C      Z      D   z coord, Z-axis lies passes through North Pole
C---------------------------------------------------------------------
      DOUBLE PRECISION LON, LAT, ELE, X, Y, Z
C
      DOUBLE PRECISION ECC2, ELIPR
      DOUBLE PRECISION SMAXIS, FLATEN, ERAD, NLAT
      PARAMETER (SMAXIS = 6378136.D0)
      PARAMETER (FLATEN = 1.D0/298.257)
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
C                                       these formulas come from the
C                                       Astronomical Almanac 1996, pg K12.
C                                       longitude is easy
      LON = ATAN2 (Y, X) * RAD2DG
C                                       latitude is not
C
C                                       work out axial displacement
      ERAD = SQRT(X*X + Y*Y)
C                                       work out earth oblateness
      ECC2 = (2.D0 - FLATEN) * FLATEN
C                                       start the ball rolling - so to speak
      LAT = 0.0D0
 10   CONTINUE
C                                       get earth depth at current latitude
      ELIPR = SMAXIS / (SQRT (1.D0 - ECC2 * SIN(LAT)**2 ))
C                                       recompute latitude
      NLAT = ATAN2 (Z + ELIPR*ECC2*SIN(LAT), ERAD)
C                                       perform reduction of the latitude
      IF (ABS(NLAT-LAT).GT.1.0D-6) THEN
         LAT = NLAT
         GO TO 10
         END IF
C                                       finally, first extract elevation
      ELE = ERAD / COS (LAT) - ELIPR
C                                       then latitude
      LAT = LAT * RAD2DG
      RETURN
      END
      SUBROUTINE ORBPOS (ORBPAR, TIME, GAST, X, Y, Z)
C----------------------------------------------------------------------
C   Given orbital parameters, return cartesian coordinates
C   Input:
C      ORBPAR   D(*)   Orbital parameters
C      TIME     D      UT at which orbital position is desired
C      GAST     D      Greenwich Sidereal Time when position is desired
C   Output:
C      X        D      x coord, X-axis lies along Greenwich Longitude
C      Y        D      y coord, Y-axis lies along N.American Longitude
C      Z        D      z coord, Z-axis lies passes through North Pole
C----------------------------------------------------------------------
      DOUBLE PRECISION ORBPAR(*), TIME, GAST, X, Y, Z
      DOUBLE PRECISION RALONG, INCLIN, PERIG, ECCEN, SEMMAJ, MEAANO
      DOUBLE PRECISION TIMSEC, TRUANO, RORBMA, RORBMI, VORBMA, VORBMI
      DOUBLE PRECISION XCELE, YCELE, ZCELE, VXCELE, VYCELE, VZCELE
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
      SEMMAJ = ORBPAR(1)
      ECCEN  = ORBPAR(2)
      INCLIN = ORBPAR(3)*DG2RAD
      RALONG = ORBPAR(4)*DG2RAD
      PERIG  = ORBPAR(5)*DG2RAD
      MEAANO = ORBPAR(6)*DG2RAD
      TIMSEC = TIME*8.64D4
      CALL ORBIT (SEMMAJ, ECCEN, INCLIN, RALONG, PERIG, MEAANO,
     *     TIMSEC, TRUANO, RORBMA, RORBMI, VORBMA, VORBMI,
     *     XCELE, YCELE, ZCELE, VXCELE, VYCELE, VZCELE)
      X = XCELE*COS(GAST) + YCELE*SIN(GAST)
      Y = -YCELE*COS(GAST) + XCELE*SIN(GAST)
      Z = ZCELE
      RETURN
      END
      SUBROUTINE CHKELE (X, Y, Z, RA, DEC, GAST, IRET)
C----------------------------------------------------------------------
C   Check that the source is visible from the given XYZ position
C   Inputs
C      X,Y,Z  D  antenna vector (meters), RHS AIPS-VLBI coord system
C      RA,DEC D  apparent source coordinates (decimal deg), mean is ok
C      GAST   D  Greenwich Apparent Sidereal Time [radians]
C   Outputs
C      IRET   I  return code, 0=> source is visible
C                             1=> source is not visible
C----------------------------------------------------------------------
      DOUBLE PRECISION X, Y, Z, RA, DEC, GAST
      INTEGER IRET
C
      DOUBLE PRECISION INPROD, GHRA, RDEC
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
      IRET = 0
      GHRA = GAST - RA * DG2RAD
      RDEC = DEC * DG2RAD
C
      INPROD = X * COS (RDEC) * COS (GHRA)
     *     -   Y * COS (RDEC) * SIN (GHRA)
     *     +   Z * SIN (RDEC)
C
      IF (INPROD.LE.0.0) IRET = 1
      RETURN
      END
      SUBROUTINE CALCUV (DATE, TIME, LAST, RA, DEC, U, V, W, RAAPP,
     *   DECAPP)
C----------------------------------------------------------------------
C   Calculate (u,v,w) coordinates.
C   Input:
C      DATE    D  julian date [days]
C      TIME    D  UT time [days]
C      LAST    D  Local Apparent Sidereal Time [radians]
C      RA      D  J2000 ra [decimal degrees]
C      DEC     D  J2000 dec [decimal degrees]
C      U,V,W   D  baseline separations measured in lambdas
C   Output:
C      U,V,W   D  uvw coordinates
C      RAAPP   D  Apparent right ascension (radians)
C      DECAPP  D  Apparent declination (radians)
C----------------------------------------------------------------------
      DOUBLE PRECISION DATE, TIME, LAST, RA, DEC, U, V, W, RAAPP,
     *   DECAPP
C
      DOUBLE PRECISION ROTANG, CROTP, SROTP, JD, OBSPOS(3)
      REAL POLAR(2)
      LOGICAL GR
      INTEGER DIR
      DOUBLE PRECISION EPOCH, DELDAT, RHOGEO, PHIGEO
      DOUBLE PRECISION RAMEA, DECMEA, COSD, SIND, SINHA, COSHA
      DOUBLE PRECISION UT, VT, WT
      LOGICAL T
      INCLUDE 'INCS:PSTD.INC'
      DATA T /.TRUE./
      DATA OBSPOS, POLAR /3*0.0D0, 2*0.0/
C----------------------------------------------------------------------
C                                       Convert to radians
      RAMEA = RA * DG2RAD
      DECMEA = DEC * DG2RAD
C                                       Get sky rotation angle in rad.
      CALL GETROT (DATE, RAMEA, DECMEA, ROTANG)
      CROTP = COS (ROTANG)
      SROTP = SIN (ROTANG)
C
      JD = DATE + TIME
      EPOCH = 2000.0
      DELDAT = 0.5
      DIR = 1
      GR = T
      RHOGEO = 0.0
      PHIGEO = 0.0
C                                       Precess position to the apparent
C                                       coordinates.
      CALL JPRECS (JD, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR, RAMEA,
     *      DECMEA, RAAPP, DECAPP)
C      CALL PRECES (JD, EPOCH, DELDAT, RAMEA, DECMEA, RAAPP, DECAPP,
C     *   DIR, GR, DA, RHOGEO, PHIGEO, LAST, NR)
      COSD = COS (DECAPP)
      SIND = SIN (DECAPP)
      SINHA = SIN (LAST - RAAPP)
      COSHA = COS (LAST - RAAPP)
C                                       Start with apparent baseline
C                                       separations
      UT = U
      VT = V
      WT = W
C                                       Rotate to apparent RA of source
      U = SINHA * UT + COSHA * VT
      V = COSHA * UT - SINHA * VT
      W = WT
C                                       Rotate to apparent DEC of
C                                       source
      UT = U
      VT = -SIND * V + COSD * W
      WT = COSD * V + SIND * W
C                                       Rotate to orientation of mean
C                                       epoch
      U = CROTP * UT + SROTP * VT
      V = -SROTP * UT + CROTP * VT
      W = WT
      RETURN
      END
      SUBROUTINE GETROT (DATE, RA, DEC, ROTANG)
C----------------------------------------------------------------------
C   Get rotation of sky about apparent position given mean position
C   Input:
C      DATE   D  julian date at UT=0
C      RA     D  J2000 RA in radians
C      DEC    D  J2000 DEC in radians
C   Output:
C      ROTANG D  sky rotation factor for the epoch of DATE
C            remembers last LSIZE rotation angles computed
C----------------------------------------------------------------------
      DOUBLE PRECISION DATE, RA, DEC, ROTANG, OBSPOS(3)
C
      DOUBLE PRECISION EPOCH, DELDAT, RHOGEO, PHIGEO, LAST
      DOUBLE PRECISION RAMEA, DECMEA, RAAPP, DECAPP, DELDEC, DELRA
      REAL POLAR(2)
      LOGICAL GR, DA, NR
      INTEGER I,LNO, LSIZE, DIR
      PARAMETER (LSIZE=10)
      DOUBLE PRECISION LDATE(LSIZE), LRA(LSIZE), LDEC(LSIZE)
      DOUBLE PRECISION LROTAN(LSIZE), ACCEPT
      LOGICAL T, F
      DATA LNO /0/
      DATA LDATE, LRA, LDEC /LSIZE*0.D0,LSIZE*0.D0,LSIZE*0.D0/
      DATA LROTAN /LSIZE*0.D0/
      DATA ACCEPT /0.0D0/
      DATA T, F /.TRUE.,.FALSE./
      DATA OBSPOS, POLAR /3*0.0D0, 2*0.0/
C----------------------------------------------------------------------
C                                       If stack is not empty,search it
      IF (LNO.GT.0) THEN
         DO 100, I = 1, LNO
            IF ( (  ABS(LRA(I)-RA).LT.ACCEPT) .AND.
     *           (ABS(LDEC(I)-DEC).LT.ACCEPT) .AND.
     *           (        LDATE(I).EQ.DATE  )      ) THEN
               ROTANG = LROTAN(I)
               GOTO 999
               ENDIF
 100        CONTINUE
         ENDIF
C                                       If stack is full, roll it
      IF (LNO.EQ.LSIZE) THEN
         DO 200 I = 1,LSIZE-1
            LDATE(I) = LDATE(I+1)
            LRA(I) = LRA(I+1)
            LDEC(I) = LDEC(I+1)
            LROTAN(I) = LROTAN(I+1)
 200        CONTINUE
         LNO = LSIZE-1
         ENDIF
C                                       Get apparent pos for offset pos
C                                       assume all coords in J2000
      EPOCH = 2000.0
C                                       Use an interpolation interval
C                                       of 1/2 day
      DELDAT = 0.5D0
C                                       Arbitrarily choose ~20 asec
C                                       dec offset in radians
      RAMEA = RA
      DECMEA = DEC + 0.0001D0
      DIR = 1
      GR = T
      DA = F
      RHOGEO = 0.0
      PHIGEO = 0.0
      LAST = 0.0
      NR = T
C                                       Precess position to the apparent
C                                       coordinates.
      CALL JPRECS (DATE, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR, RAMEA,
     *      DECMEA, RAAPP, DECAPP)
C      CALL PRECES (DATE, EPOCH, DELDAT, RAMEA, DECMEA, RAAPP, DECAPP,
C     *     DIR, GR, DA, RHOGEO, PHIGEO, LAST, NR)
      DELDEC = DECAPP
      DELRA = RAAPP
C                                       Get apparent pos for mean pos.
      RAMEA = RA
      DECMEA = DEC
C                                       Precess position to the apparent
C                                       coordinates.
      CALL JPRECS (DATE, EPOCH, DELDAT, DIR, GR, OBSPOS, POLAR, RAMEA,
     *      DECMEA, RAAPP, DECAPP)
C      CALL PRECES (DATE, EPOCH, DELDAT, RAMEA, DECMEA, RAAPP, DECAPP,
C     *     DIR, GR, DA, RHOGEO, PHIGEO, LAST, NR)
C                                       Del[ra,dec] is apparent offset
      DELDEC = DELDEC - DECAPP
      DELRA = (DELRA - RAAPP) * COS(DECMEA)
C                                       Rotang is rotation apparent offset
      ROTANG = -ATAN2 (DELRA, DELDEC)
C                                       Increment stack, store basics
      LNO = LNO + 1
      LDATE(LNO) = DATE
      LRA(LNO) = RA
      LDEC(LNO) = DEC
      LROTAN(LNO) = ROTANG
C                                       Done, end
 999  RETURN
      END
      SUBROUTINE ADNOIZ (FLUX, FLUXN, GAIN, VIS, SEED, IRET)
C----------------------------------------------------------------------
C   Produce noisy data given noise-free data and noise info
C   Input:
C      FLUX   CX(4) Full polarization noise-free visibility model
C      FLUXN  R(4)  Full polarization noise flux
C      GAIN   CX(4) Full polarization antenna gains
C      SEED   R     seed for random number generator, used only once
C   Output:
C      VIS    CX(4) Noisy full polarization visibility model
C      IRET   I     on first call, IRET=0 means portable RND calls
C                                  IRET=1 means non-portable RND calls
C                   for later calls, IRET
C----------------------------------------------------------------------
      COMPLEX FLUX(4), GAIN(4), VIS(4)
      REAL FLUXN(4), SEED
      INTEGER IRET
C                                       0 <= ADRAND < 1
      REAL ADRAND
      EXTERNAL ADRAND
      REAL SIGMAR, SIGMAI, SIGMA
      INTEGER I
      REAL R
      INTEGER  SEEDED
      SAVE SEEDED
      DATA SEEDED /0/
C----------------------------------------------------------------------
      IRET = 0
      IF (SEEDED.EQ.0) THEN
         SEEDED = 1
         SEED = MOD(SEED, 1.0)
         R = ADRAND(SEED)
         IF (SEED.NE.R) IRET = 1
      ELSE
C                                       R=0.0 means continue current
C                                       seq. of random #s
         R = 0.0
         DO 110, I = 1,4
C                                       Generate complex random # in
C                                       unit circle
 100        CONTINUE
            SIGMAR = ADRAND(R) - 0.5
            SIGMAI = ADRAND(R) - 0.5
            SIGMA = SIGMAR * SIGMAR + SIGMAI * SIGMAI
            IF (SIGMA.GT.0.25) GOTO 100
C                                       Construct measured visibility
            IF (SIGMA*FLUXN(I) .NE. 0.0) THEN
               SIGMA = FLUXN(I)*SQRT(-4.0*LOG(4.0*SIGMA))/ SQRT (SIGMA)
               VIS(I) = GAIN(I) * FLUX(I) + CMPLX (SIGMAR,SIGMAI)
     *            * SIGMA
            ELSE
               VIS(I) = GAIN(I) * FLUX(I)
               ENDIF
 110        CONTINUE
         ENDIF
 999  CONTINUE
      RETURN
      END
      SUBROUTINE ADDMOD (TYP, PAR, UCORD, VCORD, FLUX)
C----------------------------------------------------------------------
C   Add a point source model, described by MODPAR to I,Q,U,V
C   Input:
C      TYP      I     model type
C      PAR      R(*)  model parameters
C      UCORD    R     ucoordinate of baseline
C      VCORD    R     vcoordinate of baseline
C      FLUX     CX(4) complex visibility 4-vector
C   Output:
C      FLUX     CX(4) complex visibility 4-vector
C---------------------------------------------------------------------
      INTEGER   TYP
      REAL      PAR(*)
      DOUBLE PRECISION UCORD, VCORD
      COMPLEX   FLUX(4)
C
      INTEGER I
      REAL RTEMP, SKYPHS
      COMPLEX CTEMP
      INCLUDE 'INCS:PSTD.INC'
C----------------------------------------------------------------------
C                                       Ignore entry if TYPE(1) is <0.
      IF (TYP.LT.0) GOTO 990
      IF (TYP.EQ.0) THEN
         DO 10 I = 1,4
            FLUX(I) = 0.0
 10         CONTINUE
         GOTO 990
         ENDIF
C                                       B22NUL converts U*U*size*size
C                                       to gaus.
C                                       MODPAR(1) offset in ra (asec)
C                                       MODPAR(2) offset in dec (asec)
         SKYPHS = UCORD * PAR(1) + VCORD * PAR(2)
         SKYPHS = SKYPHS * AS2RAD * TWOPI
C                                       If MODTYP = 1
C                                       MODPAR(3-6) flux in I,Q,U,V (Jy)
      IF (TYP.EQ.1) THEN
         DO 100 I = 1,4
            FLUX(I) = FLUX(I) + PAR(2+I) * CEXP (CMPLX(0.0,SKYPHS))
 100        CONTINUE
         ENDIF
C                                       If MODTYP = 2
C                                       MODPAR(3) position angle (deg)
C                                       MODPAR(4) major axis (asec)
C                                       MODPAR(5) minor axis (asec)
C                                       MODPAR(6-9) I flux (Jy)
C                                       exp(-11.602077) = (pi*pi) /
C                                          360^2*sqrt(100ln2)
      IF (TYP.EQ.2) THEN
         RTEMP = PAR(3) * DG2RAD
         CTEMP = CMPLX (UCORD, VCORD) * CEXP(CMPLX (-11.602077, RTEMP))
         RTEMP = (AIMAG (CTEMP) * PAR(4)) **2 +
     *      (REAL (CTEMP) * PAR(5)) **2
         IF (RTEMP.LT.20.0) THEN
            DO 200 I = 1,4
               FLUX(I) = FLUX(I) + PAR(5+I) *
     *            CEXP (CMPLX (-RTEMP, SKYPHS))
 200           CONTINUE
            ENDIF
         ENDIF
 990  CONTINUE
      RETURN
      END
      SUBROUTINE ORIENT (JA1, JA2, JSUBA, DGAST, DRAP, DDECP, PANG)
C----------------------------------------------------------------------
C   Compute parallactic angle
C   Inputs:
C      JA1       I       Ant_1 index
C      JA2       I       Ant_2 index
C      JSUBA     I       Subarray index
C      DGAST     D       GAST (radians)
C      DRAP      D       Apparent right ascension (radians)
C      DDECP     D       Apparent declination (radians)
C   Output:
C      PANG      R(2)    Parallactic angle for (IA1,IA2) in radians
C----------------------------------------------------------------------
      DOUBLE PRECISION DGAST, DRAP, DDECP
      REAL PANG(2)
      INTEGER JA1, JA2, JSUBA
C
      INCLUDE 'INCS:DTPM.INC'
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION DLST, DHRANG, DLATR, DLONGR, DELEV
      INTEGER   JA(2), J, JANT, ISIGN
C----------------------------------------------------------------------
C                                       Initialization
      JA(1) = JA1
      JA(2) = JA2
      CALL RFILL (2, 0.0, PANG)
C                                       Loop over each antenna
      DO 200 J = 1, 2
         JANT = JA(J)
C                                       Get Latitude and longitude (rad)
         IF (IDTMNT(JANT,JSUBA).NE.2) THEN
            CALL GEOGEO (DTCPOS(1,JANT,JSUBA), DTCPOS(2,JANT,JSUBA),
     *         DTCPOS(3,JANT,JSUBA), DLONGR, DLATR, DELEV)
            DLONGR = DLONGR * DG2RAD
            DLATR = DLATR * DG2RAD
            END IF
C                                       Compute parallactic angle
C                                       only for alt-az antennas
C                                       for now (orbiters later).
C                                       incl X-Y and Nasmyth
         IF ((IDTMNT(JANT,JSUBA).NE.1) .AND.
     *         (IDTMNT(JANT,JSUBA).NE.2)) THEN
C                                       LST
            DLST = DGAST - DLONGR
C                                       Hour angle
            DHRANG = DLST - DRAP
C                                       Parallactic angle
            PANG(J) = ATAN2 (COS (DLATR) * SIN (DHRANG),
     *         (SIN (DLATR) * COS (DDECP) -
     *         COS (DLATR) * SIN (DDECP) * COS (DHRANG)))
            END IF
C                                       EW-Mount
         IF (IDTMNT(JANT,JSUBA).EQ.3) THEN
            PANG(J) = ATAN2(COS(DHRANG),SIN(DHRANG)*SIN(DDECP))
C                                       Nasmyth
         ELSE IF ((IDTMNT(JANT,JSUBA).EQ.4) .OR.
     *      (IDTMNT(JANT,JSUBA).EQ.5)) THEN
            ISIGN = 1
            IF (IDTMNT(JANT,JSUBA).EQ.5) ISIGN = -ISIGN
            PANG(J) = PANG(J) + ISIGN * DELEV
            END IF
 200     CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE DTERMS (FLUXIN, FLXOUT, DFEED1, DFEED2, PANG, WLINP)
C----------------------------------------------------------------------
C   Correct true cross-correlations for intrumental polzn. response
C   Input:
C      FLXIN     C(4)     Input correlation pairs (RR,LL,RL,LR)
C      DFEED1    D(2,2)   Instrumental polzn. for ant_1 (Re,Im; R,L)
C      DFEED2    D(2,2)   Instrumental polzn. for ant_2 (Re,Im; R,L)
C      PANG      R(2)     Parallactic angles (ant_1, ant_2) (rad)
C      WLINP     L        True if linearized polzn. model required,
C                         else include all polzn. terms.
C   Output:
C      FLXOUT    C(4)     Corrupted correlation pairs (RR,LL,RL,LR)
C----------------------------------------------------------------------
      LOGICAL WLINP
      COMPLEX FLUXIN(4), FLXOUT(4)
      DOUBLE PRECISION DFEED1(2,2), DFEED2(2,2)
      REAL PANG(2)
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL WZERO
      COMPLEX CSUM, CDIFF, CSUMC, CDIFFC, CFEED(2,2), CFEEDC(2,2),
     *   CZERO
      DOUBLE PRECISION DSUM, DIFF
      INTEGER J, IANT, JANT, IRCP, ILCP, IRR, ILL, IRL, ILR
      DATA IANT, JANT, IRCP, ILCP /1, 2, 1, 2/
      DATA IRR, ILL, IRL, ILR /1, 2, 3, 4/
C----------------------------------------------------------------------
C                                       Initialization
      DO 50 J = 1, 4
         FLXOUT(J) = FLUXIN(J)
50       CONTINUE
C                                       Complex feed terms
      CZERO = CMPLX (0.0, 0.0)
      WZERO = .TRUE.
      DO 100 J = 1, 2
         CFEED(J,IANT) = CMPLX (DFEED1(1,J), DFEED1(2,J))
         CFEED(J,JANT) = CMPLX (DFEED2(1,J), DFEED2(2,J))
         IF ((CFEED(J,IANT).NE.CZERO).OR.(CFEED(J,JANT).NE.CZERO))
     *      WZERO = .FALSE.
C                                       Conjugated feed terms
         CFEEDC(J,IANT) = CONJG (CFEED(J,IANT))
         CFEEDC(J,JANT) = CONJG (CFEED(J,JANT))
100      CONTINUE
C                                       Exit if zero D-terms
      IF (WZERO) GO TO 999
C                                       Parallactic angle terms
C                                       (sum and difference)
      DSUM = PANG(1) + PANG(2)
      DIFF = PANG(1) - PANG(2)
      CSUM = CMPLX (COS (DSUM), SIN (DSUM))
      CDIFF = CMPLX (COS (DIFF), SIN (DIFF))
C                                       Conjugated terms
      CSUMC = CONJG (CSUM)
      CDIFFC = CONJG (CDIFF)
C                                       First include linear terms only
C                                       RR:
      FLXOUT(IRR) = FLUXIN(IRR) * CDIFFC
C                                       LL:
      FLXOUT(ILL) = FLUXIN(ILL) * CDIFF
C                                       RL:
      FLXOUT(IRL) = FLUXIN(IRL) * CSUMC +
     *   FLUXIN(IRR) * CFEEDC(ILCP,JANT) * CDIFFC +
     *   FLUXIN(ILL) * CFEED(IRCP,IANT) * CDIFF
C                                       LR:
      FLXOUT(ILR) = FLUXIN(ILR) * CSUM +
     *   FLUXIN(ILL) * CFEEDC(IRCP,JANT) * CDIFF +
     *   FLUXIN(IRR) * CFEED(ILCP,IANT) * CDIFFC
C                                       Second-order terms if requested
      IF (.NOT.WLINP) THEN
C                                       RR:
         FLXOUT(IRR) = FLXOUT(IRR) +
     *      FLUXIN(IRL) * CFEEDC(IRCP,JANT) * CSUMC +
     *      FLUXIN(ILR) * CFEED(IRCP,IANT) * CSUM +
     *      FLUXIN(ILL) * CFEED(IRCP,IANT) * CFEEDC(IRCP,JANT) * CDIFF
C                                       LL:
         FLXOUT(ILL) = FLXOUT(ILL) +
     *      FLUXIN(ILR) * CFEEDC(ILCP,JANT) * CSUM +
     *      FLUXIN(IRL) * CFEED(ILCP,IANT) * CSUMC +
     *      FLUXIN(IRR) * CFEED(ILCP,IANT) * CFEEDC(ILCP,JANT) * CDIFFC
C                                       RL:
         FLXOUT(IRL) = FLXOUT(IRL) +
     *      FLUXIN(ILR) * CFEED(IRCP,IANT) * CFEEDC(ILCP,JANT) * CSUM
C                                       LR:
         FLXOUT(ILR) = FLXOUT(ILR) +
     *      FLUXIN(IRL) * CFEED(ILCP,IANT) * CFEEDC(IRCP,JANT) * CSUMC
         END IF
C                                       Exit
999   RETURN
      END
      SUBROUTINE DTBPTB (ILUN, IRET)
C----------------------------------------------------------------------
C   Create a BP table for a simulated dataset
C   Input:
C      ILUN      I       LUN for table I/O
C   Output:
C      IRET      I       Return code (0=>ok; else error)
C----------------------------------------------------------------------
      INTEGER ILUN, IRET
C
      INCLUDE 'INCS:DTPM.INC'
      LOGICAL WPOLY
      DOUBLE PRECISION DAMPL, DTIME, DSHFT(MAXIF), DXSHF
      COMPLEX BAND(2,MAXCHA)
      REAL RLOW, RDEL, TIME, TIMEC, WEIGHT(2*MAXIF),
     *   BNDPAS(2*MAXCIF), BANDW, PARM(MAXERP)
      INTEGER IBPRNO, IBPKOL(MAXBPC), IBPNMV(MAXBPC), IBCHAN, JROW,
     *   INDXCE, JIF, JPOL, J, K, NFILL, NN, M, JCHAN, JSUBA, JANT,
     *   IANT, ISOU, ISUBA, IREFA(2), IERR, I, IVER, NPOL, NZERO
      INCLUDE 'DTSIM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      IF (NDTBPD.EQ.0) GO TO 999
C                                       Create BP table
      IBCHAN = 1
      RLOW = 0.0
      RDEL = 0.0
      NPOL = MIN (2, NDTSTK)
      IVER = IDTBPV
      CALL BPINI ('WRIT', JBUFF, IDISK, ICNOUT, IVER, CATBLK, ILUN,
     *   IBPRNO, IBPKOL, IBPNMV, NDTBPA, NPOL, NDTIF, NDTCHN,
     *   IBCHAN, IDTBPN, RLOW, RDEL, LDTBPT, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Polynomial BP ?
      CALL CHBLNK (8, 1, LDTBPT, NN)
      WPOLY = (NN.NE.0)
C                                       Loop over BP entries
      DO 500 JROW = 1, NDTBPD
C                                       Start time
         TIME = RDTBPT(JROW)
C                                       Repeat:
C                                       Compute BP for center of this
C                                       solution interval.
100      TIMEC = TIME + RDTBPS(JROW) / 2.0
C                                       Loop over polarization
         DO 250 JPOL = 1,NPOL
C                                       Loop over IF
            DO 200 JIF = 1,NDTIF
C                                       Default unit bandpass
               DO 150 J = 1,NDTCHN
                  BAND(1,J) = (1.0, 0.0)
                  BAND(2,J) = (1.0, 0.0)
150               CONTINUE
C                                       Use calibration errors ?
               IF (IDTBPC(JROW).EQ.1) THEN
                  CALL DTINDX (LDTBAN(JROW), IDTBFQ(JROW), INDXCE)
C                                       Check for polynomial BP
                  IF (INDXCE.LE.0) THEN
                     CALL RFILL (MAXERP, 0.0, PARM)
                  ELSE
                     CALL RCOPY (MAXERP, RDTBP(1,JIF,JPOL,INDXCE), PARM)
                     END IF
                  IF (WPOLY) THEN
C                                       Transfer coeff. directly
                     NZERO = 0
C                                       Check for null BP
                     DO 155 M = 2, 9
                        IF (PARM(M).EQ.0.0) NZERO = NZERO + 1
155                     CONTINUE
                     IF (NZERO.EQ.8) PARM(2) = 2.0
C
                     DO 160 M = 1, NDTCHN
                        IF (M.LE.4) THEN
                           BAND(JPOL,M) = CMPLX (PARM(M+1), PARM(M+5))
                        ELSE
                           BAND(JPOL,M) = CMPLX (FBLANK, FBLANK)
                           END IF
160                     CONTINUE
                  ELSE
C                                       Simulate BP
                     DXSHF = 0.0D0
                     CALL DTBAND (INDXCE, JIF, TIMEC, DXSHF, BAND)
                     END IF
                  END IF
C                                       Fill BREAL,BIMAG arrays
C                                       (with flagging)
               DO 175 JCHAN = 1, NDTCHN
                  K = JCHAN + NDTCHN * ((JIF - 1) + NDTIF * (JPOL
     *               - 1))
                  BNDPAS(2 * K - 1) = REAL (BAND(JPOL,JCHAN))
                  BNDPAS(2 * K)      = AIMAG (BAND(JPOL,JCHAN))
C                                       Is this channel flagged ?
                  DO 165 I = 1, NDTIF
                     IF ((IDTBFP(I,JROW).NE.JPOL) .OR.
     *                  (IDTBFI(I,JROW).NE.JIF)) GO TO 165
                     M = 2 * (I - 1) + 1
                     IF ((IDTBFC(M,JROW).LE.JCHAN) .AND.
     *                  (IDTBFC(M+1,JROW).GE.JCHAN)) THEN
C                                       Blank
                        BNDPAS(2 * K - 1) = FBLANK
                        BNDPAS(2 * K)     = FBLANK
                        END IF
165                  CONTINUE
175               CONTINUE
200            CONTINUE
250         CONTINUE
C                                       If AC bandpass requested then
C                                       set phase/imag. to zero; square
C                                       if standard bandpass.
         IF (IDTBPX(JROW).EQ.1) THEN
            NFILL = NDTIF * NDTCHN * NPOL
            CALL CHBLNK (8, 1, LDTBPT, NN)
C                                       Standard BP
            IF (NN.EQ.0) THEN
               DO 275 J = 1, NFILL
                  IF ((BNDPAS(2 * J - 1).EQ.FBLANK) .OR.
     *               (BNDPAS(2 * J).EQ.FBLANK)) GO TO 275
C                                       Zero phase
                  DAMPL = SQRT (BNDPAS(2 * J - 1) ** 2
     *               + BNDPAS(2 * J) ** 2)
                  BNDPAS(2 * J) = 0.0
C                                       Square amplitude
                  BNDPAS(2 * J - 1) = DAMPL * DAMPL
275               CONTINUE
C                                       Polynomial BP
            ELSE
               DO 280 J = 1, NFILL
                  IF ((BNDPAS(2 * J - 1).NE.FBLANK) .AND.
     *               (BNDPAS(2 * J).NE.FBLANK)) THEN
                     BNDPAS(2 * J) = 0.0
                  END IF
  280             CONTINUE
               END IF
            END IF
C                                       Find antenna number
         ISUBA = 0
         IANT = 0
         JSUBA = 0
         DO 300 J = 1, NDTSUB
            IF (IDTSUB(J).EQ.IDTBSB(JROW)) JSUBA = J
300         CONTINUE
         IF (JSUBA.GT.0) THEN
            ISUBA = IDTSUB(JSUBA)
            JANT = 0
            DO 320 J = 1, NDTANT(JSUBA)
               IF (LDTANT(J,JSUBA).EQ.LDTBAN(JROW)) JANT = J
320            CONTINUE
            IF (JANT.GT.0) IANT = IDTANO(JANT,JSUBA)
            END IF
C                                       Write BP record
         DTIME = TIMEC
         ISOU = 0
         BANDW = 0.0
         CALL DFILL (NDTIF, 0.0D0, DSHFT)
         CALL FILL (2, 0, IREFA)
         CALL RFILL (NDTIF * NPOL, 1.0, WEIGHT)
         CALL TABBP ('WRIT', JBUFF, IBPRNO, IBPKOL, IBPNMV, NDTIF,
     *      NDTCHN, NPOL, DTIME, RDTBPS(JROW), ISOU, ISUBA, IANT,
     *      BANDW, DSHFT, IDTBFQ(JROW), IREFA, WEIGHT, BNDPAS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1330) IRET
            GO TO 990
            END IF
C                                       Until (time > end) or (end = 0)
C                                        or (interv = 0);
         TIME = TIME + RDTBPI(JROW)
         IF ((TIME.LE.RDTBPE(JROW)) .AND. (RDTBPE(JROW).NE.0.0)
     *      .AND. (RDTBPI(JROW).NE.0.0)) GO TO 100
C
500      CONTINUE
C                                       Close BP table
      CALL TABIO ('CLOS', 0, IBPRNO, JBUFF, JBUFF, IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1050  FORMAT ('DTBPTB: ERROR',I4,' CREATING BP TABLE')
1330  FORMAT ('DTBPTB: ERROR',I4,' WRITING BP TABLE')
      END
C----------------------------------------------------------------------
C     Function RAND came from NETLIB
C     source: http://www.netlib.org/fn.rand.f
C     GAMS taxonomic reference: L6a21
C     stated purpose: portable uniform random number generator
C     Modifications:  entire routine was capitalized
C                     changed RAND to ADRAND everywhere
C                     included two lines of variable declarations
C---------------------------------------------------------------------
      FUNCTION ADRAND (R)
C
C      THIS PSEUDO-RANDOM NUMBER GENERATOR IS PORTABLE AMOUNG A WIDE
C VARIETY OF COMPUTERS.  RAND(R) UNDOUBTEDLY IS NOT AS GOOD AS MANY
C READILY AVAILABLE INSTALLATION DEPENDENT VERSIONS, AND SO THIS
C ROUTINE IS NOT RECOMMENDED FOR WIDESPREAD USAGE.  ITS REDEEMING
C FEATURE IS THAT THE EXACT SAME RANDOM NUMBERS (TO WITHIN FINAL ROUND-
C OFF ERROR) CAN BE GENERATED FROM MACHINE TO MACHINE.  THUS, PROGRAMS
C THAT MAKE USE OF RANDOM NUMBERS CAN BE EASILY TRANSPORTED TO AND
C CHECKED IN A NEW ENVIRONMENT.
C      THE RANDOM NUMBERS ARE GENERATED BY THE LINEAR CONGRUENTIAL
C METHOD DESCRIBED, E.G., BY KNUTH IN SEMINUMERICAL METHODS (P.9),
C ADDISON-WESLEY, 1969.  GIVEN THE I-TH NUMBER OF A PSEUDO-RANDOM
C SEQUENCE, THE I+1 -ST NUMBER IS GENERATED FROM
C             X(I+1) = (A*X(I) + C) MOD M,
C WHERE HERE M = 2**22 = 4194304, C = 1731 AND SEVERAL SUITABLE VALUES
C OF THE MULTIPLIER A ARE DISCUSSED BELOW.  BOTH THE MULTIPLIER A AND
C RANDOM NUMBER X ARE REPRESENTED IN DOUBLE PRECISION AS TWO 11-BIT
C WORDS.  THE CONSTANTS ARE CHOSEN SO THAT THE PERIOD IS THE MAXIMUM
C POSSIBLE, 4194304.
C      IN ORDER THAT THE SAME NUMBERS BE GENERATED FROM MACHINE TO
C MACHINE, IT IS NECESSARY THAT 23-BIT INTEGERS BE REDUCIBLE MODULO
C 2**11 EXACTLY, THAT 23-BIT INTEGERS BE ADDED EXACTLY, AND THAT 11-BIT
C INTEGERS BE MULTIPLIED EXACTLY.  FURTHERMORE, IF THE RESTART OPTION
C IS USED (WHERE R IS BETWEEN 0 AND 1), THEN THE PRODUCT R*2**22 =
C R*4194304 MUST BE CORRECT TO THE NEAREST INTEGER.
C      THE FIRST FOUR RANDOM NUMBERS SHOULD BE .0004127026,
C .6750836372, .1614754200, AND .9086198807.  THE TENTH RANDOM NUMBER
C IS .5527787209, AND THE HUNDREDTH IS .3600893021 .  THE THOUSANDTH
C NUMBER SHOULD BE .2176990509 .
C      IN ORDER TO GENERATE SEVERAL EFFECTIVELY INDEPENDENT SEQUENCES
C WITH THE SAME GENERATOR, IT IS NECESSARY TO KNOW THE RANDOM NUMBER
C FOR SEVERAL WIDELY SPACED CALLS.  THE I-TH RANDOM NUMBER TIMES 2**22,
C WHERE I=K*P/8 AND P IS THE PERIOD OF THE SEQUENCE (P = 2**22), IS
C STILL OF THE FORM L*P/8.  IN PARTICULAR WE FIND THE I-TH RANDOM
C NUMBER MULTIPLIED BY 2**22 IS GIVEN BY
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0
C THUS THE 4*P/8 = 2097152 RANDOM NUMBER IS 2097152/2**22.
C      SEVERAL MULTIPLIERS HAVE BEEN SUBJECTED TO THE SPECTRAL TEST
C (SEE KNUTH, P. 82).  FOUR SUITABLE MULTIPLIERS ROUGHLY IN ORDER OF
C GOODNESS ACCORDING TO THE SPECTRAL TEST ARE
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1
C
C      IN THE TABLE BELOW LOG10(NU(I)) GIVES ROUGHLY THE NUMBER OF
C RANDOM DECIMAL DIGITS IN THE RANDOM NUMBERS CONSIDERED I AT A TIME.
C C IS THE PRIMARY MEASURE OF GOODNESS.  IN BOTH CASES BIGGER IS BETTER.
C
C                   LOG10 NU(I)              C(I)
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
C
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
C   BEST
C    POSSIBLE   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
C
C             INPUT ARGUMENT --
C R      IF R=0., THE NEXT RANDOM NUMBER OF THE SEQUENCE IS GENERATED.
C        IF R.LT.0., THE LAST GENERATED NUMBER WILL BE RETURNED FOR
C          POSSIBLE USE IN A RESTART PROCEDURE.
C        IF R.GT.0., THE SEQUENCE OF RANDOM NUMBERS WILL START WITH THE
C          SEED R MOD 1.  THIS SEED IS ALSO RETURNED AS THE VALUE OF
C          RAND PROVIDED THE ARITHMETIC IS DONE EXACTLY.
C
C             OUTPUT VALUE --
C RAND   A PSEUDO-RANDOM NUMBER BETWEEN 0. AND 1.
C
C IA1 AND IA0 ARE THE HI AND LO PARTS OF A.  IA1MA0 = IA1 - IA0.
      REAL R, TMP, ADRAND
      INTEGER IA1, IA0, IA1MA0, IC, IX0, IX1, IY0, IY1
      SAVE IX0, IX1

      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/
      DATA IC /1731/
      DATA IX1, IX0 /0, 0/
C-----------------------------------------------------------------------
      IF (R.LT.0.) GO TO 10
      IF (R.GT.0.) GO TO 20
C
C           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)
C                   + IA0*IX0) + IA0*IX0
C
      IY0 = IA0*IX0
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0
      IY0 = IY0 + IC
      IX0 = MOD (IY0, 2048)
      IY1 = IY1 + (IY0-IX0)/2048
      IX1 = MOD (IY1, 2048)
C
 10   TMP = IX1*2048 + IX0
      ADRAND = TMP / 4194304.
      RETURN
C
 20   IX1 = AMOD(R,1.)*4194304. + 0.5
      IX0 = MOD (IX1, 2048)
      IX1 = (IX1-IX0)/2048
      GO TO 10
C
      END
