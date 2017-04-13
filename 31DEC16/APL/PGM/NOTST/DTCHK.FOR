LOCAL INCLUDE 'DTCHK.INC'
C                                       Local include for DTCHK
      INCLUDE 'INCS:ZPBUFSZ.INC'
      LOGICAL WANTSL
      HOLLERITH XNAMEI(3), XCLAIN(2), XINFIL(12), XINEXT, XOPCOD,
     *   XOPTYP, XCOMNT(16), XOUTFL(12)
      REAL XANT(50), XTIMER(8), APARM(10), XSIN, XDISIN, XINVER,
     *   XPRTLV, TSELCT(2), DPARMS(70)
      INTEGER SEQIN, DISKIN, IANTSL(50), NANTSL, INVER, IPRTLV, JNCIF,
     *   JNCF, JNCS
      CHARACTER NAMEIN*12, CLAIN*6, LINFIL*48, LEXT*4, LOPCOD*4,
     *   LOPTYP*4, LCOMNT*64, LOUTFL*48
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XINFIL, XANT,
     *   XTIMER, XINEXT, XINVER, XOPCOD, XOPTYP, APARM, XCOMNT,
     *   XOUTFL, XPRTLV, DPARMS, TSELCT, SEQIN, DISKIN, INVER, IANTSL,
     *   NANTSL, IPRTLV, WANTSL, JNCIF, JNCF, JNCS
      COMMON /CHRCOM/ NAMEIN, CLAIN, LINFIL, LEXT, LOPCOD, LOPTYP,
     *   LCOMNT, LOUTFL
C                                       General
      LOGICAL ISCOMP, WOUTFL
      INTEGER ICNO, ILOCWT, ILUNF, IFINDF
      COMMON /GENPAR/ ICNO, ILOCWT, ILUNF, IFINDF, ISCOMP, WOUTFL
C                                       Test statistics:
      INTEGER MXSTAT
      PARAMETER (MXSTAT = 10000)
C                                       (Amp, phase, delay, rate)
      LOGICAL WTEST(5)
      DOUBLE PRECISION DERRMS(5), DERMAX(5), DERSUM(5), DERSQ(5),
     *   DERAVG(5), DERPVR(5), DSBUFF(MXSTAT,5)
      INTEGER NERCNT, NXSTAT
      COMMON /DTSTAT/ DERRMS, DERMAX, DERSUM, DERSQ, DERAVG,
     *   DERPVR, DSBUFF, NERCNT, NXSTAT, WTEST
C                                       Buffers
      INTEGER JBUFSZ
      PARAMETER (JBUFSZ = UVBFSS)
      REAL BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER JBUFF(UVBFSS)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFF
C                                       End local include for DTCHK
LOCAL END
      PROGRAM DTCHK
C-----------------------------------------------------------------------
C! Checks results obtained using simulated test data.
C# UV VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2000, 2007, 2012, 2015
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
C   DTCHK checks results obtained using simulated test data.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      INFILE         LINFIL        Simulation parameter file.
C      ANTENNAS       IANTSL        Antennas to select
C      TIMERANG       TSELCT        Time range to select
C      INEXT          LEXT          Cal. table type (CL or SN)
C      INVERS         INVERS        Cal. table version.
C      OPCODE         LOPCOD        Test type.
C      OPTYPE         LOPTYP        Test sub-type.
C      PRTLEV         IPRTLV        Print level.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'DTCHK '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       perform initialization.
      CALL DTCKIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read in simulation parameters
C                                       used by DTSIM if required.
      IF ((LOPCOD.EQ.'TABC').OR.(LOPCOD.EQ.'BSTB')) THEN
         CALL DTPARM (LINFIL, ILUNF, IPRTLV, IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
C                                       Compile test statistics
      CALL DTMAIN (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Print test results
      CALL DTPRNT (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DTCKIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   DTCKIN gets input parameters for DTCHK and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, PTYPE*2
      INTEGER   IROUND, NPARM, NN, J, IERR
      LOGICAL   T, F
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DTPM.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       LUN for text files
      ILUNF = 10
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 190
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
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (48, 1, XINFIL, LINFIL)
      CALL H2CHR (4, 1, XINEXT, LEXT)
      CALL H2CHR (4, 1, XOPCOD, LOPCOD)
      CALL H2CHR (4, 1, XOPTYP, LOPTYP)
      CALL H2CHR (64, 1, XCOMNT, LCOMNT)
      CALL H2CHR (48, 1, XOUTFL, LOUTFL)
C                                       Antennas
      WANTSL = T
      NANTSL = 0
      DO 100 J = 1, 50
         IF (XANT(J).NE.0) THEN
            NANTSL = NANTSL + 1
            IANTSL(NANTSL) = IROUND (ABS (XANT(J)))
            IF (XANT(J).LT.0) WANTSL = F
            END IF
100      CONTINUE
C                                       Time range
      TSELCT(1) = XTIMER(1) + XTIMER(2) / 24.0D0 +
     *   XTIMER(3) / 1440.0D0 + XTIMER(4) / 86400.0D0
      TSELCT(2) = XTIMER(5) + XTIMER(6) / 24.0D0 +
     *   XTIMER(7) / 1440.0D0 + XTIMER(8) / 86400.0D0
      IF (TSELCT(2).LE.TSELCT(1)) THEN
         TSELCT(1) = -999.0
         TSELCT(2) = 999.0
         END IF
C                                       Outfile specified ?
      CALL CHBLNK (48, 1, LOUTFL, NN)
      WOUTFL = (NN.GT.0)
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      INVER = IROUND (XINVER)
      IPRTLV = IROUND (XPRTLV)
C                                       Get CATBLK from input file.
      ICNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, ICNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, ICNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
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
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      JNCS = INCS / CATBLK(KINAX)
      JNCF = INCF / CATBLK(KINAX)
      JNCIF = INCIF / CATBLK(KINAX)
C                                       Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, ICNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = ICNO
      FRW(NCFILE) = 0
      JERR = 0
C                                       defaults to DTPM.INC
      DYEAR  = DPARMS(1) + 0.1
      DMONTH = DPARMS(2) + 0.1
      DDAY   = DPARMS(3) + 0.1
      CALL RFILL (MAXIF, DPARMS(40), DEFGAN)
      CALL RCOPY (30, DPARMS(11), DEFGAN)
      CALL RFILL (MAXIF, DPARMS(70), DEFSYS)
      CALL RCOPY (30, DPARMS(41), DEFSYS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DTCKIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
      END
      SUBROUTINE DTMAIN (IRET)
C----------------------------------------------------------------------
C   Main routine for checking test results.
C   Output:
C      IRET      I       Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      LOGICAL WT, WF
      INTEGER I, NFILL
      INCLUDE 'DTCHK.INC'
      DATA WT /.TRUE./, WF /.FALSE./
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      DO 20 I = 1, 5
         WTEST(I) = WF
20       CONTINUE
C                                       Initialize test statistics
      CALL DFILL (5, 0.0D0, DERSUM)
      CALL DFILL (5, 0.0D0, DERSQ)
      CALL DFILL (5, 0.0D0, DERRMS)
      CALL DFILL (5, 0.0D0, DERAVG)
      CALL DFILL (5, 0.0D0, DERPVR)
      NFILL = 5 * MXSTAT
      CALL DFILL (NFILL, 0.0D0, DSBUFF)
      NXSTAT = 0
      NERCNT = 0
C                                       Case sub_type of:
C                                       a) 'AMP' - check amplitude.
      IF (LOPTYP.EQ.'AMP') WTEST(1) = WT
C                                       b) 'PHAS' - check phase.
      IF (LOPTYP.EQ.'PHAS') WTEST(2) = WT
C                                       c) 'DELA' - check delay.
      IF (LOPTYP.EQ.'DELA') WTEST(3) = WT
C                                       d) 'RATE' - check rate.
      IF (LOPTYP.EQ.'RATE') WTEST(4) = WT
C                                       e) 'ACCL' - check acceleration
      IF (LOPTYP.EQ.'ACCL') WTEST(5) = WT
C                                       Endcase.
C
C                                       Case test type of:
C                                       1: 'TABC' - generic CL/SN check
      IF (LOPCOD.NE.'TABC') GO TO 150
         CALL CALCHK (IRET)
         GO TO 890
C                                       2: 'UV' - check uvdata directly
C                                          for unit ampl., zero phase
150   IF (LOPCOD.NE.'UVUN') GO TO 250
         CALL UVCHK (IRET)
         GO TO 890
C                                       3: 'BSTB' - check calibration
C                                          information in BS table
250   IF (LOPCOD.NE.'BSTB') GO TO 350
         CALL BSCHK (IRET)
         GO TO 890
C                                       Other: ignore
350   CONTINUE
C                                       Endcase
890   CONTINUE
      GO TO 999
C                                       Exit
999   RETURN
      END
      SUBROUTINE CALCHK (IRET)
C----------------------------------------------------------------------
C   Compile test statistics for calibration (CL/SN) tables.
C   Output:
C      IRET      I       Return code (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      LOGICAL WAPPL, WGOOD, WSELCT
      CHARACTER LDUMMY*16, BNDCOD(MAXIF)*8
      DOUBLE PRECISION DTIME, DTIMEI, DGEO(1024), DELAY(2), DPHASE(2),
     *   DRATE(2), DFEED(2,2), DGAIN(2), DTSYS(2), DAMPL(2), DIFF(4),
     *   DELAYR(2), DPHASR(2), DRATR(2), DFEEDR(2,2), DGAINR(2),
     *   DTSYSR(2), DAMPLR(2), DGERR(2), DGERRF(2), DACCEL(2),
     *   DACCLR(2), DAMP, DPHS, DFOFF(MAXIF), DMHZ, DTMHZ
      REAL GMMOD, RANOD(25), DECNOD(25), FARAD, OFFDOP(MAXIF), ATMOS,
     *   ATMOSD, XMBDEL, CLOCK(2), CLOCKD(2), XDISP(2), XDISPD(2),
     *   CREAL(2,MAXIF), CIMAG(2,MAXIF), CDELA(2,MAXIF),
     *   CRATE(2,MAXIF), CWGT(2,MAXIF), FINC(MAXIF), RTIME
      INTEGER ILUNTB, IRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT,
     *   NUMPOL, NUMIF, NTERM, IERR, SNKOLS(MAXSNC), SNNUMV(MAXSNC),
     *   NUMNOD, NREC, JREC, ISOU, NODENO, ISUBA, IREFA(2,MAXIF),
     *   IANT, IFQID, ISUBAN, IFQ, IAN, IANR, INDX, INDXR, JIF,
     *   JSTOK, KSTOK, J, ISUB, JSOU, JAN, JANR, JSUB, JFQ, ILUNFQ,
     *   IFQCUR, IFQVER, NIF, IFQD, ISBAND(MAXIF)
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LDUMMY /' '/
      DATA ILUNTB /28/, ILUNFQ /29/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      ISUBAN = 0
      IFQCUR = 0
C                                       Open CL or SN table
      IF (LEXT.EQ.'CL') THEN
         CALL CALINI ('READ', JBUFF, DISKIN, ICNO, INVER, CATBLK,
     *      ILUNTB, IRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IERR)
      ELSE
         CALL SNINI ('READ', JBUFF, DISKIN, ICNO, INVER, CATBLK,
     *      ILUNTB, IRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF,
     *      NUMNOD, GMMOD, RANOD, DECNOD, WAPPL, IERR)
         END IF
C
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR, LEXT, INVER
         GO TO 990
         END IF
C                                       Loop over each record
C                                       in table.
      NREC = JBUFF(5)
      DO 500 JREC = 1, NREC
C                                       Read CL or SN record
         IRNO = JREC
         IF (LEXT.EQ.'CL') THEN
            CALL TABCAL ('READ', JBUFF, IRNO, CLKOLS, CLNUMV, NUMPOL,
     *         NUMIF, DTIME, DTIMEI, ISOU, IANT, ISUBA, IFQID, FARAD,
     *         DGEO, OFFDOP, ATMOS, ATMOSD, XMBDEL, CLOCK, CLOCKD,
     *         XDISP, XDISPD, CREAL, CIMAG, CDELA, CRATE, CWGT, IREFA,
     *         IERR)
         ELSE
            CALL TABSN ('READ', JBUFF, IRNO, SNKOLS, SNNUMV, NUMPOL,
     *         DTIME, DTIMEI, ISOU, IANT, ISUBA, IFQID, FARAD, NODENO,
     *         XMBDEL, XDISP, XDISPD, CREAL, CIMAG, CDELA, CRATE,
     *         CWGT, IREFA, IERR)
            END IF
C
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1100) IERR, LEXT, INVER
         GO TO 990
         END IF
C                                       Antenna selection
      CALL AMATCH (IANT, IANTSL, NANTSL, WANTSL, WSELCT)
      IF (.NOT.WSELCT) GO TO 500
C                                       Time range selection
      IF ((DTIME.LT.TSELCT(1)).OR.(DTIME.GT.TSELCT(2))) GO TO 500
C                                       Get antenna information for
C                                       this subarray if not already
C                                       loaded.
      ISUB = MAX (ISUBA, 1)
      IF (ISUBAN.NE.ISUB) THEN
         ISUBAN = ISUB
         CALL GETANT (DISKIN, ICNO, ISUBAN, CATBLK, BUFF2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1200) IERR, ISUBAN
            GO TO 990
            END IF
         END IF
C                                       Get FQ information for this
C                                       FQ_ID if not already loaded
      IFQD = MAX (IFQID, 1)
      IF (IFQD.NE.IFQCUR) THEN
         IFQVER = 1
         CALL CHNDAT ('READ', BUFF1, DISKIN, ICNO, IFQVER, CATBLK,
     *      ILUNFQ, NIF, DFOFF, ISBAND, FINC, BNDCOD, IFQD, IERR)
         IF (IERR.NE.0) THEN
            IRET = 4
            WRITE (MSGTXT,1250) IERR, IFQ
            GO TO 990
            END IF
         IFQCUR = IFQD
         END IF
C                                       Loop over IF
      DO 350 JIF = 1, NUMIF
C                                       Match anten. name and
C                                       FQ_ID in cal. error arrays.
         IAN = MAX (IANT, 1)
         IFQ = MAX (IFQID, 1)
         CALL DTINDX (STNNAM(IAN), IFQ, INDX)
C                                       Look up indices in simulated
C                                       data common blocks
         CALL DTMATC (LDUMMY, IAN, ISUB, IFQ, JSOU, JAN, JSUB, JFQ)
C                                       Cal. errors.
         RTIME = DTIME
         CALL DTCALS (JSOU, JAN, JSUB, JFQ, JIF, INDX, RTIME, DELAY,
     *      DPHASE, DRATE, DACCEL, DFEED, DGAIN, DTSYS, DAMPL, DGERR)
C                                       Loop over polzn.
         DO 300 JSTOK = 1, NUMPOL
            WGOOD = .FALSE.
C                                       Reference antenna.
            IANR = MAX (IREFA(JSTOK,JIF), 1)
            CALL DTINDX (STNNAM(IANR), IFQ, INDXR)
C                                       Look up indices in simulated
C                                       data common blocks
            CALL DTMATC (LDUMMY, IANR, ISUB, IFQ, JSOU, JANR, JSUB,
     *         JFQ)
            CALL DTCALS (JSOU, JANR, JSUB, JFQ, JIF, INDXR, RTIME,
     *         DELAYR, DPHASR, DRATR, DACCLR, DFEEDR, DGAINR, DTSYSR,
     *         DAMPLR, DGERRF)
C                                       Stokes index
            KSTOK = JSTOK
            IF ((NUMPOL.EQ.1).AND.(ICOR0.LT.-1)) KSTOK = 2
C                                       Difference in (amp, phase,
C                                       delay and rate).
            IF ((WTEST(1).OR.WTEST(2)).AND.
     *         ((CREAL(JSTOK,JIF).NE.FBLANK).AND.
     *         (CIMAG(JSTOK,JIF).NE.FBLANK))) THEN
               DAMP = SQRT (CREAL(JSTOK,JIF)**2 + CIMAG(JSTOK,JIF)**2)
               IF (DAMP.NE.0.0D0) DAMP = 1.0 / (DAMP * GMMOD)
               DPHS = ATAN2 (CIMAG(JSTOK,JIF), CREAL(JSTOK,JIF))
C                                       Amplitude
               DIFF(1) = DGERR(KSTOK) - DAMP
C                                       Phase
               DIFF(2) = (DPHASE(KSTOK) - DPHASR(KSTOK)) - DPHS
               DIFF(2) = DIFF(2) * RAD2DG
               DIFF(2) = MOD (DIFF(2), 360.0D0)
               IF (DIFF(2).GT.180.0) DIFF(2) = DIFF(2) - 360.0D0
               IF (DIFF(2).LT.-180.0) DIFF(2) = DIFF(2) + 360.0D0
               WGOOD = .TRUE.
               END IF
C                                       Delay
            IF (WTEST(3).AND.(CDELA(JSTOK,JIF).NE.FBLANK)) THEN
               DIFF(3) = (DELAY(KSTOK) - DELAYR(KSTOK)) -
     *            CDELA(JSTOK,JIF)
C                                       Convert to ns.
               DIFF(3) = DIFF(3) * 1.0D9
               WGOOD = .TRUE.
               END IF
C                                       Rate
            IF (WTEST(4).AND.(CRATE(JSTOK,JIF).NE.FBLANK)) THEN
C                                       Convert to mHz
               DTMHZ = (DRATE(KSTOK) - DRATR(KSTOK)) / TWOPI * 1.0D3
               DMHZ = CRATE(JSTOK,JIF) * (FREQ + DFOFF(JIF)) * 1.0D3
               DIFF(4) = DTMHZ - DMHZ
               WGOOD = .TRUE.
               END IF
C                                       Accumulate statistics
            IF (WGOOD) THEN
               DERAVG(1) = DERAVG(1) + DAMP
               DERAVG(2) = DERAVG(2) + DPHS * RAD2DG
               DERAVG(3) = DERAVG(3) + CDELA(JSTOK,JIF)
               DERAVG(4) = DERAVG(4) + CRATE(JSTOK,JIF)
C
               NXSTAT = MIN (NXSTAT+1, MXSTAT)
               DO 280 J = 1, 4
                  DERSUM(J) = DERSUM(J) + DIFF(J)
                  DERSQ(J) = DERSQ(J) + DIFF(J) ** 2
                  DSBUFF(NXSTAT,J) = DIFF(J)
280               CONTINUE
               NERCNT = NERCNT + 1
               END IF
C
300         CONTINUE
350      CONTINUE
500   CONTINUE
C                                       Calculate final statistics
      CALL ERSTAT (IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1020  FORMAT ('CALCHK: ERROR',I4,' OPENING',A3,' TABLE',I3)
1100  FORMAT ('CALCHK: ERROR',I4,' READING',A3,' TABLE',I3)
1200  FORMAT ('CALCHK: ERROR',I4,' READING AN TABLE',I3)
1250  FORMAT ('CALCHK: ERROR',I4,' READING FQ TABLE; FQID=',I3)
      END
      SUBROUTINE BSCHK (IRET)
C----------------------------------------------------------------------
C   Compile test statistics for calibration information in BS tables
C   Output:
C      IRET    I  Return code (0 => ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSTD.INC'
      LOGICAL WGOOD, WSEL1, WSEL2
      CHARACTER LMODE*4, LDUMMY*16, BNDCOD(MAXIF)*8
      DOUBLE PRECISION DTIME, DELAY(2), DPHASE(2), DRATE(2), DACCL(2),
     *   DFEED(2,2), DGAIN(2), DTSYS(2), DAMPL(2), DGERR(2),
     *   DELAY2(2), DPHAS2(2), DRATE2(2), DACCL2(2), DFEED2(2,2),
     *   DGAIN2(2), DTSYS2(2), DAMPL2(2), DGERR2(2), DIFF(5),
     *   DFOFF(MAXIF), DMHZ, DTMHZ
      REAL TIMINT, VAMP(MAXIF), SAMP(MAXIF), RMBD, RMBERR, AMBMBD,
     *   RSBD(MAXIF), SBDERR(MAXIF), SBDAMB, RRATE(MAXIF),
     *   RTERR(MAXIF), RTAMB, RACCEL(MAXIF), ACCERR(MAXIF),
     *   RPHASE(MAXIF), PHSERR(MAXIF), FINC(MAXIF), RTIME
      INTEGER ISUBAN, ILUNTB, IRNO, BSCOLS(MAXBSC), BSNUMV(MAXBSC),
     *   NUMIF, IERR, NREC, JREC, IBASL(2), ISUBA, ISTK, ISOU, ISUB,
     *   JIF, IAN, IFQ, INDX, JSOU, JAN, JSUB, JFQ, J, K1, K2, IFQCUR,
     *   ILUNFQ, IFQVER, NIF, IFQD, ISBAND(MAXIF)
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA ILUNTB /28/, ILUNFQ /29/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      ISUBAN = 0
      IFQCUR = 0
C                                       Open BS table
      LMODE = '    '
      CALL BSINI ('READ', JBUFF, DISKIN, ICNO, INVER, CATBLK, ILUNTB,
     *   IRNO, BSCOLS, BSNUMV, LMODE, NUMIF, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1020) IERR, INVER
         GO TO 990
         END IF
C                                       Loop over each record in table
      NREC = JBUFF(5)
      DO 500 JREC = 1, NREC
C                                       Read BS record
         IRNO = JREC
         CALL TABBS ('READ', JBUFF, IRNO, BSCOLS, BSNUMV, NUMIF, DTIME,
     *      TIMINT, IBASL, ISUBA, ISTK, ISOU, VAMP, SAMP, RMBD, RMBERR,
     *      AMBMBD, RSBD, SBDERR, SBDAMB, RRATE, RTERR, RTAMB, RACCEL,
     *      ACCERR, RPHASE, PHSERR, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1100) IERR, INVER
            GO TO 990
            END IF
C                                       Antenna selection
         CALL AMATCH (IBASL(1), IANTSL, NANTSL, WANTSL, WSEL1)
         CALL AMATCH (IBASL(2), IANTSL, NANTSL, WANTSL, WSEL2)
         IF (.NOT.(WSEL1.OR.WSEL2)) GO TO 500
C                                       Time range selection
         IF ((DTIME.LT.TSELCT(1)).OR.(DTIME.GT.TSELCT(2))) GO TO 500
C                                       Get antenna information for
C                                       this subarray if not already
C                                       loaded
         ISUB = MAX (ISUBA, 1)
         IF (ISUBAN.NE.ISUB) THEN
            ISUBAN = ISUB
            CALL GETANT (DISKIN, ICNO, ISUBAN, CATBLK, BUFF2, IERR)
            IF (IERR.NE.0) THEN
               IRET = 3
               WRITE (MSGTXT,1200) IERR, ISUBAN
               GO TO 990
               END IF
            END IF
C                                       Get FQ information if not
C                                       already loaded (Note: BS
C                                       table has no fq_id col.)
         IFQD = 1
         IF (IFQD.NE.IFQCUR) THEN
            IFQVER = 1
            CALL CHNDAT ('READ', BUFF1, DISKIN, ICNO, IFQVER, CATBLK,
     *         ILUNFQ, NIF, DFOFF, ISBAND, FINC, BNDCOD, IFQD, IERR)
            IF (IERR.NE.0) THEN
               IRET = 4
               WRITE (MSGTXT,1250) IERR, IFQ
               GO TO 990
               END IF
            IFQCUR = IFQD
            END IF
C                                       Loop over IF
         DO 350 JIF = 1, NUMIF
            WGOOD = .FALSE.
C                                       Match antenna name and FQ_ID
C                                       in the calibration error arrays
C                                       Antenna_1:
            IAN = MAX (IBASL(1), 1)
            IFQ = 1
            CALL DTINDX (STNNAM(IAN), IFQ, INDX)
C                                       Look up indices in simulated
C                                       data common blocks
            CALL DTMATC (LDUMMY, IAN, ISUB, IFQ, JSOU, JAN, JSUB, JFQ)
C                                       Compute expected cal. errors
            RTIME = DTIME
            CALL DTCALS (JSOU, JAN, JSUB, JFQ, JIF, INDX, RTIME, DELAY,
     *         DPHASE, DRATE, DACCL, DFEED, DGAIN, DTSYS, DAMPL,
     *         DGERR)
C                                       Antenna_2:
            IAN = MAX (IBASL(2), 1)
            CALL DTINDX (STNNAM(IAN), IFQ, INDX)
C                                       Look up indices
            CALL DTMATC (LDUMMY, IAN, ISUBA, IFQ, JSOU, JAN, JSUB, JFQ)
C                                       Compute calibration errors
            CALL DTCALS (JSOU, JAN, JSUB, JFQ, JIF, INDX, RTIME,
     *         DELAY2, DPHAS2, DRATE2, DACCL2, DFEED2, DGAIN2, DTSYS2,
     *         DAMPL2, DGERR2)
C                                       Stokes index
C                                       RR:
            IF (ISTK.EQ.-1) THEN
               K1 = 1
               K2 = 1
               END IF
C                                       LL:
            IF (ISTK.EQ.-2) THEN
               K1 = 2
               K2 = 2
               END IF
C                                       RL:
            IF (ISTK.EQ.-3) THEN
               K1 = 1
               K2 = 2
               END IF
C                                       LR:
            IF (ISTK.EQ.-4) THEN
               K1 = 2
               K2 = 1
               END IF
C                                       Check for invalid Stokes
            IF ((ISTK.GT.0).OR.(ISTK.LT.-4)) THEN
               IRET = 4
               WRITE (MSGTXT,1350) ISTK
               GO TO 990
               END IF
C                                       Difference in (amp, phase
C                                       delay, rate and acceleration)
C                                       Amplitude:
            IF (WTEST(1).AND.(VAMP(JIF).NE.FBLANK)) THEN
               DIFF(1) = DGERR(K1) * DGERR2(K2) - VAMP(JIF)
               WGOOD = .TRUE.
               END IF
C                                       Phase
            IF (WTEST(2).AND.(RPHASE(JIF).NE.FBLANK)) THEN
               DIFF(2) = (DPHASE(K1) - DPHAS2(K2)) * RAD2DG -
     *            RPHASE(JIF)
               DIFF(2) = MOD (DIFF(2), 360.0D0)
               IF (DIFF(2).GT.180.0) DIFF(2) = DIFF(2) - 360.0D0
               IF (DIFF(2).LT.-180.0) DIFF(2) = DIFF(2) + 360.0D0
               WGOOD = .TRUE.
               END IF
C                                       Delay
            IF (WTEST(3).AND.(RSBD(JIF).NE.FBLANK)) THEN
               DIFF(3) = (DELAY(K1) - DELAY2(K2)) - RSBD(JIF)
C                                       Convert to ns
               DIFF(3) = DIFF(3) * 1.0D9
               WGOOD = .TRUE.
               END IF
C                                       Rate
            IF (WTEST(4).AND.(RRATE(JIF).NE.FBLANK)) THEN
C                                       Convert to mHz
               DTMHZ = (DRATE(K1) - DRATE2(K2)) / TWOPI * 1.0D3
               DMHZ = RRATE(JIF) * 1.0D3
               DIFF(4) = DTMHZ - DMHZ
               WGOOD = .TRUE.
               END IF
C                                       Acceleration
            IF (WTEST(5).AND.(RACCEL(JIF).NE.FBLANK)) THEN
C                                       Convert to (mHz)**2
               DTMHZ = (DACCL(K1) - DACCL2(K2)) / TWOPI * 1.0D6
               DMHZ = RACCEL(JIF) * 1.0D6
               DIFF(5) = DTMHZ - DMHZ
               WGOOD = .TRUE.
               END IF
C                                       Accumulate statistics
            IF (WGOOD) THEN
               DERAVG(1) = DERAVG(1) + VAMP(JIF)
               DERAVG(2) = DERAVG(2) + RPHASE(JIF)
               DERAVG(3) = DERAVG(3) + RSBD(JIF)
               DERAVG(4) = DERAVG(4) + RRATE(JIF)
               NXSTAT = MIN (NXSTAT+1, MXSTAT)
               DO 300 J = 1, 5
                  DERSUM(J) = DERSUM(J) + DIFF(J)
                  DERSQ(J) = DERSQ(J) + DIFF(J) ** 2
                  DSBUFF(NXSTAT,J) = DIFF(J)
300               CONTINUE
               NERCNT = NERCNT + 1
               END IF
350         CONTINUE
500      CONTINUE
C                                       Calculate final statistics
      CALL ERSTAT (IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1020  FORMAT ('BSCHK: ERROR',I4,' OPENING BS TABLE',I3)
1100  FORMAT ('BSCHK: ERROR',I4,' READING BS TABLE',I3)
1200  FORMAT ('BSCHK: ERROR',I4,' READING AN TANLE',I3)
1250  FORMAT ('BSCHK: ERROR',I4,' READING FQ TABLE; FQID=',I3)
1350  FORMAT ('BSCHK: UNSUPPORTED STOKES TYPE',I3)
      END
      SUBROUTINE DTPRNT (IRET)
C----------------------------------------------------------------------
C   Print test results
C   Output:
C      IRET    I  Return code (0 => ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      CHARACTER LTYPE(5)*6, LUNIT(5)*4, LTEST*4
      DOUBLE PRECISION DSNR
      INTEGER J, K, IERR, ITIME(8)
      INTEGER IROUND
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LTYPE /'AMPL', 'PHASE', 'DELAY', 'RATE', 'ACCL'/
      DATA LUNIT /'Jy', 'deg', 'ns', 'mHz', 'mHz2'/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Open output text file
      IF (WOUTFL) THEN
         CALL ZTXOPN ('WRIT', ILUNF, IFINDF, LOUTFL, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         END IF
C                                       Header
      WRITE (MSGTXT,1010)
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
C                                       Input comment line
      MSGTXT = LCOMNT
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
C                                       Antenna selection
      IF (WANTSL) THEN
         WRITE (MSGTXT,1020) (IANTSL(K), K=1,NANTSL)
      ELSE
         WRITE (MSGTXT,1025) (IANTSL(K), K=1,NANTSL)
         END IF
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
C                                       Time range selection
      DO 30 J = 1, 8
         ITIME(J) = IROUND (XTIMER(J))
30       CONTINUE
      WRITE (MSGTXT,1030) ITIME
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
C                                       Test type and sub_type
      WRITE (MSGTXT,1100) LOPCOD, LOPTYP, NERCNT
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
C                                       Results:
C                                       (Amp, phase, delay, rate)
      DO 500 J = 1, 5
         IF (WTEST(J)) THEN
C                                       Compute SNR
            IF (DERRMS(J).GT.0) THEN
               DSNR = ABS (DERAVG(J)) / DERRMS(J)
            ELSE
               DSNR = 999.9
               END IF
C                                       Check limits on mean
C                                       and rms deviation
            K = 2 * (J - 1) + 1
            IF ((ABS(DERSUM(J)).LT.APARM(K)).AND.
     *         (DERRMS(J).LT.APARM(K+1))) THEN
               LTEST = 'PASS'
            ELSE
               LTEST = 'FAIL'
               END IF
C                                       Write limits
            WRITE (MSGTXT,1180) LTYPE(J), APARM(K), APARM(K+1)
            CALL MSGWRT (5)
            IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT,
     *         IERR)
C                                       Write test results
            WRITE (MSGTXT,1200) DERSUM(J), DERRMS(J)
            CALL MSGWRT (5)
            IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT,
     *         IERR)
            WRITE (MSGTXT,1220) LUNIT(J), DERPVR(J), DSNR, LTEST
            CALL MSGWRT (5)
            IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT,
     *         IERR)
            END IF
500      CONTINUE
C                                       Trailer
      WRITE (MSGTXT,1010)
      CALL MSGWRT (5)
      IF (WOUTFL) CALL ZTXIO ('WRIT', ILUNF, IFINDF, MSGTXT, IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
C                                       Close output text file
999   IF (WOUTFL) CALL ZTXCLS (ILUNF, IFINDF, IERR)
      RETURN
C---------------------------------------------------------------------
1000  FORMAT ('DTPRNT: ERR',I4,' OPENING OUTPUT FILE')
1010  FORMAT (64('='))
1020  FORMAT ('Ant(T):',18I4)
1025  FORMAT ('Ant(F):',18I4)
1030  FORMAT ('Time range: ',2(I3,'/',I2.2,':',I2.2,':',I2.2,5X))
1100  FORMAT ('Test type: ',A4,'  Sub_type: ',A4,'  Npoints:',I8)
1180  FORMAT (A6,' Limits:  ',E13.6,5X,E13.6)
1200  FORMAT (6X,' Mean dev=',E13.6,' RMS=',E13.6)
1220  FORMAT ('(',A4,') Fp-sigma=',E13.6,' SNR=',F13.2,' **',A4)
      END
      SUBROUTINE UVCHK (IRET)
C----------------------------------------------------------------------
C   Reads uv-data one point at a time and compiles test statistics.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise error.
C----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER IFILE*48
      INTEGER   INIO, IPTRI, LUNI, INDI, ILENBU, IBIND, I, IA1, IA2,
     *   INCX, BO, VO, NUMVIS, XCOUNT, NCORI
      LOGICAL   T, F, WSEL1, WSEL2
      INCLUDE 'DTCHK.INC'
      REAL      BASEN, CBUFF(UVBFSS), TIME
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
C                                       Antenna selection
            CALL AMATCH (IA1, IANTSL, NANTSL, WANTSL, WSEL1)
            CALL AMATCH (IA2, IANTSL, NANTSL, WANTSL, WSEL2)
            IF (.NOT.(WSEL1.OR.WSEL2)) GO TO 190
C                                       Time range selection
            TIME = BUFF1(IPTRI+ILOCT)
            IF ((TIME.LT.TSELCT(1)).OR.(TIME.GT.TSELCT(2))) GO TO 190
C
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL UVSTAT (NUMVIS, CBUFF, INCX, IRET)
            ELSE
C                                       Un compressed data
            CALL UVSTAT (NUMVIS, BUFF1(IPTRI+NRPARM), INCX, IRET)
            END IF
C                                       Branch on his return
            IF (IRET) 140, 130, 120
C                                       Error (fatal)
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C
 130        CONTINUE
C
 140        IPTRI = IPTRI + LREC
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Final call to UVSTAT.
 200     NUMVIS = -1
         CALL UVSTAT (NUMVIS, BUFF1, INCX, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
            END IF
C                                       Close input file
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCHK: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('UVCHK: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVCHK: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVSTAT: UVSTAT ERROR',I3)
      END
      SUBROUTINE UVSTAT (NUMVIS, VIS, INCX, IRET)
C-----------------------------------------------------------------------
C   Compile test statistics for each uv-record.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      INCX    I    Increment in visibility array.
C      VIS     R(*) Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Output:
C      IRET       I    Return code  0 => ok; else error.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*)
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION DAMP, DPHAS, DIFF(2)
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXI, J
      INCLUDE 'DTCHK.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
C                                       Compare with (1,0)
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * JNCIF + (JF-1) * JNCF +
     *               (JS-1) * JNCS + 1
C                                       Skip blanked data
                  IF ((VIS(1,INDEXI).EQ.FBLANK) .OR.
     *               (VIS(2,INDEXI).EQ.FBLANK) .OR.
     *               (VIS(3,INDEXI).LE.0.0)) GO TO 20
C                                       Accumulate test statistics
                  DAMP = SQRT (VIS(1,INDEXI) ** 2 +
     *               VIS(2,INDEXI) ** 2)
C                                       Trap case of zero amplitude
                  IF (DAMP.GT.0.0D0) THEN
                     DPHAS = ATAN2 (VIS(2,INDEXI), VIS(1,INDEXI))
                     DPHAS = DPHAS * RAD2DG
                  ELSE
                     DPHAS = 0.0D0
                     END IF
C                                       Amp. and phase only
                  DIFF(1) = DAMP - 1.0D0
                  DIFF(2) = DPHAS
                  DERAVG(1) = DERAVG(1) + DAMP
                  DERAVG(2) = DERAVG(2) + DPHAS
                  NXSTAT = MIN (NXSTAT+1, MXSTAT)
                  DO 15 J = 1, 2
                     DERSUM(J) = DERSUM(J) + DIFF(J)
                     DERSQ(J) = DERSQ(J) + DIFF(J) ** 2
                     DSBUFF(NXSTAT,J) = DIFF(J)
 15                  CONTINUE
                  NERCNT = NERCNT + 1
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       Last call: compute statistics
      ELSE
         CALL ERSTAT (IRET)
         END IF
C
 999  RETURN
C----------------------------------------------------------------------
      END
      SUBROUTINE ERSTAT (IRET)
C----------------------------------------------------------------------
C   Compute error statistics
C   Output:
C      IRET     I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'DTCHK.INC'
      DOUBLE PRECISION DQA, DQB
      INTEGER J, N4A, N4B, MEDIAN, NADJ
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Loop over (amp,phas,delay,rate
C                                        and acceleration)
      DO 100 J = 1, 5
         IF (NERCNT.GT.0) THEN
            DERAVG(J) = DERAVG(J) / NERCNT
            DERSUM(J) = DERSUM(J) / NERCNT
            DERSQ(J) = DERSQ(J) / NERCNT
            DERRMS(J) = SQRT (DERSQ(J) - DERSUM(J) ** 2)
            END IF
C                                       Non-parametric variance
C                                       (F-pseudovariance)
C                                       First sort in descending order
         IF (NXSTAT.EQ.0) THEN
            DERPVR(J) = 0.0
         ELSE
            CALL DSORT (NXSTAT, DSBUFF(1,J))
            MEDIAN = (NXSTAT + 1) / 2
C                                       Depth of fourths
            N4B = (MEDIAN + 1) / 2
            N4A = NXSTAT - N4B + 1
            IF (MOD (MEDIAN,2).EQ.0) THEN
               NADJ = MIN (N4B+1, NXSTAT)
               DQB = (DSBUFF(N4B,J) + DSBUFF(NADJ,J)) / 2.0D0
               NADJ = MAX (N4A-1, 1)
               DQA = (DSBUFF(N4A,J) + DSBUFF(NADJ,J)) / 2.0D0
            ELSE
               DQB = DSBUFF(N4B,J)
               DQA = DSBUFF(N4A,J)
               END IF
C                                       F-pseudovariance
            DERPVR(J) = (DQB - DQA) / 1.349
            END IF
C
100      CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE AMATCH (IANT, IANTSL, NANTSL, WANTSL, WMATCH)
C----------------------------------------------------------------------
C   Antenna selection
C   Inputs:
C      IANT      I      Antenna number to match
C      IANTSL    I(*)   List of selected antennas
C      NANTSL    I      Element count in IANTSL
C      WANTSL    L      True if the listed antennas are selected;
C                       else anything but the listed antennas.
C   Output:
C      WMATCH    L      True if IANT is selected
C----------------------------------------------------------------------
      LOGICAL WANTSL, WMATCH
      INTEGER NANTSL
      INTEGER IANT, IANTSL(NANTSL)
C
      INTEGER J
C----------------------------------------------------------------------
C                                       Initialization
      WMATCH = (NANTSL.LE.0)
C                                       Loop over selected antennas
      DO 50 J = 1, NANTSL
         IF (IANTSL(J).EQ.IANT) WMATCH = .TRUE.
50       CONTINUE
C
      IF (.NOT.WANTSL) WMATCH = (.NOT.WMATCH)
C                                       Exit
999   RETURN
      END
