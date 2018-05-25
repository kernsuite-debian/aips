      SUBROUTINE SN2CL (SNDISK, CLDISK, SNCNO, CLCNO, SNVER, CLIVER,
     *   CLOVER, SNCAT, CLCAT, NSOUWD, SOUWAN, DOSWNT, NCALWD, CALWAN,
     *   DOCWNT, TIMRA, SUBA, FREQID, NANTSL, DOAWNT, ANTENS, INTMOD,
     *   ORIGIN, ALLPAS, ALLSUB, SNBUFF, CLIBUF, CLOBUF, DLIMI, IRET)
C-----------------------------------------------------------------------
C! Apply an SN to a CL table.
C# EXT-appl UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2003-2007, 2012, 2015-2016
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
C   Subroutine to apply a SN table to a CL table.  Both should be
C   sorted into Antenna-Time order before calling SN2CL.
C      If the output CL table does not already exist then one is created
C   and the contents of the SN table copied to it.  In this case the
C   total model parameters are left zero.
C     If the SN table contains redundant entries, only the first
C   encountered will be processed.
C   Input:
C      SNDISK     I       SN Disk
C      CLDISK     I       CL Disk
C      SNCNO      I       Catalog slot number of SN
C      CLCNO      I       Catalog slot number of CL
C      SNVER      I       SN file version
C      CLIVER     I       Input CL file version
C      CLOVER     I       Output CL file version
C      SNCAT      I(256)  Catalog header block of SN.
C      CLCAT      I(256)  Catalog header block of CL.
C      NSOUWD     I       Number of sources included or excluded.
C      DOSWNT     L       If .TRUE. then sources in SOUWAN are
C                         included, if .FALSE. then excluded.
C      SOUWAN     I(30)   The source numbers of sources included or
C                         excluded.
C      NCALWD     I       Number of calibrators included or excluded.
C      DOCWNT     L       If .TRUE. then calibrators in CALWAN are
C                         included, if .FALSE. then excluded.
C      CALWAN     I(30)   The source numbers of calibrators included or
C                         excluded.
C      TIMRA      D(2)    First and last times to be considered. (days)
C      SUBA       I       Desired subarray, 0=>all.
C      FREQID     I       Desired freq. id number.
C      NANTSL     I       Number of antennas selected, 0=> all
C      DOAWNT     L       If true antennas are selected, else
C                         deselected.
C      ANTENS     I(*)    Antenna list
C      INTMOD     I(*)    Phase interpolation mode
C                           0   '2PT' phasor interpolation
C                           1   'SELF' as 2PT but use only the same
C                                source to calibrate itself
C                           2   'SELN' use phase & rate of nearest SN
C                                entry for this source
C                           3   'SIMP' simple linear interpolation
C                                use quickest route around for
C                                phase connection
C                           4   'AMBG'  use mean rates to resolve phase
C                                ambiguity then do linear interpolation
C                           5   'CUBE' as above but fit third order
C                                polynomial to fit phases and rates
C                                at SN entries.
C      ORIGIN     I       Origin of SN table, 0 => multi-source or
C                         unknown, 1 => single source file.
C      ALLPAS     L       Pass-thru calibration enabled, or not.
C      ALLSUB     L       All subarrays included, output subarray = 0
C      DLIMI      D       Max. time over which solutions will be
C                         interpolated (days). 0=> no limit.
C   Output:
C      SNBUFF     I(*)    Buffer for TABIO use for SN table.
C      CLIBUF     I(*)    Buffer for TABIO use for CL table.
C      CLOBUF     I(*)    Buffer for TABIO use for CL table.
C      IRET       I       Return code 0=OK, else failed.
C   Note: uses LUNs 27, 28 and 29
C-----------------------------------------------------------------------
      INTEGER   SNDISK, CLDISK, SNCNO, CLCNO, SNVER, CLIVER, CLOVER,
     *   SNCAT(256), CLCAT(256), NSOUWD, SOUWAN(*), NCALWD, CALWAN(*),
     *   SUBA, FREQID, NANTSL, ANTENS(*), SNBUFF(*), CLIBUF(*),
     *   CLOBUF(*), IRET, INTMOD, ORIGIN
      LOGICAL   DOSWNT, DOCWNT, DOAWNT, ALLPAS, ALLSUB
      DOUBLE PRECISION TIMRA(2), DLIMI
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      LOGICAL   GOODCL, SLCTD, WANTED, ONEOK, TWOOK, FIRST
      CHARACTER COLHED(2)*24, BNDCOD(MAXIF)*8
      INTEGER   IRCODE, SNREC(2*XCLRSZ), CLREC(XCLRSZ), ITEMP, IPOINT,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   FRQOFF, SNLUN, CLILUN, CLOLUN, I, LIMIT, IERR, LSTCAL, LSTSOU,
     *   THSOU, I1OFFI, I1OFFR, I1OFFD, I2OFFI, I2OFFR, I2OFFD, VER,
     *   SUB, ISUB,  NUMANT, NUMPOL, NUMIF, NUMNOD, SNANT1, SNANT2,
     *   CLANT,  NUMSUB, STOFF, KOLS(2), COVER(2050), KCALWD, NSRC,
     *   SRCN(1000), SRCNOW, IFOFF,
     *   TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   MB1SN, DI1SN, RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   MB2SN, DI2SN, RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN,
     *   TIMCL, INTCL, SOUCL, ANTCL, SUBCL, FRQCL, IFRCL, GEODCL, DOPCL,
     *   MB1CL, DI1CL, RE1CL, IM1CL, DE1CL, RA1CL, WE1CL, RF1CL,
     *   MB2CL, DI2CL, RE2CL, IM2CL, DE2CL, RA2CL, WE2CL, RF2CL
      LOGICAL   ISAPPL, NEWCL, GOOD1, GOOD2, T, UPDATE, ONEBAD, TWOBAD
      INTEGER   NUMSN, NUMCL, LOOPIF, COUNT, NTERM,
     *   IPNTC1, IPNTC2, IPNTS1, IPNTS2, IPNTD1, IPNTD2, IPNTR1, IPNTR2,
     *   JPNTC1, JPNTC2, JPNTS1, JPNTS2, JPNTD1, JPNTD2, JPNTR1, JPNTR2,
     *   IPNTW1, IPNTW2, IPNTF1, IPNTF2,
     *   JPNTW1, JPNTW2, JPNTF1, JPNTF2,
     *   ISNRNO, CLIRNO, CLORNO, LOOPR,
     *   NSOU, LOOPC, LOOPD, NUMOCL, SOUR(1024), SARRAY(1024),
     *   FRQNCY(1024)
      REAL      SNRECR(2*XCLRSZ), CLRECR(XCLRSZ),
     *   AMPL, RE, IM, PH1, PH2, WT1, WT2, AMP1, AMP2, TRE, TIM, AMP,
     *   GMMOD, RANOD(25), DECNOD(25), WWT1, WWT2, LWT1, LWT2, PDELAY,
     *   GDELAY, PRATE, MBD1, MBD2, PHA, PHB, PHI, DPHIA, DPHIB, MRATE,
     *   TEMP, PHI3, PHI2, PHI1, PHI0, PI, DT, DISP1, DISP2
      DOUBLE PRECISION SNRECD(XCLRSZ), CLRECD(XCLRSZ/2), TIME1, TIME2,
     *   RFREQ, FREQ(MAXIF), TLARGE, TSMALL, TIMBG(1024), TIMED(1024),
     *   TIME
C                                       Set Times for validity tests
      PARAMETER (TLARGE=1.0D20, TSMALL=1.0D-6)
      REAL     FRQFAC(MAXIF), FINC(MAXIF)
      INTEGER ISBAND(MAXIF), NTURN
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CLREC, CLRECR, CLRECD)
C                                        Allow negative index
      EQUIVALENCE (COVER(3), SNREC, SNRECR, SNRECD)
      DATA SNLUN, CLILUN, CLOLUN /27,28,29/
      DATA COLHED /'TIME                    ',
     *             'ANTENNA NO.             '/
      DATA IRCODE /0/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      PI = 3.141592654
      UPDATE = CLIVER.EQ.CLOVER
      COUNT = 0
      KCALWD = NCALWD
C                                       If ORIGIN = 1 then SN table
C                                       was generated from a single
C                                       source file and so CALSOU is
C                                       invalid.
      IF (ORIGIN.EQ.1) KCALWD = 0
C                                       In case of negative SN table
C                                       record indices.
      COVER(1) = 0
      COVER(2) = 0
C                                       Get frequencies
      VER = 1
      NUMIF = 0
      FREQ(1) = 0.0D0
C
      CALL AXEFND (4, 'IF  ', KICTPN, SNCAT(KHCTP), IFOFF, IERR)
C
      IF ((IFOFF.GT.0) .AND. (SNCAT(KINAX+IFOFF).GE.1))
     *   CALL CHNDAT ('READ', SNBUFF, SNDISK, SNCNO, VER, SNCAT, SNLUN,
     *   NUMIF, FREQ, ISBAND, FINC, BNDCOD, FREQID, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (NUMIF.LE.0) NUMIF = 1
      CALL AXEFND (4, 'FREQ', KICTPN, SNCAT(KHCTP), FRQOFF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SN2CL: CAN NOT READ FQ (OR CH) TABLE'
         GO TO 990
         END IF
C
      IPOINT = ((KDCRV + FRQOFF - 1) * NWDPDP) + 1
C                                       Set number of polarizations
      NUMPOL = 1
      CALL AXEFND (4, 'STOK', KICTPN, SNCAT(KHCTP), STOFF, IERR)
      IF (SNCAT(KINAX+STOFF) .GE. 2) NUMPOL = 2
C                                       Get reference frequency
      CALL COPY (NWDPDP, SNCAT(IPOINT), RFREQ)
C
      DO 10 I = 1,NUMIF
         FREQ(I) = RFREQ + FREQ(I)
         FRQFAC(I) = 2.0 * 3.1415926 * FREQ(I) * 86400.0
 10      CONTINUE
C                                       Open SN table
C                                       Reformat?
      CALL SNREFM (SNDISK, SNCNO, SNVER, SNCAT, SNLUN, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL SNINI ('READ', SNBUFF, SNDISK, SNCNO, SNVER, SNCAT, SNLUN,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Lookup columns
      CALL FNDCOL (2, COLHED, 24, T, SNBUFF, KOLS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Check sort order
      IF ((SNBUFF(43).NE.KOLS(2)).OR.(SNBUFF(44).NE.KOLS(1))) THEN
         IRET = 1
         WRITE (MSGTXT,1020) SNVER
         GO TO 990
         END IF
C                                       Get number of records in table
      NUMSN = SNBUFF(5)
      IF (NUMSN.LE.0) GO TO 850
C                                       Open CL table
      GMMOD = 1.0
C                                       Reformat?
      CALL CLREFM (CLDISK, CLCNO, CLIVER, CLCAT, CLILUN, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL CALINI ('WRIT', CLIBUF, CLDISK, CLCNO, CLIVER, CLCAT,
     *   CLILUN, CLIRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM,
     *   GMMOD, IRET)
      IF (IRET.GT.0) GO TO 999
      NEWCL = IRET.LT.0
C                                       Get number of records in table
      NUMCL = CLIBUF(5)
      NEWCL = NEWCL .OR. (NUMCL.LE.0)
C                                       Set table pointers
C                                       SN Table
      TIMSN = SNKOLS(SNDTIM)
      INTSN = SNKOLS(SNRTMI)
      SOUSN = SNKOLS(SNISID)
      ANTSN = SNKOLS(SNIANT)
      SUBSN = SNKOLS(SNISUB)
      FRQSN = SNKOLS(SNIFQI)
      IFRSN = SNKOLS(SNRIFR)
      NODSN = SNKOLS(SNINOD)
      MB1SN = SNKOLS(SNRMD1)
      DI1SN = SNKOLS(SNRDS1)
      RE1SN = SNKOLS(SNRRE1)
      IM1SN = SNKOLS(SNRIM1)
      RA1SN = SNKOLS(SNRRA1)
      DL1SN = SNKOLS(SNRDE1)
      WT1SN = SNKOLS(SNRWE1)
      RF1SN = SNKOLS(SNIRF1)
      MB2SN = SNKOLS(SNRMD2)
      DI2SN = SNKOLS(SNRDS2)
      RE2SN = SNKOLS(SNRRE2)
      IM2SN = SNKOLS(SNRIM2)
      RA2SN = SNKOLS(SNRRA2)
      DL2SN = SNKOLS(SNRDE2)
      WT2SN = SNKOLS(SNRWE2)
      RF2SN = SNKOLS(SNIRF2)
C                                       CL Table
      TIMCL = CLKOLS(CLDTIM)
      INTCL = CLKOLS(CLRTMI)
      SOUCL = CLKOLS(CLISID)
      ANTCL = CLKOLS(CLIANT)
      SUBCL = CLKOLS(CLISUB)
      FRQCL = CLKOLS(CLIFQI)
      IFRCL = CLKOLS(CLRIFR)
      GEODCL = CLKOLS(CLDDEL)
      DOPCL = CLKOLS(CLRDOP)
      MB1CL = CLKOLS(CLRMD1)
      DI1CL = CLKOLS(CLRDS1)
      RE1CL = CLKOLS(CLRRE1)
      IM1CL = CLKOLS(CLRIM1)
      RA1CL = CLKOLS(CLRRA1)
      DE1CL = CLKOLS(CLRDE1)
      WE1CL = CLKOLS(CLRWE1)
      RF1CL = CLKOLS(CLIRF1)
      MB2CL = CLKOLS(CLRMD2)
      DI2CL = CLKOLS(CLRDS2)
      RE2CL = CLKOLS(CLRRE2)
      IM2CL = CLKOLS(CLRIM2)
      RA2CL = CLKOLS(CLRRA2)
      DE2CL = CLKOLS(CLRDE2)
      WE2CL = CLKOLS(CLRWE2)
      RF2CL = CLKOLS(CLIRF2)
C                                       Handle a new CL table
C                                       differently
      IF (NEWCL) GO TO 590
C                                       Check sort order
      CALL FNDCOL (2, COLHED, 24, T, CLIBUF, KOLS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((CLIBUF(43).NE.KOLS(2)) .OR. (CLIBUF(44).NE.KOLS(1))) THEN
         IRET = 1
         WRITE (MSGTXT,1031) CLIVER
         GO TO 990
         END IF
C                                       New SELF command needs lits
C                                       of sources
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CLCAT, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      IF ((SUBA.GT.0) .OR. (ALLSUB)) NUMSUB = 1
      NSRC = 0
      IF (INTMOD.EQ.1) THEN
         ISUB = 0
         IF ((SUBA.GT.0) .AND. (.NOT.ALLSUB)) ISUB = SUBA
         DO 40 ISNRNO = 1,NUMSN
            CALL TABIO ('READ', IRCODE, ISNRNO, SNREC(I1OFFI+1),
     *         SNBUFF, IRET)
            IF (IRET.LT.0) GO TO 40
            IF (IRET.NE.0) GO TO 900
            IF (ALLSUB) SNREC(I1OFFI+SUBSN) = 0
C                                       Check time
            WANTED = (SNRECD(I1OFFD+TIMSN).GE.TIMRA(1)) .AND.
     *         (SNRECD(I1OFFD+TIMSN).LE.TIMRA(2))
C                                       Test subarray
            WANTED = WANTED .AND. ((SNREC(I1OFFI+SUBSN).EQ.ISUB) .OR.
     *         (SNREC(I1OFFI+SUBSN).LE.0) .OR. (ISUB.EQ.0))
C                                       Test freqid
            WANTED = WANTED .AND. ((FREQID.LE.0) .OR.
     *         (SNREC(I1OFFI+FRQSN).LE.0) .OR.
     *         (SNREC(I1OFFI+FRQSN).EQ.FREQID))
C                                       Test if calibrator wanted
            THSOU = SNREC(I1OFFI+SOUSN)
            WANTED = WANTED .AND.
     *         (SLCTD (THSOU, CALWAN, KCALWD, DOCWNT) .OR. (THSOU.LE.0))
            IF (WANTED) THEN
               DO 20 I = 1,NSRC
                  IF (THSOU.EQ.SRCN(I)) GO TO 40
 20               CONTINUE
               NSRC = NSRC + 1
               SRCN(I) = THSOU
               END IF
 40         CONTINUE
         END IF
      NSRC = MAX (1, NSRC)
C                                       If .not. UPDATE open output
C                                       CL table.
      IF (.NOT.UPDATE) THEN
C                                       Reformat?
         CALL CLREFM (CLDISK, CLCNO, CLOVER, CLCAT, CLOLUN, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL CALINI ('WRIT', CLOBUF, CLDISK, CLCNO, CLOVER, CLCAT,
     *      CLOLUN, CLORNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IRET)
         IF (IRET.GT.0) GO TO 999
         END IF
C                                       Mark as unsorted.
      CLOBUF(43) = 0
      CLOBUF(44) = 0
C                                       Inform user
      WRITE (MSGTXT,1040)  CLIVER, CLOVER
      CALL MSGWRT (4)
C                                       Find number of subarrays.
      CALL FNDEXT ('AN', CLCAT, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      IF ((SUBA.GT.0) .OR. (ALLSUB)) NUMSUB = 1
C                                       Set up for interpolation
      I1OFFI = 0
      I1OFFR = 0
      I1OFFD = 0
      I2OFFI = XCLRSZ
      I2OFFR = XCLRSZ
      I2OFFD = XCLRSZ / NWDPDP
C                                       Loop over subarray
      SRCNOW = 0
      DO 550 I = 1,NSRC
         IF (INTMOD.EQ.1) SRCNOW = SRCN(I)
      DO 550 SUB = 1,NUMSUB
         ISUB = SUB
         IF ((SUBA.GT.0) .AND. (.NOT.ALLSUB)) ISUB = SUBA
C                                       Read first SN table entries
         TIME2 = TLARGE
         SNANT2 = 0
         ISNRNO = 0
 50      IF (NUMSN.GT.ISNRNO) THEN
            ISNRNO = ISNRNO + 1
            CALL TABIO ('READ', IRCODE, ISNRNO, SNREC(I1OFFI+1),
     *         SNBUFF, IRET)
            IF (IRET.LT.0) GO TO 50
            IF (IRET.NE.0) GO TO 900
            IF (ALLSUB) SNREC(I1OFFI+SUBSN) = 0
         ELSE
            GO TO 550
C           IF (NUMSN.LT.ISNRNO) GO TO 550
            END IF
C                                       See if any found for this
C                                       subarray.
         ONEOK = .TRUE.
C                                       Check time
         WANTED = (SNRECD(I1OFFD+TIMSN).GE.TIMRA(1)) .AND.
     *      (SNRECD(I1OFFD+TIMSN).LE.TIMRA(2))
C                                       Test subarray
         WANTED = WANTED .AND. ((SNREC(I1OFFI+SUBSN).EQ.ISUB) .OR.
     *      (SNREC(I1OFFI+SUBSN).LE.0))
C                                       Test freqid
         WANTED = WANTED .AND. ((FREQID.LE.0) .OR.
     *      (SNREC(I1OFFI+FRQSN).LE.0) .OR.
     *      (SNREC(I1OFFI+FRQSN).EQ.FREQID))
C                                       Test if calibrator wanted
         THSOU = SNREC(I1OFFI+SOUSN)
         IF (THSOU.GT.0) THEN
            WANTED = WANTED .AND. ((THSOU.EQ.SRCNOW) .OR. (SRCNOW.EQ.0))
            WANTED = WANTED .AND. SLCTD (THSOU, CALWAN, KCALWD, DOCWNT)
            END IF
C                                       Not wanted - blank
         IF (.NOT.WANTED) THEN
            ONEOK = .FALSE.
            CALL RFILL (NUMIF, FBLANK, SNRECR(I1OFFR+RE1SN))
            CALL RFILL (NUMIF, FBLANK, SNRECR(I1OFFR+RE2SN))
            GO TO 50
            END IF
         LSTSOU = THSOU
         TIME1 = SNRECD(I1OFFD+TIMSN)
         SNANT1 = SNREC(I1OFFI+ANTSN)
C                                       2nd record:
         IF (NUMSN.LE.ISNRNO) ISNRNO = ISNRNO - 1
 80      IF (NUMSN.GT.ISNRNO) THEN
            ISNRNO = ISNRNO + 1
            CALL TABIO ('READ', IRCODE, ISNRNO, SNREC(I2OFFI+1), SNBUFF,
     *         IRET)
            IF (IRET.LT.0) GO TO 80
            IF (IRET.NE.0) GO TO 900
            IF (ALLSUB) SNREC(I2OFFI+SUBSN) = 0
         ELSE
            GO TO 100
C           IF (NUMSN.LT.ISNRNO) GO TO 100
            END IF
         TWOOK = .TRUE.
C                                       Check time
         WANTED = (SNRECD(I2OFFD+TIMSN).GE.TIMRA(1)) .AND.
     *      (SNRECD(I2OFFD+TIMSN).LE.TIMRA(2))
C                                       Test subarray
         WANTED = WANTED .AND. ((SNREC(I2OFFI+SUBSN).EQ.ISUB) .OR.
     *      (SNREC(I2OFFI+SUBSN).LE.0))
C                                       Test freqid
         WANTED = WANTED .AND. ((SNREC(I2OFFI+FRQSN).EQ.FREQID) .OR.
     *      (FREQID.LE.0) .OR. (SNREC(I2OFFI+FRQSN).LE.0))
C                                       Test if calibrator wanted
         THSOU = SNREC(I2OFFI+SOUSN)
         IF (THSOU.GT.0) THEN
            WANTED = WANTED .AND. ((THSOU.EQ.SRCNOW) .OR. (SRCNOW.EQ.0))
            WANTED = WANTED .AND. SLCTD (THSOU, CALWAN, KCALWD, DOCWNT)
            END IF
C                                       Not wanted - blank
         IF (.NOT.WANTED) THEN
            TWOOK = .FALSE.
            CALL RFILL (NUMIF, FBLANK, SNRECR(I2OFFR+RE1SN))
            CALL RFILL (NUMIF, FBLANK, SNRECR(I2OFFR+RE2SN))
            GO TO 80
            END IF
 100     LSTSOU = THSOU
         IF (NUMSN.GE.ISNRNO) THEN
            TIME2 = SNRECD(I2OFFD+TIMSN)
            SNANT2 = SNREC(I2OFFI+ANTSN)
            END IF
         IF (SNANT1.NE.SNANT2) TIME2 = TLARGE
C                                       Trap multiple entry at same time
         IF ((TIME2-TIME1).LT.TSMALL) GO TO 80
         LSTCAL = -1
C                                       Loop thru tables.
         DO 500 LOOPR = 1,NUMCL
C                                       Read OLD CL record
            CLIRNO = LOOPR
            CALL TABIO ('READ', IRCODE, CLIRNO, CLREC, CLIBUF, IRET)
            IF (IRET.LT.0) GO TO 500
            IF (IRET.NE.0) GO TO 900
            IF (ALLSUB) CLREC(SUBCL) = 0
            TIME = CLRECD(TIMCL)
C                                       Check time
            IF ((TIME.LT.TIMRA(1)) .OR. (TIME.GT.TIMRA(2))) GO TO 500
            CLANT = CLREC(ANTCL)
C                                       Test if source wanted.
            THSOU = CLREC(SOUCL)
            IF (THSOU.GT.0) THEN
               IF ((THSOU.NE.LSTCAL) .AND.
     *            (.NOT. ( SLCTD (THSOU, SOUWAN, NSOUWD, DOSWNT))))
     *            GO TO 500
C                                       current SELF source?
               IF ((SRCNOW.GT.0) .AND. (THSOU.NE.SRCNOW)) GO TO 500
               END IF
            LSTCAL = THSOU
C                                       Test subarray
            IF ((CLREC(SUBCL).NE.ISUB) .AND. (CLREC(SUBCL).GT.0))
     *         GO TO 500
C                                       Test freqid
            IF ((CLREC(FRQCL).NE.FREQID) .AND. (CLREC(FRQCL).GT.0) .AND.
     *         (FREQID.GT.0)) GO TO 500
C                                       Antenna wanted?
            IF (.NOT.SLCTD (CLANT, ANTENS, NANTSL, DOAWNT)) GO TO 500
 130        IF (CLANT.LT.SNANT1) THEN
C                                       No SN entry, pass through anyway
               IF (ALLPAS) GO TO 490
C                                       No SN entry, skip CL record
               GO TO 500
               END IF
            IF ((TIME.LE.TIME2) .AND. (CLANT.EQ.SNANT1) .AND. ONEOK)
     *         GO TO 170
            IF (ISNRNO.GT.NUMSN) THEN
C                                       Finished SN table, pass through
C                                       anyway
               IF (ALLPAS) GO TO 490
C                                       Finished SN table, skip CL record
               GO TO 500
               END IF
C                                       Find appropriate SN record -
C                                       Switch buffer pointers
 140           ITEMP = I2OFFI
               I2OFFI = I1OFFI
               I1OFFI = ITEMP
               ITEMP = I2OFFR
               I2OFFR = I1OFFR
               I1OFFR = ITEMP
               ITEMP = I2OFFD
               I2OFFD = I1OFFD
               I1OFFD = ITEMP
               ONEOK = TWOOK
C                                       Try next record.
               TIME2 = TLARGE
               SNANT2 = 0
               TIME1 = SNRECD(I1OFFD+TIMSN)
               SNANT1 = SNREC(I1OFFI+ANTSN)
C                                       Jump here to read SN table
 145           ISNRNO = ISNRNO + 1
               IRET = 0
C                                       If more table entries, read
               IF (NUMSN.GE.ISNRNO) THEN
                  CALL TABIO ('READ', IRCODE, ISNRNO, SNREC(I2OFFI+1),
     *               SNBUFF, IRET)
                  IF (IRET.LT.0) GO TO 145
                  IF (IRET.NE.0) GO TO 999
                  IF (ALLSUB) SNREC(I2OFFI+SUBSN) = 0
                  END IF
               IF (NUMSN.LT.ISNRNO) GO TO 160
               TWOOK = .TRUE.
C                                       Check time
               WANTED = (SNRECD(I2OFFD+TIMSN).GE.TIMRA(1)) .AND.
     *            (SNRECD(I2OFFD+TIMSN).LE.TIMRA(2))
C                                       Test subarray
               WANTED = WANTED .AND. ((SNREC(I2OFFI+SUBSN).EQ.ISUB) .OR.
     *            (SNREC(I2OFFI+SUBSN).LE.0))
C                                       Test freq id
               WANTED = WANTED .AND. ((SNREC(I2OFFI+FRQSN).EQ.FREQID)
     *            .OR. (FREQID.LE.0) .OR. (SNREC(I2OFFI+FRQSN).LE.0))
C                                       Test if calibrator wanted
               THSOU = SNREC(I2OFFI+SOUSN)
               IF (THSOU.GT.0) THEN
                  WANTED = WANTED .AND. ((THSOU.EQ.SRCNOW) .OR.
     *               (SRCNOW.EQ.0))
                  WANTED = WANTED .AND.
     *               SLCTD (THSOU, CALWAN, KCALWD, DOCWNT)
                  END IF
C                                       Not wanted - blank
               IF (.NOT.WANTED) THEN
                  TWOOK = .FALSE.
                  CALL RFILL (NUMIF, FBLANK, SNRECR(I2OFFR+RE1SN))
                  CALL RFILL (NUMIF, FBLANK, SNRECR(I2OFFR+RE2SN))
                  GO TO 145
                  END IF
               LSTSOU = THSOU
C                                       Check antenna
               IF (SNREC(I1OFFI+ANTSN) .LT.CLANT) GO TO 140
 160           IF (NUMSN.GE.ISNRNO) THEN
                  TIME2 = SNRECD(I2OFFD+TIMSN)
                  SNANT2 = SNREC(I2OFFI+ANTSN)
                  END IF
               IF (SNANT1.NE.SNANT2) TIME2 = TLARGE
               IF ((SNANT1.NE.CLANT) .AND. (SNANT2.NE.CLANT)) THEN
C                                       Antenna # not match, pass thru
                  IF (ALLPAS) GO TO 490
C                                       Antennas not match, skip record
                  GO TO 500
                  END IF
C                                        At least one valid SN
               IF (.NOT.(ONEOK.OR.TWOOK)) GO TO 500
C                                       IF multiple entry at same time
               IF ((TIME2-TIME1).LT.TSMALL.AND.ISNRNO.LT.NUMSN) THEN
C                                       Reset second entry for read
                  SNANT2 = 0
C                                       Paddy (Jordrel) Fix
                  TIME2  = TLARGE
C                                       Jump to read next SN record
                  GO TO 145
                  END IF
               GO TO 130
C                                       Trap duplicate records.
 170           IF ((TIME2-TIME1).LT.TSMALL) GO TO 500
C                                       Interpolate
C                                       Trap "SELF" calibration.
               IF (INTMOD.EQ.2) THEN
C                                       Check sources
                  ONEBAD = (CLREC(SOUCL).NE.SNREC(I1OFFI+SOUSN)).AND.
     *               (CLREC(SOUCL).GT.0).AND.(SNREC(I1OFFI+SOUSN).GT.0)
                  ONEBAD = ONEBAD .OR. (CLANT.NE.SNANT1)
                  TWOBAD = (CLREC(SOUCL).NE.SNREC(I2OFFI+SOUSN)).AND.
     *               (CLREC(SOUCL).GT.0).AND.(SNREC(I2OFFI+SOUSN).GT.0)
                  TWOBAD = TWOBAD .OR. (CLANT.NE.SNANT2)
C
C                  ONEBAD = (CLREC(SOUCL).NE.SNREC(I1OFFI+SOUSN)) .OR.
C     *               (CLANT.NE.SNANT1)
C                  TWOBAD = (CLREC(SOUCL).NE.SNREC(I2OFFI+SOUSN)) .OR.
C     *               (CLANT.NE.SNANT2)
                  IF (ONEBAD. AND. (.NOT.TWOBAD)) THEN
                     WT1 = 0.0
                  ELSE IF (TWOBAD. AND. (.NOT.ONEBAD)) THEN
                     WT1 = 1.0
                  ELSE IF (ONEBAD.AND.TWOBAD) THEN
                     GO TO 500
C                                       Both OK pick closest
                  ELSE IF ((.NOT.ONEBAD) .AND. (.NOT.TWOBAD)) THEN
                     IF (ABS (TIME1-TIME) .LT. ABS (TIME2-TIME)) THEN
                        WT1 = 1.0
                     ELSE
                        WT1 = 0.0
                        END IF
                     END IF
               ELSE
C                                       Normal calibration.
                  WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
                  IF (SNANT1.NE.SNANT2) WT1 = 1.0
C                  IF (TIME.LT.TIME1) WT1 = 1.0
C                  IF (TIME.GT.TIME2) WT1 = 0.0
                  END IF
               WT2 = 1.0 - WT1
C                                       Set pointers
               IPNTC1 = I1OFFR + RE1SN
               IPNTC2 = I2OFFR + RE1SN
               IPNTS1 = I1OFFR + IM1SN
               IPNTS2 = I2OFFR + IM1SN
               IPNTD1 = I1OFFR + DL1SN
               IPNTD2 = I2OFFR + DL1SN
               IPNTR1 = I1OFFR + RA1SN
               IPNTR2 = I2OFFR + RA1SN
               IPNTW1 = I1OFFR + WT1SN
               IPNTW2 = I2OFFR + WT1SN
               IPNTF1 = I1OFFI + RF1SN
               IPNTF2 = I2OFFI + RF1SN
C                                       Second Stokes'
               IF (NUMPOL.GT.1) THEN
                  JPNTC1 = I1OFFR + RE2SN
                  JPNTC2 = I2OFFR + RE2SN
                  JPNTS1 = I1OFFR + IM2SN
                  JPNTS2 = I2OFFR + IM2SN
                  JPNTD1 = I1OFFR + DL2SN
                  JPNTD2 = I2OFFR + DL2SN
                  JPNTR1 = I1OFFR + RA2SN
                  JPNTR2 = I2OFFR + RA2SN
                  JPNTW1 = I1OFFR + WT2SN
                  JPNTW2 = I2OFFR + WT2SN
                  JPNTF1 = I1OFFI + RF2SN
                  JPNTF2 = I2OFFI + RF2SN
                  END IF
C                                        Multiband delays
            GOOD1 = (SNRECR(I1OFFR+MB1SN).NE.FBLANK) .AND.
     *         (WT1.NE.0.0)
            GOOD2 = (SNRECR(I2OFFR+MB1SN).NE.FBLANK) .AND.
     *         (WT2.NE.0.0)
            IF (GOOD1 .OR. GOOD2) THEN
               IF (SNREC(IPNTF1).NE.SNREC(IPNTF2)) THEN
C                                       Different reference ants -
C                                       use the closest
                  IF (GOOD1 .AND. GOOD2) THEN
                     GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                     GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                     END IF
                  END IF
               WWT1 = 0.0
               WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Fetch multiband delays corrected
C                                       for rate at TIME.
               MBD1 = 0.0
               MBD2 = 0.0
               IF (GOOD1) CALL GETMBD (TIME, SNRECR, NUMIF, TIME1,
     *            I1OFFR+MB1SN, IPNTR1, MBD1)
               IF (GOOD2) CALL GETMBD (TIME, SNRECR, NUMIF, TIME2,
     *            I2OFFR+MB1SN, IPNTR2, MBD2)
               IF (CLRECR(MB1CL).NE.FBLANK) CLRECR(MB1CL) =
     *            CLRECR(MB1CL) + WWT1*MBD1 + WWT2*MBD2
            ELSE
               CLRECR(MB1CL) = FBLANK
               END IF
C                                        Second polarization
            IF (NUMPOL.GT.1) THEN
               GOOD1 = (SNRECR(I1OFFR+MB2SN).NE.FBLANK) .AND.
     *         (WT1.NE.0.0)
               GOOD2 = (SNRECR(I2OFFR+MB2SN).NE.FBLANK) .AND.
     *         (WT2.NE.0.0)
               IF (GOOD1.OR.GOOD2) THEN
                  IF (SNREC(IPNTF1).NE.SNREC(IPNTF2)) THEN
C                                       Different reference ants -
C                                       use the closest
                     IF (GOOD1.AND.GOOD2) THEN
                        GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                        GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                        END IF
                     END IF
                  WWT1 = 0.0
                  WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Fetch multiband delays corrected
C                                       for rate at TIME.
                  MBD1 = 0.0
                  MBD2 = 0.0
                  IF (GOOD1) CALL GETMBD (TIME, SNRECR, NUMIF, TIME1,
     *               I1OFFR+MB2SN, JPNTR1, MBD1)
                  IF (GOOD2) CALL GETMBD (TIME, SNRECR, NUMIF, TIME2,
     *               I2OFFR+MB2SN, JPNTR2, MBD2)
                  IF (CLRECR(MB2CL).NE.FBLANK) CLRECR(MB2CL) =
     *               CLRECR(MB2CL) + WWT1*MBD1 + WWT2*MBD2
               ELSE
                  CLRECR(MB2CL) = FBLANK
                  END IF
               END IF
C                                        Dispersions
            GOOD1 = (SNRECR(I1OFFR+DI1SN).NE.FBLANK) .AND.
     *         (WT1.NE.0.0)
            GOOD2 = (SNRECR(I2OFFR+DI1SN).NE.FBLANK) .AND.
     *         (WT2.NE.0.0)
            IF (GOOD1 .OR. GOOD2) THEN
               IF (SNREC(IPNTF1).NE.SNREC(IPNTF2)) THEN
C                                       Different reference ants -
C                                       use the closest
                  IF (GOOD1 .AND. GOOD2) THEN
                     GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                     GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                     END IF
                  END IF
               WWT1 = 0.0
               WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Fetch multiband delays corrected
C                                       for rate at TIME.
               DISP1 = 0.0
               DISP2 = 0.0
               IF (GOOD1) DISP1 = SNRECR(I1OFFR+DI1SN)
               IF (GOOD2) DISP2 = SNRECR(I2OFFR+DI1SN)
               IF (CLRECR(DI1CL).NE.FBLANK) CLRECR(DI1CL) =
     *            CLRECR(DI1CL) + WWT1*DISP1 + WWT2*DISP2
            ELSE
               CLRECR(DI1CL) = FBLANK
               END IF
C                                        Second polarization
            IF (NUMPOL.GT.1) THEN
               GOOD1 = (SNRECR(I1OFFR+DI2SN).NE.FBLANK) .AND.
     *         (WT1.NE.0.0)
               GOOD2 = (SNRECR(I2OFFR+DI2SN).NE.FBLANK) .AND.
     *         (WT2.NE.0.0)
               IF (GOOD1.OR.GOOD2) THEN
                  IF (SNREC(IPNTF1).NE.SNREC(IPNTF2)) THEN
C                                       Different reference ants -
C                                       use the closest
                     IF (GOOD1.AND.GOOD2) THEN
                        GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                        GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                        END IF
                     END IF
                  WWT1 = 0.0
                  WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Fetch multiband delays corrected
C                                       for rate at TIME.
                  DISP1 = 0.0
                  DISP2 = 0.0
                  IF (GOOD1) DISP1 = SNRECR(I1OFFR+DI2SN)
                  IF (GOOD2) DISP2 = SNRECR(I2OFFR+DI2SN)
                  IF (CLRECR(DI2CL).NE.FBLANK) CLRECR(DI2CL) =
     *               CLRECR(DI2CL) + WWT1*DISP1 + WWT2*DISP2
               ELSE
                  CLRECR(DI2CL) = FBLANK
                  END IF
               END IF
C                                       Loop over IF
            DO 300 LOOPIF = 1,NUMIF
C                                       First Stokes
               GOOD1 = (SNRECR(IPNTS1).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTC1).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTD1).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTR1).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTW1).GT.0.0) .AND. (WT1.NE.0.0)
               GOOD2 = (SNRECR(IPNTS2).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTC2).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTD2).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTR2).NE.FBLANK) .AND.
     *                 (SNRECR(IPNTW2).GT.0.0) .AND. (WT2.NE.0.0)
C                                       Interpolating too far ?
               IF (DLIMI.GT.0.0D0) THEN
                  IF (GOOD1) THEN
                     GOOD1 = ABS(TIME1-TIME).LE.DLIMI
                     END IF
                  IF (GOOD2) THEN
                     GOOD2 = ABS(TIME2-TIME).LE.DLIMI
                     END IF
                  END IF
C                                       Different reference ants -
C                                       use the closest
               IF (SNREC(IPNTF1).NE.SNREC(IPNTF2)) THEN
                  IF (GOOD1.AND.GOOD2) THEN
                     GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                     GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                     END IF
                  END IF
               IF (.NOT.(GOOD1.OR.GOOD2)) GO TO 220
C
               PH1 = 0.0
               PH2 = 0.0
               AMP1 = 0.0
               AMP2 = 0.0
               WWT1 = 0.0
               WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               LWT1 = MAX (0.0, MIN (1.0, WWT1))
               LWT2 = MAX (0.0, MIN (1.0, WWT2))
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Interpolate Amplitude
               IF (GOOD1) AMP1 = SQRT ((SNRECR(IPNTS1)**2) +
     *            (SNRECR(IPNTC1)**2))
               IF (GOOD2) AMP2 = SQRT ((SNRECR(IPNTS2)**2) +
     *            (SNRECR(IPNTC2)**2))
C                                       Interpolate Phase
C                                        Vector `2PT' method
C                                         or `SELF'
               IF ((INTMOD.LE.2) .OR. (.NOT.(GOOD1.AND.GOOD2))) THEN
                  IF (GOOD1) PH1 = ATAN2 (SNRECR(IPNTS1),
     *               SNRECR(IPNTC1)) +
     *               SNRECR(IPNTR1) *(TIME - TIME1) * FRQFAC(LOOPIF)
                  IF (GOOD2) PH2 = ATAN2 (SNRECR(IPNTS2),
     *               SNRECR(IPNTC2)) +
     *               SNRECR(IPNTR2) * (TIME - TIME2) * FRQFAC(LOOPIF)
C
                  RE = WWT1 * COS (PH1) + WWT2 * COS (PH2)
                  IM = WWT1 * SIN (PH1) + WWT2 * SIN (PH2)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                        SIMP method
               ELSE IF (INTMOD.EQ.3) THEN
                  PHA = ATAN2(SNRECR(IPNTS1), SNRECR(IPNTC1))
                  PHB = ATAN2(SNRECR(IPNTS2), SNRECR(IPNTC2))
                  IF (ABS(PHB-PHA).GT.PI) THEN
                     IF ((PHB-PHA).GT.0.0) THEN
                        PHB=PHB-2.0*PI
                     ELSE
                        PHB=PHB+2.0*PI
                        END IF
                     END IF
                  PHI = WWT1 * PHA +  WWT2 * PHB
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                       AMBG method
               ELSE IF (INTMOD.EQ.4) THEN
                  PHA = ATAN2 (SNRECR(IPNTS1), SNRECR(IPNTC1))
                  PHB = ATAN2 (SNRECR(IPNTS2), SNRECR(IPNTC2))
                  DPHIA = SNRECR(IPNTR1)*FRQFAC(LOOPIF)
                  DPHIB = SNRECR(IPNTR2)*FRQFAC(LOOPIF)
                  MRATE = (DPHIA + DPHIB)/2.0
                  TEMP= PHA + MRATE*(TIME2-TIME1) - PHB
                  TEMP = TEMP/(2.0*PI)
                  IF (TEMP.GE.0.0) THEN
                     NTURN = INT(TEMP+0.5)
                  ELSE
                     NTURN = INT(TEMP-0.5)
                     END IF
                  PHB = PHB + (FLOAT(NTURN)*2.0*PI)
                  PHI = WWT1 * PHA + WWT2 * PHB
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                       CUBIC method
               ELSE IF (INTMOD.EQ.5) THEN
                  PHA = ATAN2 (SNRECR(IPNTS1), SNRECR(IPNTC1))
                  PHB = ATAN2 (SNRECR(IPNTS2), SNRECR(IPNTC2))
                  DPHIA = SNRECR(IPNTR1)*FRQFAC(LOOPIF)
                  DPHIB = SNRECR(IPNTR2)*FRQFAC(LOOPIF)
                  MRATE = (DPHIA + DPHIB)/2.0
                  TEMP= PHA + MRATE*(TIME2-TIME1) - PHB
                  TEMP = TEMP/(2.0*PI)
                  IF (TEMP.GE.0.0) THEN
                     NTURN = INT(TEMP+0.5)
                  ELSE
                     NTURN = INT(TEMP-0.5)
                     END IF
                  PHB = PHB + (FLOAT(NTURN)*2.0*PI)
                  DT = (TIME2 - TIME1)
                  PHI0 = PHA
                  PHI1 = DPHIA
                  PHI2 =(3*(PHB-PHA)-2*DPHIA*DT-DPHIB*DT)/DT**2
                  PHI3 =-(2*(PHB-PHA)-DPHIA*DT-DPHIB*DT)/DT**3
                  PHI = PHI0 + PHI1*(TIME-TIME1) +
     *               PHI2*(TIME-TIME1)**2 + PHI3*(TIME-TIME1)**3
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
                  END IF
C                                       Apply Phase/Amplitude
C                                       Correction to data
               TRE = CLRECR(RE1CL+LOOPIF-1)
               TIM = CLRECR(IM1CL+LOOPIF-1)
C
               GOODCL = (TIM.NE.FBLANK) .AND. (TRE.NE.FBLANK)
C                                       Do not interpolate blanked
C                                       CL records.
               IF (.NOT.GOODCL) GO TO 220
C
               IF ((ABS (TRE) + ABS(TIM)) .LT.1.0E-10) TRE = 1.0
               IF (GOODCL) THEN
                  CLRECR(RE1CL+LOOPIF-1) = TRE*RE - TIM*IM
                  CLRECR(IM1CL+LOOPIF-1) = TRE*IM + TIM*RE
               END IF
               PDELAY = ATAN2 (IM, RE) / (FREQ(LOOPIF) * 6.283185308)
C                                       Interpolate Delay
               IF (GOOD1 .AND. GOOD2) THEN
                  GDELAY = WWT1 * SNRECR(IPNTD1) + WWT2 * SNRECR(IPNTD2)
               ELSE IF (GOOD1) THEN
                  GDELAY = SNRECR(IPNTD1)
               ELSE IF (GOOD2) THEN
                  GDELAY = SNRECR(IPNTD2)
                  ENDIF
               IF (CLRECR(DE1CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(DE1CL+LOOPIF-1) = CLRECR(DE1CL+LOOPIF-1) +
     *            GDELAY
C                                       Interpolate Rate
               IF (GOOD1 .AND. GOOD2) THEN
                  PRATE = WWT1 * SNRECR(IPNTR1) + WWT2 * SNRECR(IPNTR2)
               ELSE IF (GOOD1) THEN
                  PRATE = SNRECR(IPNTR1)
               ELSE IF (GOOD2) THEN
                  PRATE = SNRECR(IPNTR2)
                  ENDIF
               IF (CLRECR(RA1CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(RA1CL+LOOPIF-1) = CLRECR(RA1CL+LOOPIF-1) +
     *            PRATE
C                                       Interpolate Weight
               CLRECR(WE1CL+LOOPIF-1) = SNRECR(IPNTW1)*LWT1 +
     *            SNRECR(IPNTW2)*LWT2
C                                       Reference antenna
               CLREC(RF1CL+LOOPIF-1) = SNREC(IPNTF1)
               IF (.NOT.GOOD1) CLREC(RF1CL+LOOPIF-1) = SNREC(IPNTF2)
C                                       Update total values.
               GO TO 225
C                                       Bad solution, blank
 220           CLRECR(RE1CL+LOOPIF-1) = FBLANK
               CLRECR(IM1CL+LOOPIF-1) = FBLANK
               CLRECR(DE1CL+LOOPIF-1) = FBLANK
               CLRECR(RA1CL+LOOPIF-1) = FBLANK
               CLRECR(WE1CL+LOOPIF-1) = 0.0
C                                       Update pointers
 225           IPNTC1 = IPNTC1 + 1
               IPNTC2 = IPNTC2 + 1
               IPNTS1 = IPNTS1 + 1
               IPNTS2 = IPNTS2 + 1
               IPNTD1 = IPNTD1 + 1
               IPNTD2 = IPNTD2 + 1
               IPNTR1 = IPNTR1 + 1
               IPNTR2 = IPNTR2 + 1
               IPNTW1 = IPNTW1 + 1
               IPNTW2 = IPNTW2 + 1
               IPNTF1 = IPNTF1 + 1
               IPNTF2 = IPNTF2 + 1
C                                       Second polarization
               IF (NUMPOL.LE.1) GO TO 300
C                                       Phase
               GOOD1 = (SNRECR(JPNTS1).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTC1).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTD1).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTR1).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTW1).GT.0.0) .AND. (WT1.NE.0.0)
               GOOD2 = (SNRECR(JPNTS2).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTC2).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTD2).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTR2).NE.FBLANK) .AND.
     *                 (SNRECR(JPNTW2).GT.0.0) .AND. (WT2.NE.0.0)
C                                       Interpolating too far ?
               IF (DLIMI.GT.0.0D0) THEN
                  IF (GOOD1) THEN
                     GOOD1 = ABS(TIME1-TIME).LE.DLIMI
                     END IF
                  IF (GOOD2) THEN
                     GOOD2 = ABS(TIME2-TIME).LE.DLIMI
                     END IF
                  END IF
C                                       Different reference ants -
C                                       use the closest
               IF (SNREC(JPNTF1).NE.SNREC(JPNTF2)) THEN
                  IF ((GOOD1) .AND. (GOOD2)) THEN
                     GOOD1 = ((TIME-TIME1) .LE. (TIME2-TIME))
                     GOOD2 = ((TIME-TIME1) .GT. (TIME2-TIME))
                     END IF
                  END IF
               IF (.NOT.(GOOD1.OR.GOOD2)) GO TO 290
C
               PH1 = 0.0
               PH2 = 0.0
               AMP1 = 0.0
               AMP2 = 0.0
               WWT1 = 0.0
               WWT2 = 0.0
               IF (GOOD1.AND.GOOD2) THEN
                  WWT1 = WT1
                  WWT2 = WT2
                  WWT1 = WWT1 / (WWT1 + WWT2)
                  WWT2 = 1.0 - WWT1
               ELSE IF (GOOD1) THEN
                  WWT1 = 1.0
               ELSE IF (GOOD2) THEN
                  WWT2 = 1.0
                  END IF
               LWT1 = MAX (0.0, MIN (1.0, WWT1))
               LWT2 = MAX (0.0, MIN (1.0, WWT2))
               WWT1 = MAX (-0.5, MIN (1.5, WWT1))
               WWT2 = MAX (-0.5, MIN (1.5, WWT2))
C                                       Interpolate Amplitude
C
               IF (GOOD1) AMP1 = SQRT ((SNRECR(JPNTS1)**2) +
     *            (SNRECR(JPNTC1)**2))
               IF (GOOD2) AMP2 = SQRT ((SNRECR(JPNTS2)**2) +
     *            (SNRECR(JPNTC2)**2))
C                                       Vector `2PT' method
C                                       or `SELF'
               IF ((INTMOD.LE.2).OR.(.NOT.(GOOD1.AND.GOOD2)))
     *             THEN
                  IF (GOOD1) PH1 = ATAN2 (SNRECR(JPNTS1),
     *               SNRECR(JPNTC1)) + SNRECR(JPNTR1) *
     *               (TIME - TIME1) * FRQFAC(LOOPIF)
                  IF (GOOD2) PH2 = ATAN2 (SNRECR(JPNTS2),
     *               SNRECR(JPNTC2)) + SNRECR(JPNTR2) *
     *               (TIME - TIME2) * FRQFAC(LOOPIF)
                  RE = WWT1 * COS (PH1) + WWT2 * COS (PH2)
                  IM = WWT1 * SIN (PH1) + WWT2 * SIN (PH2)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                       SIMP method
               ELSE IF (INTMOD.EQ.3) THEN
                  PHA = ATAN2(SNRECR(JPNTS1), SNRECR(JPNTC1))
                  PHB = ATAN2(SNRECR(JPNTS2), SNRECR(JPNTC2))
                  IF (ABS(PHB-PHA).GT.PI) THEN
                     IF ((PHB-PHA).GT.0.0) THEN
                        PHB=PHB-2.0*PI
                     ELSE
                        PHB=PHB+2.0*PI
                        END IF
                     END IF
                  PHI = WWT1 * PHA +  WWT2 * PHB
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                       AMBG method
               ELSE IF (INTMOD.EQ.4) THEN
                  PHA = ATAN2 (SNRECR(JPNTS1), SNRECR(JPNTC1))
                  PHB = ATAN2 (SNRECR(JPNTS2), SNRECR(JPNTC2))
                  DPHIA = SNRECR(JPNTR1)*FRQFAC(LOOPIF)
                  DPHIB = SNRECR(JPNTR2)*FRQFAC(LOOPIF)
                  MRATE = (DPHIA + DPHIB)/2.0
                  TEMP= PHA + MRATE*(TIME2-TIME1) - PHB
                  TEMP = TEMP/(2.0*PI)
                  IF (TEMP.GE.0.0) THEN
                     NTURN = INT(TEMP+0.5)
                  ELSE
                     NTURN = INT(TEMP-0.5)
                     END IF
                  PHB = PHB + (FLOAT(NTURN)*2.0*PI)
                  PHI = WWT1 * PHA +  WWT2 * PHB
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
C                                       CUBIC method
               ELSE IF (INTMOD.EQ.5) THEN
                  PHA = ATAN2 (SNRECR(JPNTS1), SNRECR(JPNTC1))
                  PHB = ATAN2 (SNRECR(JPNTS2), SNRECR(JPNTC2))
                  DPHIA = SNRECR(JPNTR1)*FRQFAC(LOOPIF)
                  DPHIB = SNRECR(JPNTR2)*FRQFAC(LOOPIF)
                  MRATE = (DPHIA + DPHIB)/2.0
                  TEMP= PHA + MRATE*(TIME2-TIME1)-PHB
                  TEMP = TEMP/(2.0*PI)
                  IF (TEMP.GE.0.0) THEN
                     NTURN = INT(TEMP+0.5)
                  ELSE
                     NTURN = INT(TEMP-0.5)
                     END IF
                  PHB = PHB + (FLOAT(NTURN)*2.0*PI)
                  DT = (TIME2 - TIME1)
                  PHI0 = PHA
                  PHI1 = DPHIA
                  PHI2 =(3*(PHB-PHA)-2*DPHIA*DT-DPHIB*DT)/DT**2
                  PHI3 =-(2*(PHB-PHA)-DPHIA*DT-DPHIB*DT)/DT**3
                  PHI = PHI0 + PHI1*(TIME-TIME1) +
     *               PHI2*(TIME-TIME1)**2 + PHI3*(TIME-TIME1)**3
                  RE = COS(PHI)
                  IM = SIN(PHI)
                  AMP = WWT1 * AMP1 + AMP2 * WWT2
                  AMPL = SQRT (RE*RE + IM*IM)
                  RE = AMP * RE / AMPL
                  IM = AMP * IM / AMPL
                  END IF
C                                       Apply Phase/Amplitude
C                                       Correction to data
               TRE = CLRECR(RE2CL+LOOPIF-1)
               TIM = CLRECR(IM2CL+LOOPIF-1)
C                                       Do not interpolate blanked
C                                       CL records.
               GOODCL = (TIM.NE.FBLANK) .AND. (TRE.NE.FBLANK)
               IF (.NOT.GOODCL) GO TO 290
               IF ((ABS (TRE) + ABS(TIM)) .LT.1.0E-10) TRE = 1.0
               IF (GOODCL) THEN
                  CLRECR(RE2CL+LOOPIF-1) = TRE*RE - TIM*IM
                  CLRECR(IM2CL+LOOPIF-1) = TRE*IM + TIM*RE
                  END IF
               PDELAY = ATAN2 (IM, RE) / (FREQ(LOOPIF) * 6.283185308)
C
C                                        Interpolate Delay
C
               IF (GOOD1 .AND. GOOD2) THEN
                  GDELAY = WWT1 * SNRECR(JPNTD1) + WWT2 * SNRECR(JPNTD2)
               ELSE IF (GOOD1) THEN
                  GDELAY = SNRECR(JPNTD1)
               ELSE IF (GOOD2) THEN
                  GDELAY = SNRECR(JPNTD2)
                  ENDIF
               IF (CLRECR(DE2CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(DE2CL+LOOPIF-1) = CLRECR(DE2CL+LOOPIF-1) +
     *            GDELAY
C                                       Interpolate Rate
               IF (GOOD1 .AND. GOOD2) THEN
                  PRATE = WWT1 * SNRECR(JPNTR1) + WWT2 * SNRECR(JPNTR2)
               ELSE IF (GOOD1) THEN
                  PRATE = SNRECR(JPNTR1)
               ELSE IF (GOOD2) THEN
                  PRATE = SNRECR(JPNTR2)
                  ENDIF
               IF (CLRECR(RA2CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(RA2CL+LOOPIF-1) = CLRECR(RA2CL+LOOPIF-1) +
     *            PRATE
C                                       Interpolate Weight
               CLRECR(WE2CL+LOOPIF-1) = SNRECR(JPNTW1)*LWT1 +
     *            SNRECR(JPNTW2)*LWT2
C                                       Reference antenna
               CLREC(RF2CL+LOOPIF-1) = SNREC(JPNTF1)
               IF (.NOT.GOOD1) CLREC(RF2CL+LOOPIF-1) = SNREC(JPNTF2)
C                                       Update total values.
               GO TO 295
C                                       Bad solution, blank
 290           CLRECR(RE2CL+LOOPIF-1) = FBLANK
               CLRECR(IM2CL+LOOPIF-1) = FBLANK
               CLRECR(DE2CL+LOOPIF-1) = FBLANK
               CLRECR(RA2CL+LOOPIF-1) = FBLANK
               CLRECR(WE2CL+LOOPIF-1) = 0.0
C                                       Update pointers
 295           JPNTC1 = JPNTC1 + 1
               JPNTC2 = JPNTC2 + 1
               JPNTS1 = JPNTS1 + 1
               JPNTS2 = JPNTS2 + 1
               JPNTD1 = JPNTD1 + 1
               JPNTD2 = JPNTD2 + 1
               JPNTR1 = JPNTR1 + 1
               JPNTR2 = JPNTR2 + 1
               JPNTW1 = JPNTW1 + 1
               JPNTW2 = JPNTW2 + 1
               JPNTF1 = JPNTF1 + 1
               JPNTF2 = JPNTF2 + 1
 300           CONTINUE
C                                       Rewrite record
 490           IF (UPDATE) THEN
                  CALL TABIO ('WRIT', IRCODE, CLIRNO, CLREC, CLIBUF,
     *               IRET)
               ELSE
                  CALL TABIO ('WRIT', IRCODE, CLORNO, CLREC, CLOBUF,
     *               IRET)
                  END IF
               CLORNO = CLORNO + 1
               IF (IRET.NE.0) GO TO 900
               COUNT = COUNT + 1
 500        CONTINUE
 550     CONTINUE
C                                       Done with interpolation;
C                                       if no matches found copy to
C                                       end of the CL table
      IF (COUNT.GT.0) GO TO 850
C
      CLORNO = CLORNO - 1
C                                       No overlap
      GO TO 600
C                                       New CL table, close input
C                                       open as output
 590     CALL TABIO ('CLOS', IRCODE, LOOPR, CLREC, CLIBUF, IRET)
         IF (IRET.NE.0) GO TO 900
         CLOVER = CLIVER
C                                       Reformat?
         CALL CLREFM (CLDISK, CLCNO, CLOVER, CLCAT, CLOLUN, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL CALINI ('WRIT', CLOBUF, CLDISK, CLCNO, CLOVER, CLCAT,
     *      CLOLUN, CLORNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NTERM, GMMOD, IRET)
         IF (IRET.GT.0) GO TO 999
         CLORNO = 0
C                                       Section for new CL table
C                                       Print warning message that
C                                       attempting to append SN table
 600  WRITE (MSGTXT,1600) CLOVER
      CALL MSGWRT (4)
      WRITE (MSGTXT,1700)
      CALL MSGWRT (4)
      WRITE (MSGTXT,1800) CLIVER
      CALL MSGWRT (4)
C                                       Zero record
      LIMIT = XCLRSZ
      DO 610 I = 1,LIMIT
         CLRECR(I) = 0.0
 610     CONTINUE
      LSTSOU = -1
C
      NUMOCL = CLOBUF(5)
      FIRST=.TRUE.
      DO 700 LOOPR = 1,NUMSN
C                                       Read SN record
         CALL TABIO ('READ', IRCODE, LOOPR, SNREC, SNBUFF, IRET)
         IF (IRET.LT.0) GO TO 700
         IF (IRET.NE.0) GO TO 900
C                                       Check time
         IF ((SNRECD(TIMSN).LT.TIMRA(1)) .OR.
     *       (SNRECD(TIMSN).GT.TIMRA(2))) GO TO 700
C                                       Check Subarray
         IF (ALLSUB) SNREC(SUBSN) = 0
         IF ((SNREC(SUBSN).NE.SUBA) .AND. (SNREC(SUBSN).GT.0) .AND.
     *      (SUBA.GT.0)) GO TO 700
C                                       Test freqid
         IF ((FREQID.GT.0) .AND. (SNREC(FRQSN).GT.0) .AND.
     *       (SNREC(FRQSN).NE.FREQID)) GO TO 700
C                                       Test if calibrator wanted
         THSOU = SNREC(SOUSN)
         IF ( (THSOU.NE.LSTSOU) .AND.
     *        (.NOT. ( SLCTD (THSOU, CALWAN, KCALWD, DOCWNT) .OR.
     *                 (THSOU.LE.0) ) ) ) GO TO 700
         LSTSOU = THSOU
C                                       Antenna wanted?
         CLANT = SNREC(ANTSN)
         IF (.NOT.SLCTD (CLANT, ANTENS, NANTSL, DOAWNT)) GO TO 700
C                                       Check if existing CL table
C                                       has entries in the same time
C                                       range as SN table
         IF(FIRST) THEN
C                                       Only loop through CL table
C                                       the first time through
            FIRST=.FALSE.
            NSOU=1
            DO 640 LOOPC = 1, NUMOCL
C                                       Read CL record
               CALL TABIO ('READ', IRCODE, LOOPC, CLREC, CLOBUF, IRET)
               IF (IRET.LT.0) GO TO 640
               IF (IRET.NE.0) GO TO 900
C                                       Initialize arrays
               IF(LOOPC .EQ. 1) SOUR(1) = CLREC(SOUCL)
               IF(LOOPC .EQ. 1) SARRAY(1) = CLREC(SUBCL)
               IF(LOOPC .EQ. 1) FRQNCY(1) = CLREC(FRQCL)
               IF(LOOPC .EQ. 1) TIMBG(1) = CLRECD(TIMCL)
               IF(LOOPC .EQ. 1) TIMED(1) = CLRECD(TIMCL)
               IF(LOOPC .EQ. 1) GO TO 640
               DO 630 LOOPD=1, NSOU
C                                       test if source etc needs to
C                                       be added to array
                  IF(CLREC(SOUCL) .EQ. SOUR(LOOPD) .AND.
     *               CLREC(SUBCL) .EQ. SARRAY(LOOPD) .AND.
     *               CLREC(FRQCL) .EQ. FRQNCY(LOOPD)) GO TO 635
                  IF(LOOPD .EQ. NSOU) THEN
                     NSOU = NSOU + 1
                     SOUR(NSOU) = CLREC(SOUCL)
                     SARRAY(NSOU) = CLREC(SUBCL)
                     FRQNCY(NSOU) = CLREC(FRQCL)
                     TIMBG(NSOU) = CLRECD(TIMCL)
                     TIMED(NSOU) = CLRECD(TIMCL)
                     ENDIF
630               CONTINUE
635               TIMED(LOOPD) = CLRECD(TIMCL)
640            CONTINUE
            ENDIF
         DO 650 LOOPC = 1, NSOU
            IF(SNREC(SOUSN) .EQ. SOUR(LOOPC)   .AND.
     *         SNREC(SUBSN) .EQ. SARRAY(LOOPC) .AND.
     *         SNREC(FRQSN) .EQ. FRQNCY(LOOPC) .AND.
     *         SNRECD(TIMSN) .GE. TIMBG(LOOPC) .AND.
     *         SNRECD(TIMSN) .LE. TIMED(LOOPC)) GO TO 700
650         CONTINUE
         COUNT = COUNT + 1
C                                       Copy record to CL
         CLRECD(TIMCL) = SNRECD(TIMSN)
         CLRECR(INTCL) = SNRECR(INTSN)
         CLREC(SOUCL) = SNREC(SOUSN)
         CLREC(ANTCL) = SNREC(ANTSN)
         CLREC(SUBCL) = SNREC(SUBSN)
         CLREC(FRQCL) = SNREC(FRQSN)
         CLRECR(IFRCL) = SNREC(IFRSN)
         CLRECR(MB1CL) = SNRECR(MB1SN)
         IF (NUMPOL.GT.1) CLRECR(MB2CL) = SNRECR(MB2SN)
         CLRECR(DI1CL) = SNRECR(DI1SN)
         IF (NUMPOL.GT.1) CLRECR(DI2CL) = SNRECR(DI2SN)
C                                       Loop over IFs
         DO 670 LOOPIF = 1,NUMIF
            CLRECR(RE1CL+LOOPIF-1) = SNRECR(RE1SN+LOOPIF-1)
            CLRECR(IM1CL+LOOPIF-1) = SNRECR(IM1SN+LOOPIF-1)
            CLRECR(DE1CL+LOOPIF-1) = SNRECR(DL1SN+LOOPIF-1)
            CLRECR(RA1CL+LOOPIF-1) = SNRECR(RA1SN+LOOPIF-1)
            CLRECR(WE1CL+LOOPIF-1) = SNRECR(WT1SN+LOOPIF-1)
            CLREC(RF1CL+LOOPIF-1) = SNREC(RF1SN+LOOPIF-1)
C                                       Second polarization
            IF (NUMPOL.GT.1) THEN
               CLRECR(RE2CL+LOOPIF-1) = SNRECR(RE2SN+LOOPIF-1)
               CLRECR(IM2CL+LOOPIF-1) = SNRECR(IM2SN+LOOPIF-1)
               CLRECR(DE2CL+LOOPIF-1) = SNRECR(DL2SN+LOOPIF-1)
               CLRECR(RA2CL+LOOPIF-1) = SNRECR(RA2SN+LOOPIF-1)
               CLRECR(WE2CL+LOOPIF-1) = SNRECR(WT2SN+LOOPIF-1)
               CLREC(RF2CL+LOOPIF-1) = SNREC(RF2SN+LOOPIF-1)
               END IF
C                                       Write CL record
            CLORNO = CLORNO + 1
            CALL TABIO ('WRIT', IRCODE, CLORNO, CLREC, CLOBUF, IRET)
            IF (IRET.NE.0) GO TO 900
 670        CONTINUE
 700     CONTINUE
C                                       If appended to end unmark sort
C                                       order.
      IF (.NOT.NEWCL) THEN
         CLIBUF(43) = 0
         CLIBUF(44) = 0
         END IF
C                                       Close tables
 850  CALL TABIO ('CLOS', IRCODE, LOOPR, SNREC, SNBUFF, IRET)
      IF (IRET.NE.0) GO TO 900
      IF (.NOT.NEWCL)
     *   CALL TABIO ('CLOS', IRCODE, LOOPR, CLREC, CLIBUF, IRET)
      IF (IRET.NE.0) GO TO 900
      IF (.NOT.UPDATE)
     *   CALL TABIO ('CLOS', IRCODE, LOOPR, CLREC, CLOBUF, IRET)
      IF (IRET.NE.0) GO TO 900
C                                       Warn if no records updated
      IF (COUNT.LE.0) THEN
         MSGTXT = 'SN2CL: WARNING: NO CL RECORDS WRITTEN'
         CALL MSGWRT (8)
         END IF
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET
C                                       Close SN table to allow its
C                                       destruction.
      CALL TABIO ('CLOS', IRCODE, LOOPR, SNREC, SNBUFF, IERR)
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SN2CL: SN TABLE VERSION ',I3,' IS MISSORTED')
 1040 FORMAT ('SN2CL: Applying SN tables to CL table ',I3, ', writing',
     *   ' CL table', I3)
 1031 FORMAT ('SN2CL: CL TABLE VERSION ',I3,' IS MISSORTED')
 1600 FORMAT ('SN2CL: WARNING: Will try to append new solutions to',
     *   ' CL table ',I3)
 1700 FORMAT ('SN2CL: This is because there are no entries for',
     *   ' specified,')
 1800 FORMAT ('SN2CL: TIMERANGE, SOURCE, SUBARRAY or FREQID in',
     *   ' CL table ', I3)
 1900 FORMAT ('SN2CL: TABIO ERROR',I4,' APPLYING SN TO CL TABLE')
      END
