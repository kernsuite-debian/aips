LOCAL INCLUDE 'CLCAL.INC'
C                                       Local include for CLCAL
      INTEGER   SEQIN, CNOIN, DISKIN, JBUFSZ
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XSOUCO, XCAL(4,30),
     *   XCALCO, XOPCOD, XXINTP, XXSTYP, XXSMO
      CHARACTER NAMEIN*12, CLAIN*6, OPCODE*4, XINTP*4, XSTYPE*4, XSMO*4
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XSUBA, XANTS(50), XBAND,
     *   XFREQ, XFQID, XIPARM(10), XICUT, XCUTOF, XDOBTW, XSNVER, XSN2V,
     *   XGVER, XGUSE, XREF, XBADD(10), BUFF1(2048), XDOBLK
      LOGICAL   SINGLE
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   XSOUR, XSOUCO, XCAL, XQUAL, XCALCO, XTIME, XSUBA, XANTS, XBAND,
     *   XFREQ, XFQID, XOPCOD, XXINTP, XCUTOF, XXSTYP, XIPARM, XICUT,
     *   XDOBLK, XDOBTW, XXSMO, XSNVER, XSN2V, XGVER, XGUSE, XREF,
     *   XBADD, SEQIN, DISKIN, CNOIN, SINGLE
      COMMON /CHRCOM/ NAMEIN, CLAIN, OPCODE, XINTP, XSMO, XSTYPE
LOCAL END
LOCAL INCLUDE 'CLCAL2.INC'
C                                       Selection criteria
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IUDISK, IUCNO, NSOUWD, SOUWAN(XSTBSZ), NCALWD,
     *   CALWAN(XSTBSZ), SELQUA, NANTSL, SUBARR, ANTENS(MAXANT), SMOTYP,
     *   SNVER1, SNVER2, CLVER, CLUSE, REFA, FRQSEL, IULUN, IXLUN,
     *   ICLUN, IFLUN, NCOEFF, INTMOD
      DOUBLE PRECISION DLIMI
      REAL      TSTART, TEND, INTPRM(3,5), DOBLNK
      LOGICAL   DOAPPL, DOSWNT, DOCWNT, DOAWNT, POLYFT, ALLPAS, ALLSUB,
     *   DOBTWN
      CHARACTER SOURCS(30)*16, CALSOU(30)*16, SOUCOD*4, SELCOD*4,
     *   INTFN*4
      COMMON /SELCOM/ DLIMI, TSTART, TEND, INTPRM, IUDISK, IUCNO,
     *   NSOUWD, SOUWAN, NCALWD, CALWAN, SELQUA, NANTSL, ANTENS, SUBARR,
     *   SMOTYP, SNVER1, SNVER2, CLVER, CLUSE, REFA, FRQSEL, IULUN,
     *   IXLUN, ICLUN, IFLUN, NCOEFF, DOBLNK, DOAPPL, DOSWNT, DOCWNT,
     *   DOAWNT, POLYFT, INTMOD, ALLPAS, ALLSUB, DOBTWN
      COMMON /SELCHR/ SOURCS, CALSOU, SOUCOD, SELCOD, INTFN
LOCAL END
LOCAL INCLUDE 'CLCAL3.INC'
C                                       For fitting stuff in SNFCL
C                                       Be wary of increasing MAXFIT
C                                       without also increasing NMAX
C                                       in SVDFIT.
C
      INTEGER    MAXID, MAXIM, MAXIS, MAXPAR
      PARAMETER  (MAXID=10000)
      PARAMETER  (MAXIM=10000)
      PARAMETER  (MAXIS=20000)
      PARAMETER  (MAXPAR=10)
LOCAL END
      PROGRAM CLCAL
C-----------------------------------------------------------------------
C! Filters SN tables and/or applies them to CL tables
C# Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2002-2006, 2008-2013, 2015-2016
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
C   Either merges SN tables or merges and applies SN tables to a CL
C   table.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      SOURCES....Source list.
C      SOUCODE....Source "Cal code"
C      CALSOUR....Calibrator list.
C      QUAL.......Qualifiers for sourcs and calibrator
C      CALCODE....CALCODEs to use to select calibrators.
C      TIMERANG...Time range of the data to be calibrated.
C      SUBARRAY...Subarray number to calibrate. 0=>all.
C      ANTENNAS...Antennas selected/deselected.
C      OPCODE.....Operation: 'MERG','CALI','CALP'
C      INTERPOL...The type of interpolation to be applied.
C      INTPARM....Parameters for interpolation function.
C      SMOTYPE....Specified the data to be smoothed.
C      SNVER......Input SN table(s).
C      INVERS.....Input SN table(s).
C      GAINVER....Specified the input version of the Cal (CL) table.
C      GAINUSE....Version number of the Cal. table to use.
C      REFANT.....Reference antenna.
C      BADDISK....Disks to avoid for scratch files.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'CLCAL.INC'
      INCLUDE 'CLCAL2.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'CLCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL CLCAIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Do work
      CALL CLCAUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL CLCAHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE CLCAIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   CLCAIN gets input parameters for CLCAL and finds input file.
C   Inputs:
C      PRGN  C*6  Program name
C   Output:
C      JERR         I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in CLCAL for more details.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, SMOCOD(6)*4, UTYPE*2
      INTEGER   NPARM, IROUND, IERR, I, STYPE(6), LUN, JERR, ANVER,
     *   IPLUN, NUMCL, LUN2, TABUFF(512)
      LOGICAL   T, TABLE, EXIST, FITASC, MATCH, ISVLBA
      DOUBLE PRECISION DXFREQ
      INCLUDE 'CLCAL.INC'
      INCLUDE 'CLCAL2.INC'
      INCLUDE 'CLCAL3.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA SMOCOD /'AMPL','PHAS','BOTH','FULL','VLBI','    '/
      DATA STYPE /1,2,3,4,5,4/
      DATA T /.TRUE./
      DATA LUN, LUN2 /29, 28/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2048 * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 343
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
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      REFA = IROUND (XREF)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XXINTP, XINTP)
      CALL H2CHR (4, 1, XXSTYP, XSTYPE)
      CALL H2CHR (4, 1, XXSMO, XSMO)
C                                       Get CATBLK.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       BADDISK
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Put selection criteria into
C                                       correct common.
      IUDISK = DISKIN
      IUCNO = CNOIN
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (4, 1, XCALCO, SELCOD)
      CALL H2CHR (4, 1, XSOUCO, SOUCOD)
      DO 80 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
         CALL H2CHR (16, 1, XCAL(1,I), CALSOU(I))
 80      CONTINUE
C                                       Time range
      TSTART = XTIME(1) + (XTIME(2) / 24.0) + (XTIME(3) / (24.0*60.0)) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      TEND = XTIME(5) + (XTIME(6) / 24.0) + (XTIME(7) / (24.0*60.0)) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF (TSTART.LT.1.0E-20) TSTART = -1.0E20
      IF (TEND.LT.1.0E-20) TEND = 1.0E20
C                                       Set interpolation type
      DOBTWN = XDOBTW.GT.0.0
      DOBLNK = XDOBLK
      INTMOD = 0
      IF (XINTP.EQ.'2PT ') INTMOD=0
      IF (XINTP.EQ.'SELF') INTMOD=1
      IF (XINTP.EQ.'SELN') INTMOD=2
      IF (XINTP.EQ.'SIMP') INTMOD=3
      IF (XINTP.EQ.'AMBG') INTMOD=4
      IF (XINTP.EQ.'CUBE') INTMOD=5
      IF (XINTP.EQ.'POLY') INTMOD=6
      IF (INTMOD.EQ.0) XINTP = '2PT'
      WRITE (MSGTXT,1110) XINTP
      CALL MSGWRT (4)
C                                       trap 'POLY'
C                                       for special handling
      POLYFT = XINTP.EQ.'POLY'
      INTFN = XSTYPE
      IF ((INTFN.NE.'BOX') .AND. (INTFN.NE.'MWF') .AND.
     *   (INTFN.NE.'GAUS') .AND. (INTFN.NE.'EXP') .AND.
     *   (INTFN.NE.'LINE') .AND. (INTFN.NE.'2PT') .AND.
     *   (INTFN.NE.'2PTH') .AND. (INTFN.NE.' ')) THEN
         MSGTXT = 'SAMPTYPE = ''' // INTFN // ''' UNRECOGNIZED'
         CALL MSGWRT (6)
         INTFN = ' '
         END IF
      XICUT = MAX (XICUT, 1.E-6)
      CALL RFILL (15, XICUT, INTPRM(1,1))
      INTPRM(1,1) = XIPARM(1) / 24.0
      INTPRM(1,2) = XIPARM(2) / 24.0
      INTPRM(1,3) = XIPARM(3) / 24.0
      INTPRM(1,4) = XIPARM(4) / 24.0
      INTPRM(1,5) = XIPARM(5) / 24.0
      INTPRM(2,1) = XIPARM(6) / 24.0
      INTPRM(2,2) = XIPARM(7) / 24.0
      INTPRM(2,3) = XIPARM(8) / 24.0
      INTPRM(2,4) = XIPARM(9) / 24.0
      INTPRM(2,5) = XIPARM(10) / 24.0
      IF (POLYFT) THEN
         NCOEFF = XCUTOF + 0.01
         IF (NCOEFF.GT.MAXPAR) THEN
            WRITE (MSGTXT,1085) NCOEFF, MAXPAR
            CALL MSGWRT (6)
            NCOEFF = MAXPAR
            END IF
         END IF
C                                       Smoothing type
C                                       If blanked values to be unblanked
C                                       then must replace all "full" set
C                                       of solutions.
      IF (XSMO.EQ.' ') XSMO = 'FULL'
      IF ((XDOBLK.GE.0.0) .AND. (INTFN.NE.' ') .AND. (XSMO.NE.'FULL'))
     *   THEN
         XSMO = 'FULL'
         WRITE (MSGTXT,1020) XDOBLK
         CALL MSGWRT (6)
         END IF
      SMOTYP = 0
      DO 90 I = 1,6
         IF (XSMO.EQ.SMOCOD(I)) SMOTYP = STYPE(I)
 90      CONTINUE
      IF (SMOTYP.LE.0) THEN
         JERR = 5
         WRITE (MSGTXT,1090) XSMO
         GO TO 990
         END IF
      IF ((OPCODE.NE.'CALI') .AND. (OPCODE.NE.'CALP') .AND.
     *   (OPCODE.NE.'MERG')) OPCODE = 'CALP'
      ALLPAS = OPCODE.EQ.'CALP'
      IF (ALLPAS) OPCODE = 'CALI'
      DOAPPL = OPCODE.EQ.'CALI'
      DO 100 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 100     CONTINUE
      SUBARR = IROUND (XSUBA)
      ALLSUB = SUBARR.EQ.-32000
      IF (ALLSUB) CALL ANSAME (IUDISK, IUCNO, CATBLK, ALLSUB)
      SUBARR = MAX (0, SUBARR)
C                                       Freq id
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      DXFREQ = XFREQ
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN, XBAND, DXFREQ,
     *   MATCH, FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1070)
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       CUTOFF: specifies maximum
C                                       allowed interpolation
C                                       interval.
      DLIMI = MAX (0.0, XCUTOF) / 1440.0D0
C                                       Table version numbers
      SNVER1 = IROUND (XSNVER)
      SNVER2 = IROUND (XSN2V)
      CLVER = IROUND (XGVER)
      CLUSE = IROUND (XGUSE)
C                                       See if single or multi source
      CALL MULSDB (CATBLK, SINGLE)
      IF (SINGLE) THEN
         CALL ISTAB ('SU', IUDISK, IUCNO, 1, LUN, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         SINGLE = (IERR.EQ.0) .AND. EXIST .AND. TABLE
         END IF
      SINGLE = .NOT.SINGLE
C                                       Check OPCODE
      IF ((OPCODE.NE.'MERG') .AND. (OPCODE.NE.'CALI')) THEN
         JERR = 5
         MSGTXT = 'UNKNOWN OPCODE = ''' // OPCODE // ''''
         GO TO 990
         END IF
      IF ((OPCODE.EQ.'CALI') .AND. (SINGLE)) THEN
         JERR = 5
         MSGTXT = 'OPCODE ''CALI'' NOT APPROPRIATE FOR SINGLE-SOURCE'
     *      // ' FILES'
         GO TO 990
         END IF
C                                       Defaults
      IF (.NOT.SINGLE) THEN
         CALL FNDEXT ('CL', CATBLK, NUMCL)
         IF (CLVER.LE.0) CLVER = NUMCL
         IF (((CLUSE.LE.0) .AND. (OPCODE.NE.'MERG')) .OR.
     *      ((ALLPAS) .AND. (CLUSE.GT.NUMCL))) THEN
            CLUSE = NUMCL + 1
            CALL TABCOP ('CL', CLVER, CLUSE, LUN, LUN2, DISKIN, DISKIN,
     *         CNOIN, CNOIN, CATBLK, BUFF1, TABUFF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) IERR
               GO TO 990
               END IF
            END IF
         IF ((.NOT.ALLPAS) .AND. (CLUSE.GT.NUMCL) .AND.
     *      (OPCODE.EQ.'CALI')) THEN
            MSGTXT = '*** WARNING: THE NEW CL TABLE WILL NOT CONTAIN'
     *         // ' ALL OF THE DATA'
            CALL MSGWRT (7)
            MSGTXT = '*** IF ANY OF SOURCES, SOUCODE, TIMERANG,'
     *         // ' ANTENNAS ARE USED'
            CALL MSGWRT (7)
            MSGTXT = '*** APPLICATION OF SAID CL TABLE TO OMITTED' //
     *         ' DATA CAUSES FLAGGING'
            CALL MSGWRT (7)
            END IF
C                                       Prohibit CLUSE=1
         IF (CLUSE.EQ.1) THEN
            JERR = 5
            MSGTXT = 'ERROR: IT IS FORBIDDEN TO MODIFY CL VERSION 1'
            GO TO 990
            END IF
         END IF
      JERR = 0
C                                       Is this VLBA data?
      ANVER = IROUND (XSUBA)
      IF (ANVER.LE.0) ANVER = 1
      IPLUN = 40
      CALL ANTINI ('READ', BUFF1, DISKIN, CNOIN, ANVER, CATBLK, IPLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1160) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      ISVLBA = ANAME(1:3).EQ.'VLB'
      IF (ISVLBA .AND. (INTFN.NE.' ')) THEN
         MSGTXT = 'This appears to be VLBI data and you are smoothing'
         CALL MSGWRT (4)
         MSGTXT = 'We recommend using SNSMO to smooth before CLCAL'
         CALL MSGWRT (4)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLCAIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('WARNING: DOBLANK=',F4.0,' SMOTYPE SET TO FULL')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1070 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1085 FORMAT ('TOO MANY POLY COEFFS:',I5,'.  REDUCED TO ',I5)
 1090 FORMAT ('UNKNOWN SMOOTHING TYPE = ',A4)
 1100 FORMAT ('ERROR: COPYING INPUT CL TO OUTPUT:',I4)
 1110 FORMAT ('Using interpolation mode ',A4)
 1160 FORMAT ('CLCAIN: ERROR ',I3,' READING AN TABLE')
      END
      SUBROUTINE CLCAUV (IRET)
C-----------------------------------------------------------------------
C   CLCAUV merges SN tables or merge the SN tables and applies them to
C   a CL table.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   QUALS(30), MAXSOU
      LOGICAL   TSMOTH
      DOUBLE PRECISION TIMRA(2)
      INCLUDE 'CLCAL.INC'
      INCLUDE 'CLCAL2.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Set LUNs
      IULUN = 25
      IXLUN = 28
      ICLUN = 29
      IFLUN = 30
C                                       See if smoothing requested
      TSMOTH = (INTFN.NE.' ')
C                                       Source name
      CALL FILL (30, SELQUA, QUALS)
      MAXSOU = XSTBSZ
      CALL SOUFND (IUDISK, IUCNO, CATBLK, MAXSOU, SOURCS, QUALS, SOUCOD,
     *   NSOUWD, DOSWNT, SOUWAN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Calibrator names
      CALL SOUFND (IUDISK, IUCNO, CATBLK, MAXSOU, CALSOU, QUALS, SELCOD,
     *   NCALWD, DOCWNT, CALWAN, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Antenna list
      CALL ANTFND (ANTENS, NANTSL, DOAWNT)
C                                       Purge output CL table.
      IF (DOAPPL .AND. (.NOT.SINGLE)) THEN
         TIMRA(1) = TSTART
         TIMRA(2) = TEND
         CALL CALSEL (IUDISK, IUCNO, 'CL', CLUSE, CATBLK, BUFF1,
     *      NSOUWD, SOUWAN, DOSWNT, NANTSL, ANTENS, DOAWNT, TIMRA,
     *      SUBARR, FRQSEL, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
C                                       Process table(s).
      CALL CLUPDA (SINGLE, TSMOTH, IRET)
C
 999  RETURN
      END
      SUBROUTINE CLCAHI
C-----------------------------------------------------------------------
C   CLCAHI updates history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   IERR, I, J, LUN, TIME(3), DATE(3), LIMIT, LIMIT2, I1, I2
      LOGICAL   T
      INCLUDE 'CLCAL.INC'
      INCLUDE 'CLCAL2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN, DISKIN, CNOIN, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       Task message
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2)(1:8), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPCODE
      IF (ALLPAS) OPCODE = 'CALP'
      WRITE (HILINE,2000) TSKNAM, OPCODE
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Check if updating CL
      IF (OPCODE(:3).EQ.'CAL') THEN
C                                       Source list
         IF (NSOUWD.LE.0) THEN
            WRITE (HILINE,3000) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
         ELSE
C                                       Included or excluded?
            WRITE (HILINE,3001) TSKNAM
            IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
            I1 = 1
            IF (SOURCS(1)(1:1).EQ.'-') I1 = 2
            I2 = 1
            IF (SOURCS(2)(1:1).EQ.'-') I2 = 2
            WRITE (HILINE,3003) TSKNAM, SOURCS(1)(I1:), SOURCS(2)(I2:)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       Rest of sources
            LIMIT = MIN (30, NSOUWD)
            DO 20 I = 3,LIMIT,2
               IF ((SOURCS(I).NE.' ') .OR. (SOURCS(I+1).NE.' ')) THEN
                  I1 = 1
                  IF (SOURCS(I)(1:1).EQ.'-') I1 = 2
                  I2 = 1
                  IF (SOURCS(I+1)(1:1).EQ.'-') I2 = 2
                  WRITE (HILINE,3004) TSKNAM, SOURCS(I)(I1:),
     *               SOURCS(I+1)(I2:)
                  CALL HIADD (LUN, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 20            CONTINUE
            END IF
C                                       Source "Cal code"
         WRITE (HILINE,2001) TSKNAM, SOUCOD
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Calibrators
         IF (NCALWD.LE.0) THEN
            WRITE (HILINE,3005) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
C                                       Included or excluded?
         ELSE
            WRITE (HILINE,3006) TSKNAM
            IF (DOCWNT) WRITE (HILINE,3007) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
            I1 = 1
            IF (CALSOU(1)(1:1).EQ.'-') I1 = 2
            I2 = 1
            IF (CALSOU(2)(1:1).EQ.'-') I2 = 2
            WRITE (HILINE,3008) TSKNAM, CALSOU(1)(I1:), CALSOU(2)(I2:)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       Rest of calibrators
            LIMIT = MIN (30, NCALWD)
            DO 30 I = 3,LIMIT,2
               IF ((CALSOU(I).NE.' ') .OR. (CALSOU(I+1).NE.' ')) THEN
                  I1 = 1
                  IF (CALSOU(I)(1:1).EQ.'-') I1 = 2
                  I2 = 1
                  IF (CALSOU(I+1)(1:1).EQ.'-') I2 = 2
                  WRITE (HILINE,3009) TSKNAM, CALSOU(I)(I1:),
     *               CALSOU(I+1)(I2:)
                  CALL HIADD (LUN, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 30            CONTINUE
            END IF
C                                       QUAL, CALCODE
         WRITE (HILINE,3010) TSKNAM, SELQUA, SELCOD
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       TIMERANG
         CALL HITIME (TSTART, TEND, LUN, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Subarray
         WRITE (HILINE,2003) TSKNAM, SUBARR
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Antennas
         IF (NANTSL.LE.0) THEN
            WRITE (HILINE,3011) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       Included or excluded?
         ELSE
            WRITE (HILINE,3012) TSKNAM
            IF (DOAWNT) WRITE (HILINE,3013) TSKNAM
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       1st 12 and label.
            LIMIT = MIN (12, NANTSL)
            WRITE (HILINE,3014) TSKNAM, (ANTENS(J),J=1,LIMIT)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            IF (NANTSL.GT.12) THEN
C                                       Rest of antennas
               DO 50 I = 13,NANTSL,12
                  LIMIT = I
                  LIMIT2 = I + 11
                  LIMIT2 = MIN (NANTSL, LIMIT2)
                  WRITE (HILINE,3015) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
                  CALL HIADD (LUN, HILINE, BUFF1, IERR)
                  IF (IERR.NE.0) GO TO 200
 50               CONTINUE
               END IF
            END IF
C                                       Calibration Tables
         WRITE (HILINE,2005) TSKNAM, CLVER, CLUSE
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Interpolation fn
         IF (XINTP.NE.' ') THEN
            WRITE (HILINE,2006) TSKNAM, XINTP
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
C                                       Interpolation limit
            WRITE (HILINE,2015) TSKNAM, XCUTOF
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
         END IF
C                                       input tables
      WRITE (HILINE,2004) TSKNAM, SNVER1, SNVER2
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Smoothing
      IF (INTFN.NE.' ') THEN
C                                       Smnoothing function
         WRITE (HILINE,2007) TSKNAM, INTFN
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       smoothing type
         WRITE (HILINE,2008) TSKNAM, XSMO
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       What's replaced?
         IF (XDOBLK.GT.0.0) THEN
            HILINE = TSKNAM // '/ Only blanked values replaced' //
     *         ' by smoothed'
         ELSE IF (XDOBLK.EQ.0.0) THEN
            HILINE = TSKNAM // '/ All values replaced by smoothed'
         ELSE
            HILINE = TSKNAM // '/ Only good values replaced by' //
     *         ' smoothed'
            END IF
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Sources merged
         IF (XDOBTW.GT.0.0) THEN
            HILINE = TSKNAM // '/ All sources smoothed together'
         ELSE
            HILINE = TSKNAM // '/ Sources smoothed separately'
            END IF
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Interpolation parms
         WRITE (HILINE,2009) TSKNAM, (XIPARM(I), I = 1,4)
         CALL HIADD (LUN, HILINE, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       FWHM
         IF ((INTFN.EQ.'GAUS') .OR. (INTFN.EQ.'EXP') .OR.
     *      (INTFN.EQ.'LINE')) THEN
            WRITE (HILINE,2010) TSKNAM, (XIPARM(I), I = 6,9)
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            WRITE (HILINE,2011) TSKNAM, XICUT
            CALL HIADD (LUN, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
         END IF
C                                       Reference antenna
      WRITE (HILINE,2014) TSKNAM, REFA
      CALL HIADD (LUN, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Close HI file
 200   CALL HICLOS (LUN, T, BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CLCAHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 2000 FORMAT (A6,'OPCODE =''',A4,''' / MERG, CALI, or CALP')
 2001 FORMAT (A6,'SOUCODE =''',A,''' / Source Cal code')
 2003 FORMAT (A6,'SUBARRAY =',I4)
 2004 FORMAT (A6,'SNVER = ',2I4, ' / SN table versions used')
 2005 FORMAT (A6,'GAINVER =',I3,', GAINUSE =',I3,' / CL table')
 2006 FORMAT (A6,'INTERPOL = ''',A4,''' / Interpolation type')
 2007 FORMAT (A6,'SAMPTYPE = ''',A4,''' / Smoothing function')
 2008 FORMAT (A6,'SMOTYPE = ''',A4,''' / Data to be smoothed')
 2009 FORMAT (A6,'SMOSUPP = ',4F7.3,' / smooth support')
 2010 FORMAT (A6,'SMOFWHM = ',4F7.3,' / smooth FWHM')
 2011 FORMAT (A6,'SMOCUT = ',F10.6,' / smooth weight cutoff')
 2014 FORMAT (A6,'REFANT = ',I4,' / Reference antenna used')
 2015 FORMAT (A6,'CUTOFF = ',F4.1,' minutes / Interpolation limit')
 3000 FORMAT (A6,'SOURCES = ''''     / All sources selected')
 3001 FORMAT (A6,' / Sources excluded:')
 3002 FORMAT (A6,' / Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'CALSOUR = ''''     / All calibrators selected')
 3006 FORMAT (A6,' / Calibrators excluded:')
 3007 FORMAT (A6,' / Calibrators included:')
 3008 FORMAT (A6,'CALSOUR = ''',A16,''',''',A16,'''')
 3009 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3010 FORMAT (A6,'QUAL = ',I4,' CALCODE = ''',A4,'''')
 3011 FORMAT (A6,'ANTENNAS = 0     / All antennas selected')
 3012 FORMAT (A6,' / Antennas excluded:')
 3013 FORMAT (A6,' / Antennas included:')
 3014 FORMAT (A6,'ANTENNAS = ',12(I3,' '))
 3015 FORMAT (A6,'           ',12(I3,' '))
      END
      SUBROUTINE CLUPDA (SINGLE, SNSMTH, IERR)
C-----------------------------------------------------------------------
C   Concatenates selected SN tables and rereferences to the same
C   reference antenna.  Then if SNSMTH is true the SN table will be
C   smoothed.
C   If DOAPPL (in common) is true then the SN table is applied to the
C   specified CL table.
C   Leaves the output table sorted in time-antenna order.
C    Inputs:
C       SINGLE    L    If true then the uv data is a single source file
C                      and only SN tables will be processed.
C       SNSMTH    L    If true then smooth SN tables.
C    Inputs from common.
C      IUDISK       I    Disk number
C      IUCNO        I    Catalog block number
C      CATBLK       I(256) Catalog header
C      NSOUWD       I    Number of sources included or excluded; if
C                        0 all sources are included.
C      DOSWNT       L    If .TRUE. then sources in SOUWAN are included
C                        If .FALSE. then excluded.
C      SOUWAN       I(30)The source numbers of sources included or
C                        excluded.
C      NCALWD       I    Number of calibrators included or excluded.
C      DOCWNT       L    If .TRUE. then calibrators in CALWAN are
C                        included, if .FALSE. then excluded.
C      CALWAN       I(30)The source numbers of calibrators included or
C                        excluded.
C      NANTSL       I    Number of antennas selected/excluded in ANTENS
C                        0 = All included.
C      DOAWNT       L    If .TRUE. then antennas in ANTENS included.
C                        If .FALSE. then excluded.
C      DOAPPL       L    If true then apply SN tables to the CL table.
C      REFA         I    The desired reference antenna, 0=most used.
C      SNVER        I    Desired SN table to be applied, 0=>all.
C      CLVER        I    Input Cal (CL) file version number.
C                        For single source files the input SN table.
C      CLUSE        I    Cal file version number to put smoothed gains
C                        into and use for calibration.
C                        For single source files the output SN table.
C      TSTART       R    First time to process (days) (no default)
C      TEND         R    Last time to process (days) (no default)
C    Output:
C      IERR         I    Return code, 0=>OK, otherwise failed.
C   Useage note: Sorts the relevant tables.
C-----------------------------------------------------------------------
      LOGICAL   SINGLE, SNSMTH
      INTEGER   IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER MXTIME
      PARAMETER (MXTIME = 30000)
      CHARACTER  COLHED(2)*24, KEYW*8
      INTEGER   IRET, JERR, J, KEY(2,2), NUMSN, CLANT, CLTIM, SNANT,
     *   SNTIM, NKEY, KOLS(2), NUMNOD, SNUP, LOCS, KEYTYP, IVER,
     *   MAXTIM, SNSEL, LIMSN1, LIMSN2, BUFFER(512), OBUFF(512),
     *   ICLRNO, CLKOLS(MAXCLC), CLNUMV(MAXCLC), NUMANT, NUMPOL, NUMIF,
     *   MKEY, MREC, MCOL, I, WORKS(MXTIME), KEYSUB(2,2), IBIG(512)
      LOGICAL   T, EXIST, TABLE, FITASC, ISAPPL, NOSN, LTEST, REREF
      REAL      FKEY(2,2), WRKTIM(MXTIME), WORK1(MXTIME), WORK2(MXTIME),
     *   WORK3(MXTIME), WORK4(MXTIME), WORK5(MXTIME), WORK6(MXTIME),
     *   WORK7(MXTIME), WORK8(MXTIME), SMOTIM(3), GMMOD
      DOUBLE PRECISION RANOD(25), DECNOD(25), TIMRA(2)
      REAL      BIG(9*MXTIME)
      INTEGER   REFUSE(MAXANT), NTERM, REFUSS(MAXANT,100)
      EQUIVALENCE (IBIG, BIG)
C                                       Variables for reading keywords
      INTEGER MAXKEY
      PARAMETER (MAXKEY = 50)
      CHARACTER KEYSN(MAXKEY)*8
      INTEGER   JKLOCS(MAXKEY), KSNVAL(MAXKEY), KSNTYP(MAXKEY), KPOINT,
     *   ORIGIN, SNKEY
C
      INCLUDE 'CLCAL2.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (WORK1, BIG(1)), (WORK2, BIG(MXTIME+1)),
     *   (WORK3, BIG(2*MXTIME+1)), (WORK4, BIG(3*MXTIME+1)),
     *   (WORK5, BIG(4*MXTIME+1)), (WORK6, BIG(5*MXTIME+1)),
     *   (WORK7, BIG(6*MXTIME+1)), (WORK8, BIG(7*MXTIME+1)),
     *   (WORKS, BIG(8*MXTIME+1))
      DATA FKEY /1.0,0.0,1.0,0.0/
      DATA KEYSUB /4*1/
      DATA COLHED /'TIME', 'ANTENNA NO.'/
      DATA T /.TRUE./
      DATA KEYW /'APPLIED '/
C-----------------------------------------------------------------------
C                                       MAXTIM = size of work arrays;
      MAXTIM = MXTIME
      IERR = 0
      NOSN = T
      IF (INTFN.NE.' ') THEN
         SMOTIM(1) = INTPRM(1,2)
         SMOTIM(2) = INTPRM(1,4)
         SMOTIM(3) = INTPRM(1,3)
      ELSE
         SMOTIM(1) = 0.0
         SMOTIM(2) = 0.0
         SMOTIM(3) = 0.0
         END IF
      TIMRA(1) = TSTART
      TIMRA(2) = TEND
      IF (DOBTWN) THEN
         CALL FILL (MAXTIM, -1, WORKS)
      ELSE
         CALL FILL (MAXTIM, 0, WORKS)
         END IF
C                                       Zero Ref. antenna array
      J = MAXANT
      CALL FILL (J, 0, REFUSE)
      J = 100 * MAXANT
      CALL FILL (J, 0, REFUSS)
C                                       See if any SN tables
      CALL FNDEXT ('SN', CATBLK, NUMSN)
C                                       No SN tables!
      IF (NUMSN.LT.1) THEN
         IERR = 10
         WRITE (MSGTXT,1050)
         GO TO 990
         END IF
C                                       Temporary table
      SNUP = NUMSN + 1
      IF ((SNVER1.LE.0) .AND. (SNVER2.LE.0)) THEN
         SNVER1 = 1
         SNVER2 = NUMSN
      ELSE IF (SNVER1.LE.0) THEN
         SNVER1 = 1
         SNVER2 = MIN (SNVER2, NUMSN)
      ELSE
         SNVER1 = MIN (SNVER1, NUMSN)
         SNVER2 = MAX (SNVER1, MIN (SNVER2, NUMSN))
         END IF
      LIMSN1 = SNVER1
      LIMSN2 = SNVER2
      ORIGIN = 0
C                                       Message about operation
      IF (SNVER2.EQ.SNVER1) THEN
         WRITE (MSGTXT,1070) SNVER1
      ELSE
         WRITE (MSGTXT,1071) SNVER1, SNVER2
         END IF
      CALL MSGWRT (3)
C                                       If SN table originated from a
C                                       single-source file and the user
C                                       has only specified that one SN
C                                       table be used then we should set
C                                       ORIGIN to override the use of
C                                       CALSOU sown in SN2CL.
      IF (LIMSN2.EQ.LIMSN1) THEN
         CALL SNINI ('READ', BUFFER, IUDISK, IUCNO, LIMSN1, CATBLK,
     *      ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1020) IERR
            GO TO 990
            END IF
C                                       Check to see if ORIGIN
C                                       keyword is present.
         ORIGIN = 0
         SNKEY = MAXKEY
         CALL TABKEY ('ALL ', KEYSN, SNKEY, BUFFER, JKLOCS, KSNVAL,
     *      KSNTYP, IERR)
         IF (IERR.EQ.0) THEN
            DO 30 I = 1,SNKEY
               KPOINT = JKLOCS(I)
               IF (KPOINT.LE.0) GO TO 30
               IF (KEYSN(I).EQ.'ORIGIN') ORIGIN = KSNVAL(KPOINT)
               IF (KEYSN(I).EQ.'SNORIGIN') ORIGIN = KSNVAL(KPOINT)
   30          CONTINUE
         ELSE
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR
            GO TO 990
            END IF
         END IF
C
      REREF = .FALSE.
      DO 400 J = LIMSN1,LIMSN2
C                                       Concatenate backwords to get
C                                       correct first entries in SN2CL
C                                       in the case of redundant
C                                       solutions.
         IVER = LIMSN2 - J + LIMSN1
C                                       SN table:
C                                       Check if exists
         CALL ISTAB ('SN', IUDISK, IUCNO, IVER, ICLUN, BUFFER, TABLE,
     *      EXIST, FITASC, JERR)
         IF ((JERR.EQ.0) .AND. ((EXIST.AND.TABLE))) THEN
            SNSEL = IVER
C                                       Append.
            CALL SNAPP (IUDISK, IUCNO, SNSEL, SNUP, CATBLK, BUFFER,
     *         OBUFF, REFUSE, REFUSS, REREF, IERR)
            IF (IERR.GT.0) GO TO 999
C                                       IERR = -1 if empty
            IF (IERR.EQ.0) NOSN = .FALSE.
C                                       Mark SN table APPLIED.
            IF ((DOAPPL) .AND. (IERR.EQ.0)) THEN
               MKEY = 0
               MREC = 10
               MCOL = 0
               CALL TABINI ('WRIT', 'SN', IUDISK, IUCNO, SNSEL, CATBLK,
     *            ICLUN, MKEY, MREC, MCOL, OBUFF, BUFFER, IRET)
               IF (IRET.NE.0) GO TO 999
C                                       Check old value
               NKEY = 1
               LOCS = 1
               KEYTYP = 5
               CALL TABKEY ('READ', KEYW, NKEY, BUFFER, LOCS, LTEST,
     *            KEYTYP, JERR)
C                                       SN table already applied
               IF ((JERR.EQ.0) .AND. LTEST) THEN
                  WRITE (MSGTXT,1410) SNSEL
                  CALL MSGWRT (6)
                  END IF
C                                       Rewrite
               CALL TABKEY ('WRIT', KEYW, NKEY, BUFFER, LOCS, T, KEYTYP,
     *            JERR)
               CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
               IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
               END IF
            END IF
 400     CONTINUE
      IF (NOSN) THEN
         MSGTXT = 'NO VALID SN ENTRIES FOUND'
         CALL MSGWRT (8)
         IERR = 10
         GO TO 999
         END IF
C                                       Reference phases
      IF (REREF) THEN
         CALL CALADJ (IUDISK, IUCNO, CATBLK, SUBARR, SNUP, REFA, REFUSE,
     *      REFUSS, SMOTIM, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
         MSGTXT = 'Amplitude only table does not need re-referencing'
         CALL MSGWRT (3)
         END IF
C                                       Sort SN table to antenna-time.
      KEY(2,1) = 0
      KEY(2,2) = 0
C                                       Need col. pointers, sort order.
C                                       Reformat?
      CALL SNREFM (IUDISK, IUCNO, SNUP, CATBLK, ICLUN, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL SNINI ('READ', BUFFER, IUDISK, IUCNO, SNUP, CATBLK, ICLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
C                                       Reopen write
      CALL SNINI ('WRIT', BUFFER, IUDISK, IUCNO, SNUP, CATBLK, ICLUN,
     *   ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   GMMOD, RANOD, DECNOD, ISAPPL, IRET)
C                                       Get column pointers
      IF (IRET.EQ.0) THEN
         NKEY = 2
         CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
         IF ((IRET.LT.1) .OR. (IRET.GT.10)) THEN
            IRET = 0
            SNTIM = KOLS(1)
            SNANT = KOLS(2)
            END IF
         END IF
C                                       Close table
      CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
      IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
      KEY(1,1) = SNANT
      KEY(1,2) = SNTIM
C                                       Sort if necessary
      IF ((BUFFER(43).NE.SNANT) .OR. (BUFFER(44).NE.SNTIM)) THEN
         CALL TABSRT (IUDISK, IUCNO, 'SN', SNUP, SNUP, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Merge equal times
      CALL SNMRG (IUDISK, IUCNO, SNUP, CATBLK, BUFFER, OBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Smooth if requested
      IF (SNSMTH) THEN
         CALL SNSMO (SNSMTH, INTFN, IUDISK, IUCNO, SNUP, REFA, CATBLK,
     *      BUFFER, INTPRM, SMOTYP, DOBLNK, TIMRA, CALWAN, NCALWD,
     *      DOCWNT, FRQSEL, SNUP, ALLSUB, OBUFF, MAXTIM, WRKTIM, WORK1,
     *      WORK2, WORK3, WORK4, WORK5, WORK6, WORK7, WORK8, WORKS,
     *      IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       May have to resort CL table
      IF ((DOAPPL) .AND. (.NOT.NOSN)) THEN
C                                       Sort to antenna-time order
C                                       Open table
         CALL CLREFM (IUDISK, IUCNO, CLVER, CATBLK, ICLUN, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL CALINI ('READ', BUFFER, IUDISK, IUCNO, CLVER, CATBLK,
     *      ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM,
     *      GMMOD, IRET)
C                                       If problem just call SN2CL and
C                                       let it sort things out.
         IF (IRET.EQ.0) THEN
C                                       Get column pointers
            NKEY = 2
            CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
            IF ((IRET.LT.1) .OR. (IRET.GT.10)) THEN
               IRET = 0
               CLTIM = KOLS(1)
               CLANT = KOLS(2)
               END IF
C                                       Close table
            CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
            IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
            KEY(1,1) = CLANT
            KEY(1,2) = CLTIM
C                                       Sort CLVER
            IF ((BUFFER(43).NE.CLANT) .OR. (BUFFER(44).NE.CLTIM)) THEN
               CALL TABSRT (IUDISK, IUCNO, 'CL', CLVER, CLVER, KEY,
     *            KEYSUB, FKEY, BUFFER, CATBLK, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
C                                       Apply combined SN table to CL
         IF (.NOT.POLYFT) THEN
            CALL SN2CL (IUDISK, IUDISK, IUCNO, IUCNO, SNUP, CLVER,
     *         CLUSE, CATBLK, CATBLK, NSOUWD, SOUWAN, DOSWNT, NCALWD,
     *         CALWAN, DOCWNT, TIMRA, SUBARR, FRQSEL, NANTSL, DOAWNT,
     *         ANTENS, INTMOD, ORIGIN, ALLPAS, ALLSUB, BIG, BUFFER,
     *         OBUFF, DLIMI, IERR)
C                                       Use SNFCL if doing polynomial
C                                       fit.
         ELSE
            CALL SNFCL (IUDISK, IUDISK, IUCNO, IUCNO, SNUP, CLVER,
     *         CLUSE, CATBLK, CATBLK, NSOUWD, SOUWAN, DOSWNT, NCALWD,
     *         CALWAN, DOCWNT, TIMRA, SUBARR, FRQSEL, NANTSL, DOAWNT,
     *         ANTENS, ORIGIN, ALLSUB, IBIG, BUFFER, OBUFF, NCOEFF,
     *         IERR)
            END IF
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Remove temporary SN table if it
C                                       was applied to a CL table.
      IF (DOAPPL .AND. (.NOT.SINGLE)) THEN
         CALL RMEXT (IUDISK, IUCNO, 'SN', SNUP, CATBLK, BUFFER, JERR)
         IF (IERR.NE.0) GO TO 999
C                                       Resort CL to time-antenna
C                                       Open table
         CALL CALINI ('READ', BUFFER, IUDISK, IUCNO, CLUSE, CATBLK,
     *      ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF, NTERM,
     *      GMMOD, IRET)
C                                       Get column pointers
         IF (IRET.EQ.0) THEN
            NKEY = 2
            CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
            IF ((IRET.LT.1) .OR. (IRET.GT.10)) THEN
               IRET = 0
               CLTIM = KOLS(1)
               CLANT = KOLS(2)
               END IF
            END IF
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
         IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
         KEY(1,1) = CLTIM
         KEY(1,2) = CLANT
C                                       Sort
         IF ((BUFFER(43).NE.CLTIM) .OR. (BUFFER(44).NE.CLANT))
     *      CALL TABSRT (IUDISK, IUCNO, 'CL', CLUSE, CLUSE, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IERR)
C                                       Single source file:
C                                       Resort SN to time-antenna
C                                       Open table
      ELSE IF (SINGLE) THEN
         CALL SNINI ('READ', BUFFER, IUDISK, IUCNO, SNUP, CATBLK,
     *      ICLUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL, NUMIF,
     *      NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, IRET)
C                                       Get column pointers
         IF (IRET.EQ.0) THEN
            NKEY = 2
            CALL FNDCOL (NKEY, COLHED, 24, T, BUFFER, KOLS, IRET)
            IF ((IRET.LT.1) .OR. (IRET.GT.10)) THEN
               IRET = 0
               SNTIM = KOLS(1)
               SNANT = KOLS(2)
               END IF
            END IF
C                                       Close table
         CALL TABIO ('CLOS', 0, ICLRNO, BUFFER, BUFFER, IERR)
         IF ((IERR.NE.0) .OR. (IRET.NE.0)) GO TO 995
         KEY(1,1) = SNTIM
         KEY(1,2) = SNANT
C                                       Sort
         IF ((BUFFER(43).NE.SNTIM) .OR. (BUFFER(44).NE. SNANT))
     *      CALL TABSRT (IUDISK, IUCNO, 'SN', SNUP, SNUP, KEY, KEYSUB,
     *      FKEY, BUFFER, CATBLK, IERR)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
 995  IF ((IERR.EQ.0) .AND. (IRET.NE.0)) IERR = IRET
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CLUPDA: ERROR ',I3,' READING KEYWORDS FROM SN TABLE')
 1020 FORMAT ('CLUPDA: ERROR ',I3,' OPENING SN TABLE')
 1030 FORMAT ('CLUPDA: ERROR ',I3,' CLOSING SN TABLE')
 1050 FORMAT ('CLUPDA: NO SN TABLES FOUND')
 1070 FORMAT ('Processing SN table ',I4)
 1071 FORMAT ('Processing SN tables',I4,' through',I4)
 1410 FORMAT ('WARNING: SN table ',I4,' has already been applied')
      END
      SUBROUTINE SNFCL (SNDISK, CLDISK, SNCNO, CLCNO, SNVER, CLIVER,
     *   CLOVER, SNCAT, CLCAT, NSOUWD, SOUWAN, DOSWNT, NCALWD, CALWAN,
     *   DOCWNT, TIMRA, SUBA, FREQID, NANTSL, DOAWNT, ANTENS,
     *   ORIGIN, ALLSUB, SNBUFF, CLIBUF, CLOBUF, NCOEFF, IRET)
C-----------------------------------------------------------------------
C   Subroutine to apply a SN table to a CL table.  This special version
C   of SN2CL fits the rates, single band delays, and multi band delays
C   from the SN table to polynomials (it would be easy to use other
C   functional forms).  It then uses the fitted functions to set the
C   values in the CL table.  The phases in the CL table are set by
C   integrating the single band rate equation.
C      Both tables should be sorted into Antenna-Time order before
C   calling SNFCL.
C      The output CL table must already exist as it always will in
C   CLCAL.
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
C      ORIGIN     I       Origin of SN table, 0 => multi-source or
C                         unknown, 1 => single source file.
C      ALLSUB     L       All subarrays accepted and turned into 0.
C      NCOEFF     I       The order of the polynomial
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
     *   CLOBUF(*), IRET, NCOEFF, ORIGIN
      LOGICAL   DOSWNT, DOCWNT, DOAWNT, ALLSUB
      DOUBLE PRECISION TIMRA(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INCLUDE 'INCS:PCLTAB.INC'
      INCLUDE 'CLCAL3.INC'
      LOGICAL   GOODCL, SLCTD, WANTED
      CHARACTER COLHED(2)*24, BNDCOD(MAXIF)*8
      INTEGER   IRCODE, SNREC(XCLRSZ), CLREC(XCLRSZ), IPOINT, IFOFF,
     *   SNKOLS(MAXSNC), SNNUMV(MAXSNC), CLKOLS(MAXCLC), CLNUMV(MAXCLC),
     *   FRQOFF, SNLUN, CLILUN, CLOLUN, I, IERR, THSOU, VER, KCALWD,
     *   ISUB, CLSUB, NUMANT, NUMPOL, NUMIF, NUMNOD,
     *   CLANT,  NUMSUB, STOFF, KOLS(2), COVER(2050)
      INTEGER  TIMSN, INTSN, SOUSN, ANTSN, SUBSN, FRQSN, IFRSN, NODSN,
     *   MB1SN, DI1SN, RE1SN, IM1SN, DL1SN, RA1SN, WT1SN, RF1SN,
     *   MB2SN, DI2SN, RE2SN, IM2SN, DL2SN, RA2SN, WT2SN, RF2SN,
     *   TIMCL, INTCL, SOUCL, ANTCL, SUBCL, FRQCL, IFRCL, GEODCL, DOPCL,
     *   MB1CL, DI1CL, RE1CL, IM1CL, DE1CL, RA1CL, WE1CL, RF1CL,
     *   MB2CL, DI2CL, RE2CL, IM2CL, DE2CL, RA2CL, WE2CL, RF2CL
      LOGICAL   ISAPPL, NEWCL, T, UPDATE
      INTEGER   NUMSN, NUMCL, LOOPIF, COUNT, NTERM,
     *   ISNRNO, CLIRNO, CLORNO, LOOPR
      REAL      SNRECR(XCLRSZ), CLRECR(XCLRSZ),
     *   RE, IM, TRE, TIM,
     *   GMMOD, RANOD(25), DECNOD(25),
     *   PDELAY, GDELAY, PRATE
      DOUBLE PRECISION SNRECD(XCLRSZ/2), CLRECD(XCLRSZ/2), TIME,
     *   RFREQ, FREQ(MAXIF), TLARGE, TSMALL
C                                       Set Times for validity tests
      PARAMETER (TLARGE=1.0D20, TSMALL=1.0D-6)
      REAL     FRQFAC(MAXIF), FINC(MAXIF)
      INTEGER ISBAND(MAXIF)
C                                       For fitting
      DOUBLE PRECISION  DTIME(MAXID)
      REAL       DMBD(MAXIM), DRATE(MAXIS), DWT(MAXIS), DDELAY(MAXIS)
      REAL       DMBDWT(MAXIM), DDID(MAXIM)
      INTEGER    DANT(MAXID), DSUB(MAXID), DREFAN(MAXIS), REFANT
      INTEGER    NUMDAT, IANT, IIF, IDPT, IDAT, IMDAT, ISDAT, IFADD
      REAL       MBDELY, PHI
      INTEGER    MAXSUB
      PARAMETER  (MAXSUB=4)
      INTEGER    NFITMB(MAXSUB,MAXANT,2)
      INTEGER    NFITSB(MAXSUB,MAXANT,MAXIF,2)
      INTEGER    NFITRT(MAXSUB,MAXANT,MAXIF,2)
      REAL       COEFMB(10,MAXSUB,MAXANT,2), COEFDI(10,MAXSUB,MAXANT,2)
      REAL       COEFRT(10,MAXSUB,MAXANT,MAXIF,2)
      REAL       COEFSB(10,MAXSUB,MAXANT,MAXIF,2)
C
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CLREC, CLRECR, CLRECD)
C                                        Allow negative index
      EQUIVALENCE (COVER(3), SNREC, SNRECR, SNRECD)
      DATA SNLUN, CLILUN, CLOLUN /27,28,29/
      DATA COLHED /'TIME             ',
     *             'ANTENNA NO.             '/
      DATA IRCODE /0/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
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
      IF ((IFOFF.GT.0) .AND. (SNCAT(KINAX+IFOFF).GE.1)) THEN
         CALL CHNDAT ('READ', SNBUFF, SNDISK, SNCNO, VER, SNCAT, SNLUN,
     *      NUMIF, FREQ, ISBAND, FINC, BNDCOD, FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
         END IF
      IF (NUMIF.LE.0) NUMIF = 1
      CALL AXEFND (4, 'FREQ', KICTPN, SNCAT(KHCTP), FRQOFF, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'SNFCL: CAN NOT READ FQ (OR CH) TABLE'
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
      DO 20 I = 1,NUMIF
         FREQ(I) = RFREQ + FREQ(I)
         FRQFAC(I) = 2.0 * 3.1415926 * FREQ(I) * 86400.0
 20      CONTINUE
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
C                                       Check sort order
      CALL FNDCOL (2, COLHED, 24, T, CLIBUF, KOLS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (((CLIBUF(43).EQ.KOLS(2)).AND.(CLIBUF(44).EQ.KOLS(1))) .OR.
     *   NEWCL) GO TO 40
         IRET = 1
         WRITE (MSGTXT,1031) CLIVER
         GO TO 990
C                                       Handle a new CL table
C                                       differently
 40   IF (NEWCL) THEN
         WRITE(MSGTXT,1030)
         CALL MSGWRT (8)
         IRET = -1
         GO TO 990
         END IF
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
      CALL MSGWRT (6)
C                                       Find number of subarrays.
C                                       From highest AN file version.
      CALL FNDEXT ('AN', CLCAT, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
      IF ((SUBA.GT.0) .OR. (ALLSUB)) NUMSUB = 1
C                                       Set up for fitting
C                                       Read the data into data
C                                       arrays.  Later, they will
C                                       be transfered to smaller
C                                       arrays for fitting.  This is
C                                       inefficient, but this is a
C                                       fast program.
      IDAT = 0
C                                       Read the SN table.
 145     ISNRNO = ISNRNO + 1
C                                       Finished with SN table?
         IF (ISNRNO.GT.NUMSN) GO TO 170
         IRET = 0
C                                       If more table entries, read
         CALL TABIO ('READ', IRCODE, ISNRNO, SNREC(1), SNBUFF, IRET)
         IF (IRET.LT.0) GO TO 145
         IF (IRET.NE.0) GO TO 999
C                                       Check time
         WANTED = (SNRECD(TIMSN).GE.TIMRA(1)) .AND.
     *      (SNRECD(TIMSN).LE.TIMRA(2))
C                                       Test subarray
         IF (ALLSUB) SNREC(SUBSN) = 0
         WANTED = WANTED .AND. ((SNREC(SUBSN).EQ.SUBA) .OR.
     *      (SNREC(SUBSN).LE.0) .OR. (SUBA.LE.0))
C                                       [ USE NEW CRITERIA HERE ]
C                                       Test freq id
         WANTED = WANTED .AND. ((FREQID.LE.0) .OR. (SNREC(FRQSN).LE.0)
     *      .OR. (SNREC(FRQSN).EQ.FREQID))
C                                       Antenna wanted?
         WANTED = WANTED .AND.
     *            SLCTD (SNREC(ANTSN), ANTENS, NANTSL, DOAWNT)
C                                       [ USE NEW CRITERIA HERE ]
C                                       Test if calibrator wanted
         THSOU = SNREC(SOUSN)
         WANTED = WANTED .AND.
     *      (SLCTD (THSOU, CALWAN, KCALWD, DOCWNT) .OR. (THSOU.LE.0) )
C         WANTED = WANTED .AND. SLCTD (THSOU, CALWAN, KCALWD, DOCWNT)
C
         IF (.NOT.WANTED) THEN
            GO TO 145
            END IF
C                                       Don't worry about multiple
C                                       entries at same time.
C                                       Record accepted. Get data.
C                                       Get array elements.
C                                       IMDAT and ISDAT are next - 1
         IDAT = IDAT + 1
         IMDAT = (IDAT - 1) * NUMPOL
         ISDAT = (IDAT - 1) * NUMPOL * NUMIF
C                                       Check for too much data.
         IF (IDAT .GT. MAXID .OR.
     *       IMDAT + NUMPOL .GT. MAXIM .OR.
     *       ISDAT + NUMPOL * NUMIF .GT. MAXIS)  THEN
C                                       Eventually use format
C                                       statement and more specific
C                                       error message.
            WRITE (MSGTXT, 1150) ' Too many data points '
            CALL MSGWRT (8)
            GO TO 850
C                                       Get info about data
         ELSE
            DTIME(IDAT) = SNRECD(TIMSN)
            DANT(IDAT)  = SNREC(ANTSN)
            DSUB(IDAT)  = SNREC(SUBSN)
C                                       Multiband delay.
C                                       For WT, use first IF
            DMBDWT(IMDAT+1) = SNRECR(WT1SN)
            DMBD(IMDAT+1) = SNRECR(MB1SN)
            IF (NUMPOL.GT.1) DMBD(IMDAT+2) = SNRECR(MB2SN)
            DDID(IMDAT+1) = SNRECR(DI1SN)
            IF (NUMPOL.GT.1) DDID(IMDAT+2) = SNRECR(DI2SN)
C                                       Loop over IF.
            DO 160 IFADD = 0,NUMIF-1
               ISDAT = ISDAT + 1
C                                       Set pointers
               DRATE(ISDAT) = SNRECR(RA1SN+IFADD)
               DDELAY(ISDAT) = SNRECR(DL1SN+IFADD)
               DREFAN(ISDAT) = SNREC(RF1SN+IFADD)
C                                       Eventualy test refant for
C                                       changes.
               REFANT = DREFAN(ISDAT)
               DWT(ISDAT) = SNRECR(WT1SN+IFADD)
C                                       Second Stokes'
               IF (NUMPOL.GT.1) THEN
                  ISDAT = ISDAT + 1
                  DRATE(ISDAT) = SNRECR(RA2SN+IFADD)
                  DDELAY(ISDAT) = SNRECR(DL2SN+IFADD)
                  DREFAN(ISDAT) = SNREC(RF2SN+IFADD)
                  DWT(ISDAT) = SNRECR(WT2SN+IFADD)
                  END IF
 160           CONTINUE
            END IF
         GO TO 145
C                                       Do fitting.
 170  CONTINUE
      NUMDAT = IDAT
C                                       Do the fits
      DO 250 ISUB = 1,NUMSUB
         DO 230 IANT = 1,NUMANT
            CALL FITSC (DTIME, DMBD, DMBDWT, NUMDAT, NUMPOL, 0, DSUB,
     *         DANT, ISUB, IANT, NCOEFF, NFITMB(ISUB,IANT,1),
     *         COEFMB(1,ISUB,IANT,1), FBLANK)
            IF (NUMPOL.GT.1) CALL FITSC (DTIME, DMBD, DMBDWT, NUMDAT,
     *         NUMPOL, 1, DSUB, DANT, ISUB, IANT, NCOEFF,
     *         NFITMB(ISUB,IANT,2), COEFMB(1,ISUB,IANT,2), FBLANK)
            CALL FITSC (DTIME, DDID, DMBDWT, NUMDAT, NUMPOL, 0, DSUB,
     *         DANT, ISUB, IANT, NCOEFF, NFITMB(ISUB,IANT,1),
     *         COEFDI(1,ISUB,IANT,1), FBLANK)
            IF (NUMPOL.GT.1) CALL FITSC (DTIME, DDID, DMBDWT, NUMDAT,
     *         NUMPOL, 1, DSUB, DANT, ISUB, IANT, NCOEFF,
     *         NFITMB(ISUB,IANT,2), COEFDI(1,ISUB,IANT,2), FBLANK)
            IDPT = 0
            DO 210 IIF = 1, NUMIF
               IDPT = IDPT + 1
               CALL FITSC (DTIME, DDELAY, DWT, NUMDAT, NUMIF*NUMPOL,
     *            IDPT, DSUB, DANT, ISUB, IANT, NCOEFF,
     *            NFITSB(ISUB,IANT,IIF,1), COEFSB(1,ISUB,IANT,IIF,1),
     *            FBLANK)
               CALL FITSC (DTIME, DRATE, DWT, NUMDAT, NUMIF*NUMPOL,
     *            IDPT, DSUB, DANT, ISUB, IANT, NCOEFF,
     *            NFITRT(ISUB,IANT,IIF,1), COEFRT(1,ISUB,IANT,IIF,1),
     *            FBLANK)
               IF (NUMPOL.GT.1) THEN
                  IDPT = IDPT + 1
                  CALL FITSC (DTIME, DDELAY, DWT, NUMDAT, NUMIF*NUMPOL,
     *               IDPT, DSUB, DANT, ISUB, IANT, NCOEFF,
     *               NFITSB(ISUB,IANT,IIF,2), COEFSB(1,ISUB,IANT,IIF,2),
     *               FBLANK)
                  CALL FITSC (DTIME, DRATE, DWT, NUMDAT, NUMIF*NUMPOL,
     *               IDPT, DSUB, DANT, ISUB, IANT, NCOEFF,
     *               NFITRT(ISUB,IANT,IIF,2), COEFRT(1,ISUB,IANT,IIF,2),
     *               FBLANK)
                  END IF
 210           CONTINUE
 230        CONTINUE
 250     CONTINUE
C
C  *************    CL file.  ***************
         DO 500 LOOPR = 1,NUMCL
C                                       Read OLD CL record
            CLIRNO = LOOPR
            CALL TABIO ('READ', IRCODE, CLIRNO, CLREC, CLIBUF, IRET)
            IF (IRET.LT.0) GO TO 500
            IF (IRET.NE.0) GO TO 900
            TIME = CLRECD(TIMCL)
C                                       Check time
            IF ((TIME.LT.TIMRA(1)) .OR. (TIME.GT.TIMRA(2))) GO TO 500
            CLANT = CLREC(ANTCL)
C                                       Test if source wanted.
            THSOU = CLREC(SOUCL)
            IF (.NOT.(SLCTD (THSOU, SOUWAN, NSOUWD, DOSWNT) .OR.
     *         ( THSOU.LE.0) ) ) GO TO 500
C            IF (.NOT.SLCTD (THSOU, SOUWAN, NSOUWD, DOSWNT)) GO TO 500
C                                       Test subarray
            IF (ALLSUB) CLREC(SUBCL) = 0
            IF ((CLREC(SUBCL).NE.SUBA) .AND. (CLREC(SUBCL).GT.0) .AND.
     *         (SUBA.GT.0)) GO TO 500
            CLSUB = CLREC(SUBCL)
            CLSUB = MAX (1, CLSUB)
C                                       Test freqid
            IF ((CLREC(FRQCL).NE.FREQID) .AND. (CLREC(FRQCL).GT.0) .AND.
     *         (FREQID.GT.0)) GO TO 500
C                                       Antenna wanted?
            IF (.NOT.SLCTD (CLANT, ANTENS, NANTSL, DOAWNT)) GO TO 500
C
C                                        Now get the new data values.
C                                        Multiband delays
            CALL FITEVA (TIME, COEFMB(1,CLSUB,CLANT,1), NCOEFF, MBDELY)
            IF (CLRECR(MB1CL).NE.FBLANK) CLRECR(MB1CL) =
     *         CLRECR(MB1CL) + MBDELY
C                                        Second polarization
            IF (NUMPOL.GT.1) THEN
               CALL FITEVA (TIME, COEFMB(1,CLSUB,CLANT,2), NCOEFF,
     *            MBDELY)
               IF (CLRECR(MB2CL).NE.FBLANK) CLRECR(MB2CL) =
     *            CLRECR(MB2CL) + MBDELY
               END IF
C                                        dispersions
            CALL FITEVA (TIME, COEFDI(1,CLSUB,CLANT,1), NCOEFF, MBDELY)
            IF (CLRECR(DI1CL).NE.FBLANK) CLRECR(DI1CL) =
     *         CLRECR(DI1CL) + MBDELY
C                                        Second polarization
            IF (NUMPOL.GT.1) THEN
               CALL FITEVA (TIME, COEFDI(1,CLSUB,CLANT,2), NCOEFF,
     *            MBDELY)
               IF (CLRECR(DI2CL).NE.FBLANK) CLRECR(DI2CL) =
     *            CLRECR(DI2CL) + MBDELY
               END IF
C                                       Loop over IF
            DO 300 LOOPIF = 1,NUMIF
C                                       First Stokes'
C                                       Phase - Integrate rate.
               CALL FITINT (TIME, COEFRT(1,CLSUB,CLANT,LOOPIF,1),
     *            NCOEFF, FREQ(LOOPIF), PHI)
               RE = COS (PHI)
               IM = SIN (PHI)
               TRE = CLRECR(RE1CL+LOOPIF-1)
               TIM = CLRECR(IM1CL+LOOPIF-1)
               GOODCL = (TIM.NE.FBLANK) .AND. (TRE.NE.FBLANK)
               IF ((ABS (TRE) + ABS(TIM)) .LT.1.0E-10) TRE = 1.0
               IF (GOODCL) THEN
                  CLRECR(RE1CL+LOOPIF-1) = TRE*RE - TIM*IM
                  CLRECR(IM1CL+LOOPIF-1) = TRE*IM + TIM*RE
                  END IF
C  *******  this doesn't seem to be used anywhere.
               PDELAY = ATAN2 (IM, RE) / (FREQ(LOOPIF) * 6.283185308)
C                                       Delay
               CALL FITEVA (TIME, COEFSB(1,CLSUB,CLANT,LOOPIF,1),
     *            NCOEFF, GDELAY)
               IF (CLRECR(DE1CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(DE1CL+LOOPIF-1) = CLRECR(DE1CL+LOOPIF-1) +
     *            GDELAY
C                                       Rate
               CALL FITEVA (TIME, COEFRT(1,CLSUB,CLANT,LOOPIF,1),
     *            NCOEFF, PRATE)
               IF (CLRECR(RA1CL+LOOPIF-1).NE.FBLANK)
     *            CLRECR(RA1CL+LOOPIF-1) = CLRECR(RA1CL+LOOPIF-1) +
     *            PRATE
C  ******** what should this be.        Weight
C           For now, don't change the weight.
C               CLRECR(WE1CL+LOOPIF-1) = SNRECR(IPNTW1)*WWT1 +
C     *            SNRECR(IPNTW2)*WWT2
C                                       Reference antenna
               CLREC(RF1CL+LOOPIF-1) = REFANT
C                                       Second polarization
               IF (NUMPOL.GT.1) THEN
C                                       Phase - use rate info
C                                       Phase - Integrate rate.
                  CALL FITINT (TIME, COEFRT(1,CLSUB,CLANT,LOOPIF,2),
     *               NCOEFF, FREQ(LOOPIF), PHI)
                  RE = COS (PHI)
                  IM = SIN (PHI)
                  TRE = CLRECR(RE2CL+LOOPIF-1)
                  TIM = CLRECR(IM2CL+LOOPIF-1)
                  GOODCL = (TIM.NE.FBLANK) .AND. (TRE.NE.FBLANK)
                  IF ((ABS (TRE) + ABS(TIM)) .LT.1.0E-10) TRE = 1.0
                  IF (GOODCL) THEN
                     CLRECR(RE2CL+LOOPIF-1) = TRE*RE - TIM*IM
                     CLRECR(IM2CL+LOOPIF-1) = TRE*IM + TIM*RE
                     END IF
C  *******  this doesn't seem to be used anywhere.
                  PDELAY = ATAN2 (IM, RE) / (FREQ(LOOPIF) * 6.283185308)
C                                       Delay
                  CALL FITEVA (TIME, COEFSB(1,CLSUB,CLANT,LOOPIF,2),
     *                         NCOEFF, GDELAY)
                  IF (CLRECR(DE2CL+LOOPIF-1).NE.FBLANK)
     *               CLRECR(DE2CL+LOOPIF-1) = CLRECR(DE2CL+LOOPIF-1) +
     *               GDELAY
C                                       Rate
                  CALL FITEVA (TIME, COEFRT(1,CLSUB,CLANT,LOOPIF,2),
     *               NCOEFF, PRATE)
                  IF (CLRECR(RA2CL+LOOPIF-1).NE.FBLANK)
     *               CLRECR(RA2CL+LOOPIF-1) = CLRECR(RA2CL+LOOPIF-1) +
     *               PRATE
C  ******** what should this be.        Weight
C           Don't change for now.
C                  CLRECR(WE2CL+LOOPIF-1) = SNRECR(IPNTW1)*WWT1 +
C     *               SNRECR(IPNTW2)*WWT2
C                                       Reference antenna
                  CLREC(RF1CL+LOOPIF-1) = REFANT
                  END IF
 300           CONTINUE
C                                       Rewrite record
            IF (UPDATE) THEN
               CALL TABIO ('WRIT', IRCODE, CLIRNO, CLREC, CLIBUF, IRET)
            ELSE
               CALL TABIO ('WRIT', IRCODE, CLORNO, CLREC, CLOBUF, IRET)
               END IF
            CLORNO = CLORNO + 1
            IF (IRET.NE.0) GO TO 900
            COUNT = COUNT + 1
 500     CONTINUE
C                                       Done filling table.
C                                       Close up shop.
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
         MSGTXT = 'SNFCL: WARNING: NO CL RECORDS WRITTEN'
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
 1020 FORMAT ('SNFCL: SN TABLE VERSION ',I3,' IS MISSORTED')
 1030 FORMAT ('SNFCL: Attempt to write new CL table - need old one.')
 1040 FORMAT ('SNFCL: Applying SN tables to CL table ',I3, ', writing',
     *   ' CL table', I3)
 1031 FORMAT ('SNFCL: CL TABLE VERSION ',I3,' IS MISSORTED')
 1150 FORMAT  (A)
 1900 FORMAT ('SNFCL: TABIO ERROR',I3,' APPLYING SN TO CL TABLE')
      END
      SUBROUTINE FITSC (TIME, DAT, WT, MDAT, NPDAT, IOFF, DSUB, DANT,
     *   ISUB, IANT, NCOEFF, NFIT, COEFF, FBLANK)
C-----------------------------------------------------------------------
C
C   DOCUMENT WHAT THIS DOES.
C
C   If there are efficiency problems, arrangements could be made to
C   not have to search through all of the input data by taking
C   advantage of the sort order of the SN table.
C
C   WARNING - the fitting is done with single precision time.
C   this should be adequate, but could get someone in trouble using
C   very short intervals.
C   Inputs:
C      TIME    D(*)     Array of times.
C      DAT     R(*)     Array of data.
C      WT      R(*)     Weight for the point.
C      MDAT    I        Number of data points to consider.
C      NPDAT   I        Index increment in DAT per index in TIME.
C      IOFF    I        Take IOFFth point within the increment.
C      DSUB    I(*)     Array giving subarray of data.
C      DANT    I(*)     Array giving antenna of data.
C      ISUB    I        Subarray to select.
C      IANT    I        Antenna to select.
C      NCOEFF  I        The number of polynomial coefficients to fit.
C      FBLANK  R        Value of a flagged point.
C
C   Return values:
C      NFIT    I        The number of points fitted.
C      COEFF   R(*)     The polynomial coefficients
C
C-----------------------------------------------------------------------
      INTEGER   MDAT, NPDAT, IOFF, DSUB(*), DANT(*), ISUB, IANT
      INTEGER   NCOEFF, NFIT, IPT
      REAL      DAT(*), WT(*), COEFF(*), FBLANK
      DOUBLE PRECISION TIME(*), DPTIME
      INTEGER   IDAT, JDAT, JCOEFF, IC, ITER, I, J, K
      INCLUDE 'CLCAL3.INC'
      INTEGER   MAXFIT
      PARAMETER (MAXFIT=2000)
      INTEGER   LWOR
      PARAMETER (LWOR=2*MAXFIT + 5*MAXPAR)
      REAL      U(MAXFIT,MAXPAR), V(MAXPAR,MAXPAR), W(MAXPAR)
      REAL      U2(MAXFIT,MAXFIT), WOR(LWOR), AFUNC(MAXPAR),B(MAXFIT)
      LOGICAL   FIRST
      REAL      FTIME(MAXFIT), FDAT(MAXFIT), FWT(MAXFIT), CHISQ
      REAL      DEV(MAXFIT), SORTED(MAXFIT), Y(MAXFIT), SWT(MAXFIT)
      REAL      MEDIAN, MEDIUN, FMED, TMP
      DATA      FIRST / .TRUE. /
C-----------------------------------------------------------------------
      NFIT = 0
      JCOEFF = NCOEFF
C                                       Select data
C                                       Time is single precision here.
      DO 100 IDAT = 1, MDAT
         IF (((DSUB(IDAT).EQ.ISUB) .OR. (DSUB(IDAT).LE.0)) .AND.
     *      (DANT(IDAT).EQ.IANT)) THEN
            JDAT = NPDAT *  (IDAT - 1) + IOFF
            IF ((DAT(JDAT).NE.FBLANK) .AND. (WT(JDAT).GT.0.0)) THEN
               NFIT = NFIT + 1
               FTIME(NFIT) = TIME(IDAT)
               FDAT(NFIT) = DAT(JDAT)
               FWT(NFIT) = SQRT (WT(JDAT))
               END IF
            END IF
 100     CONTINUE
C                                       Be sure not to fit too many
C                                       parameters.
      IF (NFIT.EQ.0) THEN
         JCOEFF = 0
         DO 150 IC = 1, NCOEFF
 150        COEFF(IC) = 0.0
C                                       Prevent a divide by 0 later.
      ELSE IF (NFIT.EQ.1) THEN
         COEFF(1) = FDAT(1)
         JCOEFF = 1
         DO 152 IC = 2, NCOEFF
 152        COEFF(IC) = 0.0
      ELSE
         IF (NFIT .LT. NCOEFF) THEN
C                                       Is this too little data?
            JCOEFF = NFIT
            DO 155 IC = JCOEFF + 1, NCOEFF
 155           COEFF(IC) = 0.0
            END IF
C                                       First try to downweight the
C                                       really deviant points in the
C                                       first fit.  Base this on a
C                                       multiple of the median deviation
C                                       from the median value.
         DO 170 IPT = 1, NFIT
            SORTED(IPT) = FDAT(IPT)
 170        CONTINUE
         FMED = MEDIAN (NFIT, SORTED)
         DO 172 IPT = 1, NFIT
            DEV(IPT) = ABS (FDAT(IPT) - FMED)
            SORTED(IPT) = DEV(IPT)
 172        CONTINUE
         MEDIUN = 2.0 * MEDIAN (NFIT, SORTED)
C                                       Now the attempt at a robust fit.
C                                       Taken from PTANAL.
C                                       Iterate while down weighting
C                                       deviant points.
         DO 200 ITER = 1, 6
C                                       Prevent problems with ref ant.
            IF (MEDIUN.EQ.0.0) MEDIUN = 1.0

            DO 201 I=1,NFIT
               CALL FITEQN(FTIME(I),AFUNC,JCOEFF)
C                                       Adjust weights
               SWT(I) = FWT(I) /  (1 + DEV(I) / MEDIUN)**2
               DO 202 J=1,JCOEFF
                  U(I,J) = AFUNC(J)*SWT(I)
 202              CONTINUE
                  B(I) = FDAT(I)*SWT(I)
 201           CONTINUE
C                                       invert matrix U
            CALL SGESVD('A','A',NFIT,JCOEFF,U,MAXFIT,W,U2,MAXFIT,V,
     *            MAXPAR,WOR,LWOR,I)
C                                       zero solution vector COEFF
            DO 209 I = 1,JCOEFF
               COEFF(I) = 0.0
 209           CONTINUE
C                                       save largest singular value
            CHISQ = W(1)*1.0E-5
C                                       compute correction vector
C                                       using contributions of
C                                       non-zeroed singular values.
            DO 207 I=1,JCOEFF
               IF (W(I).GE.CHISQ) THEN
                  TMP = 0.0
                  DO 208 K = 1,NFIT
                     TMP = TMP + U2(K,I)*B(K)
 208                 CONTINUE
                  TMP = TMP/W(I)
                  DO 204 J=1,JCOEFF
                     COEFF(J) = COEFF(J) + TMP*V(I,J)
 204                 CONTINUE
                  END IF
 207           CONTINUE
C                                       Get the deviations from the
C                                       model.  Need double precision
C                                       time in FITEVA.
            DO 180 IPT = 1, NFIT
               DPTIME = FTIME(IPT)
               CALL FITEVA (DPTIME, COEFF, JCOEFF, Y(IPT))
               DEV(IPT) = ABS (FDAT(IPT) - Y(IPT))
               SORTED(IPT) = DEV(IPT)
 180           CONTINUE
C                                       Base the downweighting on a
C                                       multiple of the median
C                                       deviation. SORTED used to
C                                       prevent scrambling the order of
C                                       DEV.
            MEDIUN = 3.0 * MEDIAN (NFIT, SORTED)
C
 200        CONTINUE
         FIRST = .FALSE.
C
         END IF
C
      RETURN
      END
      SUBROUTINE FITEQN (XX, AFUNC, MA)
C----------------------------------------------------------------------
C     Subroutine for SNFCL that is called by the least squares fitting
C     routine to calculate a polynomial gain curve.  Recall that XX will
C     be time in days and that most geometry terms go as sins and
C     cosines of the sidereal day.
C-----------------------------------------------------------------------
      INTEGER     MA, I
      REAL        XX, AFUNC(MA), STWOPI
      PARAMETER   (STWOPI = 6.2831853E0 * 1.0027379E0)
C-----------------------------------------------------------------------
      AFUNC(1) = 1.0
      IF (MA .GE. 2) THEN
         DO 100 I = 2, MA
            AFUNC(I) = AFUNC(I-1) * XX
 100        CONTINUE
         END IF
      RETURN
      END
      SUBROUTINE FITEVA (TIME, COEFF, MA, VAL)
C-----------------------------------------------------------------------
C     Subroutine for SNFCL that evaluates a polynomial at a given
C     point.  For use with XX is double precision time.
C     COS and SIN terms commented out.
C-----------------------------------------------------------------------
      INTEGER           MA
      REAL              COEFF(MA), VAL
      DOUBLE PRECISION  TIME
C
      INTEGER           I
      REAL              FUNC
      DOUBLE PRECISION  STWOPI
      PARAMETER   (STWOPI = 6.283185308D0 * 1.00273790934D0)
C-----------------------------------------------------------------------
      FUNC = 1.0
      VAL = COEFF(1)
      IF (MA.GE.2) THEN
         DO 100 I = 2,MA
            FUNC = FUNC * TIME
            VAL = VAL + COEFF(I) * FUNC
 100        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FITINT (TIME, COEFF, N, FREQ, PHI)
C-----------------------------------------------------------------------
C     Subroutine for SNFCL that evaluates the integral of the rate
C     equation from t = 0 to t = TIME.  Use double precision for TIME
C     and PHASE until after the MOD 2*PI.  Recall that the rate numbers
C     are in seconds per second of delay.  For a phase, they need to
C     be multiplied by a frequency.
C
C     Evaluate PHASE in radians modulo 2 pi.
C-----------------------------------------------------------------------
      REAL              COEFF(*), PHI
      DOUBLE PRECISION  TIME, FUNC, PHASE, FREQ, DELOFF, TWOPI, STWOPI
      INTEGER           N, I
      PARAMETER         (TWOPI = 6.283185308D0)
      PARAMETER         (STWOPI = TWOPI * 1.00273790934D0)
C----------------------------------------------------------------------
      FUNC = TIME
      DELOFF = COEFF(1) * FUNC
      IF (N .GE. 2) THEN
         DO 100 I = 2, N
            FUNC = FUNC * TIME
 100        DELOFF = DELOFF + COEFF(I) * FUNC / I
         END IF
C                                     Time is in days so must multiply
C                                     by frequency in Hz times sec/day.
      PHASE = FREQ * 86400.D0 * DELOFF * TWOPI
      PHASE = MOD (PHASE, TWOPI)
      PHI = PHASE
      RETURN
      END
      SUBROUTINE HSORT (K, N)
C-----------------------------------------------------------------------
C   HSORT performs a heap sort.  Transcribed from public domain code
C   donated by Fred Schwab, with this cryptic notation embedded:
C   "Algorithm for sorting on vector of keys K of length N"
C   "J. F. Monahan  Transcribed from Knuth, vol 2, PP 146-7."
C   Not sure which "Knuth" work this is referring to; he's done rather a
C   lot!
C-----------------------------------------------------------------------
      INTEGER   N, R, L, I, J
      REAL      K(N), KK
C-----------------------------------------------------------------------
      IF (N.LE.1) RETURN
      L = N/2 + 1
      R = N
 10   IF (L.GT.1) GO TO 20
      KK = K(R)
      K(R) = K(1)
      R = R - 1
      IF (R.EQ.1) GO TO 80
      GO TO 30
 20   L = L - 1
      KK = K(L)
 30   J = L
 40   I = J
      J = 2*J
      IF (J-R) 50, 60, 70
 50   IF (K(J).LT.K(J+1)) J = J + 1
 60   IF (KK.GT.K(J)) GO TO 70
      K(I) = K(J)
      GO TO 40
 70   K(I) = KK
      GO TO 10
 80   K(1) = KK
C
      RETURN
      END
      SUBROUTINE SNSMO (SMOOTH, METHOD, DISK, CNO, INVER, REFA, CATBLK,
     *   INBUF, SMOTIM, SMOTYP, XDOBLK, TIMRNG, CALWAN, NCALWD, DOCWNT,
     *   FREQID, OUTVER, ALLSUB, OUTBUF, MAXTIM, WRKTIM, WORK1, WORK2,
     *   WORK3, WORK4, WORK5, WORK6, WORK7, WORK8, WORKS, IRET)
C-----------------------------------------------------------------------
C   Smooth and/or copy an antenna-time sorted SN table to a specified
C   output SN table associated with the same uv data set.  Data are
C   smoothed using the specified filter separately for amplitude, phase
C   and delay and rate.  If delays are modified then the phase is also
C   adjusted.
C
C   If NCALWD is greater than zero and DOCWNT is true then only those
C   records that apply to sources listed in CALWAN are smoothed. If
C   NCALWD is greater than zero and DOCWNT is false then only those
C   records that do not apply to sources in CALWAN are smoothed.
C
C   If smoothing is requested the input table is always smoothed, even
C   if OUTVER does not equal INVER.
C
C   If OUTVER does not equal INVER than the mean gain modulus is not
C   updated for the table with version number OUTVER.
C
C   This routine uses logical unit numbers 28, 29 and 30.
C
C   Inputs:
C    SMOOTH     L       True if data are to be smoothed.
C    METHOD     C*4     Smoothing method, 'BOX','MWF' anything else =
C                       'BOX'
C    DISK       I       Disk number of uv data set.
C    CNO        I       Catalogue number of uv data set.
C    INVER      I       Input SN table version number.
C    CATBLK     I(256)  Catalogue header block of uv data set.
C    SMOTIM     R(3)    Boxcar widths for amplitude (1), phase (2) and
C                        delay and rate(3) in days.
C    SMOTYP     I       Type of data to smooth:
C                        1 - amplitude only;
C                        2 - phase only;
C                        3 - amplitude and phase;
C                        4 - amplitude, phase, delay and rate; or
C                        5 - delay and rate.
C    XDOBLK     R       >= 0 replace previously blanked values
C                       <= 0 replace previously good values
C    TIMRNG     D(2)    First and last time to process in days.
C    CALWAN     I(*)    List of requested calibration sources.
C    NCALWD     I       Number of requested calibration sources.
C    DOCWNT     L       .TRUE. is requested sources are selected.
C    FREQID     I       The FQ id number selected (0=> 1)
C    OUTVER     I       Output SN table version number.
C    MAXTIM     I       Maximum number of times that can be handled (the
C                        dimension of the work arrays.
C    ALLSUB     L       All subarrays read, used together, written to 0
C   Output:
C    IRET       I       Return code, non-zero if an error was detected,
C                        zero otherwise.
C
C   Scratch:
C    INBUF      I(*)    TABIO buffer for input table.
C    OUTBUF     I(*)    TABIO buffer for output table (if different).
C    WRKTIM     R(*)    Work array (holds times of SN records) of
C                        dimension MAXTIM.
C    WORK1      R(*)    Work array of dimension MAXTIM.
C    WORK2      R(*)    Work array of dimension MAXTIM.
C    WORK3      R(*)    Work array of dimension MAXTIM.
C    WORK4      R(*)    Work array of dimension MAXTIM.
C    WORK5      R(*)    Work array of dimension MAXTIM.
C    WORK6      R(*)    Work array of dimension MAXTIM.
C    WORK7      R(*)    Work array of dimension MAXTIM.
C    WORK8      R(*)    Work array of dimension MAXTIM.
C    WORKS      R(*)    Work array of dimension MAXTIM. -1 IN => none
C-----------------------------------------------------------------------
      LOGICAL   SMOOTH, DOCWNT, ALLSUB
      CHARACTER METHOD*4
      INTEGER   DISK, CNO, INVER, REFA, CATBLK(256), INBUF(*), SMOTYP,
     *   CALWAN(*), NCALWD, FREQID, OUTVER, OUTBUF(*), MAXTIM, WORKS(*),
     *   IRET
      REAL      SMOTIM(3,5), XDOBLK, WRKTIM(*), WORK1(*), WORK2(*),
     *   WORK3(*), WORK4(*), WORK5(*), WORK6(*), WORK7(*), WORK8(*)
      DOUBLE PRECISION TIMRNG(2)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      LOGICAL   APPEND, ISAPPL, FLAGED, LASTPS, BAD, SLCTD, BLNKD,
     *   DOSORC
      INTEGER   LUN1, LUN2, LUN3, GMCNT, ISNRNO, KOLS(MAXSNC), I,
     *   NUMV(MAXSNC), NUMANT, NUMPOL, NUMIF, NUMNOD, IRCODE, LOOPR,
     *   RECORD(XCLRSZ), SNKOLS(MAXSNC),NHCOL, TIMKOL, ANTKOL, SUBKOL,
     *   IFQKOL, RE1KOL, IM1KOL, DL1KOL,RA1KOL, WT1KOL, RE2KOL, IM2KOL,
     *   DL2KOL, RA2KOL, WT2KOL, SRCKOL,NUMREC, NUMSUB, OSNRNO, SUB,
     *   REKOL, IMKOL, DELKOL, RATKOL,WTKOL, POL, IIF, FSTREC, LSTREC,
     *   ANT, NUMTIM, ITIME, LGT1, RF1KOL, RF2KOL, RFKOL, DI1KOL,
     *   DI2KOL, DIKOL, MB1KOL, MB2KOL, MBKOL
      REAL      GMMOD, RANOD(25), DECNOD(25), RECR(XCLRSZ), TINY, AMPL,
     *   FACTOR, PHASE
      DOUBLE PRECISION RECD(XCLRSZ/2), GMSUM, TIMOFF, KEYVAD
      CHARACTER KEYWRD*8, COLHDR(2)*24
C     REAL      DELCOR, PCOR, SCOR, CCOR, GR, GI, FINC(MAXIF)
C     DOUBLE PRECISION FOFF(MAXIF)
C     INTEGER   FQTMP, ISBAND(MAXIF), NIF, JNVER
C     LOGICAL   CORDEL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      PARAMETER (LUN1 = 28)
      PARAMETER (LUN2 = 29)
      PARAMETER (LUN3 = 30)
      PARAMETER (TINY = 1.0E-20)
      PARAMETER (KEYWRD = 'MGMOD   ')
      DATA COLHDR /'TIME ', 'ANTENNA NO. '/
C-----------------------------------------------------------------------
C                                       If the output table is not the
C                                       same as the input table copied
C                                       or smoothed data are appended to
C                                       it.
      APPEND = OUTVER.NE.INVER
      DOSORC = WORKS(1).GE.0
C                                       GMSUM holds the sum of the
C                                       amplitude corrections and GMCNT
C                                       the number of amplitude
C                                       corrections.
      GMSUM = 0.0D0
      GMCNT = 0
C                                       Is smoothing of delays
C                                       requested?
C     CORDEL = (SMOTYP.EQ.4) .OR. (SMOTYP.EQ.5)
C     DELCOR = 0.0
C                                       Read frequency table if it is to
C                                       be needed for delay-pahse
C                                       correction.
C     JNVER = 1
C                                       This cludge will only work if
C                                       FREQID=-1 and there is only i FQ
C                                       id.
C     FQTMP = FREQID
C     IF (FQTMP.LE.0) FQTMP = 1
C     IF (CORDEL) CALL CHNDAT ('READ', INBUF, DISK, CNO, JNVER, CATBLK,
C    *   LUN1, NIF, FOFF, ISBAND, FINC, BNDCOD, FQTMP, IRET)
C     IF (IRET.NE.0) GO TO 999
C                                       Open the input table:
C                                         Reformat table if necessary:
      CALL SNREFM (DISK, CNO, INVER, CATBLK, LUN1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                         Perform a dummy open to read
C                                         the keywords:
      CALL SNINI ('READ', INBUF, DISK, CNO, INVER, CATBLK, LUN1, ISNRNO,
     *   SNKOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
      IRCODE = 0
      CALL TABIO ('CLOS', IRCODE, LOOPR, RECORD, INBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                         Reopen with write permission:
      CALL SNINI ('WRIT', INBUF, DISK, CNO, INVER, CATBLK, LUN1, ISNRNO,
     *   SNKOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      IFQKOL = SNKOLS(SNIFQI)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
      SRCKOL = SNKOLS(SNISID)
      RF1KOL = SNKOLS(SNIRF1)
      RF2KOL = SNKOLS(SNIRF2)
      MB1KOL = SNKOLS(SNRMD1)
      MB2KOL = SNKOLS(SNRMD2)
      DI1KOL = SNKOLS(SNRDS1)
      DI2KOL = SNKOLS(SNRDS2)
C                                       Check sort order
      NHCOL = 2
      CALL FNDCOL (NHCOL, COLHDR, 24, .TRUE., INBUF, KOLS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SMOOTH .AND. ((INBUF(43).NE.KOLS(2)) .OR.
     *   (INBUF(44).NE.(KOLS(1))))) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) INVER
         CALL MSGWRT (8)
         END IF
C                                       Read number of records in input
C                                       table:
      NUMREC = INBUF(5)
C                                       Read number of subarrays:
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (NUMSUB, 1)
      IF (ALLSUB) NUMSUB = 1
C                                       Open output table, if different
C                                       from input table:
      IF (APPEND) THEN
         CALL SNREFM (DISK, CNO, OUTVER, CATBLK, LUN2, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL SNINI ('WRIT', OUTBUF, DISK, CNO, OUTVER, CATBLK, LUN2,
     *      OSNRNO, SNKOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *      RANOD, DECNOD, ISAPPL, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       If the output table is empty
C                                       then the sort order is the same
C                                       as for the input table,
C                                       otherwise the sort order is
C                                       unknown:
         IF (OSNRNO.LE.1) THEN
            OUTBUF(43) = INBUF(43)
            OUTBUF(44) = INBUF(44)
         ELSE
            OUTBUF(43) = 0
            OUTBUF(44) = 0
            END IF
         END IF
C
      IF (SMOOTH) THEN
C                                       Inform user of smoothing:
         WRITE (MSGTXT, 1001)
         CALL MSGWRT (5)
C                                       Process each subarray:
         DO 440 SUB = 1,NUMSUB
C                                       Set pointers:
            REKOL  = RE1KOL
            IMKOL  = IM1KOL
            DELKOL = DL1KOL
            RATKOL = RA1KOL
            WTKOL  = WT1KOL
            RFKOL  = RF1KOL
            MBKOL = MB1KOL
            DIKOL = DI1KOL
C                                       Process each polarization:
            DO 430 POL = 1,NUMPOL
C                                       Process each IF:
               DO 420 IIF = 1, NUMIF
C                                       Start from beginning of file for
C                                       each subarray, polarization or
C                                       IF:
                  FSTREC = 1
C                                       Check if this is the last pass
C                                       through the input table for the
C                                       current subarray, in which case
C                                       we begin appending to the output
C                                       file:
                  LASTPS = (POL.EQ.NUMPOL) .AND. (IIF.EQ.NUMIF)
C                                       Process each antenna:
                  DO 410 ANT = 1,NUMANT
                     NUMTIM = 0
                     ISNRNO = FSTREC
C                                        Check not at end of file.
                     IF (ISNRNO.GT.NUMREC) GO TO 410
C                                       Read first record:
                     CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, INBUF,
     *                  IRET)
                     IF (IRET.GT.0) GO TO 999
                     FLAGED = IRET.LT.0
C                                       Process each record for antenna
C                                       ANT:
 100                 IF ((RECORD(ANTKOL).EQ.ANT) .AND.
     *                  (ISNRNO.LE.NUMREC)) THEN
C                                       Check subarray, source, time and
C                                       flagging:
                        IF (ALLSUB) RECORD(SUBKOL) = 0
                        IF (((RECORD(SUBKOL).EQ.SUB) .OR.
     *                     (RECORD(SUBKOL).LE.0)) .AND.
     *                     (SLCTD (RECORD(SRCKOL), CALWAN, NCALWD,
     *                     DOCWNT) .OR. (RECORD(SRCKOL).LE.0)) .AND.
     *                     (RECD(TIMKOL).GE.TIMRNG(1)) .AND.
     *                     (RECD(TIMKOL).LE.TIMRNG(2)) .AND.
     *                     ((RECORD(IFQKOL).EQ.FREQID) .OR.
     *                     (RECORD(IFQKOL).LE.0)) .AND. (.NOT.FLAGED))
     *                     THEN
C                                       Check for buffer overflow:
                           IF (NUMTIM.EQ.MAXTIM) THEN
                              IRET = 2
                              WRITE (MSGTXT, 1100) MAXTIM
                              CALL MSGWRT (8)
                              GO TO 999
                              END IF
                           NUMTIM = NUMTIM + 1
C                                       Check whether data is bad:
                           BAD = (RECR(WTKOL).LT.0.0) .OR.
     *                        (RECR(REKOL).EQ.FBLANK) .OR.
     *                        ((ABS (RECR(REKOL)).LT.TINY) .AND.
     *                        (ABS (RECR(IMKOL)).LT.TINY))
C
                           IF (DOSORC) WORKS(NUMTIM) = RECORD(SRCKOL) +
     *                        0.5
                           IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
                           WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
                           IF (BAD) THEN
                              WORK2(NUMTIM) = FBLANK
                              WORK3(NUMTIM) = FBLANK
                              WORK4(NUMTIM) = FBLANK
                              WORK5(NUMTIM) = FBLANK
                              WORK6(NUMTIM) = FBLANK
                              WORK7(NUMTIM) = FBLANK
                              WORK8(NUMTIM) = FBLANK
                           ELSE
                              AMPL = SQRT (RECR(REKOL)**2 +
     *                           RECR(IMKOL)**2)
                              WORK2(NUMTIM) = AMPL
                              IF (AMPL.EQ.0.0) AMPL = 1.0
                              WORK3(NUMTIM) = RECR(REKOL) / AMPL
                              WORK4(NUMTIM) = RECR(IMKOL) / AMPL
                              WORK5(NUMTIM) = RECR(DELKOL)
                              WORK6(NUMTIM) = RECR(RATKOL)
                              WORK7(NUMTIM) = RECR(MBKOL)
                              WORK8(NUMTIM) = RECR(DIKOL)
                              END IF
                           END IF
                        ISNRNO = ISNRNO + 1
                        IF (ISNRNO.LE.NUMREC) THEN
                           CALL TABIO ('READ', IRCODE, ISNRNO, RECORD,
     *                        INBUF, IRET)
                           IF (IRET.GT.0) GO TO 999
                           FLAGED = IRET.LT.0
                           END IF
                        GO TO 100
                        END IF
                     LSTREC = ISNRNO - 1
C                                       convert to phase
                     IF ((SMOTYP.GE.2) .AND. (SMOTYP.LE.4)) THEN
                        LGT1 = 0
                        DO 110 I = 1,NUMTIM
                           IF (WORK3(I).NE.FBLANK) THEN
                              WORK3(I) = ATAN2 (WORK4(I), WORK3(I) +
     *                           1.E-20)
                              IF (LGT1.GT.0) WORK3(I) = WORK3(I) -
     *                           TWOPI * NINT ((WORk3(I)-WORK3(LGT1)) /
     *                           TWOPI)
                              LGT1 = I
                              END IF
 110                       CONTINUE
                        END IF
C                                       Smooth data:
C                                       Amplitude branch.
                     IF (SMOTYP.EQ.1) THEN
                        CALL SNSMSM (METHOD, SMOTIM(1,1), WRKTIM, WORK2,
     *                     FBLANK, NUMTIM, WORKS, WORK1)
C                                       Phase branch.
                     ELSE IF (SMOTYP.EQ.2) THEN
                        CALL SNSMSM (METHOD, SMOTIM(1,2), WRKTIM, WORK3,
     *                     FBLANK, NUMTIM, WORKS, WORK2)
C                                       Amplitude and phase branch.
                     ELSE IF (SMOTYP.EQ.3) THEN
                        CALL SNSMSM (METHOD, SMOTIM(1,1), WRKTIM, WORK2,
     *                     FBLANK, NUMTIM, WORKS, WORK1)
                        CALL SNSMSM (METHOD, SMOTIM(1,2), WRKTIM, WORK4,
     *                     FBLANK, NUMTIM, WORKS, WORK3)
C                                       Amplitude, phase, delay and rate
C                                       branch.
                     ELSE IF (SMOTYP.EQ.4) THEN
                        CALL SNSMSM (METHOD, SMOTIM(1,1), WRKTIM, WORK2,
     *                     FBLANK, NUMTIM, WORKS, WORK1)
                        CALL SNSMSM (METHOD, SMOTIM(1,2), WRKTIM, WORK3,
     *                     FBLANK, NUMTIM, WORKS, WORK2)
                        CALL SNSMSM (METHOD, SMOTIM(1,4), WRKTIM, WORK5,
     *                     FBLANK, NUMTIM, WORKS, WORK4)
                        CALL SNSMSM (METHOD, SMOTIM(1,3), WRKTIM, WORK6,
     *                     FBLANK, NUMTIM, WORKS, WORK5)
                        IF (IIF.EQ.1) THEN
                           CALL SNSMSM (METHOD, SMOTIM(1,5), WRKTIM,
     *                        WORK7, FBLANK, NUMTIM, WORKS, WORK6)
                           CALL SNSMSM (METHOD, SMOTIM(1,5), WRKTIM,
     *                        WORK8, FBLANK, NUMTIM, WORKS, WORK7)
                           END IF
C                                       Delay and rate branch.
                     ELSE IF (SMOTYP.EQ.5) THEN
                        CALL SNSMSM (METHOD, SMOTIM(1,4), WRKTIM, WORK5,
     *                     FBLANK, NUMTIM, WORKS, WORK4)
                        CALL SNSMSM (METHOD, SMOTIM(1,3), WRKTIM, WORK6,
     *                     FBLANK, NUMTIM, WORKS, WORK5)
                        IF (IIF.EQ.1) THEN
                           CALL SNSMSM (METHOD, SMOTIM(1,5), WRKTIM,
     *                        WORK7, FBLANK, NUMTIM, WORKS, WORK6)
                           CALL SNSMSM (METHOD, SMOTIM(1,5), WRKTIM,
     *                        WORK8, FBLANK, NUMTIM, WORKS, WORK7)
                           END IF
                        END IF
C                                       Write out smoothed data:
                     ITIME = 1
                     DO 400 ISNRNO = FSTREC, LSTREC
                        CALL TABIO ('READ', IRCODE, ISNRNO, RECORD,
     *                     INBUF, IRET)
                        IF (IRET.GT.0) THEN
                           WRITE (MSGTXT, 1290) IRET
                           CALL MSGWRT (8)
                           END IF
                        IF (IRET.EQ.0) THEN
C                                       Record is not flagged.
C                                       Check subarray, source and time:
                           IF (ALLSUB) RECORD(SUBKOL) = 0
                           IF (((RECORD(SUBKOL).EQ.SUB) .OR.
     *                        (RECORD(SUBKOL).LE.0)) .AND.
     *                        (SLCTD (RECORD(SRCKOL), CALWAN, NCALWD,
     *                         DOCWNT) .OR. (RECORD(SRCKOL).LE.0)) .AND.
     *                        ((RECORD(IFQKOL).EQ.FREQID) .OR.
     *                        (RECORD(IFQKOL).LE.0)) .AND.
     *                        (RECD(TIMKOL).GE.TIMRNG(1)) .AND.
     *                        (RECD(TIMKOL).LE.TIMRNG(2))) THEN
                              BLNKD = (RECR(REKOL).EQ.FBLANK) .OR.
     *                           (RECR(IMKOL).EQ.FBLANK)
C                                       Amplitude branch:
                              IF (SMOTYP.EQ.1) THEN
                                 IF ((.NOT.BLNKD) .AND.
     *                              (WORK1(ITIME).NE.FBLANK)) THEN
                                    AMPL = SQRT (RECR(REKOL)**2 +
     *                                 RECR(IMKOL)**2)
                                    IF (AMPL.LE.TINY) AMPL =
     *                                 WORK1(ITIME)
                                    FACTOR = WORK1(ITIME) / AMPL
                                    RECR(REKOL) = FACTOR * RECR(REKOL)
                                    RECR(IMKOL) = FACTOR * RECR(IMKOL)
C                                       Update gain modulus:
                                    IF ((RECR(REKOL).NE.1.0) .OR.
     *                                 (RECR(IMKOL).NE.0.0)) THEN
                                       GMCNT = GMCNT + 1
                                       GMSUM = GMSUM
     *                                    + SQRT (RECR(REKOL)**2
     *                                    +       RECR(IMKOL)**2)
                                       END IF
                                    END IF
C                                       Phase branch.
                              ELSE IF (SMOTYP.EQ.2) THEN
                                 IF ((.NOT.BLNKD) .AND.
     *                              (WORK2(ITIME).NE.FBLANK)) THEN
                                    AMPL = SQRT ((RECR(REKOL)**2 +
     *                                 RECR(IMKOL)**2) /
     *                                 (WORK2(ITIME)**2 +
     *                                 WORK3(ITIME)**2))
                                    PHASE = WORK2(ITIME)
                                    RECR(REKOL) = AMPL * COS (PHASE)
                                    RECR(IMKOL) = AMPL * SIN (PHASE)
                                    END IF
C                                       Amplitude and phase branch.
                              ELSE IF (SMOTYP.EQ.3) THEN
                                 IF ((.NOT.BLNKD) .AND.
     *                              (WORK1(ITIME).NE.FBLANK) .AND.
     *                              (WORK2(ITIME).NE.FBLANK)) THEN
                                    PHASE = WORK2(ITIME)
                                    RECR(REKOL) = WORK1(ITIME) *
     *                                 COS (PHASE)
                                    RECR(IMKOL) = WORK1(ITIME) *
     *                                 SIN (PHASE)
C                                       Update gain modulus:
                                    IF (WORK1(ITIME).NE.1.0) THEN
                                       GMCNT = GMCNT + 1
                                       GMSUM = GMSUM + WORK1(ITIME)
                                       END IF
                                    END IF
C                                       Amplitude, phase, rate and delay
                              ELSE IF (SMOTYP.EQ.4) THEN
                                 IF (((BLNKD) .AND. (XDOBLK.GE.0.0))
     *                              .OR. ((.NOT.BLNKD) .AND.
     *                              (XDOBLK.LE.0.0))) THEN
                                    IF ((WORK1(ITIME).EQ.FBLANK) .OR.
     *                                 (WORK2(ITIME).EQ.FBLANK)) THEN
                                       RECR(REKOL) = FBLANK
                                       RECR(IMKOL) = FBLANK
                                    ELSE
                                       PHASE = WORK2(ITIME)
                                       RECR(REKOL) = WORK1(ITIME) *
     *                                    COS(PHASE)
                                       RECR(IMKOL) = WORK1(ITIME) *
     *                                    SIN(PHASE)
C                                       Update gain modulus:
                                       IF (WORK1(ITIME).NE.1.0) THEN
                                          GMCNT = GMCNT + 1
                                          GMSUM = GMSUM + WORK1(ITIME)
                                          END IF
                                       END IF
C                                       Delay correction for phase
C                                       correction.
                                    IF (WORK4(ITIME).NE.FBLANK) THEN
C                                      DELCOR = 0.
C                                      IF (RECR(DELKOL).NE.FBLANK)
C    *                                    DELCOR = WORK4(ITIME)
C    *                                    -RECR(DELKOL)
                                       RECR(DELKOL) = WORK4(ITIME)
                                       END IF
                                    IF (WORK5(ITIME).NE.FBLANK)
     *                                 RECR(RATKOL) = WORK5(ITIME)
                                    IF (IIF.EQ.1) THEN
                                       IF (WORK6(ITIME).NE.FBLANK)
     *                                    RECR(MBKOL) = WORK6(ITIME)
                                       IF (WORK7(ITIME).NE.FBLANK)
     *                                    RECR(DIKOL) = WORK7(ITIME)
                                       END IF
                                    END IF
C                                       Delay and rate branch.
                              ELSE IF (SMOTYP.EQ.5) THEN
C                                       Delay correction for phase
C                                       correction.
                                 IF (WORK4(ITIME).NE.FBLANK) THEN
C                                   DELCOR = 0.
C                                   IF (RECR(DELKOL).NE.FBLANK) DELCOR =
C    *                                 WORK4(ITIME) - RECR(DELKOL)
                                    RECR(DELKOL) = WORK4(ITIME)
                                    END IF
                                 IF (WORK5(ITIME).NE.FBLANK)
     *                              RECR(RATKOL) = WORK5(ITIME)
                                 IF (IIF.EQ.1) THEN
                                    IF (WORK6(ITIME).NE.FBLANK)
     *                                 RECR(MBKOL) = WORK6(ITIME)
                                    IF (WORK7(ITIME).NE.FBLANK)
     *                                 RECR(DIKOL) = WORK7(ITIME)
                                    END IF
                                 END IF
C                                       Any correction to delay?  If so
C                                       must also correct phase.
C                                       Not so decided on 12/2006.
C                             IF (CORDEL) THEN
C                                PCOR = -TWOPI * FOFF(IIF) * DELCOR
C                                CCOR = COS (PCOR)
C                                SCOR = SIN (PCOR)
C                                GR = RECR(REKOL)
C                                GI = RECR(IMKOL)
C                                IF ((RECR(REKOL).NE.FBLANK) .AND.
C    *                              (RECR(IMKOL).NE.FBLANK))THEN
C                                   RECR(REKOL) = GR * CCOR - GI * SCOR
C                                   RECR(IMKOL) = GR * SCOR + GI * CCOR
C                                   END IF
C                                END IF
C                                       check weight column
                              IF ((RECR(WTKOL).LT.TINY) .OR.
     *                           (RECR(WTKOL).EQ.FBLANK)) THEN
                                 IF ((RECR(REKOL).NE.FBLANK) .AND.
     *                              (RECR(IMKOL).NE.FBLANK) .AND.
     *                              (RECR(DELKOL).NE.FBLANK) .AND.
     *                              (RECR(RATKOL).NE.FBLANK))
     *                              RECR(WTKOL) = 1.0
                                    END IF
C                                       check ref ant column
                              IF (RECORD(RFKOL).LE.0) THEN
                                 IF ((RECR(REKOL).NE.FBLANK) .AND.
     *                              (RECR(IMKOL).NE.FBLANK) .AND.
     *                              (RECR(DELKOL).NE.FBLANK) .AND.
     *                              (RECR(RATKOL).NE.FBLANK))
     *                              RECORD(RFKOL) = REFA
                                    END IF
C                                       Write out modified record
                              CALL TABIO ('WRIT', IRCODE, ISNRNO,
     *                           RECORD, INBUF, IRET)
                              IF (IRET.NE.0) THEN
                                 WRITE (MSGTXT, 1290) IRET
                                 CALL MSGWRT (8)
                                 GO TO 999
                                 END IF
                              IF (APPEND.AND.LASTPS) THEN
                                 CALL TABIO ('WRIT', IRCODE, OSNRNO,
     *                              RECORD, OUTBUF, IRET)
                                 IF (IRET.NE.0) THEN
                                    WRITE (MSGTXT, 1290) IRET
                                    CALL MSGWRT (8)
                                    GO TO 999
                                    END IF
                                 OSNRNO = OSNRNO + 1
                                 END IF
                              ITIME = ITIME + 1
                              END IF
                           END IF
 400                    CONTINUE
                     FSTREC = LSTREC + 1
C                                       End of antenna loop.
 410                 CONTINUE
C                                       End of IF loop.
C                                       Update pointers:
                  WTKOL = WTKOL + 1
                  REKOL = REKOL + 1
                  IMKOL = IMKOL + 1
                  RATKOL = RATKOL + 1
                  DELKOL = DELKOL + 1
 420              CONTINUE
C                                       End of polarization loop.
C                                       Update pointers:
               WTKOL = WT2KOL
               RFKOL = RF2KOL
               REKOL = RE2KOL
               IMKOL = IM2KOL
               RATKOL = RA2KOL
               DELKOL = DL2KOL
               MBKOL = MB2KOL
               DIKOL = DI2KOL
 430           CONTINUE
C                                       End of subarray loop.
 440        CONTINUE
C                                       No smoothing.
      ELSE
C                                       Append the input SN table to the
C                                       output SN table:
         DO 500 ISNRNO = 1, NUMREC
            CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, INBUF, IRET)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT, 1400) IRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
C                                       Record is unflagged.
            IF (IRET.EQ.0) THEN
               IF (ALLSUB) RECORD(SUBKOL) = 0
               CALL TABIO ('WRIT', IRCODE, OSNRNO, RECORD, OUTBUF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT, 1400) IRET
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               OSNRNO = OSNRNO + 1
               END IF
 500        CONTINUE
         END IF
C                                       Update mean gain modulus of
C                                       input file:
      IF (ABS (GMMOD-1.0).GT.1.0E-5) THEN
         IF (GMCNT.NE.0) THEN
            KEYVAD = GMSUM / GMCNT
            CALL TABKEY ('WRIT', KEYWRD, 1, INBUF, 1, KEYVAD, 1, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT, 1500) IRET
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
         END IF
C                                       Close tables:
      CALL TABIO ('CLOS', IRCODE, ISNRNO, RECORD, INBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT, 1501) IRET
         CALL MSGWRT (8)
         GO TO 999
         END IF
      IF (APPEND) THEN
         CALL TABIO ('CLOS', IRCODE, OSNRNO, RECORD, OUTBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1501) IRET
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNSMO: SN TABLE ',I3, ' IS MISSORTED')
 1001 FORMAT ('SNSMO: Smoothing SN table')
 1100 FORMAT ('SNSMO: INTERNAL BUFFER (', I6, ' WORDS) OVERFLOWED')
 1290 FORMAT ('SNSMO: TABIO ERROR ', I2, ' SMOOTHING SOLUTIONS')
 1400 FORMAT ('SNSMO: TABIO ERROR ', I2, ' COPYING SOLUTIONS')
 1500 FORMAT ('SNSMO: TABIO ERROR ', I2, ' UPDATING KEYWORDS')
 1501 FORMAT ('SNSMO: TABIO ERROR ', I2, ' CLOSING SN TABLES')
      END
      SUBROUTINE SNSMSM (SMMETH, SMOTIM, TIME, IN, BLANK, NUMTIM, S,
     *   OUT)
C-----------------------------------------------------------------------
C   Routine to call appropriate smoothing routine.  Magic value blanking
C   is supported.
C   Inputs:
C      SMMETH   C*4    Method 'MWF', 'GAUS', 'EXP', 'LINE', '2PT',
C                             '2PTH', else 'BOX'
C      SMOTIM   R(*)   Smoothing time (days) and width if needed,
C                      (3) is min allowed sum weights of smooth function
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
         CALL MWFBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, OUT)
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
         CALL TPTBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, .FALSE., OUT)
C                                       2-point - hanning
      ELSE IF (SMMETH.EQ.'2PTH') THEN
         CALL TPTBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, .TRUE., OUT)
C                                       Default = Boxcar
      ELSE
         CALL BOXBSM (SMOTIM, TIME, IN, S, BLANK, NUMTIM, OUT)
         END IF
C
 999  RETURN
      END
      SUBROUTINE SNMRG (DISK, CNO, INVER, CATBLK, INBUF, OUTBUF, IRET)
C-----------------------------------------------------------------------
C   Merge an antenna-time sorted SN table to itself.  If there are 2
C   records at the same time, magic blanks in the one are replaced with
C   good values in the next.
C
C   This routine uses logical unit numbers 28, 29 and 30.
C
C   Inputs:
C      DISK     I        Disk number of uv data set.
C      CNO      I        Catalogue number of uv data set.
C      INVER    I        Input SN table version number.
C      CATBLK   I(256)   Catalogue header block of uv data set.
C   Output:
C      IRET     I        Return code, non-zero if an error was detected,
C                        zero otherwise.
C   Scratch:
C      INBUF    I(*)     TABIO buffer for input table.
C      OUTBUF   I(*)     TABIO buffer for output table
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, INVER, CATBLK(256), INBUF(*), OUTBUF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PSNTAB.INC'
      INTEGER   LUN1, LUN2, ISNRNO, KOLS(MAXSNC), NUMV(MAXSNC), NUMANT,
     *   NUMPOL, NUMIF, NUMNOD, IRCODE, RECORD(XCLRSZ), SNKOLS(MAXSNC),
     *   NHCOL, TIMKOL, ANTKOL, SUBKOL, IFQKOL, RE1KOL, IM1KOL, DL1KOL,
     *   RA1KOL, WT1KOL, RE2KOL, IM2KOL, DL2KOL, RA2KOL, WT2KOL, SRCKOL,
     *   NUMREC, OSNRNO, REKOL, IMKOL, DELKOL, RATKOL,WTKOL, POL, IIF,
     *   FSTREC, REC2(XCLRSZ), NRMM, NIMM, NDMM, NTMM
      LOGICAL   ISAPPL
      REAL      GMMOD, RANOD(25), DECNOD(25), RECR(XCLRSZ),
     *   RECR2(XCLRSZ)
      DOUBLE PRECISION RECD(XCLRSZ/2), RECD2(XCLRSZ/2), DTIME
      CHARACTER COLHDR(2)*24
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      EQUIVALENCE (REC2, RECR2, RECD2)
      PARAMETER (LUN1 = 28)
      PARAMETER (LUN2 = 29)
      DATA COLHDR /'TIME ', 'ANTENNA NO. '/
C-----------------------------------------------------------------------
      DTIME = 1.0D0 / (24.0D0 * 3600.0D0 * 20.0D0)
C                                       Reformat table if necessary:
      CALL SNREFM (DISK, CNO, INVER, CATBLK, LUN1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open for read & keywords
      CALL SNINI ('READ', INBUF, DISK, CNO, INVER, CATBLK, LUN1, ISNRNO,
     *   SNKOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD, RANOD,
     *   DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                         Reopen with write permission:
      CALL SNINI ('WRIT', OUTBUF, DISK, CNO, INVER, CATBLK, LUN2,
     *   ISNRNO, SNKOLS, NUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) GO TO 999
      TIMKOL = SNKOLS(SNDTIM)
      ANTKOL = SNKOLS(SNIANT)
      SUBKOL = SNKOLS(SNISUB)
      IFQKOL = SNKOLS(SNIFQI)
      RE1KOL = SNKOLS(SNRRE1)
      IM1KOL = SNKOLS(SNRIM1)
      RA1KOL = SNKOLS(SNRRA1)
      DL1KOL = SNKOLS(SNRDE1)
      WT1KOL = SNKOLS(SNRWE1)
      RE2KOL = SNKOLS(SNRRE2)
      IM2KOL = SNKOLS(SNRIM2)
      RA2KOL = SNKOLS(SNRRA2)
      DL2KOL = SNKOLS(SNRDE2)
      WT2KOL = SNKOLS(SNRWE2)
      SRCKOL = SNKOLS(SNISID)
C                                       Check sort order
      NHCOL = 2
      CALL FNDCOL (NHCOL, COLHDR, 24, .TRUE., INBUF, KOLS, IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((INBUF(43).NE.KOLS(2)) .OR. (INBUF(44).NE.KOLS(1))) THEN
         IRET = 1
         WRITE (MSGTXT, 1000) INVER
         CALL MSGWRT (8)
         END IF
C                                       Read number of records in input
C                                       table:
      NUMREC = INBUF(5)
C                                       Inform user of merging:
      WRITE (MSGTXT, 1001)
      CALL MSGWRT (5)
C                                       Read first record:
      IRCODE = 0
      DO 10 ISNRNO = 1,NUMREC
         CALL TABIO ('READ', IRCODE, ISNRNO, RECORD, INBUF, IRET)
         IF (IRET.GT.0) GO TO 999
         IF (IRET.EQ.0) GO TO 20
 10      CONTINUE
C                                       all flagged!!
      MSGTXT = 'SNMRG: ALL INPUT RECORDS ARE FLAGGED!'
      CALL MSGWRT (8)
      IRET = 4
      GO TO 999
C                                       read the rest
 20   FSTREC = ISNRNO + 1
      OSNRNO = 0
      NRMM = 0
      NIMM = 0
      NDMM = 0
      NTMM = 0
      DO 100 ISNRNO = FSTREC,NUMREC
         CALL TABIO ('READ', IRCODE, ISNRNO, REC2, INBUF, IRET)
         IF (IRET.GT.0) GO TO 999
         IF (IRET.LT.0) GO TO 100
C                                       does not match the active one
C                                       advance output
         IF ((RECORD(ANTKOL).NE.REC2(ANTKOL)) .OR.
     *      (RECORD(SUBKOL).NE.REC2(SUBKOL)) .OR.
     *      (RECORD(SRCKOL).NE.REC2(SRCKOL)) .OR.
     *      (RECORD(IFQKOL).NE.REC2(IFQKOL)) .OR.
     *      (ABS(RECD(TIMKOL)-RECD2(TIMKOL)).GT.DTIME)) THEN
            OSNRNO = OSNRNO + 1
            CALL TABIO ('WRIT', IRCODE, OSNRNO, RECORD, OUTBUF, IRET)
            IF (IRET.GT.0) GO TO 999
            CALL COPY (XCLRSZ, REC2, RECORD)
C                                       Matches - merge
         ELSE
            REKOL  = RE1KOL
            IMKOL  = IM1KOL
            DELKOL = DL1KOL
            RATKOL = RA1KOL
            WTKOL  = WT1KOL
C                                       Process each polarization:
            DO 50 POL = 1,NUMPOL
C                                       Process each IF:
               DO 40 IIF = 1,NUMIF
C                                       check mismatch
                  IF ((RECR(REKOL).NE.FBLANK) .AND.
     *               (RECR2(REKOL).NE.FBLANK) .AND.
     *               (RECR(REKOL).NE.RECR2(REKOL))) NRMM = NRMM + 1
                  IF ((RECR(IMKOL).NE.FBLANK) .AND.
     *               (RECR2(IMKOL).NE.FBLANK) .AND.
     *               (RECR(IMKOL).NE.RECR2(IMKOL))) NIMM = NIMM + 1
                  IF ((RECR(DELKOL).NE.FBLANK) .AND.
     *               (RECR2(DELKOL).NE.FBLANK) .AND.
     *               (RECR(DELKOL).NE.RECR2(DELKOL))) NDMM = NDMM + 1
                  IF ((RECR(RATKOL).NE.FBLANK) .AND.
     *               (RECR2(RATKOL).NE.FBLANK) .AND.
     *               (RECR(RATKOL).NE.RECR2(RATKOL))) NTMM = NTMM + 1
C                                       replace blanks
                  IF (RECR(REKOL).EQ.FBLANK) RECR(REKOL) = RECR2(REKOL)
                  IF (RECR(IMKOL).EQ.FBLANK) RECR(IMKOL) = RECR2(IMKOL)
                  IF (RECR(DELKOL).EQ.FBLANK) RECR(DELKOL) =
     *               RECR2(DELKOL)
                  IF (RECR(RATKOL).EQ.FBLANK) RECR(RATKOL) =
     *               RECR2(RATKOL)
C                                       weights
                  IF (RECR(WTKOL).EQ.FBLANK) RECR(WTKOL) = RECR2(WTKOL)
                  IF ((RECR(WTKOL).NE.FBLANK) .AND.
     *               (RECR2(WTKOL).NE.FBLANK) .AND.
     *               (RECR(WTKOL).NE.RECR2(WTKOL))) RECR(WTKOL) =
     *               MAX (RECR(WTKOL), RECR2(WTKOL))
C                                       Update pointers:
                  WTKOL = WTKOL + 1
                  REKOL = REKOL + 1
                  IMKOL = IMKOL + 1
                  RATKOL = RATKOL + 1
                  DELKOL = DELKOL + 1
 40               CONTINUE
C                                       Update pointers:
               WTKOL = WT2KOL
               REKOL = RE2KOL
               IMKOL = IM2KOL
               RATKOL = RA2KOL
               DELKOL = DL2KOL
 50            CONTINUE
            END IF
 100     CONTINUE
C                                       write last record
      OSNRNO = OSNRNO + 1
      CALL TABIO ('WRIT', IRCODE, OSNRNO, RECORD, OUTBUF, IRET)
      IF (IRET.GT.0) GO TO 999
      OUTBUF(5) = OSNRNO
      WRITE (MSGTXT,1100) OSNRNO, NUMREC
      CALL MSGWRT (5)
C                                       Close tables:
      CALL TABIO ('CLOS', IRCODE, ISNRNO, RECORD, INBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL TABIO ('CLOS', IRCODE, OSNRNO, RECORD, OUTBUF, IRET)
C                                       bad matches?
      IF ((NRMM.GT.0) .OR. (NIMM.GT.0) .OR. (NDMM.GT.0) .OR.
     *   (NTMM.GT.0)) THEN
         MSGTXT = 'There were matching records having unequal values:'
         CALL MSGWRT (8)
         WRITE (MSGTXT,1105) NRMM, NIMM, NDMM, NTMM
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SNMRG: SN TABLE ',I3, ' IS MISSORTED')
 1001 FORMAT ('SNMRG: Merging SN table')
 1100 FORMAT ('SNMRG: Write',I8,' merged records from',I8,
     *   ' input records')
 1105 FORMAT ('SNMRG: REAL, IMAG, DELAY, RATE',4I7)
      END
C ======================================================================
C NIST Guide to Available Math Software.
C Fullsource for module SGESVD from package LAPACK.
C Retrieved from NETLIB on Tue Oct 29 15:40:24 1996.
C ======================================================================
      SUBROUTINE SGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     *                   WORK, LWORK, INFO )
C
C  -- LAPACK driver routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), S( * ), U( LDU, * ),
     *                   VT( LDVT, * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SGESVD computes the singular value decomposition (SVD) of a real
C  M-by-N matrix A, optionally computing the left and/or right singular
C  vectors. The SVD is written
C
C       A = U * SIGMA * transpose(V)
C
C  where SIGMA is an M-by-N matrix which is zero except for its
C  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
C  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
C  are the singular values of A; they are real and non-negative, and
C  are returned in descending order.  The first min(m,n) columns of
C  U and V are the left and right singular vectors of A.
C
C  Note that the routine returns V**T, not V.
C
C  Arguments
C  =========
C
C  JOBU    (input) CHARACTER*1
C          Specifies options for computing all or part of the matrix U:
C          = 'A':  all M columns of U are returned in array U:
C          = 'S':  the first min(m,n) columns of U (the left singular
C                  vectors) are returned in the array U;
C          = 'O':  the first min(m,n) columns of U (the left singular
C                  vectors) are overwritten on the array A;
C          = 'N':  no columns of U (no left singular vectors) are
C                  computed.
C
C  JOBVT   (input) CHARACTER*1
C          Specifies options for computing all or part of the matrix
C          V**T:
C          = 'A':  all N rows of V**T are returned in the array VT;
C          = 'S':  the first min(m,n) rows of V**T (the right singular
C                  vectors) are returned in the array VT;
C          = 'O':  the first min(m,n) rows of V**T (the right singular
C                  vectors) are overwritten on the array A;
C          = 'N':  no rows of V**T (no right singular vectors) are
C                  computed.
C
C          JOBVT and JOBU cannot both be 'O'.
C
C  M       (input) INTEGER
C          The number of rows of the input matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the input matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the M-by-N matrix A.
C          On exit,
C          if JOBU = 'O',  A is overwritten with the first min(m,n)
C                          columns of U (the left singular vectors,
C                          stored columnwise);
C          if JOBVT = 'O', A is overwritten with the first min(m,n)
C                          rows of V**T (the right singular vectors,
C                          stored rowwise);
C          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
C                          are destroyed.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  S       (output) REAL array, dimension (min(M,N))
C          The singular values of A, sorted so that S(i) >= S(i+1).
C
C  U       (output) REAL array, dimension (LDU,UCOL)
C          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
C          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
C          if JOBU = 'S', U contains the first min(m,n) columns of U
C          (the left singular vectors, stored columnwise);
C          if JOBU = 'N' or 'O', U is not referenced.
C
C  LDU     (input) INTEGER
C          The leading dimension of the array U.  LDU >= 1; if
C          JOBU = 'S' or 'A', LDU >= M.
C
C  VT      (output) REAL array, dimension (LDVT,N)
C          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
C          V**T;
C          if JOBVT = 'S', VT contains the first min(m,n) rows of
C          V**T (the right singular vectors, stored rowwise);
C          if JOBVT = 'N' or 'O', VT is not referenced.
C
C  LDVT    (input) INTEGER
C          The leading dimension of the array VT.  LDVT >= 1; if
C          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
C          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
C          superdiagonal elements of an upper bidiagonal matrix B
C          whose diagonal is in S (not necessarily sorted). B
C          satisfies A = U * B * VT, so it has the same singular values
C          as A, and singular vectors related by U and VT.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK. LWORK >= 1.
C          LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)-4).
C          For good performance, LWORK should generally be larger.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit.
C          < 0:  if INFO = -i, the i-th argument had an illegal value.
C          > 0:  if SBDSQR did not converge, INFO specifies how many
C                superdiagonals of an intermediate bidiagonal form B
C                did not converge to zero. See the description of WORK
C                above for details.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS, WNTVA,
     *                   WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IR, ISCL,
     *                   ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
     *                   MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU,
     *                   NRVT, WRKBL
      REAL               ANRM, BIGNUM, EPS, SMLNUM
C     ..
C     .. Local Arrays ..
      REAL               DUM( 1 )
C     ..
C     .. External Subroutines ..
      EXTERNAL           SBDSQR, SGEBRD, SGELQF, SGEMM, SGEQRF, SLACPY,
     *                   SLASCL, SLASET, SORGBR, SORGLQ, SORGQR, SORMBR,
     *                   XERBLA
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      REAL               SLAMCH, SLANGE
      EXTERNAL           LSAME, ILAENV, SLAMCH, SLANGE
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = ILAENV( 6, 'SGESVD', JOBU // JOBVT, M, N, 0, 0 )
      WNTUA = LSAME( JOBU, 'A' )
      WNTUS = LSAME( JOBU, 'S' )
      WNTUAS = WNTUA .OR. WNTUS
      WNTUO = LSAME( JOBU, 'O' )
      WNTUN = LSAME( JOBU, 'N' )
      WNTVA = LSAME( JOBVT, 'A' )
      WNTVS = LSAME( JOBVT, 'S' )
      WNTVAS = WNTVA .OR. WNTVS
      WNTVO = LSAME( JOBVT, 'O' )
      WNTVN = LSAME( JOBVT, 'N' )
      MINWRK = 1
C
      IF( .NOT.( WNTUA .OR. WNTUS .OR. WNTUO .OR. WNTUN ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( WNTVA .OR. WNTVS .OR. WNTVO .OR. WNTVN ) .OR.
     *         ( WNTVO .AND. WNTUO ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDU.LT.1 .OR. ( WNTUAS .AND. LDU.LT.M ) ) THEN
         INFO = -9
      ELSE IF( LDVT.LT.1 .OR. ( WNTVA .AND. LDVT.LT.N ) .OR.
     *         ( WNTVS .AND. LDVT.LT.MINMN ) ) THEN
         INFO = -11
      END IF
C
C     Compute workspace
C      (Note: Comments in the code beginning "Workspace:" describe the
C       minimal amount of workspace needed at that point in the code,
C       as well as the preferred amount for good performance.
C       NB refers to the optimal block size for the immediately
C       following subroutine, as returned by ILAENV.)
C
      IF( INFO.EQ.0 .AND. LWORK.GE.1 .AND. M.GT.0 .AND. N.GT.0 ) THEN
         IF( M.GE.N ) THEN
C
C           Compute space needed for SBDSQR
C
            BDSPAC = MAX( 3*N, 5*N-4 )
            IF( M.GE.MNTHR ) THEN
               IF( WNTUN ) THEN
C
C                 Path 1 (M much larger than N, JOBU='N')
C
                  MAXWRK = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1,
     *                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*N+2*N*
     *                     ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  IF( WNTVO .OR. WNTVAS )
     *               MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     *                        ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVN ) THEN
C
C                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'SORGQR', ' ', M,
     *                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVAS ) THEN
C
C                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
C                 'A')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'SORGQR', ' ', M,
     *                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVN ) THEN
C
C                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'SORGQR', ' ', M,
     *                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVO ) THEN
C
C                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'SORGQR', ' ', M,
     *                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVAS ) THEN
C
C                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
C                 'A')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'SORGQR', ' ', M,
     *                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVN ) THEN
C
C                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'SORGQR', ' ', M,
     *                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVO ) THEN
C
C                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'SORGQR', ' ', M,
     *                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVAS ) THEN
C
C                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
C                 'A')
C
                  WRKBL = N + N*ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'SORGQR', ' ', M,
     *                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     *                    ILAENV( 1, 'SGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     *                    ILAENV( 1, 'SORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
C
C              Path 10 (M at least N, but not much larger)
C
               MAXWRK = 3*N + ( M+N )*ILAENV( 1, 'SGEBRD', ' ', M, N,
     *                  -1, -1 )
               IF( WNTUS .OR. WNTUO )
     *            MAXWRK = MAX( MAXWRK, 3*N+N*
     *                     ILAENV( 1, 'SORGBR', 'Q', M, N, N, -1 ) )
               IF( WNTUA )
     *            MAXWRK = MAX( MAXWRK, 3*N+M*
     *                     ILAENV( 1, 'SORGBR', 'Q', M, M, N, -1 ) )
               IF( .NOT.WNTVN )
     *            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     *                     ILAENV( 1, 'SORGBR', 'P', N, N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*N+M, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         ELSE
C
C           Compute space needed for SBDSQR
C
            BDSPAC = MAX( 3*M, 5*M-4 )
            IF( N.GE.MNTHR ) THEN
               IF( WNTVN ) THEN
C
C                 Path 1t(N much larger than M, JOBVT='N')
C
                  MAXWRK = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1,
     *                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*M+2*M*
     *                     ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  IF( WNTUO .OR. WNTUAS )
     *               MAXWRK = MAX( MAXWRK, 3*M+M*
     *                        ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUN ) THEN
C
C                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'SORGLQ', ' ', M,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUAS ) THEN
C
C                 Path 3t(N much larger than M, JOBU='S' or 'A',
C                 JOBVT='O')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'SORGLQ', ' ', M,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     *                    ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUN ) THEN
C
C                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'SORGLQ', ' ', M,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUO ) THEN
C
C                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'SORGLQ', ' ', M,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     *                    ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUAS ) THEN
C
C                 Path 6t(N much larger than M, JOBU='S' or 'A',
C                 JOBVT='S')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'SORGLQ', ' ', M,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     *                    ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUN ) THEN
C
C                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'SORGLQ', ' ', N,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUO ) THEN
C
C                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'SORGLQ', ' ', N,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     *                    ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUAS ) THEN
C
C                 Path 9t(N much larger than M, JOBU='S' or 'A',
C                 JOBVT='A')
C
                  WRKBL = M + M*ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'SORGLQ', ' ', N,
     *                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     *                    ILAENV( 1, 'SGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     *                    ILAENV( 1, 'SORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     *                    ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
C
C              Path 10t(N greater than M, but not much larger)
C
               MAXWRK = 3*M + ( M+N )*ILAENV( 1, 'SGEBRD', ' ', M, N,
     *                  -1, -1 )
               IF( WNTVS .OR. WNTVO )
     *            MAXWRK = MAX( MAXWRK, 3*M+M*
     *                     ILAENV( 1, 'SORGBR', 'P', M, N, M, -1 ) )
               IF( WNTVA )
     *            MAXWRK = MAX( MAXWRK, 3*M+N*
     *                     ILAENV( 1, 'SORGBR', 'P', N, N, M, -1 ) )
               IF( .NOT.WNTUN )
     *            MAXWRK = MAX( MAXWRK, 3*M+( M-1 )*
     *                     ILAENV( 1, 'SORGBR', 'Q', M, M, M, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*M+N, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
      END IF
C
      IF( LWORK.LT.MINWRK ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGESVD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         IF( LWORK.GE.1 )
     *      WORK( 1 ) = ONE
         RETURN
      END IF
C
C     Get machine constants
C
      EPS = SLAMCH( 'P' )
      SMLNUM = SQRT( SLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
C
C     Scale A if max element outside range [SMLNUM,BIGNUM]
C
      ANRM = SLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL SLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL SLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF
C
      IF( M.GE.N ) THEN
C
C        A has at least as many rows as columns. If A has sufficiently
C        more rows than columns, first reduce using the QR
C        decomposition (if sufficient workspace available)
C
         IF( M.GE.MNTHR ) THEN
C
            IF( WNTUN ) THEN
C
C              Path 1 (M much larger than N, JOBU='N')
C              No left singular vectors to be computed
C
               ITAU = 1
               IWORK = ITAU + N
C
C              Compute A=Q*R
C              (Workspace: need 2*N, prefer N+N*NB)
C
               CALL SGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     *                      LWORK-IWORK+1, IERR )
C
C              Zero out below R
C
               CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
C
C              Bidiagonalize R in A
C              (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
               CALL SGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     *                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     *                      IERR )
               NCVT = 0
               IF( WNTVO .OR. WNTVAS ) THEN
C
C                 If right singular vectors desired, generate P'.
C                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                  CALL SORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     *                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               END IF
               IWORK = IE + N
C
C              Perform bidiagonal QR iteration, computing right
C              singular vectors of A in A if desired
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'U', N, NCVT, 0, 0, S, WORK( IE ), A, LDA,
     *                      DUM, 1, DUM, 1, WORK( IWORK ), INFO )
C
C              If right singular vectors desired in VT, copy them there
C
               IF( WNTVAS )
     *            CALL SLACPY( 'F', N, N, A, LDA, VT, LDVT )
C
            ELSE IF( WNTUO .AND. WNTVN ) THEN
C
C              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
C              N left singular vectors to be overwritten on A and
C              no right singular vectors to be computed
C
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
C
C                 Sufficient workspace for a fast algorithm
C
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
C
C                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
C
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
C
C                    WORK(IU) is LDA by N, WORK(IR) is N by N
C
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
C
C                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
C
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
C
C                 Compute A=Q*R
C                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                  CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     *                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy R to WORK(IR) and zero out below it
C
                  CALL SLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     *                         LDWRKR )
C
C                 Generate Q in A
C                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                  CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     *                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
C
C                 Bidiagonalize R in WORK(IR)
C                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                  CALL SGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     *                         WORK( ITAUQ ), WORK( ITAUP ),
     *                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Generate left vectors bidiagonalizing R
C                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                  CALL SORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + N
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of R in WORK(IR)
C                 (Workspace: need N*N+BDSPAC)
C
                  CALL SBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, 1,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
C
C                 Multiply Q in A by left singular vectors of R in
C                 WORK(IR), storing result in WORK(IU) and copying to A
C                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
C
                  DO 10 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL SGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL SLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   10             CONTINUE
C
               ELSE
C
C                 Insufficient workspace for a fast algorithm
C
                  IE = 1
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
C
C                 Bidiagonalize A
C                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
C
                  CALL SGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Generate left vectors bidiagonalizing A
C                 (Workspace: need 4*N, prefer 3*N+N*NB)
C
                  CALL SORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of A in A
C                 (Workspace: need BDSPAC)
C
                  CALL SBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, 1,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
C
               END IF
C
            ELSE IF( WNTUO .AND. WNTVAS ) THEN
C
C              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
C              N left singular vectors to be overwritten on A and
C              N right singular vectors to be computed in VT
C
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
C
C                 Sufficient workspace for a fast algorithm
C
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
C
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is N by N
C
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
C
C                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
C
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
C
C                 Compute A=Q*R
C                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                  CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy R to VT, zeroing out below it
C
                  CALL SLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
C
C                 Generate Q in A
C                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                  CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
C
C                 Bidiagonalize R in VT, copying result to WORK(IR)
C                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                  CALL SGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL SLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
C
C                 Generate left vectors bidiagonalizing R in WORK(IR)
C                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                  CALL SORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
C
C                 Generate right vectors bidiagonalizing R in VT
C                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
C
                  CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of R in WORK(IR) and computing right
C                 singular vectors of R in VT
C                 (Workspace: need N*N+BDSPAC)
C
                  CALL SBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, LDVT,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
C
C                 Multiply Q in A by left singular vectors of R in
C                 WORK(IR), storing result in WORK(IU) and copying to A
C                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
C
                  DO 20 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL SGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL SLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
C
               ELSE
C
C                 Insufficient workspace for a fast algorithm
C
                  ITAU = 1
                  IWORK = ITAU + N
C
C                 Compute A=Q*R
C                 (Workspace: need 2*N, prefer N+N*NB)
C
                  CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy R to VT, zeroing out below it
C
                  CALL SLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
C
C                 Generate Q in A
C                 (Workspace: need 2*N, prefer N+N*NB)
C
                  CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
C
C                 Bidiagonalize R in VT
C                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                  CALL SGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Multiply Q in A by left vectors bidiagonalizing R
C                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                  CALL SORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
C
C                 Generate right vectors bidiagonalizing R in VT
C                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                  CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of A in A and computing right
C                 singular vectors of A in VT
C                 (Workspace: need BDSPAC)
C
                  CALL SBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
C
               END IF
C
            ELSE IF( WNTUS ) THEN
C
               IF( WNTVN ) THEN
C
C                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
C                 N left singular vectors to be computed in U and
C                 no right singular vectors to be computed
C
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
C
C                       WORK(IR) is LDA by N
C
                        LDWRKR = LDA
                     ELSE
C
C                       WORK(IR) is N by N
C
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to WORK(IR), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
C
C                    Generate Q in A
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IR)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left vectors bidiagonalizing R in WORK(IR)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IR)
C                    (Workspace: need N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply Q in A by left singular vectors of R in
C                    WORK(IR), storing result in U
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IR ), LDWRKR, ZERO, U, LDU )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Zero out below R in A
C
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
C
C                    Bidiagonalize R in A
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left vectors bidiagonalizing R
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTVO ) THEN
C
C                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
C                 N left singular vectors to be computed in U and
C                 N right singular vectors to be overwritten on A
C
                  IF( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
C
C                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
C
C                       WORK(IU) is LDA by N and WORK(IR) is N by N
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
C
C                       WORK(IU) is N by N and WORK(IR) is N by N
C
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R
C                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to WORK(IU), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
C
C                    Generate Q in A
C                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
C
                     CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IU), copying result to
C                    WORK(IR)
C                    (Workspace: need 2*N*N+4*N,
C                                prefer 2*N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
C
C                    Generate left bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need 2*N*N+4*N-1,
C                                prefer 2*N*N+3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IU) and computing
C                    right singular vectors of R in WORK(IR)
C                    (Workspace: need 2*N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
C
C                    Multiply Q in A by left singular vectors of R in
C                    WORK(IU), storing result in U
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
C
C                    Copy right singular vectors of R to A
C                    (Workspace: need N*N)
C
                     CALL SLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Zero out below R in A
C
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
C
C                    Bidiagonalize R in A
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left vectors bidiagonalizing R
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right vectors bidiagonalizing R in A
C                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in A
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTVAS ) THEN
C
C                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
C                         or 'A')
C                 N left singular vectors to be computed in U and
C                 N right singular vectors to be computed in VT
C
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
C
C                       WORK(IU) is LDA by N
C
                        LDWRKU = LDA
                     ELSE
C
C                       WORK(IU) is N by N
C
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to WORK(IU), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
C
C                    Generate Q in A
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IU), copying result to VT
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
C
C                    Generate left bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in VT
C                    (Workspace: need N*N+4*N-1,
C                                prefer N*N+3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IU) and computing
C                    right singular vectors of R in VT
C                    (Workspace: need N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply Q in A by left singular vectors of R in
C                    WORK(IU), storing result in U
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to VT, zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in VT
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left bidiagonalizing vectors
C                    in VT
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in VT
C                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               END IF
C
            ELSE IF( WNTUA ) THEN
C
               IF( WNTVN ) THEN
C
C                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
C                 M left singular vectors to be computed in U and
C                 no right singular vectors to be computed
C
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
C
C                       WORK(IR) is LDA by N
C
                        LDWRKR = LDA
                     ELSE
C
C                       WORK(IR) is N by N
C
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Copy R to WORK(IR), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
C
C                    Generate Q in U
C                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IR)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IR)
C                    (Workspace: need N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply Q in U by left singular vectors of R in
C                    WORK(IR), storing result in A
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IR ), LDWRKR, ZERO, A, LDA )
C
C                    Copy left singular vectors of A from A to U
C
                     CALL SLACPY( 'F', M, N, A, LDA, U, LDU )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need N+M, prefer N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Zero out below R in A
C
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
C
C                    Bidiagonalize R in A
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left bidiagonalizing vectors
C                    in A
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTVO ) THEN
C
C                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
C                 M left singular vectors to be computed in U and
C                 N right singular vectors to be overwritten on A
C
                  IF( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
C
C                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
C
C                       WORK(IU) is LDA by N and WORK(IR) is N by N
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
C
C                       WORK(IU) is N by N and WORK(IR) is N by N
C
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to WORK(IU), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IU), copying result to
C                    WORK(IR)
C                    (Workspace: need 2*N*N+4*N,
C                                prefer 2*N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
C
C                    Generate left bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need 2*N*N+4*N-1,
C                                prefer 2*N*N+3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IU) and computing
C                    right singular vectors of R in WORK(IR)
C                    (Workspace: need 2*N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
C
C                    Multiply Q in U by left singular vectors of R in
C                    WORK(IU), storing result in A
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
C
C                    Copy left singular vectors of A from A to U
C
                     CALL SLACPY( 'F', M, N, A, LDA, U, LDU )
C
C                    Copy right singular vectors of R from WORK(IR) to A
C
                     CALL SLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need N+M, prefer N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Zero out below R in A
C
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
C
C                    Bidiagonalize R in A
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left bidiagonalizing vectors
C                    in A
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in A
C                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in A
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTVAS ) THEN
C
C                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
C                         or 'A')
C                 M left singular vectors to be computed in U and
C                 N right singular vectors to be computed in VT
C
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
C
C                       WORK(IU) is LDA by N
C
                        LDWRKU = LDA
                     ELSE
C
C                       WORK(IU) is N by N
C
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R to WORK(IU), zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in WORK(IU), copying result to VT
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
C
C                    Generate left bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
C
                     CALL SORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in VT
C                    (Workspace: need N*N+4*N-1,
C                                prefer N*N+3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of R in WORK(IU) and computing
C                    right singular vectors of R in VT
C                    (Workspace: need N*N+BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply Q in U by left singular vectors of R in
C                    WORK(IU), storing result in A
C                    (Workspace: need N*N)
C
                     CALL SGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
C
C                    Copy left singular vectors of A from A to U
C
                     CALL SLACPY( 'F', M, N, A, LDA, U, LDU )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + N
C
C                    Compute A=Q*R, copying result to U
C                    (Workspace: need 2*N, prefer N+N*NB)
C
                     CALL SGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
C
C                    Generate Q in U
C                    (Workspace: need N+M, prefer N+M*NB)
C
                     CALL SORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy R from A to VT, zeroing out below it
C
                     CALL SLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL SLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
C
C                    Bidiagonalize R in VT
C                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
C
                     CALL SGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply Q in U by left bidiagonalizing vectors
C                    in VT
C                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
C
                     CALL SORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in VT
C                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
                     CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               END IF
C
            END IF
C
         ELSE
C
C           M .LT. MNTHR
C
C           Path 10 (M at least N, but not much larger)
C           Reduce to bidiagonal form without QR decomposition
C
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
C
C           Bidiagonalize A
C           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
C
            CALL SGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
C
C              If left singular vectors desired in U, copy result to U
C              and generate left bidiagonalizing vectors in U
C              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
C
               CALL SLACPY( 'L', M, N, A, LDA, U, LDU )
               IF( WNTUS )
     $            NCU = N
               IF( WNTUA )
     $            NCU = M
               CALL SORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
C
C              If right singular vectors desired in VT, copy result to
C              VT and generate right bidiagonalizing vectors in VT
C              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
               CALL SLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL SORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
C
C              If left singular vectors desired in A, generate left
C              bidiagonalizing vectors in A
C              (Workspace: need 4*N, prefer 3*N+N*NB)
C
               CALL SORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
C
C              If right singular vectors desired in A, generate right
C              bidiagonalizing vectors in A
C              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
C
               CALL SORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + N
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in U and computing right singular
C              vectors in VT
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in U and computing right singular
C              vectors in A
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in A and computing right singular
C              vectors in VT
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
C
         END IF
C
      ELSE
C
C        A has more columns than rows. If A has sufficiently more
C        columns than rows, first reduce using the LQ decomposition (if
C        sufficient workspace available)
C
         IF( N.GE.MNTHR ) THEN
C
            IF( WNTVN ) THEN
C
C              Path 1t(N much larger than M, JOBVT='N')
C              No right singular vectors to be computed
C
               ITAU = 1
               IWORK = ITAU + M
C
C              Compute A=L*Q
C              (Workspace: need 2*M, prefer M+M*NB)
C
               CALL SGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
C
C              Zero out above L
C
               CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
C
C              Bidiagonalize L in A
C              (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
               CALL SGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               IF( WNTUO .OR. WNTUAS ) THEN
C
C                 If left singular vectors desired, generate Q
C                 (Workspace: need 4*M, prefer 3*M+M*NB)
C
                  CALL SORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
               END IF
               IWORK = IE + M
               NRU = 0
               IF( WNTUO .OR. WNTUAS )
     $            NRU = M
C
C              Perform bidiagonal QR iteration, computing left singular
C              vectors of A in A if desired
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A,
     $                      LDA, DUM, 1, WORK( IWORK ), INFO )
C
C              If left singular vectors desired in U, copy them there
C
               IF( WNTUAS )
     $            CALL SLACPY( 'F', M, M, A, LDA, U, LDU )
C
            ELSE IF( WNTVO .AND. WNTUN ) THEN
C
C              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
C              M right singular vectors to be overwritten on A and
C              no left singular vectors to be computed
C
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
C
C                 Sufficient workspace for a fast algorithm
C
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
C
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is M by M
C
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
C
C                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
C
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
C
C                 Compute A=L*Q
C                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                  CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy L to WORK(IR) and zero out above it
C
                  CALL SLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                         WORK( IR+LDWRKR ), LDWRKR )
C
C                 Generate Q in A
C                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                  CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
C
C                 Bidiagonalize L in WORK(IR)
C                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                  CALL SGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Generate right vectors bidiagonalizing L
C                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
C
                  CALL SORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + M
C
C                 Perform bidiagonal QR iteration, computing right
C                 singular vectors of L in WORK(IR)
C                 (Workspace: need M*M+BDSPAC)
C
                  CALL SBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
C
C                 Multiply right singular vectors of L in WORK(IR) by Q
C                 in A, storing result in WORK(IU) and copying to A
C                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
C
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL SGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL SLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   30             CONTINUE
C
               ELSE
C
C                 Insufficient workspace for a fast algorithm
C
                  IE = 1
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
C
C                 Bidiagonalize A
C                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
C
                  CALL SGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Generate right vectors bidiagonalizing A
C                 (Workspace: need 4*M, prefer 3*M+M*NB)
C
                  CALL SORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
C
C                 Perform bidiagonal QR iteration, computing right
C                 singular vectors of A in A
C                 (Workspace: need BDSPAC)
C
                  CALL SBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA,
     $                         DUM, 1, DUM, 1, WORK( IWORK ), INFO )
C
               END IF
C
            ELSE IF( WNTVO .AND. WNTUAS ) THEN
C
C              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
C              M right singular vectors to be overwritten on A and
C              M left singular vectors to be computed in U
C
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
C
C                 Sufficient workspace for a fast algorithm
C
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
C
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
C
C                    WORK(IU) is LDA by N and WORK(IR) is M by M
C
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
C
C                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
C
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
C
C                 Compute A=L*Q
C                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                  CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy L to U, zeroing about above it
C
                  CALL SLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
C
C                 Generate Q in A
C                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                  CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
C
C                 Bidiagonalize L in U, copying result to WORK(IR)
C                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                  CALL SGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL SLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
C
C                 Generate right vectors bidiagonalizing L in WORK(IR)
C                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
C
                  CALL SORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
C
C                 Generate left vectors bidiagonalizing L in U
C                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
C
                  CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of L in U, and computing right
C                 singular vectors of L in WORK(IR)
C                 (Workspace: need M*M+BDSPAC)
C
                  CALL SBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, U, LDU, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
C
C                 Multiply right singular vectors of L in WORK(IR) by Q
C                 in A, storing result in WORK(IU) and copying to A
C                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
C
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL SGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL SLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   40             CONTINUE
C
               ELSE
C
C                 Insufficient workspace for a fast algorithm
C
                  ITAU = 1
                  IWORK = ITAU + M
C
C                 Compute A=L*Q
C                 (Workspace: need 2*M, prefer M+M*NB)
C
                  CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Copy L to U, zeroing out above it
C
                  CALL SLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
C
C                 Generate Q in A
C                 (Workspace: need 2*M, prefer M+M*NB)
C
                  CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
C
C                 Bidiagonalize L in U
C                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                  CALL SGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                 Multiply right vectors bidiagonalizing L by Q in A
C                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                  CALL SORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
C
C                 Generate left vectors bidiagonalizing L in U
C                 (Workspace: need 4*M, prefer 3*M+M*NB)
C
                  CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
C
C                 Perform bidiagonal QR iteration, computing left
C                 singular vectors of A in U and computing right
C                 singular vectors of A in A
C                 (Workspace: need BDSPAC)
C
                  CALL SBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA,
     $                         U, LDU, DUM, 1, WORK( IWORK ), INFO )
C
               END IF
C
            ELSE IF( WNTVS ) THEN
C
               IF( WNTUN ) THEN
C
C                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
C                 M right singular vectors to be computed in VT and
C                 no left singular vectors to be computed
C
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
C
C                       WORK(IR) is LDA by M
C
                        LDWRKR = LDA
                     ELSE
C
C                       WORK(IR) is M by M
C
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to WORK(IR), zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
C
C                    Generate Q in A
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IR)
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right vectors bidiagonalizing L in
C                    WORK(IR)
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing right
C                    singular vectors of L in WORK(IR)
C                    (Workspace: need M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IR) by
C                    Q in A, storing result in VT
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, A, LDA, ZERO, VT, LDVT )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy result to VT
C
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Zero out above L in A
C
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
C
C                    Bidiagonalize L in A
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right vectors bidiagonalizing L by Q in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTUO ) THEN
C
C                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
C                 M right singular vectors to be computed in VT and
C                 M left singular vectors to be overwritten on A
C
                  IF( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
C
C                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
C
C                       WORK(IU) is LDA by M and WORK(IR) is M by M
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
C
C                       WORK(IU) is M by M and WORK(IR) is M by M
C
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q
C                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to WORK(IU), zeroing out below it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
C
C                    Generate Q in A
C                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
C
                     CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IU), copying result to
C                    WORK(IR)
C                    (Workspace: need 2*M*M+4*M,
C                                prefer 2*M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
C
C                    Generate right bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need 2*M*M+4*M-1,
C                                prefer 2*M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of L in WORK(IR) and computing
C                    right singular vectors of L in WORK(IU)
C                    (Workspace: need 2*M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IU) by
C                    Q in A, storing result in VT
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
C
C                    Copy left singular vectors of L to A
C                    (Workspace: need M*M)
C
                     CALL SLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Zero out above L in A
C
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
C
C                    Bidiagonalize L in A
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right vectors bidiagonalizing L by Q in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors of L in A
C                    (Workspace: need 4*M, prefer 3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, compute left
C                    singular vectors of A in A and compute right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTUAS ) THEN
C
C                 Path 6t(N much larger than M, JOBU='S' or 'A',
C                         JOBVT='S')
C                 M right singular vectors to be computed in VT and
C                 M left singular vectors to be computed in U
C
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
C
C                       WORK(IU) is LDA by N
C
                        LDWRKU = LDA
                     ELSE
C
C                       WORK(IU) is LDA by M
C
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to WORK(IU), zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
C
C                    Generate Q in A
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IU), copying result to U
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
C
C                    Generate right bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need M*M+4*M-1,
C                                prefer M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in U
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of L in U and computing right
C                    singular vectors of L in WORK(IU)
C                    (Workspace: need M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IU) by
C                    Q in A, storing result in VT
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to U, zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in U
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right bidiagonalizing vectors in U by Q
C                    in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in U
C                    (Workspace: need 4*M, prefer 3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               END IF
C
            ELSE IF( WNTVA ) THEN
C
               IF( WNTUN ) THEN
C
C                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
C                 N right singular vectors to be computed in VT and
C                 no left singular vectors to be computed
C
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
C
C                       WORK(IR) is LDA by M
C
                        LDWRKR = LDA
                     ELSE
C
C                       WORK(IR) is M by M
C
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Copy L to WORK(IR), zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
C
C                    Generate Q in VT
C                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IR)
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate right bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need M*M+4*M-1,
C                                prefer M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing right
C                    singular vectors of L in WORK(IR)
C                    (Workspace: need M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IR) by
C                    Q in VT, storing result in A
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, VT, LDVT, ZERO, A, LDA )
C
C                    Copy right singular vectors of A from A to VT
C
                     CALL SLACPY( 'F', M, N, A, LDA, VT, LDVT )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need M+N, prefer M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Zero out above L in A
C
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
C
C                    Bidiagonalize L in A
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right bidiagonalizing vectors in A by Q
C                    in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTUO ) THEN
C
C                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
C                 N right singular vectors to be computed in VT and
C                 M left singular vectors to be overwritten on A
C
                  IF( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
C
C                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
C
C                       WORK(IU) is LDA by M and WORK(IR) is M by M
C
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
C
C                       WORK(IU) is M by M and WORK(IR) is M by M
C
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to WORK(IU), zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IU), copying result to
C                    WORK(IR)
C                    (Workspace: need 2*M*M+4*M,
C                                prefer 2*M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
C
C                    Generate right bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need 2*M*M+4*M-1,
C                                prefer 2*M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in WORK(IR)
C                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of L in WORK(IR) and computing
C                    right singular vectors of L in WORK(IU)
C                    (Workspace: need 2*M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IU) by
C                    Q in VT, storing result in A
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
C
C                    Copy right singular vectors of A from A to VT
C
                     CALL SLACPY( 'F', M, N, A, LDA, VT, LDVT )
C
C                    Copy left singular vectors of A from WORK(IR) to A
C
                     CALL SLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need M+N, prefer M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Zero out above L in A
C
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
C
C                    Bidiagonalize L in A
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right bidiagonalizing vectors in A by Q
C                    in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in A
C                    (Workspace: need 4*M, prefer 3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in A and computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               ELSE IF( WNTUAS ) THEN
C
C                 Path 9t(N much larger than M, JOBU='S' or 'A',
C                         JOBVT='A')
C                 N right singular vectors to be computed in VT and
C                 M left singular vectors to be computed in U
C
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
C
C                    Sufficient workspace for a fast algorithm
C
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
C
C                       WORK(IU) is LDA by M
C
                        LDWRKU = LDA
                     ELSE
C
C                       WORK(IU) is M by M
C
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to WORK(IU), zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in WORK(IU), copying result to U
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
C
C                    Generate right bidiagonalizing vectors in WORK(IU)
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
C
                     CALL SORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in U
C                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of L in U and computing right
C                    singular vectors of L in WORK(IU)
C                    (Workspace: need M*M+BDSPAC)
C
                     CALL SBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
C
C                    Multiply right singular vectors of L in WORK(IU) by
C                    Q in VT, storing result in A
C                    (Workspace: need M*M)
C
                     CALL SGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
C
C                    Copy right singular vectors of A from A to VT
C
                     CALL SLACPY( 'F', M, N, A, LDA, VT, LDVT )
C
                  ELSE
C
C                    Insufficient workspace for a fast algorithm
C
                     ITAU = 1
                     IWORK = ITAU + M
C
C                    Compute A=L*Q, copying result to VT
C                    (Workspace: need 2*M, prefer M+M*NB)
C
                     CALL SGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
C
C                    Generate Q in VT
C                    (Workspace: need M+N, prefer M+N*NB)
C
                     CALL SORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Copy L to U, zeroing out above it
C
                     CALL SLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL SLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
C
C                    Bidiagonalize L in U
C                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
C
                     CALL SGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Multiply right bidiagonalizing vectors in U by Q
C                    in VT
C                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
C
                     CALL SORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
C
C                    Generate left bidiagonalizing vectors in U
C                    (Workspace: need 4*M, prefer 3*M+M*NB)
C
                     CALL SORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
C
C                    Perform bidiagonal QR iteration, computing left
C                    singular vectors of A in U and computing right
C                    singular vectors of A in VT
C                    (Workspace: need BDSPAC)
C
                     CALL SBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
C
                  END IF
C
               END IF
C
            END IF
C
         ELSE
C
C           N .LT. MNTHR
C
C           Path 10t(N greater than M, but not much larger)
C           Reduce to bidiagonal form without LQ decomposition
C
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
C
C           Bidiagonalize A
C           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
C
            CALL SGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
C
C              If left singular vectors desired in U, copy result to U
C              and generate left bidiagonalizing vectors in U
C              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
C
               CALL SLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL SORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
C
C              If right singular vectors desired in VT, copy result to
C              VT and generate right bidiagonalizing vectors in VT
C              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
C
               CALL SLACPY( 'U', M, N, A, LDA, VT, LDVT )
               IF( WNTVA )
     $            NRVT = N
               IF( WNTVS )
     $            NRVT = M
               CALL SORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
C
C              If left singular vectors desired in A, generate left
C              bidiagonalizing vectors in A
C              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
C
               CALL SORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
C
C              If right singular vectors desired in A, generate right
C              bidiagonalizing vectors in A
C              (Workspace: need 4*M, prefer 3*M+M*NB)
C
               CALL SORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + M
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in U and computing right singular
C              vectors in VT
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in U and computing right singular
C              vectors in A
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
C
C              Perform bidiagonal QR iteration, if desired, computing
C              left singular vectors in A and computing right singular
C              vectors in VT
C              (Workspace: need BDSPAC)
C
               CALL SBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
C
         END IF
C
      END IF
C
C     If SBDSQR failed to converge, copy unconverged superdiagonals
C     to WORK( 2:MINMN )
C
      IF( INFO.NE.0 ) THEN
         IF( IE.GT.2 ) THEN
            DO 50 I = 1, MINMN - 1
               WORK( I+1 ) = WORK( I+IE-1 )
   50       CONTINUE
         END IF
         IF( IE.LT.2 ) THEN
            DO 60 I = MINMN - 1, 1, -1
               WORK( I+1 ) = WORK( I+IE-1 )
   60       CONTINUE
         END IF
      END IF
C
C     Undo scaling if necessary
C
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL SLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
     $      CALL SLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL SLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
     $      CALL SLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
      END IF
C
C     Return optimal workspace in WORK(1)
C
      WORK( 1 ) = MAXWRK
C
      RETURN
C
C     End of SGESVD
C
      END
      SUBROUTINE SBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
     $                   LDU, C, LDC, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
C     ..
C     .. Array Arguments ..
      REAL               C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SBDSQR computes the singular value decomposition (SVD) of a real
C  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
C  denotes the transpose of P), where S is a diagonal matrix with
C  non-negative diagonal elements (the singular values of B), and Q
C  and P are orthogonal matrices.
C
C  The routine computes S, and optionally computes U * Q, P' * VT,
C  or Q' * C, for given real input matrices U, VT, and C.
C
C  See "Computing  Small Singular Values of Bidiagonal Matrices With
C  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
C  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
C  no. 5, pp. 873-912, Sept 1990) and
C  "Accurate singular values and differential qd algorithms," by
C  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
C  Department, University of California at Berkeley, July 1992
C  for a detailed description of the algorithm.
C
C  Arguments
C  =========
C
C  UPLO    (input) CHARACTER*1
C          = 'U':  B is upper bidiagonal;
C          = 'L':  B is lower bidiagonal.
C
C  N       (input) INTEGER
C          The order of the matrix B.  N >= 0.
C
C  NCVT    (input) INTEGER
C          The number of columns of the matrix VT. NCVT >= 0.
C
C  NRU     (input) INTEGER
C          The number of rows of the matrix U. NRU >= 0.
C
C  NCC     (input) INTEGER
C          The number of columns of the matrix C. NCC >= 0.
C
C  D       (input/output) REAL array, dimension (N)
C          On entry, the n diagonal elements of the bidiagonal matrix B.
C          On exit, if INFO=0, the singular values of B in decreasing
C          order.
C
C  E       (input/output) REAL array, dimension (N)
C          On entry, the elements of E contain the
C          offdiagonal elements of the bidiagonal matrix whose SVD
C          is desired. On normal exit (INFO = 0), E is destroyed.
C          If the algorithm does not converge (INFO > 0), D and E
C          will contain the diagonal and superdiagonal elements of a
C          bidiagonal matrix orthogonally equivalent to the one given
C          as input. E(N) is used for workspace.
C
C  VT      (input/output) REAL array, dimension (LDVT, NCVT)
C          On entry, an N-by-NCVT matrix VT.
C          On exit, VT is overwritten by P' * VT.
C          VT is not referenced if NCVT = 0.
C
C  LDVT    (input) INTEGER
C          The leading dimension of the array VT.
C          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
C
C  U       (input/output) REAL array, dimension (LDU, N)
C          On entry, an NRU-by-N matrix U.
C          On exit, U is overwritten by U * Q.
C          U is not referenced if NRU = 0.
C
C  LDU     (input) INTEGER
C          The leading dimension of the array U.  LDU >= max(1,NRU).
C
C  C       (input/output) REAL array, dimension (LDC, NCC)
C          On entry, an N-by-NCC matrix C.
C          On exit, C is overwritten by Q' * C.
C          C is not referenced if NCC = 0.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C.
C          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
C
C  WORK    (workspace) REAL array, dimension
C            2*N  if only singular values wanted (NCVT = NRU = NCC = 0)
C            max( 1, 4*N-4 ) otherwise
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  If INFO = -i, the i-th argument had an illegal value
C          > 0:  the algorithm did not converge; D and E contain the
C                elements of a bidiagonal matrix which is orthogonally
C                similar to the input matrix B;  if INFO = i, i
C                elements of E have not converged to zero.
C
C  Internal Parameters
C  ===================
C
C  TOLMUL  REAL, default = max(10,min(100,EPS**(-1/8)))
C          TOLMUL controls the convergence criterion of the QR loop.
C          If it is positive, TOLMUL*EPS is the desired relative
C             precision in the computed singular values.
C          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
C             desired absolute accuracy in the computed singular
C             values (corresponds to relative accuracy
C             abs(TOLMUL*EPS) in the largest singular value.
C          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
C             between 10 (for fast convergence) and .1/EPS
C             (for there to be some accuracy in the results).
C          Default is to lose at either one eighth or 2 of the
C             available decimal digits in each computed singular value
C             (whichever is smaller).
C
C  MAXITR  INTEGER, default = 6
C          MAXITR controls the maximum number of passes of the
C          algorithm through its inner loop. The algorithms stops
C          (and so fails to converge) if the number of passes
C          through the inner loop exceeds MAXITR*N**2.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               NEGONE
      PARAMETER          ( NEGONE = -1.0E0 )
      REAL               HNDRTH
      PARAMETER          ( HNDRTH = 0.01E0 )
      REAL               TEN
      PARAMETER          ( TEN = 10.0E0 )
      REAL               HNDRD
      PARAMETER          ( HNDRD = 100.0E0 )
      REAL               MEIGTH
      PARAMETER          ( MEIGTH = -0.125E0 )
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 6 )
C     ..
C     .. Local Scalars ..
      LOGICAL            ROTATE
      INTEGER            I, IDIR, IROT, ISUB, ITER, IUPLO, J, LL, LLL,
     $                   M, MAXIT, NM1, NM12, NM13, OLDLL, OLDM
      REAL               ABSE, ABSS, COSL, COSR, CS, EPS, F, G, H, MU,
     $                   OLDCS, OLDSN, R, SHIFT, SIGMN, SIGMX, SINL,
     $                   SINR, SLL, SMAX, SMIN, SMINL, SMINLO, SMINOA,
     $                   SN, THRESH, TOL, TOLMUL, UNFL
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH
      EXTERNAL           LSAME, SLAMCH
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARTG, SLAS2, SLASQ1, SLASR, SLASV2, SROT,
     $                   SSCAL, SSWAP, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, REAL, SIGN, SQRT
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IUPLO = 0
      IF( LSAME( UPLO, 'U' ) )
     $   IUPLO = 1
      IF( LSAME( UPLO, 'L' ) )
     $   IUPLO = 2
      IF( IUPLO.EQ.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -5
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -9
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -11
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 )
     $   GO TO 150
C
C     ROTATE is true if any singular vectors desired, false otherwise
C
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
C
C     If no singular vectors desired, use qd algorithm
C
      IF( .NOT.ROTATE ) THEN
         CALL SLASQ1( N, D, E, WORK, INFO )
         RETURN
      END IF
C
      NM1 = N - 1
      NM12 = NM1 + NM1
      NM13 = NM12 + NM1
C
C     Get machine constants
C
      EPS = SLAMCH( 'Epsilon' )
      UNFL = SLAMCH( 'Safe minimum' )
C
C     If matrix lower bidiagonal, rotate to be upper bidiagonal
C     by applying Givens rotations on the left
C
      IF( IUPLO.EQ.2 ) THEN
         DO 10 I = 1, N - 1
            CALL SLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            WORK( I ) = CS
            WORK( NM1+I ) = SN
   10    CONTINUE
C
C        Update singular vectors if desired
C
         IF( NRU.GT.0 )
     $      CALL SLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ), WORK( N ), U,
     $                  LDU )
         IF( NCC.GT.0 )
     $      CALL SLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ), WORK( N ), C,
     $                  LDC )
      END IF
C
C     Compute singular values to relative accuracy TOL
C     (By setting TOL to be negative, algorithm will compute
C     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
C
      TOLMUL = MAX( TEN, MIN( HNDRD, EPS**MEIGTH ) )
      TOL = TOLMUL*EPS
C
C     Compute approximate maximum, minimum singular values
C
      SMAX = ABS( D( N ) )
      DO 20 I = 1, N - 1
         SMAX = MAX( SMAX, ABS( D( I ) ), ABS( E( I ) ) )
   20 CONTINUE
      SMINL = ZERO
      IF( TOL.GE.ZERO ) THEN
C
C        Relative accuracy desired
C
         SMINOA = ABS( D( 1 ) )
         IF( SMINOA.EQ.ZERO )
     $      GO TO 40
         MU = SMINOA
         DO 30 I = 2, N
            MU = ABS( D( I ) )*( MU / ( MU+ABS( E( I-1 ) ) ) )
            SMINOA = MIN( SMINOA, MU )
            IF( SMINOA.EQ.ZERO )
     $         GO TO 40
   30    CONTINUE
   40    CONTINUE
         SMINOA = SMINOA / SQRT( REAL( N ) )
         THRESH = MAX( TOL*SMINOA, MAXITR*N*N*UNFL )
      ELSE
C
C        Absolute accuracy desired
C
         THRESH = MAX( ABS( TOL )*SMAX, MAXITR*N*N*UNFL )
      END IF
C
C     Prepare for main iteration loop for the singular values
C     (MAXIT is the maximum number of passes through the inner
C     loop permitted before nonconvergence signalled.)
C
      MAXIT = MAXITR*N*N
      ITER = 0
      OLDLL = -1
      OLDM = -1
C
C     M points to last element of unconverged part of matrix
C
      M = N
C
C     Begin main iteration loop
C
   50 CONTINUE
C
C     Check for convergence or exceeding iteration count
C
      IF( M.LE.1 )
     $   GO TO 150
      IF( ITER.GT.MAXIT )
     $   GO TO 190
C
C     Find diagonal block of matrix to work on
C
      IF( TOL.LT.ZERO .AND. ABS( D( M ) ).LE.THRESH )
     $   D( M ) = ZERO
      SMAX = ABS( D( M ) )
      SMIN = SMAX
      DO 60 LLL = 1, M
         LL = M - LLL
         IF( LL.EQ.0 )
     $      GO TO 80
         ABSS = ABS( D( LL ) )
         ABSE = ABS( E( LL ) )
         IF( TOL.LT.ZERO .AND. ABSS.LE.THRESH )
     $      D( LL ) = ZERO
         IF( ABSE.LE.THRESH )
     $      GO TO 70
         SMIN = MIN( SMIN, ABSS )
         SMAX = MAX( SMAX, ABSS, ABSE )
   60 CONTINUE
   70 CONTINUE
      E( LL ) = ZERO
C
C     Matrix splits since E(LL) = 0
C
      IF( LL.EQ.M-1 ) THEN
C
C        Convergence of bottom singular value, return to top of loop
C
         M = M - 1
         GO TO 50
      END IF
   80 CONTINUE
      LL = LL + 1
C
C     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
C
      IF( LL.EQ.M-1 ) THEN
C
C        2 by 2 block, handle separately
C
         CALL SLASV2( D( M-1 ), E( M-1 ), D( M ), SIGMN, SIGMX, SINR,
     $                COSR, SINL, COSL )
         D( M-1 ) = SIGMX
         E( M-1 ) = ZERO
         D( M ) = SIGMN
C
C        Compute singular vectors, if desired
C
         IF( NCVT.GT.0 )
     $      CALL SROT( NCVT, VT( M-1, 1 ), LDVT, VT( M, 1 ), LDVT, COSR,
     $                 SINR )
         IF( NRU.GT.0 )
     $      CALL SROT( NRU, U( 1, M-1 ), 1, U( 1, M ), 1, COSL, SINL )
         IF( NCC.GT.0 )
     $      CALL SROT( NCC, C( M-1, 1 ), LDC, C( M, 1 ), LDC, COSL,
     $                 SINL )
         M = M - 2
         GO TO 50
      END IF
C
C     If working on new submatrix, choose shift direction
C     (from larger end diagonal element towards smaller)
C
      IF( LL.GT.OLDM .OR. M.LT.OLDLL ) THEN
         IF( ABS( D( LL ) ).GE.ABS( D( M ) ) ) THEN
C
C           Chase bulge from top (big end) to bottom (small end)
C
            IDIR = 1
         ELSE
C
C           Chase bulge from bottom (big end) to top (small end)
C
            IDIR = 2
         END IF
      END IF
C
C     Apply convergence tests
C
      IF( IDIR.EQ.1 ) THEN
C
C        Run convergence test in forward direction
C        First apply standard test to bottom of matrix
C
         IF( ABS( E( M-1 ) ).LE.ABS( TOL )*ABS( D( M ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( M-1 ) ).LE.THRESH ) ) THEN
            E( M-1 ) = ZERO
            GO TO 50
         END IF
C
         IF( TOL.GE.ZERO ) THEN
C
C           If relative accuracy desired,
C           apply convergence criterion forward
C
            MU = ABS( D( LL ) )
            SMINL = MU
            DO 90 LLL = LL, M - 1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 50
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL+1 ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
   90       CONTINUE
         END IF
C
      ELSE
C
C        Run convergence test in backward direction
C        First apply standard test to top of matrix
C
         IF( ABS( E( LL ) ).LE.ABS( TOL )*ABS( D( LL ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( LL ) ).LE.THRESH ) ) THEN
            E( LL ) = ZERO
            GO TO 50
         END IF
C
         IF( TOL.GE.ZERO ) THEN
C
C           If relative accuracy desired,
C           apply convergence criterion backward
C
            MU = ABS( D( M ) )
            SMINL = MU
            DO 100 LLL = M - 1, LL, -1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 50
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  100       CONTINUE
         END IF
      END IF
      OLDLL = LL
      OLDM = M
C
C     Compute shift.  First, test if shifting would ruin relative
C     accuracy, and if so set the shift to zero.
C
      IF( TOL.GE.ZERO .AND. N*TOL*( SMINL / SMAX ).LE.
     $    MAX( EPS, HNDRTH*TOL ) ) THEN
C
C        Use a zero shift to avoid loss of relative accuracy
C
         SHIFT = ZERO
      ELSE
C
C        Compute the shift from 2-by-2 block at end of matrix
C
         IF( IDIR.EQ.1 ) THEN
            SLL = ABS( D( LL ) )
            CALL SLAS2( D( M-1 ), E( M-1 ), D( M ), SHIFT, R )
         ELSE
            SLL = ABS( D( M ) )
            CALL SLAS2( D( LL ), E( LL ), D( LL+1 ), SHIFT, R )
         END IF
C
C        Test if shift negligible, and if so set to zero
C
         IF( SLL.GT.ZERO ) THEN
            IF( ( SHIFT / SLL )**2.LT.EPS )
     $         SHIFT = ZERO
         END IF
      END IF
C
C     Increment iteration count
C
      ITER = ITER + M - LL
C
C     If SHIFT = 0, do simplified QR iteration
C
      IF( SHIFT.EQ.ZERO ) THEN
         IF( IDIR.EQ.1 ) THEN
C
C           Chase bulge from top to bottom
C           Save cosines and sines for later singular vector updates
C
            CS = ONE
            OLDCS = ONE
            CALL SLARTG( D( LL )*CS, E( LL ), CS, SN, R )
            CALL SLARTG( OLDCS*R, D( LL+1 )*SN, OLDCS, OLDSN, D( LL ) )
            WORK( 1 ) = CS
            WORK( 1+NM1 ) = SN
            WORK( 1+NM12 ) = OLDCS
            WORK( 1+NM13 ) = OLDSN
            IROT = 1
            DO 110 I = LL + 1, M - 1
               CALL SLARTG( D( I )*CS, E( I ), CS, SN, R )
               E( I-1 ) = OLDSN*R
               CALL SLARTG( OLDCS*R, D( I+1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT + 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = OLDSN
  110       CONTINUE
            H = D( M )*CS
            D( M ) = H*OLDCS
            E( M-1 ) = H*OLDSN
C
C           Update singular vectors
C
            IF( NCVT.GT.0 )
     $         CALL SLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL SLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL SLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
C
C           Test convergence
C
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
C
         ELSE
C
C           Chase bulge from bottom to top
C           Save cosines and sines for later singular vector updates
C
            CS = ONE
            OLDCS = ONE
            CALL SLARTG( D( M )*CS, E( M-1 ), CS, SN, R )
            CALL SLARTG( OLDCS*R, D( M-1 )*SN, OLDCS, OLDSN, D( M ) )
            WORK( M-LL ) = CS
            WORK( M-LL+NM1 ) = -SN
            WORK( M-LL+NM12 ) = OLDCS
            WORK( M-LL+NM13 ) = -OLDSN
            IROT = M - LL
            DO 120 I = M - 1, LL + 1, -1
               CALL SLARTG( D( I )*CS, E( I-1 ), CS, SN, R )
               E( I ) = OLDSN*R
               CALL SLARTG( OLDCS*R, D( I-1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT - 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = -SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = -OLDSN
  120       CONTINUE
            H = D( LL )*CS
            D( LL ) = H*OLDCS
            E( LL ) = H*OLDSN
C
C           Update singular vectors
C
            IF( NCVT.GT.0 )
     $         CALL SLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL SLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL SLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
C
C           Test convergence
C
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
         END IF
      ELSE
C
C        Use nonzero shift
C
         IF( IDIR.EQ.1 ) THEN
C
C           Chase bulge from top to bottom
C           Save cosines and sines for later singular vector updates
C
            F = ( ABS( D( LL ) )-SHIFT )*
     $          ( SIGN( ONE, D( LL ) )+SHIFT / D( LL ) )
            G = E( LL )
            CALL SLARTG( F, G, COSR, SINR, R )
            F = COSR*D( LL ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL )
            G = SINR*D( LL+1 )
            D( LL+1 ) = COSR*D( LL+1 )
            CALL SLARTG( F, G, COSL, SINL, R )
            D( LL ) = R
            F = COSL*E( LL ) + SINL*D( LL+1 )
            D( LL+1 ) = COSL*D( LL+1 ) - SINL*E( LL )
            G = SINL*E( LL+1 )
            E( LL+1 ) = COSL*E( LL+1 )
            WORK( 1 ) = COSR
            WORK( 1+NM1 ) = SINR
            WORK( 1+NM12 ) = COSL
            WORK( 1+NM13 ) = SINL
            IROT = 1
            DO 130 I = LL + 1, M - 2
               CALL SLARTG( F, G, COSR, SINR, R )
               E( I-1 ) = R
               F = COSR*D( I ) + SINR*E( I )
               E( I ) = COSR*E( I ) - SINR*D( I )
               G = SINR*D( I+1 )
               D( I+1 ) = COSR*D( I+1 )
               CALL SLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I ) + SINL*D( I+1 )
               D( I+1 ) = COSL*D( I+1 ) - SINL*E( I )
               G = SINL*E( I+1 )
               E( I+1 ) = COSL*E( I+1 )
               IROT = IROT + 1
               WORK( IROT ) = COSR
               WORK( IROT+NM1 ) = SINR
               WORK( IROT+NM12 ) = COSL
               WORK( IROT+NM13 ) = SINL
  130       CONTINUE
            CALL SLARTG( F, G, COSR, SINR, R )
            E( M-2 ) = R
            F = COSR*D( M-1 ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M-1 )
            G = SINR*D( M )
            D( M ) = COSR*D( M )
            CALL SLARTG( F, G, COSL, SINL, R )
            D( M-1 ) = R
            F = COSL*E( M-1 ) + SINL*D( M )
            D( M ) = COSL*D( M ) - SINL*E( M-1 )
            IROT = IROT + 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = SINL
            E( M-1 ) = F
C
C           Update singular vectors
C
            IF( NCVT.GT.0 )
     $         CALL SLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL SLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL SLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
C
C           Test convergence
C
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
C
         ELSE
C
C           Chase bulge from bottom to top
C           Save cosines and sines for later singular vector updates
C
            F = ( ABS( D( M ) )-SHIFT )*( SIGN( ONE, D( M ) )+SHIFT /
     $          D( M ) )
            G = E( M-1 )
            CALL SLARTG( F, G, COSR, SINR, R )
            F = COSR*D( M ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M )
            G = SINR*D( M-1 )
            D( M-1 ) = COSR*D( M-1 )
            CALL SLARTG( F, G, COSL, SINL, R )
            D( M ) = R
            F = COSL*E( M-1 ) + SINL*D( M-1 )
            D( M-1 ) = COSL*D( M-1 ) - SINL*E( M-1 )
            G = SINL*E( M-2 )
            E( M-2 ) = COSL*E( M-2 )
            WORK( M-LL ) = COSR
            WORK( M-LL+NM1 ) = -SINR
            WORK( M-LL+NM12 ) = COSL
            WORK( M-LL+NM13 ) = -SINL
            IROT = M - LL
            DO 140 I = M - 1, LL + 2, -1
               CALL SLARTG( F, G, COSR, SINR, R )
               E( I ) = R
               F = COSR*D( I ) + SINR*E( I-1 )
               E( I-1 ) = COSR*E( I-1 ) - SINR*D( I )
               G = SINR*D( I-1 )
               D( I-1 ) = COSR*D( I-1 )
               CALL SLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I-1 ) + SINL*D( I-1 )
               D( I-1 ) = COSL*D( I-1 ) - SINL*E( I-1 )
               G = SINL*E( I-2 )
               E( I-2 ) = COSL*E( I-2 )
               IROT = IROT - 1
               WORK( IROT ) = COSR
               WORK( IROT+NM1 ) = -SINR
               WORK( IROT+NM12 ) = COSL
               WORK( IROT+NM13 ) = -SINL
  140       CONTINUE
            CALL SLARTG( F, G, COSR, SINR, R )
            E( LL+1 ) = R
            F = COSR*D( LL+1 ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL+1 )
            G = SINR*D( LL )
            D( LL ) = COSR*D( LL )
            CALL SLARTG( F, G, COSL, SINL, R )
            D( LL+1 ) = R
            F = COSL*E( LL ) + SINL*D( LL )
            D( LL ) = COSL*D( LL ) - SINL*E( LL )
            IROT = IROT - 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = -SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = -SINL
            E( LL ) = F
C
C           Test convergence
C
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
C
C           Update singular vectors if desired
C
            IF( NCVT.GT.0 )
     $         CALL SLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL SLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL SLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
         END IF
      END IF
C
C     QR iteration finished, go back and check convergence
C
      GO TO 50
C
C     All singular values converged, so make them positive
C
  150 CONTINUE
      DO 160 I = 1, N
         IF( D( I ).LT.ZERO ) THEN
            D( I ) = -D( I )
C
C           Change sign of singular vectors, if desired
C
            IF( NCVT.GT.0 )
     $         CALL SSCAL( NCVT, NEGONE, VT( I, 1 ), LDVT )
         END IF
  160 CONTINUE
C
C     Sort the singular values into decreasing order (insertion sort on
C     singular values, but only one transposition per singular vector)
C
      DO 180 I = 1, N - 1
C
C        Scan for smallest D(I)
C
         ISUB = 1
         SMIN = D( 1 )
         DO 170 J = 2, N + 1 - I
            IF( D( J ).LE.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
  170    CONTINUE
         IF( ISUB.NE.N+1-I ) THEN
C
C           Swap singular values and vectors
C
            D( ISUB ) = D( N+1-I )
            D( N+1-I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL SSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( N+1-I, 1 ),
     $                     LDVT )
            IF( NRU.GT.0 )
     $         CALL SSWAP( NRU, U( 1, ISUB ), 1, U( 1, N+1-I ), 1 )
            IF( NCC.GT.0 )
     $         CALL SSWAP( NCC, C( ISUB, 1 ), LDC, C( N+1-I, 1 ), LDC )
         END IF
  180 CONTINUE
      GO TO 210
C
C     Maximum number of iterations exceeded, failure to converge
C
  190 CONTINUE
      INFO = 0
      DO 200 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  200 CONTINUE
  210 CONTINUE
      RETURN
C
C     End of SBDSQR
C
      END
      SUBROUTINE SGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SGEBD2 reduces a real general m by n matrix A to upper or lower
C  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
C
C  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows in the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns in the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the m by n general matrix to be reduced.
C          On exit,
C          if m >= n, the diagonal and the first superdiagonal are
C            overwritten with the upper bidiagonal matrix B; the
C            elements below the diagonal, with the array TAUQ, represent
C            the orthogonal matrix Q as a product of elementary
C            reflectors, and the elements above the first superdiagonal,
C            with the array TAUP, represent the orthogonal matrix P as
C            a product of elementary reflectors;
C          if m < n, the diagonal and the first subdiagonal are
C            overwritten with the lower bidiagonal matrix B; the
C            elements below the first subdiagonal, with the array TAUQ,
C            represent the orthogonal matrix Q as a product of
C            elementary reflectors, and the elements above the diagonal,
C            with the array TAUP, represent the orthogonal matrix P as
C            a product of elementary reflectors.
C          See Further Details.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  D       (output) REAL array, dimension (min(M,N))
C          The diagonal elements of the bidiagonal matrix B:
C          D(i) = A(i,i).
C
C  E       (output) REAL array, dimension (min(M,N)-1)
C          The off-diagonal elements of the bidiagonal matrix B:
C          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
C          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
C
C  TAUQ    (output) REAL array dimension (min(M,N))
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix Q. See Further Details.
C
C  TAUP    (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix P. See Further Details.
C
C  WORK    (workspace) REAL array, dimension (max(M,N))
C
C  INFO    (output) INTEGER
C          = 0: successful exit.
C          < 0: if INFO = -i, the i-th argument had an illegal value.
C
C  Further Details
C  ===============
C
C  The matrices Q and P are represented as products of elementary
C  reflectors:
C
C  If m >= n,
C
C     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
C
C  Each H(i) and G(i) has the form:
C
C     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
C
C  where tauq and taup are real scalars, and v and u are real vectors;
C  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
C  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
C  tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  If m < n,
C
C     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
C
C  Each H(i) and G(i) has the form:
C
C     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
C
C  where tauq and taup are real scalars, and v and u are real vectors;
C  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
C  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
C  tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  The contents of A on exit are illustrated by the following examples:
C
C  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
C
C    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
C    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
C    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
C    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
C    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
C    (  v1  v2  v3  v4  v5 )
C
C  where d and e denote diagonal and off-diagonal elements of B, vi
C  denotes an element of the vector defining H(i), and ui an element of
C  the vector defining G(i).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, SLARFG, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'SGEBD2', -INFO )
         RETURN
      END IF
C
      IF( M.GE.N ) THEN
C
C        Reduce to upper bidiagonal form
C
         DO 10 I = 1, N
C
C           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
C
            CALL SLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
C
C           Apply H(i) to A(i:m,i+1:n) from the left
C
            CALL SLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAUQ( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = D( I )
C
            IF( I.LT.N ) THEN
C
C              Generate elementary reflector G(i) to annihilate
C              A(i,i+2:n)
C
               CALL SLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
C
C              Apply G(i) to A(i+1:m,i+1:n) from the right
C
               CALL SLARF( 'Right', M-I, N-I, A( I, I+1 ), LDA,
     $                     TAUP( I ), A( I+1, I+1 ), LDA, WORK )
               A( I, I+1 ) = E( I )
            ELSE
               TAUP( I ) = ZERO
            END IF
   10    CONTINUE
      ELSE
C
C        Reduce to lower bidiagonal form
C
         DO 20 I = 1, M
C
C           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
C
            CALL SLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
C
C           Apply G(i) to A(i+1:m,i:n) from the right
C
            CALL SLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAUP( I ),
     $                  A( MIN( I+1, M ), I ), LDA, WORK )
            A( I, I ) = D( I )
C
            IF( I.LT.M ) THEN
C
C              Generate elementary reflector H(i) to annihilate
C              A(i+2:m,i)
C
               CALL SLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
C
C              Apply H(i) to A(i+1:m,i+1:n) from the left
C
               CALL SLARF( 'Left', M-I, N-I, A( I+1, I ), 1, TAUQ( I ),
     $                     A( I+1, I+1 ), LDA, WORK )
               A( I+1, I ) = E( I )
            ELSE
               TAUQ( I ) = ZERO
            END IF
   20    CONTINUE
      END IF
      RETURN
C
C     End of SGEBD2
C
      END
      SUBROUTINE SGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
     $                   INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SGEBRD reduces a general real M-by-N matrix A to upper or lower
C  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
C
C  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows in the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns in the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the M-by-N general matrix to be reduced.
C          On exit,
C          if m >= n, the diagonal and the first superdiagonal are
C            overwritten with the upper bidiagonal matrix B; the
C            elements below the diagonal, with the array TAUQ, represent
C            the orthogonal matrix Q as a product of elementary
C            reflectors, and the elements above the first superdiagonal,
C            with the array TAUP, represent the orthogonal matrix P as
C            a product of elementary reflectors;
C          if m < n, the diagonal and the first subdiagonal are
C            overwritten with the lower bidiagonal matrix B; the
C            elements below the first subdiagonal, with the array TAUQ,
C            represent the orthogonal matrix Q as a product of
C            elementary reflectors, and the elements above the diagonal,
C            with the array TAUP, represent the orthogonal matrix P as
C            a product of elementary reflectors.
C          See Further Details.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  D       (output) REAL array, dimension (min(M,N))
C          The diagonal elements of the bidiagonal matrix B:
C          D(i) = A(i,i).
C
C  E       (output) REAL array, dimension (min(M,N)-1)
C          The off-diagonal elements of the bidiagonal matrix B:
C          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
C          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
C
C  TAUQ    (output) REAL array dimension (min(M,N))
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix Q. See Further Details.
C
C  TAUP    (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix P. See Further Details.
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The length of the array WORK.  LWORK >= max(1,M,N).
C          For optimum performance LWORK >= (M+N)*NB, where NB
C          is the optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value.
C
C  Further Details
C  ===============
C
C  The matrices Q and P are represented as products of elementary
C  reflectors:
C
C  If m >= n,
C
C     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
C
C  Each H(i) and G(i) has the form:
C
C     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
C
C  where tauq and taup are real scalars, and v and u are real vectors;
C  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
C  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
C  tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  If m < n,
C
C     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
C
C  Each H(i) and G(i) has the form:
C
C     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
C
C  where tauq and taup are real scalars, and v and u are real vectors;
C  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
C  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
C  tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  The contents of A on exit are illustrated by the following examples:
C
C  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
C
C    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
C    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
C    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
C    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
C    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
C    (  v1  v2  v3  v4  v5 )
C
C  where d and e denote diagonal and off-diagonal elements of B, vi
C  denotes an element of the vector defining H(i), and ui an element of
C  the vector defining G(i).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IINFO, J, LDWRKX, LDWRKY, MINMN, NB, NBMIN,
     $                   NX
      REAL               WS
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGEBD2, SGEMM, SLABRD, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M, N ) ) THEN
         INFO = -10
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'SGEBRD', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      MINMN = MIN( M, N )
      IF( MINMN.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
      WS = MAX( M, N )
      LDWRKX = M
      LDWRKY = N
C
C     Set the block size NB and the crossover point NX.
C
      NB = MAX( 1, ILAENV( 1, 'SGEBRD', ' ', M, N, -1, -1 ) )
C
      IF( NB.GT.1 .AND. NB.LT.MINMN ) THEN
C
C        Determine when to switch from blocked to unblocked code.
C
         NX = MAX( NB, ILAENV( 3, 'SGEBRD', ' ', M, N, -1, -1 ) )
         IF( NX.LT.MINMN ) THEN
            WS = ( M+N )*NB
            IF( LWORK.LT.WS ) THEN
C
C              Not enough work space for the optimal NB, consider using
C              a smaller block size.
C
               NBMIN = ILAENV( 2, 'SGEBRD', ' ', M, N, -1, -1 )
               IF( LWORK.GE.( M+N )*NBMIN ) THEN
                  NB = LWORK / ( M+N )
               ELSE
                  NB = 1
                  NX = MINMN
               END IF
            END IF
         END IF
      ELSE
         NX = MINMN
      END IF
C
      DO 30 I = 1, MINMN - NX, NB
C
C        Reduce rows and columns i:i+nb-1 to bidiagonal form and return
C        the matrices X and Y which are needed to update the unreduced
C        part of the matrix
C
         CALL SLABRD( M-I+1, N-I+1, NB, A( I, I ), LDA, D( I ), E( I ),
     $                TAUQ( I ), TAUP( I ), WORK, LDWRKX,
     $                WORK( LDWRKX*NB+1 ), LDWRKY )
C
C        Update the trailing submatrix A(i+nb:m,i+nb:n), using an update
C        of the form  A := A - V*Y' - X*U'
C
         CALL SGEMM( 'No transpose', 'Transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, A( I+NB, I ), LDA,
     $               WORK( LDWRKX*NB+NB+1 ), LDWRKY, ONE,
     $               A( I+NB, I+NB ), LDA )
         CALL SGEMM( 'No transpose', 'No transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, WORK( NB+1 ), LDWRKX, A( I, I+NB ), LDA,
     $               ONE, A( I+NB, I+NB ), LDA )
C
C        Copy diagonal and off-diagonal elements of B back into A
C
         IF( M.GE.N ) THEN
            DO 10 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J, J+1 ) = E( J )
   10       CONTINUE
         ELSE
            DO 20 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J+1, J ) = E( J )
   20       CONTINUE
         END IF
   30 CONTINUE
C
C     Use unblocked code to reduce the remainder of the matrix
C
      CALL SGEBD2( M-I+1, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $             TAUQ( I ), TAUP( I ), WORK, IINFO )
      WORK( 1 ) = WS
      RETURN
C
C     End of SGEBRD
C
      END
      SUBROUTINE SGELQ2( M, N, A, LDA, TAU, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SGELQ2 computes an LQ factorization of a real m by n matrix A:
C  A = L * Q.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the m by n matrix A.
C          On exit, the elements on and below the diagonal of the array
C          contain the m by min(m,n) lower trapezoidal matrix L (L is
C          lower triangular if m <= n); the elements above the diagonal,
C          with the array TAU, represent the orthogonal matrix Q as a
C          product of elementary reflectors (see Further Details).
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  TAU     (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors (see Further
C          Details).
C
C  WORK    (workspace) REAL array, dimension (M)
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument had an illegal value
C
C  Further Details
C  ===============
C
C  The matrix Q is represented as a product of elementary reflectors
C
C     Q = H(k) . . . H(2) H(1), where k = min(m,n).
C
C  Each H(i) has the form
C
C     H(i) = I - tau * v * v'
C
C  where tau is a real scalar, and v is a real vector with
C  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
C  and tau in TAU(i).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, K
      REAL               AII
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, SLARFG, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGELQ2', -INFO )
         RETURN
      END IF
C
      K = MIN( M, N )
C
      DO 10 I = 1, K
C
C        Generate elementary reflector H(i) to annihilate A(i,i+1:n)
C
         CALL SLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                TAU( I ) )
         IF( I.LT.M ) THEN
C
C           Apply H(i) to A(i+1:m,i:n) from the right
C
            AII = A( I, I )
            A( I, I ) = ONE
            CALL SLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAU( I ),
     $                  A( I+1, I ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
C
C     End of SGELQ2
C
      END
      SUBROUTINE SGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SGELQF computes an LQ factorization of a real M-by-N matrix A:
C  A = L * Q.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the M-by-N matrix A.
C          On exit, the elements on and below the diagonal of the array
C          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
C          lower triangular if m <= n); the elements above the diagonal,
C          with the array TAU, represent the orthogonal matrix Q as a
C          product of elementary reflectors (see Further Details).
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  TAU     (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors (see Further
C          Details).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK.  LWORK >= max(1,M).
C          For optimum performance LWORK >= M*NB, where NB is the
C          optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  Further Details
C  ===============
C
C  The matrix Q is represented as a product of elementary reflectors
C
C     Q = H(k) . . . H(2) H(1), where k = min(m,n).
C
C  Each H(i) has the form
C
C     H(i) = I - tau * v * v'
C
C  where tau is a real scalar, and v is a real vector with
C  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
C  and tau in TAU(i).
C
C  =====================================================================
C
C     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGELQ2, SLARFB, SLARFT, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGELQF', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.
C
      NB = ILAENV( 1, 'SGELQF', ' ', M, N, -1, -1 )
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
C
C        Determine when to cross over from blocked to unblocked code.
C
         NX = MAX( 0, ILAENV( 3, 'SGELQF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
C
C           Determine if workspace is large enough for blocked code.
C
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
C
C              Not enough workspace to use optimal NB:  reduce NB and
C              determine the minimum value of NB.
C
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'SGELQF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
C
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
C
C        Use blocked code initially
C
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
C
C           Compute the LQ factorization of the current block
C           A(i:i+ib-1,i:n)
C
            CALL SGELQ2( IB, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.M ) THEN
C
C              Form the triangular factor of the block reflector
C              H = H(i) H(i+1) . . . H(i+ib-1)
C
               CALL SLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
C
C              Apply H to A(i+ib:m,i:n) from the right
C
               CALL SLARFB( 'Right', 'No transpose', 'Forward',
     $                      'Rowwise', M-I-IB+1, N-I+1, IB, A( I, I ),
     $                      LDA, WORK, LDWORK, A( I+IB, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
C
C     Use unblocked code to factor the last or only block.
C
      IF( I.LE.K )
     $   CALL SGELQ2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
C
      WORK( 1 ) = IWS
      RETURN
C
C     End of SGELQF
C
      END
      SUBROUTINE SGEQR2( M, N, A, LDA, TAU, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SGEQR2 computes a QR factorization of a real m by n matrix A:
C  A = Q * R.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the m by n matrix A.
C          On exit, the elements on and above the diagonal of the array
C          contain the min(m,n) by n upper trapezoidal matrix R (R is
C          upper triangular if m >= n); the elements below the diagonal,
C          with the array TAU, represent the orthogonal matrix Q as a
C          product of elementary reflectors (see Further Details).
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  TAU     (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors (see Further
C          Details).
C
C  WORK    (workspace) REAL array, dimension (N)
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument had an illegal value
C
C  Further Details
C  ===============
C
C  The matrix Q is represented as a product of elementary reflectors
C
C     Q = H(1) H(2) . . . H(k), where k = min(m,n).
C
C  Each H(i) has the form
C
C     H(i) = I - tau * v * v'
C
C  where tau is a real scalar, and v is a real vector with
C  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
C  and tau in TAU(i).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, K
      REAL               AII
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, SLARFG, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGEQR2', -INFO )
         RETURN
      END IF
C
      K = MIN( M, N )
C
      DO 10 I = 1, K
C
C        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
C
         CALL SLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
C
C           Apply H(i) to A(i:m,i+1:n) from the left
C
            AII = A( I, I )
            A( I, I ) = ONE
            CALL SLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
C
C     End of SGEQR2
C
      END
      SUBROUTINE SGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SGEQRF computes a QR factorization of a real M-by-N matrix A:
C  A = Q * R.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the M-by-N matrix A.
C          On exit, the elements on and above the diagonal of the array
C          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
C          upper triangular if m >= n); the elements below the diagonal,
C          with the array TAU, represent the orthogonal matrix Q as a
C          product of min(m,n) elementary reflectors (see Further
C          Details).
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  TAU     (output) REAL array, dimension (min(M,N))
C          The scalar factors of the elementary reflectors (see Further
C          Details).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK.  LWORK >= max(1,N).
C          For optimum performance LWORK >= N*NB, where NB is
C          the optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  Further Details
C  ===============
C
C  The matrix Q is represented as a product of elementary reflectors
C
C     Q = H(1) H(2) . . . H(k), where k = min(m,n).
C
C  Each H(i) has the form
C
C     H(i) = I - tau * v * v'
C
C  where tau is a real scalar, and v is a real vector with
C  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
C  and tau in TAU(i).
C
C  =====================================================================
C
C     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGEQR2, SLARFB, SLARFT, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGEQRF', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.
C
      NB = ILAENV( 1, 'SGEQRF', ' ', M, N, -1, -1 )
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
C
C        Determine when to cross over from blocked to unblocked code.
C
         NX = MAX( 0, ILAENV( 3, 'SGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
C
C           Determine if workspace is large enough for blocked code.
C
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
C
C              Not enough workspace to use optimal NB:  reduce NB and
C              determine the minimum value of NB.
C
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'SGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
C
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
C
C        Use blocked code initially
C
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
C
C           Compute the QR factorization of the current block
C           A(i:m,i:i+ib-1)
C
            CALL SGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
C
C              Form the triangular factor of the block reflector
C              H = H(i) H(i+1) . . . H(i+ib-1)
C
               CALL SLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
C
C              Apply H' to A(i:m,i+ib:n) from the left
C
               CALL SLARFB( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
C
C     Use unblocked code to factor the last or only block.
C
      IF( I.LE.K )
     $   CALL SGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
C
      WORK( 1 ) = IWS
      RETURN
C
C     End of SGEQRF
C
      END
      SUBROUTINE SLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
     $                   LDY )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, M, N, NB
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), X( LDX, * ), Y( LDY, * )
C     ..
C
C  Purpose
C  =======
C
C  SLABRD reduces the first NB rows and columns of a real general
C  m by n matrix A to upper or lower bidiagonal form by an orthogonal
C  transformation Q' * A * P, and returns the matrices X and Y which
C  are needed to apply the transformation to the unreduced part of A.
C
C  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
C  bidiagonal form.
C
C  This is an auxiliary routine called by SGEBRD
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows in the matrix A.
C
C  N       (input) INTEGER
C          The number of columns in the matrix A.
C
C  NB      (input) INTEGER
C          The number of leading rows and columns of A to be reduced.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the m by n general matrix to be reduced.
C          On exit, the first NB rows and columns of the matrix are
C          overwritten; the rest of the array is unchanged.
C          If m >= n, elements on and below the diagonal in the first NB
C            columns, with the array TAUQ, represent the orthogonal
C            matrix Q as a product of elementary reflectors; and
C            elements above the diagonal in the first NB rows, with the
C            array TAUP, represent the orthogonal matrix P as a product
C            of elementary reflectors.
C          If m < n, elements below the diagonal in the first NB
C            columns, with the array TAUQ, represent the orthogonal
C            matrix Q as a product of elementary reflectors, and
C            elements on and above the diagonal in the first NB rows,
C            with the array TAUP, represent the orthogonal matrix P as
C            a product of elementary reflectors.
C          See Further Details.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  D       (output) REAL array, dimension (NB)
C          The diagonal elements of the first NB rows and columns of
C          the reduced matrix.  D(i) = A(i,i).
C
C  E       (output) REAL array, dimension (NB)
C          The off-diagonal elements of the first NB rows and columns of
C          the reduced matrix.
C
C  TAUQ    (output) REAL array dimension (NB)
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix Q. See Further Details.
C
C  TAUP    (output) REAL array, dimension (NB)
C          The scalar factors of the elementary reflectors which
C          represent the orthogonal matrix P. See Further Details.
C
C  X       (output) REAL array, dimension (LDX,NB)
C          The m-by-nb matrix X required to update the unreduced part
C          of A.
C
C  LDX     (input) INTEGER
C          The leading dimension of the array X. LDX >= M.
C
C  Y       (output) REAL array, dimension (LDY,NB)
C          The n-by-nb matrix Y required to update the unreduced part
C          of A.
C
C  LDY     (output) INTEGER
C          The leading dimension of the array Y. LDY >= N.
C
C  Further Details
C  ===============
C
C  The matrices Q and P are represented as products of elementary
C  reflectors:
C
C     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
C
C  Each H(i) and G(i) has the form:
C
C     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
C
C  where tauq and taup are real scalars, and v and u are real vectors.
C
C  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
C  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
C  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
C  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
C  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
C
C  The elements of the vectors v and u together form the m-by-nb matrix
C  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
C  the transformation to the unreduced part of the matrix, using a block
C  update of the form:  A := A - V*Y' - X*U'.
C
C  The contents of A on exit are illustrated by the following examples
C  with nb = 2:
C
C  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
C
C    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
C    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
C    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
C    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
C    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
C    (  v1  v2  a   a   a  )
C
C  where a denotes an element of the original matrix which is unchanged,
C  vi denotes an element of the vector defining H(i), and ui an element
C  of the vector defining G(i).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGEMV, SLARFG, SSCAL
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MIN
C     ..
C     .. Executable Statements ..
C
C     Quick return if possible
C
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
C
      IF( M.GE.N ) THEN
C
C        Reduce to upper bidiagonal form
C
         DO 10 I = 1, NB
C
C           Update A(i:m,i)
C
            CALL SGEMV( 'No transpose', M-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, Y( I, 1 ), LDY, ONE, A( I, I ), 1 )
            CALL SGEMV( 'No transpose', M-I+1, I-1, -ONE, X( I, 1 ),
     $                  LDX, A( 1, I ), 1, ONE, A( I, I ), 1 )
C
C           Generate reflection Q(i) to annihilate A(i+1:m,i)
C
            CALL SLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = ONE
C
C              Compute Y(i+1:n,i)
C
               CALL SGEMV( 'Transpose', M-I+1, N-I, ONE, A( I, I+1 ),
     $                     LDA, A( I, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', M-I+1, I-1, ONE, A( I, 1 ), LDA,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL SGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', M-I+1, I-1, ONE, X( I, 1 ), LDX,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL SGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL SSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
C
C              Update A(i,i+1:n)
C
               CALL SGEMV( 'No transpose', N-I, I, -ONE, Y( I+1, 1 ),
     $                     LDY, A( I, 1 ), LDA, ONE, A( I, I+1 ), LDA )
               CALL SGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, X( I, 1 ), LDX, ONE, A( I, I+1 ), LDA )
C
C              Generate reflection P(i) to annihilate A(i,i+2:n)
C
               CALL SLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
C
C              Compute X(i+1:m,i)
C
               CALL SGEMV( 'No transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', N-I, I, ONE, Y( I+1, 1 ), LDY,
     $                     A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL SGEMV( 'No transpose', M-I, I, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL SGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL SGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL SSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
            END IF
   10    CONTINUE
      ELSE
C
C        Reduce to lower bidiagonal form
C
         DO 20 I = 1, NB
C
C           Update A(i,i:n)
C
            CALL SGEMV( 'No transpose', N-I+1, I-1, -ONE, Y( I, 1 ),
     $                  LDY, A( I, 1 ), LDA, ONE, A( I, I ), LDA )
            CALL SGEMV( 'Transpose', I-1, N-I+1, -ONE, A( 1, I ), LDA,
     $                  X( I, 1 ), LDX, ONE, A( I, I ), LDA )
C
C           Generate reflection P(i) to annihilate A(i,i+1:n)
C
            CALL SLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
C
C              Compute X(i+1:m,i)
C
               CALL SGEMV( 'No transpose', M-I, N-I+1, ONE, A( I+1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', N-I+1, I-1, ONE, Y( I, 1 ), LDY,
     $                     A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL SGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL SGEMV( 'No transpose', I-1, N-I+1, ONE, A( 1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL SGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL SSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
C
C              Update A(i+1:m,i)
C
               CALL SGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, Y( I, 1 ), LDY, ONE, A( I+1, I ), 1 )
               CALL SGEMV( 'No transpose', M-I, I, -ONE, X( I+1, 1 ),
     $                     LDX, A( 1, I ), 1, ONE, A( I+1, I ), 1 )
C
C              Generate reflection Q(i) to annihilate A(i+2:m,i)
C
               CALL SLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
C
C              Compute Y(i+1:n,i)
C
               CALL SGEMV( 'Transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I+1, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', M-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL SGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL SGEMV( 'Transpose', M-I, I, ONE, X( I+1, 1 ), LDX,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL SGEMV( 'Transpose', I, N-I, -ONE, A( 1, I+1 ), LDA,
     $                     Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL SSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
            END IF
   20    CONTINUE
      END IF
      RETURN
C
C     End of SLABRD
C
      END
      SUBROUTINE SLACPY( UPLO, M, N, A, LDA, B, LDB )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
C     ..
C
C  Purpose
C  =======
C
C  SLACPY copies all or part of a two-dimensional matrix A to another
C  matrix B.
C
C  Arguments
C  =========
C
C  UPLO    (input) CHARACTER*1
C          Specifies the part of the matrix A to be copied to B.
C          = 'U':      Upper triangular part
C          = 'L':      Lower triangular part
C          Otherwise:  All of the matrix A
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input) REAL array, dimension (LDA,N)
C          The m by n matrix A.  If UPLO = 'U', only the upper triangle
C          or trapezoid is accessed; if UPLO = 'L', only the lower
C          triangle or trapezoid is accessed.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  B       (output) REAL array, dimension (LDB,N)
C          On exit, B = A in the locations specified by UPLO.
C
C  LDB     (input) INTEGER
C          The leading dimension of the array B.  LDB >= max(1,M).
C
C  =====================================================================
C
C     .. Local Scalars ..
      INTEGER            I, J
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MIN
C     ..
C     .. Executable Statements ..
C
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
      RETURN
C
C     End of SLACPY
C
      END
      REAL             FUNCTION SLANGE( NORM, M, N, A, LDA, WORK )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SLANGE  returns the value of the one norm,  or the Frobenius norm, or
C  the  infinity norm,  or the  element of  largest absolute value  of a
C  real matrix A.
C
C  Description
C  ===========
C
C  SLANGE returns the value
C
C     SLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
C              (
C              ( norm1(A),         NORM = '1', 'O' or 'o'
C              (
C              ( normI(A),         NORM = 'I' or 'i'
C              (
C              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
C
C  where  norm1  denotes the  one norm of a matrix (maximum column sum),
C  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
C  normF  denotes the  Frobenius norm of a matrix (square root of sum of
C  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
C
C  Arguments
C  =========
C
C  NORM    (input) CHARACTER*1
C          Specifies the value to be returned in SLANGE as described
C          above.
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.  When M = 0,
C          SLANGE is set to zero.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.  When N = 0,
C          SLANGE is set to zero.
C
C  A       (input) REAL array, dimension (LDA,N)
C          The m by n matrix A.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(M,1).
C
C  WORK    (workspace) REAL array, dimension (LWORK),
C          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
C          referenced.
C
C =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J
      REAL               SCALE, SUM, VALUE
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLASSQ
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
C     ..
C     .. Executable Statements ..
C
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
C
C        Find max(abs(A(i,j))).
C
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
C
C        Find norm1(A).
C
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
C
C        Find normI(A).
C
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
C
C        Find normF(A).
C
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL SLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
C
      SLANGE = VALUE
      RETURN
C
C     End of SLANGE
C
      END
      REAL             FUNCTION SLAPY2( X, Y )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      REAL               X, Y
C     ..
C
C  Purpose
C  =======
C
C  SLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
C  overflow.
C
C  Arguments
C  =========
C
C  X       (input) REAL
C  Y       (input) REAL
C          X and Y specify the values x and y.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
C     ..
C     .. Local Scalars ..
      REAL               W, XABS, YABS, Z
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
C     ..
C     .. Executable Statements ..
C
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         SLAPY2 = W
      ELSE
         SLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
C
C     End of SLAPY2
C
      END
      SUBROUTINE SLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      REAL               TAU
C     ..
C     .. Array Arguments ..
      REAL               C( LDC, * ), V( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SLARF applies a real elementary reflector H to a real m by n matrix
C  C, from either the left or the right. H is represented in the form
C
C        H = I - tau * v * v'
C
C  where tau is a real scalar and v is a real vector.
C
C  If tau = 0, then H is taken to be the unit matrix.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': form  H * C
C          = 'R': form  C * H
C
C  M       (input) INTEGER
C          The number of rows of the matrix C.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C.
C
C  V       (input) REAL array, dimension
C                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
C                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
C          The vector v in the representation of H. V is not used if
C          TAU = 0.
C
C  INCV    (input) INTEGER
C          The increment between elements of v. INCV <> 0.
C
C  TAU     (input) REAL
C          The value tau in the representation of H.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the m by n matrix C.
C          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
C          or C * H if SIDE = 'R'.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace) REAL array, dimension
C                         (N) if SIDE = 'L'
C                      or (M) if SIDE = 'R'
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGEMV, SGER
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Executable Statements ..
C
      IF( LSAME( SIDE, 'L' ) ) THEN
C
C        Form  H * C
C
         IF( TAU.NE.ZERO ) THEN
C
C           w := C' * v
C
            CALL SGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO,
     $                  WORK, 1 )
C
C           C := C - v * w'
C
            CALL SGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
C
C        Form  C * H
C
         IF( TAU.NE.ZERO ) THEN
C
C           w := C * v
C
            CALL SGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
C
C           C := C - w * v'
C
            CALL SGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
C
C     End of SLARF
C
      END
      SUBROUTINE SLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
C     ..
C
C  Purpose
C  =======
C
C  SLARFB applies a real block reflector H or its transpose H' to a
C  real m by n matrix C, from either the left or the right.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply H or H' from the Left
C          = 'R': apply H or H' from the Right
C
C  TRANS   (input) CHARACTER*1
C          = 'N': apply H (No transpose)
C          = 'T': apply H' (Transpose)
C
C  DIRECT  (input) CHARACTER*1
C          Indicates how H is formed from a product of elementary
C          reflectors
C          = 'F': H = H(1) H(2) . . . H(k) (Forward)
C          = 'B': H = H(k) . . . H(2) H(1) (Backward)
C
C  STOREV  (input) CHARACTER*1
C          Indicates how the vectors which define the elementary
C          reflectors are stored:
C          = 'C': Columnwise
C          = 'R': Rowwise
C
C  M       (input) INTEGER
C          The number of rows of the matrix C.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C.
C
C  K       (input) INTEGER
C          The order of the matrix T (= the number of elementary
C          reflectors whose product defines the block reflector).
C
C  V       (input) REAL array, dimension
C                                (LDV,K) if STOREV = 'C'
C                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
C                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
C          The matrix V. See further details.
C
C  LDV     (input) INTEGER
C          The leading dimension of the array V.
C          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
C          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
C          if STOREV = 'R', LDV >= K.
C
C  T       (input) REAL array, dimension (LDT,K)
C          The triangular k by k matrix T in the representation of the
C          block reflector.
C
C  LDT     (input) INTEGER
C          The leading dimension of the array T. LDT >= K.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the m by n matrix C.
C          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDA >= max(1,M).
C
C  WORK    (workspace) REAL array, dimension (LDWORK,K)
C
C  LDWORK  (input) INTEGER
C          The leading dimension of the array WORK.
C          If SIDE = 'L', LDWORK >= max(1,N);
C          if SIDE = 'R', LDWORK >= max(1,M).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SCOPY, SGEMM, STRMM
C     ..
C     .. Executable Statements ..
C
C     Quick return if possible
C
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
C
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      END IF
C
      IF( LSAME( STOREV, 'C' ) ) THEN
C
         IF( LSAME( DIRECT, 'F' ) ) THEN
C
C           Let  V =  ( V1 )    (first K rows)
C                     ( V2 )
C           where  V1  is unit lower triangular.
C
            IF( LSAME( SIDE, 'L' ) ) THEN
C
C              Form  H * C  or  H' * C  where  C = ( C1 )
C                                                  ( C2 )
C
C              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
C
C              W := C1'
C
               DO 10 J = 1, K
                  CALL SCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
C
C              W := W * V1
C
               CALL STRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
C
C                 W := W + C2'*V2
C
                  CALL SGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
C
C              W := W * T'  or  W * T
C
               CALL STRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - V * W'
C
               IF( M.GT.K ) THEN
C
C                 C2 := C2 - V2 * W'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
C
C              W := W * V1'
C
               CALL STRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
C
C              C1 := C1 - W'
C
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             CONTINUE
   30          CONTINUE
C
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
C
C              Form  C * H  or  C * H'  where  C = ( C1  C2 )
C
C              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
C
C              W := C1
C
               DO 40 J = 1, K
                  CALL SCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
C
C              W := W * V1
C
               CALL STRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
C
C                 W := W + C2 * V2
C
                  CALL SGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
C
C              W := W * T  or  W * T'
C
               CALL STRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - W * V'
C
               IF( N.GT.K ) THEN
C
C                 C2 := C2 - W * V2'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
C
C              W := W * V1'
C
               CALL STRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
C
C              C1 := C1 - W
C
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
C
         ELSE
C
C           Let  V =  ( V1 )
C                     ( V2 )    (last K rows)
C           where  V2  is unit upper triangular.
C
            IF( LSAME( SIDE, 'L' ) ) THEN
C
C              Form  H * C  or  H' * C  where  C = ( C1 )
C                                                  ( C2 )
C
C              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
C
C              W := C2'
C
               DO 70 J = 1, K
                  CALL SCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
C
C              W := W * V2
C
               CALL STRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
C
C                 W := W + C1'*V1
C
                  CALL SGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
C
C              W := W * T'  or  W * T
C
               CALL STRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - V * W'
C
               IF( M.GT.K ) THEN
C
C                 C1 := C1 - V1 * W'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
C
C              W := W * V2'
C
               CALL STRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
C
C              C2 := C2 - W'
C
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             CONTINUE
   90          CONTINUE
C
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
C
C              Form  C * H  or  C * H'  where  C = ( C1  C2 )
C
C              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
C
C              W := C2
C
               DO 100 J = 1, K
                  CALL SCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
C
C              W := W * V2
C
               CALL STRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
C
C                 W := W + C1 * V1
C
                  CALL SGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
C
C              W := W * T  or  W * T'
C
               CALL STRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - W * V'
C
               IF( N.GT.K ) THEN
C
C                 C1 := C1 - W * V1'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
C
C              W := W * V2'
C
               CALL STRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
C
C              C2 := C2 - W
C
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
C
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
C
         IF( LSAME( DIRECT, 'F' ) ) THEN
C
C           Let  V =  ( V1  V2 )    (V1: first K columns)
C           where  V1  is unit upper triangular.
C
            IF( LSAME( SIDE, 'L' ) ) THEN
C
C              Form  H * C  or  H' * C  where  C = ( C1 )
C                                                  ( C2 )
C
C              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
C
C              W := C1'
C
               DO 130 J = 1, K
                  CALL SCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
C
C              W := W * V1'
C
               CALL STRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
C
C                 W := W + C2'*V2'
C
                  CALL SGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
C
C              W := W * T'  or  W * T
C
               CALL STRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - V' * W'
C
               IF( M.GT.K ) THEN
C
C                 C2 := C2 - V2' * W'
C
                  CALL SGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
C
C              W := W * V1
C
               CALL STRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
C
C              C1 := C1 - W'
C
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             CONTINUE
  150          CONTINUE
C
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
C
C              Form  C * H  or  C * H'  where  C = ( C1  C2 )
C
C              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
C
C              W := C1
C
               DO 160 J = 1, K
                  CALL SCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
C
C              W := W * V1'
C
               CALL STRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
C
C                 W := W + C2 * V2'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
C
C              W := W * T  or  W * T'
C
               CALL STRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - W * V
C
               IF( N.GT.K ) THEN
C
C                 C2 := C2 - W * V2
C
                  CALL SGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
C
C              W := W * V1
C
               CALL STRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
C
C              C1 := C1 - W
C
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
C
            END IF
C
         ELSE
C
C           Let  V =  ( V1  V2 )    (V2: last K columns)
C           where  V2  is unit lower triangular.
C
            IF( LSAME( SIDE, 'L' ) ) THEN
C
C              Form  H * C  or  H' * C  where  C = ( C1 )
C                                                  ( C2 )
C
C              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
C
C              W := C2'
C
               DO 190 J = 1, K
                  CALL SCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
C
C              W := W * V2'
C
               CALL STRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
C
C                 W := W + C1'*V1'
C
                  CALL SGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
C
C              W := W * T'  or  W * T
C
               CALL STRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - V' * W'
C
               IF( M.GT.K ) THEN
C
C                 C1 := C1 - V1' * W'
C
                  CALL SGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
C
C              W := W * V2
C
               CALL STRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
C
C              C2 := C2 - W'
C
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             CONTINUE
  210          CONTINUE
C
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
C
C              Form  C * H  or  C * H'  where  C = ( C1  C2 )
C
C              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
C
C              W := C2
C
               DO 220 J = 1, K
                  CALL SCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
C
C              W := W * V2'
C
               CALL STRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
C
C                 W := W + C1 * V1'
C
                  CALL SGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
C
C              W := W * T  or  W * T'
C
               CALL STRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
C
C              C := C - W * V
C
               IF( N.GT.K ) THEN
C
C                 C1 := C1 - W * V1
C
                  CALL SGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
C
C              W := W * V2
C
               CALL STRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
C
C              C1 := C1 - W
C
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
C
            END IF
C
         END IF
      END IF
C
      RETURN
C
C     End of SLARFB
C
      END
      SUBROUTINE SLARFG( N, ALPHA, X, INCX, TAU )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INCX, N
      REAL               ALPHA, TAU
C     ..
C     .. Array Arguments ..
      REAL               X( * )
C     ..
C
C  Purpose
C  =======
C
C  SLARFG generates a real elementary reflector H of order n, such
C  that
C
C        H * ( alpha ) = ( beta ),   H' * H = I.
C            (   x   )   (   0  )
C
C  where alpha and beta are scalars, and x is an (n-1)-element real
C  vector. H is represented in the form
C
C        H = I - tau * ( 1 ) * ( 1 v' ) ,
C                      ( v )
C
C  where tau is a real scalar and v is a real (n-1)-element
C  vector.
C
C  If the elements of x are all zero, then tau = 0 and H is taken to be
C  the unit matrix.
C
C  Otherwise  1 <= tau <= 2.
C
C  Arguments
C  =========
C
C  N       (input) INTEGER
C          The order of the elementary reflector.
C
C  ALPHA   (input/output) REAL
C          On entry, the value alpha.
C          On exit, it is overwritten with the value beta.
C
C  X       (input/output) REAL array, dimension
C                         (1+(N-2)*abs(INCX))
C          On entry, the vector x.
C          On exit, it is overwritten with the vector v.
C
C  INCX    (input) INTEGER
C          The increment between elements of X. INCX > 0.
C
C  TAU     (output) REAL
C          The value tau.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            J, KNT
      REAL               BETA, RSAFMN, SAFMIN, XNORM
C     ..
C     .. External Functions ..
      REAL               SLAMCH, SLAPY2, SNRM2
      EXTERNAL           SLAMCH, SLAPY2, SNRM2
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
C     ..
C     .. External Subroutines ..
      EXTERNAL           SSCAL
C     ..
C     .. Executable Statements ..
C
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
C
      XNORM = SNRM2( N-1, X, INCX )
C
      IF( XNORM.EQ.ZERO ) THEN
C
C        H  =  I
C
         TAU = ZERO
      ELSE
C
C        general case
C
         BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = SLAMCH( 'S' ) / SLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
C
C           XNORM, BETA may be inaccurate; scale X and recompute them
C
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL SSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
C
C           New BETA is at most 1, at least SAFMIN
C
            XNORM = SNRM2( N-1, X, INCX )
            BETA = -SIGN( SLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
C
C           If ALPHA is subnormal, it may lose relative accuracy
C
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL SSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
C
      RETURN
C
C     End of SLARFG
C
      END
      SUBROUTINE SLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
C     ..
C     .. Array Arguments ..
      REAL               T( LDT, * ), TAU( * ), V( LDV, * )
C     ..
C
C  Purpose
C  =======
C
C  SLARFT forms the triangular factor T of a real block reflector H
C  of order n, which is defined as a product of k elementary reflectors.
C
C  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
C
C  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
C
C  If STOREV = 'C', the vector which defines the elementary reflector
C  H(i) is stored in the i-th column of the array V, and
C
C     H  =  I - V * T * V'
C
C  If STOREV = 'R', the vector which defines the elementary reflector
C  H(i) is stored in the i-th row of the array V, and
C
C     H  =  I - V' * T * V
C
C  Arguments
C  =========
C
C  DIRECT  (input) CHARACTER*1
C          Specifies the order in which the elementary reflectors are
C          multiplied to form the block reflector:
C          = 'F': H = H(1) H(2) . . . H(k) (Forward)
C          = 'B': H = H(k) . . . H(2) H(1) (Backward)
C
C  STOREV  (input) CHARACTER*1
C          Specifies how the vectors which define the elementary
C          reflectors are stored (see also Further Details):
C          = 'C': columnwise
C          = 'R': rowwise
C
C  N       (input) INTEGER
C          The order of the block reflector H. N >= 0.
C
C  K       (input) INTEGER
C          The order of the triangular factor T (= the number of
C          elementary reflectors). K >= 1.
C
C  V       (input/output) REAL array, dimension
C                               (LDV,K) if STOREV = 'C'
C                               (LDV,N) if STOREV = 'R'
C          The matrix V. See further details.
C
C  LDV     (input) INTEGER
C          The leading dimension of the array V.
C          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i).
C
C  T       (output) REAL array, dimension (LDT,K)
C          The k by k triangular factor T of the block reflector.
C          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
C          lower triangular. The rest of the array is not used.
C
C  LDT     (input) INTEGER
C          The leading dimension of the array T. LDT >= K.
C
C  Further Details
C  ===============
C
C  The shape of the matrix V and the storage of the vectors which define
C  the H(i) is best illustrated by the following example with n = 5 and
C  k = 3. The elements equal to 1 are not stored; the corresponding
C  array elements are modified but restored on exit. The rest of the
C  array is not used.
C
C  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
C
C               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
C                   ( v1  1    )                     (     1 v2 v2 v2 )
C                   ( v1 v2  1 )                     (        1 v3 v3 )
C                   ( v1 v2 v3 )
C                   ( v1 v2 v3 )
C
C  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
C
C               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
C                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
C                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
C                   (     1 v3 )
C                   (        1 )
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J
      REAL               VII
C     ..
C     .. External Subroutines ..
      EXTERNAL           SGEMV, STRMV
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Executable Statements ..
C
C     Quick return if possible
C
      IF( N.EQ.0 )
     $   RETURN
C
      IF( LSAME( DIRECT, 'F' ) ) THEN
         DO 20 I = 1, K
            IF( TAU( I ).EQ.ZERO ) THEN
C
C              H(i)  =  I
C
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
C
C              general case
C
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
C
C                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
C
                  CALL SGEMV( 'Transpose', N-I+1, I-1, -TAU( I ),
     $                        V( I, 1 ), LDV, V( I, I ), 1, ZERO,
     $                        T( 1, I ), 1 )
               ELSE
C
C                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
C
                  CALL SGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
               END IF
               V( I, I ) = VII
C
C              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
C
               CALL STRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            END IF
   20    CONTINUE
      ELSE
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
C
C              H(i)  =  I
C
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
C
C              general case
C
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
C
C                    T(i+1:k,i) :=
C                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
C
                     CALL SGEMV( 'Transpose', N-K+I, K-I, -TAU( I ),
     $                           V( 1, I+1 ), LDV, V( 1, I ), 1, ZERO,
     $                           T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
C
C                    T(i+1:k,i) :=
C                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
C
                     CALL SGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                           T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  END IF
C
C                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
C
                  CALL STRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
C
C     End of SLARFT
C
      END
      SUBROUTINE SLARTG( F, G, CS, SN, R )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      REAL               CS, F, G, R, SN
C     ..
C
C  Purpose
C  =======
C
C  SLARTG generate a plane rotation so that
C
C     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
C     [ -SN  CS  ]     [ G ]     [ 0 ]
C
C  This is a slower, more accurate version of the BLAS1 routine SROTG,
C  with the following other differences:
C     F and G are unchanged on return.
C     If G=0, then CS=1 and SN=0.
C     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
C        floating point operations (saves work in SBDSQR when
C        there are zeros on the diagonal).
C
C  If F exceeds G in magnitude, CS will be positive.
C
C  Arguments
C  =========
C
C  F       (input) REAL
C          The first component of vector to be rotated.
C
C  G       (input) REAL
C          The second component of vector to be rotated.
C
C  CS      (output) REAL
C          The cosine of the rotation.
C
C  SN      (output) REAL
C          The sine of the rotation.
C
C  R       (output) REAL
C          The nonzero component of the rotated vector.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0E0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            FIRST
      INTEGER            COUNT, I
      REAL               EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE
C     ..
C     .. External Functions ..
      REAL               SLAMCH
      EXTERNAL           SLAMCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, SQRT
C     ..
C     .. Save statement ..
      SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
C     ..
C     .. Data statements ..
      DATA               FIRST / .TRUE. /
C     ..
C     .. Executable Statements ..
C
      IF( FIRST ) THEN
         FIRST = .FALSE.
         SAFMIN = SLAMCH( 'S' )
         EPS = SLAMCH( 'E' )
         SAFMN2 = SLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( SLAMCH( 'B' ) ) / TWO )
         SAFMX2 = ONE / SAFMN2
      END IF
      IF( G.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.ZERO ) THEN
         CS = ZERO
         SN = ONE
         R = G
      ELSE
         F1 = F
         G1 = G
         SCALE = MAX( ABS( F1 ), ABS( G1 ) )
         IF( SCALE.GE.SAFMX2 ) THEN
            COUNT = 0
   10       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMN2
            G1 = G1*SAFMN2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.GE.SAFMX2 )
     $         GO TO 10
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 20 I = 1, COUNT
               R = R*SAFMX2
   20       CONTINUE
         ELSE IF( SCALE.LE.SAFMN2 ) THEN
            COUNT = 0
   30       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMX2
            G1 = G1*SAFMX2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.LE.SAFMN2 )
     $         GO TO 30
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 40 I = 1, COUNT
               R = R*SAFMN2
   40       CONTINUE
         ELSE
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
         END IF
         IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN
            CS = -CS
            SN = -SN
            R = -R
         END IF
      END IF
      RETURN
C
C     End of SLARTG
C
      END
      SUBROUTINE SLAS2( F, G, H, SSMIN, SSMAX )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      REAL               F, G, H, SSMAX, SSMIN
C     ..
C
C  Purpose
C  =======
C
C  SLAS2  computes the singular values of the 2-by-2 matrix
C     [  F   G  ]
C     [  0   H  ].
C  On return, SSMIN is the smaller singular value and SSMAX is the
C  larger singular value.
C
C  Arguments
C  =========
C
C  F       (input) REAL
C          The (1,1) element of the 2-by-2 matrix.
C
C  G       (input) REAL
C          The (1,2) element of the 2-by-2 matrix.
C
C  H       (input) REAL
C          The (2,2) element of the 2-by-2 matrix.
C
C  SSMIN   (output) REAL
C          The smaller singular value.
C
C  SSMAX   (output) REAL
C          The larger singular value.
C
C  Further Details
C  ===============
C
C  Barring over/underflow, all output quantities are correct to within
C  a few units in the last place (ulps), even in the absence of a guard
C  digit in addition/subtraction.
C
C  In IEEE arithmetic, the code works correctly if one matrix element is
C  infinite.
C
C  Overflow will not occur unless the largest singular value itself
C  overflows, or is within a few ulps of overflow. (On machines with
C  partial overflow, like the Cray, overflow may occur if the largest
C  singular value is within a factor of 2 of overflow.)
C
C  Underflow is harmless if underflow is gradual. Otherwise, results
C  may correspond to a matrix modified by perturbations of size near
C  the underflow threshold.
C
C  ====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0E0 )
C     ..
C     .. Local Scalars ..
      REAL               AS, AT, AU, C, FA, FHMN, FHMX, GA, HA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
C     ..
C     .. Executable Statements ..
C
      FA = ABS( F )
      GA = ABS( G )
      HA = ABS( H )
      FHMN = MIN( FA, HA )
      FHMX = MAX( FA, HA )
      IF( FHMN.EQ.ZERO ) THEN
         SSMIN = ZERO
         IF( FHMX.EQ.ZERO ) THEN
            SSMAX = GA
         ELSE
            SSMAX = MAX( FHMX, GA )*SQRT( ONE+
     $              ( MIN( FHMX, GA ) / MAX( FHMX, GA ) )**2 )
         END IF
      ELSE
         IF( GA.LT.FHMX ) THEN
            AS = ONE + FHMN / FHMX
            AT = ( FHMX-FHMN ) / FHMX
            AU = ( GA / FHMX )**2
            C = TWO / ( SQRT( AS*AS+AU )+SQRT( AT*AT+AU ) )
            SSMIN = FHMN*C
            SSMAX = FHMX / C
         ELSE
            AU = FHMX / GA
            IF( AU.EQ.ZERO ) THEN
C
C              Avoid possible harmful underflow if exponent range
C              asymmetric (true SSMIN may not underflow even if
C              AU underflows)
C
               SSMIN = ( FHMN*FHMX ) / GA
               SSMAX = GA
            ELSE
               AS = ONE + FHMN / FHMX
               AT = ( FHMX-FHMN ) / FHMX
               C = ONE / ( SQRT( ONE+( AS*AU )**2 )+
     $             SQRT( ONE+( AT*AU )**2 ) )
               SSMIN = ( FHMN*C )*AU
               SSMIN = SSMIN + SSMIN
               SSMAX = GA / ( C+C )
            END IF
         END IF
      END IF
      RETURN
C
C     End of SLAS2
C
      END
      SUBROUTINE SLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      REAL               CFROM, CTO
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * )
C     ..
C
C  Purpose
C  =======
C
C  SLASCL multiplies the M by N real matrix A by the real scalar
C  CTO/CFROM.  This is done without over/underflow as long as the final
C  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
C  A may be full, upper triangular, lower triangular, upper Hessenberg,
C  or banded.
C
C  Arguments
C  =========
C
C  TYPE    (input) CHARACTER*1
C          TYPE indices the storage type of the input matrix.
C          = 'G':  A is a full matrix.
C          = 'L':  A is a lower triangular matrix.
C          = 'U':  A is an upper triangular matrix.
C          = 'H':  A is an upper Hessenberg matrix.
C          = 'B':  A is a symmetric band matrix with lower bandwidth KL
C                  and upper bandwidth KU and with the only the lower
C                  half stored.
C          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
C                  and upper bandwidth KU and with the only the upper
C                  half stored.
C          = 'Z':  A is a band matrix with lower bandwidth KL and upper
C                  bandwidth KU.
C
C  KL      (input) INTEGER
C          The lower bandwidth of A.  Referenced only if TYPE = 'B',
C          'Q' or 'Z'.
C
C  KU      (input) INTEGER
C          The upper bandwidth of A.  Referenced only if TYPE = 'B',
C          'Q' or 'Z'.
C
C  CFROM   (input) REAL
C  CTO     (input) REAL
C          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
C          without over/underflow if the final result CTO*A(I,J)/CFROM
C          can be represented without over/underflow.  CFROM must be
C          nonzero.
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,M)
C          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
C          storage type.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  INFO    (output) INTEGER
C          0  - successful exit
C          <0 - if INFO = -i, the i-th argument had an illegal value.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      REAL               BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      REAL               SLAMCH
      EXTERNAL           LSAME, SLAMCH
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
C     ..
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
C
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
C
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
C
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLASCL', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
C
C     Get machine parameters
C
      SMLNUM = SLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
C
      CFROMC = CFROM
      CTOC = CTO
C
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
C
      IF( ITYPE.EQ.0 ) THEN
C
C        Full matrix
C
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
C
      ELSE IF( ITYPE.EQ.1 ) THEN
C
C        Lower triangular matrix
C
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
C
      ELSE IF( ITYPE.EQ.2 ) THEN
C
C        Upper triangular matrix
C
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
C
      ELSE IF( ITYPE.EQ.3 ) THEN
C
C        Upper Hessenberg matrix
C
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
C
      ELSE IF( ITYPE.EQ.4 ) THEN
C
C        Lower half of a symmetric band matrix
C
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
C
      ELSE IF( ITYPE.EQ.5 ) THEN
C
C        Upper half of a symmetric band matrix
C
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
C
      ELSE IF( ITYPE.EQ.6 ) THEN
C
C        Band matrix
C
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
C
      END IF
C
      IF( .NOT.DONE )
     $   GO TO 10
C
      RETURN
C
C     End of SLASCL
C
      END
      SUBROUTINE SLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      REAL               ALPHA, BETA
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * )
C     ..
C
C  Purpose
C  =======
C
C  SLASET initializes an m-by-n matrix A to BETA on the diagonal and
C  ALPHA on the offdiagonals.
C
C  Arguments
C  =========
C
C  UPLO    (input) CHARACTER*1
C          Specifies the part of the matrix A to be set.
C          = 'U':      Upper triangular part is set; the strictly lower
C                      triangular part of A is not changed.
C          = 'L':      Lower triangular part is set; the strictly upper
C                      triangular part of A is not changed.
C          Otherwise:  All of the matrix A is set.
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  N >= 0.
C
C  ALPHA   (input) REAL
C          The constant to which the offdiagonal elements are to be set.
C
C  BETA    (input) REAL
C          The constant to which the diagonal elements are to be set.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On exit, the leading m-by-n submatrix of A is set as follows:
C
C          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
C          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
C          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
C
C          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C =====================================================================
C
C     .. Local Scalars ..
      INTEGER            I, J
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MIN
C     ..
C     .. Executable Statements ..
C
      IF( LSAME( UPLO, 'U' ) ) THEN
C
C        Set the strictly upper triangular or trapezoidal part of the
C        array to ALPHA.
C
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
C
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
C
C        Set the strictly lower triangular or trapezoidal part of the
C        array to ALPHA.
C
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
C
      ELSE
C
C        Set the leading m-by-n submatrix to ALPHA.
C
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
C
C     Set the first min(M,N) diagonal elements to BETA.
C
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
C
      RETURN
C
C     End of SLASET
C
      END
      SUBROUTINE SLASQ1( N, D, E, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, N
C     ..
C     .. Array Arguments ..
      REAL               D( * ), E( * ), WORK( * )
C     ..
C
C     Purpose
C     =======
C
C     SLASQ1 computes the singular values of a real N-by-N bidiagonal
C     matrix with diagonal D and off-diagonal E. The singular values are
C     computed to high relative accuracy, barring over/underflow or
C     denormalization. The algorithm is described in
C
C     "Accurate singular values and differential qd algorithms," by
C     K. V. Fernando and B. N. Parlett,
C     Numer. Math., Vol-67, No. 2, pp. 191-230,1994.
C
C     See also
C     "Implementation of differential qd algorithms," by
C     K. V. Fernando and B. N. Parlett, Technical Report,
C     Department of Mathematics, University of California at Berkeley,
C     1994 (Under preparation).
C
C     Arguments
C     =========
C
C  N       (input) INTEGER
C          The number of rows and columns in the matrix. N >= 0.
C
C  D       (input/output) REAL array, dimension (N)
C          On entry, D contains the diagonal elements of the
C          bidiagonal matrix whose SVD is desired. On normal exit,
C          D contains the singular values in decreasing order.
C
C  E       (input/output) REAL array, dimension (N)
C          On entry, elements E(1:N-1) contain the off-diagonal elements
C          of the bidiagonal matrix whose SVD is desired.
C          On exit, E is overwritten.
C
C  WORK    (workspace) REAL array, dimension (2*N)
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C          > 0:  if INFO = i, the algorithm did not converge;  i
C                specifies how many superdiagonals did not converge.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               MEIGTH
      PARAMETER          ( MEIGTH = -0.125E0 )
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               TEN
      PARAMETER          ( TEN = 10.0E0 )
      REAL               HUNDRD
      PARAMETER          ( HUNDRD = 100.0E0 )
      REAL               TWO56
      PARAMETER          ( TWO56 = 256.0E0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            RESTRT
      INTEGER            I, IERR, J, KE, KEND, M, NY
      REAL               DM, DX, EPS, SCL, SFMIN, SIG1, SIG2, SIGMN,
     $                   SIGMX, SMALL2, THRESH, TOL, TOL2, TOLMUL
C     ..
C     .. External Functions ..
      REAL               SLAMCH
      EXTERNAL           SLAMCH
C     ..
C     .. External Subroutines ..
      EXTERNAL           SCOPY, SLAS2, SLASCL, SLASQ2, SLASRT, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, REAL, SQRT
C     ..
C     .. Executable Statements ..
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
         CALL XERBLA( 'SLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         CALL SLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
C
C     Estimate the largest singular value
C
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
C
C     Early return if sigmx is zero (matrix is already diagonal)
C
      IF( SIGMX.EQ.ZERO )
     $   GO TO 70
C
      DO 20 I = 1, N
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
C
C     Get machine parameters
C
      EPS = SLAMCH( 'EPSILON' )
      SFMIN = SLAMCH( 'SAFE MINIMUM' )
C
C     Compute singular values to relative accuracy TOL
C     It is assumed that tol**2 does not underflow.
C
      TOLMUL = MAX( TEN, MIN( HUNDRD, EPS**( -MEIGTH ) ) )
      TOL = TOLMUL*EPS
      TOL2 = TOL**2
C
      THRESH = SIGMX*SQRT( SFMIN )*TOL
C
C     Scale matrix so the square of the largest element is
C     1 / ( 256 * SFMIN )
C
      SCL = SQRT( ONE / ( TWO56*SFMIN ) )
      SMALL2 = ONE / ( TWO56*TOLMUL**2 )
      CALL SCOPY( N, D, 1, WORK( 1 ), 1 )
      CALL SCOPY( N-1, E, 1, WORK( N+1 ), 1 )
      CALL SLASCL( 'G', 0, 0, SIGMX, SCL, N, 1, WORK( 1 ), N, IERR )
      CALL SLASCL( 'G', 0, 0, SIGMX, SCL, N-1, 1, WORK( N+1 ), N-1,
     $             IERR )
C
C     Square D and E (the input for the qd algorithm)
C
      DO 30 J = 1, 2*N - 1
         WORK( J ) = WORK( J )**2
   30 CONTINUE
C
C     Apply qd algorithm
C
      M = 0
      E( N ) = ZERO
      DX = WORK( 1 )
      DM = DX
      KE = 0
      RESTRT = .FALSE.
      DO 60 I = 1, N
         IF( ABS( E( I ) ).LE.THRESH .OR. WORK( N+I ).LE.TOL2*
     $       ( DM / REAL( I-M ) ) ) THEN
            NY = I - M
            IF( NY.EQ.1 ) THEN
               GO TO 50
            ELSE IF( NY.EQ.2 ) THEN
               CALL SLAS2( D( M+1 ), E( M+1 ), D( M+2 ), SIG1, SIG2 )
               D( M+1 ) = SIG1
               D( M+2 ) = SIG2
            ELSE
               KEND = KE + 1 - M
               CALL SLASQ2( NY, D( M+1 ), E( M+1 ), WORK( M+1 ),
     $                      WORK( M+N+1 ), EPS, TOL2, SMALL2, DM, KEND,
     $                      INFO )
C
C                 Return, INFO = number of unconverged superdiagonals
C
               IF( INFO.NE.0 ) THEN
                  INFO = INFO + I
                  RETURN
               END IF
C
C                 Undo scaling
C
               DO 40 J = M + 1, M + NY
                  D( J ) = SQRT( D( J ) )
   40          CONTINUE
               CALL SLASCL( 'G', 0, 0, SCL, SIGMX, NY, 1, D( M+1 ), NY,
     $                      IERR )
            END IF
   50       CONTINUE
            M = I
            IF( I.NE.N ) THEN
               DX = WORK( I+1 )
               DM = DX
               KE = I
               RESTRT = .TRUE.
            END IF
         END IF
         IF( I.NE.N .AND. .NOT.RESTRT ) THEN
            DX = WORK( I+1 )*( DX / ( DX+WORK( N+I ) ) )
            IF( DM.GT.DX ) THEN
               DM = DX
               KE = I
            END IF
         END IF
         RESTRT = .FALSE.
   60 CONTINUE
      KEND = KE + 1
C
C     Sort the singular values into decreasing order
C
   70 CONTINUE
      CALL SLASRT( 'D', N, D, INFO )
      RETURN
C
C     End of SLASQ1
C
      END
      SUBROUTINE SLASQ2( M, Q, E, QQ, EE, EPS, TOL2, SMALL2, SUP, KEND,
     $                   INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, KEND, M
      REAL               EPS, SMALL2, SUP, TOL2
C     ..
C     .. Array Arguments ..
      REAL               E( * ), EE( * ), Q( * ), QQ( * )
C     ..
C
C     Purpose
C     =======
C
C     SLASQ2 computes the singular values of a real N-by-N unreduced
C     bidiagonal matrix with squared diagonal elements in Q and
C     squared off-diagonal elements in E. The singular values are
C     computed to relative accuracy TOL, barring over/underflow or
C     denormalization.
C
C     Arguments
C     =========
C
C  M       (input) INTEGER
C          The number of rows and columns in the matrix. M >= 0.
C
C  Q       (output) REAL array, dimension (M)
C          On normal exit, contains the squared singular values.
C
C  E       (workspace) REAL array, dimension (M)
C
C  QQ      (input/output) REAL array, dimension (M)
C          On entry, QQ contains the squared diagonal elements of the
C          bidiagonal matrix whose SVD is desired.
C          On exit, QQ is overwritten.
C
C  EE      (input/output) REAL array, dimension (M)
C          On entry, EE(1:N-1) contains the squared off-diagonal
C          elements of the bidiagonal matrix whose SVD is desired.
C          On exit, EE is overwritten.
C
C  EPS     (input) REAL
C          Machine epsilon.
C
C  TOL2    (input) REAL
C          Desired relative accuracy of computed eigenvalues
C          as defined in SLASQ1.
C
C  SMALL2  (input) REAL
C          A threshold value as defined in SLASQ1.
C
C  SUP     (input/output) REAL
C          Upper bound for the smallest eigenvalue.
C
C  KEND    (input/output) INTEGER
C          Index where minimum d occurs.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C          > 0:  if INFO = i, the algorithm did not converge;  i
C                specifies how many superdiagonals did not converge.
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
      REAL               FOUR, HALF
      PARAMETER          ( FOUR = 4.0E+0, HALF = 0.5E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            ICONV, IPHASE, ISP, N, OFF, OFF1
      REAL               QEMAX, SIGMA, XINF, XX, YY
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLASQ3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, NINT, SQRT
C     ..
C     .. Executable Statements ..
      N = M
C
C     Set the default maximum number of iterations
C
      OFF = 0
      OFF1 = OFF + 1
      SIGMA = ZERO
      XINF = ZERO
      ICONV = 0
      IPHASE = 2
C
C     Try deflation at the bottom
C
C     1x1 deflation
C
   10 CONTINUE
      IF( N.LE.2 )
     $   GO TO 20
      IF( EE( N-1 ).LE.MAX( QQ( N ), XINF, SMALL2 )*TOL2 ) THEN
         Q( N ) = QQ( N )
         N = N - 1
         IF( KEND.GT.N )
     $      KEND = N
         SUP = MIN( QQ( N ), QQ( N-1 ) )
         GO TO 10
      END IF
C
C     2x2 deflation
C
      IF( EE( N-2 ).LE.MAX( XINF, SMALL2,
     $    ( QQ( N ) / ( QQ( N )+EE( N-1 )+QQ( N-1 ) ) )*QQ( N-1 ) )*
     $    TOL2 ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.QQ( N-1 ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.QQ( N ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N-1 )-QQ( N )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*QQ( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( QQ( N ), QQ( N-1 ) ) / XX )*
     $           MIN( QQ( N ), QQ( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = XX
         Q( N ) = YY
         N = N - 2
         IF( KEND.GT.N )
     $      KEND = N
         SUP = QQ( N )
         GO TO 10
      END IF
C
   20 CONTINUE
      IF( N.EQ.0 ) THEN
C
C         The lower branch is finished
C
         IF( OFF.EQ.0 ) THEN
C
C         No upper branch; return to SLASQ1
C
            RETURN
         ELSE
C
C         Going back to upper branch
C
            XINF = ZERO
            IF( EE( OFF ).GT.ZERO ) THEN
               ISP = NINT( EE( OFF ) )
               IPHASE = 1
            ELSE
               ISP = -NINT( EE( OFF ) )
               IPHASE = 2
            END IF
            SIGMA = E( OFF )
            N = OFF - ISP + 1
            OFF1 = ISP
            OFF = OFF1 - 1
            IF( N.LE.2 )
     $         GO TO 20
            IF( IPHASE.EQ.1 ) THEN
               SUP = MIN( Q( N+OFF ), Q( N-1+OFF ), Q( N-2+OFF ) )
            ELSE
               SUP = MIN( QQ( N+OFF ), QQ( N-1+OFF ), QQ( N-2+OFF ) )
            END IF
            KEND = 0
            ICONV = -3
         END IF
      ELSE IF( N.EQ.1 ) THEN
C
C     1x1 Solver
C
         IF( IPHASE.EQ.1 ) THEN
            Q( OFF1 ) = Q( OFF1 ) + SIGMA
         ELSE
            Q( OFF1 ) = QQ( OFF1 ) + SIGMA
         END IF
         N = 0
         GO TO 20
C
C     2x2 Solver
C
      ELSE IF( N.EQ.2 ) THEN
         IF( IPHASE.EQ.2 ) THEN
            QEMAX = MAX( QQ( N+OFF ), QQ( N-1+OFF ), EE( N-1+OFF ) )
            IF( QEMAX.NE.ZERO ) THEN
               IF( QEMAX.EQ.QQ( N-1+OFF ) ) THEN
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N+OFF )-QQ( N-1+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*EE( OFF+N-1 ) /
     $                 QEMAX ) )
               ELSE IF( QEMAX.EQ.QQ( N+OFF ) ) THEN
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N-1+OFF )-QQ( N+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*EE( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE
                  XX = HALF*( QQ( N+OFF )+QQ( N-1+OFF )+EE( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( QQ( N+OFF )-QQ( N-1+OFF )+EE( N-
     $                 1+OFF ) ) / QEMAX )**2+FOUR*QQ( N-1+OFF ) /
     $                 QEMAX ) )
               END IF
               YY = ( MAX( QQ( N+OFF ), QQ( N-1+OFF ) ) / XX )*
     $              MIN( QQ( N+OFF ), QQ( N-1+OFF ) )
            ELSE
               XX = ZERO
               YY = ZERO
            END IF
         ELSE
            QEMAX = MAX( Q( N+OFF ), Q( N-1+OFF ), E( N-1+OFF ) )
            IF( QEMAX.NE.ZERO ) THEN
               IF( QEMAX.EQ.Q( N-1+OFF ) ) THEN
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N+OFF )-Q( N-1+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*E( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE IF( QEMAX.EQ.Q( N+OFF ) ) THEN
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N-1+OFF )-Q( N+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*E( N-1+OFF ) /
     $                 QEMAX ) )
               ELSE
                  XX = HALF*( Q( N+OFF )+Q( N-1+OFF )+E( N-1+OFF )+
     $                 QEMAX*SQRT( ( ( Q( N+OFF )-Q( N-1+OFF )+E( N-1+
     $                 OFF ) ) / QEMAX )**2+FOUR*Q( N-1+OFF ) /
     $                 QEMAX ) )
               END IF
               YY = ( MAX( Q( N+OFF ), Q( N-1+OFF ) ) / XX )*
     $              MIN( Q( N+OFF ), Q( N-1+OFF ) )
            ELSE
               XX = ZERO
               YY = ZERO
            END IF
         END IF
         Q( N-1+OFF ) = SIGMA + XX
         Q( N+OFF ) = YY + SIGMA
         N = 0
         GO TO 20
      END IF
      CALL SLASQ3( N, Q( OFF1 ), E( OFF1 ), QQ( OFF1 ), EE( OFF1 ), SUP,
     $             SIGMA, KEND, OFF, IPHASE, ICONV, EPS, TOL2, SMALL2 )
      IF( SUP.LT.ZERO ) THEN
         INFO = N + OFF
         RETURN
      END IF
      OFF1 = OFF + 1
      GO TO 20
C
C     End of SLASQ2
C
      END
      SUBROUTINE SLASQ3( N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
     $                   ICONV, EPS, TOL2, SMALL2 )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            ICONV, IPHASE, KEND, N, OFF
      REAL               EPS, SIGMA, SMALL2, SUP, TOL2
C     ..
C     .. Array Arguments ..
      REAL               E( * ), EE( * ), Q( * ), QQ( * )
C     ..
C
C     Purpose
C     =======
C
C     SLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
C     This can be described as the differential qd with shifts.
C
C     Arguments
C     =========
C
C  N       (input/output) INTEGER
C          On entry, N specifies the number of rows and columns
C          in the matrix. N must be at least 3.
C          On exit N is non-negative and less than the input value.
C
C  Q       (input/output) REAL array, dimension (N)
C          Q array in ping (see IPHASE below)
C
C  E       (input/output) REAL array, dimension (N)
C          E array in ping (see IPHASE below)
C
C  QQ      (input/output) REAL array, dimension (N)
C          Q array in pong (see IPHASE below)
C
C  EE      (input/output) REAL array, dimension (N)
C          E array in pong (see IPHASE below)
C
C  SUP     (input/output) REAL
C          Upper bound for the smallest eigenvalue
C
C  SIGMA   (input/output) REAL
C          Accumulated shift for the present submatrix
C
C  KEND    (input/output) INTEGER
C          Index where minimum D(i) occurs in recurrence for
C          splitting criterion
C
C  OFF     (input/output) INTEGER
C          Offset for arrays
C
C  IPHASE  (input/output) INTEGER
C          If IPHASE = 1 (ping) then data is in Q and E arrays
C          If IPHASE = 2 (pong) then data is in QQ and EE arrays
C
C  ICONV   (input) INTEGER
C          If ICONV = 0 a bottom part of a matrix (with a split)
C          If ICONV =-3 a top part of a matrix (with a split)
C
C  EPS     (input) REAL
C          Machine epsilon
C
C  TOL2    (input) REAL
C          Square of the relative tolerance TOL as defined in SLASQ1
C
C  SMALL2  (input) REAL
C          A threshold value as defined in SLASQ1
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
      INTEGER            NPP
      PARAMETER          ( NPP = 32 )
      INTEGER            IPP
      PARAMETER          ( IPP = 5 )
      REAL               HALF, FOUR
      PARAMETER          ( HALF = 0.5E+0, FOUR = 4.0E+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 2 )
C     ..
C     .. Local Scalars ..
      LOGICAL            LDEF, LSPLIT
      INTEGER            I, IC, ICNT, IFL, IP, ISP, K1END, K2END, KE,
     $                   KS, MAXIT, N1, N2
      REAL               D, DM, QEMAX, T1, TAU, TOLX, TOLY, TOLZ, XX, YY
C     ..
C     .. External Subroutines ..
      EXTERNAL           SCOPY, SLASQ4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
C     ..
C     .. Executable Statements ..
      ICNT = 0
      TAU = ZERO
      DM = SUP
      TOLX = SIGMA*TOL2
      TOLZ = MAX( SMALL2, SIGMA )*TOL2
C
C     Set maximum number of iterations
C
      MAXIT = 100*N
C
C     Flipping
C
      IC = 2
      IF( N.GT.3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            DO 10 I = 1, N - 2
               IF( Q( I ).GT.Q( I+1 ) )
     $            IC = IC + 1
               IF( E( I ).GT.E( I+1 ) )
     $            IC = IC + 1
   10       CONTINUE
            IF( Q( N-1 ).GT.Q( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL SCOPY( N, Q, 1, QQ, -1 )
               CALL SCOPY( N-1, E, 1, EE, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 2
            END IF
         ELSE
            DO 20 I = 1, N - 2
               IF( QQ( I ).GT.QQ( I+1 ) )
     $            IC = IC + 1
               IF( EE( I ).GT.EE( I+1 ) )
     $            IC = IC + 1
   20       CONTINUE
            IF( QQ( N-1 ).GT.QQ( N ) )
     $         IC = IC + 1
            IF( IC.LT.N ) THEN
               CALL SCOPY( N, QQ, 1, Q, -1 )
               CALL SCOPY( N-1, EE, 1, E, -1 )
               IF( KEND.NE.0 )
     $            KEND = N - KEND + 1
               IPHASE = 1
            END IF
         END IF
      END IF
      IF( ICONV.EQ.-3 ) THEN
         IF( IPHASE.EQ.1 ) THEN
            GO TO 180
         ELSE
            GO TO 80
         END IF
      END IF
      IF( IPHASE.EQ.2 )
     $   GO TO 130
C
C     The ping section of the code
C
   30 CONTINUE
      IFL = 0
C
C     Compute the shift
C
      IF( KEND.EQ.0 .OR. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL SLASQ4( N2, Q( N1 ), E( N1 ), TAU, SUP )
      END IF
   40 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -ONE
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
C
C     dqd algorithm
C
         D = Q( 1 )
         DM = D
         KE = 0
         DO 50 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
   50    CONTINUE
         KE = KE + 1
C
C     Penultimate dqd step (in ping)
C
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
C
C     Final dqd step (in ping)
C
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      ELSE
C
C     The dqds algorithm (in ping)
C
         D = Q( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 120
         DO 60 I = 1, N - 3
            QQ( I ) = D + E( I )
            D = ( D / QQ( I ) )*Q( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 120
            END IF
   60    CONTINUE
         KE = KE + 1
C
C     Penultimate dqds step (in ping)
C
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 120
         END IF
C
C     Final dqds step (in ping)
C
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      END IF
C
C        Convergence when QQ(N) is small (in ping)
C
      IF( ABS( QQ( N ) ).LE.SIGMA*TOL2 ) THEN
         QQ( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( QQ( N ).LT.ZERO )
     $   GO TO 120
C
C     Non-negative qd array: Update the e's
C
      DO 70 I = 1, N - 1
         EE( I ) = ( E( I ) / QQ( I ) )*Q( I+1 )
   70 CONTINUE
C
C     Updating sigma and iphase in ping
C
      SIGMA = SIGMA + TAU
      IPHASE = 2
   80 CONTINUE
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
      TOLZ = MAX( SIGMA, SMALL2 )*TOL2
C
C     Checking for deflation and convergence (in ping)
C
   90 CONTINUE
      IF( N.LE.2 )
     $   RETURN
C
C        Deflation: bottom 1x1 (in ping)
C
      LDEF = .FALSE.
      IF( EE( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( EE( N-1 ).LE.EPS*( SIGMA+QQ( N ) ) ) THEN
            IF( EE( N-1 )*( QQ( N ) / ( QQ( N )+SIGMA ) ).LE.TOL2*
     $          ( QQ( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( N-1 ).LE.QQ( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         Q( N ) = QQ( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 90
      END IF
C
C        Deflation: bottom 2x2 (in ping)
C
      LDEF = .FALSE.
      IF( EE( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + EE( N-1 )*( SIGMA / ( SIGMA+QQ( N ) ) )
         IF( EE( N-2 )*( T1 / ( QQ( N-1 )+T1 ) ).LE.TOLY ) THEN
            IF( EE( N-2 )*( QQ( N-1 ) / ( QQ( N-1 )+T1 ) ).LE.TOLX )
     $           THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( N-2 ).LE.( QQ( N ) / ( QQ( N )+EE( N-1 )+QQ( N-1 ) ) )*
     $       QQ( N-1 )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( QQ( N ), QQ( N-1 ), EE( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.QQ( N-1 ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.QQ( N ) ) THEN
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N-1 )-QQ( N )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*EE( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( QQ( N )+QQ( N-1 )+EE( N-1 )+QEMAX*
     $              SQRT( ( ( QQ( N )-QQ( N-1 )+EE( N-1 ) ) /
     $              QEMAX )**2+FOUR*QQ( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( QQ( N ), QQ( N-1 ) ) / XX )*
     $           MIN( QQ( N ), QQ( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 90
      END IF
C
C     Updating bounds before going to pong
C
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ), QQ( 1 ), QQ( 2 ),
     $         QQ( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
C
C     Checking for splitting in ping
C
      LSPLIT = .FALSE.
      DO 100 KS = N - 3, 3, -1
         IF( EE( KS ).LE.TOLY ) THEN
            IF( EE( KS )*( MIN( QQ( KS+1 ),
     $          QQ( KS ) ) / ( MIN( QQ( KS+1 ), QQ( KS ) )+SIGMA ) ).LE.
     $          TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 110
            END IF
         END IF
  100 CONTINUE
C
      KS = 2
      IF( EE( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + EE( 1 )*( SIGMA / ( SIGMA+QQ( 1 ) ) )
         IF( EE( 2 )*( T1 / ( QQ( 1 )+T1 ) ).LE.TOLY ) THEN
            IF( EE( 2 )*( QQ( 1 ) / ( QQ( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( 2 ).LE.( QQ( 1 ) / ( QQ( 1 )+EE( 1 )+QQ( 2 ) ) )*
     $       QQ( 2 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 110
C
      KS = 1
      IF( EE( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( EE( 1 ).LE.EPS*( SIGMA+QQ( 1 ) ) ) THEN
            IF( EE( 1 )*( QQ( 1 ) / ( QQ( 1 )+SIGMA ) ).LE.TOL2*
     $          ( QQ( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( EE( 1 ).LE.QQ( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
C
  110 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( QQ( N ), QQ( N-1 ), QQ( N-2 ) )
         ISP = -( OFF+1 )
         OFF = OFF + KS
         N = N - KS
         KEND = MAX( 1, KEND-KS )
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
C
C     Coincidence
C
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL SCOPY( N-KE, E( KE ), 1, QQ( KE ), 1 )
         QQ( N ) = ZERO
         CALL SCOPY( N-KE, Q( KE+1 ), 1, EE( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 130
C
C     A new shift when the previous failed (in ping)
C
  120 CONTINUE
      IFL = IFL + 1
      SUP = TAU
C
C     SUP is small or
C     Too many bad shifts (ping)
C
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 40
C
C     The asymptotic shift (in ping)
C
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 40
      END IF
C
C     the pong section of the code
C
  130 CONTINUE
      IFL = 0
C
C     Compute the shift (in pong)
C
      IF( KEND.EQ.0 .AND. SUP.EQ.ZERO ) THEN
         TAU = ZERO
      ELSE IF( ICNT.GT.0 .AND. DM.LE.TOLZ ) THEN
         TAU = ZERO
      ELSE
         IP = MAX( IPP, N / NPP )
         N2 = 2*IP + 1
         IF( N2.GE.N ) THEN
            N1 = 1
            N2 = N
         ELSE IF( KEND+IP.GT.N ) THEN
            N1 = N - 2*IP
         ELSE IF( KEND-IP.LT.1 ) THEN
            N1 = 1
         ELSE
            N1 = KEND - IP
         END IF
         CALL SLASQ4( N2, QQ( N1 ), EE( N1 ), TAU, SUP )
      END IF
  140 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -SUP
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
C
C     The dqd algorithm (in pong)
C
         D = QQ( 1 )
         DM = D
         KE = 0
         DO 150 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 )
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
            END IF
  150    CONTINUE
         KE = KE + 1
C
C     Penultimate dqd step (in pong)
C
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
C
C     Final dqd step (in pong)
C
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      ELSE
C
C     The dqds algorithm (in pong)
C
         D = QQ( 1 ) - TAU
         DM = D
         KE = 0
         IF( D.LT.ZERO )
     $      GO TO 220
         DO 160 I = 1, N - 3
            Q( I ) = D + EE( I )
            D = ( D / Q( I ) )*QQ( I+1 ) - TAU
            IF( DM.GT.D ) THEN
               DM = D
               KE = I
               IF( D.LT.ZERO )
     $            GO TO 220
            END IF
  160    CONTINUE
         KE = KE + 1
C
C     Penultimate dqds step (in pong)
C
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 220
         END IF
C
C     Final dqds step (in pong)
C
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      END IF
C
C        Convergence when is small (in pong)
C
      IF( ABS( Q( N ) ).LE.SIGMA*TOL2 ) THEN
         Q( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( Q( N ).LT.ZERO )
     $   GO TO 220
C
C     Non-negative qd array: Update the e's
C
      DO 170 I = 1, N - 1
         E( I ) = ( EE( I ) / Q( I ) )*QQ( I+1 )
  170 CONTINUE
C
C     Updating sigma and iphase in pong
C
      SIGMA = SIGMA + TAU
  180 CONTINUE
      IPHASE = 1
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
C
C     Checking for deflation and convergence (in pong)
C
  190 CONTINUE
      IF( N.LE.2 )
     $   RETURN
C
C        Deflation: bottom 1x1 (in pong)
C
      LDEF = .FALSE.
      IF( E( N-1 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( E( N-1 ).LE.EPS*( SIGMA+Q( N ) ) ) THEN
            IF( E( N-1 )*( Q( N ) / ( Q( N )+SIGMA ) ).LE.TOL2*
     $          ( Q( N )+SIGMA ) ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( N-1 ).LE.Q( N )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         Q( N ) = Q( N ) + SIGMA
         N = N - 1
         ICONV = ICONV + 1
         GO TO 190
      END IF
C
C        Deflation: bottom 2x2 (in pong)
C
      LDEF = .FALSE.
      IF( E( N-2 ).LE.TOLZ ) THEN
         LDEF = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + E( N-1 )*( SIGMA / ( SIGMA+Q( N ) ) )
         IF( E( N-2 )*( T1 / ( Q( N-1 )+T1 ) ).LE.TOLY ) THEN
            IF( E( N-2 )*( Q( N-1 ) / ( Q( N-1 )+T1 ) ).LE.TOLX ) THEN
               LDEF = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( N-2 ).LE.( Q( N ) / ( Q( N )+EE( N-1 )+Q( N-1 ) )*Q( N-
     $       1 ) )*TOL2 ) THEN
            LDEF = .TRUE.
         END IF
      END IF
      IF( LDEF ) THEN
         QEMAX = MAX( Q( N ), Q( N-1 ), E( N-1 ) )
         IF( QEMAX.NE.ZERO ) THEN
            IF( QEMAX.EQ.Q( N-1 ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE IF( QEMAX.EQ.Q( N ) ) THEN
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N-1 )-Q( N )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*E( N-1 ) / QEMAX ) )
            ELSE
               XX = HALF*( Q( N )+Q( N-1 )+E( N-1 )+QEMAX*
     $              SQRT( ( ( Q( N )-Q( N-1 )+E( N-1 ) ) / QEMAX )**2+
     $              FOUR*Q( N-1 ) / QEMAX ) )
            END IF
            YY = ( MAX( Q( N ), Q( N-1 ) ) / XX )*
     $           MIN( Q( N ), Q( N-1 ) )
         ELSE
            XX = ZERO
            YY = ZERO
         END IF
         Q( N-1 ) = SIGMA + XX
         Q( N ) = YY + SIGMA
         N = N - 2
         ICONV = ICONV + 2
         GO TO 190
      END IF
C
C     Updating bounds before going to pong
C
      IF( ICONV.EQ.0 ) THEN
         KEND = KE
         SUP = MIN( DM, SUP-TAU )
      ELSE IF( ICONV.GT.0 ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ), Q( 1 ), Q( 2 ), Q( 3 ) )
         IF( ICONV.EQ.1 ) THEN
            KEND = K1END
         ELSE IF( ICONV.EQ.2 ) THEN
            KEND = K2END
         ELSE
            KEND = N
         END IF
         ICNT = 0
         MAXIT = 100*N
      END IF
C
C     Checking for splitting in pong
C
      LSPLIT = .FALSE.
      DO 200 KS = N - 3, 3, -1
         IF( E( KS ).LE.TOLY ) THEN
            IF( E( KS )*( MIN( Q( KS+1 ), Q( KS ) ) / ( MIN( Q( KS+1 ),
     $          Q( KS ) )+SIGMA ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
               GO TO 210
            END IF
         END IF
  200 CONTINUE
C
      KS = 2
      IF( E( 2 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         T1 = SIGMA + E( 1 )*( SIGMA / ( SIGMA+Q( 1 ) ) )
         IF( E( 2 )*( T1 / ( Q( 1 )+T1 ) ).LE.TOLY ) THEN
            IF( E( 2 )*( Q( 1 ) / ( Q( 1 )+T1 ) ).LE.TOLX ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( 2 ).LE.( Q( 1 ) / ( Q( 1 )+E( 1 )+Q( 2 ) ) )*Q( 2 )*
     $       TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
      IF( LSPLIT )
     $   GO TO 210
C
      KS = 1
      IF( E( 1 ).LE.TOLZ ) THEN
         LSPLIT = .TRUE.
      ELSE IF( SIGMA.GT.ZERO ) THEN
         IF( E( 1 ).LE.EPS*( SIGMA+Q( 1 ) ) ) THEN
            IF( E( 1 )*( Q( 1 ) / ( Q( 1 )+SIGMA ) ).LE.TOL2*
     $          ( Q( 1 )+SIGMA ) ) THEN
               LSPLIT = .TRUE.
            END IF
         END IF
      ELSE
         IF( E( 1 ).LE.Q( 1 )*TOL2 ) THEN
            LSPLIT = .TRUE.
         END IF
      END IF
C
  210 CONTINUE
      IF( LSPLIT ) THEN
         SUP = MIN( Q( N ), Q( N-1 ), Q( N-2 ) )
         ISP = OFF + 1
         OFF = OFF + KS
         KEND = MAX( 1, KEND-KS )
         N = N - KS
         E( KS ) = SIGMA
         EE( KS ) = ISP
         ICONV = 0
         RETURN
      END IF
C
C     Coincidence
C
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL SCOPY( N-KE, EE( KE ), 1, Q( KE ), 1 )
         Q( N ) = ZERO
         CALL SCOPY( N-KE, QQ( KE+1 ), 1, E( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 30
C
C     Computation of a new shift when the previous failed (in pong)
C
  220 CONTINUE
      IFL = IFL + 1
      SUP = TAU
C
C     SUP is small or
C     Too many bad shifts (in pong)
C
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 140
C
C     The asymptotic shift (in pong)
C
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 140
      END IF
C
C     End of SLASQ3
C
      END
      SUBROUTINE SLASQ4( N, Q, E, TAU, SUP )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            N
      REAL               SUP, TAU
C     ..
C     .. Array Arguments ..
      REAL               E( * ), Q( * )
C     ..
C
C     Purpose
C     =======
C
C     SLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This
C     routine improves the input value of SUP which is an upper bound
C     for the smallest eigenvalue for this matrix .
C
C     Arguments
C     =========
C
C  N       (input) INTEGER
C          On entry, N specifies the number of rows and columns
C          in the matrix. N must be at least 0.
C
C  Q       (input) REAL array, dimension (N)
C          Q array
C
C  E       (input) REAL array, dimension (N)
C          E array
C
C  TAU     (output) REAL
C          Estimate of the shift
C
C  SUP     (input/output) REAL
C          Upper bound for the smallest singular value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
      REAL               BIS, BIS1
      PARAMETER          ( BIS = 0.9999E+0, BIS1 = 0.7E+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 5 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IFL
      REAL               D, DM, XINF
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
      IFL = 1
      SUP = MIN( SUP, Q( 1 ), Q( 2 ), Q( 3 ), Q( N ), Q( N-1 ),
     $      Q( N-2 ) )
      TAU = SUP*BIS
      XINF = ZERO
   10 CONTINUE
      IF( IFL.EQ.IFLMAX ) THEN
         TAU = XINF
         RETURN
      END IF
      D = Q( 1 ) - TAU
      DM = D
      DO 20 I = 1, N - 2
         D = ( D / ( D+E( I ) ) )*Q( I+1 ) - TAU
         IF( DM.GT.D )
     $      DM = D
         IF( D.LT.ZERO ) THEN
            SUP = TAU
            TAU = MAX( SUP*BIS1**IFL, D+TAU )
            IFL = IFL + 1
            GO TO 10
         END IF
   20 CONTINUE
      D = ( D / ( D+E( N-1 ) ) )*Q( N ) - TAU
      IF( DM.GT.D )
     $   DM = D
      IF( D.LT.ZERO ) THEN
         SUP = TAU
         XINF = MAX( XINF, D+TAU )
         IF( SUP*BIS1**IFL.LE.XINF ) THEN
            TAU = XINF
         ELSE
            TAU = SUP*BIS1**IFL
            IFL = IFL + 1
            GO TO 10
         END IF
      ELSE
         SUP = MIN( SUP, DM+TAU )
      END IF
      RETURN
C
C     End of SLASQ4
C
      END
      SUBROUTINE SLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( * ), S( * )
C     ..
C
C  Purpose
C  =======
C
C  SLASR   performs the transformation
C
C     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )
C
C     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )
C
C  where A is an m by n real matrix and P is an orthogonal matrix,
C  consisting of a sequence of plane rotations determined by the
C  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
C  and z = n when SIDE = 'R' or 'r' ):
C
C  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
C
C     P = P( z - 1 )*...*P( 2 )*P( 1 ),
C
C  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
C
C     P = P( 1 )*P( 2 )*...*P( z - 1 ),
C
C  where  P( k ) is a plane rotation matrix for the following planes:
C
C     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
C        the plane ( k, k + 1 )
C
C     when  PIVOT = 'T' or 't'  ( Top pivot ),
C        the plane ( 1, k + 1 )
C
C     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
C        the plane ( k, z )
C
C  c( k ) and s( k )  must contain the  cosine and sine that define the
C  matrix  P( k ).  The two by two plane rotation part of the matrix
C  P( k ), R( k ), is assumed to be of the form
C
C     R( k ) = (  c( k )  s( k ) ).
C              ( -s( k )  c( k ) )
C
C  This version vectorises across rows of the array A when SIDE = 'L'.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          Specifies whether the plane rotation matrix P is applied to
C          A on the left or the right.
C          = 'L':  Left, compute A := P*A
C          = 'R':  Right, compute A:= A*P'
C
C  DIRECT  (input) CHARACTER*1
C          Specifies whether P is a forward or backward sequence of
C          plane rotations.
C          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
C          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
C
C  PIVOT   (input) CHARACTER*1
C          Specifies the plane for which P(k) is a plane rotation
C          matrix.
C          = 'V':  Variable pivot, the plane (k,k+1)
C          = 'T':  Top pivot, the plane (1,k+1)
C          = 'B':  Bottom pivot, the plane (k,z)
C
C  M       (input) INTEGER
C          The number of rows of the matrix A.  If m <= 1, an immediate
C          return is effected.
C
C  N       (input) INTEGER
C          The number of columns of the matrix A.  If n <= 1, an
C          immediate return is effected.
C
C  C, S    (input) REAL arrays, dimension
C                  (M-1) if SIDE = 'L'
C                  (N-1) if SIDE = 'R'
C          c(k) and s(k) contain the cosine and sine that define the
C          matrix P(k).  The two by two plane rotation part of the
C          matrix P(k), R(k), is assumed to be of the form
C          R( k ) = (  c( k )  s( k ) ).
C                   ( -s( k )  c( k ) )
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          The m by n matrix A.  On exit, A is overwritten by P*A if
C          SIDE = 'R' or by A*P' if SIDE = 'L'.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.  LDA >= max(1,M).
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, INFO, J
      REAL               CTEMP, STEMP, TEMP
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters
C
      INFO = 0
      IF( .NOT.( LSAME( SIDE, 'L' ) .OR. LSAME( SIDE, 'R' ) ) ) THEN
         INFO = 1
      ELSE IF( .NOT.( LSAME( PIVOT, 'V' ) .OR. LSAME( PIVOT,
     $         'T' ) .OR. LSAME( PIVOT, 'B' ) ) ) THEN
         INFO = 2
      ELSE IF( .NOT.( LSAME( DIRECT, 'F' ) .OR. LSAME( DIRECT, 'B' ) ) )
     $          THEN
         INFO = 3
      ELSE IF( M.LT.0 ) THEN
         INFO = 4
      ELSE IF( N.LT.0 ) THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLASR ', INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
C
C        Form  P * A
C
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 20 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 10 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 40 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 30 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   30                CONTINUE
                  END IF
   40          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 60 J = 2, M
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 50 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 80 J = M, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 70 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   70                CONTINUE
                  END IF
   80          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 100 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 90 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
   90                CONTINUE
                  END IF
  100          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 120 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 110 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
  110                CONTINUE
                  END IF
  120          CONTINUE
            END IF
         END IF
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
C
C        Form A * P'
C
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 140 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 130 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  130                CONTINUE
                  END IF
  140          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 160 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 150 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  150                CONTINUE
                  END IF
  160          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 180 J = 2, N
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 170 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  170                CONTINUE
                  END IF
  180          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 200 J = N, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 190 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  190                CONTINUE
                  END IF
  200          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 220 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 210 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 240 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 230 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  230                CONTINUE
                  END IF
  240          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of SLASR
C
      END
      SUBROUTINE SLASRT( ID, N, D, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          ID
      INTEGER            INFO, N
C     ..
C     .. Array Arguments ..
      REAL               D( * )
C     ..
C
C  Purpose
C  =======
C
C  Sort the numbers in D in increasing order (if ID = 'I') or
C  in decreasing order (if ID = 'D' ).
C
C  Use Quick Sort, reverting to Insertion sort on arrays of
C  size <= 20. Dimension of STACK limits N to about 2**32.
C
C  Arguments
C  =========
C
C  ID      (input) CHARACTER*1
C          = 'I': sort D in increasing order;
C          = 'D': sort D in decreasing order.
C
C  N       (input) INTEGER
C          The length of the array D.
C
C  D       (input/output) REAL array, dimension (N)
C          On entry, the array to be sorted.
C          On exit, D has been sorted into increasing order
C          (D(1) <= ... <= D(N) ) or into decreasing order
C          (D(1) >= ... >= D(N) ), depending on ID.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
C     ..
C     .. Local Scalars ..
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      REAL               D1, D2, D3, DMNMX, TMP
C     ..
C     .. Local Arrays ..
      INTEGER            STACK( 2, 32 )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     ..
C     .. Executable Statements ..
C
C     Test the input paramters.
C
      INFO = 0
      DIR = -1
      IF( LSAME( ID, 'D' ) ) THEN
         DIR = 0
      ELSE IF( LSAME( ID, 'I' ) ) THEN
         DIR = 1
      END IF
      IF( DIR.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SLASRT', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.LE.1 )
     $   RETURN
C
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
C
C        Do Insertion sort on D( START:ENDD )
C
         IF( DIR.EQ.0 ) THEN
C
C           Sort into decreasing order
C
            DO 30 I = START + 1, ENDD
               DO 20 J = I, START + 1, -1
                  IF( D( J ).GT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 30
                  END IF
   20          CONTINUE
   30       CONTINUE
C
         ELSE
C
C           Sort into increasing order
C
            DO 50 I = START + 1, ENDD
               DO 40 J = I, START + 1, -1
                  IF( D( J ).LT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 50
                  END IF
   40          CONTINUE
   50       CONTINUE
C
         END IF
C
      ELSE IF( ENDD-START.GT.SELECT ) THEN
C
C        Partition D( START:ENDD ) and stack parts, largest one first
C
C        Choose partition entry as median of 3
C
         D1 = D( START )
         D2 = D( ENDD )
         I = ( START+ENDD ) / 2
         D3 = D( I )
         IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
               DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D2
            END IF
         ELSE
            IF( D3.LT.D2 ) THEN
               DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D1
            END IF
         END IF
C
         IF( DIR.EQ.0 ) THEN
C
C           Sort into decreasing order
C
            I = START - 1
            J = ENDD + 1
   60       CONTINUE
   70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX )
     $         GO TO 70
   80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX )
     $         GO TO 80
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 60
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         ELSE
C
C           Sort into increasing order
C
            I = START - 1
            J = ENDD + 1
   90       CONTINUE
  100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX )
     $         GO TO 100
  110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX )
     $         GO TO 110
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 90
            END IF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            END IF
         END IF
      END IF
      IF( STKPNT.GT.0 )
     $   GO TO 10
      RETURN
C
C     End of SLASRT
C
      END
      SUBROUTINE SLASSQ( N, X, INCX, SCALE, SUMSQ )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INCX, N
      REAL               SCALE, SUMSQ
C     ..
C     .. Array Arguments ..
      REAL               X( * )
C     ..
C
C  Purpose
C  =======
C
C  SLASSQ  returns the values  scl  and  smsq  such that
C
C     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
C
C  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
C  assumed to be non-negative and  scl  returns the value
C
C     scl = max( scale, abs( x( i ) ) ).
C
C  scale and sumsq must be supplied in SCALE and SUMSQ and
C  scl and smsq are overwritten on SCALE and SUMSQ respectively.
C
C  The routine makes only one pass through the vector x.
C
C  Arguments
C  =========
C
C  N       (input) INTEGER
C          The number of elements to be used from the vector X.
C
C  X       (input) REAL
C          The vector for which a scaled sum of squares is computed.
C             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
C
C  INCX    (input) INTEGER
C          The increment between successive values of the vector X.
C          INCX > 0.
C
C  SCALE   (input/output) REAL
C          On entry, the value  scale  in the equation above.
C          On exit, SCALE is overwritten with  scl , the scaling factor
C          for the sum of squares.
C
C  SUMSQ   (input/output) REAL
C          On entry, the value  sumsq  in the equation above.
C          On exit, SUMSQ is overwritten with  smsq , the basic sum of
C          squares from which  scl  has been factored out.
C
C =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            IX
      REAL               ABSXI
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS
C     ..
C     .. Executable Statements ..
C
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( X( IX ).NE.ZERO ) THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
C
C     End of SLASSQ
C
      END
      SUBROUTINE SLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      REAL               CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
C     ..
C
C  Purpose
C  =======
C
C  SLASV2 computes the singular value decomposition of a 2-by-2
C  triangular matrix
C     [  F   G  ]
C     [  0   H  ].
C  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
C  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
C  right singular vectors for abs(SSMAX), giving the decomposition
C
C     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
C     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
C
C  Arguments
C  =========
C
C  F       (input) REAL
C          The (1,1) element of the 2-by-2 matrix.
C
C  G       (input) REAL
C          The (1,2) element of the 2-by-2 matrix.
C
C  H       (input) REAL
C          The (2,2) element of the 2-by-2 matrix.
C
C  SSMIN   (output) REAL
C          abs(SSMIN) is the smaller singular value.
C
C  SSMAX   (output) REAL
C          abs(SSMAX) is the larger singular value.
C
C  SNL     (output) REAL
C  CSL     (output) REAL
C          The vector (CSL, SNL) is a unit left singular vector for the
C          singular value abs(SSMAX).
C
C  SNR     (output) REAL
C  CSR     (output) REAL
C          The vector (CSR, SNR) is a unit right singular vector for the
C          singular value abs(SSMAX).
C
C  Further Details
C  ===============
C
C  Any input parameter may be aliased with any output parameter.
C
C  Barring over/underflow and assuming a guard digit in subtraction, all
C  output quantities are correct to within a few units in the last
C  place (ulps).
C
C  In IEEE arithmetic, the code works correctly if one matrix element is
C  infinite.
C
C  Overflow will not occur unless the largest singular value itself
C  overflows or is within a few ulps of overflow. (On machines with
C  partial overflow, like the Cray, overflow may occur if the largest
C  singular value is within a factor of 2 of overflow.)
C
C  Underflow is harmless if underflow is gradual. Otherwise, results
C  may correspond to a matrix modified by perturbations of size near
C  the underflow threshold.
C
C =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
      REAL               HALF
      PARAMETER          ( HALF = 0.5E0 )
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0E0 )
      REAL               FOUR
      PARAMETER          ( FOUR = 4.0E0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            GASMAL, SWAP
      INTEGER            PMAX
      REAL               A, CLT, CRT, D, FA, FT, GA, GT, HA, HT, L, M,
     $                   MM, R, S, SLT, SRT, T, TEMP, TSIGN, TT
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
C     ..
C     .. External Functions ..
      REAL               SLAMCH
      EXTERNAL           SLAMCH
C     ..
C     .. Executable Statements ..
C
      FT = F
      FA = ABS( FT )
      HT = H
      HA = ABS( H )
C
C     PMAX points to the maximum absolute element of matrix
C       PMAX = 1 if F largest in absolute values
C       PMAX = 2 if G largest in absolute values
C       PMAX = 3 if H largest in absolute values
C
      PMAX = 1
      SWAP = ( HA.GT.FA )
      IF( SWAP ) THEN
         PMAX = 3
         TEMP = FT
         FT = HT
         HT = TEMP
         TEMP = FA
         FA = HA
         HA = TEMP
C
C        Now FA .ge. HA
C
      END IF
      GT = G
      GA = ABS( GT )
      IF( GA.EQ.ZERO ) THEN
C
C        Diagonal matrix
C
         SSMIN = HA
         SSMAX = FA
         CLT = ONE
         CRT = ONE
         SLT = ZERO
         SRT = ZERO
      ELSE
         GASMAL = .TRUE.
         IF( GA.GT.FA ) THEN
            PMAX = 2
            IF( ( FA / GA ).LT.SLAMCH( 'EPS' ) ) THEN
C
C              Case of very large GA
C
               GASMAL = .FALSE.
               SSMAX = GA
               IF( HA.GT.ONE ) THEN
                  SSMIN = FA / ( GA / HA )
               ELSE
                  SSMIN = ( FA / GA )*HA
               END IF
               CLT = ONE
               SLT = HT / GT
               SRT = ONE
               CRT = FT / GT
            END IF
         END IF
         IF( GASMAL ) THEN
C
C           Normal case
C
            D = FA - HA
            IF( D.EQ.FA ) THEN
C
C              Copes with infinite F or H
C
               L = ONE
            ELSE
               L = D / FA
            END IF
C
C           Note that 0 .le. L .le. 1
C
            M = GT / FT
C
C           Note that abs(M) .le. 1/macheps
C
            T = TWO - L
C
C           Note that T .ge. 1
C
            MM = M*M
            TT = T*T
            S = SQRT( TT+MM )
C
C           Note that 1 .le. S .le. 1 + 1/macheps
C
            IF( L.EQ.ZERO ) THEN
               R = ABS( M )
            ELSE
               R = SQRT( L*L+MM )
            END IF
C
C           Note that 0 .le. R .le. 1 + 1/macheps
C
            A = HALF*( S+R )
C
C           Note that 1 .le. A .le. 1 + abs(M)
C
            SSMIN = HA / A
            SSMAX = FA*A
            IF( MM.EQ.ZERO ) THEN
C
C              Note that M is very tiny
C
               IF( L.EQ.ZERO ) THEN
                  T = SIGN( TWO, FT )*SIGN( ONE, GT )
               ELSE
                  T = GT / SIGN( D, FT ) + M / T
               END IF
            ELSE
               T = ( M / ( S+T )+M / ( R+L ) )*( ONE+A )
            END IF
            L = SQRT( T*T+FOUR )
            CRT = TWO / L
            SRT = T / L
            CLT = ( CRT+SRT*M ) / A
            SLT = ( HT / FT )*SRT / A
         END IF
      END IF
      IF( SWAP ) THEN
         CSL = SRT
         SNL = CRT
         CSR = SLT
         SNR = CLT
      ELSE
         CSL = CLT
         SNL = SLT
         CSR = CRT
         SNR = SRT
      END IF
C
C     Correct signs of SSMAX and SSMIN
C
      IF( PMAX.EQ.1 )
     $   TSIGN = SIGN( ONE, CSR )*SIGN( ONE, CSL )*SIGN( ONE, F )
      IF( PMAX.EQ.2 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, CSL )*SIGN( ONE, G )
      IF( PMAX.EQ.3 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, SNL )*SIGN( ONE, H )
      SSMAX = SIGN( SSMAX, TSIGN )
      SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) )
      RETURN
C
C     End of SLASV2
C
      END
      SUBROUTINE SORG2R( M, N, K, A, LDA, TAU, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SORG2R generates an m by n real matrix Q with orthonormal columns,
C  which is defined as the first n columns of a product of k elementary
C  reflectors of order m
C
C        Q  =  H(1) H(2) . . . H(k)
C
C  as returned by SGEQRF.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix Q. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix Q. M >= N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines the
C          matrix Q. N >= K >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the i-th column must contain the vector which
C          defines the elementary reflector H(i), for i = 1,2,...,k, as
C          returned by SGEQRF in the first k columns of its array
C          argument A.
C          On exit, the m-by-n matrix Q.
C
C  LDA     (input) INTEGER
C          The first dimension of the array A. LDA >= max(1,M).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGEQRF.
C
C  WORK    (workspace) REAL array, dimension (N)
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument has an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J, L
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, SSCAL, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORG2R', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.LE.0 )
     $   RETURN
C
C     Initialise columns k+1:n to columns of the unit matrix
C
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( J, J ) = ONE
   20 CONTINUE
C
      DO 40 I = K, 1, -1
C
C        Apply H(i) to A(i:m,i:n) from the left
C
         IF( I.LT.N ) THEN
            A( I, I ) = ONE
            CALL SLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
         IF( I.LT.M )
     $      CALL SSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
C
C        Set A(1:i-1,i) to zero
C
         DO 30 L = 1, I - 1
            A( L, I ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
C
C     End of SORG2R
C
      END
      SUBROUTINE SORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          VECT
      INTEGER            INFO, K, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SORGBR generates one of the real orthogonal matrices Q or P**T
C  determined by SGEBRD when reducing a real matrix A to bidiagonal
C  form: A = Q * B * P**T.  Q and P**T are defined as products of
C  elementary reflectors H(i) or G(i) respectively.
C
C  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
C  is of order M:
C  if m >= k, Q = H(1) H(2) . . . H(k) and SORGBR returns the first n
C  columns of Q, where m >= n >= k;
C  if m < k, Q = H(1) H(2) . . . H(m-1) and SORGBR returns Q as an
C  M-by-M matrix.
C
C  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
C  is of order N:
C  if k < n, P**T = G(k) . . . G(2) G(1) and SORGBR returns the first m
C  rows of P**T, where n >= m >= k;
C  if k >= n, P**T = G(n-1) . . . G(2) G(1) and SORGBR returns P**T as
C  an N-by-N matrix.
C
C  Arguments
C  =========
C
C  VECT    (input) CHARACTER*1
C          Specifies whether the matrix Q or the matrix P**T is
C          required, as defined in the transformation applied by SGEBRD:
C          = 'Q':  generate Q;
C          = 'P':  generate P**T.
C
C  M       (input) INTEGER
C          The number of rows of the matrix Q or P**T to be returned.
C          M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix Q or P**T to be returned.
C          N >= 0.
C          If VECT = 'Q', M >= N >= min(M,K);
C          if VECT = 'P', N >= M >= min(N,K).
C
C  K       (input) INTEGER
C          If VECT = 'Q', the number of columns in the original M-by-K
C          matrix reduced by SGEBRD.
C          If VECT = 'P', the number of rows in the original K-by-N
C          matrix reduced by SGEBRD.
C          K >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the vectors which define the elementary reflectors,
C          as returned by SGEBRD.
C          On exit, the M-by-N matrix Q or P**T.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A. LDA >= max(1,M).
C
C  TAU     (input) REAL array, dimension
C                                (min(M,K)) if VECT = 'Q'
C                                (min(N,K)) if VECT = 'P'
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i) or G(i), which determines Q or P**T, as
C          returned by SGEBRD in its array argument TAUQ or TAUP.
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
C          For optimum performance LWORK >= min(M,N)*NB, where NB
C          is the optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            WANTQ
      INTEGER            I, IINFO, J
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SORGLQ, SORGQR, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      WANTQ = LSAME( VECT, 'Q' )
      IF( .NOT.WANTQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. ( WANTQ .AND. ( N.GT.M .OR. N.LT.MIN( M,
     $         K ) ) ) .OR. ( .NOT.WANTQ .AND. ( M.GT.N .OR. M.LT.
     $         MIN( N, K ) ) ) ) THEN
         INFO = -3
      ELSE IF( K.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LWORK.LT.MAX( 1, MIN( M, N ) ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORGBR', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
      IF( WANTQ ) THEN
C
C        Form Q, determined by a call to SGEBRD to reduce an m-by-k
C        matrix
C
         IF( M.GE.K ) THEN
C
C           If m >= k, assume m >= n >= k
C
            CALL SORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
C
         ELSE
C
C           If m < k, assume m = n
C
C           Shift the vectors which define the elementary reflectors one
C           column to the right, and set the first row and column of Q
C           to those of the unit matrix
C
            DO 20 J = M, 2, -1
               A( 1, J ) = ZERO
               DO 10 I = J + 1, M
                  A( I, J ) = A( I, J-1 )
   10          CONTINUE
   20       CONTINUE
            A( 1, 1 ) = ONE
            DO 30 I = 2, M
               A( I, 1 ) = ZERO
   30       CONTINUE
            IF( M.GT.1 ) THEN
C
C              Form Q(2:m,2:m)
C
               CALL SORGQR( M-1, M-1, M-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      ELSE
C
C        Form P', determined by a call to SGEBRD to reduce a k-by-n
C        matrix
C
         IF( K.LT.N ) THEN
C
C           If k < n, assume k <= m <= n
C
            CALL SORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
C
         ELSE
C
C           If k >= n, assume m = n
C
C           Shift the vectors which define the elementary reflectors one
C           row downward, and set the first row and column of P' to
C           those of the unit matrix
C
            A( 1, 1 ) = ONE
            DO 40 I = 2, N
               A( I, 1 ) = ZERO
   40       CONTINUE
            DO 60 J = 2, N
               DO 50 I = J - 1, 2, -1
                  A( I, J ) = A( I-1, J )
   50          CONTINUE
               A( 1, J ) = ZERO
   60       CONTINUE
            IF( N.GT.1 ) THEN
C
C              Form P'(2:n,2:n)
C
               CALL SORGLQ( N-1, N-1, N-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      END IF
      RETURN
C
C     End of SORGBR
C
      END
      SUBROUTINE SORGL2( M, N, K, A, LDA, TAU, WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SORGL2 generates an m by n real matrix Q with orthonormal rows,
C  which is defined as the first m rows of a product of k elementary
C  reflectors of order n
C
C        Q  =  H(k) . . . H(2) H(1)
C
C  as returned by SGELQF.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix Q. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix Q. N >= M.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines the
C          matrix Q. M >= K >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the i-th row must contain the vector which defines
C          the elementary reflector H(i), for i = 1,2,...,k, as returned
C          by SGELQF in the first k rows of its array argument A.
C          On exit, the m-by-n matrix Q.
C
C  LDA     (input) INTEGER
C          The first dimension of the array A. LDA >= max(1,M).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGELQF.
C
C  WORK    (workspace) REAL array, dimension (M)
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument has an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, J, L
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, SSCAL, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORGL2', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.LE.0 )
     $   RETURN
C
      IF( K.LT.M ) THEN
C
C        Initialise rows k+1:m to rows of the unit matrix
C
         DO 20 J = 1, N
            DO 10 L = K + 1, M
               A( L, J ) = ZERO
   10       CONTINUE
            IF( J.GT.K .AND. J.LE.M )
     $         A( J, J ) = ONE
   20    CONTINUE
      END IF
C
      DO 40 I = K, 1, -1
C
C        Apply H(i) to A(i:m,i:n) from the right
C
         IF( I.LT.N ) THEN
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
               CALL SLARF( 'Right', M-I, N-I+1, A( I, I ), LDA,
     $                     TAU( I ), A( I+1, I ), LDA, WORK )
            END IF
            CALL SSCAL( N-I, -TAU( I ), A( I, I+1 ), LDA )
         END IF
         A( I, I ) = ONE - TAU( I )
C
C        Set A(1:i-1,i) to zero
C
         DO 30 L = 1, I - 1
            A( I, L ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
C
C     End of SORGL2
C
      END
      SUBROUTINE SORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SORGLQ generates an M-by-N real matrix Q with orthonormal rows,
C  which is defined as the first M rows of a product of K elementary
C  reflectors of order N
C
C        Q  =  H(k) . . . H(2) H(1)
C
C  as returned by SGELQF.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix Q. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix Q. N >= M.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines the
C          matrix Q. M >= K >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the i-th row must contain the vector which defines
C          the elementary reflector H(i), for i = 1,2,...,k, as returned
C          by SGELQF in the first k rows of its array argument A.
C          On exit, the M-by-N matrix Q.
C
C  LDA     (input) INTEGER
C          The first dimension of the array A. LDA >= max(1,M).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGELQF.
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK. LWORK >= max(1,M).
C          For optimum performance LWORK >= M*NB, where NB is
C          the optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument has an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARFB, SLARFT, SORGL2, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, M ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORGLQ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.
C
      NB = ILAENV( 1, 'SORGLQ', ' ', M, N, K, -1 )
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
C
C        Determine when to cross over from blocked to unblocked code.
C
         NX = MAX( 0, ILAENV( 3, 'SORGLQ', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
C
C           Determine if workspace is large enough for blocked code.
C
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
C
C              Not enough workspace to use optimal NB:  reduce NB and
C              determine the minimum value of NB.
C
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'SORGLQ', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
C
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
C
C        Use blocked code after the last block.
C        The first kk rows are handled by the block method.
C
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
C
C        Set A(kk+1:m,1:kk) to zero.
C
         DO 20 J = 1, KK
            DO 10 I = KK + 1, M
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
C
C     Use unblocked code for the last or only block.
C
      IF( KK.LT.M )
     $   CALL SORGL2( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
C
      IF( KK.GT.0 ) THEN
C
C        Use blocked code
C
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.M ) THEN
C
C              Form the triangular factor of the block reflector
C              H = H(i) H(i+1) . . . H(i+ib-1)
C
               CALL SLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
C
C              Apply H' to A(i+ib:m,i:n) from the right
C
               CALL SLARFB( 'Right', 'Transpose', 'Forward', 'Rowwise',
     $                      M-I-IB+1, N-I+1, IB, A( I, I ), LDA, WORK,
     $                      LDWORK, A( I+IB, I ), LDA, WORK( IB+1 ),
     $                      LDWORK )
            END IF
C
C           Apply H' to columns i:n of current block
C
            CALL SORGL2( IB, N-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
C
C           Set columns 1:i-1 of current block to zero
C
            DO 40 J = 1, I - 1
               DO 30 L = I, I + IB - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
C
      WORK( 1 ) = IWS
      RETURN
C
C     End of SORGLQ
C
      END
      SUBROUTINE SORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), TAU( * ), WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SORGQR generates an M-by-N real matrix Q with orthonormal columns,
C  which is defined as the first N columns of a product of K elementary
C  reflectors of order M
C
C        Q  =  H(1) H(2) . . . H(k)
C
C  as returned by SGEQRF.
C
C  Arguments
C  =========
C
C  M       (input) INTEGER
C          The number of rows of the matrix Q. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix Q. M >= N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines the
C          matrix Q. N >= K >= 0.
C
C  A       (input/output) REAL array, dimension (LDA,N)
C          On entry, the i-th column must contain the vector which
C          defines the elementary reflector H(i), for i = 1,2,...,k, as
C          returned by SGEQRF in the first k columns of its array
C          argument A.
C          On exit, the M-by-N matrix Q.
C
C  LDA     (input) INTEGER
C          The first dimension of the array A. LDA >= max(1,M).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGEQRF.
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK. LWORK >= max(1,N).
C          For optimum performance LWORK >= N*NB, where NB is the
C          optimal blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument has an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARFB, SLARFT, SORG2R, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORGQR', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.
C
      NB = ILAENV( 1, 'SORGQR', ' ', M, N, K, -1 )
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
C
C        Determine when to cross over from blocked to unblocked code.
C
         NX = MAX( 0, ILAENV( 3, 'SORGQR', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
C
C           Determine if workspace is large enough for blocked code.
C
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
C
C              Not enough workspace to use optimal NB:  reduce NB and
C              determine the minimum value of NB.
C
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'SORGQR', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
C
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
C
C        Use blocked code after the last block.
C        The first kk columns are handled by the block method.
C
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
C
C        Set A(1:kk,kk+1:n) to zero.
C
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
C
C     Use unblocked code for the last or only block.
C
      IF( KK.LT.N )
     $   CALL SORG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
C
      IF( KK.GT.0 ) THEN
C
C        Use blocked code
C
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.N ) THEN
C
C              Form the triangular factor of the block reflector
C              H = H(i) H(i+1) . . . H(i+ib-1)
C
               CALL SLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
C
C              Apply H to A(i:m,i+ib:n) from the left
C
               CALL SLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
C
C           Apply H to rows i:m of current block
C
            CALL SORG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
C
C           Set rows 1:i-1 of current block to zero
C
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
C
      WORK( 1 ) = IWS
      RETURN
C
C     End of SORGQR
C
      END
      SUBROUTINE SORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SORM2R overwrites the general real m by n matrix C with
C
C        Q * C  if SIDE = 'L' and TRANS = 'N', or
C
C        Q'* C  if SIDE = 'L' and TRANS = 'T', or
C
C        C * Q  if SIDE = 'R' and TRANS = 'N', or
C
C        C * Q' if SIDE = 'R' and TRANS = 'T',
C
C  where Q is a real orthogonal matrix defined as the product of k
C  elementary reflectors
C
C        Q = H(1) H(2) . . . H(k)
C
C  as returned by SGEQRF. Q is of order m if SIDE = 'L' and of order n
C  if SIDE = 'R'.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply Q or Q' from the Left
C          = 'R': apply Q or Q' from the Right
C
C  TRANS   (input) CHARACTER*1
C          = 'N': apply Q  (No transpose)
C          = 'T': apply Q' (Transpose)
C
C  M       (input) INTEGER
C          The number of rows of the matrix C. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C. N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines
C          the matrix Q.
C          If SIDE = 'L', M >= K >= 0;
C          if SIDE = 'R', N >= K >= 0.
C
C  A       (input) REAL array, dimension (LDA,K)
C          The i-th column must contain the vector which defines the
C          elementary reflector H(i), for i = 1,2,...,k, as returned by
C          SGEQRF in the first k columns of its array argument A.
C          A is modified by the routine but restored on exit.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.
C          If SIDE = 'L', LDA >= max(1,M);
C          if SIDE = 'R', LDA >= max(1,N).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGEQRF.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the m by n matrix C.
C          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace) REAL array, dimension
C                                   (N) if SIDE = 'L',
C                                   (M) if SIDE = 'R'
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      REAL               AII
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ is the order of Q
C
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORM2R', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
C
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
C
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
C
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
C
C           H(i) is applied to C(i:m,1:n)
C
            MI = M - I + 1
            IC = I
         ELSE
C
C           H(i) is applied to C(1:m,i:n)
C
            NI = N - I + 1
            JC = I
         END IF
C
C        Apply H(i)
C
         AII = A( I, I )
         A( I, I ) = ONE
         CALL SLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
C
C     End of SORM2R
C
      END
      SUBROUTINE SORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
     $                   LDC, WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, VECT
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  If VECT = 'Q', SORMBR overwrites the general real M-by-N matrix C
C  with
C                  SIDE = 'L'     SIDE = 'R'
C  TRANS = 'N':      Q * C          C * Q
C  TRANS = 'T':      Q**T * C       C * Q**T
C
C  If VECT = 'P', SORMBR overwrites the general real M-by-N matrix C
C  with
C                  SIDE = 'L'     SIDE = 'R'
C  TRANS = 'N':      P * C          C * P
C  TRANS = 'T':      P**T * C       C * P**T
C
C  Here Q and P**T are the orthogonal matrices determined by SGEBRD when
C  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
C  P**T are defined as products of elementary reflectors H(i) and G(i)
C  respectively.
C
C  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
C  order of the orthogonal matrix Q or P**T that is applied.
C
C  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
C  if nq >= k, Q = H(1) H(2) . . . H(k);
C  if nq < k, Q = H(1) H(2) . . . H(nq-1).
C
C  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
C  if k < nq, P = G(1) G(2) . . . G(k);
C  if k >= nq, P = G(1) G(2) . . . G(nq-1).
C
C  Arguments
C  =========
C
C  VECT    (input) CHARACTER*1
C          = 'Q': apply Q or Q**T;
C          = 'P': apply P or P**T.
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply Q, Q**T, P or P**T from the Left;
C          = 'R': apply Q, Q**T, P or P**T from the Right.
C
C  TRANS   (input) CHARACTER*1
C          = 'N':  No transpose, apply Q  or P;
C          = 'T':  Transpose, apply Q**T or P**T.
C
C  M       (input) INTEGER
C          The number of rows of the matrix C. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C. N >= 0.
C
C  K       (input) INTEGER
C          If VECT = 'Q', the number of columns in the original
C          matrix reduced by SGEBRD.
C          If VECT = 'P', the number of rows in the original
C          matrix reduced by SGEBRD.
C          K >= 0.
C
C  A       (input) REAL array, dimension
C                                (LDA,min(nq,K)) if VECT = 'Q'
C                                (LDA,nq)        if VECT = 'P'
C          The vectors which define the elementary reflectors H(i) and
C          G(i), whose products determine the matrices Q and P, as
C          returned by SGEBRD.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.
C          If VECT = 'Q', LDA >= max(1,nq);
C          if VECT = 'P', LDA >= max(1,min(nq,K)).
C
C  TAU     (input) REAL array, dimension (min(nq,K))
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i) or G(i) which determines Q or P, as returned
C          by SGEBRD in the array argument TAUQ or TAUP.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the M-by-N matrix C.
C          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
C          or P*C or P**T*C or C*P or C*P**T.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK.
C          If SIDE = 'L', LWORK >= max(1,N);
C          if SIDE = 'R', LWORK >= max(1,M).
C          For optimum performance LWORK >= N*NB if SIDE = 'L', and
C          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
C          blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Local Scalars ..
      LOGICAL            APPLYQ, LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I1, I2, IINFO, MI, NI, NQ, NW
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SORMLQ, SORMQR, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      APPLYQ = LSAME( VECT, 'Q' )
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ is the order of Q or P and NW is the minimum dimension of WORK
C
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.APPLYQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( K.LT.0 ) THEN
         INFO = -6
      ELSE IF( ( APPLYQ .AND. LDA.LT.MAX( 1, NQ ) ) .OR.
     $         ( .NOT.APPLYQ .AND. LDA.LT.MAX( 1, MIN( NQ, K ) ) ) )
     $          THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORMBR', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      WORK( 1 ) = 1
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
C
      IF( APPLYQ ) THEN
C
C        Apply Q
C
         IF( NQ.GE.K ) THEN
C
C           Q was determined by a call to SGEBRD with nq >= k
C
            CALL SORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
C
C           Q was determined by a call to SGEBRD with nq < k
C
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL SORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                   C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      ELSE
C
C        Apply P
C
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
         IF( NQ.GT.K ) THEN
C
C           P was determined by a call to SGEBRD with nq > k
C
            CALL SORMLQ( SIDE, TRANST, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
C
C           P was determined by a call to SGEBRD with nq <= k
C
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL SORMLQ( SIDE, TRANST, MI, NI, NQ-1, A( 1, 2 ), LDA,
     $                   TAU, C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      END IF
      RETURN
C
C     End of SORMBR
C
      END
      SUBROUTINE SORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     February 29, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
C     ..
C
C  Purpose
C  =======
C
C  SORML2 overwrites the general real m by n matrix C with
C
C        Q * C  if SIDE = 'L' and TRANS = 'N', or
C
C        Q'* C  if SIDE = 'L' and TRANS = 'T', or
C
C        C * Q  if SIDE = 'R' and TRANS = 'N', or
C
C        C * Q' if SIDE = 'R' and TRANS = 'T',
C
C  where Q is a real orthogonal matrix defined as the product of k
C  elementary reflectors
C
C        Q = H(k) . . . H(2) H(1)
C
C  as returned by SGELQF. Q is of order m if SIDE = 'L' and of order n
C  if SIDE = 'R'.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply Q or Q' from the Left
C          = 'R': apply Q or Q' from the Right
C
C  TRANS   (input) CHARACTER*1
C          = 'N': apply Q  (No transpose)
C          = 'T': apply Q' (Transpose)
C
C  M       (input) INTEGER
C          The number of rows of the matrix C. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C. N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines
C          the matrix Q.
C          If SIDE = 'L', M >= K >= 0;
C          if SIDE = 'R', N >= K >= 0.
C
C  A       (input) REAL array, dimension
C                               (LDA,M) if SIDE = 'L',
C                               (LDA,N) if SIDE = 'R'
C          The i-th row must contain the vector which defines the
C          elementary reflector H(i), for i = 1,2,...,k, as returned by
C          SGELQF in the first k rows of its array argument A.
C          A is modified by the routine but restored on exit.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A. LDA >= max(1,K).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGELQF.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the m by n matrix C.
C          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace) REAL array, dimension
C                                   (N) if SIDE = 'L',
C                                   (M) if SIDE = 'R'
C
C  INFO    (output) INTEGER
C          = 0: successful exit
C          < 0: if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      REAL               AII
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARF, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ is the order of Q
C
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORML2', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
C
      IF( ( LEFT .AND. NOTRAN ) .OR. ( .NOT.LEFT .AND. .NOT.NOTRAN ) )
     $     THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
C
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
C
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
C
C           H(i) is applied to C(i:m,1:n)
C
            MI = M - I + 1
            IC = I
         ELSE
C
C           H(i) is applied to C(1:m,i:n)
C
            NI = N - I + 1
            JC = I
         END IF
C
C        Apply H(i)
C
         AII = A( I, I )
         A( I, I ) = ONE
         CALL SLARF( SIDE, MI, NI, A( I, I ), LDA, TAU( I ),
     $               C( IC, JC ), LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
C
C     End of SORML2
C
      END
      SUBROUTINE SORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SORMLQ overwrites the general real M-by-N matrix C with
C
C                  SIDE = 'L'     SIDE = 'R'
C  TRANS = 'N':      Q * C          C * Q
C  TRANS = 'T':      Q**T * C       C * Q**T
C
C  where Q is a real orthogonal matrix defined as the product of k
C  elementary reflectors
C
C        Q = H(k) . . . H(2) H(1)
C
C  as returned by SGELQF. Q is of order M if SIDE = 'L' and of order N
C  if SIDE = 'R'.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply Q or Q**T from the Left;
C          = 'R': apply Q or Q**T from the Right.
C
C  TRANS   (input) CHARACTER*1
C          = 'N':  No transpose, apply Q;
C          = 'T':  Transpose, apply Q**T.
C
C  M       (input) INTEGER
C          The number of rows of the matrix C. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C. N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines
C          the matrix Q.
C          If SIDE = 'L', M >= K >= 0;
C          if SIDE = 'R', N >= K >= 0.
C
C  A       (input) REAL array, dimension
C                               (LDA,M) if SIDE = 'L',
C                               (LDA,N) if SIDE = 'R'
C          The i-th row must contain the vector which defines the
C          elementary reflector H(i), for i = 1,2,...,k, as returned by
C          SGELQF in the first k rows of its array argument A.
C          A is modified by the routine but restored on exit.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A. LDA >= max(1,K).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGELQF.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the M-by-N matrix C.
C          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK.
C          If SIDE = 'L', LWORK >= max(1,N);
C          if SIDE = 'R', LWORK >= max(1,M).
C          For optimum performance LWORK >= N*NB if SIDE = 'L', and
C          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
C          blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
C     ..
C     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
C     ..
C     .. Local Arrays ..
      REAL               T( LDT, NBMAX )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARFB, SLARFT, SORML2, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ is the order of Q and NW is the minimum dimension of WORK
C
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORMLQ', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.  NB may be at most NBMAX, where NBMAX
C     is used to define the local array T.
C
      NB = MIN( NBMAX, ILAENV( 1, 'SORMLQ', SIDE // TRANS, M, N, K,
     $     -1 ) )
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'SORMLQ', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
C
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
C
C        Use unblocked code
C
         CALL SORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
C
C        Use blocked code
C
         IF( ( LEFT .AND. NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
C
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
C
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
C
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
C
C           Form the triangular factor of the block reflector
C           H = H(i) H(i+1) . . . H(i+ib-1)
C
            CALL SLARFT( 'Forward', 'Rowwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
C
C              H or H' is applied to C(i:m,1:n)
C
               MI = M - I + 1
               IC = I
            ELSE
C
C              H or H' is applied to C(1:m,i:n)
C
               NI = N - I + 1
               JC = I
            END IF
C
C           Apply H or H'
C
            CALL SLARFB( SIDE, TRANST, 'Forward', 'Rowwise', MI, NI, IB,
     $                   A( I, I ), LDA, T, LDT, C( IC, JC ), LDC, WORK,
     $                   LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
C
C     End of SORMLQ
C
      END
      SUBROUTINE SORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
C
C  -- LAPACK routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
C     ..
C     .. Array Arguments ..
      REAL               A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
C     ..
C
C  Purpose
C  =======
C
C  SORMQR overwrites the general real M-by-N matrix C with
C
C                  SIDE = 'L'     SIDE = 'R'
C  TRANS = 'N':      Q * C          C * Q
C  TRANS = 'T':      Q**T * C       C * Q**T
C
C  where Q is a real orthogonal matrix defined as the product of k
C  elementary reflectors
C
C        Q = H(1) H(2) . . . H(k)
C
C  as returned by SGEQRF. Q is of order M if SIDE = 'L' and of order N
C  if SIDE = 'R'.
C
C  Arguments
C  =========
C
C  SIDE    (input) CHARACTER*1
C          = 'L': apply Q or Q**T from the Left;
C          = 'R': apply Q or Q**T from the Right.
C
C  TRANS   (input) CHARACTER*1
C          = 'N':  No transpose, apply Q;
C          = 'T':  Transpose, apply Q**T.
C
C  M       (input) INTEGER
C          The number of rows of the matrix C. M >= 0.
C
C  N       (input) INTEGER
C          The number of columns of the matrix C. N >= 0.
C
C  K       (input) INTEGER
C          The number of elementary reflectors whose product defines
C          the matrix Q.
C          If SIDE = 'L', M >= K >= 0;
C          if SIDE = 'R', N >= K >= 0.
C
C  A       (input) REAL array, dimension (LDA,K)
C          The i-th column must contain the vector which defines the
C          elementary reflector H(i), for i = 1,2,...,k, as returned by
C          SGEQRF in the first k columns of its array argument A.
C          A is modified by the routine but restored on exit.
C
C  LDA     (input) INTEGER
C          The leading dimension of the array A.
C          If SIDE = 'L', LDA >= max(1,M);
C          if SIDE = 'R', LDA >= max(1,N).
C
C  TAU     (input) REAL array, dimension (K)
C          TAU(i) must contain the scalar factor of the elementary
C          reflector H(i), as returned by SGEQRF.
C
C  C       (input/output) REAL array, dimension (LDC,N)
C          On entry, the M-by-N matrix C.
C          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
C
C  LDC     (input) INTEGER
C          The leading dimension of the array C. LDC >= max(1,M).
C
C  WORK    (workspace/output) REAL array, dimension (LWORK)
C          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
C
C  LWORK   (input) INTEGER
C          The dimension of the array WORK.
C          If SIDE = 'L', LWORK >= max(1,N);
C          if SIDE = 'R', LWORK >= max(1,M).
C          For optimum performance LWORK >= N*NB if SIDE = 'L', and
C          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
C          blocksize.
C
C  INFO    (output) INTEGER
C          = 0:  successful exit
C          < 0:  if INFO = -i, the i-th argument had an illegal value
C
C  =====================================================================
C
C     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
C     ..
C     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
C     ..
C     .. Local Arrays ..
      REAL               T( LDT, NBMAX )
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLARFB, SLARFT, SORM2R, XERBLA
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
C     ..
C     .. Executable Statements ..
C
C     Test the input arguments
C
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
C
C     NQ is the order of Q and NW is the minimum dimension of WORK
C
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SORMQR', -INFO )
         RETURN
      END IF
C
C     Quick return if possible
C
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
C
C     Determine the block size.  NB may be at most NBMAX, where NBMAX
C     is used to define the local array T.
C
      NB = MIN( NBMAX, ILAENV( 1, 'SORMQR', SIDE // TRANS, M, N, K,
     $     -1 ) )
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'SORMQR', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
C
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
C
C        Use unblocked code
C
         CALL SORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
C
C        Use blocked code
C
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
C
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
C
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
C
C           Form the triangular factor of the block reflector
C           H = H(i) H(i+1) . . . H(i+ib-1)
C
            CALL SLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
C
C              H or H' is applied to C(i:m,1:n)
C
               MI = M - I + 1
               IC = I
            ELSE
C
C              H or H' is applied to C(1:m,i:n)
C
               NI = N - I + 1
               JC = I
            END IF
C
C           Apply H or H'
C
            CALL SLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
C
C     End of SORMQR
C
      END
      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
     $                 N4 )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
C     ..
C
C  Purpose
C  =======
C
C  ILAENV is called from the LAPACK routines to choose problem-dependent
C  parameters for the local environment.  See ISPEC for a description of
C  the parameters.
C
C  This version provides a set of parameters which should give good,
C  but not optimal, performance on many of the currently available
C  computers.  Users are encouraged to modify this subroutine to set
C  the tuning parameters for their particular machine using the option
C  and problem size information in the arguments.
C
C  This routine will not function correctly if it is converted to all
C  lower case.  Converting it to all upper case is allowed.
C
C  Arguments
C  =========
C
C  ISPEC   (input) INTEGER
C          Specifies the parameter to be returned as the value of
C          ILAENV.
C          = 1: the optimal blocksize; if this value is 1, an unblocked
C               algorithm will give the best performance.
C          = 2: the minimum block size for which the block routine
C               should be used; if the usable block size is less than
C               this value, an unblocked routine should be used.
C          = 3: the crossover point (in a block routine, for N less
C               than this value, an unblocked routine should be used)
C          = 4: the number of shifts, used in the nonsymmetric
C               eigenvalue routines
C          = 5: the minimum column dimension for blocking to be used;
C               rectangular blocks must have dimension at least k by m,
C               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
C          = 6: the crossover point for the SVD (when reducing an m by n
C               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
C               this value, a QR factorization is used first to reduce
C               the matrix to a triangular form.)
C          = 7: the number of processors
C          = 8: the crossover point for the multishift QR and QZ methods
C               for nonsymmetric eigenvalue problems.
C
C  NAME    (input) CHARACTER*(*)
C          The name of the calling subroutine, in either upper case or
C          lower case.
C
C  OPTS    (input) CHARACTER*(*)
C          The character options to the subroutine NAME, concatenated
C          into a single character string.  For example, UPLO = 'U',
C          TRANS = 'T', and DIAG = 'N' for a triangular routine would
C          be specified as OPTS = 'UTN'.
C
C  N1      (input) INTEGER
C  N2      (input) INTEGER
C  N3      (input) INTEGER
C  N4      (input) INTEGER
C          Problem dimensions for the subroutine NAME; these may not all
C          be required.
C
C (ILAENV) (output) INTEGER
C          >= 0: the value of the parameter specified by ISPEC
C          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
C
C  Further Details
C  ===============
C
C  The following conventions have been used when calling ILAENV from the
C  LAPACK routines:
C  1)  OPTS is a concatenation of all of the character options to
C      subroutine NAME, in the same order that they appear in the
C      argument list for NAME, even if they are not used in determining
C      the value of the parameter specified by ISPEC.
C  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
C      that they appear in the argument list for NAME.  N1 is used
C      first, N2 second, and so on, and unused problem dimensions are
C      passed a value of -1.
C  3)  The parameter value returned by ILAENV is checked for validity in
C      the calling subroutine.  For example, ILAENV is used to retrieve
C      the optimal blocksize for STRTRI as follows:
C
C      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
C      IF( NB.LE.1 ) NB = MAX( 1, N )
C
C  =====================================================================
C
C     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ, NB, NBMIN, NX
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
C     ..
C     .. Executable Statements ..
C
      GO TO ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
C
C     Invalid value for ISPEC
C
      ILAENV = -1
      RETURN
C
  100 CONTINUE
C
C     Convert NAME to upper case if the first character is lower case.
C
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
C
C        ASCII character set
C
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
C
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
C
C        EBCDIC character set
C
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1:1 ) = CHAR( IC+64 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )
     $            SUBNAM( I:I ) = CHAR( IC+64 )
   20       CONTINUE
         END IF
C
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
C
C        Prime machines:  ASCII+128
C
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
C
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
C
      GO TO ( 110, 200, 300 ) ISPEC
C
  110 CONTINUE
C
C     ISPEC = 1:  block size
C
C     In these examples, separate code is provided for setting NB for
C     real and complex.  We assume that NB will take the same value in
C     single or double precision.
C
      NB = 1
C
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 1
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
C
  200 CONTINUE
C
C     ISPEC = 2:  minimum block size
C
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 8
            ELSE
               NBMIN = 8
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1:1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
C
  300 CONTINUE
C
C     ISPEC = 3:  crossover point
C
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $       C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 1
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1:1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR.
     $          C4.EQ.'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR.
     $          C4.EQ.'BR' ) THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
C
  400 CONTINUE
C
C     ISPEC = 4:  number of shifts (used by xHSEQR)
C
      ILAENV = 6
      RETURN
C
  500 CONTINUE
C
C     ISPEC = 5:  minimum column dimension (not used)
C
      ILAENV = 2
      RETURN
C
  600 CONTINUE
C
C     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
C
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
C
  700 CONTINUE
C
C     ISPEC = 7:  number of processors (not used)
C
      ILAENV = 1
      RETURN
C
  800 CONTINUE
C
C     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
C
      ILAENV = 50
      RETURN
C
C     End of ILAENV
C
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER          CA, CB
C     ..
C
C  Purpose
C  =======
C
C  LSAME returns .TRUE. if CA is the same letter as CB regardless of
C  case.
C
C  Arguments
C  =========
C
C  CA      (input) CHARACTER*1
C  CB      (input) CHARACTER*1
C          CA and CB specify the single characters to be compared.
C
C =====================================================================
C
C     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
C     ..
C     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
C     ..
C     .. Executable Statements ..
C
C     Test if the characters are equal
C
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
C
C     Now test for equivalence if both characters are alphabetic.
C
      ZCODE = ICHAR( 'Z' )
C
C     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
C     machines, on which ICHAR returns a value with bit 8 set.
C     ICHAR('A') on Prime machines returns 193 which is the same as
C     ICHAR('A') on an EBCDIC machine.
C
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
C
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
C
C        ASCII is assumed - ZCODE is the ASCII code of either lower or
C        upper case 'Z'.
C
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
C
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
C
C        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
C        upper case 'Z'.
C
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
C
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
C
C        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
C        plus 128 of either lower or upper case 'Z'.
C
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
C
C     RETURN
C
C     End of LSAME
C
      END
      REAL             FUNCTION SLAMCH( CMACH )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      CHARACTER          CMACH
C     ..
C
C  Purpose
C  =======
C
C  SLAMCH determines single precision machine parameters.
C
C  Arguments
C  =========
C
C  CMACH   (input) CHARACTER*1
C          Specifies the value to be returned by SLAMCH:
C          = 'E' or 'e',   SLAMCH := eps
C          = 'S' or 's ,   SLAMCH := sfmin
C          = 'B' or 'b',   SLAMCH := base
C          = 'P' or 'p',   SLAMCH := eps*base
C          = 'N' or 'n',   SLAMCH := t
C          = 'R' or 'r',   SLAMCH := rnd
C          = 'M' or 'm',   SLAMCH := emin
C          = 'U' or 'u',   SLAMCH := rmin
C          = 'L' or 'l',   SLAMCH := emax
C          = 'O' or 'o',   SLAMCH := rmax
C
C          where
C
C          eps   = relative machine precision
C          sfmin = safe minimum, such that 1/sfmin does not overflow
C          base  = base of the machine
C          prec  = eps*base
C          t     = number of (base) digits in the mantissa
C          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
C          emin  = minimum exponent before (gradual) underflow
C          rmin  = underflow threshold - base**(emin-1)
C          emax  = largest exponent before overflow
C          rmax  = overflow threshold  - (base**emax)*(1-eps)
C
C =====================================================================
C
C     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Local Scalars ..
      LOGICAL            FIRST, LRND
      INTEGER            BETA, IMAX, IMIN, IT
      REAL               BASE, EMAX, EMIN, EPS, PREC, RMACH, RMAX, RMIN,
     $                   RND, SFMIN, SMALL, T
C     ..
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLAMC2
C     ..
C     .. Save statement ..
      SAVE               FIRST, EPS, SFMIN, BASE, T, RND, EMIN, RMIN,
     $                   EMAX, RMAX, PREC
C     ..
C     .. Data statements ..
      DATA               FIRST / .TRUE. /
C     ..
C     .. Executable Statements ..
C
      IF( FIRST ) THEN
         FIRST = .FALSE.
         CALL SLAMC2( BETA, IT, LRND, EPS, IMIN, RMIN, IMAX, RMAX )
         BASE = BETA
         T = IT
         IF( LRND ) THEN
            RND = ONE
            EPS = ( BASE**( 1-IT ) ) / 2
         ELSE
            RND = ZERO
            EPS = BASE**( 1-IT )
         END IF
         PREC = EPS*BASE
         EMIN = IMIN
         EMAX = IMAX
         SFMIN = RMIN
         SMALL = ONE / RMAX
         IF( SMALL.GE.SFMIN ) THEN
C
C           Use SMALL plus a bit, to avoid the possibility of rounding
C           causing overflow when computing  1/sfmin.
C
            SFMIN = SMALL*( ONE+EPS )
         END IF
      END IF
C
      IF( LSAME( CMACH, 'E' ) ) THEN
         RMACH = EPS
      ELSE IF( LSAME( CMACH, 'S' ) ) THEN
         RMACH = SFMIN
      ELSE IF( LSAME( CMACH, 'B' ) ) THEN
         RMACH = BASE
      ELSE IF( LSAME( CMACH, 'P' ) ) THEN
         RMACH = PREC
      ELSE IF( LSAME( CMACH, 'N' ) ) THEN
         RMACH = T
      ELSE IF( LSAME( CMACH, 'R' ) ) THEN
         RMACH = RND
      ELSE IF( LSAME( CMACH, 'M' ) ) THEN
         RMACH = EMIN
      ELSE IF( LSAME( CMACH, 'U' ) ) THEN
         RMACH = RMIN
      ELSE IF( LSAME( CMACH, 'L' ) ) THEN
         RMACH = EMAX
      ELSE IF( LSAME( CMACH, 'O' ) ) THEN
         RMACH = RMAX
      END IF
C
      SLAMCH = RMACH
      RETURN
C
C     End of SLAMCH
C
      END
C
C***********************************************************************
C
      SUBROUTINE SLAMC1( BETA, T, RND, IEEE1 )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      LOGICAL            IEEE1, RND
      INTEGER            BETA, T
C     ..
C
C  Purpose
C  =======
C
C  SLAMC1 determines the machine parameters given by BETA, T, RND, and
C  IEEE1.
C
C  Arguments
C  =========
C
C  BETA    (output) INTEGER
C          The base of the machine.
C
C  T       (output) INTEGER
C          The number of ( BETA ) digits in the mantissa.
C
C  RND     (output) LOGICAL
C          Specifies whether proper rounding  ( RND = .TRUE. )  or
C          chopping  ( RND = .FALSE. )  occurs in addition. This may not
C          be a reliable guide to the way in which the machine performs
C          its arithmetic.
C
C  IEEE1   (output) LOGICAL
C          Specifies whether rounding appears to be done in the IEEE
C          'round to nearest' style.
C
C  Further Details
C  ===============
C
C  The routine is based on the routine  ENVRON  by Malcolm and
C  incorporates suggestions by Gentleman and Marovich. See
C
C     Malcolm M. A. (1972) Algorithms to reveal properties of
C        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
C
C     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
C        that reveal properties of floating point arithmetic units.
C        Comms. of the ACM, 17, 276-277.
C
C =====================================================================
C
C     .. Local Scalars ..
      LOGICAL            FIRST, LIEEE1, LRND
      INTEGER            LBETA, LT
      REAL               A, B, C, F, ONE, QTR, SAVEC, T1, T2
C     ..
C     .. External Functions ..
      REAL               SLAMC3
      EXTERNAL           SLAMC3
C     ..
C     .. Save statement ..
      SAVE               FIRST, LIEEE1, LBETA, LRND, LT
C     ..
C     .. Data statements ..
      DATA               FIRST / .TRUE. /
C     ..
C     .. Executable Statements ..
C
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ONE = 1
C
C        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
C        IEEE1, T and RND.
C
C        Throughout this routine  we use the function  SLAMC3  to ensure
C        that relevant values are  stored and not held in registers,  or
C        are not affected by optimizers.
C
C        Compute  a = 2.0**m  with the  smallest positive integer m such
C        that
C
C           fl( a + 1.0 ) = a.
C
         A = 1
         C = 1
C
C+       WHILE( C.EQ.ONE )LOOP
   10    CONTINUE
         IF( C.EQ.ONE ) THEN
            A = 2*A
            C = SLAMC3( A, ONE )
            C = SLAMC3( C, -A )
            GO TO 10
         END IF
C+       END WHILE
C
C        Now compute  b = 2.0**m  with the smallest positive integer m
C        such that
C
C           fl( a + b ) .gt. a.
C
         B = 1
         C = SLAMC3( A, B )
C
C+       WHILE( C.EQ.A )LOOP
   20    CONTINUE
         IF( C.EQ.A ) THEN
            B = 2*B
            C = SLAMC3( A, B )
            GO TO 20
         END IF
C+       END WHILE
C
C        Now compute the base.  a and c  are neighbouring floating point
C        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
C        their difference is beta. Adding 0.25 to c is to ensure that it
C        is truncated to beta and not ( beta - 1 ).
C
         QTR = ONE / 4
         SAVEC = C
         C = SLAMC3( C, -A )
         LBETA = C + QTR
C
C        Now determine whether rounding or chopping occurs,  by adding a
C        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
C
         B = LBETA
         F = SLAMC3( B / 2, -B / 100 )
         C = SLAMC3( F, A )
         IF( C.EQ.A ) THEN
            LRND = .TRUE.
         ELSE
            LRND = .FALSE.
         END IF
         F = SLAMC3( B / 2, B / 100 )
         C = SLAMC3( F, A )
         IF( ( LRND ) .AND. ( C.EQ.A ) )
     $      LRND = .FALSE.
C
C        Try and decide whether rounding is done in the  IEEE  'round to
C        nearest' style. B/2 is half a unit in the last place of the two
C        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
C        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
C        A, but adding B/2 to SAVEC should change SAVEC.
C
         T1 = SLAMC3( B / 2, A )
         T2 = SLAMC3( B / 2, SAVEC )
         LIEEE1 = ( T1.EQ.A ) .AND. ( T2.GT.SAVEC ) .AND. LRND
C
C        Now find  the  mantissa, t.  It should  be the  integer part of
C        log to the base beta of a,  however it is safer to determine  t
C        by powering.  So we find t as the smallest positive integer for
C        which
C
C           fl( beta**t + 1.0 ) = 1.0.
C
         LT = 0
         A = 1
         C = 1
C
C+       WHILE( C.EQ.ONE )LOOP
   30    CONTINUE
         IF( C.EQ.ONE ) THEN
            LT = LT + 1
            A = A*LBETA
            C = SLAMC3( A, ONE )
            C = SLAMC3( C, -A )
            GO TO 30
         END IF
C+       END WHILE
C
      END IF
C
      BETA = LBETA
      T = LT
      RND = LRND
      IEEE1 = LIEEE1
      RETURN
C
C     End of SLAMC1
C
      END
C
C***********************************************************************
C
      SUBROUTINE SLAMC2( BETA, T, RND, EPS, EMIN, RMIN, EMAX, RMAX )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      LOGICAL            RND
      INTEGER            BETA, EMAX, EMIN, T
      REAL               EPS, RMAX, RMIN
C     ..
C
C  Purpose
C  =======
C
C  SLAMC2 determines the machine parameters specified in its argument
C  list.
C
C  Arguments
C  =========
C
C  BETA    (output) INTEGER
C          The base of the machine.
C
C  T       (output) INTEGER
C          The number of ( BETA ) digits in the mantissa.
C
C  RND     (output) LOGICAL
C          Specifies whether proper rounding  ( RND = .TRUE. )  or
C          chopping  ( RND = .FALSE. )  occurs in addition. This may not
C          be a reliable guide to the way in which the machine performs
C          its arithmetic.
C
C  EPS     (output) REAL
C          The smallest positive number such that
C
C             fl( 1.0 - EPS ) .LT. 1.0,
C
C          where fl denotes the computed value.
C
C  EMIN    (output) INTEGER
C          The minimum exponent before (gradual) underflow occurs.
C
C  RMIN    (output) REAL
C          The smallest normalized number for the machine, given by
C          BASE**( EMIN - 1 ), where  BASE  is the floating point value
C          of BETA.
C
C  EMAX    (output) INTEGER
C          The maximum exponent before overflow occurs.
C
C  RMAX    (output) REAL
C          The largest positive number for the machine, given by
C          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
C          value of BETA.
C
C  Further Details
C  ===============
C
C  The computation of  EPS  is based on a routine PARANOIA by
C  W. Kahan of the University of California at Berkeley.
C
C =====================================================================
C
C     .. Local Scalars ..
      LOGICAL            FIRST, IEEE, IWARN, LIEEE1, LRND
      INTEGER            GNMIN, GPMIN, I, LBETA, LEMAX, LEMIN, LT,
     $                   NGNMIN, NGPMIN
      REAL               A, B, C, HALF, LEPS, LRMAX, LRMIN, ONE, RBASE,
     $                   SIXTH, SMALL, THIRD, TWO, ZERO
C     ..
C     .. External Functions ..
      REAL               SLAMC3
      EXTERNAL           SLAMC3
C     ..
C     .. External Subroutines ..
      EXTERNAL           SLAMC1, SLAMC4, SLAMC5
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
C     ..
C     .. Save statement ..
      SAVE               FIRST, IWARN, LBETA, LEMAX, LEMIN, LEPS, LRMAX,
     $                   LRMIN, LT
C     ..
C     .. Data statements ..
      DATA               FIRST / .TRUE. / , IWARN / .FALSE. /
C     ..
C     .. Executable Statements ..
C
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ZERO = 0
         ONE = 1
         TWO = 2
C
C        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
C        BETA, T, RND, EPS, EMIN and RMIN.
C
C        Throughout this routine  we use the function  SLAMC3  to ensure
C        that relevant values are stored  and not held in registers,  or
C        are not affected by optimizers.
C
C        SLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
C
         CALL SLAMC1( LBETA, LT, LRND, LIEEE1 )
C
C        Start to find EPS.
C
         B = LBETA
         A = B**( -LT )
         LEPS = A
C
C        Try some tricks to see whether or not this is the correct  EPS.
C
         B = TWO / 3
         HALF = ONE / 2
         SIXTH = SLAMC3( B, -HALF )
         THIRD = SLAMC3( SIXTH, SIXTH )
         B = SLAMC3( THIRD, -HALF )
         B = SLAMC3( B, SIXTH )
         B = ABS( B )
         IF( B.LT.LEPS )
     $      B = LEPS
C
         LEPS = 1
C
C+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
   10    CONTINUE
         IF( ( LEPS.GT.B ) .AND. ( B.GT.ZERO ) ) THEN
            LEPS = B
            C = SLAMC3( HALF*LEPS, ( TWO**5 )*( LEPS**2 ) )
            C = SLAMC3( HALF, -C )
            B = SLAMC3( HALF, C )
            C = SLAMC3( HALF, -B )
            B = SLAMC3( HALF, C )
            GO TO 10
         END IF
C+       END WHILE
C
         IF( A.LT.LEPS )
     $      LEPS = A
C
C        Computation of EPS complete.
C
C        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
C        Keep dividing  A by BETA until (gradual) underflow occurs. This
C        is detected when we cannot recover the previous A.
C
         RBASE = ONE / LBETA
         SMALL = ONE
         DO 20 I = 1, 3
            SMALL = SLAMC3( SMALL*RBASE, ZERO )
   20    CONTINUE
         A = SLAMC3( ONE, SMALL )
         CALL SLAMC4( NGPMIN, ONE, LBETA )
         CALL SLAMC4( NGNMIN, -ONE, LBETA )
         CALL SLAMC4( GPMIN, A, LBETA )
         CALL SLAMC4( GNMIN, -A, LBETA )
         IEEE = .FALSE.
C
         IF( ( NGPMIN.EQ.NGNMIN ) .AND. ( GPMIN.EQ.GNMIN ) ) THEN
            IF( NGPMIN.EQ.GPMIN ) THEN
               LEMIN = NGPMIN
C            ( Non twos-complement machines, no gradual underflow;
C              e.g.,  VAX )
            ELSE IF( ( GPMIN-NGPMIN ).EQ.3 ) THEN
               LEMIN = NGPMIN - 1 + LT
               IEEE = .TRUE.
C            ( Non twos-complement machines, with gradual underflow;
C              e.g., IEEE standard followers )
            ELSE
               LEMIN = MIN( NGPMIN, GPMIN )
C            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
C
         ELSE IF( ( NGPMIN.EQ.GPMIN ) .AND. ( NGNMIN.EQ.GNMIN ) ) THEN
            IF( ABS( NGPMIN-NGNMIN ).EQ.1 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN )
C            ( Twos-complement machines, no gradual underflow;
C              e.g., CYBER 205 )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
C            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
C
         ELSE IF( ( ABS( NGPMIN-NGNMIN ).EQ.1 ) .AND.
     $            ( GPMIN.EQ.GNMIN ) ) THEN
            IF( ( GPMIN-MIN( NGPMIN, NGNMIN ) ).EQ.3 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN ) - 1 + LT
C            ( Twos-complement machines with gradual underflow;
C              no known machine )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
C            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
C
         ELSE
            LEMIN = MIN( NGPMIN, NGNMIN, GPMIN, GNMIN )
C         ( A guess; no known machine )
            IWARN = .TRUE.
         END IF
C**
C Comment out this if block if EMIN is ok
         IF( IWARN ) THEN
            FIRST = .TRUE.
            WRITE( 6, FMT = 9999 )LEMIN
         END IF
C**
C
C        Assume IEEE arithmetic if we found denormalised  numbers above,
C        or if arithmetic seems to round in the  IEEE style,  determined
C        in routine SLAMC1. A true IEEE machine should have both  things
C        true; however, faulty machines may have one or the other.
C
         IEEE = IEEE .OR. LIEEE1
C
C        Compute  RMIN by successive division by  BETA. We could compute
C        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
C        this computation.
C
         LRMIN = 1
         DO 30 I = 1, 1 - LEMIN
            LRMIN = SLAMC3( LRMIN*RBASE, ZERO )
   30    CONTINUE
C
C        Finally, call SLAMC5 to compute EMAX and RMAX.
C
         CALL SLAMC5( LBETA, LT, LEMIN, IEEE, LEMAX, LRMAX )
      END IF
C
      BETA = LBETA
      T = LT
      RND = LRND
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      EMAX = LEMAX
      RMAX = LRMAX
C
      RETURN
C
 9999 FORMAT( / / ' WARNING. The value EMIN may be incorrect:-',
     $      '  EMIN = ', I8, /
     $      ' If, after inspection, the value EMIN looks',
     $      ' acceptable please comment out ',
     $      / ' the IF block as marked within the code of routine',
     $      ' SLAMC2,', / ' otherwise supply EMIN explicitly.', / )
C
C     End of SLAMC2
C
      END
C
C***********************************************************************
C
      REAL             FUNCTION SLAMC3( A, B )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      REAL               A, B
C     ..
C
C  Purpose
C  =======
C
C  SLAMC3  is intended to force  A  and  B  to be stored prior to doing
C  the addition of  A  and  B ,  for use in situations where optimizers
C  might hold one of these in a register.
C
C  Arguments
C  =========
C
C  A, B    (input) REAL
C          The values A and B.
C
C =====================================================================
C
C     .. Executable Statements ..
C
      SLAMC3 = A + B
C
      RETURN
C
C     End of SLAMC3
C
      END
C
C***********************************************************************
C
      SUBROUTINE SLAMC4( EMIN, START, BASE )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      INTEGER            BASE, EMIN
      REAL               START
C     ..
C
C  Purpose
C  =======
C
C  SLAMC4 is a service routine for SLAMC2.
C
C  Arguments
C  =========
C
C  EMIN    (output) EMIN
C          The minimum exponent before (gradual) underflow, computed by
C          setting A = START and dividing by BASE until the previous A
C          can not be recovered.
C
C  START   (input) REAL
C          The starting point for determining EMIN.
C
C  BASE    (input) INTEGER
C          The base of the machine.
C
C =====================================================================
C
C     .. Local Scalars ..
      INTEGER            I
      REAL               A, B1, B2, C1, C2, D1, D2, ONE, RBASE, ZERO
C     ..
C     .. External Functions ..
      REAL               SLAMC3
      EXTERNAL           SLAMC3
C     ..
C     .. Executable Statements ..
C
      A = START
      ONE = 1
      RBASE = ONE / BASE
      ZERO = 0
      EMIN = 1
      B1 = SLAMC3( A*RBASE, ZERO )
      C1 = A
      C2 = A
      D1 = A
      D2 = A
C+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
C    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
   10 CONTINUE
      IF( ( C1.EQ.A ) .AND. ( C2.EQ.A ) .AND. ( D1.EQ.A ) .AND.
     $    ( D2.EQ.A ) ) THEN
         EMIN = EMIN - 1
         A = B1
         B1 = SLAMC3( A / BASE, ZERO )
         C1 = SLAMC3( B1*BASE, ZERO )
         D1 = ZERO
         DO 20 I = 1, BASE
            D1 = D1 + B1
   20    CONTINUE
         B2 = SLAMC3( A*RBASE, ZERO )
         C2 = SLAMC3( B2 / RBASE, ZERO )
         D2 = ZERO
         DO 30 I = 1, BASE
            D2 = D2 + B2
   30    CONTINUE
         GO TO 10
      END IF
C+    END WHILE
C
      RETURN
C
C     End of SLAMC4
C
      END
C
C***********************************************************************
C
      SUBROUTINE SLAMC5( BETA, P, EMIN, IEEE, EMAX, RMAX )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     October 31, 1992
C
C     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            BETA, EMAX, EMIN, P
      REAL               RMAX
C     ..
C
C  Purpose
C  =======
C
C  SLAMC5 attempts to compute RMAX, the largest machine floating-point
C  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
C  approximately to a power of 2.  It will fail on machines where this
C  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
C  EMAX = 28718).  It will also fail if the value supplied for EMIN is
C  too large (i.e. too close to zero), probably with overflow.
C
C  Arguments
C  =========
C
C  BETA    (input) INTEGER
C          The base of floating-point arithmetic.
C
C  P       (input) INTEGER
C          The number of base BETA digits in the mantissa of a
C          floating-point value.
C
C  EMIN    (input) INTEGER
C          The minimum exponent before (gradual) underflow.
C
C  IEEE    (input) LOGICAL
C          A logical flag specifying whether or not the arithmetic
C          system is thought to comply with the IEEE standard.
C
C  EMAX    (output) INTEGER
C          The largest exponent before overflow
C
C  RMAX    (output) REAL
C          The largest machine floating-point number.
C
C =====================================================================
C
C     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E0, ONE = 1.0E0 )
C     ..
C     .. Local Scalars ..
      INTEGER            EXBITS, EXPSUM, I, LEXP, NBITS, TRY, UEXP
      REAL               OLDY, RECBAS, Y, Z
C     ..
C     .. External Functions ..
      REAL               SLAMC3
      EXTERNAL           SLAMC3
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC          MOD
C     ..
C     .. Executable Statements ..
C
C     First compute LEXP and UEXP, two powers of 2 that bound
C     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
C     approximately to the bound that is closest to abs(EMIN).
C     (EMAX is the exponent of the required number RMAX).
C
      LEXP = 1
      EXBITS = 1
   10 CONTINUE
      TRY = LEXP*2
      IF( TRY.LE.( -EMIN ) ) THEN
         LEXP = TRY
         EXBITS = EXBITS + 1
         GO TO 10
      END IF
      IF( LEXP.EQ.-EMIN ) THEN
         UEXP = LEXP
      ELSE
         UEXP = TRY
         EXBITS = EXBITS + 1
      END IF
C
C     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
C     than or equal to EMIN. EXBITS is the number of bits needed to
C     store the exponent.
C
      IF( ( UEXP+EMIN ).GT.( -LEXP-EMIN ) ) THEN
         EXPSUM = 2*LEXP
      ELSE
         EXPSUM = 2*UEXP
      END IF
C
C     EXPSUM is the exponent range, approximately equal to
C     EMAX - EMIN + 1 .
C
      EMAX = EXPSUM + EMIN - 1
      NBITS = 1 + EXBITS + P
C
C     NBITS is the total number of bits needed to store a
C     floating-point number.
C
      IF( ( MOD( NBITS, 2 ).EQ.1 ) .AND. ( BETA.EQ.2 ) ) THEN
C
C        Either there are an odd number of bits used to store a
C        floating-point number, which is unlikely, or some bits are
C        not used in the representation of numbers, which is possible,
C        (e.g. Cray machines) or the mantissa has an implicit bit,
C        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
C        most likely. We have to assume the last alternative.
C        If this is true, then we need to reduce EMAX by one because
C        there must be some way of representing zero in an implicit-bit
C        system. On machines like Cray, we are reducing EMAX by one
C        unnecessarily.
C
         EMAX = EMAX - 1
      END IF
C
      IF( IEEE ) THEN
C
C        Assume we are on an IEEE machine which reserves one exponent
C        for infinity and NaN.
C
         EMAX = EMAX - 1
      END IF
C
C     Now create RMAX, the largest machine number, which should
C     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
C
C     First compute 1.0 - BETA**(-P), being careful that the
C     result is less than 1.0 .
C
      RECBAS = ONE / BETA
      Z = BETA - ONE
      Y = ZERO
      DO 20 I = 1, P
         Z = Z*RECBAS
         IF( Y.LT.ONE )
     $      OLDY = Y
         Y = SLAMC3( Y, Z )
   20 CONTINUE
      IF( Y.GE.ONE )
     $   Y = OLDY
C
C     Now multiply by BETA**EMAX to get RMAX.
C
      DO 30 I = 1, EMAX
         Y = SLAMC3( Y*BETA, ZERO )
   30 CONTINUE
C
      RMAX = Y
      RETURN
C
C     End of SLAMC5
C
      END
      SUBROUTINE XERBLA( SRNAME, INFO )
C
C  -- LAPACK auxiliary routine (version 2.0) --
C     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
C     Courant Institute, Argonne National Lab, and Rice University
C     September 30, 1994
C
C     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
C     ..
C
C  Purpose
C  =======
C
C  XERBLA  is an error handler for the LAPACK routines.
C  It is called by an LAPACK routine if an input parameter has an
C  invalid value.  A message is printed and execution stops.
C
C  Installers may consider modifying the STOP statement in order to
C  call system-specific exception-handling facilities.
C
C  Arguments
C  =========
C
C  SRNAME  (input) CHARACTER*6
C          The name of the routine which called XERBLA.
C
C  INFO    (input) INTEGER
C          The position of the invalid parameter in the parameter list
C          of the calling routine.
C
C =====================================================================
C
C     .. Executable Statements ..
C
      WRITE( *, FMT = 9999 )SRNAME, INFO
C
      STOP
C
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     *      'an illegal value' )
C
C     End of XERBLA
C
      END

C
C %W%    %G%
C
C Selected subset of BLAS needed to run certain LAPACK routine.  Machine
C specific overrides can be added in this file to enhance performance.
C
C Pulled over from netlib
C                                       D.S.Briggs Feb 7 1995
C
      SUBROUTINE SGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
C     .. Scalar Arguments ..
      REAL               ALPHA
      INTEGER            INCX, INCY, LDA, M, N
C     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
C     ..
C
C  Purpose
C  =======
C
C  SGER   performs the rank 1 operation
C
C     A := alpha*x*y' + A,
C
C  where alpha is a scalar, x is an m element vector, y is an n element
C  vector and A is an m by n matrix.
C
C  Parameters
C  ==========
C
C  M      - INTEGER.
C           On entry, M specifies the number of rows of the matrix A.
C           M must be at least zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the number of columns of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  ALPHA  - REAL            .
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  X      - REAL             array of dimension at least
C           ( 1 + ( m - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the m
C           element vector x.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C  Y      - REAL             array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCY ) ).
C           Before entry, the incremented array Y must contain the n
C           element vector y.
C           Unchanged on exit.
C
C  INCY   - INTEGER.
C           On entry, INCY specifies the increment for the elements of
C           Y. INCY must not be zero.
C           Unchanged on exit.
C
C  A      - REAL             array of DIMENSION ( LDA, n ).
C           Before entry, the leading m by n part of the array A must
C           contain the matrix of coefficients. On exit, A is
C           overwritten by the updated matrix.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
C     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JY, KX
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGER  ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     *   RETURN
C
C     Start the operations. In this version the elements of A are
C     accessed sequentially with one pass through A.
C
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
C
      RETURN
C
C     End of SGER  .
C
      END
C
      SUBROUTINE SROT (N,SX,INCX,SY,INCY,C,S)
C
C     APPLIES A PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*),STEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        STEMP = C*SX(IX) + S*SY(IY)
        SY(IY) = C*SY(IY) - S*SX(IX)
        SX(IX) = STEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
        STEMP = C*SX(I) + S*SY(I)
        SY(I) = C*SY(I) - S*SX(I)
        SX(I) = STEMP
   30 CONTINUE
      RETURN
      END
C
      SUBROUTINE STRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
C     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
C     .. Array Arguments ..
      REAL               A( LDA, * ), X( * )
C     ..
C
C  Purpose
C  =======
C
C  STRMV  performs one of the matrix-vector operations
C
C     x := A*x,   or   x := A'*x,
C
C  where x is an n element vector and  A is an n by n unit, or non-unit,
C  upper or lower triangular matrix.
C
C  Parameters
C  ==========
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the matrix is an upper or
C           lower triangular matrix as follows:
C
C              UPLO = 'U' or 'u'   A is an upper triangular matrix.
C
C              UPLO = 'L' or 'l'   A is a lower triangular matrix.
C
C           Unchanged on exit.
C
C  TRANS  - CHARACTER*1.
C           On entry, TRANS specifies the operation to be performed as
C           follows:
C
C              TRANS = 'N' or 'n'   x := A*x.
C
C              TRANS = 'T' or 't'   x := A'*x.
C
C              TRANS = 'C' or 'c'   x := A'*x.
C
C           Unchanged on exit.
C
C  DIAG   - CHARACTER*1.
C           On entry, DIAG specifies whether or not A is unit
C           triangular as follows:
C
C              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
C
C              DIAG = 'N' or 'n'   A is not assumed to be unit
C                                  triangular.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the order of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  A      - REAL             array of DIMENSION ( LDA, n ).
C           Before entry with  UPLO = 'U' or 'u', the leading n by n
C           upper triangular part of the array A must contain the upper
C           triangular matrix and the strictly lower triangular part of
C           A is not referenced.
C           Before entry with UPLO = 'L' or 'l', the leading n by n
C           lower triangular part of the array A must contain the lower
C           triangular matrix and the strictly upper triangular part of
C           A is not referenced.
C           Note that when  DIAG = 'U' or 'u', the diagonal elements of
C           A are not referenced either, but are assumed to be unity.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           max( 1, n ).
C           Unchanged on exit.
C
C  X      - REAL             array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the n
C           element vector x. On exit, X is overwritten with the
C           tranformed vector x.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      REAL               ZERO
      PARAMETER        ( ZERO = 0.0E+0 )
C     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     *         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     *         .NOT.LSAME( TRANS, 'T' ).AND.
     *         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     *         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRMV ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     *   RETURN
C
      NOUNIT = LSAME( DIAG, 'N' )
C
C     Set up the start point in X if the increment is not unity. This
C     will be  ( N - 1 )*INCX  too small for descending loops.
C
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
C
C     Start the operations. In this version the elements of A are
C     accessed sequentially with one pass through A.
C
      IF( LSAME( TRANS, 'N' ) )THEN
C
C        Form  x := A*x.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     *                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     *                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )
     *                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     *                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
C
C        Form  x := A'*x.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of STRMV .
C
      END
C
      REAL             FUNCTION SNRM2 ( N, X, INCX )
C     .. Scalar Arguments ..
      INTEGER                           INCX, N
C     .. Array Arguments ..
      REAL                              X( * )
C     ..
C
C  SNRM2 returns the euclidean norm of a vector via the function
C  name, so that
C
C     SNRM2 := sqrt( x'*x )
C
C
C
C  -- This version written on 25-October-1982.
C     Modified on 14-October-1993 to inline the call to SLASSQ.
C     Sven Hammarling, Nag Ltd.
C
C
C     .. Parameters ..
      REAL                  ONE         , ZERO
      PARAMETER           ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     .. Local Scalars ..
      INTEGER               IX
      REAL                  ABSXI, NORM, SCALE, SSQ
C     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
C     ..
C     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
C        The following loop is equivalent to this call to the LAPACK
C        auxiliary routine:
C        CALL SLASSQ( N, X, INCX, SCALE, SSQ )
C
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
C
      SNRM2 = NORM
      RETURN
C
C     End of SNRM2.
C
      END
C
      SUBROUTINE STRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     *                   B, LDB )
C     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      REAL               ALPHA
C     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * )
C     ..
C
C  Purpose
C  =======
C
C  STRMM  performs one of the matrix-matrix operations
C
C     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
C
C  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
C  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
C
C     op( A ) = A   or   op( A ) = A'.
C
C  Parameters
C  ==========
C
C  SIDE   - CHARACTER*1.
C           On entry,  SIDE specifies whether  op( A ) multiplies B from
C           the left or right as follows:
C
C              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
C
C              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
C
C           Unchanged on exit.
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the matrix A is an upper or
C           lower triangular matrix as follows:
C
C              UPLO = 'U' or 'u'   A is an upper triangular matrix.
C
C              UPLO = 'L' or 'l'   A is a lower triangular matrix.
C
C           Unchanged on exit.
C
C  TRANSA - CHARACTER*1.
C           On entry, TRANSA specifies the form of op( A ) to be used in
C           the matrix multiplication as follows:
C
C              TRANSA = 'N' or 'n'   op( A ) = A.
C
C              TRANSA = 'T' or 't'   op( A ) = A'.
C
C              TRANSA = 'C' or 'c'   op( A ) = A'.
C
C           Unchanged on exit.
C
C  DIAG   - CHARACTER*1.
C           On entry, DIAG specifies whether or not A is unit triangular
C           as follows:
C
C              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
C
C              DIAG = 'N' or 'n'   A is not assumed to be unit
C                                  triangular.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C           On entry, M specifies the number of rows of B. M must be at
C           least zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the number of columns of B.  N must be
C           at least zero.
C           Unchanged on exit.
C
C  ALPHA  - REAL            .
C           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
C           zero then  A is not referenced and  B need not be set before
C           entry.
C           Unchanged on exit.
C
C  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
C           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
C           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
C           upper triangular part of the array  A must contain the upper
C           triangular matrix  and the strictly lower triangular part of
C           A is not referenced.
C           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
C           lower triangular part of the array  A must contain the lower
C           triangular matrix  and the strictly upper triangular part of
C           A is not referenced.
C           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
C           A  are not referenced either,  but are assumed to be  unity.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
C           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
C           then LDA must be at least max( 1, n ).
C           Unchanged on exit.
C
C  B      - REAL             array of DIMENSION ( LDB, n ).
C           Before entry,  the leading  m by n part of the array  B must
C           contain the matrix  B,  and  on exit  is overwritten  by the
C           transformed matrix.
C
C  LDB    - INTEGER.
C           On entry, LDB specifies the first dimension of B as declared
C           in  the  calling  (sub)  program.   LDB  must  be  at  least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 3 Blas routine.
C
C  -- Written on 8-February-1989.
C     Jack Dongarra, Argonne National Laboratory.
C     Iain Duff, AERE Harwell.
C     Jeremy Du Croz, Numerical Algorithms Group Ltd.
C     Sven Hammarling, Numerical Algorithms Group Ltd.
C
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      REAL               TEMP
C     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
C
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     *         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     *         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     *         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'STRMM ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     *   RETURN
C
C     And when  alpha.eq.zero.
C
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
C
C     Start the operations.
C
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           Form  B := alpha*A*B.
C
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     *                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     *                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
C
C           Form  B := alpha*B*A'.
C
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     *                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     *                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           Form  B := alpha*B*A.
C
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
C
C           Form  B := alpha*B*A'.
C
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of STRMM .
C
      END
C
      SUBROUTINE SCOPY(N,SX,INCX,SY,INCY)
C
C     COPIES A VECTOR, X, TO A VECTOR, Y.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        SY(IY) = SX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        SY(I) = SX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        SY(I) = SX(I)
        SY(I + 1) = SX(I + 1)
        SY(I + 2) = SX(I + 2)
        SY(I + 3) = SX(I + 3)
        SY(I + 4) = SX(I + 4)
        SY(I + 5) = SX(I + 5)
        SY(I + 6) = SX(I + 6)
   50 CONTINUE
      RETURN
      END
C
      SUBROUTINE SGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     *                   BETA, Y, INCY )
C     .. Scalar Arguments ..
      REAL               ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
C     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
C     ..
C
C  Purpose
C  =======
C
C  SGEMV  performs one of the matrix-vector operations
C
C     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
C
C  where alpha and beta are scalars, x and y are vectors and A is an
C  m by n matrix.
C
C  Parameters
C  ==========
C
C  TRANS  - CHARACTER*1.
C           On entry, TRANS specifies the operation to be performed as
C           follows:
C
C              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
C
C              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
C
C              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C           On entry, M specifies the number of rows of the matrix A.
C           M must be at least zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the number of columns of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  ALPHA  - REAL            .
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  A      - REAL             array of DIMENSION ( LDA, n ).
C           Before entry, the leading m by n part of the array A must
C           contain the matrix of coefficients.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           max( 1, m ).
C           Unchanged on exit.
C
C  X      - REAL             array of DIMENSION at least
C           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
C           and at least
C           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
C           Before entry, the incremented array X must contain the
C           vector x.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C  BETA   - REAL            .
C           On entry, BETA specifies the scalar beta. When BETA is
C           supplied as zero then Y need not be set on input.
C           Unchanged on exit.
C
C  Y      - REAL             array of DIMENSION at least
C           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
C           and at least
C           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
C           Before entry with BETA non-zero, the incremented array Y
C           must contain the vector y. On exit, Y is overwritten by the
C           updated vector y.
C
C  INCY   - INTEGER.
C           On entry, INCY specifies the increment for the elements of
C           Y. INCY must not be zero.
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     .. Local Scalars ..
      REAL               TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     *         .NOT.LSAME( TRANS, 'T' ).AND.
     *         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGEMV ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     *    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     *   RETURN
C
C     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
C     up the start points in  X  and  Y.
C
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
C
C     Start the operations. In this version the elements of A are
C     accessed sequentially with one pass through A.
C
C     First form  y := beta*y.
C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     *   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
C
C        Form  y := alpha*A*x + y.
C
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
C
C        Form  y := alpha*A'*x + y.
C
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of SGEMV .
C
      END
C
      SUBROUTINE SSWAP (N,SX,INCX,SY,INCY)
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SX(*),SY(*),STEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        STEMP = SX(IX)
        SX(IX) = SY(IY)
        SY(IY) = STEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        STEMP = SX(I)
        SX(I) = SY(I)
        SY(I) = STEMP
        STEMP = SX(I + 1)
        SX(I + 1) = SY(I + 1)
        SY(I + 1) = STEMP
        STEMP = SX(I + 2)
        SX(I + 2) = SY(I + 2)
        SY(I + 2) = STEMP
   50 CONTINUE
      RETURN
      END
C
      SUBROUTINE SSCAL(N,SA,SX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      REAL SA,SX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        SX(I) = SA*SX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        SX(I) = SA*SX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        SX(I) = SA*SX(I)
        SX(I + 1) = SA*SX(I + 1)
        SX(I + 2) = SA*SX(I + 2)
        SX(I + 3) = SA*SX(I + 3)
        SX(I + 4) = SA*SX(I + 4)
   50 CONTINUE
      RETURN
      END
C
      SUBROUTINE SGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     *                   BETA, C, LDC )
C     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      REAL               ALPHA, BETA
C     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * ), C( LDC, * )
C     ..
C
C  Purpose
C  =======
C
C  SGEMM  performs one of the matrix-matrix operations
C
C     C := alpha*op( A )*op( B ) + beta*C,
C
C  where  op( X ) is one of
C
C     op( X ) = X   or   op( X ) = X',
C
C  alpha and beta are scalars, and A, B and C are matrices, with op( A )
C  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
C
C  Parameters
C  ==========
C
C  TRANSA - CHARACTER*1.
C           On entry, TRANSA specifies the form of op( A ) to be used in
C           the matrix multiplication as follows:
C
C              TRANSA = 'N' or 'n',  op( A ) = A.
C
C              TRANSA = 'T' or 't',  op( A ) = A'.
C
C              TRANSA = 'C' or 'c',  op( A ) = A'.
C
C           Unchanged on exit.
C
C  TRANSB - CHARACTER*1.
C           On entry, TRANSB specifies the form of op( B ) to be used in
C           the matrix multiplication as follows:
C
C              TRANSB = 'N' or 'n',  op( B ) = B.
C
C              TRANSB = 'T' or 't',  op( B ) = B'.
C
C              TRANSB = 'C' or 'c',  op( B ) = B'.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C           On entry,  M  specifies  the number  of rows  of the  matrix
C           op( A )  and of the  matrix  C.  M  must  be at least  zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry,  N  specifies the number  of columns of the matrix
C           op( B ) and the number of columns of the matrix C. N must be
C           at least zero.
C           Unchanged on exit.
C
C  K      - INTEGER.
C           On entry,  K  specifies  the number of columns of the matrix
C           op( A ) and the number of rows of the matrix op( B ). K must
C           be at least  zero.
C           Unchanged on exit.
C
C  ALPHA  - REAL            .
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
C           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
C           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
C           part of the array  A  must contain the matrix  A,  otherwise
C           the leading  k by m  part of the array  A  must contain  the
C           matrix A.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
C           LDA must be at least  max( 1, m ), otherwise  LDA must be at
C           least  max( 1, k ).
C           Unchanged on exit.
C
C  B      - REAL             array of DIMENSION ( LDB, kb ), where kb is
C           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
C           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
C           part of the array  B  must contain the matrix  B,  otherwise
C           the leading  n by k  part of the array  B  must contain  the
C           matrix B.
C           Unchanged on exit.
C
C  LDB    - INTEGER.
C           On entry, LDB specifies the first dimension of B as declared
C           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
C           LDB must be at least  max( 1, k ), otherwise  LDB must be at
C           least  max( 1, n ).
C           Unchanged on exit.
C
C  BETA   - REAL            .
C           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
C           supplied as zero then C need not be set on input.
C           Unchanged on exit.
C
C  C      - REAL             array of DIMENSION ( LDC, n ).
C           Before entry, the leading  m by n  part of the array  C must
C           contain the matrix  C,  except when  beta  is zero, in which
C           case C need not be set on entry.
C           On exit, the array  C  is overwritten by the  m by n  matrix
C           ( alpha*op( A )*op( B ) + beta*C ).
C
C  LDC    - INTEGER.
C           On entry, LDC specifies the first dimension of C as declared
C           in  the  calling  (sub)  program.   LDC  must  be  at  least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 3 Blas routine.
C
C  -- Written on 8-February-1989.
C     Jack Dongarra, Argonne National Laboratory.
C     Iain Duff, AERE Harwell.
C     Jeremy Du Croz, Numerical Algorithms Group Ltd.
C     Sven Hammarling, Numerical Algorithms Group Ltd.
C
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      REAL               TEMP
C     .. Parameters ..
      REAL               ONE         , ZERO
      PARAMETER        ( ONE = 1.0E+0, ZERO = 0.0E+0 )
C     ..
C     .. Executable Statements ..
C
C     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
C     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
C     and  columns of  A  and the  number of  rows  of  B  respectively.
C
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
C
C     Test the input parameters.
C
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     *         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     *         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     *         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'SGEMM ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     *    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     *   RETURN
C
C     And if  alpha.eq.zero.
C
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
C
C     Start the operations.
C
      IF( NOTB )THEN
         IF( NOTA )THEN
C
C           Form  C := alpha*A*B + beta*C.
C
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
C
C           Form  C := alpha*A'*B + beta*C
C
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
C
C           Form  C := alpha*A*B' + beta*C
C
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
C
C           Form  C := alpha*A'*B' + beta*C
C
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of SGEMM .
C
      END
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
C     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
C     ..
C
C  Purpose
C  =======
C
C  DGER   performs the rank 1 operation
C
C     A := alpha*x*y' + A,
C
C  where alpha is a scalar, x is an m element vector, y is an n element
C  vector and A is an m by n matrix.
C
C  Parameters
C  ==========
C
C  M      - INTEGER.
C           On entry, M specifies the number of rows of the matrix A.
C           M must be at least zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the number of columns of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  ALPHA  - DOUBLE PRECISION.
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  X      - DOUBLE PRECISION array of dimension at least
C           ( 1 + ( m - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the m
C           element vector x.
C           Unchanged on exit.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C  Y      - DOUBLE PRECISION array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCY ) ).
C           Before entry, the incremented array Y must contain the n
C           element vector y.
C           Unchanged on exit.
C
C  INCY   - INTEGER.
C           On entry, INCY specifies the increment for the elements of
C           Y. INCY must not be zero.
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
C           Before entry, the leading m by n part of the array A must
C           contain the matrix of coefficients. On exit, A is
C           overwritten by the updated matrix.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
C     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( M.LT.0 )THEN
         INFO = 1
      ELSE IF( N.LT.0 )THEN
         INFO = 2
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 5
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 7
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGER  ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     *   RETURN
C
C     Start the operations. In this version the elements of A are
C     accessed sequentially with one pass through A.
C
      IF( INCY.GT.0 )THEN
         JY = 1
      ELSE
         JY = 1 - ( N - 1 )*INCY
      END IF
      IF( INCX.EQ.1 )THEN
         DO 20, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               DO 10, I = 1, M
                  A( I, J ) = A( I, J ) + X( I )*TEMP
   10          CONTINUE
            END IF
            JY = JY + INCY
   20    CONTINUE
      ELSE
         IF( INCX.GT.0 )THEN
            KX = 1
         ELSE
            KX = 1 - ( M - 1 )*INCX
         END IF
         DO 40, J = 1, N
            IF( Y( JY ).NE.ZERO )THEN
               TEMP = ALPHA*Y( JY )
               IX   = KX
               DO 30, I = 1, M
                  A( I, J ) = A( I, J ) + X( IX )*TEMP
                  IX        = IX        + INCX
   30          CONTINUE
            END IF
            JY = JY + INCY
   40    CONTINUE
      END IF
C
      RETURN
C
C     End of DGER  .
C
      END
      SUBROUTINE  DROT (N,DX,INCX,DY,INCY,C,S)
C
C     APPLIES A PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      DOUBLE PRECISION DX(*),DY(*),DTEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = C*DX(IX) + S*DY(IY)
        DY(IY) = C*DY(IY) - S*DX(IX)
        DX(IX) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
        DTEMP = C*DX(I) + S*DY(I)
        DY(I) = C*DY(I) - S*DX(I)
        DX(I) = DTEMP
   30 CONTINUE
      RETURN
      END
      SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
C     .. SCALAR ARGUMENTS ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
C     ..
C
C  PURPOSE
C  =======
C
C  DTRMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
C
C     X := A*X,   OR   X := A'*X,
C
C  WHERE X IS AN N ELEMENT VECTOR AND  A IS AN N BY N UNIT, OR NON-UNIT,
C  UPPER OR LOWER TRIANGULAR MATRIX.
C
C  PARAMETERS
C  ==========
C
C  UPLO   - CHARACTER*1.
C           ON ENTRY, UPLO SPECIFIES WHETHER THE MATRIX IS AN UPPER OR
C           LOWER TRIANGULAR MATRIX AS FOLLOWS:
C
C              UPLO = 'U' OR 'U'   A IS AN UPPER TRIANGULAR MATRIX.
C
C              UPLO = 'L' OR 'L'   A IS A LOWER TRIANGULAR MATRIX.
C
C           UNCHANGED ON EXIT.
C
C  TRANS  - CHARACTER*1.
C           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
C           FOLLOWS:
C
C              TRANS = 'N' OR 'N'   X := A*X.
C
C              TRANS = 'T' OR 'T'   X := A'*X.
C
C              TRANS = 'C' OR 'C'   X := A'*X.
C
C           UNCHANGED ON EXIT.
C
C  DIAG   - CHARACTER*1.
C           On entry, DIAG specifies whether or not A is unit
C           triangular as follows:
C
C              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
C
C              DIAG = 'N' or 'n'   A is not assumed to be unit
C                                  triangular.
C
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the order of the matrix A.
C           N must be at least zero.
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
C           Before entry with  UPLO = 'U' or 'u', the leading n by n
C           upper triangular part of the array A must contain the upper
C           triangular matrix and the strictly lower triangular part of
C           A is not referenced.
C           Before entry with UPLO = 'L' or 'l', the leading n by n
C           lower triangular part of the array A must contain the lower
C           triangular matrix and the strictly upper triangular part of
C           A is not referenced.
C           Note that when  DIAG = 'U' or 'u', the diagonal elements of
C           A are not referenced either, but are assumed to be unity.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. LDA must be at least
C           max( 1, n ).
C           Unchanged on exit.
C
C  X      - DOUBLE PRECISION array of dimension at least
C           ( 1 + ( n - 1 )*abs( INCX ) ).
C           Before entry, the incremented array X must contain the n
C           element vector x. On exit, X is overwritten with the
C           tranformed vector x.
C
C  INCX   - INTEGER.
C           On entry, INCX specifies the increment for the elements of
C           X. INCX must not be zero.
C           Unchanged on exit.
C
C
C  Level 2 Blas routine.
C
C  -- Written on 22-October-1986.
C     Jack Dongarra, Argonne National Lab.
C     Jeremy Du Croz, Nag Central Office.
C     Sven Hammarling, Nag Central Office.
C     Richard Hanson, Sandia National Labs.
C
C
C     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
C     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     *         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     *         .NOT.LSAME( TRANS, 'T' ).AND.
     *         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     *         .NOT.LSAME( DIAG , 'N' )      )THEN
         INFO = 3
      ELSE IF( N.LT.0 )THEN
         INFO = 4
      ELSE IF( LDA.LT.MAX( 1, N ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMV ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     *   RETURN
C
      NOUNIT = LSAME( DIAG, 'N' )
C
C     Set up the start point in X if the increment is not unity. This
C     will be  ( N - 1 )*INCX  too small for descending loops.
C
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
C
C     Start the operations. In this version the elements of A are
C     accessed sequentially with one pass through A.
C
      IF( LSAME( TRANS, 'N' ) )THEN
C
C        Form  x := A*x.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     *                  X( J ) = X( J )*A( J, J )
                  END IF
   20          CONTINUE
            ELSE
               JX = KX
               DO 40, J = 1, N
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 30, I = 1, J - 1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      + INCX
   30                CONTINUE
                     IF( NOUNIT )
     *                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX + INCX
   40          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 60, J = N, 1, -1
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 50, I = N, J + 1, -1
                        X( I ) = X( I ) + TEMP*A( I, J )
   50                CONTINUE
                     IF( NOUNIT )
     *                  X( J ) = X( J )*A( J, J )
                  END IF
   60          CONTINUE
            ELSE
               KX = KX + ( N - 1 )*INCX
               JX = KX
               DO 80, J = N, 1, -1
                  IF( X( JX ).NE.ZERO )THEN
                     TEMP = X( JX )
                     IX   = KX
                     DO 70, I = N, J + 1, -1
                        X( IX ) = X( IX ) + TEMP*A( I, J )
                        IX      = IX      - INCX
   70                CONTINUE
                     IF( NOUNIT )
     *                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
C
C        Form  x := A'*x.
C
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 90, I = J - 1, 1, -1
                     TEMP = TEMP + A( I, J )*X( I )
   90             CONTINUE
                  X( J ) = TEMP
  100          CONTINUE
            ELSE
               JX = KX + ( N - 1 )*INCX
               DO 120, J = N, 1, -1
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 110, I = J - 1, 1, -1
                     IX   = IX   - INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  110             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   - INCX
  120          CONTINUE
            END IF
         ELSE
            IF( INCX.EQ.1 )THEN
               DO 140, J = 1, N
                  TEMP = X( J )
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 130, I = J + 1, N
                     TEMP = TEMP + A( I, J )*X( I )
  130             CONTINUE
                  X( J ) = TEMP
  140          CONTINUE
            ELSE
               JX = KX
               DO 160, J = 1, N
                  TEMP = X( JX )
                  IX   = JX
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 150, I = J + 1, N
                     IX   = IX   + INCX
                     TEMP = TEMP + A( I, J )*X( IX )
  150             CONTINUE
                  X( JX ) = TEMP
                  JX      = JX   + INCX
  160          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of DTRMV .
C
      END
      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
C     .. Scalar Arguments ..
      INTEGER                           INCX, N
C     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
C     ..
C
C  DNRM2 returns the euclidean norm of a vector via the function
C  name, so that
C
C     DNRM2 := sqrt( x'*x )
C
C
C
C  -- This version written on 25-October-1982.
C     Modified on 14-October-1993 to inline the call to DLASSQ.
C     Sven Hammarling, Nag Ltd.
C
C
C     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
C     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
C     ..
C     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
C        The following loop is equivalent to this call to the LAPACK
C        auxiliary routine:
C        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
C
         DO 10, IX = 1, 1 + ( N - 1 )*INCX, INCX
            IF( X( IX ).NE.ZERO )THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI )THEN
                  SSQ   = ONE   + SSQ*( SCALE/ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SSQ   = SSQ   +     ( ABSXI/SCALE )**2
               END IF
            END IF
   10    CONTINUE
         NORM  = SCALE * SQRT( SSQ )
      END IF
C
      DNRM2 = NORM
      RETURN
C
C     End of DNRM2.
C
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     *                   B, LDB )
C     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
C     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
C     ..
C
C  Purpose
C  =======
C
C  DTRMM  performs one of the matrix-matrix operations
C
C     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
C
C  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
C  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
C
C     op( A ) = A   or   op( A ) = A'.
C
C  Parameters
C  ==========
C
C  SIDE   - CHARACTER*1.
C           On entry,  SIDE specifies whether  op( A ) multiplies B from
C           the left or right as follows:
C
C              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
C
C              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
C
C           Unchanged on exit.
C
C  UPLO   - CHARACTER*1.
C           On entry, UPLO specifies whether the matrix A is an upper or
C           lower triangular matrix as follows:
C
C              UPLO = 'U' or 'u'   A is an upper triangular matrix.
C
C              UPLO = 'L' or 'l'   A is a lower triangular matrix.
C
C           Unchanged on exit.
C
C  TRANSA - CHARACTER*1.
C           On entry, TRANSA specifies the form of op( A ) to be used in
C           the matrix multiplication as follows:
C
C              TRANSA = 'N' or 'n'   op( A ) = A.
C
C              TRANSA = 'T' or 't'   op( A ) = A'.
C
C              TRANSA = 'C' or 'c'   op( A ) = A'.
C
C           Unchanged on exit.
C
C  DIAG   - CHARACTER*1.
C           On entry, DIAG specifies whether or not A is unit triangular
C           as follows:
C
C              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
C
C              DIAG = 'N' or 'n'   A is not assumed to be unit
C                                  triangular.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C           On entry, M specifies the number of rows of B. M must be at
C           least zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry, N specifies the number of columns of B.  N must be
C           at least zero.
C           Unchanged on exit.
C
C  ALPHA  - DOUBLE PRECISION.
C           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
C           zero then  A is not referenced and  B need not be set before
C           entry.
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
C           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
C           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
C           upper triangular part of the array  A must contain the upper
C           triangular matrix  and the strictly lower triangular part of
C           A is not referenced.
C           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
C           lower triangular part of the array  A must contain the lower
C           triangular matrix  and the strictly upper triangular part of
C           A is not referenced.
C           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
C           A  are not referenced either,  but are assumed to be  unity.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
C           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
C           then LDA must be at least max( 1, n ).
C           Unchanged on exit.
C
C  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
C           Before entry,  the leading  m by n part of the array  B must
C           contain the matrix  B,  and  on exit  is overwritten  by the
C           transformed matrix.
C
C  LDB    - INTEGER.
C           On entry, LDB specifies the first dimension of B as declared
C           in  the  calling  (sub)  program.   LDB  must  be  at  least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 3 Blas routine.
C
C  -- Written on 8-February-1989.
C     Jack Dongarra, Argonne National Laboratory.
C     Iain Duff, AERE Harwell.
C     Jeremy Du Croz, Numerical Algorithms Group Ltd.
C     Sven Hammarling, Numerical Algorithms Group Ltd.
C
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. Executable Statements ..
C
C     Test the input parameters.
C
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
C
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     *         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     *         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     *         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
         INFO = 4
      ELSE IF( M  .LT.0               )THEN
         INFO = 5
      ELSE IF( N  .LT.0               )THEN
         INFO = 6
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 9
      ELSE IF( LDB.LT.MAX( 1, M     ) )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DTRMM ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.EQ.0 )
     *   RETURN
C
C     And when  alpha.eq.zero.
C
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
C
C     Start the operations.
C
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           Form  B := alpha*A*B.
C
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     *                     TEMP = TEMP*A( K, K )
                        B( K, J ) = TEMP
                     END IF
   40             CONTINUE
   50          CONTINUE
            ELSE
               DO 80, J = 1, N
                  DO 70 K = M, 1, -1
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP      = ALPHA*B( K, J )
                        B( K, J ) = TEMP
                        IF( NOUNIT )
     *                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
C
C           Form  B := alpha*B*A'.
C
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     *                  TEMP = TEMP*A( I, I )
                     DO 90, K = 1, I - 1
                        TEMP = TEMP + A( K, I )*B( K, J )
   90                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  100             CONTINUE
  110          CONTINUE
            ELSE
               DO 140, J = 1, N
                  DO 130, I = 1, M
                     TEMP = B( I, J )
                     IF( NOUNIT )
     *                  TEMP = TEMP*A( I, I )
                     DO 120, K = I + 1, M
                        TEMP = TEMP + A( K, I )*B( K, J )
  120                CONTINUE
                     B( I, J ) = ALPHA*TEMP
  130             CONTINUE
  140          CONTINUE
            END IF
         END IF
      ELSE
         IF( LSAME( TRANSA, 'N' ) )THEN
C
C           Form  B := alpha*B*A.
C
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 150, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  150             CONTINUE
                  DO 170, K = 1, J - 1
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 160, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  160                   CONTINUE
                     END IF
  170             CONTINUE
  180          CONTINUE
            ELSE
               DO 220, J = 1, N
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( J, J )
                  DO 190, I = 1, M
                     B( I, J ) = TEMP*B( I, J )
  190             CONTINUE
                  DO 210, K = J + 1, N
                     IF( A( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*A( K, J )
                        DO 200, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  200                   CONTINUE
                     END IF
  210             CONTINUE
  220          CONTINUE
            END IF
         ELSE
C
C           Form  B := alpha*B*A'.
C
            IF( UPPER )THEN
               DO 260, K = 1, N
                  DO 240, J = 1, K - 1
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 230, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  230                   CONTINUE
                     END IF
  240             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 250, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  250                CONTINUE
                  END IF
  260          CONTINUE
            ELSE
               DO 300, K = N, 1, -1
                  DO 280, J = K + 1, N
                     IF( A( J, K ).NE.ZERO )THEN
                        TEMP = ALPHA*A( J, K )
                        DO 270, I = 1, M
                           B( I, J ) = B( I, J ) + TEMP*B( I, K )
  270                   CONTINUE
                     END IF
  280             CONTINUE
                  TEMP = ALPHA
                  IF( NOUNIT )
     *               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
C
      RETURN
C
C     End of DTRMM .
C
      END
      SUBROUTINE  DCOPY(N,DX,INCX,DY,INCY)
C
C     COPIES A VECTOR, X, TO A VECTOR, Y.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      DOUBLE PRECISION DX(*),DY(*)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DY(IY) = DX(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,7)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        DY(I) = DX(I)
   30 CONTINUE
      IF( N .LT. 7 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,7
        DY(I) = DX(I)
        DY(I + 1) = DX(I + 1)
        DY(I + 2) = DX(I + 2)
        DY(I + 3) = DX(I + 3)
        DY(I + 4) = DX(I + 4)
        DY(I + 5) = DX(I + 5)
        DY(I + 6) = DX(I + 6)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     *                   BETA, Y, INCY )
C     .. SCALAR ARGUMENTS ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
C     ..
C
C  PURPOSE
C  =======
C
C  DGEMV  PERFORMS ONE OF THE MATRIX-VECTOR OPERATIONS
C
C     Y := ALPHA*A*X + BETA*Y,   OR   Y := ALPHA*A'*X + BETA*Y,
C
C  WHERE ALPHA AND BETA ARE SCALARS, X AND Y ARE VECTORS AND A IS AN
C  M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  TRANS  - CHARACTER*1.
C           ON ENTRY, TRANS SPECIFIES THE OPERATION TO BE PERFORMED AS
C           FOLLOWS:
C
C              TRANS = 'N' OR 'N'   Y := ALPHA*A*X + BETA*Y.
C
C              TRANS = 'T' OR 'T'   Y := ALPHA*A'*X + BETA*Y.
C
C              TRANS = 'C' OR 'C'   Y := ALPHA*A'*X + BETA*Y.
C
C           UNCHANGED ON EXIT.
C
C  M      - INTEGER.
C           ON ENTRY, M SPECIFIES THE NUMBER OF ROWS OF THE MATRIX A.
C           M MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  N      - INTEGER.
C           ON ENTRY, N SPECIFIES THE NUMBER OF COLUMNS OF THE MATRIX A.
C           N MUST BE AT LEAST ZERO.
C           UNCHANGED ON EXIT.
C
C  ALPHA  - DOUBLE PRECISION.
C           ON ENTRY, ALPHA SPECIFIES THE SCALAR ALPHA.
C           UNCHANGED ON EXIT.
C
C  A      - DOUBLE PRECISION ARRAY OF DIMENSION ( LDA, N ).
C           BEFORE ENTRY, THE LEADING M BY N PART OF THE ARRAY A MUST
C           CONTAIN THE MATRIX OF COEFFICIENTS.
C           UNCHANGED ON EXIT.
C
C  LDA    - INTEGER.
C           ON ENTRY, LDA SPECIFIES THE FIRST DIMENSION OF A AS DECLARED
C           IN THE CALLING (SUB) PROGRAM. LDA MUST BE AT LEAST
C           MAX( 1, M ).
C           UNCHANGED ON EXIT.
C
C  X      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCX ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCX ) ) OTHERWISE.
C           BEFORE ENTRY, THE INCREMENTED ARRAY X MUST CONTAIN THE
C           VECTOR X.
C           UNCHANGED ON EXIT.
C
C  INCX   - INTEGER.
C           ON ENTRY, INCX SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           X. INCX MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C  BETA   - DOUBLE PRECISION.
C           ON ENTRY, BETA SPECIFIES THE SCALAR BETA. WHEN BETA IS
C           SUPPLIED AS ZERO THEN Y NEED NOT BE SET ON INPUT.
C           UNCHANGED ON EXIT.
C
C  Y      - DOUBLE PRECISION ARRAY OF DIMENSION AT LEAST
C           ( 1 + ( M - 1 )*ABS( INCY ) ) WHEN TRANS = 'N' OR 'N'
C           AND AT LEAST
C           ( 1 + ( N - 1 )*ABS( INCY ) ) OTHERWISE.
C           BEFORE ENTRY WITH BETA NON-ZERO, THE INCREMENTED ARRAY Y
C           MUST CONTAIN THE VECTOR Y. ON EXIT, Y IS OVERWRITTEN BY THE
C           UPDATED VECTOR Y.
C
C  INCY   - INTEGER.
C           ON ENTRY, INCY SPECIFIES THE INCREMENT FOR THE ELEMENTS OF
C           Y. INCY MUST NOT BE ZERO.
C           UNCHANGED ON EXIT.
C
C
C  LEVEL 2 BLAS ROUTINE.
C
C  -- WRITTEN ON 22-OCTOBER-1986.
C     JACK DONGARRA, ARGONNE NATIONAL LAB.
C     JEREMY DU CROZ, NAG CENTRAL OFFICE.
C     SVEN HAMMARLING, NAG CENTRAL OFFICE.
C     RICHARD HANSON, SANDIA NATIONAL LABS.
C
C
C     .. PARAMETERS ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     .. LOCAL SCALARS ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
C     .. EXTERNAL FUNCTIONS ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. EXTERNAL SUBROUTINES ..
      EXTERNAL           XERBLA
C     .. INTRINSIC FUNCTIONS ..
      INTRINSIC          MAX
C     ..
C     .. EXECUTABLE STATEMENTS ..
C
C     TEST THE INPUT PARAMETERS.
C
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     *         .NOT.LSAME( TRANS, 'T' ).AND.
     *         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 1
      ELSE IF( M.LT.0 )THEN
         INFO = 2
      ELSE IF( N.LT.0 )THEN
         INFO = 3
      ELSE IF( LDA.LT.MAX( 1, M ) )THEN
         INFO = 6
      ELSE IF( INCX.EQ.0 )THEN
         INFO = 8
      ELSE IF( INCY.EQ.0 )THEN
         INFO = 11
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMV ', INFO )
         RETURN
      END IF
C
C     QUICK RETURN IF POSSIBLE.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     *    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     *   RETURN
C
C     SET  LENX  AND  LENY, THE LENGTHS OF THE VECTORS X AND Y, AND SET
C     UP THE START POINTS IN  X  AND  Y.
C
      IF( LSAME( TRANS, 'N' ) )THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
      IF( INCX.GT.0 )THEN
         KX = 1
      ELSE
         KX = 1 - ( LENX - 1 )*INCX
      END IF
      IF( INCY.GT.0 )THEN
         KY = 1
      ELSE
         KY = 1 - ( LENY - 1 )*INCY
      END IF
C
C     START THE OPERATIONS. IN THIS VERSION THE ELEMENTS OF A ARE
C     ACCESSED SEQUENTIALLY WITH ONE PASS THROUGH A.
C
C     FIRST FORM  Y := BETA*Y.
C
      IF( BETA.NE.ONE )THEN
         IF( INCY.EQ.1 )THEN
            IF( BETA.EQ.ZERO )THEN
               DO 10, I = 1, LENY
                  Y( I ) = ZERO
   10          CONTINUE
            ELSE
               DO 20, I = 1, LENY
                  Y( I ) = BETA*Y( I )
   20          CONTINUE
            END IF
         ELSE
            IY = KY
            IF( BETA.EQ.ZERO )THEN
               DO 30, I = 1, LENY
                  Y( IY ) = ZERO
                  IY      = IY   + INCY
   30          CONTINUE
            ELSE
               DO 40, I = 1, LENY
                  Y( IY ) = BETA*Y( IY )
                  IY      = IY           + INCY
   40          CONTINUE
            END IF
         END IF
      END IF
      IF( ALPHA.EQ.ZERO )
     *   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
C
C        FORM  Y := ALPHA*A*X + Y.
C
         JX = KX
         IF( INCY.EQ.1 )THEN
            DO 60, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  DO 50, I = 1, M
                     Y( I ) = Y( I ) + TEMP*A( I, J )
   50             CONTINUE
               END IF
               JX = JX + INCX
   60       CONTINUE
         ELSE
            DO 80, J = 1, N
               IF( X( JX ).NE.ZERO )THEN
                  TEMP = ALPHA*X( JX )
                  IY   = KY
                  DO 70, I = 1, M
                     Y( IY ) = Y( IY ) + TEMP*A( I, J )
                     IY      = IY      + INCY
   70             CONTINUE
               END IF
               JX = JX + INCX
   80       CONTINUE
         END IF
      ELSE
C
C        FORM  Y := ALPHA*A'*X + Y.
C
         JY = KY
         IF( INCX.EQ.1 )THEN
            DO 100, J = 1, N
               TEMP = ZERO
               DO 90, I = 1, M
                  TEMP = TEMP + A( I, J )*X( I )
   90          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  100       CONTINUE
         ELSE
            DO 120, J = 1, N
               TEMP = ZERO
               IX   = KX
               DO 110, I = 1, M
                  TEMP = TEMP + A( I, J )*X( IX )
                  IX   = IX   + INCX
  110          CONTINUE
               Y( JY ) = Y( JY ) + ALPHA*TEMP
               JY      = JY      + INCY
  120       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     END OF DGEMV .
C
      END
      SUBROUTINE  DSWAP (N,DX,INCX,DY,INCY)
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        DTEMP = DX(IX)
        DX(IX) = DY(IY)
        DY(IY) = DTEMP
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        DTEMP = DX(I)
        DX(I) = DY(I)
        DY(I) = DTEMP
        DTEMP = DX(I + 1)
        DX(I + 1) = DY(I + 1)
        DY(I + 1) = DTEMP
        DTEMP = DX(I + 2)
        DX(I + 2) = DY(I + 2)
        DY(I + 2) = DTEMP
   50 CONTINUE
      RETURN
      END
      SUBROUTINE  DSCAL(N,DA,DX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C     MODIFIED 3/93 TO RETURN IF INCX .LE. 0.
C     MODIFIED 12/3/93, ARRAY(1) DECLARATIONS CHANGED TO ARRAY(*)
C
      DOUBLE PRECISION DA,DX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M.EQ.0 ) GO TO 40
      DO 30 I = 1,M
        DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     *                   BETA, C, LDC )
C     .. SCALAR ARGUMENTS ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
C     .. ARRAY ARGUMENTS ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
C     ..
C
C  PURPOSE
C  =======
C
C  DGEMM  PERFORMS ONE OF THE MATRIX-MATRIX OPERATIONS
C
C     C := ALPHA*OP( A )*OP( B ) + BETA*C,
C
C  WHERE  OP( X ) IS ONE OF
C
C     OP( X ) = X   OR   OP( X ) = X',
C
C  ALPHA AND BETA ARE SCALARS, AND A, B AND C ARE MATRICES, WITH OP( A )
C  AN M BY K MATRIX,  OP( B )  A  K BY N MATRIX AND  C AN M BY N MATRIX.
C
C  PARAMETERS
C  ==========
C
C  TRANSA - CHARACTER*1.
C           ON ENTRY, TRANSA SPECIFIES THE FORM OF OP( A ) TO BE USED IN
C           THE MATRIX MULTIPLICATION AS FOLLOWS:
C
C              TRANSA = 'N' OR 'N',  OP( A ) = A.
C
C              TRANSA = 'T' OR 'T',  OP( A ) = A'.
C
C              TRANSA = 'C' OR 'C',  OP( A ) = A'.
C
C           UNCHANGED ON EXIT.
C
C  TRANSB - CHARACTER*1.
C           On entry, TRANSB specifies the form of op( B ) to be used in
C           the matrix multiplication as follows:
C
C              TRANSB = 'N' or 'n',  op( B ) = B.
C
C              TRANSB = 'T' or 't',  op( B ) = B'.
C
C              TRANSB = 'C' or 'c',  op( B ) = B'.
C
C           Unchanged on exit.
C
C  M      - INTEGER.
C           On entry,  M  specifies  the number  of rows  of the  matrix
C           op( A )  and of the  matrix  C.  M  must  be at least  zero.
C           Unchanged on exit.
C
C  N      - INTEGER.
C           On entry,  N  specifies the number  of columns of the matrix
C           op( B ) and the number of columns of the matrix C. N must be
C           at least zero.
C           Unchanged on exit.
C
C  K      - INTEGER.
C           On entry,  K  specifies  the number of columns of the matrix
C           op( A ) and the number of rows of the matrix op( B ). K must
C           be at least  zero.
C           Unchanged on exit.
C
C  ALPHA  - DOUBLE PRECISION.
C           On entry, ALPHA specifies the scalar alpha.
C           Unchanged on exit.
C
C  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
C           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
C           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
C           part of the array  A  must contain the matrix  A,  otherwise
C           the leading  k by m  part of the array  A  must contain  the
C           matrix A.
C           Unchanged on exit.
C
C  LDA    - INTEGER.
C           On entry, LDA specifies the first dimension of A as declared
C           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
C           LDA must be at least  max( 1, m ), otherwise  LDA must be at
C           least  max( 1, k ).
C           Unchanged on exit.
C
C  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
C           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
C           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
C           part of the array  B  must contain the matrix  B,  otherwise
C           the leading  n by k  part of the array  B  must contain  the
C           matrix B.
C           Unchanged on exit.
C
C  LDB    - INTEGER.
C           On entry, LDB specifies the first dimension of B as declared
C           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
C           LDB must be at least  max( 1, k ), otherwise  LDB must be at
C           least  max( 1, n ).
C           Unchanged on exit.
C
C  BETA   - DOUBLE PRECISION.
C           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
C           supplied as zero then C need not be set on input.
C           Unchanged on exit.
C
C  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
C           Before entry, the leading  m by n  part of the array  C must
C           contain the matrix  C,  except when  beta  is zero, in which
C           case C need not be set on entry.
C           On exit, the array  C  is overwritten by the  m by n  matrix
C           ( alpha*op( A )*op( B ) + beta*C ).
C
C  LDC    - INTEGER.
C           On entry, LDC specifies the first dimension of C as declared
C           in  the  calling  (sub)  program.   LDC  must  be  at  least
C           max( 1, m ).
C           Unchanged on exit.
C
C
C  Level 3 Blas routine.
C
C  -- Written on 8-February-1989.
C     Jack Dongarra, Argonne National Laboratory.
C     Iain Duff, AERE Harwell.
C     Jeremy Du Croz, Numerical Algorithms Group Ltd.
C     Sven Hammarling, Numerical Algorithms Group Ltd.
C
C
C     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
C     .. External Subroutines ..
      EXTERNAL           XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC          MAX
C     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
C     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
C     ..
C     .. Executable Statements ..
C
C     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
C     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
C     and  columns of  A  and the  number of  rows  of  B  respectively.
C
      NOTA  = LSAME( TRANSA, 'N' )
      NOTB  = LSAME( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
C
C     Test the input parameters.
C
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     *         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     *         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     *         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     *         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEMM ', INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     *    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     *   RETURN
C
C     And if  alpha.eq.zero.
C
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
C
C     Start the operations.
C
      IF( NOTB )THEN
         IF( NOTA )THEN
C
C           Form  C := alpha*A*B + beta*C.
C
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
C
C           Form  C := alpha*A'*B + beta*C
C
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
C
C           Form  C := alpha*A*B' + beta*C
C
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
C
C           Form  C := alpha*A'*B' + beta*C
C
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
C
      RETURN
C
C     End of DGEMM .
C
      END
