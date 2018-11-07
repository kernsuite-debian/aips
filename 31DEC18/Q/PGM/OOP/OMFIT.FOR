C                                       SUBROUTINE INCLUDE
LOCAL INCLUDE 'FIT0.INC'
C                                       FIND MAGIC NUMBERS BY SEARCHING
C                                       FOR 'MAGIC'
C                                       MCP: max # comps
C                                       MEP: max # mean gain parameter
C                                       MSP: max # stations in data
C                                       MGP: max # global parameters
C                                       MXP: max # scratch parameters
C                                       MLP: max # local parameters
C                                       MTP: max # parameters
C                                            (MGP+2*MLP)
C                                       MMP: max of MGP and 2*MLP
C                                       MTT: max # solution intervals
C                                       MSPT: MTT*MSP
      INTEGER MCP, MEP, MSP, MGP, MXP, MLP, MCLP, MTP, MMP, MTT,
     *     MSPT, MST, MSC, NIF, NCHNS
      COMMON /FIT0/ MCP, MEP, MSP, MGP, MXP, MLP, MCLP, MTP, MMP, MTT,
     *     MSPT, MST, MSC, NIF, NCHNS
LOCAL END
LOCAL INCLUDE 'LINFO.INC'
C                                       LUNs assigned by AIPS for I/O
C                                       use
      INTEGER IND, PHILE, IPHILE, PL1, PL2, PRTLEV
      COMMON /LINFO/ PHILE, IPHILE, IND, PL1, PL2, PRTLEV
LOCAL END
LOCAL INCLUDE 'DINFO.INC'
C                                       info for selected data file
      INTEGER CHANI, IST, IIF
      COMMON /DINFO/ CHANI, IST, IIF
LOCAL END
LOCAL INCLUDE 'KINFO.INC'
C                                       Control Info used during program
C                                       execution
      INTEGER NITER, BIF, EIF, BEGCH, ENDCH, SNVERI, SNVERO
      DOUBLE PRECISION CUTOF, MLIMIT, SLIMIT, GAMMA, LGAMMA,
     *   GNFRAC, C2FRAC
      INTEGER TSMIN, TSMAX, MODVER
      REAL TSOLVE, SNRMIN, DOFTAR(30), WTPOW
C
      INTEGER NPIBEG, NPIEND, NAIBEG, NAIEND, NGIBEG, NGIEND
      INTEGER NPITER, NGITER, NSCAL, OUTTYP
      CHARACTER SOLMOD*3, BLSEL*8, STCODE*4
C
      REAL BRANGE(2)
C
      LOGICAL LAMP, LPHI, SIMLOC, LITER, LPRNT, LEXPLA, WRITER
      INTEGER LNOCOV
C
      COMMON /KINFO/ MLIMIT, SLIMIT, GAMMA, LGAMMA, CUTOF,
     *   GNFRAC, C2FRAC,
     *   TSOLVE, NPITER, NGITER, TSMIN, TSMAX, MODVER, NITER,
     *   SNRMIN, LITER, LPRNT, LEXPLA, LNOCOV, DOFTAR,
     *   LAMP, LPHI, SIMLOC, NSCAL, BIF, EIF, BRANGE, WRITER,
     *   BEGCH, ENDCH, SNVERI, SNVERO, OUTTYP,
     *   NPIBEG, NPIEND, NAIBEG, NAIEND, NGIBEG, NGIEND, WTPOW
      COMMON /KINFOB/ SOLMOD, BLSEL, STCODE
LOCAL END
LOCAL INCLUDE 'IINFO.INC'
C                                       iterative info
      INTEGER ITER, NEWPLS, OLDPLS, CHAN
      COMMON /IINFO/ ITER, NEWPLS, OLDPLS, CHAN
LOCAL END
LOCAL INCLUDE 'VINFO.INC'
C                                       buffers for current visibility
      REAL HDRBUF(100), VISBUF(2048)
C                                       info pertinent to
C                                       current visibility
      INTEGER IA, IB
      INTEGER LOCU, LOCV, LOCW, LOCT, LOCB, LOCSU, LOCFQ, LOCA1, LOCA2,
     *   LOCSA, LOCC, LOCS, LOCF, LOCR, LOCD, LOCIF, INCS, INCF, INCIF,
     *   NCOR, NAXI(20)
      REAL TIME, RPIX(20), RDEL(20)
      REAL BU, BV, BW, BUU, BUV, BVV
      DOUBLE PRECISION FREQ0, FREQV(2048), RVAL(20)
C
      COMMON /VINFO1/ IA, IB, NCOR, NAXI
      COMMON /VINFO2/ LOCU, LOCV, LOCW, LOCT, LOCB, LOCSU, LOCFQ,
     *   LOCA1, LOCA2, LOCSA, LOCC, LOCS, LOCF, LOCR, LOCD, LOCIF, INCS,
     *   INCF, INCIF
      COMMON /VINFO3/ TIME, HDRBUF, RPIX, RDEL
      COMMON /VINFO4/ FREQ0, RVAL, BU, BV, BW, BUU, BUV, BVV
      COMMON /VINFO5/ FREQV
      COMMON /VINFO6/ VISBUF
LOCAL END
LOCAL INCLUDE 'SINFO.INC'
C                                       statistics info
      DOUBLE PRECISION DELRMS, GNORM
      CHARACTER ANTNAM(300)*8
      COMMON /SINFO/  GNORM, DELRMS
      DOUBLE PRECISION OVRLAP, NFREE, PNFREE, PSCPAR, FTOS,
     *   AVGW, CHI2, NCHI2, RCHI2, RNOISE, MNOIZ, PMNOIZ, SCPAR,
     *   PAVGW, PCHI2, PNCHI2, PRCHI2, MAXDEV, OTHRSH, LOWSNR, LTHRSH,
     *   SUMGP, SUMLP, SUMSQR, SUMCOR, SUMWGT, SUMWT2, SUMLPS, SUMGPS
      COMMON /STATS/ OVRLAP, NFREE, PNFREE, PSCPAR, FTOS,
     *   AVGW, CHI2, NCHI2, RCHI2, RNOISE, MNOIZ, PMNOIZ, SCPAR,
     *   PAVGW, PCHI2, PNCHI2, PRCHI2, MAXDEV, OTHRSH, LOWSNR, LTHRSH,
     *   SUMGP, SUMLP, SUMSQR, SUMCOR, SUMWGT, SUMWT2, SUMLPS, SUMGPS
      COMMON /SCINFO/ ANTNAM
LOCAL END
LOCAL INCLUDE 'ZINFO.INC'
C                                       declare standard sky constants
      INCLUDE 'INCS:PSTD.INC'
C                                       POSSK(1) = VLA scaled positions
C                                       POSSK(2) = VLBI scaled positions
C                                       DISSK(1) = VLA scaled disks
C                                       DISSK(2) = VLBI scaled disks
C                                       FWHM(1)  = VLA scaled sizes
C                                       FWHM(2)  = VLBI scaled sizes
      DOUBLE PRECISION DISSK(2), FWHM(2), POSSK(2), NATSKY, MAS2RD
      DOUBLE PRECISION SCALE, BMIN, BMAX, SIZMIN
      COMMON /CODES0/ DISSK, FWHM, POSSK, NATSKY, MAS2RD
      COMMON /CODES2/ SCALE, BMIN, BMAX, SIZMIN
LOCAL END
CCCCCCCCCCCC
C  OMFIT TO DO LIST
C priority
C 0 = for NEXT checkin
C 1 = for checkin after that
C 2 = think about this
C
C still to do:
C 0 check SN table phases, amps, and SNR, and polarization structure
C 0 models to be tested: GAINS POL MAS ZEE HAL
C 0 ask Eric about memory allocation formula [grep for ZMEMRY!]
C
C 1 fix kluge in matrix inversion
C 1 read in previous SN table [written by OMFIT]
C 1 print ALL AIPS ADVERBS in OUTFILE

CCCCCCCCCCCC
      PROGRAM OMFIT
C-----------------------------------------------------------------------
C! Simultaneously self-calibrates and fits source models to UV data
C# UV MODELING CALIBRATION VLBI
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1998-2003, 2005-2007, 2010, 2012, 2015, 2018
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
C   Task for modelfitting of UV data.  This task can simultaneously
C   fit for and self-calibrate multiple components of different
C   modeltypes each.
C   The following features are currently implemented
C      Levenberg-Marquardt Non-linear modelfitting
C      Singular Value Decomposition inversion of matrices
C      Multiple components each with possibly different model parameters
C      Single Self-calibration solutions per antenna
C      Multiple self-calibration solutions per antenna
C   The following features will be implemented in the near? future
C      Fringe-fitting with differing solution intervals per antenna
C   Comments, suggestions, complaints to ... kdesai@nrao.edu
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INFILE*48, UVSCR*36
      INTEGER IRET, SCBUF(256)
      PARAMETER (PRGM = 'OMFIT')
      PARAMETER (UVSCR = 'UV Scratch Object')
C-----------------------------------------------------------------------
C                                       open file, read inputs, prepare
      CALL OMOOP (PRGM, INFILE, UVSCR, IRET)
C                                       do the job
      IF (IRET.EQ.0) CALL OMLMM (UVSCR, INFILE, IRET)
C                                       die
      CALL DIE (IRET, SCBUF)
C
 999  STOP
      END
      SUBROUTINE OMOOP (PRGM, INFILE, UVSCR, JRET)
C-----------------------------------------------------------------------
C   OMOOP gets input adverbs from AIPS and prepares the
C   UV data file for model fitting
C   Inputs:
C      PRGM    C*(*) Program name
C      INFILE  C*(*) KEYIN format input file containing control info
C   Output:
C      UVSCR   C*(*) Name of scratch file containing data for UV-fitting
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
      CHARACTER PRGM*(*), INFILE*(*), UVSCR*(*)
      INTEGER JRET

      INTEGER DIM(7), TYPE, IDUM
      CHARACTER CDUM*2
      REAL RDUM

      INTEGER ILUN, NLUN
      REAL TMIN, TMAX
      DOUBLE PRECISION DLAMCH
      LOGICAL TRUE, FALSE
      PARAMETER (TRUE = .TRUE.)
      PARAMETER (FALSE = .FALSE.)

      CHARACTER POPS*36, UVIN*36, ISNTAB*36, OSNTAB*36, ANSCR*36,
     *                   UVOUT*36
      PARAMETER (POPS   = 'POPS object')
      PARAMETER (UVIN   = 'INPUT UV object')
      PARAMETER (ISNTAB = 'Input SN Table Object')
      PARAMETER (OSNTAB = 'Output SN Table Object')
      PARAMETER (ANSCR  = 'Scratch file AN Table Object')
      PARAMETER (UVOUT  = 'OUTPUT UV Object')

      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'FIT0.INC'
      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
      INCLUDE 'SINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C
      INTEGER NKEY1
      PARAMETER (NKEY1=42)
      INTEGER TKEY1(NKEY1), DKEY1(2,NKEY1)
      CHARACTER CKEY1(NKEY1)*8
C
      INTEGER   NKEY2
      PARAMETER (NKEY2=4)
      CHARACTER IKEY2(NKEY2)*8, OKEY2(NKEY2)*16
C
      INTEGER   NKEY3
      PARAMETER (NKEY3=19)
      CHARACTER IKEY3(NKEY3)*8, OKEY3(NKEY3)*16
C
      INTEGER   NKEY4
      PARAMETER (NKEY4=4)
      CHARACTER IKEY4(NKEY4)*8, OKEY4(NKEY4)*16
C
      INTEGER   NADV
      PARAMETER (NADV=5)
      CHARACTER LIST(NADV)*8
C
      INTEGER   SNVER, DBIF, DEIF, OBCHAN, OECHAN, ONEID(30)
      LOGICAL   INPSN, OUTSN, APPEND, CLOSU, LTEMP
      INTEGER   I, J, IROUND, TSNUM, NSTAT, COUNT
      CHARACTER OSOLMO*4, OTFILE*48, OTPATH*48, OTPRNT*48
      REAL      APARM(10), DPARM(10), NOIZ(64), XDOCAL
      DOUBLE PRECISION SCALE, BMIN, BMAX
      CHARACTER FNAME*12, FCLAS*6, DNAME*48
      INTEGER   FSEQU, FDISK, IWTIT, JTRIM, JT
      CHARACTER*16 SCHAR(30), TXLINE*256

C
      DATA CKEY1
     *     /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  ',
     *      'SOURCES ', 'SUBARRAY', 'TIMERANG', 'ANTENNAS',
     *      'UVRANGE ', 'STOKES  ', 'FREQID  ', 'BCHAN   ',
     *      'ECHAN   ', 'BIF     ', 'EIF     ', 'DOCALIB ',
     *      'GAINUSE ', 'DOPOL',    'PDVER',    'BLVER',    'FLAGVER ',
     *      'DOBAND',   'BPVER',    'SMOOTH',   'INFILE  ', 'DOFIT   ',
     *      'OUTFILE ', 'PRTLEV  ', 'OUTPRINT',
     *      'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK ',
     *      'OUTVER  ', 'NOISE   ', 'APARM   ',
C     *      'SNVER   ',
     *      'NITER   ', 'SOLMODE ',
     *      'SOLINT  ', 'DPARM   ', 'WEIGHTIT', 'BADDISK '/
      DATA TKEY1
     *     / OOACAR, OOACAR, OOAINT, OOAINT,
     *       OOACAR, OOAINT, OOARE , OOAINT,
     *       OOARE , OOACAR, OOAINT, OOAINT,
     *       OOAINT, OOAINT, OOAINT, OOARE,
     *       OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
     *       OOAINT, OOAINT, OOARE,  OOACAR, OOARE ,
     *       OOACAR, OOAINT, OOACAR,
     *       OOACAR, OOACAR, OOAINT, OOAINT,
     *       OOAINT, OOARE , OOARE ,
C     *       OOAINT,
     *       OOAINT, OOACAR,
     *       OOARE , OOARE , OOAINT, OOAINT/
      DATA DKEY1
     *     / 12,1  ,  6,1  ,  1,1  ,  1,1  ,
     *       16,30 ,  1,1  ,  8,1  , 50,1  ,
     *        2,1  ,  4,1  ,  1,1  ,  1,1  ,
     *        1,1  ,  1,1  ,  1,1  ,  1,1  ,
     *        1,1  ,  1,1  ,  1,1  ,  1,1  ,  1,1  ,
     *        1,1  ,  1,1  ,  3,1,   48,1  , 30,1  ,
     *       48,1  ,  1,1  ,  48,1 ,
     *       12,1  ,  6,1  ,  1,1  ,  1,1  ,
     *        1,1  , 64,1  , 10,1  ,
C     *        1,1  ,
     *        1,1  ,  4,1  ,
     *        1,1  , 10,1  , 1,1, 10,1  /
      DATA IKEY2
     *     /'INNAME  ', 'INCLASS ', 'INSEQ   ', 'INDISK  '/
      DATA OKEY2
     *     /'FILE_NAME.NAME  ', 'FILE_NAME.CLASS ',
     *      'FILE_NAME.IMSEQ ', 'FILE_NAME.DISK  '/

      DATA IKEY3
     *     /'SOURCES ', 'SUBARRAY', 'TIMERANG', 'ANTENNAS',
     *      'UVRANGE ', 'STOKES  ', 'FREQID  ', 'BCHAN   ',
     *      'ECHAN   ', 'BIF     ', 'EIF     ', 'GAINUSE ',
     *      'FLAGVER ', 'DOPOL   ', 'BLVER   ', 'DOBAND  ',
     *      'BPVER   ', 'SMOOTH  ', 'PDVER'/
      DATA OKEY3
     *     /'CALEDIT.SOURCS  ', 'CALEDIT.SUBARR  ',
     *      'CALEDIT.TIMRNG  ', 'CALEDIT.ANTENNS ',
     *      'CALEDIT.UVRNG   ', 'CALEDIT.STOKES  ',
     *      'CALEDIT.FRQSEL  ', 'CALEDIT.BCHAN   ',
     *      'CALEDIT.ECHAN   ', 'CALEDIT.BIF     ',
     *      'CALEDIT.EIF     ', 'CALEDIT.CLUSE   ',
     *      'CALEDIT.FGVER   ', 'CALEDIT.DOPOL   ',
     *      'CALEDIT.BLVER   ', 'CALEDIT.DOBAND  ',
     *      'CALEDIT.BPVER   ', 'CALEDIT.SMOOTH  ',
     *      'CALEDIT.PDVER   '/
C                                       Output file specification info
      DATA IKEY4 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK '/
      DATA OKEY4 /'OUTNAME ', 'OUTCLASS', 'OUTSEQ  ', 'OUTDISK '/
C     history cards
      DATA LIST /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'INFILE'/
      DATA DPARM /10*0.0/
C-----------------------------------------------------------------------
      JRET = 0
C                                       read adverbs into POPSob
      CALL AV2INP (PRGM, NKEY1, CKEY1, TKEY1, DKEY1, POPS, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       load baddisk parameters
      CALL OGET (POPS, 'BADDISK', TYPE, DIM, IBAD, CDUM, JRET)
C                                       load data file name into DNAME
      CALL OGET (POPS, 'INNAME', TYPE, DIM, IDUM, FNAME, JRET)
      CALL OGET (POPS, 'INCLASS', TYPE, DIM, IDUM, FCLAS, JRET)
      CALL OGET (POPS, 'INSEQ', TYPE, DIM, FSEQU, CDUM, JRET)
      CALL OGET (POPS, 'INDISK', TYPE, DIM, FDISK, CDUM, JRET)
      WRITE (DNAME, 14001) FDISK, FNAME, FCLAS, FSEQU
C                                       MODIFY selection parameters
C                                       create UV data object
      CALL CREATE (UVIN, 'UVDATA', JRET)
      IF (JRET.NE.0) GO TO 990
C                                       copy over selection parameters
      CALL IN2OBJ (POPS, NKEY2, IKEY2, OKEY2, UVIN, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       synchronize header info
      CALL OOPEN (UVIN, 'READ', JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OCLOSE (UVIN, JRET)
      IF (JRET.NE.0) GO TO 990
C                                     Process IF/Channel/Stokes parameters
C
C                                             Get BIF/EIF from data set
      CALL OGET (UVIN, 'BIF', TYPE, DIM, DBIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (UVIN, 'EIF', TYPE, DIM, DEIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                             Get BIF/EIF from POPS
      CALL OGET (POPS, 'BIF', TYPE, DIM, BIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (POPS, 'EIF', TYPE, DIM, EIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                             Get BCHAN/ECHAN from data set
      CALL OGET (UVIN, 'BCHAN', TYPE, DIM, OBCHAN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (UVIN, 'ECHAN', TYPE, DIM, OECHAN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                             Get BCHAN/ECHAN from POPS
      CALL OGET (POPS, 'BCHAN', TYPE, DIM, BEGCH, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (POPS, 'ECHAN', TYPE, DIM, ENDCH, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                             Use special limits
      IF (BEGCH.EQ.0) THEN
         BEGCH = OBCHAN
         ENDCH = OECHAN
         END IF
      IF (BIF.EQ.0) THEN
         BIF = DBIF
         EIF = DEIF
         END IF
C                                             Check normal limits
      BIF = MAX (BIF, DBIF)
      EIF = MIN (EIF, DEIF)
      EIF = MAX (EIF, BIF)
      BEGCH = MAX (BEGCH, OBCHAN)
      ENDCH = MIN (ENDCH, OECHAN)
      ENDCH = MAX (ENDCH, BEGCH)
C                                             Put back chosen values
      CALL OPUT (POPS, 'BIF', TYPE, DIM, BIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (POPS, 'EIF', TYPE, DIM, EIF, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (POPS, 'BCHAN', TYPE, DIM, BEGCH, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (POPS, 'ECHAN', TYPE, DIM, ENDCH, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get selected STOKES code
      CALL OGET (POPS, 'STOKES', TYPE, DIM, IDUM, STCODE, JRET)
      IF (JRET.NE.0) GO TO 990
      IF (STCODE.EQ.'    ') STCODE = 'I   '
      IF ((STCODE.NE.'HALF').AND.
     *    (STCODE.NE.'RR  ').AND.(STCODE.NE.'LL  ').AND.
     *    (STCODE.NE.'I   ')) STCODE = 'I   '
      CALL OPUT (POPS, 'STOKES', TYPE, DIM, IDUM, STCODE, JRET)
      IF (JRET.NE.0) GO TO 990
C-----------------------------------------------------------------------
C                                       Create scratch file
C                                       copy over selection parameters
      CALL IN2OBJ (POPS, NKEY3, IKEY3, OKEY3, UVIN, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       DOCALIB
      CALL OGET (POPS, 'DOCALIB', TYPE, DIM, XDOCAL, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      LTEMP = XDOCAL.GT.0.0
      CALL OPUT (UVIN, 'CALEDIT.DOCAL', OOALOG, DIM, LTEMP, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      LTEMP = (XDOCAL.GT.0.0) .AND. (XDOCAL.LE.99.0)
      CALL OPUT (UVIN, 'CALEDIT.DOWTCL', OOALOG, DIM, LTEMP, CDUM,
     *   JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get selected baseline range
      CALL OGET (POPS, 'UVRANGE', TYPE, DIM, BRANGE, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                     Get source list from POPS
      CALL OGET (POPS, 'SOURCES', TYPE, DIM, IDUM, SCHAR, JRET)
      IF (JRET.NE.0) GO TO 990
C                                     Initialize source IDs
      DO 90 I = 1,30
         ONEID(I) = -1
 90      CONTINUE
      IF (SCHAR(1).NE.' ') THEN
         CALL GTSOR (UVIN, SCHAR(1), ONEID, JRET)
         IF (JRET.NE.0) GO TO 990
         END IF
C                                       copy INPUT file into SCRATCH file
      CALL CREATE (UVSCR, 'UVDATA', JRET)
      IF (JRET.NE.0) GO TO 990
C
      CALL UV2SCR (UVIN, UVSCR, TRUE, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       synchronize header info
      CALL OOPEN (UVSCR, 'READ', JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OCLOSE (UVSCR, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       prep for UVOUTob
      CALL IN2OBJ (POPS, NKEY4, IKEY4, OKEY4, UVSCR, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       put away source id - if found
      TYPE = OOAINT
      DIM(1) = 30
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVSCR, 'SORCEL', TYPE, DIM, ONEID, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       check if outna was specified:
      CALL OGET (POPS, 'OUTNAME', TYPE, DIM, IDUM, FNAME, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       was an output file requested?
      WRITER = FNAME.NE.' '
C                                       if residual file requested, prep outfile
      IF (WRITER) THEN
C                                       Create UVOUTob from UVINob
         CALL OCLONE (UVSCR, UVOUT, JRET)
         IF (JRET.NE.0) GO TO 990
C                                       Zero numvis. in UVOUTob
         CALL OGET (UVOUT, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUM,
     *        JRET)
         IF (JRET.NE.0) GO TO 990
         COUNT = 0
         CALL OPUT (UVOUT, 'UV_DESC.GCOUNT', TYPE, DIM, COUNT, CDUM,
     *     JRET)
         TYPE = OOACAR
         DIM(1) = 36
         DIM(2) = 1
         DIM(3) = 0
C                                       store output file name in UVSCR
         CALL OPUT (UVSCR, 'UV_OUTNAME', TYPE, DIM, IDUM, UVOUT, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL UVDTCO (UVSCR, UVOUT, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL OHCOPY (UVIN, UVOUT, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL OHLIST (POPS, LIST, NADV, UVOUT, JRET)
         IF (JRET.NE.0) GO TO 990
C                                       set residual type here
         CALL OGET (POPS, 'OUTVER', TYPE, DIM, OUTTYP, CDUM, JRET)
         IF (JRET.NE.0) GO TO 990
         END IF

C                                       scratch file is now ready!
C                                       set up LUNs, EGREISEN has approved!
      IND    = 10
      PHILE  = 11
      PL1    = 42
      PL2    = 43
C                                       get input modelfile name
      CALL OGET (POPS, 'INFILE', TYPE, DIM, IDUM, INFILE, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL ZTXOPN ('QRED', PHILE, IPHILE, INFILE, FALSE, JRET)
      IF (JRET.NE.0) THEN
         INFILE = 'no valid file, used default model'
         MSGTXT = 'Invalid INFILE, will use default model'
         CALL MSGWRT (3)
         JRET = 0
      ELSE
         CALL ZTXCLS (PHILE, IPHILE, JRET)
         END IF
C                                       get OUTPRINT and decompose it
C                                       to find the place to put
C                                       scratch files
      CALL OGET (POPS, 'OUTPRINT', TYPE, DIM, IDUM, OTPRNT, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       find position of ':' in OTPRNT
      I = INDEX(OTPRNT,':')
C                                       if printing was specified, do it.
      LPRNT = I.GT.1
      IF (LPRNT) THEN
         CALL ZTXOPN ('QWRT', PHILE, IPHILE, OTPRNT, TRUE, JRET)
C                                       if a valid file/location was found,
C                                       use it!
         IF (JRET.EQ.0) THEN
            WRITE (TXLINE,1500)
            JT = JTRIM (TXLINE)
            CALL ZTXIO ('WRIT', PHILE, IPHILE, TXLINE(:JT), JRET)
            IF (JRET.NE.0) LPRNT = .FALSE.
C            WRITE (PHILE, 1501)
         ELSE
            LPRNT = .FALSE.
            END IF
         END IF
C                                       OTPRNT is where the model residuals
C                                       will go
      CALL OPUT (UVSCR, 'OUTPRNT', TYPE, DIM, IDUM, OTPRNT, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       pick a path for the scratch files
      IF (LPRNT) THEN
C                                       OTPRNT was specified, use it
         OTPATH = OTPRNT(1:I-1)
      ELSE
C                                       OTPRNT was not specified, use defaults
         OTPATH = 'FITS'
         END IF
C                                       OTPATH contains pathname for scratch
C                                       files
      CALL OPUT (UVSCR, 'OUTPATH', TYPE, DIM, IDUM, OTPATH, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get output file for model
      CALL OGET (POPS, 'OUTFILE', TYPE, DIM, IDUM, OTFILE, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       check outfile
      IF (OTFILE(1:1).NE.' ') THEN
C                                       try to open for write
         NLUN = IND
         MSGSUP = 32000
C                                       if a '+' appears at end of filename
C                                       set append = TRUE
         I = INDEX(OTFILE,'+')
         J = INDEX(OTFILE,' ')
         APPEND = (I+1).EQ.J
C                                       dont forget to remove '+' sign
         IF (APPEND) OTFILE(I:I) = ' '
C
         CALL ZTXOPN ('QWRT', NLUN, ILUN, OTFILE, APPEND, JRET)
         MSGSUP = 0
         IF (JRET.NE.0) THEN
C                                       warn user and blank outfile!
            JRET = 0
            MSGTXT='Invalid OUTFILE, will not write final model out'
            CALL MSGWRT (3)
            OTFILE = ' '
         ELSE
C                                       dump input adverbs to outfile:
C
C                                       close model file
            CALL ZTXCLS (NLUN, ILUN, JRET)
            END IF
         END IF
      CALL OPUT (UVSCR, 'OUTFILE', TYPE, DIM, IDUM, OTFILE, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (UVSCR, 'INFILE', TYPE, DIM, IDUM, INFILE, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (UVSCR, 'DNAME', TYPE, DIM, IDUM, DNAME, JRET)
      IF (JRET.NE.0) GO TO 990
C
C                                       check SELF-CAL solution requested
      CALL OGET (POPS, 'SOLMODE', TYPE, DIM, IDUM, OSOLMO, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       first pick up default AIPS mode
      SOLMOD = '   '
      DO 91 I = 1,4
C                                       amplitude self-cal
         IF (OSOLMO(I:I).EQ.'A') SOLMOD(2:2) = 'A'
C                                       phase self-cal
         IF (OSOLMO(I:I).EQ.'P') SOLMOD(3:3) = 'P'
C                                       multi-component self-cal
         IF (OSOLMO(I:I).EQ.'M') SOLMOD(1:1) = 'M'
 91      CONTINUE
      IF (SOLMOD.NE.'   ') THEN
C                                       default is to force closure
         CLOSU = .TRUE.
         DO 92 I = 1,4
            IF (OSOLMO(I:I).EQ.'B') CLOSU = .FALSE.
 92         CONTINUE
      ELSE
C                                       default is to not force
C                                       closure
         CLOSU = .FALSE.
         DO 93 I = 1,4
            IF (OSOLMO(I:I).EQ.'T') CLOSU = .TRUE.
 93         CONTINUE
         END IF
      IF (CLOSU) THEN
         BLSEL = 'TRIANGLE'
      ELSE
         BLSEL = 'BASELINE'
         END IF
C                                       attach self-cal solution to UVSCR
      TYPE  = 3
      DIM(1) = 3
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVSCR, 'SOLMOD', TYPE, DIM, IDUM, SOLMOD, JRET)
      IF (JRET.NE.0) GO TO 990

C                                       niter is the number of iterations the
C                                       user has requested
      CALL OGET (POPS, 'NITER', TYPE, DIM, NITER, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      IF (NITER.EQ.0) NITER = 40
      IF (NITER.LT.0) NITER = 0
C                                       version number in model file
      CALL OGET (POPS, 'DOFIT', TYPE, DIM, DOFTAR, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      MODVER = INT(DOFTAR(1))
C                                       if an infile was specified, ignore
C                                       all but the first entry of DIFITARY
      IF ((INFILE.NE.'no valid file, used default model').OR.
     *     (DOFTAR(30).EQ.DOFTAR(1))) THEN
         DO 109 I=1,30
            DOFTAR(I) = 0.0
 109        CONTINUE
         END IF
C                                       what is the machine limit on precision?
C                                       start with machine precision
      SLIMIT = DLAMCH ('Safe')
      MLIMIT = DLAMCH ('Epsilon')
      CUTOF = MLIMIT
C                                       get print level for diagnostics
      CALL OGET (POPS, 'PRTLEV', TYPE, DIM, PRTLEV, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      LEXPLA = PRTLEV.GE.100
      IF (LEXPLA) PRTLEV = PRTLEV - 100
      LITER = PRTLEV.GE.10
      IF (LITER) PRTLEV = PRTLEV - 10
C                                       go over file to get statistics
      CALL UVSTAT (UVSCR, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get starting timestamp
      CALL OGET (UVSCR, 'TMIN', TYPE, DIM, TMIN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       - and final timestamp
      CALL OGET (UVSCR, 'TMAX', TYPE, DIM, TMAX, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       solint is the solution interval
C                                       of the data in seconds
      TSOLVE = 0.0
      IF (SOLMOD(1:3).NE.'   ') THEN
         CALL OGET (POPS, 'SOLINT', TYPE, DIM, TSOLVE, CDUM, JRET)
         IF (JRET.NE.0) GO TO 990
         END IF
C                                       if no solution interval specified
      IF (TSOLVE.LE.0.0) THEN
         TSOLVE = TMAX*1.01
      ELSE
C                                       otherwise convert to days
         TSOLVE = TSOLVE / 86400.0
         END IF
C                                       get minimum baseline
      CALL OGET (UVSCR, 'BMIN', TYPE, DIM, RDUM, CDUM, JRET)
      BMIN = RDUM
      IF (JRET.NE.0) GO TO 990
C                                       - and maximum baseline
      CALL OGET (UVSCR, 'BMAX', TYPE, DIM, RDUM, CDUM, JRET)
      BMAX = RDUM
      IF (JRET.NE.0) GO TO 990
C                                         and UV scale factor
      CALL OGET (UVSCR, 'UVSCALE', TYPE, DIM, RDUM, CDUM, JRET)
      SCALE = RDUM
      IF (JRET.NE.0) GO TO 990
C
      IF (PRTLEV.GT.0) THEN
         WRITE (MSGTXT,1111) BMIN*SCALE/1000.0D0, BMAX*SCALE/1000.0D0
         CALL MSGWRT (3)
         END IF
C                                       get maximum antenna number
      CALL OGET (UVSCR, 'MAXANT', TYPE, DIM, NSTAT, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       this is arbitrary, we're using DMA
      IF (NSTAT.GT.300) THEN
         MSGTXT = ' Sorry, too many antennas, RECOMPILE PROGRAM'
         CALL MSGWRT (3)
         GO TO 990
         END IF
      CALL UV2TAB (UVSCR, ANSCR, 'AN', 1, JRET)
C                                       get names for all the antennas
      DO 95 I = 1,NSTAT
         CALL ANTNFO (ANSCR, 1, I, 'ANNAME', TYPE, DIM, IDUM,
     *      ANTNAM(I), JRET)
 95      CONTINUE

C                                       find  maximum timestamp number
 300  CONTINUE
      TSMAX = INT (TMAX / TSOLVE)
      TSMIN = INT (TMIN / TSOLVE) - 1
      TSNUM = TSMAX - TSMIN
      IF (TSNUM.GT.100000) THEN
C                                       this is arbitrary, we are using DMA
         MSGTXT = 'Thats a lot of solution intervals, doubling SOLINT'
         CALL MSGWRT (3)
         TSOLVE = TSOLVE * 2.0
         GO TO 300
         END IF
C
      CALL OGET (POPS, 'WEIGHTIT', TYPE, DIM, IWTIT, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (POPS, 'APARM', TYPE, DIM, APARM, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (POPS, 'NOISE', TYPE, DIM, NOIZ, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       if no noise estimate is given,
C                                       set=-1; assume NOIZ(1) is a
C                                       visibility noise amplitude
C                                       estimate, we need noise in the
C                                       real and imaginary parts
C                                       separately:
C                                       [note that for normal statistics,
C                                       \sigma_A = \sigma_R = \sigma_I]
      RNOISE = NOIZ(1)
      IF (RNOISE.LE.0.0) RNOISE = 1.0
C                                       set user threshold for editting
C                                       data; IF NOIZ(2) = 0, no
C                                       editting at all.
      OTHRSH = MAX(NOIZ(2),0.0)
      OTHRSH = OTHRSH*OTHRSH
      IF (OTHRSH.EQ.0.0) OTHRSH = 100.0
      LTHRSH = NOIZ(3) * NOIZ(3)
C
      LITER = (OTFILE(1:1).NE.' ').AND.LITER
      LEXPLA  = (OTFILE(1:1).NE.' ').AND.LEXPLA
C                                       The Chi-Squared must change less
C                                       than this value to force premature
C                                       termination
      C2FRAC = APARM(1)
C                                       The gradient norm must fall below
C                                       this value to force premature termination
      GNFRAC = APARM(2)
C                                       The initial LMM factor determines
C                                       how fast the soln moves.
      LGAMMA = APARM(3)
C                                       Minimum SNR to accept - watch out!
C                                       OMFIT's SNR calculation may not agree
      SNRMIN = 0.0
      LNOCOV = IROUND(APARM(4))
C                                       default initial longest baseline
C                                       is to use all baselines
      IF (C2FRAC.EQ.0.0D0) C2FRAC = 1.0D0
      IF (GNFRAC.EQ.0.0D0) GNFRAC = 0.01D0
C                                       these limits need work!
      IF (LGAMMA.LT.-60.0D0) LGAMMA = -60.0D0
      IF (LGAMMA.GT. 60.0D0) LGAMMA =  60.0D0
      GAMMA = 2.0D0**LGAMMA
C                                       default is to accept all possible
C                                       SNRs
      SNRMIN = MAX (SNRMIN, 0.0)
C
      IF (SOLMOD(1:3).NE.'   ') THEN
         CALL OGET (POPS, 'DPARM', TYPE, DIM, DPARM, CDUM, JRET)
         IF (JRET.NE.0) GO TO 990
         END IF
      NPIBEG = IROUND(DPARM(1))
      NPIEND = IROUND(DPARM(2))
      NAIBEG = IROUND(DPARM(3))
      NAIEND = IROUND(DPARM(4))
      NGIBEG = IROUND(DPARM(5))
      NGIEND = IROUND(DPARM(6))
      IF (SOLMOD(3:3).EQ.'P') THEN
         IF (NPIBEG.EQ.0) NPIBEG = 1
         IF (NPIEND.EQ.0) NPIEND = NITER
      ELSE
         NPIBEG = 0
         NPIEND = 0
         END IF
      IF (SOLMOD(2:2).EQ.'A') THEN
         IF (NAIBEG.EQ.0) NAIBEG = 1
         IF (NAIEND.EQ.0) NAIEND = NITER
      ELSE
         NAIBEG = 0
         NAIEND = 0
         END IF
      IF (NGIBEG.EQ.0) NGIBEG = 1
      IF (NGIEND.EQ.0) NGIEND = NITER
      TYPE = 5
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C                                       MFB ADDITION
      WTPOW = 1.0
      IF (IWTIT.EQ.1) WTPOW = 0.5
      IF (IWTIT.EQ.2) WTPOW = 0.25
      IF (IWTIT.GT.2) WTPOW = 0.0
C                                       get number of vis/record
      CALL OGET (UVSCR, 'UV_DESC.NCORR', TYPE, DIM, NCOR, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get order,number of vis/record
      CALL OGET (UVSCR, 'UV_DESC.NAXIS', TYPE, DIM, NAXI, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (UVSCR, 'UV_DESC.CRVAL', TYPE, DIM, RVAL, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (UVSCR, 'UV_DESC.CRPIX', TYPE, DIM, RPIX, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OGET (UVSCR, 'UV_DESC.CDELT', TYPE, DIM, RDEL, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVDPNT (UVSCR, LOCU, LOCV, LOCW, LOCT, LOCB, LOCSU, LOCFQ,
     *   LOCA1, LOCA2, LOCSA, LOCC, LOCS, LOCF, LOCR, LOCD, LOCIF, INCS,
     *   INCF, INCIF, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVFRQS (UVSCR, FREQ0, FREQV, JRET)
      IF (JRET.NE.0) GO TO 990
      IF (LOCIF.GT.0) THEN
         NIF = NAXI(LOCIF)
      ELSE
         NIF = 1
         END IF
      IF (LOCS.GT.0) THEN
         MST = NAXI(LOCS)
      ELSE
         MST = 1
         END IF
C                                       get the number of correlators per vis.
C                                         record
      IF (LOCF.GT.0) THEN
         NCHNS = NAXI(LOCF)
      ELSE
         NCHNS = 1
         END IF
      OVRLAP = NIF*MST * NCHNS
C
      MSP = NSTAT
      MTT = TSNUM
      MSPT = MSP*MTT
C                                       need to pass in NSTAT
      MEP = MSP
      IF (INFILE.EQ.'no valid file, used default model') THEN
         CALL OMSIYE (MCP, MGP, MXP, MEP, JRET)
      ELSE
         CALL OMSIZE (INFILE, ONEID, MCP, MGP, MXP, MEP, MSP, NIF, MST,
     *        JRET)
         END IF
      IF (JRET.NE.0) GO TO 990

C     parse self-calibration stuff here.
C                                       SOLMOD contains the user selected
C                                       solution mode for self-cal
      MSC = 1
      IF (SOLMOD(1:1).EQ.'M') MSC = 2
      IF (SOLMOD(1:3).EQ.'   ') MSC = 0
C
      NSCAL = 0
      IF (SOLMOD(2:2).EQ.'A') NSCAL = 1
      IF (SOLMOD(3:3).EQ.'P') NSCAL = NSCAL + 1
C
C                                       If user wants to start from
C                                       an existing SN table, then
C                                       SNVER should be nonzero
C
C                                       Check if user requested an SN table
      SNVER = 0
C     CALL OGET (POPS, 'SNVER', TYPE, DIM, SNVER, CDUM, JRET)
C     IF (JRET.NE.0) GO TO 990
C                                       was SNVER specified?
      INPSN = SNVER.GT.0
      IF (INPSN) THEN
C                                       If specified, does it exist?
         CALL UV2TAB (UVIN, ISNTAB, 'SN', SNVER, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL TABEXI (ISNTAB, INPSN, JRET)
         IF (INPSN.AND.(JRET.EQ.0)) THEN
C                                       is it accessible?
            CALL TABOPN (ISNTAB, 'READ', JRET)
            IF (JRET.EQ.0) CALL TABCLO (ISNTAB, JRET)
C                                       INPSN = table accessible?
            INPSN = JRET.EQ.0
            JRET = 0
            END IF
C                                       if accessible,
C                                       attach table object to scratch file
         IF (INPSN) THEN
            TYPE = 3
            DIM(1) = 36
            DIM(2) = 1
            DIM(3) = 0
            CALL OPUT (UVSCR, 'ISNTAB', TYPE, DIM, IDUM, ISNTAB, JRET)
            IF (JRET.NE.0) GO TO 990
            SNVERI = SNVER
         ELSE
C                                       otherwise specified table had problem
C                                         ignore incoming SN table
            MSGTXT = 'Problem with user specified SNVER, ignoring SNVER'
            JRET = 0
            SNVERI = -1
            END IF
         END IF
C                                       if INPSN=T, an input table awaits
C                                       reading
      TYPE = 5
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVSCR, 'INPSN', TYPE, DIM, INPSN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
C                                       If self-cal requested, prep for
C                                       output SN table
      OUTSN = (SOLMOD(1:3).NE.'   ').OR.(MEP.GT.0)
      IF (OUTSN) THEN
C                                       find highest existing SN table number
         CALL UV2TAB (UVIN, OSNTAB, 'SN', 1, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL TBLHIV (OSNTAB, SNVER, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL TABDES (OSNTAB, JRET)
C                                       increment it by one
         SNVER = SNVER + 1
C                                       create the output SN table object
         CALL UV2TAB (UVIN, OSNTAB, 'SN', SNVER, JRET)
         IF (JRET.NE.0) GO TO 990
C                                       attach table object to scratch file
         TYPE = 3
         DIM(1) = 36
         DIM(2) = 1
         DIM(3) = 0
         CALL OPUT (UVSCR, 'OSNTAB', TYPE, DIM, IDUM, OSNTAB, JRET)
         IF (JRET.NE.0) GO TO 990
         WRITE (MSGTXT,1200) SNVER
         CALL MSGWRT (3)
         IF (PRTLEV.GT.2) THEN
            IF (MSC.GT.1) THEN
               MSGTXT = '  Be Warned, if # components > 25'
               CALL MSGWRT (3)
               MSGTXT = '  a self-cal solution will be computed but'
               CALL MSGWRT (3)
               MSGTXT = '  will NOT be written to an SN table !!!!!!'
               CALL MSGWRT (3)
               END IF
            END IF
         SNVERO = SNVER
      ELSE
         SNVERO = -1
         END IF
C                                       if OUTSN=T, an output table is needed
      TYPE = 5
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVSCR, 'OUTSN', TYPE, DIM, OUTSN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990

C                                      local parameters are simple if
C                                      there were no input locals and
C                                      if self-cal was not requested.
      SIMLOC = (.NOT.INPSN).AND.(SOLMOD(1:3).EQ.'   ')
C
C                                      but note that OUTSN depends upon
C                                      sensitivities as well.
C
C                                       adjust MSC and MLP
      IF (MSC.GT.1) THEN
         MSC = MCP
         END IF
      MCLP = MST * NIF * MSP * MSC
      MLP = 2*MCLP
      MTP = MGP + MLP
      MMP = MAX (MGP, MLP)
      GO TO 999
C
 990  MSGTXT = 'Problem with input adverbs or INFILE'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
14001 FORMAT ('Disk',I4,' Filename= ',A12,' . ',A6,' . ',I4)
 1111 FORMAT ('Baselines range from ',F10.3,' to ',F10.3,' klambdas')
 1200 FORMAT ('Self-Calibration solution will be written to SN#',I3)
 1500 FORMAT ('31DEC12 IA IB IF Chan Pol Time(sec)',3X,'Freq(Hz)',
     *   4X,'U(lambda) ',6X,'V(lambda) ',6X,'W(lambda) ',
     *   3X,'Weight ',9X,'Re(Vis) ',6X,'Im(Vis) ',6X,'Re(Mod) ',
     *   6X,'Im(Mod) ',6X,'Re(CMod)',6X,'Im(CMod)')
      END
      SUBROUTINE OMSIYE (NCOMP, NSCAT, NSCRA, NGAINS, JRET)
C-----------------------------------------------------------------------
C  OMSIYE process info when there is no INFILE!
C   OMSIYE gets control information from INFILE, a KEYIN format text file
C   Input:
C   Output:
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      INTEGER JRET
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'FIT0.INC'

      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
C                                       internal variables
      INTEGER MLEN, SLEN
      INTEGER NKEY, NCOMP, NSCAT, NSCRA, NGAINS
C                                       HERE ARE SOME MAGIC NUMBERS!
      PARAMETER (NKEY=256)
      CHARACTER CVALUE(NKEY)*8
C-----------------------------------------------------------------------
      JRET = 0
      NGAINS = 0
      NCOMP = 1
C                                       zero is still a point source
      IF (MODVER.LE.0) MODVER = 0
C                                       get number of parameters
      SLEN = MAX (MODVER,1)
      CALL MODELN (CVALUE(1), MLEN, SLEN)
C                                       count the number of parameters
      NSCAT = MLEN
C                                       count the number of scratch parameters
      NSCRA = SLEN
 999  RETURN
      END
      SUBROUTINE OMKEY (XGOBA, CGOBA, LGOBA, JRET)
C-----------------------------------------------------------------------
C   OMKEY gets control information when there is no INFILE
C   Output:
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      INTEGER JRET
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FIT0.INC'
      CHARACTER CGOBA(MCP)*8
      LOGICAL LGOBA(MGP)
      DOUBLE PRECISION XGOBA(MXP)

      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
C                                       internal variables
      INTEGER I, MLEN, SLEN
      INTEGER NCOMP, NSCAT, MSCAT
C                                       HERE ARE SOME MAGIC NUMBERS!!
      DOUBLE PRECISION INGLOB(30), SOUT(1), SGOBA(1)
      CHARACTER MTYPE*8, COUT(1)*8, UOUT(1)*8
C-----------------------------------------------------------------------
      JRET = 0
      IF (MODVER.EQ.0) THEN
         CALL POINTI (DOFTAR)
         END IF

C                                       get model type and number of params
      SLEN = MAX (MODVER,1)
      CALL MODELN (MTYPE, MLEN, SLEN)
C                                       line is good, increment models counter
      NCOMP = 1
C                                       save model type
      CGOBA(1) = MTYPE
C                                       save #scratch pars for this component
      XGOBA(1) = SLEN
C                                       save #model pars for this component
      XGOBA(2) = MLEN
C                                       set generic source id
      XGOBA(3) = 0
C                                       set beginning channel
      XGOBA(4) = 0
C                                       set end channel
      XGOBA(5) = 0
C                                       read model pars for this component
      DO 141 I = 1,MLEN
         INGLOB(I) = DOFTAR(2*I)
         LGOBA(I) =  DOFTAR(2*I+1).GT.0.0
 141     CONTINUE
      CALL MODELD (CGOBA(NCOMP), 'To-Prog', XGOBA(6), INGLOB,
     *   SGOBA, SOUT, COUT, UOUT)
      MSCAT = XGOBA(1)
      NSCAT = XGOBA(2)
C
 999  RETURN
      END
      SUBROUTINE OMSIZE (INFILE, SORCEL, NCOMP, NSCAT, NSCRA, NGAINS,
     *   XMSP, XNIF, XMST, JRET)
C-----------------------------------------------------------------------
C   OMSIZE gets control information from INFILE, a KEYIN format text file
C   Input:
C      INFILE  C*(*) KEYIN format text file containing control parameters
C   Output:
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER INFILE*(*)
      INTEGER JRET, XMSP, XMST, XNIF, SORCEL(30)
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
C      INCLUDE 'FIT0.INC'

      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
C                                       internal variables
      INTEGER NLUN, ILUN, MLEN, SLEN, JKEY, I, J
      INTEGER NKEY, KEYMOD, NCOMP, NSCAT, NSCRA, NGAINS, DISVER
C                                       HERE ARE SOME MAGIC NUMBERS!
      PARAMETER (NKEY=256)
      CHARACTER CVALUE(NKEY)*8, CDUMMY(NKEY)*8
      DOUBLE PRECISION DVALUE(NKEY)
      LOGICAL   FALSE
      CHARACTER KEYEND*8, MTYPE*8
      DATA FALSE, KEYEND /.FALSE., '/'/
C-----------------------------------------------------------------------
      JRET = 0
C                                       open INFILE for reading
      NLUN = IND
      CALL ZTXOPN ('QRED', NLUN, ILUN, INFILE, FALSE, JRET)
      DISVER = 0
      NGAINS = 0
      NSCAT = 0
      NSCRA = 0
      NCOMP = 0
C                                       count up the components in the INFILE
 100  CONTINUE
         CALL DFILL (NKEY, 0.0D0, DVALUE)
         JKEY = NKEY
         KEYMOD = 3
         IF (PRTLEV.GT.2) KEYMOD = 4
         CALL KEYIN (CDUMMY, DVALUE, CVALUE, JKEY, KEYEND, KEYMOD,
     *        NLUN, ILUN, JRET)
         IF (JRET.NE.0) THEN
            WRITE (MSGTXT,1100) JRET, NCOMP+1
            CALL MSGWRT (3)
            JRET = -2
            GO TO 990
            END IF
C                                       empty line => end of file
         IF (JKEY.EQ.0) GO TO 150
C                                       accept new model version number
         IF (CVALUE(1).EQ.'VERS') THEN
C                                       save new model version number
            DISVER = INT(DVALUE(2))
            END IF
C                                       start reading file when version matches
         IF ((MODVER.GT.0).AND.(MODVER.NE.DISVER)) GO TO 100
C                                       if no version was given, reset each time
C                                          VERS appears in input file.
         IF (MODVER.EQ.0) THEN
C                                       on first line, reset all values
            IF (CVALUE(1).EQ.'VERS') THEN
               NGAINS = 0
               NSCAT = 0
               NSCRA = 0
               NCOMP = 0
               GO TO 100
               END IF
         END IF
C                                       check modeltype, #pars expected
         MLEN = -1
         SLEN = 0
         CALL MODELN (CVALUE(1), MLEN, SLEN)
C                                       if model line specifies mean gains
C                                       [only one gains line is needed
C                                       to activate gains solutions
         IF (MLEN.EQ.0) THEN
            IF ((NGAINS.EQ.0).AND.(DVALUE(2).GE.0.0D0).AND.
     *          (DVALUE(2).LE.XMSP)) THEN
               NGAINS = XMSP * XNIF*XMST
               NSCAT = NSCAT + NGAINS
               SLEN = 1 + NGAINS
               END IF
            END IF
C                                       if modeltype is recognised
         I = DVALUE(3)
         J = DVALUE(4)
         IF ((MLEN.GT.0).AND.
C                                       if comp lies within chosen channel range
     *       ((I.EQ.0).OR.(I.LE.ENDCH)).AND.
     *       ((J.EQ.0).OR.(J.GE.BEGCH))      ) THEN
C
            I = -1
            IF (SORCEL(1).GT.0) I = DVALUE(2)
            IF (I.GT.0) THEN
C                                       check source id numbers
               DO 200 J = 1,30
                  IF (I.EQ.SORCEL(J)) GO TO 210
 200              CONTINUE
C                                       THIS SOURCE ID DID NOT MATCH
               GO TO 100
 210           CONTINUE
               END IF
C                                       line too short
            IF (JKEY.LT.(4+2*MLEN)) THEN
               WRITE (MSGTXT,1300) MTYPE, 4+2*MLEN
               CALL MSGWRT (1)
               WRITE (MSGTXT,1320) JKEY
               CALL MSGWRT (1)
               GO TO 100
               END IF
C                                       line too long, just give a warning!
C                                         and ignore rest of parameters
            IF (JKEY.GT.(4+2*MLEN)) THEN
               WRITE (MSGTXT,1300) MTYPE, 4+2*MLEN
               CALL MSGWRT (1)
               WRITE (MSGTXT,1310) JKEY
               CALL MSGWRT (1)
               END IF
C                                       count the number of components
            NCOMP = NCOMP + 1
C                                       count the number of parameters
            NSCAT = NSCAT + MLEN
            END IF
C                                       count the number of scratch parameters
         NSCRA = NSCRA + SLEN
C                                       loop back for more!
         GO TO 100
C                                       come here after empty line to close up
 150     CONTINUE
C                                       if MODVER = 0, accept whatever last version was
      IF (MODVER.EQ.0) MODVER = DISVER
C                                       close infile
      CALL ZTXCLS (NLUN, ILUN, JRET)
C                                       if no components, this is a problem!
      IF (NCOMP.EQ.0) THEN
         JRET = 1
         MSGTXT = '*****  OMSIZE FINDS NO COMPONENTS ******'
         CALL MSGWRT (8)
         END IF
C                                       done
      GO TO 999
C                                       ERROR
 990  MSGTXT = '******  OMSIZE REPORTS ERROR CONDITION  *****'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('****** Error #',I3,' reading scattering term #',I3)
 1300 FORMAT ('****** Model ',A8,' incorrectly specified, expected ',I4)
 1310 FORMAT ('       terms, found ',I4,'.  Truncating line ')
 1320 FORMAT ('       terms, found ',I4,'.  Skipping line ')
      END
      SUBROUTINE OMKEZ (INFILE, SORCEL, XGOBA, CGOBA, LGOBA, JRET)
C-----------------------------------------------------------------------
C   OMKEZ gets control information from INFILE, a KEYIN format text file
C   Input:
C      INFILE  C*(*) KEYIN format text file containing control parameters
C   Output:
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER INFILE*(*)
      INTEGER JRET, SORCEL(30)
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FIT0.INC'
      INTEGER BGOBA, EGOBA
      CHARACTER CGOBA(MCP)*8
      LOGICAL LGOBA(MGP)
      DOUBLE PRECISION XGOBA(MXP)

      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
C                                       internal variables
      INTEGER NLUN, ILUN, I, MLEN, SLEN, JKEY, J, K, L, SID, Z
      INTEGER NKEY, KEYMOD, NCOMP, NSCAT, MSCAT, DISVER
C                                       HERE ARE SOME MAGIC NUMBERS!!
      DOUBLE PRECISION INGLOB(30)
      PARAMETER (NKEY=256)
      CHARACTER CVALUE(NKEY)*8, CDUMMY(NKEY)*8
      DOUBLE PRECISION DVALUE(NKEY), GAIDEF, SGOBA(1), SOUT(1)
      LOGICAL FALSE, GAIFLG
      CHARACTER KEYEND*8, MTYPE*8, COUT(1)*8, UOUT(1)*8
      DATA FALSE, KEYEND /.FALSE., '/'/
C-----------------------------------------------------------------------
C                                       open INFILE for reading
      NLUN = IND
      CALL ZTXOPN ('QRED', NLUN, ILUN, INFILE, FALSE, JRET)
      DISVER = 0
      NSCAT = 0
      MSCAT = 0
      NCOMP = 0
 101  CONTINUE
C                                       initialize DVALUE
         CALL DFILL (NKEY, 0.0D0, DVALUE)
         JKEY = NKEY
         KEYMOD = 3
         CALL KEYIN (CDUMMY, DVALUE, CVALUE, JKEY, KEYEND, KEYMOD,
     *        NLUN, ILUN, JRET)
         IF (JRET.NE.0) THEN
            WRITE (MSGTXT,1100) JRET, NCOMP+1
            CALL MSGWRT (3)
            GO TO 990
            END IF
C                                       empty line => end of file
         IF (JKEY.EQ.0) GO TO 151
C                                       accept new model version number
         IF (CVALUE(1).EQ.'VERS') THEN
            DISVER = INT(DVALUE(2))
            END IF
C                                       start reading file when version matches
         IF (MODVER.NE.DISVER) GO TO 101
C                                       get model type
         MTYPE = CVALUE(1)
C                                       get #model pars for this model type
         MLEN = -1
         SLEN = 0
         CALL MODELN (MTYPE, MLEN, SLEN)
C                                       if not valid model type => skip line
         IF (MLEN.LT.0) GO TO 101
C                                       is this a mean gains line and is
C                                       it the first one?
C                                       sensitivity line
         IF (MLEN.EQ.0) THEN
C                                       NB: if a default line is present,
C                                       it should be the first one since it
C                                       overwrites previous gain information
C
            XGOBA(MXP-MEP) = MEP
C                                       get station number
            J = DVALUE(2)
C                                       process a default gains line
            IF ((J.EQ.0).AND.(JKEY.GE.4)) THEN
               INGLOB(1) = MST*NIF*MSP
               GAIDEF = DVALUE(2+1)
               GAIFLG = CVALUE(2+2).NE.'F'
               DO 119 L = 1, MSP
                  DO 118 K = 1, NIF
                     DO 117 I = 1, MST
                        J = I + (K-1)*MST + (L-1)*MST*NIF
                        INGLOB(1+J) = GAIDEF
                        LGOBA(MGP-MEP+J) = GAIFLG
 117                    CONTINUE
 118                 CONTINUE
 119              CONTINUE
               CALL MODELD('GAINS','To-Prog',XGOBA(MXP-MEP+1),INGLOB,
     *            SGOBA, SOUT, COUT, UOUT)
               J = 0
               END IF
C                                       process a normal gains line
            IF ((J.GT.0).AND.(J.LE.MSP)) THEN
C                                       get station position in gains area
C                                     get index of first position to be filled
               I = 2
C                                     set default values
               GAIDEF = 1.0D0
               GAIFLG = .TRUE.
C                                     loop over all possible slots to fill
               INGLOB(1) = NIF*MST
               DO 126 IIF = 1, NIF
                  DO 125 IST = 1, MST
                     K = (IIF-1)*MST + IST
C                                     if extra elements present, use them
                     IF ((I+2).LE.JKEY) THEN
                        GAIDEF = DVALUE(I+1)
                        GAIFLG = CVALUE(I+2).NE.'F'
                        I = I + 2
                     END IF
                     INGLOB(1+K) = GAIDEF
                     L = MGP - MEP + (J-1)*NIF*MST + K
                     LGOBA(L) = GAIFLG
 125              CONTINUE
 126           CONTINUE
               K = MXP - MEP + 1 + (J-1)*NIF*MST
               CALL MODELD('GAINS','To-Prog',XGOBA(K),INGLOB,
     *            SGOBA, SOUT, COUT, UOUT)
               END IF
            GO TO 101
            END IF
C                                       short line => skip line
         IF (JKEY .LT. (4 + 2*MLEN)) GO TO 101
C                                       save beginning channel number
         BGOBA = INT (DVALUE(3))
C                                       save ending channel number
         EGOBA = INT (DVALUE(4))
C         WRITE (*,*) 'b/e= ',BGOBA,EGOBA
C                                       if comp outside ch range => skip line
         IF ((BGOBA.NE.0).AND.(BGOBA.GT.ENDCH)) GO TO 101
         IF ((EGOBA.NE.0).AND.(EGOBA.LT.BEGCH)) GO TO 101
C                                       GET SOURCE ID
         SID = 0
         IF (SORCEL(1).GT.0) SID = INT (DVALUE(2))
         IF (SID.GT.0) THEN
C                                       IF SOURCE ID DOES NOT MATCH, SKIP LINE
            DO 131 Z = 1,30
               IF (SID.EQ.SORCEL(Z)) GO TO 132
 131           CONTINUE
            GO TO 101
 132        CONTINUE
         END IF
C                                       line is good, increment models counter
         NCOMP = NCOMP + 1
C                                       save model type
         CGOBA(NCOMP) = MTYPE
C                                       save #scratch pars for this component
         XGOBA(MSCAT+1) = SLEN
C                                       save #model pars for this component
         XGOBA(MSCAT+2) = MLEN
C                                       save user specified SOURCE id number
         XGOBA(MSCAT+3) = DVALUE(2)
C                                       save beginning channel
         XGOBA(MSCAT+4) = DVALUE(3)
C                                       save end channel
         XGOBA(MSCAT+5) = DVALUE(4)
C                                       read model pars for this component
         DO 141 I = 1,MLEN
            INGLOB(I) = DVALUE(4+2*I-1)
            LGOBA(NSCAT+I) = ( CVALUE(4+2*I).NE.'F' )
 141        CONTINUE
         CALL MODELD (CGOBA(NCOMP), 'To-Prog', XGOBA(MSCAT+6), INGLOB,
     *      SGOBA, SOUT, COUT, UOUT)
         NSCAT = NSCAT + XGOBA(MSCAT+2)
         MSCAT = MSCAT + XGOBA(MSCAT+1)
         GO TO 101
 151     CONTINUE
C                                       close infile
      CALL ZTXCLS (NLUN, ILUN, JRET)
C                                       done reading in file
      GO TO 999
C                                       ERROR
 990  CONTINUE
      MSGTXT = '******  OMFILE reports error condition  *****'
      CALL MSGWRT (3)
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('*OMSEZ* Error #',I3,' reading scattering term #',I3)
      END
      SUBROUTINE OMLMM (UVSCR, INFILE, JRET)
C-----------------------------------------------------------------------
      CHARACTER UVSCR*(*), INFILE*(*)
      INTEGER JRET
      INCLUDE 'FIT0.INC'
C
      CHARACTER CG(1024)*8
      LONGINT   OLFLAG, ODOF
      INTEGER   LFLAG(1024), DOF(1024)
      LOGICAL   LLFLAG(1024)
      EQUIVALENCE (LFLAG, LLFLAG)
      LONGINT   OIG, OBG, OEG, ONG, OMG
      REAL      IG(1024), BG(1024), EG(1024), NG(1024), MG(1024)
      LONGINT   OLG, OPG, ODG, OSG, OGMAT, ODDG, ODSG, ODGMAT
      REAL      LG(1024), PG(1024), DG(1024), SG(1024), GMAT(1024)
      LOGICAL   LLG(1024)
      DOUBLE PRECISION DDG(1024), DSG(1024), DGMAT(1024)
      EQUIVALENCE (DDG, DG), (DSG, SG), (GMAT, DGMAT)
      EQUIVALENCE (LG, LLG)
      LONGINT   OXG, OXO, ODXG, ODXO
      REAL      XG(1024), XO(1024)
      DOUBLE PRECISION DXG(1024), DXO(1024)
      EQUIVALENCE (DXG, XG), (DXO, XO)
      LONGINT   OSL, ONVECA, ODSL, ODVECA
      REAL      SL(1024), NVECA(10)
      DOUBLE PRECISION DSL(1024), DNVECA(5)
      EQUIVALENCE (DSL, SL), (DNVECA, NVECA)
C
      INTEGER NMEM, NALLOC
C-----------------------------------------------------------------------
      JRET = 0
      NALLOC = 0
C                                       id per comp
      IF (MCP.GT.1024) THEN
         NMEM = (MCP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, IG, OIG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       beginning channel per comp
         NMEM = (MCP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, BG, OBG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       ending channel per comp
         NMEM = (MCP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, EG, OEG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       number of parameters per comp
         NMEM = (MCP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, NG, ONG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       number of scratch parameters per comp
         NMEM = (MCP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, MG, OMG, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         OIG = 0
         OBG = 0
         OEG = 0
         ONG = 0
         OMG = 0
         END IF
C                                       scratch values per comp
      IF (MXP.GT.512) THEN
         NMEM = (MXP * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, XG, OXG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       scratch values per comp
         NMEM = (MXP * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, XO, OXO, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         OXG = 0
         OXO = 0
         END IF
C                                       flag for each station at each time
      IF (MSP*MTT.GT.1024) THEN
         NMEM = (MSP * MTT - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, LFLAG, OLFLAG, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         OLFLAG = 0
         END IF
C                                       vary flag per parameter
      IF (MGP.GT.1024) THEN
         NMEM = (MGP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, LG, OLG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       value per parameter
         NMEM = (MGP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, PG, OPG, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         OLG = 0
         OPG = 0
         END IF
C                                       correction per parameter
      IF (MGP.GT.512) THEN
         NMEM = (MGP * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, DG, ODG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       variance per parameter
         NMEM = (MGP * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, SG, OSG, JRET)
         IF (JRET.NE.0) GO TO 100
C                                       globals hessian matrix
         NMEM = (MGP*MGP * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, GMAT, OGMAT, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         ODG = 0
         OSG = 0
         OGMAT = 0
         END IF
C                                       locals normalization vector
      IF (5*MCLP.GT.512) THEN
         NMEM = (( 5*MCLP ) * 2 - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, NVECA, ONVECA, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         ONVECA = 0
         END IF
C                                       variance for local params
      IF (MLP.GT.1024) THEN
         NMEM = (MLP - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, SL, OSL, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         OSL = 0
         END IF
C                                       statistics for each time
      IF (MTT.GT.1024) THEN
         NMEM = (MTT - 1) / 1024 + 1
         CALL ZMEMRY ('GET', 'OMLMM', NMEM, DOF, ODOF, JRET)
         IF (JRET.NE.0) GO TO 100
         NALLOC = NALLOC + 1
      ELSE
         ODOF = 0
         END IF
      ODXG = (OXG + 1) / 2
      ODXO = (OXO + 1) / 2
      ODDG = (ODG + 1) / 2
      ODSG = (OSG + 1) / 2
      ODGMAT = (OGMAT + 1) / 2
      ODSL = (OSL + 1) / 2
      ODVECA = (ONVECA + 1) / 2
      CALL OMLOM (UVSCR, INFILE, CG, DXG(1+ODXG), DXO(1+ODXO),
     *   LLFLAG(1+OLFLAG), LLG(1+OLG), DDG(1+ODDG), DSG(1+ODSG),
     *   DGMAT(1+ODGMAT), DSL(1+ODSL), DOF(1+ODOF), DNVECA(1+ODVECA),
     *   JRET)
C
 100  IF (NALLOC.GT.0) CALL ZMEMRY ('FRAL', 'OMLMM', NMEM, LFLAG,
     *   OLFLAG, JRET)
C
      RETURN
      END
      SUBROUTINE OMLOM (UVSCR, INFILE, CGOBA, XGOBA, XOGOBA, TSFLG,
     *   LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF, NORVEC, JRET)
C-----------------------------------------------------------------------
C   OMLMM actually implements the Levenberg-Marquardt procedure
C   Input:
C      UVSCR   C*(*) OOP object name for the scratch file to be fit to
C   Output:
C      JRET    I     Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER UVSCR*(*), INFILE*(*)
      INTEGER JRET
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'FIT0.INC'
C                                       declared dimensions in FIT0.INC
      CHARACTER CGOBA(MCP)*8
      DOUBLE PRECISION XGOBA(MXP), XOGOBA(MXP)
      LOGICAL TSFLG(MSPT)
      LOGICAL LGOBA(MGP)
      DOUBLE PRECISION DGOBA(MGP), SGOBA(MGP), GLOMAT(MGP, MGP),
     *   SLOCA(MCLP), NORVEC(5,MCLP)
      INTEGER DOF(MTT)

      INCLUDE 'LINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'KINFO.INC'
      INCLUDE 'IINFO.INC'
      INCLUDE 'SINFO.INC'
C                                       internal variables
      INTEGER I, SORCEL(30), TYPE, DIM(3), J, IDUM
      CHARACTER CDUM*2, OTFILE*48
      LOGICAL TRUE, FALS, LGLO
      DATA TRUE, FALS /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      ITER = 0
      LAMP = SOLMOD(2:2).EQ.'A'
      CALL DFILL (MXP, 0.0D0, XGOBA)
      CALL LFILL (MGP, FALS, LGOBA)
C
C                                       read in control parameters for
C                                       real this time
      IF (INFILE.EQ.'no valid file, used default model') THEN
         CALL OMKEY (XGOBA, CGOBA, LGOBA, JRET)
      ELSE
         CALL OGET (UVSCR, 'SORCEL', TYPE, DIM, SORCEL, CDUM, JRET)
         IF (JRET.NE.0) GO TO 990
         CALL OMKEZ (INFILE, SORCEL, XGOBA, CGOBA, LGOBA, JRET)
         END IF
      IF (JRET.NE.0) GO TO 990
C         WRITE (*,*) XGOBA
C
      IF (PRTLEV.GT.0) THEN
         WRITE (MSGTXT,1500)
         CALL MSGWRT (3)
         WRITE (MSGTXT,1510) NITER
         CALL MSGWRT (3)
         IF (NGITER.GT.0) THEN
            WRITE (MSGTXT,1520) NGITER
            CALL MSGWRT (3)
         END IF
         WRITE (MSGTXT,1530) MCP
         CALL MSGWRT (3)
         MSGTXT = '-------- Data Description follows ---------------'
         CALL MSGWRT (3)
      END IF
C
C            OMLOOP ( LBEG, LEND, LRES, LCALA, LGLO, LAMP, LPHI, ...
C                                       LBEG is true only on the first
C                                        invocation of OMLOOP
C                                       LEND is true only on the last
C                                        invocation of OMLOOP
C                                       LRES is true only at the end
C                                        of each iteration
C                                       LCALA is true only on the first
C                                        pass of each iteration
C                                       LGLO is true only when the global
C                                        parameters are being solved for
C                                       LAMP is true only when the local
C                                        amplitudes are being solved for
C                                       LPHI is true only when the local
C                                        phases are being solved for
C                                       initialize data set statistics and
C                                       and load local parameter values
      CALL OMLOOP (UVSCR, TRUE,FALS,FALS,FALS, FALS,FALS,FALS,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C
C                                       make backup copy of global parameters
      CALL DCOPY (MXP, XGOBA, 1, XOGOBA, 1)

C                                       show current model
      LAMP = SOLMOD(2:2).EQ.'A'
      LPHI = SOLMOD(3:3).EQ.'P'
      IF (PRTLEV.GT.0) THEN
         IF (PRTLEV.GT.1) THEN
            WRITE (MSGTXT,1700)
            CALL MSGWRT (3)
            IF (LAMP.OR.LPHI) THEN
               WRITE (MSGTXT,1720) SOLMOD
               CALL MSGWRT (3)
               DO 190 J = 1, MSP
                  IF (LAMP.AND.LPHI) THEN
                     WRITE (MSGTXT,1730) ANTNAM(J)
                  ELSE IF (LAMP) THEN
                     WRITE (MSGTXT,1740) ANTNAM(J)
                  ELSE IF (LPHI) THEN
                     WRITE (MSGTXT,1750) ANTNAM(J)
                  ELSE
                     WRITE (MSGTXT,1760) ANTNAM(J)
                  END IF
                  CALL MSGWRT (3)
 190           CONTINUE
            ELSE
               WRITE (MSGTXT,1770)
               CALL MSGWRT (3)
            END IF
         END IF
         CALL TOSCRN (UVSCR, NORVEC, CGOBA, XGOBA, XGOBA(MXP-MEP),
     *      SGOBA, SGOBA(MGP-MEP+1), LGOBA, LGOBA(MGP-MEP+1))
      END IF
C
 100  CONTINUE
C
         ITER = ITER + 1
         LPHI =  (NPIBEG.LE.ITER).AND.(ITER.LE.NPIEND)
     *            .AND.(SOLMOD(3:3).EQ.'P')
         LAMP =  (NAIBEG.LE.ITER).AND.(ITER.LE.NAIEND)
     *            .AND.(SOLMOD(2:2).EQ.'A')
         LGLO = (NGIBEG.LE.ITER).AND.(ITER.LE.NGIEND)

         IF (PRTLEV.GT.1) THEN
            WRITE (MSGTXT,1809)
            CALL MSGWRT (3)
            WRITE (MSGTXT,1810) ITER, NITER
            CALL MSGWRT (3)
            IF (PRTLEV.GT.3) THEN
               MSGTXT = '---- Stage 1: Compute Corrections'
               CALL MSGWRT (3)
            END IF
         END IF
C                                       compute global corrections
         CALL OMLOOP (UVSCR, FALS,FALS,FALS,TRUE, LGLO,LAMP,LPHI,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C
         IF (PRTLEV.GT.3) THEN
            MSGTXT = '---- Stage 2: Apply Corrections'
            CALL MSGWRT (3)
            END IF
C                                       finish computing solns and apply them
         CALL OMLOOP (UVSCR, FALS,FALS,FALS,FALS, LGLO,LAMP,LPHI,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C
         IF (PRTLEV.GT.3) THEN
            MSGTXT = '---- Stage 3: Compute Residuals'
            CALL MSGWRT (3)
            END IF
C
         CALL OMLOOP (UVSCR, FALS,FALS,TRUE,FALS, FALS,FALS,FALS,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C
C                                            PrevRedChiSq - CurRedChiSq
C                                   DELRMS = -------------------------- x 100
C                                                    PrevRedChiSq
C
C                                       WAS THIS A GOOD STEP?
         IF (DELRMS.GT.0.0D0) THEN
C                                       save copy of globals
            CALL DCOPY (MXP, XGOBA, 1, XOGOBA, 1)
C                                       do nothing to locals
            IF (LAMP.OR.LPHI) THEN
               OLDPLS = OLDPLS
               NEWPLS = NEWPLS
               END IF
C                                       make covariances more important
            LGAMMA = LGAMMA - 3.0 + 1.0D0
C                                       prevent underflow in GAMMA
            LGAMMA = MAX(LGAMMA, -40.0D0)
            GAMMA = 2**LGAMMA
C                                       print out solns to screen
            IF (PRTLEV.GT.0) THEN
               CALL TOSCRN (UVSCR, NORVEC, CGOBA, XGOBA, XGOBA(MXP-MEP),
     *            SGOBA, SGOBA(MGP-MEP+1), LGOBA, LGOBA(MGP-MEP+1))
               END IF
         ELSE
C                                       recover copy of globals
            CALL DCOPY (MXP, XOGOBA, 1, XGOBA, 1)
C                                       recover copy of locals
            IF (LAMP.OR.LPHI) THEN
               I = OLDPLS
               OLDPLS = NEWPLS
               NEWPLS = I
               END IF
C                                       make covariances less important
            LGAMMA = LGAMMA + 3.0
C                                       prevent overflow in GAMMA
            LGAMMA = MIN ( LGAMMA, 40.0D0 )
            GAMMA = 2**LGAMMA
            END IF
C                                       FULL STOP if we reach NITER
      IF (ITER.GE.NITER) GO TO 101
C                                       early STOP if:
C                                       good step, parameters not changing
C                                         and Chi-Squared not changing
      IF (((GNFRAC.LE.MLIMIT).AND.(ABS(DELRMS).LT.C2FRAC)) .OR.
     *    ((C2FRAC.LE.MLIMIT).AND.(GNORM.LT.GNFRAC))       .OR.
     *   ((GNORM.LT.GNFRAC).AND.(ABS(DELRMS).LT.C2FRAC))    )
     *      GO TO 101
C       Note that DELRMS above and C2FRAC are both in percent!
C       and GNORM and GNFRAC are not (I think) MFB
C                                       GO AROUND AGAIN
      GO TO 100
 101  CONTINUE
C                                       save status of current run
      IF (PRTLEV.GT.3) THEN
         CALL OGET (UVSCR, 'OUTFILE', TYPE, DIM, IDUM, OTFILE, JRET)
         IF ((JRET.EQ.0).AND.(OTFILE(1:1).NE.' ')) THEN
            WRITE (MSGTXT,2550) OTFILE
            CALL MSGWRT (3)
         END IF
      END IF
C                                       Here, if one was so inclined,
C                                       the new global parameter values
C                                       would be saved to an output file
C
C                                       for Stage-4
      IF (PRTLEV.GT.3) THEN
         MSGTXT = '---- Stage 4: Compute Formal Error Bars'
         CALL MSGWRT (3)
         END IF
C                                       one extra iteration to compute proper
C                                       error bars
      LPHI = SOLMOD(3:3).EQ.'P'
      LAMP = SOLMOD(2:2).EQ.'A'
      LGLO = TRUE
      GAMMA = 0.0D0
C     CALL OMLOOP (UVSCR, FALS,FALS,FALS,TRUE, LGLO,LAMP,LPHI,
      CALL OMLOOP (UVSCR, FALS,TRUE,FALS,TRUE, LGLO,LAMP,LPHI,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C                                       save new local parameters to SN table
C                                       if so requested, delete scratch files
      CALL OMLOOP (UVSCR, FALS,TRUE,FALS,FALS, LGLO,LAMP,LPHI,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C                                       summarize the final model
      PRTLEV = PRTLEV + 1
      LITER = .FALSE.
      IF (PRTLEV.GT.0) THEN
         CALL TOSCRN (UVSCR, NORVEC, CGOBA, XGOBA, XGOBA(MXP-MEP),
     *      SGOBA, SGOBA(MGP-MEP+1), LGOBA, LGOBA(MGP-MEP+1))
         END IF
      IF (LPRNT) CALL ZTXCLS (PHILE, IPHILE, I)
      GO TO 999
C                                       error trap
 990  CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('--------- Model Description follows ---------------')
 1510 FORMAT (' Run ',I4,' iterations')
 1520 FORMAT (' Hold global parameters until iteration #',I3)
 1530 FORMAT (' Use ',I6,' components')
 1700 FORMAT ('------ Summarize starting model parameters ------')
 1720 FORMAT (' Solve for T-VARIABLE PARAMS using SOLMOD = ',A3)
 1730 FORMAT (' Solve for ',A8,' amplitude and phase')
 1740 FORMAT (' Solve for ',A8,' amplitude only')
 1750 FORMAT (' Solve for ',A8,' phase only')
 1760 FORMAT ('      Hold ',A8,' amplitude and phase')
 1770 FORMAT (' No Self-Cal, all variables assumed time-independent.')
 1809 FORMAT ('=================================================')
 1810 FORMAT ('         ITERATION # ',I3,' of ',I3,
     *        ' Begins           ')
 2550 FORMAT (5X,'Save state to ',A48)
      END
      SUBROUTINE OMLOOP (UVSCR, LBEG,LEND,LRES,LCALA, LGLO,LA,LP,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT, SLOCA, DOF,
     *   NORVEC)
C-----------------------------------------------------------------------
C   OMLOOP merely activates dynamic memory allocation
C-----------------------------------------------------------------------
      CHARACTER UVSCR*(*)
      LOGICAL LBEG,LGLO,LCALA,LRES,LEND,LA,LP
C                                       includes
      INCLUDE 'FIT0.INC'
C                                       these dimensions declared in FIT0.INC
      CHARACTER CGOBA(MCP)*8
      DOUBLE PRECISION XGOBA(MXP)
      LOGICAL TSFLG(MSPT)
      LOGICAL LGOBA(MGP)
      DOUBLE PRECISION DGOBA(MGP), SGOBA(MGP), GLOMAT(MGP, MGP),
     *   SLOCA(MCLP), NORVEC(5,MCLP)
      INTEGER DOF(MTT)
C
      LONGINT   OVECT, OARRA, OCGRAD, ODVECT, ODARRA
      REAL      VECT(2059), ARRA(2059), CGRAD(2059)
      DOUBLE PRECISION DVECT(2059), DARRA(2059)
      EQUIVALENCE (DVECT, VECT), (DARRA, ARRA)
      LONGINT   OSSTA  , OGSTA  , ORGRAD, OCRGRD
      REAL      SSTA(2059), GSTA(2059), RGRAD(2059)
      INTEGER   ISSTA(2059)
      LOGICAL   LGSTA(2059)
      COMPLEX   CRGRAD(2059)
      EQUIVALENCE (SSTA, ISSTA), (LGSTA, GSTA), (CRGRAD, RGRAD)
      LONGINT   OGVEC  , OGCON, ODGVEC
      REAL      GVEC(2059), GCON(2059)
      DOUBLE PRECISION DGVEC(2059)
      EQUIVALENCE (GVEC, DGVEC)
      LONGINT   OLMAT, OLVEC, OLCON, OLPAR, ODLMAT, ODLVEC, OCLPAR
      REAL      LMAT(2059), LVEC(2059), LCON(2059), LPAR(2059)
      DOUBLE PRECISION DLMAT(2059), DLVEC(2059)
      COMPLEX CLPAR(2059)
      LOGICAL   LLCON(2059)
      EQUIVALENCE (LMAT, DLMAT), (LVEC, DLVEC)
      EQUIVALENCE (LLCON, LCON), (CLPAR, LPAR)
      LONGINT   ODENL, ODENC, ODENR, OITO, OIFROM, ODDENL, ODDENC,
     *   ODDENR
      REAL      DENL(2059), DENC(2059), DENR(2059), ITO(2059),
     *   IFROM(2059)
      DOUBLE PRECISION DDENL(2059), DDENC(2059), DDENR(2059)
      INTEGER   IITO(2059), IIFROM(2059)
      EQUIVALENCE (DDENL, DENL), (DDENC, DENC), (DDENR, DENR)
      EQUIVALENCE (IITO, ITO), (IIFROM, IFROM)
      LONGINT   OWORK, ODWORK
      REAL      WORK(2059)
      DOUBLE PRECISION DWORK(2059)
      EQUIVALENCE (DWORK, WORK)
      LONGINT   ORMODC, ORMDTC, OCMODC, OCMDTC
      REAL      RMODC(2059), RMDTC(2059)
      COMPLEX   CMODC(2059), CMDTC(2059)
      EQUIVALENCE (CMODC, RMODC), (RMDTC, CMDTC)
C
      INTEGER   NMEM, JRET, NALLOC
C-----------------------------------------------------------------------
      JRET = 0
      NALLOC = 0
C                                       make all memories a bit large
C                                       offset the address into them
C                                       avoids some off by 1 issue
      IF (MSP.GT.1) THEN
C                                       baseline statistics matrix
         NMEM = (MSP*MSP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OSSTA = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, SSTA, OSSTA, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       baseline statistics matrix
         NMEM = (MSP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OGSTA = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, GSTA, OGSTA, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OSSTA = 0
         OGSTA = 0
         END IF
C
      IF (MGP.GT.1) THEN
C                                       globals gradient vector
         NMEM = (MGP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OGVEC = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, GVEC, OGVEC, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OGVEC = 0
         END IF
C
      IF (MGP.GT.2) THEN
C                                       globals vary flag vector
         NMEM = (MGP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OGCON = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, GCON, OGCON, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OGCON = 0
         END IF
C
      IF (MCLP.GT.0) THEN
C                                       locals hessian matrix
         NMEM = (MLP*MLP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OLMAT = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, LMAT, OLMAT, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       locals gradient vector
         NMEM = (MLP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OLVEC = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, LVEC, OLVEC, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OLMAT = 0
         OLVEC = 0
         END IF
C
      IF (MCLP.GT.1) THEN
C                                       locals vary flag vector
         NMEM = (MLP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OLCON = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, LCON, OLCON, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       locals parameter vector
         NMEM = (MLP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OLPAR = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, LPAR, OLPAR, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OLCON = 0
         OLPAR = 0
         END IF
C
      IF (MTP.GT.1) THEN
C                                       Gradient accumulator
         NMEM = (MTP*2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OVECT = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, VECT, OVECT, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       Hessian accumulator
         NMEM = (MTP*MTP*2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OARRA = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, ARRA, OARRA, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OVECT = 0
         OARRA = 0
         END IF
C
      IF (MMP.GT.1) THEN
C                                       left orthogonal matrix
C                                       for SVD
         NMEM = (MMP*MMP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ODENL = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, DENL, ODENL, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       diagonal singular value
C                                       vector for SVD
         NMEM = (MMP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ODENC = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, DENC, ODENC, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       right orthogonal matrix
C                                       for SVD
         NMEM = (MMP*MMP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ODENR = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, DENR, ODENR, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         ODENL = 0
         ODENC = 0
         ODENR = 0
         END IF
C
      IF (MMP.GT.0) THEN
C                                       work vector for SVD
         NMEM = (10*MMP * 2 - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OWORK = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, WORK, OWORK, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OWORK = 0
         END IF
C
      IF (MMP.GT.2) THEN
C                                       work vector for matrix
C                                       unpacking
         NMEM = (MMP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OIFROM = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, IFROM, OIFROM, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       work vector for matrix
C                                       packing
         NMEM = (MMP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OITO = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, ITO, OITO, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OIFROM = 0
         OITO = 0
         END IF
C
      IF (MSP.GT.0) THEN
C                                       these are used inside cru_nch:mod_elm
C                                       gradient pieces for hessian calc.
         NMEM = (2*2*MSP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ORGRAD = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, RGRAD, ORGRAD, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         ORGRAD = 0
         END IF
C
      IF (MTP.GT.1) THEN
C                                       gradient pieces
         NMEM = (2*MTP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            OCGRAD = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, CGRAD, OCGRAD, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         OCGRAD = 0
         END IF
C
      IF (MCP.GT.1) THEN
C                                       model pieces for Hessian calc.
         NMEM = (2*MCP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ORMODC = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, RMODC, ORMODC, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
C                                       model pieces for Hessian calc.
         NMEM = (2*MCP - 1) / 1024 + 10
         IF (NMEM.LT.12) THEN
            ORMDTC = 0
         ELSE
            CALL ZMEMRY ('GET', 'OMLOOP', NMEM, RMDTC, ORMDTC, JRET)
            IF (JRET.NE.0) GO TO 100
            NALLOC = NALLOC + 1
            END IF
      ELSE
         ORMODC = 0
         ORMDTC = 0
         END IF
      OCMODC = (ORMODC +1) / 2
      OCMDTC = (ORMDTC +1) / 2
      OCRGRD = (ORGRAD +1) / 2
      ODGVEC = (OGVEC  +1) / 2
      ODLMAT = (OLMAT  +1) / 2
      ODLVEC = (OLVEC  +1) / 2
      OCLPAR = (OLPAR  +1) / 2
      ODVECT = (OVECT +1) / 2
      ODARRA = (OARRA +1) / 2
      ODDENL = (ODENL +1) / 2
      ODDENC = (ODENC +1) / 2
      ODDENR = (ODENR +1) / 2
      ODWORK = (OWORK +1) / 2
      CALL OMPROC (UVSCR, LBEG, LEND, LRES, LCALA, LGLO, LA, LP,
     *   CMODC(10+OCMODC), CMDTC(10+OCMDTC), ISSTA(10+OSSTA),
     *   LGSTA(10+OGSTA), CRGRAD(10+OCRGRD), DGVEC(10+ODGVEC),
     *   DLMAT(10+ODLMAT), DLVEC(10+ODLVEC),
     *   LLCON(10+OLCON), LLCON(10+OLCON), CLPAR(10+OCLPAR),
     *   DVECT(10+ODVECT), DARRA(10+ODARRA), CGRAD(10+OCGRAD),
     *   DDENL(10+ODDENL), DDENC(10+ODDENC), DDENR(10+ODDENR),
     *   DWORK(10+ODWORK), IITO(10+OITO), IIFROM(10+OIFROM),
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT,
     *   SLOCA, DOF, NORVEC)
 100  CONTINUE
C                                       release memory here
      IF (NALLOC.GT.0) CALL ZMEMRY ('FRAL', 'OMLOOP', NMEM, ARRA, OARRA,
     *   JRET)
      RETURN
      END
      SUBROUTINE OMPROC (UVSCR, LBEG,LEND,LRES,LCALA, LGLO,LA,LP,
     *   MODC, MODTC, SSTATS,
     *   KEPTST, LGRADC, GLOVEC,
     *   LOCMAT, LOCVEC,
     *   LOCCON, LOCMON, LOCPAR,
     *   VECTOR, ARRAY, GRADC,
     *   DENTL, DENTC, DENTR,
     *   WORK, ITO, IFROM,
     *   CGOBA, XGOBA, TSFLG, LGOBA, DGOBA, SGOBA, GLOMAT,
     *   SLOCA, DOF, NORVEC)
C-----------------------------------------------------------------------
C   OMLOOP does the dirty work of sloughing through the data computing
C   corrections, and evaluating Chi-Square
C   Input:
C             1   examine statistics of data set and characterise it
C             2   calculate only global corrections
C             3   calculate global corrections including local effects
C             4   calculate local corrections including global effects
C             5   calculate Chi-Square for new set of model parameters
C             10  calc<ulate proper error bars for all model parameters
C      UVSCR   C*(*) OOP object name for scratch UV data file
C   Output:
C      only via commons
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER UVSCR*(*)
      LOGICAL LBEG,LGLO,LCALA,LRES,LEND,LA,LP

C                                       these are due to dynamic
C                                       memory allocation - see
C                                       above for descriptions
C                                       these dimensions declared in FIT0.INC
      INCLUDE 'FIT0.INC'
      COMPLEX MODC(MCP), MODTC(MCP)
      INTEGER SSTATS(MSP,MSP)
      LOGICAL KEPTST(MSP)
      COMPLEX LGRADC(2*MSP)
      DOUBLE PRECISION GLOMAT(MGP, MGP)
      DOUBLE PRECISION GLOVEC(MGP)
      DOUBLE PRECISION LOCMAT(MLP,MLP)
      DOUBLE PRECISION LOCVEC(MLP)
      LOGICAL LOCCON(MLP)
      LOGICAL LOCMON(2,MST,NIF,MSP,MSC)
      COMPLEX LOCPAR(MCLP)
      DOUBLE PRECISION NORVEC(5,MCLP)
      REAL    GRADC(MTP)
      DOUBLE PRECISION VECTOR(MTP), ARRAY(MTP,MTP)
      DOUBLE PRECISION DENTL(MMP,MMP), DENTC(MMP), DENTR(MMP,MMP)
      DOUBLE PRECISION WORK(10*MMP)
      INTEGER   ITO(MMP), IFROM(MMP)
      CHARACTER CGOBA(MCP)*8
      DOUBLE PRECISION XGOBA(MXP)
      LOGICAL TSFLG(MSPT)
      LOGICAL LGOBA(MGP)
      DOUBLE PRECISION DGOBA(MGP), SGOBA(MGP), SLOCA(MCLP)
      INTEGER DOF(MTT)
C                                       includes
      INCLUDE 'LINFO.INC'
      INCLUDE 'KINFO.INC'
      INCLUDE 'IINFO.INC'
      INCLUDE 'SINFO.INC'
C                                       internal variables
      INTEGER IPPAR, IPOBS, JRET, LDOF
      INTEGER TKEEP, TLOSE, TOSSTA
      LOGICAL GETGRD, LCAL, LSMP, LSTAT, WRITOT
      INTEGER NEXTN, LASTN, LPRNOW(2)
      INTEGER I, J, K, L, M, N, NEXP
      REAL      RRCOR, RICOR, AACOR
      DOUBLE PRECISION MEDK, DRCOR, DICOR
      DOUBLE PRECISION MEDKL, MEDIK, TCHI2
      DOUBLE PRECISION DZERO, T2CHI2
      INTEGER TSTMP
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL TRUE, FALSE
      COMMON /SSTA/ TKEEP, TLOSE, TOSSTA
      DATA TRUE, FALSE /.TRUE., .FALSE./
      DATA DZERO       /0.0D0 /
C-----------------------------------------------------------------------
C                                     solve for any local parameters?
      LCAL  = LA .OR. LP
C                                     calculate statistics?
      LSTAT = LRES.OR.LEND
C                                     calculate gradients?
      GETGRD = (LGLO.AND.LCALA) .OR. LCAL
C
      LSMP = GETGRD .OR. LSTAT
C                                     print out residuals?
      IF (LPRNT.AND.LEND) THEN
         LPRNOW(1) = PHILE
         LPRNOW(2) = IPHILE
      ELSE
         LPRNOW(1) = 0
         END IF
      WRITOT = WRITER.AND.LEND
      IF (LBEG) THEN
         SUMGP = 0.0D0
C                                       add in global parameters right now!
         DO 10 I = 1,MGP
            IF (LGOBA(I)) SUMGP = SUMGP + 1.0D0
 10         CONTINUE
C                                     subtract one antennas worth of parameters
C                                     if gains turned on.
         IF (MEP.GT.0) SUMGP = SUMGP - NIF*MST
C                                       init all error estimates
         CALL DFILL (MGP, DZERO, SGOBA)
         CALL DFILL (MGP, DZERO, DGOBA)
C                                       update ancillary quantities
         J = 1
         K = 1
         DO 20 I = 1,MCP
            CALL MODELS (CGOBA(I), XGOBA(K), DGOBA(J))
            J = J + XGOBA(K+1)
            K = K + XGOBA(K)
 20      CONTINUE
C                                       update station gains
         IF (MEP.GT.0) CALL MODELS ('GAINS', XGOBA(K), DGOBA(J))
C                                       set all toss flags as true
         CALL LFILL (MSPT, TRUE, TSFLG)
C                                       init all times as good
         CALL FILL (MTT, -1, DOF)
         TOSSTA = 0
         SUMLP = 0.0D0
         CALL DFILL (MCLP, DZERO, SLOCA)
      END IF
C
      IF (LCALA.AND.LGLO) THEN
C                                       initialize matrices for globals
         CALL DFILL (MGP, DZERO, GLOVEC)
         CALL DFILL (MGP*MGP, DZERO, GLOMAT)
      END IF
      IF (LSTAT) THEN
C                                       init statistics for local params
         CALL DFILL (5*MCLP, DZERO, NORVEC)
         END IF
C                                       pre-loop here
      IF (LCALA) THEN
         SUMLPS = DZERO
         END IF
      IF (LSMP) THEN
         REWIND (OLDPLS)
         REWIND (NEWPLS)
         SUMSQR = DZERO
         SUMCOR = DZERO
         SUMWGT = DZERO
         SUMWT2 = DZERO
         NEXTN = 0
         LASTN = 0
         END IF
      IF (LBEG.OR.LEND.OR.LRES.OR.GETGRD) THEN
         CALL CRUNCH (GETGRD, LBEG, WRITOT, NEXTN, LASTN, LPRNOW,
     *        UVSCR, LA, LP, LSTAT, MODC, MODTC, SSTATS,
     *        TSFLG, KEPTST, LGRADC, LOCPAR, NORVEC,
     *        VECTOR, ARRAY, GRADC, CGOBA, XGOBA, XGOBA(MXP-MEP+1),
     *        LGOBA, DOF, JRET)
         IF (JRET.GT.0) GO TO 990
         END IF
C                                     loop back to here
 100  CONTINUE
C                                     init VECTOR, ARRAY, and KEPTST
         CALL DFILL (MTP, DZERO, VECTOR)
         CALL DFILL (MTP*MTP, DZERO, ARRAY)
         CALL LFILL (MSP, FALSE, KEPTST)

         IF (LSMP) THEN
            IF (.NOT.SIMLOC) THEN
C                                     load local parameters
               TSTMP = TSMIN + NEXTN
               IF (MSC.GT.0)
     *           CALL SGET ('READ', OLDPLS, TSTMP, MCLP, LOCPAR, SLOCA)
C                                     save locals error estimates
               DO 110 J=1,MCLP
C                  RTEMP = CABS(LOCPAR(J))**2
C                  IF (RTEMP.GT.0.00001) THEN
C                     NORVEC(4,J) = NORVEC(4,J) + SLOCA(J)/RTEMP
                     NORVEC(4,J) = NORVEC(4,J) + SLOCA(J)
                     NORVEC(5,J) = NORVEC(5,J) + 1.0D0
C                  END IF
 110           CONTINUE
            END IF
         END IF
C                                     process data
         IF (LBEG.OR.LEND.OR.LRES.OR.GETGRD) THEN
            I = MSP * (NEXTN-1)
            CALL CRUNCH (GETGRD, LBEG, WRITOT, NEXTN, LASTN, LPRNOW,
     *           UVSCR, LA, LP, LSTAT, MODC, MODTC, SSTATS,
     *           TSFLG(I+1), KEPTST, LGRADC, LOCPAR, NORVEC,
     *           VECTOR, ARRAY, GRADC, CGOBA, XGOBA, XGOBA(MXP-MEP+1),
     *           LGOBA, DOF, JRET)
            IF (JRET.GT.0) GO TO 990
C
C                                       LASTN is the timestamp
C                                       just processed
C                                       NEXTN is the next timestamp
C                                       to be processed
C                                       on first pass, get some statistics
            IF (LBEG) THEN
C                                       compute IPPAR, IPOBS
               CALL STSTAT (BLSEL, MSP, SSTATS, IPPAR, IPOBS)
C                                       compute degrees of freedom for
C                                          this time
               DOF(LASTN) = 2*IPOBS*OVRLAP-NSCAL*MST*NIF*MSC*(IPPAR-1)
C               WRITE (*,*) IPOBS,OVRLAP,NSCAL,MST,NIF,MSC,IPPAR,LASTN
C                                       Have data AND enough data ?
               IF ( (IPOBS.GT.0) .AND. (DOF(LASTN).GE.0) ) THEN
C                                       this timestamp is good
C                                       IPOBS is the number of correlations
C                                       NSCAL is the number of self-cal
C                                            parameters per MSC
C                                       MSC is the number of of self-cal
C                                            parameters per source per IPPAR
C                                       IPPAR is the number of good antennas
C
                  SUMLP = SUMLP + NSCAL*MST*NIF*MSC*(IPPAR-1)
C                                       flag, count, toss stations with no data
                  I = MSP * (LASTN-1)
                  DO 120 J = 1, MSP
                     TSFLG(I+J) = SSTATS(J,J).LE.0
                     IF (TSFLG(I+J)) TOSSTA = TOSSTA + 1
 120              CONTINUE
               ELSE
C                                     count/mark/report tossed stations
                  TOSSTA = TOSSTA + MSP
                  DOF(LASTN) = -1
                  IF (PRTLEV.GT.0) THEN
                     WRITE (MSGTXT,1200) IPOBS, IPPAR, LASTN
                     WRITE (MSGTXT,1600)
                     WRITE (MSGTXT,1610) BLSEL
                  END IF
               END IF
            END IF
         END IF
C
         IF (LCAL) THEN
C                                     determine which antennas to solve for
C                                     [this preps LOCCON]
            DO 160 I = 1,MSC
               DO 150 J = 1, MSP
                  DO 140 K = 1, NIF
                     DO 130 L = 1, MST
                        LOCMON(1,L,K,J,I) = LA .AND. KEPTST(J)
                        LOCMON(2,L,K,J,I) = LP .AND. KEPTST(J)
 130                 CONTINUE
 140              CONTINUE
 150           CONTINUE
 160        CONTINUE
C                                     rescale global/local submatrix
            DO 180 I = 1, MLP
               IF (.NOT.LOCCON(I)) THEN
                  DO 170 J = 1, MGP
                     ARRAY(J,MGP+I) = DZERO
 170              CONTINUE
                  VECTOR(MGP+I) = DZERO
               END IF
 180        CONTINUE
C
            IF (PRTLEV.GT.4) THEN
               MSGTXT = '  Compress/Invert local submatrix/vector'
               CALL MSGWRT (3)
            END IF
            DO 142 I = 1, MLP
               DO 141 J = I, MLP
                  LOCMAT(I,J) = ARRAY(MGP+I,MGP+J)
 141           CONTINUE
               LOCVEC(I) = VECTOR(MGP+I)
 142        CONTINUE
            NEXP = 0
C            NEXP = NIF*MST*MSC*NSCAL
C            WRITE (*,*) 'varb- ',LOCVEC,LOCCON,LOCMAT
            CALL MATINV (MLP, LOCMAT, LOCVEC, LOCCON, MMP,
     *           NEXP, DENTL, DENTC, DENTR, WORK, ITO, IFROM,
     *           CUTOF, GAMMA)
C                                     track the number of singular self-cal
C                                     parameters [minus those expected]
            IF (LCALA) SUMLPS = SUMLPS + NEXP
            LDOF = DOF(LASTN) + NEXP
            IF (.NOT.(LCALA.OR.LEND)) THEN
C               WRITE (*,*) 'vara- ',LOCVEC,LOCCON,LOCMAT
               IF (PRTLEV.GT.4) THEN
                  MSGTXT = '  Adjust local parameters'
                  CALL MSGWRT (3)
               END IF
C                                     compute/apply locals corrections
               DO 250 J = 1, MCLP
                  I = 2*J
                  DRCOR = DZERO
                  DO 240 K = 1,MLP
                     MEDK = LOCVEC(I-1)
                     DO 230 L = 1,MGP
                        MEDK = MEDK - DGOBA(L) * ARRAY(L,MGP+K)
 230                 CONTINUE
                     DRCOR = DRCOR + LOCMAT(I-1,K) * MEDK
 240              CONTINUE
                  DICOR = DZERO
                  DO 242 K = 1,MLP
                     MEDK = LOCVEC(I)
                     DO 241 L = 1,MGP
                        MEDK = MEDK - DGOBA(L) * ARRAY(L,MGP+K)
 241                 CONTINUE
                     DICOR = DICOR + LOCMAT(I,K)   * MEDK
C                  WRITE (*,*) 'a',J,K,LOCVEC(I-1),LOCVEC(I),
C     *                    LOCMAT(I-1,K),LOCMAT(I,K)
 242              CONTINUE
                  RRCOR = DRCOR + 1.0D0
                  RICOR = DICOR
C                  IF (RRCOR.NE.0.) WRITE (*,*) 'de- ',J,RRCOR,RICOR
C  MFB alteration to make this all work for phase-only self/cal
                  IF (.NOT. LA) THEN
                     AACOR = SQRT (RRCOR**2 + RICOR**2)
                     RRCOR = RRCOR / AACOR
                     RICOR = RICOR / AACOR
                     END IF
C  end MFB
                  LOCPAR(J) = LOCPAR(J)*CMPLX(RRCOR,RICOR)
 250           CONTINUE
               IF (LDOF.LE.0) THEN
                  DO 260 J = 1, MCLP
                     IF (LDOF.LT.0) SLOCA(J) = -0.5
                     IF (LDOF.EQ.0) SLOCA(J) = 0.0
 260              CONTINUE
               ELSE
                  IF (PRTLEV.GT.4) THEN
                     MSGTXT = '  Compute local variances'
                     CALL MSGWRT (3)
                  END IF
                  DO 350 J = 1,MCLP
                     I = 2*J
                     SLOCA(J) = (LOCMAT(I,I)+LOCMAT(I-1,I-1))
                     DO 340 K = 1,MGP
                        DO 330 L = 1,MGP
                           MEDKL = 0.0D0
                           DO 320 M = 1, MLP
                              DO 310 N = 1, MLP
                                 MEDKL = MEDKL +
     *                                ARRAY(K,MGP+M) * LOCMAT(M,I) *
     *                                LOCMAT(I,N) * ARRAY(L,MGP+N)
                                 MEDKL = MEDKL +
     *                                ARRAY(K,MGP+M) * LOCMAT(M,I-1)*
     *                                LOCMAT(I-1,N) * ARRAY(L,MGP+N)
 310                          CONTINUE
 320                       CONTINUE
                           SLOCA(J) = SLOCA(J) + GLOMAT(K,L) * MEDKL
 330                    CONTINUE
 340                 CONTINUE
C                                     simulate gamma=0 error-bars
                     SLOCA(J) = SLOCA(J) * (1.0D0 + GAMMA)
                     SLOCA(J) = SLOCA(J) * CHI2 / LDOF + 5.0D-10
 350              CONTINUE
               END IF
C                                     save locals solns and variances
               IF (.NOT. SIMLOC) THEN
                  TSTMP = TSMIN + LASTN
                  IF (MSC.GT.0)
     *                 CALL SGET ('WRITE', NEWPLS, TSTMP, MCLP, LOCPAR,
     *                 SLOCA)
               END IF
            END IF
         END IF
         IF (LCALA.AND.LGLO) THEN
            IF (PRTLEV.GT.4) THEN
               MSGTXT = '  Adjust global submatrix and subvector'
               CALL MSGWRT (3)
            END IF
C   GLOMAT(I,J) += ARRAY(I,J) - ARRAY(I,MGP+L)*LOCMAT(L,K) * ARRAY(MGP+K,J)
C   GLOVEC(J)   += VECTOR(J)  - ARRAY(I,MGP+L)*LOCMAT(L,K) * VECTOR(MGP+K)
            DO 370 I = 1,MGP
               GLOVEC(I)      = GLOVEC(I)   + VECTOR(I)
C               IF (GLOVEC(I).NE.0.) WRITE (*,*) 'gv - ',I,GLOVEC(I)
               DO 360 J = I,MGP
                  GLOMAT(I,J) = GLOMAT(I,J) + ARRAY(I,J)
C         IF (GLOMAT(I,J).NE.0.) WRITE (*,*) 'gm - ',I,J,GLOMAT(I,J)
 360           CONTINUE
 370        CONTINUE
            DO 450 I = 1, MGP
               DO 440 K = 1, MLP
                  MEDIK = 0.0D0
                  DO 420 L = 1, MLP
                     MEDIK = ARRAY(I,MGP+L) * LOCMAT(L,K)
 420              CONTINUE
                  GLOVEC(I)      = GLOVEC(I)   - MEDIK*VECTOR(MGP+K)
                  GLOMAT(I,I)    = GLOMAT(I,I) - MEDIK*ARRAY(I,MGP+K)
                  MEDIK = MEDIK * (1.0D0 + GAMMA)
                  DO 430 J = I+1,MGP
                     GLOMAT(I,J) = GLOMAT(I,J) - MEDIK*ARRAY(J,MGP+K)
 430              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
      IF (LBEG.OR.LEND.OR.LRES.OR.GETGRD) THEN
C                                       end loop here
         IF (JRET.EQ.0) GO TO 100
         END IF
      IF (LBEG) THEN
         DO 510 I = 1, MTT
            IF (DOF(I).GE.0) TKEEP = TKEEP + 1
 510        CONTINUE
         TLOSE = MTT - TKEEP
         IF (PRTLEV.GT.0) THEN
            WRITE (MSGTXT,1661) TKEEP
            CALL MSGWRT (3)
            IF (TLOSE.GT.0) THEN
               WRITE (MSGTXT,1660) TLOSE
               CALL MSGWRT (3)
               END IF
            IF (TOSSTA.GT.0) THEN
               WRITE (MSGTXT,1650) TOSSTA
               CALL MSGWRT (3)
               END IF
            END IF
         END IF
      IF (LCALA) THEN
C                                     invert global submatrix
         NEXP = 0
         IF (LGLO) THEN
C            IF (MEP.GT.0) NEXP = NIF*MST
C            WRITE (*,*) 'ma ',MGP,NEXP,LGOBA,GLOVEC,' b-b ', GLOMAT
            CALL MATINV (MGP, GLOMAT, GLOVEC, LGOBA, MMP, NEXP,
     *           DENTL, DENTC, DENTR, WORK, ITO, IFROM, CUTOF, GAMMA)
         END IF
         SUMGPS = NEXP
      END IF
C
      IF (LSMP) THEN
C                                     SUMGP  = number of global parameters
C                                     SUMLP  = number of local parameters
C                                     SUMLPS = number of singular local parameters
C                                     SUMGPS = number of singular global parameters
C                                     SUMCOR = number of complex correlators measured
C                                     NFREE  = number of degrees of freedom
C                                     SUMSQR = sum of squared residuals
C                                     CHI2   = formal chi-squared
C                                     SUMWGT = sum of weights
C                                     SUMWT2 = sum of squared weights
C                                     AVGW   = average weight
C                                     NCHI2  = weighted mean squared residual per deg of freedom
C                                            = a posteriori amplitude variance
C                                     RCHI2  = reduced chi-squared IF user has supplied rms noise
C                                     MNOIZ  = expected naturally weighted map noise
C                                     SCPAR  = scaling factor for parameter errors
C                                     FTOS   = scaling factor from formal to suggested errors

         NFREE = 2.0 * SUMCOR + SUMGPS + SUMLPS - SUMGP - SUMLP
         CHI2  = SUMSQR
         AVGW  = SUMWGT / SUMCOR
         NCHI2 = ( CHI2 / AVGW ) * ( 1.0 / NFREE )
         RCHI2 = NCHI2
         IF (RNOISE.GT.0.0) RCHI2 = RCHI2 / ( RNOISE * RNOISE )
         MNOIZ = NCHI2 * SUMWT2 / (SUMWGT**2)
         SCPAR = SQRT(AVGW)
         IF (RNOISE.GT.0.0) SCPAR = SCPAR * RNOISE
         FTOS = SQRT(NCHI2)
C      MFB adds here
C         WRITE(6,*) 'xmfb: chi2,nchi2,rchi2,SCPAR,RNOISE,MNOIZ=',
C     *     CHI2,NCHI2,RCHI2,SCPAR, RNOISE, MNOIZ
C        MFB: looks like rchi2 corresponds to one printed out
C             but chi2 does not.
C        the chi2 we want is RCHI2 * AVGW * NFREE * RNOISE*RNOISE
C        except AVGW is not printed out!;  seems like rnoise = 1.0
         END IF
      IF (LCALA) THEN
         IF (LGLO) THEN

C                                     compute global corrections
            DO 580 I = 1,MGP
               DGOBA(I) = DZERO
               DO 570 J = 1,MGP
                  DGOBA(I) = DGOBA(I) + GLOMAT(I,J)*GLOVEC(J)
 570           CONTINUE
 580        CONTINUE
            DO 590 I = 1, MGP
C                                     compute global sigmas
C                                     [approximate gamma=0 error bar]
               SGOBA(I) = SQRT(GLOMAT(I,I))
 590        CONTINUE
         ELSE
            CALL DFILL (MGP, DZERO, DGOBA)
            CALL DFILL (MGP, DZERO, SGOBA)
         END IF
C                                       Save the current chi-squared as
C                                       the previous chi-squared
         PNFREE = NFREE
         PCHI2 = CHI2
         PNCHI2 = NCHI2 + 1.0D-50
         PRCHI2 = RCHI2
         PMNOIZ = 1000.0*SQRT(MNOIZ)
         MAXDEV = PNCHI2 * OTHRSH
         LOWSNR = PNCHI2 * LTHRSH
         PNCHI2 = SQRT ( PNCHI2 )
         PSCPAR = SCPAR
C                                       print out ChiSq to screen
         IF (PRTLEV.GT.1) THEN
            IF (RNOISE.GT.0.0) THEN
               WRITE (MSGTXT,2090) RNOISE
               CALL MSGWRT (3)
               END IF
            WRITE (MSGTXT,2091) PNCHI2
            CALL MSGWRT (3)
            WRITE (MSGTXT,2092) PMNOIZ
            CALL MSGWRT (3)
            END IF
         IF (PRTLEV.GT.3) THEN
            TCHI2 = PRCHI2 * PNFREE
            WRITE (MSGTXT,2093) TCHI2
            CALL MSGWRT (3)
            WRITE (MSGTXT,2094) PRCHI2
            CALL MSGWRT (3)
            END IF
C                MFB omft9 adds
C                CHI2 is available here through SINFO
C                it seems independent of APARM(1)
C                so scale it appropriately
         IF (PRTLEV.GT.1) THEN
            IF ((RNOISE.GT.0.0) .OR. (PNCHI2.LE.0.0)) THEN
               T2CHI2 = CHI2 / RNOISE / RNOISE
            ELSE
               T2CHI2 = CHI2 / PNCHI2 / PNCHI2
               END IF
            WRITE (MSGTXT,2398) T2CHI2
            CALL MSGWRT (3)
            END IF
         IF (PRTLEV.GT.2) THEN
            WRITE (MSGTXT,2399) (T2CHI2/PNFREE)
            CALL MSGWRT (3)
C                end MFB omft9 adds
            M = PNFREE
            N = SUMCOR
            WRITE (MSGTXT,2095) N, M
            CALL MSGWRT (3)
            M = SUMGP + SUMLP
            WRITE (MSGTXT,2096) M
            CALL MSGWRT (3)
            M = SUMGPS + SUMLPS
            IF (M.GT.0) THEN
               WRITE (MSGTXT,2097) M
               CALL MSGWRT (3)
            END IF
         END IF
         IF ((SUMLPS+SUMGPS).GT.DZERO) THEN
            WRITE (MSGTXT,1530) SUMLPS, SUMGPS
            CALL MSGWRT (1)
         END IF
      END IF

      IF (LGLO.AND.(.NOT.LCALA)) THEN
C                                       convert global sigmas to error bars
         DO 610 I = 1,MGP
            SGOBA(I) = SGOBA(I) * PSCPAR
 610        CONTINUE
         END IF
C
      IF (LRES) THEN
C                                       compute the sup-norm of the parameter
C                                       changes as measured in units of
C                                       estimated parameter error.
         GNORM = DZERO
         DO 620 I = 1,MGP
            IF (LGOBA(I).AND.(SGOBA(I).GT.DZERO)) THEN
               GNORM = MAX ( GNORM, ABS(DGOBA(I)/SGOBA(I)))
               END IF
 620        CONTINUE
         END IF
C
      IF (LCAL.AND.(.NOT.LCALA)) THEN
C                                       swap local solns
         I = OLDPLS
         OLDPLS = NEWPLS
         NEWPLS = I
         END IF
C
      IF (LGLO.AND.(.NOT.LCALA).AND.(.NOT.LEND)) THEN
C                                       update globals solns and
C                                       update ancillary quantities
         J = 1
         K = 1
         DO 630 I = 1,MCP
            CALL MODELS (CGOBA(I), XGOBA(K), DGOBA(J))
            J = J + XGOBA(K+1)
            K = K + XGOBA(K)
 630     CONTINUE
C                                       update station gains
         IF (MEP.GT.0) CALL MODELS ('GAINS', XGOBA(K), DGOBA(J))
C                                     BOTH OF THESE ARE GOOD DEBUGGING TOOLS
C                                     FOR GAINS
C         WRITE (*,*) 'd- ',(DGOBA(MGP-MEP+I),I=1,MEP)
C         WRITE (*,*) 'x- ',(XGOBA(MXP-MEP+I),I=1,MEP)
         END IF
C
      IF (LRES) THEN
C                                       this is the percentage change in
C                                       the weighted sum-squared residual
         IF (PCHI2.GT.1.0D-20) THEN
            DELRMS = 100.0D0 * (1.0D0 - RCHI2/PRCHI2)
         ELSE
            DELRMS = 100.0D0
            END IF
         IF (PRTLEV.GT.1) THEN
            WRITE (MSGTXT,2410) DELRMS
            CALL MSGWRT (3)
            IF (ABS(DELRMS).LT.C2FRAC) THEN
               WRITE (MSGTXT,2412) C2FRAC
               CALL MSGWRT (3)
            END IF
            WRITE (MSGTXT,2411) GNORM
            CALL MSGWRT (3)
            IF (GNORM.LT.GNFRAC) THEN
               WRITE (MSGTXT,2413) GNFRAC
               CALL MSGWRT (3)
            END IF
            IF ((GNORM.LT.GNFRAC).AND.(ABS(DELRMS).LT.C2FRAC)) THEN
               WRITE (MSGTXT,2414)
               CALL MSGWRT (3)
            END IF
            IF ((DELRMS.LT.0.0D0).AND.(ITER.LT.NITER)) THEN
               WRITE (MSGTXT,2401)
               CALL MSGWRT (6)
            END IF
         END IF
      END IF
C
      IF (LBEG) THEN
C                                       open scratch files
C                                       AND possibly fill from SN table
         I = 1
         NEWPLS = PL1
         OLDPLS = PL2
         CALL SCRGET (UVSCR, 'SOPE', OLDPLS, NEWPLS, JRET, TSOLVE,
     *      TSMIN, MSC, MSP, MTT, TSFLG, DOF, MCLP, LOCPAR, MEP,
     *      XGOBA(MXP-MEP+1), SLOCA, MST, NIF)
         IF (JRET.NE.0) GO TO 990
         END IF
C
      IF (LEND.AND.(.NOT.LCALA)) THEN
C                                       empty scratch file to SN table
C                                       AND delete scratch files
         I = 1
         CALL SCRGET (UVSCR, 'SCLO', OLDPLS, NEWPLS, JRET, TSOLVE,
     *      TSMIN, MSC, MSP, MTT, TSFLG, DOF, MCLP, LOCPAR, MEP,
     *      XGOBA(MXP-MEP+1), SLOCA, MST, NIF)
         IF (JRET.NE.0) GO TO 990
C                                       AND write to the output file!
         CALL TOFILE (UVSCR, GLOMAT, CGOBA, XGOBA, SGOBA, LGOBA,
     *        XGOBA(MXP-MEP), SGOBA(MGP-MEP+1), LGOBA(MGP-MEP+1))
         END IF
      GO TO 999
C
 990  CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT (' Discard ',I4,' vis and ',I4,' params at timecode#',I4)
 1600 FORMAT ('  because #params exceeds #measurements')
 1610 FORMAT ('  because insuf. ',A8,'S for solution')
 1530 FORMAT ('Discarded ',F5.0,' local and ',F5.0,
     *   ' global paramaters)')
 1650 FORMAT ('  (Discarded ',I6,' stations)')
 1660 FORMAT ('  (Discarded ',I6,' solution intervals)')
 1661 FORMAT (' Used ',I6,' solution intervals')
C                                       for Show-ChiSq
 2090 FORMAT (1X,'Assumed Thermal Noise = ',F17.8,' Jy/Vis')
 2091 FORMAT (1X,'Post-Fit rms          = ',F17.8,' Jy/Vis')
 2092 FORMAT (1X,'Nat. Wgtd. Map Noise  = ',F15.8,' mJy/Beam')
 2093 FORMAT (1X,'Ketan Chi-Squared         = ',G26.8)
 2094 FORMAT (1X,'Ketan Reduced Chi-Squared = ',G26.8)
 2398 FORMAT (1X,'Real Chi-Squared    = ',G26.8)
 2399 FORMAT (1X,'Real Reduced Chi^2  = ',G26.8)
 2095 FORMAT (1X,I10,' Vis. , ',I10,' Deg. of Freedom')
 2096 FORMAT (1X,'Fitted for ',I10,' model parameters')
 2097 FORMAT (1X,'          (',I10,' parameters were singular)')
 2410 FORMAT (1X,'Change in Reduced Chi-Squared = ',F12.8,' %')
 2411 FORMAT (1X,'Max Parameter (Change/Error) <',F16.8)
 2412 FORMAT (1X,'--> Reduced Chi-Squared stable to ',F8.5,' %')
 2413 FORMAT (1X,'--> Parameters converged to < ',F7.5,
     *   ' sigmas')
 2414 FORMAT (1X,'--> Model not changing, STOPPING now.')
 2401 FORMAT (1X,'--> Chi-Squared increased!, Reject Model Changes')
      END
      SUBROUTINE CRUNCH (GETGRD, CNTVIS, WRITOT, NEXTN, LASTN, LPRNOW,
     *   UVSCR, LA, LP, LSTAT, MODC, MODTC, SSTATS, BADSTA, KEPTST,
     *   LGRADC, LOCMAR, NORVEC, VECTOR, ARRAY, GRADC,
     *   CGOBA, XGOBA, XGOBG, LGOBA, DOF, KRET)
C-----------------------------------------------------------------------
C   CRUNCH steps through the data set, providing all visibilities in
C   a given time interval for analysis before exitting
C   Input:
C      GETGRD  L            logical, if true, gradient is calculated
C      CNTVIS  L            logical, if true, model is not calculated
C      NEXTN   I            timecode for current solution interval
C      MANT    I            maximum number of stations
C      NANT    I            actual number of stations
C      LA      L            solve flag for t-dep. amplitudes
C      LP      L            solve flag for t-dep. phases
C      UVSCR   C*(*)        OOP name of scratch UV data object
C      MSP     I            declared dimensions of BADSTA,SSTATS, and KEPTST
C      BADSTA  L(MSP)       flag to determine which stations are useful
C      SSTATS  I(MSP,MSP)   array to hold statistics of baselines found
C      KEPTST  L(MSP)       solve flag for current timecode's gains
C   Output:
C      NEXTN   I            timecode for next solution interval
C      LASTN   I            timecode of solution interval just processed
C      KRET    I   Error code: -1 => end of file
C                              =0 => end of timestamp
C                              >0 => error status
C-----------------------------------------------------------------------
C                                       i/o variables
      LOGICAL GETGRD, CNTVIS, WRITOT
      INTEGER NEXTN, LASTN, KRET, LPRNOW(2)
      CHARACTER UVSCR*(*)
      LOGICAL LA, LP, LSTAT
      INCLUDE 'FIT0.INC'
      COMPLEX MODC(MCP), MODTC(MCP)
      INTEGER SSTATS(MSP,MSP)
      DOUBLE PRECISION NORVEC(5,MST,NIF,MSP,MSC)
      LOGICAL BADSTA(MSP), KEPTST(MSP)
      COMPLEX LGRADC(MSP,2)
      COMPLEX LOCMAR(MST,NIF,MSP,MSC)
      REAL    GRADC(2*MTP)
      DOUBLE PRECISION VECTOR(MTP), ARRAY(MTP,MTP)
      CHARACTER CGOBA(MCP)*8
      DOUBLE PRECISION XGOBA(MXP), XGOBG(MST,NIF,MSP)
      LOGICAL LGOBA(MGP)
      INTEGER DOF(MTT)
C                                       includes
      INCLUDE 'KINFO.INC'
      INCLUDE 'LINFO.INC'
      INCLUDE 'IINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'SINFO.INC'
      INCLUDE 'ZINFO.INC'
C                                       internal variables
      INTEGER DIM(7), TYPE, IDUM, SORID
      CHARACTER UVOUT*36
      LOGICAL DATAOK
      INTEGER   ICOR, ICHAN, ISTOK, I, J
      DOUBLE PRECISION FREQ, FRQFAC, SENST
      REAL RVIS,IVIS,WVIS,RTEMP, ROVIS, IOVIS, WOVIS
      DOUBLE PRECISION TEMP1, TEMP2, TEMP3
      DOUBLE PRECISION RESR, RESI
      COMPLEX CZERO, CONE, CTWOI, CI
      PARAMETER (CZERO = (0.0,0.0))
      PARAMETER (CONE  = (1.0,0.0))
      PARAMETER (CI = (0.0,1.0))
      PARAMETER (CTWOI = (0.0,2.0))
      LOGICAL TRUE
      SAVE UVOUT
      DATA TRUE /.TRUE./
C-----------------------------------------------------------------------
      KRET = 0
      IF (CNTVIS) CALL FILL (MSP*MSP, 0, SSTATS)
C                                       init for processing
      IF (NEXTN.LE.0) THEN
         CALL OOPEN (UVSCR, 'READ', KRET)
         IF (KRET.NE.0) GO TO 990
C                                       IST = (1,NAXI(LOCS))
C                                       STOKES VALUE = RVAL(LOCS) +
C                                        (IST - RPIX(LOCS))*RDEL(LOCS)
C                                       CHANI = (1,NAXI(LOCF))
C                                       FREQ VALUE = RVAL(LOCF) +
C                                        (CHANI - RPIX(LOCF))*RDEL(LOCF)
C                                       IFI = (1,NAXI(LOCIF))
C                                       IF VALUE = RVAL(LOCIF) +
C                                        (IFI - RPIX(LOCIF))*RDEL(LOCIF)
         IF (WRITOT) THEN
            CALL OGET (UVSCR, 'UV_OUTNAME', TYPE, DIM, IDUM, UVOUT,
     *                KRET)
            CALL OOPEN (UVOUT, 'WRIT', KRET)
            IF (KRET.NE.0) GO TO 990
            END IF
         NEXTN = 0
         END IF
C                                       process previous vis
      IF (NEXTN.GT.0) GO TO 200
C
 100  CONTINUE
      KRET = 0
C                                       get next vis
      CALL UVREAD (UVSCR, HDRBUF, VISBUF, KRET)
      IF (KRET.GT.0) GO TO 990
      IF (KRET.EQ.-1) THEN
         KRET = 0
         IF (WRITOT) CALL OCLOSE (UVOUT, KRET)
         KRET = 0
         CALL OCLOSE (UVSCR, KRET)
         LASTN = NEXTN
         KRET = -1
         GO TO 990
         END IF
      IF (LOCB.GT.0) THEN
         IB = HDRBUF(LOCB) + 0.1
         IA = IB/256
         IB = IB - IA*256
      ELSE
         IA = HDRBUF(LOCA1) + 0.1
         IB = HDRBUF(LOCA2) + 0.1
         END IF
      TIME = HDRBUF(LOCT)
      IF (LOCSU.GT.0) THEN
         SORID = HDRBUF(LOCSU)
      ELSE
         SORID = 0
         END IF
C                                       trundle around UVs in Mlambda's
      BU = HDRBUF(LOCU) / SCALE
      BV = HDRBUF(LOCV) / SCALE
      BW = HDRBUF(LOCW) / SCALE
      BUU = BU*BU
      BUV = BU*BV
      BVV = BV*BV
C
 200  CONTINUE
C                                       compute change in timestamp
      KRET = ( INT (TIME/TSOLVE) - TSMIN ) - NEXTN
C
      IF (CNTVIS) THEN
         IF (KRET.GT.0) THEN
            LASTN = NEXTN
            NEXTN = NEXTN + KRET
            KRET = 0
            GO TO 990
         END IF
C                                       timestamp is old, get next vis
         IF (KRET.EQ.0) THEN
            DO 222 IST = 1,MST
               DO 221 IIF = 1,NIF
                  DO 220 CHANI = 1,NCHNS
C                                     locate current visibility
                     ICOR = (IST-1)*INCS +
     *                      (IIF-1)*INCIF +
     *                      (CHANI-1)*INCF
C                                     count it if its weight is good
                     IF (VISBUF(ICOR+3).GT.1.0E-20)
     *                    SSTATS(IA,IB) = SSTATS(IA,IB) + 1
 220              CONTINUE
 221           CONTINUE
 222        CONTINUE
         END IF
         GO TO 100
      END IF
C                                     if the timestamp is old or bad,
C                                     get next vis
      IF ((KRET.LT.0).OR.(DOF(NEXTN+KRET).LT.0)) GO TO 100
C                                       if timestamp is new, exit
      IF (KRET.GT.0) THEN
         LASTN = NEXTN
         NEXTN = NEXTN + KRET
         KRET = 0
         GO TO 990
         END IF
C                                       process next vis
      IF (BADSTA(IA).OR.(BADSTA(IB))) GO TO 100

      IF (GETGRD) THEN
C                                       prep self-cal gradient
         CALL CXFILL (2*MSP, CZERO, LGRADC)
C                                       prep simple self-cal gradient
         IF (LA) THEN
            LGRADC(IA,1) = +CONE
            LGRADC(IB,1) = +CONE
         END IF
         IF (LP) THEN
            LGRADC(IA,2)   = +CI
            LGRADC(IB,2)   = -CI
         END IF
         KEPTST(IA) = TRUE
         KEPTST(IB) = TRUE
      END IF
C                                       process data
      DO 330 IST = 1,MST
C                                       locate current stokes code
         ISTOK = RVAL(LOCS) + (IST-RPIX(LOCS))*RDEL(LOCS)
         DO 320 IIF = 1,NIF
C                                       assemble self-calibration
C                                       portion of model
C
C                                       prep sensitivity factors
            IF (MEP.GT.0) THEN
               SENST = XGOBG(IST,IIF,IA) + XGOBG(IST,IIF,IB)
               RTEMP = EXP(SENST)
            ELSE
               RTEMP = 1.0
               END IF
C                                       load sensitivity factors
            DO 249 I = 1,MCP
               MODTC(I) = RTEMP
 249           CONTINUE
C                                       now toss in time-dependant gains
            DO 250 I = 1,MSC
               MODTC(I) = MODTC(I)*      LOCMAR(IST,IIF,IA,I) *
     *                             CONJG(LOCMAR(IST,IIF,IB,I)  )
C               WRITE (*,*) 'v2-',IA,IB,MODTC(I),IST,IIF,I,
C     *              LOCMAR(IST,IIF,IA,I),LOCMAR(IST,IIF,IB,I)
 250           CONTINUE
C                                       prepare statistics on self-cal model
            IF (LSTAT) THEN
C                                       accumulate normalization vector
               IF (MEP.GT.0) THEN
                  TEMP1 = XGOBG(IST,IIF,IA)
                  TEMP1 = EXP(-TEMP1)
                  TEMP2 = XGOBG(IST,IIF,IB)
                  TEMP2 = EXP(-TEMP2)
               ELSE
                  TEMP1 = 1.0
                  TEMP2 = 1.0
                  END IF
               DO 225 I = 1,MSC
                  TEMP3 = TEMP1/CABS(LOCMAR(IST,IIF,IA,I)) - 1.0D0
                  TEMP3 = CABS(LOCMAR(IST,IIF,IA,I)) - 1.0D0
                  NORVEC(1,IST,IIF,IA,I) = NORVEC(1,IST,IIF,IA,I)+
     *                 TEMP3
                  NORVEC(2,IST,IIF,IA,I) = NORVEC(2,IST,IIF,IA,I)+
     *                 TEMP3**2
                  NORVEC(3,IST,IIF,IA,I) = NORVEC(3,IST,IIF,IA,I)+
     *                 1.0D0
                  TEMP3 = TEMP2/CABS(LOCMAR(IST,IIF,IB,I)) - 1.0D0
                  TEMP3 = CABS(LOCMAR(IST,IIF,IB,I)) - 1.0D0
                  NORVEC(1,IST,IIF,IB,I) = NORVEC(1,IST,IIF,IB,I)+
     *                 TEMP3
                  NORVEC(2,IST,IIF,IB,I) = NORVEC(2,IST,IIF,IB,I)+
     *                 TEMP3**2
                  NORVEC(3,IST,IIF,IB,I) = NORVEC(3,IST,IIF,IB,I)+
     *                 1.0D0
 225           CONTINUE
            END IF
            DO 310 CHANI = 0,NCHNS-1
C                                       locate current visibility
               ICOR = (IST - 1)*INCS + (IIF-1)*INCIF + CHANI*INCF
C                                       locate current channel number
               ICHAN = NCHNS*(IIF-1) + BEGCH + CHANI
C                                       convert freq into MHz
               FREQ = FREQV(1 + ICHAN - BEGCH)/1.0E6
C                                       construct UV scaling factor
               FRQFAC = FREQV (1 + ICHAN - BEGCH) / FREQ0
C                                       check weight
               IF (VISBUF(ICOR+3).LE.1.0E-20) GO TO 310
               RVIS = VISBUF(ICOR+1)
               IVIS = VISBUF(ICOR+2)
C                                       MFB added weight control here
               IF (WTPOW.EQ.0.0) THEN
                  WVIS = 1.0
               ELSE
                  WVIS = VISBUF(ICOR+3)
                  IF (WTPOW.NE.1.0) WVIS = WVIS ** WTPOW
                  END IF
C                                       MFB MOD to go here
C                                             prep complex gradient
               IF (GETGRD) CALL RFILL (2*MTP, 0.0, GRADC)
C                                             load self-cal corrections into
C                                             model
               CALL RCOPY (2*MCP, MODTC, MODC)
C                                             assemble full model
               I = 2*(MGP-MEP)+1
               J = 2*MGP+1
C               write (*,*) 'tc -',IST,IIF,MODC
               ROVIS = RVIS
               IOVIS = IVIS
               WOVIS = WVIS
C               IF (RVIS*RVIS+IVIS*IVIS.GT.800) THEN
C                  WRITE (*,*) TIME,IA,IB,ROVIS,IOVIS,WOVIS
C                  END IF
               CALL MODEL (GETGRD, RESR, RESI, SCALE, LPRNOW,
     *              FREQ, FRQFAC, ICHAN, ISTOK, SORID,
     *              ROVIS, IOVIS, WOVIS, MODC, LGRADC,
C                   globals,  gains,  self-cal params
     *              GRADC(1), GRADC(I), GRADC(J),
     *              CGOBA, XGOBA, LGOBA, LGOBA(MGP-MEP+1), DATAOK)
C               WRITE (*,*) 'g- ',GRADC
C
               IF (.NOT.DATAOK) THEN
                  VISBUF(ICOR+3) = -1.0
               ELSE
                  CALL DOSUMS (RESR, RESI, WVIS,
     *                 SUMSQR, SUMCOR, SUMWGT, SUMWT2)
                  IF (GETGRD) THEN
C                                             add to vector and array
                     DO 455 I = 1,MTP
                        IF ((GRADC(2*I-1).NE.0.0).OR.
     *                      (GRADC(2*I).NE.0.0)) THEN
C                              WRITE (*,*) 'ves ',RESR,RESI,WVIS,
C     *                          I,GRADC(2*I-1),GRADC(2*I)
                           VECTOR(I) = VECTOR(I)
     *                          + (RESR*GRADC(2*I-1)
     *                          - RESI*GRADC(2*I)) * WVIS
                           DO 450 J = I,MTP
                              ARRAY(I,J) = ARRAY(I,J)
     *                             + (GRADC(2*J-1)*GRADC(2*I-1)
     *                             + GRADC(2*J)*GRADC(2*I)) * WVIS
C                              WRITE (*,*) 'mes ',RESR,RESI,WVIS,
C     *                          I,GRADC(2*I-1),GRADC(2*I),
C     *                          J,GRADC(2*J-1),GRADC(2*J)
 450                          CONTINUE
                           END IF
 455                    CONTINUE
C                  IF (GETHES) THEN
C                     True Hessian calculations here
C                     END IF
                     END IF
                  VISBUF(ICOR+1) = ROVIS
                  VISBUF(ICOR+2) = IOVIS
                  VISBUF(ICOR+3) = WOVIS
                  END IF
 310        CONTINUE
 320     CONTINUE
 330  CONTINUE
      IF (WRITOT) CALL UVWRIT (UVOUT, HDRBUF, VISBUF, KRET)
C                                       dont leave yet!
      IF (KRET.EQ.0) GO TO 100
 990  CONTINUE
      RETURN
      END
      SUBROUTINE DOSUMS (RESR, RESI, WVIS, SUMSQR, SUMCOR, SUMWGT,
     *   SUMWT2)
C-----------------------------------------------------------------------
C                                       accumulate weighted residuals
C                                             # measurements
C                                             weights
C                                             squared weights
C-----------------------------------------------------------------------
      DOUBLE PRECISION RESR, RESI, SUMSQR, SUMCOR, SUMWGT, SUMWT2
      REAL WVIS
C-----------------------------------------------------------------------
      SUMSQR = SUMSQR + (RESR*RESR + RESI*RESI) * WVIS
      SUMCOR = SUMCOR + 1.0D0
      SUMWGT = SUMWGT + WVIS
      SUMWT2 = SUMWT2 + WVIS*WVIS
      RETURN
      END
      SUBROUTINE STSTAT (OP, MSP, SSTATS, IPAR, IOBS)
C-----------------------------------------------------------------------
C   Input:
C      OP     C*(*)        opcode
C      MSP    I            number of stations to loop over
C   Output:
C      SSTATS I(MSP,MSP)   baseline statistics are kept here
C      IPAR   I            number of 'acceptable' stations at this time
C      IOBS   I            number of vis. measurements at this time
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER OP*(*)
      INTEGER IPAR, IOBS
      INTEGER MSP
      INTEGER SSTATS(MSP,MSP)
C                                       includes
      INCLUDE 'SINFO.INC'
C                                       internal variables
      INTEGER I, J, K
C-----------------------------------------------------------------------
C                                       compute SSTATS(i:i)
      IF (OP.EQ.'TRIANGLE') THEN
         DO 130 I = 1,MSP-2
            DO 120 J = I+1,MSP-1
               IF ( (SSTATS(I,J) .GT. 0) .OR.
     *            (SSTATS(J,I) .GT. 0)     ) THEN
                  DO 110 K = J+1,MSP
                     IF ( ((SSTATS(I,K) .GT. 0) .OR.
     *                  (SSTATS(K,I) .GT. 0)     ) .AND.
     *                  ((SSTATS(J,K) .GT. 0) .OR.
     *                  (SSTATS(K,J) .GT. 0)     )      ) THEN
                        SSTATS(I,I) = SSTATS(I,I) + 1
                        SSTATS(J,J) = SSTATS(J,J) + 1
                        SSTATS(K,K) = SSTATS(K,K) + 1
                        ENDIF
 110                 CONTINUE
                  ENDIF
 120           CONTINUE
 130        CONTINUE
         ENDIF
      IF (OP.EQ.'BASELINE') THEN
         DO 160 I = 1,MSP
            DO 150 J = 1,MSP
               IF (I.NE.J) THEN
                  SSTATS(I,I) = SSTATS(I,I) + SSTATS(I,J)
                  SSTATS(J,J) = SSTATS(J,J) + SSTATS(I,J)
                  ENDIF
 150           CONTINUE
 160        CONTINUE
         ENDIF
C                                       compute IPAR
      IPAR = 0
      DO 180 I = 1,MSP
         IF (SSTATS(I,I).GT.0) THEN
            IPAR = IPAR + 1
         ELSE
            DO 170 J = 1,MSP
               SSTATS(I,J) = 0
               SSTATS(J,I) = 0
 170           CONTINUE
            ENDIF
 180     CONTINUE
C                                       compute IOBS
      IOBS = 0
      DO 200 J = 2,MSP
         DO 190 I = 1,J-1
            IOBS = IOBS + SSTATS(I,J) + SSTATS(J,I)
 190        CONTINUE
 200     CONTINUE
      RETURN
      END
      SUBROUTINE MODEL (GETGRD, RESR, RESI, SCALE, LPRNOW, FREQ, FRQFAC,
     *   ICHAN, ISTOK, SORID, RVIS, IVIS, WVIS, MODC, LGRADC, GRADC,
     *   GRADG, GRADS, CGOBA, XGOBA, LGOBA, LGOBG, DATAOK)
C-----------------------------------------------------------------------
C   MODEL computes the model visibility for a given baseline at
C   a given time
C   Input:
C      GETGRD  L  if true, compute model gradient also
C   Output:
C      only via COMMONs
C-----------------------------------------------------------------------
C                                       i/o variables
      LOGICAL   GETGRD, DATAOK
      INTEGER   ICHAN, ISTOK, LPRNOW(2), SORID
      DOUBLE PRECISION FREQ, FRQFAC, SCALE
      INCLUDE 'FIT0.INC'
      COMPLEX   MODC(MCP), LGRADC(MSP,2), MODTC, TRUEC
      REAL      RVIS, IVIS, WVIS
      COMPLEX   GRADC(MTP), GRADG(MST,NIF,MSP), GRADS(2,MST,NIF,MSP,MSC)
      CHARACTER CGOBA(MCP)*8, TXLINE*200
      DOUBLE PRECISION XGOBA(MXP)
      LOGICAL   LGOBA(MGP), LGOBG(MST,NIF,MSP)
C                                       includes
      INCLUDE 'KINFO.INC'
      INCLUDE 'DINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'SINFO.INC'
C                                       internal variables
      INTEGER I, J, K, P, Q, R, JT, JTRIM
      COMPLEX CZERO, CONE
      COMPLEX TC
      REAL RTC, ITC, RTRC, ITRC, RTEMP, RTEMP2
      DOUBLE PRECISION SBU, SBV, SBW, SBUU, SBUV, SBVV
      DOUBLE PRECISION RESR, RESI
      PARAMETER (CZERO = (0.0,0.0))
      PARAMETER (CONE = (1.0,0.0))
C     Just for your info.
C     ISTOK =  -8 -7 -6 -5 -4 -3 -2 -1   0  1 2 3 4
C    5     6     7        8        9
C              YX XY YY XX LR RL LL RR beam I Q U V
C  %pol fracpol polpa  sindex  optdepth
C-----------------------------------------------------------------------
      DATAOK = .TRUE.
C                                       assemble full model here
      J = 1
      K = 1
      TC = 0.0
      TRUEC = 0.0
      DO 220 I = 1,MCP
         R = INT(XGOBA(K+2))
         P = INT(XGOBA(K+3))
         Q = INT(XGOBA(K+4))
C         WRITE (*,*) 'dat -', P,ICHAN,Q,IIF
         IF (((P.EQ.0).OR.(P.LE.ICHAN)).AND.
     *       ((Q.EQ.0).OR.(ICHAN.LE.Q)).AND.
     *       ((R.EQ.0).OR.(SORID.EQ.0).OR.(SORID.EQ.R))) THEN
            SBU = BU * FRQFAC
            SBV = BV * FRQFAC
            SBW = BW * FRQFAC
            SBUU = BUU * FRQFAC*FRQFAC
            SBUV = BUV * FRQFAC*FRQFAC
            SBVV = BVV * FRQFAC*FRQFAC
            CALL MODELM (CGOBA(I), ICHAN, FREQ, ISTOK, TIME,
     *         SBU, SBV, SBW, SBUU, SBUV, SBVV,
     *         MODC(I), MODTC, XGOBA(K), LGOBA(J), GETGRD,
     *         GRADC(J))
C                                       MODTC contains only the model
            TRUEC = TRUEC + MODTC
C                                       MODC also contains the self-cal info
C                                       [amp + phase + gain corrections]
            TC = TC + MODC(I)
         END IF
         J = J + XGOBA(K+1)
         K = K + XGOBA(K)
 220  CONTINUE
      RTC  =  REAL(TC)
      ITC  = AIMAG(TC)
      RTRC =  REAL(TRUEC)
      ITRC = AIMAG(TRUEC)
C                                       calculate meas. vis - model
      RESR =   (RVIS - RTC)
      RESI = - (IVIS - ITC)
C      WRITE (*,*) 'data = ',RVIS,IVIS,RTC,ITC
C                                       here, data could be CUTOFd based upon
C                                       excessively large residuals       -OR-
C                                           [meaning outliers]
C                                       excessively small data amplitudes
C                                           [meaning low SNR]
C                                       if to be CUTOFd, set DATAOK=.FALSE.
C                                       and jump to end of subroutine
      DATAOK = (((RESR*RESR + RESI*RESI).LT.MAXDEV).OR.
     *          (MAXDEV.EQ.0.0D0)                      )    .AND.
     *         ((RVIS*RVIS + IVIS*IVIS).GT.LOWSNR)
      IF (.NOT.DATAOK) THEN
         WVIS = - WVIS
         GO TO 990
         END IF
C
C                                       send out residuals at end of day!
      IF (LPRNOW(1).GT.0) THEN
C                                       station A, station B, channel, stokes
         WRITE (TXLINE,500) IA, IB, IIF, ICHAN, ISTOK,
C                                       time(sec), freq(Hz), U,V,W(lambda)
     *      86400.0*TIME, 1.E6*FREQ, SBU*SCALE, SBV*SCALE, SBW*SCALE,
C                                       Weight, ReVis, ImVis
     *      WVIS, RVIS, IVIS,
C                                       ReMod, ImMod [full model-NOT residuals]
     *      RTC, ITC,
C                                       TrueMod [minus any self-cal]
     *      RTRC, ITRC
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', LPRNOW(1), LPRNOW(2), TXLINE(:JT), JT)
         END IF
C
C     the organization of GRADC is:
C     1          :   MGP-MSP-1        global parameters
C     MGP-MSP    :   MGP     sensitivity factors - if MEP>0
C     MGP+1      :
      IF (GETGRD) THEN
         IF (MEP.GT.0) THEN
C            WRITE (*,*) 'm ',IA,IB,IST,IIF,ICHAN,MODC,TC,
C     *           LGOBG(IST,IIF,IA), LGOBG(IST,IIF,IB), RESR,RESI
            IF (LGOBG(IST,IIF,IA)) GRADG(IST,IIF,IA) = TC
            IF (LGOBG(IST,IIF,IB)) GRADG(IST,IIF,IB) = TC
         END IF
C                                       SELF-CAL gradient
         DO 600 I = 1,MSC
            DO 68 K = 1,MSP
               GRADS(1,IST,IIF,K,I) = LGRADC(K,1) * MODC(I)
               GRADS(2,IST,IIF,K,I) = LGRADC(K,2) * MODC(I)
C                                     [force identical for all components]
               GRADS(1,IST,IIF,K,I) = LGRADC(K,1) * TC/MCP
               GRADS(2,IST,IIF,K,I) = LGRADC(K,2) * TC/MCP
 68         CONTINUE
 600     CONTINUE
      END IF
C
 990  CONTINUE
C                                     d = data
C                                     t = complete model
C                                     s = source model
C                                     c = self-cal model
C                                     t = s * c
      IF (OUTTYP.EQ.1) THEN
C                                     d - t
C                                     send out the data minus the model
         RVIS = RESR
         IVIS = -RESI
      ELSE IF (OUTTYP.EQ.2) THEN
C                                     s
C                                     send out the model
C                                     w/o amp, phase, or gain
C                                     corrections
         RVIS = RTRC
         IVIS = ITRC
      ELSE IF (OUTTYP.EQ.3) THEN
C                                     t
C                                     send out the complete model
C                                     w/ amp, phase, and gain
C                                     corrections
         RVIS = RTC
         IVIS = ITC
      ELSE IF (OUTTYP.EQ.4) THEN
C                                     send out data / self-cal model
         RTEMP2 =  RTC*RTC+ITC*ITC
         IF (RTEMP2.GT.1.0E-20) THEN
C                                     construct inverse of self-cal model
            RTEMP = (ITRC * RTC - RTRC * ITC) / RTEMP2
            RTRC  = (RTRC * RTC + ITRC * ITC) / RTEMP2
            ITRC  = RTEMP
C                                     apply it to the data
            RTEMP = RVIS*ITRC + IVIS*RTRC
            RVIS  = RVIS*RTRC - IVIS*ITRC
            IVIS  = RTEMP
C                                     construct correction amplitude
            RTEMP = RTRC*RTRC + ITRC*ITRC
            IF (RTEMP.GT.1.0E-20) THEN
               WVIS = WVIS / RTEMP
            ELSE
               WVIS = 0.0
               END IF
            END IF
      ELSE IF (OUTTYP.EQ.5) THEN
         RTEMP2 = RTRC*RTRC+ITRC*ITRC
         IF (RTEMP2.GT.1.0E-20) THEN
C                                     send out data / source model
            RTEMP = (IVIS * RTRC - RVIS * ITRC) / RTEMP2
            RVIS  = (RVIS * RTRC + IVIS * ITRC) / RTEMP2
            IVIS  = RTEMP
            WVIS  = WVIS / RTEMP2
         ELSE
            WVIS = 0.0
            END IF
      ELSE IF (OUTTYP.EQ.6) THEN
         RTEMP2 = RTC*RTC+ITC*ITC
         IF (RTEMP2.GT.1.0E-20) THEN
C                                     send out data / complete model
            RTEMP = IVIS * RTC - RVIS * ITC
            RVIS  = RVIS * RTC + IVIS * ITC
            IVIS  = RTEMP
            RVIS  = RVIS / RTEMP2
            IVIS  = IVIS / RTEMP2
            WVIS  = WVIS / RTEMP2
         ELSE
            WVIS = 0.0
            END IF
      ELSE IF (OUTTYP.EQ.7) THEN
         RTEMP2 = RVIS*RVIS + IVIS*IVIS
         IF (RTEMP2.GT.1.0E-20) THEN
C                                     send out data / complete model
            RVIS = RVIS / SQRT(RTEMP2)
            IVIS = IVIS / SQRT(RTEMP2)
            WVIS = WVIS * RTEMP2
         ELSE
            WVIS = 0.0
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 500  FORMAT (2(I2,1X),2(I4,1X),I2,1X,F9.0,1X,F14.0,1X,3(F15.0,1X),
     *   F13.9,1X,6(F13.7,1X))
      END
      SUBROUTINE MATINV (M, ARR, VEC, KCON, L, NEXP, RIGHTT, SIGMA,
     *     LEFT, WORK, ITO, IFROM, CUTOF, GAMMA)
C-----------------------------------------------------------------------
C   MATINV inverts the matrix given in ARR.
C   MATINV also repacks ARR and VEC to zero those rows/columns not being
C     solved for
C   Input:
C      M       I      Dimension of ARR, VEC, KCON
C      ARR     R(M,M) Matrix to be packed, inverted, and unpacked
C      VEC     R(M)   Solution vector to be packed, and unpacked
C      KCON    L(M)   Vary flag controlling packing, unpacking
C      L       I      Dimension of ZDEX, RIGHTT, LEFT
C   Output:
C      SIGMA    R(L)   contains error bars
C      RIGHTT   R(L,L) work matrix
C      LEFT   R(L,L) secondary work matrix
C      CUTOF  R      SVD condition number cutoff
C      NEXP   I      number of singular values being thrown away
C-----------------------------------------------------------------------
C                                       i/o variables
      INTEGER M, L, NEXP
      DOUBLE PRECISION VEC(M)
      DOUBLE PRECISION ARR(M,M), RIGHTT(L,L), SIGMA(L), LEFT(L,L),
     *   WORK(10*L)
      LOGICAL KCON(M)
      INTEGER ITO(L), IFROM(L)
      INTEGER LWORK, INFO
      DOUBLE PRECISION CUTOF, SMIN, GAMMA
C                                       no includes
C                                       internal variables
      INTEGER N, I, J, K, Q, R, NEXPTD
C-----------------------------------------------------------------------
      NEXPTD = NEXP
C                                       save some time, find largest
      K = 0
      CALL FILL (M, 0, ITO)
      CALL FILL (M, 0, IFROM)
      DO 10 I = 1,M
         IF (KCON(I)) THEN
            K = K + 1
            ITO(I) = K
            IFROM(K) = I
            END IF
 10      CONTINUE
C                                     reflect/compress/rescale matrix/vector
      DO 30 I = 1,K
         DO 20 J = I+1,K
            ARR(I,J) = ARR(IFROM(I),IFROM(J)) / (1.0D0 + GAMMA)
            ARR(J,I) = ARR(I,J)
 20         CONTINUE
         ARR(I,I) = ARR(IFROM(I),IFROM(I))
         VEC(I) = VEC(IFROM(I))
         VEC(I) = VEC(I) / (1.0D0 + GAMMA)
 30      CONTINUE
C
      N = K
C                                       invert matrix here
      IF (N.EQ.1) THEN
         RIGHTT(1,1) = 1.0
         SIGMA(1)    = ARR(1,1)
         LEFT(1,1)   = 1.0
      ELSE IF (N.GE.2) THEN
         LWORK = 10*L
         CALL DGESVD ('A', 'A', N, N, ARR, M,
     *     SIGMA, LEFT, L, RIGHTT, L, WORK, LWORK, INFO)
         END IF
C
C                                       drop off the bottom few that
C                                       were a priori expected to
C                                       be singular!
      DO 35 J = 0,NEXPTD-1
         SIGMA(N-J) = 0.0D0
 35      CONTINUE
C                                       excise singular values here
      SMIN = SIGMA(1) * CUTOF
      NEXP = 0
      DO 40 J = 1,N
         IF (SIGMA(J).GT.SMIN) THEN
            SIGMA(J) = 1.0D0/SIGMA(J)
         ELSE
            SIGMA(J) = 0.0D0
            NEXP = NEXP + 1
            END IF
 40      CONTINUE
C
C                                       assemble inverted matrix
C                                       MAX statement is a kludge -fix it!
      R = N - NEXP
      DO 70 K = 1,N
         DO 60 J = 1,N
            ARR(J,K) = 0.0
            DO 50 Q = 1,R
               ARR(J,K) = ARR(J,K) + RIGHTT(Q,J)*SIGMA(Q)*LEFT(K,Q)
 50            CONTINUE
 60         CONTINUE
         ARR(K,K) = MAX(ARR(K,K),0.0D0)
 70      CONTINUE
C
C                                       decompress inverted matrix and vector
      DO 90 I = M,1,-1
         DO 80 J = M,1,-1
            IF ((ITO(I).NE.0).AND.(ITO(J).NE.0)) THEN
               ARR(J,I) = ARR(ITO(J),ITO(I))
            ELSE
               ARR(J,I) = 0.0
               END IF
 80         CONTINUE
         IF (ITO(I).NE.0) THEN
            VEC(I) = VEC(ITO(I))
         ELSE
            VEC(I)   = 0.0
            ARR(I,I) = 0.0
            END IF
 90      CONTINUE
C
      NEXP = NEXP - NEXPTD
      RETURN
      END
      SUBROUTINE SCRGET (UVDATA, OP, OLDPLS, NEWPLS, KRET, TSOLVE,
     *   TSMIN, MSC, MSP, MTT, TSFLGM, DOF, MCLP, LOCMAR, MEP, PGOB,
     *   SLOCAM, MST, NIF)
C-----------------------------------------------------------------------
C   SCRGET prepares scratch files
C   Input:
C      UVDATA  C*(*)  UVDATA file descroptor for OOP system
C      OP      C*(*)  control code for accessing scratch files
C      OLDPLS  I      scratch file LUN with current local parameters
C      NEWPLS  I      scratch file LUN with new local parameters
C   Output:
C      KRET    I      Error code 0 => ok
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER UVDATA*(*), OP*(*)
      INTEGER   TSMIN
      REAL      TSOLVE
      INTEGER   OLDPLS, NEWPLS, MSC, KRET
      INTEGER   MSP, MCLP
      INTEGER   MTT, MEP, MST, NIF
      LOGICAL   TSFLGM(MSP,MTT)
      COMPLEX   LOCMAR(MST,NIF,MSP,MSC)
      DOUBLE PRECISION PGOB(MST,NIF,MSP), SLOCAM(MST,NIF,MSP,MSC)
      INTEGER   DOF(MTT)
C                                       no includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       internal variables
      CHARACTER SCRDEV*256
      INTEGER DIM(7), TYPE, IDUM
      REAL RDUM
      CHARACTER CDUM*2
      CHARACTER OTPATH*48, SNTAB*36
C
      INTEGER NUMANT, NUMPOL, NUMIF, NUMNOD, SNROW
      REAL GMMOD, RANOD(25), DECNOD(25)
      INTEGER NKEYS, KLOCS(10), KVALS(20), KTYPE(10)
      CHARACTER KEYS(10)*8
      LOGICAL ISAPPL, ISBAD
      DOUBLE PRECISION TIME
      REAL TIMEI, IFR, MBDELY(2), RTEMP, RTEMP1, DISP(2), DDISP(2)
C                                       MAGIC - these need NIF,MST
      REAL CREAL(2,64), CIMAG(2,64), CDELY(2,64), CRATE(2,64),CWT(2,64)
      INTEGER CREF(2,64), REFA
      SAVE REFA
      INTEGER SOURID, SUBA, FREQID, TSTMP
      INTEGER I, J, K, L, T, P, Q, NTEMP
      LOGICAL INPSN, OUTSN
      COMPLEX CONE, CVIS
      LOGICAL FALSE
      PARAMETER (FALSE = .FALSE.)
      PARAMETER (CONE = (1.0,0.0))
      DATA MBDELY, DISP, DDISP /6*0.0/
C-----------------------------------------------------------------------
      IF (OP(1:4).EQ.'SOPE') THEN
C                                       OPEN SCRATCH FILES
C                                       find path to directory where
C                                       scratch files go
         CALL OGET (UVDATA, 'OUTPATH', TYPE, DIM, IDUM, OTPATH, KRET)
         IF (KRET.NE.0) GO TO 991
C                                       open scratch files
         CALL ZFULLN (' ', OTPATH, 'OMFIT.SCRATCH', SCRDEV, KRET)
         OPEN (UNIT=NEWPLS, FILE=SCRDEV, STATUS='UNKNOWN',
     *        FORM='UNFORMATTED')
         CALL ZFULLN (' ', OTPATH, 'OMFIT.SCRATCH', SCRDEV, KRET)
         OPEN (UNIT=OLDPLS, FILE=SCRDEV, STATUS='UNKNOWN',
     *        FORM='UNFORMATTED')
C                                       did user specify an SN table?
         CALL OGET (UVDATA, 'INPSN', TYPE, DIM, INPSN, CDUM, KRET)
         IF (KRET.NE.0) GO TO 991
         IF (INPSN) THEN
            CALL OGET (UVDATA, 'ISNTAB', TYPE, DIM, IDUM, SNTAB, KRET)
            IF (KRET.NE.0) GO TO 991
C                                       If we need to load old solutions
C                                       open SN table
            CALL OSNINI (SNTAB, 'READ', SNROW, NUMANT, NUMPOL, NUMIF,
     *         NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, KRET)
            IF (KRET.NE.0) GO TO 991
C                                       get OMFIT keyword from SN table
            KEYS(1) = 'OMFIT   '
            NKEYS = 1
            CALL TABKGT (SNTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE, KRET)
            IF (KRET.NE.0) GO TO 991
C                                       this is attrocious!
C                                       this should be NWDPFP in the
C                                       first slot of COPY
            CALL COPY (1, KVALS(1), RDUM)
C                                       check that OMFIT keyword was found
C                                       and that the same integration time
C                                       is being used!
C                                       check the usual keywords:
            INPSN = INPSN
     *         .AND.(NUMANT.EQ.MSP)
     *         .AND.(NUMPOL.EQ.1)
     *         .AND.(NUMIF.EQ.1)
     *         .AND.(NUMNOD.EQ.(MSC-1))
     *         .AND.(GMMOD.EQ.1)
     *         .AND.(KLOCS(1).NE.-1)
     *         .AND.(RDUM.EQ.TSOLVE)
            IF (.NOT.INPSN) THEN
               WRITE (MSGTXT,1900)
               CALL MSGWRT (3)
               WRITE (MSGTXT,1901)
               CALL MSGWRT (3)
               END IF
            END IF
C                                       loop over times
         DO 50 T = 1,MTT
C                                       set default local parameter values
            IF (MSC.GT.0) CALL CXFILL (MCLP, CONE, LOCMAR)
C                                       enough degrees of freedom?
            IF (DOF(T).LT.0) GO TO 50
C                                       was input SN table requested?
            IF (INPSN) THEN
C                                       get gains from SN table
C                                       [NEED CODE HERE!]

C                                       NEED TO EXTRACT GAIN FROM EVERY NODE
CC                                       extract first node gain from other
CC                                       nodes
C               DO 75 L = 2, MSC
C                  DO 74 M = 1, MSP
C                     DO 73 J = 1, NIF*MST
C                        LOCMAR(J,M,L) = LOCMAR(J,M,L) / LOCMAR(J,M,1)
C 73                     CONTINUE
C 74                  CONTINUE
C 75               CONTINUE
CC                                       extract sensitivity from first node
C               MEP = PGOB(1)
C               IF ((MEP.GT.0).AND.(MSC.GT.0)) THEN
C                  DO 85 M = 1, MSP
C                     DO 84 J = 1, NIF*MST
C                        K = (M-1)*NIF*MST + J-1
C                        RTEMP = EXP(-PGOB(MEP-K))
C                        LOCMAR(J,M,1) = LOCMAR(J,M,1) * RTEMP
C 84                     CONTINUE
C 85                  CONTINUE
C                  END IF
               END IF
C                                       Fill OLDPLS
            TSTMP = TSMIN + T
            IF (MSC.GT.0)
     *      CALL SGET ('WRITE', OLDPLS, TSTMP, MCLP, LOCMAR, SLOCAM)
 50         CONTINUE
C                                       delete sn table object
         IF (INPSN) CALL TABDES (SNTAB, KRET)
         END IF
      IF (OP(1:4).EQ.'SCLO') THEN
C                                       output SN table requested?
         CALL OGET (UVDATA, 'OUTSN', TYPE, DIM, OUTSN, CDUM, KRET)
         IF (KRET.NE.0) GO TO 991
         IF (.NOT.OUTSN) GO TO 890
C                                       max #nodes exceeded?
         IF (MSC.GT.25) GO TO 890
C                                       get output SN table name
         CALL OGET (UVDATA, 'OSNTAB', TYPE, DIM, IDUM, SNTAB, KRET)
         IF (KRET.NE.0) GO TO 991
C                                       init SN table header
         NUMANT = MSP
         NUMPOL = MST
         NUMIF  = NIF
         GMMOD  = 1.0
         IF (MSC.GT.0) THEN
C                                       fill in interpolation node coordinates
            CALL RFILL (MSC, 0.0, RANOD)
            CALL RFILL (MSC, 0.0, DECNOD)
            NUMNOD = MSC-1
C         DO I = 1, MSC
C            CALL MODELD (I, 'SKYPOS', RA, DEC,
C    *          SGOBA, SOUT, COUT, UOUT)
C            SNRA(I) = RA
C            SNDEC(I) = DEC
C            CONTINUE
         ELSE
C                                       just need SN for sensitivities...
            RANOD(1) = 0.0
            DECNOD(1) = 0.0
            NUMNOD = 0
            END IF
         ISAPPL   = FALSE
C                                       init SN table
         CALL OSNINI (SNTAB, 'WRIT', SNROW, NUMANT, NUMPOL, NUMIF,
     *      NUMNOD, GMMOD, RANOD, DECNOD, ISAPPL, KRET)
         IF (KRET.NE.0) GO TO 991
C                                       add OMFIT keyword to header
         KEYS(1)  = 'OMFIT   '
         NKEYS    = 1
         KTYPE(1) = 2
C                                       this is attrocious!
C                                       should use NWDPFP as the
C                                       first arg in this COPY.
         CALL COPY (1, TSOLVE, KVALS(1))
         KLOCS(1) = 1
         CALL TABKPT (SNTAB, KEYS, NKEYS, KLOCS, KVALS, KTYPE, KRET)
         IF (KRET.NE.0) GO TO 991
C                                       prepare time independent parts
C                                       of SN table rows
         SOURID = 0
         SUBA   = 1
         FREQID = 1
         IFR    = 0.0
C                                       rewind scratch file,
         IF (MSC.GT.0) REWIND (OLDPLS)
C                                        loop over times
         DO 300 T = 1,MTT
C                                        check degrees of freedom
            IF (DOF(T).LT.0) GO TO 300
C                                       construct time information
            TSTMP = TSMIN + T
            TIMEI = TSOLVE
            TIME  = TSOLVE * (TSTMP + 0.5)
C
C                                       get gain solutions from scratch file
            IF (MSC.GT.0) THEN
               CALL SGET ('READ', OLDPLS, TSTMP, MCLP, LOCMAR, SLOCAM)
C
C                                       FOR EACH stokes,
C                                       Find antenna with min variance and
C                                        make it the reference antenna
               DO 390 P = 1, MST
                  REFA = 0
                  RTEMP1 = 0.0
                  DO 380 Q = 1, NIF
                     DO 320 L = 1, MSP
                        IF (TSFLGM(L,T)) GO TO 320
                        RTEMP = 0.0
                        NTEMP = 0
                        DO 310 J = 1, MSC
                           IF (SLOCAM(P,Q,L,J).GT.-0.25) THEN
                              RTEMP = RTEMP + SLOCAM(P,Q,L,J)
                              NTEMP = NTEMP + 1
                              END IF
 310                    CONTINUE
                        IF (NTEMP.GT.0) THEN
                           RTEMP = RTEMP / NTEMP
                           IF ((REFA.EQ.0).OR.(RTEMP.LT.RTEMP1)) THEN
                              REFA = L
                              RTEMP1 = RTEMP
                              END IF
                           END IF
 320                    CONTINUE
 380                 CONTINUE
                  CREF(P,1) = REFA
 390              CONTINUE

               END IF
C                                       loop over good antennas
            DO 240 L = 1, MSP
               IF (TSFLGM(L,T)) GO TO 240
C                                       Loop over nodes
               K = 1
 230           CONTINUE
C                                       loop over stokes
                  DO 221 P = 1, MST
C                                       loop over IFs
                     DO 220 Q = 1, NIF
                        CREF(P,Q) = CREF(P,1)
C                                       INIT SOLUTION
                        CDELY(P,Q) = 0.0
                        CRATE(P,Q) = 0.0
                        IF (MSC.GT.0) THEN
                           CVIS     = LOCMAR(P,Q,L,K)
                           CWT(P,Q) = SLOCAM(P,Q,L,K)
                        ELSE
                           CVIS = CONE
                           CWT(P,Q) = 0.0
                           END IF
C                                       TOSS IN SENSITIVITY
                        IF (MEP.GT.0) THEN
                           RTEMP = EXP(PGOB(P,Q,L))
                           CVIS = CVIS * RTEMP
C                                       TOTALLY AD HOC! [just to
C                                       get out of the hole that
C                                       self-cal may have dug...]
                           CWT(P,Q) = CWT(P,Q) + 1.0
                           END IF
C                                       SET SELF-REFERENCED IF NO SELF-CAL
                        IF (MSC.EQ.0) CREF(P,Q) = L
C                                       invert self-cal gain amplitude
                        ISBAD = (CABS(CVIS).LT.1.0E-10)
CKTEST     *                      .OR.(CWT(P,Q)  .LT.1.0E-10)
     *                      .OR.(CREF(P,Q) .EQ.0)
                        IF (ISBAD) THEN
                           CREAL(P,Q) = FBLANK
                           CIMAG(P,Q) = FBLANK
                           CRATE(P,Q) = FBLANK
                           CDELY(P,Q) = FBLANK
                           CWT(P,Q) = 0.0
                        ELSE
                           CREAL(P,Q) =   REAL(CONE/CVIS)
                           CIMAG(P,Q) = -AIMAG(CONE/CVIS)
                           CWT(P,Q) = 1.0D0 / SQRT ( CWT(P,Q) )
                           END IF
 220                    CONTINUE
 221                 CONTINUE
                  I = K - 1
                  CALL OTABSN (SNTAB, 'WRIT', SNROW, NUMPOL, TIME,
     *               TIMEI, SOURID, L, SUBA, FREQID, IFR, I, MBDELY,
     *               DISP, DDISP, CREAL, CIMAG, CDELY, CRATE, CWT, CREF,
     *               KRET)
                  IF (KRET.NE.0) GO TO 990
                  IF (K.LT.MSC) THEN
                     K = K + 1
                     GO TO 230
                     END IF
 240           CONTINUE

 300        CONTINUE
C                                       CLOSE SN TABLE
         CALL OTABSN (SNTAB, 'CLOS', SNROW, NUMPOL, TIME, TIMEI, SOURID,
     *      I, SUBA, FREQID, IFR, K, MBDELY, DISP, DDISP, CREAL, CIMAG,
     *      CDELY, CRATE, CWT, CREF, KRET)
         IF (KRET.NE.0) GO TO 990
C                                       delete SN table object
         CALL TABDES (SNTAB, KRET)
C                                       CLOSE SCRATCH FILES
 890     CONTINUE
         CLOSE (OLDPLS, STATUS='DELETE')
         CLOSE (NEWPLS, STATUS='DELETE')
      END IF
      GO TO 990
 991  CONTINUE
C                                       trap errors here
 990  CONTINUE
 1900 FORMAT ('OMFIT: Sorry, one of the SN keywords failed to match')
 1901 FORMAT ('       ignoring requested input SN table...')
      RETURN
      END
      SUBROUTINE SGET (OP, NUNIT, TSTAMP, NLOC, PLOC, SLOC)
C-----------------------------------------------------------------------
C   SGET is a accesses scratch files to get/put local parameters
C   Input
C      OP      C*(*)     'READ', 'WRITE'
C      NUNIT   I         LUN of file to access
C      TSTAMP  I         for OP='WRITE', TSTAMP is the timestamp of the
C                        solutions to write
C                        for OP='READ', TSTAMP is the timestamp of the
C                        solutions being read
C      NLOC    I         actual number of local parameters
C      PLOC    CX(*)     local parameters
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER OP*(*)
      INTEGER NUNIT, TSTAMP
      INTEGER NLOC
      COMPLEX PLOC(*)
      DOUBLE PRECISION SLOC(*)
C                                       no includes
C                                       internal variables
      INTEGER L, ITIME
C-----------------------------------------------------------------------
      IF (OP.EQ.'WRITE') THEN
         WRITE (NUNIT) TSTAMP, (PLOC(L), SLOC(L), L=1,NLOC)
         END IF
C
      IF (OP.EQ.'READ') THEN
 200     CONTINUE
C                                       Get local parameters
         READ (NUNIT, END=210) ITIME, (PLOC(L), SLOC(L), L=1,NLOC)
C                                       If this timestamp is too early,
C                                       try again
         IF (ITIME.LT.TSTAMP) GO TO 200
C                                       otherwise accept this timestamp
 210     IF (ITIME.GE.TSTAMP) TSTAMP = ITIME
C                                       Done
         END IF
      RETURN
      END
      SUBROUTINE EDCODE (OP, RA, DEC)
C-----------------------------------------------------------------------
      CHARACTER OP*(*)
      REAL      RA, DEC
C-----------------------------------------------------------------------
      IF (OP.EQ.'To-Prog') THEN
         RA  = RA  + (60.0-1000.0) * INT ( RA  / 1000.0 + 0.01 )
         DEC = DEC + (60.0-1000.0) * INT ( DEC / 1000.0 + 0.01 )
      ELSE IF (OP.EQ.'TO-User') THEN
         RA  = RA  + (1000.0-60.0) * INT ( RA  / 60.0 + 0.01 )
         DEC = DEC + (1000.0-60.0) * INT ( DEC / 60.0 + 0.01 )
         END IF
C
      RETURN
      END
      SUBROUTINE VPPLUS (N, VA, VB)
C-----------------------------------------------------------------------
C   VPPLUS performs a vector addition of N members of VB into VA
C   Input:
C      N      I       Number of vector elements to operate on
C      VA     R(*)    Array of reals
C      VB     R(*)    Array of reals
C   Output:
C      VA     R(*)    Array of reals containing vector sum of VA and VB
C-----------------------------------------------------------------------
      DOUBLE PRECISION VA(*), VB(*)
      INTEGER N
C
      INTEGER I
C-----------------------------------------------------------------------
      IF (N.GT.0) THEN
         DO 10 I = 1,N
            VA(I) = VA(I) + VB(I)
 10         CONTINUE
         END IF
      RETURN
      END
      SUBROUTINE TOSCRN (UVSCR, NORVEC, CGOBA, XGOBA, XGOBG, SGOBA,
     *   SGOBG, LGOBA, LGOBG)
C-----------------------------------------------------------------------
C   TOSCRN prints out virtually ALL messages to the user, PRTLEV
C     controls which sections are printed out.
C   Input:
C      OP   C*(*) control code determines which messages are printed to
C                 the user on a particular invocation
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER UVSCR*(*)
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'FIT0.INC'
      DOUBLE PRECISION XGOBA(MXP), XGOBG(MEP), SGOBA(MGP), SGOBG(MEP)
      LOGICAL LGOBA(MGP), LGOBG(MEP)
      CHARACTER CGOBA(MCP)*8, OTFILE*48
      DOUBLE PRECISION NORVEC(5,MST,NIF,MSP,MSC)
      INCLUDE 'LINFO.INC'
      INCLUDE 'KINFO.INC'
      INCLUDE 'IINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'SINFO.INC'
      INCLUDE 'ZINFO.INC'
C                                       internal variables
      INTEGER I, J, L, NLUN, ILUN, K, M, JJ
      INTEGER TYPE(7), DIM, IDUM
      LOGICAL TRUE, FALSE
C                                       length of MSGS is a 'MAGIC' number
      CHARACTER PRGDEV*48, STKST1*16, STKST2*16, TXLINE*132
      INTEGER IRET, JT, JTRIM, JTE
      COMMON /INTP/ PRGDEV
C     slick variables
C                                       HERE ARE 'MAGIC' NUMBERS!!
      DOUBLE PRECISION POUT(256), SOUT(256)
      CHARACTER COUT(256)*8, UOUT(256)*8, COMPID*8, CITER*11
      DATA TRUE,FALSE /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                     print to screen
      WRITE (MSGTXT,2249)
      CALL MSGWRT (3)
      IF (ITER.EQ.0) THEN
         WRITE (MSGTXT,2250)
      ELSE IF (ITER.EQ.NITER) THEN
         WRITE (MSGTXT,2253)
      ELSE
         WRITE (MSGTXT,2252) ITER
      END IF
      CALL MSGWRT (3)
      CALL OGET (UVSCR, 'OUTFILE', TYPE, DIM, IDUM, OTFILE, IRET)
      IF ((LITER).AND.(OTFILE(1:1).NE.' ').AND.(IRET.EQ.0)) THEN
         NLUN = IND
         MSGSUP = 32000
         CALL ZTXOPN ('QWRT', NLUN, ILUN, OTFILE, TRUE, IRET)
         MSGSUP = 0
         IF (IRET.NE.0) THEN
            CALL ZTXCLS (NLUN, ILUN, IRET)
            LITER = FALSE
         END IF
         WRITE (CITER,2261) ITER
      END IF
      L = 0
      M = 1
C                                     print sky model to screen
      DO 20 I = 1,MCP
         WRITE (MSGTXT,2249)
         CALL MSGWRT (3)
         COMPID = CGOBA(I)
         J = INT(XGOBA(M+2))
         WRITE (MSGTXT,2255) I, COMPID, J
         CALL MSGWRT (3)
         WRITE (MSGTXT,2260)
         CALL MSGWRT (3)
         CALL MODELD (COMPID, 'To-User', XGOBA(M+5), POUT,
     *        SGOBA(L+1), SOUT, COUT, UOUT)
         JJ = INT (XGOBA(M+1))
         DO 10 J = 1,JJ
            IF ((UOUT(J)(1:2).EQ.'Jy').AND.
     *         ((ABS(POUT(J)).LT.0.001D0).OR.
     *         (SOUT(J).LT.0.001D0))) THEN
               IF ((ABS(POUT(J)).LT.0.000001D0) .OR.
     *            (SOUT(J).LT.0.000001D0)) THEN
                  UOUT(J) = 'uJy'
                  POUT(J) = POUT(J) * 1000000.0D0
                  SOUT(J) = SOUT(J) * 1000000.0D0
               ELSE
                  UOUT(J) = 'mJy'
                  POUT(J) = POUT(J) * 1000.0D0
                  SOUT(J) = SOUT(J) * 1000.0D0
                  END IF
               END IF
            SOUT(J) = SOUT(J) * FTOS
            IF (RNOISE.GT.0.0) SOUT(J) = SOUT(J) / RNOISE
            IF (LGOBA(L+J)) THEN
C  MFB omft9 adds the 1.414.. below
C               WRITE (MSGTXT,2365) COUT(J), UOUT(J), POUT(J),
C     *            1.4142135*SOUT(J)
               WRITE (MSGTXT,2365) COUT(J), UOUT(J), POUT(J),
     *            SOUT(J)
            ELSE
               WRITE (MSGTXT,2364) COUT(J), UOUT(J), POUT(J)
               END IF
            CALL MSGWRT (3)
            IF (LITER) THEN
               WRITE (TXLINE,2262) CITER, MSGTXT
               JT = JTRIM (TXLINE)
               CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), JTE)
               END IF
 10         CONTINUE
         L = L + XGOBA(M+1)
         M = M + XGOBA(M)
 20      CONTINUE
C                                     print gains to screen
      IF (MEP.GT.0) THEN
         WRITE (MSGTXT,2249)
         CALL MSGWRT (3)
         IF (MST.EQ.1) WRITE (MSGTXT,2270)
         IF (MST.EQ.2) WRITE (MSGTXT,2271)
         CALL MSGWRT (3)
         COMPID = 'GAINS'
         CALL MODELD ('GAINS', 'To-User', XGOBG, POUT,
     *        SGOBG, SOUT, COUT, UOUT)
C                                     POUT, SOUT ARE LINEARLY
C                                     INDEXED WITH THE FOLLOWING
C                                     'FORTRAN' ORDER:
C                                     [P,S]OUT(MST,NIF,MSP)
         DO 110 L = 1, MST*NIF*MSP
            SOUT(L) = SOUT(L) * FTOS
            IF (RNOISE.GT.0.0) SOUT(L) = SOUT(L) / RNOISE
 110     CONTINUE
         DO 129 L = 1,MSP
            DO 128 M = 1, NIF
               J = (L-1)*MST*NIF + (M-1)*MST
C                                     DO FIRST STOKES
               IF (LGOBG(1+J)) THEN
                  WRITE (STKST1, 8295) POUT(1+J), SOUT(1+J)
               ELSE
                  WRITE (STKST1, 8294) POUT(1+J)
                  END IF
C                                     DO SECOND STOKES IF NECESSARY
               IF (MST.GT.1) THEN
                  IF (LGOBG(2+J)) THEN
                     WRITE (STKST2, 8295) POUT(2+J),SOUT(2+J)
                  ELSE
                     WRITE (STKST2, 8294) POUT(2+J)
                     END IF
               ELSE
                  WRITE (STKST2, 8296)
                  END IF
C                                     only label the first IF
               IF (M.EQ.1) THEN
                  WRITE (MSGTXT,8291) L, ANTNAM(L),M,STKST1,STKST2
               ELSE
                  WRITE (MSGTXT,8292) M,STKST1,STKST2
                  END IF
               CALL MSGWRT (3)
               IF (LITER) THEN
                  WRITE (TXLINE,2262) CITER, MSGTXT
                  JT = JTRIM (TXLINE)
                  CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), JTE)
                  END IF
 128           CONTINUE
 129        CONTINUE
         END IF
C                                     print self-cal quants to screen
      IF (.NOT.SIMLOC) THEN
         WRITE (MSGTXT,2249)
         CALL MSGWRT (3)
         WRITE (MSGTXT,2251)
         CALL MSGWRT (3)
         WRITE (MSGTXT,2249)
         CALL MSGWRT (3)
C                                     print self-cal quants to screen
         DO 260 J = 1,MSC
            IF (MST.EQ.1) THEN
               WRITE (MSGTXT,2280)
            ELSE
               WRITE (MSGTXT,2281)
               END IF
            CALL MSGWRT (3)
C                                     prep all Stokes/IFs
            DO 230 I = 1 , MSP
               DO 220 L = 1,NIF
                  DO 210 K = 1, MST
C      WRITE (*,*) '--',K,L,I,J, NORVEC(1,K,L,I,J),NORVEC(2,K,L,I,J),
C     *  NORVEC(3,K,L,I,J),NORVEC(4,K,L,I,J),NORVEC(5,K,L,I,J)
                     IF (NORVEC(3,K,L,I,J).GT.0.5) THEN
                        NORVEC(1,K,L,I,J) = NORVEC(1,K,L,I,J)
     *                       / NORVEC(3,K,L,I,J)
                        NORVEC(2,K,L,I,J) = NORVEC(2,K,L,I,J)
     *                       / NORVEC(3,K,L,I,J)

                        NORVEC(2,K,L,I,J) = SQRT(ABS(
     *                       NORVEC(2,K,L,I,J)-NORVEC(1,K,L,I,J)**2 ))
                        NORVEC(1,K,L,I,J) = NORVEC(1,K,L,I,J) +1.D0
C                       NORVEC(2,K,L,I,J) = NORVEC(1,K,L,I,J) /
C    *                       (NORVEC(2,K,L,I,J)+1.0D-9)
                     ELSE
                        NORVEC(1,K,L,I,J) = 1.0D0
                        NORVEC(2,K,L,I,J) = 0.0D0
                     END IF
C                                     here, do the sigmas.
                     IF (NORVEC(5,K,L,I,J).GT.0.5) THEN
                        NORVEC(4,K,L,I,J) = NORVEC(4,K,L,I,J)
     *                       / NORVEC(5,K,L,I,J)
                        NORVEC(4,K,L,I,J) = SQRT(NORVEC(4,K,L,I,J))
                     ELSE
                        NORVEC(4,K,L,I,J) = 0.0D0
                     END IF
C      WRITE (*,*) '++',K,L,I,J, NORVEC(1,K,L,I,J),
C     *   NORVEC(2,K,L,I,J),NORVEC(3,K,L,I,J),NORVEC(4,K,L,I,J)
 210              CONTINUE
 220           CONTINUE
 230        CONTINUE
C                                     outer, outer loop is over components
C                                     Outer loop is over stations
            DO 250 I = 1, MSP
C                                     do first IF
               IF (MST.EQ.1) THEN
                  WRITE (MSGTXT,2300) I, ANTNAM(I), J, BIF,
C     *  NORVEC(1,1,1,I,J), NORVEC(2,1,1,I,J), NORVEC(4,1,1,I,J)
     *  NORVEC(1,1,1,I,J), NORVEC(4,1,1,I,J)
               ELSE
                  WRITE (MSGTXT,2300) I, ANTNAM(I), J, BIF,
C     *  NORVEC(1,1,1,I,J), NORVEC(2,1,1,I,J), NORVEC(4,1,1,I,J),
C     *  NORVEC(1,2,1,I,J), NORVEC(2,2,1,I,J), NORVEC(4,2,1,I,J)
     *  NORVEC(1,1,1,I,J), NORVEC(4,1,1,I,J),
     *  NORVEC(1,2,1,I,J), NORVEC(4,2,1,I,J)
                  END IF
               CALL MSGWRT (3)
               IF (LITER) THEN
                  WRITE (TXLINE, 2262) CITER, MSGTXT
                  JT = JTRIM (TXLINE)
                  CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), JTE)
                  END IF
C                                     do rest
               DO 240 L = 2, NIF
                  IF (MST.EQ.1) THEN
                     WRITE (MSGTXT,2310) BIF+L-1,
C     *  NORVEC(1,1,L,I,J), NORVEC(2,1,L,I,J), NORVEC(4,1,L,I,J)
     *  NORVEC(1,1,L,I,J), NORVEC(4,1,L,I,J)
                  ELSE
                     WRITE (MSGTXT,2310) BIF+L-1,
C     *  NORVEC(1,1,L,I,J), NORVEC(2,1,L,I,J), NORVEC(4,1,L,I,J),
C     *  NORVEC(1,2,L,I,J), NORVEC(2,2,L,I,J), NORVEC(4,2,L,I,J)
     *  NORVEC(1,1,L,I,J), NORVEC(4,1,L,I,J),
     *  NORVEC(1,2,L,I,J), NORVEC(4,2,L,I,J)
                     END IF
                  CALL MSGWRT (3)
                  IF (LITER) THEN
                     WRITE (TXLINE,2262) CITER, MSGTXT
                     JT = JTRIM (TXLINE)
                     CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), JTE)
                     END IF
 240              CONTINUE
 250           CONTINUE
 260        CONTINUE
         END IF
      WRITE (MSGTXT,2249)
      CALL MSGWRT (3)
      IF (LITER) CALL ZTXCLS (NLUN, ILUN, IRET)
C
 999  RETURN
C-----------------------------------------------------------------------
C                                       for Show-Global-Pars
 2249 FORMAT ('-------------------------------------------------')
 2251 FORMAT ('  -------- Self-Calibration Statistics --------')
 2250 FORMAT ('------------ The Initial MODEL is ---------------')
 2252 FORMAT ('------- The MODEL after ',I3,' iterations -------')
 2253 FORMAT ('============== The Final MODEL is ===============')
 2255 FORMAT ('Component #',I3,' ModelType <',A,'> Ident. #',I3)
 2260 FORMAT ('Parameter(units)',9X,'Value',7X,'Uncertainty')
 2261 FORMAT ('! Iter:',I4)
 2262 FORMAT (A11,A121)
 2364 FORMAT (1X,A8,1X,'(',A8,')',1(1X,F13.5), '         fixed ')
 2365 FORMAT (1X,A8,1X,'(',A8,')',2(1X,F13.5))
 2270 FORMAT ('Ant Name      IF   Value Error')
 2271 FORMAT ('Ant Name      IF   Value   Error   Value   Error')
 2280 FORMAT ('Ant Name   Node IF   Value   Error')
 2281 FORMAT ('Ant Name   Node IF   Value   Error   Value   Error')
 2300 FORMAT (I3,1X,A8,1X,I2,1X,I2,6(1X,F7.3))
 2310 FORMAT (3X,1X,8X,1X,2X,1X,I2,6(1X,F7.3))
C
 8296 FORMAT (16(' '))
 8295 FORMAT (1X,F7.3,1X,F7.3)
 8294 FORMAT (1X,F7.3,1X,' fixed ')
 8292 FORMAT (3X,1X,8X,1X,I3,A16,A16)
 8291 FORMAT (I3,1X,A8,1X,I3,A16,A16)
      END
      SUBROUTINE TOFILE (UVSCR, GLOMAT, CGOBA, XGOBA, SGOBA, LGOBA,
     *   XGOBG, SGOBG, LGOBG)
C-----------------------------------------------------------------------
C   TOFILE prints out virtually ALL messages to the user, PRTLEV
C     controls which sections are printed out.
C   Input:
C      OP   C*(*) control code determines which messages are printed to
C                 the user on a particular invocation
C-----------------------------------------------------------------------
C                                       i/o variables
      CHARACTER UVSCR*(*)
C                                       includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'FIT0.INC'
      DOUBLE PRECISION GLOMAT(MGP, MGP), TCHI2
      DOUBLE PRECISION XGOBA(MXP), XGOBG(MEP), SGOBA(MGP), SGOBG(MEP)
      LOGICAL LGOBA(MGP), LGOBG(MEP)
      CHARACTER CGOBA(MCP)*8, OTFILE*48, INFILE*48, DNAME*48
      INCLUDE 'LINFO.INC'
      INCLUDE 'KINFO.INC'
      INCLUDE 'IINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'SINFO.INC'
      INCLUDE 'ZINFO.INC'
C                                       internal variables
      INTEGER   I, J, L, NLUN, ILUN, K, M, N, P, Q
      DOUBLE PRECISION TEMP
      INTEGER   TYPE(7), DIM, IDUM, ITRIM
      LOGICAL   TRUE
C                                       length of MSGS is a 'MAGIC' number
      CHARACTER PRGDEV*48, TXLINE*256
      INTEGER   IRET, JT, JTRIM
      COMMON /INTP/ PRGDEV
C     slick variables
C                                       HERE ARE 'MAGIC' NUMBERS!!
      DOUBLE PRECISION POUT(256), SOUT(256)
      CHARACTER COUT(256)*8, UOUT(256)*8, COMPID*8
      DATA TRUE /.TRUE./
C-----------------------------------------------------------------------
      CALL OGET (UVSCR, 'OUTFILE', TYPE, DIM, IDUM, OTFILE, IRET)
      IF ((IRET.NE.0).OR.(OTFILE(1:1).EQ.' ')) GO TO 990
      CALL OGET (UVSCR, 'INFILE', TYPE, DIM, IDUM, INFILE, IRET)
      CALL OGET (UVSCR, 'DNAME', TYPE, DIM, IDUM, DNAME, IRET)
      NLUN = IND
      MSGSUP = 32000
      CALL ZTXOPN ('QWRT', NLUN, ILUN, OTFILE, TRUE, IRET)
      MSGSUP = 0
C                                     header info for posterity
      WRITE (TXLINE,4000)
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,14008) INFILE
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,14009) DNAME
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      IF (TSOLVE.GT.1.0) THEN
         WRITE (TXLINE,14010) SOLMOD, TSOLVE, 'days', ITER, NITER
      ELSE
         TEMP = TSOLVE * 86400.0
         WRITE (TXLINE,14010) SOLMOD, TEMP, 'seconds', ITER, NITER
         END IF
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,14011) BEGCH, BIF, SNVERI
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,14012) ENDCH, EIF, SNVERO
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,14013) BRANGE(1), BRANGE(2), STCODE
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                     add a blank line
      WRITE (TXLINE,4040)
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C
      TCHI2 = PRCHI2 * PNFREE
      IF (PRCHI2.LT.1.0) THEN
         WRITE (TXLINE,4020) TCHI2, PRCHI2, PNFREE
      ELSE IF (PRCHI2.LT.100.0) THEN
         WRITE (TXLINE,4021) TCHI2, PRCHI2, PNFREE
      ELSE
         WRITE (TXLINE,4022) LOG10(TCHI2), LOG10(PRCHI2), PNFREE
         END IF
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4023) RNOISE, PNCHI2, PMNOIZ
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4032) SUMCOR, SUMGP, SUMLP
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4035) SUMCOR, MST, NIF, NCHNS
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4033) SUMGPS, SUMLPS
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
C                                     write blank line
      WRITE (TXLINE,4040)
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4037) GNORM, GNFRAC
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      WRITE (TXLINE,4036) DELRMS, C2FRAC
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      IF (LEXPLA) THEN
         WRITE (TXLINE,4040)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4042)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4043)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4044)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4045)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         END IF
      K = 1
      M = 1
      DO 789 I = 1,MCP
         L = K + XGOBA(M+1) - 1
C                                     get ancillary info for printing
         CALL MODELD(CGOBA(I), 'To-User', XGOBA(M+5), POUT,
     *        SGOBA(K), SOUT, COUT, UOUT)
C                                     write blank line
         WRITE (TXLINE,4040)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C
C                                     MAGIC!!
C                                     modeltype string must be < 7 letters
C                                     K to L must span 10 or less pars
C
C                                     column headers [quantities]
         WRITE (TXLINE,4070) (COUT(J-K+1), J=K,L)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C                                     column subtitles [units]
         WRITE (TXLINE,4080) (UOUT(J-K+1), J=K,L)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C                                     format modeltype
         WRITE (COMPID, 4090) CGOBA(I)(1:ITRIM(CGOBA(I)))
C                                     model line
         N = INT(XGOBA(M+2))
         P = INT(XGOBA(M+3))
         Q = INT(XGOBA(M+4))
         WRITE (TXLINE,4100) COMPID, N, P, Q,
     *        (POUT(J-K+1),LGOBA(J), J=K,L)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C                                     [if noise estimate was given, quote
C                                     a formal error]
C                                     formal 1-sigma errors
         IF (RNOISE.GT.0.0) THEN
            WRITE (TXLINE,4110) (SOUT(J-K+1), J=K,L)
            JT = JTRIM (TXLINE)
            CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
            IF (IRET.NE.0) GO TO 990
C                                     also extract the a priori sigma
C                                     from the error bar
            DO 790 J = K,L
               SOUT(J-K+1) = SOUT(J-K+1) / RNOISE
 790           CONTINUE
            END IF
         DO 791 J = K,L
            SOUT(J-K+1) = SOUT(J-K+1) * FTOS
 791        CONTINUE
C                                     suggested 1-sigma errors
         WRITE (TXLINE,4111) (SOUT(J-K+1), J=K,L)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C
         K = K + XGOBA(M+1)
         M = M + XGOBA(M)
 789  CONTINUE
      IF (MEP.GT.0) THEN
C                                     title line for this section
         WRITE (TXLINE,4120)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
C                                     send in the number of stations
         CALL MODELD('GAINS', 'To-User', XGOBG, POUT,
     *        SGOBG, SOUT, COUT, UOUT)
         DO 798 L = 1,MSP
            WRITE (TXLINE,4130) L
            DO 797 M = 1,NIF
               J = (L-1)*NIF*MST + (M-1)*MST
               JT = JTRIM (TXLINE)
               CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
               IF (IRET.NE.0) GO TO 990
               IF (MST.EQ.1) THEN
                  WRITE (TXLINE,4140) POUT(1+J), LGOBG(1+J)
               ELSE
                  WRITE (TXLINE,4141) POUT(1+J), LGOBG(1+J),
     *                 POUT(2+J), LGOBG(2+J)
                  END IF
 797           CONTINUE
            TXLINE = TXLINE(1:JTRIM(TXLINE))//'  /'
            JT = JTRIM (TXLINE)
            CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
            IF (IRET.NE.0) GO TO 990
C                                     do formal errors here
            WRITE (TXLINE,4131) L
            DO 799 M = 1,NIF
               J = (L-1)*MST*NIF + (M-1)*MST
               JT = JTRIM (TXLINE)
               CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
               IF (IRET.NE.0) GO TO 990
               IF (MST.EQ.1) THEN
                  IF (LGOBG(1+J)) THEN
                     WRITE (TXLINE,4142) SOUT(1+J)
                  ELSE
                     WRITE (TXLINE,4143)
                     END IF
               ELSE
                  IF (LGOBG(1+J).AND.LGOBA(2+J)) THEN
                     WRITE (TXLINE,4144) SOUT(1+J), SOUT(2+J)
                  ELSE IF (LGOBG(1+J)) THEN
                     WRITE (TXLINE,4145) SOUT(1+J)
                  ELSE IF (LGOBG(2+J)) THEN
                     WRITE (TXLINE,4146) SOUT(2+J)
                  ELSE
                     WRITE (TXLINE,4147)
                     END IF
                  END IF
 799           CONTINUE
            JT = JTRIM (TXLINE)
            CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
            IF (IRET.NE.0) GO TO 990
 798        CONTINUE
         END IF
C                                     ***MAGIC NUMBERS HERE****
      IF (LNOCOV.NE.2)  THEN
         DO 805 I = 1,MGP
            TEMP = ABS(GLOMAT(I,I))
            IF (TEMP.GT.0.0D0) TEMP = 1.0/SQRT(TEMP)
            DO 806 J = 1,MGP
               GLOMAT(I,J) = GLOMAT(I,J) * TEMP
               GLOMAT(J,I) = GLOMAT(J,I) * TEMP
 806        CONTINUE
 805     CONTINUE
         WRITE (TXLINE,4152)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4153)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         WRITE (TXLINE,4152)
         JT = JTRIM (TXLINE)
         CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
         IF (IRET.NE.0) GO TO 990
         IF (MGP.LE.16) THEN
            WRITE (TXLINE,4154) (I,I=1,MGP)
            DO 807 I = 1,MGP
               WRITE (TXLINE,4155) I,(GLOMAT(I,J),J=1,MGP)
               JT = JTRIM (TXLINE)
               CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
               IF (IRET.NE.0) GO TO 990
 807        CONTINUE
         ELSE IF ((MCP.EQ.1).OR.(LNOCOV.EQ.1)) THEN
            DO 809 I = 1,MGP
               DO 808 J = 1,MGP
                  WRITE (TXLINE,4160) I,J,GLOMAT(I,J)
                  JT = JTRIM (TXLINE)
                  CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
                  IF (IRET.NE.0) GO TO 990
 808              CONTINUE
 809           CONTINUE
            END IF
         END IF
      WRITE (TXLINE,4150)
      JT = JTRIM (TXLINE)
      CALL ZTXIO ('WRIT', NLUN, ILUN, TXLINE(:JT), IRET)
      IF (IRET.NE.0) GO TO 990
      CALL ZTXCLS (NLUN, ILUN, IRET)
C
 990  CONTINUE
      RETURN
C-----------------------------------------------------------------------
C                                       for Write-New-Model-File
 4000 FORMAT ('! This file was generated by OMFIT run with :')
14008 FORMAT ('! Input file = <',A48,'>')
14009 FORMAT ('! Data was on ',A48)
14010 FORMAT ('! SOLMOD = <',A3,'>   SOLINT = ',F10.3,' ',A,
     *   '  ITER = ',I4,' of ',I4,' requested')
14011 FORMAT ('! BCHAN = ',I6,'  BIF = ',I5,'   Input SN = ',I5,
     *   ' (-1 => none)')
14012 FORMAT ('! ECHAN = ',I6,'  EIF = ',I5,'  Output SN = ',I5,
     *   ' (-1 => none)')
14013 FORMAT ('! min Baseline = ',F10.3,' (klambdas)',
     *   ', max Baseline = ',F10.3,' (klambdas)',
     *   ', Selected Stokes = <',A4,'>')
C                                       for Show-ChiSq
 4020 FORMAT ('! Chi-Squared =',F19.6,', Reduced Chi-Squared =',F9.6,
     *        ' , D.o.F. = ',F10.0)
 4021 FORMAT ('! Chi-Squared =',F19.6,', Reduced Chi-Squared =',F5.1,
     *        ' , D.o.F. = ',F10.0)
 4022 FORMAT ('! Log10(Chi-Squared) =',F9.2,
     *        ' , Log10(Reduced Chi-Squared) =',F4.1,
     *        ' , D.o.F. = ',F10.0)
 4023 FORMAT ('! Noise:',
     *        ' assumed pre-fit (sigma)= ',F10.6,' Jy',
     *        ' , estimated post-fit (delta)= ',F10.6,' Jy',
     *        ' , naturally weighted map = ',F10.6,' mJy/Beam')
 4032 FORMAT ('! # complex visibilities = ',F12.0,
     *        ' , # time-independent parameters = ',F12.0,
     *        ' , # time-dependent parameters = ',F12.0)
 4035 FORMAT ('! # complex visibilities = ',F12.0,
     *   ' baselines X ',I3,' stokes  X ',I3,' IFs X ',I4,
     *        ' channels')
 4033 FORMAT ('! ',F5.0,' time-independent and ',
     *             F5.0,' time-dependent parameters were discarded')
 4037 FORMAT ('! On final iteration, GNORM =',F12.0,
     *   ' threshold requested was ',F12.0)
 4036 FORMAT ('! On final iteration, Delta Chi-Sq (%) =',F12.0,
     *   ' threshold requested was ',F12.0)
 4042 FORMAT ('! Chi-Squared and Reduced Chi-Squared were divided',
     *   ' by (sigma^2)')
 4043 FORMAT ('! Calculation of delta is independent of sigma')
 4044 FORMAT ('! Formal 1-sigma error bars were multiplied by sigma')
 4045 FORMAT ('! Suggested 1-sigma error bars were multiplied by delta')
 4040 FORMAT ('! ')
 4070 FORMAT ('! Model   ID    Channel   ',10(7X    ,A8,1X ))
 4080 FORMAT ('! type  number Begin  End ',10(6X,'(',A8,')'))
 4090 FORMAT ('''',A,'''')
 4100 FORMAT (A8,1X,I4,1X,I4,1X,I5,1X, 10(1X,F13.6,1X,'''',L1,''''))
 4110 FORMAT ('! formal 1-sigma:       ',  10(1X,F14.7,3X))
 4111 FORMAT ('! suggested 1-sigma:    ',  10(1X,F14.7,3X))
 4120 FORMAT ('!'/'! Time-independant gains:'/
     *   '!   1 card/antenna , 1 line/IF',
     *   ' , 1 (gain solve?) pair / (RR and/or LL)')
 4130 FORMAT ('''GAINS''',1X,I3,1X)
 4131 FORMAT ('! suggested 1-sigma for ant# ',1X,I3,1X)
 4140 FORMAT (11X,F10.4,5X,'''',L1,'''',2X)
 4141 FORMAT (11X,2(F10.4,5X,'''',L1,'''',2X))
 4142 FORMAT ('!',10X,F10.4)
 4143 FORMAT ('!',10X,'   fixed')
 4144 FORMAT ('!',10X,2(F10.4,10X))
 4145 FORMAT ('!',10X,F10.4,10X,'   fixed')
 4146 FORMAT ('!',10X,'   fixed  ',10X,F10.4)
 4147 FORMAT ('!',10X,'   fixed  ',10X,'   fixed')
 4150 FORMAT ('/ ! end of inputs'/'! Required Blank Line !')
 4152 FORMAT ('!')
 4153 FORMAT ('!',' Covariance Matric information follows:')
 4154 FORMAT ('!',5X,16(3X,I2,3X))
 4155 FORMAT ('!',2X,I3,16(2X,F6.3))
 4160 FORMAT ('!',2X,I3,2X,I3,2X,F6.3)
      END
      SUBROUTINE UVSTAT (UVDATA, JRET)
C-----------------------------------------------------------------------
C   UVSTAT determines the maximum and minimum baselines in the data
C   and the maximum and minimum times in the data for later use
C-----------------------------------------------------------------------
      CHARACTER UVDATA*(*)
      INTEGER JRET

      INTEGER TYPE, DIM(7), IDUM
      CHARACTER CDUM*2
      REAL RDUM

      INCLUDE 'LINFO.INC'
      INCLUDE 'VINFO.INC'
      INCLUDE 'ZINFO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'

      CHARACTER ANTAB*36
      PARAMETER (ANTAB = 'Antenna Table object')
      INTEGER TAXIS, UAXIS, VAXIS, WAXIS, MANNO, BAXIS, A1AXIS, A2AXIS,
     *   I, J
      REAL TMIN, TMAX
      DOUBLE PRECISION BLEN
C-----------------------------------------------------------------------
      JRET = 0
C                                       check sort order
      CALL OGET (UVDATA, 'UV_DESC.SORTORD', TYPE, DIM, IDUM, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      IF (CDUM.NE.'TB') THEN
         MSGTXT = 'Error: Data must be TB sorted!'
         CALL MSGWRT (3)
         JRET = 1
         GO TO 990
         END IF
C                                       check number of sources selected
C                                       If one or zero sources selected,
C                                       warn that multiple sources
C                                       selected and that source-id's
C                                       should be checked in the input
C                                       file. Check random parameter
C                                       existence
      CALL UVDFND (UVDATA, 1, 'SOURCE', IDUM, JRET)
      IF (PRTLEV.GT.2) THEN
         IF (IDUM.GE.0) THEN
            MSGTXT = '**** Multiple sources selected, check source'
            CALL MSGWRT (3)
            MSGTXT = '     id''s [second column] in INFILE carefully'
            CALL MSGWRT (3)
            END IF
         END IF
      IF ((JRET.NE.1).AND.(JRET.NE.0)) THEN
         JRET = 1
         GO TO 990
         END IF
      JRET = 0
C                                       get the maximum number of antennas
C                                       in the file
C                                       get MAXANT
C      IDUM = 1
C      CALL UV2TAB (UVDATA, ANTAB, 'AN', IDUM, JRET)
C      IF (JRET.NE.0) GO TO 990
C      CALL TABOPN (ANTAB, 'READ', JRET)
C      IF (JRET.NE.0) GO TO 990
C      CALL ANTNO (ANTAB, IDUM, JDUM, JRET)
C      IF (JRET.NE.0) GO TO 990
C      CALL TABCLO (ANTAB, JRET)
C      IF (JRET.NE.0) GO TO 990
C      MANNO = JDUM
C                                       save max antenna number
C      TYPE = 4
C      DIM(1) = 1
C      DIM(2) = 1
C      DIM(3) = 0
C      CALL OPUT (UVDATA, 'MAXANT', TYPE, DIM, JDUM, CDUM, JRET)
C      IF (JRET.NE.0) GO TO 990
C                                       open UVDATA object
      CALL OOPEN (UVDATA, 'READ', JRET)
      IF (JRET.NE.0) GO TO 990
C                                       get time random parameter location
      CALL UVDFND (UVDATA, 1, 'TIME1', TAXIS, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 1, 'UU-L', UAXIS, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 1, 'VV-L', VAXIS, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 1, 'WW-L', WAXIS, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL UVDFND (UVDATA, 1, 'BASELINE', BAXIS, JRET)
      IF (JRET.NE.0) THEN
         BAXIS = -1
         CALL UVDFND (UVDATA, 1, 'ANTENNA1', A1AXIS, JRET)
         IF (JRET.EQ.0) CALL UVDFND (UVDATA, 1, 'ANTENNA2', A2AXIS,
     *      JRET)
         END IF
      IF (JRET.NE.0) GO TO 990
      TMIN = 1.0
      TMAX = 0.0
      BMIN = 1.0E20
      BMAX = 0.0
C                                       loop over all data here
      MANNO = 0
 100  CONTINUE
         CALL UVREAD (UVDATA, HDRBUF, VISBUF, JRET)
         IF (JRET.LT.0) GO TO 200
         IF (JRET.GT.0) GO TO 990
C                                       accumulate statistics on this file
         TMIN = MIN ( TMIN, HDRBUF(TAXIS))
         TMAX = MAX ( TMAX, HDRBUF(TAXIS))
         BLEN = SQRT(HDRBUF(UAXIS)**2 + HDRBUF(VAXIS)**2)
         BMIN = MIN ( BMIN, BLEN)
         BMAX = MAX ( BMAX, BLEN)
         IF (BAXIS.GT.0) THEN
            I = MOD (HDRBUF(BAXIS) , 256.0) + 0.1
            J = INT (HDRBUF(BAXIS) / 256.0)
         ELSE
            I = HDRBUF(A1AXIS) + 0.1
            J = HDRBUF(A2AXIS) + 0.1
            END IF
         MANNO = MAX(MANNO,I,J)
         GO TO 100
 200  CONTINUE
      JRET = 0
C                                       DECIDE WHAT UV-units to use:
C                                       baselines come in in lambdas, decide
C                                       here what units to scale them to.
C
C                                       determine rescale factor in powers of 3
      TYPE = LOG10(BMAX)/3.0
      TYPE = MIN(TYPE,6)
C                                       SCALE divides all baseline lengths
      SCALE = 1000.0D0**TYPE
CKTEST 1-july-1999
      SCALE = SQRT (BMIN * BMAX)
C                                       convert baselines to internal units
      BMIN = BMIN / SCALE
      BMAX = BMAX / SCALE
C                                       implicit scaling helper
      NATSKY = SCALE * TWOPI
      MAS2RD = DG2RAD * DG2RAD * 1.0D-4 / TWOPI
C                                       NATSKY \approx 6.28e6
C                                       MASR2D \approx 1/206265e3
C
C                                       convert inverse-beam minimum size to internal units
      SIZMIN = ( PI / DSQRT(DLOG(4.0D0)) ) * SQRT ( BMIN / BMAX )
      SIZMIN = SIZMIN / 100.0D0
C
C                                       just remember that
C                                       baseline value * scale is in lambdas!!!
C
C                                       [ note that 206265 = RAD2DG*RAD2DG*20 PI ]
C
C                                       these are for the sky position offsets
C                                       remember that for
C                                       theta(max detectible offset in mas)
C                                       u( at shortest uv spacing in Mlambda)
C                                       theta * u =
C
      POSSK(2) = NATSKY * MAS2RD
      POSSK(1) = POSSK(2) * 1000.0D0
C                                       here are some mnemonics to help
C                                       with initial estimates for the sizes
C
C                                       theta(arcsec) * u(klambda at ref pt)  = scalefactor
C                                       where
C
C                                       modeltype   u ref pt        scalefactor
C                                       gauss       vis half power    91
C                                       disk        first vis null    240
C                                       ring        first vis null    150
C                                       sphere      first vis null    280
C                                       gauss       coherence scale   77.304
C
      DISSK(2) = NATSKY * MAS2RD / 2.0D0
      DISSK(1) = DISSK(2) * 1000.0D0
C                                       this is a natural log!!!
      FWHM(2) = NATSKY * MAS2RD / DSQRT(DLOG(256.0D0))
      FWHM(1) = FWHM(2) * 1000.0D0
C                                       FWHM(2) \aprox= 1/77.3
C
C                                       add statistics to UVDATA object
      TYPE = OOARE
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
      CALL OPUT (UVDATA, 'TMIN', TYPE, DIM, TMIN, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      CALL OPUT (UVDATA, 'TMAX', TYPE, DIM, TMAX, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      RDUM = BMIN
      CALL OPUT (UVDATA, 'BMIN', TYPE, DIM, RDUM, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      RDUM = BMAX
      CALL OPUT (UVDATA, 'BMAX', TYPE, DIM, RDUM, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      RDUM = SCALE
      CALL OPUT (UVDATA, 'UVSCALE', TYPE, DIM, RDUM, CDUM, JRET)
      IF (JRET.NE.0) GO TO 990
      TYPE = OOAINT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 0
C      WRITE (*,*) 'manno = ',MANNO
      CALL OPUT (UVDATA, 'MAXANT', TYPE, DIM, MANNO, CDUM, JRET)
C                                       close file
      CALL OCLOSE (UVDATA, JRET)
      IF (JRET.NE.0) GO TO 990
      GO TO 999
C                                       trap errors here
 990  CONTINUE
C                                       exit now
 999  RETURN
      END
      SUBROUTINE GTSOR (UVIN, SCHAR, ONEID, IERR)
C-----------------------------------------------------------------------
C   Find the source id that matches the name in SCHAR
C       -1 means no match was found
C       >0 means match was found
C-----------------------------------------------------------------------
      CHARACTER UVIN*(*), SCHAR*(*)
      INTEGER   ONEID(30), IERR
C
      INTEGER   DIM(7), TYPE, NROWS, NUMIF, FQID
      CHARACTER CDUM*2, CALCOD*4, SOUNAM*16
      INTEGER   JRET, I, IDSOU, SUROW, QUAL, ILOCSU
      INCLUDE 'INCS:PUVD.INC'
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, RAAPP,
     *   DECAPP, EPOCH, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      CHARACTER TMPTAB*36, VELTYP*8, VELDEF*8
      DATA TMPTAB /'temporary SU table'/
C-----------------------------------------------------------------------
      IERR = -1
      CALL UVDFND (UVIN, 1, 'SOURCE', ILOCSU, JRET)
C                                       not found
      IF (JRET.EQ.1) THEN
         JRET = 0
         GO TO 999
         END IF
      IF (JRET.NE.0) THEN
         MSGTXT = 'GTSOR ERROR FINDING SOURCE RANDOM PARAMETER'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       get #rows in source table
      I = 1
      CALL UV2TAB (UVIN, TMPTAB, 'SU', I, JRET)
      IF (JRET.NE.0) GO TO 999
      CALL OOPEN (TMPTAB, 'READ', JRET)
      CALL OGET (TMPTAB, 'NROW', TYPE, DIM, NROWS, CDUM, JRET)
      CALL OCLOSE (TMPTAB, JRET)
C                                       open for read
      CALL OSUINI (TMPTAB, 'READ', NUMIF, VELTYP, VELDEF, FQID, SUROW,
     *   JRET)
      IF (JRET.NE.0) GO TO 999
      I = 1
 100  IF (SUROW.LE.NROWS) THEN
         CALL OTABSU (TMPTAB, 'READ', SUROW, IDSOU, SOUNAM, QUAL,
     *      CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *      DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, JRET)
         IF (JRET.NE.0) THEN
            ONEID(I) = -1
            JRET = 0
            I = I - 1
            GO TO 110
            END IF
         IF (SOUNAM.EQ.SCHAR) THEN
            ONEID(I) = IDSOU
            I = I + 1
            IERR = 0
            END IF
         GO TO 100
         END IF
C
 110  CALL OTABSU (TMPTAB, 'CLOS', SUROW, IDSOU, SOUNAM, QUAL,
     *   CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO, EPOCH, RAAPP,
     *   DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA, PMDEC, JRET)
C
 999  RETURN
      END
      SUBROUTINE MODELS (MOD, XMOD, DXMOD)
C-----------------------------------------------------------------------
C   Input
C      MOD     C*(*)  model code
C   Output
C      XMOD    R(*)   scratch parameters for this component
C-----------------------------------------------------------------------
      CHARACTER MOD*(*)
      DOUBLE PRECISION XMOD(*), DXMOD(*)
      INTEGER I, J, IPAR, MPAR, SPAR, ID
C-----------------------------------------------------------------------
      IF (MOD(1:5).EQ.'GAINS') THEN
         I = XMOD(1)
         IF (I.GT.0) CALL VPPLUS (I, XMOD(2), DXMOD)
         GO TO 999
         END IF
      J = 1
      IF (MOD(1:1).EQ.'Y') J = 2
C                                       identify number of id slots ***MAGIC***
      ID = 5
C                                       extract number of model components
      I = XMOD(2)
C                                       first 5 slots identify model
      IPAR = 1
C                                       model begins in slot 6
      MPAR = IPAR+ID
C                                       model continues for I slots
      SPAR = MPAR+I
C                                       update model parameters
      CALL VPPLUS ( I, XMOD(MPAR), DXMOD)
C                                       update ancillary quantities
      IF (MOD(J:J+2).EQ.'DOT')  THEN
         CALL POINTS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+3).EQ.'CORE')  THEN
         CALL CORES (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+2).EQ.'POL')  THEN
         CALL STOKES (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+3).EQ.'DISK') THEN
         CALL DISKS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+2).EQ.'GAU')  THEN
         CALL GAUSSS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+3).EQ.'SPHR') THEN
         CALL SPHRSS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+3).EQ.'EXPT')  THEN
         CALL EXPTS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+3).EQ.'BLOB')  THEN
         CALL BLOBS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+2).EQ.'KOL')  THEN
         CALL KOLINS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+2).EQ.'MUK')  THEN
         CALL MUKOLS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(J:J+2).EQ.'MAS')  THEN
         CALL MASERS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(1:3).EQ.'ZEE') THEN
         CALL ZEMANS (XMOD(MPAR), XMOD(SPAR))
         END IF
      IF (MOD(1:3).EQ.'HAL') THEN
         CALL HALOSS (XMOD(MPAR), XMOD(SPAR))
         END IF
 999  RETURN
      END
      SUBROUTINE MODELN (MOD, NMOD, SMOD)
C-----------------------------------------------------------------------
C  Any model can be 'turned off' simply by commenting out the
C   relevant string comparison below - this will prevent the
C   program from parsing the appropriate lines in the INFILE!!!
C   Input
C      MOD     C*(*)  model code
C   Output
C      NMOD    I      number of parameters for this model type
C      SMOD    I      number of scratch parameters for this model type
C-----------------------------------------------------------------------
      CHARACTER MOD*(*)
      INTEGER NMOD, SMOD, I
C ********MAGIC NUMBER HERE IN THIS ROUTINE!!!!! **************
      CHARACTER*8 ALLMOD(200)
      DATA (ALLMOD(I),I=1,11) /   'YDOT    ', 'YCORE   ', 'YPOL    ',
     *     'YKOL    ', 'YMUK    ', 'YMAS    ', 'YDISK   ', 'YGAU    ',
     *     'YBLOB   ', 'YZEE    ', 'YHAL    '/
      DATA (ALLMOD(I),I=101,111) /'DOT     ', 'CORE    ', 'POL     ',
     *     'KOL     ', 'MUK     ', 'MAS     ', 'DISK    ', 'GAU     ',
     *     'BLOB    ', 'ZEE     ', 'HAL     '/
C-----------------------------------------------------------------------
      IF (SMOD.GT.0) THEN
         SMOD = MIN (SMOD,111)
         IF (SMOD.LT.101) SMOD = MIN (SMOD,11)
         MOD = ALLMOD(SMOD)
         END IF
      I = 1
      IF (MOD(1:1).EQ.'Y') I = 2

      IF (MOD(1:5).EQ.'GAINS') THEN
         NMOD = 0
         SMOD = 0
         END IF
      IF (MOD(I:I+2).EQ.'DOT')  CALL POINTN (NMOD, SMOD)
      IF (MOD(I:I+3).EQ.'CORE')  CALL COREN (NMOD, SMOD)
      IF (MOD(I:I+2).EQ.'POL')  CALL STOKEN (NMOD, SMOD)
      IF (MOD(I:I+2).EQ.'KOL')  CALL KOLINN (NMOD, SMOD)
      IF (MOD(I:I+2).EQ.'MUK')  CALL MUKOLN (NMOD, SMOD)
      IF (MOD(I:I+2).EQ.'MAS')  CALL MASERN (NMOD, SMOD)
      IF (MOD(I:I+3).EQ.'DISK') CALL DISKN (NMOD, SMOD)
      IF (MOD(I:I+2).EQ.'GAU')  CALL GAUSSN (NMOD, SMOD)
      IF (MOD(I:I+3).EQ.'SPHR') CALL SPHRSN (NMOD, SMOD)
      IF (MOD(I:I+3).EQ.'EXPT')  CALL EXPTN (NMOD, SMOD)
      IF (MOD(I:I+3).EQ.'BLOB')  CALL BLOBN (NMOD, SMOD)

      IF (MOD(1:3).EQ.'ZEE')  CALL ZEMANN (NMOD, SMOD)
      IF (MOD(1:3).EQ.'HAL')  CALL HALOSN (NMOD, SMOD)
      RETURN
      END
      SUBROUTINE MODELD (MOD, OP, PPMOD, UPMOD, PEMOD, UEMOD, CMOD,
     *   UMOD)
C-----------------------------------------------------------------------
C   Input
C      MOD     C*(*)  model code
C      OP      C*(*)  operation to perform
C   Input/Output
C      PPMOD   R(*)   Model parameters in internal format
C      PEMOD   R(*)   Model errors in internal format
C      UPMOD   R(*)   Model parameters in external format
C      UEMOD   R(*)   Model errors in external format
C   Output
C      CMOD    C*(*)  string containing name of parameters
C      UMOD    C*(*)  string containing units of parameters
C-----------------------------------------------------------------------
      CHARACTER MOD*(*), OP*(*), CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER   I, J
C-----------------------------------------------------------------------
      I = 1
      IF (MOD(1:1).EQ.'Y') I = 2
      J = 3-I
      IF (MOD(1:5).EQ.'GAINS') THEN
         CALL GAINS (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)

      ELSE IF (MOD(I:I+2).EQ.'DOT') THEN
         CALL POINTD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+3).EQ.'CORE') THEN
         CALL CORED (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+2).EQ.'POL') THEN
         CALL STOKED (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+3).EQ.'DISK') THEN
         CALL DISKD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+2).EQ.'GAU') THEN
         CALL GAUSSD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+3).EQ.'EXPT') THEN
         CALL EXPTD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+3).EQ.'BLOB') THEN
         CALL BLOBD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+3).EQ.'SPHR') THEN
         CALL SPHRSD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+2).EQ.'KOL') THEN
         CALL KOLIND (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+2).EQ.'MUK') THEN
         CALL MUKOLD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(I:I+2).EQ.'MAS') THEN
         CALL MASERD (OP, J, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)

C                                       these require updating.
      ELSE IF (MOD(1:3).EQ.'ZEE') THEN
         CALL ZEMAND (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
      ELSE IF (MOD(1:3).EQ.'HAL') THEN
         CALL HALOSD (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
         END IF
      RETURN
      END
      SUBROUTINE MODELM (MOD, CHAN, FREQ, STOK, TIME,
     *   BU, BV, BW, BUU, BUV, BVV, MODC, MODTC, XGOB, LGOB,
     *   LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CHARACTER MOD*(*)
      INTEGER CHAN, STOK, J, I, ID, IPAR, MPAR, SPAR
      REAL TIME
      DOUBLE PRECISION FREQ, BU, BV, BW, BUU, BUV, BVV, XGOB(*)
      COMPLEX MODC, MODTC, GRAD(*)
      LOGICAL LGRAD, LGOB(*)
C-----------------------------------------------------------------------
      J = 1
      IF (MOD(1:1).EQ.'Y') J = 2
      ID = 5
      I = XGOB(2)
      IPAR = 1
      MPAR = IPAR + ID
      SPAR = MPAR + I
C                                       the available call seq is
c     CALL xxxxxM (CHAN, FREQ, STOK,TIME,BU, BV, BW, BUU, BUV, BVV,
C    *      MODC, MODTC, XGOB(IPAR), XGOB(MPAR), XGOB(SPAR), LGOB,
C    *      LGRAD, GRAD)
C                                       following have been stripped
C                                       of unused parameters
      IF (MOD(J:J+2).EQ.'DOT') THEN
         CALL POINTM (BU, BV, BW, MODC, MODTC, XGOB(MPAR), XGOB(SPAR),
     *      LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+3).EQ.'CORE') THEN
         CALL COREM (FREQ, BU, BV, BW, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+2).EQ.'POL') THEN
         CALL STOKEM (STOK, BU, BV, BW, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+3).EQ.'DISK') THEN
         CALL DISKM (BU, BV, BW, BUU, BUV, BVV, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+2).EQ.'KOL') THEN
         CALL KOLINM (BU, BV, BW, BUU, BUV, BVV, MODC, MODTC,
     *      XGOB(MPAR), XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+2).EQ.'MUK') THEN
         CALL MUKOLM (BU, BV, BW, BUU, BUV, BVV, MODC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+2).EQ.'MAS') THEN
         CALL MASERM (CHAN, BU, BV, BW, BUU, BUV, BVV, MODC, MODTC,
     *      XGOB(MPAR), XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+2).EQ.'GAU') THEN
         CALL GAUSSM (BU, BV, BW, BUU, BUV, BVV, MODC, MODTC,
     *      XGOB(MPAR), XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+3).EQ.'EXPT') THEN
         CALL EXPTM (BU, BV, BW, BUU, BUV, BVV, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+3).EQ.'BLOB') THEN
         CALL BLOBM (TIME, BU, BV, BW, BUU, BUV, BVV, MODC, MODTC,
     *      XGOB(MPAR), XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(J:J+3).EQ.'SPHR') THEN
         CALL SPHRSM (BU, BV, BW, BUU, BVV, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(1:3).EQ.'ZEE') THEN
         CALL ZEMANM (FREQ, STOK, BU, BV, BW, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
      ELSE IF (MOD(1:3).EQ.'HAL') THEN
         CALL HALOSM (BU, BV, BW, BUU, BVV, MODC, MODTC, XGOB(MPAR),
     *      XGOB(SPAR), LGOB, LGRAD, GRAD)
         END IF
      RETURN
      END
      SUBROUTINE STDSKI (X, Y)
      DOUBLE PRECISION X(2), Y(3)
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION POSX, POSY, WTEMP, TEMP

      POSX = X(1) / NATSKY
      POSY = X(2) / NATSKY
      WTEMP = POSX*POSX + POSY*POSY
      IF (WTEMP.LT.1.0D-6) THEN
C                                       very small w-term corrections
         Y(1) =  NATSKY * WTEMP / 2.0D0
         Y(2) =  POSX
         Y(3) = -POSY
      ELSE IF (WTEMP.LT.1.0D0) THEN
C                                       relatively large w-term corrections
         TEMP =  SQRT(1.0D0 - WTEMP)
         Y(1) = -NATSKY * ( 1.0D0 - TEMP )
         Y(2) =  POSX / TEMP
         Y(3) = -POSY / TEMP
      ELSE
C                                       outrageous w-term corrections
C                                       [just give up!]
         Y(1) = 0.0D0
         Y(2) = 0.0D0
         Y(3) = 0.0D0
         ENDIF
      RETURN
      END
      SUBROUTINE APPSKI (X, Y, U, V, W, Z1, Z2, Z3)
      DOUBLE PRECISION X(2), Y(3), U, V, W
      REAL Z1, Z2, Z3
      Z1 = X(1) * U + X(2) * V + Y(1) * W
      Z2 =        U            + Y(2) * W
      Z3 =                   V + Y(3) * W
      RETURN
      END
      SUBROUTINE STDELL (METHOD, X, Y)
C  STDELL returns the model and derivatives of an elliptical model
C  using different characterizations
C  METHOD = 1 uses X1 = axis 1
C                  X2 = axis 2
C                  X3 = position angle of axis 1
C  METHOD = 4 uses X1 = ln(axis 1)
C                  X2 = SQRT( ln(axis1) - ln(axis 2) )
C                  X3 = position angle of axis 1
C
C  internally in this routine,
C                  A  = axis 1
C                  B  = axis 2
C                  C  = position angle of axis 1
C                                       the question is, how to enforce
C                                       the requirement that axis 2 < axis 1??
C  On output, Y(1-3), Y(4-6), Y(7-9), Y(10-12) are the coefficients of
C  U*U, U*V, and V*V for Dphi, dDphi/dA, dDphi/dB, and dDphi/dC respectively
C-----------------------------------------------------------------------
      INCLUDE 'ZINFO.INC'
      INTEGER METHOD
      DOUBLE PRECISION X(3), Y(12)
      DOUBLE PRECISION A, B, C, CC, SS, C2, S2
C-----------------------------------------------------------------------
C                                       prepare angle parameters
C                                       prep output parameters
      CALL DFILL (12, 0.0D0, Y)
      IF (METHOD.EQ.1) THEN
         A = EXP(2.0D0*X(1))
         B = EXP(2.0D0*X(2))
         C = X(3)
         CC = COS(C)**2
         SS = SIN(C)**2
         C2 = COS(2.0D0*C)
         S2 = SIN(2.0D0*C)
         Y(1) =    A*SS + B*CC
         Y(2) =    (A-B)*S2
         Y(3) =    A*CC + B*SS
         Y(4) =    A*SS
         Y(5) =    A*S2
         Y(6) =    A*CC
         Y(7) =    B*CC
         Y(8) =   -B*S2
         Y(9) =    B*SS
         Y(10) =   Y(2)
         Y(11) = 2.0D0*(A-B)*C2
         Y(12) =  -Y(2)
         END IF
      IF (METHOD.EQ.2) THEN
         A = ( EXP(X(1)) + SIZMIN )**2
         B = ( EXP(X(2)) + SIZMIN )**2
         C = X(3)
         CC = COS(C)**2
         SS = SIN(C)**2
         C2 = COS(2.0D0*C)
         S2 = SIN(2.0D0*C)
         Y(1) =    A*SS + B*CC
         Y(2) =    (A-B)*S2
         Y(3) =    A*CC + B*SS
         Y(4) =    SS * 2.0D0 * SQRT(A)
         Y(5) =    S2 * 2.0D0 * SQRT(A)
         Y(6) =    CC * 2.0D0 * SQRT(A)
         Y(7) =    CC * 2.0D0 * SQRT(B)
         Y(8) =   -S2 * 2.0D0 * SQRT(B)
         Y(9) =    SS * 2.0D0 * SQRT(B)
         Y(10) =   Y(2)
         Y(11) = 2.0D0*(A-B)*C2
         Y(12) =  -Y(2)
         END IF
      IF (METHOD.EQ.4) THEN
         A = EXP(2.0D0*X(1))
         B = EXP(2.0D0*X(1) - 2.0D0*X(2)*X(2))
         C = X(3)
         CC = COS(C)**2
         SS = SIN(C)**2
         C2 = COS(2.0D0*C)
         S2 = SIN(2.0D0*C)
         Y(1) =    A*SS + B*CC
         Y(2) =    (A-B)*S2
         Y(3) =    A*CC + B*SS
         Y(4) =  2.0D0*Y(1)
         Y(5) =  2.0D0*Y(2)
         Y(6) =  2.0D0*Y(3)
         Y(7) = -4.0D0*X(2)*B*CC
         Y(8) =  4.0D0*X(2)*B*S2
         Y(9) = -4.0D0*X(2)*B*SS
         Y(10) =   Y(2)
         Y(11) = 2.0D0*(A-B)*C2
         Y(12) =  -Y(2)
         END IF
      RETURN
      END
      SUBROUTINE APPELL ( Y, UU, UV, VV, Z0, Z1, Z2, Z3)
      DOUBLE PRECISION Y(12), UU, UV, VV
      REAL Z0, Z1, Z2, Z3
      Z0 = Y(1)  * UU + Y(2)  * UV + Y(3)  * VV
      Z1 = Y(4)  * UU + Y(5)  * UV + Y(6)  * VV
      Z2 = Y(7)  * UU + Y(8)  * UV + Y(9)  * VV
      Z3 = Y(10) * UU + Y(11) * UV + Y(12) * VV
      RETURN
      END
C  POINT is a point source model
      SUBROUTINE POINTN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 3
      SMOD = 5 + NMOD + 3
      RETURN
      END
      SUBROUTINE POINTD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE POINTI (DOFTAR)
C-----------------------------------------------------------------------
C  POINTI provides an initial point source model suitable for
C  a typical VLA-type problem
C     1 Jy source with 0.0 arcsec offsets from the phase center
C     solve for integrated flux and position offsets from phase center
C                                   in arcseconds
      REAL DOFTAR(*)
C-----------------------------------------------------------------------
      DOFTAR(1) = 1.0
      DOFTAR(2) = 1.0
      DOFTAR(3) = 1.0
      DOFTAR(4) = 0.0
      DOFTAR(5) = 1.0
      DOFTAR(6) = 0.0
      DOFTAR(7) = 1.0
      RETURN
      END
      SUBROUTINE POINTS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
      RETURN
      END
      SUBROUTINE POINTM (U, V, W, MODC, MODTC, XMGOB, XSGOB, LGOB,
     *   LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      LFLUX, SKYP, DEAST, DNORT
      COMPLEX   CI, TEMC
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP (TEMC)
      MODC = MODC * MODTC
      TEMC = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =            MODC
         IF (LGOB(2)) GRAD(2) =    DEAST * TEMC
         IF (LGOB(3)) GRAD(3) =    DNORT * TEMC
         END IF
C
 999  RETURN
      END
C  POINT is a point source model
      SUBROUTINE COREN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 5
      SMOD = 5 + NMOD + 3
      RETURN
      END
      SUBROUTINE CORED (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*), CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = UPMOD(4)
         PPMOD(5) = UPMOD(5)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = PPMOD(4)
         UPMOD(5) = PPMOD(5)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = PEMOD(4)
         UEMOD(5) = PEMOD(5)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Spectral'
         UMOD(4) = 'Index   '
         CMOD(5) = 'Referenc'
         UMOD(5) = 'Frequenc'
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE CORES (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
      RETURN
      END
      SUBROUTINE COREM (FREQ, U, V, W, MODC, MODTC, XMGOB, XSGOB, LGOB,
     *   LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION FREQ, U, V, W, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      LFLUX, SKYP, DEAST, DNORT, SINDEX, FQRAT
      DOUBLE PRECISION LFQRAT
      COMPLEX   CI, TEMC
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
      SINDEX = XMGOB(4)
      LFQRAT  = LOG(FREQ/XMGOB(5))
      FQRAT = LFQRAT
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      SINDEX = SINDEX * LFQRAT
      TEMC = TEMC + SINDEX
      MODTC = CEXP (TEMC)
      MODC = MODC * MODTC
      TEMC = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =            MODC
         IF (LGOB(2)) GRAD(2) =    DEAST * TEMC
         IF (LGOB(3)) GRAD(3) =    DNORT * TEMC
         IF (LGOB(4)) GRAD(4) =    FQRAT * MODC
         END IF
C
 999  RETURN
      END
C                                       THIS IS UNTESTED CODE!!!
C                                       STOKE models a polarized
C                                       point source
      SUBROUTINE STOKEN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 4
      SMOD = 5 + NMOD + 3
      RETURN
      END
      SUBROUTINE STOKED (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = UPMOD(1)
         PPMOD(2) = UPMOD(2)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = UPMOD(4) * POSSK(S)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = PPMOD(1)
         UPMOD(2) = PPMOD(2)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = PPMOD(4) / POSSK(S)
C                                       compute external error bars
         UEMOD(1) = PEMOD(1)
         UEMOD(2) = -1.0
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = PEMOD(4) / POSSK(S)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'Stokes  '
         UMOD(2) = 'AIPScode'
         CMOD(3) = 'E-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'N-offset'
         UMOD(4) = 'mas     '
         IF (S.EQ.1) THEN
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE STOKES (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
      CALL STDSKI ( XMGOB(3), XSGOB(1) )
      RETURN
      END
      SUBROUTINE STOKEM (STOK, U, V, W, MODC, MODTC, XMGOB, XSGOB, LGOB,
     *   LGRAD, GRAD)
C-----------------------------------------------------------------------
      INTEGER   STOK
      DOUBLE PRECISION U, V, W, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      FLUX, DEAST, DNORT, SKYP
      INTEGER   ISTOK
      COMPLEX   CI, TEMC, TEM2C
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      FLUX  = XMGOB(1)
      ISTOK = XMGOB(2) + 0.1
      IF (STOK.NE.ISTOK) GO TO 999
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(3), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       Outputs here
      TEMC = CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC  = MODC * MODTC
      MODTC = MODTC * FLUX
      MODC = MODTC * MODC
      TEM2C = MODC * CI
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =         TEMC
         IF (LGOB(3)) GRAD(3) = DEAST * TEM2C
         IF (LGOB(4)) GRAD(4) = DNORT * TEM2C
         END IF
 999  RETURN
      END
C  HERE BEGINS THE DISK MODEL
C  This model incorporates (with different orders)
C   The uniform disk model            \Lambda_1(z)
C   The optically thin sphere model   \Lambda_{3/2}(z)
C   The thin ring model               \Lambda_0 (z)
C   The limb darkened disk model      \Lambda_\nu (z), \nu>=0
C                                       DISK models disks, spheres,
C                                       and rings via nu.
      SUBROUTINE DISKN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 7
      SMOD = 5 + NMOD + 16
      RETURN
      END
      SUBROUTINE DISKD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * DISSK(S) )
         PPMOD(5) = LOG ( UPMOD(5) * DISSK(S) )
         PPMOD(6) = UPMOD(6) / RAD2DG
         PPMOD(7) = SQRT(UPMOD(7))
         PPMOD(7) = SQRT( 1.0 + UPMOD(7)/2.0 )
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP(PPMOD(4)) / DISSK(S)
         UPMOD(5) = EXP(PPMOD(5)) / DISSK(S)
         UPMOD(6) = PPMOD(6) * RAD2DG
         UPMOD(7) = PPMOD(7)*PPMOD(7)
         UPMOD(7) = 2.0 * PPMOD(7)*PPMOD(7) - 2.0
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP(PPMOD(4)) * PEMOD(4) / DISSK(S)
         UEMOD(5) = EXP(PPMOD(5)) * PEMOD(5) / DISSK(S)
         UEMOD(6) = PEMOD(6) * RAD2DG
         UEMOD(7) = 2.0 * ABS(PPMOD(7)) * PEMOD(7)
         UEMOD(7) = 4.0 * ABS(PPMOD(7)) * PEMOD(7)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Theta X '
         UMOD(4) = 'mas     '
         CMOD(5) = 'Theta Y '
         UMOD(5) = 'mas     '
         CMOD(6) = 'X P.A.  '
         UMOD(6) = 'deg N->E'
         CMOD(7) = 'Limb Co.'
         UMOD(7) = 'n >=-2  '
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            UMOD(5) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE DISKS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL ( 1, XMGOB(4), XSGOB(4) )
C                                       for limb darkening
      XSGOB(16) = XMGOB(7)*XMGOB(7)
      RETURN
      END
      SUBROUTINE DISKM (U, V, W, UU, UV, VV, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      LFLUX
      DOUBLE PRECISION LAMN
      COMPLEX   CI, TEMC
      REAL      SKYP, DEAST, DNORT
      REAL      DPHI0, DPHI1, DPHI2, DPHI3, RJOE
      DOUBLE PRECISION NU, BETA, LAM, DDBETA, DDNU, DDZ
      LOGICAL   GETDDB, GETDDN
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
      LAMN   = XMGOB(7)
      NU     = XSGOB(16)

      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
      CALL APPELL (XSGOB(4), UU, UV, VV, DPHI0, DPHI1, DPHI2, DPHI3)

      BETA = SQRT(DPHI0)
      GETDDB = LGRAD .AND. (LGOB(4).OR.LGOB(5).OR.LGOB(6))
      GETDDN = LGRAD .AND. LGOB(7)
      CALL LAMFCN (NU, BETA, LAM, GETDDB, DDBETA, GETDDN, DDNU)
      DDZ = DDBETA / BETA
      DPHI1 = DPHI1 * DDZ
      DPHI2 = DPHI2 * DDZ
      DPHI3 = DPHI3 * DDZ
      RJOE = LAM
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC = MODC * MODTC
      MODTC = MODTC * RJOE
      MODC = MODC * MODTC
      RJOE = 2.0D0 * LAMN * DDNU
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              MODC
         IF (LGOB(2)) GRAD(2) = DEAST * CI * MODC
         IF (LGOB(3)) GRAD(3) = DNORT * CI * MODC
         IF (LGOB(4)) GRAD(4) = DPHI1 * TEMC
         IF (LGOB(5)) GRAD(5) = DPHI2 * TEMC
         IF (LGOB(6)) GRAD(6) = DPHI3 * TEMC
         IF (LGOB(7)) GRAD(7) = RJOE  * TEMC
         END IF
 999  RETURN
      END
C                                       MASER models an elliptical
C                                       gaussian source with a linear
C                                       drift of position with frequency
      SUBROUTINE MASERN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 10
      SMOD = 5 + NMOD + 16
      RETURN
      END
      SUBROUTINE MASERD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * FWHM(S) )
         PPMOD(5) = SQRT( -LOG( UPMOD(5) ) )
         PPMOD(6) = UPMOD(6) * DG2RAD
         PPMOD(7) = UPMOD(7)
         PPMOD(8) = UPMOD(8)
         PPMOD(9) = UPMOD(9) * POSSK(S)
         PPMOD(10) = UPMOD(10) * POSSK(S)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP (PPMOD(4)) / FWHM(S)
         UPMOD(5) = EXP ( -PPMOD(5)*PPMOD(5) )
         UPMOD(6) = PPMOD(6) * RAD2DG
         UPMOD(7) = PPMOD(7)
         UPMOD(8) = PPMOD(8)
         UPMOD(9) = PPMOD(9) / POSSK(S)
         UPMOD(10) = PPMOD(10) / POSSK(S)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP ( PPMOD(4) ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = EXP (-PPMOD(5)*PPMOD(5))*2.0*ABS(PPMOD(5))*PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG
         UEMOD(7) = PEMOD(7)
         UEMOD(8) = PEMOD(8)
         UEMOD(9) = PEMOD(9) / POSSK(S)
         UEMOD(10) = PEMOD(10) / POSSK(S)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj.Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Ax.ratio'
         UMOD(5) = '----    '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         CMOD(7) = 'Velocity'
         UMOD(7) = 'channels'
         CMOD(8) = 'V-width '
         UMOD(8) = 'channels'
         CMOD(9) = 'E-shift '
         UMOD(9) = 'mas/chan'
         CMOD(10) = 'N-shift '
         UMOD(10) = 'mas/chan'
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(9) = 'asec/chn'
            UMOD(10) = 'asec/chn'
            UMOD(4) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE MASERS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL later
C
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL ( 4, XMGOB(4), XSGOB(4) )
      XSGOB(16) = 2.0*LOG(2.0) / (XMGOB(8)*XMGOB(8))
      RETURN
      END
      SUBROUTINE MASERM (CHAN, U, V, W, UU, UV, VV, MODC, MODTC, XMGOB,
     *   XSGOB, LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   CHAN
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      LFLUX, DPHI, DPHI0, DPHI1, DPHI2, DPHI3, DNORT, DEAST,
     *   SKYP, VFAC2, VFAC3, DVEL
      COMPLEX   CI, TEMC, VFAC1
      DOUBLE PRECISION VELWID, AJOE(2)
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
      DVEL   = CHAN - XMGOB(7)
      VELWID = XMGOB(8)
      VFAC2  = XSGOB(16) * DVEL**2
      VFAC3  = 2.0*VFAC2 / VELWID
      AJOE(1) = XMGOB(2) + DVEL * XMGOB(9)
      AJOE(2) = XMGOB(3) + DVEL * XMGOB(10)
      VFAC1 =  2.0*REAL(XSGOB(16) * DVEL)
     *        - CI*REAL(XMGOB(9) *DEAST)
     *        - CI*REAL(XMGOB(10)*DNORT)
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI (AJOE(1), XSGOB(1))
C                                       assemble sky offset model
      CALL APPSKI ( AJOE(1), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       assemble sky brightness model
      CALL APPELL ( XSGOB(4), UU, UV, VV, DPHI0, DPHI1, DPHI2, DPHI3)

      DPHI  = -0.5 * DPHI0
      DPHI1 = -0.5 * DPHI1
      DPHI2 = -0.5 * DPHI2
      DPHI3 = -0.5 * DPHI3
      TEMC = LFLUX - VFAC2 + DPHI + CI * SKYP
C                                       Outputs here
      MODTC = CEXP(TEMC)
      MODC = MODC * MODTC
      TEMC = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1))  GRAD(1)  =                MODC
         IF (LGOB(2))  GRAD(2)  =        DEAST * TEMC
         IF (LGOB(3))  GRAD(3)  =        DNORT * TEMC
         IF (LGOB(4))  GRAD(4)  =        DPHI1 * MODC
         IF (LGOB(5))  GRAD(5)  =        DPHI2 * MODC
         IF (LGOB(6))  GRAD(6)  =        DPHI3 * MODC
         IF (LGOB(7))  GRAD(7)  =        VFAC1 * MODC
         IF (LGOB(8))  GRAD(8)  =        VFAC3 * MODC
         IF (LGOB(9))  GRAD(9)  = DVEL * DEAST * TEMC
         IF (LGOB(10)) GRAD(10) = DVEL * DNORT * TEMC
         END IF
C
 999  RETURN
      END
C                                       BLOB models Gaussian
C                                       elliptical sources using
C                                       position angles, geometric
C                                       means, and axial ratios
      SUBROUTINE BLOBN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 9
      SMOD = 5 + NMOD + 15
      RETURN
      END
      SUBROUTINE BLOBD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
         IF (UPMOD(5).GT.1.0) UPMOD(5) = 1./UPMOD(5)
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * FWHM(S) )
         PPMOD(5) = SQRT( -LOG( UPMOD(5) ) )
         PPMOD(6) = UPMOD(6) * DG2RAD
         PPMOD(7) = UPMOD(7)
         PPMOD(8) = UPMOD(8) * POSSK(S)
         PPMOD(9) = UPMOD(9) * POSSK(S)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP ( PPMOD(4) ) / FWHM(S)
         UPMOD(5) = EXP ( -PPMOD(5)*PPMOD(5) )
         UPMOD(6) = PPMOD(6) * RAD2DG
         UPMOD(7) = PPMOD(7)
         UPMOD(8) = PPMOD(8) / POSSK(S)
         UPMOD(9) = PPMOD(9) / POSSK(S)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP ( PPMOD(4) ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = EXP (-PPMOD(5)*PPMOD(5))*2.0*ABS(PPMOD(5))*PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG
         UEMOD(7) = PEMOD(7)
         UEMOD(8) = PEMOD(8) / POSSK(S)
         UEMOD(9) = PEMOD(9) / POSSK(S)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Ax.Ratio'
         UMOD(5) = 'min/max '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         CMOD(7) = 'Ref time'
         UMOD(7) = 'days    '
         CMOD(8) = 'E-rate  '
         UMOD(8) = 'mas/day '
         CMOD(9) = 'N-rate  '
         UMOD(9) = 'mas/day '
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            UMOD(8) = 'asec/day'
            UMOD(9) = 'asec/day'
            END IF
         END IF
      RETURN
      END
      SUBROUTINE BLOBS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL later
C
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL (4, XMGOB(4), XSGOB(4) )
      RETURN
      END
      SUBROUTINE BLOBM (TIME, U, V, W, UU, UV, VV, MODC, MODTC, XMGOB,
     *   XSGOB, LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*), AJOE(2)
      REAL      MODD, TIME
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      DPHI, DPHI0, DPHI1, DPHI2, DPHI3, LFLUX, DNORT, DEAST,
     *   SKYP, DTIM
      COMPLEX   TEMC, CI, TEM2C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
      DTIM = REAL(DBLE(TIME) - XMGOB(7))
      AJOE(1) = XMGOB(2) + DTIM * XMGOB(8)
      AJOE(2) = XMGOB(3) + DTIM * XMGOB(9)
C                                       get the sky shift model w/ w-term
      CALL STDSKI (AJOE(1), XSGOB(1))
C                                       assemble sky offset model
      CALL APPSKI (AJOE(1), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       assemble sky brightness model
      CALL APPELL (XSGOB(4), UU, UV, VV, DPHI, DPHI1, DPHI2, DPHI3)
      DPHI0 = -0.5 * DPHI
      DPHI1 = -0.5 * DPHI1
      DPHI2 = -0.5 * DPHI2
      DPHI3 = -0.5 * DPHI3
      MODD  = EXP(DPHI0)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC  = MODC * MODTC
      MODTC = MODTC * MODD
      MODC = MODC * MODTC
      TEM2C = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              MODC
         IF (LGOB(2)) GRAD(2) =     DEAST * TEM2C
         IF (LGOB(3)) GRAD(3) =     DNORT * TEM2C
         IF (LGOB(4)) GRAD(4) =      DPHI1 * MODC
         IF (LGOB(5)) GRAD(5) =      DPHI2 * MODC
         IF (LGOB(6)) GRAD(6) =      DPHI3 * MODC
         IF (LGOB(8)) GRAD(8) =DTIM*DEAST * TEM2C
         IF (LGOB(9)) GRAD(9) =DTIM*DNORT * TEM2C
         END IF
 999  RETURN
      END
C
C  added by MFB
C
      SUBROUTINE SPHRSN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
C                                       4 parameters
      NMOD = 5
C                                       3 pre-computed quantities
      SMOD = 5 + NMOD + 7
      RETURN
      END
      SUBROUTINE SPHRSD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
C                                       Sanity check
         IF (UPMOD(5) .LT. 0.) UPMOD(5) = 0.
C                                       models unit is total flux
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG(UPMOD(4) * FWHM(S))
C                                      models is 1+/- shell thickness/2
         PPMOD(5) = UPMOD(5)/2.
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         IF (PPMOD(5) .LE. 0) PPMOD(5) = 0.
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP(PPMOD(4)) / FWHM(S)
         UPMOD(5) = PPMOD(5)*2.
C                                       compute external error bars
         UEMOD(1) = UPMOD(1) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP(PPMOD(4)) * PEMOD(4) / FWHM(S)
         UEMOD(5) = 2.0 * PEMOD(5)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Avg Rad '
         UMOD(4) = 'mas     '
         CMOD(5) = 'Shell f.'
         UMOD(5) = '        '
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE SPHRSS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*), FRAC
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
C                                       get the sky shift model w/ w-term
C                                       uses xsgob(1-3)
      CALL STDSKI (XMGOB(2), XSGOB(1))
C                                       The total flux
      XSGOB(4) = EXP(XMGOB(1))
C                                       radius
C                                       trap radius too large
      IF (XMGOB(4) .GT. 18) THEN
          XMGOB(4) = 18.
          END IF
      XSGOB(5) = EXP(XMGOB(4))

C                                       fraction
      FRAC = 1.D0 + XMGOB(5)
      XSGOB(6) = FRAC
C                                       flux factor
      XSGOB(7) = (1. - FRAC**(-6))
C        Trap FRAC small
      IF (XMGOB(5) .LT. 1.E-6) XSGOB(7) = 6.*XMGOB(5)
C
      RETURN
      END
      SUBROUTINE SPHRSM (U, V, W, UU, VV, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, VV, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      DEAST, DNORT, SKYP, TMPTMP
      REAL      FLUX, RADI, C1, SMALLX, AA, CAA, SAA, AA2, AA3, BB, CBB,
     *   SBB, BB2
      DOUBLE PRECISION FRAC, FLXK, SPHX, GRTMP
      COMPLEX   CI, TEMC, POSX
C
      PARAMETER (CI = (0.0,1.0))
C         assume that 1.673378e-10 is the magic number
C         by which one divides the UVFIT constant to get the
C         OMFIT constant.
C         for as**2*lambda**2; so its 1.295121e-5 for as*lambda
C         Now the UVFIT sphere constant is 3.0145174e-5 so we want
C      PARAMETER (C1 = 2.327618346)
C         Better version;  OMFIT has units of 0.012935911 mas
C         and Mlmabda.  UVFIT has units of lambda and as
C         so C1_om = C1_uv * 1.e6/0.012935911/1000
      PARAMETER (C1 = 2.354819849951039)
      PARAMETER (SMALLX = 0.1)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       get standard sky
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       get poincare transform of ellipse

C                                       Notes: AA = R*f = outside
C                                              BB = R/r = inside
      IF (XMGOB(5) .LE. 0.) THEN
         WRITE (MSGTXT,1000) '::: Oh Oh; shell thickness <=0',
     *      XMGOB(5)*2.
         CALL MSGWRT (8)
         XMGOB(5) = 1.E-7
         END IF
      FLUX = XSGOB(4)
      RADI = C1 * XSGOB(5) * SQRT(UU + VV)
      FRAC = XSGOB(6)
      FLXK = XSGOB(7)
      AA =  RADI * FRAC
      AA2 = AA*AA
      AA3 = AA*AA2
      BB =  RADI / FRAC
      BB2 = BB*BB
      CAA = COS(AA)
      SAA = SIN(AA)
      CBB = COS(BB)
      SBB = SIN(BB)
C           trap AA small
      IF (AA .GT. SMALLX) THEN
         SPHX = 3. * (SAA - AA * CAA)
      ELSE
         SPHX = AA*AA2 - 0.1 * AA**5
         END IF
      IF (BB .GT. SMALLX) THEN
         SPHX = SPHX - 3.*(SBB - BB * CBB)
      ELSE
         SPHX = SPHX - BB*BB2 + 0.1 * BB**5
         END IF
      SPHX  = SPHX /(FLXK * AA3)
      POSX  = CEXP(CI * SKYP)
      TMPTMP = FLUX * SPHX
C     MODTC = FLUX * POSX * SPHX
      MODTC = CMPLX (TMPTMP, 0.0) * POSX
      MODC  = MODC * MODTC
      TEMC  = CI * MODC
C
C                       note
C                       XSGOB(1-3) sky
C                       XSGOB(4) = flux
C                       XSGOB(5) = radius
C                       XSGOB(6) = frac
C
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =            MODC
         IF (LGOB(2)) GRAD(2) =    DEAST * TEMC
         IF (LGOB(3)) GRAD(3) =    DNORT * TEMC
         IF (LGOB(4)) THEN
           GRTMP = -3. * SPHX/RADI
     *      + 3.* ((AA*SAA*FRAC) - (BB*SBB/FRAC)) / (AA3 * FLXK)
C           IF (AA .GT. SMALLX) THEN
C                 I don't think I need to trap small AA here
C                 If the derivatives are small, then then they dont
C                 matter too much
           TMPTMP = FLUX * GRTMP * RADI
C          GRAD(4) = FLUX * POSX * GRTMP * RADI
           GRAD(4) = CMPLX (TMPTMP, 0.0) * POSX
           END IF
C
         IF (LGOB(5)) THEN
           GRTMP = -3. * SPHX/FRAC
     *      - 6.* FRAC**(-7) * SPHX / FLXK
     *      + 3. * ((AA2*SAA/FRAC) + (BB2*SBB/FRAC)) / (AA3 * FLXK)
           TMPTMP = FLUX  * GRTMP
C          GRAD(5) = FLUX * POSX * GRTMP
           GRAD(5) = CMPLX (TMPTMP, 0.0) * POSX
           END IF
C
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,G18.7)
      END
C                                       End of MFB spherical shell model
C                                       GAUSS models Gaussian
C                                       elliptical sources using
C                                       position angles, geometric
C                                       means, and axial ratios
      SUBROUTINE GAUSSN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 6
      SMOD = 5 + NMOD + 15
      RETURN
      END
      SUBROUTINE GAUSSD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
         IF (UPMOD(5).GT.1.0) UPMOD(5) = 1./UPMOD(5)
         UPMOD(4) = MAX ( UPMOD(4), 1.0D-10)
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * FWHM(S) )
         PPMOD(5) = SQRT( -LOG( UPMOD(5) ) )
         PPMOD(6) = UPMOD(6) * DG2RAD
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP ( PPMOD(4) ) / FWHM(S)
         UPMOD(5) = EXP ( -PPMOD(5)*PPMOD(5) )
         UPMOD(6) = PPMOD(6) * RAD2DG
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP ( PPMOD(4) ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = EXP (-PPMOD(5)*PPMOD(5))*2.0*ABS(PPMOD(5))*PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Ax.Ratio'
         UMOD(5) = 'min/max '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE GAUSSI (DVAL, CVAL)
C-----------------------------------------------------------------------
C  GAUSSI provides a gaussian model suitable for
C  a typical VLA-type problem
C     1 Jy source with 0.0 arcsec offsets from the phase center
C         with a 1 arcsecond gaussian.
C     solve for integrated flux and position offsets from phase center
C                                   in arcseconds
      DOUBLE PRECISION DVAL(*)
      CHARACTER*8 CVAL(*)
C-----------------------------------------------------------------------
      CVAL(1) = 'YGAU'
      DVAL(2) = 0.0D0
      DVAL(3) = 1.0D0
      DVAL(4) = 9999.0D0
      DVAL(5) = 1.0D0
      CVAL(6) = 'T'
      DVAL(7) = 0.0D0
      CVAL(8) = 'T'
      DVAL(9) = 0.0D0
      CVAL(10) = 'T'
      DVAL(11) = 1.0
      CVAL(12) = 'T'
      DVAL(13) = 0.9
      CVAL(14) = 'T'
      DVAL(15) = 0.0
      CVAL(16) = 'T'
      RETURN
      END
      SUBROUTINE GAUSSS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL (4, XMGOB(4), XSGOB(4) )
      RETURN
      END
      SUBROUTINE GAUSSM (U, V, W, UU, UV, VV, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      REAL      MODD
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      DPHI, DPHI0, DPHI1, DPHI2, DPHI3, LFLUX, DNORT, DEAST,
     *   SKYP
      COMPLEX   TEMC, CI, TEM2C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       assemble sky brightness model
      CALL APPELL (XSGOB(4), UU, UV, VV, DPHI, DPHI1, DPHI2, DPHI3)
      DPHI0 = -0.5 * DPHI
      DPHI1 = -0.5 * DPHI1
      DPHI2 = -0.5 * DPHI2
      DPHI3 = -0.5 * DPHI3
      MODD  = EXP(DPHI0)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC  = MODC * MODTC
      MODTC = MODTC * MODD
      MODC = MODC * MODTC
      TEM2C = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              MODC
         IF (LGOB(2)) GRAD(2) =     DEAST * TEM2C
         IF (LGOB(3)) GRAD(3) =     DNORT * TEM2C
         IF (LGOB(4)) GRAD(4) =      DPHI1 * MODC
         IF (LGOB(5)) GRAD(5) =      DPHI2 * MODC
         IF (LGOB(6)) GRAD(6) =      DPHI3 * MODC
         END IF
 999  RETURN
      END
C                                       EXPTS models Gaussian
C                                       elliptical sources using
C                                       position angles, geometric
C                                       means, and axial ratios
      SUBROUTINE EXPTN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 6
      SMOD = 5 + NMOD + 15
      RETURN
      END
      SUBROUTINE EXPTD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = UPMOD(4) * FWHM(S)
         PPMOD(5) = UPMOD(5) * FWHM(S)
C                                       [ force minimum size ]
         PPMOD(4) = SQRT(PPMOD(4)**2 + SIZMIN**2)
         PPMOD(5) = SQRT(PPMOD(5)**2 + SIZMIN**2)
         PPMOD(4) = LOG ( PPMOD(4) ) * FWHM(S)
         PPMOD(5) = LOG ( PPMOD(5) ) * FWHM(S)
         PPMOD(6) = UPMOD(6) * DG2RAD
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = ( EXP ( PPMOD(4) ) + SIZMIN ) / FWHM(S)
         UPMOD(5) = ( EXP ( PPMOD(5) ) + SIZMIN ) / FWHM(S)
         UPMOD(6) = PPMOD(6) * RAD2DG
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = ( EXP ( PPMOD(4) ) + SIZMIN ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = ( EXP ( PPMOD(5) ) + SIZMIN ) * PEMOD(5) / FWHM(S)
         UEMOD(6) = PEMOD(6) * RAD2DG
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Min Axis'
         UMOD(5) = 'mas     '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            UMOD(5) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE EXPTI (DVAL, CVAL)
C-----------------------------------------------------------------------
C  GAUSSI provides a gaussian model suitable for
C  a typical VLA-type problem
C     1 Jy source with 0.0 arcsec offsets from the phase center
C         with a 1 arcsecond gaussian.
C     solve for integrated flux and position offsets from phase center
C                                   in arcseconds
      DOUBLE PRECISION DVAL(*)
      CHARACTER*8 CVAL(*)
C-----------------------------------------------------------------------
      CVAL(1) = 'YGAU'
      DVAL(2) = 0.0D0
      DVAL(3) = 1.0D0
      DVAL(4) = 9999.0D0
      DVAL(5) = 1.0D0
      CVAL(6) = 'T'
      DVAL(7) = 0.0D0
      CVAL(8) = 'T'
      DVAL(9) = 0.0D0
      CVAL(10) = 'T'
      DVAL(11) = 1.0
      CVAL(12) = 'T'
      DVAL(13) = 1.0
      CVAL(14) = 'T'
      DVAL(15) = 0.0
      CVAL(16) = 'T'
      RETURN
      END
      SUBROUTINE EXPTS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
C                                       PREP SKY BRIGHTNESS MODEL
C                                       [use regularized derivative]
      CALL STDELL (2, XMGOB(4), XSGOB(4) )
      RETURN
      END
      SUBROUTINE EXPTM (U, V, W, UU, UV, VV, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      REAL      MODD
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      DPHI, DPHI0, DPHI1, DPHI2, DPHI3, LFLUX, DNORT, DEAST,
     *   SKYP
      COMPLEX TEMC, CI, TEM2C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       assemble sky brightness model
      CALL APPELL (XSGOB(4), UU, UV, VV, DPHI, DPHI1, DPHI2, DPHI3)
      DPHI0 = -0.5 * DPHI
      DPHI1 = -0.5 * DPHI1
      DPHI2 = -0.5 * DPHI2
      DPHI3 = -0.5 * DPHI3
      MODD  = EXP(DPHI0)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC  = MODC * MODTC
      MODTC = MODTC * MODD
      MODC = MODC * MODTC
      TEM2C = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              MODC
         IF (LGOB(2)) GRAD(2) =     DEAST * TEM2C
         IF (LGOB(3)) GRAD(3) =     DNORT * TEM2C
         IF (LGOB(4)) GRAD(4) =      DPHI1 * MODC
         IF (LGOB(5)) GRAD(5) =      DPHI2 * MODC
         IF (LGOB(6)) GRAD(6) =      DPHI3 * MODC
         END IF
 999  RETURN
      END
C                                       KOLIN models non-gaussian
C                                       elliptical sources using
C                                       position angles, geometric
C                                       means, and axial ratios
      SUBROUTINE KOLINN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 8
      SMOD = 5 + NMOD + 16
      RETURN
      END
      SUBROUTINE KOLIND (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
         IF (UPMOD(5).GT.1.0) UPMOD(5) = 1./UPMOD(5)
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * FWHM(S) )
         PPMOD(5) = SQRT( -LOG( UPMOD(5) ) )
         PPMOD(6) = UPMOD(6) * DG2RAD
         PPMOD(7) = UPMOD(7) / 2.0
         PPMOD(8) = UPMOD(8) / LOG10(EXP(1.0))
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP ( PPMOD(4) ) / FWHM(S)
         UPMOD(5) = EXP ( -PPMOD(5)*PPMOD(5) )
         UPMOD(6) = PPMOD(6) * RAD2DG
         UPMOD(7) = PPMOD(7) * 2.0
         UPMOD(8) = PPMOD(8) * LOG10(EXP(1.0))
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP ( PPMOD(4) ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = EXP (-PPMOD(5)*PPMOD(5))*2.0*ABS(PPMOD(5))*PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG
         UEMOD(7) = PEMOD(7) * 2.0
         UEMOD(8) = PEMOD(8) * LOG10(EXP(1.0))
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Ax.Ratio'
         UMOD(5) = 'min/max '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         CMOD(7) = 'alpha   '
         UMOD(7) = '----    '
         CMOD(8) = 'lg(Dinr)'
         UMOD(8) = '2lg(rad)'
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE KOLINI (DVAL, CVAL)
C-----------------------------------------------------------------------
C  KOLINI provides an scattering disk model suitable for
C  a typical VLA-type problem
C     1 Jy source with 0.0 arcsec offsets from the phase center
C     solve for integrated flux and position offsets from phase center
C                                   in arcseconds
      DOUBLE PRECISION DVAL(*)
      CHARACTER*8 CVAL(*)
C-----------------------------------------------------------------------
      CVAL(1) = 'YKOL'
      DVAL(2) = 0.0D0
      DVAL(3) = 1.0D0
      DVAL(4) = 9999.0D0
      DVAL(5) = 1.0D0
      CVAL(6) = 'T'
      DVAL(7) = 0.0D0
      CVAL(8) = 'T'
      DVAL(9) = 0.0D0
      CVAL(10) = 'T'
      DVAL(11) = 1.0
      CVAL(12) = 'T'
      DVAL(13) = 0.9
      CVAL(14) = 'T'
      DVAL(15) = 0.0
      CVAL(16) = 'T'
      DVAL(17) = 1.85
      CVAL(18) = 'T'
      DVAL(19) = -5.0
      CVAL(20) = 'F'
      RETURN
      END
      SUBROUTINE KOLINS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION  XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL (4, XMGOB(4), XSGOB(4) )
      IF (XMGOB(8).LT.0.0) THEN
         XSGOB(16) = EXP( XMGOB(8) * (1.0 - 1.0 / XMGOB(7)))
      ELSE
         XSGOB(16) = EXP( XMGOB(8) * (1.0 - XMGOB(7)))
         END IF
      XSGOB(16) = MAX ( XSGOB(16), -76.0D0 )
      RETURN
      END
      SUBROUTINE KOLINM (U, V, W, UU, UV, VV, MODC, MODTC, XMGOB,
     *   XSGOB, LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      REAL      MODD
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      ALPHA, DPHI, DPHI1, DPHI2, DPHI3, DPHI4, DPHI5
      REAL      LFLUX, DNORT, DEAST, SKYP, DPHI0, DPHIS, DPHII, A, B
      COMPLEX   TEMC, CI
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX = XMGOB(1)
      ALPHA = XMGOB(7)
      DPHII = XMGOB(8)
      DPHIS = XSGOB(16)
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C                                       assemble sky brightness model
      CALL APPELL (XSGOB(4), UU, UV, VV, DPHI, DPHI1, DPHI2, DPHI3)
C                                       assemble derivatives of Dphi here
C
C                                       Small inner scale
      IF (DPHII.LT.0.0) THEN
C                                       Below small inner scale
         IF (DPHI.LT.EXP(DPHII/ALPHA)) THEN
            DPHI0 = -0.5 * DPHIS * DPHI
            DPHI1 = -0.5 * DPHIS * DPHI1
            DPHI2 = -0.5 * DPHIS * DPHI2
            DPHI3 = -0.5 * DPHIS * DPHI3
            DPHI4 = -0.5 * DPHIS * DPHI * DPHII / (ALPHA*ALPHA)
            DPHI5 = -0.5 * DPHIS * DPHI * (1.0 - 1.0/ALPHA)
C                                       Above small inner scale
         ELSE
            B = ALPHA - 1.0
            A = DPHI ** B
            DPHI0 = -0.5 * A * DPHI
            DPHI1 = -0.5 * A * DPHI1 * ALPHA
            DPHI2 = -0.5 * A * DPHI2 * ALPHA
            DPHI3 = -0.5 * A * DPHI3 * ALPHA
            DPHI4 = -0.5 * A * DPHI * LOG(DPHI)
            DPHI5 = 0.0
            END IF
C                                       Large inner scale
      ELSE
C                                       Below large inner scale
         IF (DPHI.LT.EXP(DPHII)) THEN
            DPHI0 = -0.5 * DPHI
            DPHI1 = -0.5 * DPHI1
            DPHI2 = -0.5 * DPHI2
            DPHI3 = -0.5 * DPHI3
            DPHI4 =  0.0
            DPHI5 =  0.0
C                                       Above large inner scale
         ELSE
            B = ALPHA - 1.0
            A = (DPHI**B) * DPHIS
            DPHI0 = -0.5 * A * DPHI
            DPHI1 = -0.5 * A * DPHI1 * ALPHA
            DPHI2 = -0.5 * A * DPHI2 * ALPHA
            DPHI3 = -0.5 * A * DPHI3 * ALPHA
            DPHI4 = -0.5 * A * DPHI * (LOG(DPHI) - DPHII)
            DPHI5 = -0.5 * A * DPHI * (-B)
            END IF
         END IF

      MODD = EXP(DPHI0)
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = MODD * CEXP(TEMC)
      MODC = MODC * MODTC
      TEMC = CI * MODC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              MODC
         IF (LGOB(2)) GRAD(2) =      DEAST * TEMC
         IF (LGOB(3)) GRAD(3) =      DNORT * TEMC
         IF (LGOB(4)) GRAD(4) =      DPHI1 * MODC
         IF (LGOB(5)) GRAD(5) =      DPHI2 * MODC
         IF (LGOB(6)) GRAD(6) =      DPHI3 * MODC
         IF (LGOB(7)) GRAD(7) =      DPHI4 * MODC
         IF (LGOB(8)) GRAD(8) =      DPHI5 * MODC
         END IF
 999  RETURN
      END
C                                       MUKOL models non-gaussian
C                                       elliptical sources using
C                                       position angles, geometric
C                                       means, and axial ratios
      SUBROUTINE MUKOLN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 10
      SMOD = 5 + NMOD + 3 + 12 + 3
      RETURN
      END
      SUBROUTINE MUKOLD (OP, S, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER S
      INCLUDE 'ZINFO.INC'
      DOUBLE PRECISION JOE
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
         IF (UPMOD(5).GT.1.0) UPMOD(5) = 1./UPMOD(5)
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(S)
         PPMOD(3) = UPMOD(3) * POSSK(S)
         PPMOD(4) = LOG ( UPMOD(4) * FWHM(S) )
         PPMOD(5) = SQRT( -LOG( UPMOD(5) ) )
         PPMOD(6) = UPMOD(6) * DG2RAD
         PPMOD(7) = UPMOD(7) / 2.0
         PPMOD(8) = LOG(UPMOD(8))
         PPMOD(9) = UPMOD(9) * POSSK(S)
         PPMOD(10) = UPMOD(10) * POSSK(S)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       keep position angle within bounds
         JOE = PPMOD(6)
         JOE = PI + MOD (JOE, PI)
         PPMOD(6) = MOD (JOE, PI)
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(S)
         UPMOD(3) = PPMOD(3) / POSSK(S)
         UPMOD(4) = EXP ( PPMOD(4) ) / FWHM(S)
         UPMOD(5) = EXP ( -PPMOD(5)*PPMOD(5) )
         UPMOD(6) = PPMOD(6) * RAD2DG
         UPMOD(7) = PPMOD(7) * 2.0
         UPMOD(8) = EXP(PPMOD(8))
         UPMOD(9) = PPMOD(9) / POSSK(S)
         UPMOD(10) = PPMOD(10) / POSSK(S)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(S)
         UEMOD(3) = PEMOD(3) / POSSK(S)
         UEMOD(4) = EXP ( PPMOD(4) ) * PEMOD(4) / FWHM(S)
         UEMOD(5) = EXP (-PPMOD(5)*PPMOD(5))*2.0*ABS(PPMOD(5))*PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG
         UEMOD(7) = PEMOD(7) * 2.0
         UEMOD(8) = EXP(PPMOD(8)) * PEMOD(8)
         UEMOD(9) = PEMOD(9) / POSSK(S)
         UEMOD(10) = PEMOD(10) / POSSK(S)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'Maj Axis'
         UMOD(4) = 'mas     '
         CMOD(5) = 'Ax.Ratio'
         UMOD(5) = 'min/max '
         CMOD(6) = 'Maj P.A.'
         UMOD(6) = 'deg N->E'
         CMOD(7) = 'alpha   '
         UMOD(7) = '----    '
         CMOD(8) = 'flux    '
         UMOD(8) = 'Jy      '
         CMOD(9) = 'E-offset'
         UMOD(9) = 'mas     '
         CMOD(10) = 'N-offset'
         UMOD(10) = 'mas     '
         IF (S.EQ.1) THEN
            UMOD(2) = 'asec    '
            UMOD(3) = 'asec    '
            UMOD(4) = 'asec    '
            UMOD(9) = 'asec    '
            UMOD(10) = 'asec    '
            END IF
         END IF
      RETURN
      END
      SUBROUTINE MUKOLS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
C                                       GET SKY OFFSETS MODEL
      CALL STDSKI (XMGOB(2), XSGOB(1))
      CALL STDSKI (XMGOB(9), XSGOB(4))
C                                       PREP SKY BRIGHTNESS MODEL
      CALL STDELL (4, XMGOB(4), XSGOB(7) )
      RETURN
      END
      SUBROUTINE MUKOLM (U, V, W, UU, UV, VV, MODC, XMGOB, XSGOB, LGOB,
     *   LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, UV, VV, XMGOB(*), XSGOB(*)
      REAL      MODD
      COMPLEX   MODC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      REAL      ALPHA, DPHI, DPHI1, DPHI2, DPHI3, DPHI4, DPHI5, DPHI0,
     *   A, B, L2LUX, S2YP, D2AST, D2ORT, L1LUX, S1YP, D1AST, D1ORT
      COMPLEX   T1MC, M1DTC, M1DC, T2MC, M2DTC, M2DC
      COMPLEX CI
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      L1LUX = XMGOB(1)
      ALPHA = XMGOB(7)
      L2LUX = XMGOB(8)
C                                       assemble sky offset model
      CALL APPSKI (XMGOB(2), XSGOB(1), U, V, W, S1YP, D1AST, D1ORT)
      CALL APPSKI (XMGOB(9), XSGOB(4), U, V, W, S2YP, D2AST, D2ORT)
C                                       assemble sky brightness model
      CALL APPELL (XSGOB(7), UU, UV, VV, DPHI, DPHI1, DPHI2, DPHI3)
C                                       assemble derivatives of Dphi here
C
C                                       Above small inner scale
      B = ALPHA - 1.0
      A = DPHI ** B
      DPHI0 = -0.5 * A * DPHI
      DPHI1 = -0.5 * A * DPHI1 * ALPHA
      DPHI2 = -0.5 * A * DPHI2 * ALPHA
      DPHI3 = -0.5 * A * DPHI3 * ALPHA
      DPHI4 = -0.5 * A * DPHI * LOG(DPHI)
      DPHI5 = 0.0

      MODD = EXP(DPHI0)
C                                       Outputs here
      T1MC = L1LUX + CI * S1YP
      T2MC = L2LUX + CI * S2YP
      M1DTC = MODD * CEXP(T1MC)
      M2DTC = MODD * CEXP(T2MC)
      M1DC = MODC * M1DTC
      M2DC = MODC * M2DTC
      T1MC = CI * M1DC
      T2MC = CI * M2DC
      MODC = M1DC + M2DC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) =              M1DC
         IF (LGOB(2)) GRAD(2) =      D1AST * T1MC
         IF (LGOB(3)) GRAD(3) =      D1ORT * T1MC
         IF (LGOB(4)) GRAD(4) =      DPHI1 * MODC
         IF (LGOB(5)) GRAD(5) =      DPHI2 * MODC
         IF (LGOB(6)) GRAD(6) =      DPHI3 * MODC
         IF (LGOB(7)) GRAD(7) =      DPHI4 * MODC
         IF (LGOB(8)) GRAD(8) =              M2DC
         IF (LGOB(9)) GRAD(9) =      D2AST * T2MC
         IF (LGOB(10)) GRAD(10) =    D2ORT * T2MC
         END IF
 999  RETURN
      END
      SUBROUTINE GAINS (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
C   Transformations of the model gains are carried out here
C-----------------------------------------------------------------------
      CHARACTER OP*(*), CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INTEGER NMOD
      INTEGER I
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       first element in UPMOD states length
C                                       of UPMOD vector to load into
C                                       PPMOD
         NMOD = UPMOD(1)
         DO 10 I = 1,NMOD
            PPMOD(I) = -LOG(UPMOD(1+I))
 10         CONTINUE
      ELSE IF (OP(1:7).EQ.'To-User') THEN
         NMOD = PPMOD(1)
         DO 20 I = 1,NMOD
            UPMOD(I) = EXP(-PPMOD(1+I))
            UEMOD(I) = EXP(-PPMOD(1+I)) * ABS(PEMOD(I))
            CMOD(I) = 'Relative'
            UMOD(I) = 'Gain    '
 20         CONTINUE
         END IF
      RETURN
      END
C THERE IS NO CODE BELOW THIS LINE THAT I WILL VOUCH FOR!!!!!!!!
C                                       ZEMAN models a polarized
C                                       ZEEMAN split point source
C                                       this model is essentially
C                                       untested!
      SUBROUTINE ZEMANN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 8
      SMOD = 5 + NMOD + 7
      RETURN
      END
      SUBROUTINE ZEMAND (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*), CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = UPMOD(1)
         PPMOD(2) = UPMOD(2)
         PPMOD(3) = UPMOD(3)
         PPMOD(4) = UPMOD(4)
         PPMOD(5) = UPMOD(5)
         PPMOD(6) = UPMOD(6) * 2.0 / RAD2DG
         PPMOD(7) = UPMOD(7) * POSSK(2)
         PPMOD(8) = UPMOD(8) * POSSK(2)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = PPMOD(1)
         UPMOD(2) = PPMOD(2)
         UPMOD(3) = PPMOD(3)
         UPMOD(4) = PPMOD(4)
         UPMOD(5) = PPMOD(5)
         UPMOD(6) = PPMOD(6) * RAD2DG / 2.0
         UPMOD(7) = PPMOD(7) / POSSK(2)
         UPMOD(8) = PPMOD(8) / POSSK(2)
C                                       compute external error bars
         UEMOD(1) = PEMOD(1)
         UEMOD(2) = PEMOD(2)
         UEMOD(3) = PEMOD(3)
         UEMOD(4) = PEMOD(4)
         UEMOD(5) = PEMOD(5)
         UEMOD(6) = PEMOD(6) * RAD2DG / 2.0
         UEMOD(7) = PEMOD(7) / POSSK(2)
         UEMOD(8) = PEMOD(8) / POSSK(2)
C                                       assign labels and units
         CMOD(1) = 'G flux  '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'L flux  '
         UMOD(2) = 'Jy      '
         CMOD(3) = 'Nat-Freq'
         UMOD(3) = 'MHz     '
         CMOD(4) = 'Frq-Shft'
         UMOD(4) = 'MHz     '
         CMOD(5) = 'Frq-FWHM'
         UMOD(5) = 'MHz     '
         CMOD(6) = 'Mix-Ang '
         UMOD(6) = 'Degrees '
         CMOD(7) = 'E-offset'
         UMOD(7) = 'mas     '
         CMOD(8) = 'N-offset'
         UMOD(8) = 'mas     '
         END IF
      RETURN
      END
      SUBROUTINE ZEMANS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C                                       Standard sky constants
C-----------------------------------------------------------------------
      CALL STDSKI ( XMGOB(7), XSGOB(1) )
      XSGOB(4) = COS(XMGOB(6))
      XSGOB(5) = SIN(XMGOB(6))
      XSGOB(6) = (1.0 + XMGOB(4)*XMGOB(4))/2.0
      XSGOB(7) = XMGOB(5)*XMGOB(5)
      RETURN
      END
      SUBROUTINE ZEMANM (FREQ, STOK, U, V, W, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   STOK
      DOUBLE PRECISION FREQ, U, V, W, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
C
      INCLUDE 'ZINFO.INC'
      REAL      GFLUX, LFLUX, WFREQ, YPLU, YMIN, YZER, MANGL, CMANGL,
     *   SMANGL, TMANGL, OMANGL, GPLU, GMIN, GZER, LPLU, LMIN, LZER,
     *   SPLU, SMIN, SZER, FLUX, SKYP, DEAST, DNORT
      COMPLEX   CI, TEMC
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
C                                       Accept only stokes I and V
      IF ((STOK.NE.1).AND.(STOK.NE.4)) GO TO 999
C                                       XGOB(6) is the Gaussian line profile peak value (Log)
C                                       XGOB(7) is the Lorentzian line profile peak value (Log)
C                                       XGOB(8) is the center frequency of the unshifted line
C                                       XGOB(9) is the amount of the shift
C                                       XGOB(10) is the frequency width of the line profile
C                                       XGOB(11) is magnetic field angle with the line of sight
C                                       XGOB(12,13) are the standard sky offsets in ra,dec
      GFLUX = XMGOB(1)
      LFLUX = XMGOB(2)
C                                       prevent division by zero!
      WFREQ  = ABS(XMGOB(5)) + 1.0E-6
C                                       Prep Freq. stuff here
      YPLU   = (FREQ - XMGOB(3) - XMGOB(4))/WFREQ
      YZER   = (FREQ - XMGOB(3)          )/WFREQ
      YMIN   = (FREQ - XMGOB(3) + XMGOB(4))/WFREQ
C                                       mixing angle
      MANGL  = XMGOB(5)
      CMANGL = XSGOB(4)
      SMANGL = XSGOB(5)
      TMANGL = XSGOB(6)
      OMANGL = XSGOB(7)
C                                       GET SKY OFFSETS MODEL
      CALL APPSKI ( XMGOB(7), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C
C                                       Prep Flux stuff here
      GPLU   = EXP ( -0.5 * YPLU**2 )
      GZER   = EXP ( -0.5 * YZER**2 )
      GMIN   = EXP ( -0.5 * YMIN**2 )
      LPLU   = 1.0 / ( 1 + 4 * YPLU**2 )
      LZER   = 1.0 / ( 1 + 4 * YZER**2 )
      LMIN   = 1.0 / ( 1 + 4 * YMIN**2 )
C                                       prep flux model here
      SPLU = GPLU * GFLUX + LPLU * LFLUX
      SMIN = GMIN * GFLUX + LMIN * LFLUX
      IF (STOK.EQ.1) SZER = GZER * GFLUX + LZER * LFLUX
C                                       assemble flux model here
      IF (STOK.EQ.1) FLUX = TMANGL*(SPLU+SMIN) + OMANGL*SZER
      IF (STOK.EQ.4) FLUX = CMANGL*(SPLU-SMIN) / 2.0

      TEMC = CI * SKYP
      MODTC = CEXP(TEMC)
      TEMC = MODC * MODTC
      MODTC = MODTC * FLUX
      MODC = MODTC * MODC
C                                       assemble gradient here
      IF (LGRAD) THEN
C                                       do standard sky position offsets first
         IF (LGOB(8)) GRAD(8) = DNORT * CI * MODC
         IF (LGOB(7)) GRAD(7) = DEAST * CI * MODC
C                                       use original SPLU,SMIN,SZER
C                                       for #6
         IF (LGOB(6)) THEN
            IF (STOK.EQ.1) GRAD(6) = CMANGL*SMANGL*(2*SZER - SPLU+SMIN)
            IF (STOK.EQ.4) GRAD(6) = SMANGL*(SMIN - SPLU)/2.0
            END IF
C                                       prep for #3,#4,#5
         IF (LGOB(3).OR.LGOB(4).OR.LGOB(5)) THEN
            SPLU = (GPLU * GFLUX + 8*LPLU*LPLU * LFLUX) * YPLU/WFREQ
            SMIN = (GMIN * GFLUX + 8*LMIN*LMIN * LFLUX) * YMIN/WFREQ
            IF (STOK.EQ.1) SZER = (GZER * GFLUX + 8*LZER*LZER * LFLUX)
     *                                                  * YZER/WFREQ
            END IF
         IF (STOK.EQ.1) THEN
            IF (LGOB(1)) GRAD(1) = TEMC * (TMANGL*(GPLU+GMIN) +
     *                                      OMANGL*GZER)
            IF (LGOB(2)) GRAD(2) = TEMC * (TMANGL*(LPLU+LMIN) +
     *                                      OMANGL*LZER)
            IF (LGOB(3)) GRAD(3) = TEMC * (TMANGL*(SPLU+SMIN) +
     *                                      OMANGL*SZER)
            IF (LGOB(4)) GRAD(4) = TEMC * (TMANGL*(SPLU-SMIN) +
     *                                      OMANGL*SZER)
            IF (LGOB(5)) GRAD(5) = TEMC * (TMANGL*(SPLU*YPLU+SMIN*YMIN)
     *                                   +   OMANGL*SZER*YZER)
            END IF
         IF (STOK.EQ.4) THEN
            IF (LGOB(1)) GRAD(1) = TEMC * CMANGL * (GPLU-GMIN)/2.0
            IF (LGOB(2)) GRAD(2) = TEMC * CMANGL * (LPLU-LMIN)/2.0
            IF (LGOB(3)) GRAD(3) = TEMC * CMANGL * (SPLU-SMIN)/2.0
            IF (LGOB(4)) GRAD(4) = TEMC * CMANGL * (SPLU+SMIN)/2.0
            IF (LGOB(5)) GRAD(5) = TEMC * CMANGL * (SPLU*YPLU+
     *                                               SMIN*YMIN)/2.0
            END IF
         END IF
 999  RETURN
      END
C                                       The HALO model is untested
      SUBROUTINE HALOSN (NMOD, SMOD)
C-----------------------------------------------------------------------
      INTEGER   NMOD, SMOD
C-----------------------------------------------------------------------
      NMOD = 6
      SMOD = 5 + NMOD + 3
      RETURN
      END
      SUBROUTINE HALOSD (OP, PPMOD, PEMOD, UPMOD, UEMOD, CMOD, UMOD)
C-----------------------------------------------------------------------
      CHARACTER OP*(*),  CMOD(*)*8, UMOD(*)*8
      DOUBLE PRECISION PPMOD(*), PEMOD(*), UPMOD(*), UEMOD(*)
      INCLUDE 'ZINFO.INC'
C-----------------------------------------------------------------------
      IF (OP(1:7).EQ.'To-Prog') THEN
C                                       convert to internal units
         PPMOD(1) = LOG(UPMOD(1))
         PPMOD(2) = UPMOD(2) * POSSK(2)
         PPMOD(3) = UPMOD(3) * POSSK(2)
         PPMOD(4) = UPMOD(4)
         PPMOD(5) = UPMOD(5)
         PPMOD(6) = UPMOD(6)
      ELSE IF (OP(1:7).EQ.'To-User') THEN
C                                       convert to external units
         UPMOD(1) = EXP(PPMOD(1))
         UPMOD(2) = PPMOD(2) / POSSK(2)
         UPMOD(3) = PPMOD(3) / POSSK(2)
         UPMOD(4) = PPMOD(4)
         UPMOD(5) = PPMOD(5)
         UPMOD(6) = PPMOD(6)
C                                       compute external error bars
         UEMOD(1) = EXP(PPMOD(1)) * PEMOD(1)
         UEMOD(2) = PEMOD(2) / POSSK(2)
         UEMOD(3) = PEMOD(3) / POSSK(2)
         UEMOD(4) = PEMOD(4)
         UEMOD(5) = PEMOD(5)
         UEMOD(6) = PEMOD(6)
C                                       assign labels and units
         CMOD(1) = 'flux    '
         UMOD(1) = 'Jy      '
         CMOD(2) = 'E-offset'
         UMOD(2) = 'mas     '
         CMOD(3) = 'N-offset'
         UMOD(3) = 'mas     '
         CMOD(4) = 'A-1 par '
         UMOD(4) = 'm^-1    '
         CMOD(5) = 'A-1 par '
         UMOD(5) = 'm^-2    '
         CMOD(6) = 'A-1 par '
         UMOD(6) = 'm^-3    '
         END IF
      RETURN
      END
      SUBROUTINE HALOSS (XMGOB, XSGOB)
C-----------------------------------------------------------------------
      DOUBLE PRECISION XMGOB(*), XSGOB(*)
C-----------------------------------------------------------------------
      CALL STDSKI ( XMGOB(2), XSGOB(1) )
      RETURN
      END
      SUBROUTINE HALOSM (U, V, W, UU, VV, MODC, MODTC, XMGOB, XSGOB,
     *   LGOB, LGRAD, GRAD)
C-----------------------------------------------------------------------
      DOUBLE PRECISION U, V, W, UU, VV, XMGOB(*), XSGOB(*)
      COMPLEX   MODC, MODTC, GRAD(*)
      LOGICAL   LGRAD, LGOB(*)
      INCLUDE 'ZINFO.INC'
      COMPLEX   CI, TEMC
      REAL      LFLUX, SKYP, DEAST, DNORT, P1, P2, P3, DPHI, WTEMP
C
      PARAMETER (CI = (0.0,1.0))
C-----------------------------------------------------------------------
      LFLUX  = XMGOB(1)
      P1 = XMGOB(4)
      P2 = XMGOB(5)
      P3 = XMGOB(6)
C                                       get standard sky
      CALL APPSKI ( XMGOB(2), XSGOB(1), U, V, W, SKYP, DEAST, DNORT)
C
      WTEMP = SQRT(UU+VV)
      DPHI = (P1 + P2*( P3/WTEMP )/WTEMP )/WTEMP
C                                       Outputs here
      TEMC = LFLUX + CI * SKYP
      MODTC = CEXP(TEMC)
      MODC  = MODC * MODTC
      IF (LGRAD) THEN
         IF (LGOB(1)) GRAD(1) = DPHI *         MODC
         IF (LGOB(2)) GRAD(2) = DPHI * DEAST * CI * MODC
         IF (LGOB(3)) GRAD(3) = DPHI * DNORT * CI * MODC
         IF (LGOB(4)) GRAD(4) = MODC / WTEMP
         IF (LGOB(5)) GRAD(5) = MODC / (WTEMP*WTEMP)
         IF (LGOB(6)) GRAD(6) = MODC / (WTEMP*WTEMP*WTEMP)
         END IF
      MODC = MODC * DPHI
      MODTC = MODTC * DPHI
C
 999  RETURN
      END
      SUBROUTINE LAMFCN (NU, Z, LAM, GETDDZ, DDZ, GETDDN, DDN)
C-----------------------------------------------------------------------
C  LAMFCN evaluates the Lambda function, and if requested, its derivativ
C  with respect to argument.
C
C  The Lambda function is defined as:
C
C  \Lambda_\nu(z) = \Gamma(\nu+1)(2/z)^\nu J_\nu(z)
C
C  where J_\nu(z) is the bessel function of the first kind.
C
C  It is assumed that NU,Z > 0.0 - if this is not the case,
C  the may be division by zero problems...
C-----------------------------------------------------------------------
      DOUBLE PRECISION NU, Z
      DOUBLE PRECISION LAM, DDZ, DDN
      LOGICAL GETDDN, GETDDZ
      DOUBLE PRECISION GAMLN, PSI1PX
      DOUBLE PRECISION PI, PIO2, ZERO, HALF, ONE, TWO, THREE, FOUR, HLPI
      DOUBLE PRECISION LFAC, ZFAC, NFAC, MAXFAC
      DOUBLE PRECISION NUP1, NUPPF
      DOUBLE PRECISION TWOZ, ZO2, Z2O4, LZO2, ZARG, CZARG, SZARG
      DOUBLE PRECISION EPSIL, SEAM, Q, FNU
      DOUBLE PRECISION AP,BP,CP,DP, FNU2
      INTEGER QCRIT, DQ
      PARAMETER (ZERO = 0.0D0)
      PARAMETER (HALF = 0.5D0)
      PARAMETER (ONE = 1.0D0)
      PARAMETER (TWO = 2.0D0)
      PARAMETER (THREE = 3.0D0)
      PARAMETER (FOUR = 4.0D0)
      PARAMETER (PI = 3.14159265358979323846D0)
      PARAMETER (PIO2 = PI/TWO)
      PARAMETER (EPSIL = 1.0D-15)
      SEAM = 20.0D0
      HLPI = DLOG(PI)/TWO
C-----------------------------------------------------------------------
      NUP1 = NU + ONE
      ZO2 = Z / TWO
      Z2O4 = ZO2*ZO2
C                                       default is to use the first two
C                                       terms of the ascending series ex
      DDZ = -ZO2/NUP1
      DDN = DDZ*DDZ
      LAM = 1+ZO2*DDZ
C                                       If Z is _VERY_ large, use
C                                       the asymptotic expansion in larg
      IF (Z.GT.SEAM) THEN
         LFAC = ZERO
         ZFAC = ZERO
         NFAC = ZERO
         NUPPF = NU + HALF
         ZARG  = Z - NUPPF*PIO2
         TWOZ  = TWO * Z
         LAM  = ZERO
         DDZ  = ZERO
         DDN  = ZERO
         LZO2 = LOG(ZO2)
C                                       do the Q=0 term
         DQ   = 0
         Q = DBLE(DQ)
         CZARG = DCOS(ZARG)
         SZARG = DSIN(ZARG)
         AP = DEXP(GAMLN(NUP1)) / ( ZO2**NU * DSQRT(PI*ZO2) )
         BP = FNU(NU,DQ)/(Z*Q+Z)
         DP = PSI1PX(NU) - LZO2
 50      CONTINUE
C                                       compute rest of multipliers
         LFAC = CZARG - SZARG * BP
         IF (GETDDZ) THEN
            ZFAC = -(NUPPF+Q)*LFAC/Z + SZARG*(BP/Z - ONE) - BP*CZARG
            ZFAC = -ZFAC
            END IF
         IF (GETDDN) THEN
            NFAC = LFAC*DP + PIO2*(SZARG+BP*CZARG) - NU*SZARG/(Z*Q+ONE)
            END IF
C                                       add to sum
         LAM = LAM + LFAC * AP
         DDZ = DDZ + ZFAC * AP
         DDN = DDN + NFAC * AP
C                                       check if more terms are necessar
         MAXFAC = ABS(AP) * MAX ( ABS(LFAC), ABS(NFAC), ABS(ZFAC) )
         IF (MAXFAC.GT.EPSIL) THEN
C                                       prep AP,BP
            DP = DP + NU/FNU(NU,DQ)
            DQ = DQ + 1
            Q = DBLE(DQ)
            AP = - AP * BP * FNU(NU,DQ)/(Z*Q+Z)
            DP = DP + NU/FNU(NU,DQ)
            DQ = DQ + 1
            Q = DBLE(DQ)
            BP = FNU(NU,DQ)/(Z*Q+Z)
            GO TO 50
            END IF
      ELSE IF (Z.GT.1.0D-6) THEN
C                                       prep for not large z expansion
         LFAC = ZERO
         ZFAC = ZERO
         NFAC = ZERO
         AP = DSQRT(Z*Z+NU*NU) - NU
         QCRIT = AP/TWO + 1
         LAM = ZERO
         DDZ = ZERO
         DDN = ZERO
C                                       prep for the DQ=0 term
         DQ = 0
         Q = DBLE(DQ)
         AP = ONE
         BP = FNU2(NU,DQ,Z2O4)
         IF (GETDDN) THEN
            CP = ZERO
            DP = ONE/NUP1
            END IF
 100     CONTINUE
C                                       do the lambda sum
         LFAC = ONE - BP
         LAM = LAM + LFAC * AP
C                                       do the dlamda/dz sum
         IF (GETDDZ) THEN
            ZFAC = (Q*LFAC - BP)/ZO2
            DDZ = DDZ + ZFAC*AP
            END IF
C                                       do the dlamda/dnu sum
         IF (GETDDN) THEN
            NFAC = BP*DP - CP
            DDN = DDN + NFAC*AP
            END IF
C                                       check if more terms are necessar
         MAXFAC = ABS(AP) * MAX ( ABS(LFAC), ABS(NFAC), ABS(ZFAC) )
         IF ((MAXFAC.GT.EPSIL).OR.(DQ.LE.QCRIT)) THEN
            DQ = DQ + 1
            AP = AP * BP * FNU2(NU,DQ,Z2O4)
            DQ = DQ + 1
            BP = FNU2(NU,DQ,Z2O4)
            Q = DBLE(DQ)
            IF (GETDDN) THEN
               CP = DP + ONE/(NU+Q)
               DP = CP + ONE/(NUP1+Q)
               END IF
C                                       ready
            GO TO 100
            END IF
         END IF
C
 999  RETURN
      END
      DOUBLE PRECISION FUNCTION FNU2(NU,P,Z2O4)
C-----------------------------------------------------------------------
      DOUBLE PRECISION NU, X, Z2O4
      INTEGER P
C-----------------------------------------------------------------------
      X = DBLE(P+1)
      X = X*(X+NU)
      FNU2 = Z2O4/X
      RETURN
      END
      DOUBLE PRECISION FUNCTION FNU(NU,P)
      DOUBLE PRECISION NU, X
      INTEGER P
      X = DBLE(P) + 0.5D0
      FNU = ( NU*NU - X*X ) / 2.0D0
      RETURN
      END
      DOUBLE PRECISION FUNCTION PSIDIF(X,N)
C-----------------------------------------------------------------------
C   PSIDIF calculates PSI(N+X) - PSI(1+X)
C   Assume that X >=0 and N >= 1
C   assume that 1+X>=0 , N>=1
C   This uses A&S formula 6.3.6
C-----------------------------------------------------------------------
      DOUBLE PRECISION X
      INTEGER N, MYN
C-----------------------------------------------------------------------
      PSIDIF = 0.0D0
      MYN = N - 1
 100  CONTINUE
      IF (MYN.GT.0) THEN
         PSIDIF = PSIDIF + 1.0D0/(X+DBLE(MYN))
         MYN = MYN - 1
         GO TO 100
         END IF
      RETURN
      END
      DOUBLE PRECISION FUNCTION PSI1PX(X)
C-----------------------------------------------------------------------
C   PSI1PX calculates the PSI function of (1+X) using the recurrence
C   relation 6.3.5 in Abramowitz & Stegun and by calling PSITAB
C   which evaluates PSI from 1+X=1 to 1+X=2 by interpolation
C
      DOUBLE PRECISION X, MYX, ONE, PSITAB
      PARAMETER (ONE = 1.0D0)
C-----------------------------------------------------------------------
      PSI1PX = 0.0D0
      MYX = X
      IF (MYX .LT. 0.0D0) THEN
         PRINT *, ' ERROR, DIGAMMA function argument <=0'
         PSI1PX = - 9999.0D0
         GO TO 990
         END IF
 100  CONTINUE
      IF (MYX .GT. ONE) THEN
         PSI1PX = PSI1PX + ONE / MYX
         MYX = MYX - ONE
         GO TO 100
         END IF
C                                       ZERO .LT. MYX .LE. ONE
      PSI1PX = PSI1PX + PSITAB (MYX)
 990  CONTINUE
      RETURN
      END
      DOUBLE PRECISION FUNCTION PSITAB (X)
C-----------------------------------------------------------------------
C   PSITAB returns an interpolated value for PSI(1+X) using table
C   6.1 in Abramowitz & Stegun. - based upon a routine by B. Butler
C   It is assumed that 0.0 <= X <= 1.0
C-----------------------------------------------------------------------
      DOUBLE PRECISION TABPSI(201), X, DELX
      INTEGER I
      DATA ( TABPSI(I), I = 1 , 10 ) /
C 1.000 to 1.045
     *       -0.5772156649, -0.5690209113, -0.5608854579,
     *       -0.5528085156, -0.5447893105,
     *       -0.5368270828, -0.5289210873, -0.5210705921,
     *       -0.5132748789, -0.5055332428/
      DATA ( TABPSI(I), I = 11 , 20 ) /
C 1.050 to 1.095
     *       -0.4978449913, -0.4902094448, -0.4826259358,
     *       -0.4750938088, -0.4676124199,
     *       -0.4601811367, -0.4527993380, -0.4454664135,
     *       -0.4381817635, -0.4309447988/
      DATA ( TABPSI(I), I = 21 , 30 ) /
C 1.100 to 1.145
     *       -0.4237549404, -0.4166116193, -0.4095142761,
     *       -0.4024623611, -0.3954553339,
     *       -0.3884926633, -0.3815738268, -0.3746983110,
     *       -0.3678656106, -0.3610752291/
      DATA ( TABPSI(I), I = 31 , 40 ) /
C 1.150 to 1.195
     *       -0.3543266780, -0.3476194768, -0.3409531528,
     *       -0.3343272413, -0.3277412847,
     *       -0.3211948332, -0.3146874438, -0.3082186809,
     *       -0.3017881156, -0.2953953259/
      DATA ( TABPSI(I), I = 41 , 50 ) /
C 1.200 to 1.245
     *       -0.2890398966, -0.2827214187, -0.2764394897,
     *       -0.2701937135, -0.2639837000,
     *       -0.2578090652, -0.2516694307, -0.2455644243,
     *       -0.2394936791, -0.2334568341/
      DATA ( TABPSI(I), I = 51 , 60 ) /
C 1.250 to 1.295
     *       -0.2274535334, -0.2214834266, -0.2155461686,
     *       -0.2096414193, -0.2037688437,
     *       -0.1979281118, -0.1921181118, -0.1863408828,
     *       -0.1805937494, -0.1748771870/
      DATA ( TABPSI(I), I = 61 , 70 ) /
C 1.300 to 1.345
     *       -0.1691908889, -0.1635345526, -0.1579078803,
     *       -0.1523105782, -0.1467423568,
     *       -0.1412029305, -0.1356920180, -0.1302093416,
     *       -0.1247546279, -0.1193276069/
      DATA ( TABPSI(I), I = 71 , 80 ) /
C 1.350 to 1.395
     *       -0.1139280127, -0.1085555827, -0.1032100582,
     *       -0.0978911840, -0.0925987082,
     *       -0.0873323825, -0.0820919619, -0.0768772046,
     *       -0.0716878723, -0.0665237297/
      DATA ( TABPSI(I), I = 81 , 90 ) /
C 1.400 to 1.445
     *       -0.0613845446, -0.0562700879, -0.0511801337,
     *       -0.0461144589, -0.0410728433,
     *       -0.0360550697, -0.0310609237, -0.0260901935,
     *       -0.0211426703, -0.0162181479/
      DATA ( TABPSI(I), I = 91 , 100 ) /
C 1.450 to 1.495
     *       -0.0113164226, -0.0064372934, -0.0015805620,
     *       +0.0032539677, +0.0080664890,
     *       +0.0128571930, +0.0176262684, +0.0223739013,
     *       +0.0271002758, +0.0318055736/
      DATA ( TABPSI(I), I = 101 , 110 ) /
C 1.500 to 1.545
     *       +0.0364899740, +0.0411536543, +0.0457967896,
     *       +0.0504195527, +0.0550221146,
     *       +0.0596046439, +0.0641673074, +0.0687102697,
     *       +0.0732336936, +0.0777377400/
      DATA ( TABPSI(I), I = 111 , 120 ) /
C 1.550 to 1.595
     *       +0.0822225675, +0.0866883334, +0.0911351925,
     *       +0.0955632984, +0.0999728024,
     *       +0.1043638544, +0.1087366023, +0.1130911923,
     *       +0.1174277690, +0.1217464754/
      DATA ( TABPSI(I), I = 121 , 130 ) /
C 1.600 to 1.645
     *       +0.1260474528, +0.1303308407, +0.1345967772,
     *       +0.1388453988, +0.1430768404,
     *       +0.1472912354, +0.1514887158, +0.1556694120,
     *       +0.1598334529, +0.1639809660/
      DATA ( TABPSI(I), I = 131 , 140 ) /
C 1.650 to 1.695
     *       +0.1681120776, +0.1722269122, +0.1763255933,
     *       +0.1804082427, +0.1844749813,
     *       +0.1885259282, +0.1925612015, +0.1965809180,
     *       +0.2005851931, +0.2045741410/
      DATA ( TABPSI(I), I = 141 , 150 ) /
C 1.700 to 1.745
     *       +0.2085478749, +0.2125065064, +0.2164501462,
     *       +0.2203789037, +0.2242928871,
     *       +0.2281922037, +0.2320769593, +0.2359472589,
     *       +0.2398032061, +0.2436449038/
      DATA ( TABPSI(I), I = 151 , 160 ) /
C 1.750 to 1.795
     *       +0.2474724535, +0.2512859559, +0.2550855103,
     *       +0.2588712154, +0.2626431686,
     *       +0.2664014664, +0.2701462043, +0.2738774769,
     *       +0.2775953776, +0.2812999992/
      DATA ( TABPSI(I), I = 161 , 170 ) /
C 1.800 to 1.845
     *       +0.2849914333, +0.2886697707, +0.2923351012,
     *       +0.2959875138, +0.2996270966,
     *       +0.3032539367, +0.3068681205, +0.3104697335,
     *       +0.3140588602, +0.3176355846/
      DATA ( TABPSI(I), I = 171 , 180 ) /
C 1.850 to 1.895
     *       +0.3211999895, +0.3247521572, +0.3282921691,
     *       +0.3318201056, +0.3353360467,
     *       +0.3388400713, +0.3423322577, +0.3458126835,
     *       +0.3492814255, +0.3527385596/
      DATA ( TABPSI(I), I = 181 , 190 ) /
C 1.900 to 1.945
     *       +0.3561841612, +0.3596183049, +0.3630410646,
     *       +0.3664525136, +0.3698527244,
     *       +0.3732417688, +0.3766197179, +0.3799866424,
     *       +0.3833426119, +0.3866876959/
      DATA ( TABPSI(I), I = 191 , 201 ) /
C 1.950 to 2.000
     *       +0.3900219627, +0.3933454805, +0.3966583163,
     *       +0.3999605371, +0.4032522088,
     *       +0.4065333970, +0.4098041664, +0.4130645816,
     *       +0.4163147060, +0.4195546030,
     *       +0.4227843351 /
C-----------------------------------------------------------------------
      I = INT ( 200.0D0 * X )
      DELX = 200.0D0 * X - DBLE(I)
      PSITAB = TABPSI(1+I) + DELX * (TABPSI(2+I) - TABPSI(1+I))
      RETURN
      END
      DOUBLE PRECISION FUNCTION GAMLN(X)
C
C         A CDC 6600 SUBROUTINE
C
C     AUTHORS
C         D.E. AMOS AND S.L. DANIEL
C         ALBUQUERQUE, NEW MEXICO, 87115
C         JANUARY, 1975
C
C     REFERENCES
C         ABRAMOWITZ, M. AND STEGUN, I.A. HANDBOOK OF MATHEMATICAL
C         FUNCTIONS. NBS APPLIED MATHEMATICS SERIES 55, U.S. GOVERNMENT
C         PRINTING OFFICE, WASHINGTON, D.C., CHAPTER 6.
C
C         AMOS, D.E., DANIEL, S.L. AND WESTON, M.K. CDC 6600
C         SUBROUTINES IBESS AND JBESS FOR BESSEL FUNCTIONS
C         I/SUB(NU)/(X) AND J/SUB(NU)/(X), X.GE.0, NU.GE.0.
C         ACM TRANS. MATH. SOFTWARE, MARCH, 1977.
C
C         HART, J.F., ET. AL. COMPUTER APPROXIMATIONS, WILEY, NEW YORK.
C         PP. 130-136, 1968.
C
C     ABSTRACT
C         GAMLN COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR
C         X.GT.0. A RATIONAL CHEBYSHEV APPROXIMATION IS USED ON
C         8.LT.X.LT.1000., THE ASYMPTOTIC EXPANSION FOR X.GE.1000. AND
C         BACKWARD RECURSION FOR 0.LT.X.LT.8 FOR NON-INTEGRAL X. FOR
C         X=1.,...,8., GAMLN IS SET TO NATURAL LOGS OF FACTORIALS.
C
C     DESCRIPTION OF ARGUMENTS
C
C         INPUT
C           X      - X.GT.0
C
C         OUTPUT
C           GAMLN  - NATURAL LOG OF THE GAMMA FUNCTION AT X
C
C     ERROR CONDITIONS
C         IMPROPER INPUT ARGUMENT - A FATAL ERROR
      INTEGER NDX, I
      DOUBLE PRECISION X, DX, XLIM1, XLIM2, Q(2), RTWPIL, G(8), P(5),
     *                 XZ, TX, FK, DNDX, RX, SXK, SUM, PX, RXX, DXZ,
     *                 RXZ, XK
      DATA XLIM1, XLIM2, RTWPIL / 8.0D0, 1000.0D0, 9.18938533204673D-01/
      DATA G / 8.52516136106541D+00, 6.57925121201010D+00,
     1 4.78749174278205D+00, 3.17805383034795D+00, 1.79175946922806D+00,
     2 6.93147180559945D-01, 2 * 0.0D0 /
      DATA  P / 7.66345188000000D-04,-5.94095610520000D-04,
     1 7.93643110484500D-04,-2.77777775657725D-03, 8.33333333333169D-02/
      DATA  Q              /-2.77777777777778D-03, 8.33333333333333D-02/
      IF (X) 140, 140, 10
   10 DX = X - XLIM1
      IF (DX) 20, 110, 80
   20 IF (X-1.0D0) 30, 130, 40
   30 XZ = X + 8.0D0
      TX = X
      FK = -0.5D0
      NDX = 7
      GO TO 50
   40 DX = ABS(DX)
      NDX = DX
      DNDX = NDX
      NDX = NDX + 1
      IF ((DNDX-DX).EQ.0.0D0) GO TO 120
      XZ = X + DNDX + 1.0D0
      TX = 1.0D0
      FK = 0.5D0
   50 DXZ = XZ
      RXZ = 1.0D0/DXZ
      RX = RXZ
      RXX = RX*RX
      XK = 1.0D0
      DO 60 I=1,NDX
        XK = XK - RXZ
        SXK = XK
        TX = TX*SXK
   60 CONTINUE
      SUM = (X-FK)*DLOG(XZ) - DLOG(TX) - XZ
      PX = P(1)
      DO 70 I=2,5
        PX = PX*RXX + P(I)
   70 CONTINUE
      GAMLN = PX*RX + SUM + RTWPIL
      RETURN
   80 RX = 1.0D0/X
      RXX = RX*RX
      IF ((X-XLIM2).LT.0.0D0) GO TO 90
      PX = Q(1)*RXX + Q(2)
      GAMLN = PX*RX + (X-0.5D0)*DLOG(X) - X + RTWPIL
      RETURN
   90 PX = P(1)
      SUM = (X-0.5D0)*DLOG(X) - X
      DO 100 I=2,5
        PX = PX*RXX + P(I)
  100 CONTINUE
      GAMLN = PX*RX + SUM + RTWPIL
      RETURN
  110 GAMLN = G(1)
      RETURN
  120 GAMLN = G(NDX)
      RETURN
  130 GAMLN = G(8)
      RETURN
  140 PRINT 99999, X
      STOP
99999 FORMAT (' ARGUMENT FOR GAMLN IS <= ZERO, X=', E25.14)
      END

* ======================================================================
* NIST Guide to Available Math Software.
* NeededBLAS for module DGESVD from package LAPACK.
* Retrieved from CAMSUN on Wed Sep 17 19:19:33 1997.
* ======================================================================

* BLAS REQUIRED BY LAPACK ROUTINE: dgesvd
* -----------------------------------------------------------
* Note: Link to BLAS optimized for your system, if available.
* -----------------------------------------------------------
      SUBROUTINE  DCOPY (N, DX, INCX, DY, INCY)
C-----------------------------------------------------------------------
C     copies a vector, x, to a vector, y.
C     uses unrolled loops for increments equal to one.
C     jack dongarra, linpack, 3/11/78.
C     modified 12/3/93, array(1) declarations changed to array(*)
C
      DOUBLE PRECISION DX(*),DY(*)
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C-----------------------------------------------------------------------
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        code for unequal increments or equal increments
C          not equal to 1
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
C        code for both increments equal to 1
C
C
C        clean-up loop
C
   20 M = MOD(N,7)
      IF( M .EQ. 0 ) GO TO 40
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
      DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
C-----------------------------------------------------------------------
*     .. Scalar Arguments ..
      INTEGER                           INCX, N
*     .. Array Arguments ..
      DOUBLE PRECISION                  X( * )
*     ..
*
*  DNRM2 returns the euclidean norm of a vector via the function
*  name, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
*
*     .. Parameters ..
      DOUBLE PRECISION      ONE         , ZERO
      PARAMETER           ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      INTEGER               IX
      DOUBLE PRECISION      ABSXI, NORM, SCALE, SSQ
*     .. Intrinsic Functions ..
      INTRINSIC             ABS, SQRT
*     ..
*     .. Executable Statements ..
      IF( N.LT.1 .OR. INCX.LT.1 )THEN
         NORM  = ZERO
      ELSE IF( N.EQ.1 )THEN
         NORM  = ABS( X( 1 ) )
      ELSE
         SCALE = ZERO
         SSQ   = ONE
*        The following loop is equivalent to this call to the LAPACK
*        auxiliary routine:
*        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
*
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
*
      DNRM2 = NORM
      RETURN
      END
      SUBROUTINE DROT (N, DX, INCX, DY, INCY, C, S)
C-----------------------------------------------------------------------
C     applies a plane rotation.
C     jack dongarra, linpack, 3/11/78.
C     modified 12/3/93, array(1) declarations changed to array(*)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*),DY(*),DTEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
C-----------------------------------------------------------------------
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       code for unequal increments or equal increments not equal
C         to 1
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
C       code for both increments equal to 1
C
   20 DO 30 I = 1,N
        DTEMP = C*DX(I) + S*DY(I)
        DY(I) = C*DY(I) - S*DX(I)
        DX(I) = DTEMP
   30 CONTINUE
      RETURN
      END
      SUBROUTINE  DSCAL (N, DA, DX, INCX)
C-----------------------------------------------------------------------
C     scales a vector by a constant.
C     uses unrolled loops for increment equal to one.
C     jack dongarra, linpack, 3/11/78.
C     modified 3/93 to return if incx .le. 0.
C     modified 12/3/93, array(1) declarations changed to array(*)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DA,DX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
C-----------------------------------------------------------------------
      IF( N.LE.0 .OR. INCX.LE.0 )RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        code for increment not equal to 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
        DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        code for increment equal to 1
C
C        clean-up loop
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
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
      SUBROUTINE  DSWAP (N, DX, INCX, DY, INCY)
C-----------------------------------------------------------------------
C     interchanges two vectors.
C     uses unrolled loops for increments equal one.
C     jack dongarra, linpack, 3/11/78.
C     modified 12/3/93, array(1) declarations changed to array(*)
C-----------------------------------------------------------------------
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C-----------------------------------------------------------------------
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       code for unequal increments or equal increments not equal
C         to 1
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
C       code for both increments equal to 1
C
C       clean-up loop
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
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
      SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX,
     $                   BETA, Y, INCY )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA, BETA
      INTEGER            INCX, INCY, LDA, M, N
      CHARACTER*1        TRANS
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMV  performs one of the matrix-vector operations
*
*     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and A is an
*  m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
*
*              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
*
*              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
*           Before entry, the incremented array X must contain the
*           vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry, BETA specifies the scalar beta. When BETA is
*           supplied as zero then Y need not be set on input.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of DIMENSION at least
*           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
*           Before entry with BETA non-zero, the incremented array Y
*           must contain the vector y. On exit, Y is overwritten by the
*           updated vector y.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
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
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ALPHA.EQ.ZERO ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
*     up the start points in  X  and  Y.
*
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
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
*     First form  y := beta*y.
*
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
     $   RETURN
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  y := alpha*A*x + y.
*
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
*
*        Form  y := alpha*A'*x + y.
*
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
*
      RETURN
*
*     End of DGEMV .
*
      END
      SUBROUTINE DTRMV ( UPLO, TRANS, DIAG, N, A, LDA, X, INCX )
*     .. Scalar Arguments ..
      INTEGER            INCX, LDA, N
      CHARACTER*1        DIAG, TRANS, UPLO
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DTRMV  performs one of the matrix-vector operations
*
*     x := A*x,   or   x := A'*x,
*
*  where x is an n element vector and  A is an n by n unit, or non-unit,
*  upper or lower triangular matrix.
*
*  Parameters
*  ==========
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANS  - CHARACTER*1.
*           On entry, TRANS specifies the operation to be performed as
*           follows:
*
*              TRANS = 'N' or 'n'   x := A*x.
*
*              TRANS = 'T' or 't'   x := A'*x.
*
*              TRANS = 'C' or 'c'   x := A'*x.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit
*           triangular as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the order of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry with  UPLO = 'U' or 'u', the leading n by n
*           upper triangular part of the array A must contain the upper
*           triangular matrix and the strictly lower triangular part of
*           A is not referenced.
*           Before entry with UPLO = 'L' or 'l', the leading n by n
*           lower triangular part of the array A must contain the lower
*           triangular matrix and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u', the diagonal elements of
*           A are not referenced either, but are assumed to be unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, n ).
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the n
*           element vector x. On exit, X is overwritten with the
*           tranformed vector x.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JX, KX
      LOGICAL            NOUNIT
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF     ( .NOT.LSAME( UPLO , 'U' ).AND.
     $         .NOT.LSAME( UPLO , 'L' )      )THEN
         INFO = 1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ).AND.
     $         .NOT.LSAME( TRANS, 'T' ).AND.
     $         .NOT.LSAME( TRANS, 'C' )      )THEN
         INFO = 2
      ELSE IF( .NOT.LSAME( DIAG , 'U' ).AND.
     $         .NOT.LSAME( DIAG , 'N' )      )THEN
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
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Set up the start point in X if the increment is not unity. This
*     will be  ( N - 1 )*INCX  too small for descending loops.
*
      IF( INCX.LE.0 )THEN
         KX = 1 - ( N - 1 )*INCX
      ELSE IF( INCX.NE.1 )THEN
         KX = 1
      END IF
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
      IF( LSAME( TRANS, 'N' ) )THEN
*
*        Form  x := A*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 20, J = 1, N
                  IF( X( J ).NE.ZERO )THEN
                     TEMP = X( J )
                     DO 10, I = 1, J - 1
                        X( I ) = X( I ) + TEMP*A( I, J )
   10                CONTINUE
                     IF( NOUNIT )
     $                  X( J ) = X( J )*A( J, J )
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
     $                  X( JX ) = X( JX )*A( J, J )
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
     $                  X( J ) = X( J )*A( J, J )
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
     $                  X( JX ) = X( JX )*A( J, J )
                  END IF
                  JX = JX - INCX
   80          CONTINUE
            END IF
         END IF
      ELSE
*
*        Form  x := A'*x.
*
         IF( LSAME( UPLO, 'U' ) )THEN
            IF( INCX.EQ.1 )THEN
               DO 100, J = N, 1, -1
                  TEMP = X( J )
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
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
     $               TEMP = TEMP*A( J, J )
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
     $               TEMP = TEMP*A( J, J )
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
     $               TEMP = TEMP*A( J, J )
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
*
      RETURN
*
*     End of DTRMV .
*
      END
      SUBROUTINE DGER  ( M, N, ALPHA, X, INCX, Y, INCY, A, LDA )
*     .. Scalar Arguments ..
      DOUBLE PRECISION   ALPHA
      INTEGER            INCX, INCY, LDA, M, N
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGER   performs the rank 1 operation
*
*     A := alpha*x*y' + A,
*
*  where alpha is a scalar, x is an m element vector, y is an n element
*  vector and A is an m by n matrix.
*
*  Parameters
*  ==========
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of the matrix A.
*           M must be at least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of the matrix A.
*           N must be at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  X      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( m - 1 )*abs( INCX ) ).
*           Before entry, the incremented array X must contain the m
*           element vector x.
*           Unchanged on exit.
*
*  INCX   - INTEGER.
*           On entry, INCX specifies the increment for the elements of
*           X. INCX must not be zero.
*           Unchanged on exit.
*
*  Y      - DOUBLE PRECISION array of dimension at least
*           ( 1 + ( n - 1 )*abs( INCY ) ).
*           Before entry, the incremented array Y must contain the n
*           element vector y.
*           Unchanged on exit.
*
*  INCY   - INTEGER.
*           On entry, INCY specifies the increment for the elements of
*           Y. INCY must not be zero.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
*           Before entry, the leading m by n part of the array A must
*           contain the matrix of coefficients. On exit, A is
*           overwritten by the updated matrix.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. LDA must be at least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 2 Blas routine.
*
*  -- Written on 22-October-1986.
*     Jack Dongarra, Argonne National Lab.
*     Jeremy Du Croz, Nag Central Office.
*     Sven Hammarling, Nag Central Office.
*     Richard Hanson, Sandia National Labs.
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER        ( ZERO = 0.0D+0 )
*     .. Local Scalars ..
      DOUBLE PRECISION   TEMP
      INTEGER            I, INFO, IX, J, JY, KX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
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
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.( ALPHA.EQ.ZERO ) )
     $   RETURN
*
*     Start the operations. In this version the elements of A are
*     accessed sequentially with one pass through A.
*
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
*
      RETURN
*
*     End of DGER  .
*
      END
      SUBROUTINE DGEMM ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  DGEMM  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
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
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAME( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAME( TRANSB, 'T' ) )      )THEN
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
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
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
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
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
*
*           Form  C := alpha*A'*B + beta*C
*
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
*
*           Form  C := alpha*A*B' + beta*C
*
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
*
*           Form  C := alpha*A'*B' + beta*C
*
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
*
      RETURN
*
*     End of DGEMM .
*
      END
      SUBROUTINE DTRMM ( SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,
     $                   B, LDB )
*     .. Scalar Arguments ..
      CHARACTER*1        SIDE, UPLO, TRANSA, DIAG
      INTEGER            M, N, LDA, LDB
      DOUBLE PRECISION   ALPHA
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DTRMM  performs one of the matrix-matrix operations
*
*     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
*
*  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*
*     op( A ) = A   or   op( A ) = A'.
*
*  Parameters
*  ==========
*
*  SIDE   - CHARACTER*1.
*           On entry,  SIDE specifies whether  op( A ) multiplies B from
*           the left or right as follows:
*
*              SIDE = 'L' or 'l'   B := alpha*op( A )*B.
*
*              SIDE = 'R' or 'r'   B := alpha*B*op( A ).
*
*           Unchanged on exit.
*
*  UPLO   - CHARACTER*1.
*           On entry, UPLO specifies whether the matrix A is an upper or
*           lower triangular matrix as follows:
*
*              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*
*              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*
*           Unchanged on exit.
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n'   op( A ) = A.
*
*              TRANSA = 'T' or 't'   op( A ) = A'.
*
*              TRANSA = 'C' or 'c'   op( A ) = A'.
*
*           Unchanged on exit.
*
*  DIAG   - CHARACTER*1.
*           On entry, DIAG specifies whether or not A is unit triangular
*           as follows:
*
*              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*
*              DIAG = 'N' or 'n'   A is not assumed to be unit
*                                  triangular.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry, M specifies the number of rows of B. M must be at
*           least zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry, N specifies the number of columns of B.  N must be
*           at least zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*           zero then  A is not referenced and  B need not be set before
*           entry.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
*           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
*           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*           upper triangular part of the array  A must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           A is not referenced.
*           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*           lower triangular part of the array  A must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           A is not referenced.
*           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*           A  are not referenced either,  but are assumed to be  unity.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*           then LDA must be at least max( 1, n ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
*           Before entry,  the leading  m by n part of the array  B must
*           contain the matrix  B,  and  on exit  is overwritten  by the
*           transformed matrix.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in  the  calling  (sub)  program.   LDB  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            LSIDE, NOUNIT, UPPER
      INTEGER            I, INFO, J, K, NROWA
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LSIDE  = LSAME( SIDE  , 'L' )
      IF( LSIDE )THEN
         NROWA = M
      ELSE
         NROWA = N
      END IF
      NOUNIT = LSAME( DIAG  , 'N' )
      UPPER  = LSAME( UPLO  , 'U' )
*
      INFO   = 0
      IF(      ( .NOT.LSIDE                ).AND.
     $         ( .NOT.LSAME( SIDE  , 'R' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.UPPER                ).AND.
     $         ( .NOT.LSAME( UPLO  , 'L' ) )      )THEN
         INFO = 2
      ELSE IF( ( .NOT.LSAME( TRANSA, 'N' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'T' ) ).AND.
     $         ( .NOT.LSAME( TRANSA, 'C' ) )      )THEN
         INFO = 3
      ELSE IF( ( .NOT.LSAME( DIAG  , 'U' ) ).AND.
     $         ( .NOT.LSAME( DIAG  , 'N' ) )      )THEN
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
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     And when  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         DO 20, J = 1, N
            DO 10, I = 1, M
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
         RETURN
      END IF
*
*     Start the operations.
*
      IF( LSIDE )THEN
         IF( LSAME( TRANSA, 'N' ) )THEN
*
*           Form  B := alpha*A*B.
*
            IF( UPPER )THEN
               DO 50, J = 1, N
                  DO 40, K = 1, M
                     IF( B( K, J ).NE.ZERO )THEN
                        TEMP = ALPHA*B( K, J )
                        DO 30, I = 1, K - 1
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   30                   CONTINUE
                        IF( NOUNIT )
     $                     TEMP = TEMP*A( K, K )
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
     $                     B( K, J ) = B( K, J )*A( K, K )
                        DO 60, I = K + 1, M
                           B( I, J ) = B( I, J ) + TEMP*A( I, K )
   60                   CONTINUE
                     END IF
   70             CONTINUE
   80          CONTINUE
            END IF
         ELSE
*
*           Form  B := alpha*B*A'.
*
            IF( UPPER )THEN
               DO 110, J = 1, N
                  DO 100, I = M, 1, -1
                     TEMP = B( I, J )
                     IF( NOUNIT )
     $                  TEMP = TEMP*A( I, I )
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
     $                  TEMP = TEMP*A( I, I )
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
*
*           Form  B := alpha*B*A.
*
            IF( UPPER )THEN
               DO 180, J = N, 1, -1
                  TEMP = ALPHA
                  IF( NOUNIT )
     $               TEMP = TEMP*A( J, J )
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
     $               TEMP = TEMP*A( J, J )
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
*
*           Form  B := alpha*B*A'.
*
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
     $               TEMP = TEMP*A( K, K )
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
     $               TEMP = TEMP*A( K, K )
                  IF( TEMP.NE.ONE )THEN
                     DO 290, I = 1, M
                        B( I, K ) = TEMP*B( I, K )
  290                CONTINUE
                  END IF
  300          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DTRMM .
*
      END

* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module DGESVD from package LAPACK.
* Retrieved from NETLIB on Wed Sep 17 19:10:51 1997.
* ======================================================================
      SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns V**T, not V.
*
*  Arguments
*  =========
*
*  JOBU    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U are returned in array U:
*          = 'S':  the first min(m,n) columns of U (the left singular
*                  vectors) are returned in the array U;
*          = 'O':  the first min(m,n) columns of U (the left singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no columns of U (no left singular vectors) are
*                  computed.
*
*  JOBVT   (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix
*          V**T:
*          = 'A':  all N rows of V**T are returned in the array VT;
*          = 'S':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are returned in the array VT;
*          = 'O':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no rows of V**T (no right singular vectors) are
*                  computed.
*
*          JOBVT and JOBU cannot both be 'O'.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBU = 'O',  A is overwritten with the first min(m,n)
*                          columns of U (the left singular vectors,
*                          stored columnwise);
*          if JOBVT = 'O', A is overwritten with the first min(m,n)
*                          rows of V**T (the right singular vectors,
*                          stored rowwise);
*          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
*                          are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
*          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
*          if JOBU = 'S', U contains the first min(m,n) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBU = 'N' or 'O', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBU = 'S' or 'A', LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
*          V**T;
*          if JOBVT = 'S', VT contains the first min(m,n) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBVT = 'N' or 'O', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
*          superdiagonal elements of an upper bidiagonal matrix B
*          whose diagonal is in S (not necessarily sorted). B
*          satisfies A = U * B * VT, so it has the same singular values
*          as A, and singular vectors related by U and VT.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1.
*          LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)-4).
*          For good performance, LWORK should generally be larger.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if DBDSQR did not converge, INFO specifies how many
*                superdiagonals of an intermediate bidiagonal form B
*                did not converge to zero. See the description of WORK
*                above for details.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS, WNTVA,
     $                   WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IR, ISCL,
     $                   ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
     $                   MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU,
     $                   NRVT, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = ILAENV( 6, 'DGESVD', JOBU // JOBVT, M, N, 0, 0 )
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
*
      IF( .NOT.( WNTUA .OR. WNTUS .OR. WNTUO .OR. WNTUN ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( WNTVA .OR. WNTVS .OR. WNTVO .OR. WNTVN ) .OR.
     $         ( WNTVO .AND. WNTUO ) ) THEN
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
     $         ( WNTVS .AND. LDVT.LT.MINMN ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 .AND. LWORK.GE.1 .AND. M.GT.0 .AND. N.GT.0 ) THEN
         IF( M.GE.N ) THEN
*
*           Compute space needed for DBDSQR
*
            BDSPAC = MAX( 3*N, 5*N-4 )
            IF( M.GE.MNTHR ) THEN
               IF( WNTUN ) THEN
*
*                 Path 1 (M much larger than N, JOBU='N')
*
                  MAXWRK = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*N+2*N*
     $                     ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  IF( WNTVO .OR. WNTVAS )
     $               MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                        ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUS .AND. WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTUA .AND. WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
*
*              Path 10 (M at least N, but not much larger)
*
               MAXWRK = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTUS .OR. WNTUO )
     $            MAXWRK = MAX( MAXWRK, 3*N+N*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, N, N, -1 ) )
               IF( WNTUA )
     $            MAXWRK = MAX( MAXWRK, 3*N+M*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, M, N, -1 ) )
               IF( .NOT.WNTVN )
     $            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                     ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*N+M, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         ELSE
*
*           Compute space needed for DBDSQR
*
            BDSPAC = MAX( 3*M, 5*M-4 )
            IF( N.GE.MNTHR ) THEN
               IF( WNTVN ) THEN
*
*                 Path 1t(N much larger than M, JOBVT='N')
*
                  MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*M+2*M*
     $                     ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  IF( WNTUO .OR. WNTUAS )
     $               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                        ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*M, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*                 Path 3t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVS .AND. WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               ELSE IF( WNTVA .AND. WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
                  MAXWRK = MAX( MAXWRK, MINWRK )
               END IF
            ELSE
*
*              Path 10t(N greater than M, but not much larger)
*
               MAXWRK = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTVS .OR. WNTVO )
     $            MAXWRK = MAX( MAXWRK, 3*M+M*
     $                     ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 ) )
               IF( WNTVA )
     $            MAXWRK = MAX( MAXWRK, 3*M+N*
     $                     ILAENV( 1, 'DORGBR', 'P', N, N, M, -1 ) )
               IF( .NOT.WNTUN )
     $            MAXWRK = MAX( MAXWRK, 3*M+( M-1 )*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*M+N, BDSPAC )
               MAXWRK = MAX( MAXWRK, MINWRK )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
      END IF
*
      IF( LWORK.LT.MINWRK ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESVD', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         IF( LWORK.GE.1 )
     $      WORK( 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF
*
      IF( M.GE.N ) THEN
*
*        A has at least as many rows as columns. If A has sufficiently
*        more rows than columns, first reduce using the QR
*        decomposition (if sufficient workspace available)
*
         IF( M.GE.MNTHR ) THEN
*
            IF( WNTUN ) THEN
*
*              Path 1 (M much larger than N, JOBU='N')
*              No left singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need 2*N, prefer N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out below R
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               NCVT = 0
               IF( WNTVO .OR. WNTVAS ) THEN
*
*                 If right singular vectors desired, generate P'.
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               END IF
               IWORK = IE + N
*
*              Perform bidiagonal QR iteration, computing right
*              singular vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, 0, 0, S, WORK( IE ), A, LDA,
     $                      DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
*              If right singular vectors desired in VT, copy them there
*
               IF( WNTVAS )
     $            CALL DLACPY( 'F', N, N, A, LDA, VT, LDVT )
*
            ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*              N left singular vectors to be overwritten on A and
*              no right singular vectors to be computed
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to WORK(IR) and zero out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                         LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing R
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR)
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, 1,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 10 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   10             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize A
*                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing A
*                 (Workspace: need 4*N, prefer 3*N+N*NB)
*
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, 1,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT, copying result to WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
*
*                 Generate left vectors bidiagonalizing R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR) and computing right
*                 singular vectors of R in VT
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, LDVT,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 20 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT
*                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply Q in A by left vectors bidiagonalizing R
*                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                  CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A and computing right
*                 singular vectors of A in VT
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUS ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*                 N left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left vectors bidiagonalizing R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IR), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IR ), LDWRKR, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
*                    Copy right singular vectors of R to A
*                    (Workspace: need N*N)
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing R in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
*                         or 'A')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTUA ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*                 M left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IR), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IR ), LDWRKR, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
*                    Copy right singular vectors of R from WORK(IR) to A
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
*                         or 'A')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R from A to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, VT( 2, 1 ),
     $                            LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           M .LT. MNTHR
*
*           Path 10 (M at least N, but not much larger)
*           Reduce to bidiagonal form without QR decomposition
*
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
*
*           Bidiagonalize A
*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
*
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
               IF( WNTUS )
     $            NCU = N
               IF( WNTUA )
     $            NCU = M
               CALL DORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N, prefer 3*N+N*NB)
*
               CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
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
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
*
         END IF
*
      ELSE
*
*        A has more columns than rows. If A has sufficiently more
*        columns than rows, first reduce using the LQ decomposition (if
*        sufficient workspace available)
*
         IF( N.GE.MNTHR ) THEN
*
            IF( WNTVN ) THEN
*
*              Path 1t(N much larger than M, JOBVT='N')
*              No right singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need 2*M, prefer M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out above L
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               IF( WNTUO .OR. WNTUAS ) THEN
*
*                 If left singular vectors desired, generate Q
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
               END IF
               IWORK = IE + M
               NRU = 0
               IF( WNTUO .OR. WNTUAS )
     $            NRU = M
*
*              Perform bidiagonal QR iteration, computing left singular
*              vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A,
     $                      LDA, DUM, 1, WORK( IWORK ), INFO )
*
*              If left singular vectors desired in U, copy them there
*
               IF( WNTUAS )
     $            CALL DLACPY( 'F', M, M, A, LDA, U, LDU )
*
            ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              no left singular vectors to be computed
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to WORK(IR) and zero out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                         WORK( IR+LDWRKR ), LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing L
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
*
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   30             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize A
*                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing A
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA,
     $                         DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing about above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U, copying result to WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
*
*                 Generate right vectors bidiagonalizing L in WORK(IR)
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of L in U, and computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, U, LDU, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   40             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U
*                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply right vectors bidiagonalizing L by Q in A
*                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                  CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in U and computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA,
     $                         U, LDU, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVS ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing L in
*                    WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy result to VT
*
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
*                    Copy left singular vectors of L to A
*                    (Workspace: need M*M)
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors of L in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, compute left
*                    singular vectors of A in A and compute right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTVA ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
*                    Copy left singular vectors of A from WORK(IR) to A
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in A and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is M by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           N .LT. MNTHR
*
*           Path 10t(N greater than M, but not much larger)
*           Reduce to bidiagonal form without LQ decomposition
*
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
*
*           Bidiagonalize A
*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL DORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
*
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
               IF( WNTVA )
     $            NRVT = N
               IF( WNTVS )
     $            NRVT = M
               CALL DORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M, prefer 3*M+M*NB)
*
               CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
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
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
*
         END IF
*
      END IF
*
*     If DBDSQR failed to converge, copy unconverged superdiagonals
*     to WORK( 2:MINMN )
*
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
*
*     Undo scaling if necessary
*
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGESVD
*
      END
      SUBROUTINE DBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
     $                   LDU, C, LDC, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DBDSQR computes the singular value decomposition (SVD) of a real
*  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
*  denotes the transpose of P), where S is a diagonal matrix with
*  non-negative diagonal elements (the singular values of B), and Q
*  and P are orthogonal matrices.
*
*  The routine computes S, and optionally computes U * Q, P' * VT,
*  or Q' * C, for given real input matrices U, VT, and C.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
*  no. 5, pp. 873-912, Sept 1990) and
*  "Accurate singular values and differential qd algorithms," by
*  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
*  Department, University of California at Berkeley, July 1992
*  for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  B is upper bidiagonal;
*          = 'L':  B is lower bidiagonal.
*
*  N       (input) INTEGER
*          The order of the matrix B.  N >= 0.
*
*  NCVT    (input) INTEGER
*          The number of columns of the matrix VT. NCVT >= 0.
*
*  NRU     (input) INTEGER
*          The number of rows of the matrix U. NRU >= 0.
*
*  NCC     (input) INTEGER
*          The number of columns of the matrix C. NCC >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the bidiagonal matrix B.
*          On exit, if INFO=0, the singular values of B in decreasing
*          order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the elements of E contain the
*          offdiagonal elements of the bidiagonal matrix whose SVD
*          is desired. On normal exit (INFO = 0), E is destroyed.
*          If the algorithm does not converge (INFO > 0), D and E
*          will contain the diagonal and superdiagonal elements of a
*          bidiagonal matrix orthogonally equivalent to the one given
*          as input. E(N) is used for workspace.
*
*  VT      (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
*          On entry, an N-by-NCVT matrix VT.
*          On exit, VT is overwritten by P' * VT.
*          VT is not referenced if NCVT = 0.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.
*          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
*
*  U       (input/output) DOUBLE PRECISION array, dimension (LDU, N)
*          On entry, an NRU-by-N matrix U.
*          On exit, U is overwritten by U * Q.
*          U is not referenced if NRU = 0.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,NRU).
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
*          On entry, an N-by-NCC matrix C.
*          On exit, C is overwritten by Q' * C.
*          C is not referenced if NCC = 0.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C.
*          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*            2*N  if only singular values wanted (NCVT = NRU = NCC = 0)
*            max( 1, 4*N-4 ) otherwise
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  If INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm did not converge; D and E contain the
*                elements of a bidiagonal matrix which is orthogonally
*                similar to the input matrix B;  if INFO = i, i
*                elements of E have not converged to zero.
*
*  Internal Parameters
*  ===================
*
*  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
*          TOLMUL controls the convergence criterion of the QR loop.
*          If it is positive, TOLMUL*EPS is the desired relative
*             precision in the computed singular values.
*          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
*             desired absolute accuracy in the computed singular
*             values (corresponds to relative accuracy
*             abs(TOLMUL*EPS) in the largest singular value.
*          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
*             between 10 (for fast convergence) and .1/EPS
*             (for there to be some accuracy in the results).
*          Default is to lose at either one eighth or 2 of the
*             available decimal digits in each computed singular value
*             (whichever is smaller).
*
*  MAXITR  INTEGER, default = 6
*          MAXITR controls the maximum number of passes of the
*          algorithm through its inner loop. The algorithms stops
*          (and so fails to converge) if the number of passes
*          through the inner loop exceeds MAXITR*N**2.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   NEGONE
      PARAMETER          ( NEGONE = -1.0D0 )
      DOUBLE PRECISION   HNDRTH
      PARAMETER          ( HNDRTH = 0.01D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HNDRD
      PARAMETER          ( HNDRD = 100.0D0 )
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ROTATE
      INTEGER            I, IDIR, IROT, ISUB, ITER, IUPLO, J, LL, LLL,
     $                   M, MAXIT, NM1, NM12, NM13, OLDLL, OLDM
      DOUBLE PRECISION   ABSE, ABSS, COSL, COSR, CS, EPS, F, G, H, MU,
     $                   OLDCS, OLDSN, R, SHIFT, SIGMN, SIGMX, SINL,
     $                   SINR, SLL, SMAX, SMIN, SMINL, SMINLO, SMINOA,
     $                   SN, THRESH, TOL, TOLMUL, UNFL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLAS2, DLASQ1, DLASR, DLASV2, DROT,
     $                   DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
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
         CALL XERBLA( 'DBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 )
     $   GO TO 150
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
*
*     If no singular vectors desired, use qd algorithm
*
      IF( .NOT.ROTATE ) THEN
         CALL DLASQ1( N, D, E, WORK, INFO )
         RETURN
      END IF
*
      NM1 = N - 1
      NM12 = NM1 + NM1
      NM13 = NM12 + NM1
*
*     Get machine constants
*
      EPS = DLAMCH( 'Epsilon' )
      UNFL = DLAMCH( 'Safe minimum' )
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left
*
      IF( IUPLO.EQ.2 ) THEN
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            WORK( I ) = CS
            WORK( NM1+I ) = SN
   10    CONTINUE
*
*        Update singular vectors if desired
*
         IF( NRU.GT.0 )
     $      CALL DLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ), WORK( N ), U,
     $                  LDU )
         IF( NCC.GT.0 )
     $      CALL DLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ), WORK( N ), C,
     $                  LDC )
      END IF
*
*     Compute singular values to relative accuracy TOL
*     (By setting TOL to be negative, algorithm will compute
*     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
*
      TOLMUL = MAX( TEN, MIN( HNDRD, EPS**MEIGTH ) )
      TOL = TOLMUL*EPS
*
*     Compute approximate maximum, minimum singular values
*
      SMAX = ABS( D( N ) )
      DO 20 I = 1, N - 1
         SMAX = MAX( SMAX, ABS( D( I ) ), ABS( E( I ) ) )
   20 CONTINUE
      SMINL = ZERO
      IF( TOL.GE.ZERO ) THEN
*
*        Relative accuracy desired
*
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
         SMINOA = SMINOA / SQRT( DBLE( N ) )
         THRESH = MAX( TOL*SMINOA, MAXITR*N*N*UNFL )
      ELSE
*
*        Absolute accuracy desired
*
         THRESH = MAX( ABS( TOL )*SMAX, MAXITR*N*N*UNFL )
      END IF
*
*     Prepare for main iteration loop for the singular values
*     (MAXIT is the maximum number of passes through the inner
*     loop permitted before nonconvergence signalled.)
*
      MAXIT = MAXITR*N*N
      ITER = 0
      OLDLL = -1
      OLDM = -1
*
*     M points to last element of unconverged part of matrix
*
      M = N
*
*     Begin main iteration loop
*
   50 CONTINUE
*
*     Check for convergence or exceeding iteration count
*
      IF( M.LE.1 )
     $   GO TO 150
      IF( ITER.GT.MAXIT )
     $   GO TO 190
*
*     Find diagonal block of matrix to work on
*
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
*
*     Matrix splits since E(LL) = 0
*
      IF( LL.EQ.M-1 ) THEN
*
*        Convergence of bottom singular value, return to top of loop
*
         M = M - 1
         GO TO 50
      END IF
   80 CONTINUE
      LL = LL + 1
*
*     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
*
      IF( LL.EQ.M-1 ) THEN
*
*        2 by 2 block, handle separately
*
         CALL DLASV2( D( M-1 ), E( M-1 ), D( M ), SIGMN, SIGMX, SINR,
     $                COSR, SINL, COSL )
         D( M-1 ) = SIGMX
         E( M-1 ) = ZERO
         D( M ) = SIGMN
*
*        Compute singular vectors, if desired
*
         IF( NCVT.GT.0 )
     $      CALL DROT( NCVT, VT( M-1, 1 ), LDVT, VT( M, 1 ), LDVT, COSR,
     $                 SINR )
         IF( NRU.GT.0 )
     $      CALL DROT( NRU, U( 1, M-1 ), 1, U( 1, M ), 1, COSL, SINL )
         IF( NCC.GT.0 )
     $      CALL DROT( NCC, C( M-1, 1 ), LDC, C( M, 1 ), LDC, COSL,
     $                 SINL )
         M = M - 2
         GO TO 50
      END IF
*
*     If working on new submatrix, choose shift direction
*     (from larger end diagonal element towards smaller)
*
      IF( LL.GT.OLDM .OR. M.LT.OLDLL ) THEN
         IF( ABS( D( LL ) ).GE.ABS( D( M ) ) ) THEN
*
*           Chase bulge from top (big end) to bottom (small end)
*
            IDIR = 1
         ELSE
*
*           Chase bulge from bottom (big end) to top (small end)
*
            IDIR = 2
         END IF
      END IF
*
*     Apply convergence tests
*
      IF( IDIR.EQ.1 ) THEN
*
*        Run convergence test in forward direction
*        First apply standard test to bottom of matrix
*
         IF( ABS( E( M-1 ) ).LE.ABS( TOL )*ABS( D( M ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( M-1 ) ).LE.THRESH ) ) THEN
            E( M-1 ) = ZERO
            GO TO 50
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion forward
*
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
*
      ELSE
*
*        Run convergence test in backward direction
*        First apply standard test to top of matrix
*
         IF( ABS( E( LL ) ).LE.ABS( TOL )*ABS( D( LL ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( LL ) ).LE.THRESH ) ) THEN
            E( LL ) = ZERO
            GO TO 50
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion backward
*
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
*
*     Compute shift.  First, test if shifting would ruin relative
*     accuracy, and if so set the shift to zero.
*
      IF( TOL.GE.ZERO .AND. N*TOL*( SMINL / SMAX ).LE.
     $    MAX( EPS, HNDRTH*TOL ) ) THEN
*
*        Use a zero shift to avoid loss of relative accuracy
*
         SHIFT = ZERO
      ELSE
*
*        Compute the shift from 2-by-2 block at end of matrix
*
         IF( IDIR.EQ.1 ) THEN
            SLL = ABS( D( LL ) )
            CALL DLAS2( D( M-1 ), E( M-1 ), D( M ), SHIFT, R )
         ELSE
            SLL = ABS( D( M ) )
            CALL DLAS2( D( LL ), E( LL ), D( LL+1 ), SHIFT, R )
         END IF
*
*        Test if shift negligible, and if so set to zero
*
         IF( SLL.GT.ZERO ) THEN
            IF( ( SHIFT / SLL )**2.LT.EPS )
     $         SHIFT = ZERO
         END IF
      END IF
*
*     Increment iteration count
*
      ITER = ITER + M - LL
*
*     If SHIFT = 0, do simplified QR iteration
*
      IF( SHIFT.EQ.ZERO ) THEN
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            CALL DLARTG( D( LL )*CS, E( LL ), CS, SN, R )
            CALL DLARTG( OLDCS*R, D( LL+1 )*SN, OLDCS, OLDSN, D( LL ) )
            WORK( 1 ) = CS
            WORK( 1+NM1 ) = SN
            WORK( 1+NM12 ) = OLDCS
            WORK( 1+NM13 ) = OLDSN
            IROT = 1
            DO 110 I = LL + 1, M - 1
               CALL DLARTG( D( I )*CS, E( I ), CS, SN, R )
               E( I-1 ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I+1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT + 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = OLDSN
  110       CONTINUE
            H = D( M )*CS
            D( M ) = H*OLDCS
            E( M-1 ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            CALL DLARTG( D( M )*CS, E( M-1 ), CS, SN, R )
            CALL DLARTG( OLDCS*R, D( M-1 )*SN, OLDCS, OLDSN, D( M ) )
            WORK( M-LL ) = CS
            WORK( M-LL+NM1 ) = -SN
            WORK( M-LL+NM12 ) = OLDCS
            WORK( M-LL+NM13 ) = -OLDSN
            IROT = M - LL
            DO 120 I = M - 1, LL + 1, -1
               CALL DLARTG( D( I )*CS, E( I-1 ), CS, SN, R )
               E( I ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I-1 )*SN, OLDCS, OLDSN, D( I ) )
               IROT = IROT - 1
               WORK( IROT ) = CS
               WORK( IROT+NM1 ) = -SN
               WORK( IROT+NM12 ) = OLDCS
               WORK( IROT+NM13 ) = -OLDSN
  120       CONTINUE
            H = D( LL )*CS
            D( LL ) = H*OLDCS
            E( LL ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
         END IF
      ELSE
*
*        Use nonzero shift
*
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( LL ) )-SHIFT )*
     $          ( SIGN( ONE, D( LL ) )+SHIFT / D( LL ) )
            G = E( LL )
            CALL DLARTG( F, G, COSR, SINR, R )
            F = COSR*D( LL ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL )
            G = SINR*D( LL+1 )
            D( LL+1 ) = COSR*D( LL+1 )
            CALL DLARTG( F, G, COSL, SINL, R )
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
               CALL DLARTG( F, G, COSR, SINR, R )
               E( I-1 ) = R
               F = COSR*D( I ) + SINR*E( I )
               E( I ) = COSR*E( I ) - SINR*D( I )
               G = SINR*D( I+1 )
               D( I+1 ) = COSR*D( I+1 )
               CALL DLARTG( F, G, COSL, SINL, R )
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
            CALL DLARTG( F, G, COSR, SINR, R )
            E( M-2 ) = R
            F = COSR*D( M-1 ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M-1 )
            G = SINR*D( M )
            D( M ) = COSR*D( M )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( M-1 ) = R
            F = COSL*E( M-1 ) + SINL*D( M )
            D( M ) = COSL*D( M ) - SINL*E( M-1 )
            IROT = IROT + 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = SINL
            E( M-1 ) = F
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCVT, WORK( 1 ),
     $                     WORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'F', NRU, M-LL+1, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'F', M-LL+1, NCC, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( M ) )-SHIFT )*( SIGN( ONE, D( M ) )+SHIFT /
     $          D( M ) )
            G = E( M-1 )
            CALL DLARTG( F, G, COSR, SINR, R )
            F = COSR*D( M ) + SINR*E( M-1 )
            E( M-1 ) = COSR*E( M-1 ) - SINR*D( M )
            G = SINR*D( M-1 )
            D( M-1 ) = COSR*D( M-1 )
            CALL DLARTG( F, G, COSL, SINL, R )
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
               CALL DLARTG( F, G, COSR, SINR, R )
               E( I ) = R
               F = COSR*D( I ) + SINR*E( I-1 )
               E( I-1 ) = COSR*E( I-1 ) - SINR*D( I )
               G = SINR*D( I-1 )
               D( I-1 ) = COSR*D( I-1 )
               CALL DLARTG( F, G, COSL, SINL, R )
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
            CALL DLARTG( F, G, COSR, SINR, R )
            E( LL+1 ) = R
            F = COSR*D( LL+1 ) + SINR*E( LL )
            E( LL ) = COSR*E( LL ) - SINR*D( LL+1 )
            G = SINR*D( LL )
            D( LL ) = COSR*D( LL )
            CALL DLARTG( F, G, COSL, SINL, R )
            D( LL+1 ) = R
            F = COSL*E( LL ) + SINL*D( LL )
            D( LL ) = COSL*D( LL ) - SINL*E( LL )
            IROT = IROT - 1
            WORK( IROT ) = COSR
            WORK( IROT+NM1 ) = -SINR
            WORK( IROT+NM12 ) = COSL
            WORK( IROT+NM13 ) = -SINL
            E( LL ) = F
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
*
*           Update singular vectors if desired
*
            IF( NCVT.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCVT, WORK( NM12+1 ),
     $                     WORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DLASR( 'R', 'V', 'B', NRU, M-LL+1, WORK( 1 ),
     $                     WORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL DLASR( 'L', 'V', 'B', M-LL+1, NCC, WORK( 1 ),
     $                     WORK( N ), C( LL, 1 ), LDC )
         END IF
      END IF
*
*     QR iteration finished, go back and check convergence
*
      GO TO 50
*
*     All singular values converged, so make them positive
*
  150 CONTINUE
      DO 160 I = 1, N
         IF( D( I ).LT.ZERO ) THEN
            D( I ) = -D( I )
*
*           Change sign of singular vectors, if desired
*
            IF( NCVT.GT.0 )
     $         CALL DSCAL( NCVT, NEGONE, VT( I, 1 ), LDVT )
         END IF
  160 CONTINUE
*
*     Sort the singular values into decreasing order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 180 I = 1, N - 1
*
*        Scan for smallest D(I)
*
         ISUB = 1
         SMIN = D( 1 )
         DO 170 J = 2, N + 1 - I
            IF( D( J ).LE.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
  170    CONTINUE
         IF( ISUB.NE.N+1-I ) THEN
*
*           Swap singular values and vectors
*
            D( ISUB ) = D( N+1-I )
            D( N+1-I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL DSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( N+1-I, 1 ),
     $                     LDVT )
            IF( NRU.GT.0 )
     $         CALL DSWAP( NRU, U( 1, ISUB ), 1, U( 1, N+1-I ), 1 )
            IF( NCC.GT.0 )
     $         CALL DSWAP( NCC, C( ISUB, 1 ), LDC, C( N+1-I, 1 ), LDC )
         END IF
  180 CONTINUE
      GO TO 210
*
*     Maximum number of iterations exceeded, failure to converge
*
  190 CONTINUE
      INFO = 0
      DO 200 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  200 CONTINUE
  210 CONTINUE
      RETURN
*
*     End of DBDSQR
*
      END
      SUBROUTINE DGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEBD2 reduces a real general m by n matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'DGEBD2', -INFO )
         RETURN
      END IF
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, N
*
*           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
            CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAUQ( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = D( I )
*
            IF( I.LT.N ) THEN
*
*              Generate elementary reflector G(i) to annihilate
*              A(i,i+2:n)
*
               CALL DLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
*
*              Apply G(i) to A(i+1:m,i+1:n) from the right
*
               CALL DLARF( 'Right', M-I, N-I, A( I, I+1 ), LDA,
     $                     TAUP( I ), A( I+1, I+1 ), LDA, WORK )
               A( I, I+1 ) = E( I )
            ELSE
               TAUP( I ) = ZERO
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, M
*
*           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
*
            CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            A( I, I ) = ONE
*
*           Apply G(i) to A(i+1:m,i:n) from the right
*
            CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAUP( I ),
     $                  A( MIN( I+1, M ), I ), LDA, WORK )
            A( I, I ) = D( I )
*
            IF( I.LT.M ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(i+2:m,i)
*
               CALL DLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Apply H(i) to A(i+1:m,i+1:n) from the left
*
               CALL DLARF( 'Left', M-I, N-I, A( I+1, I ), 1, TAUQ( I ),
     $                     A( I+1, I+1 ), LDA, WORK )
               A( I+1, I ) = E( I )
            ELSE
               TAUQ( I ) = ZERO
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of DGEBD2
*
      END
      SUBROUTINE DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGEBRD reduces a general real M-by-N matrix A to upper or lower
*  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the orthogonal matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the orthogonal matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,M,N).
*          For optimum performance LWORK >= (M+N)*NB, where NB
*          is the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
*  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IINFO, J, LDWRKX, LDWRKY, MINMN, NB, NBMIN,
     $                   NX
      DOUBLE PRECISION   WS
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBD2, DGEMM, DLABRD, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
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
         CALL XERBLA( 'DGEBRD', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      MINMN = MIN( M, N )
      IF( MINMN.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      WS = MAX( M, N )
      LDWRKX = M
      LDWRKY = N
*
*     Set the block size NB and the crossover point NX.
*
      NB = MAX( 1, ILAENV( 1, 'DGEBRD', ' ', M, N, -1, -1 ) )
*
      IF( NB.GT.1 .AND. NB.LT.MINMN ) THEN
*
*        Determine when to switch from blocked to unblocked code.
*
         NX = MAX( NB, ILAENV( 3, 'DGEBRD', ' ', M, N, -1, -1 ) )
         IF( NX.LT.MINMN ) THEN
            WS = ( M+N )*NB
            IF( LWORK.LT.WS ) THEN
*
*              Not enough work space for the optimal NB, consider using
*              a smaller block size.
*
               NBMIN = ILAENV( 2, 'DGEBRD', ' ', M, N, -1, -1 )
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
*
      DO 30 I = 1, MINMN - NX, NB
*
*        Reduce rows and columns i:i+nb-1 to bidiagonal form and return
*        the matrices X and Y which are needed to update the unreduced
*        part of the matrix
*
         CALL DLABRD( M-I+1, N-I+1, NB, A( I, I ), LDA, D( I ), E( I ),
     $                TAUQ( I ), TAUP( I ), WORK, LDWRKX,
     $                WORK( LDWRKX*NB+1 ), LDWRKY )
*
*        Update the trailing submatrix A(i+nb:m,i+nb:n), using an update
*        of the form  A := A - V*Y' - X*U'
*
         CALL DGEMM( 'No transpose', 'Transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, A( I+NB, I ), LDA,
     $               WORK( LDWRKX*NB+NB+1 ), LDWRKY, ONE,
     $               A( I+NB, I+NB ), LDA )
         CALL DGEMM( 'No transpose', 'No transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, WORK( NB+1 ), LDWRKX, A( I, I+NB ), LDA,
     $               ONE, A( I+NB, I+NB ), LDA )
*
*        Copy diagonal and off-diagonal elements of B back into A
*
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
*
*     Use unblocked code to reduce the remainder of the matrix
*
      CALL DGEBD2( M-I+1, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $             TAUQ( I ), TAUP( I ), WORK, IINFO )
      WORK( 1 ) = WS
      RETURN
*
*     End of DGEBRD
*
      END
      SUBROUTINE DGELQ2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELQ2 computes an LQ factorization of a real m by n matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m by min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k) . . . H(2) H(1), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      DOUBLE PRECISION   AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELQ2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i,i+1:n)
*
         CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                TAU( I ) )
         IF( I.LT.M ) THEN
*
*           Apply H(i) to A(i+1:m,i:n) from the right
*
            AII = A( I, I )
            A( I, I ) = ONE
            CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAU( I ),
     $                  A( I+1, I ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
*
*     End of DGELQ2
*
      END
      SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGELQF computes an LQ factorization of a real M-by-N matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is the
*          optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k) . . . H(2) H(1), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGELQ2, DLARFB, DLARFT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DGELQF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DGELQF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DGELQF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the LQ factorization of the current block
*           A(i:i+ib-1,i:n)
*
            CALL DGELQ2( IB, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i+ib:m,i:n) from the right
*
               CALL DLARFB( 'Right', 'No transpose', 'Forward',
     $                      'Rowwise', M-I-IB+1, N-I+1, IB, A( I, I ),
     $                      LDA, WORK, LDWORK, A( I+IB, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL DGELQ2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGELQF
*
      END
      SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEQR2 computes a QR factorization of a real m by n matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(m,n) by n upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      DOUBLE PRECISION   AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQR2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
         CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            AII = A( I, I )
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
            A( I, I ) = AII
         END IF
   10 CONTINUE
      RETURN
*
*     End of DGEQR2
*
      END
      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DGEQRF computes a QR factorization of a real M-by-N matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the orthogonal matrix Q as a
*          product of min(m,n) elementary reflectors (see Further
*          Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is
*          the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real scalar, and v is a real vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQR2, DLARFB, DLARFT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DGEQRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the QR factorization of the current block
*           A(i:m,i:i+ib-1)
*
            CALL DGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL DGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGEQRF
*
      END
      SUBROUTINE DLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
     $                   LDY )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, M, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), X( LDX, * ), Y( LDY, * )
*     ..
*
*  Purpose
*  =======
*
*  DLABRD reduces the first NB rows and columns of a real general
*  m by n matrix A to upper or lower bidiagonal form by an orthogonal
*  transformation Q' * A * P, and returns the matrices X and Y which
*  are needed to apply the transformation to the unreduced part of A.
*
*  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
*  bidiagonal form.
*
*  This is an auxiliary routine called by DGEBRD
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.
*
*  NB      (input) INTEGER
*          The number of leading rows and columns of A to be reduced.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit, the first NB rows and columns of the matrix are
*          overwritten; the rest of the array is unchanged.
*          If m >= n, elements on and below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors; and
*            elements above the diagonal in the first NB rows, with the
*            array TAUP, represent the orthogonal matrix P as a product
*            of elementary reflectors.
*          If m < n, elements below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors, and
*            elements on and above the diagonal in the first NB rows,
*            with the array TAUP, represent the orthogonal matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (NB)
*          The diagonal elements of the first NB rows and columns of
*          the reduced matrix.  D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (NB)
*          The off-diagonal elements of the first NB rows and columns of
*          the reduced matrix.
*
*  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
*          The m-by-nb matrix X required to update the unreduced part
*          of A.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X. LDX >= M.
*
*  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
*          The n-by-nb matrix Y required to update the unreduced part
*          of A.
*
*  LDY     (output) INTEGER
*          The leading dimension of the array Y. LDY >= N.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are real scalars, and v and u are real vectors.
*
*  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
*  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
*  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The elements of the vectors v and u together form the m-by-nb matrix
*  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
*  the transformation to the unreduced part of the matrix, using a block
*  update of the form:  A := A - V*Y' - X*U'.
*
*  The contents of A on exit are illustrated by the following examples
*  with nb = 2:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
*    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
*    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )
*
*  where a denotes an element of the original matrix which is unchanged,
*  vi denotes an element of the vector defining H(i), and ui an element
*  of the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DLARFG, DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, NB
*
*           Update A(i:m,i)
*
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, Y( I, 1 ), LDY, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, X( I, 1 ),
     $                  LDX, A( 1, I ), 1, ONE, A( I, I ), 1 )
*
*           Generate reflection Q(i) to annihilate A(i+1:m,i)
*
            CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I+1, N-I, ONE, A( I, I+1 ),
     $                     LDA, A( I, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, A( I, 1 ), LDA,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, X( I, 1 ), LDX,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
*
*              Update A(i,i+1:n)
*
               CALL DGEMV( 'No transpose', N-I, I, -ONE, Y( I+1, 1 ),
     $                     LDY, A( I, 1 ), LDA, ONE, A( I, I+1 ), LDA )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, X( I, 1 ), LDX, ONE, A( I, I+1 ), LDA )
*
*              Generate reflection P(i) to annihilate A(i,i+2:n)
*
               CALL DLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I, ONE, Y( I+1, 1 ), LDY,
     $                     A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, NB
*
*           Update A(i,i:n)
*
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, Y( I, 1 ),
     $                  LDY, A( I, 1 ), LDA, ONE, A( I, I ), LDA )
            CALL DGEMV( 'Transpose', I-1, N-I+1, -ONE, A( 1, I ), LDA,
     $                  X( I, 1 ), LDX, ONE, A( I, I ), LDA )
*
*           Generate reflection P(i) to annihilate A(i,i+1:n)
*
            CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I+1, ONE, A( I+1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I+1, I-1, ONE, Y( I, 1 ), LDY,
     $                     A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I+1, ONE, A( 1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
*
*              Update A(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, Y( I, 1 ), LDY, ONE, A( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, X( I+1, 1 ),
     $                     LDX, A( 1, I ), 1, ONE, A( I+1, I ), 1 )
*
*              Generate reflection Q(i) to annihilate A(i+2:m,i)
*
               CALL DLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I+1, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I, ONE, X( I+1, 1 ), LDX,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I, N-I, -ONE, A( 1, I+1 ), LDA,
     $                     Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of DLABRD
*
      END
      SUBROUTINE DLACPY( UPLO, M, N, A, LDA, B, LDB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be copied to B.
*          = 'U':      Upper triangular part
*          = 'L':      Lower triangular part
*          Otherwise:  All of the matrix A
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.  If UPLO = 'U', only the upper triangle
*          or trapezoid is accessed; if UPLO = 'L', only the lower
*          triangle or trapezoid is accessed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (output) DOUBLE PRECISION array, dimension (LDB,N)
*          On exit, B = A in the locations specified by UPLO.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,M).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
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
*
*     End of DLACPY
*
      END
      DOUBLE PRECISION FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
*
*  Description
*  ===========
*
*  DLANGE returns the value
*
*     DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANGE as described
*          above.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.  When M = 0,
*          DLANGE is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*          DLANGE is set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
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
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL DLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANGE = VALUE
      RETURN
*
*     End of DLANGE
*
      END
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y
*     ..
*
*  Purpose
*  =======
*
*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
*
*  Arguments
*  =========
*
*  X       (input) DOUBLE PRECISION
*  Y       (input) DOUBLE PRECISION
*          X and Y specify the values x and y.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, Z
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
*
*     End of DLAPY2
*
      END
      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      DOUBLE PRECISION   TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARF applies a real elementary reflector H to a real m by n matrix
*  C, from either the left or the right. H is represented in the form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) DOUBLE PRECISION array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) DOUBLE PRECISION
*          The value tau in the representation of H.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C' * v
*
            CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO,
     $                  WORK, 1 )
*
*           C := C - v * w'
*
            CALL DGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C * v
*
            CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
*
*           C := C - w * v'
*
            CALL DGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
*
*     End of DLARF
*
      END
      SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFB applies a real block reflector H or its transpose H' to a
*  real m by n matrix C, from either the left or the right.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply H or H' from the Left
*          = 'R': apply H or H' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply H (No transpose)
*          = 'T': apply H' (Transpose)
*
*  DIRECT  (input) CHARACTER*1
*          Indicates how H is formed from a product of elementary
*          reflectors
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Indicates how the vectors which define the elementary
*          reflectors are stored:
*          = 'C': Columnwise
*          = 'R': Rowwise
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  K       (input) INTEGER
*          The order of the matrix T (= the number of elementary
*          reflectors whose product defines the block reflector).
*
*  V       (input) DOUBLE PRECISION array, dimension
*                                (LDV,K) if STOREV = 'C'
*                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
*                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
*          if STOREV = 'R', LDV >= K.
*
*  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
*          The triangular k by k matrix T in the representation of the
*          block reflector.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDA >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
*
*  LDWORK  (input) INTEGER
*          The leading dimension of the array WORK.
*          If SIDE = 'L', LDWORK >= max(1,N);
*          if SIDE = 'R', LDWORK >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DTRMM
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      END IF
*
      IF( LSAME( STOREV, 'C' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1 )    (first K rows)
*                     ( V2 )
*           where  V1  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C1'
*
               DO 10 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             CONTINUE
   30          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C1
*
               DO 40 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
*
         ELSE
*
*           Let  V =  ( V1 )
*                     ( V2 )    (last K rows)
*           where  V2  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C2'
*
               DO 70 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             CONTINUE
   90          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C2
*
               DO 100 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W
*
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
*
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1  V2 )    (V1: first K columns)
*           where  V1  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C1'
*
               DO 130 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             CONTINUE
  150          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C1
*
               DO 160 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
*
            END IF
*
         ELSE
*
*           Let  V =  ( V1  V2 )    (V2: last K columns)
*           where  V2  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C2'
*
               DO 190 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             CONTINUE
  210          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C2
*
               DO 220 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
*
            END IF
*
         END IF
      END IF
*
      RETURN
*
*     End of DLARFB
*
      END
      SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   ALPHA, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFG generates a real elementary reflector H of order n, such
*  that
*
*        H * ( alpha ) = ( beta ),   H' * H = I.
*            (   x   )   (   0  )
*
*  where alpha and beta are scalars, and x is an (n-1)-element real
*  vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a real scalar and v is a real (n-1)-element
*  vector.
*
*  If the elements of x are all zero, then tau = 0 and H is taken to be
*  the unit matrix.
*
*  Otherwise  1 <= tau <= 2.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) DOUBLE PRECISION
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) DOUBLE PRECISION array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) DOUBLE PRECISION
*          The value tau.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
*
      XNORM = DNRM2( N-1, X, INCX )
*
      IF( XNORM.EQ.ZERO ) THEN
*
*        H  =  I
*
         TAU = ZERO
      ELSE
*
*        general case
*
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
*
*           XNORM, BETA may be inaccurate; scale X and recompute them
*
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
*
*           New BETA is at most 1, at least SAFMIN
*
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
*
*           If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
*
      RETURN
*
*     End of DLARFG
*
      END
      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFT forms the triangular factor T of a real block reflector H
*  of order n, which is defined as a product of k elementary reflectors.
*
*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*
*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*
*  If STOREV = 'C', the vector which defines the elementary reflector
*  H(i) is stored in the i-th column of the array V, and
*
*     H  =  I - V * T * V'
*
*  If STOREV = 'R', the vector which defines the elementary reflector
*  H(i) is stored in the i-th row of the array V, and
*
*     H  =  I - V' * T * V
*
*  Arguments
*  =========
*
*  DIRECT  (input) CHARACTER*1
*          Specifies the order in which the elementary reflectors are
*          multiplied to form the block reflector:
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Specifies how the vectors which define the elementary
*          reflectors are stored (see also Further Details):
*          = 'C': columnwise
*          = 'R': rowwise
*
*  N       (input) INTEGER
*          The order of the block reflector H. N >= 0.
*
*  K       (input) INTEGER
*          The order of the triangular factor T (= the number of
*          elementary reflectors). K >= 1.
*
*  V       (input/output) DOUBLE PRECISION array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
*          The k by k triangular factor T of the block reflector.
*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*          lower triangular. The rest of the array is not used.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  Further Details
*  ===============
*
*  The shape of the matrix V and the storage of the vectors which define
*  the H(i) is best illustrated by the following example with n = 5 and
*  k = 3. The elements equal to 1 are not stored; the corresponding
*  array elements are modified but restored on exit. The rest of the
*  array is not used.
*
*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*
*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
*                   ( v1  1    )                     (     1 v2 v2 v2 )
*                   ( v1 v2  1 )                     (        1 v3 v3 )
*                   ( v1 v2 v3 )
*                   ( v1 v2 v3 )
*
*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*
*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
*                   (     1 v3 )
*                   (        1 )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   VII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( LSAME( DIRECT, 'F' ) ) THEN
         DO 20 I = 1, K
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
*
*              general case
*
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
*
*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
*
                  CALL DGEMV( 'Transpose', N-I+1, I-1, -TAU( I ),
     $                        V( I, 1 ), LDV, V( I, I ), 1, ZERO,
     $                        T( 1, I ), 1 )
               ELSE
*
*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
*
                  CALL DGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
               END IF
               V( I, I ) = VII
*
*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
*
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            END IF
   20    CONTINUE
      ELSE
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
*
*              general case
*
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
*
                     CALL DGEMV( 'Transpose', N-K+I, K-I, -TAU( I ),
     $                           V( 1, I+1 ), LDV, V( 1, I ), 1, ZERO,
     $                           T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
*
                     CALL DGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                           T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  END IF
*
*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
*
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
*
*     End of DLARFT
*
      END
      SUBROUTINE DLARTG( F, G, CS, SN, R )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CS, F, G, R, SN
*     ..
*
*  Purpose
*  =======
*
*  DLARTG generate a plane rotation so that
*
*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
*     [ -SN  CS  ]     [ G ]     [ 0 ]
*
*  This is a slower, more accurate version of the BLAS1 routine DROTG,
*  with the following other differences:
*     F and G are unchanged on return.
*     If G=0, then CS=1 and SN=0.
*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
*        floating point operations (saves work in DBDSQR when
*        there are zeros on the diagonal).
*
*  If F exceeds G in magnitude, CS will be positive.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The first component of vector to be rotated.
*
*  G       (input) DOUBLE PRECISION
*          The second component of vector to be rotated.
*
*  CS      (output) DOUBLE PRECISION
*          The cosine of the rotation.
*
*  SN      (output) DOUBLE PRECISION
*          The sine of the rotation.
*
*  R       (output) DOUBLE PRECISION
*          The nonzero component of the rotated vector.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST
      INTEGER            COUNT, I
      DOUBLE PRECISION   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, SQRT
*     ..
*     .. Save statement ..
      SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         SAFMIN = DLAMCH( 'S' )
         EPS = DLAMCH( 'E' )
         SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( DLAMCH( 'B' ) ) / TWO )
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
*
*     End of DLARTG
*
      END
      SUBROUTINE DLAS2( F, G, H, SSMIN, SSMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   F, G, H, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLAS2  computes the singular values of the 2-by-2 matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, SSMIN is the smaller singular value and SSMAX is the
*  larger singular value.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          The smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          The larger singular value.
*
*  Further Details
*  ===============
*
*  Barring over/underflow, all output quantities are correct to within
*  a few units in the last place (ulps), even in the absence of a guard
*  digit in addition/subtraction.
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows, or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
*  ====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AS, AT, AU, C, FA, FHMN, FHMX, GA, HA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
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
*
*              Avoid possible harmful underflow if exponent range
*              asymmetric (true SSMIN may not underflow even if
*              AU underflows)
*
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
*
*     End of DLAS2
*
      END
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASCL multiplies the M by N real matrix A by the real scalar
*  CTO/CFROM.  This is done without over/underflow as long as the final
*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
*  A may be full, upper triangular, lower triangular, upper Hessenberg,
*  or banded.
*
*  Arguments
*  =========
*
*  TYPE    (input) CHARACTER*1
*          TYPE indices the storage type of the input matrix.
*          = 'G':  A is a full matrix.
*          = 'L':  A is a lower triangular matrix.
*          = 'U':  A is an upper triangular matrix.
*          = 'H':  A is an upper Hessenberg matrix.
*          = 'B':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the lower
*                  half stored.
*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the upper
*                  half stored.
*          = 'Z':  A is a band matrix with lower bandwidth KL and upper
*                  bandwidth KU.
*
*  KL      (input) INTEGER
*          The lower bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  KU      (input) INTEGER
*          The upper bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  CFROM   (input) DOUBLE PRECISION
*  CTO     (input) DOUBLE PRECISION
*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
*          without over/underflow if the final result CTO*A(I,J)/CFROM
*          can be represented without over/underflow.  CFROM must be
*          nonzero.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
*          storage type.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  INFO    (output) INTEGER
*          0  - successful exit
*          <0 - if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
*
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
*
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
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASCL', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
*
      CFROMC = CFROM
      CTOC = CTO
*
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
*
      IF( ITYPE.EQ.0 ) THEN
*
*        Full matrix
*
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
*
      ELSE IF( ITYPE.EQ.1 ) THEN
*
*        Lower triangular matrix
*
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
*
      ELSE IF( ITYPE.EQ.2 ) THEN
*
*        Upper triangular matrix
*
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
*
      ELSE IF( ITYPE.EQ.3 ) THEN
*
*        Upper Hessenberg matrix
*
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
*
      ELSE IF( ITYPE.EQ.4 ) THEN
*
*        Lower half of a symmetric band matrix
*
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
*
      ELSE IF( ITYPE.EQ.5 ) THEN
*
*        Upper half of a symmetric band matrix
*
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
*
      ELSE IF( ITYPE.EQ.6 ) THEN
*
*        Band matrix
*
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
*
      END IF
*
      IF( .NOT.DONE )
     $   GO TO 10
*
      RETURN
*
*     End of DLASCL
*
      END
      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be set.
*          = 'U':      Upper triangular part is set; the strictly lower
*                      triangular part of A is not changed.
*          = 'L':      Lower triangular part is set; the strictly upper
*                      triangular part of A is not changed.
*          Otherwise:  All of the matrix A is set.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  ALPHA   (input) DOUBLE PRECISION
*          The constant to which the offdiagonal elements are to be set.
*
*  BETA    (input) DOUBLE PRECISION
*          The constant to which the diagonal elements are to be set.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On exit, the leading m-by-n submatrix of A is set as follows:
*
*          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
*          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
*          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
*
*          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Set the strictly upper triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
*
*        Set the strictly lower triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
*
      ELSE
*
*        Set the leading m-by-n submatrix to ALPHA.
*
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
*
*     Set the first min(M,N) diagonal elements to BETA.
*
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
*
      RETURN
*
*     End of DLASET
*
      END
      SUBROUTINE DLASQ1( N, D, E, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ1 computes the singular values of a real N-by-N bidiagonal
*     matrix with diagonal D and off-diagonal E. The singular values are
*     computed to high relative accuracy, barring over/underflow or
*     denormalization. The algorithm is described in
*
*     "Accurate singular values and differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett,
*     Numer. Math., Vol-67, No. 2, pp. 191-230,1994.
*
*     See also
*     "Implementation of differential qd algorithms," by
*     K. V. Fernando and B. N. Parlett, Technical Report,
*     Department of Mathematics, University of California at Berkeley,
*     1994 (Under preparation).
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          The number of rows and columns in the matrix. N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, D contains the diagonal elements of the
*          bidiagonal matrix whose SVD is desired. On normal exit,
*          D contains the singular values in decreasing order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, elements E(1:N-1) contain the off-diagonal elements
*          of the bidiagonal matrix whose SVD is desired.
*          On exit, E is overwritten.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm did not converge;  i
*                specifies how many superdiagonals did not converge.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HUNDRD
      PARAMETER          ( HUNDRD = 100.0D0 )
      DOUBLE PRECISION   TWO56
      PARAMETER          ( TWO56 = 256.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            RESTRT
      INTEGER            I, IERR, J, KE, KEND, M, NY
      DOUBLE PRECISION   DM, DX, EPS, SCL, SFMIN, SIG1, SIG2, SIGMN,
     $                   SIGMX, SMALL2, THRESH, TOL, TOL2, TOLMUL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAS2, DLASCL, DLASQ2, DLASRT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -2
         CALL XERBLA( 'DLASQ1', -INFO )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         D( 1 ) = ABS( D( 1 ) )
         RETURN
      ELSE IF( N.EQ.2 ) THEN
         CALL DLAS2( D( 1 ), E( 1 ), D( 2 ), SIGMN, SIGMX )
         D( 1 ) = SIGMX
         D( 2 ) = SIGMN
         RETURN
      END IF
*
*     Estimate the largest singular value
*
      SIGMX = ZERO
      DO 10 I = 1, N - 1
         SIGMX = MAX( SIGMX, ABS( E( I ) ) )
   10 CONTINUE
*
*     Early return if sigmx is zero (matrix is already diagonal)
*
      IF( SIGMX.EQ.ZERO )
     $   GO TO 70
*
      DO 20 I = 1, N
         D( I ) = ABS( D( I ) )
         SIGMX = MAX( SIGMX, D( I ) )
   20 CONTINUE
*
*     Get machine parameters
*
      EPS = DLAMCH( 'EPSILON' )
      SFMIN = DLAMCH( 'SAFE MINIMUM' )
*
*     Compute singular values to relative accuracy TOL
*     It is assumed that tol**2 does not underflow.
*
      TOLMUL = MAX( TEN, MIN( HUNDRD, EPS**( -MEIGTH ) ) )
      TOL = TOLMUL*EPS
      TOL2 = TOL**2
*
      THRESH = SIGMX*SQRT( SFMIN )*TOL
*
*     Scale matrix so the square of the largest element is
*     1 / ( 256 * SFMIN )
*
      SCL = SQRT( ONE / ( TWO56*SFMIN ) )
      SMALL2 = ONE / ( TWO56*TOLMUL**2 )
      CALL DCOPY( N, D, 1, WORK( 1 ), 1 )
      CALL DCOPY( N-1, E, 1, WORK( N+1 ), 1 )
      CALL DLASCL( 'G', 0, 0, SIGMX, SCL, N, 1, WORK( 1 ), N, IERR )
      CALL DLASCL( 'G', 0, 0, SIGMX, SCL, N-1, 1, WORK( N+1 ), N-1,
     $             IERR )
*
*     Square D and E (the input for the qd algorithm)
*
      DO 30 J = 1, 2*N - 1
         WORK( J ) = WORK( J )**2
   30 CONTINUE
*
*     Apply qd algorithm
*
      M = 0
      E( N ) = ZERO
      DX = WORK( 1 )
      DM = DX
      KE = 0
      RESTRT = .FALSE.
      DO 60 I = 1, N
         IF( ABS( E( I ) ).LE.THRESH .OR. WORK( N+I ).LE.TOL2*
     $       ( DM / DBLE( I-M ) ) ) THEN
            NY = I - M
            IF( NY.EQ.1 ) THEN
               GO TO 50
            ELSE IF( NY.EQ.2 ) THEN
               CALL DLAS2( D( M+1 ), E( M+1 ), D( M+2 ), SIG1, SIG2 )
               D( M+1 ) = SIG1
               D( M+2 ) = SIG2
            ELSE
               KEND = KE + 1 - M
               CALL DLASQ2( NY, D( M+1 ), E( M+1 ), WORK( M+1 ),
     $                      WORK( M+N+1 ), EPS, TOL2, SMALL2, DM, KEND,
     $                      INFO )
*
*                 Return, INFO = number of unconverged superdiagonals
*
               IF( INFO.NE.0 ) THEN
                  INFO = INFO + I
                  RETURN
               END IF
*
*                 Undo scaling
*
               DO 40 J = M + 1, M + NY
                  D( J ) = SQRT( D( J ) )
   40          CONTINUE
               CALL DLASCL( 'G', 0, 0, SCL, SIGMX, NY, 1, D( M+1 ), NY,
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
*
*     Sort the singular values into decreasing order
*
   70 CONTINUE
      CALL DLASRT( 'D', N, D, INFO )
      RETURN
*
*     End of DLASQ1
*
      END
      SUBROUTINE DLASQ2( M, Q, E, QQ, EE, EPS, TOL2, SMALL2, SUP, KEND,
     $                   INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, KEND, M
      DOUBLE PRECISION   EPS, SMALL2, SUP, TOL2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), EE( * ), Q( * ), QQ( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ2 computes the singular values of a real N-by-N unreduced
*     bidiagonal matrix with squared diagonal elements in Q and
*     squared off-diagonal elements in E. The singular values are
*     computed to relative accuracy TOL, barring over/underflow or
*     denormalization.
*
*     Arguments
*     =========
*
*  M       (input) INTEGER
*          The number of rows and columns in the matrix. M >= 0.
*
*  Q       (output) DOUBLE PRECISION array, dimension (M)
*          On normal exit, contains the squared singular values.
*
*  E       (workspace) DOUBLE PRECISION array, dimension (M)
*
*  QQ      (input/output) DOUBLE PRECISION array, dimension (M)
*          On entry, QQ contains the squared diagonal elements of the
*          bidiagonal matrix whose SVD is desired.
*          On exit, QQ is overwritten.
*
*  EE      (input/output) DOUBLE PRECISION array, dimension (M)
*          On entry, EE(1:N-1) contains the squared off-diagonal
*          elements of the bidiagonal matrix whose SVD is desired.
*          On exit, EE is overwritten.
*
*  EPS     (input) DOUBLE PRECISION
*          Machine epsilon.
*
*  TOL2    (input) DOUBLE PRECISION
*          Desired relative accuracy of computed eigenvalues
*          as defined in DLASQ1.
*
*  SMALL2  (input) DOUBLE PRECISION
*          A threshold value as defined in DLASQ1.
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest eigenvalue.
*
*  KEND    (input/output) INTEGER
*          Index where minimum d occurs.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm did not converge;  i
*                specifies how many superdiagonals did not converge.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   FOUR, HALF
      PARAMETER          ( FOUR = 4.0D+0, HALF = 0.5D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            ICONV, IPHASE, ISP, N, OFF, OFF1
      DOUBLE PRECISION   QEMAX, SIGMA, XINF, XX, YY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, NINT, SQRT
*     ..
*     .. Executable Statements ..
      N = M
*
*     Set the default maximum number of iterations
*
      OFF = 0
      OFF1 = OFF + 1
      SIGMA = ZERO
      XINF = ZERO
      ICONV = 0
      IPHASE = 2
*
*     Try deflation at the bottom
*
*     1x1 deflation
*
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
*
*     2x2 deflation
*
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
*
   20 CONTINUE
      IF( N.EQ.0 ) THEN
*
*         The lower branch is finished
*
         IF( OFF.EQ.0 ) THEN
*
*         No upper branch; return to DLASQ1
*
            RETURN
         ELSE
*
*         Going back to upper branch
*
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
*
*     1x1 Solver
*
         IF( IPHASE.EQ.1 ) THEN
            Q( OFF1 ) = Q( OFF1 ) + SIGMA
         ELSE
            Q( OFF1 ) = QQ( OFF1 ) + SIGMA
         END IF
         N = 0
         GO TO 20
*
*     2x2 Solver
*
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
      CALL DLASQ3( N, Q( OFF1 ), E( OFF1 ), QQ( OFF1 ), EE( OFF1 ), SUP,
     $             SIGMA, KEND, OFF, IPHASE, ICONV, EPS, TOL2, SMALL2 )
      IF( SUP.LT.ZERO ) THEN
         INFO = N + OFF
         RETURN
      END IF
      OFF1 = OFF + 1
      GO TO 20
*
*     End of DLASQ2
*
      END
      SUBROUTINE DLASQ3( N, Q, E, QQ, EE, SUP, SIGMA, KEND, OFF, IPHASE,
     $                   ICONV, EPS, TOL2, SMALL2 )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            ICONV, IPHASE, KEND, N, OFF
      DOUBLE PRECISION   EPS, SIGMA, SMALL2, SUP, TOL2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), EE( * ), Q( * ), QQ( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ3 is the workhorse of the whole bidiagonal SVD algorithm.
*     This can be described as the differential qd with shifts.
*
*     Arguments
*     =========
*
*  N       (input/output) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 3.
*          On exit N is non-negative and less than the input value.
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (N)
*          Q array in ping (see IPHASE below)
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          E array in ping (see IPHASE below)
*
*  QQ      (input/output) DOUBLE PRECISION array, dimension (N)
*          Q array in pong (see IPHASE below)
*
*  EE      (input/output) DOUBLE PRECISION array, dimension (N)
*          E array in pong (see IPHASE below)
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest eigenvalue
*
*  SIGMA   (input/output) DOUBLE PRECISION
*          Accumulated shift for the present submatrix
*
*  KEND    (input/output) INTEGER
*          Index where minimum D(i) occurs in recurrence for
*          splitting criterion
*
*  OFF     (input/output) INTEGER
*          Offset for arrays
*
*  IPHASE  (input/output) INTEGER
*          If IPHASE = 1 (ping) then data is in Q and E arrays
*          If IPHASE = 2 (pong) then data is in QQ and EE arrays
*
*  ICONV   (input) INTEGER
*          If ICONV = 0 a bottom part of a matrix (with a split)
*          If ICONV =-3 a top part of a matrix (with a split)
*
*  EPS     (input) DOUBLE PRECISION
*          Machine epsilon
*
*  TOL2    (input) DOUBLE PRECISION
*          Square of the relative tolerance TOL as defined in DLASQ1
*
*  SMALL2  (input) DOUBLE PRECISION
*          A threshold value as defined in DLASQ1
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      INTEGER            NPP
      PARAMETER          ( NPP = 32 )
      INTEGER            IPP
      PARAMETER          ( IPP = 5 )
      DOUBLE PRECISION   HALF, FOUR
      PARAMETER          ( HALF = 0.5D+0, FOUR = 4.0D+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 2 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LDEF, LSPLIT
      INTEGER            I, IC, ICNT, IFL, IP, ISP, K1END, K2END, KE,
     $                   KS, MAXIT, N1, N2
      DOUBLE PRECISION   D, DM, QEMAX, T1, TAU, TOLX, TOLY, TOLZ, XX, YY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASQ4
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
      ICNT = 0
      TAU = ZERO
      DM = SUP
      TOLX = SIGMA*TOL2
      TOLZ = MAX( SMALL2, SIGMA )*TOL2
*
*     Set maximum number of iterations
*
      MAXIT = 100*N
*
*     Flipping
*
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
               CALL DCOPY( N, Q, 1, QQ, -1 )
               CALL DCOPY( N-1, E, 1, EE, -1 )
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
               CALL DCOPY( N, QQ, 1, Q, -1 )
               CALL DCOPY( N-1, EE, 1, E, -1 )
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
*
*     The ping section of the code
*
   30 CONTINUE
      IFL = 0
*
*     Compute the shift
*
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
         CALL DLASQ4( N2, Q( N1 ), E( N1 ), TAU, SUP )
      END IF
   40 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -ONE
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     dqd algorithm
*
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
*
*     Penultimate dqd step (in ping)
*
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in ping)
*
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      ELSE
*
*     The dqds algorithm (in ping)
*
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
*
*     Penultimate dqds step (in ping)
*
         K2END = KE
         QQ( N-2 ) = D + E( N-2 )
         D = ( D / QQ( N-2 ) )*Q( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 120
         END IF
*
*     Final dqds step (in ping)
*
         K1END = KE
         QQ( N-1 ) = D + E( N-1 )
         D = ( D / QQ( N-1 ) )*Q( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         QQ( N ) = D
      END IF
*
*        Convergence when QQ(N) is small (in ping)
*
      IF( ABS( QQ( N ) ).LE.SIGMA*TOL2 ) THEN
         QQ( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( QQ( N ).LT.ZERO )
     $   GO TO 120
*
*     Non-negative qd array: Update the e's
*
      DO 70 I = 1, N - 1
         EE( I ) = ( E( I ) / QQ( I ) )*Q( I+1 )
   70 CONTINUE
*
*     Updating sigma and iphase in ping
*
      SIGMA = SIGMA + TAU
      IPHASE = 2
   80 CONTINUE
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
      TOLZ = MAX( SIGMA, SMALL2 )*TOL2
*
*     Checking for deflation and convergence (in ping)
*
   90 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in ping)
*
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
*
*        Deflation: bottom 2x2 (in ping)
*
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
*
*     Updating bounds before going to pong
*
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
*
*     Checking for splitting in ping
*
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
*
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
*
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
*
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
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL DCOPY( N-KE, E( KE ), 1, QQ( KE ), 1 )
         QQ( N ) = ZERO
         CALL DCOPY( N-KE, Q( KE+1 ), 1, EE( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 130
*
*     A new shift when the previous failed (in ping)
*
  120 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (ping)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 40
*
*     The asymptotic shift (in ping)
*
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 40
      END IF
*
*     the pong section of the code
*
  130 CONTINUE
      IFL = 0
*
*     Compute the shift (in pong)
*
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
         CALL DLASQ4( N2, QQ( N1 ), EE( N1 ), TAU, SUP )
      END IF
  140 CONTINUE
      ICNT = ICNT + 1
      IF( ICNT.GT.MAXIT ) THEN
         SUP = -SUP
         RETURN
      END IF
      IF( TAU.EQ.ZERO ) THEN
*
*     The dqd algorithm (in pong)
*
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
*
*     Penultimate dqd step (in pong)
*
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
         END IF
*
*     Final dqd step (in pong)
*
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N )
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      ELSE
*
*     The dqds algorithm (in pong)
*
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
*
*     Penultimate dqds step (in pong)
*
         K2END = KE
         Q( N-2 ) = D + EE( N-2 )
         D = ( D / Q( N-2 ) )*QQ( N-1 ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N - 1
            IF( D.LT.ZERO )
     $         GO TO 220
         END IF
*
*     Final dqds step (in pong)
*
         K1END = KE
         Q( N-1 ) = D + EE( N-1 )
         D = ( D / Q( N-1 ) )*QQ( N ) - TAU
         IF( DM.GT.D ) THEN
            DM = D
            KE = N
         END IF
         Q( N ) = D
      END IF
*
*        Convergence when is small (in pong)
*
      IF( ABS( Q( N ) ).LE.SIGMA*TOL2 ) THEN
         Q( N ) = ZERO
         DM = ZERO
         KE = N
      END IF
      IF( Q( N ).LT.ZERO )
     $   GO TO 220
*
*     Non-negative qd array: Update the e's
*
      DO 170 I = 1, N - 1
         E( I ) = ( EE( I ) / Q( I ) )*QQ( I+1 )
  170 CONTINUE
*
*     Updating sigma and iphase in pong
*
      SIGMA = SIGMA + TAU
  180 CONTINUE
      IPHASE = 1
      TOLX = SIGMA*TOL2
      TOLY = SIGMA*EPS
*
*     Checking for deflation and convergence (in pong)
*
  190 CONTINUE
      IF( N.LE.2 )
     $   RETURN
*
*        Deflation: bottom 1x1 (in pong)
*
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
*
*        Deflation: bottom 2x2 (in pong)
*
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
*
*     Updating bounds before going to pong
*
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
*
*     Checking for splitting in pong
*
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
*
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
*
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
*
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
*
*     Coincidence
*
      IF( TAU.EQ.ZERO .AND. DM.LE.TOLZ .AND. KEND.NE.N .AND. ICONV.EQ.
     $    0 .AND. ICNT.GT.0 ) THEN
         CALL DCOPY( N-KE, EE( KE ), 1, Q( KE ), 1 )
         Q( N ) = ZERO
         CALL DCOPY( N-KE, QQ( KE+1 ), 1, E( KE ), 1 )
         SUP = ZERO
      END IF
      ICONV = 0
      GO TO 30
*
*     Computation of a new shift when the previous failed (in pong)
*
  220 CONTINUE
      IFL = IFL + 1
      SUP = TAU
*
*     SUP is small or
*     Too many bad shifts (in pong)
*
      IF( SUP.LE.TOLZ .OR. IFL.GE.IFLMAX ) THEN
         TAU = ZERO
         GO TO 140
*
*     The asymptotic shift (in pong)
*
      ELSE
         TAU = MAX( TAU+D, ZERO )
         IF( TAU.LE.TOLZ )
     $      TAU = ZERO
         GO TO 140
      END IF
*
*     End of DLASQ3
*
      END
      SUBROUTINE DLASQ4( N, Q, E, TAU, SUP )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            N
      DOUBLE PRECISION   SUP, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * ), Q( * )
*     ..
*
*     Purpose
*     =======
*
*     DLASQ4 estimates TAU, the smallest eigenvalue of a matrix. This
*     routine improves the input value of SUP which is an upper bound
*     for the smallest eigenvalue for this matrix .
*
*     Arguments
*     =========
*
*  N       (input) INTEGER
*          On entry, N specifies the number of rows and columns
*          in the matrix. N must be at least 0.
*
*  Q       (input) DOUBLE PRECISION array, dimension (N)
*          Q array
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          E array
*
*  TAU     (output) DOUBLE PRECISION
*          Estimate of the shift
*
*  SUP     (input/output) DOUBLE PRECISION
*          Upper bound for the smallest singular value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
      DOUBLE PRECISION   BIS, BIS1
      PARAMETER          ( BIS = 0.9999D+0, BIS1 = 0.7D+0 )
      INTEGER            IFLMAX
      PARAMETER          ( IFLMAX = 5 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IFL
      DOUBLE PRECISION   D, DM, XINF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
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
*
*     End of DLASQ4
*
      END
      SUBROUTINE DLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASR   performs the transformation
*
*     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )
*
*     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )
*
*  where A is an m by n real matrix and P is an orthogonal matrix,
*  consisting of a sequence of plane rotations determined by the
*  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
*  and z = n when SIDE = 'R' or 'r' ):
*
*  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
*
*     P = P( z - 1 )*...*P( 2 )*P( 1 ),
*
*  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
*
*     P = P( 1 )*P( 2 )*...*P( z - 1 ),
*
*  where  P( k ) is a plane rotation matrix for the following planes:
*
*     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
*        the plane ( k, k + 1 )
*
*     when  PIVOT = 'T' or 't'  ( Top pivot ),
*        the plane ( 1, k + 1 )
*
*     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
*        the plane ( k, z )
*
*  c( k ) and s( k )  must contain the  cosine and sine that define the
*  matrix  P( k ).  The two by two plane rotation part of the matrix
*  P( k ), R( k ), is assumed to be of the form
*
*     R( k ) = (  c( k )  s( k ) ).
*              ( -s( k )  c( k ) )
*
*  This version vectorises across rows of the array A when SIDE = 'L'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          Specifies whether the plane rotation matrix P is applied to
*          A on the left or the right.
*          = 'L':  Left, compute A := P*A
*          = 'R':  Right, compute A:= A*P'
*
*  DIRECT  (input) CHARACTER*1
*          Specifies whether P is a forward or backward sequence of
*          plane rotations.
*          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
*          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
*
*  PIVOT   (input) CHARACTER*1
*          Specifies the plane for which P(k) is a plane rotation
*          matrix.
*          = 'V':  Variable pivot, the plane (k,k+1)
*          = 'T':  Top pivot, the plane (1,k+1)
*          = 'B':  Bottom pivot, the plane (k,z)
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  If m <= 1, an immediate
*          return is effected.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  If n <= 1, an
*          immediate return is effected.
*
*  C, S    (input) DOUBLE PRECISION arrays, dimension
*                  (M-1) if SIDE = 'L'
*                  (N-1) if SIDE = 'R'
*          c(k) and s(k) contain the cosine and sine that define the
*          matrix P(k).  The two by two plane rotation part of the
*          matrix P(k), R(k), is assumed to be of the form
*          R( k ) = (  c( k )  s( k ) ).
*                   ( -s( k )  c( k ) )
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.  On exit, A is overwritten by P*A if
*          SIDE = 'R' or by A*P' if SIDE = 'L'.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      DOUBLE PRECISION   CTEMP, STEMP, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
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
         CALL XERBLA( 'DLASR ', INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  P * A
*
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
*
*        Form A * P'
*
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
*
      RETURN
*
*     End of DLASR
*
      END
      SUBROUTINE DLASRT( ID, N, D, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          ID
      INTEGER            INFO, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * )
*     ..
*
*  Purpose
*  =======
*
*  Sort the numbers in D in increasing order (if ID = 'I') or
*  in decreasing order (if ID = 'D' ).
*
*  Use Quick Sort, reverting to Insertion sort on arrays of
*  size <= 20. Dimension of STACK limits N to about 2**32.
*
*  Arguments
*  =========
*
*  ID      (input) CHARACTER*1
*          = 'I': sort D in increasing order;
*          = 'D': sort D in decreasing order.
*
*  N       (input) INTEGER
*          The length of the array D.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the array to be sorted.
*          On exit, D has been sorted into increasing order
*          (D(1) <= ... <= D(N) ) or into decreasing order
*          (D(1) >= ... >= D(N) ), depending on ID.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
*     ..
*     .. Local Scalars ..
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP
*     ..
*     .. Local Arrays ..
      INTEGER            STACK( 2, 32 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input paramters.
*
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
         CALL XERBLA( 'DLASRT', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.1 )
     $   RETURN
*
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
*
*        Do Insertion sort on D( START:ENDD )
*
         IF( DIR.EQ.0 ) THEN
*
*           Sort into decreasing order
*
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
*
         ELSE
*
*           Sort into increasing order
*
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
*
         END IF
*
      ELSE IF( ENDD-START.GT.SELECT ) THEN
*
*        Partition D( START:ENDD ) and stack parts, largest one first
*
*        Choose partition entry as median of 3
*
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
*
         IF( DIR.EQ.0 ) THEN
*
*           Sort into decreasing order
*
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
*
*           Sort into increasing order
*
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
*
*     End of DLASRT
*
      END
      SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASSQ  returns the values  scl  and  smsq  such that
*
*     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
*
*  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
*  assumed to be non-negative and  scl  returns the value
*
*     scl = max( scale, abs( x( i ) ) ).
*
*  scale and sumsq must be supplied in SCALE and SUMSQ and
*  scl and smsq are overwritten on SCALE and SUMSQ respectively.
*
*  The routine makes only one pass through the vector x.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements to be used from the vector X.
*
*  X       (input) DOUBLE PRECISION
*          The vector for which a scaled sum of squares is computed.
*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector X.
*          INCX > 0.
*
*  SCALE   (input/output) DOUBLE PRECISION
*          On entry, the value  scale  in the equation above.
*          On exit, SCALE is overwritten with  scl , the scaling factor
*          for the sum of squares.
*
*  SUMSQ   (input/output) DOUBLE PRECISION
*          On entry, the value  sumsq  in the equation above.
*          On exit, SUMSQ is overwritten with  smsq , the basic sum of
*          squares from which  scl  has been factored out.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   ABSXI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
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
*
*     End of DLASSQ
*
      END
      SUBROUTINE DLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLASV2 computes the singular value decomposition of a 2-by-2
*  triangular matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
*  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
*  right singular vectors for abs(SSMAX), giving the decomposition
*
*     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
*     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          abs(SSMIN) is the smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          abs(SSMAX) is the larger singular value.
*
*  SNL     (output) DOUBLE PRECISION
*  CSL     (output) DOUBLE PRECISION
*          The vector (CSL, SNL) is a unit left singular vector for the
*          singular value abs(SSMAX).
*
*  SNR     (output) DOUBLE PRECISION
*  CSR     (output) DOUBLE PRECISION
*          The vector (CSR, SNR) is a unit right singular vector for the
*          singular value abs(SSMAX).
*
*  Further Details
*  ===============
*
*  Any input parameter may be aliased with any output parameter.
*
*  Barring over/underflow and assuming a guard digit in subtraction, all
*  output quantities are correct to within a few units in the last
*  place (ulps).
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   FOUR
      PARAMETER          ( FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            GASMAL, SWAP
      INTEGER            PMAX
      DOUBLE PRECISION   A, CLT, CRT, D, FA, FT, GA, GT, HA, HT, L, M,
     $                   MM, R, S, SLT, SRT, T, TEMP, TSIGN, TT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Executable Statements ..
*
      FT = F
      FA = ABS( FT )
      HT = H
      HA = ABS( H )
*
*     PMAX points to the maximum absolute element of matrix
*       PMAX = 1 if F largest in absolute values
*       PMAX = 2 if G largest in absolute values
*       PMAX = 3 if H largest in absolute values
*
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
*
*        Now FA .ge. HA
*
      END IF
      GT = G
      GA = ABS( GT )
      IF( GA.EQ.ZERO ) THEN
*
*        Diagonal matrix
*
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
            IF( ( FA / GA ).LT.DLAMCH( 'EPS' ) ) THEN
*
*              Case of very large GA
*
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
*
*           Normal case
*
            D = FA - HA
            IF( D.EQ.FA ) THEN
*
*              Copes with infinite F or H
*
               L = ONE
            ELSE
               L = D / FA
            END IF
*
*           Note that 0 .le. L .le. 1
*
            M = GT / FT
*
*           Note that abs(M) .le. 1/macheps
*
            T = TWO - L
*
*           Note that T .ge. 1
*
            MM = M*M
            TT = T*T
            S = SQRT( TT+MM )
*
*           Note that 1 .le. S .le. 1 + 1/macheps
*
            IF( L.EQ.ZERO ) THEN
               R = ABS( M )
            ELSE
               R = SQRT( L*L+MM )
            END IF
*
*           Note that 0 .le. R .le. 1 + 1/macheps
*
            A = HALF*( S+R )
*
*           Note that 1 .le. A .le. 1 + abs(M)
*
            SSMIN = HA / A
            SSMAX = FA*A
            IF( MM.EQ.ZERO ) THEN
*
*              Note that M is very tiny
*
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
*
*     Correct signs of SSMAX and SSMIN
*
      IF( PMAX.EQ.1 )
     $   TSIGN = SIGN( ONE, CSR )*SIGN( ONE, CSL )*SIGN( ONE, F )
      IF( PMAX.EQ.2 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, CSL )*SIGN( ONE, G )
      IF( PMAX.EQ.3 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, SNL )*SIGN( ONE, H )
      SSMAX = SIGN( SSMAX, TSIGN )
      SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) )
      RETURN
*
*     End of DLASV2
*
      END
      SUBROUTINE DORG2R( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORG2R generates an m by n real matrix Q with orthonormal columns,
*  which is defined as the first n columns of a product of k elementary
*  reflectors of order m
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DORG2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
*     Initialise columns k+1:n to columns of the unit matrix
*
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( J, J ) = ONE
   20 CONTINUE
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the left
*
         IF( I.LT.N ) THEN
            A( I, I ) = ONE
            CALL DLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
         IF( I.LT.M )
     $      CALL DSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( L, I ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of DORG2R
*
      END
      SUBROUTINE DORGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          VECT
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGBR generates one of the real orthogonal matrices Q or P**T
*  determined by DGEBRD when reducing a real matrix A to bidiagonal
*  form: A = Q * B * P**T.  Q and P**T are defined as products of
*  elementary reflectors H(i) or G(i) respectively.
*
*  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
*  is of order M:
*  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
*  columns of Q, where m >= n >= k;
*  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
*  M-by-M matrix.
*
*  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
*  is of order N:
*  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
*  rows of P**T, where n >= m >= k;
*  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
*  an N-by-N matrix.
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          Specifies whether the matrix Q or the matrix P**T is
*          required, as defined in the transformation applied by DGEBRD:
*          = 'Q':  generate Q;
*          = 'P':  generate P**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q or P**T to be returned.
*          M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q or P**T to be returned.
*          N >= 0.
*          If VECT = 'Q', M >= N >= min(M,K);
*          if VECT = 'P', N >= M >= min(N,K).
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original M-by-K
*          matrix reduced by DGEBRD.
*          If VECT = 'P', the number of rows in the original K-by-N
*          matrix reduced by DGEBRD.
*          K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the vectors which define the elementary reflectors,
*          as returned by DGEBRD.
*          On exit, the M-by-N matrix Q or P**T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension
*                                (min(M,K)) if VECT = 'Q'
*                                (min(N,K)) if VECT = 'P'
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i), which determines Q or P**T, as
*          returned by DGEBRD in its array argument TAUQ or TAUP.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
*          For optimum performance LWORK >= min(M,N)*NB, where NB
*          is the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WANTQ
      INTEGER            I, IINFO, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DORGLQ, DORGQR, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DORGBR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( WANTQ ) THEN
*
*        Form Q, determined by a call to DGEBRD to reduce an m-by-k
*        matrix
*
         IF( M.GE.K ) THEN
*
*           If m >= k, assume m >= n >= k
*
            CALL DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If m < k, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           column to the right, and set the first row and column of Q
*           to those of the unit matrix
*
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
*
*              Form Q(2:m,2:m)
*
               CALL DORGQR( M-1, M-1, M-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      ELSE
*
*        Form P', determined by a call to DGEBRD to reduce a k-by-n
*        matrix
*
         IF( K.LT.N ) THEN
*
*           If k < n, assume k <= m <= n
*
            CALL DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If k >= n, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           row downward, and set the first row and column of P' to
*           those of the unit matrix
*
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
*
*              Form P'(2:n,2:n)
*
               CALL DORGLQ( N-1, N-1, N-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      END IF
      RETURN
*
*     End of DORGBR
*
      END
      SUBROUTINE DORGL2( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORGL2 generates an m by n real matrix Q with orthonormal rows,
*  which is defined as the first m rows of a product of k elementary
*  reflectors of order n
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by DGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by DGELQF in the first k rows of its array argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DORGL2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 )
     $   RETURN
*
      IF( K.LT.M ) THEN
*
*        Initialise rows k+1:m to rows of the unit matrix
*
         DO 20 J = 1, N
            DO 10 L = K + 1, M
               A( L, J ) = ZERO
   10       CONTINUE
            IF( J.GT.K .AND. J.LE.M )
     $         A( J, J ) = ONE
   20    CONTINUE
      END IF
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the right
*
         IF( I.LT.N ) THEN
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
               CALL DLARF( 'Right', M-I, N-I+1, A( I, I ), LDA,
     $                     TAU( I ), A( I+1, I ), LDA, WORK )
            END IF
            CALL DSCAL( N-I, -TAU( I ), A( I, I+1 ), LDA )
         END IF
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( I, L ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of DORGL2
*
      END
      SUBROUTINE DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGLQ generates an M-by-N real matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by DGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by DGELQF in the first k rows of its array argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is
*          the optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORGL2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DORGLQ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV( 1, 'DORGLQ', ' ', M, N, K, -1 )
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DORGLQ', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DORGLQ', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk rows are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(kk+1:m,1:kk) to zero.
*
         DO 20 J = 1, KK
            DO 10 I = KK + 1, M
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.M )
     $   CALL DORGL2( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i+ib:m,i:n) from the right
*
               CALL DLARFB( 'Right', 'Transpose', 'Forward', 'Rowwise',
     $                      M-I-IB+1, N-I+1, IB, A( I, I ), LDA, WORK,
     $                      LDWORK, A( I+IB, I ), LDA, WORK( IB+1 ),
     $                      LDWORK )
            END IF
*
*           Apply H' to columns i:n of current block
*
            CALL DORGL2( IB, N-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set columns 1:i-1 of current block to zero
*
            DO 40 J = 1, I - 1
               DO 30 L = I, I + IB - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORGLQ
*
      END
      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORG2R, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
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
         CALL XERBLA( 'DORGQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.
*
      NB = ILAENV( 1, 'DORGQR', ' ', M, N, K, -1 )
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DORGQR', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DORGQR', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk columns are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(1:kk,kk+1:n) to zero.
*
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.N )
     $   CALL DORG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
*
*           Apply H to rows i:m of current block
*
            CALL DORG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set rows 1:i-1 of current block to zero
*
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORGQR
*
      END
      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORM2R overwrites the general real m by n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'T', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'T',
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'T': apply Q' (Transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
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
         CALL XERBLA( 'DORM2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
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
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i)
*
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of DORM2R
*
      END
      SUBROUTINE DORMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
     $                   LDC, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, VECT
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      P * C          C * P
*  TRANS = 'T':      P**T * C       C * P**T
*
*  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
*  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
*  P**T are defined as products of elementary reflectors H(i) and G(i)
*  respectively.
*
*  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
*  order of the orthogonal matrix Q or P**T that is applied.
*
*  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
*  if nq >= k, Q = H(1) H(2) . . . H(k);
*  if nq < k, Q = H(1) H(2) . . . H(nq-1).
*
*  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
*  if k < nq, P = G(1) G(2) . . . G(k);
*  if k >= nq, P = G(1) G(2) . . . G(nq-1).
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          = 'Q': apply Q or Q**T;
*          = 'P': apply P or P**T.
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q, Q**T, P or P**T from the Left;
*          = 'R': apply Q, Q**T, P or P**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q  or P;
*          = 'T':  Transpose, apply Q**T or P**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original
*          matrix reduced by DGEBRD.
*          If VECT = 'P', the number of rows in the original
*          matrix reduced by DGEBRD.
*          K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                                (LDA,min(nq,K)) if VECT = 'Q'
*                                (LDA,nq)        if VECT = 'P'
*          The vectors which define the elementary reflectors H(i) and
*          G(i), whose products determine the matrices Q and P, as
*          returned by DGEBRD.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If VECT = 'Q', LDA >= max(1,nq);
*          if VECT = 'P', LDA >= max(1,min(nq,K)).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i) which determines Q or P, as returned
*          by DGEBRD in the array argument TAUQ or TAUP.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
*          or P*C or P**T*C or C*P or C*P**T.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            APPLYQ, LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I1, I2, IINFO, MI, NI, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DORMLQ, DORMQR, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      APPLYQ = LSAME( VECT, 'Q' )
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q or P and NW is the minimum dimension of WORK
*
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
         CALL XERBLA( 'DORMBR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      WORK( 1 ) = 1
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      IF( APPLYQ ) THEN
*
*        Apply Q
*
         IF( NQ.GE.K ) THEN
*
*           Q was determined by a call to DGEBRD with nq >= k
*
            CALL DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           Q was determined by a call to DGEBRD with nq < k
*
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
            CALL DORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                   C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      ELSE
*
*        Apply P
*
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
         IF( NQ.GT.K ) THEN
*
*           P was determined by a call to DGEBRD with nq > k
*
            CALL DORMLQ( SIDE, TRANST, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           P was determined by a call to DGEBRD with nq <= k
*
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
            CALL DORMLQ( SIDE, TRANST, MI, NI, NQ-1, A( 1, 2 ), LDA,
     $                   TAU, C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      END IF
      RETURN
*
*     End of DORMBR
*
      END
      SUBROUTINE DORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORML2 overwrites the general real m by n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'T', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'T',
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k) . . . H(2) H(1)
*
*  as returned by DGELQF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'T': apply Q' (Transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
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
         CALL XERBLA( 'DORML2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
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
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i)
*
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), LDA, TAU( I ),
     $               C( IC, JC ), LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of DORML2
*
      END
      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORMLQ overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k) . . . H(2) H(1)
*
*  as returned by DGELQF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**T from the Left;
*          = 'R': apply Q or Q**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'T':  Transpose, apply Q**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGELQF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      CHARACTER          TRANST
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORML2, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
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
         CALL XERBLA( 'DORMLQ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.  NB may be at most NBMAX, where NBMAX
*     is used to define the local array T.
*
      NB = MIN( NBMAX, ILAENV( 1, 'DORMLQ', SIDE // TRANS, M, N, K,
     $     -1 ) )
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMLQ', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL DORML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
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
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL DLARFT( 'Forward', 'Rowwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL DLARFB( SIDE, TRANST, 'Forward', 'Rowwise', MI, NI, IB,
     $                   A( I, I ), LDA, T, LDT, C( IC, JC ), LDC, WORK,
     $                   LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORMLQ
*
      END
      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  DORMQR overwrites the general real M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'T':      Q**T * C       C * Q**T
*
*  where Q is a real orthogonal matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**T from the Left;
*          = 'R': apply Q or Q**T from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'T':  Transpose, apply Q**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          DGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORM2R, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
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
         CALL XERBLA( 'DORMQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Determine the block size.  NB may be at most NBMAX, where NBMAX
*     is used to define the local array T.
*
      NB = MIN( NBMAX, ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N, K,
     $     -1 ) )
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMQR', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
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
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = IWS
      RETURN
*
*     End of DORMQR
*
      END
      DOUBLE PRECISION FUNCTION DLAMCH( CMACH )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CMACH
*     ..
*
*  Purpose
*  =======
*
*  DLAMCH determines double precision machine parameters.
*
*  Arguments
*  =========
*
*  CMACH   (input) CHARACTER*1
*          Specifies the value to be returned by DLAMCH:
*          = 'E' or 'e',   DLAMCH := eps
*          = 'S' or 's ,   DLAMCH := sfmin
*          = 'B' or 'b',   DLAMCH := base
*          = 'P' or 'p',   DLAMCH := eps*base
*          = 'N' or 'n',   DLAMCH := t
*          = 'R' or 'r',   DLAMCH := rnd
*          = 'M' or 'm',   DLAMCH := emin
*          = 'U' or 'u',   DLAMCH := rmin
*          = 'L' or 'l',   DLAMCH := emax
*          = 'O' or 'o',   DLAMCH := rmax
*
*          where
*
*          eps   = relative machine precision
*          sfmin = safe minimum, such that 1/sfmin does not overflow
*          base  = base of the machine
*          prec  = eps*base
*          t     = number of (base) digits in the mantissa
*          rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise
*          emin  = minimum exponent before (gradual) underflow
*          rmin  = underflow threshold - base**(emin-1)
*          emax  = largest exponent before overflow
*          rmax  = overflow threshold  - (base**emax)*(1-eps)
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRST, LRND
      INTEGER            BETA, IMAX, IMIN, IT
      DOUBLE PRECISION   BASE, EMAX, EMIN, EPS, PREC, RMACH, RMAX, RMIN,
     $                   RND, SFMIN, SMALL, T
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMC2
*     ..
*     .. Save statement ..
      SAVE               FIRST, EPS, SFMIN, BASE, T, RND, EMIN, RMIN,
     $                   EMAX, RMAX, PREC
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         CALL DLAMC2( BETA, IT, LRND, EPS, IMIN, RMIN, IMAX, RMAX )
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
*
*           Use SMALL plus a bit, to avoid the possibility of rounding
*           causing overflow when computing  1/sfmin.
*
            SFMIN = SMALL*( ONE+EPS )
         END IF
      END IF
*
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
*
      DLAMCH = RMACH
      RETURN
*
*     End of DLAMCH
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC1( BETA, T, RND, IEEE1 )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE1, RND
      INTEGER            BETA, T
*     ..
*
*  Purpose
*  =======
*
*  DLAMC1 determines the machine parameters given by BETA, T, RND, and
*  IEEE1.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  IEEE1   (output) LOGICAL
*          Specifies whether rounding appears to be done in the IEEE
*          'round to nearest' style.
*
*  Further Details
*  ===============
*
*  The routine is based on the routine  ENVRON  by Malcolm and
*  incorporates suggestions by Gentleman and Marovich. See
*
*     Malcolm M. A. (1972) Algorithms to reveal properties of
*        floating-point arithmetic. Comms. of the ACM, 15, 949-951.
*
*     Gentleman W. M. and Marovich S. B. (1974) More on algorithms
*        that reveal properties of floating point arithmetic units.
*        Comms. of the ACM, 17, 276-277.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, LIEEE1, LRND
      INTEGER            LBETA, LT
      DOUBLE PRECISION   A, B, C, F, ONE, QTR, SAVEC, T1, T2
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Save statement ..
      SAVE               FIRST, LIEEE1, LBETA, LRND, LT
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ONE = 1
*
*        LBETA,  LIEEE1,  LT and  LRND  are the  local values  of  BETA,
*        IEEE1, T and RND.
*
*        Throughout this routine  we use the function  DLAMC3  to ensure
*        that relevant values are  stored and not held in registers,  or
*        are not affected by optimizers.
*
*        Compute  a = 2.0**m  with the  smallest positive integer m such
*        that
*
*           fl( a + 1.0 ) = a.
*
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   10    CONTINUE
         IF( C.EQ.ONE ) THEN
            A = 2*A
            C = DLAMC3( A, ONE )
            C = DLAMC3( C, -A )
            GO TO 10
         END IF
*+       END WHILE
*
*        Now compute  b = 2.0**m  with the smallest positive integer m
*        such that
*
*           fl( a + b ) .gt. a.
*
         B = 1
         C = DLAMC3( A, B )
*
*+       WHILE( C.EQ.A )LOOP
   20    CONTINUE
         IF( C.EQ.A ) THEN
            B = 2*B
            C = DLAMC3( A, B )
            GO TO 20
         END IF
*+       END WHILE
*
*        Now compute the base.  a and c  are neighbouring floating point
*        numbers  in the  interval  ( beta**t, beta**( t + 1 ) )  and so
*        their difference is beta. Adding 0.25 to c is to ensure that it
*        is truncated to beta and not ( beta - 1 ).
*
         QTR = ONE / 4
         SAVEC = C
         C = DLAMC3( C, -A )
         LBETA = C + QTR
*
*        Now determine whether rounding or chopping occurs,  by adding a
*        bit  less  than  beta/2  and a  bit  more  than  beta/2  to  a.
*
         B = LBETA
         F = DLAMC3( B / 2, -B / 100 )
         C = DLAMC3( F, A )
         IF( C.EQ.A ) THEN
            LRND = .TRUE.
         ELSE
            LRND = .FALSE.
         END IF
         F = DLAMC3( B / 2, B / 100 )
         C = DLAMC3( F, A )
         IF( ( LRND ) .AND. ( C.EQ.A ) )
     $      LRND = .FALSE.
*
*        Try and decide whether rounding is done in the  IEEE  'round to
*        nearest' style. B/2 is half a unit in the last place of the two
*        numbers A and SAVEC. Furthermore, A is even, i.e. has last  bit
*        zero, and SAVEC is odd. Thus adding B/2 to A should not  change
*        A, but adding B/2 to SAVEC should change SAVEC.
*
         T1 = DLAMC3( B / 2, A )
         T2 = DLAMC3( B / 2, SAVEC )
         LIEEE1 = ( T1.EQ.A ) .AND. ( T2.GT.SAVEC ) .AND. LRND
*
*        Now find  the  mantissa, t.  It should  be the  integer part of
*        log to the base beta of a,  however it is safer to determine  t
*        by powering.  So we find t as the smallest positive integer for
*        which
*
*           fl( beta**t + 1.0 ) = 1.0.
*
         LT = 0
         A = 1
         C = 1
*
*+       WHILE( C.EQ.ONE )LOOP
   30    CONTINUE
         IF( C.EQ.ONE ) THEN
            LT = LT + 1
            A = A*LBETA
            C = DLAMC3( A, ONE )
            C = DLAMC3( C, -A )
            GO TO 30
         END IF
*+       END WHILE
*
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      IEEE1 = LIEEE1
      RETURN
*
*     End of DLAMC1
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC2( BETA, T, RND, EPS, EMIN, RMIN, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            RND
      INTEGER            BETA, EMAX, EMIN, T
      DOUBLE PRECISION   EPS, RMAX, RMIN
*     ..
*
*  Purpose
*  =======
*
*  DLAMC2 determines the machine parameters specified in its argument
*  list.
*
*  Arguments
*  =========
*
*  BETA    (output) INTEGER
*          The base of the machine.
*
*  T       (output) INTEGER
*          The number of ( BETA ) digits in the mantissa.
*
*  RND     (output) LOGICAL
*          Specifies whether proper rounding  ( RND = .TRUE. )  or
*          chopping  ( RND = .FALSE. )  occurs in addition. This may not
*          be a reliable guide to the way in which the machine performs
*          its arithmetic.
*
*  EPS     (output) DOUBLE PRECISION
*          The smallest positive number such that
*
*             fl( 1.0 - EPS ) .LT. 1.0,
*
*          where fl denotes the computed value.
*
*  EMIN    (output) INTEGER
*          The minimum exponent before (gradual) underflow occurs.
*
*  RMIN    (output) DOUBLE PRECISION
*          The smallest normalized number for the machine, given by
*          BASE**( EMIN - 1 ), where  BASE  is the floating point value
*          of BETA.
*
*  EMAX    (output) INTEGER
*          The maximum exponent before overflow occurs.
*
*  RMAX    (output) DOUBLE PRECISION
*          The largest positive number for the machine, given by
*          BASE**EMAX * ( 1 - EPS ), where  BASE  is the floating point
*          value of BETA.
*
*  Further Details
*  ===============
*
*  The computation of  EPS  is based on a routine PARANOIA by
*  W. Kahan of the University of California at Berkeley.
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            FIRST, IEEE, IWARN, LIEEE1, LRND
      INTEGER            GNMIN, GPMIN, I, LBETA, LEMAX, LEMIN, LT,
     $                   NGNMIN, NGPMIN
      DOUBLE PRECISION   A, B, C, HALF, LEPS, LRMAX, LRMIN, ONE, RBASE,
     $                   SIXTH, SMALL, THIRD, TWO, ZERO
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMC1, DLAMC4, DLAMC5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Save statement ..
      SAVE               FIRST, IWARN, LBETA, LEMAX, LEMIN, LEPS, LRMAX,
     $                   LRMIN, LT
      INCLUDE 'INCS:DMSG.INC'
*     ..
*     .. Data statements ..
      DATA               FIRST / .TRUE. / , IWARN / .FALSE. /
*     ..
*     .. Executable Statements ..
*
C-----------------------------------------------------------------------
      IF( FIRST ) THEN
         FIRST = .FALSE.
         ZERO = 0
         ONE = 1
         TWO = 2
*
*        LBETA, LT, LRND, LEPS, LEMIN and LRMIN  are the local values of
*        BETA, T, RND, EPS, EMIN and RMIN.
*
*        Throughout this routine  we use the function  DLAMC3  to ensure
*        that relevant values are stored  and not held in registers,  or
*        are not affected by optimizers.
*
*        DLAMC1 returns the parameters  LBETA, LT, LRND and LIEEE1.
*
         CALL DLAMC1( LBETA, LT, LRND, LIEEE1 )
*
*        Start to find EPS.
*
         B = LBETA
         A = B**( -LT )
         LEPS = A
*
*        Try some tricks to see whether or not this is the correct  EPS.
*
         B = TWO / 3
         HALF = ONE / 2
         SIXTH = DLAMC3( B, -HALF )
         THIRD = DLAMC3( SIXTH, SIXTH )
         B = DLAMC3( THIRD, -HALF )
         B = DLAMC3( B, SIXTH )
         B = ABS( B )
         IF( B.LT.LEPS )
     $      B = LEPS
*
         LEPS = 1
*
*+       WHILE( ( LEPS.GT.B ).AND.( B.GT.ZERO ) )LOOP
   10    CONTINUE
         IF( ( LEPS.GT.B ) .AND. ( B.GT.ZERO ) ) THEN
            LEPS = B
            C = DLAMC3( HALF*LEPS, ( TWO**5 )*( LEPS**2 ) )
            C = DLAMC3( HALF, -C )
            B = DLAMC3( HALF, C )
            C = DLAMC3( HALF, -B )
            B = DLAMC3( HALF, C )
            GO TO 10
         END IF
*+       END WHILE
*
         IF( A.LT.LEPS )
     $      LEPS = A
*
*        Computation of EPS complete.
*
*        Now find  EMIN.  Let A = + or - 1, and + or - (1 + BASE**(-3)).
*        Keep dividing  A by BETA until (gradual) underflow occurs. This
*        is detected when we cannot recover the previous A.
*
         RBASE = ONE / LBETA
         SMALL = ONE
         DO 20 I = 1, 3
            SMALL = DLAMC3( SMALL*RBASE, ZERO )
   20    CONTINUE
         A = DLAMC3( ONE, SMALL )
         CALL DLAMC4( NGPMIN, ONE, LBETA )
         CALL DLAMC4( NGNMIN, -ONE, LBETA )
         CALL DLAMC4( GPMIN, A, LBETA )
         CALL DLAMC4( GNMIN, -A, LBETA )
         IEEE = .FALSE.
*
         IF( ( NGPMIN.EQ.NGNMIN ) .AND. ( GPMIN.EQ.GNMIN ) ) THEN
            IF( NGPMIN.EQ.GPMIN ) THEN
               LEMIN = NGPMIN
*            ( Non twos-complement machines, no gradual underflow;
*              e.g.,  VAX )
            ELSE IF( ( GPMIN-NGPMIN ).EQ.3 ) THEN
               LEMIN = NGPMIN - 1 + LT
               IEEE = .TRUE.
*            ( Non twos-complement machines, with gradual underflow;
*              e.g., IEEE standard followers )
            ELSE
               LEMIN = MIN( NGPMIN, GPMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( NGPMIN.EQ.GPMIN ) .AND. ( NGNMIN.EQ.GNMIN ) ) THEN
            IF( ABS( NGPMIN-NGNMIN ).EQ.1 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN )
*            ( Twos-complement machines, no gradual underflow;
*              e.g., CYBER 205 )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE IF( ( ABS( NGPMIN-NGNMIN ).EQ.1 ) .AND.
     $            ( GPMIN.EQ.GNMIN ) ) THEN
            IF( ( GPMIN-MIN( NGPMIN, NGNMIN ) ).EQ.3 ) THEN
               LEMIN = MAX( NGPMIN, NGNMIN ) - 1 + LT
*            ( Twos-complement machines with gradual underflow;
*              no known machine )
            ELSE
               LEMIN = MIN( NGPMIN, NGNMIN )
*            ( A guess; no known machine )
               IWARN = .TRUE.
            END IF
*
         ELSE
            LEMIN = MIN( NGPMIN, NGNMIN, GPMIN, GNMIN )
*         ( A guess; no known machine )
            IWARN = .TRUE.
         END IF
***
* Comment out this if block if EMIN is ok
         IF (IWARN) THEN
            FIRST = .TRUE.
            WRITE (MSGTXT,9999) LEMIN
            CALL MSGWRT (6)
            END IF
***
*
*        Assume IEEE arithmetic if we found denormalised  numbers above,
*        or if arithmetic seems to round in the  IEEE style,  determined
*        in routine DLAMC1. A true IEEE machine should have both  things
*        true; however, faulty machines may have one or the other.
*
         IEEE = IEEE .OR. LIEEE1
*
*        Compute  RMIN by successive division by  BETA. We could compute
*        RMIN as BASE**( EMIN - 1 ),  but some machines underflow during
*        this computation.
*
         LRMIN = 1
         DO 30 I = 1, 1 - LEMIN
            LRMIN = DLAMC3( LRMIN*RBASE, ZERO )
   30    CONTINUE
*
*        Finally, call DLAMC5 to compute EMAX and RMAX.
*
         CALL DLAMC5( LBETA, LT, LEMIN, IEEE, LEMAX, LRMAX )
      END IF
*
      BETA = LBETA
      T = LT
      RND = LRND
      EPS = LEPS
      EMIN = LEMIN
      RMIN = LRMIN
      EMAX = LEMAX
      RMAX = LRMAX
*
      RETURN
*
 9999 FORMAT( / / ' WARNING. The value EMIN may be incorrect:-',
     $      '  EMIN = ', I8, /
     $      ' If, after inspection, the value EMIN looks',
     $      ' acceptable please comment out ',
     $      / ' the IF block as marked within the code of routine',
     $      ' DLAMC2,', / ' otherwise supply EMIN explicitly.', / )
*
*     End of DLAMC2
*
      END
*
************************************************************************
*
      DOUBLE PRECISION FUNCTION DLAMC3( A, B )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B
*     ..
*
*  Purpose
*  =======
*
*  DLAMC3  is intended to force  A  and  B  to be stored prior to doing
*  the addition of  A  and  B ,  for use in situations where optimizers
*  might hold one of these in a register.
*
*  Arguments
*  =========
*
*  A, B    (input) DOUBLE PRECISION
*          The values A and B.
*
* =====================================================================
*
*     .. Executable Statements ..
*
      DLAMC3 = A + B
*
      RETURN
*
*     End of DLAMC3
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC4( EMIN, START, BASE )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            BASE, EMIN
      DOUBLE PRECISION   START
*     ..
*
*  Purpose
*  =======
*
*  DLAMC4 is a service routine for DLAMC2.
*
*  Arguments
*  =========
*
*  EMIN    (output) EMIN
*          The minimum exponent before (gradual) underflow, computed by
*          setting A = START and dividing by BASE until the previous A
*          can not be recovered.
*
*  START   (input) DOUBLE PRECISION
*          The starting point for determining EMIN.
*
*  BASE    (input) INTEGER
*          The base of the machine.
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   A, B1, B2, C1, C2, D1, D2, ONE, RBASE, ZERO
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Executable Statements ..
*
      A = START
      ONE = 1
      RBASE = ONE / BASE
      ZERO = 0
      EMIN = 1
      B1 = DLAMC3( A*RBASE, ZERO )
      C1 = A
      C2 = A
      D1 = A
      D2 = A
*+    WHILE( ( C1.EQ.A ).AND.( C2.EQ.A ).AND.
*    $       ( D1.EQ.A ).AND.( D2.EQ.A )      )LOOP
   10 CONTINUE
      IF( ( C1.EQ.A ) .AND. ( C2.EQ.A ) .AND. ( D1.EQ.A ) .AND.
     $    ( D2.EQ.A ) ) THEN
         EMIN = EMIN - 1
         A = B1
         B1 = DLAMC3( A / BASE, ZERO )
         C1 = DLAMC3( B1*BASE, ZERO )
         D1 = ZERO
         DO 20 I = 1, BASE
            D1 = D1 + B1
   20    CONTINUE
         B2 = DLAMC3( A*RBASE, ZERO )
         C2 = DLAMC3( B2 / RBASE, ZERO )
         D2 = ZERO
         DO 30 I = 1, BASE
            D2 = D2 + B2
   30    CONTINUE
         GO TO 10
      END IF
*+    END WHILE
*
      RETURN
*
*     End of DLAMC4
*
      END
*
************************************************************************
*
      SUBROUTINE DLAMC5( BETA, P, EMIN, IEEE, EMAX, RMAX )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            BETA, EMAX, EMIN, P
      DOUBLE PRECISION   RMAX
*     ..
*
*  Purpose
*  =======
*
*  DLAMC5 attempts to compute RMAX, the largest machine floating-point
*  number, without overflow.  It assumes that EMAX + abs(EMIN) sum
*  approximately to a power of 2.  It will fail on machines where this
*  assumption does not hold, for example, the Cyber 205 (EMIN = -28625,
*  EMAX = 28718).  It will also fail if the value supplied for EMIN is
*  too large (i.e. too close to zero), probably with overflow.
*
*  Arguments
*  =========
*
*  BETA    (input) INTEGER
*          The base of floating-point arithmetic.
*
*  P       (input) INTEGER
*          The number of base BETA digits in the mantissa of a
*          floating-point value.
*
*  EMIN    (input) INTEGER
*          The minimum exponent before (gradual) underflow.
*
*  IEEE    (input) LOGICAL
*          A logical flag specifying whether or not the arithmetic
*          system is thought to comply with the IEEE standard.
*
*  EMAX    (output) INTEGER
*          The largest exponent before overflow
*
*  RMAX    (output) DOUBLE PRECISION
*          The largest machine floating-point number.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            EXBITS, EXPSUM, I, LEXP, NBITS, TRY, UEXP
      DOUBLE PRECISION   OLDY, RECBAS, Y, Z
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3
      EXTERNAL           DLAMC3
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MOD
*     ..
*     .. Executable Statements ..
*
*     First compute LEXP and UEXP, two powers of 2 that bound
*     abs(EMIN). We then assume that EMAX + abs(EMIN) will sum
*     approximately to the bound that is closest to abs(EMIN).
*     (EMAX is the exponent of the required number RMAX).
*
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
*
*     Now -LEXP is less than or equal to EMIN, and -UEXP is greater
*     than or equal to EMIN. EXBITS is the number of bits needed to
*     store the exponent.
*
      IF( ( UEXP+EMIN ).GT.( -LEXP-EMIN ) ) THEN
         EXPSUM = 2*LEXP
      ELSE
         EXPSUM = 2*UEXP
      END IF
*
*     EXPSUM is the exponent range, approximately equal to
*     EMAX - EMIN + 1 .
*
      EMAX = EXPSUM + EMIN - 1
      NBITS = 1 + EXBITS + P
*
*     NBITS is the total number of bits needed to store a
*     floating-point number.
*
      IF( ( MOD( NBITS, 2 ).EQ.1 ) .AND. ( BETA.EQ.2 ) ) THEN
*
*        Either there are an odd number of bits used to store a
*        floating-point number, which is unlikely, or some bits are
*        not used in the representation of numbers, which is possible,
*        (e.g. Cray machines) or the mantissa has an implicit bit,
*        (e.g. IEEE machines, Dec Vax machines), which is perhaps the
*        most likely. We have to assume the last alternative.
*        If this is true, then we need to reduce EMAX by one because
*        there must be some way of representing zero in an implicit-bit
*        system. On machines like Cray, we are reducing EMAX by one
*        unnecessarily.
*
         EMAX = EMAX - 1
      END IF
*
      IF( IEEE ) THEN
*
*        Assume we are on an IEEE machine which reserves one exponent
*        for infinity and NaN.
*
         EMAX = EMAX - 1
      END IF
*
*     Now create RMAX, the largest machine number, which should
*     be equal to (1.0 - BETA**(-P)) * BETA**EMAX .
*
*     First compute 1.0 - BETA**(-P), being careful that the
*     result is less than 1.0 .
*
      RECBAS = ONE / BETA
      Z = BETA - ONE
      Y = ZERO
      DO 20 I = 1, P
         Z = Z*RECBAS
         IF( Y.LT.ONE )
     $      OLDY = Y
         Y = DLAMC3( Y, Z )
   20 CONTINUE
      IF( Y.GE.ONE )
     $   Y = OLDY
*
*     Now multiply by BETA**EMAX to get RMAX.
*
      DO 30 I = 1, EMAX
         Y = DLAMC3( Y*BETA, ZERO )
   30 CONTINUE
*
      RMAX = Y
      RETURN
*
*     End of DLAMC5
*
      END
      INTEGER          FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3,
     $                 N4 )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR and QZ methods
*               for nonsymmetric eigenvalue problems.
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            CNAME, SNAME
      CHARACTER*1        C1
      CHARACTER*2        C2, C4
      CHARACTER*3        C3
      CHARACTER*6        SUBNAM
      INTEGER            I, IC, IZ, NB, NBMIN, NX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. Executable Statements ..
*
      GO TO ( 100, 100, 100, 400, 500, 600, 700, 800 ) ISPEC
*
*     Invalid value for ISPEC
*
      ILAENV = -1
      RETURN
*
  100 CONTINUE
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1:1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
*
*        ASCII character set
*
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 10 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   10       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
*
*        EBCDIC character set
*
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
*
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
*
*        Prime machines:  ASCII+128
*
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1:1 ) = CHAR( IC-32 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I:I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I:I ) = CHAR( IC-32 )
   30       CONTINUE
         END IF
      END IF
*
      C1 = SUBNAM( 1:1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2:3 )
      C3 = SUBNAM( 4:6 )
      C4 = C3( 2:3 )
*
      GO TO ( 110, 200, 300 ) ISPEC
*
  110 CONTINUE
*
*     ISPEC = 1:  block size
*
*     In these examples, separate code is provided for setting NB for
*     real and complex.  We assume that NB will take the same value in
*     single or double precision.
*
      NB = 1
*
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
*
  200 CONTINUE
*
*     ISPEC = 2:  minimum block size
*
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
*
  300 CONTINUE
*
*     ISPEC = 3:  crossover point
*
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
*
  400 CONTINUE
*
*     ISPEC = 4:  number of shifts (used by xHSEQR)
*
      ILAENV = 6
      RETURN
*
  500 CONTINUE
*
*     ISPEC = 5:  minimum column dimension (not used)
*
      ILAENV = 2
      RETURN
*
  600 CONTINUE
*
*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
*
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
*
  700 CONTINUE
*
*     ISPEC = 7:  number of processors (not used)
*
      ILAENV = 1
      RETURN
*
  800 CONTINUE
*
*     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
*
      ILAENV = 50
      RETURN
*
*     End of ILAENV
*
      END
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
      INCLUDE 'INCS:DMSG.INC'
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
* =====================================================================
*
*     .. Executable Statements ..
*
C-----------------------------------------------------------------------
      WRITE (MSGTXT,9999 ) SRNAME, INFO
      CALL MSGWRT (9)
*
      STOP
C-----------------------------------------------------------------------
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
